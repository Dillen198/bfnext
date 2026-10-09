/*
Copyright 2026 Dillen Weerasinghe.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
for more details.
*/

//! Emergency repair crate (`Cfg::emergency_repair`).
//!
//! A damaged base normally heals one group per `repair_time / logi` seconds
//! of being left alone. This crate lets a logistics crew buy one of those
//! steps immediately by flying it in -- sling load, C-130 airdrop or
//! ground-crew dynamic cargo, all of which end up here via the cargo code.
//!
//! It is deliberately NOT a way to fight a repair race mid-assault: the base
//! must be friendly, damaged but not wiped out, not threatened, not under a
//! running capture timer and not consolidating after a capture, and a single
//! base can only take one every `cooldown_secs_per_objective`. It also can't
//! be unpacked at the base it was loaded at -- otherwise a base could simply
//! repair itself on demand without anybody flying anywhere.
//!
//! Cost and refunds: the repair is paid out of the target base's own stores
//! exactly as an automatic repair pulse is (materiel when the materiel
//! commodity is on, supply otherwise), plus the optional `cost_points` from
//! the delivering player. Any refusal -- including the base not being able
//! to afford the repair -- leaves the crate where it is, unspent, and tells
//! the player why; nothing is charged. A C-130 crate that is refused backs
//! off and retries on its own (see `c130_crate_blocked`), so a crate dropped
//! during an attack still does its job once the base goes quiet.

use super::{Db, objective::ObjGroupClass};
use crate::objective;
use anyhow::{Result, anyhow};
use bfprotocols::{
    cfg::MATERIEL_ITEM,
    db::{group::GroupId, objective::ObjectiveId},
    stats::Stat,
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{Vector2, coalition::Side, net::Ucid};
use log::info;
use smallvec::SmallVec;

/// What the rule check needs to know about the target base.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Target {
    pub owner: Side,
    pub health: u8,
    pub threatened: bool,
    pub capture_running: bool,
    pub in_capture_hold: bool,
    pub is_carrier: bool,
    /// The crate was loaded at this very base.
    pub is_origin: bool,
    /// Seconds since the last emergency repair here, if there was one.
    pub secs_since_last: Option<i64>,
}

/// Why a crate was refused. The crate is never consumed for any of these.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Refusal {
    Neutral,
    Enemy,
    Carrier,
    Origin,
    Consolidating,
    CaptureRunning,
    UnderAttack,
    FullHealth,
    Wiped,
    /// Seconds left on the cooldown.
    Cooldown(i64),
}

impl Refusal {
    pub(crate) fn explain(&self, base: &str) -> CompactString {
        match self {
            Self::Neutral => format_compact!(
                "{base} is Neutral -- it has to be retaken with troops, it can't be repaired"
            ),
            Self::Enemy => format_compact!("{base} is an enemy base"),
            Self::Carrier => format_compact!(
                "{base} is a carrier group -- use the carrier repair crate there"
            ),
            Self::Origin => format_compact!(
                "this crate was loaded at {base} -- fly it to a different friendly base"
            ),
            Self::Consolidating => format_compact!(
                "{base} is still consolidating after its capture -- nothing to repair yet"
            ),
            Self::CaptureRunning => format_compact!(
                "{base} has an enemy capture in progress -- clear the enemy troops first"
            ),
            Self::UnderAttack => format_compact!(
                "{base} is under attack -- repairs can't start while enemy units are in sight of it"
            ),
            Self::FullHealth => format_compact!("{base} is already at full health"),
            Self::Wiped => format_compact!(
                "{base}'s garrison is wiped out -- the base is falling to Neutral"
            ),
            Self::Cooldown(secs) => format_compact!(
                "{base} had an emergency repair recently -- next one possible in {}",
                fmt_wait(*secs)
            ),
        }
    }
}

fn fmt_wait(secs: i64) -> CompactString {
    if secs >= 60 {
        format_compact!("{}m {:02}s", secs / 60, secs % 60)
    } else {
        format_compact!("{}s", secs.max(0))
    }
}

/// The rules an emergency repair has to pass before the base's stores are
/// even looked at. Pure, so the gating is unit tested; `side` is the
/// delivering side.
pub(crate) fn check(side: Side, t: &Target, cooldown_secs: i64) -> Result<(), Refusal> {
    if t.owner == Side::Neutral {
        return Err(Refusal::Neutral);
    }
    if t.owner != side {
        return Err(Refusal::Enemy);
    }
    if t.is_carrier {
        return Err(Refusal::Carrier);
    }
    if t.health >= 100 {
        return Err(Refusal::FullHealth);
    }
    if t.in_capture_hold {
        return Err(Refusal::Consolidating);
    }
    if t.capture_running {
        return Err(Refusal::CaptureRunning);
    }
    if t.threatened {
        return Err(Refusal::UnderAttack);
    }
    // After the attack checks: a wiped base is reported as such only once
    // nobody is shooting at it, which is when the player can act on it.
    if t.health == 0 {
        return Err(Refusal::Wiped);
    }
    if t.is_origin {
        return Err(Refusal::Origin);
    }
    if let Some(since) = t.secs_since_last {
        if cooldown_secs > 0 && since < cooldown_secs {
            return Err(Refusal::Cooldown(cooldown_secs - since));
        }
    }
    Ok(())
}

fn class_label(c: ObjGroupClass) -> &'static str {
    match c {
        ObjGroupClass::Logi => "logistics group",
        ObjGroupClass::Services => "services group",
        ObjGroupClass::Infantry => "infantry squad",
        ObjGroupClass::Sr => "short-range air defence group",
        ObjGroupClass::Aaa => "AAA group",
        ObjGroupClass::Mr => "medium-range SAM group",
        ObjGroupClass::Lr => "long-range SAM group",
        ObjGroupClass::Armor => "armour group",
        ObjGroupClass::Naval => "naval group",
        ObjGroupClass::Other => "garrison group",
    }
}

impl Db {
    /// The emergency crate definition, if the feature is on.
    pub fn emergency_repair_crate(&self) -> Option<&bfprotocols::cfg::Crate> {
        self.ephemeral
            .cfg
            .emergency_repair
            .as_ref()
            .filter(|e| e.enabled)
            .map(|e| &e.crate_def)
    }

    /// Is `name` the emergency repair crate?
    pub fn is_emergency_repair_crate(&self, name: &str) -> bool {
        self.emergency_repair_crate()
            .map_or(false, |c| c.name.as_str() == name)
    }

    /// Can `oid` pay for one repair step out of its own stores? Mirrors the
    /// affordability check at the top of `repair_objective_inner` so a crate
    /// that would do nothing is refused with the reason instead of being
    /// spent on a no-op.
    fn emergency_repair_affordable(&self, oid: ObjectiveId) -> Result<(), CompactString> {
        let Ok(obj) = objective!(self, oid) else {
            return Err("that base no longer exists".into());
        };
        let materiel = self
            .ephemeral
            .cfg
            .warehouse
            .as_ref()
            .and_then(|w| w.materiel.as_ref())
            .filter(|m| m.enabled);
        match materiel {
            Some(m) => {
                let have = obj
                    .warehouse
                    .equipment
                    .get(&dcso3::String::from(MATERIEL_ITEM))
                    .map(|inv| inv.stored)
                    .unwrap_or(0);
                if have < m.repair_cost {
                    return Err(format_compact!(
                        "{} can't pay for the repair: {have} materiel on hand, {} needed -- it needs a convoy or supply run first",
                        obj.name,
                        m.repair_cost
                    ));
                }
            }
            None => {
                let need = self.ephemeral.cfg.repair_supply_cost;
                if obj.supply < need {
                    return Err(format_compact!(
                        "{} can't pay for the repair: supply {}% is under the {need}% a repair costs -- resupply it first",
                        obj.name,
                        obj.supply
                    ));
                }
            }
        }
        Ok(())
    }

    /// Deliver one emergency repair crate that is sitting at `pos`. `origin`
    /// is the base it was loaded at and `by` the player who brought it.
    ///
    /// `Ok(Ok(msg))`: the repair went through and the caller must delete the
    /// crate. `Ok(Err(why))`: refused, the crate stays where it is and `why`
    /// goes back to the player. Nothing is charged on a refusal.
    pub(crate) fn deliver_emergency_repair(
        &mut self,
        by: Ucid,
        side: Side,
        pos: Vector2,
        origin: ObjectiveId,
        now: DateTime<Utc>,
    ) -> Result<std::result::Result<CompactString, CompactString>> {
        let Some(er) = self.ephemeral.cfg.emergency_repair.clone().filter(|e| e.enabled) else {
            return Ok(Err("emergency repairs are switched off on this server".into()));
        };
        // Zones can overlap; if one of them is ours, that is the one meant.
        let mut hit: Option<ObjectiveId> = None;
        for (oid, obj) in &self.persisted.objectives {
            if obj.zone.contains(pos) {
                if obj.owner == side {
                    hit = Some(*oid);
                    break;
                }
                hit.get_or_insert(*oid);
            }
        }
        let Some(oid) = hit else {
            info!("[EMERGENCY_REPAIR] refused: crate from {by} at ({:.0}, {:.0}) is not inside any objective zone", pos.x, pos.y);
            return Ok(Err(
                "an emergency repair crate has to be unpacked INSIDE the zone of the friendly base it is for".into(),
            ));
        };
        let (name, target) = {
            let obj = objective!(self, oid)?;
            let target = Target {
                owner: obj.owner,
                health: obj.health,
                threatened: obj.threatened,
                capture_running: self.ephemeral.capture_progress.contains_key(&oid),
                in_capture_hold: obj.in_capture_hold(),
                is_carrier: obj.kind.is_carrier_group(),
                is_origin: oid == origin,
                secs_since_last: self
                    .ephemeral
                    .emergency_repair_last
                    .get(&oid)
                    .map(|t| (now - *t).num_seconds()),
            };
            (obj.name.clone(), target)
        };
        if let Err(why) = check(side, &target, er.cooldown_secs_per_objective as i64) {
            info!("[EMERGENCY_REPAIR] {name}: refused crate from {by}: {why:?}");
            return Ok(Err(why.explain(&name)));
        }
        if let Err(why) = self.emergency_repair_affordable(oid) {
            info!("[EMERGENCY_REPAIR] {name}: refused crate from {by}: {why}");
            return Ok(Err(why));
        }
        let points_on = self.ephemeral.cfg.points.is_some();
        if points_on && er.cost_points > 0 {
            let have = self.persisted.players.get(&by).map(|p| p.points).unwrap_or(0);
            if have < er.cost_points as i32 {
                info!("[EMERGENCY_REPAIR] {name}: refused crate from {by}: {have} points, {} needed", er.cost_points);
                return Ok(Err(format_compact!(
                    "an emergency repair costs {} points and you have {have}",
                    er.cost_points
                )));
            }
        }
        // Which groups are damaged now, so the one the repair put back can be
        // named -- `repair_objective` only says whether it did something.
        let damaged: SmallVec<[GroupId; 16]> = {
            let obj = objective!(self, oid)?;
            obj.groups
                .get(&obj.owner)
                .map(|gids| {
                    gids.into_iter()
                        .filter(|gid| self.group_has_dead(gid))
                        .copied()
                        .collect()
                })
                .unwrap_or_default()
        };
        let health_before = target.health;
        if !self.repair_objective(oid, now)? {
            info!("[EMERGENCY_REPAIR] {name}: refused crate from {by}: no damaged group it could rebuild");
            return Ok(Err(format_compact!(
                "{name} has no destroyed group an emergency repair can rebuild"
            )));
        }
        let rebuilt = damaged
            .iter()
            .find(|gid| !self.group_has_dead(gid))
            .and_then(|gid| self.persisted.groups.get(gid))
            .map(|g| class_label(g.class))
            .unwrap_or("garrison group");
        // The repair restarts the base's countdown just as an automatic
        // pulse does: `update_objective_status` moved `last_change_ts` when
        // health changed; set it outright so it holds even if the rounded
        // health figure didn't move.
        if let Some(obj) = self.persisted.objectives.get_mut_cow(&oid) {
            obj.last_change_ts = now;
        }
        self.ephemeral.emergency_repair_last.insert(oid, now);
        if let Err(e) = self.update_supply_status() {
            log::error!("[EMERGENCY_REPAIR] updating supply status: {e:?}");
        }
        if points_on && er.cost_points > 0 {
            self.adjust_points(
                &by,
                -(er.cost_points as i32),
                &format_compact!("for an emergency repair at {name}"),
            );
        }
        self.ephemeral.stat(Stat::Repair { id: oid, by });
        let health_after = objective!(self, oid)?.health;
        {
            let obj = objective!(self, oid)?;
            self.ephemeral.create_objective_markup(&self.persisted, obj);
        }
        self.ephemeral.dirty();
        info!(
            "[EMERGENCY_REPAIR] {name}: {rebuilt} rebuilt by {by}'s crate (health {health_before}% -> {health_after}%)"
        );
        Ok(Ok(format_compact!(
            "Emergency repair at {name}: {rebuilt} rebuilt (health {health_before}% -> {health_after}%)"
        )))
    }

    fn group_has_dead(&self, gid: &GroupId) -> bool {
        self.persisted.groups.get(gid).map_or(false, |g| {
            g.units
                .into_iter()
                .filter_map(|uid| self.persisted.units.get(uid))
                .any(|u| u.dead)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn quiet() -> Target {
        Target {
            owner: Side::Blue,
            health: 60,
            threatened: false,
            capture_running: false,
            in_capture_hold: false,
            is_carrier: false,
            is_origin: false,
            secs_since_last: None,
        }
    }

    #[test]
    fn quiet_damaged_friendly_base_is_accepted() {
        assert_eq!(check(Side::Blue, &quiet(), 600), Ok(()));
        // Cooldown elapsed.
        let t = Target { secs_since_last: Some(600), ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Ok(()));
    }

    #[test]
    fn ownership_refusals() {
        let t = Target { owner: Side::Neutral, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::Neutral));
        let t = Target { owner: Side::Red, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::Enemy));
        let t = Target { is_carrier: true, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::Carrier));
    }

    #[test]
    fn a_contested_base_refuses() {
        let t = Target { threatened: true, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::UnderAttack));
        let t = Target { capture_running: true, threatened: true, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::CaptureRunning));
        let t = Target { in_capture_hold: true, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::Consolidating));
    }

    #[test]
    fn health_refusals() {
        let t = Target { health: 100, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::FullHealth));
        let t = Target { health: 0, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::Wiped));
        // Being shot at is the more useful thing to hear than "wiped".
        let t = Target { health: 0, threatened: true, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::UnderAttack));
    }

    #[test]
    fn origin_and_cooldown() {
        let t = Target { is_origin: true, ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::Origin));
        let t = Target { secs_since_last: Some(100), ..quiet() };
        assert_eq!(check(Side::Blue, &t, 600), Err(Refusal::Cooldown(500)));
        // cooldown 0 = off
        assert_eq!(check(Side::Blue, &t, 0), Ok(()));
    }

    #[test]
    fn refusal_text_names_the_base() {
        assert!(Refusal::UnderAttack.explain("Kutaisi").contains("Kutaisi is under attack"));
        assert!(Refusal::Cooldown(125).explain("Kutaisi").contains("2m 05s"));
    }
}
