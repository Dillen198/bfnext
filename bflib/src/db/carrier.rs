/*
Copyright 2024 Eric Stokes.

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

//! Infantry carried in IFVs, APCs and trucks: the `cfg.dismount` types.
//!
//! Every such vehicle carries one squad, its own, for as long as it lives.
//! Before this the squad only ever got out when the vehicle was destroyed.
//! Now the player driving it (Combined Arms) or the player who deployed it
//! (AI driven) can put the squad down while the vehicle is stopped, and must
//! take that same squad back aboard (embark) before it can be put down
//! again. Nothing is ever duplicated:
//!
//! * a vehicle whose squad is lost (killed, deleted, lifted out by a helo)
//!   carries nobody from then on;
//! * a destroyed vehicle lets out whoever is aboard, and nobody if its squad
//!   is out or lost;
//! * a formation assault that sends a vehicle's squad in uses it up.
//!
//! A squad that gets out is an ordinary troop squad owned by the player who
//! gave the order: it counts against the troop limits and captures if its
//! type can.

use super::{
    Db,
    cargo::{InternalTroop, squad_strength},
    group::DeployKind,
};
use crate::{group, spawnctx::SpawnCtx, spawnctx::SpawnLoc};
use anyhow::{Result, anyhow, bail};
use bfprotocols::{
    cfg::{LimitEnforceTyp, PersistTyp, Troop, Vehicle},
    db::group::{GroupId, UnitId},
    stats::Stat,
};
use dcso3::{
    Vector2, azumith3d, coalition::Side, env::miz::MizIndex, net::Ucid, unit::Unit,
};
use enumflags2::BitFlags;
use log::info;
use serde_derive::{Deserialize, Serialize};

/// A carrier whose squad is not simply aboard as it started.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Carried {
    /// Its squad is out, as this group. Embark it to carry it again.
    Out(GroupId),
    /// Its squad is back aboard, as it now is (owner, casualties).
    Squad(InternalTroop),
    /// Nobody aboard, for good: its squad was lost or used up.
    Lost,
}

/// What a carrier has aboard right now.
#[derive(Debug, Clone)]
pub enum Aboard {
    /// The squad it started with.
    Own(Troop),
    /// Its squad, back aboard after being out.
    Squad(InternalTroop),
    /// Its squad is out in the field as this group.
    Out(GroupId),
    /// Its squad is gone: it has no troops to put down any more.
    Lost,
}

impl Aboard {
    pub fn describe(&self) -> std::string::String {
        let squad = |t: &Troop, strength: Option<(u16, u16)>| match strength {
            Some((alive, total)) => format!("a {} squad ({alive} of {total})", t.name),
            None => format!("a {} squad", t.name),
        };
        match self {
            Aboard::Own(t) => squad(t, None),
            Aboard::Squad(it) => squad(&it.troop, it.strength),
            Aboard::Out(gid) => format!("nobody (its squad is out as group {gid})"),
            Aboard::Lost => "nobody (its squad was lost)".into(),
        }
    }

    /// Is there a squad aboard to put down?
    pub fn loaded(&self) -> bool {
        matches!(self, Aboard::Own(_) | Aboard::Squad(_))
    }
}

/// A carrier must be this slow to let a squad out or in, metres a second.
const STOPPED_MPS: f64 = 1.5;
/// How close its squad must be to a carrier to embark, metres.
const EMBARK_RANGE_M: f64 = 60.;

/// The troop type for the squad a `cfg.dismount` vehicle carries: the side's
/// troop with the same template if there is one (so it shares that troop's
/// limit), else one made from the dismount spec.
fn own_squad_troop(
    troops: Option<&Vec<Troop>>,
    template: &str,
    can_capture: bool,
) -> Troop {
    troops
        .and_then(|ts| ts.iter().find(|t| t.template.as_str() == template))
        .cloned()
        .unwrap_or_else(|| Troop {
            name: "Dismounts".into(),
            template: template.into(),
            persist: PersistTyp::Forever,
            can_capture,
            limit: 10,
            limit_enforce: LimitEnforceTyp::DeleteOldest,
            weight: 0,
            cost: 0,
            jtac: None,
        })
}

/// What a carrier has aboard, from its record (`None`: as it started) and
/// whether the group its squad went out as still has anyone alive.
fn aboard(
    own: Troop,
    rec: Option<&Carried>,
    out_alive: impl Fn(&GroupId) -> bool,
) -> Aboard {
    match rec {
        None => Aboard::Own(own),
        Some(Carried::Squad(it)) => Aboard::Squad(it.clone()),
        Some(Carried::Out(gid)) if out_alive(gid) => Aboard::Out(*gid),
        Some(Carried::Out(_)) | Some(Carried::Lost) => Aboard::Lost,
    }
}

impl Db {
    /// The squad vehicle type `typ` carries for `side`, or `None` if it
    /// carries none.
    pub fn carrier_troop(&self, typ: &Vehicle, side: Side) -> Option<Troop> {
        let spec = self.ephemeral.cfg.dismount.get(typ)?;
        let template = spec.template.get(&side)?;
        Some(own_squad_troop(
            self.ephemeral.cfg.troops.get(&side),
            template.as_str(),
            spec.can_capture,
        ))
    }

    /// The unit `ucid` is driving from a Combined Arms slot, if any.
    pub fn driven_unit(&self, ucid: &Ucid) -> Option<UnitId> {
        self.ephemeral
            .ca_drivers
            .iter()
            .find(|(_, (driver, _))| driver == ucid)
            .and_then(|(oid, _)| self.ephemeral.uid_by_object_id.get(oid).copied())
    }

    /// The carriers `ucid` may order: the one they are driving, then the
    /// living carriers they deployed, by group.
    pub fn carriers_of(&self, ucid: &Ucid) -> Vec<UnitId> {
        let mut out: Vec<UnitId> = vec![];
        if let Some(uid) = self.driven_unit(ucid)
            && self.is_carrier(&uid)
        {
            out.push(uid);
        }
        let mut owned: Vec<(GroupId, UnitId)> = vec![];
        for gid in &self.persisted.deployed {
            let Some(g) = self.persisted.groups.get(gid) else { continue };
            if !matches!(&g.origin, DeployKind::Deployed { player, .. } if player == ucid)
            {
                continue;
            }
            for uid in &g.units {
                if !out.contains(uid) && self.is_carrier(uid) {
                    owned.push((*gid, *uid));
                }
            }
        }
        owned.sort_by_key(|(gid, uid)| (*gid, *uid));
        out.extend(owned.into_iter().map(|(_, uid)| uid));
        out
    }

    /// A living ground unit of a type that carries a squad for its side.
    pub fn is_carrier(&self, uid: &UnitId) -> bool {
        self.persisted
            .units
            .get(uid)
            .is_some_and(|u| !u.dead && self.carrier_troop(&u.typ, u.side).is_some())
    }

    /// May `ucid` order `uid`'s squad out or in? The player driving it, or
    /// the player who deployed it.
    fn may_order_carrier(&self, uid: &UnitId, ucid: &Ucid) -> bool {
        if self.driven_unit(ucid) == Some(*uid) {
            return true;
        }
        self.persisted
            .units
            .get(uid)
            .and_then(|u| self.persisted.groups.get(&u.group))
            .is_some_and(|g| {
                matches!(&g.origin, DeployKind::Deployed { player, .. } if player == ucid)
            })
    }

    /// Does group `gid` still have anyone alive?
    fn group_alive(&self, gid: &GroupId) -> bool {
        self.persisted.groups.get(gid).is_some_and(|g| {
            g.units
                .into_iter()
                .any(|u| self.persisted.units.get(u).is_some_and(|u| !u.dead))
        })
    }

    /// What `uid` has aboard.
    pub fn carrier_aboard(&self, uid: &UnitId) -> Result<Aboard> {
        let u = self.persisted.units.get(uid).ok_or_else(|| anyhow!("no unit {uid}"))?;
        let own = self
            .carrier_troop(&u.typ, u.side)
            .ok_or_else(|| anyhow!("a {} carries no troops", u.typ))?;
        Ok(aboard(own, self.persisted.carriers.get(uid), |g| self.group_alive(g)))
    }

    /// Does `uid` still carry the squad it started with? Only such a vehicle
    /// lets out a fresh squad when destroyed or sends one in on a formation
    /// assault.
    pub fn carries_own_squad(&self, uid: &UnitId) -> bool {
        self.persisted.carriers.get(uid).is_none()
    }

    /// `uid`'s squad has been used up (a formation assault sent it in).
    pub(super) fn carrier_squad_used(&mut self, uid: &UnitId) {
        self.persisted.carriers.insert_cow(*uid, Carried::Lost);
        self.ephemeral.dirty();
    }

    /// The live DCS unit behind `uid`, checked stopped: where it is, the way
    /// it faces (unit vector) and its heading.
    fn stopped_carrier(
        &self,
        lua: dcso3::MizLua,
        uid: &UnitId,
    ) -> Result<(Vector2, Vector2, f64)> {
        let u = self.persisted.units.get(uid).ok_or_else(|| anyhow!("no unit {uid}"))?;
        let unit = Unit::get_by_name(lua, u.name.as_str())
            .map_err(|_| anyhow!("{} {uid} is not in the world", u.typ))?;
        if unit.get_velocity()?.0.magnitude() > STOPPED_MPS {
            bail!("stop the {} first", u.typ)
        }
        let pos = unit.get_position()?;
        let fwd = Vector2::new(pos.x.x, pos.x.z);
        let fwd = if fwd.norm() > 1e-6 { fwd.normalize() } else { Vector2::new(1., 0.) };
        Ok((Vector2::new(pos.p.x, pos.p.z), fwd, azumith3d(pos.x.0)))
    }

    /// Put `uid`'s squad down behind it, for `ucid`. Returns the squad's
    /// type and its new group.
    pub fn dismount_squad(
        &mut self,
        lua: dcso3::MizLua,
        idx: &MizIndex,
        uid: &UnitId,
        ucid: &Ucid,
    ) -> Result<(Troop, GroupId)> {
        if !self.may_order_carrier(uid, ucid) {
            bail!("you can only order a vehicle you are driving or one you deployed")
        }
        let (side, typ) = {
            let u =
                self.persisted.units.get(uid).ok_or_else(|| anyhow!("no unit {uid}"))?;
            (u.side, u.typ.clone())
        };
        let it = match self.carrier_aboard(uid)? {
            Aboard::Own(troop) => InternalTroop {
                player: *ucid,
                origin: None,
                cost_fraction: 0.,
                troop,
                jtac: None,
                strength: None,
            },
            Aboard::Squad(it) => it,
            Aboard::Out(gid) => bail!(
                "the {typ}'s squad is already out as group {gid} -- embark it before it can deploy again"
            ),
            Aboard::Lost => {
                bail!("the {typ}'s squad was lost -- it has no troops to deploy")
            }
        };
        let (pos, fwd, heading) = self.stopped_carrier(lua, uid)?;
        let (n, oldest) = self.number_troops_deployed(side, it.troop.name.as_str())?;
        let to_delete = if n < it.troop.limit as usize {
            None
        } else {
            match it.troop.limit_enforce {
                LimitEnforceTyp::DeleteOldest => oldest,
                LimitEnforceTyp::DenyCrate => bail!(
                    "the maximum number of {} troops are already deployed",
                    it.troop.name
                ),
            }
        };
        // out of the back
        let spawnpos =
            SpawnLoc::AtPos { pos, offset_direction: -fwd, group_heading: heading };
        let dk = DeployKind::Troop {
            player: it.player,
            moved_by: None,
            spec: it.troop.clone(),
            origin: it.origin,
            cost_fraction: it.cost_fraction,
            jtac: it.jtac.clone(),
        };
        let spctx = SpawnCtx::new(lua)?;
        let gid = self.add_and_queue_group(
            &spctx,
            idx,
            side,
            spawnpos,
            &it.troop.template,
            dk,
            BitFlags::empty(),
            None,
        )?;
        if let Some(old) = to_delete {
            self.delete_group(&old)?
        }
        self.apply_troop_strength(gid, it.strength);
        self.persisted.carriers.insert_cow(*uid, Carried::Out(gid));
        self.ephemeral.dirty();
        self.ephemeral.stat(Stat::DeployTroop {
            gid,
            troop: it.troop.name.clone(),
            by: *ucid,
        });
        info!("[CARRIER] {ucid} put {} out of {typ} {uid} as group {gid}", it.troop.name);
        Ok((it.troop, gid))
    }

    /// Take `uid`'s own squad back aboard, for `ucid`: it must be out and
    /// within `EMBARK_RANGE_M`. Returns its type and strength.
    pub fn embark_squad(
        &mut self,
        lua: dcso3::MizLua,
        uid: &UnitId,
        ucid: &Ucid,
    ) -> Result<(Troop, Option<(u16, u16)>)> {
        if !self.may_order_carrier(uid, ucid) {
            bail!("you can only order a vehicle you are driving or one you deployed")
        }
        let typ = self
            .persisted
            .units
            .get(uid)
            .map(|u| u.typ.clone())
            .ok_or_else(|| anyhow!("no unit {uid}"))?;
        let sgid = match self.carrier_aboard(uid)? {
            Aboard::Out(gid) => gid,
            Aboard::Lost => {
                bail!("the {typ}'s squad was lost -- there is nobody to embark")
            }
            a => bail!("the {typ} already has {} aboard", a.describe()),
        };
        let (pos, _, _) = self.stopped_carrier(lua, uid)?;
        if let Some((_, o)) = self
            .persisted
            .objectives
            .into_iter()
            .find(|(_, o)| o.capture_hold.contains(&sgid))
        {
            bail!(
                "the squad is holding {} while it consolidates -- it can't embark until the capture is settled",
                o.name
            )
        }
        let g = group!(self, sgid)?;
        let dist = g
            .units
            .into_iter()
            .filter_map(|u| self.persisted.units.get(u))
            .filter(|u| !u.dead)
            .map(|u| (u.pos - pos).norm())
            .min_by(|a, b| a.total_cmp(b))
            .unwrap_or(f64::MAX);
        if dist > EMBARK_RANGE_M {
            bail!(
                "the squad (group {sgid}) is {dist:.0} m away -- bring it within {EMBARK_RANGE_M:.0} m of the {typ}"
            )
        }
        let strength = squad_strength(
            g.units
                .into_iter()
                .filter_map(|u| self.persisted.units.get(u))
                .map(|u| u.dead),
        );
        let it = match &g.origin {
            DeployKind::Troop { player, origin, spec, cost_fraction, jtac, .. } => {
                InternalTroop {
                    player: *player,
                    origin: *origin,
                    cost_fraction: *cost_fraction,
                    troop: spec.clone(),
                    jtac: jtac.clone(),
                    strength,
                }
            }
            _ => bail!("group {sgid} is not a troop squad"),
        };
        self.delete_group(&sgid)?;
        let troop = it.troop.clone();
        self.persisted.carriers.insert_cow(*uid, Carried::Squad(it));
        self.ephemeral.dirty();
        info!("[CARRIER] {ucid} embarked group {sgid} ({}) into {typ} {uid}", troop.name);
        Ok((troop, strength))
    }

    /// What `uid` had aboard as it died, forgotten from here on: `None` if
    /// it still had the squad it started with.
    pub(crate) fn take_carried(&mut self, uid: &UnitId) -> Option<Carried> {
        let c = self.persisted.carriers.remove_cow(uid);
        if c.is_some() {
            self.ephemeral.dirty();
        }
        c
    }

    /// Let embarked squad `it` out of a carrier destroyed at `pos`.
    pub(crate) fn spawn_embarked_squad(
        &mut self,
        lua: dcso3::MizLua,
        idx: &MizIndex,
        side: Side,
        it: InternalTroop,
        pos: Vector2,
        heading: f64,
    ) -> Result<GroupId> {
        let spawnpos = SpawnLoc::AtPos {
            pos,
            offset_direction: Vector2::new(heading.sin(), heading.cos()),
            group_heading: heading,
        };
        let dk = DeployKind::Troop {
            player: it.player,
            moved_by: None,
            spec: it.troop.clone(),
            origin: it.origin,
            cost_fraction: it.cost_fraction,
            jtac: it.jtac.clone(),
        };
        let spctx = SpawnCtx::new(lua)?;
        let gid = self.add_and_queue_group(
            &spctx,
            idx,
            side,
            spawnpos,
            &it.troop.template,
            dk,
            BitFlags::empty(),
            None,
        )?;
        self.apply_troop_strength(gid, it.strength);
        Ok(gid)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn troop(name: &str, template: &str) -> Troop {
        Troop {
            name: name.into(),
            template: template.into(),
            persist: PersistTyp::Forever,
            can_capture: true,
            limit: 4,
            limit_enforce: LimitEnforceTyp::DenyCrate,
            weight: 800,
            cost: 0,
            jtac: None,
        }
    }

    fn squad(strength: Option<(u16, u16)>) -> InternalTroop {
        InternalTroop {
            player: Ucid::default(),
            origin: None,
            cost_fraction: 0.,
            troop: troop("Standard", "T"),
            jtac: None,
            strength,
        }
    }

    #[test]
    fn a_carrier_squad_is_the_sides_troop_with_its_template() {
        let ts =
            vec![troop("Anti Tank", "BATTROOP"), troop("Standard", "BSTANDARDTROOP")];
        let t = own_squad_troop(Some(&ts), "BSTANDARDTROOP", false);
        assert_eq!(t.name.as_str(), "Standard");
        // shares that troop's limit
        assert_eq!(t.limit, 4);
        // no such troop: one made from the dismount spec, free
        let t = own_squad_troop(Some(&ts), "BRIFLES", false);
        assert_eq!((t.name.as_str(), t.template.as_str()), ("Dismounts", "BRIFLES"));
        assert!(!t.can_capture);
        assert_eq!(t.cost, 0);
        assert!(own_squad_troop(None, "X", true).can_capture);
    }

    #[test]
    fn one_squad_per_carrier_never_duplicated() {
        let gid: GroupId = "7".parse().unwrap();
        let own = || troop("Standard", "T");
        // as it started: loaded
        assert!(aboard(own(), None, |_| true).loaded());
        // squad out and alive: not loaded, nothing to put down
        let out = Carried::Out(gid);
        assert!(
            matches!(aboard(own(), Some(&out), |_| true), Aboard::Out(g) if g == gid)
        );
        // squad out and killed: lost for good, not reloaded
        assert!(matches!(aboard(own(), Some(&out), |_| false), Aboard::Lost));
        assert!(matches!(aboard(own(), Some(&Carried::Lost), |_| true), Aboard::Lost));
        // embarked again: loaded, with its casualties
        let back = Carried::Squad(squad(Some((3, 8))));
        let a = aboard(own(), Some(&back), |_| true);
        assert!(a.loaded());
        assert_eq!(a.describe(), "a Standard squad (3 of 8)");
    }

    #[test]
    fn aboard_says_what_is_in_it() {
        assert_eq!(Aboard::Own(troop("Standard", "T")).describe(), "a Standard squad");
        assert_eq!(Aboard::Squad(squad(None)).describe(), "a Standard squad");
        assert!(Aboard::Out("12".parse().unwrap()).describe().contains("group 12"));
        assert!(Aboard::Lost.describe().contains("lost"));
    }
}
