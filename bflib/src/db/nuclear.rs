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

//! A side's last card (`StoryEffect::NuclearStrike`): when a story event
//! orders it, an AI aircraft takes off and flies a nuclear weapon at the
//! enemy's most dangerous push. The enemy is warned the moment it is
//! airborne and can shoot it down; if it reaches the target the engine sets
//! off the blast itself (DCS's AI would only drop a dummy store).
//!
//! Once a round: the order, the flight and the outcome are in the save
//! (`Persisted::nuclear`). A flight lost to a restart (the engine drops
//! session-spawned AI on load) is launched again, not counted as shot down.

use super::{Db, group::DeployKind};
use crate::{
    airlife::{air_point, dist, heading_of},
    spawnctx::SpawnCtx,
};
use anyhow::{Context, Result};
use bfprotocols::{
    cfg::NuclearStrikeCfg,
    db::{
        group::{GroupId, UnitId},
        objective::ObjectiveId,
    },
    perf::PerfInner,
    stats::{Stat, StoryEventStat},
};
use chrono::{Duration, prelude::*};
use compact_str::{CompactString, format_compact};
use dcso3::{
    LuaVec2, LuaVec3, MizLua, Vector2, Vector3,
    coalition::Side,
    controller::{AiOption, AirOption, AirRoe, Task},
    land::Land,
    trigger::Trigger,
};
use log::{info, warn};
use serde_derive::{Deserialize, Serialize};
use smallvec::SmallVec;

use crate::db::MizIndex;

/// Where the strike is.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Phase {
    /// Ordered by a story event; launches on the next pass.
    Ordered,
    /// In the air.
    Airborne {
        strike: GroupId,
        escort: Option<GroupId>,
        target: ObjectiveId,
        target_name: String,
        pos: Vector2,
        launched: DateTime<Utc>,
    },
    Detonated {
        target_name: String,
        at: DateTime<Utc>,
        destroyed: u32,
    },
    Intercepted {
        target_name: String,
        at: DateTime<Utc>,
    },
    /// Nothing could be launched or nothing to strike.
    Failed {
        why: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NuclearState {
    pub cfg: NuclearStrikeCfg,
    pub ordered: DateTime<Utc>,
    pub phase: Phase,
    /// The groups to remove once they have hung around long enough.
    #[serde(default)]
    pub cleanup: Vec<(GroupId, DateTime<Utc>)>,
}

/// A candidate target: an objective the enemy holds.
#[derive(Debug, Clone)]
pub(crate) struct Candidate {
    pub oid: ObjectiveId,
    pub pos: Vector2,
    /// Enemy ground units within 5 km of it.
    pub enemy_near: u32,
    /// The striking side's own ground units within the kill radius.
    pub own_near: u32,
    /// Distance to the striking side's nearest objective.
    pub from_own: f64,
}

/// The enemy's most dangerous push: the most enemy ground forces round an
/// objective, least of our own under the blast, nearest our ground. None
/// when nothing is in reach.
pub(crate) fn pick_target(cands: &[Candidate], max_range_m: f64) -> Option<usize> {
    cands
        .iter()
        .enumerate()
        .filter(|(_, c)| c.from_own <= max_range_m)
        .max_by(|(_, a), (_, b)| {
            let score = |c: &Candidate| {
                c.enemy_near as f64 - 3. * c.own_near as f64 - c.from_own / 20_000.
            };
            score(a).total_cmp(&score(b))
        })
        .map(|(i, _)| i)
}

/// Is a unit at `d` metres from the burst destroyed? `roll` in [0, 1).
pub(crate) fn destroyed(cfg: &NuclearStrikeCfg, d: f64, roll: f64) -> bool {
    d <= cfg.kill_radius_m || (d <= cfg.damage_radius_m && roll < cfg.damage_chance)
}

/// A repeatable roll in [0, 1) for one unit of one blast.
fn roll(uid: UnitId, at: DateTime<Utc>) -> f64 {
    let mut x = (uid.inner() as u64)
        ^ (at.timestamp() as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15);
    x ^= x >> 33;
    x = x.wrapping_mul(0xff51_afd7_ed55_8ccd);
    x ^= x >> 33;
    (x % 10_000) as f64 / 10_000.
}

impl Db {
    /// `StoryEffect::NuclearStrike`: order it, once a round.
    pub(super) fn order_nuclear_strike(
        &mut self,
        cfg: &NuclearStrikeCfg,
        now: DateTime<Utc>,
    ) -> CompactString {
        if let Some(n) = &self.persisted.nuclear {
            return format_compact!(
                "a nuclear strike was already ordered this round ({:?})",
                n.cfg.side
            );
        }
        self.persisted.nuclear = Some(NuclearState {
            cfg: cfg.clone(),
            ordered: now,
            phase: Phase::Ordered,
            cleanup: vec![],
        });
        self.ephemeral.dirty();
        info!("[NUCLEAR] {:?} ordered a nuclear strike", cfg.side);
        format_compact!("{:?} orders a nuclear strike", cfg.side)
    }

    fn group_alive_any(&self, gid: &GroupId) -> bool {
        self.persisted.groups.get(gid).is_some_and(|g| {
            g.units
                .into_iter()
                .any(|u| self.persisted.units.get(u).is_some_and(|u| !u.dead))
        })
    }

    fn group_pos(&self, gid: &GroupId) -> Option<Vector2> {
        let g = self.persisted.groups.get(gid)?;
        g.units
            .into_iter()
            .filter_map(|u| self.persisted.units.get(u))
            .find(|u| !u.dead)
            .map(|u| u.pos)
    }

    fn nuclear_candidates(&self, side: Side, kill_radius_m: f64) -> Vec<Candidate> {
        let enemy = side.opposite();
        let own: SmallVec<[Vector2; 64]> = self
            .objectives()
            .filter(|(_, o)| o.owner() == side && !crate::groundwar::at_sea(o.kind()))
            .map(|(_, o)| o.pos())
            .collect();
        let ground: Vec<(Side, Vector2)> = self
            .persisted
            .groups
            .into_iter()
            .filter(|(_, g)| {
                matches!(g.kind, Some(dcso3::group::GroupCategory::Ground))
                    && matches!(
                        g.origin,
                        DeployKind::Objective { .. }
                            | DeployKind::Deployed { .. }
                            | DeployKind::Troop { .. }
                            | DeployKind::Dismount { .. }
                    )
            })
            .flat_map(|(_, g)| {
                g.units
                    .into_iter()
                    .filter_map(|u| self.persisted.units.get(u))
                    .filter(|u| !u.dead)
                    .map(|u| (u.side, u.pos))
                    .collect::<SmallVec<[(Side, Vector2); 16]>>()
            })
            .collect();
        self.objectives()
            .filter(|(_, o)| o.owner() == enemy && !crate::groundwar::at_sea(o.kind()))
            .map(|(oid, o)| {
                let p = o.pos();
                let enemy_near = ground
                    .iter()
                    .filter(|(s, q)| *s == enemy && dist(p, *q) <= 5000.)
                    .count();
                let own_near = ground
                    .iter()
                    .filter(|(s, q)| *s == side && dist(p, *q) <= kill_radius_m)
                    .count();
                let from_own =
                    own.iter().map(|q| dist(p, *q)).fold(f64::INFINITY, f64::min);
                Candidate {
                    oid: *oid,
                    pos: p,
                    enemy_near: enemy_near as u32,
                    own_near: own_near as u32,
                    from_own,
                }
            })
            .collect()
    }

    /// One pass: launch, follow, detonate or record the interception.
    pub fn nuclear_tick(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        perf: &mut PerfInner,
        now: DateTime<Utc>,
    ) -> Result<()> {
        // groups left over from a strike go after half an hour
        if let Some(n) = self.persisted.nuclear.as_mut() {
            let due: SmallVec<[GroupId; 2]> =
                n.cleanup.iter().filter(|(_, t)| now >= *t).map(|(g, _)| *g).collect();
            n.cleanup.retain(|(_, t)| now < *t);
            for g in due {
                if self.persisted.groups.get(&g).is_some() {
                    let _ = self.delete_group(&g);
                }
            }
        }
        let Some(state) = self.persisted.nuclear.clone() else { return Ok(()) };
        match state.phase {
            Phase::Ordered => self.nuclear_launch(lua, idx, perf, state.cfg, now),
            Phase::Airborne { strike, escort, target, target_name, pos, launched } => {
                if !self.ephemeral.nuclear_launched {
                    // a restart dropped the flight: fly it again
                    info!(
                        "[NUCLEAR] the strike flight did not survive a restart: launching it again"
                    );
                    for g in [Some(strike), escort].into_iter().flatten() {
                        if self.persisted.groups.get(&g).is_some() {
                            let _ = self.delete_group(&g);
                        }
                    }
                    if let Some(n) = self.persisted.nuclear.as_mut() {
                        n.phase = Phase::Ordered;
                    }
                    self.ephemeral.dirty();
                    return Ok(());
                }
                let cfg = state.cfg;
                if !self.group_alive_any(&strike) {
                    return self.nuclear_intercepted(&cfg, escort, &target_name, now);
                }
                if let Some(p) = self.group_pos(&strike)
                    && dist(p, pos) <= cfg.release_m
                {
                    return self.nuclear_detonate(
                        lua,
                        &cfg,
                        strike,
                        escort,
                        target,
                        &target_name,
                        pos,
                        now,
                    );
                }
                // A run that has not arrived in 90 minutes is lost (stuck,
                // or flew off): call it intercepted, so the round moves on.
                if now - launched > Duration::minutes(90) {
                    warn!(
                        "[NUCLEAR] the strike flight never reached {target_name}: calling it off"
                    );
                    return self.nuclear_intercepted(&cfg, escort, &target_name, now);
                }
                Ok(())
            }
            Phase::Detonated { .. }
            | Phase::Intercepted { .. }
            | Phase::Failed { .. } => Ok(()),
        }
    }

    fn nuclear_launch(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        perf: &mut PerfInner,
        cfg: NuclearStrikeCfg,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let side = cfg.side;
        let cands = self.nuclear_candidates(side, cfg.kill_radius_m);
        let Some(i) = pick_target(&cands, cfg.max_range_m) else {
            return self.nuclear_fail("no enemy objective within reach");
        };
        let tgt = cands[i].clone();
        let tname = self
            .persisted
            .objectives
            .get(&tgt.oid)
            .map(|o| o.name().to_string())
            .unwrap_or_default();
        let Some(field) =
            self.pick_launch_field(lua, side, tgt.pos, false, false, 30_000.)
        else {
            // try again on the next pass: every field may just be full
            warn!("[NUCLEAR] no {side:?} airfield can launch the strike yet");
            return Ok(());
        };
        let fname = self
            .persisted
            .objectives
            .get(&field.oid)
            .map(|o| o.name().to_string())
            .unwrap_or_default();
        let land = Land::singleton(lua)?;
        let elev = |p: Vector2| land.get_height(LuaVec2(p)).unwrap_or(0.);
        let alt = |p: Vector2| (elev(p) + 150.).max(cfg.altitude_m);
        let hold = || {
            Task::ComboTask(vec![Task::WrappedOption(AiOption::Air(AirOption::Roe(
                AirRoe::WeaponHold,
            )))])
        };
        let free = || {
            Task::ComboTask(vec![Task::WrappedOption(AiOption::Air(AirOption::Roe(
                AirRoe::WeaponFree,
            )))])
        };
        // field -> target -> on past it, where the run ends
        let beyond = tgt.pos + (tgt.pos - field.pos).normalize() * 20_000.;
        let route = |task: &dyn Fn() -> Task| {
            vec![
                air_point(field.pos, elev(field.pos), cfg.speed_mps, task()),
                air_point(tgt.pos, alt(tgt.pos), cfg.speed_mps, Task::ComboTask(vec![])),
                air_point(beyond, alt(beyond), cfg.speed_mps, Task::ComboTask(vec![])),
            ]
        };
        let spctx = SpawnCtx::new(lua)?;
        let heading = heading_of(tgt.pos - field.pos);
        let strike = self
            .spawn_air_flight(
                perf,
                &spctx,
                idx,
                side,
                &cfg.template,
                field.oid,
                field.pos,
                heading,
                false,
                route(&hold),
            )
            .with_context(|| {
                format_compact!("spawning the strike aircraft {}", cfg.template)
            })?;
        let escort = match cfg.escort_template.as_ref() {
            None => None,
            Some(t) => match self.spawn_air_flight(
                perf,
                &spctx,
                idx,
                side,
                t,
                field.oid,
                field.pos,
                heading,
                false,
                route(&free),
            ) {
                Ok(g) => Some(g),
                Err(e) => {
                    warn!("[NUCLEAR] escort {t} would not spawn: {e:?}");
                    None
                }
            },
        };
        self.ephemeral.nuclear_launched = true;
        if let Some(n) = self.persisted.nuclear.as_mut() {
            n.phase = Phase::Airborne {
                strike,
                escort,
                target: tgt.oid,
                target_name: tname.clone(),
                pos: tgt.pos,
                launched: now,
            };
        }
        self.ephemeral.dirty();
        let km = dist(field.pos, tgt.pos) / 1000.;
        info!(
            "[NUCLEAR] {side:?} {} airborne from {fname} for {tname} ({km:.0} km), {} enemy ground units near it",
            cfg.aircraft, tgt.enemy_near
        );
        let enemy = side.opposite();
        let msgs = self.ephemeral.msgs();
        msgs.panel_to_side(
            60,
            true,
            enemy,
            format_compact!(
                "FLASH -- a nuclear-armed {} is airborne from {fname}, heading for {tname} ({km:.0} km). Shoot it down.",
                cfg.aircraft
            ),
        );
        msgs.panel_to_side(
            60,
            true,
            side,
            format_compact!(
                "THE LAST STAND: a nuclear-armed {} is airborne from {fname} for {tname}. Keep it alive.",
                cfg.aircraft
            ),
        );
        Ok(())
    }

    fn nuclear_fail(&mut self, why: &str) -> Result<()> {
        warn!("[NUCLEAR] strike called off: {why}");
        if let Some(n) = self.persisted.nuclear.as_mut() {
            n.phase = Phase::Failed { why: why.into() };
        }
        self.ephemeral.dirty();
        Ok(())
    }

    fn nuclear_intercepted(
        &mut self,
        cfg: &NuclearStrikeCfg,
        escort: Option<GroupId>,
        target_name: &str,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let side = cfg.side;
        let enemy = side.opposite();
        info!("[NUCLEAR] the {side:?} strike on {target_name} was stopped");
        if cfg.will_on_intercept != 0. {
            self.will_story(side, cfg.will_on_intercept, "nuclear strike shot down", now);
        }
        // the enemy pilots in the air when it went down: credited, and named
        // in the record
        let up: SmallVec<[_; 16]> = self
            .instanced_players()
            .filter(|(_, p, i)| p.side == enemy && i.in_air)
            .map(|(u, p, _)| (*u, p.name.clone()))
            .collect();
        if cfg.intercept_points != 0 {
            for (u, _) in &up {
                self.adjust_points(
                    u,
                    cfg.intercept_points,
                    "for stopping the nuclear strike",
                );
            }
        }
        if let Some(n) = self.persisted.nuclear.as_mut() {
            n.phase = Phase::Intercepted { target_name: target_name.into(), at: now };
            if let Some(e) = escort {
                n.cleanup.push((e, now + Duration::minutes(30)));
            }
        }
        self.ephemeral.dirty();
        self.ephemeral.msgs().panel_to_all(
            45,
            true,
            format_compact!("The nuclear strike on {target_name} has been stopped: the aircraft is down."),
        );
        self.ephemeral.stat(last_stand_stat(
            "intercepted",
            now,
            side,
            format!("The Last Stand: the {side:?} strike on {target_name} is shot down"),
            up.into_iter().map(|(_, n)| n).collect(),
        ));
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn nuclear_detonate(
        &mut self,
        lua: MizLua,
        cfg: &NuclearStrikeCfg,
        strike: GroupId,
        escort: Option<GroupId>,
        target: ObjectiveId,
        target_name: &str,
        pos: Vector2,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let side = cfg.side;
        let land = Land::singleton(lua)?;
        let act = Trigger::singleton(lua)?.action()?;
        let ground = land.get_height(LuaVec2(pos))?;
        // the burst, and the ring round it, for everyone to see
        act.explosion(LuaVec3(Vector3::new(pos.x, ground + 300., pos.y)), 5000.)?;
        for k in 0..8 {
            let a = std::f64::consts::TAU * k as f64 / 8.;
            let p = pos + Vector2::new(a.cos(), a.sin()) * (cfg.kill_radius_m * 0.6);
            let g = land.get_height(LuaVec2(p)).unwrap_or(ground);
            act.explosion(LuaVec3(Vector3::new(p.x, g + 5., p.y)), 800.)?;
        }
        // Every ground unit and ship in the blast, both sides. One that is in
        // DCS gets a charge of its own (DCS kills it, and its death goes
        // through the usual events); one that is not is struck off here.
        let mut hit: Vec<(UnitId, Vector2, bool)> = vec![];
        for (gid, g) in &self.persisted.groups {
            if *gid == strike || Some(*gid) == escort {
                continue;
            }
            if !matches!(
                g.kind,
                Some(dcso3::group::GroupCategory::Ground)
                    | Some(dcso3::group::GroupCategory::Ship)
            ) {
                continue;
            }
            for uid in &g.units {
                let Some(u) = self.persisted.units.get(uid) else { continue };
                if u.dead {
                    continue;
                }
                let d = dist(u.pos, pos);
                if destroyed(cfg, d, roll(*uid, now)) {
                    hit.push((
                        *uid,
                        u.pos,
                        self.ephemeral.object_id_by_uid.contains_key(uid),
                    ));
                }
            }
        }
        // nobody climbs out of a carrier caught in the burst: its squad is
        // gone with it (a wreck otherwise lets its squad out)
        for (uid, _, _) in &hit {
            if self.is_carrier(uid) {
                self.carrier_squad_used(uid);
            }
        }
        let mut struck = 0u32;
        for (uid, p, live) in &hit {
            if *live {
                let g = land.get_height(LuaVec2(*p)).unwrap_or(ground);
                if let Err(e) =
                    act.explosion(LuaVec3(Vector3::new(p.x, g + 1., p.y)), 600.)
                {
                    warn!("[NUCLEAR] charge on unit {uid:?}: {e:?}");
                }
            } else if let Some(u) = self.persisted.units.get_mut_cow(uid) {
                u.dead = true;
                struck += 1;
            }
        }
        // the objectives in the blast lose what was struck off
        let near: SmallVec<[ObjectiveId; 4]> = self
            .objectives()
            .filter(|(_, o)| dist(o.pos(), pos) <= cfg.damage_radius_m + 2000.)
            .map(|(oid, _)| *oid)
            .collect();
        for oid in near.iter().chain(std::iter::once(&target)) {
            if let Err(e) = self.update_objective_status(oid, now) {
                warn!("[NUCLEAR] objective status after the blast: {e:?}");
            }
        }
        let n = hit.len() as u32;
        info!(
            "[NUCLEAR] {side:?} detonation over {target_name}: {n} units in the blast ({struck} not in DCS struck off)"
        );
        if cfg.will_on_detonation != 0. {
            self.will_story(side, cfg.will_on_detonation, "the nuclear strike", now);
        }
        if cfg.enemy_will_on_detonation != 0. {
            self.will_story(
                side.opposite(),
                cfg.enemy_will_on_detonation,
                "the nuclear strike",
                now,
            );
        }
        if let Some(st) = self.persisted.nuclear.as_mut() {
            st.phase = Phase::Detonated {
                target_name: target_name.into(),
                at: now,
                destroyed: n,
            };
            st.cleanup.push((strike, now + Duration::minutes(10)));
            if let Some(e) = escort {
                st.cleanup.push((e, now + Duration::minutes(30)));
            }
        }
        self.ephemeral.dirty();
        self.ephemeral.msgs().panel_to_all(
            60,
            true,
            format_compact!(
                "NUCLEAR DETONATION over {target_name}: {n} units destroyed in the blast."
            ),
        );
        self.ephemeral.stat(last_stand_stat(
            "detonated",
            now,
            side,
            format!("The Last Stand: {side:?} nuclear detonation over {target_name}, {n} units destroyed"),
            vec![],
        ));
        Ok(())
    }
}

/// The Last Stand's outcome as a `Stat::StoryEvent`, so bfdb has it for the
/// news and the weekly report: id `last_stand_intercepted` or
/// `last_stand_detonated`. Sent once, when the outcome is decided (the
/// order itself is the story event that called it).
fn last_stand_stat(
    outcome: &str,
    now: DateTime<Utc>,
    side: Side,
    title: String,
    caused_by: Vec<dcso3::String>,
) -> Stat {
    Stat::StoryEvent(StoryEventStat {
        id: format!("last_stand_{outcome}").into(),
        time: now,
        title: title.into(),
        text: "".into(),
        caused_by,
        side: Some(side),
        effects: vec![],
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cfg() -> NuclearStrikeCfg {
        serde_json::from_str(r#"{"side": "Red", "template": "RNUKE"}"#).unwrap()
    }

    fn cand(i: i64, enemy: u32, own: u32, from_own_km: f64) -> Candidate {
        Candidate {
            oid: ObjectiveId::from(i),
            pos: Vector2::new(0., 0.),
            enemy_near: enemy,
            own_near: own,
            from_own: from_own_km * 1000.,
        }
    }

    #[test]
    fn defaults_are_a_tactical_weapon() {
        let c = cfg();
        assert_eq!(c.kill_radius_m, 1200.);
        assert_eq!(c.damage_radius_m, 2500.);
        assert_eq!(c.release_m, 3000.);
        assert_eq!(c.max_range_m, 150_000.);
        assert!(c.escort_template.is_none());
    }

    #[test]
    fn the_target_is_the_biggest_push_in_reach() {
        let cands = [cand(1, 10, 0, 40.), cand(2, 30, 0, 60.), cand(3, 80, 0, 400.)];
        // 3 is out of reach; 2 has the most enemy around it
        assert_eq!(pick_target(&cands, 150_000.), Some(1));
    }

    #[test]
    fn it_spares_its_own_troops() {
        // as many enemies at both, but our own squads are in 2's blast
        let cands = [cand(1, 20, 0, 50.), cand(2, 24, 4, 50.)];
        assert_eq!(pick_target(&cands, 150_000.), Some(0));
    }

    #[test]
    fn nothing_in_reach_means_no_strike() {
        assert_eq!(pick_target(&[cand(1, 50, 0, 200.)], 150_000.), None);
        assert_eq!(pick_target(&[], 150_000.), None);
    }

    #[test]
    fn the_blast_kills_inside_and_maybe_outside() {
        let c = cfg();
        assert!(destroyed(&c, 0., 0.99));
        assert!(destroyed(&c, 1200., 0.99));
        assert!(destroyed(&c, 2000., 0.1));
        assert!(!destroyed(&c, 2000., 0.9));
        assert!(!destroyed(&c, 2600., 0.0));
    }

    #[test]
    fn rolls_are_repeatable_and_spread() {
        let t = DateTime::parse_from_rfc3339("2026-11-10T12:00:00Z")
            .unwrap()
            .with_timezone(&Utc);
        let a: Vec<f64> = (0..200).map(|i| roll(UnitId::from(i), t)).collect();
        assert_eq!(a[7], roll(UnitId::from(7), t));
        assert!(a.iter().all(|r| (0. ..1.).contains(r)));
        let under = a.iter().filter(|r| **r < 0.5).count();
        assert!((60..140).contains(&under), "{under} of 200 under 0.5");
    }

    #[test]
    fn last_stand_outcome_is_a_story_event() {
        let t = DateTime::parse_from_rfc3339("2026-11-10T12:00:00Z")
            .unwrap()
            .with_timezone(&Utc);
        let Stat::StoryEvent(ev) = last_stand_stat(
            "intercepted",
            t,
            Side::Red,
            "shot down".into(),
            vec![dcso3::String::from("Viper")],
        ) else {
            panic!("not a story event")
        };
        assert_eq!(&*ev.id, "last_stand_intercepted");
        assert_eq!(ev.side, Some(Side::Red));
        assert_eq!(ev.caused_by, vec![dcso3::String::from("Viper")]);
        assert_eq!(ev.time, t);
    }
}
