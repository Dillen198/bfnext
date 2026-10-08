/*
Copyright 2024 Eric Stokes.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Affero Public License for more
details.
*/

//! Base garrisons that defend themselves (`GarrisonCfg`).
//!
//! A base's armour and infantry used to stand where the mission put them,
//! with no route, for as long as the base was live. One player tank could
//! park outside their reach and take the whole objective apart. Now the
//! garrison drives out to meet an intruder, calls a formation in as a quick
//! reaction force if it stays, and moves about inside the base while it is
//! quiet.

use super::{
    formation::{ground_point, FormationId, FormationRt, Order, Posture},
    objective::ObjGroupClass,
    Db,
};
use bfprotocols::{
    cfg::{GarrisonCfg, UnitTag},
    db::{group::GroupId, objective::ObjectiveId},
};
use chrono::{prelude::*, Duration};
use compact_str::format_compact;
use dcso3::{
    coalition::Side,
    controller::{AiOption, AlarmState, GroundOption, GroundRoe, Task, VehicleFormation},
    group::{Group, GroupCategory},
    land::{Land, SurfaceType},
    LuaVec2, MizLua, Vector2,
};
use fxhash::FxHashMap;
use log::{info, warn};
use rand::Rng;
use smallvec::SmallVec;

/// Run at most this often.
const TICK_SECS: i64 = 15;
/// Re-aim the garrison when the intruder has moved this far, or this long
/// after the last route.
const REAIM_M: f64 = 800.;
const REROUTE_SECS: i64 = 60;
/// Spacing across the line of approach between the groups sent out.
const SPREAD_M: f64 = 250.;
/// Stop short of the intruder: they fight from here, they don't ram it.
const STANDOFF_M: f64 = 600.;
/// Speeds, m/s: out to fight, and the stroll of a patrol.
const REACT_MPS: f64 = 9.;
const PATROL_MPS: f64 = 4.;
/// Patrol points lie within this share of the base's radius.
const PATROL_RADIUS: f64 = 0.6;
/// A formation this close to a base already covers it.
const COVER_M: f64 = 3_000.;
/// After a QRF call (answered or not), wait this long before another.
const QRF_RETRY_SECS: i64 = 300;

fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

#[derive(Debug, Default)]
struct BaseState {
    /// This episode: when an intruder was first and last seen.
    first_seen: Option<DateTime<Utc>>,
    last_seen: Option<DateTime<Utc>>,
    /// Where the garrison was last sent to fight, and when.
    aim: Option<Vector2>,
    routed_at: Option<DateTime<Utc>>,
    /// Away from its spawn positions (fighting or patrolling).
    out: bool,
    next_patrol: Option<DateTime<Utc>>,
    qrf_at: Option<DateTime<Utc>>,
    qrf: Option<FormationId>,
    announced: bool,
}

#[derive(Debug, Default)]
pub struct GarrisonRt {
    last_tick: Option<DateTime<Utc>>,
    bases: FxHashMap<ObjectiveId, BaseState>,
    /// SAM groups: last launch, last move, next unprovoked move.
    sam_shot: FxHashMap<GroupId, DateTime<Utc>>,
    sam_moved: FxHashMap<GroupId, DateTime<Utc>>,
    sam_next: FxHashMap<GroupId, DateTime<Utc>>,
}

impl GarrisonRt {
    pub fn note_sam_shot(&mut self, gid: GroupId, now: DateTime<Utc>) {
        self.sam_shot.insert(gid, now);
    }
}

impl Db {
    /// The live garrison groups of `oid` that can go out: armour and
    /// infantry, in DCS, with something alive that drives.
    fn sortie_groups(&self, oid: &ObjectiveId) -> SmallVec<[GroupId; 8]> {
        let Some(o) = self.persisted.objectives.get(oid) else { return SmallVec::new() };
        let Some(gids) = o.groups.get(&o.owner) else { return SmallVec::new() };
        gids.into_iter()
            .copied()
            .filter(|gid| {
                self.persisted.groups.get(gid).map_or(false, |g| {
                    matches!(g.class, ObjGroupClass::Armor | ObjGroupClass::Infantry)
                        && g.kind == Some(GroupCategory::Ground)
                        && g.units.into_iter().any(|u| {
                            self.persisted.units.get(u).map_or(false, |u| !u.dead)
                                && self.ephemeral.object_id_by_uid.contains_key(u)
                        })
                })
            })
            .filter(|gid| self.group_can_drive(gid))
            .collect()
    }

    /// Armed enemy ground units in DCS that aren't sitting in their own
    /// base's garrison: player and AI vehicles, deployed units, troops,
    /// formations. (side, position).
    fn ground_intruders(&self) -> Vec<(Side, Vector2)> {
        self.ephemeral
            .object_id_by_uid
            .keys()
            .filter_map(|uid| self.persisted.units.get(uid))
            .filter(|u| !u.dead && u.side != Side::Neutral && !u.tags.contains(UnitTag::Unarmed))
            .filter(|u| self.persisted.objectives_by_group.get(&u.group).is_none())
            .filter(|u| {
                self.persisted
                    .groups
                    .get(&u.group)
                    .map_or(false, |g| g.kind == Some(GroupCategory::Ground))
            })
            .map(|u| (u.side, u.pos))
            .collect()
    }

    /// Drive each of `gids` to its own point; `alarm` red to fight.
    fn route_garrison(
        &mut self,
        lua: MizLua,
        land: &Land,
        legs: &[(GroupId, Vector2)],
        speed: f64,
        fight: bool,
    ) {
        for (gid, to) in legs {
            let Some(g) = self.persisted.groups.get(gid) else { continue };
            let Ok(from) = self.group_center(gid) else { continue };
            let uids: SmallVec<[_; 16]> = g.units.into_iter().copied().collect();
            let name = g.name.clone();
            let res = Group::get_by_name(lua, name.as_str()).and_then(|group| {
                let con = group.get_controller()?;
                con.set_option(AiOption::Ground(GroundOption::DisperseOnAttack(0)))?;
                con.set_option(AiOption::Ground(GroundOption::Roe(GroundRoe::WeaponFree)))?;
                con.set_option(AiOption::Ground(GroundOption::AlarmState(if fight {
                    AlarmState::Red
                } else {
                    AlarmState::Auto
                })))?;
                let formation = if fight { VehicleFormation::Rank } else { VehicleFormation::OffRoad };
                let route = vec![
                    ground_point(land, from, VehicleFormation::OffRoad, speed, Task::ComboTask(vec![])),
                    ground_point(land, *to, formation, speed, Task::ComboTask(vec![])),
                ];
                con.set_task(Task::Mission { airborne: Some(false), route })
            });
            match res {
                Ok(()) => {
                    // Read their positions while they move: capture, the
                    // command map and the next respawn all go by them.
                    for uid in uids {
                        self.ephemeral.units_able_to_move.insert(uid);
                    }
                }
                Err(e) => warn!("garrison: routing {name}: {e:?}"),
            }
        }
    }

    /// The live SAM groups of `oid` that can relocate: every vehicle in them
    /// drives.
    fn mobile_sams(&self, oid: &ObjectiveId) -> SmallVec<[GroupId; 8]> {
        let Some(o) = self.persisted.objectives.get(oid) else { return SmallVec::new() };
        let Some(gids) = o.groups.get(&o.owner) else { return SmallVec::new() };
        gids.into_iter()
            .copied()
            .filter(|gid| {
                self.persisted.groups.get(gid).map_or(false, |g| {
                    matches!(g.class, ObjGroupClass::Lr | ObjGroupClass::Mr | ObjGroupClass::Sr)
                        && g.kind == Some(GroupCategory::Ground)
                        && g.tags.contains(UnitTag::SAM)
                        && g.units.into_iter().any(|u| self.ephemeral.object_id_by_uid.contains_key(u))
                })
            })
            .filter(|gid| self.group_can_drive(gid))
            .collect()
    }

    /// Move the base's mobile SAM sites that are due: after they have
    /// fired and gone quiet, or now and then unprovoked.
    fn relocate_sams(
        &mut self,
        grt: &mut GarrisonRt,
        lua: MizLua,
        land: &Land,
        cfg: &GarrisonCfg,
        oid: ObjectiveId,
        center: Vector2,
        radius: f64,
        now: DateTime<Utc>,
    ) {
        let mut rng = rand::thread_rng();
        let avg = cfg.sam_redeploy_secs.max(300) as f64;
        for gid in self.mobile_sams(&oid) {
            let shot = grt.sam_shot.get(&gid).copied();
            let moved = grt.sam_moved.get(&gid).copied();
            let scoot = shot.map_or(false, |s| {
                now - s >= Duration::seconds(cfg.sam_scoot_secs as i64)
                    && now - s < Duration::minutes(15)
                    && moved.map_or(true, |m| m < s)
            });
            let next = *grt
                .sam_next
                .entry(gid)
                .or_insert_with(|| now + Duration::seconds(rng.gen_range(avg * 0.5..avg * 1.5) as i64));
            let firing = shot.map_or(false, |s| now - s < Duration::minutes(10));
            let redeploy = now >= next && !firing;
            if !(scoot || redeploy) {
                continue;
            }
            grt.sam_next.insert(gid, now + Duration::seconds(rng.gen_range(avg * 0.5..avg * 1.5) as i64));
            let Ok(from) = self.group_center(&gid) else { continue };
            // Somewhere new inside the base, not next door to where it was.
            let reach = cfg.sam_move_m.max(600.);
            let inner = (radius * 0.85).max(800.);
            let mut to = None;
            for _ in 0..10 {
                let a = rng.gen_range(0.0..std::f64::consts::TAU);
                let r = rng.gen_range(reach * 0.35..reach);
                let p = from + Vector2::new(a.cos(), a.sin()) * r;
                if dist(p, center) > inner {
                    continue;
                }
                let wet = matches!(
                    land.get_surface_type(LuaVec2(p)),
                    Ok(SurfaceType::Water | SurfaceType::ShallowWater)
                );
                if !wet {
                    to = Some(p);
                    break;
                }
            }
            let Some(to) = to else { continue };
            grt.sam_moved.insert(gid, now);
            let name = self.persisted.groups.get(&gid).map(|g| g.name.clone()).unwrap_or_default();
            info!(
                "garrison: SAM {name} relocating {:.0} m ({})",
                dist(from, to),
                if scoot { "after firing" } else { "redeploying" }
            );
            self.route_garrison(lua, land, &[(gid, to)], PATROL_MPS * 2., false);
        }
    }

    /// Where each of a base's spawned groups started out.
    fn home_of(&self, gid: &GroupId) -> Option<Vector2> {
        let g = self.persisted.groups.get(gid)?;
        let pts: SmallVec<[Vector2; 16]> = g
            .units
            .into_iter()
            .filter_map(|u| self.persisted.units.get(u))
            .filter(|u| !u.dead)
            .map(|u| u.spawn_pos)
            .collect();
        (!pts.is_empty()).then(|| dcso3::centroid2d(pts.into_iter()))
    }

    /// Bring a formation to defend `oid`: the nearest idle AI one in reach,
    /// or a new one raised at the nearest friendly base that can spare it.
    fn call_qrf(
        &mut self,
        rt: &mut FormationRt,
        lua: MizLua,
        cfg: &GarrisonCfg,
        side: Side,
        oid: ObjectiveId,
        now: DateTime<Utc>,
    ) -> Option<FormationId> {
        let gw = self.ground_war_cfg().ok()?;
        let at = self.persisted.objectives.get(&oid)?.zone.pos();
        let covered = self
            .formations()
            .any(|f| f.side == side && (f.order == Order::Defend(oid) || dist(f.pos, at) <= COVER_M));
        if covered {
            return None;
        }
        let idle = self
            .formations()
            .filter(|f| {
                f.side == side
                    && f.ai_controlled(now)
                    && !f.broken
                    && f.posture == Posture::Holding
                    && !matches!(f.order, Order::Withdraw(_))
                    && dist(f.pos, at) <= cfg.qrf_range_m
            })
            .min_by(|a, b| dist(a.pos, at).total_cmp(&dist(b.pos, at)))
            .map(|f| f.id);
        let id = match idle {
            Some(id) => id,
            None => {
                let mut bases: Vec<(ObjectiveId, f64)> = self
                    .objectives()
                    .filter(|(id, o)| **id != oid && o.owner == side && !o.threatened)
                    .map(|(id, o)| (*id, dist(o.zone.pos(), at)))
                    .filter(|(_, d)| *d <= cfg.qrf_range_m)
                    .collect();
                bases.sort_by(|a, b| a.1.total_cmp(&b.1));
                let mut raised = None;
                for (b, _) in bases.into_iter().take(4) {
                    let spare = self.formation_candidates(&gw, &b, side).map_or(false, |g| !g.is_empty());
                    if !spare {
                        continue;
                    }
                    match self.raise_formation(rt, lua, side, b, None, now) {
                        Ok(id) => {
                            raised = Some(id);
                            break;
                        }
                        Err(e) => log::debug!("garrison: raising a QRF at {b}: {e:?}"),
                    }
                }
                raised?
            }
        };
        match self.order_formation(rt, lua, id, Order::Defend(oid), None, now) {
            Ok(_) => Some(id),
            Err(e) => {
                log::debug!("garrison: QRF {id} to {oid}: {e:?}");
                None
            }
        }
    }

    /// Once a slow tick: every live base's garrison reacts, calls for help,
    /// goes home or patrols.
    pub fn tick_garrisons(
        &mut self,
        rt: &mut FormationRt,
        grt: &mut GarrisonRt,
        lua: MizLua,
        now: DateTime<Utc>,
    ) {
        let cfg = self.ephemeral.cfg.garrison.clone();
        if !cfg.enabled {
            return;
        }
        if grt.last_tick.map_or(false, |t| now - t < Duration::seconds(TICK_SECS)) {
            return;
        }
        grt.last_tick = Some(now);
        let land = match Land::singleton(lua) {
            Ok(l) => l,
            Err(e) => {
                warn!("garrison: no land singleton: {e:?}");
                return;
            }
        };
        let intruders = self.ground_intruders();
        let live: SmallVec<[(ObjectiveId, Side, Vector2, f64, dcso3::String); 32]> = self
            .objectives()
            .filter(|(_, o)| o.spawned && o.owner != Side::Neutral)
            .map(|(id, o)| (*id, o.owner, o.zone.pos(), o.zone.radius(), o.name.clone()))
            .collect();
        grt.bases.retain(|id, _| live.iter().any(|(l, ..)| l == id));
        grt.sam_shot.retain(|_, t| now - *t < Duration::hours(1));
        grt.sam_moved.retain(|_, t| now - *t < Duration::hours(1));
        let qrfs_out = |grt: &GarrisonRt, side: Side, db: &Db| {
            grt.bases
                .values()
                .filter_map(|b| b.qrf)
                .filter(|id| db.formation(*id).map_or(false, |f| f.side == side))
                .count()
        };
        let mut rng = rand::thread_rng();
        for (oid, side, center, radius, name) in live.iter().cloned() {
            if cfg.sam_relocate {
                self.relocate_sams(grt, lua, &land, &cfg, oid, center, radius, now);
            }
            let groups = self.sortie_groups(&oid);
            if groups.is_empty() {
                continue;
            }
            let threat = intruders
                .iter()
                .filter(|(s, p)| *s != side && dist(*p, center) <= radius + cfg.react_m)
                .map(|(_, p)| *p)
                .min_by(|a, b| dist(*a, center).total_cmp(&dist(*b, center)));
            let qrf_count = qrfs_out(grt, side, self);
            let st = grt.bases.entry(oid).or_default();
            if let Some(t) = threat {
                st.first_seen.get_or_insert(now);
                st.last_seen = Some(now);
                st.next_patrol = None;
                let away = dist(t, center);
                let dir = if away > 1. { (t - center) / away } else { Vector2::new(1., 0.) };
                let reach = (away - STANDOFF_M).clamp(0., radius + cfg.leash_m);
                let aim = center + dir * reach;
                let due = st.aim.map_or(true, |a| dist(a, aim) > REAIM_M)
                    || st.routed_at.map_or(true, |r| now - r >= Duration::seconds(REROUTE_SECS));
                if due {
                    st.aim = Some(aim);
                    st.routed_at = Some(now);
                    st.out = true;
                    let across = Vector2::new(-dir.y, dir.x);
                    let n = groups.len() as f64;
                    let legs: SmallVec<[(GroupId, Vector2); 8]> = groups
                        .iter()
                        .enumerate()
                        .map(|(i, g)| (*g, aim + across * ((i as f64 - (n - 1.) / 2.) * SPREAD_M)))
                        .collect();
                    let first = !st.announced;
                    st.announced = true;
                    self.route_garrison(lua, &land, &legs, REACT_MPS, true);
                    if first {
                        info!("garrison: {name} ({side:?}) moving out to meet enemy ground units {away:.0} m out");
                        rt.event(
                            side,
                            "contact",
                            format_compact!("{name}: enemy armour at the perimeter, the garrison is moving to engage"),
                            Some(t),
                            None,
                            now,
                        );
                        self.ephemeral.msgs().panel_to_side(
                            15,
                            false,
                            side,
                            format_compact!("{name}: enemy armour at the perimeter, the garrison is moving to engage."),
                        );
                    }
                }
                let st = grt.bases.get_mut(&oid).unwrap();
                let lingered = st.first_seen.map_or(false, |f| now - f >= Duration::seconds(cfg.qrf_delay_secs as i64));
                let may_call = st.qrf_at.map_or(true, |t| now - t >= Duration::seconds(QRF_RETRY_SECS));
                if cfg.qrf && lingered && may_call && st.qrf.is_none() && qrf_count < cfg.qrf_max as usize {
                    st.qrf_at = Some(now);
                    if let Some(id) = self.call_qrf(rt, lua, &cfg, side, oid, now) {
                        grt.bases.get_mut(&oid).unwrap().qrf = Some(id);
                        let fname = self.formation(id).map(|f| f.name.clone()).unwrap_or_default();
                        info!("garrison: {fname} sent as a quick reaction force to {name}");
                        rt.event(
                            side,
                            "order",
                            format_compact!("{fname} is the quick reaction force for {name}"),
                            Some(center),
                            Some(id),
                            now,
                        );
                        self.ephemeral.msgs().panel_to_side(
                            15,
                            false,
                            side,
                            format_compact!("GROUND COMMAND: {fname} is the quick reaction force for {name}."),
                        );
                    }
                }
                continue;
            }
            // Quiet.
            let clear = st.last_seen.map_or(true, |t| now - t >= Duration::seconds(cfg.clear_secs as i64));
            if !clear {
                continue;
            }
            if st.first_seen.is_some() {
                // The fight is over: home, and let the QRF go back to the AI.
                *st = BaseState { out: st.out, ..Default::default() };
                if st.out {
                    st.out = false;
                    let legs: SmallVec<[(GroupId, Vector2); 8]> =
                        groups.iter().filter_map(|g| self.home_of(g).map(|h| (*g, h))).collect();
                    self.route_garrison(lua, &land, &legs, PATROL_MPS * 1.5, false);
                    info!("garrison: {name} ({side:?}) all clear, back to its positions");
                    rt.event(side, "arrived", format_compact!("{name}: all clear, the garrison is back in its positions"), Some(center), None, now);
                }
                continue;
            }
            if !cfg.patrol {
                continue;
            }
            let avg = cfg.patrol_secs.max(60) as f64;
            let st = grt.bases.get_mut(&oid).unwrap();
            let due = match st.next_patrol {
                None => {
                    st.next_patrol = Some(now + Duration::seconds(rng.gen_range(avg * 0.5..avg * 1.5) as i64));
                    false
                }
                Some(t) => now >= t,
            };
            if !due {
                continue;
            }
            st.next_patrol = Some(now + Duration::seconds(rng.gen_range(avg * 0.5..avg * 1.5) as i64));
            st.out = true;
            // About half the armour moves; the rest holds. Infantry stays put.
            let movers: SmallVec<[GroupId; 8]> = groups
                .iter()
                .copied()
                .filter(|g| self.persisted.groups.get(g).map_or(false, |g| g.class == ObjGroupClass::Armor))
                .filter(|_| rng.gen_bool(0.5))
                .collect();
            let mut legs: SmallVec<[(GroupId, Vector2); 8]> = SmallVec::new();
            for g in movers {
                for _ in 0..6 {
                    let a = rng.gen_range(0.0..std::f64::consts::TAU);
                    let r = radius * PATROL_RADIUS * rng.gen_range(0.0f64..1.).sqrt();
                    let p = center + Vector2::new(a.cos(), a.sin()) * r;
                    let wet = matches!(
                        land.get_surface_type(LuaVec2(p)),
                        Ok(SurfaceType::Water | SurfaceType::ShallowWater)
                    );
                    if !wet {
                        legs.push((g, p));
                        break;
                    }
                }
            }
            if !legs.is_empty() {
                self.route_garrison(lua, &land, &legs, PATROL_MPS, false);
            }
        }
        // A QRF that has gone (dissolved, wiped out) or finished: forget it.
        for st in grt.bases.values_mut() {
            if st.qrf.map_or(false, |id| self.formation(id).is_none()) {
                st.qrf = None;
            }
        }
    }
}
