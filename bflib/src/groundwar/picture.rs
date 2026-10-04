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

//! The ground war as one side sees it, for the dashboard's command page
//! (`query-ground-war`). Built for exactly one side:
//!
//! - its own formations in full, every vehicle where it really is;
//! - enemy formations only where it has spotted them, at the position and
//!   time it last saw them -- and their vehicles only while they are in
//!   sight and close (`GroundCombatCfg::spot_m`);
//! - the battles, which both sides are in;
//! - its own human players, read straight from DCS so the page can show them
//!   moving (the engine's own copy of a player's position is only refreshed
//!   every several seconds).

use super::{at_sea, cfg};
use crate::{
    db::{
        formation::{Formation, Order, Posture},
        objective::ObjGroupClass,
    },
    Context,
};
use bfprotocols::{
    cfg::UnitTag,
    db::objective::ObjectiveKind,
    groundwar::{
        BattleInfo, EnemyContact, FormationInfo, GroundEvent, GroundObjective, GroundPicture,
        GroundUnit, LatLon, LivePlayer,
    },
};
use chrono::prelude::*;
use dcso3::{coalition::Side, coord::Coord, group::GroupCategory, unit::Unit, LuaVec3, MizLua, Vector2, Vector3};
use std::collections::BTreeMap;

/// Path points sent to the dashboard: about one a kilometre...
const PATH_SPACING_M: f64 = 1_000.;
/// ...and no more than this many.
const PATH_MAX: usize = 150;
/// A battle's ring on the dashboard.
const BATTLE_RADIUS_M: f64 = 2_500.;
/// An attack on a base this close to a battle makes it an assault.
const ASSAULT_M: f64 = 8_000.;
/// Events sent per picture.
const EVENTS_MAX: usize = 40;

fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

fn kind_name(k: &ObjectiveKind) -> &'static str {
    match k {
        ObjectiveKind::Airbase => "airbase",
        ObjectiveKind::Fob => "fob",
        ObjectiveKind::Logistics => "logistics",
        ObjectiveKind::Farp { .. } => "farp",
        ObjectiveKind::Factory { .. } => "factory",
        ObjectiveKind::NavalBase => "naval",
        ObjectiveKind::CarrierGroup { .. } => "carrier",
        ObjectiveKind::SpecialSamSite { .. } => "sam",
        ObjectiveKind::CommandCenter => "command",
    }
}

fn side_name(s: Side) -> String {
    format!("{s:?}")
}

fn deg(rad: f64) -> f64 {
    (rad.to_degrees() + 360.) % 360.
}

/// Map position to lat/lon. One DCS call per formation; its vehicles are a
/// few hundred metres away and go through `Local` instead.
struct Geo<'lua> {
    coord: Option<Coord<'lua>>,
}

impl<'lua> Geo<'lua> {
    fn ll(&self, p: Vector2) -> LatLon {
        self.coord
            .as_ref()
            .and_then(|c| c.lo_to_ll(LuaVec3(Vector3::new(p.x, 0., p.y))).ok())
            .map(|l| [l.latitude, l.longitude])
            .unwrap_or([0., 0.])
    }

    fn local(&self, origin: Vector2) -> Local {
        Local { origin, ll: self.ll(origin) }
    }
}

/// Flat-earth lat/lon around a point: plenty for vehicles within a few km.
/// DCS map x is north, y (the z axis) east.
struct Local {
    origin: Vector2,
    ll: LatLon,
}

impl Local {
    fn ll(&self, p: Vector2) -> LatLon {
        let d = p - self.origin;
        let lat = self.ll[0] + d.x / 111_320.;
        let lon = self.ll[1] + d.y / (111_320. * self.ll[0].to_radians().cos().max(0.01));
        [lat, lon]
    }
}

fn units_of(ctx: &Context, f: &Formation, local: &Local) -> (Vec<GroundUnit>, BTreeMap<String, u32>) {
    let db = &ctx.db;
    let mut units = vec![];
    let mut make_up: BTreeMap<String, u32> = BTreeMap::new();
    for gid in &f.groups {
        let Some(g) = db.persisted.groups.get(gid) else { continue };
        for uid in &g.units {
            let Some(u) = db.persisted.units.get(uid) else { continue };
            if u.dead {
                continue;
            }
            let role = db.unit_role(&u.typ, g.class);
            *make_up.entry(role.name().into()).or_default() += 1;
            units.push(GroundUnit {
                pos: local.ll(u.pos),
                heading: deg(u.heading),
                role: role.name().into(),
                typ: u.typ.to_string(),
            });
        }
    }
    (units, make_up)
}

fn players(ctx: &Context, lua: MizLua, geo: &Geo, side: Side) -> Vec<LivePlayer> {
    let db = &ctx.db;
    db.instanced_players()
        .filter(|(_, p, _)| p.side == side)
        .map(|(ucid, p, i)| {
            // Straight from DCS; the cached copy is the fallback.
            let live = Unit::get_by_name(lua, i.unit_name.as_str()).ok().and_then(|u| {
                let pos = u.get_position().ok()?;
                let vel = u.get_velocity().ok().map(|v| v.0).unwrap_or(i.velocity);
                let air = u.in_air().unwrap_or(i.in_air);
                Some((pos, vel, air))
            });
            let (pos, vel, in_air) = live.unwrap_or((i.position.clone(), i.velocity, i.in_air));
            let tags = db.ephemeral.cfg.unit_classification.get(&i.typ).map(|t| t.0);
            let category = match tags {
                Some(t) if t.contains(UnitTag::Helicopter) => "helicopter",
                Some(t) if t.contains(UnitTag::Aircraft) => "plane",
                Some(t) if t.contains(UnitTag::Boat) => "ship",
                _ => "ground",
            };
            let fwd = pos.x.0;
            LivePlayer {
                name: p.name.to_string(),
                typ: i.typ.to_string(),
                category: category.into(),
                pos: geo.ll(Vector2::new(pos.p.0.x, pos.p.0.z)),
                alt_m: pos.p.0.y,
                heading: deg(fwd.z.atan2(fwd.x)),
                speed_kts: vel.norm() * 1.943_84,
                in_air,
                is_self: false,
                ucid: Some(ucid.to_string()),
            }
        })
        .collect()
}

pub(crate) fn picture(ctx: &Context, lua: MizLua, side: Side) -> GroundPicture {
    let now = Utc::now();
    let geo = Geo { coord: Coord::singleton(lua).ok() };
    let Some(cfg) = cfg(ctx) else {
        return GroundPicture {
            side: side_name(side),
            enabled: false,
            max_formations: 0,
            live: 0,
            max_live: 0,
            player_lock_secs: 0,
            formations: vec![],
            enemy: vec![],
            battles: vec![],
            objectives: vec![],
            players: vec![],
            events: vec![],
            time: now.timestamp(),
            spot_m: 0.,
            engage_m: 0.,
        };
    };
    let db = &ctx.db;
    let rt = &ctx.groundwar.rt;
    let obj_name = |o| db.persisted.objectives.get(&o).map(|o| o.name.to_string());
    let formations: Vec<FormationInfo> = db
        .formations()
        .filter(|f| f.side == side)
        .map(|f| {
            let (alive, total) = db.formation_strength(f);
            let (order, target) = match f.order {
                Order::Hold => ("hold", None),
                Order::Attack(o) => ("attack", Some(o)),
                Order::Defend(o) => ("defend", Some(o)),
                Order::Withdraw(o) => ("withdraw", Some(o)),
            };
            let mut path = vec![geo.ll(f.pos)];
            let mut last = f.pos;
            let mut spacing = PATH_SPACING_M;
            if f.path.len() > PATH_MAX {
                spacing *= f.path.len() as f64 / PATH_MAX as f64;
            }
            for (i, p) in f.path.iter().enumerate() {
                if dist(*p, last) >= spacing || i + 1 == f.path.len() {
                    path.push(geo.ll(*p));
                    last = *p;
                }
            }
            let km = f.path_len_m() / 1000.;
            let moving = f.posture == Posture::Moving;
            let halted = rt.is_halted(f.id);
            let speed = db.formation_speed_kph(&cfg, f);
            let ai = f.ai_controlled(now);
            let local = geo.local(f.pos);
            let (units, make_up) = units_of(ctx, f, &local);
            let (power, power_full) = db.formation_power(&cfg.combat, f);
            let trail: Vec<LatLon> = f.trail.iter().rev().take(25).rev().map(|p| geo.ll(*p)).collect();
            FormationInfo {
                id: f.id,
                name: f.name.to_string(),
                pos: geo.ll(f.pos),
                heading: deg(f.heading),
                order: order.into(),
                target: target.map(|o| o.inner() as u64),
                target_name: target.and_then(obj_name),
                posture: match f.posture {
                    Posture::Moving => "moving",
                    Posture::Holding => "holding",
                    Posture::Assaulting => "assaulting",
                }
                .into(),
                alive,
                total,
                // "Can take a base": infantry, or IFVs / APCs carrying a squad.
                has_infantry: db.formation_can_assault(f),
                live: rt.is_live(f),
                halted,
                engaged: db.formation_engaged(f),
                home: f.home.inner() as u64,
                home_name: obj_name(f.home).unwrap_or_default(),
                commander: match (&f.commander, ai) {
                    (Some(u), false) => db.player(u).map(|p| p.name.to_string()),
                    _ => None,
                },
                locked_mins: if ai {
                    None
                } else {
                    f.locked_until.map(|t| ((t - now).num_seconds().max(0) / 60) as u32)
                },
                path: if moving { path } else { vec![] },
                km_to_go: if moving { km } else { 0. },
                eta_mins: (moving && !halted && speed > 0.5).then(|| (km / speed * 60.).round() as u32),
                kind: db.formation_kind(f).into(),
                units,
                make_up,
                power: (power * 10.).round() / 10.,
                power_full: (power_full * 10.).round() / 10.,
                supply_pct: (f.supply * 100.).round().clamp(0., 100.) as u8,
                in_supply: rt.in_supply(f.id),
                morale_pct: (f.morale * 100.).round().clamp(0., 100.) as u8,
                broken: f.broken,
                deployment: f.deployment.name().into(),
                speed_kph: if moving && !halted { (speed * 10.).round() / 10. } else { 0. },
                trail,
                losses: f.losses,
                kills: f.kills,
            }
        })
        .collect();
    // Enemy formations only as this side has seen them: where and when.
    let enemy: Vec<EnemyContact> = rt
        .sightings(side)
        .filter_map(|(id, s)| {
            let f = db.formation(id)?;
            // As it was when seen, not as it is: the picture must not carry
            // what the side hasn't seen.
            let alive = s.alive;
            let in_sight = rt.in_sight(s);
            let units = if in_sight && s.close {
                let local = geo.local(f.pos);
                units_of(ctx, f, &local).0
            } else {
                vec![]
            };
            Some(EnemyContact {
                pos: geo.ll(s.pos),
                kind: s.kind.into(),
                approx_vehicles: ((alive + 2) / 5 * 5).max(alive.min(5)),
                heading: deg(s.heading),
                id,
                last_seen_secs: if in_sight { 0 } else { (now - s.at).num_seconds().max(0) as u32 },
                moving: s.moving,
                units,
            })
        })
        .collect();
    let battles: Vec<BattleInfo> = rt
        .battles()
        .iter()
        .map(|b| {
            // An attack on a base nearby makes it an assault.
            let assault = b.formations.iter().find_map(|id| {
                let f = db.formation(*id)?;
                let Order::Attack(oid) = f.order else { return None };
                let o = db.persisted.objectives.get(&oid)?;
                (dist(o.pos(), b.pos) <= ASSAULT_M).then_some(oid)
            });
            BattleInfo {
                id: b.id,
                pos: geo.ll(b.pos),
                radius_m: BATTLE_RADIUS_M,
                near: b.near.as_ref().map(|n| n.to_string()),
                since: b.started.timestamp(),
                live: b.live,
                ours: b
                    .formations
                    .iter()
                    .copied()
                    .filter(|id| db.formation(*id).map_or(false, |f| f.side == side))
                    .collect(),
                intensity: (b.heat * 100.).round() / 100.,
                our_losses: b.losses(side),
                enemy_losses: b.losses(side.opposite()),
                kind: if assault.is_some() { "assault" } else { "meeting" }.into(),
                objective: assault.map(|o| o.inner() as u64),
            }
        })
        .collect();
    let objectives: Vec<GroundObjective> = db
        .objectives()
        .filter(|(_, o)| !at_sea(o.kind()))
        .map(|(id, o)| {
            let own = o.owner() == side;
            let garrison = own.then(|| {
                o.groups()
                    .get(&side)
                    .map(|gids| {
                        gids.into_iter()
                            .filter_map(|g| db.persisted.groups.get(g))
                            .filter(|g| {
                                g.kind == Some(GroupCategory::Ground)
                                    && !matches!(g.class, ObjGroupClass::Logi | ObjGroupClass::Services)
                            })
                            .flat_map(|g| g.units.into_iter())
                            .filter(|u| db.persisted.units.get(u).map_or(false, |u| !u.dead))
                            .count() as u32
                    })
                    .unwrap_or(0)
            });
            GroundObjective {
                id: id.inner() as u64,
                name: o.name.to_string(),
                pos: geo.ll(o.pos()),
                owner: side_name(o.owner()),
                kind: kind_name(o.kind()).into(),
                health: own.then(|| o.health()),
                threatened: own.then(|| o.threatened()),
                can_raise: own.then(|| {
                    db.formation_candidates(&cfg, id, side).map(|g| g.len() as u32).unwrap_or(0)
                }),
                being_captured: db.capture_in_progress(id),
                supply: own.then(|| o.supply()),
                garrison,
            }
        })
        .collect();
    let events: Vec<GroundEvent> = rt
        .events(side)
        .take(EVENTS_MAX)
        .map(|e| GroundEvent {
            at: e.at.timestamp(),
            kind: e.kind.into(),
            text: e.text.to_string(),
            pos: e.pos.map(|p| geo.ll(p)),
            formation: e.formation,
        })
        .collect();
    GroundPicture {
        side: side_name(side),
        enabled: true,
        max_formations: cfg.max_formations_per_side,
        live: rt.live_count(db) as u32,
        max_live: cfg.max_live_formations,
        player_lock_secs: cfg.player_order_lock_secs,
        formations,
        enemy,
        battles,
        objectives,
        players: players(ctx, lua, &geo, side),
        events,
        time: now.timestamp(),
        spot_m: cfg.combat.spot_m * rt.visibility(),
        engage_m: cfg.combat.engage_m,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_lat_lon_runs_north_and_east() {
        let l = Local { origin: Vector2::new(0., 0.), ll: [42., 43.] };
        let n = l.ll(Vector2::new(1_113.2, 0.));
        assert!((n[0] - 42.01).abs() < 1e-6 && (n[1] - 43.).abs() < 1e-9);
        let e = l.ll(Vector2::new(0., 1_000.));
        assert!(e[0] == 42. && e[1] > 43.01 && e[1] < 43.02);
    }
}
