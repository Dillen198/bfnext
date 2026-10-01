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
//! (`query-ground-war`). Built for exactly one side: its own formations in
//! full, enemy formations only where its forces are in contact with them,
//! and the battles -- which both sides are in.

use super::{at_sea, cfg};
use crate::{
    db::{
        formation::{Formation, Order, Posture},
        objective::ObjGroupClass,
    },
    Context,
};
use bfprotocols::{
    db::objective::ObjectiveKind,
    groundwar::{BattleInfo, EnemyContact, FormationInfo, GroundObjective, GroundPicture, LatLon},
};
use chrono::prelude::*;
use dcso3::{coalition::Side, coord::Coord, LuaVec3, MizLua, Vector2, Vector3};

/// Path points sent to the dashboard: about one a kilometre...
const PATH_SPACING_M: f64 = 1_000.;
/// ...and no more than this many.
const PATH_MAX: usize = 150;
/// A battle's ring on the dashboard.
const BATTLE_RADIUS_M: f64 = 2_500.;

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

/// "armour" | "mechanised" | "infantry", from what is left of it.
fn make_up(ctx: &Context, f: &Formation) -> &'static str {
    let (mut armor, mut inf) = (false, false);
    for gid in &f.groups {
        if let Some(g) = ctx.db.persisted.groups.get(gid) {
            let alive = g
                .units
                .into_iter()
                .any(|u| ctx.db.persisted.units.get(u).map_or(false, |u| !u.dead));
            if alive {
                armor |= g.class == ObjGroupClass::Armor;
                inf |= g.class.is_infantry();
            }
        }
    }
    match (armor, inf) {
        (true, true) => "mechanised",
        (true, false) => "armour",
        _ => "infantry",
    }
}

pub(crate) fn picture(ctx: &Context, lua: MizLua, side: Side) -> GroundPicture {
    let now = Utc::now();
    let coord = Coord::singleton(lua).ok();
    let ll = |p: Vector2| -> LatLon {
        coord
            .as_ref()
            .and_then(|c| c.lo_to_ll(LuaVec3(Vector3::new(p.x, 0., p.y))).ok())
            .map(|l| [l.latitude, l.longitude])
            .unwrap_or([0., 0.])
    };
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
        };
    };
    let db = &ctx.db;
    let rt = &ctx.groundwar.rt;
    let obj_name = |o| db.persisted.objectives.get(&o).map(|o| o.name.to_string());
    let speed_kmh = cfg.speed_kph.max(1.);
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
            let mut path = vec![ll(f.pos)];
            let mut last = f.pos;
            let mut spacing = PATH_SPACING_M;
            if f.path.len() > PATH_MAX {
                spacing *= f.path.len() as f64 / PATH_MAX as f64;
            }
            for (i, p) in f.path.iter().enumerate() {
                if dist(*p, last) >= spacing || i + 1 == f.path.len() {
                    path.push(ll(*p));
                    last = *p;
                }
            }
            let km = f.path_len_m() / 1000.;
            let moving = f.posture == Posture::Moving;
            let halted = rt.is_halted(f.id);
            let road = if f.off_road { speed_kmh * 0.5 } else { speed_kmh };
            let ai = f.ai_controlled(now);
            FormationInfo {
                id: f.id,
                name: f.name.to_string(),
                pos: ll(f.pos),
                heading: (f.heading.to_degrees() + 360.) % 360.,
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
                has_infantry: db.formation_has_infantry(f),
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
                eta_mins: (moving && !halted).then(|| (km / road * 60.).round() as u32),
            }
        })
        .collect();
    // Enemy formations show only where we are in contact with them: near one
    // of our formations or one of our bases.
    let ours: Vec<Vector2> = db
        .formations()
        .filter(|f| f.side == side)
        .map(|f| f.pos)
        .chain(db.objectives().filter(|(_, o)| o.owner() == side).map(|(_, o)| o.pos()))
        .collect();
    let enemy: Vec<EnemyContact> = db
        .formations()
        .filter(|f| f.side != side)
        .filter(|f| ours.iter().any(|p| dist(*p, f.pos) <= cfg.contact_m))
        .map(|f| {
            let alive = db.formation_strength(f).0;
            EnemyContact {
                pos: ll(f.pos),
                kind: make_up(ctx, f).into(),
                approx_vehicles: ((alive + 2) / 5 * 5).max(alive.min(5)),
                heading: (f.heading.to_degrees() + 360.) % 360.,
            }
        })
        .collect();
    let battles: Vec<BattleInfo> = rt
        .battles()
        .iter()
        .map(|b| BattleInfo {
            id: b.id,
            pos: ll(b.pos),
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
        })
        .collect();
    let objectives: Vec<GroundObjective> = db
        .objectives()
        .filter(|(_, o)| !at_sea(o.kind()))
        .map(|(id, o)| {
            let own = o.owner() == side;
            GroundObjective {
                id: id.inner() as u64,
                name: o.name.to_string(),
                pos: ll(o.pos()),
                owner: side_name(o.owner()),
                kind: kind_name(o.kind()).into(),
                health: own.then(|| o.health()),
                threatened: own.then(|| o.threatened()),
                can_raise: own.then(|| {
                    db.formation_candidates(&cfg, id, side).map(|g| g.len() as u32).unwrap_or(0)
                }),
                being_captured: db.capture_in_progress(id),
            }
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
    }
}
