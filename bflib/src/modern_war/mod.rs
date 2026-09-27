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

//! Modern-war systems (`Cfg::modern_war`), modelled on Russia-Ukraine and the
//! 2025 Iran war. Every part reads the campaign as it stands -- who owns
//! what, what is alive, how much supply a base holds, who is in the air --
//! and changes it back:
//!
//! - `ew`: jammer trucks at key objectives switch on as enemy aircraft come
//!   in, break the enemy's GCI voice and reveal themselves to ELINT.
//! - `sam_stock`: SAM sites fire from a finite magazine and only logistics
//!   refill it.
//! - `raids`: drone and missile raids on infrastructure cut supply and halt
//!   factories.
//! - `boats`: sea-drone attacks on enemy ships.
//! - `tempo`: each side alternates offensives and regroups, which set how
//!   often the rest of it fires and where.
//! - `rail`: supply trains between stations on the railway; a train
//!   destroyed on the line takes its cargo with it.
//!
//! All of it is session state; the groups it spawns are `EventSpawn` or live
//! outside the campaign db, so a restart starts it clean.

pub(crate) mod boats;
pub(crate) mod ew;
pub(crate) mod raids;
pub(crate) mod rail;
pub(crate) mod sam_stock;
pub(crate) mod tempo;

use crate::Context;
use bfprotocols::{
    cfg::ModernWarCfg,
    db::{group::GroupId, objective::ObjectiveId},
    perf::PerfInner,
};
use chrono::prelude::*;
use dcso3::{coalition::Side, land::Land, timer::Timer, LuaVec2, MizLua, Vector2};

#[derive(Debug, Default)]
pub(crate) struct ModernWar {
    pub(crate) ew: ew::Ew,
    pub(crate) sam: sam_stock::SamStock,
    pub(crate) raids: raids::Raids,
    pub(crate) boats: boats::Boats,
    pub(crate) tempo: tempo::Tempo,
    pub(crate) rail: rail::Rail,
}

fn cfg(ctx: &Context) -> Option<ModernWarCfg> {
    ctx.db.ephemeral.cfg.modern_war.clone()
}

/// Human pilots in aircraft, either side.
pub(crate) fn humans(ctx: &Context) -> usize {
    ctx.db.instanced_players().count()
}

/// Hour of the day on the mission clock, 0..24.
pub(crate) fn mission_hour(lua: MizLua) -> Option<u32> {
    let t = Timer::singleton(lua).ok()?.get_abs_time().ok()?;
    Some(((t.0 / 3600.).floor() as i64).rem_euclid(24) as u32)
}

/// True if `hour` is inside [start, end), wrapping past midnight.
pub(crate) fn in_hours(hour: u32, (start, end): (u32, u32)) -> bool {
    if start <= end {
        hour >= start && hour < end
    } else {
        hour >= start || hour < end
    }
}

/// Positions of `side`'s enemies in the air: whatever `side`'s radar
/// network paints, plus every airborne enemy player (radar gaps are not a
/// hiding place from a jammer's own receivers).
pub(crate) fn enemy_air(ctx: &Context, side: Side, now: DateTime<Utc>) -> Vec<Vector2> {
    let mut out = ctx.ewr.detected_enemy_positions(side, now);
    out.extend(
        ctx.db
            .instanced_players()
            .filter(|(_, p, i)| p.side == side.opposite() && i.in_air)
            .map(|(_, _, i)| Vector2::new(i.position.p.x, i.position.p.z)),
    );
    out
}

/// A point on dry land (land or road) within `max_r` of `center`, searched in
/// rings. `None` if there is none.
pub(crate) fn land_near(land: &Land, center: Vector2, max_r: f64) -> Option<Vector2> {
    use dcso3::land::SurfaceType;
    let ok = |p: Vector2| {
        matches!(land.get_surface_type(LuaVec2(p)), Ok(SurfaceType::Land | SurfaceType::Road))
    };
    if ok(center) {
        return Some(center);
    }
    let mut r = 150.;
    while r <= max_r {
        for i in 0..12 {
            let a = i as f64 * std::f64::consts::TAU / 12.;
            let p = center + Vector2::new(a.cos(), a.sin()) * r;
            if ok(p) {
                return Some(p);
            }
        }
        r += 150.;
    }
    None
}

/// A point on open water within `max_r` of `center`, searched in rings.
pub(crate) fn water_near(land: &Land, center: Vector2, max_r: f64) -> Option<Vector2> {
    use dcso3::land::SurfaceType;
    let mut r = 500.;
    while r <= max_r {
        for i in 0..16 {
            let a = i as f64 * std::f64::consts::TAU / 16.;
            let p = center + Vector2::new(a.cos(), a.sin()) * r;
            if matches!(land.get_surface_type(LuaVec2(p)), Ok(SurfaceType::Water)) {
                return Some(p);
            }
        }
        r += 500.;
    }
    None
}

/// The objective a group belongs to, or the nearest one its side owns.
pub(crate) fn home_objective(ctx: &Context, gid: &GroupId) -> Option<ObjectiveId> {
    use crate::db::group::DeployKind;
    let group = ctx.db.persisted.groups.get(gid)?;
    if let DeployKind::Objective { origin } = &group.origin {
        return Some(*origin);
    }
    let pos = ctx.db.group_center(gid).ok()?;
    ctx.db
        .objectives()
        .filter(|(_, o)| o.owner() == group.side)
        .min_by(|(_, a), (_, b)| {
            crate::airlife::dist(a.pos(), pos).total_cmp(&crate::airlife::dist(b.pos(), pos))
        })
        .map(|(id, _)| *id)
}

/// A SAM launch by a unit of `gid` (from the Shot event).
pub(crate) fn on_sam_shot(ctx: &mut Context, gid: GroupId, now: DateTime<Utc>) {
    if let Some(c) = cfg(ctx).and_then(|c| c.sam_stock).filter(|c| c.enabled) {
        sam_stock::on_shot(ctx, &c, gid, now);
    }
}

/// Is a flight of `side` at `pos` inside an active enemy radio jammer?
pub(crate) fn comms_jammed(ctx: &Context, side: Side, pos: Vector2) -> bool {
    match cfg(ctx).and_then(|c| c.ew).filter(|c| c.enabled) {
        Some(c) => ctx.modern_war.ew.comms_jammed(&c, side, pos),
        None => false,
    }
}

/// Group names the engine spawns outside the campaign db (sea drones,
/// supply trains). The birth handler must leave them alone, the same as
/// civil traffic.
pub(crate) fn is_unmanaged(name: &str) -> bool {
    name.starts_with(boats::USV_PREFIX) || name.starts_with(rail::RAIL_PREFIX)
}

/// The slow-tick entry point.
pub(crate) fn tick(lua: MizLua, ctx: &mut Context, perf: &mut PerfInner, now: DateTime<Utc>) {
    let Some(cfg) = cfg(ctx) else { return };
    if let Some(c) = cfg.tempo.as_ref().filter(|c| c.enabled) {
        tempo::tick(ctx, c, now);
    }
    if let Some(c) = cfg.ew.as_ref().filter(|c| c.enabled) {
        ew::tick(lua, ctx, perf, c, now);
    }
    if let Some(c) = cfg.sam_stock.as_ref().filter(|c| c.enabled) {
        sam_stock::tick(lua, ctx, c, now);
    }
    if let Some(c) = cfg.raids.as_ref().filter(|c| c.enabled) {
        raids::tick(lua, ctx, perf, c, cfg.tempo.as_ref(), now);
    }
    if let Some(c) = cfg.boat_raids.as_ref().filter(|c| c.enabled) {
        boats::tick(lua, ctx, c, cfg.tempo.as_ref(), now);
    }
    if let Some(c) = cfg.rail.as_ref().filter(|c| c.enabled) {
        rail::tick(lua, ctx, c, now);
    }
}
