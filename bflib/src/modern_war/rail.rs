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

//! Rail logistics. Both current wars run on railways -- Russia's army moves
//! by train, and Ukraine's supply lines and the strikes on them are about
//! rail as much as roads. Here:
//!
//! - An objective is a **station** when a railway passes within
//!   `station_radius_m` of it.
//! - A side's logistics hub or factory with stock sends a **supply train** to
//!   its most needy station nearer the front, along the actual rails, never
//!   on a line that passes close to the enemy.
//! - The load is taken out of the origin when the train leaves and credited
//!   on arrival (the same in-flight ledger the convoys use). **Destroy the
//!   train and the cargo is lost** -- the enemy is told, and rail becomes a
//!   target.
//!
//! A DCS train is one `Train` unit carrying a `wagons` list (locomotive
//! first), routed with "On Railroads" waypoints. The trains are spawned
//! straight into DCS outside the campaign db; their names start with
//! `RAIL_PREFIX`, which the birth handler leaves alone.

use crate::airlife::{dist, front_dist, front_points};
use crate::Context;
use bfprotocols::{
    cfg::RailCfg,
    db::objective::{ObjectiveId, ObjectiveKind},
};
use chrono::{prelude::*, Duration};
use compact_str::format_compact;
use dcso3::{
    coalition::{Coalition, Side},
    country::Country,
    env::miz,
    group::{Group, GroupCategory},
    land::{Land, RoadType},
    LuaEnv, LuaVec2, MizLua, String, Vector2,
};
use fxhash::FxHashMap;
use log::{info, warn};
use mlua::{FromLua, Value};

pub(crate) const RAIL_PREFIX: &str = "RAIL ";

/// A train closer than this to its destination's rail point has arrived.
const ARRIVAL_M: f64 = 1_500.;
/// Route waypoints are kept about this far apart.
const WAYPOINT_SPACING_M: f64 = 2_000.;
/// A train that has moved less than this in `STALL_SECS` is stuck.
const STALL_M: f64 = 200.;
const STALL_SECS: i64 = 900;

#[derive(Debug)]
struct Train {
    group: String,
    side: Side,
    origin_name: String,
    dest_name: String,
    dest_rail: Vector2,
    expires: DateTime<Utc>,
    last_pos: Vector2,
    last_moved: DateTime<Utc>,
}

#[derive(Debug, Default)]
pub(crate) struct Rail {
    /// Each objective's nearest rail point, if it has one in range. Rails
    /// and objectives don't move, so this is worked out once per session.
    stations: FxHashMap<ObjectiveId, Option<Vector2>>,
    trains: Vec<Train>,
    next: FxHashMap<Side, DateTime<Utc>>,
    seq: u32,
}

fn station(land: &Land, rail: &mut Rail, oid: ObjectiveId, pos: Vector2, radius: f64) -> Option<Vector2> {
    *rail.stations.entry(oid).or_insert_with(|| {
        land.get_closest_point_on_roads(RoadType::Rail, LuaVec2(pos))
            .ok()
            .map(|p| p.0)
            .filter(|p| dist(*p, pos) <= radius)
    })
}

/// The rail path from `a` to `b`, thinned to roughly `WAYPOINT_SPACING_M`,
/// and its length.
fn rail_path(land: &Land, a: Vector2, b: Vector2) -> Option<(Vec<Vector2>, f64)> {
    let raw: Vec<Vector2> = land
        .find_path_on_roads(RoadType::Rail, LuaVec2(a), LuaVec2(b))
        .ok()?
        .into_iter()
        .filter_map(|p| p.ok())
        .map(|p| p.0)
        .collect();
    if raw.len() < 2 {
        return None;
    }
    let length: f64 = raw.windows(2).map(|w| dist(w[0], w[1])).sum();
    let mut out = vec![raw[0]];
    for (i, p) in raw.iter().enumerate().skip(1) {
        if dist(*out.last().expect("nonempty"), *p) >= WAYPOINT_SPACING_M || i + 1 == raw.len() {
            out.push(*p);
        }
    }
    Some((out, length))
}

fn spawn_train(
    lua: MizLua,
    side: Side,
    name: &str,
    consist: &[String],
    path: &[Vector2],
    speed: f64,
) -> anyhow::Result<()> {
    let l = lua.inner();
    let points = l.create_table()?;
    for (i, p) in path.iter().enumerate() {
        let wp = l.create_table()?;
        wp.raw_set("x", p.x)?;
        wp.raw_set("y", p.y)?;
        wp.raw_set("type", "Turning Point")?;
        wp.raw_set("action", "On Railroads")?;
        wp.raw_set("speed", speed)?;
        wp.raw_set("alt", 0.)?;
        wp.raw_set("alt_type", "BARO")?;
        points.raw_set(i + 1, wp)?;
    }
    let route = l.create_table()?;
    route.raw_set("points", points)?;
    let wagons = l.create_table()?;
    for (i, w) in consist.iter().enumerate() {
        wagons.raw_set(i + 1, w.as_str())?;
    }
    let unit = l.create_table()?;
    unit.raw_set("name", format_compact!("{name}-1").as_str())?;
    unit.raw_set("type", "Train")?;
    unit.raw_set("x", path[0].x)?;
    unit.raw_set("y", path[0].y)?;
    unit.raw_set("heading", (path[1] - path[0]).y.atan2((path[1] - path[0]).x))?;
    unit.raw_set("skill", "Average")?;
    unit.raw_set("wagons", wagons)?;
    let units = l.create_table()?;
    units.raw_set(1, unit)?;
    let group = l.create_table()?;
    group.raw_set("name", name)?;
    group.raw_set("route", route)?;
    group.raw_set("units", units)?;
    let country = match side {
        Side::Blue => Country::CJTF_BLUE,
        _ => Country::CJTF_RED,
    };
    let coalition = Coalition::singleton(lua)?;
    // DCS has a train group category; fall back to ground if this build
    // wants trains added as vehicles.
    let as_train = miz::Group::from_lua(Value::Table(group.clone()), l)?;
    if let Err(e) = coalition.add_group(country, GroupCategory::Train, as_train) {
        warn!("rail: adding {name} as a train failed ({e:?}), trying as a ground group");
        let as_ground = miz::Group::from_lua(Value::Table(group), l)?;
        coalition.add_group(country, GroupCategory::Ground, as_ground)?;
    }
    Ok(())
}

fn train_pos(lua: MizLua, name: &str) -> Option<Vector2> {
    let g = Group::get_by_name(lua, name).ok()?;
    let p = g.get_unit(1).ok()?.get_point().ok()?;
    Some(Vector2::new(p.x, p.z))
}

fn destroy(lua: MizLua, name: &str) {
    if let Ok(g) = Group::get_by_name(lua, name) {
        let _ = g.destroy();
    }
}

/// Pick and send one train for `side`, if there is a run worth making.
fn dispatch(lua: MizLua, ctx: &mut Context, cfg: &RailCfg, side: Side, now: DateTime<Utc>) -> bool {
    let Ok(land) = Land::singleton(lua) else { return false };
    let front = front_points(ctx);
    let enemy: Vec<Vector2> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == side.opposite())
        .map(|(_, o)| o.pos())
        .collect();
    let ours: Vec<(ObjectiveId, String, Vector2, ObjectiveKind, u8, bool)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == side)
        .map(|(id, o)| (*id, o.name.clone(), o.pos(), o.kind().clone(), o.supply(), o.threatened()))
        .collect();
    let mut with_rail: Vec<(ObjectiveId, String, Vector2, ObjectiveKind, u8, bool, Vector2)> = vec![];
    for (id, name, pos, kind, supply, threatened) in ours {
        if let Some(r) = station(&land, &mut ctx.modern_war.rail, id, pos, cfg.station_radius_m) {
            with_rail.push((id, name, pos, kind, supply, threatened, r));
        }
    }
    // Origins: hubs and factories with stock to spare. Destinations: the
    // neediest stations, nearest the front first.
    let origins: Vec<_> = with_rail
        .iter()
        .filter(|s| {
            matches!(s.3, ObjectiveKind::Logistics | ObjectiveKind::Factory { .. })
                && s.4 >= 50
                && !s.5
        })
        .collect();
    let mut dests: Vec<_> = with_rail
        .iter()
        .filter(|s| s.4 < cfg.destination_supply_below)
        .collect();
    dests.sort_by(|a, b| {
        (a.4 as f64 + front_dist(&front, a.2) / 10_000.)
            .total_cmp(&(b.4 as f64 + front_dist(&front, b.2) / 10_000.))
    });
    let busy: Vec<String> = ctx.modern_war.rail.trains.iter().map(|t| t.dest_name.clone()).collect();
    for d in dests.iter().take(6) {
        if busy.contains(&d.1) {
            continue;
        }
        // The nearest origin that isn't the destination itself.
        let mut candidates: Vec<_> = origins.iter().filter(|o| o.0 != d.0).collect();
        candidates.sort_by(|a, b| dist(a.6, d.6).total_cmp(&dist(b.6, d.6)));
        for o in candidates.into_iter().take(3) {
            let Some((path, length)) = rail_path(&land, o.6, d.6) else { continue };
            if length < cfg.min_route_m || length > cfg.max_route_m {
                continue;
            }
            // Never along a line that runs past the enemy.
            let exposed = path
                .iter()
                .any(|p| enemy.iter().any(|e| dist(*e, *p) < cfg.enemy_clearance_m));
            if exposed {
                continue;
            }
            ctx.modern_war.rail.seq += 1;
            let name = String::from(format_compact!(
                "{RAIL_PREFIX}{:?} {}",
                side,
                ctx.modern_war.rail.seq
            ));
            let loaded = match ctx.db.load_unmanaged_cargo(
                name.as_str(),
                o.0,
                d.0,
                side,
                cfg.per_item_cap,
                now,
            ) {
                Ok(l) => l,
                Err(e) => {
                    warn!("rail: loading {name}: {e:?}");
                    continue;
                }
            };
            if loaded.is_empty() {
                continue;
            }
            let consist = match side {
                Side::Blue => &cfg.train_blue,
                _ => &cfg.train_red,
            };
            let speed = cfg.speed_kph / 3.6;
            if let Err(e) = spawn_train(lua, side, name.as_str(), consist, &path, speed) {
                warn!("rail: {name} would not spawn: {e:?}");
                ctx.db.refund_unmanaged_cargo(name.as_str());
                return false;
            }
            let units: u32 = loaded.iter().map(|t| t.amount()).sum();
            info!(
                "rail: {name} {} -> {} ({:.0} km of track, {units} units)",
                o.1,
                d.1,
                length / 1000.
            );
            ctx.db.ephemeral.msgs().panel_to_side(
                15,
                false,
                side,
                format_compact!(
                    "Supply train departing {} for {} ({:.0} km by rail).",
                    o.1,
                    d.1,
                    length / 1000.
                ),
            );
            ctx.modern_war.rail.trains.push(Train {
                group: name,
                side,
                origin_name: o.1.clone(),
                dest_name: d.1.clone(),
                dest_rail: d.6,
                expires: now + Duration::seconds((length / speed.max(1.) * 2. + 900.) as i64),
                last_pos: path[0],
                last_moved: now,
            });
            return true;
        }
    }
    false
}

pub(crate) fn tick(lua: MizLua, ctx: &mut Context, cfg: &RailCfg, now: DateTime<Utc>) {
    // Trains on the line.
    let mut i = 0;
    while i < ctx.modern_war.rail.trains.len() {
        let t = &ctx.modern_war.rail.trains[i];
        let (name, side) = (t.group.clone(), t.side);
        let outcome: Option<&str> = match train_pos(lua, name.as_str()) {
            None => Some("destroyed"),
            Some(p) if dist(p, t.dest_rail) <= ARRIVAL_M => Some("arrived"),
            Some(_) if now >= t.expires => Some("overdue"),
            Some(p) => {
                let t = &mut ctx.modern_war.rail.trains[i];
                if dist(p, t.last_pos) >= STALL_M {
                    t.last_pos = p;
                    t.last_moved = now;
                    None
                } else if (now - t.last_moved).num_seconds() >= STALL_SECS {
                    Some("stuck")
                } else {
                    None
                }
            }
        };
        let Some(outcome) = outcome else {
            i += 1;
            continue;
        };
        let t = ctx.modern_war.rail.trains.swap_remove(i);
        match outcome {
            "arrived" => {
                ctx.db.deliver_unmanaged_cargo(name.as_str(), side);
                destroy(lua, name.as_str());
                info!("rail: {name} arrived at {}", t.dest_name);
                ctx.db.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side,
                    format_compact!("Supply train from {} has arrived at {}.", t.origin_name, t.dest_name),
                );
            }
            "destroyed" => {
                ctx.db.lose_unmanaged_cargo(name.as_str());
                info!("rail: {name} ({} -> {}) destroyed, cargo lost", t.origin_name, t.dest_name);
                ctx.db.ephemeral.msgs().panel_to_side(
                    15,
                    true,
                    side,
                    format_compact!(
                        "Our supply train to {} was destroyed -- its cargo is lost.",
                        t.dest_name
                    ),
                );
                ctx.db.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side.opposite(),
                    format_compact!("Enemy supply train bound for {} destroyed.", t.dest_name),
                );
            }
            why => {
                // Overdue or not moving: take it off the line and give the
                // load back rather than let a pathing fault eat it.
                warn!("rail: {name} {why} on the way to {}, refunding its load", t.dest_name);
                ctx.db.refund_unmanaged_cargo(name.as_str());
                destroy(lua, name.as_str());
            }
        }
    }
    // Departures.
    for side in [Side::Blue, Side::Red] {
        let Some(next) = ctx.modern_war.rail.next.get(&side).copied() else {
            ctx.modern_war
                .rail
                .next
                .insert(side, now + Duration::seconds(cfg.interval_secs as i64 / 3));
            continue;
        };
        if now < next {
            continue;
        }
        let running = ctx.modern_war.rail.trains.iter().filter(|t| t.side == side).count();
        let sent = running < cfg.max_trains_per_side as usize && dispatch(lua, ctx, cfg, side, now);
        let wait = if sent { cfg.interval_secs as i64 } else { 600 };
        ctx.modern_war.rail.next.insert(side, now + Duration::seconds(wait));
    }
}
