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

//! Naval patrols (`Cfg::naval_patrols`): warship groups sailing fixed loops
//! for whoever holds their naval base.
//!
//! Each patrol is session state: a restart drops the ships (`EventSpawn`) and
//! the next tick puts them back to sea. A patrol that is sunk is replaced
//! `respawn_minutes` later; one whose base changes hands is withdrawn and
//! the new owner's ships sail the same loop.

use crate::{Context, airlife::alive, db::logistics::ship_route};
use bfprotocols::{
    cfg::NavalPatrol,
    db::{group::GroupId, objective::ObjectiveId},
};
use chrono::{Duration, prelude::*};
use compact_str::format_compact;
use dcso3::{MizLua, Vector2, coalition::Side, controller::Task, group::Group};
use fxhash::FxHashMap;
use log::{info, warn};

/// How often patrols are looked after.
const TICK_SECS: i64 = 30;
/// A patrol this close to the end of its loop starts the next lap.
const LAP_DONE_M: f64 = 2_000.;
/// It doesn't count as finishing a lap this soon after starting one: it
/// starts at the point it also finishes at.
const LAP_MIN_SECS: i64 = 600;

#[derive(Debug)]
struct Patrol {
    gid: GroupId,
    side: Side,
    /// The loop it was given, in open water; it starts and ends at path[0].
    path: Vec<Vector2>,
    lap_started: DateTime<Utc>,
}

#[derive(Debug, Default)]
pub(crate) struct NavalPatrols {
    /// By the patrol's name.
    at_sea: FxHashMap<dcso3::String, Patrol>,
    /// When each patrol may next put to sea: after a sinking, a failed
    /// spawn, or a change of owner.
    next_sail: FxHashMap<dcso3::String, DateTime<Utc>>,
    last_tick: Option<DateTime<Utc>>,
}

/// The owner of the patrol's naval base, and the base, if it exists.
fn base_owner(ctx: &Context, p: &NavalPatrol) -> Option<(ObjectiveId, Side)> {
    ctx.db
        .objectives()
        .find(|(_, o)| o.name.as_str() == p.base.as_str())
        .map(|(id, o)| (*id, o.owner()))
}

/// The loop as sailed: the route, back to its start.
fn lap(route: &[(f64, f64)]) -> Vec<Vector2> {
    let mut v: Vec<Vector2> = route.iter().map(|(x, y)| Vector2::new(*x, *y)).collect();
    if let Some(first) = v.first().copied() {
        v.push(first);
    }
    v
}

pub(crate) fn tick(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let Some(cfg) = ctx.db.ephemeral.cfg.naval_patrols.clone().filter(|c| c.enabled)
    else {
        // switched off: anything at sea goes home
        let gone: Vec<GroupId> = ctx.naval.at_sea.drain().map(|(_, p)| p.gid).collect();
        for gid in gone {
            let _ = ctx.db.delete_group(&gid);
        }
        return;
    };
    if ctx.naval.last_tick.is_some_and(|t| (now - t).num_seconds() < TICK_SECS) {
        return;
    }
    ctx.naval.last_tick = Some(now);
    let respawn = Duration::minutes(cfg.respawn_minutes as i64);
    for p in &cfg.patrols {
        let owner = base_owner(ctx, p);
        // what is at sea: still afloat, and still the owner's?
        if let Some(cur) = ctx.naval.at_sea.get(&p.name) {
            let gid = cur.gid;
            if !alive(ctx, &gid) {
                let side = cur.side;
                ctx.naval.at_sea.remove(&p.name);
                let _ = ctx.db.delete_group(&gid);
                ctx.naval.next_sail.insert(p.name.clone(), now + respawn);
                info!("[NAVAL_PATROL] {} ({side:?}) sunk", p.name);
                ctx.db.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side,
                    format_compact!(
                        "Our naval patrol {} has been sunk. A replacement sails in {} min.",
                        p.name,
                        cfg.respawn_minutes
                    ),
                );
                continue;
            }
            if owner.map(|(_, s)| s) != Some(cur.side) {
                // the base changed hands: these ships are withdrawn
                info!(
                    "[NAVAL_PATROL] {}: base lost by {:?}, withdrawn",
                    p.name, cur.side
                );
                ctx.naval.at_sea.remove(&p.name);
                let _ = ctx.db.delete_group(&gid);
            } else {
                next_lap(lua, ctx, p, now);
                continue;
            }
        }
        // nothing at sea: sail one if the base has an owner with ships for it
        let Some((base, side)) = owner else { continue };
        if side == Side::Neutral {
            continue;
        }
        let Some(template) = p.templates.get(&side) else { continue };
        if ctx.naval.next_sail.get(&p.name).is_some_and(|t| now < *t) {
            continue;
        }
        let what = format!("naval patrol {}", p.name);
        match ctx.db.spawn_ships_on_path(
            lua,
            &ctx.idx,
            side,
            template.as_str(),
            base,
            &lap(&p.route),
            p.speed_kph,
            &what,
        ) {
            Ok((gid, path)) => {
                info!("[NAVAL_PATROL] {} ({side:?}) at sea with {template}", p.name);
                ctx.naval.next_sail.remove(&p.name);
                ctx.naval
                    .at_sea
                    .insert(p.name.clone(), Patrol { gid, side, path, lap_started: now });
            }
            Err(e) => {
                warn!("[NAVAL_PATROL] {} could not sail {template}: {e:?}", p.name);
                ctx.naval.next_sail.insert(p.name.clone(), now + respawn);
            }
        }
    }
}

/// Back at the start of the loop: go round again.
fn next_lap(lua: MizLua, ctx: &mut Context, p: &NavalPatrol, now: DateTime<Utc>) {
    let Some(cur) = ctx.naval.at_sea.get(&p.name) else { return };
    if (now - cur.lap_started).num_seconds() < LAP_MIN_SECS {
        return;
    }
    let Some(name) = ctx.db.persisted.groups.get(&cur.gid).map(|g| g.name.clone()) else {
        return;
    };
    let Some(end) = cur.path.last().copied() else { return };
    let lapped = (|| -> anyhow::Result<bool> {
        let g = Group::get_by_name(lua, name.as_str())?;
        let at = g.get_unit(1)?.get_point()?;
        if (Vector2::new(at.x, at.z) - end).norm() > LAP_DONE_M {
            return Ok(false);
        }
        g.get_controller()?.set_task(Task::Mission {
            route: ship_route(&cur.path, p.speed_kph),
            airborne: None,
        })?;
        Ok(true)
    })();
    match lapped {
        Ok(true) => {
            if let Some(cur) = ctx.naval.at_sea.get_mut(&p.name) {
                cur.lap_started = now;
            }
        }
        Ok(false) => (),
        Err(e) => warn!("[NAVAL_PATROL] {}: could not start the next lap: {e:?}", p.name),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_lap_closes_on_its_start() {
        let l = lap(&[(0., 0.), (1_000., 0.), (1_000., 1_000.)]);
        assert_eq!(l.len(), 4);
        assert_eq!(l[0], l[3]);
    }
}
