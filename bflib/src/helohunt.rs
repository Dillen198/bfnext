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

//! AI fighters go after player helicopters flying high enough to be fair game.
//!
//! Since 13 November AI fighters and the reactive CAP engage planes only:
//! DCS AI sees a helicopter even sitting landed in a valley, and a Ka-50 took
//! an AMRAAM before it had lifted. That left helos with no air threat at all
//! (Discord, 21 November: "ignoring them completely is also not good ... I
//! still need to keep an eye out for above"). So the engine picks the
//! targets itself: an enemy player helicopter in the air more than
//! `campaign_events.cap_hunts_helos_above_ft` above the ground, within a
//! fighter flight's engage radius, gets that flight sent after it. Drop below
//! the line and the flight is called off, so going low is the defence.

use crate::{
    Context,
    db::{events::EventId, group::DeployKind},
};
use bfprotocols::{
    cfg::{ActionKind, UnitTag},
    db::group::GroupId,
};
use chrono::prelude::*;
use dcso3::{
    LuaVec2, MizLua, String, Vector2,
    coalition::Side,
    controller::{AttackParams, Task},
    group::Group,
    land::Land,
    net::Ucid,
    unit::Unit,
};
use fxhash::{FxHashMap, FxHashSet};
use log::{info, warn};

/// A flight that has chased one helicopter this long is called off and may
/// pick again, so it isn't led across the front by one target.
const HUNT_MAX_SECS: i64 = 300;
/// After a hunt ends the flight waits this long before taking another, so a
/// helo bobbing round the height line doesn't flip its task every pass.
const REST_SECS: i64 = 60;
/// A hunt is called off once the helo is this much further than the engage
/// radius.
const LEASH_FACTOR: f64 = 1.5;
const FT_PER_M: f64 = 3.28084;

#[derive(Debug, Clone)]
pub(crate) struct Hunt {
    ucid: Ucid,
    unit_name: String,
    started: DateTime<Utc>,
    /// The reactive CAP event the flight belongs to: it is put back on
    /// station by clearing its station so `retarget_cap_groups` re-tasks it,
    /// instead of popping a task that event may already have replaced.
    event: Option<EventId>,
}

#[derive(Debug, Default)]
pub(crate) struct HeloHunts {
    hunts: FxHashMap<GroupId, Hunt>,
    rest_until: FxHashMap<GroupId, DateTime<Utc>>,
}

/// Should a helicopter this high above the ground be hunted? `min_ft` 0
/// turns hunting off.
pub(crate) fn huntable(agl_m: f64, min_ft: u32) -> bool {
    min_ft > 0 && agl_m * FT_PER_M >= min_ft as f64
}

struct Helo {
    ucid: Ucid,
    side: Side,
    unit_name: String,
    pos: Vector2,
    agl_m: f64,
}

struct Fighter {
    gid: GroupId,
    side: Side,
    name: String,
    pos: Vector2,
    event: Option<EventId>,
}

fn helos(ctx: &Context, land: &Land) -> Vec<Helo> {
    let classes = &ctx.db.ephemeral.cfg.unit_classification;
    ctx.db
        .instanced_players()
        .filter(|(_, _, inst)| {
            inst.in_air
                && classes.get(&inst.typ).is_some_and(|t| t.contains(UnitTag::Helicopter))
        })
        .filter_map(|(ucid, p, inst)| {
            let p3 = inst.position.p;
            let ground = land.get_height(LuaVec2(Vector2::new(p3.x, p3.z))).ok()?;
            Some(Helo {
                ucid: *ucid,
                side: p.side,
                unit_name: inst.unit_name.clone(),
                pos: Vector2::new(p3.x, p3.z),
                agl_m: p3.y - ground,
            })
        })
        .collect()
}

/// Every AI fighter flight: fighter-cover actions (bought by players or by the
/// HQ) and the reactive jet CAP. Flights heading home are left out.
fn fighters(ctx: &Context) -> Vec<Fighter> {
    let db = &ctx.db;
    let ev = &ctx.event_scheduler;
    let pos = |gid: &GroupId| {
        let g = db.persisted.groups.get(gid)?;
        g.units
            .into_iter()
            .filter_map(|uid| db.persisted.units.get(uid))
            .find(|u| !u.dead)
            .map(|u| u.pos)
    };
    let mut out = vec![];
    for g in db.actions() {
        if let DeployKind::Action { spec, .. } = &g.origin
            && matches!(spec.kind, ActionKind::Fighters(_))
            && !ev.cap_rtb.contains_key(&g.id)
            && let Some(p) = pos(&g.id)
        {
            out.push(Fighter {
                gid: g.id,
                side: g.side,
                name: g.name.clone(),
                pos: p,
                event: None,
            });
        }
    }
    let rotary: FxHashSet<EventId> = ev
        .active_events
        .iter()
        .filter_map(|e| match e {
            crate::db::events::CampaignEvent::EnemyCap { id, rotary: true, .. } => {
                Some(*id)
            }
            _ => None,
        })
        .collect();
    for (eid, gids) in &ev.cap_groups {
        if rotary.contains(eid) {
            continue;
        }
        for gid in gids {
            if ev.cap_rtb.contains_key(gid) {
                continue;
            }
            if let (Some(g), Some(p)) = (db.persisted.groups.get(gid), pos(gid)) {
                out.push(Fighter {
                    gid: *gid,
                    side: g.side,
                    name: g.name.clone(),
                    pos: p,
                    event: Some(*eid),
                });
            }
        }
    }
    out
}

fn end_hunt(
    lua: MizLua,
    ctx: &mut Context,
    gid: GroupId,
    hunt: &Hunt,
    target_alive: bool,
    now: DateTime<Utc>,
) {
    ctx.helo_hunts.rest_until.insert(gid, now + chrono::Duration::seconds(REST_SECS));
    match hunt.event {
        // Re-stationed by `retarget_cap_groups` on its next pass.
        Some(eid) => {
            ctx.event_scheduler.cap_station_by_event.remove(&eid);
        }
        // DCS drops an attack task by itself when its target dies, and then
        // the mission underneath is current again: only pop one that is
        // still running, or the flight's own mission goes with it.
        None if target_alive => {
            let Some(name) = ctx.db.persisted.groups.get(&gid).map(|g| g.name.clone())
            else {
                return;
            };
            // No group in DCS any more: nothing to call off.
            if let Ok(c) =
                Group::get_by_name(lua, name.as_str()).and_then(|g| g.get_controller())
                && let Err(e) = c.pop_task()
            {
                warn!("helo hunt: calling off {name}: {e:?}")
            }
        }
        None => (),
    }
}

fn attack(lua: MizLua, fighter: &Fighter, unit_name: &str) -> anyhow::Result<()> {
    let group = Group::get_by_name(lua, fighter.name.as_str())?;
    // Still taxiing or taking off: a task now would cut that short.
    if !group.get_unit(1)?.in_air()? {
        anyhow::bail!("not airborne yet")
    }
    let target = Unit::get_by_name(lua, unit_name)?;
    let task = Task::AttackUnit {
        unit: target.id()?,
        params: AttackParams {
            weapon_type: None,
            expend: None,
            direction: None,
            altitude: None,
            attack_qty_limit: None,
            altitude_enabled: None,
            direction_enabled: None,
            point: None,
            attack_qty: None,
            group_attack: Some(true),
            x: None,
            y: None,
        },
    };
    group.get_controller()?.push_task(task)?;
    Ok(())
}

/// One pass: call off the hunts that are over, then send idle fighters after
/// helicopters flying high enough.
pub(crate) fn tick(
    lua: MizLua,
    ctx: &mut Context,
    now: DateTime<Utc>,
) -> anyhow::Result<()> {
    let ecfg = ctx.db.ephemeral.cfg.campaign_events.clone().unwrap_or_default();
    let min_ft = ecfg.cap_hunts_helos_above_ft;
    let radius = ecfg.cap_engage_radius_m;
    let land = Land::singleton(lua)?;
    let helos = helos(ctx, &land);
    let fighters = fighters(ctx);
    ctx.helo_hunts.rest_until.retain(|_, t| *t > now);

    let running: Vec<(GroupId, Hunt)> =
        ctx.helo_hunts.hunts.iter().map(|(g, h)| (*g, h.clone())).collect();
    for (gid, hunt) in running {
        let fighter = fighters.iter().find(|f| f.gid == gid);
        let helo =
            helos.iter().find(|h| h.ucid == hunt.ucid && h.unit_name == hunt.unit_name);
        let alive = ctx
            .db
            .instanced_players()
            .any(|(u, _, i)| *u == hunt.ucid && i.unit_name == hunt.unit_name);
        let why = match (fighter, helo) {
            (None, _) => Some("the flight is gone or going home"),
            (_, None) if alive => Some("it landed"),
            (_, None) => Some("it is gone"),
            (Some(f), Some(h)) => {
                if !huntable(h.agl_m, min_ft) {
                    Some("it went low")
                } else if (f.pos - h.pos).norm() > radius * LEASH_FACTOR {
                    Some("it got away")
                } else if (now - hunt.started).num_seconds() >= HUNT_MAX_SECS {
                    Some("time up")
                } else {
                    None
                }
            }
        };
        if let Some(why) = why {
            info!("helo hunt: {gid} off {} ({why})", hunt.unit_name);
            ctx.helo_hunts.hunts.remove(&gid);
            if fighter.is_some() {
                end_hunt(lua, ctx, gid, &hunt, alive, now);
            }
        }
    }

    if min_ft == 0 {
        return Ok(());
    }
    let mut taken: FxHashSet<Ucid> =
        ctx.helo_hunts.hunts.values().map(|h| h.ucid).collect();
    for h in helos.iter().filter(|h| huntable(h.agl_m, min_ft)) {
        if taken.contains(&h.ucid) {
            continue;
        }
        let best = fighters
            .iter()
            .filter(|f| f.side != h.side && f.side != Side::Neutral)
            .filter(|f| !ctx.helo_hunts.hunts.contains_key(&f.gid))
            .filter(|f| !ctx.helo_hunts.rest_until.contains_key(&f.gid))
            .map(|f| (f, (f.pos - h.pos).norm()))
            .filter(|(_, d)| *d <= radius)
            .min_by(|a, b| a.1.total_cmp(&b.1));
        let Some((f, d)) = best else { continue };
        match attack(lua, f, &h.unit_name) {
            Ok(()) => {
                info!(
                    "helo hunt: {} after {} ({:.0} ft AGL, {:.0} km)",
                    f.name,
                    h.unit_name,
                    h.agl_m * FT_PER_M,
                    d / 1000.
                );
                taken.insert(h.ucid);
                ctx.helo_hunts.hunts.insert(
                    f.gid,
                    Hunt {
                        ucid: h.ucid,
                        unit_name: h.unit_name.clone(),
                        started: now,
                        event: f.event,
                    },
                );
            }
            // Not airborne yet, or DCS doesn't know the unit: try next pass.
            Err(e) => {
                log::debug!("helo hunt: {} can't go after {}: {e:?}", f.name, h.unit_name)
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_helos_above_the_line_are_hunted() {
        // 100 ft is 30.48 m
        assert!(!huntable(0., 100));
        assert!(!huntable(25., 100));
        assert!(huntable(31., 100));
        assert!(huntable(500., 100));
        // 0 turns it off
        assert!(!huntable(500., 0));
    }
}
