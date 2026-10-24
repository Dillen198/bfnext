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

//! Sea-drone raids, after Ukraine's Black Sea campaign: a pack of fast boats
//! leaves friendly water near one of the side's naval bases, runs at an
//! enemy ship and detonates alongside. The boats are real enemy surface
//! contacts to DCS, so escorts, helicopters and players can stop them; the
//! damage from one that gets there is DCS's own explosion.
//!
//! DCS has no unmanned surface vessel, so `boat_type` (a fast attack boat by
//! default) stands in. The boats are spawned straight into DCS, outside the
//! campaign db; their names start with `USV_PREFIX`, which the birth handler
//! leaves alone.

use super::{humans, water_near};
use crate::{airlife::dist, Context};
use bfprotocols::cfg::{BoatRaidsCfg, TempoCfg, UnitTag};
use chrono::{prelude::*, Duration};
use compact_str::format_compact;
use dcso3::{
    coalition::{Coalition, Side},
    controller::{MissionPoint, PointType, Task},
    country::Country,
    env::miz,
    group::{Group, GroupCategory},
    land::Land,
    trigger::Trigger,
    unit::Unit,
    LuaEnv, LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3,
};
use fxhash::FxHashMap;
use log::{info, warn};
use mlua::{FromLua, Value};
use rand::{thread_rng, Rng};

pub(crate) const USV_PREFIX: &str = "USV ";

#[derive(Debug)]
struct Boat {
    group: String,
    raid: u32,
    expires: DateTime<Utc>,
}

#[derive(Debug)]
struct BoatRaid {
    id: u32,
    side: Side,
    /// The enemy ship unit being run at.
    target_unit: String,
    target_label: String,
    launched: u32,
    pending: u32,
    hits: u32,
    warned: bool,
}

#[derive(Debug, Default)]
pub(crate) struct Boats {
    next: FxHashMap<Side, DateTime<Utc>>,
    boats: Vec<Boat>,
    raids: Vec<BoatRaid>,
    seq: u32,
}

fn point<'lua>(p: Vector2, speed: f64) -> MissionPoint<'lua> {
    MissionPoint {
        typ: PointType::TurningPoint,
        airdrome_id: None,
        time_re_fu_ar: None,
        helipad: None,
        link_unit: None,
        action: None,
        pos: LuaVec2(p),
        alt: 0.,
        alt_typ: None,
        speed,
        speed_locked: None,
        eta: None,
        eta_locked: None,
        name: None,
        task: Box::new(Task::ComboTask(vec![])),
    }
}

fn unit_pos(lua: MizLua, name: &str) -> Option<Vector2> {
    let p = Unit::get_by_name(lua, name).ok()?.get_point().ok()?;
    Some(Vector2::new(p.x, p.z))
}

fn spawn_boat(
    lua: MizLua,
    side: Side,
    name: &str,
    typ: &str,
    at: Vector2,
    to: Vector2,
    speed: f64,
) -> anyhow::Result<()> {
    let l = lua.inner();
    let pts = l.create_table()?;
    pts.raw_set(1, point(at, speed))?;
    pts.raw_set(2, point(to, speed))?;
    let route = l.create_table()?;
    route.raw_set("points", pts)?;
    let unit = l.create_table()?;
    unit.raw_set("name", format_compact!("{name}-1").as_str())?;
    unit.raw_set("type", typ)?;
    unit.raw_set("x", at.x)?;
    unit.raw_set("y", at.y)?;
    unit.raw_set("heading", (to - at).y.atan2((to - at).x))?;
    unit.raw_set("skill", "Excellent")?;
    let units = l.create_table()?;
    units.raw_set(1, unit)?;
    let group = l.create_table()?;
    group.raw_set("name", name)?;
    group.raw_set("route", route)?;
    group.raw_set("units", units)?;
    let group = miz::Group::from_lua(Value::Table(group), l)?;
    let country = match side {
        Side::Blue => Country::CJTF_BLUE,
        _ => Country::CJTF_RED,
    };
    Coalition::singleton(lua)?.add_group(country, GroupCategory::Ship, group)?;
    Ok(())
}

/// An enemy ship `side` can reach from one of its naval bases: (target unit
/// name, label, launch point).
fn pick_target(
    lua: MizLua,
    ctx: &Context,
    cfg: &BoatRaidsCfg,
    side: Side,
) -> Option<(String, String, Vector2)> {
    let land = Land::singleton(lua).ok()?;
    let bases: Vec<Vector2> = ctx
        .db
        .objectives()
        .filter(|(_, o)| {
            o.owner() == side
                && matches!(o.kind(), bfprotocols::db::objective::ObjectiveKind::NavalBase)
        })
        .map(|(_, o)| o.pos())
        .collect();
    if bases.is_empty() {
        return None;
    }
    let mut best: Option<(String, String, Vector2, f64)> = None;
    for (_, u) in ctx.db.persisted.units.into_iter() {
        if u.side != side.opposite() || u.dead || !u.tags.contains(UnitTag::Boat) {
            continue;
        }
        let Some(tpos) = unit_pos(lua, u.name.as_str()) else { continue };
        let Some(base) = bases.iter().copied().min_by(|a, b| dist(*a, tpos).total_cmp(&dist(*b, tpos)))
        else {
            continue;
        };
        let Some(start) = water_near(&land, base, 15_000.) else { continue };
        let d = dist(start, tpos);
        if d > cfg.max_range_m {
            continue;
        }
        if best.as_ref().map(|b| d < b.3).unwrap_or(true) {
            best = Some((u.name.clone(), String::from(u.typ.0.as_str()), start, d));
        }
    }
    best.map(|(n, l, s, _)| (n, l, s))
}

pub(crate) fn tick(
    lua: MizLua,
    ctx: &mut Context,
    cfg: &BoatRaidsCfg,
    tempo: Option<&TempoCfg>,
    now: DateTime<Utc>,
) {
    // Boats under way.
    let mut i = 0;
    while i < ctx.modern_war.boats.boats.len() {
        let b = &ctx.modern_war.boats.boats[i];
        let (gname, rid, expires) = (b.group.clone(), b.raid, b.expires);
        let Some(ri) = ctx.modern_war.boats.raids.iter().position(|r| r.id == rid) else {
            if let Ok(g) = Group::get_by_name(lua, gname.as_str()) {
                let _ = g.destroy();
            }
            ctx.modern_war.boats.boats.swap_remove(i);
            continue;
        };
        let target_unit = ctx.modern_war.boats.raids[ri].target_unit.clone();
        let group = Group::get_by_name(lua, gname.as_str()).ok();
        let here = group
            .as_ref()
            .and_then(|g| g.get_unit(1).ok())
            .and_then(|u| u.get_point().ok())
            .map(|p| Vector2::new(p.x, p.z));
        let target = unit_pos(lua, target_unit.as_str());
        let done: Option<bool> = match (group, here, target) {
            // Sunk on the way.
            (None, _, _) | (_, None, _) => Some(false),
            // The target is gone; nothing left to run at.
            (Some(g), Some(_), None) => {
                let _ = g.destroy();
                Some(false)
            }
            (Some(g), Some(here), Some(target)) => {
                let d = dist(here, target);
                if d <= cfg.trigger_radius_m {
                    if let Ok(act) = Trigger::singleton(lua).and_then(|t| t.action()) {
                        let _ = act.explosion(
                            LuaVec3(Vector3::new(here.x, 1., here.y)),
                            cfg.warhead_kg as f32,
                        );
                    }
                    let _ = g.destroy();
                    Some(true)
                } else if now >= expires {
                    let _ = g.destroy();
                    Some(false)
                } else {
                    // The target moves: steer at where it is now.
                    if let Ok(c) = g.get_controller() {
                        let _ = c.set_task(Task::Mission {
                            airborne: None,
                            route: vec![point(here, cfg.speed_ms), point(target, cfg.speed_ms)],
                        });
                    }
                    let r = &mut ctx.modern_war.boats.raids[ri];
                    if d <= 15_000. && !r.warned {
                        r.warned = true;
                        let (side, label) = (r.side, r.target_label.clone());
                        ctx.db.ephemeral.msgs().panel_to_side(
                            15,
                            true,
                            side.opposite(),
                            format_compact!("Fast boats closing on our {label} -- engage them!"),
                        );
                    }
                    None
                }
            }
        };
        match done {
            None => i += 1,
            Some(hit) => {
                ctx.modern_war.boats.boats.swap_remove(i);
                let r = &mut ctx.modern_war.boats.raids[ri];
                r.pending = r.pending.saturating_sub(1);
                if hit {
                    r.hits += 1;
                }
            }
        }
    }
    // Finished raids.
    let mut ri = 0;
    while ri < ctx.modern_war.boats.raids.len() {
        if ctx.modern_war.boats.raids[ri].pending > 0 {
            ri += 1;
            continue;
        }
        let r = ctx.modern_war.boats.raids.swap_remove(ri);
        info!("boats: raid {} on {} over, {}/{} hit", r.id, r.target_label, r.hits, r.launched);
        ctx.db.ephemeral.msgs().panel_to_side(
            15,
            false,
            r.side,
            format_compact!("Sea-drone attack on the enemy {}: {}/{} boats reached it.", r.target_label, r.hits, r.launched),
        );
        ctx.db.ephemeral.msgs().panel_to_side(
            15,
            false,
            r.side.opposite(),
            format_compact!("Sea-drone attack on our {} is over: {} of {} boats hit.", r.target_label, r.hits, r.launched),
        );
    }
    // New raids.
    if humans(ctx) == 0 && !cfg.run_when_empty {
        return;
    }
    for side in [Side::Blue, Side::Red] {
        let factor = ctx.modern_war.tempo.factor(tempo, side, now);
        let jitter: f64 = thread_rng().gen_range(0.7..1.3);
        let wait = Duration::seconds((cfg.interval_secs as f64 * jitter * factor.max(0.1)) as i64);
        let Some(next) = ctx.modern_war.boats.next.get(&side).copied() else {
            ctx.modern_war.boats.next.insert(side, now + wait / 2);
            continue;
        };
        if now < next || ctx.modern_war.boats.raids.iter().any(|r| r.side == side) {
            continue;
        }
        ctx.modern_war.boats.next.insert(side, now + wait);
        let Some((target_unit, label, start)) = pick_target(lua, ctx, cfg, side) else { continue };
        let Some(tpos) = unit_pos(lua, target_unit.as_str()) else { continue };
        ctx.modern_war.boats.seq += 1;
        let id = ctx.modern_war.boats.seq;
        let dir = (tpos - start).normalize();
        let lateral = Vector2::new(-dir.y, dir.x);
        let mut launched = 0;
        for n in 0..cfg.boats_per_raid {
            let at = start + lateral * ((n as f64 - (cfg.boats_per_raid as f64 - 1.) / 2.) * 200.);
            let name = format_compact!("{USV_PREFIX}{id}-{n}");
            match spawn_boat(lua, side, name.as_str(), cfg.boat_type.as_str(), at, tpos, cfg.speed_ms) {
                Ok(()) => {
                    launched += 1;
                    let secs = cfg.max_range_m / cfg.speed_ms.max(1.) * 1.5;
                    ctx.modern_war.boats.boats.push(Boat {
                        group: String::from(name.as_str()),
                        raid: id,
                        expires: now + Duration::seconds(secs as i64),
                    });
                }
                Err(e) => warn!("boats: sea drone would not spawn: {e:?}"),
            }
        }
        if launched > 0 {
            info!("boats: {side:?} raid {id}: {launched} boats at {label} ({target_unit})");
            ctx.db.ephemeral.msgs().panel_to_side(
                15,
                false,
                side,
                format_compact!("Sea drones launched: {launched} boats running at an enemy {label}."),
            );
            ctx.modern_war.boats.raids.push(BoatRaid {
                id,
                side,
                target_unit,
                target_label: label,
                launched,
                pending: launched,
                hits: 0,
                warned: false,
            });
        }
    }
}
