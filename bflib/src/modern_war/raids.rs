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

//! Long-range strike raids on infrastructure, the way both of the current
//! wars are fought between the fronts: one-way attack drones and ballistic
//! missiles at fuel, factories, logistics hubs and airbases, mostly at night.
//!
//! - **Target.** An enemy objective of `target_kinds` in reach, weighted by
//!   what it is worth and how full its stores are, and pulled toward the
//!   side's offensive axis when tempo is on.
//! - **Drones** fly a template low (radar altitude) from deep in friendly
//!   territory. They are real enemy aircraft to DCS: SAMs, AAA and players
//!   shoot them down. One that reaches the target detonates there.
//! - **Missiles** come from the side's own deployed launchers in range
//!   (`artillery.units` ranges). DCS flies them; the engine decides how many
//!   got through from the defender's missile-defence sites near the target,
//!   and every interception spends that site's interceptors (`sam_stock`).
//! - **Effect.** Each hit cuts the target's stores by `supply_damage_pct` and
//!   holds a factory's production for `factory_pause_secs`. Both sides are
//!   told how the raid went.

use super::{humans, in_hours, mission_hour};
use crate::{
    airlife::{air_point, alive, dist, front_dist, front_points, heading_of},
    jtac::{aim_and_fire_route, group_facing},
    spawnctx::SpawnCtx,
    Context,
};
use bfprotocols::{
    cfg::{RaidsCfg, TempoCfg, UnitTag},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::PerfInner,
};
use chrono::{prelude::*, Duration};
use compact_str::format_compact;
use dcso3::{
    coalition::Side,
    controller::{
        AiOption, AirOption, AirReactionToThreat, AirRoe, AltType, Command, MissionPoint, Task,
    },
    group::Group,
    land::Land,
    trigger::Trigger,
    LuaVec2, LuaVec3, MizLua, Vector2, Vector3,
};
use fxhash::FxHashMap;
use log::{info, warn};
use rand::{seq::SliceRandom, thread_rng, Rng};

/// A drone this close to its target detonates.
const DRONE_ARRIVAL_M: f64 = 1_200.;
/// Missile-defence sites this close to the target can intercept.
const DEFENCE_RADIUS_M: f64 = 40_000.;
/// Drones start on the ground at their launch site; this long for engine
/// start and the takeoff run before they are on their way.
const DRONE_LAUNCH_SECS: f64 = 120.;
/// Widest gap between drones lined up at the launch site (each sits inside
/// the site's zone whatever its size).
const DRONE_LAUNCH_SPACING_M: f64 = 60.;

#[derive(Debug)]
struct Raid {
    id: u32,
    side: Side,
    target: ObjectiveId,
    target_name: dcso3::String,
    target_pos: Vector2,
    drones_launched: u32,
    drones_pending: u32,
    drone_hits: u32,
    missiles: u32,
    missile_hits: u32,
    missiles_resolve_at: Option<DateTime<Utc>>,
    intercept_p: f64,
    /// Enemy missile-defence groups near the target, for interceptor spend.
    defenders: Vec<GroupId>,
    supply_cut: u32,
    factory_halted: bool,
}

#[derive(Debug)]
struct Drone {
    gid: GroupId,
    raid: u32,
    expires: DateTime<Utc>,
}

#[derive(Debug, Default)]
pub(crate) struct Raids {
    next: FxHashMap<Side, DateTime<Utc>>,
    raids: Vec<Raid>,
    drones: Vec<Drone>,
    seq: u32,
}

fn kind_value(kind: &ObjectiveKind) -> f64 {
    match kind {
        ObjectiveKind::Factory { .. } => 3.0,
        ObjectiveKind::Logistics => 2.5,
        ObjectiveKind::Airbase => 2.0,
        ObjectiveKind::NavalBase => 1.5,
        _ => 1.0,
    }
}

fn interval(cfg: &RaidsCfg, factor: f64) -> Duration {
    let jitter: f64 = thread_rng().gen_range(0.7..1.3);
    Duration::seconds((cfg.interval_secs as f64 * jitter * factor.max(0.1)) as i64)
}

/// The side's deployed ballistic-missile launchers that can reach `target`,
/// nearest first: (group, launcher units alive).
fn launchers_in_range(
    ctx: &Context,
    cfg: &RaidsCfg,
    side: Side,
    target: Vector2,
) -> Vec<(GroupId, u32)> {
    let Some(arty) = ctx.db.ephemeral.cfg.artillery.as_ref() else { return vec![] };
    let Some(gids) = ctx.db.persisted.groups_by_side.get(&side) else { return vec![] };
    let mut out: Vec<(GroupId, u32, f64)> = gids
        .into_iter()
        .filter_map(|gid| {
            let g = ctx.db.persisted.groups.get(gid)?;
            let launchers: Vec<_> = g
                .units
                .into_iter()
                .filter_map(|u| ctx.db.persisted.units.get(u))
                .filter(|u| {
                    !u.dead
                        && u.tags.contains(UnitTag::Launcher)
                        && cfg.missile_types.iter().any(|t| t.as_str() == u.typ.0.as_str())
                })
                .collect();
            let first = launchers.first()?;
            // Only types with a configured range: a guessed one fires into
            // the ground short of the target or refuses the task.
            let range = arty.units.get(first.typ.0.as_str())?;
            let d = dist(ctx.db.group_center(gid).ok()?, target);
            (d <= range.max_range_m && d >= range.min_range_m)
                .then_some((*gid, launchers.len() as u32, d))
        })
        .collect();
    out.sort_by(|a, b| a.2.total_cmp(&b.2));
    out.into_iter().map(|(g, n, _)| (g, n)).collect()
}

/// The target side's missile-defence groups near `target` that still have
/// interceptors.
fn defence_near(ctx: &Context, defender: Side, target: Vector2) -> Vec<GroupId> {
    let Some(gids) = ctx.db.persisted.groups_by_side.get(&defender) else { return vec![] };
    gids.into_iter()
        .filter(|gid| {
            let Some(g) = ctx.db.persisted.groups.get(gid) else { return false };
            let capable = g.units.into_iter().filter_map(|u| ctx.db.persisted.units.get(u)).any(|u| {
                !u.dead && u.tags.contains(UnitTag::SAM) && u.tags.contains(UnitTag::EngagesWeapons)
            });
            capable
                && ctx.db.group_center(gid).map(|p| dist(p, target) <= DEFENCE_RADIUS_M).unwrap_or(false)
                && ctx.modern_war.sam.remaining(gid) != Some(0)
        })
        .copied()
        .collect()
}

/// Where `side` launches drones at `target` from: its objective deepest
/// behind its own lines that is still in range, and at least 30 km out.
fn drone_launch_site(
    ctx: &Context,
    side: Side,
    target: Vector2,
    range: f64,
    front: &[Vector2],
) -> Option<(ObjectiveId, Vector2)> {
    ctx.db
        .objectives()
        .filter(|(_, o)| o.owner() == side)
        .filter(|(_, o)| {
            let d = dist(o.pos(), target);
            d <= range && d >= 30_000.
        })
        .max_by(|(_, a), (_, b)| front_dist(front, a.pos()).total_cmp(&front_dist(front, b.pos())))
        .map(|(id, o)| (*id, o.pos()))
}

fn pick_target(
    ctx: &Context,
    cfg: &RaidsCfg,
    side: Side,
    reachable: impl Fn(Vector2) -> bool,
) -> Option<(ObjectiveId, dcso3::String, Vector2)> {
    let axis = ctx.modern_war.tempo.axis(side);
    let mut scored: Vec<(ObjectiveId, dcso3::String, Vector2, f64)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == side.opposite() && o.health() > 0)
        .filter(|(_, o)| cfg.target_kinds.iter().any(|k| k.as_str() == o.kind().name()))
        .filter(|(_, o)| reachable(o.pos()))
        .map(|(id, o)| {
            let mut score = kind_value(o.kind()) * (0.5 + o.supply() as f64 / 100.);
            if Some(*id) == axis {
                score *= 2.0;
            }
            (*id, o.name.clone(), o.pos(), score)
        })
        .collect();
    scored.sort_by(|a, b| b.3.total_cmp(&a.3));
    scored.truncate(3);
    scored
        .choose_weighted(&mut thread_rng(), |t| t.3.max(0.01))
        .ok()
        .map(|t| (t.0, t.1.clone(), t.2))
}

/// One weapon reached the target: cut its stores and halt it if it makes
/// things.
fn apply_hit(lua: MizLua, ctx: &mut Context, cfg: &RaidsCfg, raid: usize) {
    let target = ctx.modern_war.raids.raids[raid].target;
    if let Err(e) = ctx.db.admin_reduce_inventory(lua, target, cfg.supply_damage_pct.min(100)) {
        warn!("raids: could not cut stores at {target}: {e:?}");
    } else {
        ctx.modern_war.raids.raids[raid].supply_cut += cfg.supply_damage_pct as u32;
    }
    if let Ok(true) =
        ctx.db.pause_factory(&target, Duration::seconds(cfg.factory_pause_secs as i64))
    {
        ctx.modern_war.raids.raids[raid].factory_halted = true;
    }
}

fn launch(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    cfg: &RaidsCfg,
    side: Side,
    now: DateTime<Utc>,
) -> bool {
    let front = front_points(ctx);
    let templates = match side {
        Side::Blue => &cfg.drone_templates_blue,
        _ => &cfg.drone_templates_red,
    };
    let can_drone = !templates.is_empty();
    let target = pick_target(ctx, cfg, side, |p| {
        (can_drone && drone_launch_site(ctx, side, p, cfg.drone_range_m, &front).is_some())
            || (cfg.use_missiles && !launchers_in_range(ctx, cfg, side, p).is_empty())
    });
    let Some((target, target_name, target_pos)) = target else {
        return false;
    };
    ctx.modern_war.raids.seq += 1;
    let id = ctx.modern_war.raids.seq;
    let mut raid = Raid {
        id,
        side,
        target,
        target_name: target_name.clone(),
        target_pos,
        drones_launched: 0,
        drones_pending: 0,
        drone_hits: 0,
        missiles: 0,
        missile_hits: 0,
        missiles_resolve_at: None,
        intercept_p: 0.,
        defenders: vec![],
        supply_cut: 0,
        factory_halted: false,
    };
    // Drones.
    let land = Land::singleton(lua).ok();
    let mut eta_min = 0.;
    if let (true, Some((origin, from)), Some(_), Ok(spctx)) = (
        can_drone,
        drone_launch_site(ctx, side, target_pos, cfg.drone_range_m, &front),
        land.as_ref(),
        SpawnCtx::new(lua),
    ) {
        let dir = (target_pos - from).normalize();
        let lateral = Vector2::new(-dir.y, dir.x);
        let leg = dist(from, target_pos);
        // Engine start and the run-up off the launch site come first.
        eta_min = (leg / cfg.drone_speed_ms.max(1.) + DRONE_LAUNCH_SECS) / 60.;
        // The drones go up from the launch site itself, side by side inside
        // its zone -- not strung out across kilometres of open country.
        let zone_r = ctx
            .db
            .persisted
            .objectives
            .get(&origin)
            .map(|o| o.radius())
            .unwrap_or(300.);
        let step = DRONE_LAUNCH_SPACING_M
            .min(zone_r * 1.2 / (cfg.drones_per_raid.max(1) as f64));
        // A patch of the site clear of its garrison, as for a helicopter.
        let from = ctx.db.launch_spot(lua, &origin, true, false).unwrap_or(from);
        let mut pool = templates.clone();
        for i in 0..cfg.drones_per_raid {
            pool.shuffle(&mut thread_rng());
            let spread = (i as f64 - (cfg.drones_per_raid as f64 - 1.) / 2.) * step;
            let start = from + lateral * spread;
            let quiet = Task::ComboTask(vec![
                Task::WrappedCommand(Command::SetUnlimitedFuel(true)),
                Task::WrappedOption(AiOption::Air(AirOption::Roe(AirRoe::WeaponHold))),
                Task::WrappedOption(AiOption::Air(AirOption::ReactionOnThreat(
                    AirReactionToThreat::NoReaction,
                ))),
            ]);
            let low = |p: Vector2, task| MissionPoint {
                alt_typ: Some(AltType::RADIO),
                ..air_point(p, cfg.drone_alt_agl_m, cfg.drone_speed_ms, task)
            };
            // Waypoint 0 is the launch spot; the spawn turns it into the
            // takeoff and keeps the quiet-running orders on it.
            let mission = vec![
                low(start, quiet),
                // Converge on the aim point from a little spread out.
                low(target_pos + lateral * spread, Task::ComboTask(vec![])),
            ];
            let mut spawned = None;
            for template in pool.iter() {
                match ctx.db.spawn_air_flight(
                    perf,
                    &spctx,
                    &ctx.idx,
                    side,
                    template.as_str(),
                    origin,
                    start,
                    heading_of(dir),
                    // One-way attack drones are launched off a rail or a
                    // strip of road, not a runway: from open ground where
                    // the site has no parking for them.
                    true,
                    mission.clone(),
                ) {
                    Ok(gid) => {
                        spawned = Some(gid);
                        break;
                    }
                    Err(e) => warn!("raids: drone template {template} would not spawn: {e:?}"),
                }
            }
            if let Some(gid) = spawned {
                raid.drones_launched += 1;
                raid.drones_pending += 1;
                let secs = leg / cfg.drone_speed_ms.max(1.) * 1.6 + 180. + DRONE_LAUNCH_SECS;
                ctx.modern_war.raids.drones.push(Drone {
                    gid,
                    raid: id,
                    expires: now + Duration::seconds(secs as i64),
                });
            }
        }
    }
    // Missiles.
    if cfg.use_missiles {
        let launchers = launchers_in_range(ctx, cfg, side, target_pos);
        let land_alt = land
            .as_ref()
            .and_then(|l| l.get_height(LuaVec2(target_pos)).ok())
            .unwrap_or(0.);
        let fire = Task::FireAtPoint {
            point: LuaVec2(target_pos),
            radius: Some(300.),
            expend_qty: None,
            weapon_type: None,
            altitude: Some(land_alt),
            altitude_type: Some(AltType::BARO),
            counter_battery_radius: crate::shoot_and_scoot(&ctx.db.ephemeral.cfg),
        };
        let mut far = 0f64;
        for (gid, n) in launchers.into_iter().take(cfg.max_launchers as usize) {
            let Some(name) = ctx.db.persisted.groups.get(&gid).map(|g| g.name.clone()) else {
                continue;
            };
            // A culled battery isn't in DCS to take the order.
            let Ok(group) = Group::get_by_name(lua, name.as_str()) else { continue };
            let center = ctx.db.group_center(&gid).unwrap_or(target_pos);
            let task = aim_and_fire_route(center, target_pos, group_facing(&ctx.db, &gid), fire.clone());
            match group.get_controller().and_then(|c| c.set_task(task)) {
                Ok(()) => {
                    raid.missiles += n;
                    far = far.max(dist(center, target_pos));
                }
                Err(e) => warn!("raids: launcher {name} would not fire: {e:?}"),
            }
        }
        if raid.missiles > 0 {
            raid.defenders = defence_near(ctx, side.opposite(), target_pos);
            raid.intercept_p = (cfg.intercept_per_site * raid.defenders.len() as f64).min(0.85);
            raid.missiles_resolve_at = Some(now + Duration::seconds((60. + far / 1_200.) as i64));
        }
    }
    if raid.drones_launched == 0 && raid.missiles == 0 {
        return false;
    }
    info!(
        "raids: {side:?} raid {id} on {target_name}: {} drones, {} missiles (intercept p {:.2})",
        raid.drones_launched, raid.missiles, raid.intercept_p
    );
    let what = match (raid.drones_launched, raid.missiles) {
        (0, m) => format_compact!("{m} ballistic missiles"),
        (d, 0) => format_compact!("{d} one-way attack drones"),
        (d, m) => format_compact!("{d} one-way attack drones and {m} ballistic missiles"),
    };
    ctx.db.ephemeral.msgs().panel_to_side(
        15,
        false,
        side,
        format_compact!("Strike launched on {target_name}: {what}."),
    );
    if cfg.warn_defenders {
        let eta = if raid.drones_launched > 0 {
            format_compact!(" Drones expected in about {:.0} minutes.", eta_min.max(1.))
        } else {
            format_compact!("")
        };
        ctx.db.ephemeral.msgs().panel_to_side(
            20,
            true,
            side.opposite(),
            format_compact!("AIR RAID WARNING: {what} inbound toward {target_name}.{eta}"),
        );
    }
    ctx.modern_war.raids.raids.push(raid);
    true
}

pub(crate) fn tick(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    cfg: &RaidsCfg,
    tempo: Option<&TempoCfg>,
    now: DateTime<Utc>,
) {
    // Drones in flight.
    let land = Land::singleton(lua).ok();
    let mut i = 0;
    while i < ctx.modern_war.raids.drones.len() {
        let d = &ctx.modern_war.raids.drones[i];
        let (gid, rid, expires) = (d.gid, d.raid, d.expires);
        let Some(ri) = ctx.modern_war.raids.raids.iter().position(|r| r.id == rid) else {
            let _ = ctx.db.delete_group(&gid);
            ctx.modern_war.raids.drones.swap_remove(i);
            continue;
        };
        let target_pos = ctx.modern_war.raids.raids[ri].target_pos;
        let pos = ctx
            .db
            .persisted
            .groups
            .get(&gid)
            .map(|g| g.name.clone())
            .and_then(|n| Group::get_by_name(lua, n.as_str()).ok())
            .and_then(|g| g.get_unit(1).ok())
            .and_then(|u| u.get_point().ok());
        let outcome = if !alive(ctx, &gid) || pos.is_none() {
            Some(false) // shot down
        } else if let Some(p) = pos.filter(|p| dist(Vector2::new(p.x, p.z), target_pos) <= DRONE_ARRIVAL_M) {
            // Detonate where it is, on the ground under it.
            let at = Vector2::new(p.x, p.z);
            let h = land.as_ref().and_then(|l| l.get_height(LuaVec2(at)).ok()).unwrap_or(p.y);
            if let Ok(act) = Trigger::singleton(lua).and_then(|t| t.action()) {
                let _ = act.explosion(LuaVec3(Vector3::new(at.x, h + 2., at.y)), cfg.drone_warhead_kg as f32);
            }
            Some(true)
        } else if now >= expires {
            Some(false)
        } else {
            None
        };
        match outcome {
            None => i += 1,
            Some(hit) => {
                let _ = ctx.db.delete_group(&gid);
                ctx.modern_war.raids.drones.swap_remove(i);
                let r = &mut ctx.modern_war.raids.raids[ri];
                r.drones_pending = r.drones_pending.saturating_sub(1);
                if hit {
                    r.drone_hits += 1;
                    apply_hit(lua, ctx, cfg, ri);
                }
            }
        }
    }
    // Missiles due to land.
    for ri in 0..ctx.modern_war.raids.raids.len() {
        let due = ctx.modern_war.raids.raids[ri]
            .missiles_resolve_at
            .map(|t| now >= t)
            .unwrap_or(false);
        if !due {
            continue;
        }
        let (n, p, defenders) = {
            let r = &mut ctx.modern_war.raids.raids[ri];
            r.missiles_resolve_at = None;
            (r.missiles, r.intercept_p, r.defenders.clone())
        };
        let mut hits = 0;
        for _ in 0..n {
            if thread_rng().gen_bool(p.clamp(0., 1.)) {
                // Two interceptors per missile, from a random nearby site.
                if let (Some(g), Some(sc)) = (
                    defenders.choose(&mut thread_rng()),
                    ctx.db
                        .ephemeral
                        .cfg
                        .modern_war
                        .as_ref()
                        .and_then(|m| m.sam_stock.clone())
                        .filter(|c| c.enabled),
                ) {
                    let g = *g;
                    let db = &ctx.db;
                    ctx.modern_war.sam.spend(db, &sc, g, 2);
                }
            } else {
                hits += 1;
            }
        }
        ctx.modern_war.raids.raids[ri].missile_hits = hits;
        for _ in 0..hits {
            apply_hit(lua, ctx, cfg, ri);
        }
    }
    // Finished raids: report and forget.
    let mut ri = 0;
    while ri < ctx.modern_war.raids.raids.len() {
        let r = &ctx.modern_war.raids.raids[ri];
        if r.drones_pending > 0 || r.missiles_resolve_at.is_some() {
            ri += 1;
            continue;
        }
        let r = ctx.modern_war.raids.raids.swap_remove(ri);
        let mut parts = vec![];
        if r.drones_launched > 0 {
            parts.push(format!("{}/{} drones got through", r.drone_hits, r.drones_launched));
        }
        if r.missiles > 0 {
            parts.push(format!("{}/{} missiles hit", r.missile_hits, r.missiles));
        }
        let mut effect = vec![];
        if r.supply_cut > 0 {
            effect.push(format!("stores cut {}%", r.supply_cut.min(100)));
        }
        if r.factory_halted {
            effect.push("production halted".to_string());
        }
        let effect = if effect.is_empty() {
            "no damage".to_string()
        } else {
            effect.join(", ")
        };
        info!("raids: raid {} on {} over: {} ({effect})", r.id, r.target_name, parts.join(", "));
        ctx.db.ephemeral.msgs().panel_to_side(
            15,
            false,
            r.side,
            format_compact!("Strike on {}: {}. {}.", r.target_name, parts.join(", "), effect),
        );
        ctx.db.ephemeral.msgs().panel_to_side(
            15,
            false,
            r.side.opposite(),
            format_compact!(
                "Raid on {} is over: {}. {}.",
                r.target_name,
                parts.join(", "),
                effect
            ),
        );
    }
    // New raids.
    if humans(ctx) == 0 && !cfg.run_when_empty {
        return;
    }
    if let Some(window) = cfg.hours {
        match mission_hour(lua) {
            Some(h) if in_hours(h, window) => (),
            _ => return,
        }
    }
    for side in [Side::Blue, Side::Red] {
        let factor = ctx.modern_war.tempo.factor(tempo, side, now);
        let Some(next) = ctx.modern_war.raids.next.get(&side).copied() else {
            // First look this session: don't open with a raid the moment the
            // server starts.
            let first = now + interval(cfg, factor * 0.5);
            ctx.modern_war.raids.next.insert(side, first);
            continue;
        };
        if now < next {
            continue;
        }
        let busy = ctx.modern_war.raids.raids.iter().any(|r| r.side == side);
        let launched = !busy && launch(lua, ctx, perf, cfg, side, now);
        let wait = if launched { interval(cfg, factor) } else { Duration::seconds(600) };
        ctx.modern_war.raids.next.insert(side, now + wait);
    }
}
