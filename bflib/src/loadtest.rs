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

//! TESTING ONLY: simulated players (`Cfg::load_test`).
//!
//! A full server can't be staged with real clients, so this stands in for
//! their load. Each simulated player registers on a side and, for as long as
//! the mission runs:
//!
//! - flies an AI copy of a player aircraft (a slot template, as the wingman
//!   copy uses) from a friendly field to the front, fights there, lands
//!   after `sortie_minutes` and launches again;
//! - every `act_every_secs` (jittered) does what players do on the ground:
//!   deploys a unit or troops near the front, buys an action aimed at the
//!   front, moves supplies between bases, or forms a formation from its
//!   deployed units.
//!
//! Every minute it logs what it did, what was refused and whether the
//! simulation is keeping up with the clock (game seconds per real second);
//! the engine's own perf tables follow every ten minutes. Never on a live
//! server: the config refuses quietly to be absent, and logs a warning when
//! present.

use crate::{
    Context,
    airlife::{air_point, alive, front_points, heading_of, send_home},
    db::{Db, actions::ActionCmd, group::DeployKind, logistics::Carries},
    menu::action::pos_action_args,
    spawnctx::{SpawnCtx, SpawnLoc},
};
use bfprotocols::{
    cfg::{DeployableKind, LoadTestCfg, UnitTag},
    db::group::GroupId,
    perf::PerfInner,
};
use chrono::{Duration, prelude::*};
use dcso3::{
    LuaVec2, MizLua, String, Vector2,
    attribute::Attribute,
    coalition::Side,
    controller::{OrbitPattern, Task},
    land::Land,
    net::Ucid,
    timer::Timer,
};
use enumflags2::BitFlags;
use fxhash::FxHashMap;
use log::{info, warn};
use rand::{Rng, seq::SliceRandom, thread_rng};
use std::collections::VecDeque;

/// How often the simulation is looked after.
const TICK_SECS: i64 = 5;
/// At most this many AI copies launch a side per tick, so the fleet builds up
/// the way players join, not in one spike.
const LAUNCH_PER_TICK: usize = 2;
/// Points each simulated player starts with: enough never to be the limit.
const BUDGET: i32 = 1_000_000;

#[derive(Debug)]
struct Sim {
    ucid: Ucid,
    side: Side,
    next_act: DateTime<Utc>,
    flight: Option<(GroupId, DateTime<Utc>)>,
    deployed: VecDeque<GroupId>,
}

#[derive(Debug, Default)]
pub(crate) struct LoadTest {
    sims: Vec<Sim>,
    last_tick: Option<DateTime<Utc>>,
    /// (real time, game time) at the last report.
    last_report: Option<(DateTime<Utc>, f64)>,
    counts: FxHashMap<&'static str, u32>,
    /// Next slot type to fly, per side: every type is flown in turn.
    next_type: FxHashMap<Side, usize>,
    /// Per (side, aircraft type): (launched, refused, last refusal).
    by_type: FxHashMap<(Side, String), (u32, u32, std::string::String)>,
    last_type_report: Option<DateTime<Utc>>,
    /// The simulated players have joined (once a mission, even when there
    /// are none, as in a baseline run).
    joined: bool,
}

impl LoadTest {
    fn count(&mut self, what: &'static str) {
        *self.counts.entry(what).or_default() += 1;
    }
}

/// The deterministic id of simulated player `n`: 32 hex digits, starting
/// "10ad7e57" so it can't be mistaken for a real one in the logs.
fn sim_ucid(n: u32) -> Ucid {
    format!("10ad7e57{n:024x}").parse().expect("a valid ucid")
}

pub(crate) fn tick(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    now: DateTime<Utc>,
) {
    let Some(cfg) = ctx.db.ephemeral.cfg.load_test.clone().filter(|c| c.enabled) else {
        return;
    };
    if ctx.loadtest.last_tick.is_some_and(|t| (now - t).num_seconds() < TICK_SECS) {
        return;
    }
    ctx.loadtest.last_tick = Some(now);
    if !ctx.loadtest.joined {
        ctx.loadtest.joined = true;
        join(ctx, &cfg, now);
    }
    fly(lua, ctx, perf, &cfg, now);
    act(lua, ctx, perf, &cfg, now);
    report(lua, ctx, now);
}

/// Register the simulated players, half a side each.
fn join(ctx: &mut Context, cfg: &LoadTestCfg, now: DateTime<Utc>) {
    let mut rng = thread_rng();
    for (k, side) in [Side::Blue, Side::Red].into_iter().enumerate() {
        for i in 0..cfg.players_per_side {
            let n = k as u32 * 1000 + i;
            let ucid = sim_ucid(n);
            let name = String::from(format!("LoadTest {side:?} {:02}", i + 1));
            // Ok, or already registered by an earlier run of this save
            let _ = ctx.db.register_player(ucid, name, side);
            ctx.db.adjust_points_silent(&ucid, BUDGET, "load test budget");
            let jitter = rng.gen_range(0..cfg.act_every_secs.max(1) as i64);
            ctx.loadtest.sims.push(Sim {
                ucid,
                side,
                next_act: now + Duration::seconds(jitter),
                flight: None,
                deployed: VecDeque::new(),
            });
        }
    }
    warn!(
        "[LOAD_TEST] {} simulated players joined ({} a side). TEST SERVER ONLY.",
        ctx.loadtest.sims.len(),
        cfg.players_per_side
    );
}

/// Keep the share of simulated players airborne, as AI copies of player
/// aircraft.
fn fly(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    cfg: &LoadTestCfg,
    now: DateTime<Utc>,
) {
    // land the ones whose sortie is over; forget the ones shot down
    for i in 0..ctx.loadtest.sims.len() {
        let Some((gid, due)) = ctx.loadtest.sims[i].flight else { continue };
        if !alive(ctx, &gid) {
            ctx.loadtest.sims[i].flight = None;
            let _ = ctx.db.delete_group(&gid);
            ctx.loadtest.count("aircraft lost");
        } else if now >= due {
            ctx.loadtest.sims[i].flight = None;
            send_home(lua, ctx, gid, now);
            ctx.loadtest.count("sorties flown");
        }
    }
    let front = front_points(ctx);
    if front.is_empty() {
        return;
    }
    let want = (cfg.players_per_side as f64 * cfg.airborne_share).round() as usize;
    for side in [Side::Blue, Side::Red] {
        let up = ctx
            .loadtest
            .sims
            .iter()
            .filter(|s| s.side == side && s.flight.is_some())
            .count();
        let idle: Vec<usize> = ctx
            .loadtest
            .sims
            .iter()
            .enumerate()
            .filter(|(_, s)| s.side == side && s.flight.is_none())
            .map(|(i, _)| i)
            .collect();
        for &i in idle.iter().take(want.saturating_sub(up).min(LAUNCH_PER_TICK)) {
            match launch(lua, ctx, perf, side, &front) {
                Ok((gid, typ)) => {
                    ctx.loadtest.by_type.entry((side, typ)).or_default().0 += 1;
                    let mins =
                        cfg.sortie_minutes as f64 * thread_rng().gen_range(0.8..1.2);
                    ctx.loadtest.sims[i].flight =
                        Some((gid, now + Duration::seconds((mins * 60.) as i64)));
                    ctx.loadtest.count("aircraft launched");
                }
                Err((typ, e)) => {
                    ctx.loadtest.count("launch refused");
                    let t = ctx.loadtest.by_type.entry((side, typ.clone())).or_default();
                    t.1 += 1;
                    t.2 = format!("{e:#}").chars().take(160).collect();
                    log::debug!("[LOAD_TEST] {side:?} {typ} launch refused: {e:?}");
                }
            }
        }
    }
}

/// One AI copy of a player aircraft of `side` -- the next slot type in turn,
/// so a run flies every aircraft the mission offers -- from the friendly
/// field nearest a random point of the front, to orbit and fight over it.
/// Returns the group and the type, or the type and why it couldn't launch.
fn launch(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    side: Side,
    front: &[Vector2],
) -> Result<(GroupId, String), (String, anyhow::Error)> {
    let mut slots: Vec<(String, String)> = ctx
        .db
        .ephemeral
        .clone_templates
        .iter()
        .filter(|((s, _), _)| *s == side)
        .map(|((_, typ), t)| (typ.clone(), t.clone()))
        .collect();
    if slots.is_empty() {
        return Err((String::from("none"), anyhow::anyhow!("no slot templates")));
    }
    slots.sort();
    let n = ctx.loadtest.next_type.entry(side).or_default();
    let (typ, template) = slots[*n % slots.len()].clone();
    *n += 1;
    launch_type(lua, ctx, perf, side, front, &typ, &template)
        .map(|gid| (gid, typ.clone()))
        .map_err(|e| (typ, e))
}

fn launch_type(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    side: Side,
    front: &[Vector2],
    typ: &String,
    template: &String,
) -> anyhow::Result<GroupId> {
    let mut rng = thread_rng();
    let rotary = ctx
        .db
        .ephemeral
        .cfg
        .unit_classification
        .get(&bfprotocols::cfg::Vehicle::from(typ.clone()))
        .is_some_and(|t| t.contains(UnitTag::Helicopter));
    let target = *front.choose(&mut rng).expect("not empty");
    let field = ctx
        .db
        .pick_launch_field(
            lua,
            side,
            target,
            rotary,
            Db::needs_big_spot(typ.as_str()),
            0.,
        )
        .ok_or_else(|| anyhow::anyhow!("no field for a {typ}"))?;
    let ground = Land::singleton(lua)?;
    let elev = ground.get_height(LuaVec2(field.pos)).unwrap_or(0.);
    let over = ground.get_height(LuaVec2(target)).unwrap_or(0.);
    let (alt, speed, radius, targets) = if rotary {
        (over + 150., 55., 6_000., vec![Attribute::GroundUnits, Attribute::Helicopters])
    } else {
        (over + 6_000., 220., 25_000., vec![Attribute::Air, Attribute::GroundUnits])
    };
    let mission = vec![
        air_point(field.pos, elev, speed, Task::ComboTask(vec![])),
        air_point(
            target,
            alt,
            speed,
            Task::ComboTask(vec![
                Task::Orbit {
                    pattern: OrbitPattern::Circle,
                    point: Some(LuaVec2(target)),
                    point2: None,
                    speed: Some(speed),
                    altitude: Some(alt),
                },
                Task::EngageTargetsInZone {
                    point: LuaVec2(target),
                    zone_radius: radius,
                    target_types: targets,
                    priority: None,
                },
            ]),
        ),
    ];
    let spctx = SpawnCtx::new(lua)?;
    ctx.db.spawn_air_flight(
        perf,
        &spctx,
        &ctx.idx,
        side,
        template.as_str(),
        field.oid,
        field.pos,
        heading_of(target - field.pos),
        false,
        mission,
    )
}

/// The ground, logistics and action moves of every simulated player whose
/// turn it is.
fn act(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    cfg: &LoadTestCfg,
    now: DateTime<Utc>,
) {
    let front = front_points(ctx);
    if front.is_empty() {
        return;
    }
    let due: Vec<usize> = ctx
        .loadtest
        .sims
        .iter()
        .enumerate()
        .filter(|(_, s)| s.next_act <= now)
        .map(|(i, _)| i)
        .collect();
    for i in due {
        let every = cfg.act_every_secs.max(10) as f64;
        let next = every * thread_rng().gen_range(0.5..1.5);
        ctx.loadtest.sims[i].next_act = now + Duration::seconds(next as i64);
        let roll = thread_rng().gen_range(0..100);
        if (75..88).contains(&roll) {
            match carrier(lua, ctx, i) {
                Ok(done) => ctx.loadtest.count(done),
                Err(e) => {
                    ctx.loadtest.count("carrier refused");
                    log::debug!("[LOAD_TEST] carrier refused: {e:?}");
                }
            }
            continue;
        }
        let (what, res) = match roll {
            0..25 => ("deploy", deploy(lua, ctx, i, cfg, &front, false)),
            25..37 => ("troops", deploy(lua, ctx, i, cfg, &front, true)),
            37..60 => ("action", action(lua, ctx, perf, i, &front)),
            60..75 => ("supply run", supply(lua, ctx, i, &front)),
            _ => ("formation", formation(lua, ctx, i, now)),
        };
        match res {
            Ok(()) => ctx.loadtest.count(match what {
                "deploy" => "deployed",
                "troops" => "troops dropped",
                "action" => "actions bought",
                "supply run" => "supply runs",
                _ => "formations formed",
            }),
            Err(e) => {
                ctx.loadtest.count(match what {
                    "deploy" => "deploy refused",
                    "troops" => "troops refused",
                    "action" => "action refused",
                    "supply run" => "supply refused",
                    _ => "formation refused",
                });
                log::debug!("[LOAD_TEST] {what} refused: {e:?}");
            }
        }
    }
}

/// One of the simulated player's own IFVs/APCs/trucks: its squad out if it
/// is aboard, back in if it is out (`db::carrier`). The deployed vehicles
/// stand still, so the squad is still beside it to embark.
fn carrier(lua: MizLua, ctx: &mut Context, i: usize) -> anyhow::Result<&'static str> {
    let ucid = ctx.loadtest.sims[i].ucid;
    let uid = *ctx
        .db
        .carriers_of(&ucid)
        .choose(&mut thread_rng())
        .ok_or_else(|| anyhow::anyhow!("no troop carrier deployed"))?;
    match ctx.db.carrier_aboard(&uid)? {
        crate::db::carrier::Aboard::Out(_) => {
            ctx.db.embark_squad(lua, &uid, &ucid)?;
            Ok("carrier embarks")
        }
        a if a.loaded() => {
            ctx.db.dismount_squad(lua, &ctx.idx, &uid, &ucid)?;
            Ok("carrier dismounts")
        }
        _ => {
            // its squad is lost: the second try must be refused too
            match ctx.db.dismount_squad(lua, &ctx.idx, &uid, &ucid) {
                Ok(_) => {
                    log::error!(
                        "[LOAD_TEST] a carrier whose squad was lost dismounted another"
                    );
                    Ok("carrier DUPLICATED a squad")
                }
                Err(_) => Ok("carrier empty (refused, as it should be)"),
            }
        }
    }
}

/// The friendly objective of `side` nearest a random point of the front.
fn near_front(ctx: &Context, side: Side, front: &[Vector2]) -> Option<Vector2> {
    let p = *front.choose(&mut thread_rng())?;
    ctx.db
        .objectives()
        .filter(|(_, o)| o.owner() == side)
        .map(|(_, o)| o.pos())
        .min_by(|a, b| (*a - p).norm().total_cmp(&(*b - p).norm()))
}

fn deploy(
    lua: MizLua,
    ctx: &mut Context,
    i: usize,
    cfg: &LoadTestCfg,
    front: &[Vector2],
    troops: bool,
) -> anyhow::Result<()> {
    let (ucid, side) = (ctx.loadtest.sims[i].ucid, ctx.loadtest.sims[i].side);
    let base =
        near_front(ctx, side, front).ok_or_else(|| anyhow::anyhow!("no front base"))?;
    let mut rng = thread_rng();
    let a: f64 = rng.gen_range(0.0..std::f64::consts::TAU);
    let pos = base + Vector2::new(a.cos(), a.sin()) * rng.gen_range(600.0..1_800.0);
    let spctx = SpawnCtx::new(lua)?;
    let loc = SpawnLoc::AtPos {
        pos,
        offset_direction: Vector2::new(a.cos(), a.sin()),
        group_heading: a,
    };
    let gid = if troops {
        let spec = ctx
            .db
            .ephemeral
            .cfg
            .troops
            .get(&side)
            .and_then(|t| t.choose(&mut rng))
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("no troops"))?;
        let origin = DeployKind::Troop {
            player: ucid,
            moved_by: None,
            spec: spec.clone(),
            origin: None,
            cost_fraction: 1.,
            jtac: None,
        };
        ctx.db.adjust_points_silent(&ucid, -(spec.cost as i32), "load test troops");
        ctx.db.add_and_queue_group(
            &spctx,
            &ctx.idx,
            side,
            loc,
            &spec.template,
            origin,
            BitFlags::empty(),
            None,
        )?
    } else {
        let specs: Vec<_> = ctx
            .db
            .ephemeral
            .cfg
            .deployables
            .get(&side)
            .map(|d| {
                d.iter()
                    .filter(|d| matches!(d.kind, DeployableKind::Group { .. }))
                    .cloned()
                    .collect()
            })
            .unwrap_or_default();
        let spec = specs
            .choose(&mut rng)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("no deployables"))?;
        let DeployableKind::Group { template } = &spec.kind else { unreachable!() };
        let template = template.clone();
        let origin = DeployKind::Deployed {
            player: ucid,
            moved_by: None,
            spec: spec.clone(),
            cost_fraction: 1.0,
            origin: None,
            jtac: None,
            ordered: false,
        };
        ctx.db.adjust_points_silent(&ucid, -(spec.cost as i32), "load test deploy");
        ctx.db.add_and_queue_group(
            &spctx,
            &ctx.idx,
            side,
            loc,
            &template,
            origin,
            BitFlags::empty(),
            None,
        )?
    };
    // a player keeps only so many: the oldest goes, as deployable limits do
    let sim = &mut ctx.loadtest.sims[i];
    sim.deployed.push_back(gid);
    if sim.deployed.len() > cfg.deployed_per_player.max(1) as usize
        && let Some(old) = sim.deployed.pop_front()
    {
        let _ = ctx.db.delete_group(&old);
    }
    Ok(())
}

fn action(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    i: usize,
    front: &[Vector2],
) -> anyhow::Result<()> {
    let (ucid, side) = (ctx.loadtest.sims[i].ucid, ctx.loadtest.sims[i].side);
    let mut rng = thread_rng();
    let p = *front.choose(&mut rng).ok_or_else(|| anyhow::anyhow!("no front"))?;
    let pos = p + Vector2::new(
        rng.gen_range(-10_000.0..10_000.0),
        rng.gen_range(-10_000.0..10_000.0),
    );
    let mut actions: Vec<(String, bfprotocols::cfg::Action)> = ctx
        .db
        .ephemeral
        .cfg
        .actions
        .get(&side)
        .map(|a| {
            a.iter()
                .filter(|(_, a)| pos_action_args(&a.kind, pos).is_some())
                .map(|(n, a)| (n.clone(), a.clone()))
                .collect()
        })
        .unwrap_or_default();
    actions.shuffle(&mut rng);
    let (name, action) =
        actions.pop().ok_or_else(|| anyhow::anyhow!("no position actions"))?;
    let args = pos_action_args(&action.kind, pos).expect("filtered");
    let spctx = SpawnCtx::new(lua)?;
    ctx.db.start_action(
        lua,
        perf,
        &spctx,
        &ctx.idx,
        &ctx.jtac,
        side,
        Some(ucid),
        ActionCmd { name, action, args },
    )
}

/// A crate run: supplies from a friendly airbase in the rear to the friendly
/// airbase nearest the front.
fn supply(
    lua: MizLua,
    ctx: &mut Context,
    i: usize,
    front: &[Vector2],
) -> anyhow::Result<()> {
    let side = ctx.loadtest.sims[i].side;
    let p =
        *front.choose(&mut thread_rng()).ok_or_else(|| anyhow::anyhow!("no front"))?;
    let mut bases: Vec<_> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == side && o.is_airbase())
        .map(|(id, o)| (*id, (o.pos() - p).norm()))
        .collect();
    bases.sort_by(|a, b| a.1.total_cmp(&b.1));
    let (to, from) = match (bases.first(), bases.last()) {
        (Some(t), Some(f)) if t.0 != f.0 => (t.0, f.0),
        _ => anyhow::bail!("not two friendly airbases"),
    };
    ctx.db.transfer_supplies(lua, from, to, &Carries::everything())?;
    Ok(())
}

fn formation(
    lua: MizLua,
    ctx: &mut Context,
    i: usize,
    now: DateTime<Utc>,
) -> anyhow::Result<()> {
    let (ucid, side) = (ctx.loadtest.sims[i].ucid, ctx.loadtest.sims[i].side);
    let at = ctx.loadtest.sims[i]
        .deployed
        .iter()
        .find_map(|g| ctx.db.group_center(g).ok())
        .ok_or_else(|| anyhow::anyhow!("nothing deployed"))?;
    ctx.db.form_from_deployed(&mut ctx.groundwar.rt, lua, side, ucid, at, now)?;
    // the groups are the formation's now
    ctx.loadtest.sims[i].deployed.clear();
    Ok(())
}

/// Once a minute: what the simulated players did, and whether the game
/// clock is keeping up with the real one.
fn report(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let game =
        Timer::singleton(lua).and_then(|t| t.get_time()).map(|t| t.0).unwrap_or(0.);
    let Some((then, game_then)) = ctx.loadtest.last_report else {
        ctx.loadtest.last_report = Some((now, game));
        return;
    };
    let real = (now - then).num_milliseconds() as f64 / 1000.;
    if real < 60. {
        return;
    }
    ctx.loadtest.last_report = Some((now, game));
    let airborne = ctx.loadtest.sims.iter().filter(|s| s.flight.is_some()).count();
    let mut counts: Vec<_> =
        ctx.loadtest.counts.iter().map(|(k, v)| format!("{k} {v}")).collect();
    counts.sort();
    // every ten minutes: how each aircraft type is doing
    if ctx.loadtest.last_type_report.is_none_or(|t| (now - t).num_seconds() >= 600) {
        ctx.loadtest.last_type_report = Some(now);
        let mut types: Vec<_> = ctx.loadtest.by_type.iter().collect();
        types.sort_by(|a, b| (a.0.0 as u8, &a.0.1).cmp(&(b.0.0 as u8, &b.0.1)));
        for ((side, typ), (ok, bad, why)) in types {
            let why = if *bad > 0 {
                format!(" (last: {why})")
            } else {
                std::string::String::new()
            };
            info!("[LOAD_TEST_TYPE] {side:?} {typ}: launched {ok}, refused {bad}{why}");
        }
    }
    info!(
        "[LOAD_TEST] {} players, {airborne} airborne; game {:.2}x real time; {} groups in the db; {}",
        ctx.loadtest.sims.len(),
        (game - game_then) / real,
        ctx.db.persisted.groups.len(),
        counts.join(", ")
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simulated_ucids_are_valid_and_distinct() {
        let a = sim_ucid(0);
        let b = sim_ucid(1001);
        assert_ne!(a, b);
        assert!(a.to_string().starts_with("10ad7e57"));
    }
}
