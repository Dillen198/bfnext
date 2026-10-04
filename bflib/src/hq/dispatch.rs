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

//! Running an operation through the system that already does it. The HQ
//! never spawns anything itself; it calls what a player's purchase would
//! have called, on the engine's account (no player charged, none credited),
//! and pays from the side's treasury only once the operation is actually up.

use super::{planner::Cand, requests, Handle, Op, OpStatus};
use crate::{
    db::actions::{ActionArgs, ActionCmd, WithJtac, WithObj, WithPos},
    spawnctx::SpawnCtx,
    Context,
};
use bfprotocols::{
    cfg::{ActionKind, HqCfg},
    db::group::GroupId,
    hq::OpKind,
    perf::PerfInner,
};
use chrono::{prelude::*, Duration};
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, MizLua};
use fxhash::FxHashSet;
use log::{info, warn};

type Res = Result<(Handle, CompactString), CompactString>;

fn err(e: anyhow::Error) -> CompactString {
    format_compact!("{e:#}")
}

/// The action groups the side has right now, to find the one an action call
/// just spawned.
fn action_groups(ctx: &Context, side: Side) -> FxHashSet<GroupId> {
    ctx.db.actions().filter(|g| g.side == side).map(|g| g.id).collect()
}

fn run_action(lua: MizLua, ctx: &mut Context, perf: &mut PerfInner, side: Side, c: &Cand) -> Result<Option<GroupId>, CompactString> {
    let (name, action) = c.action.clone().ok_or_else(|| CompactString::from("no action configured"))?;
    let args = match &action.kind {
        ActionKind::Recon(cfg) => ActionArgs::Recon(WithPos { cfg: cfg.clone(), pos: c.pos }),
        ActionKind::Drone(cfg) => ActionArgs::Drone(WithPos { cfg: cfg.clone(), pos: c.pos }),
        ActionKind::Artillery(cfg) => ActionArgs::Artillery(WithPos { cfg: cfg.clone(), pos: c.pos }),
        ActionKind::Reinforce(cfg) => {
            let oid = c.target.ok_or_else(|| CompactString::from("a reinforcement needs an objective"))?;
            ActionArgs::Reinforce(WithObj { cfg: cfg.clone(), oid })
        }
        ActionKind::Bomber(cfg) => {
            let jtac = c.jtac.clone().ok_or_else(|| CompactString::from("a bomber needs a JTAC target"))?;
            ActionArgs::Bomber(WithJtac { cfg: cfg.clone(), jtac })
        }
        ActionKind::Awacs(cfg) => ActionArgs::Awacs(WithPos { cfg: cfg.clone(), pos: c.pos }),
        ActionKind::Tanker(cfg) => ActionArgs::Tanker(WithPos { cfg: cfg.clone(), pos: c.pos }),
        ActionKind::NavalCruiseMissileStrike(cfg) => {
            let oid = c.target.ok_or_else(|| CompactString::from("a naval strike needs an objective"))?;
            ActionArgs::NavalCruiseMissileStrike(WithObj { cfg: cfg.clone(), oid })
        }
        ActionKind::LogisticsRepair(cfg) => {
            let oid = c.target.ok_or_else(|| CompactString::from("an air repair needs an objective"))?;
            ActionArgs::LogisticsRepair(WithObj { cfg: cfg.clone(), oid })
        }
        _ => return Err(format_compact!("{name} is not an action the HQ runs this way")),
    };
    let spctx = SpawnCtx::new(lua).map_err(err)?;
    let before = action_groups(ctx, side);
    ctx.db
        .start_action(lua, perf, &spctx, &ctx.idx, &ctx.jtac, side, None, ActionCmd { name, action, args })
        .map_err(err)?;
    Ok(action_groups(ctx, side).difference(&before).next().copied())
}

fn launch_inner(lua: MizLua, ctx: &mut Context, perf: &mut PerfInner, cfg: &HqCfg, side: Side, c: &Cand, now: DateTime<Utc>) -> Res {
    match c.kind {
        OpKind::Cap | OpKind::Strike | OpKind::Sead => {
            let (_, action) = c.action.clone().ok_or_else(|| CompactString::from("no action configured"))?;
            let id = ctx.hq.next_id();
            let name = format_compact!("HQ {} {id}", c.kind.label());
            let spctx = SpawnCtx::new(lua).map_err(err)?;
            let gid = ctx
                .db
                .spawn_auto_package(perf, &spctx, &ctx.idx, side, name.as_str().into(), action, c.pos)
                .map_err(err)?;
            let expires = now + Duration::seconds(cfg.package_lifetime_secs.max(300) as i64);
            Ok((Handle::Package { gid, expires }, format_compact!("{name} airborne")))
        }
        OpKind::Recon | OpKind::Reinforce | OpKind::Bomber | OpKind::Awacs | OpKind::Tanker | OpKind::AirRepair => {
            let gid = run_action(lua, ctx, perf, side, c)?;
            match gid {
                Some(gid) => Ok((Handle::Group(gid), "under way".into())),
                // The action ran but there is no group to follow.
                None => Ok((Handle::Fired, "ordered".into())),
            }
        }
        OpKind::NavalStrike => {
            run_action(lua, ctx, perf, side, c)?;
            Ok((Handle::Fired, "missiles away".into()))
        }
        OpKind::Artillery => {
            if c.action.is_some() {
                run_action(lua, ctx, perf, side, c)?;
            } else {
                let cfg = ctx
                    .db
                    .ephemeral
                    .cfg
                    .artillery
                    .clone()
                    .ok_or_else(|| CompactString::from("no artillery config"))?;
                ctx.db.artillery_strike(lua, side, None, WithPos { cfg, pos: c.pos }).map_err(err)?;
            }
            Ok((Handle::Fired, "fire mission sent".into()))
        }
        OpKind::MissileStrike => {
            let ecfg = ctx
                .db
                .ephemeral
                .cfg
                .campaign_events
                .clone()
                .ok_or_else(|| CompactString::from("campaign events are off"))?;
            if c.shooters.is_empty() {
                return Err("no launcher in range".into());
            }
            let mut msgs = vec![];
            ctx.event_scheduler.spawn_missile_strike_event(
                &ecfg,
                now,
                side,
                c.shooters.clone(),
                c.pos,
                c.name.clone(),
                &mut msgs,
            );
            for m in msgs {
                ctx.db.ephemeral.msgs().panel_to_all(15, false, m);
            }
            Ok((Handle::Fired, format_compact!("{} launcher(s) firing", c.shooters.len())))
        }
        OpKind::Ambush => {
            let ecfg = ctx
                .db
                .ephemeral
                .cfg
                .campaign_events
                .clone()
                .ok_or_else(|| CompactString::from("campaign events are off"))?;
            let cands = ctx.event_scheduler.build_candidates(&ctx.db);
            let (mut msgs, mut effects) = (vec![], vec![]);
            let ok = ctx.event_scheduler.spawn_convoy_ambush(&ctx.db, &ecfg, now, side, &cands, &mut msgs, &mut effects);
            ctx.event_scheduler.pending_effects.extend(effects);
            for m in msgs {
                ctx.db.ephemeral.msgs().panel_to_all(15, false, m);
            }
            if ok {
                Ok((Handle::Fired, "ambush set".into()))
            } else {
                Err("no enemy convoy to ambush".into())
            }
        }
        OpKind::Convoy => {
            let dest = c.target.ok_or_else(|| CompactString::from("no destination"))?;
            let (id, how) = ctx.db.hq_dispatch_supply(lua, side, dest, now).map_err(err)?;
            ctx.db.ephemeral.hq_watch.insert(id.clone());
            Ok((Handle::Transport(id), format_compact!("{how} dispatched")))
        }
        OpKind::HeloSupply => {
            let dest = c.target.ok_or_else(|| CompactString::from("no destination"))?;
            let id = ctx.db.call_helo_resource_delivery(lua, side, None, dest, now).map_err(err)?;
            ctx.db.ephemeral.hq_watch.insert(id.clone());
            Ok((Handle::Transport(id), "helo starting up".into()))
        }
        OpKind::HeloTroops => {
            let dest = c.target.ok_or_else(|| CompactString::from("no destination"))?;
            let id = ctx.db.call_helo_troop_insertion(lua, side, None, dest, now).map_err(err)?;
            ctx.db.ephemeral.hq_watch.insert(id.clone());
            Ok((Handle::Transport(id), "helo starting up".into()))
        }
    }
}

/// The flights that go with a strike: fighter escorts (tasked to escort the
/// strike once both are in the air, see `super::track`) and SEAD at the air
/// defence covering the target. A flight that won't launch is left out; the
/// strike goes anyway. Returns (group, role) for each.
fn package_flights(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    cfg: &HqCfg,
    side: Side,
    c: &Cand,
    main: Option<GroupId>,
) -> Vec<(GroupId, &'static str)> {
    let mut out = vec![];
    if c.escort == 0 && c.sead_at.is_none() {
        return out;
    }
    let avail = super::planner::available(ctx, cfg, side);
    let pic = super::picture::build(ctx, cfg, side, Utc::now());
    let night = ctx.hq.night;
    let Ok(spctx) = SpawnCtx::new(lua) else { return out };
    let fly = |ctx: &mut Context, perf: &mut PerfInner, kind: OpKind, role: &'static str, pos| {
        // The escort is chosen for the enemy air over the target; the SEAD
        // for the air defence it is going after.
        let target = if kind == OpKind::Sead { "sam" } else { "base" };
        let sit = super::planner::Situation::at(&pic, pos, night, target);
        let chosen = avail
            .get(&kind)
            .and_then(|a| super::planner::pick_air(&a.options, &sit))
            .map(|o| o.action.clone());
        let Some(action) = chosen.or_else(|| avail.get(&kind).and_then(|a| a.action.clone()).map(|(_, a)| a)) else {
            return None;
        };
        let id = ctx.hq.next_id();
        let name = format_compact!("HQ {role} {id}");
        match ctx.db.spawn_auto_package(perf, &spctx, &ctx.idx, side, name.as_str().into(), action, pos) {
            Ok(gid) => Some(gid),
            Err(e) => {
                info!("hq: {side:?} {role} for {} would not launch: {e:#}", c.name);
                None
            }
        }
    };
    if let Some(sam) = c.sead_at {
        if let Some(gid) = fly(ctx, perf, OpKind::Sead, "SEAD", sam) {
            out.push((gid, "sead"));
        }
    }
    for _ in 0..c.escort {
        if let Some(gid) = fly(ctx, perf, OpKind::Cap, "escort", c.pos) {
            out.push((gid, "escort"));
            if let Some(main) = main {
                ctx.hq.side(side).escorts.push(super::PendingEscort { escort: gid, escorted: main, since: Utc::now() });
            }
        }
    }
    out
}

/// Run `c`, charge for it and start following it. Ok is what to tell the
/// side.
pub(crate) fn launch(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    cfg: &HqCfg,
    side: Side,
    c: &Cand,
    now: DateTime<Utc>,
) -> Result<CompactString, CompactString> {
    let (handle, detail) = match launch_inner(lua, ctx, perf, cfg, side, c, now) {
        Ok(r) => r,
        Err(e) => {
            info!("hq: {side:?} {} on {} would not launch: {e}", c.kind.label(), c.name);
            return Err(e);
        }
    };
    let main = match &handle {
        Handle::Package { gid, .. } | Handle::Group(gid) => Some(*gid),
        _ => None,
    };
    let support = package_flights(lua, ctx, perf, cfg, side, c, main);
    // Only pay for the package flights that actually went.
    let unused = {
        let avail = super::planner::available(ctx, cfg, side);
        let escorts_up = support.iter().filter(|(_, r)| *r == "escort").count() as i64;
        let sead_up = support.iter().any(|(_, r)| *r == "sead");
        let ecost = avail.get(&OpKind::Cap).map_or(0, |a| a.cost);
        let scost = avail.get(&OpKind::Sead).map_or(0, |a| a.cost);
        (c.escort as i64 - escorts_up).max(0) * ecost
            + if c.sead_at.is_some() && !sead_up { scost } else { 0 }
    };
    let cost = (c.cost - unused).max(0);
    let detail = if support.is_empty() {
        detail
    } else {
        let e = support.iter().filter(|(_, r)| *r == "escort").count();
        let sd = support.iter().filter(|(_, r)| *r == "sead").count();
        let mut d = detail;
        if e > 0 {
            d.push_str(&format_compact!(", {e} escort flight(s)"));
        }
        if sd > 0 {
            d.push_str(", SEAD going in first");
        }
        d
    };
    let bal = ctx.db.persisted.adjust_treasury(side, -cost);
    ctx.db.ephemeral.dirty();
    let r = ctx.db.persisted.hq.side_mut(side).record.entry(c.kind).or_default();
    r.launched += 1;
    let id = ctx.hq.next_id();
    let text = format_compact!("HQ: {} on {} -- {} ({})", c.kind.label(), c.name, c.why, detail);
    info!("hq: {side:?} {text} [score {:.0}, cost {cost}, treasury {bal}]", c.score);
    let fires = c.kind.line() == bfprotocols::hq::Line::Fires;
    {
        let rt = ctx.hq.side(side);
        if fires {
            rt.last_fired.insert(c.anchor, now);
        }
        rt.note(now, text.clone());
        rt.ops.push(Op {
            id,
            kind: c.kind,
            target: c.target,
            target_name: c.name.clone(),
            pos: c.pos,
            started: now,
            cost,
            handle,
            support,
            status: OpStatus::Active,
            detail: detail.clone(),
            request: c.request,
            ended: None,
        });
    }
    if let Some(rid) = c.request {
        requests::answered(ctx, side, rid, &format_compact!("{} tasked ({detail})", c.kind.label()));
    }
    if cfg.announce {
        ctx.db.ephemeral.msgs().panel_to_side(
            15,
            false,
            side,
            format_compact!("HQ: {} tasked on {}.", c.kind.label(), c.name),
        );
    }
    if bal <= 0 {
        warn!("hq: {side:?} treasury is empty");
    }
    Ok(detail)
}
