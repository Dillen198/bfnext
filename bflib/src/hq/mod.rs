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

//! The theatre HQ (`smart_commander.hq`): each coalition's AI commander.
//!
//! Every planning pass, for each side it commands:
//!
//! 1. `picture`: what the side can see -- its own bases in full, the
//!    enemy's objectives (public), enemy aircraft its radars hold, enemy
//!    ground its intel holds, how many of its humans are up and doing what.
//! 2. `strategy`: a posture, a main effort, the bases to hold and the ones
//!    to resupply first. A human override wins, then the strategist's
//!    directive (bfdb's language model), then the HQ's own rules -- field by
//!    field, so a directive that only names a main effort still gets the
//!    rules' supply priorities.
//! 3. `planner`: every operation the side could run right now, valued
//!    against that plan, scaled down for each human already doing the job,
//!    and bought out of the treasury best-value first.
//! 4. `dispatch`: the operation is run by the system that already does it --
//!    `spawn_auto_package` for air, the Artillery / Recon / Reinforce
//!    actions on the engine's account, the logistics system's convoys and
//!    helo missions, the campaign events' missile strike and ambush. The HQ
//!    owns no units of its own.
//!
//! Operations are tracked to the end (`track`): a package that comes home,
//! a convoy that delivers, a helo that lands count as successes, the rest as
//! failures, and that record feeds back into how much the HQ trusts each
//! kind of operation. The record and any human override are saved with the
//! campaign.
//!
//! Players see the plan (F10 > Info > HQ, `-hq`, the dashboard), can ask
//! for support (`-request`, the same menu), and -- if `override_rule` lets
//! them -- take command.

mod chat;
mod dispatch;
mod picture;
mod planner;
mod requests;
mod strategy;
mod view;

pub(crate) use chat::{chat, run_chat};
pub(crate) use view::{intent_text, ops_text, view};

use crate::{db::tasks::TaskId, Context};
use bfprotocols::{
    cfg::{Cfg, HqCfg, SmartCommanderCfg},
    db::{group::GroupId, objective::ObjectiveId},
    hq::{Directive, HqCommand, HqReply, Line, OpKind, Posture, RequestKind},
    perf::PerfInner,
};
use chrono::{prelude::*, Duration};
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, net::Ucid, MizLua, Vector2};
use fxhash::FxHashMap;
use log::{info, warn};
use serde_derive::{Deserialize, Serialize};
use std::collections::{BTreeMap, VecDeque};

/// Finished operations kept for the record, per side.
const KEEP_FINISHED: usize = 25;
/// Log lines kept per side.
const KEEP_LOG: usize = 40;
/// A transport the HQ has heard nothing about for this long is written off.
const TRANSPORT_GIVEUP_SECS: i64 = 4 * 3600;

pub(crate) fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

/// The HQ config, if it is on.
pub(crate) fn cfg(ctx: &Context) -> Option<(SmartCommanderCfg, HqCfg)> {
    let sc = ctx.db.ephemeral.cfg.smart_commander.as_ref()?;
    let hq = sc.hq.as_ref().filter(|h| h.enabled)?;
    Some((sc.clone(), hq.clone()))
}

/// The HQ is running the war (so the older systems it replaces -- the Smart
/// Commander's four event actions and air_life's packages -- stand down).
pub(crate) fn active(cfg: &Cfg) -> bool {
    cfg.smart_commander
        .as_ref()
        .and_then(|s| s.hq.as_ref())
        .map_or(false, |h| h.enabled)
}

// ---------------------------------------------------------------------------
// Saved with the campaign
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct Record {
    pub launched: u32,
    pub succeeded: u32,
    pub failed: u32,
}

impl Record {
    /// How far the HQ trusts this kind of operation, around 1.0: a kind
    /// that keeps failing (a strike package that never comes back) is worth
    /// less than its value says, one that keeps working a little more.
    /// Laplace-smoothed, so a handful of outcomes only nudge it.
    pub fn trust(&self) -> f64 {
        let p = (self.succeeded as f64 + 2.) / ((self.succeeded + self.failed) as f64 + 4.);
        0.5 + p
    }
}

/// A human commander's orders.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Override {
    pub by: CompactString,
    pub set: DateTime<Utc>,
    pub expires: DateTime<Utc>,
    pub directive: Directive,
    #[serde(default)]
    pub disabled_ops: Vec<OpKind>,
    #[serde(default)]
    pub paused: bool,
}

/// The strategist's latest directive.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HeldDirective {
    pub directive: Directive,
    pub received: DateTime<Utc>,
    pub expires: DateTime<Utc>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SideSaved {
    #[serde(default)]
    pub human: Option<Override>,
    #[serde(default)]
    pub directive: Option<HeldDirective>,
    #[serde(default)]
    pub record: BTreeMap<OpKind, Record>,
}

/// What the HQ keeps in the save (`Persisted::hq`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HqSaved {
    #[serde(default)]
    pub blue: SideSaved,
    #[serde(default)]
    pub red: SideSaved,
}

impl HqSaved {
    pub fn side(&self, side: Side) -> &SideSaved {
        match side {
            Side::Red => &self.red,
            _ => &self.blue,
        }
    }

    pub fn side_mut(&mut self, side: Side) -> &mut SideSaved {
        match side {
            Side::Red => &mut self.red,
            _ => &mut self.blue,
        }
    }
}

// ---------------------------------------------------------------------------
// Session state
// ---------------------------------------------------------------------------

/// How the HQ follows an operation it started.
#[derive(Debug, Clone)]
pub(crate) enum Handle {
    /// An AI air package: sent home at `expires`, a failure if it dies first.
    Package { gid: GroupId, expires: DateTime<Utc> },
    /// A group an action spawned (recon drone, reinforcement convoy): a
    /// success when the action is done with it and it is gone, a failure if
    /// it is destroyed.
    Group(GroupId),
    /// A convoy, cargo flight or helo mission, by its logistics id. The
    /// logistics system reports how it ended.
    Transport(CompactString),
    /// Fire and forget (artillery, missiles, an ambush): done when fired.
    Fired,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OpStatus {
    Active,
    Succeeded,
    Failed,
    Cancelled,
}

impl OpStatus {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Active => "active",
            Self::Succeeded => "succeeded",
            Self::Failed => "failed",
            Self::Cancelled => "cancelled",
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Op {
    pub(crate) id: u64,
    pub(crate) kind: OpKind,
    pub(crate) target: Option<ObjectiveId>,
    pub(crate) target_name: CompactString,
    pub(crate) pos: Vector2,
    pub(crate) started: DateTime<Utc>,
    pub(crate) cost: i64,
    pub(crate) handle: Handle,
    pub(crate) status: OpStatus,
    pub(crate) detail: CompactString,
    pub(crate) request: Option<u64>,
    pub(crate) ended: Option<DateTime<Utc>>,
    /// The flights flying with it: (group, "escort" | "sead"). Sent home
    /// when it is over.
    pub(crate) support: Vec<(GroupId, &'static str)>,
}

/// An escort waiting for itself and the flight it escorts to be in the air,
/// to be given the DCS escort task.
#[derive(Debug, Clone)]
pub(crate) struct PendingEscort {
    pub(crate) escort: GroupId,
    pub(crate) escorted: GroupId,
    pub(crate) since: DateTime<Utc>,
}

/// An escort that hasn't joined its flight in this long flies its own sweep.
const ESCORT_JOIN_SECS: i64 = 20 * 60;

#[derive(Debug, Default)]
pub(crate) struct SideRt {
    pub(crate) last_think: Option<DateTime<Utc>>,
    pub(crate) plan: Option<strategy::Plan>,
    pub(crate) ops: Vec<Op>,
    pub(crate) requests: Vec<requests::Request>,
    pub(crate) log: VecDeque<(DateTime<Utc>, CompactString)>,
    /// Last time the HQ fired on each target, for the fires cooldown.
    pub(crate) last_fired: FxHashMap<ObjectiveId, DateTime<Utc>>,
    pub(crate) last_request: FxHashMap<Ucid, DateTime<Utc>>,
    /// Tasking-board entries the HQ posted.
    pub(crate) tasks: Vec<TaskId>,
    pub(crate) announced_effort: Option<ObjectiveId>,
    pub(crate) gap: f64,
    pub(crate) humans: u32,
    pub(crate) escorts: Vec<PendingEscort>,
}

impl SideRt {
    pub(crate) fn note(&mut self, now: DateTime<Utc>, text: CompactString) {
        self.log.push_back((now, text));
        while self.log.len() > KEEP_LOG {
            self.log.pop_front();
        }
    }

    pub(crate) fn active(&self) -> impl Iterator<Item = &Op> {
        self.ops.iter().filter(|o| o.status == OpStatus::Active)
    }
}

#[derive(Debug, Default)]
pub(crate) struct Hq {
    pub(crate) sides: FxHashMap<Side, SideRt>,
    pub(crate) seq: u64,
    /// `-hq` / `-request` commands from chat, waiting for the mission state:
    /// (player, is a request, the rest of the line).
    pub(crate) chat: Vec<(dcso3::net::PlayerId, bool, CompactString)>,
    /// Dark over the theatre right now (mission time of day), for picking
    /// aircraft that can fly at night.
    pub(crate) night: bool,
}

impl Hq {
    pub(crate) fn side(&mut self, side: Side) -> &mut SideRt {
        self.sides.entry(side).or_default()
    }

    pub(crate) fn next_id(&mut self) -> u64 {
        self.seq += 1;
        self.seq
    }

    /// The main effort and posture the ground war's AI should follow, when
    /// the HQ steers it.
    pub(crate) fn ground_steer(&self, side: Side) -> Option<(Posture, Option<ObjectiveId>)> {
        let plan = self.sides.get(&side)?.plan.as_ref()?;
        Some((plan.posture, plan.main_effort))
    }
}

// ---------------------------------------------------------------------------
// Tick
// ---------------------------------------------------------------------------

fn humans_on(ctx: &Context, side: Side) -> u32 {
    ctx.connected
        .info_by_player_id
        .values()
        .filter(|ifo| ctx.db.persisted.players.get(&ifo.ucid).map_or(false, |p| p.side == side))
        .count() as u32
}

/// How much of a full HQ to run for `humans` players on the side.
fn gap_factor(cfg: &HqCfg, humans: u32) -> f64 {
    let g = &cfg.gap_fill;
    if humans <= g.full_until_players {
        return 1.;
    }
    let span = g.fade_out_players.saturating_sub(g.full_until_players).max(1) as f64;
    let t = ((humans - g.full_until_players) as f64 / span).min(1.);
    1. - t * (1. - g.min_factor.clamp(0., 1.))
}

/// The slow-tick entry point: track what is under way, expire stale orders
/// and requests, and plan for each side whose turn it is.
pub(crate) fn tick(lua: MizLua, ctx: &mut Context, perf: &mut PerfInner, now: DateTime<Utc>) {
    let Some((sc, cfg)) = cfg(ctx) else {
        return;
    };
    ctx.hq.night = crate::groundwar::daylight_now(lua) < 0.5;
    track(lua, ctx, now);
    let empty = ctx.connected.info_by_player_id.is_empty();
    for side in cfg.sides.iter().copied().filter(|s| *s != Side::Neutral) {
        expire_orders(ctx, &cfg, side, now);
        requests::expire(ctx, &cfg, side, now);
        let humans = humans_on(ctx, side);
        let gap = gap_factor(&cfg, humans);
        {
            let rt = ctx.hq.side(side);
            rt.humans = humans;
            rt.gap = gap;
        }
        let emergency = !ctx.db.objectives_being_captured_by(side).is_empty();
        let every = if emergency { cfg.emergency_think_secs } else { cfg.think_secs }.max(15);
        let due = ctx
            .hq
            .side(side)
            .last_think
            .map_or(true, |t| now - t >= Duration::seconds(every as i64));
        if !due {
            continue;
        }
        ctx.hq.side(side).last_think = Some(now);
        think(lua, ctx, perf, &sc, &cfg, side, empty, now);
    }
}

#[allow(clippy::too_many_arguments)]
fn think(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    sc: &SmartCommanderCfg,
    cfg: &HqCfg,
    side: Side,
    empty: bool,
    now: DateTime<Utc>,
) {
    let pic = picture::build(ctx, cfg, side, now);
    let saved = ctx.db.persisted.hq.side(side).clone();
    let prev = ctx.hq.side(side).plan.clone();
    let plan = strategy::plan(
        cfg,
        &pic,
        prev.as_ref(),
        saved.directive.as_ref().map(|d| &d.directive),
        saved.human.as_ref(),
    );
    announce_effort(ctx, cfg, side, &plan, now);
    ctx.hq.side(side).plan = Some(plan.clone());
    if cfg.post_tasks {
        planner::post_tasks(ctx, cfg, side, &pic, &plan, now);
    }
    if plan.paused {
        return;
    }
    if empty && !cfg.run_when_empty {
        return;
    }
    // A request for something already under way is answered by it.
    let covered: Vec<u64> = ctx
        .hq
        .side(side)
        .requests
        .iter()
        .filter(|r| r.status == requests::RequestStatus::Open)
        .filter(|r| r.kind.answered_by().iter().any(|k| pic.busy(*k, Some(r.target))))
        .map(|r| r.id)
        .collect();
    for rid in covered {
        requests::answered(ctx, side, rid, "already under way");
    }
    let gap = ctx.hq.side(side).gap;
    let ranked = planner::rank(ctx, cfg, side, &pic, &plan, &saved.record, gap, now);
    // What this pass may commit: a share of what is above the reserve, all
    // of it when a base is falling.
    let spendable = ctx.db.persisted.treasury(side) - sc.action_reserve.max(0);
    let emergency = pic.own().any(|o| o.being_captured);
    let share = if emergency { (cfg.max_spend_fraction * 2.).min(1.) } else { cfg.max_spend_fraction };
    let mut budget = (spendable as f64 * share.clamp(0., 1.)) as i64;
    let mut left = cfg.max_new_ops_per_think as usize;
    let mut done: Vec<(Line, ObjectiveId)> = vec![];
    let mut kinds: Vec<OpKind> = vec![];
    let mut skipped = 0usize;
    for cand in ranked.iter() {
        if left == 0 {
            break;
        }
        if cand.cost > budget {
            skipped += 1;
            continue;
        }
        // One operation per target per line per pass, one of each kind per
        // pass, and the standing limits.
        if done.contains(&(cand.kind.line(), cand.anchor)) || kinds.contains(&cand.kind) {
            continue;
        }
        if planner::room(cfg, ctx, side, cand.kind) == 0 {
            continue;
        }
        if dispatch::launch(lua, ctx, perf, cfg, side, cand, now).is_ok() {
            budget -= cand.cost;
            left -= 1;
            done.push((cand.kind.line(), cand.anchor));
            kinds.push(cand.kind);
        }
    }
    // Worth a line when there was money and nothing came of it; a side that
    // is simply broke would say so every pass.
    if left == cfg.max_new_ops_per_think as usize && !ranked.is_empty() && spendable > 0 {
        info!(
            "hq: {side:?} launched nothing this pass: {} candidate(s), {skipped} over the budget of {budget} \
             (treasury {}, reserve {})",
            ranked.len(),
            ctx.db.persisted.treasury(side),
            sc.action_reserve
        );
    }
}

fn announce_effort(ctx: &mut Context, cfg: &HqCfg, side: Side, plan: &strategy::Plan, now: DateTime<Utc>) {
    let rt = ctx.hq.side(side);
    if rt.announced_effort == plan.main_effort {
        return;
    }
    rt.announced_effort = plan.main_effort;
    let Some(oid) = plan.main_effort else { return };
    let name = ctx
        .db
        .persisted
        .objectives
        .get(&oid)
        .map(|o| CompactString::from(o.name()))
        .unwrap_or_default();
    let text = format_compact!("HQ: main effort is now {name} ({}).", plan.posture.label());
    info!("hq: {side:?} {text}");
    ctx.hq.side(side).note(now, text.clone());
    if cfg.announce {
        ctx.db.ephemeral.msgs().panel_to_side(15, false, side, text);
    }
}

fn expire_orders(ctx: &mut Context, cfg: &HqCfg, side: Side, now: DateTime<Utc>) {
    let saved = ctx.db.persisted.hq.side_mut(side);
    let mut changed = false;
    if saved.human.as_ref().map_or(false, |o| now >= o.expires) {
        saved.human = None;
        changed = true;
    }
    if saved.directive.as_ref().map_or(false, |d| now >= d.expires) || !cfg.strategist.enabled {
        changed |= saved.directive.take().is_some();
    }
    if changed {
        ctx.db.ephemeral.dirty();
        ctx.hq.side(side).note(now, "HQ: standing orders expired, back to own judgement".into());
    }
}

// ---------------------------------------------------------------------------
// Tracking
// ---------------------------------------------------------------------------

fn finish(
    lua: MizLua,
    ctx: &mut Context,
    side: Side,
    i: usize,
    status: OpStatus,
    detail: CompactString,
    now: DateTime<Utc>,
) {
    let (kind, request, text, support) = {
        let op = &mut ctx.hq.side(side).ops[i];
        op.status = status;
        op.ended = Some(now);
        op.detail = detail;
        (
            op.kind,
            op.request,
            format_compact!("HQ: {} on {} {}: {}", op.kind.label(), op.target_name, status.label(), op.detail),
            op.support.clone(),
        )
    };
    // The escorts and SEAD were there for it; with it done, they go home.
    for (gid, _) in support {
        if group_gone_or_dead(ctx, &gid).is_none() {
            crate::airlife::send_home(lua, ctx, gid, now);
        }
        ctx.hq.side(side).escorts.retain(|e| e.escort != gid);
    }
    info!("hq: {side:?} {text}");
    ctx.hq.side(side).note(now, text);
    if matches!(status, OpStatus::Succeeded | OpStatus::Failed) {
        let r = ctx.db.persisted.hq.side_mut(side).record.entry(kind).or_default();
        match status {
            OpStatus::Succeeded => r.succeeded += 1,
            _ => r.failed += 1,
        }
        ctx.db.ephemeral.dirty();
    }
    if let Some(rid) = request {
        requests::settle(ctx, side, rid, status, now);
    }
}

fn group_gone_or_dead(ctx: &Context, gid: &GroupId) -> Option<bool> {
    match ctx.db.group_health(gid) {
        Err(_) => Some(false),
        Ok((alive, _)) if alive == 0 => Some(true),
        Ok(_) => None,
    }
}

/// Follow every active operation of every side to its end.
fn track(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let sides: Vec<Side> = ctx.hq.sides.keys().copied().collect();
    for side in sides {
        let n = ctx.hq.side(side).ops.len();
        for i in 0..n {
            let op = ctx.hq.side(side).ops[i].clone();
            if op.status != OpStatus::Active {
                continue;
            }
            match &op.handle {
                Handle::Fired => finish(lua, ctx, side, i, OpStatus::Succeeded, "fired".into(), now),
                Handle::Package { gid, expires } => match group_gone_or_dead(ctx, gid) {
                    Some(_) => {
                        let _ = ctx.db.delete_group(gid);
                        finish(lua, ctx, side, i, OpStatus::Failed, "package lost".into(), now)
                    }
                    None if now >= *expires => {
                        crate::airlife::send_home(lua, ctx, *gid, now);
                        finish(lua, ctx, side, i, OpStatus::Succeeded, "on station to the end, RTB".into(), now)
                    }
                    None => (),
                },
                Handle::Group(gid) => match group_gone_or_dead(ctx, gid) {
                    Some(true) => finish(lua, ctx, side, i, OpStatus::Failed, "destroyed".into(), now),
                    Some(false) => finish(lua, ctx, side, i, OpStatus::Succeeded, "done".into(), now),
                    None => (),
                },
                Handle::Transport(id) => match ctx.db.ephemeral.hq_outcomes.remove(id) {
                    Some(true) => finish(lua, ctx, side, i, OpStatus::Succeeded, "delivered".into(), now),
                    Some(false) => finish(lua, ctx, side, i, OpStatus::Failed, "did not deliver".into(), now),
                    None if (now - op.started).num_seconds() > TRANSPORT_GIVEUP_SECS => {
                        ctx.db.ephemeral.hq_watch.remove(id);
                        finish(lua, ctx, side, i, OpStatus::Failed, "no word from it".into(), now)
                    }
                    None => (),
                },
            }
        }
        join_escorts(lua, ctx, side, now);
        // Keep the record short: every active op, the latest finished ones.
        let rt = ctx.hq.side(side);
        let finished = rt.ops.iter().filter(|o| o.status != OpStatus::Active).count();
        if finished > KEEP_FINISHED {
            let mut drop = finished - KEEP_FINISHED;
            rt.ops.retain(|o| {
                if drop > 0 && o.status != OpStatus::Active {
                    drop -= 1;
                    false
                } else {
                    true
                }
            });
        }
    }
}

/// Give every escort whose flight is now in the air with it the DCS escort
/// task on that flight. Until then (it spawns as a fighter sweep to the
/// target) it covers the target area on its own, which is also what it
/// keeps doing if the two never meet up.
fn join_escorts(lua: MizLua, ctx: &mut Context, side: Side, now: DateTime<Utc>) {
    use dcso3::{
        controller::{AirOption, AirRoe, AiOption, FollowParams, Task},
        group::Group,
        attribute::Attribute,
        LuaVec3, Vector3,
    };
    let engage = ctx
        .db
        .ephemeral
        .cfg
        .smart_commander
        .as_ref()
        .and_then(|s| s.hq.as_ref())
        .map_or(60_000., |h| h.packages.escort_engage_m);
    let pending = std::mem::take(&mut ctx.hq.side(side).escorts);
    let mut keep = vec![];
    for p in pending {
        if (now - p.since).num_seconds() > ESCORT_JOIN_SECS {
            continue;
        }
        let names = (
            ctx.db.persisted.groups.get(&p.escort).map(|g| g.name.clone()),
            ctx.db.persisted.groups.get(&p.escorted).map(|g| g.name.clone()),
        );
        let (Some(escort), Some(escorted)) = names else { continue };
        let joined = (|| -> anyhow::Result<bool> {
            let (Ok(e), Ok(t)) = (Group::get_by_name(lua, escort.as_str()), Group::get_by_name(lua, escorted.as_str()))
            else {
                return Ok(false);
            };
            // Both have to be off the ground: an escort task set on the
            // ramp is thrown away when the takeoff waypoint starts.
            let airborne = |g: &Group| g.get_unit(1).and_then(|u| u.in_air()).unwrap_or(false);
            if !airborne(&e) || !airborne(&t) {
                return Ok(false);
            }
            let task = Task::ComboTask(vec![
                Task::WrappedOption(AiOption::Air(AirOption::Roe(AirRoe::WeaponFree))),
                Task::Escort {
                    engagement_dist_max: engage,
                    target_types: vec![Attribute::Air],
                    params: FollowParams {
                        group: t.id()?,
                        pos: LuaVec3(Vector3::new(-500., 300., 600.)),
                        last_waypoint_index: None,
                    },
                },
            ]);
            e.get_controller()?.set_task(task)?;
            Ok(true)
        })();
        match joined {
            Ok(true) => info!("hq: {side:?} {escort} is escorting {escorted}"),
            Ok(false) => keep.push(p),
            Err(e) => {
                warn!("hq: {side:?} tasking {escort} to escort {escorted}: {e:?}");
                keep.push(p);
            }
        }
    }
    ctx.hq.side(side).escorts.extend(keep);
}

// ---------------------------------------------------------------------------
// Orders from outside: the strategist, human commanders, players
// ---------------------------------------------------------------------------

fn reply(ok: bool, message: impl Into<String>) -> HqReply {
    HqReply { ok, message: message.into() }
}

/// Keep only the parts of a directive that make sense for `side`: objective
/// ids that exist and are on the right side for their field, and weights
/// inside the allowed range.
fn sanitize(ctx: &Context, cfg: &HqCfg, side: Side, d: &mut Directive) {
    let owner = |id: u64| {
        ctx.db
            .persisted
            .objectives
            .get(&ObjectiveId::from(id as i64))
            .map(|o| o.owner())
    };
    if let Some(me) = d.main_effort {
        if owner(me).map_or(true, |o| o == side) {
            warn!("hq: {side:?} directive main effort {me} is not an enemy objective, ignored");
            d.main_effort = None;
        }
    }
    d.defend.retain(|id| owner(*id) == Some(side));
    d.supply_priority.retain(|id| owner(*id) == Some(side));
    d.avoid.retain(|id| owner(*id).is_some());
    d.defend.truncate(6);
    d.supply_priority.truncate(8);
    d.avoid.truncate(12);
    let max = cfg.strategist.max_weight.max(1.);
    d.weights.retain(|_, w| w.is_finite());
    for w in d.weights.values_mut() {
        *w = w.clamp(0., max);
    }
    if let Some(i) = d.intent.as_mut() {
        if i.chars().count() > 280 {
            *i = i.chars().take(280).collect();
        }
    }
    if let Some(r) = d.rationale.as_mut() {
        if r.chars().count() > 1200 {
            *r = r.chars().take(1200).collect();
        }
    }
}

/// A directive from bfdb's strategist (`hq-directive`).
pub(crate) fn directive(ctx: &mut Context, side: Side, mut d: Directive) -> HqReply {
    let now = Utc::now();
    let Some((_, cfg)) = cfg(ctx) else {
        return reply(false, "the HQ is not enabled on this server");
    };
    if !cfg.strategist.enabled {
        return reply(false, "this server's HQ does not take strategist directives");
    }
    if !cfg.sides.contains(&side) {
        return reply(false, format!("the HQ does not command {side:?}"));
    }
    sanitize(ctx, &cfg, side, &mut d);
    let ttl = d
        .ttl_secs
        .unwrap_or(cfg.strategist.max_directive_secs)
        .clamp(300, cfg.strategist.max_directive_secs.max(300));
    let summary = format_compact!(
        "HQ: strategist directive -- posture {}, main effort {}",
        d.posture.map(|p| p.label()).unwrap_or("(HQ's call)"),
        d.main_effort.map(|m| format_compact!("#{m}")).unwrap_or_else(|| "(HQ's call)".into())
    );
    info!("hq: {side:?} {summary}");
    ctx.db.persisted.hq.side_mut(side).directive =
        Some(HeldDirective { directive: d, received: now, expires: now + Duration::seconds(ttl as i64) });
    ctx.db.ephemeral.dirty();
    let rt = ctx.hq.side(side);
    rt.note(now, summary);
    // Re-plan on the next tick rather than waiting out the interval.
    rt.last_think = None;
    reply(true, "directive accepted")
}

/// May `ucid` take command of the HQ? `None` is bfdb on an admin's behalf.
pub(crate) fn may_command(ctx: &Context, cfg: &HqCfg, ucid: Option<&Ucid>) -> bool {
    match ucid {
        None => true,
        Some(u) => {
            ctx.db.ephemeral.cfg.admins.contains_key(u)
                || cfg.override_rule.check(u)
                || ctx
                    .db
                    .player(u)
                    .and_then(|p| crate::command::is_commander(ctx, u, p.side))
                    .unwrap_or(false)
        }
    }
}

/// A command from the dashboard, chat or F10. `ucid` is the player (None =
/// bfdb acting for an admin); `from` is where they are, for a request with
/// no objective named.
pub(crate) fn command(
    lua: MizLua,
    ctx: &mut Context,
    side: Side,
    ucid: Option<Ucid>,
    cmd: HqCommand,
    from: Option<Vector2>,
) -> HqReply {
    let now = Utc::now();
    let Some((_, cfg)) = cfg(ctx) else {
        return reply(false, "the HQ is not enabled on this server");
    };
    if !cfg.sides.contains(&side) {
        return reply(false, format!("the HQ does not command {side:?}"));
    }
    let name: CompactString = match ucid.as_ref() {
        None => "server admin".into(),
        Some(u) => match ctx.db.player(u) {
            Some(p) if p.side == side => p.name.as_str().into(),
            Some(_) => return reply(false, "you are not on that side"),
            None => return reply(false, "unknown player"),
        },
    };
    match cmd {
        HqCommand::Request { request, objective } => {
            let Some(u) = ucid else {
                return reply(false, "a support request needs a player");
            };
            let objective = objective.map(|o| ObjectiveId::from(o as i64));
            match requests::submit(ctx, &cfg, side, u, request, objective, from, now) {
                Ok(m) => reply(true, m.as_str()),
                Err(m) => reply(false, m.as_str()),
            }
        }
        HqCommand::CancelRequest { request_id } => {
            let admin = may_command(ctx, &cfg, ucid.as_ref());
            match requests::cancel(ctx, side, ucid.as_ref(), admin, request_id, now) {
                Ok(m) => reply(true, m.as_str()),
                Err(m) => reply(false, m.as_str()),
            }
        }
        _ if !may_command(ctx, &cfg, ucid.as_ref()) => {
            reply(false, "you are not cleared to command this side's HQ")
        }
        HqCommand::Override { mut directive, disabled_ops, paused } => {
            sanitize(ctx, &cfg, side, &mut directive);
            let max = cfg.override_max_secs.max(300);
            let ttl = directive.ttl_secs.unwrap_or(max).clamp(300, max);
            let o = Override {
                by: name.clone(),
                set: now,
                expires: now + Duration::seconds(ttl as i64),
                directive,
                disabled_ops,
                paused,
            };
            let text = format_compact!(
                "HQ: {name} has taken command{}{}",
                o.directive.posture.map(|p| format_compact!(", posture {}", p.label())).unwrap_or_default(),
                if o.paused { ", HQ planning paused" } else { "" }
            );
            ctx.db.persisted.hq.side_mut(side).human = Some(o);
            ctx.db.ephemeral.dirty();
            let rt = ctx.hq.side(side);
            rt.note(now, text.clone());
            rt.last_think = None;
            info!("hq: {side:?} {text}");
            ctx.db.ephemeral.msgs().panel_to_side(15, false, side, text);
            reply(true, "orders set")
        }
        HqCommand::ClearOverride => {
            if ctx.db.persisted.hq.side_mut(side).human.take().is_none() {
                return reply(false, "there are no human orders standing");
            }
            ctx.db.ephemeral.dirty();
            let text = format_compact!("HQ: {name} has handed command back to the HQ");
            let rt = ctx.hq.side(side);
            rt.note(now, text.clone());
            rt.last_think = None;
            ctx.db.ephemeral.msgs().panel_to_side(15, false, side, text);
            reply(true, "command handed back")
        }
        HqCommand::CancelOp { op } => {
            let Some(i) = ctx
                .hq
                .side(side)
                .ops
                .iter()
                .position(|o| o.id == op && o.status == OpStatus::Active)
            else {
                return reply(false, format!("no active operation {op}"));
            };
            match ctx.hq.side(side).ops[i].handle.clone() {
                Handle::Package { gid, .. } => crate::airlife::send_home(lua, ctx, gid, now),
                Handle::Group(gid) => {
                    let _ = ctx.db.delete_group(&gid);
                }
                // Transports and fires can't be called back; the HQ just
                // stops following them.
                Handle::Transport(id) => {
                    ctx.db.ephemeral.hq_watch.remove(&id);
                }
                Handle::Fired => (),
            }
            finish(lua, ctx, side, i, OpStatus::Cancelled, format_compact!("called off by {name}"), now);
            reply(true, "operation called off")
        }
    }
}

/// The objective nearest `pos` that `pred` accepts.
pub(crate) fn nearest_objective(
    ctx: &Context,
    pos: Vector2,
    pred: impl Fn(&crate::db::objective::Objective) -> bool,
) -> Option<ObjectiveId> {
    ctx.db
        .objectives()
        .filter(|(_, o)| pred(o))
        .min_by(|a, b| dist(a.1.pos(), pos).total_cmp(&dist(b.1.pos(), pos)))
        .map(|(id, _)| *id)
}

/// Line weights a posture starts from, before directives.
pub(crate) fn posture_weights(p: Posture) -> BTreeMap<Line, f64> {
    let w = match p {
        Posture::Offensive => [1.2, 1.2, 0.9, 1.4, 1.2],
        Posture::Balanced => [1., 1., 1., 1., 1.],
        Posture::Defensive => [1.1, 0.9, 1.4, 0.8, 1.3],
    };
    Line::ALL.iter().copied().zip(w).collect()
}

/// Shared by the menu and chat: a player asking for support.
pub(crate) fn player_request(
    lua: MizLua,
    ctx: &mut Context,
    ucid: Ucid,
    kind: RequestKind,
    objective: Option<ObjectiveId>,
    from: Option<Vector2>,
) -> HqReply {
    let Some(side) = ctx.db.player(&ucid).map(|p| p.side) else {
        return reply(false, "unknown player");
    };
    command(
        lua,
        ctx,
        side,
        Some(ucid),
        HqCommand::Request { request: kind, objective: objective.map(|o| o.inner() as u64) },
        from,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_hq_steps_back_as_players_arrive() {
        let cfg = HqCfg::default();
        assert_eq!(gap_factor(&cfg, 0), 1.);
        assert_eq!(gap_factor(&cfg, 2), 1.);
        let mid = gap_factor(&cfg, 9);
        assert!(mid < 1. && mid > cfg.gap_fill.min_factor);
        assert!((gap_factor(&cfg, 16) - cfg.gap_fill.min_factor).abs() < 1e-9);
        assert!((gap_factor(&cfg, 60) - cfg.gap_fill.min_factor).abs() < 1e-9);
    }

    #[test]
    fn trust_moves_with_the_record_but_not_much_at_first() {
        let fresh = Record::default().trust();
        assert!((fresh - 1.).abs() < 1e-9);
        let bad = Record { launched: 3, succeeded: 0, failed: 3 }.trust();
        let good = Record { launched: 3, succeeded: 3, failed: 0 }.trust();
        assert!(bad < 1. && bad > 0.75);
        assert!(good > 1. && good < 1.25);
        let awful = Record { launched: 40, succeeded: 0, failed: 40 }.trust();
        assert!(awful < 0.6);
    }
}
