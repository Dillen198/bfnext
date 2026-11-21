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

//! Story events (`Cfg::story`, Red Storm Rising ENGINE.md 3.4): events the
//! campaign writes, each set off by something players or the AI do, that
//! tell everyone what happened, credit who caused it and change the war.
//!
//! The engine feeds facts in where it already feeds war will (`db::will`):
//!
//! - **deaths** (`Db::unit_dead`, `Db::static_dead`), with the killers
//!   attached from the shot record when the slow tick reports kills
//!   (`Db::story_kills`);
//! - **captures** (`Db::check_capture`, carrier boarding);
//! - **supply routes** lost (convoys in the kill report, supply flights and
//!   ships in logistics);
//! - **objective health** and ownership, **fuel** and **will**, read from
//!   the campaign state on the slow tick.
//!
//! `Story::step` is the pure state machine (tested below without a
//! mission): it turns the facts and the state into firings, once per round
//! unless an event repeats, honouring `requires` and `excludes`. A state
//! trigger (fuel, will) has to be seen on the safe side of its line before
//! it can fire, so nothing fires because of how a round starts: the story
//! only moves when someone moves it. The `Db` methods then apply each
//! firing's effects through the engine's own mechanisms (will, production,
//! warehouse stock, the action list, points, the HQ, intel, the round end),
//! tell both sides and publish `Stat::StoryEvent`.
//!
//! Everything that matters is in `Persisted::story`: fired events (so a
//! restart never refires one), facts not yet evaluated, the windows the
//! counting triggers read, and the effects that last (production, action
//! overrides, the round end).

use super::{Db, intel::IntelSource, logistics::Inventory};
use crate::{hq::HeldDirective, msgq::MsgTyp};
use bfprotocols::{
    cfg::{Action, Cfg, StoryCfg, StoryEffect, StoryEventCfg, StoryTrigger, UnitTags},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    hq::Directive,
    shots::{Dead, Shot, Who},
    stats::{Stat, StoryEventStat},
};
use chrono::{DateTime, Duration, Utc};
use compact_str::format_compact;
use dcso3::{
    String, Vector2, coalition::Side, net::Ucid, object::DcsOid,
    static_object::ClassStatic, unit::ClassUnit,
};
use fxhash::{FxBuildHasher, FxHashMap};
use indexmap::IndexMap;
use log::{info, warn};
use serde_derive::{Deserialize, Serialize};
use std::sync::Arc;

/// The most people credited with one event.
const MAX_CAUSES: usize = 6;
/// How far back the attackers of an objective are looked for.
const ATTACK_MEMORY: Duration = Duration::minutes(60);
/// How far back the people behind a stock or will crossing are looked for.
const STATE_BLAME: Duration = Duration::hours(3);
/// How many rounds of chained events one tick follows.
const MAX_CHAIN: usize = 8;
/// The most kill records kept for the windows.
const MAX_KILLS: usize = 4000;

/// `format!` to the engine's string type.
macro_rules! fstr {
    ($($t:tt)*) => {
        String::from(format_compact!($($t)*))
    };
}

type Actions = FxHashMap<Side, IndexMap<String, Action, FxBuildHasher>>;

/// Someone behind an action: a pilot (with their ucid, for points) or the
/// AI ("AI Su-27").
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Cause {
    pub name: String,
    #[serde(default)]
    pub ucid: Option<Ucid>,
}

impl Cause {
    fn ai(what: &str) -> Self {
        Self { name: fstr!("AI {what}"), ucid: None }
    }
}

/// A unit or building destroyed.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KillRec {
    pub time: DateTime<Utc>,
    /// The victim's side.
    pub side: Side,
    pub typ: String,
    #[serde(default)]
    pub tags: UnitTags,
    /// Where it died, when known (AI units and buildings).
    #[serde(default)]
    pub pos: Option<Vector2>,
    /// The objective it belonged to.
    #[serde(default)]
    pub objective: Option<ObjectiveId>,
    /// Who killed it, the killing blow first.
    #[serde(default)]
    pub by: Vec<Cause>,
}

/// A convoy, supply flight or supply ship lost.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteRec {
    pub time: DateTime<Utc>,
    pub side: Side,
    /// One per convoy or route, so a convoy of six trucks counts once.
    pub key: String,
    #[serde(default)]
    pub by: Vec<Cause>,
}

/// An objective a side has captured and still holds.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HeldRec {
    pub oid: ObjectiveId,
    pub side: Side,
    pub since: DateTime<Utc>,
    #[serde(default)]
    pub by: Vec<Cause>,
}

/// Something that happened, waiting for the tick to evaluate it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Fact {
    Capture {
        oid: ObjectiveId,
        from: Side,
        to: Side,
        by: Vec<Cause>,
        time: DateTime<Utc>,
    },
    Kill(KillRec),
    Route(RouteRec),
}

impl Fact {
    fn time(&self) -> DateTime<Utc> {
        match self {
            Self::Capture { time, .. } => *time,
            Self::Kill(k) => k.time,
            Self::Route(r) => r.time,
        }
    }
}

/// An event that fired this round.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FiredEvent {
    pub id: String,
    pub time: DateTime<Utc>,
    pub title: String,
    /// What players were told, placeholders filled in.
    #[serde(default)]
    pub text: String,
    #[serde(default)]
    pub caused_by: Vec<String>,
    #[serde(default)]
    pub side: Option<Side>,
    /// What it changed, one line each.
    #[serde(default)]
    pub effects: Vec<String>,
}

/// A side's production, as a multiple of what it would be.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ProdMult {
    pub fuel: f64,
    pub equipment: f64,
}

impl Default for ProdMult {
    fn default() -> Self {
        Self { fuel: 1., equipment: 1. }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ActionChange {
    Lock,
    Unlock,
    Cost(u32),
}

/// A change a story event made to a side's action list, in the order made.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ActionOverride {
    pub side: Side,
    pub action: String,
    pub change: ActionChange,
}

/// Not saved: what the engine needs between ticks.
#[derive(Debug, Clone, Default)]
struct StoryRt {
    /// Deaths seen, waiting for the kill report to name the killers.
    deaths: Vec<(DcsOid<ClassUnit>, KillRec)>,
    /// Who last hit each static that died, from the hit event.
    static_killers: FxHashMap<DcsOid<ClassStatic>, Cause>,
    /// The players who hit each convoy, until it is lost.
    convoy_killers: FxHashMap<GroupId, Vec<Cause>>,
    /// The config's actions as loaded, before any story change.
    base_actions: Option<Actions>,
    /// The action list has been brought in line with the story this run.
    synced: bool,
    /// The objective names have been checked against the mission.
    checked: bool,
}

/// The story so far (`Persisted::story`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Story {
    /// Every event fired this round, oldest first.
    #[serde(default)]
    pub fired: Vec<FiredEvent>,
    /// Facts not yet evaluated.
    #[serde(default)]
    pub pending: Vec<Fact>,
    /// Recent kills, for the counting triggers and for credit.
    #[serde(default)]
    pub kills: Vec<KillRec>,
    /// Recent supply routes lost.
    #[serde(default)]
    pub routes: Vec<RouteRec>,
    /// Objectives captured and still held, for `ObjectiveHeld`.
    #[serde(default)]
    pub held: Vec<HeldRec>,
    /// Each objective's owner and health as last seen, so a change is read
    /// against the same owner's previous health.
    #[serde(default)]
    pub seen: Vec<(ObjectiveId, Side, u8)>,
    /// State-triggered events seen on the safe side of their line since
    /// they last fired: only these may fire.
    #[serde(default)]
    pub armed: Vec<String>,
    #[serde(default)]
    pub blue_production: ProdMult,
    #[serde(default)]
    pub red_production: ProdMult,
    #[serde(default)]
    pub actions: Vec<ActionOverride>,
    /// An event ended the round with this winner.
    #[serde(default)]
    pub ending: Option<Side>,
    #[serde(skip)]
    rt: StoryRt,
}

/// One objective as the story sees it.
#[derive(Debug, Clone)]
pub struct ObjState {
    pub id: ObjectiveId,
    pub name: String,
    pub owner: Side,
    pub health: u8,
    pub pos: Vector2,
    pub radius: f64,
    /// A logistics hub or factory: the side's supply.
    pub supply: bool,
}

/// The campaign state a tick reads.
#[derive(Debug, Clone, Default)]
pub struct World {
    pub objectives: Vec<ObjState>,
    /// Each side's fuel stock in percent, Blue then Red.
    pub fuel: [Option<f64>; 2],
    /// Each side's will, Blue then Red.
    pub will: [Option<f64>; 2],
    /// Each side's airframes by type (lower case) over its bases, Blue then
    /// Red; only filled when a `StockBelow` trigger needs it.
    pub airframes: super::air_stock::StockTotals,
}

impl World {
    fn by_name(&self, name: &str) -> Option<&ObjState> {
        self.objectives.iter().find(|o| o.name.as_str() == name)
    }

    fn by_id(&self, id: ObjectiveId) -> Option<&ObjState> {
        self.objectives.iter().find(|o| o.id == id)
    }
}

/// An event the tick fired, for the `Db` to apply.
#[derive(Debug, Clone)]
pub struct Firing {
    /// Index into `StoryCfg::events`.
    pub idx: usize,
    /// Index into `Story::fired`.
    pub fired: usize,
    pub causes: Vec<Cause>,
    pub objective: Option<String>,
}

fn side_idx(side: Side) -> Option<usize> {
    match side {
        Side::Blue => Some(0),
        Side::Red => Some(1),
        Side::Neutral => None,
    }
}

/// "A", "A and B", "A, B and C".
fn join_names(names: &[String]) -> String {
    match names {
        [] => String::from(""),
        [a] => a.clone(),
        _ => {
            let (last, rest) = names.split_last().unwrap();
            let rest: Vec<&str> = rest.iter().map(|s| s.as_str()).collect();
            fstr!("{} and {last}", rest.join(", "))
        }
    }
}

/// Fill an event's text: `{by}`, `{objective}`, `{side}`, `{enemy}`.
pub fn render(
    text: &str,
    by: &[Cause],
    objective: Option<&str>,
    side: Option<Side>,
) -> String {
    let names: Vec<String> = by.iter().map(|c| c.name.clone()).collect();
    let by = if names.is_empty() { "unknown forces".into() } else { join_names(&names) };
    let side_name = |s: Option<Side>| s.map(|s| format!("{s:?}")).unwrap_or_default();
    String::from(
        text.replace("{by}", &by)
            .replace("{objective}", objective.unwrap_or(""))
            .replace("{side}", &side_name(side))
            .replace("{enemy}", &side_name(side.map(|s| s.opposite())))
            .trim(),
    )
}

/// The people behind `recs`: most often first, then the most recent.
fn top_causes<'a>(
    recs: impl Iterator<Item = (DateTime<Utc>, &'a [Cause])>,
) -> Vec<Cause> {
    let mut tally: Vec<(Cause, u32, DateTime<Utc>)> = vec![];
    for (t, by) in recs {
        for c in by {
            match tally.iter_mut().find(|(o, _, _)| o.name == c.name) {
                Some((_, n, last)) => {
                    *n += 1;
                    *last = (*last).max(t);
                }
                None => tally.push((c.clone(), 1, t)),
            }
        }
    }
    tally.sort_by(|a, b| b.1.cmp(&a.1).then(b.2.cmp(&a.2)));
    tally.into_iter().take(MAX_CAUSES).map(|(c, _, _)| c).collect()
}

fn kill_matches(t: &StoryTrigger, k: &KillRec, world: &World) -> bool {
    let StoryTrigger::Kills { types, tags, side, objective, radius_m, .. } = t else {
        return false;
    };
    if side.is_some_and(|s| s != k.side) {
        return false;
    }
    let any = types.is_empty() && tags.is_empty();
    let by_type = types.iter().any(|ty| ty.eq_ignore_ascii_case(k.typ.as_str()));
    let by_tag = !tags.is_empty() && k.tags.intersects(tags.0);
    if !(any || by_type || by_tag) {
        return false;
    }
    match objective {
        None => true,
        Some(name) => {
            let (Some(o), Some(p)) = (world.by_name(name), k.pos) else { return false };
            let r = radius_m.filter(|r| r.is_finite() && *r > 0.).unwrap_or(o.radius);
            (o.pos - p).norm() <= r
        }
    }
}

impl Story {
    fn last_fired(&self, id: &str) -> Option<DateTime<Utc>> {
        self.fired.iter().rev().find(|f| f.id.as_str() == id).map(|f| f.time)
    }

    fn has_fired(&self, id: &str) -> bool {
        self.fired.iter().any(|f| f.id.as_str() == id)
    }

    /// Whether `ev` may fire now: what it requires has fired, nothing it
    /// excludes has, and it has not fired this round (or its cooldown is
    /// over).
    fn eligible(&self, ev: &StoryEventCfg, now: DateTime<Utc>) -> bool {
        if !ev.requires.iter().all(|r| self.has_fired(r)) {
            return false;
        }
        if ev.excludes.iter().any(|x| self.has_fired(x)) {
            return false;
        }
        match (self.last_fired(&ev.id), ev.repeat) {
            (None, _) => true,
            (Some(_), None) => false,
            (Some(t), Some(cd)) => now - t >= Duration::seconds(cd.max(1) as i64),
        }
    }

    fn fire(
        &mut self,
        cfg: &StoryCfg,
        idx: usize,
        causes: Vec<Cause>,
        objective: Option<&str>,
        now: DateTime<Utc>,
        out: &mut Vec<Firing>,
    ) {
        let ev = &cfg.events[idx];
        self.fired.push(FiredEvent {
            id: ev.id.as_str().into(),
            time: now,
            title: ev.title.as_str().into(),
            text: render(&ev.text, &causes, objective, ev.side),
            caused_by: causes.iter().map(|c| c.name.clone()).collect(),
            side: ev.side,
            effects: vec![],
        });
        out.push(Firing {
            idx,
            fired: self.fired.len() - 1,
            causes,
            objective: objective.map(String::from),
        });
    }

    /// Who attacked objective `oid` lately, the most recent first.
    fn attackers(&self, oid: ObjectiveId, now: DateTime<Utc>) -> Vec<Cause> {
        let mut out: Vec<Cause> = vec![];
        for k in self.kills.iter().rev() {
            if k.objective != Some(oid) || now - k.time > ATTACK_MEMORY {
                continue;
            }
            for c in &k.by {
                if !out.iter().any(|o| o.name == c.name) {
                    out.push(c.clone());
                }
            }
        }
        out.truncate(MAX_CAUSES);
        out
    }

    /// Who has been killing `side`'s units lately; with `supply`, only its
    /// supply (hubs, factories, convoys) when anyone has.
    fn blame(
        &self,
        side: Side,
        supply: bool,
        world: &World,
        now: DateTime<Utc>,
    ) -> Vec<Cause> {
        let recent = |t: DateTime<Utc>| now - t <= STATE_BLAME;
        if supply {
            let at_supply = |k: &KillRec| {
                k.objective.and_then(|o| world.by_id(o)).is_some_and(|o| o.supply)
            };
            let kills = self
                .kills
                .iter()
                .filter(|k| k.side == side && recent(k.time) && at_supply(k))
                .map(|k| (k.time, k.by.as_slice()));
            let routes = self
                .routes
                .iter()
                .filter(|r| r.side == side && recent(r.time))
                .map(|r| (r.time, r.by.as_slice()));
            let c = top_causes(kills.chain(routes));
            if !c.is_empty() {
                return c;
            }
        }
        top_causes(
            self.kills
                .iter()
                .filter(|k| k.side == side && recent(k.time))
                .map(|k| (k.time, k.by.as_slice())),
        )
    }

    /// A state trigger: fires when `cond` holds, if it was seen not holding
    /// since it last fired.
    #[allow(clippy::too_many_arguments)]
    fn level(
        &mut self,
        cfg: &StoryCfg,
        idx: usize,
        cond: Option<bool>,
        causes: impl FnOnce(&Self) -> Vec<Cause>,
        now: DateTime<Utc>,
        out: &mut Vec<Firing>,
    ) {
        let id = cfg.events[idx].id.as_str();
        let armed = self.armed.iter().any(|a| a.as_str() == id);
        match cond {
            Some(false) if !armed => self.armed.push(id.into()),
            Some(true) if armed && self.eligible(&cfg.events[idx], now) => {
                self.armed.retain(|a| a.as_str() != id);
                let c = causes(self);
                self.fire(cfg, idx, c, None, now, out);
            }
            _ => (),
        }
    }

    fn state_triggers(
        &mut self,
        cfg: &StoryCfg,
        world: &World,
        now: DateTime<Utc>,
        out: &mut Vec<Firing>,
    ) {
        for (i, ev) in cfg.events.iter().enumerate() {
            match &ev.trigger {
                StoryTrigger::FuelBelow { side, pct } => {
                    let v = side_idx(*side).and_then(|i| world.fuel[i]);
                    let side = *side;
                    self.level(
                        cfg,
                        i,
                        v.map(|v| v < *pct),
                        |s| s.blame(side, true, world, now),
                        now,
                        out,
                    )
                }
                StoryTrigger::WillBelow { side, value } => {
                    let v = side_idx(*side).and_then(|i| world.will[i]);
                    let side = *side;
                    self.level(
                        cfg,
                        i,
                        v.map(|v| v < *value),
                        |s| s.blame(side, false, world, now),
                        now,
                        out,
                    )
                }
                StoryTrigger::ObjectivesHeldBelow { side, count } => {
                    let held =
                        world.objectives.iter().filter(|o| o.owner == *side).count();
                    let side = *side;
                    self.level(
                        cfg,
                        i,
                        Some(held < *count as usize),
                        |s| s.blame(side, false, world, now),
                        now,
                        out,
                    )
                }
                StoryTrigger::WillAbove { side, value } => {
                    let v = side_idx(*side).and_then(|i| world.will[i]);
                    // its successes: the enemy's losses
                    let enemy = side.opposite();
                    self.level(
                        cfg,
                        i,
                        v.map(|v| v > *value),
                        |s| s.blame(enemy, false, world, now),
                        now,
                        out,
                    )
                }
                StoryTrigger::StockBelow { side, types, below } => {
                    // An unlimited base holding the type never runs low.
                    let v = side_idx(*side)
                        .map(|i| super::air_stock::stock_of(&world.airframes[i], types))
                        .map(|n| n.is_some_and(|n| n < *below as u64));
                    let side = *side;
                    self.level(cfg, i, v, |s| s.blame(side, false, world, now), now, out)
                }
                StoryTrigger::ObjectiveHeld { objective, by, minutes } => {
                    let Some(o) = world.by_name(objective) else { continue };
                    if o.owner != *by {
                        continue;
                    }
                    let Some(h) =
                        self.held.iter().find(|h| h.oid == o.id && h.side == *by)
                    else {
                        continue;
                    };
                    // a repeating hold counts again from its last firing
                    let start = match self.last_fired(&ev.id) {
                        Some(t) if t > h.since => t,
                        _ => h.since,
                    };
                    if now - start >= Duration::minutes(*minutes as i64)
                        && self.eligible(ev, now)
                    {
                        let by = h.by.clone();
                        let name = o.name.clone();
                        self.fire(cfg, i, by, Some(&name), now, out)
                    }
                }
                _ => (),
            }
        }
    }

    fn on_fact(
        &mut self,
        cfg: &StoryCfg,
        world: &World,
        fact: Fact,
        now: DateTime<Utc>,
        out: &mut Vec<Firing>,
    ) {
        match fact {
            Fact::Capture { oid, from, to, by, time } => {
                self.held.retain(|h| h.oid != oid);
                if to != Side::Neutral {
                    self.held.push(HeldRec {
                        oid,
                        side: to,
                        since: time,
                        by: by.clone(),
                    });
                }
                if from == to {
                    return;
                }
                let Some(name) = world.by_id(oid).map(|o| o.name.clone()) else { return };
                for (i, ev) in cfg.events.iter().enumerate() {
                    if let StoryTrigger::ObjectiveCaptured { objective, by: side } =
                        &ev.trigger
                        && objective.as_str() == name.as_str()
                        && side.is_none_or(|s| s == to)
                        && self.eligible(ev, now)
                    {
                        self.fire(cfg, i, by.clone(), Some(&name), now, out)
                    }
                }
            }
            Fact::Kill(k) => {
                self.kills.push(k.clone());
                for (i, ev) in cfg.events.iter().enumerate() {
                    let StoryTrigger::Kills { count, .. } = &ev.trigger else { continue };
                    if !kill_matches(&ev.trigger, &k, world) || !self.eligible(ev, now) {
                        continue;
                    }
                    let window = Duration::minutes(
                        ev.trigger.window_minutes().unwrap_or(60) as i64,
                    );
                    let after = self.last_fired(&ev.id);
                    let hits: Vec<&KillRec> = self
                        .kills
                        .iter()
                        .filter(|r| {
                            r.time <= k.time
                                && k.time - r.time <= window
                                && after.is_none_or(|t| r.time > t)
                                && kill_matches(&ev.trigger, r, world)
                        })
                        .collect();
                    if hits.len() as u32 >= (*count).max(1) {
                        let causes =
                            top_causes(hits.iter().map(|r| (r.time, r.by.as_slice())));
                        let objective = ev.trigger.objective().map(String::from);
                        self.fire(
                            cfg,
                            i,
                            causes,
                            objective.as_ref().map(|s| s.as_str()),
                            now,
                            out,
                        )
                    }
                }
            }
            Fact::Route(r) => {
                // one convoy is one route, however many trucks it loses
                if self.routes.iter().any(|o| o.key == r.key) {
                    return;
                }
                self.routes.push(r.clone());
                for (i, ev) in cfg.events.iter().enumerate() {
                    let StoryTrigger::SupplyRoutesDestroyed { count, side, .. } =
                        &ev.trigger
                    else {
                        continue;
                    };
                    if side.is_some_and(|s| s != r.side) || !self.eligible(ev, now) {
                        continue;
                    }
                    let window = Duration::minutes(
                        ev.trigger.window_minutes().unwrap_or(60) as i64,
                    );
                    let after = self.last_fired(&ev.id);
                    let hits: Vec<&RouteRec> = self
                        .routes
                        .iter()
                        .filter(|o| {
                            o.time <= r.time
                                && r.time - o.time <= window
                                && after.is_none_or(|t| o.time > t)
                                && side.is_none_or(|s| s == o.side)
                        })
                        .collect();
                    if hits.len() as u32 >= (*count).max(1) {
                        let causes =
                            top_causes(hits.iter().map(|o| (o.time, o.by.as_slice())));
                        self.fire(cfg, i, causes, None, now, out)
                    }
                }
            }
        }
    }

    /// Objectives destroyed or fallen below a line since the last look.
    fn transitions(
        &mut self,
        cfg: &StoryCfg,
        world: &World,
        now: DateTime<Utc>,
        out: &mut Vec<Firing>,
    ) {
        for o in &world.objectives {
            let prev = match self.seen.iter_mut().find(|s| s.0 == o.id) {
                Some(s) => {
                    let p = (s.1, s.2);
                    (s.1, s.2) = (o.owner, o.health);
                    Some(p)
                }
                None => {
                    // the first look only records
                    self.seen.push((o.id, o.owner, o.health));
                    None
                }
            };
            let Some((po, ph)) = prev else { continue };
            if (po, ph) == (o.owner, o.health) || po == Side::Neutral {
                continue;
            }
            let mut attackers = None;
            for (i, ev) in cfg.events.iter().enumerate() {
                let hit = match &ev.trigger {
                    StoryTrigger::ObjectiveDestroyed { objective, owner } => {
                        objective.as_str() == o.name.as_str()
                            && ph > 0
                            && o.health == 0
                            && (o.owner == po || o.owner == Side::Neutral)
                            && owner.is_none_or(|s| s == po)
                    }
                    StoryTrigger::ObjectiveHealthBelow { objective, pct, owner } => {
                        objective.as_str() == o.name.as_str()
                            && o.owner == po
                            && ph >= *pct
                            && o.health < *pct
                            && owner.is_none_or(|s| s == po)
                    }
                    _ => false,
                };
                if hit && self.eligible(ev, now) {
                    let by = attackers
                        .get_or_insert_with(|| self.attackers(o.id, now))
                        .clone();
                    self.fire(cfg, i, by, Some(&o.name), now, out)
                }
            }
        }
    }

    /// One tick: evaluate the pending facts and the state, fire what is due.
    /// Each firing is recorded in `fired` (with no effects yet) and returned.
    pub fn step(
        &mut self,
        cfg: &StoryCfg,
        world: &World,
        now: DateTime<Utc>,
    ) -> Vec<Firing> {
        let mut out = vec![];
        let mut facts = std::mem::take(&mut self.pending);
        facts.sort_by_key(|f| f.time());
        for fact in facts {
            self.on_fact(cfg, world, fact, now, &mut out);
        }
        self.transitions(cfg, world, now, &mut out);
        // State triggers and chains, until nothing more fires.
        let mut from = 0;
        for _ in 0..MAX_CHAIN {
            let before = out.len();
            let newly: Vec<(std::string::String, Vec<Cause>)> = out[from..]
                .iter()
                .map(|f| (cfg.events[f.idx].id.clone(), f.causes.clone()))
                .collect();
            from = out.len();
            for (id, causes) in &newly {
                for (i, ev) in cfg.events.iter().enumerate() {
                    if let StoryTrigger::EventFired { id: w } = &ev.trigger
                        && w == id
                        && self.eligible(ev, now)
                    {
                        self.fire(cfg, i, causes.clone(), None, now, &mut out)
                    }
                }
            }
            self.state_triggers(cfg, world, now, &mut out);
            if out.len() == before {
                break;
            }
        }
        // Keep what the windows and the credit still need.
        let keep = Duration::minutes(cfg.max_window_minutes() as i64).max(STATE_BLAME);
        self.kills.retain(|k| now - k.time <= keep);
        if self.kills.len() > MAX_KILLS {
            let extra = self.kills.len() - MAX_KILLS;
            self.kills.drain(..extra);
        }
        self.routes.retain(|r| now - r.time <= keep);
        self.held.retain(|h| world.by_id(h.oid).is_some_and(|o| o.owner == h.side));
        self.seen.retain(|s| world.by_id(s.0).is_some());
        out
    }

    /// The production multiples of `side`.
    pub fn production(&self, side: Side) -> ProdMult {
        match side {
            Side::Blue => self.blue_production,
            Side::Red => self.red_production,
            Side::Neutral => ProdMult::default(),
        }
    }

    fn production_mut(&mut self, side: Side) -> Option<&mut ProdMult> {
        match side {
            Side::Blue => Some(&mut self.blue_production),
            Side::Red => Some(&mut self.red_production),
            Side::Neutral => None,
        }
    }

    /// The action list `base` comes to under `locked` (the config's
    /// `locked_actions`) and the overrides made so far, in the base order.
    fn effective_actions(&self, base: &Actions, locked: Option<&StoryCfg>) -> Actions {
        let mut out = Actions::default();
        for (side, actions) in base {
            let mut v: IndexMap<String, Action, FxBuildHasher> = IndexMap::default();
            for (name, a) in actions {
                let mut off = locked.is_some_and(|s| {
                    s.locked_actions
                        .get(side)
                        .is_some_and(|l| l.iter().any(|n| n.as_str() == name.as_str()))
                });
                let mut cost = None;
                for o in
                    self.actions.iter().filter(|o| o.side == *side && o.action == *name)
                {
                    match o.change {
                        ActionChange::Lock => off = true,
                        ActionChange::Unlock => off = false,
                        ActionChange::Cost(c) => cost = Some(c),
                    }
                }
                if !off {
                    let mut a = a.clone();
                    if let Some(c) = cost {
                        a.cost = c;
                    }
                    v.insert(name.clone(), a);
                }
            }
            out.insert(*side, v);
        }
        out
    }
}

fn fmt_delta(d: f64) -> String {
    if (d - d.round()).abs() < 0.05 { fstr!("{d:+.0}") } else { fstr!("{d:+.1}") }
}

impl Db {
    /// The config, when `story` is on.
    fn story_cfg(&self) -> Option<Arc<Cfg>> {
        let cfg = self.ephemeral.cfg.clone();
        cfg.story.as_ref().is_some_and(|s| s.enabled).then_some(cfg)
    }

    fn player_cause(&self, ucid: &Ucid) -> Cause {
        let name = self
            .persisted
            .players
            .get(ucid)
            .map(|p| p.name.clone())
            .unwrap_or_else(|| "unknown pilot".into());
        Cause { name, ucid: Some(*ucid) }
    }

    /// The person or AI behind `who`, a unit of type `typ` if known.
    fn who_cause(&self, who: &Who, typ: Option<&str>) -> Cause {
        match who {
            Who::Player { ucid, .. } => self.player_cause(ucid),
            // an AI group a player put on the map is theirs
            Who::AI { ucid: Some(u), .. } if *u != Ucid::default() => {
                self.player_cause(u)
            }
            Who::AI { uid, .. } => {
                let unit = self.persisted.units.get(uid).map(|u| u.typ.0.clone());
                Cause::ai(typ.or(unit.as_ref().map(|s| s.as_str())).unwrap_or("unit"))
            }
        }
    }

    /// The unit `id` died. Called before the death is processed, while the
    /// engine still knows what and where it was; the killers come with the
    /// kill report (`story_kills`).
    pub(super) fn story_unit_lost(&mut self, id: &DcsOid<ClassUnit>, now: DateTime<Utc>) {
        if self.story_cfg().is_none() {
            return;
        }
        let rec = if let Some(slot) = self.ephemeral.get_slot_by_object_id(id) {
            self.ephemeral.slot_info.get(slot).map(|ifo| KillRec {
                time: now,
                side: ifo.side,
                typ: ifo.typ.0.clone(),
                tags: self
                    .ephemeral
                    .cfg
                    .unit_classification
                    .get(&ifo.typ)
                    .copied()
                    .unwrap_or_default(),
                pos: None,
                objective: None,
                by: vec![],
            })
        } else if let Some(uid) = self.ephemeral.get_uid_by_object_id(id) {
            self.persisted.units.get(uid).map(|u| KillRec {
                time: now,
                side: u.side,
                typ: u.typ.0.clone(),
                tags: u.tags,
                pos: Some(u.pos),
                objective: self.persisted.objectives_by_group.get(&u.group).copied(),
                by: vec![],
            })
        } else {
            None
        };
        if let Some(rec) = rec {
            self.persisted.story.rt.deaths.push((id.clone(), rec));
        }
    }

    /// The static `id` was hit and died; `who` fired the shot.
    pub fn story_static_hit(&mut self, id: &DcsOid<ClassStatic>, who: Option<Who>) {
        if self.story_cfg().is_none() {
            return;
        }
        if let Some(who) = who {
            let c = self.who_cause(&who, None);
            self.persisted.story.rt.static_killers.insert(id.clone(), c);
        }
    }

    /// The static `id` (a building of an objective) died. Called before the
    /// death is processed.
    pub(super) fn story_static_lost(
        &mut self,
        id: &DcsOid<ClassStatic>,
        now: DateTime<Utc>,
    ) {
        let killer = self.persisted.story.rt.static_killers.remove(id);
        if self.story_cfg().is_none() {
            return;
        }
        let Some(uid) = self.ephemeral.uid_by_static.get(id) else { return };
        let Some(u) = self.persisted.units.get(uid) else { return };
        if u.dead {
            return;
        }
        let rec = KillRec {
            time: now,
            side: u.side,
            typ: u.typ.0.clone(),
            tags: u.tags,
            pos: Some(u.pos),
            objective: self.persisted.objectives_by_group.get(&u.group).copied(),
            by: killer.into_iter().collect(),
        };
        self.persisted.story.pending.push(Fact::Kill(rec));
        self.ephemeral.dirty();
    }

    /// The kill report: name the killers of the deaths seen since the last
    /// one and queue them all (a death no shot is recorded for is queued
    /// with nobody credited).
    pub fn story_kills(&mut self, dead: &[Dead]) {
        if self.persisted.story.rt.deaths.is_empty() {
            return;
        }
        let deaths = std::mem::take(&mut self.persisted.story.rt.deaths);
        for (oid, mut rec) in deaths {
            let victim = |w: &Who| match w {
                Who::AI { unit, .. } | Who::Player { unit, .. } => unit == &oid,
            };
            if let Some(d) = dead.iter().find(|d| victim(&d.victim)) {
                // enemy shots only; the ones that hit if any did, the last
                // first: whoever landed the killing blow leads
                let mut shots: Vec<&Shot> =
                    d.shots.iter().filter(|s| *s.shooter.side() != rec.side).collect();
                if shots.iter().any(|s| s.hit) {
                    shots.retain(|s| s.hit);
                }
                shots.sort_by_key(|s| std::cmp::Reverse(s.time));
                for s in shots {
                    let c = self.who_cause(
                        &s.shooter,
                        s.shooter_typ.as_ref().map(|s| s.as_str()),
                    );
                    if !rec.by.iter().any(|b| b.name == c.name) {
                        rec.by.push(c);
                    }
                }
                rec.by.truncate(MAX_CAUSES);
            }
            self.persisted.story.pending.push(Fact::Kill(rec));
        }
        self.ephemeral.dirty();
    }

    /// `to` captured `oid` from `from`; `ucids` are the players whose troops
    /// took it.
    pub(super) fn story_capture(
        &mut self,
        oid: ObjectiveId,
        from: Side,
        to: Side,
        ucids: &[Ucid],
        now: DateTime<Utc>,
    ) {
        if self.story_cfg().is_none() {
            return;
        }
        let by = if ucids.is_empty() {
            vec![Cause::ai("troops")]
        } else {
            ucids.iter().map(|u| self.player_cause(u)).collect()
        };
        self.persisted.story.pending.push(Fact::Capture { oid, from, to, by, time: now });
        self.ephemeral.dirty();
    }

    /// A convoy, supply flight or supply ship of `side` was destroyed;
    /// `key` names it so a convoy counts once.
    pub fn story_route_destroyed(
        &mut self,
        side: Side,
        key: &str,
        killer: Option<Ucid>,
        now: DateTime<Utc>,
    ) {
        if self.story_cfg().is_none() {
            return;
        }
        let by = killer.map(|u| self.player_cause(&u)).into_iter().collect();
        self.persisted.story.pending.push(Fact::Route(RouteRec {
            time: now,
            side,
            key: key.into(),
            by,
        }));
        self.ephemeral.dirty();
    }

    /// A unit of convoy `gid` was killed, by `killer` if a player.
    pub fn story_convoy_hit(&mut self, gid: GroupId, killer: Option<Ucid>) {
        if self.story_cfg().is_none() {
            return;
        }
        if let Some(u) = killer {
            let c = self.player_cause(&u);
            let by = self.persisted.story.rt.convoy_killers.entry(gid).or_default();
            if !by.iter().any(|b| b.name == c.name) {
                by.push(c);
            }
        }
    }

    /// Convoy `gid` of `side` was destroyed on the road.
    pub(super) fn story_convoy_lost(
        &mut self,
        side: Side,
        gid: GroupId,
        now: DateTime<Utc>,
    ) {
        let by = self.persisted.story.rt.convoy_killers.remove(&gid).unwrap_or_default();
        if self.story_cfg().is_none() {
            return;
        }
        self.persisted.story.pending.push(Fact::Route(RouteRec {
            time: now,
            side,
            key: fstr!("convoy:{gid}"),
            by,
        }));
        self.ephemeral.dirty();
    }

    /// The production multiples a story has left `side` with.
    pub(super) fn story_production(&self, side: Side) -> ProdMult {
        self.persisted.story.production(side)
    }

    /// The winner a story event ended the round with, if one did.
    pub(super) fn story_ending(&self) -> Option<Side> {
        self.story_cfg()?;
        self.persisted.story.ending
    }

    /// The config to write to disk: without the story's changes to the
    /// action list, which belong to this round, not to the campaign.
    pub(crate) fn cfg_for_save(&self) -> Arc<Cfg> {
        match self.persisted.story.rt.base_actions.as_ref() {
            Some(base) => {
                let mut cfg = (*self.ephemeral.cfg).clone();
                cfg.actions = base.clone();
                Arc::new(cfg)
            }
            None => Arc::clone(&self.ephemeral.cfg),
        }
    }

    /// Bring the live action list in line with the story: the config's
    /// `locked_actions` taken out, and every lock, unlock and price change
    /// events have made this round applied.
    pub(crate) fn story_sync_actions(&mut self) {
        let cfg = Arc::clone(&self.ephemeral.cfg);
        let story = cfg.story.as_ref().filter(|s| s.enabled);
        let st = &mut self.persisted.story;
        st.rt.synced = true;
        if st.rt.base_actions.is_none() {
            let nothing = story.is_none_or(|s| s.locked_actions.is_empty())
                && st.actions.is_empty();
            if nothing {
                return;
            }
            st.rt.base_actions = Some(cfg.actions.clone());
        }
        let Some(base) = st.rt.base_actions.as_ref() else { return };
        let actions = st.effective_actions(base, story);
        Arc::make_mut(&mut self.ephemeral.cfg).actions = actions;
    }

    /// Warn once about objectives the story names that the mission lacks.
    fn story_check_names(&mut self, sc: &StoryCfg) {
        if self.persisted.story.rt.checked {
            return;
        }
        self.persisted.story.rt.checked = true;
        for n in sc.objective_names() {
            if self.persisted.objectives_by_name.get(&String::from(n)).is_none() {
                warn!("story: no objective named '{n}' in this mission")
            }
        }
    }

    fn story_world(&self) -> World {
        let objectives = self
            .persisted
            .objectives
            .into_iter()
            .map(|(id, o)| ObjState {
                id: *id,
                name: o.name.clone(),
                owner: o.owner,
                health: o.health,
                pos: o.pos(),
                radius: o.radius(),
                supply: matches!(
                    o.kind,
                    ObjectiveKind::Logistics | ObjectiveKind::Factory { .. }
                ),
            })
            .collect();
        World {
            objectives,
            fuel: self.will_fuel(),
            will: [self.will_of(Side::Blue), self.will_of(Side::Red)],
            airframes: if self.story_needs_stock() {
                self.air_stock_totals()
            } else {
                Default::default()
            },
        }
    }

    /// The slow tick: evaluate what happened, fire what is due, apply it,
    /// tell everyone and publish it.
    pub(crate) fn story_tick(&mut self, now: DateTime<Utc>) {
        let Some(cfg) = self.story_cfg() else { return };
        let Some(sc) = cfg.story.as_ref() else { return };
        if !self.persisted.story.rt.synced {
            self.story_sync_actions();
        }
        self.story_check_names(sc);
        let world = self.story_world();
        let firings = self.persisted.story.step(sc, &world, now);
        for f in &firings {
            self.story_apply(sc, f, now);
        }
        self.ephemeral.dirty();
    }

    fn story_apply(&mut self, sc: &StoryCfg, f: &Firing, now: DateTime<Utc>) {
        let ev = &sc.events[f.idx];
        let mut fx: Vec<String> = vec![];
        for e in &ev.effects {
            fx.extend(self.story_effect(ev, e, f, now));
        }
        let Some(fired) = self.persisted.story.fired.get_mut(f.fired) else { return };
        fired.effects = fx;
        let fired = fired.clone();
        info!(
            "story: {} fired, caused by {:?}, effects {:?}",
            fired.id, fired.caused_by, fired.effects
        );
        let msg = if fired.text.is_empty() {
            fstr!("{}", fired.title.to_uppercase())
        } else {
            fstr!("{}: {}", fired.title.to_uppercase(), fired.text)
        };
        let msgs = self.ephemeral.msgs();
        msgs.send(MsgTyp::Chat(None), msg.clone());
        msgs.panel_to_all(30, true, msg);
        self.ephemeral.stat(Stat::StoryEvent(StoryEventStat {
            id: fired.id,
            time: fired.time,
            title: fired.title,
            text: fired.text,
            caused_by: fired.caused_by,
            side: fired.side,
            effects: fired.effects,
        }));
    }

    fn oid_by_name(&self, name: &str) -> Option<ObjectiveId> {
        self.persisted.objectives_by_name.get(&String::from(name)).copied()
    }

    /// Apply one effect; what it changed, for the record.
    fn story_effect(
        &mut self,
        ev: &StoryEventCfg,
        e: &StoryEffect,
        f: &Firing,
        now: DateTime<Utc>,
    ) -> Vec<String> {
        let mut out: Vec<String> = vec![];
        match e {
            StoryEffect::Will { side, delta } => {
                if let Some(d) = self.will_story(*side, *delta, &ev.title, now)
                    && d.abs() >= 0.05
                {
                    out.push(fstr!("{side:?} will {}", fmt_delta(d)))
                }
            }
            StoryEffect::Production { side, fuel_pct, equipment_pct } => {
                let Some(p) = self.persisted.story.production_mut(*side) else {
                    return out;
                };
                let scale = |m: &mut f64, pct: f64| {
                    if pct.is_finite() && pct != 0. {
                        *m = (*m * (1. + pct / 100.)).clamp(0., 5.);
                        true
                    } else {
                        false
                    }
                };
                if scale(&mut p.fuel, *fuel_pct) {
                    out.push(fstr!("{side:?} fuel output {}%", fmt_delta(*fuel_pct)))
                }
                if scale(&mut p.equipment, *equipment_pct) {
                    out.push(fstr!(
                        "{side:?} equipment output {}%",
                        fmt_delta(*equipment_pct)
                    ))
                }
            }
            StoryEffect::Stock { side, objective, item, pct } => {
                let frac = (pct / 100.).clamp(0., 1.) as f32;
                if !(frac > 0.) {
                    return out;
                }
                let (targets, place): (Vec<ObjectiveId>, String) = match objective {
                    Some(name) => match self.oid_by_name(name) {
                        Some(oid) => (vec![oid], fstr!("at {name}")),
                        None => return out,
                    },
                    None => {
                        let Some(side) = side else { return out };
                        let hubs: Vec<ObjectiveId> = self
                            .persisted
                            .logistics_hubs
                            .into_iter()
                            .copied()
                            .filter(|o| {
                                self.persisted
                                    .objectives
                                    .get(o)
                                    .is_some_and(|o| o.owner == *side)
                            })
                            .collect();
                        let n = hubs.len();
                        (hubs, fstr!("at its {n} logistics hub(s)"))
                    }
                };
                let item = item.trim();
                let fuel =
                    item.eq_ignore_ascii_case("fuel") || item.eq_ignore_ascii_case("all");
                let all_eq = item.eq_ignore_ascii_case("equipment")
                    || item.eq_ignore_ascii_case("all");
                let mut taken = false;
                let reduce = |inv: &mut Inventory| inv.reduce(frac) > 0;
                for oid in &targets {
                    let Some(o) = self.persisted.objectives.get_mut_cow(oid) else {
                        continue;
                    };
                    if fuel {
                        for (_, inv) in o.warehouse.liquids.iter_mut_cow() {
                            taken |= reduce(inv);
                        }
                    }
                    for (name, inv) in o.warehouse.equipment.iter_mut_cow() {
                        if all_eq || name.as_str().eq_ignore_ascii_case(item) {
                            taken |= reduce(inv);
                        }
                    }
                }
                if taken {
                    let owner = match (objective, side) {
                        (_, Some(s)) => fstr!("{s:?}"),
                        (Some(n), None) => self
                            .oid_by_name(n)
                            .and_then(|o| self.persisted.objectives.get(&o))
                            .map(|o| fstr!("{:?}", o.owner))
                            .unwrap_or_default(),
                        (None, None) => String::from(""),
                    };
                    out.push(fstr!("{owner} loses {pct:.0}% of its {item} {place}"))
                }
            }
            StoryEffect::LockAction { side, action }
            | StoryEffect::UnlockAction { side, action }
            | StoryEffect::PriceAction { side, action, .. } => {
                let known = self
                    .persisted
                    .story
                    .rt
                    .base_actions
                    .as_ref()
                    .unwrap_or(&self.ephemeral.cfg.actions)
                    .get(side)
                    .is_some_and(|a| a.contains_key(action.as_str()));
                if !known {
                    warn!(
                        "story: {} names {side:?} action '{action}', which does not exist",
                        ev.id
                    );
                    return out;
                }
                let change = match e {
                    StoryEffect::LockAction { .. } => ActionChange::Lock,
                    StoryEffect::UnlockAction { .. } => ActionChange::Unlock,
                    StoryEffect::PriceAction { cost, .. } => ActionChange::Cost(*cost),
                    _ => return out,
                };
                let st = &mut self.persisted.story;
                if st.rt.base_actions.is_none() {
                    st.rt.base_actions = Some(self.ephemeral.cfg.actions.clone());
                }
                st.actions.push(ActionOverride {
                    side: *side,
                    action: action.as_str().into(),
                    change,
                });
                self.story_sync_actions();
                out.push(match change {
                    ActionChange::Lock => fstr!("{side:?} loses {action}"),
                    ActionChange::Unlock => fstr!("{side:?} gains {action}"),
                    ActionChange::Cost(c) => fstr!("{side:?} {action} now costs {c}"),
                })
            }
            StoryEffect::Points { points } => {
                let mut paid: Vec<String> = vec![];
                for c in &f.causes {
                    if let Some(u) = c.ucid {
                        self.adjust_points(&u, *points, &format!("for {}", ev.title));
                        paid.push(c.name.clone());
                    }
                }
                if !paid.is_empty() {
                    out.push(fstr!("{points:+} points to {}", join_names(&paid)))
                }
            }
            StoryEffect::Hq {
                side,
                posture,
                main_effort,
                defend,
                supply_priority,
                avoid,
                weights,
                intent,
                hours,
            } => {
                let ids = |v: &[std::string::String]| -> Vec<u64> {
                    v.iter()
                        .filter_map(|n| self.oid_by_name(n))
                        .map(|o| o.inner() as u64)
                        .collect()
                };
                let d = Directive {
                    posture: *posture,
                    main_effort: main_effort
                        .as_deref()
                        .and_then(|n| self.oid_by_name(n))
                        .map(|o| o.inner() as u64),
                    defend: ids(defend),
                    supply_priority: ids(supply_priority),
                    avoid: ids(avoid),
                    weights: weights.clone(),
                    intent: intent.clone(),
                    rationale: Some(format!("story event: {}", ev.title)),
                    ttl_secs: None,
                };
                let hours = if hours.is_finite() { hours.clamp(0.1, 72.) } else { 4. };
                self.persisted.hq.side_mut(*side).story = Some(HeldDirective {
                    directive: d,
                    received: now,
                    expires: now + Duration::seconds((hours * 3600.) as i64),
                });
                let mut parts: Vec<std::string::String> = vec![];
                if let Some(p) = posture {
                    parts.push(p.label().to_lowercase());
                }
                if let Some(m) = main_effort {
                    parts.push(format!("main effort {m}"));
                }
                if !defend.is_empty() {
                    parts.push(format!("defend {}", defend.join(", ")));
                }
                if !supply_priority.is_empty() {
                    parts.push(format!("resupply {}", supply_priority.join(", ")));
                }
                if !avoid.is_empty() {
                    parts.push(format!("avoid {}", avoid.join(", ")));
                }
                for (l, w) in weights {
                    parts.push(format!("{} x{w:.1}", l.label()));
                }
                if parts.is_empty() {
                    parts.push("new orders".into());
                }
                out.push(fstr!("{side:?} HQ: {} ({hours:.0} h)", parts.join("; ")))
            }
            StoryEffect::RevealIntel { side, objective, radius_m } => {
                let Some(pos) = self
                    .oid_by_name(objective)
                    .and_then(|o| self.persisted.objectives.get(&o))
                    .map(|o| o.pos())
                else {
                    return out;
                };
                let r = if radius_m.is_finite() && *radius_m > 0. {
                    *radius_m
                } else {
                    15_000.
                };
                let n = self.apply_recon_scan(
                    pos,
                    r,
                    *side,
                    IntelSource::HumanInt,
                    None,
                    false,
                    now,
                );
                out.push(fstr!("{side:?} intel: {n} enemy units around {objective}"))
            }
            StoryEffect::ReinforceAirframes { objective, typ, count } => {
                let Some(oid) = self.oid_by_name(objective) else { return out };
                let owner = self
                    .persisted
                    .objectives
                    .get(&oid)
                    .map(|o| o.owner)
                    .unwrap_or(Side::Neutral);
                if owner == Side::Neutral || *count == 0 {
                    return out;
                }
                let n = self.air_stock_credit(oid, &String::from(typ.as_str()), *count);
                if n > 0 {
                    out.push(fstr!("{owner:?} receives {n} {typ} at {objective}"))
                }
            }
            StoryEffect::EndRound { winner } => {
                self.persisted.story.ending = Some(*winner);
                out.push(match winner {
                    Side::Neutral => "The round ends in a draw".into(),
                    w => fstr!("The round ends: {w:?} wins"),
                })
            }
            StoryEffect::NuclearStrike(c) => {
                out.push(self.order_nuclear_strike(c, now).as_str().into());
            }
            StoryEffect::Message { side, text } => {
                let text = render(
                    text,
                    &f.causes,
                    f.objective.as_ref().map(|s| s.as_str()),
                    ev.side,
                );
                let msgs = self.ephemeral.msgs();
                match side {
                    Some(s) => msgs.panel_to_side(20, false, *s, text),
                    None => msgs.panel_to_all(20, false, text),
                }
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bfprotocols::cfg::UnitTag;

    fn t0() -> DateTime<Utc> {
        DateTime::parse_from_rfc3339("2026-10-31T12:00:00Z").unwrap().with_timezone(&Utc)
    }

    fn mins(n: i64) -> Duration {
        Duration::minutes(n)
    }

    fn story(json: &str) -> StoryCfg {
        let c: StoryCfg = serde_json::from_str(json).unwrap();
        c.validate().unwrap();
        c
    }

    fn obj(id: i64, name: &str, owner: Side, health: u8) -> ObjState {
        ObjState {
            id: ObjectiveId::from(id),
            name: name.into(),
            owner,
            health,
            pos: Vector2::new(id as f64 * 100_000., 0.),
            radius: 2_000.,
            supply: false,
        }
    }

    fn world(objs: Vec<ObjState>) -> World {
        World { objectives: objs, ..Default::default() }
    }

    fn pilot(name: &str) -> Cause {
        Cause { name: name.into(), ucid: None }
    }

    fn kill(time: DateTime<Utc>, side: Side, typ: &str, by: &str) -> Fact {
        Fact::Kill(KillRec {
            time,
            side,
            typ: typ.into(),
            tags: UnitTags::from(vec![UnitTag::Aircraft]),
            pos: None,
            objective: None,
            by: vec![pilot(by)],
        })
    }

    fn ids(f: &[Firing], cfg: &StoryCfg) -> Vec<std::string::String> {
        f.iter().map(|f| cfg.events[f.idx].id.clone()).collect()
    }

    const TUAPSE: &str = r#"{"events": [
        {"id": "tuapse_burns", "title": "Tuapse burns", "side": "Red",
         "text": "{by} set {objective} on fire.",
         "trigger": {"ObjectiveDestroyed": {"objective": "Tuapse refinery"}},
         "effects": [{"Will": {"side": "Red", "delta": -6}}]},
        {"id": "carrier_hit", "title": "The carrier is hit",
         "trigger": {"ObjectiveHealthBelow": {"objective": "CVBG", "pct": 75, "owner": "Blue"}},
         "effects": [{"Points": {"points": 100}}]}
    ]}"#;

    #[test]
    fn objective_destroyed_and_health_below_fire_on_the_crossing_only() {
        let cfg = story(TUAPSE);
        let mut s = Story::default();
        let w = |hp, cv| {
            world(vec![
                obj(1, "Tuapse refinery", Side::Red, hp),
                obj(2, "CVBG", Side::Blue, cv),
            ])
        };
        // already burning and already hit when first seen: nothing (the
        // first look only records)
        assert!(s.step(&cfg, &w(0, 50), t0()).is_empty());
        let mut s = Story::default();
        s.step(&cfg, &w(100, 100), t0());
        // the refinery's last building is destroyed by Shade
        s.pending.push(Fact::Kill(KillRec {
            time: t0(),
            side: Side::Red,
            typ: "refinery".into(),
            tags: UnitTags::default(),
            pos: None,
            objective: Some(ObjectiveId::from(1)),
            by: vec![pilot("Shade")],
        }));
        let f = s.step(&cfg, &w(0, 80), t0() + mins(1));
        assert_eq!(ids(&f, &cfg), vec!["tuapse_burns"]);
        assert_eq!(f[0].causes, vec![pilot("Shade")]);
        assert_eq!(s.fired[0].text.as_str(), "Shade set Tuapse refinery on fire.");
        assert_eq!(s.fired[0].caused_by, vec![String::from("Shade")]);
        // the carrier falls to 74: under the line
        let f = s.step(&cfg, &w(0, 74), t0() + mins(2));
        assert_eq!(ids(&f, &cfg), vec!["carrier_hit"]);
        // once per round: repaired and hit again, nothing
        s.step(&cfg, &w(100, 100), t0() + mins(3));
        assert!(s.step(&cfg, &w(0, 10), t0() + mins(4)).is_empty());
    }

    #[test]
    fn a_change_of_owner_is_not_damage() {
        let cfg = story(TUAPSE);
        let mut s = Story::default();
        s.step(&cfg, &world(vec![obj(2, "CVBG", Side::Red, 100)]), t0());
        // captured by Blue with a damaged garrison: not "hit"
        assert!(
            s.step(&cfg, &world(vec![obj(2, "CVBG", Side::Blue, 50)]), t0()).is_empty()
        );
    }

    #[test]
    fn captured_and_held() {
        let cfg = story(
            r#"{"events": [
            {"id": "senaki_falls", "title": "Senaki falls",
             "trigger": {"ObjectiveCaptured": {"objective": "Senaki-Kolkhi", "by": "Blue"}}},
            {"id": "senaki_held", "title": "Senaki held",
             "trigger": {"ObjectiveHeld": {"objective": "Senaki-Kolkhi", "by": "Blue", "minutes": 120}}}
        ]}"#,
        );
        let mut s = Story::default();
        let w = |owner| world(vec![obj(1, "Senaki-Kolkhi", owner, 100)]);
        // Blue holding it from the start is not holding it
        assert!(s.step(&cfg, &w(Side::Blue), t0() + mins(500)).is_empty());
        let mut s = Story::default();
        // Red takes it: not the event's side
        s.pending.push(Fact::Capture {
            oid: ObjectiveId::from(1),
            from: Side::Blue,
            to: Side::Red,
            by: vec![pilot("Ivan")],
            time: t0(),
        });
        assert!(s.step(&cfg, &w(Side::Red), t0()).is_empty());
        s.pending.push(Fact::Capture {
            oid: ObjectiveId::from(1),
            from: Side::Red,
            to: Side::Blue,
            by: vec![pilot("Shade"), pilot("Kilo")],
            time: t0() + mins(10),
        });
        let f = s.step(&cfg, &w(Side::Blue), t0() + mins(10));
        assert_eq!(ids(&f, &cfg), vec!["senaki_falls"]);
        assert_eq!(f[0].causes.len(), 2);
        assert!(s.step(&cfg, &w(Side::Blue), t0() + mins(100)).is_empty());
        let f = s.step(&cfg, &w(Side::Blue), t0() + mins(131));
        assert_eq!(ids(&f, &cfg), vec!["senaki_held"]);
        assert_eq!(f[0].causes, vec![pilot("Shade"), pilot("Kilo")]);
    }

    #[test]
    fn kills_in_a_window_with_a_cooldown() {
        let cfg = story(
            r#"{"events": [
            {"id": "backfires", "title": "Backfire regiment broken", "repeat": 7200,
             "trigger": {"Kills": {"count": 3, "window_minutes": 60, "types": ["Tu-22M3"], "side": "Red"}}}
        ]}"#,
        );
        let w = world(vec![]);
        let mut s = Story::default();
        // two, then the third outside the hour: no
        s.pending.push(kill(t0(), Side::Red, "Tu-22M3", "Shade"));
        s.pending.push(kill(t0() + mins(5), Side::Red, "Tu-22M3", "Kilo"));
        s.pending.push(kill(t0() + mins(5), Side::Red, "MiG-29A", "Kilo"));
        s.pending.push(kill(t0() + mins(5), Side::Blue, "Tu-22M3", "Kilo"));
        assert!(s.step(&cfg, &w, t0() + mins(5)).is_empty());
        s.pending.push(kill(t0() + mins(70), Side::Red, "TU-22M3", "Kilo"));
        assert!(s.step(&cfg, &w, t0() + mins(70)).is_empty());
        // the first at 5 is out of the hour by now: these make three
        s.pending.push(kill(t0() + mins(75), Side::Red, "Tu-22M3", "Kilo"));
        s.pending.push(kill(t0() + mins(75), Side::Red, "Tu-22M3", "Shade"));
        let f = s.step(&cfg, &w, t0() + mins(75));
        assert_eq!(ids(&f, &cfg), vec!["backfires"]);
        // the most kills first
        assert_eq!(f[0].causes[0], pilot("Kilo"));
        // within the cooldown: three more do not fire it
        for m in [80, 81, 82] {
            s.pending.push(kill(t0() + mins(m), Side::Red, "Tu-22M3", "Shade"));
        }
        assert!(s.step(&cfg, &w, t0() + mins(82)).is_empty());
        // after it: the kills since the last firing count
        s.pending.push(kill(t0() + mins(76 + 120), Side::Red, "Tu-22M3", "Shade"));
        s.pending.push(kill(t0() + mins(77 + 120), Side::Red, "Tu-22M3", "Shade"));
        s.pending.push(kill(t0() + mins(78 + 120), Side::Red, "Tu-22M3", "Shade"));
        let f = s.step(&cfg, &w, t0() + mins(78 + 120));
        assert_eq!(ids(&f, &cfg), vec!["backfires"]);
        assert_eq!(s.fired.len(), 2);
    }

    #[test]
    fn kills_inside_an_objective_zone() {
        let cfg = story(
            r#"{"events": [
            {"id": "coast_road", "title": "Coast road", "trigger": {"Kills":
              {"count": 1, "window_minutes": 60, "tags": ["Armor"], "objective": "Sukhumi", "radius_m": 10000}}}
        ]}"#,
        );
        let w = world(vec![obj(1, "Sukhumi", Side::Red, 100)]);
        let mk = |x: f64, tags: Vec<UnitTag>| {
            Fact::Kill(KillRec {
                time: t0(),
                side: Side::Red,
                typ: "T-72B".into(),
                tags: UnitTags::from(tags),
                pos: Some(Vector2::new(100_000. + x, 0.)),
                objective: None,
                by: vec![pilot("Shade")],
            })
        };
        let mut s = Story::default();
        s.pending.push(mk(20_000., vec![UnitTag::Armor]));
        s.pending.push(mk(0., vec![UnitTag::Infantry]));
        assert!(s.step(&cfg, &w, t0()).is_empty());
        s.pending.push(mk(5_000., vec![UnitTag::Armor]));
        assert_eq!(s.step(&cfg, &w, t0()).len(), 1);
    }

    #[test]
    fn supply_routes_count_each_convoy_once() {
        let cfg = story(
            r#"{"events": [
            {"id": "coast_convoys", "title": "Convoys burning",
             "trigger": {"SupplyRoutesDestroyed": {"count": 2, "window_minutes": 600, "side": "Red"}}}
        ]}"#,
        );
        let route = |key: &str, side, m| {
            Fact::Route(RouteRec {
                time: t0() + mins(m),
                side,
                key: key.into(),
                by: vec![pilot("Shade")],
            })
        };
        let mut s = Story::default();
        s.pending.push(route("convoy:7", Side::Red, 0));
        s.pending.push(route("convoy:7", Side::Red, 1));
        s.pending.push(route("convoy:8", Side::Blue, 2));
        assert!(s.step(&cfg, &world(vec![]), t0() + mins(2)).is_empty());
        s.pending.push(route("air:3", Side::Red, 30));
        let f = s.step(&cfg, &world(vec![]), t0() + mins(30));
        assert_eq!(ids(&f, &cfg), vec!["coast_convoys"]);
        assert_eq!(f[0].causes, vec![pilot("Shade")]);
    }

    #[test]
    fn fuel_and_will_fire_only_after_crossing_and_chains_follow() {
        let cfg = story(
            r#"{"events": [
            {"id": "fuel_crisis", "title": "Fuel crisis", "side": "Red",
             "trigger": {"FuelBelow": {"side": "Red", "pct": 30}}},
            {"id": "rationing", "title": "Rationing", "requires": ["fuel_crisis"],
             "trigger": {"EventFired": {"id": "fuel_crisis"}}},
            {"id": "terms", "title": "Moscow asks for terms",
             "trigger": {"WillBelow": {"side": "Red", "value": 20}}},
            {"id": "elated", "title": "Elated",
             "trigger": {"WillAbove": {"side": "Blue", "value": 90}}}
        ]}"#,
        );
        let w = |fuel: f64, red: f64, blue: f64| World {
            objectives: vec![],
            fuel: [Some(50.), Some(fuel)],
            will: [Some(blue), Some(red)],
            airframes: Default::default(),
        };
        let mut s = Story::default();
        // starts short of fuel, broken and elated: nothing fires
        assert!(s.step(&cfg, &w(10., 10., 95.), t0()).is_empty());
        assert!(s.armed.is_empty());
        // recovers, then falls again: now it is play that did it
        s.pending.push(kill(t0(), Side::Red, "fuel truck", "Shade"));
        assert!(s.step(&cfg, &w(40., 30., 70.), t0() + mins(1)).is_empty());
        assert_eq!(s.armed.len(), 3);
        let f = s.step(&cfg, &w(25., 30., 70.), t0() + mins(2));
        assert_eq!(ids(&f, &cfg), vec!["fuel_crisis", "rationing"]);
        assert_eq!(f[0].causes, vec![pilot("Shade")]);
        assert_eq!(f[1].causes, f[0].causes);
        let f = s.step(&cfg, &w(25., 15., 95.), t0() + mins(3));
        assert_eq!(ids(&f, &cfg), vec!["terms", "elated"]);
        // once: falling and recovering again does nothing
        s.step(&cfg, &w(40., 30., 70.), t0() + mins(4));
        assert!(s.step(&cfg, &w(20., 10., 95.), t0() + mins(5)).is_empty());
    }

    #[test]
    fn stock_below_fires_on_crossing_and_never_for_unlimited() {
        use super::super::air_stock::SideStock;
        let cfg = story(
            r#"{"events": [
            {"id": "backfires_spent", "title": "Backfires spent", "side": "Red",
             "trigger": {"StockBelow": {"side": "Red", "types": ["Tu-22M3", "tu-22m3x"], "below": 2}}}
        ]}"#,
        );
        let w = |n: u32, unlimited: bool| {
            let mut w = World::default();
            w.airframes[1].insert("tu-22m3".into(), SideStock { stock: n, unlimited });
            w.airframes[0]
                .insert("tu-22m3".into(), SideStock { stock: 0, unlimited: false });
            w
        };
        let mut s = Story::default();
        // none at the start: nothing fires, it was never above the line
        assert!(s.step(&cfg, &w(0, false), t0()).is_empty());
        s.pending.push(kill(t0(), Side::Red, "Tu-22M3", "Shade"));
        assert!(s.step(&cfg, &w(4, false), t0() + mins(1)).is_empty());
        let f = s.step(&cfg, &w(1, false), t0() + mins(2));
        assert_eq!(ids(&f, &cfg), vec!["backfires_spent"]);
        assert_eq!(f[0].causes, vec![pilot("Shade")]);
        // an unlimited base holds them: never low
        let mut s = Story::default();
        assert!(s.step(&cfg, &w(4, true), t0()).is_empty());
        assert!(s.step(&cfg, &w(0, true), t0() + mins(1)).is_empty());
    }

    #[test]
    fn branches_exclude_each_other() {
        let cfg = story(
            r#"{"events": [
            {"id": "blue_takes_senaki", "title": "Senaki falls", "excludes": ["red_holds_senaki"],
             "trigger": {"ObjectiveCaptured": {"objective": "Senaki-Kolkhi", "by": "Blue"}}},
            {"id": "red_holds_senaki", "title": "Senaki holds", "excludes": ["blue_takes_senaki"],
             "trigger": {"ObjectiveCaptured": {"objective": "Senaki-Kolkhi", "by": "Red"}}}
        ]}"#,
        );
        let w = |owner| world(vec![obj(1, "Senaki-Kolkhi", owner, 100)]);
        let cap = |to: Side, m: i64| Fact::Capture {
            oid: ObjectiveId::from(1),
            from: to.opposite(),
            to,
            by: vec![],
            time: t0() + mins(m),
        };
        let mut s = Story::default();
        s.pending.push(cap(Side::Red, 0));
        assert_eq!(
            ids(&s.step(&cfg, &w(Side::Red), t0()), &cfg),
            vec!["red_holds_senaki"]
        );
        s.pending.push(cap(Side::Blue, 5));
        assert!(s.step(&cfg, &w(Side::Blue), t0() + mins(5)).is_empty());
    }

    #[test]
    fn a_reload_never_refires_and_keeps_pending_facts() {
        let cfg = story(TUAPSE);
        let mut s = Story::default();
        s.step(&cfg, &world(vec![obj(1, "Tuapse refinery", Side::Red, 100)]), t0());
        let f = s.step(&cfg, &world(vec![obj(1, "Tuapse refinery", Side::Red, 0)]), t0());
        assert_eq!(f.len(), 1);
        s.pending.push(kill(t0(), Side::Red, "Tu-22M3", "Shade"));
        s.blue_production.fuel = 0.6;
        s.actions.push(ActionOverride {
            side: Side::Blue,
            action: "B-52".into(),
            change: ActionChange::Unlock,
        });
        let mut back: Story =
            serde_json::from_str(&serde_json::to_string(&s).unwrap()).unwrap();
        assert_eq!(back.fired, s.fired);
        assert_eq!(back.pending.len(), 1);
        assert_eq!(back.blue_production.fuel, 0.6);
        assert_eq!(back.actions, s.actions);
        // repaired and destroyed again after the restart: not again
        back.step(&cfg, &world(vec![obj(1, "Tuapse refinery", Side::Red, 100)]), t0());
        assert!(
            back.step(&cfg, &world(vec![obj(1, "Tuapse refinery", Side::Red, 0)]), t0())
                .is_empty()
        );
        // a save from before the story: no field at all
        let mut json =
            serde_json::to_value(super::super::persisted::Persisted::default()).unwrap();
        json.as_object_mut().unwrap().remove("story");
        let p: super::super::persisted::Persisted = serde_json::from_value(json).unwrap();
        assert!(p.story.fired.is_empty());
        assert_eq!(p.story.red_production, ProdMult::default());
    }

    #[test]
    fn effective_actions_lock_unlock_and_price_in_order() {
        let base_cfg = Cfg::default();
        let proto =
            base_cfg.actions.values().flat_map(|a| a.values()).next().unwrap().clone();
        let mut base = Actions::default();
        let blue = base.entry(Side::Blue).or_default();
        for n in ["Tanker", "B-52", "Strike"] {
            blue.insert(n.into(), proto.clone());
        }
        let cfg = story(r#"{"locked_actions": {"Blue": ["B-52"]}, "events": []}"#);
        let mut s = Story::default();
        let names = |a: &Actions| -> Vec<std::string::String> {
            a[&Side::Blue].keys().map(|k| k.to_string()).collect()
        };
        assert_eq!(
            names(&s.effective_actions(&base, Some(&cfg))),
            vec!["Tanker", "Strike"]
        );
        let over = |action: &str, change| ActionOverride {
            side: Side::Blue,
            action: action.into(),
            change,
        };
        s.actions.push(over("B-52", ActionChange::Unlock));
        s.actions.push(over("Strike", ActionChange::Lock));
        s.actions.push(over("B-52", ActionChange::Cost(7)));
        let a = s.effective_actions(&base, Some(&cfg));
        assert_eq!(names(&a), vec!["Tanker", "B-52"]);
        assert_eq!(a[&Side::Blue]["B-52"].cost, 7);
    }

    #[test]
    fn render_fills_placeholders() {
        assert_eq!(
            render(
                "{by} hit {objective} ({side} vs {enemy})",
                &[pilot("A"), pilot("B"), pilot("C")],
                Some("X"),
                Some(Side::Red)
            )
            .as_str(),
            "A, B and C hit X (Red vs Blue)"
        );
        assert_eq!(
            render("{by} did it", &[], None, None).as_str(),
            "unknown forces did it"
        );
    }

    // ---- the effects, on a Db -------------------------------------------

    fn strs(v: &[String]) -> Vec<&str> {
        v.iter().map(|s| s.as_str()).collect()
    }

    /// A Db with war will on, this story, Tuapse (a Red logistics hub with
    /// fuel and equipment) and Kutaisi, and one Blue player.
    fn db_with(story_json: &str) -> (Db, Ucid) {
        let mut cfg = Cfg::default();
        cfg.war_will = Some(Default::default());
        cfg.story = Some(serde_json::from_str(story_json).unwrap());
        let proto = cfg.actions.values().flat_map(|a| a.values()).next().unwrap().clone();
        let blue = cfg.actions.entry(Side::Blue).or_default();
        blue.clear();
        blue.insert("B-52".into(), proto.clone());
        blue.insert("Tanker".into(), proto);
        let mut db = Db::default();
        db.ephemeral.cfg = Arc::new(cfg);
        for (id, name, owner) in [(1, "Tuapse refinery", "Red"), (2, "Kutaisi", "Red")] {
            let mut o: super::super::objective::Objective = serde_json::from_value(serde_json::json!({
                "id": id, "name": name, "owner": owner, "kind": "Logistics", "groups": {},
                "health": 100, "logi": 100, "threatened": false,
                "last_threatened_ts": "2026-10-31T00:00:00Z", "last_change_ts": "2026-10-31T00:00:00Z",
            }))
            .unwrap();
            o.warehouse.liquids.insert_cow(
                dcso3::warehouse::LiquidType::JetFuel,
                Inventory { stored: 1000, capacity: 2000, ..Default::default() },
            );
            o.warehouse.equipment.insert_cow(
                "AIM-9M".into(),
                Inventory { stored: 100, capacity: 200, ..Default::default() },
            );
            let oid = ObjectiveId::from(id);
            db.persisted.objectives.insert_cow(oid, o);
            db.persisted.objectives_by_name.insert_cow(name.into(), oid);
            db.persisted.logistics_hubs.insert_cow(oid);
        }
        let ucid: Ucid = "0123456789abcdef0123456789abcdef".parse().unwrap();
        let p: super::super::player::Player = serde_json::from_value(serde_json::json!({
            "name": "Shade", "alts": [], "side": "Blue", "side_switches": null,
            "lives": {}, "crates": [], "points": 50,
        }))
        .unwrap_or_else(|e| panic!("player: {e}"));
        db.persisted.players.insert_cow(ucid, p);
        (db, ucid)
    }

    fn fire_now(db: &mut Db, causes: Vec<Cause>) -> FiredEvent {
        let cfg = Arc::clone(&db.ephemeral.cfg);
        let sc = cfg.story.as_ref().unwrap();
        db.story_sync_actions();
        let world = db.story_world();
        db.persisted.story.step(sc, &world, t0());
        db.persisted.story.pending.push(Fact::Capture {
            oid: ObjectiveId::from(2),
            from: Side::Red,
            to: Side::Blue,
            by: causes,
            time: t0(),
        });
        let world = db.story_world();
        let firings = db.persisted.story.step(sc, &world, t0());
        assert_eq!(firings.len(), 1, "{firings:?}");
        db.story_apply(sc, &firings[0], t0());
        db.persisted.story.fired.last().unwrap().clone()
    }

    fn one_effect(effects: &str) -> std::string::String {
        format!(
            r#"{{"locked_actions": {{"Blue": ["B-52"]}}, "events": [{{"id": "kutaisi", "title": "Kutaisi falls",
              "trigger": {{"ObjectiveCaptured": {{"objective": "Kutaisi", "by": "Blue"}}}},
              "effects": {effects}}}]}}"#
        )
    }

    #[test]
    fn effect_will_and_production() {
        let (mut db, _) = db_with(&one_effect(
            r#"[{"Will": {"side": "Red", "delta": -6}},
                {"Production": {"side": "Red", "fuel_pct": -40, "equipment_pct": -20}}]"#,
        ));
        let f = fire_now(&mut db, vec![]);
        assert_eq!(
            strs(&f.effects),
            vec!["Red will -6", "Red fuel output -40%", "Red equipment output -20%"]
        );
        assert_eq!(db.will_of(Side::Red), Some(64.));
        let p = db.story_production(Side::Red);
        assert!((p.fuel - 0.6).abs() < 1e-9 && (p.equipment - 0.8).abs() < 1e-9);
        assert_eq!(db.story_production(Side::Blue), ProdMult::default());
    }

    #[test]
    fn effect_stock() {
        let (mut db, _) = db_with(&one_effect(
            r#"[{"Stock": {"objective": "Tuapse refinery", "pct": 50}},
                {"Stock": {"side": "Red", "item": "AIM-9M", "pct": 10}}]"#,
        ));
        let f = fire_now(&mut db, vec![]);
        assert_eq!(
            strs(&f.effects),
            vec![
                "Red loses 50% of its fuel at Tuapse refinery",
                "Red loses 10% of its AIM-9M at its 2 logistics hub(s)"
            ]
        );
        let o = db.persisted.objectives.get(&ObjectiveId::from(1)).unwrap();
        assert_eq!(
            o.warehouse
                .liquids
                .get(&dcso3::warehouse::LiquidType::JetFuel)
                .unwrap()
                .stored,
            500
        );
        assert_eq!(
            o.warehouse.equipment.get(&String::from("AIM-9M")).unwrap().stored,
            90
        );
    }

    #[test]
    fn effect_reinforce_airframes() {
        let (mut db, _) = db_with(&one_effect(
            r#"[{"ReinforceAirframes": {"objective": "Kutaisi", "type": "Tu-22M3", "count": 6}},
                {"ReinforceAirframes": {"objective": "Tuapse refinery", "type": "AIM-9M", "count": 500}},
                {"ReinforceAirframes": {"objective": "Nowhere", "type": "Tu-22M3", "count": 6}}]"#,
        ));
        let f = fire_now(&mut db, vec![]);
        // Kutaisi never stocked the type, so it takes all six; Tuapse is full
        // past 200; an unknown objective changes nothing.
        assert_eq!(
            strs(&f.effects),
            vec![
                "Red receives 6 Tu-22M3 at Kutaisi",
                "Red receives 100 AIM-9M at Tuapse refinery"
            ]
        );
        let o = db.persisted.objectives.get(&ObjectiveId::from(2)).unwrap();
        assert_eq!(
            o.warehouse.equipment.get(&String::from("Tu-22M3")).unwrap().stored,
            6
        );
    }

    #[test]
    fn effect_actions_and_save() {
        let (mut db, _) = db_with(&one_effect(
            r#"[{"UnlockAction": {"side": "Blue", "action": "B-52"}},
                {"PriceAction": {"side": "Blue", "action": "Tanker", "cost": 3}},
                {"LockAction": {"side": "Blue", "action": "Nope"}}]"#,
        ));
        db.story_sync_actions();
        let names = |db: &Db| -> Vec<std::string::String> {
            db.ephemeral.cfg.actions[&Side::Blue].keys().map(|k| k.to_string()).collect()
        };
        assert_eq!(names(&db), vec!["Tanker"]);
        let f = fire_now(&mut db, vec![]);
        assert_eq!(strs(&f.effects), vec!["Blue gains B-52", "Blue Tanker now costs 3"]);
        assert_eq!(names(&db), vec!["B-52", "Tanker"]);
        assert_eq!(db.ephemeral.cfg.actions[&Side::Blue]["Tanker"].cost, 3);
        // what is written to disk is the campaign's list, not the round's
        let saved = db.cfg_for_save();
        assert_eq!(saved.actions[&Side::Blue].len(), 2);
        assert_ne!(saved.actions[&Side::Blue]["Tanker"].cost, 3);
        // after a restart the same list comes back from the save
        let mut back = Db::default();
        back.ephemeral.cfg = Arc::clone(&saved);
        back.persisted.story =
            serde_json::from_str(&serde_json::to_string(&db.persisted.story).unwrap())
                .unwrap();
        back.story_sync_actions();
        assert_eq!(names(&back), vec!["B-52", "Tanker"]);
    }

    #[test]
    fn effect_points_go_to_the_players_who_caused_it() {
        let (mut db, ucid) = db_with(&one_effect(r#"[{"Points": {"points": 200}}]"#));
        let f = fire_now(
            &mut db,
            vec![Cause { name: "Shade".into(), ucid: Some(ucid) }, Cause::ai("troops")],
        );
        assert_eq!(f.caused_by, vec![String::from("Shade"), String::from("AI troops")]);
        assert_eq!(strs(&f.effects), vec!["+200 points to Shade"]);
        assert_eq!(db.persisted.players.get(&ucid).unwrap().points, 250);
    }

    #[test]
    fn effect_hq_intel_end_and_message() {
        let (mut db, _) = db_with(&one_effect(
            r#"[{"Hq": {"side": "Red", "posture": "defensive", "defend": ["Tuapse refinery"], "hours": 2}},
                {"RevealIntel": {"side": "Blue", "objective": "Tuapse refinery"}},
                {"Message": {"side": "Red", "text": "Hold {objective}."}},
                {"EndRound": {"winner": "Blue"}}]"#,
        ));
        let f = fire_now(&mut db, vec![]);
        assert_eq!(
            strs(&f.effects),
            vec![
                "Red HQ: defensive; defend Tuapse refinery (2 h)",
                "Blue intel: 0 enemy units around Tuapse refinery",
                "The round ends: Blue wins"
            ]
        );
        let held = db.persisted.hq.side(Side::Red).story.clone().unwrap();
        assert_eq!(held.directive.defend, vec![1]);
        assert_eq!(held.expires - held.received, Duration::hours(2));
        assert_eq!(db.story_ending(), Some(Side::Blue));
    }

    #[test]
    fn the_last_bases_trigger_fires_on_the_crossing() {
        let cfg = story(
            r#"{"events": [
            {"id": "last_bases", "title": "Down to the last bases", "side": "Red",
             "trigger": {"ObjectivesHeldBelow": {"side": "Red", "count": 3}}}
        ]}"#,
        );
        let w = |red: usize| World {
            objectives: (0..red)
                .map(|i| obj(i as i64, &format!("R{i}"), Side::Red, 100))
                .chain((0..5).map(|i| obj(100 + i, &format!("B{i}"), Side::Blue, 100)))
                .collect(),
            fuel: [None, None],
            will: [None, None],
            airframes: Default::default(),
        };
        let mut s = Story::default();
        // 4 held: armed, nothing yet
        assert!(s.step(&cfg, &w(4), t0()).is_empty());
        assert_eq!(s.armed.len(), 1);
        // down to 2: it fires, once
        let f = s.step(&cfg, &w(2), t0() + mins(1));
        assert_eq!(ids(&f, &cfg), vec!["last_bases"]);
        s.step(&cfg, &w(4), t0() + mins(2));
        assert!(s.step(&cfg, &w(1), t0() + mins(3)).is_empty());
    }

    #[test]
    fn a_nuclear_strike_is_ordered_once_a_round() {
        let (mut db, _) = db_with(&one_effect(
            r#"[{"NuclearStrike": {"side": "Red", "template": "RNUKE", "aircraft": "MiG-21bis",
                                   "will_on_detonation": 20, "enemy_will_on_detonation": -10}}]"#,
        ));
        let f = fire_now(&mut db, vec![]);
        assert_eq!(strs(&f.effects), vec!["Red orders a nuclear strike"]);
        let n = db.persisted.nuclear.clone().unwrap();
        assert!(matches!(n.phase, super::super::nuclear::Phase::Ordered));
        assert_eq!(n.cfg.aircraft, "MiG-21bis");
        assert_eq!(n.cfg.will_on_detonation, 20.);
        // a second order the same round changes nothing
        let again = db.order_nuclear_strike(&n.cfg, t0());
        assert!(again.contains("already ordered"));
        assert!(matches!(
            db.persisted.nuclear.as_ref().unwrap().phase,
            super::super::nuclear::Phase::Ordered
        ));
        // and the order survives a save and reload
        let saved = serde_json::to_string(&db.persisted.nuclear).unwrap();
        let back: Option<super::super::nuclear::NuclearState> =
            serde_json::from_str(&saved).unwrap();
        assert_eq!(back.unwrap().cfg.template, "RNUKE");
    }
}
