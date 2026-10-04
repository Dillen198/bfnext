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

//! What the side could do right now, and what each is worth.
//!
//! Every generator turns part of the plan into candidate operations with a
//! base value (roughly 0-100: how much the operation would help). The value
//! is then weighed by the plan's line weights, by how many humans are
//! already doing that job (`gap`), by the HQ's record with that kind of
//! operation, and by whether a player asked for it. `rank` returns the lot,
//! best value for money first; the caller buys down the list until the
//! budget or the per-pass limit runs out, skipping whatever fails to launch.

use super::{
    dist,
    picture::{Obj, Picture},
    requests::RequestStatus,
    strategy::Plan,
    Record,
};
use crate::{db::intel::IntelUnitClass, Context};
use bfprotocols::{
    cfg::{Action, ActionKind, EscortPolicy, HqCfg, TaskCfg, TaskTarget},
    db::{group::GroupId, objective::ObjectiveId},
    hq::{Line, OpKind, Posture},
};
use chrono::prelude::*;
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, String, Vector2};
use log::debug;
use smallvec::SmallVec;
use std::collections::BTreeMap;

/// Below this (after weighting) an operation is not worth the treasury.
const MIN_SCORE: f64 = 12.;
/// How strongly cost discounts value: 1 = pure value per point, 0 = cost
/// ignored. Pure value-per-point always buys the cheapest thing on the list.
const COST_EXPONENT: f64 = 0.6;
/// Air defence this close to a strike target makes the strike a bad bet
/// without SEAD.
const SAM_RISK_M: f64 = 15_000.;

#[derive(Debug, Clone)]
pub(crate) struct Avail {
    pub(crate) cost: i64,
    /// The side's action that runs it, for those that go through an action.
    pub(crate) action: Option<(String, Action)>,
    /// Every aircraft the side can fly for this job, for `pick_air` to
    /// choose from by situation (CAP, strike, SEAD).
    pub(crate) options: Vec<AirOption>,
}

/// One aircraft the HQ may send for a job, and when it is the one to send:
/// an entry of `HqCfg::air`, or one of the side's own actions (which suit
/// anything).
#[derive(Debug, Clone)]
pub(crate) struct AirOption {
    pub(crate) label: String,
    pub(crate) action: Action,
    pub(crate) weight: f64,
    pub(crate) min_threat: u32,
    pub(crate) max_threat: Option<u32>,
    pub(crate) max_air_defence: Option<u32>,
    pub(crate) night: bool,
    pub(crate) max_range_m: Option<f64>,
    pub(crate) targets: Vec<String>,
    pub(crate) helicopter: bool,
}

/// What a package is flying into.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Situation {
    /// Enemy aircraft our radars hold within 80 km of the target.
    pub(crate) threat: u32,
    /// Known air defence within 15 km of it.
    pub(crate) air_defence: u32,
    pub(crate) night: bool,
    /// Distance from the nearest friendly airbase, and from any friendly
    /// ground (where a helicopter can launch).
    pub(crate) from_airbase_m: f64,
    pub(crate) from_ground_m: f64,
    /// "armor" | "base" | "sam"
    pub(crate) target: &'static str,
}

impl Situation {
    pub(crate) fn at(pic: &Picture, pos: Vector2, night: bool, target: &'static str) -> Self {
        Self {
            threat: pic.enemy_air_near(pos, 80_000.),
            air_defence: pic.air_defence_near(pos, SAM_RISK_M),
            night,
            from_airbase_m: pic.air_gap(pos),
            from_ground_m: pic.gap(pos),
            target,
        }
    }
}

/// The aircraft to send into `sit`: of the options that fit it, the most
/// specialised for it -- the one built for the most enemy air it meets,
/// then one meant for this kind of target -- chosen by weight among equals.
/// None when nothing the side has fits (a helicopter into a SAM belt, a
/// day-only jet at night, a target past everyone's range).
pub(crate) fn pick_air<'a>(options: &'a [AirOption], sit: &Situation) -> Option<&'a AirOption> {
    use rand::seq::SliceRandom;
    let fits = |o: &&AirOption| {
        let range = o.max_range_m.unwrap_or(if o.helicopter { 120_000. } else { f64::INFINITY });
        let dist = if o.helicopter { sit.from_ground_m } else { sit.from_airbase_m };
        sit.threat >= o.min_threat
            && o.max_threat.map_or(true, |m| sit.threat <= m)
            && o.max_air_defence.map_or(true, |m| sit.air_defence <= m)
            && (o.night || !sit.night)
            && dist <= range
            && (o.targets.is_empty() || o.targets.iter().any(|t| t.eq_ignore_ascii_case(sit.target)))
    };
    let fit: Vec<&AirOption> = options.iter().filter(fits).collect();
    let rank = |o: &AirOption| (o.min_threat, !o.targets.is_empty() as u32);
    let best = fit.iter().map(|o| rank(o)).max()?;
    let top: Vec<&AirOption> = fit.into_iter().filter(|o| rank(o) == best).collect();
    top.choose_weighted(&mut rand::thread_rng(), |o| o.weight.max(0.01)).ok().copied()
}

#[derive(Debug, Clone)]
pub(crate) struct Cand {
    pub(crate) kind: OpKind,
    /// The objective it is about, if it is about one.
    pub(crate) target: Option<ObjectiveId>,
    /// Target or, for a point target, the nearest objective: what the
    /// per-target cooldown and "one op per target per pass" key on.
    pub(crate) anchor: ObjectiveId,
    pub(crate) name: CompactString,
    pub(crate) pos: Vector2,
    pub(crate) value: f64,
    pub(crate) score: f64,
    pub(crate) cost: i64,
    pub(crate) why: CompactString,
    pub(crate) request: Option<u64>,
    pub(crate) action: Option<(String, Action)>,
    pub(crate) shooters: SmallVec<[GroupId; 4]>,
    /// The JTAC whose target a bomber goes for.
    pub(crate) jtac: Option<crate::jtac::JtId>,
    /// Fighter escort flights to send with it, and the air defence to send
    /// SEAD at first (`HqPackagesCfg`). Their cost is in `cost`.
    pub(crate) escort: u32,
    pub(crate) sead_at: Option<Vector2>,
}

fn allowed(cfg: &HqCfg, side: Side, name: &str) -> bool {
    let names = match side {
        Side::Red => &cfg.actions_red,
        _ => &cfg.actions_blue,
    };
    names.is_empty() || names.iter().any(|n| n.as_str() == name)
}

fn scaled(cfg: &HqCfg, cost: i64) -> i64 {
    ((cost as f64) * cfg.cost_scale.max(0.)).round() as i64
}

/// The operations `side` has the means to run, and what each costs.
pub(crate) fn available(ctx: &Context, cfg: &HqCfg, side: Side) -> BTreeMap<OpKind, Avail> {
    let mut out = BTreeMap::new();
    let c = &cfg.costs;
    // Air rosters: the HQ's own entries, then the side's actions of the
    // same kind. `pick_air` chooses from these per package.
    let mut rosters: BTreeMap<OpKind, Vec<AirOption>> = BTreeMap::new();
    if let Some(air) = cfg.air.get(&side) {
        for (kind, list) in [(OpKind::Cap, &air.cap), (OpKind::Strike, &air.strike), (OpKind::Sead, &air.sead)] {
            for t in list {
                let helicopter = matches!(t.plane.kind, bfprotocols::cfg::AiPlaneKind::Helicopter);
                let akind = match kind {
                    OpKind::Cap => ActionKind::Fighters(t.plane.clone()),
                    OpKind::Strike => ActionKind::Attackers(t.plane.clone()),
                    _ => ActionKind::Sead(t.plane.clone()),
                };
                rosters.entry(kind).or_default().push(AirOption {
                    label: t.label.clone().unwrap_or_else(|| t.plane.template.clone()),
                    action: Action { kind: akind, cost: 0, penalty: None, limit: None, geo_limit: Default::default() },
                    weight: t.weight,
                    min_threat: t.min_threat,
                    max_threat: t.max_threat,
                    max_air_defence: t.max_air_defence,
                    night: t.night,
                    max_range_m: t.max_range_m,
                    targets: t.targets.clone(),
                    helicopter,
                });
            }
        }
    }
    if let Some(actions) = ctx.db.ephemeral.cfg.actions.get(&side) {
        for (name, a) in actions.iter() {
            if !allowed(cfg, side, name.as_str()) {
                continue;
            }
            let (kind, plane) = match &a.kind {
                ActionKind::Fighters(p) => (OpKind::Cap, p),
                ActionKind::Attackers(p) => (OpKind::Strike, p),
                ActionKind::Sead(p) => (OpKind::Sead, p),
                _ => continue,
            };
            rosters.entry(kind).or_default().push(AirOption {
                label: name.clone(),
                action: a.clone(),
                weight: 1.,
                min_threat: 0,
                max_threat: None,
                max_air_defence: None,
                night: true,
                max_range_m: None,
                targets: vec![],
                helicopter: matches!(plane.kind, bfprotocols::cfg::AiPlaneKind::Helicopter),
            });
        }
    }
    for (kind, cost) in [(OpKind::Cap, c.cap), (OpKind::Strike, c.strike), (OpKind::Sead, c.sead)] {
        if let Some(opts) = rosters.remove(&kind) {
            let first = opts.first().map(|o| (String::from(o.label.as_str()), o.action.clone()));
            out.insert(kind, Avail { cost: scaled(cfg, cost), action: first, options: opts });
        }
    }
    let mut put = |k: OpKind, cost: i64, action: Option<(String, Action)>| {
        out.entry(k).or_insert(Avail { cost: scaled(cfg, cost), action, options: vec![] });
    };
    if let Some(actions) = ctx.db.ephemeral.cfg.actions.get(&side) {
        for (name, a) in actions.iter() {
            if !allowed(cfg, side, name.as_str()) {
                continue;
            }
            let pick = Some((name.clone(), a.clone()));
            match &a.kind {
                ActionKind::Recon(_) | ActionKind::Drone(_) => put(OpKind::Recon, c.recon, pick),
                ActionKind::Artillery(_) => put(OpKind::Artillery, c.artillery, pick),
                ActionKind::Reinforce(_) => put(OpKind::Reinforce, c.reinforce, pick),
                ActionKind::Bomber(_) => put(OpKind::Bomber, c.bomber, pick),
                ActionKind::Awacs(_) => put(OpKind::Awacs, c.awacs, pick),
                ActionKind::Tanker(_) => put(OpKind::Tanker, c.tanker, pick),
                ActionKind::NavalCruiseMissileStrike(_) => put(OpKind::NavalStrike, c.naval_strike, pick),
                ActionKind::LogisticsRepair(_) => put(OpKind::AirRepair, c.air_repair, pick),
                _ => (),
            }
        }
    }
    let cfgs = &ctx.db.ephemeral.cfg;
    // No Artillery action: the campaign's own fire-mission config (the one
    // F10 "Request Fires" uses) still lets the HQ task its batteries.
    if cfgs.artillery.is_some() {
        put(OpKind::Artillery, c.artillery, None);
    }
    if cfgs.campaign_events.as_ref().map_or(false, |e| e.enabled) {
        put(OpKind::MissileStrike, c.missile_strike, None);
        put(OpKind::Ambush, c.ambush, None);
    }
    if cfgs
        .warehouse
        .as_ref()
        .and_then(|w| w.convoy.as_ref())
        .map_or(false, |c| c.enabled)
    {
        put(OpKind::Convoy, c.convoy, None);
    }
    if cfgs.helo_insertion.is_some() {
        put(OpKind::HeloSupply, c.helo_supply, None);
        put(OpKind::HeloTroops, c.helo_troops, None);
    }
    out
}

/// The side's tasking-board config, borrowed from its AddTask action.
pub(crate) fn task_cfg(ctx: &Context, side: Side) -> Option<TaskCfg> {
    ctx.db.ephemeral.cfg.actions.get(&side)?.values().find_map(|a| match &a.kind {
        ActionKind::AddTask(t) => Some(t.clone()),
        _ => None,
    })
}

struct Gen<'a> {
    cfg: &'a HqCfg,
    pic: &'a Picture,
    plan: &'a Plan,
    avail: &'a BTreeMap<OpKind, Avail>,
    out: Vec<Cand>,
}

impl<'a> Gen<'a> {
    fn anchor(&self, p: Vector2) -> Option<ObjectiveId> {
        self.pic
            .objs
            .iter()
            .min_by(|a, b| dist(a.pos, p).total_cmp(&dist(b.pos, p)))
            .map(|o| o.id)
    }

    fn push(&mut self, kind: OpKind, target: Option<&Obj>, pos: Vector2, name: CompactString, value: f64, why: CompactString) {
        let Some(av) = self.avail.get(&kind) else { return };
        let Some(anchor) = target.map(|o| o.id).or_else(|| self.anchor(pos)) else { return };
        if value <= 0. {
            return;
        }
        self.out.push(Cand {
            kind,
            target: target.map(|o| o.id),
            anchor,
            name,
            pos,
            value,
            score: 0.,
            cost: av.cost,
            why,
            request: None,
            action: av.action.clone(),
            shooters: SmallVec::new(),
            jtac: None,
            escort: 0,
            sead_at: None,
        });
    }

    fn main_effort(&self) -> Option<&'a Obj> {
        self.plan.main_effort.and_then(|id| self.pic.get(&id))
    }

    /// The friendly base the main effort is pushed from.
    fn staging(&self) -> Option<&'a Obj> {
        let me = self.main_effort()?;
        self.pic.own().min_by(|a, b| dist(a.pos, me.pos).total_cmp(&dist(b.pos, me.pos)))
    }

    fn in_air_range(&self, p: Vector2) -> bool {
        self.pic.air_gap(p) <= self.cfg.max_air_range_m
    }

    fn air(&mut self) {
        let pic = self.pic;
        // CAP over the bases we are holding and the one we stage from, where
        // the radars show enemy aircraft about.
        let mut cover: Vec<&Obj> = self.plan.defend.iter().filter_map(|id| pic.get(id)).collect();
        if let Some(s) = self.staging() {
            if !cover.iter().any(|o| o.id == s.id) {
                cover.push(s);
            }
        }
        for o in cover {
            if !self.in_air_range(o.pos) {
                continue;
            }
            let near = pic.enemy_air_near(o.pos, 80_000.);
            let value = if near > 0 {
                35. + 12. * near.min(6) as f64
            } else if o.being_captured {
                18.
            } else {
                0.
            };
            self.push(
                OpKind::Cap,
                Some(o),
                o.pos,
                o.name.clone(),
                value,
                format_compact!("{near} enemy aircraft within 80 km of {}", o.name),
            );
        }

        // Strike the main effort, unless its air defence would eat the flight.
        let sead_up = pic.under_way.iter().any(|(k, _)| *k == OpKind::Sead);
        if let Some(me) = self.main_effort() {
            if self.in_air_range(me.pos) {
                let ad = pic.air_defence_near(me.pos, SAM_RISK_M);
                let risk = if ad > 0 && !sead_up { 0.35 } else { 1. };
                let value = 45. * (1. + (100 - me.health.min(100)) as f64 / 200.) * risk;
                self.push(
                    OpKind::Strike,
                    Some(me),
                    me.pos,
                    me.name.clone(),
                    value,
                    format_compact!("main effort{}", if ad > 0 { ", air defence known nearby" } else { "" }),
                );
            }
        }
        // ... and whatever enemy ground our intel holds closing on a base we
        // are holding.
        let defended: Vec<&Obj> = pic.own().filter(|o| o.threatened || o.being_captured).collect();
        for c in pic.enemy_ground.iter() {
            if !matches!(c.class, IntelUnitClass::Armor | IntelUnitClass::Artillery | IntelUnitClass::Infantry) {
                continue;
            }
            let Some(base) = defended.iter().find(|o| dist(o.pos, c.pos) <= 20_000.) else { continue };
            if !self.in_air_range(c.pos) || c.age_mins > 20 {
                continue;
            }
            let value = (40. + 6. * c.count as f64).min(65.);
            self.push(
                OpKind::Strike,
                None,
                c.pos,
                format_compact!("{} x{} near {}", c.class.label(), c.count, base.name),
                value,
                format_compact!("enemy ground closing on {}", base.name),
            );
        }
        // Close air support for our formations: any enemy formation in sight
        // that is closing on a base we hold or fighting one of our
        // formations.
        for p in pic.enemy_formations.iter() {
            if !self.in_air_range(*p) {
                continue;
            }
            let base = defended.iter().find(|o| dist(o.pos, *p) <= 20_000.);
            let battle = pic.battles.iter().any(|b| dist(*b, *p) <= 8_000.);
            let (value, why) = match (base, battle) {
                (_, true) => (55., CompactString::from("enemy formation fighting ours")),
                (Some(b), false) => (50., format_compact!("enemy formation closing on {}", b.name)),
                (None, false) => continue,
            };
            let near = pic
                .objs
                .iter()
                .min_by(|a, b| dist(a.pos, *p).total_cmp(&dist(b.pos, *p)))
                .map(|o| o.name.clone())
                .unwrap_or_default();
            self.push(OpKind::Strike, None, *p, format_compact!("enemy formation near {near}"), value, why);
        }
        // A JTAC drone over every battle we are in that has no JTAC on it:
        // the bombers need someone lasing for them.
        for b in pic.battles.iter() {
            let covered = pic.jtac_targets.iter().any(|(_, t)| dist(*t, *b) <= 10_000.);
            if covered || !self.in_air_range(*b) {
                continue;
            }
            self.push(OpKind::Recon, None, *b, "battle area".into(), 34., "no JTAC over our battle".into());
        }

        // SEAD whatever air defence covers the main effort.
        if let Some(me) = self.main_effort() {
            for c in pic.sams().filter(|c| dist(c.pos, me.pos) <= 50_000.) {
                if !self.in_air_range(c.pos) {
                    continue;
                }
                self.push(
                    OpKind::Sead,
                    None,
                    c.pos,
                    format_compact!("air defence near {}", me.name),
                    40. + 20. / (1. + dist(c.pos, me.pos) / 15_000.),
                    format_compact!("covers the main effort {}", me.name),
                );
            }
            for site in pic.enemy().filter(|o| o.sam_site() && dist(o.pos, me.pos) <= 50_000.) {
                if !self.in_air_range(site.pos) {
                    continue;
                }
                self.push(
                    OpKind::Sead,
                    Some(site),
                    site.pos,
                    site.name.clone(),
                    45. + 20. / (1. + dist(site.pos, me.pos) / 15_000.),
                    format_compact!("SAM site covering {}", me.name),
                );
            }
        }

        // Recon where the side is blind: the main effort, and the base most
        // under threat.
        let blind = |p: Vector2| !pic.enemy_ground.iter().any(|c| dist(c.pos, p) <= 8_000. && c.age_mins < 30);
        if let Some(me) = self.main_effort() {
            if blind(me.pos) && self.in_air_range(me.pos) {
                self.push(
                    OpKind::Recon,
                    Some(me),
                    me.pos,
                    me.name.clone(),
                    30.,
                    "no fresh intel on the main effort".into(),
                );
            }
        }
        if let Some(d) = self.plan.defend.first().and_then(|id| pic.get(id)) {
            if d.threatened && blind(d.pos) && self.in_air_range(d.pos) {
                self.push(
                    OpKind::Recon,
                    Some(d),
                    d.pos,
                    d.name.clone(),
                    24.,
                    format_compact!("{} is threatened by something we can't see", d.name),
                );
            }
        }
    }

    fn fires(&mut self, ctx: &Context) {
        let pic = self.pic;
        let batteries: Vec<&Obj> = pic.own().filter(|o| o.fires).collect();
        let reach = ctx.db.ephemeral.cfg.artillery_mission_range as f64;
        let reach = if reach > 0. { reach.min(40_000.) } else { 30_000. };
        let covered = |p: Vector2| batteries.iter().any(|b| dist(b.pos, p) <= reach && dist(b.pos, p) >= 5_000.);
        let me = self.plan.main_effort;
        for o in pic.enemy().filter(|o| !o.at_sea()) {
            if !covered(o.pos) {
                continue;
            }
            let weak = (100 - o.health.min(100)) as f64 * 0.3;
            let value = if Some(o.id) == me { 45. + weak } else { 20. + weak + if o.capturable { 15. } else { 0. } };
            self.push(OpKind::Artillery, Some(o), o.pos, o.name.clone(), value, "in range of our guns".into());
        }
        for c in pic.enemy_ground.iter().filter(|c| c.age_mins <= 15) {
            if !covered(c.pos) || c.class == IntelUnitClass::AirBase {
                continue;
            }
            let near_own = pic.gap(c.pos) <= 15_000.;
            let value = 25. + 5. * c.count.min(6) as f64 + if near_own { 15. } else { 0. };
            self.push(
                OpKind::Artillery,
                None,
                c.pos,
                format_compact!("{} x{}", c.class.label(), c.count),
                value,
                "fresh intel contact in range".into(),
            );
        }

        // Missiles go deep, at what hurts most.
        if !pic.missile_groups.is_empty() {
            let range = ctx.db.ephemeral.cfg.alcm_mission_range as f64;
            for o in pic.enemy().filter(|o| !o.at_sea()) {
                let shooters: SmallVec<[GroupId; 4]> = pic
                    .missile_groups
                    .iter()
                    .filter(|(_, p)| dist(*p, o.pos) <= range)
                    .map(|(g, _)| *g)
                    .collect();
                if shooters.is_empty() {
                    continue;
                }
                let strategic = matches!(
                    o.kind,
                    bfprotocols::db::objective::ObjectiveKind::Logistics
                        | bfprotocols::db::objective::ObjectiveKind::Factory { .. }
                        | bfprotocols::db::objective::ObjectiveKind::Airbase
                );
                if !strategic && Some(o.id) != me {
                    continue;
                }
                let value = 35. * o.value() + if Some(o.id) == me { 20. } else { 0. };
                let n = self.out.len();
                self.push(OpKind::MissileStrike, Some(o), o.pos, o.name.clone(), value, "high-value target in missile range".into());
                if let Some(c) = self.out.get_mut(n) {
                    c.shooters = shooters;
                }
            }
        }

        if pic.enemy_convoys_near > 0 {
            if let Some(anchor) = pic.own().min_by(|a, b| a.front_m.total_cmp(&b.front_m)) {
                let value = 28. + 12. * pic.enemy_convoys_near.min(4) as f64;
                self.push(
                    OpKind::Ambush,
                    None,
                    anchor.pos,
                    "enemy supply route".into(),
                    value,
                    format_compact!("{} enemy convoy(s) on roads near the front", pic.enemy_convoys_near),
                );
            }
        }
    }

    /// Heavy bombers go for what our JTACs are lasing: at the main effort
    /// first, then at whatever is closing on a base we are holding.
    fn bombers(&mut self) {
        let pic = self.pic;
        let me = self.main_effort();
        let defended: Vec<&Obj> = pic.own().filter(|o| o.threatened || o.being_captured).collect();
        for (jt, pos) in pic.jtac_targets.iter() {
            if !self.in_air_range(*pos) {
                continue;
            }
            let at_effort = me.filter(|m| dist(m.pos, *pos) <= 25_000.);
            let (value, why) = match (at_effort, defended.iter().find(|o| dist(o.pos, *pos) <= 20_000.)) {
                (Some(m), _) => (60., format_compact!("JTAC target at the main effort {}", m.name)),
                (_, Some(d)) => (55., format_compact!("JTAC target closing on {}", d.name)),
                _ if pic.gap(*pos) <= self.cfg.max_ground_range_m => (35., CompactString::from("JTAC target near the front")),
                _ => continue,
            };
            let name = match at_effort {
                Some(m) => format_compact!("JTAC target at {}", m.name),
                None => CompactString::from("JTAC target"),
            };
            let n = self.out.len();
            self.push(OpKind::Bomber, None, *pos, name, value, why);
            if let Some(c) = self.out.get_mut(n) {
                c.jtac = Some(jt.clone());
            }
        }
    }

    /// AWACS and tankers on station behind the front, while there is an air
    /// war to support.
    fn support(&mut self, ctx: &Context, side: Side) {
        let pic = self.pic;
        let air_ops = ctx
            .hq
            .sides
            .get(&side)
            .map_or(0, |rt| rt.active().filter(|o| o.kind.line() == Line::Air).count()) as u32;
        let enemy_air = pic.enemy_air.len() as u32;
        if enemy_air > 0 || pic.humans_fw_air > 0 || air_ops > 0 {
            if let Some(pos) = pic.station_behind_front(80_000.) {
                if self.in_air_range(pos) {
                    self.push(
                        OpKind::Awacs,
                        None,
                        pos,
                        "AWACS station".into(),
                        30. + 10. * enemy_air.min(4) as f64,
                        format_compact!("{enemy_air} enemy aircraft on radar, {} of ours up", pic.humans_fw_air + air_ops),
                    );
                }
            }
        }
        let thirsty = pic.humans_fw_air + air_ops;
        if thirsty >= 2 {
            if let Some(pos) = pic.station_behind_front(50_000.) {
                if self.in_air_range(pos) {
                    self.push(
                        OpKind::Tanker,
                        None,
                        pos,
                        "tanker track".into(),
                        20. + 8. * thirsty.min(5) as f64,
                        format_compact!("{thirsty} friendly jets up"),
                    );
                }
            }
        }
    }

    /// Cruise missiles from a carrier group, at what hurts most.
    fn naval(&mut self) {
        let pic = self.pic;
        let range = self.avail.get(&OpKind::NavalStrike).and_then(|a| match a.action.as_ref().map(|(_, a)| &a.kind) {
            Some(ActionKind::NavalCruiseMissileStrike(c)) => Some(c.max_range as f64),
            _ => None,
        });
        let Some(range) = range else { return };
        if pic.carriers.is_empty() {
            return;
        }
        let me = self.plan.main_effort;
        for o in pic.enemy().filter(|o| !o.at_sea()) {
            if !pic.carriers.iter().any(|c| dist(*c, o.pos) <= range) {
                continue;
            }
            let strategic = matches!(
                o.kind,
                bfprotocols::db::objective::ObjectiveKind::Logistics
                    | bfprotocols::db::objective::ObjectiveKind::Factory { .. }
                    | bfprotocols::db::objective::ObjectiveKind::Airbase
            );
            if !strategic && Some(o.id) != me {
                continue;
            }
            let value = 40. * o.value() + if Some(o.id) == me { 20. } else { 0. };
            self.push(OpKind::NavalStrike, Some(o), o.pos, o.name.clone(), value, "in range of our carrier group".into());
        }
    }

    fn logistics(&mut self) {
        let pic = self.pic;
        for (rank, id) in self.plan.supply.iter().enumerate() {
            let Some(o) = pic.get(id) else { continue };
            if o.inbound || o.stores() >= self.cfg.resupply_below_pct {
                continue;
            }
            let need = (100 - o.stores().min(100)) as f64;
            let mut value = 20. + 0.7 * need;
            if self.plan.defend.contains(id) {
                value *= 1.4;
            }
            if o.being_captured || o.front_m < 25_000. {
                value *= 1.25;
            }
            // The list is in priority order already.
            value *= 1. - (rank as f64 * 0.05).min(0.3);
            let why = format_compact!("supply {}%, fuel {}%", o.supply, o.fuel);
            self.push(OpKind::Convoy, Some(o), o.pos, o.name.clone(), value, why.clone());
            // A helo run is faster and does not care about a cut road, but
            // carries less: a little less value for more money.
            self.push(OpKind::HeloSupply, Some(o), o.pos, o.name.clone(), value * 0.9, why);
        }
        // Logistics flown in by air to bases whose logistics are shot up.
        for o in pic.own().filter(|o| o.logi < 60 && !o.at_sea() && !o.being_captured) {
            let mut value = 25. + 0.5 * (100 - o.logi.min(100)) as f64;
            if self.plan.defend.contains(&o.id) {
                value *= 1.4;
            }
            self.push(OpKind::AirRepair, Some(o), o.pos, o.name.clone(), value, format_compact!("logistics at {}%", o.logi));
        }
    }

    fn troops(&mut self, ctx: &Context) {
        let pic = self.pic;
        let helo_range = ctx
            .db
            .ephemeral
            .cfg
            .helo_insertion
            .as_ref()
            .map(|h| h.max_range_m)
            .unwrap_or(0.);
        let offensive = self.plan.posture != Posture::Defensive;
        let me = self.plan.main_effort;
        if helo_range > 0. {
            for o in pic.enemy().filter(|o| o.capturable && !o.at_sea() && !o.sam_site()) {
                if pic.gap(o.pos) > helo_range.min(self.cfg.max_ground_range_m * 1.5) {
                    continue;
                }
                let mut value = 45. * o.value();
                if Some(o.id) == me {
                    value *= 1.5;
                }
                if offensive {
                    value *= 1.2;
                }
                self.push(OpKind::HeloTroops, Some(o), o.pos, o.name.clone(), value, "no logistics left, troops will take it".into());
            }
            for o in pic.own().filter(|o| o.capturable && (o.being_captured || o.threatened)) {
                self.push(OpKind::HeloTroops, Some(o), o.pos, o.name.clone(), 50., "garrison gone, troops to hold it".into());
            }
        }
        // Reinforcement convoys rebuild the garrisons we are holding and
        // staging from.
        let mut bases: Vec<&Obj> = self.plan.defend.iter().filter_map(|id| pic.get(id)).collect();
        if let Some(s) = self.staging() {
            if !bases.iter().any(|o| o.id == s.id) {
                bases.push(s);
            }
        }
        for o in bases {
            let n = ctx.db.reinforce_candidates(&o.id).map(|v| v.len()).unwrap_or(0);
            if n == 0 {
                continue;
            }
            let value = 28. + 8. * n.min(4) as f64 + if o.threatened { 15. } else { 0. };
            self.push(OpKind::Reinforce, Some(o), o.pos, o.name.clone(), value, format_compact!("{n} garrison group(s) to rebuild"));
        }
    }

    /// One candidate per answerable kind for each open request.
    fn requests(&mut self, ctx: &Context, side: Side, bonus: f64) {
        let Some(rt) = ctx.hq.sides.get(&side) else { return };
        for r in rt.requests.iter().filter(|r| r.status == RequestStatus::Open) {
            let Some(o) = self.pic.get(&r.target) else { continue };
            for kind in r.kind.answered_by() {
                // An existing candidate for the same thing just gets the bonus.
                let mut found = false;
                for c in self.out.iter_mut().filter(|c| c.kind == *kind && c.anchor == r.target) {
                    c.value *= bonus;
                    c.request = Some(r.id);
                    found = true;
                }
                if found {
                    continue;
                }
                // A bomber needs something a JTAC of ours is lasing there.
                let jtac = if *kind == OpKind::Bomber {
                    match self
                        .pic
                        .jtac_targets
                        .iter()
                        .filter(|(_, p)| dist(*p, o.pos) <= 25_000.)
                        .min_by(|a, b| dist(a.1, o.pos).total_cmp(&dist(b.1, o.pos)))
                    {
                        Some(t) => Some(t.clone()),
                        None => continue,
                    }
                } else {
                    None
                };
                let n = self.out.len();
                self.push(
                    *kind,
                    Some(o),
                    jtac.as_ref().map_or(o.pos, |(_, p)| *p),
                    o.name.clone(),
                    30. * bonus,
                    format_compact!("{} asked for {}", r.by, r.kind.label()),
                );
                if let Some(c) = self.out.get_mut(n) {
                    c.request = Some(r.id);
                    c.jtac = jtac.map(|(j, _)| j);
                    if *kind == OpKind::MissileStrike {
                        let range = ctx.db.ephemeral.cfg.alcm_mission_range as f64;
                        c.shooters = self
                            .pic
                            .missile_groups
                            .iter()
                            .filter(|(_, p)| dist(*p, o.pos) <= range)
                            .map(|(g, _)| *g)
                            .collect();
                    }
                }
            }
        }
    }
}

/// Share of a line's full effort the HQ puts in, given the humans already
/// doing it.
pub(crate) fn line_gap(cfg: &HqCfg, pic: &Picture, gap: f64, line: Line) -> f64 {
    let cut = |n: u32| (1. - cfg.gap_fill.per_human_cut * n as f64).max(0.15);
    match line {
        Line::Air => gap * cut(pic.humans_fw_air),
        Line::Logistics | Line::Troops => gap * cut(pic.humans_helo_air),
        Line::Fires | Line::Ground => gap,
    }
}

/// Every operation worth considering this pass, best first.
#[allow(clippy::too_many_arguments)]
pub(crate) fn rank(
    ctx: &Context,
    cfg: &HqCfg,
    side: Side,
    pic: &Picture,
    plan: &Plan,
    record: &BTreeMap<OpKind, Record>,
    gap: f64,
    now: DateTime<Utc>,
) -> Vec<Cand> {
    let avail = available(ctx, cfg, side);
    let mut g = Gen { cfg, pic, plan, avail: &avail, out: vec![] };
    g.air();
    g.bombers();
    g.support(ctx, side);
    g.fires(ctx);
    g.naval();
    g.logistics();
    g.troops(ctx);
    if cfg.requests.enabled {
        g.requests(ctx, side, cfg.requests.bonus.max(1.));
    }
    // Strikes fly as packages: fighters escorting them, SEAD ahead of them
    // where our intel holds air defence near the target. What the side
    // cannot fly goes without -- an unescorted bomber is still a bomber.
    let escort_cost = avail.get(&OpKind::Cap).filter(|a| a.action.is_some()).map(|a| a.cost);
    let sead_cost = avail.get(&OpKind::Sead).filter(|a| a.action.is_some()).map(|a| a.cost);
    for c in g.out.iter_mut() {
        let policy = match c.kind {
            OpKind::Bomber => cfg.packages.escort_bombers,
            OpKind::Strike => cfg.packages.escort_strikes,
            _ => continue,
        };
        let threatened = pic.enemy_air_near(c.pos, 120_000.) > 0;
        let escort = match policy {
            EscortPolicy::Always => true,
            EscortPolicy::Threatened => threatened,
            EscortPolicy::Never => false,
        };
        if let (true, Some(cost)) = (escort, escort_cost) {
            c.escort = cfg.packages.escort_flights.max(1);
            c.cost += cost * c.escort as i64;
        }
        if !cfg.packages.sead_with_strikes {
            continue;
        }
        let sam = pic
            .sams()
            .map(|s| s.pos)
            .chain(pic.enemy().filter(|o| o.sam_site()).map(|o| o.pos))
            .filter(|p| dist(*p, c.pos) <= SAM_RISK_M)
            .min_by(|a, b| dist(*a, c.pos).total_cmp(&dist(*b, c.pos)));
        if let (Some(cost), Some(sam)) = (sead_cost, sam) {
            c.sead_at = Some(sam);
            c.cost += cost;
        }
    }
    // The right aircraft for each air job, for where it is going: a job
    // nothing the side has fits is dropped.
    let night = ctx.hq.night;
    g.out.retain_mut(|c| {
        let target = match c.kind {
            OpKind::Strike if c.target.is_some() => "base",
            OpKind::Strike => "armor",
            OpKind::Sead => "sam",
            OpKind::Cap => "base",
            _ => return true,
        };
        let Some(av) = avail.get(&c.kind) else { return true };
        if av.options.is_empty() {
            return true;
        }
        let sit = Situation::at(pic, c.pos, night, target);
        match pick_air(&av.options, &sit) {
            Some(o) => {
                c.action = Some((String::from(o.label.as_str()), o.action.clone()));
                c.why.push_str(&format_compact!(" [{}]", o.label));
                true
            }
            None => {
                debug!(
                    "hq: {side:?} nothing fits a {} on {} (threat {}, air defence {}, night {night})",
                    c.kind.label(),
                    c.name,
                    sit.threat,
                    sit.air_defence
                );
                false
            }
        }
    });
    let rt = ctx.hq.sides.get(&side);
    let cooldown = chrono::Duration::seconds(cfg.limits.fires_cooldown_secs as i64);
    let mut out: Vec<Cand> = g
        .out
        .into_iter()
        .filter(|c| !plan.disabled.contains(&c.kind))
        .filter(|c| !plan.avoided(c.target) && !plan.avoided(Some(c.anchor)))
        .filter(|c| !pic.busy(c.kind, c.target))
        .filter(|c| {
            c.kind.line() != Line::Fires
                || rt
                    .and_then(|rt| rt.last_fired.get(&c.anchor))
                    .map_or(true, |t| now - *t >= cooldown)
        })
        .map(|mut c| {
            let line = c.kind.line();
            let mut gap = line_gap(cfg, pic, gap, line);
            if c.request.is_some() {
                // A human asked: they are the one doing the job, and they
                // need help with it.
                gap = gap.max(0.8);
            }
            let trust = record.get(&c.kind).copied().unwrap_or_default().trust();
            let effort = if c.target.is_some() && c.target == plan.main_effort { 1.25 } else { 1. };
            c.score = c.value * plan.weight(line) * gap * trust * effort;
            c
        })
        .filter(|c| c.score >= MIN_SCORE)
        .collect();
    out.sort_by(|a, b| {
        let ea = a.score / (a.cost.max(1) as f64).powf(COST_EXPONENT);
        let eb = b.score / (b.cost.max(1) as f64).powf(COST_EXPONENT);
        eb.total_cmp(&ea)
    });
    debug!("hq: {side:?} {} candidate operation(s)", out.len());
    out
}

/// How many of `kind`'s line the side may still start, against the limits.
pub(crate) fn room(cfg: &HqCfg, ctx: &Context, side: Side, kind: OpKind) -> u32 {
    let Some(rt) = ctx.hq.sides.get(&side) else { return u32::MAX };
    let count = |pred: &dyn Fn(OpKind) -> bool| rt.active().filter(|o| pred(o.kind)).count() as u32;
    let l = &cfg.limits;
    match kind {
        OpKind::Cap | OpKind::Strike | OpKind::Sead => {
            l.air.saturating_sub(count(&|k| matches!(k, OpKind::Cap | OpKind::Strike | OpKind::Sead)))
        }
        OpKind::Recon => l.recon.saturating_sub(count(&|k| k == OpKind::Recon)),
        OpKind::Convoy | OpKind::HeloSupply => l.logistics.saturating_sub(count(&|k| {
            matches!(k, OpKind::Convoy | OpKind::HeloSupply | OpKind::AirRepair)
        })),
        OpKind::HeloTroops => l.troops.saturating_sub(count(&|k| k == OpKind::HeloTroops)),
        OpKind::Reinforce => l.reinforce.saturating_sub(count(&|k| k == OpKind::Reinforce)),
        OpKind::Bomber => l.bomber.saturating_sub(count(&|k| k == OpKind::Bomber)),
        OpKind::Awacs => l.awacs.saturating_sub(count(&|k| k == OpKind::Awacs)),
        OpKind::Tanker => l.tanker.saturating_sub(count(&|k| k == OpKind::Tanker)),
        OpKind::AirRepair => l.logistics.saturating_sub(count(&|k| {
            matches!(k, OpKind::Convoy | OpKind::HeloSupply | OpKind::AirRepair)
        })),
        OpKind::Artillery | OpKind::MissileStrike | OpKind::Ambush | OpKind::NavalStrike => u32::MAX,
    }
}

/// Keep the HQ's asks on the coalition tasking board: take the main effort,
/// supply the base that needs it most, CAS on the main effort and CAP over
/// the base most under air threat -- whichever of those the side's board has
/// task types for.
pub(crate) fn post_tasks(ctx: &mut Context, cfg: &HqCfg, side: Side, pic: &Picture, plan: &Plan, now: DateTime<Utc>) {
    let Some(tcfg) = task_cfg(ctx, side) else { return };
    // Forget the ones that have come off the board.
    let open: Vec<_> = {
        let rt = ctx.hq.side(side);
        let tasks = std::mem::take(&mut rt.tasks);
        tasks.into_iter().filter(|id| ctx.db.persisted.tasks.get(id).is_some()).collect()
    };
    ctx.hq.side(side).tasks = open;
    let mut room = (cfg.max_tasks as usize).saturating_sub(ctx.hq.side(side).tasks.len());
    if room == 0 {
        return;
    }
    let by_target = |f: fn(&TaskTarget) -> bool| tcfg.types.iter().find(|t| f(&t.target)).map(|t| t.name.clone());
    let by_name = |n: &str| {
        tcfg.types
            .iter()
            .find(|t| t.name.eq_ignore_ascii_case(n) && matches!(t.target, TaskTarget::Position))
            .map(|t| t.name.clone())
    };
    let mut wants: Vec<(String, Option<ObjectiveId>, Vector2)> = vec![];
    if let Some(me) = plan.main_effort.and_then(|id| pic.get(&id)) {
        if plan.posture != Posture::Defensive {
            if let Some(k) = by_target(|t| matches!(t, TaskTarget::CaptureObjective)) {
                wants.push((k, Some(me.id), me.pos));
            }
        }
        if let Some(k) = by_name("CAS") {
            wants.push((k, None, me.pos));
        }
    }
    if let Some(s) = plan.supply.first().and_then(|id| pic.get(id)) {
        if let Some(k) = by_target(|t| matches!(t, TaskTarget::SupplyObjective { .. })) {
            wants.push((k, Some(s.id), s.pos));
        }
    }
    if let Some(d) = plan
        .defend
        .iter()
        .filter_map(|id| pic.get(id))
        .find(|o| pic.enemy_air_near(o.pos, 80_000.) > 0)
    {
        if let Some(k) = by_name("CAP") {
            wants.push((k, None, d.pos));
        }
    }
    for (kind, oid, pos) in wants {
        if room == 0 {
            break;
        }
        // Don't re-post a position task the board already has nearby.
        let dup = ctx
            .db
            .tasks(side)
            .any(|t| t.kind == kind && (t.oid == oid && oid.is_some() || dist(t.pos, pos) < 10_000.));
        if dup {
            continue;
        }
        let res = match oid {
            Some(oid) => ctx.db.add_objective_task(&tcfg, side, None, kind.as_str(), oid, now),
            None => ctx.db.add_task(&tcfg, side, None, kind.as_str(), pos, now),
        };
        match res {
            Ok(id) => {
                ctx.hq.side(side).tasks.push(id);
                room -= 1;
            }
            Err(e) => debug!("hq: {side:?} could not post {kind} task: {e:?}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn opt(label: &str, helicopter: bool) -> AirOption {
        let plane: bfprotocols::cfg::AiPlaneCfg = serde_json::from_value(serde_json::json!({
            "kind": if helicopter { "Helicopter" } else { "FixedWing" },
            "duration": 3600, "template": label, "altitude": 5000, "altitude_typ": "BARO", "speed": 200
        }))
        .unwrap();
        AirOption {
            label: label.into(),
            action: Action {
                kind: ActionKind::Attackers(plane),
                cost: 0,
                penalty: None,
                limit: None,
                geo_limit: Default::default(),
            },
            weight: 1.,
            min_threat: 0,
            max_threat: None,
            max_air_defence: None,
            night: true,
            max_range_m: None,
            targets: vec![],
            helicopter,
        }
    }

    fn sit(threat: u32, air_defence: u32, night: bool, km: f64) -> Situation {
        Situation {
            threat,
            air_defence,
            night,
            from_airbase_m: km * 1000.,
            from_ground_m: km * 1000.,
            target: "armor",
        }
    }

    #[test]
    fn the_heavy_fighters_go_when_the_threat_is_big() {
        let light = AirOption { max_threat: Some(2), ..opt("F-16 pair", false) };
        let heavy = AirOption { min_threat: 3, ..opt("F-15C four-ship", false) };
        let opts = [light, heavy];
        assert_eq!(pick_air(&opts, &sit(0, 0, false, 50.)).unwrap().label.as_str(), "F-16 pair");
        assert_eq!(pick_air(&opts, &sit(5, 0, false, 50.)).unwrap().label.as_str(), "F-15C four-ship");
    }

    #[test]
    fn helicopters_stay_out_of_air_defence_and_out_of_range() {
        let helo = AirOption { max_air_defence: Some(0), ..opt("Ka-50", true) };
        let jet = AirOption { targets: vec!["base".into()], ..opt("Su-25T", false) };
        let opts = [helo, jet];
        // Armour near the front, nothing defending it: the helicopter.
        assert_eq!(pick_air(&opts, &sit(0, 0, false, 40.)).unwrap().label.as_str(), "Ka-50");
        // A SAM over it: the jet is the only thing that fits, and it is for
        // bases only, so nothing goes.
        assert!(pick_air(&opts, &sit(0, 1, false, 40.)).is_none());
        // Too far for a helicopter.
        assert!(pick_air(&opts, &sit(0, 0, false, 200.)).is_none());
    }

    #[test]
    fn day_only_aircraft_stay_home_at_night() {
        let day = AirOption { night: false, ..opt("A-10A", false) };
        let opts = [day];
        assert!(pick_air(&opts, &sit(0, 0, false, 50.)).is_some());
        assert!(pick_air(&opts, &sit(0, 0, true, 50.)).is_none());
    }
}
