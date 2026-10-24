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

//! Threat-aware routing for the AI helo missions.
//!
//! The terrain planner in `logistics` keeps a helo off the rocks but flew it
//! straight over whatever air defence lay on the line, at cruise height, to
//! the middle of a defended zone -- and it died there. This plans around the
//! air defence the calling side *knows about*, and nothing else:
//!
//! - its intel picture (`IntelDatabase`: recon, JTAC, special forces, AWACS
//!   and EWR contacts classed as air defence), each sized the way the
//!   tacmap's threat rings are (`situation::build_threats`): by the widest
//!   weapon inside the contact's own uncertainty bubble, plus that
//!   uncertainty -- the reach, without the planner learning what it is;
//! - a presumed short-range garrison (MANPADS, AAA) around every enemy
//!   objective the side can see on the F10 map whose garrison is still
//!   standing -- positions and health are public, what's in them isn't;
//! - what the crew itself has seen: an air-defence site that fired or hit
//!   near it, or one on its own radar warning receiver.
//!
//! Routes are planned in 2D over a visibility graph -- polygons circumscribing
//! each threat circle -- by Dijkstra on distance plus a heavy penalty per
//! metre flown inside a circle, weighted by how lethal it is. A threat-free
//! route of acceptable length wins outright; when there is none (the target
//! sits inside its own defences, or a wall of SAMs is wider than a detour is
//! worth) the same search returns the route with the least weighted exposure.
//! `logistics::plan_helo_route` then gives it the terrain profile, flown
//! nap-of-the-earth near threats and on the final approach.
//!
//! The helo lands at the spot in the target zone farthest from known enemies
//! that is flat, on land and clear of campaign units, instead of the zone
//! centre where the garrison stands. In flight it re-routes when it comes
//! under fire or new air defence is reported across the rest of its route,
//! rate-limited per mission.
//!
//! A side that knows of no threat near the route gets exactly the old route;
//! only the defensive AI options and the landing spot differ.

use super::{
    logistics::{plan_helo_route, HeloMissionId, HeloMissionState, HeloRoutePlan},
    markup::objective_visible_to,
    objective::Objective,
    Db,
};
use crate::{db::intel::IntelUnitClass, unitdb::UnitDb};
use anyhow::Result;
use bfprotocols::{
    cfg::{HeloInsertionCfg, HeloThreatAvoidanceCfg, UnitTag},
    db::{group::UnitId, objective::ObjectiveId},
};
use chrono::prelude::*;
use compact_str::format_compact;
use dcso3::{
    coalition::Side,
    controller::{
        ActionTyp, AiOption, AirEcmUsing, AirFlareUsing, AirOption, AirReactionToThreat, AirRoe,
        AltType, MissionPoint, PointType, Task, TurnMethod,
    },
    land::Land,
    object::DcsObject,
    LuaVec2, MizLua, Vector2,
};
use enumflags2::BitFlags;
use log::{info, warn};
use smallvec::SmallVec;
use std::{cmp::Ordering, collections::BinaryHeap, f64::consts::PI};

/// Vertices of the polygon planned around each threat circle. Twelve keeps
/// the detour within 4% of the circle itself.
const VERTS_PER_THREAT: usize = 12;
/// What a metre inside a threat circle costs, in metres of extra flying, at
/// full lethality. High enough that any detour inside the length limit wins
/// over flying through, low enough that the least-exposed crossing is still
/// found when there is no way around.
const EXPOSURE_WEIGHT: f64 = 30.;
/// Longest route the planner will consider, as a multiple of the straight
/// line plus a constant: detours cost fuel, the mission deadline and the
/// player's patience, and past this flying low through is the better bet.
const MAX_DETOUR_FACTOR: f64 = 1.6;
const MAX_DETOUR_EXTRA_M: f64 = 10_000.;
/// Threats fed to the graph search, nearest the direct line first. Bounds
/// the search at ~300 nodes whatever the map holds.
const MAX_THREATS: usize = 24;
/// A radar SAM's full reach doesn't apply to a helicopter at 40m: the radar
/// horizon for a target that low is ~40km over flat ground, and far less
/// behind any terrain. Circles are capped here before the margin.
const LOW_LEVEL_WEZ_CAP_M: f64 = 40_000.;
/// Flown NOE this far outside any threat circle, and this close to the
/// landing point, whenever the side knows of any threat at all.
const NOE_BUFFER_M: f64 = 2_000.;
const NOE_FINAL_M: f64 = 6_000.;
/// Inside this of the landing point the helo is committed: re-routing it on
/// short final only turns a landing into a go-around under fire.
const FINAL_NO_REPLAN_M: f64 = 3_000.;
/// How long being shot at stays a reason to re-route.
const FIRE_MEMORY_SECS: i64 = 60;
/// A newly known threat only forces a re-route if the rest of the route
/// spends at least this much (weighted) inside it.
const MIN_NEW_EXPOSURE_M: f64 = 200.;
/// How far from the target zone known enemies still count when picking the
/// landing spot.
const HAZARD_SEARCH_M: f64 = 5_000.;
/// Same clearance `Db::clear_helo_spot` keeps the launch spot from units.
const LANDING_UNIT_CLEARANCE_M: f64 = 45.;
/// Most threats one mission remembers seeing itself.
const MAX_OBSERVED: usize = 16;
/// Farthest a crew notices a launch or tracers that weren't aimed at it.
const LAUNCH_SEEN_M: f64 = 15_000.;
/// Reach assumed for a ground unit that isn't air defence but hit the helo
/// anyway (heavy machine guns, cannon).
const GROUND_FIRE_M: f64 = 3_000.;

// ─── Threats ─────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ThreatKind {
    /// An air-defence contact in the side's intel picture.
    Intel,
    /// Short-range air defence presumed around a visible enemy objective.
    Garrison,
    /// Something this helo has seen fire, or has on its warning receiver.
    Observed,
}

impl ThreatKind {
    fn label(self) -> &'static str {
        match self {
            Self::Intel => "air-defence contact",
            Self::Garrison => "enemy garrison",
            Self::Observed => "active air defence",
        }
    }
}

/// One circle the route should stay out of.
#[derive(Debug, Clone, Copy)]
pub(super) struct Threat {
    pub pos: Vector2,
    /// Engagement radius, margin and position uncertainty already applied.
    pub radius: f64,
    /// 0..1: how much a metre inside this circle is worth avoiding.
    pub lethality: f64,
    pub kind: ThreatKind,
}

impl Threat {
    fn contains(&self, p: Vector2) -> bool {
        (p - self.pos).norm() < self.radius
    }

    /// Whether `other` is this same threat seen again: intel positions drift
    /// as contacts merge and their uncertainty changes with the source, and
    /// none of that is news worth re-routing for.
    fn covers(&self, other: &Threat) -> bool {
        let tol = (0.25 * self.radius).max(1_000.);
        (self.pos - other.pos).norm() <= tol && other.radius <= self.radius * 1.1 + 500.
    }
}

// ─── Geometry ────────────────────────────────────────────────────────────────

/// Distance from `p` to the segment `a`..`b`.
fn seg_dist(a: Vector2, b: Vector2, p: Vector2) -> f64 {
    let d = b - a;
    let len2 = d.norm_squared();
    if len2 < 1e-9 {
        return (p - a).norm();
    }
    let t = ((p - a).dot(&d) / len2).clamp(0., 1.);
    (a + d * t - p).norm()
}

/// Length of the part of the segment `a`..`b` inside the circle (`c`, `r`).
fn chord_inside(a: Vector2, b: Vector2, c: Vector2, r: f64) -> f64 {
    let d = b - a;
    let len2 = d.norm_squared();
    if len2 < 1e-9 {
        return 0.;
    }
    // |a + t d - c|^2 = r^2, solved for t and clipped to the segment.
    let f = a - c;
    let bq = f.dot(&d);
    let cq = f.norm_squared() - r * r;
    let disc = bq * bq - len2 * cq;
    if disc <= 0. {
        return 0.;
    }
    let s = disc.sqrt();
    let t0 = ((-bq - s) / len2).max(0.);
    let t1 = ((-bq + s) / len2).min(1.);
    if t1 <= t0 { 0. } else { (t1 - t0) * len2.sqrt() }
}

/// Lethality-weighted metres of `a`..`b` inside `threats`. Overlapping
/// circles count once each: two sites covering the same stretch are twice
/// the danger.
fn exposure(a: Vector2, b: Vector2, threats: &[Threat]) -> f64 {
    threats
        .iter()
        .map(|t| t.lethality * chord_inside(a, b, t.pos, t.radius))
        .sum()
}

fn track_exposure(track: &[Vector2], threats: &[Threat]) -> f64 {
    track.windows(2).map(|w| exposure(w[0], w[1], threats)).sum()
}

fn track_len(track: &[Vector2]) -> f64 {
    track.windows(2).map(|w| (w[1] - w[0]).norm()).sum()
}

// ─── 2D planner ──────────────────────────────────────────────────────────────

/// A ground track from the 2D planner.
#[derive(Debug, Clone)]
pub(super) struct PlannedTrack {
    /// Start and landing point included.
    pub track: Vec<Vector2>,
    pub length: f64,
    /// Lethality-weighted metres inside any threat circle.
    pub exposure: f64,
    /// The part of `exposure` in circles that contain neither the start nor
    /// the landing point -- i.e. that a route could in principle have
    /// avoided. Zero is a clean route.
    pub avoidable: f64,
    /// Threats the straight line would have crossed and this route doesn't.
    pub avoided: usize,
}

#[derive(PartialEq)]
struct Open(f64, usize);

impl Eq for Open {}

impl Ord for Open {
    // Reversed: `BinaryHeap` is a max-heap and this wants the cheapest first.
    fn cmp(&self, other: &Self) -> Ordering {
        other.0.total_cmp(&self.0).then_with(|| other.1.cmp(&self.1))
    }
}

impl PartialOrd for Open {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Plan a ground track from `start` to `goal` around `threats`. See the
/// module docs. Always returns a route: at worst the straight line, which is
/// always inside the length limit.
pub(super) fn plan_track(start: Vector2, goal: Vector2, threats: &[Threat]) -> PlannedTrack {
    let direct = (goal - start).norm();
    let max_len = direct * MAX_DETOUR_FACTOR + MAX_DETOUR_EXTRA_M;
    // Only a circle an acceptable route could touch matters, and of those
    // the ones nearest the straight line matter most.
    let mut relevant: Vec<(f64, Threat)> = threats
        .iter()
        .filter(|t| (t.pos - start).norm() + (t.pos - goal).norm() - 2. * t.radius <= max_len)
        .map(|t| (seg_dist(start, goal, t.pos) - t.radius, *t))
        .collect();
    relevant.sort_by(|a, b| a.0.total_cmp(&b.0));
    relevant.truncate(MAX_THREATS);
    let relevant: Vec<Threat> = relevant.into_iter().map(|(_, t)| t).collect();

    // Nodes: start, goal, and a polygon around every circle drawn just far
    // enough out that its edges stay outside the circle.
    let mut nodes: Vec<Vector2> = vec![start, goal];
    let out = 1.02 / (PI / VERTS_PER_THREAT as f64).cos();
    for t in &relevant {
        for i in 0..VERTS_PER_THREAT {
            let a = i as f64 * 2. * PI / VERTS_PER_THREAT as f64;
            let p = t.pos + Vector2::new(a.cos(), a.sin()) * (t.radius * out);
            if (p - start).norm() + (p - goal).norm() <= max_len {
                nodes.push(p);
            }
        }
    }

    // Dijkstra on distance plus weighted exposure over the complete graph,
    // pruning anything that can no longer reach the goal inside `max_len`.
    // That pruning is a heuristic on a cost-ordered search, which is fine:
    // the direct edge always survives it, so the goal is always reached.
    let n = nodes.len();
    let mut cost = vec![f64::INFINITY; n];
    let mut len = vec![f64::INFINITY; n];
    let mut prev = vec![usize::MAX; n];
    let mut done = vec![false; n];
    let mut open = BinaryHeap::new();
    cost[0] = 0.;
    len[0] = 0.;
    open.push(Open(0., 0));
    while let Some(Open(c, u)) = open.pop() {
        if done[u] {
            continue;
        }
        done[u] = true;
        if u == 1 {
            break;
        }
        for v in 0..n {
            if done[v] {
                continue;
            }
            let d = (nodes[v] - nodes[u]).norm();
            let l = len[u] + d;
            if l + (goal - nodes[v]).norm() > max_len + 1. {
                continue;
            }
            let c2 = c + d + EXPOSURE_WEIGHT * exposure(nodes[u], nodes[v], &relevant);
            if c2 < cost[v] {
                cost[v] = c2;
                len[v] = l;
                prev[v] = u;
                open.push(Open(c2, v));
            }
        }
    }

    let mut track = vec![goal];
    let mut at = 1;
    while prev[at] != usize::MAX {
        at = prev[at];
        track.push(nodes[at]);
    }
    if at != 0 {
        // Unreachable by construction; the straight line is the safe answer.
        track = vec![goal, start];
    }
    track.reverse();

    let unavoidable: Vec<Threat> = threats
        .iter()
        .filter(|t| t.contains(start) || t.contains(goal))
        .copied()
        .collect();
    let avoidable: Vec<Threat> = threats
        .iter()
        .filter(|t| !t.contains(start) && !t.contains(goal))
        .copied()
        .collect();
    let avoided = avoidable
        .iter()
        .filter(|t| {
            chord_inside(start, goal, t.pos, t.radius) > 0.
                && track_exposure(&track, std::slice::from_ref(*t)) == 0.
        })
        .count();
    let avoidable_exp = track_exposure(&track, &avoidable);
    PlannedTrack {
        length: track_len(&track),
        exposure: avoidable_exp + track_exposure(&track, &unavoidable),
        avoidable: avoidable_exp,
        avoided,
        track,
    }
}

/// What `logistics::plan_helo_route` needs to fly a threat-aware track: the
/// track itself, and where along it to hug the ground.
pub(super) struct ProfileHint {
    pub track: Vec<Vector2>,
    pub threats: Vec<Threat>,
    pub noe_agl_m: f64,
    pub land_pos: Vector2,
}

impl ProfileHint {
    /// Whether the route is flown nap-of-the-earth at `p`.
    pub fn is_noe(&self, p: Vector2) -> bool {
        (p - self.land_pos).norm() <= NOE_FINAL_M
            || self
                .threats
                .iter()
                .any(|t| (p - t.pos).norm() <= t.radius + NOE_BUFFER_M)
    }

    /// Whether any part of the leg `a`..`b` is flown nap-of-the-earth, so it
    /// needs cutting finely enough to follow the ground.
    pub fn leg_is_noe(&self, a: Vector2, b: Vector2) -> bool {
        seg_dist(a, b, self.land_pos) <= NOE_FINAL_M
            || self
                .threats
                .iter()
                .any(|t| seg_dist(a, b, t.pos) <= t.radius + NOE_BUFFER_M)
    }
}

// ─── Landing spot ────────────────────────────────────────────────────────────

/// Pick where in the target zone to land: the candidate farthest from every
/// known `hazard` (ties to the side the helo comes in from, which is the
/// shorter run inside the defences), or the zone centre when there are none,
/// among candidates `in_zone` accepts and `usable` passes. `usable` is the
/// expensive one (terrain queries), so it only runs on the best few.
pub(super) fn choose_landing_point(
    center: Vector2,
    radius: f64,
    hazards: &[Vector2],
    inbound_from: Vector2,
    in_zone: impl Fn(Vector2) -> bool,
    mut usable: impl FnMut(Vector2) -> bool,
) -> Vector2 {
    const RINGS: [f64; 4] = [0.25, 0.5, 0.75, 1.0];
    const BEARINGS: usize = 12;
    const MAX_CHECKS: usize = 16;
    let score = |p: Vector2| -> f64 {
        if hazards.is_empty() {
            -(p - center).norm()
        } else {
            let clear = hazards
                .iter()
                .map(|h| (p - *h).norm())
                .fold(f64::INFINITY, f64::min);
            clear - 0.05 * (p - inbound_from).norm()
        }
    };
    let mut cands: Vec<(f64, Vector2)> = vec![(score(center), center)];
    if radius > 1. {
        for frac in RINGS {
            for k in 0..BEARINGS {
                let a = k as f64 * 2. * PI / BEARINGS as f64;
                let p = center + Vector2::new(a.cos(), a.sin()) * (radius * frac);
                cands.push((score(p), p));
            }
        }
    }
    cands.retain(|(_, p)| *p == center || in_zone(*p));
    cands.sort_by(|a, b| b.0.total_cmp(&a.0));
    cands
        .into_iter()
        .take(MAX_CHECKS)
        .map(|(_, p)| p)
        .find(|p| usable(*p))
        .unwrap_or(center)
}

// ─── Re-route bookkeeping ────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ReplanBlocked {
    TooSoon,
    Exhausted,
}

/// Rate limit on in-flight re-routes: every one is a new task for the AI,
/// which resets what it was doing, and a helo that is re-planned every poll
/// flies in circles.
#[derive(Debug, Clone, Default)]
pub(super) struct ReplanGate {
    last: Option<DateTime<Utc>>,
    count: u32,
}

impl ReplanGate {
    pub fn check(&self, now: DateTime<Utc>, min_secs: u32, max: u32) -> Result<(), ReplanBlocked> {
        if self.count >= max {
            return Err(ReplanBlocked::Exhausted);
        }
        match self.last {
            Some(last) if (now - last).num_seconds() < min_secs as i64 => Err(ReplanBlocked::TooSoon),
            _ => Ok(()),
        }
    }

    pub fn record(&mut self, now: DateTime<Utc>) {
        self.last = Some(now);
        self.count += 1;
    }
}

/// The rest of `track` from `pos`: `pos` itself, then every vertex after the
/// leg the helo is nearest to.
fn remaining_track(track: &[Vector2], pos: Vector2) -> Vec<Vector2> {
    let leg = track
        .windows(2)
        .enumerate()
        .map(|(i, w)| (i, seg_dist(w[0], w[1], pos)))
        .min_by(|a, b| a.1.total_cmp(&b.1))
        .map(|(i, _)| i);
    let mut out = vec![pos];
    match leg {
        Some(i) => out.extend_from_slice(&track[i + 1..]),
        None => out.extend(track.last().copied()),
    }
    out
}

/// A threat in `current` that the route was not planned against and that
/// the rest of it runs through.
fn new_threat_on_route<'a>(
    remaining: &[Vector2],
    planned: &[Threat],
    current: &'a [Threat],
) -> Option<&'a Threat> {
    current.iter().find(|t| {
        !planned.iter().any(|p| p.covers(t))
            && track_exposure(remaining, std::slice::from_ref(*t)) >= MIN_NEW_EXPOSURE_M
    })
}

/// Per-mission routing state, alongside `Ephemeral::active_helo_missions`.
#[derive(Debug, Clone)]
pub(super) struct HeloRouteState {
    /// The ground track being flown, landing point last.
    track: Vec<Vector2>,
    pub land_pos: Vector2,
    /// The threats `track` was planned against.
    planned: Vec<Threat>,
    /// What this crew has seen for itself.
    observed: Vec<Threat>,
    /// Last time it was shot at or hit, and by what.
    under_fire: Option<(DateTime<Utc>, &'static str)>,
    gate: ReplanGate,
    told_exhausted: bool,
}

impl HeloRouteState {
    fn observe(&mut self, t: Threat) {
        if self.observed.iter().any(|o| o.covers(&t)) {
            return;
        }
        if self.observed.len() >= MAX_OBSERVED {
            self.observed.remove(0);
        }
        self.observed.push(t);
    }
}

/// A planned route: where to land, and the track and profile hint to get
/// there (`hint` is `None` when no known threat is near it, which flies the
/// plain terrain route).
pub(super) struct SmartRoute {
    pub land_pos: Vector2,
    pub hint: Option<ProfileHint>,
    planned: PlannedTrack,
    threats: Vec<Threat>,
    /// Straight-line distance to the landing point, for the detour figure.
    direct: f64,
}

// ─── DCS side ────────────────────────────────────────────────────────────────

/// The options every helo mission flies with.
fn defensive_options<'lua>() -> [AiOption<'lua>; 6] {
    [
        // Never stop to fight: a transport that turns to engage something is
        // a transport that doesn't land.
        AiOption::Air(AirOption::Roe(AirRoe::WeaponHold)),
        // Break and jink when shot at, then carry on. Bypass-and-escape and
        // allow-abort both let the AI give up on the landing.
        AiOption::Air(AirOption::ReactionOnThreat(AirReactionToThreat::EvadeFire)),
        // Flares pre-emptively inside a SAM's reach: an IR MANPADS gives most
        // helicopters no launch warning to react to.
        AiOption::Air(AirOption::FlareUsing(AirFlareUsing::WhenFlyingInSamWez)),
        AiOption::Air(AirOption::EcmUsing(AirEcmUsing::UseIfDetectedLockByRadar)),
        // Nothing sends it home early; the mission deadline decides that.
        AiOption::Air(AirOption::RtbOnBingo(false)),
        AiOption::Air(AirOption::RtbOnOutOfAmmo(false)),
    ]
}

/// The defensive options as waypoint-0 tasks for a spawning helo mission,
/// or nothing when threat avoidance is off.
pub(super) fn defensive_task<'lua>(cfg: &HeloInsertionCfg) -> Task<'lua> {
    if cfg.threat_avoidance.enabled {
        Task::ComboTask(defensive_options().into_iter().map(Task::WrappedOption).collect())
    } else {
        Task::ComboTask(vec![])
    }
}

/// The `Land` waypoint, in the shape the Mission Editor writes (see the
/// same point in `Db::spawn_helo_mission`).
fn land_point<'lua>(pos: Vector2, alt: f64, speed: f64) -> MissionPoint<'lua> {
    MissionPoint {
        action: Some(ActionTyp::Air(TurnMethod::Landing)),
        airdrome_id: None,
        helipad: None,
        typ: PointType::Land,
        link_unit: None,
        pos: LuaVec2(pos),
        alt,
        alt_typ: Some(AltType::BARO),
        time_re_fu_ar: Some(600),
        eta: None,
        eta_locked: None,
        speed,
        speed_locked: None,
        name: None,
        task: Box::new(Task::ComboTask(vec![])),
    }
}

/// Hand a group already in the air its new route, re-asserting the
/// defensive options first -- a new mission task must not leave it flying
/// with whatever the AI defaults to.
fn push_route(
    lua: MizLua,
    group_name: &str,
    plan: &HeloRoutePlan,
    here: Vector2,
    land_pos: Vector2,
    land_alt: f64,
    speed: f64,
) -> Result<()> {
    use crate::airlife::air_point;
    let con = dcso3::group::Group::get_by_name(lua, group_name)?.get_controller()?;
    for opt in defensive_options() {
        con.set_option(opt)?;
    }
    let mut route = Vec::with_capacity(plan.cruise.len() + 3);
    route.push(air_point(here, plan.departure_alt, speed, Task::ComboTask(vec![])));
    for (p, a) in plan.cruise.iter().copied() {
        route.push(air_point(p, a, speed, Task::ComboTask(vec![])));
    }
    route.push(air_point(plan.approach.0, plan.approach.1, speed, Task::ComboTask(vec![])));
    route.push(land_point(land_pos, land_alt, speed));
    con.set_task(Task::Mission { airborne: Some(true), route })?;
    Ok(())
}

/// Engagement range of one air-defence unit type, or `None` if it isn't
/// air defence. DCS's own `ThreatRange` when the unit harvest has it, else a
/// class figure from the campaign's tags.
fn ad_range_m(typ: &str, tags: BitFlags<UnitTag>, udb: &UnitDb) -> Option<f64> {
    if tags.intersects(UnitTag::Aircraft | UnitTag::Helicopter)
        || !tags.intersects(UnitTag::SAM | UnitTag::AAA)
    {
        return None;
    }
    if let Some(r) = udb.get(typ).and_then(|i| i.threat_range_m) {
        return Some(r);
    }
    Some(if tags.contains(UnitTag::LR) {
        40_000.
    } else if tags.contains(UnitTag::MR) {
        20_000.
    } else if tags.contains(UnitTag::SR) {
        8_000.
    } else if tags.contains(UnitTag::SAM) {
        10_000.
    } else {
        3_000.
    })
}

impl Db {
    /// Live air-defence units of `side`'s enemy, as (position, range).
    /// Only ever used to size what the side already has a contact on.
    fn enemy_air_defence(&self, side: Side) -> Vec<(Vector2, f64)> {
        let udb = crate::unitdb::get();
        let enemy = side.opposite();
        self.persisted
            .units
            .into_iter()
            .filter(|(_, u)| u.side == enemy && !u.dead)
            .filter_map(|(_, u)| ad_range_m(u.typ.0.as_str(), u.tags.0, &udb).map(|r| (u.pos, r)))
            .collect()
    }

    /// The air-defence site `uid` belongs to, as (position, reach, what it
    /// is), if it is one. Reach is the widest weapon in its group: a search
    /// radar has no range of its own, the launchers it cues do.
    fn site_threat(&self, uid: &UnitId) -> Option<(Vector2, f64, Side, &'static str)> {
        let udb = crate::unitdb::get();
        let u = self.persisted.units.get(uid)?;
        let own = ad_range_m(u.typ.0.as_str(), u.tags.0, &udb)?;
        let site = self
            .persisted
            .groups
            .get(&u.group)
            .map(|g| {
                g.units
                    .into_iter()
                    .filter_map(|id| self.persisted.units.get(id))
                    .filter(|m| !m.dead)
                    .filter_map(|m| ad_range_m(m.typ.0.as_str(), m.tags.0, &udb))
                    .fold(own, f64::max)
            })
            .unwrap_or(own);
        let what = if u.tags.0.contains(UnitTag::SAM) { "SAM" } else { "AAA" };
        Some((u.pos, site.min(LOW_LEVEL_WEZ_CAP_M), u.side, what))
    }

    /// Every air-defence threat `side` knows about, plus `observed`. See the
    /// module docs for the sources.
    fn known_air_threats(
        &self,
        side: Side,
        ta: &HeloThreatAvoidanceCfg,
        observed: &[Threat],
    ) -> Vec<Threat> {
        let margin = ta.margin.max(1.);
        let ad = self.enemy_air_defence(side);
        let mut out: Vec<Threat> = vec![];
        for c in self.ephemeral.intel_db.contacts_for(side) {
            if c.unit_class != IntelUnitClass::AirDefense || c.enemy_side == side {
                continue;
            }
            let unc = c.pos_uncertainty_m as f64;
            let bubble = unc.max(2_500.);
            let reach = ad
                .iter()
                .filter(|(p, _)| (p - c.pos).norm() <= bubble)
                .map(|(_, r)| *r)
                .fold(None, |acc: Option<f64>, r| Some(acc.map_or(r, |a| a.max(r))))
                .unwrap_or(ta.unknown_radius_m);
            out.push(Threat {
                pos: c.pos,
                radius: reach.min(LOW_LEVEL_WEZ_CAP_M) * margin + unc,
                lethality: 0.5 + 0.5 * (c.confidence as f64).clamp(0., 1.),
                kind: ThreatKind::Intel,
            });
        }
        if ta.garrison_radius_m > 0. {
            for (_, o) in self.objectives() {
                if o.owner != side.opposite() || o.health() == 0 || !objective_visible_to(o, side) {
                    continue;
                }
                out.push(Threat {
                    pos: o.pos(),
                    radius: o.zone.radius() + ta.garrison_radius_m * margin,
                    lethality: 0.4,
                    kind: ThreatKind::Garrison,
                });
            }
        }
        out.extend(observed.iter().copied());
        out
    }

    /// Positions to keep the landing spot away from: every known threat and
    /// every enemy contact of any kind near `dest`.
    fn landing_hazards(&self, side: Side, dest: &Objective, threats: &[Threat]) -> Vec<Vector2> {
        let center = dest.zone.pos();
        let reach = dest.zone.radius() + HAZARD_SEARCH_M;
        let near = |p: Vector2| (p - center).norm() <= reach;
        threats
            .iter()
            .map(|t| t.pos)
            .chain(
                self.ephemeral
                    .intel_db
                    .contacts_for(side)
                    .filter(|c| c.enemy_side != side)
                    .map(|c| c.pos),
            )
            .filter(|p| near(*p))
            .collect()
    }

    /// Somewhere a helicopter can put down: land (or road or runway), level
    /// across a 30m pad, and clear of every campaign unit -- a garrison that
    /// is despawned now wakes up the moment the zone is contested.
    fn landing_spot_usable(&self, land: &Land, p: Vector2) -> bool {
        use dcso3::land::SurfaceType;
        match land.get_surface_type(LuaVec2(p)) {
            Ok(SurfaceType::Land | SurfaceType::Road | SurfaceType::Runway) => (),
            _ => return false,
        }
        let Ok(h) = land.get_height(LuaVec2(p)) else { return false };
        for (dx, dz) in [(15., 0.), (-15., 0.), (0., 15.), (0., -15.)] {
            match land.get_height(LuaVec2(p + Vector2::new(dx, dz))) {
                Ok(hq) if (hq - h).abs() <= 2.5 => (),
                _ => return false,
            }
        }
        self.persisted
            .units
            .into_iter()
            .all(|(_, u)| (u.pos - p).norm() >= LANDING_UNIT_CLEARANCE_M)
    }

    /// Plan a helo mission's route from `from` to `dest` for `side`, or
    /// `None` when threat avoidance is off. `observed` is what the crew has
    /// seen for itself (empty at launch).
    pub(super) fn plan_helo_threat_route(
        &self,
        land: &Land,
        side: Side,
        from: Vector2,
        dest: &ObjectiveId,
        cfg: &HeloInsertionCfg,
        observed: &[Threat],
    ) -> Option<SmartRoute> {
        let ta = &cfg.threat_avoidance;
        if !ta.enabled {
            return None;
        }
        let obj = self.persisted.objectives.get(dest)?;
        let threats = self.known_air_threats(side, ta, observed);
        let hazards = self.landing_hazards(side, obj, &threats);
        let r = obj.zone.radius();
        // Troops only capture from inside the zone, and they spread out when
        // they dismount: keep the spot well inside it.
        let margin = (r * 0.2).clamp(100., 400.);
        let land_pos = choose_landing_point(
            obj.zone.pos(),
            (r - margin).max(0.),
            &hazards,
            from,
            |p| obj.zone.contains_circle(p, margin),
            |p| self.landing_spot_usable(land, p),
        );
        let planned = plan_track(from, land_pos, &threats);
        let near_route = threats.iter().any(|t| {
            planned
                .track
                .windows(2)
                .any(|w| seg_dist(w[0], w[1], t.pos) <= t.radius + NOE_BUFFER_M)
        });
        let hint = near_route.then(|| ProfileHint {
            track: planned.track.clone(),
            threats: threats.clone(),
            noe_agl_m: ta.noe_agl_m.max(10.),
            land_pos,
        });
        Some(SmartRoute { land_pos, hint, planned, threats, direct: (land_pos - from).norm() })
    }

    /// Record a just-spawned mission's route, stretch its deadline to cover
    /// any detour, and tell the player when the route is anything but plain.
    pub(super) fn helo_route_dispatched(
        &mut self,
        id: &HeloMissionId,
        from: Vector2,
        route: SmartRoute,
        dest_name: &str,
        speed_mps: f64,
    ) {
        let extra = (route.planned.length - route.direct).max(0.);
        let Some(m) = self.ephemeral.active_helo_missions.get_mut(id) else { return };
        if let Some(d) = m.deadline.as_mut() {
            *d += chrono::Duration::seconds((extra / speed_mps.max(1.) * 1.2) as i64);
        }
        let player = m.player;
        let dest_center = self.persisted.objectives.get(&m.destination).map(|o| o.pos());
        let track = match &route.hint {
            Some(h) => h.track.clone(),
            None => vec![from, route.land_pos],
        };
        info!(
            "[HELO_ROUTE] {} to {}: {} known threat(s), {} near the route; {:.1}km vs {:.1}km direct, \
             {} avoided, exposure {:.1}km ({:.1}km avoidable){}; landing {:.0}m off the zone centre",
            id,
            dest_name,
            route.threats.len(),
            if route.hint.is_some() { "some" } else { "none" },
            route.planned.length / 1000.,
            route.direct / 1000.,
            route.planned.avoided,
            route.planned.exposure / 1000.,
            route.planned.avoidable / 1000.,
            if route.planned.avoidable > 0. { " -- no threat-free route, least exposure taken" } else { "" },
            dest_center.map(|c| (route.land_pos - c).norm()).unwrap_or(0.)
        );
        let msg = if route.hint.is_none() {
            None
        } else if route.planned.avoidable > 0. {
            Some(format_compact!(
                "no clean way into {dest_name} past known air defence -- your helo takes the least-defended gap, flying low"
            ))
        } else if route.planned.avoided > 0 {
            Some(format_compact!(
                "your helo to {dest_name} is routed around {} known air-defence threat(s) (+{:.0} km), flying low near them",
                route.planned.avoided,
                extra / 1000.
            ))
        } else {
            None
        };
        self.ephemeral.helo_routes.insert(
            id.clone(),
            HeloRouteState {
                track,
                land_pos: route.land_pos,
                planned: route.threats,
                observed: vec![],
                under_fire: None,
                gate: ReplanGate::default(),
                told_exhausted: false,
            },
        );
        if let Some(msg) = msg {
            self.ephemeral.panel_to_player(&self.persisted, 15, &player, msg);
        }
    }

    /// An air-defence unit fired (missile launch or gun burst). Every helo
    /// mission of its enemy inside its reach counts as under fire -- the crew
    /// can see the launch -- and remembers the site. The target itself is
    /// never read: `weapon.getTarget()` on a ground shooter's weapon can take
    /// the server down (see `shots.rs`).
    pub fn note_air_defence_fire(&mut self, shooter: &UnitId, now: DateTime<Utc>) {
        if self.ephemeral.helo_routes.is_empty() {
            return;
        }
        let Some(margin) = self.helo_threat_margin() else { return };
        let Some((pos, reach, side, what)) = self.site_threat(shooter) else { return };
        let eph = &mut self.ephemeral;
        for (id, st) in eph.helo_routes.iter_mut() {
            let Some(m) = eph.active_helo_missions.get(id) else { continue };
            let d = (m.last_pos - pos).norm();
            // Only what the crew could plausibly see: a long-range SAM
            // launching at a jet 40km away is none of its business.
            if m.side != side.opposite() || d > (reach * 1.2).min(LAUNCH_SEEN_M) {
                continue;
            }
            st.observe(Threat { pos, radius: reach * margin, lethality: 1., kind: ThreatKind::Observed });
            let fresh = st.under_fire.is_none_or(|(t, _)| (now - t).num_seconds() > FIRE_MEMORY_SECS);
            st.under_fire = Some((now, if what == "SAM" { "SAM launch" } else { "AAA fire" }));
            if fresh {
                info!("[HELO_ROUTE] {} under fire: {} {:.1}km away", id, what, d / 1000.);
            }
        }
    }

    /// One of a helo mission's units was hit by the campaign ground unit
    /// `shooter`. Anything else -- a fighter, a player, an unknown -- is not
    /// something a new route gets it away from, so it is left to the AI's
    /// own evasion.
    pub fn note_helo_hit(&mut self, target: &UnitId, shooter: Option<UnitId>, now: DateTime<Utc>) {
        if self.ephemeral.helo_routes.is_empty() {
            return;
        }
        let (Some(shooter), Some(gid)) = (shooter, self.persisted.units.get(target).map(|u| u.group))
        else {
            return;
        };
        let Some((id, side)) = self
            .ephemeral
            .active_helo_missions
            .iter()
            .find(|(_, m)| m.group_id == gid)
            .map(|(id, m)| (id.clone(), m.side))
        else {
            return;
        };
        let site = self.site_threat(&shooter).or_else(|| {
            self.persisted
                .units
                .get(&shooter)
                .filter(|u| !u.tags.0.intersects(UnitTag::Aircraft | UnitTag::Helicopter))
                .map(|u| (u.pos, GROUND_FIRE_M, u.side, "ground fire"))
        });
        let Some((pos, reach, _, what)) = site.filter(|s| s.2 == side.opposite()) else { return };
        let margin = self.helo_threat_margin().unwrap_or(1.);
        let Some(st) = self.ephemeral.helo_routes.get_mut(&id) else { return };
        st.observe(Threat { pos, radius: reach * margin, lethality: 1., kind: ThreatKind::Observed });
        st.under_fire = Some((now, "hit"));
        info!("[HELO_ROUTE] {} hit by {}", id, what);
    }

    fn helo_threat_margin(&self) -> Option<f64> {
        self.ephemeral
            .cfg
            .helo_insertion
            .as_ref()
            .filter(|c| c.threat_avoidance.enabled)
            .map(|c| c.threat_avoidance.margin.max(1.))
    }

    /// What the helo's own radar warning receiver has on it: enemy
    /// air-defence radars whose site can reach it become observed threats.
    /// An airframe without one simply reports nothing.
    fn scan_rwr(&mut self, lua: MizLua, id: &HeloMissionId, group_name: &str, pos: Vector2) {
        use dcso3::controller::Detection;
        let Some(margin) = self.helo_threat_margin() else { return };
        let seen: Result<SmallVec<[(Vector2, f64); 4]>> = (|| {
            let con = dcso3::group::Group::get_by_name(lua, group_name)?.get_controller()?;
            let mut out = SmallVec::new();
            for t in con.get_detected_targets(BitFlags::from(Detection::Rwr))?.into_iter().take(16) {
                let Ok(t) = t else { continue };
                let Some(uid) = t
                    .object
                    .as_unit()
                    .ok()
                    .and_then(|u| u.object_id().ok())
                    .and_then(|oid| self.ephemeral.get_uid_by_object_id(&oid).copied())
                else {
                    continue;
                };
                if let Some((p, reach, _, _)) = self.site_threat(&uid)
                    && (p - pos).norm() <= reach * margin
                {
                    out.push((p, reach));
                }
            }
            Ok(out)
        })();
        let seen = match seen {
            Ok(s) => s,
            Err(e) => {
                log::debug!("[HELO_ROUTE] {id} warning receiver read failed: {e:?}");
                return;
            }
        };
        if let Some(st) = self.ephemeral.helo_routes.get_mut(id) {
            for (p, reach) in seen {
                st.observe(Threat { pos: p, radius: reach * margin, lethality: 1., kind: ThreatKind::Observed });
            }
        }
    }

    /// In-flight re-routing, run from `tick_helo_missions` on its poll: a
    /// helo that is under fire, or whose remaining route crosses air defence
    /// that wasn't known when it was planned, is re-planned from where it is
    /// and handed the new route. See `ReplanGate` for the rate limit.
    pub(super) fn tick_helo_routes(&mut self, lua: MizLua, now: DateTime<Utc>) {
        let missions = &self.ephemeral.active_helo_missions;
        self.ephemeral.helo_routes.retain(|id, _| missions.contains_key(id));
        let Some(cfg) = self.ephemeral.cfg.helo_insertion.clone() else { return };
        let ta = &cfg.threat_avoidance;
        if !ta.enabled || !ta.replan || self.ephemeral.helo_routes.is_empty() {
            return;
        }
        let speed = (cfg.speed_kph / 3.6).max(10.);
        let ids: SmallVec<[HeloMissionId; 4]> = self.ephemeral.helo_routes.keys().cloned().collect();
        for id in ids {
            let Some(m) = self.ephemeral.active_helo_missions.get(&id) else { continue };
            // Only on the ticks `tick_helo_missions` actually polled this
            // mission (every ~10s), so `last_pos` is fresh and the warning
            // receiver isn't read every frame.
            if m.last_check != now || m.state != HeloMissionState::InTransit || m.airborne_at.is_none() {
                continue;
            }
            let (side, pos, dest, player, gid) = (m.side, m.last_pos, m.destination, m.player, m.group_id);
            let Some(group_name) = self.persisted.groups.get(&gid).map(|g| g.name.clone()) else { continue };
            self.scan_rwr(lua, &id, &group_name, pos);
            let Some(st) = self.ephemeral.helo_routes.get(&id) else { continue };
            if (pos - st.land_pos).norm() < FINAL_NO_REPLAN_M {
                continue;
            }
            let fire = st
                .under_fire
                .filter(|(t, _)| (now - *t).num_seconds() <= FIRE_MEMORY_SECS)
                .map(|(_, why)| why);
            let reason = match fire {
                Some(why) => format_compact!("under fire ({why})"),
                None => {
                    let threats = self.known_air_threats(side, ta, &st.observed);
                    let remaining = remaining_track(&st.track, pos);
                    match new_threat_on_route(&remaining, &st.planned, &threats) {
                        Some(t) => format_compact!(
                            "new {} ({:.1}km radius) across the route",
                            t.kind.label(),
                            t.radius / 1000.
                        ),
                        None => continue,
                    }
                }
            };
            match st.gate.check(now, ta.min_replan_secs, ta.max_replans) {
                Ok(()) => (),
                Err(ReplanBlocked::TooSoon) => continue,
                Err(ReplanBlocked::Exhausted) => {
                    if let Some(st) = self.ephemeral.helo_routes.get_mut(&id) {
                        if !st.told_exhausted {
                            info!(
                                "[HELO_ROUTE] {} {} but has used all {} re-routes, pressing on",
                                id, reason, ta.max_replans
                            );
                            st.told_exhausted = true;
                        }
                        st.under_fire = None;
                    }
                    continue;
                }
            }
            let observed = st.observed.clone();
            let dest_name = self
                .persisted
                .objectives
                .get(&dest)
                .map(|o| o.name.clone())
                .unwrap_or_default();
            let land = match Land::singleton(lua) {
                Ok(l) => l,
                Err(e) => {
                    warn!("[HELO_ROUTE] {id} re-route skipped, no land: {e:?}");
                    continue;
                }
            };
            let Some(route) = self.plan_helo_threat_route(&land, side, pos, &dest, &cfg, &observed) else {
                continue;
            };
            let plan = plan_helo_route(&land, pos, route.land_pos, &cfg, route.hint.as_ref());
            let land_alt = land.get_height(LuaVec2(route.land_pos)).unwrap_or(0.);
            if let Err(e) = push_route(lua, &group_name, &plan, pos, route.land_pos, land_alt, speed) {
                warn!("[HELO_ROUTE] {id} re-route ({reason}) could not be handed to the AI: {e:?}");
                continue;
            }
            let track = match &route.hint {
                Some(h) => h.track.clone(),
                None => vec![pos, route.land_pos],
            };
            info!(
                "[HELO_ROUTE] {} re-routed ({}) from {:.1}km out: {:.1}km to go, exposure {:.1}km, {} cruise waypoint(s)",
                id,
                reason,
                (pos - route.land_pos).norm() / 1000.,
                route.planned.length / 1000.,
                route.planned.exposure / 1000.,
                plan.cruise.len()
            );
            // Whatever is left of the flight, plus the time on the ground.
            let need = now
                + chrono::Duration::seconds((route.planned.length / speed * 1.5) as i64 + 12 * 60);
            if let Some(m) = self.ephemeral.active_helo_missions.get_mut(&id)
                && let Some(d) = m.deadline.as_mut()
                && *d < need
            {
                *d = need;
            }
            let under_fire = fire.is_some();
            if let Some(st) = self.ephemeral.helo_routes.get_mut(&id) {
                st.gate.record(now);
                st.under_fire = None;
                st.track = track;
                st.land_pos = route.land_pos;
                st.planned = route.threats;
            }
            let msg = if under_fire {
                format_compact!("your helo to {dest_name} is under fire -- re-routing")
            } else {
                format_compact!("your helo to {dest_name} is re-routing around newly reported air defence")
            };
            self.ephemeral.panel_to_player(&self.persisted, 10, &player, msg);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(x: f64, y: f64) -> Vector2 {
        Vector2::new(x, y)
    }

    fn threat(x: f64, y: f64, r: f64, lethality: f64) -> Threat {
        Threat { pos: v(x, y), radius: r, lethality, kind: ThreatKind::Intel }
    }

    #[test]
    fn chord_through_the_middle_and_past_the_edge() {
        let c = v(0., 0.);
        assert!((chord_inside(v(-10., 0.), v(10., 0.), c, 5.) - 10.).abs() < 1e-9);
        assert_eq!(chord_inside(v(-10., 6.), v(10., 6.), c, 5.), 0.);
        // Starts inside: only the part up to the edge.
        assert!((chord_inside(v(0., 0.), v(10., 0.), c, 5.) - 5.).abs() < 1e-9);
        // Ends short of the circle.
        assert_eq!(chord_inside(v(-20., 0.), v(-10., 0.), c, 5.), 0.);
    }

    #[test]
    fn no_threats_is_the_straight_line() {
        let p = plan_track(v(0., 0.), v(50_000., 0.), &[]);
        assert_eq!(p.track, vec![v(0., 0.), v(50_000., 0.)]);
        assert_eq!((p.exposure, p.avoided), (0., 0));
    }

    #[test]
    fn goes_around_a_threat_on_the_line() {
        let t = [threat(25_000., 0., 8_000., 1.)];
        let p = plan_track(v(0., 0.), v(50_000., 0.), &t);
        assert_eq!(p.exposure, 0.);
        assert_eq!(p.avoided, 1);
        assert!(p.track.len() > 2);
        assert!(p.length > 50_000. && p.length < 50_000. * MAX_DETOUR_FACTOR + MAX_DETOUR_EXTRA_M);
        for w in p.track.windows(2) {
            assert_eq!(chord_inside(w[0], w[1], t[0].pos, t[0].radius), 0.);
        }
    }

    #[test]
    fn threat_off_the_line_changes_nothing() {
        let t = [threat(25_000., 20_000., 5_000., 1.)];
        let p = plan_track(v(0., 0.), v(50_000., 0.), &t);
        assert_eq!(p.track, vec![v(0., 0.), v(50_000., 0.)]);
    }

    #[test]
    fn wall_of_sams_is_crossed_at_its_weakest_point() {
        // Overlapping circles from y=-40km to y=40km: going round is far past
        // the detour limit. The one at y=16km is barely defended.
        let mut t = vec![];
        let mut y = -40_000.;
        while y <= 40_000. {
            let lethality = if y == 16_000. { 0.2 } else { 1. };
            t.push(threat(0., y, 5_000., lethality));
            y += 8_000.;
        }
        let (start, goal) = (v(-20_000., 0.), v(20_000., 0.));
        let p = plan_track(start, goal, &t);
        assert!(p.avoidable > 0., "there is no clean route");
        assert!(p.length <= 40_000. * MAX_DETOUR_FACTOR + MAX_DETOUR_EXTRA_M + 1.);
        // Cheaper than punching straight through the middle.
        let straight = exposure(start, goal, &t);
        assert!(p.exposure < straight);
        // And it crosses x=0 inside the weak circle.
        let crossing = p
            .track
            .windows(2)
            .find(|w| w[0].x <= 0. && w[1].x >= 0.)
            .map(|w| w[0].y + (w[1].y - w[0].y) * (-w[0].x) / (w[1].x - w[0].x))
            .unwrap();
        assert!(crossing > 12_000. && crossing < 20_000., "crossed at {crossing}");
    }

    #[test]
    fn target_inside_its_own_defences_is_unavoidable_not_dirty() {
        let t = [threat(50_000., 0., 6_000., 0.4)];
        let p = plan_track(v(0., 0.), v(50_000., 0.), &t);
        assert_eq!(p.avoidable, 0.);
        // Straight in: the run inside is just the radius.
        assert!((p.exposure - 6_000. * 0.4).abs() < 50.);
    }

    #[test]
    fn noe_near_threats_and_on_final_only() {
        let h = ProfileHint {
            track: vec![v(0., 0.), v(100_000., 0.)],
            threats: vec![threat(30_000., 6_000., 5_000., 1.)],
            noe_agl_m: 40.,
            land_pos: v(100_000., 0.),
        };
        assert!(h.is_noe(v(30_000., 3_500.)));
        assert!(h.is_noe(v(96_000., 0.)));
        assert!(!h.is_noe(v(60_000., 0.)));
        assert!(h.leg_is_noe(v(0., 0.), v(60_000., 0.)));
        assert!(!h.leg_is_noe(v(50_000., 0.), v(90_000., 0.)));
    }

    #[test]
    fn landing_moves_away_from_the_threat() {
        let center = v(0., 0.);
        let p = choose_landing_point(
            center,
            800.,
            &[v(0., 900.)],
            v(0., -20_000.),
            |q| (q - center).norm() <= 800.5,
            |_| true,
        );
        assert!(p.y < -500., "landed at {p:?}");
        assert!((p - center).norm() <= 800.5);
    }

    #[test]
    fn landing_with_nothing_known_is_the_centre() {
        let center = v(1_000., 1_000.);
        let p = choose_landing_point(center, 800., &[], v(0., 0.), |_| true, |_| true);
        assert_eq!(p, center);
    }

    #[test]
    fn landing_skips_unusable_ground() {
        // Centre and everything east of it is water.
        let center = v(0., 0.);
        let p = choose_landing_point(center, 800., &[], v(0., 0.), |_| true, |q| q.x < -100.);
        assert!(p.x < -100.);
        // Nothing usable at all: the centre, as before.
        let p = choose_landing_point(center, 800., &[v(0., 500.)], v(0., 0.), |_| true, |_| false);
        assert_eq!(p, center);
    }

    #[test]
    fn replans_are_rate_limited() {
        let t0 = Utc::now();
        let mut g = ReplanGate::default();
        assert_eq!(g.check(t0, 20, 2), Ok(()));
        g.record(t0);
        assert_eq!(g.check(t0 + chrono::Duration::seconds(10), 20, 2), Err(ReplanBlocked::TooSoon));
        assert_eq!(g.check(t0 + chrono::Duration::seconds(20), 20, 2), Ok(()));
        g.record(t0 + chrono::Duration::seconds(20));
        assert_eq!(g.check(t0 + chrono::Duration::seconds(300), 20, 2), Err(ReplanBlocked::Exhausted));
    }

    #[test]
    fn only_new_threats_across_the_rest_of_the_route_count() {
        let track = vec![v(0., 0.), v(50_000., 0.)];
        let planned = vec![threat(25_000., 20_000., 5_000., 1.)];
        // The same contact drifted a little and its ring grew a touch: old news.
        let drifted = [threat(25_400., 19_700., 5_300., 1.)];
        let rest = remaining_track(&track, v(10_000., 0.));
        assert!(new_threat_on_route(&rest, &planned, &drifted).is_none());
        // Something new ahead on the line: re-route.
        let ahead = [threat(30_000., 0., 4_000., 1.)];
        assert!(new_threat_on_route(&rest, &planned, &ahead).is_some());
        // Something new behind the helo: nothing to do.
        let behind = [threat(3_000., 0., 2_000., 1.)];
        assert!(new_threat_on_route(&rest, &planned, &behind).is_none());
    }
}
