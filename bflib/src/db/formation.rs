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

//! Ground formations (`Cfg::ground_war`).
//!
//! A formation is a handful of an objective's own garrison groups -- armour,
//! infantry, a little AAA -- that have left the base. They come out of
//! `obj.groups` while they are away, so the base is weaker for it, and the
//! base's cull/respawn, ghost check and repair no longer touch them: this
//! module owns them until they go home.
//!
//! A formation lives in one of two states:
//!
//! - On the map only. Its units are not in DCS; the engine moves it along
//!   the road path it planned and keeps every unit's persisted position up
//!   to date, so it can be put into the world at any moment exactly where it
//!   is.
//! - Live. Its groups are real DCS groups following the same path, and DCS
//!   fights whatever they meet. A formation goes live when a player comes
//!   near, while it is carrying out a player's order, when it meets an
//!   enemy formation or comes within reach of any enemy ground unit that is
//!   already in DCS, or when it closes on the objective it is attacking --
//!   as many as `max_live_formations` allow.
//!   Two enemy formations that meet while the budget is spent halt and fight
//!   it out on the map instead.
//!
//! Fighting on the map is a model, not a coin toss (`super::combat`,
//! `GroundCombatCfg`): firepower by vehicle type, speed of the slowest
//! vehicle, supply that runs down and is made good near friendly bases,
//! morale that breaks, columns that have to deploy before they can fight,
//! positions that get dug in, and garrisons that fight back from cover. A
//! side only sees enemy formations its own forces are close enough to see,
//! and remembers where it saw them.
//!
//! An attack ends the way a player assault does: once the target is down to
//! the point where it can be taken, the formation's infantry goes in as a
//! capture-capable dismount squad, and the ordinary capture timer and
//! consolidation hold take it from there. A formation without infantry can
//! break a base but not take it.
//!
//! A formation that withdraws to a friendly base, or is wiped out, rejoins
//! that base's garrison: the survivors are back at their posts and the dead
//! are rebuilt by the base's repair and reinforcement like any other
//! garrison loss.

use super::{
    combat::{self, Condition, Deployment, Role},
    group::DeployKind,
    objective::ObjGroupClass,
    persisted::Persisted,
    Db,
};
use crate::{
    group, group_mut, objective, objective_mut,
    spawnctx::{Despawn, SpawnCtx, SpawnLoc},
    unit_mut,
};
use anyhow::{anyhow, bail, Context, Result};
use bfprotocols::{
    cfg::{GroundCombatCfg, GroundWarCfg, UnitTag},
    db::{
        group::{GroupId, UnitId},
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::PerfInner,
};
use chrono::{prelude::*, Duration};
use compact_str::{format_compact, CompactString};
use dcso3::{
    centroid2d,
    coalition::Side,
    controller::{
        ActionTyp, AiOption, AlarmState, AltType, GroundOption, GroundRoe, MissionPoint, PointType, Task,
        VehicleFormation,
    },
    env::miz::MizIndex,
    group::{Group, GroupCategory},
    land::{Land, RoadType, SurfaceType},
    net::Ucid,
    trigger::{ArrowSpec, LineType, MarkId, SideFilter},
    object::DcsObject,
    unit::Unit,
    LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3,
};
use enumflags2::BitFlags;
use fxhash::{FxHashMap, FxHashSet};
use log::{info, warn};
use serde_derive::{Deserialize, Serialize};
use smallvec::SmallVec;
use std::collections::VecDeque;

pub type FormationId = u32;

/// Spacing between vehicles when the engine lays a formation out on the map.
const COLUMN_GAP_M: f64 = 30.;
/// A live formation that has moved less than this...
const STALL_MOVE_M: f64 = 150.;
/// ...in this long, with no enemy close, is stuck on the terrain.
const STALL_SECS: i64 = 300;
/// An enemy this close means a formation is in contact: fighting, not
/// stuck, and a battle on the map (`super::battle`).
pub(super) const ENGAGED_M: f64 = 6_000.;
/// Road paths are kept at about this point spacing on the map...
const PATH_SPACING_M: f64 = 200.;
/// ...and handed to DCS at about this one (DCS chokes on long routes).
const ROUTE_SPACING_M: f64 = 2_500.;
const MAX_ROUTE_POINTS: usize = 50;
/// A road path this much longer than the straight line is a detour around
/// half the map; drive across country instead.
const ROAD_DETOUR_FACTOR: f64 = 2.5;
/// Live units whose positions are read from DCS per tick.
const SYNC_UNITS_PER_TICK: usize = 48;
/// Map pins are redrawn at most this often per formation.
const PIN_MIN_SECS: i64 = 120;
const PIN_MOVE_M: f64 = 2_000.;
/// After an assault squad goes in, wait this long before sending another
/// at the same objective.
const ASSAULT_COOLDOWN_SECS: i64 = 600;

fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

fn heading_of(v: Vector2) -> f64 {
    v.y.atan2(v.x)
}

/// Distance from `p` to the segment `a`-`b`.
fn seg_dist(a: Vector2, b: Vector2, p: Vector2) -> f64 {
    let ab = b - a;
    let len2 = ab.norm_squared();
    if len2 < 1. {
        return dist(a, p);
    }
    let t = ((p - a).dot(&ab) / len2).clamp(0., 1.);
    dist(a + ab * t, p)
}

fn ordinal(n: u32) -> CompactString {
    let suffix = match (n % 10, n % 100) {
        (_, 11..=13) => "th",
        (1, _) => "st",
        (2, _) => "nd",
        (3, _) => "rd",
        _ => "th",
    };
    format_compact!("{n}{suffix}")
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Order {
    /// Stay where it is and fight anything that comes.
    Hold,
    /// March on an enemy (or neutral) objective, break it and take it.
    Attack(ObjectiveId),
    /// March to a friendly objective and hold it.
    Defend(ObjectiveId),
    /// Fall back to a friendly objective and rejoin its garrison.
    Withdraw(ObjectiveId),
}

impl Order {
    pub fn target(&self) -> Option<ObjectiveId> {
        match self {
            Order::Hold => None,
            Order::Attack(o) | Order::Defend(o) | Order::Withdraw(o) => Some(*o),
        }
    }

    pub fn verb(&self) -> &'static str {
        match self {
            Order::Hold => "holding",
            Order::Attack(_) => "attacking",
            Order::Defend(_) => "defending",
            Order::Withdraw(_) => "withdrawing to",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Posture {
    Moving,
    Holding,
    /// At the objective it was sent to take, fighting for it.
    Assaulting,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Formation {
    pub id: FormationId,
    pub name: String,
    pub side: Side,
    /// The objective it was raised from. Survivors rejoin it here.
    pub home: ObjectiveId,
    pub groups: Vec<GroupId>,
    /// Vehicles it set out with.
    pub strength0: u32,
    pub order: Order,
    pub posture: Posture,
    /// Where it is: the centre of its live units when it is in DCS, the head
    /// of the column when it is on the map only.
    pub pos: Vector2,
    pub heading: f64,
    /// The rest of its route, next point first, destination last.
    #[serde(default)]
    pub path: Vec<Vector2>,
    /// Route points it has passed, oldest first (bounded): the road behind
    /// the head of the column, where the rest of the column is laid out.
    #[serde(default)]
    pub trail: Vec<Vector2>,
    /// The path is a straight cross-country line, not a road.
    #[serde(default)]
    pub off_road: bool,
    /// The player whose order it is carrying out, if it is not the AI's.
    #[serde(default)]
    pub commander: Option<Ucid>,
    /// The AI leaves it alone until then.
    #[serde(default)]
    pub locked_until: Option<DateTime<Utc>>,
    pub order_ts: DateTime<Utc>,
    pub created: DateTime<Utc>,
    /// Fuel and ammunition carried, 0..1 (`GroundCombatCfg`).
    #[serde(default = "full")]
    pub supply: f64,
    /// 0..1. Falls with losses and isolation; below `break_morale` the
    /// formation breaks.
    #[serde(default = "full")]
    pub morale: f64,
    #[serde(default)]
    pub deployment: Deployment,
    /// When it took up its current deployment: deploying turns into
    /// deployed, and holding into dug in, after a while.
    #[serde(default)]
    pub deployment_ts: Option<DateTime<Utc>>,
    /// Its morale has collapsed and it is falling back.
    #[serde(default)]
    pub broken: bool,
    /// Vehicles it has lost, and enemy vehicles it has destroyed.
    #[serde(default)]
    pub losses: u32,
    #[serde(default)]
    pub kills: u32,
    /// Damage taken that hasn't yet added up to a vehicle lost.
    #[serde(default)]
    pub damage: f64,
}

fn full() -> f64 {
    1.
}

impl Formation {
    pub fn ai_controlled(&self, now: DateTime<Utc>) -> bool {
        self.locked_until.map_or(true, |t| now >= t)
    }

    /// On the march under a player's Move order: going somewhere, not
    /// fighting. It keeps to column and drives on past an enemy it merely
    /// sees, where an attack, a defence or the AI's own moves stop and
    /// deploy. A Move is the only player order that leaves `Order::Hold`.
    pub fn marching(&self, now: DateTime<Utc>) -> bool {
        self.posture == Posture::Moving
            && self.order == Order::Hold
            && self.commander.is_some()
            && !self.ai_controlled(now)
            && !self.broken
    }

    pub fn destination(&self) -> Option<Vector2> {
        self.path.last().copied()
    }

    pub fn path_len_m(&self) -> f64 {
        let mut prev = self.pos;
        let mut d = 0.;
        for p in &self.path {
            d += dist(prev, *p);
            prev = *p;
        }
        d
    }
}

#[derive(Debug, Clone)]
struct Pin {
    mark: MarkId,
    arrow: Option<MarkId>,
    pos: Vector2,
    text: CompactString,
    ts: DateTime<Utc>,
}

/// Session state for the formations. Nothing here survives a restart, and
/// nothing needs to: after a restart no formation is in DCS, and the rest is
/// rebuilt as it goes.
#[derive(Debug, Default)]
pub struct FormationRt {
    /// Formation groups that exist in DCS right now.
    live: FxHashSet<GroupId>,
    /// When each formation last had a reason to be live.
    wanted_ts: FxHashMap<FormationId, DateTime<Utc>>,
    /// Formations held on the map at the edge of a fight the live budget
    /// can't cover.
    halted: FxHashSet<FormationId>,
    spawnq: VecDeque<(FormationId, GroupId)>,
    /// Live formations whose DCS groups need their route (re)issued.
    route_dirty: FxHashSet<FormationId>,
    /// (anchor, since, re-routes so far)
    stall: FxHashMap<FormationId, (Vector2, DateTime<Utc>, u8)>,
    assault_ts: FxHashMap<ObjectiveId, DateTime<Utc>>,
    /// Formations already told they have no infantry for an assault.
    no_infantry_said: FxHashSet<FormationId>,
    pins: FxHashMap<FormationId, Pin>,
    sync_cursor: usize,
    last_tick: Option<DateTime<Utc>>,
    /// Battles, smoke and burning wrecks (`super::battle`).
    pub(super) battle: super::battle::BattleRt,
    /// Where each side last saw each enemy formation.
    spotted: FxHashMap<(Side, FormationId), Sighting>,
    /// When the last spotting pass ran: a sighting from it is in sight now.
    spot_ts: Option<DateTime<Utc>>,
    /// How far anyone can see right now, as a share of a clear day
    /// (`set_visibility`). `None` = clear day.
    visibility: Option<f64>,
    /// Damage a garrison has taken that hasn't killed anything yet.
    garrison_damage: FxHashMap<ObjectiveId, f64>,
    last_combat: Option<DateTime<Utc>>,
    /// When each formation was last in contact.
    contact_ts: FxHashMap<FormationId, DateTime<Utc>>,
    /// Formations a friendly base is keeping supplied, as of the last tick.
    supplied: FxHashSet<FormationId>,
    /// Formations already told they are cut off.
    cut_off_said: FxHashSet<FormationId>,
    /// Each side's recent ground-war events, oldest first.
    events: FxHashMap<Side, VecDeque<Event>>,
    /// Real fire missions sent in support, by (side, ~3 km cell), so the
    /// batteries aren't re-tasked onto the same spot every round.
    fire_ts: FxHashMap<(Side, i64, i64), DateTime<Utc>>,
    /// When each side was last told its artillery is firing near a ~5 km
    /// cell.
    fire_said: FxHashMap<(Side, i64, i64), DateTime<Utc>>,
}

/// Where a side last saw an enemy formation.
#[derive(Debug, Clone)]
pub struct Sighting {
    pub pos: Vector2,
    pub heading: f64,
    pub at: DateTime<Utc>,
    pub moving: bool,
    /// Close enough to make out its vehicles.
    pub close: bool,
    /// What it looked like then: its make-up and how many vehicles it had.
    pub kind: &'static str,
    pub alive: u32,
}

#[derive(Debug, Clone)]
pub struct Event {
    pub at: DateTime<Utc>,
    pub kind: &'static str,
    pub text: CompactString,
    pub pos: Option<Vector2>,
    pub formation: Option<FormationId>,
}

/// Events kept per side.
const MAX_EVENTS: usize = 60;

impl FormationRt {
    pub fn is_live(&self, f: &Formation) -> bool {
        f.groups.iter().any(|g| self.live.contains(g))
    }

    pub fn is_halted(&self, id: FormationId) -> bool {
        self.halted.contains(&id)
    }

    pub fn live_group(&self, gid: &GroupId) -> bool {
        self.live.contains(gid)
    }

    pub fn battles(&self) -> &[super::battle::Battle] {
        self.battle.battles()
    }

    pub fn live_count(&self, db: &Db) -> usize {
        db.persisted
            .formations
            .into_iter()
            .filter(|(_, f)| self.is_live(f))
            .count()
    }

    /// `side`'s sightings of enemy formations: (which, where and when).
    pub fn sightings(&self, side: Side) -> impl Iterator<Item = (FormationId, &Sighting)> {
        self.spotted.iter().filter(move |((s, _), _)| *s == side).map(|((_, id), v)| (*id, v))
    }

    /// The light and the weather: share of the daytime spotting range
    /// anyone has right now.
    pub fn set_visibility(&mut self, v: f64) {
        self.visibility = Some(v);
    }

    pub fn visibility(&self) -> f64 {
        self.visibility.unwrap_or(1.)
    }

    /// Seen on the latest spotting pass, as opposed to remembered.
    pub fn in_sight(&self, s: &Sighting) -> bool {
        Some(s.at) == self.spot_ts
    }

    pub fn in_supply(&self, id: FormationId) -> bool {
        self.supplied.contains(&id)
    }

    /// `side`'s recent events, newest first.
    pub fn events(&self, side: Side) -> impl Iterator<Item = &Event> {
        self.events.get(&side).into_iter().flat_map(|q| q.iter().rev())
    }

    pub(crate) fn event(
        &mut self,
        side: Side,
        kind: &'static str,
        text: impl Into<CompactString>,
        pos: Option<Vector2>,
        formation: Option<FormationId>,
        now: DateTime<Utc>,
    ) {
        let q = self.events.entry(side).or_default();
        q.push_back(Event { at: now, kind, text: text.into(), pos, formation });
        while q.len() > MAX_EVENTS {
            q.pop_front();
        }
    }
}

/// Why a formation wants to be in DCS, best reason first.
#[derive(Debug, Clone, Copy, Default)]
struct Want {
    player: bool,
    /// Carrying out a player's order.
    ordered: bool,
    contact: bool,
    /// An enemy ground unit that is in DCS is within reach: only a live
    /// formation can fight it, but unlike `contact` it is no reason to halt
    /// when there is no room to go live.
    enemy: bool,
    target: bool,
    assault: bool,
    /// On the move with `live_only`: it can only drive in DCS.
    moving: bool,
}

impl Want {
    fn any(&self) -> bool {
        self.player || self.ordered || self.contact || self.enemy || self.target || self.assault || self.moving
    }

    fn reason(&self) -> &'static str {
        if self.ordered {
            "under a player's orders"
        } else if self.player {
            "a player is near"
        } else if self.assault {
            "assaulting"
        } else if self.contact {
            "in contact with an enemy formation"
        } else if self.enemy {
            "enemy units in DCS within reach"
        } else if self.target {
            "closing on its target"
        } else {
            "on the move"
        }
    }

    fn fighting(&self) -> bool {
        self.contact || self.target || self.assault
    }

    fn priority(&self) -> u32 {
        (self.ordered as u32) * 16
            + (self.player as u32) * 8
            + (self.assault as u32) * 4
            + (self.contact as u32) * 2
            + (self.target || self.enemy || self.moving) as u32
    }
}

/// Point `back` metres behind the head at `pos`, along `trail` (newest last),
/// and the heading of travel there. Past the end of the trail the column
/// carries on straight back.
fn back_along(pos: Vector2, heading: f64, trail: &[Vector2], back: f64) -> (Vector2, f64) {
    let mut cur = pos;
    let mut left = back;
    let mut dir_fwd = Vector2::new(heading.cos(), heading.sin());
    for prev in trail.iter().rev() {
        let seg = *prev - cur;
        let len = seg.norm();
        if len < 1. {
            continue;
        }
        dir_fwd = -seg / len;
        if len >= left {
            return (cur + seg / len * left, heading_of(dir_fwd));
        }
        left -= len;
        cur = *prev;
    }
    (cur - dir_fwd * left, heading_of(dir_fwd))
}

/// Slot `i` of `n` in a formation deployed in line abreast at `center`
/// facing `heading`: one rank for a company, two for anything bigger, ~70 m
/// between vehicles and 120 m between ranks.
fn line_slot(center: Vector2, heading: f64, i: usize, n: usize) -> Vector2 {
    let per_rank = if n <= 8 { n.max(1) } else { (n + 1) / 2 };
    let rank = (i / per_rank) as f64;
    let file = (i % per_rank) as f64 - (per_rank as f64 - 1.) / 2.;
    let fwd = Vector2::new(heading.cos(), heading.sin());
    let right = Vector2::new(-heading.sin(), heading.cos());
    center + right * (file * 70.) - fwd * (rank * 120.)
}

/// Slot `i` of a formation drawn up around `center`: rings of eight.
fn ring_slot(center: Vector2, i: usize) -> Vector2 {
    let ring = (i / 8) as f64;
    let a = (i % 8) as f64 * std::f64::consts::TAU / 8. + ring * 0.4;
    center + Vector2::new(a.cos(), a.sin()) * (80. + ring * 60.)
}

/// Thin a road polyline to about `spacing` metres between points, always
/// keeping the last.
fn decimate(pts: &[Vector2], spacing: f64) -> Vec<Vector2> {
    let mut out: Vec<Vector2> = vec![];
    for (i, p) in pts.iter().enumerate() {
        let last = i + 1 == pts.len();
        match out.last() {
            Some(q) if dist(*q, *p) < spacing && !last => (),
            _ => out.push(*p),
        }
    }
    out
}

pub(super) fn ground_point<'lua>(
    land: &Land<'lua>,
    pos: Vector2,
    formation: VehicleFormation,
    speed: f64,
    task: Task<'lua>,
) -> MissionPoint<'lua> {
    MissionPoint {
        action: Some(ActionTyp::Ground(formation)),
        airdrome_id: None,
        helipad: None,
        typ: PointType::TurningPoint,
        link_unit: None,
        pos: LuaVec2(pos),
        alt: land.get_height(LuaVec2(pos)).unwrap_or(0.),
        alt_typ: Some(AltType::BARO),
        time_re_fu_ar: None,
        eta: None,
        eta_locked: None,
        speed,
        speed_locked: None,
        name: None,
        task: Box::new(task),
    }
}

/// The DCS route for one of a formation's groups from `from` along `path`.
/// An empty path is a halt where it stands.
fn dcs_route<'lua>(
    land: &Land<'lua>,
    from: Vector2,
    path: &[Vector2],
    off_road: bool,
    deployed: bool,
    speed_mps: f64,
) -> Vec<MissionPoint<'lua>> {
    // Deployed for a fight, it advances in line abreast across country; on
    // the march it keeps to the road.
    let along = if deployed {
        VehicleFormation::Rank
    } else if off_road {
        VehicleFormation::OffRoad
    } else {
        VehicleFormation::OnRoad
    };
    let speed = if off_road && !deployed { speed_mps * 0.5 } else { speed_mps };
    let mut start = ground_point(land, from, VehicleFormation::OffRoad, speed, Task::ComboTask(vec![]));
    // As the supply convoys' routes, which DCS does drive: the start point
    // is now, at this speed.
    start.eta = Some(dcso3::Time(0.));
    start.eta_locked = Some(true);
    start.speed_locked = Some(true);
    let mut route = vec![start];
    if path.is_empty() {
        return route;
    }
    let mut spacing = ROUTE_SPACING_M;
    let mut pts = decimate(path, spacing);
    while pts.len() > MAX_ROUTE_POINTS {
        spacing *= 1.5;
        pts = decimate(path, spacing);
    }
    let n = pts.len();
    for (i, p) in pts.into_iter().enumerate() {
        if i + 1 == n {
            // The last leg is off road: an "On Road" end point snaps to the
            // road nearest the objective, which can be kilometres short.
            route.push(ground_point(
                land,
                p,
                VehicleFormation::OffRoad,
                speed_mps * 0.5,
                Task::ComboTask(vec![Task::WrappedOption(AiOption::Ground(
                    GroundOption::AlarmState(AlarmState::Red),
                ))]),
            ));
        } else {
            route.push(ground_point(land, p, along.clone(), speed, Task::ComboTask(vec![])));
        }
    }
    route
}

/// March discipline for a formation's DCS group: keep the column on the
/// road under fire (dispersing breaks DCS columns for good), fight when it
/// sees the enemy. On a player's Move (`marching`) it only returns fire:
/// weapons free, DCS halts a ground group to engage anything it sees in
/// range, so an ordered column parked on its first waypoint the moment an
/// enemy came into view and never drove on.
fn set_march_ai(group: &Group, marching: bool) -> Result<()> {
    let con = group.get_controller()?;
    con.set_option(AiOption::Ground(GroundOption::DisperseOnAttack(0)))?;
    con.set_option(AiOption::Ground(GroundOption::AlarmState(AlarmState::Auto)))?;
    let roe = if marching { GroundRoe::ReturnFire } else { GroundRoe::WeaponFree };
    con.set_option(AiOption::Ground(GroundOption::Roe(roe)))?;
    Ok(())
}

/// (vehicles alive, vehicles it set out with) for `f`.
pub fn strength_in(persisted: &Persisted, f: &Formation) -> (u32, u32) {
    let mut alive = 0;
    for gid in &f.groups {
        if let Some(g) = persisted.groups.get(gid) {
            for uid in &g.units {
                if persisted.units.get(uid).map_or(false, |u| !u.dead) {
                    alive += 1;
                }
            }
        }
    }
    (alive, f.strength0.max(alive))
}

/// Formations as weights on the F10 front line (`(x, y, signed weight)`,
/// blue positive), scaled by how much of each is left.
pub fn pressure(persisted: &Persisted, weight: f64) -> Vec<(f64, f64, f64)> {
    if weight <= 0. {
        return vec![];
    }
    persisted
        .formations
        .into_iter()
        .filter_map(|(_, f)| {
            let (alive, total) = strength_in(persisted, f);
            let pct = if total == 0 { 0. } else { alive as f64 / total as f64 };
            let sign = match f.side {
                Side::Blue => 1.,
                Side::Red => -1.,
                Side::Neutral => return None,
            };
            (pct > 0.).then(|| (f.pos.x, f.pos.y, sign * weight * pct))
        })
        .collect()
}

impl Db {
    pub(crate) fn ground_war_cfg(&self) -> Result<GroundWarCfg> {
        self.ephemeral
            .cfg
            .ground_war
            .clone()
            .filter(|c| c.enabled)
            .ok_or_else(|| anyhow!("the ground war is not enabled on this server"))
    }

    pub fn formations(&self) -> impl Iterator<Item = &Formation> {
        self.persisted.formations.into_iter().map(|(_, f)| f)
    }

    pub fn formation(&self, id: FormationId) -> Option<&Formation> {
        self.persisted.formations.get(&id)
    }

    /// (vehicles alive, vehicles it set out with)
    pub fn formation_strength(&self, f: &Formation) -> (u32, u32) {
        strength_in(&self.persisted, f)
    }

    pub fn formation_strength_pct(&self, f: &Formation) -> u32 {
        let (alive, total) = self.formation_strength(f);
        if total == 0 { 0 } else { alive * 100 / total }
    }

    /// What a unit is, from its DCS type's tags; a type the config doesn't
    /// classify goes by its group's class.
    pub fn unit_role(&self, typ: &bfprotocols::cfg::Vehicle, class: ObjGroupClass) -> Role {
        match self.ephemeral.cfg.unit_classification.get(typ) {
            Some(t) if !t.0.is_empty() => Role::of(t.0),
            _ => match class {
                ObjGroupClass::Armor => Role::Tank,
                ObjGroupClass::Infantry => Role::Infantry,
                ObjGroupClass::Aaa => Role::Aaa,
                ObjGroupClass::Lr | ObjGroupClass::Mr | ObjGroupClass::Sr => Role::Sam,
                _ => Role::Truck,
            },
        }
    }

    /// `f`'s live vehicles and their roles.
    pub fn formation_units(&self, f: &Formation) -> SmallVec<[(UnitId, Role); 32]> {
        let mut out = SmallVec::new();
        for gid in &f.groups {
            let Some(g) = self.persisted.groups.get(gid) else { continue };
            for uid in &g.units {
                let Some(u) = self.persisted.units.get(uid) else { continue };
                if !u.dead {
                    out.push((*uid, self.unit_role(&u.typ, g.class)));
                }
            }
        }
        out
    }

    pub fn formation_condition(&self, f: &Formation) -> Condition {
        Condition { supply: f.supply, morale: f.morale, deployment: f.deployment, broken: f.broken }
    }

    /// (combat power now, raw firepower at full strength).
    pub fn formation_power(&self, cfg: &GroundCombatCfg, f: &Formation) -> (f64, f64) {
        let alive: SmallVec<[Role; 32]> = self.formation_units(f).into_iter().map(|(_, r)| r).collect();
        let now = combat::raw_power(cfg, &alive) * self.formation_condition(f).factor(cfg);
        let mut full = 0.;
        for gid in &f.groups {
            let Some(g) = self.persisted.groups.get(gid) else { continue };
            for uid in &g.units {
                if let Some(u) = self.persisted.units.get(uid) {
                    full += self.unit_role(&u.typ, g.class).firepower(cfg);
                }
            }
        }
        (now, full)
    }

    /// "armour" | "mechanised" | "motorised" | "infantry", from what is left.
    pub fn formation_kind(&self, f: &Formation) -> &'static str {
        let units = self.formation_units(f);
        let has = |r: Role| units.iter().any(|(_, x)| *x == r);
        let tanks = has(Role::Tank);
        let carriers = has(Role::Ifv) || has(Role::Apc);
        let trucks = has(Role::Truck);
        let inf = has(Role::Infantry);
        match (tanks, carriers || inf) {
            (true, true) => "mechanised",
            (true, false) => "armour",
            _ if carriers || trucks => "motorised",
            _ => "infantry",
        }
    }

    /// How fast `f` moves right now, km/h: its slowest vehicle on road or
    /// across country, stopped while it deploys, slowed advancing in
    /// contact and when it is out of fuel. A broken formation just runs.
    pub fn formation_speed_kph(&self, cfg: &GroundWarCfg, f: &Formation) -> f64 {
        let roles: SmallVec<[Role; 32]> = self.formation_units(f).into_iter().map(|(_, r)| r).collect();
        let (road, off) = combat::speed_of(&roles, cfg.speed_kph);
        let mut v = if f.off_road { off } else { road };
        if !f.broken {
            match f.deployment {
                Deployment::Deploying => v = 0.,
                Deployment::Deployed | Deployment::DugIn => v *= cfg.combat.contact_speed.clamp(0.05, 1.),
                Deployment::Column => (),
            }
        }
        if f.supply < cfg.combat.low_supply {
            v *= 0.5;
        }
        v
    }

    /// How much of the usual supply `f` burns: its own trucks carry fuel and
    /// ammunition forward, so a formation with plenty of them lasts longer.
    fn supply_use_factor(&self, f: &Formation) -> f64 {
        let units = self.formation_units(f);
        if units.is_empty() {
            return 1.;
        }
        let trucks = units.iter().filter(|(_, r)| *r == Role::Truck).count() as f64;
        1. - 0.4 * (trucks * 2. / units.len() as f64).min(1.)
    }

    /// Take `loads` vehicles' worth of supply out of `oid`'s warehouse: with
    /// the materiel commodity, `materiel_per_vehicle` units of it each;
    /// otherwise a `base_drain_per_vehicle` share of every stocked item.
    fn drain_base(&mut self, cfg: &GroundCombatCfg, oid: ObjectiveId, loads: f64) {
        if loads <= 0. {
            return;
        }
        let materiel = self
            .ephemeral
            .cfg
            .warehouse
            .as_ref()
            .and_then(|w| w.materiel.as_ref())
            .map_or(false, |m| m.enabled);
        let production = self
            .persisted
            .objectives
            .get(&oid)
            .and_then(|o| self.ephemeral.production_by_side.get(&o.owner).cloned());
        let Some(obj) = self.persisted.objectives.get_mut_cow(&oid) else { return };
        if materiel {
            let key = String::from(bfprotocols::cfg::MATERIEL_ITEM);
            if let Some(inv) = obj.warehouse.equipment.get_mut_cow(&key) {
                *inv -= (loads * cfg.materiel_per_vehicle).ceil() as u32;
            }
        } else if let Some(production) = production {
            let frac = ((loads * cfg.base_drain_per_vehicle) as f32).clamp(0., 0.5);
            for name in production.equipment.keys() {
                if let Some(inv) = obj.warehouse.equipment.get_mut_cow(name) {
                    inv.reduce(frac);
                }
            }
            for liq in production.liquids.keys() {
                if let Some(inv) = obj.warehouse.liquids.get_mut_cow(liq) {
                    inv.reduce(frac);
                }
            }
        }
        self.ephemeral.dirty();
    }

    /// " near <objective>" for the nearest objective within 20 km, else "".
    fn near_text(&self, pos: Vector2) -> CompactString {
        self.objectives()
            .map(|(_, o)| (dist(o.pos(), pos), o))
            .filter(|(d, _)| *d <= 20_000.)
            .min_by(|a, b| a.0.total_cmp(&b.0))
            .map(|(_, o)| format_compact!(" near {}", o.name))
            .unwrap_or_default()
    }

    /// Whether `f` can take a base: it has an infantry group, or a vehicle
    /// that carries a squad (see `troop_carrier`).
    pub fn formation_can_assault(&self, f: &Formation) -> bool {
        self.infantry_group(f).is_some() || self.troop_carrier(f).is_some()
    }

    fn infantry_group(&self, f: &Formation) -> Option<GroupId> {
        f.groups.iter().copied().find(|gid| {
            self.persisted.groups.get(gid).map_or(false, |g| {
                g.class.is_infantry()
                    && g.units
                        .into_iter()
                        .any(|u| self.persisted.units.get(u).map_or(false, |u| !u.dead))
            })
        })
    }

    /// A live vehicle in `f` that carries infantry -- an IFV or APC with a
    /// `cfg.dismount` squad for `f`'s side: (its group, where it is, the
    /// squad's template). Garrisons keep their last infantry group at home,
    /// so on most maps this is how an armoured formation takes a base.
    fn troop_carrier(&self, f: &Formation) -> Option<(GroupId, Vector2, String)> {
        for gid in &f.groups {
            let Some(g) = self.persisted.groups.get(gid) else { continue };
            for uid in &g.units {
                let Some(u) = self.persisted.units.get(uid) else { continue };
                if u.dead {
                    continue;
                }
                let squad = self.ephemeral.cfg.dismount.get(&u.typ).and_then(|d| d.template.get(&f.side));
                if let Some(t) = squad {
                    return Some((*gid, u.pos, t.clone()));
                }
            }
        }
        None
    }

    /// One line describing a formation, for the menus and the dashboard.
    pub fn formation_status(&self, f: &Formation, now: DateTime<Utc>) -> CompactString {
        let (alive, total) = self.formation_strength(f);
        let target = f
            .order
            .target()
            .and_then(|o| self.persisted.objectives.get(&o))
            .map(|o| o.name.clone());
        let order = match (&f.order, target) {
            (Order::Hold, _) => CompactString::new("holding position"),
            (o, Some(t)) => format_compact!("{} {t}", o.verb()),
            (o, None) => CompactString::new(o.verb()),
        };
        let state = match f.posture {
            Posture::Moving => format_compact!(", {:.0} km to go", f.path_len_m() / 1000.),
            Posture::Assaulting => CompactString::new(", in the assault"),
            Posture::Holding => CompactString::new(""),
        };
        let who = match (&f.commander, f.ai_controlled(now)) {
            (Some(ucid), false) => self
                .persisted
                .players
                .get(ucid)
                .map(|p| format_compact!(", under {}", p.name))
                .unwrap_or_default(),
            _ => CompactString::new(""),
        };
        format_compact!("{}: {order}{state}, {alive}/{total} vehicles{who}", f.name)
    }

    /// A formation's F10 pin. Coarser than `formation_status` on purpose: a
    /// pin is redrawn whenever its text changes, and a distance-to-go would
    /// change on every tick of every march.
    fn formation_pin_text(&self, f: &Formation) -> CompactString {
        let (alive, total) = self.formation_strength(f);
        let target = f
            .order
            .target()
            .and_then(|o| self.persisted.objectives.get(&o))
            .map(|o| o.name.clone());
        let order = match (&f.order, target) {
            (Order::Hold, _) => CompactString::new("holding"),
            (o, Some(t)) => format_compact!("{} {t}", o.verb()),
            (o, None) => CompactString::new(o.verb()),
        };
        let quarter = if total == 0 { 0 } else { (alive * 4 + total - 1) / total * 25 };
        format_compact!("{}\n{order} | ~{quarter}% strength", f.name)
    }

    /// Put every formation back into its garrison: the ground war has been
    /// switched off, and nothing else would ever bring them home.
    pub fn disband_all_formations(&mut self, rt: &mut FormationRt, now: DateTime<Utc>) {
        let all: SmallVec<[(FormationId, ObjectiveId); 16]> =
            self.formations().map(|f| (f.id, f.home)).collect();
        for (id, home) in all {
            if let Err(e) = self.dissolve_formation(rt, id, home, now) {
                warn!("ground war: disbanding formation {id}: {e:?}");
                self.persisted.formations.remove_cow(&id);
                self.forget_formation(rt, id);
            }
        }
        self.ephemeral.dirty();
    }

    /// The garrison groups `oid` can send out on `side`'s behalf, best first:
    /// an infantry group (so the formation can take ground), armour, more
    /// infantry, then one AAA group for air cover. The last infantry group
    /// and `keep_home_combat_groups` combat groups always stay home.
    pub fn formation_candidates(
        &self,
        cfg: &GroundWarCfg,
        oid: &ObjectiveId,
        side: Side,
    ) -> Result<Vec<GroupId>> {
        let obj = objective!(self, oid)?;
        let Some(groups) = obj.groups.get(&side) else { return Ok(vec![]) };
        let (mut armor, mut inf, mut aaa) = (vec![], vec![], vec![]);
        for gid in groups {
            let g = group!(self, gid)?;
            if g.kind != Some(GroupCategory::Ground) {
                continue;
            }
            // Emplaced and towed guns (a KS-19, a ZU-23 emplacement, a
            // mortar) stay put whatever they are told, and hold their whole
            // group with them.
            if !self.group_can_drive(gid) {
                continue;
            }
            let (alive, total) = self.group_health(gid)?;
            // A group that has lost half its vehicles stays to be rebuilt.
            if alive == 0 || alive * 2 < total {
                continue;
            }
            match g.class {
                ObjGroupClass::Armor => armor.push(*gid),
                ObjGroupClass::Infantry => inf.push(*gid),
                ObjGroupClass::Aaa => aaa.push(*gid),
                _ => (),
            }
        }
        // The base's last infantry group never leaves: a base with no
        // infantry reads 0% and can be walked into.
        let home_inf = inf.pop().is_some() as usize;
        let keep_more = (cfg.keep_home_combat_groups as usize).saturating_sub(home_inf);
        let spare_combat = (armor.len() + inf.len()).saturating_sub(keep_more);
        let max = cfg.groups_per_formation.max(1) as usize;
        let mut out = vec![];
        let mut combat_taken = 0;
        let take = |pool: &mut Vec<GroupId>, out: &mut Vec<GroupId>, combat_taken: &mut usize| {
            if out.len() < max && *combat_taken < spare_combat {
                if let Some(g) = pool.pop() {
                    out.push(g);
                    *combat_taken += 1;
                }
            }
        };
        take(&mut inf, &mut out, &mut combat_taken);
        while out.len() < max && combat_taken < spare_combat && !(armor.is_empty() && inf.is_empty()) {
            if !armor.is_empty() {
                take(&mut armor, &mut out, &mut combat_taken);
            } else {
                take(&mut inf, &mut out, &mut combat_taken);
            }
        }
        // Air cover only for a real formation, and never the base's only AAA.
        if cfg.take_aaa && !out.is_empty() && out.len() < max && aaa.len() >= 2 {
            out.extend(aaa.pop());
        }
        Ok(out)
    }

    /// Form a new formation out of `oid`'s garrison.
    pub fn raise_formation(
        &mut self,
        rt: &mut FormationRt,
        lua: MizLua,
        side: Side,
        oid: ObjectiveId,
        by: Option<Ucid>,
        now: DateTime<Utc>,
    ) -> Result<FormationId> {
        let cfg = self.ground_war_cfg()?;
        let in_field = self.formations().filter(|f| f.side == side).count();
        if in_field >= cfg.max_formations_per_side as usize {
            bail!("{side:?} already has {in_field} formations in the field, the most it can have")
        }
        let total = self.formations().count();
        if cfg.live_only && total >= cfg.max_live_formations as usize {
            bail!("the server can run no more than {total} formations in DCS at once")
        }
        let obj = objective!(self, oid)?;
        let oname = obj.name.clone();
        if obj.owner != side {
            bail!("{oname} is not ours")
        }
        if matches!(obj.kind, ObjectiveKind::CarrierGroup { .. }) {
            bail!("a carrier group has no ground forces to send")
        }
        if obj.threatened {
            bail!("{oname} is under threat and can't spare its garrison")
        }
        if obj.in_capture_hold() {
            bail!("{oname} is still consolidating")
        }
        let spawned = obj.spawned;
        // It sets out with what the base can give it.
        let supply = (obj.supply() as f64 / 100.).clamp(0.3, 1.);
        let gids = self.formation_candidates(&cfg, &oid, side)?;
        if gids.is_empty() {
            bail!("{oname} has no armour or infantry it can spare")
        }
        // Out of the garrison: the base's cull, ghost check and repair stop
        // seeing them from here on.
        {
            let obj = objective_mut!(self, oid)?;
            if let Some(set) = obj.groups.get_mut_cow(&side) {
                for gid in &gids {
                    set.remove_cow(gid);
                }
            }
        }
        for gid in &gids {
            self.persisted.objectives_by_group.remove_cow(gid);
            // Already standing at their posts in DCS: they drive out from
            // there.
            if spawned && Group::get_by_name(lua, &group!(self, gid)?.name).is_ok() {
                rt.live.insert(*gid);
            }
        }
        let mut strength0 = 0;
        let (mut armor, mut inf) = (false, false);
        let mut pts = vec![];
        for gid in &gids {
            let g = group!(self, gid)?;
            armor |= g.class == ObjGroupClass::Armor;
            inf |= g.class.is_infantry();
            for uid in &g.units {
                if let Some(u) = self.persisted.units.get(uid) {
                    if !u.dead {
                        strength0 += 1;
                        pts.push(u.pos);
                    }
                }
            }
        }
        let id = self.formations().map(|f| f.id).max().unwrap_or(0) + 1;
        let kind = match cfg.unit_names.get(&side) {
            Some(n) => n.clone(),
            None => String::from(match (armor, inf) {
                (true, true) => "Mech Coy",
                (true, false) => "Armd Coy",
                _ => "Inf Coy",
            }),
        };
        let name = String::from(format_compact!("{} {kind} ({oname})", ordinal(id)));
        let pos = centroid2d(pts.into_iter());
        let f = Formation {
            id,
            name: name.clone(),
            side,
            home: oid,
            groups: gids,
            strength0,
            order: Order::Hold,
            posture: Posture::Holding,
            pos,
            heading: 0.,
            path: vec![],
            trail: vec![],
            off_road: false,
            commander: by,
            locked_until: None,
            order_ts: now,
            created: now,
            supply,
            morale: 1.,
            deployment: Deployment::Deployed,
            deployment_ts: Some(now),
            broken: false,
            losses: 0,
            kills: 0,
            damage: 0.,
        };
        self.persisted.formations.insert_cow(id, f);
        if let Err(e) = self.update_objective_status(&oid, now) {
            warn!("ground war: status of {oname} after raising {name}: {e:?}");
        }
        self.ephemeral.dirty();
        info!("ground war: {side:?} raised {name} ({id}), {strength0} vehicles from {oname}");
        rt.event(side, "raised", format_compact!("{name} formed up at {oname}, {strength0} vehicles"), Some(pos), Some(id), now);
        Ok(id)
    }

    /// The road from `from` to `to` as map points, or a straight line
    /// across country (`true`) when there's no sensible road.
    fn plan_path(&self, lua: MizLua, from: Vector2, to: Vector2) -> Result<(Vec<Vector2>, bool)> {
        let land = Land::singleton(lua)?;
        let straight = dist(from, to);
        if straight < 3_000. {
            return Ok((vec![to], true));
        }
        if let Ok(seq) = land.find_path_on_roads(RoadType::Road, LuaVec2(from), LuaVec2(to)) {
            let mut pts: Vec<Vector2> = seq.into_iter().filter_map(|p| p.ok()).map(|p| p.0).collect();
            if pts.len() >= 2 {
                let mut len = dist(from, pts[0]);
                for w in pts.windows(2) {
                    len += dist(w[0], w[1]);
                }
                if len <= straight * ROAD_DETOUR_FACTOR + 5_000. {
                    pts.push(to);
                    return Ok((decimate(&pts, PATH_SPACING_M), false));
                }
            }
        }
        Ok((vec![to], true))
    }

    /// Give formation `id` an order. `by` is the player giving it, `None` for
    /// the AI (which can't override a player's order while it is locked).
    pub fn order_formation(
        &mut self,
        rt: &mut FormationRt,
        lua: MizLua,
        id: FormationId,
        order: Order,
        by: Option<Ucid>,
        now: DateTime<Utc>,
    ) -> Result<CompactString> {
        let cfg = self.ground_war_cfg()?;
        let f = self
            .persisted
            .formations
            .get(&id)
            .ok_or_else(|| anyhow!("no such formation {id}"))?;
        let (side, from, name) = (f.side, f.pos, f.name.clone());
        if by.is_none() && !f.ai_controlled(now) {
            bail!("{name} is under a player's command")
        }
        // A broken formation is past taking orders: all it will do is get
        // back to a friendly base.
        if f.broken && !matches!(order, Order::Withdraw(_)) {
            bail!("{name} has broken and is falling back; it can only be ordered to withdraw")
        }
        let target = match order.target() {
            None => None,
            Some(oid) => {
                let obj = objective!(self, oid)?;
                if matches!(obj.kind, ObjectiveKind::CarrierGroup { .. }) {
                    bail!("{} is at sea", obj.name)
                }
                match order {
                    Order::Attack(_) if obj.owner == side => bail!("{} is already ours", obj.name),
                    Order::Defend(_) | Order::Withdraw(_) if obj.owner != side => {
                        bail!("{} is not ours", obj.name)
                    }
                    _ => (),
                }
                Some((obj.zone.pos(), obj.name.clone()))
            }
        };
        let (path, off_road) = match &target {
            None => (vec![], false),
            Some((pos, _)) => self.plan_path(lua, from, *pos)?,
        };
        let km = {
            let mut prev = from;
            path.iter().fold(0., |d, p| {
                let d = d + dist(prev, *p);
                prev = *p;
                d
            }) / 1000.
        };
        let f = self.persisted.formations.get_mut_cow(&id).unwrap();
        f.order = order;
        f.posture = if path.is_empty() { Posture::Holding } else { Posture::Moving };
        f.path = path;
        f.off_road = off_road;
        f.order_ts = now;
        match by {
            Some(ucid) => {
                f.commander = Some(ucid);
                f.locked_until = Some(now + Duration::seconds(cfg.player_order_lock_secs as i64));
            }
            None => f.commander = None,
        }
        let live = rt.is_live(f);
        rt.stall.remove(&id);
        rt.halted.remove(&id);
        rt.no_infantry_said.remove(&id);
        if live {
            rt.route_dirty.insert(id);
        }
        self.ephemeral.dirty();
        let what = match (&order, &target) {
            (Order::Hold, _) => format_compact!("{name}: holding position"),
            (o, Some((_, t))) => format_compact!(
                "{name}: {} {t}, {km:.0} km{}",
                o.verb(),
                if off_road { " across country" } else { " by road" }
            ),
            (o, None) => format_compact!("{name}: {}", o.verb()),
        };
        info!("ground war: {side:?} {what} (by {by:?})");
        if let Some(ucid) = by.as_ref() {
            let who = self.persisted.players.get(ucid).map(|p| p.name.clone()).unwrap_or_default();
            rt.event(side, "order", format_compact!("{who}: {what}"), Some(from), Some(id), now);
        }
        Ok(what)
    }

    /// Send formation `id` to a point of the commander's choosing, to hold
    /// there: the command map's Move. `by` is the commander (None = a server
    /// admin). It is a human order like any other: the AI leaves the
    /// formation alone for `player_order_lock_secs`.
    ///
    /// Not a way round the rules: the point can't be inside an enemy base
    /// (taking one is an Attack, with its assault and capture rules) or in
    /// the water, a broken formation only withdraws, and a commander can't
    /// take a formation from another player whose order still stands.
    pub fn move_formation_to(
        &mut self,
        rt: &mut FormationRt,
        lua: MizLua,
        id: FormationId,
        to: Vector2,
        by: Option<Ucid>,
        now: DateTime<Utc>,
    ) -> Result<CompactString> {
        let cfg = self.ground_war_cfg()?;
        let f = self
            .persisted
            .formations
            .get(&id)
            .ok_or_else(|| anyhow!("no such formation {id}"))?;
        let (side, from, name) = (f.side, f.pos, f.name.clone());
        if f.broken {
            bail!("{name} has broken and is falling back; it can only be ordered to withdraw")
        }
        if let (Some(b), Some(c)) = (by.as_ref(), f.commander.as_ref()) {
            if b != c && !f.ai_controlled(now) {
                let who = self.persisted.players.get(c).map(|p| p.name.clone()).unwrap_or_default();
                bail!("{name} is carrying out {who}'s order")
            }
        }
        if let Some((_, o)) = self
            .objectives()
            .find(|(_, o)| o.owner() != side && o.owner() != Side::Neutral && o.contains(to))
        {
            bail!("{} is an enemy base: order an attack to take it", o.name)
        }
        let land = Land::singleton(lua)?;
        if matches!(land.get_surface_type(LuaVec2(to))?, SurfaceType::Water | SurfaceType::ShallowWater) {
            bail!("that point is in the water")
        }
        let (path, off_road) = self.plan_path(lua, from, to)?;
        let km = {
            let mut prev = from;
            path.iter().fold(0., |d, p| {
                let d = d + dist(prev, *p);
                prev = *p;
                d
            }) / 1000.
        };
        let f = self.persisted.formations.get_mut_cow(&id).unwrap();
        f.order = Order::Hold;
        f.posture = Posture::Moving;
        f.path = path;
        f.off_road = off_road;
        f.order_ts = now;
        f.commander = by;
        f.locked_until = Some(now + Duration::seconds(cfg.player_order_lock_secs as i64));
        let live = rt.is_live(f);
        rt.stall.remove(&id);
        rt.halted.remove(&id);
        if live {
            rt.route_dirty.insert(id);
        }
        self.ephemeral.dirty();
        let what = format_compact!(
            "{name}: moving to a position {km:.0} km away{}{}",
            if off_road { " across country" } else { " by road" },
            if by.is_some() { ", returning fire only" } else { "" }
        );
        let who = by
            .as_ref()
            .and_then(|u| self.persisted.players.get(u))
            .map(|p| p.name.clone())
            .unwrap_or_else(|| "Command".into());
        info!("ground war: {side:?} {what} (by {who})");
        rt.event(side, "order", format_compact!("{who}: {what}"), Some(from), Some(id), now);
        Ok(what)
    }

    /// Hand a player-commanded formation back to the AI.
    pub fn release_formation(&mut self, id: FormationId) -> Result<()> {
        let f = self
            .persisted
            .formations
            .get_mut_cow(&id)
            .ok_or_else(|| anyhow!("no such formation {id}"))?;
        f.locked_until = None;
        f.commander = None;
        self.ephemeral.dirty();
        Ok(())
    }

    /// Lay every live-less formation's units out where the formation is: in
    /// column along the road on the march, in line abreast facing the enemy
    /// when it has deployed, in all-round positions when it has dug in.
    fn place_units(&mut self, id: FormationId) -> Result<()> {
        let f = self.persisted.formations.get(&id).ok_or_else(|| anyhow!("no formation {id}"))?;
        let (pos, heading, trail, deployment) = (f.pos, f.heading, f.trail.clone(), f.deployment);
        let moving = f.posture == Posture::Moving && deployment == Deployment::Column;
        let uids: SmallVec<[UnitId; 32]> = f
            .groups
            .iter()
            .filter_map(|g| self.persisted.groups.get(g))
            .flat_map(|g| g.units.into_iter().copied())
            .filter(|u| self.persisted.units.get(u).map_or(false, |u| !u.dead))
            .collect();
        let n = uids.len();
        for (i, uid) in uids.iter().enumerate() {
            let (p, h) = if moving {
                back_along(pos, heading, &trail, i as f64 * COLUMN_GAP_M)
            } else if deployment == Deployment::DugIn {
                (ring_slot(pos, i), heading)
            } else {
                (line_slot(pos, heading, i, n), heading)
            };
            let u = unit_mut!(self, uid)?;
            u.pos = p;
            u.heading = h;
            u.position.p.0.x = p.x;
            u.position.p.0.z = p.y;
        }
        Ok(())
    }

    /// Move the formations that are on the map only along their paths, each
    /// at its own speed, burning fuel as they go.
    fn advance_on_map(&mut self, rt: &FormationRt, cfg: &GroundWarCfg, dt: f64) {
        if cfg.live_only {
            // Driving happens in DCS or not at all.
            return;
        }
        let ids: SmallVec<[(FormationId, f64, f64); 16]> = self
            .formations()
            .filter(|f| f.posture == Posture::Moving && !rt.is_live(f) && !rt.is_halted(f.id))
            .map(|f| (f.id, self.formation_speed_kph(cfg, f) / 3.6, self.supply_use_factor(f)))
            .collect();
        for (id, speed, use_factor) in ids {
            let f = self.persisted.formations.get_mut_cow(&id).unwrap();
            let mut budget = speed * dt;
            let mut moved = 0.;
            while budget > 0. && !f.path.is_empty() {
                let next = f.path[0];
                let d = dist(f.pos, next);
                if d > 1. {
                    f.heading = heading_of(next - f.pos);
                }
                if d <= budget {
                    budget -= d;
                    moved += d;
                    f.pos = next;
                    f.trail.push(next);
                    f.path.remove(0);
                } else {
                    f.pos += (next - f.pos) / d * budget;
                    moved += budget;
                    budget = 0.;
                }
            }
            f.supply = (f.supply - moved / 100_000. * cfg.combat.supply_per_100km * use_factor).max(0.);
            if f.trail.len() > 40 {
                let excess = f.trail.len() - 40;
                f.trail.drain(..excess);
            }
            if let Err(e) = self.place_units(id) {
                warn!("ground war: laying out formation {id}: {e:?}");
            }
        }
        self.ephemeral.dirty();
    }

    /// Read where the live formations' units are from DCS, a slice per tick,
    /// and move each live formation's position to the centre of its units.
    fn sync_live(&mut self, rt: &mut FormationRt, lua: MizLua, now: DateTime<Utc>) {
        self.register_unannounced(rt, lua);
        let uids: Vec<UnitId> = self
            .formations()
            .flat_map(|f| f.groups.iter())
            .filter(|g| rt.live.contains(g))
            .filter_map(|g| self.persisted.groups.get(g))
            .flat_map(|g| g.units.into_iter().copied())
            .filter(|u| {
                self.persisted.units.get(u).map_or(false, |u| !u.dead)
                    && self.ephemeral.object_id_by_uid.contains_key(u)
            })
            .collect();
        if !uids.is_empty() {
            let start = rt.sync_cursor % uids.len();
            let batch: Vec<UnitId> =
                uids.iter().cycle().skip(start).take(SYNC_UNITS_PER_TICK.min(uids.len())).copied().collect();
            rt.sync_cursor = start + batch.len();
            if let Err(e) = self.update_unit_positions(lua, now, &batch) {
                warn!("ground war: reading unit positions: {e:?}");
            }
        }
        let ids: SmallVec<[FormationId; 16]> =
            self.formations().filter(|f| rt.is_live(f)).map(|f| f.id).collect();
        for id in ids {
            let f = self.persisted.formations.get(&id).unwrap();
            let pts: SmallVec<[Vector2; 32]> = f
                .groups
                .iter()
                .filter(|g| rt.live.contains(g))
                .filter_map(|g| self.persisted.groups.get(g))
                .flat_map(|g| g.units.into_iter())
                .filter_map(|u| self.persisted.units.get(u))
                .filter(|u| !u.dead)
                .map(|u| u.pos)
                .collect();
            if pts.is_empty() {
                continue;
            }
            let c = centroid2d(pts.into_iter());
            let per_100km = self.ephemeral.cfg.ground_war.as_ref().map_or(0., |g| g.combat.supply_per_100km);
            let f = self.persisted.formations.get_mut_cow(&id).unwrap();
            let moved = dist(c, f.pos);
            if moved > 5. {
                f.heading = heading_of(c - f.pos);
            }
            // A centroid jump of kilometres is a group joining or leaving,
            // not driving.
            if moved < 2_000. {
                f.supply = (f.supply - moved / 100_000. * per_100km).max(0.);
            }
            f.pos = c;
            // Drop the route points it has passed.
            let window = f.path.len().min(25);
            if window > 1 {
                let (best, _) = f.path[..window]
                    .iter()
                    .enumerate()
                    .map(|(i, p)| (i, dist(*p, c)))
                    .min_by(|a, b| a.1.total_cmp(&b.1))
                    .unwrap();
                if best > 0 {
                    let passed: Vec<Vector2> = f.path.drain(..best).collect();
                    f.trail.extend(passed);
                }
            }
            if f.path.len() > 1 && dist(f.path[0], c) < 250. {
                let p = f.path.remove(0);
                f.trail.push(p);
            }
            if f.trail.len() > 40 {
                let excess = f.trail.len() - 40;
                f.trail.drain(..excess);
            }
        }
    }

    /// A unit is only tracked once its DCS birth event has tied it to its
    /// DCS object; one whose event never arrived was skipped by the position
    /// sync without a word, so its formation stood still on the map (and
    /// went "stuck" and was halted for real) while it drove in DCS. Look
    /// those up by name instead, a few a tick.
    fn register_unannounced(&mut self, rt: &FormationRt, lua: MizLua) {
        let missing: SmallVec<[(UnitId, String); 8]> = self
            .formations()
            .flat_map(|f| f.groups.iter())
            .filter(|g| rt.live.contains(g))
            .filter_map(|g| self.persisted.groups.get(g))
            .flat_map(|g| g.units.into_iter().copied())
            .filter(|u| !self.ephemeral.object_id_by_uid.contains_key(u))
            .filter_map(|u| self.persisted.units.get(&u).filter(|su| !su.dead).map(|su| (u, su.name.clone())))
            .take(8)
            .collect();
        let mut found = 0;
        for (uid, name) in missing {
            let Ok(unit) = Unit::get_by_name(lua, name.as_str()) else { continue };
            let Ok(oid) = unit.object_id() else { continue };
            self.ephemeral.uid_by_object_id.insert(oid.clone(), uid);
            self.ephemeral.object_id_by_uid.insert(uid, oid);
            self.ephemeral.units_potentially_close_to_enemies.insert(uid);
            if self.persisted.units.get(&uid).map_or(false, |u| u.tags.contains(UnitTag::Driveable)) {
                self.ephemeral.units_able_to_move.insert(uid);
            }
            found += 1;
        }
        if found > 0 {
            warn!("ground war: {found} live formation unit(s) had no DCS birth event; found them by name");
        }
    }

    /// What DCS itself says about a live formation that isn't moving: is the
    /// group there, does it have a task, is the lead vehicle moving, and is
    /// it where we think it is. One line, for the log.
    fn dcs_report(&self, rt: &FormationRt, lua: MizLua, f: &Formation) -> CompactString {
        use std::fmt::Write;
        let mut out = CompactString::default();
        for gid in f.groups.iter().filter(|g| rt.live.contains(g)).take(2) {
            let Some(g) = self.persisted.groups.get(gid) else { continue };
            let tracked = g.units.into_iter().filter(|u| self.ephemeral.object_id_by_uid.contains_key(u)).count();
            let alive = g.units.into_iter().filter(|u| self.persisted.units.get(u).map_or(false, |u| !u.dead)).count();
            let _ = write!(out, "[{}: {tracked}/{alive} tracked", g.name);
            match Group::get_by_name(lua, g.name.as_str()) {
                Err(_) => {
                    let _ = write!(out, ", NOT IN DCS]");
                    continue;
                }
                Ok(dg) => {
                    let size = dg.get_size().unwrap_or(-1);
                    let task = dg.get_controller().and_then(|c| c.has_task()).map_or("?", |t| if t { "yes" } else { "NO" });
                    let _ = write!(out, ", dcs size {size}, task {task}");
                    let lead = dg.get_units().ok().and_then(|us| us.into_iter().filter_map(|u| u.ok()).next());
                    if let Some(u) = lead {
                        let speed = u.get_velocity().map(|v| v.0.norm()).unwrap_or(-1.);
                        let at = u.get_point().map(|p| Vector2::new(p.0.x, p.0.z)).ok();
                        let ours = u
                            .get_name()
                            .ok()
                            .and_then(|n| self.persisted.units_by_name.get(n.as_str()).copied())
                            .and_then(|uid| self.persisted.units.get(&uid))
                            .map(|su| su.pos);
                        let drift = match (at, ours) {
                            (Some(a), Some(o)) => format_compact!("{:.0} m", dist(a, o)),
                            _ => "?".into(),
                        };
                        let to_next = match (at, f.path.first()) {
                            (Some(a), Some(n)) => format_compact!("{:.0} m", dist(a, *n)),
                            _ => "-".into(),
                        };
                        let _ = write!(
                            out,
                            ", lead {:.1} m/s, ours vs DCS {drift}, next waypoint {to_next}",
                            speed
                        );
                    }
                    let _ = write!(out, "]");
                }
            }
        }
        out
    }

    /// Positions of `side`'s enemies on the ground: formations and the
    /// objectives they hold.
    fn enemy_formations_near(&self, side: Side, pos: Vector2, r: f64) -> bool {
        self.formations().any(|f| f.side != side && dist(f.pos, pos) <= r)
    }

    /// Is any enemy ground unit that is in DCS right now within `r` of `pos`
    /// -- a garrison, a SAM site, troops, a convoy? A live formation that
    /// close is shooting at it, and DCS halts ground units to fire.
    fn enemy_live_ground_near(&self, side: Side, pos: Vector2, r: f64) -> bool {
        self.ephemeral.object_id_by_uid.keys().any(|uid| {
            self.persisted.units.get(uid).map_or(false, |u| {
                !u.dead
                    && u.side != side
                    && u.side != Side::Neutral
                    && dist(u.pos, pos) <= r
                    && self
                        .persisted
                        .groups
                        .get(&u.group)
                        .map_or(false, |g| g.kind == Some(GroupCategory::Ground))
            })
        })
    }

    /// In contact with the enemy: an enemy formation, or the objective it is
    /// attacking, within `ENGAGED_M`.
    pub fn formation_engaged(&self, f: &Formation) -> bool {
        self.engaged(f)
    }

    fn engaged(&self, f: &Formation) -> bool {
        if self.enemy_formations_near(f.side, f.pos, ENGAGED_M) {
            return true;
        }
        match f.order {
            Order::Attack(oid) => self
                .persisted
                .objectives
                .get(&oid)
                .map_or(false, |o| dist(o.zone.pos(), f.pos) <= ENGAGED_M),
            _ => false,
        }
    }

    /// Unstick live formations DCS has wedged on the terrain: a fresh road
    /// route first, then straight across country, then give up and hold.
    fn unstick_live(&mut self, rt: &mut FormationRt, lua: MizLua, now: DateTime<Utc>) {
        let ids: SmallVec<[FormationId; 16]> = self
            .formations()
            .filter(|f| f.posture == Posture::Moving && rt.is_live(f))
            .map(|f| f.id)
            .collect();
        for id in ids {
            let f = self.persisted.formations.get(&id).unwrap();
            // Fighting is not being stuck: DCS stops ground units to shoot,
            // at an enemy formation or at any enemy unit in DCS in reach.
            let engaged = self.engaged(f) || self.enemy_live_ground_near(f.side, f.pos, ENGAGED_M);
            let (pos, dest, side, name) = (f.pos, f.destination(), f.side, f.name.clone());
            let e = rt.stall.entry(id).or_insert((pos, now, 0));
            if dist(pos, e.0) >= STALL_MOVE_M || engaged {
                e.0 = pos;
                e.1 = now;
                continue;
            }
            if now - e.1 < Duration::seconds(STALL_SECS) {
                continue;
            }
            e.0 = pos;
            e.1 = now;
            e.2 += 1;
            let attempt = e.2;
            {
                let f = self.persisted.formations.get(&id).unwrap();
                let report = self.dcs_report(rt, lua, f);
                warn!(
                    "ground war: {name} has not moved in {} min ({:?}, {:?}, {} route points): {report}",
                    STALL_SECS / 60,
                    f.order,
                    f.deployment,
                    f.path.len()
                );
            }
            let Some(dest) = dest else { continue };
            let replanned = match attempt {
                1 => self.plan_path(lua, pos, dest).ok(),
                2 => Some((vec![dest], true)),
                _ => None,
            };
            let f = self.persisted.formations.get_mut_cow(&id).unwrap();
            match replanned {
                Some((path, off_road)) => {
                    info!("ground war: {name} stuck, re-route {attempt} (off road: {off_road})");
                    f.path = path;
                    f.off_road = off_road;
                    rt.route_dirty.insert(id);
                }
                None => {
                    warn!("ground war: {name} stuck for good, holding where it is");
                    f.path.clear();
                    f.posture = Posture::Holding;
                    f.order = Order::Hold;
                    rt.route_dirty.insert(id);
                    self.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        side,
                        format_compact!("{name} is stuck on the terrain and has halted. Give it a new order."),
                    );
                }
            }
        }
    }

    /// Formations that have reached the end of their route.
    fn arrivals(&mut self, rt: &mut FormationRt, cfg: &GroundWarCfg, now: DateTime<Utc>) {
        // An attack has arrived only once it is inside the objective's zone:
        // that is where its infantry has to get out.
        let inside_target = |f: &Formation| match f.order {
            Order::Attack(oid) => self.persisted.objectives.get(&oid).map_or(true, |o| o.zone.contains(f.pos)),
            _ => true,
        };
        let ids: SmallVec<[FormationId; 16]> = self
            .formations()
            .filter(|f| {
                f.posture == Posture::Moving
                    && match f.destination() {
                        None => true,
                        Some(d) => dist(f.pos, d) <= cfg.arrive_m && inside_target(f) || dist(f.pos, d) <= 150.,
                    }
            })
            .map(|f| f.id)
            .collect();
        for id in ids {
            let f = self.persisted.formations.get_mut_cow(&id).unwrap();
            f.path.clear();
            let (side, name, order) = (f.side, f.name.clone(), f.order);
            f.posture = match order {
                Order::Attack(_) => Posture::Assaulting,
                _ => Posture::Holding,
            };
            if !rt.is_live(f) {
                let _ = self.place_units(id);
            }
            self.ephemeral.dirty();
            let tname = order
                .target()
                .and_then(|o| self.persisted.objectives.get(&o))
                .map(|o| (o.name.clone(), o.owner));
            if let Some((t, _)) = tname.as_ref() {
                let at = self.persisted.formations.get(&id).map(|f| f.pos);
                rt.event(side, "arrived", format_compact!("{name} has reached {t}"), at, Some(id), now);
            }
            match (order, tname) {
                (Order::Withdraw(oid), Some((_, owner))) if owner == side => {
                    if let Err(e) = self.dissolve_formation(rt, id, oid, now) {
                        warn!("ground war: {name} rejoining its garrison: {e:?}");
                    }
                }
                (Order::Attack(_), Some((t, _))) if cfg.announce => {
                    self.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        side,
                        format_compact!("{name} has reached {t} and is going in."),
                    );
                    self.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        side.opposite(),
                        format_compact!("Enemy armour is assaulting {t}!"),
                    );
                }
                (Order::Defend(_), Some((t, _))) if cfg.announce => {
                    self.ephemeral.msgs().panel_to_side(
                        10,
                        false,
                        side,
                        format_compact!("{name} is in position at {t}."),
                    );
                }
                _ => (),
            }
        }
    }

    /// Formations at the objective they were sent to take.
    fn assaults(
        &mut self,
        rt: &mut FormationRt,
        cfg: &GroundWarCfg,
        lua: MizLua,
        idx: &MizIndex,
        now: DateTime<Utc>,
    ) {
        let ids: SmallVec<[FormationId; 16]> = self
            .formations()
            .filter(|f| f.posture == Posture::Assaulting)
            .map(|f| f.id)
            .collect();
        for id in ids {
            let f = self.persisted.formations.get(&id).unwrap();
            let (side, name, pos) = (f.side, f.name.clone(), f.pos);
            let Order::Attack(oid) = f.order else {
                let f = self.persisted.formations.get_mut_cow(&id).unwrap();
                f.posture = Posture::Holding;
                continue;
            };
            let Some(obj) = self.persisted.objectives.get(&oid) else {
                let f = self.persisted.formations.get_mut_cow(&id).unwrap();
                f.order = Order::Hold;
                f.posture = Posture::Holding;
                continue;
            };
            let oname = obj.name.clone();
            if obj.owner == side {
                // Taken (by its own assault squad or anyone else's): it holds
                // the place now.
                let f = self.persisted.formations.get_mut_cow(&id).unwrap();
                f.order = Order::Defend(oid);
                f.posture = Posture::Holding;
                self.ephemeral.dirty();
                if cfg.announce {
                    self.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        side,
                        format_compact!("{name} holds {oname}."),
                    );
                }
                continue;
            }
            let open = obj.captureable() && !obj.in_capture_hold() && !self.capture_in_progress(&oid);
            let cooling = rt
                .assault_ts
                .get(&oid)
                .map_or(false, |t| now - *t < Duration::seconds(ASSAULT_COOLDOWN_SECS));
            // A formation the server isn't simulating can go in too: the
            // squad is a real DCS group either way, and the capture is the
            // ordinary capture. Waiting for a live slot used to leave an
            // attack outside a broken base for good.
            if !open || cooling {
                continue;
            }
            match self.launch_assault(rt, lua, idx, id, oid, now) {
                Ok(true) => {
                    rt.assault_ts.insert(oid, now);
                    info!("ground war: {name} sends its infantry into {oname}");
                    rt.event(side, "assault", format_compact!("{name}'s infantry is going in to take {oname}"), Some(pos), Some(id), now);
                    rt.event(side.opposite(), "assault", format_compact!("Enemy infantry is assaulting {oname}"), Some(pos), None, now);
                    self.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        side,
                        format_compact!("{name}'s infantry is going in to take {oname}. Cover them."),
                    );
                }
                Ok(false) => {
                    if rt.no_infantry_said.insert(id) {
                        self.ephemeral.msgs().panel_to_side(
                            15,
                            false,
                            side,
                            format_compact!(
                                "{oname} is broken, but {name} has no infantry or troop carriers \
                                 left to take it. Send troops in."
                            ),
                        );
                    }
                }
                Err(e) => warn!("ground war: {name} assault on {oname} at {pos:?}: {e:?}"),
            }
        }
    }

    /// Send formation `id`'s infantry into `oid` as a capture squad: its
    /// infantry group if it has one (the group becomes the squad), else a
    /// squad out of one of its IFVs / APCs (the vehicle stays). False if it
    /// has neither.
    fn launch_assault(
        &mut self,
        rt: &mut FormationRt,
        lua: MizLua,
        idx: &MizIndex,
        id: FormationId,
        oid: ObjectiveId,
        now: DateTime<Utc>,
    ) -> Result<bool> {
        let f = self.persisted.formations.get(&id).unwrap();
        let (side, home) = (f.side, f.home);
        // (group the squad comes from, where it gets out, template, the
        // group itself becomes the squad)
        let (gid, from, template, whole_group) = match self.infantry_group(f) {
            Some(gid) => {
                let g = group!(self, gid)?;
                (gid, self.group_center(&gid)?, g.template_name.clone(), true)
            }
            None => match self.troop_carrier(f) {
                Some((gid, pos, template)) => (gid, pos, template, false),
                None => return Ok(false),
            },
        };
        let obj = objective!(self, oid)?;
        let (zpos, zr) = (obj.zone.pos(), obj.zone.radius());
        // Where they get out: where they are. The formation only counts as
        // arrived once it is inside the zone (`arrivals`), so this is almost
        // always already inside; a vehicle at the tail of the line can be
        // just outside, and its squad gets out at the edge nearest it.
        let at = if obj.zone.contains(from) {
            from
        } else {
            let d = from - zpos;
            let n = d.norm();
            if n > 1. { zpos + d / n * (zr * 0.9).min(n) } else { zpos }
        };
        let dir = {
            let d = zpos - at;
            if d.norm() > 1. { d / d.norm() } else { Vector2::new(1., 0.) }
        };
        let spctx = SpawnCtx::new(lua)?;
        self.add_and_queue_group(
            &spctx,
            idx,
            side,
            SpawnLoc::AtPos { pos: at, offset_direction: dir, group_heading: heading_of(dir) },
            &template,
            DeployKind::Dismount { from_group: gid, can_capture: true },
            BitFlags::empty(),
            None,
        )
        .context("spawning the assault squad")?;
        if !whole_group {
            return Ok(true);
        }
        // The squad IS that infantry group: it leaves the formation, and its
        // empty place goes home to be rebuilt.
        let gname = group!(self, gid)?.name.clone();
        for uid in group!(self, gid)?.units.clone().into_iter() {
            let u = unit_mut!(self, uid)?;
            u.dead = true;
            u.pos = u.spawn_pos;
            u.heading = u.spawn_heading;
            u.position = u.spawn_position;
        }
        if rt.live.remove(&gid) {
            self.ephemeral.push_despawn(gid, Despawn::GroupByName(gname.to_string()));
        }
        self.persisted.formations.get_mut_cow(&id).unwrap().groups.retain(|g| *g != gid);
        self.attach_to_objective(gid, side, home, false)?;
        if let Err(e) = self.update_objective_status(&home, now) {
            warn!("ground war: status of {home} after an assault: {e:?}");
        }
        self.ephemeral.dirty();
        Ok(true)
    }

    /// Length of the road route `order_formation` would plan from `from` to
    /// `to`, km (a straight line when there is no sensible road).
    pub fn route_km(&self, lua: MizLua, from: Vector2, to: Vector2) -> Result<f64> {
        let (path, _) = self.plan_path(lua, from, to)?;
        let mut prev = from;
        Ok(path.iter().fold(0., |d, p| {
            let d = d + dist(prev, *p);
            prev = *p;
            d
        }) / 1000.)
    }

    /// Put `gid` back into `oid`'s garrison for `side`. With `at_posts`
    /// false its units keep whatever positions they have; `rehome` moves its
    /// posts to `oid`.
    fn attach_to_objective(&mut self, gid: GroupId, side: Side, oid: ObjectiveId, rehome: bool) -> Result<()> {
        if self.persisted.objectives.get(&oid).is_none() {
            return self.delete_group(&gid);
        }
        if rehome {
            let center = objective!(self, oid)?.zone.pos();
            let uids: SmallVec<[UnitId; 16]> = group!(self, gid)?.units.into_iter().copied().collect();
            for (i, uid) in uids.iter().enumerate() {
                let p = ring_slot(center, i + 16);
                let u = unit_mut!(self, uid)?;
                u.spawn_pos = p;
                u.spawn_position.p.0.x = p.x;
                u.spawn_position.p.0.z = p.y;
            }
            group_mut!(self, gid)?.origin = DeployKind::Objective { origin: oid };
        }
        let obj = objective_mut!(self, oid)?;
        obj.groups.get_or_default_cow(side).insert_cow(gid);
        self.persisted.objectives_by_group.insert_cow(gid, oid);
        Ok(())
    }

    /// Formation `id` rejoins the garrison of `at` (its home, or the friendly
    /// base it fell back to): survivors back at their posts, the dead left
    /// for the base's repair to rebuild.
    pub fn dissolve_formation(
        &mut self,
        rt: &mut FormationRt,
        id: FormationId,
        at: ObjectiveId,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let f = self
            .persisted
            .formations
            .remove_cow(&id)
            .ok_or_else(|| anyhow!("no such formation {id}"))?;
        let rehome = at != f.home;
        for gid in &f.groups {
            self.rehome_group(rt, *gid, f.side, at, rehome, now)?;
        }
        if let Err(e) = self.update_objective_status(&at, now) {
            warn!("ground war: status of {at} after {} rejoined: {e:?}", f.name);
        }
        self.forget_formation(rt, id);
        self.ephemeral.dirty();
        info!("ground war: {:?} {} rejoined the garrison at {at}", f.side, f.name);
        Ok(())
    }

    /// Put one of a formation's groups back in `at`'s garrison: out of DCS
    /// if it was live, back at its post, and spawned there again if the
    /// base is spawned. `rehome` moves it to a base that isn't its own.
    fn rehome_group(
        &mut self,
        rt: &mut FormationRt,
        gid: GroupId,
        side: Side,
        at: ObjectiveId,
        rehome: bool,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let Some(g) = self.persisted.groups.get(&gid) else { return Ok(()) };
        let name = g.name.clone();
        rt.spawnq.retain(|(_, g)| *g != gid);
        let was_live = rt.live.remove(&gid);
        if was_live {
            self.ephemeral.push_despawn(gid, Despawn::GroupByName(name.to_string()));
        }
        self.attach_to_objective(gid, side, at, rehome)?;
        let Some(g) = self.persisted.groups.get(&gid) else { return Ok(()) };
        let uids: SmallVec<[UnitId; 16]> = g.units.into_iter().copied().collect();
        let mut alive = false;
        for uid in uids {
            let u = unit_mut!(self, uid)?;
            u.pos = u.spawn_pos;
            u.heading = u.spawn_heading;
            u.position = u.spawn_position;
            alive |= !u.dead;
        }
        let spawn_now = self
            .persisted
            .objectives
            .get(&at)
            .map_or(false, |o| o.owner == side && o.spawned);
        if alive && spawn_now {
            // After the despawn above has gone through, or DCS keeps the old
            // group where it stood.
            let at_ts = now + Duration::seconds(if was_live { 10 } else { 1 });
            self.ephemeral.delayspawnq.entry(at_ts).or_default().push(gid);
        }
        Ok(())
    }

    /// Every unit in `gid` can drive (`crate::unitdb::can_drive`).
    pub(super) fn group_can_drive(&self, gid: &GroupId) -> bool {
        self.persisted.groups.get(gid).map_or(true, |g| {
            g.units
                .into_iter()
                .filter_map(|u| self.persisted.units.get(u))
                .all(|u| crate::unitdb::can_drive(&u.typ.0))
        })
    }

    /// Send home any group a formation can't take with it. Formations raised
    /// before towed and emplaced guns were kept out of them could carry a
    /// KS-19 battery that sat at the gate in DCS while the map moved it on.
    /// A formation left with nothing that drives is stood down quietly -- it
    /// wasn't destroyed.
    fn shed_immobile(&mut self, rt: &mut FormationRt, now: DateTime<Utc>) {
        let stuck: SmallVec<[(FormationId, GroupId); 8]> = self
            .formations()
            .flat_map(|f| f.groups.iter().map(move |g| (f.id, *g)))
            .filter(|(_, g)| !self.group_can_drive(g))
            .collect();
        for (id, gid) in stuck {
            let Some(f) = self.persisted.formations.get_mut_cow(&id) else { continue };
            f.groups.retain(|g| *g != gid);
            let (side, home, name, empty) = (f.side, f.home, f.name.clone(), f.groups.is_empty());
            if let Err(e) = self.rehome_group(rt, gid, side, home, false, now) {
                warn!("ground war: sending {gid} of {name} home: {e:?}");
            }
            info!("ground war: {side:?} {name} left group {gid} at {home}: it can't drive");
            if empty {
                self.persisted.formations.remove_cow(&id);
                self.forget_formation(rt, id);
                info!("ground war: {side:?} {name} stood down: nothing left in it can drive");
            }
            if let Err(e) = self.update_objective_status(&home, now) {
                warn!("ground war: status of {home} after {name} left a group: {e:?}");
            }
            self.ephemeral.dirty();
        }
    }

    fn forget_formation(&mut self, rt: &mut FormationRt, id: FormationId) {
        rt.wanted_ts.remove(&id);
        rt.halted.remove(&id);
        rt.route_dirty.remove(&id);
        rt.stall.remove(&id);
        rt.contact_ts.remove(&id);
        rt.supplied.remove(&id);
        rt.cut_off_said.remove(&id);
        rt.spotted.retain(|(_, f), _| *f != id);
        rt.no_infantry_said.remove(&id);
        rt.spawnq.retain(|(f, _)| *f != id);
        if let Some(pin) = rt.pins.remove(&id) {
            self.ephemeral.msgs().delete_mark(pin.mark);
            if let Some(a) = pin.arrow {
                self.ephemeral.msgs().delete_mark(a);
            }
        }
    }

    /// Drop groups that have been deleted out from under a formation, and
    /// send the dead of a wiped-out formation home to be rebuilt.
    fn cleanup(&mut self, rt: &mut FormationRt, cfg: &GroundWarCfg, now: DateTime<Utc>) {
        let ids: SmallVec<[FormationId; 16]> = self.formations().map(|f| f.id).collect();
        for id in ids {
            let f = self.persisted.formations.get_mut_cow(&id).unwrap();
            let before = f.groups.len();
            let groups = &self.persisted.groups;
            f.groups.retain(|g| groups.get(g).is_some());
            if f.groups.len() != before {
                self.ephemeral.dirty();
            }
            let f = self.persisted.formations.get(&id).unwrap();
            let (side, name, home) = (f.side, f.name.clone(), f.home);
            if self.formation_strength(f).0 > 0 {
                continue;
            }
            info!("ground war: {side:?} {name} destroyed");
            let at = self.persisted.formations.get(&id).map(|f| f.pos);
            rt.event(side, "destroyed", format_compact!("{name} has been destroyed"), at, Some(id), now);
            rt.event(side.opposite(), "kill", format_compact!("Enemy {name} destroyed"), at, None, now);
            if cfg.announce {
                self.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side,
                    format_compact!("{name} has been destroyed."),
                );
                self.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side.opposite(),
                    format_compact!("Enemy formation {name} destroyed."),
                );
            }
            if let Err(e) = self.dissolve_formation(rt, id, home, now) {
                warn!("ground war: returning the wreck of {name}: {e:?}");
                self.persisted.formations.remove_cow(&id);
                self.forget_formation(rt, id);
            }
        }
    }

    /// Decide which formations should be in DCS, queue spawns and despawns,
    /// and halt the ones that would fight unseen with no budget left.
    fn plan_live(&mut self, rt: &mut FormationRt, cfg: &GroundWarCfg, lua: MizLua, now: DateTime<Utc>) {
        let players: SmallVec<[Vector2; 32]> = self
            .instanced_players()
            .map(|(_, _, i)| Vector2::new(i.position.p.x, i.position.p.z))
            .collect();
        let all: SmallVec<[(FormationId, Side, Vector2, Order, Posture, bool, bool); 16]> = self
            .formations()
            .map(|f| {
                // Locked = a human order (a player's, or an admin's) stands.
                let ordered = !f.ai_controlled(now);
                (f.id, f.side, f.pos, f.order, f.posture, rt.is_live(f), ordered)
            })
            .collect();
        // Every ground group in DCS right now, one point each: whatever a
        // formation would have to be in DCS itself to fight.
        let mut in_dcs: SmallVec<[(Side, Vector2); 64]> = SmallVec::new();
        let mut seen: FxHashSet<GroupId> = FxHashSet::default();
        for uid in self.ephemeral.object_id_by_uid.keys() {
            let Some(u) = self.persisted.units.get(uid) else { continue };
            if u.dead || !seen.insert(u.group) {
                continue;
            }
            match self.persisted.groups.get(&u.group) {
                Some(g) if g.kind == Some(GroupCategory::Ground) && g.side != Side::Neutral => {
                    in_dcs.push((g.side, u.pos))
                }
                _ => (),
            }
        }
        let mut wants: SmallVec<[(FormationId, Want, bool); 16]> = smallvec::smallvec![];
        for (id, side, pos, order, posture, live, ordered) in &all {
            let want = Want {
                player: players.iter().any(|p| dist(*p, *pos) <= cfg.player_bubble_m),
                ordered: *ordered && cfg.live_when_ordered,
                contact: all
                    .iter()
                    .any(|(_, s, p, ..)| s != side && dist(*p, *pos) <= cfg.contact_m),
                enemy: in_dcs.iter().any(|(s, p)| s != side && dist(*p, *pos) <= cfg.contact_m),
                target: match order {
                    Order::Attack(oid) => self
                        .persisted
                        .objectives
                        .get(oid)
                        .map_or(false, |o| dist(o.zone.pos(), *pos) <= cfg.contact_m),
                    _ => false,
                },
                assault: *posture == Posture::Assaulting,
                moving: cfg.live_only && *posture == Posture::Moving,
            };
            if want.any() {
                rt.wanted_ts.insert(*id, now);
            }
            wants.push((*id, want, *live));
        }
        // A fight waiting for a live slot doesn't wait out the grace period of
        // formations nobody needs any more.
        let fight_waiting = wants.iter().any(|(_, w, live)| !*live && w.fighting());
        let grace = if fight_waiting {
            Duration::zero()
        } else {
            Duration::seconds(cfg.despawn_grace_secs as i64)
        };
        // Despawn the live formations nobody needs any more.
        for (id, want, live) in &wants {
            if *live && !want.any() {
                let idle_since = rt.wanted_ts.get(id).copied().unwrap_or(now - grace);
                if now - idle_since >= grace {
                    self.dematerialize(rt, lua, *id, now);
                }
            }
        }
        let mut budget =
            (cfg.max_live_formations as usize).saturating_sub(rt.live_count(self));
        let mut waiting: SmallVec<[(FormationId, Want); 16]> = wants
            .iter()
            .filter(|(id, want, live)| {
                !*live && want.any() && !rt.spawnq.iter().any(|(f, _)| f == id)
            })
            .map(|(id, want, _)| (*id, *want))
            .collect();
        waiting.sort_by_key(|(_, w)| std::cmp::Reverse(w.priority()));
        // Queued spawns already hold their budget.
        let queued: FxHashSet<FormationId> = rt.spawnq.iter().map(|(f, _)| *f).collect();
        budget = budget.saturating_sub(queued.len());
        // With the budget spent, a player's order still gets its formation
        // into DCS: it takes the slot of a live formation with a weaker claim
        // that isn't fighting (weakest first).
        let mut bumpable: SmallVec<[(FormationId, u32); 16]> = wants
            .iter()
            .filter(|(_, w, live)| *live && w.any() && !w.ordered && !w.fighting() && !w.moving)
            .map(|(id, w, _)| (*id, w.priority()))
            .collect();
        bumpable.sort_by_key(|(_, p)| std::cmp::Reverse(*p));
        let mut halted = FxHashSet::default();
        for (id, want) in waiting {
            if budget > 0 {
                budget -= 1;
                info!("ground war: formation {id} into DCS: {}", want.reason());
                self.materialize(rt, id);
            } else if (want.ordered || (cfg.live_only && (want.fighting() || want.moving)))
                && bumpable.last().map_or(false, |(_, p)| *p < want.priority())
            {
                let (out, _) = bumpable.pop().unwrap();
                info!("ground war: formation {out} leaves DCS to make room for {id}, {}", want.reason());
                self.dematerialize(rt, lua, out, now);
                self.materialize(rt, id);
            } else if want.fighting() {
                halted.insert(id);
            } else if want.ordered {
                log::debug!("ground war: formation {id} is under orders but all {} live slots are fighting", cfg.max_live_formations);
            }
        }
        rt.halted = halted;
    }

    fn materialize(&mut self, rt: &mut FormationRt, id: FormationId) {
        let Some(f) = self.persisted.formations.get(&id) else { return };
        for gid in &f.groups {
            let alive = self
                .persisted
                .groups
                .get(gid)
                .map_or(false, |g| g.units.into_iter().any(|u| self.persisted.units.get(u).map_or(false, |u| !u.dead)));
            if alive && !rt.live.contains(gid) && !rt.spawnq.iter().any(|(_, g)| g == gid) {
                rt.spawnq.push_back((id, *gid));
            }
        }
    }

    fn dematerialize(&mut self, rt: &mut FormationRt, lua: MizLua, id: FormationId, now: DateTime<Utc>) {
        let Some(f) = self.persisted.formations.get(&id) else { return };
        let gids: SmallVec<[GroupId; 8]> = f.groups.iter().copied().filter(|g| rt.live.contains(g)).collect();
        // Last look at where everyone is before they leave DCS.
        let uids: Vec<UnitId> = gids
            .iter()
            .filter_map(|g| self.persisted.groups.get(g))
            .flat_map(|g| g.units.into_iter().copied())
            .filter(|u| self.ephemeral.object_id_by_uid.contains_key(u))
            .collect();
        if !uids.is_empty() {
            if let Err(e) = self.update_unit_positions(lua, now, &uids) {
                warn!("ground war: final positions of formation {id}: {e:?}");
            }
        }
        for gid in gids {
            if let Some(g) = self.persisted.groups.get(&gid) {
                let name = g.name.to_string();
                self.ephemeral.push_despawn(gid, Despawn::GroupByName(name));
            }
            rt.live.remove(&gid);
        }
        rt.route_dirty.remove(&id);
        rt.stall.remove(&id);
        info!("ground war: formation {id} back to the map only");
    }

    /// Spawn one queued formation group, on its route.
    fn spawn_next(
        &mut self,
        rt: &mut FormationRt,
        cfg: &GroundWarCfg,
        lua: MizLua,
        idx: &MizIndex,
        perf: &mut PerfInner,
    ) -> Result<()> {
        let Some((id, gid)) = rt.spawnq.pop_front() else { return Ok(()) };
        let Some(f) = self.persisted.formations.get(&id) else { return Ok(()) };
        if self.persisted.groups.get(&gid).is_none() {
            return Ok(());
        }
        // Despawned a moment ago and wanted straight back: the despawn is
        // still queued, and DCS would run it against the group spawned here
        // under the same name. Wait for it.
        if self.ephemeral.spawn_pending_gids().contains(&gid) {
            rt.spawnq.push_back((id, gid));
            return Ok(());
        }
        let (path, off_road) = match f.posture {
            Posture::Moving => (f.path.clone(), f.off_road),
            _ => (vec![], false),
        };
        let deployed = matches!(f.deployment, Deployment::Deployed | Deployment::DugIn) && !f.broken;
        let marching = f.marching(Utc::now());
        let speed = self.formation_speed_kph(cfg, f).max(3.) / 3.6;
        let land = Land::singleton(lua)?;
        // The persisted positions carry the map's x/y; DCS wants the ground
        // height too.
        let uids: SmallVec<[UnitId; 16]> = group!(self, gid)?.units.into_iter().copied().collect();
        for uid in &uids {
            let u = unit_mut!(self, uid)?;
            if let Ok(h) = land.get_height(LuaVec2(u.pos)) {
                u.position.p.0.y = h;
            }
        }
        let from = self.group_center(&gid)?;
        let route = dcs_route(&land, from, &path, off_road, deployed, speed);
        let spctx = SpawnCtx::new(lua)?;
        let spawned = self
            .ephemeral
            .spawn_group(perf, &self.persisted, idx, &spctx, group!(self, gid)?, route)
            .with_context(|| format_compact!("spawning formation {id} group {gid}"))?;
        if let Some(crate::spawnctx::Spawned::Group(g)) = spawned {
            if let Err(e) = set_march_ai(&g, marching) {
                warn!("ground war: march orders for {gid}: {e:?}");
            }
        }
        rt.live.insert(gid);
        Ok(())
    }

    /// (Re)issue the route of every live formation whose orders changed.
    fn issue_routes(&mut self, rt: &mut FormationRt, cfg: &GroundWarCfg, lua: MizLua) {
        let ids: SmallVec<[FormationId; 8]> = rt.route_dirty.drain().collect();
        let land = match Land::singleton(lua) {
            Ok(l) => l,
            Err(e) => {
                warn!("ground war: no land singleton: {e:?}");
                return;
            }
        };
        for id in ids {
            let Some(f) = self.persisted.formations.get(&id) else { continue };
            let (path, off_road) = match f.posture {
                Posture::Moving => (f.path.clone(), f.off_road),
                _ => (vec![], false),
            };
            let deployed = matches!(f.deployment, Deployment::Deployed | Deployment::DugIn) && !f.broken;
            let marching = f.marching(Utc::now());
            let speed = self.formation_speed_kph(cfg, f).max(3.) / 3.6;
            for gid in f.groups.iter().filter(|g| rt.live.contains(g)) {
                let Some(g) = self.persisted.groups.get(gid) else { continue };
                let from = self.group_center(gid).unwrap_or(f.pos);
                let res = Group::get_by_name(lua, &g.name).and_then(|group| {
                    set_march_ai(&group, marching)?;
                    let route = dcs_route(&land, from, &path, off_road, deployed, speed);
                    group
                        .get_controller()?
                        .set_task(Task::Mission { airborne: Some(false), route })
                });
                if let Err(e) = res {
                    warn!("ground war: routing {} of formation {id}: {e:?}", g.name);
                }
            }
        }
    }

    /// Who sees whom. A side sees an enemy formation:
    ///
    /// - from its own formations within `spot_m`, and from its bases within
    ///   3/4 of that -- less far if the enemy is standing still, less again
    ///   if it is dug in -- as long as the ground between them doesn't block
    ///   the view (`line_of_sight`);
    /// - from its aircraft flying low enough to see the ground
    ///   (`air_spot_m` / `air_spot_agl_m`), and its drones;
    /// - wherever its intel (recon, JTACs, special forces) has a fresh
    ///   contact on it.
    ///
    /// All of it shrinks with the light and the weather (`FormationRt::
    /// visibility`, set from the mission's time of day and visibility).
    /// Everything seen is remembered, with what it looked like then, where
    /// it was last seen for `remember_secs`.
    fn spot(&mut self, rt: &mut FormationRt, cfg: &GroundCombatCfg, lua: MizLua, now: DateTime<Utc>) {
        use super::intel::IntelUnitClass;
        use bfprotocols::cfg::ActionKind;
        let vis = rt.visibility.unwrap_or(1.).clamp(0.15, 1.);
        let forms: SmallVec<[(FormationId, Side, Vector2, f64, bool, Deployment); 32]> = self
            .formations()
            .map(|f| (f.id, f.side, f.pos, f.heading, f.posture == Posture::Moving, f.deployment))
            .collect();
        let bases: SmallVec<[(Side, Vector2); 128]> = self
            .objectives()
            .filter(|(_, o)| o.owner() != Side::Neutral)
            .map(|(_, o)| (o.owner(), o.pos()))
            .collect();
        // Eyes in the air: players low enough to see the ground, and drones.
        let land = Land::singleton(lua).ok();
        let height = |p: Vector2| land.as_ref().and_then(|l| l.get_height(LuaVec2(p)).ok()).unwrap_or(0.);
        let mut air: SmallVec<[(Side, Vector3); 32]> = SmallVec::new();
        for (_, p, i) in self.instanced_players() {
            if !i.in_air {
                continue;
            }
            let at = Vector2::new(i.position.p.x, i.position.p.z);
            if i.position.p.y - height(at) <= cfg.air_spot_agl_m {
                air.push((p.side, i.position.p.0));
            }
        }
        for g in self.actions() {
            let drone = matches!(
                &g.origin,
                DeployKind::Action { spec, .. } if matches!(spec.kind, ActionKind::Drone(_) | ActionKind::Recon(_))
            );
            if !drone {
                continue;
            }
            if let Ok(c) = self.group_center(&g.id) {
                let alt = g
                    .units
                    .into_iter()
                    .filter_map(|u| self.persisted.units.get(u))
                    .map(|u| u.position.p.y)
                    .next()
                    .unwrap_or(height(c) + 3_000.);
                air.push((g.side, Vector3::new(c.x, alt, c.y)));
            }
        }
        let fresh_intel = Duration::seconds(300);
        let visible = |from: Vector3, to: Vector2| -> bool {
            if !cfg.line_of_sight {
                return true;
            }
            match land.as_ref() {
                None => true,
                Some(l) => {
                    let to3 = LuaVec3(Vector3::new(to.x, height(to) + 2.5, to.y));
                    l.is_visible(LuaVec3(from), to3).unwrap_or(true)
                }
            }
        };
        let close_m = cfg.engage_m * 1.5;
        rt.spot_ts = Some(now);
        let mut new_contacts: SmallVec<[(Side, FormationId, Vector2); 8]> = SmallVec::new();
        for (id, side, pos, heading, moving, dep) in &forms {
            let hide = match dep {
                Deployment::DugIn => 0.6,
                _ if !*moving => 0.8,
                _ => 1.,
            };
            let observer = side.opposite();
            let range = cfg.spot_m * hide * vis;
            // Nearest observers first: one with a view is enough, and the
            // line-of-sight test is a terrain query.
            let mut ground: SmallVec<[(f64, Vector3); 8]> = forms
                .iter()
                .filter(|o| o.1 == observer)
                .map(|o| (dist(o.2, *pos), Vector3::new(o.2.x, height(o.2) + 4., o.2.y)))
                .filter(|(d, _)| *d <= range)
                .chain(
                    bases
                        .iter()
                        .filter(|b| b.0 == observer)
                        .map(|b| (dist(b.1, *pos), Vector3::new(b.1.x, height(b.1) + 15., b.1.y)))
                        .filter(|(d, _)| *d <= range * 0.75),
                )
                .collect();
            ground.sort_by(|a, b| a.0.total_cmp(&b.0));
            let by_ground = ground.iter().take(3).find(|(_, from)| visible(*from, *pos)).map(|(d, _)| *d);
            let by_air = air
                .iter()
                .filter(|(s, _)| *s == observer)
                .map(|(_, p)| (dist(Vector2::new(p.x, p.z), *pos), *p))
                .filter(|(d, _)| *d <= cfg.air_spot_m * hide * vis)
                .find(|(_, p)| visible(*p, *pos))
                .map(|(d, _)| d);
            let by_intel = self.ephemeral.intel_db.contacts_for(observer).any(|c| {
                matches!(c.unit_class, IntelUnitClass::Armor | IntelUnitClass::Infantry | IntelUnitClass::Artillery)
                    && now - c.detected_at <= fresh_intel
                    && dist(c.pos, *pos) <= 1_500.
            });
            let seen_at = match (by_ground, by_air) {
                (Some(g), Some(a)) => Some(g.min(a)),
                (Some(g), None) => Some(g),
                (None, Some(a)) => Some(a),
                (None, None) if by_intel => Some(f64::INFINITY),
                _ => None,
            };
            let Some(d) = seen_at else { continue };
            let key = (observer, *id);
            let fresh = rt
                .spotted
                .get(&key)
                .map_or(true, |s| (now - s.at).num_seconds() > cfg.remember_secs as i64);
            // What it looked like: the picture shows this, not what it is now.
            let (kind, alive) = self
                .formation(*id)
                .map(|f| (self.formation_kind(f), self.formation_strength(f).0))
                .unwrap_or(("formation", 0));
            rt.spotted.insert(
                key,
                Sighting { pos: *pos, heading: *heading, at: now, moving: *moving, close: d <= close_m, kind, alive },
            );
            if fresh {
                new_contacts.push((observer, *id, *pos));
            }
        }
        let keep = Duration::seconds(cfg.remember_secs as i64);
        let formations = &self.persisted.formations;
        rt.spotted.retain(|(_, id), s| now - s.at <= keep && formations.get(id).is_some());
        for (observer, id, pos) in new_contacts {
            let what = rt.spotted.get(&(observer, id)).map(|s| s.kind).unwrap_or("formation");
            let near = self.near_text(pos);
            rt.event(observer, "contact", format_compact!("Enemy {what} spotted{near}"), Some(pos), None, now);
        }
    }

    /// Column, deploying, deployed, dug in. A column that runs into an enemy
    /// it can see stops and deploys into line facing it; a formation that
    /// stands still deploys, and digs in after `dig_in_secs`; one that moves
    /// off with nothing near it closes back up into column.
    fn deploy_states(&mut self, rt: &mut FormationRt, cfg: &GroundCombatCfg, now: DateTime<Utc>) {
        let ids: SmallVec<[FormationId; 16]> = self.formations().map(|f| f.id).collect();
        let reach = cfg.engage_m * 1.5;
        for id in ids {
            let f = self.persisted.formations.get(&id).unwrap();
            let (side, pos, name) = (f.side, f.pos, f.name.clone());
            let threat = rt
                .sightings(side)
                .filter(|(_, s)| s.at == now && dist(s.pos, pos) <= reach)
                .map(|(_, s)| s.pos)
                .min_by(|a, b| dist(*a, pos).total_cmp(&dist(*b, pos)));
            let target = match f.order {
                Order::Attack(oid) => self
                    .persisted
                    .objectives
                    .get(&oid)
                    .map(|o| o.zone.pos())
                    .filter(|p| dist(*p, pos) <= reach + 2_000.),
                _ => None,
            };
            let contact = threat.or(target);
            if contact.is_some() {
                rt.contact_ts.insert(id, now);
            }
            let quiet_for = rt.contact_ts.get(&id).map_or(i64::MAX, |t| (now - *t).num_seconds());
            let stationary = f.posture != Posture::Moving;
            let held = (now - f.deployment_ts.unwrap_or(f.order_ts)).num_seconds();
            // On a player's Move it drives on past what it only sees, and
            // stops to fight only when the enemy is close enough to engage.
            let marching = f.marching(now) && !threat.map_or(false, |t| dist(t, pos) <= cfg.engage_m);
            let next = match (f.deployment, contact.is_some()) {
                (Deployment::Column, _) if marching => Deployment::Column,
                (Deployment::Deploying | Deployment::Deployed | Deployment::DugIn, _) if marching => Deployment::Column,
                (Deployment::Column, true) if !f.broken => Deployment::Deploying,
                (Deployment::Deploying, _) if held >= cfg.deploy_secs as i64 => Deployment::Deployed,
                (Deployment::Column, false) if stationary => Deployment::Deployed,
                (Deployment::Deployed, false) if stationary && held >= cfg.dig_in_secs as i64 => Deployment::DugIn,
                (Deployment::Deployed | Deployment::DugIn, false) if !stationary && quiet_for >= 180 => {
                    Deployment::Column
                }
                // Moving off: it leaves its prepared positions.
                (Deployment::DugIn, _) if !stationary => Deployment::Deployed,
                (d, _) => d,
            };
            if next == f.deployment {
                continue;
            }
            let live = rt.is_live(f);
            let f = self.persisted.formations.get_mut_cow(&id).unwrap();
            f.deployment = next;
            f.deployment_ts = Some(now);
            if let Some(c) = contact {
                if dist(c, pos) > 1. {
                    f.heading = heading_of(c - pos);
                }
            }
            self.ephemeral.dirty();
            if live {
                rt.route_dirty.insert(id);
            } else if let Err(e) = self.place_units(id) {
                warn!("ground war: laying out formation {id}: {e:?}");
            }
            match next {
                Deployment::Deploying => {
                    let near = self.near_text(pos);
                    rt.event(side, "battle", format_compact!("{name} has made contact{near} and is deploying"), Some(pos), Some(id), now);
                }
                Deployment::DugIn => {
                    rt.event(side, "order", format_compact!("{name} has dug in"), Some(pos), Some(id), now);
                }
                _ => (),
            }
        }
    }

    /// One round of fighting on the map, every `combat_secs`, for every force
    /// the server isn't simulating in DCS: formations, and the garrisons of
    /// bases that aren't spawned. Each force in reach of an enemy it can see
    /// deals damage in proportion to its power, split across what it is
    /// fighting; damage kills vehicles once it adds up to their toughness
    /// (`combat`). A force in DCS still shoots back on the map -- the force it
    /// is shooting at isn't in DCS to be hit -- but takes its losses in DCS.
    fn fight(&mut self, rt: &mut FormationRt, cfg: &GroundCombatCfg, lua: MizLua, now: DateTime<Utc>) {
        let every = Duration::seconds(cfg.combat_secs.max(10) as i64);
        let minutes = match rt.last_combat {
            None => {
                rt.last_combat = Some(now);
                return;
            }
            Some(t) if now - t < every => return,
            Some(t) => ((now - t).num_seconds() as f64 / 60.).min(5.),
        };
        rt.last_combat = Some(now);

        #[derive(Clone, Copy, PartialEq)]
        enum Who {
            F(FormationId),
            G(ObjectiveId),
        }
        struct Force {
            who: Who,
            side: Side,
            pos: Vector2,
            /// How far it reaches beyond `engage_m` (a garrison's zone).
            extra: f64,
            power: f64,
            raw: f64,
            cover: f64,
            /// In DCS: takes no damage on the map.
            live: bool,
            units: SmallVec<[(UnitId, Role); 32]>,
            name: CompactString,
        }
        let mut forces: Vec<Force> = vec![];
        for f in self.formations() {
            let units = self.formation_units(f);
            if units.is_empty() {
                continue;
            }
            let raw = combat::raw_power(cfg, units.iter().map(|(_, r)| r));
            forces.push(Force {
                who: Who::F(f.id),
                side: f.side,
                pos: f.pos,
                extra: 0.,
                power: raw * self.formation_condition(f).factor(cfg),
                raw,
                cover: f.deployment.cover(cfg),
                live: rt.is_live(f),
                units,
                name: f.name.as_str().into(),
            });
        }
        // Garrisons of enemy bases a formation is close to.
        let near_bases: FxHashSet<ObjectiveId> = self
            .objectives()
            .filter(|(_, o)| o.owner() != Side::Neutral && !crate::groundwar::at_sea(o.kind()))
            .filter(|(_, o)| {
                forces.iter().any(|f| {
                    f.side != o.owner() && dist(f.pos, o.zone.pos()) <= cfg.engage_m + o.zone.radius() * 0.5
                })
            })
            .map(|(id, _)| *id)
            .collect();
        for oid in near_bases {
            let Some(o) = self.persisted.objectives.get(&oid) else { continue };
            let side = o.owner();
            let mut units: SmallVec<[(UnitId, Role); 32]> = SmallVec::new();
            if let Some(gids) = o.groups.get(&side) {
                for gid in gids {
                    let Some(g) = self.persisted.groups.get(gid) else { continue };
                    if g.kind != Some(GroupCategory::Ground) || matches!(g.class, ObjGroupClass::Logi | ObjGroupClass::Services) {
                        continue;
                    }
                    for uid in &g.units {
                        if let Some(u) = self.persisted.units.get(uid) {
                            if !u.dead {
                                units.push((*uid, self.unit_role(&u.typ, g.class)));
                            }
                        }
                    }
                }
            }
            if units.is_empty() {
                continue;
            }
            let raw = combat::raw_power(cfg, units.iter().map(|(_, r)| r));
            let cond = Condition {
                supply: o.supply() as f64 / 100.,
                morale: 1.,
                deployment: Deployment::DugIn,
                broken: false,
            };
            forces.push(Force {
                who: Who::G(oid),
                side,
                pos: o.zone.pos(),
                extra: o.zone.radius() * 0.5,
                power: raw * cond.factor(cfg),
                raw,
                cover: cfg.garrison_cover.max(1.),
                live: o.spawned,
                units,
                name: format_compact!("{} garrison", o.name),
            });
        }
        let sees = |rt: &FormationRt, side: Side, who: Who| match who {
            Who::G(_) => true,
            Who::F(id) => rt.spotted.get(&(side, id)).map_or(false, |s| s.at == now),
        };
        let n = forces.len();
        let use_factor: Vec<f64> = forces
            .iter()
            .map(|f| match f.who {
                Who::F(id) => self.formation(id).map_or(1., |f| self.supply_use_factor(f)),
                Who::G(_) => 1.,
            })
            .collect();
        let mut damage = vec![0f64; n];
        // (target, attacker, damage)
        let mut credit: Vec<(usize, usize, f64)> = vec![];
        let mut fighting = vec![false; n];
        for a in 0..n {
            let targets: SmallVec<[usize; 8]> = (0..n)
                .filter(|b| {
                    let (x, y) = (&forces[a], &forces[*b]);
                    x.side != y.side
                        && !matches!((x.who, y.who), (Who::G(_), Who::G(_)))
                        && dist(x.pos, y.pos) <= cfg.engage_m + x.extra + y.extra
                        && sees(rt, x.side, y.who)
                })
                .collect();
            if targets.is_empty() {
                continue;
            }
            fighting[a] = true;
            let tp: SmallVec<[(f64, f64); 8]> = targets.iter().map(|b| (forces[*b].power, forces[*b].cover)).collect();
            let dealt = combat::damage_dealt(cfg, forces[a].power, minutes, &tp);
            for (b, d) in targets.iter().zip(dealt) {
                fighting[*b] = true;
                if !forces[*b].live {
                    damage[*b] += d;
                    credit.push((*b, a, d));
                }
            }
        }
        // Artillery in range fires in support: any battery of a side's that
        // can reach an enemy that side has in sight and is fighting. On the
        // map its fire is more damage (indirect fire, so cover counts
        // double); on an enemy in DCS, a real fire mission.
        let mut fire_missions: SmallVec<[(Side, Vector2); 4]> = SmallVec::new();
        let mut arty_events: SmallVec<[(Side, CompactString, Vector2); 4]> = SmallVec::new();
        if cfg.artillery_support {
            let range = self
                .ephemeral
                .cfg
                .artillery
                .as_ref()
                .map_or(cfg.artillery_support_m, |a| a.default_max_range_m.max(1_000.));
            // (side, where, power, base name) of every battery.
            let mut batteries: SmallVec<[(Side, Vector2, f64, CompactString); 16]> = SmallVec::new();
            for (_, o) in self.objectives() {
                let side = o.owner();
                if side == Side::Neutral {
                    continue;
                }
                let Some(gids) = o.groups().get(&side) else { continue };
                let mut power = 0.;
                for gid in gids {
                    let Some(g) = self.persisted.groups.get(gid) else { continue };
                    for uid in &g.units {
                        if let Some(u) = self.persisted.units.get(uid) {
                            if !u.dead && self.unit_role(&u.typ, g.class) == Role::Artillery {
                                power += Role::Artillery.firepower(cfg);
                            }
                        }
                    }
                }
                if power > 0. {
                    let supply = 0.3 + 0.7 * (o.supply() as f64 / 100.);
                    batteries.push((side, o.pos(), power * supply, o.name().into()));
                }
            }
            for b in 0..n {
                if !fighting[b] {
                    continue;
                }
                let enemy = forces[b].side.opposite();
                if !sees(rt, enemy, forces[b].who) {
                    continue;
                }
                let support: SmallVec<[&(Side, Vector2, f64, CompactString); 4]> = batteries
                    .iter()
                    .filter(|(s, p, ..)| *s == enemy && dist(*p, forces[b].pos) <= range && dist(*p, forces[b].pos) >= 3_000.)
                    .take(3)
                    .collect();
                if support.is_empty() {
                    continue;
                }
                if forces[b].live {
                    fire_missions.push((enemy, forces[b].pos));
                } else {
                    let power: f64 = support.iter().map(|(_, _, p, _)| *p).sum();
                    let d = cfg.lethality * power * minutes * 0.6 / (forces[b].cover * forces[b].cover).max(1.);
                    damage[b] += d;
                    credit.push((b, usize::MAX, d));
                }
                arty_events.push((enemy, support[0].3.clone(), forces[b].pos));
            }
        }
        let mut kills = vec![0u32; n];
        let mut arty_kills: FxHashMap<Side, u32> = FxHashMap::default();
        for b in 0..n {
            if damage[b] <= 0. {
                continue;
            }
            let carried = match forces[b].who {
                Who::F(id) => self.persisted.formations.get(&id).map_or(0., |f| f.damage),
                Who::G(oid) => rt.garrison_damage.get(&oid).copied().unwrap_or(0.),
            };
            let units: SmallVec<[(Role, u64); 32]> =
                forces[b].units.iter().map(|(u, r)| (*r, u.inner() as u64)).collect();
            let cap = ((units.len() as f64 * 0.25).ceil() as usize).max(1);
            let seed = now.timestamp() as u64 ^ (b as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15);
            let (dead, left) = combat::casualties(&units, carried + damage[b], cap, seed);
            let lost_raw: f64 = dead.iter().map(|i| units[*i].0.firepower(cfg)).sum();
            for i in &dead {
                let uid = forces[b].units[*i].0;
                if let Ok(u) = unit_mut!(self, uid) {
                    u.dead = true;
                    u.pos = u.spawn_pos;
                    u.heading = u.spawn_heading;
                    u.position = u.spawn_position;
                }
            }
            let k = dead.len() as u32;
            let (side, pos, name) = (forces[b].side, forces[b].pos, forces[b].name.clone());
            match forces[b].who {
                Who::F(id) => {
                    if let Some(f) = self.persisted.formations.get_mut_cow(&id) {
                        f.damage = left;
                        f.losses += k;
                        if forces[b].raw > 0. {
                            f.morale = (f.morale - cfg.morale_per_loss * lost_raw / forces[b].raw).max(0.);
                        }
                    }
                }
                Who::G(oid) => {
                    rt.garrison_damage.insert(oid, left);
                    if k > 0 {
                        if let Err(e) = self.update_objective_status(&oid, now) {
                            warn!("ground war: status of {name} after losses: {e:?}");
                        }
                    }
                }
            }
            if k == 0 {
                continue;
            }
            self.ephemeral.dirty();
            // Credit the shooters in proportion to the damage they did.
            let total: f64 = credit.iter().filter(|(t, ..)| *t == b).map(|(_, _, d)| d).sum();
            let mut given = 0;
            let mut best: Option<(usize, f64)> = None;
            for (_, a, d) in credit.iter().filter(|(t, ..)| *t == b) {
                let share = ((k as f64) * d / total.max(1e-9)).floor() as u32;
                if *a == usize::MAX {
                    *arty_kills.entry(forces[b].side.opposite()).or_default() += share;
                } else {
                    kills[*a] += share;
                }
                given += share;
                if best.map_or(true, |(_, x)| *d > x) {
                    best = Some((*a, *d));
                }
            }
            if let Some((a, _)) = best {
                let rest = k - given.min(k);
                if a == usize::MAX {
                    *arty_kills.entry(forces[b].side.opposite()).or_default() += rest;
                } else {
                    kills[a] += rest;
                }
            }
            rt.battle.note_losses(pos, side, k);
            let near = self.near_text(pos);
            info!("ground war: {name} loses {k} vehicle(s) in fighting on the map{near}");
            let fid = match forces[b].who {
                Who::F(id) => Some(id),
                Who::G(_) => None,
            };
            rt.event(side, "loss", format_compact!("{name} lost {k} vehicle(s){near}"), Some(pos), fid, now);
        }
        for a in 0..n {
            let (side, pos) = (forces[a].side, forces[a].pos);
            if kills[a] > 0 {
                let near = self.near_text(pos);
                let k = kills[a];
                let name = forces[a].name.clone();
                rt.event(side, "kill", format_compact!("{name} destroyed {k} enemy vehicle(s){near}"), Some(pos), match forces[a].who {
                    Who::F(id) => Some(id),
                    Who::G(_) => None,
                }, now);
            }
            if let Who::F(id) = forces[a].who {
                if let Some(f) = self.persisted.formations.get_mut_cow(&id) {
                    f.kills += kills[a];
                    if fighting[a] {
                        f.supply = (f.supply - cfg.supply_per_combat_min * minutes * use_factor[a]).max(0.);
                    }
                }
            }
        }
        // Supporting fire: say so once in a while, and put real rounds on
        // enemies DCS is simulating.
        for (side, base, pos) in arty_events {
            let key = (side, (pos.x / 5_000.).round() as i64, (pos.y / 5_000.).round() as i64);
            let due = rt.fire_said.get(&key).map_or(true, |t| (now - *t).num_seconds() >= 600);
            if due {
                rt.fire_said.insert(key, now);
                let near = self.near_text(pos);
                rt.event(side, "battle", format_compact!("Artillery at {base} is firing in support{near}"), Some(pos), None, now);
            }
        }
        for (side, k) in arty_kills {
            if k > 0 {
                rt.event(side, "kill", format_compact!("Our artillery destroyed {k} enemy vehicle(s)"), None, None, now);
            }
        }
        if let Some(acfg) = self.ephemeral.cfg.artillery.clone() {
            for (side, pos) in fire_missions {
                let key = (side, (pos.x / 3_000.).round() as i64, (pos.y / 3_000.).round() as i64);
                if rt.fire_ts.get(&key).map_or(false, |t| (now - *t).num_seconds() < 180) {
                    continue;
                }
                rt.fire_ts.insert(key, now);
                match self.artillery_strike(lua, side, None, super::actions::WithPos { cfg: acfg.clone(), pos }) {
                    Ok(_) => info!("ground war: {side:?} artillery fire mission in support at {pos:?}"),
                    Err(e) => log::debug!("ground war: {side:?} supporting fire at {pos:?}: {e:?}"),
                }
            }
        }
        rt.fire_ts.retain(|_, t| (now - *t).num_seconds() < 600);
        rt.fire_said.retain(|_, t| (now - *t).num_seconds() < 1_800);
        // A garrison no longer under fire stops carrying damage over.
        let hot: FxHashSet<ObjectiveId> = forces
            .iter()
            .filter_map(|f| match f.who {
                Who::G(oid) => Some(oid),
                _ => None,
            })
            .collect();
        rt.garrison_damage.retain(|oid, _| hot.contains(oid));
    }

    /// Supply and morale. Out of contact and within `supply_range_m` of a
    /// friendly base that has supply, a formation refuels, rearms and
    /// recovers its nerve; cut off from one, its morale slowly drains. A
    /// formation whose morale breaks falls back to the nearest friendly base
    /// whatever its orders, and rallies if it recovers on the way.
    fn sustain(&mut self, rt: &mut FormationRt, cfg: &GroundCombatCfg, lua: MizLua, dt: f64, now: DateTime<Utc>) {
        let bases: SmallVec<[(ObjectiveId, Side, Vector2, bool); 128]> = self
            .objectives()
            .filter(|(_, o)| o.owner() != Side::Neutral && !crate::groundwar::at_sea(o.kind()))
            .map(|(id, o)| (*id, o.owner(), o.pos(), o.supply() >= cfg.supply_base_pct))
            .collect();
        let mins = dt / 60.;
        let ids: SmallVec<[FormationId; 16]> = self.formations().map(|f| f.id).collect();
        let forms: SmallVec<[(Side, Vector2); 32]> = self.formations().map(|f| (f.side, f.pos)).collect();
        // (base, vehicle loads) to take out of each supplying base's stores.
        let mut drains: FxHashMap<ObjectiveId, f64> = FxHashMap::default();
        let mut broke: SmallVec<[(FormationId, Side, CompactString, Vector2, ObjectiveId); 4]> = SmallVec::new();
        for id in ids {
            let f = self.persisted.formations.get(&id).unwrap();
            let (side, pos, name, home) = (f.side, f.pos, f.name.clone(), f.home);
            // Supplied by the nearest friendly base in range that has stores,
            // along a line the enemy isn't sitting on: an enemy base astride
            // the road, or an enemy formation close to it, cuts it.
            let line_open = |from: Vector2| {
                !super::logistics::route_interdicted(&self.persisted, side, from, pos, cfg.supply_line_cut_m)
                    && !forms
                        .iter()
                        .any(|(s, p)| *s != side && seg_dist(from, pos, *p) <= cfg.supply_line_cut_m)
            };
            let supplier = bases
                .iter()
                .filter(|(_, s, p, ok)| *s == side && *ok && dist(*p, pos) <= cfg.supply_range_m)
                .filter(|(_, _, p, _)| line_open(*p))
                .min_by(|a, b| dist(a.2, pos).total_cmp(&dist(b.2, pos)))
                .map(|(oid, ..)| *oid);
            let supplied = supplier.is_some();
            // Cut off with a friendly base in range is encircled; with none in
            // range it has simply outrun its supply.
            let encircled = !supplied
                && bases
                    .iter()
                    .any(|(_, s, p, ok)| *s == side && *ok && dist(*p, pos) <= cfg.supply_range_m);
            let vehicles = self.persisted.formations.get(&id).map_or(0, |f| self.formation_units(f).len()) as f64;
            let fighting = rt
                .contact_ts
                .get(&id)
                .map_or(false, |t| (now - *t).num_seconds() < (cfg.combat_secs.max(10) * 2) as i64);
            if supplied {
                rt.supplied.insert(id);
                if rt.cut_off_said.remove(&id) {
                    rt.event(side, "supply", format_compact!("{name} is back in supply"), Some(pos), Some(id), now);
                }
            } else {
                rt.supplied.remove(&id);
                if rt.cut_off_said.insert(id) {
                    let why = if encircled { "is encircled: its supply line is cut" } else { "is beyond reach of supply" };
                    rt.event(side, "supply", format_compact!("{name} {why}"), Some(pos), Some(id), now);
                }
            }
            let f = self.persisted.formations.get_mut_cow(&id).unwrap();
            if let (Some(base), false) = (supplier, fighting) {
                let gain = (cfg.resupply_per_min * mins).min(1. - f.supply).max(0.);
                f.supply += gain;
                f.morale = (f.morale + cfg.morale_recovery_per_min * mins).min(1.);
                *drains.entry(base).or_default() += gain * vehicles;
            } else if !supplied {
                // Encircled troops lose heart faster than ones that have just
                // outrun their trucks.
                let rate = if encircled { 0.006 } else { 0.002 };
                f.morale = (f.morale - rate * mins).max(0.);
            }
            if !f.broken && f.morale < cfg.break_morale {
                f.broken = true;
                f.commander = None;
                f.locked_until = None;
                // Home if it is still ours, else the nearest friendly base.
                let dest = bases
                    .iter()
                    .find(|(oid, s, ..)| *oid == home && *s == side)
                    .or_else(|| {
                        bases
                            .iter()
                            .filter(|(_, s, ..)| *s == side)
                            .min_by(|a, b| dist(a.2, pos).total_cmp(&dist(b.2, pos)))
                    })
                    .map(|(oid, ..)| *oid);
                if let Some(dest) = dest {
                    broke.push((id, side, name.as_str().into(), pos, dest));
                }
            } else if f.broken && f.morale >= 0.5 {
                f.broken = false;
                rt.event(side, "order", format_compact!("{name} has rallied"), Some(pos), Some(id), now);
            }
        }
        if !drains.is_empty() {
            for (oid, loads) in drains {
                self.drain_base(cfg, oid, loads);
            }
            if let Err(e) = self.update_supply_status() {
                warn!("ground war: supply status after resupply: {e:?}");
            }
        }
        for (id, side, name, pos, dest) in broke {
            let dname = self.persisted.objectives.get(&dest).map(|o| o.name.clone()).unwrap_or_default();
            info!("ground war: {side:?} {name} has broken, falling back to {dname}");
            if let Err(e) = self.order_formation(rt, lua, id, Order::Withdraw(dest), None, now) {
                warn!("ground war: {name} falling back: {e:?}");
            }
            rt.event(side, "broken", format_compact!("{name} has broken and is falling back to {dname}"), Some(pos), Some(id), now);
            rt.event(side.opposite(), "broken", format_compact!("An enemy formation is breaking{}", self.near_text(pos)), Some(pos), None, now);
            self.ephemeral.msgs().panel_to_side(
                15,
                false,
                side,
                format_compact!("{name} has broken under fire and is falling back to {dname}."),
            );
        }
        self.ephemeral.dirty();
    }

    /// Coalition-only F10 pins (and an arrow toward the objective it is
    /// attacking) for every formation.
    fn draw_pins(&mut self, rt: &mut FormationRt, now: DateTime<Utc>) {
        let gone: SmallVec<[FormationId; 8]> = rt
            .pins
            .keys()
            .copied()
            .filter(|id| self.persisted.formations.get(id).is_none())
            .collect();
        for id in gone {
            self.forget_formation(rt, id);
        }
        let ids: SmallVec<[FormationId; 16]> = self.formations().map(|f| f.id).collect();
        for id in ids {
            let f = self.persisted.formations.get(&id).unwrap();
            let text = self.formation_pin_text(f);
            let (side, pos) = (f.side, f.pos);
            let arrow_to = match f.order {
                Order::Attack(oid) => self.persisted.objectives.get(&oid).map(|o| o.zone.pos()),
                _ => None,
            };
            if let Some(pin) = rt.pins.get(&id) {
                let changed = pin.text != text || dist(pin.pos, pos) > PIN_MOVE_M;
                if !changed || now - pin.ts < Duration::seconds(PIN_MIN_SECS) {
                    continue;
                }
            }
            if let Some(pin) = rt.pins.remove(&id) {
                self.ephemeral.msgs().delete_mark(pin.mark);
                if let Some(a) = pin.arrow {
                    self.ephemeral.msgs().delete_mark(a);
                }
            }
            let mark = self.ephemeral.msgs().mark_to_side(side, pos, true, text.clone());
            let arrow = arrow_to.and_then(|t| {
                let d = t - pos;
                let n = d.norm();
                if n < 3_000. {
                    return None;
                }
                let dir = d / n;
                let tail = pos + dir * 800.;
                let head = pos + dir * (n - 1_500.).min(10_000.);
                let v3 = |p: Vector2| LuaVec3(Vector3::new(p.x, 0., p.y));
                let col = crate::mapcolor::side_color(side, 0.85);
                let id = MarkId::new();
                self.ephemeral.msgs().arrow_to(
                    SideFilter::from(side),
                    id,
                    // DCS draws an arrow's head at `start` (the supply
                    // arrows in markup.rs do the same), so the head -- the
                    // end toward the target -- goes there.
                    ArrowSpec {
                        start: v3(head),
                        end: v3(tail),
                        color: col,
                        fill_color: crate::mapcolor::side_color(side, 0.35),
                        line_type: LineType::Solid,
                        read_only: true,
                    },
                    None,
                );
                Some(id)
            });
            rt.pins.insert(id, Pin { mark, arrow, pos, text, ts: now });
        }
    }

    /// One pass of the ground war's mechanics (not the AI): movement,
    /// arrivals, assaults, who is in DCS, spotting, deployment, fighting,
    /// supply and morale, map pins.
    pub fn tick_formations(
        &mut self,
        rt: &mut FormationRt,
        lua: MizLua,
        idx: &MizIndex,
        perf: &mut PerfInner,
        now: DateTime<Utc>,
    ) {
        let Ok(cfg) = self.ground_war_cfg() else { return };
        let dt = rt
            .last_tick
            .map(|t| (now - t).num_milliseconds().max(0) as f64 / 1000.)
            .unwrap_or(0.)
            .min(120.);
        rt.last_tick = Some(now);
        self.sync_live(rt, lua, now);
        // Before cleanup, which takes a wiped-out formation's groups away.
        self.wreck_fires(rt, &cfg, lua, now);
        self.shed_immobile(rt, now);
        self.cleanup(rt, &cfg, now);
        self.advance_on_map(rt, &cfg, dt);
        self.unstick_live(rt, lua, now);
        self.arrivals(rt, &cfg, now);
        self.assaults(rt, &cfg, lua, idx, now);
        self.plan_live(rt, &cfg, lua, now);
        if let Err(e) = self.spawn_next(rt, &cfg, lua, idx, perf) {
            warn!("ground war: {e:?}");
        }
        self.issue_routes(rt, &cfg, lua);
        self.spot(rt, &cfg.combat, lua, now);
        self.deploy_states(rt, &cfg.combat, now);
        if !cfg.live_only {
            // With `live_only` every fight is in DCS, and DCS deals the losses.
            self.fight(rt, &cfg.combat, lua, now);
        }
        self.sustain(rt, &cfg.combat, lua, dt, now);
        if cfg.map_pins {
            self.draw_pins(rt, now);
        }
        self.track_battles(rt, &cfg, lua, now);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ordinals() {
        let got: Vec<CompactString> = [1, 2, 3, 4, 11, 12, 13, 21, 22, 101, 111].iter().map(|n| ordinal(*n)).collect();
        assert_eq!(got, ["1st", "2nd", "3rd", "4th", "11th", "12th", "13th", "21st", "22nd", "101st", "111th"]);
    }

    #[test]
    fn column_follows_the_road_back() {
        // Came up the y axis to (0, 0), turned east, head now at (100, 0).
        // Trail newest last.
        let trail = [Vector2::new(0., -100.), Vector2::new(0., 0.)];
        let head = Vector2::new(100., 0.);
        let (p, _) = back_along(head, 0., &trail, 50.);
        assert!((p - Vector2::new(50., 0.)).norm() < 1e-6);
        // Past the corner it follows the earlier leg.
        let (p, _) = back_along(head, 0., &trail, 150.);
        assert!((p - Vector2::new(0., -50.)).norm() < 1e-6);
        // Past the end of the trail it carries straight on back.
        let (p, _) = back_along(head, 0., &trail, 250.);
        assert!((p - Vector2::new(0., -150.)).norm() < 1e-6);
    }

    #[test]
    fn a_formation_beside_the_road_cuts_it_and_one_far_off_does_not() {
        let base = Vector2::new(0., 0.);
        let front = Vector2::new(20_000., 0.);
        assert!(seg_dist(base, front, Vector2::new(10_000., 2_000.)) <= 3_000.);
        assert!(seg_dist(base, front, Vector2::new(10_000., 9_000.)) > 3_000.);
        // Past the end of the line it is the distance to the end.
        assert!((seg_dist(base, front, Vector2::new(25_000., 0.)) - 5_000.).abs() < 1e-6);
    }

    #[test]
    fn a_deployed_line_faces_its_heading() {
        // Facing north (+x): the line runs east-west, centred on the spot.
        let pts: Vec<Vector2> = (0..4).map(|i| line_slot(Vector2::new(0., 0.), 0., i, 4)).collect();
        assert!(pts.iter().all(|p| p.x.abs() < 1e-6));
        let sum: f64 = pts.iter().map(|p| p.y).sum();
        assert!(sum.abs() < 1e-6);
        // A big company forms two ranks, the second behind the first.
        let back = line_slot(Vector2::new(0., 0.), 0., 9, 12);
        assert!(back.x < -100.);
    }

    #[test]
    fn decimate_keeps_ends() {
        let pts: Vec<Vector2> = (0..=100).map(|i| Vector2::new(i as f64 * 10., 0.)).collect();
        let d = decimate(&pts, 200.);
        assert_eq!(d.first(), pts.first());
        assert_eq!(d.last(), pts.last());
        assert!(d.len() <= 7);
    }

    #[test]
    fn want_priority_puts_players_first() {
        let p = Want { player: true, ..Default::default() };
        let c = Want { contact: true, target: true, assault: true, ..Default::default() };
        assert!(p.priority() > c.priority());
        assert!(c.fighting() && !p.fighting());
    }
}
