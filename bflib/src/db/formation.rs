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
//!   near, when it meets an enemy formation, or when it closes on the
//!   objective it is attacking -- as many as `max_live_formations` allow.
//!   Two enemy formations that meet while the budget is spent halt and fight
//!   it out on the map instead (`attrition`).
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

use super::{group::DeployKind, objective::ObjGroupClass, persisted::Persisted, Db};
use crate::{
    group, group_mut, objective, objective_mut,
    spawnctx::{Despawn, SpawnCtx, SpawnLoc},
    unit_mut,
};
use anyhow::{anyhow, bail, Context, Result};
use bfprotocols::{
    cfg::GroundWarCfg,
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
        ActionTyp, AiOption, AlarmState, AltType, GroundOption, MissionPoint, PointType, Task,
        VehicleFormation,
    },
    env::miz::MizIndex,
    group::{Group, GroupCategory},
    land::{Land, RoadType},
    net::Ucid,
    trigger::{ArrowSpec, LineType, MarkId, SideFilter},
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
}

impl Formation {
    pub fn ai_controlled(&self, now: DateTime<Utc>) -> bool {
        self.locked_until.map_or(true, |t| now >= t)
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
    attrition_ts: FxHashMap<FormationId, DateTime<Utc>>,
    assault_ts: FxHashMap<ObjectiveId, DateTime<Utc>>,
    /// Formations already told they have no infantry for an assault.
    no_infantry_said: FxHashSet<FormationId>,
    pins: FxHashMap<FormationId, Pin>,
    sync_cursor: usize,
    last_tick: Option<DateTime<Utc>>,
    /// Battles, smoke and burning wrecks (`super::battle`).
    pub(super) battle: super::battle::BattleRt,
}

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
}

/// Why a formation wants to be in DCS, best reason first.
#[derive(Debug, Clone, Copy, Default)]
struct Want {
    player: bool,
    contact: bool,
    target: bool,
    assault: bool,
}

impl Want {
    fn any(&self) -> bool {
        self.player || self.contact || self.target || self.assault
    }

    fn fighting(&self) -> bool {
        self.contact || self.target || self.assault
    }

    fn priority(&self) -> u32 {
        (self.player as u32) * 8 + (self.assault as u32) * 4 + (self.contact as u32) * 2
            + self.target as u32
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

fn ground_point<'lua>(
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
    speed_mps: f64,
) -> Vec<MissionPoint<'lua>> {
    let along = if off_road { VehicleFormation::OffRoad } else { VehicleFormation::OnRoad };
    let speed = if off_road { speed_mps * 0.5 } else { speed_mps };
    let mut route = vec![ground_point(land, from, VehicleFormation::OffRoad, speed, Task::ComboTask(vec![]))];
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
/// sees the enemy.
fn set_march_ai(group: &Group) -> Result<()> {
    let con = group.get_controller()?;
    con.set_option(AiOption::Ground(GroundOption::DisperseOnAttack(0)))?;
    con.set_option(AiOption::Ground(GroundOption::AlarmState(AlarmState::Auto)))?;
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
    fn ground_war_cfg(&self) -> Result<GroundWarCfg> {
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

    pub fn formation_has_infantry(&self, f: &Formation) -> bool {
        f.groups.iter().any(|gid| {
            self.persisted.groups.get(gid).map_or(false, |g| {
                g.class.is_infantry()
                    && g.units
                        .into_iter()
                        .any(|u| self.persisted.units.get(u).map_or(false, |u| !u.dead))
            })
        })
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
        };
        self.persisted.formations.insert_cow(id, f);
        if let Err(e) = self.update_objective_status(&oid, now) {
            warn!("ground war: status of {oname} after raising {name}: {e:?}");
        }
        self.ephemeral.dirty();
        info!("ground war: {side:?} raised {name} ({id}), {strength0} vehicles from {oname}");
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
    /// column along the road while it moves, in a ring when it stands.
    fn place_units(&mut self, id: FormationId) -> Result<()> {
        let f = self.persisted.formations.get(&id).ok_or_else(|| anyhow!("no formation {id}"))?;
        let (pos, heading, moving, trail) =
            (f.pos, f.heading, f.posture == Posture::Moving, f.trail.clone());
        let uids: SmallVec<[UnitId; 32]> = f
            .groups
            .iter()
            .filter_map(|g| self.persisted.groups.get(g))
            .flat_map(|g| g.units.into_iter().copied())
            .filter(|u| self.persisted.units.get(u).map_or(false, |u| !u.dead))
            .collect();
        for (i, uid) in uids.iter().enumerate() {
            let (p, h) = if moving {
                back_along(pos, heading, &trail, i as f64 * COLUMN_GAP_M)
            } else {
                (ring_slot(pos, i), heading)
            };
            let u = unit_mut!(self, uid)?;
            u.pos = p;
            u.heading = h;
            u.position.p.0.x = p.x;
            u.position.p.0.z = p.y;
        }
        Ok(())
    }

    /// Move the formations that are on the map only along their paths.
    fn advance_on_map(&mut self, rt: &FormationRt, cfg: &GroundWarCfg, dt: f64) {
        let ids: SmallVec<[FormationId; 16]> = self
            .formations()
            .filter(|f| f.posture == Posture::Moving && !rt.is_live(f) && !rt.is_halted(f.id))
            .map(|f| f.id)
            .collect();
        for id in ids {
            let f = self.persisted.formations.get_mut_cow(&id).unwrap();
            let speed = cfg.speed_kph.max(1.) / 3.6 * if f.off_road { 0.5 } else { 1. };
            let mut budget = speed * dt;
            while budget > 0. && !f.path.is_empty() {
                let next = f.path[0];
                let d = dist(f.pos, next);
                if d > 1. {
                    f.heading = heading_of(next - f.pos);
                }
                if d <= budget {
                    budget -= d;
                    f.pos = next;
                    f.trail.push(next);
                    f.path.remove(0);
                } else {
                    f.pos += (next - f.pos) / d * budget;
                    budget = 0.;
                }
            }
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
            let f = self.persisted.formations.get_mut_cow(&id).unwrap();
            if dist(c, f.pos) > 5. {
                f.heading = heading_of(c - f.pos);
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

    /// Positions of `side`'s enemies on the ground: formations and the
    /// objectives they hold.
    fn enemy_formations_near(&self, side: Side, pos: Vector2, r: f64) -> bool {
        self.formations().any(|f| f.side != side && dist(f.pos, pos) <= r)
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
            let (pos, engaged, dest, side, name) =
                (f.pos, self.engaged(f), f.destination(), f.side, f.name.clone());
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
        let ids: SmallVec<[FormationId; 16]> = self
            .formations()
            .filter(|f| {
                f.posture == Posture::Moving
                    && match f.destination() {
                        None => true,
                        Some(d) => dist(f.pos, d) <= cfg.arrive_m,
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
            let (side, name, pos, live) = (f.side, f.name.clone(), f.pos, rt.is_live(f));
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
            if !open || cooling || !live {
                continue;
            }
            match self.launch_assault(rt, lua, idx, id, oid, now) {
                Ok(true) => {
                    rt.assault_ts.insert(oid, now);
                    info!("ground war: {name} sends its infantry into {oname}");
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
                                "{oname} is broken, but {name} has no infantry left to take it. \
                                 Send troops in."
                            ),
                        );
                    }
                }
                Err(e) => warn!("ground war: {name} assault on {oname} at {pos:?}: {e:?}"),
            }
        }
    }

    /// Send formation `id`'s infantry into `oid` as a capture squad. False
    /// if it has none left.
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
        let (side, fpos, home) = (f.side, f.pos, f.home);
        let Some(gid) = f.groups.iter().copied().find(|gid| {
            self.persisted.groups.get(gid).map_or(false, |g| {
                g.class.is_infantry()
                    && g.units
                        .into_iter()
                        .any(|u| self.persisted.units.get(u).map_or(false, |u| !u.dead))
            })
        }) else {
            return Ok(false);
        };
        let obj = objective!(self, oid)?;
        let (zpos, zr) = (obj.zone.pos(), obj.zone.radius());
        let g = group!(self, gid)?;
        let (template, gname) = (g.template_name.clone(), g.name.clone());
        let ipos = self.group_center(&gid)?;
        // Where they get out: where they are, if that is inside the zone,
        // else halfway in from the formation's side.
        let at = if obj.zone.contains(ipos) {
            ipos
        } else {
            let d = fpos - zpos;
            let n = d.norm();
            if n > 1. { zpos + d / n * (zr * 0.4).min(n) } else { zpos }
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
        // The squad IS that infantry group: it leaves the formation, and its
        // empty place goes home to be rebuilt.
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
        let spawn_now = self
            .persisted
            .objectives
            .get(&at)
            .map_or(false, |o| o.owner == f.side && o.spawned);
        for gid in &f.groups {
            let Some(g) = self.persisted.groups.get(gid) else { continue };
            let name = g.name.clone();
            let was_live = rt.live.remove(gid);
            if was_live {
                self.ephemeral.push_despawn(*gid, Despawn::GroupByName(name.to_string()));
            }
            self.attach_to_objective(*gid, f.side, at, rehome)?;
            let Some(g) = self.persisted.groups.get(gid) else { continue };
            let uids: SmallVec<[UnitId; 16]> = g.units.into_iter().copied().collect();
            let mut alive = false;
            for uid in uids {
                let u = unit_mut!(self, uid)?;
                u.pos = u.spawn_pos;
                u.heading = u.spawn_heading;
                u.position = u.spawn_position;
                alive |= !u.dead;
            }
            if alive && spawn_now {
                // After the despawn above has gone through, or DCS keeps the
                // old group where it stood.
                let at_ts = now + Duration::seconds(if was_live { 10 } else { 1 });
                self.ephemeral.delayspawnq.entry(at_ts).or_default().push(*gid);
            }
        }
        if let Err(e) = self.update_objective_status(&at, now) {
            warn!("ground war: status of {at} after {} rejoined: {e:?}", f.name);
        }
        self.forget_formation(rt, id);
        self.ephemeral.dirty();
        info!("ground war: {:?} {} rejoined the garrison at {at}", f.side, f.name);
        Ok(())
    }

    fn forget_formation(&mut self, rt: &mut FormationRt, id: FormationId) {
        rt.wanted_ts.remove(&id);
        rt.halted.remove(&id);
        rt.route_dirty.remove(&id);
        rt.stall.remove(&id);
        rt.attrition_ts.remove(&id);
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
        let all: SmallVec<[(FormationId, Side, Vector2, Order, Posture, bool); 16]> = self
            .formations()
            .map(|f| (f.id, f.side, f.pos, f.order, f.posture, rt.is_live(f)))
            .collect();
        let mut wants: SmallVec<[(FormationId, Want, bool); 16]> = smallvec::smallvec![];
        for (id, side, pos, order, posture, live) in &all {
            let want = Want {
                player: players.iter().any(|p| dist(*p, *pos) <= cfg.player_bubble_m),
                contact: all
                    .iter()
                    .any(|(_, s, p, ..)| s != side && dist(*p, *pos) <= cfg.contact_m),
                target: match order {
                    Order::Attack(oid) => self
                        .persisted
                        .objectives
                        .get(oid)
                        .map_or(false, |o| dist(o.zone.pos(), *pos) <= cfg.contact_m),
                    _ => false,
                },
                assault: *posture == Posture::Assaulting,
            };
            if want.any() {
                rt.wanted_ts.insert(*id, now);
            }
            wants.push((*id, want, *live));
        }
        let grace = Duration::seconds(cfg.despawn_grace_secs as i64);
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
        let mut halted = FxHashSet::default();
        for (id, want) in waiting {
            if budget > 0 {
                budget -= 1;
                self.materialize(rt, id);
            } else if want.fighting() {
                halted.insert(id);
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
        let route = dcs_route(&land, from, &path, off_road, cfg.speed_kph.max(1.) / 3.6);
        let spctx = SpawnCtx::new(lua)?;
        let spawned = self
            .ephemeral
            .spawn_group(perf, &self.persisted, idx, &spctx, group!(self, gid)?, route)
            .with_context(|| format_compact!("spawning formation {id} group {gid}"))?;
        if let Some(crate::spawnctx::Spawned::Group(g)) = spawned {
            if let Err(e) = set_march_ai(&g) {
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
            for gid in f.groups.iter().filter(|g| rt.live.contains(g)) {
                let Some(g) = self.persisted.groups.get(gid) else { continue };
                let from = self.group_center(gid).unwrap_or(f.pos);
                let res = Group::get_by_name(lua, &g.name).and_then(|group| {
                    set_march_ai(&group)?;
                    let route = dcs_route(&land, from, &path, off_road, cfg.speed_kph.max(1.) / 3.6);
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

    /// Two enemy formations in contact that DCS isn't simulating wear each
    /// other down on the map.
    fn attrition(&mut self, rt: &mut FormationRt, cfg: &GroundWarCfg, now: DateTime<Utc>) {
        let every = Duration::seconds(cfg.attrition_secs.max(30) as i64);
        let all: SmallVec<[(FormationId, Side, Vector2, u32, bool); 16]> = self
            .formations()
            .map(|f| (f.id, f.side, f.pos, self.formation_strength(f).0, rt.is_live(f)))
            .collect();
        let mut losses: SmallVec<[(FormationId, u32); 8]> = smallvec::smallvec![];
        for (id, side, pos, _, live) in &all {
            if *live {
                continue;
            }
            let enemy: u32 = all
                .iter()
                .filter(|(_, s, p, _, l)| s != side && !*l && dist(*p, *pos) <= cfg.contact_m)
                .map(|(_, _, _, a, _)| *a)
                .sum();
            if enemy == 0 {
                rt.attrition_ts.remove(id);
                continue;
            }
            let since = *rt.attrition_ts.entry(*id).or_insert(now);
            if now - since < every {
                continue;
            }
            rt.attrition_ts.insert(*id, now);
            let n = ((enemy as f64 * cfg.attrition_rate).ceil() as u32).max(1);
            losses.push((*id, n));
        }
        for (id, n) in losses {
            let f = self.persisted.formations.get(&id).unwrap();
            let name = f.name.clone();
            // The rear of the column first.
            let doomed: SmallVec<[UnitId; 8]> = f
                .groups
                .iter()
                .rev()
                .filter_map(|g| self.persisted.groups.get(g))
                .flat_map(|g| g.units.into_iter().copied())
                .filter(|u| self.persisted.units.get(u).map_or(false, |u| !u.dead))
                .take(n as usize)
                .collect();
            for uid in &doomed {
                if let Ok(u) = unit_mut!(self, uid) {
                    u.dead = true;
                    u.pos = u.spawn_pos;
                    u.heading = u.spawn_heading;
                    u.position = u.spawn_position;
                }
            }
            info!("ground war: {name} loses {} vehicle(s) in contact on the map", doomed.len());
            self.ephemeral.dirty();
        }
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
                let start = pos + dir * 800.;
                let end = pos + dir * (n - 1_500.).min(10_000.);
                let v3 = |p: Vector2| LuaVec3(Vector3::new(p.x, 0., p.y));
                let col = crate::mapcolor::side_color(side, 0.85);
                let id = MarkId::new();
                self.ephemeral.msgs().arrow_to(
                    SideFilter::from(side),
                    id,
                    ArrowSpec {
                        start: v3(start),
                        end: v3(end),
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
    /// arrivals, assaults, who is in DCS, attrition, map pins.
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
        self.attrition(rt, &cfg, now);
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
