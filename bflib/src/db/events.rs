/*
Copyright 2024 Eric Stokes.

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

use anyhow::Result;
use bfprotocols::{
    cfg::{CampaignEventsCfg, UnitTag},
    db::group::GroupId,
};
use chrono::{DateTime, Utc};
use compact_str::{format_compact, CompactString};
use dcso3::{
    coalition::Side,
    env::miz::MizIndex,
    land::{Land, RoadType},
    trigger::MarkId,
    LuaVec2, Vector2,
};
use crate::spawnctx::SpawnCtx;
use std::sync::Arc;
use fxhash::FxHashMap;
use log::*;
use rand::Rng;
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;

use super::Db;
use bfprotocols::db::objective::ObjectiveId;

/// Unique identifier for campaign events
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EventId(u64);

impl EventId {
    pub fn new() -> Self {
        Self(rand::thread_rng().r#gen())
    }
}

/// Types of dynamic campaign events
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CampaignEvent {

    /// Artillery/armor fire-support barrage against a contested position.
    Barrage {
        id: EventId,
        /// Side conducting the barrage (their groups will fire).
        side: Side,
        /// Objective that owns the firing groups.
        source_objective: ObjectiveId,
        /// World-space position to fire at.
        target_pos: Vector2,
        expires_at: DateTime<Utc>,
        /// False on the first tick — used to trigger the fire order exactly once.
        #[serde(default)]
        fire_ordered: bool,
    },
    /// ALCM / ballistic-missile strike ordered by the Smart Commander.
    MissileStrike {
        id: EventId,
        /// Side launching the missiles.
        side: Side,
        /// Deployed group IDs that will fire (ALCM-tagged, pre-selected by commander).
        shooter_gids: SmallVec<[bfprotocols::db::group::GroupId; 4]>,
        /// World-space position to strike.
        target_pos: Vector2,
        expires_at: DateTime<Utc>,
        /// False on the first tick — fire order issued exactly once.
        #[serde(default)]
        fire_ordered: bool,
    },
    /// Enemy ambush force spawned along an active supply-convoy route.
    ConvoyAmbush {
        id: EventId,
        /// Side that set the ambush (enemy of the convoy).
        ambush_side: Side,
        /// Where the ambush sets up: a point ahead on the convoy's way that
        /// the force reaches first. (Name kept for saved events; the force
        /// no longer spawns here.)
        spawn_pos: Vector2,
        /// Friendly objective the ambush force drives out from (and whose
        /// template it uses).
        source_objective: ObjectiveId,
        expires_at: DateTime<Utc>,
        /// False on the first tick — spawn happens exactly once.
        #[serde(default)]
        spawned: bool,
        /// DCS group ID of the convoy being ambushed (for AttackGroup task).
        convoy_group_id: bfprotocols::db::group::GroupId,
        /// Last known convoy position (fallback if AttackGroup unavailable).
        convoy_pos: Vector2,
    },
    /// Enemy CAP orbit spawned over/near an objective when players penetrate enemy airspace.
    EnemyCap {
        id: EventId,
        /// Side that owns the CAP (the defending side).
        cap_side: Side,
        /// Objective the CAP orbits over.
        objective: ObjectiveId,
        /// When the CAP event expires and the aircraft despawn.
        expires_at: DateTime<Utc>,
        /// False until the aircraft is actually spawned (first tick).
        #[serde(default)]
        spawned: bool,
        /// True when this is a HELICOPTER patrol answering enemy helicopter
        /// players rather than a fixed-wing CAP answering jets. Same event,
        /// same spawn/station/RTB machinery -- every knob it reads (template,
        /// altitude, speed, leash, duration, cooldown, launch-field kinds)
        /// switches to the rotary set. Defaults to false so CAP events saved
        /// before helicopter patrols existed load unchanged.
        #[serde(default)]
        rotary: bool,
        /// How many contacts the cluster that triggered this scramble held.
        /// Chooses which templates in the side's roster are eligible, so a
        /// two-ship probe and an eight-ship push are not answered by the same
        /// flight. Defaults to 0 (every roster entry eligible) for events
        /// saved before rosters existed.
        #[serde(default)]
        threat: u32,
    },
    /// Commander-dispatched CAP: a friendly AI CAP flight launched by the Smart
    /// Commander when friendly pilot coverage is thin.  Uses the same DCS spawn
    /// machinery as EnemyCap but is tracked separately so the commander can
    /// enforce a post-expiry cooldown before spawning another.
    CommanderCap {
        id: EventId,
        /// Side that owns (and benefits from) this CAP flight.
        cap_side: Side,
        /// Objective the CAP orbits over.
        objective: ObjectiveId,
        /// When the CAP event expires and the aircraft despawn.
        expires_at: DateTime<Utc>,
        /// False until the aircraft is actually spawned (first tick).
        #[serde(default)]
        spawned: bool,
    },
}

impl CampaignEvent {
    pub fn id(&self) -> EventId {
        match self {

            Self::Barrage { id, .. } => *id,
            Self::MissileStrike { id, .. } => *id,
            Self::ConvoyAmbush { id, .. } => *id,
            Self::EnemyCap { id, .. } => *id,
            Self::CommanderCap { id, .. } => *id,
        }
    }

    /// The side that owns the event -- the one whose units act.
    pub fn side(&self) -> Side {
        match self {
            Self::Barrage { side, .. } | Self::MissileStrike { side, .. } => *side,
            Self::ConvoyAmbush { ambush_side, .. } => *ambush_side,
            Self::EnemyCap { cap_side, .. } | Self::CommanderCap { cap_side, .. } => *cap_side,
        }
    }

    pub fn description(&self) -> CompactString {
        match self {

            Self::Barrage { side, .. } => format_compact!("{:?} Barrage", side),
            Self::MissileStrike { side, .. } => format_compact!("{:?} Missile Strike", side),
            Self::ConvoyAmbush { ambush_side, .. } => format_compact!("{:?} Convoy Ambush", ambush_side),
            Self::EnemyCap { cap_side, rotary, .. } => {
                if *rotary {
                    format_compact!("{:?} Enemy Helo Patrol", cap_side)
                } else {
                    format_compact!("{:?} Enemy CAP", cap_side)
                }
            }
            Self::CommanderCap { cap_side, .. } => format_compact!("{:?} Commander CAP", cap_side),
        }
    }
}

/// DCS-side effects that need to be executed after tick() returns.
#[derive(Debug, Clone)]
pub enum EventEffect {

    /// Issue FireAtPoint orders to armor/LR groups at `source_objective`.
    FireBarrage {
        event_id: EventId,
        side: Side,
        source_objective: ObjectiveId,
        target_pos: Vector2,
    },
    /// Issue FireAtPoint to pre-selected ALCM/missile groups.
    FireMissileStrike {
        event_id: EventId,
        side: Side,
        shooter_gids: SmallVec<[bfprotocols::db::group::GroupId; 4]>,
        target_pos: Vector2,
    },
    /// Send an ambush force for `ambush_side` out of `source_objective` by
    /// road to the intercept `spawn_pos`.
    SpawnAmbush {
        event_id: EventId,
        ambush_side: Side,
        spawn_pos: Vector2,
        source_objective: ObjectiveId,
        /// DCS GroupId of the convoy being ambushed — used to issue AttackGroup order.
        convoy_group_id: bfprotocols::db::group::GroupId,
        /// Last known position of the convoy (fallback if AttackGroup fails).
        convoy_pos: Vector2,
    },
    /// Remove F10 marks associated with a finished event.
    DeleteMarks {
        ids: SmallVec<[MarkId; 4]>,
    },
    /// Spawn a CAP aircraft from a template at an objective for a defending side.
    SpawnCap {
        event_id: EventId,
        cap_side: Side,
        objective: ObjectiveId,
        obj_pos: Vector2,
        /// Helicopter patrol rather than fixed-wing CAP.
        rotary: bool,
        /// Contact count of the incursion, for roster template selection.
        threat: u32,
    },
    /// Despawn all groups registered to a CAP event.
    DespawnCap {
        event_id: EventId,
        cap_side: Side,
        /// The field this wave launched from -- its cooldown starts here.
        objective: ObjectiveId,
        /// Helicopter patrol rather than fixed-wing CAP.
        rotary: bool,
    },

    /// Despawn all groups registered to a convoy-ambush event.
    DespawnAmbush {
        event_id: EventId,
    },

}

/// Ambush forces drive at this, and plan their drive with it.
const AMBUSH_SPEED_MPS: f64 = 12.;
/// The furthest an ambush force drives to get in place (estimated as the
/// straight line times `ROAD_FACTOR`). Past this the convoy is out of reach.
const AMBUSH_MAX_DRIVE_M: f64 = 40_000.;
/// The real road route may wind further than the estimate; past this it is
/// called off.
const AMBUSH_MAX_ROAD_M: f64 = 55_000.;
/// Road distance against the straight line, for planning.
const ROAD_FACTOR: f64 = 1.3;
/// Time to get off the road and into position before the convoy arrives.
const AMBUSH_SETUP_SECS: f64 = 120.;
/// Not right on top of the convoy -- that is a meeting, not an ambush...
const AMBUSH_MIN_LEAD_M: f64 = 2_000.;
/// ...and not at its destination, which is the enemy's own base.
const AMBUSH_DEST_CLEARANCE_M: f64 = 3_000.;
/// How finely the convoy's way ahead is searched for an intercept.
const AMBUSH_SAMPLE_M: f64 = 500.;

/// A convoy's speed for planning; one that never recorded one drives at a
/// typical truck convoy pace.
fn convoy_speed(speed_mps: f64) -> f64 {
    if speed_mps > 0.5 { speed_mps } else { 8. }
}

/// The first point on `path` (a convoy's way ahead, starting where it is
/// now) that a force driving from `from` reaches and sets up at before the
/// convoy gets there, and the estimated drive to it. `None` if there is no
/// such point within driving range.
pub(crate) fn intercept_on_path(path: &[Vector2], convoy_mps: f64, from: Vector2) -> Option<(Vector2, f64)> {
    let total: f64 = path.windows(2).map(|w| (w[1] - w[0]).norm()).sum();
    let mut done = 0.;
    for w in path.windows(2) {
        let (a, b) = (w[0], w[1]);
        let seg = (b - a).norm();
        let mut t = 0.;
        while seg > 1e-6 && t <= seg {
            let s = done + t;
            if s >= AMBUSH_MIN_LEAD_M && total - s >= AMBUSH_DEST_CLEARANCE_M {
                let p = a + (b - a) * (t / seg);
                let drive = (p - from).norm() * ROAD_FACTOR;
                if drive <= AMBUSH_MAX_DRIVE_M
                    && drive / AMBUSH_SPEED_MPS + AMBUSH_SETUP_SECS <= s / convoy_mps
                {
                    return Some((p, drive));
                }
            }
            t += AMBUSH_SAMPLE_M;
        }
        done += seg;
    }
    None
}

/// Which of `ours` (id, position) gets a force ahead of the convoy on `path`
/// with the shortest drive: (id, intercept, drive).
fn best_ambush_source<T: Copy>(
    path: &[Vector2],
    convoy_mps: f64,
    ours: &[(T, Vector2)],
) -> Option<(T, Vector2, f64)> {
    ours.iter()
        .filter_map(|(id, pos)| intercept_on_path(path, convoy_mps, *pos).map(|(p, d)| (*id, p, d)))
        .min_by(|a, b| a.2.total_cmp(&b.2))
}

impl Db {
    /// Send the force for a convoy ambush out of objective `source` by road
    /// to a point ahead of convoy group `convoy_gid` that it reaches first.
    /// The intercept is re-planned on the convoy's real road when it is
    /// still on its way (`planned`, from when the event was set, is the
    /// fallback). `Ok(None)` when no road gets the force there in time or
    /// within range: the ambush is off, nothing spawns.
    pub(crate) fn launch_ambush(
        &mut self,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        source: ObjectiveId,
        template: &str,
        convoy_gid: GroupId,
        planned: Vector2,
    ) -> Result<Option<(GroupId, Vector2)>> {
        let lua = spctx.lua();
        let from = self
            .persisted
            .objectives
            .get(&source)
            .map(|o| o.pos())
            .ok_or_else(|| anyhow::anyhow!("no such objective {source:?}"))?;
        let convoy = self
            .ephemeral
            .active_convoys
            .values()
            .find(|c| c.group_id == convoy_gid)
            .and_then(|c| {
                let dest = self.persisted.objectives.get(&c.destination)?.pos();
                Some((c.last_pos, dest, convoy_speed(c.speed_mps)))
            });
        let mut at = planned;
        if let Some((pos, dest, speed)) = convoy {
            let road: Option<Vec<Vector2>> = Land::singleton(lua)
                .ok()
                .and_then(|l| l.find_path_on_roads(RoadType::Road, LuaVec2(pos), LuaVec2(dest)).ok())
                .map(|seq| seq.into_iter().filter_map(|p| p.ok()).map(|p| p.0).collect());
            if let Some(road) = road.filter(|r| r.len() >= 2) {
                let mut path = Vec::with_capacity(road.len() + 1);
                path.push(pos);
                path.extend(road);
                match intercept_on_path(&path, speed, from) {
                    Some((p, _)) => at = p,
                    None => {
                        info!(
                            "SpawnAmbush: {side:?} can no longer get ahead of convoy {convoy_gid:?} \
                             on its road"
                        );
                        return Ok(None);
                    }
                }
            }
        }
        let Some(plan) = self.plan_drive(lua, from, at, false) else {
            info!("SpawnAmbush: no road from {source:?} to the intercept, ambush called off");
            return Ok(None);
        };
        if plan.route_m > AMBUSH_MAX_ROAD_M {
            info!(
                "SpawnAmbush: the road from {source:?} to the intercept is {:.0} km, too far, \
                 ambush called off",
                plan.route_m / 1000.
            );
            return Ok(None);
        }
        let gid = self.queue_drive_from_objective(
            spctx,
            idx,
            side,
            source,
            template,
            &plan,
            AMBUSH_SPEED_MPS,
            // Event-owned: a restart drops it.
            UnitTag::EventSpawn.into(),
        )?;
        info!(
            "SpawnAmbush: {gid:?} driving {:.1} km by road from {source:?} to its intercept",
            plan.route_m / 1000.
        );
        Ok(Some((gid, at)))
    }
}

/// Convert a 2D bearing (from → to) into an 8-point compass label.
pub(crate) fn bearing_to_compass(from: Vector2, to: Vector2) -> &'static str {
    let dx = to.x - from.x;
    let dy = to.y - from.y; // DCS: +Y is north
    // atan2(dy, dx) gives angle from east; convert to bearing from north, clockwise
    let angle_rad = dy.atan2(dx);
    let deg = (90.0 - angle_rad.to_degrees()).rem_euclid(360.0);
    match deg as u32 {
        0..=22   => "North",
        23..=67  => "Northeast",
        68..=112 => "East",
        113..=157 => "Southeast",
        158..=202 => "South",
        203..=247 => "Southwest",
        248..=292 => "West",
        293..=337 => "Northwest",
        _         => "North",
    }
}

/// Air-start watch state for one CAP flight. See `cap_spawn_ts`.
#[derive(Debug, Clone, Copy)]
pub struct CapSpawnWatch {
    pub spawned_at: DateTime<Utc>,
    /// Consecutive `enforce_cap_ground_start` observations where the lead unit
    /// was airborne and had *never* been seen on the ground. Two of these
    /// (or one within the fast-air-start window) scraps the event.
    pub airborne_strikes: u8,
}

impl CapSpawnWatch {
    pub fn new(now: DateTime<Utc>) -> Self {
        Self { spawned_at: now, airborne_strikes: 0 }
    }
}

/// Manages dynamic campaign events
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EventScheduler {
    pub active_events: Vec<CampaignEvent>,
    pub last_event_check: Option<DateTime<Utc>>,
    pub total_events_spawned: u64,
    /// Per-side last commander decision timestamp (smart commander).
    #[serde(default)]
    pub last_commander_check_blue: Option<DateTime<Utc>>,
    #[serde(default)]
    pub last_commander_check_red: Option<DateTime<Utc>>,
    /// Deferred move orders: GroupId → ordered waypoints (route).
    /// Retried each tick until the DCS group appears (spawn queue lag).
    #[serde(skip)]
    pub pending_moves: FxHashMap<GroupId, Vec<Vector2>>,
    /// CAP event → list of spawned group IDs (for cleanup on expiry).
    #[serde(skip)]
    pub cap_groups: FxHashMap<EventId, SmallVec<[GroupId; 2]>>,
    /// CAP event → which side owns it (needed for retargeting, to know who the enemy is).
    #[serde(skip)]
    pub cap_side_by_event: FxHashMap<EventId, Side>,
    /// Deferred CAP initial-task setup: GroupId → (spawn position, rotary).
    /// Used only once, until DCS reports the group as alive; after that,
    /// dynamic retargeting takes over. The flag rides along because the group
    /// is tasked before we look its event up again, and a helicopter patrol
    /// needs a different orbit block and target list than a CAP.
    #[serde(skip)]
    pub pending_cap_tasks: FxHashMap<GroupId, (Vector2, bool)>,
    /// CAP group → air-start watch state. A CAP is meant to ground-start (taxi
    /// + takeoff take a minute+). `enforce_cap_ground_start` polls the lead
    /// unit: the instant it is seen ON THE GROUND the watch is dropped (a real
    /// ground start); if it is only ever seen AIRBORNE it air-started (bad
    /// template / DCS ignored the parking start) and the whole event is
    /// despawned. Not wall-clock-boxed, so spawn-queue lag can't wave an
    /// air-started flight through.
    #[serde(skip)]
    pub cap_spawn_ts: FxHashMap<GroupId, CapSpawnWatch>,
    /// CAP event → the ground point its flights are currently stationed over.
    /// Used by retarget_cap_groups to avoid re-issuing an identical CAP-station
    /// task every slow tick (which would interrupt an in-progress intercept).
    #[serde(skip)]
    pub cap_station_by_event: FxHashMap<EventId, Vector2>,
    /// CAP event → last time its side's radar network still painted a threat it
    /// could work. Once a CAP has had nothing to do for `cap_idle_rtb_secs` it
    /// is sent home early instead of burning its full `cap_duration_secs`.
    #[serde(skip)]
    pub cap_last_threat_seen: FxHashMap<EventId, DateTime<Utc>>,
    /// (launch field, rotary) -> when that field's last wave of this class
    /// ended. The between-waves cooldown is enforced per FIELD, not per side:
    /// a wave ending on one flank must not ground the whole coalition.
    #[serde(skip)]
    pub cap_field_cooldown: FxHashMap<(ObjectiveId, bool), DateTime<Utc>>,
    /// Launch times of recent reactive scrambles, (side, rotary, when). Read
    /// as a rolling-hour window for the side's sortie budget and pruned to the
    /// last hour on every check, so it stays a handful of entries.
    #[serde(skip)]
    pub cap_sortie_log: Vec<(Side, bool, DateTime<Utc>)>,
    /// Flights that have been sent home and are still flying the approach.
    /// GroupId -> when the RTB order was issued. `flush_cap_rtb` deletes each
    /// one once it is down (or once it has had long enough to get there);
    /// before this existed every timed-out wave stayed parked at its field
    /// forever, alive and weapons-free, and persisted into the save.
    #[serde(skip)]
    pub cap_rtb: FxHashMap<GroupId, DateTime<Utc>>,
    /// CAP event -> the latest it may stay up no matter how much fighting it
    /// is doing. Set at spawn; bounds the in-contact extensions so a flight
    /// trading shots with a stream of players can't loiter indefinitely.
    #[serde(skip)]
    pub cap_hard_expiry: FxHashMap<EventId, DateTime<Utc>>,


    #[serde(skip)]
    pub ambush_groups: FxHashMap<EventId, GroupId>,
    #[serde(skip)]
    pub event_marks: FxHashMap<EventId, SmallVec<[MarkId; 4]>>,
    /// to avoid blocking DCS Lua for too long in a single frame.
    #[serde(skip)]
    pub pending_effects: std::collections::VecDeque<EventEffect>,
    /// Timestamp when the last commander-dispatched CAP for Blue ended (expired or
    /// all aircraft destroyed).  Used to enforce the post-expiry cooldown.
    #[serde(default)]
    pub last_commander_cap_ended_blue: Option<DateTime<Utc>>,
    /// Same as above for Red.
    #[serde(default)]
    pub last_commander_cap_ended_red: Option<DateTime<Utc>>,
    /// When Blue's last reactive HELICOPTER patrol ended (shot down or RTB'd).
    /// Tracked apart from the CAP cooldown so a jet wave and a helo wave don't
    /// gate each other -- they answer different threats and different players.
    #[serde(default)]
    pub last_helo_patrol_ended_blue: Option<DateTime<Utc>>,
    /// Same as above for Red.
    #[serde(default)]
    pub last_helo_patrol_ended_red: Option<DateTime<Utc>>,
    /// Timestamp of the first tick in this server session. Not persisted — resets on
    /// every restart so hvt_startup_delay_secs counts from each fresh session start.
    #[serde(skip)]
    pub session_start: Option<DateTime<Utc>>,
    /// Cached list of owned (non-neutral, non-special) objectives — rebuilt when dirty.
    /// Stored in Arc so callers can clone cheaply (atomic increment) without borrowing self.
    #[serde(skip)]
    cached_owned: Arc<Vec<(ObjectiveId, Side, Vector2, dcso3::String, u8)>>,
    /// Set to true whenever objective ownership or supply changes significantly.
    #[serde(skip)]
    pub owned_cache_dirty: bool,
}

impl EventScheduler {
    /// Maximum event effects applied per slow tick to avoid stalling DCS Lua.
    pub const EFFECTS_PER_TICK: usize = 2;

    /// Build the candidate objective list used by spawn functions.
    pub(crate) fn build_candidates(
        &mut self,
        db: &Db,
    ) -> Arc<Vec<(ObjectiveId, Side, Vector2, dcso3::String, u8)>> {
        if self.owned_cache_dirty || self.cached_owned.is_empty() {
            self.cached_owned = Arc::new(
                db.persisted
                    .objectives
                    .into_iter()
                    .filter(|(_, o)| {
                        o.owner() != Side::Neutral
                            && !o.kind().is_naval_base()
                            && !o.kind().is_carrier_group()
                            && !o.kind().is_special_sam_site()
                    })
                    .map(|(id, o)| (*id, o.owner(), o.pos(), dcso3::String::from(o.name.as_str()), o.supply()))
                    .collect(),
            );
            self.owned_cache_dirty = false;
        }
        Arc::clone(&self.cached_owned)
    }

    pub fn register_mark(&mut self, event_id: EventId, mark_id: MarkId) {
        self.event_marks.entry(event_id).or_default().push(mark_id);
    }


    // -------------------------------------------------------------------------
    // Main tick
    // -------------------------------------------------------------------------

    /// Main tick — returns (messages, effects). Caller must execute effects with lua access.
    pub fn tick(
        &mut self,
        db: &Db,
        _cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
    ) -> Result<(Vec<CompactString>, Vec<EventEffect>)> {
        // Record the first tick time so tick_events can enforce hvt_startup_delay_secs.
        self.session_start = self.session_start.or(Some(now));
        let mut messages: Vec<CompactString> = Vec::new();
        let mut effects: Vec<EventEffect> = Vec::new();



        // ---- Process active events ----
        let mut expired_indices: Vec<usize> = Vec::new();
        for (i, event) in self.active_events.iter_mut().enumerate() {
            match event {
                // -- Barrage --
                CampaignEvent::Barrage { id, side, source_objective, target_pos, expires_at, fire_ordered } => {
                    if !*fire_ordered {
                        *fire_ordered = true;
                        effects.push(EventEffect::FireBarrage {
                            event_id: *id,
                            side: *side,
                            source_objective: *source_objective,
                            target_pos: *target_pos,
                        });
                    }
                    if now >= *expires_at {
                        messages.push(format_compact!(
                            "INTEL: {:?} fire-support mission has ended",
                            side
                        ));
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        expired_indices.push(i);
                    }
                }

                // -- Missile strike --
                CampaignEvent::MissileStrike { id, side, shooter_gids, target_pos, expires_at, fire_ordered } => {
                    if !*fire_ordered {
                        *fire_ordered = true;
                        effects.push(EventEffect::FireMissileStrike {
                            event_id: *id,
                            side: *side,
                            shooter_gids: shooter_gids.clone(),
                            target_pos: *target_pos,
                        });
                    }
                    if now >= *expires_at {
                        messages.push(format_compact!(
                            "INTEL: {:?} missile strike mission has ended",
                            side
                        ));
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        expired_indices.push(i);
                    }
                }

                // -- Convoy ambush --
                CampaignEvent::ConvoyAmbush { id, ambush_side, spawn_pos, source_objective, expires_at, spawned, convoy_group_id, convoy_pos } => {
                    if !*spawned {
                        *spawned = true;
                        effects.push(EventEffect::SpawnAmbush {
                            event_id: *id,
                            ambush_side: *ambush_side,
                            spawn_pos: *spawn_pos,
                            source_objective: *source_objective,
                            convoy_group_id: *convoy_group_id,
                            convoy_pos: *convoy_pos,
                        });
                    }
                    if now >= *expires_at {
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        effects.push(EventEffect::DespawnAmbush { event_id: *id });
                        expired_indices.push(i);
                    }
                }

                CampaignEvent::EnemyCap { id, cap_side, objective, expires_at, spawned, rotary, threat } => {
                    if !*spawned {
                        *spawned = true;
                        let obj_pos = db.persisted.objectives.get(objective)
                            .map(|o| o.pos()).unwrap_or_default();
                        effects.push(EventEffect::SpawnCap {
                            event_id: *id,
                            cap_side: *cap_side,
                            objective: *objective,
                            obj_pos,
                            rotary: *rotary,
                            threat: *threat,
                        });
                    }
                    if now >= *expires_at {
                        effects.push(EventEffect::DespawnCap { event_id: *id, cap_side: *cap_side, objective: *objective, rotary: *rotary });
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        expired_indices.push(i);
                    }
                }

                CampaignEvent::CommanderCap { id, cap_side, objective, expires_at, spawned } => {
                    if !*spawned {
                        *spawned = true;
                        let obj_pos = db.persisted.objectives.get(objective)
                            .map(|o| o.pos()).unwrap_or_default();
                        effects.push(EventEffect::SpawnCap {
                            event_id: *id,
                            cap_side: *cap_side,
                            objective: *objective,
                            obj_pos,
                            rotary: false,
                            threat: 0,
                        });
                    }
                    if now >= *expires_at {
                        effects.push(EventEffect::DespawnCap { event_id: *id, cap_side: *cap_side, objective: *objective, rotary: false });
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        // Record when this commander CAP ended so the cooldown
                        // enforcer in commander.rs can gate the next dispatch.
                        match *cap_side {
                            Side::Blue => self.last_commander_cap_ended_blue = Some(now),
                            Side::Red => self.last_commander_cap_ended_red = Some(now),
                            Side::Neutral => {}
                        }
                        expired_indices.push(i);
                    }
                }
            }
        }
        for i in expired_indices.into_iter().rev() {
            let event = self.active_events.remove(i);
            info!("Campaign event expired: {}", event.description());
        }


        Ok((messages, effects))
    }

    // -------------------------------------------------------------------------
    // Event spawning
    // -------------------------------------------------------------------------

    // -- C: Artillery/armor barrage --

    /// Spawn a missile-strike event using the pre-selected shooters and target chosen by
    /// the Smart Commander. All target selection and range checking has already been done.
    pub(crate) fn spawn_missile_strike_event(
        &mut self,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        side: Side,
        shooter_gids: SmallVec<[bfprotocols::db::group::GroupId; 4]>,
        target_pos: Vector2,
        target_name: CompactString,
        messages: &mut Vec<CompactString>,
    ) {
        let id = EventId::new();
        let event = CampaignEvent::MissileStrike {
            id,
            side,
            shooter_gids,
            target_pos,
            expires_at: now + chrono::Duration::seconds(cfg.barrage_duration_secs as i64),
            fire_ordered: false,
        };

        messages.push(format_compact!(
            "INTEL: {:?} missile strike inbound — {} is the target!",
            side, target_name
        ));

        self.total_events_spawned += 1;
        info!("Spawned missile strike by {:?} at {}", side, target_name);
        self.active_events.push(event);
    }

    /// Spawn a barrage event using the pre-selected (src, target) pair chosen by
    /// the Smart Commander. All target selection and range checking has already been
    /// done by `commander::score_actions`; this function only records the event.
    pub(crate) fn spawn_barrage_event(
        &mut self,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        src_oid: ObjectiveId,
        src_side: Side,
        target_oid: ObjectiveId,
        target_pos: Vector2,
        target_name: CompactString,
        messages: &mut Vec<CompactString>,
    ) {
        let id = EventId::new();
        let event = CampaignEvent::Barrage {
            id,
            side: src_side,
            source_objective: src_oid,
            target_pos,
            expires_at: now + chrono::Duration::seconds(cfg.barrage_duration_secs as i64),
            fire_ordered: false,
        };

        messages.push(format_compact!(
            "INTEL: {:?} fire-support mission in progress — {} is under artillery fire! ({} min)",
            src_side, target_name, cfg.barrage_duration_secs / 60
        ));

        self.total_events_spawned += 1;
        info!("Spawned barrage by {:?} at {:?} → {:?}", src_side, src_oid, target_oid);
        self.active_events.push(event);
    }

    // -- D: Convoy ambush --

    /// Set an ambush for `ambush_side` on one of the ENEMY's convoys. Returns
    /// false, creating nothing, when there is no enemy convoy or no friendly
    /// objective to draw the ambush force from -- the caller only pays for
    /// an event that was actually created.
    ///
    /// The convoy used to be drawn from every active convoy on the map, so
    /// the side that paid for the ambush could end up with it set by -- and
    /// against -- the other team.
    pub(crate) fn spawn_convoy_ambush(
        &mut self,
        db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        ambush_side: Side,
        all_owned: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        _messages: &mut Vec<CompactString>,
        _effects: &mut Vec<EventEffect>,
    ) -> bool {
        let mut rng = rand::thread_rng();

        let target_side = match ambush_side {
            Side::Red => Side::Blue,
            Side::Blue => Side::Red,
            Side::Neutral => return false,
        };
        // The ambush force drives out of one of our own objectives to a point
        // on the convoy's way that it can reach first. Only convoys some
        // objective of ours can get ahead of within driving range qualify;
        // the road check itself needs Lua and happens at spawn time
        // (`Db::launch_ambush`).
        let ours: SmallVec<[(ObjectiveId, Vector2); 32]> = all_owned
            .iter()
            .filter(|(_, s, ..)| *s == ambush_side)
            .map(|(oid, _, pos, _, _)| (*oid, *pos))
            .collect();
        let mut options: SmallVec<[(usize, ObjectiveId, Vector2, f64); 8]> = SmallVec::new();
        let convoys: Vec<_> = db
            .ephemeral
            .active_convoys
            .values()
            .filter(|c| c.side == target_side)
            .collect();
        for (i, convoy) in convoys.iter().enumerate() {
            let Some(dest) = db.persisted.objectives.get(&convoy.destination).map(|o| o.pos())
            else {
                continue;
            };
            let path = [convoy.last_pos, dest];
            if let Some((oid, at, drive_m)) = best_ambush_source(&path, convoy_speed(convoy.speed_mps), &ours) {
                options.push((i, oid, at, drive_m));
            }
        }
        if options.is_empty() { return false; }
        let (i, source_objective, spawn_pos, drive_m) = options[rng.r#gen_range(0..options.len())];
        let convoy = convoys[i];

        let convoy_group_id = convoy.group_id;
        let convoy_pos = convoy.last_pos;
        let id = EventId::new();
        // The clock starts when the force is in place, not when it leaves.
        let drive_secs = (drive_m / AMBUSH_SPEED_MPS) as i64;
        let event = CampaignEvent::ConvoyAmbush {
            id,
            ambush_side,
            spawn_pos,
            source_objective,
            expires_at: now
                + chrono::Duration::seconds(drive_secs + cfg.ambush_duration_secs as i64),
            spawned: false,
            convoy_group_id,
            convoy_pos,
        };


        self.total_events_spawned += 1;
        info!(
            "Spawned convoy ambush by {:?} on convoy {:?}: force from {:?}, {:.1} km drive to the \
             intercept",
            ambush_side,
            convoy.id,
            source_objective,
            drive_m / 1000.
        );
        self.active_events.push(event);
        true
    }

    /// Spawn a commander-dispatched CAP flight for `cap_side` orbiting its best
    /// defended objective.  Picks the most threatened owned objective that isn't
    /// already covered by an active CAP. Returns false, creating nothing, when
    /// every friendly airbase already has a CAP over it.
    pub(crate) fn spawn_commander_cap(
        &mut self,
        db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        cap_side: Side,
        _messages: &mut Vec<CompactString>,
    ) -> bool {
        // Find the most threatened owned objective without existing CAP coverage.
        let active_cap_objs: Vec<ObjectiveId> = self
            .active_events
            .iter()
            .filter_map(|e| match e {
                CampaignEvent::EnemyCap { objective, .. }
                | CampaignEvent::CommanderCap { objective, .. } => Some(*objective),
                _ => None,
            })
            .collect();

        let obj_score = |obj: &crate::db::objective::Objective| -> u8 {
            let mut s = 0u8;
            if obj.threatened() { s += 2; }
            if obj.captureable() { s += 4; }
            s
        };
        // Find the most threatened objective to act as our "threat center"
        let threat_center = db
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, obj)| obj.owner() == cap_side)
            .max_by(|(_, a), (_, b)| obj_score(a).cmp(&obj_score(b)))
            .map(|(_, obj)| obj.pos())
            .unwrap_or_else(|| Vector2::new(0., 0.));

        // Scramble from the friendly airbase *nearest* the threatened sector --
        // that's where the fight is, and it's what gets CAP onto a northern
        // front instead of leaving every flight orbiting a rear base in the
        // south. Bases already covered by an active CAP event are skipped so
        // multiple fronts each get their own flight.
        let best = db
            .persisted
            .objectives
            .into_iter()
            .filter(|(oid, obj)| {
                obj.owner() == cap_side
                    && obj.is_airbase()
                    && !active_cap_objs.contains(oid)
            })
            .min_by(|(_, a), (_, b)| {
                let da = na::distance_squared(&a.pos().into(), &threat_center.into());
                let db = na::distance_squared(&b.pos().into(), &threat_center.into());
                da.partial_cmp(&db).unwrap_or(std::cmp::Ordering::Equal)
            });

        let (objective, obj) = match best {
            Some(pair) => pair,
            None => return false,
        };

        let _obj_pos = obj.pos();
        let obj_name = dcso3::String::from(obj.name.as_str());
        let id = EventId::new();
        let event = CampaignEvent::CommanderCap {
            id,
            cap_side,
            objective: *objective,
            expires_at: now + chrono::Duration::seconds(cfg.cap_duration_secs as i64),
            spawned: false,
        };


        self.total_events_spawned += 1;
        info!("[Commander] Dispatched {:?} CAP over {}", cap_side, obj_name);
        self.active_events.push(event);
        true
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

}

#[cfg(test)]
mod tests {
    use super::*;

    // A convoy 60 km from home, east along y = 0 at 8 m/s.
    fn road() -> [Vector2; 2] {
        [Vector2::new(0., 0.), Vector2::new(60_000., 0.)]
    }

    #[test]
    fn ambush_sets_up_ahead_of_the_convoy() {
        let from = Vector2::new(20_000., 10_000.);
        let (at, drive) = intercept_on_path(&road(), 8., from).expect("reachable");
        // On the road, ahead of the convoy, and reached first.
        assert!(at.y.abs() < 1e-6 && at.x >= AMBUSH_MIN_LEAD_M);
        assert!(drive / AMBUSH_SPEED_MPS + AMBUSH_SETUP_SECS <= at.x / 8.);
        // Not past the closest stretch of road: it takes the first point it can make.
        assert!(at.x <= 20_000.);
    }

    #[test]
    fn no_ambush_out_of_range_or_behind() {
        // 50 km off the road: too far to drive.
        assert!(intercept_on_path(&road(), 8., Vector2::new(30_000., 50_000.)).is_none());
        // Behind the convoy and slower than it can't catch up anywhere.
        assert!(intercept_on_path(&road(), 30., Vector2::new(-30_000., 0.)).is_none());
    }

    #[test]
    fn nearest_source_wins() {
        let ours = [(1, Vector2::new(40_000., 30_000.)), (2, Vector2::new(30_000., 5_000.))];
        let (id, _, _) = best_ambush_source(&road(), 8., &ours).expect("someone can reach it");
        assert_eq!(id, 2);
    }
}

