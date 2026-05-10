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
use bfprotocols::{cfg::CampaignEventsCfg, db::group::GroupId};
use chrono::{DateTime, Utc};
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, trigger::MarkId, Vector2};
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
    HighValueTarget {
        id: EventId,
        objective: ObjectiveId,
        side: Side,
        expires_at: DateTime<Utc>,
        reward_points: i32,
        /// true until the HVT unit has been spawned (first tick after creation)
        #[serde(default)]
        announced: bool,
        /// Actual world-space spawn position (offset from objective into a nearby town/area).
        /// None on older saved states — falls back to objective position.
        #[serde(default)]
        spawn_pos: Option<Vector2>,
        /// GroupId of the spawned HVT unit — persisted so we can despawn it after a restart.
        #[serde(default)]
        spawned_gid: Option<GroupId>,
    },
    ReinforcementWave {
        id: EventId,
        side: Side,
        objective: ObjectiveId,
        arrival_time: DateTime<Utc>,
        /// Source friendly objective position where units will be spawned to march from.
        /// None on older saved states — falls back to spawning at the destination.
        #[serde(default)]
        source_pos: Option<Vector2>,
    },
    CounterOffensive {
        id: EventId,
        attacking_side: Side,
        target_objectives: SmallVec<[ObjectiveId; 4]>,
        started_at: DateTime<Utc>,
        duration_secs: u32,
        #[serde(default)]
        orders_issued: bool,
    },
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
        /// Position near the convoy where the ambush spawns.
        spawn_pos: Vector2,
        /// Friendly objective closest to the ambush (used for template lookup).
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
            Self::HighValueTarget { id, .. } => *id,
            Self::ReinforcementWave { id, .. } => *id,
            Self::CounterOffensive { id, .. } => *id,
            Self::Barrage { id, .. } => *id,
            Self::MissileStrike { id, .. } => *id,
            Self::ConvoyAmbush { id, .. } => *id,
            Self::EnemyCap { id, .. } => *id,
            Self::CommanderCap { id, .. } => *id,
        }
    }

    pub fn description(&self) -> CompactString {
        match self {
            Self::HighValueTarget { .. } => format_compact!("High-Value Target"),
            Self::ReinforcementWave { side, .. } => format_compact!("{:?} Reinforcement Wave", side),
            Self::CounterOffensive { attacking_side, .. } => {
                format_compact!("{:?} Counter-Offensive", attacking_side)
            }
            Self::Barrage { side, .. } => format_compact!("{:?} Barrage", side),
            Self::MissileStrike { side, .. } => format_compact!("{:?} Missile Strike", side),
            Self::ConvoyAmbush { ambush_side, .. } => format_compact!("{:?} Convoy Ambush", ambush_side),
            Self::EnemyCap { cap_side, .. } => format_compact!("{:?} Enemy CAP", cap_side),
            Self::CommanderCap { cap_side, .. } => format_compact!("{:?} Commander CAP", cap_side),
        }
    }
}

/// DCS-side effects that need to be executed after tick() returns.
#[derive(Debug, Clone)]
pub enum EventEffect {
    /// Place an "inbound" map mark for a reinforcement wave before it arrives.
    MarkInbound {
        event_id: EventId,
        side: Side,
        obj_pos: Vector2,
        obj_name: CompactString,
    },
    /// Spawn reinforcement groups for `side` at `objective` (inbound mark already deleted).
    SpawnReinforcements {
        event_id: EventId,
        side: Side,
        objective: ObjectiveId,
        obj_pos: Vector2,
    },
    /// Order existing groups of `attacking_side` toward targets and place map marks.
    OrderAttack {
        event_id: EventId,
        attacking_side: Side,
        target_positions: SmallVec<[Vector2; 4]>,
    },
    /// Spawn an HVT group, place a map mark, then issue an escape-move order.
    SpawnHvt {
        event_id: EventId,
        side: Side,
        objective: ObjectiveId,
        obj_pos: Vector2,
        reward_points: i32,
        /// Dedicated HVT template name from config.
        template: dcso3::String,
        /// F10 circle radius in metres.
        circle_radius_m: f64,
        /// Chain of friendly objective positions for the HVT to flee through (deep into own territory).
        escape_route: Vec<Vector2>,
    },
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
    /// Spawn an ambush force for `ambush_side` near `spawn_pos`.
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
    },
    /// Despawn all groups registered to a CAP event.
    DespawnCap {
        event_id: EventId,
    },
    /// Despawn the HVT group when the event expires (it "escaped" but must be cleaned up).
    DespawnHvt {
        event_id: EventId,
        /// GroupId of the HVT unit to despawn. Taken from the persisted event so it
        /// survives server restarts (hvt_group_by_event is serde(skip)).
        gid: Option<GroupId>,
    },
    /// Despawn all groups registered to a convoy-ambush event.
    DespawnAmbush {
        event_id: EventId,
    },

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
    /// Pending escalation entries: (attacking_side, trigger_at).
    /// When trigger_at is reached a counter-offensive is spawned for that side.
    #[serde(default)]
    pub escalation_queue: Vec<(Side, DateTime<Utc>)>,
    /// F10 mark IDs per event — not persisted, re-created when event next fires.
    #[serde(skip)]
    pub event_marks: FxHashMap<EventId, SmallVec<[MarkId; 4]>>,
    /// Maps HVT group ID → (event_id, reward_points) for kill detection.
    #[serde(skip)]
    pub hvt_groups: FxHashMap<GroupId, (EventId, i32)>,
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
    /// Deferred CAP initial-task setup: GroupId → spawn position (used only
    /// once, until DCS reports the group as alive; after that, dynamic
    /// retargeting takes over).
    #[serde(skip)]
    pub pending_cap_tasks: FxHashMap<GroupId, Vector2>,
    /// Tracks when each reactive CAP flight was destroyed (keyed by (side, objective)).
    /// Used to enforce a respawn cooldown: if all aircraft in a reactive CAP are shot
    /// down, the defending side cannot spawn another CAP for that objective until the
    /// cooldown (cfg.cap_respawn_cooldown_secs) expires.
    #[serde(skip)]
    pub last_reactive_cap_died_at: FxHashMap<(Side, ObjectiveId), DateTime<Utc>>,
    /// Reactive CAP event → anchor objective (for death-time tracking).
    /// When the EnemyCap event expires via DespawnCap, we look up the objective here
    /// to know which cooldown bucket to update.
    #[serde(skip)]
    pub reactive_cap_objective: FxHashMap<EventId, (Side, ObjectiveId)>,
    #[serde(skip)]
    pub ambush_groups: FxHashMap<EventId, GroupId>,
    /// HVT event → spawned group ID (for cleanup on expiry).
    #[serde(skip)]
    pub hvt_group_by_event: FxHashMap<EventId, GroupId>,
    /// Deferred event effects — drained at most `EFFECTS_PER_TICK` per slow tick
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

    /// Schedule a revenge counter-offensive for `side` to trigger after a delay.
    pub fn schedule_revenge(&mut self, side: Side, trigger_at: DateTime<Utc>) {
        self.escalation_queue.push((side, trigger_at));
        info!("Revenge counter-offensive queued for {:?} at {:?}", side, trigger_at);
    }

    // -------------------------------------------------------------------------
    // Main tick
    // -------------------------------------------------------------------------

    /// Main tick — returns (messages, effects). Caller must execute effects with lua access.
    pub fn tick(
        &mut self,
        db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
    ) -> Result<(Vec<CompactString>, Vec<EventEffect>)> {
        // Record the first tick time so tick_events can enforce hvt_startup_delay_secs.
        self.session_start = self.session_start.or(Some(now));
        let mut messages: Vec<CompactString> = Vec::new();
        let mut effects: Vec<EventEffect> = Vec::new();

        // ---- Build candidate lists used by multiple paths ----
        // build_candidates returns a cached Arc — O(1) if objectives haven't changed.
        let all_owned = self.build_candidates(db);

        // ---- Process active events ----
        let mut expired_indices: Vec<usize> = Vec::new();
        for (i, event) in self.active_events.iter_mut().enumerate() {
            match event {
                // -- Reinforcement wave --
                CampaignEvent::ReinforcementWave { id, side, objective, arrival_time, source_pos: _ } => {
                    if now >= *arrival_time {
                        info!("ReinforcementWave arriving for {:?} at {:?}", side, objective);
                        let obj = db.persisted.objectives.get(objective);
                        let obj_pos = obj.map(|o| o.pos()).unwrap_or_default();
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        effects.push(EventEffect::SpawnReinforcements {
                            event_id: *id,
                            side: *side,
                            objective: *objective,
                            obj_pos,
                        });
                        // Escalation: a fresh wave creates pressure → counter-offensive after delay
                        if cfg.escalation_enabled {
                            self.escalation_queue.push((
                                *side,
                                now + chrono::Duration::seconds(cfg.escalation_delay_secs as i64),
                            ));
                        }
                        expired_indices.push(i);
                    }
                }

                // -- Counter-offensive --
                CampaignEvent::CounterOffensive {
                    id, attacking_side, target_objectives, started_at, duration_secs, orders_issued,
                } => {
                    if !*orders_issued {
                        *orders_issued = true;
                        let target_positions: SmallVec<[Vector2; 4]> = target_objectives
                            .iter()
                            .filter_map(|oid| db.persisted.objectives.get(oid).map(|o| o.pos()))
                            .collect();
                        effects.push(EventEffect::OrderAttack {
                            event_id: *id,
                            attacking_side: *attacking_side,
                            target_positions,
                        });
                    }
                    let end = *started_at + chrono::Duration::seconds(*duration_secs as i64);
                    if now >= end {
                        messages.push(format_compact!(
                            "INTEL: {:?} counter-offensive has ended",
                            attacking_side
                        ));
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        expired_indices.push(i);
                    }
                }

                // -- High-value target --
                CampaignEvent::HighValueTarget {
                    id, side, objective, expires_at, reward_points, announced, spawn_pos, spawned_gid,
                } => {
                    if now >= *expires_at {
                        messages.push(format_compact!(
                            "INTEL: High-value target in the area of {} has escaped!",
                            db.persisted.objectives.get(objective)
                                .map(|o| o.name.as_str()).unwrap_or("unknown")
                        ));
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        self.hvt_groups.retain(|_, (eid, _)| *eid != *id);
                        // Prefer the persisted gid (survives restarts) then fall back to the
                        // in-session map which is populated the same tick the group spawned.
                        let gid = spawned_gid.or_else(|| self.hvt_group_by_event.get(id).copied());
                        effects.push(EventEffect::DespawnHvt { event_id: *id, gid });
                        expired_indices.push(i);
                    } else if *announced {
                        let obj_pos = db.persisted.objectives.get(objective)
                            .map(|o| o.pos()).unwrap_or_default();
                        // Spawn at the objective position itself (players must intercept en route)
                        let actual_spawn_pos = spawn_pos.unwrap_or(obj_pos);
                        // Build a multi-hop escape route through friendly objectives deeper into own territory
                        let escape_route = build_hvt_escape_route(db, *side, actual_spawn_pos, *objective, 3);
                        let template = match *side {
                            Side::Red => cfg.hvt_template_red.clone(),
                            Side::Blue => cfg.hvt_template_blue.clone(),
                            Side::Neutral => cfg.hvt_template_red.clone(),
                        };
                        effects.push(EventEffect::SpawnHvt {
                            event_id: *id,
                            side: *side,
                            objective: *objective,
                            obj_pos: actual_spawn_pos,
                            reward_points: *reward_points,
                            template,
                            circle_radius_m: cfg.hvt_circle_radius_m,
                            escape_route,
                        });
                        *announced = false;
                    }
                }

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

                CampaignEvent::EnemyCap { id, cap_side, objective, expires_at, spawned } => {
                    if !*spawned {
                        *spawned = true;
                        let obj_pos = db.persisted.objectives.get(objective)
                            .map(|o| o.pos()).unwrap_or_default();
                        effects.push(EventEffect::SpawnCap {
                            event_id: *id,
                            cap_side: *cap_side,
                            objective: *objective,
                            obj_pos,
                        });
                    }
                    if now >= *expires_at {
                        effects.push(EventEffect::DespawnCap { event_id: *id });
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
                        });
                    }
                    if now >= *expires_at {
                        effects.push(EventEffect::DespawnCap { event_id: *id });
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

        // ---- Process escalation queue ----
        let mut to_escalate: Vec<Side> = Vec::new();
        self.escalation_queue.retain(|(side, trigger_at)| {
            if now >= *trigger_at {
                to_escalate.push(*side);
                false
            } else {
                true
            }
        });
        for attacking_side in to_escalate {
            if self.active_events.len() < cfg.max_concurrent_events as usize {
                self.spawn_counter_offensive(db, cfg, now, attacking_side, &all_owned, &mut messages);
            }
        }

        Ok((messages, effects))
    }

    // -------------------------------------------------------------------------
    // Event spawning
    // -- HVT --

    pub(crate) fn spawn_hvt_event(
        &mut self,
        db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        candidates: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        messages: &mut Vec<CompactString>,
        _effects: &mut Vec<EventEffect>,
    ) {
        let mut rng = rand::thread_rng();

        // Pre-collect all non-neutral objective positions once — avoids re-traversing the
        // tree once per candidate (O(n²) → O(n + c*n) with cache-local Vec scan).
        let enemy_obj_positions: SmallVec<[(Side, Vector2); 32]> = db.persisted.objectives
            .into_iter()
            .filter(|(_, o)| o.owner() != Side::Neutral)
            .map(|(_, o)| (o.owner(), o.pos()))
            .collect();

        // Score each candidate by proximity to nearest enemy objective.
        // Frontline objectives (closest to the enemy) make the most compelling HVT targets.
        let scored: Vec<(usize, f64)> = candidates.iter().enumerate().map(|(i, (_, side, pos, _, supply))| {
            let nearest_enemy_dist = enemy_obj_positions.iter()
                .filter(|(owner, _)| owner != side)
                .map(|(_, ep)| na::distance(&(*pos).into(), &(*ep).into()))
                .fold(f64::MAX, f64::min);
            // Low-supply objectives on the frontline score highest
            let supply_factor = 1.0 - (*supply as f64 / 100.0).min(1.0);
            let proximity_score = if nearest_enemy_dist < 1.0 { 1e9 } else { 1.0 / nearest_enemy_dist };
            let score = proximity_score * (1.0 + supply_factor);
            (i, score)
        }).collect();

        // Pick the highest-scoring candidate (with small random tie-breaking noise).
        let idx = scored.iter()
            .max_by(|a, b| {
                let a_s = a.1 + rng.r#gen::<f64>() * 0.05;
                let b_s = b.1 + rng.r#gen::<f64>() * 0.05;
                a_s.partial_cmp(&b_s).unwrap_or(std::cmp::Ordering::Equal)
            })
            .map(|(i, _)| *i)
            .unwrap_or(0);

        let (oid, side, obj_pos, ref name, _supply) = candidates[idx];

        // Spawn HVT at the objective itself — it will drive through subsequent objectives
        // toward the rear, giving players a chance to intercept along the route.
        let event = CampaignEvent::HighValueTarget {
            id: EventId::new(),
            objective: oid,
            side,
            expires_at: now + chrono::Duration::seconds(cfg.hvt_duration_secs as i64),
            reward_points: cfg.hvt_reward_points,
            announced: true,
            spawn_pos: Some(obj_pos),
            spawned_gid: None,
        };

        messages.push(format_compact!(
            "INTEL: High-value target detected in the area of {}! Destroy or capture within {} minutes for {} bonus points. It is attempting to evacuate!",
            name, cfg.hvt_duration_secs / 60, cfg.hvt_reward_points
        ));

        self.total_events_spawned += 1;
        info!("Spawned HVT event near objective {:?}", oid);
        self.active_events.push(event);
    }

    // -- Reinforcement wave (F: supply-pressure weighted) --

    pub(crate) fn spawn_reinforcement_wave(
        &mut self,
        db: &Db,
        _cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        red_count: usize,
        blue_count: usize,
        candidates: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        _messages: &mut Vec<CompactString>,
        effects: &mut Vec<EventEffect>,
    ) {
        let mut rng = rand::thread_rng();

        // Pre-compute the set of objectives currently being captured (enemy troops in zone).
        let being_captured: std::collections::HashSet<ObjectiveId> = db
            .ephemeral
            .capture_progress
            .iter()
            .map(|(oid, _)| *oid)
            .collect();

        // Bias toward the losing side, low-supply objectives, and — most importantly —
        // any objective that is currently under attack or being captured.
        let total_count = (red_count + blue_count).max(1);
        let red_share = red_count as f64 / total_count as f64;
        let blue_share = blue_count as f64 / total_count as f64;

        let weighted: Vec<(u32, usize)> = candidates
            .iter()
            .enumerate()
            .map(|(i, (oid, side, _, _, supply))| {
                // Losing-side bonus
                let side_bonus = match side {
                    Side::Red => ((1.0 - red_share) * 10.0) as u32 + 1,
                    Side::Blue => ((1.0 - blue_share) * 10.0) as u32 + 1,
                    Side::Neutral => 0,
                };
                // Supply pressure: low supply = high priority
                let supply_bonus = match supply {
                    0..=24 => 8,
                    25..=49 => 4,
                    50..=74 => 2,
                    _ => 1,
                };
                // Threat multiplier: massively prioritise objectives under active attack.
                // "being_captured" means enemy troops are in the zone right now (highest
                // urgency); "threatened" means enemy units are nearby.
                let threat_mul = {
                    let obj = db.persisted.objectives.get(oid);
                    let is_captured = being_captured.contains(oid);
                    let is_threatened = obj.map(|o| o.threatened()).unwrap_or(false);
                    if is_captured { 50u32 }
                    else if is_threatened { 10u32 }
                    else { 1u32 }
                };
                (side_bonus * supply_bonus * threat_mul, i)
            })
            .filter(|(w, _)| *w > 0)
            .collect();

        if weighted.is_empty() {
            return;
        }

        let total_w: u32 = weighted.iter().map(|(w, _)| w).sum();
        let mut roll = rng.r#gen_range(0..total_w);
        let mut chosen_idx = 0;
        for (w, i) in &weighted {
            if roll < *w {
                chosen_idx = *i;
                break;
            }
            roll -= w;
        }

        let (oid, side, obj_pos, ref obj_name, supply) = candidates[chosen_idx];

        // Don't stack multiple pending waves for the same side
        let already_pending = self.active_events.iter().any(|e| matches!(
            e, CampaignEvent::ReinforcementWave { side: s, .. } if *s == side
        ));
        if already_pending {
            return;
        }

        // Source objective is resolved at spawn time (inside EventEffect::SpawnReinforcements)
        // so we have the freshest objective ownership data.  The field is kept in the event
        // struct for backwards-compat with saved campaigns that stored it.
        let arrival_delay = rng.r#gen_range(30..120i64); // 30s–2min so it's visible in testing
        let id = EventId::new();
        let event = CampaignEvent::ReinforcementWave {
            id,
            side,
            objective: oid,
            arrival_time: now + chrono::Duration::seconds(arrival_delay),
            source_pos: None,
        };

        effects.push(EventEffect::MarkInbound {
            event_id: id,
            side,
            obj_pos,
            obj_name: format_compact!("{}", obj_name),
        });

        self.total_events_spawned += 1;
        info!("Spawned reinforcement wave for {:?} at {:?} (supply {}%)", side, oid, supply);
        self.active_events.push(event);
    }

    // -- Counter-offensive --

    /// Spawn a counter-offensive for `attacking_side` (pre-chosen by the Smart Commander).
    /// Targets are selected by opportunity score: weakest health + lowest supply + strategic
    /// value, closest to the attacker's front. Up to 3 targets are picked simultaneously.
    pub(crate) fn spawn_counter_offensive(
        &mut self,
        db: &Db,
        _cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        attacking_side: Side,
        all_owned: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        messages: &mut Vec<CompactString>,
    ) {
        use super::objective::ObjGroupClass;
        let mut rng = rand::thread_rng();

        let enemy_side = match attacking_side {
            Side::Red => Side::Blue,
            Side::Blue => Side::Red,
            Side::Neutral => return,
        };

        // Compute centroid of attacking side's objectives for proximity scoring.
        let attacker_positions: Vec<Vector2> = all_owned
            .iter()
            .filter(|(_, s, ..)| *s == attacking_side)
            .map(|(_, _, pos, ..)| *pos)
            .collect();
        let attacker_centroid = if attacker_positions.is_empty() {
            return;
        } else {
            let n = attacker_positions.len() as f64;
            attacker_positions.iter().fold(Vector2::zeros(), |a, p| a + p) / n
        };

        // Score every enemy objective: weak health + low supply + strategic + proximity.
        let mut scored: Vec<(f64, ObjectiveId, Vector2)> = all_owned
            .iter()
            .filter(|(_, s, ..)| *s == enemy_side)
            .filter_map(|(oid, _, pos, _, supply)| {
                let obj = db.persisted.objectives.get(oid)?;
                let health = obj.health() as f64;
                let capturable = obj.captureable();
                let is_logi = matches!(obj.kind(), bfprotocols::db::objective::ObjectiveKind::Logistics);
                let is_factory = matches!(obj.kind(), bfprotocols::db::objective::ObjectiveKind::Factory { .. });

                // Fewer defenders = better target.
                let has_defenders = obj.groups().get(&enemy_side).map(|gids| {
                    gids.into_iter().any(|gid| {
                        db.persisted.groups.get(gid).map(|g| {
                            matches!(g.class, ObjGroupClass::Armor | ObjGroupClass::Sr | ObjGroupClass::Mr | ObjGroupClass::Lr)
                                && g.units.into_iter().any(|uid| {
                                    db.persisted.units.get(uid).map(|u| !u.dead).unwrap_or(false)
                                })
                        }).unwrap_or(false)
                    })
                }).unwrap_or(false);

                let weakness = (100.0 - health) * 0.8 + (100u8.saturating_sub(*supply)) as f64 * 0.5;
                let strategic = if is_logi { 1.8 } else if is_factory { 1.4 } else { 1.0 };
                let capturable_bonus = if capturable { 50.0 } else { 0.0 };
                let defender_penalty = if has_defenders { 0.0 } else { 20.0 };
                let dist = na::distance(&attacker_centroid.into(), &(*pos).into());
                // Prefer closer targets (proximity bonus up to 30 pts within 20 km).
                let proximity = (20_000.0_f64 - dist.min(20_000.0)) / 20_000.0 * 30.0;

                let score = (weakness + capturable_bonus + defender_penalty + proximity) * strategic;
                Some((score, *oid, *pos))
            })
            .collect();

        if scored.is_empty() { return; }

        // Sort descending by score; pick up to 3 targets.
        scored.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
        let count = rng.r#gen_range(1..=3usize).min(scored.len());
        let targets: SmallVec<[ObjectiveId; 4]> = scored.iter().take(count).map(|(_, oid, _)| *oid).collect();
        let target_names: Vec<&str> = scored.iter().take(count).filter_map(|(_, oid, _)| {
            db.persisted.objectives.get(oid).map(|o| o.name())
        }).collect();

        let duration = rng.r#gen_range(600i64..=1800);
        let event = CampaignEvent::CounterOffensive {
            id: EventId::new(),
            attacking_side,
            target_objectives: targets,
            started_at: now,
            duration_secs: duration as u32,
            orders_issued: false,
        };

        let target_list = target_names.join(", ");
        messages.push(format_compact!(
            "WARNING: {:?} forces launching counter-offensive against {}! Duration: {} minutes",
            attacking_side, target_list, duration / 60
        ));

        self.total_events_spawned += 1;
        info!("Spawned counter-offensive by {:?} targeting [{}]", attacking_side, target_list);
        self.active_events.push(event);
    }

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

    pub(crate) fn spawn_convoy_ambush(
        &mut self,
        db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        all_owned: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        _messages: &mut Vec<CompactString>,
        _effects: &mut Vec<EventEffect>,
    ) {
        let mut rng = rand::thread_rng();

        let convoys: Vec<_> = db.ephemeral.active_convoys.values().collect();
        if convoys.is_empty() { return; }

        let convoy = &convoys[rng.r#gen_range(0..convoys.len())];
        let ambush_side = match convoy.side {
            Side::Red => Side::Blue,
            Side::Blue => Side::Red,
            Side::Neutral => return,
        };

        // Find a friendly objective on the ambush side to pull the template from
        let source_objective = all_owned.iter()
            .filter(|(_, s, ..)| *s == ambush_side)
            .map(|(oid, _, pos, _, _)| {
                let d = na::distance(&(*pos).into(), &convoy.last_pos.into());
                (d, *oid)
            })
            .min_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .map(|(_, oid)| oid);

        let source_objective = match source_objective {
            Some(o) => o,
            None => return,
        };

        // Spawn point: near the convoy's last known position with small random offset
        let offset = dcso3::Vector2::new(
            rng.r#gen_range(-500.0..500.0f64),
            rng.r#gen_range(-500.0..500.0f64),
        );
        let spawn_pos = convoy.last_pos + offset;

        let convoy_group_id = convoy.group_id;
        let convoy_pos = convoy.last_pos;
        let id = EventId::new();
        let event = CampaignEvent::ConvoyAmbush {
            id,
            ambush_side,
            spawn_pos,
            source_objective,
            expires_at: now + chrono::Duration::seconds(cfg.ambush_duration_secs as i64),
            spawned: false,
            convoy_group_id,
            convoy_pos,
        };


        self.total_events_spawned += 1;
        info!("Spawned convoy ambush by {:?} near convoy {:?}", ambush_side, convoy.id);
        self.active_events.push(event);
    }

    /// Spawn a commander-dispatched CAP flight for `cap_side` orbiting its best
    /// defended objective.  Picks the most threatened owned objective that isn't
    /// already covered by an active CAP.
    pub(crate) fn spawn_commander_cap(
        &mut self,
        db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        cap_side: Side,
        _messages: &mut Vec<CompactString>,
    ) {
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
        let best = db
            .persisted
            .objectives
            .into_iter()
            .filter(|(oid, obj)| {
                obj.owner() == cap_side
                    && !obj.kind().is_naval_base()
                    && !obj.kind().is_carrier_group()
                    && !active_cap_objs.contains(oid)
            })
            .max_by(|(_, a), (_, b)| obj_score(a).cmp(&obj_score(b)));

        let (objective, obj) = match best {
            Some(pair) => pair,
            None => return,
        };

        let obj_pos = obj.pos();
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
        let _ = obj_pos; // position used by SpawnCap effect via EnemyCap path
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

}

/// Find the nearest friendly objective to `pos` owned by `side`, excluding any in `exclude`.
/// Returns `(ObjectiveId, position)` so callers can chain hops.
pub fn find_nearest_friendly_objective(
    db: &Db,
    side: Side,
    pos: Vector2,
    exclude: Option<ObjectiveId>,
) -> Option<Vector2> {
    find_nearest_friendly_objective_id(db, side, pos, &exclude.map(|e| vec![e]).unwrap_or_default())
        .map(|(_, p)| p)
}

pub fn find_nearest_friendly_objective_id(
    db: &Db,
    side: Side,
    pos: Vector2,
    exclude: &[ObjectiveId],
) -> Option<(ObjectiveId, Vector2)> {
    db.persisted
        .objectives
        .into_iter()
        .filter(|(id, o)| o.owner() == side && !exclude.contains(id))
        .map(|(id, o)| {
            let opos = o.pos();
            let d = na::distance(&opos.into(), &pos.into());
            (d, *id, opos)
        })
        .min_by(|(a, _, _), (b, _, _)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .map(|(_, id, opos)| (id, opos))
}

/// Build an escape route for an HVT: a chain of up to `max_hops` friendly objective
/// positions moving progressively away from the frontline (deeper into own territory).
pub fn build_hvt_escape_route(
    db: &Db,
    side: Side,
    start_pos: Vector2,
    departed_obj: ObjectiveId,
    max_hops: usize,
) -> Vec<Vector2> {
    let mut route = Vec::with_capacity(max_hops);
    let mut current_pos = start_pos;
    let mut excluded = vec![departed_obj];
    for _ in 0..max_hops {
        if let Some((next_id, next_pos)) = find_nearest_friendly_objective_id(db, side, current_pos, &excluded) {
            route.push(next_pos);
            excluded.push(next_id);
            current_pos = next_pos;
        } else {
            break;
        }
    }
    route
}

/// Build a road-following waypoint list from `source_pos` to `dest_pos`, threading
/// intermediate friendly objectives that lie within a 30%-wide corridor along the
/// route.  Returns `[intermediates..., dest_pos]` — the spawned group is already at
/// `source_pos` so it is not included.  DCS pathfinds between waypoints on roads via
/// `VehicleFormation::OnRoad`.
pub fn build_reinforcement_route(
    db: &Db,
    side: Side,
    source_pos: Vector2,
    dest_pos: Vector2,
) -> Vec<Vector2> {
    let total_dist = na::distance(&source_pos.into(), &dest_pos.into());
    if total_dist < 1.0 {
        return vec![dest_pos];
    }
    let route_dir = (dest_pos - source_pos) / total_dist;
    let corridor = total_dist * 0.30;

    let mut intermediates: Vec<(f64, Vector2)> = db
        .persisted
        .objectives
        .into_iter()
        .filter_map(|(_, o)| {
            if o.owner() != side {
                return None;
            }
            let p = o.pos();
            // Exclude the source and destination endpoints (50 m threshold)
            if na::distance(&p.into(), &source_pos.into()) < 50.0 {
                return None;
            }
            if na::distance(&p.into(), &dest_pos.into()) < 50.0 {
                return None;
            }
            // Project p onto the route axis
            let offset = p - source_pos;
            let t = offset.x * route_dir.x + offset.y * route_dir.y;
            if t <= 0.0 || t >= total_dist {
                return None;
            }
            // Perpendicular distance from the straight line
            let perp_x = offset.x - route_dir.x * t;
            let perp_y = offset.y - route_dir.y * t;
            let perp = (perp_x * perp_x + perp_y * perp_y).sqrt();
            if perp > corridor {
                return None;
            }
            Some((t, p))
        })
        .collect();

    // Sort by progress along the route; keep at most 3 intermediates
    intermediates
        .sort_by(|(t1, _), (t2, _)| t1.partial_cmp(t2).unwrap_or(std::cmp::Ordering::Equal));
    intermediates.truncate(3);

    let mut route: Vec<Vector2> = intermediates.into_iter().map(|(_, p)| p).collect();
    route.push(dest_pos);
    route
}
