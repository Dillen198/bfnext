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

            Self::Barrage { id, .. } => *id,
            Self::MissileStrike { id, .. } => *id,
            Self::ConvoyAmbush { id, .. } => *id,
            Self::EnemyCap { id, .. } => *id,
            Self::CommanderCap { id, .. } => *id,
        }
    }

    pub fn description(&self) -> CompactString {
        match self {

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
        cap_side: Side,
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
                        effects.push(EventEffect::DespawnCap { event_id: *id, cap_side: *cap_side });
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
                        effects.push(EventEffect::DespawnCap { event_id: *id, cap_side: *cap_side });
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
        // Find the most threatened objective to act as our "threat center"
        let threat_center = db
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, obj)| obj.owner() == cap_side)
            .max_by(|(_, a), (_, b)| obj_score(a).cmp(&obj_score(b)))
            .map(|(_, obj)| obj.pos())
            .unwrap_or_else(|| Vector2::new(0., 0.));

        // Now pick the furthest airbase from the threat center to scramble from
        let best = db
            .persisted
            .objectives
            .into_iter()
            .filter(|(oid, obj)| {
                obj.owner() == cap_side
                    && obj.is_airbase()
                    && !active_cap_objs.contains(oid)
            })
            .max_by(|(_, a), (_, b)| {
                let da = na::distance_squared(&a.pos().into(), &threat_center.into());
                let db = na::distance_squared(&b.pos().into(), &threat_center.into());
                da.partial_cmp(&db).unwrap_or(std::cmp::Ordering::Equal)
            });

        let (objective, obj) = match best {
            Some(pair) => pair,
            None => return,
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
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

}

