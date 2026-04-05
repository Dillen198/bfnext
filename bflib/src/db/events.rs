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
    },
    ReinforcementWave {
        id: EventId,
        side: Side,
        objective: ObjectiveId,
        arrival_time: DateTime<Utc>,
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
}

impl CampaignEvent {
    pub fn id(&self) -> EventId {
        match self {
            Self::HighValueTarget { id, .. } => *id,
            Self::ReinforcementWave { id, .. } => *id,
            Self::CounterOffensive { id, .. } => *id,
            Self::Barrage { id, .. } => *id,
            Self::ConvoyAmbush { id, .. } => *id,
            Self::EnemyCap { id, .. } => *id,
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
            Self::ConvoyAmbush { ambush_side, .. } => format_compact!("{:?} Convoy Ambush", ambush_side),
            Self::EnemyCap { cap_side, .. } => format_compact!("{:?} Enemy CAP", cap_side),
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
        /// Nearest friendly objective position for the HVT to flee toward.
        escape_pos: Option<Vector2>,
    },
    /// Issue FireAtPoint orders to armor/LR groups at `source_objective`.
    FireBarrage {
        event_id: EventId,
        side: Side,
        source_objective: ObjectiveId,
        target_pos: Vector2,
    },
    /// Spawn an ambush force for `ambush_side` near `spawn_pos`.
    SpawnAmbush {
        event_id: EventId,
        ambush_side: Side,
        spawn_pos: Vector2,
        source_objective: ObjectiveId,
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
}

/// Manages dynamic campaign events
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EventScheduler {
    pub active_events: Vec<CampaignEvent>,
    pub last_event_check: Option<DateTime<Utc>>,
    pub total_events_spawned: u64,
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
    /// Deferred move orders: GroupId → target position.
    /// Retried each tick until the DCS group appears (spawn queue lag).
    #[serde(skip)]
    pub pending_moves: FxHashMap<GroupId, Vector2>,
    /// CAP event → list of spawned group IDs (for cleanup on expiry).
    #[serde(skip)]
    pub cap_groups: FxHashMap<EventId, SmallVec<[GroupId; 2]>>,
}

impl EventScheduler {
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
        let mut messages: Vec<CompactString> = Vec::new();
        let mut effects: Vec<EventEffect> = Vec::new();

        // ---- Build candidate lists used by multiple paths ----
        let all_owned: Vec<_> = db
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, o)| o.owner() != Side::Neutral)
            .map(|(id, o)| (*id, o.owner(), o.pos(), o.name.as_str().into(), o.supply()))
            .collect();

        let red_count = all_owned.iter().filter(|(_, s, ..)| *s == Side::Red).count();
        let blue_count = all_owned.iter().filter(|(_, s, ..)| *s == Side::Blue).count();
        let total_count = (red_count + blue_count).max(1);

        // ---- Process active events ----
        let mut expired_indices: Vec<usize> = Vec::new();
        for (i, event) in self.active_events.iter_mut().enumerate() {
            match event {
                // -- Reinforcement wave --
                CampaignEvent::ReinforcementWave { id, side, objective, arrival_time } => {
                    if now >= *arrival_time {
                        info!("ReinforcementWave arriving for {:?} at {:?}", side, objective);
                        let obj = db.persisted.objectives.get(objective);
                        let obj_pos = obj.map(|o| o.pos()).unwrap_or_default();
                        let obj_name = obj.map(|o| o.name.as_str()).unwrap_or("unknown");
                        messages.push(format_compact!(
                            "COMMAND: {:?} reinforcements have arrived at {}!",
                            side, obj_name
                        ));
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
                    id, side, objective, expires_at, reward_points, announced,
                } => {
                    if now >= *expires_at {
                        messages.push(format_compact!(
                            "INTEL: High-value target near {} has escaped!",
                            db.persisted.objectives.get(objective)
                                .map(|o| o.name.as_str()).unwrap_or("unknown")
                        ));
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        self.hvt_groups.retain(|_, (eid, _)| *eid != *id);
                        expired_indices.push(i);
                    } else if *announced {
                        let obj_pos = db.persisted.objectives.get(objective)
                            .map(|o| o.pos()).unwrap_or_default();
                        // Find a nearby friendly objective for the HVT to escape toward
                        let escape_pos = find_nearest_friendly_objective(db, *side, obj_pos, Some(*objective));
                        effects.push(EventEffect::SpawnHvt {
                            event_id: *id,
                            side: *side,
                            objective: *objective,
                            obj_pos,
                            reward_points: *reward_points,
                            escape_pos,
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

                // -- Convoy ambush --
                CampaignEvent::ConvoyAmbush { id, ambush_side, spawn_pos, source_objective, expires_at, spawned } => {
                    if !*spawned {
                        *spawned = true;
                        effects.push(EventEffect::SpawnAmbush {
                            event_id: *id,
                            ambush_side: *ambush_side,
                            spawn_pos: *spawn_pos,
                            source_objective: *source_objective,
                        });
                    }
                    if now >= *expires_at {
                        if let Some(marks) = self.event_marks.remove(id) {
                            effects.push(EventEffect::DeleteMarks { ids: marks });
                        }
                        expired_indices.push(i);
                    }
                }

                // -- Enemy CAP --
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
                let enemy_side = match attacking_side {
                    Side::Red => Side::Blue,
                    Side::Blue => Side::Red,
                    Side::Neutral => continue,
                };
                let targets: SmallVec<[ObjectiveId; 4]> = all_owned
                    .iter()
                    .filter(|(_, s, ..)| *s == enemy_side)
                    .take(2)
                    .map(|(id, ..)| *id)
                    .collect();
                if !targets.is_empty() {
                    let duration = 900u32;
                    let event = CampaignEvent::CounterOffensive {
                        id: EventId::new(),
                        attacking_side,
                        target_objectives: targets,
                        started_at: now,
                        duration_secs: duration,
                        orders_issued: false,
                    };
                    messages.push(format_compact!(
                        "WARNING: {:?} forces are mounting a counter-attack! Duration: {} minutes",
                        attacking_side, duration / 60
                    ));
                    self.total_events_spawned += 1;
                    info!("Escalation: spawned counter-offensive by {:?}", attacking_side);
                    self.active_events.push(event);
                }
            }
        }

        // ---- Periodic event check ----
        let should_check = match self.last_event_check {
            None => true,
            Some(last) => (now - last).num_seconds() >= cfg.check_interval_secs as i64,
        };

        if should_check {
            self.last_event_check = Some(now);
            info!(
                "Campaign events check: {} active, {} escalations queued",
                self.active_events.len(),
                self.escalation_queue.len()
            );
            if self.active_events.len() < cfg.max_concurrent_events as usize {
                let mut rng = rand::thread_rng();
                if rng.r#gen::<f64>() < cfg.event_probability {
                    self.try_spawn_event(
                        db, cfg, now,
                        red_count, blue_count, total_count,
                        &all_owned,
                        &mut messages, &mut effects,
                    )?;
                }
            }
        }

        Ok((messages, effects))
    }

    // -------------------------------------------------------------------------
    // Event spawning
    // -------------------------------------------------------------------------

    fn try_spawn_event(
        &mut self,
        db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        red_count: usize,
        blue_count: usize,
        total_count: usize,
        all_owned: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        messages: &mut Vec<CompactString>,
        effects: &mut Vec<EventEffect>,
    ) -> Result<()> {
        let mut rng = rand::thread_rng();

        // ---- A: State-aware weights ----
        // Losing side (fewer objectives) favours reinforcements.
        // Winning side favours counter-offensive / barrage.
        // Both always eligible for HVT.
        let red_share = red_count as f64 / total_count as f64;
        let blue_share = blue_count as f64 / total_count as f64;

        // Build weighted event-type table:
        // (weight, type_id): 0=HVT, 1=Reinforcement, 2=CounterOffensive, 3=Barrage, 4=Ambush
        let mut weighted: Vec<(u32, u8)> = vec![(10, 0)]; // HVT always available

        if cfg.reinforcement_waves_enabled {
            // Losing side gets disproportionately more reinforcements
            let red_rein = ((1.0 - red_share) * 20.0) as u32 + 5;
            let blue_rein = ((1.0 - blue_share) * 20.0) as u32 + 5;
            // We pick the reinforcement event for the side with higher weight (50/50 otherwise)
            // Encode: side choice is handled inside the spawn function based on supply pressure.
            weighted.push((red_rein.max(blue_rein), 1));
        }

        if cfg.counter_offensives_enabled {
            // Winning side launches counter-offensive
            let weight = ((red_share - blue_share).abs() * 15.0) as u32 + 3;
            weighted.push((weight, 2));
        }

        if cfg.barrage_enabled {
            // Barrage probability rises with contested territory
            let contested_count = all_owned.iter().filter(|(_, _, _, _, _)| true)
                .filter(|(oid, side, _, _, _)| {
                    let enemy = match side {
                        Side::Red => Side::Blue,
                        Side::Blue => Side::Red,
                        Side::Neutral => return false,
                    };
                    all_owned.iter().any(|(oid2, s2, _pos2, _, _)| {
                        if *s2 != enemy { return false; }
                        let obj1 = db.persisted.objectives.get(oid);
                        let obj2 = db.persisted.objectives.get(oid2);
                        if let (Some(o1), Some(o2)) = (obj1, obj2) {
                            na::distance(&o1.pos().into(), &o2.pos().into()) < 50_000.0
                        } else {
                            false
                        }
                    })
                })
                .count();
            let weight = (contested_count as u32).min(10) + 2;
            weighted.push((weight, 3));
        }

        if cfg.ambush_enabled && !db.ephemeral.active_convoys.is_empty() {
            weighted.push((8, 4));
        }

        if cfg.enemy_cap_enabled {
            // CAP fires at a flat probability rather than weighted
            let weight = (cfg.cap_probability * 10.0) as u32 + 1;
            weighted.push((weight, 5));
        }

        // Weighted random selection
        let total_weight: u32 = weighted.iter().map(|(w, _)| w).sum();
        let mut roll = rng.r#gen_range(0..total_weight);
        let mut choice = 0u8;
        for (w, t) in &weighted {
            if roll < *w {
                choice = *t;
                break;
            }
            roll -= w;
        }

        match choice {
            0 => self.spawn_hvt_event(db, cfg, now, all_owned, messages, effects),
            1 => self.spawn_reinforcement_wave(db, cfg, now, red_count, blue_count, all_owned, messages, effects),
            2 => self.spawn_counter_offensive(cfg, now, all_owned, messages),
            3 => self.spawn_barrage_event(db, cfg, now, all_owned, messages, effects),
            4 => self.spawn_convoy_ambush(db, cfg, now, all_owned, messages, effects),
            5 => self.spawn_cap_event(cfg, now, all_owned, messages),
            _ => {}
        }
        Ok(())
    }

    // -- HVT --

    fn spawn_hvt_event(
        &mut self,
        _db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        candidates: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        messages: &mut Vec<CompactString>,
        _effects: &mut Vec<EventEffect>,
    ) {
        let mut rng = rand::thread_rng();
        let idx = rng.r#gen_range(0..candidates.len());
        let (oid, side, _pos, ref name, _supply) = candidates[idx];

        let event = CampaignEvent::HighValueTarget {
            id: EventId::new(),
            objective: oid,
            side,
            expires_at: now + chrono::Duration::seconds(cfg.hvt_duration_secs as i64),
            reward_points: cfg.hvt_reward_points,
            announced: true,
        };

        messages.push(format_compact!(
            "INTEL: High-value target detected near {}! Destroy within {} minutes for {} bonus points. It is attempting to evacuate!",
            name, cfg.hvt_duration_secs / 60, cfg.hvt_reward_points
        ));

        self.total_events_spawned += 1;
        info!("Spawned HVT event near objective {:?}", oid);
        self.active_events.push(event);
    }

    // -- Reinforcement wave (F: supply-pressure weighted) --

    fn spawn_reinforcement_wave(
        &mut self,
        _db: &Db,
        _cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        red_count: usize,
        blue_count: usize,
        candidates: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        messages: &mut Vec<CompactString>,
        effects: &mut Vec<EventEffect>,
    ) {
        let mut rng = rand::thread_rng();

        // F: Bias toward the losing side and toward low-supply objectives.
        // Build a weighted list of candidates.
        let total_count = (red_count + blue_count).max(1);
        let red_share = red_count as f64 / total_count as f64;
        let blue_share = blue_count as f64 / total_count as f64;

        let weighted: Vec<(u32, usize)> = candidates
            .iter()
            .enumerate()
            .map(|(i, (_, side, _, _, supply))| {
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
                (side_bonus * supply_bonus, i)
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

        let arrival_delay = rng.r#gen_range(30..120i64); // 30s–2min so it's visible in testing
        let id = EventId::new();
        let event = CampaignEvent::ReinforcementWave {
            id,
            side,
            objective: oid,
            arrival_time: now + chrono::Duration::seconds(arrival_delay),
        };

        let supply_note = if supply < 50 {
            format_compact!(" (critically low supplies)", )
        } else {
            format_compact!("")
        };

        messages.push(format_compact!(
            "COMMAND: {:?} reinforcements inbound to {}{}, ETA {} minutes",
            side, obj_name, supply_note, arrival_delay / 60
        ));

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

    // -- Counter-offensive (A: state-aware side selection) --

    fn spawn_counter_offensive(
        &mut self,
        _cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        all_owned: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        messages: &mut Vec<CompactString>,
    ) {
        let mut rng = rand::thread_rng();

        // A: The winning side launches the counter-offensive
        let red_count = all_owned.iter().filter(|(_, s, ..)| *s == Side::Red).count();
        let blue_count = all_owned.iter().filter(|(_, s, ..)| *s == Side::Blue).count();
        let attacking_side = if red_count > blue_count {
            Side::Red
        } else if blue_count > red_count {
            Side::Blue
        } else {
            if rng.r#gen_bool(0.5) { Side::Red } else { Side::Blue }
        };
        let enemy_side = match attacking_side {
            Side::Red => Side::Blue,
            Side::Blue => Side::Red,
            Side::Neutral => return,
        };

        let count = rng.r#gen_range(1..=3);
        let mut targets: SmallVec<[ObjectiveId; 4]> = SmallVec::new();
        let mut used = std::collections::HashSet::new();

        for (oid, side, _, _, _) in all_owned {
            if targets.len() >= count { break; }
            if *side == enemy_side && !used.contains(oid) {
                targets.push(*oid);
                used.insert(*oid);
            }
        }

        if targets.is_empty() { return; }

        let duration = rng.r#gen_range(600..1800i64);
        let event = CampaignEvent::CounterOffensive {
            id: EventId::new(),
            attacking_side,
            target_objectives: targets,
            started_at: now,
            duration_secs: duration as u32,
            orders_issued: false,
        };

        messages.push(format_compact!(
            "WARNING: {:?} forces launching counter-offensive! Duration: {} minutes",
            attacking_side, duration / 60
        ));

        self.total_events_spawned += 1;
        info!("Spawned counter-offensive by {:?} (winning side)", attacking_side);
        self.active_events.push(event);
    }

    // -- C: Artillery/armor barrage --

    fn spawn_barrage_event(
        &mut self,
        db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        all_owned: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        messages: &mut Vec<CompactString>,
        _effects: &mut Vec<EventEffect>,
    ) {
        use super::objective::ObjGroupClass;
        let mut rng = rand::thread_rng();

        // Find a source objective that has alive Armor/Mr/Lr groups
        let mut sources: Vec<(ObjectiveId, Side, Vector2)> = Vec::new();
        for (oid, side, pos, _, _) in all_owned {
            if *side == Side::Neutral { continue; }
            let obj = match db.persisted.objectives.get(oid) {
                Some(o) => o,
                None => continue,
            };
            let has_fire_groups = obj.groups().get(side).map(|gids| {
                gids.into_iter().any(|gid| {
                    db.persisted.groups.get(gid).map(|g| {
                        matches!(g.class, ObjGroupClass::Armor | ObjGroupClass::Mr | ObjGroupClass::Lr)
                            && g.units.into_iter().any(|uid| {
                                db.persisted.units.get(uid).map(|u| !u.dead).unwrap_or(false)
                            })
                    }).unwrap_or(false)
                })
            }).unwrap_or(false);
            if has_fire_groups {
                sources.push((*oid, *side, *pos));
            }
        }

        if sources.is_empty() { return; }

        let src_idx = rng.r#gen_range(0..sources.len());
        let (src_oid, src_side, src_pos) = sources[src_idx];
        let enemy_side = match src_side {
            Side::Red => Side::Blue,
            Side::Blue => Side::Red,
            Side::Neutral => return,
        };

        // Find an enemy objective within 40 km to fire at
        let target = all_owned.iter()
            .filter(|(_, s, pos, _, _)| {
                *s == enemy_side
                    && na::distance(&(*pos).into(), &src_pos.into()) < 40_000.0
                    && na::distance(&(*pos).into(), &src_pos.into()) > 5_000.0
            })
            .min_by(|(_, _, pa, ..), (_, _, pb, ..)| {
                let da = na::distance(&(*pa).into(), &src_pos.into());
                let db_d = na::distance(&(*pb).into(), &src_pos.into());
                da.partial_cmp(&db_d).unwrap_or(std::cmp::Ordering::Equal)
            });

        let (target_oid, _, target_pos, target_name, _) = match target {
            Some(t) => t,
            None => return,
        };

        let id = EventId::new();
        let event = CampaignEvent::Barrage {
            id,
            side: src_side,
            source_objective: src_oid,
            target_pos: *target_pos,
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

    fn spawn_convoy_ambush(
        &mut self,
        db: &Db,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        all_owned: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        messages: &mut Vec<CompactString>,
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

        let id = EventId::new();
        let event = CampaignEvent::ConvoyAmbush {
            id,
            ambush_side,
            spawn_pos,
            source_objective,
            expires_at: now + chrono::Duration::seconds(cfg.ambush_duration_secs as i64),
            spawned: false,
        };

        messages.push(format_compact!(
            "COMMAND: {:?} supply convoy is under attack — enemy ambush forces detected!",
            convoy.side
        ));

        self.total_events_spawned += 1;
        info!("Spawned convoy ambush by {:?} near convoy {:?}", ambush_side, convoy.id);
        self.active_events.push(event);
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    // -- E: Dynamic enemy CAP --

    fn spawn_cap_event(
        &mut self,
        cfg: &CampaignEventsCfg,
        now: DateTime<Utc>,
        all_owned: &[(ObjectiveId, Side, Vector2, dcso3::String, u8)],
        messages: &mut Vec<CompactString>,
    ) {
        let mut rng = rand::thread_rng();

        // Avoid stacking multiple CAP events
        let already_active = self.active_events.iter().any(|e| matches!(e, CampaignEvent::EnemyCap { .. }));
        if already_active {
            return;
        }

        // Pick a random owned objective to defend with CAP
        let candidates: Vec<_> = all_owned.iter()
            .filter(|(_, s, ..)| *s != Side::Neutral)
            .collect();
        if candidates.is_empty() { return; }

        let idx = rng.r#gen_range(0..candidates.len());
        let (oid, side, _, ref name, _) = *candidates[idx];

        let id = EventId::new();
        let event = CampaignEvent::EnemyCap {
            id,
            cap_side: side,
            objective: oid,
            expires_at: now + chrono::Duration::seconds(cfg.cap_duration_secs as i64),
            spawned: false,
        };

        let enemy_side = match side {
            Side::Red => Side::Blue,
            Side::Blue => Side::Red,
            _ => return,
        };
        messages.push(format_compact!(
            "INTEL: [{:?}] Enemy CAP detected over {}! Watch your six.",
            enemy_side, name
        ));

        self.total_events_spawned += 1;
        info!("Spawned enemy CAP for {:?} over {:?}", side, oid);
        self.active_events.push(event);
    }
}

/// Find the nearest friendly objective to `pos` owned by `side`, excluding `exclude`.
pub fn find_nearest_friendly_objective(
    db: &Db,
    side: Side,
    pos: Vector2,
    exclude: Option<ObjectiveId>,
) -> Option<Vector2> {
    db.persisted
        .objectives
        .into_iter()
        .filter(|(id, o)| {
            o.owner() == side && exclude.map(|ex| **id != ex).unwrap_or(true)
        })
        .map(|(_, o)| {
            let opos = o.pos();
            let d = na::distance(&opos.into(), &pos.into());
            (d, opos)
        })
        .min_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .map(|(_, opos)| opos)
}
