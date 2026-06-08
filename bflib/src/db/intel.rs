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

//! ELINT/SIGINT intelligence database.
//!
//! Accumulates geo-located ground unit contacts from recon flights,
//! AWACS detections, and EWR fusion.  Each contact carries a confidence score that
//! decays exponentially with a configurable half-life and is removed when it falls
//! below a threshold.  F10 map markers are maintained in sync with contact state.

use bfprotocols::cfg::ElintConfig;
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{Vector2, coalition::Side, trigger::MarkId};
use fxhash::FxHashMap;
use std::sync::atomic::{AtomicU64, Ordering};

// ─── Types ───────────────────────────────────────────────────────────────────

/// Stable identifier for an intel contact across ticks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ContactId(u64);

impl ContactId {
    pub fn new() -> Self {
        static SEQ: AtomicU64 = AtomicU64::new(1);
        Self(SEQ.fetch_add(1, Ordering::Relaxed))
    }
}

/// Coarse unit classification stored in an intel contact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum IntelUnitClass {
    Armor,
    AirDefense,
    Artillery,
    Infantry,
    AirBase,
    Naval,
    Unknown,
}

impl IntelUnitClass {
    pub fn label(self) -> &'static str {
        match self {
            Self::Armor      => "Armor",
            Self::AirDefense => "ADS",
            Self::Artillery  => "ARTY",
            Self::Infantry   => "INF",
            Self::AirBase    => "AIRBASE",
            Self::Naval      => "NAVAL",
            Self::Unknown    => "UNK",
        }
    }
}

/// The sensor origin that produced an intel contact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum IntelSource {
    ReconFlight,
    SpecialForces,
    Awacs,
    EwrFusion,
    /// Reserved for operator/admin-injected intel.
    HumanInt,
}

impl IntelSource {
    /// Exponential decay half-life for this source, using the active config.
    pub fn half_life_secs(self, cfg: &ElintConfig) -> u32 {
        match self {
            Self::ReconFlight    => cfg.half_life_recon_secs,
            Self::SpecialForces  => cfg.half_life_sf_secs,
            Self::Awacs          => cfg.half_life_awacs_secs,
            Self::EwrFusion      => cfg.half_life_ewr_secs,
            Self::HumanInt       => cfg.half_life_sf_secs,
        }
    }
}

/// A single geo-located intelligence contact.
#[derive(Debug, Clone)]
pub struct IntelContact {
    pub id: ContactId,
    /// Coalition that owns (can see) this intel.
    pub side: Side,
    /// Side of the detected units.
    pub enemy_side: Side,
    /// Best-known 2-D position (DCS XZ plane).
    pub pos: Vector2,
    /// 1-sigma position uncertainty radius (meters).
    pub pos_uncertainty_m: f32,
    pub unit_class: IntelUnitClass,
    pub unit_count: u8,
    pub source: IntelSource,
    /// 0.0 (expired) – 1.0 (freshly confirmed).
    pub confidence: f32,
    pub detected_at: DateTime<Utc>,
    /// Active F10 map marker ID (None until the mark is placed).
    pub map_mark_rect: Option<MarkId>,
    pub map_mark_label: Option<MarkId>,
}

// ─── Database ────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default)]
pub struct IntelDatabase {
    pub contacts: FxHashMap<ContactId, IntelContact>,
    /// Per-side index for fast enumeration.
    by_side: FxHashMap<Side, Vec<ContactId>>,
}

impl IntelDatabase {
    /// Insert a new contact or update an existing nearby one.
    /// Returns the ContactId that was created or updated.
    pub fn upsert(
        &mut self,
        side: Side,
        enemy_side: Side,
        pos: Vector2,
        unit_class: IntelUnitClass,
        unit_count: u8,
        source: IntelSource,
        cfg: &ElintConfig,
        now: DateTime<Utc>,
    ) -> ContactId {
        let assoc_sq = cfg.contact_cluster_radius_m.powi(2);

        // Look for an existing contact of the same class close enough to merge.
        let existing_id = self.by_side
            .get(&side)
            .and_then(|ids| {
                ids.iter().find(|&&id| {
                    self.contacts.get(&id).map_or(false, |c| {
                        c.unit_class == unit_class
                            && na::distance_squared(&c.pos.into(), &pos.into()) <= assoc_sq
                    })
                }).copied()
            });

        if let Some(id) = existing_id {
            if let Some(c) = self.contacts.get_mut(&id) {
                // Merge: update position towards new obs, refresh confidence.
                c.pos = Vector2::new(
                    c.pos.x * 0.6 + pos.x * 0.4,
                    c.pos.y * 0.6 + pos.y * 0.4,
                );
                c.unit_count = c.unit_count.max(unit_count);
                c.source = source;
                c.confidence = 1.0;
                c.detected_at = now;
            }
            id
        } else {
            // Enforce per-side cap — evict lowest-confidence contact if needed.
            let side_ids = self.by_side.entry(side).or_default();
            if side_ids.len() >= cfg.max_contacts_per_side {
                // Find and evict the least confident entry.
                if let Some(evict_id) = side_ids
                    .iter()
                    .min_by(|&&a, &&b| {
                        let ca = self.contacts.get(&a).map_or(0.0_f32, |c| c.confidence);
                        let cb = self.contacts.get(&b).map_or(0.0_f32, |c| c.confidence);
                        ca.partial_cmp(&cb).unwrap_or(std::cmp::Ordering::Equal)
                    })
                    .copied()
                {
                    self.contacts.remove(&evict_id);
                    side_ids.retain(|&id| id != evict_id);
                }
            }
            let id = ContactId::new();
            self.contacts.insert(id, IntelContact {
                id,
                side,
                enemy_side,
                pos,
                pos_uncertainty_m: 1500.0,
                unit_class,
                unit_count,
                source,
                confidence: 1.0,
                detected_at: now,
                map_mark_rect: None,
                map_mark_label: None,
            });
            self.by_side.entry(side).or_default().push(id);
            id
        }
    }

    /// Decay confidence on all contacts. Returns IDs whose marks need
    /// updating (confidence changed) and IDs whose contacts were deleted
    /// (marks need removing).
    pub fn tick_decay(
        &mut self,
        cfg: &ElintConfig,
        _now: DateTime<Utc>,
        dt_secs: f64,
    ) -> (Vec<ContactId>, Vec<(Option<MarkId>, Option<MarkId>)>) {
        let mut updated: Vec<ContactId> = Vec::new();
        let mut removed: Vec<(Option<MarkId>, Option<MarkId>)> = Vec::new();
        let ln2 = std::f64::consts::LN_2;

        self.contacts.retain(|_, c| {
            let half_life = c.source.half_life_secs(cfg) as f64;
            let lambda = ln2 / half_life;
            c.confidence *= (-lambda * dt_secs).exp() as f32;
            if c.confidence < cfg.confidence_delete_threshold {
                removed.push((c.map_mark_rect, c.map_mark_label));
                // Also remove from by_side index
                false
            } else {
                updated.push(c.id);
                true
            }
        });

        // Rebuild by_side to remove stale entries
        for ids in self.by_side.values_mut() {
            ids.retain(|id| self.contacts.contains_key(id));
        }

        (updated, removed)
    }

    /// Top N highest-confidence contacts visible to `side`, ordered by
    /// `confidence × 1/distance` so nearby high-quality intel is ranked first.
    pub fn top_contacts_for_side(
        &self,
        side: Side,
        observer_pos: Vector2,
        n: usize,
    ) -> Vec<&IntelContact> {
        let mut scored: Vec<(&IntelContact, f32)> = self
            .by_side
            .get(&side)
            .into_iter()
            .flat_map(|ids| ids.iter())
            .filter_map(|id| self.contacts.get(id))
            .map(|c| {
                let dist = na::distance(&observer_pos.into(), &c.pos.into()).max(1.0) as f32;
                let score = c.confidence / dist * 10_000.0;
                (c, score)
            })
            .collect();
        scored.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        scored.into_iter().take(n).map(|(c, _)| c).collect()
    }

    /// Build the map marker label text for a contact.
    pub fn marker_text(contact: &IntelContact, cfg: &ElintConfig) -> CompactString {
        let class_part = if cfg.show_unit_class {
            format_compact!("{}×{}", contact.unit_count, contact.unit_class.label())
        } else {
            format_compact!("{}×UNK", contact.unit_count)
        };
        let side_label = match contact.enemy_side {
            dcso3::coalition::Side::Blue => "BLU",
            dcso3::coalition::Side::Red  => "RED",
            _                            => "NEU",
        };
        let acc_km = (contact.pos_uncertainty_m / 1000.0).max(0.1);
        if cfg.show_confidence_on_map {
            format_compact!(
                "[INTEL/{side_label}] {class_part} | {:.0}% ±{acc_km:.1}km",
                contact.confidence * 100.0
            )
        } else {
            format_compact!("[INTEL/{side_label}] {class_part} ±{acc_km:.1}km")
        }
    }
}
