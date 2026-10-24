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

use bfprotocols::{cfg::ElintConfig, db::group::UnitId};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{Vector2, coalition::Side, trigger::MarkId};
use fxhash::FxHashMap;
use smallvec::SmallVec;
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

    /// The raw counter value, for use as a stable wire id.
    pub fn raw(self) -> u64 {
        self.0
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

    /// Classify a unit from its `unit_classification` tags. Shared by every
    /// intel source (recon scan, JTAC contacts, ...) so they all bucket units
    /// the same way.
    pub fn from_tags(tags: bfprotocols::cfg::UnitTags) -> Self {
        use bfprotocols::cfg::UnitTag;
        if tags.0.contains(UnitTag::SAM) || tags.0.contains(UnitTag::AAA) {
            Self::AirDefense
        } else if tags.0.contains(UnitTag::Armor) || tags.0.contains(UnitTag::APC) {
            Self::Armor
        } else if tags.0.contains(UnitTag::Artillery) {
            Self::Artillery
        } else if tags.0.contains(UnitTag::Infantry) {
            Self::Infantry
        } else if tags.0.contains(UnitTag::Boat) {
            Self::Naval
        } else {
            Self::Unknown
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
    /// A JTAC (player or AI) currently has, or recently had, eyes-on the
    /// contact. Refreshed to full confidence every tick the JTAC still sees
    /// it, so it only starts decaying once the JTAC loses it -- and uses a
    /// much longer half-life so it stays on the map long after that.
    Jtac,
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
            Self::Jtac           => cfg.half_life_jtac_secs,
            Self::HumanInt       => cfg.half_life_sf_secs,
        }
    }

    /// How well this sensor locates what it found, as a 1-sigma radius in
    /// metres. A JTAC with eyes on a target knows exactly where it is; a
    /// radar fusion track is a guess with a kilometre or two in it. The map
    /// draws this as the uncertainty ring, and only bothers when there is
    /// enough of it to be worth flying against.
    pub fn pos_uncertainty_m(self) -> f32 {
        match self {
            Self::Jtac => 150.,
            Self::SpecialForces => 300.,
            Self::ReconFlight => 600.,
            Self::HumanInt => 1_000.,
            Self::Awacs => 2_500.,
            Self::EwrFusion => 4_000.,
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
    /// Where the marks were last drawn. A pin cannot be edited in place, so it
    /// is only re-dropped once the contact has actually moved.
    pub mark_pos: Vector2,
    /// Confidence the marker text was written at, so a decaying contact's
    /// marker is refreshed a few times over its life instead of either
    /// freezing at its first reading or being re-dropped every tick.
    pub mark_confidence: f32,
    /// Position-uncertainty ring. Its radius is how unsure the engine is about
    /// where this contact actually is, which is the part a pilot acts on.
    pub map_mark_ring: Option<MarkId>,
    /// The units a JTAC has had eyes on inside this contact, see
    /// `IntelDatabase::note_jtac_unit`. Empty for every other source.
    pub jtac_units: SmallVec<[UnitId; 4]>,
}

// ─── Database ────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default)]
pub struct IntelDatabase {
    pub contacts: FxHashMap<ContactId, IntelContact>,
    /// Per-side index for fast enumeration.
    by_side: FxHashMap<Side, Vec<ContactId>>,
    /// Marks belonging to contacts that were dropped outside the decay pass
    /// (the per-side cap evicting the least confident one). Drained by
    /// `Ephemeral::tick_intel_decay`, which is what actually talks to the map.
    pub orphaned_marks: Vec<(Option<MarkId>, Option<MarkId>, Option<MarkId>)>,
    /// The contact each JTAC-tracked unit currently feeds, keyed by the side
    /// that owns the intel. See `note_jtac_unit`.
    jtac_unit_contact: FxHashMap<(Side, UnitId), ContactId>,
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
                c.pos_uncertainty_m = source.pos_uncertainty_m();
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
                    // Take the evicted contact's marks with it. Dropping the
                    // contact alone left its shape, ring and pin on the F10
                    // map with nothing tracking them, so once a side hit the
                    // contact cap every further detection added permanent
                    // clutter that no decay could ever clear.
                    if let Some(c) = self.contacts.remove(&evict_id) {
                        self.orphaned_marks
                            .push((c.map_mark_rect, c.map_mark_label, c.map_mark_ring));
                    }
                    side_ids.retain(|&id| id != evict_id);
                }
            }
            let id = ContactId::new();
            self.contacts.insert(id, IntelContact {
                id,
                side,
                enemy_side,
                pos,
                pos_uncertainty_m: source.pos_uncertainty_m(),
                unit_class,
                unit_count,
                source,
                confidence: 1.0,
                detected_at: now,
                map_mark_rect: None,
                map_mark_ring: None,
                mark_pos: pos,
                mark_confidence: 1.0,
                map_mark_label: None,
                jtac_units: SmallVec::new(),
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
    ) -> (Vec<ContactId>, Vec<(Option<MarkId>, Option<MarkId>, Option<MarkId>)>) {
        let mut updated: Vec<ContactId> = Vec::new();
        let mut removed: Vec<(Option<MarkId>, Option<MarkId>, Option<MarkId>)> = Vec::new();
        let ln2 = std::f64::consts::LN_2;

        self.contacts.retain(|_, c| {
            let half_life = c.source.half_life_secs(cfg) as f64;
            let lambda = ln2 / half_life;
            c.confidence *= (-lambda * dt_secs).exp() as f32;
            if c.confidence < cfg.confidence_delete_threshold {
                removed.push((c.map_mark_rect, c.map_mark_label, c.map_mark_ring));
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
        // Same for the JTAC unit index: a contact that faded out (or was
        // evicted by the cap) no longer owns the units that fed it.
        let contacts = &self.contacts;
        self.jtac_unit_contact.retain(|_, id| contacts.contains_key(id));

        (updated, removed)
    }

    /// `upsert` for one unit a JTAC has eyes on, remembering which contact
    /// the unit feeds so the contact can leave with it.
    ///
    /// Players reported JTAC drones leaving a carpet of stale intel pins
    /// around a base: every tank a Reaper had watched stayed pinned at 100%
    /// long after it burned, and one that drove off left its old pin behind
    /// and grew a new one, because a JTAC contact only ever faded out over
    /// its (hour-long by default) half-life. Now a unit that shows up in a
    /// different contact is taken out of the old one, and a JTAC contact
    /// with none of its units left in it is removed straight away -- the
    /// JTAC saw it move on. `retire_jtac_units` does the same for units that
    /// died.
    pub fn note_jtac_unit(
        &mut self,
        side: Side,
        enemy_side: Side,
        uid: UnitId,
        pos: Vector2,
        unit_class: IntelUnitClass,
        cfg: &ElintConfig,
        now: DateTime<Utc>,
    ) -> ContactId {
        let id = self.upsert(side, enemy_side, pos, unit_class, 1, IntelSource::Jtac, cfg, now);
        if let Some(c) = self.contacts.get_mut(&id) {
            if !c.jtac_units.contains(&uid) {
                c.jtac_units.push(uid);
            }
        }
        match self.jtac_unit_contact.insert((side, uid), id) {
            Some(old) if old != id => self.release_jtac_unit(old, uid),
            Some(_) | None => (),
        }
        id
    }

    /// Take `uid` out of contact `id`, removing the contact if it was the
    /// JTAC's last unit in it. A contact some other sensor has refreshed
    /// since (its source is no longer `Jtac`) is left to decay as that
    /// sensor's intel.
    fn release_jtac_unit(&mut self, id: ContactId, uid: UnitId) {
        let Some(c) = self.contacts.get_mut(&id) else { return };
        c.jtac_units.retain(|u| *u != uid);
        if c.jtac_units.is_empty() && c.source == IntelSource::Jtac {
            self.remove_contact(id);
        }
    }

    /// Drop a contact outside the decay pass. Its marks go to
    /// `orphaned_marks` for `Ephemeral::tick_intel_decay` to delete.
    fn remove_contact(&mut self, id: ContactId) {
        let Some(c) = self.contacts.remove(&id) else { return };
        self.orphaned_marks
            .push((c.map_mark_rect, c.map_mark_label, c.map_mark_ring));
        if let Some(ids) = self.by_side.get_mut(&c.side) {
            ids.retain(|i| *i != id);
        }
        for uid in &c.jtac_units {
            if self.jtac_unit_contact.get(&(c.side, *uid)) == Some(&id) {
                self.jtac_unit_contact.remove(&(c.side, *uid));
            }
        }
    }

    /// Forget every JTAC-tracked unit that `gone` says no longer exists
    /// (dead, retired as a ghost, deleted), removing the JTAC contacts that
    /// leaves empty. Only touches contacts that actually lost a unit, so a
    /// quiet map costs one hash lookup per tracked unit and no map traffic.
    /// Returns how many contacts were removed.
    pub fn retire_jtac_units(&mut self, mut gone: impl FnMut(UnitId) -> bool) -> usize {
        let stale: SmallVec<[(Side, UnitId); 16]> = self
            .jtac_unit_contact
            .keys()
            .filter(|(_, uid)| gone(*uid))
            .copied()
            .collect();
        let before = self.contacts.len();
        for key in stale {
            if let Some(id) = self.jtac_unit_contact.remove(&key) {
                self.release_jtac_unit(id, key.1);
            }
        }
        before - self.contacts.len()
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

    /// Every contact visible to `side`, unordered. Backs the dashboard
    /// TACMAP ground picture (`crate::admin::query_tacmap`).
    pub fn contacts_for(&self, side: Side) -> impl Iterator<Item = &IntelContact> {
        self.by_side
            .get(&side)
            .into_iter()
            .flat_map(|ids| ids.iter())
            .filter_map(move |id| self.contacts.get(id))
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

#[cfg(test)]
mod tests {
    use super::*;

    fn at(x: f64) -> Vector2 {
        Vector2::new(x, 0.)
    }

    #[test]
    fn jtac_contact_leaves_with_its_last_unit() {
        let cfg = ElintConfig::default();
        let now = Utc::now();
        let mut db = IntelDatabase::default();
        let (a, b) = (UnitId::from(1), UnitId::from(2));
        let id = db.note_jtac_unit(Side::Blue, Side::Red, a, at(0.), IntelUnitClass::Armor, &cfg, now);
        assert_eq!(db.note_jtac_unit(Side::Blue, Side::Red, b, at(50.), IntelUnitClass::Armor, &cfg, now), id);
        db.contacts.get_mut(&id).unwrap().map_mark_label = Some(MarkId::new());
        // One of two tanks burns: the contact still has something in it.
        assert_eq!(db.retire_jtac_units(|u| u == a), 0);
        assert!(db.contacts.contains_key(&id));
        assert!(db.orphaned_marks.is_empty());
        // The second one goes too: so does the pin.
        assert_eq!(db.retire_jtac_units(|u| u == b), 1);
        assert!(db.contacts.is_empty());
        assert_eq!(db.orphaned_marks.len(), 1);
        assert!(db.orphaned_marks[0].1.is_some());
        assert_eq!(db.contacts_for(Side::Blue).count(), 0);
    }

    #[test]
    fn jtac_contact_left_behind_by_a_moving_unit_goes() {
        let cfg = ElintConfig::default();
        let now = Utc::now();
        let mut db = IntelDatabase::default();
        let a = UnitId::from(1);
        let old = db.note_jtac_unit(Side::Blue, Side::Red, a, at(0.), IntelUnitClass::Armor, &cfg, now);
        // Well past the cluster radius: a new contact, and the old one empties.
        let new = db.note_jtac_unit(Side::Blue, Side::Red, a, at(5_000.), IntelUnitClass::Armor, &cfg, now);
        assert_ne!(old, new);
        assert!(!db.contacts.contains_key(&old));
        assert!(db.contacts.contains_key(&new));
        assert_eq!(db.orphaned_marks.len(), 1);
    }

    #[test]
    fn contact_another_sensor_refreshed_is_left_to_decay() {
        let cfg = ElintConfig::default();
        let now = Utc::now();
        let mut db = IntelDatabase::default();
        let a = UnitId::from(1);
        let id = db.note_jtac_unit(Side::Blue, Side::Red, a, at(0.), IntelUnitClass::Armor, &cfg, now);
        // A recon pass confirms the site after the JTAC saw it.
        let id2 = db.upsert(Side::Blue, Side::Red, at(10.), IntelUnitClass::Armor, 4, IntelSource::ReconFlight, &cfg, now);
        assert_eq!(id, id2);
        assert_eq!(db.retire_jtac_units(|_| true), 0);
        assert!(db.contacts.contains_key(&id));
    }

    #[test]
    fn living_units_and_other_sides_are_untouched() {
        let cfg = ElintConfig::default();
        let now = Utc::now();
        let mut db = IntelDatabase::default();
        let a = UnitId::from(1);
        let blue = db.note_jtac_unit(Side::Blue, Side::Red, a, at(0.), IntelUnitClass::Armor, &cfg, now);
        let red = db.note_jtac_unit(Side::Red, Side::Blue, UnitId::from(2), at(0.), IntelUnitClass::Armor, &cfg, now);
        assert_eq!(db.retire_jtac_units(|_| false), 0);
        assert_eq!(db.retire_jtac_units(|u| u == UnitId::from(2)), 1);
        assert!(db.contacts.contains_key(&blue));
        assert!(!db.contacts.contains_key(&red));
    }
}
