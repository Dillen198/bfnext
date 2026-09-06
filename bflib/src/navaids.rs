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

//! Auto-generated navaids for the objective kinds DCS gives nothing:
//! FARP / FOB / Logistics Hub / Naval Base / Carrier Group. Real airbases are
//! deliberately excluded -- the terrain already carries their ILS/TACAN/VOR/NDB
//! and the scripting API can't read those to avoid a collision.
//!
//! [`reallocate`] is a deterministic pure function over the persisted state: it
//! sorts the eligible objectives by id and hands out the lowest free TACAN
//! channel (from the owning side's pool) / NDB frequency / ICLS channel that no
//! other navaid within `min_separation_nm` already holds. Re-running it after
//! the objective set changes is safe and idempotent.
//!
//! Broadcasting is separate: [`activate_on_group`] / [`activate_carrier`] issue
//! the DCS controller commands, and they are called from the group spawn path
//! so a culled+respawned host re-lights its beacon.

use crate::db::{
    objective::{ObjGroupClass, Objective},
    persisted::Persisted,
    MapS,
};
use anyhow::Result;
use bfprotocols::{
    cfg::{NavRange, NavaidsCfg},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
};
use compact_str::{format_compact, CompactString};
use dcso3::{
    coalition::Side,
    controller::{BeaconSystem, BeaconType, Command},
    group::Group,
    unit::Unit,
    String as LuaString, Vector2,
};
use serde_derive::{Deserialize, Serialize};

const NM_M: f64 = 1852.0;

fn default_band() -> String {
    "Y".to_string()
}

/// A navaid assignment persisted against an objective. `tacan_band` is stored as
/// a plain "X"/"Y" string to keep the persisted shape trivial.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Navaid {
    /// Ground objective: `None`. Carrier task force: the ship this set is for
    /// (a task force may hold several carriers, one `Navaid` each).
    #[serde(default)]
    pub label: Option<String>,
    /// Carrier decks: the DCS unit name to resolve for ICLS/ACLS/Link-4.
    #[serde(default)]
    pub deck_unit: Option<String>,
    /// The objective group whose controller broadcasts the ground beacon.
    /// `None` for carriers (the carrier unit hosts its own).
    #[serde(default)]
    pub host_gid: Option<GroupId>,
    #[serde(default)]
    pub tacan_channel: Option<u16>,
    #[serde(default = "default_band")]
    pub tacan_band: String,
    #[serde(default)]
    pub ndb_khz: Option<u16>,
    #[serde(default)]
    pub icls_channel: Option<u8>,
    /// Link-4 frequency in MHz (carriers only).
    #[serde(default)]
    pub link4_mhz: Option<f64>,
    #[serde(default)]
    pub acls: bool,
    /// Morse station identifier (2-3 chars).
    #[serde(default)]
    pub morse: CompactString,
}

impl Navaid {
    fn empty() -> Self {
        Navaid {
            label: None,
            deck_unit: None,
            host_gid: None,
            tacan_channel: None,
            tacan_band: default_band(),
            ndb_khz: None,
            icls_channel: None,
            link4_mhz: None,
            acls: false,
            morse: CompactString::from(""),
        }
    }

    /// True if this assignment carries nothing worth persisting/broadcasting.
    fn is_empty(&self) -> bool {
        self.tacan_channel.is_none() && self.ndb_khz.is_none() && self.icls_channel.is_none()
    }

    pub fn tacan_band_enum(&self) -> dcso3::controller::TacanBand {
        match self.tacan_band.as_str() {
            "X" | "x" => dcso3::controller::TacanBand::X,
            _ => dcso3::controller::TacanBand::Y,
        }
    }

    /// Compact one-line summary for F10 reports, e.g.
    /// `TACAN 74Y KUT  NDB 375  ICLS 3  ACLS`.
    pub fn summary(&self) -> CompactString {
        let mut s = CompactString::from("");
        if let Some(ch) = self.tacan_channel {
            s.push_str(&format_compact!("TACAN {ch}{} {}  ", self.tacan_band, self.morse));
        }
        if let Some(khz) = self.ndb_khz {
            s.push_str(&format_compact!("NDB {khz}  "));
        }
        if let Some(icls) = self.icls_channel {
            s.push_str(&format_compact!("ICLS {icls}  "));
        }
        if let Some(mhz) = self.link4_mhz {
            s.push_str(&format_compact!("Link4 {mhz:.1}  "));
        }
        if self.acls {
            s.push_str("ACLS  ");
        }
        let t = s.trim_end();
        if t.is_empty() {
            CompactString::from("none")
        } else {
            CompactString::from(t)
        }
    }
}

/// One-line-per-entry summary for a whole objective's navaid set. A ground
/// objective has one entry; a carrier task force has one per ship (prefixed
/// with the ship label).
pub fn summarize(navs: &[Navaid]) -> CompactString {
    if navs.is_empty() {
        return CompactString::from("none");
    }
    if navs.len() == 1 && navs[0].label.is_none() {
        return navs[0].summary();
    }
    let mut out = CompactString::from("");
    for n in navs {
        if !out.is_empty() {
            out.push('\n');
        }
        match &n.label {
            Some(l) => out.push_str(&format_compact!("{l}: {}", n.summary())),
            None => out.push_str(&n.summary()),
        }
    }
    out
}

#[derive(Clone, Copy, PartialEq)]
enum Want {
    TacanNdb,
    NdbOnly,
    Carrier,
    None,
}

fn wants(kind: &ObjectiveKind, cfg: &NavaidsCfg) -> Want {
    match kind {
        ObjectiveKind::Farp { .. } | ObjectiveKind::Logistics | ObjectiveKind::NavalBase => {
            Want::TacanNdb
        }
        ObjectiveKind::Fob if cfg.ndb_on_fob => Want::NdbOnly,
        ObjectiveKind::CarrierGroup { .. } => Want::Carrier,
        _ => Want::None,
    }
}

fn dist_nm(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm() / NM_M
}

fn morse_of(name: &str) -> CompactString {
    let s: std::string::String = name
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .take(3)
        .collect();
    if s.is_empty() {
        CompactString::from("NAV")
    } else {
        CompactString::from(s.to_uppercase())
    }
}

struct Assigned {
    pos: Vector2,
    nav: Navaid,
}

fn pick_tacan(side: Side, pos: Vector2, cfg: &NavaidsCfg, done: &[Assigned]) -> Option<u16> {
    let pool: NavRange = match side {
        Side::Red => cfg.red_tacan,
        _ => cfg.blue_tacan,
    };
    (pool.lo..=pool.hi).find(|ch| {
        !done.iter().any(|a| {
            a.nav.tacan_channel == Some(*ch) && dist_nm(pos, a.pos) < cfg.min_separation_nm
        })
    })
}

fn pick_ndb(pos: Vector2, cfg: &NavaidsCfg, done: &[Assigned]) -> Option<u16> {
    (cfg.ndb_khz.lo..=cfg.ndb_khz.hi).step_by(5).find(|khz| {
        !done
            .iter()
            .any(|a| a.nav.ndb_khz == Some(*khz) && dist_nm(pos, a.pos) < cfg.min_separation_nm)
    })
}

fn pick_icls(done: &[Assigned]) -> Option<u8> {
    (1u8..=20).find(|c| !done.iter().any(|a| a.nav.icls_channel == Some(*c)))
}

fn dedupe_morse(base: CompactString, pos: Vector2, cfg: &NavaidsCfg, done: &[Assigned]) -> CompactString {
    let collides = |m: &str| {
        done.iter()
            .any(|a| a.nav.morse == m && dist_nm(pos, a.pos) < cfg.min_separation_nm)
    };
    if !collides(&base) {
        return base;
    }
    let stem: std::string::String = base.chars().take(2).collect();
    for d in 1..=9 {
        let cand = format_compact!("{stem}{d}");
        if !collides(&cand) {
            return cand;
        }
    }
    base
}

/// Priority of an objective group as a ground-beacon host. Lower is better.
/// Logi groups are picked first: they are the last to fall, so the beacon
/// tracks the base's usefulness. Infantry/Services groups have no useful
/// controller and are skipped.
fn host_priority(class: ObjGroupClass) -> Option<u8> {
    match class {
        ObjGroupClass::Logi => Some(0),
        ObjGroupClass::Aaa => Some(1),
        ObjGroupClass::Armor => Some(2),
        ObjGroupClass::Sr => Some(3),
        ObjGroupClass::Mr => Some(4),
        ObjGroupClass::Lr => Some(5),
        ObjGroupClass::Naval => Some(6),
        ObjGroupClass::Other => Some(7),
        ObjGroupClass::Infantry | ObjGroupClass::Services => None,
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum DeckClass {
    /// cats & traps: TACAN + ICLS + ACLS + Link-4
    Catobar,
    /// amphib flat-top: TACAN + ICLS only
    Helo,
}

fn deck_class(typ: &str, cfg: &NavaidsCfg) -> Option<DeckClass> {
    if cfg.western_carrier_types.iter().any(|w| typ.contains(w.as_str())) {
        Some(DeckClass::Catobar)
    } else if cfg.helo_carrier_types.iter().any(|w| typ.contains(w.as_str())) {
        Some(DeckClass::Helo)
    } else {
        None
    }
}

/// Every aircraft-carrying ship in a task force, as `(unit name, class)`. A
/// task force can hold several (e.g. two CVNs + an LHA) -- each gets its own
/// navaid set. Keyed by ship type, not coalition, so it survives a capture.
fn carrier_decks(
    persisted: &Persisted,
    oid: &ObjectiveId,
    cfg: &NavaidsCfg,
) -> Vec<(std::string::String, DeckClass)> {
    let mut decks = vec![];
    let Some(obj) = persisted.objectives.get(oid) else { return decks };
    for (_, gids) in obj.groups() {
        for gid in gids {
            let Some(g) = persisted.groups.get(gid) else { continue };
            for uid in g.units.into_iter() {
                let Some(u) = persisted.units.get(uid) else { continue };
                if let Some(class) = deck_class(u.typ.0.as_str(), cfg) {
                    decks.push((u.template_name.to_string(), class));
                }
            }
        }
    }
    decks.sort();
    decks.dedup();
    decks
}

fn pick_host(persisted: &Persisted, obj: &Objective) -> Option<GroupId> {
    let groups = obj.groups().get(&obj.owner())?;
    let mut cands: Vec<(u8, GroupId)> = groups
        .into_iter()
        .filter_map(|gid| {
            let g = persisted.groups.get(gid)?;
            host_priority(g.class).map(|p| (p, *gid))
        })
        .collect();
    cands.sort();
    cands.first().map(|(_, g)| *g)
}

/// Deterministically (re)assign navaids for every eligible objective. A ground
/// objective gets one entry; a carrier task force gets one per aircraft-carrying
/// ship. Returns the objectives whose assignment changed.
pub fn reallocate(persisted: &mut Persisted, cfg: &NavaidsCfg) -> Vec<ObjectiveId> {
    let old: MapS<ObjectiveId, Vec<Navaid>> = persisted.navaids.clone();

    if !cfg.enabled {
        persisted.navaids = MapS::new();
        return old.into_iter().map(|(k, _)| *k).collect();
    }

    let band = if cfg.tacan_band == dcso3::controller::TacanBand::X { "X" } else { "Y" };

    // Eligible objectives, sorted by id for a stable assignment order.
    let mut elig: Vec<(ObjectiveId, Side, Vector2, CompactString, Want)> = persisted
        .objectives
        .into_iter()
        .filter_map(|(oid, o)| match wants(o.kind(), cfg) {
            Want::None => None,
            w => Some((*oid, o.owner(), o.pos(), CompactString::from(o.name()), w)),
        })
        .collect();
    elig.sort_by_key(|e| e.0);

    let mut done: Vec<Assigned> = Vec::with_capacity(elig.len());
    let mut out: Vec<(ObjectiveId, Vec<Navaid>)> = Vec::with_capacity(elig.len());

    for (oid, side, pos, name, want) in &elig {
        let mut set: Vec<Navaid> = vec![];
        match want {
            Want::TacanNdb | Want::NdbOnly => {
                let mut nav = Navaid::empty();
                nav.tacan_band = band.to_string();
                // Russian-pattern aircraft home on ADF/ARK, not TACAN -- a
                // red-owned ground objective gets an NDB only. Follows the
                // current owner, so a captured base flips.
                let ground_tacan = *want == Want::TacanNdb
                    && !(cfg.red_ground_ndb_only && *side == Side::Red);
                if ground_tacan {
                    nav.tacan_channel = pick_tacan(*side, *pos, cfg, &done);
                }
                if cfg.ndb_enabled {
                    nav.ndb_khz = pick_ndb(*pos, cfg, &done);
                }
                if !nav.is_empty() {
                    nav.morse = dedupe_morse(morse_of(name), *pos, cfg, &done);
                    nav.host_gid = persisted
                        .objectives
                        .get(oid)
                        .and_then(|o| pick_host(persisted, o));
                    done.push(Assigned { pos: *pos, nav: nav.clone() });
                    set.push(nav);
                }
            }
            Want::Carrier => {
                for (deck_name, class) in carrier_decks(persisted, oid, cfg) {
                    let mut nav = Navaid::empty();
                    nav.tacan_band = band.to_string();
                    nav.label = Some(deck_name.clone());
                    nav.deck_unit = Some(deck_name.clone());
                    nav.tacan_channel = pick_tacan(*side, *pos, cfg, &done);
                    if cfg.carrier_icls {
                        nav.icls_channel = pick_icls(&done);
                    }
                    if class == DeckClass::Catobar {
                        nav.acls = cfg.carrier_acls;
                        if cfg.carrier_link4_mhz > 0.0 {
                            nav.link4_mhz = Some(cfg.carrier_link4_mhz);
                        }
                    }
                    if nav.is_empty() {
                        continue;
                    }
                    nav.morse = dedupe_morse(morse_of(&deck_name), *pos, cfg, &done);
                    done.push(Assigned { pos: *pos, nav: nav.clone() });
                    set.push(nav);
                }
            }
            Want::None => {}
        }
        if !set.is_empty() {
            out.push((*oid, set));
        }
    }

    let mut new = MapS::new();
    for (oid, set) in out {
        new.insert_cow(oid, set);
    }

    // changed set = symmetric difference by a cheap fingerprint of the whole set
    let mut changed: Vec<ObjectiveId> = vec![];
    for (oid, set) in &new {
        if old.get(oid).map(|s| fingerprint(s)) != Some(fingerprint(set)) {
            changed.push(*oid);
        }
    }
    for (oid, _) in &old {
        if new.get(oid).is_none() {
            changed.push(*oid);
        }
    }

    persisted.navaids = new;
    changed
}

fn fingerprint(navs: &[Navaid]) -> Vec<(Option<u16>, Option<u16>, Option<u8>, bool, Option<u64>)> {
    navs.iter()
        .map(|n| {
            (
                n.tacan_channel,
                n.ndb_khz,
                n.icls_channel,
                n.acls,
                n.link4_mhz.map(|f| f.to_bits()),
            )
        })
        .collect()
}

// ── broadcasting ───────────────────────────────────────────────────────────

fn tacan_command(nav: &Navaid) -> Option<Command> {
    let ch = nav.tacan_channel?;
    Some(Command::ActivateBeacon {
        typ: BeaconType::TACAN,
        system: BeaconSystem::TACAN,
        name: None,
        callsign: LuaString::from(nav.morse.as_str()),
        // DCS derives the RF frequency from channel + mode_channel; this is the
        // same placeholder the AI-aircraft TACAN task uses.
        frequency: 1_088_000_000,
        channel: Some(ch as i64),
        mode_channel: Some(nav.tacan_band_enum()),
        aa: Some(false),
        bearing: Some(true),
    })
}

fn ndb_command(nav: &Navaid) -> Option<Command> {
    let khz = nav.ndb_khz?;
    Some(Command::ActivateBeacon {
        typ: BeaconType::NauticalHomer,
        system: BeaconSystem::PAR10,
        name: None,
        callsign: LuaString::from(nav.morse.as_str()),
        frequency: khz as i64 * 1000,
        channel: None,
        mode_channel: None,
        aa: Some(false),
        bearing: Some(true),
    })
}

/// Broadcast a ground objective's beacons from `group`'s controller. Best
/// effort -- a failure here must never abort a spawn, so callers log and move on.
pub fn activate_on_group(group: &Group, nav: &Navaid) -> Result<()> {
    let con = group.get_controller()?;
    if let Some(cmd) = tacan_command(nav) {
        con.set_command(cmd)?;
    }
    if let Some(cmd) = ndb_command(nav) {
        con.set_command(cmd)?;
    }
    Ok(())
}

/// Broadcast one carrier deck's navaids. Each ship in a task force is lit
/// individually on its own unit controller (TACAN/ICLS/ACLS/Link-4 are all
/// per-ship in DCS), so a multi-carrier group gets independent sets.
pub fn activate_carrier(deck: &Unit, nav: &Navaid) -> Result<()> {
    let id = deck.id()?;
    let con = deck.get_controller()?;
    if let Some(cmd) = tacan_command(nav) {
        con.set_command(cmd)?;
    }
    if let Some(ch) = nav.icls_channel {
        con.set_command(Command::ActivateICLS { channel: ch as i64, unit: id, name: None })?;
    }
    if nav.acls {
        con.set_command(Command::ActivateACLS { unit: id, name: None })?;
    }
    if let Some(mhz) = nav.link4_mhz {
        con.set_command(Command::ActivateLink4 {
            unit: id,
            frequency: (mhz * 1_000_000.0) as i64,
            name: None,
        })?;
    }
    Ok(())
}
