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

//! The installed DCS unit database, harvested at runtime.
//!
//! DCS knows every unit's engagement and detection range -- `ThreatRange`,
//! `ThreatRangeMin`, `DetectionRange` -- but removed the `db` global from the
//! *mission scripting* sandbox in 2.7, which is why range tables normally get
//! copied into config by hand and then rot on the next patch.
//!
//! We don't have to do that. `bflib.dll` is loaded into the **hooks** Lua state
//! (see `bflib/lua/hooks.lua`), and that state still has `_G.db` -- it's the
//! same state third-party dumpers like Quaggles' dcs-lua-datamine read. So we
//! walk it ourselves at startup and keep the result in a process-wide table
//! that the mission-state code reads through the shared `Context`.
//!
//! Two consequences worth having: modded units (Currenthill, China Asset Pack,
//! ColdWar) are covered for free because they're in `db` like anything else,
//! and the numbers can't go stale against an installed version, because they
//! *are* the installed version.
//!
//! Config still wins. [`artillery_range`] consults `cfg.artillery.units` first,
//! falls back to what DCS says, and only then to the flat defaults -- so a
//! deliberate gameplay override stays an override, and everything else stops
//! being a maintenance burden.

use anyhow::{anyhow, Result};
use chrono::Utc;
use dcso3::{dcs::Dcs, lfs::Lfs, HooksLua, LuaEnv};
use bfprotocols::cfg::ArtilleryCfg;
use fxhash::FxHashMap;
use log::{info, warn};
use mlua::prelude::*;
use serde_derive::Serialize;
use std::sync::{Arc, RwLock};

/// What we keep for one DCS unit type. Everything is optional because the
/// shape of `db` varies by category -- a supply truck has no `ThreatRange`,
/// a launcher has no `DetectionRange` -- and a missing field must read as
/// "DCS doesn't say", never as zero.
#[derive(Debug, Clone, Default, Serialize)]
pub struct UnitInfo {
    /// DCS type name, i.e. what `Unit.getTypeName()` returns.
    pub typ: String,
    pub display_name: Option<String>,
    /// `db.Units.<Category>` this came from, e.g. "Cars/Car".
    pub category: Option<String>,
    /// The DCS attribute list ("Artillery", "SAM SR", "Indirect fire", ...).
    /// Numeric entries are dropped; only the named attributes are useful here.
    pub attributes: Vec<String>,
    /// Maximum engagement range in metres. Guns and launchers only.
    pub threat_range_m: Option<f64>,
    /// Minimum engagement range in metres. Nonzero for indirect fire and for
    /// ballistic launchers, whose dead zone the flat default badly misses.
    pub threat_range_min_m: Option<f64>,
    /// Radar detection range in metres, for units that have a radar.
    pub detection_range_m: Option<f64>,
    /// Deepest `maxTargetDetectionRange` found under the unit's weapon
    /// systems. Some launchers carry their acquisition range here rather than
    /// in `DetectionRange`.
    pub max_target_detection_range_m: Option<f64>,
}

impl UnitInfo {
    pub fn has_attribute(&self, attr: &str) -> bool {
        self.attributes.iter().any(|a| a == attr)
    }

    /// True when DCS classes this as indirect fire -- the units for which
    /// `threat_range_min_m` actually matters.
    pub fn is_indirect_fire(&self) -> bool {
        self.has_attribute("Indirect fire")
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct UnitDb {
    /// The DCS build this was read out of, e.g. "2.9.29.27468". This is the
    /// version actually running, not the one anybody believes is installed,
    /// which is the whole point of harvesting rather than vendoring: bfdb
    /// keys snapshots on it and diffs when it moves.
    pub dcs_version: Option<String>,
    pub harvested_at: Option<String>,
    by_type: FxHashMap<String, UnitInfo>,
}

impl UnitDb {
    pub fn get(&self, typ: &str) -> Option<&UnitInfo> {
        self.by_type.get(typ)
    }

    pub fn len(&self) -> usize {
        self.by_type.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_type.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &UnitInfo)> {
        self.by_type.iter()
    }
}

static DB: RwLock<Option<Arc<UnitDb>>> = RwLock::new(None);

/// The harvested database, or an empty one if the harvest hasn't run or
/// failed. Callers get an `Arc` so a lookup loop takes the lock once.
pub fn get() -> Arc<UnitDb> {
    match DB.read() {
        Ok(g) => g.clone().unwrap_or_default(),
        // A poisoned lock here means a panic while harvesting. Degrade to the
        // config-only path rather than taking the mission down with us.
        Err(e) => {
            warn!("unit db lock poisoned: {e}");
            Arc::default()
        }
    }
}

pub fn is_loaded() -> bool {
    DB.read().map(|g| g.is_some()).unwrap_or(false)
}

fn set(db: UnitDb) {
    match DB.write() {
        Ok(mut g) => *g = Some(Arc::new(db)),
        Err(e) => warn!("could not store unit db, lock poisoned: {e}"),
    }
}

/// `db.Units` subtrees that are not units. `GT_t` holds the shared ground-tech
/// component tables (chassis, launchers, weapon systems) keyed by the *same*
/// names as the real units, so walking it silently overwrites good entries
/// with rangeless stubs.
const NOT_UNITS: &[&str] = &["GT_t"];

/// How deep to walk below `db.Units` looking for unit tables. Units live at
/// `db.Units.<Category>.<SubCategory>.<index>`, i.e. depth 3; the extra level
/// is slack for categories that nest differently.
const UNIT_SCAN_DEPTH: usize = 4;

/// How deep to walk inside one unit hunting `maxTargetDetectionRange`. It sits
/// under `WS[n].LN[n]...`, which is a few levels down but not unbounded.
const SENSOR_SCAN_DEPTH: usize = 6;

fn opt_f64(t: &LuaTable, key: &str) -> Option<f64> {
    match t.raw_get::<_, LuaValue>(key) {
        Ok(LuaValue::Number(n)) => Some(n),
        Ok(LuaValue::Integer(i)) => Some(i as f64),
        _ => None,
    }
}

fn opt_string(t: &LuaTable, key: &str) -> Option<String> {
    match t.raw_get::<_, LuaValue>(key) {
        Ok(LuaValue::String(s)) => s.to_str().ok().map(|s| s.to_string()),
        _ => None,
    }
}

/// Pull the named attributes out of a DCS `attribute` list. The list mixes
/// leading numeric ids with the names we actually match on, so the numbers get
/// dropped.
fn attributes(t: &LuaTable) -> Vec<String> {
    let mut out = vec![];
    if let Ok(attrs) = t.raw_get::<_, LuaTable>("attribute") {
        for v in attrs.sequence_values::<LuaValue>().flatten() {
            if let LuaValue::String(s) = v {
                if let Ok(s) = s.to_str() {
                    out.push(s.to_string());
                }
            }
        }
    }
    out
}

/// Deepest `maxTargetDetectionRange` anywhere under `t`, bounded by `depth`.
/// `seen` guards against the pointer-shared tables DCS's db is full of.
fn max_target_detection(
    t: &LuaTable,
    depth: usize,
    seen: &mut Vec<*const std::ffi::c_void>,
) -> Option<f64> {
    if depth == 0 {
        return None;
    }
    let ptr = t.to_pointer();
    if seen.contains(&ptr) {
        return None;
    }
    seen.push(ptr);
    let mut best = opt_f64(t, "maxTargetDetectionRange");
    for pair in t.clone().pairs::<LuaValue, LuaValue>().flatten() {
        if let (_, LuaValue::Table(child)) = pair {
            if let Some(v) = max_target_detection(&child, depth - 1, seen) {
                best = Some(best.map_or(v, |b: f64| b.max(v)));
            }
        }
    }
    best
}

/// A unit table is one with a string `type`. Component tables under `GT_t`
/// have one too, which is exactly why `GT_t` is excluded before we get here.
fn parse_unit(t: &LuaTable, category: &str) -> Option<UnitInfo> {
    let typ = opt_string(t, "type")?;
    if typ.is_empty() {
        return None;
    }
    // Only the weapon-system subtree, not the whole unit: that's where
    // maxTargetDetectionRange lives, and a unit table also carries damage
    // models, visuals and armour schemes we'd otherwise walk for nothing --
    // once per unit type, several hundred times, at startup.
    let mut seen = Vec::with_capacity(32);
    let ws = t.raw_get::<_, LuaTable>("WS").ok();
    Some(UnitInfo {
        typ,
        display_name: opt_string(t, "DisplayName").or_else(|| opt_string(t, "Name")),
        category: Some(category.to_string()),
        attributes: attributes(t),
        // DCS writes 0 for "not applicable" on both of these. Keep that as
        // None so a lookup falls through to the configured default instead of
        // claiming a unit can shoot 0m.
        threat_range_m: opt_f64(t, "ThreatRange").filter(|v| *v > 0.0),
        threat_range_min_m: opt_f64(t, "ThreatRangeMin").filter(|v| *v > 0.0),
        detection_range_m: opt_f64(t, "DetectionRange").filter(|v| *v > 0.0),
        max_target_detection_range_m: ws
            .and_then(|ws| max_target_detection(&ws, SENSOR_SCAN_DEPTH, &mut seen))
            .filter(|v| *v > 0.0),
    })
}

fn walk(
    t: &LuaTable,
    path: &str,
    depth: usize,
    out: &mut FxHashMap<String, UnitInfo>,
    seen: &mut Vec<*const std::ffi::c_void>,
) {
    if depth == 0 {
        return;
    }
    let ptr = t.to_pointer();
    if seen.contains(&ptr) {
        return;
    }
    seen.push(ptr);
    for pair in t.clone().pairs::<LuaValue, LuaValue>().flatten() {
        let (k, v) = pair;
        let LuaValue::Table(child) = v else { continue };
        let name = match &k {
            LuaValue::String(s) => s.to_str().unwrap_or("?").to_string(),
            LuaValue::Integer(i) => i.to_string(),
            LuaValue::Number(n) => n.to_string(),
            _ => continue,
        };
        if depth == UNIT_SCAN_DEPTH && NOT_UNITS.contains(&name.as_str()) {
            continue;
        }
        match parse_unit(&child, path) {
            Some(info) => {
                out.insert(info.typ.clone(), info);
            }
            None => {
                let path = if path.is_empty() {
                    name
                } else {
                    format!("{path}/{name}")
                };
                walk(&child, &path, depth - 1, out, seen)
            }
        }
    }
}

/// Read `_G.db.Units` out of the hooks Lua state.
///
/// Returns an error rather than an empty db when `db` isn't reachable, so the
/// caller can tell "DCS changed on us" from "this install has no units".
pub fn harvest<'lua, L: LuaEnv<'lua>>(lua: L) -> Result<UnitDb> {
    let lua = lua.inner();
    // Not `.context()`: both anyhow and mlua's ErrorContext are in scope here
    // and the call is ambiguous.
    let db: LuaTable = lua
        .globals()
        .raw_get("db")
        .map_err(|e| anyhow!("_G.db is not reachable from this lua state: {e}"))?;
    let units: LuaTable = db
        .raw_get("Units")
        .map_err(|e| anyhow!("_G.db.Units is missing: {e}"))?;
    let mut by_type = FxHashMap::default();
    let mut seen = Vec::with_capacity(256);
    walk(&units, "", UNIT_SCAN_DEPTH, &mut by_type, &mut seen);
    Ok(UnitDb {
        dcs_version: None,
        harvested_at: None,
        by_type,
    })
}

/// Harvest and store, and drop a `Logs/unitdb.json` alongside the DCS logs so
/// the numbers can be eyeballed (and diffed across DCS updates) without a
/// running mission. A failure here is never fatal: every consumer falls back
/// to config.
pub fn init(lua: HooksLua) -> Result<()> {
    let mut db = harvest(lua)?;
    if db.is_empty() {
        warn!("unit db harvest found no units, falling back to config ranges");
        return Ok(());
    }
    db.dcs_version = Dcs::singleton(lua)
        .and_then(|dcs| dcs.get_version())
        .map(|v| v.to_string())
        .map_err(|e| warn!("could not read the DCS version: {e:?}"))
        .ok();
    db.harvested_at = Some(Utc::now().to_rfc3339());
    let arty = db.iter().filter(|(_, i)| i.is_indirect_fire()).count();
    let radar = db
        .iter()
        .filter(|(_, i)| i.detection_range_m.is_some())
        .count();
    info!(
        "unit db harvested from DCS {}: {} types ({arty} indirect fire, {radar} with radar)",
        db.dcs_version.as_deref().unwrap_or("unknown"),
        db.len()
    );
    if let Ok(write_dir) = Lfs::singleton(lua).and_then(|l| l.writedir()) {
        let path = std::path::PathBuf::from(write_dir.as_str())
            .join("Logs")
            .join("unitdb.json");
        match serde_json::to_vec_pretty(&db) {
            Ok(body) => {
                if let Err(e) = std::fs::write(&path, body) {
                    warn!("could not write unit db dump {path:?}: {e:?}");
                }
            }
            Err(e) => warn!("could not serialize unit db: {e:?}"),
        }
    }
    set(db);
    Ok(())
}

/// Effective (max, min) engagement range in metres for an artillery unit type.
///
/// Precedence is config, then DCS, then the flat defaults. The config layer is
/// first on purpose -- an entry in `cfg.artillery.units` is a deliberate
/// gameplay decision (or a correction to a DCS value we don't like) and must
/// not be silently overridden by the harvest.
pub fn artillery_range(cfg: Option<&ArtilleryCfg>, typ: &str) -> (f64, f64) {
    let (default_max, default_min) = cfg
        .map(|c| (c.default_max_range_m, c.default_min_range_m))
        .unwrap_or((30_000.0, 4_000.0));
    if let Some(r) = cfg.and_then(|c| c.units.get(typ)) {
        return (r.max_range_m, r.min_range_m);
    }
    if let Some(info) = get().get(typ) {
        if let Some(max) = info.threat_range_m {
            return (max, info.threat_range_min_m.unwrap_or(0.0));
        }
    }
    (default_max, default_min)
}
