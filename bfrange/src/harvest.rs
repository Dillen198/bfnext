// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Data read from DCS's own databases in the hooks Lua state.
//!
//! The mission sandbox lost `_G.db` in 2.7, but the hooks state (where
//! `bfrange.dll` is also loaded) still has it, plus the weapon tables. We read
//! what the range needs once at startup and keep it process-wide, the same
//! way bflib's `unitdb` does:
//!
//! - bomb ballistic data (`_G.bombs`, `weapons_table`) for the release
//!   calculator on the range site, and
//! - each aircraft type's maximum internal fuel (`db.Units.Planes/Helicopters`),
//!   which turns `Unit:getFuel()`'s fraction into kilograms for AAR grading.

use anyhow::{anyhow, Result};
use bfprotocols::range::{WeaponBallistics, WeaponDb};
use dcso3::{dcs::Dcs, HooksLua, LuaEnv};
use fxhash::FxHashMap;
use log::{info, warn};
use mlua::prelude::*;
use std::sync::{Arc, RwLock};

#[derive(Debug, Default)]
pub struct Harvest {
    pub weapons: WeaponDb,
    /// DCS type -> max internal fuel, kg
    pub max_fuel_kg: FxHashMap<String, f64>,
    /// DCS type -> [(pylon number, CLSIDs that pylon accepts)]
    pub pylons: FxHashMap<String, Vec<(u8, Vec<String>)>>,
    /// CLSID -> display name ("R-77", "SD-10", "AIM-120C" ...), from
    /// `db.Weapons`, so loadouts can be picked by what a store IS rather
    /// than by a guessed CLSID
    pub clsid_names: FxHashMap<String, String>,
}

static HARVEST: RwLock<Option<Arc<Harvest>>> = RwLock::new(None);

pub fn get() -> Option<Arc<Harvest>> {
    HARVEST.read().ok().and_then(|h| h.clone())
}

pub fn max_fuel_kg(typ: &str) -> Option<f64> {
    get().and_then(|h| h.max_fuel_kg.get(typ).copied())
}

/// Build a loadout by placing, in preference order, the first CLSIDs from
/// `wanted` that each pylon of `typ` actually accepts, up to `max` stores.
/// Anything DCS's own pylon table doesn't list is skipped, so a guessed CLSID
/// can never produce an illegal loadout.
pub fn auto_loadout(typ: &str, wanted: &[&str], max: usize) -> std::collections::BTreeMap<u8, String> {
    let mut out = std::collections::BTreeMap::new();
    let Some(h) = get() else { return out };
    let Some(pylons) = h.pylons.get(typ) else { return out };
    for want in wanted {
        for (n, clsids) in pylons {
            if out.len() >= max {
                return out;
            }
            if !out.contains_key(n) && clsids.iter().any(|c| c.eq_ignore_ascii_case(want)) {
                out.insert(*n, want.to_string());
            }
        }
    }
    out
}

/// Air-to-air missile classes, matched against DCS display names. Fox 1 is
/// semi-active radar (R-27R/ER, AIM-7, Super 530), Fox 2 infrared, Fox 3
/// active radar. The IR R-27T/ET are Fox 2s.
pub fn aam_class(name: &str) -> Option<&'static str> {
    let n = name.to_ascii_uppercase().replace(' ', "");
    let has = |k: &[&str]| k.iter().any(|x| n.contains(x));
    if has(&[
        "R-27T", "R-27ET", "R-73", "R-60", "R-3S", "R-13", "AIM-9", "AIM9", "PL-5", "PL-8", "PL-9",
        "MAGIC", "MICA-IR", "IRIS-T", "ASRAAM", "PYTHON", "SHAFRIR", "R550",
    ]) {
        Some("fox2")
    } else if has(&[
        "R-27R", "R-27ER", "R-27", "AIM-7", "AIM7", "SUPER530", "SUPER_530", "SKYFLASH", "ASPIDE",
        "R-24R", "R-23R",
    ]) {
        Some("fox1")
    } else if has(&[
        "R-77", "AIM-120", "AIM120", "SD-10", "SD10", "PL-12", "PL12", "PL-15", "PL15", "METEOR",
        "MICA-EM", "MICA_EM", "R-37", "AIM-54", "AIM54",
    ]) {
        Some("fox3")
    } else {
        None
    }
}

/// Loadout of `class` ("fox1", "fox2", "fox3") for `typ`, chosen from the
/// stores its pylons accept by their DCS names, `max` stores.
pub fn loadout_by_class(typ: &str, class: &str, max: usize) -> std::collections::BTreeMap<u8, String> {
    let mut out = std::collections::BTreeMap::new();
    let Some(h) = get() else { return out };
    let Some(pylons) = h.pylons.get(typ) else { return out };
    for (n, clsids) in pylons {
        if out.len() >= max {
            break;
        }
        if let Some(c) = clsids
            .iter()
            .find(|c| h.clsid_names.get(*c).and_then(|nm| aam_class(nm)) == Some(class))
        {
            out.insert(*n, c.clone());
        }
    }
    out
}

fn num(t: &LuaTable, k: &str) -> Option<f64> {
    t.raw_get::<_, Option<f64>>(k).ok().flatten()
}

fn bomb_entry(name: &str, t: &LuaTable) -> Option<WeaponBallistics> {
    let fm: LuaTable = t.raw_get::<_, Option<LuaTable>>("fm").ok().flatten()?;
    let mass_kg = num(&fm, "mass")?;
    let caliber_m = num(&fm, "caliber").unwrap_or(0.);
    let length_m = num(&fm, "L").unwrap_or(0.);
    let cx_coeff = fm
        .raw_get::<_, Option<LuaTable>>("cx_coeff")
        .ok()
        .flatten()
        .map(|c| c.sequence_values::<f64>().filter_map(|v| v.ok()).collect())
        .unwrap_or_default();
    let char_time_s = t
        .raw_get::<_, Option<LuaTable>>("targeting_data")
        .ok()
        .flatten()
        .and_then(|td| num(&td, "char_time"));
    let display_name = ["user_name", "display_name", "displayName"]
        .iter()
        .find_map(|k| t.raw_get::<_, Option<String>>(*k).ok().flatten())
        .unwrap_or_default();
    let guided = ["laser", "seeker", "autopilot", "guidance", "control_block"]
        .iter()
        .any(|k| !matches!(t.raw_get::<_, LuaValue>(*k), Ok(LuaValue::Nil) | Err(_)));
    Some(WeaponBallistics {
        name: name.to_string(),
        display_name,
        mass_kg,
        caliber_m,
        length_m,
        cx_coeff,
        char_time_s,
        class: if guided { "guided".into() } else { "unguided".into() },
    })
}

fn harvest_bombs(g: &LuaTable) -> Vec<WeaponBallistics> {
    let mut out: Vec<WeaponBallistics> = vec![];
    let mut scan = |tbl: LuaTable| {
        for pair in tbl.pairs::<LuaValue, LuaValue>() {
            let Ok((k, v)) = pair else { continue };
            let LuaValue::Table(t) = v else { continue };
            let name = match k {
                LuaValue::String(s) => s.to_str().map(|s| s.to_string()).ok(),
                _ => t.raw_get::<_, Option<String>>("name").ok().flatten(),
            };
            if let Some(name) = name {
                if let Some(b) = bomb_entry(&name, &t) {
                    if !out.iter().any(|x| x.name == b.name) {
                        out.push(b)
                    }
                }
            }
        }
    };
    if let Ok(Some(b)) = g.raw_get::<_, Option<LuaTable>>("bombs") {
        scan(b)
    }
    if let Ok(Some(wt)) = g.raw_get::<_, Option<LuaTable>>("weapons_table") {
        if let Ok(Some(w)) = wt.raw_get::<_, Option<LuaTable>>("weapons") {
            if let Ok(Some(b)) = w.raw_get::<_, Option<LuaTable>>("bombs") {
                scan(b)
            }
        }
    }
    out.sort_by(|a, b| a.name.cmp(&b.name));
    out
}

fn harvest_pylons(g: &LuaTable) -> FxHashMap<String, Vec<(u8, Vec<String>)>> {
    let mut m = FxHashMap::default();
    let Ok(Some(db)) = g.raw_get::<_, Option<LuaTable>>("db") else { return m };
    let Ok(Some(units)) = db.raw_get::<_, Option<LuaTable>>("Units") else { return m };
    for (cat, sub) in [("Planes", "Plane"), ("Helicopters", "Helicopter")] {
        let Ok(Some(c)) = units.raw_get::<_, Option<LuaTable>>(cat) else { continue };
        let Ok(Some(list)) = c.raw_get::<_, Option<LuaTable>>(sub) else { continue };
        for v in list.sequence_values::<LuaTable>() {
            let Ok(t) = v else { continue };
            let Some(typ) = t.raw_get::<_, Option<String>>("type").ok().flatten() else { continue };
            let Some(pyl) = t.raw_get::<_, Option<LuaTable>>("Pylons").ok().flatten() else { continue };
            let mut ps = vec![];
            for p in pyl.sequence_values::<LuaTable>() {
                let Ok(p) = p else { continue };
                let Some(n) = p.raw_get::<_, Option<i64>>("Number").ok().flatten() else { continue };
                let mut clsids = vec![];
                if let Some(ls) = p.raw_get::<_, Option<LuaTable>>("Launchers").ok().flatten() {
                    for l in ls.sequence_values::<LuaTable>() {
                        if let Ok(l) = l {
                            if let Some(c) = l.raw_get::<_, Option<String>>("CLSID").ok().flatten() {
                                clsids.push(c);
                            }
                        }
                    }
                }
                ps.push((n as u8, clsids));
            }
            m.insert(typ, ps);
        }
    }
    m
}

fn harvest_clsid_names(g: &LuaTable) -> FxHashMap<String, String> {
    let mut m = FxHashMap::default();
    let Ok(Some(db)) = g.raw_get::<_, Option<LuaTable>>("db") else { return m };
    let Ok(Some(w)) = db.raw_get::<_, Option<LuaTable>>("Weapons") else { return m };
    let name_of = |t: &LuaTable| -> Option<String> {
        ["displayName", "Name", "name"]
            .iter()
            .find_map(|k| t.raw_get::<_, Option<String>>(*k).ok().flatten())
    };
    if let Ok(Some(by)) = w.raw_get::<_, Option<LuaTable>>("ByCLSID") {
        for (clsid, t) in by.pairs::<String, LuaTable>().flatten() {
            if let Some(n) = name_of(&t) {
                m.insert(clsid, n);
            }
        }
    }
    if let Ok(Some(cats)) = w.raw_get::<_, Option<LuaTable>>("Categories") {
        for c in cats.sequence_values::<LuaTable>().flatten() {
            let Some(ls) = c.raw_get::<_, Option<LuaTable>>("Launchers").ok().flatten() else { continue };
            for l in ls.sequence_values::<LuaTable>().flatten() {
                if let (Some(clsid), Some(n)) = (l.raw_get::<_, Option<String>>("CLSID").ok().flatten(), name_of(&l)) {
                    m.entry(clsid).or_insert(n);
                }
            }
        }
    }
    m
}

fn harvest_fuel(g: &LuaTable) -> FxHashMap<String, f64> {
    let mut m = FxHashMap::default();
    let Ok(Some(db)) = g.raw_get::<_, Option<LuaTable>>("db") else { return m };
    let Ok(Some(units)) = db.raw_get::<_, Option<LuaTable>>("Units") else { return m };
    for (cat, sub) in [("Planes", "Plane"), ("Helicopters", "Helicopter")] {
        let Ok(Some(c)) = units.raw_get::<_, Option<LuaTable>>(cat) else { continue };
        let Ok(Some(list)) = c.raw_get::<_, Option<LuaTable>>(sub) else { continue };
        for v in list.sequence_values::<LuaTable>() {
            let Ok(t) = v else { continue };
            let Some(typ) = t.raw_get::<_, Option<String>>("type").ok().flatten() else { continue };
            if let Some(f) = num(&t, "MaxFuelWeight") {
                m.insert(typ, f);
            }
        }
    }
    m
}

/// Walk the hooks state's tables. Returns an error when they aren't there
/// yet (too early in startup) so the caller can retry.
pub fn init(lua: HooksLua) -> Result<()> {
    let g = lua.inner().globals();
    let bombs = harvest_bombs(&g);
    let max_fuel_kg = harvest_fuel(&g);
    let pylons = harvest_pylons(&g);
    let clsid_names = harvest_clsid_names(&g);
    if bombs.is_empty() && max_fuel_kg.is_empty() {
        return Err(anyhow!("DCS databases are not populated yet"));
    }
    let dcs_version = Dcs::singleton(lua)
        .and_then(|d| d.get_version())
        .map(|v| v.to_string())
        .unwrap_or_default();
    info!(
        "harvested {} bomb types, max fuel for {} and pylon tables for {} aircraft types, {} store names (DCS {dcs_version})",
        bombs.len(),
        max_fuel_kg.len(),
        pylons.len(),
        clsid_names.len()
    );
    let h = Harvest { weapons: WeaponDb { dcs_version, bombs }, max_fuel_kg, pylons, clsid_names };
    match HARVEST.write() {
        Ok(mut w) => *w = Some(Arc::new(h)),
        Err(_) => warn!("harvest lock poisoned"),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::aam_class;

    #[test]
    fn missile_classes() {
        assert_eq!(aam_class("R-27ER"), Some("fox1"));
        assert_eq!(aam_class("R-27R"), Some("fox1"));
        assert_eq!(aam_class("R-27ET"), Some("fox2"));
        assert_eq!(aam_class("AIM-7M"), Some("fox1"));
        assert_eq!(aam_class("R-77"), Some("fox3"));
        assert_eq!(aam_class("SD-10"), Some("fox3"));
        assert_eq!(aam_class("PL-12"), Some("fox3"));
        assert_eq!(aam_class("AIM-120C"), Some("fox3"));
        assert_eq!(aam_class("R-73"), Some("fox2"));
        assert_eq!(aam_class("PL-5EII"), Some("fox2"));
        assert_eq!(aam_class("Mk-82"), None);
    }
}
