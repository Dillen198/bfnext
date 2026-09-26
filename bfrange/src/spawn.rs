// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Building DCS group/static tables and keeping track of what the engine
//! spawned, for whom, and until when.
//!
//! Groups are built as JSON in the Mission Editor's own shape and converted
//! to Lua by `util::json_to_lua`, which is far easier to read and check than
//! typed builders, and gives full control over payloads, callsigns and route
//! tasks. Controller commands (TACAN, frequency, immortal ...) must not be
//! issued in the same frame as `addGroup` (DCS can crash), so they are queued
//! as `Pending` actions and run a second or two later.

use crate::util::{self, json_to_lua, V3};
use anyhow::{anyhow, bail, Result};
use chrono::{DateTime, Duration, Utc};
use dcso3::{coalition::Side, country::Country, LuaEnv, MizLua};
use fxhash::FxHashMap;
use log::{info, warn};
use mlua::prelude::*;
use serde_json::{json, Value as J};
use std::collections::BTreeMap;

pub const AIRPLANE: i64 = 0;
pub const HELICOPTER: i64 = 1;
pub const GROUND: i64 = 2;
pub const SHIP: i64 = 3;

/// DCS country id from a config name: "CJTF Blue", "cjtf_blue", "USA".
pub fn country_id(name: &str) -> Result<i64> {
    let norm = name.trim().to_ascii_uppercase().replace([' ', '-'], "_");
    let c: Country = serde_json::from_value(J::String(norm.clone()))
        .map_err(|_| anyhow!("unknown DCS country {name:?} ({norm})"))?;
    Ok(c as u8 as i64)
}

pub fn side_of_str(s: &str) -> Side {
    match s.to_ascii_lowercase().as_str() {
        "blue" => Side::Blue,
        "neutral" | "neutrals" => Side::Neutral,
        _ => Side::Red,
    }
}

/// The default country for AI on a side.
pub fn default_country(side: Side) -> &'static str {
    match side {
        Side::Blue => "CJTF_BLUE",
        Side::Red => "CJTF_RED",
        Side::Neutral => "UN_PEACEKEEPERS",
    }
}

/// A route point. `tasks` are wrapped in a ComboTask.
pub fn waypoint(p: V3, alt_m: f64, speed_ms: f64, tasks: Vec<J>) -> J {
    json!({
        "x": p.x, "y": p.z,
        "alt": alt_m, "alt_type": "BARO",
        "type": "Turning Point", "action": "Turning Point",
        "speed": speed_ms, "speed_locked": true,
        "ETA": 0, "ETA_locked": false,
        "task": { "id": "ComboTask", "params": { "tasks": tasks } }
    })
}

pub fn ground_waypoint(p: V3, speed_ms: f64, on_road: bool, tasks: Vec<J>) -> J {
    let action = if on_road { "On Road" } else { "Off Road" };
    json!({
        "x": p.x, "y": p.z, "alt": 0, "alt_type": "RADIO",
        "type": "Turning Point", "action": action,
        "speed": speed_ms, "speed_locked": true,
        "ETA": 0, "ETA_locked": false,
        "task": { "id": "ComboTask", "params": { "tasks": tasks } }
    })
}

/// Wrap a task in a route-point task entry.
pub fn task_entry(n: usize, task: J) -> J {
    let mut t = task;
    if let J::Object(o) = &mut t {
        o.insert("number".into(), json!(n));
        o.insert("enabled".into(), json!(true));
        o.insert("auto".into(), json!(false));
    }
    t
}

/// A WrappedAction route task around a command.
pub fn wrapped(n: usize, cmd: J) -> J {
    task_entry(n, json!({ "id": "WrappedAction", "params": { "action": cmd } }))
}

pub fn wrapped_option(n: usize, id: i64, value: J) -> J {
    task_entry(
        n,
        json!({ "id": "WrappedAction", "params": { "action": { "id": "Option", "params": { "name": id, "value": value } } } }),
    )
}

#[derive(Debug, Clone)]
pub struct AirSpec {
    pub name: String,
    pub typ: String,
    pub count: u32,
    pub skill: String,
    pub pos: V3,
    pub alt_m: f64,
    pub speed_ms: f64,
    pub hdg: f64,
    pub pylons: BTreeMap<u8, String>,
    pub livery: Option<String>,
    /// (callsign name id, group number, name e.g. "Texaco") for NATO
    /// callsigns; None for the default.
    pub callsign: Option<(i64, i64, String)>,
    pub freq_mhz: Option<f64>,
    /// "CAP", "Refueling", "CAS", "Nothing" ...
    pub task: String,
    pub route: Vec<J>,
    pub fuel_kg: Option<f64>,
    pub side: Side,
}

pub fn air_group(s: &AirSpec) -> J {
    let mut units = vec![];
    for i in 0..s.count.max(1) {
        // echelon right, 150 m spacing
        let p = util::offset(s.pos, s.hdg, -150. * i as f64, 150. * i as f64);
        let pylons: serde_json::Map<String, J> = s
            .pylons
            .iter()
            .map(|(k, v)| (k.to_string(), json!({ "CLSID": v })))
            .collect();
        let mut u = json!({
            "name": format!("{}-{}", s.name, i + 1),
            "type": s.typ,
            "x": p.x, "y": p.z,
            "alt": s.alt_m, "alt_type": "BARO",
            "speed": s.speed_ms,
            "heading": s.hdg.to_radians(),
            "skill": s.skill,
            "payload": {
                "pylons": pylons,
                // max internal fuel from DCS's own db when we have it; a
                // small safe load otherwise (every engine-spawned aircraft
                // also gets SetUnlimitedFuel)
                "fuel": s.fuel_kg.unwrap_or(1000.),
                "flare": 60, "chaff": 60, "gun": 100
            },
            "onboard_num": format!("{:03}", 10 + i),
        });
        if let Some(l) = &s.livery {
            u["livery_id"] = json!(l);
        }
        if let Some((cs, grp, name)) = &s.callsign {
            if s.side == Side::Blue {
                u["callsign"] = json!({
                    "1": cs, "2": grp, "3": i + 1,
                    "name": format!("{}{}{}", name, grp, i + 1)
                });
            } else {
                u["callsign"] = json!(100 + i);
            }
        }
        units.push(u);
    }
    let mut g = json!({
        "name": s.name,
        "task": s.task,
        "units": units,
        "route": { "points": s.route },
        "uncontrolled": false,
        "hidden": false,
    });
    if let Some(f) = s.freq_mhz {
        g["frequency"] = json!(f);
        g["modulation"] = json!(0);
        g["communication"] = json!(true);
    }
    g
}

/// One ground/ship unit: (type, position, heading deg).
pub fn surface_group(name: &str, units: &[(String, V3, f64)], skill: &str, route: Vec<J>) -> J {
    let us: Vec<J> = units
        .iter()
        .enumerate()
        .map(|(i, (typ, p, h))| {
            json!({
                "name": format!("{name}-{}", i + 1),
                "type": typ,
                "x": p.x, "y": p.z,
                "heading": h.to_radians(),
                "skill": skill,
                "playerCanDrive": false,
            })
        })
        .collect();
    json!({
        "name": name,
        "task": "Ground Nothing",
        "units": us,
        "route": { "points": route },
        "visible": true,
        "hidden": false,
    })
}

pub fn static_object(name: &str, typ: &str, p: V3, hdg: f64, cargo_mass: Option<f64>) -> J {
    let mut s = json!({
        "name": name,
        "type": typ,
        "x": p.x, "y": p.z,
        "heading": hdg.to_radians(),
        "dead": false,
    });
    match cargo_mass {
        Some(m) => {
            s["canCargo"] = json!(true);
            s["mass"] = json!(m);
            s["category"] = json!("Cargos");
        }
        // DCS infers most statics' category from the type, but an explicit
        // one is what the Mission Editor always writes
        None => s["category"] = json!("Fortifications"),
    }
    s
}

pub fn add_group(lua: MizLua, country: i64, category: i64, g: &J) -> Result<()> {
    let l = lua.inner();
    let coa: LuaTable = l.globals().raw_get("coalition")?;
    let data = json_to_lua(l, g)?;
    let r: LuaValue = coa.call_function("addGroup", (country, category, data))?;
    if matches!(r, LuaValue::Nil) {
        bail!("DCS refused the group (check the country is in the mission's coalitions)")
    }
    Ok(())
}

pub fn add_static(lua: MizLua, country: i64, s: &J) -> Result<()> {
    let l = lua.inner();
    let coa: LuaTable = l.globals().raw_get("coalition")?;
    let data = json_to_lua(l, s)?;
    let r: LuaValue = coa.call_function("addStaticObject", (country, data))?;
    if matches!(r, LuaValue::Nil) {
        bail!("DCS refused the static object")
    }
    Ok(())
}

pub fn group_exists(lua: MizLua, name: &str) -> bool {
    (|| -> Result<bool> {
        let g: LuaTable = lua.inner().globals().raw_get("Group")?;
        let v: LuaValue = g.call_function("getByName", name)?;
        match v {
            LuaValue::Table(t) => Ok(t.call_method::<_, bool>("isExist", ())?
                && t.call_method::<_, i64>("getSize", ())? > 0),
            _ => Ok(false),
        }
    })()
    .unwrap_or(false)
}

pub fn destroy_group(lua: MizLua, name: &str) {
    let r = (|| -> Result<()> {
        let g: LuaTable = lua.inner().globals().raw_get("Group")?;
        if let LuaValue::Table(t) = g.call_function::<_, LuaValue>("getByName", name)? {
            t.call_method::<_, ()>("destroy", ())?;
        }
        Ok(())
    })();
    if let Err(e) = r {
        warn!("could not destroy group {name}: {e:?}")
    }
}

pub fn destroy_static(lua: MizLua, name: &str) {
    let r = (|| -> Result<()> {
        let g: LuaTable = lua.inner().globals().raw_get("StaticObject")?;
        if let LuaValue::Table(t) = g.call_function::<_, LuaValue>("getByName", name)? {
            t.call_method::<_, ()>("destroy", ())?;
        }
        Ok(())
    })();
    if let Err(e) = r {
        warn!("could not destroy static {name}: {e:?}")
    }
}

/// Is `unit` a member of `group`? Engine groups name their units
/// `<group>-<n>`, so a bare prefix test would put "RNG-ADV-50-1" in
/// "RNG-ADV-5".
pub fn in_group(unit: &str, group: &str) -> bool {
    unit.len() > group.len() && unit.starts_with(group) && unit.as_bytes()[group.len()] == b'-'
}

/// Deferred controller work.
#[derive(Debug, Clone)]
pub enum Pending {
    GroupCommand { group: String, cmd: J },
    GroupTask { group: String, task: J },
    GroupOption { group: String, id: i64, value: J },
    UnitCommand { unit: String, cmd: J },
}

/// What a spawn is, for the modules that drive it after creation.
#[derive(Debug, Clone)]
pub enum SpawnKind {
    Adversary { engagement: u64 },
    Tanker,
    Targets,
    Ship,
    Sam,
    Cargo,
}

#[derive(Debug, Clone)]
pub struct Spawn {
    pub id: String,
    pub item: String,
    pub label: String,
    pub owner: Option<String>,
    pub owner_name: String,
    pub groups: Vec<String>,
    pub statics: Vec<String>,
    pub created: DateTime<Utc>,
    pub expires: Option<DateTime<Utc>>,
    pub units: u32,
    pub pos: V3,
    pub kind: SpawnKind,
}

#[derive(Debug, Default)]
pub struct Spawns {
    seq: u64,
    pub active: FxHashMap<String, Spawn>,
    pending: Vec<(f64, Pending)>,
    /// ucid -> last request time, for the cooldown
    last_request: FxHashMap<String, DateTime<Utc>>,
}

impl Spawns {
    pub fn next_name(&mut self, kind: &str) -> String {
        self.seq += 1;
        format!("RNG-{kind}-{}", self.seq)
    }

    pub fn defer(&mut self, due: f64, p: Pending) {
        self.pending.push((due, p));
    }

    pub fn ai_units(&self) -> u32 {
        self.active.values().map(|s| s.units).sum()
    }

    pub fn owned_by(&self, ucid: &str) -> usize {
        self.active.values().filter(|s| s.owner.as_deref() == Some(ucid)).count()
    }

    /// Check the per-player cooldown; records the request when allowed.
    pub fn cooldown_ok(&mut self, ucid: &str, cooldown_s: u32) -> Result<()> {
        let now = Utc::now();
        if let Some(t) = self.last_request.get(ucid) {
            let left = *t + Duration::seconds(cooldown_s as i64) - now;
            if left > Duration::zero() {
                bail!("wait {} s before spawning again", left.num_seconds() + 1)
            }
        }
        self.last_request.insert(ucid.to_string(), now);
        Ok(())
    }

    pub fn insert(&mut self, s: Spawn) {
        info!("spawned {} ({}) for {}", s.id, s.label, s.owner_name);
        self.active.insert(s.id.clone(), s);
    }

    pub fn remove(&mut self, lua: MizLua, id: &str) -> Option<Spawn> {
        let s = self.active.remove(id)?;
        for g in &s.groups {
            destroy_group(lua, g);
        }
        for st in &s.statics {
            destroy_static(lua, st);
        }
        info!("despawned {} ({})", s.id, s.label);
        Some(s)
    }

    pub fn remove_owned(&mut self, lua: MizLua, ucid: &str) -> Vec<Spawn> {
        let ids: Vec<String> = self
            .active
            .values()
            .filter(|s| s.owner.as_deref() == Some(ucid))
            .map(|s| s.id.clone())
            .collect();
        ids.iter().filter_map(|id| self.remove(lua, id)).collect()
    }

    /// Run deferred controller work that is due; drop expired spawns.
    pub fn tick(&mut self, lua: MizLua, now: f64) -> Vec<Spawn> {
        let mut i = 0;
        while i < self.pending.len() {
            if self.pending[i].0 <= now {
                let (_, p) = self.pending.swap_remove(i);
                let r = match &p {
                    Pending::GroupCommand { group, cmd } => {
                        util::group_controller_call(lua, group, "setCommand", cmd.clone())
                    }
                    Pending::GroupTask { group, task } => {
                        util::group_controller_call(lua, group, "setTask", task.clone())
                    }
                    Pending::GroupOption { group, id, value } => {
                        util::group_set_option(lua, group, *id, value.clone())
                    }
                    Pending::UnitCommand { unit, cmd } => {
                        util::unit_controller_call(lua, unit, "setCommand", cmd.clone())
                    }
                };
                if let Err(e) = r {
                    warn!("deferred controller action {p:?} failed: {e:?}")
                }
            } else {
                i += 1;
            }
        }
        let t = Utc::now();
        let expired: Vec<String> = self
            .active
            .values()
            .filter(|s| s.expires.map(|e| e <= t).unwrap_or(false))
            .map(|s| s.id.clone())
            .collect();
        expired.iter().filter_map(|id| self.remove(lua, id)).collect()
    }

    /// Spawns whose every group has died (and that have no statics).
    pub fn dead(&self, lua: MizLua) -> Vec<String> {
        self.active
            .values()
            .filter(|s| s.statics.is_empty() && !s.groups.is_empty())
            .filter(|s| s.groups.iter().all(|g| !group_exists(lua, g)))
            .map(|s| s.id.clone())
            .collect()
    }
}

/// The NATO tanker callsign ids DCS uses: Texaco 1, Arco 2, Shell 3.
pub fn tanker_callsign_id(name: &str) -> i64 {
    match name.to_ascii_lowercase().as_str() {
        "arco" => 2,
        "shell" => 3,
        _ => 1,
    }
}

/// ActivateBeacon command for an airborne TACAN (tankers) or a ship.
pub fn tacan_cmd(unit_id: i64, t: &bfprotocols::range::cfg::TacanCfg, aa: bool) -> J {
    use bfprotocols::range::cfg::TacanBand;
    let (system, mode) = match (t.band, aa) {
        (TacanBand::X, true) => (4, "X"),
        (TacanBand::Y, true) => (5, "Y"),
        (TacanBand::X, false) => (3, "X"),
        (TacanBand::Y, false) => (3, "Y"),
    };
    json!({
        "id": "ActivateBeacon",
        "params": {
            "type": 4,
            "system": system,
            "callsign": t.morse,
            "frequency": t.frequency_hz(),
            "unitId": unit_id,
            "channel": t.channel,
            "modeChannel": mode,
            "AA": aa,
            "bearing": true,
        }
    })
}

pub fn set_frequency_cmd(mhz: f64) -> J {
    json!({ "id": "SetFrequency", "params": { "frequency": mhz * 1e6, "modulation": 0, "power": 10 } })
}

pub fn bool_cmd(id: &str, v: bool) -> J {
    json!({ "id": id, "params": { "value": v } })
}

/// Unit id of the first unit of a group, once it exists.
pub fn first_unit_id(lua: MizLua, group: &str) -> Option<i64> {
    (|| -> Result<Option<i64>> {
        let g: LuaTable = lua.inner().globals().raw_get("Group")?;
        let LuaValue::Table(t) = g.call_function::<_, LuaValue>("getByName", group)? else {
            return Ok(None);
        };
        let u: LuaValue = t.call_method("getUnit", 1)?;
        let LuaValue::Table(u) = u else { return Ok(None) };
        Ok(Some(u.call_method::<_, i64>("getID", ())?))
    })()
    .ok()
    .flatten()
}
