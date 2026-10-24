// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Anti-ship: ship targets (from the config's ShipTarget stations and on
//! demand) and scoring the weapons fired at them.

use crate::{
    ag::AirToGround,
    players::Flying,
    records::{self, Recorder},
    spawn::{self, Spawn, SpawnKind, Spawns},
    util::{self, V3},
    weapons::{Impact, Purpose},
};
use anyhow::{bail, Result};
use bfprotocols::range::{cfg::RangeCfg, AntiShipResult, PilotRef, RangeResult, StationKind, Track};
use chrono::{Duration, Utc};
use dcso3::{unit::Unit, MizLua};
use fxhash::FxHashMap;
use serde_json::json;

/// Undefended hulls anyone may spawn.
pub const TARGET_SHIPS: &[(&str, &str)] = &[
    ("Dry-cargo ship-1", "Cargo ship"),
    ("ELNYA", "Tanker (Elnya)"),
    ("HandyWind", "Bulk carrier"),
    ("Ship_Tilde_Supply", "Supply ship"),
];

/// Defended warships (instructor-only by default).
pub const WARSHIPS: &[(&str, &str)] = &[
    ("MOLNIYA", "Molniya corvette"),
    ("ALBATROS", "Grisha corvette"),
    ("REZKY", "Krivak frigate"),
    ("NEUSTRASH", "Neustrashimy frigate"),
];

#[derive(Debug, Default)]
pub struct AntiShip {
    /// ship unit name -> life at the last check (for damage accounting)
    life: FxHashMap<String, f64>,
    /// spawned ship group names
    pub groups: Vec<String>,
}

impl AntiShip {
    /// Every ship unit the range owns: config ship stations + spawned.
    pub fn ship_units(&self, ag: &AirToGround) -> Vec<String> {
        let mut v: Vec<String> = ag
            .stations
            .iter()
            .filter(|s| s.cfg.kind == StationKind::ShipTarget)
            .flat_map(|s| s.targets.iter().map(|t| t.name.clone()))
            .collect();
        for g in &self.groups {
            for i in 1..=6 {
                v.push(format!("{g}-{i}"));
            }
        }
        v
    }

    /// Spawn a ship `dist_nm` ahead of the player, over water, optionally
    /// steaming a 10 nm leg back and forth.
    #[allow(clippy::too_many_arguments)]
    pub fn spawn_for(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        spawns: &mut Spawns,
        f: &Flying,
        typ: &str,
        count: u32,
        dist_nm: f64,
        moving: bool,
        weapons_free: bool,
        now: f64,
    ) -> Result<String> {
        let h = util::hdg(f.vel).max(0.);
        let hdg = if f.vel.norm() < 5. { f.hdg } else { h };
        let pos = util::offset(f.pos, hdg, dist_nm * util::NM, 0.);
        if !util::is_water(lua, pos) {
            bail!("{:.0} nm ahead of you is not open water - turn toward the sea and try again", dist_nm)
        }
        let name = spawns.next_name("SHIP");
        let units: Vec<(String, V3, f64)> = (0..count.clamp(1, 4))
            .map(|i| (typ.to_string(), util::offset(pos, hdg + 90., 0., 400. * i as f64), hdg + 90.))
            .collect();
        let speed = if moving { 8. } else { 0. };
        let p2 = util::offset(pos, hdg + 90., 10. * util::NM, 0.);
        let route = if moving {
            vec![
                spawn::waypoint(pos, 0., speed, vec![]),
                spawn::waypoint(p2, 0., speed, vec![spawn::wrapped(
                    1,
                    json!({ "id": "SwitchWaypoint", "params": { "fromWaypointIndex": 2, "goToWaypointIndex": 1 } }),
                )]),
            ]
        } else {
            vec![spawn::waypoint(pos, 0., 0., vec![])]
        };
        let g = spawn::surface_group(&name, &units, "Average", route);
        let side = f.side.opposite();
        spawn::add_group(lua, spawn::country_id(spawn::default_country(side))?, spawn::SHIP, &g)?;
        // hold fire unless the instructor asked for a live warship
        spawns.defer(now + 2., spawn::Pending::GroupOption { group: name.clone(), id: 0, value: json!(if weapons_free { 2 } else { 4 }) });
        self.groups.push(name.clone());
        spawns.insert(Spawn {
            id: name.clone(),
            item: "ship_target".into(),
            label: format!("{} x{}{}", typ, units.len(), if moving { " (moving)" } else { "" }),
            owner: Some(f.ucid.to_string()),
            owner_name: f.name.clone(),
            groups: vec![name.clone()],
            statics: vec![],
            created: Utc::now(),
            expires: Some(Utc::now() + Duration::seconds(cfg.spawn.despawn_after_s as i64)),
            units: units.len() as u32,
            pos,
            kind: SpawnKind::Ship,
        });
        records::to_group(
            lua,
            f.group_id,
            &format!("{} x{} spawned {:03.0} for {:.0} nm", typ, units.len(), hdg, dist_nm),
            15,
        );
        Ok(name)
    }

    pub fn forget(&mut self, group: &str) {
        self.groups.retain(|g| g != group);
    }

    /// Refresh the life table so a hit's damage can be computed.
    pub fn slow_tick(&mut self, lua: MizLua, ag: &AirToGround) {
        for u in self.ship_units(ag) {
            if let Ok(unit) = Unit::get_by_name(lua, &u) {
                if let (Ok(l), Ok(l0)) = (unit.get_life(), unit.get_life0()) {
                    if l0 > 0. {
                        self.life.entry(u).or_insert(l / l0);
                    }
                }
            }
        }
    }

    /// Score an impact if it was aimed at / hit / landed near a ship.
    /// Returns the impact unchanged when it had nothing to do with ships.
    pub fn score_impact(&mut self, lua: MizLua, cfg: &RangeCfg, ag: &AirToGround, rec: &mut Recorder, imp: Impact) -> Option<Impact> {
        if !imp.w.shooter.is_player() {
            return Some(imp);
        }
        let ships = self.ship_units(ag);
        let hit_ship = imp.w.hits.iter().find(|h| ships.contains(h)).cloned();
        let aimed = imp.w.target_name.clone().filter(|t| ships.contains(t));
        let near = ships
            .iter()
            .filter_map(|s| Some((s.clone(), Unit::get_by_name(lua, s).ok()?.get_point().ok()?.0)))
            .map(|(s, p)| (s, util::dist2(p, imp.pos), p))
            .min_by(|a, b| a.1.total_cmp(&b.1));
        let ship = match (&hit_ship, &aimed, &near) {
            (Some(s), _, _) => s.clone(),
            (None, Some(s), _) => s.clone(),
            (None, None, Some((s, d, _))) if *d < 500. || imp.w.purpose == Purpose::AntiShip => s.clone(),
            _ => return Some(imp),
        };
        let ship_pos = near.as_ref().filter(|n| n.0 == ship).map(|n| n.2).unwrap_or(imp.pos);
        let (typ, life_now) = Unit::get_by_name(lua, &ship)
            .ok()
            .map(|u| {
                let t = u.get_type_name().map(|s| s.to_string()).unwrap_or_default();
                let l = match (u.get_life(), u.get_life0()) {
                    (Ok(l), Ok(l0)) if l0 > 0. => l / l0,
                    _ => 0.,
                };
                (t, l)
            })
            .unwrap_or_default();
        let before = self.life.get(&ship).copied().unwrap_or(1.);
        let damage = (before - life_now).max(0.);
        self.life.insert(ship.clone(), life_now);
        let hit = hit_ship.is_some() || damage > 0.001;
        let res = AntiShipResult {
            ship: ship.clone(),
            ship_type: typ.clone(),
            weapon: imp.w.display.clone(),
            launch_range_m: util::dist2(imp.w.rel_pos, ship_pos),
            weapon_max_range_m: imp.w.max_range_m,
            hit,
            damage,
            ship_sunk: life_now <= 0.,
            time_of_flight_s: imp.tof,
            intercepted: !hit && util::dist2(imp.pos, ship_pos) > 200. && imp.w.purpose == Purpose::AntiShip,
            launch_pos: util::geo(lua, imp.w.rel_pos),
            ship_pos: util::geo(lua, ship_pos),
        };
        let sh = &imp.w.shooter;
        if cfg.in_game_results {
            if let Some(g) = sh.group_id {
                records::to_group(
                    lua,
                    g,
                    &format!(
                        "ANTI-SHIP {} on {}: {} - launched {:.1} nm, {:.0} s flight{}",
                        imp.w.display,
                        typ,
                        if hit { "HIT" } else { "MISS" },
                        res.launch_range_m / util::NM,
                        imp.tof,
                        if hit { format!(", {:.0}% damage", damage * 100.) } else { String::new() }
                    ),
                    cfg.message_s,
                );
            }
        }
        rec.emit(
            lua,
            PilotRef { ucid: sh.ucid.map(|u| u.to_string()), name: sh.name.clone() },
            &sh.typ,
            sh.side,
            &sh.callsign,
            Some(if hit { 5. } else { 1. }),
            RangeResult::AntiShip(res),
            Some(Track::Weapon { points: imp.w.pts.clone() }),
        );
        None
    }
}
