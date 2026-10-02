// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Combined Arms gunnery lanes and CAS drills with an AI JTAC.
//!
//! A CAS drill: the pilot asks a JTAC for work; the JTAC picks a live target
//! at its station, starts lasing it and passes a nine-line in text. The drill
//! is scored on the pilot's first weapon impact near the station: time from
//! the nine-line, miss distance to the designated target, whether it was the
//! right target, and whether it landed danger-close to the friendlies.

use crate::{
    ag::{self, AirToGround},
    players::Flying,
    records::{self, Recorder},
    spawn::{self, Spawns},
    util::{self, V3},
    weapons::Impact,
};
use anyhow::{anyhow, bail, Result};
use bfprotocols::range::{
    cfg::{JtacCfg, RangeCfg},
    CasResult, GunneryResult, PilotRef, RangeResult, StationKind,
};
use dcso3::{coord::Coord, unit::Unit, LuaEnv, LuaVec3, MizLua};
use fxhash::FxHashMap;
use log::{info, warn};
use mlua::{prelude::*, RegistryKey};
use serde_json::json;

#[derive(Debug)]
struct Drill {
    jtac: String,
    station: usize,
    target: String,
    target_pos: V3,
    unit: String,
    started: f64,
    friendlies: Option<V3>,
}

#[derive(Debug)]
struct JtacRt {
    cfg: JtacCfg,
    pos: V3,
    unit_group: Option<String>,
    spot: Option<RegistryKey>,
    friendly_group: Option<String>,
    friendly_pos: Option<V3>,
}

#[derive(Debug)]
struct Lane {
    station: usize,
    ucid: String,
    pilot: PilotRef,
    typ: String,
    side: dcso3::coalition::Side,
    callsign: String,
    started: f64,
    last: f64,
    shots: u32,
    hits: u32,
    kills: u32,
    first_round: u32,
}

#[derive(Debug, Default)]
pub struct Ground {
    jtacs: Vec<JtacRt>,
    drills: FxHashMap<String, Drill>,
    lanes: FxHashMap<String, Lane>,
}

fn cardinal(brg: f64) -> &'static str {
    const C: [&str; 8] = ["north", "north-east", "east", "south-east", "south", "south-west", "west", "north-west"];
    C[(((brg + 22.5).rem_euclid(360.)) / 45.) as usize % 8]
}

fn dms(v: f64, pos: char, neg: char) -> String {
    let h = if v >= 0. { pos } else { neg };
    let a = v.abs();
    let d = a.floor();
    let m = (a - d) * 60.;
    format!("{h} {d:.0} {m:06.3}")
}

impl Ground {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg) {
        for j in &cfg.jtacs {
            match ag::resolve(lua, &j.loc) {
                Ok(pos) => self.jtacs.push(JtacRt {
                    cfg: j.clone(),
                    pos,
                    unit_group: None,
                    spot: None,
                    friendly_group: None,
                    friendly_pos: None,
                }),
                Err(e) => warn!("JTAC {}: {e:?}", j.id),
            }
        }
    }

    pub fn jtac_list(&self) -> Vec<(String, String)> {
        self.jtacs.iter().map(|j| (j.cfg.id.clone(), j.cfg.callsign.clone())).collect()
    }

    fn ensure_jtac(lua: MizLua, spawns: &mut Spawns, j: &mut JtacRt, side: dcso3::coalition::Side, now: f64) -> Result<()> {
        if j.unit_group.as_ref().map(|g| spawn::group_exists(lua, g)).unwrap_or(false) {
            return Ok(());
        }
        let name = format!("RNG-JTAC-{}", j.cfg.id);
        let country = spawn::country_id(spawn::default_country(side))?;
        if j.cfg.typ.contains("MQ-9") || j.cfg.typ.contains("MQ-1") {
            let alt = j.pos.y + 4600.;
            let orbit = json!({ "id": "Orbit", "params": { "pattern": "Circle", "point": { "x": j.pos.x, "y": j.pos.z }, "speed": 60, "altitude": alt } });
            let spec = spawn::AirSpec {
                name: name.clone(),
                typ: j.cfg.typ.clone(),
                count: 1,
                skill: "Excellent".into(),
                pos: util::offset(j.pos, 0., -3000., 0.),
                alt_m: alt,
                speed_ms: 60.,
                hdg: 90.,
                pylons: Default::default(),
                livery: None,
                callsign: None,
                freq_mhz: j.cfg.freq_mhz,
                task: "Reconnaissance".into(),
                route: vec![spawn::waypoint(j.pos, alt, 60., vec![spawn::task_entry(1, orbit)])],
                fuel_kg: None,
                side,
            };
            spawn::add_group(lua, country, spawn::AIRPLANE, &spawn::air_group(&spec))?;
        } else {
            let g = spawn::surface_group(&name, &[(j.cfg.typ.clone(), j.pos, 0.)], "Excellent", vec![spawn::ground_waypoint(j.pos, 0., false, vec![])]);
            spawn::add_group(lua, country, spawn::GROUND, &g)?;
            spawns.defer(now + 2., spawn::Pending::GroupOption { group: name.clone(), id: 0, value: json!(4) });
        }
        for c in [spawn::bool_cmd("SetInvisible", true), spawn::bool_cmd("SetImmortal", true), spawn::bool_cmd("SetUnlimitedFuel", true)] {
            spawns.defer(now + 2., spawn::Pending::GroupCommand { group: name.clone(), cmd: c });
        }
        j.unit_group = Some(name);
        Ok(())
    }

    /// Start a CAS drill for `f` with JTAC `jtac_id`. Returns the nine-line.
    #[allow(clippy::too_many_arguments)]
    pub fn request_cas(
        &mut self,
        lua: MizLua,
        spawns: &mut Spawns,
        ag: &AirToGround,
        f: &Flying,
        jtac_id: &str,
        now: f64,
    ) -> Result<String> {
        let j = self
            .jtacs
            .iter_mut()
            .find(|j| j.cfg.id == jtac_id)
            .ok_or_else(|| anyhow!("no JTAC {jtac_id}"))?;
        let si = ag.station_by_id(&j.cfg.station).ok_or_else(|| anyhow!("JTAC station {} missing", j.cfg.station))?;
        let st = &ag.stations[si];
        let tgt = st
            .targets
            .iter()
            .filter(|t| t.alive)
            .min_by(|a, b| util::dist2(a.pos, st.center).total_cmp(&util::dist2(b.pos, st.center)))
            .ok_or_else(|| anyhow!("{} has no live targets right now", st.cfg.name))?;
        Self::ensure_jtac(lua, spawns, j, f.side, now)?;
        // friendlies for danger-close training
        if let (Some(d), None) = (j.cfg.friendlies_m, &j.friendly_group) {
            let fp = util::offset(tgt.pos, util::bearing(tgt.pos, j.pos), d, 0.);
            let name = format!("RNG-FRND-{}", j.cfg.id);
            let units: Vec<(String, V3, f64)> = (0..4)
                .map(|i| ("Soldier M4".to_string(), util::offset(fp, 0., 0., 8. * i as f64), 0.))
                .collect();
            let g = spawn::surface_group(&name, &units, "Average", vec![spawn::ground_waypoint(fp, 0., false, vec![])]);
            if let Ok(c) = spawn::country_id(spawn::default_country(f.side)) {
                if spawn::add_group(lua, c, spawn::GROUND, &g).is_ok() {
                    spawns.defer(now + 2., spawn::Pending::GroupOption { group: name.clone(), id: 0, value: json!(4) });
                    spawns.defer(now + 2., spawn::Pending::GroupCommand { group: name.clone(), cmd: spawn::bool_cmd("SetImmortal", true) });
                    j.friendly_group = Some(name);
                    j.friendly_pos = Some(fp);
                }
            }
        }
        // laser on
        let aim = V3::new(tgt.pos.x, tgt.pos.y + 1.5, tgt.pos.z);
        if let Some(g) = &j.unit_group {
            match &j.spot {
                Some(k) => {
                    if let Ok(t) = lua.inner().registry_value::<LuaTable>(k) {
                        let _ = t.call_method::<_, ()>("setPoint", LuaVec3(aim));
                    }
                }
                None => {
                    if let Ok(u) = Unit::get_by_name(lua, &format!("{g}-1")) {
                        match dcso3::spot::Spot::create_laser(lua, u.as_object()?, Some(LuaVec3(V3::new(0., 2., 0.))), LuaVec3(aim), j.cfg.laser_code) {
                            Ok(s) => {
                                let t: LuaTable = (*s).clone();
                                j.spot = Some(lua.inner().create_registry_value(t)?);
                            }
                            Err(e) => warn!("JTAC {} laser: {e:?}", j.cfg.id),
                        }
                    }
                }
            }
        }
        let coord = Coord::singleton(lua)?;
        let ll = coord.lo_to_ll(LuaVec3(tgt.pos))?;
        let mgrs = coord
            .ll_to_mgrs(ll.latitude, ll.longitude)
            .map(|m| format!("{} {} {:05.0} {:05.0}", m.utm_zone, m.mgrs_digraph, m.easting, m.northing))
            .unwrap_or_default();
        let brg = util::bearing(f.pos, tgt.pos);
        let fr = j.friendly_pos.map(|fp| {
            format!("{:.0} m {} of the target", util::dist2(fp, tgt.pos), cardinal(util::bearing(tgt.pos, fp)))
        });
        let nine = format!(
            "{cs}, nine-line:\n1. IP: {ip}\n2. Heading {brg:03.0} (from your position), offset none\n3. Distance {dist:.1} nm\n4. Elevation {elev:.0} ft MSL\n5. {desc}\n6. {lat} {lon}  MGRS {mgrs}\n7. Mark: laser, code {code}\n8. Friendlies: {fr}\n9. Egress: as required\nRemarks: cleared hot on the laser. Report IN.",
            cs = j.cfg.callsign,
            ip = st.cfg.name,
            dist = util::dist2(f.pos, tgt.pos) / util::NM,
            elev = tgt.pos.y * util::M_TO_FT,
            desc = tgt.typ,
            lat = dms(ll.latitude, 'N', 'S'),
            lon = dms(ll.longitude, 'E', 'W'),
            code = j.cfg.laser_code,
            fr = fr.unwrap_or_else(|| "none".into()),
        );
        info!("CAS drill: {} with {} on {}", f.name, j.cfg.callsign, tgt.name);
        self.drills.insert(
            f.ucid.to_string(),
            Drill {
                jtac: j.cfg.callsign.clone(),
                station: si,
                target: tgt.name.clone(),
                target_pos: tgt.pos,
                unit: f.unit_name.clone(),
                started: now,
                friendlies: j.friendly_pos,
            },
        );
        Ok(nine)
    }

    /// Offer an impact to a running CAS drill. Called for every scored
    /// air-to-ground impact.
    pub fn impact(&mut self, lua: MizLua, cfg: &RangeCfg, ag: &AirToGround, rec: &mut Recorder, imp: &Impact, now: f64) {
        let Some(u) = imp.w.shooter.ucid.map(|u| u.to_string()) else { return };
        let Some(d) = self.drills.get(&u) else { return };
        let st = &ag.stations[d.station];
        if util::dist2(imp.pos, st.center) > 3000. {
            return;
        }
        let d = self.drills.remove(&u).unwrap();
        let miss = util::dist2(imp.pos, d.target_pos);
        let nearest = st
            .targets
            .iter()
            .min_by(|a, b| util::dist2(a.pos, imp.pos).total_cmp(&util::dist2(b.pos, imp.pos)))
            .map(|t| t.name.clone());
        let correct = nearest.as_deref() == Some(d.target.as_str()) || imp.w.hits.contains(&d.target);
        let nf = d.friendlies.map(|fp| util::dist2(fp, imp.pos));
        let danger = nf.map(|x| x < 150.).unwrap_or(false);
        let res = CasResult {
            jtac: d.jtac.clone(),
            target: d.target.clone(),
            weapon: imp.w.display.clone(),
            time_to_impact_s: now - d.started,
            miss_m: miss,
            correct_target: correct,
            danger_close: danger,
            nearest_friendly_m: nf,
            laser_code: None,
        };
        let score = if !correct || danger {
            1.
        } else {
            bfprotocols::range::grading::bomb_quality(&cfg.scoring, imp.w.class, miss).score()
        };
        if let Some(g) = imp.w.shooter.group_id {
            records::to_group(
                lua,
                g,
                &format!(
                    "{}: {} - {:.0} m from the designated target, {:.0} s from the nine-line{}",
                    d.jtac,
                    if correct { "good hit on the right target" } else { "WRONG TARGET" },
                    miss,
                    now - d.started,
                    if danger { ". DANGER CLOSE - friendlies within 150 m!" } else { "" }
                ),
                cfg.message_s,
            );
        }
        rec.emit(
            lua,
            PilotRef { ucid: Some(u), name: imp.w.shooter.name.clone() },
            &imp.w.shooter.typ,
            imp.w.shooter.side,
            &imp.w.shooter.callsign,
            Some(score),
            RangeResult::Cas(res),
            None,
        );
    }

    // ----------------------------------------------------------- gunnery

    /// A hit or kill by a (CA) player on a gunnery-lane target.
    #[allow(clippy::too_many_arguments)]
    pub fn lane_event(&mut self, ag: &AirToGround, f: &Flying, target: &str, kill: bool, now: f64) {
        let Some(si) = ag.is_station_target(target) else { return };
        if ag.stations[si].cfg.kind != StationKind::GunneryLane {
            return;
        }
        let l = self.lanes.entry(f.unit_name.clone()).or_insert_with(|| Lane {
            station: si,
            ucid: f.ucid.to_string(),
            pilot: records::pilot_of(f),
            typ: f.typ.clone(),
            side: f.side,
            callsign: f.group_name.clone(),
            started: now,
            last: now,
            shots: 0,
            hits: 0,
            kills: 0,
            first_round: 0,
        });
        l.last = now;
        if kill {
            l.kills += 1;
        } else {
            l.hits += 1;
            if l.shots <= l.kills + 1 {
                l.first_round += 1;
            }
        }
    }

    pub fn lane_shot(&mut self, unit: &str) {
        if let Some(l) = self.lanes.get_mut(unit) {
            l.shots += 1;
        }
    }

    /// Close lanes that are cleared or idle for two minutes.
    pub fn slow_tick(&mut self, lua: MizLua, ag: &AirToGround, rec: &mut Recorder, now: f64) {
        let done: Vec<String> = self
            .lanes
            .iter()
            .filter(|(_, l)| now - l.last > 120. || ag.stations[l.station].alive() == 0)
            .map(|(k, _)| k.clone())
            .collect();
        for k in done {
            let Some(l) = self.lanes.remove(&k) else { continue };
            let st = &ag.stations[l.station];
            let res = GunneryResult {
                lane: st.cfg.name.clone(),
                targets_total: st.targets.len() as u32,
                targets_killed: l.kills,
                shots: l.shots.max(l.hits),
                hits: l.hits,
                time_s: l.last - l.started,
                first_round_hits: l.first_round,
            };
            let frac = if res.targets_total > 0 { l.kills as f64 / res.targets_total as f64 } else { 0. };
            let _ = l.ucid;
            rec.emit(lua, l.pilot.clone(), &l.typ, l.side, &l.callsign, Some((frac * 5.).clamp(1., 5.)), RangeResult::Gunnery(res), None);
        }
    }

    pub fn player_left(&mut self, unit: &str) {
        self.drills.retain(|_, d| d.unit != unit);
    }

    pub fn require_jtac(&self) -> Result<()> {
        if self.jtacs.is_empty() {
            bail!("this range has no JTACs configured")
        }
        Ok(())
    }
}
