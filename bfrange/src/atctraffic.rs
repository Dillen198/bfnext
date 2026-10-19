// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! AI traffic at the home field, for the spoken ATC to control.
//!
//! A steady trickle of single-ship departures and arrivals: tankers, lifters,
//! bombers and fighters. bfdb's controllers work them like anyone else, and
//! their pilots answer on the frequency. The controllers really control them
//! through `range-atc` commands:
//!
//! - A departure is spawned on a free stand, engines running but
//!   *uncontrolled*: it sits there until Ground's taxi clearance sends
//!   `start`, which is held back while the runway is busy.
//! - An arrival flies in from 35-40 nm and lands. Approach can put it in a
//!   `hold` (an orbit over its present position) to keep it out of a
//!   player's way, then `resume` it.
//!
//! Arrivals are removed once they have stopped after landing, departures once
//! they are well clear. The traffic flies whether or not anyone is on the
//! server (`fly_when_empty`), so a pilot who joins finds a field already
//! busy. A departure Ground never answers (no ATC running, or its call lost)
//! starts on its own after `self_release_s`, so it cannot block the stands.

use crate::{
    harvest,
    spawn::{self, Spawns},
    util::{self, V3},
};
use bfprotocols::{
    atc::AtcTraffic,
    range::cfg::{AtcTrafficCfg, AtcTrafficType},
};
use dcso3::{airbase::AirbaseCategory, coalition::Side, unit::Unit, world::World, MizLua};
use log::{info, warn};
use serde_json::json;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Plan {
    Departure,
    Arrival,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    /// On the stand, uncontrolled, waiting for Ground.
    Waiting,
    Active,
    Holding,
}

#[derive(Debug, Clone)]
struct Flight {
    group: String,
    unit: String,
    callsign: String,
    typ: String,
    plan: Plan,
    state: State,
    spawned: f64,
    still_since: Option<f64>,
    was_airborne: bool,
}

#[derive(Debug, Clone)]
struct Home {
    id: i64,
    name: String,
    pos: V3,
    elev: f64,
}

#[derive(Debug, Default)]
pub struct AtcTrafficSim {
    cfg: Option<AtcTrafficCfg>,
    home: Option<Home>,
    flights: Vec<Flight>,
    next_dep: f64,
    next_arr: f64,
    seq: u64,
    last: f64,
    /// A cheap generator: nothing here needs good randomness.
    rng: u64,
}

fn default_types() -> Vec<AtcTrafficType> {
    let t = |typ: &str, cs: &str, weight: u32| AtcTrafficType { typ: typ.into(), callsign: cs.into(), livery: None, weight };
    vec![
        t("C-17A", "Reach", 3),
        t("KC-135", "Shell", 2),
        t("C-130", "Herky", 2),
        t("B-52H", "Doom", 1),
        t("F-16C_50", "Venom", 2),
        t("FA-18C_hornet", "Ragin", 1),
    ]
}

impl AtcTrafficSim {
    fn rand(&mut self) -> f64 {
        // xorshift64*
        let mut x = self.rng.max(1);
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.rng = x;
        (x.wrapping_mul(0x2545_F491_4F6C_DD1D) >> 11) as f64 / (1u64 << 53) as f64
    }

    /// Time to the next event of a Poisson process at `per_hour`.
    fn gap(&mut self, per_hour: f64) -> f64 {
        let mean = 3600. / per_hour.max(0.1);
        (-(1. - self.rand()).max(1e-9).ln() * mean).clamp(mean * 0.3, mean * 2.5)
    }

    pub fn init(&mut self, lua: MizLua, cfg: &AtcTrafficCfg, now: f64) {
        if !cfg.enabled {
            return;
        }
        self.rng = (now.to_bits() ^ 0x9E37_79B9_7F4A_7C15) | 1;
        let want = cfg.field.to_lowercase();
        let r = (|| -> anyhow::Result<Option<Home>> {
            for ab in World::singleton(lua)?.get_airbases()? {
                let ab = ab?;
                if ab.get_category().ok() != Some(AirbaseCategory::Airdrome) {
                    continue;
                }
                let name = ab.as_object()?.get_name()?.to_string();
                if !name.to_lowercase().contains(&want) {
                    continue;
                }
                let pos = ab.get_point()?.0;
                let id = ab.get_id()?.inner();
                return Ok(Some(Home { id, name, pos, elev: util::ground_height(lua, pos) }));
            }
            Ok(None)
        })();
        match r {
            Ok(Some(h)) => {
                info!("atc traffic: AI departures and arrivals at {}", h.name);
                self.home = Some(h);
                self.cfg = Some(cfg.clone());
                // The first of each a couple of minutes in, not at once.
                self.next_dep = now + 90. + self.rand() * 120.;
                self.next_arr = now + 150. + self.rand() * 180.;
            }
            Ok(None) => warn!("atc traffic: no airfield matching {:?}", cfg.field),
            Err(e) => warn!("atc traffic: reading the airfields failed: {e:?}"),
        }
    }

    /// Spawn and clean up, every few seconds.
    pub fn tick(&mut self, lua: MizLua, spawns: &mut Spawns, players_flying: usize, now: f64) {
        let (Some(cfg), Some(home)) = (self.cfg.clone(), self.home.clone()) else { return };
        if now - self.last < 5. {
            return;
        }
        self.last = now;
        self.cleanup(lua, &home, now);
        self.self_release(lua, &cfg, now);
        if players_flying == 0 && !cfg.fly_when_empty {
            // Push the schedule along so traffic does not burst in when the
            // first player arrives.
            self.next_dep = self.next_dep.max(now + 60.);
            self.next_arr = self.next_arr.max(now + 120.);
            return;
        }
        if self.flights.len() as u32 >= cfg.max_active {
            return;
        }
        if now >= self.next_dep && cfg.departures_per_hour > 0. {
            self.next_dep = now + self.gap(cfg.departures_per_hour);
            if let Err(e) = self.spawn_departure(lua, &cfg, &home, spawns, now) {
                warn!("atc traffic: departure not spawned: {e:#}");
            }
        } else if now >= self.next_arr && cfg.arrivals_per_hour > 0. {
            self.next_arr = now + self.gap(cfg.arrivals_per_hour);
            if let Err(e) = self.spawn_arrival(lua, &cfg, &home, spawns, now) {
                warn!("atc traffic: arrival not spawned: {e:#}");
            }
        }
    }

    fn pick_type(&mut self, cfg: &AtcTrafficCfg) -> AtcTrafficType {
        let types = if cfg.types.is_empty() { default_types() } else { cfg.types.clone() };
        let total: u32 = types.iter().map(|t| t.weight.max(1)).sum();
        let mut r = (self.rand() * total as f64) as u32;
        for t in &types {
            let w = t.weight.max(1);
            if r < w {
                return t.clone();
            }
            r -= w;
        }
        types[0].clone()
    }

    /// "Reach 3-1": a flight number and lead, unused by anyone else airborne.
    fn callsign(&mut self, word: &str) -> String {
        loop {
            let n = 1 + (self.rand() * 9.) as u32;
            let cs = format!("{word} {n}-1");
            if !self.flights.iter().any(|f| f.callsign == cs) {
                return cs;
            }
        }
    }

    fn names(&mut self, cs: &str) -> (String, String) {
        self.seq += 1;
        let g = format!("ATC-{}-{}", cs.replace([' ', '-'], ""), self.seq);
        let u = format!("{g}-1");
        (g, u)
    }

    fn base_group(&self, name: &str, unit: &str, typ: &AtcTrafficType, unit_pos: V3, hdg: f64, speed: f64, alt: f64, route: Vec<serde_json::Value>) -> serde_json::Value {
        let mut u = json!({
            "name": unit,
            "type": typ.typ,
            "x": unit_pos.x, "y": unit_pos.z,
            "alt": alt, "alt_type": "BARO",
            "speed": speed,
            "heading": hdg.to_radians(),
            "skill": "Excellent",
            "payload": { "pylons": {}, "fuel": harvest::max_fuel_kg(&typ.typ).unwrap_or(8000.), "flare": 0, "chaff": 0, "gun": 100 },
            "onboard_num": "010",
        });
        if let Some(l) = &typ.livery {
            u["livery_id"] = json!(l);
        }
        json!({
            "name": name,
            "task": "Transport",
            "units": [u],
            "route": { "points": route },
            "uncontrolled": false,
            "hidden": false,
        })
    }

    fn spawn_departure(&mut self, lua: MizLua, cfg: &AtcTrafficCfg, home: &Home, spawns: &mut Spawns, now: f64) -> anyhow::Result<()> {
        let typ = self.pick_type(cfg);
        let heavy = !(typ.typ.starts_with("F-") || typ.typ.starts_with("FA-") || typ.typ.starts_with("AV8") || typ.typ.starts_with("A-10"));
        let ab = dcso3::airbase::Airbase::get_by_name(lua, home.name.as_str().into())?;
        let spots: Vec<_> = ab
            .get_parking_spots(true)?
            .into_iter()
            .filter(|s| s.usable_by(false) && (!heavy || s.term_type == dcso3::airbase::term_type::OPEN_BIG))
            .collect();
        let pick = (self.rand() * spots.len() as f64) as usize;
        let Some(spot) = spots.get(pick).copied() else {
            anyhow::bail!("no free stand for a {} at {}", typ.typ, home.name)
        };
        let cs = self.callsign(&typ.callsign);
        let (gname, uname) = self.names(&cs);
        let p = spot.pos.0;
        let out_brg = self.rand() * 360.;
        let cruise = 6000. + self.rand() * 3000.;
        let w1 = util::offset(home.pos, out_brg, 30. * util::NM, 0.);
        let w2 = util::offset(home.pos, out_brg, 60. * util::NM, 0.);
        let route = vec![
            json!({
                "x": p.x, "y": p.z, "alt": home.elev, "alt_type": "BARO",
                "type": "TakeOffParkingHot", "action": "From Parking Area Hot",
                "airdromeId": home.id,
                "speed": 0, "speed_locked": true, "ETA": 0, "ETA_locked": true,
                "task": { "id": "ComboTask", "params": { "tasks": [] } }
            }),
            spawn::waypoint(w1, cruise, 200., vec![]),
            spawn::waypoint(w2, cruise, 220., vec![]),
        ];
        let mut g = self.base_group(&gname, &uname, &typ, p, util::bearing(p, w1), 0., home.elev, route);
        g["units"][0]["parking"] = json!(spot.term_index);
        g["x"] = json!(p.x);
        g["y"] = json!(p.z);
        // Sits on its stand until Ground releases it.
        g["uncontrolled"] = json!(true);
        let country = spawn::country_id(&cfg.country).or_else(|_| spawn::country_id(spawn::default_country(Side::Blue)))?;
        spawn::add_group(lua, country, spawn::AIRPLANE, &g)?;
        for c in [spawn::bool_cmd("SetUnlimitedFuel", true), spawn::bool_cmd("SetImmortal", true)] {
            spawns.defer(now + 2., spawn::Pending::GroupCommand { group: gname.clone(), cmd: c });
        }
        info!("atc traffic: {cs} ({}) on a stand at {}, waiting for taxi", typ.typ, home.name);
        self.flights.push(Flight {
            group: gname,
            unit: uname,
            callsign: cs,
            typ: typ.typ,
            plan: Plan::Departure,
            state: State::Waiting,
            spawned: now,
            still_since: None,
            was_airborne: false,
        });
        Ok(())
    }

    fn spawn_arrival(&mut self, lua: MizLua, cfg: &AtcTrafficCfg, home: &Home, spawns: &mut Spawns, now: f64) -> anyhow::Result<()> {
        let typ = self.pick_type(cfg);
        let cs = self.callsign(&typ.callsign);
        let (gname, uname) = self.names(&cs);
        // From open water to the north, east or south of Guam, away from
        // the range's own working areas to the north-west where possible.
        let brg = 30. + self.rand() * 180.;
        let d = (35. + self.rand() * 5.) * util::NM;
        let alt = 3600. + self.rand() * 1500.;
        let mut start = util::offset(home.pos, brg, d, 0.);
        start.y = alt;
        let inbound = (brg + 180.).rem_euclid(360.);
        let mid = util::offset(home.pos, brg, 15. * util::NM, 0.);
        let route = vec![
            spawn::waypoint(start, alt, 190., vec![]),
            spawn::waypoint(mid, home.elev + 1200., 140., vec![]),
            json!({
                "x": home.pos.x, "y": home.pos.z, "alt": home.elev, "alt_type": "BARO",
                "type": "Land", "action": "Landing", "airdromeId": home.id,
                "speed": 70, "ETA": 0, "ETA_locked": false,
                "task": { "id": "ComboTask", "params": { "tasks": [] } }
            }),
        ];
        let g = self.base_group(&gname, &uname, &typ, start, inbound, 190., alt, route);
        let country = spawn::country_id(&cfg.country).or_else(|_| spawn::country_id(spawn::default_country(Side::Blue)))?;
        spawn::add_group(lua, country, spawn::AIRPLANE, &g)?;
        for c in [spawn::bool_cmd("SetUnlimitedFuel", true), spawn::bool_cmd("SetImmortal", true)] {
            spawns.defer(now + 2., spawn::Pending::GroupCommand { group: gname.clone(), cmd: c });
        }
        info!("atc traffic: {cs} ({}) inbound to {} from {:.0}°", typ.typ, home.name, brg);
        self.flights.push(Flight {
            group: gname,
            unit: uname,
            callsign: cs,
            typ: typ.typ,
            plan: Plan::Arrival,
            state: State::Active,
            spawned: now,
            still_since: None,
            was_airborne: true,
        });
        Ok(())
    }

    fn cleanup(&mut self, lua: MizLua, home: &Home, now: f64) {
        let mut gone = vec![];
        for (i, f) in self.flights.iter_mut().enumerate() {
            let Ok(u) = Unit::get_by_name(lua, &f.unit) else {
                if now - f.spawned > 10. {
                    gone.push((i, false));
                }
                continue;
            };
            let (Ok(p), Ok(v)) = (u.get_point(), u.get_velocity()) else { continue };
            let (p, v) = (p.0, v.0);
            let airborne = u.in_air().unwrap_or(false);
            f.was_airborne |= airborne;
            let far = util::dist2(p, home.pos) > 45. * util::NM;
            let stopped = !airborne && util::gs_kts(v) < 1.;
            if stopped {
                f.still_since.get_or_insert(now);
            } else {
                f.still_since = None;
            }
            let parked = f.plan == Plan::Arrival && f.was_airborne && !airborne && f.still_since.map_or(false, |t| now - t > 90.);
            let done = (f.plan == Plan::Departure && airborne && far) || parked || now - f.spawned > 3600.;
            if done {
                gone.push((i, true));
            }
        }
        for (i, destroy) in gone.into_iter().rev() {
            let f = self.flights.remove(i);
            if destroy {
                spawn::destroy_group(lua, &f.group);
            }
        }
    }

    /// Start departures that have waited on their stands longer than
    /// `self_release_s` without a word from Ground.
    fn self_release(&mut self, lua: MizLua, cfg: &AtcTrafficCfg, now: f64) {
        if cfg.self_release_s <= 0. {
            return;
        }
        let due: Vec<String> = self
            .flights
            .iter()
            .filter(|f| f.state == State::Waiting && now - f.spawned > cfg.self_release_s)
            .map(|f| f.group.clone())
            .collect();
        for g in due {
            match self.command(lua, &g, "start") {
                Ok(m) => info!("atc traffic: {m} by itself: Ground never released it"),
                Err(e) => warn!("atc traffic: could not start {g}: {e:#}"),
            }
        }
    }

    /// A controller's command: `start` a waiting departure, `hold` an
    /// arrival over its present position, `resume` it.
    pub fn command(&mut self, lua: MizLua, group: &str, action: &str) -> anyhow::Result<String> {
        let f = self.flights.iter_mut().find(|f| f.group == group).ok_or_else(|| anyhow::anyhow!("no AI flight {group}"))?;
        match action {
            "start" => {
                util::group_controller_call(lua, &f.group, "setCommand", json!({ "id": "Start", "params": {} }))?;
                f.state = State::Active;
                Ok(format!("{} released", f.callsign))
            }
            "hold" => {
                let u = Unit::get_by_name(lua, &f.unit)?;
                let p = u.get_point()?.0;
                util::group_controller_call(
                    lua,
                    &f.group,
                    "pushTask",
                    json!({ "id": "Orbit", "params": { "pattern": "Circle", "point": { "x": p.x, "y": p.z }, "altitude": p.y.max(900.), "speed": 150 } }),
                )?;
                f.state = State::Holding;
                Ok(format!("{} holding", f.callsign))
            }
            "resume" => {
                util::group_controller_call(lua, &f.group, "popTask", serde_json::Value::Null)?;
                f.state = State::Active;
                Ok(format!("{} resumed", f.callsign))
            }
            other => anyhow::bail!("unknown action {other:?}"),
        }
    }

    /// The AI flights as radar sees them.
    pub fn live(&self, lua: MizLua) -> Vec<AtcTraffic> {
        let Some(home) = &self.home else { return vec![] };
        self.flights
            .iter()
            .filter_map(|f| {
                let u = Unit::get_by_name(lua, &f.unit).ok()?;
                let p = u.get_point().ok()?.0;
                let v = u.get_velocity().ok()?.0;
                let g = util::geo(lua, p);
                let agl = p.y - util::ground_height(lua, p);
                Some(AtcTraffic {
                    ucid: String::new(),
                    unit_id: u.id().ok().map(|i| i.inner() as u64),
                    callsign: f.callsign.clone(),
                    player_name: f.group.clone(),
                    lat: g.lat,
                    lon: g.lon,
                    alt_ft: (p.y * util::M_TO_FT).round() as i32,
                    agl_ft: (agl * util::M_TO_FT).round() as i32,
                    heading: util::hdg(v).round() as u16 % 360,
                    speed_kts: util::gs_kts(v).round() as u16,
                    vspd_fpm: (v.y * util::M_TO_FT * 60.).round() as i32,
                    on_ground: !u.in_air().unwrap_or(false),
                    field: Some(home.name.clone()),
                    field_brg: util::bearing(home.pos, p).round() as u16 % 360,
                    field_rng_m: util::dist2(home.pos, p).round() as u32,
                    rotary: false,
                    ai: true,
                    ai_plan: Some(match f.plan {
                        Plan::Departure => "departure",
                        Plan::Arrival => "arrival",
                    }
                    .into()),
                    ai_state: Some(match f.state {
                        State::Waiting => "waiting",
                        State::Active => "active",
                        State::Holding => "holding",
                    }
                    .into()),
                    unit_type: f.typ.clone(),
                })
            })
            .collect()
    }
}
