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
//! Beyond plain departures and arrivals, AI flights fly the range's own work:
//!
//! - **Range sorties**: fighters off Andersen to a range, holding at the IP
//!   until Range Control's slot comes, dry passes over the target, and home.
//! - **Tanker sorties**: fighters off Andersen to a tanker that suits them
//!   (boom for an F-16, basket for a Hornet), on the boom, and home.
//! - **Carrier recoveries**: Hornets inbound to a carrier, through Marshal's
//!   stack to a trap.
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
    range::{
        RefuelMethod, StationKind,
        cfg::{AtcTrafficCfg, AtcTrafficType, CarrierKind, RangeCfg, tanker_type},
    },
};
use dcso3::{
    MizLua, airbase::AirbaseCategory, coalition::Side, unit::Unit, world::World,
};
use log::{info, warn};
use serde_json::json;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Plan {
    Departure,
    Arrival,
    /// Work this range station, then land at home.
    Range(String),
    /// Refuel from this tanker, then land at home.
    Tanker(String),
    /// Recover aboard this carrier.
    Carrier(String),
}

impl Plan {
    /// As the controllers read it: "departure", "range:<station>"...
    fn wire(&self) -> String {
        match self {
            Plan::Departure => "departure".into(),
            Plan::Arrival => "arrival".into(),
            Plan::Range(s) => format!("range:{s}"),
            Plan::Tanker(t) => format!("tanker:{t}"),
            Plan::Carrier(c) => format!("carrier:{c}"),
        }
    }

    /// The flight ends by landing (and is removed once it has).
    fn lands(&self) -> bool {
        !matches!(self, Plan::Departure)
    }
}

/// A range target the AI can work.
#[derive(Debug, Clone)]
struct Target {
    id: String,
    pos: V3,
}

/// A tanker the AI can refuel from.
#[derive(Debug, Clone)]
struct TankerSpot {
    id: String,
    method: RefuelMethod,
    pos: V3,
    alt_m: f64,
}

/// A carrier the AI can recover to.
#[derive(Debug, Clone)]
struct Deck {
    id: String,
    unit: String,
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
    /// On the ground (field or deck) after flying, since.
    landed_since: Option<f64>,
    /// Where a sortie works (the range target, the tanker's track).
    goal: Option<V3>,
    /// The sortie has been to its goal.
    visited: bool,
    /// The sortie is done and on its way home: the airfield takes it as an
    /// arrival, the range controllers let it go.
    returning: bool,
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
    next_range: f64,
    next_tanker: f64,
    next_carrier: f64,
    targets: Vec<Target>,
    tankers: Vec<TankerSpot>,
    decks: Vec<Deck>,
    seq: u64,
    last: f64,
    /// A cheap generator: nothing here needs good randomness.
    rng: u64,
}

fn default_types() -> Vec<AtcTrafficType> {
    let t = |typ: &str, cs: &str, weight: u32| AtcTrafficType {
        typ: typ.into(),
        callsign: cs.into(),
        livery: None,
        weight,
    };
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

    pub fn init(&mut self, lua: MizLua, range: &RangeCfg, now: f64) {
        let cfg = &range.atc_traffic;
        if !cfg.enabled {
            return;
        }
        // What the AI can work: land targets, tankers, carriers.
        for s in &range.stations {
            if matches!(s.kind, StationKind::ShipTarget | StationKind::SamSite) {
                continue;
            }
            match crate::ag::resolve(lua, &s.loc) {
                Ok(pos) => self.targets.push(Target { id: s.id.clone(), pos }),
                Err(e) => warn!("atc traffic: station {}: {e:?}", s.id),
            }
        }
        for t in &range.tankers {
            let (Some(tt), Ok(pos)) =
                (tanker_type(&t.typ), crate::ag::resolve(lua, &t.loc))
            else {
                continue;
            };
            self.tankers.push(TankerSpot {
                id: t.id.clone(),
                method: tt.method,
                pos,
                alt_m: t.alt_ft / util::M_TO_FT,
            });
        }
        for c in &range.carriers {
            if matches!(c.kind, CarrierKind::Nimitz | CarrierKind::Forrestal) {
                self.decks.push(Deck { id: c.id.clone(), unit: c.unit_name.clone() });
            }
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
                return Ok(Some(Home {
                    id,
                    name,
                    pos,
                    elev: util::ground_height(lua, pos),
                }));
            }
            Ok(None)
        })();
        match r {
            Ok(Some(h)) => {
                info!("atc traffic: AI departures and arrivals at {}", h.name);
                self.home = Some(h);
                self.cfg = Some(cfg.clone());
                // The first of each a few minutes in, not at once.
                self.next_dep = now + 90. + self.rand() * 120.;
                self.next_arr = now + 150. + self.rand() * 180.;
                self.next_range = now + 240. + self.rand() * 240.;
                self.next_tanker = now + 360. + self.rand() * 300.;
                self.next_carrier = now + 200. + self.rand() * 300.;
                info!(
                    "atc traffic: {} range targets, {} tankers, {} carriers for AI sorties",
                    self.targets.len(),
                    self.tankers.len(),
                    self.decks.len()
                );
            }
            Ok(None) => warn!("atc traffic: no airfield matching {:?}", cfg.field),
            Err(e) => warn!("atc traffic: reading the airfields failed: {e:?}"),
        }
    }

    /// Spawn and clean up, every few seconds.
    pub fn tick(
        &mut self,
        lua: MizLua,
        spawns: &mut Spawns,
        players_flying: usize,
        now: f64,
    ) {
        let (Some(cfg), Some(home)) = (self.cfg.clone(), self.home.clone()) else {
            return;
        };
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
            self.next_range = self.next_range.max(now + 180.);
            self.next_tanker = self.next_tanker.max(now + 240.);
            self.next_carrier = self.next_carrier.max(now + 150.);
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
        } else if now >= self.next_range
            && cfg.range_sorties_per_hour > 0.
            && !self.targets.is_empty()
        {
            self.next_range = now + self.gap(cfg.range_sorties_per_hour);
            if let Err(e) = self.spawn_range_sortie(lua, &cfg, &home, spawns, now) {
                warn!("atc traffic: range sortie not spawned: {e:#}");
            }
        } else if now >= self.next_tanker
            && cfg.tanker_sorties_per_hour > 0.
            && !self.tankers.is_empty()
        {
            self.next_tanker = now + self.gap(cfg.tanker_sorties_per_hour);
            if let Err(e) = self.spawn_tanker_sortie(lua, &cfg, &home, spawns, now) {
                warn!("atc traffic: tanker sortie not spawned: {e:#}");
            }
        } else if now >= self.next_carrier
            && cfg.carrier_recoveries_per_hour > 0.
            && !self.decks.is_empty()
        {
            self.next_carrier = now + self.gap(cfg.carrier_recoveries_per_hour);
            if let Err(e) = self.spawn_carrier_recovery(lua, &cfg, spawns, now) {
                warn!("atc traffic: carrier recovery not spawned: {e:#}");
            }
        }
    }

    /// A fighter type for a sortie (F-16 or Hornet unless configured).
    fn pick_fighter(
        &mut self,
        cfg: &AtcTrafficCfg,
        want: Option<RefuelMethod>,
    ) -> AtcTrafficType {
        let mut v: Vec<AtcTrafficType> = cfg
            .types
            .iter()
            .filter(|t| t.typ.starts_with("F-") || t.typ.starts_with("FA-"))
            .cloned()
            .collect();
        if v.is_empty() {
            v = vec![
                AtcTrafficType {
                    typ: "F-16C_50".into(),
                    callsign: "Venom".into(),
                    livery: None,
                    weight: 1,
                },
                AtcTrafficType {
                    typ: "FA-18C_hornet".into(),
                    callsign: "Ragin".into(),
                    livery: None,
                    weight: 1,
                },
            ];
        }
        // F-16s take the boom, Hornets the basket
        if let Some(m) = want {
            v.retain(|t| {
                (t.typ.starts_with("F-16") || t.typ.starts_with("F-15"))
                    == (m == RefuelMethod::Boom)
            });
        }
        let i = (self.rand() * v.len() as f64) as usize;
        v.get(i).cloned().unwrap_or_else(|| v[0].clone())
    }

    /// A free stand at home, for a type.
    fn stand(
        &mut self,
        lua: MizLua,
        home: &Home,
        typ: &str,
    ) -> anyhow::Result<dcso3::airbase::ParkingSpot> {
        let heavy = !(typ.starts_with("F-")
            || typ.starts_with("FA-")
            || typ.starts_with("AV8")
            || typ.starts_with("A-10"));
        let ab = dcso3::airbase::Airbase::get_by_name(lua, home.name.as_str().into())?;
        let spots: Vec<_> = ab
            .get_parking_spots(true)?
            .into_iter()
            .filter(|s| {
                s.usable_by(false)
                    && (!heavy || s.term_type == dcso3::airbase::term_type::OPEN_BIG)
            })
            .collect();
        let pick = (self.rand() * spots.len() as f64) as usize;
        spots
            .get(pick)
            .copied()
            .ok_or_else(|| anyhow::anyhow!("no free stand for a {typ} at {}", home.name))
    }

    /// Put a flight on a stand at home, uncontrolled until Ground releases
    /// it, with the route it flies after takeoff.
    #[allow(clippy::too_many_arguments)]
    fn spawn_on_the_ramp(
        &mut self,
        lua: MizLua,
        cfg: &AtcTrafficCfg,
        home: &Home,
        spawns: &mut Spawns,
        typ: AtcTrafficType,
        plan: Plan,
        after: Vec<serde_json::Value>,
        goal: V3,
        now: f64,
    ) -> anyhow::Result<()> {
        let spot = self.stand(lua, home, &typ.typ)?;
        let cs = self.callsign(&typ.callsign);
        let (gname, uname) = self.names(&cs);
        let p = spot.pos.0;
        let mut route = vec![json!({
            "x": p.x, "y": p.z, "alt": home.elev, "alt_type": "BARO",
            "type": "TakeOffParkingHot", "action": "From Parking Area Hot",
            "airdromeId": home.id,
            "speed": 0, "speed_locked": true, "ETA": 0, "ETA_locked": true,
            "task": { "id": "ComboTask", "params": { "tasks": [] } }
        })];
        let first = after
            .first()
            .and_then(|w| Some(V3::new(w["x"].as_f64()?, 0., w["y"].as_f64()?)))
            .unwrap_or(home.pos);
        route.extend(after);
        let mut g = self.base_group(
            &gname,
            &uname,
            &typ,
            p,
            util::bearing(p, first),
            0.,
            home.elev,
            route,
        );
        g["units"][0]["parking"] = json!(spot.term_index);
        g["x"] = json!(p.x);
        g["y"] = json!(p.z);
        g["uncontrolled"] = json!(true);
        let country = spawn::country_id(&cfg.country)
            .or_else(|_| spawn::country_id(spawn::default_country(Side::Blue)))?;
        spawn::add_group(lua, country, spawn::AIRPLANE, &g)?;
        for c in [
            spawn::bool_cmd("SetUnlimitedFuel", true),
            spawn::bool_cmd("SetImmortal", true),
        ] {
            spawns.defer(
                now + 2.,
                spawn::Pending::GroupCommand { group: gname.clone(), cmd: c },
            );
        }
        info!(
            "atc traffic: {cs} ({}) on a stand at {} for {}",
            typ.typ,
            home.name,
            plan.wire()
        );
        self.flights.push(Flight {
            group: gname,
            unit: uname,
            callsign: cs,
            typ: typ.typ,
            plan,
            state: State::Waiting,
            spawned: now,
            still_since: None,
            was_airborne: false,
            landed_since: None,
            goal: Some(goal),
            visited: false,
            returning: false,
        });
        Ok(())
    }

    /// The way home: a point 15 nm out on the way in, then the field.
    fn home_legs(home: &Home, from: V3) -> Vec<serde_json::Value> {
        let brg = util::bearing(home.pos, from);
        let gate = util::offset(home.pos, brg, 15. * util::NM, 0.);
        vec![
            spawn::waypoint(gate, home.elev + 1200., 150., vec![]),
            json!({
                "x": home.pos.x, "y": home.pos.z, "alt": home.elev, "alt_type": "BARO",
                "type": "Land", "action": "Landing", "airdromeId": home.id,
                "speed": 70, "ETA": 0, "ETA_locked": false,
                "task": { "id": "ComboTask", "params": { "tasks": [] } }
            }),
        ]
    }

    fn spawn_range_sortie(
        &mut self,
        lua: MizLua,
        cfg: &AtcTrafficCfg,
        home: &Home,
        spawns: &mut Spawns,
        now: f64,
    ) -> anyhow::Result<()> {
        let i = (self.rand() * self.targets.len() as f64) as usize;
        let t = self.targets[i.min(self.targets.len() - 1)].clone();
        let typ = self.pick_fighter(cfg, None);
        // the IP: 15 nm short of the target, on the way from home
        let ip = util::offset(t.pos, util::bearing(t.pos, home.pos), 15. * util::NM, 0.);
        let passes = json!({ "id": "ControlledTask", "params": {
            "task": { "id": "Orbit", "params": { "pattern": "Circle", "point": { "x": t.pos.x, "y": t.pos.z }, "altitude": 2740., "speed": 190. } },
            "stopCondition": { "duration": 360 }
        } });
        let mut after = vec![
            spawn::waypoint(ip, 3660., 210., vec![]),
            spawn::waypoint(t.pos, 2740., 190., vec![spawn::task_entry(1, passes)]),
        ];
        after.extend(Self::home_legs(home, t.pos));
        self.spawn_on_the_ramp(
            lua,
            cfg,
            home,
            spawns,
            typ,
            Plan::Range(t.id.clone()),
            after,
            t.pos,
            now,
        )
    }

    fn spawn_tanker_sortie(
        &mut self,
        lua: MizLua,
        cfg: &AtcTrafficCfg,
        home: &Home,
        spawns: &mut Spawns,
        now: f64,
    ) -> anyhow::Result<()> {
        let i = (self.rand() * self.tankers.len() as f64) as usize;
        let tk = self.tankers[i.min(self.tankers.len() - 1)].clone();
        let typ = self.pick_fighter(cfg, Some(tk.method));
        let refuel = spawn::task_entry(1, json!({ "id": "Refueling", "params": {} }));
        let mut after = vec![spawn::waypoint(
            tk.pos,
            (tk.alt_m - 300.).max(1500.),
            210.,
            vec![refuel],
        )];
        after.extend(Self::home_legs(home, tk.pos));
        self.spawn_on_the_ramp(
            lua,
            cfg,
            home,
            spawns,
            typ,
            Plan::Tanker(tk.id.clone()),
            after,
            tk.pos,
            now,
        )
    }

    fn spawn_carrier_recovery(
        &mut self,
        lua: MizLua,
        cfg: &AtcTrafficCfg,
        spawns: &mut Spawns,
        now: f64,
    ) -> anyhow::Result<()> {
        let i = (self.rand() * self.decks.len() as f64) as usize;
        let d = self.decks[i.min(self.decks.len() - 1)].clone();
        let ship = Unit::get_by_name(lua, &d.unit)?;
        let sp = ship.get_point()?.0;
        let ship_id = ship.id()?.inner();
        let typ = AtcTrafficType {
            typ: "FA-18C_hornet".into(),
            callsign: "Ragin".into(),
            livery: None,
            weight: 1,
        };
        let cs = self.callsign(&typ.callsign);
        let (gname, uname) = self.names(&cs);
        let brg = self.rand() * 360.;
        let alt = 4600. + self.rand() * 1500.;
        let mut start = util::offset(sp, brg, 40. * util::NM, 0.);
        start.y = alt;
        let near = util::offset(sp, brg, 10. * util::NM, 0.);
        let route = vec![
            spawn::waypoint(start, alt, 220., vec![]),
            spawn::waypoint(near, 610., 160., vec![]),
            json!({
                "x": sp.x, "y": sp.z, "alt": 0, "alt_type": "BARO",
                "type": "Land", "action": "Landing",
                "linkUnit": ship_id, "helipadId": ship_id,
                "speed": 70, "ETA": 0, "ETA_locked": false,
                "task": { "id": "ComboTask", "params": { "tasks": [] } }
            }),
        ];
        let g = self.base_group(
            &gname,
            &uname,
            &typ,
            start,
            (brg + 180.).rem_euclid(360.),
            220.,
            alt,
            route,
        );
        let country = spawn::country_id(&cfg.country)
            .or_else(|_| spawn::country_id(spawn::default_country(Side::Blue)))?;
        spawn::add_group(lua, country, spawn::AIRPLANE, &g)?;
        for c in [
            spawn::bool_cmd("SetUnlimitedFuel", true),
            spawn::bool_cmd("SetImmortal", true),
        ] {
            spawns.defer(
                now + 2.,
                spawn::Pending::GroupCommand { group: gname.clone(), cmd: c },
            );
        }
        info!("atc traffic: {cs} (Hornet) inbound to carrier {} from {:.0}°", d.id, brg);
        self.flights.push(Flight {
            group: gname,
            unit: uname,
            callsign: cs,
            typ: typ.typ,
            plan: Plan::Carrier(d.id.clone()),
            state: State::Active,
            spawned: now,
            still_since: None,
            was_airborne: true,
            landed_since: None,
            goal: None,
            visited: false,
            returning: false,
        });
        Ok(())
    }

    fn pick_type(&mut self, cfg: &AtcTrafficCfg) -> AtcTrafficType {
        let types =
            if cfg.types.is_empty() { default_types() } else { cfg.types.clone() };
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

    fn base_group(
        &self,
        name: &str,
        unit: &str,
        typ: &AtcTrafficType,
        unit_pos: V3,
        hdg: f64,
        speed: f64,
        alt: f64,
        route: Vec<serde_json::Value>,
    ) -> serde_json::Value {
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

    fn spawn_departure(
        &mut self,
        lua: MizLua,
        cfg: &AtcTrafficCfg,
        home: &Home,
        spawns: &mut Spawns,
        now: f64,
    ) -> anyhow::Result<()> {
        let typ = self.pick_type(cfg);
        let heavy = !(typ.typ.starts_with("F-")
            || typ.typ.starts_with("FA-")
            || typ.typ.starts_with("AV8")
            || typ.typ.starts_with("A-10"));
        let ab = dcso3::airbase::Airbase::get_by_name(lua, home.name.as_str().into())?;
        let spots: Vec<_> = ab
            .get_parking_spots(true)?
            .into_iter()
            .filter(|s| {
                s.usable_by(false)
                    && (!heavy || s.term_type == dcso3::airbase::term_type::OPEN_BIG)
            })
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
        let mut g = self.base_group(
            &gname,
            &uname,
            &typ,
            p,
            util::bearing(p, w1),
            0.,
            home.elev,
            route,
        );
        g["units"][0]["parking"] = json!(spot.term_index);
        g["x"] = json!(p.x);
        g["y"] = json!(p.z);
        // Sits on its stand until Ground releases it.
        g["uncontrolled"] = json!(true);
        let country = spawn::country_id(&cfg.country)
            .or_else(|_| spawn::country_id(spawn::default_country(Side::Blue)))?;
        spawn::add_group(lua, country, spawn::AIRPLANE, &g)?;
        for c in [
            spawn::bool_cmd("SetUnlimitedFuel", true),
            spawn::bool_cmd("SetImmortal", true),
        ] {
            spawns.defer(
                now + 2.,
                spawn::Pending::GroupCommand { group: gname.clone(), cmd: c },
            );
        }
        info!(
            "atc traffic: {cs} ({}) on a stand at {}, waiting for taxi",
            typ.typ, home.name
        );
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
            landed_since: None,
            goal: None,
            visited: false,
            returning: false,
        });
        Ok(())
    }

    fn spawn_arrival(
        &mut self,
        lua: MizLua,
        cfg: &AtcTrafficCfg,
        home: &Home,
        spawns: &mut Spawns,
        now: f64,
    ) -> anyhow::Result<()> {
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
        let country = spawn::country_id(&cfg.country)
            .or_else(|_| spawn::country_id(spawn::default_country(Side::Blue)))?;
        spawn::add_group(lua, country, spawn::AIRPLANE, &g)?;
        for c in [
            spawn::bool_cmd("SetUnlimitedFuel", true),
            spawn::bool_cmd("SetImmortal", true),
        ] {
            spawns.defer(
                now + 2.,
                spawn::Pending::GroupCommand { group: gname.clone(), cmd: c },
            );
        }
        info!(
            "atc traffic: {cs} ({}) inbound to {} from {:.0}°",
            typ.typ, home.name, brg
        );
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
            landed_since: None,
            goal: None,
            visited: false,
            returning: false,
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
            // a sortie is on its way home once it has worked its goal and
            // left it (range), or is nearly back (tanker: the track moves)
            if let (Some(g), false) = (f.goal, f.returning) {
                let d = util::dist2(p, g);
                match f.plan {
                    Plan::Range(_) => {
                        f.visited |= d < 4. * util::NM;
                        f.returning = f.visited && d > 7. * util::NM;
                    }
                    _ => {
                        f.visited |= d < 15. * util::NM;
                        f.returning =
                            f.visited && util::dist2(p, home.pos) < 30. * util::NM;
                    }
                }
            }
            if f.was_airborne && !airborne {
                f.landed_since.get_or_insert(now);
            } else {
                f.landed_since = None;
            }
            // landed: parked at the field, or down on a moving deck
            let parked = f.plan.lands()
                && f.was_airborne
                && !airborne
                && (f.still_since.is_some_and(|t| now - t > 90.)
                    || f.landed_since.is_some_and(|t| now - t > 150.));
            let done = (f.plan == Plan::Departure && airborne && far)
                || parked
                || now - f.spawned > 5400.;
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
    pub fn command(
        &mut self,
        lua: MizLua,
        group: &str,
        action: &str,
    ) -> anyhow::Result<String> {
        let f = self
            .flights
            .iter_mut()
            .find(|f| f.group == group)
            .ok_or_else(|| anyhow::anyhow!("no AI flight {group}"))?;
        match action {
            "start" => {
                util::group_controller_call(
                    lua,
                    &f.group,
                    "setCommand",
                    json!({ "id": "Start", "params": {} }),
                )?;
                f.state = State::Active;
                Ok(format!("{} released", f.callsign))
            }
            "hold" if f.state == State::Holding => {
                Ok(format!("{} already holding", f.callsign))
            }
            "resume" if f.state != State::Holding => {
                Ok(format!("{} not holding", f.callsign))
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
                util::group_controller_call(
                    lua,
                    &f.group,
                    "popTask",
                    serde_json::Value::Null,
                )?;
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
                    ai_plan: Some(match f.returning {
                        true => format!("{}:home", f.plan.wire()),
                        false => f.plan.wire(),
                    }),
                    ai_state: Some(
                        match f.state {
                            State::Waiting => "waiting",
                            State::Active => "active",
                            State::Holding => "holding",
                        }
                        .into(),
                    ),
                    unit_type: f.typ.clone(),
                })
            })
            .collect()
    }
}
