// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Hot zones: areas that always fight back.
//!
//! The thing pilots miss most about a PvE server is somewhere to go and
//! fight without setting anything up. A hot zone keeps AI fighters on CAP
//! while any player of the other side is inside it (launched from the far
//! edge, a fresh flight a while after one is lost), ground defences and
//! targets that respawn, and an AI AWACS on the players' side whose picture
//! is on F10. Nobody has to spawn anything; when the zone has been empty a
//! while the fighters go home so the server isn't flying them for nobody.
//!
//! Each player's trip through the zone is one `HotZoneResult`: time inside,
//! what they killed, what was shot at them and whether they came home.

use crate::{
    aa::{self, TrainerKill},
    ag,
    players::{Flying, Players},
    records::{self, Recorder},
    spawn::{self, AirSpec, Spawns},
    util::{self, V3},
};
use anyhow::Result;
use bfprotocols::range::{
    cfg::{awacs_callsign_id, AdversaryCfg, HotZoneCfg, HotZoneSiteCfg, RangeCfg},
    HotZoneOutcome, HotZoneResult, LiveHotZone, PilotRef, RangeResult,
};
use dcso3::{coalition::Side, env::miz::GroupId, unit::Unit, MizLua};
use fxhash::{FxHashMap, FxHashSet};
use log::{info, warn};
use serde_json::json;
use std::collections::BTreeMap;

/// Ground compositions for hot-zone sites: (key, label, red kit, blue kit).
pub const HOT_ZONE_COMPOSITIONS: &[(&str, &str, &[&str], &[&str])] = &[
    ("armor", "Armour platoon", &["T-72B", "T-72B", "BMP-2", "BMP-2"], &["M-1 Abrams", "M-1 Abrams", "M-2 Bradley", "M-2 Bradley"]),
    ("armor_modern", "Modern armour", &["CHAP_T90M", "CHAP_T90M", "CHAP_BMPT", "BMP-3"], &["Leopard-2", "Leopard-2", "Marder", "CHAP_M1130"]),
    ("trucks", "Supply trucks", &["Ural-375", "Ural-375", "KAMAZ Truck", "GAZ-66"], &["M 818", "M 818", "M 818", "Hummer"]),
    ("artillery", "Artillery battery", &["SAU Msta", "SAU Msta", "Grad-URAL", "Ural-375"], &["M-109", "M-109", "MLRS", "M 818"]),
    ("aaa", "AAA", &["ZSU-23-4 Shilka", "Ural-375 ZU-23", "ZU-23 Emplacement"], &["Gepard", "Vulcan", "Vulcan"]),
    ("shorad", "SHORAD", &["Strela-10M3", "2S6 Tunguska"], &["M1097 Avenger", "M6 Linebacker"]),
    ("manpads", "MANPADS team", &["SA-18 Igla manpad", "SA-18 Igla comm", "Infantry AK"], &["Soldier stinger", "Stinger comm", "Soldier M4"]),
    ("sam_short", "Short-range SAM", &["Tor 9A331", "2S6 Tunguska"], &["Roland ADS", "Roland Radar", "Gepard"]),
    ("sam_modern", "Modern SHORAD", &["CHAP_PantsirS1", "CHAP_TorM2"], &["CHAP_IRISTSLM_STR", "CHAP_IRISTSLM_LN", "CHAP_IRISTSLM_CP"]),
    ("hq", "Command post", &["BTR-80", "ZIL-131 KUNG", "Ural-375", "ZSU-23-4 Shilka"], &["M1043 HMMWV Armament", "Hummer", "M 818", "Vulcan"]),
];

pub fn composition(key: &str, side: Side) -> Option<(&'static str, &'static [&'static str])> {
    HOT_ZONE_COMPOSITIONS
        .iter()
        .find(|(k, ..)| *k == key)
        .map(|(_, l, red, blue)| (*l, if side == Side::Blue { *blue } else { *red }))
}

#[derive(Debug)]
struct Site {
    cfg: HotZoneSiteCfg,
    pos: V3,
    group: String,
    typ: FxHashMap<String, String>,
    alive: FxHashSet<String>,
    dead_at: Option<f64>,
}

#[derive(Debug)]
struct Flight {
    group: String,
    typ: String,
}

#[derive(Debug)]
struct Zone {
    cfg: HotZoneCfg,
    centre: V3,
    radius: f64,
    ai: Side,
    sites: Vec<Site>,
    flights: Vec<Flight>,
    next_launch: f64,
    last_occupied: f64,
    awacs: Option<String>,
}

#[derive(Debug)]
struct Visit {
    zone: usize,
    pilot: PilotRef,
    typ: String,
    side: Side,
    callsign: String,
    gid: GroupId,
    time_in: f64,
    last_t: f64,
    outside_since: Option<f64>,
    air_kills: u32,
    ground_kills: u32,
    kills: Vec<String>,
    shots: u32,
    missiles_at: u32,
    trainer_deaths: u32,
}

#[derive(Debug, Default)]
pub struct HotZones {
    zones: Vec<Zone>,
    /// player unit name -> their current trip through a zone
    visits: FxHashMap<String, Visit>,
    last: f64,
    seq: u64,
}

fn spawn_site(lua: MizLua, z: &HotZoneCfg, ai: Side, s: &mut Site, spawns: &mut Spawns, now: f64) -> Result<()> {
    let (_, types) = composition(&s.cfg.composition, ai)
        .ok_or_else(|| anyhow::anyhow!("unknown composition {:?}", s.cfg.composition))?;
    let country = spawn::country_id(z.country.as_deref().unwrap_or(spawn::default_country(ai)))?;
    let n = types.len().max(1);
    let units: Vec<(String, V3, f64)> = types
        .iter()
        .enumerate()
        .map(|(i, t)| {
            let a = i as f64 * 360. / n as f64;
            let p = util::offset(s.pos, a, if i == 0 { 0. } else { 45. }, 0.);
            let mut q = util::nearest_land(lua, p, 200.).unwrap_or(p);
            q.y = util::ground_height(lua, q);
            (t.to_string(), q, a)
        })
        .collect();
    let g = spawn::surface_group(&s.group, &units, "Good", vec![spawn::ground_waypoint(s.pos, 0., false, vec![])]);
    spawn::add_group(lua, country, spawn::GROUND, &g)?;
    s.typ.clear();
    s.alive.clear();
    for (i, (t, ..)) in units.iter().enumerate() {
        let name = format!("{}-{}", s.group, i + 1);
        s.typ.insert(name.clone(), t.clone());
        s.alive.insert(name);
    }
    s.dead_at = None;
    // weapons free, radars up
    spawns.defer(now + 2., spawn::Pending::GroupOption { group: s.group.clone(), id: 0, value: json!(2) });
    spawns.defer(now + 2., spawn::Pending::GroupOption { group: s.group.clone(), id: 9, value: json!(2) });
    Ok(())
}

fn spawn_awacs(lua: MizLua, z: &HotZoneCfg, side: Side, spawns: &mut Spawns, now: f64) -> Result<Option<String>> {
    let Some(a) = &z.awacs else { return Ok(None) };
    let start = ag::resolve(lua, &a.loc)?;
    let alt = a.alt_ft / util::M_TO_FT;
    let end = util::offset(start, a.heading_deg, a.leg_nm * util::NM, 0.);
    let name = format!("RNG-AWACS-{}", z.id);
    let speed = 180.;
    let orbit = json!({
        "id": "Orbit",
        "params": { "pattern": "Race-Track", "point": { "x": start.x, "y": start.z }, "point2": { "x": end.x, "y": end.z }, "speed": speed, "altitude": alt }
    });
    let spec = AirSpec {
        name: name.clone(),
        typ: a.typ.clone(),
        count: 1,
        skill: "Excellent".into(),
        pos: start,
        alt_m: alt,
        speed_ms: speed,
        hdg: a.heading_deg,
        pylons: Default::default(),
        livery: None,
        callsign: Some((awacs_callsign_id(&a.callsign), a.callsign_number as i64, a.callsign.clone())),
        freq_mhz: Some(a.freq_mhz),
        task: "AWACS".into(),
        route: vec![
            spawn::waypoint(
                start,
                alt,
                speed,
                vec![
                    spawn::task_entry(1, json!({ "id": "AWACS", "params": {} })),
                    spawn::wrapped(2, json!({ "id": "EPLRS", "params": { "value": true } })),
                    spawn::task_entry(3, orbit),
                ],
            ),
            spawn::waypoint(end, alt, speed, vec![]),
        ],
        fuel_kg: crate::harvest::max_fuel_kg(&a.typ),
        side,
    };
    spawn::add_group(lua, spawn::country_id(spawn::default_country(side))?, spawn::AIRPLANE, &spawn::air_group(&spec))?;
    for c in [spawn::bool_cmd("SetImmortal", true), spawn::bool_cmd("SetUnlimitedFuel", true), spawn::bool_cmd("SetInvisible", true)] {
        spawns.defer(now + 2., spawn::Pending::GroupCommand { group: name.clone(), cmd: c });
    }
    spawns.defer(now + 2.5, spawn::Pending::GroupCommand { group: name.clone(), cmd: spawn::set_frequency_cmd(a.freq_mhz) });
    info!("hot zone {}: AWACS {} {} up on {:.1} MHz", z.id, a.callsign, a.typ, a.freq_mhz);
    Ok(Some(name))
}

fn aspect(bandit_hdg: f64, bandit_to_player: f64) -> &'static str {
    let off = util::angdiff(bandit_hdg, bandit_to_player).abs();
    if off < 30. {
        "hot"
    } else if off < 70. {
        "flanking"
    } else if off < 110. {
        "beam"
    } else {
        "drag"
    }
}

impl HotZones {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg, spawns: &mut Spawns, now: f64) {
        for zc in &cfg.hot_zones {
            let centre = match ag::resolve(lua, &zc.loc) {
                Ok(p) => p,
                Err(e) => {
                    warn!("hot zone {}: {e:?}", zc.id);
                    continue;
                }
            };
            let ai = spawn::side_of_str(&zc.ai_side);
            let mut z = Zone {
                cfg: zc.clone(),
                centre,
                radius: zc.radius_nm * util::NM,
                ai,
                sites: vec![],
                flights: vec![],
                next_launch: 0.,
                last_occupied: f64::MIN,
                awacs: None,
            };
            for sc in &zc.ground {
                let pos = match ag::resolve(lua, &sc.loc) {
                    Ok(p) => p,
                    Err(e) => {
                        warn!("hot zone {} site {}: {e:?}", zc.id, sc.id);
                        continue;
                    }
                };
                let mut s = Site {
                    cfg: sc.clone(),
                    pos,
                    group: format!("RNG-HZ-{}", sc.id),
                    typ: FxHashMap::default(),
                    alive: FxHashSet::default(),
                    dead_at: None,
                };
                match spawn_site(lua, zc, ai, &mut s, spawns, now) {
                    Ok(()) => z.sites.push(s),
                    Err(e) => warn!("hot zone {} site {}: {e:?}", zc.id, sc.id),
                }
            }
            match spawn_awacs(lua, zc, ai.opposite(), spawns, now) {
                Ok(a) => z.awacs = a,
                Err(e) => warn!("hot zone {} AWACS: {e:?}", zc.id),
            }
            info!("hot zone {} ({}): {} ground sites, {} CAP flights of {}", zc.id, zc.name, z.sites.len(), zc.cap_flights, zc.flight_size);
            self.zones.push(z);
        }
    }

    fn zone_of_unit(&self, unit: &str) -> Option<(usize, bool)> {
        for (i, z) in self.zones.iter().enumerate() {
            if z.flights.iter().any(|f| spawn::in_group(unit, &f.group)) {
                return Some((i, true));
            }
            if z.sites.iter().any(|s| spawn::in_group(unit, &s.group)) {
                return Some((i, false));
            }
        }
        None
    }

    pub fn is_zone_unit(&self, unit: &str) -> bool {
        self.zone_of_unit(unit).is_some()
    }

    fn launch(&mut self, lua: MizLua, zi: usize, spawns: &mut Spawns, toward: V3, now: f64) -> Result<()> {
        self.seq += 1;
        let z = &self.zones[zi];
        let typ = z.cfg.cap_types[(self.seq as usize) % z.cfg.cap_types.len()].clone();
        // from the far side of the zone, pointing at the player who is in it
        let brg = util::bearing(toward, z.centre);
        let mut pos = util::offset(z.centre, brg, z.radius * 0.9, 0.);
        let alt = z.cfg.cap_alt_ft / util::M_TO_FT;
        pos.y = alt.max(util::ground_height(lua, pos) + 600.);
        let adv = AdversaryCfg {
            typ: typ.clone(),
            label: typ.clone(),
            loadouts: BTreeMap::new(),
            side: records::side_str(z.ai).into(),
            country: z.cfg.country.clone().unwrap_or_else(|| spawn::default_country(z.ai).into()),
            livery: None,
        };
        let pylons = aa::loadout(&adv, &z.cfg.cap_weapons);
        let name = format!("RNG-HZCAP-{}-{}", z.cfg.id, self.seq);
        let speed = 220.;
        let engage = json!({
            "id": "EngageTargetsInZone",
            "params": { "point": { "x": z.centre.x, "y": z.centre.z }, "zoneRadius": z.radius * 1.2, "targetTypes": ["Air"], "priority": 0 }
        });
        let orbit = json!({
            "id": "Orbit",
            "params": { "pattern": "Circle", "point": { "x": z.centre.x, "y": z.centre.z }, "speed": speed, "altitude": pos.y }
        });
        let spec = AirSpec {
            name: name.clone(),
            typ: typ.clone(),
            count: z.cfg.flight_size.clamp(1, 4),
            skill: z.cfg.cap_skill.clone(),
            pos,
            alt_m: pos.y,
            speed_ms: speed,
            hdg: (brg + 180.).rem_euclid(360.),
            pylons,
            livery: None,
            callsign: None,
            freq_mhz: None,
            task: "CAP".into(),
            route: vec![
                spawn::waypoint(
                    pos,
                    pos.y,
                    speed,
                    vec![
                        spawn::wrapped_option(1, 0, json!(0)), // ROE weapons free
                        spawn::wrapped_option(2, 1, json!(2)), // reaction: evade fire
                        spawn::wrapped_option(3, 3, json!(3)), // radar: continuous search
                        spawn::task_entry(4, engage),
                    ],
                ),
                spawn::waypoint(z.centre, pos.y, speed, vec![spawn::task_entry(1, orbit)]),
            ],
            fuel_kg: crate::harvest::max_fuel_kg(&typ),
            side: z.ai,
        };
        let country = spawn::country_id(&adv.country).or_else(|_| spawn::country_id(spawn::default_country(z.ai)))?;
        spawn::add_group(lua, country, spawn::AIRPLANE, &spawn::air_group(&spec))?;
        spawns.defer(now + 1.5, spawn::Pending::GroupCommand { group: name.clone(), cmd: spawn::bool_cmd("SetUnlimitedFuel", true) });
        info!("hot zone {}: CAP {} x{} launched", z.cfg.id, typ, spec.count);
        self.zones[zi].flights.push(Flight { group: name, typ });
        Ok(())
    }

    /// Occupancy, the CAP, rebuilding ground sites, finishing trips; every 2 s.
    pub fn tick(&mut self, lua: MizLua, cfg: &RangeCfg, players: &Players, spawns: &mut Spawns, rec: &mut Recorder, now: f64) {
        if now - self.last < 2. || self.zones.is_empty() {
            return;
        }
        let dt = if self.last == 0. { 0. } else { now - self.last };
        self.last = now;
        let mut finished = vec![];
        for zi in 0..self.zones.len() {
            // who is inside
            let (ai, centre, radius) = (self.zones[zi].ai, self.zones[zi].centre, self.zones[zi].radius);
            let mut inside: Vec<&Flying> = vec![];
            for f in players.flying.values().filter(|f| f.side != ai && !f.is_ground) {
                let d = util::dist2(f.pos, centre);
                let v = self.visits.get_mut(&f.unit_name);
                if d <= radius && f.in_air {
                    inside.push(f);
                    match v {
                        Some(v) if v.zone == zi => {
                            v.time_in += dt;
                            v.outside_since = None;
                            v.last_t = now;
                        }
                        Some(_) => (),
                        None => {
                            records::to_group(
                                lua,
                                f.group_id,
                                &format!(
                                    "HOT ZONE {}: weapons free, everything here fights back. {}Missile trainer {}.",
                                    self.zones[zi].cfg.name,
                                    self.zones[zi]
                                        .cfg
                                        .awacs
                                        .as_ref()
                                        .map(|a| format!("{} {} on {:.1} MHz; picture on F10 > Range > Hot zones. ", a.callsign, a.callsign_number, a.freq_mhz))
                                        .unwrap_or_default(),
                                    if f.trainer { "ON" } else { "OFF - missiles are live" }
                                ),
                                15,
                            );
                            self.visits.insert(
                                f.unit_name.clone(),
                                Visit {
                                    zone: zi,
                                    pilot: records::pilot_of(f),
                                    typ: f.typ.clone(),
                                    side: f.side,
                                    callsign: f.group_name.clone(),
                                    gid: f.group_id,
                                    time_in: 0.,
                                    last_t: now,
                                    outside_since: None,
                                    air_kills: 0,
                                    ground_kills: 0,
                                    kills: vec![],
                                    shots: 0,
                                    missiles_at: 0,
                                    trainer_deaths: 0,
                                },
                            );
                        }
                    }
                } else if let Some(v) = v {
                    if v.zone == zi && d > radius * 1.1 {
                        let since = *v.outside_since.get_or_insert(now);
                        if now - since > 60. {
                            finished.push((f.unit_name.clone(), HotZoneOutcome::Egressed));
                        }
                    }
                }
            }
            let occupied = !inside.is_empty();
            let toward = inside.first().map(|f| f.pos);
            let z = &mut self.zones[zi];
            if occupied {
                z.last_occupied = now;
            }
            // the CAP
            let before = z.flights.len();
            z.flights.retain(|f| spawn::group_exists(lua, &f.group));
            if z.flights.len() < before {
                z.next_launch = z.next_launch.max(now + z.cfg.cap_respawn_s as f64);
            }
            if !occupied && now - z.last_occupied > z.cfg.idle_despawn_s as f64 && !z.flights.is_empty() {
                info!("hot zone {}: empty for {} s, CAP goes home", z.cfg.id, z.cfg.idle_despawn_s);
                for f in z.flights.drain(..) {
                    spawn::destroy_group(lua, &f.group);
                }
            }
            if let Some(t) = toward {
                let z = &self.zones[zi];
                if (z.flights.len() as u32) < z.cfg.cap_flights && now >= z.next_launch && !z.cfg.cap_types.is_empty() {
                    let gap = if z.flights.is_empty() { 60. } else { 120. };
                    if let Err(e) = self.launch(lua, zi, spawns, t, now) {
                        warn!("hot zone {} CAP launch: {e:?}", self.zones[zi].cfg.id)
                    }
                    self.zones[zi].next_launch = now + gap;
                }
            }
            // ground sites
            let z = &mut self.zones[zi];
            for si in 0..z.sites.len() {
                let s = &mut z.sites[si];
                if s.alive.is_empty() {
                    let at = *s.dead_at.get_or_insert(now);
                    if now - at >= s.cfg.respawn_s as f64 {
                        spawn::destroy_group(lua, &s.group);
                        let (c, a) = (z.cfg.clone(), z.ai);
                        if let Err(e) = spawn_site(lua, &c, a, s, spawns, now) {
                            warn!("hot zone {} site {} respawn: {e:?}", c.id, s.cfg.id)
                        }
                    }
                }
            }
            // AWACS: put a new one up if it went away
            if z.cfg.awacs.is_some() && z.awacs.as_ref().map(|g| !spawn::group_exists(lua, g)).unwrap_or(true) {
                let c = z.cfg.clone();
                match spawn_awacs(lua, &c, z.ai.opposite(), spawns, now) {
                    Ok(a) => self.zones[zi].awacs = a,
                    Err(e) => warn!("hot zone {} AWACS: {e:?}", c.id),
                }
            }
        }
        for (unit, outcome) in finished {
            self.finish(lua, cfg, rec, &unit, outcome);
        }
    }

    fn finish(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, unit: &str, outcome: HotZoneOutcome) {
        let Some(v) = self.visits.remove(unit) else { return };
        let kills = v.air_kills + v.ground_kills;
        if v.time_in < 30. && kills == 0 {
            return;
        }
        let zone = self.zones.get(v.zone).map(|z| z.cfg.name.clone()).unwrap_or_default();
        let res = HotZoneResult {
            zone: zone.clone(),
            time_in_zone_s: v.time_in,
            air_kills: v.air_kills,
            ground_kills: v.ground_kills,
            kills: v.kills.clone(),
            shots_fired: v.shots,
            missiles_defeated: v.missiles_at.saturating_sub(v.trainer_deaths),
            trainer_deaths: v.trainer_deaths,
            outcome,
        };
        let score = match outcome {
            HotZoneOutcome::ShotDown => Some(1.),
            HotZoneOutcome::Left if kills == 0 => None,
            _ => {
                let mut s: f64 = 3.;
                if kills >= 1 {
                    s += 1.
                }
                if kills >= 3 {
                    s += 1.
                }
                s -= v.trainer_deaths as f64;
                Some(s.clamp(1., 5.))
            }
        };
        if cfg.in_game_results && outcome != HotZoneOutcome::Left {
            records::to_group(
                lua,
                v.gid,
                &format!(
                    "HOT ZONE {}: {} air and {} ground kills in {:.0} min, {} missiles defeated, {} would have killed you - {}",
                    zone,
                    v.air_kills,
                    v.ground_kills,
                    v.time_in / 60.,
                    res.missiles_defeated,
                    v.trainer_deaths,
                    outcome.label()
                ),
                cfg.message_s,
            );
        }
        rec.emit(lua, v.pilot, &v.typ, v.side, &v.callsign, score, RangeResult::HotZone(res), None);
    }

    /// The player's aircraft is gone: finish their trip.
    pub fn player_out(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, unit: &str, died: bool) {
        let o = if died { HotZoneOutcome::ShotDown } else { HotZoneOutcome::Left };
        self.finish(lua, cfg, rec, unit, o);
    }

    pub fn landed(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, unit: &str) {
        if self.visits.contains_key(unit) {
            self.finish(lua, cfg, rec, unit, HotZoneOutcome::Landed);
        }
    }

    /// A shot: count the player's, and missiles hot-zone AI fire at players.
    pub fn on_shot(&mut self, shooter_unit: &str, target: Option<&str>, missile: bool) {
        if let Some(v) = self.visits.get_mut(shooter_unit) {
            v.shots += 1;
            return;
        }
        if missile && self.is_zone_unit(shooter_unit) {
            if let Some(v) = target.and_then(|t| self.visits.get_mut(t)) {
                v.missiles_at += 1;
            }
        }
    }

    pub fn trainer_kill(&mut self, k: &TrainerKill) {
        if self.is_zone_unit(&k.shooter_unit) {
            if let Some(v) = self.visits.get_mut(&k.target_unit) {
                v.trainer_deaths += 1;
            }
        }
    }

    pub fn unit_dead(&mut self, unit: &str, now: f64) {
        for z in self.zones.iter_mut() {
            for s in z.sites.iter_mut() {
                if s.alive.remove(unit) && s.alive.is_empty() {
                    s.dead_at = Some(now);
                }
            }
        }
    }

    pub fn on_kill(&mut self, shooter_unit: Option<&str>, target_unit: &str, target_type: &str, now: f64) {
        let which = self.zone_of_unit(target_unit);
        self.unit_dead(target_unit, now);
        let (Some((_, air)), Some(s)) = (which, shooter_unit) else { return };
        if let Some(v) = self.visits.get_mut(s) {
            if air {
                v.air_kills += 1
            } else {
                v.ground_kills += 1
            }
            v.kills.push(target_type.to_string());
        }
    }

    /// F10 > Hot zones > Picture: the AWACS's view of the bandits, BRAA from you.
    pub fn picture(&self, lua: MizLua, f: &Flying) -> String {
        let mine: Vec<&Zone> = self.zones.iter().filter(|z| z.ai != f.side).collect();
        if mine.is_empty() {
            return "No hot zones for your side".into();
        }
        let mut out = vec![];
        for z in mine {
            let caller = z
                .cfg
                .awacs
                .as_ref()
                .filter(|_| z.awacs.as_ref().map(|g| spawn::group_exists(lua, g)).unwrap_or(false))
                .map(|a| format!("{} {}", a.callsign, a.callsign_number));
            let Some(caller) = caller else {
                out.push(format!("{}: no AWACS on station", z.cfg.name));
                continue;
            };
            let mut groups = vec![];
            for fl in &z.flights {
                let mut n = 0;
                let mut lead: Option<(V3, V3)> = None;
                for i in 1..=4 {
                    if let Ok(u) = Unit::get_by_name(lua, &format!("{}-{i}", fl.group)) {
                        if let (Ok(p), Ok(v)) = (u.get_point(), u.get_velocity()) {
                            n += 1;
                            lead.get_or_insert((p.0, v.0));
                        }
                    }
                }
                if let Some((p, v)) = lead {
                    let d = util::dist2(f.pos, p);
                    groups.push((
                        d,
                        format!(
                            "  {n}x {} BRAA {:03.0}/{:.0}/{:.0}k {}",
                            fl.typ,
                            util::bearing(f.pos, p),
                            d / util::NM,
                            (p.y * util::M_TO_FT / 1000.).round(),
                            aspect(util::hdg(v), util::bearing(p, f.pos))
                        ),
                    ));
                }
            }
            groups.sort_by(|a, b| a.0.total_cmp(&b.0));
            if groups.is_empty() {
                out.push(format!("{caller}, {}: picture clean", z.cfg.name));
            } else {
                out.push(format!("{caller}, {}: {} group(s)", z.cfg.name, groups.len()));
                out.extend(groups.into_iter().map(|(_, s)| s));
            }
        }
        out.join("\n")
    }

    pub fn describe(&self, from: V3, side: Side) -> Vec<String> {
        self.zones
            .iter()
            .filter(|z| z.ai != side)
            .map(|z| {
                let (alive, total): (usize, usize) = z.sites.iter().fold((0, 0), |a, s| (a.0 + s.alive.len(), a.1 + s.typ.len()));
                format!(
                    "{} {:03.0}/{:.0}nm r{:.0}nm: {} CAP flight(s) up, ground {alive}/{total}{}",
                    z.cfg.name,
                    util::bearing(from, z.centre),
                    util::dist2(from, z.centre) / util::NM,
                    z.cfg.radius_nm,
                    z.flights.len(),
                    z.cfg.awacs.as_ref().map(|a| format!(", AWACS {} {:.1} MHz", a.callsign, a.freq_mhz)).unwrap_or_default()
                )
            })
            .collect()
    }

    pub fn live(&self, lua: MizLua) -> Vec<LiveHotZone> {
        self.zones
            .iter()
            .enumerate()
            .map(|(zi, z)| LiveHotZone {
                id: z.cfg.id.clone(),
                name: z.cfg.name.clone(),
                pos: util::geo(lua, z.centre),
                radius_m: z.radius,
                ai_side: records::side_str(z.ai).into(),
                bandits: z.flights.len() as u32 * z.cfg.flight_size,
                players: self.visits.values().filter(|v| v.zone == zi).map(|v| v.pilot.name.clone()).collect(),
                ground_alive: z.sites.iter().map(|s| s.alive.len() as u32).sum(),
                ground_total: z.sites.iter().map(|s| s.typ.len() as u32).sum(),
                awacs: z.cfg.awacs.as_ref().map(|a| format!("{} {} {:.1} MHz", a.callsign, a.callsign_number, a.freq_mhz)),
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compositions_have_both_sides() {
        for (k, _, red, blue) in HOT_ZONE_COMPOSITIONS {
            assert!(!red.is_empty() && !blue.is_empty(), "{k}");
        }
        assert!(composition("armor", Side::Blue).unwrap().1.contains(&"M-1 Abrams"));
        assert!(composition("nope", Side::Red).is_none());
    }

    #[test]
    fn aspects() {
        assert_eq!(aspect(90., 95.), "hot");
        assert_eq!(aspect(0., 90.), "beam");
        assert_eq!(aspect(0., 180.), "drag");
    }
}
