// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Air-to-air refuelling: keeping tankers on station, and grading receivers.
//!
//! A session starts when a player is within a mile in trail of a tanker and
//! ends when they leave. While it runs we sample the receiver's position in
//! the tanker's own body frame five times a second. "Connected" is the DCS
//! REFUELING event -- which, on a dedicated server, names the TANKER instead
//! of a client receiver (open ED bug), so the receiver is resolved by
//! proximity -- backed up by the receiver's fuel actually rising. Stability
//! is measured from the connected samples themselves, so no per-tanker
//! boom/basket geometry is needed.

use crate::{
    harvest,
    players::{Flying, Players},
    records::{self, Recorder},
    spawn::{self, AirSpec, Spawn, SpawnKind, Spawns},
    util::{self, Frame, V3},
};
use anyhow::{anyhow, bail, Result};
use bfprotocols::range::{
    cfg::{Loc, RangeCfg, TacanBand, TacanCfg, TankerCfg},
    grading, AarResult, LiveTanker, PilotRef, RangeResult, RefuelMethod, RelSample, Stability,
    TankerState, Track,
};
use chrono::{Duration, Utc};
use dcso3::{unit::Unit, MizLua};
use fxhash::FxHashMap;
use log::{info, warn};
use serde_json::json;

#[derive(Debug)]
pub struct Tanker {
    pub cfg: TankerCfg,
    pub method: RefuelMethod,
    pub group: String,
    pub state: TankerState,
    pub respawn_at: Option<f64>,
    pub owner: Option<(String, String)>,
    pub recovery_for: Option<String>,
    pub spawn_id: Option<String>,
}

impl Tanker {
    pub fn unit_name(&self) -> String {
        format!("{}-1", self.group)
    }
}

#[derive(Debug)]
struct Session {
    tanker: String,
    pilot: PilotRef,
    typ: String,
    side: dcso3::coalition::Side,
    group_id: dcso3::env::miz::GroupId,
    callsign: String,
    start: f64,
    contacts: u32,
    disconnects: u32,
    connected: bool,
    event_open: bool,
    time_connected: f64,
    first_contact: Option<f64>,
    fuel_last: Option<f64>,
    fuel_gained: f64,
    samples: Vec<RelSample>,
    precontact_closure: Option<f64>,
    overshoot: bool,
    last_close: f64,
    last_sample: f64,
    alt_ft: f64,
    speed_kts: f64,
}

#[derive(Debug, Default)]
pub struct Aar {
    pub tankers: Vec<Tanker>,
    sessions: FxHashMap<String, Session>,
    tacan_pool: u8,
}

pub fn tanker_speed_ms(t: &TankerCfg) -> f64 {
    t.speed_kts
        .or_else(|| bfprotocols::range::cfg::tanker_type(&t.typ).map(|tt| tt.default_speed_kts))
        .unwrap_or(280.)
        / util::MS_TO_KTS
}

/// Build and add a tanker group. Returns the group name.
pub fn spawn_tanker(
    lua: MizLua,
    t: &TankerCfg,
    start: V3,
    spawns: &mut Spawns,
    recovery_carrier_group: Option<i64>,
    now: f64,
) -> Result<String> {
    let tt = bfprotocols::range::cfg::tanker_type(&t.typ)
        .ok_or_else(|| anyhow!("{} is not a tanker type", t.typ))?;
    let name = format!("RNG-TKR-{}", t.id);
    let alt = t.alt_ft / util::M_TO_FT;
    let speed = tanker_speed_ms(t);
    let mut pos = start;
    pos.y = alt;
    let p2 = util::offset(pos, t.heading_deg, t.leg_nm * util::NM, 0.);
    let mut pylons = t.pylons.clone();
    if let Some((n, clsid)) = tt.required_store {
        pylons.entry(n).or_insert_with(|| clsid.to_string());
    }
    let mut tasks = vec![spawn::task_entry(1, json!({ "id": "Tanker", "params": {} }))];
    match recovery_carrier_group {
        Some(gid) => tasks.push(spawn::task_entry(
            2,
            json!({ "id": "RecoveryTanker", "params": { "groupId": gid, "speed": speed, "altitude": alt, "lastWptIndexFlag": false } }),
        )),
        None => tasks.push(spawn::task_entry(
            2,
            json!({ "id": "Orbit", "params": {
                "pattern": "Race-Track",
                "point": { "x": pos.x, "y": pos.z },
                "point2": { "x": p2.x, "y": p2.z },
                "speed": speed,
                "altitude": alt
            } }),
        )),
    }
    let side = spawn::side_of_str(&t.side);
    let spec = AirSpec {
        name: name.clone(),
        typ: t.typ.clone(),
        count: 1,
        skill: "Excellent".into(),
        pos,
        alt_m: alt,
        speed_ms: speed,
        hdg: t.heading_deg,
        pylons,
        livery: t.livery.clone(),
        callsign: Some((spawn::tanker_callsign_id(&t.callsign), t.callsign_number as i64, t.callsign.clone())),
        freq_mhz: Some(t.freq_mhz),
        task: "Refueling".into(),
        route: vec![spawn::waypoint(pos, alt, speed, tasks), spawn::waypoint(p2, alt, speed, vec![])],
        fuel_kg: harvest::max_fuel_kg(&t.typ),
        side,
    };
    let country = spawn::country_id(&t.country).or_else(|_| spawn::country_id(spawn::default_country(side)))?;
    spawn::add_group(lua, country, spawn::AIRPLANE, &spawn::air_group(&spec))?;
    for c in [
        spawn::bool_cmd("SetUnlimitedFuel", true),
        spawn::bool_cmd("SetImmortal", true),
        spawn::bool_cmd("SetInvisible", true),
        spawn::set_frequency_cmd(t.freq_mhz),
    ] {
        spawns.defer(now + 2., spawn::Pending::GroupCommand { group: name.clone(), cmd: c });
    }
    if t.tacan.is_some() {
        // needs the unit id, which only exists after the spawn: done in
        // `activate_tacans` on the next slow tick
    }
    info!(
        "tanker {} ({} {}{}) on station, {:.0} ft, {:.3} MHz{}",
        t.id,
        t.typ,
        t.callsign,
        t.callsign_number,
        t.alt_ft,
        t.freq_mhz,
        t.tacan.as_ref().map(|t| format!(", TACAN {}", t.describe())).unwrap_or_default()
    );
    Ok(name)
}

impl Aar {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg, spawns: &mut Spawns, now: f64) {
        for t in cfg.tankers.iter().filter(|t| t.permanent) {
            let start = match crate::ag::resolve(lua, &t.loc) {
                Ok(p) => p,
                Err(e) => {
                    warn!("tanker {}: {e:?}", t.id);
                    continue;
                }
            };
            let method = bfprotocols::range::cfg::tanker_type(&t.typ).map(|tt| tt.method).unwrap_or(RefuelMethod::Drogue);
            match spawn_tanker(lua, t, start, spawns, None, now) {
                Ok(group) => self.tankers.push(Tanker {
                    cfg: t.clone(),
                    method,
                    group,
                    state: TankerState::Spawning,
                    respawn_at: None,
                    owner: None,
                    recovery_for: None,
                    spawn_id: None,
                }),
                Err(e) => warn!("tanker {} not spawned: {e:?}", t.id),
            }
        }
    }

    /// Next free TACAN channel for an on-demand tanker (40Y..59Y).
    fn next_tacan(&mut self) -> TacanCfg {
        self.tacan_pool = (self.tacan_pool + 1) % 20;
        TacanCfg { channel: 40 + self.tacan_pool, band: TacanBand::Y, morse: format!("T{:02}", 40 + self.tacan_pool) }
    }

    /// Spawn a tanker for a player: a 20 nm race-track starting where they
    /// are, along their heading.
    #[allow(clippy::too_many_arguments)]
    pub fn spawn_for(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        spawns: &mut Spawns,
        player: &Flying,
        typ: &str,
        alt_ft: f64,
        leg_nm: f64,
        now: f64,
    ) -> Result<String> {
        if !player.in_air {
            bail!("you need to be airborne to call a tanker to your position")
        }
        let tt = bfprotocols::range::cfg::tanker_type(typ).ok_or_else(|| anyhow!("{typ} is not a tanker"))?;
        let n = self.tankers.len() + 1;
        let id = format!("od{}", spawns.next_name("t").trim_start_matches("RNG-t-"));
        let tacan = self.next_tacan();
        let freq = 250.0 + (n % 9) as f64 + 0.5;
        let g = util::geo(lua, player.pos);
        let t = TankerCfg {
            id: id.clone(),
            callsign: "Shell".into(),
            callsign_number: (n % 9) as u8 + 1,
            typ: typ.into(),
            side: records::side_str(player.side).into(),
            country: spawn::default_country(player.side).into(),
            loc: Loc::latlon(g.lat, g.lon),
            heading_deg: util::hdg(player.vel),
            leg_nm,
            alt_ft,
            speed_kts: None,
            freq_mhz: freq,
            tacan: Some(tacan.clone()),
            permanent: false,
            livery: None,
            pylons: Default::default(),
        };
        let start = util::offset(player.pos, t.heading_deg, 3. * util::NM, 0.);
        let group = spawn_tanker(lua, &t, start, spawns, None, now)?;
        let spawn_id = group.clone();
        spawns.insert(Spawn {
            id: spawn_id.clone(),
            item: "tanker".into(),
            label: format!("Tanker {} {}{} ({typ})", t.callsign, t.callsign, t.callsign_number),
            owner: Some(player.ucid.to_string()),
            owner_name: player.name.clone(),
            groups: vec![group.clone()],
            statics: vec![],
            created: Utc::now(),
            expires: Some(Utc::now() + Duration::seconds(cfg.spawn.despawn_after_s as i64)),
            units: 1,
            pos: start,
            kind: SpawnKind::Tanker,
        });
        self.tankers.push(Tanker {
            cfg: t.clone(),
            method: tt.method,
            group,
            state: TankerState::Spawning,
            respawn_at: None,
            owner: Some((player.ucid.to_string(), player.name.clone())),
            recovery_for: None,
            spawn_id: Some(spawn_id.clone()),
        });
        records::to_group(
            lua,
            player.group_id,
            &format!(
                "Tanker {}{} ({typ}) ahead of you: {:.0} ft, {:.1} MHz, TACAN {}. Race-track {:.0} nm along {:03.0}.",
                t.callsign,
                t.callsign_number,
                alt_ft,
                freq,
                tacan.describe(),
                leg_nm,
                t.heading_deg
            ),
            20,
        );
        Ok(spawn_id)
    }

    pub fn add_recovery_tanker(&mut self, t: Tanker) {
        self.tankers.push(t)
    }

    pub fn remove_spawned(&mut self, spawn_id: &str) {
        self.tankers.retain(|t| t.spawn_id.as_deref() != Some(spawn_id));
    }

    /// TACAN needs the unit id; activate once the tanker exists, and keep
    /// respawning permanent tankers that died.
    pub fn slow_tick(&mut self, lua: MizLua, spawns: &mut Spawns, now: f64) {
        for t in self.tankers.iter_mut() {
            let exists = spawn::group_exists(lua, &t.group);
            match t.state {
                TankerState::Spawning if exists => {
                    if let (Some(tac), Some(uid)) = (&t.cfg.tacan, spawn::first_unit_id(lua, &t.group)) {
                        spawns.defer(now + 0.5, spawn::Pending::UnitCommand {
                            unit: t.unit_name(),
                            cmd: spawn::tacan_cmd(uid, tac, true),
                        });
                    }
                    t.state = TankerState::OnStation;
                }
                TankerState::OnStation if !exists => {
                    t.state = TankerState::Dead;
                    if t.cfg.permanent {
                        t.respawn_at = Some(now + 60.);
                        warn!("tanker {} lost, respawning in 60 s", t.cfg.id);
                    }
                }
                TankerState::Dead => {
                    if let Some(r) = t.respawn_at {
                        if now >= r && t.recovery_for.is_none() {
                            t.respawn_at = None;
                            match crate::ag::resolve(lua, &t.cfg.loc)
                                .and_then(|p| spawn_tanker(lua, &t.cfg, p, spawns, None, now))
                            {
                                Ok(g) => {
                                    t.group = g;
                                    t.state = TankerState::Spawning;
                                }
                                Err(e) => warn!("tanker {} respawn failed: {e:?}", t.cfg.id),
                            }
                        }
                    }
                }
                _ => (),
            }
        }
    }

    pub fn active(&self) -> bool {
        !self.sessions.is_empty()
    }

    /// REFUELING / REFUELING_STOP. `unit` is the event initiator's unit name.
    pub fn refuel_event(&mut self, lua: MizLua, unit: &str, start: bool) {
        if let Some(s) = self.sessions.get_mut(unit) {
            s.event_open = start;
            return;
        }
        // the MP bug: the initiator is the tanker; pick its closest receiver
        let Some(t) = self.tankers.iter().find(|t| t.unit_name() == unit) else { return };
        let tpos = match Unit::get_by_name(lua, unit).and_then(|u| u.get_point()) {
            Ok(p) => p.0,
            Err(_) => return,
        };
        let tid = t.cfg.id.clone();
        let best = self
            .sessions
            .iter_mut()
            .filter(|(_, s)| s.tanker == tid)
            .filter_map(|(u, s)| {
                let p = Unit::get_by_name(lua, u).and_then(|u| u.get_point()).ok()?.0;
                Some((util::dist3(p, tpos), s))
            })
            .min_by(|a, b| a.0.total_cmp(&b.0));
        if let Some((_, s)) = best {
            s.event_open = start;
        }
    }

    /// Start/advance/end sessions. Runs at 5 Hz while anyone is near a tanker.
    pub fn tick(&mut self, lua: MizLua, players: &mut Players, rec: &mut Recorder, msg_s: u32, now: f64) {
        let mut frames: Vec<(String, String, Frame, V3)> = vec![];
        for t in self.tankers.iter().filter(|t| t.state == TankerState::OnStation) {
            if let Ok(u) = Unit::get_by_name(lua, &t.unit_name()) {
                if let (Ok(p), Ok(v)) = (u.get_position(), u.get_velocity()) {
                    frames.push((t.cfg.id.clone(), format!("{}{}", t.cfg.callsign, t.cfg.callsign_number), Frame::from_pos(&p), v.0));
                }
            }
        }
        // start sessions
        for f in players.flying.values_mut() {
            if !f.in_air || f.is_ground || self.sessions.contains_key(&f.unit_name) {
                continue;
            }
            for (tid, cs, fr, _) in &frames {
                let (fwd, up, right) = fr.to_local(f.pos);
                if (-1852. ..0.).contains(&fwd) && right.abs() < 600. && up.abs() < 300. {
                    let tk = self.tankers.iter().find(|t| &t.cfg.id == tid);
                    let (tt, tc) = tk.map(|t| (t.cfg.typ.clone(), t.cfg.side.clone())).unwrap_or_default();
                    if spawn::side_of_str(&tc) != f.side {
                        continue;
                    }
                    info!("AAR session: {} ({}) behind {cs} ({tt})", f.name, f.typ);
                    f.activity = Some(format!("AAR {cs}"));
                    self.sessions.insert(
                        f.unit_name.clone(),
                        Session {
                            tanker: tid.clone(),
                            pilot: records::pilot_of(f),
                            typ: f.typ.clone(),
                            side: f.side,
                            group_id: f.group_id,
                            callsign: f.group_name.clone(),
                            start: now,
                            contacts: 0,
                            disconnects: 0,
                            connected: false,
                            event_open: false,
                            time_connected: 0.,
                            first_contact: None,
                            fuel_last: None,
                            fuel_gained: 0.,
                            samples: vec![],
                            precontact_closure: None,
                            overshoot: false,
                            last_close: now,
                            last_sample: now,
                            alt_ft: f.pos.y * util::M_TO_FT,
                            speed_kts: f.vel.norm() * util::MS_TO_KTS,
                        },
                    );
                    break;
                }
            }
        }
        // advance
        let mut ended = vec![];
        for (unit, s) in self.sessions.iter_mut() {
            let Some((_, cs, fr, tvel)) = frames.iter().find(|(id, ..)| *id == s.tanker) else {
                ended.push(unit.clone());
                continue;
            };
            let Ok(u) = Unit::get_by_name(lua, unit) else {
                ended.push(unit.clone());
                continue;
            };
            let (Ok(p), Ok(v)) = (u.get_point(), u.get_velocity()) else {
                ended.push(unit.clone());
                continue;
            };
            let (p, v) = (p.0, v.0);
            let dt = now - s.last_sample;
            s.last_sample = now;
            let (fwd, up, right) = fr.to_local(p);
            let closure = (v - tvel).dot(&fr.x) * util::MS_TO_KTS;
            let fuel_kg = u
                .get_fuel()
                .ok()
                .map(|f| f as f64 * harvest::max_fuel_kg(&s.typ).unwrap_or(5000.));
            let rising = match (s.fuel_last, fuel_kg) {
                (Some(a), Some(b)) if dt > 0. => (b - a) / dt > 0.2,
                _ => false,
            };
            if let (Some(a), Some(b)) = (s.fuel_last, fuel_kg) {
                if b > a {
                    s.fuel_gained += b - a;
                }
            }
            s.fuel_last = fuel_kg;
            let connected = s.event_open || rising;
            if connected && !s.connected {
                s.contacts += 1;
                s.first_contact.get_or_insert(now);
                records::to_group(lua, s.group_id, &format!("{cs}: contact"), 3);
            } else if !connected && s.connected {
                s.disconnects += 1;
                records::to_group(lua, s.group_id, &format!("{cs}: disconnect"), 3);
            }
            s.connected = connected;
            if connected {
                s.time_connected += dt;
            }
            if s.precontact_closure.is_none() && !connected && fwd > -60. && fwd < -20. {
                s.precontact_closure = Some(closure);
            }
            if !connected && fwd > -5. && right.abs() < 60. && up.abs() < 40. {
                s.overshoot = true;
            }
            s.samples.push(RelSample {
                t: now - s.start,
                fwd_m: fwd,
                right_m: right,
                up_m: up,
                connected,
                fuel_kg: fuel_kg.unwrap_or(0.),
                closure_kts: closure,
            });
            let d = util::dist3(p, fr.p);
            if d < 2. * util::NM {
                s.last_close = now;
            }
            if now - s.last_close > 15. || now - s.start > 1800. {
                ended.push(unit.clone());
            }
        }
        for unit in ended {
            if let Some(s) = self.sessions.remove(&unit) {
                if let Some(f) = players.flying.get_mut(&unit) {
                    f.activity = None;
                }
                self.finish(lua, rec, s, msg_s, now);
            }
        }
    }

    /// The receiver left its aircraft: close their session now.
    pub fn player_left(&mut self, lua: MizLua, rec: &mut Recorder, unit: &str, msg_s: u32, now: f64) {
        if let Some(s) = self.sessions.remove(unit) {
            self.finish(lua, rec, s, msg_s, now);
        }
    }

    fn finish(&mut self, lua: MizLua, rec: &mut Recorder, s: Session, msg_s: u32, now: f64) {
        let dur = now - s.start;
        if s.contacts == 0 && dur < 60. {
            return;
        }
        let t = self.tankers.iter().find(|t| t.cfg.id == s.tanker);
        let (tname, ttype, method) = t
            .map(|t| (format!("{}{}", t.cfg.callsign, t.cfg.callsign_number), t.cfg.typ.clone(), t.method))
            .unwrap_or_else(|| (s.tanker.clone(), String::new(), RefuelMethod::Drogue));
        let conn: Vec<&RelSample> = s.samples.iter().filter(|x| x.connected).collect();
        let stat = |f: &dyn Fn(&RelSample) -> f64| -> (f64, f64) {
            if conn.is_empty() {
                return (0., 0.);
            }
            let n = conn.len() as f64;
            let m = conn.iter().map(|x| f(x)).sum::<f64>() / n;
            let sd = (conn.iter().map(|x| (f(x) - m).powi(2)).sum::<f64>() / n).sqrt();
            (m, sd)
        };
        let (mf, sf) = stat(&|x| x.fwd_m);
        let (mr, sr) = stat(&|x| x.right_m);
        let (mu, su) = stat(&|x| x.up_m);
        let join = s.first_contact.map(|fc| fc - s.start);
        let mut calls = vec![];
        let (score, grade) = grading::aar_grade(
            s.contacts,
            s.disconnects,
            join,
            sf,
            sr,
            su,
            s.precontact_closure,
            s.overshoot,
            &mut calls,
        );
        let fuel_lbs = s.fuel_gained * util::KG_TO_LB;
        let res = AarResult {
            tanker: tname.clone(),
            tanker_type: ttype,
            method,
            join_time_s: join,
            contacts: s.contacts,
            disconnects: s.disconnects,
            time_connected_s: s.time_connected,
            fuel_kg: s.fuel_gained,
            fuel_lbs,
            onload_rate_lbs_min: if s.time_connected > 0. { fuel_lbs / (s.time_connected / 60.) } else { 0. },
            stability: Stability {
                fore_aft_sd_m: sf,
                lateral_sd_m: sr,
                vertical_sd_m: su,
                mean_fwd_m: mf,
                mean_right_m: mr,
                mean_up_m: mu,
            },
            precontact_closure_kts: s.precontact_closure,
            overshoot: s.overshoot,
            alt_ft: s.alt_ft,
            speed_kts: s.speed_kts,
            grade: grade.clone(),
            calls: calls.clone(),
            session_s: dur,
        };
        records::to_group(
            lua,
            s.group_id,
            &format!(
                "AAR {tname}: grade {grade} - {} contact(s), {} disconnect(s), {:.0} lb in {:.0} s\n{}",
                s.contacts,
                s.disconnects,
                fuel_lbs,
                s.time_connected,
                calls.join("; ")
            ),
            msg_s,
        );
        rec.emit(
            lua,
            s.pilot.clone(),
            &s.typ,
            s.side,
            &s.callsign,
            Some(score),
            RangeResult::Aar(res),
            Some(Track::Aar { samples: s.samples }),
        );
    }

    pub fn live(&self, lua: MizLua) -> Vec<LiveTanker> {
        self.tankers
            .iter()
            .filter_map(|t| {
                let u = Unit::get_by_name(lua, &t.unit_name()).ok();
                let (p, v) = u
                    .and_then(|u| Some((u.get_point().ok()?.0, u.get_velocity().ok()?.0)))
                    .unwrap_or((V3::zeros(), V3::zeros()));
                let receivers = self
                    .sessions
                    .values()
                    .filter(|s| s.tanker == t.cfg.id)
                    .map(|s| s.pilot.name.clone())
                    .collect();
                Some(LiveTanker {
                    id: t.cfg.id.clone(),
                    callsign: format!("{}{}", t.cfg.callsign, t.cfg.callsign_number),
                    unit_type: t.cfg.typ.clone(),
                    method: t.method,
                    pos: util::geo(lua, p),
                    heading_deg: util::hdg(v),
                    speed_kts: v.norm() * util::MS_TO_KTS,
                    alt_ft: p.y * util::M_TO_FT,
                    tacan: t.cfg.tacan.as_ref().map(|t| t.describe()),
                    freq_mhz: t.cfg.freq_mhz,
                    state: t.state,
                    receivers,
                    owner: t.owner.as_ref().map(|o| o.1.clone()),
                    recovery_for: t.recovery_for.clone(),
                })
            })
            .collect()
    }

    pub fn describe(&self) -> Vec<String> {
        self.tankers
            .iter()
            .map(|t| {
                format!(
                    "{}{} {} ({}) {:.0} ft, {:.3} MHz{}{}",
                    t.cfg.callsign,
                    t.cfg.callsign_number,
                    t.cfg.typ,
                    match t.method {
                        RefuelMethod::Boom => "boom",
                        RefuelMethod::Drogue => "basket",
                    },
                    t.cfg.alt_ft,
                    t.cfg.freq_mhz,
                    t.cfg.tacan.as_ref().map(|t| format!(", TACAN {}", t.describe())).unwrap_or_default(),
                    match t.state {
                        TankerState::OnStation => "",
                        TankerState::Spawning => " (launching)",
                        TankerState::Rtb => " (RTB)",
                        TankerState::Dead => " (lost, respawning)",
                    }
                )
            })
            .collect()
    }
}
