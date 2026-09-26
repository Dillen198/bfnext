// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Carrier operations: steering the boat into the wind, lighting its navaids,
//! a recovery tanker, and grading every pass.
//!
//! Two graders run side by side:
//!
//! 1. DCS's own Supercarrier LSO (LANDING_QUALITY_MARK). When it grades a
//!    pass, that grade is the official one on the card.
//! 2. Our groove tracker (MOOSE AIRBOSS's geometry): 10 Hz samples from the
//!    start of the groove to touchdown, in the landing-area frame --
//!    glideslope error against the optical glideslope, lineup error off the
//!    angled-deck centreline, AoA from velocity and attitude against the
//!    type's on-speed band, groove time. It grades every pass DCS doesn't
//!    (and draws the trap sheet for all of them).
//!
//! Positions of multiplayer clients reach the server at network update
//! rate, so these numbers are good for teaching, not for a court of law:
//! expect the estimated wire to be +-1 and a little noise on the deviations.

use crate::{
    aar::{self, Aar, Tanker},
    players::{Flying, Players},
    records::{self, Recorder},
    spawn::{self, Spawns},
    util::{self, Frame, MissionClock, V3},
};
use bfprotocols::range::{
    cfg::{AoaBand, CarrierCfg, DeckGeometry, RangeCfg},
    grading,
    lso::{self, Magnitude},
    EngineGrade, GradeSource, GrooveSample, LiveCarrier, PassOutcome, PatternSummary, PilotRef,
    RangeResult, RefuelMethod, TankerState, Track, TrapResult,
};
use dcso3::{unit::Unit, MizLua};
use fxhash::FxHashMap;
use log::{info, warn};
use serde_json::json;

/// Groove windows, metres aft of the landing point: X (start), IM, IC, AR.
const X_START: f64 = 0.75 * 1852.;
const IM_START: f64 = 0.5 * 1852.;
const IC_START: f64 = 0.25 * 1852.;
const AR_START: f64 = 75.;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PassState {
    Pattern,
    Groove,
    Touched,
    Done,
}

#[derive(Debug)]
struct Pass {
    pilot: PilotRef,
    typ: String,
    side: dcso3::coalition::Side,
    group_id: dcso3::env::miz::GroupId,
    callsign: String,
    t0: f64,
    state: PassState,
    samples: Vec<GrooveSample>,
    last_sample: f64,
    groove_start: Option<f64>,
    touch_t: Option<f64>,
    end_t: Option<f64>,
    outcome: Option<PassOutcome>,
    stop_x: Option<f64>,
    pattern: PatternSummary,
    last_y: Option<f64>,
    hook: Option<bool>,
    dcs: Option<lso::LsoComment>,
    wod: f64,
    fb: f64,
    left_area: f64,
}

#[derive(Debug)]
pub struct Carrier {
    pub cfg: CarrierCfg,
    pub deck: DeckGeometry,
    pub group: Option<String>,
    pub op_center: V3,
    brc: f64,
    speed_kts: f64,
    last_route: f64,
    last_navaids: f64,
    recovery_open: bool,
    repositioning: bool,
    passes: FxHashMap<String, Pass>,
    tanker_spawned: bool,
    guard_spawned: bool,
}

/// The landing area frame at an instant.
#[derive(Debug, Clone, Copy)]
struct Deck {
    /// landing point (between the 2- and 3-wire), deck height
    l: V3,
    /// unit vector along the landing area, pointing forward
    u: V3,
    /// unit vector to the right of the landing area
    r: V3,
    ship_vel: V3,
    fb: f64,
    brc: f64,
}

impl Carrier {
    fn deck_now(&self, lua: MizLua) -> Option<Deck> {
        let u = Unit::get_by_name(lua, &self.cfg.unit_name).ok()?;
        let pos = u.get_position().ok()?;
        let vel = u.get_velocity().ok()?.0;
        let f = Frame::from_pos(&pos);
        let brc = util::hdg(f.x);
        let fb = (brc + self.deck.angle_deg).rem_euclid(360.);
        let ua = util::dir(fb);
        let ra = util::dir(fb + 90.);
        let mut stern = f.p + util::dir(brc) * self.deck.stern_m + util::dir(brc + 90.) * self.deck.stern_offset_m;
        stern.y = f.p.y + self.deck.deck_height_m;
        let wire = if self.deck.wires_m[2] > 0. { self.deck.wires_m[2] } else { 60. };
        let l = stern + ua * wire;
        Some(Deck { l, u: ua, r: ra, ship_vel: vel, fb, brc })
    }

    fn wire_estimate(&self, stop_fwd_of_stern: f64, typ: &str) -> Option<u8> {
        if self.deck.wires_m[0] <= 0. {
            return None;
        }
        // an arrested aircraft rolls out well past its wire: MOOSE's
        // correction is 100 m for the Hornet and Tomcat, 56 m otherwise
        let corr = if typ.starts_with("FA-18") || typ.starts_with("F-14") { 100. } else { 56. };
        let d = stop_fwd_of_stern - corr;
        let w = &self.deck.wires_m;
        let mut best = (1u8, f64::MAX);
        for (i, x) in w.iter().enumerate() {
            let e = (d - x).abs();
            if e < best.1 {
                best = ((i + 1) as u8, e);
            }
        }
        Some(best.0)
    }
}

#[derive(Debug, Default)]
pub struct Carriers {
    pub carriers: Vec<Carrier>,
}

fn carrier_group(lua: MizLua, unit: &str) -> Option<(String, i64)> {
    let u = Unit::get_by_name(lua, unit).ok()?;
    let g = u.get_group().ok()?;
    Some((g.get_name().ok()?.to_string(), g.id().ok()?.inner()))
}

impl Carriers {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg) {
        for c in &cfg.carriers {
            let op_center = match crate::ag::resolve(lua, &c.op_center) {
                Ok(p) => p,
                Err(e) => {
                    warn!("carrier {}: op_center {e:?}; using its current position", c.id);
                    Unit::get_by_name(lua, &c.unit_name).and_then(|u| u.get_point()).map(|p| p.0).unwrap_or_default()
                }
            };
            let group = carrier_group(lua, &c.unit_name).map(|g| g.0);
            if group.is_none() {
                warn!("carrier {}: unit {:?} is not in the mission", c.id, c.unit_name)
            }
            info!("carrier {} ({}) {:?}", c.id, c.name, group);
            self.carriers.push(Carrier {
                deck: c.deck.unwrap_or_else(|| c.kind.deck()),
                cfg: c.clone(),
                group,
                op_center,
                brc: 0.,
                speed_kts: 0.,
                last_route: f64::MIN,
                last_navaids: f64::MIN,
                recovery_open: true,
                repositioning: false,
                passes: FxHashMap::default(),
                tanker_spawned: false,
                guard_spawned: false,
            });
        }
    }

    fn window(c: &CarrierCfg, mission_min: f64) -> (bool, u8, Option<String>) {
        if c.recovery_windows.is_empty() {
            return (true, 1, None);
        }
        let mut next: Option<u32> = None;
        for w in &c.recovery_windows {
            if mission_min >= w.start_min as f64 && mission_min < w.end_min as f64 {
                return (true, w.case, None);
            }
            if (w.start_min as f64) > mission_min {
                next = Some(next.map(|n| n.min(w.start_min)).unwrap_or(w.start_min));
            }
        }
        (false, 1, next.map(|n| format!("opens in {:.0} min", n as f64 - mission_min)))
    }

    /// Steer, light navaids, launch the recovery tanker. Every 30 s is plenty.
    pub fn slow_tick(&mut self, lua: MizLua, spawns: &mut Spawns, aar: &mut Aar, now: f64) {
        for c in self.carriers.iter_mut() {
            let Some(group) = c.group.clone() else { continue };
            let Ok(u) = Unit::get_by_name(lua, &c.cfg.unit_name) else { continue };
            let Ok(p) = u.get_point() else { continue };
            let p = p.0;
            let (open, _, _) = Self::window(&c.cfg, now / 60.);
            // Recovery course from DCS's wind at the deck right now: the
            // ship's own motion plus the true wind must come straight down
            // the angled deck (grading::recovery_course).
            let deck = c.deck_now(lua);
            let anemometer = deck.as_ref().map(|d| d.l).unwrap_or(V3::new(p.x, p.y + 20., p.z));
            let (wind_from, wind_kts, _) = util::wind_at(lua, anemometer);
            let rc = grading::recovery_course(wind_from, wind_kts, c.deck.angle_deg, c.cfg.wind_over_deck_kts, 5., 30.);
            let (brc_want, speed_want) = (rc.brc_deg, rc.speed_kts);
            // the ship's real heading, for anything placed relative to it
            let heading_now = deck.as_ref().map(|d| d.brc).unwrap_or(c.brc);
            let from_center = util::dist2(p, c.op_center);
            let (brc, speed, repositioning) = if from_center > c.cfg.op_radius_nm * util::NM {
                (util::bearing(p, c.op_center), 20., true)
            } else if open {
                (brc_want, speed_want, false)
            } else {
                // between windows: loiter back toward the centre slowly
                (util::bearing(p, c.op_center), 10., false)
            };
            c.recovery_open = open && !repositioning;
            c.repositioning = repositioning;
            let changed = util::angdiff(brc, c.brc).abs() > 5. || (speed - c.speed_kts).abs() > 2.;
            if changed || now - c.last_route > 600. {
                let ahead = util::offset(p, brc, 40. * util::NM, 0.);
                let sp = speed / util::MS_TO_KTS;
                let task = json!({ "id": "Mission", "params": { "route": { "points": [
                    spawn::waypoint(p, 0., sp, vec![]),
                    spawn::waypoint(ahead, 0., sp, vec![]),
                ] } } });
                spawns.defer(now, spawn::Pending::GroupTask { group: group.clone(), task });
                c.brc = brc;
                c.speed_kts = speed;
                c.last_route = now;
                c.last_navaids = f64::MIN; // route changes can drop beacons: relight
                info!(
                    "carrier {} steering {:03.0} at {:.0} kt (DCS wind {:03.0}/{:.0} kt at the deck; {:.0} kt down the angle{})",
                    c.cfg.id,
                    brc,
                    speed,
                    wind_from,
                    wind_kts,
                    rc.wod_kts,
                    if rc.aligned { "" } else { ", too little wind to line it up" }
                );
            }
            if now - c.last_navaids > 600. {
                c.last_navaids = now;
                if let Some(uid) = u.id().ok().map(|i| i.inner()) {
                    if let Some(t) = &c.cfg.tacan {
                        spawns.defer(now + 1., spawn::Pending::UnitCommand { unit: c.cfg.unit_name.clone(), cmd: spawn::tacan_cmd(uid, t, false) });
                    }
                    if let Some(ch) = c.cfg.icls_channel {
                        spawns.defer(now + 1., spawn::Pending::UnitCommand {
                            unit: c.cfg.unit_name.clone(),
                            cmd: json!({ "id": "ActivateICLS", "params": { "type": 131584, "channel": ch, "unitId": uid, "name": c.cfg.tacan.as_ref().map(|t| t.morse.clone()).unwrap_or_default() } }),
                        });
                    }
                    if c.cfg.acls {
                        spawns.defer(now + 1., spawn::Pending::UnitCommand {
                            unit: c.cfg.unit_name.clone(),
                            cmd: json!({ "id": "ActivateACLS", "params": { "unitId": uid, "name": c.cfg.name } }),
                        });
                    }
                    if let Some(mhz) = c.cfg.link4_mhz {
                        spawns.defer(now + 1., spawn::Pending::UnitCommand {
                            unit: c.cfg.unit_name.clone(),
                            cmd: json!({ "id": "ActivateLink4", "params": { "unitId": uid, "frequency": mhz * 1e6, "name": c.cfg.name } }),
                        });
                    }
                }
            }
            if !c.tanker_spawned {
                c.tanker_spawned = true;
                if let Some(t) = &c.cfg.recovery_tanker {
                    let gid = carrier_group(lua, &c.cfg.unit_name).map(|g| g.1);
                    let start = util::offset(V3::new(p.x, t.alt_ft / util::M_TO_FT, p.z), heading_now, -3. * util::NM, 0.);
                    match aar::spawn_tanker(lua, t, start, spawns, gid, now) {
                        Ok(g) => {
                            let method = bfprotocols::range::cfg::tanker_type(&t.typ).map(|tt| tt.method).unwrap_or(RefuelMethod::Drogue);
                            aar.add_recovery_tanker(Tanker {
                                cfg: t.clone(),
                                method,
                                group: g,
                                state: TankerState::Spawning,
                                respawn_at: None,
                                owner: None,
                                recovery_for: Some(c.cfg.id.clone()),
                                spawn_id: None,
                            });
                            info!("carrier {}: recovery tanker {} ({}) up", c.cfg.id, t.callsign, t.typ);
                        }
                        Err(e) => warn!("carrier {}: recovery tanker failed: {e:?}", c.cfg.id),
                    }
                }
            }
            if !c.guard_spawned {
                c.guard_spawned = true;
                if let (Some(typ), Some((_, gid))) = (&c.cfg.plane_guard, carrier_group(lua, &c.cfg.unit_name)) {
                    let name = format!("RNG-GUARD-{}", c.cfg.id);
                    let start = util::offset(V3::new(p.x, 70., p.z), heading_now, 0., 1000.);
                    let follow = spawn::task_entry(1, json!({ "id": "Follow", "params": { "groupId": gid, "pos": { "x": -200, "y": 70, "z": 1000 }, "lastWptIndexFlag": false } }));
                    let spec = spawn::AirSpec {
                        name: name.clone(),
                        typ: typ.clone(),
                        count: 1,
                        skill: "Excellent".into(),
                        pos: start,
                        alt_m: 70.,
                        speed_ms: 30.,
                        hdg: heading_now,
                        pylons: Default::default(),
                        livery: None,
                        callsign: None,
                        freq_mhz: None,
                        task: "Transport".into(),
                        route: vec![spawn::waypoint(start, 70., 30., vec![follow])],
                        fuel_kg: None,
                        side: dcso3::coalition::Side::Blue,
                    };
                    if let Ok(country) = spawn::country_id("CJTF_BLUE") {
                        if let Err(e) = spawn::add_group(lua, country, spawn::HELICOPTER, &spawn::air_group(&spec)) {
                            warn!("carrier {}: plane guard failed: {e:?}", c.cfg.id)
                        } else {
                            spawns.defer(now + 2., spawn::Pending::GroupCommand { group: name.clone(), cmd: spawn::bool_cmd("SetImmortal", true) });
                            spawns.defer(now + 2., spawn::Pending::GroupCommand { group: name, cmd: spawn::bool_cmd("SetUnlimitedFuel", true) });
                        }
                    }
                }
            }
        }
    }

    /// 10 Hz while anyone is near a carrier: open passes, sample, detect
    /// waveoffs.
    #[allow(clippy::too_many_arguments)]
    pub fn tick(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        players: &mut Players,
        clock: &MissionClock,
        rec: &mut Recorder,
        now: f64,
    ) -> bool {
        let mut any_near = false;
        for c in self.carriers.iter_mut() {
            let Some(deck) = c.deck_now(lua) else { continue };
            for f in players.flying.values_mut() {
                if f.is_helo || f.is_ground || !f.in_air {
                    continue;
                }
                let d = util::dist2(f.pos, deck.l);
                if d > 5. * util::NM || f.pos.y - deck.l.y > 2500. / util::M_TO_FT {
                    continue;
                }
                any_near = true;
                if !c.passes.contains_key(&f.unit_name) && d < 4. * util::NM {
                    f.activity = Some(format!("{} pattern", c.cfg.name));
                    c.passes.insert(f.unit_name.clone(), new_pass(f, now));
                }
            }
            let band_tbl = &cfg.scoring.aoa;
            let unit_names: Vec<String> = c.passes.keys().cloned().collect();
            for un in unit_names {
                let Some(p) = c.passes.get_mut(&un) else { continue };
                if p.state == PassState::Done {
                    continue;
                }
                let Ok(u) = Unit::get_by_name(lua, &un) else { continue };
                let (Ok(pos3), Ok(v)) = (u.get_position(), u.get_velocity()) else { continue };
                let pos = pos3.p.0;
                let v = v.0;
                let d = pos - deck.l;
                let x = -d.dot(&deck.u);
                let y = d.dot(&deck.r);
                let h = pos.y - deck.l.y;
                let track = util::hdg(v - deck.ship_vel);
                // pattern landmarks
                if p.state == PassState::Pattern {
                    let rel = util::angdiff(util::bearing(deck.l, pos), deck.brc);
                    let opposite = util::angdiff(track, deck.brc + 180.).abs() < 25.;
                    if opposite && (rel + 90.).abs() < 15. && p.pattern.abeam_distance_nm.is_none() {
                        p.pattern.abeam_distance_nm = Some(y.abs() / util::NM);
                        p.pattern.abeam_alt_ft = Some(h * util::M_TO_FT);
                    }
                    if util::angdiff(track, deck.brc - 90.).abs() < 20. && x > 0. && p.pattern.ninety_alt_ft.is_none() && p.pattern.abeam_distance_nm.is_some() {
                        p.pattern.ninety_alt_ft = Some(h * util::M_TO_FT);
                    }
                    if let Some(ly) = p.last_y {
                        if ly.signum() != y.signum() && x > 0. && x < 1.5 * util::NM && p.pattern.wake_alt_ft.is_none() {
                            p.pattern.wake_alt_ft = Some(h * util::M_TO_FT);
                        }
                    }
                    let aligned = util::angdiff(track, deck.fb).abs() < 20.;
                    let lue = y.atan2(x.max(1.)).to_degrees();
                    if aligned && x < X_START && x > 0. && lue.abs() < 12. {
                        p.state = PassState::Groove;
                        p.groove_start = Some(now);
                        p.fb = deck.fb;
                        f_activity(players, &un, &format!("{} groove", c.cfg.name));
                        if p.pattern.wake_alt_ft.is_none() {
                            p.pattern.wake_alt_ft = Some(h * util::M_TO_FT);
                        }
                        let band = band_tbl.get(&p.typ);
                        if let Some(arg) = band.and_then(|b| b.hook_arg) {
                            p.hook = u.get_draw_argument_value(arg).ok().map(|a| a > 0.5);
                        }
                        let (_, wkts, wv) = util::wind_at(lua, deck.l);
                        let rel_wind = wv - deck.ship_vel;
                        p.wod = (rel_wind.dot(&(-deck.u))).max(0.) * util::MS_TO_KTS;
                        let _ = wkts;
                    }
                }
                p.last_y = Some(y);
                // samples at 10 Hz, out to 1.5 nm
                if now - p.last_sample >= 0.1 && x < 1.5 * util::NM && x > -400. {
                    p.last_sample = now;
                    let xa = x.max(1.);
                    let gse = h.atan2(xa).to_degrees() - c.deck.glideslope_deg;
                    let lue = y.atan2(xa).to_degrees();
                    let (_, _, wv) = util::wind_at(lua, pos);
                    let aoa = band_tbl
                        .get(&p.typ)
                        .map(|_| Frame::from_pos(&pos3).aoa(v - wv));
                    let g = util::geo(lua, pos);
                    p.samples.push(GrooveSample {
                        t: now - p.t0,
                        x_m: x,
                        y_m: y,
                        alt_ft: h * util::M_TO_FT,
                        gse_deg: gse,
                        lue_deg: lue,
                        aoa_deg: aoa,
                        closure_kts: (v - deck.ship_vel).dot(&deck.u) * util::MS_TO_KTS,
                        vs_fpm: v.y * 60. * util::M_TO_FT,
                        lat: g.lat,
                        lon: g.lon,
                    });
                }
                // waveoff: past the ramp without touching down, or climbing
                // away in close
                if p.state == PassState::Groove && p.touch_t.is_none() {
                    if x < -30. && h > 15. {
                        p.state = PassState::Done;
                        p.outcome = Some(PassOutcome::Waveoff);
                        p.end_t = Some(now);
                    } else if x > X_START * 1.6 {
                        p.state = PassState::Done;
                        p.outcome = Some(PassOutcome::OwnWaveoff);
                        p.end_t = Some(now);
                    }
                }
                if p.state == PassState::Touched {
                    let rel_speed = (v - deck.ship_vel).norm();
                    if rel_speed < 1.5 {
                        p.state = PassState::Done;
                        p.outcome = Some(PassOutcome::Trap);
                        p.stop_x = Some(x);
                        p.end_t = Some(now);
                    } else if now - p.touch_t.unwrap_or(now) > 12. {
                        p.state = PassState::Done;
                        p.outcome = Some(if u.in_air().unwrap_or(false) { PassOutcome::Bolter } else { PassOutcome::TouchAndGo });
                        p.end_t = Some(now);
                    }
                }
                let far = util::dist2(pos, deck.l) > 6. * util::NM;
                if far {
                    p.left_area += 0.1;
                    if p.left_area > 5. && p.state == PassState::Pattern {
                        // never made it to the groove: not a pass
                        p.state = PassState::Done;
                        p.outcome = None;
                        p.end_t = Some(now - 30.);
                    }
                }
            }
            // finalise passes whose grade window (8 s for the DCS LSO) is over
            let done: Vec<String> = c
                .passes
                .iter()
                .filter(|(_, p)| p.state == PassState::Done && p.end_t.map(|e| now - e > 8.).unwrap_or(false))
                .map(|(k, _)| k.clone())
                .collect();
            for k in done {
                if let Some(p) = c.passes.remove(&k) {
                    f_activity(players, &k, "");
                    if let Some(o) = p.outcome {
                        finish_pass(lua, cfg, c, clock, rec, p, o, now);
                    }
                }
            }
        }
        any_near
    }

    /// RUNWAY_TOUCH / LAND on a carrier deck.
    pub fn touch(&mut self, lua: MizLua, unit: &str, place: &str, landed: bool, now: f64) {
        for c in self.carriers.iter_mut() {
            if c.cfg.unit_name != place {
                continue;
            }
            let deck = c.deck_now(lua);
            if let Some(p) = c.passes.get_mut(unit) {
                if p.state == PassState::Groove || p.state == PassState::Pattern {
                    p.state = PassState::Touched;
                    p.touch_t = Some(now);
                }
                if landed && p.state != PassState::Done {
                    p.state = PassState::Done;
                    p.outcome = Some(PassOutcome::Trap);
                    p.end_t = Some(now);
                    if let (Some(d), Ok(u)) = (deck, Unit::get_by_name(lua, unit)) {
                        if let Ok(pt) = u.get_point() {
                            p.stop_x = Some(-(pt.0 - d.l).dot(&d.u));
                        }
                    }
                }
            }
        }
    }

    /// RUNWAY_TAKEOFF from a carrier after a touch: bolter.
    pub fn runway_takeoff(&mut self, unit: &str, place: &str, now: f64) {
        for c in self.carriers.iter_mut() {
            if c.cfg.unit_name != place {
                continue;
            }
            if let Some(p) = c.passes.get_mut(unit) {
                if p.state == PassState::Touched {
                    p.state = PassState::Done;
                    p.outcome = Some(PassOutcome::Bolter);
                    p.end_t = Some(now);
                }
            }
        }
    }

    /// DCS's LSO graded a pass.
    pub fn landing_quality_mark(&mut self, unit: &str, comment: &str, now: f64) {
        let Some(parsed) = lso::parse_comment(comment) else {
            warn!("unparsed LSO comment {comment:?}");
            return;
        };
        info!("DCS LSO for {unit}: {comment}");
        for c in self.carriers.iter_mut() {
            if let Some(p) = c.passes.get_mut(unit) {
                p.dcs = Some(parsed.clone());
                // the LSO's verdict ends the pass if our tracker hadn't yet
                if p.state != PassState::Done {
                    let o = match parsed.grade.as_str() {
                        "B" => PassOutcome::Bolter,
                        "WO" | "WOP" | "WOFD" => PassOutcome::Waveoff,
                        "OWO" => PassOutcome::OwnWaveoff,
                        _ => PassOutcome::Trap,
                    };
                    p.state = PassState::Done;
                    p.outcome = Some(o);
                    p.end_t = Some(now);
                }
                return;
            }
        }
    }

    pub fn player_left(&mut self, unit: &str, now: f64) {
        for c in self.carriers.iter_mut() {
            if let Some(p) = c.passes.get_mut(unit) {
                if p.state == PassState::Groove || p.state == PassState::Touched {
                    p.state = PassState::Done;
                    p.outcome = Some(PassOutcome::Crash);
                    p.end_t = Some(now - 8.);
                } else if p.state == PassState::Pattern {
                    p.state = PassState::Done;
                    p.outcome = None;
                    p.end_t = Some(now - 8.);
                }
            }
        }
    }

    pub fn live(&self, lua: MizLua, clock: &MissionClock, abs: f64, aar: &Aar, now: f64) -> Vec<LiveCarrier> {
        self.carriers
            .iter()
            .filter_map(|c| {
                let d = c.deck_now(lua)?;
                let (true_from, true_kts, wv) = util::wind_at(lua, d.l);
                let rel = wv - d.ship_vel;
                let wod = rel.norm() * util::MS_TO_KTS;
                let wod_angle = util::angdiff(util::hdg(-rel), d.fb);
                let (open, case, next) = Self::window(&c.cfg, now / 60.);
                let g = util::geo(lua, d.l);
                let night = clock.is_night(g.lat, g.lon, abs);
                Some(LiveCarrier {
                    id: c.cfg.id.clone(),
                    name: c.cfg.name.clone(),
                    unit_type: format!("{:?}", c.cfg.kind),
                    pos: g,
                    brc_deg: d.brc,
                    fb_deg: d.fb,
                    speed_kts: d.ship_vel.norm() * util::MS_TO_KTS,
                    wind_over_deck_kts: wod,
                    wind_over_deck_angle_deg: wod_angle,
                    recovery_open: open && !c.repositioning,
                    next_window: if c.repositioning { Some("repositioning".into()) } else { next },
                    tacan: c.cfg.tacan.as_ref().map(|t| t.describe()),
                    icls: c.cfg.icls_channel,
                    link4_mhz: c.cfg.link4_mhz,
                    tower_mhz: c.cfg.tower_mhz,
                    recovery_tanker: aar
                        .tankers
                        .iter()
                        .find(|t| t.recovery_for.as_deref() == Some(c.cfg.id.as_str()))
                        .map(|t| format!("{}{} ({})", t.cfg.callsign, t.cfg.callsign_number, t.cfg.typ)),
                    pattern: c.passes.values().map(|p| p.pilot.name.clone()).collect(),
                    case: if night { 3 } else { case },
                    true_wind_from_deg: true_from,
                    true_wind_kts: true_kts,
                    deck_angle_deg: util::angdiff(d.fb, d.brc),
                })
            })
            .collect()
    }

    pub fn describe(&self, lua: MizLua) -> Vec<String> {
        self.carriers
            .iter()
            .filter_map(|c| {
                let d = c.deck_now(lua)?;
                let (_, _, wv) = util::wind_at(lua, d.l);
                let wod = (wv - d.ship_vel).norm() * util::MS_TO_KTS;
                Some(format!(
                    "{}: BRC {:03.0} FB {:03.0}, {:.0} kt WOD, {}{}{}",
                    c.cfg.name,
                    d.brc,
                    d.fb,
                    wod,
                    if c.recovery_open { "deck OPEN" } else { "deck closed" },
                    c.cfg.tacan.as_ref().map(|t| format!(", TACAN {}", t.describe())).unwrap_or_default(),
                    c.cfg.icls_channel.map(|i| format!(", ICLS {i}")).unwrap_or_default(),
                ))
            })
            .collect()
    }
}

fn f_activity(players: &mut Players, unit: &str, act: &str) {
    if let Some(f) = players.flying.get_mut(unit) {
        f.activity = if act.is_empty() { None } else { Some(act.to_string()) };
    }
}

fn new_pass(f: &Flying, now: f64) -> Pass {
    Pass {
        pilot: records::pilot_of(f),
        typ: f.typ.clone(),
        side: f.side,
        group_id: f.group_id,
        callsign: f.group_name.clone(),
        t0: now,
        state: PassState::Pattern,
        samples: vec![],
        last_sample: f64::MIN,
        groove_start: None,
        touch_t: None,
        end_t: None,
        outcome: None,
        stop_x: None,
        pattern: PatternSummary::default(),
        last_y: None,
        hook: None,
        dcs: None,
        wod: 0.,
        fb: 0.,
        left_area: 0.,
    }
}

/// Average of `f` over the groove samples in `[lo, hi)` metres aft.
fn window_avg(s: &[GrooveSample], lo: f64, hi: f64, f: impl Fn(&GrooveSample) -> Option<f64>) -> Option<f64> {
    let v: Vec<f64> = s.iter().filter(|x| x.x_m >= lo && x.x_m < hi).filter_map(&f).collect();
    if v.is_empty() {
        None
    } else {
        Some(v.iter().sum::<f64>() / v.len() as f64)
    }
}

fn token(err: &str, pos: &str, m: Magnitude) -> String {
    match m {
        Magnitude::Little => format!("({err}{pos})"),
        Magnitude::Normal => format!("{err}{pos}"),
        Magnitude::Lot => format!("_{err}{pos}_"),
    }
}

/// The engine's own grade from the groove samples.
fn engine_grade(
    s: &[GrooveSample],
    band: Option<&AoaBand>,
    outcome: PassOutcome,
    groove_s: Option<f64>,
    wire: Option<u8>,
) -> EngineGrade {
    let mut calls: Vec<String> = vec![];
    let mut worst = 0; // 0 none, 1 little, 2 normal, 3 lot
    let mut waveoff_limits = false;
    for (pos, lo, hi) in [("X", IM_START, X_START), ("IM", IC_START, IM_START), ("IC", AR_START, IC_START), ("AR", 0., AR_START)] {
        let gse = window_avg(s, lo, hi, |x| Some(x.gse_deg));
        let lue = window_avg(s, lo, hi, |x| Some(x.lue_deg));
        let aoa = window_avg(s, lo, hi, |x| x.aoa_deg);
        let mut add = |c: Option<(&'static str, Magnitude)>| {
            if let Some((e, m)) = c {
                worst = worst.max(match m {
                    Magnitude::Little => 1,
                    Magnitude::Normal => 2,
                    Magnitude::Lot => 3,
                });
                calls.push(token(e, pos, m));
            }
        };
        if let Some(g) = gse {
            add(grading::glideslope_call(g));
        }
        if let Some(l) = lue {
            add(grading::lineup_call(l));
        }
        if let (Some(a), Some(b)) = (aoa, band) {
            add(grading::aoa_call(b, a));
        }
        if matches!(pos, "IC" | "AR") {
            if let (Some(g), Some(l)) = (gse, lue) {
                waveoff_limits |= grading::waveoff_limits(g, l);
            }
        }
    }
    if let Some(g) = groove_s {
        if g > 20. {
            calls.insert(0, "LIG".into());
        } else if g < 13. {
            calls.insert(0, "NESA".into());
        }
    }
    let grade = match outcome {
        PassOutcome::Bolter => "B".to_string(),
        PassOutcome::Waveoff => {
            if waveoff_limits { "WO".into() } else { "OWO".into() }
        }
        PassOutcome::OwnWaveoff => "OWO".into(),
        PassOutcome::Crash => "C".into(),
        _ => {
            if worst >= 3 {
                "--".into()
            } else if worst == 2 {
                "(OK)".into()
            } else if worst == 0
                && groove_s.map(|g| (15. ..=19.).contains(&g)).unwrap_or(false)
                && wire == Some(3)
            {
                "_OK_".into()
            } else {
                "OK".into()
            }
        }
    };
    EngineGrade { points: lso::grade_points(&grade), grade, comment: calls.join(" ") }
}

#[allow(clippy::too_many_arguments)]
fn finish_pass(
    lua: MizLua,
    cfg: &RangeCfg,
    c: &Carrier,
    clock: &MissionClock,
    rec: &mut Recorder,
    p: Pass,
    outcome: PassOutcome,
    _now: f64,
) {
    let groove_s = match (p.groove_start, p.touch_t.or(p.end_t)) {
        (Some(a), Some(b)) if b > a => Some(b - a),
        _ => None,
    };
    // keep the groove part of the track (plus a little of the approach turn
    // for the top view)
    let samples: Vec<GrooveSample> = p.samples.iter().filter(|s| s.x_m < 1.5 * util::NM).cloned().collect();
    let est_wire = if outcome == PassOutcome::Trap {
        p.stop_x.and_then(|x| {
            let wire3 = if c.deck.wires_m[2] > 0. { c.deck.wires_m[2] } else { 60. };
            // stop_x is aft of the landing point; forward of the stern it is
            // wire3 - stop_x
            c.wire_estimate(wire3 - x, &p.typ)
        })
    } else {
        None
    };
    let band = cfg.scoring.aoa.get(&p.typ);
    let eg = engine_grade(&samples, band, outcome, groove_s, est_wire);
    let (grade, details, wire, wire_from_dcs, source) = match &p.dcs {
        Some(d) => (
            d.grade.clone(),
            d.details.clone(),
            d.wire.or(est_wire),
            d.wire.is_some(),
            GradeSource::Dcs,
        ),
        None => (eg.grade.clone(), eg.comment.clone(), est_wire, false, GradeSource::Engine),
    };
    let points = lso::grade_points(&grade);
    let g0 = samples.last().map(|s| (s.lat, s.lon)).unwrap_or((0., 0.));
    let abs = dcso3::timer::Timer::singleton(lua).and_then(|t| t.get_abs_time()).map(|t| t.0 as f64).unwrap_or(0.);
    let night = clock.is_night(g0.0, g0.1, abs);
    let case = if night { 3 } else { Carriers::window(&c.cfg, _now / 60.).1 };
    let description = lso::describe(&details);
    let res = TrapResult {
        carrier: c.cfg.name.clone(),
        carrier_type: format!("{:?}", c.cfg.kind),
        case,
        night,
        outcome,
        grade: grade.clone(),
        points,
        lso_comment: details.clone(),
        lso_description: description.clone(),
        wire,
        wire_from_dcs,
        groove_time_s: groove_s,
        wind_over_deck_kts: Some(p.wod),
        final_bearing_deg: Some(p.fb),
        source,
        dcs_comment: p.dcs.as_ref().map(|d| d.raw.clone()),
        engine_grade: Some(eg),
        pattern: p.pattern.clone(),
        hook_down: p.hook,
    };
    let mut text = format!(
        "LSO {}: {} => {}{}{}",
        c.cfg.name,
        grade,
        lso::grade_name(&grade),
        wire.map(|w| format!(", #{w} wire")).unwrap_or_default(),
        groove_s.map(|g| format!(", groove {g:.1} s")).unwrap_or_default()
    );
    if !details.is_empty() {
        text.push_str(&format!("\n{details}\n{}", description.join("\n")));
    }
    if p.hook == Some(false) {
        text.push_str("\nHOOK UP in the groove!");
    }
    if cfg.in_game_results {
        records::to_group(lua, p.group_id, &text, cfg.message_s.max(25));
    }
    rec.emit(
        lua,
        p.pilot.clone(),
        &p.typ,
        p.side,
        &p.callsign,
        points,
        RangeResult::Trap(res),
        Some(Track::Groove { samples }),
    );
}

pub fn carrier_by_place(cs: &Carriers, place: &str) -> bool {
    cs.carriers.iter().any(|c| c.cfg.unit_name == place)
}
