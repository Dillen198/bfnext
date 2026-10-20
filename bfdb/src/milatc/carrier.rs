// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! A carrier's CATCC: Marshal, Approach and Paddles.
//!
//! Two frequencies, the way a real recovery runs:
//!
//! - **Marshal** (the carrier's own frequency, the one on the kneeboard)
//!   checks every recovering aircraft in and gives it a slot. Case I: an
//!   altitude in the overhead stack and an expected Charlie time. Case III:
//!   a radial, DME and angels in the Marshal stack and an expected approach
//!   time, one minute and a half apart. When the slot comes up, Marshal
//!   signals Charlie (Case I) or sends the aircraft to Approach (Case III).
//! - **Approach** ("button", Marshal + 0.5 MHz) talks a Case III aircraft
//!   down the final bearing (ten miles, six, three) to "call the ball".
//!   Paddles, the LSO, works the same frequency in his own voice: "roger
//!   ball", then "a little high", "power", "right for lineup", and "wave off"
//!   when it is not going to work, or "bolter" when the hook skips.
//!
//! AI flights recovering to the carrier check in, hold for their slot and
//! call the ball like anyone else.

use super::{
    controller::{spoken_type, type_label, Caller, Tx},
    facility::{angdiff, Air, Calls, Clock, Facility, Local, Mouth, Picture, NM},
    intent::{Intent, Parsed},
    llm, speech, Pos,
};
use crate::voice::{stt, FreqSpec};
use bfprotocols::range::LiveCarrier;
use serde::Deserialize;
use std::collections::HashMap;

/// Frequency overrides for one carrier, by the engine's carrier id.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CarrierFreqCfg {
    #[serde(default)]
    pub marshal: Vec<FreqSpec>,
    #[serde(default)]
    pub approach: Vec<FreqSpec>,
}

/// Time between one aircraft's slot and the next, seconds.
const INTERVAL: f64 = 90.;
/// The carrier's glideslope, degrees.
const GLIDESLOPE: f64 = 3.5;
/// The landing area is aft of the ship's reference point, metres.
const TOUCHDOWN_AFT: f64 = 80.;
/// Deck height, feet.
const DECK_FT: f64 = 65.;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stage {
    /// Checked in; holding for the slot.
    Stack,
    /// Case I: Charlie signalled, in the break and the pattern.
    Charlie,
    /// Case III: pushed, on the approach frequency.
    Approach,
    /// In the groove, ball called.
    Ball,
    /// Trapped.
    Down,
}

#[derive(Debug, Clone)]
struct Recovery {
    unit: u64,
    call: String,
    stage: Stage,
    angels: i32,
    slot: f64,
    since: f64,
    /// Range calls already made on the approach (10, 6, 3 nm).
    called: Vec<u32>,
    last_lso: f64,
    airborne: bool,
}

pub(crate) struct Carrier {
    pub id: String,
    live: LiveCarrier,
    ship: String,
    marshal: Vec<FreqSpec>,
    approach: Vec<FreqSpec>,
    line: Vec<Recovery>,
    air: Vec<Air>,
    calls: Calls,
    clock: Clock,
    mouth: Mouth,
    now: f64,
    /// AI recoveries already checked in.
    ai_seen: Vec<u64>,
}

/// "CVN-72 Abraham Lincoln" → "Lincoln".
fn ship_name(name: &str) -> String {
    name.split_whitespace().last().unwrap_or(name).to_string()
}

impl Carrier {
    pub(crate) fn new(c: &LiveCarrier, over: Option<&CarrierFreqCfg>) -> Self {
        let base = c.tower_mhz.unwrap_or(309.0);
        let f = |mhz: f64| vec![FreqSpec { mhz, modulation: None, label: None }];
        let marshal = over.map(|o| o.marshal.clone()).filter(|v| !v.is_empty()).unwrap_or_else(|| f(base));
        let approach = over.map(|o| o.approach.clone()).filter(|v| !v.is_empty()).unwrap_or_else(|| f(base + 0.5));
        Carrier {
            id: c.id.clone(),
            live: c.clone(),
            ship: ship_name(&c.name),
            marshal,
            approach,
            line: vec![],
            air: vec![],
            calls: Calls::default(),
            clock: Clock::default(),
            mouth: Mouth::seeded(c.id.len() as u64 * 7919),
            now: 0.,
            ai_seen: vec![],
        }
    }

    fn case(&self) -> u8 {
        self.live.case.clamp(1, 3)
    }

    fn approach_freq(&self) -> String {
        self.approach.first().map(|f| speech::freq(f.mhz)).unwrap_or_default()
    }

    /// The touchdown point, and (out along the final bearing, right of it) for
    /// an aircraft, metres.
    fn final_frame(&self, a: &Air) -> (f64, f64) {
        let l = Local::at(self.live.pos.lat, self.live.pos.lon);
        let (n, e) = l.xy(a.lat, a.lon);
        let brc = self.live.brc_deg.to_radians();
        // touchdown point, aft of the reference point along the ship's heading
        let (tn, te) = (-TOUCHDOWN_AFT * brc.cos(), -TOUCHDOWN_AFT * brc.sin());
        let (dn, de) = (n - tn, e - te);
        let fb = self.live.fb_deg.to_radians();
        let along = dn * fb.cos() + de * fb.sin();
        let right = -dn * fb.sin() + de * fb.cos();
        (-along, right)
    }

    fn range_nm(&self, a: &Air) -> f64 {
        Local::at(self.live.pos.lat, self.live.pos.lon).polar(a.lat, a.lon).0 / NM
    }

    /// The next free slot time, and the stack altitude that goes with it.
    fn next_slot(&self) -> (f64, i32) {
        let k = self.line.iter().filter(|r| r.stage == Stage::Stack).count() as i32;
        let last = self.line.iter().filter(|r| r.stage == Stage::Stack).map(|r| r.slot).fold(self.now + 120., f64::max);
        let base_angels = if self.case() == 1 { 2 } else { 6 };
        let t = if k == 0 { self.now + 120. } else { last + INTERVAL };
        (t, base_angels + k)
    }

    fn check_in(&mut self, a: &Air, out: &mut Vec<Tx>) {
        let call = self.calls.of(a);
        let me = format!("{} Marshal", self.ship);
        let now = self.now;
        if let Some(r) = self.line.iter().find(|r| r.unit == a.id) {
            let t = format!("{call}, {me}, you're in the stack, angels {}, expected time {}.", r.angels, self.clock.say_minute(r.slot));
            return self.mouth.say(Pos::Marshal, None, a, &call, &me, t, now, out);
        }
        let (slot, angels) = self.next_slot();
        let case = self.case();
        let radial = (self.live.fb_deg + 180.).rem_euclid(360.);
        let dme = angels + 15;
        let brc = speech::hdg(self.live.brc_deg);
        let mut t = match case {
            3 => format!(
                "{call}, {me}, case three recovery, CV-1 approach, expected final bearing {}, marshal on the {} radial, {dme} DME, angels {angels}, expected approach time {}.",
                speech::hdg(self.live.fb_deg),
                speech::hdg(radial),
                self.clock.say_minute(slot)
            ),
            n => format!(
                "{call}, {me}, case {} recovery, BRC {brc}, hold overhead at angels {angels}, expected Charlie time {}.",
                if n == 2 { "two" } else { "one" },
                self.clock.say_minute(slot)
            ),
        };
        if !self.live.recovery_open {
            let when = self.live.next_window.clone().unwrap_or_else(|| "later".into());
            t = format!("{call}, {me}, deck is closed, recovery {when}. Hold at angels {angels} and expect to be called.");
        }
        self.line.push(Recovery {
            unit: a.id,
            call: call.clone(),
            stage: Stage::Stack,
            angels,
            slot,
            since: now,
            called: vec![],
            last_lso: 0.,
            airborne: true,
        });
        self.mouth.say(Pos::Marshal, None, a, &call, &me, t, now, out);
        if a.ai {
            self.mouth.command(a, "hold", out);
        }
    }

    /// Paddles in the groove: one call every couple of seconds.
    fn lso(&mut self, i: usize, a: &Air, out: &mut Vec<Tx>) {
        let now = self.now;
        if now - self.line[i].last_lso < self.mouth.between(1.6, 2.6) {
            return;
        }
        let (out_m, right) = self.final_frame(a);
        if out_m < 60. {
            return; // over the ramp: nothing more to say
        }
        let want_ft = out_m * GLIDESLOPE.to_radians().tan() / 0.3048 + DECK_FT;
        let err = a.alt_ft as f64 - want_ft;
        let tol = 12. + out_m / NM * 60.;
        let call = self.line[i].call.clone();
        let me = "Paddles".to_string();
        // not going to make it: wave off
        let lined_up = right.abs() < 30. + out_m * 0.05;
        if out_m < 0.4 * NM && (err.abs() > tol * 4. || !lined_up && right.abs() > 60.) {
            let t = format!("Wave off, wave off. {call}, {}", self.mouth.pick(&["wave off.", "take it around."]));
            self.mouth.say(Pos::Approach, Some(Pos::Paddles), a, &call, &me, t, now, out);
            let again = if self.case() == 3 { Stage::Approach } else { Stage::Charlie };
            let r = &mut self.line[i];
            r.stage = again;
            r.called.clear();
            r.last_lso = now;
            return;
        }
        let mut words: Vec<&str> = vec![];
        if err > tol * 2. {
            words.push("you're high");
        } else if err > tol {
            words.push("a little high");
        } else if err < -tol * 2. {
            words.push("power");
        } else if err < -tol {
            words.push("a little power");
        }
        if right < -(15. + out_m * 0.02) {
            words.push("right for lineup");
        } else if right > 15. + out_m * 0.02 {
            words.push("come left");
        }
        if words.is_empty() {
            if self.mouth.rand() < 0.35 {
                words.push(self.mouth.pick(&["looking good", "keep it coming", "easy with it"]));
            } else {
                return;
            }
        }
        self.line[i].last_lso = now;
        let mut t = words.join(", ");
        t.push('.');
        let t = super::controller::capitalize(&t);
        self.mouth.say(Pos::Approach, Some(Pos::Paddles), a, &call, &me, t, now, out);
    }
}

impl Facility for Carrier {
    fn name(&self) -> String {
        format!("{} CATCC", self.ship)
    }

    fn radios(&self) -> Vec<(Pos, Vec<FreqSpec>)> {
        vec![(Pos::Marshal, self.marshal.clone()), (Pos::Approach, self.approach.clone())]
    }

    fn facility(&self, pos: Pos) -> String {
        match pos {
            Pos::Paddles => "Paddles".into(),
            p => format!("{} {}", self.ship, p.title()),
        }
    }

    fn label(&self, pos: Pos) -> String {
        match pos {
            Pos::Marshal => format!("{} Marshal", self.ship),
            _ => format!("{} Approach", self.ship),
        }
    }

    fn wake(&self) -> String {
        self.ship.clone()
    }

    fn update(&mut self, pic: &Picture) -> Vec<Tx> {
        self.now = pic.now;
        self.clock.sync(pic);
        let now = self.now;
        let mut out = vec![];
        if let Some(c) = pic.range.and_then(|r| r.carriers.iter().find(|c| c.id == self.id)) {
            self.live = c.clone();
        }
        self.air = super::facility::traffic(pic.atc);
        let alive: HashMap<u64, Air> = self.air.iter().map(|a| (a.id, a.clone())).collect();
        self.line.retain(|r| alive.contains_key(&r.unit) && r.stage != Stage::Down);
        let marshal = format!("{} Marshal", self.ship);
        let approach = format!("{} Approach", self.ship);

        for i in 0..self.line.len() {
            let r = self.line[i].clone();
            let Some(a) = alive.get(&r.unit).cloned() else { continue };
            let nm = self.range_nm(&a);
            // On deck: trapped (or a bolter in the making).
            if a.on_ground && nm < 0.3 {
                self.line[i].airborne = false;
                self.line[i].since = now;
                continue;
            }
            if !r.airborne && !a.on_ground {
                // airborne again off the deck: a bolter
                let t = format!("Bolter, bolter. {}, climb to 1,200, re-enter the pattern.", r.call);
                self.mouth.say(Pos::Approach, Some(Pos::Paddles), &a, &r.call, "Paddles", t, now, &mut out);
                let again = if self.case() == 3 { Stage::Approach } else { Stage::Charlie };
                let x = &mut self.line[i];
                x.airborne = true;
                x.stage = again;
                x.called.clear();
                continue;
            }
            if !r.airborne && a.on_ground && now - r.since > 8. {
                self.line[i].stage = Stage::Down;
                continue;
            }
            match r.stage {
                Stage::Stack if now >= r.slot && self.live.recovery_open => {
                    if self.case() == 3 {
                        let t = format!("{}, {marshal}, push time, commence, switch approach {}.", r.call, self.approach_freq());
                        self.mouth.say(Pos::Marshal, None, &a, &r.call, &marshal, t, now, &mut out);
                        self.line[i].stage = Stage::Approach;
                    } else {
                        let t = format!("{}, {marshal}, Charlie. Paddles on {}.", r.call, self.approach_freq());
                        self.mouth.say(Pos::Marshal, None, &a, &r.call, &marshal, t, now, &mut out);
                        self.line[i].stage = Stage::Charlie;
                    }
                    self.mouth.command(&a, "resume", &mut out);
                }
                Stage::Approach => {
                    let (out_m, right) = self.final_frame(&a);
                    let d = out_m / NM;
                    let on_final = angdiff(a.hdg, self.live.fb_deg).abs() < 40. && right.abs() < NM;
                    for (mark, text) in [(10u32, "ten miles, dirty up"), (6, "six miles"), (3, "three miles")] {
                        if on_final && d < mark as f64 && d > mark as f64 - 1.5 && !self.line[i].called.contains(&mark) {
                            self.line[i].called.push(mark);
                            let course = if right.abs() < 120. {
                                "on course".to_string()
                            } else if right > 0. {
                                format!("right of course, come left heading {}", speech::hdg(self.live.fb_deg - 5.))
                            } else {
                                format!("left of course, come right heading {}", speech::hdg(self.live.fb_deg + 5.))
                            };
                            let t = format!("{}, {approach}, {text}, {course}.", r.call);
                            self.mouth.say(Pos::Approach, None, &a, &r.call, &approach, t, now, &mut out);
                        }
                    }
                    if on_final && d < 1.0 && d > 0.5 && !self.line[i].called.contains(&1) {
                        self.line[i].called.push(1);
                        let t = format!("{}, {approach}, three quarters of a mile, call the ball.", r.call);
                        self.mouth.say(Pos::Approach, None, &a, &r.call, &approach, t, now, &mut out);
                    }
                }
                Stage::Ball => self.lso(i, &a, &mut out),
                _ => {}
            }
            // AI pilots call the ball in the groove
            let r = self.line[i].clone();
            if a.ai && matches!(r.stage, Stage::Charlie | Stage::Approach) {
                let (out_m, right) = self.final_frame(&a);
                if out_m < 0.8 * NM && out_m > 0.3 * NM && right.abs() < 300. {
                    let t = format!("{}, {}ball, {:.1}.", r.call, spoken_type(&a.typ).trim_start_matches(", ").to_string() + " ", 4.0 + self.mouth.rand() * 3.);
                    self.mouth.pilot(Pos::Approach, &a, &r.call, t, now, &mut out);
                    let wod = self.live.wind_over_deck_kts.round() as i32;
                    let t = format!("Roger ball, {wod} knots.");
                    self.mouth.say(Pos::Approach, Some(Pos::Paddles), &a, &r.call, "Paddles", t, now, &mut out);
                    self.line[i].stage = Stage::Ball;
                    self.line[i].last_lso = now;
                }
            }
        }

        // AI recoveries check in with Marshal.
        let mine = format!("carrier:{}", self.id);
        let ai: Vec<Air> = self.air.iter().filter(|a| a.ai && a.ai_plan.as_deref() == Some(mine.as_str()) && !a.on_ground).cloned().collect();
        for a in ai {
            if self.ai_seen.contains(&a.id) || self.range_nm(&a) > 45. {
                continue;
            }
            self.ai_seen.push(a.id);
            let call = self.calls.of(&a);
            let nm = self.range_nm(&a).round() as i32;
            let t = format!(
                "{marshal}, {call}{}, {nm} miles, angels {}, state {:.1}.",
                spoken_type(&a.typ),
                (a.alt_ft + 500) / 1000,
                4.0 + self.mouth.rand() * 4.
            );
            self.mouth.pilot(Pos::Marshal, &a, &call, t, now, &mut out);
            self.check_in(&a, &mut out);
        }
        out
    }

    fn respond(&mut self, pos: Pos, who: &Caller, heard: &str, parsed: Option<Parsed>, now: f64) -> Vec<Tx> {
        self.now = now;
        let mut out = vec![];
        let Some(a) = super::facility::locate(&self.air, who, parsed.as_ref(), &self.calls.0).cloned() else {
            return out;
        };
        self.calls.learn(&a, parsed.as_ref());
        let call = self.calls.of(&a);
        let me = self.facility(pos);
        let Some(p) = parsed else {
            if stt::mentions(heard, &self.ship) || stt::mentions(heard, "marshal") || stt::mentions(heard, "paddles") {
                let t = format!("{call}, {me}, say again.");
                self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
            }
            return out;
        };
        match (pos, p.intent) {
            (_, Intent::RadioCheck) => {
                let t = format!("{call}, {me}, loud and clear.");
                self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
            }
            (Pos::Marshal, Intent::CheckIn | Intent::RequestApproach) => self.check_in(&a, &mut out),
            (_, Intent::Commencing) => {
                if let Some(r) = self.line.iter_mut().find(|r| r.unit == a.id) {
                    r.stage = Stage::Approach;
                }
                // usually still on Marshal when commencing: send him over
                let t = if pos == Pos::Marshal {
                    format!("{call}, {me}, roger, switch approach {}.", self.approach_freq())
                } else {
                    format!("{call}, {me}, radar contact, report platform.")
                };
                self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
            }
            (_, Intent::Ball) => {
                let clara = stt::contains_phrase(heard, "clara");
                if let Some(r) = self.line.iter_mut().find(|r| r.unit == a.id) {
                    r.stage = Stage::Ball;
                    r.last_lso = now;
                } else {
                    // never checked in: work him anyway
                    self.line.push(Recovery {
                        unit: a.id,
                        call: call.clone(),
                        stage: Stage::Ball,
                        angels: 0,
                        slot: now,
                        since: now,
                        called: vec![],
                        last_lso: now,
                        airborne: true,
                    });
                }
                let wod = self.live.wind_over_deck_kts.round() as i32;
                let t = if clara { "Paddles contact, you're on glideslope, keep it coming.".to_string() } else { format!("Roger ball, {wod} knots.") };
                self.mouth.say(Pos::Approach, Some(Pos::Paddles), &a, &call, "Paddles", t, now, &mut out);
            }
            (Pos::Approach, Intent::CheckIn | Intent::RequestApproach) => {
                if let Some(r) = self.line.iter_mut().find(|r| r.unit == a.id) {
                    r.stage = Stage::Approach;
                }
                let nm = self.range_nm(&a).round() as i32;
                let t = format!("{call}, {me}, radar contact, {nm} miles, final bearing {}, report platform.", speech::hdg(self.live.fb_deg));
                self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
            }
            (_, Intent::None | Intent::Readback) => {}
            (Pos::Marshal, _) => {
                let t = format!("{call}, {me}, say request.");
                self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
            }
            _ => {}
        }
        out
    }

    fn caller(&mut self, who: &Caller, parsed: Option<&Parsed>) -> (Option<String>, Option<String>) {
        match super::facility::locate(&self.air, who, parsed, &self.calls.0).cloned() {
            Some(a) => {
                self.calls.learn(&a, parsed);
                (Some(self.calls.of(&a)), type_label(&a.typ))
            }
            None => (None, None),
        }
    }

    fn llm_context(&mut self, pos: Pos, who: &Caller, heard: &str) -> llm::Context {
        let a = super::facility::locate(&self.air, who, None, &self.calls.0);
        let situation = match a.and_then(|a| self.line.iter().find(|r| r.unit == a.id)) {
            Some(r) => format!("in the carrier recovery, {:?}, angels {}", r.stage, r.angels),
            None => "not checked in with the carrier".into(),
        };
        llm::Context { position: self.facility(pos), situation, last_instruction: None, heard: heard.to_string() }
    }

    fn vocabulary(&self) -> Vec<String> {
        vec![
            format!("{} Marshal", self.ship),
            format!("{} Approach", self.ship),
            "Paddles".into(),
            "Hornet ball".into(),
            "Clara".into(),
            "commencing".into(),
            "platform".into(),
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bfprotocols::{
        atc::{AtcPicture, AtcTraffic},
        range::{GeoPt, RangeLive},
    };

    fn ship(case: u8) -> LiveCarrier {
        LiveCarrier {
            id: "cvn72".into(),
            name: "CVN-72 Abraham Lincoln".into(),
            unit_type: "CVN_72".into(),
            pos: GeoPt { lat: 14.0, lon: 144.0, alt_m: 0. },
            brc_deg: 0.,
            fb_deg: 351.,
            speed_kts: 25.,
            wind_over_deck_kts: 27.,
            wind_over_deck_angle_deg: 0.,
            recovery_open: true,
            next_window: None,
            tacan: None,
            icls: None,
            link4_mhz: None,
            tower_mhz: Some(305.0),
            recovery_tanker: None,
            pattern: vec![],
            case,
            true_wind_from_deg: 0.,
            true_wind_kts: 0.,
            deck_angle_deg: -9.,
        }
    }

    fn jet(id: u64, n_m: f64, e_m: f64, alt_ft: i32, hdg: u16) -> AtcTraffic {
        let l = 111_132.;
        let k = 111_320. * 14f64.to_radians().cos();
        AtcTraffic {
            ucid: String::new(),
            unit_id: Some(id),
            callsign: if id == 1 { "Rhino 1-1".into() } else { "Rhino 1-2".into() },
            player_name: format!("p{id}"),
            lat: 14.0 + n_m / l,
            lon: 144.0 + e_m / k,
            alt_ft,
            agl_ft: alt_ft,
            heading: hdg,
            speed_kts: 140,
            vspd_fpm: -700,
            on_ground: false,
            field: None,
            field_brg: 0,
            field_rng_m: 0,
            rotary: false,
            ai: false,
            ai_plan: None,
            ai_state: None,
            unit_type: "FA-18C_hornet".into(),
        }
    }

    fn texts(v: &[Tx]) -> String {
        v.iter().map(|t| t.text.clone()).collect::<Vec<_>>().join(" | ")
    }

    #[test]
    fn marshal_gives_each_aircraft_a_slot() {
        let c = ship(1);
        let mut k = Carrier::new(&c, None);
        let range = RangeLive { carriers: vec![c], mission_time: "10:00:00".into(), ..Default::default() };
        let mut atc = AtcPicture::default();
        atc.traffic = vec![jet(1, 30. * NM, 0., 20000, 180), jet(2, 32. * NM, 0., 22000, 180)];
        k.update(&Picture { atc: &atc, range: Some(&range), now: 0., mission_s: Some(36000.) });
        let caller = |u| Caller { unit_id: Some(u), srs_name: String::new() };
        let p = |s: &str| super::super::intent::parse(s, "Lincoln");
        let r = k.respond(Pos::Marshal, &caller(1), "Marshal, Rhino 11, 30 miles, angels 20, state 6.2", p("Marshal, Rhino 11, 30 miles, angels 20, state 6.2"), 0.);
        assert!(texts(&r).contains("case one recovery, BRC 360, hold overhead at angels 2, expected Charlie time 0-2"), "{}", texts(&r));
        let r = k.respond(Pos::Marshal, &caller(2), "Marshal, Rhino 12, 32 miles, angels 22, state 5.8", p("Marshal, Rhino 12, 32 miles, angels 22, state 5.8"), 1.);
        assert!(texts(&r).contains("angels 3, expected Charlie time 0-3"), "{}", texts(&r));
        // the first slot comes up: Charlie
        let r = k.update(&Picture { atc: &atc, range: Some(&range), now: 121., mission_s: Some(36121.) });
        assert!(texts(&r).contains("Rhino 1-1, Lincoln Marshal, Charlie. Paddles on 305.5"), "{}", texts(&r));
    }

    #[test]
    fn case_three_marshal_and_the_ball() {
        let c = ship(3);
        let mut k = Carrier::new(&c, None);
        let range = RangeLive { carriers: vec![c], mission_time: "22:10:00".into(), ..Default::default() };
        let mut atc = AtcPicture::default();
        atc.traffic = vec![jet(1, 25. * NM, 0., 7000, 180)];
        k.update(&Picture { atc: &atc, range: Some(&range), now: 0., mission_s: Some(22. * 3600. + 600.) });
        let caller = Caller { unit_id: Some(1), srs_name: String::new() };
        let r = k.respond(Pos::Marshal, &caller, "Lincoln Marshal, Rhino 11, checking in, 25 miles", super::super::intent::parse("Lincoln Marshal, Rhino 11, checking in, 25 miles", "Lincoln"), 0.);
        let t = texts(&r);
        assert!(t.contains("case three recovery, CV-1 approach, expected final bearing 351, marshal on the 171 radial, 21 DME, angels 6, expected approach time 1-2"), "{t}");
        // in the groove, high and left: Paddles talks
        let fb = 351f64.to_radians();
        let out_m = 0.6 * NM;
        let (n, e) = (-out_m * fb.cos() - 80., -out_m * fb.sin() - 40.);
        atc.traffic = vec![jet(1, n, e, 520, 351)];
        k.update(&Picture { atc: &atc, range: Some(&range), now: 200., mission_s: None });
        let r = k.respond(Pos::Approach, &caller, "Rhino 11, Hornet ball, 5.1", super::super::intent::parse("Rhino 11, Hornet ball, 5.1", "Lincoln"), 200.);
        assert!(texts(&r).contains("Roger ball, 27 knots"), "{}", texts(&r));
        assert_eq!(r[0].voice, Some(Pos::Paddles));
        let r = k.update(&Picture { atc: &atc, range: Some(&range), now: 203., mission_s: None });
        let t = texts(&r);
        assert!(t.contains("high") && t.contains("right for lineup"), "{t}");
    }
}
