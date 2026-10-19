// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! A tanker's crew on the tanker's frequency, running the rejoin and the line
//! of receivers so everyone gets a turn on the boom (or the basket).
//!
//! - "Texaco 1-1, Viper 1-1, request rejoin" → cleared to join, with the
//!   tanker's altitude, heading and speed; the first receiver is cleared
//!   straight to pre-contact, the others to the left observation position in
//!   order ("number 2").
//! - Stabilised behind the tanker → "cleared contact" (a boom) or "cleared to
//!   plug" (a basket).
//! - Off the boom and back out → "cleared to depart", and the next receiver
//!   in line is cleared astern.
//!
//! AI flights on a tanker sortie join and refuel the same way.

use super::{
    controller::{spoken_type, type_label, Caller, Tx},
    facility::{Air, Calls, Facility, Local, Mouth, Picture, NM},
    intent::{Intent, Parsed},
    llm, speech, Pos,
};
use crate::voice::{stt, FreqSpec};
use bfprotocols::range::{LiveTanker, RefuelMethod, TankerState};

/// "Texaco1" → "Texaco 1-1".
pub(crate) fn spoken_callsign(cs: &str) -> String {
    let word: String = cs.chars().take_while(|c| !c.is_ascii_digit()).collect();
    let num: String = cs.chars().filter(|c| c.is_ascii_digit()).collect();
    match num.len() {
        0 => cs.to_string(),
        1 => format!("{} {num}-1", word.trim()),
        _ => speech::format_callsign(word.trim(), &num),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stage {
    /// Cleared to join; waiting its turn at the observation position.
    Waiting,
    /// Cleared astern / to pre-contact.
    Astern,
    /// Stabilised behind the tanker and cleared to contact.
    Contact,
}

#[derive(Debug, Clone)]
struct Receiver {
    unit: u64,
    call: String,
    stage: Stage,
    since: f64,
    /// Was on the boom (close in behind) at some point.
    plugged: bool,
}

pub(crate) struct Tanker {
    /// The engine's tanker id.
    pub id: String,
    live: LiveTanker,
    call: String,
    line: Vec<Receiver>,
    air: Vec<Air>,
    calls: Calls,
    mouth: Mouth,
    now: f64,
    gone: bool,
    rtb_said: bool,
    /// AI receivers already on our frequency.
    ai_seen: Vec<u64>,
}

impl Tanker {
    pub(crate) fn new(t: &LiveTanker) -> Self {
        let mut seed = 0u64;
        for b in t.id.bytes() {
            seed = seed.wrapping_mul(31).wrapping_add(b as u64);
        }
        Tanker {
            id: t.id.clone(),
            live: t.clone(),
            call: spoken_callsign(&t.callsign),
            line: vec![],
            air: vec![],
            calls: Calls::default(),
            mouth: Mouth::seeded(seed),
            now: 0.,
            gone: false,
            rtb_said: false,
            ai_seen: vec![],
        }
    }

    fn boom(&self) -> bool {
        matches!(self.live.method, RefuelMethod::Boom)
    }

    /// Where a receiver is relative to the tanker: (metres behind, metres to
    /// the side, feet below).
    fn relative(&self, a: &Air) -> (f64, f64, f64) {
        let l = Local::at(self.live.pos.lat, self.live.pos.lon);
        let (n, e) = l.xy(a.lat, a.lon);
        let h = self.live.heading_deg.to_radians();
        let ahead = n * h.cos() + e * h.sin();
        let right = -n * h.sin() + e * h.cos();
        let tanker_ft = self.live.alt_ft;
        (-ahead, right, tanker_ft - a.alt_ft as f64)
    }

    fn altitude(&self) -> String {
        speech::alt((self.live.alt_ft / 100.).round() as i32 * 100)
    }

    fn join(&mut self, a: &Air, out: &mut Vec<Tx>) {
        let call = self.calls.of(a);
        let me = self.call.clone();
        let now = self.now;
        if self.line.iter().any(|r| r.unit == a.id) {
            let n = self.line.iter().position(|r| r.unit == a.id).unwrap_or(0) + 1;
            let t = format!("{call}, {me}, you're number {n}, stand by.");
            return self.mouth.say(Pos::Tanker, None, a, &call, &me, t, now, out);
        }
        let first = self.line.is_empty();
        let n = self.line.len() + 1;
        self.line.push(Receiver { unit: a.id, call: call.clone(), stage: if first { Stage::Astern } else { Stage::Waiting }, since: now, plugged: false });
        let track = format!(
            "{}, heading {}, {} knots",
            self.altitude(),
            speech::hdg(self.live.heading_deg),
            (self.live.speed_kts / 5.).round() as i32 * 5
        );
        let t = if first {
            format!("{call}, {me}, cleared to join, {track}, cleared pre-contact.")
        } else {
            format!("{call}, {me}, cleared to join, {track}, you're number {n}, cleared to the left observation.")
        };
        self.mouth.say(Pos::Tanker, None, a, &call, &me, t, now, out);
    }

    fn done(&mut self, unit: u64, out: &mut Vec<Tx>) {
        let me = self.call.clone();
        let now = self.now;
        if let Some(i) = self.line.iter().position(|r| r.unit == unit) {
            let r = self.line.remove(i);
            if let Some(a) = self.air.iter().find(|a| a.id == unit).cloned() {
                let t = format!(
                    "{}, {me}, cleared to depart{}",
                    r.call,
                    self.mouth.pick(&[", thanks for the business.", ", good hunting.", ", see you next time."])
                );
                self.mouth.say(Pos::Tanker, None, &a, &r.call, &me, t, now, out);
            }
            // the next in line moves up
            if i == 0 {
                if let Some(next) = self.line.first_mut() {
                    next.stage = Stage::Astern;
                    next.since = now;
                    let call = next.call.clone();
                    if let Some(a) = self.air.iter().find(|a| a.id == next.unit).cloned() {
                        let t = format!("{call}, {me}, cleared astern, cleared pre-contact.");
                        self.mouth.say(Pos::Tanker, None, &a, &call, &me, t, now, out);
                    }
                }
            }
        }
    }
}

impl Facility for Tanker {
    fn name(&self) -> String {
        self.call.clone()
    }

    fn radios(&self) -> Vec<(Pos, Vec<FreqSpec>)> {
        vec![(Pos::Tanker, vec![FreqSpec { mhz: self.live.freq_mhz, modulation: None, label: None }])]
    }

    fn facility(&self, _pos: Pos) -> String {
        self.call.clone()
    }

    fn label(&self, _pos: Pos) -> String {
        self.call.clone()
    }

    fn wake(&self) -> String {
        self.call.split(' ').next().unwrap_or("").to_string()
    }

    fn gone(&self) -> bool {
        self.gone
    }

    fn update(&mut self, pic: &Picture) -> Vec<Tx> {
        self.now = pic.now;
        let now = self.now;
        let me = self.call.clone();
        let mut out = vec![];
        match pic.range.and_then(|r| r.tankers.iter().find(|t| t.id == self.id)) {
            Some(t) => self.live = t.clone(),
            None => {
                self.gone = true;
                return out;
            }
        }
        self.air = super::facility::traffic(pic.atc);
        let alive: Vec<u64> = self.air.iter().map(|a| a.id).collect();
        self.line.retain(|r| alive.contains(&r.unit));

        if self.live.state == TankerState::Rtb && !self.rtb_said && !self.line.is_empty() {
            self.rtb_said = true;
            self.mouth.broadcast(Pos::Tanker, None, format!("All receivers, {me} is RTB, cleared to depart."), now, &mut out);
            self.line.clear();
        }

        // The receiver at the front: pre-contact, contact, and off again.
        if let Some(r) = self.line.first().cloned() {
            if let Some(a) = self.air.iter().find(|a| a.id == r.unit).cloned() {
                let (behind, side, below) = self.relative(&a);
                let close = behind > 10. && behind < 250. && side.abs() < 60. && below.abs() < 300.;
                let on = behind > 5. && behind < 90. && side.abs() < 25.;
                if r.stage == Stage::Astern && close && (a.gs - self.live.speed_kts).abs() < 25. {
                    let t = if self.boom() {
                        format!("{}, {me}, stabilized pre-contact, cleared contact.", r.call)
                    } else {
                        format!("{}, {me}, cleared to plug.", r.call)
                    };
                    self.mouth.say(Pos::Tanker, None, &a, &r.call, &me, t, now, &mut out);
                    if let Some(x) = self.line.first_mut() {
                        x.stage = Stage::Contact;
                        x.since = now;
                    }
                }
                if on {
                    if let Some(x) = self.line.first_mut() {
                        x.plugged = true;
                    }
                }
                // backed out well behind after taking fuel, or wandered off
                let gone_far = behind.abs() > 2. * NM || side.abs() > NM;
                let backed_off = r.plugged && behind > 800.;
                if gone_far || backed_off || (r.stage == Stage::Contact && now - r.since > 20. * 60.) {
                    self.done(r.unit, &mut out);
                }
            }
        }

        // AI receivers on a tanker sortie ask to join when they are close.
        let mine = format!("tanker:{}", self.id);
        let ai: Vec<Air> = self.air.iter().filter(|a| a.ai && a.ai_plan.as_deref() == Some(mine.as_str())).cloned().collect();
        for a in ai {
            if self.ai_seen.contains(&a.id) || a.on_ground {
                continue;
            }
            let (behind, side, _) = self.relative(&a);
            if (behind * behind + side * side).sqrt() < 12. * NM {
                self.ai_seen.push(a.id);
                let call = self.calls.of(&a);
                let t = format!("{me}, {call}{}, request rejoin.", spoken_type(&a.typ));
                self.mouth.pilot(Pos::Tanker, &a, &call, t, now, &mut out);
                self.join(&a, &mut out);
            }
        }
        out
    }

    fn respond(&mut self, pos: Pos, who: &Caller, heard: &str, parsed: Option<Parsed>, now: f64) -> Vec<Tx> {
        self.now = now;
        let mut out = vec![];
        let me = self.call.clone();
        let Some(a) = super::facility::locate(&self.air, who, parsed.as_ref(), &self.calls.0).cloned() else {
            return out;
        };
        self.calls.learn(&a, parsed.as_ref());
        let call = self.calls.of(&a);
        match parsed.map(|p| p.intent) {
            None => {
                if stt::mentions(heard, &self.wake()) {
                    let t = format!("{call}, {me}, say again.");
                    self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
                }
            }
            Some(Intent::RadioCheck) => {
                let t = format!("{call}, {me}, loud and clear.");
                self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
            }
            Some(Intent::RequestFuel) | Some(Intent::CheckIn) => self.join(&a, &mut out),
            Some(Intent::FuelComplete) | Some(Intent::RequestFrequencyChange) => self.done(a.id, &mut out),
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

    fn llm_context(&mut self, _pos: Pos, who: &Caller, heard: &str) -> llm::Context {
        let a = super::facility::locate(&self.air, who, None, &self.calls.0);
        let situation = match a.and_then(|a| self.line.iter().position(|r| r.unit == a.id)) {
            Some(0) => "first in line to refuel from this tanker".into(),
            Some(n) => format!("number {} in line to refuel from this tanker", n + 1),
            None => "not yet joined on this tanker".into(),
        };
        llm::Context { position: self.call.clone(), situation, last_instruction: None, heard: heard.to_string() }
    }

    fn vocabulary(&self) -> Vec<String> {
        vec![self.call.clone(), "request rejoin".into(), "pre-contact".into(), "fuel complete".into()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bfprotocols::{
        atc::{AtcPicture, AtcTraffic},
        range::{GeoPt, RangeLive},
    };

    fn tanker() -> LiveTanker {
        LiveTanker {
            id: "texaco".into(),
            callsign: "Texaco1".into(),
            unit_type: "KC-135".into(),
            method: RefuelMethod::Boom,
            pos: GeoPt { lat: 15.0, lon: 146.0, alt_m: 7620. },
            heading_deg: 90.,
            speed_kts: 280.,
            alt_ft: 25000.,
            tacan: None,
            freq_mhz: 251.0,
            state: TankerState::OnStation,
            receivers: vec![],
            owner: None,
            recovery_for: None,
        }
    }

    /// A receiver `behind` metres behind the tanker (heading east).
    fn receiver(id: u64, name: &str, behind: f64) -> AtcTraffic {
        AtcTraffic {
            ucid: String::new(),
            unit_id: Some(id),
            callsign: if id == 1 { "Viper 1-1".into() } else { "Enfield 2-1".into() },
            player_name: name.into(),
            lat: 15.0,
            lon: 146.0 - behind / (111_320. * 15f64.to_radians().cos()),
            alt_ft: 24950,
            agl_ft: 24950,
            heading: 90,
            speed_kts: 280,
            vspd_fpm: 0,
            on_ground: false,
            field: None,
            field_brg: 0,
            field_rng_m: 0,
            rotary: false,
            ai: false,
            ai_plan: None,
            ai_state: None,
            unit_type: "F-16C_50".into(),
        }
    }

    fn texts(v: &[Tx]) -> String {
        v.iter().map(|t| t.text.clone()).collect::<Vec<_>>().join(" | ")
    }

    #[test]
    fn callsigns() {
        assert_eq!(spoken_callsign("Texaco1"), "Texaco 1-1");
        assert_eq!(spoken_callsign("Arco3"), "Arco 3-1");
        assert_eq!(spoken_callsign("Shell12"), "Shell 1-2");
    }

    #[test]
    fn receivers_take_turns() {
        let t = tanker();
        let mut k = Tanker::new(&t);
        let range = RangeLive { tankers: vec![t], ..Default::default() };
        let mut atc = AtcPicture::default();
        atc.traffic = vec![receiver(1, "alice", 3000.), receiver(2, "bob", 4000.)];
        k.update(&Picture { atc: &atc, range: Some(&range), now: 0., mission_s: None });
        let caller = |u| Caller { unit_id: Some(u), srs_name: String::new() };
        let p = |s: &str| super::super::intent::parse(s, "Texaco");
        let r = k.respond(Pos::Tanker, &caller(1), "Texaco 11, Viper 11, request rejoin", p("Texaco 11, Viper 11, request rejoin"), 1.);
        assert!(texts(&r).contains("cleared to join, FL250, heading 090, 280 knots, cleared pre-contact"), "{}", texts(&r));
        let r = k.respond(Pos::Tanker, &caller(2), "Texaco 11, Enfield 21, request rejoin", p("Texaco 11, Enfield 21, request rejoin"), 2.);
        assert!(texts(&r).contains("number 2, cleared to the left observation"), "{}", texts(&r));
        // Viper stabilises 60 m behind: cleared contact
        atc.traffic[0] = receiver(1, "alice", 60.);
        let r = k.update(&Picture { atc: &atc, range: Some(&range), now: 30., mission_s: None });
        assert!(texts(&r).contains("Viper 1-1, Texaco 1-1, stabilized pre-contact, cleared contact"), "{}", texts(&r));
        // fuelled and backed out: Enfield is cleared astern
        atc.traffic[0] = receiver(1, "alice", 1500.);
        let r = k.update(&Picture { atc: &atc, range: Some(&range), now: 300., mission_s: None });
        let t = texts(&r);
        assert!(t.contains("Viper 1-1, Texaco 1-1, cleared to depart") && t.contains("Enfield 2-1, Texaco 1-1, cleared astern"), "{t}");
    }
}
