// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! Range Control: who works which range, and when.
//!
//! Every range station (a bomb circle, a strafe pit, a convoy...) is booked in
//! slots, one flight at a time, so nobody drops on someone else's target run:
//!
//! - "Range Control, Viper 1-1, request R-7201 bomb circle." → "cleared into
//!   R-7201, your slot until 4-5, range is cold, report IP inbound", or, if
//!   it is taken, the time it frees up and a free range of the same kind.
//! - "IP inbound" → "cleared hot" (one flight hot on a station at a time).
//! - "Off safe" → acknowledged; "departing the range" frees the slot.
//! - Two minutes before a slot ends the flight is warned; at the end it is
//!   told to clear the range, and the next flight is called in.
//! - Someone working a range without a slot is booked into it if it is free,
//!   and told to hold off if it is not.
//!
//! AI flights on a range sortie do the same, and are held at their IP when
//! their range is not free yet.

use super::{
    controller::{type_label, Caller, Tx},
    facility::{range_bearing, Air, Calls, Clock, Facility, Mouth, Picture},
    intent::{Intent, Parsed},
    llm, Pos,
};
use crate::voice::{stt, FreqSpec};
use bfprotocols::range::{LiveStation, StationKind};
use serde::Deserialize;
use std::collections::HashMap;

fn d_true() -> bool {
    true
}
fn d_name() -> String {
    "Range Control".into()
}
fn d_freqs() -> Vec<FreqSpec> {
    vec![FreqSpec { mhz: 348.6, modulation: None, label: None }]
}
fn d_slot() -> f64 {
    20.
}

/// The `rangeControl` block of the range ATC config.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct RangeControlCfg {
    #[serde(default = "d_true")]
    pub enabled: bool,
    #[serde(default = "d_name")]
    pub name: String,
    #[serde(default = "d_freqs")]
    pub freqs: Vec<FreqSpec>,
    /// Slot length, minutes.
    #[serde(default = "d_slot")]
    pub slot_minutes: f64,
}

impl Default for RangeControlCfg {
    fn default() -> Self {
        serde_json::from_str("{}").unwrap()
    }
}

#[derive(Debug, Clone)]
struct Booking {
    unit: u64,
    call: String,
    start: f64,
    end: f64,
    hot: bool,
    warned: bool,
}

/// Where an AI flight is in its range sortie.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AiStage {
    Inbound,
    Booked,
    Hot,
    Done,
}

#[derive(Debug, Default)]
struct Book {
    /// Station id → bookings, by start time.
    by_station: HashMap<String, Vec<Booking>>,
}

impl Book {
    /// The earliest a slot of `len` can start on a station at or after `from`.
    fn next_free(&self, station: &str, from: f64, len: f64) -> f64 {
        let mut t = from;
        if let Some(v) = self.by_station.get(station) {
            let mut v: Vec<&Booking> = v.iter().collect();
            v.sort_by(|a, b| a.start.total_cmp(&b.start));
            for b in v {
                if b.end > t && b.start < t + len {
                    t = b.end;
                }
            }
        }
        t
    }

    fn of(&self, unit: u64) -> Option<(&str, &Booking)> {
        self.by_station
            .iter()
            .find_map(|(s, v)| v.iter().find(|b| b.unit == unit).map(|b| (s.as_str(), b)))
    }

    fn of_mut(&mut self, unit: u64) -> Option<(String, &mut Booking)> {
        self.by_station.iter_mut().find_map(|(s, v)| {
            v.iter_mut().find(|b| b.unit == unit).map(|b| (s.clone(), b))
        })
    }

    fn release(&mut self, unit: u64) {
        for v in self.by_station.values_mut() {
            v.retain(|b| b.unit != unit);
        }
    }

    fn book(&mut self, station: &str, b: Booking) {
        self.by_station.entry(station.to_string()).or_default().push(b);
    }

    /// Someone else is cleared hot on the station right now.
    fn hot_other(&self, station: &str, unit: u64) -> Option<&Booking> {
        self.by_station.get(station)?.iter().find(|b| b.hot && b.unit != unit)
    }
}

pub(crate) struct RangeControl {
    cfg: RangeControlCfg,
    stations: Vec<LiveStation>,
    air: Vec<Air>,
    book: Book,
    calls: Calls,
    clock: Clock,
    mouth: Mouth,
    now: f64,
    /// AI flights' progress on their range sortie.
    ai: HashMap<u64, AiStage>,
    /// Flights already told about working a range without a slot.
    nagged: HashMap<u64, f64>,
    /// Requests answered "standby, checking the schedule": due, flight,
    /// what it meant, what was heard.
    deferred: Vec<(f64, u64, Parsed, String)>,
    /// When the last range request came in.
    last_req: f64,
    /// The chance a request gets "standby" first (doubled right after
    /// another one).
    pub standby_odds: f64,
}

/// The way a controller says a station: "R-7201 FDM bomb circle".
fn spoken_station(s: &LiveStation) -> String {
    s.name.replace(" - ", " ").replace('-', "-")
}

fn kind_words(k: StationKind) -> &'static [&'static str] {
    match k {
        StationKind::BombCircle => &["bomb", "circle", "bombing"],
        StationKind::StrafePit => &["strafe", "strafing", "pit", "gun"],
        StationKind::TacticalArray => &["array", "tactical"],
        StationKind::Convoy => &["convoy", "moving"],
        StationKind::CoordTarget => &["coordinate", "jdam", "gps"],
        StationKind::LaserTarget => &["laser", "lase"],
        StationKind::ShipTarget => &["ship", "sinkex", "hulk"],
        StationKind::GunneryLane => &["gunnery", "lane"],
        StationKind::SamSite => &["sam", "threat", "sead"],
    }
}

impl RangeControl {
    pub(crate) fn new(cfg: RangeControlCfg) -> Self {
        RangeControl {
            cfg,
            stations: vec![],
            air: vec![],
            book: Book::default(),
            calls: Calls::default(),
            clock: Clock::default(),
            mouth: Mouth::default(),
            now: 0.,
            ai: HashMap::new(),
            nagged: HashMap::new(),
            deferred: vec![],
            last_req: f64::NEG_INFINITY,
            standby_odds: 0.35,
        }
    }

    fn me(&self) -> String {
        self.cfg.name.clone()
    }

    fn slot_len(&self) -> f64 {
        self.cfg.slot_minutes.max(5.) * 60.
    }

    fn station(&self, id: &str) -> Option<&LiveStation> {
        self.stations.iter().find(|s| s.id == id)
    }

    /// The station a call names: the best overlap of its words with each
    /// station's id and name, or of the kind words ("strafe", "convoy").
    fn named(&self, text: &str) -> Option<&LiveStation> {
        let t = text.to_lowercase();
        let digits: String = t.chars().filter(|c| c.is_ascii_digit()).collect();
        let mut best: Option<(usize, &LiveStation)> = None;
        for s in &self.stations {
            let mut score = 0;
            for w in s
                .name
                .split(|c: char| !c.is_alphanumeric())
                .chain(s.id.split(|c: char| !c.is_alphanumeric()))
            {
                if w.len() >= 3 && stt::mentions(&t, w) {
                    score += 2;
                }
                let wd: String = w.chars().filter(|c| c.is_ascii_digit()).collect();
                if wd.len() >= 2 && digits.contains(&wd) {
                    score += 3;
                }
            }
            if kind_words(s.kind).iter().any(|k| stt::mentions(&t, k)) {
                score += 1;
            }
            if score > 0 && best.is_none_or(|(b, _)| score > b) {
                best = Some((score, s));
            }
        }
        best.map(|(_, s)| s)
    }

    /// A station of the same kind that is free now, for "or ... is free".
    fn alternative(&self, kind: StationKind, not: &str) -> Option<&LiveStation> {
        self.stations.iter().filter(|s| s.kind == kind && s.id != not).find(|s| {
            self.book.next_free(&s.id, self.now, self.slot_len()) <= self.now + 60.
        })
    }

    /// Book `a` onto a station, and say so.
    fn book_into(&mut self, a: &Air, call: &str, sid: &str, out: &mut Vec<Tx>) {
        let Some(s) = self.station(sid).cloned() else { return };
        self.book.release(a.id);
        let len = self.slot_len();
        let start = self.book.next_free(&s.id, self.now, len);
        let end = start + len;
        self.book.book(
            &s.id,
            Booking {
                unit: a.id,
                call: call.to_string(),
                start,
                end,
                hot: false,
                warned: false,
            },
        );
        let me = self.me();
        let name = spoken_station(&s);
        let t = if start <= self.now + 60. {
            let laser =
                s.laser_code.map(|c| format!(", laser code {c}")).unwrap_or_default();
            format!(
                "{call}, {me}, cleared into {name}, your slot until {}, range is cold{laser}, report IP inbound.",
                self.clock.say_minute(end)
            )
        } else {
            let alt = self
                .alternative(s.kind, &s.id)
                .map(|x| format!(", or {} is free now", spoken_station(x)))
                .unwrap_or_default();
            format!(
                "{call}, {me}, {name} is in use, expect it at {}, hold clear of the range{alt}.",
                self.clock.say_minute(start)
            )
        };
        self.mouth.say(Pos::Range, None, a, call, &me, t, self.now, out);
        if a.ai && start > self.now + 60. {
            self.mouth.command(a, "hold", out);
        }
    }

    fn handle(
        &mut self,
        a: &Air,
        intent: Intent,
        p: &Parsed,
        heard: &str,
        out: &mut Vec<Tx>,
    ) {
        let call = self.calls.of(a);
        let me = self.me();
        let now = self.now;
        match intent {
            Intent::RequestRange | Intent::CheckIn => {
                let wanted = if p.destination.trim().is_empty() {
                    heard.to_string()
                } else {
                    p.destination.clone()
                };
                let pick = self.named(&wanted).map(|s| s.id.clone()).or_else(|| {
                    // "request range time": the first range free now
                    (intent == Intent::RequestRange)
                        .then(|| {
                            self.stations
                                .iter()
                                .find(|s| {
                                    self.book.next_free(&s.id, now, self.slot_len())
                                        <= now + 60.
                                })
                                .map(|s| s.id.clone())
                        })
                        .flatten()
                });
                match pick {
                    Some(sid) => self.book_into(a, &call, &sid, out),
                    None => {
                        let t = format!("{call}, {me}, say the range you want.");
                        self.mouth.say(Pos::Range, None, a, &call, &me, t, now, out);
                    }
                }
            }
            Intent::IpInbound => {
                let Some((sid, b)) =
                    self.book.of(a.id).map(|(s, b)| (s.to_string(), b.clone()))
                else {
                    let t = format!("{call}, {me}, you have no slot, remain clear, say the range you want.");
                    return self.mouth.say(Pos::Range, None, a, &call, &me, t, now, out);
                };
                if b.start > now + 30. {
                    let t = format!(
                        "{call}, {me}, negative, your slot starts at {}, hold clear.",
                        self.clock.say_minute(b.start)
                    );
                    return self.mouth.say(Pos::Range, None, a, &call, &me, t, now, out);
                }
                if let Some(o) = self.book.hot_other(&sid, a.id) {
                    let t = format!(
                        "{call}, {me}, continue, {} is still hot, report IP again.",
                        o.call
                    );
                    return self.mouth.say(Pos::Range, None, a, &call, &me, t, now, out);
                }
                if let Some((_, b)) = self.book.of_mut(a.id) {
                    b.hot = true;
                }
                let t = format!("{call}, {me}, cleared hot, report off.");
                self.mouth.say(Pos::Range, None, a, &call, &me, t, now, out);
            }
            Intent::OffTarget => {
                if let Some((_, b)) = self.book.of_mut(a.id) {
                    b.hot = false;
                }
                let t = format!(
                    "{call}, {}",
                    self.mouth.pick(&[
                        "copy off.",
                        "roger, off.",
                        "copy, cleared to reattack, report IP."
                    ])
                );
                self.mouth.say(Pos::Range, None, a, &call, &me, t, now, out);
            }
            Intent::DepartingRange => {
                let sid = self.book.of(a.id).map(|(s, _)| s.to_string());
                self.book.release(a.id);
                if let Some(sid) = sid {
                    self.call_next(&sid, now, out);
                }
                let t = format!(
                    "{call}, {me}, cleared off the range{}",
                    self.mouth.pick(&[
                        ", thanks for the business.",
                        ", results are on the site.",
                        ", good hits today."
                    ])
                );
                self.mouth.say(Pos::Range, None, a, &call, &me, t, now, out);
            }
            _ => {}
        }
    }
}

impl RangeControl {
    /// A range has come free: the next flight booked on it gets it now, its
    /// slot moved up to start now.
    fn call_next(&mut self, sid: &str, now: f64, out: &mut Vec<Tx>) {
        let me = self.me();
        let Some(b) = self.book.by_station.get_mut(sid).and_then(|v| {
            v.iter_mut().filter(|b| !b.hot).min_by(|a, b| a.start.total_cmp(&b.start))
        }) else {
            return;
        };
        if b.start > now {
            let len = b.end - b.start;
            b.start = now;
            b.end = now + len;
        }
        let (unit, call) = (b.unit, b.call.clone());
        if let Some(a) = self.air.iter().find(|a| a.id == unit).cloned() {
            let name = self.station(sid).map(spoken_station).unwrap_or_default();
            let t =
                format!("{call}, {me}, {name} is yours, cleared in, report IP inbound.");
            self.mouth.say(Pos::Range, None, &a, &call, &me, t, now, out);
            self.mouth.command(&a, "resume", out);
        }
    }
}

impl Facility for RangeControl {
    fn name(&self) -> String {
        self.cfg.name.clone()
    }

    fn radios(&self) -> Vec<(Pos, Vec<FreqSpec>)> {
        vec![(Pos::Range, self.cfg.freqs.clone())]
    }

    fn facility(&self, _pos: Pos) -> String {
        self.me()
    }

    fn label(&self, _pos: Pos) -> String {
        "Range".into()
    }

    fn wake(&self) -> String {
        "Range".into()
    }

    fn update(&mut self, pic: &Picture) -> Vec<Tx> {
        self.now = pic.now;
        self.clock.sync(pic);
        if let Some(r) = pic.range {
            self.stations = r.stations.clone();
        }
        self.air = super::facility::traffic(pic.atc);
        let now = self.now;
        let me = self.me();
        let mut out = vec![];

        // Answers after a "standby".
        let (due, keep): (Vec<_>, Vec<_>) =
            std::mem::take(&mut self.deferred).into_iter().partition(|d| d.0 <= now);
        self.deferred = keep;
        for (_, id, p, heard) in due {
            if let Some(a) = self.air.iter().find(|a| a.id == id).cloned() {
                self.handle(&a, p.intent, &p, &heard, &mut out);
            }
        }

        // Bookings: warn, expire, and drop flights that have gone.
        let alive: Vec<u64> = self.air.iter().map(|a| a.id).collect();
        let mut expired = vec![];
        let mut warn = vec![];
        for (sid, v) in self.book.by_station.iter_mut() {
            v.retain(|b| alive.contains(&b.unit));
            for b in v.iter_mut() {
                if now >= b.end {
                    expired.push((sid.clone(), b.unit));
                } else if !b.warned && b.end - now < 120. && b.start <= now {
                    b.warned = true;
                    warn.push(b.unit);
                }
            }
        }
        for u in warn {
            if let Some(a) = self.air.iter().find(|a| a.id == u).cloned() {
                let call = self.calls.of(&a);
                let t = format!("{call}, {me}, two minutes remaining in your slot.");
                self.mouth.say(Pos::Range, None, &a, &call, &me, t, now, &mut out);
            }
        }
        for (sid, u) in expired {
            self.book.release(u);
            if let Some(a) = self.air.iter().find(|a| a.id == u).cloned() {
                let call = self.calls.of(&a);
                let t = format!("{call}, {me}, your time is up, cleared off the range.");
                self.mouth.say(Pos::Range, None, &a, &call, &me, t, now, &mut out);
            }
            self.call_next(&sid, now, &mut out);
        }

        // Working a range without a slot.
        let stations = self.stations.clone();
        for s in &stations {
            for name in &s.hot_by {
                let Some(a) = self.air.iter().find(|a| !a.ai && &a.name == name).cloned()
                else {
                    continue;
                };
                if self.book.of(a.id).is_some_and(|(sid, _)| sid == s.id) {
                    continue;
                }
                if self.nagged.get(&a.id).is_some_and(|t| now - t < 300.) {
                    continue;
                }
                self.nagged.insert(a.id, now);
                let call = self.calls.of(&a);
                if self.book.next_free(&s.id, now, self.slot_len()) <= now + 60. {
                    self.book.release(a.id);
                    let end = now + self.slot_len();
                    self.book.book(
                        &s.id,
                        Booking {
                            unit: a.id,
                            call: call.clone(),
                            start: now,
                            end,
                            hot: true,
                            warned: false,
                        },
                    );
                    let t = format!(
                        "{call}, {me}, you're working {} without a slot. It's free: you're booked until {}, call Range Control before you go in next time.",
                        spoken_station(s),
                        self.clock.say_minute(end)
                    );
                    self.mouth.say(Pos::Range, None, &a, &call, &me, t, now, &mut out);
                } else {
                    let t = format!(
                        "{call}, {me}, {} is booked by another flight. Cease, remain clear, and call me for a slot.",
                        spoken_station(s)
                    );
                    self.mouth.say(Pos::Range, None, &a, &call, &me, t, now, &mut out);
                }
            }
        }

        // AI flights on a range sortie talk to us like anyone else.
        let ai: Vec<Air> = self
            .air
            .iter()
            .filter(|a| {
                a.ai && a
                    .ai_plan
                    .as_deref()
                    .is_some_and(|p| p.starts_with("range:") && !p.ends_with(":home"))
            })
            .cloned()
            .collect();
        self.ai.retain(|u, _| ai.iter().any(|a| a.id == *u));
        for a in ai {
            let sid = a
                .ai_plan
                .as_deref()
                .unwrap_or("")
                .trim_start_matches("range:")
                .to_string();
            let Some(s) = self.station(&sid).cloned() else { continue };
            let (nm, _) = range_bearing(s.pos.lat, s.pos.lon, a.lat, a.lon);
            let call = self.calls.of(&a);
            let stage = *self.ai.entry(a.id).or_insert(AiStage::Inbound);
            if a.on_ground {
                continue;
            }
            match stage {
                AiStage::Inbound if nm < 45. => {
                    let t = format!(
                        "{me}, {call}{}, request {}.",
                        super::controller::spoken_type(&a.typ),
                        spoken_station(&s)
                    );
                    self.mouth.pilot(Pos::Range, &a, &call, t, now, &mut out);
                    self.book_into(&a, &call, &s.id, &mut out);
                    self.ai.insert(a.id, AiStage::Booked);
                }
                AiStage::Booked if nm < 9. => {
                    let mine = self
                        .book
                        .of(a.id)
                        .is_some_and(|(id, b)| id == s.id && b.start <= now + 30.);
                    if mine && self.book.hot_other(&s.id, a.id).is_none() {
                        let t = format!("{me}, {call}, IP inbound.");
                        self.mouth.pilot(Pos::Range, &a, &call, t, now, &mut out);
                        let p = Parsed::of(Intent::IpInbound);
                        self.handle(&a, Intent::IpInbound, &p, "", &mut out);
                        self.ai.insert(a.id, AiStage::Hot);
                    }
                }
                AiStage::Hot if nm > 12. => {
                    let t = format!("{me}, {call}, off safe, departing the range.");
                    self.mouth.pilot(Pos::Range, &a, &call, t, now, &mut out);
                    let p = Parsed::of(Intent::DepartingRange);
                    self.handle(&a, Intent::DepartingRange, &p, "", &mut out);
                    self.ai.insert(a.id, AiStage::Done);
                }
                _ => {}
            }
        }
        out
    }

    fn respond(
        &mut self,
        pos: Pos,
        who: &Caller,
        heard: &str,
        parsed: Option<Parsed>,
        now: f64,
    ) -> Vec<Tx> {
        self.now = now;
        let mut out = vec![];
        let me = self.me();
        let Some(a) =
            super::facility::locate(&self.air, who, parsed.as_ref(), &self.calls.0)
                .cloned()
        else {
            if parsed
                .as_ref()
                .is_some_and(|p| p.intent != Intent::None && p.intent != Intent::Readback)
            {
                self.mouth.broadcast(
                    pos,
                    None,
                    format!("Station calling {me}, say again callsign."),
                    now,
                    &mut out,
                );
            }
            return out;
        };
        self.calls.learn(&a, parsed.as_ref());
        let call = self.calls.of(&a);
        match parsed {
            None => {
                if stt::mentions(heard, "range") {
                    let t = format!("{call}, {me}, say again.");
                    self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
                }
            }
            Some(p) => match p.intent {
                // Range Control checks the schedule before it answers, now and then.
                Intent::RequestRange
                    if !a.ai && {
                        let odds = if now - self.last_req < 30. {
                            2. * self.standby_odds
                        } else {
                            self.standby_odds
                        };
                        self.last_req = now;
                        self.mouth.rand() < odds
                    } =>
                {
                    let t = format!(
                        "{call}, {me}, {}",
                        self.mouth.pick(&[
                            "standby, checking the schedule.",
                            "standby.",
                            "copy, standby one."
                        ])
                    );
                    self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
                    let due = now + self.mouth.between(5., 9.);
                    self.deferred.push((due, a.id, p, heard.to_string()));
                }
                Intent::RadioCheck => {
                    let t = format!("{call}, {me}, loud and clear.");
                    self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
                }
                Intent::RequestRange
                | Intent::CheckIn
                | Intent::IpInbound
                | Intent::OffTarget
                | Intent::DepartingRange => {
                    let i = p.intent;
                    self.handle(&a, i, &p, heard, &mut out);
                }
                Intent::None | Intent::Readback => {}
                _ => {
                    let t = format!("{call}, {me}, say request.");
                    self.mouth.say(pos, None, &a, &call, &me, t, now, &mut out);
                }
            },
        }
        out
    }

    fn caller(
        &mut self,
        who: &Caller,
        parsed: Option<&Parsed>,
    ) -> (Option<String>, Option<String>) {
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
        let situation = match a.and_then(|a| self.book.of(a.id)) {
            Some((sid, b)) => format!(
                "booked on range station {} until minute {}{}",
                sid,
                self.clock.minute(b.end),
                if b.hot { ", cleared hot" } else { "" }
            ),
            None => "no range slot".into(),
        };
        llm::Context {
            position: self.facility(pos),
            situation,
            last_instruction: None,
            heard: heard.to_string(),
        }
    }

    fn vocabulary(&self) -> Vec<String> {
        let mut v = vec![
            self.me(),
            "IP inbound".into(),
            "off safe".into(),
            "cleared hot".into(),
            "winchester".into(),
        ];
        v.extend(self.stations.iter().map(|s| s.name.clone()));
        v
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bfprotocols::{
        atc::{AtcPicture, AtcTraffic},
        range::{GeoPt, RangeLive},
    };

    fn station(
        id: &str,
        name: &str,
        kind: StationKind,
        lat: f64,
        hot: &[&str],
    ) -> LiveStation {
        LiveStation {
            id: id.into(),
            name: name.into(),
            kind,
            pos: GeoPt { lat, lon: 145.0, alt_m: 0. },
            hot_by: hot.iter().map(|s| s.to_string()).collect(),
            targets_alive: 1,
            targets_total: 1,
            laser_code: None,
            rings_m: vec![],
            note: None,
            elev_m: None,
            atmo: vec![],
        }
    }

    fn plane(id: u64, name: &str, lat: f64) -> AtcTraffic {
        AtcTraffic {
            ucid: String::new(),
            unit_id: Some(id),
            callsign: "Viper 1-1".into(),
            player_name: name.into(),
            lat,
            lon: 145.0,
            alt_ft: 15000,
            agl_ft: 15000,
            heading: 0,
            speed_kts: 400,
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
    fn slots_one_flight_at_a_time() {
        let mut rc = RangeControl::new(RangeControlCfg::default());
        rc.standby_odds = 0.;
        let stations = vec![
            station(
                "r7201-bomb",
                "R-7201 FDM - Bomb Circle",
                StationKind::BombCircle,
                16.0,
                &[],
            ),
            station(
                "tn2-easy",
                "TN-2 Tinian MLA - Easy",
                StationKind::BombCircle,
                15.0,
                &[],
            ),
            station(
                "r7201-strafe",
                "R-7201 FDM - Strafe Pit",
                StationKind::StrafePit,
                16.0,
                &[],
            ),
        ];
        let mut atc = AtcPicture::default();
        atc.traffic = vec![plane(1, "alice", 15.5), plane(2, "bob", 15.5)];
        let range =
            RangeLive { stations, mission_time: "10:00:00".into(), ..Default::default() };
        let pic =
            Picture { atc: &atc, range: Some(&range), now: 0., mission_s: Some(36000.) };
        rc.update(&pic);

        let caller = |u| Caller { unit_id: Some(u), srs_name: String::new() };
        let req = |s: &str| super::super::intent::parse(s, "Range");
        let r = rc.respond(
            Pos::Range,
            &caller(1),
            "Range Control, Viper 11, request R-7201 bomb circle",
            req("Range Control, Viper 11, request R-7201 bomb circle"),
            0.,
        );
        assert!(
            texts(&r)
                .contains("cleared into R-7201 FDM Bomb Circle, your slot until 2-0"),
            "{}",
            texts(&r)
        );
        // the second flight is told when, and offered the other bomb circle
        let r = rc.respond(
            Pos::Range,
            &caller(2),
            "Range Control, Enfield 21, request R-7201 bomb circle",
            req("Range Control, Enfield 21, request R-7201 bomb circle"),
            10.,
        );
        let t = texts(&r);
        assert!(
            t.contains("is in use, expect it at 2-0")
                && t.contains("TN-2 Tinian MLA Easy is free now"),
            "{t}"
        );
        // hot, one at a time
        let r = rc.respond(
            Pos::Range,
            &caller(1),
            "Viper 11 IP inbound",
            req("Viper 11 IP inbound"),
            20.,
        );
        assert!(texts(&r).contains("cleared hot"), "{}", texts(&r));
        // the slot runs out: warned, then cleared off, and the next flight is called in
        let pic = Picture {
            atc: &atc,
            range: Some(&range),
            now: 19. * 60.,
            mission_s: Some(36000. + 19. * 60.),
        };
        let r = rc.update(&pic);
        assert!(texts(&r).contains("two minutes remaining"), "{}", texts(&r));
        let pic = Picture {
            atc: &atc,
            range: Some(&range),
            now: 20. * 60. + 1.,
            mission_s: Some(36000. + 20. * 60. + 1.),
        };
        let r = rc.update(&pic);
        let t = texts(&r);
        assert!(
            t.contains("your time is up")
                && t.contains(
                    "Enfield 2-1, Range Control, R-7201 FDM Bomb Circle is yours"
                ),
            "{t}"
        );
    }

    #[test]
    fn leaving_early_calls_the_next_flight_in() {
        let mut rc = RangeControl::new(RangeControlCfg::default());
        rc.standby_odds = 0.;
        let stations = vec![station(
            "r7201-bomb",
            "R-7201 FDM - Bomb Circle",
            StationKind::BombCircle,
            16.0,
            &[],
        )];
        let mut atc = AtcPicture::default();
        atc.traffic = vec![plane(1, "alice", 15.5), plane(2, "bob", 15.5)];
        let range =
            RangeLive { stations, mission_time: "10:00:00".into(), ..Default::default() };
        let pic =
            Picture { atc: &atc, range: Some(&range), now: 0., mission_s: Some(36000.) };
        rc.update(&pic);
        let caller = |u| Caller { unit_id: Some(u), srs_name: String::new() };
        let req = |s: &str| super::super::intent::parse(s, "Range");
        rc.respond(
            Pos::Range,
            &caller(1),
            "Range Control, Viper 11, request R-7201 bomb circle",
            req("Range Control, Viper 11, request R-7201 bomb circle"),
            0.,
        );
        rc.respond(
            Pos::Range,
            &caller(2),
            "Range Control, Enfield 21, request R-7201 bomb circle",
            req("Range Control, Enfield 21, request R-7201 bomb circle"),
            10.,
        );
        let r = rc.respond(
            Pos::Range,
            &caller(1),
            "Range Control, Viper 11, departing the range",
            req("Range Control, Viper 11, departing the range"),
            300.,
        );
        let t = texts(&r);
        assert!(
            t.contains("cleared off the range")
                && t.contains(
                    "Enfield 2-1, Range Control, R-7201 FDM Bomb Circle is yours"
                ),
            "{t}"
        );
    }

    #[test]
    fn standby_then_the_booking() {
        let mut rc = RangeControl::new(RangeControlCfg::default());
        rc.standby_odds = 1.;
        let stations = vec![station(
            "r7201-bomb",
            "R-7201 FDM - Bomb Circle",
            StationKind::BombCircle,
            16.0,
            &[],
        )];
        let mut atc = AtcPicture::default();
        atc.traffic = vec![plane(1, "alice", 15.5)];
        let range =
            RangeLive { stations, mission_time: "10:00:00".into(), ..Default::default() };
        rc.update(&Picture {
            atc: &atc,
            range: Some(&range),
            now: 0.,
            mission_s: Some(36000.),
        });
        let caller = Caller { unit_id: Some(1), srs_name: String::new() };
        let heard = "Range Control, Viper 11, request R-7201 bomb circle";
        let r = rc.respond(
            Pos::Range,
            &caller,
            heard,
            super::super::intent::parse(heard, "Range"),
            0.,
        );
        assert!(texts(&r).contains("stand"), "{}", texts(&r));
        let r = rc.update(&Picture {
            atc: &atc,
            range: Some(&range),
            now: 10.,
            mission_s: Some(36010.),
        });
        assert!(
            texts(&r).contains("cleared into R-7201 FDM Bomb Circle"),
            "{}",
            texts(&r)
        );
    }

    #[test]
    fn unscheduled_flight_is_booked_if_free() {
        let mut rc = RangeControl::new(RangeControlCfg::default());
        rc.standby_odds = 0.;
        let stations = vec![station(
            "tn1-array",
            "TN-1 North Field - Tactical Array",
            StationKind::TacticalArray,
            15.0,
            &["alice"],
        )];
        let mut atc = AtcPicture::default();
        atc.traffic = vec![plane(1, "alice", 15.0)];
        let range =
            RangeLive { stations, mission_time: "10:00:00".into(), ..Default::default() };
        let pic =
            Picture { atc: &atc, range: Some(&range), now: 0., mission_s: Some(36000.) };
        let r = rc.update(&pic);
        assert!(texts(&r).contains("without a slot. It's free"), "{}", texts(&r));
    }
}
