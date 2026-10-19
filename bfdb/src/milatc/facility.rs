// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! What every controlling facility shares: an airfield's tower and radar
//! rooms, a carrier's CATCC, a tanker crew, Range Control.
//!
//! A [`Facility`] owns one or more positions (frequencies), is fed the
//! engine's picture once a second, and answers what is heard on its
//! frequencies. The runtime in [`super`] does the radio, voices, transcript
//! and live feed for all of them alike.

use super::{
    controller::{guess_callsign, norm, readback, type_label, Caller, Tx},
    intent::Parsed,
    llm, Pos,
};
use crate::voice::FreqSpec;
use bfprotocols::{
    atc::{AtcPicture, AtcTraffic},
    range::RangeLive,
};
use std::collections::HashMap;

/// What the facilities work from, once a second.
pub(crate) struct Picture<'a> {
    pub atc: &'a AtcPicture,
    /// The range engine's live picture: carriers, tankers, range stations.
    pub range: Option<&'a RangeLive>,
    /// The runtime clock, seconds.
    pub now: f64,
    /// The mission clock, seconds since midnight, now.
    pub mission_s: Option<f64>,
}

pub(crate) trait Facility: Send {
    /// What it is called in logs and in the SRS client list ("Lincoln CATCC").
    fn name(&self) -> String;
    /// The positions it works and their frequencies.
    fn radios(&self) -> Vec<(Pos, Vec<FreqSpec>)>;
    /// What a position calls itself on the radio ("Lincoln Marshal").
    fn facility(&self, pos: Pos) -> String;
    /// The short name for the site's tabs ("Marshal", "Texaco 1-1").
    fn label(&self, pos: Pos) -> String;
    /// The word pilots address it by, for the parser ("Andersen", "Texaco").
    fn wake(&self) -> String;
    /// A new picture: what it says unprompted.
    fn update(&mut self, pic: &Picture) -> Vec<Tx>;
    /// Answer a transmission heard on `pos`.
    fn respond(&mut self, pos: Pos, who: &Caller, heard: &str, parsed: Option<Parsed>, now: f64) -> Vec<Tx>;
    /// Who a transmission came from: callsign and aircraft type, for the
    /// transcript.
    fn caller(&mut self, who: &Caller, parsed: Option<&Parsed>) -> (Option<String>, Option<String>);
    fn llm_context(&mut self, pos: Pos, who: &Caller, heard: &str) -> llm::Context;
    /// Names and phrases whisper should expect on its frequencies.
    fn vocabulary(&self) -> Vec<String>;
    /// The thing it controls has gone (a tanker landed or died): retire it.
    fn gone(&self) -> bool {
        false
    }
}

// ─── aircraft ──────────────────────────────────────────────────────────────

/// An aircraft as any facility sees it.
#[derive(Debug, Clone)]
pub(crate) struct Air {
    pub id: u64,
    /// Player name, or the AI flight's DCS group.
    pub name: String,
    /// The engine's callsign for it (group name for players, "Reach 1-1" for AI).
    pub group: String,
    pub lat: f64,
    pub lon: f64,
    pub alt_ft: i32,
    pub agl_ft: i32,
    pub hdg: f64,
    pub gs: f64,
    pub vs_fpm: i32,
    pub on_ground: bool,
    pub ai: bool,
    /// For AI: what it is doing, as "range:<station>", "tanker:<id>",
    /// "carrier:<id>", "departure", "arrival".
    pub ai_plan: Option<String>,
    pub ai_state: String,
    pub typ: String,
}

pub(crate) fn traffic(atc: &AtcPicture) -> Vec<Air> {
    atc.traffic.iter().filter_map(air).collect()
}

fn air(t: &AtcTraffic) -> Option<Air> {
    Some(Air {
        id: t.unit_id?,
        name: t.player_name.clone(),
        group: t.callsign.clone(),
        lat: t.lat,
        lon: t.lon,
        alt_ft: t.alt_ft,
        agl_ft: t.agl_ft,
        hdg: t.heading as f64,
        gs: t.speed_kts as f64,
        vs_fpm: t.vspd_fpm,
        on_ground: t.on_ground,
        ai: t.ai,
        ai_plan: t.ai_plan.clone(),
        ai_state: t.ai_state.clone().unwrap_or_default(),
        typ: t.unit_type.clone(),
    })
}

/// Which aircraft a transmission came from: the SRS unit id, else the SRS
/// name against player names, else the callsign the pilot gave.
pub(crate) fn locate<'a>(air: &'a [Air], who: &Caller, parsed: Option<&Parsed>, calls: &HashMap<u64, String>) -> Option<&'a Air> {
    if let Some(a) = who.unit_id.and_then(|u| air.iter().find(|a| a.id == u)) {
        return Some(a);
    }
    let n = norm(&who.srs_name);
    if !n.is_empty() {
        if let Some(a) = air.iter().find(|a| {
            let m = norm(&a.name);
            !a.ai && !m.is_empty() && (n.contains(&m) || m.contains(&n))
        }) {
            return Some(a);
        }
    }
    let c = parsed.map(|p| norm(&p.callsign)).filter(|c| !c.is_empty())?;
    air.iter().find(|a| calls.get(&a.id).map(|k| norm(k) == c).unwrap_or_else(|| norm(&call_of(a)) == c))
}

/// How a facility addresses an aircraft before the pilot has said: the
/// callsign the engine has for it, else the player's name.
pub(crate) fn call_of(a: &Air) -> String {
    guess_callsign(&a.group).unwrap_or_else(|| if a.ai { a.group.clone() } else { a.name.clone() })
}

/// Callsigns learnt from what pilots say, by unit.
#[derive(Default)]
pub(crate) struct Calls(pub HashMap<u64, String>);

impl Calls {
    pub(crate) fn of(&self, a: &Air) -> String {
        self.0.get(&a.id).cloned().unwrap_or_else(|| call_of(a))
    }
    pub(crate) fn learn(&mut self, a: &Air, parsed: Option<&Parsed>) {
        if let Some(p) = parsed.filter(|p| !p.callsign.is_empty()) {
            self.0.insert(a.id, p.callsign.clone());
        }
    }
}

// ─── geometry ──────────────────────────────────────────────────────────────

pub(crate) const NM: f64 = 1852.;

/// Flat-earth metres about a point: (north, east).
#[derive(Debug, Clone, Copy)]
pub(crate) struct Local {
    lat0: f64,
    lon0: f64,
    k_lon: f64,
}

impl Local {
    pub(crate) fn at(lat0: f64, lon0: f64) -> Self {
        Local { lat0, lon0, k_lon: 111_320. * lat0.to_radians().cos() }
    }
    pub(crate) fn xy(&self, lat: f64, lon: f64) -> (f64, f64) {
        ((lat - self.lat0) * 111_132., (lon - self.lon0) * self.k_lon)
    }
    /// Range (metres) and true bearing (degrees) from the origin to a point.
    pub(crate) fn polar(&self, lat: f64, lon: f64) -> (f64, f64) {
        let (n, e) = self.xy(lat, lon);
        ((n * n + e * e).sqrt(), e.atan2(n).to_degrees().rem_euclid(360.))
    }
}

/// Range in nm and bearing from one point to another.
pub(crate) fn range_bearing(from_lat: f64, from_lon: f64, lat: f64, lon: f64) -> (f64, f64) {
    let (m, b) = Local::at(from_lat, from_lon).polar(lat, lon);
    (m / NM, b)
}

/// Signed smallest difference a - b, in (-180, 180].
pub(crate) fn angdiff(a: f64, b: f64) -> f64 {
    let d = (a - b + 540.).rem_euclid(360.) - 180.;
    if d == -180. {
        180.
    } else {
        d
    }
}

// ─── the mission clock ─────────────────────────────────────────────────────

/// The mission clock from the live picture's "HH:MM:SS", seconds.
pub(crate) fn mission_seconds(range: &RangeLive) -> Option<f64> {
    let mut it = range.mission_time.split(':').map(|x| x.trim().parse::<f64>());
    let (h, m) = (it.next()?.ok()?, it.next()?.ok()?);
    let s = it.next().and_then(|x| x.ok()).unwrap_or(0.);
    Some(h * 3600. + m * 60. + s)
}

/// Tracks the mission clock against the runtime clock, so a time handed out
/// now ("expected approach time 32") can be said in mission minutes.
#[derive(Default, Clone, Copy)]
pub(crate) struct Clock {
    /// mission seconds minus runtime seconds
    offset: Option<f64>,
}

impl Clock {
    pub(crate) fn sync(&mut self, pic: &Picture) {
        if let Some(m) = pic.mission_s {
            self.offset = Some(m - pic.now);
        }
    }
    /// The minute past the hour (0-59) a runtime time falls at, as said on
    /// the radio: "3-2".
    pub(crate) fn minute(&self, at: f64) -> u32 {
        let m = at + self.offset.unwrap_or(0.);
        ((m / 60.).floor() as i64).rem_euclid(60) as u32
    }
    /// Spoken: "3-2" for 32, "0-5" for 5.
    pub(crate) fn say_minute(&self, at: f64) -> String {
        let m = self.minute(at);
        format!("{}-{}", m / 10, m % 10)
    }
}

// ─── speaking ──────────────────────────────────────────────────────────────

/// The human touches every facility shares: pauses, a turn of phrase, and
/// AI pilots who read back what they are told.
pub(crate) struct Mouth {
    rng: u64,
}

impl Default for Mouth {
    fn default() -> Self {
        Mouth { rng: 0x9E37_79B9_7F4A_7C15 }
    }
}

impl Mouth {
    pub(crate) fn seeded(seed: u64) -> Self {
        Mouth { rng: seed | 1 }
    }

    pub(crate) fn rand(&mut self) -> f64 {
        let mut x = self.rng;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.rng = x;
        (x.wrapping_mul(0x2545_F491_4F6C_DD1D) >> 11) as f64 / (1u64 << 53) as f64
    }

    pub(crate) fn between(&mut self, lo: f64, hi: f64) -> f64 {
        lo + (hi - lo) * self.rand()
    }

    pub(crate) fn pick<'a>(&mut self, opts: &[&'a str]) -> &'a str {
        opts[((self.rand() * opts.len() as f64) as usize).min(opts.len() - 1)]
    }

    /// The controller (or a voice override, e.g. the LSO on the approach
    /// frequency) talks to `a`. AI flights read it back.
    pub(crate) fn say(&mut self, pos: Pos, voice: Option<Pos>, a: &Air, call: &str, field: &str, text: String, now: f64, out: &mut Vec<Tx>) {
        let mut tx = Tx::new(pos, text.clone(), Some(a.id), now + 14.);
        tx.delay = self.between(0.8, 2.2);
        tx.subtitle = (!a.ai).then_some(a.id);
        tx.aircraft = type_label(&a.typ);
        tx.voice = voice;
        out.push(tx);
        if a.ai {
            if let Some(rb) = readback(&text, call, field, self.rand()) {
                let mut r = Tx::new(pos, rb, Some(a.id), now + 30.);
                r.pilot = Some(call.to_string());
                r.aircraft = type_label(&a.typ);
                r.delay = self.between(0.7, 1.8);
                out.push(r);
            }
        }
    }

    /// The controller to everyone on frequency (no addressee).
    pub(crate) fn broadcast(&mut self, pos: Pos, voice: Option<Pos>, text: String, now: f64, out: &mut Vec<Tx>) {
        let mut tx = Tx::new(pos, text, None, now + 20.);
        tx.delay = self.between(0.5, 1.5);
        tx.voice = voice;
        out.push(tx);
    }

    /// An AI pilot keys up.
    pub(crate) fn pilot(&mut self, pos: Pos, a: &Air, call: &str, text: String, now: f64, out: &mut Vec<Tx>) {
        let mut tx = Tx::new(pos, text, Some(a.id), now + 30.);
        tx.pilot = Some(call.to_string());
        tx.aircraft = type_label(&a.typ);
        tx.delay = self.between(0.3, 1.2);
        out.push(tx);
    }

    /// Attach an AI command to the last thing said to `a`.
    pub(crate) fn command(&self, a: &Air, action: &'static str, out: &mut [Tx]) {
        if !a.ai {
            return;
        }
        if let Some(tx) = out.iter_mut().rev().find(|t| t.to == Some(a.id) && t.pilot.is_none()) {
            tx.action = Some((a.name.clone(), action));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clock_says_mission_minutes() {
        let mut c = Clock::default();
        let atc = AtcPicture::default();
        let pic = Picture { atc: &atc, range: None, now: 100., mission_s: Some(10. * 3600. + 28. * 60.) };
        c.sync(&pic);
        assert_eq!(c.minute(100.), 28);
        assert_eq!(c.say_minute(100. + 4. * 60.), "3-2");
        assert_eq!(c.say_minute(100. + 35. * 60.), "0-3");
    }

    #[test]
    fn mission_time_parses() {
        let r = RangeLive { mission_time: "08:38:47".into(), ..Default::default() };
        assert_eq!(mission_seconds(&r), Some(8. * 3600. + 38. * 60. + 47.));
    }

    #[test]
    fn range_and_bearing() {
        let (nm, brg) = range_bearing(13.5, 144.9, 13.5 + 10. / 60., 144.9);
        assert!((nm - 10.).abs() < 0.1, "{nm}");
        assert!(brg < 1. || brg > 359.);
    }
}
