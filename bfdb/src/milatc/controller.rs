// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! The controllers: one airfield's Clearance Delivery, Ground, Tower,
//! Departure, Approach and precision Final, working one shared set of flight
//! strips.
//!
//! Everything here is synchronous and clock-injected (`now` in seconds), so it
//! can be driven by tests with a synthetic traffic picture. The runtime in
//! [`super`] feeds it the engine's picture once a second ([`Controller::update`],
//! which returns what the controllers say unprompted: handoffs, landing and
//! takeoff clearances that were waiting on traffic, the PAR talkdown, ATIS)
//! and every understood transmission ([`Controller::respond`]).
//!
//! Clearances are decided from the picture, never by the language model: a
//! runway with an aircraft on it is not cleared for anyone else, an arrival
//! inside three miles holds a departure short, and so on.
//!
//! Like people, the controllers do not always answer at once. Clearance
//! Delivery puts a clearance "on request" and calls back when it has it; a
//! busy frequency gets "standby" and the answer when there is a gap. And they
//! deal with what goes wrong: an aircraft stopped on a runway is asked its
//! status, told to vacate, and the runway closed if it stays; a crash closes
//! the runway; traffic stopped on a taxiway gets the others routed around it
//! or held; taxiing or landing without a clearance is called out.

use super::{
    airport::{angdiff, seg_dist, Airport, TaxiRoute, FT, NM, P2},
    intent::{Approach, Intent, LandingOption, Parsed},
    llm,
    speech::{self, alt, freq, hdg, wind},
    Pos,
};
use bfprotocols::atc::{AtcAirfield, AtcTraffic};
use std::collections::{HashMap, HashSet};

/// How long a proactive call stays worth saying if the frequency is busy.
const STALE_S: f64 = 8.;

/// One transmission a controller wants to make.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Tx {
    pub pos: Pos,
    pub text: String,
    /// The flight it is addressed to (none for ATIS).
    pub to: Option<u64>,
    /// Mission-clock time after which it is no longer worth saying.
    pub expires: f64,
    /// An AI pilot is talking (this callsign), not the controller.
    pub pilot: Option<String>,
    /// The pause before speaking, seconds, counted from when the frequency
    /// last went quiet: nobody answers the instant a pilot unkeys.
    pub delay: f64,
    /// An AI flight command to send once this has been said: (group,
    /// "start" | "hold" | "resume").
    pub action: Option<(String, &'static str)>,
    /// Show the text in DCS to the player in this unit.
    pub subtitle: Option<u64>,
    /// The type of the aircraft it is from or to, for the transcript.
    pub aircraft: Option<String>,
    /// Speak in another position's voice on this position's frequency (the
    /// LSO on a carrier's approach frequency).
    pub voice: Option<Pos>,
}

impl Tx {
    pub(crate) fn new(pos: Pos, text: String, to: Option<u64>, expires: f64) -> Self {
        Tx {
            pos,
            text,
            to,
            expires,
            pilot: None,
            delay: 0.,
            action: None,
            subtitle: None,
            aircraft: None,
            voice: None,
        }
    }
}

/// Where a flight is in its sortie, as the controllers see it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Phase {
    /// On the ground with nothing issued.
    Ground,
    /// Cleared to taxi for departure.
    TaxiOut,
    /// At the departure hold-short point.
    HoldingShort,
    /// Lined up, waiting for a takeoff clearance.
    LinedUp,
    TakeoffCleared,
    /// Airborne after takeoff, being handed to Departure.
    Departure,
    /// Checked in with Departure.
    Climbout,
    /// Released, or airborne and never talked to us.
    Enroute,
    /// With Approach.
    Arrival,
    /// With the precision Final controller.
    RadarFinal,
    /// With Tower in the visual pattern or on a straight-in.
    Pattern,
    /// On the runway after touchdown.
    Landing,
    /// Cleared to taxi to parking.
    TaxiIn,
}

/// The latest radar return for an aircraft, in airfield coordinates.
#[derive(Debug, Clone)]
pub(crate) struct Ac {
    pub unit_id: u64,
    pub name: String,
    pub group: String,
    pub p: P2,
    pub alt_ft: i32,
    pub agl_ft: i32,
    pub gs: f64,
    pub hdg: f64,
    pub vs_fpm: i32,
    pub on_ground: bool,
    pub rotary: bool,
    /// An AI flight the range is flying for us; `name` is its group.
    pub ai: Option<AiPlan>,
    /// "waiting" / "active" / "holding", as the engine reports it.
    pub ai_state: String,
    pub typ: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AiPlan {
    Departure,
    Arrival,
}

/// The engine's plan for an AI flight as the airfield sees it: a range or
/// tanker sortie departs, and is an arrival once it is on its way home
/// ("range:<id>:home").
fn ai_plan(p: Option<&str>) -> AiPlan {
    match p {
        Some("departure") => AiPlan::Departure,
        Some(p) if p.ends_with(":home") => AiPlan::Arrival,
        Some(p) if p.starts_with("range:") || p.starts_with("tanker:") => {
            AiPlan::Departure
        }
        _ => AiPlan::Arrival,
    }
}

/// A flight strip.
#[derive(Debug, Clone)]
pub(crate) struct Flight {
    pub key: u64,
    pub call: String,
    /// The callsign was given by the pilot (not guessed from the group name).
    pub call_confirmed: bool,
    pub phase: Phase,
    pub t_phase: f64,
    /// The position the pilot was last heard on or last sent to.
    pub freq: Option<Pos>,
    pub t_handoff: f64,
    pub runway: Option<String>,
    pub route: Option<TaxiRoute>,
    /// A runway the flight has been cleared to cross.
    pub crossing_cleared: Option<String>,
    pub squawk: Option<u16>,
    pub cleared_alt: i32,
    pub approach: Approach,
    pub option: LandingOption,
    pub landing_cleared: bool,
    pub takeoff_waiting: bool,
    pub landing_waiting: bool,
    pub emergency: bool,
    /// Last transmission to the flight from each position, for "say again"
    /// and for telling readbacks from requests.
    pub last: HashMap<Pos, String>,
    /// One-shot calls already made in this phase.
    said: HashSet<&'static str>,
    pub ac: Ac,
    pub seen: f64,
    was_airborne: bool,
    // radar vectoring
    vector_hdg: Option<f64>,
    t_vector: f64,
    // PAR
    gca_started: bool,
    t_gca: f64,
    gca_mile: i32,
    // AI pilots
    /// The earliest the AI pilot speaks up again on his own.
    ai_next: f64,
    /// Ground is holding an AI departure on its stand.
    ai_taxi_hold: bool,
    /// Approach put an AI arrival in a hold, at this time.
    ai_hold_since: Option<f64>,
    ai_clear_since: Option<f64>,
    /// Clearance Delivery working on the flight's clearance.
    clr: Clr,
    clr_dest: String,
    /// Has been airborne since its strip was opened.
    flown: bool,
    /// Where it first sat on the ground, for taxiing without a clearance.
    ground_from: Option<P2>,
    /// Stopped on a runway since.
    rwy_stop: Option<f64>,
    /// Stopped on the ground since.
    stop_since: Option<f64>,
    /// Held by Ground for traffic stopped ahead (that flight).
    held_for: Option<u64>,
    /// Held by Ground to give way to moving traffic (that flight, since).
    give_way: Option<(u64, f64)>,
    /// Flights it has been told to follow.
    followed: HashSet<u64>,
    /// When it called ready for departure.
    ready_at: Option<f64>,
    /// Missed from the picture, and already looked at.
    vanished: bool,
}

/// Clearance Delivery's work on a clearance.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Clr {
    None,
    /// "Clearance on request": it is ready at this time.
    Preparing(f64),
    /// "I have your clearance, advise ready to copy."
    Ready,
}

/// A runway out of use.
#[derive(Debug, Clone)]
struct Closure {
    /// Reopens at this time (infinite: when `by` moves off it).
    until: f64,
    why: String,
    by: Option<u64>,
}

impl Flight {
    fn new(ac: Ac, now: f64) -> Self {
        let call = guess_callsign(&ac.group).unwrap_or_else(|| ac.name.clone());
        let call_confirmed = ac.ai.is_some();
        let airborne = !ac.on_ground;
        Flight {
            key: ac.unit_id,
            call,
            call_confirmed,
            phase: if airborne { Phase::Enroute } else { Phase::Ground },
            t_phase: now,
            freq: None,
            t_handoff: now,
            runway: None,
            route: None,
            crossing_cleared: None,
            squawk: None,
            cleared_alt: 0,
            approach: Approach::Unspecified,
            option: LandingOption::Unspecified,
            landing_cleared: false,
            takeoff_waiting: false,
            landing_waiting: false,
            emergency: false,
            last: HashMap::new(),
            said: HashSet::new(),
            ac,
            seen: now,
            was_airborne: airborne,
            vector_hdg: None,
            t_vector: f64::NEG_INFINITY,
            gca_started: false,
            t_gca: f64::NEG_INFINITY,
            gca_mile: i32::MAX,
            ai_next: now + 15.,
            ai_taxi_hold: false,
            ai_hold_since: None,
            ai_clear_since: None,
            clr: Clr::None,
            clr_dest: String::new(),
            flown: airborne,
            ground_from: None,
            rwy_stop: None,
            stop_since: None,
            held_for: None,
            give_way: None,
            followed: HashSet::new(),
            ready_at: None,
            vanished: false,
        }
    }

    pub(crate) fn is_ai(&self) -> bool {
        self.ac.ai.is_some()
    }

    fn set_phase(&mut self, p: Phase, now: f64) {
        if self.phase != p {
            self.phase = p;
            self.t_phase = now;
            self.said.clear();
        }
    }

    fn once(&mut self, k: &'static str) -> bool {
        self.said.insert(k)
    }

    /// A short description for the language model's context.
    fn situation(&self) -> String {
        let r = self.runway.as_deref().unwrap_or("the active");
        match self.phase {
            Phase::Ground => "on the ground, parked or stopped".into(),
            Phase::TaxiOut => format!("taxiing for departure to runway {r}"),
            Phase::HoldingShort => format!("holding short of runway {r}"),
            Phase::LinedUp => format!("lined up on runway {r}"),
            Phase::TakeoffCleared => format!("cleared for takeoff runway {r}"),
            Phase::Departure | Phase::Climbout => {
                format!("departing, climbing through {} feet", self.ac.alt_ft)
            }
            Phase::Enroute => format!("airborne at {} feet", self.ac.alt_ft),
            Phase::Arrival => {
                format!("inbound under approach control at {} feet", self.ac.alt_ft)
            }
            Phase::RadarFinal => "on a precision radar final".into(),
            Phase::Pattern => format!("in the tower's pattern for runway {r}"),
            Phase::Landing => format!("on the runway after landing on {r}"),
            Phase::TaxiIn => "taxiing to parking".into(),
        }
    }
}

/// "Viper 1-1" from a DCS group name like "Viper 1-1", "Viper11", "VIPER 1".
pub(crate) fn guess_callsign(group: &str) -> Option<String> {
    let g = group.trim();
    let word: String = g.chars().take_while(|c| c.is_alphabetic()).collect();
    let rest: String = g[word.len()..].chars().filter(|c| c.is_ascii_digit()).collect();
    if word.len() >= 3
        && (1..=3).contains(&rest.len())
        && g[word.len()..]
            .chars()
            .all(|c| c.is_ascii_digit() || c == '-' || c == ' ' || c == '#')
    {
        Some(speech::format_callsign(&word, &rest))
    } else {
        None
    }
}

/// Settings the controllers need beyond the airfield file.
#[derive(Debug, Clone)]
pub(crate) struct CtlCfg {
    /// Ground refuses taxi until Clearance Delivery has issued a clearance.
    pub require_clearance: bool,
    /// Who Departure hands released traffic to ("Overlord", 251.5).
    pub handoff_out: Option<(String, f64)>,
    pub gca_interval_s: f64,
    pub atis_interval_s: f64,
}

impl Default for CtlCfg {
    fn default() -> Self {
        CtlCfg {
            require_clearance: false,
            handoff_out: None,
            gca_interval_s: 5.,
            atis_interval_s: 60.,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct Weather {
    wind_from: u16,
    wind_kts: u16,
    qnh_inhg: f64,
    temp_c: i16,
    vis_m: u32,
    cloud_base_ft: Option<i32>,
    cloud_cover: Option<String>,
}

#[derive(Debug, Clone, Default)]
struct Atis {
    letter: u8,
    issued: Option<(String, u16, u16, i32)>,
    last: f64,
}

pub(crate) struct Controller {
    pub ap: Airport,
    cfg: CtlCfg,
    pub flights: HashMap<u64, Flight>,
    traffic: Vec<Ac>,
    /// The direction of operation, "06" or "24".
    pub direction: String,
    wx: Weather,
    atis: Atis,
    next_squawk: u16,
    pub have_picture: bool,
    /// For the human touches: pauses and the odd turn of phrase.
    rng: std::cell::Cell<u64>,
    /// When each position last had something queued: AI pilots wait for a
    /// quiet frequency before they call.
    last_said: std::cell::RefCell<HashMap<Pos, f64>>,
    /// Until when each frequency is busy with what is queued on it, roughly,
    /// and who the last of it is to.
    air_until: std::cell::RefCell<HashMap<Pos, (f64, Option<u64>)>>,
    /// Answers held back after a "standby": due, position, flight, what was
    /// heard and what it meant.
    deferred: Vec<(f64, Pos, u64, String, Parsed)>,
    replaying: bool,
    /// The chance of a "standby" for a check-in or a taxi request on a quiet
    /// frequency (a busy one always gets one).
    pub standby_odds: f64,
    /// Runways out of use, by pair ("06R/24L").
    closed: HashMap<String, Closure>,
    /// The last heavy to take off: runway, when, its type.
    heavy_off: std::cell::RefCell<Option<(String, f64, String)>>,
    /// Radar traffic calls made: (to, about) → when.
    advised: HashMap<(u64, u64), f64>,
}

/// Which flight a transmission came from.
pub(crate) struct Caller {
    pub unit_id: Option<u64>,
    /// The SRS client name (usually the player's name).
    pub srs_name: String,
}

impl Controller {
    pub(crate) fn new(ap: Airport, cfg: CtlCfg) -> Self {
        let sq = ap.departure.squawk_base;
        Controller {
            ap,
            cfg,
            flights: HashMap::new(),
            traffic: vec![],
            direction: String::new(),
            wx: Weather::default(),
            atis: Atis::default(),
            next_squawk: sq,
            have_picture: false,
            rng: std::cell::Cell::new(0x2545_F491_4F6C_DD1D),
            last_said: Default::default(),
            air_until: Default::default(),
            deferred: vec![],
            replaying: false,
            standby_odds: 0.12,
            closed: HashMap::new(),
            heavy_off: Default::default(),
            advised: HashMap::new(),
        }
    }

    /// Seed the pauses and phrasing (tests use a fixed seed).
    #[cfg(test)]
    pub(crate) fn seed(&self, s: u64) {
        self.rng.set(s | 1);
    }

    /// 0..1.
    fn rand(&self) -> f64 {
        let mut x = self.rng.get();
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.rng.set(x);
        (x.wrapping_mul(0x2545_F491_4F6C_DD1D) >> 11) as f64 / (1u64 << 53) as f64
    }

    fn between(&self, lo: f64, hi: f64) -> f64 {
        lo + (hi - lo) * self.rand()
    }

    /// One of several ways of saying the same thing.
    fn pick<'a>(&self, opts: &[&'a str]) -> &'a str {
        opts[((self.rand() * opts.len() as f64) as usize).min(opts.len() - 1)]
    }

    fn quiet(&self, pos: Pos, now: f64, secs: f64) -> bool {
        self.last_said.borrow().get(&pos).is_none_or(|t| now - t >= secs)
    }

    // ─── names and numbers ────────────────────────────────────────────────

    pub(crate) fn facility(&self, pos: Pos) -> String {
        format!("{} {}", self.ap.callsign, pos.title())
    }

    /// The written frequency of a position: "340.2".
    pub(crate) fn freq_of(&self, pos: Pos) -> Option<String> {
        self.ap.positions.get(&pos).and_then(|v| v.first()).map(|f| freq(f.mhz))
    }

    fn has(&self, pos: Pos) -> bool {
        self.ap.positions.get(&pos).is_some_and(|v| !v.is_empty())
    }

    fn contact(&self, pos: Pos) -> String {
        match self.freq_of(pos) {
            Some(f) => format!("contact {} {f}", self.facility(pos)),
            None => format!("contact {}", self.facility(pos)),
        }
    }

    fn wind(&self) -> String {
        wind(self.wx.wind_from, self.wx.wind_kts)
    }

    fn altimeter(&self) -> String {
        format!("altimeter {}", speech::altimeter(self.wx.qnh_inhg))
    }

    pub(crate) fn dep_runway(&self) -> String {
        self.in_use(true)
    }

    pub(crate) fn arr_runway(&self) -> String {
        self.in_use(false)
    }

    /// The runway in use, or a parallel one while it is closed.
    fn in_use(&self, departure: bool) -> String {
        let Some(r) = self.ap.runway_for(&self.direction, departure) else {
            return String::new();
        };
        if !self.closed.contains_key(&r.pair) {
            return r.name.clone();
        }
        self.ap
            .runways
            .iter()
            .find(|o| {
                o.direction() == self.direction && !self.closed.contains_key(&o.pair)
            })
            .unwrap_or(r)
            .name
            .clone()
    }

    fn is_closed(&self, pair: &str) -> Option<&str> {
        self.closed.get(pair).map(|c| c.why.as_str())
    }

    fn take_squawk(&mut self) -> u16 {
        loop {
            let s = self.next_squawk;
            self.next_squawk =
                if s >= 4277 { self.ap.departure.squawk_base } else { s + 1 };
            if s.to_string().chars().all(|c| ('0'..='7').contains(&c)) {
                return s;
            }
        }
    }

    // ─── the picture ──────────────────────────────────────────────────────

    /// Feed the latest picture. Returns what the controllers say unprompted.
    pub(crate) fn update(
        &mut self,
        field: &AtcAirfield,
        traffic: &[AtcTraffic],
        now: f64,
    ) -> Vec<Tx> {
        self.ap.elev_ft = field.elev_ft;
        self.wx = Weather {
            wind_from: field.wind_from_deg,
            wind_kts: field.wind_speed_kts,
            qnh_inhg: field.qnh_inhg,
            temp_c: field.temp_c,
            vis_m: field.visibility_m,
            cloud_base_ft: field.cloud_base_ft,
            cloud_cover: field.cloud_cover.clone(),
        };
        let prefer = (!self.direction.is_empty()).then(|| self.direction.clone());
        let dir = self.ap.direction_for_wind(
            field.wind_from_deg as f64,
            field.wind_speed_kts as f64,
            prefer.as_deref(),
        );
        if dir != self.direction {
            if !self.direction.is_empty() {
                log::info!(
                    "milatc {}: runway in use changes to {}",
                    self.ap.callsign,
                    dir
                );
            }
            self.direction = dir;
        }
        self.have_picture = true;
        self.traffic = traffic
            .iter()
            .filter(|t| {
                !(t.ai && t.ai_plan.as_deref().is_some_and(|p| p.starts_with("carrier:")))
            })
            .filter_map(|t| {
                Some(Ac {
                    unit_id: t.unit_id?,
                    name: t.player_name.clone(),
                    group: t.callsign.clone(),
                    p: self.ap.local(t.lat, t.lon),
                    alt_ft: t.alt_ft,
                    agl_ft: t.agl_ft,
                    gs: t.speed_kts as f64,
                    hdg: t.heading as f64,
                    vs_fpm: t.vspd_fpm,
                    on_ground: t.on_ground,
                    rotary: t.rotary,
                    ai: t.ai.then(|| ai_plan(t.ai_plan.as_deref())),
                    ai_state: t.ai_state.clone().unwrap_or_default(),
                    typ: t.unit_type.clone(),
                })
            })
            .collect();
        for ac in self.traffic.clone() {
            match self.flights.get_mut(&ac.unit_id) {
                Some(f) => {
                    f.ac = ac;
                    f.seen = now;
                    f.vanished = false;
                }
                None => {
                    // Only aircraft near the field get a strip.
                    if ac.p.dist(P2::default()) < 80. * NM {
                        self.flights.insert(ac.unit_id, Flight::new(ac, now));
                    }
                }
            }
        }
        let mut out = vec![];
        self.vanished(now, &mut out);
        self.flights.retain(|_, f| now - f.seen < 30.);

        let keys: Vec<u64> = self.flights.keys().copied().collect();
        for k in keys {
            let Some(mut f) = self.flights.remove(&k) else { continue };
            self.tick_flight(&mut f, now, &mut out);
            self.flights.insert(k, f);
        }
        self.runway_closures(now, &mut out);
        self.taxiway_blocks(now, &mut out);
        self.ground_traffic(now, &mut out);
        self.traffic_advisories(now, &mut out);
        self.release_deferred(now, &mut out);
        if let Some(a) = self.atis_due(now) {
            out.push(a);
        }
        self.with_readbacks(out, now)
    }

    /// Put an AI pilot's readback after every controller transmission to an
    /// AI flight.
    pub(crate) fn with_readbacks(&self, out: Vec<Tx>, now: f64) -> Vec<Tx> {
        let mut v = Vec::with_capacity(out.len());
        for tx in out {
            let rb = match (tx.pilot.is_none(), tx.to.and_then(|k| self.flights.get(&k)))
            {
                (true, Some(f)) if f.is_ai() => {
                    readback(&tx.text, &f.call, &self.ap.callsign, self.rand())
                }
                _ => None,
            };
            let (pos, to) = (tx.pos, tx.to);
            v.push(tx);
            if let Some(text) = rb {
                let mut r = Tx::new(pos, text, to, now + 30.);
                let f = to.and_then(|k| self.flights.get(&k));
                r.pilot = f.map(|f| f.call.clone());
                r.aircraft = f.and_then(|f| type_label(&f.ac.typ));
                r.delay = self.between(0.7, 1.8);
                v.push(r);
            }
        }
        v
    }

    /// Something else on (or just above) a runway strip: rolling, stopped,
    /// lined up, or in the flare.
    fn runway_occupant(&self, pair: &str, except: u64) -> Option<&Ac> {
        self.traffic.iter().find(|a| {
            a.unit_id != except
                && (a.on_ground || a.agl_ft < 100)
                && (self.ap.strip_at(a.p, 0.) == Some(pair))
        })
    }

    /// The nearest airborne arrival to `runway` within `nm` of its threshold,
    /// and its distance.
    fn arrival_within(&self, runway: &str, nm: f64, except: u64) -> Option<(&Ac, f64)> {
        let r = self.ap.runway(runway)?;
        self.traffic
            .iter()
            .filter(|a| a.unit_id != except && !a.on_ground && a.agl_ft < 2500)
            .filter_map(|a| {
                let (along, cross) = r.frame(a.p);
                let d = -along / NM;
                (d > -0.3
                    && d < nm
                    && cross.abs() < 0.12 * (-along).max(800.) + 300.
                    && angdiff(a.hdg, r.hdg).abs() < 45.)
                    .then_some((a, d.max(0.)))
            })
            .min_by(|a, b| a.1.total_cmp(&b.1))
    }

    /// A departure just airborne off `runway`, still close.
    fn departure_ahead(&self, runway: &str, except: u64) -> Option<&Ac> {
        let r = self.ap.runway(runway)?;
        self.traffic.iter().find(|a| {
            if a.unit_id == except || angdiff(a.hdg, r.hdg).abs() > 45. {
                return false;
            }
            let (along, cross) = r.frame(a.p);
            let rolling = a.on_ground && a.gs > 40. && cross.abs() < r.width;
            let climbing = !a.on_ground
                && along > 0.
                && along < r.len + 2. * NM
                && a.agl_ft < 2000
                && cross.abs() < NM;
            rolling || climbing
        })
    }

    /// The arrival just ahead of `f` to the runway: its type and miles out.
    fn landing_ahead(&self, f: &Flight) -> Option<(&'static str, i32)> {
        let r = self.ap.runway(&self.arr_runway())?;
        let mine = f.ac.p.dist(r.thr);
        self.flights
            .values()
            .filter(|o| {
                o.key != f.key
                    && !o.ac.on_ground
                    && matches!(o.phase, Phase::Pattern | Phase::RadarFinal)
            })
            .map(|o| (o, o.ac.p.dist(r.thr)))
            .filter(|(_, d)| *d < mine)
            .max_by(|a, b| a.1.total_cmp(&b.1))
            .map(|(o, d)| {
                (
                    type_name(&o.ac.typ).unwrap_or("traffic"),
                    ((d / NM).round() as i32).max(1),
                )
            })
    }

    /// The landing sequence number of a flight: 1 + arrivals nearer the
    /// threshold than it.
    fn sequence(&self, f: &Flight) -> usize {
        let Some(r) = self.ap.runway(&self.arr_runway()).cloned() else { return 1 };
        let mine = f.ac.p.dist(r.thr);
        1 + self
            .flights
            .values()
            .filter(|o| o.key != f.key && !o.ac.on_ground)
            .filter(|o| matches!(o.phase, Phase::Pattern | Phase::RadarFinal))
            .filter(|o| o.ac.p.dist(r.thr) < mine)
            .count()
    }

    // ─── ATIS ─────────────────────────────────────────────────────────────

    fn atis_due(&mut self, now: f64) -> Option<Tx> {
        if !self.has(Pos::Atis) || !self.have_picture {
            return None;
        }
        let mut closed: Vec<&String> = self.closed.keys().collect();
        closed.sort();
        let key = (
            format!("{} {closed:?}", self.dep_runway()),
            (self.wx.wind_from / 30) * 30,
            (self.wx.wind_kts / 5) * 5,
            (self.wx.qnh_inhg * 100.).round() as i32,
        );
        let changed = match &self.atis.issued {
            None => true,
            Some(k) => {
                k.0 != key.0
                    || angdiff(k.1 as f64, key.1 as f64).abs() >= 30.
                        && self.wx.wind_kts >= 5
                    || (k.2 as i32 - key.2 as i32).abs() >= 10
                    || k.3 != key.3
            }
        };
        if changed {
            if self.atis.issued.is_some() {
                self.atis.letter = (self.atis.letter + 1) % 26;
            }
            self.atis.issued = Some(key);
            self.atis.last = f64::NEG_INFINITY;
        }
        if now - self.atis.last < self.cfg.atis_interval_s {
            return None;
        }
        self.atis.last = now;
        Some(Tx::new(Pos::Atis, self.atis_text(), None, now + 20.))
    }

    pub(crate) fn atis_letter(&self) -> &'static str {
        speech::letter(self.atis.letter)
    }

    fn atis_text(&self) -> String {
        let w = &self.wx;
        let mut s = format!(
            "{} information {}. {}. ",
            self.ap.callsign,
            self.atis_letter(),
            capitalize(&self.wind())
        );
        if w.vis_m >= 9000 || w.vis_m == 0 {
            s.push_str("Visibility 10 kilometres or more. ");
        } else {
            s.push_str(&format!("Visibility {} metres. ", (w.vis_m / 100) * 100));
        }
        match (w.cloud_base_ft, w.cloud_cover.as_deref()) {
            (Some(b), Some(c)) if !c.is_empty() => s.push_str(&format!(
                "{} {}. ",
                capitalize(c),
                alt(((b + 50) / 100) * 100)
            )),
            (Some(b), _) => {
                s.push_str(&format!("Ceiling {}. ", alt(((b + 50) / 100) * 100)))
            }
            _ => s.push_str("Sky clear. "),
        }
        s.push_str(&format!("Temperature {}. ", w.temp_c));
        s.push_str(&format!("{}. ", capitalize(&self.altimeter())));
        let dep = self.dep_runway();
        let arr = self.arr_runway();
        if dep == arr {
            s.push_str(&format!("Runway {dep} in use. "));
        } else {
            s.push_str(&format!("Departures runway {dep}, arrivals runway {arr}. "));
        }
        let mut closed: Vec<(&String, &Closure)> = self.closed.iter().collect();
        closed.sort_by(|a, b| a.0.cmp(b.0));
        for (pair, c) in closed {
            s.push_str(&format!(
                "Runway {} closed, {}. ",
                self.ap.strip_name(pair, &self.direction),
                c.why
            ));
        }
        s.push_str(&format!(
            "Overhead pattern altitude {}. Precision approaches available on request. ",
            alt(self.ap.arrival.pattern_alt_ft)
        ));
        s.push_str(&format!(
            "Advise on initial contact you have information {}.",
            self.atis_letter()
        ));
        s
    }

    // ─── proactive control ────────────────────────────────────────────────

    /// Keep the estimate of how long a frequency is busy with what is queued.
    fn on_air(&self, pos: Pos, text: &str, delay: f64, to: Option<u64>, now: f64) {
        let words = text.split_whitespace().count() as f64;
        let mut m = self.air_until.borrow_mut();
        let start = m.get(&pos).map_or(now, |e| e.0).max(now) + delay;
        m.insert(pos, (start + words / 2.6 + 0.5, to));
    }

    /// Seconds of speech still queued on a frequency, roughly. A pilot
    /// calling straight after being spoken to has heard it out (or stepped
    /// on it): that does not count as busy for them.
    fn busy_for(&self, pos: Pos, now: f64, caller: Option<u64>) -> f64 {
        match self.air_until.borrow().get(&pos) {
            Some((t, to)) if caller.is_none() || *to != caller => (t - now).max(0.),
            _ => 0.,
        }
    }

    fn say(&self, f: &mut Flight, pos: Pos, text: String, now: f64, out: &mut Vec<Tx>) {
        f.last.insert(pos, text.clone());
        self.last_said.borrow_mut().insert(pos, now);
        let mut tx = Tx::new(pos, text, Some(f.key), now + STALE_S + 6.);
        // A person takes a moment: to find the strip, look out of the window,
        // key the mic.
        tx.delay = self.between(0.8, 2.4);
        self.on_air(pos, &tx.text, tx.delay, Some(f.key), now);
        tx.subtitle = (!f.is_ai()).then_some(f.key);
        tx.aircraft = type_label(&f.ac.typ);
        out.push(tx);
    }

    /// An AI pilot keys up on `pos`.
    fn pilot_says(
        &self,
        f: &mut Flight,
        pos: Pos,
        text: String,
        now: f64,
        out: &mut Vec<Tx>,
    ) {
        self.last_said.borrow_mut().insert(pos, now);
        let mut tx = Tx::new(pos, text, Some(f.key), now + 30.);
        tx.pilot = Some(f.call.clone());
        tx.delay = self.between(0.3, 1.2);
        self.on_air(pos, &tx.text, tx.delay, Some(f.key), now);
        tx.aircraft = type_label(&f.ac.typ);
        out.push(tx);
        f.freq = Some(pos);
        f.ai_next = now + self.between(25., 45.);
    }

    /// To everyone on a frequency ("All aircraft, Andersen Tower, ...").
    fn broadcast(&self, pos: Pos, text: String, now: f64, out: &mut Vec<Tx>) {
        if !self.has(pos) {
            return;
        }
        self.last_said.borrow_mut().insert(pos, now);
        let mut tx = Tx::new(pos, text, None, now + 30.);
        tx.delay = self.between(1.0, 2.5);
        self.on_air(pos, &tx.text, tx.delay, None, now);
        out.push(tx);
    }

    /// Attach an AI command to the last thing said to `f`.
    fn command(&self, f: &Flight, action: &'static str, out: &mut [Tx]) {
        if let Some(tx) =
            out.iter_mut().rev().find(|t| t.to == Some(f.key) && t.pilot.is_none())
        {
            tx.action = Some((f.ac.name.clone(), action));
        }
    }

    /// Hand a flight to another position.
    fn handoff(&self, f: &mut Flight, from: Pos, to: Pos, now: f64, out: &mut Vec<Tx>) {
        let text = format!("{}, {}.", f.call, self.contact(to));
        self.say(f, from, text, now, out);
        f.freq = Some(to);
        f.t_handoff = now;
    }

    /// Who speaks to this flight unprompted: the position it is on, else the
    /// position that owns its phase.
    fn speaker(&self, f: &Flight, owner: Pos) -> Pos {
        match f.freq {
            Some(p) if p != Pos::Atis => p,
            _ => owner,
        }
    }

    fn tick_flight(&mut self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        let airborne = !f.ac.on_ground;
        let took_off = airborne && !f.was_airborne;
        let touched_down = !airborne && f.was_airborne;
        f.was_airborne = airborne;
        f.flown |= airborne;
        if f.ac.on_ground && f.ac.gs < 2. {
            f.stop_since.get_or_insert(now);
        } else {
            f.stop_since = None;
        }

        // Clearance Delivery has the clearance ready.
        if let Clr::Preparing(due) = f.clr {
            if now >= due && self.quiet(Pos::Clearance, now, 4.) {
                f.clr = Clr::Ready;
                let t = format!(
                    "{}, {}, {}",
                    f.call,
                    self.facility(Pos::Clearance),
                    self.pick(&[
                        "I have your clearance, advise ready to copy.",
                        "clearance is ready, advise ready to copy.",
                        "your clearance is ready, advise when ready to copy."
                    ])
                );
                self.say(f, Pos::Clearance, t, now, out);
            }
        }

        // Off a runway: any crossing clearance has been used.
        if let Some(c) = f.crossing_cleared.clone() {
            if self.ap.strip_at(f.ac.p, 20.).is_none_or(|s| s != c)
                && f.t_phase + 5. < now
            {
                f.crossing_cleared = None;
            }
        }

        if touched_down {
            self.touchdown(f, now, out);
        }
        if took_off {
            self.liftoff(f, now, out);
        }

        if f.is_ai() {
            self.drive_ai(f, now, out);
        }

        match f.phase {
            Phase::Ground | Phase::TaxiOut | Phase::TaxiIn | Phase::HoldingShort => {
                if !f.is_ai() {
                    self.incursion_check(f, now, out);
                    self.taxi_without_clearance(f, now, out);
                }
                match f.phase {
                    Phase::TaxiOut => self.taxi_out(f, now, out),
                    Phase::TaxiIn => self.taxi_in(f, now),
                    Phase::HoldingShort => self.waiting_takeoff(f, now, out),
                    _ => {}
                }
            }
            Phase::LinedUp => self.waiting_takeoff(f, now, out),
            Phase::TakeoffCleared => {
                if airborne && f.ac.agl_ft >= self.ap.departure.handoff_agl_ft {
                    if f.option == LandingOption::TouchAndGo
                        || matches!(f.approach, Approach::Overhead)
                            && f.option != LandingOption::FullStop
                            && f.option != LandingOption::Unspecified
                    {
                        f.set_phase(Phase::Pattern, now);
                    } else if self.has(Pos::Departure) {
                        let from = self.speaker(f, Pos::Tower);
                        self.handoff(f, from, Pos::Departure, now, out);
                        f.set_phase(Phase::Departure, now);
                    } else {
                        f.set_phase(Phase::Enroute, now);
                    }
                }
            }
            Phase::Departure | Phase::Climbout => self.departing(f, now, out),
            Phase::Arrival => self.arriving(f, now, out),
            Phase::RadarFinal => self.par(f, now, out),
            Phase::Pattern => self.pattern(f, now, out),
            Phase::Landing => self.rollout(f, now, out),
            Phase::Enroute => {}
        }
    }

    fn liftoff(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        if heavy(&f.ac.typ) {
            let rwy = f.runway.clone().unwrap_or_else(|| self.dep_runway());
            *self.heavy_off.borrow_mut() = Some((rwy, now, f.ac.typ.clone()));
        }
        match f.phase {
            Phase::TakeoffCleared | Phase::Pattern => {}
            Phase::Landing => {
                // a touch-and-go
                f.landing_cleared = false;
                f.set_phase(Phase::Pattern, now);
                if f.freq == Some(Pos::Tower) {
                    let t = format!("{}, report base.", f.call);
                    self.say(f, Pos::Tower, t, now, out);
                }
            }
            Phase::Ground | Phase::TaxiOut | Phase::HoldingShort | Phase::LinedUp
                if f.is_ai() =>
            {
                // AI does not wait for its takeoff clearance: just hand it on.
                f.set_phase(Phase::TakeoffCleared, now);
            }
            Phase::Ground | Phase::TaxiOut | Phase::HoldingShort | Phase::LinedUp => {
                // Off without a clearance. Note it and pass them on.
                if self.ap.strip_at(f.ac.p, 400.).is_some() && f.once("no-clearance") {
                    let t = format!(
                        "{}, {}, no takeoff clearance was issued. {}.",
                        f.call,
                        self.facility(Pos::Tower),
                        capitalize(&self.contact(Pos::Departure))
                    );
                    let sp = self.speaker(f, Pos::Tower);
                    self.say(f, sp, t, now, out);
                    f.freq = Some(Pos::Departure);
                    f.set_phase(Phase::Departure, now);
                } else {
                    f.set_phase(Phase::Enroute, now);
                }
            }
            _ => {}
        }
    }

    fn touchdown(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        let Some(end) = self.ap.on_runway(f.ac.p, 15.) else {
            if f.ac.rotary {
                f.set_phase(Phase::Ground, now);
            }
            return;
        };
        f.runway = Some(end.name.clone());
        if matches!(f.option, LandingOption::TouchAndGo | LandingOption::LowApproach) {
            f.set_phase(Phase::Landing, now); // liftoff() turns it back into Pattern
            return;
        }
        let deviation = !f.is_ai()
            && !f.landing_cleared
            && !f.emergency
            && !f.last.is_empty()
            && self.has(Pos::Tower)
            && matches!(
                f.phase,
                Phase::Pattern | Phase::Arrival | Phase::RadarFinal | Phase::Enroute
            );
        f.set_phase(Phase::Landing, now);
        if deviation && f.once("landed-no-clr") {
            let t = format!(
                "{}, {}, no landing clearance was issued. Possible pilot deviation, {}.",
                f.call,
                self.facility(Pos::Tower),
                self.pick(&[
                    "advise you call the tower supervisor after you park",
                    "expect a call from the supervisor on the ground",
                    "we'll talk about it on the ground"
                ])
            );
            self.say(f, Pos::Tower, t, now, out);
            log::warn!(
                "milatc {}: {} landed without a clearance",
                self.ap.callsign,
                f.call
            );
        }
    }

    fn rollout(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        let on_rwy = self.ap.strip_at(f.ac.p, 0.).is_some();
        if on_rwy
            && f.ac.gs < 60.
            && f.ac.gs > 3.
            && f.once("exit")
            && f.option != LandingOption::TouchAndGo
        {
            // Which way are the ramps?
            let side = self
                .ap
                .runway(f.runway.as_deref().unwrap_or(""))
                .zip(self.ap.ramp_for(f.ac.p, None))
                .map(|(r, ramp)| if r.frame(ramp.p).1 >= 0. { "right" } else { "left" })
                .unwrap_or("left");
            let sp = self.speaker(f, Pos::Tower);
            let t = if self.has(Pos::Ground) {
                format!(
                    "{}, turn {side} next taxiway, {} when clear.",
                    f.call,
                    self.contact(Pos::Ground)
                )
            } else {
                format!("{}, turn {side} next taxiway, taxi to parking.", f.call)
            };
            self.say(f, sp, t, now, out);
            f.freq = Some(Pos::Ground);
        }
        if !on_rwy && f.ac.gs < 30. {
            f.set_phase(Phase::Ground, now);
            f.landing_cleared = false;
            f.approach = Approach::Unspecified;
            f.option = LandingOption::Unspecified;
        }
    }

    fn incursion_check(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        let Some(pair) = self.ap.strip_at(f.ac.p, 0.) else { return };
        if f.crossing_cleared.as_deref() == Some(pair) || f.ac.rotary {
            return;
        }
        if f.once("incursion") {
            let name = self.ap.strip_name(pair, &self.direction).to_string();
            let sp = self.speaker(f, Pos::Ground);
            let t = format!(
                "{}, {}, stop! You are on runway {name} without a clearance. Hold position.",
                f.call,
                self.facility(sp)
            );
            self.say(f, sp, t, now, out);
            log::warn!(
                "milatc {}: runway incursion by {} on {name}",
                self.ap.callsign,
                f.call
            );
        }
    }

    fn taxi_out(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        let Some(route) = f.route.clone() else { return };
        let stopped = f.ac.gs < 4.;
        // A runway to cross on the way.
        if let Some(pair) = route.crossing.clone() {
            if let Some(hi) = self.ap.crossing_hold(&route, &pair) {
                let at = self.ap.node(self.ap.holds[hi].node).dist(f.ac.p) < 45.;
                if at && stopped {
                    self.offer_crossing(f, &pair, now, out);
                    return;
                }
            }
        }
        // Arrived at the departure hold.
        if let Some(hi) = route.hold {
            let d = self.ap.node(self.ap.holds[hi].node).dist(f.ac.p);
            if d < 45. && stopped {
                f.set_phase(Phase::HoldingShort, now);
                if f.freq == Some(Pos::Ground) && self.has(Pos::Tower) {
                    self.handoff(f, Pos::Ground, Pos::Tower, now, out);
                }
                return;
            }
        }
        // Wandering off the route.
        if f.ac.gs > 3. && self.ap.off_route(&route, f.ac.p) > 120. && f.once("off-route")
        {
            let via = route
                .via
                .iter()
                .map(|v| speech::taxiway(v))
                .collect::<Vec<_>>()
                .join(", ");
            let t = format!("{}, verify taxiing via {via}.", f.call);
            let sp = self.speaker(f, Pos::Ground);
            self.say(f, sp, t, now, out);
        }
    }

    fn offer_crossing(&self, f: &mut Flight, pair: &str, now: f64, out: &mut Vec<Tx>) {
        let name = self.ap.strip_name(pair, &self.direction).to_string();
        let blocked = self.runway_occupant(pair, f.key).is_some()
            || pair.split('/').any(|e| self.arrival_within(e, 2.5, f.key).is_some());
        let sp = self.speaker(f, Pos::Ground);
        if blocked {
            if f.once("cross-hold") {
                let t =
                    format!("{}, continue holding short runway {name}, traffic.", f.call);
                self.say(f, sp, t, now, out);
            }
            return;
        }
        let rest = f.route.as_ref().map(|r| r.via.clone()).unwrap_or_default();
        let dep = f.runway.clone().unwrap_or_else(|| self.dep_runway());
        let via = rest.iter().map(|v| speech::taxiway(v)).collect::<Vec<_>>().join(", ");
        let t = if via.is_empty() {
            format!("{}, cross runway {name}, hold short runway {dep}.", f.call)
        } else {
            format!(
                "{}, cross runway {name}, continue via {via}, hold short runway {dep}.",
                f.call
            )
        };
        self.say(f, sp, t, now, out);
        f.crossing_cleared = Some(pair.to_string());
        if let Some(r) = f.route.as_mut() {
            r.crossing = None;
        }
    }

    fn taxi_in(&self, f: &mut Flight, now: f64) {
        if let Some(r) = &f.route {
            if r.end.dist(f.ac.p) < 80. && f.ac.gs < 2. && now - f.t_phase > 10. {
                f.set_phase(Phase::Ground, now);
                f.route = None;
            }
        }
    }

    /// Why a departure cannot go yet, if it cannot.
    fn takeoff_blocked(&self, f: &Flight, runway: &str) -> Option<String> {
        let r = self.ap.runway(runway)?;
        if let Some(why) = self.is_closed(&r.pair) {
            return Some(format!("runway closed, {why}"));
        }
        if let Some((a, d)) = self.arrival_within(runway, 3., f.key) {
            return Some(format!(
                "traffic{} {} mile final",
                type_phrase(&a.typ),
                d.ceil().max(1.) as i32
            ));
        }
        if let Some(opp) = self.ap.opposite(r) {
            if self.arrival_within(&opp.name, 3., f.key).is_some() {
                return Some("opposite direction traffic".into());
            }
        }
        if let Some(o) = self.runway_occupant(&r.pair, f.key) {
            if !(o.on_ground && o.gs > 40.) {
                return Some("traffic on the runway".into());
            }
            return Some("departing traffic".into());
        }
        if self.departure_ahead(runway, f.key).is_some() {
            return Some("departing traffic".into());
        }
        // cleared ahead of us, not rolling yet
        if self.flights.values().any(|o| {
            o.key != f.key
                && o.ac.on_ground
                && matches!(o.phase, Phase::TakeoffCleared | Phase::LinedUp)
                && o.runway
                    .as_deref()
                    .and_then(|x| self.ap.runway(x))
                    .is_some_and(|x| x.pair == r.pair)
        }) {
            return Some("departing traffic".into());
        }
        None
    }

    /// "caution wake turbulence, departing KC-135, " after a heavy has just
    /// gone off the same runway.
    fn wake_departing(&self, f: &Flight, runway: &str, now: f64) -> String {
        match &*self.heavy_off.borrow() {
            Some((r, t, typ)) if r == runway && now - t < 150. && !heavy(&f.ac.typ) => {
                format!(
                    "caution wake turbulence, departing {}, ",
                    type_name(typ).unwrap_or("heavy")
                )
            }
            _ => String::new(),
        }
    }

    fn takeoff_clearance(&self, f: &Flight, runway: &str, now: f64) -> String {
        let wake = self.wake_departing(f, runway, now);
        let mut t = format!(
            "{}, {wake}{}, runway {runway}, cleared for takeoff.",
            f.call,
            self.wind()
        );
        if f.squawk.is_none() && self.has(Pos::Departure) {
            t = format!(
                "{}, {wake}{}, runway {runway}, cleared for takeoff, fly runway heading, climb and maintain {}.",
                f.call,
                self.wind(),
                alt(self.ap.departure.initial_alt_ft)
            );
        }
        t
    }

    /// Flights waiting to go off `runway` ahead of `f` (in the order they
    /// called ready).
    fn departures_ahead(&self, f: &Flight, runway: &str) -> Vec<&Flight> {
        let mine = f.ready_at.unwrap_or(f64::INFINITY);
        let mut v: Vec<&Flight> = self
            .flights
            .values()
            .filter(|o| {
                o.key != f.key && o.takeoff_waiting && o.runway.as_deref() == Some(runway)
            })
            .filter(|o| o.ready_at.is_some_and(|t| t < mine))
            .collect();
        v.sort_by(|a, b| a.ready_at.unwrap_or(0.).total_cmp(&b.ready_at.unwrap_or(0.)));
        v
    }

    fn waiting_takeoff(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        if !f.takeoff_waiting || f.freq != Some(Pos::Tower) {
            return;
        }
        let runway = f.runway.clone().unwrap_or_else(|| self.dep_runway());
        if !self.departures_ahead(f, &runway).is_empty() {
            return;
        }
        if self.takeoff_blocked(f, &runway).is_none() {
            let t = self.takeoff_clearance(f, &runway, now);
            self.say(f, Pos::Tower, t, now, out);
            f.takeoff_waiting = false;
            f.ready_at = None;
            f.set_phase(Phase::TakeoffCleared, now);
        }
    }

    fn departing(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        let d_nm = f.ac.p.dist(P2::default()) / NM;
        let release = d_nm > self.ap.departure.release_nm
            || f.ac.alt_ft > self.ap.departure.release_alt_ft;
        if f.phase == Phase::Departure {
            // Never checked in: let it go quietly.
            if release || now - f.t_phase > 300. {
                f.set_phase(Phase::Enroute, now);
            }
            return;
        }
        if f.cleared_alt > 0
            && f.ac.alt_ft > f.cleared_alt + 400
            && f.ac.vs_fpm > 500
            && f.once("alt-bust")
        {
            let t =
                format!("{}, check altitude, maintain {}.", f.call, alt(f.cleared_alt));
            self.say(f, Pos::Departure, t, now, out);
        }
        if release && f.once("release") {
            let t = match &self.cfg.handoff_out {
                Some((name, mhz)) => format!(
                    "{}, {}, radar service terminated, contact {name} {}{}",
                    f.call,
                    self.facility(Pos::Departure),
                    freq(*mhz),
                    self.pick(&[".", ", good day.", ", have a good one.", "."])
                ),
                None => format!(
                    "{}, {}, radar service terminated, frequency change approved.",
                    f.call,
                    self.facility(Pos::Departure)
                ),
            };
            self.say(f, Pos::Departure, t, now, out);
            f.freq = None;
            f.set_phase(Phase::Enroute, now);
        }
    }

    /// The point a radar arrival is steered to.
    fn approach_gate(&self, f: &Flight) -> Option<(P2, f64)> {
        let r = self.ap.runway(&self.arr_runway())?;
        let back = (r.hdg + 180.).rem_euclid(360.);
        let nm = match f.approach {
            Approach::Overhead => self.ap.arrival.initial_nm + 2.,
            Approach::StraightIn | Approach::Visual => 8.,
            _ => self.ap.arrival.final_join_nm,
        };
        Some((r.thr.offset(back, nm * NM), r.hdg))
    }

    /// A vector to join the final course at the gate: direct when well off to
    /// the side, a 30° intercept once near the extended centreline.
    fn vector(&self, f: &Flight) -> Option<f64> {
        let r = self.ap.runway(&self.arr_runway())?;
        let (gate, course) = self.approach_gate(f)?;
        let (along, cross) = r.frame(f.ac.p);
        let out_nm = -along / NM;
        let gate_nm = r.frame(gate).0.abs() / NM;
        if out_nm > gate_nm - 1. && cross.abs() < 4. * NM {
            // Abeam or outside the gate and not far off: intercept.
            let side = if cross > 0. { -1. } else { 1. };
            let cut = (cross.abs() / NM * 15.).clamp(10., 30.);
            Some((course + side * cut).rem_euclid(360.))
        } else {
            Some(f.ac.p.bearing_to(gate))
        }
    }

    fn arriving(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        if f.ac.on_ground {
            return;
        }
        let Some(r) = self.ap.runway(&self.arr_runway()).cloned() else { return };
        let (along, cross) = r.frame(f.ac.p);
        let out_nm = -along / NM;
        let aligned = angdiff(f.ac.hdg, r.hdg).abs() < 35.;
        let field_nm = f.ac.p.dist(r.thr) / NM;
        let sp = self.speaker(f, Pos::Approach);
        if sp != Pos::Approach {
            return; // not on our frequency yet
        }
        if f.is_ai() && self.ai_sequencing(f, field_nm, now, out) {
            return;
        }
        match f.approach {
            Approach::Unspecified => {}
            Approach::Par | Approach::Asr => {
                if aligned && out_nm > 4. && out_nm < 14. && cross.abs() < 1.2 * NM {
                    if self.has(Pos::Final) {
                        let t = format!(
                            "{}, {} miles from touchdown, on final, {}.",
                            f.call,
                            out_nm.round() as i32,
                            self.contact(Pos::Final)
                        );
                        self.say(f, Pos::Approach, t, now, out);
                        f.freq = Some(Pos::Final);
                        f.t_handoff = now;
                        f.set_phase(Phase::RadarFinal, now);
                        return;
                    }
                    f.approach = Approach::StraightIn;
                }
            }
            Approach::Ils | Approach::Tacan => {
                if aligned && out_nm > 4. && out_nm < 15. && cross.abs() < 1.5 * NM {
                    let kind = if f.approach == Approach::Ils { "ILS" } else { "TACAN" };
                    let t = format!(
                        "{}, {} miles from the airport, maintain {} until established, cleared {kind} runway {} approach, {}.",
                        f.call,
                        out_nm.round() as i32,
                        alt(self.ap.arrival.approach_alt_ft),
                        r.name,
                        self.contact(Pos::Tower)
                    );
                    self.say(f, Pos::Approach, t, now, out);
                    f.freq = Some(Pos::Tower);
                    f.t_handoff = now;
                    f.set_phase(Phase::Pattern, now);
                    return;
                }
            }
            Approach::Overhead | Approach::StraightIn | Approach::Visual => {
                if field_nm < self.ap.arrival.tower_radius_nm + 4. {
                    let t = format!("{}, {}.", f.call, self.contact(Pos::Tower));
                    self.say(f, Pos::Approach, t, now, out);
                    f.freq = Some(Pos::Tower);
                    f.t_handoff = now;
                    f.set_phase(Phase::Pattern, now);
                    return;
                }
            }
        }
        // Keep the vectors coming.
        if !f.is_ai() && f.approach != Approach::Unspecified && now - f.t_vector > 25. {
            if let Some(v) = self.vector(f) {
                let old = f.vector_hdg.unwrap_or(f.ac.hdg);
                if angdiff(v, f.ac.hdg).abs() > 20. && angdiff(v, old).abs() > 10. {
                    let dir = if angdiff(v, f.ac.hdg) > 0. { "right" } else { "left" };
                    let t = format!("{}, turn {dir} heading {}.", f.call, hdg(v));
                    self.say(f, Pos::Approach, t, now, out);
                    f.vector_hdg = Some(v);
                    f.t_vector = now;
                }
            }
        }
    }

    /// The precision approach talkdown.
    fn par(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        let Some(r) = self.ap.runway(&self.arr_runway()).cloned() else { return };
        if f.ac.on_ground {
            return; // touchdown() takes over
        }
        let fac = self.facility(Pos::Final);
        let a = &self.ap.arrival;
        // The Final controller calls first if the pilot has not checked in.
        if !f.gca_started {
            if now - f.t_handoff > 10. && f.once("hear") {
                let t = format!("{}, {fac}, how do you hear?", f.call);
                self.say(f, Pos::Final, t, now, out);
            }
            if now - f.t_handoff > 18. {
                self.gca_intro(f, now, out);
            }
            return;
        }
        if now - f.t_gca < self.cfg.gca_interval_s {
            return;
        }
        f.t_gca = now;
        let (along, cross) = r.frame(f.ac.p);
        let dist_m = (a.touchdown_m - along).max(0.);
        let dist_nm = dist_m / NM;
        let path_ft =
            (dist_m * a.glidepath_deg.to_radians().tan()) / FT + self.ap.elev_ft as f64;
        let err_ft = f.ac.alt_ft as f64 - path_ft;
        let ang = cross.atan2(dist_m.max(1.)).to_degrees(); // + right of course
        let hat = f.ac.alt_ft - self.ap.elev_ft;

        // Unsafe: too far off to continue.
        if ang.abs() > 6.
            || err_ft.abs() > (dist_nm * 300.).max(250.)
            || angdiff(f.ac.hdg, r.hdg).abs() > 60.
        {
            let side = if ang > 0. { "right" } else { "left" };
            let what = if ang.abs() > 6. {
                format!("too far {side} of course")
            } else if err_ft > 0. {
                "too high for safe approach".into()
            } else {
                "too low for safe approach".into()
            };
            let t = format!(
                "{}, {what}. If runway not in sight, execute missed approach. Fly runway heading, climb and maintain {}, {}.",
                f.call,
                alt(a.approach_alt_ft),
                self.contact(Pos::Approach)
            );
            self.say(f, Pos::Final, t, now, out);
            self.missed(f, now);
            return;
        }

        let mut parts: Vec<String> = vec![];
        // Course
        if ang.abs() < 0.4 {
            parts.push("on course".into());
        } else {
            let side = if ang > 0. { "right" } else { "left" };
            let corr = (-ang * 3.).clamp(-10., 10.);
            let h = hdg(r.hdg + corr);
            let deg = if ang.abs() < 1.2 {
                format!("slightly {side} of course")
            } else if ang.abs() < 3. {
                format!("{side} of course")
            } else {
                format!("well {side} of course")
            };
            let turn = if corr > 0. { "right" } else { "left" };
            parts.push(format!("{deg}, turn {turn} heading {h}"));
        }
        // Glidepath
        let tol = (dist_nm * 25.).clamp(15., 120.);
        if err_ft.abs() <= tol {
            parts.push("on glidepath".into());
        } else if err_ft.abs() <= tol * 3. {
            parts.push(if err_ft > 0. {
                "slightly above glidepath".into()
            } else {
                "slightly below glidepath".into()
            });
        } else {
            parts.push(if err_ft > 0. {
                "well above glidepath".into()
            } else {
                "well below glidepath".into()
            });
        }
        // Range at each whole mile
        let mile = dist_nm.floor() as i32;
        if mile < f.gca_mile && mile >= 1 {
            f.gca_mile = mile;
            parts.push(format!(
                "{mile} {} from touchdown",
                if mile == 1 { "mile" } else { "miles" }
            ));
        }
        let mut t = format!("{}, {}.", f.call, parts.join(", "));

        // Landing clearance at about four miles.
        if dist_nm < 4.2 && !f.landing_cleared {
            match self.landing_blocked(f, &r.name) {
                None => {
                    t.push_str(&format!(
                        " Runway {}, cleared to land, {}.",
                        r.name,
                        self.wind()
                    ));
                    f.landing_cleared = true;
                }
                Some(why) if dist_nm < 1.2 => {
                    let gt = format!(
                        "{}, go around, {why}. Fly runway heading, climb and maintain {}, {}.",
                        f.call,
                        alt(a.approach_alt_ft),
                        self.contact(Pos::Approach)
                    );
                    self.say(f, Pos::Final, gt, now, out);
                    self.missed(f, now);
                    return;
                }
                Some(_) if f.once("continue") => {
                    t.push_str(" Continue, traffic on the runway.")
                }
                Some(_) => {}
            }
        }
        // Decision height, then the threshold.
        if hat <= a.decision_height_ft + 50 && f.once("dh") {
            t = format!(
                "{}, at decision height, {}.",
                f.call,
                parts[..parts.len().min(2)].join(", ")
            );
        } else if dist_m < a.touchdown_m + 400. && f.once("thr") {
            t = format!("{}, over landing threshold.", f.call);
        }
        self.say(f, Pos::Final, t, now, out);
    }

    fn gca_intro(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        if f.gca_started {
            return;
        }
        f.gca_started = true;
        f.t_gca = now;
        let t = format!(
            "{}, {}, this will be a precision approach to runway {}. Wheels should be down. Do not acknowledge further transmissions.",
            f.call,
            self.facility(Pos::Final),
            self.arr_runway()
        );
        self.say(f, Pos::Final, t, now, out);
    }

    fn missed(&self, f: &mut Flight, now: f64) {
        f.landing_cleared = false;
        f.gca_started = false;
        f.gca_mile = i32::MAX;
        f.freq = Some(Pos::Approach);
        f.t_handoff = now;
        f.cleared_alt = self.ap.arrival.approach_alt_ft;
        f.set_phase(Phase::Arrival, now);
    }

    /// Why an arrival cannot be cleared to land, if it cannot.
    fn landing_blocked(&self, f: &Flight, runway: &str) -> Option<String> {
        let r = self.ap.runway(runway)?;
        if let Some(why) = self.is_closed(&r.pair) {
            return Some(format!("runway closed, {why}"));
        }
        if let Some(o) = self.runway_occupant(&r.pair, f.key) {
            // A departure already rolling away from us is fine.
            let rolling_out = o.on_ground
                && o.gs > 60.
                && angdiff(o.hdg, r.hdg).abs() < 20.
                && r.frame(o.p).0 > 0.;
            if !rolling_out {
                return Some("traffic on the runway".into());
            }
        }
        if let Some(opp) = self.ap.opposite(r) {
            if self.departure_ahead(&opp.name, f.key).is_some() {
                return Some("opposite direction traffic".into());
            }
        }
        None
    }

    fn landing_clearance(&self, f: &Flight, runway: &str) -> String {
        let what = match f.option {
            LandingOption::TouchAndGo => "cleared touch and go",
            LandingOption::LowApproach => "cleared low approach",
            LandingOption::TheOption => "cleared for the option",
            _ => "cleared to land",
        };
        // a heavy landing ahead on the same final
        let wake = self
            .flights
            .values()
            .filter(|o| o.key != f.key && heavy(&o.ac.typ) && !heavy(&f.ac.typ))
            .filter(|o| {
                matches!(o.phase, Phase::Pattern | Phase::RadarFinal | Phase::Landing)
            })
            .filter_map(|o| {
                let r = self.ap.runway(runway)?;
                let (mine, theirs) = (f.ac.p.dist(r.thr), o.ac.p.dist(r.thr));
                (theirs < mine && mine - theirs < 6. * NM)
                    .then(|| type_name(&o.ac.typ).unwrap_or("heavy"))
            })
            .next()
            .map(|t| format!(", caution wake turbulence, landing {t}"))
            .unwrap_or_default();
        if f.emergency {
            format!(
                "{}, runway {runway}, {what}, {}, emergency equipment is standing by.",
                f.call,
                self.wind()
            )
        } else {
            format!(
                "{}, check wheels down, runway {runway}, {what}, {}{wake}.",
                f.call,
                self.wind()
            )
        }
    }

    fn pattern(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        if f.ac.on_ground || f.freq != Some(Pos::Tower) {
            return;
        }
        let rwy = self.arr_runway();
        let Some(r) = self.ap.runway(&rwy).cloned() else { return };
        let (along, cross) = r.frame(f.ac.p);
        let out_nm = -along / NM;
        let on_final = out_nm > -0.2
            && out_nm < 3.
            && cross.abs() < 0.6 * NM
            && angdiff(f.ac.hdg, r.hdg).abs() < 40.;
        if !on_final {
            return;
        }
        if !f.landing_cleared {
            match self.landing_blocked(f, &rwy) {
                None => {
                    let t = self.landing_clearance(f, &rwy);
                    self.say(f, Pos::Tower, t, now, out);
                    f.landing_cleared = true;
                    f.landing_waiting = false;
                }
                Some(why) => {
                    if out_nm < 0.6 && f.ac.agl_ft > 50 && !f.emergency {
                        let t = format!(
                            "{}, go around, {why}. Climb to {}, report downwind.",
                            f.call,
                            alt(self.ap.arrival.pattern_alt_ft)
                        );
                        self.say(f, Pos::Tower, t, now, out);
                        f.said.insert("continue");
                        f.landing_waiting = false;
                    } else if f.once("continue") {
                        let t = format!("{}, continue, {why}.", f.call);
                        self.say(f, Pos::Tower, t, now, out);
                        f.landing_waiting = true;
                    }
                }
            }
        } else if out_nm < 0.5 && f.ac.agl_ft > 30 && !f.emergency {
            // Cleared earlier, but someone has since rolled onto the runway.
            if let Some(why) = self.landing_blocked(f, &rwy) {
                if f.once("late-go-around") {
                    let t = format!(
                        "{}, go around, {why}. Climb to {}, report downwind.",
                        f.call,
                        alt(self.ap.arrival.pattern_alt_ft)
                    );
                    self.say(f, Pos::Tower, t, now, out);
                    f.landing_cleared = false;
                }
            }
        }
    }

    // ─── answering calls ──────────────────────────────────────────────────

    /// Which flight a transmission is from, adopting a callsign the pilot
    /// gave.
    pub(crate) fn identify(
        &mut self,
        who: &Caller,
        parsed: Option<&Parsed>,
    ) -> Option<u64> {
        let key = who
            .unit_id
            .filter(|u| self.flights.contains_key(u))
            .or_else(|| {
                let n = norm(&who.srs_name);
                (!n.is_empty())
                    .then(|| {
                        self.flights.values().find(|f| {
                            let m = norm(&f.ac.name);
                            !m.is_empty() && (n.contains(&m) || m.contains(&n))
                        })
                    })
                    .flatten()
                    .map(|f| f.key)
            })
            .or_else(|| {
                let c = parsed.map(|p| norm(&p.callsign)).filter(|c| !c.is_empty())?;
                self.flights.values().find(|f| norm(&f.call) == c).map(|f| f.key)
            })?;
        if let Some(p) = parsed {
            if !p.callsign.is_empty() {
                if let Some(f) = self.flights.get_mut(&key) {
                    f.call = p.callsign.clone();
                    f.call_confirmed = true;
                }
            }
        }
        Some(key)
    }

    /// The context the language model gets.
    pub(crate) fn llm_context(
        &self,
        pos: Pos,
        key: Option<u64>,
        heard: &str,
    ) -> llm::Context {
        let f = key.and_then(|k| self.flights.get(&k));
        llm::Context {
            position: self.facility(pos),
            situation: f.map(|f| f.situation()).unwrap_or_else(|| "unknown".into()),
            last_instruction: f.and_then(|f| f.last.get(&pos).cloned()),
            heard: heard.to_string(),
        }
    }

    /// Which position should handle a request from a flight in this state.
    fn owner(&self, f: &Flight, intent: Intent) -> Pos {
        use Intent::*;
        let first =
            |ps: &[Pos]| ps.iter().copied().find(|p| self.has(*p)).unwrap_or(Pos::Tower);
        match intent {
            RequestClearance | ClearanceReadback => first(&[Pos::Clearance, Pos::Ground]),
            RequestStartup | RequestTaxi | RequestTaxiParking | RequestCrossing => {
                first(&[Pos::Ground, Pos::Tower])
            }
            ClearOfRunway => first(&[Pos::Ground, Pos::Tower]),
            ReadyForDeparture | ReportInitial | ReportBreak | ReportDownwind
            | ReportBase | ReportFinal | RequestClosed => Pos::Tower,
            GoingAround => {
                if f.phase == Phase::RadarFinal {
                    Pos::Final
                } else {
                    Pos::Tower
                }
            }
            CheckIn | RequestApproach => {
                if f.ac.on_ground {
                    first(&[Pos::Ground, Pos::Tower])
                } else if f.phase == Phase::RadarFinal {
                    Pos::Final
                } else if matches!(
                    f.phase,
                    Phase::Departure | Phase::Climbout | Phase::TakeoffCleared
                ) {
                    first(&[Pos::Departure, Pos::Approach, Pos::Tower])
                } else {
                    first(&[Pos::Approach, Pos::Tower])
                }
            }
            _ => Pos::Tower,
        }
    }

    /// Answer a transmission heard on `pos`. `parsed` is `None` when neither
    /// the rules nor the language model understood it.
    pub(crate) fn respond(
        &mut self,
        pos: Pos,
        who: &Caller,
        heard: &str,
        parsed: Option<Parsed>,
        now: f64,
    ) -> Vec<Tx> {
        let fac = self.facility(pos);
        let key = self.identify(who, parsed.as_ref());
        let mut out = vec![];
        let Some(key) = key else {
            // Nobody we have on radar.
            let addressed = crate::voice::stt::mentions(heard, &self.ap.callsign);
            match parsed.as_ref().map(|p| p.intent) {
                Some(Intent::RadioCheck) => out.push(Tx::new(
                    pos,
                    format!("Station calling {fac}, loud and clear, say callsign."),
                    None,
                    now + STALE_S,
                )),
                Some(Intent::RequestWeather) => out.push(Tx::new(
                    pos,
                    format!(
                        "Station calling {fac}, {}, {}.",
                        self.wind(),
                        self.altimeter()
                    ),
                    None,
                    now + STALE_S,
                )),
                Some(Intent::None) | Some(Intent::Readback) => {}
                _ if addressed => out.push(Tx::new(
                    pos,
                    format!("Station calling {fac}, say again callsign."),
                    None,
                    now + STALE_S,
                )),
                _ => {}
            }
            return out;
        };
        let Some(mut f) = self.flights.remove(&key) else { return out };
        f.freq = Some(pos);
        self.answer(&mut f, pos, heard, parsed, now, &mut out);
        self.flights.insert(key, f);
        out
    }

    fn answer(
        &mut self,
        f: &mut Flight,
        pos: Pos,
        heard: &str,
        parsed: Option<Parsed>,
        now: f64,
        out: &mut Vec<Tx>,
    ) {
        let fac = self.facility(pos);
        let Some(p) = parsed else {
            if crate::voice::stt::mentions(heard, &self.ap.callsign)
                || f.call_confirmed && crate::voice::stt::mentions(heard, &f.call)
            {
                let t = format!("{}, {fac}, say again.", f.call);
                self.say(f, pos, t, now, out);
            }
            return;
        };
        use Intent::*;
        // Things any position answers.
        match p.intent {
            None => return,
            RadioCheck => {
                let t = format!(
                    "{}, {fac}, {}.",
                    f.call,
                    self.pick(&[
                        "loud and clear",
                        "read you loud and clear",
                        "loud and clear, go ahead"
                    ])
                );
                return self.say(f, pos, t, now, out);
            }
            SayAgain => {
                let t = f.last.get(&pos).cloned().unwrap_or_else(|| {
                    format!("{}, {fac}, no further instructions.", f.call)
                });
                let mut tx = Tx::new(pos, t, Some(f.key), now + STALE_S);
                tx.delay = self.between(0.6, 1.5);
                tx.subtitle = (!f.is_ai()).then_some(f.key);
                out.push(tx);
                return;
            }
            RequestWeather => {
                let t = format!(
                    "{}, {fac}, {}, {}, runway {} in use, information {}.",
                    f.call,
                    self.wind(),
                    self.altimeter(),
                    if f.ac.on_ground { self.dep_runway() } else { self.arr_runway() },
                    self.atis_letter()
                );
                return self.say(f, pos, t, now, out);
            }
            Emergency => return self.emergency(f, pos, now, out),
            Unable => {
                let t = format!("{}, roger, say intentions.", f.call);
                return self.say(f, pos, t, now, out);
            }
            RequestFrequencyChange => {
                let t = format!(
                    "{}, frequency change approved{}",
                    f.call,
                    self.pick(&[".", ", good day.", ", see you."])
                );
                self.say(f, pos, t, now, out);
                if matches!(f.phase, Phase::Departure | Phase::Climbout) {
                    f.set_phase(Phase::Enroute, now);
                }
                f.freq = Option::None;
                return;
            }
            Readback => return self.check_readback(f, pos, heard, now, out),
            _ => {}
        }
        // Everything else belongs to one position.
        let owner = self.owner(f, p.intent);
        if owner != pos && !(pos == Pos::Tower && self.tower_takes(f, p.intent)) {
            let t = format!("{}, {fac}, {}.", f.call, self.contact(owner));
            self.say(f, pos, t, now, out);
            f.freq = Some(owner);
            return;
        }
        // Already told to stand by: still coming.
        if !self.replaying
            && self
                .deferred
                .iter()
                .any(|d| d.2 == f.key && d.1 == pos && d.4.intent == p.intent)
        {
            if f.once("standby-again") {
                let t = format!(
                    "{}, {}",
                    f.call,
                    self.pick(&[
                        "roger, standby.",
                        "I have you, standby.",
                        "copy, stand by one."
                    ])
                );
                self.say(f, pos, t, now, out);
            }
            return;
        }
        if !self.replaying && self.wants_standby(f, pos, p.intent, now) {
            let t = format!(
                "{}, {fac}, {}",
                f.call,
                self.pick(&["standby.", "stand by.", "standby one.", "roger, standby."])
            );
            self.say(f, pos, t, now, out);
            let due = now + self.busy_for(pos, now, Option::None) + self.between(3., 8.);
            self.deferred.push((due, pos, f.key, heard.to_string(), p));
            return;
        }
        match p.intent {
            RequestClearance => self.clearance(f, pos, &p, now, out),
            ClearanceReadback => self.clearance_readback(f, pos, &p, now, out),
            RequestStartup => {
                let t = format!(
                    "{}, {fac}, start up approved, {}, {}, advise ready to taxi.",
                    f.call,
                    self.wind(),
                    self.altimeter()
                );
                self.say(f, pos, t, now, out);
            }
            RequestTaxi => self.taxi(f, pos, &p, now, out),
            RequestTaxiParking | ClearOfRunway => {
                if pos == Pos::Tower && self.has(Pos::Ground) {
                    let t = format!("{}, {}.", f.call, self.contact(Pos::Ground));
                    self.say(f, pos, t, now, out);
                    f.freq = Some(Pos::Ground);
                } else {
                    self.taxi_to_parking(f, pos, &p, now, out);
                }
            }
            RequestCrossing => {
                let pair =
                    f.route.as_ref().and_then(|r| r.crossing.clone()).or_else(|| {
                        self.ap
                            .holds
                            .get(self.ap.hold_near(f.ac.p, 60.)?)
                            .map(|h| h.pair.clone())
                    });
                match pair {
                    Some(pair) => {
                        f.said.remove("cross-hold");
                        self.offer_crossing(f, &pair, now, out);
                    }
                    Option::None => {
                        let t = format!("{}, {fac}, say position.", f.call);
                        self.say(f, pos, t, now, out);
                    }
                }
            }
            ReadyForDeparture => self.ready(f, pos, &p, now, out),
            CheckIn | RequestApproach => self.check_in(f, pos, &p, now, out),
            ReportInitial => {
                f.set_phase(Phase::Pattern, now);
                f.approach = Approach::Overhead;
                if p.option != LandingOption::Unspecified {
                    f.option = p.option;
                }
                let n = self.sequence(f);
                let rwy = self.arr_runway();
                let t = if n > 1 {
                    let follow = self
                        .landing_ahead(f)
                        .map(|(typ, d)| format!(", follow the {typ} on a {d} mile final"))
                        .unwrap_or_default();
                    format!("{}, {fac}, runway {rwy}, number {n}{follow}, break at the numbers, report base.", f.call)
                } else {
                    format!(
                        "{}, {fac}, runway {rwy}, break approved, {}, report base.",
                        f.call,
                        self.altimeter()
                    )
                };
                self.say(f, pos, t, now, out);
            }
            ReportBreak | ReportDownwind => {
                f.set_phase(Phase::Pattern, now);
                let t = format!("{}, report base.", f.call);
                self.say(f, pos, t, now, out);
            }
            ReportBase | ReportFinal => {
                if f.phase != Phase::Pattern {
                    f.set_phase(Phase::Pattern, now);
                }
                if p.option != LandingOption::Unspecified {
                    f.option = p.option;
                }
                let rwy = self.arr_runway();
                match self.landing_blocked(f, &rwy) {
                    Option::None => {
                        let t = self.landing_clearance(f, &rwy);
                        self.say(f, pos, t, now, out);
                        f.landing_cleared = true;
                    }
                    Some(why) => {
                        let n = self.sequence(f);
                        let t = format!("{}, number {n}, continue, {why}.", f.call);
                        self.say(f, pos, t, now, out);
                        f.said.insert("continue");
                        f.landing_waiting = true;
                    }
                }
            }
            GoingAround => {
                if pos == Pos::Final {
                    let t = format!(
                        "{}, roger, fly runway heading, climb and maintain {}, {}.",
                        f.call,
                        alt(self.ap.arrival.approach_alt_ft),
                        self.contact(Pos::Approach)
                    );
                    self.say(f, pos, t, now, out);
                    self.missed(f, now);
                } else {
                    f.landing_cleared = false;
                    f.set_phase(Phase::Pattern, now);
                    let t = format!(
                        "{}, roger, climb to {}, report downwind.",
                        f.call,
                        alt(self.ap.arrival.pattern_alt_ft)
                    );
                    self.say(f, pos, t, now, out);
                }
            }
            RequestClosed => {
                f.landing_cleared = false;
                f.set_phase(Phase::Pattern, now);
                let t = format!("{}, closed approved, report base.", f.call);
                self.say(f, pos, t, now, out);
            }
            _ => {}
        }
    }

    /// Does a request get "standby" before its answer: always on a busy
    /// frequency, now and then on a quiet one (the controller finishing
    /// something else, finding the strip).
    fn wants_standby(&self, f: &Flight, pos: Pos, intent: Intent, now: f64) -> bool {
        use Intent::*;
        if f.emergency
            || !matches!(
                intent,
                RequestTaxi
                    | CheckIn
                    | RequestApproach
                    | RequestTaxiParking
                    | RequestStartup
            )
        {
            return false;
        }
        if self.busy_for(pos, now, Some(f.key)) > 6. {
            return true;
        }
        let odds = if self.quiet(pos, now, 15.) {
            self.standby_odds
        } else {
            2.5 * self.standby_odds
        };
        self.rand() < odds
    }

    /// The answers held back with "standby", now due.
    fn release_deferred(&mut self, now: f64, out: &mut Vec<Tx>) {
        let (due, keep): (Vec<_>, Vec<_>) =
            std::mem::take(&mut self.deferred).into_iter().partition(|d| d.0 <= now);
        self.deferred = keep;
        for (_, pos, key, heard, p) in due {
            let Some(mut f) = self.flights.remove(&key) else { continue };
            self.replaying = true;
            self.answer(&mut f, pos, &heard, Some(p), now, out);
            self.replaying = false;
            f.said.remove("standby-again");
            self.flights.insert(key, f);
        }
    }

    /// Tower also takes an arrival's first call when it is already close in
    /// and flying a visual recovery: sending it back out to Approach would be
    /// silly.
    fn tower_takes(&self, f: &Flight, intent: Intent) -> bool {
        matches!(intent, Intent::CheckIn | Intent::RequestApproach)
            && !f.ac.on_ground
            && self.ap.runway(&self.arr_runway()).is_some_and(|r| {
                f.ac.p.dist(r.thr) < (self.ap.arrival.tower_radius_nm + 4.) * NM
            })
    }

    fn emergency(&self, f: &mut Flight, pos: Pos, now: f64, out: &mut Vec<Tx>) {
        f.emergency = true;
        let fac = self.facility(pos);
        let t = if f.ac.on_ground {
            format!(
                "{}, {fac}, roger, hold position, emergency equipment is responding.",
                f.call
            )
        } else {
            let rwy = self.arr_runway();
            let brg =
                self.ap.runway(&rwy).map(|r| f.ac.p.bearing_to(r.thr)).unwrap_or(0.);
            format!(
                "{}, {fac}, roger your emergency. Runway {rwy} is yours, fly heading {}, descend at pilot's discretion, {}, {}. Say souls on board and fuel remaining.",
                f.call,
                hdg(brg),
                self.wind(),
                self.altimeter()
            )
        };
        self.say(f, pos, t, now, out);
        log::warn!("milatc {}: {} declared an emergency", self.ap.callsign, f.call);
    }

    fn check_readback(
        &self,
        f: &mut Flight,
        pos: Pos,
        heard: &str,
        now: f64,
        out: &mut Vec<Tx>,
    ) {
        let Some(last) = f.last.get(&pos).cloned() else { return };
        if last.contains("hold short")
            && !crate::voice::stt::contains_phrase(heard, "hold short")
            && f.once("rb-hold-short")
        {
            let t = format!("{}, read back hold short instruction.", f.call);
            self.say(f, pos, t, now, out);
        }
    }

    fn clearance(
        &mut self,
        f: &mut Flight,
        pos: Pos,
        p: &Parsed,
        now: f64,
        out: &mut Vec<Tx>,
    ) {
        let fac = self.facility(pos);
        if !f.ac.on_ground {
            let t = format!("{}, {fac}, {}.", f.call, self.contact(Pos::Departure));
            return self.say(f, pos, t, now, out);
        }
        match f.clr {
            Clr::Ready => {}
            Clr::Preparing(_) => {
                let t = format!(
                    "{}, {fac}, {}",
                    f.call,
                    self.pick(&[
                        "still working on your clearance, standby.",
                        "standby, clearance is coming.",
                        "copy, standby."
                    ])
                );
                return self.say(f, pos, t, now, out);
            }
            // The first request: Clearance has to get it from the system.
            Clr::None if f.squawk.is_none() && !f.is_ai() => {
                f.clr = Clr::Preparing(now + self.between(10., 22.));
                f.clr_dest = p.destination.clone();
                let t = format!(
                    "{}, {fac}, {}",
                    f.call,
                    self.pick(&[
                        "clearance on request, standby.",
                        "copy, clearance on request.",
                        "roger, standby for your clearance."
                    ])
                );
                return self.say(f, pos, t, now, out);
            }
            Clr::None => {}
        }
        f.clr = Clr::None;
        let sq = match f.squawk {
            Some(s) => s,
            None => self.take_squawk(),
        };
        f.squawk = Some(sq);
        f.cleared_alt = self.ap.departure.initial_alt_ft;
        let wanted = if p.destination.trim().is_empty() {
            f.clr_dest.clone()
        } else {
            p.destination.clone()
        };
        let limit = if wanted.trim().is_empty() {
            self.ap.departure.clearance_limit.clone()
        } else {
            wanted
        };
        let dep = self
            .freq_of(Pos::Departure)
            .map(|d| format!(", departure frequency {d}"))
            .unwrap_or_default();
        let t = format!(
            "{}, {fac}, cleared to {limit} via radar vectors, fly runway heading, climb and maintain {}, expect {} 10 minutes after departure{dep}, squawk {sq:04}.",
            f.call,
            alt(self.ap.departure.initial_alt_ft),
            alt(self.ap.departure.expect_alt_ft),
        );
        self.say(f, pos, t, now, out);
    }

    fn clearance_readback(
        &self,
        f: &mut Flight,
        pos: Pos,
        p: &Parsed,
        now: f64,
        out: &mut Vec<Tx>,
    ) {
        let Some(sq) = f.squawk else {
            let t = format!(
                "{}, {}, no clearance on file, say request.",
                f.call,
                self.facility(pos)
            );
            return self.say(f, pos, t, now, out);
        };
        let t = if p.squawk != 0 && p.squawk != sq {
            format!("{}, negative, squawk {sq:04}.", f.call)
        } else if p.altitude_ft != 0
            && (p.altitude_ft - f.cleared_alt).abs() > 50
            && p.altitude_ft != self.ap.departure.expect_alt_ft
        {
            format!("{}, negative, climb and maintain {}.", f.call, alt(f.cleared_alt))
        } else if self.has(Pos::Ground) && pos != Pos::Ground {
            format!(
                "{}, readback correct, {} when ready to taxi.",
                f.call,
                self.contact(Pos::Ground)
            )
        } else {
            format!("{}, readback correct.", f.call)
        };
        self.say(f, pos, t, now, out);
    }

    fn taxi(
        &mut self,
        f: &mut Flight,
        pos: Pos,
        p: &Parsed,
        now: f64,
        out: &mut Vec<Tx>,
    ) {
        let fac = self.facility(pos);
        if !f.ac.on_ground {
            return;
        }
        if self.cfg.require_clearance && f.squawk.is_none() && self.has(Pos::Clearance) {
            let t = format!(
                "{}, {fac}, clearance on request, {}.",
                f.call,
                self.contact(Pos::Clearance)
            );
            return self.say(f, pos, t, now, out);
        }
        // The runway asked for, if it is one in use; else the departure runway.
        let mut rwy = self.dep_runway();
        if !p.runway.is_empty() {
            if let Some(r) = self.ap.runway(&p.runway) {
                if r.direction() == self.direction {
                    rwy = r.name.clone();
                }
            }
        }
        // An AI departure cannot be stopped at the hold line once it is
        // rolling, so Ground keeps it on its stand while the runway is busy.
        if f.is_ai() {
            if let Some(why) = self.ai_departure_blocked(f, &rwy) {
                if !f.ai_taxi_hold {
                    let t = format!(
                        "{}, {fac}, hold position, {why}, expect taxi in {} minutes.",
                        f.call,
                        if why.contains("final") { "two" } else { "three" }
                    );
                    self.say(f, pos, t, now, out);
                    f.ai_taxi_hold = true;
                }
                return;
            }
            f.ai_taxi_hold = false;
        }
        let route = self.ap.route_to_runway(f.ac.p, &rwy);
        if route.is_none() {
            log::warn!(
                "milatc {}: no taxi route for {} to runway {rwy} from {:.0} m N {:.0} m E of the field",
                self.ap.callsign,
                f.call,
                f.ac.p.n,
                f.ac.p.e
            );
        }
        let tail = format!("{}, {}", self.wind(), self.altimeter());
        let t = match &route {
            Some(r) => {
                let via = r.via.iter().map(|v| speech::taxiway(v)).collect::<Vec<_>>();
                let via = if via.is_empty() { String::new() } else { format!(" via {}", via.join(", ")) };
                // the holding point, by its taxiway
                let at = r
                    .hold
                    .and_then(|h| self.ap.holds.get(h))
                    .filter(|h| !h.taxiway.is_empty())
                    .map(|h| format!(" at {}", speech::taxiway(&h.taxiway)))
                    .unwrap_or_default();
                match &r.crossing {
                    Some(pair) => {
                        let cross = self.ap.strip_name(pair, &self.direction);
                        // The route up to the crossing only.
                        format!("{}, {fac}, runway {rwy}, taxi{via}, hold short runway {cross}, {tail}.", f.call)
                    }
                    Option::None if via.is_empty() && !at.is_empty() => {
                        format!("{}, {fac}, runway {rwy}, taxi to holding point{}, hold short runway {rwy}, {tail}.", f.call, at.trim_start_matches(" at"))
                    }
                    Option::None => format!("{}, {fac}, runway {rwy}, taxi{via}, hold short runway {rwy}{at}, {tail}.", f.call),
                }
            }
            Option::None => format!("{}, {fac}, runway {rwy}, taxi to and hold short of runway {rwy}, {tail}.", f.call),
        };
        f.runway = Some(rwy);
        f.route = route;
        f.set_phase(Phase::TaxiOut, now);
        self.say(f, pos, t, now, out);
        if f.is_ai() && f.ac.ai_state == "waiting" {
            self.command(f, "start", out);
        }
    }

    fn taxi_to_parking(
        &self,
        f: &mut Flight,
        pos: Pos,
        p: &Parsed,
        now: f64,
        out: &mut Vec<Tx>,
    ) {
        let fac = self.facility(pos);
        if !f.ac.on_ground {
            return;
        }
        let ramp = self.ap.ramp_for(f.ac.p, Some(&p.destination)).cloned();
        let route = ramp.as_ref().and_then(|rm| self.ap.route_to_ramp(f.ac.p, rm));
        let dest = ramp
            .as_ref()
            .map(|r| format!("the {}", r.name))
            .unwrap_or_else(|| "parking".into());
        let t = match &route {
            Some(r) if !r.via.is_empty() => {
                let via = r
                    .via
                    .iter()
                    .map(|v| speech::taxiway(v))
                    .collect::<Vec<_>>()
                    .join(", ");
                match &r.crossing {
                    Some(pair) => format!(
                        "{}, {fac}, taxi to {dest} via {via}, hold short runway {}.",
                        f.call,
                        self.ap.strip_name(pair, &self.direction)
                    ),
                    Option::None => {
                        format!("{}, {fac}, taxi to {dest} via {via}.", f.call)
                    }
                }
            }
            _ => format!("{}, {fac}, taxi to {dest}.", f.call),
        };
        f.route = route;
        f.set_phase(Phase::TaxiIn, now);
        self.say(f, pos, t, now, out);
    }

    fn ready(&self, f: &mut Flight, pos: Pos, p: &Parsed, now: f64, out: &mut Vec<Tx>) {
        let fac = self.facility(pos);
        if !f.ac.on_ground {
            return;
        }
        let rwy = f
            .runway
            .clone()
            .or_else(|| (!p.runway.is_empty()).then(|| p.runway.clone()))
            .unwrap_or_else(|| self.dep_runway());
        if p.option != LandingOption::Unspecified {
            f.option = p.option;
        }
        f.runway = Some(rwy.clone());
        f.ready_at.get_or_insert(now);
        // someone else called ready first
        let ahead = self.departures_ahead(f, &rwy).len();
        if ahead > 0 {
            let t = format!(
                "{}, {fac}, hold short runway {rwy}, number {} for departure.",
                f.call,
                ahead + 1
            );
            self.say(f, pos, t, now, out);
            f.takeoff_waiting = true;
            f.set_phase(Phase::HoldingShort, now);
            return;
        }
        match self.takeoff_blocked(f, &rwy) {
            Option::None => {
                let t = self.takeoff_clearance(f, &rwy, now).replacen(
                    &f.call,
                    &format!("{}, {fac}", f.call),
                    1,
                );
                self.say(f, pos, t, now, out);
                f.takeoff_waiting = false;
                f.ready_at = None;
                f.set_phase(Phase::TakeoffCleared, now);
            }
            Some(why) if why == "departing traffic" => {
                let t =
                    format!("{}, {fac}, runway {rwy}, line up and wait, {why}.", f.call);
                self.say(f, pos, t, now, out);
                f.takeoff_waiting = true;
                f.set_phase(Phase::LinedUp, now);
            }
            Some(why) => {
                let t = format!("{}, {fac}, hold short runway {rwy}, {why}.", f.call);
                self.say(f, pos, t, now, out);
                f.takeoff_waiting = true;
                f.set_phase(Phase::HoldingShort, now);
            }
        }
    }

    fn check_in(
        &self,
        f: &mut Flight,
        pos: Pos,
        p: &Parsed,
        now: f64,
        out: &mut Vec<Tx>,
    ) {
        let fac = self.facility(pos);
        if f.ac.on_ground {
            return;
        }
        let rwy = self.arr_runway();
        let field = self.ap.runway(&rwy).map(|r| r.thr).unwrap_or_default();
        let d = (f.ac.p.dist(field) / NM).round() as i32;
        let dir = speech::compass(field.bearing_to(f.ac.p));
        let where_ = format!("{d} miles {dir} of {}", self.ap.callsign);
        if p.approach != Approach::Unspecified {
            f.approach = p.approach;
        }
        if p.option != LandingOption::Unspecified {
            f.option = p.option;
        }
        match pos {
            Pos::Departure => {
                f.cleared_alt = self.ap.departure.expect_alt_ft;
                let t = format!(
                    "{}, {fac}, radar contact {where_}, climb and maintain {}, resume own navigation.",
                    f.call,
                    alt(f.cleared_alt)
                );
                self.say(f, pos, t, now, out);
                f.set_phase(Phase::Climbout, now);
            }
            Pos::Final => {
                f.set_phase(Phase::RadarFinal, now);
                self.gca_intro(f, now, out);
            }
            Pos::Tower => {
                f.set_phase(Phase::Pattern, now);
                let report = match f.approach {
                    Approach::StraightIn
                    | Approach::Visual
                    | Approach::Ils
                    | Approach::Tacan
                    | Approach::Par
                    | Approach::Asr => "report 3 mile final",
                    _ if f.ac.rotary => "report short final",
                    _ => "report initial",
                };
                let t = format!(
                    "{}, {fac}, runway {rwy}, {}, {}, {report}.",
                    f.call,
                    self.wind(),
                    self.altimeter()
                );
                self.say(f, pos, t, now, out);
            }
            _ => {
                // Approach
                f.set_phase(Phase::Arrival, now);
                let a = &self.ap.arrival;
                let descend = if f.ac.alt_ft > a.approach_alt_ft + 300 {
                    format!(", descend and maintain {}", alt(a.approach_alt_ft))
                } else {
                    String::new()
                };
                f.cleared_alt = a.approach_alt_ft;
                let t = match f.approach {
                    Approach::Unspecified => format!(
                        "{}, {fac}, radar contact {where_}{descend}, {}, say approach request.",
                        f.call,
                        self.altimeter()
                    ),
                    kind => {
                        let what = match kind {
                            Approach::Overhead => format!("expect the overhead runway {rwy}"),
                            Approach::StraightIn => format!("expect a straight-in runway {rwy}"),
                            Approach::Visual => format!("expect a visual approach runway {rwy}"),
                            Approach::Par => format!("this will be a PAR approach to runway {rwy}"),
                            Approach::Asr => format!("this will be a surveillance approach to runway {rwy}"),
                            Approach::Ils => format!("expect the ILS runway {rwy}"),
                            Approach::Tacan => format!("expect the TACAN runway {rwy}"),
                            Approach::Unspecified => unreachable!(),
                        };
                        let v = if f.is_ai() {
                            format!(", proceed direct {}", self.ap.callsign)
                        } else {
                            self.vector(f).map(|h| format!(", fly heading {}", hdg(h))).unwrap_or_default()
                        };
                        if let Some(h) = self.vector(f).filter(|_| !f.is_ai()) {
                            f.vector_hdg = Some(h);
                            f.t_vector = now;
                        }
                        format!("{}, {fac}, radar contact {where_}{v}{descend}, {}, {what}.", f.call, self.altimeter())
                    }
                };
                self.say(f, pos, t, now, out);
            }
        }
    }
}

// ─── AI traffic ────────────────────────────────────────────────────────────

// ─── when things go wrong ─────────────────────────────────────────────────

impl Controller {
    /// Taxiing on the ramp with nothing issued: hold position.
    fn taxi_without_clearance(&self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        if f.phase != Phase::Ground || f.flown || f.ac.rotary || !self.has(Pos::Ground) {
            f.ground_from = None;
            return;
        }
        let from = *f.ground_from.get_or_insert(f.ac.p);
        // only for those working us: the rest are not listening
        if f.ac.gs > 8.
            && f.ac.p.dist(from) > 150.
            && !f.last.is_empty()
            && f.once("taxi-no-clr")
        {
            let t = format!(
                "{}, {}, you are taxiing without a clearance. Hold position and say request.",
                f.call,
                self.facility(Pos::Ground)
            );
            let sp = self.speaker(f, Pos::Ground);
            self.say(f, sp, t, now, out);
            log::info!(
                "milatc {}: {} taxiing without a clearance",
                self.ap.callsign,
                f.call
            );
        }
    }

    fn close(
        &mut self,
        pair: &str,
        until: f64,
        why: &str,
        by: Option<u64>,
        now: f64,
        out: &mut Vec<Tx>,
    ) {
        if self.closed.contains_key(pair) {
            return;
        }
        let before = (self.dep_runway(), self.arr_runway());
        self.closed.insert(pair.to_string(), Closure { until, why: why.to_string(), by });
        let name = self.ap.strip_name(pair, &self.direction).to_string();
        let after = (self.dep_runway(), self.arr_runway());
        let mut t = format!(
            "All aircraft, {}, runway {name} is closed, {why}.",
            self.facility(Pos::Tower)
        );
        if after != before
            && !self.closed.contains_key(
                self.ap.runway(&after.0).map(|r| r.pair.as_str()).unwrap_or(""),
            )
        {
            t.push_str(&format!(" Runway {} in use.", after.0));
        }
        log::warn!("milatc {}: runway {name} closed: {why}", self.ap.callsign);
        self.broadcast(Pos::Tower, t.clone(), now, out);
        if self.has(Pos::Approach) {
            self.broadcast(
                Pos::Approach,
                t.replace(&self.facility(Pos::Tower), &self.facility(Pos::Approach)),
                now,
                out,
            );
        }
    }

    /// An aircraft stopped on a runway: its status, then vacate, then the
    /// runway closes until it moves; and closures that have run their time.
    fn runway_closures(&mut self, now: f64, out: &mut Vec<Tx>) {
        let keys: Vec<u64> = self.flights.keys().copied().collect();
        let mut close = vec![];
        for k in keys {
            let Some(mut f) = self.flights.remove(&k) else { continue };
            let pair =
                (f.ac.on_ground && f.ac.gs < 3. && !f.ac.rotary && now - f.seen < 3.)
                    .then(|| self.ap.strip_at(f.ac.p, 0.).map(String::from))
                    .flatten();
            match pair {
                None => f.rwy_stop = None,
                Some(pair) => {
                    let since = *f.rwy_stop.get_or_insert(now);
                    // lined up and waiting is fine, for a while
                    let grace =
                        if matches!(f.phase, Phase::LinedUp | Phase::TakeoffCleared)
                            || f.takeoff_waiting
                        {
                            120.
                        } else {
                            25.
                        };
                    let t = now - since - grace;
                    if t >= 0. && self.has(Pos::Tower) {
                        let name = self.ap.strip_name(&pair, &self.direction).to_string();
                        let sp = self.speaker(&f, Pos::Tower);
                        if !f.is_ai() && f.once("rwy-status") {
                            let tx = format!(
                                "{}, {}, say status, you are stopped on runway {name}.",
                                f.call,
                                self.facility(sp)
                            );
                            self.say(&mut f, sp, tx, now, out);
                        } else if !f.is_ai() && t >= 60. && f.once("rwy-vacate") {
                            let tx = format!("{}, vacate runway {name} at the next available taxiway, report clear of the runway.", f.call);
                            self.say(&mut f, sp, tx, now, out);
                        }
                        if t >= 150. {
                            close.push((pair, f.key));
                        }
                    }
                }
            }
            self.flights.insert(k, f);
        }
        for (pair, by) in close {
            self.close(
                &pair,
                f64::INFINITY,
                "disabled aircraft on the runway",
                Some(by),
                now,
                out,
            );
        }
        // reopen: the time is up, or the aircraft has moved off
        let reopen: Vec<String> = self
            .closed
            .iter()
            .filter(|(pair, c)| {
                now >= c.until
                    || c.by.is_some_and(|k| {
                        self.flights.get(&k).is_none_or(|f| {
                            self.ap.strip_at(f.ac.p, 0.) != Some(pair.as_str())
                        })
                    })
            })
            .map(|(p, _)| p.clone())
            .collect();
        for pair in reopen {
            self.closed.remove(&pair);
            let name = self.ap.strip_name(&pair, &self.direction).to_string();
            log::info!("milatc {}: runway {name} open again", self.ap.callsign);
            let t = format!(
                "All aircraft, {}, runway {name} is open.",
                self.facility(Pos::Tower)
            );
            self.broadcast(Pos::Tower, t, now, out);
        }
    }

    /// Strips that missed the picture: an aircraft gone fast on a runway, or
    /// low over it, has crashed there, and the runway closes for the wreck.
    fn vanished(&mut self, now: f64, out: &mut Vec<Tx>) {
        let mut crashed = vec![];
        for f in self.flights.values_mut() {
            if now - f.seen < 3. || f.vanished {
                continue;
            }
            f.vanished = true;
            let fast_on_rwy = f.ac.on_ground
                && f.ac.gs > 40.
                && self.ap.strip_at(f.ac.p, 30.).is_some();
            let low_over = !f.ac.on_ground
                && f.ac.agl_ft < 200
                && f.ac.vs_fpm < -300
                && self.ap.strip_at(f.ac.p, 150.).is_some();
            if fast_on_rwy || low_over {
                if let Some(pair) = self.ap.strip_at(f.ac.p, 150.) {
                    crashed.push((pair.to_string(), f.call.clone()));
                }
            }
        }
        for (pair, call) in crashed {
            log::warn!(
                "milatc {}: {call} lost on runway {pair}: closing it",
                self.ap.callsign
            );
            self.close(
                &pair,
                now + 600.,
                "crash on the runway, emergency vehicles responding",
                None,
                now,
                out,
            );
        }
    }

    /// Traffic stopped on a taxiway: ask it what is up, and route the flights
    /// behind it around it, or hold them until it moves.
    fn taxiway_blocks(&mut self, now: f64, out: &mut Vec<Tx>) {
        if !self.has(Pos::Ground) {
            return;
        }
        let blockers: Vec<(u64, P2, String)> = self
            .flights
            .values()
            .filter(|f| {
                f.ac.on_ground
                    && !f.ac.rotary
                    && now - f.seen < 3.
                    && f.stop_since.is_some_and(|t| now - t > 45.)
            })
            .filter(|f| {
                self.ap.strip_at(f.ac.p, 20.).is_none()
                    && self.ap.hold_near(f.ac.p, 80.).is_none()
            })
            .filter_map(|f| {
                Some((f.key, f.ac.p, self.ap.taxiway_at(f.ac.p, 20.)?.to_string()))
            })
            .collect();
        let keys: Vec<u64> = self.flights.keys().copied().collect();
        for k in keys {
            let Some(mut f) = self.flights.remove(&k) else { continue };
            // the blocker itself, if it is a player stopped a while
            if let Some((_, _, tw)) = blockers.iter().find(|b| b.0 == f.key) {
                let long = f.stop_since.is_some_and(|t| now - t > 90.);
                if !f.is_ai()
                    && long
                    && matches!(f.phase, Phase::TaxiOut | Phase::TaxiIn)
                    && f.once("tw-status")
                {
                    let t = format!(
                        "{}, {}, say status, you are stopped on taxiway {}.",
                        f.call,
                        self.facility(Pos::Ground),
                        speech::taxiway(tw)
                    );
                    let sp = self.speaker(&f, Pos::Ground);
                    self.say(&mut f, sp, t, now, out);
                }
            }
            // a player behind it
            if !f.is_ai() && matches!(f.phase, Phase::TaxiOut | Phase::TaxiIn) {
                if let Some(b) = f.held_for {
                    if !blockers.iter().any(|x| x.0 == b) {
                        f.held_for = None;
                        let via = f
                            .route
                            .as_ref()
                            .map(|r| {
                                r.via
                                    .iter()
                                    .map(|v| speech::taxiway(v))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            })
                            .unwrap_or_default();
                        let t = if via.is_empty() {
                            format!("{}, traffic has moved, continue taxi.", f.call)
                        } else {
                            format!(
                                "{}, traffic has moved, continue taxi via {via}.",
                                f.call
                            )
                        };
                        self.say(&mut f, Pos::Ground, t, now, out);
                    }
                } else if let Some(route) = f.route.clone() {
                    let near = blockers.iter().find(|b| {
                        b.0 != f.key
                            && b.1.dist(f.ac.p) < 900.
                            && self.ap.route_ahead_passes(&route, f.ac.p, b.1, 35.)
                    });
                    if let Some((bk, bp, tw)) = near.cloned() {
                        let avoid = [(bp, 30.)];
                        let alt_route = match f.phase {
                            Phase::TaxiOut => f.runway.clone().and_then(|r| {
                                self.ap.route_to_runway_avoiding(f.ac.p, &r, &avoid)
                            }),
                            _ => {
                                self.ap.route_to_point_avoiding(f.ac.p, route.end, &avoid)
                            }
                        };
                        let tw = speech::taxiway(&tw);
                        match alt_route {
                            Some(nr)
                                if nr.via != route.via
                                    && nr.length_m < route.length_m * 2.5 + 400. =>
                            {
                                let via = nr
                                    .via
                                    .iter()
                                    .map(|v| speech::taxiway(v))
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                let tail = match (f.phase, f.runway.clone()) {
                                    (Phase::TaxiOut, Some(r)) => {
                                        format!(", hold short runway {r}")
                                    }
                                    _ => String::new(),
                                };
                                let t = format!("{}, amend taxi instructions, traffic stopped on {tw}, taxi via {via}{tail}.", f.call);
                                self.say(&mut f, Pos::Ground, t, now, out);
                                f.route = Some(nr);
                            }
                            _ => {
                                let t = format!("{}, hold position, traffic stopped on {tw} ahead, expect a short delay.", f.call);
                                self.say(&mut f, Pos::Ground, t, now, out);
                                f.held_for = Some(bk);
                            }
                        }
                    }
                }
            }
            self.flights.insert(k, f);
        }
    }
}

// ─── ground traffic and radar traffic calls ───────────────────────────────

/// Knots to metres a second.
const KT: f64 = 0.5144;

/// Where a moving aircraft will be in `t` seconds, on its heading.
fn ahead_of(a: &Ac, t: f64) -> P2 {
    a.p.offset(a.hdg, a.gs * KT * t)
}

/// "left to right" / "right to left", as `me` sees `other` crossing ahead.
fn crossing_way(me: &Ac, other: &Ac) -> &'static str {
    // which side it is on now, relative to my heading
    let rel = angdiff(me.p.bearing_to(other.p), me.hdg);
    if rel < 0. {
        "left to right"
    } else {
        "right to left"
    }
}

/// The o'clock of `other` from `me`.
fn oclock(me: &Ac, other: &Ac) -> i32 {
    let rel = (me.p.bearing_to(other.p) - me.hdg).rem_euclid(360.);
    let h = ((rel / 30.).round() as i32) % 12;
    if h == 0 {
        12
    } else {
        h
    }
}

impl Controller {
    /// Players taxiing: hold them for traffic crossing ahead or coming the
    /// other way, let them go once it has passed, and tell them who to
    /// follow.
    fn ground_traffic(&mut self, now: f64, out: &mut Vec<Tx>) {
        if !self.has(Pos::Ground) {
            return;
        }
        let movers: Vec<Ac> = self
            .traffic
            .iter()
            .filter(|a| a.on_ground && a.gs > 3. && !a.rotary)
            .cloned()
            .collect();
        let keys: Vec<u64> = self.flights.keys().copied().collect();
        for k in keys {
            let Some(mut f) = self.flights.remove(&k) else { continue };
            if !f.is_ai()
                && f.ac.on_ground
                && matches!(f.phase, Phase::TaxiOut | Phase::TaxiIn)
                && f.freq != Some(Pos::Tower)
            {
                self.give_way(&mut f, &movers, now, out);
            }
            self.flights.insert(k, f);
        }
    }

    /// The traffic `f` must give way to, if any: (who, "crossing left to
    /// right" / "opposite direction", where).
    fn conflict_for(&self, f: &Flight, movers: &[Ac]) -> Option<(Ac, String)> {
        let route = f.route.as_ref()?;
        let path = self.ap.path_ahead(route, f.ac.p, 300.);
        if path.len() < 2 {
            return None;
        }
        let near = |q: P2| {
            path.windows(2).map(|w| seg_dist(q, w[0], w[1])).fold(f64::INFINITY, f64::min)
        };
        // how far along my path a point is
        let along = |q: P2| {
            let mut run = 0.;
            let mut best = (f64::INFINITY, 0.);
            for w in path.windows(2) {
                let d = seg_dist(q, w[0], w[1]);
                if d < best.0 {
                    best = (d, run + w[0].dist(q).min(w[0].dist(w[1])));
                }
                run += w[0].dist(w[1]);
            }
            best.1
        };
        let my_speed = (f.ac.gs * KT).max(4.);
        movers
            .iter()
            .filter(|o| o.unit_id != f.key && o.p.dist(f.ac.p) < 600.)
            .filter_map(|o| {
                let diff = angdiff(o.hdg, f.ac.hdg).abs();
                // where it will be over the next 20 s, and the first time it is on my path
                let hit = (0..=10)
                    .map(|i| i as f64 * 2.)
                    .find(|t| near(ahead_of(o, *t)) < 35.)?;
                let at = ahead_of(o, hit);
                let d_me = along(at);
                if diff < 45. {
                    return None; // going my way: following, not a conflict
                }
                // it gets there first, or near enough together
                if hit > d_me / my_speed + 8. {
                    return None;
                }
                let tw = self
                    .ap
                    .taxiway_at(at, 40.)
                    .map(|t| format!(" on {}", speech::taxiway(t)))
                    .unwrap_or_default();
                let what = if diff > 135. {
                    "opposite direction traffic".to_string()
                } else {
                    format!("traffic crossing {}", crossing_way(&f.ac, o))
                };
                let typ = type_phrase(&o.typ);
                let typ = typ.trim_end_matches(" on a");
                Some((o.clone(), format!("{what}{typ}{tw}")))
            })
            .next()
    }

    fn give_way(&self, f: &mut Flight, movers: &[Ac], now: f64, out: &mut Vec<Tx>) {
        let conflict = self.conflict_for(f, movers);
        match (f.give_way, conflict) {
            (None, Some((o, why))) => {
                // both players: only the one further from it holds
                if let Some(other) = self.flights.get(&o.unit_id) {
                    if !other.is_ai() && other.give_way.is_some_and(|g| g.0 == f.key) {
                        return;
                    }
                }
                let t = format!("{}, hold position, {why}.", f.call);
                let sp = self.speaker(f, Pos::Ground);
                self.say(f, sp, t, now, out);
                f.give_way = Some((o.unit_id, now));
            }
            (Some((who, since)), c) => {
                let still = c.as_ref().is_some_and(|(o, _)| o.unit_id == who);
                if (!still && now - since > 4.) || now - since > 120. {
                    f.give_way = None;
                    let t = format!(
                        "{}, {}",
                        f.call,
                        self.pick(&[
                            "traffic has passed, continue taxi.",
                            "continue taxi.",
                            "traffic is clear, continue taxi."
                        ])
                    );
                    let sp = self.speaker(f, Pos::Ground);
                    self.say(f, sp, t, now, out);
                }
            }
            (None, None) => {
                // following someone along the same taxiway
                let Some(route) = f.route.as_ref() else { return };
                let lead = movers
                    .iter()
                    .filter(|o| o.unit_id != f.key && !f.followed.contains(&o.unit_id))
                    .filter(|o| {
                        angdiff(o.hdg, f.ac.hdg).abs() < 40.
                            && o.p.dist(f.ac.p) < 350.
                            && o.p.dist(f.ac.p) > 40.
                    })
                    .find(|o| self.ap.route_ahead_passes(route, f.ac.p, o.p, 30.))
                    .cloned();
                if let Some(o) = lead {
                    f.followed.insert(o.unit_id);
                    if f.ac.gs > 3. {
                        let typ = type_name(&o.typ).unwrap_or("traffic");
                        let t = format!("{}, follow the {typ} ahead.", f.call);
                        let sp = self.speaker(f, Pos::Ground);
                        self.say(f, sp, t, now, out);
                    }
                }
            }
        }
    }

    /// Radar traffic calls to players working Approach or Departure: other
    /// aircraft within 4 miles and 1,500 feet that are closing.
    fn traffic_advisories(&mut self, now: f64, out: &mut Vec<Tx>) {
        self.advised.retain(|_, t| now - *t < 300.);
        let air: Vec<Ac> = self
            .traffic
            .iter()
            .filter(|a| !a.on_ground && a.agl_ft > 300)
            .cloned()
            .collect();
        let keys: Vec<u64> = self.flights.keys().copied().collect();
        for k in keys {
            let Some(mut f) = self.flights.remove(&k) else { continue };
            let radar = matches!(f.freq, Some(Pos::Approach) | Some(Pos::Departure))
                && matches!(f.phase, Phase::Arrival | Phase::Climbout)
                && !f.is_ai()
                && !f.ac.on_ground;
            if radar {
                let pos = f.freq.unwrap_or(Pos::Approach);
                let call = air
                    .iter()
                    .filter(|o| {
                        o.unit_id != f.key
                            && !self.advised.contains_key(&(f.key, o.unit_id))
                    })
                    .filter(|o| {
                        (o.alt_ft - f.ac.alt_ft).abs() < 1500
                            && o.p.dist(f.ac.p) < 4. * NM
                    })
                    // closing: nearer in 20 s than now
                    .filter(|o| {
                        ahead_of(o, 20.).dist(ahead_of(&f.ac, 20.)) < o.p.dist(f.ac.p)
                    })
                    .min_by(|a, b| a.p.dist(f.ac.p).total_cmp(&b.p.dist(f.ac.p)))
                    .cloned();
                if let Some(o) = call {
                    if self.quiet(pos, now, 4.) {
                        self.advised.insert((f.key, o.unit_id), now);
                        let d = ((o.p.dist(f.ac.p) / NM).round() as i32).max(1);
                        let diff = angdiff(o.hdg, f.ac.hdg).abs();
                        let dir = if diff > 135. {
                            "opposite direction".to_string()
                        } else if diff < 45. {
                            "same direction".to_string()
                        } else {
                            let way = if angdiff(o.hdg, f.ac.hdg) > 0. {
                                "left to right"
                            } else {
                                "right to left"
                            };
                            format!("crossing {way}")
                        };
                        let typ = type_name(&o.typ)
                            .map(|t| format!(", a {t}"))
                            .unwrap_or_default();
                        let t = format!(
                            "{}, traffic, {} o'clock, {d} mile{}, {dir}{typ}, {}.",
                            f.call,
                            oclock(&f.ac, &o),
                            if d == 1 { "" } else { "s" },
                            alt(((o.alt_ft + 50) / 100) * 100)
                        );
                        self.say(&mut f, pos, t, now, out);
                    }
                }
            }
            self.flights.insert(k, f);
        }
    }
}

impl Controller {
    /// Why an AI departure must stay on its stand: the runway or the final is
    /// busy, or a player is working the pattern.
    fn ai_departure_blocked(&self, f: &Flight, runway: &str) -> Option<String> {
        if let Some((a, d)) = self.arrival_within(runway, 6., f.key) {
            return Some(format!(
                "traffic{} {} mile final",
                type_phrase(&a.typ),
                d.ceil().max(1.) as i32
            ));
        }
        let r = self.ap.runway(runway)?;
        if self.is_closed(&r.pair).is_some() {
            return Some("the runway is closed".into());
        }
        if self.runway_occupant(&r.pair, f.key).is_some() {
            return Some("traffic on the runway".into());
        }
        let busy = self.flights.values().any(|o| {
            !o.is_ai()
                && matches!(
                    o.phase,
                    Phase::RadarFinal
                        | Phase::Pattern
                        | Phase::TakeoffCleared
                        | Phase::LinedUp
                )
        });
        if busy {
            return Some("traffic in the pattern".into());
        }
        // a player taxiing where it would go
        let path = self.ap.route_to_runway(f.ac.p, runway)?;
        let ahead = self.ap.path_ahead(&path, f.ac.p, 1500.);
        self.flights
            .values()
            .filter(|o| {
                o.key != f.key
                    && !o.is_ai()
                    && o.ac.on_ground
                    && matches!(o.phase, Phase::TaxiOut | Phase::TaxiIn)
            })
            .find(|o| {
                ahead.windows(2).any(|w| seg_dist(o.ac.p, w[0], w[1]) < 60.)
                    || o.route.as_ref().is_some_and(|r| {
                        let theirs = self.ap.path_ahead(r, o.ac.p, 600.);
                        theirs.iter().any(|q| {
                            ahead.windows(2).any(|w| seg_dist(*q, w[0], w[1]) < 40.)
                        })
                    })
            })
            .map(|o| match self.ap.taxiway_at(o.ac.p, 40.) {
                Some(tw) => format!("traffic taxiing on {}", speech::taxiway(tw)),
                None => "traffic taxiing".into(),
            })
    }

    /// Hold an AI arrival while a player is on the approach or in the
    /// pattern, and let it go again. Returns true while it is holding (no
    /// other Approach business with it).
    fn ai_sequencing(
        &self,
        f: &mut Flight,
        field_nm: f64,
        now: f64,
        out: &mut Vec<Tx>,
    ) -> bool {
        let conflict = self.flights.values().any(|o| {
            !o.is_ai()
                && !o.ac.on_ground
                && matches!(o.phase, Phase::RadarFinal | Phase::Pattern | Phase::Arrival)
                && self
                    .ap
                    .runway(&self.arr_runway())
                    .is_some_and(|r| o.ac.p.dist(r.thr) / NM < field_nm.min(12.))
        });
        match f.ai_hold_since {
            None => {
                if conflict
                    && field_nm > 14.
                    && f.ac.ai_state == "active"
                    && f.once("ai-hold")
                {
                    let t = format!(
                        "{}, hold present position, left turns, maintain {}, expect further clearance in {} minutes.",
                        f.call,
                        alt(((f.ac.alt_ft + 50) / 1000).max(3) * 1000),
                        self.pick(&["three", "four", "five"])
                    );
                    self.say(f, Pos::Approach, t, now, out);
                    self.command(f, "hold", out);
                    f.ai_hold_since = Some(now);
                    f.ai_clear_since = None;
                    return true;
                }
                false
            }
            Some(since) => {
                if conflict {
                    f.ai_clear_since = None;
                } else {
                    f.ai_clear_since.get_or_insert(now);
                }
                let clear_long = f.ai_clear_since.is_some_and(|t| now - t > 30.);
                if (clear_long && now - since > 60.) || now - since > 360. {
                    let rwy = self.arr_runway();
                    let t = format!(
                        "{}, cleared direct {}, descend and maintain {}, expect a visual approach runway {rwy}.",
                        f.call,
                        self.ap.callsign,
                        alt(self.ap.arrival.approach_alt_ft)
                    );
                    self.say(f, Pos::Approach, t, now, out);
                    self.command(f, "resume", out);
                    f.ai_hold_since = None;
                    return false;
                }
                true
            }
        }
    }

    /// What an AI pilot says unprompted, as the flight goes along: the
    /// requests a real crew would make, each answered by the controller the
    /// same way a player's would be.
    fn drive_ai(&mut self, f: &mut Flight, now: f64, out: &mut Vec<Tx>) {
        let Some(plan) = f.ac.ai else { return };
        // Ground's hold on a departure: release it once the runway clears.
        if f.ai_taxi_hold && self.quiet(Pos::Ground, now, 10.) {
            let rwy = self.dep_runway();
            if self.ai_departure_blocked(f, &rwy).is_none() {
                let p = Parsed::of(Intent::RequestTaxi);
                self.taxi(f, Pos::Ground, &p, now, out);
            }
            return;
        }
        if now < f.ai_next {
            return;
        }
        let letter = self.atis_letter();
        let rwy_arr = self.arr_runway();
        let quiet = |c: &Self, p: Pos| c.quiet(p, now, 6.);
        let thr = self.ap.runway(&rwy_arr).map(|r| r.thr).unwrap_or_default();
        let d_nm = (f.ac.p.dist(thr) / NM).round() as i32;
        let dir = speech::compass(thr.bearing_to(f.ac.p));
        match (plan, f.phase) {
            (AiPlan::Departure, Phase::Ground)
                if f.ac.ai_state == "waiting" && !f.ai_taxi_hold =>
            {
                if quiet(self, Pos::Ground) && f.once("ai-taxi") {
                    let ramp = self
                        .ap
                        .ramp_for(f.ac.p, None)
                        .map(|r| r.name.clone())
                        .unwrap_or_else(|| "the ramp".into());
                    let t = format!(
                        "{}, {}{}, {ramp}, with {letter}, request taxi.",
                        self.facility(Pos::Ground),
                        f.call,
                        spoken_type(&f.ac.typ)
                    );
                    self.pilot_says(f, Pos::Ground, t, now, out);
                    let p = Parsed::of(Intent::RequestTaxi);
                    self.taxi(f, Pos::Ground, &p, now, out);
                }
            }
            (AiPlan::Departure, Phase::TaxiOut | Phase::HoldingShort) => {
                // Near the runway: Ground hands it over, and it calls ready.
                let rwy = f.runway.clone().unwrap_or_else(|| self.dep_runway());
                let near = self.ap.runway(&rwy).is_some_and(|r| {
                    let (along, cross) = r.frame(f.ac.p);
                    along < 900. && cross.abs() < 250.
                });
                if near
                    && f.freq == Some(Pos::Ground)
                    && self.has(Pos::Tower)
                    && f.once("ai-to-tower")
                {
                    self.handoff(f, Pos::Ground, Pos::Tower, now, out);
                    f.ai_next = now + self.between(4., 8.);
                } else if f.freq == Some(Pos::Tower)
                    && quiet(self, Pos::Tower)
                    && f.once("ai-ready")
                {
                    let t = format!(
                        "{}, {}, holding short {rwy}, ready for departure.",
                        self.facility(Pos::Tower),
                        f.call
                    );
                    self.pilot_says(f, Pos::Tower, t, now, out);
                    let p = Parsed::of(Intent::ReadyForDeparture);
                    self.ready(f, Pos::Tower, &p, now, out);
                }
            }
            (AiPlan::Departure, Phase::Departure) => {
                if now - f.t_handoff > 4.
                    && quiet(self, Pos::Departure)
                    && f.once("ai-dep")
                {
                    let t = format!(
                        "{}, {}, passing {} for {}.",
                        self.facility(Pos::Departure),
                        f.call,
                        alt(((f.ac.alt_ft + 50) / 100) * 100),
                        alt(self.ap.departure.initial_alt_ft)
                    );
                    self.pilot_says(f, Pos::Departure, t, now, out);
                    let p = Parsed::of(Intent::CheckIn);
                    self.check_in(f, Pos::Departure, &p, now, out);
                }
            }
            (AiPlan::Arrival, Phase::Enroute) => {
                if self.has(Pos::Approach)
                    && quiet(self, Pos::Approach)
                    && f.once("ai-app")
                {
                    let t = format!(
                        "{}, {}{}, {d_nm} miles {dir}, {}, inbound full stop with {letter}.",
                        self.facility(Pos::Approach),
                        f.call,
                        spoken_type(&f.ac.typ),
                        alt(((f.ac.alt_ft + 500) / 1000) * 1000)
                    );
                    self.pilot_says(f, Pos::Approach, t, now, out);
                    let mut p = Parsed::of(Intent::RequestApproach);
                    p.approach = Approach::Visual;
                    p.option = LandingOption::FullStop;
                    self.check_in(f, Pos::Approach, &p, now, out);
                }
            }
            (AiPlan::Arrival, Phase::Pattern) => {
                if f.freq == Some(Pos::Tower)
                    && now - f.t_handoff > 4.
                    && quiet(self, Pos::Tower)
                    && f.once("ai-twr")
                {
                    let t = format!(
                        "{}, {}{}, {d_nm} miles {dir}, full stop.",
                        self.facility(Pos::Tower),
                        f.call,
                        spoken_type(&f.ac.typ)
                    );
                    self.pilot_says(f, Pos::Tower, t, now, out);
                    let mut p = Parsed::of(Intent::CheckIn);
                    p.option = LandingOption::FullStop;
                    self.check_in(f, Pos::Tower, &p, now, out);
                }
            }
            (AiPlan::Arrival, Phase::Ground)
                if f.freq == Some(Pos::Ground)
                    && quiet(self, Pos::Ground)
                    && f.once("ai-in") =>
            {
                let t = format!(
                    "{}, {}, clear of {rwy_arr}, taxi to parking.",
                    self.facility(Pos::Ground),
                    f.call
                );
                self.pilot_says(f, Pos::Ground, t, now, out);
                let p = Parsed::of(Intent::ClearOfRunway);
                self.taxi_to_parking(f, Pos::Ground, &p, now, out);
            }
            _ => {}
        }
    }
}

/// What a pilot reads back of a controller's transmission: the clearances,
/// runways, altitudes, headings and frequencies, without the controller's
/// callsign, the weather or the pleasantries, and his own callsign last.
pub(crate) fn readback(text: &str, call: &str, field: &str, r: f64) -> Option<String> {
    let mut t = text.trim().trim_end_matches('.').to_string();
    if let Some(rest) = t.strip_prefix(&format!("{call}, ")) {
        t = rest.to_string();
    }
    // the controller's own callsign
    if t.starts_with(field) {
        if let Some(i) = t.find(", ") {
            t = t[i + 2..].to_string();
        }
    }
    let lower = t.to_lowercase();
    // Transmissions that get only an acknowledgement.
    if lower.starts_with("roger")
        || lower.starts_with("standby")
        || lower.starts_with("stand by")
        || lower.starts_with("copy")
        || lower.contains("say again")
        || lower.contains("how do you hear")
        || lower.contains("loud and clear")
    {
        return None;
    }
    let mut keep: Vec<String> = vec![];
    for clause in t.split(", ") {
        let c = clause.trim();
        let cl = c.to_lowercase();
        let drop = cl.starts_with("wind ")
            || cl.starts_with("wind calm")
            || cl.starts_with("altimeter")
            || cl.starts_with("radar contact")
            || cl.starts_with("check wheels")
            || cl.starts_with("good day")
            || cl.starts_with("have a good")
            || cl.starts_with("see you")
            || cl.starts_with("expect further")
            || cl.starts_with("emergency equipment")
            || cl.is_empty();
        if drop {
            continue;
        }
        // "contact Andersen Tower 340.2" → "Tower 340.2" (or just the number)
        if let Some(rest) = c.strip_prefix("contact ") {
            let rest = rest.strip_prefix(&format!("{field} ")).unwrap_or(rest);
            keep.push(if r < 0.5 {
                format!("over to {rest}")
            } else {
                rest.split(' ').next_back().unwrap_or(rest).to_string()
            });
            continue;
        }
        keep.push(c.replace("runway ", ""));
    }
    if keep.is_empty() {
        return Some(format!("{}, {call}", if r < 0.5 { "Roger" } else { "Wilco" }));
    }
    let mut s = keep.join(", ");
    if let Some(f) = s.get_mut(0..1) {
        f.make_ascii_uppercase();
    }
    let tail = if lower.contains("radar service terminated") && r > 0.4 {
        ", good day"
    } else {
        ""
    };
    Some(format!("{s}, {call}{tail}."))
}

/// Heavies, for wake turbulence.
fn heavy(dcs_type: &str) -> bool {
    let t = dcs_type.to_uppercase();
    [
        "C-17", "KC-135", "KC135", "KC-10", "B-52", "B-1", "E-3", "C-5", "IL-76",
        "IL-78", "A-50", "TU-",
    ]
    .iter()
    .any(|h| t.starts_with(h))
}

/// What pilots and controllers call a DCS type: "C-17", "Hornet". `None`
/// when the type means nothing said over the radio.
fn type_name(dcs_type: &str) -> Option<&'static str> {
    let t = dcs_type.to_uppercase();
    Some(if t.starts_with("C-17") {
        "C-17"
    } else if t.starts_with("KC-135") || t.starts_with("KC135") {
        "KC-135"
    } else if t.starts_with("KC-10") {
        "KC-10"
    } else if t.starts_with("C-130") {
        "C-130"
    } else if t.starts_with("B-52") {
        "B-52"
    } else if t.starts_with("B-1") {
        "B-1"
    } else if t.starts_with("F-16") {
        "F-16"
    } else if t.starts_with("FA-18") || t.starts_with("F/A-18") {
        "Hornet"
    } else if t.starts_with("F-15") {
        "F-15"
    } else if t.starts_with("F-14") {
        "Tomcat"
    } else if t.starts_with("A-10") {
        "A-10"
    } else if t.starts_with("AV8") {
        "Harrier"
    } else if t.starts_with("A-4") {
        "A-4"
    } else if t.starts_with("M-2000") || t.starts_with("MIRAGE") {
        "Mirage"
    } else if t.starts_with("MIG-29") {
        "MiG-29"
    } else if t.starts_with("SU-27") || t.starts_with("SU-33") {
        "Flanker"
    } else if t.starts_with("UH-1") {
        "Huey"
    } else if t.starts_with("AH-64") {
        "Apache"
    } else if t.starts_with("CH-47") {
        "Chinook"
    } else if t.starts_with("MI-8") {
        "Hip"
    } else if t.starts_with("E-3") {
        "AWACS"
    } else {
        return None;
    })
}

/// The type for the transcript: the radio name when there is one, else the
/// DCS type as it is ("JF-17").
pub(crate) fn type_label(dcs_type: &str) -> Option<String> {
    let t = dcs_type.trim();
    if t.is_empty() {
        return None;
    }
    Some(type_name(t).map(str::to_string).unwrap_or_else(|| t.replace('_', " ")))
}

/// ", C-17" after a callsign on first contact, or nothing.
pub(crate) fn spoken_type(dcs_type: &str) -> String {
    type_name(dcs_type).map(|n| format!(", {n}")).unwrap_or_default()
}

/// ", a C-17 on a" / ", an F-16 on a" for a traffic call; "" when the type
/// means nothing to a listener.
fn type_phrase(dcs_type: &str) -> String {
    let Some(name) = type_name(dcs_type) else { return String::new() };
    let article = if name.starts_with(['A', 'E', 'F', 'H'])
        && !name.starts_with("Ho")
        && !name.starts_with("Ha")
    {
        "an"
    } else {
        "a"
    };
    format!(", {article} {name} on a")
}

pub(crate) fn norm(s: &str) -> String {
    s.chars().filter(|c| c.is_alphanumeric()).collect::<String>().to_lowercase()
}

pub(crate) fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::milatc::airport::BUILTIN_ANDERSEN;
    use crate::milatc::intent::parse;

    fn field(ap: &Airport) -> AtcAirfield {
        let r = ap.runway("06R").unwrap();
        let (lat, lon) = (13.5743, 144.9140);
        let _ = r;
        AtcAirfield {
            id: "Andersen AFB".into(),
            name: "Andersen AFB".into(),
            lat,
            lon,
            elev_ft: 612,
            kind: "airbase".into(),
            runways: vec![],
            active_runway: None,
            brc: None,
            wind_from_deg: 70,
            wind_speed_kts: 12,
            qnh_inhg: 29.92,
            qnh_hpa: 1013.,
            qfe_inhg: 29.3,
            qfe_hpa: 992.,
            temp_c: 28,
            dewpoint_c: 22,
            visibility_m: 10000,
            cloud_base_ft: None,
            cloud_cover: None,
            precipitation: false,
            recovery_case: None,
            logi: 100,
            health: 100,
            supply: 100,
            fuel: 100,
            threatened: false,
            open: true,
        }
    }

    /// A controller over Andersen with its network trusted as-is, and a way
    /// to put aircraft at airfield-local positions.
    struct World {
        c: Controller,
        f: AtcAirfield,
        traffic: Vec<AtcTraffic>,
        now: f64,
    }

    impl World {
        fn new() -> Self {
            let mut ap = Airport::from_json(BUILTIN_ANDERSEN).unwrap();
            let dcs: Vec<(String, P2)> =
                ap.runways.iter().map(|r| (r.name.clone(), r.thr)).collect();
            ap.align(&dcs).unwrap();
            let f = field(&ap);
            let mut c = Controller::new(ap, CtlCfg::default());
            c.seed(42);
            c.standby_odds = 0.;
            World { c, f, traffic: vec![], now: 0. }
        }

        fn ll(&self, p: P2) -> (f64, f64) {
            let lat0 = 13.5792862;
            let lon0 = 144.9130588;
            let k_lat = 111_132.;
            let k_lon = 111_320. * f64::to_radians(lat0).cos();
            (lat0 + p.n / k_lat, lon0 + p.e / k_lon)
        }

        fn put(
            &mut self,
            id: u64,
            p: P2,
            alt_ft: i32,
            hdg: f64,
            gs: f64,
            on_ground: bool,
        ) {
            let (lat, lon) = self.ll(p);
            self.traffic.retain(|t| t.unit_id != Some(id));
            self.traffic.push(AtcTraffic {
                ucid: format!("u{id}"),
                unit_id: Some(id),
                callsign: "Viper 1-1".into(),
                player_name: format!("pilot{id}"),
                lat,
                lon,
                alt_ft,
                agl_ft: alt_ft - 612,
                heading: hdg as u16,
                speed_kts: gs as u16,
                vspd_fpm: 0,
                on_ground,
                field: None,
                field_brg: 0,
                field_rng_m: 0,
                rotary: false,
                ai: false,
                ai_plan: None,
                ai_state: None,
                unit_type: "F-16C_50".into(),
            });
        }

        fn tick(&mut self) -> Vec<Tx> {
            self.now += 1.;
            let t = self.traffic.clone();
            self.c.update(&self.f.clone(), &t, self.now)
        }

        fn say(&mut self, id: u64, pos: Pos, text: &str) -> Vec<Tx> {
            let p = parse(text, "Andersen");
            self.c.respond(
                pos,
                &Caller { unit_id: Some(id), srs_name: format!("pilot{id}") },
                text,
                p,
                self.now,
            )
        }
    }

    fn texts(v: &[Tx]) -> Vec<String> {
        v.iter().map(|t| t.text.clone()).collect()
    }

    #[test]
    fn a_full_departure() {
        let mut w = World::new();
        let ramp = w.c.ap.ramps.iter().find(|r| r.name == "south ramp").unwrap().p;
        w.put(1, ramp, 612, 0., 0., true);
        w.tick();
        assert_eq!(w.c.direction, "06");

        // Clearance on request, then a call back when it is ready.
        let r = w.say(
            1,
            Pos::Clearance,
            "Andersen Clearance, Viper 11, request clearance to the range",
        );
        assert!(
            texts(&r)[0].contains("standby") || texts(&r)[0].contains("on request"),
            "{r:?}"
        );
        let mut ready = vec![];
        for _ in 0..30 {
            ready.extend(texts(&w.tick()));
        }
        assert!(
            ready.iter().any(|t| t.contains("advise") && t.contains("ready to copy")),
            "{ready:?}"
        );
        let r = w.say(1, Pos::Clearance, "Viper 11, ready to copy");
        assert!(
            texts(&r)[0].contains("cleared to") && texts(&r)[0].contains("squawk 4201"),
            "{r:?}"
        );
        let r = w.say(1, Pos::Clearance, "cleared to the range, maintain 4,000, departure 363.8, squawk 4201, Viper 11");
        assert!(texts(&r)[0].contains("readback correct"), "{r:?}");

        let r = w.say(1, Pos::Ground, "Andersen Ground, Viper 11, request taxi");
        let t = &texts(&r)[0];
        assert!(t.starts_with("Viper 1-1, Andersen Ground, runway 06R, taxi via"), "{t}");
        assert!(t.contains("hold short runway 06R"), "{t}");
        let hold = w.c.flights[&1].route.as_ref().unwrap().hold.unwrap();
        let hp = w.c.ap.node(w.c.ap.holds[hold].node);

        // A readback without the hold short gets challenged.
        let r = w.say(1, Pos::Ground, "taxi via Bravo, Echo, Viper 11");
        assert!(texts(&r)[0].contains("read back hold short"), "{r:?}");

        // Arrive at the hold: Ground hands off to Tower.
        w.put(1, hp, 612, 66., 0., true);
        let r = w.tick();
        assert!(
            texts(&r).iter().any(|t| t.contains("contact Andersen Tower 340.2")),
            "{r:?}"
        );

        // Traffic on a 2 mile final: hold short.
        let thr = w.c.ap.runway("06R").unwrap().thr;
        w.put(2, thr.offset(246., 2. * NM), 1300, 66., 150., false);
        w.tick();
        let r = w.say(
            1,
            Pos::Tower,
            "Andersen Tower, Viper 11, holding short 06R, ready for departure",
        );
        assert!(
            texts(&r)[0]
                .contains("hold short runway 06R, traffic, an F-16 on a 2 mile final"),
            "{r:?}"
        );
        // It lands and clears; the takeoff clearance follows unprompted.
        w.traffic.retain(|t| t.unit_id != Some(2));
        let r = w.tick();
        assert!(texts(&r).iter().any(|t| t.contains("cleared for takeoff")), "{r:?}");

        // Airborne and climbing: Tower hands to Departure.
        let rw = w.c.ap.runway("06R").unwrap().clone();
        w.put(1, rw.thr.offset(66., 2500.), 612 + 800, 66., 200., false);
        let r = w.tick();
        assert!(
            texts(&r).iter().any(|t| t.contains("contact Andersen Departure 363.8")),
            "{r:?}"
        );
        let r = w.say(
            1,
            Pos::Departure,
            "Andersen Departure, Viper 11, passing 2,000 for 4,000",
        );
        assert!(texts(&r)[0].contains("radar contact"), "{r:?}");
        // Far out: released.
        w.put(1, rw.thr.offset(66., 30. * NM), 14000, 66., 400., false);
        let r = w.tick();
        assert!(
            texts(&r).iter().any(|t| t.contains("radar service terminated")),
            "{r:?}"
        );
    }

    #[test]
    fn clearance_and_taxi_get_standby_when_busy() {
        let mut w = World::new();
        let ramp = w.c.ap.ramps.iter().find(|r| r.name == "south ramp").unwrap().p;
        w.put(1, ramp, 612, 0., 0., true);
        w.put(2, ramp.offset(90., 60.), 612, 0., 0., true);
        w.tick();
        let r = w.say(1, Pos::Ground, "Andersen Ground, Viper 11, request taxi");
        assert!(texts(&r)[0].contains("taxi"), "{r:?}");
        // straight after: Ground is still talking
        let r = w.say(2, Pos::Ground, "Andersen Ground, Viper 12, request taxi");
        assert!(texts(&r)[0].contains("stand"), "{r:?}");
        let mut later = vec![];
        for _ in 0..30 {
            later.extend(texts(&w.tick()));
        }
        assert!(later.iter().any(|t| t.contains("hold short runway 06R")), "{later:?}");
    }

    #[test]
    fn stopped_on_the_runway_is_asked_then_the_runway_closes() {
        let mut w = World::new();
        let r = w.c.ap.runway("06R").unwrap().clone();
        w.put(1, r.thr.offset(r.hdg, 1200.), 612, r.hdg, 0., true);
        let mut said = vec![];
        for _ in 0..200 {
            said.extend(texts(&w.tick()));
        }
        assert!(
            said.iter().any(|t| t.contains("say status, you are stopped on runway 06R")),
            "{said:?}"
        );
        assert!(said.iter().any(|t| t.contains("vacate runway 06R")), "{said:?}");
        assert!(said.iter().any(|t| t.contains("runway 06R is closed")), "{said:?}");
        assert_eq!(w.c.dep_runway(), "06L");
        // it moves off: open again
        w.put(1, r.thr.offset(r.hdg + 90., 400.), 612, r.hdg, 0., true);
        let said: Vec<String> = (0..3).flat_map(|_| texts(&w.tick())).collect();
        assert!(said.iter().any(|t| t.contains("runway 06R is open")), "{said:?}");
        assert_eq!(w.c.dep_runway(), "06R");
    }

    #[test]
    fn a_crash_on_the_runway_closes_it() {
        let mut w = World::new();
        let r = w.c.ap.runway("06R").unwrap().clone();
        w.put(1, r.thr.offset(r.hdg, 800.), 612, r.hdg, 120., true);
        w.tick();
        w.traffic.clear();
        let said: Vec<String> = (0..5).flat_map(|_| texts(&w.tick())).collect();
        assert!(
            said.iter().any(|t| t.contains("runway 06R is closed, crash on the runway")),
            "{said:?}"
        );
        assert!(w.c.atis_text().contains("Runway 06R closed"));
    }

    #[test]
    fn traffic_stopped_on_the_taxiway_is_routed_around_or_held() {
        let mut w = World::new();
        let ramp = w.c.ap.ramps.iter().find(|r| r.name == "south ramp").unwrap().p;
        w.put(1, ramp, 612, 0., 0., true);
        w.tick();
        w.say(1, Pos::Ground, "Andersen Ground, Viper 11, request taxi");
        let route = w.c.flights[&1].route.clone().unwrap();
        // somewhere along its route, on a taxiway, clear of holds and runways
        let spot = route
            .nodes
            .iter()
            .map(|&n| w.c.ap.node(n))
            .find(|&p| {
                p.dist(ramp) > 150.
                    && p.dist(ramp) < 800.
                    && w.c.ap.taxiway_at(p, 20.).is_some()
                    && w.c.ap.hold_near(p, 80.).is_none()
                    && w.c.ap.strip_at(p, 20.).is_none()
            })
            .expect("a spot on the route");
        w.put(2, spot, 612, 0., 0., true);
        let said: Vec<String> = (0..60).flat_map(|_| texts(&w.tick())).collect();
        assert!(
            said.iter().any(|t| t.starts_with("Viper 1-1")
                && (t.contains("amend taxi instructions")
                    || t.contains("hold position, traffic stopped"))),
            "{said:?}"
        );
    }

    #[test]
    fn taxi_to_24l_names_the_route_and_the_holding_point() {
        let mut w = World::new();
        w.f.wind_from_deg = 245;
        for (i, ramp) in w.c.ap.ramps.clone().iter().enumerate() {
            let id = 10 + i as u64;
            w.put(id, ramp.p, 612, 0., 0., true);
            w.tick();
            assert_eq!(w.c.direction, "24");
            let r = w.say(id, Pos::Ground, "Andersen Ground, Viper 11, request taxi");
            let t = &texts(&r)[0];
            assert!(t.contains("runway 24L, taxi via "), "{}: {t}", ramp.name);
            assert!(!t.contains("taxi to and hold short"), "{}: {t}", ramp.name);
            assert!(t.contains("hold short runway"), "{}: {t}", ramp.name);
            w.traffic.clear();
            w.tick();
            w.now += 60.;
        }
    }

    #[test]
    fn taxiing_traffic_crossing_ahead_is_given_way_to() {
        let mut w = World::new();
        let ramp = w.c.ap.ramps.iter().find(|r| r.name == "south ramp").unwrap().p;
        w.put(1, ramp, 612, 0., 0., true);
        w.tick();
        w.say(1, Pos::Ground, "Andersen Ground, Viper 11, request taxi");
        let route = w.c.flights[&1].route.clone().unwrap();
        let path = w.c.ap.path_ahead(&route, ramp, 300.);
        // a point about 150 m along, and someone coming across it
        let (mut run, mut q, mut seg) = (0., path[0], 0.);
        for win in path.windows(2) {
            let l = win[0].dist(win[1]);
            if run + l >= 150. {
                let t = (150. - run) / l;
                q = P2::new(
                    win[0].n + t * (win[1].n - win[0].n),
                    win[0].e + t * (win[1].e - win[0].e),
                );
                seg = win[0].bearing_to(win[1]);
                break;
            }
            run += l;
        }
        w.put(2, q.offset(seg + 90., 70.), 612, (seg + 270.) % 360., 15., true);
        w.put(1, ramp, 612, path[0].bearing_to(path[1]), 10., true);
        let said: Vec<String> = (0..3).flat_map(|_| texts(&w.tick())).collect();
        assert!(
            said.iter().any(|t| t.contains("hold position, traffic crossing")),
            "{said:?}"
        );
        // it has gone by
        w.put(2, q.offset(seg + 270., 300.), 612, (seg + 270.) % 360., 15., true);
        let said: Vec<String> = (0..8).flat_map(|_| texts(&w.tick())).collect();
        assert!(said.iter().any(|t| t.contains("continue taxi")), "{said:?}");
    }

    #[test]
    fn departures_go_in_the_order_they_called_ready() {
        let mut w = World::new();
        let hold =
            w.c.ap.route_to_runway(w.c.ap.ramps[1].p, "06R").unwrap().hold.unwrap();
        let hp = w.c.ap.node(w.c.ap.holds[hold].node);
        w.put(1, hp, 612, 66., 0., true);
        w.put(2, hp.offset(246., 60.), 612, 66., 0., true);
        let thr = w.c.ap.runway("06R").unwrap().thr;
        w.put(3, thr.offset(246., 2. * NM), 1300, 66., 150., false);
        w.tick();
        let r = w.say(
            1,
            Pos::Tower,
            "Andersen Tower, Viper 11, holding short 06R, ready for departure",
        );
        assert!(texts(&r)[0].contains("hold short runway 06R, traffic"), "{r:?}");
        w.now += 1.;
        let r = w.say(
            2,
            Pos::Tower,
            "Andersen Tower, Viper 12, holding short 06R, ready for departure",
        );
        assert!(texts(&r)[0].contains("number 2 for departure"), "{r:?}");
        // the arrival lands: the first goes, the second lines up behind it
        w.traffic.retain(|t| t.unit_id != Some(3));
        let r = w.tick();
        let t = texts(&r);
        assert!(
            t.iter()
                .any(|t| t.starts_with("Viper 1-1") && t.contains("cleared for takeoff")),
            "{t:?}"
        );
        assert!(
            !t.iter()
                .any(|t| t.contains("Viper 1-2") && t.contains("cleared for takeoff")),
            "{t:?}"
        );
    }

    #[test]
    fn wake_turbulence_behind_a_departing_heavy() {
        let mut w = World::new();
        let r = w.c.ap.runway("06R").unwrap().clone();
        w.put(1, r.thr.offset(r.hdg, 300.), 612, r.hdg, 120., true);
        w.traffic.last_mut().unwrap().unit_type = "KC-135".into();
        w.tick();
        w.c.flights.get_mut(&1).unwrap().phase = Phase::TakeoffCleared;
        w.put(1, r.thr.offset(r.hdg, 2500.), 1100, r.hdg, 160., false);
        w.traffic.last_mut().unwrap().unit_type = "KC-135".into();
        w.tick();
        let hold =
            w.c.ap.route_to_runway(w.c.ap.ramps[1].p, "06R").unwrap().hold.unwrap();
        w.put(2, w.c.ap.node(w.c.ap.holds[hold].node), 612, 66., 0., true);
        w.traffic.retain(|t| t.unit_id != Some(1));
        w.tick();
        let r = w.say(
            2,
            Pos::Tower,
            "Andersen Tower, Viper 12, holding short 06R, ready for departure",
        );
        assert!(
            texts(&r)[0].contains("caution wake turbulence, departing KC-135"),
            "{r:?}"
        );
    }

    #[test]
    fn wrong_frequency_is_redirected() {
        let mut w = World::new();
        let ramp = w.c.ap.ramps.iter().find(|r| r.name == "south ramp").unwrap().p;
        w.put(1, ramp, 612, 0., 0., true);
        w.tick();
        let r = w.say(1, Pos::Tower, "Andersen Tower, Viper 11, request taxi");
        assert!(texts(&r)[0].contains("contact Andersen Ground 275.8"), "{r:?}");
    }

    #[test]
    fn par_approach_hands_off_and_talks_down() {
        let mut w = World::new();
        let r = w.c.ap.runway("06R").unwrap().clone();
        let back = 246.;
        w.put(
            1,
            r.thr.offset(back, 25. * NM).offset(156., 5. * NM),
            12000,
            66.,
            300.,
            false,
        );
        w.tick();
        let rr = w.say(1, Pos::Approach, "Andersen Approach, Viper 11, 25 miles southwest, angels 12, request PAR full stop");
        let t = &texts(&rr)[0];
        assert!(t.contains("PAR approach to runway 06R"), "{t}");
        assert!(t.contains("descend and maintain 3,000"), "{t}");
        // Established on a 9 mile final: handed to Final.
        w.put(1, r.thr.offset(back, 9. * NM), 3000, 66., 180., false);
        let rr = w.tick();
        assert!(
            texts(&rr).iter().any(|t| t.contains("contact Andersen Final 385.4")),
            "{rr:?}"
        );
        let rr = w.say(1, Pos::Final, "Andersen Final, Viper 11, how do you read");
        // radio check answers, then a check-in starts the approach
        assert!(texts(&rr)[0].contains("loud and clear"), "{rr:?}");
        let rr = w.say(1, Pos::Final, "Andersen Final, Viper 11, checking in");
        assert!(texts(&rr)[0].contains("precision approach to runway 06R"), "{rr:?}");
        // On the glidepath at 6 miles.
        let d = 6. * NM;
        let path = ((d + 300.) * 3f64.to_radians().tan() / FT) as i32 + 612;
        w.put(1, r.thr.offset(back, d), path, 66., 160., false);
        w.now += 10.;
        let rr = w.tick();
        let t = texts(&rr).join(" | ");
        assert!(t.contains("on course") && t.contains("on glidepath"), "{t}");
        // At 3.5 miles, cleared to land in the talkdown.
        let d = 3.5 * NM;
        let path = ((d + 300.) * 3f64.to_radians().tan() / FT) as i32 + 612;
        w.put(1, r.thr.offset(back, d).offset(156., 150.), path + 200, 66., 150., false);
        w.now += 10.;
        let rr = w.tick();
        let t = texts(&rr).join(" | ");
        assert!(
            t.contains("right of course")
                && t.contains("above glidepath")
                && t.contains("cleared to land"),
            "{t}"
        );
    }

    #[test]
    fn overhead_and_go_around_when_the_runway_is_fouled() {
        let mut w = World::new();
        let r = w.c.ap.runway("06R").unwrap().clone();
        w.put(1, r.thr.offset(246., 6. * NM), 2100, 66., 300., false);
        w.tick();
        let rr = w.say(1, Pos::Tower, "Andersen Tower, Viper 11, initial runway 06R");
        assert!(texts(&rr)[0].contains("break approved"), "{rr:?}");
        // Someone taxis onto the runway.
        w.put(2, r.thr.offset(66., 600.), 612, 156., 10., true);
        w.tick();
        // Viper on a 1 mile final: continue, then go around inside half a mile.
        w.put(1, r.thr.offset(246., 1.5 * NM), 1100, 66., 160., false);
        let rr = w.tick();
        assert!(
            texts(&rr).iter().any(|t| t.contains("continue, traffic on the runway")),
            "{rr:?}"
        );
        w.put(1, r.thr.offset(246., 0.4 * NM), 750, 66., 150., false);
        let rr = w.tick();
        let all = texts(&rr).join(" | ");
        assert!(all.contains("go around"), "{all}");
        // and the intruder was told to stop
        assert!(w.c.flights[&2].last.values().any(|t| t.contains("without a clearance")));
    }

    #[test]
    fn landing_rollout_hands_to_ground_and_taxis_in() {
        let mut w = World::new();
        let r = w.c.ap.runway("06R").unwrap().clone();
        w.put(1, r.thr.offset(246., 2. * NM), 1200, 66., 160., false);
        w.tick();
        w.c.flights.get_mut(&1).unwrap().phase = Phase::Pattern;
        w.c.flights.get_mut(&1).unwrap().freq = Some(Pos::Tower);
        let rr = w.tick();
        assert!(texts(&rr).iter().any(|t| t.contains("cleared to land")), "{rr:?}");
        w.put(1, r.thr.offset(66., 900.), 612, 66., 40., true);
        let rr = w.tick();
        assert!(
            texts(&rr)
                .iter()
                .any(|t| t.contains("turn right next taxiway, contact Andersen Ground")),
            "{rr:?}"
        );
        // Off the runway to the south, stopped.
        w.put(1, r.thr.offset(66., 1200.).offset(156., 150.), 612, 156., 5., true);
        w.tick();
        let rr = w.say(
            1,
            Pos::Ground,
            "Andersen Ground, Viper 11, clear of the runway, request taxi to parking",
        );
        assert!(
            texts(&rr)[0].starts_with("Viper 1-1, Andersen Ground, taxi to the"),
            "{rr:?}"
        );
    }

    #[test]
    fn aircraft_types() {
        assert_eq!(type_label("C-17A").as_deref(), Some("C-17"));
        assert_eq!(type_label("FA-18C_hornet").as_deref(), Some("Hornet"));
        assert_eq!(type_label("JF-17").as_deref(), Some("JF-17"));
        assert_eq!(type_label("Bf_109").as_deref(), Some("Bf 109"));
        assert_eq!(type_label(""), None);
        assert_eq!(spoken_type("KC-135"), ", KC-135");
        assert_eq!(spoken_type("JF-17"), "");
    }

    #[test]
    fn readbacks_keep_the_clearance() {
        assert_eq!(
            readback("Reach 3-1, Andersen Ground, runway 06R, taxi via Bravo, Echo, hold short runway 06R, wind 070 at 12, altimeter 2992.", "Reach 3-1", "Andersen", 0.9).unwrap(),
            "06R, taxi via Bravo, Echo, hold short 06R, Reach 3-1."
        );
        assert_eq!(
            readback(
                "Reach 3-1, contact Andersen Tower 340.2.",
                "Reach 3-1",
                "Andersen",
                0.9
            )
            .unwrap(),
            "340.2, Reach 3-1."
        );
        assert_eq!(
            readback(
                "Reach 3-1, contact Andersen Tower 340.2.",
                "Reach 3-1",
                "Andersen",
                0.1
            )
            .unwrap(),
            "Over to Tower 340.2, Reach 3-1."
        );
        assert!(readback(
            "Reach 3-1, Andersen Tower, loud and clear.",
            "Reach 3-1",
            "Andersen",
            0.5
        )
        .is_none());
    }

    fn put_ai(
        w: &mut World,
        id: u64,
        call: &str,
        plan: &str,
        state: &str,
        p: P2,
        alt_ft: i32,
        hdg: f64,
        gs: f64,
        on_ground: bool,
    ) {
        w.put(id, p, alt_ft, hdg, gs, on_ground);
        let t = w.traffic.iter_mut().find(|t| t.unit_id == Some(id)).unwrap();
        t.ai = true;
        t.callsign = call.into();
        t.player_name = format!("ATC-{id}");
        t.ai_plan = Some(plan.into());
        t.ai_state = Some(state.into());
        t.unit_type = "C-17A".into();
    }

    #[test]
    fn ai_departure_asks_for_taxi_and_is_released_when_clear() {
        let mut w = World::new();
        let ramp = w.c.ap.ramps.iter().find(|r| r.name == "south ramp").unwrap().p;
        put_ai(&mut w, 7, "Reach 3-1", "departure", "waiting", ramp, 612, 0., 0., true);
        // a player on a 3 mile final keeps it on the stand
        let thr = w.c.ap.runway("06R").unwrap().thr;
        w.put(2, thr.offset(246., 3. * NM), 1500, 66., 150., false);
        let mut said = vec![];
        for _ in 0..40 {
            said.extend(w.tick());
        }
        let all = texts(&said).join(" | ");
        assert!(all.contains("Andersen Ground, Reach 3-1, C-17"), "{all}");
        assert!(
            said.iter().any(|t| t.pilot.is_some() && t.aircraft.is_some()),
            "AI calls carry the type"
        );
        assert!(all.contains("hold position, traffic"), "{all}");
        assert!(
            said.iter().all(|t| t.action.is_none()),
            "not released while the final is busy"
        );
        // the player lands and is gone: Ground releases it
        w.traffic.retain(|t| t.unit_id != Some(2));
        let mut said = vec![];
        for _ in 0..40 {
            said.extend(w.tick());
        }
        let rel = said.iter().find(|t| t.action.is_some()).expect("released");
        assert_eq!(rel.action.as_ref().unwrap().1, "start");
        assert!(rel.text.contains("taxi via"), "{}", rel.text);
        // and the pilot reads it back in his own voice
        assert!(
            said.iter().any(|t| t.pilot.is_some()
                && t.text.contains("hold short 06R")
                && t.text.contains("Reach 3-1")),
            "{:?}",
            texts(&said)
        );
    }

    #[test]
    fn ai_arrival_is_held_for_a_player_on_the_approach() {
        let mut w = World::new();
        let thr = w.c.ap.runway("06R").unwrap().thr;
        put_ai(
            &mut w,
            8,
            "Shell 7-1",
            "arrival",
            "active",
            thr.offset(20., 30. * NM),
            12000,
            200.,
            300.,
            false,
        );
        let mut said = vec![];
        for _ in 0..30 {
            said.extend(w.tick());
        }
        assert!(
            texts(&said).iter().any(|t| t.contains("Andersen Approach, Shell 7-1")),
            "{:?}",
            texts(&said)
        );
        // a player starts a PAR inside it
        w.put(1, thr.offset(246., 9. * NM), 3000, 66., 180., false);
        w.tick();
        w.c.flights.get_mut(&1).unwrap().phase = Phase::RadarFinal;
        let mut said = vec![];
        for _ in 0..5 {
            said.extend(w.tick());
        }
        let hold = said
            .iter()
            .find(|t| t.action.as_ref().map(|a| a.1) == Some("hold"))
            .expect("held");
        assert!(hold.text.contains("hold present position"), "{}", hold.text);
        assert!(hold.subtitle.is_none(), "no subtitles for AI");
    }

    #[test]
    fn atis_cycles_letter_on_material_change() {
        let mut w = World::new();
        let r = w.tick();
        let atis: Vec<_> = r.iter().filter(|t| t.pos == Pos::Atis).collect();
        assert_eq!(atis.len(), 1);
        assert!(
            atis[0].text.starts_with("Andersen information Alpha"),
            "{}",
            atis[0].text
        );
        assert!(atis[0].text.contains("Runway 06R in use"));
        w.f.wind_from_deg = 250;
        let r = w.tick();
        let a =
            r.iter().find(|t| t.pos == Pos::Atis).expect("new letter broadcast at once");
        assert!(
            a.text.contains("information Bravo") && a.text.contains("Runway 24L in use"),
            "{}",
            a.text
        );
    }
}
