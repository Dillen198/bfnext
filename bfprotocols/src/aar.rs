//! Air-to-air refuelling cues for the cockpit plugin's director-light overlay.
//!
//! Servers run with object export off, so the plugin can see its own jet
//! but not the tanker. The engines (bflib, bfrange) work out where each
//! receiver is relative to its tanker, a few times a second, and publish an
//! [`AarCues`] over netidx. bfdb keeps the latest one and hands each player
//! their own cue at `/api/cockpit/aar`.
//!
//! Where the cue comes from, best first ([`CueSource`]):
//!
//! - **tanker**: DCS's KC-135 drives its own pilot director lights as model
//!   arguments 73 (up/down) and 74 (fore/aft). The engine reads them
//!   (`getDrawArgumentValue`), so the overlay shows exactly what the real
//!   tanker's belly shows. Each runs 0..1 in five bands (below 0.01 is off);
//!   the bands come from the KC-135 model, as reported by the AsilTurq
//!   refuel mod's argument table.
//! - **learned**: where receivers of this type actually were, in the tanker's
//!   frame, each time DCS reported them connected. Median of the last
//!   [`KEEP`] contacts, used once there are [`MIN_SAMPLES`].
//! - **estimate**: a built-in guess at the contact point for drogue tankers,
//!   from the airframes' dimensions. Only until something is learned. The
//!   boom has none: a wrong guess there would steer people off the boom, and
//!   the tanker's own lights cover it.
//!
//! Frames: the tanker's body frame is (fwd, up, right) metres from the
//! tanker's reference point, DCS's `x`, `y`, `z` unit vectors. An error is
//! the receiver minus the contact point in that frame: +fwd means the
//! receiver is ahead of where it should be.
//!
//! Light values are corrections to fly, -2..=2, `None` when the light is
//! off: `ud` +up / -down, `fa` +forward / -aft, `lat` +right / -left. 0 is
//! the centre (on position), 1 a green light, 2 a red one.

use crate::range::{cfg::tanker_type, RefuelMethod};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::Path};

/// KC-135 pilot director lights: the elevation (up/down) column.
pub const PDL_UD_ARG: i64 = 73;
/// KC-135 pilot director lights: the telescope (fore/aft) column.
pub const PDL_FA_ARG: i64 = 74;

/// Tanker types whose model drives the director lights.
pub fn has_pdl_args(typ: &str) -> bool {
    typ == "KC-135"
}

/// Contacts kept per (tanker, receiver, station).
pub const KEEP: usize = 31;
/// Contacts needed before a learned point replaces the estimate.
pub const MIN_SAMPLES: usize = 3;

/// The zone a cue is given in, tanker frame: from this far behind...
pub const ZONE_AFT_M: f64 = 600.;
/// ...to this far ahead of the tanker's reference point,
pub const ZONE_FWD_M: f64 = 30.;
/// this far either side,
pub const ZONE_SIDE_M: f64 = 200.;
/// and this far above or below.
pub const ZONE_VERT_M: f64 = 150.;

pub type V3 = [f64; 3];

fn sub(a: V3, b: V3) -> V3 {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

fn dot(a: V3, b: V3) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn norm(a: V3) -> f64 {
    dot(a, a).sqrt()
}

/// One director-light column from its model argument.
pub fn pdl_from_arg(v: f64) -> Option<i8> {
    if !v.is_finite() || v < 0.01 {
        return None;
    }
    let band = ((v / 0.2).floor() as i64).clamp(0, 4) as i8;
    // band 4 (0.8..1.0) is the red DOWN / AFT light: fly down, fly aft
    Some(2 - band)
}

/// A light from an error and its tolerance: within 0.4 tol centred, to 1.2
/// tol green, red beyond. A positive error (high, ahead, right) asks for a
/// negative correction (down, aft, left).
pub fn level(err: f64, tol: f64) -> i8 {
    let a = err.abs() / tol.max(0.01);
    let m = if a < 0.4 {
        0
    } else if a < 1.2 {
        1
    } else {
        2
    };
    if err > 0. {
        -m
    } else {
        m
    }
}

/// DCS's unit callsign ("Texaco11", "Arco21") the way it is said: "Texaco
/// 1-1". Anything else comes back as it was.
pub fn pretty_callsign(cs: &str) -> String {
    let word: String = cs.chars().take_while(|c| !c.is_ascii_digit()).collect();
    let num: String = cs.chars().skip(word.chars().count()).collect();
    let word = word.trim();
    if word.is_empty() || num.len() != 2 || !num.chars().all(|c| c.is_ascii_digit()) {
        return cs.to_string();
    }
    format!("{word} {}-{}", &num[..1], &num[1..])
}

/// Which hose or boom on the tanker.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Station {
    Centre,
    Left,
    Right,
}

impl Station {
    fn key(self) -> &'static str {
        match self {
            Self::Centre => "centre",
            Self::Left => "left",
            Self::Right => "right",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CueSource {
    /// The tanker's own director lights.
    Tanker,
    /// The learned contact point.
    Learned,
    /// The built-in estimate.
    Estimate,
    /// Behind a tanker, but nothing to steer by yet.
    None,
}

/// A drogue pod's light.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PodLight {
    /// Too fast or past the basket: back off.
    Red,
    /// Cleared to make contact.
    Amber,
    /// Connected, fuel flowing.
    Green,
}

/// How close to the contact point counts as on it, metres (fwd, up, right).
pub fn tolerance(method: RefuelMethod) -> V3 {
    match method {
        RefuelMethod::Boom => [3.5, 2.5, 3.0],
        RefuelMethod::Drogue => [3.0, 1.5, 1.5],
    }
}

/// Built-in estimates of where a receiver's reference point sits, in the
/// tanker's frame, when its probe is in the basket: the basket trails the
/// pod by the hose length, and a fighter's reference point is some 7 m
/// behind its probe. Rough on purpose -- learning replaces them after a few
/// contacts. None for the boom (see the module docs).
pub fn estimate(typ: &str) -> &'static [(Station, V3)] {
    match typ {
        "KC130" | "KC130J" => {
            &[(Station::Left, [-29., -3.5, -11.5]), (Station::Right, [-29., -3.5, 11.5])]
        }
        "KC135MPRS" => {
            &[(Station::Left, [-33., -4.5, -14.5]), (Station::Right, [-33., -4.5, 14.5])]
        }
        "IL-78M" => {
            &[(Station::Left, [-36., -5., -15.]), (Station::Right, [-36., -5., 15.])]
        }
        "S-3B Tanker" | "A6E" => &[(Station::Centre, [-23., -3., 0.])],
        _ => &[],
    }
}

/// The stations a tanker type refuels from.
pub fn stations(typ: &str) -> Vec<Station> {
    match tanker_type(typ).map(|t| t.method) {
        Some(RefuelMethod::Boom) => vec![Station::Centre],
        Some(RefuelMethod::Drogue) => {
            let e = estimate(typ);
            if e.is_empty() {
                vec![Station::Centre]
            } else {
                e.iter().map(|(s, _)| *s).collect()
            }
        }
        None => vec![],
    }
}

/// The station a receiver at `rel` (tanker frame) is lined up on: the one
/// whose contact point is nearest sideways.
pub fn station_for(typ: &str, rel: V3) -> Station {
    let e = estimate(typ);
    e.iter()
        .min_by(|a, b| (a.1[2] - rel[2]).abs().total_cmp(&(b.1[2] - rel[2]).abs()))
        .map(|(s, _)| *s)
        .unwrap_or(Station::Centre)
}

fn median(mut v: Vec<f64>) -> f64 {
    v.sort_by(f64::total_cmp);
    let n = v.len();
    if n % 2 == 1 {
        v[n / 2]
    } else {
        (v[n / 2 - 1] + v[n / 2]) / 2.
    }
}

/// Where receivers were when DCS said they connected, per (tanker type,
/// receiver type, station). Saved as JSON beside the engine's other files.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Learned {
    #[serde(default)]
    pub contacts: BTreeMap<String, Vec<V3>>,
}

impl Learned {
    fn key(tanker: &str, receiver: &str, st: Station) -> String {
        format!("{tanker}|{receiver}|{}", st.key())
    }

    /// Record a contact. Points far from anything a hose or boom reaches
    /// (a mis-resolved receiver) are refused. True when it was kept.
    pub fn record(&mut self, tanker: &str, receiver: &str, st: Station, rel: V3) -> bool {
        if !rel.iter().all(|v| v.is_finite()) || norm(rel) > 80. || rel[0] > 5. {
            return false;
        }
        let v = self.contacts.entry(Self::key(tanker, receiver, st)).or_default();
        v.push(rel);
        if v.len() > KEEP {
            let drop = v.len() - KEEP;
            v.drain(..drop);
        }
        true
    }

    /// The learned contact point, once there are enough contacts.
    pub fn get(&self, tanker: &str, receiver: &str, st: Station) -> Option<V3> {
        let v = self.contacts.get(&Self::key(tanker, receiver, st))?;
        if v.len() < MIN_SAMPLES {
            return None;
        }
        let axis = |i: usize| median(v.iter().map(|p| p[i]).collect());
        Some([axis(0), axis(1), axis(2)])
    }

    /// Read `path`; empty when it is missing or unreadable (logged).
    pub fn load(path: &Path) -> Self {
        match std::fs::read(path) {
            Ok(b) => serde_json::from_slice(&b).unwrap_or_else(|e| {
                log::warn!("aar: {path:?} is unreadable, starting afresh: {e}");
                Self::default()
            }),
            Err(_) => Self::default(),
        }
    }

    /// Write `path` through a temporary file, so a crash mid-write leaves
    /// the old one.
    pub fn save(&self, path: &Path) -> anyhow::Result<()> {
        let mut tmp = path.to_path_buf();
        tmp.set_extension("json.tmp");
        std::fs::write(&tmp, serde_json::to_vec_pretty(self)?)?;
        std::fs::rename(&tmp, path)?;
        Ok(())
    }
}

/// The contact point for a receiver, and where it came from.
pub fn contact_point(
    learned: &Learned,
    tanker: &str,
    receiver: &str,
    st: Station,
) -> Option<(V3, CueSource)> {
    if let Some(p) = learned.get(tanker, receiver, st) {
        return Some((p, CueSource::Learned));
    }
    estimate(tanker)
        .iter()
        .find(|(s, _)| *s == st)
        .map(|(_, p)| (*p, CueSource::Estimate))
}

/// A tanker as the engine sees it this tick.
#[derive(Debug, Clone)]
pub struct TankerObs {
    /// Unit name.
    pub unit: String,
    /// What to call it on the overlay ("Texaco 1-1").
    pub callsign: String,
    pub typ: String,
    pub side: u8,
    /// Position and body axes, DCS world frame.
    pub p: V3,
    pub x: V3,
    pub y: V3,
    pub z: V3,
    pub vel: V3,
    /// Arguments 73 and 74, for a type that has them.
    pub pdl: Option<(f64, f64)>,
}

impl TankerObs {
    /// A world point in this tanker's (fwd, up, right) frame.
    pub fn to_local(&self, w: V3) -> V3 {
        let d = sub(w, self.p);
        [dot(d, self.x), dot(d, self.y), dot(d, self.z)]
    }
}

/// A player's aircraft as the engine sees it this tick.
#[derive(Debug, Clone)]
pub struct ReceiverObs {
    /// What the cue is published under: the player's ucid.
    pub key: String,
    pub unit: String,
    pub typ: String,
    pub side: u8,
    pub p: V3,
    pub vel: V3,
}

/// One player's cue. Everything the overlay draws, plus what it needs to
/// move the cue between updates: the tanker's axes and velocity, so its own
/// velocity can be put in the tanker's frame.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AarCue {
    pub tanker: String,
    pub typ: String,
    pub method: RefuelMethod,
    pub station: Station,
    pub source: CueSource,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ud: Option<i8>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fa: Option<i8>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lat: Option<i8>,
    /// Receiver minus contact point, tanker frame (fwd, up, right), metres.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub err: Option<V3>,
    /// On-position tolerances (fwd, up, right), metres.
    pub tol: V3,
    /// Receiver relative to the tanker's reference point, tanker frame.
    pub rel: V3,
    pub range_m: f64,
    /// Closing speed, knots: + closing on the tanker.
    pub closure_kts: f64,
    pub connected: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pod: Option<PodLight>,
    /// The tanker's body axes and velocity, DCS world frame.
    pub tx: V3,
    pub ty: V3,
    pub tz: V3,
    pub tvel: V3,
}

/// What an engine publishes: every cue, by ucid, at engine time `t`.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct AarCues {
    pub t: f64,
    pub cues: BTreeMap<String, AarCue>,
}

fn round(v: f64, places: i32) -> f64 {
    let m = 10f64.powi(places);
    (v * m).round() / m
}

fn round3(v: V3, places: i32) -> V3 {
    [round(v[0], places), round(v[1], places), round(v[2], places)]
}

/// The cue for one receiver behind one tanker, or None outside the zone.
pub fn cue(
    learned: &Learned,
    t: &TankerObs,
    r: &ReceiverObs,
    connected: bool,
) -> Option<AarCue> {
    let method = tanker_type(&t.typ)?.method;
    let rel = t.to_local(r.p);
    if !(-ZONE_AFT_M..ZONE_FWD_M).contains(&rel[0])
        || rel[2].abs() > ZONE_SIDE_M
        || rel[1].abs() > ZONE_VERT_M
    {
        return None;
    }
    let st = station_for(&t.typ, rel);
    let tol = tolerance(method);
    let rv = sub(r.vel, t.vel);
    // the rate the distance to the tanker shrinks
    let d = sub(r.p, t.p);
    let closure_kts = -dot(rv, d) / norm(d).max(1.) * 1.943_844;
    let point = contact_point(learned, &t.typ, &r.typ, st);
    let err = point.map(|(p, _)| sub(rel, p));
    let from_err = |i: usize| err.map(|e| level(e[i], tol[i]));
    let tanker_lights = t
        .pdl
        .filter(|_| has_pdl_args(&t.typ))
        .map(|(ud, fa)| (pdl_from_arg(ud), pdl_from_arg(fa)))
        .filter(|(ud, fa)| ud.is_some() || fa.is_some());
    let (source, ud, fa) = match (tanker_lights, point) {
        (Some((ud, fa)), _) => (CueSource::Tanker, ud, fa),
        // A boom has no estimate (see `estimate`), so it steers only by its
        // own lights or by what was learned.
        (None, Some((_, src))) => (src, from_err(1), from_err(0)),
        (None, None) => (CueSource::None, None, None),
    };
    let pod = (method == RefuelMethod::Drogue).then(|| {
        if connected {
            PodLight::Green
        } else if err.is_some_and(|e| {
            e[0] > 1.5 * tol[0] && e[0] < 15. && e[2].abs() < 6. && e[1].abs() < 6.
        }) || (closure_kts > 8. && err.is_some_and(|e| e[0] > -15.))
        {
            PodLight::Red
        } else {
            PodLight::Amber
        }
    });
    Some(AarCue {
        tanker: t.callsign.clone(),
        typ: t.typ.clone(),
        method,
        station: st,
        source,
        ud,
        fa,
        lat: from_err(2),
        err: err.map(|e| round3(e, 2)),
        tol,
        rel: round3(rel, 2),
        range_m: round(norm(rel), 1),
        closure_kts: round(closure_kts, 1),
        connected,
        pod,
        tx: round3(t.x, 5),
        ty: round3(t.y, 5),
        tz: round3(t.z, 5),
        tvel: round3(t.vel, 2),
    })
}

#[derive(Debug, Clone, Default)]
struct Rx {
    /// The tanker it was behind at the last update, and how far.
    tanker: String,
    dist: f64,
    connected: bool,
    /// A contact not yet learned from.
    learn: bool,
}

/// What an engine keeps between ticks: the learned points, who is
/// connected, and who is behind which tanker (for the dedicated-server
/// REFUELING bug, which names the tanker instead of the receiver).
#[derive(Debug, Default)]
pub struct Tracker {
    pub learned: Learned,
    rx: BTreeMap<String, Rx>,
    dirty: bool,
}

impl Tracker {
    pub fn new(learned: Learned) -> Self {
        Self { learned, ..Self::default() }
    }

    /// A receiver started (or stopped) taking fuel.
    pub fn set_connected(&mut self, unit: &str, on: bool) {
        let r = self.rx.entry(unit.to_string()).or_default();
        if on && !r.connected {
            r.learn = true;
        }
        if !on {
            r.learn = false;
        }
        r.connected = on;
    }

    /// REFUELING / REFUELING_STOP naming `unit`: a receiver, or (the
    /// dedicated-server bug) a tanker, in which case its closest receiver
    /// within 80 m is the one.
    pub fn refuel_event(&mut self, unit: &str, on: bool) {
        if self.rx.contains_key(unit) {
            self.set_connected(unit, on);
            return;
        }
        let best = self
            .rx
            .iter()
            .filter(|(_, r)| r.tanker == unit && r.dist < 80.)
            .min_by(|a, b| a.1.dist.total_cmp(&b.1.dist))
            .map(|(u, _)| u.clone());
        if let Some(u) = best {
            self.set_connected(&u, on);
        }
    }

    /// True once since the learned points last changed.
    pub fn take_dirty(&mut self) -> bool {
        std::mem::take(&mut self.dirty)
    }

    /// Every receiver's cue this tick. Each receiver is matched with the
    /// closest friendly tanker it is in the zone of.
    pub fn update(
        &mut self,
        t: f64,
        tankers: &[TankerObs],
        receivers: &[ReceiverObs],
    ) -> AarCues {
        let mut out = AarCues { t: round(t, 3), cues: BTreeMap::new() };
        let mut seen = vec![];
        for r in receivers {
            let best = tankers
                .iter()
                .filter(|tk| tk.side == r.side && tk.unit != r.unit)
                .map(|tk| (norm(sub(r.p, tk.p)), tk))
                .filter(|(d, _)| *d < ZONE_AFT_M + ZONE_SIDE_M)
                .min_by(|a, b| a.0.total_cmp(&b.0));
            let Some((dist, tk)) = best else { continue };
            seen.push(r.unit.clone());
            let st = self.rx.entry(r.unit.clone()).or_default();
            st.tanker = tk.unit.clone();
            st.dist = dist;
            if st.connected && st.learn {
                st.learn = false;
                let rel = tk.to_local(r.p);
                let station = station_for(&tk.typ, rel);
                if self.learned.record(&tk.typ, &r.typ, station, rel) {
                    self.dirty = true;
                    log::info!(
                        "aar: learned a {} contact behind a {} ({station:?}) at {:.1} {:.1} {:.1}",
                        r.typ,
                        tk.typ,
                        rel[0],
                        rel[1],
                        rel[2]
                    );
                }
            }
            let connected = st.connected;
            if let Some(c) = cue(&self.learned, tk, r, connected) {
                out.cues.insert(r.key.clone(), c);
            }
        }
        // forget whoever is no longer near a tanker
        self.rx.retain(|u, _| seen.contains(u));
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tanker(typ: &str, pdl: Option<(f64, f64)>) -> TankerObs {
        TankerObs {
            unit: "TKR-1".into(),
            callsign: "Texaco 1-1".into(),
            typ: typ.into(),
            side: 2,
            p: [1000., 7000., 2000.],
            // flying north (+x), up is +y, right is +z
            x: [1., 0., 0.],
            y: [0., 1., 0.],
            z: [0., 0., 1.],
            vel: [150., 0., 0.],
            pdl,
        }
    }

    fn receiver(rel: V3, vel: V3) -> ReceiverObs {
        ReceiverObs {
            key: "ucid1".into(),
            unit: "Viper-1".into(),
            typ: "F-16C_50".into(),
            side: 2,
            p: [1000. + rel[0], 7000. + rel[1], 2000. + rel[2]],
            vel,
        }
    }

    #[test]
    fn pdl_bands() {
        assert_eq!(pdl_from_arg(0.), None);
        assert_eq!(pdl_from_arg(0.005), None);
        assert_eq!(pdl_from_arg(0.1), Some(2)); // red U / red F
        assert_eq!(pdl_from_arg(0.3), Some(1));
        assert_eq!(pdl_from_arg(0.5), Some(0));
        assert_eq!(pdl_from_arg(0.7), Some(-1));
        assert_eq!(pdl_from_arg(0.9), Some(-2)); // red D / red A
        assert_eq!(pdl_from_arg(1.01), Some(-2));
        assert_eq!(pdl_from_arg(f64::NAN), None);
    }

    #[test]
    fn callsigns() {
        assert_eq!(pretty_callsign("Texaco11"), "Texaco 1-1");
        assert_eq!(pretty_callsign("Arco21"), "Arco 2-1");
        assert_eq!(pretty_callsign("Shell"), "Shell");
        assert_eq!(pretty_callsign("101"), "101");
    }

    #[test]
    fn levels() {
        assert_eq!(level(0.3, 2.5), 0);
        assert_eq!(level(2., 2.5), -1); // high: fly down, green
        assert_eq!(level(-2., 2.5), 1);
        assert_eq!(level(5., 2.5), -2);
        assert_eq!(level(-5., 2.5), 2);
    }

    #[test]
    fn boom_mirrors_the_tankers_lights() {
        let t = tanker("KC-135", Some((0.9, 0.5)));
        let r = receiver([-30., -9., 0.], [150., 0., 0.]);
        let c = cue(&Learned::default(), &t, &r, false).unwrap();
        assert_eq!(c.source, CueSource::Tanker);
        assert_eq!((c.ud, c.fa), (Some(-2), Some(0)));
        assert_eq!(c.method, RefuelMethod::Boom);
        assert!(c.pod.is_none());
    }

    #[test]
    fn boom_without_lights_or_learning_steers_nothing() {
        let t = tanker("KC-135", Some((0., 0.)));
        let r = receiver([-60., -10., 0.], [152., 0., 0.]);
        let c = cue(&Learned::default(), &t, &r, false).unwrap();
        assert_eq!(c.source, CueSource::None);
        assert_eq!((c.ud, c.fa, c.err), (None, None, None));
        // 2 m/s faster, along a line 9.5 degrees off the tanker's axis
        assert!((c.closure_kts - 3.83).abs() < 0.05, "{}", c.closure_kts);
    }

    #[test]
    fn drogue_uses_the_estimate_and_picks_the_near_pod() {
        let t = tanker("KC130", None);
        // 2 m high and 1 m left of the right basket's receiver position
        let r = receiver([-29., -1.5, 10.5], [150., 0., 0.]);
        let c = cue(&Learned::default(), &t, &r, false).unwrap();
        assert_eq!(c.station, Station::Right);
        assert_eq!(c.source, CueSource::Estimate);
        assert_eq!(c.ud, Some(-2)); // 2 m high on a 1.5 m tolerance: red, down
        assert_eq!(c.lat, Some(1)); // 1 m left: green, right
        assert_eq!(c.fa, Some(0));
        assert_eq!(c.pod, Some(PodLight::Amber));
    }

    #[test]
    fn outside_the_zone_no_cue() {
        let t = tanker("KC130", None);
        assert!(cue(
            &Learned::default(),
            &t,
            &receiver([-900., 0., 0.], [150., 0., 0.]),
            false
        )
        .is_none());
        assert!(cue(
            &Learned::default(),
            &t,
            &receiver([50., 0., 0.], [150., 0., 0.]),
            false
        )
        .is_none());
        assert!(cue(
            &Learned::default(),
            &tanker("F-16C_50", None),
            &receiver([-30., 0., 0.], [150., 0., 0.]),
            false
        )
        .is_none());
    }

    #[test]
    fn learning_replaces_the_estimate() {
        let mut l = Learned::default();
        for d in [-0.2, 0., 0.3] {
            assert!(l.record("KC130", "F-16C_50", Station::Right, [-27. + d, -3., 12.]));
        }
        // nonsense is refused
        assert!(!l.record("KC130", "F-16C_50", Station::Right, [-500., 0., 0.]));
        assert!(!l.record("KC130", "F-16C_50", Station::Right, [20., 0., 0.]));
        let (p, src) = contact_point(&l, "KC130", "F-16C_50", Station::Right).unwrap();
        assert_eq!(src, CueSource::Learned);
        assert_eq!(p, [-27., -3., 12.]);
        // another receiver type still uses the estimate
        let (_, src) =
            contact_point(&l, "KC130", "FA-18C_hornet", Station::Right).unwrap();
        assert_eq!(src, CueSource::Estimate);
        // the boom has no estimate
        assert!(contact_point(&l, "KC-135", "F-16C_50", Station::Centre).is_none());
    }

    #[test]
    fn keeps_the_last_contacts() {
        let mut l = Learned::default();
        for i in 0..(KEEP + 10) {
            l.record(
                "KC-135",
                "F-16C_50",
                Station::Centre,
                [-30. - i as f64 * 0.01, -9., 0.],
            );
        }
        assert_eq!(l.contacts.values().next().unwrap().len(), KEEP);
    }

    #[test]
    fn tracker_learns_from_a_tanker_named_refuel_event() {
        let mut tr = Tracker::default();
        let t = tanker("KC-135", Some((0.5, 0.5)));
        let r = receiver([-30., -9., 0.5], [150., 0., 0.]);
        let c = tr.update(1., std::slice::from_ref(&t), std::slice::from_ref(&r));
        assert!(!c.cues["ucid1"].connected);
        // the dedicated-server bug: REFUELING names the tanker
        tr.refuel_event("TKR-1", true);
        let c = tr.update(1.2, std::slice::from_ref(&t), std::slice::from_ref(&r));
        assert!(c.cues["ucid1"].connected);
        assert!(tr.take_dirty());
        assert!(!tr.take_dirty());
        assert_eq!(tr.learned.contacts.values().next().unwrap().len(), 1);
        // learned once per contact, not every tick
        tr.update(1.4, std::slice::from_ref(&t), std::slice::from_ref(&r));
        assert_eq!(tr.learned.contacts.values().next().unwrap().len(), 1);
        tr.refuel_event("TKR-1", false);
        tr.refuel_event("Viper-1", true);
        tr.update(1.6, std::slice::from_ref(&t), std::slice::from_ref(&r));
        assert_eq!(tr.learned.contacts.values().next().unwrap().len(), 2);
    }

    #[test]
    fn tracker_ignores_the_other_side() {
        let mut tr = Tracker::default();
        let t = tanker("KC-135", Some((0.5, 0.5)));
        let mut r = receiver([-30., -9., 0.], [150., 0., 0.]);
        r.side = 1;
        assert!(tr.update(1., &[t], &[r]).cues.is_empty());
    }

    #[test]
    fn learned_round_trips() {
        let dir =
            std::env::temp_dir().join(format!("aar-learned-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("aar_contacts.json");
        let mut l = Learned::default();
        l.record("KC130", "AV8BNA", Station::Left, [-28., -3., -11.]);
        l.save(&path).unwrap();
        let back = Learned::load(&path);
        assert_eq!(back.contacts, l.contacts);
        assert!(Learned::load(&dir.join("missing.json")).contacts.is_empty());
        let _ = std::fs::remove_dir_all(&dir);
    }
}
