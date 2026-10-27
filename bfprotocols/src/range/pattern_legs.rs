//! The traffic pattern and the taxi of a field landing, from the track.
//! Shared so the engine (which writes the record's summaries) and bfdb
//! (whose card colours every leg) read a pattern the same way; the range
//! site has a port in `bfrange-web/src/lib/patternLegs.ts`.
//!
//! Pure functions over samples already in the runway frame (`along` metres
//! from the landing threshold, + in the landing direction; `across` metres,
//! + right of the centreline; `rel` = track minus runway heading, degrees),
//! so they are tested without DCS (see the tests at the bottom).
//!
//! Walking back from the touchdown:
//! - **final**: track within 25° of the runway, near the extended centreline,
//!   coming steadily down it -- its first sample is the roll-out
//! - **base**: the turn before it
//! - **downwind**: track within 25° of the reciprocal; the sample nearest
//!   abeam the threshold gives the spacing, its side the traffic direction
//! - **break / initial**: before the downwind, track within 25° of the
//!   runway over or near it: the last of those is where the break started
//!
//! The calls are the ones an instructor makes at a field: wide or tight
//! downwind, low or high downwind, an extended downwind, a short final.

use super::{FieldPattern, FieldTaxi};

const NM: f64 = 1852.;
const M_TO_FT: f64 = 3.28084;

#[derive(Debug, Clone, Copy)]
pub struct RelSample {
    pub t: f64,
    pub along: f64,
    pub across: f64,
    /// metres above the field
    pub agl: f64,
    /// track minus the landing direction, -180..180
    pub rel: f64,
    pub gs_kts: f64,
}

fn median(mut v: Vec<f64>) -> Option<f64> {
    if v.is_empty() {
        return None;
    }
    v.sort_by(f64::total_cmp);
    Some(v[v.len() / 2])
}

fn wrap(a: f64) -> f64 {
    let x = (a + 180.).rem_euclid(360.) - 180.;
    if x == -180. {
        180.
    } else {
        x
    }
}

/// Where each leg starts, walking back from the last sample (the touchdown):
/// `initial..brk` initial, `brk..downwind` the break, `downwind..base`
/// downwind, `base..fin` base, `fin..` final.
struct Walk {
    initial: usize,
    brk: usize,
    downwind: usize,
    base: usize,
    fin: usize,
}

fn walk(s: &[RelSample]) -> Walk {
    let aligned = |x: &RelSample| wrap(x.rel).abs() < 25.;
    let reciprocal = |x: &RelSample| (180. - wrap(x.rel).abs()) < 25.;
    // final: back from touchdown while lined up, near the centreline, short of the threshold (or just past it)
    let mut i = s.len() - 1;
    while i > 0
        && aligned(&s[i - 1])
        && s[i - 1].across.abs() < 1500.
        && s[i - 1].along <= s[i].along + 50.
    {
        i -= 1;
    }
    // base: the turn before final
    let mut j = i;
    while j > 0 && !reciprocal(&s[j - 1]) && !aligned(&s[j - 1]) {
        j -= 1;
    }
    // downwind: before the base, flying the reciprocal
    let mut k = j;
    while k > 0 && reciprocal(&s[k - 1]) {
        k -= 1;
    }
    // break / initial: before the downwind (and its 180° turn), lined up over or near the runway
    let mut b = k;
    while b > 0 && !aligned(&s[b - 1]) {
        b -= 1;
    }
    let mut a = b;
    while a > 0 && aligned(&s[a - 1]) && s[a - 1].across.abs() < 900. {
        a -= 1;
    }
    Walk { initial: a, brk: b, downwind: k, base: j, fin: i }
}

/// A leg of a field landing track.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Leg {
    Initial,
    Break,
    Downwind,
    Base,
    Final,
    Rollout,
    Other,
}

impl Leg {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Initial => "initial",
            Self::Break => "break",
            Self::Downwind => "downwind",
            Self::Base => "base",
            Self::Final => "final",
            Self::Rollout => "rollout",
            Self::Other => "other",
        }
    }
}

/// The leg of every sample (`s` oldest first; after `t_touch` is the rollout).
pub fn legs(s: &[RelSample], t_touch: f64) -> Vec<Leg> {
    let mut out: Vec<Leg> =
        s.iter().map(|x| if x.t > t_touch { Leg::Rollout } else { Leg::Other }).collect();
    let pre = s.iter().filter(|x| x.t <= t_touch).count();
    if pre < 5 {
        return out;
    }
    let w = walk(&s[..pre]);
    let mut set = |r: std::ops::Range<usize>, l: Leg| r.for_each(|n| out[n] = l);
    set(w.fin..pre, Leg::Final);
    set(w.base..w.fin, Leg::Base);
    set(w.downwind..w.base, Leg::Downwind);
    if w.base - w.downwind >= 3 {
        set(w.brk..w.downwind, Leg::Break);
        set(w.initial..w.brk, Leg::Initial);
    }
    out
}

/// The pattern into a touchdown at `t_touch`; `s` oldest first.
pub fn pattern(s: &[RelSample], t_touch: f64) -> FieldPattern {
    let s: Vec<RelSample> = s.iter().filter(|x| x.t <= t_touch).copied().collect();
    let mut p = FieldPattern { kind: "other".into(), ..Default::default() };
    if s.len() < 5 {
        return p;
    }
    let w = walk(&s);
    let fin = w.fin;
    let roll = &s[fin];
    let final_len = (s[s.len() - 1].along - roll.along).abs();
    p.rollout_nm = Some((-roll.along).max(0.) / NM);
    p.rollout_alt_ft = Some(roll.agl * M_TO_FT);
    p.final_gs_kts =
        median(s[fin..].iter().filter(|x| x.along < 0.).map(|x| x.gs_kts).collect());
    let (base_start, k) = (w.base, w.downwind);
    let dw = &s[k..base_start];
    let dw_secs = dw.last().map(|l| l.t - dw[0].t).unwrap_or(0.);
    if dw.len() >= 3 && dw_secs >= 8. {
        p.base_at_m = Some(s[base_start.min(s.len() - 1)].along);
        p.downwind_alt_ft = median(dw.iter().map(|x| x.agl * M_TO_FT).collect());
        if let Some(ab) = dw.iter().min_by(|a, b| a.along.abs().total_cmp(&b.along.abs()))
        {
            if ab.along.abs() < 1500. {
                p.abeam_nm = Some(ab.across.abs() / NM);
                p.abeam_alt_ft = Some(ab.agl * M_TO_FT);
            }
            p.side = Some(if ab.across < 0. { "left" } else { "right" }.into());
        }
        let (brk, a) = (w.brk, w.initial);
        let initial = &s[a..brk];
        let initial_secs = initial.last().map(|l| l.t - initial[0].t).unwrap_or(0.);
        if brk > 0 && initial.len() >= 2 && initial_secs >= 4. {
            let bp = &s[brk - 1];
            p.kind = "overhead".into();
            p.break_at_m = Some(bp.along);
            p.break_alt_ft = Some(bp.agl * M_TO_FT);
            p.initial_alt_ft = median(initial.iter().map(|x| x.agl * M_TO_FT).collect());
            p.pattern_time_s = Some(t_touch - bp.t);
        } else {
            p.kind = "downwind".into();
            p.pattern_time_s = Some(t_touch - dw[0].t);
        }
    } else if final_len > 1.5 * NM {
        p.kind = "straight_in".into();
        p.pattern_time_s = Some(t_touch - roll.t);
    } else {
        p.pattern_time_s = Some(t_touch - roll.t);
    }
    // calls
    if let Some(a) = p.abeam_nm {
        if a > 1.8 {
            p.calls.push(format!("wide downwind, {a:.1} nm abeam"));
        } else if a < 0.6 {
            p.calls.push(format!("tight downwind, {a:.1} nm abeam"));
        }
    }
    if let Some(h) = p.downwind_alt_ft {
        if h < 600. {
            p.calls.push(format!("low downwind, {h:.0} ft"));
        } else if h > 2500. {
            p.calls.push(format!("high downwind, {h:.0} ft"));
        }
    }
    if let Some(b) = p.base_at_m {
        if b < -1.5 * NM {
            p.calls.push(format!(
                "extended downwind, base {:.1} nm past the threshold",
                -b / NM
            ));
        }
    }
    if p.kind != "straight_in" {
        if let Some(r) = p.rollout_nm {
            if r < 0.3 {
                p.calls.push(format!("short final, rolled out {:.2} nm", r));
            }
        }
    }
    // overshooting the final: the track crosses well beyond the centreline after the roll-out
    let over = s[fin..]
        .iter()
        .filter(|x| x.along < -300.)
        .map(|x| x.across)
        .fold(0., |m: f64, a| if a.abs() > m.abs() { a } else { m });
    if over.abs() > 150. {
        p.calls.push(format!(
            "overshot the final, {:.0} m {} of centreline",
            over.abs(),
            if over > 0. { "right" } else { "left" }
        ));
    }
    p
}

#[derive(Debug, Clone, Copy)]
pub struct TaxiSample {
    pub t: f64,
    pub along: f64,
    pub across: f64,
    pub gs_kts: f64,
    /// world x/z, metres, for distance
    pub x: f64,
    pub z: f64,
}

/// Fast-taxi limit: the usual 25-30 kt straight-line limit.
pub const TAXI_LIMIT_KTS: f64 = 30.;

/// The rollout and taxi after a touchdown at `t_touch`; `s` oldest first, from the touchdown on.
pub fn taxi(
    s: &[TaxiSample],
    t_touch: f64,
    rwy_len: f64,
    rwy_width: f64,
    end: &str,
) -> FieldTaxi {
    let mut t = FieldTaxi { end: end.into(), ..Default::default() };
    if s.is_empty() {
        return t;
    }
    let td = s[0];
    // rollout: down to taxi speed
    let slow = s.iter().position(|x| x.gs_kts <= TAXI_LIMIT_KTS).unwrap_or(s.len() - 1);
    t.rollout_m = (s[slow].along - td.along).max(0.);
    t.rollout_s = s[slow].t - t_touch;
    // clear of the runway: off its rectangle (with a little margin)
    let on_rwy = |x: &TaxiSample| {
        x.across.abs() <= rwy_width / 2. + 4.
            && x.along >= -30.
            && x.along <= rwy_len + 30.
    };
    if let Some(e) = s.iter().position(|x| !on_rwy(x)) {
        t.runway_clear_s = Some(s[e].t - t_touch);
        t.exit_at_m = Some(s[e].along);
    }
    // the taxi: from slowing to taxi speed to the end
    let taxi = &s[slow..];
    t.taxi_time_s = taxi.last().map(|l| l.t - taxi[0].t).unwrap_or(0.);
    t.taxi_distance_m = taxi
        .windows(2)
        .map(|w| ((w[1].x - w[0].x).powi(2) + (w[1].z - w[0].z).powi(2)).sqrt())
        .sum();
    t.max_taxi_kts = taxi.iter().map(|x| x.gs_kts).fold(0., f64::max);
    // stops: moving, then under 1 kt for 3 s or more
    let mut stopped_since: Option<f64> = None;
    let mut counted = false;
    for x in taxi {
        if x.gs_kts < 1. {
            let since = *stopped_since.get_or_insert(x.t);
            if !counted && x.t - since >= 3. {
                t.stops += 1;
                counted = true;
            }
        } else if x.gs_kts > 3. {
            stopped_since = None;
            counted = false;
        }
    }
    if t.max_taxi_kts > TAXI_LIMIT_KTS {
        t.calls.push(format!("fast taxi, {:.0} kt", t.max_taxi_kts));
    }
    match t.runway_clear_s {
        Some(c) if c > 60. => t.calls.push(format!("slow to clear the runway, {c:.0} s")),
        None => t.calls.push("never cleared the runway".into()),
        _ => (),
    }
    if end == "stopped_on_runway" {
        t.calls.push("stopped on the runway".into());
    }
    t
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    /// A left-hand overhead pattern to a runway heading 0 (north): initial at
    /// 1500 ft up the runway, break at the far end, downwind at 1000 ft 1.2 nm
    /// to the left, base past the threshold, a 0.6 nm final.
    pub(crate) fn overhead() -> Vec<RelSample> {
        let mut s = vec![];
        let mut t = 0.;
        let mut push = |along: f64, across: f64, agl_ft: f64, rel: f64, gs: f64| {
            s.push(RelSample {
                t,
                along,
                across,
                agl: agl_ft / M_TO_FT,
                rel,
                gs_kts: gs,
            });
            t += 1.;
        };
        // initial: 3 nm out to 1000 m down the runway, lined up
        let mut a = -3. * NM;
        while a < 1000. {
            push(a, 10., 1500., 0., 300.);
            a += 150.;
        }
        // the break: a 180 to the left, descending
        for k in 0..12 {
            let th = (k as f64 + 0.5) / 12. * std::f64::consts::PI;
            push(
                1000. + th.sin() * 400.,
                -(1. - th.cos()) * 1.2 * NM / 2.,
                1500. - k as f64 * 40.,
                -(k as f64 + 0.5) * 15.,
                250.,
            )
        }
        // downwind: back past the threshold
        let mut a = 1000.;
        while a > -1200. {
            push(a, -1.2 * NM, 1000., 180., 150.);
            a -= 75.;
        }
        // base: the turn in
        for k in 0..10 {
            let th = (k as f64 + 0.5) / 10. * std::f64::consts::PI;
            push(
                -1200. - th.sin() * 350.,
                -1.2 * NM * th.cos().max(0.).max((1. - th / std::f64::consts::PI) * 0.6),
                900. - k as f64 * 30.,
                180. - (k as f64 + 0.5) * 18.,
                140.,
            );
        }
        // final: 0.6 nm in to the touchdown
        let mut a = -0.6 * NM;
        while a < 300. {
            push(a, 2., ((-a).max(0.) * 0.0524 * M_TO_FT).max(0.), 0., 135.);
            a += 60.;
        }
        s
    }

    #[test]
    fn reads_an_overhead_pattern() {
        let s = overhead();
        let td = s.last().unwrap().t;
        let p = pattern(&s, td);
        assert_eq!(p.kind, "overhead", "{p:?}");
        assert_eq!(p.side.as_deref(), Some("left"));
        let ab = p.abeam_nm.unwrap();
        assert!((ab - 1.2).abs() < 0.05, "{ab}");
        assert!((p.downwind_alt_ft.unwrap() - 1000.).abs() < 1.);
        assert!((p.initial_alt_ft.unwrap() - 1500.).abs() < 1.);
        assert!(p.break_at_m.unwrap() > 800.);
        let r = p.rollout_nm.unwrap();
        assert!(r > 0.4 && r < 0.8, "{r}");
        assert!(p.calls.is_empty(), "{:?}", p.calls);
        assert!(p.pattern_time_s.unwrap() > 30.);
        // the legs come in order, and agree with the summary
        let lg = legs(&s, td);
        let mut order: Vec<Leg> = lg.clone();
        order.dedup();
        assert_eq!(
            order,
            vec![Leg::Initial, Leg::Break, Leg::Downwind, Leg::Base, Leg::Final],
            "{order:?}"
        );
        let first_final = lg.iter().position(|l| *l == Leg::Final).unwrap();
        assert!(((-s[first_final].along).max(0.) / NM - r).abs() < 1e-9);
    }

    #[test]
    fn calls_a_wide_extended_downwind() {
        let mut s = overhead();
        for x in s.iter_mut() {
            if (180. - wrap(x.rel).abs()) < 25. {
                x.across *= 2.;
            }
        }
        let p = pattern(&s, s.last().unwrap().t);
        assert!(p.calls.iter().any(|c| c.starts_with("wide downwind")), "{:?}", p.calls);
    }

    #[test]
    fn a_straight_in() {
        let mut s = vec![];
        let mut a = -6. * NM;
        let mut t = 0.;
        while a < 300. {
            s.push(RelSample {
                t,
                along: a,
                across: 0.,
                agl: (-a).max(0.) * 0.0524,
                rel: 1.,
                gs_kts: 140.,
            });
            a += 100.;
            t += 1.4;
        }
        let p = pattern(&s, t - 1.4);
        assert_eq!(p.kind, "straight_in");
        assert!(p.rollout_nm.unwrap() > 5.);
    }

    #[test]
    fn rollout_exit_and_taxi() {
        let mut s = vec![];
        let mut t = 100.;
        let (mut along, mut gs) = (300., 130.);
        // decelerate down the runway
        while gs > 20. {
            s.push(TaxiSample { t, along, across: 0., gs_kts: gs, x: along, z: 0. });
            along += gs * 0.514;
            gs -= 6.;
            t += 1.;
        }
        // turn off and taxi, too fast for a while, one stop, then park
        for k in 0..40 {
            let g = if (10..15).contains(&k) {
                38.
            } else if (20..26).contains(&k) {
                0.
            } else {
                15.
            };
            s.push(TaxiSample {
                t,
                along,
                across: 30. + k as f64 * 8.,
                gs_kts: g,
                x: along,
                z: 30. + k as f64 * 8.,
            });
            t += 1.;
        }
        let x = taxi(&s, 100., 2400., 45., "parked");
        assert!(x.rollout_m > 500. && x.rollout_s > 10., "{x:?}");
        assert!(x.runway_clear_s.unwrap() > x.rollout_s);
        assert_eq!(x.max_taxi_kts, 38.);
        assert_eq!(x.stops, 1);
        assert!(x.calls.iter().any(|c| c.starts_with("fast taxi")));
        assert_eq!(x.end, "parked");
    }
}
