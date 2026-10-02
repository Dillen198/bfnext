// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Unguided bomb ballistics: a point-mass trajectory model and a one-number
//! drag calibration fitted to the drops actually recorded on the range.
//!
//! DCS's own bomb drag data (`fm.cx_coeff`) feeds a model ED never documented,
//! so rather than guess at it we fit a single drag multiplier per weapon so
//! that this simple model reproduces what the sim really did. The range site
//! runs the SAME model in TypeScript for its release calculator, so the
//! equations below are the contract -- change them in both places or not at
//! all.
//!
//! # Model (3-DOF point mass, flat earth)
//!
//! Frame, origin at the release point's horizontal position:
//! * `x` metres along the release ground track (`Release::heading_deg`),
//! * `y` metres cross-track, positive to the RIGHT of the track,
//! * `z` metres above the impact point's ground (`impact.alt_m` MSL).
//!
//! Every height below is `h = impact.alt_m + z`, metres MSL.
//!
//! Initial state (`dive_deg` > 0 means descending):
//! * `x = y = 0`, `z = release.pos.alt_m - impact.alt_m`
//!   (falls back to `release.alt_agl_m` when the release altitude is 0),
//! * `vx = gs`, `vy = 0`, `vz = -gs * tan(dive)`, where `gs` is
//!   `Release::gs_kts` in m/s (x 0.514444). Both come from the same inertial
//!   velocity vector, so this is exact. When `gs_kts <= 0` the model uses
//!   `tas_kts` instead. `dive` is clamped to [-80, 85] degrees.
//!
//! The air the bomb falls through -- wind `(w_along(h), w_cross(h))` and
//! density `rho(h)` -- comes from one of two sources:
//!
//! ## DCS atmosphere (`Release::atmo` has at least one usable layer)
//!
//! The layers are what DCS itself reported over the release point, so the
//! model flies the bomb through the mission's real wind and air at every
//! height. A layer is usable when all its fields are finite,
//! `pressure_hpa > 0` and `temp_c > -273.15`; the rest are dropped. The
//! usable layers are sorted by `alt_m` ascending (a stable sort).
//!
//! Per layer: `to = wind_from_deg + 180` (the direction it blows TO),
//! `n = wind_kts * 0.514444 * cos(to)`, `e = wind_kts * 0.514444 * sin(to)`
//! (m/s, north / east), `T = temp_c + 273.15` (K), `p = pressure_hpa * 100`
//! (Pa).
//!
//! At height `h`, with `first` / `last` the lowest / highest layer:
//! * `h <= first.alt_m`: `n, e, T` of `first`,
//!   `p = first.p * exp(-g (h - first.alt_m) / (287.053 first.T))`;
//! * `h >= last.alt_m`: the same with `last`;
//! * otherwise the bracketing pair `lo, hi = lo + 1`, where `lo` is the
//!   highest-index layer with `alt_m <= h` (so `hi.alt_m > h`), and
//!   `f = (h - lo.alt_m) / (hi.alt_m - lo.alt_m)`:
//!   `n, e, T` linear in `f`; `p = exp(ln lo.p + f (ln hi.p - ln lo.p))`
//!   (log-linear).
//!
//! Then, with `H = heading_deg`: `w_along = n cos(H) + e sin(H)`,
//! `w_cross = e cos(H) - n sin(H)` (the constant-wind formula below, per
//! height), and `rho = p / (287.053 T)`. No height clamp.
//!
//! ## Constant wind + ISA (no usable layer: records from older engines)
//!
//! Wind is `Release::wind_kts` blowing FROM `wind_from_deg` (meteorological),
//! assumed constant with height. Its components in the track frame, with
//! `a = (wind_from_deg + 180) - heading_deg`:
//! * `w_along = W * cos(a)`, `w_cross = W * sin(a)` (m/s).
//!
//! `rho(h)` is ISA density:
//! troposphere (`h <= 11000`): `T = 288.15 - 0.0065 h`,
//! `p = 101325 (T / 288.15)^5.255877`; above: `T = 216.65`,
//! `p = 22632.06 exp(-0.000157688 (h - 11000))`; `rho = p / (287.053 T)`.
//! `h` is clamped to [-500, 20000].
//!
//! ## Equations of motion
//!
//! Air-relative velocity `va = (vx - w_along(h), vy - w_cross(h), vz)`.
//!
//! Acceleration:
//! ```text
//! a = -(k * 0.5 * rho(h) * |va| * Cd_ref * A / m) * va  +  (0, 0, -g)
//! ```
//! * `g = 9.80665`, `m = WeaponBallistics::mass_kg`,
//! * `A = pi * (caliber_m / 2)^2`,
//! * `Cd_ref = cx_coeff[0]` when it lies in 0.05..=0.8 (a plausible subsonic
//!   drag coefficient for a bomb body), else 0.3. DCS usually writes a literal
//!   1 there (a scale factor, not a Cd), in which case 0.3 applies,
//! * `k` is the fitted drag multiplier.
//!
//! Integration: classic 4th-order Runge-Kutta, fixed step `dt = 0.02 s`,
//! until `z <= 0` (or 180 s). The impact is linearly interpolated inside the
//! last step: `f = z_prev / (z_prev - z_next)`, impact = `prev + f (next -
//! prev)`, time of flight likewise.
//!
//! Output ground range = `sqrt(x^2 + y^2)` at impact.
//!
//! # Calibration
//!
//! For each unguided bomb type with at least [`MIN_SAMPLES`] recorded Bomb
//! results, the measured ground range is the horizontal distance from
//! `release.pos` to `impact` (local flat earth: `dN = dlat * R`,
//! `dE = dlon * R * cos(mean lat)`, `R = 6371008.8 m`, angles in radians).
//! `k` minimises the RMS of (model range - measured range) over those samples,
//! found by golden-section search over `k` in [0.2, 5] to a bracket width of
//! 1e-4. `residual_m` is that RMS at the fitted `k`. Each drop is flown
//! through its own recorded DCS atmosphere when it has one (`layered` counts
//! those), else through the constant wind + ISA.

use bfprotocols::range::{AtmoLayer, BombResult, GeoPt, WeaponBallistics};
use serde::Serialize;

pub(crate) const G: f64 = 9.80665;
pub(crate) const DT: f64 = 0.02;
const KT: f64 = 0.514444;
/// specific gas constant of dry air, J/(kg K)
const R_AIR: f64 = 287.053;
const MAX_T: f64 = 180.0;
pub(crate) const K_MIN: f64 = 0.2;
pub(crate) const K_MAX: f64 = 5.0;
/// Fewest recorded drops a weapon needs before it is calibrated.
pub(crate) const MIN_SAMPLES: usize = 8;
/// Most recent drops used per weapon.
const MAX_SAMPLES: usize = 200;
const EARTH_R: f64 = 6371008.8;

/// ISA air density, kg/m^3, at `h` metres MSL.
pub(crate) fn isa_density(h: f64) -> f64 {
    let h = h.clamp(-500., 20000.);
    let (t, p) = if h <= 11000. {
        let t = 288.15 - 0.0065 * h;
        (t, 101325. * (t / 288.15).powf(5.255877))
    } else {
        (216.65, 22632.06 * (-0.000157688 * (h - 11000.)).exp())
    };
    p / (R_AIR * t)
}

/// One usable DCS atmosphere layer in the model's units.
#[derive(Debug, Clone, Copy, PartialEq)]
struct Level {
    /// metres MSL
    h: f64,
    /// wind blowing TO the north / east, m/s
    n: f64,
    e: f64,
    /// K
    t: f64,
    /// Pa, and its natural log
    p: f64,
    ln_p: f64,
}

/// DCS's atmosphere over a drop, ready for the integrator: the usable layers
/// sorted by height, and the release track to resolve the wind against. See
/// the module doc ("DCS atmosphere").
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Atmo {
    levels: Vec<Level>,
    cos_h: f64,
    sin_h: f64,
}

/// The air at one height: wind in the track frame and density.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Air {
    pub(crate) wind_along_mps: f64,
    /// + = blowing to the right of the track
    pub(crate) wind_cross_mps: f64,
    pub(crate) rho: f64,
}

fn usable(l: &AtmoLayer) -> bool {
    [l.alt_m, l.wind_from_deg, l.wind_kts, l.temp_c, l.pressure_hpa].iter().all(|v| v.is_finite())
        && l.pressure_hpa > 0.
        && l.temp_c > -273.15
}

impl Atmo {
    /// `None` when no layer is usable (e.g. a record from an older engine):
    /// the model then uses the constant wind and ISA.
    pub(crate) fn new(layers: &[AtmoLayer], heading_deg: f64) -> Option<Self> {
        let mut levels: Vec<Level> = layers
            .iter()
            .filter(|l| usable(l))
            .map(|l| {
                let to = (l.wind_from_deg + 180.).to_radians();
                let w = l.wind_kts * KT;
                let p = l.pressure_hpa * 100.;
                Level { h: l.alt_m, n: w * to.cos(), e: w * to.sin(), t: l.temp_c + 273.15, p, ln_p: p.ln() }
            })
            .collect();
        if levels.is_empty() {
            return None;
        }
        // stable, so equal heights keep their order (the TS port relies on it)
        levels.sort_by(|a, b| a.h.total_cmp(&b.h));
        let hdg = heading_deg.to_radians();
        Some(Self { levels, cos_h: hdg.cos(), sin_h: hdg.sin() })
    }

    /// Wind and density at `h` metres MSL.
    pub(crate) fn at(&self, h: f64) -> Air {
        let lv = &self.levels;
        let (first, last) = (&lv[0], &lv[lv.len() - 1]);
        // outside the profile: the end layer's wind and temperature, and
        // pressure from the hydrostatic equation at that temperature
        let (n, e, t, p) = if h <= first.h {
            (first.n, first.e, first.t, first.p * (-G * (h - first.h) / (R_AIR * first.t)).exp())
        } else if h >= last.h {
            (last.n, last.e, last.t, last.p * (-G * (h - last.h) / (R_AIR * last.t)).exp())
        } else {
            // lv[lo].h <= h < lv[hi].h throughout
            let (mut lo, mut hi) = (0, lv.len() - 1);
            while hi - lo > 1 {
                let mid = (lo + hi) / 2;
                if lv[mid].h <= h {
                    lo = mid
                } else {
                    hi = mid
                }
            }
            let (a, b) = (&lv[lo], &lv[hi]);
            let f = (h - a.h) / (b.h - a.h);
            (
                a.n + f * (b.n - a.n),
                a.e + f * (b.e - a.e),
                a.t + f * (b.t - a.t),
                (a.ln_p + f * (b.ln_p - a.ln_p)).exp(),
            )
        };
        Air {
            wind_along_mps: n * self.cos_h + e * self.sin_h,
            wind_cross_mps: e * self.cos_h - n * self.sin_h,
            rho: p / (R_AIR * t),
        }
    }
}

/// The drag-relevant properties of a bomb.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Body {
    pub(crate) mass_kg: f64,
    pub(crate) area_m2: f64,
    pub(crate) cd_ref: f64,
}

impl Body {
    /// `None` when the weapon database entry is unusable (no mass/caliber).
    pub(crate) fn from_weapon(w: &WeaponBallistics) -> Option<Self> {
        if !(w.mass_kg > 0.) || !(w.caliber_m > 0.) {
            return None;
        }
        Some(Self {
            mass_kg: w.mass_kg,
            area_m2: std::f64::consts::PI * (w.caliber_m / 2.).powi(2),
            cd_ref: cd_ref(&w.cx_coeff),
        })
    }
}

/// See the module doc: `cx_coeff[0]` if it looks like a drag coefficient.
pub(crate) fn cd_ref(cx: &[f64]) -> f64 {
    match cx.first() {
        Some(c) if (0.05..=0.8).contains(c) => *c,
        _ => 0.3,
    }
}

/// Release conditions in the model's own terms.
#[derive(Debug, Clone)]
pub(crate) struct Launch {
    /// height above the impact ground, m
    pub(crate) z0_m: f64,
    /// MSL height of the impact ground, m
    pub(crate) ground_msl_m: f64,
    /// inertial horizontal speed along track, m/s
    pub(crate) gs_mps: f64,
    /// flight-path dive angle, degrees, + = descending
    pub(crate) dive_deg: f64,
    /// the constant wind, used only when `atmo` is `None`
    pub(crate) wind_along_mps: f64,
    pub(crate) wind_cross_mps: f64,
    /// DCS's layered atmosphere; when present it replaces the constant wind
    /// and ISA entirely
    pub(crate) atmo: Option<Atmo>,
}

impl Launch {
    /// Wind and density at `h` metres MSL.
    pub(crate) fn air(&self, h: f64) -> Air {
        match &self.atmo {
            Some(a) => a.at(h),
            None => Air { wind_along_mps: self.wind_along_mps, wind_cross_mps: self.wind_cross_mps, rho: isa_density(h) },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Impact {
    pub(crate) along_m: f64,
    pub(crate) cross_m: f64,
    pub(crate) range_m: f64,
    pub(crate) tof_s: f64,
}

type State = [f64; 6];

fn deriv(s: &State, b: &Body, l: &Launch, k: f64) -> State {
    let air = l.air(l.ground_msl_m + s[2]);
    let va = [s[3] - air.wind_along_mps, s[4] - air.wind_cross_mps, s[5]];
    let speed = (va[0] * va[0] + va[1] * va[1] + va[2] * va[2]).sqrt();
    let c = k * 0.5 * air.rho * speed * b.cd_ref * b.area_m2 / b.mass_kg;
    [s[3], s[4], s[5], -c * va[0], -c * va[1], -c * va[2] - G]
}

fn add(a: &State, b: &State, h: f64) -> State {
    let mut o = *a;
    for i in 0..6 {
        o[i] += b[i] * h;
    }
    o
}

/// Fly one bomb. `k` is the drag multiplier (0 = vacuum).
pub(crate) fn simulate(b: &Body, l: &Launch, k: f64) -> Impact {
    let dive = l.dive_deg.clamp(-80., 85.).to_radians();
    let mut s: State = [0., 0., l.z0_m.max(0.), l.gs_mps, 0., -l.gs_mps * dive.tan()];
    let mut t = 0.;
    if s[2] <= 0. {
        return Impact { along_m: 0., cross_m: 0., range_m: 0., tof_s: 0. };
    }
    while t < MAX_T {
        let k1 = deriv(&s, b, l, k);
        let k2 = deriv(&add(&s, &k1, DT / 2.), b, l, k);
        let k3 = deriv(&add(&s, &k2, DT / 2.), b, l, k);
        let k4 = deriv(&add(&s, &k3, DT), b, l, k);
        let mut n = s;
        for i in 0..6 {
            n[i] += DT / 6. * (k1[i] + 2. * k2[i] + 2. * k3[i] + k4[i]);
        }
        if n[2] <= 0. {
            let f = s[2] / (s[2] - n[2]);
            let x = s[0] + f * (n[0] - s[0]);
            let y = s[1] + f * (n[1] - s[1]);
            return Impact { along_m: x, cross_m: y, range_m: x.hypot(y), tof_s: t + f * DT };
        }
        s = n;
        t += DT;
    }
    Impact { along_m: s[0], cross_m: s[1], range_m: s[0].hypot(s[1]), tof_s: t }
}

/// Horizontal distance between two points, local flat earth.
pub(crate) fn ground_distance(a: &GeoPt, b: &GeoPt) -> f64 {
    let lat = ((a.lat + b.lat) / 2.).to_radians();
    let dn = (b.lat - a.lat).to_radians() * EARTH_R;
    let de = (b.lon - a.lon).to_radians() * EARTH_R * lat.cos();
    dn.hypot(de)
}

/// One recorded drop reduced to what the fit needs.
#[derive(Debug, Clone)]
pub(crate) struct Sample {
    pub(crate) launch: Launch,
    pub(crate) measured_range_m: f64,
}

impl Sample {
    /// `None` when the record lacks the geometry (no release/impact position).
    pub(crate) fn from_bomb(b: &BombResult) -> Option<Self> {
        let r = &b.release;
        if (r.pos.lat == 0. && r.pos.lon == 0.) || (b.impact.lat == 0. && b.impact.lon == 0.) {
            return None;
        }
        let z0 = if r.pos.alt_m != 0. { r.pos.alt_m - b.impact.alt_m } else { r.alt_agl_m };
        let gs = if r.gs_kts > 0. { r.gs_kts } else { r.tas_kts } * KT;
        if !(z0 > 0.) || !(gs > 0.) {
            return None;
        }
        let a = (r.wind_from_deg + 180. - r.heading_deg).to_radians();
        let w = r.wind_kts * KT;
        Some(Self {
            launch: Launch {
                z0_m: z0,
                ground_msl_m: b.impact.alt_m,
                gs_mps: gs,
                dive_deg: r.dive_deg,
                wind_along_mps: w * a.cos(),
                wind_cross_mps: w * a.sin(),
                atmo: Atmo::new(&r.atmo, r.heading_deg),
            },
            measured_range_m: ground_distance(&r.pos, &b.impact),
        })
    }
}

/// RMS range error of the model at `k` over `samples`.
pub(crate) fn rms(b: &Body, samples: &[Sample], k: f64) -> f64 {
    if samples.is_empty() {
        return 0.;
    }
    let se: f64 = samples
        .iter()
        .map(|s| {
            let e = simulate(b, &s.launch, k).range_m - s.measured_range_m;
            e * e
        })
        .sum();
    (se / samples.len() as f64).sqrt()
}

/// Golden-section search for the `k` in [K_MIN, K_MAX] minimising `rms`.
/// Returns (k, rms at k).
pub(crate) fn fit(b: &Body, samples: &[Sample]) -> (f64, f64) {
    let phi = (5f64.sqrt() - 1.) / 2.;
    let (mut lo, mut hi) = (K_MIN, K_MAX);
    let mut c = hi - phi * (hi - lo);
    let mut d = lo + phi * (hi - lo);
    let mut fc = rms(b, samples, c);
    let mut fd = rms(b, samples, d);
    while hi - lo > 1e-4 {
        if fc < fd {
            hi = d;
            d = c;
            fd = fc;
            c = hi - phi * (hi - lo);
            fc = rms(b, samples, c);
        } else {
            lo = c;
            c = d;
            fc = fd;
            d = lo + phi * (hi - lo);
            fd = rms(b, samples, d);
        }
    }
    let k = (lo + hi) / 2.;
    (k, rms(b, samples, k))
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct Calibration {
    pub(crate) weapon: String,
    pub(crate) samples: usize,
    /// how many of `samples` were flown through their recorded DCS
    /// atmosphere (the rest through constant wind + ISA)
    pub(crate) layered: usize,
    pub(crate) drag_scale: f64,
    pub(crate) residual_m: f64,
    /// the inputs the site needs to reproduce the model
    pub(crate) cd_ref: f64,
    pub(crate) mass_kg: f64,
    pub(crate) caliber_m: f64,
}

/// Fit every unguided weapon with enough recorded drops. `drops` is
/// (weapon type name, result), newest first.
pub(crate) fn calibrate(weapons: &[WeaponBallistics], drops: &[(String, BombResult)]) -> Vec<Calibration> {
    let mut by: std::collections::BTreeMap<&str, Vec<Sample>> = Default::default();
    for (w, b) in drops {
        let v = by.entry(w.as_str()).or_default();
        if v.len() >= MAX_SAMPLES {
            continue;
        }
        if let Some(s) = Sample::from_bomb(b) {
            v.push(s);
        }
    }
    let mut out = vec![];
    for (name, samples) in by {
        if samples.len() < MIN_SAMPLES {
            continue;
        }
        let Some(w) = weapons.iter().find(|w| w.name == name) else {
            continue;
        };
        let Some(body) = Body::from_weapon(w) else {
            continue;
        };
        let (k, res) = fit(&body, &samples);
        out.push(Calibration {
            weapon: name.to_string(),
            samples: samples.len(),
            layered: samples.iter().filter(|s| s.launch.atmo.is_some()).count(),
            drag_scale: (k * 1000.).round() / 1000.,
            residual_m: (res * 10.).round() / 10.,
            cd_ref: body.cd_ref,
            mass_kg: w.mass_kg,
            caliber_m: w.caliber_m,
        });
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use bfprotocols::range::{BombQuality, Release, WeaponClass};

    fn mk82() -> Body {
        Body { mass_kg: 241., area_m2: std::f64::consts::PI * (0.273f64 / 2.).powi(2), cd_ref: 0.3 }
    }

    fn launch(z0: f64, gs: f64, dive: f64) -> Launch {
        Launch {
            z0_m: z0,
            ground_msl_m: 0.,
            gs_mps: gs,
            dive_deg: dive,
            wind_along_mps: 0.,
            wind_cross_mps: 0.,
            atmo: None,
        }
    }

    /// A DCS layer at `h` m with ISA temperature and pressure.
    fn layer(h: f64, from: f64, kts: f64) -> AtmoLayer {
        let t = 288.15 - 0.0065 * h;
        AtmoLayer {
            alt_m: h,
            wind_from_deg: from,
            wind_kts: kts,
            temp_c: t - 273.15,
            pressure_hpa: 101325. * (t / 288.15).powf(5.255877) / 100.,
        }
    }

    fn bomb(release: Release, impact: GeoPt) -> BombResult {
        BombResult {
            station_id: "s".into(),
            range: "r".into(),
            target: "t".into(),
            weapon: "Mk_82".into(),
            weapon_display: String::new(),
            weapon_class: WeaponClass::Unguided,
            guidance: "none".into(),
            release,
            target_pos: impact,
            impact,
            impact_north_m: 0.,
            impact_east_m: 0.,
            miss_m: 0.,
            radial_deg: 0.,
            clock: 12,
            long_m: 0.,
            cross_m: 0.,
            time_of_flight_s: 0.,
            quality: BombQuality::Good,
            target_hit: false,
            laser_code: None,
            rings_m: vec![],
            good_radius_m: 0.,
        }
    }

    #[test]
    fn isa_points() {
        assert!((isa_density(0.) - 1.2250).abs() < 1e-3);
        assert!((isa_density(5000.) - 0.7364).abs() < 2e-3);
        assert!((isa_density(11000.) - 0.3639).abs() < 2e-3);
    }

    #[test]
    fn vacuum_matches_closed_form() {
        let l = launch(1000., 200., 0.);
        let i = simulate(&mk82(), &l, 0.);
        let t = (2000. / G).sqrt();
        assert!((i.tof_s - t).abs() < 0.01, "tof {}", i.tof_s);
        assert!((i.range_m - 200. * t).abs() < 1.0, "range {}", i.range_m);
        assert!(i.cross_m.abs() < 1e-9);
    }

    #[test]
    fn drag_and_wind_behave() {
        let b = mk82();
        let l = launch(3000., 230., 20.);
        let r0 = simulate(&b, &l, 0.5).range_m;
        let r1 = simulate(&b, &l, 1.0).range_m;
        let r2 = simulate(&b, &l, 2.0).range_m;
        assert!(r0 > r1 && r1 > r2);
        let tail = Launch { wind_along_mps: 15., ..l.clone() };
        assert!(simulate(&b, &tail, 1.0).range_m > r1);
        let cross = Launch { wind_cross_mps: 10., ..l.clone() };
        assert!(simulate(&b, &cross, 1.0).cross_m > 0.);
    }

    #[test]
    fn fit_recovers_known_drag() {
        let b = mk82();
        let truth = 1.7;
        let samples: Vec<Sample> = [
            (1500., 220., 0.),
            (2500., 240., 10.),
            (3500., 250., 30.),
            (1200., 200., 5.),
            (4000., 260., 45.),
            (3000., 230., 20.),
            (2000., 210., 15.),
            (5000., 270., 0.),
        ]
        .iter()
        .map(|&(z, v, d)| {
            let l = launch(z, v, d);
            let measured_range_m = simulate(&b, &l, truth).range_m;
            Sample { launch: l, measured_range_m }
        })
        .collect();
        let (k, res) = fit(&b, &samples);
        assert!((k - truth).abs() < 0.01, "k {k}");
        assert!(res < 0.5, "residual {res}");
    }

    #[test]
    fn cd_ref_rule() {
        assert_eq!(cd_ref(&[1., 0.39, 0.38]), 0.3);
        assert_eq!(cd_ref(&[0.25]), 0.25);
        assert_eq!(cd_ref(&[]), 0.3);
    }

    /// (a) A DCS profile that is the constant wind and a standard day at
    /// every height flies the bomb like the constant-wind + ISA model.
    #[test]
    fn uniform_profile_matches_constant_model() {
        let b = mk82();
        let (hdg, from, kts) = (75., 300., 25.);
        let layers: Vec<AtmoLayer> = (0..=80).map(|i| layer(i as f64 * 100., from, kts)).collect();
        let a = (from + 180. - hdg).to_radians();
        let w = kts * KT;
        for (z0, ground, gs, dive) in [(1500., 200., 230., 0.), (4500., 350., 250., 30.), (6000., 0., 240., 10.)] {
            let flat = Launch {
                z0_m: z0,
                ground_msl_m: ground,
                gs_mps: gs,
                dive_deg: dive,
                wind_along_mps: w * a.cos(),
                wind_cross_mps: w * a.sin(),
                atmo: None,
            };
            let layered = Launch { atmo: Atmo::new(&layers, hdg), ..flat.clone() };
            assert!(layered.atmo.is_some());
            let (i0, i1) = (simulate(&b, &flat, 1.1), simulate(&b, &layered, 1.1));
            assert!((i0.along_m - i1.along_m).abs() < 0.03, "along {} vs {}", i0.along_m, i1.along_m);
            assert!((i0.cross_m - i1.cross_m).abs() < 0.03, "cross {} vs {}", i0.cross_m, i1.cross_m);
            assert!((i0.tof_s - i1.tof_s).abs() < 1e-3, "tof {} vs {}", i0.tof_s, i1.tof_s);
        }
    }

    /// (b) Wind that only exists above 2000 m moves a bomb released above it
    /// and leaves one released at 1000 m exactly where it was.
    #[test]
    fn wind_aloft_only_moves_a_high_release() {
        let b = mk82();
        let hdg = 90.;
        // blowing towards the attack course: a tailwind
        let prof = |tail: f64| -> Vec<AtmoLayer> {
            [0., 1000., 2000.]
                .iter()
                .map(|&h| layer(h, hdg + 180., 0.))
                .chain([2500., 4000., 6000., 8000.].iter().map(|&h| layer(h, hdg + 180., tail)))
                .collect()
        };
        let (calm, windy) = (prof(0.), prof(40.));
        let fly = |z0: f64, layers: &[AtmoLayer]| {
            simulate(&b, &Launch { atmo: Atmo::new(layers, hdg), ..launch(z0, 230., 0.) }, 1.0)
        };
        assert_eq!(fly(1000., &calm), fly(1000., &windy));
        let (hi_calm, hi_wind) = (fly(6000., &calm), fly(6000., &windy));
        assert!(hi_wind.along_m > hi_calm.along_m + 50., "{} vs {}", hi_wind.along_m, hi_calm.along_m);
        assert!(hi_wind.cross_m.abs() < 1e-6);
    }

    /// (c) Outside the profile the end layer holds (pressure hydrostatic);
    /// inside, wind and temperature are linear and pressure log-linear.
    #[test]
    fn profile_clamps_outside_and_interpolates_inside() {
        let lay = |alt_m, wind_from_deg, wind_kts, temp_c, pressure_hpa| AtmoLayer {
            alt_m,
            wind_from_deg,
            wind_kts,
            temp_c,
            pressure_hpa,
        };
        // out of order, and the 1000 m layer has no pressure so is dropped
        let a = Atmo::new(
            &[lay(3000., 0., 30., -4.5, 701.), lay(1000., 0., 0., 0., 0.), lay(500., 270., 10., 15., 955.)],
            0.,
        )
        .unwrap();
        assert_eq!(a.levels.len(), 2);
        // heading north: along = the north component, cross = the east one
        for h in [-200., 0., 500.] {
            let air = a.at(h);
            assert!(air.wind_along_mps.abs() < 1e-9, "{h}");
            assert!((air.wind_cross_mps - 10. * KT).abs() < 1e-9, "{h}");
            let t = 15. + 273.15;
            let p = 95500. * (-G * (h - 500.) / (R_AIR * t)).exp();
            assert!((air.rho - p / (R_AIR * t)).abs() < 1e-12, "{h}");
        }
        for h in [3000., 4000., 12000.] {
            let air = a.at(h);
            assert!((air.wind_along_mps + 30. * KT).abs() < 1e-9, "{h}");
            assert!(air.wind_cross_mps.abs() < 1e-9, "{h}");
            let t = -4.5 + 273.15;
            let p = 70100. * (-G * (h - 3000.) / (R_AIR * t)).exp();
            assert!((air.rho - p / (R_AIR * t)).abs() < 1e-12, "{h}");
        }
        let mid = a.at(1750.);
        assert!((mid.wind_along_mps + 15. * KT).abs() < 1e-9);
        assert!((mid.wind_cross_mps - 5. * KT).abs() < 1e-9);
        let t = (15. - 4.5) / 2. + 273.15;
        assert!((mid.rho - (95500f64 * 70100.).sqrt() / (R_AIR * t)).abs() < 1e-9);
        // nothing usable: the constant model
        assert!(Atmo::new(&[lay(0., 0., 5., 15., 0.)], 0.).is_none());
        assert!(Atmo::new(&[], 0.).is_none());
    }

    #[test]
    fn samples_fly_their_own_recorded_atmosphere() {
        let mut r = Release {
            pos: GeoPt { lat: 42., lon: 42., alt_m: 3000. },
            gs_kts: 450.,
            heading_deg: 90.,
            dive_deg: 10.,
            wind_from_deg: 270.,
            wind_kts: 10.,
            ..Default::default()
        };
        let impact = GeoPt { lat: 42., lon: 42.05, alt_m: 100. };
        let old = Sample::from_bomb(&bomb(r.clone(), impact)).unwrap();
        assert!(old.launch.atmo.is_none());
        let layers = vec![layer(100., 270., 10.), layer(3000., 250., 35.)];
        r.atmo = layers.clone();
        let new = Sample::from_bomb(&bomb(r, impact)).unwrap();
        assert_eq!(new.launch.atmo, Atmo::new(&layers, 90.));
        assert_eq!(new.launch.wind_along_mps, old.launch.wind_along_mps);
        assert_eq!(new.measured_range_m, old.measured_range_m);
        let b = mk82();
        assert!((simulate(&b, &new.launch, 1.).cross_m - simulate(&b, &old.launch, 1.).cross_m).abs() > 1.);
    }

    #[test]
    fn fit_recovers_known_drag_through_a_dcs_profile() {
        let b = mk82();
        let truth = 1.3;
        // veering and strengthening with height, as DCS writes it
        let layers: Vec<AtmoLayer> = [0., 1000., 3000., 6000., 9000.]
            .iter()
            .enumerate()
            .map(|(i, &h)| layer(h, 250. + 8. * i as f64, 8. + 9. * i as f64))
            .collect();
        let samples: Vec<Sample> = [
            (1500., 220., 0., 10.),
            (2500., 240., 10., 100.),
            (3500., 250., 30., 190.),
            (1200., 200., 5., 280.),
            (4000., 260., 45., 45.),
            (3000., 230., 20., 135.),
            (2000., 210., 15., 225.),
            (5000., 270., 0., 315.),
        ]
        .iter()
        .map(|&(z, v, d, hdg)| {
            let l = Launch { atmo: Atmo::new(&layers, hdg), ..launch(z, v, d) };
            let measured_range_m = simulate(&b, &l, truth).range_m;
            Sample { launch: l, measured_range_m }
        })
        .collect();
        let (k, res) = fit(&b, &samples);
        assert!((k - truth).abs() < 0.01, "k {k}");
        assert!(res < 0.5, "residual {res}");
    }
}
