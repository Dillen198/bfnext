//! Pure grading rules, shared so the engine (which grades) and bfdb (which
//! re-derives aggregates and insights) agree on every threshold.

use super::{
    cfg::{AoaBand, ScoringCfg},
    BombQuality, PrecisionQuality, StrafeQuality, WeaponClass,
};

/// The GOOD radius a weapon class is graded against.
pub fn good_radius(cfg: &ScoringCfg, class: WeaponClass) -> f64 {
    match class {
        WeaponClass::Guided | WeaponClass::Missile => cfg.good_guided_m,
        WeaponClass::Rocket => cfg.good_rocket_m,
        WeaponClass::Unguided | WeaponClass::Cluster | WeaponClass::Gun => cfg.good_unguided_m,
    }
}

/// MOOSE RANGE's bands: SHACK inside `shack_m`, EXCELLENT inside half the
/// GOOD radius, GOOD inside it, INEFFECTIVE inside twice it, else POOR.
pub fn bomb_quality(cfg: &ScoringCfg, class: WeaponClass, miss_m: f64) -> BombQuality {
    let good = good_radius(cfg, class);
    if miss_m <= cfg.shack_m {
        BombQuality::Shack
    } else if miss_m <= good * 0.5 {
        BombQuality::Excellent
    } else if miss_m <= good {
        BombQuality::Good
    } else if miss_m <= good * 2. {
        BombQuality::Ineffective
    } else {
        BombQuality::Poor
    }
}

pub fn strafe_quality(cfg: &ScoringCfg, accuracy_pct: f64, valid: bool) -> StrafeQuality {
    if !valid {
        return StrafeQuality::Invalid;
    }
    let [deadeye, excellent, good, ineffective] = cfg.strafe_bands;
    if accuracy_pct >= deadeye {
        StrafeQuality::Deadeye
    } else if accuracy_pct >= excellent {
        StrafeQuality::Excellent
    } else if accuracy_pct >= good {
        StrafeQuality::Good
    } else if accuracy_pct >= ineffective {
        StrafeQuality::Ineffective
    } else {
        StrafeQuality::Poor
    }
}

/// Precision bands scaled from the "perfect" radius: PERFECT inside it,
/// EXCELLENT inside 2x, GOOD 4x, FAIR 8x, else POOR.
pub fn precision_quality(perfect_m: f64, distance_m: f64) -> PrecisionQuality {
    if distance_m <= perfect_m {
        PrecisionQuality::Perfect
    } else if distance_m <= perfect_m * 2. {
        PrecisionQuality::Excellent
    } else if distance_m <= perfect_m * 4. {
        PrecisionQuality::Good
    } else if distance_m <= perfect_m * 8. {
        PrecisionQuality::Fair
    } else {
        PrecisionQuality::Poor
    }
}

/// Clock position of an impact relative to the attack heading. `rel_deg` is
/// the bearing from the target to the impact minus the attack heading; 0 is
/// 12 o'clock (long), 180 is 6 o'clock (short).
pub fn clock(rel_deg: f64) -> u8 {
    let r = rel_deg.rem_euclid(360.);
    let c = ((r + 15.) / 30.).floor() as i64 % 12;
    if c == 0 {
        12
    } else {
        c as u8
    }
}

/// LSO glideslope call for an error in degrees (+ high), MOOSE's big-deck
/// thresholds. Returns `None` when inside the "no call" band.
pub fn glideslope_call(gse_deg: f64) -> Option<(&'static str, super::lso::Magnitude)> {
    use super::lso::Magnitude::*;
    if gse_deg > 1.5 {
        Some(("H", Lot))
    } else if gse_deg > 0.8 {
        Some(("H", Normal))
    } else if gse_deg > 0.4 {
        Some(("H", Little))
    } else if gse_deg < -0.9 {
        Some(("LO", Lot))
    } else if gse_deg < -0.6 {
        Some(("LO", Normal))
    } else if gse_deg < -0.3 {
        Some(("LO", Little))
    } else {
        None
    }
}

/// Lineup call for an error in degrees (+ = right of centreline, which the
/// LSO calls "lined up right").
pub fn lineup_call(lue_deg: f64) -> Option<(&'static str, super::lso::Magnitude)> {
    use super::lso::Magnitude::*;
    let (err, a) = if lue_deg > 0. { ("LUR", lue_deg) } else { ("LUL", -lue_deg) };
    if a > 3.0 {
        Some((err, Lot))
    } else if a > 1.0 {
        Some((err, Normal))
    } else if a > 0.5 {
        Some((err, Little))
    } else {
        None
    }
}

pub fn aoa_call(band: &AoaBand, aoa_deg: f64) -> Option<(&'static str, super::lso::Magnitude)> {
    use super::lso::Magnitude::*;
    if aoa_deg < band.fast_lot {
        Some(("F", Lot))
    } else if aoa_deg < band.fast {
        Some(("F", Normal))
    } else if aoa_deg < band.fast_little {
        Some(("F", Little))
    } else if aoa_deg > band.slow_lot {
        Some(("SLO", Lot))
    } else if aoa_deg > band.slow {
        Some(("SLO", Normal))
    } else if aoa_deg > band.slow_little {
        Some(("SLO", Little))
    } else {
        None
    }
}

/// Automatic waveoff limits (MOOSE): lineup beyond 3 deg, or glideslope below
/// -1.2 / above +1.8 deg, inside the in-close window.
pub fn waveoff_limits(gse_deg: f64, lue_deg: f64) -> bool {
    lue_deg.abs() > 3.0 || gse_deg < -1.2 || gse_deg > 1.8
}

/// AAR grade from its measured components. Returns (score 0..5, letter).
///
/// The score starts at 5 and loses points for: slow join (> 4 min), each
/// disconnect beyond the first, poor stability while connected (a lateral or
/// vertical spread over 1.5 m, fore-aft over 2 m), a hot pre-contact closure
/// (> 5 kt) and an overshoot. No contact at all is an F.
pub fn aar_grade(
    contacts: u32,
    disconnects: u32,
    join_time_s: Option<f64>,
    fore_aft_sd: f64,
    lateral_sd: f64,
    vertical_sd: f64,
    precontact_closure_kts: Option<f64>,
    overshoot: bool,
    calls: &mut Vec<String>,
) -> (f64, String) {
    if contacts == 0 {
        calls.push("No contact made".into());
        return (0.0, "F".into());
    }
    let mut s: f64 = 5.0;
    if let Some(j) = join_time_s {
        if j > 240. {
            let pen = ((j - 240.) / 120.).min(1.0);
            s -= pen;
            calls.push(format!("Slow join-up ({:.0} s to first contact)", j));
        }
    }
    let extra = disconnects.saturating_sub(1) as f64;
    if extra > 0. {
        s -= (extra * 0.5).min(1.5);
        calls.push(format!("{disconnects} disconnects"));
    }
    let spread = |sd: f64, lim: f64| ((sd - lim) / lim).clamp(0., 1.);
    let st = spread(fore_aft_sd, 2.0) * 0.6 + spread(lateral_sd, 1.5) * 0.7 + spread(vertical_sd, 1.5) * 0.7;
    if st > 0.05 {
        s -= st;
        let mut worst = vec![(fore_aft_sd / 2.0, "fore-aft"), (lateral_sd / 1.5, "lateral"), (vertical_sd / 1.5, "vertical")];
        worst.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
        calls.push(format!("Unstable in contact, mostly {}", worst[0].1));
    }
    if let Some(c) = precontact_closure_kts {
        if c > 5. {
            s -= 0.5;
            calls.push(format!("Hot closure at pre-contact ({c:.0} kt)"));
        }
    }
    if overshoot {
        s -= 0.75;
        calls.push("Overshot the tanker".into());
    }
    let s = s.clamp(0., 5.);
    let letter = if s >= 4.5 {
        "A"
    } else if s >= 3.75 {
        "B"
    } else if s >= 3.0 {
        "C"
    } else if s >= 2.0 {
        "D"
    } else {
        "F"
    };
    if calls.is_empty() {
        calls.push("Smooth join and stable in contact".into());
    }
    (s, letter.into())
}

/// A carrier recovery course from DCS's live wind.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RecoveryCourse {
    /// ship heading to steer, degrees true
    pub brc_deg: f64,
    pub speed_kts: f64,
    /// the wind the pilot then gets straight down the landing area, knots
    pub wod_kts: f64,
    /// false when the wind is too light to line the relative wind up with the
    /// landing area at any allowed speed: the ship then steams into the wind
    /// at full speed and the pilot sees a little crosswind
    pub aligned: bool,
}

/// The BRC and ship speed that put the relative wind straight down the
/// landing area at `want_wod_kts`: what a real carrier steers, and what the
/// MOOSE AIRBOSS "into the wind" solver does.
///
/// `wind_from_deg` / `wind_kts` are the true wind at the deck (DCS's
/// `atmosphere.getWind`). `deck_angle_deg` is the landing area relative to
/// the bow (FB - BRC): about -9 on a Nimitz, 0 on a straight deck. The ship's
/// own motion is a wind from dead ahead, so for an angled deck the ship has
/// to point a few degrees right of the true wind for the SUM to come down the
/// angle.
///
/// In the ship's frame (along the bow, to starboard) the true wind (blowing
/// to) must be `W = (s - D cos a, -D sin a)` for a relative wind of `D` from
/// the final bearing, with `s` the ship's speed and `a` the deck angle. With
/// `|W| = w` that gives `s = D cos a - sqrt(w^2 - D^2 sin^2 a)`; when `s`
/// falls outside the allowed speeds it is clamped and `D` solved from it.
pub fn recovery_course(
    wind_from_deg: f64,
    wind_kts: f64,
    deck_angle_deg: f64,
    want_wod_kts: f64,
    min_kts: f64,
    max_kts: f64,
) -> RecoveryCourse {
    let a = deck_angle_deg.to_radians();
    let (sa, ca) = (a.sin(), a.cos());
    let w = wind_kts.max(0.);
    let into_wind = RecoveryCourse {
        brc_deg: wind_from_deg.rem_euclid(360.),
        speed_kts: max_kts,
        wod_kts: w + max_kts,
        aligned: deck_angle_deg.abs() < 0.5,
    };
    if w < 0.5 {
        return into_wind;
    }
    let wod_at = |s: f64| -> Option<f64> {
        let disc = w * w - s * s * sa * sa;
        (disc >= 0.).then(|| s * ca + disc.sqrt())
    };
    let disc = w * w - want_wod_kts * want_wod_kts * sa * sa;
    let (s, d) = if disc >= 0. {
        let s = want_wod_kts * ca - disc.sqrt();
        if s < min_kts {
            match wod_at(min_kts) {
                Some(d) => (min_kts, d),
                None => return into_wind,
            }
        } else if s > max_kts {
            match wod_at(max_kts) {
                Some(d) => (max_kts, d),
                None => return into_wind,
            }
        } else {
            (s, want_wod_kts)
        }
    } else {
        // too little wind to align at the wanted WOD: go fast, which needs
        // the least wind to line up
        match wod_at(max_kts) {
            Some(d) => (max_kts, d),
            None => return into_wind,
        }
    };
    // the true wind in the ship's frame, and so the ship's heading
    let phi = (-d * sa).atan2(s - d * ca).to_degrees();
    let brc = (wind_from_deg + 180. - phi).rem_euclid(360.);
    RecoveryCourse { brc_deg: brc, speed_kts: s, wod_kts: d, aligned: true }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Relative wind (from, kts) a ship on `brc` at `s` kt feels in a true
    /// wind from `wf` at `wk` kt: the two "from" vectors add.
    fn relative(brc: f64, s: f64, wf: f64, wk: f64) -> (f64, f64) {
        let (b, w) = (brc.to_radians(), wf.to_radians());
        let n = s * b.cos() + wk * w.cos();
        let e = s * b.sin() + wk * w.sin();
        (e.atan2(n).to_degrees().rem_euclid(360.), (n * n + e * e).sqrt())
    }

    #[test]
    fn recovery_course_lines_the_wind_up_with_the_angled_deck() {
        for &(wf, wk) in &[(0., 10.), (135., 18.), (270., 6.), (45., 24.)] {
            let c = recovery_course(wf, wk, -9., 27., 5., 30.);
            assert!(c.aligned, "{wf}/{wk}");
            let fb = c.brc_deg - 9.;
            let (from, kts) = relative(c.brc_deg, c.speed_kts, wf, wk);
            let off = (from - fb + 540.).rem_euclid(360.) - 180.;
            assert!(off.abs() < 0.01, "{wf}/{wk}: relative wind {off} deg off the angle");
            assert!((kts - c.wod_kts).abs() < 0.01, "{wf}/{wk}: {kts} vs {}", c.wod_kts);
            assert!((5.0..=30.0).contains(&c.speed_kts));
        }
        // 10 kt from the north: 17.6 kt on 025, FB 016, 27 kt down the angle
        let c = recovery_course(0., 10., -9., 27., 5., 30.);
        assert!((c.brc_deg - 25.).abs() < 0.5, "{}", c.brc_deg);
        assert!((c.speed_kts - 17.6).abs() < 0.2, "{}", c.speed_kts);
        assert!((c.wod_kts - 27.).abs() < 1e-6);
    }

    #[test]
    fn recovery_course_limits() {
        // gale: the slowest allowed speed, more than the wanted WOD
        let c = recovery_course(200., 40., -9., 27., 5., 30.);
        assert_eq!(c.speed_kts, 5.);
        assert!(c.wod_kts > 40.);
        // flat calm: full speed into the (no) wind
        let c = recovery_course(90., 0., -9., 27., 5., 30.);
        assert_eq!(c.speed_kts, 30.);
        assert!(!c.aligned);
        // a straight deck just points into the wind
        let c = recovery_course(310., 12., 0., 27., 5., 30.);
        assert!((c.brc_deg - 310.).abs() < 1e-6);
        assert!((c.speed_kts - 15.).abs() < 1e-6);
    }

    #[test]
    fn bomb_bands_match_funkman_example() {
        let cfg = ScoringCfg::default();
        // GBU-16 at 37 m graded against the unguided 25 m radius is
        // INEFFECTIVE, as on the reference card.
        assert_eq!(bomb_quality(&cfg, WeaponClass::Unguided, 37.), BombQuality::Ineffective);
        assert_eq!(bomb_quality(&cfg, WeaponClass::Unguided, 1.0), BombQuality::Shack);
        assert_eq!(bomb_quality(&cfg, WeaponClass::Unguided, 12.), BombQuality::Excellent);
        assert_eq!(bomb_quality(&cfg, WeaponClass::Unguided, 60.), BombQuality::Poor);
    }

    #[test]
    fn clocks() {
        assert_eq!(clock(0.), 12);
        assert_eq!(clock(180.), 6);
        assert_eq!(clock(90.), 3);
        assert_eq!(clock(-90.), 9);
        assert_eq!(clock(14.), 12);
        assert_eq!(clock(16.), 1);
    }
}
