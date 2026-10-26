/*
Copyright 2024 Eric Stokes.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
for more details.
*/

//! Decisions made by the event handlers in lib.rs, pulled out as plain
//! functions so they can be tested without DCS: takeoff confirmation and the
//! runway check, the era weapon rules, and when the reactive air response
//! scrambles.

use chrono::{prelude::*, Duration};
use dcso3::Vector2;

/// A held takeoff counts once the aircraft is this high above the ground...
pub(crate) const TAKEOFF_CONFIRM_AGL_M: f64 = 30.;
/// ...or has stayed in the air this long (a helicopter departing low).
pub(crate) const TAKEOFF_CONFIRM_SECS: i64 = 30;

/// Has an aircraft that left the ground climbed away for real (rather than
/// bounced or hopped)?
pub(crate) fn takeoff_confirmed(agl_m: f64, airborne_for: Duration) -> bool {
    agl_m >= TAKEOFF_CONFIRM_AGL_M || airborne_for >= Duration::seconds(TAKEOFF_CONFIRM_SECS)
}

/// Where a lift-off point is relative to one runway.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum RunwayFit {
    /// On the runway (or a "runway" with no rectangle, like a carrier deck).
    On,
    /// Off it; the closest projection onto its axes, for the log.
    Off { along: f64, across: f64 },
}

/// Is the lift-off point `(dn, de)` -- metres north and east of the runway's
/// centre -- on a runway of this `course` (radians), `length` and `width`?
///
/// DCS's `course` has an inconsistent sign convention across terrains and
/// versions, so both `course` and `-course` are tried and either may accept.
/// The margins are generous on purpose: fast jets float well past the far
/// threshold before the takeoff event, `getRunways()`'s length and centre are
/// not always the full paved surface, and its course is a few degrees off on
/// some fields -- an angular error that grows into a lateral one the farther
/// down the runway the wheels leave the ground.
pub(crate) fn runway_fit(dn: f64, de: f64, course: f64, length: f64, width: f64) -> RunwayFit {
    // Carriers / LHAs (and the odd heliport) report length 0, width 0 and a
    // junk course: there is no rectangle to be off of.
    if length < 1.0 || width < 1.0 {
        return RunwayFit::On;
    }
    let end_margin = length / 2.0 + 1500.0;
    // 0.06 ~= 3.4 degrees of slop in the reported course.
    let side_margin = |along: f64| width / 2.0 + 60.0 + along.abs() * 0.06;
    let mut best: Option<(f64, f64)> = None;
    for course in [course, -course] {
        let (s, c) = course.sin_cos();
        let along = dn * c + de * s;
        let across = -dn * s + de * c;
        if along.abs() <= end_margin && across.abs() <= side_margin(along) {
            return RunwayFit::On;
        }
        if best.map(|(_, a): (f64, f64)| across.abs() < a.abs()).unwrap_or(true) {
            best = Some((along, across));
        }
    }
    let (along, across) = best.unwrap_or((0., 0.));
    RunwayFit::Off { along, across }
}

/// The banned weapons in a loadout, by the name the player knows them by,
/// each once. `ammo` is `(type name, display name)` per pylon entry; either
/// name can be the banned one.
pub(crate) fn banned_weapon_labels(
    ammo: impl IntoIterator<Item = (String, String)>,
    banned: impl Fn(&str) -> bool,
) -> Vec<String> {
    let mut found: Vec<String> = vec![];
    for (typ, shown) in ammo {
        if banned(&typ) || banned(&shown) {
            let label = if shown.is_empty() { typ } else { shown };
            if !found.contains(&label) {
                found.push(label);
            }
        }
    }
    found
}

/// Seconds a pilot gets to land with a banned weapon: the configured time,
/// but never less than 30.
pub(crate) fn banned_land_secs(configured: u32) -> u32 {
    configured.max(30)
}

/// What to do about a pilot who took off with a banned weapon.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BannedVerdict {
    /// Landed or left the slot: forgiven (the next takeoff checks again).
    Forgive,
    /// Still airborne past the deadline: remove them.
    Remove,
    /// Still airborne, still in time.
    Wait,
}

/// `in_air` is `None` when the pilot is no longer in the slot they took off
/// in.
pub(crate) fn banned_loadout_verdict(
    in_air: Option<bool>,
    now: DateTime<Utc>,
    deadline: DateTime<Utc>,
) -> BannedVerdict {
    match in_air {
        None | Some(false) => BannedVerdict::Forgive,
        Some(true) if now >= deadline => BannedVerdict::Remove,
        Some(true) => BannedVerdict::Wait,
    }
}

/// Can one more reactive-air wave start for a side with `side_active` waves
/// up, when `total_active` are up across both sides?
pub(crate) fn wave_has_room(
    side_active: usize,
    total_active: usize,
    max_per_side: usize,
    max_concurrent: usize,
) -> bool {
    side_active < max_per_side && total_active < max_concurrent
}

/// Why a side scrambles, if it does.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Scramble {
    No,
    /// Enough enemy players detected (or airborne).
    Threat,
    /// Not enough of a threat, but outnumbered in the air by `balance_gap`
    /// or more.
    Balance,
}

/// Is a side with `my_air` players up outnumbered by `balance_gap` or more?
/// A gap of 0 turns air balancing off.
pub(crate) fn outnumbered(my_air: usize, their_air: usize, balance_gap: u32) -> bool {
    balance_gap > 0 && their_air >= my_air + balance_gap as usize
}

/// Does a side scramble against `threat` enemy players (detected, or known
/// airborne), with `my_air` of its own players up against `their_air`?
/// `balance_gap == 0` turns the air-balance scramble off.
pub(crate) fn scramble_reason(
    threat: usize,
    min_threat: usize,
    my_air: usize,
    their_air: usize,
    balance_gap: u32,
) -> Scramble {
    if threat >= min_threat {
        Scramble::Threat
    } else if outnumbered(my_air, their_air, balance_gap) {
        Scramble::Balance
    } else {
        Scramble::No
    }
}

/// The mean of some points, or the origin for none.
pub(crate) fn centroid(points: impl IntoIterator<Item = Vector2>) -> Vector2 {
    let (sum, n) = points
        .into_iter()
        .fold((Vector2::new(0., 0.), 0usize), |(acc, n), p| {
            (Vector2::new(acc.x + p.x, acc.y + p.y), n + 1)
        });
    if n == 0 {
        Vector2::new(0., 0.)
    } else {
        Vector2::new(sum.x / n as f64, sum.y / n as f64)
    }
}

/// The wall clock running this much further than DCS model time between two
/// ticks is worth a warning.
pub(crate) const CLOCK_GAP_WARN_SECS: i64 = 30;

/// How many seconds the wall clock ran ahead of model time between two
/// ticks, if it is worth a warning (see `CLOCK_GAP_WARN_SECS`).
pub(crate) fn clock_gap_secs(
    model_then: f64,
    model_now: f64,
    wall_then: DateTime<Utc>,
    wall_now: DateTime<Utc>,
) -> Option<i64> {
    let model = (model_now - model_then).max(0.);
    let wall = (wall_now - wall_then).num_milliseconds() as f64 / 1000.;
    let gap = (wall - model).round() as i64;
    (gap >= CLOCK_GAP_WARN_SECS).then_some(gap)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::FRAC_PI_2;

    #[test]
    fn takeoff_needs_height_or_time() {
        assert!(!takeoff_confirmed(5., Duration::seconds(3)));
        assert!(takeoff_confirmed(TAKEOFF_CONFIRM_AGL_M, Duration::zero()));
        // a helicopter departing low still counts after a while
        assert!(takeoff_confirmed(2., Duration::seconds(TAKEOFF_CONFIRM_SECS)));
        assert!(!takeoff_confirmed(29.9, Duration::seconds(TAKEOFF_CONFIRM_SECS - 1)));
    }

    #[test]
    fn runway_fit_accepts_the_runway_and_rejects_a_parallel_taxiway() {
        // a 3000 x 45 m runway pointing north (course 0)
        assert_eq!(runway_fit(0., 0., 0., 3000., 45.), RunwayFit::On);
        // floated well past the far threshold
        assert_eq!(runway_fit(2800., 0., 0., 3000., 45.), RunwayFit::On);
        // a parallel taxiway 180 m east, mid-field
        match runway_fit(0., 180., 0., 3000., 45.) {
            RunwayFit::Off { across, .. } => assert!((across.abs() - 180.).abs() < 1e-6),
            RunwayFit::On => panic!("taxiway accepted"),
        }
    }

    #[test]
    fn runway_fit_tries_both_course_signs() {
        // a diagonal runway (course 45 degrees); DCS sometimes reports -45
        let c = std::f64::consts::FRAC_PI_4;
        let (dn, de) = (1000. * c.cos(), 1000. * c.sin());
        assert_eq!(runway_fit(dn, de, c, 3000., 45.), RunwayFit::On);
        assert_eq!(runway_fit(dn, de, -c, 3000., 45.), RunwayFit::On);
    }

    #[test]
    fn runway_fit_lateral_margin_grows_along_the_runway() {
        // 120 m off the centreline is too far at the centre...
        assert!(matches!(runway_fit(0., 120., 0., 3000., 45.), RunwayFit::Off { .. }));
        // ...but within the course-error slop 1500 m down the runway
        assert_eq!(runway_fit(1500., 120., 0., 3000., 45.), RunwayFit::On);
    }

    #[test]
    fn runway_fit_carrier_deck_is_always_on() {
        assert_eq!(runway_fit(5000., 5000., 1.3, 0., 0.), RunwayFit::On);
    }

    #[test]
    fn runway_fit_reports_the_closest_projection() {
        // east-west runway, a point 5 km north of it
        match runway_fit(5000., 0., FRAC_PI_2, 3000., 45.) {
            RunwayFit::Off { along, across } => {
                assert!(along.abs() < 1e-6);
                assert!((across.abs() - 5000.).abs() < 1e-6);
            }
            RunwayFit::On => panic!("accepted"),
        }
    }

    #[test]
    fn banned_labels_use_display_names_once() {
        let ammo = vec![
            ("AIM_120C".to_string(), "AIM-120C".to_string()),
            ("AIM_120C".to_string(), "AIM-120C".to_string()),
            ("AIM_9X".to_string(), String::new()),
            ("MK_82".to_string(), "Mk-82".to_string()),
        ];
        let banned = |n: &str| n == "AIM-120C" || n == "AIM_9X";
        assert_eq!(banned_weapon_labels(ammo, banned), vec!["AIM-120C", "AIM_9X"]);
        assert!(banned_weapon_labels(vec![], |_| true).is_empty());
    }

    #[test]
    fn banned_land_time_has_a_floor() {
        assert_eq!(banned_land_secs(0), 30);
        assert_eq!(banned_land_secs(600), 600);
    }

    #[test]
    fn banned_verdicts() {
        let t = Utc.with_ymd_and_hms(2026, 10, 25, 12, 0, 0).unwrap();
        let deadline = t + Duration::seconds(60);
        assert_eq!(banned_loadout_verdict(None, t, deadline), BannedVerdict::Forgive);
        assert_eq!(banned_loadout_verdict(Some(false), deadline, deadline), BannedVerdict::Forgive);
        assert_eq!(banned_loadout_verdict(Some(true), t, deadline), BannedVerdict::Wait);
        assert_eq!(banned_loadout_verdict(Some(true), deadline, deadline), BannedVerdict::Remove);
    }

    #[test]
    fn wave_room_respects_both_caps() {
        assert!(wave_has_room(0, 0, 2, 3));
        assert!(!wave_has_room(2, 2, 2, 3));
        assert!(!wave_has_room(1, 3, 2, 3));
        assert!(wave_has_room(1, 2, 2, 3));
    }

    #[test]
    fn scramble_on_threat_or_imbalance() {
        assert_eq!(scramble_reason(2, 2, 0, 0, 0), Scramble::Threat);
        assert_eq!(scramble_reason(1, 2, 0, 0, 0), Scramble::No);
        // outnumbered 4 to 1 with a gap of 3
        assert_eq!(scramble_reason(0, 2, 1, 4, 3), Scramble::Balance);
        assert_eq!(scramble_reason(0, 2, 1, 3, 3), Scramble::No);
        // a gap of 0 turns balancing off
        assert_eq!(scramble_reason(0, 2, 0, 10, 0), Scramble::No);
        // a real threat wins over balance
        assert_eq!(scramble_reason(3, 2, 0, 10, 1), Scramble::Threat);
    }

    #[test]
    fn clock_gap_only_for_a_real_stall() {
        let t = Utc.with_ymd_and_hms(2026, 10, 25, 12, 0, 0).unwrap();
        // a normal tick
        assert_eq!(clock_gap_secs(100., 101., t, t + Duration::seconds(1)), None);
        // a slow frame or two
        assert_eq!(clock_gap_secs(100., 101., t, t + Duration::seconds(10)), None);
        // paused for five minutes
        assert_eq!(clock_gap_secs(100., 101., t, t + Duration::seconds(301)), Some(300));
    }

    #[test]
    fn centroid_of_points() {
        assert_eq!(centroid(std::iter::empty()), Vector2::new(0., 0.));
        let c = centroid([Vector2::new(0., 0.), Vector2::new(2., 4.)]);
        assert_eq!(c, Vector2::new(1., 2.));
    }
}
