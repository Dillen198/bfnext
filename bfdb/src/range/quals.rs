// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Range qualifications: fixed definitions evaluated against a pilot's
//! records, each with a progress fraction so the site can draw a bar.
//!
//! These are currency-style: every qual looks at a rolling window (30 days
//! for carrier quals, 90 for the rest), so a qual earned last season and not
//! flown since lapses, like the real thing.

use super::boards::median;
use bfprotocols::range::{
    MissileOutcome, PassOutcome, PrecisionQuality, RangeRecord, RangeResult, StrafeQuality,
    WeaponClass,
};
use chrono::{DateTime, Duration, Utc};
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub(crate) struct Qual {
    pub(crate) id: &'static str,
    pub(crate) name: &'static str,
    pub(crate) description: &'static str,
    pub(crate) earned: bool,
    /// 0..1
    pub(crate) progress: f64,
    pub(crate) detail: String,
}

fn frac(have: usize, need: usize) -> f64 {
    (have as f64 / need as f64).min(1.)
}

/// Progress for "n things, and a quality bar over them": half for the count,
/// half for the bar; never 1.0 unless earned.
fn blend(count: f64, quality: f64, earned: bool) -> f64 {
    let p = 0.5 * count + 0.5 * quality.clamp(0., 1.);
    if earned {
        1.
    } else {
        (p.min(0.99) * 100.).round() / 100.
    }
}

fn counted(have: usize, need: usize, earned: bool) -> f64 {
    if earned {
        1.
    } else {
        (frac(have, need).min(0.99) * 100.).round() / 100.
    }
}

/// Evaluate every qualification. `recs` newest first.
pub(crate) fn evaluate(recs: &[&RangeRecord], now: DateTime<Utc>) -> Vec<Qual> {
    let within = |days: i64| {
        let from = now - Duration::days(days);
        recs.iter().filter(move |r| r.ts >= from).copied()
    };
    let mut out = vec![];

    // CQ Day: 6 day traps, average >= 3.0 points, last 30 days
    {
        let traps: Vec<f64> = within(30)
            .filter_map(|r| match &r.result {
                RangeResult::Trap(t) if t.outcome == PassOutcome::Trap && !t.night => {
                    Some(t.points.unwrap_or(0.))
                }
                _ => None,
            })
            .collect();
        let n = traps.len();
        let avg = if n > 0 { traps.iter().sum::<f64>() / n as f64 } else { 0. };
        let earned = n >= 6 && avg >= 3.0;
        out.push(Qual {
            id: "cq_day",
            name: "CQ Day",
            description: "6 day traps averaging 3.0 LSO points or better in the last 30 days",
            earned,
            progress: blend(frac(n, 6), avg / 3.0, earned),
            detail: format!("{}/6 day traps, average {avg:.1} points (need 3.0)", n.min(6)),
        });
    }
    // CQ Night: 4 night traps, last 30 days
    {
        let n = within(30)
            .filter(|r| {
                matches!(&r.result, RangeResult::Trap(t) if t.outcome == PassOutcome::Trap && t.night)
            })
            .count();
        let earned = n >= 4;
        out.push(Qual {
            id: "cq_night",
            name: "CQ Night",
            description: "4 night traps in the last 30 days",
            earned,
            progress: counted(n, 4, earned),
            detail: format!("{}/4 night traps", n.min(4)),
        });
    }
    // Bombing: CEP of the most recent 6 bombs of a class, last 90 days
    for (id, name, desc, class, need_cep) in [
        (
            "bombing_unguided",
            "Bombing Unguided",
            "CEP of 15 m or better over your 6 most recent unguided bombs (90 days)",
            WeaponClass::Unguided,
            15.,
        ),
        (
            "bombing_guided",
            "Bombing Guided",
            "CEP of 5 m or better over your 6 most recent guided bombs (90 days)",
            WeaponClass::Guided,
            5.,
        ),
    ] {
        let misses: Vec<f64> = within(90)
            .filter_map(|r| match &r.result {
                RangeResult::Bomb(b) if b.weapon_class == class => Some(b.miss_m),
                _ => None,
            })
            .take(6)
            .collect();
        let n = misses.len();
        let cep = median(&misses);
        let earned = n >= 6 && cep.map_or(false, |c| c <= need_cep);
        let q = cep.map_or(0., |c| if c <= 0. { 1. } else { need_cep / c });
        out.push(Qual {
            id,
            name,
            description: desc,
            earned,
            progress: blend(frac(n, 6), q, earned),
            detail: match cep {
                Some(c) => format!("{n}/6 bombs, CEP {c:.1} m (need {need_cep:.0} m)"),
                None => "no bombs of this class yet".into(),
            },
        });
    }
    // Strafe: 3 valid passes at 50%+
    {
        let n = within(90)
            .filter(|r| {
                matches!(&r.result, RangeResult::Strafe(s)
                    if s.quality != StrafeQuality::Invalid && s.accuracy_pct >= 50.)
            })
            .count();
        let earned = n >= 3;
        out.push(Qual {
            id: "strafe",
            name: "Strafe",
            description: "3 valid strafe passes at 50% accuracy or better (90 days)",
            earned,
            progress: counted(n, 3, earned),
            detail: format!("{}/3 passes at 50%+", n.min(3)),
        });
    }
    // AAR: 3 sessions graded B or better
    {
        let n = within(90)
            .filter(|r| matches!(&r.result, RangeResult::Aar(a) if a.grade == "A" || a.grade == "B"))
            .count();
        let earned = n >= 3;
        out.push(Qual {
            id: "aar",
            name: "AAR",
            description: "3 refuelling sessions graded B or better (90 days)",
            earned,
            progress: counted(n, 3, earned),
            detail: format!("{}/3 sessions at B or better", n.min(3)),
        });
    }
    // Missile defense: defeat 5 trainer missiles
    {
        let n = within(90)
            .filter(|r| {
                matches!(&r.result, RangeResult::Missile(m)
                    if m.perspective == "target" && m.outcome == MissileOutcome::Defeated)
            })
            .count();
        let earned = n >= 5;
        out.push(Qual {
            id: "missile_defense",
            name: "Missile Defense",
            description: "Defeat 5 missile-trainer shots (90 days)",
            earned,
            progress: counted(n, 5, earned),
            detail: format!("{}/5 missiles defeated", n.min(5)),
        });
    }
    // Helo precision: 5 landings GOOD or better
    {
        let n = within(90)
            .filter(|r| {
                matches!(&r.result, RangeResult::Landing(l) if l.quality >= PrecisionQuality::Good)
            })
            .count();
        let earned = n >= 5;
        out.push(Qual {
            id: "helo_precision",
            name: "Helo Precision",
            description: "5 precision landings graded GOOD or better (90 days)",
            earned,
            progress: counted(n, 5, earned),
            detail: format!("{}/5 landings GOOD or better", n.min(5)),
        });
    }
    // Helicopter cargo: 3 deliveries GOOD or better, slung or carried inside
    // (DCS dynamic cargo)
    {
        let n = within(90)
            .filter(|r| {
                matches!(&r.result, RangeResult::Sling(s) if s.quality >= PrecisionQuality::Good)
            })
            .count();
        let earned = n >= 3;
        out.push(Qual {
            id: "sling",
            name: "Helo Cargo",
            description: "3 cargo deliveries, sling or internal, graded GOOD or better (90 days)",
            earned,
            progress: counted(n, 3, earned),
            detail: format!("{}/3 deliveries GOOD or better", n.min(3)),
        });
    }
    out
}
