// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Deterministic coaching notes from a pilot's recent range records.
//!
//! Every rule looks at the newest [`PER_KIND`] records of one kind, needs a
//! minimum sample before it says anything, and cites the records it is based
//! on (`evidence`), so the site can link each claim to the passes behind it.
//! No statistics beyond means and medians: a pilot should be able to check
//! the claim against their own list of results.

use super::boards::median;
use bfprotocols::range::{
    lso, MissileOutcome, PassOutcome, RangeRecord, RangeResult, StrafeQuality,
    WeaponClass,
};
use serde::Serialize;
use std::collections::HashMap;

/// Records per kind the rules look at.
pub(crate) const PER_KIND: usize = 30;

#[derive(Debug, Clone, Serialize)]
pub(crate) struct Insight {
    pub(crate) id: String,
    pub(crate) kind: String,
    /// "info" | "warn" | "good"
    pub(crate) severity: &'static str,
    pub(crate) title: String,
    pub(crate) detail: String,
    pub(crate) evidence: Vec<String>,
}

fn ins(id: &str, kind: &str, sev: &'static str, title: String, detail: String, ev: Vec<String>) -> Insight {
    Insight { id: id.into(), kind: kind.into(), severity: sev, title, detail, evidence: ev }
}

fn mean(v: impl Iterator<Item = f64>) -> Option<(f64, usize)> {
    let (s, n) = v.fold((0., 0usize), |(s, n), x| (s + x, n + 1));
    (n > 0).then(|| (s / n as f64, n))
}

/// Derive insights from `recs`, newest first.
pub(crate) fn derive(recs: &[&RangeRecord]) -> Vec<Insight> {
    let mut by_kind: HashMap<&str, Vec<&RangeRecord>> = HashMap::new();
    for r in recs {
        let v = by_kind.entry(r.kind()).or_default();
        if v.len() < PER_KIND {
            v.push(r);
        }
    }
    let mut out = vec![];
    let get = |k: &str| by_kind.get(k).cloned().unwrap_or_default();
    bombs(&get("bomb"), &mut out);
    strafe(&get("strafe"), &mut out);
    traps(&get("trap"), &mut out);
    aar(&get("aar"), &mut out);
    missiles(&get("missile"), &mut out);
    out
}

// ── bombing ────────────────────────────────────────────────────────────

fn bombs(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let bombs: Vec<(&RangeRecord, &bfprotocols::range::BombResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::Bomb(b) => Some((*r, b)),
            _ => None,
        })
        .collect();
    if bombs.len() < 5 {
        return;
    }
    let good = median(
        &bombs.iter().map(|(_, b)| b.good_radius_m).filter(|g| *g > 0.).collect::<Vec<_>>(),
    )
    .unwrap_or(25.);
    let thr = 0.5 * good;
    // long/short, per dive band
    for (lo, hi, code, band) in [
        (f64::MIN, 15., "0_15", "0-15°"),
        (15., 35., "15_35", "15-35°"),
        (35., f64::MAX, "35_up", "35°+"),
    ] {
        let set: Vec<_> = bombs
            .iter()
            .filter(|(_, b)| b.release.dive_deg >= lo && b.release.dive_deg < hi)
            .collect();
        if set.len() < 5 {
            continue;
        }
        let (m, n) = mean(set.iter().map(|(_, b)| b.long_m)).unwrap();
        if m.abs() > thr {
            let (dir, fix) = if m > 0. {
                ("long", "release a touch earlier or lower, and check you are not pickling while still accelerating or with the pipper above the target")
            } else {
                ("short", "release a touch later or higher, and check you are not shallow on your planned dive angle or slow at the pickle")
            };
            out.push(ins(
                &format!("bomb_{dir}_{code}"),
                "bomb",
                "warn",
                format!("Consistently {dir} in {band} dives"),
                format!(
                    "Your last {n} bombs released in a {band} dive landed {:.0} m {dir} on average, \
                     against a GOOD radius of {good:.0} m. To correct: {fix}.",
                    m.abs()
                ),
                set.iter().map(|(r, _)| r.id.clone()).collect(),
            ));
        }
    }
    // left/right
    let (m, n) = mean(bombs.iter().map(|(_, b)| b.cross_m)).unwrap();
    if m.abs() > thr {
        let side = if m > 0. { "right" } else { "left" };
        out.push(ins(
            &format!("bomb_{side}"),
            "bomb",
            "warn",
            format!("Bombs drift {side} of the run-in line"),
            format!(
                "Over your last {n} bombs the mean cross-track error is {:.0} m {side}. Usual \
                 causes: not wings-level at release, a slip in the dive, or no crosswind \
                 correction in the aim point.",
                m.abs()
            ),
            bombs.iter().map(|(r, _)| r.id.clone()).collect(),
        ));
    }
    // CEP trend: newest half vs older half
    if bombs.len() >= 10 {
        let half = bombs.len() / 2;
        let newer: Vec<f64> = bombs[..half].iter().map(|(_, b)| b.miss_m).collect();
        let older: Vec<f64> = bombs[half..].iter().map(|(_, b)| b.miss_m).collect();
        if let (Some(cn), Some(co)) = (median(&newer), median(&older)) {
            if co > 0. && cn < 0.8 * co {
                out.push(ins(
                    "bomb_cep_improving",
                    "bomb",
                    "good",
                    "Your bombing is tightening up".into(),
                    format!("CEP of your latest {half} bombs is {cn:.0} m, down from {co:.0} m before that."),
                    bombs[..half].iter().map(|(r, _)| r.id.clone()).collect(),
                ));
            } else if cn > 1.25 * co && cn > 0.5 * good {
                out.push(ins(
                    "bomb_cep_worsening",
                    "bomb",
                    "warn",
                    "Your bombing is spreading out".into(),
                    format!("CEP of your latest {half} bombs is {cn:.0} m, up from {co:.0} m before that."),
                    bombs[..half].iter().map(|(r, _)| r.id.clone()).collect(),
                ));
            }
        }
    }
    // unguided CEP inside the GOOD radius
    let ung: Vec<f64> = bombs
        .iter()
        .filter(|(_, b)| b.weapon_class == WeaponClass::Unguided)
        .map(|(_, b)| b.miss_m)
        .collect();
    if ung.len() >= 5 {
        if let Some(c) = median(&ung) {
            if c <= good * 0.5 {
                out.push(ins(
                    "bomb_unguided_tight",
                    "bomb",
                    "good",
                    format!("Unguided CEP {c:.0} m"),
                    format!("Half of your last {} unguided bombs landed within {c:.0} m -- EXCELLENT territory.", ung.len()),
                    vec![],
                ));
            }
        }
    }
}

// ── strafing ───────────────────────────────────────────────────────────

fn strafe(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let passes: Vec<(&RangeRecord, &bfprotocols::range::StrafeResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::Strafe(s) => Some((*r, s)),
            _ => None,
        })
        .collect();
    let recent: Vec<_> = passes.iter().take(10).collect();
    let fouls: Vec<_> = recent.iter().filter(|(_, s)| s.foul_line_crossed).collect();
    if !fouls.is_empty() {
        out.push(ins(
            "strafe_foul_line",
            "strafe",
            "warn",
            format!("Foul line crossed on {} of your last {} passes", fouls.len(), recent.len()),
            "A pass that crosses the foul line is scored INVALID. Plan the break-off: stop firing \
             and pull off before the foul line, even when the pass feels good."
                .into(),
            fouls.iter().map(|(r, _)| r.id.clone()).collect(),
        ));
    }
    let valid: Vec<_> = passes.iter().filter(|(_, s)| s.quality != StrafeQuality::Invalid).collect();
    if valid.len() >= 3 {
        let (m, n) = mean(valid.iter().map(|(_, s)| s.accuracy_pct)).unwrap();
        if m < 25. {
            let far = mean(valid.iter().map(|(_, s)| s.min_range_m)).map(|x| x.0).unwrap_or(0.);
            out.push(ins(
                "strafe_low_accuracy",
                "strafe",
                "warn",
                format!("Low strafe accuracy ({m:.0}%)"),
                format!(
                    "Average accuracy over your last {n} valid passes is {m:.0}%, with a mean \
                     closest firing range of {far:.0} m. Open fire later, track the pipper \
                     steady on the target before squeezing, and keep bursts short."
                ),
                valid.iter().map(|(r, _)| r.id.clone()).collect(),
            ));
        } else if m >= 75. {
            out.push(ins(
                "strafe_high_accuracy",
                "strafe",
                "good",
                format!("Deadly on the gun ({m:.0}%)"),
                format!("Average accuracy over your last {n} valid passes is {m:.0}%."),
                vec![],
            ));
        }
    }
}

// ── carrier ────────────────────────────────────────────────────────────

fn traps(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let passes: Vec<(&RangeRecord, &bfprotocols::range::TrapResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::Trap(t) => Some((*r, t)),
            _ => None,
        })
        .collect();
    if passes.len() >= 5 {
        // most frequent calls, counted once per pass, magnitude ignored
        let mut calls: HashMap<(String, Option<String>), Vec<String>> = HashMap::new();
        for (r, t) in &passes {
            let mut seen = std::collections::HashSet::new();
            for tok in t.lso_comment.split_whitespace() {
                let c = lso::parse_call(tok);
                let key = (c.error.clone(), c.position.clone());
                if seen.insert(key.clone()) {
                    calls.entry(key).or_default().push(r.id.clone());
                }
            }
        }
        let n = passes.len();
        let mut top: Vec<_> = calls
            .into_iter()
            .filter(|(_, ids)| ids.len() >= 3 && ids.len() * 10 >= n * 4)
            .collect();
        top.sort_by(|a, b| b.1.len().cmp(&a.1.len()).then(a.0.cmp(&b.0)));
        for ((err, pos), ids) in top.into_iter().take(3) {
            let code = format!("{err}{}", pos.clone().unwrap_or_default());
            let text = lso::parse_call(&code).text;
            let mut title = text.clone();
            if let Some(c) = title.get_mut(0..1) {
                c.make_ascii_uppercase();
            }
            out.push(ins(
                &format!("trap_call_{code}"),
                "trap",
                "warn",
                format!("{title} in {} of {n} passes", ids.len()),
                format!(
                    "The LSO called {code} ({text}) on {} of your last {n} passes -- your most \
                     repeated deviation. Work on it deliberately on the next few passes.",
                    ids.len()
                ),
                ids,
            ));
        }
    }
    let groove: Vec<(f64, String)> =
        passes.iter().filter_map(|(r, t)| t.groove_time_s.map(|g| (g, r.id.clone()))).collect();
    if groove.len() >= 5 {
        let (m, n) = mean(groove.iter().map(|(g, _)| *g)).unwrap();
        if m < 15. {
            out.push(ins(
                "trap_groove_short",
                "trap",
                "warn",
                format!("Short groove ({m:.0} s average)"),
                format!(
                    "Your last {n} grooves averaged {m:.1} s; 15-19 s is the target. A short \
                     groove usually means a tight 90 or overshooting the start: start the \
                     approach turn a little later and roll out with more straightaway."
                ),
                groove.iter().map(|(_, id)| id.clone()).collect(),
            ));
        } else if m > 19. {
            out.push(ins(
                "trap_groove_long",
                "trap",
                "warn",
                format!("Long groove ({m:.0} s average)"),
                format!(
                    "Your last {n} grooves averaged {m:.1} s; 15-19 s is the target. A long \
                     groove means a wide abeam or an early turn in: tighten the downwind \
                     spacing."
                ),
                groove.iter().map(|(_, id)| id.clone()).collect(),
            ));
        }
    }
    let landed: Vec<_> = passes
        .iter()
        .filter(|(_, t)| matches!(t.outcome, PassOutcome::Trap | PassOutcome::Bolter))
        .collect();
    if landed.len() >= 5 {
        let b: Vec<_> = landed.iter().filter(|(_, t)| t.outcome == PassOutcome::Bolter).collect();
        let rate = b.len() as f64 / landed.len() as f64;
        if rate >= 0.3 {
            out.push(ins(
                "trap_bolter_rate",
                "trap",
                "warn",
                format!("Bolter rate {:.0}%", rate * 100.),
                format!(
                    "{} of your last {} touchdowns were bolters. Most bolters come from a high \
                     ball or easing the power in close: fly the ball to touchdown and do not \
                     reduce power at the ramp.",
                    b.len(),
                    landed.len()
                ),
                b.iter().map(|(r, _)| r.id.clone()).collect(),
            ));
        }
    }
    let hook_up: Vec<_> = passes
        .iter()
        .take(10)
        .filter(|(_, t)| t.hook_down == Some(false) && t.outcome != PassOutcome::Trap)
        .collect();
    if !hook_up.is_empty() {
        out.push(ins(
            "trap_hook_up",
            "trap",
            "warn",
            format!("Hook up on {} pass(es)", hook_up.len()),
            "The hook was not down in the groove. Make hook-down part of the landing checklist \
             at the break."
                .into(),
            hook_up.iter().map(|(r, _)| r.id.clone()).collect(),
        ));
    }
    let pts: Vec<f64> = passes.iter().take(10).filter_map(|(_, t)| t.points).collect();
    if pts.len() >= 5 {
        let (m, n) = mean(pts.iter().cloned()).unwrap();
        if m >= 4. {
            out.push(ins(
                "trap_greenie",
                "trap",
                "good",
                format!("Greenie average {m:.1}"),
                format!("Your last {n} graded passes averaged {m:.2} points."),
                vec![],
            ));
        }
    }
}

// ── AAR ────────────────────────────────────────────────────────────────

fn aar(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let sessions: Vec<(&RangeRecord, &bfprotocols::range::AarResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::Aar(a) if a.contacts > 0 => Some((*r, a)),
            _ => None,
        })
        .collect();
    if sessions.len() < 2 {
        return;
    }
    let n = sessions.len() as f64;
    let fa = sessions.iter().map(|(_, a)| a.stability.fore_aft_sd_m / 2.0).sum::<f64>() / n;
    let lat = sessions.iter().map(|(_, a)| a.stability.lateral_sd_m / 1.5).sum::<f64>() / n;
    let vert = sessions.iter().map(|(_, a)| a.stability.vertical_sd_m / 1.5).sum::<f64>() / n;
    let mut axes = [
        (fa, "fore-aft", "Chase the tanker with small, early throttle inputs and use a fixed reference on the tanker for closure."),
        (lat, "lateral", "Use rudder-free, small bank corrections and pick a lateral reference (the tanker's centreline or engine) and hold it."),
        (vert, "vertical", "Trim for the refuelling speed and make tiny, anticipatory pitch corrections; vertical wander is usually over-control."),
    ];
    axes.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
    let (score, axis, tip) = axes[0];
    if score > 1.0 {
        out.push(ins(
            &format!("aar_unstable_{axis}"),
            "aar",
            "warn",
            format!("Mostly unstable {axis} in contact"),
            format!(
                "Across your last {} sessions your {axis} spread while connected is {:.1}x the \
                 target. {tip}",
                sessions.len(),
                score
            ),
            sessions.iter().map(|(r, _)| r.id.clone()).collect(),
        ));
    }
    let (d, _) = mean(sessions.iter().map(|(_, a)| a.disconnects as f64)).unwrap();
    if d > 1.5 {
        out.push(ins(
            "aar_disconnects",
            "aar",
            "warn",
            format!("{d:.1} disconnects per session"),
            "Frequent disconnects: stabilise in pre-contact first, then move in slowly; most \
             breakaways come from closing too fast on the last few feet."
                .into(),
            sessions.iter().filter(|(_, a)| a.disconnects > 1).map(|(r, _)| r.id.clone()).collect(),
        ));
    }
}

// ── A/A missiles ───────────────────────────────────────────────────────

fn missiles(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let all: Vec<(&RangeRecord, &bfprotocols::range::MissileResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::Missile(m) => Some((*r, m)),
            _ => None,
        })
        .collect();
    let def: Vec<_> = all.iter().filter(|(_, m)| m.perspective == "target").collect();
    if def.len() >= 3 {
        let reacted: Vec<f64> = def.iter().filter_map(|(_, m)| m.defense.reaction_s).collect();
        let never = def.iter().filter(|(_, m)| m.defense.reaction_s.is_none()).count();
        let slow = mean(reacted.iter().cloned()).map(|x| x.0).unwrap_or(0.);
        if slow > 3. || never * 3 >= def.len() {
            out.push(ins(
                "md_late_reaction",
                "missile",
                "warn",
                if never * 3 >= def.len() {
                    format!("No defensive reaction to {never} of {} missiles", def.len())
                } else {
                    format!("Late reaction to missiles ({slow:.1} s)")
                },
                "Start the defence the moment the launch is called or the RWR shows it: a hard turn \
                 to beam or drag within 3 seconds is what defeats a shot."
                    .into(),
                def.iter().map(|(r, _)| r.id.clone()).collect(),
            ));
        }
        let hot: Vec<_> = def
            .iter()
            .filter(|(_, m)| m.time_of_flight_s > 0. && m.defense.hot_s / m.time_of_flight_s > 0.4)
            .collect();
        if hot.len() * 3 >= def.len() && !hot.is_empty() {
            out.push(ins(
                "md_staying_hot",
                "missile",
                "warn",
                format!("Staying hot on {} of {} missiles", hot.len(), def.len()),
                "You kept the missile on your nose for much of its flight. Unless you are \
                 cranking to support your own shot, turn to put it on the beam or behind you."
                    .into(),
                hot.iter().map(|(r, _)| r.id.clone()).collect(),
            ));
        }
        let sams: Vec<_> = def.iter().filter(|(_, m)| m.weapon_category == "sam").collect();
        if sams.len() >= 3 {
            let high: Vec<_> = sams.iter().filter(|(_, m)| !m.defense.went_low).collect();
            if high.len() * 10 >= sams.len() * 7 {
                out.push(ins(
                    "md_sam_not_low",
                    "missile",
                    "info",
                    format!("Not going low against SAMs ({} of {})", high.len(), sams.len()),
                    "Against a SAM, descending toward the terrain while beaming puts you in the \
                     radar's ground clutter and the missile's energy-poor regime."
                        .into(),
                    high.iter().map(|(r, _)| r.id.clone()).collect(),
                ));
            }
        }
        let beat = def
            .iter()
            .filter(|(_, m)| matches!(m.outcome, MissileOutcome::Defeated | MissileOutcome::Timeout))
            .count();
        if def.len() >= 5 && beat * 10 >= def.len() * 8 {
            out.push(ins(
                "md_solid",
                "missile",
                "good",
                format!("Defeated {beat} of {} missiles", def.len()),
                "Solid missile defence.".into(),
                vec![],
            ));
        }
    }
    // shooting: kill rate by launch-range band
    let shots: Vec<_> = all.iter().filter(|(_, m)| m.perspective != "target").collect();
    let nm = 1852.;
    let bands = [(0., 10. * nm, "inside 10 nm"), (10. * nm, 20. * nm, "10-20 nm"), (20. * nm, 35. * nm, "20-35 nm"), (35. * nm, f64::MAX, "beyond 35 nm")];
    let mut rates = vec![];
    for (lo, hi, label) in bands {
        let set: Vec<_> = shots.iter().filter(|(_, m)| m.launch.range_m >= lo && m.launch.range_m < hi).collect();
        if set.len() >= 3 {
            let k = set.iter().filter(|(_, m)| matches!(m.outcome, MissileOutcome::Kill | MissileOutcome::Hit)).count();
            rates.push((k as f64 / set.len() as f64, label, set.len(), set.iter().map(|(r, _)| r.id.clone()).collect::<Vec<_>>()));
        }
    }
    if rates.len() >= 2 {
        rates.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
        let best = &rates[0];
        let worst = &rates[rates.len() - 1];
        if best.0 - worst.0 >= 0.25 {
            out.push(ins(
                "aa_kill_rate_by_range",
                "missile",
                "info",
                format!("Kill rate {:.0}% {} vs {:.0}% {}", best.0 * 100., best.1, worst.0 * 100., worst.1),
                format!(
                    "Trainer kills by launch range: {:.0}% of {} shots {} against {:.0}% of {} \
                     {}. Shots from the weaker band are mostly giving the target time to \
                     defend.",
                    best.0 * 100., best.2, best.1, worst.0 * 100., worst.2, worst.1
                ),
                worst.3.clone(),
            ));
        }
    }
}
