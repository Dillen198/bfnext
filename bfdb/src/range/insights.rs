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
    lso, CsarOutcome, FieldLandingOutcome, HotZoneOutcome, MissileOutcome, PassOutcome,
    PrecisionQuality, RangeRecord, RangeResult, StrafeQuality, WeaponClass,
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
    sead(&get("sead"), &mut out);
    hot_zone(&get("hot_zone"), &mut out);
    low_level(&get("low_level"), &mut out);
    field_landing(&get("field_landing"), &mut out);
    landing(&get("landing"), &mut out);
    csar(&get("csar"), &mut out);
    out
}

fn ids<T>(v: &[(&RangeRecord, T)]) -> Vec<String> {
    v.iter().map(|(r, _)| r.id.clone()).collect()
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

// ── SEAD / DEAD ────────────────────────────────────────────────────────

fn sead(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let kills: Vec<(&RangeRecord, &bfprotocols::range::SeadResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::Sead(s) => Some((*r, s)),
            _ => None,
        })
        .collect();
    if kills.is_empty() {
        return;
    }
    // anti-radiation shots that were fired at a dark radar
    let arm: Vec<_> = kills.iter().filter(|(_, s)| s.guidance.eq_ignore_ascii_case("arm")).cloned().collect();
    let dark: Vec<_> = arm.iter().filter(|(_, s)| !s.site_was_emitting).cloned().collect();
    if arm.len() >= 3 && dark.len() * 10 >= arm.len() * 4 {
        out.push(ins(
            "sead_arm_dark",
            "sead",
            "warn",
            format!("{} of {} anti-radiation kills fired at a dark radar", dark.len(), arm.len()),
            "Those missiles flew to where the radar had been, not to an emitter. A real site that \
             shuts down in time defeats that shot. Fire when the site is emitting (a fresh RWR \
             spike on it), or use pre-briefed / position mode deliberately and expect a lower \
             hit rate."
                .into(),
            ids(&dark),
        ));
    }
    // how often the network would have killed the pilot: one count per
    // sortie (records of the same network within 30 minutes)
    let mut sorties: Vec<(&str, i64, u32, u32, Vec<String>)> = vec![];
    for (r, sd) in &kills {
        let t = r.ts.timestamp();
        match sorties.iter_mut().find(|x| x.0 == sd.network && (x.1 - t).abs() < 1800) {
            Some(x) => {
                x.1 = t;
                x.2 = x.2.max(sd.shots_at_you);
                x.3 = x.3.max(sd.trainer_deaths);
                x.4.push(r.id.clone());
            }
            None => sorties.push((sd.network.as_str(), t, sd.shots_at_you, sd.trainer_deaths, vec![r.id.clone()])),
        }
    }
    let deaths: u32 = sorties.iter().map(|x| x.3).sum();
    let shot_at: u32 = sorties.iter().map(|x| x.2).sum();
    if deaths >= 2 {
        out.push(ins(
            "sead_trainer_deaths",
            "sead",
            "warn",
            format!("SAMs would have killed you {deaths} times in {} SEAD sorties", sorties.len()),
            format!(
                "The networks fired {shot_at} missiles at you and the trainer removed {deaths} that \
                 would have hit. Launch anti-radiation missiles from nearer their maximum range, \
                 stay outside the site's engagement ring until it is suppressed, and use the \
                 terrain to mask your ingress."
            ),
            sorties.iter().filter(|x| x.3 > 0).flat_map(|x| x.4.clone()).collect(),
        ));
    }
    let ranges: Vec<f64> = arm.iter().filter_map(|(_, s)| s.launch_range_m).collect();
    if ranges.len() >= 3 {
        if let Some(m) = median(&ranges) {
            if m < 20_000. {
                out.push(ins(
                    "sead_arm_close",
                    "sead",
                    "info",
                    format!("Anti-radiation shots from {:.0} nm (median)", m / 1852.),
                    "You are getting close to the sites before firing. An anti-radiation missile's \
                     job is to kill or suppress the radar from outside its missiles' reach: \
                     try firing earlier and turning away."
                        .into(),
                    ids(&arm),
                ));
            }
        }
    }
    let destroyed: Vec<_> = kills.iter().filter(|(_, s)| s.site_destroyed).cloned().collect();
    if destroyed.len() >= 3 && deaths == 0 {
        out.push(ins(
            "sead_clean",
            "sead",
            "good",
            format!("{} sites rolled back", destroyed.len()),
            "Every radar at those sites is dead and no SAM got close to you doing it.".into(),
            vec![],
        ));
    }
}

// ── hot zone ───────────────────────────────────────────────────────────

fn hot_zone(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let sorties: Vec<(&RangeRecord, &bfprotocols::range::HotZoneResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::HotZone(h) => Some((*r, h)),
            _ => None,
        })
        .collect();
    if sorties.len() < 3 {
        return;
    }
    let n = sorties.len();
    let died: Vec<_> = sorties
        .iter()
        .filter(|(_, h)| h.trainer_deaths > 0 || h.outcome == HotZoneOutcome::ShotDown)
        .cloned()
        .collect();
    if died.len() * 3 >= n {
        let defeated: u32 = sorties.iter().map(|(_, h)| h.missiles_defeated).sum();
        let deaths: u32 = sorties.iter().map(|(_, h)| h.trainer_deaths).sum();
        out.push(ins(
            "hz_dying",
            "hot_zone",
            "warn",
            format!("Killed on {} of your last {n} hot-zone sorties", died.len()),
            format!(
                "You defeated {defeated} missiles but {deaths} more would have hit. Check six \
                 before committing to a ground target, ask AWACS for the picture before you \
                 push, and keep enough energy and altitude to defend when the CAP comes in."
            ),
            ids(&died),
        ));
    }
    let shots: u32 = sorties.iter().map(|(_, h)| h.shots_fired).sum();
    let kills: u32 = sorties.iter().map(|(_, h)| h.air_kills + h.ground_kills).sum();
    if shots >= 8 && (kills == 0 || shots as f64 / kills as f64 > 4.) {
        out.push(ins(
            "hz_shots_per_kill",
            "hot_zone",
            "info",
            if kills == 0 { format!("{shots} shots, no kills") } else { format!("{:.1} shots per kill", shots as f64 / kills as f64) },
            "A lot of weapons are going out for what they kill. Shoot air-to-air missiles \
             inside the no-escape zone rather than at max range, and check the ground target \
             is in range and locked before you release."
                .into(),
            ids(&sorties),
        ));
    }
    let landed: Vec<_> = sorties.iter().filter(|(_, h)| h.outcome == HotZoneOutcome::Landed).cloned().collect();
    if landed.len() >= 2 {
        out.push(ins(
            "hz_no_egress",
            "hot_zone",
            "info",
            format!("{} sorties ended without an egress", landed.len()),
            "Plan the way out as carefully as the way in: fly out of the zone before you \
             land, so the sortie counts as a clean egress."
                .into(),
            ids(&landed),
        ));
    }
    if died.is_empty() && kills as f64 / n as f64 >= 2. {
        out.push(ins(
            "hz_good",
            "hot_zone",
            "good",
            format!("{:.1} kills per sortie, no deaths", kills as f64 / n as f64),
            format!("{kills} kills over your last {n} hot-zone sorties without the trainer saving you once."),
            vec![],
        ));
    }
}

// ── low-level routes ───────────────────────────────────────────────────

fn low_level(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let runs: Vec<(&RangeRecord, &bfprotocols::range::LowLevelResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::LowLevel(l) => Some((*r, l)),
            _ => None,
        })
        .collect();
    if runs.len() < 3 {
        return;
    }
    let n = runs.len();
    let (below, _) = mean(runs.iter().map(|(_, l)| l.pct_below_ceiling)).unwrap();
    if below < 90. {
        let busts: Vec<_> = runs.iter().filter(|(_, l)| l.pct_below_ceiling < 95.).cloned().collect();
        let ceil = runs[0].1.max_allowed_agl_ft;
        out.push(ins(
            "ll_ceiling",
            "low_level",
            "warn",
            format!("Above the ceiling for {:.0}% of your routes", 100. - below),
            format!(
                "Over your last {n} runs you spent {:.0}% of the route above the {ceil:.0} ft \
                 ceiling. Most busts come climbing over ridgelines: cross them at the saddle, \
                 unload over the top and get back down on the far side instead of holding the \
                 climb.",
                100. - below
            ),
            ids(&busts),
        ));
    }
    let floor: Vec<_> = runs.iter().filter(|(_, l)| l.below_floor_s > 5.).cloned().collect();
    if floor.len() >= 2 {
        out.push(ins(
            "ll_floor",
            "low_level",
            "warn",
            format!("Below the safety floor on {} runs", floor.len()),
            "Low is good, too low is how people hit the ground: pick a height you can hold with \
             your eyes out of the cockpit and fly the floor as a hard limit."
                .into(),
            ids(&floor),
        ));
    }
    let (tot, _) = mean(runs.iter().map(|(_, l)| l.tot_error_s)).unwrap();
    if tot.abs() > 15. {
        let (dir, fix) = if tot > 0. {
            ("late", "Plan and hold the planned ground speed, and make up time on the straight legs rather than at the last gate")
        } else {
            ("early", "Throttle back to the planned ground speed; arriving early is as wrong as late when the package is timed")
        };
        out.push(ins(
            &format!("ll_tot_{dir}"),
            "low_level",
            "warn",
            format!("Consistently {dir} on target ({tot:+.0} s average)"),
            format!(
                "Your last {n} runs finished {:.0} s {dir} on average. {fix}. Check the gate \
                 times on each run to see where the error builds up.",
                tot.abs()
            ),
            ids(&runs),
        ));
    }
    // the gate that gets missed most
    let mut missed: HashMap<&str, Vec<String>> = HashMap::new();
    for (r, l) in &runs {
        for g in l.gates.iter().filter(|g| g.t.is_none()) {
            missed.entry(g.gate.as_str()).or_default().push(r.id.clone());
        }
    }
    if let Some((gate, ev)) = missed.into_iter().filter(|(_, v)| v.len() >= 2).max_by_key(|(_, v)| v.len()) {
        out.push(ins(
            "ll_missed_gate",
            "low_level",
            "warn",
            format!("Gate {gate} missed on {} runs", ev.len()),
            format!(
                "You miss {gate} more than any other gate. Mark it on the map before you go and \
                 pick a visual feature near it to steer for."
            ),
            ev,
        ));
    }
    let scores: Vec<f64> = runs.iter().filter_map(|(r, _)| r.score).collect();
    if scores.len() >= 3 {
        let (m, _) = mean(scores.iter().cloned()).unwrap();
        if m >= 4. {
            out.push(ins(
                "ll_good",
                "low_level",
                "good",
                format!("Low-level average {m:.1}"),
                "Low, on the route and on time.".into(),
                vec![],
            ));
        }
    }
}

// ── field landings ─────────────────────────────────────────────────────

fn field_landing(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let all: Vec<(&RangeRecord, &bfprotocols::range::FieldLandingResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::FieldLanding(f) => Some((*r, f)),
            _ => None,
        })
        .collect();
    if all.len() < 4 {
        return;
    }
    let n = all.len();
    let under: Vec<_> = all.iter().filter(|(_, f)| f.outcome == FieldLandingOutcome::Undershoot).cloned().collect();
    if !under.is_empty() {
        out.push(ins(
            "fl_undershoot",
            "field_landing",
            "warn",
            format!("{} landing(s) short of the runway", under.len()),
            "Touching down before the threshold is a crash in real life. Keep the aim point \
             fixed in the windscreen: if it moves up you are going low, add power early."
                .into(),
            ids(&under),
        ));
    }
    let on: Vec<_> = all.iter().filter(|(_, f)| f.outcome != FieldLandingOutcome::Undershoot).cloned().collect();
    if on.len() >= 4 {
        let (aim, _) = mean(on.iter().map(|(_, f)| f.aim_error_m)).unwrap();
        if aim.abs() > 100. {
            let (dir, fix) = if aim > 0. {
                ("long", "you are floating: cross the threshold at the right speed and height, then close the throttle and let it land rather than holding it off")
            } else {
                ("short", "you are low on the approach: fly the glideslope down to the aim point and don't duck under it in the last half mile")
            };
            out.push(ins(
                &format!("fl_{dir}"),
                "field_landing",
                "warn",
                format!("Landing {dir} of the aim point ({:.0} m average)", aim.abs()),
                format!("Your last {} touchdowns landed {:.0} m {dir} of the aim point on average: {fix}.", on.len(), aim.abs()),
                ids(&on),
            ));
        }
        let (cl, _) = mean(on.iter().map(|(_, f)| f.centreline_m)).unwrap();
        if cl.abs() > 3. {
            let side = if cl > 0. { "right" } else { "left" };
            out.push(ins(
                &format!("fl_centreline_{side}"),
                "field_landing",
                "warn",
                format!("Touching down {side} of the centreline ({:.1} m average)", cl.abs()),
                format!(
                    "Consistently {side} usually means an uncorrected crosswind drift or lining \
                     up on the runway edge in the flare. Hold the centreline with rudder and \
                     into-wind aileron all the way to touchdown."
                ),
                ids(&on),
            ));
        }
    }
    let (fpm, _) = mean(all.iter().map(|(_, f)| f.touchdown_fpm)).unwrap();
    let hard: Vec<_> = all.iter().filter(|(_, f)| f.touchdown_fpm > 600.).cloned().collect();
    if fpm > 600. || hard.len() * 3 >= n {
        out.push(ins(
            "fl_firm",
            "field_landing",
            "warn",
            format!("Firm touchdowns ({fpm:.0} fpm average)"),
            "Start the flare a little higher and smoother; a stable approach at the right speed \
             is what makes a soft landing possible."
                .into(),
            ids(&hard),
        ));
    }
    let unstable: Vec<_> = all.iter().filter(|(_, f)| !f.stable).cloned().collect();
    if unstable.len() * 10 >= n * 4 {
        // what makes them unstable: glideslope or lineup at half a mile
        let gs: Vec<f64> = all.iter().filter_map(|(_, f)| f.gs_error_half_nm_deg).collect();
        let lu: Vec<f64> = all.iter().filter_map(|(_, f)| f.lineup_half_nm_deg).collect();
        let gsm = mean(gs.iter().cloned()).map(|x| x.0).unwrap_or(0.);
        let lum = mean(lu.iter().cloned()).map(|x| x.0).unwrap_or(0.);
        let why = if gsm.abs() >= 0.5 && gsm.abs() >= lum.abs() {
            format!("You are {:.1}° {} the glideslope at half a mile on average.", gsm.abs(), if gsm > 0. { "above" } else { "below" })
        } else if lum.abs() >= 0.5 {
            format!("You are {:.1}° {} of the centreline at half a mile on average.", lum.abs(), if lum > 0. { "right" } else { "left" })
        } else {
            "Your glideslope and lineup wander more than they are off on average: make smaller, earlier corrections.".into()
        };
        out.push(ins(
            "fl_unstable",
            "field_landing",
            "warn",
            format!("Unstable approach on {} of your last {n} landings", unstable.len()),
            format!(
                "{why} Be on speed, on glideslope and lined up by 1 nm; if you are not, go \
                 around rather than fixing it in the flare."
            ),
            ids(&unstable),
        ));
    }
    let scores: Vec<f64> = all.iter().filter_map(|(r, _)| r.score).collect();
    if scores.len() >= 5 {
        let (m, _) = mean(scores.iter().cloned()).unwrap();
        if m >= 4. {
            out.push(ins(
                "fl_good",
                "field_landing",
                "good",
                format!("Landing average {m:.1}"),
                "On the numbers, on the centreline and soft.".into(),
                vec![],
            ));
        }
    }
}

// ── helicopter landings ────────────────────────────────────────────────

fn landing(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let all: Vec<(&RangeRecord, &bfprotocols::range::LandingResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::Landing(l) => Some((*r, l)),
            _ => None,
        })
        .collect();
    if all.len() >= 5 {
        let (fpm, n) = mean(all.iter().map(|(_, l)| l.touchdown_fpm)).unwrap();
        if fpm > 300. {
            out.push(ins(
                "landing_firm",
                "landing",
                "warn",
                format!("Firm helicopter touchdowns ({fpm:.0} fpm average)"),
                format!(
                    "Your last {n} landings touched down at {fpm:.0} fpm on average. Arrive in a \
                     stable hover over the spot first, then lower the collective slowly until \
                     the gear settles."
                ),
                ids(&all),
            ));
        }
    }
    let deck: Vec<_> = all.iter().filter(|(_, l)| l.drill == "ship").cloned().collect();
    if deck.len() >= 3 {
        let (d, n) = mean(deck.iter().map(|(_, l)| l.distance_m)).unwrap();
        if deck.iter().filter(|(_, l)| l.quality <= PrecisionQuality::Fair).count() * 2 >= deck.len() {
            out.push(ins(
                "deck_landing_off",
                "landing",
                "warn",
                format!("Deck landings {d:.1} m off the spot"),
                format!(
                    "Your last {n} deck landings averaged {d:.1} m from the spot. Match the \
                     ship's speed and heading alongside first, then move across and down \
                     together with the deck rather than chasing it."
                ),
                ids(&deck),
            ));
        }
    }
}

// ── CSAR ───────────────────────────────────────────────────────────────

fn csar(recs: &[&RangeRecord], out: &mut Vec<Insight>) {
    let all: Vec<(&RangeRecord, &bfprotocols::range::CsarResult)> = recs
        .iter()
        .filter_map(|r| match &r.result {
            RangeResult::Csar(c) => Some((*r, c)),
            _ => None,
        })
        .collect();
    if all.len() < 2 {
        return;
    }
    let dropped: Vec<_> = all.iter().filter(|(_, c)| c.outcome == CsarOutcome::PickedUp).cloned().collect();
    if dropped.len() >= 2 {
        out.push(ins(
            "csar_not_delivered",
            "csar",
            "warn",
            format!("{} survivors picked up but never brought home", dropped.len()),
            "The rescue only counts once he is at a friendly base or FARP. Plan the fuel for the \
             trip home before you go looking."
                .into(),
            ids(&dropped),
        ));
    }
    let failed: Vec<_> = all.iter().filter(|(_, c)| c.outcome == CsarOutcome::Failed).cloned().collect();
    if failed.len() >= 2 {
        out.push(ins(
            "csar_failed",
            "csar",
            "warn",
            format!("{} survivors not reached", failed.len()),
            "Tune the survivor's beacon on the ADF as soon as the MAYDAY comes in and fly the \
             needle; once close, ask for smoke and approach from downwind."
                .into(),
            ids(&failed),
        ));
    }
    let pick: Vec<(f64, String)> = all.iter().filter_map(|(r, c)| c.time_to_pickup_s.map(|t| (t, r.id.clone()))).collect();
    if pick.len() >= 3 {
        if let Some(m) = median(&pick.iter().map(|x| x.0).collect::<Vec<_>>()) {
            if m > 20. * 60. {
                out.push(ins(
                    "csar_slow_find",
                    "csar",
                    "info",
                    format!("{:.0} minutes to reach the survivor (median)", m / 60.),
                    "Most of the time goes into the search. Take the bearing from the beacon \
                     straight away, fly it at a speed you can see from, and cross-check with a \
                     second bearing rather than searching the whole area."
                        .into(),
                    pick.iter().map(|x| x.1.clone()).collect(),
                ));
            }
        }
    }
    let hostile_hover: Vec<_> = all
        .iter()
        .filter(|(_, c)| c.hostile && c.pickup_method == "hover")
        .cloned()
        .collect();
    if hostile_hover.len() >= 2 && hostile_hover.len() * 2 >= all.iter().filter(|(_, c)| c.hostile).count() {
        out.push(ins(
            "csar_hostile_hover",
            "csar",
            "info",
            format!("Hover pickups in hostile areas ({})", hostile_hover.len()),
            "A long hover with troops hunting the survivor is when helicopters get shot. If there \
             is a clearing, land, load and go."
                .into(),
            ids(&hostile_hover),
        ));
    }
    let rescued: Vec<_> = all.iter().filter(|(_, c)| c.outcome == CsarOutcome::Rescued).collect();
    if rescued.len() >= 3 && rescued.len() == all.len() {
        out.push(ins(
            "csar_good",
            "csar",
            "good",
            format!("{} of {} survivors home", rescued.len(), all.len()),
            "Every survivor you went for came home.".into(),
            vec![],
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_kinds_produce_insights() {
        let mut recs = vec![];
        for _ in 0..5 {
            recs.extend(super::super::cards::tests::new_kinds());
        }
        // the CSAR / field landings need a sample, the SEAD a death per sortie
        for (i, r) in recs.iter_mut().enumerate() {
            r.ts = r.ts - chrono::Duration::hours(i as i64);
        }
        let refs: Vec<&RangeRecord> = recs.iter().collect();
        let out = derive(&refs);
        let has = |id: &str| out.iter().any(|i| i.id == id);
        assert!(has("sead_trainer_deaths"), "{out:#?}");
        assert!(has("ll_tot_late"), "{out:#?}");
        assert!(has("ll_missed_gate"), "{out:#?}");
        assert!(has("fl_undershoot"), "{out:#?}");
        for i in &out {
            assert!(!i.title.is_empty() && !i.detail.is_empty());
        }
    }
}
