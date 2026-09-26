// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Aggregates over stored range records: list summaries, the pilot profile,
//! the greenie board, the leaderboards and the grouped-impact view.
//!
//! Everything here is a pure function of the records, recomputed on request
//! (the routes cache the whole-window ones for a short while). Nothing is
//! pre-aggregated in the database, so a grading fix in the engine or a new
//! board never needs a migration.

use super::{
    insights, quals,
    store::{RangeStore, Stored},
    RangeCtx,
};
use bfprotocols::range::{
    EngagementOutcome, MissileOutcome, PassOutcome, RangeRecord, RangeResult, WeaponClass,
};
use chrono::{DateTime, Datelike, Duration, Utc};
use serde_json::{json, Value};
use std::collections::{BTreeMap, HashMap};

/// A feed / list entry: the stored record (never with its track) plus what a
/// list needs to render it without a second request.
pub(crate) fn summary(store: &RangeStore, s: &Stored) -> Value {
    let mut v = s.raw.clone();
    let headline = match s.decode() {
        Some(r) => r.headline(),
        None => format!("{} ({}) {}", s.pilot_name(), s.unit_type(), s.kind()),
    };
    if let Some(o) = v.as_object_mut() {
        o.remove("track");
        o.insert("headline".into(), json!(headline));
        o.insert("has_track".into(), json!(store.has_track(&s.id)));
        o.insert("card_png".into(), json!(RangeCtx::card_url(&s.id, "png")));
        o.insert("card_svg".into(), json!(RangeCtx::card_url(&s.id, "svg")));
        o.insert("instance".into(), json!(s.instance));
    }
    v
}

fn mean(v: &[f64]) -> Option<f64> {
    (!v.is_empty()).then(|| v.iter().sum::<f64>() / v.len() as f64)
}

/// Median, the usual estimator of CEP from a set of radial misses.
pub(crate) fn median(v: &[f64]) -> Option<f64> {
    if v.is_empty() {
        return None;
    }
    let mut s = v.to_vec();
    s.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let n = s.len();
    Some(if n % 2 == 1 { s[n / 2] } else { (s[n / 2 - 1] + s[n / 2]) / 2. })
}

fn r1(x: f64) -> f64 {
    (x * 10.).round() / 10.
}

fn r2(x: f64) -> f64 {
    (x * 100.).round() / 100.
}

fn rfc(ts: DateTime<Utc>) -> String {
    ts.to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}

/// Monday of the ISO week a timestamp falls in, "YYYY-MM-DD".
fn week_of(ts: DateTime<Utc>) -> String {
    let d = ts.date_naive();
    let monday = d - Duration::days(d.weekday().num_days_from_monday() as i64);
    monday.format("%Y-%m-%d").to_string()
}

/// Decoded records, newest first, keeping the store's timestamp beside each.
pub(crate) type Decoded = Vec<(Stored, RangeRecord)>;

pub(crate) fn decode_all(rows: Vec<Stored>) -> Decoded {
    rows.into_iter()
        .filter_map(|s| {
            let r = s.decode()?;
            Some((s, r))
        })
        .collect()
}

// ── pilot profile ──────────────────────────────────────────────────────

pub(crate) fn pilot_profile(
    store: &RangeStore,
    instances: &[String],
    ucid: &str,
    now: DateTime<Utc>,
) -> anyhow::Result<Option<Value>> {
    let mut rows = vec![];
    store.walk_pilot(ucid, instances, |s| {
        rows.push(s);
        true
    })?;
    if rows.is_empty() {
        return Ok(None);
    }
    let name = store
        .pilot_name(ucid, instances)
        .unwrap_or_else(|| rows[0].pilot_name().to_string());
    let recent: Vec<Value> = rows.iter().take(20).map(|s| summary(store, s)).collect();

    // per kind, over everything on record
    let mut per_kind: BTreeMap<String, (usize, Vec<f64>, Option<u64>)> = BTreeMap::new();
    let mut airframes: HashMap<String, usize> = HashMap::new();
    let trend_from = now - Duration::weeks(26);
    let mut trend: BTreeMap<String, BTreeMap<String, (usize, Vec<f64>)>> = BTreeMap::new();
    for s in &rows {
        let e = per_kind.entry(s.kind().to_string()).or_default();
        e.0 += 1;
        let score = s.raw.get("score").and_then(|v| v.as_f64());
        if let Some(sc) = score {
            e.1.push(sc);
        }
        e.2 = Some(e.2.map_or(s.ts_ms, |t| t.max(s.ts_ms)));
        *airframes.entry(s.unit_type().to_string()).or_default() += 1;
        if s.ts() >= trend_from {
            let w = trend
                .entry(s.kind().to_string())
                .or_default()
                .entry(week_of(s.ts()))
                .or_default();
            w.0 += 1;
            if let Some(sc) = score {
                w.1.push(sc);
            }
        }
    }
    let per_kind: serde_json::Map<String, Value> = per_kind
        .into_iter()
        .map(|(k, (n, scores, last))| {
            let best = scores.iter().cloned().fold(None, |m: Option<f64>, x| {
                Some(m.map_or(x, |m| m.max(x)))
            });
            let last = last
                .and_then(|t| DateTime::from_timestamp_millis(t as i64))
                .map(rfc);
            (
                k,
                json!({
                    "count": n,
                    "avg_score": mean(&scores).map(r2),
                    "best_score": best,
                    "last_ts": last,
                }),
            )
        })
        .collect();
    let trend: serde_json::Map<String, Value> = trend
        .into_iter()
        .map(|(k, weeks)| {
            let pts: Vec<Value> = weeks
                .into_iter()
                .map(|(w, (n, sc))| json!({ "week": w, "avg_score": mean(&sc).map(r2), "count": n }))
                .collect();
            (k, Value::Array(pts))
        })
        .collect();
    let mut airframes: Vec<(String, usize)> = airframes.into_iter().collect();
    airframes.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));

    let decoded = decode_all(rows);
    let recs: Vec<&RangeRecord> = decoded.iter().map(|(_, r)| r).collect();
    let ins = insights::derive(&recs);
    let q = quals::evaluate(&recs, now);
    Ok(Some(json!({
        "ucid": ucid,
        "name": name,
        "per_kind": per_kind,
        "trend": trend,
        "insights": ins,
        "quals": q,
        "airframes": airframes
            .into_iter()
            .map(|(t, n)| json!({ "unit_type": t, "count": n }))
            .collect::<Vec<_>>(),
        "recent": recent,
    })))
}

// ── greenie board ──────────────────────────────────────────────────────

pub(crate) fn greenie(
    rows: &[Stored],
    carrier: Option<&str>,
    unit_type: Option<&str>,
) -> Value {
    struct Row {
        name: String,
        points: Vec<f64>,
        passes: Vec<Value>,
        count: usize,
    }
    let mut by: HashMap<String, Row> = HashMap::new();
    // newest first so the pass list and the name come out right
    for s in rows.iter().rev() {
        if s.kind() != "trap" {
            continue;
        }
        let Some(ucid) = s.ucid() else { continue };
        if let Some(u) = unit_type {
            if !s.unit_type().eq_ignore_ascii_case(u) {
                continue;
            }
        }
        let Some(r) = s.decode() else { continue };
        let RangeResult::Trap(t) = &r.result else { continue };
        if let Some(c) = carrier {
            if !t.carrier.eq_ignore_ascii_case(c) && !t.carrier_type.eq_ignore_ascii_case(c) {
                continue;
            }
        }
        let row = by.entry(ucid.to_string()).or_insert_with(|| Row {
            name: r.pilot.name.clone(),
            points: vec![],
            passes: vec![],
            count: 0,
        });
        row.count += 1;
        if let Some(p) = t.points {
            row.points.push(p);
        }
        if row.passes.len() < 30 {
            row.passes.push(json!({
                "id": s.id,
                "ts": rfc(s.ts()),
                "grade": t.grade,
                "points": t.points,
                "wire": t.wire,
                "case": t.case,
                "night": t.night,
                "outcome": t.outcome,
                "unit_type": r.unit_type,
            }));
        }
    }
    let mut out: Vec<(Option<f64>, usize, Value)> = by
        .into_iter()
        .map(|(ucid, row)| {
            let avg = mean(&row.points).map(r2);
            (
                avg,
                row.count,
                json!({
                    "ucid": ucid,
                    "name": row.name,
                    "avg_points": avg,
                    "count": row.count,
                    "passes": row.passes,
                }),
            )
        })
        .collect();
    out.sort_by(|a, b| {
        let ka = a.0.unwrap_or(-1.);
        let kb = b.0.unwrap_or(-1.);
        kb.partial_cmp(&ka).unwrap_or(std::cmp::Ordering::Equal).then(b.1.cmp(&a.1))
    });
    json!({ "rows": out.into_iter().map(|(_, _, v)| v).collect::<Vec<_>>() })
}

// ── leaderboards ───────────────────────────────────────────────────────

/// Standard Elo expectation / update, K = 32, start 1500.
fn elo_update(ra: f64, rb: f64, score_a: f64) -> f64 {
    let ea = 1. / (1. + 10f64.powf((rb - ra) / 400.));
    ra + 32. * (score_a - ea)
}

pub(crate) fn leaderboards(rows: &[Stored]) -> Value {
    #[derive(Default)]
    struct P {
        name: String,
        bomb_miss: Vec<f64>,
        bomb_score: Vec<f64>,
        strafe_acc: Vec<f64>,
        strafe_score: Vec<f64>,
        lso_points: Vec<f64>,
        traps: usize,
        aar_score: Vec<f64>,
        wins: usize,
        losses: usize,
        draws: usize,
        defeated: usize,
        killed: usize,
    }
    let mut by: HashMap<String, P> = HashMap::new();
    // (sorted pair, 30 s bucket) -> already applied, so the two participants'
    // records of one duel update the ratings once.
    let mut duel_seen: std::collections::HashSet<(String, String, i64)> = Default::default();
    let mut elo: HashMap<String, f64> = HashMap::new();
    // oldest first: Elo is order dependent
    for s in rows {
        let Some(ucid) = s.ucid() else { continue };
        let kind = s.kind();
        if !matches!(kind, "bomb" | "strafe" | "trap" | "aar" | "engagement" | "missile") {
            continue;
        }
        let Some(r) = s.decode() else { continue };
        let p = by.entry(ucid.to_string()).or_default();
        p.name = r.pilot.name.clone();
        match &r.result {
            RangeResult::Bomb(b) => {
                p.bomb_miss.push(b.miss_m);
                if let Some(sc) = r.score {
                    p.bomb_score.push(sc);
                }
            }
            RangeResult::Strafe(st) => {
                if st.quality != bfprotocols::range::StrafeQuality::Invalid {
                    p.strafe_acc.push(st.accuracy_pct);
                    if let Some(sc) = r.score {
                        p.strafe_score.push(sc);
                    }
                }
            }
            RangeResult::Trap(t) => {
                if let Some(pt) = t.points {
                    p.lso_points.push(pt);
                }
                if t.outcome == PassOutcome::Trap {
                    p.traps += 1;
                }
            }
            RangeResult::Aar(_) => {
                if let Some(sc) = r.score {
                    p.aar_score.push(sc);
                }
            }
            RangeResult::Engagement(e) => {
                let Some(opp) = e.opponent.as_ref().and_then(|o| o.ucid.clone()) else {
                    continue;
                };
                let sa = match e.outcome {
                    EngagementOutcome::Win => 1.,
                    EngagementOutcome::Loss => 0.,
                    EngagementOutcome::Draw => 0.5,
                    EngagementOutcome::Abort => continue,
                };
                match e.outcome {
                    EngagementOutcome::Win => p.wins += 1,
                    EngagementOutcome::Loss => p.losses += 1,
                    _ => p.draws += 1,
                }
                let (a, b) = if ucid < opp.as_str() {
                    (ucid.to_string(), opp.clone())
                } else {
                    (opp.clone(), ucid.to_string())
                };
                if duel_seen.insert((a, b, s.ts_ms as i64 / 30_000)) {
                    let ra = *elo.get(ucid).unwrap_or(&1500.);
                    let rb = *elo.get(&opp).unwrap_or(&1500.);
                    elo.insert(ucid.to_string(), elo_update(ra, rb, sa));
                    elo.insert(opp.clone(), elo_update(rb, ra, 1. - sa));
                }
            }
            RangeResult::Missile(m) => {
                if m.perspective == "target" {
                    match m.outcome {
                        MissileOutcome::Defeated | MissileOutcome::Timeout => p.defeated += 1,
                        MissileOutcome::Kill | MissileOutcome::Hit => p.killed += 1,
                    }
                }
            }
            _ => (),
        }
    }
    let mut bombing = vec![];
    let mut strafe = vec![];
    let mut lso = vec![];
    let mut aar = vec![];
    let mut duels = vec![];
    let mut md = vec![];
    for (ucid, p) in &by {
        if p.bomb_miss.len() >= 5 {
            bombing.push(json!({
                "ucid": ucid, "name": p.name, "count": p.bomb_miss.len(),
                "cep_m": median(&p.bomb_miss).map(r1),
                "avg_score": mean(&p.bomb_score).map(r2),
            }));
        }
        if p.strafe_acc.len() >= 3 {
            strafe.push(json!({
                "ucid": ucid, "name": p.name, "count": p.strafe_acc.len(),
                "avg_accuracy": mean(&p.strafe_acc).map(r1),
                "avg_score": mean(&p.strafe_score).map(r2),
            }));
        }
        if p.lso_points.len() >= 3 {
            lso.push(json!({
                "ucid": ucid, "name": p.name, "count": p.lso_points.len(),
                "avg_points": mean(&p.lso_points).map(r2),
                "traps": p.traps,
            }));
        }
        if p.aar_score.len() >= 2 {
            aar.push(json!({
                "ucid": ucid, "name": p.name, "count": p.aar_score.len(),
                "avg_score": mean(&p.aar_score).map(r2),
            }));
        }
        if p.wins + p.losses + p.draws > 0 {
            duels.push(json!({
                "ucid": ucid, "name": p.name, "count": p.wins + p.losses + p.draws,
                "wins": p.wins, "losses": p.losses,
                "elo": elo.get(ucid).map(|e| e.round()).unwrap_or(1500.),
            }));
        }
        if p.defeated + p.killed > 0 {
            md.push(json!({
                "ucid": ucid, "name": p.name, "count": p.defeated + p.killed,
                "defeated": p.defeated, "killed": p.killed,
            }));
        }
    }
    let f = |v: &Value, k: &str| v.get(k).and_then(|x| x.as_f64()).unwrap_or(f64::NAN);
    let desc = |k: &'static str| {
        move |a: &Value, b: &Value| {
            f(b, k).partial_cmp(&f(a, k)).unwrap_or(std::cmp::Ordering::Equal)
        }
    };
    bombing.sort_by(|a, b| f(a, "cep_m").partial_cmp(&f(b, "cep_m")).unwrap_or(std::cmp::Ordering::Equal));
    strafe.sort_by(desc("avg_accuracy"));
    lso.sort_by(desc("avg_points"));
    aar.sort_by(desc("avg_score"));
    duels.sort_by(desc("elo"));
    md.sort_by(|a, b| {
        let ra = f(a, "defeated") / f(a, "count");
        let rb = f(b, "defeated") / f(b, "count");
        rb.partial_cmp(&ra).unwrap_or(std::cmp::Ordering::Equal).then(
            f(b, "defeated").partial_cmp(&f(a, "defeated")).unwrap_or(std::cmp::Ordering::Equal),
        )
    });
    json!({
        "bombing": bombing,
        "strafe": strafe,
        "lso": lso,
        "aar": aar,
        "duels": duels,
        "missile_defense": md,
    })
}

// ── grouped impacts ────────────────────────────────────────────────────

pub(crate) fn station_impacts(rows: &[Stored], station: &str, pilot: Option<&str>) -> Value {
    let mut impacts = vec![];
    let mut misses = vec![];
    let mut target = Value::Null;
    let mut rings = json!([]);
    // newest first; the newest record defines the target and rings
    for s in rows.iter().rev() {
        if s.kind() != "bomb" || s.station() != Some(station) {
            continue;
        }
        if let Some(p) = pilot {
            if s.ucid() != Some(p) {
                continue;
            }
        }
        let Some(r) = s.decode() else { continue };
        let RangeResult::Bomb(b) = &r.result else { continue };
        if target.is_null() {
            target = json!(b.target_pos);
            rings = json!(b.rings_m);
        }
        if impacts.len() >= 1000 {
            continue;
        }
        misses.push(b.miss_m);
        let weapon = if b.weapon_display.is_empty() { &b.weapon } else { &b.weapon_display };
        impacts.push(json!({
            "id": s.id,
            "north_m": r1(b.impact_north_m),
            "east_m": r1(b.impact_east_m),
            "miss_m": r1(b.miss_m),
            "weapon": weapon,
            "weapon_class": b.weapon_class,
            "quality": b.quality,
            "ucid": r.pilot.ucid,
            "name": r.pilot.name,
            "ts": rfc(s.ts()),
        }));
    }
    json!({
        "target": target,
        "rings_m": rings,
        "impacts": impacts,
        "cep_m": median(&misses).map(r1),
    })
}

/// Unguided drops for the ballistics calibration: (weapon, result), newest
/// first.
pub(crate) fn unguided_drops(rows: &[Stored]) -> Vec<(String, bfprotocols::range::BombResult)> {
    rows.iter()
        .rev()
        .filter(|s| s.kind() == "bomb")
        .filter_map(|s| match s.decode()?.result {
            RangeResult::Bomb(b) if b.weapon_class == WeaponClass::Unguided => {
                Some((b.weapon.clone(), b))
            }
            _ => None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn medians_and_weeks() {
        assert_eq!(median(&[3., 1., 2.]), Some(2.));
        assert_eq!(median(&[4., 1., 2., 3.]), Some(2.5));
        assert_eq!(median(&[]), None);
        let t = DateTime::parse_from_rfc3339("2026-09-23T10:00:00Z").unwrap().with_timezone(&Utc);
        assert_eq!(week_of(t), "2026-09-21");
    }

    #[test]
    fn elo_is_symmetric() {
        let a = elo_update(1500., 1500., 1.);
        let b = elo_update(1500., 1500., 0.);
        assert!((a - 1516.).abs() < 1e-9 && (b - 1484.).abs() < 1e-9);
    }
}
