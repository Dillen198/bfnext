//! Golden fixtures shared with the range site.
//!
//! `bfrange-web/src/lib/{lso,grading,patternLegs}.ts` are hand ports of
//! `lso.rs`, `grading.rs` and `pattern_legs.rs`. The JSON files in
//! `fixtures/` hold inputs and what the Rust makes of them; this test checks
//! the Rust still agrees, and the vitest suites (`*.fixtures.test.ts`) check
//! the TypeScript does, so neither side can drift without a red test.
//!
//! After an intended change to the Rust, regenerate the files and review the
//! diff (then make the TypeScript agree):
//!
//! ```text
//! BLESS_RANGE_FIXTURES=1 cargo test -p bfprotocols range::fixtures
//! ```

use super::{
    cfg::ScoringCfg,
    grading, lso,
    pattern_legs::{self, RelSample, TaxiSample},
    WeaponClass,
};
use serde_json::{json, Value};
use std::path::PathBuf;

fn path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/range/fixtures").join(name)
}

fn bless() -> bool {
    std::env::var_os("BLESS_RANGE_FIXTURES").is_some()
}

// ------------------------------------------------------------ comparing

/// Equal, with floats within a relative 1e-9 (the TypeScript side compares
/// the same way).
fn same(a: &Value, b: &Value, at: &str) -> Result<(), String> {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => {
            let (x, y) = (x.as_f64().unwrap(), y.as_f64().unwrap());
            if (x - y).abs() <= 1e-9 * x.abs().max(y.abs()).max(1.) {
                Ok(())
            } else {
                Err(format!("{at}: {x} != {y}"))
            }
        }
        (Value::Array(x), Value::Array(y)) => {
            if x.len() != y.len() {
                return Err(format!("{at}: {} items != {}", x.len(), y.len()));
            }
            x.iter()
                .zip(y)
                .enumerate()
                .try_for_each(|(i, (x, y))| same(x, y, &format!("{at}[{i}]")))
        }
        (Value::Object(x), Value::Object(y)) => {
            let keys: std::collections::BTreeSet<&String> =
                x.keys().chain(y.keys()).collect();
            keys.into_iter().try_for_each(|k| {
                let n = Value::Null;
                same(x.get(k).unwrap_or(&n), y.get(k).unwrap_or(&n), &format!("{at}.{k}"))
            })
        }
        _ if a == b => Ok(()),
        _ => Err(format!("{at}: {a} != {b}")),
    }
}

// ------------------------------------------------------------ writing

/// Pretty JSON, but an array or object of plain values stays on one line,
/// so a track sample is one line rather than eight.
fn write_json(v: &Value, ind: usize, out: &mut String) {
    let flat = |v: &Value| match v {
        Value::Array(a) => a.iter().all(|x| !x.is_array() && !x.is_object()),
        Value::Object(o) => o.values().all(|x| !x.is_array() && !x.is_object()),
        _ => true,
    };
    let pad = |n: usize| "  ".repeat(n);
    match v {
        _ if flat(v) && v.to_string().len() <= 160 => {
            let s = serde_json::to_string(v).unwrap();
            // one space after separators, for reading
            out.push_str(&if v.is_object() || v.is_array() { spaced(&s) } else { s });
        }
        Value::Array(a) => {
            out.push_str("[\n");
            for (i, x) in a.iter().enumerate() {
                out.push_str(&pad(ind + 1));
                write_json(x, ind + 1, out);
                out.push_str(if i + 1 < a.len() { ",\n" } else { "\n" });
            }
            out.push_str(&pad(ind));
            out.push(']');
        }
        Value::Object(o) => {
            out.push_str("{\n");
            for (i, (k, x)) in o.iter().enumerate() {
                out.push_str(&pad(ind + 1));
                out.push_str(&serde_json::to_string(k).unwrap());
                out.push_str(": ");
                write_json(x, ind + 1, out);
                out.push_str(if i + 1 < o.len() { ",\n" } else { "\n" });
            }
            out.push_str(&pad(ind));
            out.push('}');
        }
        _ => out.push_str(&v.to_string()),
    }
}

/// `{"a":1,"b":[2,3]}` -> `{"a": 1, "b": [2, 3]}`, outside strings only.
fn spaced(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 16);
    let (mut in_str, mut esc) = (false, false);
    for c in s.chars() {
        out.push(c);
        if in_str {
            match (esc, c) {
                (true, _) => esc = false,
                (false, '\\') => esc = true,
                (false, '"') => in_str = false,
                _ => (),
            }
        } else if c == '"' {
            in_str = true;
        } else if c == ',' || c == ':' {
            out.push(' ');
        }
    }
    out
}

/// Bless: write `v`. Otherwise: compare `v` (computed from the file's own
/// inputs) with the file.
fn check(name: &str, computed: Value) {
    let p = path(name);
    if bless() {
        let mut s = String::new();
        write_json(&computed, 0, &mut s);
        s.push('\n');
        std::fs::write(&p, s).unwrap();
        return;
    }
    let txt = std::fs::read_to_string(&p).unwrap_or_else(|e| panic!("{p:?}: {e}"));
    let stored: Value = serde_json::from_str(&txt).unwrap();
    if let Err(e) = same(&stored, &computed, name) {
        panic!("{e}\n\nthe Rust no longer matches {p:?}; if the change is intended, run BLESS_RANGE_FIXTURES=1 cargo test -p bfprotocols range::fixtures, review the diff and port it to bfrange-web")
    }
}

/// The inputs: the file's when checking (so a case added to the file is
/// tested), the code's when blessing.
fn inputs(name: &str, key: &str, default: Value) -> Vec<Value> {
    if bless() {
        return default.as_array().unwrap().clone();
    }
    let txt =
        std::fs::read_to_string(path(name)).unwrap_or_else(|e| panic!("{name}: {e}"));
    let v: Value = serde_json::from_str(&txt).unwrap();
    v[key].as_array().unwrap_or_else(|| panic!("{name}: no {key}")).clone()
}

fn note(what: &str) -> Value {
    json!(format!(
        "Generated by bfprotocols/src/range/fixtures.rs from {what}: run BLESS_RANGE_FIXTURES=1 cargo test -p bfprotocols range::fixtures to regenerate. Read by the Rust test and by bfrange-web's vitest suites."
    ))
}

// ------------------------------------------------------------ lso

#[test]
fn lso_fixtures() {
    const F: &str = "lso.json";
    let comments = inputs(
        F,
        "comments",
        json!([
            "LSO: GRADE:WO AAX FIM (SLO)AR _HAW_",
            "LSO: GRADE:_OK_ : WIRE# 3",
            "LSO: GRADE:C _SLOX_ _LURX_ 3PTSIW LNFIW WIRE #1",
            "GRADE:--- : _LOIC_ _LOAR_",
            "GRADE:WO  AAX FIM (SLO)AR _HAW_",
            "LSO: GRADE:(OK) (DRX) _SLO_AR (LULIM) WIRE# 2",
            "LSO: GRADE:OK",
            "LSO: GRADE:BOLTER (LOIC) _FIC_",
            "LSO: GRADE:OWO TMRDIC NERDAR",
            "LSO: GRADE:WOP",
            "LSO: GRADE:CUT LLWDIW WIRE#4",
            "lso: grade:ok wire 2",
            "LSO: GRADE:OK WIRE# 7",
            "LSO: GRADE:B (SLO (HIM _X ZZZ",
            "no grade in this one",
        ]),
    );
    let comments: Vec<Value> = comments
        .iter()
        .map(|c| {
            let raw = c.get("raw").unwrap_or(c).as_str().unwrap();
            json!({ "raw": raw, "expect": lso::parse_comment(raw) })
        })
        .collect();
    let grades = inputs(
        F,
        "grades",
        json!([
            "_OK_", "OK", "(OK)", "B", "--", "OWO", "WO", "WOP", "WOFD", "C", "NC", "ZZ"
        ]),
    );
    let grades: Vec<Value> = grades
        .iter()
        .map(|g| {
            let g = g.get("grade").unwrap_or(g).as_str().unwrap();
            json!({ "grade": g, "name": lso::grade_name(g), "points": lso::grade_points(g) })
        })
        .collect();
    let normalize = inputs(
        F,
        "normalize",
        json!([
            "---", "--", "-", "CUT", "BOLTER", "WOAF", "TWO", "TLU", " OK: ", "_OK_::",
            "WOFD"
        ]),
    );
    let normalize: Vec<Value> = normalize
        .iter()
        .map(|g| {
            let g = g.get("raw").unwrap_or(g).as_str().unwrap();
            json!({ "raw": g, "expect": lso::normalize_grade(g) })
        })
        .collect();
    check(
        F,
        json!({
            "_note": note("lso.rs"),
            "comments": comments,
            "grades": grades,
            "normalize": normalize,
        }),
    );
}

// ------------------------------------------------------------ grading

#[test]
fn grading_fixtures() {
    const F: &str = "grading.json";
    let cfg = ScoringCfg::default();
    let class = |v: &Value| -> WeaponClass { serde_json::from_value(v.clone()).unwrap() };
    let f = |v: &Value, k: &str| v[k].as_f64().unwrap();
    let mut bombs = vec![];
    for c in ["unguided", "guided", "rocket", "cluster", "missile", "gun"] {
        for m in [0.5, 1.53, 4., 9., 12.5, 18., 25., 40., 61., 120.] {
            bombs.push(json!({ "class": c, "miss_m": m }));
        }
    }
    let bomb: Vec<Value> = inputs(F, "bomb", json!(bombs))
        .iter()
        .map(|b| {
            let c = class(&b["class"]);
            json!({
                "class": b["class"],
                "miss_m": b["miss_m"],
                "good_radius_m": grading::good_radius(&cfg, c),
                "expect": grading::bomb_quality(&cfg, c, f(b, "miss_m")),
            })
        })
        .collect();
    let mut strafes = vec![];
    for a in [100., 95., 90., 89.9, 75., 60., 50., 40., 25., 10., 0.] {
        strafes.push(json!({ "accuracy_pct": a, "valid": true }));
    }
    strafes.push(json!({ "accuracy_pct": 95., "valid": false }));
    let strafe: Vec<Value> = inputs(F, "strafe", json!(strafes))
        .iter()
        .map(|s| {
            json!({
                "accuracy_pct": s["accuracy_pct"],
                "valid": s["valid"],
                "expect": grading::strafe_quality(&cfg, f(s, "accuracy_pct"), s["valid"].as_bool().unwrap()),
            })
        })
        .collect();
    let mut precs = vec![];
    for d in [0., 2., 2.5, 4., 7.9, 8., 15., 16., 16.1, 40.] {
        precs.push(json!({ "perfect_m": 2., "distance_m": d }));
    }
    precs.push(json!({ "perfect_m": 15., "distance_m": 100. }));
    let precision: Vec<Value> = inputs(F, "precision", json!(precs))
        .iter()
        .map(|p| {
            json!({
                "perfect_m": p["perfect_m"],
                "distance_m": p["distance_m"],
                "expect": grading::precision_quality(f(p, "perfect_m"), f(p, "distance_m")),
            })
        })
        .collect();
    let clock: Vec<Value> = inputs(
        F,
        "clock",
        json!([
            0., 14., 16., 45., 90., 179., 180., 270., -90., -14., -16., 345., 359., 360.,
            735.
        ]),
    )
    .iter()
    .map(|r| {
        let r = r.get("rel_deg").unwrap_or(r).as_f64().unwrap();
        json!({ "rel_deg": r, "expect": grading::clock(r) })
    })
    .collect();
    let call = |c: Option<(&str, lso::Magnitude)>| match c {
        Some((e, m)) => json!({ "error": e, "mag": m }),
        None => Value::Null,
    };
    let glideslope: Vec<Value> = inputs(
        F,
        "glideslope",
        json!([
            2., 1.55, 1.5, 1.2, 0.85, 0.8, 0.6, 0.45, 0.4, 0.2, 0., -0.2, -0.3, -0.35,
            -0.5, -0.6, -0.65, -0.8, -0.9, -0.95, -1.4
        ]),
    )
    .iter()
    .map(|g| {
        let g = g.get("gse_deg").unwrap_or(g).as_f64().unwrap();
        json!({ "gse_deg": g, "expect": call(grading::glideslope_call(g)) })
    })
    .collect();
    let lineup: Vec<Value> = inputs(
        F,
        "lineup",
        json!([
            4., 3.05, 3., 2., 1.05, 1., 0.7, 0.55, 0.5, 0.2, 0., -0.2, -0.55, -0.7,
            -1.05, -1.5, -3.05, -3.5
        ]),
    )
    .iter()
    .map(|l| {
        let l = l.get("lue_deg").unwrap_or(l).as_f64().unwrap();
        json!({ "lue_deg": l, "expect": call(grading::lineup_call(l)) })
    })
    .collect();
    let waveoff: Vec<Value> = inputs(
        F,
        "waveoff",
        json!([
            [0., 0.],
            [0., 3.1],
            [0., -3.1],
            [-1.3, 0.],
            [1.9, 0.],
            [-1.2, 3.],
            [1.8, -3.]
        ]),
    )
    .iter()
    .map(|w| {
        let (g, l) = match w.get("gse_deg") {
            Some(g) => (g.as_f64().unwrap(), f(w, "lue_deg")),
            None => (w[0].as_f64().unwrap(), w[1].as_f64().unwrap()),
        };
        json!({ "gse_deg": g, "lue_deg": l, "expect": grading::waveoff_limits(g, l) })
    })
    .collect();
    let aar_in = json!([
        { "contacts": 1, "disconnects": 1, "join_time_s": 90., "fore_aft_sd": 0.8, "lateral_sd": 0.5, "vertical_sd": 0.4, "precontact_closure_kts": 2., "overshoot": false },
        { "contacts": 0, "disconnects": 0, "join_time_s": null, "fore_aft_sd": 0., "lateral_sd": 0., "vertical_sd": 0., "precontact_closure_kts": null, "overshoot": false },
        { "contacts": 2, "disconnects": 2, "join_time_s": 300., "fore_aft_sd": 2.6, "lateral_sd": 1.9, "vertical_sd": 1.2, "precontact_closure_kts": 6.2, "overshoot": false },
        { "contacts": 4, "disconnects": 5, "join_time_s": 500., "fore_aft_sd": 5., "lateral_sd": 4., "vertical_sd": 3.5, "precontact_closure_kts": 12.4, "overshoot": true },
        { "contacts": 1, "disconnects": 0, "join_time_s": 241., "fore_aft_sd": 2.1, "lateral_sd": 1.4, "vertical_sd": 1.7, "precontact_closure_kts": null, "overshoot": false },
        { "contacts": 3, "disconnects": 3, "join_time_s": 120., "fore_aft_sd": 1., "lateral_sd": 1., "vertical_sd": 1., "precontact_closure_kts": 4.9, "overshoot": true }
    ]);
    let aar: Vec<Value> = inputs(F, "aar", aar_in)
        .iter()
        .map(|a| {
            let a = a.get("input").unwrap_or(a);
            let mut calls = vec![];
            let (score, letter) = grading::aar_grade(
                a["contacts"].as_u64().unwrap() as u32,
                a["disconnects"].as_u64().unwrap() as u32,
                a["join_time_s"].as_f64(),
                f(a, "fore_aft_sd"),
                f(a, "lateral_sd"),
                f(a, "vertical_sd"),
                a["precontact_closure_kts"].as_f64(),
                a["overshoot"].as_bool().unwrap(),
                &mut calls,
            );
            json!({ "input": a, "expect": { "score": score, "letter": letter, "calls": calls } })
        })
        .collect();
    check(
        F,
        json!({
            "_note": note("grading.rs with the default ScoringCfg"),
            "scoring_defaults": {
                "shack_m": cfg.shack_m,
                "good_unguided_m": cfg.good_unguided_m,
                "good_guided_m": cfg.good_guided_m,
                "good_rocket_m": cfg.good_rocket_m,
                "strafe_bands": cfg.strafe_bands,
            },
            "bomb": bomb,
            "strafe": strafe,
            "precision": precision,
            "clock": clock,
            "glideslope": glideslope,
            "lineup": lineup,
            "waveoff": waveoff,
            "aar": aar,
        }),
    );
}

// ------------------------------------------------------------ pattern legs

const NM: f64 = 1852.;
const M_TO_FT: f64 = 3.28084;

fn rel_json(s: &[RelSample]) -> Value {
    json!(s
        .iter()
        .map(|x| json!([x.t, x.along, x.across, x.agl, x.rel, x.gs_kts]))
        .collect::<Vec<_>>())
}

fn rel_from(v: &Value) -> Vec<RelSample> {
    v.as_array()
        .unwrap()
        .iter()
        .map(|x| {
            let n = |i: usize| x[i].as_f64().unwrap();
            RelSample {
                t: n(0),
                along: n(1),
                across: n(2),
                agl: n(3),
                rel: n(4),
                gs_kts: n(5),
            }
        })
        .collect()
}

fn taxi_json(s: &[TaxiSample]) -> Value {
    json!(s
        .iter()
        .map(|x| json!([x.t, x.along, x.across, x.gs_kts, x.x, x.z]))
        .collect::<Vec<_>>())
}

fn taxi_from(v: &Value) -> Vec<TaxiSample> {
    v.as_array()
        .unwrap()
        .iter()
        .map(|x| {
            let n = |i: usize| x[i].as_f64().unwrap();
            TaxiSample {
                t: n(0),
                along: n(1),
                across: n(2),
                gs_kts: n(3),
                x: n(4),
                z: n(5),
            }
        })
        .collect()
}

/// Rounded to the centimetre / hundredth so the file stays readable; the
/// computation then runs on exactly these values on both sides.
fn tidy(mut s: Vec<RelSample>) -> Vec<RelSample> {
    let r = |x: f64| (x * 100.).round() / 100.;
    for x in s.iter_mut() {
        *x = RelSample {
            t: r(x.t),
            along: r(x.along),
            across: r(x.across),
            agl: r(x.agl),
            rel: r(x.rel),
            gs_kts: r(x.gs_kts),
        };
    }
    s
}

fn straight_in(drift_right_m: f64) -> Vec<RelSample> {
    let mut s = vec![];
    let mut a = -6. * NM;
    let mut t = 0.;
    while a < 300. {
        // a final that swings `drift_right_m` across the centreline between
        // 1.5 and 0.3 nm, then corrects
        let over = if (-1.5 * NM..-0.3 * NM).contains(&a) { drift_right_m } else { 0. };
        s.push(RelSample {
            t,
            along: a,
            across: over,
            agl: (-a).max(0.) * 0.0524,
            rel: 1.,
            gs_kts: 140.,
        });
        a += 100.;
        t += 1.4;
    }
    s
}

fn pattern_cases() -> Vec<(&'static str, Vec<RelSample>)> {
    let overhead = pattern_legs::tests::overhead();
    let wide: Vec<RelSample> = overhead
        .iter()
        .map(|x| {
            let recip = (180. - ((x.rel + 180.).rem_euclid(360.) - 180.).abs()) < 25.;
            RelSample { across: if recip { x.across * 2. } else { x.across }, ..*x }
        })
        .collect();
    // joining on the downwind: the overhead without its initial and break
    let first_dw = overhead.iter().position(|x| (180. - x.rel.abs()) < 25.).unwrap();
    let downwind_entry = overhead[first_dw..].to_vec();
    // a low, tight downwind that extends well past the threshold
    let low_tight: Vec<RelSample> = overhead
        .iter()
        .map(|x| {
            let recip = (180. - ((x.rel + 180.).rem_euclid(360.) - 180.).abs()) < 25.;
            if recip {
                RelSample { across: x.across * 0.4, agl: 450. / M_TO_FT, ..*x }
            } else {
                *x
            }
        })
        .collect();
    vec![
        ("overhead_left", overhead.clone()),
        ("wide_downwind", wide),
        ("downwind_entry", downwind_entry),
        ("low_tight_downwind", low_tight),
        ("straight_in", straight_in(0.)),
        ("straight_in_overshoot", straight_in(240.)),
        ("too_short", overhead[..4].to_vec()),
    ]
}

fn taxi_cases() -> Vec<(&'static str, f64, f64, f64, &'static str, Vec<TaxiSample>)> {
    let mut parked = vec![];
    let mut t = 100.;
    let (mut along, mut gs) = (300., 130.);
    while gs > 20. {
        parked.push(TaxiSample { t, along, across: 0., gs_kts: gs, x: along, z: 0. });
        along += gs * 0.514;
        gs -= 6.;
        t += 1.;
    }
    let roll_end = (t, along);
    for k in 0..40 {
        let g = if (10..15).contains(&k) {
            38.
        } else if (20..26).contains(&k) {
            0.
        } else {
            15.
        };
        parked.push(TaxiSample {
            t,
            along,
            across: 30. + k as f64 * 8.,
            gs_kts: g,
            x: along,
            z: 30. + k as f64 * 8.,
        });
        t += 1.;
    }
    // stops on the runway after the rollout and never turns off
    let mut stuck: Vec<TaxiSample> =
        parked.iter().filter(|x| x.t < roll_end.0).copied().collect();
    let (mut t, along) = roll_end;
    for _ in 0..20 {
        stuck.push(TaxiSample { t, along, across: 1., gs_kts: 0., x: along, z: 1. });
        t += 1.;
    }
    // a slow exit: crawls down the runway for 80 s before turning off
    let mut slow: Vec<TaxiSample> =
        parked.iter().filter(|x| x.t < roll_end.0).copied().collect();
    let (mut t, mut along) = roll_end;
    for k in 0..90 {
        let across = if k < 80 { 0. } else { 30. + (k - 80) as f64 * 8. };
        slow.push(TaxiSample { t, along, across, gs_kts: 8., x: along, z: across });
        along += 4.;
        t += 1.;
    }
    vec![
        ("parked", 100., 2400., 45., "parked", parked),
        ("stopped_on_runway", 100., 2400., 45., "stopped_on_runway", stuck),
        ("slow_exit", 100., 2400., 45., "parked", slow),
        ("empty", 100., 2400., 45., "timeout", vec![]),
    ]
}

#[test]
fn pattern_leg_fixtures() {
    const F: &str = "pattern_legs.json";
    let pats: Vec<Value> = pattern_cases()
        .into_iter()
        .map(|(name, s)| {
            let s = tidy(s);
            json!({ "name": name, "t_touch": s.last().map(|x| x.t).unwrap_or(0.), "samples": rel_json(&s) })
        })
        .collect();
    let patterns: Vec<Value> = inputs(F, "patterns", json!(pats))
        .iter()
        .map(|p| {
            let s = rel_from(&p["samples"]);
            let tt = p["t_touch"].as_f64().unwrap();
            let legs: Vec<&str> =
                pattern_legs::legs(&s, tt).iter().map(|l| l.label()).collect();
            json!({
                "name": p["name"],
                "t_touch": tt,
                "samples": p["samples"],
                "legs": legs,
                "pattern": pattern_legs::pattern(&s, tt),
            })
        })
        .collect();
    let tx: Vec<Value> = taxi_cases()
        .into_iter()
        .map(|(name, tt, len, width, end, s)| {
            json!({ "name": name, "t_touch": tt, "rwy_len": len, "rwy_width": width, "end": end, "samples": taxi_json(&s) })
        })
        .collect();
    let taxi: Vec<Value> = inputs(F, "taxi", json!(tx))
        .iter()
        .map(|c| {
            let s = taxi_from(&c["samples"]);
            let f = |k: &str| c[k].as_f64().unwrap();
            json!({
                "name": c["name"],
                "t_touch": c["t_touch"],
                "rwy_len": c["rwy_len"],
                "rwy_width": c["rwy_width"],
                "end": c["end"],
                "samples": c["samples"],
                "expect": pattern_legs::taxi(&s, f("t_touch"), f("rwy_len"), f("rwy_width"), c["end"].as_str().unwrap()),
            })
        })
        .collect();
    check(
        F,
        json!({
            "_note": note("pattern_legs.rs; samples are [t, along, across, agl, rel, gs_kts] and taxi samples [t, along, across, gs_kts, x, z]"),
            "patterns": patterns,
            "taxi": taxi,
        }),
    );
}

// ------------------------------------------------------------ JSON Schema

/// The JSON Schema of every range wire type, which bfrange-web's
/// `types.schema.test.ts` checks `types.ts` against: the TypeScript is still
/// written by hand (with its comments), but a field added, removed, renamed
/// or retyped on one side only fails a test.
#[test]
fn wire_schema() {
    use super::*;
    let mut g = schemars::gen::SchemaSettings::draft07().into_generator();
    macro_rules! roots {
        ($($t:ty),* $(,)?) => { $( let _ = g.subschema_for::<$t>(); )* };
    }
    roots!(
        AarResult,
        AirdropRelease,
        AirdropResult,
        AntiShipResult,
        AtmoLayer,
        BombQuality,
        BombResult,
        CasResult,
        CatalogItem,
        CsarOutcome,
        CsarResult,
        DefenseSummary,
        EngagementOutcome,
        EngagementResult,
        EngineGrade,
        FieldLandingOutcome,
        FieldLandingResult,
        FieldPattern,
        FieldTaxi,
        GateTime,
        GeoPt,
        GradeSource,
        GrooveSample,
        GunneryResult,
        HotZoneOutcome,
        HotZoneResult,
        LandingResult,
        LaunchGeom,
        LiveArena,
        LiveCarrier,
        LiveCsar,
        LiveHotZone,
        LiveIads,
        LiveJammer,
        LivePlayer,
        LiveSamSite,
        LiveShipDeck,
        LiveSpawn,
        LiveStation,
        LiveTanker,
        LowLevelResult,
        MissileOutcome,
        MissileResult,
        ParamKind,
        ParamOption,
        ParamSpec,
        PassOutcome,
        PatternSummary,
        PilotRef,
        PrecisionQuality,
        RangeLive,
        RangeRecord,
        RangeResult,
        RefuelMethod,
        RelSample,
        Release,
        SeadResult,
        SkyInfo,
        SlingResult,
        SpawnCatalog,
        SpawnCategory,
        SpawnReply,
        SpawnRequest,
        Stability,
        StationKind,
        StrafeQuality,
        StrafeResult,
        TankerState,
        Track,
        TrackPt,
        TrapResult,
        TroopResult,
        WeaponBallistics,
        WeaponClass,
        WeaponDb,
        WindInfo,
        cfg::SectorCfg,
    );
    // the shape only: a doc comment edited in mod.rs is not a change
    fn strip(v: &mut Value) {
        match v {
            Value::Object(o) => {
                for k in ["description", "format", "minimum", "maximum"] {
                    o.remove(k);
                }
                for (k, x) in o.iter_mut() {
                    match (k.as_str(), x) {
                        // field names, not schema keywords: keep the keys
                        ("properties", Value::Object(p)) => {
                            p.values_mut().for_each(strip)
                        }
                        (_, x) => strip(x),
                    }
                }
            }
            Value::Array(a) => a.iter_mut().for_each(strip),
            _ => (),
        }
    }
    let mut defs = serde_json::to_value(g.definitions()).unwrap();
    if let Value::Object(d) = &mut defs {
        d.values_mut().for_each(strip);
    }
    check(
        "range.schema.json",
        json!({
            "_note": note("the range wire types (bfprotocols/src/range/mod.rs, plus the sector types from cfg.rs)"),
            "definitions": defs,
        }),
    );
}

#[test]
fn compare_tolerates_float_noise_only() {
    assert!(same(&json!({"a": 1.0, "b": null}), &json!({"a": 1.0 + 1e-12}), "x").is_ok());
    assert!(same(&json!({"a": 1.0}), &json!({"a": 1.001}), "x").is_err());
    assert!(same(&json!(["a"]), &json!(["b"]), "x").is_err());
    assert_eq!(spaced(r#"{"a":"x,y:z","b":[1,2]}"#), r#"{"a": "x,y:z", "b": [1, 2]}"#);
}
