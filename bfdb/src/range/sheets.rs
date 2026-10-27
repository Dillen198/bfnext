// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Result cards as forms for every discipline the carrier pass, the bombing
//! sheet and the field landing record ([`super::forms`]) do not cover: one
//! layout, the way a squadron's forms share one -- the header, two rows of
//! boxed fields (the event, then its parameters), the score block, a main
//! drawing with a side panel, and the instructor's remarks -- with each
//! discipline's own drawing in it, made from what its result records:
//!
//! * strafe -- the pit from above on the run-in, the foul line and the
//!   closest approach; the burst as rounds against hits
//! * air refuelling -- the receiver in the tanker's frame, plan and side
//! * missile defence -- the engagement in plan, the miss; range against time
//! * BFM -- both flight paths in plan; altitude against time
//! * anti-ship -- launch to ship with the weapon's reach; the weapon's height
//! * sling load, landing -- the drop zone or the pad, with the set-down
//! * troops, gunnery, CSAR, hot zone -- the timeline, the lane, the tally
//! * CAS -- target, impact and the nearest friendlies on one distance line
//! * SEAD -- the site, the launch and the range rings
//! * low level -- the route with its gates; height at every gate
//! * airdrop -- the drop zone with where the load came down; the release

use super::{
    cards::{
        en, nice_step, precision_color, strafe_color, trunc, Anchor, Svg, BLUE, GREEN,
        GRID, MUTED, ORANGE, PANEL, RED, SAND, TEXT, YELLOW,
    },
    discord,
    forms::{field, foot, head, text_f, FOOT_H, HAND, MONO},
};
use bfprotocols::range::{
    AarResult, AirdropResult, AntiShipResult, CasResult, CsarOutcome, CsarResult,
    EngagementOutcome, EngagementResult, GeoPt, GunneryResult, HotZoneOutcome,
    HotZoneResult, LandingResult, LowLevelResult, MissileOutcome, MissileResult,
    RangeRecord, RangeResult, SeadResult, SlingResult, StrafeResult, Track, TrackPt,
    TroopResult,
};

const W: f64 = 1100.;
const FT: f64 = 3.28084;
const NM: f64 = 1852.;
/// Where the drawings start, how tall the main one is.
const DRAW_Y: f64 = 346.;
const DRAW_H: f64 = 560.;
/// The main drawing (left) and the side column (right).
const MX: f64 = 24.;
const MW: f64 = 620.;
const SX: f64 = 664.;
const SW: f64 = W - 24. - 664.;

type Draw<'a> = Box<dyn Fn(&mut Svg, f64, f64, f64, f64) + 'a>;

/// One form: everything but the drawings is data.
struct Sheet<'a> {
    title: &'a str,
    sub: &'a str,
    form_no: &'a str,
    /// the event, then its parameters: (label, value, colour, mono)
    row_a: Vec<(&'a str, String, &'a str, bool)>,
    row_b: Vec<(&'a str, String, &'a str, bool)>,
    /// the big number, what it is, its colour, a line under it
    score: (String, String, &'a str, String),
    /// the score block's right half: up to six (label, value)
    detail: Vec<(&'a str, String)>,
    main: (&'a str, Draw<'a>),
    side: (&'a str, f64, Draw<'a>),
    remarks: Vec<String>,
}

fn render(rec: &RangeRecord, sh: Sheet) -> String {
    let h = DRAW_Y + DRAW_H + 22. + FOOT_H + 10.;
    let mut s = Svg::new(W, h);
    head(&mut s, rec, sh.title, sh.sub, sh.form_no);
    for (y, row) in [(108., &sh.row_a), (158., &sh.row_b)] {
        let fw = (W - 48.) / row.len().max(1) as f64;
        for (i, (k, v, c, mono)) in row.iter().enumerate() {
            field(&mut s, 24. + i as f64 * fw, y, fw, 46., k, v, 13., c, *mono);
        }
    }
    // the score
    let (big, what, col, under) = &sh.score;
    s.rect(24., 212., 360., 110., "none", Some((TEXT, 1.6)), 0.);
    s.text(32., 226., "SCORE", 9.5, MUTED, Anchor::Start, true);
    let size = if big.chars().count() > 9 { 30. } else { 42. };
    text_f(&mut s, 204., 274., big, size, col, Anchor::Middle, true, MONO);
    text_f(&mut s, 204., 298., &trunc(what, 40), 13., TEXT, Anchor::Middle, true, MONO);
    s.text(204., 315., &trunc(under, 52), 11.5, col, Anchor::Middle, true);
    s.rect(392., 212., W - 24. - 392., 110., "none", Some((TEXT, 1.)), 0.);
    s.text(400., 226., "DETAIL", 9.5, MUTED, Anchor::Start, true);
    for (i, (k, v)) in sh.detail.iter().take(6).enumerate() {
        let (x, y) = (400. + (i / 3) as f64 * 340., 248. + (i % 3) as f64 * 24.);
        s.text(x, y, k, 11.5, MUTED, Anchor::Start, false);
        text_f(&mut s, x + 316., y, &trunc(v, 30), 13., TEXT, Anchor::End, true, MONO);
    }
    // the drawings
    let (mt, md) = &sh.main;
    s.text(MX, DRAW_Y - 8., mt, 12., TEXT, Anchor::Start, true);
    s.rect(MX, DRAW_Y, MW, DRAW_H, PANEL, Some((TEXT, 1.)), 0.);
    md(&mut s, MX, DRAW_Y, MW, DRAW_H);
    let (st, sh_h, sd) = &sh.side;
    s.text(SX, DRAW_Y - 8., st, 12., TEXT, Anchor::Start, true);
    s.rect(SX, DRAW_Y, SW, *sh_h, PANEL, Some((TEXT, 1.)), 0.);
    sd(&mut s, SX, DRAW_Y, SW, *sh_h);
    // the remarks, under the side panel
    let (by, bh) = (DRAW_Y + sh_h + 30., DRAW_H - sh_h - 30.);
    s.rect(SX, by, SW, bh, "none", Some((TEXT, 1.)), 0.);
    s.text(SX + 8., by + 14., "INSTRUCTOR REMARKS", 9.5, MUTED, Anchor::Start, true);
    let lines = ((bh - 40.) / 25.).floor().max(1.) as usize;
    for (i, r) in sh.remarks.iter().take(lines).enumerate() {
        text_f(
            &mut s,
            SX + 12.,
            by + 40. + i as f64 * 25.,
            &trunc(r, 54),
            13.,
            "#1a2f6e",
            Anchor::Start,
            false,
            HAND,
        );
    }
    s.text(
        SX + SW - 10.,
        by + bh - 8.,
        "SCORED BY: RANGE ENGINE",
        9.5,
        MUTED,
        Anchor::End,
        true,
    );
    foot(&mut s, rec);
    s.finish()
}

// ── drawing helpers ─────────────────────────────────────────────────────

/// Capitalise the first letter, end with a full stop.
fn sentence(c: &str) -> String {
    let c = c.trim().trim_end_matches('.');
    let mut ch = c.chars();
    ch.next().map(|f| format!("{}{}.", f.to_uppercase(), ch.as_str())).unwrap_or_default()
}

fn mmss(s: f64) -> String {
    discord::mmss(s)
}

/// A plan view, north up, every point fitted with a margin; returns the
/// metres -> page mapping and the scale (px per metre). `o` is the origin.
struct Plan {
    o: (f64, f64),
    cx: f64,
    cy: f64,
    k: f64,
    c: (f64, f64),
}

impl Plan {
    fn fit(
        o: (f64, f64),
        pts: &[(f64, f64)],
        x: f64,
        y: f64,
        w: f64,
        h: f64,
        min_span: f64,
    ) -> Self {
        let (mut x0, mut x1, mut y0, mut y1) =
            (-min_span / 2., min_span / 2., -min_span / 2., min_span / 2.);
        for p in pts {
            let (e, n) = en(o, *p);
            x0 = x0.min(e);
            x1 = x1.max(e);
            y0 = y0.min(n);
            y1 = y1.max(n);
        }
        let k = ((w - 60.) / (x1 - x0)).min((h - 60.) / (y1 - y0));
        Plan { o, cx: x + w / 2., cy: y + h / 2., k, c: ((x0 + x1) / 2., (y0 + y1) / 2.) }
    }
    fn at(&self, p: (f64, f64)) -> (f64, f64) {
        let (e, n) = en(self.o, p);
        self.m(e, n)
    }
    /// east / north metres from the origin
    fn m(&self, e: f64, n: f64) -> (f64, f64) {
        (self.cx + (e - self.c.0) * self.k, self.cy - (n - self.c.1) * self.k)
    }
}

fn ll(p: &GeoPt) -> (f64, f64) {
    (p.lat, p.lon)
}
fn ll_t(p: &TrackPt) -> (f64, f64) {
    (p.lat, p.lon)
}
fn has(p: &GeoPt) -> bool {
    p.lat != 0. || p.lon != 0.
}

/// North arrow and a scale bar in a plan panel's corners.
fn plan_marks(s: &mut Svg, x: f64, y: f64, w: f64, h: f64, k: f64) {
    let (nx, ny) = (x + w - 28., y + 30.);
    s.circle(nx, ny, 15., "#ffffff", Some((TEXT, 0.8)), None);
    s.arrow(nx, ny + 10., nx, ny - 11., TEXT, 1.6);
    s.text(nx, ny - 20., "N", 11., TEXT, Anchor::Middle, true);
    let target = 120. / k;
    let step = nice_step(target, 1.);
    let bar = step * k;
    let (bx, by) = (x + 14., y + h - 16.);
    s.line(bx, by, bx + bar, by, TEXT, 2., None);
    s.line(bx, by - 5., bx, by + 5., TEXT, 1.2, None);
    s.line(bx + bar, by - 5., bx + bar, by + 5., TEXT, 1.2, None);
    let lbl = if step >= 1000. {
        format!("{:.1} NM", step / NM)
    } else {
        format!("{step:.0} M")
    };
    text_f(s, bx + bar / 2., by - 8., &lbl, 9.5, TEXT, Anchor::Middle, true, MONO);
}

/// Rings round a point (radii in metres, labelled).
fn rings(s: &mut Svg, c: (f64, f64), k: f64, radii: &[f64], unit_nm: bool) {
    for r in radii.iter().filter(|r| **r > 0.) {
        s.circle(c.0, c.1, r * k, "none", Some((GRID, 1.)), Some("4 4"));
        let lbl = if unit_nm { format!("{:.0} NM", r / NM) } else { format!("{r:.0} M") };
        text_f(
            s,
            c.0 + r * k * 0.71 + 3.,
            c.1 - r * k * 0.71 - 3.,
            &lbl,
            9.5,
            MUTED,
            Anchor::Start,
            false,
            MONO,
        );
    }
}

/// A line chart: series of (x, y) in their own units, shared axes.
fn chart(
    s: &mut Svg,
    x: f64,
    y: f64,
    w: f64,
    h: f64,
    series: &[(&[(f64, f64)], &str)],
    xlab: &str,
    ylab: &str,
    yline: Option<(f64, &str)>,
) {
    let pts: Vec<(f64, f64)> =
        series.iter().flat_map(|(p, _)| p.iter().copied()).collect();
    if pts.len() < 2 {
        s.text(
            x + w / 2.,
            y + h / 2.,
            "no track on this record",
            12.,
            MUTED,
            Anchor::Middle,
            false,
        );
        return;
    }
    let (mut x0, mut x1, mut y0, mut y1) = (f64::MAX, f64::MIN, 0f64, f64::MIN);
    for (a, b) in &pts {
        x0 = x0.min(*a);
        x1 = x1.max(*a);
        y0 = y0.min(*b);
        y1 = y1.max(*b);
    }
    if let Some((v, _)) = yline {
        y1 = y1.max(v);
    }
    y1 = if y1 > y0 { y1 + (y1 - y0) * 0.1 } else { y0 + 1. };
    if x1 <= x0 {
        x1 = x0 + 1.;
    }
    let (l, r, t, b) = (x + 44., x + w - 12., y + 14., y + h - 26.);
    let fx = |v: f64| l + (v - x0) / (x1 - x0) * (r - l);
    let fy = |v: f64| b - (v - y0) / (y1 - y0) * (b - t);
    let ys = nice_step(y1 - y0, 4.);
    let mut g = (y0 / ys).ceil() * ys;
    while g <= y1 {
        s.line(l, fy(g), r, fy(g), GRID, 0.6, None);
        text_f(
            s,
            l - 4.,
            fy(g) + 3.,
            &format!("{g:.0}"),
            9.,
            MUTED,
            Anchor::End,
            false,
            MONO,
        );
        g += ys;
    }
    let xs = nice_step(x1 - x0, 5.);
    let mut g = (x0 / xs).ceil() * xs;
    while g <= x1 {
        s.line(fx(g), t, fx(g), b, GRID, 0.6, None);
        text_f(
            s,
            fx(g),
            b + 12.,
            &format!("{g:.0}"),
            9.,
            MUTED,
            Anchor::Middle,
            false,
            MONO,
        );
        g += xs;
    }
    if let Some((v, lbl)) = yline {
        s.line(l, fy(v), r, fy(v), RED, 1., Some("5 4"));
        s.text(r - 2., fy(v) - 4., lbl, 9.5, RED, Anchor::End, true);
    }
    for (p, c) in series {
        let q: Vec<(f64, f64)> = p.iter().map(|(a, bb)| (fx(*a), fy(*bb))).collect();
        s.polyline(&q, c, 2., None, None);
    }
    s.text(r, y + h - 4., xlab, 9.5, MUTED, Anchor::End, false);
    s.text(x + 6., t - 2., ylab, 9.5, MUTED, Anchor::Start, false);
}

/// A horizontal bar of `v` out of `of`, with its numbers.
fn bar(
    s: &mut Svg,
    x: f64,
    y: f64,
    w: f64,
    v: f64,
    of: f64,
    col: &str,
    label: &str,
    value: &str,
) {
    s.text(x, y - 6., label, 10.5, MUTED, Anchor::Start, true);
    text_f(s, x + w, y - 6., value, 11.5, TEXT, Anchor::End, true, MONO);
    s.rect(x, y, w, 14., "#ffffff", Some((TEXT, 0.8)), 0.);
    let f = if of > 0. { (v / of).clamp(0., 1.) } else { 0. };
    if f > 0. {
        s.rect(x, y, w * f, 14., col, None, 0.);
    }
}

/// Key / value lines down a panel.
fn kv(s: &mut Svg, x: f64, y: f64, w: f64, rows: &[(&str, String)]) {
    for (i, (k, v)) in rows.iter().enumerate() {
        let yy = y + i as f64 * 24.;
        s.text(x, yy, k, 11.5, MUTED, Anchor::Start, false);
        text_f(s, x + w, yy, &trunc(v, 28), 12.5, TEXT, Anchor::End, true, MONO);
        s.line(x, yy + 7., x + w, yy + 7., GRID, 0.5, None);
    }
}

/// A target or a pad, rings in metres, the set-down point marked.
fn target_diagram(
    s: &mut Svg,
    x: f64,
    y: f64,
    w: f64,
    h: f64,
    centre: &GeoPt,
    hit: &GeoPt,
    miss_m: f64,
    ring_m: &[f64],
    col: &str,
    centre_mark: &str,
) {
    let (cx, cy) = (x + w / 2., y + h / 2.);
    let ext = ring_m.iter().cloned().fold(miss_m * 1.3, f64::max).max(10.);
    let k = (w.min(h) / 2. - 26.) / ext;
    s.clip(&format!("td{}", (x + y) as i64), x, y, w, h);
    s.raw(&format!(r#"<g clip-path="url(#td{})">"#, (x + y) as i64));
    for g in 0..12 {
        let a = (g as f64 * 30.).to_radians();
        s.line(cx, cy, cx + a.sin() * w, cy - a.cos() * w, GRID, 0.5, None);
    }
    for r in ring_m {
        s.circle(cx, cy, r * k, "none", Some((TEXT, 1.2)), None);
        text_f(
            s,
            cx + r * k * 0.71 + 3.,
            cy - r * k * 0.71 - 3.,
            &format!("{r:.0} M"),
            10.,
            TEXT,
            Anchor::Start,
            true,
            MONO,
        );
    }
    if centre_mark == "H" {
        s.circle(cx, cy, 16., "#ffffff", Some((TEXT, 1.4)), None);
        s.text(cx, cy + 6., "H", 17., TEXT, Anchor::Middle, true);
    } else {
        s.rect(cx - 7., cy - 7., 14., 14., SAND, Some((TEXT, 1.2)), 0.);
    }
    if has(centre) && has(hit) {
        let (e, n) = en(ll(centre), ll(hit));
        let (ix, iy) = (cx + e * k, cy - n * k);
        s.line(cx, cy, ix, iy, col, 1.4, Some("3 3"));
        s.line(ix - 8., iy - 8., ix + 8., iy + 8., col, 3., None);
        s.line(ix + 8., iy - 8., ix - 8., iy + 8., col, 3., None);
        let brg = e.atan2(n).to_degrees().rem_euclid(360.);
        text_f(
            s,
            ix + 12.,
            iy - 10.,
            &format!("{miss_m:.1} M @ {brg:03.0}°"),
            11.5,
            col,
            Anchor::Start,
            true,
            MONO,
        );
    } else {
        s.circle(cx, cy, miss_m * k, "none", Some((col, 2.)), Some("6 4"));
        text_f(
            s,
            cx,
            cy + miss_m * k + 16.,
            &format!("{miss_m:.1} M off (bearing not recorded)"),
            11.,
            col,
            Anchor::Middle,
            true,
            MONO,
        );
    }
    s.raw("</g>");
    for (k2, l) in ["N", "E", "S", "W"].iter().enumerate() {
        let a = (k2 as f64 * 90.).to_radians();
        s.text(
            cx + a.sin() * (w.min(h) / 2. - 12.),
            cy - a.cos() * (w.min(h) / 2. - 12.) + 4.,
            l,
            11.,
            TEXT,
            Anchor::Middle,
            true,
        );
    }
}

/// A timeline: labelled spans along one bar, then the ticks.
fn timeline(
    s: &mut Svg,
    x: f64,
    y: f64,
    w: f64,
    total: f64,
    spans: &[(f64, f64, &str, &str)],
    marks: &[(f64, &str)],
) {
    let total = total.max(1.);
    let fx = |t: f64| x + (t / total).clamp(0., 1.) * w;
    s.rect(x, y, w, 26., "#ffffff", Some((TEXT, 1.)), 0.);
    for (a, b, c, l) in spans {
        s.rect(fx(*a), y, (fx(*b) - fx(*a)).max(1.), 26., c, None, 0.);
        if fx(*b) - fx(*a) > 60. {
            s.text(
                (fx(*a) + fx(*b)) / 2.,
                y + 17.,
                l,
                11.,
                "#ffffff",
                Anchor::Middle,
                true,
            );
        }
    }
    for (t, l) in marks {
        let xx = fx(*t);
        s.line(xx, y - 8., xx, y + 34., TEXT, 1.2, None);
        text_f(s, xx, y - 12., l, 10.5, TEXT, Anchor::Middle, true, MONO);
    }
    let step = nice_step(total, 6.);
    let mut g = 0.;
    while g <= total + 1e-6 {
        text_f(s, fx(g), y + 46., &mmss(g), 9.5, MUTED, Anchor::Middle, false, MONO);
        g += step;
    }
}

// ── the forms ───────────────────────────────────────────────────────────

/// The form for a result, or None for a discipline that has its own module.
pub(crate) fn sheet(rec: &RangeRecord) -> Option<String> {
    Some(match &rec.result {
        RangeResult::Strafe(r) => strafe(rec, r),
        RangeResult::Aar(r) => aar(rec, r),
        RangeResult::Missile(r) => missile(rec, r),
        RangeResult::Engagement(r) => engagement(rec, r),
        RangeResult::AntiShip(r) => anti_ship(rec, r),
        RangeResult::Sling(r) => sling(rec, r),
        RangeResult::Landing(r) => landing(rec, r),
        RangeResult::Troops(r) => troops(rec, r),
        RangeResult::Gunnery(r) => gunnery(rec, r),
        RangeResult::Cas(r) => cas(rec, r),
        RangeResult::Sead(r) => sead(rec, r),
        RangeResult::HotZone(r) => hot_zone(rec, r),
        RangeResult::LowLevel(r) => low_level(rec, r),
        RangeResult::Csar(r) => csar(rec, r),
        RangeResult::Airdrop(r) => airdrop(rec, r),
        _ => return None,
    })
}

fn pilot_row<'a>(rec: &RangeRecord) -> [(&'a str, String, &'a str, bool); 2] {
    [
        ("PILOT", rec.pilot.name.clone(), TEXT, false),
        ("A/C", discord::short_airframe(&rec.unit_type), TEXT, false),
    ]
}

// strafe

fn strafe(rec: &RangeRecord, r: &StrafeResult) -> String {
    let col = strafe_color(r.quality);
    let [p, a] = pilot_row(rec);
    let mut remarks = vec![
        sentence(&format!(
            "{} {} hits from {} rounds, {:.1}%",
            r.gun, r.hits, r.rounds_fired, r.accuracy_pct
        )),
        sentence(&format!(
            "run in on {:03.0}°, entry {:.0} ft AGL, closest {:.0} m",
            r.run_in_heading_deg.rem_euclid(360.),
            r.entry_alt_agl_m * FT,
            r.min_range_m
        )),
    ];
    if r.foul_line_crossed {
        remarks.push("Crossed the foul line: run not scored as safe.".into());
    }
    if let Some(w) = &r.invalid_reason {
        remarks.push(sentence(&format!("invalid: {w}")));
    }
    render(
        rec,
        Sheet {
            title: "STRAFE SCORE SHEET",
            sub: "AIR-TO-GROUND · GUNNERY · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 13-214 (REV 10-26)",
            row_a: vec![
                ("RANGE", r.range.clone(), TEXT, false),
                ("PIT", r.pit.clone(), TEXT, false),
                p,
                a,
                ("GUN", r.gun.clone(), TEXT, true),
            ],
            row_b: vec![
                (
                    "RUN-IN HDG",
                    format!("{:03.0}°", r.run_in_heading_deg.rem_euclid(360.)),
                    TEXT,
                    true,
                ),
                (
                    "ENTRY ALT AGL",
                    format!("{:.0} FT", r.entry_alt_agl_m * FT),
                    TEXT,
                    true,
                ),
                ("CLOSEST", format!("{:.0} M", r.min_range_m), TEXT, true),
                ("FOUL LINE AT", format!("{:.0} M", r.foul_line_m), TEXT, true),
                (
                    "FOUL LINE",
                    if r.foul_line_crossed { "CROSSED".into() } else { "CLEAR".into() },
                    if r.foul_line_crossed { RED } else { GREEN },
                    true,
                ),
            ],
            score: (
                format!("{}/{}", r.hits, r.rounds_fired),
                format!("{:.1}% HITS", r.accuracy_pct),
                col,
                r.quality.label().into(),
            ),
            detail: vec![
                ("Rounds fired", r.rounds_fired.to_string()),
                ("Hits", r.hits.to_string()),
                ("Accuracy", format!("{:.1} %", r.accuracy_pct)),
                ("Closest approach", format!("{:.0} m", r.min_range_m)),
                ("Foul line", format!("{:.0} m", r.foul_line_m)),
                (
                    "Valid",
                    if r.invalid_reason.is_some() { "NO".into() } else { "yes".into() },
                ),
            ],
            main: (
                "THE PIT · from above, run-in up the page",
                Box::new(move |s, x, y, w, h| {
                    // run-in up the page: the target near the top, the attack from below
                    let ext = (r.foul_line_m.max(r.min_range_m) * 1.5).max(600.);
                    let k = (h - 80.) / ext;
                    let (cx, ty) = (x + w / 2., y + 50.);
                    s.line(cx, ty, cx, y + h - 10., MUTED, 1., Some("8 6"));
                    s.rect(cx - 9., ty - 9., 18., 18., SAND, Some((TEXT, 1.2)), 0.);
                    s.text(cx + 16., ty + 4., "TARGET", 10.5, TEXT, Anchor::Start, true);
                    let fy = ty + r.foul_line_m * k;
                    s.line(x + 20., fy, x + w - 20., fy, RED, 2., Some("10 5"));
                    s.text(
                        x + 24.,
                        fy - 6.,
                        &format!("FOUL LINE {:.0} M", r.foul_line_m),
                        10.5,
                        RED,
                        Anchor::Start,
                        true,
                    );
                    let my = ty + r.min_range_m * k;
                    let mc = if r.foul_line_crossed { RED } else { GREEN };
                    s.circle(cx, my, 6., mc, Some(("#ffffff", 1.5)), None);
                    text_f(
                        s,
                        cx + 12.,
                        my + 4.,
                        &format!("CLOSEST {:.0} M", r.min_range_m),
                        11.,
                        mc,
                        Anchor::Start,
                        true,
                        MONO,
                    );
                    s.arrow(cx, y + h - 20., cx, my + 14., BLUE, 2.);
                    text_f(
                        s,
                        cx - 10.,
                        y + h - 26.,
                        &format!(
                            "RUN-IN {:03.0}°",
                            r.run_in_heading_deg.rem_euclid(360.)
                        ),
                        10.5,
                        BLUE,
                        Anchor::End,
                        true,
                        MONO,
                    );
                    // north, turned to the run-in
                    let a = (-r.run_in_heading_deg).to_radians();
                    let (nx, ny) = (x + w - 30., y + 32.);
                    s.circle(nx, ny, 15., "#ffffff", Some((TEXT, 0.8)), None);
                    s.arrow(
                        nx - a.sin() * 10.,
                        ny + a.cos() * 10.,
                        nx + a.sin() * 11.,
                        ny - a.cos() * 11.,
                        TEXT,
                        1.6,
                    );
                    s.text(
                        nx + a.sin() * 22.,
                        ny - a.cos() * 22. + 4.,
                        "N",
                        10.,
                        TEXT,
                        Anchor::Middle,
                        true,
                    );
                }),
            ),
            side: (
                "THE BURST",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    bar(
                        s,
                        x + 14.,
                        y + 40.,
                        w - 28.,
                        r.hits as f64,
                        r.rounds_fired.max(1) as f64,
                        col,
                        "HITS OF ROUNDS FIRED",
                        &format!("{} / {}", r.hits, r.rounds_fired),
                    );
                    bar(
                        s,
                        x + 14.,
                        y + 96.,
                        w - 28.,
                        r.accuracy_pct,
                        100.,
                        col,
                        "ACCURACY",
                        &format!("{:.1} %", r.accuracy_pct),
                    );
                    bar(
                        s,
                        x + 14.,
                        y + 152.,
                        w - 28.,
                        r.min_range_m,
                        r.foul_line_m.max(r.min_range_m).max(1.),
                        if r.foul_line_crossed { RED } else { GREEN },
                        "CLOSEST OF THE FOUL LINE",
                        &format!("{:.0} / {:.0} M", r.min_range_m, r.foul_line_m),
                    );
                }),
            ),
            remarks,
        },
    )
}

// air refuelling

fn aar(rec: &RangeRecord, r: &AarResult) -> String {
    let samples = match &rec.track {
        Some(Track::Aar { samples }) => samples.clone(),
        _ => vec![],
    };
    let col = match r.grade.as_str() {
        "A" => GREEN,
        "B" => GREEN,
        "C" => YELLOW,
        "D" => ORANGE,
        _ => RED,
    };
    let [p, a] = pilot_row(rec);
    let st = &r.stability;
    let mut remarks: Vec<String> = r.calls.iter().map(|c| sentence(c)).collect();
    remarks.insert(
        0,
        sentence(&format!(
            "{} contact{}, {} disconnect{}, {:.0} s on",
            r.contacts,
            if r.contacts == 1 { "" } else { "s" },
            r.disconnects,
            if r.disconnects == 1 { "" } else { "s" },
            r.time_connected_s
        )),
    );
    if r.overshoot {
        remarks.push("Overshot the contact position.".into());
    }
    let pts = samples.clone();
    let pts2 = samples;
    render(
        rec,
        Sheet {
            title: "AIR REFUELING RECORD",
            sub: "TANKER QUALIFICATION · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 3710/9 (REV 10-26)",
            row_a: vec![
                ("TANKER", r.tanker.clone(), TEXT, false),
                ("TYPE", r.tanker_type.clone(), TEXT, false),
                p,
                a,
                ("METHOD", format!("{:?}", r.method).to_uppercase(), TEXT, true),
            ],
            row_b: vec![
                ("ALT", format!("{:.0} FT", r.alt_ft), TEXT, true),
                ("SPEED", format!("{:.0} KT", r.speed_kts), TEXT, true),
                (
                    "JOIN",
                    r.join_time_s.map(mmss).unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
                (
                    "PRE-CONTACT CLOSURE",
                    r.precontact_closure_kts
                        .map(|c| format!("{c:.1} KT"))
                        .unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
                (
                    "OVERSHOOT",
                    if r.overshoot { "YES".into() } else { "NO".into() },
                    if r.overshoot { RED } else { TEXT },
                    true,
                ),
            ],
            score: (
                r.grade.clone(),
                format!("{:.0} LB TAKEN", r.fuel_lbs),
                col,
                format!(
                    "{} contacts · {:.0} s connected",
                    r.contacts, r.time_connected_s
                ),
            ),
            detail: vec![
                ("Contacts", r.contacts.to_string()),
                ("Disconnects", r.disconnects.to_string()),
                ("Connected", format!("{:.0} s", r.time_connected_s)),
                ("Fuel", format!("{:.0} lb", r.fuel_lbs)),
                ("Onload rate", format!("{:.0} lb/min", r.onload_rate_lbs_min)),
                ("Session", mmss(r.session_s)),
            ],
            main: (
                "IN THE TANKER'S FRAME · from above, tanker ahead",
                Box::new(move |s, x, y, w, h| {
                    frame_plot(
                        s,
                        x,
                        y,
                        w,
                        h,
                        &pts,
                        |q| (q.right_m, q.fwd_m),
                        "right of the tanker, m",
                        (st.mean_right_m, st.mean_fwd_m),
                        (st.lateral_sd_m, st.fore_aft_sd_m),
                    );
                }),
            ),
            side: (
                "SIDE · height against fore/aft",
                230.,
                Box::new(move |s, x, y, w, h| {
                    frame_plot(
                        s,
                        x,
                        y,
                        w,
                        h,
                        &pts2,
                        |q| (q.fwd_m, q.up_m),
                        "forward of the tanker, m",
                        (st.mean_fwd_m, st.mean_up_m),
                        (st.fore_aft_sd_m, st.vertical_sd_m),
                    );
                }),
            ),
            remarks,
        },
    )
}

/// The receiver's samples in the tanker frame: connected in green, the rest
/// grey, the mean and its one-sigma box.
fn frame_plot(
    s: &mut Svg,
    x: f64,
    y: f64,
    w: f64,
    h: f64,
    pts: &[bfprotocols::range::RelSample],
    f: impl Fn(&bfprotocols::range::RelSample) -> (f64, f64),
    xlab: &str,
    mean: (f64, f64),
    sd: (f64, f64),
) {
    if pts.len() < 2 {
        s.text(
            x + w / 2.,
            y + h / 2.,
            "no track on this record",
            12.,
            MUTED,
            Anchor::Middle,
            false,
        );
        return;
    }
    // the contact envelope: the mean and a good margin past one sigma (the
    // join from far astern runs off the panel, clipped)
    let (hx, hy) = ((sd.0 * 5.).max(12.), (sd.1 * 5.).max(12.));
    let (x0, x1, y0, y1) = (mean.0 - hx, mean.0 + hx, mean.1 - hy, mean.1 + hy);
    let k = ((w - 60.) / (x1 - x0)).min((h - 60.) / (y1 - y0));
    let (cx, cy) = (x + w / 2., y + h / 2.);
    let at =
        |a: f64, b: f64| (cx + (a - (x0 + x1) / 2.) * k, cy - (b - (y0 + y1) / 2.) * k);
    let id = format!("fp{}", (x + y) as i64);
    s.clip(&id, x, y, w, h);
    s.raw(&format!(r#"<g clip-path="url(#{id})">"#));
    let (zx, zy) = at(0., 0.);
    s.line(zx, y, zx, y + h, GRID, 0.8, None);
    s.line(x, zy, x + w, zy, GRID, 0.8, None);
    for win in pts.windows(2) {
        let (p0, p1) = (at(f(&win[0]).0, f(&win[0]).1), at(f(&win[1]).0, f(&win[1]).1));
        s.line(
            p0.0,
            p0.1,
            p1.0,
            p1.1,
            if win[1].connected { GREEN } else { MUTED },
            if win[1].connected { 2.2 } else { 1.2 },
            None,
        );
    }
    let (mx, my) = at(mean.0, mean.1);
    s.rect(
        mx - sd.0 * k,
        my - sd.1 * k,
        2. * sd.0 * k,
        2. * sd.1 * k,
        "none",
        Some((BLUE, 1.4)),
        0.,
    );
    s.circle(mx, my, 4., BLUE, None, None);
    s.raw("</g>");
    text_f(
        s,
        mx + sd.0 * k + 4.,
        my - sd.1 * k - 4.,
        "±1σ",
        10.,
        BLUE,
        Anchor::Start,
        true,
        MONO,
    );
    s.text(x + w - 8., y + h - 6., xlab, 9.5, MUTED, Anchor::End, false);
}

// missile defence

fn missile(rec: &RangeRecord, r: &MissileResult) -> String {
    let (mp, tp) = match &rec.track {
        Some(Track::Intercept { missile, target }) => (missile.clone(), target.clone()),
        _ => (vec![], vec![]),
    };
    let defending = r.perspective == "target";
    let good = match (r.outcome, defending) {
        (MissileOutcome::Kill | MissileOutcome::Hit, true) => false,
        (MissileOutcome::Kill | MissileOutcome::Hit, false) => true,
        (_, true) => true,
        _ => false,
    };
    let col = if good { GREEN } else { RED };
    let l = &r.launch;
    let d = &r.defense;
    let mut remarks = vec![sentence(&format!(
        "{} launched at {:.1} nm, {:.0}° aspect, {:.0} kt closure",
        r.weapon,
        l.range_m / NM,
        l.aspect_deg,
        l.closure_kts
    ))];
    if defending {
        remarks.push(sentence(&match d.reaction_s {
            Some(t) => format!(
                "reacted in {t:.1} s: beam {:.0} s, drag {:.0} s",
                d.beam_s, d.drag_s
            ),
            None => "no reaction to the launch".into(),
        }));
        if d.went_low {
            remarks.push(sentence(&format!("went low, {:+.0} ft", d.alt_change_m * FT)));
        }
    }
    remarks.push(sentence(&format!(
        "miss distance {:.0} m against a {:.0} m kill radius",
        r.min_distance_m, r.kill_radius_m
    )));
    let (mp2, tp2) = (mp.clone(), tp.clone());
    render(
        rec,
        Sheet {
            title: "MISSILE DEFENSE ENGAGEMENT RECORD",
            sub: if defending {
                "AIR-TO-AIR · DEFENDING · VECTOR STRIKE TRAINING RANGE"
            } else {
                "AIR-TO-AIR · SHOOTING · VECTOR STRIKE TRAINING RANGE"
            },
            form_no: "VSR FORM 11-2 (REV 10-26)",
            row_a: vec![
                (
                    "SHOOTER",
                    format!(
                        "{} ({})",
                        r.shooter.name,
                        discord::short_airframe(&r.shooter_type)
                    ),
                    TEXT,
                    false,
                ),
                (
                    "TARGET",
                    format!(
                        "{} ({})",
                        r.target.name,
                        discord::short_airframe(&r.target_type)
                    ),
                    TEXT,
                    false,
                ),
                ("WEAPON", r.weapon.clone(), TEXT, true),
                ("CATEGORY", r.weapon_category.to_uppercase(), TEXT, true),
                (
                    "SIDE",
                    if defending { "DEFENDING".into() } else { "SHOOTING".into() },
                    TEXT,
                    true,
                ),
            ],
            row_b: vec![
                ("LAUNCH RANGE", format!("{:.1} NM", l.range_m / NM), TEXT, true),
                ("ASPECT", format!("{:.0}°", l.aspect_deg), TEXT, true),
                (
                    "SHOOTER / TGT ALT",
                    format!(
                        "{:.0} / {:.0} FT",
                        l.shooter_alt_m * FT,
                        l.target_alt_m * FT
                    ),
                    TEXT,
                    true,
                ),
                ("CLOSURE", format!("{:.0} KT", l.closure_kts), TEXT, true),
                ("TIME OF FLIGHT", format!("{:.1} S", r.time_of_flight_s), TEXT, true),
            ],
            score: (
                r.outcome.label().to_uppercase(),
                format!("MISS {:.0} M", r.min_distance_m),
                col,
                format!("kill radius {:.0} m", r.kill_radius_m),
            ),
            detail: vec![
                (
                    "Reaction",
                    d.reaction_s
                        .map(|t| format!("{t:.1} s"))
                        .unwrap_or_else(|| "none".into()),
                ),
                ("Beam", format!("{:.0} s", d.beam_s)),
                ("Drag", format!("{:.0} s", d.drag_s)),
                ("Hot", format!("{:.0} s", d.hot_s)),
                ("Altitude change", format!("{:+.0} ft", d.alt_change_m * FT)),
                ("Went low", if d.went_low { "yes".into() } else { "no".into() }),
            ],
            main: (
                "THE ENGAGEMENT · from above, north up",
                Box::new(move |s, x, y, w, h| {
                    let o = if has(&l.target_pos) {
                        ll(&l.target_pos)
                    } else {
                        mp.first().map(ll_t).unwrap_or((0., 0.))
                    };
                    let all: Vec<(f64, f64)> = mp
                        .iter()
                        .chain(tp.iter())
                        .map(ll_t)
                        .chain(
                            [ll(&l.shooter_pos), ll(&l.target_pos)]
                                .into_iter()
                                .filter(|p| p.0 != 0.),
                        )
                        .collect();
                    if all.len() < 2 {
                        s.text(
                            x + w / 2.,
                            y + h / 2.,
                            "no track on this record",
                            12.,
                            MUTED,
                            Anchor::Middle,
                            false,
                        );
                        return;
                    }
                    let pl = Plan::fit(o, &all, x, y, w, h, 2000.);
                    let tl: Vec<(f64, f64)> = tp.iter().map(|q| pl.at(ll_t(q))).collect();
                    let ml: Vec<(f64, f64)> = mp.iter().map(|q| pl.at(ll_t(q))).collect();
                    s.polyline(&tl, BLUE, 2.2, None, None);
                    s.polyline(&ml, RED, 2.2, None, None);
                    if let Some(p0) = ml.first() {
                        s.circle(p0.0, p0.1, 5., RED, Some(("#ffffff", 1.2)), None);
                        s.text(
                            p0.0 + 9.,
                            p0.1 - 7.,
                            "LAUNCH",
                            10.,
                            RED,
                            Anchor::Start,
                            true,
                        );
                    }
                    if let (Some(pe), Some(te)) = (ml.last(), tl.last()) {
                        s.circle(
                            te.0,
                            te.1,
                            (r.kill_radius_m * pl.k).max(3.),
                            "none",
                            Some((RED, 1.)),
                            Some("4 3"),
                        );
                        s.line(pe.0, pe.1, te.0, te.1, TEXT, 1., Some("2 2"));
                        text_f(
                            s,
                            pe.0 + 10.,
                            pe.1 + 14.,
                            &format!("CPA {:.0} M", r.min_distance_m),
                            11.,
                            col,
                            Anchor::Start,
                            true,
                            MONO,
                        );
                    }
                    if let Some(t0) = tl.first() {
                        s.text(
                            t0.0 + 8.,
                            t0.1 + 14.,
                            &trunc(&r.target.name, 18),
                            10.,
                            BLUE,
                            Anchor::Start,
                            true,
                        );
                    }
                    plan_marks(s, x, y, w, h, pl.k);
                }),
            ),
            side: (
                "RANGE TO THE TARGET · against time",
                230.,
                Box::new(move |s, x, y, w, h| {
                    // missile to target, matched on time
                    let mut dist = vec![];
                    for q in &mp2 {
                        if let Some(t) = tp2.iter().min_by(|a, b| {
                            (a.t - q.t).abs().total_cmp(&(b.t - q.t).abs())
                        }) {
                            let (e, n) = en(ll_t(t), ll_t(q));
                            dist.push((
                                q.t - mp2[0].t,
                                (e * e + n * n + (q.alt_m - t.alt_m).powi(2)).sqrt(),
                            ));
                        }
                    }
                    chart(
                        s,
                        x,
                        y,
                        w,
                        h,
                        &[(&dist, RED)],
                        "s after launch",
                        "m",
                        Some((r.kill_radius_m, "kill radius")),
                    );
                }),
            ),
            remarks,
        },
    )
}

// BFM

fn engagement(rec: &RangeRecord, r: &EngagementResult) -> String {
    let paths: Vec<(String, Vec<TrackPt>)> = match &rec.track {
        Some(Track::Path { paths }) => {
            paths.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
        }
        _ => vec![],
    };
    let col = match r.outcome {
        EngagementOutcome::Win => GREEN,
        EngagementOutcome::Loss => RED,
        EngagementOutcome::Draw => YELLOW,
        EngagementOutcome::Abort => MUTED,
    };
    let me = rec.pilot.name.clone();
    let [p, a] = pilot_row(rec);
    let mut remarks: Vec<String> = r.notes.iter().map(|n| sentence(n)).collect();
    remarks.insert(
        0,
        sentence(&format!(
            "{} against {} ({}), {}",
            r.setup,
            r.adversary,
            r.adversary_skill,
            r.outcome.label()
        )),
    );
    let paths2 = paths.clone();
    let me2 = me.clone();
    render(
        rec,
        Sheet {
            title: "AIR COMBAT MANEUVERING RECORD",
            sub: "BFM · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 11-4 (REV 10-26)",
            row_a: vec![
                p,
                a,
                ("SETUP", r.setup.clone(), TEXT, false),
                ("ADVERSARY", r.adversary.clone(), TEXT, false),
                ("SKILL", r.adversary_skill.to_uppercase(), TEXT, true),
            ],
            row_b: vec![
                ("DURATION", mmss(r.duration_s), TEXT, true),
                ("SHOTS", r.shots_fired.to_string(), TEXT, true),
                ("KILLS", r.trainer_kills.to_string(), TEXT, true),
                ("GUN HITS", r.gun_hits.to_string(), TEXT, true),
                (
                    "OPPONENT",
                    r.opponent
                        .as_ref()
                        .map(|o| o.name.clone())
                        .unwrap_or_else(|| "AI".into()),
                    TEXT,
                    false,
                ),
            ],
            score: (
                r.outcome.label().to_uppercase(),
                mmss(r.duration_s),
                col,
                format!("{} kills · {} shots", r.trainer_kills, r.shots_fired),
            ),
            detail: vec![
                ("Shots fired", r.shots_fired.to_string()),
                ("Kills", r.trainer_kills.to_string()),
                ("Gun hits", r.gun_hits.to_string()),
                ("Duration", mmss(r.duration_s)),
                ("Setup", r.setup.clone()),
                ("Adversary", r.adversary.clone()),
            ],
            main: (
                "THE FIGHT · from above, north up",
                Box::new(move |s, x, y, w, h| {
                    let all: Vec<(f64, f64)> =
                        paths.iter().flat_map(|(_, v)| v.iter().map(ll_t)).collect();
                    if all.len() < 2 {
                        s.text(
                            x + w / 2.,
                            y + h / 2.,
                            "no track on this record",
                            12.,
                            MUTED,
                            Anchor::Middle,
                            false,
                        );
                        return;
                    }
                    let pl = Plan::fit(all[0], &all, x, y, w, h, 2000.);
                    for (name, v) in &paths {
                        let c = if *name == me { BLUE } else { RED };
                        let q: Vec<(f64, f64)> =
                            v.iter().map(|p| pl.at(ll_t(p))).collect();
                        s.polyline(&q, c, 2., None, None);
                        if let (Some(a0), Some(a1)) = (q.first(), q.last()) {
                            s.circle(a0.0, a0.1, 4., "#ffffff", Some((c, 1.5)), None);
                            s.circle(a1.0, a1.1, 4.5, c, None, None);
                            s.text(
                                a1.0 + 8.,
                                a1.1 - 6.,
                                &trunc(name, 18),
                                10.,
                                c,
                                Anchor::Start,
                                true,
                            );
                        }
                    }
                    plan_marks(s, x, y, w, h, pl.k);
                }),
            ),
            side: (
                "ALTITUDE · against time",
                230.,
                Box::new(move |s, x, y, w, h| {
                    let series: Vec<(Vec<(f64, f64)>, &str)> = paths2
                        .iter()
                        .map(|(n, v)| {
                            (
                                v.iter().map(|p| (p.t, p.alt_m * FT)).collect(),
                                if *n == me2 { BLUE } else { RED },
                            )
                        })
                        .collect();
                    let refs: Vec<(&[(f64, f64)], &str)> =
                        series.iter().map(|(v, c)| (v.as_slice(), *c)).collect();
                    chart(s, x, y, w, h, &refs, "s", "ft", None);
                }),
            ),
            remarks,
        },
    )
}

// anti-ship

fn anti_ship(rec: &RangeRecord, r: &AntiShipResult) -> String {
    let wpts = match &rec.track {
        Some(Track::Weapon { points }) => points.clone(),
        _ => vec![],
    };
    let col = if r.hit { GREEN } else { RED };
    let [p, a] = pilot_row(rec);
    let mut remarks = vec![sentence(&format!(
        "{} from {:.1} nm, {:.0} s of flight",
        r.weapon,
        r.launch_range_m / NM,
        r.time_of_flight_s
    ))];
    if let Some(m) = r.weapon_max_range_m {
        remarks.push(sentence(&format!(
            "launched at {:.0}% of the weapon's reach",
            r.launch_range_m / m * 100.
        )));
    }
    if r.intercepted {
        remarks.push("The ship's defences shot it down.".into());
    }
    remarks.push(sentence(&format!(
        "{}: {:.0}% damage{}",
        if r.hit { "hit" } else { "missed" },
        r.damage * 100.,
        if r.ship_sunk { ", ship sunk" } else { "" }
    )));
    let w2 = wpts.clone();
    render(
        rec,
        Sheet {
            title: "ANTI-SHIP STRIKE RECORD",
            sub: "MARITIME STRIKE · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 13-230 (REV 10-26)",
            row_a: vec![
                ("SHIP", r.ship.clone(), TEXT, false),
                ("CLASS", r.ship_type.clone(), TEXT, false),
                p,
                a,
                ("WEAPON", r.weapon.clone(), TEXT, true),
            ],
            row_b: vec![
                ("LAUNCH RANGE", format!("{:.1} NM", r.launch_range_m / NM), TEXT, true),
                (
                    "MAX RANGE",
                    r.weapon_max_range_m
                        .map(|m| format!("{:.0} NM", m / NM))
                        .unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
                ("TIME OF FLIGHT", format!("{:.0} S", r.time_of_flight_s), TEXT, true),
                (
                    "INTERCEPTED",
                    if r.intercepted { "YES".into() } else { "NO".into() },
                    if r.intercepted { RED } else { TEXT },
                    true,
                ),
                (
                    "SUNK",
                    if r.ship_sunk { "YES".into() } else { "NO".into() },
                    if r.ship_sunk { GREEN } else { TEXT },
                    true,
                ),
            ],
            score: (
                if r.hit { "HIT".into() } else { "MISS".into() },
                format!("{:.0}% DAMAGE", r.damage * 100.),
                col,
                if r.ship_sunk { "ship sunk".into() } else { "ship afloat".into() },
            ),
            detail: vec![
                ("Launch range", format!("{:.1} nm", r.launch_range_m / NM)),
                ("Time of flight", format!("{:.0} s", r.time_of_flight_s)),
                ("Damage", format!("{:.0} %", r.damage * 100.)),
                ("Intercepted", if r.intercepted { "yes".into() } else { "no".into() }),
                ("Sunk", if r.ship_sunk { "yes".into() } else { "no".into() }),
                ("Ship", r.ship_type.clone()),
            ],
            main: (
                "THE ATTACK · from above, north up",
                Box::new(move |s, x, y, w, h| {
                    if !has(&r.ship_pos) {
                        s.text(
                            x + w / 2.,
                            y + h / 2.,
                            "no positions on this record",
                            12.,
                            MUTED,
                            Anchor::Middle,
                            false,
                        );
                        return;
                    }
                    let o = ll(&r.ship_pos);
                    let mut all: Vec<(f64, f64)> = wpts.iter().map(ll_t).collect();
                    all.push(o);
                    if has(&r.launch_pos) {
                        all.push(ll(&r.launch_pos));
                    }
                    let pl = Plan::fit(o, &all, x, y, w, h, 4000.);
                    let c = pl.at(o);
                    let id = format!("as{}", (x + y) as i64);
                    s.clip(&id, x, y, w, h);
                    s.raw(&format!(r#"<g clip-path="url(#{id})">"#));
                    let step = nice_step(r.launch_range_m.max(NM), 3.).max(NM);
                    rings(s, c, pl.k, &[step, step * 2., step * 3.], true);
                    if let Some(m) = r.weapon_max_range_m {
                        s.circle(
                            c.0,
                            c.1,
                            m * pl.k,
                            "none",
                            Some((ORANGE, 1.4)),
                            Some("8 4"),
                        );
                    }
                    let q: Vec<(f64, f64)> =
                        wpts.iter().map(|p| pl.at(ll_t(p))).collect();
                    s.polyline(&q, col, 2., None, None);
                    if has(&r.launch_pos) {
                        let l = pl.at(ll(&r.launch_pos));
                        s.circle(l.0, l.1, 5., BLUE, Some(("#ffffff", 1.2)), None);
                        s.text(
                            l.0 + 9.,
                            l.1 - 7.,
                            "LAUNCH",
                            10.,
                            BLUE,
                            Anchor::Start,
                            true,
                        );
                    }
                    s.raw("</g>");
                    s.polygon(
                        &[
                            (c.0 - 14., c.1 + 4.),
                            (c.0 + 14., c.1 + 4.),
                            (c.0 + 9., c.1 + 9.),
                            (c.0 - 9., c.1 + 9.),
                        ],
                        "#5e6872",
                        Some((TEXT, 1.)),
                        None,
                    );
                    s.rect(c.0 - 4., c.1 - 3., 8., 7., "#5e6872", None, 0.);
                    s.text(
                        c.0,
                        c.1 + 24.,
                        &trunc(&r.ship, 22),
                        10.5,
                        TEXT,
                        Anchor::Middle,
                        true,
                    );
                    if r.weapon_max_range_m.is_some() {
                        s.text(
                            x + 14.,
                            y + 20.,
                            "orange: the weapon's reach",
                            10.,
                            ORANGE,
                            Anchor::Start,
                            false,
                        );
                    }
                    plan_marks(s, x, y, w, h, pl.k);
                }),
            ),
            side: (
                "THE WEAPON'S HEIGHT · against distance to go",
                230.,
                Box::new(move |s, x, y, w, h| {
                    let o = ll(&r.ship_pos);
                    let prof: Vec<(f64, f64)> = w2
                        .iter()
                        .map(|p| {
                            let (e, n) = en(o, ll_t(p));
                            ((e * e + n * n).sqrt() / NM, p.alt_m * FT)
                        })
                        .collect();
                    chart(s, x, y, w, h, &[(&prof, col)], "nm to the ship", "ft", None);
                }),
            ),
            remarks,
        },
    )
}

// sling load

fn sling(rec: &RangeRecord, r: &SlingResult) -> String {
    let col = precision_color(r.quality);
    let [p, a] = pilot_row(rec);
    let remarks = vec![
        sentence(&format!(
            "{} ({:.0} kg), {}",
            r.cargo,
            r.mass_kg,
            if r.method == "internal" { "carried inside" } else { "on the hook" }
        )),
        sentence(&format!(
            "set down {:.1} m from the drop zone after {}",
            r.distance_m,
            mmss(r.time_s)
        )),
        sentence(&format!("delivered with {:.0}% damage", r.damage * 100.)),
    ];
    render(
        rec,
        Sheet {
            title: "EXTERNAL LOAD RECORD",
            sub: "ROTARY WING · SLING LOAD · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 3710/12 (REV 10-26)",
            row_a: vec![
                ("COURSE", r.course.clone(), TEXT, false),
                p,
                a,
                ("CARGO", r.cargo.clone(), TEXT, false),
                ("METHOD", r.method.to_uppercase(), TEXT, true),
            ],
            row_b: vec![
                ("MASS", format!("{:.0} KG", r.mass_kg), TEXT, true),
                ("TIME", mmss(r.time_s), TEXT, true),
                ("OFF THE DZ", format!("{:.1} M", r.distance_m), TEXT, true),
                (
                    "DAMAGE",
                    format!("{:.0} %", r.damage * 100.),
                    if r.damage > 0.25 { RED } else { TEXT },
                    true,
                ),
            ],
            score: (
                format!("{:.1} M", r.distance_m),
                "OFF THE DROP ZONE".into(),
                col,
                r.quality.label().into(),
            ),
            detail: vec![
                ("Cargo", r.cargo.clone()),
                ("Mass", format!("{:.0} kg", r.mass_kg)),
                ("Time", mmss(r.time_s)),
                ("Damage", format!("{:.0} %", r.damage * 100.)),
                ("Method", r.method.clone()),
                ("Course", r.course.clone()),
            ],
            main: (
                "THE DROP ZONE · north up, rings in metres",
                Box::new(move |s, x, y, w, h| {
                    target_diagram(
                        s,
                        x,
                        y,
                        w,
                        h,
                        &r.dz_pos,
                        &r.set_down_pos,
                        r.distance_m,
                        &[5., 10., 20.],
                        col,
                        "box",
                    );
                }),
            ),
            side: (
                "THE LOAD",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    bar(
                        s,
                        x + 14.,
                        y + 40.,
                        w - 28.,
                        r.mass_kg,
                        (r.mass_kg * 1.25).max(1000.),
                        BLUE,
                        "MASS",
                        &format!("{:.0} KG", r.mass_kg),
                    );
                    bar(
                        s,
                        x + 14.,
                        y + 96.,
                        w - 28.,
                        r.damage,
                        1.,
                        if r.damage > 0.25 { RED } else { GREEN },
                        "DAMAGE ON DELIVERY",
                        &format!("{:.0} %", r.damage * 100.),
                    );
                    bar(
                        s,
                        x + 14.,
                        y + 152.,
                        w - 28.,
                        r.distance_m,
                        20.,
                        col,
                        "OFF THE DROP ZONE (OF 20 M)",
                        &format!("{:.1} M", r.distance_m),
                    );
                }),
            ),
            remarks,
        },
    )
}

// landing (precision or deck)

fn landing(rec: &RangeRecord, r: &LandingResult) -> String {
    let col = precision_color(r.quality);
    let ship = r.drill == "ship";
    let [p, a] = pilot_row(rec);
    let mut remarks = vec![sentence(&format!(
        "touched down {:.1} m from the pad centre at {:.0} fpm",
        r.distance_m, r.touchdown_fpm
    ))];
    if let Some(hd) = r.heading_error_deg {
        remarks.push(sentence(&format!("{hd:.0}° off the pad heading")));
    }
    if r.hover_s > 0. {
        remarks
            .push(sentence(&format!("hovered {:.0} s before setting down", r.hover_s)));
    }
    if let Some(v) = r.ship_speed_kts {
        remarks.push(sentence(&format!("the deck making {v:.0} kt")));
    }
    render(
        rec,
        Sheet {
            title: if ship {
                "DECK LANDING QUALIFICATION RECORD"
            } else {
                "PRECISION LANDING RECORD"
            },
            sub: if ship {
                "ROTARY WING · SHIPBOARD · VECTOR STRIKE TRAINING RANGE"
            } else {
                "ROTARY WING · CONFINED AREA · VECTOR STRIKE TRAINING RANGE"
            },
            form_no: "VSR FORM 3710/14 (REV 10-26)",
            row_a: vec![
                ("DRILL", r.drill.to_uppercase(), TEXT, true),
                ("PAD", r.pad.clone(), TEXT, false),
                p,
                a,
            ],
            row_b: vec![
                ("OFF CENTRE", format!("{:.1} M", r.distance_m), TEXT, true),
                ("SINK AT TD", format!("{:.0} FPM", r.touchdown_fpm), TEXT, true),
                (
                    "HEADING ERR",
                    r.heading_error_deg
                        .map(|h| format!("{h:.0}°"))
                        .unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
                ("HOVER", format!("{:.0} S", r.hover_s), TEXT, true),
                (
                    "DECK SPEED",
                    r.ship_speed_kts
                        .map(|v| format!("{v:.0} KT"))
                        .unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
            ],
            score: (
                format!("{:.1} M", r.distance_m),
                "OFF THE PAD CENTRE".into(),
                col,
                r.quality.label().into(),
            ),
            detail: vec![
                ("Off centre", format!("{:.1} m", r.distance_m)),
                ("Sink rate", format!("{:.0} fpm", r.touchdown_fpm)),
                (
                    "Heading error",
                    r.heading_error_deg
                        .map(|h| format!("{h:.0}°"))
                        .unwrap_or_else(|| "-".into()),
                ),
                ("Hover", format!("{:.0} s", r.hover_s)),
                (
                    "Deck speed",
                    r.ship_speed_kts
                        .map(|v| format!("{v:.0} kt"))
                        .unwrap_or_else(|| "-".into()),
                ),
                ("Pad", r.pad.clone()),
            ],
            main: (
                "THE PAD · north up, rings in metres",
                Box::new(move |s, x, y, w, h| {
                    target_diagram(
                        s,
                        x,
                        y,
                        w,
                        h,
                        &r.pad_pos,
                        &r.touchdown_pos,
                        r.distance_m,
                        &[2., 5., 10.],
                        col,
                        "H",
                    );
                }),
            ),
            side: (
                "THE TOUCHDOWN",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    bar(
                        s,
                        x + 14.,
                        y + 40.,
                        w - 28.,
                        r.touchdown_fpm,
                        600.,
                        if r.touchdown_fpm > 400. { RED } else { col },
                        "SINK RATE (SCALE 600 FPM)",
                        &format!("{:.0} FPM", r.touchdown_fpm),
                    );
                    bar(
                        s,
                        x + 14.,
                        y + 96.,
                        w - 28.,
                        r.distance_m,
                        10.,
                        col,
                        "OFF CENTRE (OF 10 M)",
                        &format!("{:.1} M", r.distance_m),
                    );
                    bar(
                        s,
                        x + 14.,
                        y + 152.,
                        w - 28.,
                        r.heading_error_deg.unwrap_or(0.).abs(),
                        45.,
                        MUTED,
                        "HEADING ERROR (OF 45°)",
                        &r.heading_error_deg
                            .map(|h| format!("{h:.0}°"))
                            .unwrap_or_else(|| "-".into()),
                    );
                }),
            ),
            remarks,
        },
    )
}

// troops

fn troops(rec: &RangeRecord, r: &TroopResult) -> String {
    let col = precision_color(r.quality);
    let [p, a] = pilot_row(rec);
    let remarks = vec![
        sentence(&format!("{} troops, loaded in {}", r.troops, mmss(r.load_time_s))),
        sentence(&format!(
            "on the ground at {} {:.0} m from its centre, {} after the call",
            r.lz,
            r.landing_distance_m,
            mmss(r.total_time_s)
        )),
    ];
    render(
        rec,
        Sheet {
            title: "TROOP INSERTION RECORD",
            sub: "ROTARY WING · AIR ASSAULT · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 3710/15 (REV 10-26)",
            row_a: vec![
                ("LZ", r.lz.clone(), TEXT, false),
                p,
                a,
                ("TROOPS", r.troops.to_string(), TEXT, true),
            ],
            row_b: vec![
                ("LOAD", mmss(r.load_time_s), TEXT, true),
                ("TOTAL", mmss(r.total_time_s), TEXT, true),
                ("OFF THE LZ", format!("{:.0} M", r.landing_distance_m), TEXT, true),
            ],
            score: (
                mmss(r.total_time_s),
                "CALL TO TROOPS ON THE GROUND".into(),
                col,
                r.quality.label().into(),
            ),
            detail: vec![
                ("Troops", r.troops.to_string()),
                ("Load time", mmss(r.load_time_s)),
                ("Transit and unload", mmss((r.total_time_s - r.load_time_s).max(0.))),
                ("Total", mmss(r.total_time_s)),
                ("Off the LZ", format!("{:.0} m", r.landing_distance_m)),
            ],
            main: (
                "THE INSERTION · against the clock",
                Box::new(move |s, x, y, w, _h| {
                    timeline(
                        s,
                        x + 30.,
                        y + 90.,
                        w - 60.,
                        r.total_time_s,
                        &[
                            (0., r.load_time_s, BLUE, "LOAD"),
                            (r.load_time_s, r.total_time_s, GREEN, "TRANSIT AND UNLOAD"),
                        ],
                        &[(r.load_time_s, "LOADED"), (r.total_time_s, "ON THE LZ")],
                    );
                    // the stick, one figure a troop
                    let n = r.troops.min(24) as usize;
                    let per = 12usize;
                    for i in 0..n {
                        let (cx, cy) = (
                            x + 50. + (i % per) as f64 * 44.,
                            y + 230. + (i / per) as f64 * 70.,
                        );
                        s.circle(cx, cy, 6., "none", Some((TEXT, 1.6)), None);
                        s.line(cx, cy + 6., cx, cy + 26., TEXT, 1.6, None);
                        s.line(cx, cy + 26., cx - 7., cy + 42., TEXT, 1.6, None);
                        s.line(cx, cy + 26., cx + 7., cy + 42., TEXT, 1.6, None);
                        s.line(cx - 9., cy + 14., cx + 9., cy + 14., GREEN, 1.6, None);
                    }
                    s.text(
                        x + 30.,
                        y + 210.,
                        &format!("{} TROOPS", r.troops),
                        11.,
                        MUTED,
                        Anchor::Start,
                        true,
                    );
                }),
            ),
            side: (
                "AT THE LZ",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    let (cx, cy) = (x + w / 2., y + 104.);
                    for rr in [25., 50., 75.] {
                        s.circle(cx, cy, rr, "none", Some((GRID, 1.)), Some("4 4"));
                    }
                    s.circle(cx, cy, 6., SAND, Some((TEXT, 1.)), None);
                    let k = 75. / r.landing_distance_m.max(100.);
                    s.circle(
                        cx,
                        cy,
                        (r.landing_distance_m * k).max(3.),
                        "none",
                        Some((col, 2.)),
                        None,
                    );
                    text_f(
                        s,
                        cx,
                        y + 196.,
                        &format!(
                            "{:.0} M OFF (bearing not recorded)",
                            r.landing_distance_m
                        ),
                        10.5,
                        col,
                        Anchor::Middle,
                        true,
                        MONO,
                    );
                }),
            ),
            remarks,
        },
    )
}

// gunnery

fn gunnery(rec: &RangeRecord, r: &GunneryResult) -> String {
    let frac = if r.targets_total > 0 {
        r.targets_killed as f64 / r.targets_total as f64
    } else {
        0.
    };
    let col = if frac >= 0.9 {
        GREEN
    } else if frac >= 0.6 {
        YELLOW
    } else {
        RED
    };
    let [p, a] = pilot_row(rec);
    let acc = if r.shots > 0 { r.hits as f64 / r.shots as f64 * 100. } else { 0. };
    let remarks = vec![
        sentence(&format!(
            "{} of {} targets in {}",
            r.targets_killed,
            r.targets_total,
            mmss(r.time_s)
        )),
        sentence(&format!(
            "{} hits from {} shots, {:.0}%; {} first-round hits",
            r.hits, r.shots, acc, r.first_round_hits
        )),
    ];
    render(
        rec,
        Sheet {
            title: "GUNNERY LANE SCORE SHEET",
            sub: "GROUND GUNNERY · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 13-240 (REV 10-26)",
            row_a: vec![
                ("LANE", r.lane.clone(), TEXT, false),
                p,
                a,
                ("TARGETS", r.targets_total.to_string(), TEXT, true),
            ],
            row_b: vec![
                ("KILLED", r.targets_killed.to_string(), TEXT, true),
                ("SHOTS", r.shots.to_string(), TEXT, true),
                ("HITS", r.hits.to_string(), TEXT, true),
                ("FIRST-ROUND HITS", r.first_round_hits.to_string(), TEXT, true),
                ("TIME", mmss(r.time_s), TEXT, true),
            ],
            score: (
                format!("{}/{}", r.targets_killed, r.targets_total),
                "TARGETS KILLED".into(),
                col,
                format!("{acc:.0}% hits · {}", mmss(r.time_s)),
            ),
            detail: vec![
                ("Shots", r.shots.to_string()),
                ("Hits", r.hits.to_string()),
                ("Accuracy", format!("{acc:.0} %")),
                ("First-round hits", r.first_round_hits.to_string()),
                ("Time", mmss(r.time_s)),
                (
                    "Kills a minute",
                    format!("{:.1}", r.targets_killed as f64 / (r.time_s / 60.).max(0.1)),
                ),
            ],
            main: (
                "THE LANE · every target, killed or standing",
                Box::new(move |s, x, y, _w, _h| {
                    let n = r.targets_total.min(30) as usize;
                    let per = 6usize;
                    for i in 0..n {
                        let (cx, by) = (
                            x + 70. + (i % per) as f64 * 96.,
                            y + 120. + (i / per) as f64 * 100.,
                        );
                        let dead = (i as u32) < r.targets_killed;
                        s.line(cx, by, cx, by - 22., TEXT, 1.6, None);
                        s.line(cx - 14., by, cx + 14., by, TEXT, 1.6, None);
                        s.rect(
                            cx - 13.,
                            by - 60.,
                            26.,
                            38.,
                            if dead { "#e9d6d3" } else { "#ffffff" },
                            Some((TEXT, 1.4)),
                            6.,
                        );
                        if dead {
                            s.line(cx - 16., by - 64., cx + 16., by - 18., RED, 3., None);
                            s.line(cx + 16., by - 64., cx - 16., by - 18., RED, 3., None);
                        }
                        text_f(
                            s,
                            cx,
                            by + 16.,
                            &format!("T{}", i + 1),
                            10.,
                            MUTED,
                            Anchor::Middle,
                            true,
                            MONO,
                        );
                    }
                }),
            ),
            side: (
                "RATES",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    bar(
                        s,
                        x + 14.,
                        y + 40.,
                        w - 28.,
                        r.targets_killed as f64,
                        r.targets_total.max(1) as f64,
                        col,
                        "TARGETS KILLED",
                        &format!("{} / {}", r.targets_killed, r.targets_total),
                    );
                    bar(
                        s,
                        x + 14.,
                        y + 96.,
                        w - 28.,
                        r.hits as f64,
                        r.shots.max(1) as f64,
                        BLUE,
                        "HITS OF SHOTS",
                        &format!("{} / {}", r.hits, r.shots),
                    );
                    bar(
                        s,
                        x + 14.,
                        y + 152.,
                        w - 28.,
                        r.first_round_hits as f64,
                        r.targets_total.max(1) as f64,
                        GREEN,
                        "FIRST-ROUND HITS OF TARGETS",
                        &format!("{} / {}", r.first_round_hits, r.targets_total),
                    );
                }),
            ),
            remarks,
        },
    )
}

// CAS

fn cas(rec: &RangeRecord, r: &CasResult) -> String {
    let col = if !r.correct_target {
        RED
    } else if r.danger_close {
        ORANGE
    } else {
        GREEN
    };
    let [p, a] = pilot_row(rec);
    let mut remarks = vec![sentence(&format!(
        "{} on {}, {:.0} s from the call to impact",
        r.weapon, r.target, r.time_to_impact_s
    ))];
    remarks.push(if r.correct_target {
        sentence(&format!("correct target, {:.0} m miss", r.miss_m))
    } else {
        "Wrong target.".into()
    });
    if r.danger_close {
        remarks.push(sentence(&match r.nearest_friendly_m {
            Some(d) => format!("danger close: friendlies {d:.0} m from the impact"),
            None => "danger close".into(),
        }));
    }
    render(
        rec,
        Sheet {
            title: "CLOSE AIR SUPPORT RECORD",
            sub: "JTAC-CONTROLLED ATTACK · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 13-250 (REV 10-26)",
            row_a: vec![
                ("JTAC", r.jtac.clone(), TEXT, false),
                ("TARGET", r.target.clone(), TEXT, false),
                p,
                a,
                ("WEAPON", r.weapon.clone(), TEXT, true),
            ],
            row_b: vec![
                (
                    "CORRECT TGT",
                    if r.correct_target { "YES".into() } else { "NO".into() },
                    if r.correct_target { GREEN } else { RED },
                    true,
                ),
                ("MISS", format!("{:.0} M", r.miss_m), TEXT, true),
                ("CALL TO IMPACT", format!("{:.0} S", r.time_to_impact_s), TEXT, true),
                (
                    "DANGER CLOSE",
                    if r.danger_close { "YES".into() } else { "NO".into() },
                    if r.danger_close { ORANGE } else { TEXT },
                    true,
                ),
                (
                    "LASER CODE",
                    r.laser_code.map(|c| c.to_string()).unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
            ],
            score: (
                format!("{:.0} M", r.miss_m),
                if r.correct_target {
                    "CORRECT TARGET".into()
                } else {
                    "WRONG TARGET".into()
                },
                col,
                if r.danger_close {
                    "danger close".into()
                } else {
                    "clear of friendlies".into()
                },
            ),
            detail: vec![
                ("JTAC", r.jtac.clone()),
                ("Target", r.target.clone()),
                ("Weapon", r.weapon.clone()),
                ("Call to impact", format!("{:.0} s", r.time_to_impact_s)),
                (
                    "Nearest friendly",
                    r.nearest_friendly_m
                        .map(|d| format!("{d:.0} m"))
                        .unwrap_or_else(|| "-".into()),
                ),
                (
                    "Laser code",
                    r.laser_code.map(|c| c.to_string()).unwrap_or_else(|| "-".into()),
                ),
            ],
            main: (
                "DISTANCES FROM THE TARGET",
                Box::new(move |s, x, y, w, _h| {
                    let far =
                        r.nearest_friendly_m.unwrap_or(0.).max(r.miss_m).max(50.) * 1.2;
                    let (l, rr, ly) = (x + 40., x + w - 40., y + 200.);
                    let fx = |d: f64| l + d / far * (rr - l);
                    s.line(l, ly, rr, ly, TEXT, 1.6, None);
                    let step = nice_step(far, 6.);
                    let mut g = 0.;
                    while g <= far {
                        s.line(fx(g), ly - 6., fx(g), ly + 6., TEXT, 1., None);
                        text_f(
                            s,
                            fx(g),
                            ly + 22.,
                            &format!("{g:.0}"),
                            10.,
                            MUTED,
                            Anchor::Middle,
                            false,
                            MONO,
                        );
                        g += step;
                    }
                    s.rect(fx(0.) - 8., ly - 30., 16., 16., SAND, Some((TEXT, 1.2)), 0.);
                    s.text(fx(0.), ly - 38., "TARGET", 10.5, TEXT, Anchor::Middle, true);
                    let ix = fx(r.miss_m);
                    s.line(ix - 8., ly - 30., ix + 8., ly - 14., col, 3., None);
                    s.line(ix + 8., ly - 30., ix - 8., ly - 14., col, 3., None);
                    // above the target's own label when the miss is too small to separate them
                    let iy = if ix - fx(0.) < 80. { ly - 62. } else { ly - 46. };
                    text_f(
                        s,
                        ix.max(fx(0.) + 30.),
                        iy,
                        &format!("IMPACT {:.0} M", r.miss_m),
                        11.,
                        col,
                        Anchor::Middle,
                        true,
                        MONO,
                    );
                    if let Some(d) = r.nearest_friendly_m {
                        let fxx = fx(d);
                        s.circle(fxx, ly - 22., 8., BLUE, Some(("#ffffff", 1.5)), None);
                        text_f(
                            s,
                            fxx,
                            ly + 46.,
                            &format!("FRIENDLIES {d:.0} M FROM THE IMPACT"),
                            11.,
                            BLUE,
                            Anchor::Middle,
                            true,
                            MONO,
                        );
                    }
                    s.text(
                        rr,
                        ly + 70.,
                        "metres along the ground (bearings not recorded)",
                        10.,
                        MUTED,
                        Anchor::End,
                        false,
                    );
                }),
            ),
            side: (
                "THE TASKING",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    kv(
                        s,
                        x + 14.,
                        y + 34.,
                        w - 28.,
                        &[
                            ("JTAC", r.jtac.clone()),
                            ("Target", r.target.clone()),
                            ("Weapon", r.weapon.clone()),
                            (
                                "Laser code",
                                r.laser_code
                                    .map(|c| c.to_string())
                                    .unwrap_or_else(|| "-".into()),
                            ),
                            ("Call to impact", format!("{:.0} s", r.time_to_impact_s)),
                            (
                                "Correct target",
                                if r.correct_target { "yes".into() } else { "NO".into() },
                            ),
                        ],
                    );
                }),
            ),
            remarks,
        },
    )
}

// SEAD

fn sead(rec: &RangeRecord, r: &SeadResult) -> String {
    let col = if r.site_destroyed {
        GREEN
    } else if r.site_was_emitting {
        GREEN
    } else {
        YELLOW
    };
    let [p, a] = pilot_row(rec);
    let mut remarks = vec![sentence(&format!(
        "{} on the {} of {} ({})",
        r.weapon, r.role, r.site, r.system
    ))];
    remarks.push(if r.site_was_emitting {
        "The radar was up when you fired.".into()
    } else {
        "The radar was dark when you fired.".into()
    });
    if r.site_destroyed {
        remarks.push("Every radar at the site is down.".into());
    }
    if r.shots_at_you > 0 {
        remarks.push(sentence(&format!(
            "{} SAM{} at you, {} would have killed you",
            r.shots_at_you,
            if r.shots_at_you == 1 { "" } else { "s" },
            r.trainer_deaths
        )));
    }
    render(
        rec,
        Sheet {
            title: "SEAD / DEAD MISSION RECORD",
            sub: "SUPPRESSION OF ENEMY AIR DEFENCES · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 13-260 (REV 10-26)",
            row_a: vec![
                ("NETWORK", r.network.clone(), TEXT, false),
                ("SITE", r.site.clone(), TEXT, false),
                ("SYSTEM", r.system.clone(), TEXT, false),
                p,
                a,
            ],
            row_b: vec![
                ("KILLED", format!("{} ({})", r.role, r.unit_type), TEXT, false),
                ("WEAPON", r.weapon.clone(), TEXT, true),
                ("GUIDANCE", r.guidance.to_uppercase(), TEXT, true),
                (
                    "LAUNCH RANGE",
                    r.launch_range_m
                        .map(|m| format!("{:.1} NM", m / NM))
                        .unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
                (
                    "RADAR AT LAUNCH",
                    if r.site_was_emitting { "UP".into() } else { "DARK".into() },
                    if r.site_was_emitting { GREEN } else { YELLOW },
                    true,
                ),
            ],
            score: (
                if r.site_destroyed { "DESTROYED".into() } else { "KILL".into() },
                format!("{} · {}", r.role.to_uppercase(), r.system),
                col,
                if r.site_destroyed {
                    "site destroyed".into()
                } else {
                    "radars still alive".into()
                },
            ),
            detail: vec![
                ("Site", r.site.clone()),
                ("Killed", r.unit_type.clone()),
                (
                    "Launch range",
                    r.launch_range_m
                        .map(|m| format!("{:.1} nm", m / NM))
                        .unwrap_or_else(|| "-".into()),
                ),
                (
                    "Radar at launch",
                    if r.site_was_emitting { "up".into() } else { "dark".into() },
                ),
                ("SAMs at you", r.shots_at_you.to_string()),
                ("Would have died", r.trainer_deaths.to_string()),
            ],
            main: (
                "THE SHOT · from above, north up",
                Box::new(move |s, x, y, w, h| {
                    if !has(&r.site_pos) {
                        s.text(
                            x + w / 2.,
                            y + h / 2.,
                            "no positions on this record",
                            12.,
                            MUTED,
                            Anchor::Middle,
                            false,
                        );
                        return;
                    }
                    let o = ll(&r.site_pos);
                    let mut all = vec![o];
                    if let Some(l) = &r.launch_pos {
                        all.push(ll(l));
                    }
                    let pl = Plan::fit(o, &all, x, y, w, h, 10. * NM);
                    let c = pl.at(o);
                    let id = format!("sd{}", (x + y) as i64);
                    s.clip(&id, x, y, w, h);
                    s.raw(&format!(r#"<g clip-path="url(#{id})">"#));
                    rings(s, c, pl.k, &[5. * NM, 10. * NM, 15. * NM, 20. * NM], true);
                    if let Some(l) = &r.launch_pos {
                        let lp = pl.at(ll(l));
                        s.line(lp.0, lp.1, c.0, c.1, col, 2., Some("8 5"));
                        s.circle(lp.0, lp.1, 5., BLUE, Some(("#ffffff", 1.2)), None);
                        s.text(
                            lp.0 + 9.,
                            lp.1 - 7.,
                            "LAUNCH",
                            10.,
                            BLUE,
                            Anchor::Start,
                            true,
                        );
                    }
                    s.raw("</g>");
                    s.circle(
                        c.0,
                        c.1,
                        9.,
                        if r.site_was_emitting { RED } else { "#ffffff" },
                        Some((TEXT, 1.4)),
                        None,
                    );
                    if r.site_destroyed {
                        s.line(
                            c.0 - 12.,
                            c.1 - 12.,
                            c.0 + 12.,
                            c.1 + 12.,
                            TEXT,
                            3.,
                            None,
                        );
                        s.line(
                            c.0 + 12.,
                            c.1 - 12.,
                            c.0 - 12.,
                            c.1 + 12.,
                            TEXT,
                            3.,
                            None,
                        );
                    }
                    s.text(
                        c.0,
                        c.1 + 26.,
                        &trunc(&r.site, 24),
                        10.5,
                        TEXT,
                        Anchor::Middle,
                        true,
                    );
                    s.text(
                        x + 14.,
                        y + 20.,
                        if r.site_was_emitting {
                            "red: radar emitting at launch"
                        } else {
                            "white: radar dark at launch"
                        },
                        10.,
                        MUTED,
                        Anchor::Start,
                        false,
                    );
                    plan_marks(s, x, y, w, h, pl.k);
                }),
            ),
            side: (
                "THE THREAT",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    s.text(
                        x + 14.,
                        y + 30.,
                        "SAMS AT YOU",
                        10.5,
                        MUTED,
                        Anchor::Start,
                        true,
                    );
                    for i in 0..r.shots_at_you.min(12) {
                        let cx = x + 24. + i as f64 * 30.;
                        let dead = i < r.trainer_deaths;
                        s.polygon(
                            &[(cx, y + 44.), (cx + 7., y + 80.), (cx - 7., y + 80.)],
                            if dead { RED } else { "#ffffff" },
                            Some((TEXT, 1.2)),
                            None,
                        );
                    }
                    if r.shots_at_you == 0 {
                        text_f(
                            s,
                            x + 14.,
                            y + 64.,
                            "NONE",
                            13.,
                            GREEN,
                            Anchor::Start,
                            true,
                            MONO,
                        );
                    }
                    kv(
                        s,
                        x + 14.,
                        y + 120.,
                        w - 28.,
                        &[
                            ("Would have killed you", r.trainer_deaths.to_string()),
                            ("Guidance", r.guidance.to_uppercase()),
                            ("Weapon", r.weapon.clone()),
                        ],
                    );
                }),
            ),
            remarks,
        },
    )
}

// hot zone

fn hot_zone(rec: &RangeRecord, r: &HotZoneResult) -> String {
    let col = match r.outcome {
        HotZoneOutcome::ShotDown => RED,
        _ if r.trainer_deaths > 0 => ORANGE,
        HotZoneOutcome::Left => MUTED,
        _ if r.air_kills + r.ground_kills > 0 => GREEN,
        _ => YELLOW,
    };
    let [p, a] = pilot_row(rec);
    let mut remarks = vec![sentence(&format!(
        "{} in the zone, {}",
        mmss(r.time_in_zone_s),
        r.outcome.label()
    ))];
    remarks.push(sentence(&format!(
        "{} air and {} ground kills from {} shots",
        r.air_kills, r.ground_kills, r.shots_fired
    )));
    if r.missiles_defeated > 0 {
        remarks.push(sentence(&format!(
            "defeated {} missile{}",
            r.missiles_defeated,
            if r.missiles_defeated == 1 { "" } else { "s" }
        )));
    }
    if r.trainer_deaths > 0 {
        remarks.push(sentence(&format!(
            "would have died {} time{}",
            r.trainer_deaths,
            if r.trainer_deaths == 1 { "" } else { "s" }
        )));
    }
    let kills = r.kills.clone();
    render(
        rec,
        Sheet {
            title: "HOT ZONE SORTIE RECORD",
            sub: "LIVE OPPOSITION · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 11-6 (REV 10-26)",
            row_a: vec![
                ("ZONE", r.zone.clone(), TEXT, false),
                p,
                a,
                ("OUTCOME", r.outcome.label().to_uppercase(), col, false),
            ],
            row_b: vec![
                ("TIME IN ZONE", mmss(r.time_in_zone_s), TEXT, true),
                ("AIR KILLS", r.air_kills.to_string(), TEXT, true),
                ("GROUND KILLS", r.ground_kills.to_string(), TEXT, true),
                ("SHOTS", r.shots_fired.to_string(), TEXT, true),
                ("MSL DEFEATED", r.missiles_defeated.to_string(), TEXT, true),
            ],
            score: (
                format!("{}", r.air_kills + r.ground_kills),
                "KILLS".into(),
                col,
                r.outcome.label().into(),
            ),
            detail: vec![
                ("Air kills", r.air_kills.to_string()),
                ("Ground kills", r.ground_kills.to_string()),
                ("Shots fired", r.shots_fired.to_string()),
                ("Missiles defeated", r.missiles_defeated.to_string()),
                ("Trainer deaths", r.trainer_deaths.to_string()),
                ("Time in zone", mmss(r.time_in_zone_s)),
            ],
            main: (
                "THE TALLY",
                Box::new(move |s, x, y, w, _h| {
                    let boxes = [
                        ("AIR", r.air_kills, BLUE),
                        ("GROUND", r.ground_kills, SAND),
                        ("MISSILES DEFEATED", r.missiles_defeated, GREEN),
                        ("DEATHS", r.trainer_deaths, RED),
                    ];
                    let bw = (w - 50.) / 4.;
                    for (i, (l, v, c)) in boxes.iter().enumerate() {
                        let bx = x + 20. + i as f64 * (bw + 3.);
                        s.rect(bx, y + 24., bw, 90., "#ffffff", Some((TEXT, 1.)), 0.);
                        text_f(
                            s,
                            bx + bw / 2.,
                            y + 80.,
                            &v.to_string(),
                            36.,
                            c,
                            Anchor::Middle,
                            true,
                            MONO,
                        );
                        s.text(
                            bx + bw / 2.,
                            y + 106.,
                            l,
                            9.5,
                            MUTED,
                            Anchor::Middle,
                            true,
                        );
                    }
                    s.text(x + 20., y + 150., "KILLS", 10.5, MUTED, Anchor::Start, true);
                    for (i, k) in kills.iter().take(14).enumerate() {
                        text_f(
                            s,
                            x + 20. + (i / 7) as f64 * (w / 2.),
                            y + 176. + (i % 7) as f64 * 22.,
                            &format!("{:>2}. {}", i + 1, trunc(k, 30)),
                            12.,
                            TEXT,
                            Anchor::Start,
                            false,
                            MONO,
                        );
                    }
                    if kills.is_empty() {
                        s.text(
                            x + 20.,
                            y + 176.,
                            "none",
                            12.,
                            MUTED,
                            Anchor::Start,
                            false,
                        );
                    }
                }),
            ),
            side: (
                "IN THE ZONE",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    bar(
                        s,
                        x + 14.,
                        y + 40.,
                        w - 28.,
                        r.time_in_zone_s,
                        r.time_in_zone_s.max(600.),
                        col,
                        "TIME IN THE ZONE",
                        &mmss(r.time_in_zone_s),
                    );
                    bar(
                        s,
                        x + 14.,
                        y + 96.,
                        w - 28.,
                        (r.air_kills + r.ground_kills) as f64,
                        r.shots_fired.max(1) as f64,
                        GREEN,
                        "KILLS OF SHOTS",
                        &format!("{} / {}", r.air_kills + r.ground_kills, r.shots_fired),
                    );
                    kv(
                        s,
                        x + 14.,
                        y + 150.,
                        w - 28.,
                        &[("Outcome", r.outcome.label().into())],
                    );
                }),
            ),
            remarks,
        },
    )
}

// low level

fn low_level(rec: &RangeRecord, r: &LowLevelResult) -> String {
    let col = precision_color(r.quality);
    let path: Vec<TrackPt> = match &rec.track {
        Some(Track::Path { paths }) => paths.values().next().cloned().unwrap_or_default(),
        _ => vec![],
    };
    let [p, a] = pilot_row(rec);
    let when = if r.tot_error_s > 0.5 {
        "late"
    } else if r.tot_error_s < -0.5 {
        "early"
    } else {
        "on time"
    };
    let mut remarks = vec![
        sentence(&format!(
            "{} of {} gates, {} against {} planned",
            r.gates_hit,
            r.gates_total,
            mmss(r.time_s),
            mmss(r.planned_s)
        )),
        sentence(&format!("on target {:+.0} s, {when}", r.tot_error_s)),
        sentence(&format!(
            "{:.0}% under the {:.0} ft ceiling, {:.0} ft average",
            r.pct_below_ceiling, r.max_allowed_agl_ft, r.avg_agl_ft
        )),
    ];
    if r.below_floor_s > 0. {
        remarks.push(sentence(&format!(
            "{:.0} s under the {:.0} ft floor",
            r.below_floor_s, r.min_allowed_agl_ft
        )));
    }
    remarks.extend(r.calls.iter().map(|c| sentence(c)));
    let gates = r.gates.clone();
    let gates2 = r.gates.clone();
    render(
        rec,
        Sheet {
            title: "LOW-LEVEL ROUTE RECORD",
            sub: "LOW-LEVEL NAVIGATION · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 11-202 (REV 10-26)",
            row_a: vec![
                ("ROUTE", r.route.clone(), TEXT, false),
                p,
                a,
                ("GATES", format!("{} / {}", r.gates_hit, r.gates_total), TEXT, true),
            ],
            row_b: vec![
                (
                    "TIME / PLAN",
                    format!("{} / {}", mmss(r.time_s), mmss(r.planned_s)),
                    TEXT,
                    true,
                ),
                ("TIME ON TARGET", format!("{:+.0} S", r.tot_error_s), TEXT, true),
                ("AGL AVG", format!("{:.0} FT", r.avg_agl_ft), TEXT, true),
                (
                    "AGL MIN / MAX",
                    format!("{:.0} / {:.0} FT", r.min_agl_ft, r.max_agl_ft),
                    TEXT,
                    true,
                ),
                ("SPEED", format!("{:.0} KT", r.avg_speed_kts), TEXT, true),
            ],
            score: (
                format!("{}/{}", r.gates_hit, r.gates_total),
                "GATES".into(),
                col,
                format!("{} · TOT {:+.0} s", r.quality.label(), r.tot_error_s),
            ),
            detail: vec![
                ("Under the ceiling", format!("{:.0} %", r.pct_below_ceiling)),
                ("Ceiling", format!("{:.0} ft AGL", r.max_allowed_agl_ft)),
                ("Floor", format!("{:.0} ft AGL", r.min_allowed_agl_ft)),
                ("Below the floor", format!("{:.0} s", r.below_floor_s)),
                ("Average speed", format!("{:.0} kt", r.avg_speed_kts)),
                ("Time on target", format!("{:+.0} s", r.tot_error_s)),
            ],
            main: (
                "THE ROUTE · from above, north up",
                Box::new(move |s, x, y, w, h| {
                    let all: Vec<(f64, f64)> = path
                        .iter()
                        .map(ll_t)
                        .chain(gates.iter().filter(|g| has(&g.pos)).map(|g| ll(&g.pos)))
                        .collect();
                    if all.len() < 2 {
                        s.text(
                            x + w / 2.,
                            y + h / 2.,
                            "no track on this record",
                            12.,
                            MUTED,
                            Anchor::Middle,
                            false,
                        );
                        return;
                    }
                    let pl = Plan::fit(all[0], &all, x, y, w, h, 4000.);
                    let q: Vec<(f64, f64)> =
                        path.iter().map(|p| pl.at(ll_t(p))).collect();
                    s.polyline(&q, BLUE, 2., None, None);
                    for g in gates.iter().filter(|g| has(&g.pos)) {
                        let c = pl.at(ll(&g.pos));
                        let ok = g.t.is_some();
                        s.rect(
                            c.0 - 6.,
                            c.1 - 6.,
                            12.,
                            12.,
                            if ok { GREEN } else { "#ffffff" },
                            Some((if ok { TEXT } else { RED }, 1.4)),
                            0.,
                        );
                        s.text(
                            c.0 + 10.,
                            c.1 - 8.,
                            &trunc(&g.gate, 8),
                            10.,
                            if ok { TEXT } else { RED },
                            Anchor::Start,
                            true,
                        );
                    }
                    plan_marks(s, x, y, w, h, pl.k);
                }),
            ),
            side: (
                "HEIGHT AT EVERY GATE · ft AGL",
                230.,
                Box::new(move |s, x, y, w, h| {
                    let n = gates2.len().max(1);
                    let top = r
                        .max_allowed_agl_ft
                        .max(gates2.iter().filter_map(|g| g.agl_ft).fold(0., f64::max))
                        * 1.2;
                    let (l, rr, t, b) = (x + 40., x + w - 10., y + 20., y + h - 30.);
                    let fy = |v: f64| b - v / top.max(1.) * (b - t);
                    s.line(
                        l,
                        fy(r.max_allowed_agl_ft),
                        rr,
                        fy(r.max_allowed_agl_ft),
                        RED,
                        1.,
                        Some("5 4"),
                    );
                    text_f(
                        s,
                        l - 4.,
                        fy(r.max_allowed_agl_ft) + 3.,
                        &format!("{:.0}", r.max_allowed_agl_ft),
                        9.,
                        RED,
                        Anchor::End,
                        false,
                        MONO,
                    );
                    if r.min_allowed_agl_ft > 0. {
                        s.line(
                            l,
                            fy(r.min_allowed_agl_ft),
                            rr,
                            fy(r.min_allowed_agl_ft),
                            ORANGE,
                            1.,
                            Some("5 4"),
                        );
                    }
                    s.line(l, b, rr, b, TEXT, 1., None);
                    let bw = ((rr - l) / n as f64 * 0.6).min(28.);
                    for (i, g) in gates2.iter().enumerate() {
                        let cx = l + (i as f64 + 0.5) * (rr - l) / n as f64;
                        if let Some(v) = g.agl_ft {
                            let c =
                                if v > r.max_allowed_agl_ft || v < r.min_allowed_agl_ft {
                                    RED
                                } else {
                                    GREEN
                                };
                            s.rect(cx - bw / 2., fy(v), bw, b - fy(v), c, None, 0.);
                        } else {
                            text_f(
                                s,
                                cx,
                                b - 6.,
                                "×",
                                13.,
                                RED,
                                Anchor::Middle,
                                true,
                                MONO,
                            );
                        }
                        text_f(
                            s,
                            cx,
                            b + 13.,
                            &trunc(&g.gate, 4),
                            8.5,
                            MUTED,
                            Anchor::Middle,
                            false,
                            MONO,
                        );
                    }
                }),
            ),
            remarks,
        },
    )
}

// CSAR

fn csar(rec: &RangeRecord, r: &CsarResult) -> String {
    let col =
        if r.outcome == CsarOutcome::Failed { RED } else { precision_color(r.quality) };
    let [p, a] = pilot_row(rec);
    let mut remarks = vec![sentence(&format!(
        "{} survivor in {}",
        if r.hostile { "hostile area:" } else { "permissive area:" },
        r.area
    ))];
    if let Some(t) = r.time_to_pickup_s {
        remarks.push(sentence(&format!(
            "picked up {} after the MAYDAY{}",
            mmss(t),
            if r.pickup_method.is_empty() {
                String::new()
            } else {
                format!(", {}", r.pickup_method)
            }
        )));
    }
    if let (Some(t), Some(d)) = (r.time_total_s, &r.delivered_to) {
        remarks.push(sentence(&format!("delivered to {d}, {} from the MAYDAY", mmss(t))));
    }
    remarks.push(sentence(r.outcome.label()));
    render(
        rec,
        Sheet {
            title: "COMBAT SEARCH AND RESCUE RECORD",
            sub: "PERSONNEL RECOVERY · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 3710/20 (REV 10-26)",
            row_a: vec![
                ("AREA", r.area.clone(), TEXT, false),
                p,
                a,
                (
                    "THREAT",
                    if r.hostile { "HOSTILE".into() } else { "PERMISSIVE".into() },
                    if r.hostile { RED } else { TEXT },
                    true,
                ),
            ],
            row_b: vec![
                (
                    "TO PICKUP",
                    r.time_to_pickup_s.map(mmss).unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
                (
                    "TO HOME",
                    r.time_total_s.map(mmss).unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
                (
                    "PICKUP",
                    if r.pickup_method.is_empty() {
                        "-".into()
                    } else {
                        r.pickup_method.to_uppercase()
                    },
                    TEXT,
                    true,
                ),
                (
                    "DELIVERED TO",
                    r.delivered_to.clone().unwrap_or_else(|| "-".into()),
                    TEXT,
                    false,
                ),
            ],
            score: (
                r.time_total_s
                    .or(r.time_to_pickup_s)
                    .map(mmss)
                    .unwrap_or_else(|| "-".into()),
                r.outcome.label().to_uppercase(),
                col,
                r.quality.label().into(),
            ),
            detail: vec![
                ("Area", r.area.clone()),
                (
                    "Threat",
                    if r.hostile { "hostile".into() } else { "permissive".into() },
                ),
                (
                    "MAYDAY to pickup",
                    r.time_to_pickup_s.map(mmss).unwrap_or_else(|| "-".into()),
                ),
                (
                    "MAYDAY to home",
                    r.time_total_s.map(mmss).unwrap_or_else(|| "-".into()),
                ),
                ("Pickup", r.pickup_method.clone()),
                ("Delivered to", r.delivered_to.clone().unwrap_or_else(|| "-".into())),
            ],
            main: (
                "THE RESCUE · from the MAYDAY",
                Box::new(move |s, x, y, w, _h| {
                    let total =
                        r.time_total_s.or(r.time_to_pickup_s).unwrap_or(600.).max(60.);
                    let pick = r.time_to_pickup_s.unwrap_or(total);
                    let mut spans = vec![(0., pick, ORANGE, "TO THE SURVIVOR")];
                    if r.time_total_s.is_some() {
                        spans.push((pick, total, GREEN, "HOME"));
                    }
                    let mut marks = vec![(0., "MAYDAY")];
                    if r.time_to_pickup_s.is_some() {
                        marks.push((pick, "PICKUP"));
                    }
                    if r.time_total_s.is_some() {
                        marks.push((total, "DELIVERED"));
                    }
                    timeline(s, x + 30., y + 90., w - 60., total, &spans, &marks);
                    if r.hostile {
                        s.text(
                            x + 30.,
                            y + 200.,
                            "HOSTILE: troops were hunting the survivor",
                            12.,
                            RED,
                            Anchor::Start,
                            true,
                        );
                    }
                }),
            ),
            side: (
                "THE SURVIVOR",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    kv(
                        s,
                        x + 14.,
                        y + 34.,
                        w - 28.,
                        &[
                            ("Area", r.area.clone()),
                            (
                                "Threat",
                                if r.hostile {
                                    "hostile".into()
                                } else {
                                    "permissive".into()
                                },
                            ),
                            (
                                "Position",
                                if has(&r.survivor_pos) {
                                    format!(
                                        "{:.4} {:.4}",
                                        r.survivor_pos.lat, r.survivor_pos.lon
                                    )
                                } else {
                                    "-".into()
                                },
                            ),
                            (
                                "Pickup",
                                if r.pickup_method.is_empty() {
                                    "-".into()
                                } else {
                                    r.pickup_method.clone()
                                },
                            ),
                            (
                                "Delivered to",
                                r.delivered_to.clone().unwrap_or_else(|| "-".into()),
                            ),
                            ("Outcome", r.outcome.label().into()),
                        ],
                    );
                }),
            ),
            remarks,
        },
    )
}

// airdrop

fn airdrop(rec: &RangeRecord, r: &AirdropResult) -> String {
    let col = precision_color(r.quality);
    let [p, a] = pilot_row(rec);
    if r.method == "airland" {
        let remarks = vec![
            sentence(&format!(
                "{} ({:.0} kg) air-landed at {}",
                r.cargo, r.mass_kg, r.zone
            )),
            sentence(&format!("{} from loading to delivery", mmss(r.flight_s))),
            sentence(&format!("delivered with {:.0}% damage", r.damage * 100.)),
        ];
        return render(
            rec,
            Sheet {
                title: "AIR-LAND DELIVERY RECORD",
                sub: "AIRLIFT · AIR-LAND · VECTOR STRIKE TRAINING RANGE",
                form_no: "VSR FORM 3710/30 (REV 10-26)",
                row_a: vec![
                    ("FIELD", r.zone.clone(), TEXT, false),
                    p,
                    a,
                    ("CARGO", r.cargo.clone(), TEXT, false),
                ],
                row_b: vec![
                    ("MASS", format!("{:.0} KG", r.mass_kg), TEXT, true),
                    ("FLIGHT", mmss(r.flight_s), TEXT, true),
                    (
                        "DAMAGE",
                        format!("{:.0} %", r.damage * 100.),
                        if r.damage > 0.25 { RED } else { TEXT },
                        true,
                    ),
                ],
                score: (
                    mmss(r.flight_s),
                    "LOAD TO DELIVERY".into(),
                    col,
                    format!("{:.0}% damage", r.damage * 100.),
                ),
                detail: vec![
                    ("Delivered to", r.zone.clone()),
                    ("Cargo", r.cargo.clone()),
                    ("Mass", format!("{:.0} kg", r.mass_kg)),
                    ("Damage", format!("{:.0} %", r.damage * 100.)),
                ],
                main: (
                    "THE DELIVERY · against the clock",
                    Box::new(move |s, x, y, w, _h| {
                        timeline(
                            s,
                            x + 30.,
                            y + 90.,
                            w - 60.,
                            r.flight_s.max(60.),
                            &[(0., r.flight_s, BLUE, "LOADED TO DELIVERED")],
                            &[(0., "LOADED"), (r.flight_s, "UNLOADED")],
                        );
                    }),
                ),
                side: (
                    "THE LOAD",
                    200.,
                    Box::new(move |s, x, y, w, _h| {
                        bar(
                            s,
                            x + 14.,
                            y + 40.,
                            w - 28.,
                            r.mass_kg,
                            (r.mass_kg * 1.25).max(5000.),
                            BLUE,
                            "MASS",
                            &format!("{:.0} KG", r.mass_kg),
                        );
                        bar(
                            s,
                            x + 14.,
                            y + 96.,
                            w - 28.,
                            r.damage,
                            1.,
                            if r.damage > 0.25 { RED } else { GREEN },
                            "DAMAGE ON DELIVERY",
                            &format!("{:.0} %", r.damage * 100.),
                        );
                    }),
                ),
                remarks,
            },
        );
    }
    let rel = r.release.clone().unwrap_or_default();
    let mut remarks = vec![sentence(&format!(
        "{} ({:.0} kg) released {:.0} ft AGL at {:.0} kt on {:03.0}°",
        r.cargo,
        r.mass_kg,
        rel.alt_agl_ft,
        rel.gs_kts,
        rel.heading_deg.rem_euclid(360.)
    ))];
    if let (Some(l), Some(x)) = (r.long_m, r.cross_m) {
        remarks.push(sentence(&format!(
            "{:.0} m {} and {:.0} m {} of the PI",
            l.abs(),
            if l >= 0. { "long" } else { "short" },
            x.abs(),
            if x >= 0. { "right" } else { "left" }
        )));
    } else {
        remarks.push(sentence(&format!(
            "{:.0} m from the PI at {:03.0}°",
            r.distance_m,
            r.bearing_deg.rem_euclid(360.)
        )));
    }
    if let (Some(t), Some(d)) = (r.fall_s, r.drift_m) {
        remarks.push(sentence(&format!(
            "{t:.0} s under canopy, {d:.0} m from the release point"
        )));
    }
    if r.damage > 0.05 {
        remarks.push(sentence(&format!("{:.0}% damaged on landing", r.damage * 100.)));
    }
    render(
        rec,
        Sheet {
            title: "AIRDROP SCORE SHEET",
            sub: "AIRLIFT · AIRDROP · VECTOR STRIKE TRAINING RANGE",
            form_no: "VSR FORM 3710/31 (REV 10-26)",
            row_a: vec![
                ("DROP ZONE", r.zone.clone(), TEXT, false),
                p,
                a,
                ("CARGO", r.cargo.clone(), TEXT, false),
                ("MASS", format!("{:.0} KG", r.mass_kg), TEXT, true),
            ],
            row_b: vec![
                ("REL ALT AGL", format!("{:.0} FT", rel.alt_agl_ft), TEXT, true),
                ("REL ALT MSL", format!("{:.0} FT", rel.alt_msl_ft), TEXT, true),
                ("GROUND SPEED", format!("{:.0} KT", rel.gs_kts), TEXT, true),
                (
                    "HEADING",
                    format!("{:03.0}°", rel.heading_deg.rem_euclid(360.)),
                    TEXT,
                    true,
                ),
                (
                    "UNDER CANOPY",
                    r.fall_s.map(|t| format!("{t:.0} S")).unwrap_or_else(|| "-".into()),
                    TEXT,
                    true,
                ),
            ],
            score: (
                format!("{:.0} M", r.distance_m),
                "FROM THE POINT OF IMPACT".into(),
                col,
                r.quality.label().into(),
            ),
            detail: vec![
                (
                    "Bearing from the PI",
                    format!("{:03.0}°", r.bearing_deg.rem_euclid(360.)),
                ),
                (
                    "Long / short",
                    r.long_m.map(|l| format!("{l:+.0} m")).unwrap_or_else(|| "-".into()),
                ),
                (
                    "Left / right",
                    r.cross_m.map(|x| format!("{x:+.0} m")).unwrap_or_else(|| "-".into()),
                ),
                (
                    "Release to PI",
                    rel.to_dz_m
                        .map(|d| format!("{d:.0} m"))
                        .unwrap_or_else(|| "-".into()),
                ),
                (
                    "Drift",
                    r.drift_m.map(|d| format!("{d:.0} m")).unwrap_or_else(|| "-".into()),
                ),
                ("Damage", format!("{:.0} %", r.damage * 100.)),
            ],
            main: (
                "THE DROP ZONE · north up, rings in metres",
                Box::new(move |s, x, y, w, h| {
                    target_diagram(
                        s,
                        x,
                        y,
                        w,
                        h,
                        &r.dz_pos,
                        &r.impact_pos,
                        r.distance_m,
                        &[50., 100., 200., 400.],
                        col,
                        "box",
                    );
                }),
            ),
            side: (
                "THE RELEASE",
                200.,
                Box::new(move |s, x, y, w, _h| {
                    kv(
                        s,
                        x + 14.,
                        y + 34.,
                        w - 28.,
                        &[
                            ("Height", format!("{:.0} ft AGL", rel.alt_agl_ft)),
                            ("Ground speed", format!("{:.0} kt", rel.gs_kts)),
                            (
                                "Heading",
                                format!("{:03.0}°", rel.heading_deg.rem_euclid(360.)),
                            ),
                            (
                                "Release to PI",
                                rel.to_dz_m
                                    .map(|d| format!("{d:.0} m"))
                                    .unwrap_or_else(|| "-".into()),
                            ),
                            (
                                "Under canopy",
                                r.fall_s
                                    .map(|t| format!("{t:.0} s"))
                                    .unwrap_or_else(|| "-".into()),
                            ),
                            (
                                "Drift",
                                r.drift_m
                                    .map(|d| format!("{d:.0} m"))
                                    .unwrap_or_else(|| "-".into()),
                            ),
                        ],
                    );
                }),
            ),
            remarks,
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_discipline_has_a_form() {
        for r in super::super::cards::tests::all_cards() {
            let Some(svg) = sheet(&r) else { continue };
            assert!(!svg.contains("NaN"), "{}", r.kind());
            assert!(super::super::cards::png(&svg).is_ok(), "{}", r.kind());
        }
    }
}
