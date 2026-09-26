// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Result cards: an SVG per record, built as a string, rasterised to PNG with
//! resvg for Discord.
//!
//! Dark theme matching a Discord embed (#2b2d31) so the PNG sits in the embed
//! without a visible frame. Text uses Arial / Segoe UI from the system font
//! database (the server is Windows); with no fonts at all resvg simply draws
//! no text rather than failing.
//!
//! * trap    -- glideslope side view above a lineup top view (ship left, the
//!   pilot's track coming in from the right), LSO header
//! * bomb    -- FunkMan-style plot: target, scoring rings, the impact with its
//!   r / phi / quality box, the release box, a compass with the attack heading
//! * strafe  -- pit diagram, hits/rounds and an accuracy bar
//! * aar     -- receiver position in the tanker frame (side + top), contact
//!   timeline, stats
//! * missile -- plan view of missile and target tracks, launch point, closest
//!   approach, outcome
//! * anything else -- a generic card with the record's key fields

use super::discord;
use anyhow::{anyhow, Result};
use bfprotocols::range::{
    lso, AarResult, BombQuality, BombResult, GrooveSample, MissileOutcome, MissileResult,
    RangeRecord, RangeResult, RefuelMethod, RelSample, StrafeQuality, StrafeResult, Track,
    TrackPt, TrapResult,
};
use resvg::{tiny_skia, usvg, usvg::fontdb};
use std::{
    fmt::Write,
    sync::{Arc, OnceLock},
};

// ── palette ────────────────────────────────────────────────────────────

const BG: &str = "#2b2d31";
const PANEL: &str = "#1e1f22";
const GRID: &str = "#3f4147";
const TEXT: &str = "#f2f3f5";
const MUTED: &str = "#b5bac1";
const GREEN: &str = "#57f287";
const YELLOW: &str = "#fee75c";
const RED: &str = "#ed4245";
const ORANGE: &str = "#f0a04b";
const BLUE: &str = "#5da9ff";
const SAND: &str = "#c8ad7f";
const SHIP: &str = "#6d717a";
const FONT: &str = "Arial, 'Segoe UI', Helvetica, sans-serif";

const FT: f64 = 3.28084;
const NM: f64 = 1852.;

// ── fonts / rasterising ────────────────────────────────────────────────

static FONTS: OnceLock<Arc<fontdb::Database>> = OnceLock::new();

/// The system font database, loaded once.
pub(crate) fn fontdb() -> Arc<fontdb::Database> {
    FONTS
        .get_or_init(|| {
            let mut db = fontdb::Database::new();
            db.load_system_fonts();
            if db.faces().any(|f| f.families.iter().any(|(n, _)| n == "Arial")) {
                db.set_sans_serif_family("Arial");
            }
            log::info!("range cards: {} system font face(s) available", db.len());
            Arc::new(db)
        })
        .clone()
}

/// Rasterise an SVG to PNG bytes.
pub(crate) fn png(svg: &str) -> Result<Vec<u8>> {
    let mut opt = usvg::Options::default();
    opt.fontdb = fontdb();
    opt.font_family = "Arial".into();
    let tree = usvg::Tree::from_str(svg, &opt).map_err(|e| anyhow!("card svg: {e}"))?;
    let size = tree.size().to_int_size();
    let mut pm = tiny_skia::Pixmap::new(size.width(), size.height())
        .ok_or_else(|| anyhow!("card has an empty size"))?;
    resvg::render(&tree, tiny_skia::Transform::default(), &mut pm.as_mut());
    pm.encode_png().map_err(|e| anyhow!("png encode: {e}"))
}

// ── svg builder ────────────────────────────────────────────────────────

fn esc(s: &str) -> String {
    let mut o = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => o.push_str("&amp;"),
            '<' => o.push_str("&lt;"),
            '>' => o.push_str("&gt;"),
            '"' => o.push_str("&quot;"),
            '\'' => o.push_str("&apos;"),
            c if (c as u32) < 0x20 && c != '\t' => (),
            c => o.push(c),
        }
    }
    o
}

/// Finite or zero: an NaN in a coordinate makes the whole SVG unparsable.
fn n(x: f64) -> f64 {
    if x.is_finite() {
        x
    } else {
        0.
    }
}

fn trunc(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        let mut t: String = s.chars().take(max.saturating_sub(1)).collect();
        t.push('…');
        t
    }
}

struct Svg {
    w: f64,
    h: f64,
    b: String,
}

#[derive(Clone, Copy)]
enum Anchor {
    Start,
    Middle,
    End,
}

impl Svg {
    fn new(w: f64, h: f64) -> Self {
        let mut b = String::with_capacity(16 * 1024);
        let _ = write!(
            b,
            r#"<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="0 0 {w} {h}" font-family="{FONT}"><rect width="{w}" height="{h}" fill="{BG}"/>"#
        );
        Self { w, h, b }
    }

    fn raw(&mut self, s: &str) {
        self.b.push_str(s);
    }

    fn clip(&mut self, id: &str, x: f64, y: f64, w: f64, h: f64) {
        let _ = write!(
            self.b,
            r#"<clipPath id="{id}"><rect x="{:.1}" y="{:.1}" width="{:.1}" height="{:.1}"/></clipPath>"#,
            n(x),
            n(y),
            n(w),
            n(h)
        );
    }

    fn rect(&mut self, x: f64, y: f64, w: f64, h: f64, fill: &str, stroke: Option<(&str, f64)>, rx: f64) {
        let st = stroke
            .map(|(c, w)| format!(r#" stroke="{c}" stroke-width="{w}""#))
            .unwrap_or_default();
        let _ = write!(
            self.b,
            r#"<rect x="{:.1}" y="{:.1}" width="{:.1}" height="{:.1}" rx="{rx}" fill="{fill}"{st}/>"#,
            n(x),
            n(y),
            n(w.max(0.)),
            n(h.max(0.))
        );
    }

    fn line(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, stroke: &str, w: f64, dash: Option<&str>) {
        let d = dash.map(|d| format!(r#" stroke-dasharray="{d}""#)).unwrap_or_default();
        let _ = write!(
            self.b,
            r#"<line x1="{:.1}" y1="{:.1}" x2="{:.1}" y2="{:.1}" stroke="{stroke}" stroke-width="{w}"{d}/>"#,
            n(x1),
            n(y1),
            n(x2),
            n(y2)
        );
    }

    fn polyline(&mut self, pts: &[(f64, f64)], stroke: &str, w: f64, dash: Option<&str>, clip: Option<&str>) {
        if pts.len() < 2 {
            return;
        }
        let mut p = String::with_capacity(pts.len() * 14);
        for (x, y) in pts {
            let _ = write!(p, "{:.1},{:.1} ", n(*x), n(*y));
        }
        let d = dash.map(|d| format!(r#" stroke-dasharray="{d}""#)).unwrap_or_default();
        let c = clip.map(|c| format!(r#" clip-path="url(#{c})""#)).unwrap_or_default();
        let _ = write!(
            self.b,
            r#"<polyline points="{}" fill="none" stroke="{stroke}" stroke-width="{w}" stroke-linejoin="round" stroke-linecap="round"{d}{c}/>"#,
            p.trim_end()
        );
    }

    fn polygon(&mut self, pts: &[(f64, f64)], fill: &str, stroke: Option<(&str, f64)>, clip: Option<&str>) {
        let mut p = String::new();
        for (x, y) in pts {
            let _ = write!(p, "{:.1},{:.1} ", n(*x), n(*y));
        }
        let st = stroke
            .map(|(c, w)| format!(r#" stroke="{c}" stroke-width="{w}""#))
            .unwrap_or_default();
        let c = clip.map(|c| format!(r#" clip-path="url(#{c})""#)).unwrap_or_default();
        let _ = write!(self.b, r#"<polygon points="{}" fill="{fill}"{st}{c}/>"#, p.trim_end());
    }

    fn circle(&mut self, cx: f64, cy: f64, r: f64, fill: &str, stroke: Option<(&str, f64)>, dash: Option<&str>) {
        let st = stroke
            .map(|(c, w)| format!(r#" stroke="{c}" stroke-width="{w}""#))
            .unwrap_or_default();
        let d = dash.map(|d| format!(r#" stroke-dasharray="{d}""#)).unwrap_or_default();
        let _ = write!(
            self.b,
            r#"<circle cx="{:.1}" cy="{:.1}" r="{:.1}" fill="{fill}"{st}{d}/>"#,
            n(cx),
            n(cy),
            n(r.max(0.))
        );
    }

    fn text(&mut self, x: f64, y: f64, s: &str, size: f64, fill: &str, anchor: Anchor, bold: bool) {
        let a = match anchor {
            Anchor::Start => "start",
            Anchor::Middle => "middle",
            Anchor::End => "end",
        };
        let wgt = if bold { r#" font-weight="bold""# } else { "" };
        let _ = write!(
            self.b,
            r#"<text x="{:.1}" y="{:.1}" font-size="{size}" fill="{fill}" text-anchor="{a}"{wgt}>{}</text>"#,
            n(x),
            n(y),
            esc(s)
        );
    }

    /// An arrow from (x1,y1) to (x2,y2) with a head at the end.
    fn arrow(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, stroke: &str, w: f64) {
        self.line(x1, y1, x2, y2, stroke, w, None);
        let a = (y2 - y1).atan2(x2 - x1);
        let hl = 6. + w * 2.;
        let p1 = (x2 - hl * (a - 0.45).cos(), y2 - hl * (a - 0.45).sin());
        let p2 = (x2 - hl * (a + 0.45).cos(), y2 - hl * (a + 0.45).sin());
        self.polygon(&[(x2, y2), p1, p2], stroke, None, None);
    }

    fn finish(mut self) -> String {
        self.b.push_str("</svg>");
        self.b
    }
}

// ── shared bits ────────────────────────────────────────────────────────

fn footer(s: &mut Svg, rec: &RangeRecord) {
    let y = s.h - 16.;
    let left = if rec.theatre.is_empty() && rec.mission_date.is_empty() {
        "Vector Strike Range".to_string()
    } else {
        format!("{}: {} ({})", rec.theatre, rec.mission_date, rec.mission_time)
    };
    s.text(24., y, &left, 13., MUTED, Anchor::Start, false);
    let right = rec.ts.format("%Y-%m-%d %H:%M:%SZ").to_string();
    let w = s.w;
    s.text(w - 24., y, &right, 13., MUTED, Anchor::End, false);
}

fn bomb_color(q: BombQuality) -> &'static str {
    match q {
        BombQuality::Shack | BombQuality::Excellent => GREEN,
        BombQuality::Good => YELLOW,
        BombQuality::Ineffective => ORANGE,
        BombQuality::Poor => RED,
    }
}

fn strafe_color(q: StrafeQuality) -> &'static str {
    match q {
        StrafeQuality::Deadeye | StrafeQuality::Excellent => GREEN,
        StrafeQuality::Good => YELLOW,
        StrafeQuality::Ineffective => ORANGE,
        StrafeQuality::Poor | StrafeQuality::Invalid => RED,
    }
}

pub(crate) fn grade_color(grade: &str) -> &'static str {
    match grade {
        "_OK_" | "OK" => GREEN,
        "(OK)" => YELLOW,
        "B" | "--" | "OWO" => ORANGE,
        "WO" | "WOP" | "C" => RED,
        _ => MUTED,
    }
}

/// A "nice" axis value >= x: 1, 2, 2.5, 5 x 10^n.
fn nice_ceil(x: f64) -> f64 {
    if !(x > 0.) {
        return 1.;
    }
    let p = 10f64.powf(x.log10().floor());
    for m in [1., 2., 2.5, 5., 7.5, 10.] {
        if m * p >= x {
            return m * p;
        }
    }
    10. * p
}

/// A grid step giving about `ticks` divisions of `span`.
fn nice_step(span: f64, ticks: f64) -> f64 {
    let raw = span / ticks;
    let p = 10f64.powf(raw.log10().floor());
    for m in [1., 2., 5., 10.] {
        if m * p >= raw {
            return m * p;
        }
    }
    10. * p
}

/// Local flat-earth east/north metres of `p` from `o`.
fn en(o: (f64, f64), p: (f64, f64)) -> (f64, f64) {
    let r = 6371008.8;
    let lat = ((o.0 + p.0) / 2.).to_radians();
    ((p.1 - o.1).to_radians() * r * lat.cos(), (p.0 - o.0).to_radians() * r)
}

// ── dispatch ───────────────────────────────────────────────────────────

/// The card for a record (its `track` should be attached when it has one).
pub(crate) fn render_svg(rec: &RangeRecord) -> String {
    match &rec.result {
        RangeResult::Trap(t) => trap_card(rec, t),
        RangeResult::Bomb(b) => bomb_card(rec, b),
        RangeResult::Strafe(s) => strafe_card(rec, s),
        RangeResult::Aar(a) => aar_card(rec, a),
        RangeResult::Missile(m) => missile_card(rec, m),
        _ => generic_card(rec),
    }
}

/// A card for a record this build cannot decode: just what is certain.
pub(crate) fn unknown_svg(pilot: &str, unit_type: &str, kind: &str, ts: &str) -> String {
    let mut s = Svg::new(800., 220.);
    s.text(24., 48., &format!("Range result: {kind}"), 26., TEXT, Anchor::Start, true);
    s.text(24., 84., &format!("{pilot} | {unit_type}"), 17., MUTED, Anchor::Start, false);
    s.text(24., 130., "This result was recorded by a newer range engine.", 15., MUTED, Anchor::Start, false);
    s.text(776., 204., ts, 13., MUTED, Anchor::End, false);
    s.finish()
}

// ── trap ───────────────────────────────────────────────────────────────

fn trap_card(rec: &RangeRecord, t: &TrapResult) -> String {
    const W: f64 = 900.;
    const H: f64 = 930.;
    const PX: f64 = 60.;
    const PW: f64 = 800.;
    const X_MIN: f64 = -300.;
    const X_MAX: f64 = 2000.;
    const GS: f64 = 3.5;
    let mut s = Svg::new(W, H);
    let samples: Vec<GrooveSample> = match &rec.track {
        Some(Track::Groove { samples }) => samples.clone(),
        _ => vec![],
    };
    // header
    let gc = grade_color(&t.grade);
    s.text(24., 40., &format!("LSO GRADE: {}  =>  {}", t.grade, lso::grade_name(&t.grade)), 26., gc, Anchor::Start, true);
    s.text(
        24.,
        68.,
        &format!(
            "{} | {} | {} | CASE {} | {} | {}",
            rec.pilot.name,
            discord::short_airframe(&rec.unit_type),
            t.carrier,
            t.case,
            if t.night { "Night" } else { "Day" },
            t.outcome.label()
        ),
        16.,
        TEXT,
        Anchor::Start,
        false,
    );
    let comment = if t.lso_comment.is_empty() { "-".to_string() } else { t.lso_comment.clone() };
    s.text(24., 94., &trunc(&format!("LSO: {comment}"), 90), 15., MUTED, Anchor::Start, false);
    let pts = t.points.map(|p| format!("{p:.1} PTS")).unwrap_or_else(|| "NO COUNT".into());
    s.text(W - 24., 40., &pts, 24., gc, Anchor::End, true);
    let mut bits = vec![];
    if let Some(w) = t.wire {
        bits.push(format!("Wire #{w}"));
    }
    if let Some(g) = t.groove_time_s {
        bits.push(format!("Groove {g:.1}s"));
    }
    if let Some(w) = t.wind_over_deck_kts {
        bits.push(format!("WOD {w:.0} kts"));
    }
    s.text(W - 24., 68., &bits.join("  ·  "), 15., TEXT, Anchor::End, false);

    let sx = PW / (X_MAX - X_MIN);
    let px = |x_m: f64| PX + (x_m - X_MIN) * sx;

    // ── side view: glideslope ──
    let (sy0, sh) = (130., 270.);
    let max_alt = samples
        .iter()
        .filter(|p| p.x_m >= X_MIN && p.x_m <= X_MAX)
        .map(|p| p.alt_ft)
        .fold(0., f64::max);
    let alt_max = nice_ceil((max_alt * 1.1).clamp(500., 1500.));
    let ay = |alt: f64| sy0 + sh - alt / alt_max * sh;
    s.text(PX, sy0 - 10., "GLIDESLOPE (side view)", 13., MUTED, Anchor::Start, true);
    s.clip("side", PX, sy0, PW, sh);
    s.rect(PX, sy0, PW, sh, PANEL, None, 4.);
    let step = nice_step(alt_max, 5.);
    let mut a = step;
    while a < alt_max {
        s.line(PX, ay(a), PX + PW, ay(a), GRID, 1., None);
        s.text(PX - 6., ay(a) + 4., &format!("{a:.0}"), 11., MUTED, Anchor::End, false);
        a += step;
    }
    s.text(PX - 6., sy0 + 12., "ft", 11., MUTED, Anchor::End, false);
    for (q, lbl) in [(0.25, "1/4 NM"), (0.5, "1/2 NM"), (0.75, "3/4 NM"), (1., "1 NM")] {
        let x = px(q * NM);
        s.line(x, sy0, x, sy0 + sh, GRID, 1., Some("2 4"));
        s.text(x, sy0 + sh + 16., lbl, 11., MUTED, Anchor::Middle, false);
    }
    // glideslope and its bands
    let gs_line = |s: &mut Svg, deg: f64, col: &str, dash: Option<&str>, w: f64| {
        let x1 = X_MAX;
        let alt1 = x1 * deg.to_radians().tan() * FT;
        let _ = write!(
            s.b,
            r#"<line x1="{:.1}" y1="{:.1}" x2="{:.1}" y2="{:.1}" stroke="{col}" stroke-width="{w}"{} clip-path="url(#side)"/>"#,
            px(0.),
            ay(0.),
            px(x1),
            ay(alt1),
            dash.map(|d| format!(r#" stroke-dasharray="{d}""#)).unwrap_or_default()
        );
    };
    for (d, col) in [(1.5, RED), (0.8, YELLOW), (0.4, GREEN)] {
        gs_line(&mut s, GS + d, col, Some("3 5"), 1.5);
        gs_line(&mut s, GS - d, col, Some("3 5"), 1.5);
    }
    gs_line(&mut s, GS, GREEN, None, 1.2);
    // deck
    s.line(px(X_MIN), ay(0.), px(50.), ay(0.), SHIP, 5., None);
    let prof: Vec<(f64, f64)> = samples
        .iter()
        .filter(|p| p.x_m >= X_MIN - 50. && p.x_m <= X_MAX + 200.)
        .map(|p| (px(p.x_m), ay(p.alt_ft.max(-50.))))
        .collect();
    s.polyline(&prof, TEXT, 2.2, None, Some("side"));

    // ── top view: lineup ──
    let (ty0, th) = (450., 360.);
    let y_half = 300.;
    let tsy = (th / 2.) / y_half;
    let tyc = ty0 + th / 2.;
    let py = |y_m: f64| tyc - y_m * tsy;
    s.text(PX, ty0 - 10., "LINEUP (top view)", 13., MUTED, Anchor::Start, true);
    s.clip("top", PX, ty0, PW, th);
    s.rect(PX, ty0, PW, th, PANEL, None, 4.);
    // ship: the flight deck laid along the ship's heading, 9 deg right of
    // the landing area. Hull coordinates (a forward from the stern, b to
    // starboard); the angled-deck sponson sticks out to port, over the
    // landing strip.
    let ang = 9f64.to_radians();
    let fwd = (-ang.cos(), ang.sin());
    let right = (ang.sin(), ang.cos());
    let stern = (60., 12.);
    let hull = |a: f64, b: f64| {
        let x = stern.0 + a * fwd.0 + b * right.0;
        let y = stern.1 + a * fwd.1 + b * right.1;
        (px(x), py(y))
    };
    let hull_pts: Vec<(f64, f64)> = [
        (0., -18.),
        (0., 22.),
        (40., 38.),
        (230., 40.),
        (290., 26.),
        (330., 8.),
        (330., -6.),
        (250., -22.),
        (170., -44.),
        (60., -44.),
        (20., -30.),
    ]
    .iter()
    .map(|(a, b)| hull(*a, *b))
    .collect();
    s.polygon(&hull_pts, SHIP, Some(("#8a8f98", 1.)), Some("top"));
    s.polygon(
        &[(px(-230.), py(12.)), (px(55.), py(12.)), (px(55.), py(-12.)), (px(-230.), py(-12.))],
        "#8a8f98",
        None,
        Some("top"),
    );
    // centreline, wake, wedges
    s.line(px(-230.), tyc, px(X_MAX), tyc, "#9aa0a6", 1., Some("6 6"));
    let wake_end = X_MAX;
    s.raw(r#"<g clip-path="url(#top)">"#);
    s.line(px(60.), py(-60. * ang.tan()), px(wake_end), py(-wake_end * ang.tan()), "#4a4e55", 4., None);
    s.raw("</g>");
    s.text(px(X_MAX * 0.97), py(-X_MAX * 0.97 * ang.tan()) - 8., "wake", 11., MUTED, Anchor::End, false);
    for (d, col) in [(3., RED), (1., YELLOW), (0.5, GREEN)] {
        let y = X_MAX * f64::to_radians(d).tan();
        for sign in [1., -1.] {
            let _ = write!(
                s.b,
                r#"<line x1="{:.1}" y1="{:.1}" x2="{:.1}" y2="{:.1}" stroke="{col}" stroke-width="1.5" stroke-dasharray="3 5" clip-path="url(#top)"/>"#,
                px(0.),
                tyc,
                px(X_MAX),
                py(sign * y)
            );
        }
    }
    // distance arcs (right halves of ellipses: the axes are scaled differently)
    for (q, lbl) in [(0.25, "1/4 NM"), (0.5, "1/2 NM"), (0.75, "3/4 NM"), (1., "1 NM")] {
        let r = q * NM;
        let (rx, ry) = (r * sx, r * tsy);
        let cx = px(0.);
        let _ = write!(
            s.b,
            r#"<path d="M {:.1} {:.1} A {:.1} {:.1} 0 0 1 {:.1} {:.1}" fill="none" stroke="{GRID}" stroke-width="1.2" stroke-dasharray="2 4" clip-path="url(#top)"/>"#,
            cx,
            tyc - ry,
            rx,
            ry,
            cx,
            tyc + ry
        );
        let top = ty0 + 16.;
        let dy = tyc - top;
        let (lx, ly) = if dy < ry {
            (cx + rx * (1. - (dy / ry).powi(2)).sqrt() + 4., top)
        } else {
            (cx + 4., tyc - ry - 4.)
        };
        s.text(lx, ly, lbl, 11., MUTED, Anchor::Start, false);
    }
    // the pilot's ground track
    let track: Vec<(f64, f64)> = samples
        .iter()
        .filter(|p| p.x_m >= X_MIN - 100. && p.x_m <= X_MAX + 1000.)
        .map(|p| (px(p.x_m), py(p.y_m.clamp(-5000., 5000.))))
        .collect();
    s.polyline(&track, TEXT, 2.2, None, Some("top"));
    // wake crossing: where y + x tan(9 deg) changes sign, aft of the ship
    let wake = |p: &GrooveSample| p.y_m + p.x_m * ang.tan();
    let mut cross = None;
    for w in samples.windows(2) {
        let (a, b) = (&w[0], &w[1]);
        if a.x_m > 60. && b.x_m > 60. && wake(a).signum() != wake(b).signum() {
            let f = wake(a) / (wake(a) - wake(b));
            cross = Some((
                a.x_m + f * (b.x_m - a.x_m),
                a.y_m + f * (b.y_m - a.y_m),
                a.alt_ft + f * (b.alt_ft - a.alt_ft),
            ));
        }
    }
    if let Some(alt) = t.pattern.wake_alt_ft.or(cross.map(|c| c.2)) {
        let (cx, cy) = match cross {
            Some((x, y, _)) => (px(x.min(X_MAX)), py(y.clamp(-y_half, y_half))),
            None => (px(X_MAX * 0.8), py(-X_MAX * 0.8 * ang.tan())),
        };
        s.circle(cx, cy, 5., YELLOW, None, None);
        let bx = if cx + 160. > PX + PW { cx - 156. } else { cx + 16. };
        let by = if cy + 50. > ty0 + th { cy - 44. } else { cy + 18. };
        let (bx, by) = (bx.clamp(PX + 4., PX + PW - 144.), by.clamp(ty0 + 4., ty0 + th - 30.));
        s.line(cx, cy, bx + 70., by + 13., YELLOW, 1., None);
        s.rect(bx, by, 140., 26., PANEL, Some((YELLOW, 1.5)), 4.);
        s.text(bx + 70., by + 18., &format!("Wake Alt: {alt:.0}ft"), 13., YELLOW, Anchor::Middle, true);
    }
    if samples.is_empty() {
        s.text(W / 2., sy0 + sh / 2., "no groove track recorded", 16., MUTED, Anchor::Middle, false);
        s.text(W / 2., tyc, "no groove track recorded", 16., MUTED, Anchor::Middle, false);
    }
    // legend + decoded calls
    let ly = ty0 + th + 26.;
    let mut x = PX;
    for (col, lbl) in [(GREEN, "±0.4° / ±0.5°"), (YELLOW, "±0.8° / ±1°"), (RED, "±1.5° / ±3°")] {
        s.line(x, ly - 4., x + 24., ly - 4., col, 2., Some("3 4"));
        s.text(x + 30., ly, lbl, 12., MUTED, Anchor::Start, false);
        x += 150.;
    }
    let desc = if t.lso_description.is_empty() {
        lso::describe(&t.lso_comment)
    } else {
        t.lso_description.clone()
    };
    let mut lines: Vec<String> = vec![String::new()];
    for d in &desc {
        let cur = lines.last_mut().unwrap();
        if !cur.is_empty() && cur.chars().count() + d.chars().count() + 2 > 105 {
            lines.push(d.clone());
        } else {
            if !cur.is_empty() {
                cur.push_str(", ");
            }
            cur.push_str(d);
        }
    }
    for (i, l) in lines.iter().take(2).enumerate() {
        s.text(PX, ly + 24. + i as f64 * 17., &trunc(l, 110), 13., TEXT, Anchor::Start, false);
    }
    footer(&mut s, rec);
    s.finish()
}

// ── bomb ───────────────────────────────────────────────────────────────

fn bomb_card(rec: &RangeRecord, b: &BombResult) -> String {
    const W: f64 = 800.;
    const H: f64 = 900.;
    const PX: f64 = 50.;
    const PY: f64 = 96.;
    const PS: f64 = 700.;
    let mut s = Svg::new(W, H);
    s.text(24., 40., &format!("Bombing result of {}", rec.pilot.name), 26., TEXT, Anchor::Start, true);
    s.text(24., 70., &format!("{}: {}", b.range, b.target), 16., MUTED, Anchor::Start, false);

    let ring_max = b.rings_m.iter().cloned().fold(0., f64::max);
    let ext = nice_ceil((ring_max * 1.15).max(b.miss_m * 1.25).max(30.));
    let sc = (PS / 2.) / ext;
    let (cx, cy) = (PX + PS / 2., PY + PS / 2.);
    let to = |e: f64, nn: f64| (cx + e * sc, cy - nn * sc);
    s.clip("plot", PX, PY, PS, PS);
    s.rect(PX, PY, PS, PS, PANEL, None, 4.);
    let step = nice_step(2. * ext, 8.);
    let mut g = -((ext / step).floor()) * step;
    while g <= ext + 1e-9 {
        let (x, _) = to(g, 0.);
        let (_, y) = to(0., g);
        s.line(x, PY, x, PY + PS, GRID, 1., None);
        s.line(PX, y, PX + PS, y, GRID, 1., None);
        s.text(x, PY + PS + 16., &format!("{g:.0}"), 11., MUTED, Anchor::Middle, false);
        s.text(PX - 6., y + 4., &format!("{g:.0}"), 11., MUTED, Anchor::End, false);
        g += step;
    }
    s.text(PX + PS, PY + PS + 32., "east, m", 11., MUTED, Anchor::End, false);
    s.text(PX, PY - 6., "north, m", 11., MUTED, Anchor::Start, false);
    // target and rings
    let tr = (ext * 0.03).max(3.) * sc;
    s.circle(cx, cy, tr.max(6.), SAND, Some(("#8f7a52", 1.5)), None);
    for r in &b.rings_m {
        if *r <= 0. {
            continue;
        }
        s.circle(cx, cy, r * sc, "none", Some((MUTED, 1.2)), Some("3 5"));
        let a = 40f64.to_radians();
        let lbl = if r.fract().abs() > 1e-6 { format!("{r:.1} m") } else { format!("{r:.0} m") };
        s.text(cx + r * sc * a.cos() + 3., cy - r * sc * a.sin() - 3., &lbl, 11., MUTED, Anchor::Start, false);
    }
    // aircraft box, placed back along the attack heading
    let hdg = b.release.heading_deg.to_radians();
    let back = (-hdg.sin(), hdg.cos()); // screen dx, dy of "behind the target"
    let (bw, bh) = (170., 86.);
    let ax = (cx + back.0 * PS * 0.36 - bw / 2.).clamp(PX + 8., PX + PS - bw - 8.);
    let ay = (cy + back.1 * PS * 0.36 - bh / 2.).clamp(PY + 8., PY + PS - bh - 8.);
    let (acx, acy) = (ax + bw / 2., ay + bh / 2.);
    let (dx, dy) = (cx - acx, cy - acy);
    let dl = dx.hypot(dy).max(1.);
    let start = (acx + dx / dl * (bh / 2. + 8.), acy + dy / dl * (bh / 2. + 8.));
    let end = (cx - dx / dl * 28., cy - dy / dl * 28.);
    if dl > 90. {
        s.arrow(start.0, start.1, end.0, end.1, GREEN, 2.);
    }
    s.rect(ax, ay, bw, bh, PANEL, Some((GREEN, 2.)), 4.);
    let alt_ft = if b.release.pos.alt_m != 0. { b.release.pos.alt_m } else { b.release.alt_agl_m } * FT;
    s.text(ax + 10., ay + 20., &trunc(&rec.unit_type, 20), 14., GREEN, Anchor::Start, true);
    s.text(ax + 10., ay + 39., &format!("h={alt_ft:.0} ft"), 13., TEXT, Anchor::Start, false);
    s.text(ax + 10., ay + 57., &format!("v={:.0} kts", b.release.tas_kts), 13., TEXT, Anchor::Start, false);
    s.text(ax + 10., ay + 75., &format!("ψ={:.0}°", b.release.heading_deg.rem_euclid(360.)), 13., TEXT, Anchor::Start, false);
    // impact and its box
    let (ix, iy) = to(b.impact_east_m, b.impact_north_m);
    let (ox, oy) = (ix - cx, iy - cy);
    let ol = ox.hypot(oy);
    let dir = if ol > 1. { (ox / ol, oy / ol) } else { (0.7, -0.7) };
    let (qw, qh) = (180., 90.);
    let bx = (ix + dir.0 * 70. - qw / 2.).clamp(PX + 8., PX + PS - qw - 8.);
    let mut by = (iy + dir.1 * 70. - qh / 2.).clamp(PY + 8., PY + PS - qh - 8.);
    // keep the two boxes apart
    if bx < ax + bw && bx + qw > ax && by < ay + bh && by + qh > ay {
        by = if ay > PY + PS / 2. { (ay - qh - 10.).max(PY + 8.) } else { (ay + bh + 10.).min(PY + PS - qh - 8.) };
    }
    let qc = bomb_color(b.quality);
    s.line(ix, iy, bx + qw / 2., by + qh / 2., RED, 1.5, None);
    s.rect(bx, by, qw, qh, PANEL, Some((RED, 2.)), 4.);
    let wpn = if b.weapon_display.is_empty() { &b.weapon } else { &b.weapon_display };
    s.text(bx + 10., by + 20., &trunc(wpn, 20), 14., TEXT, Anchor::Start, true);
    s.text(bx + 10., by + 39., &format!("r={:.1} m", b.miss_m), 13., TEXT, Anchor::Start, false);
    s.text(bx + 10., by + 57., &format!("φ={:.1}°", b.radial_deg.rem_euclid(360.)), 13., TEXT, Anchor::Start, false);
    s.text(bx + 10., by + 76., b.quality.label(), 14., qc, Anchor::Start, true);
    s.circle(ix, iy, 6., RED, Some(("#ffffff", 1.5)), None);
    // compass: north plus the attack heading
    let (kx, ky, kr) = (PX + PS - 44., PY + 44., 30.);
    s.circle(kx, ky, kr, BG, Some((GRID, 1.5)), None);
    s.arrow(kx, ky + kr * 0.6, kx, ky - kr * 0.85, TEXT, 2.);
    s.text(kx, ky - kr - 6., "N", 13., TEXT, Anchor::Middle, true);
    let (hx, hy) = (hdg.sin(), -hdg.cos());
    s.arrow(kx - hx * kr * 0.6, ky - hy * kr * 0.6, kx + hx * kr * 0.85, ky + hy * kr * 0.85, GREEN, 2.);
    // stats line
    let class = format!("{:?}", b.weapon_class).to_lowercase();
    s.text(
        24.,
        PY + PS + 58.,
        &format!(
            "{} o'clock  ·  long {:+.0} m  ·  cross {:+.0} m  ·  dive {:.0}°  ·  AGL {:.0} ft  ·  ToF {:.1} s  ·  {class}",
            b.clock, b.long_m, b.cross_m, b.release.dive_deg, b.release.alt_agl_m * FT, b.time_of_flight_s
        ),
        13.,
        TEXT,
        Anchor::Start,
        false,
    );
    footer(&mut s, rec);
    s.finish()
}

// ── strafe ─────────────────────────────────────────────────────────────

fn strafe_card(rec: &RangeRecord, st: &StrafeResult) -> String {
    const W: f64 = 800.;
    const H: f64 = 600.;
    let mut s = Svg::new(W, H);
    s.text(24., 40., &format!("Strafe result of {}", rec.pilot.name), 26., TEXT, Anchor::Start, true);
    s.text(24., 70., &format!("{}: {}  ·  {}  ·  {}", st.range, st.pit, st.gun, rec.unit_type), 16., MUTED, Anchor::Start, false);
    // pit diagram
    let (px, py, pw, ph) = (40., 100., 300., 440.);
    s.rect(px, py, pw, ph, PANEL, None, 4.);
    let far = (st.foul_line_m * 1.6).max(st.min_range_m * 1.3).max(600.);
    let sc = (ph - 70.) / far;
    let (tx, ty) = (px + pw / 2., py + 40.);
    s.rect(tx - 14., ty - 14., 28., 28., SAND, Some(("#8f7a52", 1.5)), 2.);
    s.text(tx, ty - 20., "TARGET", 11., MUTED, Anchor::Middle, true);
    if st.foul_line_m > 0. {
        let fy = ty + st.foul_line_m * sc;
        s.line(px + 10., fy, px + pw - 10., fy, RED, 2., Some("8 5"));
        s.text(px + pw - 12., fy - 6., &format!("FOUL LINE {:.0} m", st.foul_line_m), 11., RED, Anchor::End, true);
    }
    if st.min_range_m > 0. {
        let my = ty + st.min_range_m * sc;
        let col = if st.foul_line_crossed { RED } else { YELLOW };
        s.line(tx - 40., my, tx + 40., my, col, 2., None);
        s.text(tx + 46., my + 4., &format!("closest {:.0} m", st.min_range_m), 11., col, Anchor::Start, false);
    }
    s.arrow(tx, py + ph - 14., tx, ty + st.min_range_m.max(st.foul_line_m) * sc + 26., TEXT, 3.);
    s.text(tx + 10., py + ph - 20., &format!("run-in {:03.0}°", st.run_in_heading_deg.rem_euclid(360.)), 12., TEXT, Anchor::Start, false);
    // numbers
    let (rx, qc) = (380., strafe_color(st.quality));
    s.text(rx, 150., &format!("{} / {}", st.hits, st.rounds_fired), 44., TEXT, Anchor::Start, true);
    s.text(rx, 176., "hits / rounds fired", 13., MUTED, Anchor::Start, false);
    let (bx, by, bw, bh) = (rx, 210., 380., 26.);
    s.rect(bx, by, bw, bh, PANEL, Some((GRID, 1.)), 4.);
    s.rect(bx, by, bw * (st.accuracy_pct / 100.).clamp(0., 1.), bh, qc, None, 4.);
    for band in [25., 50., 75., 90.] {
        let x = bx + bw * band / 100.;
        s.line(x, by - 4., x, by + bh + 4., MUTED, 1., None);
        s.text(x, by + bh + 18., &format!("{band:.0}%"), 11., MUTED, Anchor::Middle, false);
    }
    s.text(rx, 290., &format!("{:.1}%", st.accuracy_pct), 30., qc, Anchor::Start, true);
    s.text(rx + 130., 290., st.quality.label(), 26., qc, Anchor::Start, true);
    let mut y = 340.;
    let mut row = |s: &mut Svg, k: &str, v: String, col: &str| {
        s.text(rx, y, k, 14., MUTED, Anchor::Start, false);
        s.text(rx + 170., y, &v, 14., col, Anchor::Start, true);
        y += 26.;
    };
    row(&mut s, "Entry altitude", format!("{:.0} ft AGL", st.entry_alt_agl_m * FT), TEXT);
    row(&mut s, "Closest firing range", format!("{:.0} m", st.min_range_m), TEXT);
    row(
        &mut s,
        "Foul line",
        if st.foul_line_crossed { "CROSSED".into() } else { "respected".into() },
        if st.foul_line_crossed { RED } else { GREEN },
    );
    if let Some(r) = &st.invalid_reason {
        row(&mut s, "Invalid", trunc(r, 28), RED);
    }
    footer(&mut s, rec);
    s.finish()
}

// ── AAR ────────────────────────────────────────────────────────────────

fn aar_card(rec: &RangeRecord, a: &AarResult) -> String {
    const W: f64 = 900.;
    const H: f64 = 720.;
    let mut s = Svg::new(W, H);
    s.text(24., 40., &format!("AAR result of {}", rec.pilot.name), 26., TEXT, Anchor::Start, true);
    s.text(
        24.,
        70.,
        &format!(
            "{} ({})  ·  {}  ·  {}",
            a.tanker,
            a.tanker_type,
            match a.method {
                RefuelMethod::Boom => "boom",
                RefuelMethod::Drogue => "drogue",
            },
            rec.unit_type
        ),
        16.,
        MUTED,
        Anchor::Start,
        false,
    );
    let gcol = match a.grade.as_str() {
        "A" => GREEN,
        "B" => GREEN,
        "C" => YELLOW,
        "D" => ORANGE,
        _ => RED,
    };
    s.text(W - 24., 44., &format!("GRADE {}", a.grade), 28., gcol, Anchor::End, true);
    let samples: Vec<RelSample> = match &rec.track {
        Some(Track::Aar { samples }) => samples.clone(),
        _ => vec![],
    };
    let panel = |s: &mut Svg, id: &str, x0: f64, y0: f64, w: f64, h: f64, title: &str, xl: &str, yl: &str, pts: &[(f64, f64, bool)], mean: (f64, f64)| {
        s.text(x0, y0 - 8., title, 13., MUTED, Anchor::Start, true);
        s.clip(id, x0, y0, w, h);
        s.rect(x0, y0, w, h, PANEL, None, 4.);
        if pts.is_empty() {
            s.text(x0 + w / 2., y0 + h / 2., "no track", 14., MUTED, Anchor::Middle, false);
            return;
        }
        let (mut xmin, mut xmax, mut ymin, mut ymax) = (f64::MAX, f64::MIN, f64::MAX, f64::MIN);
        for (x, y, _) in pts {
            xmin = xmin.min(*x);
            xmax = xmax.max(*x);
            ymin = ymin.min(*y);
            ymax = ymax.max(*y);
        }
        let pad = |lo: f64, hi: f64| {
            let span = (hi - lo).max(6.);
            let mid = (hi + lo) / 2.;
            (mid - span * 0.6, mid + span * 0.6)
        };
        let (xmin, xmax) = pad(xmin, xmax);
        let (ymin, ymax) = pad(ymin, ymax);
        let fx = |x: f64| x0 + (x - xmin) / (xmax - xmin) * w;
        let fy = |y: f64| y0 + h - (y - ymin) / (ymax - ymin) * h;
        let gs = nice_step(xmax - xmin, 6.);
        let mut g = (xmin / gs).ceil() * gs;
        while g < xmax {
            s.line(fx(g), y0, fx(g), y0 + h, GRID, 1., None);
            s.text(fx(g), y0 + h + 14., &format!("{g:.0}"), 10., MUTED, Anchor::Middle, false);
            g += gs;
        }
        let gs = nice_step(ymax - ymin, 5.);
        let mut g = (ymin / gs).ceil() * gs;
        while g < ymax {
            s.line(x0, fy(g), x0 + w, fy(g), GRID, 1., None);
            s.text(x0 - 4., fy(g) + 4., &format!("{g:.0}"), 10., MUTED, Anchor::End, false);
            g += gs;
        }
        s.text(x0 + w, y0 + h + 28., xl, 11., MUTED, Anchor::End, false);
        s.text(x0 + 4., y0 + 14., yl, 11., MUTED, Anchor::Start, false);
        let _ = write!(s.b, r#"<g clip-path="url(#{id})">"#);
        for (x, y, _) in pts.iter().filter(|p| !p.2) {
            s.circle(fx(*x), fy(*y), 1.6, MUTED, None, None);
        }
        for (x, y, _) in pts.iter().filter(|p| p.2) {
            s.circle(fx(*x), fy(*y), 2.4, GREEN, None, None);
        }
        let (mx, my) = (fx(mean.0), fy(mean.1));
        s.line(mx - 10., my, mx + 10., my, YELLOW, 2., None);
        s.line(mx, my - 10., mx, my + 10., YELLOW, 2., None);
        s.raw("</g>");
    };
    let side: Vec<(f64, f64, bool)> = samples.iter().map(|p| (p.fwd_m, p.up_m, p.connected)).collect();
    let top: Vec<(f64, f64, bool)> = samples.iter().map(|p| (p.right_m, p.fwd_m, p.connected)).collect();
    let st = &a.stability;
    panel(&mut s, "aar_side", 60., 110., 380., 280., "SIDE VIEW (tanker frame)", "fwd, m", "up, m", &side, (st.mean_fwd_m, st.mean_up_m));
    panel(&mut s, "aar_top", 490., 110., 380., 280., "TOP VIEW (tanker frame)", "right, m", "fwd, m", &top, (st.mean_right_m, st.mean_fwd_m));
    // contact timeline
    let (lx, ly, lw, lh) = (60., 440., 810., 22.);
    s.text(lx, ly - 8., &format!("CONTACT TIMELINE  ·  {} contact(s), {} disconnect(s)", a.contacts, a.disconnects), 13., MUTED, Anchor::Start, true);
    s.rect(lx, ly, lw, lh, PANEL, Some((GRID, 1.)), 3.);
    let t_end = samples.last().map(|p| p.t).unwrap_or(a.session_s).max(1.);
    let t0 = samples.first().map(|p| p.t).unwrap_or(0.);
    let span = (t_end - t0).max(1.);
    let mut on: Option<f64> = None;
    for p in &samples {
        match (p.connected, on) {
            (true, None) => on = Some(p.t),
            (false, Some(st0)) => {
                s.rect(lx + (st0 - t0) / span * lw, ly, (p.t - st0) / span * lw, lh, GREEN, None, 0.);
                on = None;
            }
            _ => (),
        }
    }
    if let Some(st0) = on {
        s.rect(lx + (st0 - t0) / span * lw, ly, (t_end - st0) / span * lw, lh, GREEN, None, 0.);
    }
    s.text(lx, ly + lh + 16., "0 s", 11., MUTED, Anchor::Start, false);
    s.text(lx + lw, ly + lh + 16., &format!("{span:.0} s"), 11., MUTED, Anchor::End, false);
    // stats
    let rows: Vec<(String, String)> = vec![
        ("Time connected".into(), format!("{:.0} s", a.time_connected_s)),
        ("Fuel on-loaded".into(), format!("{:.0} lb ({:.0} kg)", a.fuel_lbs, a.fuel_kg)),
        ("On-load rate".into(), format!("{:.0} lb/min", a.onload_rate_lbs_min)),
        ("Join time".into(), a.join_time_s.map(|j| format!("{j:.0} s")).unwrap_or_else(|| "-".into())),
        ("Stability fore/aft".into(), format!("±{:.1} m", st.fore_aft_sd_m)),
        ("Stability lateral".into(), format!("±{:.1} m", st.lateral_sd_m)),
        ("Stability vertical".into(), format!("±{:.1} m", st.vertical_sd_m)),
        ("Pre-contact closure".into(), a.precontact_closure_kts.map(|c| format!("{c:.1} kt")).unwrap_or_else(|| "-".into())),
        ("Tanker".into(), format!("{:.0} ft, {:.0} kts", a.alt_ft, a.speed_kts)),
        ("Overshoot".into(), if a.overshoot { "yes".into() } else { "no".into() }),
    ];
    for (i, (k, v)) in rows.iter().enumerate() {
        let (x, y) = (60. + (i % 2) as f64 * 420., 510. + (i / 2) as f64 * 24.);
        s.text(x, y, k, 14., MUTED, Anchor::Start, false);
        s.text(x + 190., y, v, 14., TEXT, Anchor::Start, true);
    }
    let mut y = 510. + 5. * 24. + 10.;
    for c in a.calls.iter().take(3) {
        s.text(60., y, &format!("• {}", trunc(c, 100)), 14., YELLOW, Anchor::Start, false);
        y += 22.;
    }
    footer(&mut s, rec);
    s.finish()
}

// ── missile ────────────────────────────────────────────────────────────

fn missile_card(rec: &RangeRecord, m: &MissileResult) -> String {
    const W: f64 = 800.;
    const H: f64 = 860.;
    let mut s = Svg::new(W, H);
    let oc = match m.outcome {
        MissileOutcome::Kill | MissileOutcome::Hit => RED,
        MissileOutcome::Defeated => GREEN,
        MissileOutcome::Timeout => YELLOW,
    };
    s.text(24., 40., &format!("{}  —  {}", m.weapon, m.outcome.label()), 26., oc, Anchor::Start, true);
    s.text(
        24.,
        70.,
        &format!("{} ({})  →  {} ({})", m.shooter.name, m.shooter_type, m.target.name, m.target_type),
        16.,
        MUTED,
        Anchor::Start,
        false,
    );
    let (px, py, pw, ph) = (50., 96., 700., 520.);
    s.clip("plan", px, py, pw, ph);
    s.rect(px, py, pw, ph, PANEL, None, 4.);
    let (mis, tgt): (Vec<TrackPt>, Vec<TrackPt>) = match &rec.track {
        Some(Track::Intercept { missile, target }) => (missile.clone(), target.clone()),
        _ => (vec![], vec![]),
    };
    let origin = mis
        .first()
        .map(|p| (p.lat, p.lon))
        .unwrap_or((m.launch.shooter_pos.lat, m.launch.shooter_pos.lon));
    let me: Vec<(f64, f64)> = mis.iter().map(|p| en(origin, (p.lat, p.lon))).collect();
    let te: Vec<(f64, f64)> = tgt.iter().map(|p| en(origin, (p.lat, p.lon))).collect();
    if me.is_empty() && te.is_empty() {
        s.text(px + pw / 2., py + ph / 2., "no intercept track recorded", 16., MUTED, Anchor::Middle, false);
    } else {
        let all = me.iter().chain(te.iter());
        let (mut x0, mut x1, mut y0, mut y1) = (f64::MAX, f64::MIN, f64::MAX, f64::MIN);
        for (e, nn) in all {
            x0 = x0.min(*e);
            x1 = x1.max(*e);
            y0 = y0.min(*nn);
            y1 = y1.max(*nn);
        }
        let span = ((x1 - x0) / pw).max((y1 - y0) / ph).max(1.) * 1.15;
        let (mx, my) = ((x0 + x1) / 2., (y0 + y1) / 2.);
        let f = |p: &(f64, f64)| (px + pw / 2. + (p.0 - mx) / span, py + ph / 2. - (p.1 - my) / span);
        // grid + scale bar
        let step = nice_step(span * pw, 6.);
        let bar = step / span;
        s.line(px + 20., py + ph - 20., px + 20. + bar, py + ph - 20., TEXT, 2., None);
        let lbl = if step >= NM { format!("{:.0} nm", step / NM) } else { format!("{step:.0} m") };
        s.text(px + 20. + bar / 2., py + ph - 28., &lbl, 11., TEXT, Anchor::Middle, false);
        let tp: Vec<(f64, f64)> = te.iter().map(f).collect();
        let mp: Vec<(f64, f64)> = me.iter().map(f).collect();
        s.polyline(&tp, BLUE, 2.4, None, Some("plan"));
        s.polyline(&mp, RED, 2.4, None, Some("plan"));
        if let Some(p) = mp.first() {
            s.circle(p.0, p.1, 6., RED, Some(("#ffffff", 1.5)), None);
            s.text(p.0 + 9., p.1 - 8., "LAUNCH", 12., RED, Anchor::Start, true);
        }
        if let Some(p) = tp.first() {
            s.circle(p.0, p.1, 5., BLUE, Some(("#ffffff", 1.5)), None);
            s.text(p.0 + 9., p.1 + 16., "TARGET", 12., BLUE, Anchor::Start, true);
        }
        // closest approach: nearest target sample in time to each missile sample
        let mut best: Option<(f64, usize)> = None;
        for (i, p) in mis.iter().enumerate() {
            let j = tgt
                .iter()
                .enumerate()
                .min_by(|a, b| (a.1.t - p.t).abs().partial_cmp(&(b.1.t - p.t).abs()).unwrap_or(std::cmp::Ordering::Equal))
                .map(|(j, _)| j);
            if let Some(j) = j {
                let (a, b) = (me[i], te[j]);
                let d = (a.0 - b.0).hypot(a.1 - b.1);
                if best.map_or(true, |(bd, _)| d < bd) {
                    best = Some((d, i));
                }
            }
        }
        if let Some((_, i)) = best {
            let p = mp[i];
            s.circle(p.0, p.1, 10., "none", Some((YELLOW, 2.)), None);
            s.text(p.0 + 14., p.1 + 4., &format!("CPA {:.0} m", m.min_distance_m), 13., YELLOW, Anchor::Start, true);
        }
        if let Some(p) = mp.last() {
            s.text(p.0, p.1 - 14., m.outcome.label(), 13., oc, Anchor::Middle, true);
        }
    }
    let l = &m.launch;
    let d = &m.defense;
    let rows: Vec<(String, String)> = vec![
        ("Launch range".into(), format!("{:.1} nm", l.range_m / NM)),
        ("Aspect at launch".into(), format!("{:.0}°", l.aspect_deg)),
        ("Shooter / target alt".into(), format!("{:.0} / {:.0} ft", l.shooter_alt_m * FT, l.target_alt_m * FT)),
        ("Closure".into(), format!("{:.0} kts", l.closure_kts)),
        ("Time of flight".into(), format!("{:.1} s", m.time_of_flight_s)),
        ("Miss distance".into(), format!("{:.0} m (kill radius {:.0} m)", m.min_distance_m, m.kill_radius_m)),
        ("Reaction".into(), d.reaction_s.map(|r| format!("{r:.1} s")).unwrap_or_else(|| "none".into())),
        ("Beam / drag / hot".into(), format!("{:.0} / {:.0} / {:.0} s", d.beam_s, d.drag_s, d.hot_s)),
        ("Altitude change".into(), format!("{:+.0} ft{}", d.alt_change_m * FT, if d.went_low { ", went low" } else { "" })),
        ("Category".into(), m.weapon_category.to_uppercase()),
    ];
    for (i, (k, v)) in rows.iter().enumerate() {
        let (x, y) = (50. + (i % 2) as f64 * 370., 650. + (i / 2) as f64 * 26.);
        s.text(x, y, k, 14., MUTED, Anchor::Start, false);
        s.text(x + 170., y, v, 14., TEXT, Anchor::Start, true);
    }
    footer(&mut s, rec);
    s.finish()
}

// ── generic ────────────────────────────────────────────────────────────

fn generic_card(rec: &RangeRecord) -> String {
    let fields = discord::fields(rec);
    let rows = fields.len().div_ceil(2) as f64;
    let h = (180. + rows * 30.).max(260.);
    let mut s = Svg::new(800., h);
    let (title, _) = discord::title_and_color(rec);
    s.text(24., 42., &title, 26., TEXT, Anchor::Start, true);
    s.text(24., 72., &format!("{} | {}", rec.pilot.name, rec.unit_type), 16., MUTED, Anchor::Start, false);
    s.text(24., 104., &trunc(&rec.headline(), 90), 15., TEXT, Anchor::Start, false);
    for (i, (k, v, _)) in fields.iter().enumerate() {
        let (x, y) = (24. + (i % 2) as f64 * 390., 146. + (i / 2) as f64 * 30.);
        s.text(x, y, k, 14., MUTED, Anchor::Start, false);
        s.text(x + 150., y, &trunc(&v.replace("**", ""), 30), 14., TEXT, Anchor::Start, true);
    }
    footer(&mut s, rec);
    s.finish()
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use bfprotocols::range::*;
    use chrono::Utc;

    pub(crate) fn base(result: RangeResult, track: Option<Track>) -> RangeRecord {
        RangeRecord {
            id: "test-1-1".into(),
            v: RECORD_VERSION,
            ts: Utc::now(),
            mission_time: "15:48:13".into(),
            mission_date: "2024/8/1".into(),
            theatre: "Caucasus".into(),
            pilot: PilotRef { ucid: Some("u1".into()), name: "Casper <&> 1".into() },
            unit_type: "FA-18C_hornet".into(),
            side: "blue".into(),
            callsign: "Casper 1-1".into(),
            score: Some(3.),
            result,
            track,
        }
    }

    pub(crate) fn trap() -> RangeRecord {
        let samples = (0..120)
            .map(|i| {
                let t = i as f64 * 0.5;
                let x = 2400. - t * 40.;
                GrooveSample {
                    t,
                    x_m: x,
                    y_m: if x > 1200. { -(x - 1200.) * 0.5 } else { 5. },
                    alt_ft: (x * 3.5f64.to_radians().tan() * FT + 30.).max(0.),
                    gse_deg: 0.2,
                    lue_deg: 0.1,
                    aoa_deg: Some(8.1),
                    closure_kts: 130.,
                    vs_fpm: 700.,
                    lat: 0.,
                    lon: 0.,
                }
            })
            .collect();
        base(
            RangeResult::Trap(TrapResult {
                carrier: "CVN-73".into(),
                carrier_type: "CVN_73".into(),
                case: 1,
                night: false,
                outcome: PassOutcome::Waveoff,
                grade: "WO".into(),
                points: Some(1.),
                lso_comment: "AAX FIM (SLO)AR _HAW_".into(),
                lso_description: vec![],
                wire: None,
                wire_from_dcs: false,
                groove_time_s: Some(16.4),
                wind_over_deck_kts: Some(25.),
                final_bearing_deg: Some(350.),
                source: GradeSource::Dcs,
                dcs_comment: None,
                engine_grade: None,
                pattern: PatternSummary { wake_alt_ft: Some(412.), ..Default::default() },
                hook_down: Some(true),
            }),
            Some(Track::Groove { samples }),
        )
    }

    pub(crate) fn bomb() -> RangeRecord {
        base(
            RangeResult::Bomb(BombResult {
                station_id: "A".into(),
                range: "Range A - Bomb Circle".into(),
                target: "Circle 1".into(),
                weapon: "GBU_16".into(),
                weapon_display: "GBU-16".into(),
                weapon_class: WeaponClass::Guided,
                guidance: "laser".into(),
                release: Release {
                    pos: GeoPt { lat: 42.0, lon: 41.9, alt_m: 3234. },
                    alt_agl_m: 3200.,
                    tas_kts: 317.,
                    gs_kts: 330.,
                    heading_deg: 78.,
                    dive_deg: 10.,
                    slant_range_m: 6000.,
                    ground_range_m: 5000.,
                    wind_from_deg: 270.,
                    wind_kts: 12.,
                    mach: 0.5,
                    atmo: vec![],
                },
                target_pos: GeoPt { lat: 42.01, lon: 41.95, alt_m: 30. },
                impact: GeoPt { lat: 42.0099, lon: 41.9496, alt_m: 30. },
                impact_north_m: -6.4,
                impact_east_m: -36.4,
                miss_m: 37.,
                radial_deg: 260.,
                clock: 7,
                long_m: -20.,
                cross_m: -30.,
                time_of_flight_s: 31.,
                quality: BombQuality::Ineffective,
                target_hit: false,
                laser_code: Some(1688),
                rings_m: vec![5., 12.5, 25., 50.],
                good_radius_m: 25.,
            }),
            Some(Track::Weapon { points: vec![] }),
        )
    }

    fn all_cards() -> Vec<RangeRecord> {
        let strafe = base(
            RangeResult::Strafe(StrafeResult {
                station_id: "S".into(),
                range: "Range B".into(),
                pit: "Pit 2".into(),
                gun: "M61".into(),
                rounds_fired: 400,
                hits: 123,
                accuracy_pct: 30.75,
                quality: StrafeQuality::Ineffective,
                foul_line_crossed: true,
                invalid_reason: Some("crossed foul line".into()),
                run_in_heading_deg: 45.,
                min_range_m: 450.,
                entry_alt_agl_m: 300.,
                target_pos: GeoPt::default(),
                foul_line_m: 500.,
            }),
            None,
        );
        let aar_samples = (0..200)
            .map(|i| RelSample {
                t: i as f64,
                fwd_m: -20. + (i as f64 * 0.1).sin() * 2.,
                right_m: (i as f64 * 0.07).cos(),
                up_m: -8. + (i as f64 * 0.13).sin(),
                connected: (40..150).contains(&i),
                fuel_kg: i as f64 * 10.,
                closure_kts: 0.5,
            })
            .collect();
        let aar = base(
            RangeResult::Aar(AarResult {
                tanker: "Texaco".into(),
                tanker_type: "KC-135".into(),
                method: RefuelMethod::Boom,
                join_time_s: Some(180.),
                contacts: 2,
                disconnects: 1,
                time_connected_s: 110.,
                fuel_kg: 3000.,
                fuel_lbs: 6614.,
                onload_rate_lbs_min: 3600.,
                stability: Stability { fore_aft_sd_m: 1.2, lateral_sd_m: 0.6, vertical_sd_m: 0.9, mean_fwd_m: -20., mean_right_m: 0., mean_up_m: -8. },
                precontact_closure_kts: Some(3.),
                overshoot: false,
                alt_ft: 22000.,
                speed_kts: 280.,
                grade: "B".into(),
                calls: vec!["Unstable in contact, mostly fore-aft".into()],
                session_s: 200.,
            }),
            Some(Track::Aar { samples: aar_samples }),
        );
        let pt = |t: f64, lat: f64, lon: f64| TrackPt { t, lat, lon, alt_m: 8000., speed_kts: 900. };
        let missile = base(
            RangeResult::Missile(MissileResult {
                weapon: "AIM_120C".into(),
                weapon_category: "aam".into(),
                shooter: PilotRef { ucid: Some("u1".into()), name: "Casper".into() },
                shooter_type: "FA-18C_hornet".into(),
                target: PilotRef { ucid: None, name: "MiG-29".into() },
                target_type: "MiG-29A".into(),
                outcome: MissileOutcome::Kill,
                launch: LaunchGeom { range_m: 30000., aspect_deg: 10., ..Default::default() },
                min_distance_m: 8.,
                time_of_flight_s: 40.,
                kill_radius_m: 15.,
                defense: DefenseSummary::default(),
                perspective: "shooter".into(),
            }),
            Some(Track::Intercept {
                missile: (0..40).map(|i| pt(i as f64, 42. + i as f64 * 0.005, 41.)).collect(),
                target: (0..40).map(|i| pt(i as f64, 42.3 - i as f64 * 0.002, 41.02)).collect(),
            }),
        );
        let sling = base(
            RangeResult::Sling(SlingResult {
                method: "sling".into(),
                course: "Course 1".into(),
                cargo: "container".into(),
                mass_kg: 1500.,
                time_s: 300.,
                distance_m: 4.2,
                damage: 0.,
                quality: PrecisionQuality::Good,
                dz_pos: GeoPt::default(),
                set_down_pos: GeoPt::default(),
            }),
            None,
        );
        let no_track_trap = RangeRecord { track: None, ..trap() };
        vec![trap(), no_track_trap, bomb(), strafe, aar, missile, sling]
    }

    #[test]
    fn every_card_renders_svg_and_png() {
        for rec in all_cards() {
            let svg = render_svg(&rec);
            assert!(svg.starts_with("<svg") && svg.ends_with("</svg>"), "{}", rec.kind());
            assert!(!svg.contains("NaN"), "{}", rec.kind());
            let png = png(&svg).unwrap_or_else(|e| panic!("{}: {e:?}", rec.kind()));
            assert!(png.len() > 1000 && png.starts_with(b"\x89PNG"), "{}", rec.kind());
        }
        let u = unknown_svg("x", "y", "z", "now");
        assert!(png(&u).is_ok());
    }

    #[test]
    fn nice_numbers() {
        assert_eq!(nice_ceil(37.), 50.);
        assert_eq!(nice_ceil(46.), 50.);
        assert_eq!(nice_ceil(120.), 200.);
        assert_eq!(nice_step(100., 5.), 20.);
    }
}
