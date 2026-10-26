// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Result cards: an SVG per record, built as a string, rasterised to PNG with
//! resvg for Discord.
//!
//! Ink on paper, laid out as the forms the services use (see `forms.rs`:
//! the LSO pass record, the range score sheet, and the form frame every
//! other card sits in). Text uses Arial / Segoe UI from the system font
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
//! * field landing -- [`forms::field_form`]: the pattern, the final, the
//!   touchdown, the rollout and the taxi
//! * low level -- AGL and timing at every gate against the ceiling and the
//!   plan
//! * anything else (SEAD, hot zone, CSAR, helo drills ...) -- a generic card
//!   with the record's key fields

use super::{discord, forms, sheets};
use anyhow::{anyhow, Result};
use bfprotocols::range::{BombQuality, PrecisionQuality, RangeRecord, RangeResult, StrafeQuality};
use resvg::{tiny_skia, usvg, usvg::fontdb};
use std::{
    fmt::Write,
    sync::{Arc, OnceLock},
};

// ── palette ────────────────────────────────────────────────────────────

pub(crate) const BG: &str = "#f4f1e8";
pub(crate) const PANEL: &str = "#ebe6d6";
pub(crate) const GRID: &str = "#cfc8b4";
pub(crate) const TEXT: &str = "#15191e";
pub(crate) const MUTED: &str = "#5d6369";
pub(crate) const GREEN: &str = "#1d7a3e";
pub(crate) const YELLOW: &str = "#a87f00";
pub(crate) const RED: &str = "#b42318";
pub(crate) const ORANGE: &str = "#c05a00";
pub(crate) const BLUE: &str = "#1f5fa6";
pub(crate) const SAND: &str = "#a8894a";
/// blueprint ink, for the technical drawings
pub(crate) const INK: &str = "#1b3a5c";
const FONT: &str = "Arial, 'Segoe UI', Helvetica, sans-serif";

#[cfg(test)]
const FT: f64 = 3.28084;

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

pub(crate) fn esc(s: &str) -> String {
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
pub(crate) fn n(x: f64) -> f64 {
    if x.is_finite() {
        x
    } else {
        0.
    }
}

pub(crate) fn trunc(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        let mut t: String = s.chars().take(max.saturating_sub(1)).collect();
        t.push('…');
        t
    }
}

pub(crate) struct Svg {
    pub(crate) w: f64,
    pub(crate) h: f64,
    pub(crate) b: String,
    /// where the body starts, after the opening tag and the background
    start: usize,
}

#[derive(Clone, Copy)]
pub(crate) enum Anchor {
    Start,
    Middle,
    End,
}

impl Svg {
    pub(crate) fn new(w: f64, h: f64) -> Self {
        let mut b = String::with_capacity(16 * 1024);
        let _ = write!(
            b,
            r#"<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="0 0 {w} {h}" font-family="{FONT}"><rect width="{w}" height="{h}" fill="{BG}"/>"#
        );
        let start = b.len();
        Self { w, h, b, start }
    }

    /// Everything drawn so far, without the opening tag and the background.
    pub(crate) fn inner(&self) -> &str {
        &self.b[self.start..]
    }

    pub(crate) fn raw(&mut self, s: &str) {
        self.b.push_str(s);
    }

    pub(crate) fn clip(&mut self, id: &str, x: f64, y: f64, w: f64, h: f64) {
        let _ = write!(
            self.b,
            r#"<clipPath id="{id}"><rect x="{:.1}" y="{:.1}" width="{:.1}" height="{:.1}"/></clipPath>"#,
            n(x),
            n(y),
            n(w),
            n(h)
        );
    }

    pub(crate) fn rect(&mut self, x: f64, y: f64, w: f64, h: f64, fill: &str, stroke: Option<(&str, f64)>, rx: f64) {
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

    pub(crate) fn line(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, stroke: &str, w: f64, dash: Option<&str>) {
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

    pub(crate) fn polyline(&mut self, pts: &[(f64, f64)], stroke: &str, w: f64, dash: Option<&str>, clip: Option<&str>) {
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

    pub(crate) fn polygon(&mut self, pts: &[(f64, f64)], fill: &str, stroke: Option<(&str, f64)>, clip: Option<&str>) {
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

    pub(crate) fn circle(&mut self, cx: f64, cy: f64, r: f64, fill: &str, stroke: Option<(&str, f64)>, dash: Option<&str>) {
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

    pub(crate) fn text(&mut self, x: f64, y: f64, s: &str, size: f64, fill: &str, anchor: Anchor, bold: bool) {
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
    pub(crate) fn arrow(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, stroke: &str, w: f64) {
        self.line(x1, y1, x2, y2, stroke, w, None);
        let a = (y2 - y1).atan2(x2 - x1);
        let hl = 6. + w * 2.;
        let p1 = (x2 - hl * (a - 0.45).cos(), y2 - hl * (a - 0.45).sin());
        let p2 = (x2 - hl * (a + 0.45).cos(), y2 - hl * (a + 0.45).sin());
        self.polygon(&[(x2, y2), p1, p2], stroke, None, None);
    }

    pub(crate) fn finish(mut self) -> String {
        self.b.push_str("</svg>");
        self.b
    }
}

// ── shared bits ────────────────────────────────────────────────────────


pub(crate) fn bomb_color(q: BombQuality) -> &'static str {
    match q {
        BombQuality::Shack | BombQuality::Excellent => GREEN,
        BombQuality::Good => YELLOW,
        BombQuality::Ineffective => ORANGE,
        BombQuality::Poor => RED,
    }
}

pub(crate) fn strafe_color(q: StrafeQuality) -> &'static str {
    match q {
        StrafeQuality::Deadeye | StrafeQuality::Excellent => GREEN,
        StrafeQuality::Good => YELLOW,
        StrafeQuality::Ineffective => ORANGE,
        StrafeQuality::Poor | StrafeQuality::Invalid => RED,
    }
}

pub(crate) fn precision_color(q: PrecisionQuality) -> &'static str {
    match q {
        PrecisionQuality::Perfect | PrecisionQuality::Excellent => GREEN,
        PrecisionQuality::Good => YELLOW,
        PrecisionQuality::Fair => ORANGE,
        PrecisionQuality::Poor => RED,
    }
}

pub(crate) fn tier_color(tier: &str) -> &'static str {
    match tier {
        "easy" => GREEN,
        "medium" => YELLOW,
        "hard" => RED,
        _ => MUTED,
    }
}

/// Label / value rows in two columns from `y`, 24 px apart; returns the y
/// below the last row.
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
#[cfg(test)]
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
pub(crate) fn nice_step(span: f64, ticks: f64) -> f64 {
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
pub(crate) fn en(o: (f64, f64), p: (f64, f64)) -> (f64, f64) {
    let r = 6371008.8;
    let lat = ((o.0 + p.0) / 2.).to_radians();
    ((p.1 - o.1).to_radians() * r * lat.cos(), (p.0 - o.0).to_radians() * r)
}

// ── dispatch ───────────────────────────────────────────────────────────

/// The card for a record (its `track` should be attached when it has one).
pub(crate) fn render_svg(rec: &RangeRecord) -> String {
    match &rec.result {
        RangeResult::Trap(t) => forms::trap_form(rec, t),
        RangeResult::Bomb(b) => forms::bomb_form(rec, b),
        RangeResult::FieldLanding(f) => forms::field_form(rec, f),
        // every other discipline: its own form on the shared layout
        _ => sheets::sheet(rec).unwrap_or_else(|| forms::frame(generic_card(rec), rec, "TRAINING EVENT RECORD", "VSR FORM 11-1 (REV 10-26)")),
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

// ── generic ────────────────────────────────────────────────────────────

fn generic_card(rec: &RangeRecord) -> Svg {
    let fields = discord::fields(rec);
    // short fields in two columns, long ones (calls, kill lists) full width
    let (short, long): (Vec<_>, Vec<_>) = fields.into_iter().partition(|(_, _, inline)| *inline);
    let rows = short.len().div_ceil(2) as f64 + long.len() as f64;
    let h = (180. + rows * 30.).max(260.);
    let mut s = Svg::new(800., h);
    let (title, _) = discord::title_and_color(rec);
    s.text(24., 42., &title, 26., TEXT, Anchor::Start, true);
    s.text(24., 72., &format!("{} | {}", rec.pilot.name, rec.unit_type), 16., MUTED, Anchor::Start, false);
    s.text(24., 104., &trunc(&rec.headline(), 90), 15., TEXT, Anchor::Start, false);
    let clean = |v: &str| v.replace("**", "").replace('\n', "  ·  ");
    for (i, (k, v, _)) in short.iter().enumerate() {
        let (x, y) = (24. + (i % 2) as f64 * 390., 146. + (i / 2) as f64 * 30.);
        s.text(x, y, k, 14., MUTED, Anchor::Start, false);
        s.text(x + 150., y, &trunc(&clean(v), 30), 14., TEXT, Anchor::Start, true);
    }
    let y0 = 146. + short.len().div_ceil(2) as f64 * 30.;
    for (i, (k, v, _)) in long.iter().enumerate() {
        let y = y0 + i as f64 * 30.;
        s.text(24., y, k, 14., MUTED, Anchor::Start, false);
        s.text(174., y, &trunc(&clean(v), 78), 14., TEXT, Anchor::Start, true);
    }
    s
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use bfprotocols::range::*;
    use chrono::Utc;

    /// An overhead, left-hand, to runway 31 at the fixture's threshold, then the rollout and a taxi.
    pub(crate) fn field_track() -> Track {
        let (lat0, lon0, elev, h) = (41.6283f64, 45.0347f64, 460., 315f64.to_radians());
        let pt = |t: f64, along: f64, across: f64, agl_ft: f64, gs: f64| {
            let nn = along * h.cos() - across * h.sin();
            let e = along * h.sin() + across * h.cos();
            TrackPt {
                t,
                lat: lat0 + (nn / 6371008.8).to_degrees(),
                lon: lon0 + (e / (6371008.8 * lat0.to_radians().cos())).to_degrees(),
                alt_m: elev + agl_ft / 3.28084,
                speed_kts: gs,
            }
        };
        let nm = 1852.;
        let mut p = vec![];
        let mut push = |a: f64, c: f64, ft: f64, gs: f64| p.push((a, c, ft, gs));
        let mut a = -3. * nm;
        while a < 900. {
            push(a, 10., 1500., 300.);
            a += 150.;
        }
        for k in 0..12 {
            let th = (k as f64 + 0.5) / 12. * std::f64::consts::PI;
            push(900. + th.sin() * 400., -(1. - th.cos()) * 1.2 * nm / 2., 1500. - k as f64 * 40., 250.);
        }
        let mut a = 900.;
        while a > -1200. {
            push(a, -1.2 * nm, 1000., 150.);
            a -= 75.;
        }
        for k in 0..10 {
            let th = (k as f64 + 0.5) / 10. * std::f64::consts::PI;
            push(-1200. - th.sin() * 350., -1.2 * nm * th.cos().max(0.).max((1. - th / std::f64::consts::PI) * 0.6), 900. - k as f64 * 30., 140.);
        }
        let mut a = -0.6 * nm;
        while a < 480. {
            push(a, if a > 0. { -4.2 * a / 480. } else { 2. }, (-(a - 300.)).max(0.) * 0.0524 * 3.28084, 135.);
            a += 60.;
        }
        let n = p.len() as f64;
        let pattern: Vec<TrackPt> = p.iter().enumerate().map(|(i, q)| pt(i as f64 - n + 1., q.0, q.1, q.2, q.3)).collect();
        // rollout, a turn off at 1580 m, a taxi with a stop, parked
        let mut taxi = vec![];
        let (mut t, mut along, mut gs) = (0., 480., 135.);
        while gs > 25. {
            taxi.push(pt(t, along, -4., 0., gs));
            along += gs * 0.514;
            gs -= 6.;
            t += 1.;
        }
        for k in 0..60 {
            let (c, g) = (-(k as f64) * 9. - 25., if (8..14).contains(&k) { 38. } else if (30..36).contains(&k) { 0. } else { 15. });
            taxi.push(pt(t, along + if k > 40 { (k - 40) as f64 * 9. } else { 0. }, c.max(-380.), 0., g));
            t += 1.;
        }
        Track::Path { paths: [("pattern".to_string(), pattern), ("taxi".to_string(), taxi)].into_iter().collect() }
    }

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
                tier: Some("hard".into()),
                gps_denied: Some("Jammer North".into()),
            }),
            Some(Track::Weapon { points: vec![] }),
        )
    }

    pub(crate) fn all_cards() -> Vec<RangeRecord> {
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
        let mut v = vec![trap(), no_track_trap, bomb(), strafe, aar, missile, sling];
        v.extend(new_kinds());
        v.extend(more_kinds());
        v
    }

    /// The disciplines the forms cover that the lists above do not: BFM,
    /// anti-ship, troops, gunnery and CAS, with tracks and positions.
    pub(crate) fn more_kinds() -> Vec<RangeRecord> {
        let (lat0, lon0) = (42.1f64, 44.9f64);
        let at = |e: f64, n: f64, alt: f64, t: f64| TrackPt {
            t,
            lat: lat0 + n / 111_320.,
            lon: lon0 + e / (111_320. * lat0.to_radians().cos()),
            alt_m: alt,
            speed_kts: 400.,
        };
        let geo = |e: f64, n: f64| GeoPt { lat: lat0 + n / 111_320., lon: lon0 + e / (111_320. * lat0.to_radians().cos()), alt_m: 0. };
        // two fighters turning round each other
        let me: Vec<TrackPt> = (0..90).map(|i| { let a = i as f64 * 0.09; at(a.cos() * 2500., a.sin() * 2500. - 4000. + i as f64 * 20., 6000. - i as f64 * 25., i as f64) }).collect();
        let bandit: Vec<TrackPt> = (0..90).map(|i| { let a = i as f64 * 0.09 + 2.4; at(a.cos() * 2300., a.sin() * 2300. - 3800. + i as f64 * 20., 5800. - i as f64 * 22., i as f64) }).collect();
        let bfm = base(
            RangeResult::Engagement(EngagementResult {
                setup: "Neutral merge".into(),
                adversary: "MiG-29S".into(),
                adversary_skill: "excellent".into(),
                opponent: None,
                outcome: EngagementOutcome::Win,
                duration_s: 151.,
                shots_fired: 3,
                trainer_kills: 1,
                gun_hits: 12,
                notes: vec!["Rate fight won in the second circle".into(), "Gun kill at 600 m".into()],
            }),
            Some(Track::Path { paths: [("Casper <&> 1".to_string(), me), ("MiG-29S".to_string(), bandit)].into_iter().collect() }),
        );
        // a sea-skimmer from 40 nm
        let wpn: Vec<TrackPt> = (0..60).map(|i| { let f = i as f64 / 59.; at(-70_000. * (1. - f), 15_000. * (1. - f), if f < 0.1 { 3000. * (1. - f * 10.) + 30. } else { 15. }, i as f64 * 4.) }).collect();
        let antiship = base(
            RangeResult::AntiShip(AntiShipResult {
                ship: "Molniya 3".into(),
                ship_type: "MOLNIYA".into(),
                weapon: "AGM-84D Harpoon".into(),
                launch_range_m: 71_600.,
                weapon_max_range_m: Some(124_000.),
                hit: true,
                damage: 0.85,
                ship_sunk: true,
                time_of_flight_s: 236.,
                intercepted: false,
                launch_pos: geo(-70_000., 15_000.),
                ship_pos: geo(0., 0.),
            }),
            Some(Track::Weapon { points: wpn }),
        );
        let troops = base(
            RangeResult::Troops(TroopResult { lz: "LZ Tango".into(), troops: 10, load_time_s: 95., total_time_s: 540., landing_distance_m: 15., quality: PrecisionQuality::Excellent }),
            None,
        );
        let gunnery = base(
            RangeResult::Gunnery(GunneryResult { lane: "Lane 3".into(), targets_total: 12, targets_killed: 9, shots: 21, hits: 11, time_s: 248., first_round_hits: 6 }),
            None,
        );
        let cas = base(
            RangeResult::Cas(CasResult {
                jtac: "Axeman".into(),
                target: "T-72B in the treeline".into(),
                weapon: "GBU-12".into(),
                time_to_impact_s: 74.,
                miss_m: 6.,
                correct_target: true,
                danger_close: true,
                nearest_friendly_m: Some(420.),
                laser_code: Some(1688),
            }),
            None,
        );
        let airdrop = RangeRecord {
            unit_type: "C-130J-30".into(),
            ..base(
                RangeResult::Airdrop(AirdropResult {
                    method: "airdrop".into(),
                    zone: "Northwest Field".into(),
                    cargo: "CDS bundle".into(),
                    mass_kg: 1000.,
                    distance_m: 86.,
                    bearing_deg: 62.,
                    long_m: Some(70.),
                    cross_m: Some(50.),
                    quality: PrecisionQuality::Excellent,
                    damage: 0.,
                    release: Some(AirdropRelease { pos: geo(-900., -480.), alt_agl_ft: 650., alt_msl_ft: 1180., gs_kts: 132., heading_deg: 62., to_dz_m: Some(1020.) }),
                    fall_s: Some(31.),
                    drift_m: Some(1105.),
                    flight_s: 1840.,
                    dz_pos: geo(0., 0.),
                    impact_pos: geo(76., 40.),
                }),
                None,
            )
        };
        vec![bfm, antiship, troops, gunnery, cas, airdrop]
    }

    /// One record of every discipline added with the SEAD / hot zone /
    /// low-level / field landing / CSAR sectors, plus a deck landing.
    pub(crate) fn new_kinds() -> Vec<RangeRecord> {
        let sead = base(
            RangeResult::Sead(SeadResult {
                network: "S-1 Gardabani IADS".into(),
                site: "SA-6 battery".into(),
                system: "SA-6 Kub".into(),
                unit_type: "Kub 1S91 str".into(),
                role: "track radar".into(),
                weapon: "AGM_88C".into(),
                guidance: "arm".into(),
                launch_range_m: Some(38000.),
                site_was_emitting: true,
                site_destroyed: true,
                shots_at_you: 2,
                trainer_deaths: 1,
                site_pos: GeoPt { lat: 41.4, lon: 45.0, alt_m: 300. },
                launch_pos: None,
            }),
            None,
        );
        let hz = base(
            RangeResult::HotZone(HotZoneResult {
                zone: "HZ-1 Kakheti".into(),
                time_in_zone_s: 1260.,
                air_kills: 2,
                ground_kills: 3,
                kills: vec!["MiG-29S".into(), "Su-27".into(), "T-72B".into(), "ZSU-23-4 Shilka".into(), "Ural-375".into()],
                shots_fired: 9,
                missiles_defeated: 3,
                trainer_deaths: 0,
                outcome: HotZoneOutcome::Egressed,
            }),
            None,
        );
        let gates: Vec<GateTime> = (0..6)
            .map(|i| GateTime {
                gate: format!("G{}", i + 1),
                t: if i == 3 { None } else { Some(i as f64 * 62. + (i as f64 * 1.7)) },
                planned_t: i as f64 * 60.,
                agl_ft: if i == 3 { None } else { Some(220. + i as f64 * 70.) },
                pos: GeoPt::default(),
            })
            .collect();
        let ll = base(
            RangeResult::LowLevel(LowLevelResult {
                route: "LL-1 Alazani valley".into(),
                gates_hit: 5,
                gates_total: 6,
                gates,
                time_s: 318.,
                planned_s: 300.,
                tot_error_s: 18.,
                avg_agl_ft: 380.,
                max_agl_ft: 640.,
                min_agl_ft: 140.,
                max_allowed_agl_ft: 500.,
                min_allowed_agl_ft: 100.,
                pct_below_ceiling: 91.,
                below_floor_s: 0.,
                avg_speed_kts: 405.,
                quality: PrecisionQuality::Fair,
                calls: vec!["Missed gate G4".into(), "Late at the last gate".into()],
            }),
            None,
        );
        let fl = base(
            RangeResult::FieldLanding(FieldLandingResult {
                airfield: "Vaziani".into(),
                runway: "31".into(),
                outcome: FieldLandingOutcome::FullStop,
                touchdown_from_threshold_m: 480.,
                aim_error_m: 180.,
                centreline_m: -4.2,
                touchdown_fpm: 520.,
                touchdown_gs_kts: 142.,
                gs_error_1nm_deg: Some(0.6),
                gs_error_half_nm_deg: Some(0.4),
                lineup_1nm_deg: Some(-0.5),
                lineup_half_nm_deg: None,
                stable: true,
                quality: PrecisionQuality::Good,
                calls: vec!["A little high at 1 nm".into()],
                touchdown_pos: GeoPt { lat: 41.6283, lon: 45.0347, alt_m: 460. },
                runway_heading_deg: 315.,
                threshold_pos: GeoPt { lat: 41.6283, lon: 45.0347, alt_m: 460. },
                aim_point_m: 300.,
                runway_length_m: 2400.,
                runway_width_m: 45.,
                pattern: Some(FieldPattern {
                    kind: "overhead".into(),
                    side: Some("left".into()),
                    initial_alt_ft: Some(1500.),
                    break_alt_ft: Some(1480.),
                    break_at_m: Some(900.),
                    downwind_alt_ft: Some(1000.),
                    abeam_nm: Some(1.2),
                    abeam_alt_ft: Some(1000.),
                    base_at_m: Some(-1200.),
                    rollout_nm: Some(0.6),
                    rollout_alt_ft: Some(330.),
                    final_gs_kts: Some(135.),
                    pattern_time_s: Some(150.),
                    calls: vec![],
                }),
                taxi: Some(FieldTaxi {
                    rollout_m: 1050.,
                    rollout_s: 18.,
                    runway_clear_s: Some(27.),
                    exit_at_m: Some(1580.),
                    taxi_time_s: 82.,
                    taxi_distance_m: 640.,
                    max_taxi_kts: 38.,
                    stops: 1,
                    end: "parked".into(),
                    calls: vec!["fast taxi, 38 kt".into()],
                }),
            }),
            Some(field_track()),
        );
        let short = base(
            RangeResult::FieldLanding(FieldLandingResult {
                airfield: "Vaziani".into(),
                runway: "13".into(),
                outcome: FieldLandingOutcome::Undershoot,
                touchdown_from_threshold_m: -60.,
                aim_error_m: -360.,
                centreline_m: 30.,
                touchdown_fpm: 1100.,
                touchdown_gs_kts: 150.,
                gs_error_1nm_deg: None,
                gs_error_half_nm_deg: None,
                lineup_1nm_deg: None,
                lineup_half_nm_deg: None,
                stable: false,
                quality: PrecisionQuality::Poor,
                calls: vec![],
                touchdown_pos: GeoPt::default(),
                runway_heading_deg: 95.,
                threshold_pos: GeoPt::default(),
                aim_point_m: 300.,
                runway_length_m: 2400.,
                runway_width_m: 45.,
                pattern: None,
                taxi: None,
            }),
            None,
        );
        let csar = base(
            RangeResult::Csar(CsarResult {
                area: "CS-1 Tianeti".into(),
                hostile: true,
                outcome: CsarOutcome::Rescued,
                time_to_pickup_s: Some(840.),
                time_total_s: Some(1500.),
                pickup_method: "landed".into(),
                delivered_to: Some("Vaziani".into()),
                survivor_pos: GeoPt { lat: 42.1, lon: 44.9, alt_m: 900. },
                quality: PrecisionQuality::Excellent,
            }),
            None,
        );
        let deck = base(
            RangeResult::Landing(LandingResult {
                drill: "ship".into(),
                pad: "FFG-7 Perry".into(),
                distance_m: 1.1,
                touchdown_fpm: 240.,
                heading_error_deg: Some(3.),
                hover_s: 12.,
                quality: PrecisionQuality::Excellent,
                pad_pos: GeoPt::default(),
                touchdown_pos: GeoPt::default(),
                ship_speed_kts: Some(12.),
            }),
            None,
        );
        vec![sead, hz, ll, fl, short, csar, deck]
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
    fn new_kinds_round_trip_and_title() {
        for rec in new_kinds() {
            let js = serde_json::to_value(&rec).unwrap();
            assert_eq!(js["result"]["kind"], rec.kind());
            let back: RangeRecord = serde_json::from_value(js).unwrap();
            assert_eq!(back.kind(), rec.kind());
            let (title, _) = discord::title_and_color(&rec);
            assert!(!title.is_empty());
            assert!(!discord::fields(&rec).is_empty(), "{}", rec.kind());
        }
        let b = bomb();
        let f = discord::fields(&b);
        // the weapon block carries the tier and the GPS-denied note
        let w = &f.iter().find(|(k, _, _)| k == "Weapon").unwrap().1;
        assert!(w.contains("tier HARD") && w.contains("**GPS denied**"), "{w}");
    }

    #[test]
    fn nice_numbers() {
        assert_eq!(nice_ceil(37.), 50.);
        assert_eq!(nice_ceil(46.), 50.);
        assert_eq!(nice_ceil(120.), 200.);
        assert_eq!(nice_step(100., 5.), 20.);
    }
}
