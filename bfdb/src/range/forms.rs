// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Result cards as the forms the services use: black ink on paper, a
//! classification line, a numbered form header, boxed fields, and the
//! technical drawings a debrief room pins up.
//!
//! * [`trap_form`] -- the LSO pass record: ship and pilot blocks, the grade
//!   and the wire box, the LSO's shorthand as written and in plain words, a
//!   position-by-position table (X / IM / IC / AR / IW) worked out from the
//!   groove track, and the carrier as a wireframe in side elevation, in plan
//!   and in deck detail, with the pass drawn on it to scale
//!   ([`super::deckplan`]).
//! * [`bomb_form`] -- the range score sheet: release parameters, the score,
//!   the target diagram with its scoring rings, the attack axis and the
//!   impact, and the fall seen from the side.
//! * [`field_form`] -- the field landing and pattern record: the pattern in
//!   plan on the runway frame, each leg coloured the way the engine read it
//!   ([`bfprotocols::range::pattern_legs`]), height against time, the final
//!   against a 3° glidepath, and the rollout and the taxi.
//! * [`frame`] -- the same form chrome around every other card.

use super::{
    cards::{bomb_color, esc, grade_color, n, precision_color, tier_color, trunc, Anchor, Svg, BLUE, GREEN, GRID, INK, MUTED, ORANGE, PANEL, RED, SAND, TEXT, YELLOW},
    deckplan::{plan_for, Plan, P},
    discord,
};
use bfprotocols::range::{
    lso,
    pattern_legs::{self, Leg, RelSample},
    BombResult, FieldLandingOutcome, FieldLandingResult, GradeSource, GrooveSample, PassOutcome, RangeRecord, Track, TrackPt,
    TrapResult,
};
use std::fmt::Write;

const FT: f64 = 3.28084;
const NM: f64 = 1852.;
pub(crate) const MONO: &str = "Consolas, 'Cascadia Mono', 'Courier New', monospace";
pub(crate) const HAND: &str = "'Segoe Print', 'Bradley Hand', 'Comic Sans MS', cursive";
const HEAD_H: f64 = 104.;
pub(crate) const FOOT_H: f64 = 44.;
const DECK_FILL: &str = "#dde3e8";

// ── form chrome ─────────────────────────────────────────────────────────

pub(crate) fn text_f(s: &mut Svg, x: f64, y: f64, t: &str, size: f64, fill: &str, anchor: Anchor, bold: bool, family: &str) {
    let a = match anchor {
        Anchor::Start => "start",
        Anchor::Middle => "middle",
        Anchor::End => "end",
    };
    let wgt = if bold { r#" font-weight="bold""# } else { "" };
    let _ = write!(
        s.b,
        r#"<text x="{:.1}" y="{:.1}" font-size="{size}" fill="{fill}" text-anchor="{a}" font-family="{family}"{wgt}>{}</text>"#,
        n(x),
        n(y),
        esc(t)
    );
}

fn classification(s: &mut Svg, y: f64) {
    let w = s.w;
    s.rect(0., y, w, 20., "#e5dfcd", None, 0.);
    s.text(w / 2., y + 14., "UNCLASSIFIED // FOR TRAINING USE ONLY", 11., RED, Anchor::Middle, true);
}

/// The form header: classification, title, the form number box. Ends at `HEAD_H`.
pub(crate) fn head(s: &mut Svg, rec: &RangeRecord, title: &str, sub: &str, form_no: &str) {
    let w = s.w;
    classification(s, 0.);
    s.text(24., 54., title, 27., TEXT, Anchor::Start, true);
    s.text(24., 76., sub, 12.5, MUTED, Anchor::Start, false);
    let bx = w - 300.;
    s.rect(bx, 30., 276., 56., "none", Some((TEXT, 1.2)), 0.);
    s.text(bx + 8., 45., form_no, 10.5, MUTED, Anchor::Start, true);
    text_f(s, bx + 8., 64., &format!("SER {}", trunc(&rec.id, 30)), 12., TEXT, Anchor::Start, false, MONO);
    let place = if rec.theatre.is_empty() { "VECTOR STRIKE RANGE".to_string() } else { rec.theatre.to_uppercase() };
    s.text(bx + 8., 80., &trunc(&format!("{place} · {}", rec.mission_date), 42), 10.5, MUTED, Anchor::Start, false);
    s.line(24., 93., w - 24., 93., TEXT, 2.2, None);
    s.line(24., 97., w - 24., 97., TEXT, 0.6, None);
}

pub(crate) fn foot(s: &mut Svg, rec: &RangeRecord) {
    let (w, h) = (s.w, s.h);
    let y = h - FOOT_H;
    s.line(24., y + 4., w - 24., y + 4., TEXT, 0.8, None);
    s.text(24., y + 19., "Graded by the Vector Strike Range engine · range.vectorstrike.org", 10.5, MUTED, Anchor::Start, false);
    text_f(s, w - 24., y + 19., &rec.ts.format("%Y-%m-%d %H:%MZ").to_string(), 10.5, MUTED, Anchor::End, false, MONO);
    classification(s, h - 20.);
}

/// A boxed field: small caps label top-left, the value under it.
pub(crate) fn field(s: &mut Svg, x: f64, y: f64, w: f64, h: f64, label: &str, value: &str, size: f64, color: &str, mono: bool) {
    s.rect(x, y, w, h, "none", Some((TEXT, 1.)), 0.);
    s.text(x + 6., y + 12., label, 9.5, MUTED, Anchor::Start, true);
    let max = ((w - 12.) / (size * 0.56)).max(4.) as usize;
    if mono {
        text_f(s, x + 6., y + h - 9., &trunc(value, max), size, color, Anchor::Start, true, MONO);
    } else {
        s.text(x + 6., y + h - 9., &trunc(value, max), size, color, Anchor::Start, true);
    }
}

pub(crate) fn panel_title(s: &mut Svg, x: f64, y: f64, title: &str, note: &str) {
    s.text(x, y, title, 12., TEXT, Anchor::Start, true);
    let w = s.w;
    if !note.is_empty() {
        s.text(w - 30., y, note, 10.5, MUTED, Anchor::End, false);
    }
}

/// Any card body, framed as a form: the body moves down under the header.
pub(crate) fn frame(body: Svg, rec: &RangeRecord, title: &str, form_no: &str) -> String {
    let (w, h) = (body.w, body.h);
    let mut s = Svg::new(w, h + HEAD_H + FOOT_H - 10.);
    head(&mut s, rec, title, &header_sub(rec), form_no);
    let _ = write!(s.b, r#"<g transform="translate(0 {HEAD_H})">{}</g>"#, body.inner());
    foot(&mut s, rec);
    s.finish()
}

fn header_sub(rec: &RangeRecord) -> String {
    format!(
        "{} · {} · {} · mission {} {}",
        rec.pilot.name,
        if rec.callsign.is_empty() { "-" } else { &rec.callsign },
        discord::short_airframe(&rec.unit_type),
        rec.mission_date,
        rec.mission_time
    )
}

// ── carrier drawings ────────────────────────────────────────────────────

fn poly(pts: &[P]) -> String {
    let mut o = String::new();
    for (x, y) in pts {
        let _ = write!(o, "{:.1},{:.1} ", n(*x), n(*y));
    }
    o.trim_end().to_string()
}

/// The ship in plan, landing frame → page through `tx`/`ty`. `detail` adds labels.
fn draw_plan(s: &mut Svg, p: &Plan, tx: &dyn Fn(f64) -> f64, ty: &dyn Fn(f64) -> f64, detail: bool, caught: Option<u8>, clip: &str) {
    let pg = |q: P| {
        let l = p.to_landing(q);
        (tx(l.0), ty(l.1))
    };
    let _ = write!(s.b, r#"<g clip-path="url(#{clip})">"#);
    // hull at the waterline (hidden line), then the flight deck over it
    let wl: Vec<P> = p.waterline.iter().map(|q| pg(*q)).collect();
    let _ = write!(s.b, r#"<polygon points="{}" fill="none" stroke="{INK}" stroke-width="0.8" stroke-dasharray="4 3"/>"#, poly(&wl));
    let ol: Vec<P> = p.outline.iter().map(|q| pg(*q)).collect();
    let _ = write!(s.b, r#"<polygon points="{}" fill="{DECK_FILL}" stroke="{INK}" stroke-width="1.4" stroke-linejoin="round"/>"#, poly(&ol));
    // landing area: edge lines, centreline, foul line
    let hw = p.landing_half_w;
    let la = |d: f64, off: f64| pg(p.on_landing(d, off));
    let (a0, a1) = (la(0., -hw), la(p.landing_len, -hw));
    s.line(a0.0, a0.1, a1.0, a1.1, INK, 1., None);
    let (b0, b1) = (la(0., hw), la(p.landing_len, hw));
    s.line(b0.0, b0.1, b1.0, b1.1, INK, 1., None);
    let (c0, c1) = (la(4., 0.), la(p.landing_len, 0.));
    s.line(c0.0, c0.1, c1.0, c1.1, INK, 0.8, Some("8 5"));
    if p.g.angle_deg != 0. {
        let (f0, f1) = (la(-4., hw + 5.), la(p.landing_len, hw + 5.));
        s.line(f0.0, f0.1, f1.0, f1.1, RED, 0.9, Some("3 3"));
    }
    // ramp stripes
    for i in 0..9 {
        let o = -hw + i as f64 * (2. * hw) / 8.;
        let (r0, r1) = (la(0., o), la(2.5, o));
        if i % 2 == 0 {
            s.line(r0.0, r0.1, r1.0, r1.1, INK, 1.2, None);
        }
    }
    // wires
    for (i, d) in p.g.wires_m.iter().enumerate().filter(|(_, d)| **d > 0.) {
        let (w0, w1) = (la(*d, -hw - 1.), la(*d, hw + 1.));
        let hit = caught == Some(i as u8 + 1);
        s.line(w0.0, w0.1, w1.0, w1.1, if hit { RED } else { INK }, if hit { 2.6 } else { 1.1 }, None);
        if detail {
            let l = la(*d, -hw - 4.5);
            s.text(l.0, l.1 + 4., &format!("{}", i + 1), 10., if hit { RED } else { INK }, Anchor::Middle, true);
        }
    }
    // catapults: bow two along the axis, waist two along the angled deck;
    // each label at its middle, out on the side away from its pair
    let mids: Vec<P> = p.cats.iter().map(|(a, b)| {
        let (p0, p1) = (pg(*a), pg(*b));
        ((p0.0 + p1.0) / 2., (p0.1 + p1.1) / 2.)
    }).collect();
    for (k, (a, b)) in p.cats.iter().enumerate() {
        let (x0, y0) = pg(*a);
        let (x1, y1) = pg(*b);
        s.line(x0, y0, x1, y1, INK, 0.9, None);
        s.circle(x0, y0, 1.6, INK, None, None);
        if detail {
            let len = (x1 - x0).hypot(y1 - y0).max(1e-6);
            let (mut nx, mut ny) = (-(y1 - y0) / len, (x1 - x0) / len);
            let (mx, my) = mids[k];
            if let Some(&(ox, oy)) = mids.get(k ^ 1) {
                if (ox - mx) * nx + (oy - my) * ny > 0. {
                    (nx, ny) = (-nx, -ny);
                }
            }
            s.text(mx + nx * 7., my + ny * 7. + 3., &format!("CAT {}", k + 1), 8.5, INK, Anchor::Middle, false);
        }
    }
    // ski-jump take-off runs (dashed, from the start spots to the bow)
    for (a, b) in &p.runs {
        let (x0, y0) = pg(*a);
        let (x1, y1) = pg(*b);
        s.line(x0, y0, x1, y1, INK, 0.9, Some("6 4"));
        s.circle(x0, y0, 2.2, "none", Some((INK, 0.9)), None);
    }
    if let Some((a, b)) = p.ski_jump {
        // the ramp, hatched across the bow
        for k in 0..6 {
            let f = a + (b - a) * k as f64 / 5.;
            let (x0, y0) = pg((f, -16.));
            let (x1, y1) = pg((f, 14.));
            s.line(x0, y0, x1, y1, INK, 0.5, Some("2 3"));
        }
        if detail {
            let (x, y) = pg(((a + b) / 2., 0.));
            s.text(x, y + 3., "SKI JUMP", 8.5, INK, Anchor::Middle, true);
        }
    }
    // helicopter / VTOL spots
    for (k, q) in p.spots.iter().enumerate() {
        let (x, y) = pg(*q);
        let r = if detail { 7.5 } else { 3. };
        s.circle(x, y, r, "none", Some((INK, 0.9)), None);
        if detail {
            s.text(x, y + 3.5, &format!("{}", k + 1), 9., INK, Anchor::Middle, true);
        }
    }
    // elevators
    for (k, e) in p.elevators.iter().enumerate() {
        let pts: Vec<P> = e.iter().map(|q| pg(*q)).collect();
        let _ = write!(s.b, r#"<polygon points="{}" fill="none" stroke="{INK}" stroke-width="0.9"/>"#, poly(&pts));
        if detail {
            let cx = pts.iter().map(|q| q.0).sum::<f64>() / pts.len().max(1) as f64;
            let cy = pts.iter().map(|q| q.1).sum::<f64>() / pts.len().max(1) as f64;
            s.text(cx, cy + 3.5, &format!("{}", k + 1), 9., INK, Anchor::Middle, false);
        }
    }
    // island
    let isl: Vec<P> = p.island.iter().map(|q| pg(*q)).collect();
    let _ = write!(s.b, r#"<polygon points="{}" fill="{INK}" fill-opacity="0.18" stroke="{INK}" stroke-width="1.2"/>"#, poly(&isl));
    let top: Vec<P> = p.island_top.iter().map(|q| pg(*q)).collect();
    let _ = write!(s.b, r#"<polygon points="{}" fill="none" stroke="{INK}" stroke-width="0.8"/>"#, poly(&top));
    // lens and LSO platform
    if let Some(l) = p.lens {
        let (x, y) = pg(l);
        s.rect(x - 2.5, y - 2.5, 5., 5., ORANGE, Some((INK, 0.8)), 0.);
        if detail {
            s.text(x, y - 6., "LENS", 8.5, INK, Anchor::Middle, true);
        }
    }
    if let Some(l) = p.lso {
        let (x, y) = pg(l);
        s.rect(x - 3., y - 3., 6., 6., GREEN, Some((INK, 0.8)), 0.);
        if detail {
            s.text(x, y - 6., "LSO", 8.5, INK, Anchor::Middle, true);
        }
    }
    if let (Some(h), true) = (&p.hull_no, p.spots.is_empty()) {
        let (x, y) = pg((p.length * 0.86, -1.));
        s.text(x, y + 5., h, if detail { 16. } else { 10. }, INK, Anchor::Middle, true);
    }
    s.raw("</g>");
}

/// The ship in side elevation (seen from port). `pg` takes a ship-frame
/// (f, z above the waterline) point to the page; `detail` adds the
/// dimension lines of a technical drawing.
fn draw_side(s: &mut Svg, p: &Plan, pg: &dyn Fn(P) -> P, detail: bool, clip: &str) {
    let dh = p.g.deck_height_m;
    let _ = write!(s.b, r#"<g clip-path="url(#{clip})">"#);
    // the sea, then the keel as a hidden line
    let (w0, w1) = (pg((-2000., 0.)), pg((p.length + 2000., 0.)));
    if detail {
        let below = pg((0., -p.draft - 4.)).1;
        s.rect(w0.0.min(w1.0), w0.1, (w1.0 - w0.0).abs(), (below - w0.1).max(0.), "#dfe8ef", None, 0.);
    }
    let (k0, k1) = (pg((p.length * 0.06, -p.draft)), pg((p.length * 0.93, -p.draft)));
    s.line(k0.0, k0.1, k1.0, k1.1, INK, 0.8, Some("4 3"));
    if p.side_silhouette.len() > 4 {
        let sil: Vec<P> = p.side_silhouette.iter().map(|q| pg(*q)).collect();
        let _ = write!(s.b, r#"<polygon points="{}" fill="{DECK_FILL}" stroke="{INK}" stroke-width="1.1" stroke-linejoin="round"/>"#, poly(&sil));
    } else {
        let hull: Vec<P> = p.side_hull.iter().map(|q| pg(*q)).collect();
        let _ = write!(s.b, r#"<polygon points="{}" fill="{DECK_FILL}" stroke="{INK}" stroke-width="1.2" stroke-linejoin="round"/>"#, poly(&hull));
        for part in &p.side_island {
            let pts: Vec<P> = part.iter().map(|q| pg(*q)).collect();
            if pts.len() == 2 {
                s.line(pts[0].0, pts[0].1, pts[1].0, pts[1].1, INK, 1., None);
            } else {
                let _ = write!(s.b, r#"<polygon points="{}" fill="{DECK_FILL}" stroke="{INK}" stroke-width="1"/>"#, poly(&pts));
            }
        }
    }
    // the flight deck edge, the gallery deck under it, the hangar deck
    let (d0, d1) = (pg((0., dh)), pg((p.length, dh)));
    s.line(d0.0, d0.1, d1.0, d1.1, INK, 1.6, None);
    let (g0, g1) = (pg((p.length * 0.01, dh - 2.6)), pg((p.length * 0.99, dh - 2.6)));
    s.line(g0.0, g0.1, g1.0, g1.1, INK, 0.5, None);
    let (h0, h1) = (pg((p.length * 0.05, dh * 0.62)), pg((p.length * 0.95, dh * 0.62)));
    s.line(h0.0, h0.1, h1.0, h1.1, INK, 0.6, Some("6 4"));
    for (a, b) in &p.side_elevators {
        let (x0, y0) = pg((*a, dh));
        let (x1, y1) = pg((*b, dh * 0.66));
        s.rect(x0.min(x1), y0.min(y1), (x1 - x0).abs(), (y1 - y0).abs(), "none", Some((INK, 0.9)), 0.);
    }
    if let (Some(h), false) = (&p.hull_no, p.island.is_empty()) {
        let f = p.island.iter().map(|q| q.0).sum::<f64>() / p.island.len() as f64;
        let (x, y) = pg((f, dh + 7.));
        s.text(x, y + 4., h, if detail { 12. } else { 10. }, INK, Anchor::Middle, true);
    }
    s.line(w0.0, w0.1, w1.0, w1.1, BLUE, 1., None);
    if detail {
        // length overall, under the keel
        let y = pg((0., -p.draft - 2.)).1;
        let (a, b) = (pg((0., 0.)).0, pg((p.length, 0.)).0);
        s.line(a, y, b, y, INK, 0.7, None);
        s.line(a, y - 4., a, y + 4., INK, 0.7, None);
        s.line(b, y - 4., b, y + 4., INK, 0.7, None);
        s.text((a + b) / 2., y - 3., &format!("LOA {:.0} m", p.length), 9.5, INK, Anchor::Middle, true);
        // flight deck above the waterline, at the stern
        let (sx, sy0) = pg((0., 0.));
        let sy1 = pg((0., dh)).1;
        let sx = sx + (pg((1., 0.)).0 - sx).signum() * -10.;
        s.line(sx, sy0, sx, sy1, INK, 0.7, None);
        s.line(sx - 3., sy0, sx + 3., sy0, INK, 0.7, None);
        s.line(sx - 3., sy1, sx + 3., sy1, INK, 0.7, None);
        s.text(sx + (pg((1., 0.)).0 - pg((0., 0.)).0).signum() * -4., (sy0 + sy1) / 2. + 3., &format!("{dh:.1} m"), 9., INK, if pg((1., 0.)).0 > pg((0., 0.)).0 { Anchor::End } else { Anchor::Start }, false);
        // the masthead
        let top = p.side_silhouette.iter().chain(p.side_island.iter().flatten()).cloned().fold((0., 0.), |m: P, q| if q.1 > m.1 { q } else { m });
        if top.1 > dh {
            let (x, y) = pg(top);
            s.text(x + 6., y + 3., &format!("{:.0} m above the waterline", top.1), 9., INK, Anchor::Start, false);
        }
    }
    s.raw("</g>");
}

// ── the LSO pass record ─────────────────────────────────────────────────

/// LSO windows along the groove, metres aft of the landing point. AR runs to the ramp; IW is over the deck.
fn windows(p: &Plan) -> [(&'static str, f64, f64); 5] {
    let ramp = p.ramp_x();
    [("X", 0.75 * NM, 0.5 * NM), ("IM", 0.5 * NM, 0.25 * NM), ("IC", 0.25 * NM, 0.1 * NM), ("AR", 0.1 * NM, ramp), ("IW", ramp, -60.)]
}

fn gs_band(g: f64) -> &'static str {
    let a = g.abs();
    if a <= 0.4 {
        GREEN
    } else if a <= 0.8 {
        YELLOW
    } else if a <= 1.5 {
        ORANGE
    } else {
        RED
    }
}

fn lu_band(l: f64) -> &'static str {
    let a = l.abs();
    if a <= 0.5 {
        GREEN
    } else if a <= 1. {
        YELLOW
    } else if a <= 3. {
        ORANGE
    } else {
        RED
    }
}

/// The value at landing-frame `x`, interpolated along the groove.
fn at_x(samples: &[GrooveSample], x: f64, f: impl Fn(&GrooveSample) -> f64) -> Option<f64> {
    for w in samples.windows(2) {
        let (a, b) = (&w[0], &w[1]);
        if (a.x_m - x) * (b.x_m - x) <= 0. && a.x_m != b.x_m {
            let u = (x - a.x_m) / (b.x_m - a.x_m);
            return Some(f(a) + (f(b) - f(a)) * u);
        }
    }
    None
}

pub(crate) fn trap_form(rec: &RangeRecord, t: &TrapResult) -> String {
    const W: f64 = 1100.;
    const PX: f64 = 30.;
    const PW: f64 = 1040.;
    let p = plan_for(&t.carrier_type);
    let samples: Vec<GrooveSample> = match &rec.track {
        Some(Track::Groove { samples }) => samples.clone(),
        _ => vec![],
    };
    // the groove: from the aft-most point onward
    let gs_start = samples.iter().enumerate().max_by(|a, b| a.1.x_m.total_cmp(&b.1.x_m)).map(|(i, _)| i).unwrap_or(0);
    let groove: Vec<GrooveSample> = samples[gs_start.min(samples.len())..].to_vec();
    let ramp = p.ramp_x();
    let touchdown = groove.iter().find(|q| q.x_m < ramp && q.alt_ft <= 6.).copied();
    let ramp_alt = at_x(&groove, ramp, |q| q.alt_ft);
    let ramp_lu = at_x(&groove, ramp, |q| q.y_m);
    let caught = if t.outcome == PassOutcome::Trap { t.wire } else { None };

    let side_h = 300.;
    let plan_half = 190.;
    let x_fwd = p.bow_x() - 25.;
    let x_aft = 0.78 * NM;
    let sxm = PW / (x_aft - x_fwd);
    let plan_h = 2. * plan_half * sxm;
    let det_scale = 1.32;
    let det_fwd = p.bow_x() - 12.;
    let det_aft = det_fwd + PW / det_scale;
    let det_ys: Vec<f64> = p.outline.iter().map(|q| p.to_landing(*q).1).collect();
    let det_lo = det_ys.iter().cloned().fold(f64::INFINITY, f64::min).min(-75.) - 12.;
    let det_hi = det_ys.iter().cloned().fold(f64::NEG_INFINITY, f64::max).max(70.) + 12.;
    let det_h = (det_hi - det_lo) * det_scale;

    // the lens row, then the drawings under it
    let lens_y = 452.;
    let lens_h = 170.;
    let y_draw = lens_y + lens_h + 14.;
    // the side elevation under the deck detail: the same scale, no exaggeration
    let top_z = p.side_silhouette.iter().chain(p.side_island.iter().flatten()).map(|q| q.1).fold(p.g.deck_height_m + 20., f64::max);
    let (el_lo, el_hi) = (-(p.draft + 9.), top_z + 8.);
    let el_h = (el_hi - el_lo) * det_scale;
    let h = y_draw + 28. + side_h + 52. + plan_h + 52. + det_h + 52. + el_h + 40. + FOOT_H + 6.;
    let mut s = Svg::new(W, h);
    head(&mut s, rec, "LSO PASS RECORD", "CARRIER LANDING · VECTOR STRIKE TRAINING RANGE · CARRIER QUALIFICATION", "VSR FORM 3710/2 (REV 10-26)");

    // ── ship / pilot blocks ──
    let gx = W - 24. - 236.;
    let fw = (gx - 8. - 24.) / 6.;
    let brc = t.final_bearing_deg.map(|f| (f - p.g.angle_deg).rem_euclid(360.));
    let row_a: [(&str, String); 6] = [
        ("SHIP", t.carrier.clone()),
        ("CLASS", p.name.to_string()),
        ("CASE", ["I", "II", "III"].get(t.case.saturating_sub(1) as usize).map(|s| s.to_string()).unwrap_or_else(|| t.case.to_string())),
        ("LIGHT", if t.night { "NIGHT".into() } else { "DAY".into() }),
        ("BRC / FB", match (brc, t.final_bearing_deg) {
            (Some(b), Some(f)) => format!("{b:03.0} / {f:03.0}"),
            _ => "-".into(),
        }),
        ("WIND OVER DECK", t.wind_over_deck_kts.map(|w| format!("{w:.0} KT")).unwrap_or_else(|| "-".into())),
    ];
    for (i, (k, v)) in row_a.iter().enumerate() {
        let w = if i == 1 { fw * 1.25 } else if i == 0 { fw * 1.25 } else { fw * 0.875 };
        let x = 24. + row_a[..i].iter().enumerate().map(|(j, _)| if j <= 1 { fw * 1.25 } else { fw * 0.875 }).sum::<f64>();
        field(&mut s, x, 108., w, 46., k, v, 14., TEXT, i >= 2);
    }
    let row_b: [(&str, String); 6] = [
        ("PILOT", rec.pilot.name.clone()),
        ("CALLSIGN", if rec.callsign.is_empty() { "-".into() } else { rec.callsign.clone() }),
        ("A/C", discord::short_airframe(&rec.unit_type)),
        ("HOOK", match t.hook_down {
            Some(true) => "DOWN".into(),
            Some(false) => "UP".into(),
            None => "-".into(),
        }),
        ("GROOVE", t.groove_time_s.map(|g| format!("{g:.1} S")).unwrap_or_else(|| "-".into())),
        ("RESULT", t.outcome.label().to_uppercase()),
    ];
    for (i, (k, v)) in row_b.iter().enumerate() {
        let w = if i <= 1 { fw * 1.25 } else { fw * 0.875 };
        let x = 24. + (0..i).map(|j| if j <= 1 { fw * 1.25 } else { fw * 0.875 }).sum::<f64>();
        field(&mut s, x, 158., w, 46., k, v, 14., TEXT, i >= 3);
    }
    // the grade box
    let gc = grade_color(&t.grade);
    s.rect(gx, 108., 236., 96., "none", Some((TEXT, 1.6)), 0.);
    s.text(gx + 8., 121., "LSO GRADE", 9.5, MUTED, Anchor::Start, true);
    text_f(&mut s, gx + 118., 168., &t.grade, if t.grade.len() > 4 { 34. } else { 46. }, gc, Anchor::Middle, true, MONO);
    s.text(gx + 118., 190., &lso::grade_name(&t.grade).to_uppercase(), 11., gc, Anchor::Middle, true);
    let pts = t.points.map(|p| format!("{p:.1} PTS")).unwrap_or_else(|| "NO COUNT".into());
    s.text(gx + 228., 121., &pts, 10., TEXT, Anchor::End, true);
    s.text(gx + 228., 200., if t.source == GradeSource::Dcs { "GRADED BY DCS LSO" } else { "GRADED BY RANGE ENGINE" }, 8.5, MUTED, Anchor::End, false);

    // ── the LSO's comment, and the wire box ──
    let cw = gx - 8. - 24.;
    s.rect(24., 212., cw, 96., "none", Some((TEXT, 1.)), 0.);
    s.text(30., 224., "LSO COMMENTS", 9.5, MUTED, Anchor::Start, true);
    let comment = if t.lso_comment.trim().is_empty() { "-".to_string() } else { t.lso_comment.clone() };
    text_f(&mut s, 34., 256., &trunc(&comment, 46), 24., "#1a2f6e", Anchor::Start, false, HAND);
    let desc = if t.lso_description.is_empty() { lso::describe(&t.lso_comment) } else { t.lso_description.clone() };
    let mut lines: Vec<String> = vec![String::new()];
    for d in &desc {
        let cur = lines.last_mut().unwrap();
        if !cur.is_empty() && cur.chars().count() + d.chars().count() + 3 > 92 {
            lines.push(d.clone());
        } else {
            if !cur.is_empty() {
                cur.push_str(" · ");
            }
            cur.push_str(d);
        }
    }
    for (i, l) in lines.iter().take(2).enumerate() {
        s.text(34., 280. + i as f64 * 15., &trunc(l, 100), 11.5, TEXT, Anchor::Start, false);
    }
    if let Some(e) = &t.engine_grade {
        if e.grade != t.grade {
            s.text(24. + cw - 8., 224., &trunc(&format!("ENGINE: {} {}", e.grade, e.comment), 48), 9.5, MUTED, Anchor::End, false);
        }
    }
    s.rect(gx, 212., 236., 96., "none", Some((TEXT, 1.)), 0.);
    s.text(gx + 8., 224., "ARRESTING WIRE", 9.5, MUTED, Anchor::Start, true);
    for i in 0..4u8 {
        let bx = gx + 18. + i as f64 * 52.;
        let hit = caught == Some(i + 1);
        s.rect(bx, 232., 40., 34., if hit { "#f5d7d3" } else { "none" }, Some((TEXT, 1.)), 0.);
        text_f(&mut s, bx + 20., 256., &format!("{}", i + 1), 17., if hit { RED } else { TEXT }, Anchor::Middle, true, MONO);
        if hit {
            s.line(bx + 3., 235., bx + 37., 263., RED, 2., None);
            s.line(bx + 37., 235., bx + 3., 263., RED, 2., None);
        }
    }
    let flags = [
        ("TRAP", t.outcome == PassOutcome::Trap),
        ("BOLTER", t.outcome == PassOutcome::Bolter),
        ("WO", t.outcome == PassOutcome::Waveoff),
        ("OWO", t.outcome == PassOutcome::OwnWaveoff),
    ];
    for (i, (l, on)) in flags.iter().enumerate() {
        let bx = gx + 12. + i as f64 * 56.;
        s.rect(bx, 280., 10., 10., if *on { TEXT } else { "none" }, Some((TEXT, 1.)), 0.);
        s.text(bx + 14., 289., l, 9.5, TEXT, Anchor::Start, *on);
    }
    if t.wire.is_some() && !t.wire_from_dcs {
        s.text(gx + 228., 302., "wire estimated from the stop point", 8., MUTED, Anchor::End, false);
    }

    // ── position table and key numbers ──
    let tw = cw;
    s.rect(24., 316., tw, 124., "none", Some((TEXT, 1.)), 0.);
    s.text(30., 328., "GROOVE BY POSITION (from the track)", 9.5, MUTED, Anchor::Start, true);
    let wins = windows(&p);
    let col0 = 130.;
    let colw = (tw - col0 - 8.) / 5.;
    for (j, (k, _, _)) in wins.iter().enumerate() {
        let x = 24. + col0 + j as f64 * colw;
        s.line(x, 334., x, 436., GRID, 1., None);
        text_f(&mut s, x + colw / 2., 348., k, 13., TEXT, Anchor::Middle, true, MONO);
    }
    s.line(28., 354., 24. + tw - 4., 354., TEXT, 0.8, None);
    let rows: [(&str, &dyn Fn(&GrooveSample) -> f64); 4] =
        [("GLIDESLOPE °", &|q| q.gse_deg), ("LINEUP °", &|q| q.lue_deg), ("AOA °", &|q| q.aoa_deg.unwrap_or(f64::NAN)), ("CLOSURE KT", &|q| q.closure_kts)];
    for (ri, (label, f)) in rows.iter().enumerate() {
        let y = 372. + ri as f64 * 20.;
        s.text(30., y, label, 10.5, TEXT, Anchor::Start, true);
        for (j, (_, from, to)) in wins.iter().enumerate() {
            let vals: Vec<f64> = groove.iter().filter(|q| q.x_m <= *from && q.x_m > *to).map(|q| f(q)).filter(|v| v.is_finite()).collect();
            let x = 24. + col0 + j as f64 * colw + colw / 2.;
            if vals.is_empty() || (ri < 2 && j == 4) {
                s.text(x, y, "-", 11., MUTED, Anchor::Middle, false);
                continue;
            }
            let avg = vals.iter().sum::<f64>() / vals.len() as f64;
            let (txt, col) = match ri {
                0 => (format!("{avg:+.2} {}", if avg > 0.4 { "H" } else if avg < -0.4 { "LO" } else { "" }), gs_band(avg)),
                1 => (format!("{avg:+.2} {}", if avg > 0.5 { "R" } else if avg < -0.5 { "L" } else { "" }), lu_band(avg)),
                2 => (format!("{avg:.1}"), TEXT),
                _ => (format!("{avg:.0}"), TEXT),
            };
            text_f(&mut s, x, y, txt.trim_end(), 12., col, Anchor::Middle, true, MONO);
        }
    }
    s.rect(gx, 316., 236., 124., "none", Some((TEXT, 1.)), 0.);
    s.text(gx + 8., 328., "AT THE RAMP / DECK", 9.5, MUTED, Anchor::Start, true);
    let kv: [(&str, String); 5] = [
        ("Over the ramp", ramp_alt.map(|a| format!("{a:.0} ft")).unwrap_or_else(|| "-".into())),
        ("Lineup at ramp", ramp_lu.map(|y| format!("{:+.0} ft {}", y * FT, if y > 0. { "R" } else { "L" })).unwrap_or_else(|| "-".into())),
        ("Touchdown", touchdown.map(|q| format!("{:.0} ft {}", q.x_m.abs() * FT, if q.x_m > 0. { "short of 3" } else { "past 3" })).unwrap_or_else(|| "-".into())),
        ("Wake crossing", t.pattern.wake_alt_ft.map(|a| format!("{a:.0} ft")).unwrap_or_else(|| "-".into())),
        ("Groove time", t.groove_time_s.map(|g| format!("{g:.1} s{}", if g < 15. { " (short)" } else if g > 20. { " (long)" } else { "" })).unwrap_or_else(|| "-".into())),
    ];
    for (i, (k, v)) in kv.iter().enumerate() {
        let y = 348. + i as f64 * 19.;
        s.text(gx + 8., y, k, 10.5, MUTED, Anchor::Start, false);
        text_f(&mut s, gx + 228., y, v, 11.5, TEXT, Anchor::End, true, MONO);
    }

    // ── the lens at each position, as the pilot saw it ──
    panel_title(&mut s, PX, lens_y + 12., "THE LENS (IFLOLS) · the ball at each position", &format!("one cell ≈ {LENS_DEG_PER_CELL:.2}° · bottom two cells red · wave-off lights lit on a wave-off"));
    let lw = PW / 5.;
    let ly0 = lens_y + 22.;
    for (j, (k, from, to)) in wins.iter().enumerate().take(4) {
        let vals: Vec<f64> = groove.iter().filter(|q| q.x_m <= *from && q.x_m > *to).map(|q| q.gse_deg).collect();
        let g = (!vals.is_empty()).then(|| vals.iter().sum::<f64>() / vals.len() as f64);
        let x0 = PX + j as f64 * lw;
        s.rect(x0 + 3., ly0, lw - 6., lens_h - 10., PANEL, Some((TEXT, 1.)), 0.);
        text_f(&mut s, x0 + 12., ly0 + 18., k, 13., TEXT, Anchor::Start, true, MONO);
        let wo = matches!(t.outcome, PassOutcome::Waveoff) && j == 3;
        draw_lens(&mut s, x0 + lw / 2., ly0 + 84., 0.92, g, wo);
        let (txt, col) = match g {
            Some(g) => (format!("{g:+.2}° · {}", ball_say(g)), gs_band(g)),
            None => ("no reading".to_string(), MUTED),
        };
        text_f(&mut s, x0 + lw / 2., ly0 + lens_h - 20., &txt, 11., col, Anchor::Middle, true, MONO);
    }
    // the key
    let x0 = PX + 4. * lw;
    s.rect(x0 + 3., ly0, lw - 6., lens_h - 10., "none", Some((TEXT, 1.)), 0.);
    s.text(x0 + 12., ly0 + 18., "READING THE LENS", 9.5, MUTED, Anchor::Start, true);
    let key: [(&str, &str); 4] = [("#ffb000", "ball: above the datum is high"), ("#2fd16b", "datum lights: on glideslope"), ("#ff3b30", "wave-off lights: go around"), ("#24452f", "cut lights: power / roger ball")];
    for (i, (c, l)) in key.iter().enumerate() {
        let y = ly0 + 42. + i as f64 * 26.;
        s.circle(x0 + 20., y - 4., 5., c, Some((TEXT, 0.6)), None);
        s.text(x0 + 32., y, l, 10.5, TEXT, Anchor::Start, false);
    }

    // ── side elevation: glideslope ──
    let tx = |x: f64| PX + (x - x_fwd) * sxm;
    let sy0 = y_draw + 22.;
    let max_alt = groove.iter().filter(|q| q.x_m <= x_aft).map(|q| q.alt_ft).fold(0., f64::max);
    let alt_top = (max_alt * 1.12).clamp(450., 1000.).ceil();
    let alt_bot = -(p.g.deck_height_m * FT + 18.);
    let tz = |ft: f64| sy0 + side_h - (ft - alt_bot) / (alt_top - alt_bot) * side_h;
    let exag = (side_h / ((alt_top - alt_bot) / FT)) / sxm;
    panel_title(&mut s, PX, y_draw + 12., "SIDE ELEVATION · GLIDESLOPE", &format!("to scale along the final bearing · vertical exaggeration ×{exag:.1}"));
    s.clip("fside", PX, sy0, PW, side_h);
    s.rect(PX, sy0, PW, side_h, PANEL, Some((TEXT, 1.)), 0.);
    for (k, from, to) in windows(&p).iter().take(4) {
        let (a, b) = (tx(*from), tx(*to));
        s.rect(a, sy0, b - a, side_h, if *k == "IM" || *k == "AR" { "#e3ddcb" } else { "none" }, None, 0.);
        text_f(&mut s, (a + b) / 2., sy0 + 14., k, 11., MUTED, Anchor::Middle, true, MONO);
    }
    let step = if alt_top > 700. { 200. } else { 100. };
    let mut a = step;
    while a < alt_top {
        s.line(PX, tz(a), PX + PW, tz(a), GRID, 0.8, None);
        text_f(&mut s, PX + 4., tz(a) - 3., &format!("{a:.0}"), 9.5, MUTED, Anchor::Start, false, MONO);
        a += step;
    }
    for q in [0.25, 0.5, 0.75] {
        let x = tx(q * NM);
        s.line(x, sy0 + side_h - 8., x, sy0 + side_h, TEXT, 1., None);
        s.text(x, sy0 + side_h + 13., &format!("{q} NM"), 10., MUTED, Anchor::Middle, false);
    }
    let lp_label = match p.wires_x().len() {
        0 => "LP (deck spot)".to_string(),
        _ => format!("LP (target wire, {:.0} m from the ramp)", p.lp_from_stern()),
    };
    s.text(tx(0.), sy0 + side_h + 13., &lp_label, 10., MUTED, Anchor::Middle, false);
    let dh = p.g.deck_height_m;
    draw_side(&mut s, &p, &|q: P| (tx(p.side_x(q.0)), tz((q.1 - dh) * FT)), false, "fside");
    let gs = p.g.glideslope_deg;
    let gsl = |s: &mut Svg, d: f64, col: &str, dash: Option<&str>, w: f64| {
        let x1 = x_aft;
        let a1 = x1 * (gs + d).to_radians().tan() * FT;
        let dsh = dash.map(|d| format!(r#" stroke-dasharray="{d}""#)).unwrap_or_default();
        let _ = write!(s.b, r#"<line x1="{:.1}" y1="{:.1}" x2="{:.1}" y2="{:.1}" stroke="{col}" stroke-width="{w}"{dsh} clip-path="url(#fside)"/>"#, tx(0.), tz(0.), tx(x1), tz(a1));
    };
    for (d, col) in [(1.5, RED), (0.8, YELLOW), (0.4, GREEN)] {
        gsl(&mut s, d, col, Some("4 4"), 1.1);
        gsl(&mut s, -d, col, Some("4 4"), 1.1);
    }
    gsl(&mut s, 0., GREEN, None, 1.3);
    s.raw(r#"<g clip-path="url(#fside)">"#);
    for w in groove.windows(2) {
        let (a, b) = (&w[0], &w[1]);
        if b.x_m > x_aft + 50. {
            continue;
        }
        let col = if b.x_m > 25. { gs_band(b.gse_deg) } else { BLUE };
        s.line(tx(a.x_m), tz(a.alt_ft), tx(b.x_m), tz(b.alt_ft), col, 2.4, None);
    }
    if let Some(a) = ramp_alt {
        s.circle(tx(ramp), tz(a), 3.5, "none", Some((TEXT, 1.2)), None);
        s.text(tx(ramp) + 8., tz(a) - 8., &format!("ramp {a:.0} ft"), 10., TEXT, Anchor::Start, true);
    }
    if let Some(q) = touchdown {
        s.circle(tx(q.x_m), tz(0.), 4., "none", Some((RED, 1.6)), None);
    }
    s.raw("</g>");

    // ── plan view: lineup, to scale ──
    let py0 = sy0 + side_h + 52.;
    let pcy = py0 + plan_h / 2.;
    // seen from above: the stern is on the right, so the ship heads left and
    // its port side is down the page (starboard, + y, up)
    let ty = |y: f64| pcy - y * sxm;
    panel_title(&mut s, PX, py0 - 10., "PLAN VIEW · LINEUP", "to scale · seen from above · port down");
    s.clip("fplan", PX, py0, PW, plan_h);
    s.rect(PX, py0, PW, plan_h, PANEL, Some((TEXT, 1.)), 0.);
    s.raw(r#"<g clip-path="url(#fplan)">"#);
    for q in [0.25, 0.5, 0.75] {
        let r = q * NM * sxm;
        let _ = write!(s.b, r#"<circle cx="{:.1}" cy="{:.1}" r="{:.1}" fill="none" stroke="{GRID}" stroke-dasharray="2 4"/>"#, tx(0.), pcy, r);
        s.text(tx(q * NM) - 4., pcy - 5., &format!("{q} NM"), 9.5, MUTED, Anchor::End, false);
    }
    for (d, col) in [(3., RED), (1., YELLOW), (0.5, GREEN)] {
        let y = x_aft * f64::to_radians(d).tan();
        s.line(tx(ramp), ty(ramp * f64::to_radians(d).tan()), tx(x_aft), ty(y), col, 1., Some("3 4"));
        s.line(tx(ramp), ty(-ramp * f64::to_radians(d).tan()), tx(x_aft), ty(-y), col, 1., Some("3 4"));
    }
    s.line(tx(ramp), ty(0.), tx(x_aft), ty(0.), GREEN, 1., None);
    s.line(tx(ramp), ty(p.wake_y(ramp)), tx(x_aft), ty(p.wake_y(x_aft)), MUTED, 1., Some("1 4"));
    s.raw("</g>");
    draw_plan(&mut s, &p, &tx, &ty, false, caught, "fplan");
    s.raw(r#"<g clip-path="url(#fplan)">"#);
    for w in samples.windows(2) {
        let (a, b) = (&w[0], &w[1]);
        let col = if b.x_m > 25. && b.x_m < 0.8 * NM { lu_band(b.lue_deg) } else { BLUE };
        s.line(tx(a.x_m), ty(a.y_m), tx(b.x_m), ty(b.y_m), col, 2., None);
    }
    if let Some(alt) = t.pattern.wake_alt_ft {
        // where the track crosses the wake, aft of the ship
        let mut cross = None;
        for w in samples.windows(2) {
            let (a, b) = (&w[0], &w[1]);
            let (da, db) = (a.y_m - p.wake_y(a.x_m), b.y_m - p.wake_y(b.x_m));
            if a.x_m > 200. && da.signum() != db.signum() {
                cross = Some((a.x_m, a.y_m));
            }
        }
        if let Some((x, y)) = cross {
            s.circle(tx(x), ty(y), 4., "none", Some((TEXT, 1.4)), None);
            s.text(tx(x) + 7., ty(y) - 6., &format!("wake {alt:.0} ft"), 10., TEXT, Anchor::Start, true);
        }
    }
    s.raw("</g>");

    // ── deck detail ──
    let dy0 = py0 + plan_h + 52.;
    let dtx = |x: f64| PX + (x - det_fwd) * det_scale;
    let dty = |y: f64| dy0 + (det_hi - y) * det_scale;
    panel_title(&mut s, PX, dy0 - 10., &format!("DECK DETAIL · {}", p.name), "seen from above · port down · to scale · wires, catapults, elevators, lens, LSO");
    s.clip("fdeck", PX, dy0, PW, det_h);
    s.rect(PX, dy0, PW, det_h, PANEL, Some((TEXT, 1.)), 0.);
    draw_plan(&mut s, &p, &dtx, &dty, true, caught, "fdeck");
    s.raw(r#"<g clip-path="url(#fdeck)">"#);
    let near: Vec<(f64, f64)> = samples.iter().filter(|q| q.x_m < det_aft + 60.).map(|q| (dtx(q.x_m), dty(q.y_m))).collect();
    s.polyline(&near, BLUE, 2., None, None);
    if let Some(q) = touchdown {
        s.circle(dtx(q.x_m), dty(q.y_m), 5., "none", Some((RED, 2.)), None);
        s.text(dtx(q.x_m), dty(q.y_m) + 17., "TOUCHDOWN", 9., RED, Anchor::Middle, true);
    }
    // a 50 m scale bar
    let (bx, by) = (PX + PW - 90., dy0 + det_h - 14.);
    s.line(bx, by, bx + 50. * det_scale, by, TEXT, 2., None);
    s.text(bx + 25. * det_scale, by - 5., "50 m", 9.5, TEXT, Anchor::Middle, true);
    s.raw("</g>");

    // ── side elevation, to scale ──
    let ey0 = dy0 + det_h + 52.;
    let ez = |z: f64| ey0 + (el_hi - z) * det_scale;
    panel_title(&mut s, PX, ey0 - 10., &format!("SIDE ELEVATION · {} · from port", p.name), "to scale, same as the deck detail · no vertical exaggeration");
    s.clip("felev", PX, ey0, PW, el_h);
    s.rect(PX, ey0, PW, el_h, PANEL, Some((TEXT, 1.)), 0.);
    draw_side(&mut s, &p, &|q: P| (dtx(p.side_x(q.0)), ez(q.1)), true, "felev");
    // the jet's last moments over the ramp, at the same scale
    s.raw(r#"<g clip-path="url(#felev)">"#);
    let dhm = p.g.deck_height_m;
    let low: Vec<(f64, f64)> = groove.iter().filter(|q| q.x_m < det_aft + 60. && q.alt_ft < 400.).map(|q| (dtx(q.x_m), ez(dhm + q.alt_ft / FT))).collect();
    s.polyline(&low, BLUE, 1.8, None, None);
    s.raw("</g>");

    // legend
    let ly = ey0 + el_h + 22.;
    let mut x = PX;
    for (col, l) in [(GREEN, "on glideslope ±0.4° / lineup ±0.5°"), (YELLOW, "(a little) to ±0.8° / ±1°"), (ORANGE, "called to ±1.5° / ±3°"), (RED, "_a lot_ beyond"), (BLUE, "pattern / on deck")] {
        s.line(x, ly - 4., x + 20., ly - 4., col, 3., None);
        s.text(x + 26., ly, l, 10.5, MUTED, Anchor::Start, false);
        x += 210.;
    }
    if samples.is_empty() {
        s.text(W / 2., sy0 + side_h / 2., "no groove track recorded for this pass", 15., MUTED, Anchor::Middle, true);
    }
    foot(&mut s, rec);
    s.finish()
}

// ── the lens (IFLOLS) ───────────────────────────────────────────────────

/// Cells in the IFLOLS source and the glideslope error one cell stands for:
/// the ball runs about ±0.8° from the top cell to the bottom one.
pub(crate) const LENS_CELLS: usize = 12;
pub(crate) const LENS_DEG_PER_CELL: f64 = 1.6 / LENS_CELLS as f64;

/// Where the ball is, in cells from the top of the source (6 = the middle,
/// level with the datum lights), and whether it has gone off the lens.
pub(crate) fn ball_pos(gse: f64) -> (f64, bool) {
    let p = LENS_CELLS as f64 / 2. - gse / LENS_DEG_PER_CELL;
    (p, p < 0.25 || p > LENS_CELLS as f64 - 0.25)
}

/// The cells the ball lights: the one or two whose middles are nearest it
/// (on glideslope it straddles the datum, lighting the two middle cells).
pub(crate) fn lit_cells(p: f64) -> Vec<usize> {
    (0..LENS_CELLS).filter(|i| (p - (*i as f64 + 0.5)).abs() < 0.75).collect()
}

/// The ball in words: "centred", "1.5 cells high", "off the bottom".
pub(crate) fn ball_say(gse: f64) -> String {
    let (p, off) = ball_pos(gse);
    if off {
        return if gse > 0. { "off the top".into() } else { "off the bottom".into() };
    }
    let c = ((LENS_CELLS as f64 / 2. - p) * 2.).round() / 2.;
    let red = lit_cells(p).iter().any(|i| *i >= LENS_CELLS - 2);
    if c.abs() < 0.5 {
        return "centred".into();
    }
    let n = if c.fract() == 0. { format!("{:.0}", c.abs()) } else { format!("{:.1}", c.abs()) };
    format!("{n} cell{} {}{}", if c.abs() == 1. { "" } else { "s" }, if c > 0. { "high" } else { "low" }, if red { ", red" } else { "" })
}

/// The IFLOLS as the pilot sees it, centred on (cx, cy): the source's twelve
/// cells with the ball (red in the bottom two), ten green datum lights each
/// side level with the centre, the red wave-off lights either side of the
/// source and the green cut lights over it. `gse` None draws the lens dark.
fn draw_lens(s: &mut Svg, cx: f64, cy: f64, k: f64, gse: Option<f64>, waveoff: bool) {
    let cell = 7. * k;
    let src_w = 15. * k;
    let src_h = cell * LENS_CELLS as f64;
    let (top, half) = (cy - src_h / 2., src_w / 2. + 22. * k + 9. * 7. * k);
    s.rect(cx - half - 6. * k, top - 26. * k, 2. * (half + 6. * k), src_h + 40. * k, "#16191d", Some((TEXT, 1.)), 4. * k);
    // the source and the ball
    let ball = gse.map(ball_pos);
    let lit_now: Vec<usize> = match ball {
        Some((p, false)) => lit_cells(p),
        _ => vec![],
    };
    for i in 0..LENS_CELLS {
        let lit = lit_now.contains(&i);
        let red = i >= LENS_CELLS - 2;
        let fill = match (lit, red) {
            (true, true) => "#ff3b30",
            (true, false) => "#ffb000",
            (false, true) => "#3a1d1b",
            (false, false) => "#3a3020",
        };
        s.rect(cx - src_w / 2., top + i as f64 * cell + 0.6, src_w, cell - 1.2, fill, None, 1.);
    }
    match ball {
        Some((p, false)) => {
            let red = lit_now.iter().any(|i| *i >= LENS_CELLS - 2);
            let _ = write!(s.b, r#"<circle cx="{:.1}" cy="{:.1}" r="{:.1}" fill="{}" opacity="0.35"/>"#, n(cx), n(top + p * cell), n(cell * 1.3), if red { "#ff3b30" } else { "#ffb000" });
        }
        // off the lens: nothing shows on the real thing; mark which way it went
        Some((_, true)) => {
            let up = gse.unwrap_or(0.) > 0.;
            let (y0, d) = if up { (top - 3. * k, -6. * k) } else { (top + src_h + 3. * k, 6. * k) };
            let _ = write!(s.b, r#"<path d="M{:.1} {:.1}L{:.1} {:.1}L{:.1} {:.1}Z" fill="{}"/>"#, n(cx - 6. * k), n(y0), n(cx), n(y0 + d), n(cx + 6. * k), n(y0), if up { "#ffb000" } else { "#ff3b30" });
        }
        None => (),
    }
    // datum lights: ten each side, level with the middle of the source
    for side in [-1., 1.] {
        for j in 0..10 {
            let x = cx + side * (src_w / 2. + 22. * k + j as f64 * 7. * k);
            s.circle(x, cy, 2.6 * k, "#2fd16b", None, None);
        }
        // wave-off lights: a column of four each side of the source, above the datum
        for j in 0..4 {
            let x = cx + side * (src_w / 2. + 10. * k);
            let y = top + 2. * k + j as f64 * 7. * k;
            s.circle(x, y, 2.6 * k, if waveoff { "#ff3b30" } else { "#4a2422" }, None, None);
        }
    }
    // cut lights: four over the source
    for j in 0..4 {
        s.circle(cx + (j as f64 - 1.5) * 7. * k, top - 13. * k, 2.4 * k, "#24452f", None, None);
    }
}

// ── the bombing score sheet ─────────────────────────────────────────────

pub(crate) fn bomb_form(rec: &RangeRecord, b: &BombResult) -> String {
    const W: f64 = 1100.;
    let h = 1032.;
    let mut s = Svg::new(W, h);
    head(&mut s, rec, "WEAPONS RANGE SCORE SHEET", "AIR-TO-GROUND · CONVENTIONAL DELIVERY · VECTOR STRIKE TRAINING RANGE", "VSR FORM 13-212 (REV 10-26)");
    let wpn = if b.weapon_display.is_empty() { b.weapon.clone() } else { b.weapon_display.clone() };
    let alt_ft = if b.release.pos.alt_m != 0. { b.release.pos.alt_m } else { b.release.alt_agl_m } * FT;
    // row A: the event
    let fw = (W - 48.) / 7.;
    let row_a: [(&str, String); 7] = [
        ("RANGE", b.range.clone()),
        ("TARGET", b.target.clone()),
        ("PILOT", rec.pilot.name.clone()),
        ("A/C", discord::short_airframe(&rec.unit_type)),
        ("ORDNANCE", wpn.clone()),
        ("GUIDANCE", b.guidance.to_uppercase()),
        ("TIER", b.tier.clone().map(|t| t.to_uppercase()).unwrap_or_else(|| "-".into())),
    ];
    for (i, (k, v)) in row_a.iter().enumerate() {
        let col = if i == 6 { b.tier.as_deref().map(tier_color).unwrap_or(TEXT) } else { TEXT };
        field(&mut s, 24. + i as f64 * fw, 108., fw, 46., k, v, 13., col, i >= 4);
    }
    // row B: release parameters
    let row_b: [(&str, String); 7] = [
        ("REL ALT MSL", format!("{alt_ft:.0} FT")),
        ("REL ALT AGL", format!("{:.0} FT", b.release.alt_agl_m * FT)),
        ("TAS / GS", format!("{:.0} / {:.0} KT", b.release.tas_kts, b.release.gs_kts)),
        ("MACH", format!("{:.2}", b.release.mach)),
        ("RUN-IN HDG", format!("{:03.0}°", b.release.heading_deg.rem_euclid(360.))),
        ("DIVE", format!("{:.0}°", b.release.dive_deg)),
        ("SLANT / GND", format!("{:.0} / {:.0} M", b.release.slant_range_m, b.release.ground_range_m)),
    ];
    for (i, (k, v)) in row_b.iter().enumerate() {
        field(&mut s, 24. + i as f64 * fw, 158., fw, 46., k, v, 13., TEXT, true);
    }
    // row C: the score
    let qc = bomb_color(b.quality);
    s.rect(24., 212., 360., 110., "none", Some((TEXT, 1.6)), 0.);
    s.text(32., 226., "SCORE", 9.5, MUTED, Anchor::Start, true);
    text_f(&mut s, 204., 276., &format!("{:.0} M", b.miss_m), 44., qc, Anchor::Middle, true, MONO);
    text_f(&mut s, 204., 300., &format!("@ {:03.0}°  ·  {} O'CLOCK", b.radial_deg.rem_euclid(360.), b.clock), 13., TEXT, Anchor::Middle, true, MONO);
    s.text(204., 316., b.quality.label(), 12., qc, Anchor::Middle, true);
    let kv: [(&str, String); 6] = [
        ("Long / short", format!("{:+.0} m", b.long_m)),
        ("Left / right", format!("{:+.0} m", b.cross_m)),
        ("Time of fall", format!("{:.1} s", b.time_of_flight_s)),
        ("Wind at release", format!("{:03.0}/{:.0} kt", b.release.wind_from_deg.rem_euclid(360.), b.release.wind_kts)),
        ("Target hit", if b.target_hit { "YES".into() } else { "NO".into() }),
        ("Laser code", b.laser_code.map(|c| c.to_string()).unwrap_or_else(|| "-".into())),
    ];
    s.rect(392., 212., W - 24. - 392., 110., "none", Some((TEXT, 1.)), 0.);
    s.text(400., 226., "DELIVERY", 9.5, MUTED, Anchor::Start, true);
    for (i, (k, v)) in kv.iter().enumerate() {
        let col = i / 3;
        let (x, y) = (400. + col as f64 * 340., 248. + (i % 3) as f64 * 24.);
        s.text(x, y, k, 11.5, MUTED, Anchor::Start, false);
        text_f(&mut s, x + 300., y, v, 13., TEXT, Anchor::End, true, MONO);
    }
    if let Some(j) = &b.gps_denied {
        s.text(W - 32., 226., &trunc(&format!("GPS DENIED · {j}"), 40), 10.5, ORANGE, Anchor::End, true);
    }

    // ── the target diagram ──
    let (dx, dy, ds) = (24., 346., 620.);
    s.text(dx, dy - 8., "TARGET DIAGRAM · north up · scoring rings", 12., TEXT, Anchor::Start, true);
    s.clip("tgt", dx, dy, ds, ds);
    s.rect(dx, dy, ds, ds, PANEL, Some((TEXT, 1.)), 0.);
    let ring_max = b.rings_m.iter().cloned().fold(0., f64::max);
    let ext = (ring_max * 1.2).max(b.miss_m * 1.3).max(30.);
    let sc = (ds / 2.) / ext;
    let (cx, cy) = (dx + ds / 2., dy + ds / 2.);
    let to = |e: f64, nn: f64| (cx + e * sc, cy - nn * sc);
    s.raw(r#"<g clip-path="url(#tgt)">"#);
    // range grid every 10 m (or 50 m when wide)
    let gstep = if ext > 300. { 100. } else if ext > 120. { 50. } else { 10. };
    let mut g = gstep;
    while g < ext * 1.5 {
        s.circle(cx, cy, g * sc, "none", Some((GRID, 0.6)), None);
        g += gstep;
    }
    for k in 0..12 {
        let a = (k as f64 * 30.).to_radians();
        s.line(cx, cy, cx + a.sin() * ds, cy - a.cos() * ds, GRID, 0.6, None);
    }
    // the scoring rings
    for r in b.rings_m.iter().filter(|r| **r > 0.) {
        s.circle(cx, cy, r * sc, "none", Some((TEXT, 1.4)), None);
        text_f(&mut s, cx + r * sc * 0.72 + 3., cy - r * sc * 0.72 - 3., &format!("{r:.0} M"), 10., TEXT, Anchor::Start, true, MONO);
    }
    if b.good_radius_m > 0. {
        s.circle(cx, cy, b.good_radius_m * sc, "none", Some((GREEN, 1.6)), Some("6 4"));
    }
    // the attack axis through the target
    let hd = b.release.heading_deg.to_radians();
    let (ux, uy) = (hd.sin(), -hd.cos());
    s.line(cx - ux * ds, cy - uy * ds, cx + ux * ds, cy + uy * ds, BLUE, 1.2, Some("10 6"));
    s.arrow(cx - ux * ds * 0.46, cy - uy * ds * 0.46, cx - ux * ds * 0.3, cy - uy * ds * 0.3, BLUE, 2.);
    s.text(cx - ux * ds * 0.4 + 10., cy - uy * ds * 0.4, "RUN-IN", 10., BLUE, Anchor::Start, true);
    // the target and the impact
    s.rect(cx - 7., cy - 7., 14., 14., SAND, Some((TEXT, 1.2)), 0.);
    s.line(cx - 12., cy, cx + 12., cy, TEXT, 1., None);
    s.line(cx, cy - 12., cx, cy + 12., TEXT, 1., None);
    let (ix, iy) = to(b.impact_east_m, b.impact_north_m);
    s.line(cx, cy, ix, iy, qc, 1.4, Some("3 3"));
    s.line(ix - 8., iy - 8., ix + 8., iy + 8., qc, 3., None);
    s.line(ix + 8., iy - 8., ix - 8., iy + 8., qc, 3., None);
    text_f(&mut s, ix + 12., iy - 10., &format!("{:.0} M @ {:03.0}°", b.miss_m, b.radial_deg.rem_euclid(360.)), 11.5, qc, Anchor::Start, true, MONO);
    s.raw("</g>");
    // compass rose ticks round the edge
    for (k, l) in ["N", "E", "S", "W"].iter().enumerate() {
        let a = (k as f64 * 90.).to_radians();
        let (x, y) = (cx + a.sin() * (ds / 2. - 14.), cy - a.cos() * (ds / 2. - 14.) + 4.);
        s.text(x, y, l, 12., TEXT, Anchor::Middle, true);
    }
    s.text(dx + 8., dy + ds - 8., &format!("grid {gstep:.0} m"), 10., MUTED, Anchor::Start, false);

    // ── the fall, seen from the side ──
    let (fx, fy, fwid, fh) = (dx + ds + 20., dy, W - 24. - (dx + ds + 20.), 300.);
    s.text(fx, fy - 8., "RELEASE PROFILE · along the run-in", 12., TEXT, Anchor::Start, true);
    s.clip("fall", fx, fy, fwid, fh);
    s.rect(fx, fy, fwid, fh, PANEL, Some((TEXT, 1.)), 0.);
    let pts: Vec<(f64, f64)> = match &rec.track {
        Some(Track::Weapon { points }) if points.len() > 1 => {
            let o = (b.target_pos.lat, b.target_pos.lon);
            points
                .iter()
                .map(|q| {
                    let (e, nn) = super::cards::en(o, (q.lat, q.lon));
                    // distance before the target along the run-in, height above the target
                    (-(e * hd.sin() + nn * hd.cos()), q.alt_m - b.target_pos.alt_m)
                })
                .collect()
        }
        _ => vec![(b.release.ground_range_m, b.release.alt_agl_m), (b.long_m.min(0.), 0.)],
    };
    let dmax = pts.iter().map(|q| q.0).fold(b.release.ground_range_m, f64::max).max(100.) * 1.1;
    let dmin = pts.iter().map(|q| q.0).fold(0., f64::min).min(0.) - dmax * 0.08;
    let hmax = pts.iter().map(|q| q.1).fold(b.release.alt_agl_m, f64::max).max(50.) * 1.12;
    let fxm = |d: f64| fx + fwid - 12. - (d - dmin) / (dmax - dmin) * (fwid - 24.);
    let fym = |hh: f64| fy + fh - 22. - hh / hmax * (fh - 40.);
    s.raw(r#"<g clip-path="url(#fall)">"#);
    s.line(fx, fym(0.), fx + fwid, fym(0.), TEXT, 1.4, None);
    let hmax_ft = hmax * FT;
    let hs = if hmax_ft > 12_000. { 4000. } else if hmax_ft > 6000. { 2000. } else if hmax_ft > 2500. { 1000. } else { 500. };
    let mut gg = 0.;
    while gg < hmax_ft {
        s.line(fx, fym(gg / FT), fx + fwid, fym(gg / FT), GRID, 0.6, None);
        text_f(&mut s, fx + 4., fym(gg / FT) - 3., &format!("{gg:.0} ft"), 9., MUTED, Anchor::Start, false, MONO);
        gg += hs;
    }
    let prof: Vec<(f64, f64)> = pts.iter().map(|q| (fxm(q.0), fym(q.1.max(0.)))).collect();
    s.polyline(&prof, qc, 2.2, None, None);
    if let Some(r) = prof.first() {
        s.circle(r.0, r.1, 4.5, BLUE, Some((TEXT, 1.)), None);
        s.text(r.0 - 8., r.1 - 8., "REL", 10., BLUE, Anchor::End, true);
    }
    s.rect(fxm(0.) - 5., fym(0.) - 10., 10., 10., SAND, Some((TEXT, 1.)), 0.);
    s.raw("</g>");
    s.text(fx + fwid - 6., fy + fh + 14., &format!("{:.0} m ground range", b.release.ground_range_m), 10., MUTED, Anchor::End, false);

    // ── the scorer's block ──
    let (bx, by, bw, bh) = (fx, fy + fh + 30., fwid, ds - fh - 30.);
    s.rect(bx, by, bw, bh, "none", Some((TEXT, 1.)), 0.);
    s.text(bx + 8., by + 14., "RANGE CONTROL REMARKS", 9.5, MUTED, Anchor::Start, true);
    let remarks = [
        format!("{} {} delivery, {} guidance.", wpn, format!("{:?}", b.weapon_class).to_lowercase(), b.guidance),
        format!("Released {:.0} ft AGL, {:.0}° dive, {:.0} kt TAS on {:03.0}°.", b.release.alt_agl_m * FT, b.release.dive_deg, b.release.tas_kts, b.release.heading_deg.rem_euclid(360.)),
        format!("Impact {:.0} m {} and {:.0} m {} of the target.", b.long_m.abs(), if b.long_m >= 0. { "long" } else { "short" }, b.cross_m.abs(), if b.cross_m >= 0. { "right" } else { "left" }),
        format!("Scored {} ({}).", b.quality.label(), if b.target_hit { "target destroyed" } else { "target standing" }),
    ];
    for (i, r) in remarks.iter().enumerate() {
        text_f(&mut s, bx + 12., by + 42. + i as f64 * 26., &trunc(r, 58), 13., "#1a2f6e", Anchor::Start, false, HAND);
    }
    s.text(bx + bw - 10., by + bh - 10., "SCORED BY: RANGE ENGINE", 9.5, MUTED, Anchor::End, true);
    foot(&mut s, rec);
    s.finish()
}

// ── the field landing / pattern record ──────────────────────────────────

const LEGS: [(Leg, &str); 7] = [
    (Leg::Initial, BLUE),
    (Leg::Break, "#7a3fb0"),
    (Leg::Downwind, GREEN),
    (Leg::Base, ORANGE),
    (Leg::Final, TEXT),
    (Leg::Rollout, MUTED),
    (Leg::Other, GRID),
];

fn leg_color(l: Leg) -> &'static str {
    LEGS.iter().find(|(x, _)| *x == l).map(|(_, c)| *c).unwrap_or(MUTED)
}

/// A field landing's track in the runway frame (`along` + in the landing
/// direction from the threshold, `across` + right), heights above the field.
struct RwyFrame {
    o: (f64, f64),
    h: f64,
    elev: f64,
}

impl RwyFrame {
    fn of(fl: &FieldLandingResult) -> Self {
        let thr = if fl.threshold_pos.lat != 0. || fl.threshold_pos.lon != 0. { &fl.threshold_pos } else { &fl.touchdown_pos };
        Self { o: (thr.lat, thr.lon), h: fl.runway_heading_deg.to_radians(), elev: thr.alt_m }
    }

    fn rel(&self, p: &TrackPt) -> (f64, f64) {
        let (e, nn) = super::cards::en(self.o, (p.lat, p.lon));
        (nn * self.h.cos() + e * self.h.sin(), -nn * self.h.sin() + e * self.h.cos())
    }

    fn samples(&self, pts: &[TrackPt]) -> Vec<RelSample> {
        let loc: Vec<(f64, f64)> = pts.iter().map(|p| self.rel(p)).collect();
        (0..pts.len())
            .map(|i| {
                let (a, b) = (loc[i.saturating_sub(1)], loc[(i + 1).min(loc.len() - 1)]);
                let (da, dc) = (b.0 - a.0, b.1 - a.1);
                let rel = if da.hypot(dc) > 1. { dc.atan2(da).to_degrees() } else { 0. };
                RelSample { t: pts[i].t, along: loc[i].0, across: loc[i].1, agl: pts[i].alt_m - self.elev, rel, gs_kts: pts[i].speed_kts }
            })
            .collect()
    }
}

/// Runs of the same leg, each with the first sample of the next so the line is unbroken.
fn leg_runs(lg: &[Leg]) -> Vec<(Leg, usize, usize)> {
    let mut out = vec![];
    let mut i = 0;
    while i < lg.len() {
        let mut j = i;
        while j + 1 < lg.len() && lg[j + 1] == lg[i] {
            j += 1;
        }
        out.push((lg[i], i, (j + 1).min(lg.len() - 1)));
        i = j + 1;
    }
    out
}

pub(crate) fn field_form(rec: &RangeRecord, fl: &FieldLandingResult) -> String {
    const W: f64 = 1100.;
    let h = 1070.;
    let mut s = Svg::new(W, h);
    head(&mut s, rec, "FIELD LANDING / PATTERN RECORD", "VFR TRAFFIC PATTERN · APPROACH AND LANDING · ROLLOUT AND TAXI", "VSR FORM 3710/4 (REV 10-26)");
    let qc = if fl.outcome == FieldLandingOutcome::Undershoot { RED } else { precision_color(fl.quality) };
    let pat = fl.pattern.as_ref();
    let ft = |v: Option<f64>| v.map(|v| format!("{v:.0} FT")).unwrap_or_else(|| "-".into());
    let deg = |v: Option<f64>| v.map(|v| format!("{v:+.1}°")).unwrap_or_else(|| "-".into());
    let kind = pat
        .map(|p| {
            let k = p.kind.replace('_', "-").to_uppercase();
            match &p.side {
                Some(sd) => format!("{k} · {}", sd.to_uppercase()),
                None => k,
            }
        })
        .unwrap_or_else(|| "-".into());
    let rwy = if fl.runway_length_m > 0. { format!("{} · {:.0}×{:.0} M", fl.runway, fl.runway_length_m, fl.runway_width_m.max(1.)) } else { fl.runway.clone() };
    // row A: the event
    let fw = (W - 48.) / 7.;
    let row_a: [(&str, String, &str); 7] = [
        ("AIRFIELD", fl.airfield.to_uppercase(), TEXT),
        ("RUNWAY", rwy, TEXT),
        ("PILOT", rec.pilot.name.clone(), TEXT),
        ("A/C", discord::short_airframe(&rec.unit_type), TEXT),
        ("PATTERN", kind, TEXT),
        ("OUTCOME", fl.outcome.label().to_uppercase(), if fl.outcome == FieldLandingOutcome::Undershoot { RED } else { TEXT }),
        ("GRADE", fl.quality.label().to_uppercase(), qc),
    ];
    for (i, (k, v, c)) in row_a.iter().enumerate() {
        field(&mut s, 24. + i as f64 * fw, 108., fw, 46., k, v, 13., c, false);
    }
    // row B: the touchdown
    let td = fl.touchdown_from_threshold_m;
    let side = |v: f64, pos: &str, neg: &str| format!("{:.0} M {}", v.abs(), if v >= 0. { pos } else { neg });
    let row_b: [(&str, String); 7] = [
        ("TOUCHDOWN", if td >= 0. { format!("{td:.0} M PAST THR") } else { format!("{:.0} M SHORT", -td) }),
        ("AIM POINT", side(fl.aim_error_m, "LONG", "SHORT")),
        ("CENTRELINE", format!("{:.1} M {}", fl.centreline_m.abs(), if fl.centreline_m >= 0. { "R" } else { "L" })),
        ("SINK AT TD", format!("{:.0} FPM", fl.touchdown_fpm)),
        ("GS AT TD", format!("{:.0} KT", fl.touchdown_gs_kts)),
        ("GLIDESLOPE 1 / ½ NM", format!("{} / {}", deg(fl.gs_error_1nm_deg), deg(fl.gs_error_half_nm_deg))),
        ("LINEUP 1 / ½ NM", format!("{} / {}", deg(fl.lineup_1nm_deg), deg(fl.lineup_half_nm_deg))),
    ];
    for (i, (k, v)) in row_b.iter().enumerate() {
        field(&mut s, 24. + i as f64 * fw, 158., fw, 46., k, v, 13., TEXT, true);
    }
    // row C: the pattern
    let row_c: [(&str, String); 7] = [
        ("INITIAL", ft(pat.and_then(|p| p.initial_alt_ft))),
        ("BREAK", pat.and_then(|p| Some(format!("{:.0} FT @ {:+.0} M", p.break_alt_ft?, p.break_at_m?))).unwrap_or_else(|| "-".into())),
        ("DOWNWIND", ft(pat.and_then(|p| p.downwind_alt_ft))),
        ("ABEAM", pat.and_then(|p| Some(format!("{:.1} NM · {:.0} FT", p.abeam_nm?, p.abeam_alt_ft.unwrap_or(0.)))).unwrap_or_else(|| "-".into())),
        ("BASE TURN", pat.and_then(|p| p.base_at_m).map(|b| format!("{:.2} NM {}", b.abs() / NM, if b < 0. { "PAST" } else { "SHORT" })).unwrap_or_else(|| "-".into())),
        ("WINGS LEVEL", pat.and_then(|p| Some(format!("{:.2} NM · {:.0} FT", p.rollout_nm?, p.rollout_alt_ft.unwrap_or(0.)))).unwrap_or_else(|| "-".into())),
        ("FINAL GS · TIME", pat.map(|p| format!("{} · {}", p.final_gs_kts.map(|v| format!("{v:.0} KT")).unwrap_or("-".into()), p.pattern_time_s.map(|v| format!("{v:.0} S")).unwrap_or("-".into()))).unwrap_or_else(|| "-".into())),
    ];
    for (i, (k, v)) in row_c.iter().enumerate() {
        field(&mut s, 24. + i as f64 * fw, 208., fw, 46., k, v, 13., TEXT, true);
    }

    let f = RwyFrame::of(fl);
    let (ppts, tpts): (&[TrackPt], &[TrackPt]) = match &rec.track {
        Some(Track::Path { paths }) => (
            paths.get("pattern").or_else(|| paths.values().next()).map(|v| v.as_slice()).unwrap_or(&[]),
            paths.get("taxi").map(|v| v.as_slice()).unwrap_or(&[]),
        ),
        _ => (&[], &[]),
    };
    let ps = f.samples(ppts);
    let lg = pattern_legs::legs(&ps, 0.);
    let len = if fl.runway_length_m > 0. { fl.runway_length_m } else { 2400. };
    let wid = if fl.runway_width_m > 0. { fl.runway_width_m } else { 45. };
    let aim = td - fl.aim_error_m;

    // ── the pattern in plan: landing up the page, to scale ──
    let (px, py, pw, ph) = (24., 290., 600., 540.);
    panel_title(&mut s, px, py - 8., "TRAFFIC PATTERN · plan · landing up the page · to scale", "");
    s.clip("pat", px, py, pw, ph);
    s.rect(px, py, pw, ph, PANEL, Some((TEXT, 1.)), 0.);
    let (mut a0, mut a1, mut c0, mut c1) = (-1.6 * NM, len + 400., -0.9 * NM, 0.9 * NM);
    for q in &ps {
        a0 = a0.min(q.along);
        a1 = a1.max(q.along);
        c0 = c0.min(q.across);
        c1 = c1.max(q.across);
    }
    let sc = ((pw - 40.) / (c1 - c0)).min((ph - 50.) / (a1 - a0));
    let (cx, cy) = (px + pw / 2. - (c0 + c1) / 2. * sc, py + ph / 2. + (a0 + a1) / 2. * sc);
    let pg = |along: f64, across: f64| (cx + across * sc, cy - along * sc);
    s.raw(r#"<g clip-path="url(#pat)">"#);
    // half-mile grid on the runway frame
    let mut g = -6. * NM;
    while g <= 6. * NM {
        let (x0, y0) = pg(g, -8. * NM);
        let (x1, y1) = pg(g, 8. * NM);
        s.line(x0, y0, x1, y1, GRID, if (g / NM).fract().abs() < 1e-6 { 0.9 } else { 0.5 }, None);
        let (x0, y0) = pg(-8. * NM, g);
        let (x1, y1) = pg(8. * NM, g);
        s.line(x0, y0, x1, y1, GRID, if (g / NM).fract().abs() < 1e-6 { 0.9 } else { 0.5 }, None);
        g += NM / 2.;
    }
    // extended centreline, the runway, the threshold and aim point
    let (e0, e1) = (pg(-8. * NM, 0.), pg(len + 8. * NM, 0.));
    s.line(e0.0, e0.1, e1.0, e1.1, INK, 0.8, Some("10 6"));
    let hw = (wid / 2.).max(3. / sc);
    let rw = [pg(0., -hw), pg(len, -hw), pg(len, hw), pg(0., hw)];
    s.polygon(&rw, "#3a3d44", Some((TEXT, 1.)), None);
    let (t0, t1) = (pg(0., -hw), pg(0., hw));
    s.line(t0.0, t0.1, t1.0, t1.1, "#ffffff", 2., None);
    let am = pg(aim, 0.);
    s.circle(am.0, am.1, 2.5, "#ffffff", None, None);
    // the track, leg by leg
    for (l, i, j) in leg_runs(&lg) {
        let pts: Vec<P> = ps[i..=j].iter().map(|q| pg(q.along, q.across)).collect();
        s.polyline(&pts, leg_color(l), if l == Leg::Final { 2.4 } else { 2. }, if l == Leg::Other { Some("4 3") } else { None }, None);
    }
    // the marks along it
    let mark = |s: &mut Svg, q: &RelSample, label: &str, color: &str| {
        let (x, y) = pg(q.along, q.across);
        s.circle(x, y, 4., "#ffffff", Some((color, 1.6)), None);
        let right = q.across >= (c0 + c1) / 2.;
        text_f(s, if right { x + 8. } else { x - 8. }, y + 4., label, 10., color, if right { Anchor::Start } else { Anchor::End }, true, MONO);
    };
    let first = |l: Leg| lg.iter().position(|x| *x == l);
    if let Some(i) = first(Leg::Break) {
        mark(&mut s, &ps[i], &format!("BREAK {:.0}", ps[i].agl * FT), leg_color(Leg::Break));
    }
    let dw: Vec<&RelSample> = ps.iter().zip(&lg).filter(|(_, l)| **l == Leg::Downwind).map(|(q, _)| q).collect();
    if let Some(ab) = dw.iter().min_by(|a, b| a.along.abs().total_cmp(&b.along.abs())) {
        if ab.along.abs() < 1500. {
            mark(&mut s, ab, &format!("ABEAM {:.1} NM", ab.across.abs() / NM), leg_color(Leg::Downwind));
        }
    }
    if let Some(i) = first(Leg::Base) {
        mark(&mut s, &ps[i], &format!("BASE {:.0}", ps[i].agl * FT), leg_color(Leg::Base));
    }
    if let Some(i) = first(Leg::Final) {
        if lg.iter().any(|l| *l == Leg::Downwind || *l == Leg::Base) {
            mark(&mut s, &ps[i], &format!("WINGS LVL {:.0}", ps[i].agl * FT), leg_color(Leg::Final));
        }
    }
    let tdp = pg(td, fl.centreline_m);
    s.circle(tdp.0, tdp.1, 5.5, qc, Some(("#ffffff", 1.5)), None);
    text_f(&mut s, tdp.0 + 9., tdp.1 + 4., "TD", 10.5, qc, Anchor::Start, true, MONO);
    if ps.is_empty() {
        s.text(px + pw / 2., py + ph / 2. + 40., "no pattern track on this record", 13., MUTED, Anchor::Middle, false);
    }
    s.raw("</g>");
    // north arrow (the page is turned to the runway), the scale bar, the legend
    let (nx, ny) = (px + pw - 30., py + 34.);
    let nh = -f.h;
    s.circle(nx, ny, 17., "#ffffff", Some((TEXT, 0.8)), None);
    s.arrow(nx - nh.sin() * 12., ny + nh.cos() * 12., nx + nh.sin() * 13., ny - nh.cos() * 13., TEXT, 1.6);
    s.text(nx + nh.sin() * 26., ny - nh.cos() * 26. + 4., "N", 11., TEXT, Anchor::Middle, true);
    let bar = NM * sc;
    s.line(px + 14., py + ph - 16., px + 14. + bar, py + ph - 16., TEXT, 2., None);
    s.line(px + 14., py + ph - 21., px + 14., py + ph - 11., TEXT, 1.2, None);
    s.line(px + 14. + bar, py + ph - 21., px + 14. + bar, py + ph - 11., TEXT, 1.2, None);
    text_f(&mut s, px + 14. + bar / 2., py + ph - 24., "1 NM", 9.5, TEXT, Anchor::Middle, true, MONO);
    s.text(px + 14., py + 20., &format!("RWY {} · {:03.0}°T", fl.runway, fl.runway_heading_deg.rem_euclid(360.)), 11., TEXT, Anchor::Start, true);
    let mut lx = px;
    for (l, c) in LEGS.iter().take(6) {
        s.line(lx, py + ph + 14., lx + 16., py + ph + 14., c, 3., None);
        s.text(lx + 20., py + ph + 18., &l.label().to_uppercase(), 9.5, MUTED, Anchor::Start, true);
        lx += 100.;
    }

    // ── height against time, to the touchdown ──
    let (rx, rwid) = (644., W - 24. - 644.);
    let (hy, hh) = (290., 170.);
    panel_title(&mut s, rx, hy - 8., "HEIGHT · AGL against time to touchdown", "");
    s.clip("hgt", rx, hy, rwid, hh);
    s.rect(rx, hy, rwid, hh, PANEL, Some((TEXT, 1.)), 0.);
    let tmin = ps.first().map(|q| q.t).unwrap_or(-120.).min(-30.);
    let hmax = ps.iter().map(|q| q.agl * FT).fold(1500., f64::max) * 1.1;
    let hxm = |t: f64| rx + 34. + (t - tmin) / (0. - tmin) * (rwid - 44.);
    let hym = |v: f64| hy + hh - 16. - v / hmax * (hh - 26.);
    s.raw(r#"<g clip-path="url(#hgt)">"#);
    let hs = if hmax > 4000. { 1000. } else { 500. };
    let mut gg = 0.;
    while gg < hmax {
        s.line(rx, hym(gg), rx + rwid, hym(gg), GRID, 0.6, None);
        text_f(&mut s, rx + 4., hym(gg) - 2., &format!("{gg:.0}"), 9., MUTED, Anchor::Start, false, MONO);
        gg += hs;
    }
    if let Some(d) = pat.and_then(|p| p.downwind_alt_ft) {
        s.line(rx, hym(d), rx + rwid, hym(d), leg_color(Leg::Downwind), 0.8, Some("5 4"));
    }
    for (l, i, j) in leg_runs(&lg) {
        let pts: Vec<P> = ps[i..=j].iter().map(|q| (hxm(q.t), hym((q.agl * FT).max(0.)))).collect();
        s.polyline(&pts, leg_color(l), 2., None, None);
    }
    if ps.is_empty() {
        s.text(rx + rwid / 2., hy + hh / 2. + 4., "no pattern track on this record", 12., MUTED, Anchor::Middle, false);
    }
    s.raw("</g>");
    let step = nice_t(-tmin);
    let mut t = 0.;
    while t >= tmin {
        text_f(&mut s, hxm(t), hy + hh + 12., &format!("{t:.0}"), 9., MUTED, Anchor::Middle, false, MONO);
        t -= step;
    }
    s.text(rx + rwid, hy + hh + 24., "seconds to touchdown · ft AGL", 9.5, MUTED, Anchor::End, false);

    // ── the final: height against distance to the aim point, 3° glidepath ──
    let (gy, gh) = (hy + hh + 44., 150.);
    panel_title(&mut s, rx, gy - 8., "FINAL · 3° glidepath to the aim point", "");
    s.clip("fin", rx, gy, rwid, gh);
    s.rect(rx, gy, rwid, gh, PANEL, Some((TEXT, 1.)), 0.);
    let dmax = 2. * NM;
    let gmax = dmax * 4f64.to_radians().tan() * FT;
    let gxm = |d: f64| rx + rwid - 14. - d / dmax * (rwid - 28.);
    let gym = |v: f64| gy + gh - 14. - v / gmax * (gh - 24.);
    s.raw(r#"<g clip-path="url(#fin)">"#);
    for (d, c, w, dash) in [(3., INK, 1.4, None), (3.7, MUTED, 0.8, Some("4 3")), (2.3, MUTED, 0.8, Some("4 3"))] {
        s.line(gxm(0.), gym(0.), gxm(dmax), gym(dmax * f64::to_radians(d).tan() * FT), c, w, dash);
    }
    for k in 1..4 {
        let d = k as f64 * NM / 2.;
        s.line(gxm(d), gy, gxm(d), gy + gh, GRID, 0.6, None);
        text_f(&mut s, gxm(d), gy + 12., &format!("{:.1}", d / NM), 9., MUTED, Anchor::Middle, false, MONO);
    }
    let fin: Vec<P> = ps.iter().zip(&lg).filter(|(q, l)| **l == Leg::Final && aim - q.along <= dmax * 1.05).map(|(q, _)| (gxm((aim - q.along).max(0.)), gym((q.agl * FT).max(0.)))).collect();
    s.polyline(&fin, qc, 2.2, None, None);
    if fin.len() < 2 {
        s.text(rx + rwid / 2., gy + gh / 2. + 4., "no final on this record", 12., MUTED, Anchor::Middle, false);
    }
    s.raw("</g>");
    s.line(gxm(0.), gym(0.) - 6., gxm(0.), gym(0.) + 6., TEXT, 1.6, None);
    s.text(rx + rwid, gy + gh + 14., "nm to the aim point · 3° ± 0.7°", 9.5, MUTED, Anchor::End, false);

    // ── the rollout and the taxi: landing to the right ──
    let (ty0, th) = (gy + gh + 40., 830. - (gy + gh + 40.));
    panel_title(&mut s, rx, ty0 - 8., "ROLLOUT AND TAXI · landing to the right", "");
    s.clip("txi", rx, ty0, rwid, th);
    s.rect(rx, ty0, rwid, th, PANEL, Some((TEXT, 1.)), 0.);
    let ts: Vec<(f64, f64, f64)> = tpts.iter().map(|p| {
        let (a, c) = f.rel(p);
        (a, c, p.speed_kts)
    }).collect();
    let (mut b0, mut b1, mut d0, mut d1) = (td.min(0.) - 100., len + 100., -wid * 2., wid * 2.);
    for q in &ts {
        b0 = b0.min(q.0);
        b1 = b1.max(q.0);
        d0 = d0.min(q.1);
        d1 = d1.max(q.1);
    }
    let tsc = ((rwid - 24.) / (b1 - b0)).min((th - 24.) / (d1 - d0));
    let (tcx, tcy) = (rx + rwid / 2. - (b0 + b1) / 2. * tsc, ty0 + th / 2. - (d0 + d1) / 2. * tsc);
    let tg = |a: f64, c: f64| (tcx + a * tsc, tcy + c * tsc);
    s.raw(r#"<g clip-path="url(#txi)">"#);
    let twh = (wid / 2.).max(2.5 / tsc);
    s.polygon(&[tg(0., -twh), tg(len, -twh), tg(len, twh), tg(0., twh)], "#3a3d44", Some((TEXT, 1.)), None);
    let (c0p, c1p) = (tg(0., 0.), tg(len, 0.));
    s.line(c0p.0, c0p.1, c1p.0, c1p.1, "#c9ccd1", 0.8, Some("8 6"));
    let tcol = |q: &(f64, f64, f64)| {
        let fast = q.2 > pattern_legs::TAXI_LIMIT_KTS;
        if fast && q.1.abs() > wid / 2. { RED } else if fast { MUTED } else { BLUE }
    };
    let mut i = 1;
    while i < ts.len() {
        let c = tcol(&ts[i]);
        let mut run = vec![tg(ts[i - 1].0, ts[i - 1].1)];
        while i < ts.len() && tcol(&ts[i]) == c {
            run.push(tg(ts[i].0, ts[i].1));
            i += 1;
        }
        s.polyline(&run, c, 2.2, None, None);
    }
    let tdq = tg(td, fl.centreline_m);
    s.circle(tdq.0, tdq.1, 4.5, qc, Some(("#ffffff", 1.2)), None);
    if let Some(x) = &fl.taxi {
        if let Some(e) = x.exit_at_m {
            if let Some(q) = ts.iter().find(|q| (q.0 - e).abs() < 1. && q.1.abs() > wid / 2.).or_else(|| ts.iter().find(|q| q.1.abs() > wid / 2. + 4.)) {
                let p = tg(q.0, q.1);
                s.circle(p.0, p.1, 4., "#ffffff", Some((ORANGE, 1.6)), None);
                text_f(&mut s, p.0 + 7., p.1 - 6., "CLEAR", 9.5, ORANGE, Anchor::Start, true, MONO);
            }
        }
    }
    if let Some(l) = ts.last() {
        let p = tg(l.0, l.1);
        s.rect(p.0 - 4., p.1 - 4., 8., 8., SAND, Some((TEXT, 1.)), 0.);
    }
    if ts.is_empty() {
        let note = match fl.outcome {
            FieldLandingOutcome::TouchAndGo => "touch-and-go: no taxi",
            _ => "no taxi track on this record",
        };
        s.text(rx + rwid / 2., ty0 + th - 14., note, 12., MUTED, Anchor::Middle, false);
    }
    s.raw("</g>");
    s.text(rx + rwid, ty0 + th + 14., &format!("blue taxi · red over {:.0} kt off the runway · ■ end", pattern_legs::TAXI_LIMIT_KTS), 9.5, MUTED, Anchor::End, false);

    // ── the instructor's remarks and the taxi block ──
    let by = 862.;
    let bh = h - FOOT_H - 8. - by;
    s.rect(24., by, 600., bh, "none", Some((TEXT, 1.)), 0.);
    s.text(32., by + 14., "INSTRUCTOR REMARKS", 9.5, MUTED, Anchor::Start, true);
    let mut remarks: Vec<String> = vec![];
    if let Some(p) = pat {
        remarks.push(match p.kind.as_str() {
            "overhead" => format!("Overhead, {} break at {:.0} ft, downwind {:.0} ft.", p.side.as_deref().unwrap_or("-"), p.break_alt_ft.unwrap_or(0.), p.downwind_alt_ft.unwrap_or(0.)),
            "downwind" => format!("Joined downwind ({}), {:.0} ft, {:.1} nm abeam.", p.side.as_deref().unwrap_or("-"), p.downwind_alt_ft.unwrap_or(0.), p.abeam_nm.unwrap_or(0.)),
            "straight_in" => format!("Straight-in, wings level at {:.1} nm.", p.rollout_nm.unwrap_or(0.)),
            _ => "No standard pattern flown.".into(),
        });
    }
    let cl = fl.centreline_m.abs();
    remarks.push(format!(
        "Touched down {:.0} m {} the aim point, {} m {}, {:.0} fpm.",
        fl.aim_error_m.abs(),
        if fl.aim_error_m >= 0. { "past" } else { "short of" },
        if cl < 10. { format!("{cl:.1}") } else { format!("{cl:.0}") },
        if fl.centreline_m >= 0. { "right" } else { "left" },
        fl.touchdown_fpm
    ));
    let calls: Vec<&String> = fl.calls.iter().chain(pat.map(|p| p.calls.iter()).into_iter().flatten()).chain(fl.taxi.as_ref().map(|t| t.calls.iter()).into_iter().flatten()).collect();
    for c in calls {
        let mut ch = c.chars();
        remarks.push(ch.next().map(|f| format!("{}{}.", f.to_uppercase(), ch.as_str())).unwrap_or_default());
    }
    if remarks.len() == 1 + pat.is_some() as usize {
        remarks.push(if fl.stable { "Stable approach. Good pass.".into() } else { "Not stable from 1 nm.".into() });
    }
    for (i, r) in remarks.iter().take(5).enumerate() {
        text_f(&mut s, 36., by + 38. + i as f64 * 24., &trunc(r, 78), 13., "#1a2f6e", Anchor::Start, false, HAND);
    }
    s.text(24. + 600. - 10., by + bh - 8., "GRADED BY: RANGE ENGINE", 9.5, MUTED, Anchor::End, true);
    s.rect(rx, by, rwid, bh, "none", Some((TEXT, 1.)), 0.);
    s.text(rx + 8., by + 14., "ROLLOUT / TAXI", 9.5, MUTED, Anchor::Start, true);
    let end_label = |e: &str| match e {
        "parked" => "PARKED",
        "stopped_on_runway" => "STOPPED ON RWY",
        "left" => "LEFT THE A/C",
        "took_off" => "TOOK OFF AGAIN",
        "timeout" => "LOST (10 MIN)",
        _ => "-",
    };
    let rows: Vec<(&str, String)> = match &fl.taxi {
        Some(x) => vec![
            ("Rollout to 30 kt", format!("{:.0} m · {:.0} s", x.rollout_m, x.rollout_s)),
            ("Clear of the runway", x.runway_clear_s.map(|c| format!("{c:.0} s{}", x.exit_at_m.map(|e| format!(" @ {e:.0} m")).unwrap_or_default())).unwrap_or_else(|| "NEVER".into())),
            ("Taxi", format!("{:.0} m · {:.0} s", x.taxi_distance_m, x.taxi_time_s)),
            ("Top taxi speed", format!("{:.0} kt", x.max_taxi_kts)),
            ("Stops · ended", format!("{} · {}", x.stops, end_label(&x.end))),
        ],
        None => vec![(
            "Taxi",
            match fl.outcome {
                FieldLandingOutcome::FullStop => "not recorded".into(),
                FieldLandingOutcome::TouchAndGo => "touch-and-go".into(),
                FieldLandingOutcome::Undershoot => "-".into(),
            },
        )],
    };
    for (i, (k, v)) in rows.iter().enumerate() {
        let y = by + 36. + i as f64 * 23.;
        s.text(rx + 10., y, k, 11.5, MUTED, Anchor::Start, false);
        let col = if (*k == "Top taxi speed" && fl.taxi.as_ref().is_some_and(|x| x.max_taxi_kts > pattern_legs::TAXI_LIMIT_KTS)) || v == "NEVER" { RED } else { TEXT };
        text_f(&mut s, rx + rwid - 10., y, v, 13., col, Anchor::End, true, MONO);
    }
    foot(&mut s, rec);
    s.finish()
}

fn nice_t(span: f64) -> f64 {
    [10., 15., 30., 60., 120.].into_iter().find(|st| span / st <= 7.).unwrap_or(180.)
}

#[cfg(test)]
mod tests {
    use super::super::cards::tests::{bomb, trap};
    use super::*;
    use bfprotocols::range::RangeResult;

    #[test]
    fn forms_render() {
        let RangeResult::Trap(t) = &trap().result else { panic!() };
        let svg = trap_form(&trap(), t);
        assert!(svg.contains("LSO PASS RECORD") && !svg.contains("NaN"));
        assert!(super::super::cards::png(&svg).is_ok());
        let RangeResult::Bomb(b) = &bomb().result else { panic!() };
        let svg = bomb_form(&bomb(), b);
        assert!(svg.contains("WEAPONS RANGE SCORE SHEET") && !svg.contains("NaN"));
        assert!(super::super::cards::png(&svg).is_ok());
        for r in super::super::cards::tests::all_cards() {
            let RangeResult::FieldLanding(f) = &r.result else { continue };
            let svg = field_form(&r, f);
            assert!(svg.contains("FIELD LANDING / PATTERN RECORD") && !svg.contains("NaN"));
            assert!(super::super::cards::png(&svg).is_ok());
        }
    }

    #[test]
    fn the_lens_matches_the_site() {
        // the same cases as bfrange-web/src/lib/lens.test.ts
        assert_eq!(lit_cells(ball_pos(0.).0), vec![5, 6]);
        assert_eq!(ball_say(0.), "centred");
        assert_eq!(ball_say(0.2), "1.5 cells high");
        assert_eq!(lit_cells(ball_pos(0.2).0), vec![4]);
        assert!(ball_say(-0.7).ends_with("low, red"));
        assert_eq!(ball_say(1.9), "off the top");
        assert_eq!(ball_say(-1.2), "off the bottom");
    }

    /// `CARD_OUT=<dir> cargo test -p bfdb write_card_samples` writes every card as a PNG to look at.
    #[test]
    fn write_card_samples() {
        let Ok(dir) = std::env::var("CARD_OUT") else { return };
        std::fs::create_dir_all(&dir).unwrap();
        let mut recs = vec![trap(), bomb()];
        recs.extend(super::super::cards::tests::all_cards());
        for (i, r) in recs.iter().enumerate() {
            let svg = super::super::cards::render_svg(r);
            let png = super::super::cards::png(&svg).unwrap();
            std::fs::write(format!("{dir}/{i:02}-{}.png", r.kind()), png).unwrap();
        }
        // the same pass on every class of deck
        for ct in ["CVN_73", "Forrestal", "KUZNECOW", "CdG", "LHA_Tarawa"] {
            let mut r = trap();
            if let RangeResult::Trap(t) = &mut r.result {
                t.carrier_type = ct.into();
                t.carrier = ct.into();
            }
            let svg = super::super::cards::render_svg(&r);
            std::fs::write(format!("{dir}/deck-{ct}.png"), super::super::cards::png(&svg).unwrap()).unwrap();
        }
    }
}
