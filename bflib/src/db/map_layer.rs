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

//! Centralised F10 map drawing layer.
//!
//! Every feature in the engine that needs a map overlay goes through this
//! module.  The goal is to use the full DCS draw toolkit â€” line, circle,
//! rect, quad, text, arrow, and mark pins â€” deliberately and consistently,
//! so the F10 map gives players a rich tactical picture without cluttering
//! the code across a dozen separate files.
//!
//! # Layers
//! | Layer | Primitives | Side visibility |
//! |-------|-----------|----------------|
//! | Ground convoy routes | line + arrow + text | All (interdiction targets) |
//! | Air logistics orbits | line + rect (orbit) + circle (AWACS) + text | Own-side only |
//! | Sea logistics routes | line + arrow + text | Own-side only |
//! | Fire mission overlays | line (trajectory) + circle (impact) + text | All |
//! | Planned mission AOs | quad (AO) + arrow (ingress) + rect (panel) + text | Own-side only |
//! | CSAR pilot markers | circle (search area) + text (countdown) | Own-side only |
//! | JTAC layer | circle (lase range) + circle (target) + line (bearing) + text (9-line) | Own-side only |

use super::{
    group::DeployKind,
    intel::{IntelContact, IntelDatabase, IntelUnitClass},
    logistics::{AirLogisticsRoute, ConvoyId, LogiRouteId, SeaLogisticsRoute, SupplyConvoy},
    persisted::Persisted,
    tasks::TaskId,
};
use bfprotocols::db::objective::ObjectiveId;
use bfprotocols::db::group::GroupId;
use chrono::{DateTime, Duration, Utc};
use compact_str::format_compact;
use dcso3::{
    Color, LuaVec3, Vector2, Vector3,
    coalition::Side,
    trigger::{
        ArrowSpec, CircleSpec, LineSpec, LineType, MarkId, QuadSpec, RectSpec, SideFilter, TextSpec,
    },
};
use fxhash::{FxHashMap, FxHashSet};

use crate::msgq::MsgQ;

// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
// Helpers
// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

/// Radius of the "under attack" ring -- just outside a typical objective's
/// own rings, so it reads as an escalation rather than more clutter.
const UNDER_ATTACK_RING_M: f64 = 3400.;

fn side_color(side: Side, alpha: f32) -> Color {
    crate::mapcolor::side_color(side, alpha)
}

fn side_filter(side: Side) -> SideFilter {
    side.into()
}

/// Draw a closed ring of (north, east) offsets around `c` as ONE mark.
///
/// Every status shape in this module is a single freeform (markupToAll
/// shapeId 7), so a symbol costs one MarkId however many vertices it has.
/// Shapes are drawn on the ground, so they shrink as you zoom out and fade at
/// theatre zoom -- that IS the declutter. Anything a pilot must read at any
/// zoom stays a pin or text.
///
/// DCS renders a 3-point freeform as an OPEN polyline, so short rings are
/// padded rather than coming out as a bare "V".
fn poly(c: Vector2, offsets: &[(f64, f64)], outline: Color, fill: Color, to: SideFilter, msgs: &mut MsgQ) -> MarkId {
    let mut points: Vec<LuaVec3> = offsets.iter().map(|&(n, e)| v3(c.x + n, c.y + e)).collect();
    if let Some(first) = points.first().copied() {
        points.push(first);
    }
    while points.len() < 4 {
        if let Some(last) = points.last().copied() {
            points.push(last)
        }
    }
    let id = MarkId::new();
    msgs.freeform_to_all(
        to,
        id,
        dcso3::trigger::PolylineSpec {
            points,
            color: outline,
            fill_color: fill,
            line_type: LineType::Solid,
            read_only: true,
        },
        None,
    );
    id
}

/// Regular n-gon; `rot_deg` turns the first vertex off north.
fn ngon(c: Vector2, r: f64, n: usize, rot_deg: f64, outline: Color, fill: Color, to: SideFilter, msgs: &mut MsgQ) -> MarkId {
    let offs: Vec<(f64, f64)> = (0..n)
        .map(|i| {
            let a = (360. * i as f64 / n as f64 + rot_deg).to_radians();
            (r * a.cos(), r * a.sin())
        })
        .collect();
    poly(c, &offs, outline, fill, to, msgs)
}

/// A pointed burst: `spikes` points alternating between `r` and 0.42r.
fn burst(c: Vector2, r: f64, spikes: usize, outline: Color, fill: Color, to: SideFilter, msgs: &mut MsgQ) -> MarkId {
    let offs: Vec<(f64, f64)> = (0..spikes * 2)
        .map(|i| {
            let rad = if i % 2 == 0 { r } else { r * 0.42 };
            let a = (180. * i as f64 / spikes as f64 - 90.).to_radians();
            (rad * a.cos(), rad * a.sin())
        })
        .collect();
    poly(c, &offs, outline, fill, to, msgs)
}

/// An arrowhead pointing along `heading_deg`, for direction of travel.
fn chevron(c: Vector2, r: f64, heading_deg: f64, outline: Color, fill: Color, to: SideFilter, msgs: &mut MsgQ) -> MarkId {
    let h = heading_deg.to_radians();
    let (sn, cs) = (h.sin(), h.cos());
    let body = [
        (1.0, 0.0), (-0.2, 0.9), (-0.2, 0.35), (-1.0, 0.35),
        (-1.0, -0.35), (-0.2, -0.35), (-0.2, -0.9),
    ];
    let offs: Vec<(f64, f64)> = body
        .iter()
        .map(|&(f, rt)| {
            let (f, rt) = (f * r, rt * r);
            (f * cs - rt * sn, f * sn + rt * cs)
        })
        .collect();
    poly(c, &offs, outline, fill, to, msgs)
}

/// Status colour for a 0-100 value, matching the objective hexes exactly.
fn bucket_color(v: u8) -> Color {
    if v > 66 {
        Color::new(0.20, 0.85, 0.31, 1.)
    } else if v > 33 {
        Color::new(1., 0.70, 0., 1.)
    } else {
        Color::new(0.95, 0.16, 0.16, 1.)
    }
}

/// Outline for every status shape -- dark, so the fill reads on any terrain.
fn shape_outline() -> Color {
    Color::black(0.80)
}

fn v3(x: f64, y: f64) -> LuaVec3 {
    LuaVec3(Vector3::new(x, 0., y))
}


/// Build a 2-D cardinal "box" around a midpoint to approximate a racetrack
/// orbit.  `half_len` is the half-length along the major axis, `half_wid`
/// the half-width.  Returns (p0, p1, p2, p3) clockwise.
fn racetrack_quad(center: Vector2, heading_deg: f64, half_len: f64, half_wid: f64)
    -> (Vector2, Vector2, Vector2, Vector2)
{
    let hdg = heading_deg.to_radians();
    let fwd = Vector2::new(hdg.sin(), hdg.cos());
    let right = Vector2::new(hdg.cos(), -hdg.sin());
    let p0 = center + fwd * half_len - right * half_wid;
    let p1 = center + fwd * half_len + right * half_wid;
    let p2 = center - fwd * half_len + right * half_wid;
    let p3 = center - fwd * half_len - right * half_wid;
    (p0, p1, p2, p3)
}

/// Compute a start and end point for an arrow that represents a convoy's
/// direction of travel, centred on `pos` with a fixed length of `len_m`.
fn direction_arrow(pos: Vector2, heading_deg: f64, len_m: f64) -> (Vector2, Vector2) {
    let hdg = heading_deg.to_radians();
    let dir = Vector2::new(hdg.sin(), hdg.cos());
    (pos - dir * (len_m * 0.5), pos + dir * (len_m * 0.5))
}

// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
// Per-feature mark bundles
// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

#[derive(Debug)]
struct ConvoyMarks {
    /// Single map-marker pin riding with the convoy. Replaces the old
    /// origin->destination line + 3 km direction arrow + floating text label,
    /// which stacked into serious F10 clutter once several convoys were
    /// rolling. The pin collapses to an icon until clicked.
    pin: MarkId,
    /// Pin label text, kept so the pin can be re-dropped at a new position
    /// (DCS map pins can't be moved in place).
    text: dcso3::String,
    /// Cache of last known position so the pin is only re-dropped after the
    /// convoy has travelled a meaningful distance.
    last_pos: Vector2,
    /// Arrowhead pointing along the convoy's heading, so direction of travel
    /// reads off the map without clicking the pin open.
    heading_arrow: MarkId,
}

impl ConvoyMarks {
    fn new(
        _origin: Vector2,
        _destination: Vector2,
        current_pos: Vector2,
        side: Side,
        heading_deg: f64,
        cargo_label: impl Into<dcso3::String>,
        msgs: &mut MsgQ,
    ) -> Self {
        let text = cargo_label.into();
        let pin = msgs.mark_to_all(current_pos, true, text.clone());
        let heading_arrow = chevron(
            current_pos,
            900.,
            heading_deg,
            shape_outline(),
            side_color(side, 0.95),
            SideFilter::All,
            msgs,
        );
        Self { pin, text, last_pos: current_pos, heading_arrow }
    }

    /// Call when the convoy moves. Re-drops the pin at the new position, but
    /// only once the convoy has moved far enough to matter -- a map pin can't
    /// be repositioned in place, so each move is a delete + re-add.
    fn on_move(&mut self, new_pos: Vector2, heading_deg: f64, side: Side, msgs: &mut MsgQ) {
        if (new_pos - self.last_pos).norm() < 3_000. {
            return;
        }
        self.last_pos = new_pos;
        msgs.delete_mark(self.pin);
        self.pin = msgs.mark_to_all(new_pos, true, self.text.clone());
        msgs.delete_mark(self.heading_arrow);
        self.heading_arrow = chevron(
            new_pos,
            900.,
            heading_deg,
            shape_outline(),
            side_color(side, 0.95),
            SideFilter::All,
            msgs,
        );
    }
}

#[derive(Debug)]
struct AirRouteMarks {
    /// Dashed line from origin airbase to orbit entry point
    transit_line: MarkId,
    /// Rect approximating the racetrack orbit box
    orbit_rect: MarkId,
    /// Circle: radar coverage bubble (AWACS only; drawn invisible for tankers)
    coverage: MarkId,
    /// Text inside the orbit: callsign, frequency, type, altitude
    label: MarkId,
}

impl AirRouteMarks {
    fn new(
        origin: Vector2,
        orbit_center: Vector2,
        side: Side,
        coverage_m: f64,
        label_text: impl Into<dcso3::String>,
        msgs: &mut MsgQ,
    ) -> Self {
        let sf = side_filter(side);
        let col = side_color(side, 0.7);
        let fill_faint = side_color(side, 0.03);

        let transit_line = MarkId::new();
        msgs.line_to_all(
            sf,
            transit_line,
            LineSpec {
                start: v3(origin.x, origin.y),
                end: v3(orbit_center.x, orbit_center.y),
                color: side_color(side, 0.4),
                line_type: LineType::Dotted,
                read_only: true,
            },
            None,
        );

        let (p0, p1, p2, p3) = racetrack_quad(orbit_center, 90., 20_000., 5_000.);
        let orbit_rect = MarkId::new();
        msgs.quad_to_all(
            sf,
            orbit_rect,
            QuadSpec {
                p0: v3(p0.x, p0.y),
                p1: v3(p1.x, p1.y),
                p2: v3(p2.x, p2.y),
                p3: v3(p3.x, p3.y),
                color: col,
                fill_color: fill_faint,
                line_type: LineType::DotDash,
                read_only: true,
            },
            None,
        );

        // Coverage circle: visible for AWACS (large range), near-invisible for tankers
        let coverage = MarkId::new();
        let cov_alpha = if coverage_m > 50_000. { 0.04 } else { 0. };
        msgs.circle_to_all(
            sf,
            coverage,
            CircleSpec {
                center: v3(orbit_center.x, orbit_center.y),
                radius: coverage_m,
                color: side_color(side, if coverage_m > 50_000. { 0.3 } else { 0. }),
                fill_color: side_color(side, cov_alpha),
                line_type: LineType::LongDash,
                read_only: true,
            },
            None,
        );

        let label = MarkId::new();
        msgs.text_to_all(
            sf,
            label,
            TextSpec {
                pos: v3(orbit_center.x, orbit_center.y),
                color: col,
                fill_color: crate::mapcolor::text_plate(),
                font_size: 10,
                read_only: true,
                text: label_text.into(),
            },
        );

        Self { transit_line, orbit_rect, coverage, label }
    }

}

#[derive(Debug)]
struct SeaRouteMarks {
    /// Dotted line from origin port to destination
    route: MarkId,
    /// Arrow showing direction of travel
    arrow: MarkId,
    /// Text: cargo + ship type
    label: MarkId,
}

impl SeaRouteMarks {
    fn new(
        origin: Vector2,
        destination: Vector2,
        side: Side,
        label_text: impl Into<dcso3::String>,
        msgs: &mut MsgQ,
    ) -> Self {
        let sf = side_filter(side);
        let col = side_color(side, 0.6);
        let midpoint = (origin + destination) * 0.5;

        let route = MarkId::new();
        msgs.line_to_all(
            sf,
            route,
            LineSpec {
                start: v3(origin.x, origin.y),
                end: v3(destination.x, destination.y),
                color: col,
                line_type: LineType::Dotted,
                read_only: true,
            },
            None,
        );

        let (a_start, a_end) = direction_arrow(midpoint, 0., 5_000.);
        let arrow = MarkId::new();
        msgs.arrow_to(
            sf,
            arrow,
            ArrowSpec {
                start: v3(a_start.x, a_start.y),
                end: v3(a_end.x, a_end.y),
                color: col,
                fill_color: col,
                line_type: LineType::Solid,
                read_only: true,
            },
            None,
        );

        let label = MarkId::new();
        msgs.text_to_all(
            sf,
            label,
            TextSpec {
                pos: v3(midpoint.x + 500., midpoint.y + 500.),
                color: col,
                fill_color: crate::mapcolor::text_plate(),
                font_size: 9,
                read_only: true,
                text: label_text.into(),
            },
        );

        Self { route, arrow, label }
    }
}

/// Temporary fire-mission overlay.  Auto-expires after `FIRE_MARK_TTL`.
#[derive(Debug)]
pub struct FireOverlay {
    /// Dashed line from nearest gun to target
    trajectory: MarkId,
    /// Solid circle at the impact zone
    impact: MarkId,
    /// Text at impact: grid, guns count, radius
    label: MarkId,
    /// When to auto-remove this overlay
    expires: DateTime<Utc>,
}

const FIRE_MARK_TTL: i64 = 300; // seconds

impl FireOverlay {
    /// Draw a new fire mission overlay.
    ///
    /// * `gun_pos`    â€“ centroid of the firing battery
    /// * `target_pos` â€“ impact point
    /// * `radius_m`   â€“ burst radius
    /// * `gun_count`  â€“ number of firing groups
    /// * `side`       â€“ which coalition is firing
    pub fn new(
        gun_pos: Vector2,
        target_pos: Vector2,
        radius_m: f64,
        gun_count: u32,
        side: Side,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) -> Self {
        let col = side_color(side, 1.);

        let trajectory = MarkId::new();
        msgs.line_to_all(
            SideFilter::All,
            trajectory,
            LineSpec {
                start: v3(gun_pos.x, gun_pos.y),
                end: v3(target_pos.x, target_pos.y),
                color: side_color(side, 0.6),
                line_type: LineType::Dashed,
                read_only: true,
            },
            None,
        );

        let impact = MarkId::new();
        msgs.circle_to_all(
            SideFilter::All,
            impact,
            CircleSpec {
                center: v3(target_pos.x, target_pos.y),
                radius: radius_m.max(500.),
                color: col,
                fill_color: Color::new(1., 0.5, 0., 0.15),
                line_type: LineType::Dashed,
                read_only: true,
            },
            None,
        );

        // A burst glyph at the aimpoint instead of a three-line caption. The
        // impact circle already carries position and radius; the gun count and
        // firing side were never something a pilot could act on mid-flight,
        // and this label was the noisiest mark on the map.
        let _ = gun_count;
        let label = burst(
            target_pos,
            (radius_m * 0.45).clamp(300., 1200.),
            8,
            shape_outline(),
            col,
            SideFilter::All,
            msgs,
        );

        Self {
            trajectory,
            impact,
            label,
            expires: now + Duration::seconds(FIRE_MARK_TTL),
        }
    }

}


#[derive(Debug)]
struct CsarMarks {
    /// Dashed white circle â€” search area around downed pilot
    search_ring: MarkId,
    /// Map pin carrying the pilot's name and the exact time remaining.
    label: MarkId,
    /// Hexagon that ripens green -> amber -> red as the timer runs down, so
    /// a rescue flight reads "how long have I got" without opening the pin.
    urgency_hex: MarkId,
}

impl CsarMarks {
    fn new(
        pos: Vector2,
        side: Side,
        label_text: impl Into<dcso3::String>,
        msgs: &mut MsgQ,
    ) -> Self {
        let sf = side_filter(side);

        let search_ring = MarkId::new();
        msgs.circle_to_all(
            sf,
            search_ring,
            CircleSpec {
                center: v3(pos.x, pos.y),
                radius: 5_000.,
                color: Color::white(0.8),
                fill_color: Color::white(0.03),
                line_type: LineType::Dashed,
                read_only: true,
            },
            None,
        );

        // A hexagon that ripens as the capture timer runs down, instead of a
        // live countdown in text. The pilot's name and exact time remaining
        // move into a pin, so nothing is lost -- but a rescue flight can see
        // how long it has without stopping to read.
        let urgency_hex = ngon(
            Vector2::new(pos.x - 2_200., pos.y),
            700.,
            6,
            30.,
            shape_outline(),
            bucket_color(100),
            sf,
            msgs,
        );
        let label = msgs.mark_to_side(side, pos, true, label_text);

        Self { search_ring, label, urgency_hex }
    }

    /// Change the ring border color as the capture timer runs down:
    /// white â†’ yellow â†’ red.
    fn set_urgency(&self, level: UrgencyLevel, msgs: &mut MsgQ) {
        let (border, fill) = match level {
            UrgencyLevel::Low => (Color::white(0.8), Color::white(0.03)),
            UrgencyLevel::Medium => (Color::yellow(0.9), Color::yellow(0.04)),
            UrgencyLevel::High => (Color::red(1.), Color::red(0.06)),
        };
        msgs.set_markup_color(self.search_ring, border);
        msgs.set_markup_fill_color(self.search_ring, fill);
        msgs.set_markup_fill_color(
            self.urgency_hex,
            bucket_color(match level {
                UrgencyLevel::Low => 100,
                UrgencyLevel::Medium => 50,
                UrgencyLevel::High => 10,
            }),
        );
    }

    /// DCS map pins cannot be re-texted in place, so the countdown pin is
    /// dropped and re-dropped. This runs on the slow tick, and the urgency
    /// hex -- not the pin -- is what a pilot actually reads in flight.
    fn update_label(
        &mut self,
        pos: Vector2,
        side: Side,
        text: impl Into<dcso3::String>,
        msgs: &mut MsgQ,
    ) {
        msgs.delete_mark(self.label);
        self.label = msgs.mark_to_side(side, pos, true, text);
    }

}

#[derive(Debug, Clone, Copy)]
pub enum UrgencyLevel {
    Low,
    Medium,
    High,
}

/// Per-JTAC layer marks that supplement the existing `JtacTarget.mark` in
/// jtac.rs.  Keyed by the JTAC `GroupId`.
#[derive(Debug)]
pub struct JtacLayerMarks {
    /// Straight line from JTAC to target -- bearing / range aid. The one
    /// drawn primitive kept for the JTAC layer.
    bearing_line: MarkId,
    /// Map-marker pin at the target carrying the 9-line text. Replaces the
    /// old lase-range circle + target circle + floating text panel, which
    /// were a big share of the F10 clutter around an active JTAC.
    info_pin: MarkId,
    /// Diamond drawn on the lased target.
    target_shape: MarkId,
    /// The laser code, the one thing a pilot must read without clicking.
    code_label: MarkId,
    /// Side the pin is shown to, kept so it can be re-dropped on target move
    /// (DCS map pins can't be moved in place).
    side: Side,
    /// Cached target position for movement detection
    last_target: Vector2,
    /// Cached JTAC position for movement detection
    last_jtac: Vector2,
}

impl JtacLayerMarks {
    pub fn new(
        jtac_pos: Vector2,
        target_pos: Vector2,
        _lase_range_m: f64,
        side: Side,
        nine_line_text: impl Into<dcso3::String>,
        laser_code: u16,
        msgs: &mut MsgQ,
    ) -> Self {
        let sf = side_filter(side);

        let bearing_line = MarkId::new();
        msgs.line_to_all(
            sf,
            bearing_line,
            LineSpec {
                start: v3(jtac_pos.x, jtac_pos.y),
                end: v3(target_pos.x, target_pos.y),
                color: Color::violet(0.6),
                line_type: LineType::DotDash,
                read_only: true,
            },
            None,
        );

        let info_pin = msgs.mark_to_side(side, target_pos, true, nine_line_text);

        // A diamond ON the target, so the thing being lased is a shape you
        // can see rather than the bare end of a line, plus the laser code as
        // the ONE piece of text -- it is what a pilot has to dial in, and
        // digging it out of a pin mid-run is exactly the wrong moment.
        let target_shape = ngon(
            target_pos,
            700.,
            4,
            0.,
            shape_outline(),
            Color::violet(0.95),
            sf,
            msgs,
        );
        let code_label = MarkId::new();
        msgs.text_to_all(
            sf,
            code_label,
            TextSpec {
                pos: v3(target_pos.x - 1_600., target_pos.y),
                color: Color::violet(1.),
                fill_color: crate::mapcolor::text_plate(),
                font_size: 12,
                read_only: true,
                text: format_compact!("{}", laser_code).into(),
            },
        );

        Self {
            bearing_line,
            target_shape,
            code_label,
            info_pin,
            side,
            last_target: target_pos,
            last_jtac: jtac_pos,
        }
    }

    /// Call whenever the target position changes (e.g. target moves or JTAC
    /// shifts to a new contact).  Only pushes the update commands that are
    /// actually needed.
    pub fn on_target_move(
        &mut self,
        new_target: Vector2,
        new_nine_line: impl Into<dcso3::String>,
        msgs: &mut MsgQ,
    ) {
        if (new_target - self.last_target).norm() < 20. {
            return;
        }
        self.last_target = new_target;
        msgs.set_markup_pos_end(self.bearing_line, v3(new_target.x, new_target.y));
        // Pins can't be repositioned in place -- drop and re-drop it.
        msgs.delete_mark(self.info_pin);
        msgs.delete_mark(self.target_shape);
        msgs.delete_mark(self.code_label);
        self.info_pin = msgs.mark_to_side(self.side, new_target, true, new_nine_line);
    }

    /// Call when the JTAC itself moves (airborne JTAC / drone).
    pub fn on_jtac_move(&mut self, new_jtac: Vector2, msgs: &mut MsgQ) {
        if (new_jtac - self.last_jtac).norm() < 50. {
            return;
        }
        self.last_jtac = new_jtac;
        msgs.set_markup_pos_start(self.bearing_line, v3(new_jtac.x, new_jtac.y));
    }

    pub fn remove(self, msgs: &mut MsgQ) {
        msgs.delete_mark(self.bearing_line);
        msgs.delete_mark(self.info_pin);
    }
}

// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
// MapLayer â€” top-level owner of all marks
// â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

/// The two marks that make up one coalition tasking board entry: the area
/// circle and the pin carrying its details. Posted from the Actions menu,
/// see `crate::db::tasks`.
#[derive(Debug)]
struct TaskMarks {
    area: MarkId,
    pin: MarkId,
    /// Gold star over the task circle, so a tasked objective stands out from
    /// the ordinary rings around it.
    star: MarkId,
}

impl TaskMarks {
    fn new(task: &crate::db::tasks::Task, msgs: &mut MsgQ) -> Self {
        let [r, g, b, a] = task.color;
        let color = Color::new(r, g, b, a);
        let area = MarkId::new();
        msgs.circle_to_all(
            side_filter(task.side),
            area,
            CircleSpec {
                center: v3(task.pos.x, task.pos.y),
                radius: task.radius,
                color,
                fill_color: Color::new(r, g, b, 0.05),
                line_type: LineType::Dashed,
                read_only: true,
            },
            None,
        );
        let pin = msgs.mark_to_side(task.side, task.pos, true, task.pin_text().as_str());
        // A star on top of the task circle. The circle alone is easy to miss
        // among the objective rings; the star says "somebody asked for this".
        let star_id = burst(
            task.pos,
            1_000.,
            5,
            shape_outline(),
            Color::new(1., 0.82, 0.29, 1.),
            side_filter(task.side),
            msgs,
        );
        Self { area, pin, star: star_id }
    }

    fn remove(self, msgs: &mut MsgQ) {
        msgs.delete_mark(self.area);
        msgs.delete_mark(self.pin);
        msgs.delete_mark(self.star);
    }
}

/// Generic timed mark bundle â€” up to 3 MarkIds that expire together.
#[derive(Debug)]
struct TimedMark {
    ids: [Option<MarkId>; 3],
    expires: DateTime<Utc>,
}

impl TimedMark {
    fn one(id: MarkId, ttl_secs: i64, now: DateTime<Utc>) -> Self {
        Self { ids: [Some(id), None, None], expires: now + Duration::seconds(ttl_secs) }
    }
    #[allow(dead_code)]
    fn two(a: MarkId, b: MarkId, ttl_secs: i64, now: DateTime<Utc>) -> Self {
        Self { ids: [Some(a), Some(b), None], expires: now + Duration::seconds(ttl_secs) }
    }
    #[allow(dead_code)]
    fn three(a: MarkId, b: MarkId, c: MarkId, ttl_secs: i64, now: DateTime<Utc>) -> Self {
        Self { ids: [Some(a), Some(b), Some(c)], expires: now + Duration::seconds(ttl_secs) }
    }
    fn remove(self, msgs: &mut MsgQ) {
        for id in self.ids.into_iter().flatten() {
            msgs.delete_mark(id);
        }
    }
}

/// A timed group of arbitrarily many marks, for shapes drawn in a row.
#[derive(Debug)]
struct TimedGroup {
    ids: Vec<MarkId>,
    expires: DateTime<Utc>,
}

impl TimedGroup {
    fn new(ids: Vec<MarkId>, ttl_secs: i64, now: DateTime<Utc>) -> Self {
        Self { ids, expires: now + Duration::seconds(ttl_secs) }
    }
    fn remove(self, msgs: &mut MsgQ) {
        for id in self.ids {
            msgs.delete_mark(id);
        }
    }
}

#[derive(Debug, Default)]
pub struct MapLayer {
    convoy_marks: FxHashMap<ConvoyId, ConvoyMarks>,
    air_route_marks: FxHashMap<LogiRouteId, AirRouteMarks>,
    sea_route_marks: FxHashMap<LogiRouteId, SeaRouteMarks>,
    fire_marks: Vec<FireOverlay>,
    /// Multi-shape transient overlays (recon strength rows).
    timed_groups: Vec<TimedGroup>,
    csar_marks: FxHashMap<GroupId, CsarMarks>,
    pub jtac_marks: FxHashMap<GroupId, JtacLayerMarks>,
    supply_critical_marks: FxHashMap<ObjectiveId, MarkId>,
    task_marks: FxHashMap<TaskId, TaskMarks>,
    timed_marks: Vec<TimedMark>,
}

impl MapLayer {
    // â”€â”€ Fire missions (explicit event) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    /// Draw a fire-mission overlay.  Call this from `db/actions.rs` instead
    /// of the inline `circle_to_all` that currently lives there.
    pub fn on_fire_mission(
        &mut self,
        gun_pos: Vector2,
        target_pos: Vector2,
        radius_m: f64,
        gun_count: u32,
        side: Side,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) {
        let overlay = FireOverlay::new(
            gun_pos, target_pos, radius_m, gun_count, side, now, msgs,
        );
        self.fire_marks.push(overlay);
    }

    // â”€â”€ JTAC layer (explicit events from jtac.rs) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    /// Draw JTAC layer marks when a target is acquired.
    pub fn on_jtac_target(
        &mut self,
        gid: GroupId,
        jtac_pos: Vector2,
        target_pos: Vector2,
        lase_range_m: f64,
        side: Side,
        nine_line_text: impl Into<dcso3::String>,
        laser_code: u16,
        msgs: &mut MsgQ,
    ) {
        if let Some(old) = self.jtac_marks.remove(&gid) {
            old.remove(msgs);
        }
        let marks = JtacLayerMarks::new(
            jtac_pos, target_pos, lase_range_m, side, nine_line_text, laser_code, msgs,
        );
        self.jtac_marks.insert(gid, marks);
    }

    /// Remove JTAC layer marks when target is cleared.
    pub fn on_jtac_cleared(&mut self, gid: &GroupId, msgs: &mut MsgQ) {
        if let Some(m) = self.jtac_marks.remove(gid) {
            m.remove(msgs);
        }
    }

    // â”€â”€ Supply critical warnings â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    /// Draw a persistent F10 map marker when an objective's supply is critical.
    ///
    /// `detail` names what is actually short and by how much ("munitions 18% /
    /// fuel 9%"), taken from the same three numbers the objective's own label
    /// shows -- see the caller in `logistics.rs`.
    /// Supply-critical used to drop a `! LOW SUPPLY` label at the objective.
    /// That is now redundant: the objective's own supply/fuel status hexes go
    /// red at the same threshold, in the same place, without a line of text.
    /// Kept as a no-op so the logistics caller does not need to know.
    pub fn on_supply_critical(
        &mut self,
        _oid: ObjectiveId,
        _pos: Vector2,
        _side: Side,
        _name: &str,
        _detail: &str,
        _msgs: &mut MsgQ,
    ) {
    }

    /// Remove the supply-critical marker when supply has recovered.
    pub fn on_supply_recovered(&mut self, oid: &ObjectiveId, msgs: &mut MsgQ) {
        if let Some(mark) = self.supply_critical_marks.remove(oid) {
            msgs.delete_mark(mark);
        }
    }

    // â”€â”€ Transient tactical events â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    /// Dotted square bounding the recon scan area + text label.
    pub fn on_recon_result(
        &mut self,
        target_pos: Vector2,
        scan_radius_m: f64,
        unit_count: usize,
        side: Side,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) {
        let sf = side_filter(side);
        let col = side_color(side, 0.85);
        let h = scan_radius_m;
        let rect = MarkId::new();
        msgs.rect_to_all(
            sf,
            rect,
            RectSpec {
                start: v3(target_pos.x - h, target_pos.y - h),
                end:   v3(target_pos.x + h, target_pos.y + h),
                color: col,
                fill_color: Color::new(1., 1., 0., 0.04),
                line_type: LineType::Dotted,
                read_only: true,
            },
            None,
        );
        // Strength as a row of diamonds -- one per five units, capped at five
        // -- so the SCALE of a contact reads at a glance. The exact count is
        // not thrown away: it moves into a map pin, which stays collapsed to
        // an icon until somebody clicks it. Shapes to glance at, pin for detail.
        let pips = (unit_count / 5).clamp(1, 5);
        let mut ids: Vec<MarkId> = (0..pips)
            .map(|i| {
                let c = Vector2::new(
                    target_pos.x - scan_radius_m * 0.55,
                    target_pos.y + (i as f64 - (pips as f64 - 1.) / 2.) * 1500.,
                );
                ngon(c, 620., 4, 0., shape_outline(), col, sf, msgs)
            })
            .collect();
        ids.push(rect);
        self.timed_groups.push(TimedGroup::new(ids, 120, now));
        let pin = msgs.mark_to_side(
            side,
            target_pos,
            true,
            format_compact!("RECON\n~{} enemy units", unit_count).as_str(),
        );
        self.timed_marks.push(TimedMark::one(pin, 120, now));
    }

    /// Place or refresh an F10 map marker for an ELINT/SIGINT intel contact.
    /// The marker label and opacity track the contact's confidence score.
    /// Old marks are deleted and replaced when refreshed.
    pub fn update_intel_contact_mark(
        &mut self,
        contact: &mut IntelContact,
        cfg: &bfprotocols::cfg::ElintConfig,
        msgs: &mut MsgQ,
    ) {
        // Remove stale marks first.
        if let Some(rect_id) = contact.map_mark_rect.take() {
            msgs.delete_mark(rect_id);
        }
        if let Some(label_id) = contact.map_mark_label.take() {
            msgs.delete_mark(label_id);
        }
        if let Some(ring_id) = contact.map_mark_ring.take() {
            msgs.delete_mark(ring_id);
        }

        // The mark is shown only to the side that owns the intel.
        let sf = side_filter(contact.side);
        // Color fades from bright hostile-red towards dark as confidence drops.
        let alpha = (contact.confidence * 0.9 + 0.05).clamp(0.1, 0.95);
        let fill_alpha = alpha * 0.15;
        let (enemy_col, fill_col) = match contact.side {
            Side::Blue => (Color::new(0.9, 0.2, 0.2, alpha), Color::new(0.9, 0.2, 0.2, fill_alpha)),
            Side::Red  => (Color::new(0.2, 0.4, 0.9, alpha), Color::new(0.2, 0.4, 0.9, fill_alpha)),
            _          => (Color::white(alpha),                Color::white(fill_alpha)),
        };
        let pos = contact.pos;

        // The SHAPE carries the class, so one look tells you what kind of thing
        // is there: diamond = air defence, square = armour, triangle = infantry,
        // hexagon = artillery, larger hexagon = naval or airbase. Anything
        // unidentified stays a near-circular octagon, which reads as "something
        // here, not yet classified".
        let (sides, rot, r) = match contact.unit_class {
            IntelUnitClass::AirDefense => (4usize, 0., 620.),
            IntelUnitClass::Armor => (4, 45., 560.),
            IntelUnitClass::Infantry => (3, 90., 640.),
            IntelUnitClass::Artillery => (6, 0., 560.),
            IntelUnitClass::Naval | IntelUnitClass::AirBase => (6, 30., 700.),
            IntelUnitClass::Unknown => (8, 0., 520.),
        };
        let shape_id = ngon(pos, r, sides, rot, enemy_col, fill_col, sf, msgs);

        // Confidence stops being a word and becomes the position-uncertainty
        // ring: the bigger the ring, the less sure the engine is about where
        // this actually is. That is the part a pilot has to fly against.
        let unc = (contact.pos_uncertainty_m as f64).clamp(600., 12_000.);
        let ring_id = MarkId::new();
        msgs.circle_to_all(
            sf,
            ring_id,
            CircleSpec {
                center: v3(pos.x, pos.y),
                radius: unc,
                color: enemy_col,
                fill_color: fill_col,
                line_type: LineType::Dashed,
                read_only: true,
            },
            None,
        );

        // Class, count, source, confidence and age all still exist -- they move
        // into a pin, collapsed to an icon until clicked, rather than a caption
        // stacked over every contact on the map.
        let pin_id = msgs.mark_to_side(
            contact.side,
            pos,
            true,
            IntelDatabase::marker_text(contact, cfg).as_str(),
        );

        contact.map_mark_rect = Some(shape_id);
        contact.map_mark_label = Some(pin_id);
        contact.map_mark_ring = Some(ring_id);
    }

    /// Remove F10 map marks for a deleted intel contact.
    pub fn remove_intel_contact_marks(
        &mut self,
        shape: Option<dcso3::trigger::MarkId>,
        pin: Option<dcso3::trigger::MarkId>,
        ring: Option<dcso3::trigger::MarkId>,
        msgs: &mut MsgQ,
    ) {
        for id in [shape, pin, ring].into_iter().flatten() {
            msgs.delete_mark(id);
        }
    }


    /// "ENEMY CONTACT" label at an objective that just became threatened.
    /// Previously also drew a fixed-bearing "axis of advance" arrow into the
    /// objective, but the bearing was never the real threat direction (just
    /// a universal NE-converging placeholder), so it read as a stray colored
    /// arrow with no useful information -- dropped, keeping the label.
    /// Threat used to drop an `ENEMY CONTACT` label at the objective. The
    /// objective's yellow threatened ring already means exactly this, at the
    /// same position, so the label was drawing the same fact twice. No-op.
    pub fn on_objective_threatened(
        &mut self,
        _obj_pos: Vector2,
        _side: Side,
        _obj_name: &str,
        _now: DateTime<Utc>,
        _msgs: &mut MsgQ,
    ) {
    }

    /// "UNDER ATTACK" label at an objective that is actively under attack.
    /// Previously also drew two converging NW/NE arrows (a hasty-attack
    /// symbol), but that duplicated the single "threatened" arrow and
    /// cluttered the map, so it's label-only now.
    pub fn on_objective_under_attack(
        &mut self,
        obj_pos: Vector2,
        side: Side,
        obj_name: &str,
        ttl_secs: i64,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) {
        // A thick red ring around the objective instead of an `UNDER ATTACK`
        // label. The objective already carries a yellow ring for "threatened";
        // escalating the same shape to red reads faster than a second floating
        // caption, and it costs one mark that expires with the event.
        let _ = obj_name;
        let sf = side_filter(side);
        let ring = MarkId::new();
        msgs.circle_to_all(
            sf,
            ring,
            CircleSpec {
                center: v3(obj_pos.x, obj_pos.y),
                radius: UNDER_ATTACK_RING_M,
                color: Color::new(0.95, 0.16, 0.16, 1.),
                fill_color: Color::new(0.95, 0.16, 0.16, 0.06),
                line_type: LineType::Solid,
                read_only: true,
            },
            None,
        );
        self.timed_marks.push(TimedMark::one(ring, ttl_secs, now));
    }



    fn expire_timed_marks(&mut self, now: DateTime<Utc>, msgs: &mut MsgQ) {
        let mut i = 0;
        while i < self.timed_marks.len() {
            if now >= self.timed_marks[i].expires {
                let m = self.timed_marks.swap_remove(i);
                m.remove(msgs);
            } else {
                i += 1;
            }
        }
        let mut i = 0;
        while i < self.timed_groups.len() {
            if now >= self.timed_groups[i].expires {
                let g = self.timed_groups.swap_remove(i);
                g.remove(msgs);
            } else {
                i += 1;
            }
        }
    }

    // â”€â”€ Full diff-based update (call from slow tick) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    /// Performs a full diff of the entire map layer against current DB state.
    ///
    /// * Draws new convoys / routes / SAM groups / missions / CSAR that have
    ///   appeared since the last call.
    /// * Updates moving elements (convoy arrows, sea route arrows) in-place.
    /// * Removes marks whose corresponding entity no longer exists.
    /// * Expires timed fire-mission overlays.
    pub fn update_all(
        &mut self,
        persisted: &Persisted,
        active_convoys: &FxHashMap<ConvoyId, SupplyConvoy>,
        active_air_routes: &FxHashMap<LogiRouteId, AirLogisticsRoute>,
        active_sea_routes: &FxHashMap<LogiRouteId, SeaLogisticsRoute>,
        csar_capture_mins: u32,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) {
        self.update_convoys(persisted, active_convoys, msgs);
        self.update_air_routes(persisted, active_air_routes, msgs);
        self.update_sea_routes(persisted, active_sea_routes, msgs);
        self.update_csar(persisted, csar_capture_mins, now, msgs);
        self.update_tasks(persisted, msgs);
        self.expire_fire_marks(now, msgs);
        self.expire_timed_marks(now, msgs);
    }

    // â”€â”€ Ground convoys â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    fn update_convoys(
        &mut self,
        persisted: &Persisted,
        active_convoys: &FxHashMap<ConvoyId, SupplyConvoy>,
        msgs: &mut MsgQ,
    ) {
        // Draw new convoys
        for (id, convoy) in active_convoys {
            if self.convoy_marks.contains_key(id) {
                // Update arrow position
                let marks = self.convoy_marks.get_mut(id).unwrap();
                // Estimate heading from last_pos toward destination
                let dst_pos = persisted
                    .objectives
                    .get(&convoy.destination)
                    .map(|o| o.zone.pos())
                    .unwrap_or(convoy.last_pos);
                let delta = dst_pos - convoy.last_pos;
                let hdg = if delta.norm() > 1. {
                    delta.y.atan2(delta.x).to_degrees()
                } else {
                    0.
                };
                marks.on_move(convoy.last_pos, hdg, convoy.side, msgs);
            } else {
                let origin_pos = persisted
                    .objectives
                    .get(&convoy.origin)
                    .map(|o| o.zone.pos())
                    .unwrap_or(convoy.last_pos);
                let dst_pos = persisted
                    .objectives
                    .get(&convoy.destination)
                    .map(|o| o.zone.pos())
                    .unwrap_or(convoy.last_pos);
                let cargo_str = format_compact!(
                    "Convoy [{:?}]\n{:?} â†’ {}",
                    convoy.side,
                    convoy.cargo_type,
                    persisted
                        .objectives
                        .get(&convoy.destination)
                        .map(|o| o.name.as_str())
                        .unwrap_or("unknown")
                );
                // Same heading estimate the update path uses: toward the
                // destination, so the arrow points where the convoy is going
                // rather than where it happens to be facing this instant.
                let delta = dst_pos - convoy.last_pos;
                let hdg = if delta.norm() > 1. {
                    delta.y.atan2(delta.x).to_degrees()
                } else {
                    0.
                };
                let marks = ConvoyMarks::new(
                    origin_pos,
                    dst_pos,
                    convoy.last_pos,
                    convoy.side,
                    hdg,
                    cargo_str,
                    msgs,
                );
                self.convoy_marks.insert(id.clone(), marks);
            }
        }

        // Remove stale convoy marks
        self.convoy_marks.retain(|id, marks| {
            if !active_convoys.contains_key(id.as_str()) {
                msgs.delete_mark(marks.pin);
                msgs.delete_mark(marks.heading_arrow);
                false
            } else {
                true
            }
        });
    }

    // â”€â”€ Air logistics routes â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    fn update_air_routes(
        &mut self,
        persisted: &Persisted,
        active_air_routes: &FxHashMap<LogiRouteId, AirLogisticsRoute>,
        msgs: &mut MsgQ,
    ) {
        for (id, route) in active_air_routes {
            if self.air_route_marks.contains_key(id) {
                continue;
            }
            let origin_pos = persisted
                .objectives
                .get(&route.origin)
                .map(|o| o.zone.pos())
                .unwrap_or_default();
            let dst_pos = persisted
                .objectives
                .get(&route.destination)
                .map(|o| o.zone.pos())
                .unwrap_or_default();
            // Get side from the group
            let side = persisted
                .groups
                .get(&route.group_id)
                .map(|g| g.side)
                .unwrap_or(Side::Neutral);

            let label = format_compact!(
                "Air Logi [{:?}]\n{:?} â†’ {}",
                side,
                route.cargo_type,
                persisted
                    .objectives
                    .get(&route.destination)
                    .map(|o| o.name.as_str())
                    .unwrap_or("unknown")
            );
            // Use 0 coverage for cargo aircraft (not AWACS)
            let marks =
                AirRouteMarks::new(origin_pos, dst_pos, side, 0., label, msgs);
            self.air_route_marks.insert(id.clone(), marks);
        }

        // Remove stale
        self.air_route_marks.retain(|id, marks| {
            if !active_air_routes.contains_key(id.as_str()) {
                msgs.delete_mark(marks.transit_line);
                msgs.delete_mark(marks.orbit_rect);
                msgs.delete_mark(marks.coverage);
                msgs.delete_mark(marks.label);
                false
            } else {
                true
            }
        });
    }

    // â”€â”€ Sea logistics routes â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    fn update_sea_routes(
        &mut self,
        persisted: &Persisted,
        active_sea_routes: &FxHashMap<LogiRouteId, SeaLogisticsRoute>,
        msgs: &mut MsgQ,
    ) {
        for (id, route) in active_sea_routes {
            if self.sea_route_marks.contains_key(id) {
                continue;
            }
            let origin_pos = persisted
                .objectives
                .get(&route.origin)
                .map(|o| o.zone.pos())
                .unwrap_or_default();
            let dst_pos = persisted
                .objectives
                .get(&route.destination)
                .map(|o| o.zone.pos())
                .unwrap_or_default();
            let side = persisted
                .groups
                .get(&route.group_id)
                .map(|g| g.side)
                .unwrap_or(Side::Neutral);

            let label = format_compact!(
                "Sea Logi [{:?}]\n{:?} â†’ {}",
                side,
                route.cargo_type,
                persisted
                    .objectives
                    .get(&route.destination)
                    .map(|o| o.name.as_str())
                    .unwrap_or("unknown")
            );
            let marks = SeaRouteMarks::new(origin_pos, dst_pos, side, label, msgs);
            self.sea_route_marks.insert(id.clone(), marks);
        }

        self.sea_route_marks.retain(|id, marks| {
            if !active_sea_routes.contains_key(id.as_str()) {
                msgs.delete_mark(marks.route);
                msgs.delete_mark(marks.arrow);
                msgs.delete_mark(marks.label);
                false
            } else {
                true
            }
        });
    }

    // â”€â”€ CSAR / downed pilots â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    fn update_csar(
        &mut self,
        persisted: &Persisted,
        csar_capture_mins: u32,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) {
        let mut live_pilots: FxHashSet<GroupId> = FxHashSet::default();
        let capture_secs = csar_capture_mins as i64 * 60;

        for gid in persisted.downed_pilots.into_iter() {
            let group = match persisted.groups.get(gid) {
                Some(g) => g,
                None => continue,
            };
            let name = match &group.origin {
                DeployKind::DownedPilot { name, .. } => name,
                _ => continue,
            };
            live_pilots.insert(*gid);

            let spawn_time = persisted
                .downed_pilot_spawn_times
                .get(gid)
                .copied()
                .unwrap_or(now);
            let elapsed_secs = (now - spawn_time).num_seconds().max(0);

            let (label, urgency) = if capture_secs > 0 {
                let remaining_secs = (capture_secs - elapsed_secs).max(0);
                let remaining_mins = remaining_secs / 60;
                let remaining_s = remaining_secs % 60;
                let frac = elapsed_secs as f64 / capture_secs as f64;
                let urgency = if frac >= 0.66 {
                    UrgencyLevel::High
                } else if frac >= 0.33 {
                    UrgencyLevel::Medium
                } else {
                    UrgencyLevel::Low
                };
                let lbl = format_compact!(
                    "CSAR\n{}\nCapture in {}:{:02}",
                    name,
                    remaining_mins,
                    remaining_s
                );
                (lbl, urgency)
            } else {
                (format_compact!("CSAR\n{}\nAwaiting rescue", name), UrgencyLevel::Low)
            };

            // Position of the downed pilot: first unit of the group, O(1).
            // Hoisted out of the else-branch because the update path now
            // needs it too -- a pin has to be re-dropped to change its text.
            let pos = group.units.into_iter()
                .next()
                .and_then(|uid| persisted.units.get(uid))
                .map(|u| u.pos)
                .unwrap_or_default();
            if self.csar_marks.contains_key(gid) {
                let marks = self.csar_marks.get_mut(gid).unwrap();
                marks.set_urgency(urgency, msgs);
                marks.update_label(pos, group.side, label, msgs);
            } else {
                let marks = CsarMarks::new(pos, group.side, label, msgs);
                self.csar_marks.insert(*gid, marks);
                let marks = self.csar_marks.get(gid).unwrap();
                marks.set_urgency(urgency, msgs);
            }
        }

        // Remove marks for rescued / captured pilots
        self.csar_marks.retain(|gid, marks| {
            if !live_pilots.contains(gid) {
                msgs.delete_mark(marks.search_ring);
                msgs.delete_mark(marks.label);
                msgs.delete_mark(marks.urgency_hex);
                false
            } else {
                true
            }
        });
    }

    // â”€â”€ Coalition tasking board â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    /// Draw tasks that have been posted since the last pass and erase the
    /// ones that have been removed or have expired. Diffing against
    /// `persisted` (rather than drawing at post time) is what redraws the
    /// whole board after a mission restart, when the DCS marks are gone but
    /// the tasks are still in the save.
    fn update_tasks(&mut self, persisted: &Persisted, msgs: &mut MsgQ) {
        for (id, task) in persisted.tasks.into_iter() {
            if !self.task_marks.contains_key(id) {
                self.task_marks.insert(*id, TaskMarks::new(task, msgs));
            }
        }
        self.task_marks.retain(|id, marks| {
            if persisted.tasks.get(id).is_some() {
                true
            } else {
                msgs.delete_mark(marks.area);
                msgs.delete_mark(marks.pin);
                false
            }
        });
    }

    // â”€â”€ Fire mark expiry â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    fn expire_fire_marks(&mut self, now: DateTime<Utc>, msgs: &mut MsgQ) {
        self.fire_marks.retain(|overlay| {
            if now >= overlay.expires {
                msgs.delete_mark(overlay.trajectory);
                msgs.delete_mark(overlay.impact);
                msgs.delete_mark(overlay.label);
                false
            } else {
                true
            }
        });
    }

    // â”€â”€ Full removal (e.g. on mission reset) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

    pub fn remove_all(&mut self, msgs: &mut MsgQ) {
        for (_, c) in self.convoy_marks.drain() {
            msgs.delete_mark(c.pin);
        }
        for (_, a) in self.air_route_marks.drain() {
            msgs.delete_mark(a.transit_line);
            msgs.delete_mark(a.orbit_rect);
            msgs.delete_mark(a.coverage);
            msgs.delete_mark(a.label);
        }
        for (_, s) in self.sea_route_marks.drain() {
            msgs.delete_mark(s.route);
            msgs.delete_mark(s.arrow);
            msgs.delete_mark(s.label);
        }
        for f in self.fire_marks.drain(..) {
            msgs.delete_mark(f.trajectory);
            msgs.delete_mark(f.impact);
            msgs.delete_mark(f.label);
        }
        for (_, c) in self.csar_marks.drain() {
            msgs.delete_mark(c.search_ring);
            msgs.delete_mark(c.label);
        }
        for (_, j) in self.jtac_marks.drain() {
            msgs.delete_mark(j.bearing_line);
            msgs.delete_mark(j.info_pin);
        }
        for (_, m) in self.supply_critical_marks.drain() {
            msgs.delete_mark(m);
        }
        for (_, t) in self.task_marks.drain() {
            t.remove(msgs);
        }
        for m in self.timed_marks.drain(..) {
            m.remove(msgs);
        }
        for g in self.timed_groups.drain(..) {
            g.remove(msgs);
        }
    }
}

