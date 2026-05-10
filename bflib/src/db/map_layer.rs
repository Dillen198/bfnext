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
//! module.  The goal is to use the full DCS draw toolkit — line, circle,
//! rect, quad, text, arrow, and mark pins — deliberately and consistently,
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
    logistics::{AirLogisticsRoute, ConvoyId, LogiRouteId, SeaLogisticsRoute, SupplyConvoy},
    persisted::Persisted,
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

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

fn side_color(side: Side, alpha: f32) -> Color {
    match side {
        Side::Red => Color::red(alpha),
        Side::Blue => Color::blue(alpha),
        Side::Neutral => Color::white(alpha),
    }
}

fn side_filter(side: Side) -> SideFilter {
    side.into()
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

// ─────────────────────────────────────────────────────────────────────────────
// Per-feature mark bundles
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug)]
struct ConvoyMarks {
    /// Long-dash line: origin → destination
    route: MarkId,
    /// Arrow at current convoy position showing direction of travel
    arrow: MarkId,
    /// Text label: cargo type + active status
    label: MarkId,
    /// Cache of last known position so we only push updates when it changes
    last_pos: Vector2,
}

impl ConvoyMarks {
    fn new(
        origin: Vector2,
        destination: Vector2,
        current_pos: Vector2,
        side: Side,
        cargo_label: impl Into<dcso3::String>,
        msgs: &mut MsgQ,
    ) -> Self {
        let sf = SideFilter::All;
        let col = side_color(side, 0.7);

        let route = MarkId::new();
        msgs.line_to_all(
            sf,
            route,
            LineSpec {
                start: v3(origin.x, origin.y),
                end: v3(destination.x, destination.y),
                color: col,
                line_type: LineType::LongDash,
                read_only: true,
            },
            None,
        );

        let (a_start, a_end) = direction_arrow(current_pos, 0., 3_000.);
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
                pos: v3(current_pos.x + 500., current_pos.y + 500.),
                color: col,
                fill_color: Color::black(0.4),
                font_size: 9,
                read_only: true,
                text: cargo_label.into(),
            },
        );

        Self { route, arrow, label, last_pos: current_pos }
    }

    /// Call when the convoy moves.  Updates the arrow and text positions
    /// in-place without a full redraw.
    fn on_move(&mut self, new_pos: Vector2, heading_deg: f64, msgs: &mut MsgQ) {
        if (new_pos - self.last_pos).norm() < 50. {
            return;
        }
        self.last_pos = new_pos;
        let (a_start, a_end) = direction_arrow(new_pos, heading_deg, 3_000.);
        msgs.set_markup_pos_start(self.arrow, v3(a_start.x, a_start.y));
        msgs.set_markup_pos_end(self.arrow, v3(a_end.x, a_end.y));
        msgs.set_markup_pos_start(self.label, v3(new_pos.x + 500., new_pos.y + 500.));
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
                fill_color: Color::black(0.45),
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
                fill_color: Color::black(0.4),
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
    /// * `gun_pos`    – centroid of the firing battery
    /// * `target_pos` – impact point
    /// * `radius_m`   – burst radius
    /// * `gun_count`  – number of firing groups
    /// * `side`       – which coalition is firing
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

        let label = MarkId::new();
        let txt = format_compact!(
            "FIRES [{:?}]\n{} gun group(s)\nRadius: {}m",
            side,
            gun_count,
            radius_m as u32
        );
        msgs.text_to_all(
            SideFilter::All,
            label,
            TextSpec {
                pos: v3(target_pos.x, target_pos.y),
                color: col,
                fill_color: Color::black(0.5),
                font_size: 11,
                read_only: true,
                text: txt.into(),
            },
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
    /// Dashed white circle — search area around downed pilot
    search_ring: MarkId,
    /// Text label with pilot name + capture countdown
    label: MarkId,
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

        let label = MarkId::new();
        msgs.text_to_all(
            sf,
            label,
            TextSpec {
                pos: v3(pos.x, pos.y),
                color: Color::white(0.9),
                fill_color: Color::black(0.5),
                font_size: 11,
                read_only: true,
                text: label_text.into(),
            },
        );

        Self { search_ring, label }
    }

    /// Change the ring border color as the capture timer runs down:
    /// white → yellow → red.
    fn set_urgency(&self, level: UrgencyLevel, msgs: &mut MsgQ) {
        let (border, fill) = match level {
            UrgencyLevel::Low => (Color::white(0.8), Color::white(0.03)),
            UrgencyLevel::Medium => (Color::yellow(0.9), Color::yellow(0.04)),
            UrgencyLevel::High => (Color::red(1.), Color::red(0.06)),
        };
        msgs.set_markup_color(self.search_ring, border);
        msgs.set_markup_fill_color(self.search_ring, fill);
    }

    fn update_label(&self, text: impl Into<dcso3::String>, msgs: &mut MsgQ) {
        msgs.set_markup_text(self.label, text.into());
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
    /// Large dashed circle at JTAC position: lasing / detection range
    lase_ring: MarkId,
    /// Small dotted circle at the lased target (~200 m radius)
    target_ring: MarkId,
    /// Straight line from JTAC to target — bearing / range aid
    bearing_line: MarkId,
    /// Rich text 9-line panel beside the target
    nine_line: MarkId,
    /// Cached target position for movement detection
    last_target: Vector2,
    /// Cached JTAC position for movement detection
    last_jtac: Vector2,
}

impl JtacLayerMarks {
    pub fn new(
        jtac_pos: Vector2,
        target_pos: Vector2,
        lase_range_m: f64,
        side: Side,
        nine_line_text: impl Into<dcso3::String>,
        msgs: &mut MsgQ,
    ) -> Self {
        let sf = side_filter(side);
        let col = side_color(side, 0.85);

        let lase_ring = MarkId::new();
        msgs.circle_to_all(
            sf,
            lase_ring,
            CircleSpec {
                center: v3(jtac_pos.x, jtac_pos.y),
                radius: lase_range_m,
                color: side_color(side, 0.3),
                fill_color: Color::white(0.),
                line_type: LineType::Dashed,
                read_only: true,
            },
            None,
        );

        let target_ring = MarkId::new();
        msgs.circle_to_all(
            sf,
            target_ring,
            CircleSpec {
                center: v3(target_pos.x, target_pos.y),
                radius: 200.,
                color: Color::red(0.9),
                fill_color: Color::new(1., 0., 0., 0.08),
                line_type: LineType::Dotted,
                read_only: true,
            },
            None,
        );

        let bearing_line = MarkId::new();
        msgs.line_to_all(
            sf,
            bearing_line,
            LineSpec {
                start: v3(jtac_pos.x, jtac_pos.y),
                end: v3(target_pos.x, target_pos.y),
                color: Color::red(0.6),
                line_type: LineType::DotDash,
                read_only: true,
            },
            None,
        );

        let nine_line = MarkId::new();
        msgs.text_to_all(
            sf,
            nine_line,
            TextSpec {
                pos: v3(target_pos.x + 500., target_pos.y + 500.),
                color: col,
                fill_color: Color::black(0.55),
                font_size: 10,
                read_only: true,
                text: nine_line_text.into(),
            },
        );

        Self {
            lase_ring,
            target_ring,
            bearing_line,
            nine_line,
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
        msgs.set_markup_pos_start(self.target_ring, v3(new_target.x, new_target.y));
        msgs.set_markup_pos_end(self.bearing_line, v3(new_target.x, new_target.y));
        msgs.set_markup_pos_start(self.nine_line, v3(new_target.x + 500., new_target.y + 500.));
        msgs.set_markup_text(self.nine_line, new_nine_line.into());
    }

    /// Call when the JTAC itself moves (airborne JTAC / drone).
    pub fn on_jtac_move(&mut self, new_jtac: Vector2, msgs: &mut MsgQ) {
        if (new_jtac - self.last_jtac).norm() < 50. {
            return;
        }
        self.last_jtac = new_jtac;
        msgs.set_markup_pos_start(self.lase_ring, v3(new_jtac.x, new_jtac.y));
        msgs.set_markup_pos_start(self.bearing_line, v3(new_jtac.x, new_jtac.y));
    }

    pub fn remove(self, msgs: &mut MsgQ) {
        msgs.delete_mark(self.lase_ring);
        msgs.delete_mark(self.target_ring);
        msgs.delete_mark(self.bearing_line);
        msgs.delete_mark(self.nine_line);
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MapLayer — top-level owner of all marks
// ─────────────────────────────────────────────────────────────────────────────

/// Generic timed mark bundle — up to 3 MarkIds that expire together.
#[derive(Debug)]
struct TimedMark {
    ids: [Option<MarkId>; 3],
    expires: DateTime<Utc>,
}

impl TimedMark {
    #[allow(dead_code)]
    fn one(id: MarkId, ttl_secs: i64, now: DateTime<Utc>) -> Self {
        Self { ids: [Some(id), None, None], expires: now + Duration::seconds(ttl_secs) }
    }
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

#[derive(Debug, Default)]
pub struct MapLayer {
    convoy_marks: FxHashMap<ConvoyId, ConvoyMarks>,
    air_route_marks: FxHashMap<LogiRouteId, AirRouteMarks>,
    sea_route_marks: FxHashMap<LogiRouteId, SeaRouteMarks>,
    fire_marks: Vec<FireOverlay>,
    csar_marks: FxHashMap<GroupId, CsarMarks>,
    pub jtac_marks: FxHashMap<GroupId, JtacLayerMarks>,
    supply_critical_marks: FxHashMap<ObjectiveId, MarkId>,
    timed_marks: Vec<TimedMark>,
}

impl MapLayer {
    // ── Fire missions (explicit event) ──────────────────────────────────────

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

    // ── JTAC layer (explicit events from jtac.rs) ───────────────────────────

    /// Draw JTAC layer marks when a target is acquired.
    pub fn on_jtac_target(
        &mut self,
        gid: GroupId,
        jtac_pos: Vector2,
        target_pos: Vector2,
        lase_range_m: f64,
        side: Side,
        nine_line_text: impl Into<dcso3::String>,
        msgs: &mut MsgQ,
    ) {
        if let Some(old) = self.jtac_marks.remove(&gid) {
            old.remove(msgs);
        }
        let marks = JtacLayerMarks::new(
            jtac_pos, target_pos, lase_range_m, side, nine_line_text, msgs,
        );
        self.jtac_marks.insert(gid, marks);
    }

    /// Remove JTAC layer marks when target is cleared.
    pub fn on_jtac_cleared(&mut self, gid: &GroupId, msgs: &mut MsgQ) {
        if let Some(m) = self.jtac_marks.remove(gid) {
            m.remove(msgs);
        }
    }

    // ── Supply critical warnings ────────────────────────────────────────────

    /// Draw a persistent F10 map marker when an objective's supply is critical.
    pub fn on_supply_critical(
        &mut self,
        oid: ObjectiveId,
        pos: Vector2,
        side: Side,
        name: &str,
        threshold: u8,
        msgs: &mut MsgQ,
    ) {
        if self.supply_critical_marks.contains_key(&oid) {
            return;
        }
        let sf = side_filter(side);
        let col = side_color(side, 0.9);
        let mark = MarkId::new();
        msgs.text_to_all(
            sf,
            mark,
            TextSpec {
                pos: v3(pos.x, pos.y),
                color: col,
                fill_color: Color::black(0.55),
                font_size: 12,
                read_only: true,
                text: format_compact!("⚠ LOW SUPPLY\n{}\n< {}%", name, threshold).into(),
            },
        );
        self.supply_critical_marks.insert(oid, mark);
    }

    /// Remove the supply-critical marker when supply has recovered.
    pub fn on_supply_recovered(&mut self, oid: &ObjectiveId, msgs: &mut MsgQ) {
        if let Some(mark) = self.supply_critical_marks.remove(oid) {
            msgs.delete_mark(mark);
        }
    }

    // ── Transient tactical events ────────────────────────────────────────────

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
        let label = MarkId::new();
        msgs.text_to_all(
            sf,
            label,
            TextSpec {
                pos: v3(target_pos.x, target_pos.y),
                color: col,
                fill_color: Color::black(0.5),
                font_size: 11,
                read_only: true,
                text: format_compact!("RECON\n~{} enemy units", unit_count).into(),
            },
        );
        self.timed_marks.push(TimedMark::two(rect, label, 120, now));
    }

    /// NATO hostile unit symbol (diamond) at the detected enemy arty position.
    ///
    /// A diamond (◇) is the NATO APP-6 symbol for a hostile ground unit.
    /// Shown only to the friendly side so they can call counter-fire.
    pub fn on_counter_battery(
        &mut self,
        enemy_pos: Vector2,
        friendly_side: Side,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) {
        let sf = side_filter(friendly_side);
        let enemy_col = match friendly_side {
            Side::Blue => Color::red(0.95),
            _ => Color::blue(0.95),
        };
        let r = 1_200_f64;
        let diamond = MarkId::new();
        msgs.quad_to_all(
            sf,
            diamond,
            QuadSpec {
                p0: v3(enemy_pos.x,     enemy_pos.y + r),
                p1: v3(enemy_pos.x + r, enemy_pos.y),
                p2: v3(enemy_pos.x,     enemy_pos.y - r),
                p3: v3(enemy_pos.x - r, enemy_pos.y),
                color: enemy_col,
                fill_color: Color::new(1., 0., 0., 0.08),
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
                pos: v3(enemy_pos.x, enemy_pos.y),
                color: enemy_col,
                fill_color: Color::black(0.5),
                font_size: 11,
                read_only: true,
                text: "ARTY\nCOUNTER-BATTERY".into(),
            },
        );
        self.timed_marks.push(TimedMark::two(diamond, label, 60, now));
    }

    /// Enemy axis-of-advance arrow pointing at the objective — the standard
    /// military symbol for a ground threat approaching a position.
    pub fn on_objective_threatened(
        &mut self,
        obj_pos: Vector2,
        side: Side,
        obj_name: &str,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) {
        let sf = side_filter(side);
        let enemy_col = match side {
            Side::Blue => Color::red(0.95),
            _ => Color::blue(0.95),
        };
        // Arrow tip at the objective; tail 5 km out (direction of enemy approach
        // is unknown so we use a universal "converging" bearing from the NE).
        let tail = obj_pos + Vector2::new(3_500., 3_500.);
        let arrow = MarkId::new();
        msgs.arrow_to(
            sf,
            arrow,
            ArrowSpec {
                start: v3(tail.x, tail.y),
                end:   v3(obj_pos.x, obj_pos.y),
                color: enemy_col,
                fill_color: enemy_col,
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
                pos: v3(obj_pos.x, obj_pos.y),
                color: enemy_col,
                fill_color: Color::black(0.55),
                font_size: 12,
                read_only: true,
                text: format_compact!("ENEMY CONTACT\n{}", obj_name).into(),
            },
        );
        self.timed_marks.push(TimedMark::two(arrow, label, 120, now));
    }

    /// Bold enemy axis-of-advance arrow at an objective that is actively under
    /// attack — heavier weight and shorter range than the "threatened" arrow to
    /// show immediate close combat.
    pub fn on_objective_under_attack(
        &mut self,
        obj_pos: Vector2,
        side: Side,
        obj_name: &str,
        ttl_secs: i64,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) {
        let sf = side_filter(side);
        let enemy_col = match side {
            Side::Blue => Color::red(1.),
            _ => Color::blue(1.),
        };
        // Two converging attack arrows from NW and NE — standard hasty-attack
        // symbol showing multi-axis pressure on the position.
        let offset = 2_500_f64;
        let arrow_nw = MarkId::new();
        msgs.arrow_to(
            sf,
            arrow_nw,
            ArrowSpec {
                start: v3(obj_pos.x - offset, obj_pos.y + offset),
                end:   v3(obj_pos.x, obj_pos.y),
                color: enemy_col,
                fill_color: enemy_col,
                line_type: LineType::Solid,
                read_only: true,
            },
            None,
        );
        let arrow_ne = MarkId::new();
        msgs.arrow_to(
            sf,
            arrow_ne,
            ArrowSpec {
                start: v3(obj_pos.x + offset, obj_pos.y + offset),
                end:   v3(obj_pos.x, obj_pos.y),
                color: enemy_col,
                fill_color: enemy_col,
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
                pos: v3(obj_pos.x, obj_pos.y),
                color: enemy_col,
                fill_color: Color::black(0.6),
                font_size: 13,
                read_only: true,
                text: format_compact!("UNDER ATTACK\n{}", obj_name).into(),
            },
        );
        self.timed_marks.push(TimedMark::three(arrow_nw, arrow_ne, label, ttl_secs, now));
    }

    /// NATO friendly unit symbol (rectangle) at the objective + a movement
    /// arrow showing the axis of advance — standard symbol for friendly forces
    /// arriving at a position.
    pub fn on_reinforcements_arrived(
        &mut self,
        obj_pos: Vector2,
        side: Side,
        now: DateTime<Utc>,
        msgs: &mut MsgQ,
    ) {
        let sf = side_filter(side);
        let col = side_color(side, 0.9);
        // Movement arrow: axis of advance pointing into the objective from the north
        let arrow = MarkId::new();
        msgs.arrow_to(
            sf,
            arrow,
            ArrowSpec {
                start: v3(obj_pos.x, obj_pos.y + 3_500.),
                end:   v3(obj_pos.x, obj_pos.y),
                color: col,
                fill_color: col,
                line_type: LineType::Solid,
                read_only: true,
            },
            None,
        );
        // Friendly unit rectangle at the destination (NATO APP-6 friendly ground symbol)
        let h = 800_f64;
        let w = 1_400_f64;
        let unit_box = MarkId::new();
        msgs.rect_to_all(
            sf,
            unit_box,
            RectSpec {
                start: v3(obj_pos.x - w, obj_pos.y - h),
                end:   v3(obj_pos.x + w, obj_pos.y + h),
                color: col,
                fill_color: Color::new(0., 0.5, 1., 0.08),
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
                pos: v3(obj_pos.x, obj_pos.y),
                color: col,
                fill_color: Color::black(0.5),
                font_size: 12,
                read_only: true,
                text: format_compact!("REINFORCEMENTS\nARRIVED [{:?}]", side).into(),
            },
        );
        self.timed_marks.push(TimedMark::three(arrow, unit_box, label, 120, now));
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
    }

    // ── Full diff-based update (call from slow tick) ─────────────────────────

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
        self.expire_fire_marks(now, msgs);
        self.expire_timed_marks(now, msgs);
    }

    // ── Ground convoys ───────────────────────────────────────────────────────

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
                marks.on_move(convoy.last_pos, hdg, msgs);
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
                    "Convoy [{:?}]\n{:?} → {}",
                    convoy.side,
                    convoy.cargo_type,
                    persisted
                        .objectives
                        .get(&convoy.destination)
                        .map(|o| o.name.as_str())
                        .unwrap_or("unknown")
                );
                let marks = ConvoyMarks::new(
                    origin_pos,
                    dst_pos,
                    convoy.last_pos,
                    convoy.side,
                    cargo_str,
                    msgs,
                );
                self.convoy_marks.insert(id.clone(), marks);
            }
        }

        // Remove stale convoy marks
        self.convoy_marks.retain(|id, marks| {
            if !active_convoys.contains_key(id.as_str()) {
                msgs.delete_mark(marks.route);
                msgs.delete_mark(marks.arrow);
                msgs.delete_mark(marks.label);
                false
            } else {
                true
            }
        });
    }

    // ── Air logistics routes ─────────────────────────────────────────────────

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
                "Air Logi [{:?}]\n{:?} → {}",
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

    // ── Sea logistics routes ─────────────────────────────────────────────────

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
                "Sea Logi [{:?}]\n{:?} → {}",
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

    // ── CSAR / downed pilots ─────────────────────────────────────────────────

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

            if self.csar_marks.contains_key(gid) {
                let marks = self.csar_marks.get(gid).unwrap();
                marks.set_urgency(urgency, msgs);
                marks.update_label(label, msgs);
            } else {
                // Position: take the first unit from the group — O(1), no full scan needed
                let pos = group.units.into_iter()
                    .next()
                    .and_then(|uid| persisted.units.get(uid))
                    .map(|u| u.pos)
                    .unwrap_or_default();
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
                false
            } else {
                true
            }
        });
    }

    // ── Fire mark expiry ─────────────────────────────────────────────────────

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

    // ── Full removal (e.g. on mission reset) ────────────────────────────────

    pub fn remove_all(&mut self, msgs: &mut MsgQ) {
        for (_, c) in self.convoy_marks.drain() {
            msgs.delete_mark(c.route);
            msgs.delete_mark(c.arrow);
            msgs.delete_mark(c.label);
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
            msgs.delete_mark(j.lase_ring);
            msgs.delete_mark(j.target_ring);
            msgs.delete_mark(j.bearing_line);
            msgs.delete_mark(j.nine_line);
        }
        for (_, m) in self.supply_critical_marks.drain() {
            msgs.delete_mark(m);
        }
        for m in self.timed_marks.drain(..) {
            m.remove(msgs);
        }
    }
}
