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

use super::{
    objective::{Objective, Zone},
    persisted::Persisted,
};
use crate::msgq::MsgQ;
use bfprotocols::{
    cfg::Cfg,
    db::objective::{ObjectiveId, ObjectiveKind},
};
use compact_str::{CompactString, format_compact};
use dcso3::{
    Color, LuaVec3, Vector2, Vector3,
    coalition::Side,
    trigger::{ArrowSpec, CircleSpec, LineType, MarkId, PolylineSpec, QuadSpec, SideFilter, TextSpec},
};
use fxhash::FxHashMap;

// ─────────────────────────────────────────────────────────────────────────────
// Kind symbols — one connected polyline path per symbol, one MarkId each.
//
// Coordinate convention: north_offset = Vector3.x direction (up on F10 map),
// east_offset = Vector3.z direction (right on F10 map).
// ─────────────────────────────────────────────────────────────────────────────

fn pt(center: Vector2, dn: f64, de: f64) -> LuaVec3 {
    LuaVec3(Vector3::new(center.x + dn, 0., center.y + de))
}

/// Scale a slice of (north, east) normalized offsets by `r` and translate to `center`.
fn make_pts(center: Vector2, r: f64, offsets: &[(f64, f64)]) -> Vec<LuaVec3> {
    offsets.iter().map(|&(n, e)| pt(center, n * r, e * r)).collect()
}

fn draw_polyline(center: Vector2, r: f64, offsets: &[(f64, f64)], sf: SideFilter, color: Color, msgq: &mut MsgQ) -> MarkId {
    let id = MarkId::new();
    msgq.out_line_to_all(sf, id, PolylineSpec {
        points: make_pts(center, r, offsets),
        color,
        fill_color: Color::white(0.),
        line_type: LineType::Solid,
        read_only: true,
    }, None);
    id
}

/// Anchor icon for naval bases — derived from big-anchor-svgrepo-com.svg.
///
/// SVG viewBox: 0 0 372.479 372.479, centre (186.24, 186.24).
/// Normalisation:  east  = (svg_x − 186.24) / 186.24
///                 north = −(svg_y − 186.24) / 186.24  (SVG y-down → DCS north-up)
///
/// Structural measurements from SVG:
///   Eye ring   — centre svg(187.74, 51.998), outer r=26.655  → N=0.721, r=0.143
///   Stock      — y=151.993, half-span=63.205, ball-cap r=22.766
///                → N=0.184, E=±0.339, cap-r=0.122
///   Crown      — bottom of shank y≈306                       → N=−0.644
///   Left fluke — bezier endpoints svg(104,327),(80,299),(73,309)
///                → (N=−0.756,E=−0.441), (N=−0.608,E=−0.568), (N=−0.657,E=−0.607)
///   Bottom     — trace start svg(186.24,372.479)             → N=−1.000
///
/// Single connected trace:
///   eye ring → shank → left ball cap → centre → right ball cap →
///   centre → shank → crown → left fluke → bottom → right fluke → crown
fn anchor_path() -> &'static [(f64, f64)] {
    &[
        // ── Eye ring: 8-pt circle (N=0.721, E=0, r=0.143), start at bottom ──
        ( 0.578,  0.000), // S  — shank junction
        ( 0.619, -0.101), // SW
        ( 0.721, -0.143), // W
        ( 0.821, -0.101), // NW
        ( 0.863,  0.000), // N  — top of eye
        ( 0.821,  0.101), // NE
        ( 0.721,  0.143), // E
        ( 0.619,  0.101), // SE
        ( 0.578,  0.000), // close eye

        // ── Shank to stock level (y=151.993 → N=0.184) ──
        ( 0.184,  0.000),

        // ── Left ball cap (centre N=0.184, E=−0.339, r=0.122) ──
        // Enter from E (stock junction), trace CW, exit back to stock
        ( 0.184, -0.217), // E edge  (−0.339 + 0.122)
        ( 0.062, -0.339), // S       (0.184 − 0.122)
        ( 0.184, -0.461), // W edge  (−0.339 − 0.122)
        ( 0.306, -0.339), // N       (0.184 + 0.122)
        ( 0.184, -0.217), // back to E edge

        // ── Back to shank centre ──
        ( 0.184,  0.000),

        // ── Right ball cap (centre N=0.184, E=+0.339, r=0.122) ──
        ( 0.184,  0.217), // W edge
        ( 0.062,  0.339), // S
        ( 0.184,  0.461), // E edge
        ( 0.306,  0.339), // N
        ( 0.184,  0.217), // back to W edge

        // ── Back to shank centre, down to crown ──
        ( 0.184,  0.000),
        (-0.644,  0.000), // crown — bottom of shank (svg y≈306)

        // ── Left fluke (bezier-endpoint approximation from SVG path) ──
        // M(186,372) c(−82,−45)→ c(−24,−28)→ l(−7,+10)
        (-0.756, -0.441), // svg(104, 327) — outer fluke sweep
        (-0.608, -0.568), // svg(80,  299) — fluke arm
        (-0.657, -0.607), // svg(73,  309) — fluke tip corner

        // ── Bottom of anchor (SVG trace M-point, svg(186, 372)) ──
        (-1.000,  0.000),

        // ── Right fluke (symmetric) ──
        (-0.657,  0.607),
        (-0.608,  0.568),
        (-0.756,  0.441),

        // ── Return to crown ──
        (-0.644,  0.000),
    ]
}

/// NATO APP-6C hostile (Red side) SAM — diamond frame + missile + launcher arc.
///
/// Derived from SVG "Military_Symbol_-_Hostile_Unit_..._Air_Defence_-_Missiles".
/// SVG is 600×600, centre (300,300). Normalised to [-1,1] with:
///   east   = (svg_x - 300) / 300
///   north  = -(svg_y - 300) / 300   (SVG y is down, DCS north is up)
///
/// Hostile frame: diamond M 7,300 300,7 593,300 300,593 Z  →  ±0.977
/// Missile body: ICON group (translate(0,100) + inner matrix), bezier nose cap.
/// Launcher arc: AirDef cubic bezier across the bottom (5-point approximation).
///
/// Single connected trace:
///   diamond → center-spine → missile left side → launcher arc → missile right side → nose
fn hostile_sam_path() -> &'static [(f64, f64)] {
    &[
        // Diamond frame (CW from top)
        ( 0.977,  0.000), // top
        ( 0.000,  0.977), // right
        (-0.977,  0.000), // bottom
        ( 0.000, -0.977), // left
        ( 0.977,  0.000), // close — pen continues along centre spine
        // Centre spine → missile nose tip
        ( 0.342,  0.000), // nose (ICON group: SVG ~(300,197) after transforms)
        // Left side of missile body
        ( 0.155, -0.130), // left shoulder  (~261, 253 after transforms)
        (-0.341, -0.130), // left body base (~261, 402 after transforms)
        // Launcher arc — 5-point cubic-bezier approximation (AirDef path, no extra transform)
        // SVG: M 449,440  C 415,416 360,401 300,401  c -60,0 -115,15 -149,39
        (-0.468, -0.498), // arc right end  (SVG 449,440)
        (-0.373, -0.280), // arc right-mid  (t=0.5 of first bezier → SVG ~384,412)
        (-0.337,  0.000), // arc centre     (SVG 300,401)
        (-0.373,  0.280), // arc left-mid   (SVG ~216,412)
        (-0.468,  0.498), // arc left end   (SVG 151,440)
        // Right side of missile body back up
        (-0.334,  0.130), // right body base (~339, 400 after transforms)
        ( 0.155,  0.130), // right shoulder  (~339, 253 after transforms)
        ( 0.260,  0.065), // right nose curve (bezier approximation)
        ( 0.342,  0.000), // nose tip — close the missile outline
    ]
}

/// NATO APP-6C friendly (Blue side) SAM — rectangle frame + same missile icon.
///
/// Friendly ground unit frame is a rectangle spanning the same area as the diamond.
/// Frame corners chosen to match APP-6 proportions (height ≈ width × 0.8).
///
/// Trace: rectangle → top-centre → centre spine → missile + arc (identical to hostile).
fn friendly_sam_path() -> &'static [(f64, f64)] {
    &[
        // Rectangle frame (CW from top-left)
        ( 0.800, -0.650), // top-left
        ( 0.800,  0.650), // top-right
        (-0.800,  0.650), // bottom-right
        (-0.800, -0.650), // bottom-left
        ( 0.800, -0.650), // close
        ( 0.800,  0.000), // top-centre (on top edge — starts centre spine)
        // Centre spine → missile nose tip
        ( 0.342,  0.000),
        // Left side of missile body
        ( 0.155, -0.130),
        (-0.341, -0.130),
        // Launcher arc (same as hostile)
        (-0.468, -0.498),
        (-0.373, -0.280),
        (-0.337,  0.000),
        (-0.373,  0.280),
        (-0.468,  0.498),
        // Right side of missile body
        (-0.334,  0.130),
        ( 0.155,  0.130),
        ( 0.260,  0.065),
        ( 0.342,  0.000),
    ]
}

/// Carrier group icon — single traced hull silhouette.
///
/// Elongated N-S hull: pointed bow (N), flat stern (S), island stub on starboard (E).
fn carrier_path() -> &'static [(f64, f64)] {
    &[
        // Hull outline starting from stern-port (SW corner), going CW
        (-0.85, -0.22), // stern-port
        (-0.85,  0.22), // stern-starboard
        ( 0.55,  0.22), // mid-starboard (before island)
        // Island superstructure stub on starboard
        ( 0.55,  0.40),
        ( 0.20,  0.40),
        ( 0.20,  0.22),
        // Continue hull to bow
        ( 0.85,  0.22), // bow-starboard shoulder
        ( 1.00,  0.00), // bow tip
        ( 0.85, -0.22), // bow-port shoulder
        (-0.85, -0.22), // back to stern-port (close hull)
    ]
}

#[derive(Debug, Clone, Default)]
enum KindSymbol {
    #[default]
    None,
    Single(MarkId),
}

impl KindSymbol {
    fn remove(self, msgq: &mut MsgQ) {
        if let Self::Single(id) = self {
            msgq.delete_mark(id);
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(super) struct ObjectiveMarkup {
    side: Side,
    threatened: bool,
    health: u8,
    logi: u8,
    supply: u8,
    fuel: u8,
    points: i32,
    name: String,
    owner_ring: MarkId,
    capturable_ring: MarkId,
    threatened_ring: MarkId,
    label: MarkId,
    pos: Vector2,
    supply_connections: FxHashMap<ObjectiveId, MarkId>,
    kind_symbol: KindSymbol,
}

fn text_color(side: Side, a: f32) -> Color {
    match side {
        Side::Red => Color::red(a),
        Side::Blue => Color::blue(a),
        Side::Neutral => Color::white(a),
    }
}

fn objective_label(name: &str, obj: &Objective) -> CompactString {
    match obj.kind {
        ObjectiveKind::SpecialSamSite => format_compact!(
            "{}\nHealth: {}\nSupply: {}\nFuel: {}\nPoints: {}",
            name,
            obj.health,
            obj.supply,
            obj.fuel,
            obj.points
        ),
        _ => format_compact!(
            "{}\nHealth: {}\nLogi: {}\nSupply: {}\nFuel: {}\nPoints: {}",
            name,
            obj.health,
            obj.logi,
            obj.supply,
            obj.fuel,
            obj.points
        ),
    }
}

fn arrow_coords(obj: &Objective, dst: &Objective) -> (Vector2, Vector2) {
    let pos = obj.zone.pos();
    let dpos = dst.zone.pos();
    let dir = (dpos - pos).normalize();
    let spos = pos + dir * obj.zone.radius() * 1.1;
    let rdir = (pos - dpos).normalize();
    let dpos = dpos + rdir * dst.zone.radius() * 1.1;
    (spos, dpos)
}

impl ObjectiveMarkup {
    pub(super) fn remove(self, msgq: &mut MsgQ) {
        let ObjectiveMarkup {
            side: _,
            threatened: _,
            health: _,
            logi: _,
            supply: _,
            fuel: _,
            points: _,
            name: _,
            pos: _,
            owner_ring,
            capturable_ring,
            threatened_ring,
            supply_connections,
            label,
            kind_symbol,
        } = self;
        msgq.delete_mark(owner_ring);
        msgq.delete_mark(threatened_ring);
        msgq.delete_mark(capturable_ring);
        msgq.delete_mark(label);
        kind_symbol.remove(msgq);
        for (_, id) in supply_connections {
            msgq.delete_mark(id)
        }
    }

    pub(super) fn update(
        &mut self,
        persisted: &Persisted,
        msgq: &mut MsgQ,
        obj: &Objective,
        moved: &[ObjectiveId],
    ) {
        if obj.owner != self.side {
            let text_color = |a| text_color(obj.owner, a);
            self.side = obj.owner;
            msgq.set_markup_color(self.label, text_color(0.75));
            msgq.set_markup_color(self.owner_ring, text_color(1.));
            if obj.kind.is_special_sam_site() {
                let capturable_color = if obj.health == 0 { Color::white(0.75) } else { text_color(0.75) };
                msgq.set_markup_color(self.capturable_ring, capturable_color);
            }
            for (_, id) in self.supply_connections.drain() {
                msgq.delete_mark(id);
            }
        }
        if obj.threatened != self.threatened {
            self.threatened = obj.threatened;
            msgq.set_markup_color(
                self.threatened_ring,
                Color::yellow(if self.threatened { 0.75 } else { 0. }),
            );
        }
        if self.health != obj.health
            || self.logi != obj.logi
            || self.supply != obj.supply
            || self.fuel != obj.fuel
            || self.points != obj.points
        {
            if self.logi != obj.logi || (obj.kind.is_special_sam_site() && self.health != obj.health) {
                let text_color = |a| text_color(obj.owner, a);
                let capturable_color = if obj.kind.is_special_sam_site() {
                    if obj.health == 0 { Color::white(0.75) } else { text_color(0.75) }
                } else {
                    Color::white(if obj.captureable() { 0.75 } else { 0. })
                };
                msgq.set_markup_color(self.capturable_ring, capturable_color);
            }
            self.health = obj.health;
            self.logi = obj.logi;
            self.supply = obj.supply;
            self.fuel = obj.fuel;
            self.points = obj.points;
            msgq.set_markup_text(self.label, objective_label(&self.name, obj).into());
        }
        if let Zone::Circle { pos, .. } = obj.zone
            && self.pos != pos
        {
            self.pos = pos;
            let v3 = LuaVec3(Vector3::new(pos.x, 0., pos.y));
            msgq.set_markup_pos_start(self.owner_ring, v3);
            msgq.set_markup_pos_start(self.capturable_ring, v3);
            msgq.set_markup_pos_start(self.threatened_ring, v3);
            msgq.set_markup_pos_start(
                self.label,
                LuaVec3(Vector3::new(pos.x + 1500., 1., pos.y + 1500.)),
            );
        }
        for oid in moved {
            if obj.warehouse.destination.contains(oid)
                && let Some(id) = self.supply_connections.get(oid)
            {
                let dst = &persisted.objectives[oid];
                let (spos, dpos) = arrow_coords(obj, dst);
                msgq.set_markup_pos_start(*id, LuaVec3(Vector3::new(dpos.x, 0., dpos.y)));
                msgq.set_markup_pos_end(*id, LuaVec3(Vector3::new(spos.x, 0., spos.y)));
            }
        }
    }

    pub(super) fn new(cfg: &Cfg, msgq: &mut MsgQ, obj: &Objective, persisted: &Persisted) -> Self {
        let text_color = |a| text_color(obj.owner, a);
        let all_spec = match obj.kind {
            ObjectiveKind::Airbase | ObjectiveKind::Fob | ObjectiveKind::Logistics | ObjectiveKind::NavalBase | ObjectiveKind::Factory { .. } | ObjectiveKind::CommandCenter => {
                SideFilter::All
            }
            ObjectiveKind::Farp { .. } | ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::SpecialSamSite { .. } => obj.owner.into(),
        };
        let mut t = ObjectiveMarkup::default();
        t.side = obj.owner;
        t.threatened = obj.threatened;
        t.health = obj.health;
        t.logi = obj.logi;
        t.supply = obj.supply;
        t.fuel = obj.fuel;
        t.name = match obj.kind {
            ObjectiveKind::SpecialSamSite => format_compact!("{}", obj.name).into(),
            _ => format_compact!("{} {}", obj.name, obj.kind.name()).into(),
        };
        t.pos = obj.zone.pos();
        let pos3 = Vector3::new(t.pos.x, 0., t.pos.y);
        macro_rules! threat_circle {
            ($radius:expr) => {
                msgq.circle_to_all(
                    all_spec,
                    t.threatened_ring,
                    CircleSpec {
                        center: LuaVec3(pos3),
                        radius: (cfg.logistics_exclusion as f64).max($radius * 1.1),
                        color: Color::yellow(if obj.threatened { 0.75 } else { 0. }),
                        fill_color: Color::white(0.),
                        line_type: LineType::Solid,
                        read_only: true,
                    },
                    None,
                )
            };
        }
        match obj.zone {
            Zone::Circle { radius, .. } => {
                msgq.circle_to_all(
                    all_spec,
                    t.owner_ring,
                    CircleSpec {
                        center: LuaVec3(pos3),
                        radius,
                        color: text_color(1.),
                        fill_color: Color::white(0.),
                        line_type: LineType::Dashed,
                        read_only: true,
                    },
                    None,
                );
                threat_circle!(radius);
            }
            Zone::Quad { points, pos } => {
                msgq.quad_to_all(
                    all_spec,
                    t.owner_ring,
                    QuadSpec {
                        p0: LuaVec3(Vector3::new(points.p0.x, 0., points.p0.y)),
                        p1: LuaVec3(Vector3::new(points.p1.x, 0., points.p1.y)),
                        p2: LuaVec3(Vector3::new(points.p2.x, 0., points.p2.y)),
                        p3: LuaVec3(Vector3::new(points.p3.x, 0., points.p3.y)),
                        color: text_color(1.),
                        fill_color: Color::white(0.),
                        line_type: LineType::Dashed,
                        read_only: true,
                    },
                    None,
                );
                if !points.contains_circle(pos, cfg.logistics_exclusion as f64) {
                    threat_circle!(0.);
                } else {
                    let points = points.scale(1.1);
                    msgq.quad_to_all(
                        all_spec,
                        t.threatened_ring,
                        QuadSpec {
                            p0: LuaVec3(Vector3::new(points.p0.x, 0., points.p0.y)),
                            p1: LuaVec3(Vector3::new(points.p1.x, 0., points.p1.y)),
                            p2: LuaVec3(Vector3::new(points.p2.x, 0., points.p2.y)),
                            p3: LuaVec3(Vector3::new(points.p3.x, 0., points.p3.y)),
                            color: Color::yellow(if obj.threatened { 0.75 } else { 0. }),
                            fill_color: Color::white(0.),
                            line_type: LineType::Solid,
                            read_only: true,
                        },
                        None,
                    );
                }
            }
        }
        // For SpecialSamSite, the capturable ring shows owner color when healthy,
        // white when destroyed (health=0), since logi is always 0 and captureable() is always true.
        let capturable_color = if obj.kind.is_special_sam_site() {
            if obj.health == 0 { Color::white(0.75) } else { text_color(0.75) }
        } else {
            Color::white(if obj.captureable() { 0.75 } else { 0. })
        };
        match obj.zone {
            Zone::Circle { pos: _, radius } => {
                msgq.circle_to_all(
                    all_spec,
                    t.capturable_ring,
                    CircleSpec {
                        center: LuaVec3(pos3),
                        radius: radius as f64 * 0.9,
                        color: capturable_color,
                        fill_color: Color::white(0.),
                        line_type: LineType::Solid,
                        read_only: true,
                    },
                    None,
                );
            }
            Zone::Quad { pos: _, points } => {
                let points = points.scale(0.9);
                msgq.quad_to_all(
                    all_spec,
                    t.capturable_ring,
                    QuadSpec {
                        p0: LuaVec3(Vector3::new(points.p0.x, 0., points.p0.y)),
                        p1: LuaVec3(Vector3::new(points.p1.x, 0., points.p1.y)),
                        p2: LuaVec3(Vector3::new(points.p2.x, 0., points.p2.y)),
                        p3: LuaVec3(Vector3::new(points.p3.x, 0., points.p3.y)),
                        color: capturable_color,
                        fill_color: Color::white(0.),
                        line_type: LineType::Solid,
                        read_only: true,
                    },
                    None,
                );
            }
        }
        msgq.text_to_all(
            all_spec,
            t.label,
            TextSpec {
                pos: LuaVec3(Vector3::new(pos3.x + 1500., 1., pos3.z + 1500.)),
                color: text_color(1.),
                fill_color: Color::black(0.),
                font_size: 10,
                read_only: true,
                text: objective_label(&t.name, obj).into(),
            },
        );
        // Draw kind-specific icon symbol inside the objective zone
        let sym_r = obj.zone.radius().max(500.) * 0.45;
        let sym_color = text_color(0.85);
        t.kind_symbol = match obj.kind {
            ObjectiveKind::SpecialSamSite => {
                let path = if obj.owner == Side::Blue { friendly_sam_path() } else { hostile_sam_path() };
                KindSymbol::Single(draw_polyline(t.pos, sym_r, path, all_spec, sym_color, msgq))
            }
            ObjectiveKind::NavalBase => KindSymbol::Single(
                draw_polyline(t.pos, sym_r, anchor_path(), all_spec, sym_color, msgq)
            ),
            ObjectiveKind::CarrierGroup { .. } => KindSymbol::Single(
                draw_polyline(t.pos, sym_r, carrier_path(), all_spec, sym_color, msgq)
            ),
            _ => KindSymbol::None,
        };

        match obj.kind {
            ObjectiveKind::Airbase | ObjectiveKind::Farp { .. } | ObjectiveKind::Fob | ObjectiveKind::Factory { .. } | ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::SpecialSamSite { .. } | ObjectiveKind::CommandCenter => (),
            ObjectiveKind::Logistics => {
                for oid in &obj.warehouse.destination {
                    let id = MarkId::new();
                    let dobj = &persisted.objectives[oid];
                    let (spos, dpos) = arrow_coords(obj, dobj);
                    msgq.arrow_to(
                        if dobj.is_farp() {
                            dobj.owner.into()
                        } else {
                            all_spec
                        },
                        id,
                        ArrowSpec {
                            start: LuaVec3(Vector3::new(dpos.x, 0., dpos.y)),
                            end: LuaVec3(Vector3::new(spos.x, 0., spos.y)),
                            color: Color::gray(0.5),
                            fill_color: Color::gray(0.5),
                            line_type: LineType::NoLine,
                            read_only: true,
                        },
                        None,
                    );
                    t.supply_connections.insert(*oid, id);
                }
            }
            ObjectiveKind::NavalBase => {
                for oid in &obj.warehouse.destination {
                    let dst_obj = &persisted.objectives[oid];
                    if let ObjectiveKind::CarrierGroup { .. } = dst_obj.kind {
                        let id = MarkId::new();
                        let (spos, dpos) = arrow_coords(obj, dst_obj);
                        msgq.arrow_to(
                            dst_obj.owner.into(),
                            id,
                            ArrowSpec {
                                start: LuaVec3(Vector3::new(dpos.x, 0., dpos.y)),
                                end: LuaVec3(Vector3::new(spos.x, 0., spos.y)),
                                color: Color::gray(0.5),
                                fill_color: Color::gray(0.5),
                                line_type: LineType::NoLine,
                                read_only: true,
                            },
                            None,
                        );
                        t.supply_connections.insert(*oid, id);
                    }
                }
            }
        }
        t
    }
}
