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

//! F10-map frontline overlay. The geometry lives in
//! `bfprotocols::frontline` so bfdb can serve the identical line to the web
//! dashboard; this module just gathers the objectives, calls it, and draws
//! the three lines (blue-edge / centre / red-edge) via the message queue.

use crate::{db::persisted::Persisted, msgq::MsgQ};
use bfprotocols::{cfg::FrontLineConfig, db::objective::ObjectiveKind, frontline as fl};
use chrono::{DateTime, Utc};
use dcso3::{
    coalition::Side,
    trigger::{LineSpec, LineType, MarkId, SideFilter},
    Color, LuaVec3, Vector3,
};
use log::*;

const LINE_ALPHA: f32 = 0.9;
/// Centre line ("no man's land") style.
const MID_LINE: LineType = LineType::Dotted;
/// Blue-edge and red-edge line style.
const EDGE_LINE: LineType = LineType::Dashed;

/// Most line segments one frontline redraw may put on the F10 map.
///
/// Every segment is its own `lineToAll`, and a redraw first deletes the
/// previous set, so a redraw costs `2 * segments` entries in the message
/// queue -- which drains at `max_msgs_per_second` (3 by default) for the
/// whole server. The raw marching-squares contour off a 100x100 grid is
/// ~850 segments, so one objective flipping owner used to enqueue ~1700
/// commands and put the F10 map ~11 minutes behind the campaign. Simplify
/// the contour until it fits this budget instead: at F10 zoom the dropped
/// detail is well under a pixel.
const MAX_SEGMENTS: usize = 250;

/// Starting simplification tolerance in metres, doubled until the contour
/// fits `MAX_SEGMENTS`. The frontline is drawn over hundreds of kilometres,
/// so a kilometre of deviation is not visible.
const SIMPLIFY_TOLERANCE_M: f64 = 1000.0;
/// Give up doubling rather than loop forever on a pathological contour.
const MAX_SIMPLIFY_PASSES: u32 = 12;

/// Perpendicular distance from `p` to the segment `a`-`b`.
fn point_segment_distance(p: [f64; 2], a: [f64; 2], b: [f64; 2]) -> f64 {
    let (dx, dy) = (b[0] - a[0], b[1] - a[1]);
    let len2 = dx * dx + dy * dy;
    if len2 <= f64::EPSILON {
        return ((p[0] - a[0]).powi(2) + (p[1] - a[1]).powi(2)).sqrt();
    }
    // Projection parameter, clamped so a point beyond an end of the segment
    // measures to that end rather than to the infinite line.
    let t = (((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / len2).clamp(0.0, 1.0);
    let (px, py) = (a[0] + t * dx, a[1] + t * dy);
    ((p[0] - px).powi(2) + (p[1] - py).powi(2)).sqrt()
}

/// Ramer-Douglas-Peucker, iterative so a long contour can't blow the stack.
/// Keeps every vertex further than `tol` metres from the chord it would be
/// dropped onto, so corners survive and near-collinear runs collapse.
fn simplify(line: &[[f64; 2]], tol: f64) -> Vec<[f64; 2]> {
    if line.len() < 3 {
        return line.to_vec();
    }
    let mut keep = vec![false; line.len()];
    keep[0] = true;
    keep[line.len() - 1] = true;
    let mut stack = vec![(0usize, line.len() - 1)];
    while let Some((first, last)) = stack.pop() {
        if last <= first + 1 {
            continue;
        }
        let mut worst = 0.0;
        let mut worst_ix = first;
        for ix in (first + 1)..last {
            let d = point_segment_distance(line[ix], line[first], line[last]);
            if d > worst {
                worst = d;
                worst_ix = ix;
            }
        }
        if worst > tol {
            keep[worst_ix] = true;
            stack.push((first, worst_ix));
            stack.push((worst_ix, last));
        }
    }
    line.iter()
        .zip(keep)
        .filter_map(|(p, k)| if k { Some(*p) } else { None })
        .collect()
}

/// Number of segments a set of polylines would draw.
fn segment_count(lines: &[Vec<[f64; 2]>]) -> usize {
    lines.iter().map(|l| l.len().saturating_sub(1)).sum()
}

/// Objectives that count toward the front: owned, on the ground. Carrier
/// groups move and sit at sea, so they're excluded.
pub fn frontline_objectives(persisted: &Persisted) -> Vec<(f64, f64, f64)> {
    persisted
        .objectives
        .into_iter()
        .filter(|(_, o)| matches!(o.owner, Side::Blue | Side::Red))
        .filter(|(_, o)| !matches!(o.kind(), ObjectiveKind::CarrierGroup { .. }))
        .map(|(_, o)| {
            let p = o.pos();
            (p.x, p.y, if o.owner == Side::Blue { 1.0 } else { -1.0 })
        })
        .collect()
}

/// Stores frontline drawing state.
#[derive(Debug, Clone)]
pub struct FrontLine {
    marks: Vec<MarkId>,
    config: FrontLineConfig,
    objective_ownership_hash: u64,
}

impl FrontLine {
    pub fn new(config: FrontLineConfig) -> Self {
        Self {
            marks: Vec::new(),
            config,
            objective_ownership_hash: 0,
        }
    }

    fn clear_marks(&mut self, msgq: &mut MsgQ) {
        for mark_id in self.marks.drain(..) {
            msgq.delete_mark(mark_id);
        }
    }

    fn calculate_ownership_hash(persisted: &Persisted) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        let mut objectives: Vec<_> = persisted.objectives.into_iter().collect();
        objectives.sort_by_key(|(id, _)| *id);
        for (id, obj) in objectives {
            id.hash(&mut hasher);
            (obj.owner as u8).hash(&mut hasher);
        }
        hasher.finish()
    }

    fn params(&self) -> fl::Params {
        fl::Params {
            grid_res: self.config.samples_per_boundary,
            ..fl::Params::default()
        }
    }

    fn draw_frontline(&mut self, persisted: &Persisted, msgq: &mut MsgQ) {
        let objs = frontline_objectives(persisted);
        let mut fl = fl::compute(&objs, &self.params());
        if fl.mid.is_empty() && fl.blue.is_empty() && fl.red.is_empty() {
            return;
        }
        // Thin the contour before it reaches the message queue -- see
        // MAX_SEGMENTS. bfdb computes its own line from the same function for
        // the web map, so this only affects what the F10 map draws.
        let raw = segment_count(&fl.mid) + segment_count(&fl.blue) + segment_count(&fl.red);
        if raw > MAX_SEGMENTS {
            let mut tol = SIMPLIFY_TOLERANCE_M;
            for _ in 0..MAX_SIMPLIFY_PASSES {
                let mid: Vec<_> = fl.mid.iter().map(|l| simplify(l, tol)).collect();
                let blue: Vec<_> = fl.blue.iter().map(|l| simplify(l, tol)).collect();
                let red: Vec<_> = fl.red.iter().map(|l| simplify(l, tol)).collect();
                let n = segment_count(&mid) + segment_count(&blue) + segment_count(&red);
                fl.mid = mid;
                fl.blue = blue;
                fl.red = red;
                if n <= MAX_SEGMENTS {
                    break;
                }
                // Still over budget: coarsen and run again on the already
                // thinned lines, which converges much faster than restarting
                // from the raw contour.
                tol *= 2.0;
            }
            info!(
                "Frontline: simplified {} raw segment(s) to {} (tolerance up to {:.0}m)",
                raw,
                segment_count(&fl.mid) + segment_count(&fl.blue) + segment_count(&fl.red),
                tol
            );
        }

        let draw = |line: &[[f64; 2]], color: Color, lt: LineType, marks: &mut Vec<MarkId>, msgq: &mut MsgQ| {
            for w in line.windows(2) {
                let (a, b) = (w[0], w[1]);
                let mark_id = MarkId::new();
                msgq.line_to_all(
                    SideFilter::All,
                    mark_id,
                    LineSpec {
                        start: LuaVec3(Vector3::new(a[0], 0., a[1])),
                        end: LuaVec3(Vector3::new(b[0], 0., b[1])),
                        color,
                        line_type: lt,
                        read_only: true,
                    },
                    None,
                );
                marks.push(mark_id);
            }
        };

        for l in &fl.blue {
            draw(l, Color::new(0.0, 0.4, 1.0, LINE_ALPHA), EDGE_LINE, &mut self.marks, msgq);
        }
        for l in &fl.red {
            draw(l, Color::new(1.0, 0.2, 0.2, LINE_ALPHA), EDGE_LINE, &mut self.marks, msgq);
        }
        for l in &fl.mid {
            draw(l, Color::new(1.0, 1.0, 1.0, LINE_ALPHA), MID_LINE, &mut self.marks, msgq);
        }
    }

    /// Redraw the frontline from current objective ownership.
    /// Returns true if it was redrawn.
    pub fn update(&mut self, persisted: &Persisted, msgq: &mut MsgQ, _now: DateTime<Utc>) -> bool {
        if !self.config.enabled {
            if !self.marks.is_empty() {
                info!("Frontline: disabled, clearing {} marks", self.marks.len());
                self.clear_marks(msgq);
                self.objective_ownership_hash = 0;
            }
            return false;
        }

        let new_hash = Self::calculate_ownership_hash(persisted);
        let is_initial = self.objective_ownership_hash == 0;
        if self.config.update_on_objective_change_only && !is_initial && new_hash == self.objective_ownership_hash {
            debug!("Frontline: no ownership change, skipping redraw");
            return false;
        }
        self.objective_ownership_hash = new_hash;

        self.clear_marks(msgq);
        self.draw_frontline(persisted, msgq);

        if self.marks.len() > 5000 {
            warn!("Frontline: excessive segment count {} — bug?", self.marks.len());
        } else {
            info!("Frontline: drew {} segment(s)", self.marks.len());
        }
        true
    }

    /// Dummy method for compatibility (no longer used)
    pub fn collect_unit_pressure(&mut self, _persisted: &Persisted, _now: DateTime<Utc>) {}

    /// Remove all frontline segments
    pub fn remove(mut self, msgq: &mut MsgQ) {
        self.clear_marks(msgq);
    }
}
