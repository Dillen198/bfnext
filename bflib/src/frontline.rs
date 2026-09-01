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

//! Dynamic frontline overlay.
//!
//! The front is the contour that separates blue-held ground from red-held
//! ground. It is computed as the iso-line of the objective ownership field:
//!
//! 1. Take every territory-defining objective (airbase, naval base, logistics
//!    hub, FOB, FARP). SAM sites, command centres, factories and carriers are
//!    left out — they sit inside friendly territory and are not a front.
//! 2. Delaunay-triangulate the objective positions. That is the natural
//!    "who borders whom" planar graph, with no arbitrary distance or k.
//! 3. March the triangles: any triangle with both a blue and a red corner has
//!    the front passing through it. It crosses the two edges that join a blue
//!    corner to a red corner, at a point pushed toward whichever side is
//!    weaker (lower objective health). Join those two crossings — one segment.
//! 4. Segments from neighbouring triangles share a crossing point exactly, so
//!    they chain into connected polylines. A map with an island and two land
//!    borders yields several separate lines, each its own front.
//! 5. Drop tiny fragments, simplify (Ramer–Douglas–Peucker), and draw each
//!    line as a chain of dashed segments coloured by which side is winning
//!    along it (white + dotted where the two are even).

use crate::{db::persisted::Persisted, msgq::MsgQ};
use bfprotocols::cfg::FrontLineConfig;
use chrono::{DateTime, Utc};
use dcso3::{
    coalition::Side,
    trigger::{LineSpec, LineType, MarkId, SideFilter},
    Color, LuaVec3, Vector2, Vector3,
};
use delaunator::{triangulate, Point};
use fxhash::{FxHashMap, FxHashSet};
use log::*;

/// Perpendicular distance from `p` to the segment `a`–`b` (or to `a` when the
/// segment is degenerate).
fn perp_dist(p: Vector2, a: Vector2, b: Vector2) -> f64 {
    let ab = b - a;
    let len = ab.norm();
    if len < 1e-6 {
        return (p - a).norm();
    }
    ((ab.x * (p.y - a.y) - ab.y * (p.x - a.x)) / len).abs()
}

/// Ramer–Douglas–Peucker polyline simplification. Appends the result to `out`.
fn rdp(points: &[Vector2], epsilon: f64, out: &mut Vec<Vector2>) {
    if points.len() < 3 {
        out.extend_from_slice(points);
        return;
    }
    let (a, b) = (points[0], points[points.len() - 1]);
    let mut idx = 0;
    let mut dmax = 0.0;
    for (k, p) in points.iter().enumerate().take(points.len() - 1).skip(1) {
        let d = perp_dist(*p, a, b);
        if d > dmax {
            dmax = d;
            idx = k;
        }
    }
    if dmax > epsilon {
        let mut left = Vec::new();
        rdp(&points[..=idx], epsilon, &mut left);
        left.pop();
        out.extend(left);
        rdp(&points[idx..], epsilon, out);
    } else {
        out.push(a);
        out.push(b);
    }
}

/// Undirected key for the objective pair `(a, b)`.
fn ekey(a: usize, b: usize) -> (usize, usize) {
    if a < b {
        (a, b)
    } else {
        (b, a)
    }
}

// Line styling is deliberately kept out of FrontLineConfig: `Cfg` is
// snapshotted positionally (bincode) into bfdb's session tree, so adding
// fields to it breaks decoding of older snapshots.
const LINE_ALPHA: f32 = 0.9;
/// Line style for a stretch of front one side is winning.
const HELD_LINE: LineType = LineType::Dashed;
/// Line style for an even stretch.
const CONTESTED_LINE: LineType = LineType::Dotted;
/// Summed objective-health difference (percentage points) between a triangle's
/// blue and red corners below which that stretch is drawn as contested.
const HEALTH_CONTESTED_DELTA: f64 = 25.0;
/// A frontline shorter than this (metres) is noise around an isolated pocket.
const MIN_FRONT_LEN: f64 = 25_000.0;
/// Skip triangles whose longest edge exceeds this multiple of the median
/// Delaunay edge — trans-water and convex-hull sliver triangles.
const EDGE_CUTOFF_MULT: f64 = 2.5;

#[derive(Debug, Clone, Copy)]
struct Obj {
    pos: Vector2,
    side: Side,
    health: f64,
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

    /// Hash of objective ownership, for skipping redraws when nothing changed.
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

    /// Compute the frontline(s): each is an ordered, simplified polyline plus
    /// the per-triangle crossing points (with the winning side) that colour
    /// its segments. Empty when there is no blue/red contact.
    fn compute_frontlines(&self, persisted: &Persisted) -> Vec<(Vec<Vector2>, Vec<(Vector2, Side)>)> {
        use bfprotocols::db::objective::ObjectiveKind as K;

        let objs: Vec<Obj> = persisted
            .objectives
            .into_iter()
            .filter(|(_, o)| matches!(o.owner, Side::Blue | Side::Red))
            .filter(|(_, o)| {
                matches!(
                    o.kind(),
                    K::Airbase | K::NavalBase | K::Logistics | K::Fob | K::Farp { .. }
                )
            })
            .map(|(_, o)| Obj {
                pos: o.pos(),
                side: o.owner,
                health: (o.health() as f64).max(1.0),
            })
            .collect();
        if objs.len() < 3 {
            return Vec::new();
        }
        let blue = objs.iter().filter(|o| o.side == Side::Blue).count();
        let red = objs.iter().filter(|o| o.side == Side::Red).count();
        if blue == 0 || red == 0 {
            info!("Frontline: {} blue / {} red territory objectives — no front", blue, red);
            return Vec::new();
        }

        let pts: Vec<Point> = objs.iter().map(|o| Point { x: o.pos.x, y: o.pos.y }).collect();
        let tri = triangulate(&pts);
        if tri.triangles.len() < 3 {
            return Vec::new();
        }

        // Median Delaunay edge length → cutoff for spurious long triangles.
        let mut elens: Vec<f64> = Vec::with_capacity(tri.triangles.len());
        for t in tri.triangles.chunks_exact(3) {
            for &(a, b) in &[(t[0], t[1]), (t[1], t[2]), (t[2], t[0])] {
                elens.push((objs[a].pos - objs[b].pos).norm());
            }
        }
        elens.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let median_edge = elens[elens.len() / 2].max(1.0);
        let edge_cutoff = median_edge * EDGE_CUTOFF_MULT;

        // March the triangles.
        let mut cross: FxHashMap<(usize, usize), Vector2> = FxHashMap::default();
        let mut segs: Vec<((usize, usize), (usize, usize), Side)> = Vec::new();
        for t in tri.triangles.chunks_exact(3) {
            let corners = [t[0], t[1], t[2]];
            let has_blue = corners.iter().any(|&c| objs[c].side == Side::Blue);
            let has_red = corners.iter().any(|&c| objs[c].side == Side::Red);
            if !(has_blue && has_red) {
                continue;
            }
            let edges = [
                (corners[0], corners[1]),
                (corners[1], corners[2]),
                (corners[2], corners[0]),
            ];
            if edges
                .iter()
                .any(|&(a, b)| (objs[a].pos - objs[b].pos).norm() > edge_cutoff)
            {
                continue;
            }
            let mut xs: Vec<(usize, usize)> = Vec::with_capacity(2);
            for &(a, b) in &edges {
                if objs[a].side == objs[b].side {
                    continue;
                }
                let k = ekey(a, b);
                cross.entry(k).or_insert_with(|| {
                    let (bc, rc) = if objs[a].side == Side::Blue { (a, b) } else { (b, a) };
                    // push the crossing toward the weaker side
                    let f = (objs[bc].health / (objs[bc].health + objs[rc].health)).clamp(0.25, 0.75);
                    objs[bc].pos + (objs[rc].pos - objs[bc].pos) * f
                });
                xs.push(k);
            }
            if xs.len() == 2 {
                let (mut bh, mut rh) = (0.0, 0.0);
                for &c in &corners {
                    match objs[c].side {
                        Side::Blue => bh += objs[c].health,
                        Side::Red => rh += objs[c].health,
                        Side::Neutral => {}
                    }
                }
                let adv = if (bh - rh).abs() < HEALTH_CONTESTED_DELTA {
                    Side::Neutral
                } else if bh > rh {
                    Side::Blue
                } else {
                    Side::Red
                };
                segs.push((xs[0], xs[1], adv));
            }
        }
        if segs.is_empty() {
            return Vec::new();
        }

        // Index crossing points densely and build the segment graph.
        let mut node_of: FxHashMap<(usize, usize), usize> = FxHashMap::default();
        let mut positions: Vec<Vector2> = Vec::with_capacity(cross.len());
        for (k, v) in &cross {
            node_of.insert(*k, positions.len());
            positions.push(*v);
        }
        let n = positions.len();
        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
        let mut adv_nodes: Vec<(Vector2, Side)> = Vec::with_capacity(segs.len());
        for (a, b, adv) in &segs {
            let (ia, ib) = (node_of[a], node_of[b]);
            adj[ia].push(ib);
            adj[ib].push(ia);
            adv_nodes.push(((positions[ia] + positions[ib]) * 0.5, *adv));
        }

        // Walk the graph into chains: start at endpoints / junctions, then
        // mop up loops.
        let sidx = |a: usize, b: usize| if a < b { (a, b) } else { (b, a) };
        let mut used: FxHashSet<(usize, usize)> = FxHashSet::default();
        let walk = |start: usize, via: usize, used: &mut FxHashSet<(usize, usize)>| {
            let mut chain = vec![positions[start]];
            let (mut prev, mut cur) = (start, via);
            loop {
                used.insert(sidx(prev, cur));
                chain.push(positions[cur]);
                if adj[cur].len() != 2 {
                    break;
                }
                match adj[cur]
                    .iter()
                    .copied()
                    .find(|&m| m != prev && !used.contains(&sidx(cur, m)))
                {
                    Some(m) => {
                        prev = cur;
                        cur = m;
                    }
                    None => break,
                }
            }
            chain
        };

        let mut raw: Vec<Vec<Vector2>> = Vec::new();
        for v in 0..n {
            if adj[v].len() == 2 {
                continue;
            }
            for k in 0..adj[v].len() {
                let m = adj[v][k];
                if !used.contains(&sidx(v, m)) {
                    raw.push(walk(v, m, &mut used));
                }
            }
        }
        for v in 0..n {
            for k in 0..adj[v].len() {
                let m = adj[v][k];
                if !used.contains(&sidx(v, m)) {
                    raw.push(walk(v, m, &mut used));
                }
            }
        }

        // Filter tiny fragments, simplify, keep.
        let budget = self.config.max_marks.max(8);
        let epsilon = (median_edge * 0.2).clamp(2_000.0, 8_000.0);
        let mut lines: Vec<(Vec<Vector2>, Vec<(Vector2, Side)>)> = Vec::new();
        for chain in raw {
            if chain.len() < 2 {
                continue;
            }
            let len: f64 = chain.windows(2).map(|w| (w[1] - w[0]).norm()).sum();
            if len < MIN_FRONT_LEN {
                continue;
            }
            let mut eps = epsilon;
            let mut simp = Vec::new();
            rdp(&chain, eps, &mut simp);
            simp.dedup_by(|a, b| (*a - *b).norm() < 1.0);
            while simp.len().saturating_sub(1) > budget && eps < len {
                eps *= 2.0;
                simp.clear();
                rdp(&chain, eps, &mut simp);
                simp.dedup_by(|a, b| (*a - *b).norm() < 1.0);
            }
            if simp.len() >= 2 {
                lines.push((simp, adv_nodes.clone()));
            }
        }
        lines.sort_by(|a, b| {
            a.0[0]
                .x
                .partial_cmp(&b.0[0].x)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        lines
    }

    fn draw_frontline(&mut self, persisted: &Persisted, msgq: &mut MsgQ) {
        let lines = self.compute_frontlines(persisted);
        if lines.is_empty() {
            return;
        }
        let total: usize = lines.iter().map(|(l, _)| l.len().saturating_sub(1)).sum();
        info!("Frontline: drawing {} line(s), {} segment(s)", lines.len(), total);

        for (line, adv_nodes) in &lines {
            for w in line.windows(2) {
                let (a, b) = (w[0], w[1]);
                let mid = (a + b) * 0.5;
                let side = adv_nodes
                    .iter()
                    .min_by(|x, y| {
                        (x.0 - mid)
                            .norm()
                            .partial_cmp(&(y.0 - mid).norm())
                            .unwrap_or(std::cmp::Ordering::Equal)
                    })
                    .map(|(_, s)| *s)
                    .unwrap_or(Side::Neutral);
                let (color, line_type) = match side {
                    Side::Red => (Color::new(1.0, 0.0, 0.0, LINE_ALPHA), HELD_LINE),
                    Side::Blue => (Color::new(0.0, 0.0, 1.0, LINE_ALPHA), HELD_LINE),
                    Side::Neutral => (Color::new(1.0, 1.0, 1.0, LINE_ALPHA), CONTESTED_LINE),
                };
                let mark_id = MarkId::new();
                msgq.line_to_all(
                    SideFilter::All,
                    mark_id,
                    LineSpec {
                        start: LuaVec3(Vector3::new(a.x, 0., a.y)),
                        end: LuaVec3(Vector3::new(b.x, 0., b.y)),
                        color,
                        line_type,
                        read_only: true,
                    },
                    None,
                );
                self.marks.push(mark_id);
            }
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
