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

use crate::{db::persisted::Persisted, msgq::MsgQ};
use bfprotocols::cfg::FrontLineConfig;
use chrono::{DateTime, Utc};
use dcso3::{
    coalition::Side,
    trigger::{LineSpec, LineType, MarkId, SideFilter},
    Color, LuaVec3, Vector2, Vector3,
};
use log::*;
use rstar::{RTree, RTreeObject, AABB, PointDistance};

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

/// Ramer–Douglas–Peucker polyline simplification. Appends the simplified point
/// list to `out`.
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
        left.pop(); // drop the shared vertex so it isn't duplicated
        out.extend(left);
        rdp(&points[idx..], epsilon, out);
    } else {
        out.push(a);
        out.push(b);
    }
}

/// Point wrapper for R-tree spatial indexing of objectives
#[derive(Debug, Clone, Copy)]
struct ObjectivePoint {
    pos: [f64; 2],
    side: Side,
}

impl RTreeObject for ObjectivePoint {
    type Envelope = AABB<[f64; 2]>;
    fn envelope(&self) -> Self::Envelope {
        AABB::from_point(self.pos)
    }
}

impl PointDistance for ObjectivePoint {
    fn distance_2(&self, point: &[f64; 2]) -> f64 {
        let dx = self.pos[0] - point[0];
        let dy = self.pos[1] - point[1];
        dx * dx + dy * dy
    }
}

// Line styling is deliberately kept out of FrontLineConfig: `Cfg` is
// snapshotted positionally (bincode) into bfdb's session tree, so adding
// fields to it breaks decoding of older snapshots. These constants give the
// look from the reference drawing (thick coloured dashes, dotted where
// contested) without touching the config layout.
const LINE_ALPHA: f32 = 0.9;
/// Line style for a segment where one side clearly holds the adjacent ground.
const HELD_LINE: LineType = LineType::Dashed;
/// Line style for a segment that is genuinely contested.
const CONTESTED_LINE: LineType = LineType::Dotted;
/// |red - blue| / (red + blue) within the sample window below which a
/// boundary segment is treated as contested (drawn white / dotted).
const CONTESTED_THRESHOLD: f64 = 0.22;

/// Stores front line drawing state
#[derive(Debug, Clone)]
pub struct FrontLine {
    /// Stored mark IDs for the drawn frontline segments
    marks: Vec<MarkId>,
    /// Configuration
    config: FrontLineConfig,
    /// Hash of objective ownership to detect changes
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

    /// Clear all existing territory zone marks
    fn clear_marks(&mut self, msgq: &mut MsgQ) {
        for mark_id in self.marks.drain(..) {
            msgq.delete_mark(mark_id);
        }
    }

    /// Calculate a simple hash of objective ownership for change detection
    fn calculate_ownership_hash(persisted: &Persisted) -> u64 {
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;

        let mut hasher = DefaultHasher::new();
        let mut objectives: Vec<_> = persisted.objectives.into_iter().collect();
        objectives.sort_by_key(|(id, _)| *id);

        for (id, obj) in objectives {
            id.hash(&mut hasher);
            (obj.owner as u8).hash(&mut hasher);
        }

        hasher.finish()
    }

    /// Calculate map bounds with padding
    fn calculate_bounds(&self, objectives: &[(Vector2, Side)]) -> (Vector2, Vector2) {
        let first = objectives[0].0;
        let mut min = first;
        let mut max = first;

        for (pos, _) in objectives.iter().skip(1) {
            min.x = min.x.min(pos.x);
            min.y = min.y.min(pos.y);
            max.x = max.x.max(pos.x);
            max.y = max.y.max(pos.y);
        }

        // Add 15% padding
        let width = max.x - min.x;
        let height = max.y - min.y;
        let padding = width.max(height) * 0.15;

        min -= Vector2::new(padding, padding);
        max += Vector2::new(padding, padding);

        (min, max)
    }

    /// Build Voronoi tessellation grid using R-tree for O(log n) nearest-neighbor lookups
    fn build_voronoi_grid(&self, objectives: &[(Vector2, Side)], min: Vector2, max: Vector2) -> Vec<Vec<Side>> {
        // Adaptive resolution based on config
        let grid_size = self.config.samples_per_boundary.clamp(50, 200);
        let cell_width = (max.x - min.x) / grid_size as f64;
        let cell_height = (max.y - min.y) / grid_size as f64;

        info!("Frontline: Building {}x{} Voronoi grid (cell: {:.0}m x {:.0}m) using R-tree",
              grid_size, grid_size, cell_width, cell_height);

        // Build R-tree from objectives for O(log n) nearest-neighbor queries
        let points: Vec<ObjectivePoint> = objectives.iter()
            .map(|(pos, side)| ObjectivePoint {
                pos: [pos.x, pos.y],
                side: *side,
            })
            .collect();
        let rtree = RTree::bulk_load(points);

        let mut grid = vec![vec![Side::Neutral; grid_size]; grid_size];

        // For each grid cell, find nearest objective via R-tree (O(log n) per query)
        for i in 0..grid_size {
            for j in 0..grid_size {
                let x = min.x + (j as f64 + 0.5) * cell_width;
                let y = min.y + (i as f64 + 0.5) * cell_height;

                if let Some(nearest) = rtree.nearest_neighbor(&[x, y]) {
                    grid[i][j] = nearest.side;
                }
            }
        }

        grid
    }

    /// Locally dominant side around boundary cell `(i, j)`: sample a square
    /// window of the ownership grid and see which side holds more of the
    /// surrounding ground. A roughly even split is reported as `Neutral`
    /// (genuinely contested → drawn white / dotted).
    fn segment_side(grid: &[Vec<Side>], i: usize, j: usize, window: i64) -> Side {
        let rows = grid.len() as i64;
        let cols = grid[0].len() as i64;
        let (mut red, mut blue) = (0i64, 0i64);
        for di in -window..=window {
            for dj in -window..=window {
                let ii = i as i64 + di;
                let jj = j as i64 + dj;
                if ii < 0 || jj < 0 || ii >= rows || jj >= cols {
                    continue;
                }
                match grid[ii as usize][jj as usize] {
                    Side::Red => red += 1,
                    Side::Blue => blue += 1,
                    Side::Neutral => {}
                }
            }
        }
        let total = red + blue;
        if total == 0 {
            return Side::Neutral;
        }
        if (red - blue).abs() as f64 / total as f64 <= CONTESTED_THRESHOLD {
            Side::Neutral
        } else if red > blue {
            Side::Red
        } else {
            Side::Blue
        }
    }

    /// Locally dominant side at a world position (maps the point back to a
    /// grid cell, then reuses `segment_side`).
    fn side_at_world(
        grid: &[Vec<Side>],
        min: Vector2,
        cw: f64,
        ch: f64,
        p: Vector2,
        window: i64,
    ) -> Side {
        let rows = grid.len() as i64;
        let cols = grid[0].len() as i64;
        let j = (((p.x - min.x) / cw).floor() as i64).clamp(0, cols - 1);
        let i = (((p.y - min.y) / ch).floor() as i64).clamp(0, rows - 1);
        Self::segment_side(grid, i as usize, j as usize, window)
    }

    /// Trace the Red/Blue territory boundary of the ownership grid into a
    /// small number of **connected** polylines: collect the lattice edges
    /// that separate a Red cell from a Blue cell, chain them end to end
    /// through the grid vertices, then simplify each chain (Ramer–Douglas–
    /// Peucker) so a clean multi-segment line is left instead of a stair-
    /// stepped mess. Short fragments around isolated pockets are dropped.
    fn trace_frontline(
        &self,
        grid: &[Vec<Side>],
        min: Vector2,
        max: Vector2,
    ) -> Vec<Vec<Vector2>> {
        let rows = grid.len();
        if rows == 0 {
            return Vec::new();
        }
        let cols = grid[0].len();
        let cw = (max.x - min.x) / cols as f64;
        let ch = (max.y - min.y) / rows as f64;
        let vcols = cols + 1;
        let nverts = (rows + 1) * vcols;
        let vid = |vi: usize, vj: usize| (vi * vcols + vj) as u32;
        let vpos = |v: u32| {
            let v = v as usize;
            Vector2::new(min.x + (v % vcols) as f64 * cw, min.y + (v / vcols) as f64 * ch)
        };
        let opposite = |a: Side, b: Side| {
            matches!((a, b), (Side::Red, Side::Blue) | (Side::Blue, Side::Red))
        };

        // Boundary lattice edges, plus vertex -> incident edge-index adjacency.
        let mut edges: Vec<(u32, u32)> = Vec::new();
        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); nverts];
        let push_edge = |edges: &mut Vec<(u32, u32)>, adj: &mut Vec<Vec<usize>>, a: u32, b: u32| {
            let idx = edges.len();
            edges.push((a, b));
            adj[a as usize].push(idx);
            adj[b as usize].push(idx);
        };
        for i in 0..rows {
            for j in 0..cols {
                let here = grid[i][j];
                if here == Side::Neutral {
                    continue;
                }
                if j + 1 < cols && opposite(here, grid[i][j + 1]) {
                    push_edge(&mut edges, &mut adj, vid(i, j + 1), vid(i + 1, j + 1));
                }
                if i + 1 < rows && opposite(here, grid[i + 1][j]) {
                    push_edge(&mut edges, &mut adj, vid(i + 1, j), vid(i + 1, j + 1));
                }
            }
        }
        if edges.is_empty() {
            return Vec::new();
        }

        // Walk chains: start at every vertex that is an endpoint or a junction
        // (degree != 2), following unused edges; then mop up any pure loops.
        let mut used = vec![false; edges.len()];
        let other = |e: usize, v: u32| {
            let (a, b) = edges[e];
            if a == v { b } else { a }
        };
        let mut raw_chains: Vec<Vec<u32>> = Vec::new();
        let walk = |start: u32, first_edge: usize, used: &mut Vec<bool>| {
            let mut chain = vec![start];
            let mut v = start;
            let mut e = first_edge;
            loop {
                used[e] = true;
                let n = other(e, v);
                chain.push(n);
                if adj[n as usize].len() != 2 {
                    break;
                }
                match adj[n as usize].iter().copied().find(|&ne| !used[ne]) {
                    Some(ne) => {
                        v = n;
                        e = ne;
                    }
                    None => break,
                }
            }
            chain
        };
        for v in 0..nverts as u32 {
            if adj[v as usize].len() == 2 {
                continue;
            }
            for k in 0..adj[v as usize].len() {
                let e = adj[v as usize][k];
                if !used[e] {
                    raw_chains.push(walk(v, e, &mut used));
                }
            }
        }
        for e in 0..edges.len() {
            if !used[e] {
                raw_chains.push(walk(edges[e].0, e, &mut used));
            }
        }

        // Simplify + length-filter.
        let diag = (max - min).norm();
        let epsilon = (cw.max(ch) * 2.5).max(diag * 0.006);
        let min_len = diag * 0.05;
        let mut chains: Vec<Vec<Vector2>> = Vec::new();
        for rc in raw_chains {
            if rc.len() < 3 {
                continue;
            }
            let pts: Vec<Vector2> = rc.iter().map(|&v| vpos(v)).collect();
            let total: f64 = pts.windows(2).map(|w| (w[1] - w[0]).norm()).sum();
            if total < min_len {
                continue;
            }
            let mut simp = Vec::new();
            rdp(&pts, epsilon, &mut simp);
            simp.dedup_by(|a, b| (*a - *b).norm() < 1.0);
            if simp.len() >= 2 {
                chains.push(simp);
            }
        }

        // Stitch fragments: neutral territory between the two sides breaks the
        // Red|Blue adjacency, so one real front often comes out as several
        // aligned pieces. Repeatedly join the closest pair of chain endpoints
        // while the gap is under `stitch_gap`, so the map shows one line.
        let stitch_gap = diag * 0.14;
        loop {
            let mut best: Option<(usize, bool, usize, bool, f64)> = None;
            for a in 0..chains.len() {
                for b in (a + 1)..chains.len() {
                    let ends_a = [
                        (*chains[a].first().unwrap(), false),
                        (*chains[a].last().unwrap(), true),
                    ];
                    let ends_b = [
                        (*chains[b].first().unwrap(), false),
                        (*chains[b].last().unwrap(), true),
                    ];
                    for (pa, a_tail) in ends_a {
                        for (pb, b_tail) in ends_b {
                            let d = (pa - pb).norm();
                            if d < stitch_gap && best.map_or(true, |(_, _, _, _, bd)| d < bd) {
                                best = Some((a, a_tail, b, b_tail, d));
                            }
                        }
                    }
                }
            }
            let Some((a, a_tail, b, b_tail, _)) = best else { break };
            let mut ca = std::mem::take(&mut chains[a]);
            let mut cb = std::mem::take(&mut chains[b]);
            // Orient so ca's tail meets cb's head.
            if !a_tail {
                ca.reverse();
            }
            if b_tail {
                cb.reverse();
            }
            ca.extend(cb);
            chains[a] = ca;
            chains.remove(b);
        }

        // Budget: if the simplified lines still hold more segments than
        // max_marks, drop the shortest chains until they fit.
        let budget = self.config.max_marks.max(8);
        let seg_count =
            |cs: &[Vec<Vector2>]| cs.iter().map(|c| c.len().saturating_sub(1)).sum::<usize>();
        if seg_count(&chains) > budget {
            chains.sort_by(|a, b| {
                let la: f64 = a.windows(2).map(|w| (w[1] - w[0]).norm()).sum();
                let lb: f64 = b.windows(2).map(|w| (w[1] - w[0]).norm()).sum();
                lb.partial_cmp(&la).unwrap_or(std::cmp::Ordering::Equal)
            });
            while chains.len() > 1 && seg_count(&chains) > budget {
                chains.pop();
            }
        }

        // Keep a stable, readable draw order.
        chains.sort_by(|a, b| {
            a[0].x
                .partial_cmp(&b[0].x)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        chains
    }

    /// Draw the frontline: one connected dashed line per traced boundary
    /// chain, each segment coloured by local advantage.
    fn draw_frontline(&mut self, persisted: &Persisted, msgq: &mut MsgQ) {
        let all_objectives: Vec<(Vector2, Side)> = persisted
            .objectives
            .into_iter()
            .map(|(_, obj)| (obj.pos(), obj.owner))
            .collect();

        if all_objectives.len() < 2 {
            return;
        }

        let red_count = all_objectives.iter().filter(|(_, s)| *s == Side::Red).count();
        let blue_count = all_objectives.iter().filter(|(_, s)| *s == Side::Blue).count();
        if red_count == 0 || blue_count == 0 {
            info!("Frontline: one side holds no objectives, nothing to draw");
            return;
        }

        let (min, max) = self.calculate_bounds(&all_objectives);
        let grid = self.build_voronoi_grid(&all_objectives, min, max);
        let cols = grid[0].len();
        let rows = grid.len();
        let cw = (max.x - min.x) / cols as f64;
        let ch = (max.y - min.y) / rows as f64;
        let window = ((rows.min(cols) / 12).max(2).min(8)) as i64;

        let chains = self.trace_frontline(&grid, min, max);
        let seg_total: usize = chains.iter().map(|c| c.len().saturating_sub(1)).sum();

        info!(
            "Frontline: drawing {} line(s), {} segment(s) (Red obj: {}, Blue obj: {})",
            chains.len(), seg_total, red_count, blue_count
        );

        for chain in &chains {
            for w in chain.windows(2) {
                let (a, b) = (w[0], w[1]);
                let mid = (a + b) * 0.5;
                let side = Self::side_at_world(&grid, min, cw, ch, mid, window);
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
            // Clear marks if disabled
            if !self.marks.is_empty() {
                info!("Frontline: Feature disabled, clearing {} existing marks", self.marks.len());
                self.clear_marks(msgq);
                self.objective_ownership_hash = 0;
            }
            return false;
        }

        // Check if objectives have changed ownership
        let new_hash = Self::calculate_ownership_hash(persisted);
        let is_initial_calculation = self.objective_ownership_hash == 0;

        if self.config.update_on_objective_change_only && !is_initial_calculation && new_hash == self.objective_ownership_hash {
            debug!("Frontline: No objective ownership changes detected, skipping update");
            return false;
        }

        if is_initial_calculation {
            info!("Frontline: performing initial frontline draw");
        } else {
            info!("Frontline: objective ownership changed, redrawing frontline");
        }

        self.objective_ownership_hash = new_hash;

        // Clear old marks
        if !self.marks.is_empty() {
            debug!("Frontline: clearing {} old segments before redraw", self.marks.len());
        }
        self.clear_marks(msgq);

        self.draw_frontline(persisted, msgq);

        // Safety check: warn if we somehow drew an absurd number of segments.
        if self.marks.len() > 5000 {
            warn!("Frontline: WARNING - excessive segment count: {} created! This may indicate a bug.",
                  self.marks.len());
        } else {
            info!("Frontline: drew {} frontline segment(s)", self.marks.len());
        }

        true
    }

    /// Dummy method for compatibility (no longer used)
    pub fn collect_unit_pressure(&mut self, _persisted: &Persisted, _now: DateTime<Utc>) {
        // No-op: pressure system removed for polygon-only mode
    }

    /// Remove all frontline segments
    pub fn remove(mut self, msgq: &mut MsgQ) {
        self.clear_marks(msgq);
    }
}
