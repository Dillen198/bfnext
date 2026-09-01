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
//! The frontline is drawn as a dashed line on the F10 map that separates the
//! objectives one side holds from the objectives the other side holds. It is
//! built from the objectives themselves, not a territory grid:
//!
//! 1. For every objective, look at its nearest neighbours. A neighbour on the
//!    opposite side is a **contested pair**; the front crosses at the midpoint.
//! 2. Drop pairs that are far-flung outliers (an isolated pocket deep in enemy
//!    land) so they don't drag the line across the map.
//! 3. Order the midpoints along their principal axis (the direction they are
//!    most spread along — usually roughly N/S) into a single path.
//! 4. Simplify the path (Ramer–Douglas–Peucker) so it reads as a handful of
//!    clean segments.
//! 5. Colour each segment by which side is stronger there (objective health),
//!    or white/dotted when the two sides are even.

use crate::{db::persisted::Persisted, msgq::MsgQ};
use bfprotocols::cfg::FrontLineConfig;
use chrono::{DateTime, Utc};
use dcso3::{
    coalition::Side,
    trigger::{LineSpec, LineType, MarkId, SideFilter},
    Color, LuaVec3, Vector2, Vector3,
};
use fxhash::{FxHashMap, FxHashSet};
use log::*;
use rstar::{PointDistance, RTree, RTreeObject, AABB};

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

/// Principal axis (unit vector) of a 2×2 covariance `[[sxx, sxy], [sxy, syy]]`
/// — the direction the frontline nodes are most spread along, used to order
/// them into a path.
fn principal_axis(sxx: f64, sxy: f64, syy: f64) -> Vector2 {
    if sxy.abs() < 1e-9 {
        return if sxx >= syy {
            Vector2::new(1.0, 0.0)
        } else {
            Vector2::new(0.0, 1.0)
        };
    }
    let tr = sxx + syy;
    let det = sxx * syy - sxy * sxy;
    let lambda = tr / 2.0 + ((tr * tr / 4.0) - det).max(0.0).sqrt();
    let v = Vector2::new(sxy, lambda - sxx);
    let n = v.norm();
    if n < 1e-9 {
        Vector2::new(1.0, 0.0)
    } else {
        v / n
    }
}

/// Union-find root with path halving.
fn uf_find(parent: &mut [usize], mut x: usize) -> usize {
    while parent[x] != x {
        parent[x] = parent[parent[x]];
        x = parent[x];
    }
    x
}

/// Order one cluster of frontline nodes into a path (nearest-neighbour walk
/// from the principal-axis extreme), trim outlier end-hops, and simplify with
/// Ramer–Douglas–Peucker down to at most `budget` segments.
fn order_and_simplify(cluster: &[(Vector2, Side)], budget: usize) -> Option<Vec<Vector2>> {
    if cluster.len() < 2 {
        return None;
    }
    let centroid = cluster
        .iter()
        .fold(Vector2::zeros(), |acc, (p, _)| acc + *p)
        / cluster.len() as f64;
    let (mut sxx, mut sxy, mut syy) = (0.0, 0.0, 0.0);
    for (p, _) in cluster {
        let d = *p - centroid;
        sxx += d.x * d.x;
        sxy += d.x * d.y;
        syy += d.y * d.y;
    }
    let axis = principal_axis(sxx, sxy, syy);

    let start = (0..cluster.len())
        .min_by(|&a, &b| {
            let pa = (cluster[a].0 - centroid).dot(&axis);
            let pb = (cluster[b].0 - centroid).dot(&axis);
            pa.partial_cmp(&pb).unwrap_or(std::cmp::Ordering::Equal)
        })
        .unwrap();
    let mut visited = vec![false; cluster.len()];
    let mut ordered: Vec<Vector2> = Vec::with_capacity(cluster.len());
    let mut cur = start;
    loop {
        visited[cur] = true;
        ordered.push(cluster[cur].0);
        let next = (0..cluster.len()).filter(|&k| !visited[k]).min_by(|&a, &b| {
            let da = (cluster[a].0 - cluster[cur].0).norm();
            let db = (cluster[b].0 - cluster[cur].0).norm();
            da.partial_cmp(&db).unwrap_or(std::cmp::Ordering::Equal)
        });
        match next {
            Some(n) => cur = n,
            None => break,
        }
    }
    ordered.dedup_by(|a, b| (*a - *b).norm() < 500.0);
    if ordered.len() < 2 {
        return None;
    }

    if ordered.len() > 3 {
        let mut hops: Vec<f64> = ordered.windows(2).map(|w| (w[0] - w[1]).norm()).collect();
        hops.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let hop_cutoff = hops[hops.len() / 2].max(1.0) * 3.0;
        while ordered.len() > 3 {
            let n = ordered.len();
            if (ordered[n - 1] - ordered[n - 2]).norm() > hop_cutoff {
                ordered.pop();
            } else if (ordered[1] - ordered[0]).norm() > hop_cutoff {
                ordered.remove(0);
            } else {
                break;
            }
        }
    }

    let span = (ordered[0] - ordered[ordered.len() - 1]).norm().max(1.0);
    let mut epsilon = (span * 0.02).clamp(1_000.0, 15_000.0);
    let mut simplified = Vec::new();
    rdp(&ordered, epsilon, &mut simplified);
    simplified.dedup_by(|a, b| (*a - *b).norm() < 1.0);
    while simplified.len().saturating_sub(1) > budget && epsilon < span {
        epsilon *= 2.0;
        simplified.clear();
        rdp(&ordered, epsilon, &mut simplified);
        simplified.dedup_by(|a, b| (*a - *b).norm() < 1.0);
    }
    (simplified.len() >= 2).then_some(simplified)
}

/// R-tree node: an objective's position plus its index into the `objs` slice.
#[derive(Debug, Clone, Copy)]
struct ObjIdx {
    pos: [f64; 2],
    idx: usize,
}

impl RTreeObject for ObjIdx {
    type Envelope = AABB<[f64; 2]>;
    fn envelope(&self) -> Self::Envelope {
        AABB::from_point(self.pos)
    }
}

impl PointDistance for ObjIdx {
    fn distance_2(&self, point: &[f64; 2]) -> f64 {
        let dx = self.pos[0] - point[0];
        let dy = self.pos[1] - point[1];
        dx * dx + dy * dy
    }
}

// Line styling is deliberately kept out of FrontLineConfig: `Cfg` is
// snapshotted positionally (bincode) into bfdb's session tree, so adding
// fields to it breaks decoding of older snapshots. These constants give the
// look from the reference drawing (coloured dashes, dotted where contested)
// without touching the config layout.
const LINE_ALPHA: f32 = 0.9;
/// Line style for a stretch of front one side clearly dominates.
const HELD_LINE: LineType = LineType::Dashed;
/// Line style for a genuinely contested stretch.
const CONTESTED_LINE: LineType = LineType::Dotted;
/// Objective-health difference (percentage points) between the two sides of a
/// contested pair below which that stretch is treated as contested (white).
const HEALTH_CONTESTED_DELTA: f64 = 15.0;
/// How many nearest neighbours of each objective to test for a contested pair.
const NEIGHBOURS: usize = 6;

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

    /// Delete every frontline segment we've drawn.
    fn clear_marks(&mut self, msgq: &mut MsgQ) {
        for mark_id in self.marks.drain(..) {
            msgq.delete_mark(mark_id);
        }
    }

    /// Calculate a simple hash of objective ownership for change detection
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

    /// Build the frontline(s): for a Syria-style map with an island and two
    /// land borders this comes out as several separate lines (Cyprus, the
    /// Turkey/Syria front, the Syria/Israel front). Each entry is an ordered,
    /// simplified polyline plus the contested-pair midpoints (with the locally
    /// stronger side) that colour its segments. Empty when there is no front.
    fn compute_frontline(&self, persisted: &Persisted) -> Vec<(Vec<Vector2>, Vec<(Vector2, Side)>)> {
        use bfprotocols::db::objective::ObjectiveKind as K;

        // (position, side, health%) for every non-neutral objective that
        // actually defines held ground. SAM sites, command centres, factories
        // and carrier groups are excluded: they're scattered through friendly
        // territory and would turn the front into salt-and-pepper noise.
        let objs: Vec<(Vector2, Side, f64)> = persisted
            .objectives
            .into_iter()
            .filter(|(_, o)| o.owner != Side::Neutral)
            .filter(|(_, o)| {
                matches!(
                    o.kind(),
                    K::Airbase | K::NavalBase | K::Logistics | K::Fob | K::Farp { .. }
                )
            })
            .map(|(_, o)| (o.pos(), o.owner, o.health() as f64))
            .collect();
        if objs.len() < 3 {
            return Vec::new();
        }
        let red = objs.iter().filter(|(_, s, _)| *s == Side::Red).count();
        let blue = objs.iter().filter(|(_, s, _)| *s == Side::Blue).count();
        if red == 0 || blue == 0 {
            info!("Frontline: one side holds no objectives, nothing to draw");
            return Vec::new();
        }

        let tree = RTree::bulk_load(
            objs.iter()
                .enumerate()
                .map(|(idx, (p, _, _))| ObjIdx { pos: [p.x, p.y], idx })
                .collect::<Vec<_>>(),
        );

        // Contested pairs: an objective and a near neighbour on the other side.
        let mut pairs: Vec<(usize, usize, f64)> = Vec::new();
        let mut seen: FxHashSet<(usize, usize)> = FxHashSet::default();
        for (i, (p, si, _)) in objs.iter().enumerate() {
            let mut tested = 0;
            for nb in tree.nearest_neighbor_iter(&[p.x, p.y]) {
                if nb.idx == i {
                    continue;
                }
                tested += 1;
                if tested > NEIGHBOURS {
                    break;
                }
                let (pj, sj, _) = objs[nb.idx];
                if *si != sj {
                    let key = (i.min(nb.idx), i.max(nb.idx));
                    if seen.insert(key) {
                        pairs.push((i, nb.idx, (pj - *p).norm()));
                    }
                }
            }
        }
        if pairs.len() < 2 {
            return Vec::new();
        }

        // Drop far-flung outlier pairs (an isolated pocket, or a pair reaching
        // across open water): keep pairs no longer than 1.8× the median.
        let mut lens: Vec<f64> = pairs.iter().map(|(_, _, d)| *d).collect();
        lens.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let median_pair = lens[lens.len() / 2].max(1.0);
        pairs.retain(|(_, _, d)| *d <= median_pair * 1.8);
        if pairs.len() < 2 {
            return Vec::new();
        }

        // Frontline nodes: midpoint of each contested pair, tagged with the
        // side whose objective is healthier there, or Neutral when they're
        // within HEALTH_CONTESTED_DELTA of each other.
        let nodes: Vec<(Vector2, Side)> = pairs
            .iter()
            .map(|&(a, b, _)| {
                let (pa, sa, ha) = objs[a];
                let (pb, sb, hb) = objs[b];
                let side = if (ha - hb).abs() < HEALTH_CONTESTED_DELTA {
                    Side::Neutral
                } else if ha > hb {
                    sa
                } else {
                    sb
                };
                ((pa + pb) * 0.5, side)
            })
            .collect();

        // Cluster the nodes: sea gaps (Cyprus ↔ mainland) and front-free
        // stretches of coast split the front into pieces. Single-linkage
        // union-find, linking nodes within `link_dist` of each other.
        let node_tree = RTree::bulk_load(
            nodes
                .iter()
                .enumerate()
                .map(|(idx, (p, _))| ObjIdx { pos: [p.x, p.y], idx })
                .collect::<Vec<_>>(),
        );
        let mut nn: Vec<f64> = Vec::new();
        for (i, (p, _)) in nodes.iter().enumerate() {
            if let Some(o) = node_tree
                .nearest_neighbor_iter(&[p.x, p.y])
                .find(|o| o.idx != i)
            {
                nn.push((Vector2::new(o.pos[0], o.pos[1]) - *p).norm());
            }
        }
        nn.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let spacing = nn.get(nn.len() / 2).copied().unwrap_or(20_000.0).max(1.0);
        let link_dist = (spacing * 4.0).clamp(20_000.0, 90_000.0);

        let mut parent: Vec<usize> = (0..nodes.len()).collect();
        for (i, (p, _)) in nodes.iter().enumerate() {
            for o in node_tree.locate_within_distance([p.x, p.y], link_dist * link_dist) {
                if o.idx != i {
                    let (ra, rb) = (uf_find(&mut parent, i), uf_find(&mut parent, o.idx));
                    if ra != rb {
                        parent[ra] = rb;
                    }
                }
            }
        }
        let mut groups: FxHashMap<usize, Vec<(Vector2, Side)>> = FxHashMap::default();
        for (i, node) in nodes.iter().enumerate() {
            groups.entry(uf_find(&mut parent, i)).or_default().push(*node);
        }

        let budget = self.config.max_marks.max(8);
        let mut lines: Vec<(Vec<Vector2>, Vec<(Vector2, Side)>)> = Vec::new();
        for (_, group) in groups {
            if group.len() < 3 {
                continue;
            }
            // Drop groups too small to be a real front (< 25 km across).
            let (mut mn, mut mx) = (group[0].0, group[0].0);
            for (p, _) in &group {
                mn.x = mn.x.min(p.x);
                mn.y = mn.y.min(p.y);
                mx.x = mx.x.max(p.x);
                mx.y = mx.y.max(p.y);
            }
            if (mx - mn).norm() < 25_000.0 {
                continue;
            }
            if let Some(line) = order_and_simplify(&group, budget) {
                lines.push((line, group));
            }
        }
        lines
    }

    /// Draw the frontline: one connected dashed line per front, each segment
    /// coloured by which side is stronger along it.
    fn draw_frontline(&mut self, persisted: &Persisted, msgq: &mut MsgQ) {
        let lines = self.compute_frontline(persisted);
        if lines.is_empty() {
            return;
        }
        let pts: usize = lines.iter().map(|(l, _)| l.len()).sum();
        info!("Frontline: drawing {} line(s), {} point(s) total", lines.len(), pts);

        for (line, nodes) in &lines {
            for w in line.windows(2) {
                let (a, b) = (w[0], w[1]);
                let mid = (a + b) * 0.5;
                let side = nodes
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
