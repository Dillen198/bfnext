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
//! The front is drawn as the zero contour of a smooth "who controls this
//! ground" field:
//!
//! 1. Every owned, capturable objective on the ground votes for its side with
//!    an influence that falls off with distance (a Gaussian). Blue is +1,
//!    red is −1.
//! 2. The field is `F(p) = Σ sᵢ · exp(−|p − objᵢ|² / 2σ²)`. Where blue
//!    influence balances red influence, `F = 0` — that is the front.
//! 3. `σ` (the blur radius) is a few times the spacing between objectives, so
//!    the line ignores a single base sitting behind enemy lines and instead
//!    follows the overall division of the theatre — the way a staff officer
//!    would draw it.
//! 4. `F` is sampled on a grid and the `F = 0` contour is traced with
//!    marching squares, which produces smooth, connected lines. A theatre
//!    with an island and two land borders comes out as several separate
//!    fronts on its own.
//! 5. Short fragments are dropped and each line is lightly simplified, then
//!    drawn as a dashed line on the F10 map.

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

const LINE_ALPHA: f32 = 0.9;
const FRONT_LINE: LineType = LineType::Dashed;
/// Bandwidth of the influence blur, as a multiple of the median spacing
/// between neighbouring objectives. Bigger = smoother, more strategic.
const SIGMA_MULT: f64 = 2.6;
/// σ is clamped to this range (metres) regardless of objective density.
const SIGMA_MIN: f64 = 22_000.0;
const SIGMA_MAX: f64 = 95_000.0;
/// A traced contour shorter than this (metres) is noise around a pocket.
const MIN_FRONT_LEN: f64 = 30_000.0;

/// Perpendicular distance from `p` to segment `a`–`b` (to `a` if degenerate).
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

#[derive(Debug, Clone, Copy)]
struct Obj {
    pos: Vector2,
    /// +1 blue, −1 red
    sign: f64,
}

/// A grid-edge crossing, keyed so that the two cells sharing an edge land on
/// the same key and their segments join. `h = true` → horizontal edge between
/// corner (i, j) and (i, j+1); `h = false` → vertical edge between (i, j) and
/// (i+1, j).
type EdgeKey = (bool, usize, usize);

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

    /// Trace the F = 0 contour(s) of the ownership field into simplified
    /// polylines.
    fn compute_frontlines(&self, persisted: &Persisted) -> Vec<Vec<Vector2>> {
        use bfprotocols::db::objective::ObjectiveKind as K;

        let objs: Vec<Obj> = persisted
            .objectives
            .into_iter()
            .filter(|(_, o)| matches!(o.owner, Side::Blue | Side::Red))
            .filter(|(_, o)| !matches!(o.kind(), K::CarrierGroup { .. }))
            .map(|(_, o)| Obj {
                pos: o.pos(),
                sign: if o.owner == Side::Blue { 1.0 } else { -1.0 },
            })
            .collect();
        let blue = objs.iter().filter(|o| o.sign > 0.0).count();
        let red = objs.len() - blue;
        info!("Frontline: {} objectives ({} blue / {} red)", objs.len(), blue, red);
        if objs.len() < 4 || blue == 0 || red == 0 {
            return Vec::new();
        }

        // Bounding box + padding.
        let (mut mn, mut mx) = (objs[0].pos, objs[0].pos);
        for o in &objs {
            mn.x = mn.x.min(o.pos.x);
            mn.y = mn.y.min(o.pos.y);
            mx.x = mx.x.max(o.pos.x);
            mx.y = mx.y.max(o.pos.y);
        }
        let pad = (mx - mn).norm() * 0.05;
        mn -= Vector2::new(pad, pad);
        mx += Vector2::new(pad, pad);

        // σ from the median nearest-neighbour spacing.
        let mut nn: Vec<f64> = Vec::with_capacity(objs.len());
        for (i, a) in objs.iter().enumerate() {
            let mut best = f64::INFINITY;
            for (j, b) in objs.iter().enumerate() {
                if i != j {
                    best = best.min((a.pos - b.pos).norm());
                }
            }
            if best.is_finite() {
                nn.push(best);
            }
        }
        nn.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let spacing = nn.get(nn.len() / 2).copied().unwrap_or(30_000.0).max(1.0);
        let sigma = (spacing * SIGMA_MULT).clamp(SIGMA_MIN, SIGMA_MAX);
        let inv_2s2 = 1.0 / (2.0 * sigma * sigma);
        let cutoff2 = (3.0 * sigma).powi(2);

        // Grid. i indexes north (Vector2.x), j indexes east (Vector2.y).
        let res = (self.config.samples_per_boundary.clamp(60, 240)).max(60);
        let (rows, cols) = (res, res);
        let dn = (mx.x - mn.x) / rows as f64;
        let de = (mx.y - mn.y) / cols as f64;
        let field = |p: Vector2| -> f64 {
            let mut s = 0.0;
            for o in &objs {
                let d2 = (o.pos - p).norm_squared();
                if d2 <= cutoff2 {
                    s += o.sign * (-d2 * inv_2s2).exp();
                }
            }
            s
        };
        let mut f = vec![vec![0.0f64; cols + 1]; rows + 1];
        for i in 0..=rows {
            for j in 0..=cols {
                f[i][j] = field(Vector2::new(mn.x + i as f64 * dn, mn.y + j as f64 * de));
            }
        }

        // Position of a crossing on a grid edge (linear interpolation of F=0).
        let hpos = |i: usize, j: usize| {
            let (a, b) = (f[i][j], f[i][j + 1]);
            let t = if (a - b).abs() < 1e-12 { 0.5 } else { a / (a - b) };
            Vector2::new(mn.x + i as f64 * dn, mn.y + (j as f64 + t) * de)
        };
        let vpos = |i: usize, j: usize| {
            let (a, b) = (f[i][j], f[i + 1][j]);
            let t = if (a - b).abs() < 1e-12 { 0.5 } else { a / (a - b) };
            Vector2::new(mn.x + (i as f64 + t) * dn, mn.y + j as f64 * de)
        };

        // Marching squares: collect segments between edge crossings.
        let mut pos_of: FxHashMap<EdgeKey, Vector2> = FxHashMap::default();
        let mut segs: Vec<(EdgeKey, EdgeKey)> = Vec::new();
        let push = |segs: &mut Vec<(EdgeKey, EdgeKey)>,
                    pos_of: &mut FxHashMap<EdgeKey, Vector2>,
                    e0: EdgeKey,
                    p0: Vector2,
                    e1: EdgeKey,
                    p1: Vector2| {
            pos_of.entry(e0).or_insert(p0);
            pos_of.entry(e1).or_insert(p1);
            segs.push((e0, e1));
        };
        for i in 0..rows {
            for j in 0..cols {
                // Corners A(i,j) B(i,j+1) C(i+1,j+1) D(i+1,j)
                let mut c = 0u8;
                if f[i][j] > 0.0 {
                    c |= 1;
                }
                if f[i][j + 1] > 0.0 {
                    c |= 2;
                }
                if f[i + 1][j + 1] > 0.0 {
                    c |= 4;
                }
                if f[i + 1][j] > 0.0 {
                    c |= 8;
                }
                if c == 0 || c == 15 {
                    continue;
                }
                let ab = ((true, i, j), hpos(i, j)); // top edge
                let cd = ((true, i + 1, j), hpos(i + 1, j)); // bottom edge
                let da = ((false, i, j), vpos(i, j)); // left edge
                let bc = ((false, i, j + 1), vpos(i, j + 1)); // right edge
                match c {
                    1 | 14 => push(&mut segs, &mut pos_of, ab.0, ab.1, da.0, da.1),
                    2 | 13 => push(&mut segs, &mut pos_of, ab.0, ab.1, bc.0, bc.1),
                    3 | 12 => push(&mut segs, &mut pos_of, da.0, da.1, bc.0, bc.1),
                    4 | 11 => push(&mut segs, &mut pos_of, bc.0, bc.1, cd.0, cd.1),
                    6 | 9 => push(&mut segs, &mut pos_of, ab.0, ab.1, cd.0, cd.1),
                    7 | 8 => push(&mut segs, &mut pos_of, cd.0, cd.1, da.0, da.1),
                    5 => {
                        // saddle — connect A-corner and C-corner pairs
                        push(&mut segs, &mut pos_of, ab.0, ab.1, da.0, da.1);
                        push(&mut segs, &mut pos_of, bc.0, bc.1, cd.0, cd.1);
                    }
                    10 => {
                        push(&mut segs, &mut pos_of, ab.0, ab.1, bc.0, bc.1);
                        push(&mut segs, &mut pos_of, cd.0, cd.1, da.0, da.1);
                    }
                    _ => {}
                }
            }
        }
        info!(
            "Frontline: σ {:.0} km, {}×{} grid, {} contour segments",
            sigma / 1000.0,
            res,
            res,
            segs.len()
        );
        if segs.is_empty() {
            return Vec::new();
        }

        // Index crossings and chain the segments into polylines.
        let mut node_of: FxHashMap<EdgeKey, usize> = FxHashMap::default();
        let mut positions: Vec<Vector2> = Vec::with_capacity(pos_of.len());
        for (k, v) in &pos_of {
            node_of.insert(*k, positions.len());
            positions.push(*v);
        }
        let n = positions.len();
        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (a, b) in &segs {
            let (ia, ib) = (node_of[a], node_of[b]);
            adj[ia].push(ib);
            adj[ib].push(ia);
        }

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

        let epsilon = (dn.max(de) * 1.5).clamp(1_500.0, 6_000.0);
        let mut kept: Vec<Vec<Vector2>> = Vec::new();
        let mut dropped = 0usize;
        for chain in raw {
            if chain.len() < 2 {
                continue;
            }
            let len: f64 = chain.windows(2).map(|w| (w[1] - w[0]).norm()).sum();
            if len < MIN_FRONT_LEN {
                dropped += 1;
                continue;
            }
            let mut simp = Vec::new();
            rdp(&chain, epsilon, &mut simp);
            simp.dedup_by(|a, b| (*a - *b).norm() < 1.0);
            if simp.len() >= 2 {
                kept.push(simp);
            }
        }
        kept.sort_by(|a, b| {
            a[0].x
                .partial_cmp(&b[0].x)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        info!(
            "Frontline: {} contour(s), {} dropped as <{:.0} km, {} kept",
            kept.len() + dropped,
            dropped,
            MIN_FRONT_LEN / 1000.0,
            kept.len()
        );
        kept
    }

    fn draw_frontline(&mut self, persisted: &Persisted, msgq: &mut MsgQ) {
        let lines = self.compute_frontlines(persisted);
        if lines.is_empty() {
            return;
        }
        let total: usize = lines.iter().map(|l| l.len().saturating_sub(1)).sum();
        info!("Frontline: drawing {} line(s), {} segment(s)", lines.len(), total);
        let color = Color::new(1.0, 1.0, 1.0, LINE_ALPHA);
        for line in &lines {
            for w in line.windows(2) {
                let (a, b) = (w[0], w[1]);
                let mark_id = MarkId::new();
                msgq.line_to_all(
                    SideFilter::All,
                    mark_id,
                    LineSpec {
                        start: LuaVec3(Vector3::new(a.x, 0., a.y)),
                        end: LuaVec3(Vector3::new(b.x, 0., b.y)),
                        color,
                        line_type: FRONT_LINE,
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
