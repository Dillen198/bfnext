/*
Copyright 2024 Eric Stokes.

Shared frontline geometry — used by bflib to draw the F10 map overlay and by
bfdb to serve the same line to the web dashboard, so the two never disagree.

The front is the zero contour of a smooth "who controls this ground" field:
every owned objective votes for its side with a Gaussian influence that falls
off with distance (blue +1, red −1). Where the votes balance, `F = 0` — that
is the front. `F` is sampled on a grid, its zero contour traced with marching
squares, the contested stretches kept, smoothed (Chaikin), and offset to
either side to give the blue-edge / centre / red-edge lines.

Coordinates are an opaque planar system: pass metres (DCS world x/y, or a
local ENU projection of lat/lon) so the metre-denominated clamps in `Params`
mean something. Output points are in the same system.
*/

use dcso3::Vector2;
use fxhash::{FxHashMap, FxHashSet};
use log::info;
use serde_derive::{Deserialize, Serialize};

/// Tunable parameters. `Params::default()` is what both callers use.
#[derive(Debug, Clone, Copy)]
pub struct Params {
    /// Contour sampling grid is `grid_res × grid_res`. Higher = finer,
    /// slower. Clamped to [80, 400].
    pub grid_res: usize,
    /// Influence blur σ, as a multiple of the median nearest-neighbour
    /// spacing between objectives.
    pub sigma_mult: f64,
    /// σ clamp (metres).
    pub sigma_min: f64,
    pub sigma_max: f64,
    /// Drop a traced contour shorter than this (metres).
    pub min_front_len: f64,
    /// A contour segment counts as a real front only if a blue AND a red
    /// objective sit within `contested_mult · σ` of it.
    pub contested_mult: f64,
    /// Perpendicular offset of the blue-edge / red-edge lines from the
    /// centre line, as a fraction of σ.
    pub edge_offset_frac: f64,
    /// Offset clamp (metres).
    pub edge_offset_min: f64,
    pub edge_offset_max: f64,
    /// Chaikin smoothing passes applied to each centre line.
    pub chaikin_iters: usize,
    /// Bounding-box padding as a fraction of the box diagonal.
    pub pad_frac: f64,
}

impl Default for Params {
    fn default() -> Self {
        Self {
            grid_res: 160,
            sigma_mult: 2.4,
            sigma_min: 20_000.0,
            sigma_max: 95_000.0,
            min_front_len: 30_000.0,
            contested_mult: 2.3,
            edge_offset_frac: 0.035,
            edge_offset_min: 900.0,
            edge_offset_max: 2_200.0,
            chaikin_iters: 3,
            pad_frac: 0.12,
        }
    }
}

/// One front: the centre ("no man's land") line and the offset lines on the
/// blue and red sides. Points are `[x, y]` in the input coordinate system.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Front {
    pub mid: Vec<[f64; 2]>,
    pub blue: Vec<[f64; 2]>,
    pub red: Vec<[f64; 2]>,
}

#[derive(Debug, Clone, Copy)]
struct Obj {
    pos: Vector2,
    /// > 0 blue, < 0 red
    sign: f64,
}

/// A grid-edge crossing, keyed so the two cells sharing an edge land on the
/// same key and their segments join. `h = true` → horizontal edge between
/// corner (i, j) and (i, j+1); `h = false` → vertical edge between (i, j) and
/// (i+1, j).
type EdgeKey = (bool, usize, usize);

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

/// Chaikin corner-cutting: turns a coarse polyline into a smooth curve while
/// keeping the endpoints. Each pass roughly doubles the point count.
fn chaikin(pts: &[Vector2], iters: usize) -> Vec<Vector2> {
    let mut cur = pts.to_vec();
    for _ in 0..iters {
        if cur.len() < 3 {
            break;
        }
        let mut next = Vec::with_capacity(cur.len() * 2);
        next.push(cur[0]);
        for w in cur.windows(2) {
            let (p, q) = (w[0], w[1]);
            next.push(p + (q - p) * 0.25);
            next.push(p + (q - p) * 0.75);
        }
        next.push(*cur.last().unwrap());
        cur = next;
    }
    cur
}

/// Trace the F = 0 contour(s) of the ownership field over `objs`
/// (`(x, y, sign)`, sign > 0 blue / < 0 red), smooth them, and build the
/// blue-side / red-side offset lines. Empty when there is no blue/red contact.
pub fn compute(objs: &[(f64, f64, f64)], p: &Params) -> Vec<Front> {
    let objs: Vec<Obj> = objs
        .iter()
        .filter(|(_, _, s)| *s != 0.0)
        .map(|&(x, y, s)| Obj {
            pos: Vector2::new(x, y),
            sign: s,
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
    let pad = (mx - mn).norm() * p.pad_frac;
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
    let sigma = (spacing * p.sigma_mult).clamp(p.sigma_min, p.sigma_max);
    let inv_2s2 = 1.0 / (2.0 * sigma * sigma);
    let cutoff2 = (3.0 * sigma).powi(2);

    // Grid. i indexes x, j indexes y.
    let res = p.grid_res.clamp(80, 400);
    let (rows, cols) = (res, res);
    let dx = (mx.x - mn.x) / rows as f64;
    let dy = (mx.y - mn.y) / cols as f64;
    let field = |q: Vector2| -> f64 {
        let mut s = 0.0;
        for o in &objs {
            let d2 = (o.pos - q).norm_squared();
            if d2 <= cutoff2 {
                s += o.sign * (-d2 * inv_2s2).exp();
            }
        }
        s
    };
    let mut f = vec![vec![0.0f64; cols + 1]; rows + 1];
    for i in 0..=rows {
        for j in 0..=cols {
            f[i][j] = field(Vector2::new(mn.x + i as f64 * dx, mn.y + j as f64 * dy));
        }
    }

    let hpos = |i: usize, j: usize| {
        let (a, b) = (f[i][j], f[i][j + 1]);
        let t = if (a - b).abs() < 1e-12 { 0.5 } else { a / (a - b) };
        Vector2::new(mn.x + i as f64 * dx, mn.y + (j as f64 + t) * dy)
    };
    let vpos = |i: usize, j: usize| {
        let (a, b) = (f[i][j], f[i + 1][j]);
        let t = if (a - b).abs() < 1e-12 { 0.5 } else { a / (a - b) };
        Vector2::new(mn.x + (i as f64 + t) * dx, mn.y + j as f64 * dy)
    };

    // Marching squares.
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
            let ab = ((true, i, j), hpos(i, j));
            let cd = ((true, i + 1, j), hpos(i + 1, j));
            let da = ((false, i, j), vpos(i, j));
            let bc = ((false, i, j + 1), vpos(i, j + 1));
            match c {
                1 | 14 => push(&mut segs, &mut pos_of, ab.0, ab.1, da.0, da.1),
                2 | 13 => push(&mut segs, &mut pos_of, ab.0, ab.1, bc.0, bc.1),
                3 | 12 => push(&mut segs, &mut pos_of, da.0, da.1, bc.0, bc.1),
                4 | 11 => push(&mut segs, &mut pos_of, bc.0, bc.1, cd.0, cd.1),
                6 | 9 => push(&mut segs, &mut pos_of, ab.0, ab.1, cd.0, cd.1),
                7 | 8 => push(&mut segs, &mut pos_of, cd.0, cd.1, da.0, da.1),
                5 => {
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

    // "Contested" = a blue AND a red objective within `contested_mult · σ`.
    // Used to keep the real front and drop the blue-vs-open-sea contour,
    // but applied per *chain* (not per segment) so a kept front stays
    // continuous — only its non-contested tails are trimmed.
    let keep_dist2 = (sigma * p.contested_mult).powi(2);
    let contested_at = |q: Vector2| -> bool {
        let (mut b, mut r) = (false, false);
        for o in &objs {
            if (o.pos - q).norm_squared() <= keep_dist2 {
                if o.sign > 0.0 {
                    b = true;
                } else {
                    r = true;
                }
                if b && r {
                    return true;
                }
            }
        }
        false
    };

    // Chain the segments into polylines.
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

    let epsilon = (dx.max(dy) * 0.4).clamp(500.0, 2_500.0);
    let gap = (sigma * p.edge_offset_frac).clamp(p.edge_offset_min, p.edge_offset_max);
    let grad_h = dx.min(dy) * 0.75;
    let grad_toward_blue = |q: Vector2| -> Vector2 {
        let gx = field(q + Vector2::new(grad_h, 0.0)) - field(q - Vector2::new(grad_h, 0.0));
        let gy = field(q + Vector2::new(0.0, grad_h)) - field(q - Vector2::new(0.0, grad_h));
        let g = Vector2::new(gx, gy);
        if g.norm() > 1e-9 {
            g.normalize()
        } else {
            Vector2::new(0.0, 0.0)
        }
    };

    let v2a = |v: Vector2| [v.x, v.y];
    let mut fronts: Vec<Front> = Vec::new();
    let mut dropped = 0usize;
    let n_raw = raw.len();
    for chain in raw {
        if chain.len() < 2 {
            continue;
        }
        // Trim the chain to its contested span: find the first and last
        // vertex that sits between blue and red, keep everything in between
        // (so the front stays one continuous line), drop the rest.
        let flags: Vec<bool> = chain.iter().map(|&q| contested_at(q)).collect();
        let first = flags.iter().position(|&c| c);
        let last = flags.iter().rposition(|&c| c);
        let (Some(first), Some(last)) = (first, last) else {
            dropped += 1;
            continue;
        };
        let n_contested = flags[first..=last].iter().filter(|&&c| c).count();
        // Mostly-uncontested chains (a coastline loop that only grazes the
        // front) are noise.
        if (n_contested as f64) < 0.35 * (last - first + 1) as f64 {
            dropped += 1;
            continue;
        }
        let chain: Vec<Vector2> = chain[first..=last].to_vec();
        if chain.len() < 2 {
            continue;
        }
        let len: f64 = chain.windows(2).map(|w| (w[1] - w[0]).norm()).sum();
        if len < p.min_front_len {
            dropped += 1;
            continue;
        }
        let mut simp = Vec::new();
        rdp(&chain, epsilon, &mut simp);
        simp.dedup_by(|a, b| (*a - *b).norm() < 1.0);
        if simp.len() < 2 {
            continue;
        }
        let mid = chaikin(&simp, p.chaikin_iters);

        let mut blue = Vec::with_capacity(mid.len());
        let mut red = Vec::with_capacity(mid.len());
        for (k, &q) in mid.iter().enumerate() {
            let mut nrm = grad_toward_blue(q);
            if nrm.norm() < 0.5 {
                let a = mid[k.saturating_sub(1)];
                let b = mid[(k + 1).min(mid.len() - 1)];
                let t = b - a;
                nrm = if t.norm() > 1e-6 {
                    Vector2::new(-t.y, t.x).normalize()
                } else {
                    Vector2::new(1.0, 0.0)
                };
            }
            blue.push(v2a(q + nrm * gap));
            red.push(v2a(q - nrm * gap));
        }
        fronts.push(Front {
            mid: mid.iter().map(|&v| v2a(v)).collect(),
            blue,
            red,
        });
    }
    fronts.sort_by(|a, b| {
        a.mid[0][0]
            .partial_cmp(&b.mid[0][0])
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    info!(
        "Frontline: {} contour(s), {} dropped as <{:.0} km, {} front(s), gap {:.0} km",
        n_raw,
        dropped,
        p.min_front_len / 1000.0,
        fronts.len(),
        gap / 1000.0
    );
    fronts
}
