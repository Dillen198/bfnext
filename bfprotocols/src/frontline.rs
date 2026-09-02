/*
Copyright 2024 Eric Stokes.

Shared frontline geometry — used by bflib to draw the F10 map overlay and by
bfdb to serve the same lines to the web dashboard, so the two never disagree.

Every owned objective votes for its side with a Gaussian influence that falls
off with distance (blue +1, red −1); `F` is that field. The **white** centre
line is `F = 0`. The **blue** and **red** lines are the `F = ±edge_level`
iso-contours — the boundary of each side's dominance. Where the two sides'
objectives sit close, the three lines bunch together (a sharp front); where
there's a wide contested band, they spread. Only stretches that actually run
between a blue and a red objective are kept (no coastline loops), and each is
chained, stitched, and Chaikin-smoothed.

Coordinates are an opaque planar system: pass metres (DCS world x/y, or a
local ENU projection of lat/lon) so the metre clamps in `Params` mean
something. Output points are in the same system.
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
    /// The blue and red lines are the `F = ±edge_level` iso-contours of the
    /// influence field (the white line is `F = 0`). Bigger = the coloured
    /// lines pull back further onto each side's own ground.
    pub edge_level: f64,
    /// Chaikin smoothing passes applied to each line.
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
            min_front_len: 18_000.0,
            contested_mult: 2.3,
            edge_level: 0.13,
            chaikin_iters: 3,
            pad_frac: 0.12,
        }
    }
}

/// The frontline as three independent sets of polylines: the white centre
/// ("no man's land", `F = 0`), the blue-dominance edge (`F = +edge_level`)
/// and the red-dominance edge (`F = -edge_level`). Points are `[x, y]` in the
/// input coordinate system.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Frontlines {
    pub mid: Vec<Vec<[f64; 2]>>,
    pub blue: Vec<Vec<[f64; 2]>>,
    pub red: Vec<Vec<[f64; 2]>>,
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

/// Trace the F = 0 / F = ±edge_level contours of the ownership field over
/// `objs` (`(x, y, sign)`, sign > 0 blue / < 0 red).
pub fn compute(objs: &[(f64, f64, f64)], p: &Params) -> Frontlines {
    let objs: Vec<Obj> = objs
        .iter()
        .filter(|(_, _, s)| *s != 0.0)
        .map(|&(x, y, s)| Obj {
            pos: Vector2::new(x, y),
            sign: s,
        })
        .collect();
    let blue_n = objs.iter().filter(|o| o.sign > 0.0).count();
    let red_n = objs.len() - blue_n;
    info!("Frontline: {} objectives ({} blue / {} red)", objs.len(), blue_n, red_n);
    if objs.len() < 4 || blue_n == 0 || red_n == 0 {
        return Frontlines::default();
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

    let cell = dx.max(dy);
    let epsilon = (cell * 0.4).clamp(500.0, 2_500.0);
    // Non-contested vertices to bridge across within one run (~σ worth).
    let max_bridge = ((sigma * 0.9) / cell).ceil().max(2.0) as usize;
    let stitch_gap = (sigma * 1.6).max(cell * 4.0);

    // "Contested" = a blue AND a red objective within contested_mult·σ.
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

    let sidx = |a: usize, b: usize| if a < b { (a, b) } else { (b, a) };

    // Trace the F = `level` contour, keep its contested stretches, chain,
    // stitch, and smooth. Returns a handful of polylines.
    let trace = |level: f64| -> Vec<Vec<Vector2>> {
        let hpos = |i: usize, j: usize| {
            let (a, b) = (f[i][j] - level, f[i][j + 1] - level);
            let t = if (a - b).abs() < 1e-12 { 0.5 } else { a / (a - b) };
            Vector2::new(mn.x + i as f64 * dx, mn.y + (j as f64 + t) * dy)
        };
        let vpos = |i: usize, j: usize| {
            let (a, b) = (f[i][j] - level, f[i + 1][j] - level);
            let t = if (a - b).abs() < 1e-12 { 0.5 } else { a / (a - b) };
            Vector2::new(mn.x + (i as f64 + t) * dx, mn.y + j as f64 * dy)
        };

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
                if f[i][j] > level {
                    c |= 1;
                }
                if f[i][j + 1] > level {
                    c |= 2;
                }
                if f[i + 1][j + 1] > level {
                    c |= 4;
                }
                if f[i + 1][j] > level {
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
        if segs.is_empty() {
            return Vec::new();
        }

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

        // Cut every contour into its maximal contested runs.
        let mut runs: Vec<Vec<Vector2>> = Vec::new();
        for mut chain in raw {
            if chain.len() < 3 {
                continue;
            }
            let closed = (chain[0] - chain[chain.len() - 1]).norm() < cell * 2.0;
            if closed {
                chain.pop();
                let cflags: Vec<bool> = chain.iter().map(|&q| contested_at(q)).collect();
                if let Some(rot) = cflags.iter().position(|&c| !c) {
                    chain.rotate_left(rot);
                }
            }
            let flags: Vec<bool> = chain.iter().map(|&q| contested_at(q)).collect();
            if !flags.iter().any(|&c| c) {
                continue;
            }
            let mut i = 0;
            while i < chain.len() {
                if !flags[i] {
                    i += 1;
                    continue;
                }
                let start = i;
                let mut end = i;
                let mut j = i + 1;
                let mut g = 0usize;
                while j < chain.len() {
                    if flags[j] {
                        end = j;
                        g = 0;
                    } else {
                        g += 1;
                        if g > max_bridge {
                            break;
                        }
                    }
                    j += 1;
                }
                runs.push(chain[start..=end].to_vec());
                i = end + 1;
            }
        }

        // Stitch runs whose endpoints nearly touch.
        loop {
            let mut best: Option<(usize, bool, usize, bool, f64)> = None;
            for a in 0..runs.len() {
                for b in (a + 1)..runs.len() {
                    let ea = [(false, runs[a][0]), (true, *runs[a].last().unwrap())];
                    let eb = [(false, runs[b][0]), (true, *runs[b].last().unwrap())];
                    for (at, pa) in ea {
                        for (bt, pb) in eb {
                            let d = (pa - pb).norm();
                            if d < stitch_gap && best.map_or(true, |x| d < x.4) {
                                best = Some((a, at, b, bt, d));
                            }
                        }
                    }
                }
            }
            let Some((a, at, b, bt, _)) = best else { break };
            let mut ca = std::mem::take(&mut runs[a]);
            let mut cb = std::mem::take(&mut runs[b]);
            if !at {
                ca.reverse();
            }
            if bt {
                cb.reverse();
            }
            ca.extend(cb);
            runs[a] = ca;
            runs.remove(b);
        }

        // Length-filter + smooth.
        let mut out: Vec<Vec<Vector2>> = Vec::new();
        for run in &runs {
            let len: f64 = run.windows(2).map(|w| (w[1] - w[0]).norm()).sum();
            if len < p.min_front_len {
                continue;
            }
            let mut simp = Vec::new();
            rdp(run, epsilon, &mut simp);
            simp.dedup_by(|a, b| (*a - *b).norm() < 1.0);
            if simp.len() >= 2 {
                out.push(chaikin(&simp, p.chaikin_iters));
            }
        }
        out
    };

    let to_ll = |lines: Vec<Vec<Vector2>>| -> Vec<Vec<[f64; 2]>> {
        lines
            .iter()
            .map(|l| l.iter().map(|&v| [v.x, v.y]).collect())
            .collect()
    };
    let lvl = p.edge_level.abs().max(0.01);
    let mid = to_ll(trace(0.0));
    let blue = to_ll(trace(lvl));
    let red = to_ll(trace(-lvl));
    info!(
        "Frontline: σ {:.0} km, {}×{} grid, level ±{:.2} -> {} white / {} blue / {} red line(s)",
        sigma / 1000.0,
        res,
        res,
        lvl,
        mid.len(),
        blue.len(),
        red.len()
    );
    Frontlines { mid, blue, red }
}
