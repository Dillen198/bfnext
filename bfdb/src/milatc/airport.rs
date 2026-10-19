// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! The ground a controller works: runways, the taxi network and hold-short
//! points, ramps, and the airfield's frequencies and procedures.
//!
//! Loaded from an airfield file (`bfdb/data/atc/*.json`, built from
//! OpenStreetMap by `scripts/atc_field_from_osm.py` and then hand-tuned). DCS
//! does not expose its taxi network, so the file supplies it; [`Airport::align`]
//! then shifts it onto the DCS runways, which are the ground truth for where an
//! aircraft actually is.
//!
//! All geometry is in flat-earth metres about the file's first runway
//! threshold: `n` north, `e` east. Over an airfield that is exact enough.

use super::Pos;
use crate::voice::FreqSpec;
use serde::Deserialize;
use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
};

/// Andersen AFB, Guam. Shipped in the binary so a fresh range box needs no
/// extra file to get a working controller.
pub(crate) const BUILTIN_ANDERSEN: &str = include_str!("../../data/atc/andersen.json");

pub(crate) const NM: f64 = 1852.;
pub(crate) const FT: f64 = 0.3048;

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub(crate) struct P2 {
    pub n: f64,
    pub e: f64,
}

impl P2 {
    pub(crate) fn new(n: f64, e: f64) -> Self {
        P2 { n, e }
    }
    pub(crate) fn dist(self, o: P2) -> f64 {
        ((self.n - o.n).powi(2) + (self.e - o.e).powi(2)).sqrt()
    }
    /// True bearing from here to `o`, degrees.
    pub(crate) fn bearing_to(self, o: P2) -> f64 {
        (o.e - self.e).atan2(o.n - self.n).to_degrees().rem_euclid(360.)
    }
    /// The point `d` metres from here on true bearing `brg`.
    pub(crate) fn offset(self, brg: f64, d: f64) -> P2 {
        let r = brg.to_radians();
        P2::new(self.n + d * r.cos(), self.e + d * r.sin())
    }
}

/// Signed smallest difference a - b, in (-180, 180].
pub(crate) fn angdiff(a: f64, b: f64) -> f64 {
    let d = (a - b + 540.).rem_euclid(360.) - 180.;
    if d == -180. {
        180.
    } else {
        d
    }
}

// ─── file format ───────────────────────────────────────────────────────────

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct FileRunway {
    name: String,
    pair: String,
    thr_lat: f64,
    thr_lon: f64,
    hdg_true: f64,
    length_m: f64,
    width_m: f64,
}

#[derive(Debug, Clone, Deserialize)]
struct FileEdge {
    a: usize,
    b: usize,
    #[serde(default)]
    name: String,
    #[serde(default)]
    runway: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct FileHold {
    node: usize,
    runway: String,
    near_end: String,
    from_thr_m: f64,
}

#[derive(Debug, Clone, Deserialize)]
struct FileRamp {
    name: String,
    lat: f64,
    lon: f64,
}

/// Which runway each direction of operation uses. Keyed by the two-digit
/// direction ("06", "24"): with parallel runways, departures and arrivals can
/// be split.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct RunwayUse {
    pub departure: String,
    pub arrival: String,
}

/// What Clearance Delivery and Departure hand out.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct DepartureProc {
    /// "the Andersen range complex": the clearance limit when the pilot names
    /// none.
    #[serde(default = "d_clearance_limit")]
    pub clearance_limit: String,
    /// Initial altitude, feet MSL.
    #[serde(default = "d_initial_alt")]
    pub initial_alt_ft: i32,
    /// "Expect" altitude ten minutes after departure.
    #[serde(default = "d_expect_alt")]
    pub expect_alt_ft: i32,
    /// Tower hands a departure to Departure once it is this high AGL...
    #[serde(default = "d_handoff_agl")]
    pub handoff_agl_ft: i32,
    /// ...and Departure releases it this far out or this high.
    #[serde(default = "d_release_nm")]
    pub release_nm: f64,
    #[serde(default = "d_release_alt")]
    pub release_alt_ft: i32,
    /// First squawk code handed out; each clearance takes the next.
    #[serde(default = "d_squawk")]
    pub squawk_base: u16,
}

/// What Approach, Final and Tower work arrivals with.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ArrivalProc {
    /// Overhead pattern altitude, feet MSL.
    #[serde(default = "d_pattern_alt")]
    pub pattern_alt_ft: i32,
    /// Initial point, nm out on the extended centreline.
    #[serde(default = "d_initial_nm")]
    pub initial_nm: f64,
    /// Altitude Approach descends arrivals to, feet MSL.
    #[serde(default = "d_approach_alt")]
    pub approach_alt_ft: i32,
    /// Where a radar final is joined, nm from the threshold.
    #[serde(default = "d_final_nm")]
    pub final_join_nm: f64,
    /// Precision approach glidepath, degrees.
    #[serde(default = "d_glidepath")]
    pub glidepath_deg: f64,
    /// Touchdown point past the threshold, metres (the PAR's reference).
    #[serde(default = "d_tdz")]
    pub touchdown_m: f64,
    /// Decision height above the threshold, feet.
    #[serde(default = "d_dh")]
    pub decision_height_ft: i32,
    /// Inside this radius an aircraft is Tower's (nm).
    #[serde(default = "d_tower_nm")]
    pub tower_radius_nm: f64,
}

fn d_clearance_limit() -> String {
    "the range complex".into()
}
fn d_initial_alt() -> i32 {
    4000
}
fn d_expect_alt() -> i32 {
    22_000
}
fn d_handoff_agl() -> i32 {
    600
}
fn d_release_nm() -> f64 {
    25.
}
fn d_release_alt() -> i32 {
    12_000
}
fn d_squawk() -> u16 {
    4201
}
fn d_pattern_alt() -> i32 {
    2100
}
fn d_initial_nm() -> f64 {
    5.
}
fn d_approach_alt() -> i32 {
    3000
}
fn d_final_nm() -> f64 {
    10.
}
fn d_glidepath() -> f64 {
    3.
}
fn d_tdz() -> f64 {
    300.
}
fn d_dh() -> i32 {
    200
}
fn d_tower_nm() -> f64 {
    8.
}

impl Default for DepartureProc {
    fn default() -> Self {
        serde_json::from_str("{}").unwrap()
    }
}
impl Default for ArrivalProc {
    fn default() -> Self {
        serde_json::from_str("{}").unwrap()
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct FieldFile {
    icao: String,
    dcs_name: String,
    callsign: String,
    runways: Vec<FileRunway>,
    nodes: Vec<[f64; 2]>,
    edges: Vec<FileEdge>,
    #[serde(default)]
    holds: Vec<FileHold>,
    #[serde(default)]
    ramps: Vec<FileRamp>,
    #[serde(default)]
    positions: HashMap<Pos, Vec<FreqSpec>>,
    #[serde(default)]
    runway_use: HashMap<String, RunwayUse>,
    #[serde(default)]
    departure: DepartureProc,
    #[serde(default)]
    arrival: ArrivalProc,
}

// ─── the airport ───────────────────────────────────────────────────────────

/// One runway end.
#[derive(Debug, Clone)]
pub(crate) struct RwyEnd {
    /// "06R"
    pub name: String,
    /// "06R/24L"
    pub pair: String,
    pub thr: P2,
    /// Landing direction, degrees true.
    pub hdg: f64,
    pub len: f64,
    pub width: f64,
}

impl RwyEnd {
    /// (along the landing direction from the threshold, right of the
    /// centreline), metres.
    pub(crate) fn frame(&self, p: P2) -> (f64, f64) {
        let r = self.hdg.to_radians();
        let (dn, de) = (p.n - self.thr.n, p.e - self.thr.e);
        (dn * r.cos() + de * r.sin(), -dn * r.sin() + de * r.cos())
    }
    /// The two-digit direction, "06" for "06R".
    pub(crate) fn direction(&self) -> &str {
        direction_of(&self.name)
    }
}

pub(crate) fn direction_of(name: &str) -> &str {
    let n = name.trim_end_matches(|c: char| c.is_alphabetic());
    n
}

#[derive(Debug, Clone)]
struct Edge {
    a: usize,
    b: usize,
    name: String,
    runway: Option<String>,
    len: f64,
}

#[derive(Debug, Clone)]
pub(crate) struct Hold {
    pub node: usize,
    pub pair: String,
    pub near_end: String,
    pub from_thr: f64,
}

#[derive(Debug, Clone)]
pub(crate) struct Ramp {
    pub name: String,
    pub p: P2,
}

/// A planned taxi route.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TaxiRoute {
    /// The named taxiways in order, consecutive repeats collapsed and unnamed
    /// connectors left out.
    pub via: Vec<String>,
    /// The node path.
    pub nodes: Vec<usize>,
    /// The first runway the route crosses, if any: the aircraft holds short of
    /// it and waits for a crossing clearance.
    pub crossing: Option<String>,
    /// The hold-short point the route ends at (a departure), as an index into
    /// [`Airport::holds`].
    pub hold: Option<usize>,
    /// Where the route ends.
    pub end: P2,
    pub length_m: f64,
}

#[derive(Debug, Clone)]
pub(crate) struct Airport {
    pub icao: String,
    pub dcs_name: String,
    pub callsign: String,
    pub runways: Vec<RwyEnd>,
    nodes: Vec<P2>,
    edges: Vec<Edge>,
    adj: Vec<Vec<usize>>,
    pub holds: Vec<Hold>,
    pub ramps: Vec<Ramp>,
    pub positions: HashMap<Pos, Vec<FreqSpec>>,
    pub runway_use: HashMap<String, RunwayUse>,
    pub departure: DepartureProc,
    pub arrival: ArrivalProc,
    lat0: f64,
    lon0: f64,
    k_lat: f64,
    k_lon: f64,
    /// The shift applied to the file's geometry to match DCS, once known.
    pub aligned: Option<P2>,
    pub elev_ft: i32,
}

impl Airport {
    pub(crate) fn from_json(json: &str) -> anyhow::Result<Self> {
        let f: FieldFile = serde_json::from_str(json)?;
        anyhow::ensure!(!f.runways.is_empty(), "airfield file has no runways");
        let lat0 = f.runways[0].thr_lat;
        let lon0 = f.runways[0].thr_lon;
        let k_lat = 111_132.;
        let k_lon = 111_320. * lat0.to_radians().cos();
        let loc = |lat: f64, lon: f64| P2::new((lat - lat0) * k_lat, (lon - lon0) * k_lon);
        let nodes: Vec<P2> = f.nodes.iter().map(|[la, lo]| loc(*la, *lo)).collect();
        let mut edges = vec![];
        let mut adj = vec![vec![]; nodes.len()];
        for e in &f.edges {
            if e.a >= nodes.len() || e.b >= nodes.len() {
                anyhow::bail!("edge {}-{} refers to a missing node", e.a, e.b);
            }
            adj[e.a].push(edges.len());
            adj[e.b].push(edges.len());
            edges.push(Edge {
                a: e.a,
                b: e.b,
                name: e.name.trim().to_uppercase(),
                runway: e.runway.clone(),
                len: nodes[e.a].dist(nodes[e.b]),
            });
        }
        Ok(Airport {
            icao: f.icao,
            dcs_name: f.dcs_name,
            callsign: f.callsign,
            runways: f
                .runways
                .iter()
                .map(|r| RwyEnd {
                    name: r.name.clone(),
                    pair: r.pair.clone(),
                    thr: loc(r.thr_lat, r.thr_lon),
                    hdg: r.hdg_true,
                    len: r.length_m,
                    width: r.width_m,
                })
                .collect(),
            holds: f
                .holds
                .iter()
                .filter(|h| h.node < nodes.len())
                .map(|h| Hold {
                    node: h.node,
                    pair: h.runway.clone(),
                    near_end: h.near_end.clone(),
                    from_thr: h.from_thr_m,
                })
                .collect(),
            ramps: f.ramps.iter().map(|r| Ramp { name: r.name.clone(), p: loc(r.lat, r.lon) }).collect(),
            nodes,
            edges,
            adj,
            positions: f.positions,
            runway_use: f.runway_use,
            departure: f.departure,
            arrival: f.arrival,
            lat0,
            lon0,
            k_lat,
            k_lon,
            aligned: None,
            elev_ft: 0,
        })
    }

    pub(crate) fn local(&self, lat: f64, lon: f64) -> P2 {
        P2::new((lat - self.lat0) * self.k_lat, (lon - self.lon0) * self.k_lon)
    }

    /// Does this airfield answer to a DCS airbase name? "Andersen AFB" matches
    /// "Andersen AFB" and "Andersen".
    pub(crate) fn is_named(&self, dcs: &str) -> bool {
        let a = self.dcs_name.to_lowercase();
        let b = dcs.to_lowercase();
        a == b || a.contains(&b) || b.contains(&a) || b.contains(&self.callsign.to_lowercase())
    }

    /// Shift the file's geometry onto DCS's. `dcs` is each runway end DCS
    /// reports, by designator, with its threshold. Returns the shift applied,
    /// metres, or `None` when no runway could be matched or the two disagree
    /// by more than a few hundred metres (the wrong airfield, or a terrain
    /// that is not built from real data) — the taxi network is then left out
    /// of clearances rather than trusted.
    pub(crate) fn align(&mut self, dcs: &[(String, P2)]) -> Option<f64> {
        let mut sum = P2::default();
        let mut n = 0.;
        for (name, p) in dcs {
            if let Some(r) = self.runways.iter().find(|r| r.name.eq_ignore_ascii_case(name)) {
                sum.n += p.n - r.thr.n;
                sum.e += p.e - r.thr.e;
                n += 1.;
            }
        }
        if n == 0. {
            return None;
        }
        let d = P2::new(sum.n / n, sum.e / n);
        let mag = d.dist(P2::default());
        if mag > 400. {
            return None;
        }
        for p in self.nodes.iter_mut() {
            p.n += d.n;
            p.e += d.e;
        }
        for r in self.ramps.iter_mut() {
            r.p.n += d.n;
            r.p.e += d.e;
        }
        // The runways themselves take DCS's positions exactly.
        for (name, p) in dcs {
            if let Some(r) = self.runways.iter_mut().find(|r| r.name.eq_ignore_ascii_case(name)) {
                r.thr = *p;
            }
        }
        self.aligned = Some(d);
        Some(mag)
    }

    pub(crate) fn runway(&self, name: &str) -> Option<&RwyEnd> {
        self.runways.iter().find(|r| r.name.eq_ignore_ascii_case(name))
    }

    /// The other end of the same strip.
    pub(crate) fn opposite(&self, end: &RwyEnd) -> Option<&RwyEnd> {
        self.runways.iter().find(|r| r.pair == end.pair && r.name != end.name)
    }

    /// Runway ends whose direction is closest to the wind, i.e. the ends in
    /// use. Calm or near-calm keeps `prefer` (the last direction used).
    pub(crate) fn direction_for_wind(&self, wind_from: f64, wind_kts: f64, prefer: Option<&str>) -> String {
        let mut dirs: Vec<(&str, f64)> = vec![];
        for r in &self.runways {
            let head = wind_kts * angdiff(wind_from, r.hdg).to_radians().cos();
            if !dirs.iter().any(|(d, _)| *d == r.direction()) {
                dirs.push((r.direction(), head));
            }
        }
        dirs.sort_by(|a, b| b.1.total_cmp(&a.1));
        if let Some(p) = prefer {
            // Keep the current direction unless the tailwind is real.
            if let Some((_, head)) = dirs.iter().find(|(d, _)| *d == p) {
                if *head > -5. {
                    return p.to_string();
                }
            }
        }
        dirs.first().map(|d| d.0.to_string()).unwrap_or_default()
    }

    /// The runway departures (or arrivals) use in a direction of operation.
    pub(crate) fn runway_for(&self, direction: &str, departure: bool) -> Option<&RwyEnd> {
        if let Some(u) = self.runway_use.get(direction) {
            let want = if departure { &u.departure } else { &u.arrival };
            if let Some(r) = self.runway(want) {
                return Some(r);
            }
        }
        self.runways.iter().find(|r| r.direction() == direction)
    }

    /// Is this point on a runway surface (with a small margin)? Returns the
    /// end whose landing direction the point is nearest to the threshold of.
    pub(crate) fn on_runway(&self, p: P2, margin: f64) -> Option<&RwyEnd> {
        self.runways.iter().find(|r| {
            let (along, cross) = r.frame(p);
            cross.abs() <= r.width / 2. + margin && (-margin..=r.len + margin).contains(&along) && along <= r.len / 2.
        })
    }

    /// The runway strip `p` is on, whichever half.
    pub(crate) fn strip_at(&self, p: P2, margin: f64) -> Option<&str> {
        self.runways
            .iter()
            .find(|r| {
                let (along, cross) = r.frame(p);
                cross.abs() <= r.width / 2. + margin && (-margin..=r.len + margin).contains(&along)
            })
            .map(|r| r.pair.as_str())
    }

    pub(crate) fn node(&self, i: usize) -> P2 {
        self.nodes[i]
    }

    fn nearest_node(&self, p: P2) -> Option<(usize, f64)> {
        self.nodes
            .iter()
            .enumerate()
            .filter(|(i, _)| !self.adj[*i].is_empty())
            .map(|(i, q)| (i, q.dist(p)))
            .min_by(|a, b| a.1.total_cmp(&b.1))
    }

    /// Distance from `p` to the route's centreline, metres.
    pub(crate) fn off_route(&self, route: &TaxiRoute, p: P2) -> f64 {
        let mut best = f64::MAX;
        for w in route.nodes.windows(2) {
            best = best.min(seg_dist(p, self.nodes[w[0]], self.nodes[w[1]]));
        }
        if route.nodes.len() == 1 {
            best = self.nodes[route.nodes[0]].dist(p);
        }
        best
    }

    /// Dijkstra from the node nearest `from` to the nearest of `goals`.
    /// Crossing a runway costs as much as a long detour, and unnamed connectors
    /// a little more than taxiways, so routes read the way a controller would
    /// give them.
    fn shortest(&self, from: P2, goals: &[usize]) -> Option<(Vec<usize>, f64)> {
        let (start, d0) = self.nearest_node(from)?;
        if d0 > 600. {
            return None; // not on this airfield's taxi network
        }
        #[derive(PartialEq)]
        struct St(f64, usize);
        impl Eq for St {}
        impl PartialOrd for St {
            fn partial_cmp(&self, o: &Self) -> Option<Ordering> {
                Some(self.cmp(o))
            }
        }
        impl Ord for St {
            fn cmp(&self, o: &Self) -> Ordering {
                o.0.total_cmp(&self.0)
            }
        }
        let mut dist = vec![f64::INFINITY; self.nodes.len()];
        let mut prev = vec![usize::MAX; self.nodes.len()];
        let mut heap = BinaryHeap::new();
        dist[start] = 0.;
        heap.push(St(0., start));
        while let Some(St(d, u)) = heap.pop() {
            if d > dist[u] {
                continue;
            }
            if goals.contains(&u) {
                let mut path = vec![u];
                let mut x = u;
                while prev[x] != usize::MAX {
                    x = prev[x];
                    path.push(x);
                }
                path.reverse();
                return Some((path, d));
            }
            for &ei in &self.adj[u] {
                let e = &self.edges[ei];
                let v = if e.a == u { e.b } else { e.a };
                let mut w = e.len;
                if e.runway.is_some() {
                    w += 3000.;
                }
                if e.name.is_empty() {
                    w *= 1.15;
                }
                if d + w < dist[v] {
                    dist[v] = d + w;
                    prev[v] = u;
                    heap.push(St(d + w, v));
                }
            }
        }
        None
    }

    fn describe(&self, path: Vec<usize>, hold: Option<usize>) -> TaxiRoute {
        let mut via: Vec<String> = vec![];
        let mut crossing = None;
        let mut length = 0.;
        for w in path.windows(2) {
            let Some(e) = self.adj[w[0]]
                .iter()
                .map(|&i| &self.edges[i])
                .find(|e| (e.a == w[0] && e.b == w[1]) || (e.a == w[1] && e.b == w[0]))
            else {
                continue;
            };
            length += e.len;
            if let Some(r) = &e.runway {
                if crossing.is_none() {
                    crossing = Some(r.clone());
                }
            }
            if !e.name.is_empty() && via.last() != Some(&e.name) {
                via.push(e.name.clone());
            }
        }
        let end = path.last().map(|&n| self.nodes[n]).unwrap_or_default();
        TaxiRoute { via, nodes: path, crossing, hold, end, length_m: length }
    }

    /// A departure's route from where it sits to a hold-short point for
    /// `runway`, preferring a full-length departure (a hold within 700 m of
    /// the threshold) and the holds on the aircraft's side of the runway.
    pub(crate) fn route_to_runway(&self, from: P2, runway: &str) -> Option<TaxiRoute> {
        let rwy = self.runway(runway)?;
        let mut cands: Vec<usize> = self
            .holds
            .iter()
            .enumerate()
            .filter(|(_, h)| h.pair == rwy.pair && h.near_end == rwy.name && h.from_thr < 700.)
            .map(|(i, _)| i)
            .collect();
        if cands.is_empty() {
            cands = self
                .holds
                .iter()
                .enumerate()
                .filter(|(_, h)| h.pair == rwy.pair && h.near_end == rwy.name)
                .map(|(i, _)| i)
                .collect();
        }
        let goals: Vec<usize> = cands.iter().map(|&i| self.holds[i].node).collect();
        let (path, _) = self.shortest(from, &goals)?;
        let last = *path.last()?;
        let hold = cands.into_iter().find(|&i| self.holds[i].node == last);
        let mut r = self.describe(path, hold);
        // Holding short of the departure runway itself is not a crossing.
        if r.crossing.as_deref() == Some(rwy.pair.as_str()) {
            r.crossing = None;
        }
        Some(r)
    }

    /// An arrival's route from where it sits to a ramp.
    pub(crate) fn route_to_ramp(&self, from: P2, ramp: &Ramp) -> Option<TaxiRoute> {
        let (goal, _) = self.nearest_node(ramp.p)?;
        let (path, _) = self.shortest(from, &[goal])?;
        Some(self.describe(path, None))
    }

    /// The ramp an arriving aircraft is sent to: the one named, else the one
    /// nearest where it is.
    pub(crate) fn ramp_for(&self, p: P2, named: Option<&str>) -> Option<&Ramp> {
        if let Some(n) = named.map(|s| s.to_lowercase()).filter(|s| !s.is_empty()) {
            if let Some(r) = self.ramps.iter().find(|r| r.name.to_lowercase().contains(&n) || n.contains(&r.name.to_lowercase())) {
                return Some(r);
            }
        }
        self.ramps.iter().min_by(|a, b| a.p.dist(p).total_cmp(&b.p.dist(p)))
    }

    /// The hold the aircraft is sitting at, if any.
    pub(crate) fn hold_near(&self, p: P2, within: f64) -> Option<usize> {
        self.holds
            .iter()
            .enumerate()
            .map(|(i, h)| (i, self.nodes[h.node].dist(p)))
            .filter(|(_, d)| *d <= within)
            .min_by(|a, b| a.1.total_cmp(&b.1))
            .map(|(i, _)| i)
    }

    /// The hold-short point for `pair` on the route, as a node index: where
    /// an aircraft taxiing the route stops before crossing.
    pub(crate) fn crossing_hold(&self, route: &TaxiRoute, pair: &str) -> Option<usize> {
        route.nodes.iter().find_map(|&n| self.holds.iter().position(|h| h.node == n && h.pair == pair))
    }

    /// The name a controller uses for a strip: "06R/24L" → "06R" if that end
    /// is the one in use, else the pair spoken as the first end.
    pub(crate) fn strip_name<'a>(&self, pair: &'a str, direction: &str) -> &'a str {
        pair.split('/').find(|e| direction_of(e) == direction).unwrap_or_else(|| pair.split('/').next().unwrap_or(pair))
    }
}

fn seg_dist(p: P2, a: P2, b: P2) -> f64 {
    let (dn, de) = (b.n - a.n, b.e - a.e);
    let l2 = dn * dn + de * de;
    if l2 < 1e-6 {
        return p.dist(a);
    }
    let t = (((p.n - a.n) * dn + (p.e - a.e) * de) / l2).clamp(0., 1.);
    p.dist(P2::new(a.n + t * dn, a.e + t * de))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn andersen() -> Airport {
        Airport::from_json(BUILTIN_ANDERSEN).expect("andersen parses")
    }

    #[test]
    fn loads_andersen() {
        let a = andersen();
        assert_eq!(a.runways.len(), 4);
        assert!(a.runway("06R").is_some());
        assert!(!a.holds.is_empty());
        assert!(a.positions.contains_key(&Pos::Tower));
        assert_eq!(a.runway_for("06", true).unwrap().name, "06R");
        let r = a.runway("06R").unwrap();
        // 06R's opposite end is 24L, ~3.7 km away on 246°.
        let o = a.opposite(r).unwrap();
        assert_eq!(o.name, "24L");
        assert!((r.thr.dist(o.thr) - r.len).abs() < 20.);
    }

    #[test]
    fn wind_picks_direction() {
        let a = andersen();
        assert_eq!(a.direction_for_wind(70., 12., None), "06");
        assert_eq!(a.direction_for_wind(250., 12., None), "24");
        // light tailwind keeps the current direction
        assert_eq!(a.direction_for_wind(250., 3., Some("06")), "06");
    }

    #[test]
    fn taxi_from_south_ramp_to_06r_holds_short_on_the_near_side() {
        let a = andersen();
        let ramp = a.ramps.iter().find(|r| r.name == "south ramp").unwrap().p;
        let r = a.route_to_runway(ramp, "06R").expect("a route");
        assert!(r.hold.is_some(), "ends at a hold-short point");
        assert!(r.crossing.is_none(), "no runway crossing from the south side: {r:?}");
        assert!(!r.via.is_empty());
        let h = &a.holds[r.hold.unwrap()];
        assert_eq!(h.near_end, "06R");
        // the hold is on the ramp's (south, i.e. right of 06R) side
        let (_, cross) = a.runway("06R").unwrap().frame(a.node(h.node));
        assert!(cross > 0., "hold on the south side, cross {cross}");
    }

    #[test]
    fn taxi_from_north_ramp_to_06r_crosses_06l() {
        let a = andersen();
        let ramp = a.ramps.iter().find(|r| r.name == "north ramp").unwrap().p;
        let r = a.route_to_runway(ramp, "06R").expect("a route");
        assert_eq!(r.crossing.as_deref(), Some("06L/24R"));
        assert!(a.crossing_hold(&r, "06L/24R").is_some());
    }

    #[test]
    fn alignment_shifts_the_network() {
        let mut a = andersen();
        let before = a.node(0);
        let dcs: Vec<(String, P2)> =
            a.runways.iter().map(|r| (r.name.clone(), P2::new(r.thr.n + 30., r.thr.e - 20.))).collect();
        let mag = a.align(&dcs).unwrap();
        assert!((mag - 36.06).abs() < 0.1);
        let after = a.node(0);
        assert!((after.n - before.n - 30.).abs() < 1e-6);
        // a wildly different "Andersen" is refused
        let mut b = andersen();
        let far: Vec<(String, P2)> =
            b.runways.iter().map(|r| (r.name.clone(), P2::new(r.thr.n + 5000., r.thr.e))).collect();
        assert!(b.align(&far).is_none());
    }

    #[test]
    fn on_runway_finds_the_landing_end() {
        let a = andersen();
        let r = a.runway("06R").unwrap();
        let p = r.thr.offset(r.hdg, 500.);
        assert_eq!(a.on_runway(p, 5.).unwrap().name, "06R");
        let q = r.thr.offset(r.hdg, r.len - 400.);
        assert_eq!(a.on_runway(q, 5.).unwrap().name, "24L");
        assert!(a.on_runway(r.thr.offset(r.hdg + 90., 200.), 5.).is_none());
    }
}
