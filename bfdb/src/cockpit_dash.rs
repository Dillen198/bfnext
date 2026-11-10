//! The cockpit panel's DASH tab: the campaign dashboard, in the cockpit.
//!
//! `GET /api/cockpit/dash` is the whole tab in one small answer: the round,
//! the headline numbers, the objectives that need attention (with their
//! positions, so the panel can give range and bearing from the player's own
//! jet), the latest kills and captures, who is flying what, the weather and
//! where the map is. Everything in it is what the public web dashboard
//! already shows (`/api/objectives`, `/api/frontline`, `/api/stats`,
//! `/api/kills`, `/api/capture-events`, `/api/online`); it is put together
//! here because the panel runs in DCS's GUI thread, where one request beats
//! six and a decoded table costs frame time.
//!
//! `GET /api/cockpit/dash/map?w=<px>&rev=<rev>` is the theatre map as a PNG:
//! dxgui shows a picture only from a local file, so the server draws it (an
//! SVG rasterised by resvg, like the wiki's diagrams) and the panel downloads
//! it. Each base's ground is shaded by its owner (the web map's Voronoi
//! territory, cut to the coast where `territory.ts` has one), with the
//! engine's front line and the bases on top. `rev` names what the picture
//! shows, so the panel fetches a new one only when a base changes hands or
//! comes under attack. The player's own position is drawn by the panel over
//! the picture (from the bounds in `/api/cockpit/dash`), so one picture
//! serves every player.
//!
//! Who may read it: anyone, for a public campaign (it is the public
//! dashboard). A non-public campaign's dashboard is locked to admins on the
//! web; here the panel's key opens it, as for every cockpit route, and the
//! map then asks for the `rev` that only the dashboard answer gives.

use crate::*;
use resvg::{tiny_skia, usvg};
use std::fmt::Write as _;

/// How long one put-together answer serves every panel of an instance.
const DASH_TTL: std::time::Duration = std::time::Duration::from_secs(10);

/// Map widths the panel may ask for; bflib's panel rounds up to one of
/// these (`MAP_WIDTHS` in bfcockpit.lua), so resizing the panel a little
/// reuses the picture it has.
pub(crate) const MAP_WIDTHS: [u32; 8] = [320, 400, 480, 560, 640, 800, 960, 1280];

/// How tall the map may be for its width, so a long thin theatre still
/// reads in a panel: the bounds are widened to fit.
const MIN_ASPECT: f64 = 0.6;
const MAX_ASPECT: f64 = 1.1;

const KILLS_SHOWN: usize = 8;
const CAPTURES_SHOWN: usize = 5;
const FRONT_SHOWN: usize = 12;
/// A capture this recent still makes its base worth a look (lib/objectives.ts).
const RECENT_CAPTURE_SECS: i64 = 6 * 3600;

// ── The map ──────────────────────────────────────────────────────────

/// One objective as the map draws it.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct MapObj {
    pub(crate) name: std::string::String,
    pub(crate) kind: std::string::String,
    /// "Blue", "Red" or "Neutral", as `/api/objectives` spells it.
    pub(crate) owner: std::string::String,
    pub(crate) lat: f64,
    pub(crate) lon: f64,
    pub(crate) threatened: bool,
    pub(crate) captureable: bool,
}

/// What the map covers, in degrees.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Bounds {
    pub(crate) north: f64,
    pub(crate) south: f64,
    pub(crate) west: f64,
    pub(crate) east: f64,
}

impl Bounds {
    fn coslat(&self) -> f64 {
        ((self.north + self.south) / 2.).to_radians().cos().max(0.2)
    }

    /// Height over width of the picture: the panel sizes the map row with
    /// it before the picture is there.
    pub(crate) fn aspect(&self) -> f64 {
        (self.north - self.south) / ((self.east - self.west) * self.coslat())
    }

    /// Pixel position of a lat/lon on a `w` x `h` picture. Equirectangular,
    /// scaled by the cosine of the middle latitude, as the panel does it.
    pub(crate) fn xy(&self, lat: f64, lon: f64, w: f64, h: f64) -> (f64, f64) {
        (
            (lon - self.west) / (self.east - self.west) * w,
            (self.north - lat) / (self.north - self.south) * h,
        )
    }
}

/// The picture's height for width `w`. bfcockpit.lua works it out the same
/// way (`math.floor(w * aspect + 0.5)`), so its row is the picture's size.
pub(crate) fn map_height(b: &Bounds, w: u32) -> u32 {
    ((w as f64) * b.aspect() + 0.5).floor().max(1.) as u32
}

/// Everything the map shows.
#[derive(Clone, Debug)]
pub(crate) struct MapData {
    pub(crate) objs: Vec<MapObj>,
    /// The engine's front line (`/api/frontline`): middle, and each side's edge.
    pub(crate) mid: Vec<Vec<[f64; 2]>>,
    pub(crate) blue: Vec<Vec<[f64; 2]>>,
    pub(crate) red: Vec<Vec<[f64; 2]>>,
    pub(crate) bounds: Bounds,
    /// Names what is drawn; changes when any of it does.
    pub(crate) rev: std::string::String,
}

fn str_of<'a>(v: &'a serde_json::Value, k: &str) -> &'a str {
    v.get(k).and_then(|x| x.as_str()).unwrap_or("")
}

fn f64_of(v: &serde_json::Value, k: &str) -> f64 {
    v.get(k).and_then(|x| x.as_f64()).unwrap_or(0.)
}

fn bool_of(v: &serde_json::Value, k: &str) -> bool {
    v.get(k).and_then(|x| x.as_bool()).unwrap_or(false)
}

fn is_carrier(kind: &str) -> bool {
    kind.to_ascii_lowercase().contains("carrier")
}

/// The objectives a map can place: not carriers (their position is
/// withheld as 0,0), and not anything else at 0,0.
pub(crate) fn map_objs(objectives: &[serde_json::Value]) -> Vec<MapObj> {
    objectives
        .iter()
        .filter_map(|o| {
            let (lat, lon) = (f64_of(o, "lat"), f64_of(o, "lon"));
            let kind = str_of(o, "kind");
            if is_carrier(kind)
                || (lat == 0. && lon == 0.)
                || !lat.is_finite()
                || !lon.is_finite()
            {
                return None;
            }
            Some(MapObj {
                name: str_of(o, "name").to_string(),
                kind: kind.to_string(),
                owner: str_of(o, "owner").to_string(),
                lat,
                lon,
                threatened: bool_of(o, "threatened"),
                captureable: bool_of(o, "captureable"),
            })
        })
        .collect()
}

/// The area around the bases, padded, and widened to a shape a panel shows.
pub(crate) fn bounds_of(objs: &[MapObj]) -> Option<Bounds> {
    let first = objs.first()?;
    let (mut s, mut n, mut w, mut e) = (first.lat, first.lat, first.lon, first.lon);
    for o in objs {
        s = s.min(o.lat);
        n = n.max(o.lat);
        w = w.min(o.lon);
        e = e.max(o.lon);
    }
    let pad_lat = ((n - s) * 0.1).max(0.2);
    let pad_lon = ((e - w) * 0.1).max(0.2);
    let mut b = Bounds {
        north: n + pad_lat,
        south: s - pad_lat,
        west: w - pad_lon,
        east: e + pad_lon,
    };
    let a = b.aspect();
    if a < MIN_ASPECT {
        let width = (b.east - b.west) * b.coslat();
        let grow = (MIN_ASPECT * width - (b.north - b.south)) / 2.;
        b.north += grow;
        b.south -= grow;
    } else if a > MAX_ASPECT {
        let k = b.coslat();
        let width = (b.east - b.west) * k;
        let grow = ((b.north - b.south) / MAX_ASPECT - width) / k / 2.;
        b.west -= grow;
        b.east += grow;
    }
    Some(b)
}

fn lines_of(v: &serde_json::Value, k: &str) -> Vec<Vec<[f64; 2]>> {
    v.get(k)
        .and_then(|x| serde_json::from_value::<Vec<Vec<[f64; 2]>>>(x.clone()).ok())
        .unwrap_or_default()
}

/// The map's data from `/api/objectives`' entries and `/api/frontline`'s
/// answer. None when there is nothing to place.
pub(crate) fn map_data(
    objectives: &[serde_json::Value],
    front: &serde_json::Value,
) -> Option<MapData> {
    let objs = map_objs(objectives);
    let bounds = bounds_of(&objs)?;
    let (mid, blue, red) =
        (lines_of(front, "mid"), lines_of(front, "blue"), lines_of(front, "red"));
    let mut key = format!(
        "v1|{:.4}|{:.4}|{:.4}|{:.4}",
        bounds.north, bounds.south, bounds.west, bounds.east
    );
    for o in &objs {
        let _ = write!(
            key,
            "|{}:{}:{}:{}:{}:{:.4}:{:.4}",
            o.name,
            o.kind,
            o.owner,
            o.threatened as u8,
            o.captureable as u8,
            o.lat,
            o.lon
        );
    }
    for (tag, lines) in [("m", &mid), ("b", &blue), ("r", &red)] {
        for l in lines.iter() {
            key.push('|');
            key.push_str(tag);
            for p in l {
                let _ = write!(key, ";{:.3},{:.3}", p[0], p[1]);
            }
        }
    }
    let rev = websec::sha256(key.as_bytes())[..6]
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect::<std::string::String>();
    Some(MapData { objs, mid, blue, red, bounds, rev })
}

/// The Caucasus' land from bfweb/src/lib/territory.ts (`CAUCASUS_LAND`): the
/// Black Sea, the Sea of Azov and the Caspian traced by hand, as [lat, lon].
/// Keep the two copies the same.
#[rustfmt::skip]
const CAUCASUS_LAND: &[[f64; 2]] = &[
    [48.5, 31.5], [46.62, 31.5],
    [46.5, 32.0], [46.25, 32.5], [46.12, 32.9], [46.07, 33.4], [46.1, 33.62], [45.9, 33.5], [45.85, 33.0],
    [45.6, 32.75], [45.4, 32.5], [45.19, 33.37], [44.95, 33.6], [44.6, 33.45], [44.4, 33.73], [44.49, 34.17],
    [44.67, 34.41], [44.84, 34.97], [45.03, 35.4], [45.0, 35.8], [45.08, 36.2], [45.2, 36.45], [45.35, 36.47],
    [45.44, 36.3], [45.47, 35.85], [45.36, 35.45], [45.6, 35.2], [45.95, 34.97], [46.17, 34.82], [46.35, 35.35],
    [46.73, 36.34], [46.75, 36.8], [46.95, 37.3], [47.08, 37.6], [47.1, 38.2], [47.2, 38.9], [47.2, 39.25],
    [47.05, 39.1], [46.9, 38.6], [46.7, 38.27], [46.5, 38.5], [46.05, 38.17],
    [45.75, 37.72], [45.6, 37.66], [45.4, 37.55], [45.29, 37.4], [45.31, 37.2], [45.37, 37.02], [45.42, 36.86],
    [45.37, 36.67], [45.42, 36.5],
    [45.22, 36.5], [45.18, 36.68], [45.1, 36.85], [45.04, 37.1], [44.96, 37.25], [44.89, 37.31], [44.8, 37.41],
    [44.71, 37.57], [44.66, 37.74], [44.72, 37.79], [44.66, 37.92], [44.56, 38.06], [44.46, 38.3], [44.37, 38.53],
    [44.31, 38.71], [44.24, 38.86], [44.1, 39.07], [43.98, 39.22], [43.9, 39.34], [43.76, 39.52], [43.66, 39.65],
    [43.58, 39.72], [43.49, 39.85], [43.42, 39.95], [43.38, 40.01], [43.31, 40.18], [43.26, 40.27], [43.18, 40.3],
    [43.15, 40.37], [43.12, 40.55], [43.09, 40.76], [43.03, 40.94], [42.99, 41.03], [42.93, 41.13], [42.83, 41.3],
    [42.71, 41.46], [42.55, 41.53], [42.39, 41.56], [42.25, 41.62], [42.15, 41.66], [42.03, 41.73], [41.9, 41.77],
    [41.82, 41.77], [41.72, 41.72], [41.66, 41.64], [41.62, 41.62], [41.56, 41.57], [41.52, 41.55], [41.4, 41.42],
    [41.35, 41.3], [41.25, 41.1], [41.18, 40.9], [41.08, 40.66], [41.03, 40.52], [40.98, 40.2], [41.0, 39.72],
    [40.96, 39.2], [40.92, 38.6], [40.96, 38.0], [41.08, 37.4], [41.2, 36.95], [41.3, 36.5],
    [41.45, 36.15], [41.74, 35.95], [42.03, 35.15], [42.1, 34.93], [41.98, 34.3], [41.98, 33.76], [41.9, 33.0],
    [41.75, 32.39], [41.6, 32.05], [41.45, 31.79], [41.33, 31.5],
    [37.5, 31.5], [37.5, 48.9], [37.8, 48.98], [38.43, 48.88], [38.75, 48.85], [39.15, 48.95], [39.33, 49.4],
    [39.95, 49.42], [40.36, 49.86], [40.55, 49.97], [40.59, 49.67], [41.08, 49.12], [41.6, 48.7], [42.06, 48.3],
    [42.56, 47.87], [42.98, 47.5], [43.27, 47.55], [43.65, 47.55], [43.9, 47.4], [44.3, 47.0], [44.8, 46.95],
    [45.39, 47.37], [45.8, 47.6], [46.0, 48.3], [46.25, 49.0], [46.6, 49.6], [46.75, 50.0],
    [48.5, 50.0],
];

/// The coast for a theatre, when we have one: chosen, as on the web, by the
/// middle of the bases.
fn coast_for(objs: &[MapObj]) -> Option<&'static [[f64; 2]]> {
    if objs.is_empty() {
        return None;
    }
    let n = objs.len() as f64;
    let lat0 = objs.iter().map(|o| o.lat).sum::<f64>() / n;
    let lon0 = objs.iter().map(|o| o.lon).sum::<f64>() / n;
    ((40.0..=47.0).contains(&lat0) && (36.5..=48.0).contains(&lon0))
        .then_some(CAUCASUS_LAND)
}

type Xy = [f64; 2];

/// Clip a convex polygon to the half-plane (p - m) . n <= 0.
fn clip(poly: &[Xy], m: Xy, n: Xy) -> Vec<Xy> {
    let side = |p: Xy| (p[0] - m[0]) * n[0] + (p[1] - m[1]) * n[1];
    let mut out = Vec::with_capacity(poly.len() + 1);
    for i in 0..poly.len() {
        let a = poly[i];
        let b = poly[(i + 1) % poly.len()];
        let (sa, sb) = (side(a), side(b));
        if sa <= 0. {
            out.push(a);
        }
        if (sa <= 0.) != (sb <= 0.) {
            let t = sa / (sa - sb);
            out.push([a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]);
        }
    }
    out
}

/// Each base's ground in picture space: the box cut by the bisector with
/// every other base (lib/territory.ts' cells). Pixels are equal in x and y,
/// so nearest in pixels is nearest on the ground.
pub(crate) fn voronoi(sites: &[Xy], bx: [Xy; 2]) -> Vec<Vec<Xy>> {
    let boxp = vec![bx[0], [bx[1][0], bx[0][1]], bx[1], [bx[0][0], bx[1][1]]];
    sites
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let mut poly = boxp.clone();
            for (j, t) in sites.iter().enumerate() {
                if j == i || poly.len() < 3 {
                    continue;
                }
                let n = [t[0] - s[0], t[1] - s[1]];
                if n[0] == 0. && n[1] == 0. {
                    continue;
                }
                poly = clip(&poly, [(s[0] + t[0]) / 2., (s[1] + t[1]) / 2.], n);
            }
            poly
        })
        .collect()
}

fn esc(s: &str) -> std::string::String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn path_d(pts: impl IntoIterator<Item = Xy>, close: bool) -> std::string::String {
    let mut d = std::string::String::new();
    for (i, p) in pts.into_iter().enumerate() {
        let _ = write!(d, "{}{:.1},{:.1}", if i == 0 { "M" } else { "L" }, p[0], p[1]);
    }
    if close && !d.is_empty() {
        d.push('Z');
    }
    d
}

const SEA: &str = "#0b1519";
const LAND: &str = "#161c15";
const GRID: &str = "#c8d0b8";
const INK: &str = "#d9dfcb";
const BLUE: &str = "#4c8fe0";
const RED: &str = "#e0574a";
const NEUTRAL: &str = "#9a9f92";

fn side_colour(owner: &str) -> &'static str {
    match owner {
        "Blue" => BLUE,
        "Red" => RED,
        _ => NEUTRAL,
    }
}

/// Degrees between grid lines, for a span of `deg`: four to eight lines.
fn grid_step(deg: f64) -> f64 {
    for s in [0.25, 0.5, 1., 2., 5.] {
        if deg / s <= 8. {
            return s;
        }
    }
    10.
}

/// The map as an SVG `w` pixels wide.
pub(crate) fn map_svg(m: &MapData, w: u32) -> std::string::String {
    let b = &m.bounds;
    let h = map_height(b, w);
    let (wf, hf) = (w as f64, h as f64);
    let xy = |lat: f64, lon: f64| -> Xy {
        let (x, y) = b.xy(lat, lon, wf, hf);
        [x, y]
    };
    let unit = (wf / 480.).clamp(0.7, 2.2);
    let mut s = std::string::String::with_capacity(32 * 1024);
    let _ = write!(
        s,
        r#"<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="0 0 {w} {h}" font-family="Arial">"#
    );
    let coast =
        coast_for(&m.objs).map(|ring| path_d(ring.iter().map(|p| xy(p[0], p[1])), true));
    match &coast {
        Some(d) => {
            let _ = write!(
                s,
                r#"<defs><clipPath id="land"><path d="{d}"/></clipPath></defs><rect width="{w}" height="{h}" fill="{SEA}"/><path d="{d}" fill="{LAND}"/>"#
            );
        }
        None => {
            let _ = write!(s, r#"<rect width="{w}" height="{h}" fill="{LAND}"/>"#);
        }
    }

    // Each side's ground.
    let sites: Vec<Xy> = m.objs.iter().map(|o| xy(o.lat, o.lon)).collect();
    let cells = voronoi(&sites, [[-wf, -hf], [2. * wf, 2. * hf]]);
    let _ = write!(
        s,
        r#"<g opacity="0.24"{}>"#,
        if coast.is_some() { r#" clip-path="url(#land)""# } else { "" }
    );
    for (o, cell) in m.objs.iter().zip(&cells) {
        if cell.len() < 3 || !(o.owner == "Blue" || o.owner == "Red") {
            continue;
        }
        let _ = write!(
            s,
            r#"<path d="{}" fill="{}"/>"#,
            path_d(cell.iter().copied(), true),
            side_colour(&o.owner)
        );
    }
    s.push_str("</g>");

    // Graticule, labelled along the top and the left.
    let step = grid_step((b.east - b.west).max(b.north - b.south));
    let fs = (9. * unit).round();
    let _ = write!(s, r#"<g stroke="{GRID}" stroke-opacity="0.12" stroke-width="1">"#);
    let mut lon = (b.west / step).ceil() * step;
    let mut labels = std::string::String::new();
    while lon < b.east {
        let x = xy(b.north, lon)[0];
        let _ = write!(s, r#"<line x1="{x:.1}" y1="0" x2="{x:.1}" y2="{hf:.1}"/>"#);
        let _ = write!(
            labels,
            r#"<text x="{:.1}" y="{:.1}" font-size="{fs}">{}°{}</text>"#,
            x + 3.,
            fs + 2.,
            fmt_deg(lon),
            if lon >= 0. { "E" } else { "W" }
        );
        lon += step;
    }
    let mut lat = (b.south / step).ceil() * step;
    while lat < b.north {
        let y = xy(lat, b.west)[1];
        let _ = write!(s, r#"<line x1="0" y1="{y:.1}" x2="{wf:.1}" y2="{y:.1}"/>"#);
        let _ = write!(
            labels,
            r#"<text x="3" y="{:.1}" font-size="{fs}">{}°{}</text>"#,
            y - 3.,
            fmt_deg(lat),
            if lat >= 0. { "N" } else { "S" }
        );
        lat += step;
    }
    let _ = write!(s, r#"</g><g fill="{GRID}" fill-opacity="0.4">{labels}</g>"#);
    if let Some(d) = &coast {
        let _ = write!(
            s,
            r#"<path d="{d}" fill="none" stroke="{GRID}" stroke-opacity="0.3" stroke-width="{:.1}"/>"#,
            unit
        );
    }

    // The front: each side's edge thin, the middle bold.
    for (lines, colour, width, dash) in [
        (&m.blue, BLUE, 1.2, true),
        (&m.red, RED, 1.2, true),
        (&m.mid, "#f2ecd2", 2.2, false),
    ] {
        for l in lines.iter() {
            if l.len() < 2 {
                continue;
            }
            let _ = write!(
                s,
                r#"<path d="{}" fill="none" stroke="{colour}" stroke-width="{:.1}" stroke-linejoin="round" stroke-linecap="round"{}/>"#,
                path_d(l.iter().map(|p| xy(p[0], p[1])), false),
                width * unit,
                if dash {
                    format!(
                        r#" stroke-dasharray="{:.1} {:.1}" stroke-opacity="0.8""#,
                        4. * unit,
                        3. * unit
                    )
                } else {
                    std::string::String::new()
                }
            );
        }
    }

    // The bases: a shape per kind in the owner's colour, a ring for one
    // under attack (orange) or ready to capture (yellow, dashed).
    let r = (4.5 * unit).max(3.5);
    let label_all = w >= 800;
    // (priority, x, y, name) of each base that wants a label
    let mut wanted: Vec<(u8, f64, f64, &str)> = Vec::new();
    for o in &m.objs {
        let [x, y] = xy(o.lat, o.lon);
        if o.captureable {
            let _ = write!(
                s,
                r##"<circle cx="{x:.1}" cy="{y:.1}" r="{:.1}" fill="none" stroke="#ffd84a" stroke-width="{:.1}" stroke-dasharray="{:.1} {:.1}"/>"##,
                r + 6. * unit,
                1.6 * unit,
                3. * unit,
                2. * unit
            );
        }
        if o.threatened {
            let _ = write!(
                s,
                r##"<circle cx="{x:.1}" cy="{y:.1}" r="{:.1}" fill="none" stroke="#ff9f1a" stroke-width="{:.1}"/>"##,
                r + 3. * unit,
                1.6 * unit
            );
        }
        let fill = side_colour(&o.owner);
        let shape = match o.kind.as_str() {
            "Airbase" | "Naval Base" => format!(
                r#"<rect x="{:.1}" y="{:.1}" width="{:.1}" height="{:.1}"/>"#,
                x - r,
                y - r,
                2. * r,
                2. * r
            ),
            "FOB" => path_shape(&[
                [x, y - r * 1.2],
                [x + r * 1.1, y + r * 0.9],
                [x - r * 1.1, y + r * 0.9],
            ]),
            "Logistics Hub" | "Command Center" => path_shape(&[
                [x, y - r * 1.25],
                [x + r * 1.25, y],
                [x, y + r * 1.25],
                [x - r * 1.25, y],
            ]),
            _ => format!(r#"<circle cx="{x:.1}" cy="{y:.1}" r="{:.1}"/>"#, r * 0.85),
        };
        let _ = write!(
            s,
            r##"<g fill="{fill}" stroke="#0b0f0c" stroke-width="{:.1}">{shape}</g>"##,
            unit
        );
        let priority = match o.kind.as_str() {
            "Airbase" | "Naval Base" => 0,
            "Logistics Hub" | "Command Center" => 1,
            "FOB" => 2,
            _ => 3,
        };
        if label_all || priority < 3 {
            wanted.push((priority, x, y, o.name.as_str()));
        }
    }
    let names = place_labels(&mut wanted, &sites, r, unit, wf, hf);
    // Names last, over every marker, with a dark halo to read on any ground.
    let _ = write!(
        s,
        r##"<g fill="{INK}" stroke="#0b0f0c" stroke-width="{:.1}" stroke-opacity="0.85" paint-order="stroke" stroke-linejoin="round">{names}</g>"##,
        2.5 * unit
    );
    s.push_str("</svg>");
    s
}

/// The bases' names, each right or left of its marker where it covers no
/// other name or marker and stays on the picture; airbases first, and a name
/// with no room is left off rather than drawn over another.
fn place_labels(
    wanted: &mut [(u8, f64, f64, &str)],
    markers: &[Xy],
    r: f64,
    unit: f64,
    w: f64,
    h: f64,
) -> std::string::String {
    type Rect = [f64; 4]; // x0, y0, x1, y1
    let overlaps =
        |a: &Rect, b: &Rect| a[0] < b[2] && b[0] < a[2] && a[1] < b[3] && b[1] < a[3];
    let lfs = (10. * unit).round();
    let gap = r + 3. * unit;
    let mut taken: Vec<Rect> = Vec::new();
    let mut out = std::string::String::new();
    wanted.sort_by_key(|l| l.0);
    for &(_, x, y, name) in wanted.iter() {
        // a guess at Arial's width, a little generous for the halo
        let tw = name.chars().count() as f64 * lfs * 0.56 + 2. * unit;
        let (y0, y1) = (y - lfs * 0.6, y + lfs * 0.6);
        let free = |rect: &Rect| {
            rect[0] >= 0.
                && rect[2] <= w
                && rect[1] >= 0.
                && rect[3] <= h
                && !taken.iter().any(|t| overlaps(t, rect))
                && !markers.iter().any(|m| {
                    let mb = [m[0] - r, m[1] - r, m[0] + r, m[1] + r];
                    !(m[0] == x && m[1] == y) && overlaps(&mb, rect)
                })
        };
        // right, left, above, below: (box, anchor, text x, text y offset)
        let lift = r + lfs * 0.75;
        let tries = [
            ([x + gap, y0, x + gap + tw, y1], "start", x + gap, 0.),
            ([x - gap - tw, y0, x - gap, y1], "end", x - gap, 0.),
            ([x - tw / 2., y0 - lift, x + tw / 2., y1 - lift], "middle", x, -lift),
            ([x - tw / 2., y0 + lift, x + tw / 2., y1 + lift], "middle", x, lift),
        ];
        let Some((rect, anchor, tx, dy)) = tries.into_iter().find(|t| free(&t.0)) else {
            continue;
        };
        taken.push(rect);
        let _ = write!(
            out,
            r#"<text x="{tx:.1}" y="{:.1}" font-size="{lfs}" text-anchor="{anchor}">{}</text>"#,
            y + dy + lfs * 0.35,
            esc(name)
        );
    }
    out
}

fn path_shape(pts: &[Xy]) -> std::string::String {
    format!(r#"<path d="{}"/>"#, path_d(pts.iter().copied(), true))
}

fn fmt_deg(v: f64) -> std::string::String {
    let a = v.abs();
    if (a - a.round()).abs() < 1e-9 {
        format!("{}", a.round() as i64)
    } else {
        let t = format!("{a:.2}");
        t.trim_end_matches('0').to_string()
    }
}

/// The map as a PNG `w` pixels wide.
pub(crate) fn map_png(m: &MapData, w: u32) -> Result<Vec<u8>> {
    let svg = map_svg(m, w);
    let mut opt = usvg::Options::default();
    opt.fontdb = crate::range::cards::fontdb();
    opt.font_family = "Arial".into();
    let tree = usvg::Tree::from_str(&svg, &opt)
        .map_err(|e| anyhow::anyhow!("dash map svg: {e}"))?;
    let mut pm = tiny_skia::Pixmap::new(w, map_height(&m.bounds, w))
        .ok_or_else(|| anyhow::anyhow!("the map has no size"))?;
    resvg::render(&tree, tiny_skia::Transform::default(), &mut pm.as_mut());
    pm.encode_png().map_err(|e| anyhow::anyhow!("writing the map PNG: {e}"))
}

/// The smallest of [`MAP_WIDTHS`] at least `want` wide, or the largest.
pub(crate) fn snap_width(want: u32) -> u32 {
    MAP_WIDTHS
        .iter()
        .copied()
        .find(|&s| s >= want)
        .unwrap_or(MAP_WIDTHS[MAP_WIDTHS.len() - 1])
}

// ── The tab's data ───────────────────────────────────────────────────

/// One line of the kill feed, names already looked up.
#[derive(Clone, Debug)]
pub(crate) struct KillLine {
    pub(crate) time: chrono::DateTime<chrono::Utc>,
    pub(crate) killer: Option<std::string::String>,
    pub(crate) killer_side: std::string::String,
    pub(crate) airframe: Option<std::string::String>,
    pub(crate) weapon: Option<std::string::String>,
    pub(crate) victim: std::string::String,
    pub(crate) victim_side: std::string::String,
    pub(crate) air: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct CaptureLine {
    pub(crate) time: chrono::DateTime<chrono::Utc>,
    pub(crate) objective: std::string::String,
    pub(crate) side: std::string::String,
    pub(crate) by: Vec<std::string::String>,
}

/// (name, side, aircraft) of each connected pilot.
pub(crate) type OnlinePilot =
    (std::string::String, std::string::String, Option<std::string::String>);

pub(crate) struct DashInput<'a> {
    pub(crate) now: chrono::DateTime<chrono::Utc>,
    pub(crate) objectives: &'a [serde_json::Value],
    pub(crate) live: bool,
    pub(crate) stats: &'a serde_json::Value,
    pub(crate) kills: &'a [KillLine],
    pub(crate) captures: &'a [CaptureLine],
    pub(crate) online: &'a [OnlinePilot],
    pub(crate) map: Option<&'a MapData>,
}

fn secs_since(
    now: chrono::DateTime<chrono::Utc>,
    t: chrono::DateTime<chrono::Utc>,
) -> i64 {
    (now - t).num_seconds().max(0)
}

/// Why a base needs attention, ranked as bfweb/src/lib/objectives.ts ranks
/// it (0 about to fall ... 4 short of stock); None when it doesn't.
pub(crate) fn attention_of(
    o: &serde_json::Value,
    last_capture: Option<&CaptureLine>,
    now: chrono::DateTime<chrono::Utc>,
) -> Option<(u8, Vec<std::string::String>, &'static str)> {
    let mut reasons = Vec::new();
    let mut rank = 9u8;
    let mut tone = "info";
    if bool_of(o, "captureable") {
        reasons.push("Ready to capture".to_string());
        rank = 0;
        tone = "crit";
    }
    if bool_of(o, "threatened") {
        reasons.push("Under attack".to_string());
        rank = rank.min(1);
        tone = "crit";
    }
    let pct = |k: &str| f64_of(o, k).round() as i64;
    if pct("health") < 40 {
        reasons.push(format!("Defences {}%", pct("health")));
        rank = rank.min(2);
        if tone != "crit" {
            tone = "warn";
        }
    }
    if let Some(c) = last_capture {
        let ago = secs_since(now, c.time);
        if ago < RECENT_CAPTURE_SECS {
            reasons.push(format!(
                "Taken by {} {} ago",
                c.side.to_ascii_lowercase(),
                fmt_ago(ago)
            ));
            rank = rank.min(3);
        }
    }
    for (k, label) in [("supply", "Supply"), ("fuel", "Fuel")] {
        if pct(k) < 25 {
            reasons.push(format!("{label} {}%", pct(k)));
            rank = rank.min(4);
            if tone == "info" {
                tone = "warn";
            }
        }
    }
    (!reasons.is_empty()).then_some((rank, reasons, tone))
}

/// "45s", "12m", "3h", "2d".
pub(crate) fn fmt_ago(secs: i64) -> std::string::String {
    match secs {
        s if s < 60 => format!("{s}s"),
        s if s < 3600 => format!("{}m", s / 60),
        s if s < 86_400 => format!("{}h", s / 3600),
        s => format!("{}d", s / 86_400),
    }
}

/// The DASH tab's answer.
pub(crate) fn dash_json(d: &DashInput) -> serde_json::Value {
    use serde_json::json;
    let count =
        |owner: &str| d.objectives.iter().filter(|o| str_of(o, "owner") == owner).count();
    let mut last_cap: std::collections::HashMap<&str, &CaptureLine> =
        std::collections::HashMap::new();
    // newest first, as recent_captures gives them
    for c in d.captures {
        last_cap.entry(c.objective.as_str()).or_insert(c);
    }
    let mut attention: Vec<(u8, f64, serde_json::Value)> = d
        .objectives
        .iter()
        .filter_map(|o| {
            let (rank, reasons, tone) = attention_of(o, last_cap.get(str_of(o, "name")).copied(), d.now)?;
            let hide_pos = is_carrier(str_of(o, "kind"));
            Some((
                rank,
                f64_of(o, "health"),
                json!({
                    "name": str_of(o, "name"),
                    "kind": str_of(o, "kind"),
                    "owner": str_of(o, "owner"),
                    "lat": if hide_pos { serde_json::Value::Null } else { json!(f64_of(o, "lat")) },
                    "lon": if hide_pos { serde_json::Value::Null } else { json!(f64_of(o, "lon")) },
                    "health": f64_of(o, "health").round() as i64,
                    "rank": rank,
                    "tone": tone,
                    "reasons": reasons,
                }),
            ))
        })
        .collect();
    attention.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.total_cmp(&b.1)));
    let at_risk = attention.iter().filter(|a| a.0 <= 2).count();
    let front: Vec<_> = attention.into_iter().take(FRONT_SHOWN).map(|a| a.2).collect();

    let kills_1h = d.kills.iter().filter(|k| secs_since(d.now, k.time) < 3600).count();
    let kills: Vec<_> = d
        .kills
        .iter()
        .take(KILLS_SHOWN)
        .map(|k| {
            json!({
                "ago": fmt_ago(secs_since(d.now, k.time)),
                "killer": k.killer,
                "killer_side": k.killer_side,
                "airframe": k.airframe,
                "weapon": k.weapon,
                "victim": k.victim,
                "victim_side": k.victim_side,
                "air": k.air,
            })
        })
        .collect();
    let captures: Vec<_> = d
        .captures
        .iter()
        .take(CAPTURES_SHOWN)
        .map(|c| {
            json!({
                "ago": fmt_ago(secs_since(d.now, c.time)),
                "objective": c.objective,
                "side": c.side,
                "by": c.by.join(", "),
            })
        })
        .collect();

    // Who is flying what, each side's types most flown first.
    let mut air = serde_json::Map::new();
    let mut flying = 0;
    for side in ["Blue", "Red"] {
        let mut by_type: Vec<(std::string::String, usize)> = Vec::new();
        let mut on = 0;
        for (_, _, aircraft) in d.online.iter().filter(|p| p.1 == side) {
            on += 1;
            if let Some(a) = aircraft.as_deref().filter(|a| !a.is_empty()) {
                flying += 1;
                match by_type.iter_mut().find(|(t, _)| t == a) {
                    Some(e) => e.1 += 1,
                    None => by_type.push((a.to_string(), 1)),
                }
            }
        }
        by_type.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        air.insert(
            side.to_ascii_lowercase(),
            json!({
                "online": on,
                "types": by_type.iter().map(|(t, n)| json!({ "type": t, "n": n })).collect::<Vec<_>>(),
            }),
        );
    }

    let restart_in = d
        .stats
        .get("restart_at")
        .and_then(|v| v.as_str())
        .and_then(|s| chrono::DateTime::parse_from_rfc3339(s).ok())
        .map(|t| (t.with_timezone(&chrono::Utc) - d.now).num_seconds())
        .filter(|s| *s > 0);
    let round = d.stats.get("active_round").filter(|v| v.is_object()).map(|r| {
        json!({
            "number": r.get("number"),
            "scenario": r.get("scenario"),
            "age": r.get("start").and_then(|v| v.as_str())
                .and_then(|s| chrono::DateTime::parse_from_rfc3339(s).ok())
                .map(|t| fmt_ago(secs_since(d.now, t.with_timezone(&chrono::Utc)))),
        })
    });
    json!({
        "round": round,
        "live": d.live,
        "restart_in": restart_in,
        "kpi": {
            "online": d.online.len(),
            "flying": flying,
            "blue_obj": count("Blue"),
            "red_obj": count("Red"),
            "neutral_obj": count("Neutral"),
            "at_risk": at_risk,
            "kills_1h": kills_1h,
            "total_pilots": d.stats.get("total_pilots"),
        },
        "front": front,
        "kills": kills,
        "captures": captures,
        "air": air,
        "weather": d.stats.get("weather").cloned().unwrap_or(serde_json::Value::Null),
        "map": d.map.map(|m| json!({
            "rev": m.rev,
            "north": m.bounds.north,
            "south": m.bounds.south,
            "west": m.bounds.west,
            "east": m.bounds.east,
            "aspect": m.bounds.aspect(),
            "widths": MAP_WIDTHS,
        })),
        "updated": d.now.format("%H:%M:%SZ").to_string(),
    })
}

// ── Routes ───────────────────────────────────────────────────────────

pub(crate) struct Snapshot {
    json: serde_json::Value,
    map: Option<Arc<MapData>>,
}

async fn build(
    db: &StatsDb,
    bot_cfg: &Arc<Option<BotLinkConfig>>,
    inst: &Inst,
) -> std::result::Result<Snapshot, Error> {
    let (objectives, live) = objectives_entries(db, None, inst).await?;
    let front = frontline_value(db, None, inst).await?;
    let stats = stats_value(db, bot_cfg, inst).await?;
    let (kills, captures, online) = task::block_in_place(
        || -> Result<(Vec<KillLine>, Vec<CaptureLine>, Vec<OnlinePilot>)> {
            let rounds = db.latest_rounds_for(&inst.id)?;
            let Some(rid) =
                rounds.iter().find(|(_, _, r)| r.end.is_none()).map(|(_, rid, _)| *rid)
            else {
                return Ok((vec![], vec![], vec![]));
            };
            let name = |u: &dcso3::net::Ucid| db.pilot_name(u);
            let kills = db
                .recent_kills(rid, 300)?
                .iter()
                .map(|dead| {
                    let shot = dead
                        .shots
                        .iter()
                        .filter(|s| s.hit)
                        .max_by_key(|s| s.time)
                        .or_else(|| dead.shots.last());
                    let target = dead.shots.first().map(|s| s.target_typ.to_string());
                    let victim = match (dead.victim.ucid().and_then(name), &target) {
                        (Some(n), Some(t)) => format!("{n} ({t})"),
                        (Some(n), None) => n,
                        (None, Some(t)) => t.clone(),
                        (None, None) => "?".to_string(),
                    };
                    KillLine {
                        time: dead.time,
                        killer: shot.and_then(|s| s.shooter.ucid().and_then(name)),
                        killer_side: shot
                            .map(|s| format!("{:?}", s.shooter.side()))
                            .unwrap_or_default(),
                        airframe: shot
                            .and_then(|s| s.shooter_typ.as_ref().map(|t| t.to_string())),
                        weapon: shot.and_then(|s| display_weapon(s.weapon_name.as_ref())),
                        victim,
                        victim_side: format!("{:?}", dead.victim.side()),
                        air: db.victim_is_air(rid, &dead.victim).unwrap_or(false),
                    }
                })
                .collect();
            let captures = db
                .recent_captures(rid, 20)?
                .into_iter()
                .map(|c| CaptureLine {
                    time: c.time,
                    objective: c.objective_name.clone(),
                    side: format!("{:?}", c.side),
                    by: c
                        .by
                        .iter()
                        .map(|u| name(u).unwrap_or_else(|| u.to_string()))
                        .collect(),
                })
                .collect();
            let online = db
                .connected_pilots(rid)?
                .into_iter()
                .map(|(_, n, side, aircraft)| (n, format!("{side:?}"), aircraft))
                .collect();
            Ok((kills, captures, online))
        },
    )?;
    let map = map_data(&objectives, &front).map(Arc::new);
    let json = dash_json(&DashInput {
        now: chrono::Utc::now(),
        objectives: &objectives,
        live,
        stats: &stats,
        kills: &kills,
        captures: &captures,
        online: &online,
        map: map.as_deref(),
    });
    Ok(Snapshot { json, map })
}

async fn snapshot(
    db: &StatsDb,
    bot_cfg: &Arc<Option<BotLinkConfig>>,
    inst: &Inst,
) -> std::result::Result<Arc<Snapshot>, Error> {
    static DASH: std::sync::LazyLock<websec::CacheMap<InstanceId, Arc<Snapshot>>> =
        std::sync::LazyLock::new(websec::CacheMap::new);
    DASH.entry(&inst.id)
        .get_or_refresh(DASH_TTL, || async {
            build(db, bot_cfg, inst).await.map(Arc::new)
        })
        .await
}

/// Not a campaign this caller may read: a range, a `?server=` that names no
/// campaign when there are several, or a non-public one without the
/// panel's key (or an admin session).
async fn refuse(
    query: &Q,
    session_id: Option<Uuid>,
    db: &StatsDb,
    bot_cfg: &Arc<Option<BotLinkConfig>>,
    inst: &Inst,
    ip: std::net::IpAddr,
    keyless_ok: bool,
) -> std::result::Result<(), Error> {
    note_cockpit_plugin(query);
    if inst.cfg.is_range() {
        return Err(
            websec::bad_request("the training range has no campaign dashboard").into()
        );
    }
    if let Some(name) = query.get("server").filter(|s| !s.trim().is_empty()) {
        if !query.contains_key("instance")
            && db.instances().by_dcs_server_name(name).is_none()
            && db.instances().all().len() > 1
        {
            return Err(websec::bad_request(format!("unknown instance {name:?}")).into());
        }
    }
    if inst.cfg.public || keyless_ok || session_is_admin(db, session_id) {
        return Ok(());
    }
    require_linked_player(query, session_id, db.clone(), bot_cfg, inst, ip)
        .await
        .map(|_| ())
}

/// `GET /api/cockpit/dash`
pub(crate) async fn api_dash(
    session_id: Option<Uuid>,
    query: Q,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    refuse(&query, session_id, &db, &bot_cfg, &inst, ip, false).await?;
    let snap = snapshot(&db, &bot_cfg, &inst).await?;
    Ok(json_response(serde_json::to_string(&snap.json).map_err(anyhow::Error::from)?))
}

/// The last few maps drawn: every panel on an instance asks for the same
/// picture at one of a few widths.
const MAP_CACHE: usize = 24;
type MapKey = (InstanceId, std::string::String, u32);

fn map_cached(inst: &InstanceId, m: &MapData, w: u32) -> Result<Arc<Vec<u8>>> {
    static CACHE: std::sync::LazyLock<Mutex<VecDeque<(MapKey, Arc<Vec<u8>>)>>> =
        std::sync::LazyLock::new(|| Mutex::new(VecDeque::new()));
    let key: MapKey = (inst.clone(), m.rev.clone(), w);
    let lock = || CACHE.lock().unwrap_or_else(|e| e.into_inner());
    if let Some((_, png)) = lock().iter().find(|(k, _)| *k == key) {
        return Ok(png.clone());
    }
    let png = Arc::new(map_png(m, w)?);
    let mut c = lock();
    c.retain(|(k, _)| *k != key);
    c.push_back((key, png.clone()));
    while c.len() > MAP_CACHE {
        c.pop_front();
    }
    Ok(png)
}

/// `GET /api/cockpit/dash/map?w=<px>&rev=<rev>`
pub(crate) async fn api_map(
    session_id: Option<Uuid>,
    query: Q,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<warp::reply::Response, Error> {
    use warp::http::HeaderValue;
    let snap = snapshot(&db, &bot_cfg, &inst).await?;
    // The rev of the map being served comes only from the dashboard answer,
    // so asking with it is as good as the key that answer took. The panel
    // downloads with no headers it would have to put in the URL.
    let rev_ok =
        snap.map.as_ref().is_some_and(|m| query.get("rev").is_some_and(|r| *r == m.rev));
    refuse(&query, session_id, &db, &bot_cfg, &inst, ip, rev_ok).await?;
    let m = snap
        .map
        .clone()
        .ok_or_else(|| websec::not_found("no map: the campaign has no objectives yet"))?;
    let want = query.get("w").and_then(|s| s.parse::<u32>().ok()).unwrap_or(480);
    let w = snap_width(want);
    let png = task::block_in_place(|| map_cached(&inst.id, &m, w))?;
    let mut resp = warp::reply::Response::new((*png).clone().into());
    let h = resp.headers_mut();
    h.insert("content-type", HeaderValue::from_static("image/png"));
    h.insert(
        "cache-control",
        HeaderValue::from_static(if rev_ok {
            "private, max-age=3600"
        } else {
            "no-store"
        }),
    );
    h.insert("x-content-type-options", HeaderValue::from_static("nosniff"));
    Ok(resp)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn obj(name: &str, kind: &str, owner: &str, lat: f64, lon: f64) -> serde_json::Value {
        json!({ "name": name, "kind": kind, "owner": owner, "lat": lat, "lon": lon,
                "health": 100, "logi": 100, "supply": 100, "fuel": 100,
                "threatened": false, "captureable": false, "live": true })
    }

    fn caucasus() -> Vec<serde_json::Value> {
        vec![
            obj("Batumi", "Airbase", "Blue", 41.61, 41.6),
            obj("Kutaisi", "Airbase", "Blue", 42.18, 42.48),
            obj("Senaki", "FOB", "Blue", 42.24, 42.05),
            obj("Sukhumi", "Airbase", "Red", 42.86, 41.13),
            obj("Gudauta", "FARP", "Red", 43.1, 40.57),
            obj("Sochi", "Logistics Hub", "Red", 43.44, 39.95),
            obj("Stennis", "Carrier Group", "Blue", 0., 0.),
        ]
    }

    #[test]
    fn carriers_and_unplaced_are_not_on_the_map() {
        let objs = map_objs(&caucasus());
        assert_eq!(objs.len(), 6);
        assert!(objs.iter().all(|o| o.name != "Stennis"));
    }

    #[test]
    fn bounds_hold_every_base_and_keep_a_panel_shape() {
        let objs = map_objs(&caucasus());
        let b = bounds_of(&objs).unwrap();
        for o in &objs {
            assert!(
                o.lat > b.south && o.lat < b.north && o.lon > b.west && o.lon < b.east,
                "{o:?}"
            );
        }
        let a = b.aspect();
        assert!((MIN_ASPECT - 1e-9..=MAX_ASPECT + 1e-9).contains(&a), "aspect {a}");
        // a single base still gets an area
        let one = bounds_of(&objs[..1]).unwrap();
        assert!(one.north > one.south && one.east > one.west);
        // a long east-west line is made taller
        let line: Vec<MapObj> = (0..10)
            .map(|i| MapObj {
                name: format!("{i}"),
                kind: "FOB".into(),
                owner: "Blue".into(),
                lat: 42.,
                lon: 30. + i as f64,
                threatened: false,
                captureable: false,
            })
            .collect();
        assert!((bounds_of(&line).unwrap().aspect() - MIN_ASPECT).abs() < 1e-9);
    }

    #[test]
    fn projection_corners() {
        let b = Bounds { north: 44., south: 40., west: 38., east: 46. };
        assert_eq!(b.xy(44., 38., 400., 300.), (0., 0.));
        assert_eq!(b.xy(40., 46., 400., 300.), (400., 300.));
        assert_eq!(map_height(&b, 400), (400. * b.aspect() + 0.5).floor() as u32);
    }

    #[test]
    fn rev_follows_what_is_drawn() {
        let objs = caucasus();
        let front =
            json!({ "mid": [[[42.5, 41.0], [42.6, 42.0]]], "blue": [], "red": [] });
        let a = map_data(&objs, &front).unwrap();
        let b = map_data(&objs, &front).unwrap();
        assert_eq!(a.rev, b.rev);
        assert_eq!(a.rev.len(), 12);
        let mut flipped = objs.clone();
        flipped[2]["owner"] = json!("Red");
        assert_ne!(
            map_data(&flipped, &front).unwrap().rev,
            a.rev,
            "a base changing hands"
        );
        let mut hit = objs.clone();
        hit[0]["threatened"] = json!(true);
        assert_ne!(map_data(&hit, &front).unwrap().rev, a.rev, "a base under attack");
        let mut healthier = objs.clone();
        healthier[0]["health"] = json!(40);
        assert_eq!(
            map_data(&healthier, &front).unwrap().rev,
            a.rev,
            "health is not drawn"
        );
        assert_ne!(map_data(&objs, &json!([])).unwrap().rev, a.rev, "the front line");
        assert!(map_data(&[], &front).is_none());
    }

    #[test]
    fn voronoi_cells_hold_their_own_site() {
        let sites = [[10., 10.], [90., 10.], [50., 80.]];
        let cells = voronoi(&sites, [[-100., -100.], [200., 200.]]);
        for (s, cell) in sites.iter().zip(&cells) {
            assert!(cell.len() >= 3);
            // inside a convex polygon: the same side of every edge
            let signs: Vec<bool> = (0..cell.len())
                .map(|i| {
                    let (a, b) = (cell[i], cell[(i + 1) % cell.len()]);
                    (b[0] - a[0]) * (s[1] - a[1]) - (b[1] - a[1]) * (s[0] - a[0]) >= 0.
                })
                .collect();
            assert!(signs.iter().all(|x| *x == signs[0]), "{s:?} not in {cell:?}");
        }
    }

    #[test]
    fn map_draws_at_every_width() {
        let mut objs = caucasus();
        objs[3]["threatened"] = json!(true);
        objs[4]["captureable"] = json!(true);
        objs[5]["name"] = json!("Sochi <&> \"Adler\"");
        let front = json!({ "mid": [[[42.5, 41.0], [42.6, 42.0]]], "blue": [[[42.4, 41.0], [42.5, 42.0]]], "red": [] });
        let m = map_data(&objs, &front).unwrap();
        assert!(coast_for(&m.objs).is_some(), "the Caucasus has a coast");
        for w in [MAP_WIDTHS[0], 640, MAP_WIDTHS[MAP_WIDTHS.len() - 1]] {
            let svg = map_svg(&m, w);
            assert!(svg.contains("clip-path=\"url(#land)\""));
            assert!(svg.contains("Sochi &lt;&amp;&gt; &quot;Adler&quot;"));
            let png = map_png(&m, w).unwrap();
            assert_eq!(&png[..8], b"\x89PNG\r\n\x1a\n");
            let pm = tiny_skia::Pixmap::decode_png(&png).unwrap();
            assert_eq!((pm.width(), pm.height()), (w, map_height(&m.bounds, w)));
        }
        // somewhere with no coast: plain ground
        let far: Vec<_> = caucasus()
            .into_iter()
            .map(|mut o| {
                o["lat"] = json!(f64_of(&o, "lat") - 10.);
                o
            })
            .collect();
        let m2 = map_data(&far, &json!([])).unwrap();
        assert!(coast_for(&m2.objs).is_none());
        assert!(!map_svg(&m2, 480).contains("clipPath"));
    }

    #[test]
    fn widths_snap_up() {
        assert_eq!(snap_width(1), 320);
        assert_eq!(snap_width(480), 480);
        assert_eq!(snap_width(481), 560);
        assert_eq!(snap_width(5000), 1280);
    }

    #[test]
    fn attention_ranks_like_the_web() {
        let now = chrono::Utc::now();
        let mut o = obj("A", "FOB", "Blue", 42., 42.);
        assert!(attention_of(&o, None, now).is_none());
        o["supply"] = json!(10);
        let (rank, reasons, tone) = attention_of(&o, None, now).unwrap();
        assert_eq!((rank, tone), (4, "warn"));
        assert_eq!(reasons, vec!["Supply 10%"]);
        o["threatened"] = json!(true);
        o["health"] = json!(30);
        let (rank, reasons, tone) = attention_of(&o, None, now).unwrap();
        assert_eq!((rank, tone), (1, "crit"));
        assert_eq!(reasons, vec!["Under attack", "Defences 30%", "Supply 10%"]);
        let cap = CaptureLine {
            time: now - chrono::Duration::minutes(5),
            objective: "A".into(),
            side: "Red".into(),
            by: vec![],
        };
        let (_, reasons, _) =
            attention_of(&obj("A", "FOB", "Red", 42., 42.), Some(&cap), now).unwrap();
        assert_eq!(reasons, vec!["Taken by red 5m ago"]);
        let old = CaptureLine { time: now - chrono::Duration::hours(7), ..cap };
        assert!(
            attention_of(&obj("A", "FOB", "Red", 42., 42.), Some(&old), now).is_none()
        );
    }

    #[test]
    fn dash_answer() {
        let now = chrono::Utc::now();
        let mut objs = caucasus();
        objs[3]["threatened"] = json!(true);
        objs[6]["captureable"] = json!(true); // a carrier: listed, never placed
        let stats = json!({
            "total_pilots": 42,
            "restart_at": (now + chrono::Duration::minutes(90)).to_rfc3339(),
            "active_round": { "id": 7, "number": 12, "scenario": "Southern Flank", "start": (now - chrono::Duration::hours(3)).to_rfc3339() },
            "weather": { "temp_c": 14, "wind_speed_kts": 8, "wind_from_deg": 270, "qnh_hpa": 1013 },
        });
        let kills = vec![
            KillLine {
                time: now - chrono::Duration::minutes(2),
                killer: Some("Viper".into()),
                killer_side: "Blue".into(),
                airframe: Some("F-16C_50".into()),
                weapon: Some("AIM-120C".into()),
                victim: "Su-27".into(),
                victim_side: "Red".into(),
                air: true,
            },
            KillLine {
                time: now - chrono::Duration::hours(2),
                killer: None,
                killer_side: "Red".into(),
                airframe: Some("SA-11".into()),
                weapon: None,
                victim: "Hawg (A-10C)".into(),
                victim_side: "Blue".into(),
                air: true,
            },
        ];
        let caps = vec![CaptureLine {
            time: now - chrono::Duration::minutes(30),
            objective: "Senaki".into(),
            side: "Blue".into(),
            by: vec!["Viper".into(), "Goose".into()],
        }];
        let online: Vec<OnlinePilot> = vec![
            ("Viper".into(), "Blue".into(), Some("F-16C_50".into())),
            ("Goose".into(), "Blue".into(), Some("F-16C_50".into())),
            ("Hawg".into(), "Blue".into(), Some("A-10C_2".into())),
            ("Spec".into(), "Blue".into(), None),
            ("Ivan".into(), "Red".into(), Some("Su-27".into())),
        ];
        let front = json!([]);
        let map = map_data(&objs, &front);
        let v = dash_json(&DashInput {
            now,
            objectives: &objs,
            live: true,
            stats: &stats,
            kills: &kills,
            captures: &caps,
            online: &online,
            map: map.as_ref(),
        });
        assert_eq!(v["round"]["number"], 12);
        assert_eq!(v["round"]["age"], "3h");
        let r = v["restart_in"].as_i64().unwrap();
        assert!((5390..=5400).contains(&r), "{r}");
        assert_eq!(v["kpi"]["online"], 5);
        assert_eq!(v["kpi"]["flying"], 4);
        assert_eq!(v["kpi"]["blue_obj"], 4);
        assert_eq!(v["kpi"]["red_obj"], 3);
        assert_eq!(v["kpi"]["kills_1h"], 1);
        assert_eq!(v["kpi"]["at_risk"], 2);
        let front = v["front"].as_array().unwrap();
        assert_eq!(front[0]["name"], "Stennis", "ready to capture sorts first");
        assert!(front[0]["lat"].is_null(), "a carrier's position stays hidden");
        assert_eq!(front[1]["name"], "Sukhumi");
        assert_eq!(front[1]["reasons"][0], "Under attack");
        assert!(
            front
                .iter()
                .any(|f| f["name"] == "Senaki"
                    && f["reasons"][0] == "Taken by blue 30m ago")
        );
        assert_eq!(v["kills"][0]["ago"], "2m");
        assert_eq!(v["kills"][1]["killer"], serde_json::Value::Null);
        assert_eq!(v["captures"][0]["by"], "Viper, Goose");
        assert_eq!(v["air"]["blue"]["online"], 4);
        assert_eq!(v["air"]["blue"]["types"][0], json!({ "type": "F-16C_50", "n": 2 }));
        assert_eq!(v["air"]["red"]["types"][0]["type"], "Su-27");
        assert_eq!(v["weather"]["qnh_hpa"], 1013);
        assert_eq!(v["map"]["rev"].as_str().unwrap().len(), 12);
        assert!(v["map"]["aspect"].as_f64().unwrap() > 0.);
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn dash_routes() {
        use crate::route_tests::{body_json, ctype, harness, harness_with};
        let h = harness();
        let r = h.get("/api/cockpit/dash", None).await;
        assert_eq!(r.status(), 200);
        assert!(ctype(&r).starts_with("application/json"));
        let v = body_json(r);
        assert!(v["round"].is_null(), "{v}");
        assert_eq!(v["kpi"]["online"], 0);
        assert!(v["map"].is_null(), "no objectives, no map");
        assert_eq!(h.get("/api/cockpit/dash/map?w=480", None).await.status(), 404);

        // a campaign that is not public: the panel's key or an admin
        let h = harness_with(
            json!({ "instances": [{ "id": "test", "public": false }] }),
            |_| (),
        );
        assert_eq!(h.get("/api/cockpit/dash", None).await.status(), 401);
        assert_eq!(
            h.get("/api/cockpit/dash/map?w=480&rev=000000000000", None).await.status(),
            401
        );
        let admin = h.session(true);
        assert_eq!(h.get("/api/cockpit/dash", Some(&admin)).await.status(), 200);
    }

    /// `DASH_MAP_OUT=<folder> cargo test -p bfdb dash_map_sample -- --ignored`
    /// writes sample maps there, to look at.
    #[test]
    #[ignore]
    fn dash_map_sample() {
        let Ok(dir) = std::env::var("DASH_MAP_OUT") else { return };
        let mut objs = vec![
            obj("Batumi", "Airbase", "Blue", 41.61, 41.6),
            obj("Kobuleti", "Airbase", "Blue", 41.93, 41.86),
            obj("Kutaisi", "Airbase", "Blue", 42.18, 42.48),
            obj("Senaki-Kolkhi", "Airbase", "Blue", 42.24, 42.05),
            obj("Zugdidi FOB", "FOB", "Blue", 42.51, 41.87),
            obj("Tbilisi", "Airbase", "Blue", 41.67, 44.95),
            obj("Gori FARP", "FARP", "Blue", 41.98, 44.1),
            obj("Kutaisi Logistics", "Logistics Hub", "Blue", 42.27, 42.7),
            obj("Sukhumi", "Airbase", "Red", 42.86, 41.13),
            obj("Gudauta", "Airbase", "Red", 43.1, 40.57),
            obj("Sochi", "Airbase", "Red", 43.44, 39.95),
            obj("Ochamchire FOB", "FOB", "Red", 42.73, 41.47),
            obj("Tkvarcheli FARP", "FARP", "Red", 42.85, 41.68),
            obj("Mineralnye Vody", "Airbase", "Red", 44.22, 43.08),
            obj("Nalchik", "Airbase", "Red", 43.51, 43.64),
            obj("Beslan", "Airbase", "Red", 43.2, 44.6),
            obj("Mozdok", "Airbase", "Red", 43.79, 44.6),
            obj("Tskhinvali FOB", "FOB", "Neutral", 42.23, 43.97),
        ];
        objs[8]["threatened"] = json!(true);
        objs[11]["captureable"] = json!(true);
        let front = json!({
            "mid": [[[42.62, 41.3], [42.55, 41.9], [42.6, 42.6], [42.5, 43.4], [42.4, 44.4], [42.6, 45.2]]],
            "blue": [[[42.55, 41.3], [42.48, 41.9], [42.53, 42.6], [42.43, 43.4], [42.33, 44.4], [42.53, 45.2]]],
            "red": [[[42.69, 41.3], [42.62, 41.9], [42.67, 42.6], [42.57, 43.4], [42.47, 44.4], [42.67, 45.2]]],
        });
        let m = map_data(&objs, &front).unwrap();
        for w in [560, 960] {
            let path = std::path::Path::new(&dir).join(format!("dash-map-{w}.png"));
            std::fs::write(&path, map_png(&m, w).unwrap()).unwrap();
            println!("wrote {}", path.display());
        }
    }

    #[test]
    fn ago() {
        assert_eq!(fmt_ago(5), "5s");
        assert_eq!(fmt_ago(125), "2m");
        assert_eq!(fmt_ago(7300), "2h");
        assert_eq!(fmt_ago(200_000), "2d");
    }
}
