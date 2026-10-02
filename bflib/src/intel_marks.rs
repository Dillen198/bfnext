//! Draw the dashboard's coalition recon markup onto the in-game F10 map.
//!
//! bfdb pushes the active round's markup as JSON on the `intel-marks` RPC
//! every few seconds. We reconcile it against what's currently drawn:
//! newly-added shapes get drawn once (markup is immutable once created),
//! deleted shapes get their marks removed. Each item is visible only to its
//! own coalition.

use anyhow::{Context as _, Result};
use dcso3::{
    coalition::Side,
    coord::{Coord, LLPos},
    trigger::{CircleSpec, LineSpec, LineType, MarkId, RectSpec, TextSpec},
    Color, LuaVec3, MizLua, Vector2, Vector3,
};
use fxhash::{FxHashMap, FxHashSet};
use log::warn;
use serde::Deserialize;
use smallvec::{smallvec, SmallVec};
use std::str::FromStr;

#[derive(Debug, Deserialize)]
pub struct IntelMarksPayload {
    #[serde(default)]
    pub marks: Vec<IntelMark>,
}

#[derive(Debug, Deserialize)]
pub struct IntelMark {
    pub id: String,
    pub side: String,
    pub kind: String,
    pub points: Vec<[f64; 2]>, // [lat, lon]
    #[serde(default)]
    pub color: String,
    #[serde(default)]
    pub by_name: String,
}

/// Most points drawn for one freehand line. A pencil stroke from the
/// dashboard can carry thousands of points and every segment is its own F10
/// line (and message queue entry); past this the stroke is thinned.
const MAX_LINE_POINTS: usize = 200;

/// `#rrggbb` (the `#` optional) to a colour, yellow for anything else.
fn parse_hex_rgb(s: &str) -> Option<(u8, u8, u8)> {
    let h = s.strip_prefix('#').unwrap_or(s).as_bytes();
    // Checked byte-wise before slicing: the colour is dashboard input, and a
    // six-BYTE string holding a multi-byte character used to panic on a
    // non-char-boundary `&h[0..2]` inside the admin command loop.
    if h.len() != 6 || !h.iter().all(u8::is_ascii_hexdigit) {
        return None;
    }
    let byte = |i: usize| {
        let d = |c: u8| (c as char).to_digit(16).unwrap_or(0) as u8;
        d(h[i]) * 16 + d(h[i + 1])
    };
    Some((byte(0), byte(2), byte(4)))
}

fn hex_color(s: &str, alpha: f32) -> Color {
    match parse_hex_rgb(s) {
        Some((r, g, b)) => Color::new(r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0, alpha),
        None => Color::yellow(alpha),
    }
}

/// Thin `pts` to at most `max` points, keeping the first and last.
fn decimate<T: Copy>(pts: &[T], max: usize) -> Vec<T> {
    if pts.len() <= max || max < 2 {
        return pts.to_vec();
    }
    let step = pts.len().div_ceil(max - 1);
    let mut out: Vec<T> = pts.iter().step_by(step).copied().collect();
    if let Some(last) = pts.last() {
        if (pts.len() - 1) % step != 0 {
            out.push(*last);
        }
    }
    out
}

fn ground(coord: &Coord, p: [f64; 2]) -> Result<LuaVec3> {
    let v = coord.ll_to_lo(LLPos {
        latitude: p[0],
        longitude: p[1],
        altitude: 0.0,
    })?;
    // Map drawing wants (x, 0, z) — drop the terrain altitude.
    Ok(LuaVec3(Vector3::new(v.0.x, 0.0, v.0.z)))
}

fn dist(a: &LuaVec3, b: &LuaVec3) -> f64 {
    ((a.0.x - b.0.x).powi(2) + (a.0.z - b.0.z).powi(2)).sqrt()
}

/// Reconcile the F10 map drawing against `json`, tracking drawn marks in
/// `state` (item id -> the MarkIds it produced).
pub fn reconcile(
    state: &mut FxHashMap<String, SmallVec<[MarkId; 4]>>,
    msgs: &mut crate::msgq::MsgQ,
    lua: MizLua,
    json: &str,
) -> Result<()> {
    let payload: IntelMarksPayload =
        serde_json::from_str(json).context("parsing intel-marks payload")?;
    let coord = Coord::singleton(lua)?;
    let clear = Color::black(0.0);

    let mut seen: FxHashSet<String> = FxHashSet::default();
    for m in &payload.marks {
        seen.insert(m.id.clone());
        if state.contains_key(&m.id) || m.points.is_empty() {
            continue;
        }
        let side = match Side::from_str(&m.side) {
            Ok(s) => s,
            Err(_) => continue,
        };
        let sf = side.into();
        let col = hex_color(&m.color, 0.9);
        let mut ids: SmallVec<[MarkId; 4]> = smallvec![];

        // Convert every point before drawing anything. A `?` part way through
        // used to abort the whole push with some of this item's marks already
        // queued but never recorded in `state` -- stuck on the map for good --
        // and every later item never drawn. A shape that can't be converted
        // is recorded with no marks instead: markup is immutable, so trying
        // it again on the next push would only fail the same way.
        let pts: Vec<[f64; 2]> = match m.kind.as_str() {
            "circle" | "rect" => m.points.iter().take(2).copied().collect(),
            _ => decimate(&m.points, MAX_LINE_POINTS),
        };
        let pts: Vec<LuaVec3> = match pts.iter().map(|p| ground(&coord, *p)).collect::<Result<_>>() {
            Ok(pts) => pts,
            Err(e) => {
                warn!("intel mark {} could not be placed, skipping it: {e:?}", m.id);
                state.insert(m.id.clone(), ids);
                continue;
            }
        };

        match m.kind.as_str() {
            "circle" if pts.len() >= 2 => {
                let (c, e) = (pts[0], pts[1]);
                let id = MarkId::new();
                msgs.circle_to_all(
                    sf,
                    id,
                    CircleSpec {
                        center: c,
                        radius: dist(&c, &e).max(50.0),
                        color: col,
                        fill_color: clear,
                        line_type: LineType::Solid,
                        read_only: true,
                    },
                    None,
                );
                ids.push(id);
            }
            "rect" if pts.len() >= 2 => {
                let (a, b) = (pts[0], pts[1]);
                let id = MarkId::new();
                msgs.rect_to_all(
                    sf,
                    id,
                    RectSpec {
                        start: a,
                        end: b,
                        color: col,
                        fill_color: clear,
                        line_type: LineType::Solid,
                        read_only: true,
                    },
                    None,
                );
                ids.push(id);
            }
            // line, pencil, x: draw as connected segments (x/single point → a dot)
            _ => {
                if pts.len() == 1 {
                    let p = Vector2::new(pts[0].0.x, pts[0].0.z);
                    ids.push(msgs.mark_to_side(side, p, true, "✕ recon"));
                } else {
                    for w in pts.windows(2) {
                        let id = MarkId::new();
                        msgs.line_to_all(
                            sf,
                            id,
                            LineSpec {
                                start: w[0],
                                end: w[1],
                                color: col,
                                line_type: LineType::Solid,
                                read_only: true,
                            },
                            None,
                        );
                        ids.push(id);
                    }
                }
            }
        }

        // author label at the first point
        if !m.by_name.is_empty() {
            if let Some(&anchor) = pts.first() {
                let id = MarkId::new();
                msgs.text_to_all(
                    sf,
                    id,
                    TextSpec {
                        pos: anchor,
                        color: col,
                        fill_color: clear,
                        font_size: 10,
                        read_only: true,
                        text: format!("recon · {}", m.by_name).into(),
                    },
                );
                ids.push(id);
            }
        }

        state.insert(m.id.clone(), ids);
    }

    // Remove marks whose source item is gone.
    let stale: Vec<String> = state.keys().filter(|k| !seen.contains(*k)).cloned().collect();
    for k in stale {
        if let Some(ids) = state.remove(&k) {
            for id in ids {
                msgs.delete_mark(id);
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hex_colours_parse_and_bad_input_falls_back() {
        assert_eq!(parse_hex_rgb("#ff8000"), Some((255, 128, 0)));
        assert_eq!(parse_hex_rgb("00FFaa"), Some((0, 255, 170)));
        assert_eq!(parse_hex_rgb("#fff"), None);
        assert_eq!(parse_hex_rgb("#gg0000"), None);
        // six bytes with a multi-byte char: used to panic slicing mid-char
        assert_eq!(parse_hex_rgb("#a\u{e9}123"), None);
        assert_eq!(parse_hex_rgb("\u{e9}\u{e9}\u{e9}"), None);
        assert_eq!(parse_hex_rgb(""), None);
    }

    #[test]
    fn decimate_caps_and_keeps_ends() {
        let pts: Vec<usize> = (0..1000).collect();
        let d = decimate(&pts, 200);
        assert!(d.len() <= 200, "{}", d.len());
        assert_eq!(d.first(), Some(&0));
        assert_eq!(d.last(), Some(&999));
        let short: Vec<usize> = (0..10).collect();
        assert_eq!(decimate(&short, 200), short);
        let exact: Vec<usize> = (0..201).collect();
        let d = decimate(&exact, 200);
        assert!(d.len() <= 200);
        assert_eq!(d.last(), Some(&200));
    }
}