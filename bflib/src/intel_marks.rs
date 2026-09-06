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

fn hex_color(s: &str, alpha: f32) -> Color {
    let h = s.trim_start_matches('#');
    if h.len() == 6 {
        if let (Ok(r), Ok(g), Ok(b)) = (
            u8::from_str_radix(&h[0..2], 16),
            u8::from_str_radix(&h[2..4], 16),
            u8::from_str_radix(&h[4..6], 16),
        ) {
            return Color::new(r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0, alpha);
        }
    }
    Color::yellow(alpha)
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

        match m.kind.as_str() {
            "circle" if m.points.len() >= 2 => {
                let c = ground(&coord, m.points[0])?;
                let e = ground(&coord, m.points[1])?;
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
            "rect" if m.points.len() >= 2 => {
                let a = ground(&coord, m.points[0])?;
                let b = ground(&coord, m.points[1])?;
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
                let pts: Vec<LuaVec3> = m
                    .points
                    .iter()
                    .map(|p| ground(&coord, *p))
                    .collect::<Result<_>>()?;
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
            if let Ok(anchor) = ground(&coord, m.points[0]) {
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
