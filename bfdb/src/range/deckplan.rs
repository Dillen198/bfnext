// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Carrier deck plans for the result cards: the ship as a technical drawing,
//! in the frame the groove tracker grades in, so a pass drawn over it meets
//! the deck where the engine said it did.
//!
//! The plans live in `shared/carriers/decks.json`, which the range site's 2D
//! sheet and 3D model read too, so all three draw the same ship. They are
//! traced from scale drawings and overhead photographs (sources in the
//! file): Nimitz, Forrestal (the Nimitz plan scaled), Admiral Kuznetsov,
//! Charles de Gaulle and the Tarawa-class LHA.
//!
//! The grading numbers (where the landing point is, the deck angle, the
//! wires) are the engine's: for a class it grades (`CarrierKind`), the
//! engine's own `deck()` is used, and a test checks the file agrees.
//!
//! Frames:
//! - ship: `f` metres forward of the stern, `r` metres right of the keel,
//!   `z` metres above the waterline
//! - landing (the record's): `x` metres AFT of the landing point along the
//!   final bearing, `y` metres right of the landing-area centreline

use bfprotocols::range::cfg::{CarrierKind, DeckGeometry};
use serde::Deserialize;
use std::{collections::BTreeMap, sync::OnceLock};

pub(crate) type P = (f64, f64);

const DECKS_JSON: &str = include_str!("../../../shared/carriers/decks.json");

#[derive(Debug, Clone, Deserialize)]
struct Grading {
    stern_m: f64,
    stern_offset_m: f64,
    deck_height_m: f64,
    angle_deg: f64,
    wires_m: [f64; 4],
    /// 1-based; 0 = no wires
    target_wire: usize,
    glideslope_deg: f64,
}

#[derive(Debug, Clone, Default, Deserialize)]
struct Side {
    #[serde(default)]
    hull: Vec<P>,
    #[serde(default)]
    island: Vec<Vec<P>>,
    #[serde(default)]
    near_elevators: Vec<P>,
    /// the whole ship in elevation, traced (hull, island and masts in one outline)
    #[serde(default)]
    silhouette: Vec<P>,
}

#[derive(Debug, Clone, Deserialize)]
struct Class {
    name: String,
    #[serde(rename = "match")]
    matches: String,
    #[serde(default)]
    scale_of: Option<String>,
    #[serde(default = "one")]
    scale_f: f64,
    #[serde(default = "one")]
    scale_r: f64,
    length: f64,
    draft: f64,
    grading: Grading,
    landing_len: f64,
    landing_half_w: f64,
    #[serde(default)]
    outline: Vec<P>,
    #[serde(default)]
    waterline: Vec<P>,
    #[serde(default)]
    island: Vec<P>,
    #[serde(default)]
    island_top: Vec<P>,
    #[serde(default)]
    elevators: Vec<Vec<P>>,
    #[serde(default)]
    cats: Vec<(P, P)>,
    #[serde(default)]
    runs: Vec<(P, P)>,
    #[serde(default)]
    spots: Vec<P>,
    #[serde(default)]
    lens: Option<P>,
    #[serde(default)]
    lso: Option<P>,
    #[serde(default)]
    ski_jump: Option<P>,
    #[serde(default)]
    side: Side,
}

fn one() -> f64 {
    1.
}

#[derive(Deserialize)]
struct File {
    classes: BTreeMap<String, Class>,
}

pub(crate) struct Plan {
    /// the class key in decks.json ("nimitz", "cdg" ...)
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) key: &'static str,
    pub(crate) name: String,
    pub(crate) g: DeckGeometry,
    target_wire: usize,
    pub(crate) length: f64,
    pub(crate) draft: f64,
    /// flight deck, ship frame
    pub(crate) outline: Vec<P>,
    /// the hull at the waterline (hidden under the overhang in plan)
    pub(crate) waterline: Vec<P>,
    pub(crate) island: Vec<P>,
    pub(crate) island_top: Vec<P>,
    pub(crate) elevators: Vec<Vec<P>>,
    /// catapult tracks
    pub(crate) cats: Vec<(P, P)>,
    /// ski-jump take-off runs (no catapults)
    pub(crate) runs: Vec<(P, P)>,
    /// helicopter / VTOL landing spots
    pub(crate) spots: Vec<P>,
    pub(crate) ski_jump: Option<P>,
    pub(crate) lens: Option<P>,
    pub(crate) lso: Option<P>,
    pub(crate) landing_len: f64,
    pub(crate) landing_half_w: f64,
    /// side elevation, (f, z): the hull above water, closed
    pub(crate) side_hull: Vec<P>,
    /// side elevation, (f, z): the island blocks and the mast (two points = a line)
    pub(crate) side_island: Vec<Vec<P>>,
    /// side elevation: deck-edge elevators on the near (port) side, f range
    pub(crate) side_elevators: Vec<P>,
    /// side elevation, (f, z): the traced outline of the whole ship, when there is one
    pub(crate) side_silhouette: Vec<P>,
    /// hull number painted on the deck and the island
    pub(crate) hull_no: Option<String>,
}

const ORDER: [&str; 5] = ["forrestal", "kuznetsov", "cdg", "tarawa", "nimitz"];

fn file() -> &'static File {
    static F: OnceLock<File> = OnceLock::new();
    F.get_or_init(|| {
        serde_json::from_str(DECKS_JSON).expect("shared/carriers/decks.json")
    })
}

/// The engine's deck for the classes it grades.
fn engine_kind(key: &str) -> Option<CarrierKind> {
    match key {
        "nimitz" => Some(CarrierKind::Nimitz),
        "forrestal" => Some(CarrierKind::Forrestal),
        "kuznetsov" => Some(CarrierKind::Kuznetsov),
        "tarawa" => Some(CarrierKind::Tarawa),
        _ => None,
    }
}

fn class_key(carrier_type: &str) -> &'static str {
    let t = carrier_type.to_ascii_lowercase();
    let f = file();
    for k in ORDER {
        let Some(c) = f.classes.get(k) else { continue };
        if c.matches
            .split('|')
            .any(|m| !m.is_empty() && t.contains(&m.to_ascii_lowercase()))
        {
            return k;
        }
    }
    "nimitz"
}

fn hull_no(carrier_type: &str) -> Option<String> {
    let t = carrier_type.to_ascii_uppercase();
    // "CVN_73", "CVN-71", "LHA_1": the digits after the type letters
    for pre in ["CVN", "CV", "LHA", "LHD", "R"] {
        if let Some(i) = t.find(pre) {
            let digits: String = t[i + pre.len()..]
                .trim_start_matches(['_', '-', ' '])
                .chars()
                .take_while(|c| c.is_ascii_digit())
                .collect();
            if !digits.is_empty() && digits.len() <= 3 {
                return Some(digits);
            }
        }
    }
    if t.contains("STENNIS") {
        return Some("74".into());
    }
    if t.contains("FORRESTAL") {
        return Some("59".into());
    }
    if t.contains("TARAWA") {
        return Some("1".into());
    }
    None
}

fn scaled(v: &[P], sf: f64, sr: f64) -> Vec<P> {
    v.iter().map(|(a, b)| (a * sf, b * sr)).collect()
}

/// The deck plan for a DCS carrier type ("CVN_73", "Stennis", "Forrestal", "KUZNECOW", "LHA_Tarawa", "CdG"...).
pub(crate) fn plan_for(carrier_type: &str) -> Plan {
    let key = class_key(carrier_type);
    let f = file();
    let c = &f.classes[key];
    // a class drawn as another one scaled (Forrestal = Nimitz)
    let base = c.scale_of.as_deref().and_then(|b| f.classes.get(b)).unwrap_or(c);
    let (sf, sr) = if c.scale_of.is_some() { (c.scale_f, c.scale_r) } else { (1., 1.) };
    let s = |v: &[P]| scaled(v, sf, sr);
    let sp = |p: &Option<P>| p.map(|(a, b)| (a * sf, b * sr));
    let gr = &c.grading;
    let g = engine_kind(key).map(|k| k.deck()).unwrap_or(DeckGeometry {
        stern_m: gr.stern_m,
        stern_offset_m: gr.stern_offset_m,
        deck_height_m: gr.deck_height_m,
        angle_deg: gr.angle_deg,
        wires_m: gr.wires_m,
        glideslope_deg: gr.glideslope_deg,
    });
    Plan {
        key,
        name: c.name.clone(),
        g,
        target_wire: gr.target_wire,
        length: c.length,
        draft: c.draft,
        outline: s(&base.outline),
        waterline: s(&base.waterline),
        island: s(&base.island),
        island_top: s(&base.island_top),
        elevators: base.elevators.iter().map(|e| s(e)).collect(),
        cats: base
            .cats
            .iter()
            .map(|(a, b)| ((a.0 * sf, a.1 * sr), (b.0 * sf, b.1 * sr)))
            .collect(),
        runs: base
            .runs
            .iter()
            .map(|(a, b)| ((a.0 * sf, a.1 * sr), (b.0 * sf, b.1 * sr)))
            .collect(),
        spots: s(&base.spots),
        ski_jump: sp(&base.ski_jump),
        lens: sp(&base.lens),
        lso: sp(&base.lso),
        landing_len: c.landing_len,
        landing_half_w: c.landing_half_w,
        side_hull: base.side.hull.iter().map(|(a, z)| (a * sf, *z)).collect(),
        side_island: base
            .side
            .island
            .iter()
            .map(|v| v.iter().map(|(a, z)| (a * sf, *z)).collect())
            .collect(),
        side_elevators: base
            .side
            .near_elevators
            .iter()
            .map(|(a, b)| (a * sf, b * sf))
            .collect(),
        side_silhouette: base.side.silhouette.iter().map(|(a, z)| (a * sf, *z)).collect(),
        hull_no: hull_no(carrier_type),
    }
}

impl Plan {
    /// The engine's landing point, metres from the stern along the landing area (the target wire; 60 m without wires).
    pub(crate) fn lp_from_stern(&self) -> f64 {
        match self.target_wire {
            n @ 1..=4 if self.g.wires_m[n - 1] > 0. => self.g.wires_m[n - 1],
            _ => 60.,
        }
    }

    /// The landing area at `d` metres from the stern, `off` metres right of its centreline, in the ship frame.
    pub(crate) fn on_landing(&self, d: f64, off: f64) -> P {
        let a = self.g.angle_deg.to_radians();
        (d * a.cos() - off * a.sin(), self.g.stern_offset_m + d * a.sin() + off * a.cos())
    }

    /// Ship frame → landing frame.
    pub(crate) fn to_landing(&self, p: P) -> P {
        let a = -self.g.angle_deg.to_radians();
        let fwd = (-a.cos(), a.sin());
        let right = (a.sin(), a.cos());
        let k = (
            self.lp_from_stern() - self.g.stern_offset_m * right.0,
            -self.g.stern_offset_m * right.1,
        );
        (k.0 + p.0 * fwd.0 + p.1 * right.0, k.1 + p.0 * fwd.1 + p.1 * right.1)
    }

    /// x (landing frame) of the keel at `f`, for the side elevation.
    pub(crate) fn side_x(&self, f: f64) -> f64 {
        self.to_landing((f, 0.)).0
    }

    /// Landing-frame x of each wire.
    pub(crate) fn wires_x(&self) -> Vec<f64> {
        let lp = self.lp_from_stern();
        self.g.wires_m.iter().filter(|w| **w > 0.).map(|w| lp - w).collect()
    }

    pub(crate) fn ramp_x(&self) -> f64 {
        self.lp_from_stern()
    }

    pub(crate) fn bow_x(&self) -> f64 {
        self.outline.iter().map(|p| self.to_landing(*p).0).fold(f64::INFINITY, f64::min)
    }

    /// The keel line extended aft: the wake's lateral position at landing-frame `x`.
    pub(crate) fn wake_y(&self, x: f64) -> f64 {
        let s = self.to_landing((0., 0.));
        s.1 - (x - s.0) * (-self.g.angle_deg).to_radians().tan()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_landing_point_is_the_target_wire() {
        let p = plan_for("CVN_73");
        let lp = p.to_landing(p.on_landing(p.lp_from_stern(), 0.));
        assert!(lp.0.abs() < 1e-9 && lp.1.abs() < 1e-9, "{lp:?}");
        assert_eq!(p.ramp_x(), 70.);
        let w = p.wires_x();
        assert_eq!(w.len(), 4);
        assert!(w[0] > w[1] && w[1] > w[2] && w[2] > w[3]);
        assert!(p.bow_x() < -250.);
        // the island is aft of amidships, to starboard
        let ic = p.island.iter().map(|q| q.0).sum::<f64>() / p.island.len() as f64;
        assert!(ic > 90. && ic < 140. && p.island.iter().all(|q| q.1 > 0.));
        // the Charles de Gaulle grades against its middle wire of three
        let c = plan_for("CdG");
        assert_eq!(c.key, "cdg");
        assert_eq!(c.wires_x().len(), 3);
        assert_eq!(c.lp_from_stern(), 54.);
    }

    #[test]
    fn every_class_has_a_plan_and_agrees_with_the_engine() {
        for (t, k) in [
            ("CVN_71", "nimitz"),
            ("Stennis", "nimitz"),
            ("Forrestal", "forrestal"),
            ("KUZNECOW", "kuznetsov"),
            ("LHA_Tarawa", "tarawa"),
            ("Charles de Gaulle", "cdg"),
        ] {
            let p = plan_for(t);
            assert_eq!(p.key, k, "{t}");
            assert!(
                p.outline.len() > 4 && p.side_hull.len() > 4 && !p.island.is_empty(),
                "{t}"
            );
            // every plan point inside the ship's length
            assert!(
                p.outline.iter().all(|q| q.0 >= -0.01 && q.0 <= p.length + 0.5),
                "{t}"
            );
        }
        // the file's grading block is the engine's, for the classes the engine grades
        for (k, kind) in [
            ("nimitz", CarrierKind::Nimitz),
            ("forrestal", CarrierKind::Forrestal),
            ("kuznetsov", CarrierKind::Kuznetsov),
            ("tarawa", CarrierKind::Tarawa),
        ] {
            let g = &file().classes[k].grading;
            let e = kind.deck();
            assert_eq!(
                (
                    g.stern_m,
                    g.stern_offset_m,
                    g.deck_height_m,
                    g.angle_deg,
                    g.wires_m,
                    g.glideslope_deg
                ),
                (
                    e.stern_m,
                    e.stern_offset_m,
                    e.deck_height_m,
                    e.angle_deg,
                    e.wires_m,
                    e.glideslope_deg
                ),
                "{k}"
            );
        }
        assert_eq!(plan_for("CVN_73").hull_no.as_deref(), Some("73"));
    }
}
