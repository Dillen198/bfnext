// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! The theatre's sectors: which part of the map is for what.
//!
//! The mission draws them on the F10 map; this is the engine's side of the
//! same table. It lists them on F10 > Range > Sectors and tells a player what
//! a sector is for the first time they fly into it (and again after a while
//! away), so nobody has to learn the map before their first sortie.

use crate::{players::Players, records, util::{self, V3}};
use bfprotocols::range::cfg::{RangeCfg, SectorCfg};
use dcso3::{coalition::Side, MizLua};
use fxhash::FxHashMap;
use log::{info, warn};

/// Don't repeat a sector's announcement to the same unit within this long.
const ANNOUNCE_AGAIN_S: f64 = 900.;

enum Geom {
    /// DCS x/z vertices
    Poly(Vec<(f64, f64)>),
    Circle { c: V3, r: f64 },
    /// the leg from `a` to `b`, `r` either side
    Track { a: V3, b: V3, r: f64 },
}

pub struct Sector {
    pub cfg: SectorCfg,
    geom: Geom,
    pub centre: V3,
    area: f64,
}

impl Sector {
    fn contains(&self, p: V3) -> bool {
        match &self.geom {
            Geom::Circle { c, r } => util::dist2(*c, p) <= *r,
            Geom::Track { a, b, r } => seg_dist(p, *a, *b) <= *r,
            Geom::Poly(v) => {
                // even-odd rule in the x/z plane
                let (px, pz) = (p.x, p.z);
                let mut inside = false;
                let mut j = v.len() - 1;
                for i in 0..v.len() {
                    let (xi, zi) = v[i];
                    let (xj, zj) = v[j];
                    if (zi > pz) != (zj > pz) && px < (xj - xi) * (pz - zi) / (zj - zi) + xi {
                        inside = !inside;
                    }
                    j = i;
                }
                inside
            }
        }
    }

    fn for_side(&self, side: Side) -> bool {
        match self.cfg.side.as_str() {
            "blue" => side == Side::Blue,
            "red" => side == Side::Red,
            _ => true,
        }
    }

    /// "R-1 SAMGORI (AIR-TO-GROUND)"
    pub fn title(&self) -> String {
        format!("{} ({})", self.cfg.name, self.cfg.kind.label())
    }
}

fn seg_dist(p: V3, a: V3, b: V3) -> f64 {
    let (ax, az, bx, bz) = (a.x, a.z, b.x, b.z);
    let (dx, dz) = (bx - ax, bz - az);
    let l2 = dx * dx + dz * dz;
    let t = if l2 > 0. { (((p.x - ax) * dx + (p.z - az) * dz) / l2).clamp(0., 1.) } else { 0. };
    let (cx, cz) = (ax + t * dx, az + t * dz);
    ((p.x - cx).powi(2) + (p.z - cz).powi(2)).sqrt()
}

#[derive(Default)]
pub struct Sectors {
    pub list: Vec<Sector>,
    /// unit name -> the sector it was in at the last check
    current: FxHashMap<String, Option<usize>>,
    /// (unit name, sector) -> when it was last announced
    told: FxHashMap<(String, usize), f64>,
}

impl Sectors {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg) {
        self.list.clear();
        for s in &cfg.sectors {
            let sh = &s.shape;
            let r: anyhow::Result<(Geom, V3, f64)> = (|| {
                if let Some(pts) = &sh.polygon {
                    let v = pts
                        .iter()
                        .map(|p| util::from_latlon(lua, p.lat, p.lon).map(|q| (q.x, q.z)))
                        .collect::<anyhow::Result<Vec<_>>>()?;
                    let n = v.len() as f64;
                    let c = V3::new(v.iter().map(|p| p.0).sum::<f64>() / n, 0., v.iter().map(|p| p.1).sum::<f64>() / n);
                    let mut area = 0.;
                    for i in 0..v.len() {
                        let (x0, z0) = v[i];
                        let (x1, z1) = v[(i + 1) % v.len()];
                        area += x0 * z1 - x1 * z0;
                    }
                    Ok((Geom::Poly(v), c, area.abs() / 2.))
                } else if let Some(c) = &sh.circle {
                    let p = util::from_latlon(lua, c.lat, c.lon)?;
                    Ok((Geom::Circle { c: p, r: c.radius_m }, p, std::f64::consts::PI * c.radius_m * c.radius_m))
                } else if let Some(t) = &sh.track {
                    let a = util::from_latlon(lua, t.lat, t.lon)?;
                    let b = util::offset(a, t.heading_deg, t.leg_m, 0.);
                    let r = t.width_m / 2.;
                    let area = t.leg_m * t.width_m + std::f64::consts::PI * r * r;
                    Ok((Geom::Track { a, b, r }, (a + b) / 2., area))
                } else {
                    Err(anyhow::anyhow!("no shape"))
                }
            })();
            match r {
                Ok((geom, mut centre, area)) => {
                    centre.y = util::ground_height(lua, centre);
                    self.list.push(Sector { cfg: s.clone(), geom, centre, area });
                }
                Err(e) => warn!("sector {}: {e:?}", s.id),
            }
        }
        info!("{} sectors", self.list.len());
    }

    /// The most specific sector `p` is in: where two overlap, the smaller.
    pub fn at(&self, p: V3) -> Option<usize> {
        self.list
            .iter()
            .enumerate()
            .filter(|(_, s)| s.contains(p))
            .min_by(|a, b| a.1.area.total_cmp(&b.1.area))
            .map(|(i, _)| i)
    }

    /// Tell each player what a sector is for when they fly into it.
    pub fn tick(&mut self, lua: MizLua, players: &Players, now: f64) {
        self.current.retain(|unit, _| players.flying.contains_key(unit));
        for (unit, f) in &players.flying {
            let here = self.at(f.pos);
            let before = self.current.insert(unit.clone(), here);
            // the first check after slotting in only records where they are:
            // nobody needs telling they are parked at their home field
            let Some(before) = before else { continue };
            let Some(i) = here else { continue };
            if before == Some(i) {
                continue;
            }
            let s = &self.list[i];
            if !s.cfg.announce {
                continue;
            }
            let key = (unit.clone(), i);
            if self.told.get(&key).is_some_and(|t| now - t < ANNOUNCE_AGAIN_S) {
                continue;
            }
            self.told.insert(key, now);
            let other = if s.for_side(f.side) { "" } else { " (laid out for the other side)" };
            let what = if s.cfg.purpose.is_empty() { String::new() } else { format!("\n{}", s.cfg.purpose) };
            records::to_group(lua, f.group_id, &format!("Entering {}{other}{what}", s.title()), 12);
        }
        self.told.retain(|_, t| now - *t < ANNOUNCE_AGAIN_S);
    }

    /// F10 > Range > Sectors: this side's sectors and the shared ones,
    /// nearest first, with bearing and range from `from`.
    pub fn describe(&self, from: V3, side: Side) -> Vec<String> {
        let mut v: Vec<(f64, String)> = self
            .list
            .iter()
            .filter(|s| s.for_side(side))
            .map(|s| {
                let d = if s.contains(from) { 0. } else { util::dist2(from, s.centre) };
                let where_ = if d == 0. {
                    "you are here".to_string()
                } else {
                    format!("{:03.0}/{:.0}nm", util::bearing(from, s.centre), d / util::NM)
                };
                (d, format!("{} {where_}{}", s.title(), if s.cfg.purpose.is_empty() { String::new() } else { format!(" - {}", s.cfg.purpose) }))
            })
            .collect();
        v.sort_by(|a, b| a.0.total_cmp(&b.0));
        v.into_iter().map(|(_, s)| s).collect()
    }
}
