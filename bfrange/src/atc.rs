// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! The traffic picture for the range's spoken military ATC.
//!
//! bfdb's controllers (`bfdb/src/milatc`) poll `query-atc` once a second. They
//! get what a radar and a tower cab would show: every airfield's runways with
//! their thresholds (the controllers align their taxi network to these),
//! the surface weather, and every player aircraft with its height, speed,
//! climb rate and whether it is on the ground. The decisions are all made on
//! the bfdb side; this only looks.

use crate::{
    players::Players,
    util::{self, V3},
};
use bfprotocols::atc::{AtcAirfield, AtcPicture, AtcRunway, AtcTraffic};
use dcso3::{LuaEnv, MizLua, airbase::AirbaseCategory, world::World};
use log::{info, warn};
use mlua::prelude::*;

#[derive(Debug, Clone)]
struct End {
    name: String,
    thr: V3,
    hdg: f64,
    len: f64,
    width: f64,
}

#[derive(Debug)]
struct Field {
    name: String,
    pos: V3,
    elev_m: f64,
    ends: Vec<End>,
}

#[derive(Debug, Default)]
pub struct Atc {
    fields: Vec<Field>,
    last: f64,
    pub json: Option<String>,
}

/// The two-digit number of a designator, "06" of "06L".
fn number(s: &str) -> Option<u32> {
    s.chars().take_while(|c| c.is_ascii_digit()).collect::<String>().parse().ok()
}

/// Which way a DCS runway really points. `Runway.course` comes with an
/// inconsistent sign across terrains (see `pattern.rs`), so of the course and
/// its negation, take the one whose ends best match the designator numbers in
/// the runway's name ("06L-24R" wants 060 / 240). Falls back to the course.
fn true_axis(name: &str, course_deg: f64) -> f64 {
    let nums: Vec<f64> = name
        .split(['-', '/', ' '])
        .filter_map(|p| number(p.trim()))
        .map(|n| n as f64 * 10.)
        .collect();
    let cands = [course_deg.rem_euclid(360.), (-course_deg).rem_euclid(360.)];
    if nums.is_empty() {
        return cands[0];
    }
    let score = |c: f64| {
        nums.iter()
            .map(|n| util::angdiff(c, *n).abs().min(util::angdiff(c + 180., *n).abs()))
            .fold(f64::MAX, f64::min)
    };
    if score(cands[1]) < score(cands[0]) { cands[1] } else { cands[0] }
}

impl Atc {
    pub fn init(&mut self, lua: MizLua) {
        let r = (|| -> anyhow::Result<()> {
            for ab in World::singleton(lua)?.get_airbases()? {
                let ab = ab?;
                if ab.get_category().ok() != Some(AirbaseCategory::Airdrome) {
                    continue;
                }
                let name = ab.as_object()?.get_name()?.to_string();
                let pos = ab.get_point()?.0;
                let mut ends: Vec<End> = vec![];
                for rw in ab.get_runways()? {
                    let Ok(rw) = rw else { continue };
                    let (Ok(c), Ok(course), Ok(len), Ok(w)) =
                        (rw.position(), rw.course(), rw.length(), rw.width())
                    else {
                        continue;
                    };
                    if len < 300. {
                        continue;
                    }
                    let rname = rw.name().map(|s| s.to_string()).unwrap_or_default();
                    let axis = true_axis(&rname, course.to_degrees());
                    for h in [axis, (axis + 180.).rem_euclid(360.)] {
                        ends.push(End {
                            name: crate::pattern::designator(&rname, h),
                            thr: c.0 - util::dir(h) * (len / 2.),
                            hdg: h,
                            len,
                            width: w,
                        });
                    }
                }
                name_parallels(&mut ends);
                let elev_m = util::ground_height(lua, pos);
                self.fields.push(Field { name, pos, elev_m, ends });
            }
            Ok(())
        })();
        if let Err(e) = r {
            warn!("atc: reading the airfields failed: {e:?}")
        }
        info!("atc: picture covers {} airfields", self.fields.len());
    }

    /// Rebuild the picture, once a second.
    pub fn tick(
        &mut self,
        lua: MizLua,
        players: &Players,
        ai: Vec<AtcTraffic>,
        now: f64,
    ) {
        if self.fields.is_empty() || now - self.last < 1. {
            return;
        }
        self.last = now;
        let wx = Clouds::read(lua);
        let airfields: Vec<AtcAirfield> =
            self.fields.iter().map(|f| airfield(lua, f, &wx)).collect();
        let mut traffic: Vec<AtcTraffic> = players
            .flying
            .values()
            .filter(|f| !f.is_ground)
            .map(|f| {
                let g = util::geo(lua, f.pos);
                let near = self.fields.iter().min_by(|a, b| {
                    util::dist2(a.pos, f.pos).total_cmp(&util::dist2(b.pos, f.pos))
                });
                AtcTraffic {
                    ucid: f.ucid.to_string(),
                    unit_id: Some(f.unit_id.inner() as u64),
                    callsign: f.group_name.clone(),
                    player_name: f.name.clone(),
                    lat: g.lat,
                    lon: g.lon,
                    alt_ft: (f.pos.y * util::M_TO_FT).round() as i32,
                    agl_ft: (f.alt_agl * util::M_TO_FT).round() as i32,
                    heading: f.hdg.round().rem_euclid(360.) as u16,
                    speed_kts: (util::gs_kts(f.vel)).round().max(0.) as u16,
                    vspd_fpm: (f.vel.y * util::M_TO_FT * 60.).round() as i32,
                    on_ground: !f.in_air,
                    field: near.map(|n| n.name.clone()),
                    field_brg: near
                        .map(|n| util::bearing(n.pos, f.pos).round() as u16 % 360)
                        .unwrap_or(0),
                    field_rng_m: near
                        .map(|n| util::dist2(n.pos, f.pos).round() as u32)
                        .unwrap_or(0),
                    rotary: f.is_helo,
                    ai: false,
                    ai_plan: None,
                    ai_state: None,
                    unit_type: f.typ.clone(),
                }
            })
            .collect();
        traffic.extend(ai);
        self.json = serde_json::to_string(&AtcPicture { airfields, traffic }).ok();
    }
}

/// DCS sometimes names both of a pair of parallel runways "06-24". Give them
/// their L and R back from where they lie: looking down the runway, the left
/// one is L.
fn name_parallels(ends: &mut [End]) {
    let idx: Vec<usize> = (0..ends.len()).collect();
    for &i in &idx {
        if ends[i].name.chars().last().map_or(false, |c| c.is_alphabetic()) {
            continue;
        }
        let same: Vec<usize> =
            idx.iter().copied().filter(|&j| ends[j].name == ends[i].name).collect();
        if same.len() != 2 {
            continue;
        }
        let (a, b) = (same[0], same[1]);
        let h = ends[a].hdg;
        // right of the landing direction is positive
        let side = |p: V3| {
            let r = h.to_radians();
            -(p.x) * r.sin() + p.z * r.cos()
        };
        let (left, right) =
            if side(ends[a].thr) < side(ends[b].thr) { (a, b) } else { (b, a) };
        let n = ends[left].name.clone();
        ends[left].name = format!("{n}L");
        ends[right].name = format!("{n}R");
    }
}

/// The mission's cloud layer and visibility.
struct Clouds {
    base_m: Option<f64>,
    cover: Option<String>,
    vis_m: f64,
    precip: bool,
}

impl Clouds {
    fn read(lua: MizLua) -> Self {
        let r = (|| -> LuaResult<Clouds> {
            let g = lua.inner().globals();
            // dcso3 wraps the mission's weather table but has no accessors for its fields
            let wx = dcso3::env::miz::Miz::singleton(lua)
                .and_then(|m| m.weather())
                .map_err(LuaError::external)?;
            let clouds: Option<LuaTable> = wx.raw_get("clouds").ok();
            let density: f64 =
                clouds.as_ref().and_then(|c| c.get("density").ok()).unwrap_or(0.);
            let preset: Option<String> = clouds
                .as_ref()
                .and_then(|c| c.get::<String>("preset").ok())
                .filter(|s| !s.is_empty());
            let base: Option<f64> = clouds.as_ref().and_then(|c| c.get("base").ok());
            let precip = clouds
                .as_ref()
                .and_then(|c| c.get::<f64>("iprecptns").ok())
                .map_or(false, |p| p > 0.)
                || preset.as_deref().map_or(false, |p| p.starts_with("Rainy"));
            let cover = if preset.is_some() {
                Some("clouds".to_string())
            } else {
                match density.round() as i64 {
                    0 => None,
                    1..=2 => Some("few".into()),
                    3..=5 => Some("scattered".into()),
                    6..=8 => Some("broken".into()),
                    _ => Some("overcast".into()),
                }
            };
            let mut vis: f64 = wx
                .get::<LuaTable>("visibility")
                .and_then(|v| v.get("distance"))
                .unwrap_or(80_000.);
            let fog = (|| -> LuaResult<f64> {
                let world: LuaTable = g.raw_get("world")?;
                let weather: LuaTable = world.raw_get("weather")?;
                weather.call_function("getFogVisibilityDistance", ())
            })();
            if let Ok(f) = fog {
                if f > 0. {
                    vis = vis.min(f);
                }
            }
            Ok(Clouds { base_m: cover.as_ref().and(base), cover, vis_m: vis, precip })
        })();
        r.unwrap_or(Clouds { base_m: None, cover: None, vis_m: 80_000., precip: false })
    }
}

fn airfield(lua: MizLua, f: &Field, c: &Clouds) -> AtcAirfield {
    let g = util::geo(lua, f.pos);
    let (wf, wk, _) = util::wind_at(lua, V3::new(f.pos.x, f.elev_m + 10., f.pos.z));
    let (temp, qfe) =
        util::temp_pressure_at(lua, V3::new(f.pos.x, f.elev_m + 2., f.pos.z));
    // QFE to QNH through the standard atmosphere.
    let qnh = qfe / (1. - 0.0065 * f.elev_m / 288.15).powf(5.255877);
    let runways = f
        .ends
        .iter()
        .map(|e| {
            let t = util::geo(lua, e.thr);
            AtcRunway {
                name: e.name.clone(),
                heading: e.hdg.round() as u16 % 360,
                length_m: e.len.round() as u32,
                width_m: e.width.round() as u32,
                thr_lat: Some(t.lat),
                thr_lon: Some(t.lon),
            }
        })
        .collect();
    AtcAirfield {
        id: f.name.clone(),
        name: f.name.clone(),
        lat: g.lat,
        lon: g.lon,
        elev_ft: (f.elev_m * util::M_TO_FT).round() as i32,
        kind: "airbase".into(),
        runways,
        active_runway: None,
        brc: None,
        wind_from_deg: wf.round() as u16 % 360,
        wind_speed_kts: wk.round() as u16,
        qnh_inhg: qnh * 0.029_53,
        qnh_hpa: qnh,
        qfe_inhg: qfe * 0.029_53,
        qfe_hpa: qfe,
        temp_c: temp.round() as i16,
        // DCS models no humidity; nobody reads this on the range.
        dewpoint_c: temp.round() as i16,
        visibility_m: c.vis_m.round().clamp(0., u32::MAX as f64) as u32,
        cloud_base_ft: c.base_m.map(|b| (b * util::M_TO_FT).round() as i32),
        cloud_cover: c.cover.clone(),
        precipitation: c.precip,
        recovery_case: None,
        logi: 100,
        health: 100,
        supply: 100,
        fuel: 100,
        threatened: false,
        open: true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn axis_follows_the_designators() {
        // DCS may give the negated course; the name decides.
        assert!((true_axis("06-24", 66.) - 66.).abs() < 1e-9);
        assert!((true_axis("06-24", -66.) - 66.).abs() < 1e-9);
        assert!((true_axis("13-31", -(360. - 128.)) - 128.).abs() < 1e-9);
        assert!((true_axis("", 90.) - 90.).abs() < 1e-9);
    }

    #[test]
    fn parallels_get_left_and_right() {
        let mk = |name: &str, x: f64, z: f64| End {
            name: name.into(),
            thr: V3::new(x, 0., z),
            hdg: 66.,
            len: 3700.,
            width: 60.,
        };
        // 06 heading 066: the northern runway is on the left.
        let n = mk("06", 600., 0.);
        let s = mk("06", 0., 200.);
        let mut ends = vec![n, s];
        name_parallels(&mut ends);
        assert_eq!(ends[0].name, "06L");
        assert_eq!(ends[1].name, "06R");
    }
}
