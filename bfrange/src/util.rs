// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Geometry, Lua table construction, clocks and the sun.
//!
//! DCS world frame: x = north, y = up, z = east (metres). Headings here are
//! degrees true, clockwise from north, so `atan2(z, x)`.

use anyhow::{anyhow, Result};
use bfprotocols::range::{AtmoLayer, GeoPt};
use chrono::{Datelike, NaiveDate};
use dcso3::{
    coord::{Coord, LLPos},
    LuaEnv, LuaVec3, MizLua, Position3,
};
use mlua::{prelude::*, Value};

pub type V3 = nalgebra::Vector3<f64>;

pub const MS_TO_KTS: f64 = 1.943_844;
pub const M_TO_FT: f64 = 3.280_84;
pub const NM: f64 = 1852.;
pub const KG_TO_LB: f64 = 2.204_62;

/// A position as lat/lon/alt. Falls back to 0,0 rather than failing a whole
/// result card over a coordinate conversion.
pub fn geo(lua: MizLua, p: V3) -> GeoPt {
    match Coord::singleton(lua).and_then(|c| c.lo_to_ll(LuaVec3(p))) {
        Ok(ll) => GeoPt { lat: ll.latitude, lon: ll.longitude, alt_m: p.y },
        Err(_) => GeoPt { lat: 0., lon: 0., alt_m: p.y },
    }
}

pub fn from_latlon(lua: MizLua, lat: f64, lon: f64) -> Result<V3> {
    let v = Coord::singleton(lua)?.ll_to_lo(LLPos { latitude: lat, longitude: lon, altitude: 0. })?;
    Ok(v.0)
}

/// Heading of a vector's horizontal component, 0..360.
pub fn hdg(v: V3) -> f64 {
    v.z.atan2(v.x).to_degrees().rem_euclid(360.)
}

pub fn bearing(from: V3, to: V3) -> f64 {
    hdg(to - from)
}

pub fn dist2(a: V3, b: V3) -> f64 {
    ((a.x - b.x).powi(2) + (a.z - b.z).powi(2)).sqrt()
}

pub fn dist3(a: V3, b: V3) -> f64 {
    (a - b).norm()
}

/// Signed smallest difference a - b, in (-180, 180].
pub fn angdiff(a: f64, b: f64) -> f64 {
    let d = (a - b + 540.).rem_euclid(360.) - 180.;
    if d == -180. { 180. } else { d }
}

/// Unit vector along a heading, horizontal.
pub fn dir(hdg_deg: f64) -> V3 {
    let r = hdg_deg.to_radians();
    V3::new(r.cos(), 0., r.sin())
}

/// `p` moved `fwd` metres along `hdg_deg` and `right` metres to its right.
pub fn offset(p: V3, hdg_deg: f64, fwd: f64, right: f64) -> V3 {
    let f = dir(hdg_deg);
    let r = dir(hdg_deg + 90.);
    p + f * fwd + r * right
}

/// Horizontal speed in knots.
pub fn gs_kts(v: V3) -> f64 {
    (v.x * v.x + v.z * v.z).sqrt() * MS_TO_KTS
}

/// Flight-path angle, degrees, + = climbing.
pub fn fpa(v: V3) -> f64 {
    let h = (v.x * v.x + v.z * v.z).sqrt();
    v.y.atan2(h).to_degrees()
}

/// A body frame from `getPosition()`: x forward, y up, z right.
#[derive(Debug, Clone, Copy)]
pub struct Frame {
    pub p: V3,
    pub x: V3,
    pub y: V3,
    pub z: V3,
}

impl Frame {
    pub fn from_pos(p: &Position3) -> Self {
        Self { p: p.p.0, x: p.x.0, y: p.y.0, z: p.z.0 }
    }

    /// World point -> (fwd, up, right) in this frame, metres.
    pub fn to_local(&self, w: V3) -> (f64, f64, f64) {
        let d = w - self.p;
        (d.dot(&self.x), d.dot(&self.y), d.dot(&self.z))
    }

    /// Angle of attack from a velocity (air-relative) vector, degrees: the
    /// angle of the velocity in the body x/y plane, positive when the
    /// velocity comes from below the nose. The MOOSE method.
    pub fn aoa(&self, v_air: V3) -> f64 {
        let vx = v_air.dot(&self.x);
        let vy = v_air.dot(&self.y);
        (-vy).atan2(vx).to_degrees()
    }
}

/// Wind at a point: (from degrees, knots, vector m/s).
pub fn wind_at(lua: MizLua, p: V3) -> (f64, f64, V3) {
    let w = dcso3::atmosphere::Atmosphere::singleton(lua)
        .and_then(|a| a.get_wind(LuaVec3(p)))
        .map(|w| w.0)
        .unwrap_or_else(|_| V3::zeros());
    let from = (-w.z).atan2(-w.x).to_degrees().rem_euclid(360.);
    let kts = (w.x * w.x + w.z * w.z).sqrt() * MS_TO_KTS;
    (from, kts, w)
}

/// DCS's temperature (C) and pressure (hPa) at a point; a standard day when
/// the atmosphere API fails.
pub fn temp_pressure_at(lua: MizLua, p: V3) -> (f64, f64) {
    dcso3::atmosphere::Atmosphere::singleton(lua)
        .and_then(|a| a.get_temperature_and_pressure(LuaVec3(p)))
        .map(|t| (t.temperature_k - 273.15, t.pressure_pa / 100.))
        .unwrap_or_else(|_| {
            let t = 288.15 - 0.0065 * p.y.clamp(-500., 11000.);
            (t - 273.15, 1013.25 * (t / 288.15).powf(5.255877))
        })
}

/// Everything DCS says about the air at one point.
pub fn atmo_at(lua: MizLua, p: V3) -> AtmoLayer {
    let (wind_from_deg, wind_kts, _) = wind_at(lua, p);
    let (temp_c, pressure_hpa) = temp_pressure_at(lua, p);
    AtmoLayer { alt_m: p.y, wind_from_deg, wind_kts, temp_c, pressure_hpa }
}

/// DCS's atmosphere over (x, z) from `lo_m` to `hi_m` MSL, lowest first:
/// both ends plus evenly spaced layers at most `max_step_m` apart (and never
/// more than 32 layers).
pub fn atmo_profile(lua: MizLua, x: f64, z: f64, lo_m: f64, hi_m: f64, max_step_m: f64) -> Vec<AtmoLayer> {
    let (lo, hi) = if lo_m <= hi_m { (lo_m, hi_m) } else { (hi_m, lo_m) };
    let span = hi - lo;
    let n = ((span / max_step_m.max(1.)).ceil() as usize).clamp(1, 31);
    (0..=n)
        .map(|i| atmo_at(lua, V3::new(x, lo + span * i as f64 / n as f64, z)))
        .collect()
}

/// Mach number of a true airspeed in air of the given temperature.
pub fn mach(tas_kts: f64, temp_c: f64) -> f64 {
    let a = (1.4 * 287.053 * (temp_c + 273.15).max(150.)).sqrt();
    tas_kts / MS_TO_KTS / a
}

pub fn ground_height(lua: MizLua, p: V3) -> f64 {
    dcso3::land::Land::singleton(lua)
        .and_then(|l| l.get_height(dcso3::LuaVec2(nalgebra::Vector2::new(p.x, p.z))))
        .unwrap_or(0.)
}

/// Is the surface under `p` open water (sea or lake)?
pub fn is_water(lua: MizLua, p: V3) -> bool {
    use dcso3::land::SurfaceType;
    matches!(
        dcso3::land::Land::singleton(lua)
            .and_then(|l| l.get_surface_type(dcso3::LuaVec2(nalgebra::Vector2::new(p.x, p.z)))),
        Ok(SurfaceType::Water | SurfaceType::ShallowWater)
    )
}

/// `p` if it is dry land, else the nearest dry point found spiralling out in
/// 20 m steps up to `max_m`. A zone dropped a little too close to a river in
/// the Mission Editor still gets its targets on the bank instead of DCS
/// refusing to spawn them ("you can't spawn this unit in water").
pub fn nearest_land(lua: MizLua, p: V3, max_m: f64) -> Option<V3> {
    if !is_water(lua, p) {
        return Some(p);
    }
    let mut r = 20.;
    while r <= max_m {
        let steps = ((2. * std::f64::consts::PI * r) / 20.).ceil().max(8.) as usize;
        for i in 0..steps {
            let a = i as f64 / steps as f64 * 360.;
            let mut q = offset(p, a, r, 0.);
            if !is_water(lua, q) {
                q.y = ground_height(lua, q);
                return Some(q);
            }
        }
        r += 20.;
    }
    None
}

/// Build a Lua value from JSON, the way DCS wants mission tables: arrays
/// become 1-based sequences and object keys that are integers ("3") become
/// integer keys -- pylon tables and callsign tables are keyed by number.
/// JSON null becomes nil (never mlua's null userdata, which DCS chokes on).
pub fn json_to_lua<'l>(lua: &'l Lua, v: &serde_json::Value) -> LuaResult<Value<'l>> {
    use serde_json::Value as J;
    Ok(match v {
        J::Null => Value::Nil,
        J::Bool(b) => Value::Boolean(*b),
        J::Number(n) => match n.as_i64() {
            Some(i) => Value::Integer(i),
            None => Value::Number(n.as_f64().unwrap_or(0.)),
        },
        J::String(s) => Value::String(lua.create_string(s)?),
        J::Array(a) => {
            let t = lua.create_table_with_capacity(a.len(), 0)?;
            for (i, x) in a.iter().enumerate() {
                t.raw_set(i + 1, json_to_lua(lua, x)?)?;
            }
            Value::Table(t)
        }
        J::Object(o) => {
            let t = lua.create_table_with_capacity(0, o.len())?;
            for (k, x) in o.iter() {
                let val = json_to_lua(lua, x)?;
                match k.parse::<i64>() {
                    Ok(i) => t.raw_set(i, val)?,
                    Err(_) => t.raw_set(k.as_str(), val)?,
                }
            }
            Value::Table(t)
        }
    })
}

/// Controller:setCommand / setOption / setTask / pushTask on a group,
/// with the argument built from JSON.
pub fn group_controller_call(lua: MizLua, group: &str, method: &str, arg: serde_json::Value) -> Result<()> {
    let l = lua.inner();
    let grp_cls: LuaTable = l.globals().raw_get("Group")?;
    let g: Value = grp_cls.call_function("getByName", group)?;
    let g = match g {
        Value::Table(t) => t,
        _ => return Err(anyhow!("group {group} does not exist")),
    };
    let ctl: LuaTable = g.call_method("getController", ())?;
    let a = json_to_lua(l, &arg)?;
    ctl.call_method::<_, ()>(method, a)?;
    Ok(())
}

/// Controller:setOption(id, value) on a group.
pub fn group_set_option(lua: MizLua, group: &str, id: i64, value: serde_json::Value) -> Result<()> {
    let l = lua.inner();
    let grp_cls: LuaTable = l.globals().raw_get("Group")?;
    let g: Value = grp_cls.call_function("getByName", group)?;
    let g = match g {
        Value::Table(t) => t,
        _ => return Err(anyhow!("group {group} does not exist")),
    };
    let ctl: LuaTable = g.call_method("getController", ())?;
    ctl.call_method::<_, ()>("setOption", (id, json_to_lua(l, &value)?))?;
    Ok(())
}

/// Controller:setCommand on one UNIT (TACAN/ICLS/Link-4 are per unit).
pub fn unit_controller_call(lua: MizLua, unit: &str, method: &str, arg: serde_json::Value) -> Result<()> {
    let l = lua.inner();
    let cls: LuaTable = l.globals().raw_get("Unit")?;
    let u: Value = cls.call_function("getByName", unit)?;
    let u = match u {
        Value::Table(t) => t,
        _ => return Err(anyhow!("unit {unit} does not exist")),
    };
    let ctl: LuaTable = u.call_method("getController", ())?;
    ctl.call_method::<_, ()>(method, json_to_lua(l, &arg)?)?;
    Ok(())
}

/// The mission's own theatre, date and clock.
#[derive(Debug, Clone, Default)]
pub struct MissionClock {
    pub theatre: String,
    pub date: Option<NaiveDate>,
}

impl MissionClock {
    pub fn read(lua: MizLua) -> Self {
        let r = (|| -> Result<Self> {
            let env: LuaTable = lua.inner().globals().raw_get("env")?;
            let m: LuaTable = env.raw_get("mission")?;
            let theatre: String = m.raw_get::<_, Option<String>>("theatre")?.unwrap_or_default();
            let date = m
                .raw_get::<_, Option<LuaTable>>("date")?
                .and_then(|d| {
                    let y: i32 = d.raw_get("Year").ok()?;
                    let mo: u32 = d.raw_get("Month").ok()?;
                    let dd: u32 = d.raw_get("Day").ok()?;
                    NaiveDate::from_ymd_opt(y, mo, dd)
                });
            Ok(Self { theatre, date })
        })();
        r.unwrap_or_default()
    }

    /// (date "YYYY/M/D", time "HH:MM:SS") for an absolute mission time
    /// (`timer.getAbsTime()`, seconds since midnight of the start date).
    pub fn stamp(&self, abs_s: f64) -> (String, String) {
        let days = (abs_s / 86400.).floor() as i64;
        let tod = abs_s.rem_euclid(86400.);
        let date = self
            .date
            .map(|d| d + chrono::Duration::days(days))
            .map(|d| format!("{}/{}/{}", d.year(), d.month(), d.day()))
            .unwrap_or_default();
        let (h, m, s) = ((tod / 3600.) as u32, ((tod % 3600.) / 60.) as u32, (tod % 60.) as u32);
        (date, format!("{h:02}:{m:02}:{s:02}"))
    }

    /// Local time offset from UTC for the theatre, hours (the mission clock is
    /// local time).
    pub fn utc_offset_h(&self) -> f64 {
        match self.theatre.as_str() {
            "Caucasus" => 4.,
            "Syria" => 3.,
            "PersianGulf" => 4.,
            "MarianaIslands" => 10.,
            "Nevada" => -7.,
            "Normandy" | "TheChannel" => 2.,
            "SinaiMap" | "Sinai" => 2.,
            "Kola" => 3.,
            "Afghanistan" => 4.5,
            "Falklands" | "SouthAtlantic" => -3.,
            "Iraq" => 3.,
            "GermanyCW" => 1.,
            _ => 0.,
        }
    }

    /// Solar elevation, degrees, at lat/lon for an absolute mission time.
    /// NOAA's low-precision algorithm; plenty for "is it night".
    pub fn sun_elevation(&self, lat: f64, lon: f64, abs_s: f64) -> f64 {
        let Some(d0) = self.date else { return 45. };
        let days = (abs_s / 86400.).floor() as i64;
        let date = d0 + chrono::Duration::days(days);
        let doy = date.ordinal() as f64;
        let hours_utc = (abs_s.rem_euclid(86400.)) / 3600. - self.utc_offset_h();
        let g = 2. * std::f64::consts::PI / 365. * (doy - 1. + (hours_utc - 12.) / 24.);
        let decl = 0.006918 - 0.399912 * g.cos() + 0.070257 * g.sin() - 0.006758 * (2. * g).cos()
            + 0.000907 * (2. * g).sin()
            - 0.002697 * (3. * g).cos()
            + 0.00148 * (3. * g).sin();
        let eqtime = 229.18
            * (0.000075 + 0.001868 * g.cos() - 0.032077 * g.sin() - 0.014615 * (2. * g).cos()
                - 0.040849 * (2. * g).sin());
        let tst = hours_utc * 60. + eqtime + 4. * lon;
        let ha = (tst / 4. - 180.).to_radians();
        let latr = lat.to_radians();
        let cosz = latr.sin() * decl.sin() + latr.cos() * decl.cos() * ha.cos();
        90. - cosz.clamp(-1., 1.).acos().to_degrees()
    }

    pub fn is_night(&self, lat: f64, lon: f64, abs_s: f64) -> bool {
        self.sun_elevation(lat, lon, abs_s) < -6.
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn headings() {
        assert!((hdg(V3::new(1., 0., 0.)) - 0.).abs() < 1e-9);
        assert!((hdg(V3::new(0., 0., 1.)) - 90.).abs() < 1e-9);
        assert!((hdg(V3::new(-1., 0., 0.)) - 180.).abs() < 1e-9);
        assert!((angdiff(10., 350.) - 20.).abs() < 1e-9);
        assert!((angdiff(350., 10.) + 20.).abs() < 1e-9);
        let p = offset(V3::zeros(), 90., 100., 10.);
        assert!((p.z - 100.).abs() < 1e-9 && (p.x + 10.).abs() < 1e-9);
    }

    #[test]
    fn sun() {
        let c = MissionClock {
            theatre: "Caucasus".into(),
            date: NaiveDate::from_ymd_opt(2024, 6, 21),
        };
        // local noon in Batumi is high sun, local midnight is night
        assert!(c.sun_elevation(41.6, 41.6, 13. * 3600.) > 60.);
        assert!(c.is_night(41.6, 41.6, 1. * 3600.));
    }
}
