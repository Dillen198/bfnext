use crate::{db::Db, Context};
use anyhow::Result;
use bfprotocols::{
    cfg::UnitTag,
    db::objective::ObjectiveKind,
    stats::Stat,
};
use compact_str::format_compact;
use dcso3::{
    airbase::{Airbase, ClassAirbase},
    net::SlotId,
    object::{DcsObject as _, DcsOid},
    timer::Timer,
    LuaEnv,
    MizLua,
};
use log::error;
use mlua::prelude::*;

struct WeatherData {
    wind_from_deg: f64,
    wind_speed_kts: f64,
    qnh_inhg: f64,
    qnh_hpa: f64,
    temp_c: f64,
    cloud_base_m: f64,
    cloud_density: u8,
    visibility_m: f64,
    winds_aloft: Vec<AltitudeWind>,
}

pub struct AltitudeWind {
    pub alt_ft: u32,
    pub wind_from_deg: f64,
    pub wind_speed_kts: f64,
    pub temp_c: f64,
}

// Standard levels reported in a winds-aloft brief.
const WINDS_ALOFT_LEVELS_FT: [u32; 6] = [3000, 6000, 9000, 12000, 18000, 24000];
const M_TO_FT: f64 = 3.28084;
// DCS doesn't expose a real altitude-temperature profile via the Lua API,
// so aloft temps are the surface temp plus the standard ISA lapse rate
// (~1.98C/1000ft). Winds aloft come from atmosphere.getWind at each
// level's world Y, which DCS does model accurately.
const ISA_LAPSE_C_PER_FT: f64 = 0.00198;

fn wind_at(lua: MizLua, x: f64, y: f64, z: f64) -> Result<(f64, f64)> {
    let globals = lua.inner().globals();
    let atmosphere: LuaTable = globals.raw_get("atmosphere")?;
    let pt = lua.inner().create_table()?;
    pt.set("x", x)?;
    pt.set("y", y)?;
    pt.set("z", z)?;
    let wind: LuaTable = atmosphere.call_function("getWind", pt)?;
    let wind_x: f64 = wind.get("x")?;
    let wind_z: f64 = wind.get("z")?;
    let wind_speed_ms = (wind_x * wind_x + wind_z * wind_z).sqrt();
    let wind_speed_kts = wind_speed_ms * 1.944;
    let wind_from_deg = {
        let deg = (-wind_x).atan2(-wind_z).to_degrees();
        if deg < 0.0 { deg + 360.0 } else { deg }
    };
    Ok((wind_from_deg, wind_speed_kts))
}

fn fetch_weather(lua: MizLua, pos_x: f64, pos_z: f64) -> Result<WeatherData> {
    let globals = lua.inner().globals();

    let (wind_from_deg, wind_speed_kts) = wind_at(lua, pos_x, 0.0, pos_z)?;

    let env_tbl: LuaTable = globals.raw_get("env")?;
    let mission: LuaTable = env_tbl.raw_get("mission")?;
    let wx: LuaTable = mission.raw_get("weather")?;
    let qnh_mmhg: f64 = wx.get("qnh").unwrap_or(760.0);
    let qnh_inhg = qnh_mmhg / 25.4;
    let qnh_hpa = qnh_mmhg * 1.33322;

    let season: LuaTable = wx.raw_get("season")?;
    let temp_c: f64 = season.get("temperature").unwrap_or(15.0);

    let clouds: Option<LuaTable> = wx.raw_get("clouds").ok();
    let cloud_base_m: f64 = clouds
        .as_ref()
        .and_then(|c| c.get("base").ok())
        .unwrap_or(3000.0);
    let cloud_density: u8 = clouds
        .as_ref()
        .and_then(|c| c.get::<_, f64>("density").ok())
        .map(|d| d.round() as u8)
        .unwrap_or(0);

    // Fog / visibility
    let fog_enabled: bool = wx.get("enable_fog").unwrap_or(false);
    let visibility_m: f64 = if fog_enabled {
        wx.raw_get::<_, LuaTable>("fog")
            .ok()
            .and_then(|f| f.get::<_, f64>("visibility").ok())
            .unwrap_or(10000.0)
            .min(10000.0)
    } else {
        10000.0
    };

    let ground_elev_m = dcso3::land::Land::singleton(lua)
        .and_then(|land| land.get_height(dcso3::LuaVec2(dcso3::Vector2::new(pos_x, pos_z))))
        .unwrap_or(0.0);
    let winds_aloft = WINDS_ALOFT_LEVELS_FT
        .iter()
        .filter_map(|&alt_ft| {
            let y = ground_elev_m + alt_ft as f64 / M_TO_FT;
            let (dir, spd) = wind_at(lua, pos_x, y, pos_z).ok()?;
            Some(AltitudeWind {
                alt_ft,
                wind_from_deg: dir,
                wind_speed_kts: spd,
                temp_c: temp_c - alt_ft as f64 * ISA_LAPSE_C_PER_FT,
            })
        })
        .collect();

    Ok(WeatherData { wind_from_deg, wind_speed_kts, qnh_inhg, qnh_hpa, temp_c, cloud_base_m, cloud_density, visibility_m, winds_aloft })
}

fn active_runway(lua: MizLua, airbase_id: &DcsOid<ClassAirbase>, wind_from_deg: f64) -> Option<compact_str::CompactString> {
    let ab = Airbase::get_instance(lua, airbase_id).ok()?;
    let runways = ab.get_runways().ok()?;
    let mut best_rwy: Option<compact_str::CompactString> = None;
    let mut best_diff = f64::MAX;
    for rwy in runways {
        let rwy = rwy.ok()?;
        let course_rad = rwy.course().ok()?;
        let course_deg = course_rad.to_degrees().rem_euclid(360.0);
        for &heading in &[course_deg, (course_deg + 180.0).rem_euclid(360.0)] {
            let diff = angle_diff(wind_from_deg, heading);
            if diff < best_diff {
                best_diff = diff;
                let rwy_num = ((heading / 10.0).round() as i32).rem_euclid(36);
                let rwy_num = if rwy_num == 0 { 36 } else { rwy_num };
                best_rwy = Some(format_compact!("{:02}", rwy_num));
            }
        }
    }
    best_rwy
}

fn angle_diff(a: f64, b: f64) -> f64 {
    let diff = (a - b).rem_euclid(360.0);
    if diff > 180.0 { 360.0 - diff } else { diff }
}

fn case_advisory(cloud_base_m: f64) -> &'static str {
    if cloud_base_m < 305.0 {
        "CASE III"
    } else if cloud_base_m < 914.0 {
        "CASE II"
    } else {
        "CASE I"
    }
}

fn temp_sign(t: f64) -> &'static str {
    if t >= 0.0 { "+" } else { "" }
}

fn format_winds_aloft(winds: &[AltitudeWind]) -> compact_str::CompactString {
    use std::fmt::Write;
    let mut s = compact_str::CompactString::from("\nWinds Aloft:");
    for w in winds {
        let _ = write!(
            s,
            "\n  {alt:>5}ft: {wdir:03}°/{wspd:.0}kt {sign}{temp:.0}°C",
            alt = w.alt_ft,
            wdir = w.wind_from_deg as u32,
            wspd = w.wind_speed_kts,
            sign = temp_sign(w.temp_c),
            temp = w.temp_c,
        );
    }
    s
}

fn is_aircraft_slot(db: &Db, slot: &SlotId) -> bool {
    let sifo = match db.ephemeral.get_slot_info(slot) {
        Some(s) => s,
        None => return false,
    };
    if let Some(tags) = db.ephemeral.cfg.unit_classification.get(&sifo.typ) {
        tags.contains(UnitTag::Aircraft) || tags.contains(UnitTag::Helicopter)
    } else {
        false
    }
}

fn carrier_brc(db: &Db, kind: &ObjectiveKind) -> u32 {
    let carrier_template = match kind {
        ObjectiveKind::CarrierGroup { carrier_template, .. } => carrier_template,
        _ => return 0,
    };
    let group = db
        .persisted
        .groups
        .into_iter()
        .find(|(_, g)| g.template_name.starts_with(carrier_template.as_str()));
    let (_, group) = match group {
        Some(g) => g,
        None => return 0,
    };
    for uid in group.units.into_iter() {
        if let Some(unit) = db.persisted.units.get(uid) {
            if !unit.dead {
                let deg = unit.heading.to_degrees().rem_euclid(360.0);
                return deg.round() as u32 % 360;
            }
        }
    }
    0
}

fn send_atis(lua: MizLua, slot: SlotId, full: bool) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };

    let (oid, miz_gid) = match ctx.db.ephemeral.get_slot_info(&slot) {
        Some(s) => (s.objective, s.miz_gid),
        None => return Ok(()),
    };

    let obj = match ctx.db.persisted.objectives.get(&oid) {
        Some(o) => o,
        None => return Ok(()),
    };

    let pos = obj.pos();
    let wx = fetch_weather(lua, pos.x as f64, pos.y as f64)?;
    let aloft_str = if full { format_winds_aloft(&wx.winds_aloft) } else { compact_str::CompactString::default() };

    let mut msg: compact_str::CompactString = if obj.kind().is_carrier_group() {
        let brc_deg = carrier_brc(&ctx.db, obj.kind());
        let case = case_advisory(wx.cloud_base_m);
        format_compact!(
            "CARRIER ATIS - {name}\nBRC: {brc:03}°\nWind: {wdir:03}° at {wspd:.0}kts | Deck: {deck:.0}kts\nQNH: {inhg:.2} inHg / {hpa:.0} hPa\nTemp: {sign}{temp:.0}°C\nRecovery: {case}",
            name = obj.name(),
            brc = brc_deg,
            wdir = wx.wind_from_deg as u32,
            wspd = wx.wind_speed_kts,
            deck = wx.wind_speed_kts,
            inhg = wx.qnh_inhg,
            hpa = wx.qnh_hpa,
            sign = temp_sign(wx.temp_c),
            temp = wx.temp_c,
            case = case,
        )
    } else if obj.kind().is_airbase() {
        let rwy_str = ctx
            .db
            .ephemeral
            .get_airbase_by_oid(&oid)
            .and_then(|ab_id| active_runway(lua, ab_id, wx.wind_from_deg))
            .map(|r| format_compact!("\nActive RWY: {}", r))
            .unwrap_or_default();
        format_compact!(
            "ATIS - {name}\nWind: {wdir:03}° at {wspd:.0}kts\nQNH: {inhg:.2} inHg / {hpa:.0} hPa\nTemp: {sign}{temp:.0}°C{rwy}",
            name = obj.name(),
            wdir = wx.wind_from_deg as u32,
            wspd = wx.wind_speed_kts,
            inhg = wx.qnh_inhg,
            hpa = wx.qnh_hpa,
            sign = temp_sign(wx.temp_c),
            temp = wx.temp_c,
            rwy = rwy_str,
        )
    } else {
        return Ok(());
    };
    msg.push_str(&aloft_str);

    ctx.db.ephemeral.msgs().panel_to_group(30, false, miz_gid, msg);
    Ok(())
}

/// On-demand full weather report (surface + winds/temp aloft) for the
/// player's current slot, triggered via the `-weather` chat command.
pub fn send_full_weather(lua: MizLua, slot: SlotId) -> Result<()> {
    send_atis(lua, slot, true)
}

pub fn publish_weather(lua: MizLua, ctx: &mut Context) -> Result<()> {
    // Use map origin as reference point for dashboard weather
    let wx = fetch_weather(lua, 0.0, 0.0)?;
    ctx.db.ephemeral.stat(Stat::Weather {
        temp_c: wx.temp_c,
        wind_speed_kts: wx.wind_speed_kts,
        wind_from_deg: wx.wind_from_deg,
        cloud_base_m: wx.cloud_base_m,
        qnh_hpa: wx.qnh_hpa,
        cloud_density: Some(wx.cloud_density),
        visibility_m: Some(wx.visibility_m),
    });
    Ok(())
}

pub fn schedule_atis(lua: MizLua, slot: SlotId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    if !is_aircraft_slot(&ctx.db, &slot) {
        return Ok(());
    }
    let timer = Timer::singleton(lua)?;
    let when = timer.get_time()? + 15.0;
    timer.schedule_function(when, slot, move |lua, slot, _| {
        if let Err(e) = send_atis(lua, slot, false) {
            error!("atis send failed: {:?}", e);
        }
        Ok(None)
    })?;
    Ok(())
}
