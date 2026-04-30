use crate::{db::Db, Context};
use anyhow::Result;
use bfprotocols::{
    cfg::UnitTag,
    db::objective::ObjectiveKind,
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
}

fn fetch_weather(lua: MizLua, pos_x: f64, pos_z: f64) -> Result<WeatherData> {
    let globals = lua.inner().globals();

    let atmosphere: LuaTable = globals.raw_get("atmosphere")?;
    let pt = lua.inner().create_table()?;
    pt.set("x", pos_x)?;
    pt.set("y", 0.0_f64)?;
    pt.set("z", pos_z)?;
    let wind: LuaTable = atmosphere.call_function("getWind", pt)?;
    let wind_x: f64 = wind.get("x")?;
    let wind_z: f64 = wind.get("z")?;
    let wind_speed_ms = (wind_x * wind_x + wind_z * wind_z).sqrt();
    let wind_speed_kts = wind_speed_ms * 1.944;
    let wind_from_deg = {
        let deg = (-wind_x).atan2(-wind_z).to_degrees();
        if deg < 0.0 { deg + 360.0 } else { deg }
    };

    let env_tbl: LuaTable = globals.raw_get("env")?;
    let mission: LuaTable = env_tbl.raw_get("mission")?;
    let wx: LuaTable = mission.raw_get("weather")?;
    let qnh_mmhg: f64 = wx.get("qnh").unwrap_or(760.0);
    let qnh_inhg = qnh_mmhg / 25.4;
    let qnh_hpa = qnh_mmhg * 1.33322;

    let season: LuaTable = wx.raw_get("season")?;
    let temp_c: f64 = season.get("temperature").unwrap_or(15.0);

    let cloud_base_m: f64 = wx
        .raw_get::<_, LuaTable>("clouds")
        .ok()
        .and_then(|c: LuaTable| c.get("base").ok())
        .unwrap_or(3000.0);

    Ok(WeatherData { wind_from_deg, wind_speed_kts, qnh_inhg, qnh_hpa, temp_c, cloud_base_m })
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

fn send_atis(lua: MizLua, slot: SlotId) -> Result<()> {
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

    let msg: compact_str::CompactString = if obj.kind().is_carrier_group() {
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

    ctx.db.ephemeral.msgs().panel_to_group(30, false, miz_gid, msg);
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
        if let Err(e) = send_atis(lua, slot) {
            error!("atis send failed: {:?}", e);
        }
        Ok(None)
    })?;
    Ok(())
}
