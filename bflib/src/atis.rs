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
    cloud_preset: Option<compact_str::CompactString>,
    precip: bool,
    visibility_m: f64,
    ground_elev_m: f64,
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
    // DCS world frame: X = North, Z = East. getWind returns the velocity vector,
    // i.e. the direction the air is moving TOWARD. Meteorological wind direction
    // is the compass bearing it comes FROM: atan2(East, North) of the reversed
    // vector.
    let wind_x: f64 = wind.get("x")?; // north component
    let wind_z: f64 = wind.get("z")?; // east component
    let wind_speed_ms = (wind_x * wind_x + wind_z * wind_z).sqrt();
    let wind_speed_kts = wind_speed_ms * 1.944;
    let wind_from_deg = (-wind_z).atan2(-wind_x).to_degrees().rem_euclid(360.0);
    Ok((wind_from_deg, wind_speed_kts))
}

fn fetch_weather(lua: MizLua, pos_x: f64, pos_z: f64) -> Result<WeatherData> {
    let globals = lua.inner().globals();

    // Ground elevation at this point -- DCS's y coordinate is height above
    // the map's sea-level datum, not height-above-ground, so querying wind
    // at a hardcoded y=0.0 asks for wind at sea level. Anywhere the terrain
    // itself sits above sea level, that point is underground, and
    // atmosphere.getWind() returns a zero vector there instead of the
    // configured surface wind. Query at actual ground elevation instead.
    let ground_elev_m = dcso3::land::Land::singleton(lua)
        .and_then(|land| land.get_height(dcso3::LuaVec2(dcso3::Vector2::new(pos_x, pos_z))))
        .unwrap_or(0.0);
    let (wind_from_deg, wind_speed_kts) = wind_at(lua, pos_x, ground_elev_m, pos_z)?;

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
    let cloud_preset: Option<compact_str::CompactString> = clouds
        .as_ref()
        .and_then(|c| c.get::<_, std::string::String>("preset").ok())
        .map(|s| compact_str::CompactString::from(s.as_str()))
        .filter(|s| !s.is_empty());
    let precip: bool = clouds
        .as_ref()
        .and_then(|c| c.get::<_, f64>("iprecptns").ok())
        .map(|p| p > 0.0)
        .unwrap_or(false)
        || cloud_preset.as_deref().is_some_and(|p| p.starts_with("Rainy"));

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

    Ok(WeatherData {
        wind_from_deg,
        wind_speed_kts,
        qnh_inhg,
        qnh_hpa,
        temp_c,
        cloud_base_m,
        cloud_density,
        cloud_preset,
        precip,
        visibility_m,
        ground_elev_m,
        winds_aloft,
    })
}

/// Designator number 01-36 for a heading in degrees.
fn rwy_num_for(heading: f64) -> i32 {
    let n = ((heading / 10.0).round() as i32).rem_euclid(36);
    if n == 0 { 36 } else { n }
}

/// Leading digits of a runway-name part, e.g. "31R" -> 31, "09" -> 9.
fn part_num(part: &str) -> Option<i32> {
    part.trim_matches(|c: char| !c.is_ascii_digit())
        .parse()
        .ok()
        .filter(|n| (1..=36).contains(n))
}

/// Pick the runway end best aligned with the wind, reported with the real DCS
/// designator (so it can't name a runway the airfield doesn't have). Falls back
/// to a heading-derived number only when DCS gives no usable name.
fn active_runway(
    lua: MizLua,
    airbase_id: &DcsOid<ClassAirbase>,
    wind_from_deg: f64,
    wind_speed_kts: f64,
) -> Option<compact_str::CompactString> {
    let ab = Airbase::get_instance(lua, airbase_id).ok()?;
    let ab_name = ab
        .as_object()
        .and_then(|o| o.get_name())
        .map(|n| n.to_string())
        .unwrap_or_default();
    let ab_callsign = ab.get_callsign().map(|c| c.to_string()).unwrap_or_default();
    let runways = ab.get_runways().ok()?;
    // Each candidate end: (heading_deg, designator, course_aligned). The
    // `course_aligned` flag marks the end pointing the same way as DCS's own
    // `course` field for that runway — its "primary" direction, used as the
    // calm-wind tie-break.
    let mut ends: Vec<(f64, compact_str::CompactString, bool)> = Vec::new();
    for rwy in runways {
        let Ok(rwy) = rwy else { continue };
        let Ok(course) = rwy.course() else { continue };
        let raw_name = rwy.name().ok();
        let rwy_pos = rwy.position().ok();
        let c1 = course.to_degrees().rem_euclid(360.0);
        let parts: Vec<compact_str::CompactString> = raw_name
            .as_deref()
            .map(|n| {
                n.split(['-', '/', ' '])
                    .map(|s| s.trim())
                    .filter(|s| !s.is_empty() && part_num(s).is_some())
                    .map(compact_str::CompactString::from)
                    .collect()
            })
            .unwrap_or_default();
        log::info!(
            "[ATIS_RWY] {ab_name} (cs {ab_callsign}): runway name={raw_name:?} \
             course={course:.4}rad ({c1:.0}deg) pos={rwy_pos:?} parsed_parts={parts:?}"
        );
        // Designators (the number) come from DCS's runway name when it has one;
        // each maps to ~num*10 deg. Only fall back to the raw course heading when
        // there is no usable name.
        let named: Vec<(f64, compact_str::CompactString)> = if parts.len() == 2 {
            parts
                .iter()
                .filter_map(|p| part_num(p).map(|n| (n as f64 * 10.0, p.clone())))
                .collect()
        } else if parts.len() == 1 {
            let n = part_num(&parts[0]).unwrap();
            let recip = ((n + 18 - 1) % 36) + 1;
            vec![
                (n as f64 * 10.0, parts[0].clone()),
                (recip as f64 * 10.0, format_compact!("{recip:02}")),
            ]
        } else {
            [c1, (c1 + 180.0).rem_euclid(360.0)]
                .into_iter()
                .map(|h| (h, format_compact!("{:02}", rwy_num_for(h))))
                .collect()
        };
        for (h, label) in named {
            ends.push((h, label, angle_diff(h, c1) <= 90.0));
        }
    }
    // Wind ≥ 3 kt: land into it. Calm: use the runway's own primary (course-
    // aligned) direction, then the lower-numbered end as a final tie-break.
    let calm = wind_speed_kts < 3.0;
    let best = ends.iter().min_by(|a, b| {
        if calm {
            b.2.cmp(&a.2).then(a.0.total_cmp(&b.0))
        } else {
            angle_diff(wind_from_deg, a.0).total_cmp(&angle_diff(wind_from_deg, b.0))
        }
    });
    log::info!(
        "[ATIS_RWY] {ab_name}: wind {wind_from_deg:.0}deg/{wind_speed_kts:.0}kt (calm={calm}) \
         -> active {:?}",
        best.map(|(_, l, _)| l)
    );
    best.map(|(_, l, _)| l.clone())
}

fn angle_diff(a: f64, b: f64) -> f64 {
    let diff = (a - b).rem_euclid(360.0);
    if diff > 180.0 { 360.0 - diff } else { diff }
}

/// Cloud layer line: coverage + base AGL. DCS 2.9 preset weather usually
/// reports density 0 even with a solid overcast, so fall back to the preset
/// name when we have one.
fn clouds_line(wx: &WeatherData) -> compact_str::CompactString {
    let base_agl_m = (wx.cloud_base_m - wx.ground_elev_m).max(0.0);
    let base_agl_ft = (base_agl_m * M_TO_FT).round() as i64;
    let base_agl_m = base_agl_m.round() as i64;
    let cover = match wx.cloud_density {
        0 => None,
        1..=2 => Some("FEW"),
        3..=4 => Some("SCT"),
        5..=7 => Some("BKN"),
        _ => Some("OVC"),
    };
    match (cover, wx.cloud_preset.as_deref()) {
        (Some(c), _) => format_compact!("\nClouds: {c} {base_agl_ft}ft / {base_agl_m}m AGL"),
        (None, Some(p)) => format_compact!("\nClouds: {p} @ {base_agl_ft}ft / {base_agl_m}m AGL"),
        (None, None) => compact_str::CompactString::from("\nClouds: SKC"),
    }
}

fn visibility_line(vis_m: f64) -> compact_str::CompactString {
    let sm = vis_m / 1609.344;
    if vis_m >= 10000.0 {
        compact_str::CompactString::from("\nVisibility: 10km+ / 6SM+")
    } else if vis_m >= 1000.0 {
        format_compact!("\nVisibility: {:.1}km / {sm:.1}SM", vis_m / 1000.0)
    } else {
        format_compact!("\nVisibility: {:.0}m / {sm:.1}SM", vis_m)
    }
}

/// QFE (pressure at field elevation) from QNH via the ISA barometric formula.
fn qfe(qnh_hpa: f64, elev_m: f64) -> (f64, f64) {
    let hpa = qnh_hpa * (1.0 - 0.0065 * elev_m / 288.15).powf(5.25588);
    (hpa, hpa / 33.8639)
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

fn c_to_f(c: f64) -> f64 {
    c * 9.0 / 5.0 + 32.0
}

/// "+18°C (64°F)"
fn temp_both(c: f64) -> compact_str::CompactString {
    let f = c_to_f(c);
    format_compact!("{}{:.0}°C ({}{:.0}°F)", temp_sign(c), c, temp_sign(f), f)
}

/// "13kt (7m/s)"
fn wind_speed_both(kts: f64) -> compact_str::CompactString {
    format_compact!("{:.0}kt ({:.0}m/s)", kts, kts * 0.514444)
}

fn format_winds_aloft(winds: &[AltitudeWind]) -> compact_str::CompactString {
    use std::fmt::Write;
    let mut s = compact_str::CompactString::from("\nWinds Aloft:");
    for w in winds {
        let alt_m = (w.alt_ft as f64 / M_TO_FT).round() as u32;
        let _ = write!(
            s,
            "\n  {alt:>5}ft/{alt_m}m: {wdir:03}°/{wspd} {temp}",
            alt = w.alt_ft,
            wdir = w.wind_from_deg as u32,
            wspd = wind_speed_both(w.wind_speed_kts),
            temp = temp_both(w.temp_c),
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
    let obj_name = obj.name().to_string();
    let wx = fetch_weather(lua, pos.x as f64, pos.y as f64)?;
    // Diagnostic: compare our computed FROM bearing with the .miz authored wind
    // (which stores the TOWARD direction). computed_from should ~= authored + 180.
    if let Ok(authored) = lua
        .inner()
        .globals()
        .raw_get::<_, LuaTable>("env")
        .and_then(|e| e.raw_get::<_, LuaTable>("mission"))
        .and_then(|m| m.raw_get::<_, LuaTable>("weather"))
        .and_then(|w| w.raw_get::<_, LuaTable>("wind"))
        .and_then(|w| w.raw_get::<_, LuaTable>("atGround"))
    {
        let a_dir: f64 = authored.get("dir").unwrap_or(-1.0);
        let a_spd: f64 = authored.get("speed").unwrap_or(-1.0);
        log::info!(
            "[ATIS_WIND] {obj_name}: authored atGround dir(TOWARD)={a_dir:.0} speed={a_spd:.1}m/s \
             -> expected FROM={:.0}; computed FROM={:.0} speed={:.1}kt",
            (a_dir + 180.0).rem_euclid(360.0),
            wx.wind_from_deg,
            wx.wind_speed_kts,
        );
    }
    let aloft_str = if full { format_winds_aloft(&wx.winds_aloft) } else { compact_str::CompactString::default() };

    let mut msg: compact_str::CompactString = if obj.kind().is_carrier_group() {
        let brc_deg = carrier_brc(&ctx.db, obj.kind());
        let case = case_advisory(wx.cloud_base_m);
        format_compact!(
            "CARRIER ATIS - {name}\nBRC: {brc:03}°\nWind: {wdir:03}° at {wind} | Deck: {wind}\n\
             QNH: {inhg:.2} inHg / {hpa:.0} hPa / {mmhg:.0} mmHg\nTemp: {temp}\nRecovery: {case}",
            name = obj.name(),
            brc = brc_deg,
            wdir = wx.wind_from_deg as u32,
            wind = wind_speed_both(wx.wind_speed_kts),
            inhg = wx.qnh_inhg,
            hpa = wx.qnh_hpa,
            mmhg = wx.qnh_hpa / 1.33322,
            temp = temp_both(wx.temp_c),
            case = case,
        )
    } else if obj.kind().is_airbase() {
        let rwy_str = ctx
            .db
            .ephemeral
            .get_airbase_by_oid(&oid)
            .and_then(|ab_id| active_runway(lua, ab_id, wx.wind_from_deg, wx.wind_speed_kts))
            .map(|r| format_compact!("\nActive RWY: {}", r))
            .unwrap_or_default();
        let elev_ft = (wx.ground_elev_m * M_TO_FT).round() as i64;
        let elev_m = wx.ground_elev_m.round() as i64;
        let (qfe_hpa, qfe_inhg) = qfe(wx.qnh_hpa, wx.ground_elev_m);
        format_compact!(
            "ATIS - {name}\nField elev: {elev_ft}ft / {elev_m}m{rwy}\nWind: {wdir:03}° at {wind}\n\
             QNH: {inhg:.2} inHg / {hpa:.0} hPa / {mmhg:.0} mmHg\n\
             QFE: {qfe_inhg:.2} inHg / {qfe_hpa:.0} hPa / {qfe_mmhg:.0} mmHg\n\
             Temp: {temp}{clouds}{vis}{precip}",
            name = obj.name(),
            wdir = wx.wind_from_deg as u32,
            wind = wind_speed_both(wx.wind_speed_kts),
            inhg = wx.qnh_inhg,
            hpa = wx.qnh_hpa,
            mmhg = wx.qnh_hpa / 1.33322,
            qfe_hpa = qfe_hpa,
            qfe_inhg = qfe_inhg,
            qfe_mmhg = qfe_hpa / 1.33322,
            temp = temp_both(wx.temp_c),
            clouds = clouds_line(&wx),
            vis = visibility_line(wx.visibility_m),
            precip = if wx.precip { "\nPrecipitation: yes" } else { "" },
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
