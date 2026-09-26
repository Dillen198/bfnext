use crate::{db::Db, Context};
use anyhow::{bail, Result};
use bfprotocols::{
    cfg::UnitTag,
    db::objective::ObjectiveKind,
    stats::Stat,
};
use compact_str::format_compact;
use dcso3::{
    airbase::{Airbase, ClassAirbase},
    env::miz::GroupId,
    net::SlotId,
    object::{DcsObject as _, DcsOid},
    timer::Timer,
    HooksLua,
    LuaEnv,
    MizLua,
    Vector2,
};
use log::{error, info};
use mlua::prelude::*;
use std::{
    collections::HashMap,
    fmt::Write as _,
    sync::{OnceLock, RwLock},
};

// The ATIS is built to agree, number for number, with DCSServerBot's
// `-atis` / slot-entry ATIS (plugins/mission/lua/commands.lua getWeatherInfo +
// getAirbases, rendered by plugins/mission/atis.py), because players get both
// and any disagreement reads as "the engine ATIS is wrong":
//
// - temperature and QFE are DCS's own atmosphere model sampled AT THE FIELD
//   (atmosphere.getTemperatureAndPressure at the airdrome reference point and
//   terrain height), not the mission's sea-level season temperature / an ISA
//   reduction of the mission QNH;
// - QNH is that QFE plus elevation * 0.12017 hPa/m, the bot's reduction;
// - the surface wind in static weather is the mission's `atGround` wind
//   (what DCS's own briefing and the bot's Weather.getGroundWindAtPoint
//   report), not atmosphere.getWind at some height above the field -- that
//   blends in the 2000 m layer over high terrain and disagreed by 20-30 deg;
// - clouds are the mission's layer as authored (base MSL + thickness), or the
//   preset's METAR text from Config/Effects/clouds.lua;
// - visibility is the mission visibility, overridden by the live fog distance;
// - code, position, MGRS, tower frequencies, runway names and runway heading
//   come from the terrain's airdrome table, which only the hooks lua state can
//   read, so it is harvested there at mission load (`harvest_airdromes`).

pub(crate) struct WeatherData {
    pub(crate) wind_from_deg: f64,
    pub(crate) wind_speed_kts: f64,
    wind_speed_ms: f64,
    /// true when the surface wind was sampled live (dynamic weather); false
    /// when it is the mission's authored `atGround` wind (static weather).
    wind_live: bool,
    pub(crate) qnh_inhg: f64,
    pub(crate) qnh_hpa: f64,
    qfe_hpa: f64,
    pub(crate) temp_c: f64,
    /// Cloud base, metres MSL, as the mission authors it.
    pub(crate) cloud_base_m: f64,
    cloud_thickness_m: Option<f64>,
    pub(crate) cloud_density: u8,
    cloud_preset: Option<compact_str::CompactString>,
    has_cloud_table: bool,
    pub(crate) precip: bool,
    pub(crate) visibility_m: f64,
    ground_elev_m: f64,
    winds_aloft: Vec<AltitudeWind>,
}

impl WeatherData {
    fn has_clouds(&self) -> bool {
        self.cloud_preset.is_some() || self.cloud_density > 0
    }
}

pub struct AltitudeWind {
    pub alt_ft: u32,
    pub wind_from_deg: f64,
    pub wind_speed_kts: f64,
    pub temp_c: f64,
}

// Standard levels reported in a winds-aloft brief, feet MSL.
const WINDS_ALOFT_LEVELS_FT: [u32; 6] = [3000, 6000, 9000, 12000, 18000, 24000];
const M_TO_FT: f64 = 3.28084;
const MS_TO_KTS: f64 = 1.94384;
const HPA_TO_INHG: f64 = 0.0295300586467;
const HPA_TO_MMHG: f64 = 0.7500637554192;
const MMHG_TO_HPA: f64 = 1.33322;
const M_PER_SM: f64 = 1609.344;
/// DCSServerBot's QFE -> QNH reduction, hPa per metre of field elevation.
const QFE_TO_QNH_HPA_PER_M: f64 = 0.12017;
/// Fallback only, when DCS's own temperature at a level can't be read.
const ISA_LAPSE_C_PER_M: f64 = 0.0065;
// Only used when the weather is dynamic and there is no authored ground wind:
// the standard surface-wind reference height, which also keeps the getWind
// query clear of the terrain mesh (a point at or below ground returns zero).
const SURFACE_WIND_AGL_M: f64 = 10.0;
/// How far an objective may sit from the terrain's airdrome reference point
/// and still be treated as that airdrome.
const AIRDROME_MATCH_M: f64 = 10_000.0;

/// Wind at a world point: (meteorological FROM bearing, true; speed m/s).
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
    let wind_from_deg = (-wind_z).atan2(-wind_x).to_degrees().rem_euclid(360.0);
    Ok((wind_from_deg, wind_speed_ms))
}

/// DCS's own temperature (C) and pressure (Pa) at a world point.
fn temp_pressure_at(lua: MizLua, x: f64, y: f64, z: f64) -> Result<(f64, f64)> {
    let atmosphere: LuaTable = lua.inner().globals().raw_get("atmosphere")?;
    let pt = lua.inner().create_table()?;
    pt.set("x", x)?;
    pt.set("y", y)?;
    pt.set("z", z)?;
    let (t, p): (f64, f64) = atmosphere.call_function("getTemperatureAndPressure", pt)?;
    if !(p > 0.0) {
        bail!("getTemperatureAndPressure returned no pressure ({p})");
    }
    // The mission API documents Kelvin; the gui API returns Celsius. Accept
    // either rather than report a 296 degree day.
    let t_c = if t > 150.0 { t - 273.15 } else { t };
    Ok((t_c, p))
}

/// Live fog visibility, metres; 0 when there is no fog.
fn fog_visibility_m(lua: MizLua) -> Option<f64> {
    let world: LuaTable = lua.inner().globals().raw_get("world").ok()?;
    let weather: LuaTable = world.raw_get("weather").ok()?;
    weather
        .call_function::<_, f64>("getFogVisibilityDistance", ())
        .ok()
}

pub(crate) fn fetch_weather(lua: MizLua, pos_x: f64, pos_z: f64) -> Result<WeatherData> {
    let globals = lua.inner().globals();

    // Terrain height at the point. For an airfield this is called with the
    // airdrome reference point, so it is the same field elevation the bot
    // reports (Terrain.GetHeight at that point).
    let ground_elev_m = dcso3::land::Land::singleton(lua)
        .and_then(|land| land.get_height(dcso3::LuaVec2(dcso3::Vector2::new(pos_x, pos_z))))
        .unwrap_or(0.0);

    let env_tbl: LuaTable = globals.raw_get("env")?;
    let mission: LuaTable = env_tbl.raw_get("mission")?;
    let wx: LuaTable = mission.raw_get("weather")?;

    // Surface wind. Static weather (atmosphere_type 0): the authored ground
    // wind, whose `dir` is the direction the air blows TOWARD, so FROM is
    // dir + 180 -- exactly what DCS's briefing and the bot show. Dynamic
    // weather has no meaningful authored value, so sample DCS live.
    let static_atmo = wx
        .get::<_, Option<f64>>("atmosphere_type")
        .ok()
        .flatten()
        .unwrap_or(0.0)
        == 0.0;
    let authored_ground = if static_atmo {
        wx.get::<_, LuaTable>("wind")
            .and_then(|w| w.get::<_, LuaTable>("atGround"))
            .ok()
            .and_then(|g| Some((g.get::<_, f64>("dir").ok()?, g.get::<_, f64>("speed").ok()?)))
    } else {
        None
    };
    let (wind_from_deg, wind_speed_ms, wind_live) = match authored_ground {
        Some((dir, spd)) => ((dir + 180.0).rem_euclid(360.0), spd.max(0.0), false),
        None => {
            let (d, s) = wind_at(lua, pos_x, ground_elev_m + SURFACE_WIND_AGL_M, pos_z)?;
            (d, s, true)
        }
    };

    // Temperature and pressure at the field, from DCS's atmosphere.
    let qnh_mmhg_miz: f64 = wx.get("qnh").unwrap_or(760.0);
    let season_temp_c: f64 = wx
        .get::<_, LuaTable>("season")
        .and_then(|s| s.get("temperature"))
        .unwrap_or(15.0);
    let (temp_c, qfe_hpa) = match temp_pressure_at(lua, pos_x, ground_elev_m, pos_z) {
        Ok((t_c, p_pa)) => (t_c, p_pa / 100.0),
        Err(e) => {
            log::debug!("[ATIS] getTemperatureAndPressure failed, using ISA from the mission: {e:?}");
            let qnh = qnh_mmhg_miz * MMHG_TO_HPA;
            (
                season_temp_c - ISA_LAPSE_C_PER_M * ground_elev_m,
                qnh * (1.0 - 0.0065 * ground_elev_m / 288.15).powf(5.25588),
            )
        }
    };
    let qnh_hpa = qfe_hpa + ground_elev_m * QFE_TO_QNH_HPA_PER_M;
    let qnh_inhg = qnh_hpa * HPA_TO_INHG;

    let clouds: Option<LuaTable> = wx.raw_get::<_, Option<LuaTable>>("clouds").ok().flatten();
    let has_cloud_table = clouds.is_some();
    let cloud_base_m: f64 = clouds
        .as_ref()
        .and_then(|c| c.get("base").ok())
        .unwrap_or(3000.0);
    let cloud_thickness_m: Option<f64> = clouds.as_ref().and_then(|c| c.get("thickness").ok());
    let cloud_density: u8 = clouds
        .as_ref()
        .and_then(|c| c.get::<_, f64>("density").ok())
        .map(|d| d.round().clamp(0.0, 10.0) as u8)
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

    // Visibility: the mission's, unless fog is up (the bot does the same).
    let mut visibility_m: f64 = wx
        .get::<_, LuaTable>("visibility")
        .and_then(|v| v.get("distance"))
        .unwrap_or(80_000.0);
    match fog_visibility_m(lua) {
        Some(v) if v > 0.0 => visibility_m = v,
        Some(_) => (),
        None => {
            if wx.get::<_, bool>("enable_fog").unwrap_or(false) {
                if let Some(v) = wx
                    .raw_get::<_, LuaTable>("fog")
                    .ok()
                    .and_then(|f| f.get::<_, f64>("visibility").ok())
                    .filter(|v| *v > 0.0)
                {
                    visibility_m = visibility_m.min(v);
                }
            }
        }
    }

    // Winds and temperatures aloft at standard MSL levels, both live from
    // DCS. Levels at or below the field are skipped.
    let winds_aloft = WINDS_ALOFT_LEVELS_FT
        .iter()
        .filter_map(|&alt_ft| {
            let y = alt_ft as f64 / M_TO_FT;
            if y < ground_elev_m + 150.0 {
                return None;
            }
            let (dir, spd_ms) = wind_at(lua, pos_x, y, pos_z).ok()?;
            let t = temp_pressure_at(lua, pos_x, y, pos_z)
                .map(|(t, _)| t)
                .unwrap_or(temp_c - (y - ground_elev_m) * ISA_LAPSE_C_PER_M);
            Some(AltitudeWind {
                alt_ft,
                wind_from_deg: dir,
                wind_speed_kts: spd_ms * MS_TO_KTS,
                temp_c: t,
            })
        })
        .collect();

    Ok(WeatherData {
        wind_from_deg,
        wind_speed_kts: wind_speed_ms * MS_TO_KTS,
        wind_speed_ms,
        wind_live,
        qnh_inhg,
        qnh_hpa,
        qfe_hpa,
        temp_c,
        cloud_base_m,
        cloud_thickness_m,
        cloud_density,
        cloud_preset,
        has_cloud_table,
        precip,
        visibility_m,
        ground_elev_m,
        winds_aloft,
    })
}

// ── terrain airdrome table (harvested in the hooks state) ─────────────────

#[derive(Debug, Clone, Default)]
pub(crate) struct Airdrome {
    pub(crate) name: std::string::String,
    pub(crate) code: Option<std::string::String>,
    /// Reference point, DCS world x / z.
    pub(crate) x: f64,
    pub(crate) z: f64,
    pub(crate) alt_m: f64,
    pub(crate) lat: Option<f64>,
    pub(crate) lon: Option<f64>,
    pub(crate) mgrs: Option<std::string::String>,
    pub(crate) freqs_hz: Vec<f64>,
    /// Runway-end designators as the terrain names them, e.g. ["23L", "05R"].
    pub(crate) runways: Vec<std::string::String>,
    /// True heading of the main runway (Terrain.getRunwayHeading), degrees.
    pub(crate) rwy_heading_deg: Option<f64>,
    /// The bot's own figures for this field from DCS's Mission-Editor weather
    /// model: (temperature C, QFE hPa, wind from deg, wind m/s).
    pub(crate) me_wx: Option<(f64, f64, f64, f64)>,
}

static AIRDROMES: RwLock<Vec<Airdrome>> = RwLock::new(Vec::new());

// Mirrors DCSServerBot's dcsbot.getAirbases (and DCS's own AirdromeData.lua).
const HARVEST_AIRDROMES_LUA: &str = r#"
local okT, Terrain = pcall(require, 'terrain')
if not okT or not Terrain then return nil end
local airdromes = Terrain.GetTerrainConfig('Airdromes')
if not airdromes then return nil end
local sim = DCS or Sim
-- The bot's weather numbers come from DCS's Mission-Editor weather model
-- (Weather.getTemperatureAndPressureAtPoint / getGroundWindAtPoint), run in
-- this hooks state against the mission's weather table -- not from the live
-- mission-state atmosphere. Sample the same model here so the two agree.
local okW, Weather = pcall(require, 'Weather')
local cur = sim and sim.getCurrentMission and sim.getCurrentMission()
local mwx = cur and cur.mission and cur.mission.weather
local wxok = okW and Weather and mwx and pcall(Weather.initAtmospere, mwx)
local function addFreq(list, f)
    if type(f) == 'table' then f = f[1] end
    if type(f) == 'number' then list[#list + 1] = f end
end
local out = {}
for id, a in pairs(airdromes) do
    if a.reference_point and a.abandoned ~= true then
        pcall(function()
            local r = {}
            r.code = a.code
            r.name = a.display_name or (a.names and a.names.en) or tostring(id)
            r.x = a.reference_point.x
            r.z = a.reference_point.y
            r.alt = Terrain.GetHeight(r.x, r.z)
            if wxok then
                local pos = { x = r.x, y = r.alt, z = r.z }
                local okP, t, p = pcall(Weather.getTemperatureAndPressureAtPoint, { position = pos })
                if okP and type(t) == 'number' and type(p) == 'number' then
                    r.me_temp = t
                    r.me_qfe = p / 100
                end
                local okG, w = pcall(Weather.getGroundWindAtPoint, { position = pos })
                if okG and type(w) == 'table' and type(w.v) == 'number' and type(w.a) == 'number' then
                    r.me_wind_ms = w.v
                    r.me_wind_from = math.deg(w.a + math.pi) % 360
                end
            end
            local okL, lat, lon = pcall(Terrain.convertMetersToLatLon, r.x, r.z)
            if okL then r.lat = lat; r.lon = lon end
            local okM, mgrs = pcall(Terrain.GetMGRScoordinates, r.x, r.z)
            if okM and type(mgrs) == 'string' then r.mgrs = mgrs end
            local freqs = {}
            if a.frequency then
                for _, f in pairs(a.frequency) do addFreq(freqs, f) end
            elseif a.radio and sim and sim.getATCradiosData then
                for _, radioId in pairs(a.radio) do
                    local fs = sim.getATCradiosData(radioId)
                    if fs then for _, f in pairs(fs) do addFreq(freqs, f) end end
                end
            end
            r.freqs = freqs
            local rw = {}
            if a.runwayName then
                for _, n in pairs(a.runwayName) do rw[#rw + 1] = tostring(n) end
            end
            r.runways = rw
            if a.roadnet and Terrain.getRunwayHeading then
                local okH, h = pcall(Terrain.getRunwayHeading, a.roadnet)
                if okH and type(h) == 'number' then r.heading = h * 180 / math.pi end
            end
            out[#out + 1] = r
        end)
    end
end
return out
"#;

/// Read the terrain's airdrome table (code, position, MGRS, tower
/// frequencies, runway names/heading). Only the hooks lua state has
/// `require('terrain')`, so this runs from onMissionLoadEnd and the result is
/// shared with the mission state through a static.
pub fn harvest_airdromes(lua: HooksLua) -> Result<()> {
    let v: LuaValue = lua
        .inner()
        .load(HARVEST_AIRDROMES_LUA)
        .set_name("bflib_atis_airdromes")
        .call(())?;
    let LuaValue::Table(tbl) = v else {
        bail!("the terrain airdrome table is not available in this lua state")
    };
    let mut out: Vec<Airdrome> = vec![];
    for r in tbl.sequence_values::<LuaTable>() {
        let Ok(r) = r else { continue };
        let seq_f64 = |k: &str| -> Vec<f64> {
            r.get::<_, Option<LuaTable>>(k)
                .ok()
                .flatten()
                .map(|t| t.sequence_values::<f64>().filter_map(|v| v.ok()).collect())
                .unwrap_or_default()
        };
        let freqs_hz = seq_f64("freqs");
        let runways: Vec<std::string::String> = r
            .get::<_, Option<LuaTable>>("runways")
            .ok()
            .flatten()
            .map(|t| {
                t.sequence_values::<std::string::String>()
                    .filter_map(|v| v.ok())
                    .collect()
            })
            .unwrap_or_default();
        let (Ok(x), Ok(z)) = (r.get::<_, f64>("x"), r.get::<_, f64>("z")) else {
            continue;
        };
        out.push(Airdrome {
            name: r.get::<_, Option<std::string::String>>("name").ok().flatten().unwrap_or_default(),
            code: r
                .get::<_, Option<std::string::String>>("code")
                .ok()
                .flatten()
                .filter(|c| !c.is_empty()),
            x,
            z,
            alt_m: r.get::<_, f64>("alt").unwrap_or(0.0),
            lat: r.get::<_, Option<f64>>("lat").ok().flatten(),
            lon: r.get::<_, Option<f64>>("lon").ok().flatten(),
            mgrs: r
                .get::<_, Option<std::string::String>>("mgrs")
                .ok()
                .flatten()
                .filter(|m| !m.is_empty()),
            freqs_hz,
            runways,
            rwy_heading_deg: r
                .get::<_, Option<f64>>("heading")
                .ok()
                .flatten()
                .map(|h| h.rem_euclid(360.0)),
            me_wx: (|| {
                Some((
                    r.get::<_, Option<f64>>("me_temp").ok()??,
                    r.get::<_, Option<f64>>("me_qfe").ok()??,
                    r.get::<_, Option<f64>>("me_wind_from").ok()??,
                    r.get::<_, Option<f64>>("me_wind_ms").ok()??,
                ))
            })(),
        });
    }
    info!(
        "[ATIS] harvested {} airdromes from the terrain ({} with Mission-Editor weather)",
        out.len(),
        out.iter().filter(|a| a.me_wx.is_some()).count()
    );
    *AIRDROMES.write().unwrap_or_else(|e| e.into_inner()) = out;
    Ok(())
}

/// The terrain airdrome whose reference point is nearest `(x, z)`.
fn nearest_airdrome(x: f64, z: f64) -> Option<Airdrome> {
    let ads = AIRDROMES.read().unwrap_or_else(|e| e.into_inner());
    ads.iter()
        .map(|a| (((a.x - x).powi(2) + (a.z - z).powi(2)).sqrt(), a))
        .filter(|(d, _)| *d <= AIRDROME_MATCH_M)
        .min_by(|a, b| a.0.total_cmp(&b.0))
        .map(|(_, a)| a.clone())
}

// ── cloud presets (Config/Effects/clouds.lua) ─────────────────────────────

/// Preset name -> METAR text, parsed out of the DCS install's clouds.lua the
/// same way the bot does (the part of `readableName` after "METAR:").
fn cloud_preset_metar(preset: &str) -> Option<std::string::String> {
    static PRESETS: OnceLock<HashMap<std::string::String, std::string::String>> = OnceLock::new();
    PRESETS
        .get_or_init(|| {
            let mut candidates = vec![];
            if let Ok(d) = std::env::current_dir() {
                candidates.push(d.join("Config").join("Effects").join("clouds.lua"));
            }
            if let Some(root) = std::env::current_exe()
                .ok()
                .and_then(|e| e.parent()?.parent().map(|p| p.to_path_buf()))
            {
                candidates.push(root.join("Config").join("Effects").join("clouds.lua"));
            }
            let Some(src) = candidates.iter().find_map(|p| std::fs::read_to_string(p).ok()) else {
                log::warn!("[ATIS] could not read clouds.lua from {candidates:?}; presets shown by name");
                return HashMap::new();
            };
            let mut map = HashMap::new();
            let mut current: Option<std::string::String> = None;
            for line in src.lines() {
                let t = line.trim();
                if let Some(name) = t.strip_suffix('=').map(str::trim) {
                    if !name.is_empty() && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
                        current = Some(name.to_string());
                    }
                } else if t.starts_with("readableName ") || t.starts_with("readableName=") {
                    if let (Some(name), Some(i)) = (current.as_ref(), t.find("METAR:")) {
                        let rest = &t[i + "METAR:".len()..];
                        let end = rest.find(['\'', '"']).unwrap_or(rest.len());
                        let metar = rest[..end].trim();
                        if !metar.is_empty() {
                            map.insert(name.clone(), metar.to_string());
                        }
                    }
                }
            }
            map
        })
        .get(preset)
        .cloned()
}

// ── runways ────────────────────────────────────────────────────────────────

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

/// Leading (up to two) digits of a designator, as the bot reads it
/// (`int(runway[:2])`): "23L" -> 23, "5" -> 5.
fn designator_num(name: &str) -> Option<i32> {
    let digits: std::string::String = name
        .trim()
        .chars()
        .take_while(|c| c.is_ascii_digit())
        .take(2)
        .collect();
    digits.parse().ok().filter(|n| (1..=36).contains(n))
}

/// Every runway end DCS reports for the airbase (mission-state getRunways):
/// (heading_deg, designator, course_aligned). The `course_aligned` flag marks
/// the end pointing the same way as DCS's own `course` field for that runway
/// — its "primary" direction, used as the calm-wind tie-break.
fn runway_ends(
    lua: MizLua,
    airbase_id: &DcsOid<ClassAirbase>,
) -> Option<Vec<(f64, compact_str::CompactString, bool)>> {
    let ab = Airbase::get_instance(lua, airbase_id).ok()?;
    let ab_name = ab
        .as_object()
        .and_then(|o| o.get_name())
        .map(|n| n.to_string())
        .unwrap_or_default();
    let runways = ab.get_runways().ok()?;
    let mut ends: Vec<(f64, compact_str::CompactString, bool)> = Vec::new();
    for rwy in runways {
        let Ok(rwy) = rwy else { continue };
        let Ok(course) = rwy.course() else { continue };
        let raw_name = rwy.name().ok();
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
        // debug, not info: this fires once per runway per airbase on every ATIS
        // regeneration; at info it once made up 86% of the engine log.
        log::debug!(
            "[ATIS_RWY] {ab_name}: runway name={raw_name:?} course={course:.4}rad \
             ({c1:.0}deg) parsed_parts={parts:?}"
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
    Some(ends)
}

/// Pick the runway end best aligned with the wind, reported with the real DCS
/// designator (so it can't name a runway the airfield doesn't have).
fn active_runway(
    lua: MizLua,
    airbase_id: &DcsOid<ClassAirbase>,
    wind_from_deg: f64,
    wind_speed_kts: f64,
) -> Option<compact_str::CompactString> {
    let ends = runway_ends(lua, airbase_id)?;
    // Wind ≥ 3 kt: land into it. Calm: use the runway's own primary (course-
    // aligned) direction, then the lower-numbered end as a final tie-break.
    // Only truly still air has no direction. This was 3 kt, so a 2 kt
    // northerly at Gudauta fell through to the primary end and called 15
    // active while the bot (and the wind) said 33.
    let calm = wind_speed_kts < 0.5;
    let best = ends.iter().min_by(|a, b| {
        if calm {
            b.2.cmp(&a.2).then(a.0.total_cmp(&b.0))
        } else {
            angle_diff(wind_from_deg, a.0).total_cmp(&angle_diff(wind_from_deg, b.0))
        }
    });
    best.map(|(_, l, _)| l.clone())
}

/// "Runways (# = active): 23L# 238° | 05R 058°". With the terrain's runway
/// names the active marking is the bot's rule: every end whose designator is
/// within 90° of the wind it lands into. Headings pair the terrain's main
/// runway heading with the end it points along.
fn runways_line(
    lua: MizLua,
    ad: Option<&Airdrome>,
    airbase_id: Option<&DcsOid<ClassAirbase>>,
    wx: &WeatherData,
) -> compact_str::CompactString {
    if let Some(ad) = ad.filter(|a| !a.runways.is_empty()) {
        let parts: Vec<std::string::String> = ad
            .runways
            .iter()
            .map(|name| {
                let num = designator_num(name);
                let mut s = name.clone();
                if num.is_some_and(|n| angle_diff(wx.wind_from_deg, n as f64 * 10.0) <= 90.0) {
                    s.push('#');
                }
                let hdg = num.and_then(|n| {
                    let h = ad.rwy_heading_deg?;
                    [h, (h + 180.0).rem_euclid(360.0)]
                        .into_iter()
                        .find(|c| angle_diff(*c, n as f64 * 10.0) <= 45.0)
                });
                if let Some(h) = hdg {
                    let _ = write!(s, " {:03}°", (h.round() as u32) % 360);
                }
                s
            })
            .collect();
        return format_compact!("\nRunways (# = active): {}", parts.join(" | "));
    }
    // No terrain table: fall back to the mission state's getRunways.
    let Some(ab_id) = airbase_id else { return Default::default() };
    let Some(ends) = runway_ends(lua, ab_id) else { return Default::default() };
    if ends.is_empty() {
        return Default::default();
    }
    let active = active_runway(lua, ab_id, wx.wind_from_deg, wx.wind_speed_kts);
    let parts: Vec<std::string::String> = ends
        .iter()
        .map(|(h, l, _)| {
            let mark = if active.as_ref() == Some(l) { "#" } else { "" };
            format!("{l}{mark} {:03}°", (h.round() as u32) % 360)
        })
        .collect();
    format_compact!("\nRunways (# = active): {}", parts.join(" | "))
}

fn angle_diff(a: f64, b: f64) -> f64 {
    let diff = (a - b).rem_euclid(360.0);
    if diff > 180.0 { 360.0 - diff } else { diff }
}

// ── formatting ─────────────────────────────────────────────────────────────

/// 13780 -> "13,780"
fn thousands(n: i64) -> std::string::String {
    let digits = n.unsigned_abs().to_string();
    let mut out = std::string::String::with_capacity(digits.len() + digits.len() / 3 + 1);
    if n < 0 {
        out.push('-');
    }
    for (i, c) in digits.chars().enumerate() {
        if i > 0 && (digits.len() - i) % 3 == 0 {
            out.push(',');
        }
        out.push(c);
    }
    out
}

/// N32°42'14" / E036°24'49" (truncated seconds, like the bot).
fn dms(v: f64, pos: char, neg: char, deg_width: usize) -> std::string::String {
    let h = if v >= 0.0 { pos } else { neg };
    let a = v.abs();
    let d = a.trunc();
    let m = ((a - d) * 60.0).trunc();
    let s = (((a - d) * 60.0 - m) * 60.0).trunc().min(59.0);
    format!("{h}{:0w$}°{:02}'{:02}\"", d as u32, m as u32, s as u32, w = deg_width)
}

/// Cloud line. A preset shows its METAR text; an authored layer shows base
/// (MSL, as the mission sets it), thickness and density, as the bot does.
fn clouds_line(wx: &WeatherData) -> compact_str::CompactString {
    let base_ft = thousands((wx.cloud_base_m * M_TO_FT + 0.5) as i64);
    if let Some(p) = wx.cloud_preset.as_deref() {
        return match cloud_preset_metar(p) {
            Some(metar) => format_compact!("\nClouds: {metar} (base {base_ft} ft MSL)"),
            None => format_compact!("\nClouds: preset {p}, base {base_ft} ft MSL"),
        };
    }
    if !wx.has_cloud_table {
        return compact_str::CompactString::from("\nClouds: n/a");
    }
    let thick = wx
        .cloud_thickness_m
        .map(|t| format!(" | {} ft thick", thousands((t * M_TO_FT + 0.5) as i64)))
        .unwrap_or_default();
    format_compact!(
        "\nClouds: base {base_ft} ft MSL{thick} | density {}/10{}",
        wx.cloud_density,
        if wx.cloud_density == 0 { " (clear)" } else { "" },
    )
}

fn visibility_line(vis_m: f64) -> compact_str::CompactString {
    if vis_m >= 10000.0 {
        compact_str::CompactString::from("\nVisibility: 10 km (+) / 6 SM (+)")
    } else {
        format_compact!(
            "\nVisibility: {} m / {:.2} SM",
            thousands(vis_m as i64),
            vis_m / M_PER_SM
        )
    }
}

/// "219° @ 19 kts / 10 m/s"; knots truncated like the bot.
fn wind_str(wx: &WeatherData) -> compact_str::CompactString {
    let kts = (wx.wind_speed_ms * MS_TO_KTS) as u32;
    if kts == 0 {
        return compact_str::CompactString::from("calm");
    }
    format_compact!(
        "{:03}° @ {kts} kts / {:.0} m/s{}",
        (wx.wind_from_deg.round() as u32) % 360,
        wx.wind_speed_ms,
        // Dynamic weather: sampled live, so it can differ from the bot's.
        if wx.wind_live { " (live)" } else { "" },
    )
}

/// "1022 hPa | 30.19 inHg | 766 mmHg"; hPa truncated like the bot.
fn pressure_str(hpa: f64) -> compact_str::CompactString {
    format_compact!(
        "{} hPa | {:.2} inHg | {:.0} mmHg",
        hpa as i64,
        hpa * HPA_TO_INHG,
        hpa * HPA_TO_MMHG
    )
}

fn case_advisory(wx: &WeatherData) -> &'static str {
    if !wx.has_clouds() {
        "CASE I"
    } else if wx.cloud_base_m < 305.0 {
        "CASE III"
    } else if wx.cloud_base_m < 914.0 {
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
    if winds.is_empty() {
        return Default::default();
    }
    let mut s = compact_str::CompactString::from("\nWinds/Temps Aloft (MSL):");
    for w in winds {
        let alt_m = (w.alt_ft as f64 / M_TO_FT).round() as u32;
        let _ = write!(
            s,
            "\n  {alt:>5}ft/{alt_m}m: {wdir:03}°/{wspd} {temp}",
            alt = w.alt_ft,
            wdir = (w.wind_from_deg.round() as u32) % 360,
            wspd = wind_speed_both(w.wind_speed_kts),
            temp = temp_both(w.temp_c),
        );
    }
    s
}

/// The shared surface-weather block: temperature, wind, visibility, clouds,
/// precipitation, QFE and QNH.
fn surface_block(wx: &WeatherData, with_qfe: bool) -> compact_str::CompactString {
    let mut s = format_compact!(
        "\nTemperature: {}{:.1}°C ({}{:.0}°F)\nSurface Wind: {}{}{}",
        temp_sign(wx.temp_c),
        wx.temp_c,
        temp_sign(c_to_f(wx.temp_c)),
        c_to_f(wx.temp_c),
        wind_str(wx),
        visibility_line(wx.visibility_m),
        clouds_line(wx),
    );
    if wx.precip {
        s.push_str("\nPrecipitation: yes");
    }
    if with_qfe {
        let _ = write!(s, "\nQFE: {}", pressure_str(wx.qfe_hpa));
    }
    let _ = write!(s, "\nQNH: {}", pressure_str(wx.qnh_hpa));
    s
}

pub(crate) fn is_aircraft_slot(db: &Db, slot: &SlotId) -> bool {
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

pub(crate) fn carrier_brc(db: &Db, kind: &ObjectiveKind) -> u32 {
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

/// Returns `Ok(true)` if a report was sent, `Ok(false)` if there was no slot
/// context to build one from (caller can fall back to a general brief).
/// Swap in the bot's own numbers for a field -- DCS's Mission-Editor weather
/// model sampled at the airdrome reference point (see HARVEST_AIRDROMES_LUA)
/// -- and derive QNH the bot's way, so the two ATIS panels players get on
/// spawn agree to the hPa. The live atmosphere stays the fallback.
fn apply_me_weather(wx: &mut WeatherData, ad: &Airdrome) {
    let Some((temp_c, qfe_hpa, wind_from, wind_ms)) = ad.me_wx else { return };
    wx.temp_c = temp_c;
    wx.qfe_hpa = qfe_hpa;
    wx.qnh_hpa = qfe_hpa + ad.alt_m * QFE_TO_QNH_HPA_PER_M;
    wx.qnh_inhg = wx.qnh_hpa * HPA_TO_INHG;
    wx.wind_from_deg = wind_from;
    wx.wind_speed_ms = wind_ms;
    wx.wind_speed_kts = wind_ms * MS_TO_KTS;
    wx.wind_live = false;
    wx.ground_elev_m = ad.alt_m;
}

/// The airfield ATIS in DCSServerBot's layout, line for line, so the two
/// panels a player gets on spawn read the same: title, Code, Position
/// (lat | lon | MGRS), Altitude, Tower Frequencies, Runways (# = active),
/// Heading, Temperature, Surface Wind, Visibility, Cloud Cover, QFE, QNH.
fn bot_layout_atis(
    lua: MizLua,
    obj_name: &str,
    ad: Option<&Airdrome>,
    ab_id: Option<&DcsOid<ClassAirbase>>,
    wx: &WeatherData,
    px: f64,
    pz: f64,
) -> compact_str::CompactString {
    const RULE: &str = "\n==============================";
    let title = format_compact!("ATIS-REPORT FOR {}", obj_name.to_uppercase());
    let mut m = format_compact!("{title}\n{}", "=".repeat(title.chars().count()));
    if let Some(code) = ad.and_then(|a| a.code.as_deref()) {
        let _ = write!(m, "\nCode: {code}");
    }
    let ll = ad.and_then(|a| Some((a.lat?, a.lon?))).or_else(|| {
        dcso3::coord::Coord::singleton(lua)
            .ok()?
            .lo_to_ll(dcso3::LuaVec3(dcso3::Vector3::new(px, wx.ground_elev_m, pz)))
            .ok()
            .map(|l| (l.latitude, l.longitude))
    });
    if let Some((lat, lon)) = ll {
        let mgrs = ad.and_then(|a| a.mgrs.clone()).or_else(|| {
            let g = dcso3::coord::Coord::singleton(lua).ok()?.ll_to_mgrs(lat, lon).ok()?;
            Some(format!(
                "{} {} {:05} {:05}",
                g.utm_zone,
                g.mgrs_digraph,
                g.easting as u32 % 100_000,
                g.northing as u32 % 100_000
            ))
        });
        let _ = write!(m, "\nPosition: {} | {}", dms(lat, 'N', 'S', 2), dms(lon, 'E', 'W', 3));
        if let Some(mgrs) = mgrs {
            let _ = write!(m, " | {mgrs}");
        }
    }
    let _ = write!(m, "\nAltitude: {} ft", (wx.ground_elev_m * M_TO_FT) as i64);
    m.push_str(RULE);
    if let Some(a) = ad.filter(|a| !a.freqs_hz.is_empty()) {
        let f: Vec<std::string::String> =
            a.freqs_hz.iter().map(|hz| format!("{:.3} MHz", hz / 1_000_000.0)).collect();
        let _ = write!(m, "\nTower Frequencies: {}", f.join(" | "));
    }
    match ad.filter(|a| !a.runways.is_empty()) {
        Some(a) => {
            // The bot's rule: every end within 90 deg of the wind is active.
            let parts: Vec<std::string::String> = a
                .runways
                .iter()
                .map(|name| {
                    let active = designator_num(name)
                        .is_some_and(|n| angle_diff(wx.wind_from_deg, n as f64 * 10.0) <= 90.0);
                    if active { format!("{name}#") } else { name.clone() }
                })
                .collect();
            let _ = write!(m, "\nRunways (# = active): {}", parts.join(" | "));
            if let Some(h) = a.rwy_heading_deg {
                let _ = write!(
                    m,
                    "\nHeading: {}° | {}°",
                    ((h + 180.0).rem_euclid(360.0)) as u32,
                    (h as u32) % 360
                );
            }
        }
        None => m.push_str(&runways_line(lua, None, ab_id, wx)),
    }
    m.push_str(RULE);
    let _ = write!(m, "\nTemperature: {:.2}° C", wx.temp_c);
    let kts = (wx.wind_speed_ms * MS_TO_KTS) as u32;
    let dir = match (wx.wind_from_deg.round() as u32) % 360 {
        0 => 360,
        d => d,
    };
    let _ = write!(
        m,
        "\nSurface Wind: {dir}° @ {kts} kts{}",
        if wx.wind_live { " (live)" } else { "" }
    );
    m.push_str(&visibility_line(wx.visibility_m));
    match wx.cloud_preset.as_deref() {
        Some(p) => {
            let _ = write!(
                m,
                "\nCloud Cover: {}",
                cloud_preset_metar(p).unwrap_or_else(|| p.to_string())
            );
        }
        None if wx.has_cloud_table && wx.cloud_density > 0 => {
            let _ = write!(
                m,
                "\nClouds: Base {} ft | Thickness {} ft",
                thousands((wx.cloud_base_m * M_TO_FT + 0.5) as i64),
                thousands((wx.cloud_thickness_m.unwrap_or(0.0) * M_TO_FT + 0.5) as i64)
            );
        }
        None => m.push_str("\nClouds: n/a"),
    }
    if wx.precip {
        m.push_str("\nPrecipitation: yes");
    }
    let _ = write!(m, "\nQFE: {} hPa | {:.2} inHg", wx.qfe_hpa as i64, wx.qfe_hpa * HPA_TO_INHG);
    let _ = write!(m, "\nQNH: {} hPa | {:.2} inHg", wx.qnh_hpa as i64, wx.qnh_hpa * HPA_TO_INHG);
    m
}

fn send_atis(lua: MizLua, slot: SlotId, full: bool) -> Result<bool> {
    let ctx = unsafe { Context::get_mut() };

    let (oid, miz_gid) = match ctx.db.ephemeral.get_slot_info(&slot) {
        Some(s) => (s.objective, s.miz_gid),
        None => return Ok(false),
    };

    let obj = match ctx.db.persisted.objectives.get(&oid) {
        Some(o) => o,
        None => return Ok(false),
    };

    let pos = obj.pos();
    let obj_name = obj.name().to_string();

    let msg: compact_str::CompactString = if obj.kind().is_carrier_group() {
        let wx = fetch_weather(lua, pos.x, pos.y)?;
        let brc_deg = carrier_brc(&ctx.db, obj.kind());
        let mut m = format_compact!(
            "CARRIER ATIS - {name}\nBRC: {brc:03}°\nRecovery: {case}",
            name = obj_name.to_uppercase(),
            brc = brc_deg,
            case = case_advisory(&wx),
        );
        m.push_str(&surface_block(&wx, false));
        if full {
            m.push_str(&format_winds_aloft(&wx.winds_aloft));
        }
        m
    } else if obj.kind().is_airbase() {
        // Weather is taken at the airdrome reference point, the same point the
        // bot samples, so elevation, temperature and QFE line up with it.
        let ad = nearest_airdrome(pos.x, pos.y);
        let (px, pz) = ad.as_ref().map(|a| (a.x, a.z)).unwrap_or((pos.x, pos.y));
        let mut wx = fetch_weather(lua, px, pz)?;
        if let Some(a) = ad.as_ref() {
            apply_me_weather(&mut wx, a);
        }
        if log::log_enabled!(log::Level::Debug) {
            let live = wind_at(lua, px, wx.ground_elev_m + SURFACE_WIND_AGL_M, pz).ok();
            log::debug!(
                "[ATIS_WIND] {obj_name} (airdrome {:?}): elev {:.1}m; surface {:.0}/{:.1}kt \
                 (live={}); getWind @10m AGL {:?}",
                ad.as_ref().map(|a| (a.name.as_str(), a.alt_m)),
                wx.ground_elev_m,
                wx.wind_from_deg,
                wx.wind_speed_kts,
                wx.wind_live,
                live.map(|(d, s)| (d.round(), (s * MS_TO_KTS).round())),
            );
        }
        let ab_id = ctx.db.ephemeral.get_airbase_by_oid(&oid);
        let mut m = bot_layout_atis(lua, &obj_name, ad.as_ref(), ab_id, &wx, px, pz);
        if full {
            m.push_str(&format_winds_aloft(&wx.winds_aloft));
        }
        m
    } else {
        return Ok(false);
    };

    ctx.db.ephemeral.msgs().panel_to_group(30, false, miz_gid, msg);
    Ok(true)
}

/// A general surface-weather brief at `pos`, for when there's no slot/field
/// context (e.g. the F10 "Weather" item used from the map or a ground slot).
pub fn send_weather_brief(lua: MizLua, gid: GroupId, pos: Vector2) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let wx = fetch_weather(lua, pos.x, pos.y)?;
    let mut msg = format_compact!(
        "WEATHER (general)\nElevation: {} ft / {} m",
        (wx.ground_elev_m * M_TO_FT) as i64,
        wx.ground_elev_m.round() as i64
    );
    msg.push_str(&surface_block(&wx, true));
    ctx.db.ephemeral.msgs().panel_to_group(30, false, gid, msg);
    Ok(())
}

/// On-demand full weather report (surface + winds/temp aloft) for the
/// player's current slot, triggered via the `-weather` chat command.
/// `Ok(false)` when the player isn't in a slot at a known field.
pub fn send_full_weather(lua: MizLua, slot: SlotId) -> Result<bool> {
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

/// Build one coalition's ATC picture: every field it holds, with the same
/// weather and active-runway figures the text ATIS reports, plus its aircraft.
///
/// Lives here rather than in `admin.rs` so it reuses `fetch_weather` and
/// `active_runway` directly — a spoken ATIS and the F10 text ATIS can then
/// never disagree about the wind or the duty runway.
pub(crate) fn query_atc(
    lua: MizLua,
    ctx: &Context,
    side: dcso3::coalition::Side,
) -> bfprotocols::atc::AtcPicture {
    use bfprotocols::atc::{AtcAirfield, AtcPicture, AtcRunway, AtcTraffic};
    use dcso3::airbase::Airbase;
    use dcso3::object::DcsObject as _;
    use std::string::String as StdString;

    let db = &ctx.db;
    let coord = dcso3::coord::Coord::singleton(lua).ok();
    let to_ll = |x: f64, z: f64| -> (f64, f64) {
        coord
            .as_ref()
            .and_then(|c| c.lo_to_ll(dcso3::LuaVec3(dcso3::Vector3::new(x, 0.0, z))).ok())
            .map(|ll| (ll.latitude, ll.longitude))
            .unwrap_or((0.0, 0.0))
    };

    let mut airfields: Vec<AtcAirfield> = vec![];
    for (oid, obj) in &db.persisted.objectives {
        if obj.owner != side {
            continue;
        }
        let is_carrier = obj.kind().is_carrier_group();
        if !obj.kind().is_airbase() && !is_carrier {
            continue;
        }
        let pos = obj.pos();
        // Same sample point as the text ATIS: the airdrome reference point.
        let (px, pz) = if is_carrier {
            (pos.x, pos.y)
        } else {
            nearest_airdrome(pos.x, pos.y)
                .map(|a| (a.x, a.z))
                .unwrap_or((pos.x, pos.y))
        };
        let Ok(wx) = fetch_weather(lua, px, pz) else {
            continue;
        };
        let (lat, lon) = to_ll(pos.x, pos.y);
        let (qfe_hpa, qfe_inhg) = (wx.qfe_hpa, wx.qfe_hpa * HPA_TO_INHG);

        // Runways, straight from DCS — no per-map table needed.
        let ab = db.ephemeral.get_airbase_by_oid(oid);
        let mut runways: Vec<AtcRunway> = vec![];
        if let Some(ab_id) = ab {
            if let Ok(abase) = Airbase::get_instance(lua, ab_id) {
                if let Ok(seq) = abase.get_runways() {
                    for r in seq {
                        let Ok(r) = r else { continue };
                        let course = r.course().unwrap_or(0.0).to_degrees().rem_euclid(360.0);
                        runways.push(AtcRunway {
                            name: r.name().map(|n| n.to_string()).unwrap_or_default(),
                            heading: course as u16,
                            length_m: r.length().unwrap_or(0.0) as u32,
                            width_m: r.width().unwrap_or(0.0) as u32,
                        });
                    }
                }
            }
        }
        let active_runway = ab
            .and_then(|ab_id| active_runway(lua, ab_id, wx.wind_from_deg, wx.wind_speed_kts))
            .map(|r| r.to_string());

        // Dewpoint from temperature and the cloud base (the standard
        // spread/lapse approximation — DCS models no humidity of its own).
        let base_agl_m = (wx.cloud_base_m - wx.ground_elev_m).max(0.0);
        let dewpoint_c = wx.temp_c - (base_agl_m * M_TO_FT / 1000.0) * 4.4 / 2.5;

        airfields.push(AtcAirfield {
            id: format_compact!("{oid}").to_string(),
            name: obj.name().to_string(),
            lat,
            lon,
            elev_ft: (wx.ground_elev_m * M_TO_FT) as i32,
            kind: StdString::from(if is_carrier { "carrier" } else { "airbase" }),
            runways,
            active_runway,
            brc: is_carrier.then(|| carrier_brc(db, obj.kind()) as u16),
            wind_from_deg: wx.wind_from_deg.round() as u16 % 360,
            wind_speed_kts: wx.wind_speed_kts as u16,
            qnh_inhg: wx.qnh_inhg,
            qnh_hpa: wx.qnh_hpa,
            qfe_inhg,
            qfe_hpa,
            temp_c: wx.temp_c.round() as i16,
            dewpoint_c: dewpoint_c as i16,
            visibility_m: wx.visibility_m as u32,
            cloud_base_ft: (wx.has_clouds() && wx.cloud_base_m > 0.0)
                .then(|| (wx.cloud_base_m * M_TO_FT) as i32),
            cloud_cover: wx.cloud_preset.as_ref().map(|p| {
                cloud_preset_metar(p).unwrap_or_else(|| p.to_string())
            }),
            precipitation: wx.precip,
            recovery_case: is_carrier.then(|| match case_advisory(&wx) {
                "CASE III" => 3u8,
                "CASE II" => 2,
                _ => 1,
            }),
            logi: obj.logi(),
            health: obj.health(),
            supply: obj.supply(),
            fuel: obj.fuel(),
            threatened: obj.threatened(),
            // A field with its logistics flattened can't turn aircraft round,
            // so ATIS calls it closed rather than pretending otherwise.
            open: obj.logi() > 0 && obj.health() > 0,
        });
    }

    // Aircraft on this side, with the field each is nearest to.
    let mut traffic: Vec<AtcTraffic> = vec![];
    for (ucid, player, inst) in db.instanced_players() {
        if player.side != side {
            continue;
        }
        let unit = player
            .current_slot
            .as_ref()
            .and_then(|(slot, _)| db.ephemeral.slot_instance_unit(lua, slot).ok());
        let unit_id = unit
            .as_ref()
            .and_then(|u| u.id().ok())
            .map(|id| id.inner())
            .filter(|id| *id > 0)
            .map(|id| id as u64);
        let callsign = unit
            .as_ref()
            .and_then(|u| u.get_callsign().ok())
            .map(|s| s.to_string())
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| player.name.to_string());
        let fp = Vector2::new(inst.position.p.x, inst.position.p.z);
        let v = inst.velocity;
        let heading = if v.x.abs() > f64::EPSILON || v.z.abs() > f64::EPSILON {
            ((v.z.atan2(v.x).to_degrees() + 360.0) % 360.0) as u16
        } else {
            0
        };
        let ground_elev = dcso3::land::Land::singleton(lua)
            .and_then(|l| l.get_height(dcso3::LuaVec2(fp)))
            .unwrap_or(0.0);
        let (lat, lon) = to_ll(fp.x, fp.y);
        let mut nearest: Option<(f64, u16, StdString)> = None;
        for (oid, obj) in &db.persisted.objectives {
            if obj.owner != side || !(obj.kind().is_airbase() || obj.kind().is_carrier_group()) {
                continue;
            }
            let d = obj.pos() - fp;
            let rng = d.magnitude();
            if nearest.as_ref().map_or(true, |(r, _, _)| rng < *r) {
                let brg = ((d.y.atan2(d.x).to_degrees() + 360.0) % 360.0) as u16;
                nearest = Some((rng, brg, format_compact!("{oid}").to_string()));
            }
        }
        traffic.push(AtcTraffic {
            ucid: ucid.to_string(),
            unit_id,
            callsign,
            player_name: player.name.to_string(),
            lat,
            lon,
            alt_ft: (inst.position.p.y * M_TO_FT) as i32,
            agl_ft: ((inst.position.p.y - ground_elev).max(0.0) * M_TO_FT) as i32,
            heading,
            speed_kts: ((v.x * v.x + v.y * v.y + v.z * v.z).sqrt() * 1.944) as u16,
            vspd_fpm: (v.y * M_TO_FT * 60.0) as i32,
            on_ground: !inst.in_air,
            field: nearest.as_ref().map(|(_, _, id)| id.clone()),
            field_brg: nearest.as_ref().map_or(0, |(_, b, _)| *b),
            field_rng_m: nearest.as_ref().map_or(0, |(r, _, _)| *r as u32),
            rotary: false,
        });
    }

    AtcPicture { airfields, traffic }
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
