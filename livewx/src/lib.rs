// Copyright 2026 Dillen Weerasinghe. MIT licensed: see livewx/LICENSE.
// Moved out of bflib (bflib/src/bg/live_weather.rs) so the training range
// engine (bfrange) can share it.

//! Real-world weather and time for a DCS mission file.
//!
//! DCS has no API to change an already-running mission's weather or clock,
//! so live weather sync works the same way as the dcs-real-weather project:
//! rewrite the .miz on disk before the mission next loads. bflib does it
//! right before it triggers the process restart (bflib admin_shutdown);
//! bfrange does it when DCS starts and when a mission stops.

use anyhow::{Context, Result};
use bfprotocols::cfg::LiveWeatherConfig;
use chrono::{Datelike, FixedOffset, Timelike, Utc};
use dcso3::env::miz_pack::{
    read_table_from_miz, rewrite_entry_in_miz, scratch_lua, serialize_to_lua,
};
use mlua::{Table, Value};
use std::path::Path;

/// Fetch the real weather (and, with `cfg.sync_time`, the real time) and
/// write it into the mission file at `miz_path`. Blocking: two HTTP requests
/// of up to 10 s each, then a rewrite of the archive (atomic: written beside
/// it and renamed over it).
pub fn apply(miz_path: &Path, cfg: &LiveWeatherConfig) -> Result<()> {
    // Dropped at the end of the call. This used to be leaked, because
    // serialize_to_lua only took 'static values -- a whole Lua state holding
    // the entire mission table, lost on every call. scratch_lua also keeps
    // io/os away from the mission script it evaluates.
    let lua = scratch_lua()?;
    let mission: Table = read_table_from_miz(&lua, miz_path, "mission")
        .context("reading mission table from miz")?;
    if cfg.sync_time {
        apply_live_time(&mission, cfg.utc_offset_hours).context("applying live time")?;
    }
    apply_live_weather(&mission, cfg).context("applying live weather")?;
    let s = serialize_to_lua("mission", Value::Table(mission))
        .context("serializing updated mission table")?;
    rewrite_entry_in_miz(miz_path, "mission", &s)
        .context("writing updated mission back into miz")?;
    Ok(())
}

/// set the mission's date and start_time to the current real-world local
/// date/time: the theatre's, when its UTC offset is given (the Marianas are
/// UTC+10), else the machine running the server's
pub fn apply_live_time(mission: &Table, utc_offset_hours: Option<f64>) -> Result<()> {
    let now = match utc_offset_hours {
        Some(h) => {
            let secs = (h * 3600.).round() as i32;
            let tz = FixedOffset::east_opt(secs)
                .with_context(|| format!("utc_offset_hours {h} is not a UTC offset"))?;
            Utc::now().with_timezone(&tz).naive_local()
        }
        None => chrono::Local::now().naive_local(),
    };
    let date: Table = mission.raw_get("date").context("getting date table")?;
    date.raw_set("Day", now.day() as i64)
        .context("setting date.Day")?;
    date.raw_set("Month", now.month() as i64)
        .context("setting date.Month")?;
    date.raw_set("Year", now.year() as i64)
        .context("setting date.Year")?;
    let start_time = now.hour() as i64 * 3600 + now.minute() as i64 * 60 + now.second() as i64;
    mission
        .raw_set("start_time", start_time)
        .context("setting start_time")?;
    log::info!(
        "applied live local time to mission: {}",
        now.format("%Y-%m-%d %H:%M:%S")
    );
    Ok(())
}

/// DCS 2.9's dynamic weather system replaced the old density/thickness/base
/// cloud sliders with a fixed set of named presets ("Preset1".."Preset27",
/// "RainyPreset1".."RainyPreset3") that each bake in their own visual
/// coverage -- setting density/thickness/base directly (or preset = "", the
/// old "use the sliders" sentinel) renders no clouds at all any more. This
/// picks the preset that best matches real cloud cover and precipitation
/// intensity, plus that preset's own base altitude, both taken from the
/// preset .miz templates DCS itself ships in the mission editor.
pub fn select_cloud_preset(cover_pct: f64, precip_mm: f64) -> (Option<&'static str>, i64) {
    if precip_mm > 0.2 {
        return if precip_mm > 6.0 {
            (Some("RainyPreset3"), 1700)
        } else if precip_mm > 2.0 {
            (Some("RainyPreset2"), 2500)
        } else {
            (Some("RainyPreset1"), 2900)
        };
    }
    if cover_pct <= 6.0 {
        (None, 4200)
    } else if cover_pct <= 25.0 {
        (Some("Preset2"), 2500)
    } else if cover_pct <= 45.0 {
        (Some("Preset6"), 2500)
    } else if cover_pct <= 65.0 {
        (Some("Preset14"), 2500)
    } else {
        (Some("Preset22"), 2500)
    }
}

/// Convert a wind speed to m/s given the unit string open-meteo reported for
/// it. DCS is metric (m/s); a km/h value fed straight in reads as a ~3.6x gale,
/// so convert by the label and fall back to a plausibility check when it's
/// missing.
fn wind_to_ms(raw: f64, unit: &str) -> f64 {
    match unit {
        "ms" | "m/s" => raw,
        "kmh" | "km/h" => raw / 3.6,
        "mph" => raw * 0.44704,
        "kn" | "kt" | "kts" => raw * 0.514444,
        _ if raw > 40.0 => raw / 3.6, // unlabelled + implausible → assume km/h
        _ => raw,
    }
}

/// Real-world surface weather decoded from one station's METAR
/// (checkwxapi.com). It drives only the mission's surface layer: METAR has
/// no upper-air data, so the winds aloft come from open-meteo.
#[derive(Debug, Clone, PartialEq)]
pub struct MetarSurface {
    pub temp_c: f64,
    /// None when the station reports no pressure in any unit.
    pub pressure_hpa: Option<f64>,
    pub wind_speed_ms: f64,
    pub wind_from_dir: f64,
    pub cloud_cover_pct: f64,
    pub precipitation_mm: f64,
    /// The station's own position, as checkwx reports it.
    pub lat: Option<f64>,
    pub lon: Option<f64>,
}

/// Fetch and decode `station`'s current METAR from checkwxapi.com.
pub fn fetch_checkwx_surface(api_key: &str, station: &str) -> Result<MetarSurface> {
    let url = format!("https://api.checkwx.com/metar/{station}/decoded");
    log::info!("[LIVE_WX] requesting {url}");
    let body = ureq::get(&url)
        .set("X-API-Key", api_key)
        .timeout(std::time::Duration::from_secs(10))
        .call()
        .context("requesting METAR from checkwxapi.com")?
        .into_string()
        .context("reading METAR response body")?;
    // debug, not info: the whole report on every restart is noise in the
    // server log, and it is the answer to a keyed request.
    log::debug!("[LIVE_WX] checkwx raw response: {body}");
    let resp: serde_json::Value =
        serde_json::from_str(&body).context("parsing METAR response")?;
    decode_checkwx_metar(&resp)
}

/// Decode a checkwx `/metar/{station}/decoded` response.
pub fn decode_checkwx_metar(resp: &serde_json::Value) -> Result<MetarSurface> {
    let d = resp
        .get("data")
        .and_then(|d| d.as_array())
        .and_then(|a| a.first())
        .context("METAR response has no data[0] -- bad station ICAO or API key?")?;

    let temp_c = d
        .pointer("/temperature/celsius")
        .and_then(|v| v.as_f64())
        .context("METAR missing temperature.celsius")?;
    // Some stations (OSDI, Sept 29) come back with no hPa reading; take the
    // same pressure in whichever unit checkwx did report.
    let baro = |key: &str| d.pointer(&format!("/barometer/{key}")).and_then(|v| v.as_f64());
    let pressure_hpa = baro("hpa")
        .or_else(|| baro("mb"))
        .or_else(|| baro("kpa").map(|k| k * 10.0))
        .or_else(|| baro("hg").map(|h| h * 33.8639));
    let (wind_speed_ms, wind_from_dir) = match d.get("wind") {
        Some(w) if !w.is_null() => {
            let spd = w
                .get("speed_mps")
                .and_then(|v| v.as_f64())
                .or_else(|| {
                    w.get("speed_kts")
                        .and_then(|v| v.as_f64())
                        .map(|k| k * 0.514444)
                })
                .unwrap_or(0.0);
            let dir = w.get("degrees").and_then(|v| v.as_f64()).unwrap_or(0.0);
            (spd, dir)
        }
        _ => (0.0, 0.0),
    };
    let cloud_cover_pct = d
        .get("clouds")
        .and_then(|c| c.as_array())
        .map(|layers| {
            layers
                .iter()
                .filter_map(|l| l.get("code").and_then(|v| v.as_str()))
                .map(|code| match code {
                    "OVC" => 100.0,
                    "BKN" => 75.0,
                    "SCT" => 40.0,
                    "FEW" => 15.0,
                    _ => 0.0,
                })
                .fold(0.0_f64, f64::max)
        })
        .unwrap_or(0.0);
    let precipitation_mm = d
        .get("conditions")
        .and_then(|c| c.as_array())
        .map(|conds| {
            let mut mm = 0.0_f64;
            for c in conds {
                let code = c.get("code").and_then(|v| v.as_str()).unwrap_or("");
                let precip = ["RA", "SN", "DZ", "GR", "GS", "PL", "SG", "IC", "UP"]
                    .iter()
                    .any(|p| code.contains(p));
                if !precip {
                    continue;
                }
                let v = if code.starts_with('+') {
                    8.0
                } else if code.starts_with('-') {
                    0.5
                } else {
                    3.0
                };
                mm = mm.max(v);
            }
            mm
        })
        .unwrap_or(0.0);

    // checkwx reports the station position as GeoJSON, so coordinates are
    // [lon, lat] -- not the other way around.
    let coords = d.pointer("/station/geometry/coordinates");
    let coord = |i: usize| coords.and_then(|c| c.get(i)).and_then(|v| v.as_f64());
    let (lon, lat) = (coord(0), coord(1));

    Ok(MetarSurface {
        temp_c,
        pressure_hpa,
        wind_speed_ms,
        wind_from_dir,
        cloud_cover_pct,
        precipitation_mm,
        lat,
        lon,
    })
}

/// fetch current real-world weather from open-meteo.com (no API key required)
/// and apply ground-level temperature, QNH, wind, clouds, and the two upper
/// wind layers (2000 m / 8000 m, from the 800 hPa / 300 hPa pressure levels)
/// to the mission's weather table. When `cfg.checkwx_api_key` + `metar_station`
/// are set, the surface layer is replaced with that station's real decoded
/// METAR (open-meteo still supplies the winds aloft, and a failed METAR fetch
/// falls back to open-meteo for everything).
fn apply_live_weather(mission: &Table, cfg: &LiveWeatherConfig) -> Result<()> {
    let (lat, lon) = (cfg.lat, cfg.lon);
    // Ask for m/s explicitly AND convert defensively below -- if the API ever
    // ignores/changes the unit param, a km/h value fed straight into DCS
    // (which is metric, m/s) reads as a ~3.6x gale.
    let url = format!(
        "https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&current=temperature_2m,pressure_msl,wind_speed_10m,wind_direction_10m,cloud_cover,precipitation&hourly=wind_speed_800hPa,wind_direction_800hPa,wind_speed_300hPa,wind_direction_300hPa&forecast_days=1&timezone=UTC&wind_speed_unit=ms"
    );
    log::info!("[LIVE_WX] requesting {url}");
    let body = ureq::get(&url)
        .timeout(std::time::Duration::from_secs(10))
        .call()
        .context("requesting live weather from open-meteo")?
        .into_string()
        .context("reading live weather response body")?;
    log::debug!("[LIVE_WX] raw response: {body}");
    let resp: serde_json::Value =
        serde_json::from_str(&body).context("parsing live weather response")?;
    let current = resp
        .get("current")
        .context("live weather response missing 'current'")?;
    let mut temp_c = current
        .get("temperature_2m")
        .and_then(|v| v.as_f64())
        .context("live weather response missing temperature_2m")?;
    let mut pressure_hpa = current
        .get("pressure_msl")
        .and_then(|v| v.as_f64())
        .context("live weather response missing pressure_msl")?;
    let wind_speed_raw = current
        .get("wind_speed_10m")
        .and_then(|v| v.as_f64())
        .context("live weather response missing wind_speed_10m")?;
    // Confirm the unit the API actually used; if it isn't m/s, convert. Real
    // 10 m surface wind almost never exceeds ~30 m/s, so a value that large is
    // a strong signal the API returned km/h despite wind_speed_unit=ms.
    let wind_unit = resp
        .get("current_units")
        .and_then(|u| u.get("wind_speed_10m"))
        .and_then(|v| v.as_str())
        .unwrap_or("");
    let mut wind_speed_ms = wind_to_ms(wind_speed_raw, wind_unit);
    let hourly_wind_unit = resp
        .get("hourly_units")
        .and_then(|u| u.get("wind_speed_800hPa"))
        .and_then(|v| v.as_str())
        .unwrap_or("");
    let mut wind_from_dir = current
        .get("wind_direction_10m")
        .and_then(|v| v.as_f64())
        .context("live weather response missing wind_direction_10m")?;
    let mut cloud_cover_pct = current
        .get("cloud_cover")
        .and_then(|v| v.as_f64())
        .context("live weather response missing cloud_cover")?;
    let mut precipitation_mm = current
        .get("precipitation")
        .and_then(|v| v.as_f64())
        .context("live weather response missing precipitation")?;

    // If a METAR station is configured, its real decoded observation replaces
    // the open-meteo *surface* values (winds aloft below still come from the
    // model). A failed fetch just leaves the open-meteo values in place.
    if let (Some(key), Some(station)) =
        (cfg.checkwx_api_key.as_deref(), cfg.metar_station.as_deref())
    {
        match fetch_checkwx_surface(key, station) {
            Ok(m) => {
                log::info!(
                    "[LIVE_WX] CheckWX {station}: temp={:.0}C qnh={} \
                     wind={:.1}m/s@{:.0}deg cloud={:.0}% precip={:.1}mm -- \
                     overriding open-meteo surface",
                    m.temp_c,
                    m.pressure_hpa
                        .map(|p| format!("{p:.0}hPa"))
                        .unwrap_or_else(|| "not reported, keeping open-meteo's".into()),
                    m.wind_speed_ms,
                    m.wind_from_dir,
                    m.cloud_cover_pct,
                    m.precipitation_mm,
                );
                temp_c = m.temp_c;
                if let Some(p) = m.pressure_hpa {
                    pressure_hpa = p;
                }
                wind_speed_ms = m.wind_speed_ms;
                wind_from_dir = m.wind_from_dir;
                cloud_cover_pct = m.cloud_cover_pct;
                precipitation_mm = m.precipitation_mm;
            }
            Err(e) => log::warn!(
                "[LIVE_WX] CheckWX fetch for {station} failed ({e:#}) -- \
                 keeping open-meteo surface values"
            ),
        }
    }

    // DCS's wind direction is the direction the wind blows TOWARD, the
    // opposite of the real-world meteorological "from" convention
    let wind_to_dir = (wind_from_dir + 180.0) % 360.0;
    let qnh_mmhg = (pressure_hpa * 0.750062).round() as i64;

    log::info!(
        "[LIVE_WX] API current: temp={temp_c}C pressure_msl={pressure_hpa}hPa \
         wind_10m={wind_speed_raw} (unit {wind_unit:?} -> {wind_speed_ms:.2}m/s) \
         wind_dir_from={wind_from_dir}deg (-> blows toward {wind_to_dir:.0}deg) \
         cloud_cover={cloud_cover_pct}% precip={precipitation_mm}mm; \
         hourly wind unit {hourly_wind_unit:?}, using hour index {}",
        Utc::now().hour()
    );

    let weather: Table = mission.raw_get("weather").context("getting weather table")?;
    let season: Table = weather
        .raw_get("season")
        .context("getting weather.season table")?;

    // Log what the .miz was authored with, so a "still too windy" report can be
    // traced to either the source data, our conversion, or an untouched layer.
    {
        let old_temp: Option<f64> = season.raw_get("temperature").ok();
        let old_qnh: Option<f64> = weather.raw_get("qnh").ok();
        let old_turb: Option<f64> = weather.raw_get("groundTurbulence").ok();
        let w: Option<Table> = weather.raw_get("wind").ok();
        let layer = |name: &str| -> String {
            w.as_ref()
                .and_then(|w| w.raw_get::<_, Table>(name).ok())
                .map(|t| {
                    format!(
                        "{:.1}m/s@{:.0}",
                        t.raw_get::<_, f64>("speed").unwrap_or(f64::NAN),
                        t.raw_get::<_, f64>("dir").unwrap_or(f64::NAN)
                    )
                })
                .unwrap_or_else(|| "?".into())
        };
        log::info!(
            "[LIVE_WX] mission BEFORE: temp={old_temp:?}C qnh={old_qnh:?}mmHg turbulence={old_turb:?} \
             wind atGround={} at2000={} at8000={}",
            layer("atGround"),
            layer("at2000"),
            layer("at8000"),
        );
    }

    season
        .raw_set("temperature", temp_c.round() as i64)
        .context("setting weather.season.temperature")?;
    weather
        .raw_set("qnh", qnh_mmhg)
        .context("setting weather.qnh")?;
    // Sanity cap so a bad reading can't drop a hurricane on the server. Kept
    // deliberately low -- DCS blends this surface layer into the 2000 m layer
    // below, so a stiff surface wind plus an even stiffer layer just above it
    // makes low-level flight (helicopters especially) miserable.
    let wind_speed_uncapped = wind_speed_ms;
    let wind_speed_ms = wind_speed_ms.clamp(0.0, 8.0);
    if wind_speed_uncapped > wind_speed_ms {
        log::info!(
            "[LIVE_WX] ground wind clamped {wind_speed_uncapped:.1} -> {wind_speed_ms:.1}m/s"
        );
    }
    let wind: Table = weather.raw_get("wind").context("getting weather.wind table")?;
    let at_ground: Table = wind
        .raw_get("atGround")
        .context("getting weather.wind.atGround table")?;
    at_ground
        .raw_set("speed", wind_speed_ms)
        .context("setting weather.wind.atGround.speed")?;
    at_ground
        .raw_set("dir", wind_to_dir.round() as i64)
        .context("setting weather.wind.atGround.dir")?;
    log::info!(
        "[LIVE_WX] set atGround -> {wind_speed_ms:.1}m/s @ {}deg",
        wind_to_dir.round() as i64
    );

    // Upper wind layers: DCS wants a speed/dir at 2000 m and 8000 m MSL. Pull
    // them from the 800 hPa (~1900 m) and 300 hPa (~9200 m) pressure levels of
    // the hourly forecast, indexed by the current UTC hour. Clamp harder than
    // the surface -- real jet-stream winds (50+ m/s) are legal but make the
    // server miserable to fly. Falls back to the authored values if the data
    // is missing.
    let hour_idx = Utc::now().hour() as usize;
    let hourly = resp.get("hourly");
    let upper = |field_speed: &str, field_dir: &str, layer: &str, cap: f64| -> Result<()> {
        let (Some(spd_raw), Some(dir_from)) = (
            hourly
                .and_then(|h| h.get(field_speed))
                .and_then(|v| v.as_array())
                .and_then(|a| a.get(hour_idx))
                .and_then(|v| v.as_f64()),
            hourly
                .and_then(|h| h.get(field_dir))
                .and_then(|v| v.as_array())
                .and_then(|a| a.get(hour_idx))
                .and_then(|v| v.as_f64()),
        ) else {
            log::warn!(
                "[LIVE_WX] {layer}: no data for {field_speed}/{field_dir} at hour {hour_idx} \
                 -- leaving the mission's authored value untouched"
            );
            return Ok(());
        };
        let spd_conv = wind_to_ms(spd_raw, hourly_wind_unit);
        let spd = spd_conv.clamp(0.0, cap);
        let dir_to = ((dir_from + 180.0) % 360.0).round() as i64;
        let t: Table = wind
            .raw_get(layer)
            .with_context(|| format!("getting weather.wind.{layer} table"))?;
        let old_spd: f64 = t.raw_get("speed").unwrap_or(f64::NAN);
        let old_dir: f64 = t.raw_get("dir").unwrap_or(f64::NAN);
        t.raw_set("speed", spd)
            .with_context(|| format!("setting weather.wind.{layer}.speed"))?;
        t.raw_set("dir", dir_to)
            .with_context(|| format!("setting weather.wind.{layer}.dir"))?;
        log::info!(
            "[LIVE_WX] {layer}: API {spd_raw} {hourly_wind_unit:?} -> {spd_conv:.1}m/s{} \
             from {dir_from:.0}deg -> toward {dir_to}deg; mission {old_spd:.1}m/s@{old_dir:.0} \
             -> {spd:.1}m/s@{dir_to}",
            if spd_conv > spd { format!(" (clamped to {cap:.0})") } else { String::new() },
        );
        Ok(())
    };
    upper("wind_speed_800hPa", "wind_direction_800hPa", "at2000", 12.0)?;
    upper("wind_speed_300hPa", "wind_direction_300hPa", "at8000", 20.0)?;

    // groundTurbulence adds gustiness ON TOP of the steady wind speed above --
    // we have no live gust data, but the mission's authored default (often
    // set high for testing) was otherwise left untouched, which made flights
    // feel far windier than the displayed/ATIS wind speed implied. Derive a
    // modest gust component from the wind itself instead.
    let old_turbulence: f64 = weather.raw_get("groundTurbulence").unwrap_or(f64::NAN);
    let ground_turbulence = (wind_speed_ms * 0.15).min(1.5);
    weather
        .raw_set("groundTurbulence", ground_turbulence)
        .context("setting weather.groundTurbulence")?;
    log::info!(
        "[LIVE_WX] set groundTurbulence {old_turbulence:.1} -> {ground_turbulence:.1}m/s"
    );

    let (preset, cloud_base_m) = select_cloud_preset(cloud_cover_pct, precipitation_mm);
    let clouds: Table = weather.raw_get("clouds").context("getting weather.clouds table")?;
    match preset {
        Some(name) => clouds.raw_set("preset", name).context("setting clouds.preset")?,
        // no preset key at all for clear skies, matching DCS's own
        // "Preset00 - Nothing" template
        None => clouds.raw_set("preset", Value::Nil).context("clearing clouds.preset")?,
    }
    clouds.raw_set("density", 0).context("setting clouds.density")?;
    clouds.raw_set("thickness", 200).context("setting clouds.thickness")?;
    clouds.raw_set("base", cloud_base_m).context("setting clouds.base")?;
    let iprecptns = if preset.is_some_and(|p| p.starts_with("Rainy")) { 1 } else { 0 };
    clouds.raw_set("iprecptns", iprecptns).context("setting clouds.iprecptns")?;

    log::info!(
        "[LIVE_WX] DONE at ({lat}, {lon}): {}C, {qnh_mmhg}mmHg, ground wind {}m/s @ {}deg, turbulence {ground_turbulence:.1}m/s, cloud cover {cloud_cover_pct}% (preset {}, base {cloud_base_m}m, iprecptns {iprecptns})",
        temp_c.round() as i64,
        wind_speed_ms,
        wind_to_dir.round() as i64,
        preset.unwrap_or("none"),
    );
    Ok(())
}

/// apply real-world weather to the mission's weather table: temperature, QNH,
/// wind (ground plus two upper bands), clouds, and fog/dust obscurant state.
///
/// The station's decoded METAR is the whole surface layer, and its own
/// coordinates are where open-meteo (no API key required) is sampled for the
/// winds aloft, which METAR has nothing to say about. One ICAO is therefore
/// the entire configuration -- there is no lat/lon to keep in sync with it.
///
/// Both fetches are required: if either service is down this returns an error
/// rather than a half-real sky, and the caller is expected to fall back to the
/// weather authored in the options template.
///
/// Winds are clamped hard (ground 8, 2000 m 12, 8000 m 20 m/s) -- DCS blends
/// the layers, so a stiff upper wind drags low-level flight around; the caps
/// keep it flyable, especially for helicopters.
pub fn apply_metar_station_weather(
    mission: &Table,
    checkwx_key: &str,
    metar_station: &str,
) -> Result<()> {
    let metar = fetch_checkwx_surface(checkwx_key, metar_station)
        .with_context(|| format!("fetching METAR for {metar_station}"))?;
    log::info!(
        "CheckWX {metar_station}: temp={:.0}C qnh={} wind={:.1}m/s@{:.0} cloud={:.0}% precip={:.1}mm",
        metar.temp_c,
        metar.pressure_hpa.map(|p| format!("{p:.0}hPa")).unwrap_or_else(|| "not reported".into()),
        metar.wind_speed_ms,
        metar.wind_from_dir,
        metar.cloud_cover_pct,
        metar.precipitation_mm,
    );
    let (lat, lon) = match (metar.lat, metar.lon) {
        (Some(lat), Some(lon)) => (lat, lon),
        _ => anyhow::bail!(
            "checkwx has no position for station {metar_station} -- is that a real ICAO?"
        ),
    };
    log::info!("winds aloft sampled at {metar_station} ({lat}, {lon})");
    let url = format!(
        "https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&current=\
         wind_speed_700hPa,wind_direction_700hPa,\
         wind_speed_300hPa,wind_direction_300hPa,\
         visibility\
         &wind_speed_unit=ms"
    );
    let body = ureq::get(&url)
        .timeout(std::time::Duration::from_secs(10))
        .call()
        .context("requesting live weather from open-meteo")?
        .into_string()
        .context("reading live weather response body")?;
    let resp: serde_json::Value =
        serde_json::from_str(&body).context("parsing live weather response")?;
    let current = resp
        .get("current")
        .context("live weather response missing 'current'")?;
    let get = |key: &'static str| -> Result<f64> {
        current
            .get(key)
            .and_then(|v| v.as_f64())
            .with_context(|| format!("live weather response missing {key}"))
    };
    // open-meteo is asked for nothing the METAR already answers: only the two
    // upper wind bands, and the visibility the fog/dust layer is derived from.
    let wind_speed_2000 = get("wind_speed_700hPa")?;
    let wind_from_dir_2000 = get("wind_direction_700hPa")?;
    let wind_speed_8000 = get("wind_speed_300hPa")?;
    let wind_from_dir_8000 = get("wind_direction_300hPa")?;
    let visibility_m = get("visibility")?;

    // the surface layer is the station's, in full.
    let temp_c = metar.temp_c;
    let pressure_hpa = metar.pressure_hpa;
    let wind_speed_ground = metar.wind_speed_ms;
    let wind_from_dir_ground = metar.wind_from_dir;
    let cloud_cover_pct = metar.cloud_cover_pct;
    let precipitation_mm = metar.precipitation_mm;

    // Sanity caps -- DCS interpolates between bands, so a stiff wind aloft
    // drags the low-altitude wind up. Keep it flyable.
    let wind_speed_ground = wind_speed_ground.clamp(0.0, 8.0);
    let wind_speed_2000 = wind_speed_2000.clamp(0.0, 12.0);
    let wind_speed_8000 = wind_speed_8000.clamp(0.0, 20.0);

    // DCS's wind direction is the direction the wind blows TOWARD, the
    // opposite of the real-world meteorological "from" convention
    let to_dir = |from_dir: f64| (from_dir + 180.0) % 360.0;
    let qnh_mmhg = pressure_hpa.map(|p| (p * 0.750062).round() as i64);

    let weather: Table = mission.raw_get("weather").context("getting weather table")?;
    let season: Table = weather
        .raw_get("season")
        .context("getting weather.season table")?;
    season
        .raw_set("temperature", temp_c.round() as i64)
        .context("setting weather.season.temperature")?;
    match qnh_mmhg {
        Some(q) => weather.raw_set("qnh", q).context("setting weather.qnh")?,
        None => log::warn!("METAR for {metar_station} has no pressure, keeping the template's QNH"),
    }
    // Derive a modest gust component from the (clamped) surface wind rather
    // than leaving whatever the template authored -- a high authored
    // groundTurbulence makes flights feel far windier than the reported speed.
    let ground_turbulence = (wind_speed_ground * 0.15).min(1.5);
    weather
        .raw_set("groundTurbulence", ground_turbulence)
        .context("setting weather.groundTurbulence")?;

    let wind: Table = weather.raw_get("wind").context("getting weather.wind table")?;
    let set_wind_band = |band: &'static str, speed: f64, dir: f64| -> Result<()> {
        let t: Table = wind
            .raw_get(band)
            .with_context(|| format!("getting weather.wind.{band} table"))?;
        t.raw_set("speed", speed)
            .with_context(|| format!("setting weather.wind.{band}.speed"))?;
        t.raw_set("dir", dir.round() as i64)
            .with_context(|| format!("setting weather.wind.{band}.dir"))?;
        Ok(())
    };
    set_wind_band("atGround", wind_speed_ground, to_dir(wind_from_dir_ground))?;
    // at2000/at8000 are approximated from the nearest standard pressure
    // levels (700hPa ~ 3000m, 300hPa ~ 9000m in a standard atmosphere) --
    // open-meteo doesn't offer wind at DCS's exact meter-based altitude
    // bands, but real directional shear is still far better than repeating
    // the ground reading, and this mirrors what dcs-real-weather itself
    // does when its OpenMeteo winds-aloft provider is enabled.
    set_wind_band("at2000", wind_speed_2000, to_dir(wind_from_dir_2000))?;
    set_wind_band("at8000", wind_speed_8000, to_dir(wind_from_dir_8000))?;

    let (preset, cloud_base_m) = select_cloud_preset(cloud_cover_pct, precipitation_mm);
    let clouds: Table = weather.raw_get("clouds").context("getting weather.clouds table")?;
    match preset {
        Some(name) => clouds.raw_set("preset", name).context("setting clouds.preset")?,
        // no preset key at all for clear skies, matching DCS's own
        // "Preset00 - Nothing" template
        None => clouds.raw_set("preset", Value::Nil).context("clearing clouds.preset")?,
    }
    // density/thickness must stay at these fixed values -- in the new
    // preset-driven system the preset itself bakes in the actual cloud
    // appearance, and these fields are only left over from the old system
    clouds.raw_set("density", 0).context("setting clouds.density")?;
    clouds.raw_set("thickness", 200).context("setting clouds.thickness")?;
    clouds.raw_set("base", cloud_base_m).context("setting clouds.base")?;
    // rain only -- DCS's snow precipitation constant isn't confidently
    // known here, so snowfall is intentionally left unhandled rather than
    // risk setting the wrong effect
    let iprecptns = if preset.is_some_and(|p| p.starts_with("Rainy")) { 1 } else { 0 };
    clouds.raw_set("iprecptns", iprecptns).context("setting clouds.iprecptns")?;

    log::info!(
        "applied live weather at ({lat}, {lon}) to mission: {}C, {}mmHg, ground wind {wind_speed_ground}m/s @ {}deg, \
         cloud cover {cloud_cover_pct}% (preset {}, base {cloud_base_m}m), precipitation {precipitation_mm}mm",
        temp_c.round() as i64,
        qnh_mmhg.map(|q| q.to_string()).unwrap_or_else(|| "template".into()),
        to_dir(wind_from_dir_ground).round() as i64,
        preset.unwrap_or("none"),
    );

    // Its dust fetch happens after the rest is written, so a failure here
    // must not undo the build: the template's fog/dust simply stays.
    if let Err(e) = apply_live_obscurants(&weather, lat, lon, visibility_m) {
        log::warn!("live fog/dust unavailable, keeping the template's: {e:#}");
    }
    Ok(())
}

/// Fog and dust storm are mutually exclusive in DCS -- the Mission Editor
/// disables one when the other is enabled -- so this fetches real dust
/// concentration (open-meteo's air quality API, a separate host from the
/// main weather API above) and picks at most one obscurant: dust if the air
/// quality reading calls for it, otherwise fog if ground visibility is
/// poor, otherwise neither.
///
/// dust_density and fog.visibility are both actually visibility distances
/// in meters despite the field name "density" -- lower means a worse storm.
/// Confirmed against the mission editor: a saved dust storm showing
/// "visibility: 5000 feet" in the UI serialized to `dust_density = 1524`,
/// which is exactly 5000ft in meters.
const DUST_ON_THRESHOLD_UGM3: f64 = 50.0;
const DUST_MAX_UGM3: f64 = 800.0;
const DUST_VISIBILITY_MIN_M: f64 = 300.0;
const DUST_VISIBILITY_MAX_M: f64 = 3000.0;
const FOG_ON_THRESHOLD_M: f64 = 3000.0;

fn apply_live_obscurants(weather: &Table, lat: f64, lon: f64, visibility_m: f64) -> Result<()> {
    let url = format!(
        "https://air-quality-api.open-meteo.com/v1/air-quality?latitude={lat}&longitude={lon}&current=dust"
    );
    let body = ureq::get(&url)
        .timeout(std::time::Duration::from_secs(10))
        .call()
        .context("requesting live dust data from open-meteo")?
        .into_string()
        .context("reading live dust response body")?;
    let resp: serde_json::Value =
        serde_json::from_str(&body).context("parsing live dust response")?;
    let dust_ugm3 = resp.get("current").and_then(|c| c.get("dust")).and_then(|v| v.as_f64());

    let fog: Table = weather.raw_get("fog").context("getting weather.fog table")?;
    let dust_active = match dust_ugm3 {
        Some(d) if d >= DUST_ON_THRESHOLD_UGM3 => Some(d),
        _ => None,
    };

    if let Some(dust_ugm3) = dust_active {
        let frac = ((dust_ugm3 - DUST_ON_THRESHOLD_UGM3) / (DUST_MAX_UGM3 - DUST_ON_THRESHOLD_UGM3))
            .clamp(0.0, 1.0);
        // worse dust (higher ug/m3) -> lower visibility
        let visibility =
            (DUST_VISIBILITY_MAX_M - frac * (DUST_VISIBILITY_MAX_M - DUST_VISIBILITY_MIN_M)).round() as i64;
        weather.raw_set("enable_dust", true).context("enabling dust storm")?;
        weather.raw_set("dust_density", visibility).context("setting dust_density")?;
        weather.raw_set("enable_fog", false).context("disabling fog for dust")?;
        log::info!(
            "live dust reading {dust_ugm3}ug/m3 at ({lat}, {lon}) -> dust storm enabled, visibility {visibility}m"
        );
    } else if visibility_m < FOG_ON_THRESHOLD_M {
        weather.raw_set("enable_fog", true).context("enabling fog")?;
        weather.raw_set("enable_dust", false).context("disabling dust for fog")?;
        fog.raw_set("visibility", visibility_m.round() as i64)
            .context("setting fog.visibility")?;
        fog.raw_set("thickness", 200).context("setting fog.thickness")?;
        log::info!("live visibility {visibility_m}m at ({lat}, {lon}) -> fog enabled");
    } else {
        weather.raw_set("enable_dust", false).context("disabling dust storm")?;
        weather.raw_set("enable_fog", false).context("disabling fog")?;
        match dust_ugm3 {
            Some(d) => log::info!(
                "live dust reading {d}ug/m3, visibility {visibility_m}m at ({lat}, {lon}) -- clear, dust and fog disabled"
            ),
            None => log::warn!(
                "no live dust reading available at ({lat}, {lon}); visibility {visibility_m}m -- clear, dust and fog disabled"
            ),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn cloud_preset_by_cover_and_rain() {
        assert_eq!(select_cloud_preset(0., 0.), (None, 4200));
        assert_eq!(select_cloud_preset(6., 0.), (None, 4200));
        assert_eq!(select_cloud_preset(20., 0.), (Some("Preset2"), 2500));
        assert_eq!(select_cloud_preset(40., 0.), (Some("Preset6"), 2500));
        assert_eq!(select_cloud_preset(60., 0.), (Some("Preset14"), 2500));
        assert_eq!(select_cloud_preset(100., 0.), (Some("Preset22"), 2500));
        // any real rain wins over cover, heavier rain lowers the base
        assert_eq!(select_cloud_preset(0., 0.5), (Some("RainyPreset1"), 2900));
        assert_eq!(select_cloud_preset(100., 3.), (Some("RainyPreset2"), 2500));
        assert_eq!(select_cloud_preset(100., 8.), (Some("RainyPreset3"), 1700));
        // a trace is not rain
        assert_eq!(select_cloud_preset(0., 0.2), (None, 4200));
    }

    #[test]
    fn wind_units() {
        assert_eq!(wind_to_ms(5., "m/s"), 5.);
        assert_eq!(wind_to_ms(5., "ms"), 5.);
        assert!((wind_to_ms(36., "km/h") - 10.).abs() < 1e-9);
        assert!((wind_to_ms(10., "kn") - 5.14444).abs() < 1e-9);
        assert!((wind_to_ms(10., "mph") - 4.4704).abs() < 1e-9);
        // unlabelled: plausible values pass, a "gale" is taken as km/h
        assert_eq!(wind_to_ms(12., ""), 12.);
        assert!((wind_to_ms(72., "") - 20.).abs() < 1e-9);
    }

    #[test]
    fn metar_decode() {
        let resp = json!({ "data": [{
            "temperature": { "celsius": 27 },
            "barometer": { "hpa": 1009 },
            "wind": { "degrees": 90, "speed_kts": 10 },
            "clouds": [{ "code": "FEW" }, { "code": "BKN" }],
            "conditions": [{ "code": "-RA" }, { "code": "BR" }],
            "station": { "geometry": { "coordinates": [144.79, 13.48] } }
        }]});
        let m = decode_checkwx_metar(&resp).unwrap();
        assert_eq!(m.temp_c, 27.);
        assert_eq!(m.pressure_hpa, Some(1009.));
        assert!((m.wind_speed_ms - 5.14444).abs() < 1e-9);
        assert_eq!(m.wind_from_dir, 90.);
        // the densest layer wins
        assert_eq!(m.cloud_cover_pct, 75.);
        // light rain; mist is not precipitation
        assert_eq!(m.precipitation_mm, 0.5);
        // GeoJSON order: [lon, lat]
        assert_eq!((m.lat, m.lon), (Some(13.48), Some(144.79)));
    }

    #[test]
    fn metar_decode_edge_cases() {
        // calm, no clouds, pressure only in inHg, no station position
        let resp = json!({ "data": [{
            "temperature": { "celsius": -5 },
            "barometer": { "hg": 29.92 },
            "wind": null,
            "conditions": [{ "code": "+SN" }]
        }]});
        let m = decode_checkwx_metar(&resp).unwrap();
        assert_eq!((m.wind_speed_ms, m.wind_from_dir), (0., 0.));
        assert_eq!(m.cloud_cover_pct, 0.);
        assert_eq!(m.precipitation_mm, 8.);
        assert!((m.pressure_hpa.unwrap() - 1013.21).abs() < 0.01);
        assert_eq!((m.lat, m.lon), (None, None));
        // no pressure in any unit is not an error
        let resp = json!({ "data": [{ "temperature": { "celsius": 10 } }] });
        assert_eq!(decode_checkwx_metar(&resp).unwrap().pressure_hpa, None);
        // a bad station or key comes back with no data
        assert!(decode_checkwx_metar(&json!({ "results": 0, "data": [] })).is_err());
        assert!(decode_checkwx_metar(&json!({ "data": [{}] })).is_err());
    }

    #[test]
    fn live_time_writes_the_offset_clock() {
        let lua = mlua::Lua::new();
        let mission: Table = lua
            .load("{ date = { Day = 1, Month = 1, Year = 2000 }, start_time = 0 }")
            .eval()
            .unwrap();
        apply_live_time(&mission, Some(10.)).unwrap();
        let now = Utc::now().with_timezone(&FixedOffset::east_opt(10 * 3600).unwrap());
        let date: Table = mission.raw_get("date").unwrap();
        assert_eq!(date.raw_get::<_, i64>("Year").unwrap(), now.year() as i64);
        let start: i64 = mission.raw_get("start_time").unwrap();
        let expect = now.hour() as i64 * 3600 + now.minute() as i64 * 60 + now.second() as i64;
        // within a few seconds, allowing for midnight
        assert!((start - expect).rem_euclid(86400) < 5 || (expect - start).rem_euclid(86400) < 5);
        assert!(apply_live_time(&mission, Some(30.)).is_err());
    }
}
