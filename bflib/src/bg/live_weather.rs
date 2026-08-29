/*
Copyright 2024 Eric Stokes.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
for more details.
*/

//! DCS has no API to change an already-running mission's weather or clock,
//! so live weather sync works the same way as the dcs-real-weather project:
//! rewrite the .miz on disk right before bflib triggers the process restart
//! (see admin::admin_shutdown), so the new conditions take effect the next
//! time the mission loads.

use anyhow::{Context, Result};
use bfprotocols::cfg::LiveWeatherConfig;
use chrono::{Datelike, Timelike, Utc};
use dcso3::env::miz_pack::{read_table_from_miz, rewrite_entry_in_miz, serialize_to_lua};
use mlua::{Lua, Table, Value};
use std::path::PathBuf;

pub(super) struct LiveWeatherRequest {
    pub miz_path: PathBuf,
    pub cfg: LiveWeatherConfig,
}

pub(super) fn apply(req: &LiveWeatherRequest) -> Result<()> {
    let lua = Box::leak(Box::new(Lua::new()));
    let mission: Table = read_table_from_miz(lua, &req.miz_path, "mission")
        .context("reading mission table from miz")?;
    if req.cfg.sync_time {
        apply_live_time(&mission).context("applying live time")?;
    }
    apply_live_weather(&mission, req.cfg.lat, req.cfg.lon).context("applying live weather")?;
    let s = serialize_to_lua("mission", Value::Table(mission))
        .context("serializing updated mission table")?;
    rewrite_entry_in_miz(&req.miz_path, "mission", &s)
        .context("writing updated mission back into miz")?;
    Ok(())
}

/// set the mission's date and start_time to the current real-world local
/// date/time of the machine running the server
fn apply_live_time(mission: &Table) -> Result<()> {
    let now = chrono::Local::now();
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
fn select_cloud_preset(cover_pct: f64, precip_mm: f64) -> (Option<&'static str>, i64) {
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

/// fetch current real-world weather at (lat, lon) from open-meteo.com (no
/// API key required) and apply ground-level temperature, QNH, wind, clouds,
/// and the two upper wind layers (2000 m / 8000 m, from the 800 hPa / 300 hPa
/// pressure levels) to the mission's weather table.
fn apply_live_weather(mission: &Table, lat: f64, lon: f64) -> Result<()> {
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
    log::info!("[LIVE_WX] raw response: {body}");
    let resp: serde_json::Value =
        serde_json::from_str(&body).context("parsing live weather response")?;
    let current = resp
        .get("current")
        .context("live weather response missing 'current'")?;
    let temp_c = current
        .get("temperature_2m")
        .and_then(|v| v.as_f64())
        .context("live weather response missing temperature_2m")?;
    let pressure_hpa = current
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
    let wind_speed_ms = wind_to_ms(wind_speed_raw, wind_unit);
    let hourly_wind_unit = resp
        .get("hourly_units")
        .and_then(|u| u.get("wind_speed_800hPa"))
        .and_then(|v| v.as_str())
        .unwrap_or("");
    let wind_from_dir = current
        .get("wind_direction_10m")
        .and_then(|v| v.as_f64())
        .context("live weather response missing wind_direction_10m")?;
    let cloud_cover_pct = current
        .get("cloud_cover")
        .and_then(|v| v.as_f64())
        .context("live weather response missing cloud_cover")?;
    let precipitation_mm = current
        .get("precipitation")
        .and_then(|v| v.as_f64())
        .context("live weather response missing precipitation")?;
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
    // Sanity cap so a bad reading can't drop a hurricane on the server.
    let wind_speed_uncapped = wind_speed_ms;
    let wind_speed_ms = wind_speed_ms.clamp(0.0, 25.0);
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
    upper("wind_speed_800hPa", "wind_direction_800hPa", "at2000", 30.0)?;
    upper("wind_speed_300hPa", "wind_direction_300hPa", "at8000", 35.0)?;

    // groundTurbulence adds gustiness ON TOP of the steady wind speed above --
    // we have no live gust data, but the mission's authored default (often
    // set high for testing) was otherwise left untouched, which made flights
    // feel far windier than the displayed/ATIS wind speed implied. Derive a
    // modest gust component from the wind itself instead.
    let old_turbulence: f64 = weather.raw_get("groundTurbulence").unwrap_or(f64::NAN);
    let ground_turbulence = (wind_speed_ms * 0.2).min(3.0);
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
