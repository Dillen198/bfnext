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
use chrono::{Datelike, Timelike};
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

/// fetch current real-world weather at (lat, lon) from open-meteo.com (no
/// API key required) and apply ground-level temperature, QNH, wind, and
/// clouds to the mission's weather table. Upper winds are left as authored,
/// since accurate free data for them isn't readily available.
fn apply_live_weather(mission: &Table, lat: f64, lon: f64) -> Result<()> {
    let url = format!(
        "https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&current=temperature_2m,pressure_msl,wind_speed_10m,wind_direction_10m,cloud_cover,precipitation&wind_speed_unit=ms"
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
    let temp_c = current
        .get("temperature_2m")
        .and_then(|v| v.as_f64())
        .context("live weather response missing temperature_2m")?;
    let pressure_hpa = current
        .get("pressure_msl")
        .and_then(|v| v.as_f64())
        .context("live weather response missing pressure_msl")?;
    let wind_speed_ms = current
        .get("wind_speed_10m")
        .and_then(|v| v.as_f64())
        .context("live weather response missing wind_speed_10m")?;
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

    let weather: Table = mission.raw_get("weather").context("getting weather table")?;
    let season: Table = weather
        .raw_get("season")
        .context("getting weather.season table")?;
    season
        .raw_set("temperature", temp_c.round() as i64)
        .context("setting weather.season.temperature")?;
    weather
        .raw_set("qnh", qnh_mmhg)
        .context("setting weather.qnh")?;
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
        "applied live weather at ({lat}, {lon}) to mission: {}C, {qnh_mmhg}mmHg, ground wind {}m/s @ {}deg, cloud cover {cloud_cover_pct}% (preset {}, base {cloud_base_m}m) (upper winds left as authored)",
        temp_c.round() as i64,
        wind_speed_ms,
        wind_to_dir.round() as i64,
        preset.unwrap_or("none"),
    );
    Ok(())
}
