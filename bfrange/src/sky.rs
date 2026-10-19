// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! The sky over the range for the site's weather briefing: the mission's
//! clouds, visibility, precipitation and dust (`env.mission.weather`), and
//! the fog as it is now (`world.weather`, which a mission can animate).
//! Read with every live picture: a handful of table lookups.

use bfprotocols::range::SkyInfo;
use dcso3::{LuaEnv, MizLua};
use mlua::prelude::*;

/// The Mission Editor's cloud presets: what it calls each, and the METAR
/// cover of its main layer.
fn preset(name: &str) -> Option<(&'static str, &'static str)> {
    Some(match name {
        "Preset1" => ("Light Scattered 1", "FEW"),
        "Preset2" => ("Light Scattered 2", "FEW"),
        "Preset3" => ("High Scattered 1", "FEW"),
        "Preset4" => ("High Scattered 2", "FEW"),
        "Preset5" => ("Scattered 1", "SCT"),
        "Preset6" => ("Scattered 2", "SCT"),
        "Preset7" => ("Scattered 3", "SCT"),
        "Preset8" => ("High Scattered 3", "SCT"),
        "Preset9" => ("Scattered 4", "SCT"),
        "Preset10" => ("Scattered 5", "SCT"),
        "Preset11" => ("Scattered 6", "SCT"),
        "Preset12" => ("Scattered 7", "SCT"),
        "Preset13" => ("Broken 1", "BKN"),
        "Preset14" => ("Broken 2", "BKN"),
        "Preset15" => ("Broken 3", "BKN"),
        "Preset16" => ("Broken 4", "BKN"),
        "Preset17" => ("Broken 5", "BKN"),
        "Preset18" => ("Broken 6", "BKN"),
        "Preset19" => ("Broken 7", "BKN"),
        "Preset20" => ("Broken 8", "BKN"),
        "Preset21" => ("Overcast 1", "OVC"),
        "Preset22" => ("Overcast 2", "OVC"),
        "Preset23" => ("Overcast 3", "OVC"),
        "Preset24" => ("Overcast 4", "OVC"),
        "Preset25" => ("Overcast 5", "OVC"),
        "Preset26" => ("Overcast 6", "OVC"),
        "Preset27" => ("Overcast 7", "OVC"),
        "RainyPreset1" => ("Overcast And Rain 1", "OVC"),
        "RainyPreset2" => ("Overcast And Rain 2", "OVC"),
        "RainyPreset3" => ("Overcast And Rain 3", "OVC"),
        _ => return None,
    })
}

/// Legacy (density 0..10) clouds as a METAR cover.
fn cover_of_density(d: f64) -> Option<&'static str> {
    match d.round() as i64 {
        i64::MIN..=0 => None,
        1..=2 => Some("FEW"),
        3..=5 => Some("SCT"),
        6..=8 => Some("BKN"),
        _ => Some("OVC"),
    }
}

pub fn read(lua: MizLua, ground_m: f64) -> Option<SkyInfo> {
    let r = (|| -> LuaResult<SkyInfo> {
        let g = lua.inner().globals();
        let env: LuaTable = g.raw_get("env")?;
        let mission: LuaTable = env.raw_get("mission")?;
        let wx: LuaTable = mission.raw_get("weather")?;
        let clouds: Option<LuaTable> = wx.raw_get("clouds").ok();
        let num = |k: &str| clouds.as_ref().and_then(|c| c.raw_get::<_, Option<f64>>(k).ok().flatten());
        let preset_key: Option<String> =
            clouds.as_ref().and_then(|c| c.raw_get::<_, Option<String>>("preset").ok().flatten()).filter(|s| !s.is_empty());
        let named = preset_key.as_deref().and_then(preset);
        let cover = match (&preset_key, named) {
            (Some(_), Some((_, c))) => Some(c),
            // a preset this table does not know yet: clouds of some kind
            (Some(_), None) => Some("SCT"),
            (None, _) => cover_of_density(num("density").unwrap_or(0.)),
        };
        let base = num("base");
        let thick = num("thickness");
        let mut precip = match num("iprecptns").unwrap_or(0.).round() as i64 {
            1 => "rain",
            2 => "thunderstorm",
            3 => "snow",
            4 => "snowstorm",
            _ => "none",
        };
        if precip == "none" && preset_key.as_deref().is_some_and(|p| p.starts_with("Rainy")) {
            precip = "rain";
        }
        let mut vis: f64 = wx
            .raw_get::<_, Option<LuaTable>>("visibility")?
            .and_then(|v| v.raw_get::<_, Option<f64>>("distance").ok().flatten())
            .unwrap_or(80_000.);
        // fog as it is now: a mission can thicken or lift it as it runs
        let live_fog = (|| -> LuaResult<(f64, f64)> {
            let world: LuaTable = g.raw_get("world")?;
            let weather: LuaTable = world.raw_get("weather")?;
            let v: f64 = weather.call_function("getFogVisibilityDistance", ())?;
            let t: f64 = weather.call_function("getFogThickness", ())?;
            Ok((v, t))
        })();
        let (fog_vis, fog_thick) = match live_fog {
            Ok((v, t)) if v > 0. && t > 0. => (Some(v), Some(t)),
            Ok(_) => (None, None),
            Err(_) => {
                // no live fog API: the mission's own setting
                let on: bool = wx.raw_get::<_, Option<bool>>("enable_fog")?.unwrap_or(false);
                let fog: Option<LuaTable> = wx.raw_get("fog").ok();
                let v = fog.as_ref().and_then(|f| f.raw_get::<_, Option<f64>>("visibility").ok().flatten());
                let t = fog.as_ref().and_then(|f| f.raw_get::<_, Option<f64>>("thickness").ok().flatten());
                if on { (v.filter(|v| *v > 0.), t.filter(|t| *t > 0.)) } else { (None, None) }
            }
        };
        if let Some(f) = fog_vis {
            vis = vis.min(f);
        }
        let dust = if wx.raw_get::<_, Option<bool>>("enable_dust")?.unwrap_or(false) {
            wx.raw_get::<_, Option<f64>>("dust_density")?.filter(|d| *d > 0.)
        } else {
            None
        };
        if let Some(d) = dust {
            vis = vis.min(d);
        }
        Ok(SkyInfo {
            preset: named.map(|(n, _)| n.to_string()).or(preset_key),
            cover: cover.map(str::to_string),
            base_m: cover.and(base),
            tops_m: if named.is_none() && cover.is_some() { base.zip(thick).map(|(b, t)| b + t) } else { None },
            precip: precip.into(),
            visibility_m: vis,
            fog_visibility_m: fog_vis,
            fog_thickness_m: fog_thick,
            dust_visibility_m: dust,
            ground_m,
        })
    })();
    r.ok()
}
