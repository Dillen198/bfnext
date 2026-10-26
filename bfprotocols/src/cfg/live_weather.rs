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

//! Live weather/time (see the livewx crate) and the `Secret` type.

use super::*;

/// A credential in the config (an API key). Serializes as the plain string,
/// so the config file round-trips and `Cfg::load` can write it back, but
/// `Debug` and `Display` print `<redacted>`: a logged or `{:?}`-dumped config
/// must not leak it.
#[derive(Clone, Default, PartialEq, Eq, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(transparent)]
pub struct Secret(String);

impl Secret {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    /// The secret itself, for the one place that sends it.
    pub fn expose(&self) -> &str {
        &self.0
    }
}

impl std::ops::Deref for Secret {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

impl From<String> for Secret {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for Secret {
    fn from(s: &str) -> Self {
        Self(s.into())
    }
}

impl fmt::Debug for Secret {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(if self.0.is_empty() { "\"\"" } else { "<redacted>" })
    }
}

impl fmt::Display for Secret {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

/// Syncs the mission file's ground-level weather (and optionally date/time)
/// with real-world conditions before each scheduled restart. DCS has no API
/// to change an already-running mission's weather or clock, so this rewrites
/// the .miz on disk before bflib triggers the process restart; the new
/// conditions take effect the next time the mission loads, same approach as
/// the dcs-real-weather project. The training range uses the same settings
/// (`RangeCfg::live_weather`); the work itself is the `livewx` crate.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct LiveWeatherConfig {
    /// latitude to fetch live weather for
    pub lat: f64,
    /// longitude to fetch live weather for
    pub lon: f64,
    /// also set the mission's date/start_time to the real-world local date
    /// and time (see `utc_offset_hours` for whose)
    #[serde(default)]
    pub sync_time: bool,
    /// With `sync_time`: the theatre's offset from UTC in hours, so the
    /// mission clock is the real local time where the mission is set (10 for
    /// the Marianas, which keep no daylight saving). Unset, the clock of the
    /// machine running the server is used.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub utc_offset_hours: Option<f64>,
    /// checkwxapi.com API key. When set together with `metar_station`, the
    /// mission's *surface* layer (wind, temperature, QNH, cloud cover) is taken
    /// from that station's real decoded METAR instead of the open-meteo model.
    /// The winds-aloft layers still come from open-meteo (METAR has no upper
    /// air), and a failed/empty METAR fetch falls back to open-meteo for
    /// everything.
    #[serde(default)]
    pub checkwx_api_key: Option<Secret>,
    /// ICAO of the station whose METAR drives the surface layer -- pick the
    /// real-world field nearest the operating area (e.g. "LTAG" Incirlik,
    /// "OSDI" Damascus, "OLBA" Beirut, "OJAI" Amman). Only used when
    /// `checkwx_api_key` is also set.
    #[serde(default)]
    pub metar_station: Option<String>,
}
