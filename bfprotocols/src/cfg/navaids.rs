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

//! Navaids (TACAN/NDB pools).

use super::*;

/// An inclusive numeric range (used for TACAN channel and NDB kHz pools).
#[derive(Debug, Clone, Copy, Serialize, Deserialize, schemars::JsonSchema)]
pub struct NavRange {
    pub lo: u16,
    pub hi: u16,
}

impl NavRange {
    pub fn contains(&self, v: u16) -> bool {
        v >= self.lo && v <= self.hi
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct NavaidsCfg {
    /// Master switch. When false no navaids are generated or broadcast.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Two navaids on the same band are never assigned within this range of
    /// each other (nautical miles). Default 150.
    #[serde(default = "default_navaid_separation_nm")]
    pub min_separation_nm: f64,
    /// TACAN channel pool for blue-owned objectives. Default 2..=62.
    #[serde(default = "default_blue_tacan")]
    pub blue_tacan: NavRange,
    /// TACAN channel pool for red-owned objectives. Default 63..=125.
    #[serde(default = "default_red_tacan")]
    pub red_tacan: NavRange,
    /// Band (X or Y) for generated ground TACAN beacons. Default Y.
    #[serde(default = "default_navaid_band")]
    pub tacan_band: TacanBand,
    /// Red-owned ground objectives (FARP/Logistics/Naval Base) get an NDB
    /// homer only -- no TACAN, since Russian-pattern aircraft home on ADB/ARK,
    /// not TACAN. Blue-owned objectives get TACAN + NDB. Follows current
    /// ownership, so a captured objective switches to its captor's set.
    /// Default true.
    #[serde(default = "default_true")]
    pub red_ground_ndb_only: bool,
    /// A ship gets the full CATOBAR suite (TACAN + ICLS + ACLS + Link-4) iff its
    /// unit type name contains one of these substrings -- keyed to the ship, not
    /// the owning coalition, and applied per ship, so a task force with several
    /// carriers gets an independent set on each. Default: the DCS US CVN decks.
    #[serde(default = "default_western_carriers")]
    pub western_carrier_types: Vec<String>,
    /// Ships that carry aircraft but have no cats/traps: TACAN + ICLS only, no
    /// ACLS/Link-4 (LHA/LHD amphibs). Applied per ship, same as above.
    #[serde(default = "default_helo_carriers")]
    pub helo_carrier_types: Vec<String>,
    /// Generate NDB homers as well as TACAN. Default true.
    #[serde(default = "default_true")]
    pub ndb_enabled: bool,
    /// NDB frequency pool in kHz. Default 200..=1400.
    #[serde(default = "default_ndb_khz")]
    pub ndb_khz: NavRange,
    /// Give FOBs an NDB homer (they never get TACAN). Default false.
    #[serde(default)]
    pub ndb_on_fob: bool,
    /// Generate carrier ICLS. Default true.
    #[serde(default = "default_true")]
    pub carrier_icls: bool,
    /// Generate carrier ACLS (auto-land). Default true.
    #[serde(default = "default_true")]
    pub carrier_acls: bool,
    /// Carrier Link-4 datalink frequency in MHz, or 0 to disable. Default 336.0.
    #[serde(default = "default_carrier_link4_mhz")]
    pub carrier_link4_mhz: f64,
}

pub(super) fn default_navaid_separation_nm() -> f64 {
    150.0
}

pub(super) fn default_blue_tacan() -> NavRange {
    NavRange { lo: 2, hi: 62 }
}

pub(super) fn default_red_tacan() -> NavRange {
    NavRange { lo: 63, hi: 125 }
}

pub(super) fn default_navaid_band() -> TacanBand {
    TacanBand::Y
}

pub(super) fn default_ndb_khz() -> NavRange {
    NavRange { lo: 200, hi: 1400 }
}

pub(super) fn default_carrier_link4_mhz() -> f64 {
    336.0
}

pub(super) fn default_western_carriers() -> Vec<String> {
    ["CVN_", "Stennis", "Forrestal"].into_iter().map(String::from).collect()
}

pub(super) fn default_helo_carriers() -> Vec<String> {
    ["LHA_Tarawa", "Tarawa", "LHA", "LHD"].into_iter().map(String::from).collect()
}

impl Default for NavaidsCfg {
    fn default() -> Self {
        Self {
            enabled: true,
            min_separation_nm: default_navaid_separation_nm(),
            blue_tacan: default_blue_tacan(),
            red_tacan: default_red_tacan(),
            tacan_band: default_navaid_band(),
            red_ground_ndb_only: true,
            western_carrier_types: default_western_carriers(),
            helo_carrier_types: default_helo_carriers(),
            ndb_enabled: true,
            ndb_khz: default_ndb_khz(),
            ndb_on_fob: false,
            carrier_icls: true,
            carrier_acls: true,
            carrier_link4_mhz: default_carrier_link4_mhz(),
        }
    }
}

pub(super) fn default_repair_supply_cost() -> u8 {
    5
}

pub(super) fn default_deploy_supply_cost() -> u8 {
    3
}

pub(super) fn default_artillery_min_range() -> u32 {
    1500
}

pub(super) fn default_sam_capture_radius() -> f64 {
    609.6 // 2000 ft
}

pub(super) fn default_red_country() -> Country {
    Country::CJTF_RED
}

pub(super) fn default_blue_country() -> Country {
    Country::CJTF_BLUE
}
