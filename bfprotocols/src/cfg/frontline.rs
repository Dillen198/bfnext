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

//! The F10 frontline, factories and carriers.

use super::*;

pub(super) fn default_frontline_on_change_only() -> bool {
    true
}

pub(super) fn default_frontline_samples() -> usize {
    100
}

pub(super) fn default_territory_zone_alpha() -> f32 {
    0.15 // 15% opacity for subtle territory shading
}

pub(super) fn default_frontline_max_marks() -> usize {
    200
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct FrontLineConfig {
    /// Draw the frontline on the F10 map: dashed coloured segments along the
    /// Red/Blue territory boundary (blue where blue holds the adjacent ground,
    /// red where red does, white and dotted where genuinely contested).
    pub enabled: bool,
    /// Redraw only when an objective changes hands (recommended for performance).
    #[serde(default = "default_frontline_on_change_only")]
    pub update_on_objective_change_only: bool,
    /// Resolution of the ownership grid the boundary is traced from (higher =
    /// finer, more stair-steps resolved, slower). Clamped to 50-200.
    #[serde(default = "default_frontline_samples")]
    pub samples_per_boundary: usize,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// Upper bound on the number of line segments drawn. Fewer = lighter on the
    /// server and clients. Default: 200. The segment list is thinned to fit.
    #[serde(default = "default_frontline_max_marks")]
    pub max_marks: usize,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// Legacy: transparency of the old filled-territory shading. No longer used
    /// by the line renderer; kept so existing configs still parse.
    #[serde(default = "default_territory_zone_alpha")]
    pub territory_zone_alpha: f32,
}

impl Default for FrontLineConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            update_on_objective_change_only: default_frontline_on_change_only(),
            samples_per_boundary: default_frontline_samples(),
            max_marks: default_frontline_max_marks(),
            territory_zone_alpha: default_territory_zone_alpha(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct FactoryCfg {
    pub production_rate: u32,
    pub production_interval: u32,
}

/// Configuration for a carrier group
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct CarrierGroupCfg {
    /// The template name in the mission file (e.g., "BCARRIER", "RCARRIER")
    pub template: String,
    /// The display name for this carrier group (e.g., "CVN-73 Washington")
    pub display_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct CarrierCfg {
    pub repair_cost: u32,
    pub respawn_cost: u32,
    pub movement_speed: f64,
    /// Speed in m/s to use when repositioning carrier to last saved position after restart.
    /// Carriers always spawn at mission editor position; this speed controls how fast they
    /// navigate back to where they were. Default: 100.0 m/s (~194 knots, ~6 min for 38km)
    #[serde(default = "default_carrier_spawn_repositioning_speed")]
    pub spawn_repositioning_speed: f64,
    /// Time in seconds to complete a carrier repair with a single repair
    /// crate (default: 1800 = 30 minutes). Each additional repair crate
    /// delivered while the repair is running divides this (floored at 60s).
    #[serde(default = "default_carrier_repair_time")]
    pub repair_time: u32,
    /// Carrier group definitions - maps template names to display names
    /// If not specified, carriers are detected by BCARRIER/RCARRIER/NCARRIER prefix
    #[serde(default)]
    pub groups: Vec<CarrierGroupCfg>,
}

pub(super) fn default_carrier_spawn_repositioning_speed() -> f64 {
    100.0
}

pub(super) fn default_carrier_repair_time() -> u32 {
    1800
}
