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

//! Slung and internal cargo, C-130 cargo, CSAR.

use super::*;

/// Configuration for vehicles that can be loaded into C-130 cargo
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct C130Vehicle {
    /// The display name of the vehicle in the menu
    pub name: String,
    /// The name of the template used to spawn the vehicle group
    pub template: String,
    /// How much weight does the vehicle add to the carrier unit (kg)
    pub weight: u32,
    /// How many simultaneous instances of the vehicle are allowed
    #[serde(default = "default_c130_vehicle_limit")]
    pub limit: u32,
    /// How to deal with it when the max number of instances are deployed
    #[serde(default)]
    pub limit_enforce: LimitEnforceTyp,
    /// How many points does this vehicle cost
    #[serde(default)]
    pub cost: u32,
    /// Menu path for organizing vehicles (e.g., ["Light Vehicles"] or ["APCs"])
    #[serde(default)]
    pub path: Vec<String>,
}

pub(super) fn default_c130_vehicle_limit() -> u32 {
    10
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct CargoConfig {
    /// How many troop slots does this vehicle have
    pub troop_slots: u8,
    /// How many crate slots does this vehicle have
    pub crate_slots: u8,
    /// How many total troops and crates can this vehicle carry.
    /// e.g. if troop_slots is 1, crate_slots is 1, and total_slots is 1
    /// then the vehicle can carry either a troop or a crate but not both.
    pub total_slots: u16,
    /// How many downed pilots this vehicle can carry for CSAR missions.
    /// Pilots count against total_slots.
    #[serde(default)]
    pub pilot_slots: u8,
    /// The default distance (meters) to spawn crates from this vehicle.
    #[serde(default)]
    pub spawn_distance: Option<f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CsarConfig {
    /// If false, no downed pilots will be spawned and CSAR is disabled
    #[serde(default = "default_csar_enabled")]
    pub enabled: bool,
    /// The DCS unit template name used to spawn the downed pilot ground marker, per side
    pub pilot_template: FxHashMap<Side, String>,
    /// Points awarded to the rescuing player on delivery
    #[serde(default)]
    pub rescue_reward: u32,
    /// Radius in meters within which a landed helicopter triggers the downed pilot
    /// to walk toward it and fire a flare (default 100m)
    #[serde(default = "default_csar_pickup_radius")]
    pub pickup_radius: u32,
    /// Radius in meters within which the downed pilot auto-boards the helicopter
    /// (default 20m)
    #[serde(default = "default_csar_board_radius")]
    pub board_radius: u32,
    /// Minutes after which an unrescued downed pilot is auto-captured (0 = never, default 15)
    #[serde(default = "default_csar_capture_timer")]
    pub capture_timer: u32,
    /// Radius in meters within which an enemy unit captures/eliminates the downed pilot (0 = disabled, default 50)
    #[serde(default = "default_csar_enemy_capture_radius")]
    pub enemy_capture_radius: u32,
    /// DCS unit template name for enemy search-party infantry per side (empty map = disabled)
    #[serde(default)]
    pub search_party_template: FxHashMap<Side, String>,
    /// Number of enemy search-party groups spawned when a pilot goes down (0 = disabled, default 0)
    #[serde(default)]
    pub search_party_size: u8,
    /// How often (minutes) to re-broadcast downed pilot location to all friendly helo pilots (0 = never, default 5)
    #[serde(default = "default_csar_renotify_interval")]
    pub renotify_interval: u32,
    /// Cooldown in seconds before the same pilot can pop smoke again via the menu (default 300)
    #[serde(default = "default_csar_smoke_cooldown")]
    pub smoke_cooldown: u32,
}

pub(super) fn default_csar_enabled() -> bool {
    true
}

pub(super) fn default_csar_pickup_radius() -> u32 {
    100
}

pub(super) fn default_csar_board_radius() -> u32 {
    20
}

pub(super) fn default_csar_capture_timer() -> u32 {
    15
}

pub(super) fn default_csar_enemy_capture_radius() -> u32 {
    50
}

pub(super) fn default_csar_renotify_interval() -> u32 {
    5
}

pub(super) fn default_csar_smoke_cooldown() -> u32 {
    300
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct C130CargoConfig {
    /// List of vehicle types that can use C-130 physical cargo system (e.g., "C-130")
    pub enabled_vehicles: FxHashSet<Vehicle>,
    /// Spawn delay between crates when using "Spawn All" (seconds)
    #[serde(default = "default_c130_spawn_delay")]
    pub spawn_delay: u32,
    /// Maximum number of crates that can be spawned at once with "Spawn All"
    #[serde(default = "default_c130_max_spawn")]
    pub max_spawn_all: u32,
    /// The default distance (meters) to spawn crates from C-130s.
    #[serde(default)]
    pub spawn_distance: Option<f64>,
    /// Vehicles that can be loaded into C-130 cargo for each side
    #[serde(default)]
    pub loadable_vehicles: FxHashMap<Side, Vec<C130Vehicle>>,
}

pub(super) fn default_c130_spawn_delay() -> u32 {
    1
}

pub(super) fn default_c130_max_spawn() -> u32 {
    50
}

/// Configuration for helicopters using dynamic (physical) cargo system.
/// Helicopters with dynamic cargo spawn physical crate objects via the DCS cargo menu.
/// Crate slot limits from CargoConfig are ignored for vehicles in this list.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct HeloCargoConfig {
    /// Vehicle types that use dynamic physical cargo instead of the old slot-based system
    pub enabled_vehicles: FxHashSet<Vehicle>,
    /// Spawn delay between crates when using "Spawn All" (seconds)
    #[serde(default = "default_c130_spawn_delay")]
    pub spawn_delay: u32,
    /// Maximum number of crates that can be spawned at once with "Spawn All"
    #[serde(default = "default_c130_max_spawn")]
    pub max_spawn_all: u32,
    /// The default distance (meters) to spawn crates from helicopters.
    #[serde(default)]
    pub spawn_distance: Option<f64>,
    /// If true, crates auto-unpack when they land after being dropped (like C-130 airdrop).
    /// If false (default), the player must use "Unpack Nearby Crate(s)" manually.
    #[serde(default)]
    pub auto_unpack: bool,
}
