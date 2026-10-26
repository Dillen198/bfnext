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

//! The modern-war systems: EW, rail, SAM stock, raids, tempo, wingmen, AI packages, civil traffic.

use super::*;

/// See `Cfg::modern_war`. Each part is independent; leave one out to turn it
/// off.
#[derive(Debug, Clone, Default, Serialize, Deserialize, schemars::JsonSchema)]
pub struct ModernWarCfg {
    #[serde(default)]
    pub ew: Option<EwCfg>,
    #[serde(default)]
    pub sam_stock: Option<SamStockCfg>,
    #[serde(default)]
    pub raids: Option<RaidsCfg>,
    #[serde(default)]
    pub boat_raids: Option<BoatRaidsCfg>,
    #[serde(default)]
    pub tempo: Option<TempoCfg>,
    #[serde(default)]
    pub rail: Option<RailCfg>,
}

/// Rail logistics: supply trains between the side's objectives that sit on a
/// railway. A train loads a big share of its origin's surplus (debited on
/// departure, like any transport), runs the actual rails, and delivers on
/// arrival. Destroy the locomotive and the cargo is lost -- rail lines and
/// the trains on them become interdiction targets, as in both current wars.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct RailCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Seconds between one side's train departures. Default 1800.
    #[serde(default = "default_rail_interval")]
    pub interval_secs: u32,
    /// Trains one side may have running at once. Default 2.
    #[serde(default = "default_rail_max")]
    pub max_trains_per_side: u32,
    /// Locomotive first, then the wagons, as DCS type names.
    #[serde(default = "default_rail_train_red")]
    pub train_red: Vec<String>,
    #[serde(default = "default_rail_train_blue")]
    pub train_blue: Vec<String>,
    #[serde(default = "default_rail_speed")]
    pub speed_kph: f64,
    /// An objective is a station if a railway passes within this. Default 5 km.
    #[serde(default = "default_rail_station_radius")]
    pub station_radius_m: f64,
    /// Units of each item a train carries, capped by what the origin holds
    /// and the destination has room for. Default 150.
    #[serde(default = "default_rail_cap")]
    pub per_item_cap: u32,
    /// Only stations below this supply % are sent a train. Default 70.
    #[serde(default = "default_rail_below")]
    pub destination_supply_below: u8,
    #[serde(default = "default_rail_min_route")]
    pub min_route_m: f64,
    #[serde(default = "default_rail_max_route")]
    pub max_route_m: f64,
    /// Routes passing this close to an enemy objective are not used.
    /// Default 15 km.
    #[serde(default = "default_rail_clearance")]
    pub enemy_clearance_m: f64,
}

pub(super) fn default_rail_interval() -> u32 { 1800 }
pub(super) fn default_rail_max() -> u32 { 2 }
pub(super) fn default_rail_train_red() -> Vec<String> {
    ["Locomotive", "Coach a tank yellow", "Coach cargo", "Coach cargo open", "Coach cargo"]
        .into_iter()
        .map(String::from)
        .collect()
}
pub(super) fn default_rail_train_blue() -> Vec<String> {
    ["ES44AH", "Tankcartrinity", "Boxcartrinity", "Wellcarnsc", "Boxcartrinity"]
        .into_iter()
        .map(String::from)
        .collect()
}
pub(super) fn default_rail_speed() -> f64 { 60.0 }
pub(super) fn default_rail_station_radius() -> f64 { 5_000.0 }
pub(super) fn default_rail_cap() -> u32 { 150 }
pub(super) fn default_rail_below() -> u8 { 70 }
pub(super) fn default_rail_min_route() -> f64 { 30_000.0 }
pub(super) fn default_rail_max_route() -> f64 { 400_000.0 }
pub(super) fn default_rail_clearance() -> f64 { 15_000.0 }

/// Ground electronic warfare. Objectives of `host_kinds` get a radio/GNSS
/// jammer truck (`GPS_Spoofer_Blue`/`_Red`) that switches on while enemy
/// aircraft are inside `activation_radius_m` and off again once they have
/// gone -- emitting gives it away. Killing the truck silences the site until
/// the objective replaces it. Radio jamming also breaks the enemy's GCI voice
/// inside `comms_jam_radius_m`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct EwCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Objective kinds that host a jammer, by display name: "Airbase", "FOB",
    /// "FARP", "Logistics Hub", "Naval Base", "Carrier Group", "Factory",
    /// "Special SAM Site", "Command Center".
    #[serde(default = "default_ew_host_kinds")]
    pub host_kinds: Vec<String>,
    /// Of those, the kinds that SPOOF satellite navigation (push receivers
    /// toward a false position) rather than just jam it.
    #[serde(default = "default_ew_spoof_kinds")]
    pub spoof_kinds: Vec<String>,
    /// Enemy aircraft this close switch a jammer on. Default 80 km.
    #[serde(default = "default_ew_activation")]
    pub activation_radius_m: f64,
    /// A jammer stays on at least this long once it switches on. Default 180.
    #[serde(default = "default_ew_min_on")]
    pub min_on_secs: u32,
    #[serde(default = "default_ew_gnss")]
    pub gps: GnssMode,
    #[serde(default = "default_ew_gnss")]
    pub glonass: GnssMode,
    #[serde(default = "default_ew_radio")]
    pub radio: RadioJamMode,
    /// Radio band jammed, MHz. Default 225-400 (military UHF).
    #[serde(default = "default_ew_band")]
    pub band_mhz: (f64, f64),
    /// How far from a spoofing site its false position is. Default 25 km.
    #[serde(default = "default_ew_spoof_offset")]
    pub spoof_offset_m: f64,
    /// Enemy flights this close to an active radio jammer get broken GCI.
    /// Default 60 km.
    #[serde(default = "default_ew_comms_radius")]
    pub comms_jam_radius_m: f64,
    /// Seconds before a destroyed jammer is replaced (the objective must still
    /// be held and not at zero health). Default 1800.
    #[serde(default = "default_ew_respawn")]
    pub respawn_secs: u32,
    /// Put an intel mark for the enemy near an active jammer -- ELINT finds
    /// emitters. Offset by up to `intel_uncertainty_m`. Default true.
    #[serde(default = "default_true")]
    pub intel_marks: bool,
    #[serde(default = "default_ew_uncertainty")]
    pub intel_uncertainty_m: f64,
    /// Objective kinds whose jammer GUARDS a SAM site: it jams GPS/GLONASS
    /// only (so GPS-guided weapons -- JDAM, JSOW, GMLRS -- go astray; it does
    /// nothing against radar-homing HARMs), never radio, switches on only
    /// for aircraft inside `sam_activation_radius_m`, and stays hidden: the
    /// enemy's ELINT only places it, roughly, after it has emitted for
    /// `sam_reveal_after_secs`. A kind in both lists is a SAM guard.
    #[serde(default = "default_ew_sam_kinds")]
    pub sam_site_kinds: Vec<String>,
    #[serde(default = "default_ew_sam_activation")]
    pub sam_activation_radius_m: f64,
    #[serde(default = "default_ew_sam_reveal_after")]
    pub sam_reveal_after_secs: u32,
    #[serde(default = "default_ew_sam_uncertainty")]
    pub sam_reveal_uncertainty_m: f64,
}

/// Satellite-navigation mode of a jammer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, schemars::JsonSchema)]
pub enum GnssMode {
    Off,
    Jam,
    Spoof,
}

/// Radio mode of a jammer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, schemars::JsonSchema)]
pub enum RadioJamMode {
    Off,
    Simple,
    Adaptive,
}

pub(super) fn default_ew_host_kinds() -> Vec<String> {
    ["Command Center", "Naval Base"].into_iter().map(String::from).collect()
}
pub(super) fn default_ew_sam_kinds() -> Vec<String> {
    ["Special SAM Site"].into_iter().map(String::from).collect()
}
pub(super) fn default_ew_sam_activation() -> f64 { 40_000.0 }
pub(super) fn default_ew_sam_reveal_after() -> u32 { 600 }
pub(super) fn default_ew_sam_uncertainty() -> f64 { 15_000.0 }
pub(super) fn default_ew_spoof_kinds() -> Vec<String> {
    ["Naval Base"].into_iter().map(String::from).collect()
}
pub(super) fn default_ew_activation() -> f64 { 80_000.0 }
pub(super) fn default_ew_min_on() -> u32 { 180 }
pub(super) fn default_ew_gnss() -> GnssMode { GnssMode::Jam }
pub(super) fn default_ew_radio() -> RadioJamMode { RadioJamMode::Adaptive }
pub(super) fn default_ew_band() -> (f64, f64) { (225.0, 400.0) }
pub(super) fn default_ew_spoof_offset() -> f64 { 25_000.0 }
pub(super) fn default_ew_comms_radius() -> f64 { 60_000.0 }
pub(super) fn default_ew_respawn() -> u32 { 1800 }
pub(super) fn default_ew_uncertainty() -> f64 { 5_000.0 }

/// Finite SAM interceptors. Each SAM group holds `missiles_per_launcher` x
/// its launchers x `reload_multiplier` missiles; every SAM launch spends one.
/// A site at zero goes weapons-hold ("Winchester") until logistics rearm it:
/// every `resupply_period_secs` it gets `resupply_per_period` back, but only
/// while the objective it belongs to has at least `min_supply` supply. The
/// count survives the site being culled and respawned (which refills DCS's
/// own launcher ammo).
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct SamStockCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    #[serde(default = "default_sam_per_launcher")]
    pub missiles_per_launcher: u32,
    #[serde(default = "default_sam_reload_mult")]
    pub reload_multiplier: f32,
    #[serde(default = "default_sam_resupply_period")]
    pub resupply_period_secs: u32,
    #[serde(default = "default_sam_resupply")]
    pub resupply_per_period: u32,
    #[serde(default = "default_sam_min_supply")]
    pub min_supply: u8,
    /// Tell the owning side when a site runs dry and when it is rearmed.
    #[serde(default = "default_true")]
    pub announce: bool,
}

pub(super) fn default_sam_per_launcher() -> u32 { 4 }
pub(super) fn default_sam_reload_mult() -> f32 { 2.0 }
pub(super) fn default_sam_resupply_period() -> u32 { 900 }
pub(super) fn default_sam_resupply() -> u32 { 4 }
pub(super) fn default_sam_min_supply() -> u8 { 40 }

/// Long-range strike raids on infrastructure: one-way attack drones (a
/// template flown low to the target, which explodes on arrival) and ballistic
/// missiles from the side's deployed launchers. Damage is real on the map and
/// in the economy: every drone that arrives, and every missile not
/// intercepted, cuts the target's stores by `supply_damage_pct` and pauses a
/// factory for `factory_pause_secs`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct RaidsCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Raid with nobody on the server. Default false.
    #[serde(default)]
    pub run_when_empty: bool,
    /// Mission-clock hours a raid may launch in, [start, end), wrapping past
    /// midnight. Default 20-05: raids come at night. Null = any time.
    #[serde(default = "default_raid_hours")]
    pub hours: Option<(u32, u32)>,
    /// Seconds between one side's raids (jittered +-30%, shortened during an
    /// offensive). Default 2700.
    #[serde(default = "default_raid_interval")]
    pub interval_secs: u32,
    /// Plane-section .miz groups flown as one-way attack drones. Empty = no
    /// drone raids for that side. Any slow UAV or light aircraft will do.
    #[serde(default)]
    pub drone_templates_red: Vec<String>,
    #[serde(default)]
    pub drone_templates_blue: Vec<String>,
    #[serde(default = "default_raid_drones")]
    pub drones_per_raid: u32,
    #[serde(default = "default_raid_drone_speed")]
    pub drone_speed_ms: f64,
    #[serde(default = "default_raid_drone_alt")]
    pub drone_alt_agl_m: f64,
    /// Longest drone leg. Default 400 km.
    #[serde(default = "default_raid_drone_range")]
    pub drone_range_m: f64,
    /// Explosive mass detonated on arrival, kg. Default 60.
    #[serde(default = "default_raid_warhead")]
    pub drone_warhead_kg: f64,
    /// Fire the side's in-range ballistic-missile launchers (`Launcher`
    /// tagged, `artillery.units` ranges) at the target as part of a raid.
    #[serde(default = "default_true")]
    pub use_missiles: bool,
    #[serde(default = "default_raid_launchers")]
    pub max_launchers: u32,
    /// Launcher unit types that fire raid missiles -- surface-to-surface
    /// ballistic ones. Anti-ship launchers (Silkworm `hy_launcher`) carry the
    /// same `Launcher` tag and must not be sent at a factory.
    #[serde(default = "default_raid_missile_types")]
    pub missile_types: Vec<String>,
    /// Chance each missile is intercepted, per enemy missile-defence SAM site
    /// (`EngagesWeapons` tag) within 40 km of the target, capped at 85%.
    #[serde(default = "default_raid_intercept")]
    pub intercept_per_site: f64,
    /// Target kinds, by objective display name.
    #[serde(default = "default_raid_targets")]
    pub target_kinds: Vec<String>,
    #[serde(default = "default_raid_supply_damage")]
    pub supply_damage_pct: u8,
    #[serde(default = "default_raid_factory_pause")]
    pub factory_pause_secs: u32,
    /// Warn the target side when a raid launches. Default true.
    #[serde(default = "default_true")]
    pub warn_defenders: bool,
}

pub(super) fn default_raid_hours() -> Option<(u32, u32)> { Some((20, 5)) }
pub(super) fn default_raid_interval() -> u32 { 2700 }
pub(super) fn default_raid_drones() -> u32 { 4 }
pub(super) fn default_raid_drone_speed() -> f64 { 50.0 }
pub(super) fn default_raid_drone_alt() -> f64 { 400.0 }
pub(super) fn default_raid_drone_range() -> f64 { 400_000.0 }
pub(super) fn default_raid_warhead() -> f64 { 60.0 }
pub(super) fn default_raid_launchers() -> u32 { 2 }
pub(super) fn default_raid_missile_types() -> Vec<String> {
    [
        "Scud_B",
        "CHAP_9K720_HE",
        "CHAP_9K720_Cluster",
        "CHAP_M142_ATACMS_M39A1",
        "CHAP_M142_ATACMS_M48",
    ]
    .into_iter()
    .map(String::from)
    .collect()
}
pub(super) fn default_raid_intercept() -> f64 { 0.35 }
pub(super) fn default_raid_targets() -> Vec<String> {
    ["Factory", "Logistics Hub", "Airbase", "Naval Base"].into_iter().map(String::from).collect()
}
pub(super) fn default_raid_supply_damage() -> u8 { 5 }
pub(super) fn default_raid_factory_pause() -> u32 { 1800 }

/// Sea-drone raids: fast boats run at enemy ships from friendly water and
/// detonate alongside. Needs a friendly Naval Base to launch from.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct BoatRaidsCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    #[serde(default)]
    pub run_when_empty: bool,
    #[serde(default = "default_boat_interval")]
    pub interval_secs: u32,
    #[serde(default = "default_boat_count")]
    pub boats_per_raid: u32,
    /// DCS ship type used as the sea drone. Default "speedboat".
    #[serde(default = "default_boat_type")]
    pub boat_type: String,
    #[serde(default = "default_boat_speed")]
    pub speed_ms: f64,
    #[serde(default = "default_boat_warhead")]
    pub warhead_kg: f64,
    /// Detonation distance from the target ship, metres. Default 150.
    #[serde(default = "default_boat_trigger")]
    pub trigger_radius_m: f64,
    #[serde(default = "default_boat_range")]
    pub max_range_m: f64,
}

pub(super) fn default_boat_interval() -> u32 { 3600 }
pub(super) fn default_boat_count() -> u32 { 3 }
pub(super) fn default_boat_type() -> String { "speedboat".into() }
pub(super) fn default_boat_speed() -> f64 { 18.0 }
pub(super) fn default_boat_warhead() -> f64 { 250.0 }
pub(super) fn default_boat_trigger() -> f64 { 150.0 }
pub(super) fn default_boat_range() -> f64 { 150_000.0 }

/// Campaign tempo: each side alternates an offensive (raids and AI packages
/// come faster, aimed along one axis) with a regroup, and is told when its
/// posture changes. The two sides run out of phase.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct TempoCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    #[serde(default = "default_tempo_offensive")]
    pub offensive_hours: f64,
    #[serde(default = "default_tempo_regroup")]
    pub regroup_hours: f64,
    /// Interval multiplier during an offensive (0.5 = twice as often).
    #[serde(default = "default_tempo_offensive_factor")]
    pub offensive_factor: f64,
    /// Interval multiplier while regrouping.
    #[serde(default = "default_tempo_regroup_factor")]
    pub regroup_factor: f64,
    #[serde(default = "default_true")]
    pub announce: bool,
}

pub(super) fn default_tempo_offensive() -> f64 { 6.0 }
pub(super) fn default_tempo_regroup() -> f64 { 10.0 }
pub(super) fn default_tempo_offensive_factor() -> f64 { 0.5 }
pub(super) fn default_tempo_regroup_factor() -> f64 { 1.5 }


/// See `Cfg::air_life`. Each part is independent; leave one out to disable it.
#[derive(Debug, Clone, Default, Serialize, Deserialize, schemars::JsonSchema)]
pub struct AirLifeCfg {
    #[serde(default)]
    pub wingman: Option<WingmanCfg>,
    #[serde(default)]
    pub packages: Option<AiPackagesCfg>,
    #[serde(default)]
    pub civil_traffic: Option<CivilTrafficCfg>,
}

/// F10 > Wingman: a player asks for an AI flight, it spawns in the air behind
/// them and flies DCS's Escort task on their aircraft until they release it,
/// land, leave the slot, or it runs out of `lifetime_secs`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct WingmanCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Only offered while the requester's side has at most this many human
    /// pilots in slots -- it is for thin servers, not a force multiplier on a
    /// full one. 0 = always offered. Default 4.
    #[serde(default = "default_wingman_max_side_pilots")]
    pub max_side_pilots: u32,
    /// Points charged per request. Default 0.
    #[serde(default)]
    pub cost: i32,
    /// Seconds before the wingman is sent home regardless. Default 3600.
    #[serde(default = "default_wingman_lifetime")]
    pub lifetime_secs: u32,
    /// Seconds a player must wait for a new wingman after losing one. Default 300.
    #[serde(default = "default_wingman_cooldown")]
    pub cooldown_secs: u32,
    /// Plane-section groups in the .miz to draw a fixed-wing player's wingman
    /// from. Empty = fall back to `campaign_events.cap_templates_red/_blue`.
    #[serde(default)]
    pub templates_red: Vec<String>,
    #[serde(default)]
    pub templates_blue: Vec<String>,
    /// Helicopter-section groups for a helicopter player's wingman. Empty =
    /// fall back to `campaign_events.helo_templates_red/_blue`; with neither,
    /// helicopter pilots are told no wingman is available.
    #[serde(default)]
    pub rotary_templates_red: Vec<String>,
    #[serde(default)]
    pub rotary_templates_blue: Vec<String>,
    /// How far from the escorted player a fixed-wing wingman will go to engage
    /// an air threat. Default 40 km.
    #[serde(default = "default_wingman_engage_m")]
    pub engage_dist_m: f64,
    /// Same for a helicopter wingman, which engages helicopters and ground
    /// units. Default 8 km.
    #[serde(default = "default_wingman_rotary_engage_m")]
    pub rotary_engage_dist_m: f64,
}

pub(super) fn default_wingman_max_side_pilots() -> u32 { 4 }
pub(super) fn default_wingman_lifetime() -> u32 { 3600 }
pub(super) fn default_wingman_cooldown() -> u32 { 300 }
pub(super) fn default_wingman_engage_m() -> f64 { 40_000.0 }
pub(super) fn default_wingman_rotary_engage_m() -> f64 { 8_000.0 }

/// Autonomous AI packages: while a side has few human pilots, the engine
/// launches that side's own Fighters / Attackers / SEAD actions (the same
/// templates players buy through the Actions menu) against the front, so a
/// solo player has both friends and enemies in the air.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct AiPackagesCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// A side with MORE human pilots in slots than this gets no packages.
    /// Default 3.
    #[serde(default = "default_packages_max_side_pilots")]
    pub max_side_pilots: u32,
    /// Launch packages even with nobody on the server. Off by default: the
    /// point is company for the players who are on, and an empty server
    /// should not grind itself down overnight.
    #[serde(default)]
    pub run_when_empty: bool,
    /// Seconds between launch decisions. Default 120.
    #[serde(default = "default_packages_check_interval")]
    pub check_interval_secs: u32,
    /// Minimum seconds between two launches for one side. Default 900.
    #[serde(default = "default_packages_launch_interval")]
    pub launch_interval_secs: u32,
    /// Packages one side may have up at once. Default 2.
    #[serde(default = "default_packages_max_active")]
    pub max_active_per_side: u32,
    /// Seconds a package works its target before it is sent home. Default 1800.
    #[serde(default = "default_packages_lifetime")]
    pub lifetime_secs: u32,
    /// Side treasury charged per launch. Default 0 (free, rate-limited only).
    #[serde(default)]
    pub treasury_cost: i64,
    /// Names from `actions.red` to launch. Empty = every Fighters, Attackers
    /// and SEAD action the side has.
    #[serde(default)]
    pub actions_red: Vec<String>,
    #[serde(default)]
    pub actions_blue: Vec<String>,
    /// Targets farther than this from every friendly airbase are skipped.
    /// Default 150 km.
    #[serde(default = "default_packages_max_range")]
    pub max_target_range_m: f64,
    /// Tell the side's players when a package launches. Default true.
    #[serde(default = "default_true")]
    pub announce: bool,
}

pub(super) fn default_packages_max_side_pilots() -> u32 { 3 }
pub(super) fn default_packages_check_interval() -> u32 { 120 }
pub(super) fn default_packages_launch_interval() -> u32 { 900 }
pub(super) fn default_packages_max_active() -> u32 { 2 }
pub(super) fn default_packages_lifetime() -> u32 { 1800 }
pub(super) fn default_packages_max_range() -> f64 { 150_000.0 }

/// Neutral civilian airliners: high overflights that keep clear of the front
/// where they can, plus departures from and arrivals at airports well behind
/// it. They belong to a neutral country, so no AI on either side engages
/// them -- only a player can, and `shootdown_penalty_points` says what that
/// costs.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CivilTrafficCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Only fly while at least this many players are in aircraft. 0 =
    /// always, including an empty server. Default 1.
    #[serde(default = "default_civil_min_players")]
    pub min_players: u32,
    /// Airliners in the air at once. Default 4.
    #[serde(default = "default_civil_max_active")]
    pub max_active: u32,
    /// Seconds between spawns (jittered +-30%). Default 300.
    #[serde(default = "default_civil_spawn_interval")]
    pub spawn_interval_secs: u32,
    /// Country the airliners fly for. Must be in the mission's NEUTRAL
    /// coalition. Unset = the first neutral country the mission lists.
    #[serde(default)]
    pub country: Option<Country>,
    #[serde(default = "default_civil_aircraft")]
    pub aircraft: Vec<CivilAircraftCfg>,
    /// Airline prefixes for the flight numbers players see ("THY1843").
    #[serde(default = "default_civil_airline_codes")]
    pub airline_codes: Vec<String>,
    /// Relative pick weights of the three flight kinds.
    #[serde(default = "default_civil_overflight_weight")]
    pub overflight_weight: u32,
    #[serde(default = "default_civil_airport_weight")]
    pub departure_weight: u32,
    #[serde(default = "default_civil_airport_weight")]
    pub arrival_weight: u32,
    /// An airport is "rear" -- used for departures and arrivals -- only if it
    /// is at least this far from the front. Default 60 km.
    #[serde(default = "default_civil_rear_airport_min_front")]
    pub rear_airport_min_front_m: f64,
    /// Overflight routes try to stay at least this far from the front, and
    /// take the widest berth available when none clears it. Default 40 km.
    #[serde(default = "default_civil_front_standoff")]
    pub front_standoff_m: f64,
    /// How far beyond the outermost airbase or objective the map-edge entry
    /// and exit points sit. Default 20 km.
    #[serde(default = "default_civil_map_margin")]
    pub map_margin_m: f64,
    /// Anything still flying after this long is removed. Default 5400.
    #[serde(default = "default_civil_max_flight")]
    pub max_flight_secs: u32,
    /// Points taken from a player who shoots one down. Default 500.
    #[serde(default = "default_civil_shootdown_points")]
    pub shootdown_penalty_points: i32,
    /// Treasury taken from the shooter's side. Default 1000.
    #[serde(default = "default_civil_shootdown_treasury")]
    pub shootdown_treasury_penalty: i64,
    /// Neutral merchant shipping on sea lanes across the map's water (a
    /// lane is only used if every sample along it is open water). Sinking
    /// one costs the same as an airliner. Empty = no ships.
    #[serde(default = "default_civil_ships")]
    pub ships: Vec<CivilShipCfg>,
    /// Merchant ships at sea at once. Default 3.
    #[serde(default = "default_civil_max_ships")]
    pub max_ships: u32,
    /// Seconds between ship spawns (jittered). Default 900.
    #[serde(default = "default_civil_ship_interval")]
    pub ship_spawn_interval_secs: u32,
}

/// One merchant ship type. `typ` is the DCS unit type name.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CivilShipCfg {
    pub typ: String,
    #[serde(default = "default_cap_template_weight")]
    pub weight: u32,
    /// Speed, m/s. Default 7 (about 14 knots).
    #[serde(default = "default_civil_ship_speed")]
    pub speed_ms: f64,
}

pub(super) fn default_civil_ship_speed() -> f64 { 7.0 }
pub(super) fn default_civil_max_ships() -> u32 { 3 }
pub(super) fn default_civil_ship_interval() -> u32 { 900 }

pub(super) fn default_civil_ships() -> Vec<CivilShipCfg> {
    ["Dry-cargo ship-1", "Dry-cargo ship-2", "HandyWind", "ELNYA", "Seawise_Giant"]
        .into_iter()
        .map(|t| CivilShipCfg { typ: t.into(), weight: 1, speed_ms: default_civil_ship_speed() })
        .collect()
}

/// One airliner type. `typ` is the DCS unit type name.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CivilAircraftCfg {
    pub typ: String,
    #[serde(default = "default_cap_template_weight")]
    pub weight: u32,
    /// Livery names to pick from. Empty = the country's default livery.
    #[serde(default)]
    pub liveries: Vec<String>,
    pub cruise_alt_min_m: f64,
    pub cruise_alt_max_m: f64,
    /// Cruise true airspeed, m/s.
    pub cruise_speed_ms: f64,
}

pub(super) fn default_civil_min_players() -> u32 { 1 }
pub(super) fn default_civil_max_active() -> u32 { 4 }
pub(super) fn default_civil_spawn_interval() -> u32 { 300 }
pub(super) fn default_civil_overflight_weight() -> u32 { 3 }
pub(super) fn default_civil_airport_weight() -> u32 { 1 }
pub(super) fn default_civil_rear_airport_min_front() -> f64 { 60_000.0 }
pub(super) fn default_civil_front_standoff() -> f64 { 40_000.0 }
pub(super) fn default_civil_map_margin() -> f64 { 20_000.0 }
pub(super) fn default_civil_max_flight() -> u32 { 5400 }
pub(super) fn default_civil_shootdown_points() -> i32 { 500 }
pub(super) fn default_civil_shootdown_treasury() -> i64 { 1000 }

pub(super) fn default_civil_aircraft() -> Vec<CivilAircraftCfg> {
    let t = |typ: &str, alt_min: f64, alt_max: f64, speed: f64| CivilAircraftCfg {
        typ: typ.into(),
        weight: 1,
        liveries: vec![],
        cruise_alt_min_m: alt_min,
        cruise_alt_max_m: alt_max,
        cruise_speed_ms: speed,
    };
    vec![
        t("Yak-40", 6_000., 7_500., 150.),
        t("An-26B", 5_000., 6_500., 120.),
        t("IL-76MD", 9_000., 11_000., 215.),
    ]
}

pub(super) fn default_civil_airline_codes() -> Vec<String> {
    ["AFL", "THY", "AZE", "GEO", "UAE", "QTR", "DLH", "SVA", "ETD", "PGT"]
        .into_iter()
        .map(String::from)
        .collect()
}

impl Default for CivilTrafficCfg {
    fn default() -> Self {
        Self {
            enabled: true,
            min_players: default_civil_min_players(),
            max_active: default_civil_max_active(),
            spawn_interval_secs: default_civil_spawn_interval(),
            country: None,
            aircraft: default_civil_aircraft(),
            airline_codes: default_civil_airline_codes(),
            overflight_weight: default_civil_overflight_weight(),
            departure_weight: default_civil_airport_weight(),
            arrival_weight: default_civil_airport_weight(),
            rear_airport_min_front_m: default_civil_rear_airport_min_front(),
            front_standoff_m: default_civil_front_standoff(),
            map_margin_m: default_civil_map_margin(),
            max_flight_secs: default_civil_max_flight(),
            shootdown_penalty_points: default_civil_shootdown_points(),
            shootdown_treasury_penalty: default_civil_shootdown_treasury(),
            ships: default_civil_ships(),
            max_ships: default_civil_max_ships(),
            ship_spawn_interval_secs: default_civil_ship_interval(),
        }
    }
}
