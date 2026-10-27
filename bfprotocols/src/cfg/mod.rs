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

use anyhow::{anyhow, bail, Context, Result};
use chrono::prelude::*;
use compact_str::format_compact;
use dcso3::{
    coalition::Side,
    controller::{AltType, TacanBand},
    country::Country,
    net::Ucid,
    String,
};
use enumflags2::{bitflags, BitFlags};
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use indexmap::IndexMap;
use log::warn;
use netidx::path::Path as NetIdxPath;
use regex::Regex;
use serde_derive::{Deserialize, Serialize};
use std::{
    borrow::Borrow,
    fmt,
    fs::{self, File},
    io,
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    sync::Arc,
};

mod balance;
mod command;
mod economy;
mod example;
mod ground_war;
pub use ground_war::{
    GarrisonCfg, GroundAiCfg, GroundCombatCfg, GroundWarCfg, PurchasedFormationCfg,
};
mod hq;
pub use hq::{
    EscortPolicy, HqAirCfg, HqAirTemplate, HqCfg, HqCostsCfg, HqGapFillCfg, HqLimitsCfg,
    HqPackagesCfg, HqRequestsCfg, HqStrategistCfg, HqSupportCfg,
};

pub use balance::{fmt_mult, EmergencyRepairCfg, PopulationScalingCfg};
pub use command::{rank_min_score, rank_tier, CommandCfg, HuntersCfg, RANK_MIN_SCORE};
pub use economy::{split_by_weight, EconomyCfg};

// The rest of the config, split out of this file by subsystem. Every item
// is re-exported here, so its path (`bfprotocols::cfg::X`) is unchanged.
mod actions;
pub use actions::*;
mod air_defense;
pub use air_defense::*;
mod briefing;
pub use briefing::*;
mod campaign;
pub use campaign::*;
mod cargo;
pub use cargo::*;
mod deployables;
pub use deployables::*;
mod events;
pub use events::*;
mod frontline;
pub use frontline::*;
mod helo;
pub use helo::*;
mod live_weather;
pub use live_weather::*;
mod logistics;
pub use logistics::*;
mod modern_war;
pub use modern_war::*;
mod navaids;
pub use navaids::*;
mod points;
pub use points::*;
mod rules;
pub use rules::*;
mod smart_commander;
pub use smart_commander::*;
mod types;
pub use types::*;

fn default_tk_window() -> u32 {
    24
}

fn default_true() -> bool {
    true
}

fn default_convoy_interdiction() -> u32 {
    10
}

fn default_msgs_per_second() -> usize {
    // Sustained, and spread over several drains per second rather than sent in
    // one burst. The old value of 5 was a per-second BURST, so this is a higher
    // throughput at a lower peak.
    20
}

fn default_cull_after() -> u32 {
    1800
}

fn default_capture_consolidation_secs() -> u32 {
    300
}

fn default_consolidation_zone_grace_secs() -> u32 {
    15
}

fn default_consolidation_squad_bonus() -> f32 {
    0.5
}

fn default_consolidation_crate_progress_secs() -> u32 {
    120
}

fn default_slot_leave_kill_radius_m() -> f64 {
    15000.0
}

fn default_lr_cull_distance() -> u32 {
    80_000
}

fn default_ewr_cull_distance() -> u32 {
    300_000
}

fn default_weapon_spawn_radius() -> u32 {
    60_000
}

fn default_weapon_spawn_expiry_secs() -> u32 {
    120
}

fn default_lock_sides() -> bool {
    true
}

fn default_limited_lives() -> bool {
    true
}

fn default_ewr_delay() -> u32 {
    60
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct Cfg {
    /// The config format version, `CFG_VERSION` when this build writes it.
    /// Absent in files from before versions existed, which read as version 1.
    /// Lets a future change that has to reinterpret an existing key tell old
    /// files from new ones; `load` warns about a file from a newer build.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub version: Option<u32>,
    #[serde(default)]
    #[schemars(with = "Option<std::string::String>")]
    pub netidx_base: Option<NetIdxPath>,
    /// Mutual TLS for the engine's netidx publisher (deploy/netidx-tls.md):
    /// this engine's certificate and the resolver's name, applied over the
    /// netidx client config. Unset, the client config is used as written
    /// (normally Anonymous).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub netidx_tls: Option<fowlbg::netidx::Tls>,
    /// if specified, automatically reset the server state and record
    /// a victory in the stats when the condition is met.
    #[serde(default)]
    pub auto_reset: Option<AutoResetOnVictory>,
    /// Longest a round may run, in days. Past it the round ends in a draw
    /// (through the same countdown as a victory) and the campaign resets --
    /// a theatre rotation's way of moving on from a front that froze.
    /// Absent or 0 = no limit.
    #[serde(default)]
    pub max_round_days: Option<u32>,
    /// ucids in this list are able to run admin commands
    #[serde(default)]
    pub admins: FxHashMap<Ucid, String>,
    /// ucids in this list are banned
    #[serde(default)]
    pub banned: FxHashMap<Ucid, (Option<DateTime<Utc>>, String)>,
    /// who can do what
    #[serde(default)]
    pub rules: Rules,
    /// Because DCS. Reject names that don't match this regex
    #[serde(default)]
    pub name_filter: Option<NameFilter>,
    /// Refuse a second connection from an IP address that already has a
    /// player connected (anti multi-accounting). Turn it off to let players
    /// behind one NAT -- a household, a squadron LAN party -- join together.
    /// Default true (the long-standing behaviour).
    #[serde(default = "default_true")]
    pub one_player_per_ip: bool,
    /// The maximum number of messages, including markup, we will push to dcs
    /// per second.
    ///
    /// This is a SUSTAINED rate, not a burst. The engine drains the queue
    /// several times a second (see `MSGQ_DRAIN_HZ`) and sends at most
    /// `max_msgs_per_second / MSGQ_DRAIN_HZ` in any one go, so raising this
    /// buys throughput without increasing the amount of work landing in a
    /// single DCS frame -- which is what the limit exists to protect.
    #[serde(default = "default_msgs_per_second")]
    pub max_msgs_per_second: usize,
    /// shutdown after the specified number of hours, don't shutdown
    /// if None.
    #[serde(default)]
    pub shutdown: Option<u32>,
    /// how many points are various actions worth (if any)
    #[serde(default)]
    pub points: Option<PointsCfg>,
    /// do not attempt to get the target of any weapon in this list
    #[serde(default)]
    pub weapon_target_exclusions: FxHashSet<String>,
    /// how often a base will repair if it has full logistics (Seconds)
    pub repair_time: u32,
    /// how much supply (0-100) is consumed each time an objective repairs a
    /// unit/group. If the objective's supply is below this amount repair is
    /// skipped until supply is replenished.
    #[serde(default = "default_repair_supply_cost")]
    pub repair_supply_cost: u8,
    /// Supply (0-100) drawn from a crate's origin objective for every crate
    /// consumed when a deployable is unpacked. Multiple origin objectives each
    /// pay for the crates that came from them. Applied multiplicatively to the
    /// warehouse stock (like repair), so it tapers and never fully drains a
    /// base. 0 disables the cost. Default 3.
    #[serde(default = "default_deploy_supply_cost")]
    pub deploy_supply_cost: u8,
    /// The base repair crate
    pub repair_crate: FxHashMap<Side, Crate>,
    /// Global artillery fire-support system. When set, a "Request Fires" item
    /// appears automatically in Actions for any player whose side has alive
    /// artillery groups (BLR/RLR/BMR/RMR prefix). No per-unit action entries
    /// needed. Set to null/omit to disable.
    #[serde(default)]
    pub artillery: Option<ArtilleryCfg>,
    /// If the warehouse system is to be used then this should be specified,
    /// otherwise warehouses will be ignored and you should set them to unlimited
    pub warehouse: Option<WarehouseConfig>,
    /// how far must you fly from an objective to spawn deployables
    /// without penalty (Meters)
    pub logistics_exclusion: u32,
    /// an objective will cull it's units if there are no enemy units
    /// within this distance (Meters)
    pub unit_cull_distance: u32,
    /// an objective will cull it's units if there are no enemy ground units
    /// within this distance (Meters)
    pub ground_vehicle_cull_distance: u32,
    /// cull distance override for long-range ground units (LR tag) such as
    /// MLRS, HIMARS, Smerch. Defaults to 150 000 m so LR units fully threaten
    /// the objective map. Set to 0 to fall back to ground_vehicle_cull_distance.
    #[serde(default = "default_lr_cull_distance")]
    pub lr_cull_distance: u32,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// cull distance override for EWR units. Defaults to 300 000 m.
    /// Set to 0 to fall back to ground_vehicle_cull_distance.
    #[serde(default = "default_ewr_cull_distance")]
    pub ewr_cull_distance: u32,
    /// Per-unit-type aircraft wake distance (metres) for special SAM sites,
    /// keyed by DCS unit type (e.g. "S-300PS 54K6 cp"). A site's effective
    /// wake distance is the maximum across its own units' configured
    /// distances here; units not listed fall back to lr_cull_distance (if
    /// LR-tagged in unit_classification) or unit_cull_distance otherwise.
    /// Lets e.g. an SA-10 site wake from farther out than an SA-11 site even
    /// though both are LR-tagged.
    #[serde(default)]
    pub special_sam_wake_distance: FxHashMap<Vehicle, u32>,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// spawn objectives within this radius (m) of a recent weapon launch
    /// position. Allows objectives to be awake when missiles or artillery rounds
    /// are inbound. Default: 60 000 m.
    #[serde(default = "default_weapon_spawn_radius")]
    pub weapon_spawn_radius: u32,
    /// how long (seconds) a weapon-launch event keeps nearby objectives awake.
    /// Default: 120 s.
    #[serde(default = "default_weapon_spawn_expiry_secs")]
    pub weapon_spawn_expiry_secs: u32,
    /// If a base has been inactive for this long then cull it's units (Seconds)
    #[serde(default = "default_cull_after")]
    pub cull_after: u32,
    /// After a capture the objective is "held" only by the assaulting troops
    /// until this many seconds pass. During the hold its garrison does not
    /// spawn and it stays capturable. If the assault troops are all killed
    /// before the timer, the base goes Neutral; if they survive it, the base
    /// consolidates and its garrison spawns. Default 300. 0 = instant
    /// consolidation (old behaviour).
    #[serde(default = "default_capture_consolidation_secs")]
    pub capture_consolidation_secs: u32,
    /// Seconds the holding troops may be outside the objective zone before
    /// the consolidation clock stops accruing. Covers position-update gaps and
    /// short repositioning; leave the zone for longer and progress simply
    /// pauses (it is never lost) until they come back. Default 15.
    #[serde(default = "default_consolidation_zone_grace_secs")]
    pub consolidation_zone_grace_secs: u32,
    /// Extra holding squads in the zone speed consolidation up, mirroring the
    /// way extra squads shorten the capture timer. Each squad past the first
    /// adds this fraction to the accrual rate (0.5 = a second squad
    /// consolidates at 1.5x, a third at 2x). 0 disables the bonus.
    #[serde(default = "default_consolidation_squad_bonus")]
    pub consolidation_squad_bonus: f32,
    /// Seconds of consolidation progress granted outright by landing a
    /// logistics repair kit or a supply crate at a base that is mid-hold. This
    /// is what lets a crew beat the wall clock by actually flying the
    /// logistics sortie. 0 disables it. Default 120.
    #[serde(default = "default_consolidation_crate_progress_secs")]
    pub consolidation_crate_progress_secs: u32,
    /// Emergency repair crate: logistics players deliver it to a damaged,
    /// quiet friendly base for one immediate repair step. Absent = off.
    #[serde(default)]
    pub emergency_repair: Option<EmergencyRepairCfg>,
    /// Empty-server protection: captures against a side with (almost) nobody
    /// online take longer, and optionally its bases repair faster. Absent =
    /// off.
    #[serde(default)]
    pub population_scaling: Option<PopulationScalingCfg>,
    /// Who gets paid and how much: underdog pay, wealth taper, late-joiner
    /// start, kill values by target, hauler-paid logistics. On with moderate
    /// defaults when absent; `"economy": {"enabled": false}` turns it off.
    #[serde(default)]
    pub economy: EconomyCfg,
    /// Seconds a player must sit in a freshly-taken slot before they're allowed
    /// to get airborne. They get a once-a-second "time remaining" message; take
    /// off early and they're sent straight back to spectators. 0 disables it.
    #[serde(default)]
    pub takeoff_delay_secs: u32,
    /// Anti "bail out of a losing fight" abuse: if a player leaves their slot
    /// while airborne (and not landed at a friendly base) with an enemy
    /// aircraft within this many metres, credit that enemy with the kill (and
    /// the player still loses the life for the sortie). 0 disables it.
    /// Default 15000.
    #[serde(default = "default_slot_leave_kill_radius_m")]
    pub slot_leave_kill_radius_m: f64,
    /// how often to do more expensive checks such as unit culling and
    /// updating unit positions (Seconds)
    pub slow_timed_events_freq: u32,
    /// how close various kinds of enemy units can be (with LOS) for an objective
    /// to be considered threatened. Threatened objectives can't spawn deployables
    /// within the exclusion zone. (Meters)
    pub threatened_distance: FxHashMap<Vehicle, u32>,
    /// how long before threatened is removed if no enemy can be seen
    pub threatened_cooldown: u32,
    /// how far can a crate be from the player and still be
    /// loadable (Meters)
    pub crate_load_distance: u32,
    /// how far crates apart crates can be and still unpack (Meters)
    pub crate_spread: u32,
    /// how close must artillery be to participate in an artillery mission
    /// (meters). Also the maximum gun->target distance for a fire order.
    pub artillery_mission_range: u32,
    /// minimum gun->target distance (meters) for an artillery fire order.
    /// Guns closer than this to the target are reported as "too close" and
    /// skipped rather than firing at a target inside their minimum range.
    /// Default 1500.
    #[serde(default = "default_artillery_min_range")]
    pub artillery_min_range: u32,
    /// how close must alcm be to participate in an alcm mission
    /// (meters).
    pub alcm_mission_range: u32,
    /// If true players will be locked to the side they initially
    /// choose for the duration of the round
    #[serde(default = "default_lock_sides")]
    pub lock_sides: bool,
    /// how many times a user may switch sides in a given round,
    /// or None for unlimited side switches
    #[serde(default)]
    pub side_switches: Option<u8>,
    /// How many crates a player may spawn at the same time
    #[serde(default)]
    pub max_crates: Option<u32>,
    /// the life types different vehicles use
    pub life_types: FxHashMap<Vehicle, LifeType>,
    /// the life reset configuration for each life type. A pair
    /// of number of lives per reset, and reset time in seconds.
    pub default_lives: FxHashMap<LifeType, (u8, u32)>,
    /// If true, lives will be limited according to the default_lives
    /// and life_types specification
    #[serde(default = "default_limited_lives")]
    pub limited_lives: bool,
    /// Available actions per side
    #[serde(default)]
    pub actions: FxHashMap<Side, IndexMap<String, Action, FxBuildHasher>>,
    /// vehicle cargo configuration
    #[serde(default)]
    pub cargo: FxHashMap<Vehicle, CargoConfig>,
    /// The name of the crate group for each side
    #[serde(default)]
    pub crate_template: FxHashMap<Side, String>,
    /// The name of the C-130 physical cargo crate template for each side
    #[serde(default)]
    pub c130_cargo_template: FxHashMap<Side, String>,
    /// The name of the helicopter physical cargo crate template for each side.
    /// Falls back to c130_cargo_template if not set.
    #[serde(default)]
    pub helo_cargo_template: FxHashMap<Side, String>,
    /// The unit type name of a pre-placed cargo spawn point marker on each
    /// side's carrier deck (e.g. "RCARGO_SPAWN"/"BCARGO_SPAWN"). When set and
    /// the player is on that carrier, crates spawn at this marker's exact
    /// live position instead of an offset computed from the player, since a
    /// player-relative offset can easily land off the (small, moving) deck.
    #[serde(default)]
    pub carrier_cargo_spawn_point: FxHashMap<Side, String>,
    /// C-130 physical cargo configuration
    #[serde(default)]
    pub c130_cargo: Option<C130CargoConfig>,
    /// Helicopter dynamic cargo configuration
    #[serde(default)]
    pub helo_cargo: Option<HeloCargoConfig>,
    /// If set, sync the mission file's weather (and optionally date/time)
    /// with real-world conditions before each scheduled restart. Unset
    /// (the default) disables the feature entirely.
    #[serde(default)]
    pub live_weather: Option<LiveWeatherConfig>,
    /// deployables configuration for each side
    #[serde(default)]
    pub deployables: FxHashMap<Side, Vec<Deployable>>,
    /// deployable troops configuration for each side
    pub troops: FxHashMap<Side, Vec<Troop>>,
    /// classification of ground units in the mission
    pub unit_classification: FxHashMap<Vehicle, UnitTags>,
    /// airborne jtacs
    #[serde(default)]
    pub airborne_jtacs: FxHashMap<Vehicle, DeployableJtac>,
    /// Airborne radar EWR contributors keyed by vehicle type.
    /// Any instanced player of these types will donate radar coverage to
    /// the EWR network based on range and aspect.
    #[serde(default)]
    pub airborne_ewrs: FxHashMap<Vehicle, AirborneEwr>,
    /// Ground / naval radar EWR contributors keyed by vehicle type.
    /// Spawned AI units (ships, SAM search radars) of these types will donate
    /// radar coverage to the EWR network based on range and aspect.
    #[serde(default)]
    pub ground_radar_ewrs: FxHashMap<Vehicle, AirborneEwr>,
    /// The jtac target priority list
    pub jtac_priority: Vec<UnitTags>,
    /// Objectives that can host fixed wing even though they aren't
    /// airbases. Used by actions to choose a spawn point. E.G. You
    /// want to make an airbase a logistics hub because it's close to
    /// a port.
    #[serde(default)]
    pub extra_fixed_wing_objectives: FxHashSet<String>,
    /// EWR system mode - controls track update timing
    #[serde(default)]
    pub ewr_mode: EwrMode,
    /// EWR track update delay in seconds (only used when ewr_mode is Delayed)
    #[serde(default = "default_ewr_delay")]
    pub ewr_delay: u32,
    /// Poll each airborne player's radar (DCS `Unit.getRadar`) for a lock on
    /// a friendly aircraft and warn them -- text plus the GCI "buddy spike"
    /// call. A missile fired at a friendly is caught from the Shot event
    /// either way; this only switches off the radar poll.
    #[serde(default = "default_true")]
    pub buddy_spike_radar: bool,
    /// Front line drawing configuration
    #[serde(default)]
    pub frontline: Option<FrontLineConfig>,
    /// Factory production configuration
    #[serde(default)]
    pub factory: Option<FactoryCfg>,
    /// Carrier group configuration
    #[serde(default)]
    pub carrier: Option<CarrierCfg>,
    /// Auto-generated navaids (TACAN / NDB for FARP-FOB-Logistics-NavalBase,
    /// TACAN + ICLS + ACLS + Link-4 for carrier groups). Real airbases are
    /// never given generated navaids -- DCS terrain already provides them.
    #[serde(default)]
    pub navaids: NavaidsCfg,
    /// DCS unit type name -> AGM-88 ALIC / threat code, shown on the kneeboard
    /// briefing's threat page for enemy SAM types currently in play. Free-form
    /// value string (e.g. "715" or "SA-6 715"). Empty = threat page lists
    /// types/bands without codes.
    #[serde(default)]
    pub harm_codes: FxHashMap<String, String>,
    /// Map-fixed SAM sites that can change hands via troop capture
    #[serde(default)]
    pub special_sam_sites: Vec<SpecialSamSiteCfg>,
    /// Capture zone radius in metres, shared by every special SAM site (they have
    /// no mission-editor trigger zone of their own to derive one from)
    #[serde(default = "default_sam_capture_radius")]
    pub special_sam_capture_radius_m: f64,
    /// Weather effects on gameplay
    #[serde(default)]
    pub weather_effects: Option<WeatherEffectsCfg>,
    /// Time-of-day effects on gameplay
    #[serde(default)]
    pub time_of_day_effects: Option<TimeOfDayEffectsCfg>,
    /// Dynamic campaign events configuration
    #[serde(default)]
    pub campaign_events: Option<Arc<CampaignEventsCfg>>,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// Pilot experience and progression system
    #[serde(default)]
    pub pilot_experience: Option<PilotExperienceCfg>,
    /// CSAR (Combat Search and Rescue) configuration
    #[serde(default)]
    pub csar: Option<CsarConfig>,
    /// Supply percentage (0-100) at which an objective broadcasts a "supply critical" alert
    /// to its owning side. Set to 0 to disable. Default: 20.
    #[serde(default = "default_supply_alert_threshold")]
    pub supply_alert_threshold: u8,
    /// Seconds after the "supply critical" alert fires before a convoy is automatically
    /// dispatched if no player has sent one. Set to 0 to disable auto-dispatch. Default: 300 (5 min).
    #[serde(default = "default_supply_auto_convoy_delay")]
    pub supply_auto_convoy_delay_secs: u32,
    /// Smart Commander: automated treasury, objective funding, mission rewards,
    /// and holding bonuses. Disabled if absent.
    #[serde(default)]
    pub smart_commander: Option<SmartCommanderCfg>,
    /// Per-side starting points seeded into each owned objective on a fresh map
    /// init. Overrides smart_commander.objective_start_points per side.
    /// e.g. { "Blue": 1000, "Red": 500 }
    #[serde(default)]
    pub objective_start_points: FxHashMap<Side, i32>,
    /// Per vehicle type: if present, destroying that vehicle spawns infantry dismounts at the wreck.
    #[serde(default)]
    pub dismount: FxHashMap<Vehicle, DismountSpec>,
    /// Seconds a dismount squad lives before it is removed. None (the
    /// default) keeps them until they are killed.
    #[serde(default)]
    pub dismount_ttl_secs: Option<u32>,
    /// Mercy timer: when a side reaches `trigger_count` or fewer primary objectives,
    /// starts a countdown. On expiry the losing side's victory is triggered.
    #[serde(default)]
    pub last_stand: Option<LastStandCfg>,
    /// Under-attack notifications: send a panel message to a coalition when an
    /// objective they own becomes threatened by enemy units.
    #[serde(default)]
    pub under_attack: Option<UnderAttackCfg>,
    /// Rear-base protection: no enemy aircraft near a side's protected
    /// airfields. An intruder is warned, and once it has spent `grace_secs`
    /// inside (in total) it is destroyed and sent to spectators, and staff
    /// get a `[REAR_BASE]` log line. Absent = off.
    #[serde(default)]
    pub rear_base_protection: Option<RearBaseProtectionCfg>,
    /// Deprecated / ignored. This once drew an "ARTY / COUNTER-BATTERY" text
    /// mark at a firing enemy battery, but it had no gameplay behind it and
    /// stacked overlapping copies when batteries repositioned, so the cue was
    /// removed. The key is still parsed (so old configs load) but does nothing.
    #[serde(default)]
    pub counter_battery: Option<CounterBatteryCfg>,
    /// Era restrictions: limit which airframes are available based on the active era.
    #[serde(default)]
    pub era: Option<EraCfg>,
    /// Advanced radar physics simulation. Disabled if absent (legacy binary detection).
    #[serde(default)]
    pub radar_physics: Option<RadarPhysicsCfg>,
    /// Integrated Air Defence Network — fused multi-sensor air picture and SAM cueing.
    #[serde(default)]
    pub iadn: Option<IadnConfig>,
    /// ELINT/SIGINT persistent intel database with decay and classified contacts.
    #[serde(default)]
    pub elint: Option<ElintConfig>,
    /// Ground vehicle cargo (IFV/APC troop transport). Keyed by vehicle type.
    #[serde(default)]
    pub ground_vehicle_cargo: FxHashMap<Vehicle, GroundVehicleCargo>,
    /// Player-flown reconnaissance: a timed F10 "Recon Pass" that reveals enemy
    /// units (SAMs included) around an objective on the F10 map. Requires the
    /// aircraft type to carry `UnitTag::Recon` in `unit_classification`.
    /// Disabled if absent.
    #[serde(default)]
    pub player_recon: Option<PlayerReconCfg>,
    /// Short "GCI: <callsign> on <freq> (SRS)" panel shown to a player on slot
    /// entry so they know where to tune for the live GCI controller. Omit to
    /// show nothing. The GCI system itself is configured in bfdb's `gci.json`.
    #[serde(default)]
    pub gci_briefing: Option<GciBriefingCfg>,
    /// Auto-generated situational briefing -- the slot-entry "what is going on"
    /// panel, the F10 -> Info -> Situation report and the dashboard BRIEFING
    /// page. Omit to keep the slot-entry panel off (the pull paths still work
    /// on defaults).
    #[serde(default)]
    pub situation_briefing: Option<SituationBriefingCfg>,
    /// Coalition frequency allocation shown on the briefing and the kneeboard
    /// PDF. Omit to use the built-in deconflicted plan (blue UHF 251-270 +
    /// VHF-FM 30-31, red VHF-AM 124-145 + UHF 228-237, guard on 243.0/121.5).
    #[serde(default)]
    pub comms_plan: Option<CommsPlanCfg>,
    /// When set, an objective's logistics rating is derived from the real
    /// map-terrain buildings inside its zone (the scenery scan -- warehouses,
    /// fuel depots, industrial structures) instead of from spawned
    /// LOGI/LOGIA/LOGIB/DEPOT template groups, and those template groups are
    /// not spawned at all. Repairing logistics clears the objective's
    /// destroyed-building count (DCS can't rebuild terrain, so the rubble
    /// stays but the rating recovers). Absent = legacy behaviour (spawned
    /// logi groups).
    #[serde(default)]
    pub logi_from_scenery: Option<LogiFromSceneryCfg>,
    /// AI helicopter missions callable from the F10 menu: insert fresh troops
    /// at a capturable objective, or fly a batch of surplus supply from the
    /// nearest friendly hub to any objective. Both cold-start from the
    /// nearest eligible friendly airbase (same TakeOffParkingHot mechanism as
    /// reactive CAP) and actually land at the destination before doing
    /// anything -- unlike the Paratrooper/LogisticsTransfer actions, which
    /// just fly within range and deliver mid-air. Needs its own
    /// `HeloInsertionCfg.aircraft_template` per side. Absent = feature
    /// disabled.
    #[serde(default)]
    pub helo_insertion: Option<HeloInsertionCfg>,
    /// Magnetic variation for the theatre, degrees, EAST positive (Caucasus
    /// is about +6.5). Bearings read or spoken to pilots (BRAA, 9-line, BRC,
    /// ATIS surface wind) and the ATIS active-runway pick are magnetic. Omit
    /// to use the engine's built-in approximate per-theatre value.
    #[serde(default)]
    pub magnetic_variation_deg: Option<f64>,
    /// Air activity for thin servers: an on-call AI wingman (F10 > Wingman),
    /// autonomous AI CAP/strike packages for a side short of human pilots, and
    /// neutral civilian airliners. Absent = all three off.
    #[serde(default)]
    pub air_life: Option<AirLifeCfg>,
    /// Modern-war systems modelled on Russia-Ukraine and the 2025 Iran war:
    /// electronic warfare, finite SAM interceptors, drone and missile raids
    /// on infrastructure, sea-drone raids and campaign tempo. Absent = off.
    #[serde(default)]
    pub modern_war: Option<ModernWarCfg>,
    /// The dynamic ground war: AI formations made of objective garrisons
    /// that leave their bases, march on the enemy and fight, under an AI
    /// commander and the players. See `GroundWarCfg`. Absent = off.
    #[serde(default)]
    pub ground_war: Option<GroundWarCfg>,
    /// Who may command: commander access earned by rank, or granted by an
    /// admin (`CommandCfg`). On with defaults when absent.
    #[serde(default)]
    pub command: CommandCfg,
    /// Base garrisons defend themselves: react to enemy ground units, call
    /// a quick reaction force, patrol (`GarrisonCfg`). On with defaults when
    /// absent.
    #[serde(default)]
    pub garrison: GarrisonCfg,
}

/// Replace `path` with `data` so that a crash or power cut leaves either the
/// old file or the new one, never a truncated mix: write a sibling temp
/// file, fsync it, then rename it over the target. The temp file is removed
/// if any step fails. (A rename without the fsync could still surface an
/// empty file after a hard kill, which is what corrupted save files before.)
fn write_atomic(path: &Path, data: &[u8]) -> Result<()> {
    use std::io::Write;
    // append rather than set_extension: config names contain dots
    let mut tmp = path.as_os_str().to_os_string();
    tmp.push(".tmp");
    let tmp = PathBuf::from(tmp);
    let res = (|| -> Result<()> {
        let mut fd =
            File::create(&tmp).with_context(|| format_compact!("creating {:?}", tmp))?;
        fd.write_all(data).with_context(|| format_compact!("writing {:?}", tmp))?;
        fd.sync_all().with_context(|| format_compact!("syncing {:?}", tmp))?;
        drop(fd);
        fs::rename(&tmp, path)
            .with_context(|| format_compact!("moving {:?} into place", tmp))?;
        Ok(())
    })();
    if res.is_err() {
        let _ = fs::remove_file(&tmp);
    }
    res
}

/// Collect the object keys present in `raw` (the config as written) that
/// have no counterpart in `known` (the parsed config serialized back), i.e.
/// keys serde ignored. Only descends where both sides have the same shape;
/// map-valued settings serialize every key they parsed, so they never
/// report false positives.
fn unknown_keys(
    raw: &serde_json::Value,
    known: &serde_json::Value,
    path: &mut std::string::String,
    out: &mut Vec<std::string::String>,
) {
    use serde_json::Value as J;
    match (raw, known) {
        (J::Object(r), J::Object(k)) => {
            for (key, rv) in r {
                // `_note`-style keys are comments by convention in the live
                // configs (JSON has no comments); they are ignored on purpose.
                if key.starts_with('_') {
                    continue;
                }
                let len = path.len();
                if !path.is_empty() {
                    path.push('.');
                }
                path.push_str(key);
                match k.get(key) {
                    None => out.push(path.clone()),
                    Some(kv) => unknown_keys(rv, kv, path, out),
                }
                path.truncate(len);
            }
        }
        (J::Array(r), J::Array(k)) if r.len() == k.len() => {
            for (i, (rv, kv)) in r.iter().zip(k).enumerate() {
                let len = path.len();
                path.push_str(&format_compact!("[{i}]"));
                unknown_keys(rv, kv, path, out);
                path.truncate(len);
            }
        }
        _ => (),
    }
}

#[cfg(test)]
mod load_tests {
    use super::*;

    #[test]
    fn unknown_keys_reports_only_ignored_keys() {
        let raw = serde_json::json!({
            "a": 1, "typo": 2, "_note": "a comment",
            "nested": {"b": 1, "bogus": true, "_why": "also a comment"},
            "list": [{"c": 1, "extra": 0}]
        });
        let known = serde_json::json!({
            "a": 1, "nested": {"b": 1}, "list": [{"c": 1}], "unset": null
        });
        let mut out = vec![];
        unknown_keys(&raw, &known, &mut std::string::String::new(), &mut out);
        out.sort();
        assert_eq!(out, vec!["list[0].extra", "nested.bogus", "typo"]);
    }

    /// `BF_CHECK_CFG=path1;path2 cargo test -p bfprotocols check_named_cfgs`
    /// loads real campaign configs as the engine would and lists every key
    /// under `smart_commander.hq` and `ground_war` the engine would ignore.
    /// Does nothing without the variable.
    #[test]
    fn check_named_cfgs() {
        let Ok(paths) = std::env::var("BF_CHECK_CFG") else { return };
        for path in paths.split(';').filter(|p| !p.is_empty()) {
            let text = std::fs::read_to_string(path).unwrap();
            let text = text.trim_start_matches('\u{feff}');
            let raw: serde_json::Value = serde_json::from_str(text).unwrap();
            let cfg: Cfg =
                serde_json::from_str(text).unwrap_or_else(|e| panic!("{path}: {e}"));
            let hq = cfg
                .smart_commander
                .as_ref()
                .and_then(|s| s.hq.as_ref())
                .expect("no smart_commander.hq");
            let known = serde_json::to_value(&cfg).unwrap();
            let mut out = vec![];
            for key in ["smart_commander", "ground_war"] {
                let mut p = std::string::String::from(key);
                unknown_keys(&raw[key], &known[key], &mut p, &mut out);
            }
            let out: Vec<_> = out
                .into_iter()
                .filter(|k| {
                    k.starts_with("smart_commander.hq") || k.starts_with("ground_war")
                })
                .collect();
            eprintln!(
                "{path}: loads; hq sides {:?}, cost_scale {}, air rosters (cap, strike, sead) {:?}; ignored keys {:?}",
                hq.sides,
                hq.cost_scale,
                hq.air.iter().map(|(s, a)| (*s, a.cap.len(), a.strike.len(), a.sead.len())).collect::<Vec<_>>(),
                out
            );
            assert!(out.is_empty(), "{path}: keys the engine ignores: {out:?}");
        }
    }

    #[test]
    fn default_cfg_is_valid_and_has_no_unknown_keys() {
        let cfg = Cfg::default();
        cfg.validate().unwrap();
        let v = serde_json::to_value(&cfg).unwrap();
        let back: Cfg = serde_json::from_value(v.clone()).unwrap();
        let mut out = vec![];
        unknown_keys(
            &v,
            &serde_json::to_value(&back).unwrap(),
            &mut Default::default(),
            &mut out,
        );
        assert!(out.is_empty(), "{out:?}");
    }

    #[test]
    fn air_life_empty_blocks_take_defaults() {
        let al: AirLifeCfg = serde_json::from_value(serde_json::json!({
            "wingman": {}, "packages": {}, "civil_traffic": {}
        }))
        .unwrap();
        let w = al.wingman.as_ref().unwrap();
        assert!(w.enabled);
        assert_eq!(w.max_side_pilots, 4);
        assert_eq!(w.lifetime_secs, 3600);
        let p = al.packages.as_ref().unwrap();
        assert!(p.enabled && !p.run_when_empty);
        assert_eq!(p.max_side_pilots, 3);
        let c = al.civil_traffic.as_ref().unwrap();
        assert!(c.enabled);
        assert_eq!(c.aircraft.len(), 3);
        assert!(!c.airline_codes.is_empty());
        assert_eq!(c.shootdown_penalty_points, 500);

        let mut cfg = Cfg::default();
        cfg.air_life = Some(al);
        cfg.validate().unwrap();
        // An airliner whose altitude band is upside down is refused.
        if let Some(c) = cfg.air_life.as_mut().and_then(|a| a.civil_traffic.as_mut()) {
            c.aircraft[0].cruise_alt_min_m = 12_000.;
        }
        assert!(cfg.validate().is_err());
    }

    #[test]
    fn modern_war_empty_blocks_take_defaults() {
        let mw: ModernWarCfg = serde_json::from_value(serde_json::json!({
            "ew": {}, "sam_stock": {}, "raids": {}, "boat_raids": {}, "tempo": {}
        }))
        .unwrap();
        let ew = mw.ew.as_ref().unwrap();
        assert_eq!(ew.gps, GnssMode::Jam);
        assert_eq!(ew.radio, RadioJamMode::Adaptive);
        assert_eq!(ew.band_mhz, (225., 400.));
        let r = mw.raids.as_ref().unwrap();
        assert_eq!(r.hours, Some((20, 5)));
        assert!(r.use_missiles);
        assert_eq!(mw.sam_stock.as_ref().unwrap().missiles_per_launcher, 4);
        assert_eq!(mw.boat_raids.as_ref().unwrap().boat_type.as_str(), "speedboat");
        let rail: RailCfg = serde_json::from_value(serde_json::json!({})).unwrap();
        assert_eq!(rail.train_red[0].as_str(), "Locomotive");
        assert_eq!(rail.train_blue[0].as_str(), "ES44AH");
        let convoy = ConvoyConfig::default();
        assert_eq!(
            convoy.trailer_pairs.get("TZ-22_KrAZ").map(|s| s.as_str()),
            Some("TZ-22_TANK")
        );
        let mut cfg = Cfg::default();
        cfg.modern_war = Some(mw);
        cfg.validate().unwrap();
        if let Some(t) = cfg.modern_war.as_mut().and_then(|m| m.tempo.as_mut()) {
            t.regroup_hours = 0.;
        }
        assert!(cfg.validate().is_err());
        // Enum values are spelled as in the docs.
        let e: EwCfg =
            serde_json::from_value(serde_json::json!({"gps": "Spoof", "radio": "Off"}))
                .unwrap();
        assert_eq!(e.gps, GnssMode::Spoof);
        assert_eq!(e.radio, RadioJamMode::Off);
    }

    #[test]
    fn write_atomic_replaces_and_cleans_up() {
        let dir = std::env::temp_dir().join(format!("bfcfg-test-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        let p = dir.join("x_CFG");
        write_atomic(&p, b"one").unwrap();
        write_atomic(&p, b"two").unwrap();
        assert_eq!(fs::read(&p).unwrap(), b"two");
        assert!(!dir.join("x_CFG.tmp").exists());
        let _ = fs::remove_dir_all(&dir);
    }
}

/// The config format version this build writes (see `Cfg::version`).
pub const CFG_VERSION: u32 = 1;

/// Copy back into `new` (what this build would write) every key of `raw`
/// (the file as read) that this build does not know, so rewriting an
/// operator's config never silently deletes a setting a newer build, a typo
/// fix still to come, or a `_note` comment put there. `known` is `raw` as this
/// build parsed and re-serialized it: a key in `raw` but not in `known` is one
/// this build dropped. A key that is known is never copied back, so clearing a
/// known setting still clears it.
fn restore_unknown_keys(
    raw: &serde_json::Value,
    known: &serde_json::Value,
    new: &mut serde_json::Value,
) {
    use serde_json::Value as J;
    match (raw, known, new) {
        (J::Object(r), J::Object(k), J::Object(n)) => {
            for (key, rv) in r {
                match (k.get(key), n.get_mut(key)) {
                    (None, None) => {
                        n.insert(key.clone(), rv.clone());
                    }
                    (Some(kv), Some(nv)) => restore_unknown_keys(rv, kv, nv),
                    _ => (),
                }
            }
        }
        (J::Array(r), J::Array(k), J::Array(n))
            if r.len() == k.len() && k.len() == n.len() =>
        {
            for ((rv, kv), nv) in r.iter().zip(k).zip(n.iter_mut()) {
                restore_unknown_keys(rv, kv, nv)
            }
        }
        _ => (),
    }
}

impl Cfg {
    /// `self` as JSON, plus any keys `raw` (the file being replaced) has that
    /// this build does not know. See `restore_unknown_keys`.
    fn to_json_keeping_unknown(
        &self,
        raw: Option<(&serde_json::Value, &serde_json::Value)>,
    ) -> Result<std::string::String> {
        let mut new = serde_json::to_value(self).context("serializing cfg")?;
        if let Some((raw, known)) = raw {
            restore_unknown_keys(raw, known, &mut new);
        }
        Ok(serde_json::to_string_pretty(&new)?)
    }

    fn path(miz_state_path: &Path) -> PathBuf {
        let mut path = PathBuf::from(miz_state_path);
        let file_name = path
            .file_name()
            .map(|s| {
                let mut s = s.to_string_lossy().into_owned();
                s.push_str("_CFG");
                s
            })
            .unwrap_or_else(|| "CFG".into());
        path.set_file_name(file_name);
        path
    }

    pub fn load(miz_state_path: &Path) -> Result<Self> {
        let path = Self::path(miz_state_path);
        let file = loop {
            match File::open(&path) {
                Ok(f) => break f,
                Err(e) => match e.kind() {
                    io::ErrorKind::NotFound => {
                        let default = serde_json::to_string_pretty(&Cfg::default())
                            .map_err(|e| {
                                anyhow!("could not encode default config {}", e)
                            })?;
                        write_atomic(&path, default.as_bytes()).map_err(|e| {
                            anyhow!("could not write default config {:?}", e)
                        })?;
                    }
                    e => {
                        return Err(anyhow!("error opening config file {:?}", e));
                    }
                },
            }
        };
        let text = io::read_to_string(file)
            .map_err(|e| anyhow!("failed to read cfg file {:?}, {:?}", path, e))?;
        let mut cfg: Self = serde_json::from_str(&text)
            .map_err(|e| anyhow!("failed to decode cfg file {:?}, {:?}", path, e))?;
        if let Some(v) = cfg.version.filter(|v| *v > CFG_VERSION) {
            warn!(
                "cfg {:?} is version {v}, from a newer engine (this one reads up to \
                 {CFG_VERSION}): settings it does not know are ignored",
                path
            )
        }
        // Unknown keys are silently dropped by serde (deny_unknown_fields is
        // off so old configs keep loading), which hid typos like
        // "tk_windw": the setting just quietly ran at its default. Say so.
        // Done before the deprecated-field migration so the comparison sees
        // the deprecated keys still in place.
        let raw_and_known = match serde_json::from_str::<serde_json::Value>(&text)
            .and_then(|raw| Ok((raw, serde_json::to_value(&cfg)?)))
        {
            Ok((raw, known)) => {
                let mut unknown = vec![];
                unknown_keys(&raw, &known, &mut std::string::String::new(), &mut unknown);
                for key in unknown {
                    warn!("cfg {:?}: unknown key {key} is ignored (typo, or removed setting?)", path)
                }
                Some((raw, known))
            }
            Err(e) => {
                warn!("cfg {:?}: could not check for unknown keys {e:?}", path);
                None
            }
        };
        for actions in cfg.actions.values_mut() {
            actions.sort_by(|name0, _, name1, _| name0.cmp(name1));
        }
        // translate deployables to the new format
        let mut has_deprecated = false;
        for deps in cfg.deployables.values_mut() {
            for dep in deps.iter_mut() {
                if let Some(mut parts) = dep.deprecated_logistics.take() {
                    parts.defenses_template = dep.deprecated_template.take();
                    dep.kind = DeployableKind::Objective(parts);
                    has_deprecated = true;
                } else if let Some(template) = dep.deprecated_template.take() {
                    dep.kind = DeployableKind::Group { template };
                    has_deprecated = true;
                }
            }
        }
        if has_deprecated {
            // the operator's only copy of their config: never leave it
            // half-written if we die mid-write, and never drop a key this
            // build does not understand. If the file could not be compared
            // (it parsed as a Cfg but not as JSON, which cannot happen) it
            // is left alone rather than rewritten blind.
            match &raw_and_known {
                Some((raw, known)) => {
                    let mut migrated = cfg.clone();
                    migrated.version = Some(CFG_VERSION);
                    let text = migrated.to_json_keeping_unknown(Some((raw, known)))?;
                    write_atomic(&path, text.as_bytes())?
                }
                None => warn!("cfg {:?}: not rewriting the migrated deployables", path),
            }
        }
        cfg.validate().with_context(|| format_compact!("invalid cfg {:?}", path))?;
        for key in cfg.unused_settings() {
            warn!("cfg {path:?}: {key} is set, but nothing in the engine reads it: it has no effect")
        }
        Ok(cfg)
    }

    /// Settings that are parsed but that nothing reads, so `load` can tell an
    /// operator who set one that it does nothing. Only values that differ
    /// from the default count as set: a config written out from the defaults
    /// has every key.
    pub fn unused_settings(&self) -> Vec<&'static str> {
        let mut unused: Vec<&'static str> = vec![];
        let mut check = |set: bool, key: &'static str| {
            if set {
                unused.push(key)
            }
        };
        check(self.pilot_experience.is_some(), "pilot_experience");
        check(self.counter_battery.is_some(), "counter_battery");
        check(self.ewr_cull_distance != default_ewr_cull_distance(), "ewr_cull_distance");
        check(
            self.weapon_spawn_radius != default_weapon_spawn_radius(),
            "weapon_spawn_radius",
        );
        if let Some(f) = &self.frontline {
            check(f.max_marks != default_frontline_max_marks(), "frontline.max_marks");
            check(
                f.territory_zone_alpha != default_territory_zone_alpha(),
                "frontline.territory_zone_alpha",
            );
        }
        if let Some(w) = &self.weather_effects {
            check(
                w.min_visibility_fixed_wing != default_min_vis_fixed_wing(),
                "weather_effects.min_visibility_fixed_wing",
            );
            check(!w.no_helo_in_thunderstorm, "weather_effects.no_helo_in_thunderstorm");
            check(
                w.ewr_weather_range_multiplier != default_ewr_weather_mult(),
                "weather_effects.ewr_weather_range_multiplier",
            );
        }
        if let Some(e) = &self.campaign_events {
            check(
                e.event_probability != default_event_probability(),
                "campaign_events.event_probability",
            );
            check(
                e.vip_reward_points != default_vip_reward(),
                "campaign_events.vip_reward_points",
            );
            check(
                e.evacuation_reward_per_civilian != default_evac_reward(),
                "campaign_events.evacuation_reward_per_civilian",
            );
            check(
                e.cap_orbit_radius_m != default_cap_orbit_radius(),
                "campaign_events.cap_orbit_radius_m",
            );
        }
        if let Some(g) = &self.ground_war {
            check(
                g.attrition_rate != ground_war::default_attrition_rate(),
                "ground_war.attrition_rate",
            );
            check(
                g.attrition_secs != ground_war::default_attrition_secs(),
                "ground_war.attrition_secs",
            );
        }
        let naval_cost = self.actions.values().flat_map(|a| a.values()).any(|a| {
            matches!(&a.kind, ActionKind::NavalCruiseMissileStrike(c) if c.supply_cost != 0)
        });
        check(naval_cost, "actions.*.NavalCruiseMissileStrike.supply_cost");
        unused
    }

    /// Sanity checks on values that would crash or silently disable a core
    /// system. Hard errors only for values that are never meaningful (a
    /// divisor of zero, a message budget of zero); merely odd values are
    /// logged as warnings so an existing server still starts.
    pub fn validate(&self) -> Result<()> {
        let cfg = self;
        if let Some(t) = &cfg.netidx_tls {
            t.validate()?;
        }
        if let Some(points) = &cfg.points {
            // player.rs divides the team-kill age by it: 0 panics on the first TK
            if points.tk_window == 0 {
                bail!("points.tk_window must be at least 1 (hours)")
            }
        }
        // the message queue drains at this rate; 0 means no chat, no F10
        // markup and no panels ever go out
        if cfg.max_msgs_per_second == 0 {
            bail!("max_msgs_per_second must be at least 1")
        }
        if let Some(wh) = &cfg.warehouse {
            // the logistics tick period in minutes; 0 runs the whole
            // warehouse sync every frame
            if wh.tick == 0 {
                bail!("warehouse.tick must be at least 1 (minutes)")
            }
            if wh.ticks_per_delivery == 0 {
                warn!("warehouse.ticks_per_delivery is 0: every tick is a delivery")
            }
        }
        if cfg.slow_timed_events_freq == 0 {
            warn!("slow_timed_events_freq is 0: slow timed events run every frame")
        }
        if let Some(ps) = cfg.population_scaling.as_ref().filter(|p| p.enabled) {
            for (name, m) in [
                (
                    "capture_time_mult_when_undefended",
                    ps.capture_time_mult_when_undefended,
                ),
                ("max_capture_time_mult", ps.max_capture_time_mult),
                (
                    "repair_speed_mult_when_undefended",
                    ps.repair_speed_mult_when_undefended,
                ),
            ] {
                if !(m >= 1.) {
                    warn!("population_scaling.{name} is {m}: anything under 1 is treated as 1 (no effect)")
                }
            }
            if ps.min_defenders == 0 {
                warn!("population_scaling.min_defenders is 0: no side ever counts as undefended")
            }
        }
        if let Some(er) = cfg.emergency_repair.as_ref().filter(|e| e.enabled) {
            if er.crate_def.required != 1 {
                warn!(
                    "emergency_repair.crate.required is {}: an emergency repair always takes one crate",
                    er.crate_def.required
                )
            }
            // Crates are matched by name everywhere (spawn menus, unpack,
            // C-130 tracking), so a shared name would make the emergency
            // crate unpack as something else, or vice versa.
            let name = &er.crate_def.name;
            let wh = cfg.warehouse.as_ref();
            let clash = cfg.repair_crate.values().any(|c| &c.name == name)
                || wh.is_some_and(|w| {
                    w.supply_transfer_fuel_crate
                        .values()
                        .chain(w.supply_transfer_weapons_crate.values())
                        .chain(w.carrier_repair_crate.values())
                        .any(|c| &c.name == name)
                })
                || cfg.deployables.values().flatten().any(|d| {
                    d.crates.iter().chain(d.repair_crate.iter()).any(|c| &c.name == name)
                });
            if clash {
                bail!("emergency_repair.crate.name '{name}' is already used by another crate")
            }
        }
        if let Some(sc) = &cfg.smart_commander {
            for (name, cost) in [
                ("barrage_cost", sc.barrage_cost),
                ("ambush_cost", sc.ambush_cost),
                ("cap_cost", sc.cap_cost),
                ("action_reserve", sc.action_reserve),
                ("treasury_income_amount", sc.treasury_income_amount),
            ] {
                if cost < 0 {
                    warn!("smart_commander.{name} is negative ({cost}): it pays the side instead")
                }
            }
        }
        // A reactive air response with no templates to spawn is a config that
        // looks enabled and does nothing: every scramble picks from an empty
        // roster and cancels itself, hours into a session, in the log only.
        // Refuse to start instead.
        if let Some(ce) = &cfg.campaign_events {
            if ce.enabled && ce.enemy_cap_enabled {
                for (side, roster) in
                    [("red", &ce.cap_templates_red), ("blue", &ce.cap_templates_blue)]
                {
                    if roster.is_empty() {
                        bail!(
                            "campaign_events.enemy_cap_enabled is on but cap_templates_{side} \
                             is empty -- list at least one plane-section group from the .miz, \
                             e.g. [{{\"template\": \"RCAP\"}}]"
                        )
                    }
                }
            }
            if ce.enabled && ce.enemy_helo_enabled {
                for (side, roster) in
                    [("red", &ce.helo_templates_red), ("blue", &ce.helo_templates_blue)]
                {
                    if roster.is_empty() {
                        bail!(
                            "campaign_events.enemy_helo_enabled is on but helo_templates_{side} \
                             is empty -- list at least one helicopter-section group from the .miz"
                        )
                    }
                }
            }
        }
        if let Some(al) = &cfg.air_life {
            if let Some(w) = al.wingman.as_ref().filter(|w| w.enabled) {
                let cap = cfg.campaign_events.as_deref();
                for (side, own, fallback) in [
                    ("red", &w.templates_red, cap.map(|c| c.cap_templates_red.len())),
                    ("blue", &w.templates_blue, cap.map(|c| c.cap_templates_blue.len())),
                ] {
                    if own.is_empty() && fallback.unwrap_or(0) == 0 {
                        warn!(
                            "air_life.wingman: templates_{side} is empty and there is no \
                             campaign_events.cap_templates_{side} to fall back on -- {side} \
                             jet pilots will be told no wingman is available"
                        )
                    }
                }
            }
            if let Some(ct) = al.civil_traffic.as_ref().filter(|c| c.enabled) {
                if ct.aircraft.is_empty() {
                    bail!("air_life.civil_traffic is enabled but its aircraft list is empty")
                }
                for a in &ct.aircraft {
                    if a.cruise_alt_min_m > a.cruise_alt_max_m || a.cruise_speed_ms <= 0.
                    {
                        bail!(
                            "air_life.civil_traffic.aircraft {}: cruise_alt_min_m must not exceed \
                             cruise_alt_max_m and cruise_speed_ms must be positive",
                            a.typ
                        )
                    }
                }
                if ct.airline_codes.is_empty() {
                    bail!("air_life.civil_traffic.airline_codes is empty")
                }
            }
        }
        if let Some(mw) = &cfg.modern_war {
            const KINDS: &[&str] = &[
                "Airbase",
                "FOB",
                "FARP",
                "Logistics Hub",
                "Naval Base",
                "Carrier Group",
                "Factory",
                "Special SAM Site",
                "Command Center",
            ];
            let check_kinds = |what: &str, kinds: &[String]| {
                for k in kinds {
                    if !KINDS.contains(&k.as_str()) {
                        warn!("modern_war.{what}: '{k}' is not an objective kind; expected one of {KINDS:?}")
                    }
                }
            };
            if let Some(ew) = mw.ew.as_ref().filter(|e| e.enabled) {
                check_kinds("ew.host_kinds", &ew.host_kinds);
                check_kinds("ew.spoof_kinds", &ew.spoof_kinds);
                check_kinds("ew.sam_site_kinds", &ew.sam_site_kinds);
                for side in ["GPS_Spoofer_Blue", "GPS_Spoofer_Red"] {
                    if !cfg.unit_classification.contains_key(side) {
                        warn!("modern_war.ew: {side} is not in unit_classification -- that side's jammers can't spawn")
                    }
                }
            }
            if let Some(r) = mw.raids.as_ref().filter(|r| r.enabled) {
                check_kinds("raids.target_kinds", &r.target_kinds);
                if r.drone_templates_red.is_empty()
                    && r.drone_templates_blue.is_empty()
                    && !r.use_missiles
                {
                    warn!("modern_war.raids: no drone templates and use_missiles off -- nothing can raid")
                }
                if let Some((a, b)) = r.hours {
                    if a > 23 || b > 24 {
                        bail!("modern_war.raids.hours must be within 0..24")
                    }
                }
            }
            if let Some(t) = mw.tempo.as_ref().filter(|t| t.enabled) {
                if t.offensive_hours <= 0. || t.regroup_hours <= 0. {
                    bail!("modern_war.tempo: offensive_hours and regroup_hours must be positive")
                }
            }
        }
        Ok(())
    }

    /// Write the config, keeping any key the file on disk has that this
    /// build does not know (and `_note` comments), so saving from an older
    /// build or after a typo does not delete the operator's settings.
    pub fn save(&self, miz_state_path: &Path) -> Result<()> {
        let path = Self::path(miz_state_path);
        let old = fs::read_to_string(&path).ok().and_then(|text| {
            let raw = serde_json::from_str::<serde_json::Value>(&text).ok()?;
            let known =
                serde_json::to_value(serde_json::from_str::<Cfg>(&text).ok()?).ok()?;
            Some((raw, known))
        });
        let s = self.to_json_keeping_unknown(old.as_ref().map(|(r, k)| (r, k)))?;
        write_atomic(&path, s.as_bytes())
    }

    pub fn check_vehicle_has_threat_distance(&self, vehicle: &Vehicle) -> Result<()> {
        match self.threatened_distance.get(vehicle) {
            Some(_) => (),
            None => bail!(
                "vehicle {:?} doesn't have a configured theatened distance",
                vehicle
            ),
        }
        Ok(())
    }

    pub fn check_vehicle_has_life_type(&self, vehicle: &Vehicle) -> Result<()> {
        match self.life_types.get(vehicle) {
            None => bail!("vehicle {:?} doesn't have a configured life type", vehicle),
            Some(typ) => match self.default_lives.get(typ) {
                Some((n, f)) if *n > 0 && *f > 0 => (),
                None => bail!("vehicle {:?} has no configured life type", vehicle),
                Some((n, f)) => {
                    bail!(
                        "vehicle {:?} life type {:?} has no configured lives ({n}) or negative reset time ({f})",
                        vehicle, typ
                    )
                }
            },
        }
        Ok(())
    }
}

#[cfg(test)]
mod secret_tests {
    use super::*;

    #[test]
    fn secret_round_trips_but_does_not_print() {
        let cfg: LiveWeatherConfig = serde_json::from_str(
            r#"{"lat":1,"lon":2,"checkwx_api_key":"abc123","metar_station":"PGUA"}"#,
        )
        .unwrap();
        let key = cfg.checkwx_api_key.as_ref().unwrap();
        assert_eq!(key.expose(), "abc123");
        assert_eq!(cfg.checkwx_api_key.as_deref(), Some("abc123"));
        assert!(!format!("{cfg:?}").contains("abc123"));
        assert_eq!(key.to_string(), "<redacted>");
        // what Cfg::load writes back still has the real key
        let json = serde_json::to_string(&cfg).unwrap();
        assert!(json.contains(r#""checkwx_api_key":"abc123""#));
    }
}

#[cfg(test)]
mod unused_settings_tests {
    use super::*;

    #[test]
    fn defaults_report_nothing_and_set_values_are_named() {
        let mut cfg = Cfg::default();
        assert_eq!(cfg.unused_settings(), Vec::<&str>::new());
        cfg.weapon_spawn_radius = 1;
        cfg.weather_effects = Some(WeatherEffectsCfg::default());
        assert_eq!(cfg.unused_settings(), vec!["weapon_spawn_radius"]);
        cfg.weather_effects.as_mut().unwrap().no_helo_in_thunderstorm = false;
        assert_eq!(
            cfg.unused_settings(),
            vec!["weapon_spawn_radius", "weather_effects.no_helo_in_thunderstorm"]
        );
    }
}

#[cfg(test)]
mod keep_unknown_tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn restore_puts_back_only_unknown_keys() {
        let raw = json!({
            "a": 1, "future": {"x": 1}, "_note": "why a is 1",
            "nested": {"b": 1, "typo": 2}, "list": [{"c": 1, "extra": 0}],
            "cleared": 5
        });
        // what this build parsed: it knows a, nested.b, list[].c, cleared
        let known = json!({"a": 1, "nested": {"b": 1}, "list": [{"c": 1}], "cleared": 5});
        // what it now wants to write: a changed, cleared removed
        let mut new = json!({"a": 2, "nested": {"b": 1}, "list": [{"c": 1}]});
        restore_unknown_keys(&raw, &known, &mut new);
        assert_eq!(
            new,
            json!({
                "a": 2, "future": {"x": 1}, "_note": "why a is 1",
                "nested": {"b": 1, "typo": 2}, "list": [{"c": 1, "extra": 0}]
            })
        );
    }

    #[test]
    fn save_keeps_unknown_keys_and_versions_are_optional() {
        let dir = std::env::temp_dir().join(format!("bfcfg-keep-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        let miz = dir.join("state");
        let cfg = Cfg::default();
        // an operator file with a comment and a key from some other build
        let mut v = serde_json::to_value(&cfg).unwrap();
        v["_note"] = json!("hand edited");
        v["setting_from_the_future"] = json!({"on": true});
        v.as_object_mut().unwrap().remove("version");
        fs::write(Cfg::path(&miz), serde_json::to_string(&v).unwrap()).unwrap();
        // a file without a version still loads
        let mut loaded = Cfg::load(&miz).unwrap();
        assert_eq!(loaded.version, None);
        loaded.max_msgs_per_second = 7;
        loaded.save(&miz).unwrap();
        let back: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(Cfg::path(&miz)).unwrap()).unwrap();
        assert_eq!(back["_note"], json!("hand edited"));
        assert_eq!(back["setting_from_the_future"], json!({"on": true}));
        assert_eq!(back["max_msgs_per_second"], json!(7));
        let _ = fs::remove_dir_all(&dir);
    }
}
