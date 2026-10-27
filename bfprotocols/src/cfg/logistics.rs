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

//! Warehouses, materiel, production, convoys, air and sea logistics.

use super::*;

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct WarehouseConfig {
    /// Logistics hub max supply stock as a multiple of the delivery amount.
    /// This is the depth of the theatre stockpile: at 1 a hub can hold
    /// exactly one delivery, so it fills completely every delivery and
    /// empties completely on the next distribution tick, and neither side can
    /// ever build up (or be ground down out of) a reserve. Give it several
    /// deliveries of headroom if you want the supply situation to be
    /// something the campaign can actually win or lose.
    pub hub_max: u32,
    /// Airbase max supply stock as a multiple of the delivery amount. The
    /// depth of a front-line base's own magazine -- how long it can keep
    /// generating sorties with its supply line cut.
    pub airbase_max: u32,
    /// Airframe stockpile depth, as a multiple of the supply source's count
    /// for that type, replacing `hub_max` / `airbase_max` for aircraft only.
    ///
    /// Aircraft and bullets want very different depths: a base holding three
    /// deliveries' worth of AMRAAM is sensible, a base holding three
    /// deliveries' worth of airframes has an aircraft carrier's worth of jets
    /// parked on it and will never run short however badly the war is going.
    /// `(hub, airbase)`. Omit to use `hub_max` / `airbase_max` for airframes
    /// too, which is the original behaviour.
    #[serde(default)]
    pub airframe_max: Option<(u32, u32)>,
    /// Fraction of a hub's capacity (0-100) that it will not ship forward.
    /// A depot that empties itself into the first convoy that asks has no
    /// operational reserve: one lost convoy and the whole theatre is dry
    /// until the next production delivery. The reserve is still available to
    /// hub-to-hub balancing and to the player-driven supply transfer -- it
    /// just isn't handed out automatically.
    #[serde(default = "default_hub_reserve_percent")]
    pub hub_reserve_percent: u8,
    /// Logistics tick in minutes. Supplies move automatically every tick
    pub tick: u32,
    /// How many logistics ticks does it take before supplies are delivered
    /// from outside
    pub ticks_per_delivery: u32,
    /// The supply transfer crate for fuel
    #[serde(default)]
    pub supply_transfer_fuel_crate: FxHashMap<Side, Crate>,
    /// The supply transfer crate for weapons/equipment
    #[serde(default)]
    pub supply_transfer_weapons_crate: FxHashMap<Side, Crate>,
    /// The percentage of supply that is transfered by a transfer crate
    pub supply_transfer_size: u8,
    /// The carrier repair crate
    #[serde(default)]
    pub carrier_repair_crate: FxHashMap<Side, Crate>,
    /// The name of the warehouse that is the source of supply every
    /// restart
    pub supply_source: FxHashMap<Side, String>,
    /// Airframes that do not play nice with the warehouse that are exempt from the
    /// warehouse check
    #[serde(default)]
    pub exempt_airframes: FxHashSet<String>,
    /// Objective names (exact match) that should never run low on aircraft or
    /// supplies -- e.g. a home/rear airbase you want guaranteed always fully
    /// stocked regardless of production or consumption. Only applies to
    /// items the objective's side actually has access to (a nonzero entry in
    /// its supply_source inventory); it does not grant types the side
    /// doesn't otherwise have.
    #[serde(default)]
    pub unlimited_objectives: FxHashSet<String>,
    /// Convoy system configuration (optional, defaults to disabled)
    #[serde(default)]
    pub convoy: Option<ConvoyConfig>,
    /// Air logistics configuration (optional, defaults to disabled)
    #[serde(default)]
    pub air_logistics: Option<AirLogisticsConfig>,
    /// Sea logistics configuration (optional, defaults to disabled)
    #[serde(default)]
    pub sea_logistics: Option<SeaLogisticsConfig>,
    /// Materiel: a single generic war-stock commodity (fuel drums, spares,
    /// engineering plant, ammunition that isn't a specific pylon load) that
    /// repairs and deployments are actually paid for in. Optional; when it is
    /// absent, repair and deploy fall back to shaving a flat percentage off
    /// every item type in the warehouse, which is what the campaign did
    /// before -- patching a runway would consume 5% of your fighter airframes
    /// and 5% of every missile type on the base.
    #[serde(default)]
    pub materiel: Option<MaterielConfig>,
    /// Ties each side's production output to how the war is going. Without
    /// it, production is a fixed constant: losing half the map, or having
    /// every factory flattened, changes nothing about how much materiel
    /// arrives at the hubs.
    #[serde(default)]
    pub production_scaling: Option<ProductionScalingConfig>,
    /// Make supply routes something the enemy can cut. A hub is only a
    /// candidate supplier for a base if the ground between them is clear of
    /// enemy-held objectives, and a base whose road is severed can only be
    /// resupplied by air. With this off, supply is assigned by straight-line
    /// distance alone -- a depot on the far side of the front line will
    /// happily truck fuel through it.
    #[serde(default = "default_true")]
    pub front_line_routing: bool,
    /// How far either side of an enemy-held objective the ground is treated
    /// as interdicted, on top of that objective's own zone radius. Zone radii
    /// are small enough that on their own they only block a road running
    /// almost through the base; this is the width of the belt its garrison
    /// actually denies.
    #[serde(default = "default_route_block_margin")]
    pub route_block_margin_m: f64,
    /// What happens to the aircraft the losing side left behind when an
    /// objective changes hands. Omit (the default) and they are destroyed on
    /// capture, which is the original behaviour.
    #[serde(default)]
    pub captured_airframes: Option<CapturedAirframeConfig>,
    /// Percentage (0-100) of a captured base's airframe capacity the captors
    /// start with, for each type they produce -- the quick-reaction force
    /// flown in on capture while the supply line catches up. 0 (the default)
    /// means a captured base holds only what was salvaged and waits for
    /// transports. Rounded up, so any nonzero value gives at least one of
    /// each type the base can hold.
    #[serde(default)]
    pub capture_airframe_stock_percent: u8,
}

pub(super) fn default_route_block_margin() -> f64 {
    12000.
}

/// Salvage rules for enemy aircraft left on a captured objective.
///
/// A base that changes hands is holding whatever the previous owner had
/// parked on it. Without this, `capture_warehouse` zeroes every airframe the
/// new owner does not itself produce, so taking a fully-stocked enemy airbase
/// yields nothing but the ramp. With it, a fraction of that stock survives
/// and the captors can fly it -- until it runs out, because nothing in the
/// supply system will ever replace an airframe the side does not produce.
///
/// This only decides what is in the *warehouse*. Whether anyone can actually
/// climb into one depends on the mission having client slots of that type on
/// the captor's coalition at that base -- see bftools' `captured` slot-zone
/// property, which generates them -- or on the base using DCS dynamic slots,
/// which read their roster straight out of the warehouse.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CapturedAirframeConfig {
    /// Percentage of the previous owner's stock that survives the capture
    /// (0-100). The rest is assumed destroyed on the ramp, sabotaged, or
    /// flown out ahead of the assault. Anything that rounds down to zero
    /// leaves no entry at all.
    #[serde(default = "default_salvage_percent")]
    pub salvage_percent: u8,
    /// Hard ceiling on how many of any one type can be salvaged, regardless
    /// of `salvage_percent`. Stops the capture of a deep rear airbase from
    /// handing over a whole extra squadron.
    #[serde(default)]
    pub max_per_type: Option<u32>,
    /// Minimum objective health (0-100) before salvaged airframes can be
    /// slotted. Captured jets are not flyable the instant the last defender
    /// dies -- the base has to be consolidated and repaired first. 100 mirrors
    /// the rule captured carriers already use; set 0 to make them available
    /// immediately.
    #[serde(default = "default_captured_min_health")]
    pub min_health: u8,
    /// Airframe types that never survive a capture, whatever the roll. Use it
    /// for modules you do not want appearing on the wrong coalition at all.
    #[serde(default)]
    pub exclude: FxHashSet<String>,
}

pub(super) fn default_salvage_percent() -> u8 {
    25
}

pub(super) fn default_captured_min_health() -> u8 {
    100
}

impl Default for CapturedAirframeConfig {
    fn default() -> Self {
        Self {
            salvage_percent: default_salvage_percent(),
            max_per_type: None,
            min_health: default_captured_min_health(),
            exclude: FxHashSet::default(),
        }
    }
}

impl CapturedAirframeConfig {
    /// How many of `stored` survive the capture, after the percentage and the
    /// per-type ceiling. Zero means don't keep an entry at all.
    pub fn salvaged(&self, stored: u32) -> u32 {
        // An UNLIMITED_AIRCRAFTS objective has no real count to take a
        // fraction of -- 25% of `UNLIMITED_CAPACITY` is a quarter of a
        // million jets. Fall back to the explicit ceiling, and salvage
        // nothing at all if the mission never set one.
        if stored >= UNLIMITED_CAPACITY {
            return self.max_per_type.unwrap_or(0);
        }
        let kept = (stored as f32 * (self.salvage_percent.min(100) as f32 / 100.)) as u32;
        match self.max_per_type {
            None => kept,
            Some(cap) => kept.min(cap),
        }
    }
}

/// The warehouse item name the materiel commodity is stored under.
///
/// The `campaign.` prefix marks it as model-only: it deliberately has no
/// entry in DCS's resource map, and the warehouse sync skips every item with
/// this prefix in both directions, so it lives purely in the campaign model
/// and is never pushed into (or read back from, and thereby zeroed by) a DCS
/// warehouse.
pub const MATERIEL_ITEM: &str = "campaign.materiel";

/// True for synthetic campaign-model items that have no DCS counterpart.
pub fn is_model_only_item(name: &str) -> bool {
    name.starts_with("campaign.")
}

/// The generic materiel commodity. See `WarehouseConfig::materiel`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct MaterielConfig {
    /// Master switch. When false the whole commodity is inert and repair /
    /// deploy keep using the old flat-percentage draw.
    pub enabled: bool,
    /// Units produced per side per production delivery, before production
    /// scaling. This is the strategic tap: everything a coalition repairs or
    /// deploys ultimately comes out of it.
    pub hub_production: u32,
    /// Hub stockpile capacity, as a multiple of `hub_production`.
    #[serde(default = "default_materiel_hub_capacity")]
    pub hub_capacity: u32,
    /// Forward-base stockpile capacity, as a multiple of `hub_production`.
    #[serde(default = "default_materiel_airbase_capacity")]
    pub airbase_capacity: u32,
    /// Units consumed each time an objective repairs one group. If the
    /// objective doesn't have this much materiel on hand the repair does not
    /// happen -- the base stays broken until a convoy gets through, which is
    /// the entire point of running a supply line.
    pub repair_cost: u32,
    /// Units drawn from a crate's origin objective for every crate consumed
    /// when a deployable is unpacked.
    pub deploy_cost: u32,
}

pub(super) fn default_materiel_hub_capacity() -> u32 {
    6
}

pub(super) fn default_materiel_airbase_capacity() -> u32 {
    3
}

/// Ties production output to territory held and factories still standing.
/// See `WarehouseConfig::production_scaling`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct ProductionScalingConfig {
    pub enabled: bool,
    /// Output floor as a percentage of nominal. However badly a coalition is
    /// losing, it keeps producing at least this much -- a side that gets
    /// ground down to zero production can never come back, which makes for a
    /// miserable campaign rather than a realistic one.
    #[serde(default = "default_production_floor")]
    pub floor_percent: u8,
    /// Output ceiling as a percentage of nominal, for a side that has taken
    /// more than it started with. Keep this close to 100 -- an unbounded
    /// bonus for winning turns every campaign into a runaway.
    #[serde(default = "default_production_ceiling")]
    pub ceiling_percent: u8,
    /// Territory weights. Each objective a side holds contributes its weight
    /// scaled by that objective's logistics health, and the side's output is
    /// the ratio of its current score to the score it started the campaign
    /// with.
    #[serde(default = "default_weight_airbase")]
    pub airbase_weight: f64,
    #[serde(default = "default_weight_logistics")]
    pub logistics_weight: f64,
    #[serde(default = "default_weight_factory")]
    pub factory_weight: f64,
    #[serde(default = "default_weight_command_center")]
    pub command_center_weight: f64,
}

pub(super) fn default_production_floor() -> u8 {
    35
}

pub(super) fn default_production_ceiling() -> u8 {
    125
}

pub(super) fn default_weight_airbase() -> f64 {
    1.0
}

pub(super) fn default_weight_logistics() -> f64 {
    3.0
}

pub(super) fn default_weight_factory() -> f64 {
    4.0
}

pub(super) fn default_weight_command_center() -> f64 {
    2.0
}

impl Default for ProductionScalingConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            floor_percent: default_production_floor(),
            ceiling_percent: default_production_ceiling(),
            airbase_weight: default_weight_airbase(),
            logistics_weight: default_weight_logistics(),
            factory_weight: default_weight_factory(),
            command_center_weight: default_weight_command_center(),
        }
    }
}

pub(super) fn default_hub_reserve_percent() -> u8 {
    20
}

/// Effectively unlimited for gameplay purposes, but small enough that
/// percentage/ratio math elsewhere (stored as f32 / capacity as f32) stays
/// well-behaved instead of flirting with u32::MAX overflow.
pub const UNLIMITED_CAPACITY: u32 = 1_000_000;

impl WarehouseConfig {
    /// How much of `inv` a hub is willing to release to forward bases: its
    /// stock less the reserve floor. Returns 0 for a hub sitting at or below
    /// its reserve.
    pub fn releasable(&self, stored: u32, capacity: u32) -> u32 {
        let reserve =
            (capacity as f32 * (self.hub_reserve_percent.min(100) as f32 / 100.)) as u32;
        stored.saturating_sub(reserve)
    }

    pub fn capacity(&self, hub: bool, qty: u32) -> u32 {
        if hub {
            qty * self.hub_max
        } else {
            qty * self.airbase_max
        }
    }

    /// Like `capacity`, but for airframes, which get their own depth so they
    /// can be scarce while munitions are deep. Falls back to `capacity` when
    /// `airframe_max` isn't configured.
    pub fn airframe_capacity(&self, hub: bool, qty: u32) -> u32 {
        match self.airframe_max {
            None => self.capacity(hub, qty),
            Some((h, a)) => qty * if hub { h } else { a },
        }
    }

    /// `capacity_for`, routed to whichever depth applies to this item.
    pub fn capacity_for_item(
        &self,
        obj_name: &str,
        is_airframe: bool,
        unlimited: bool,
        hub: bool,
        qty: u32,
    ) -> u32 {
        if qty > 0 && (unlimited || self.unlimited_objectives.contains(obj_name)) {
            UNLIMITED_CAPACITY
        } else if is_airframe {
            self.airframe_capacity(hub, qty)
        } else {
            self.capacity(hub, qty)
        }
    }

    /// Like `capacity`, but returns UNLIMITED_CAPACITY when `unlimited` is
    /// true -- either because the objective's own UNLIMITED_SUPPLY trigger
    /// zone property is set, or its name is listed in `unlimited_objectives`
    /// here. Only meaningful for items the side already has access to
    /// (qty > 0 upstream) -- this never grants a type the side doesn't
    /// otherwise produce.
    pub fn capacity_for(
        &self,
        obj_name: &str,
        unlimited: bool,
        hub: bool,
        qty: u32,
    ) -> u32 {
        if qty > 0 && (unlimited || self.unlimited_objectives.contains(obj_name)) {
            UNLIMITED_CAPACITY
        } else {
            self.capacity(hub, qty)
        }
    }
}

/// Configuration for supply convoy system
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct ConvoyConfig {
    /// Enable convoy system. If false, LOGISTICS_DETACHED objectives get no automatic supplies.
    pub enabled: bool,
    /// Truck unit type per side (e.g., "M939" for Blue, "Ural-375" for Red)
    pub truck_template: FxHashMap<Side, String>,
    /// How many trucks per convoy
    pub trucks_per_convoy: u32,
    /// Convoy speed in km/h
    pub speed_kph: f64,
    /// How often to spawn convoys (in logistics ticks)
    pub spawn_interval_ticks: u32,
    /// Maximum convoys that can be in transit at once per side
    pub max_concurrent_convoys: u32,
    /// Minimum distance from destination to consider "delivered" (meters)
    #[serde(default = "default_convoy_delivery_distance")]
    pub delivery_distance: f64,
    /// How often to check convoy status (in seconds)
    #[serde(default = "default_convoy_check_interval")]
    pub check_interval_secs: u32,
    /// How long a convoy may stay on the road before it is written off as
    /// wedged. DCS ground pathing strands convoys on terrain regularly; when
    /// this expires the trucks are despawned and the load is returned to the
    /// hub it came from, so the supply isn't lost to a pathing bug.
    #[serde(default = "default_convoy_max_transit")]
    pub max_transit_minutes: u32,
    /// Minimum gap between two convoys bound for the same destination, in
    /// logistics ticks. Without it every hub dispatched a fresh pair of
    /// convoys to every under-stocked destination on every tick.
    #[serde(default = "default_dispatch_cooldown")]
    pub dispatch_cooldown_ticks: u32,
    /// Template per side for FUEL convoys -- e.g. tractor-trailer refuelers
    /// (KrAZ + TZ-22). Unset side = `truck_template`.
    #[serde(default)]
    pub fuel_template: FxHashMap<Side, String>,
    /// Tractor type -> the trailer type it tows. After a convoy spawns, every
    /// tractor in it is hitched to the nearest free trailer of its type (DCS
    /// `AttachTrailer`); a trailer left unhitched just sits there, so a
    /// template with trailers needs its tractors listed here. Defaults to
    /// every pair DCS ships.
    #[serde(default = "default_trailer_pairs")]
    pub trailer_pairs: FxHashMap<String, String>,
}

pub fn default_trailer_pairs() -> FxHashMap<String, String> {
    [
        ("TZ-22_KrAZ", "TZ-22_TANK"),
        ("ATZ-60_Maz", "ATZ-60_TANK"),
        ("S-75_Zil", "S_75_Zil_Trailer"),
        ("CHAP_HX81_Tractor", "CHAP_SLT50_Trailer"),
        ("CHAP_SLT50_Tractor", "CHAP_SLT50_Trailer"),
    ]
    .into_iter()
    .map(|(a, b)| (String::from(a), String::from(b)))
    .collect()
}

pub(super) fn default_convoy_max_transit() -> u32 {
    90
}

pub(super) fn default_dispatch_cooldown() -> u32 {
    2
}

pub(super) fn default_convoy_delivery_distance() -> f64 {
    500.0
}

pub(super) fn default_convoy_check_interval() -> u32 {
    10
}

impl Default for ConvoyConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            truck_template: FxHashMap::default(),
            trucks_per_convoy: 5,
            speed_kph: 60.0,
            spawn_interval_ticks: 2,
            max_concurrent_convoys: 10,
            delivery_distance: default_convoy_delivery_distance(),
            check_interval_secs: default_convoy_check_interval(),
            max_transit_minutes: default_convoy_max_transit(),
            dispatch_cooldown_ticks: default_dispatch_cooldown(),
            fuel_template: FxHashMap::default(),
            trailer_pairs: default_trailer_pairs(),
        }
    }
}

/// Configuration for automated AI air logistics routes
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct AirLogisticsConfig {
    /// Enable air logistics. When enabled, AI cargo aircraft fly from logistics hubs to
    /// understocked destinations instead of instant warehouse transfers.
    pub enabled: bool,
    /// Cargo aircraft group template name per side (must be a Plane group in the miz file)
    pub aircraft_template: FxHashMap<Side, String>,
    /// Cruise altitude in meters (BARO)
    #[serde(default = "default_air_altitude")]
    pub altitude_m: f64,
    /// Cruise speed in km/h
    pub speed_kph: f64,
    /// How often to spawn new air routes (in logistics ticks)
    pub spawn_interval_ticks: u32,
    /// Maximum simultaneous air routes in transit per side
    pub max_concurrent_routes: u32,
    /// Distance in meters from destination to trigger delivery
    #[serde(default = "default_air_delivery_distance")]
    pub delivery_distance: f64,
    /// How often to check route status (in seconds)
    #[serde(default = "default_air_check_interval")]
    pub check_interval_secs: u32,
    /// Supply % threshold below which a destination qualifies for an air run (0–100)
    #[serde(default = "default_air_supply_threshold")]
    pub supply_threshold: u8,
    /// How long a cargo flight may stay airborne before it is written off and
    /// its load returned to the origin.
    #[serde(default = "default_air_max_transit")]
    pub max_transit_minutes: u32,
}

pub(super) fn default_air_max_transit() -> u32 {
    60
}

pub(super) fn default_air_altitude() -> f64 {
    2500.0
}

pub(super) fn default_air_delivery_distance() -> f64 {
    2000.0
}

pub(super) fn default_air_check_interval() -> u32 {
    15
}

pub(super) fn default_air_supply_threshold() -> u8 {
    50
}

impl Default for AirLogisticsConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            aircraft_template: FxHashMap::default(),
            altitude_m: default_air_altitude(),
            speed_kph: 400.0,
            spawn_interval_ticks: 3,
            max_concurrent_routes: 6,
            delivery_distance: default_air_delivery_distance(),
            check_interval_secs: default_air_check_interval(),
            supply_threshold: default_air_supply_threshold(),
            max_transit_minutes: default_air_max_transit(),
        }
    }
}

/// Configuration for automated AI sea logistics routes
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct SeaLogisticsConfig {
    /// Enable sea logistics. When enabled, AI ships transport supplies from naval bases
    /// to carrier groups.
    pub enabled: bool,
    /// Ship group template name per side (must be a Ship group in the miz file)
    pub ship_template: FxHashMap<Side, String>,
    /// Ship speed in km/h
    pub speed_kph: f64,
    /// How often to spawn new sea routes (in logistics ticks)
    pub spawn_interval_ticks: u32,
    /// Maximum simultaneous sea routes in transit per side
    pub max_concurrent_routes: u32,
    /// Distance in meters from destination to trigger delivery
    #[serde(default = "default_sea_delivery_distance")]
    pub delivery_distance: f64,
    /// How often to check route status (in seconds)
    #[serde(default = "default_sea_check_interval")]
    pub check_interval_secs: u32,
    /// Supply % threshold below which a carrier group qualifies for a sea run (0–100)
    #[serde(default = "default_sea_supply_threshold")]
    pub supply_threshold: u8,
    /// How long a supply ship may stay at sea before it is written off and
    /// its load returned to the naval base.
    #[serde(default = "default_sea_max_transit")]
    pub max_transit_minutes: u32,
}

pub(super) fn default_sea_max_transit() -> u32 {
    180
}

pub(super) fn default_sea_delivery_distance() -> f64 {
    1500.0
}

pub(super) fn default_sea_check_interval() -> u32 {
    20
}

pub(super) fn default_sea_supply_threshold() -> u8 {
    50
}

impl Default for SeaLogisticsConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            ship_template: FxHashMap::default(),
            speed_kph: 30.0,
            spawn_interval_ticks: 3,
            max_concurrent_routes: 4,
            delivery_distance: default_sea_delivery_distance(),
            check_interval_secs: default_sea_check_interval(),
            supply_threshold: default_sea_supply_threshold(),
            max_transit_minutes: default_sea_max_transit(),
        }
    }
}
