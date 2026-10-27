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

//! Purchasable actions: AI flights, strikes, drones, nukes, artillery, ...

use super::*;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, schemars::JsonSchema)]
pub enum AiPlaneKind {
    FixedWing,
    Helicopter,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct AiPlaneCfg {
    pub kind: AiPlaneKind,
    pub duration: Option<u32>,
    pub template: String,
    pub altitude: f64,
    pub altitude_typ: AltType,
    pub speed: f64,
    #[serde(default)]
    pub freq: Option<i64>,
    /// TACAN channel (1-126) to broadcast so players can home in on this
    /// aircraft without needing its radio frequency. Unset = no TACAN beacon.
    #[serde(default)]
    pub tacan_channel: Option<u8>,
    /// TACAN channel band (X or Y). Required if `tacan_channel` is set.
    #[serde(default)]
    pub tacan_band: Option<TacanBand>,
    /// TACAN station identifier morse callsign (e.g. "TEX" for a Texaco tanker).
    /// Defaults to a truncated/uppercased version of the action's name if unset.
    #[serde(default)]
    pub tacan_callsign: Option<String>,
    /// DCS's numeric callsign family id for this aircraft (the same id shown in
    /// the Mission Editor's group "Callsign" dropdown for this aircraft category
    /// -- e.g. Tanker family: Texaco/Arco/Shell, AWACS family: Overlord/Magic/
    /// Wizard/Focus/Darkstar). Unset = DCS's own auto-generated name is used.
    /// We deliberately don't hardcode a name->id table here since it differs by
    /// aircraft category and has changed across DCS versions -- check the ME's
    /// own dropdown for the number that matches the name you want.
    #[serde(default)]
    pub callsign_id: Option<i64>,
    /// Flight number within the callsign family (e.g. 1 for "Texaco 1-1").
    /// Only meaningful alongside `callsign_id`. Defaults to 1 if unset.
    #[serde(default)]
    pub callsign_number: Option<u8>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct AwacsCfg {
    pub ewr: DeployableEwr,
    pub plane: AiPlaneCfg,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct BomberCfg {
    pub targets: u32,
    pub power: u32,
    // in meters radius around the target point
    pub accuracy: u32,
    pub plane: AiPlaneCfg,
}

/// What a `Strike` action goes after.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, schemars::JsonSchema,
)]
pub enum StrikeMission {
    /// Bomb an enemy objective's buildings (its logistics) -- an F-117 with
    /// GBU-27s. Aimed at an enemy objective.
    Precision,
    /// Crater an enemy airfield's runways. Aimed at an enemy airbase.
    Runway,
    /// Missile or bomb attack on the ships of an enemy carrier group.
    /// Aimed at the carrier group.
    AntiShip,
    /// Attack the enemy convoy or ground column nearest the marked point.
    Interdiction,
    /// Lay a line of bombs through the marked point.
    Carpet,
}

impl StrikeMission {
    /// Aimed at an enemy objective (else at a marked point).
    pub fn targets_objective(self) -> bool {
        matches!(self, Self::Precision | Self::Runway | Self::AntiShip)
    }

    pub fn label(self) -> &'static str {
        match self {
            Self::Precision => "precision strike",
            Self::Runway => "runway attack",
            Self::AntiShip => "anti-ship strike",
            Self::Interdiction => "interdiction",
            Self::Carpet => "carpet bombing",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct StrikeCfg {
    pub mission: StrikeMission,
    pub plane: AiPlaneCfg,
    /// DCS weapon flags the flight may use (14 guided bombs, 65536 anti-ship
    /// missiles, 2147485694 any bomb, ...). Absent = a sensible default for
    /// the mission.
    #[serde(default)]
    pub weapon_type: Option<u64>,
    /// Aim points (buildings, runways, ships) to attack. Default 2.
    #[serde(default = "default_strike_attacks")]
    pub attacks: u32,
    /// Length of a carpet-bombing line, metres. Default 1500.
    #[serde(default = "default_carpet_length")]
    pub carpet_length_m: f64,
}

pub(super) fn default_strike_attacks() -> u32 {
    2
}

pub(super) fn default_carpet_length() -> f64 {
    1_500.
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct DeployableCfg {
    pub name: String,
    pub plane: Option<AiPlaneCfg>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct DroneCfg {
    pub jtac: DeployableJtac,
    pub plane: AiPlaneCfg,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct NukeCfg {
    /// using a nuke reduces the cost of nukes for everyone by this
    /// factor. e.g. cost_scale: 4, with initial cost 1000. The first
    /// nuke would cost 1000 points. The next nuke would cost 250
    /// points. The next nuke would cost 62 points, and so on until a
    /// nuke costs 1 point at which point it stops scaling.
    pub cost_scale: u8,
    /// in Kilotons of TNT
    pub power: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct MoveCfg {
    /// max distance for troop moves in meters per unit cost
    pub troop: u32,
    /// max distance for deployable moves in meters per unit cost
    pub deployable: u32,
}

/// A player-called reinforcement convoy. It sets off from the friendly
/// objective nearest the one being reinforced, drives the road network there,
/// and on arrival puts that objective's destroyed garrison groups back
/// together, armour first. Each transporter lost on the way is a group that
/// doesn't arrive.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct ReinforceCfg {
    /// Convoy group template in the .miz: tank transporters (HX81 or SLT-50
    /// tractors with `CHAP_SLT50_Trailer`), any tractor-trailer pair from
    /// `warehouse.convoy.trailer_pairs`, or plain trucks. Tractors are hitched
    /// to their trailers when it sets off.
    pub template: String,
    /// Destroyed garrison groups rebuilt when the whole convoy arrives. Fewer
    /// if transporters were lost on the way -- in proportion to what survived.
    #[serde(default = "default_reinforce_groups")]
    pub groups: u32,
    /// The convoy starts at the nearest friendly objective within this many
    /// metres (straight line) of the one being reinforced.
    #[serde(default = "default_reinforce_max_source_m")]
    pub max_source_m: u32,
    /// Road speed in km/h. Transporters are slow.
    #[serde(default = "default_reinforce_speed_kph")]
    pub speed_kph: f64,
    /// Materiel taken from the source objective's stores for every group the
    /// convoy carries (0 = free). A source that can't spare it isn't used.
    #[serde(default)]
    pub materiel_per_group: u32,
    /// How close to the objective the convoy has to get to have arrived.
    #[serde(default = "default_reinforce_arrive_m")]
    pub arrive_m: u32,
}

pub(super) fn default_reinforce_groups() -> u32 {
    2
}

pub(super) fn default_reinforce_max_source_m() -> u32 {
    80_000
}

pub(super) fn default_reinforce_speed_kph() -> f64 {
    40.
}

pub(super) fn default_reinforce_arrive_m() -> u32 {
    1_000
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct NavalCruiseMissileCfg {
    /// number of missiles to fire per strike
    pub missiles_per_strike: u8,
    /// maximum range in meters from carrier to target
    pub max_range: u32,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// supply cost deducted from carrier group warehouse
    pub supply_cost: u32,
}

/// Per-unit-type range override entry inside ArtilleryCfg.
#[derive(Debug, Clone, Serialize, Deserialize, Default, schemars::JsonSchema)]
pub struct UnitRangeCfg {
    pub max_range_m: f64,
    #[serde(default)]
    pub min_range_m: f64,
}

/// Configuration for a player-callable artillery / indirect fire support action.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct ArtilleryCfg {
    /// Per-unit-type range config. Keys are DCS unit type names (e.g. "M142 HIMARS", "Scud_B").
    /// Any unit type not listed falls back to default_max_range_m / default_min_range_m.
    #[serde(default)]
    pub units: FxHashMap<String, UnitRangeCfg>,
    /// Fallback max range (metres) for unit types not in `units`. Default: 30000.
    #[serde(default = "default_arty_max")]
    pub default_max_range_m: f64,
    /// Fallback min range (metres) for unit types not in `units`. Default: 4000.
    #[serde(default = "default_arty_min")]
    pub default_min_range_m: f64,
    /// FireAtPoint scatter radius in metres. Default: 200.
    #[serde(default = "default_arty_radius")]
    pub radius_m: f64,
    /// Maximum number of groups that will fire simultaneously. Default: 3.
    #[serde(default = "default_arty_group_count")]
    pub max_groups: usize,
    /// Seconds a battery must wait after firing a player-requested mission
    /// before it can be tasked again. Batteries still reloading are skipped
    /// and the next one in range fires instead. Default: 60.
    #[serde(default = "default_arty_cooldown")]
    pub cooldown_secs: u32,
    /// Shoot and scoot: after a fire mission a battery relocates up to this
    /// many metres (DCS `counterbattaryRadius`, 0..500), so counter-battery
    /// fire and players hunting the muzzle flashes find an empty position.
    /// Unset = batteries stay put.
    #[serde(default)]
    pub shoot_and_scoot_m: Option<f64>,
}

pub(super) fn default_arty_max() -> f64 {
    30_000.0
}
pub(super) fn default_arty_min() -> f64 {
    4_000.0
}
pub(super) fn default_arty_radius() -> f64 {
    200.0
}
pub(super) fn default_arty_group_count() -> usize {
    3
}
pub(super) fn default_arty_cooldown() -> u32 {
    60
}

impl Default for ArtilleryCfg {
    fn default() -> Self {
        Self {
            units: FxHashMap::default(),
            default_max_range_m: default_arty_max(),
            default_min_range_m: default_arty_min(),
            radius_m: default_arty_radius(),
            max_groups: default_arty_group_count(),
            cooldown_secs: default_arty_cooldown(),
            shoot_and_scoot_m: None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub enum ActionKind {
    Tanker(AiPlaneCfg),
    Awacs(AwacsCfg),
    Bomber(BomberCfg),
    /// A flight that attacks with its own weapons -- precision, runway,
    /// anti-ship, interdiction or carpet bombing (`StrikeCfg::mission`).
    Strike(StrikeCfg),
    Fighters(AiPlaneCfg),
    Attackers(AiPlaneCfg),
    Sead(AiPlaneCfg),
    CruiseMissileSpawn(AiPlaneCfg),
    CruiseMissileWaypoint,
    Drone(DroneCfg),
    Nuke(NukeCfg),
    FighersWaypoint,
    AttackersWaypoint,
    SeadWaypoint,
    DroneWaypoint,
    TankerWaypoint,
    AwacsWaypoint,
    Paratrooper(DeployableCfg),
    Deployable(DeployableCfg),
    LogisticsRepair(AiPlaneCfg),
    LogisticsTransfer(AiPlaneCfg),
    Move(MoveCfg),
    Rtb,
    CarrierWaypoint,
    CarrierRepair,
    CarrierRespawn,
    NavalCruiseMissileStrike(NavalCruiseMissileCfg),
    /// Player-callable ground artillery / indirect fire support.
    Artillery(ArtilleryCfg),
    Recon(ReconCfg),
    /// Post a task (CAP, CAS, LOGISTICS, ...) to the coalition tasking
    /// board at a map mark. The task area is drawn on the F10 map for the
    /// whole coalition.
    AddTask(TaskCfg),
    /// Take a task back off the coalition tasking board.
    RemoveTask(RemoveTaskCfg),
    /// Send a heavy-transport convoy (tank transporters) by road from the
    /// nearest friendly objective to rebuild a damaged friendly objective's
    /// garrison. Kill the convoy and the reinforcements never arrive.
    Reinforce(ReinforceCfg),
    /// An AI heavy-lift helicopter flies from the nearest friendly field to
    /// a map mark, lands, sets up a Fat Cow around itself, stays for
    /// `duration_mins`, then packs it up and flies home. Flight settings
    /// come from `helo_insertion`.
    FatCow(FatCowActionCfg),
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct FatCowActionCfg {
    /// The Fat Cow deployable to set up (the last element of its path,
    /// e.g. "Fat Cow"). Its stock, pads and limit apply.
    pub deployable: String,
    /// AI helicopter group template to fly it. Default is the side's
    /// `helo_insertion.aircraft_template`.
    #[serde(default)]
    pub aircraft_template: Option<String>,
    /// How long it stays set up, in minutes.
    #[serde(default = "default_fat_cow_duration")]
    pub duration_mins: u32,
}

pub(super) fn default_fat_cow_duration() -> u32 {
    20
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub enum ActionGeoLimit {
    Unlimited,
    /// This action can only be run within `max` in meters of a friendly objective
    NearFriendlyObjective {
        max: u32,
    },
}

impl Default for ActionGeoLimit {
    fn default() -> Self {
        Self::Unlimited
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct Action {
    pub kind: ActionKind,
    pub cost: u32,
    pub penalty: Option<u32>,
    /// For actions that put an AI group on the map: how many of this
    /// action's groups a side may have up at once (a slot frees when one
    /// dies, lands or is removed). For actions without a group of their own
    /// (nuke, waypoints, carrier orders, fires, tasks): how many times a side
    /// may use it per server session.
    pub limit: Option<u32>,
    /// defines where this action is allowed to run
    #[serde(default)]
    pub geo_limit: ActionGeoLimit,
}
