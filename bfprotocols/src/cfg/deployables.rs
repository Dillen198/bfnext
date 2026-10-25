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

//! Deployables: crates, deployable objectives/EWR/GCI/JTAC, troops.

use super::*;

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct Crate {
    /// The name of the crate in the menu
    pub name: String,
    /// The weight of the crate in kg
    pub weight: u32,
    /// The number of crates of this type required to build the deployable
    pub required: u32,
    /// The type of unit in the associated deployable group that will inherit
    /// this crate's position when the deployable is spawned. This is only
    /// needed for multi unit groups with distinct parts.
    pub pos_unit: Option<String>,
    /// the maximum height in meters agl that the user can drop this crate from
    pub max_drop_height_agl: u32,
    /// the maximum speed in m/s that the user can be going when they drop this
    /// cargo
    pub max_drop_speed: u32,
    /// Override the DCS static object type spawned for this crate (e.g.
    /// "iso_container"). If unset, the side's crate_template/c130_cargo_template/
    /// helo_cargo_template is used unmodified. Lets each crate have a distinct
    /// in-game model/weight without needing a separate .miz template per crate.
    #[serde(default)]
    pub dcs_type: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct DeployableObjective {
    pub pad_templates: Vec<String>,
    #[serde(default)]
    pub defenses_template: Option<String>,
    #[serde(default)]
    pub ammo_template: Option<String>,
    #[serde(default)]
    pub fuel_template: Option<String>,
    #[serde(default)]
    pub barracks_template: Option<String>,
    /// Makes this objective a Fat Cow: a temporary forward arming and
    /// refuelling point flown in by a heavy-lift helicopter. It is not
    /// supplied by any logistics hub, so it only ever holds the stock it was
    /// built with. bflib generates its templates (the pads named in
    /// `pad_templates`, unless the mission already has them, and the CG Fat
    /// Cow layout of fuel point, fire point, crew and guards), so the
    /// defenses/ammo/fuel/barracks templates are not used. It is gone when
    /// its two fuel points are destroyed.
    #[serde(default)]
    pub fat_cow: Option<FatCowCfg>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct FatCowCfg {
    /// Radius of the objective zone in meters. Regular FARPs use 2000; a
    /// Fat Cow is a few helicopters on a field and wants something small.
    #[serde(default = "default_fat_cow_radius")]
    pub radius: u32,
    /// Jet fuel the Fat Cow is built with, in kg
    pub jet_fuel: u32,
    /// Weapons the Fat Cow is built with, keyed by DCS warehouse item name
    /// (e.g. "weapons.missiles.AGM_114K"), value is the quantity
    #[serde(default)]
    pub weapons: FxHashMap<String, u32>,
    /// Unit type of the crew and guards. Default "Soldier M4 GRG" for blue,
    /// "Paratrooper AKS-74" for red.
    #[serde(default)]
    pub soldier: Option<String>,
    /// Unit type of the guard detail's MANPAD. Default "Soldier stinger" for
    /// blue, "SA-18 Igla-S manpad" for red.
    #[serde(default)]
    pub manpad: Option<String>,
}

pub(super) fn default_fat_cow_radius() -> u32 {
    500
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct DeployableEwr {
    /// range for likely detection (Meters)
    pub range: u32,
    // CR estokes: Actual radar simulation ...
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct DeployableGci {
    /// GCI radio channel (TACAN-style channel number set on the station)
    pub channel: i64,
    /// Max control radius (Meters)
    pub radius: u32,
}

/// Ground vehicle cargo configuration for a specific vehicle type (IFV/APC).
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct GroundVehicleCargo {
    /// Maximum infantry squads this vehicle can carry.
    pub troop_capacity: u8,
    /// Distance (m) within which troops must be to board. Default 50.
    #[serde(default = "default_board_radius_m")]
    pub board_radius_m: f64,
    /// Max vehicle speed (m/s) that still allows boarding. Default 1.0.
    #[serde(default = "default_board_speed_threshold_ms")]
    pub board_speed_threshold_ms: f64,
    /// Radius (m) within which dismounted troops spawn around the vehicle. Default 30.
    #[serde(default = "default_dismount_radius_m")]
    pub dismount_radius_m: f64,
    /// Allow boarding while the vehicle is moving. Default false.
    #[serde(default)]
    pub can_board_while_moving: bool,
}

pub(super) fn default_board_radius_m() -> f64 { 50.0 }
pub(super) fn default_board_speed_threshold_ms() -> f64 { 1.0 }
pub(super) fn default_dismount_radius_m() -> f64 { 30.0 }

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct DeployableJtac {
    /// jtac detection and lasing range (Meters)
    pub range: u32,
    /// if true line of sight checks are not required, the jtac will
    /// see every unit in range regardless of terrain or cover
    #[serde(default)]
    pub nolos: bool,
    /// default laser code for this JTAC (1111-1788), defaults to 1688
    #[serde(default = "default_laser_code")]
    pub default_laser_code: u16,
    /// optional callsign displayed instead of numeric group ID (e.g. "Axeman 11")
    #[serde(default)]
    pub name: Option<String>,
}

pub(super) fn default_laser_code() -> u16 {
    1688
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct JtacState {
    #[schemars(with = "Vec<UnitTag>")]
    pub filter: BitFlags<UnitTag>,
    pub priority: Vec<UnitTags>,
    pub autoshift: Option<usize>,
    pub ir_pointer: bool,
    pub code: u16,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub enum DeployableKind {
    Group { template: String },
    Objective(DeployableObjective),
}

impl DeployableKind {
    pub fn is_group(&self) -> bool {
        match self {
            Self::Group { .. } => true,
            Self::Objective(_) => false,
        }
    }

    pub fn is_objective(&self) -> bool {
        match self {
            Self::Objective(_) => true,
            Self::Group { .. } => false,
        }
    }
}

pub(super) fn default_deployable_kind() -> DeployableKind {
    DeployableKind::Group {
        template: "".into(),
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct Deployable {
    /// The full menu path of the deployable in the menu
    pub path: Vec<String>,
    /// The type of deployable
    #[serde(default = "default_deployable_kind")]
    pub kind: DeployableKind,
    /// How the deployable should persist across restarts
    pub persist: PersistTyp,
    /// How many instances are allowed at the same time
    pub limit: u32,
    /// How to deal with it when the max number of instances are deployed and
    /// a player wants to deploy a new instance
    pub limit_enforce: LimitEnforceTyp,
    /// What crates are required to build the deployable
    pub crates: Vec<Crate>,
    /// Can the damaged deployable be repaired, and if so, by which crate.
    pub repair_crate: Option<Crate>,
    /// How much does the damaged deployable cost to repair
    #[serde(default)]
    pub repair_cost: u32,
    /// How many points does this deployable cost (if any)
    #[serde(default)]
    pub cost: u32,
    /// Is this unit an early warning radar
    pub ewr: Option<DeployableEwr>,
    /// Is this unit a jtac
    pub jtac: Option<DeployableJtac>,
    /// Is this unit a MiG-29 GCI station
    #[serde(default)]
    pub gci: Option<DeployableGci>,
    #[serde(default)]
    #[serde(rename = "template")]
    pub deprecated_template: Option<String>,
    #[serde(default)]
    #[serde(rename = "logistics")]
    pub deprecated_logistics: Option<DeployableObjective>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct Troop {
    /// The name of the squad in the menu
    pub name: String,
    /// The name of the template used to spawn the group
    pub template: String,
    /// How the troops will persist
    pub persist: PersistTyp,
    /// Can the troops capture objectives?
    pub can_capture: bool,
    /// How many simultaneous instances of the group are allowed
    pub limit: u32,
    /// How to deal with it when the max number of instances are deployed and the user
    /// wants to deploy an additional instance
    pub limit_enforce: LimitEnforceTyp,
    /// How much weight does the group add to the carrier unit
    pub weight: u32,
    /// How many points does this troop cost
    #[serde(default)]
    pub cost: u32,
    /// Can laser designate and scout
    pub jtac: Option<DeployableJtac>,
}

/// Configuration for infantry that dismount from a destroyed vehicle
#[derive(Debug, Clone, Default, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct DismountSpec {
    /// DCS group template name per side. If a side has no entry no dismounts spawn for it.
    pub template: FxHashMap<Side, String>,
    /// Stop spawning this type's dismounts once the side has this many
    /// dismount groups alive (all vehicle types count). 0 = unlimited.
    #[serde(default)]
    pub max_concurrent: u32,
    /// Can these dismounts capture objectives?
    #[serde(default)]
    pub can_capture: bool,
    /// Chance (0.0-1.0) that a destroyed vehicle of this type has a squad
    /// that gets out. Defaults to 1.0, which was the only behaviour before:
    /// every kill, a full squad. That fits an APC; for a supply truck parked
    /// on an airfield it means clearing a base spawns a fresh squad per
    /// truck (Discord, Sept 28: "2 sets of super infantry just spawned in
    /// the middle of the runway").
    #[serde(default = "default_dismount_chance")]
    pub chance: f32,
}

pub(super) fn default_dismount_chance() -> f32 {
    1.0
}
