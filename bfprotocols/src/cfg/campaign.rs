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

//! Objective defence, era limits, recon and tasks.

use super::*;

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct RearBaseProtectionCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Objective names protected for each side. A base is only protected
    /// while that side still owns it.
    #[serde(default)]
    pub bases: FxHashMap<Side, Vec<String>>,
    /// Protected radius around each base, metres. Default 27780 (15 nm).
    #[serde(default = "default_rear_base_radius")]
    pub radius_m: f64,
    /// Seconds an intruder may spend inside, in total, before it is
    /// removed. Default 60.
    #[serde(default = "default_rear_base_grace")]
    pub grace_secs: u32,
    /// Ignore aircraft at or below this altitude above ground, metres (a
    /// helicopter crossing at treetop height is not a threat to the
    /// airfield). 0 = protect at any height. Default 0.
    #[serde(default)]
    pub min_agl_m: f64,
}

pub(super) fn default_rear_base_radius() -> f64 {
    27_780.
}

pub(super) fn default_rear_base_grace() -> u32 {
    60
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct LastStandCfg {
    /// Seconds of countdown once a side is at or below `trigger_count` primary objectives.
    /// Primary objectives are Airbase, NavalBase, and Farp. Default: 3600.
    #[serde(default = "default_last_stand_countdown")]
    pub countdown_secs: u32,
    /// Number of primary objectives at or below which the last stand timer arms. Default: 1.
    #[serde(default = "default_last_stand_trigger")]
    pub trigger_count: usize,
}

pub(super) fn default_last_stand_countdown() -> u32 {
    3600
}

pub(super) fn default_last_stand_trigger() -> usize {
    1
}

impl Default for LastStandCfg {
    fn default() -> Self {
        Self {
            countdown_secs: default_last_stand_countdown(),
            trigger_count: default_last_stand_trigger(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct UnderAttackCfg {
    /// Cooldown in seconds between repeat under-attack notifications per objective. Default: 120.
    #[serde(default = "default_under_attack_cooldown")]
    pub cooldown_secs: u32,
}

pub(super) fn default_under_attack_cooldown() -> u32 {
    120
}

impl Default for UnderAttackCfg {
    fn default() -> Self {
        Self {
            cooldown_secs: default_under_attack_cooldown(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct CounterBatteryCfg {
    /// Cooldown in seconds between counter-battery reports for the same grid cell. Default: 60.
    #[serde(default = "default_cb_cooldown")]
    pub cooldown_secs: u32,
    /// Grid resolution in meters for report deduplication. Default: 2000.
    #[serde(default = "default_cb_grid")]
    pub grid_resolution_m: f64,
}

pub(super) fn default_cb_cooldown() -> u32 {
    60
}

pub(super) fn default_cb_grid() -> f64 {
    2000.0
}

impl Default for CounterBatteryCfg {
    fn default() -> Self {
        Self {
            cooldown_secs: default_cb_cooldown(),
            grid_resolution_m: default_cb_grid(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct EraCfg {
    /// Name of the currently active era. Must match a key in `eras`.
    pub current: String,
    /// Map of era name → list of allowed Vehicle types.
    /// Vehicles not listed in the active era's list are denied.
    pub eras: FxHashMap<String, Vec<Vehicle>>,
    /// Weapons banned in each era, matched case-insensitively as a
    /// substring of the DCS weapon type or display name ("AIM_120" bans
    /// every AMRAAM). A pilot who takes off carrying one gets
    /// `banned_weapon_land_secs` to land; firing one, or still flying when
    /// the time is up, destroys the aircraft and sends them to spectators.
    #[serde(default)]
    pub banned_weapons: FxHashMap<String, Vec<String>>,
    #[serde(default = "default_banned_weapon_land_secs")]
    pub banned_weapon_land_secs: u32,
}

pub(super) fn default_banned_weapon_land_secs() -> u32 {
    300
}

impl EraCfg {
    /// The active era's ban that `weapon` (a type or display name) falls
    /// under, if any.
    pub fn banned(&self, weapon: &str) -> Option<&str> {
        let w = weapon.to_lowercase();
        self.banned_weapons
            .get(self.current.as_str())?
            .iter()
            .find(|b| !b.is_empty() && w.contains(b.to_lowercase().as_str()))
            .map(|b| b.as_str())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct ReconCfg {
    /// AI plane template to spawn for the recon mission.
    pub plane: AiPlaneCfg,
    /// Radius in meters around the target position to scan for enemy units. Default: 10000.
    #[serde(default = "default_recon_radius")]
    pub scan_radius_m: f64,
    /// How long in seconds the recon plane loiters before RTB. Default: 300.
    #[serde(default = "default_recon_duration")]
    pub duration_secs: u32,
}

pub(super) fn default_recon_radius() -> f64 {
    10_000.0
}

pub(super) fn default_recon_duration() -> u32 {
    300
}

/// What a task type is posted against, and how the engine decides it is
/// done.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub enum TaskTarget {
    /// Posted at one of the posting player's F10 map marks. Nothing the
    /// engine can measure finishes it, so it stays up until somebody takes
    /// it off the board or its ttl runs out.
    Position,
    /// Posted against an objective the coalition does not own. Completes by
    /// itself the moment the posting coalition owns it.
    CaptureObjective,
    /// Posted against an objective the coalition owns. Completes by itself
    /// once that objective's supply and fuel are both back at `threshold`
    /// percent -- whether that was done by an AI helo supply run, a convoy,
    /// or players flying crates.
    SupplyObjective {
        #[serde(default = "default_supply_task_threshold")]
        threshold: u8,
    },
}

impl Default for TaskTarget {
    fn default() -> Self {
        Self::Position
    }
}

pub(super) fn default_supply_task_threshold() -> u8 {
    80
}

/// One selectable mission type on the coalition tasking board (see
/// `ActionKind::AddTask`).
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct TaskTypeCfg {
    /// Short label shown in the menu, in the map pin, and used as the task's
    /// type. e.g. "CAP", "CAS", "LOGISTICS".
    pub name: String,
    /// What the task is posted against. Position tasks pick one of the
    /// player's map marks; the objective kinds pick an objective from a list
    /// and complete on their own when the coalition has done the job.
    #[serde(default)]
    pub target: TaskTarget,
    /// Radius in meters of the task area circle drawn on the F10 map.
    #[serde(default = "default_task_radius")]
    pub radius_m: f64,
    /// RGBA colour of the task area, 0..1 per channel. If unset the task is
    /// drawn in the posting coalition's colour.
    #[serde(default)]
    pub color: Option<[f32; 4]>,
    /// Optional longer description shown in the F10 map pin under the label.
    #[serde(default)]
    pub description: Option<String>,
}

pub(super) fn default_task_radius() -> f64 {
    15_000.0
}

/// The coalition tasking board: players post tasks (CAP, CAS, LOGISTICS, ...)
/// at a map mark and every player on their side sees the task area and its
/// pin on the F10 map.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct TaskCfg {
    /// The task types players can post, in menu order.
    pub types: Vec<TaskTypeCfg>,
    /// Maximum number of open tasks a coalition may have at once. Default: 12.
    #[serde(default = "default_max_tasks")]
    pub max_per_side: usize,
    /// Tasks are removed automatically this many seconds after being posted.
    /// 0 means they stay until somebody removes them. Default: 7200 (2h).
    #[serde(default = "default_task_ttl")]
    pub ttl_secs: u32,
    /// Broadcast newly posted tasks over the GCI voice net (SRS) as well as
    /// on the F10 map. Default: true.
    #[serde(default = "default_true")]
    pub announce_gci: bool,
}

pub(super) fn default_max_tasks() -> usize {
    12
}

pub(super) fn default_task_ttl() -> u32 {
    7200
}

/// Removing a task from the coalition tasking board (see
/// `ActionKind::RemoveTask`).
#[derive(Debug, Clone, Default, Serialize, Deserialize, schemars::JsonSchema)]
pub struct RemoveTaskCfg {
    /// When true only the player who posted a task (or an admin) may remove
    /// it. When false anyone on the posting coalition may. Default: false.
    #[serde(default)]
    pub owner_only: bool,
}

/// Player-flown reconnaissance ("Recon Pass").
///
/// A player in a `UnitTag::Recon` aircraft starts a timed pass from the F10
/// menu while within `range_m` of an enemy objective. Holding station for
/// `dwell_secs` scans every enemy unit within `scan_radius_m` of that objective
/// (optionally gated by terrain line-of-sight from the aircraft) and feeds the
/// detections into the ELINT/SIGINT `IntelDatabase`, which renders the decaying
/// F10 map contacts.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct PlayerReconCfg {
    /// Max distance (m) aircraft -> objective to start and sustain a pass. Default 25000.
    #[serde(default = "default_player_recon_range")]
    pub range_m: f64,
    /// Station time (s) required to complete a pass. Default 120.
    #[serde(default = "default_player_recon_dwell")]
    pub dwell_secs: u32,
    /// Radius (m) around the objective scanned for enemy units. Default 12000.
    #[serde(default = "default_player_recon_scan_radius")]
    pub scan_radius_m: f64,
    /// Require terrain line-of-sight from the aircraft to each unit. Default true.
    #[serde(default = "default_true")]
    pub require_los: bool,
    /// Reveal contacts progressively at 25/50/75/100% dwell rather than only on
    /// completion. Default true.
    #[serde(default = "default_true")]
    pub progressive: bool,
    /// Points charged when a pass starts (refunded if it aborts). Default 0.
    #[serde(default)]
    pub cost: u32,
    /// Per-player cooldown (s) between passes. Default 300.
    #[serde(default = "default_player_recon_cooldown")]
    pub cooldown_secs: u32,
    /// Optional altitude ceiling (m MSL) for the aircraft during a pass.
    /// 0 disables the check. Default 0.
    #[serde(default)]
    pub max_altitude_m: f64,
}

pub(super) fn default_player_recon_range() -> f64 { 25_000.0 }
pub(super) fn default_player_recon_dwell() -> u32 { 120 }
pub(super) fn default_player_recon_scan_radius() -> f64 { 12_000.0 }
pub(super) fn default_player_recon_cooldown() -> u32 { 300 }

impl Default for PlayerReconCfg {
    fn default() -> Self {
        Self {
            range_m: default_player_recon_range(),
            dwell_secs: default_player_recon_dwell(),
            scan_radius_m: default_player_recon_scan_radius(),
            require_los: true,
            progressive: true,
            cost: 0,
            cooldown_secs: default_player_recon_cooldown(),
            max_altitude_m: 0.0,
        }
    }
}
