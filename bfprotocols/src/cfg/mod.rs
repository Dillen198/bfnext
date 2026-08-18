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
use dcso3::{coalition::Side, controller::AltType, country::Country, net::Ucid, String};
use enumflags2::{bitflags, BitFlags};
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use indexmap::IndexMap;
use netidx::path::Path as NetIdxPath;
use regex::Regex;
use serde_derive::{Deserialize, Serialize};
use std::{
    borrow::Borrow,
    fmt,
    fs::{self, File},
    io,
    sync::Arc,
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
};

mod example;

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, Eq, PartialOrd, Ord, Default, schemars::JsonSchema)]
pub struct Vehicle(pub String);

impl fmt::Display for Vehicle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> From<&'a str> for Vehicle {
    fn from(value: &'a str) -> Self {
        Self(value.into())
    }
}

impl From<String> for Vehicle {
    fn from(value: String) -> Self {
        Vehicle(value)
    }
}

impl Borrow<str> for Vehicle {
    fn borrow(&self) -> &str {
        &*self.0
    }
}

impl Vehicle {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub enum Rule {
    Whitelist { allowed: FxHashMap<Ucid, String> },
    Blacklist { denied: FxHashMap<Ucid, String> },
    AlwaysAllowed,
    NeverAllowed,
}

impl Default for Rule {
    fn default() -> Self {
        Self::AlwaysAllowed
    }
}

impl Rule {
    pub fn check(&self, ucid: &Ucid) -> bool {
        match self {
            Self::Whitelist { allowed } => allowed.contains_key(ucid),
            Self::Blacklist { denied } => !denied.contains_key(&ucid),
            Self::AlwaysAllowed => true,
            Self::NeverAllowed => false,
        }
    }

    pub fn blacklist(&mut self, ucid: Ucid, name: String) {
        match self {
            Self::Blacklist { denied } => {
                denied.insert(ucid, name);
            }
            Self::Whitelist { allowed } => {
                allowed.remove(&ucid);
            }
            Self::AlwaysAllowed => {
                let denied = FxHashMap::from_iter([(ucid, name)]);
                *self = Self::Blacklist { denied };
            }
            Self::NeverAllowed => (),
        }
    }

    pub fn whitelist(&mut self, ucid: Ucid, name: String) {
        match self {
            Self::Blacklist { denied } => {
                denied.remove(&ucid);
            }
            Self::Whitelist { allowed } => {
                allowed.insert(ucid, name);
            }
            Self::NeverAllowed => {
                let allowed = FxHashMap::from_iter([(ucid, name)]);
                *self = Self::Whitelist { allowed };
            }
            Self::AlwaysAllowed => (),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, schemars::JsonSchema)]
#[bitflags]
#[repr(u64)]
pub enum UnitTag {
    SAM,
    AAA,
    Armor,
    APC,
    Logistics,
    Infantry,
    EWR,
    Aircraft,
    Helicopter,
    LR,
    SR,
    MR,
    IRGuided,
    RadarGuided,
    OpticallyGuided,
    EngagesWeapons,
    Unguided,
    TrackRadar,
    SearchRadar,
    AuxRadarUnit,
    ControlUnit,
    Launcher,
    ATGM,
    Artillery,
    LightCannon,
    HeavyCannon,
    RPG,
    SmallArms,
    Unarmed,
    Invincible,
    Driveable,
    AWACS,
    Link16,
    Boat,
    ALCM,
    NavalSpawnPoint,
    CAP,
    HotStart,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
    schemars::JsonSchema,
)]
#[serde(from = "Vec<UnitTag>", into = "Vec<UnitTag>")]
pub struct UnitTags(#[schemars(with = "Vec<UnitTag>")] pub BitFlags<UnitTag>);

impl fmt::Display for UnitTags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let len = self.0.len();
        write!(f, "[")?;
        for (i, tag) in self.0.iter().enumerate() {
            if i < len - 1 {
                write!(f, "{tag:?}, ")?
            } else {
                write!(f, "{tag:?}")?
            }
        }
        write!(f, "]")
    }
}

impl Deref for UnitTags {
    type Target = BitFlags<UnitTag>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for UnitTags {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<UnitTag>> for UnitTags {
    fn from(value: Vec<UnitTag>) -> Self {
        Self(value.into_iter().collect())
    }
}

impl From<BitFlags<UnitTag>> for UnitTags {
    fn from(value: BitFlags<UnitTag>) -> Self {
        Self(value)
    }
}

impl Into<Vec<UnitTag>> for UnitTags {
    fn into(self) -> Vec<UnitTag> {
        self.0.into_iter().collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash, schemars::JsonSchema)]
pub enum LifeType {
    Standard,
    Intercept,
    Logistics,
    Attack,
    Recon,
}

impl fmt::Display for LifeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Standard => "standard",
            Self::Intercept => "intercept",
            Self::Logistics => "logistics",
            Self::Attack => "attack",
            Self::Recon => "recon",
        };
        write!(f, "{s}")
    }
}

impl LifeType {
    pub fn up(&self) -> Option<LifeType> {
        match self {
            LifeType::Recon => Some(LifeType::Logistics),
            LifeType::Logistics => Some(LifeType::Intercept),
            LifeType::Intercept => Some(LifeType::Attack),
            LifeType::Attack => Some(LifeType::Standard),
            LifeType::Standard => None,
        }
    }

    pub fn down(&self) -> Option<LifeType> {
        match self {
            LifeType::Recon => None,
            LifeType::Logistics => Some(LifeType::Recon),
            LifeType::Intercept => Some(LifeType::Logistics),
            LifeType::Attack => Some(LifeType::Intercept),
            LifeType::Standard => Some(LifeType::Attack),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub enum PersistTyp {
    /// The deployable persists until it is destroyed
    Forever,
    /// The deployable doesn't persist across restarts
    UntilRestart,
    /// The deployable persists for the specified number of
    /// real world seconds
    WallTime(f32),
    /// The deployable persists for the the specified number
    /// of server restart cycles
    Restarts(u32),
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, schemars::JsonSchema)]
pub enum LimitEnforceTyp {
    /// Handle the limit by removing the oldest instance of the deployable when
    /// a new one is unpacked. (lifo)
    #[default]
    DeleteOldest,
    /// Handle the limit by refusing to spawn new construction crates for
    /// the deployable
    DenyCrate,
}

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
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct DeployableEwr {
    /// range for likely detection (Meters)
    pub range: u32,
    // CR estokes: Actual radar simulation ...
}

/// Radar frequency band — determines aspect/RCS variation and stealth effectiveness.
/// Lower bands (VHF/UHF) have compressed aspect variation, making shaping less effective.
/// Higher bands (X/Ku) have sharp aspect dependence and are best countered by shaping.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Default, schemars::JsonSchema)]
pub enum RadarBand {
    Vhf,   // 30–300 MHz   — EWR (55G6, P-14, 1L13). Least affected by stealth shaping.
    Uhf,   // 300–3000 MHz — some older SAMs
    Lband, // 1–2 GHz      — FPS-117, some ship/SAM search radars
    Sband, // 2–4 GHz      — Patriot, most naval surface-search
    Cband, // 4–8 GHz      — some acquisition radars
    #[default]
    Xband, // 8–12 GHz     — most fighters, SAMs, NASAMS, Tor. Sharpest aspect dependence.
    Kuband,// 12–18 GHz    — some precision track/fire-control radars
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct AirborneEwr {
    /// Radar detection range in meters
    pub range: u32,
    /// Half-angle of the radar cone in degrees (None = omnidirectional, e.g. AWACS/ships)
    /// A fighter with a nose radar would be ~60, an AWACS would be None
    #[serde(default)]
    pub aspect_half_angle: Option<u16>,
    /// True if this radar uses a pulse-Doppler waveform.
    /// Enables Doppler notch exploitation — beam-aspect targets at low closure
    /// rates get a significant detection probability penalty.
    #[serde(default)]
    pub pulse_doppler: bool,
    /// Whether this radar can effectively look down into ground clutter.
    /// False for older radars (MiG-21, F-4, early Hawk) — they are nearly blind
    /// against low-altitude targets regardless of altitude factor tuning.
    #[serde(default = "default_look_down_capable")]
    pub look_down_capable: bool,
    /// Susceptibility to chaff (0.0 = immune, 1.0 = fully defeated).
    /// Modern PD radars ≈ 0.1, older non-PD ≈ 0.8–0.9.
    /// Used when a chaff mechanic is active (stored for future use).
    #[serde(default = "default_chaff_susceptibility")]
    pub chaff_susceptibility: f32,
    /// Susceptibility to stand-off jamming/ECM (0.0 = immune, 1.0 = fully defeated).
    /// Modern AESA/LPI ≈ 0.1, older analog radars ≈ 0.7–0.9.
    /// Used when a jamming mechanic is active (stored for future use).
    #[serde(default = "default_ecm_susceptibility")]
    pub ecm_susceptibility: f32,
    /// Scan interval in seconds — how often the radar refreshes a track.
    /// Slow-rotating EWRs (~10s) update less frequently than AESA fighters (~1s).
    #[serde(default = "default_scan_interval_secs")]
    pub scan_interval_secs: u32,
    /// Frequency band — affects aspect/RCS variation and stealth penetration.
    #[serde(default)]
    pub frequency_band: RadarBand,
}

fn default_look_down_capable() -> bool { true }
fn default_chaff_susceptibility() -> f32 { 0.3 }
fn default_ecm_susceptibility() -> f32 { 0.4 }
fn default_scan_interval_secs() -> u32 { 2 }

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub enum EwrMode {
    /// Original EWR implementation with immediate track updates
    Original,
    /// EWR with configurable delay on track updates
    Delayed,
}

impl Default for EwrMode {
    fn default() -> Self {
        Self::Original
    }
}

/// Sensor category used by the IADN fusion layer to apply appropriate detection physics.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, schemars::JsonSchema)]
pub enum SensorType {
    /// Ground-based omnidirectional EWR / deployed radar.
    GroundEwr,
    /// Naval surface-search radar (omnidirectional, some look-down).
    NavalRadar,
    /// SAM search/acquisition radar (directional, limited look-down).
    SamSearchRadar,
    /// Player/AI fighter with nose radar (directional forward cone).
    AirborneFighter,
    /// AWACS — look-down/shoot-down capable, pulse-Doppler.
    Awacs,
}

/// Advanced probabilistic radar physics configuration.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct RadarPhysicsCfg {
    /// RCS factor for Hot aspect (nose-on). Default 1.0.
    #[serde(default = "default_rcs_hot")]
    pub rcs_hot: f32,
    /// RCS factor for Flank aspect. Default 0.7.
    #[serde(default = "default_rcs_flank")]
    pub rcs_flank: f32,
    /// RCS factor for Beam aspect (perpendicular). Default 0.35.
    #[serde(default = "default_rcs_beam")]
    pub rcs_beam: f32,
    /// RCS factor for Cold aspect (tail-on). Default 0.15.
    #[serde(default = "default_rcs_cold")]
    pub rcs_cold: f32,
    /// Detection probability attenuation in the Doppler notch window. Default 0.05.
    #[serde(default = "default_notch_attenuation")]
    pub notch_attenuation: f32,
    /// Closure rate (m/s) below which beam-aspect targets are notching. Default 50.
    #[serde(default = "default_notch_closure_threshold_ms")]
    pub notch_closure_threshold_ms: f32,
    /// AGL (m) below which ground EWR probability degrades due to ground clutter. Default 300.
    #[serde(default = "default_ground_radar_low_alt_threshold_m")]
    pub ground_radar_low_alt_threshold_m: f32,
    /// Exponential smoothing alpha for track position/velocity. Default 0.3.
    #[serde(default = "default_track_smoothing_alpha")]
    pub track_smoothing_alpha: f32,
    /// AWACS look-down bonus multiplier vs ground EWR at low altitude. Default 1.5.
    #[serde(default = "default_awacs_look_down_bonus")]
    pub awacs_look_down_bonus: f32,
}

impl Default for RadarPhysicsCfg {
    fn default() -> Self {
        Self {
            rcs_hot: default_rcs_hot(),
            rcs_flank: default_rcs_flank(),
            rcs_beam: default_rcs_beam(),
            rcs_cold: default_rcs_cold(),
            notch_attenuation: default_notch_attenuation(),
            notch_closure_threshold_ms: default_notch_closure_threshold_ms(),
            ground_radar_low_alt_threshold_m: default_ground_radar_low_alt_threshold_m(),
            track_smoothing_alpha: default_track_smoothing_alpha(),
            awacs_look_down_bonus: default_awacs_look_down_bonus(),
        }
    }
}

fn default_rcs_hot() -> f32 { 1.0 }
fn default_rcs_flank() -> f32 { 0.7 }
fn default_rcs_beam() -> f32 { 0.35 }
fn default_rcs_cold() -> f32 { 0.15 }
fn default_notch_attenuation() -> f32 { 0.05 }
fn default_notch_closure_threshold_ms() -> f32 { 50.0 }
fn default_ground_radar_low_alt_threshold_m() -> f32 { 300.0 }
fn default_track_smoothing_alpha() -> f32 { 0.3 }
fn default_awacs_look_down_bonus() -> f32 { 1.5 }

/// Integrated Air Defence Network configuration.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct IadnConfig {
    /// Radius (m) within which two detections are fused into one track. Default 3000.
    #[serde(default = "default_track_association_radius_m")]
    pub track_association_radius_m: f64,
    /// Minimum SNR (0–1) required to register a detection. Default 0.5.
    #[serde(default = "default_detection_snr_threshold")]
    pub detection_snr_threshold: f32,
    /// Seconds with no detection before a track is marked stale. Default 60.
    #[serde(default = "default_track_stale_secs")]
    pub track_stale_secs: u32,
    /// Seconds before a stale track is dropped entirely. Default 120.
    #[serde(default = "default_track_drop_secs")]
    pub track_drop_secs: u32,
    /// Enable automatic SAM target cueing from the fused picture.
    #[serde(default = "default_sam_cue_enabled")]
    pub sam_cue_enabled: bool,
    /// Minimum fused track confidence (0–1) required before a SAM engages. Default 0.4.
    #[serde(default = "default_sam_cue_confidence_threshold")]
    pub sam_cue_confidence_threshold: f32,
    /// DCS weapon type names (Weapon:getTypeName()) treated as anti-radiation
    /// missiles for SAM HARM-defense purposes -- e.g. "AGM_88C", "Kh25MPU",
    /// "Kh58Ushke", "ALARM". Not derived from DCS's own guidance metadata
    /// since dcso3 doesn't parse Weapon:getDesc() yet; configure explicitly.
    #[serde(default)]
    pub anti_radiation_weapons: FxHashSet<std::string::String>,
    /// Distance (m) from a tracked inbound ARM within which a SAM site is
    /// considered threatened and forced dark. Default 20000 (~typical ARM
    /// engagement envelope, deliberately generous -- this is a "might be
    /// targeting us" heuristic, not missile guidance physics).
    #[serde(default = "default_harm_defense_radius_m")]
    pub harm_defense_radius_m: f64,
    /// How long (s) a threatened SAM site's radar stays forced dark after
    /// an ARM is detected nearby. Default 20 -- long enough for the missile
    /// to lose lock/fly past, short enough not to blind the site for long.
    #[serde(default = "default_harm_defense_cooldown_secs")]
    pub harm_defense_cooldown_secs: u32,
}

impl Default for IadnConfig {
    fn default() -> Self {
        Self {
            track_association_radius_m: default_track_association_radius_m(),
            detection_snr_threshold: default_detection_snr_threshold(),
            track_stale_secs: default_track_stale_secs(),
            track_drop_secs: default_track_drop_secs(),
            sam_cue_enabled: default_sam_cue_enabled(),
            sam_cue_confidence_threshold: default_sam_cue_confidence_threshold(),
            anti_radiation_weapons: FxHashSet::default(),
            harm_defense_radius_m: default_harm_defense_radius_m(),
            harm_defense_cooldown_secs: default_harm_defense_cooldown_secs(),
        }
    }
}

fn default_track_association_radius_m() -> f64 { 3000.0 }
fn default_detection_snr_threshold() -> f32 { 0.5 }
fn default_track_stale_secs() -> u32 { 60 }
fn default_track_drop_secs() -> u32 { 120 }
fn default_sam_cue_enabled() -> bool { true }
fn default_sam_cue_confidence_threshold() -> f32 { 0.4 }
fn default_harm_defense_radius_m() -> f64 { 20_000.0 }
fn default_harm_defense_cooldown_secs() -> u32 { 20 }

/// ELINT/SIGINT intelligence system configuration.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct ElintConfig {
    /// Radius (m) within which units are clustered into one intel contact. Default 500.
    #[serde(default = "default_contact_cluster_radius_m")]
    pub contact_cluster_radius_m: f64,
    /// Confidence half-life (s) for recon-flight intel. Default 600.
    #[serde(default = "default_half_life_recon")]
    pub half_life_recon_secs: u32,
    /// Confidence half-life (s) for special-forces intel. Default 1800.
    #[serde(default = "default_half_life_sf")]
    pub half_life_sf_secs: u32,
    /// Confidence half-life (s) for AWACS-derived intel. Default 300.
    #[serde(default = "default_half_life_awacs")]
    pub half_life_awacs_secs: u32,
    /// Confidence half-life (s) for EWR-fused intel. Default 180.
    #[serde(default = "default_half_life_ewr")]
    pub half_life_ewr_secs: u32,
    /// Confidence below which a contact is deleted. Default 0.05.
    #[serde(default = "default_confidence_delete_threshold")]
    pub confidence_delete_threshold: f32,
    /// Max intel contacts stored per side. Default 200.
    #[serde(default = "default_max_contacts_per_side")]
    pub max_contacts_per_side: usize,
    /// Show unit class label on F10 map markers. Default true.
    #[serde(default = "default_show_unit_class")]
    pub show_unit_class: bool,
    /// Show confidence percentage on F10 map markers. Default true.
    #[serde(default = "default_show_confidence_on_map")]
    pub show_confidence_on_map: bool,
}

impl Default for ElintConfig {
    fn default() -> Self {
        Self {
            contact_cluster_radius_m: default_contact_cluster_radius_m(),
            half_life_recon_secs: default_half_life_recon(),
            half_life_sf_secs: default_half_life_sf(),
            half_life_awacs_secs: default_half_life_awacs(),
            half_life_ewr_secs: default_half_life_ewr(),
            confidence_delete_threshold: default_confidence_delete_threshold(),
            max_contacts_per_side: default_max_contacts_per_side(),
            show_unit_class: default_show_unit_class(),
            show_confidence_on_map: default_show_confidence_on_map(),
        }
    }
}

fn default_contact_cluster_radius_m() -> f64 { 500.0 }
fn default_half_life_recon() -> u32 { 600 }
fn default_half_life_sf() -> u32 { 1800 }
fn default_half_life_awacs() -> u32 { 300 }
fn default_half_life_ewr() -> u32 { 180 }
fn default_confidence_delete_threshold() -> f32 { 0.05 }
fn default_max_contacts_per_side() -> usize { 200 }
fn default_show_unit_class() -> bool { true }
fn default_show_confidence_on_map() -> bool { true }

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

fn default_board_radius_m() -> f64 { 50.0 }
fn default_board_speed_threshold_ms() -> f64 { 1.0 }
fn default_dismount_radius_m() -> f64 { 30.0 }

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

fn default_laser_code() -> u16 {
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

fn default_deployable_kind() -> DeployableKind {
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
    /// Max simultaneous dismount groups spawned from this vehicle type. 0 = unlimited.
    #[serde(default)]
    pub max_concurrent: u32,
    /// Can these dismounts capture objectives?
    #[serde(default)]
    pub can_capture: bool,
}

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

fn default_c130_vehicle_limit() -> u32 {
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

fn default_csar_enabled() -> bool {
    true
}

fn default_csar_pickup_radius() -> u32 {
    100
}

fn default_csar_board_radius() -> u32 {
    20
}

fn default_csar_capture_timer() -> u32 {
    15
}

fn default_csar_enemy_capture_radius() -> u32 {
    50
}

fn default_csar_renotify_interval() -> u32 {
    5
}

fn default_csar_smoke_cooldown() -> u32 {
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

fn default_c130_spawn_delay() -> u32 {
    1
}

fn default_c130_max_spawn() -> u32 {
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

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct WarehouseConfig {
    /// Logistics hub max supply stock as a multiple of the delivery amount
    pub hub_max: u32,
    /// Airbase max supply stock as a multiple of the delivery amount
    pub airbase_max: u32,
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
}

/// Effectively unlimited for gameplay purposes, but small enough that
/// percentage/ratio math elsewhere (stored as f32 / capacity as f32) stays
/// well-behaved instead of flirting with u32::MAX overflow.
pub const UNLIMITED_CAPACITY: u32 = 1_000_000;

impl WarehouseConfig {
    pub fn capacity(&self, hub: bool, qty: u32) -> u32 {
        if hub {
            qty * self.hub_max
        } else {
            qty * self.airbase_max
        }
    }

    /// Like `capacity`, but returns UNLIMITED_CAPACITY when `unlimited` is
    /// true -- either because the objective's own UNLIMITED_SUPPLY trigger
    /// zone property is set, or its name is listed in `unlimited_objectives`
    /// here. Only meaningful for items the side already has access to
    /// (qty > 0 upstream) -- this never grants a type the side doesn't
    /// otherwise produce.
    pub fn capacity_for(&self, obj_name: &str, unlimited: bool, hub: bool, qty: u32) -> u32 {
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
}

fn default_convoy_delivery_distance() -> f64 {
    500.0
}

fn default_convoy_check_interval() -> u32 {
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
}

fn default_air_altitude() -> f64 {
    2500.0
}

fn default_air_delivery_distance() -> f64 {
    2000.0
}

fn default_air_check_interval() -> u32 {
    15
}

fn default_air_supply_threshold() -> u8 {
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
}

fn default_sea_delivery_distance() -> f64 {
    1500.0
}

fn default_sea_check_interval() -> u32 {
    20
}

fn default_sea_supply_threshold() -> u8 {
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
        }
    }
}

fn default_tk_window() -> u32 {
    24
}

fn default_true() -> bool {
    true
}

fn default_convoy_interdiction() -> u32 {
    10
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct PointsCfg {
    /// Bonus issued to new players when they register
    pub new_player_join: u32,
    /// Points awarded for each air kill
    pub air_kill: u32,
    /// Base points awared for each ground kill
    pub ground_kill: u32,
    /// Bonus points awarded to heavy sam kills
    pub lr_sam_bonus: u32,
    /// Points awarded for repairing base logistics
    pub logistics_repair: u32,
    /// Points awarded for logistics transfers
    pub logistics_transfer: u32,
    /// Points awarded for base capture
    pub capture: u32,
    /// How many hours before previous team kills are forgotten for
    /// the purposes of computing the penalty of a team kill.
    #[serde(default = "default_tk_window")]
    pub tk_window: u32,
    /// If provisional is true then points earned in a sortie are only
    /// committed to the player's points balance when they land at a
    /// friendly objective
    #[serde(default)]
    pub provisional: bool,
    /// If strict is true then the player cannot take off when their
    /// loadout or airframe costs more points than they have. They
    /// will be deleted on takeoff, and no points or lives will be
    /// deducted. If struct is false then the player's points will go
    /// negative if they take off with an airframe/loadout that
    /// exceeds their current balance.
    #[serde(default)]
    pub strict: bool,
    /// How many points does it cost to slot in a given airframe. This
    /// need not cover all airframes on the server, and the default is 0.
    #[serde(default)]
    pub airframe_cost: FxHashMap<Vehicle, u32>,
    /// How many points does it cost to load a given weapon. This need
    /// not cover all weapons, and the default is zero.
    #[serde(default)]
    pub weapon_cost: FxHashMap<String, u32>,
    /// How many points do connected players automatically gain per
    /// time interval. This is a pair of the number of points with the
    /// interval in seconds. The number of points CAN be negative, the
    /// interval must be positive. The default is (0, 0)
    #[serde(default)]
    pub periodic_point_gain: (i32, u32),
    /// Whether to award points for kills. If false, no points are awarded
    /// for air or ground kills. Default is true.
    #[serde(default = "default_true")]
    pub award_kill_points: bool,
    /// Points awarded for destroying an enemy supply convoy unit. Default: 10.
    #[serde(default = "default_convoy_interdiction")]
    pub convoy_interdiction_points: u32,
    /// Kill streak bonus thresholds. Each entry is (minimum_streak, bonus_multiplier).
    /// e.g. [(3, 1.5), (5, 2.0)] means after 3 kills in a single sortie the base points
    /// are multiplied by 1.5, and after 5 kills by 2.0. Streak resets on death.
    /// Default: empty (no bonus).
    #[serde(default)]
    pub kill_streak_bonuses: Vec<(u8, f64)>,
}

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

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct NavalCruiseMissileCfg {
    /// number of missiles to fire per strike
    pub missiles_per_strike: u8,
    /// maximum range in meters from carrier to target
    pub max_range: u32,
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
}

fn default_arty_max() -> f64 { 30_000.0 }
fn default_arty_min() -> f64 { 4_000.0 }
fn default_arty_radius() -> f64 { 200.0 }
fn default_arty_group_count() -> usize { 3 }

impl Default for ArtilleryCfg {
    fn default() -> Self {
        Self {
            units: FxHashMap::default(),
            default_max_range_m: default_arty_max(),
            default_min_range_m: default_arty_min(),
            radius_m: default_arty_radius(),
            max_groups: default_arty_group_count(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub enum ActionKind {
    Tanker(AiPlaneCfg),
    Awacs(AwacsCfg),
    Bomber(BomberCfg),
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
    pub limit: Option<u32>,
    /// defines where this action is allowed to run
    #[serde(default)]
    pub geo_limit: ActionGeoLimit,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct Rules {
    /// who can use actions
    pub actions: Rule,
    /// who gets the cargo menu
    pub cargo: Rule,
    /// who gets the troops menu
    pub troops: Rule,
    /// who gets the jtac menu
    pub jtac: Rule,
    /// who can access the jtac slots
    pub ca: Rule,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(try_from = "String", into = "String")]
pub struct NameFilter(#[schemars(with = "std::string::String")] Regex);

impl TryFrom<String> for NameFilter {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self> {
        Ok(Self(Regex::new(&value)?))
    }
}

impl TryFrom<&str> for NameFilter {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self> {
        Ok(Self(Regex::new(value)?))
    }
}

impl Into<String> for NameFilter {
    fn into(self) -> String {
        self.0.as_str().into()
    }
}

impl NameFilter {
    /// Check if a name is allowed
    pub fn check(&self, name: &str) -> bool {
        self.0.is_match(name)
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, schemars::JsonSchema)]
pub enum VictoryCondition {
    /// Victory is triggered when the specified percentage of the map
    /// is owned by a given team, or is neutral. Every objective is
    /// considered equally in this calculation. Must be between 0 and 1
    MapOwned { fraction: f64 },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, schemars::JsonSchema)]
pub struct AutoResetOnVictory {
    /// What victory condition triggers an automatic reset
    pub condition: VictoryCondition,
    /// How long, in seconds, must the condition hold before reset is
    /// tiggered
    pub delay: u32,
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

fn default_last_stand_countdown() -> u32 {
    3600
}

fn default_last_stand_trigger() -> usize {
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

fn default_under_attack_cooldown() -> u32 {
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

fn default_cb_cooldown() -> u32 {
    60
}

fn default_cb_grid() -> f64 {
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

fn default_recon_radius() -> f64 {
    10_000.0
}

fn default_recon_duration() -> u32 {
    300
}

fn default_msgs_per_second() -> usize {
    5
}

fn default_cull_after() -> u32 {
    1800
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

fn default_frontline_on_change_only() -> bool {
    true
}

fn default_frontline_samples() -> usize {
    100
}

fn default_territory_zone_alpha() -> f32 {
    0.15  // 15% opacity for subtle territory shading
}

fn default_frontline_max_marks() -> usize {
    200
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct FrontLineConfig {
    /// Enable territory zone drawing on F10 map
    pub enabled: bool,
    /// Update only when objectives change ownership (recommended for best performance)
    #[serde(default = "default_frontline_on_change_only")]
    pub update_on_objective_change_only: bool,
    /// Number of sample points for Voronoi grid calculation (higher = finer detail but slower)
    /// Range: 50-200 recommended
    #[serde(default = "default_frontline_samples")]
    pub samples_per_boundary: usize,
    /// Maximum number of F10 map marks to draw for territory zones.
    /// Fewer marks = better server and client performance. Default: 200.
    /// The draw step is derived automatically: step = ceil(sqrt(grid² / max_marks)).
    #[serde(default = "default_frontline_max_marks")]
    pub max_marks: usize,
    /// Transparency for filled territory zones (0.0-1.0, where 1.0 is opaque)
    /// Recommended: 0.1-0.3 for subtle shading
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
    /// Time in seconds to complete carrier repair (default: 600 = 10 minutes)
    #[serde(default = "default_carrier_repair_time")]
    pub repair_time: u32,
    /// Carrier group definitions - maps template names to display names
    /// If not specified, carriers are detected by BCARRIER/RCARRIER/NCARRIER prefix
    #[serde(default)]
    pub groups: Vec<CarrierGroupCfg>,
}

fn default_carrier_spawn_repositioning_speed() -> f64 {
    100.0
}

fn default_carrier_repair_time() -> u32 {
    600
}

fn default_sam_capture_radius() -> f64 {
    609.6 // 2000 ft
}

fn default_red_country() -> Country {
    Country::CJTF_RED
}

fn default_blue_country() -> Country {
    Country::CJTF_BLUE
}

/// A plain 2-D position in DCS LO coordinates that serializes as `{"x":…,"y":…}`.
/// Used instead of `nalgebra::Vector2` in config structs because nalgebra serializes
/// as a matrix array, not a named-field map.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, schemars::JsonSchema)]
pub struct Pos2d {
    /// North-south (DCS x)
    pub x: f64,
    /// East-west (DCS y)
    pub y: f64,
}

/// A single unit definition for an inline SAM site group.
/// Positions are absolute DCS LO coordinates (x = north-south, y = east-west).
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct SpecialSamUnitCfg {
    /// DCS unit type string (e.g. "SNR_75V", "ZSU-23-4 Shilka")
    #[serde(rename = "type")]
    pub typ: String,
    /// Absolute position in DCS LO space
    pub pos: Pos2d,
    /// Heading in radians
    pub heading: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct SpecialSamSiteCfg {
    pub name: String,
    /// Centroid of the site in DCS LO space.
    pub pos: Pos2d,
    /// Starting coalition
    pub coalition: Side,
    /// Inline unit definitions for Red coalition. If non-empty, takes precedence over red_template.
    #[serde(default)]
    pub red_units: Vec<SpecialSamUnitCfg>,
    /// Inline unit definitions for Blue coalition. If non-empty, takes precedence over blue_template.
    #[serde(default)]
    pub blue_units: Vec<SpecialSamUnitCfg>,
    /// DCS country for Red inline units (ignored when using red_template).
    #[serde(default = "default_red_country")]
    pub red_country: Country,
    /// DCS country for Blue inline units (ignored when using blue_template).
    #[serde(default = "default_blue_country")]
    pub blue_country: Country,
    /// Template group name in the .miz for the Red coalition (legacy; use red_units instead).
    pub red_template: Option<String>,
    /// Template group name in the .miz for the Blue coalition (legacy; use blue_units instead).
    pub blue_template: Option<String>,
    /// Crate type that repairs the site; if None the site is not repairable
    #[serde(default)]
    pub repair_crate: Option<Crate>,
}

/// Weather effects on logistics and operations
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct WeatherEffectsCfg {
    /// Convoy speed multiplier in rain (0.0-1.0)
    #[serde(default = "default_rain_speed_mult")]
    pub rain_speed_multiplier: f64,
    /// Convoy speed multiplier in thunderstorm (0.0-1.0)
    #[serde(default = "default_storm_speed_mult")]
    pub thunderstorm_speed_multiplier: f64,
    /// Convoy speed multiplier in snow (0.0-1.0)
    #[serde(default = "default_snow_speed_mult")]
    pub snow_speed_multiplier: f64,
    /// Minimum visibility (meters) for fixed-wing takeoff
    #[serde(default = "default_min_vis_fixed_wing")]
    pub min_visibility_fixed_wing: f64,
    /// Disable helicopter operations in thunderstorms
    #[serde(default = "default_true")]
    pub no_helo_in_thunderstorm: bool,
    /// EWR detection range multiplier in bad weather (0.0-1.0)
    #[serde(default = "default_ewr_weather_mult")]
    pub ewr_weather_range_multiplier: f64,
}

fn default_rain_speed_mult() -> f64 { 0.8 }
fn default_storm_speed_mult() -> f64 { 0.6 }
fn default_snow_speed_mult() -> f64 { 0.5 }
fn default_min_vis_fixed_wing() -> f64 { 800.0 }
fn default_ewr_weather_mult() -> f64 { 0.85 }

impl Default for WeatherEffectsCfg {
    fn default() -> Self {
        Self {
            rain_speed_multiplier: default_rain_speed_mult(),
            thunderstorm_speed_multiplier: default_storm_speed_mult(),
            snow_speed_multiplier: default_snow_speed_mult(),
            min_visibility_fixed_wing: default_min_vis_fixed_wing(),
            no_helo_in_thunderstorm: true,
            ewr_weather_range_multiplier: default_ewr_weather_mult(),
        }
    }
}

/// Time-of-day effects on gameplay
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct TimeOfDayEffectsCfg {
    /// Points multiplier for kills during night hours (e.g. 1.5 = 50% bonus)
    #[serde(default = "default_night_kill_bonus")]
    pub night_kill_bonus: f64,
    /// Hour when night begins (0-23, local mission time)
    #[serde(default = "default_night_start")]
    pub night_start_hour: u8,
    /// Hour when night ends (0-23, local mission time)
    #[serde(default = "default_night_end")]
    pub night_end_hour: u8,
}

fn default_night_kill_bonus() -> f64 { 1.5 }
fn default_night_start() -> u8 { 22 }
fn default_night_end() -> u8 { 6 }

impl Default for TimeOfDayEffectsCfg {
    fn default() -> Self {
        Self {
            night_kill_bonus: default_night_kill_bonus(),
            night_start_hour: default_night_start(),
            night_end_hour: default_night_end(),
        }
    }
}

/// Dynamic campaign events configuration
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct CampaignEventsCfg {
    /// Enable the dynamic events system
    #[serde(default)]
    pub enabled: bool,
    /// Minimum seconds between event checks
    #[serde(default = "default_event_check_interval")]
    pub check_interval_secs: u32,
    /// Probability (0.0-1.0) of spawning an event per check
    #[serde(default = "default_event_probability")]
    pub event_probability: f64,
    /// Maximum number of concurrent active events
    #[serde(default = "default_max_events")]
    pub max_concurrent_events: u32,
    /// Points awarded for successful VIP extraction
    #[serde(default = "default_vip_reward")]
    pub vip_reward_points: i32,
    /// Points per civilian evacuated
    #[serde(default = "default_evac_reward")]
    pub evacuation_reward_per_civilian: i32,
    /// Enable artillery barrage events
    #[serde(default = "default_true")]
    pub barrage_enabled: bool,
    /// Duration in seconds of a barrage event
    #[serde(default = "default_barrage_duration")]
    pub barrage_duration_secs: u32,
    /// FireAtPoint scatter radius (metres) used by the auto-barrage event. Default: 500.
    #[serde(default = "default_barrage_radius_m")]
    pub barrage_radius_m: f64,
    /// Maximum number of groups the auto-barrage simultaneously orders. Default: 5.
    #[serde(default = "default_barrage_max_groups")]
    pub barrage_max_groups: usize,
    /// Enable convoy ambush events
    #[serde(default = "default_true")]
    pub ambush_enabled: bool,
    /// Duration in seconds before an ambush expires without contact
    #[serde(default = "default_ambush_duration")]
    pub ambush_duration_secs: u32,
    /// Enable automatic enemy CAP intercept spawns when players are deep in enemy territory
    #[serde(default)]
    pub enemy_cap_enabled: bool,
    /// Template name prefix for enemy CAP aircraft (e.g. "RCAP" for red, "BCAP" for blue).
    /// The system appends the side prefix automatically.
    #[serde(default = "default_cap_template_red")]
    pub cap_template_red: String,
    #[serde(default = "default_cap_template_blue")]
    pub cap_template_blue: String,
    /// How long (seconds) a CAP orbit lasts before despawning. Default: 600.
    #[serde(default = "default_cap_duration")]
    pub cap_duration_secs: u32,
    /// Distance from an objective (metres) within which a CAP orbit is placed. Default: 15000.
    #[serde(default = "default_cap_orbit_radius")]
    pub cap_orbit_radius_m: f64,
    /// Probability that a CAP event is spawned on each slow-events check (0.0–1.0). Default: 0.35.
    #[serde(default = "default_cap_probability")]
    pub cap_probability: f64,
    /// Seconds a troop must continuously occupy an objective before it is captured.
    /// Set to 0 to disable momentum (instant capture). Default: 60.
    #[serde(default = "default_capture_time")]
    pub capture_time_secs: u32,
    /// Fraction (0.0–1.0) of an objective's defending units that must be destroyed
    /// before it becomes capturable, in addition to logi == 0. Default: 0.0 (disabled).
    /// Example: 0.30 requires 30% of defenders to be killed first.
    #[serde(default)]
    pub capture_min_unit_pct_destroyed: f64,
    /// Distance (metres) from an enemy-owned objective within which an in-air player aircraft
    /// is considered a threat and triggers a reactive CAP spawn. Default: 60000 (60 km).
    #[serde(default = "default_cap_trigger_radius")]
    pub cap_trigger_radius_m: f64,
    /// Maximum number of CAP events active simultaneously across both sides. Default: 3.
    #[serde(default = "default_cap_max_concurrent")]
    pub cap_max_concurrent: usize,
    /// Maximum reactive EnemyCap events active for a single side at the same time.
    /// Prevents one side from consuming all concurrent CAP slots. Default: 2.
    #[serde(default = "default_cap_max_per_side")]
    pub cap_max_per_side: usize,
    /// Minimum number of enemy aircraft within `cap_trigger_radius_m` of an objective
    /// before a reactive CAP spawn is triggered. A lone scout will not trigger CAP;
    /// a real incursion of 2+ aircraft will. Default: 2.
    #[serde(default = "default_cap_min_threat")]
    pub cap_min_threat_count: u32,
    /// Cooldown (seconds) before a new reactive CAP can spawn for an objective after
    /// the previous reactive CAP was destroyed mid-mission. Prevents instant respawn
    /// of CAP and gives the attacker a window. Default: 120 (2 minutes).
    #[serde(default = "default_cap_respawn_cooldown")]
    pub cap_respawn_cooldown_secs: u64,
    /// How often (seconds) to call world.removeJunk to clean up debris. Default: 300 (5 min).
    /// Set to 0 to disable.
    #[serde(default = "default_junk_removal_interval")]
    pub junk_removal_interval_secs: u32,
    /// Sphere radius (metres) passed to world.removeJunk. Default: 500_000 (covers most maps).
    #[serde(default = "default_junk_removal_radius")]
    pub junk_removal_radius_m: f64,

}

fn default_cap_respawn_cooldown() -> u64 { 120 }

fn default_event_check_interval() -> u32 { 300 }
fn default_event_probability() -> f64 { 0.15 }
fn default_max_events() -> u32 { 3 }
fn default_vip_reward() -> i32 { 300 }
fn default_evac_reward() -> i32 { 50 }
fn default_barrage_duration() -> u32 { 300 }
fn default_ambush_duration() -> u32 { 600 }
fn default_cap_template_red() -> String { "RCAP".into() }
fn default_cap_template_blue() -> String { "BCAP".into() }
fn default_cap_duration() -> u32 { 600 }
fn default_cap_orbit_radius() -> f64 { 15_000.0 }
fn default_cap_probability() -> f64 { 0.35 }
fn default_capture_time() -> u32 { 60 }
fn default_barrage_radius_m() -> f64 { 500.0 }
fn default_barrage_max_groups() -> usize { 5 }
fn default_cap_trigger_radius() -> f64 { 90_000.0 }
fn default_cap_max_concurrent() -> usize { 3 }
fn default_cap_max_per_side() -> usize { 2 }
fn default_cap_min_threat() -> u32 { 2 }
fn default_junk_removal_interval() -> u32 { 300 }
fn default_junk_removal_radius() -> f64 { 500_000.0 }

impl Default for CampaignEventsCfg {
    fn default() -> Self {
        Self {
            enabled: false,
            check_interval_secs: default_event_check_interval(),
            event_probability: default_event_probability(),
            max_concurrent_events: default_max_events(),
            vip_reward_points: default_vip_reward(),
            evacuation_reward_per_civilian: default_evac_reward(),
            barrage_enabled: true,
            barrage_duration_secs: default_barrage_duration(),
            ambush_enabled: true,
            ambush_duration_secs: default_ambush_duration(),
            enemy_cap_enabled: false,
            cap_template_red: default_cap_template_red(),
            cap_template_blue: default_cap_template_blue(),
            cap_duration_secs: default_cap_duration(),
            cap_orbit_radius_m: default_cap_orbit_radius(),
            cap_probability: default_cap_probability(),
            capture_time_secs: default_capture_time(),
            capture_min_unit_pct_destroyed: 0.0,
            barrage_radius_m: default_barrage_radius_m(),
            barrage_max_groups: default_barrage_max_groups(),
            cap_trigger_radius_m: default_cap_trigger_radius(),
            cap_max_concurrent: default_cap_max_concurrent(),
            cap_max_per_side: default_cap_max_per_side(),
            cap_min_threat_count: default_cap_min_threat(),
            cap_respawn_cooldown_secs: default_cap_respawn_cooldown(),
            junk_removal_interval_secs: default_junk_removal_interval(),
            junk_removal_radius_m: default_junk_removal_radius(),
        }
    }
}

/// Pilot experience and progression configuration
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct PilotExperienceCfg {
    /// Enable the pilot experience system
    #[serde(default)]
    pub enabled: bool,
    /// XP awarded per air kill
    #[serde(default = "default_xp_air_kill")]
    pub xp_per_air_kill: u32,
    /// XP awarded per ground kill
    #[serde(default = "default_xp_ground_kill")]
    pub xp_per_ground_kill: u32,
    /// XP awarded per successful sortie (land safely)
    #[serde(default = "default_xp_sortie")]
    pub xp_per_sortie: u32,
    /// XP awarded per cargo delivery
    #[serde(default = "default_xp_delivery")]
    pub xp_per_delivery: u32,
    /// XP thresholds for rank progression
    #[serde(default = "default_rank_thresholds")]
    pub rank_thresholds: Vec<(u32, String)>,
}

fn default_xp_air_kill() -> u32 { 100 }
fn default_xp_ground_kill() -> u32 { 50 }
fn default_xp_sortie() -> u32 { 25 }
fn default_xp_delivery() -> u32 { 75 }
fn default_rank_thresholds() -> Vec<(u32, String)> {
    vec![
        (0, "Cadet".into()),
        (500, "2nd Lieutenant".into()),
        (1500, "1st Lieutenant".into()),
        (3000, "Captain".into()),
        (6000, "Major".into()),
        (10000, "Lieutenant Colonel".into()),
        (20000, "Colonel".into()),
    ]
}

impl Default for PilotExperienceCfg {
    fn default() -> Self {
        Self {
            enabled: false,
            xp_per_air_kill: default_xp_air_kill(),
            xp_per_ground_kill: default_xp_ground_kill(),
            xp_per_sortie: default_xp_sortie(),
            xp_per_delivery: default_xp_delivery(),
            rank_thresholds: default_rank_thresholds(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct Cfg {
    #[serde(default)]
    #[schemars(with = "Option<std::string::String>")]
    pub netidx_base: Option<NetIdxPath>,
    /// if specified, automatically reset the server state and record
    /// a victory in the stats when the condition is met.
    #[serde(default)]
    pub auto_reset: Option<AutoResetOnVictory>,
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
    /// The maximum number of messages, including markup, we will push to dcs
    /// per second.
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
    /// (meters).
    pub artillery_mission_range: u32,
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
    /// Front line drawing configuration
    #[serde(default)]
    pub frontline: Option<FrontLineConfig>,
    /// Factory production configuration
    #[serde(default)]
    pub factory: Option<FactoryCfg>,
    /// Carrier group configuration
    #[serde(default)]
    pub carrier: Option<CarrierCfg>,
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
    /// Mercy timer: when a side reaches `trigger_count` or fewer primary objectives,
    /// starts a countdown. On expiry the losing side's victory is triggered.
    #[serde(default)]
    pub last_stand: Option<LastStandCfg>,
    /// Under-attack notifications: send a panel message to a coalition when an
    /// objective they own becomes threatened by enemy units.
    #[serde(default)]
    pub under_attack: Option<UnderAttackCfg>,
    /// Counter-battery: when an enemy artillery/launcher unit fires, report its
    /// approximate position to the opposing coalition.
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
}

fn default_supply_alert_threshold() -> u8 {
    20
}

fn default_supply_auto_convoy_delay() -> u32 {
    300
}

fn default_treasury_income_period() -> u32 {
    300
}
fn default_treasury_income_amount() -> i64 {
    500
}
fn default_obj_fund_period() -> u32 {
    120
}
fn default_obj_fund_max() -> i32 {
    200
}
fn default_commander_period() -> u32 {
    60
}
fn default_holding_bonus() -> i32 {
    5
}
fn default_objective_start_points() -> i32 {
    500
}
fn default_barrage_cost() -> i64 {
    150
}
fn default_ambush_cost() -> i64 {
    100
}
fn default_cap_cost() -> i64 {
    250
}
fn default_cap_min_friendly_pilots() -> u32 {
    2
}
fn default_cap_cooldown_secs() -> u32 {
    300
}


#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct SmartCommanderCfg {
    /// Seconds between commander ticks (holding bonuses, objective funding).
    #[serde(default = "default_commander_period")]
    pub tick_period_secs: u32,
    /// Starting treasury for each side on a fresh map. Default: 0.
    #[serde(default)]
    pub treasury_start: i64,
    /// Points added to each side's treasury every `treasury_income_period_secs`.
    #[serde(default = "default_treasury_income_amount")]
    pub treasury_income_amount: i64,
    /// Interval in seconds between treasury income deposits. Default: 300.
    #[serde(default = "default_treasury_income_period")]
    pub treasury_income_period_secs: u32,
    /// Max points injected per objective per funding pass (scales with damage). Default: 200.
    #[serde(default = "default_obj_fund_max")]
    pub objective_fund_max_per_tick: i32,
    /// Seconds between objective funding passes. Default: 120.
    #[serde(default = "default_obj_fund_period")]
    pub objective_fund_period_secs: u32,
    /// Points per owned objective awarded to each connected player per tick. Default: 5.
    #[serde(default = "default_holding_bonus")]
    pub holding_bonus_per_objective: i32,
    /// Points seeded into each owned objective on a fresh map init. Default: 500.
    #[serde(default = "default_objective_start_points")]
    pub objective_start_points: i32,
    /// Treasury cost per side to order an artillery barrage. Default: 150.
    #[serde(default = "default_barrage_cost")]
    pub barrage_cost: i64,
    /// Treasury cost per side to set a convoy ambush. Default: 100.
    #[serde(default = "default_ambush_cost")]
    pub ambush_cost: i64,
    /// Treasury cost per side to scramble an enemy CAP. Default: 250.
    #[serde(default = "default_cap_cost")]
    pub cap_cost: i64,
    /// Minimum air-superiority gap (enemy_in_air − friendly_in_air) required
    /// before the commander dispatches a CAP flight to balance the skies.
    /// 1 = dispatch as soon as enemy has even one more aircraft airborne;
    /// 2+ = only dispatch when significantly outnumbered. Default: 1.
    #[serde(default = "default_cap_min_friendly_pilots")]
    pub cap_min_friendly_pilots: u32,
    /// Seconds the commander waits after a dispatched CAP flight ends before
    /// spawning another one. Default: 300 (5 min).
    #[serde(default = "default_cap_cooldown_secs")]
    pub cap_cooldown_secs: u32,
}

impl Cfg {
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
                        let file = File::create(&path)
                            .map_err(|e| anyhow!("could not create default config {}", e))?;
                        serde_json::to_writer_pretty(file, &Cfg::default())
                            .map_err(|e| anyhow!("could not write default config {}", e))?;
                    }
                    e => {
                        return Err(anyhow!("error opening config file {:?}", e));
                    }
                },
            }
        };
        let mut cfg: Self = serde_json::from_reader(file)
            .map_err(|e| anyhow!("failed to decode cfg file {:?}, {:?}", path, e))?;
        for (_, actions) in &mut cfg.actions {
            actions.sort_by(|name0, _, name1, _| name0.cmp(name1));
        }
        // translate deployables to the new format
        let mut has_deprecated = false;
        for (_, deps) in cfg.deployables.iter_mut() {
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
            fs::write(path, serde_json::to_string_pretty(&cfg)?)?
        }
        Ok(cfg)
    }

    pub fn save(&self, miz_state_path: &Path) -> Result<()> {
        let mut path = Self::path(miz_state_path);
        path.set_extension("bak");
        let fd = File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&path)
            .with_context(|| format_compact!("opening {:?}", path))?;
        serde_json::to_writer_pretty(fd, self).context("serializing cfg")?;
        fs::rename(&path, Self::path(miz_state_path)).context("moving new file into place")?;
        Ok(())
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
            Some(typ) => match self.default_lives.get(&typ) {
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
