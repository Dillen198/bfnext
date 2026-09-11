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
use dcso3::{coalition::Side, controller::{AltType, TacanBand}, country::Country, net::Ucid, String};
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
    /// Carries jamming/ECM equipment (dedicated EW aircraft or
    /// self-protection pods) -- degrades nearby radar detection
    /// probability for the IADN jamming mechanic.
    Jammer,
    /// Aircraft type is cleared to run a player "Recon Pass" (see
    /// `Cfg::player_recon`). Only meaningful on Aircraft/Helicopter entries.
    Recon,
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
    /// Explicit sensor classification, overriding the aspect_half_angle-based
    /// guess radar_donors() otherwise makes for ground/naval units (Some =
    /// SamSearchRadar, None = NavalRadar). That guess is wrong for any real
    /// ground SAM search radar configured omnidirectional (aspect_half_angle:
    /// null) -- which is the physically correct setting for most SAM search
    /// radars (Hawk, S-300, Patriot, etc. all rotate/scan 360°), but without
    /// this override it gets misclassified NavalRadar and silently excluded
    /// from every IADN SAM-specific mechanic (cueing, EMCON, HARM defense,
    /// layered radar). Set explicitly to SamSearchRadar for any ground SAM
    /// search radar unit type; leave unset (defaults to the aspect_half_angle
    /// guess) for genuinely non-SAM entries (ship radars, standalone EWRs).
    #[serde(default)]
    pub sensor_type_override: Option<SensorType>,
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
    /// Modern AESA/LPI ≈ 0.1, older analog radars ≈ 0.7–0.9. Scales the
    /// IADN jamming mechanic's detection-probability penalty when a
    /// UnitTag::Jammer unit is within iadn.jamming_range_m.
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
    /// Once cued hot, a SAM site stays hot for at least this long (s) even
    /// if the cue drops, instead of flickering dark the instant a target
    /// loses fused-track quality for a moment. Default 15.
    #[serde(default = "default_min_hot_dwell_secs")]
    pub min_hot_dwell_secs: u32,
    /// On first detecting a qualifying cue, a site waits a random 0..=N
    /// second delay (rolled per site, per activation) before actually going
    /// hot, so a cluster of networked sites doesn't snap to Auto in
    /// lockstep -- that's an obvious tell that it's scripted. Default 4.
    #[serde(default = "default_reaction_delay_max_secs")]
    pub reaction_delay_max_secs: u32,
    /// Fraction (0–1) of a SAM's search-radar range within which its
    /// separate tracking/engagement radar (units tagged UnitTag::TrackRadar)
    /// is allowed to power up. Search radar (UnitTag::SearchRadar) follows
    /// the site's normal hot/dark state; tracking radar only lights up once
    /// a target is this close, mirroring how real layered SAM systems
    /// (SA-10, Patriot) keep the higher-exposure engagement radar dark until
    /// close to actually firing. Default 0.5. Sites with no unit tagged
    /// TrackRadar/SearchRadar in their group are unaffected -- this only
    /// adds a second control layer on top of the group AlarmState, it
    /// doesn't replace it.
    #[serde(default = "default_track_radar_range_fraction")]
    pub track_radar_range_fraction: f32,
    /// Enable the jamming/ECM detection-degradation mechanic.
    #[serde(default = "default_jamming_enabled")]
    pub jamming_enabled: bool,
    /// Distance (m) from a UnitTag::Jammer-tagged unit within which nearby
    /// radar donors have their detection probability degraded. Default
    /// 40000 -- stand-off/self-protection jamming has a wide effective
    /// radius compared to a SAM's own detection range.
    #[serde(default = "default_jamming_range_m")]
    pub jamming_range_m: f64,
    /// Detection-probability multiplier applied at full jamming exposure
    /// (donor right on top of a jammer), scaled by the donor's own
    /// ecm_susceptibility and linearly by distance out to jamming_range_m.
    /// Default 0.5 (halves detection probability at worst case for a fully
    /// susceptible donor -- this degrades detection, it doesn't grant
    /// stealth).
    #[serde(default = "default_jamming_detection_penalty")]
    pub jamming_detection_penalty: f32,
    /// Keep a command-center-networked SAM site fighting after it loses its
    /// own search/acquisition radar: as long as it still has a live tracking
    /// radar or launcher, the fused network picture (EWR + AWACS + other SAM
    /// search radars) decides when it comes up hot, and its remaining radar is
    /// held on so DCS's own AI can acquire and engage within that sensor's
    /// sector. A site with no live command-center link just goes inert as
    /// before. Default true.
    #[serde(default = "default_sam_offboard_cue_enabled")]
    pub sam_offboard_cue_enabled: bool,
    /// Engagement range (m) assumed for a blinded SAM site being cued purely
    /// off-board -- there's no live search radar to read a range from. A fused
    /// hostile inside this radius brings the site up hot. Default 60000.
    #[serde(default = "default_sam_offboard_cue_range_m")]
    pub sam_offboard_cue_range_m: f64,
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
            min_hot_dwell_secs: default_min_hot_dwell_secs(),
            reaction_delay_max_secs: default_reaction_delay_max_secs(),
            track_radar_range_fraction: default_track_radar_range_fraction(),
            jamming_enabled: default_jamming_enabled(),
            jamming_range_m: default_jamming_range_m(),
            jamming_detection_penalty: default_jamming_detection_penalty(),
            sam_offboard_cue_enabled: default_sam_offboard_cue_enabled(),
            sam_offboard_cue_range_m: default_sam_offboard_cue_range_m(),
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
fn default_min_hot_dwell_secs() -> u32 { 15 }
fn default_reaction_delay_max_secs() -> u32 { 4 }
fn default_track_radar_range_fraction() -> f32 { 0.5 }
fn default_jamming_enabled() -> bool { true }
fn default_jamming_range_m() -> f64 { 40_000.0 }
fn default_jamming_detection_penalty() -> f32 { 0.5 }
fn default_sam_offboard_cue_enabled() -> bool { true }
fn default_sam_offboard_cue_range_m() -> f64 { 60_000.0 }

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
    /// Confidence half-life (s) for JTAC eyes-on intel. Refreshed to full
    /// confidence every tick a JTAC still has the contact, so this only
    /// governs how long it lingers on the map after the JTAC loses it.
    /// Default 3600 (1 hour).
    #[serde(default = "default_half_life_jtac")]
    pub half_life_jtac_secs: u32,
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
            half_life_jtac_secs: default_half_life_jtac(),
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
fn default_half_life_jtac() -> u32 { 3600 }
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

/// Syncs the mission file's ground-level weather (and optionally date/time)
/// with real-world conditions before each scheduled restart. DCS has no API
/// to change an already-running mission's weather or clock, so this rewrites
/// the .miz on disk before bflib triggers the process restart; the new
/// conditions take effect the next time the mission loads, same approach as
/// the dcs-real-weather project.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct LiveWeatherConfig {
    /// latitude to fetch live weather for
    pub lat: f64,
    /// longitude to fetch live weather for
    pub lon: f64,
    /// also set the mission's date/start_time to the real-world local date
    /// and time of the machine running the server
    #[serde(default)]
    pub sync_time: bool,
    /// checkwxapi.com API key. When set together with `metar_station`, the
    /// mission's *surface* layer (wind, temperature, QNH, cloud cover) is taken
    /// from that station's real decoded METAR instead of the open-meteo model.
    /// The winds-aloft layers still come from open-meteo (METAR has no upper
    /// air), and a failed/empty METAR fetch falls back to open-meteo for
    /// everything.
    #[serde(default)]
    pub checkwx_api_key: Option<String>,
    /// ICAO of the station whose METAR drives the surface layer -- pick the
    /// real-world field nearest the operating area (e.g. "LTAG" Incirlik,
    /// "OSDI" Damascus, "OLBA" Beirut, "OJAI" Amman). Only used when
    /// `checkwx_api_key` is also set.
    #[serde(default)]
    pub metar_station: Option<String>,
}

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
}

fn default_route_block_margin() -> f64 {
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

fn default_salvage_percent() -> u8 {
    25
}

fn default_captured_min_health() -> u8 {
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

fn default_materiel_hub_capacity() -> u32 {
    6
}

fn default_materiel_airbase_capacity() -> u32 {
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

fn default_production_floor() -> u8 {
    35
}

fn default_production_ceiling() -> u8 {
    125
}

fn default_weight_airbase() -> f64 {
    1.0
}

fn default_weight_logistics() -> f64 {
    3.0
}

fn default_weight_factory() -> f64 {
    4.0
}

fn default_weight_command_center() -> f64 {
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

fn default_hub_reserve_percent() -> u8 {
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
        let reserve = (capacity as f32 * (self.hub_reserve_percent.min(100) as f32 / 100.)) as u32;
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
}

fn default_convoy_max_transit() -> u32 {
    90
}

fn default_dispatch_cooldown() -> u32 {
    2
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
            max_transit_minutes: default_convoy_max_transit(),
            dispatch_cooldown_ticks: default_dispatch_cooldown(),
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

fn default_air_max_transit() -> u32 {
    60
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

fn default_sea_max_transit() -> u32 {
    180
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
            max_transit_minutes: default_sea_max_transit(),
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
    /// Post a task (CAP, CAS, LOGISTICS, ...) to the coalition tasking
    /// board at a map mark. The task area is drawn on the F10 map for the
    /// whole coalition.
    AddTask(TaskCfg),
    /// Take a task back off the coalition tasking board.
    RemoveTask(RemoveTaskCfg),
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

fn default_supply_task_threshold() -> u8 {
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

fn default_task_radius() -> f64 {
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

fn default_max_tasks() -> usize {
    12
}

fn default_task_ttl() -> u32 {
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

fn default_player_recon_range() -> f64 { 25_000.0 }
fn default_player_recon_dwell() -> u32 { 120 }
fn default_player_recon_scan_radius() -> f64 { 12_000.0 }
fn default_player_recon_cooldown() -> u32 { 300 }

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

fn default_msgs_per_second() -> usize {
    5
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
    /// Upper bound on the number of line segments drawn. Fewer = lighter on the
    /// server and clients. Default: 200. The segment list is thinned to fit.
    #[serde(default = "default_frontline_max_marks")]
    pub max_marks: usize,
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

fn default_carrier_spawn_repositioning_speed() -> f64 {
    100.0
}

fn default_carrier_repair_time() -> u32 {
    1800
}

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

fn default_navaid_separation_nm() -> f64 {
    150.0
}

fn default_blue_tacan() -> NavRange {
    NavRange { lo: 2, hi: 62 }
}

fn default_red_tacan() -> NavRange {
    NavRange { lo: 63, hi: 125 }
}

fn default_navaid_band() -> TacanBand {
    TacanBand::Y
}

fn default_ndb_khz() -> NavRange {
    NavRange { lo: 200, hi: 1400 }
}

fn default_carrier_link4_mhz() -> f64 {
    336.0
}

fn default_western_carriers() -> Vec<String> {
    ["CVN_", "Stennis", "Forrestal"]
        .into_iter()
        .map(String::from)
        .collect()
}

fn default_helo_carriers() -> Vec<String> {
    ["LHA_Tarawa", "Tarawa", "LHA", "LHD"]
        .into_iter()
        .map(String::from)
        .collect()
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

fn default_repair_supply_cost() -> u8 {
    5
}

fn default_deploy_supply_cost() -> u8 {
    3
}

fn default_artillery_min_range() -> u32 {
    1500
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
    /// Barometric altitude (metres MSL) AI CAP flights hold and intercept at.
    /// Default 7000 (~FL230) -- without this DCS parks them low and slow.
    #[serde(default = "default_cap_altitude_m")]
    pub cap_altitude_m: f64,
    /// True airspeed (m/s) for AI CAP orbit and intercept legs. Default 230
    /// (~450 kt).
    #[serde(default = "default_cap_speed_ms")]
    pub cap_speed_ms: f64,
    /// How far (metres) a CAP flight may push from the objective it is
    /// defending toward a detected threat. Keeps interceptors leaning forward
    /// without chasing contacts deep across the front line. Default 60000.
    #[serde(default = "default_cap_max_push_m")]
    pub cap_max_push_m: f64,
    /// Radius (metres) of a CAP flight's engage-in-zone order around its
    /// station. This is the leash: the flight will chase an air contact this
    /// far from where it is stationed and no further. Keep it modest -- a large
    /// value plus `cap_max_push_m` lets interceptors wander into enemy SAM
    /// belts and get swatted, which reads in-game as CAP that spawns, dies, and
    /// respawns on a loop. Default 45000.
    #[serde(default = "default_cap_engage_radius_m")]
    pub cap_engage_radius_m: f64,
    /// If a CAP flight's side has painted no workable threat for this many
    /// seconds, it is sent home (RTB) and despawned early rather than loitering
    /// out its full `cap_duration_secs`. Keeps the air picture matched to the
    /// actual threat instead of accumulating idle flights. Default 240.
    #[serde(default = "default_cap_idle_rtb_secs")]
    pub cap_idle_rtb_secs: u32,
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
    /// Seconds after a base changes hands (captured, or fell to Neutral) during
    /// which no new capture timer can start against it. Gives the new owner a
    /// window to defend / reinforce and stops a zone with capture troops from
    /// both sides in it from flipping on a loop. Default 120.
    #[serde(default = "default_capture_cooldown_secs")]
    pub capture_cooldown_secs: u32,
    /// Fraction (0.0–1.0) of the new owner's *non-SAM* combat garrison (AAA,
    /// infantry, armour) brought back the moment a base is captured -- spent
    /// AAA/infantry-first. The rest, and all of the SAMs, rebuild gradually
    /// through normal auto-repair (or must be delivered by crate). 1.0 = full
    /// non-SAM garrison; lower = a freshly-taken base is a light target you
    /// have to build up. Default 0.25.
    #[serde(default = "default_capture_garrison_revive_fraction")]
    pub capture_garrison_revive_fraction: f64,
    /// Also bring back a share of the new owner's SAM garrison (short/medium/
    /// long-range) on capture. Default false -- a freshly-taken base has no
    /// working SAM cover until it's rebuilt or resupplied, which is both more
    /// realistic and stops "fly over a wrecked base, get killed by an
    /// instant Pantsir".
    #[serde(default)]
    pub capture_garrison_revive_include_sam: bool,
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
    /// Cooldown (seconds) after a side's reactive CAP wave ends -- shot down OR
    /// flown its full duration and RTB'd -- before that side may scramble
    /// another. Gives the attacking side a real window between waves.
    /// Default: 1800 (30 minutes).
    #[serde(default = "default_cap_respawn_cooldown")]
    pub cap_respawn_cooldown_secs: u64,
    /// When true, reactive CAP triggers on the actual count of enemy fixed-wing
    /// PLAYERS airborne on the attacking side, not on what the defending side's
    /// radar network has painted. Use this if a side (usually Red) never
    /// scrambles because its EWR / SAM search radars are down, dark (EMCON), or
    /// have coverage gaps. Costs the fog-of-war realism -- CAP will vector
    /// toward players the defender can't "see". Default: false.
    #[serde(default)]
    pub cap_trigger_on_known_players: bool,
    /// Air-balance CAP: also scramble for whichever side has this many FEWER
    /// fixed-wing players airborne than the other -- so if Blue puts up 4 and
    /// Red 1, Red gets a CAP even without a specific detected incursion. Uses
    /// actual player counts (radar-independent). 0 disables. Default: 2.
    #[serde(default = "default_cap_balance_gap")]
    pub cap_balance_gap: u32,
    /// How often (seconds) to call world.removeJunk to clean up debris. Default: 300 (5 min).
    /// Set to 0 to disable.
    #[serde(default = "default_junk_removal_interval")]
    pub junk_removal_interval_secs: u32,
    /// Sphere radius (metres) passed to world.removeJunk. Default: 500_000 (covers most maps).
    #[serde(default = "default_junk_removal_radius")]
    pub junk_removal_radius_m: f64,

}

fn default_cap_respawn_cooldown() -> u64 { 1800 }
fn default_cap_balance_gap() -> u32 { 2 }

fn default_event_check_interval() -> u32 { 300 }
fn default_event_probability() -> f64 { 0.15 }
fn default_max_events() -> u32 { 3 }
fn default_vip_reward() -> i32 { 300 }
fn default_evac_reward() -> i32 { 50 }
fn default_barrage_duration() -> u32 { 300 }
fn default_ambush_duration() -> u32 { 600 }
fn default_cap_template_red() -> String { "RCAP".into() }
fn default_cap_template_blue() -> String { "BCAP".into() }
fn default_cap_altitude_m() -> f64 { 8000.0 }
fn default_cap_speed_ms() -> f64 { 250.0 }
fn default_cap_max_push_m() -> f64 { 60000.0 }
fn default_cap_engage_radius_m() -> f64 { 45000.0 }
fn default_cap_idle_rtb_secs() -> u32 { 240 }
fn default_cap_duration() -> u32 { 600 }
fn default_cap_orbit_radius() -> f64 { 15_000.0 }
fn default_cap_probability() -> f64 { 0.35 }
fn default_capture_time() -> u32 { 180 }
fn default_capture_garrison_revive_fraction() -> f64 { 0.25 }
fn default_capture_cooldown_secs() -> u32 { 120 }
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
            cap_altitude_m: default_cap_altitude_m(),
            cap_speed_ms: default_cap_speed_ms(),
            cap_max_push_m: default_cap_max_push_m(),
            cap_engage_radius_m: default_cap_engage_radius_m(),
            cap_idle_rtb_secs: default_cap_idle_rtb_secs(),
            cap_orbit_radius_m: default_cap_orbit_radius(),
            cap_probability: default_cap_probability(),
            capture_time_secs: default_capture_time(),
            capture_min_unit_pct_destroyed: 0.0,
            capture_cooldown_secs: default_capture_cooldown_secs(),
            capture_garrison_revive_fraction: default_capture_garrison_revive_fraction(),
            capture_garrison_revive_include_sam: false,
            barrage_radius_m: default_barrage_radius_m(),
            barrage_max_groups: default_barrage_max_groups(),
            cap_trigger_radius_m: default_cap_trigger_radius(),
            cap_max_concurrent: default_cap_max_concurrent(),
            cap_max_per_side: default_cap_max_per_side(),
            cap_min_threat_count: default_cap_min_threat(),
            cap_respawn_cooldown_secs: default_cap_respawn_cooldown(),
            cap_trigger_on_known_players: false,
            cap_balance_gap: default_cap_balance_gap(),
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

/// Short slot-entry radio briefing shown to a player when they take a slot,
/// so they know where to tune for the live GCI controller. Purely cosmetic —
/// the operational GCI configuration lives in bfdb's `gci.json`. Omit the
/// whole block to show nothing on slot entry.
#[derive(Debug, Clone, Serialize, Deserialize, Default, schemars::JsonSchema)]
pub struct GciBriefingCfg {
    /// Blue GCI/AWACS radio, spoken as written (e.g. "251.0 AM" or
    /// "251.0 AM / 33X"). Omit to skip the frequency line for blue.
    #[serde(default)]
    pub blue_freq: Option<String>,
    /// Red GCI/AWACS radio.
    #[serde(default)]
    pub red_freq: Option<String>,
    /// Blue controller callsign shown in the briefing. Defaults to "Magic".
    #[serde(default)]
    pub blue_callsign: Option<String>,
    /// Red controller callsign shown in the briefing. Defaults to "Overlord".
    #[serde(default)]
    pub red_callsign: Option<String>,
    /// Optional extra line appended to the briefing (e.g. "Broadcast only —
    /// no check-in required" or a SRS server note).
    #[serde(default)]
    pub note: Option<String>,
    /// Seconds the panel message stays on screen. Default 20.
    #[serde(default = "default_gci_briefing_secs")]
    pub display_secs: i64,
}

fn default_gci_briefing_secs() -> i64 {
    20
}

impl GciBriefingCfg {
    /// Render the briefing panel text for a side, or `None` if there is
    /// nothing configured to show that side.
    pub fn render(&self, side: Side) -> Option<std::string::String> {
        let (freq, callsign, default_cs) = match side {
            Side::Red => (&self.red_freq, &self.red_callsign, "Overlord"),
            _ => (&self.blue_freq, &self.blue_callsign, "Magic"),
        };
        let str_opt = |o: &Option<String>| -> Option<std::string::String> {
            o.as_ref().map(|s| s.as_str().to_string()).filter(|s| !s.is_empty())
        };
        let freq = str_opt(freq)?;
        let callsign = str_opt(callsign).unwrap_or_else(|| default_cs.to_string());
        let mut out = format!("GCI: {callsign} on {freq} (SRS)");
        if let Some(note) = str_opt(&self.note) {
            out.push('\n');
            out.push_str(&note);
        }
        Some(out)
    }
}

/// The auto-generated situational briefing: a short "what is going on right
/// now" panel on slot entry, the paged F10 -> Info -> Situation report, and
/// the dashboard BRIEFING page. All three render the same engine-built
/// [`bfprotocols::situation::SituationReport`], so they cannot disagree.
///
/// Omit the whole block to keep the slot-entry panel off; the F10 report and
/// the dashboard page are always available (they are pull, not push).
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct SituationBriefingCfg {
    /// Show the condensed briefing panel when a player takes a slot.
    /// Default true.
    #[serde(default = "default_true")]
    pub on_slot_entry: bool,
    /// Seconds the slot-entry panel stays on screen. Default 40 -- long
    /// enough to read a dozen lines while the engines spool.
    #[serde(default = "default_situation_display_secs")]
    pub display_secs: i64,
    /// Seconds after slot entry before the panel fires. Sequenced after the
    /// ATIS (15s) so the two don't overwrite each other. Default 22.
    #[serde(default = "default_situation_delay_secs")]
    pub delay_secs: u32,
    /// Tasking lines on the slot-entry panel. The full list is always on the
    /// F10 report and the dashboard. Default 3.
    #[serde(default = "default_situation_panel_tasks")]
    pub panel_tasks: usize,
    /// Tasking lines the engine will generate at all, across every category.
    /// Default 12.
    #[serde(default = "default_situation_max_tasks")]
    pub max_tasks: usize,
    /// Include known enemy air-defence areas (from this side's own recon /
    /// ELINT / JTAC intel -- never omniscient) in the report. Default true.
    #[serde(default = "default_true")]
    pub include_threats: bool,
    /// Free-form line appended to the briefing, e.g. a campaign premise or a
    /// Discord link.
    #[serde(default)]
    pub note: Option<String>,
}

fn default_situation_display_secs() -> i64 {
    40
}

fn default_situation_delay_secs() -> u32 {
    22
}

fn default_situation_panel_tasks() -> usize {
    3
}

fn default_situation_max_tasks() -> usize {
    12
}

impl Default for SituationBriefingCfg {
    fn default() -> Self {
        Self {
            on_slot_entry: true,
            display_secs: default_situation_display_secs(),
            delay_secs: default_situation_delay_secs(),
            panel_tasks: default_situation_panel_tasks(),
            max_tasks: default_situation_max_tasks(),
            include_threats: true,
            note: None,
        }
    }
}

/// One line of the coalition comms card.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CommsChannelCfg {
    /// Radio preset/channel number a pilot dials up, when the airframe has
    /// presets. Purely for the card -- nothing writes it into the .miz.
    #[serde(default)]
    pub preset: Option<u8>,
    /// Who is on it: "AWACS / GCI -- MAGIC".
    pub label: String,
    pub freq_mhz: f64,
    /// "AM" or "FM". UHF and VHF-air are AM; the 30-76 MHz ground net is FM.
    #[serde(default = "default_modulation")]
    pub modulation: String,
    /// What it is for, one short line. Shown under the label on the briefing.
    #[serde(default)]
    pub purpose: Option<String>,
}

fn default_modulation() -> String {
    String::from("AM")
}

/// Coalition frequency allocation -- the comms card every pilot flies with.
///
/// The defaults are a deconflicted plan for a mixed DCS module set: blue lives
/// in UHF 251-270 (every western jet's 225-400 AM radio) plus VHF-FM 30-31 for
/// the helo/A-10 ground net; red lives in VHF-AM 124-145 and UHF 228-237,
/// inside the 100-150 / 220-400 coverage of the Russian R-862/R-863 radios.
/// The two sides never share a working channel, and 243.000 / 121.500 are
/// reserved as guard on both.
///
/// The engine overlays the *live* frequencies it actually knows -- the AWACS
/// and tanker radios from the running action specs, the JTAC laser codes --
/// on top of this plan when it builds a briefing, so a channel that is really
/// up shows as on-station and one that is only planned shows as planned.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CommsPlanCfg {
    #[serde(default = "default_blue_comms")]
    pub blue: Vec<CommsChannelCfg>,
    #[serde(default = "default_red_comms")]
    pub red: Vec<CommsChannelCfg>,
    /// First intra-flight frequency, MHz. FLIGHT 1 gets this, FLIGHT 2 gets
    /// `+ flight_step_mhz`, and so on. Kept clear of every channel above.
    #[serde(default = "default_blue_flight_base")]
    pub blue_flight_base_mhz: f64,
    #[serde(default = "default_red_flight_base")]
    pub red_flight_base_mhz: f64,
    #[serde(default = "default_flight_step")]
    pub flight_step_mhz: f64,
    /// How many numbered intra-flight channels to publish. Default 8.
    #[serde(default = "default_flight_count")]
    pub flight_count: u8,
}

fn default_modulation_fm() -> String {
    String::from("FM")
}

fn ch(preset: u8, label: &str, freq_mhz: f64, purpose: &str) -> CommsChannelCfg {
    CommsChannelCfg {
        preset: Some(preset),
        label: String::from(label),
        freq_mhz,
        modulation: default_modulation(),
        purpose: Some(String::from(purpose)),
    }
}

fn ch_fm(preset: u8, label: &str, freq_mhz: f64, purpose: &str) -> CommsChannelCfg {
    CommsChannelCfg {
        preset: Some(preset),
        label: String::from(label),
        freq_mhz,
        modulation: default_modulation_fm(),
        purpose: Some(String::from(purpose)),
    }
}

fn default_blue_comms() -> Vec<CommsChannelCfg> {
    vec![
        ch(1, "AWACS / GCI -- MAGIC", 251.0, "Primary control: picture, bogey dope, commit, declare"),
        ch(2, "GCI VHF relay -- MAGIC", 119.0, "Same controller for VHF-only airframes"),
        ch(3, "AWACS alternate -- DARKSTAR", 252.0, "Second controller / overflow when MAGIC is saturated"),
        ch(4, "Tanker TEXACO (boom)", 253.0, "Pre-strike and post-strike refuel, fixed-wing boom"),
        ch(5, "Tanker ARCO (boom)", 254.0, "Second boom track"),
        ch(6, "Tanker SHELL (drogue)", 255.0, "Probe-and-drogue: Hornet, Tomcat, Harrier, Viggen"),
        ch(7, "JTAC 1", 256.0, "Nine-line, talk-on, laser for the first active JTAC"),
        ch(8, "JTAC 2", 257.0, "Second active JTAC"),
        ch(9, "JTAC 3", 258.0, "Third active JTAC"),
        ch(10, "JTAC 4", 259.0, "Fourth active JTAC"),
        ch(11, "CSAR -- SANDY", 260.0, "Downed-pilot pickup: on-scene commander and the helo"),
        ch(12, "Package common -- STRIKE", 265.0, "Strike package internal, all flights"),
        ch(13, "Package common -- SEAD", 266.0, "SEAD/DEAD package internal"),
        ch(14, "Package common -- CAP", 267.0, "Sweep and escort internal"),
        ch(15, "Package common -- CAS", 268.0, "CAS stack check-in and deconfliction"),
        ch(16, "Carrier MARSHAL", 270.0, "Carrier approach control, marshal stack, case II/III"),
        ch(17, "Carrier TOWER / LSO", 127.5, "Ball call, paddles, deck ops"),
        ch(18, "GUARD (UHF)", 243.0, "Emergency only -- never used for traffic"),
        ch(19, "GUARD (VHF)", 121.5, "Emergency only"),
        ch_fm(20, "Ground / logistics net", 30.0, "Convoy, crate and warehouse coordination"),
        ch_fm(21, "Troop & crate ops", 31.0, "Helo lift working channel, troop drops"),
    ]
}

fn default_red_comms() -> Vec<CommsChannelCfg> {
    vec![
        ch(1, "GCI -- OVERLORD", 124.0, "Primary control: picture, bogey dope, commit, declare"),
        ch(2, "GCI UHF relay -- OVERLORD", 228.0, "Same controller for UHF-preferred airframes"),
        ch(3, "AWACS A-50 -- DRAGNET", 125.0, "Airborne early warning when an A-50 is up"),
        ch(4, "Tanker IL-78 -- KUZNETS", 126.0, "Probe-and-drogue refuel"),
        ch(5, "JTAC 1", 133.0, "Nine-line, talk-on, laser for the first active JTAC"),
        ch(6, "JTAC 2", 134.0, "Second active JTAC"),
        ch(7, "JTAC 3", 135.0, "Third active JTAC"),
        ch(8, "JTAC 4", 136.0, "Fourth active JTAC"),
        ch(9, "CSAR -- rescue", 137.0, "Downed-pilot pickup"),
        ch(10, "Package common -- STRIKE", 142.0, "Strike package internal, all flights"),
        ch(11, "Package common -- SEAD", 143.0, "SEAD/DEAD package internal"),
        ch(12, "Package common -- CAP", 144.0, "Sweep and escort internal"),
        ch(13, "Package common -- CAS", 145.0, "CAS stack check-in and deconfliction"),
        ch(14, "GUARD (UHF)", 243.0, "Emergency only -- never used for traffic"),
        ch(15, "GUARD (VHF)", 121.5, "Emergency only"),
        ch_fm(16, "Ground / logistics net", 40.0, "Convoy, crate and warehouse coordination"),
        ch_fm(17, "Troop & crate ops", 41.0, "Helo lift working channel, troop drops"),
    ]
}

fn default_blue_flight_base() -> f64 {
    305.0
}

fn default_red_flight_base() -> f64 {
    230.0
}

fn default_flight_step() -> f64 {
    1.0
}

fn default_flight_count() -> u8 {
    8
}

impl Default for CommsPlanCfg {
    fn default() -> Self {
        Self {
            blue: default_blue_comms(),
            red: default_red_comms(),
            blue_flight_base_mhz: default_blue_flight_base(),
            red_flight_base_mhz: default_red_flight_base(),
            flight_step_mhz: default_flight_step(),
            flight_count: default_flight_count(),
        }
    }
}

impl CommsPlanCfg {
    pub fn for_side(&self, side: Side) -> &[CommsChannelCfg] {
        match side {
            Side::Red => &self.red,
            _ => &self.blue,
        }
    }

    /// The numbered intra-flight channels for a side, as
    /// `("FLIGHT 3", 307.0)`.
    pub fn flight_channels(&self, side: Side) -> Vec<(std::string::String, f64)> {
        let base = match side {
            Side::Red => self.red_flight_base_mhz,
            _ => self.blue_flight_base_mhz,
        };
        (0..self.flight_count)
            .map(|i| {
                (
                    format!("FLIGHT {}", i + 1),
                    base + self.flight_step_mhz * i as f64,
                )
            })
            .collect()
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
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct LogiFromSceneryCfg {
    /// Logi rating (0-100) for an objective where the scenery scan found no
    /// trackable buildings at all. Default 100 (treated as fully supplied).
    #[serde(default = "default_scenery_fallback_logi")]
    pub fallback_logi: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HeloInsertionCfg {
    /// Helicopter group template name per side (must be a Helicopter group in
    /// the miz file). Deliberately separate from
    /// `warehouse.air_logistics.aircraft_template` -- that feature flies
    /// fixed-wing cargo planes between logistics hubs and shouldn't be
    /// repointed at a helicopter, and these missions land at arbitrary
    /// objectives in the open, not just at airfields, so they need an
    /// airframe that can actually do that. A side with no entry here can't
    /// launch either mission -- the F10 command tells the player why.
    #[serde(default)]
    pub aircraft_template: FxHashMap<Side, String>,
    /// Cruise altitude in meters (BARO) for the transit leg.
    #[serde(default = "default_helo_altitude_m")]
    pub altitude_m: f64,
    /// Cruise speed in km/h for the transit leg.
    #[serde(default = "default_helo_speed_kph")]
    pub speed_kph: f64,
    /// Which `troops[side]` entry (looked up by name) the troop-insertion
    /// mission carries. Must exist for both sides; wants `can_capture: true`
    /// to actually be useful at the destination.
    #[serde(default = "default_helo_troop_name")]
    pub troop_name: String,
    /// Points charged to call the troop-insertion mission, ON TOP of the
    /// troop's own `cost` (which is charged too, same as a normal deploy).
    #[serde(default)]
    pub troop_mission_cost: i32,
    /// Points charged to call the resource-delivery mission.
    #[serde(default = "default_helo_supply_cost")]
    pub supply_mission_cost: i32,
    /// Max units of each equipment/liquid item moved per resource-delivery
    /// run (further capped by what the origin objective actually has stored).
    #[serde(default = "default_helo_supply_per_item")]
    pub supply_amount_per_item: u32,
    /// Neither mission will launch if the chosen origin/destination pair is
    /// farther apart than this (metres). Default 150km.
    #[serde(default = "default_helo_max_range_m")]
    pub max_range_m: f64,
    /// How close to the destination point, while on the ground, counts as
    /// "landed and delivered". Default 200m.
    #[serde(default = "default_helo_landing_radius_m")]
    pub landing_radius_m: f64,
}

fn default_helo_troop_name() -> String { String::from("Standard") }
fn default_helo_altitude_m() -> f64 { 500.0 }
fn default_helo_speed_kph() -> f64 { 220.0 }
fn default_helo_supply_cost() -> i32 { 50 }
fn default_helo_supply_per_item() -> u32 { 50 }
fn default_helo_max_range_m() -> f64 { 150_000.0 }
fn default_helo_landing_radius_m() -> f64 { 200.0 }

fn default_scenery_fallback_logi() -> u8 {
    100
}

impl Default for LogiFromSceneryCfg {
    fn default() -> Self {
        Self { fallback_logi: default_scenery_fallback_logi() }
    }
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
    /// Treasury each side keeps in reserve for military actions -- the passive
    /// objective point-funding pass will not spend a side's treasury below
    /// this floor. Without it, a losing side with many damaged/threatened
    /// objectives drains its entire income into point-drips every tick and can
    /// never accumulate enough for a counterattack, barrage, ambush or CAP
    /// (observed: Red pinned at treasury 0 for a whole session, "no affordable
    /// action" every tick). Default: 300 (covers the priciest default action).
    #[serde(default = "default_commander_action_reserve")]
    pub action_reserve: i64,
}

fn default_commander_action_reserve() -> i64 {
    300
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
