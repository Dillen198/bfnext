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

//! Radar, IADN and ELINT modelling, airborne EWR, special SAM sites.

use super::*;

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

pub(super) fn default_look_down_capable() -> bool { true }
pub(super) fn default_chaff_susceptibility() -> f32 { 0.3 }
pub(super) fn default_ecm_susceptibility() -> f32 { 0.4 }
pub(super) fn default_scan_interval_secs() -> u32 { 2 }

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

pub(super) fn default_rcs_hot() -> f32 { 1.0 }
pub(super) fn default_rcs_flank() -> f32 { 0.7 }
pub(super) fn default_rcs_beam() -> f32 { 0.35 }
pub(super) fn default_rcs_cold() -> f32 { 0.15 }
pub(super) fn default_notch_attenuation() -> f32 { 0.05 }
pub(super) fn default_notch_closure_threshold_ms() -> f32 { 50.0 }
pub(super) fn default_ground_radar_low_alt_threshold_m() -> f32 { 300.0 }
pub(super) fn default_track_smoothing_alpha() -> f32 { 0.3 }
pub(super) fn default_awacs_look_down_bonus() -> f32 { 1.5 }

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
    /// Share of SAM sites (0..1) drilled to shut down for anti-radiation
    /// missiles. Rolled per site every mission load, so which sites drop
    /// their radar for a HARM and which keep radiating (and die to it)
    /// changes from one round to the next. The rest have DCS's own
    /// ARM evasion turned off too. Default 0.6.
    #[serde(default = "default_arm_discipline")]
    pub arm_discipline: f64,
    /// Chance (0..1) that a drilled site notices one ARM that is actually
    /// flying at it, before decoys. Rolled once per missile per site.
    /// Default 0.75.
    #[serde(default = "default_arm_react_chance")]
    pub arm_react_chance: f64,
    /// An ARM counts as flying at a site when the site is within this many
    /// degrees of its heading. Sites off its nose ignore it. Default 25.
    #[serde(default = "default_arm_aim_cone_deg")]
    pub arm_aim_cone_deg: f64,
    /// Decoy weapon type names (TALD, MALD). Decoys in the air near a site
    /// saturate it: each cuts the chance it notices a real ARM by
    /// `decoy_saturation`. Default ADM_141A, ADM_141B.
    #[serde(default = "default_decoy_weapons")]
    pub decoy_weapons: FxHashSet<std::string::String>,
    /// How much each decoy within `harm_defense_radius_m` of a site cuts its
    /// chance of noticing an ARM (multiplicative). Default 0.3: four TALDs
    /// leave a drilled site under a one-in-five chance.
    #[serde(default = "default_decoy_saturation")]
    pub decoy_saturation: f64,
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
            arm_discipline: default_arm_discipline(),
            arm_react_chance: default_arm_react_chance(),
            arm_aim_cone_deg: default_arm_aim_cone_deg(),
            decoy_weapons: default_decoy_weapons(),
            decoy_saturation: default_decoy_saturation(),
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

pub(super) fn default_track_association_radius_m() -> f64 { 3000.0 }
pub(super) fn default_detection_snr_threshold() -> f32 { 0.5 }
pub(super) fn default_track_stale_secs() -> u32 { 60 }
pub(super) fn default_track_drop_secs() -> u32 { 120 }
pub(super) fn default_sam_cue_enabled() -> bool { true }
pub(super) fn default_sam_cue_confidence_threshold() -> f32 { 0.4 }
pub(super) fn default_harm_defense_radius_m() -> f64 { 20_000.0 }
pub(super) fn default_harm_defense_cooldown_secs() -> u32 { 20 }
pub(super) fn default_arm_discipline() -> f64 { 0.6 }
pub(super) fn default_arm_react_chance() -> f64 { 0.75 }
pub(super) fn default_arm_aim_cone_deg() -> f64 { 25.0 }
pub(super) fn default_decoy_weapons() -> FxHashSet<std::string::String> {
    ["ADM_141A", "ADM_141B"].into_iter().map(std::string::String::from).collect()
}
pub(super) fn default_decoy_saturation() -> f64 { 0.3 }
pub(super) fn default_min_hot_dwell_secs() -> u32 { 15 }
pub(super) fn default_reaction_delay_max_secs() -> u32 { 4 }
pub(super) fn default_track_radar_range_fraction() -> f32 { 0.5 }
pub(super) fn default_jamming_enabled() -> bool { true }
pub(super) fn default_jamming_range_m() -> f64 { 40_000.0 }
pub(super) fn default_jamming_detection_penalty() -> f32 { 0.5 }
pub(super) fn default_sam_offboard_cue_enabled() -> bool { true }
pub(super) fn default_sam_offboard_cue_range_m() -> f64 { 60_000.0 }

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

pub(super) fn default_contact_cluster_radius_m() -> f64 { 500.0 }
pub(super) fn default_half_life_recon() -> u32 { 600 }
pub(super) fn default_half_life_sf() -> u32 { 1800 }
pub(super) fn default_half_life_awacs() -> u32 { 300 }
pub(super) fn default_half_life_ewr() -> u32 { 180 }
pub(super) fn default_half_life_jtac() -> u32 { 3600 }
pub(super) fn default_confidence_delete_threshold() -> f32 { 0.05 }
pub(super) fn default_max_contacts_per_side() -> usize { 200 }
pub(super) fn default_show_unit_class() -> bool { true }
pub(super) fn default_show_confidence_on_map() -> bool { true }

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
