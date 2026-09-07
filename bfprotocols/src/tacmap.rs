//! Server-authoritative tactical picture for the dashboard TACMAP.
//!
//! The engine (`bflib`) fuses its per-side sensor state — the EWR radar
//! network ([`crate::stats::DetectionSource::EWR`]), JTAC eyes-on, and the
//! recon/ELINT [`IntelDatabase`](../../bflib/src/db/intel.rs) — into one
//! [`TacPicture`] per coalition. `bfdb` caches both sides and streams the
//! right one over `/ws/tacmap` after resolving the viewer's session cookie
//! to a coalition. A browser never receives a contact its side has not
//! earned, so the map cannot be used to cheat.

use chrono::prelude::*;
use dcso3::coalition::Side;
use serde::{Deserialize, Serialize};

/// IFF disposition of a track, from the viewing side's perspective.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Iff {
    Friendly,
    Hostile,
    Unknown,
}

/// Coarse air-track classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AirClass {
    Fighter,
    Bomber,
    Attack,
    Helo,
    Awacs,
    Tanker,
    Transport,
    Unknown,
}

/// Which sensor family is holding an air track right now.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TrackSource {
    /// Ground-based EWR / SAM acquisition radar.
    GroundRadar,
    /// Airborne early warning (AWACS / E-3 / A-50).
    Awacs,
    /// Both ground and airborne sensors contribute.
    Fused,
    /// Friendly blue-force tracking / datalink (own-side contact, no radar
    /// hit required).
    Datalink,
}

/// Coarse ground-contact classification (mirrors bflib `IntelUnitClass`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GroundClass {
    Armor,
    #[serde(rename = "airdefense")]
    AirDefense,
    Artillery,
    Infantry,
    Airbase,
    Naval,
    Unknown,
}

/// Sensor origin of a ground contact (mirrors bflib `IntelSource`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GroundSource {
    Recon,
    #[serde(rename = "sf")]
    SpecialForces,
    Awacs,
    #[serde(rename = "ewr")]
    EwrFusion,
    Jtac,
    #[serde(rename = "humint")]
    HumanInt,
}

/// One air track on a coalition's fused picture.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AirTrack {
    /// Stable per-session id (hash of the engine `EnId`) so the client can
    /// keep a trail across ticks.
    pub id: u64,
    /// Owning coalition of the tracked unit (present on the `god` picture so
    /// a merged view can still colour by real side; `None` on a single-side
    /// picture where `iff` already says everything the viewer may know).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub side: Option<Side>,
    pub lat: f64,
    pub lon: f64,
    /// Altitude, metres MSL.
    pub alt_m: f64,
    /// Heading, degrees true.
    pub heading: f64,
    /// Ground speed, knots.
    pub speed_kts: f64,
    /// Vertical speed, m/s (positive = climbing).
    #[serde(default)]
    pub vspd_ms: f64,
    pub iff: Iff,
    pub class: AirClass,
    /// Seconds since the last sensor hit.
    pub age_s: u32,
    /// Past the drop-age threshold — render dimmed / coasting.
    pub stale: bool,
    /// Track quality degraded by ECM / chaff / a jam corridor.
    pub jammed: bool,
    pub source: TrackSource,
    /// Callsign, only when the engine already knows it (player slots). AI
    /// flights stay anonymous — the client numbers them itself.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
}

/// One ground / naval contact on a coalition's picture.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroundContact {
    pub id: u64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub side: Option<Side>,
    pub lat: f64,
    pub lon: f64,
    pub class: GroundClass,
    /// Estimated number of units in the contact.
    pub count: u8,
    /// Intel confidence, 0.0 (expired) – 1.0 (fresh / eyes-on).
    pub confidence: f32,
    /// 1-sigma position-uncertainty radius, metres (draw as a circle).
    pub uncertainty_m: f32,
    pub source: GroundSource,
    /// Seconds since the contact was last confirmed.
    pub age_s: u32,
    /// Engagement / lethal range in metres for an air-defence contact, when
    /// the engine can identify the emitter — the client auto-draws the threat
    /// ring from this so the operator doesn't type it in. `None` for
    /// non-SAM/AAA contacts or an unidentified emitter.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub threat_range_m: Option<f32>,
}

/// A friendly radar's nominal detection footprint — drawn as a coverage
/// ring so players can see where their picture has holes.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RadarRing {
    pub lat: f64,
    pub lon: f64,
    /// Nominal range, metres.
    pub range_m: f64,
    /// Airborne sensor (AWACS) vs a fixed ground site.
    pub airborne: bool,
    pub alive: bool,
}

/// One coalition's bullseye reference point.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct TacBullseye {
    pub side: Side,
    pub lat: f64,
    pub lon: f64,
}

/// The complete tactical picture handed to one coalition (or, for `side ==
/// None`, the merged god view for admins).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TacPicture {
    /// The coalition this picture belongs to; `None` = merged god view.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub side: Option<Side>,
    pub time: DateTime<Utc>,
    #[serde(default)]
    pub bullseye: Vec<TacBullseye>,
    #[serde(default)]
    pub air: Vec<AirTrack>,
    #[serde(default)]
    pub ground: Vec<GroundContact>,
    #[serde(default)]
    pub radar_rings: Vec<RadarRing>,
}

/// Frame shape sent over `/ws/tacmap`. `picture` is `None` for a viewer who
/// is not logged in or has no coalition this campaign — the client then
/// renders the territory map only.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TacFrame {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub picture: Option<TacPicture>,
    /// Why `picture` is absent: `"login"` (no session) or `"nocoalition"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}
