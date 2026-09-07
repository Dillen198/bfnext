//! Serializable GCI / air-defense picture for dashboard and netidx stats.

use chrono::prelude::*;
use dcso3::coalition::Side;
use serde::{Deserialize, Serialize};

/// One fused or friendly track on the coalition picture.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciTrack {
    pub id: u64,
    /// Display track number (e.g. "TN042").
    pub tn: String,
    /// 0=hostile 1=friendly 2=unknown 3=neutral
    pub iff: u8,
    /// 0=unknown 1=fighter 2=bomber 3=helo
    pub cls: u8,
    pub lat: f64,
    pub lon: f64,
    /// Altitude feet MSL (rounded).
    pub alt_ft: i32,
    /// Heading degrees true.
    pub hdg: u16,
    /// Speed knots (rounded).
    pub spd_kts: u16,
    /// Bearing degrees from reference point.
    pub brg: u16,
    /// Range nautical miles from reference point.
    pub rng_nm: u16,
    /// Seconds since last sensor update.
    pub age: u16,
    pub stale: bool,
    /// Bitmask: 1=ground 2=airborne (matches bflib DetectedBy).
    pub src: u8,
    /// Fusion confidence 0.0–1.0.
    pub conf: f32,
    /// Track under ECM/chaff/jam corridor (degraded picture).
    #[serde(default)]
    pub contested: bool,
    /// EW strength 0–100 for display (jam + chaff + zones).
    #[serde(default)]
    pub jam: u8,
}

/// Mission jam corridor for GCI overlay.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciJamZone {
    pub lat: f64,
    pub lon: f64,
    pub radius_nm: u16,
    /// 0–100
    pub strength: u8,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciDonor {
    pub side: Side,
    pub lat: f64,
    pub lon: f64,
    pub range_m: u32,
    pub airborne: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciBullseye {
    pub side: Side,
    pub lat: f64,
    pub lon: f64,
}

/// Per–radar-donor terrain horizon: max line-of-sight range per bearing (nautical miles).
/// Values past `max_nm` along that bearing are terrain-shadowed at probe altitude.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciTerrainHorizon {
    pub side: Side,
    pub lat: f64,
    pub lon: f64,
    /// Donor nominal range (NM), caps shadow extent.
    pub range_nm: u16,
    pub brg_step: u8,
    /// One entry per bearing slice (0°, step°, 2×step°, …); 0 = blocked at the radar site.
    pub max_nm: Vec<u16>,
    #[serde(default)]
    pub airborne: bool,
}

// ─── Live voice GCI (proactive SRS callouts) ────────────────────────────────
//
// Distinct from the map-oriented `GciPicture` below: these types are the
// per-player *controller picture* returned by the `query-gci` engine RPC and
// consumed by bfdb's `gci` module, which diffs successive pictures and turns
// the changes into spoken NATO-brevity calls transmitted over SRS. It is a
// strict fog-of-war view — only contacts the player's own coalition sensors
// are painting appear here.

/// Aspect of a contact relative to the flight it is being called to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GciAspect {
    Hot,
    Flank,
    Beam,
    Cold,
}

/// A flight's preferred spoken units for GCI calls.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GciUnits {
    Metric,
    Imperial,
}

/// A flight's preferred position reference for GCI calls.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GciRef {
    /// Bearing/range from the flight's own aircraft (BRAA).
    Braa,
    /// Bearing/range from the coalition bullseye.
    Bullseye,
    /// Clock position relative to the flight's nose + high/low.
    Clock,
}

impl GciRef {
    pub fn from_u8(n: u8) -> Self {
        match n {
            1 => GciRef::Bullseye,
            2 => GciRef::Clock,
            _ => GciRef::Braa,
        }
    }
}

/// One hostile group on a flight's BRAA picture (already clustered). All
/// magnitudes are raw SI — bfdb's `gci` module converts to the flight's
/// chosen units at render time.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciContact {
    /// True bearing flight→group, degrees.
    pub brg: u16,
    /// Slant range, metres.
    pub rng_m: u32,
    /// Altitude, metres MSL.
    pub alt_m: i32,
    /// Group heading, degrees true.
    pub hdg: u16,
    /// Ground speed, m/s.
    pub spd_ms: u16,
    /// Vertical speed, m/s (positive = climbing).
    #[serde(default)]
    pub vspd_ms: i16,
    pub aspect: GciAspect,
    /// 0=unknown 1=fighter 2=bomber 3=helo (matches bflib `ContactClass`).
    pub class: u8,
    /// Raw DCS type name of the lead contact (e.g. "MiG-29A", "F-16C_50"),
    /// when the engine can identify it — bfdb maps it to a reporting name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub type_name: Option<String>,
    /// Number of contacts merged into this group (1 = single).
    pub group_size: u8,
    /// Track data is past the stale threshold but not yet dropped.
    pub stale: bool,
}

/// A live enemy surface-to-air threat whose engagement zone covers this flight.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciSamThreat {
    /// True bearing flight→site, degrees.
    pub brg: u16,
    /// Range to the site, metres.
    pub rng_m: u32,
    /// Nominal engagement range of the site, metres — used to classify the
    /// call as short / medium / long range.
    pub site_range_m: u32,
}

/// One friendly human flight the GCI is controlling.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciFlight {
    pub ucid: String,
    /// DCS flight callsign ("Enfield11"), or the player name as a fallback.
    pub callsign: String,
    /// The pilot's name. GCI addresses calls to this when the server prefers
    /// names over callsigns (players who don't set a flight callsign).
    #[serde(default)]
    pub player_name: String,
    pub lat: f64,
    pub lon: f64,
    pub alt_m: i32,
    pub heading: u16,
    pub speed_ms: u16,
    /// The flight's explicit GCI unit choice, when the player set one. `None`
    /// means "use the server default".
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub units: Option<GciUnits>,
    /// The flight's explicit position-reference choice. `None` = server default.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub reference: Option<GciRef>,
    /// Hostile groups this flight's coalition sensors are painting, nearest
    /// first. Empty is meaningful: the flight is up but its picture is clean.
    pub contacts: Vec<GciContact>,
    /// Live enemy SAM threats covering this flight, nearest first.
    #[serde(default)]
    pub sam_threats: Vec<GciSamThreat>,
    /// Enemy SAM missile launches near this flight in the last few seconds
    /// (from the engine's Shot events). `brg`/`rng_m` point at the launch site.
    #[serde(default)]
    pub sam_launches: Vec<GciSamThreat>,
    /// Hostile air killed near this flight in the last few seconds — "splash".
    /// `brg`/`rng_m` point at the kill location.
    #[serde(default)]
    pub splashes: Vec<GciSamThreat>,
}

/// A friendly support asset (tanker / AWACS) on the coalition picture.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciSupport {
    /// "tanker" or "awacs".
    pub kind: String,
    pub lat: f64,
    pub lon: f64,
    pub alt_m: i32,
    /// Callsign when known ("Texaco", "Overlord").
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub callsign: Option<String>,
}

/// The `query-gci` RPC return: one coalition's live controller picture.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciControlPicture {
    pub side: Side,
    pub time: DateTime<Utc>,
    /// Coalition bullseye lat/lon, when the engine knows it. Enables
    /// bullseye-format picture calls.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bullseye: Option<(f64, f64)>,
    #[serde(default)]
    pub flights: Vec<GciFlight>,
    /// Any friendly ground/air radar donor is alive. `false` → "tumbleweed".
    #[serde(default = "yes")]
    pub radar_up: bool,
    /// Friendly ejections in the last ~60 s (lat/lon) — "chute observed".
    #[serde(default)]
    pub ejections: Vec<(f64, f64)>,
    /// Friendly tanker / AWACS assets, for periodic support-location calls.
    #[serde(default)]
    pub support: Vec<GciSupport>,
}

fn yes() -> bool {
    true
}

/// Full theater picture (both coalitions); filter server-side before WebSocket.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GciPicture {
    pub time: DateTime<Utc>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub theater: Option<String>,
    #[serde(default)]
    pub bullseyes: Vec<GciBullseye>,
    #[serde(default)]
    pub donors: Vec<GciDonor>,
    /// Hostile/unknown tracks seen by Blue sensors (IADN fused).
    #[serde(default)]
    pub blue_hostile: Vec<GciTrack>,
    /// Hostile/unknown tracks seen by Red sensors.
    #[serde(default)]
    pub red_hostile: Vec<GciTrack>,
    /// Blue coalition friendly air (BFT).
    #[serde(default)]
    pub blue_friendly: Vec<GciTrack>,
    #[serde(default)]
    pub red_friendly: Vec<GciTrack>,
    /// Terrain LOS masks for ground EWR sites (recomputed periodically).
    #[serde(default)]
    pub terrain_horizons: Vec<GciTerrainHorizon>,
    /// Geographic jam corridors from mission cfg.
    #[serde(default)]
    pub jam_zones: Vec<GciJamZone>,
}
