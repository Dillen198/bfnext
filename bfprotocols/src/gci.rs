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
