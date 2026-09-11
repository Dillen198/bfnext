//! Air traffic control picture — the state a spoken tower, ground or approach
//! controller works from.
//!
//! Everything here comes out of the engine once per poll. The weather and
//! active-runway fields are the same ones the text ATIS already computes, so a
//! spoken ATIS and a text ATIS can never disagree.

use serde::{Deserialize, Serialize};

/// One runway, as DCS reports it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AtcRunway {
    /// The designator DCS gives, e.g. "09", "13L". Both ends of a strip appear
    /// as separate entries.
    pub name: String,
    /// Magnetic-ish heading implied by the designator, degrees.
    pub heading: u16,
    pub length_m: u32,
    pub width_m: u32,
}

/// An airfield or carrier a controller can speak for.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AtcAirfield {
    /// Objective id, stringified — stable across a campaign.
    pub id: String,
    /// Objective name, which is what players call the field ("Incirlik").
    pub name: String,
    pub lat: f64,
    pub lon: f64,
    /// Field elevation, feet MSL.
    pub elev_ft: i32,
    /// "airbase", "farp" or "carrier" — they get different phraseology.
    pub kind: String,

    // ── runways ────────────────────────────────────────────────────────────
    #[serde(default)]
    pub runways: Vec<AtcRunway>,
    /// The end currently favoured by the wind, by designator. `None` when DCS
    /// reports no usable runway (FARPs, carriers).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub active_runway: Option<String>,
    /// Carrier base recovery course, degrees. Only set for carrier groups.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub brc: Option<u16>,

    // ── weather ────────────────────────────────────────────────────────────
    /// Surface wind, direction it blows *from*.
    pub wind_from_deg: u16,
    pub wind_speed_kts: u16,
    pub qnh_inhg: f64,
    pub qnh_hpa: f64,
    /// Field-level pressure, for the altimeter setting on the deck.
    pub qfe_inhg: f64,
    pub qfe_hpa: f64,
    pub temp_c: i16,
    pub dewpoint_c: i16,
    pub visibility_m: u32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cloud_base_ft: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cloud_cover: Option<String>,
    pub precipitation: bool,
    /// Carrier recovery case (1, 2 or 3) implied by the cloud base.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub recovery_case: Option<u8>,

    // ── campaign state ─────────────────────────────────────────────────────
    /// Logistics rating, 0-100. Drives whether rearm and refuel are available.
    pub logi: u8,
    pub health: u8,
    pub supply: u8,
    pub fuel: u8,
    /// An enemy is inside or close to the zone — the field is under threat and
    /// ATIS says so.
    pub threatened: bool,
    /// Field is serviceable at all (owned, not neutral, logistics alive).
    pub open: bool,
}

/// A human aircraft in an airfield's area of responsibility.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AtcTraffic {
    pub ucid: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub unit_id: Option<u64>,
    pub callsign: String,
    #[serde(default)]
    pub player_name: String,
    pub lat: f64,
    pub lon: f64,
    /// Altitude feet MSL and height above the field, feet.
    pub alt_ft: i32,
    pub agl_ft: i32,
    pub heading: u16,
    pub speed_kts: u16,
    /// Climbing/descending, feet per minute.
    pub vspd_fpm: i32,
    pub on_ground: bool,
    /// Nearest friendly field, and where the aircraft is from it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub field: Option<String>,
    #[serde(default)]
    pub field_brg: u16,
    /// Range to that field, metres.
    #[serde(default)]
    pub field_rng_m: u32,
    /// True when the aircraft is a helicopter — different pattern and
    /// clearances.
    #[serde(default)]
    pub rotary: bool,
}

/// One coalition's ATC picture.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AtcPicture {
    #[serde(default)]
    pub airfields: Vec<AtcAirfield>,
    #[serde(default)]
    pub traffic: Vec<AtcTraffic>,
}
