//! Close air support picture — what a JTAC knows, shaped for a spoken CAS
//! engagement.
//!
//! The engine fills this from `jtac::Jtacs` once per poll; the voice side turns
//! it into a check-in, a situation update, a nine-line and clearance calls. It
//! is deliberately a *picture*, not a command channel: everything the
//! controller says is derived from state the engine already owns.

use serde::{Deserialize, Serialize};

/// A group of like contacts the JTAC is looking at, for the target description
/// and the situation update ("eight armoured vehicles, four trucks").
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CasContactGroup {
    /// Human-facing type name, already tidied by the engine.
    pub typ: String,
    pub count: u16,
}

/// The JTAC's currently designated target.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CasTarget {
    pub typ: String,
    pub lat: f64,
    pub lon: f64,
    /// Target elevation, feet MSL — nine-line line 4.
    pub elev_ft: i32,
    /// Bearing and range from the JTAC to the target.
    pub brg: u16,
    pub rng_m: u32,
    /// Is the laser actually firing right now?
    pub lasing: bool,
    /// How many like vehicles are in the immediate area of the target — the
    /// difference between "one tank" and "tank platoon".
    #[serde(default)]
    pub group_size: u16,
}

/// One JTAC, presented as a controlling agency a pilot can talk to.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CasJtac {
    /// Stable id (the engine's `JtId`, stringified) — the key for commands.
    pub id: String,
    /// Spoken callsign. Falls back to the group name when unnamed.
    pub callsign: String,
    pub lat: f64,
    pub lon: f64,
    pub alt_m: i32,
    /// Airborne JTACs (drones, helos) describe themselves differently.
    pub airborne: bool,
    pub laser_code: u16,
    pub ir_pointer: bool,
    /// Objective the JTAC is working — used as the initial point.
    pub location_name: String,
    /// Bearing/range from the JTAC to that objective's centre.
    pub location_brg: u16,
    pub location_rng_m: u32,
    pub target: Option<CasTarget>,
    /// Everything it can see, largest group first.
    #[serde(default)]
    pub contacts: Vec<CasContactGroup>,
    pub contact_count: u16,
    /// Distance to the nearest friendly ground unit — drives line 8 and the
    /// danger-close call.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub nearest_friendly_m: Option<u32>,
    /// Bearing from the target to that friendly, spoken as a cardinal.
    #[serde(default)]
    pub nearest_friendly_brg: u16,
    /// Artillery in range of the target, for "artillery available" in remarks.
    #[serde(default)]
    pub artillery_available: bool,
    /// Enemy air defences the JTAC knows about within a few miles of the
    /// target — the threat line of the situation update.
    #[serde(default)]
    pub threats: Vec<CasContactGroup>,
}

/// A human aircraft that could be working with a JTAC.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CasFlight {
    pub ucid: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub unit_id: Option<u64>,
    pub callsign: String,
    #[serde(default)]
    pub player_name: String,
    pub lat: f64,
    pub lon: f64,
    pub alt_m: i32,
    pub heading: u16,
    pub speed_ms: u16,
    /// Nearest JTAC on this coalition, and where it is from the flight.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub nearest_jtac: Option<String>,
    #[serde(default)]
    pub jtac_brg: u16,
    #[serde(default)]
    pub jtac_rng_m: u32,
}

/// One coalition's CAS picture.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CasPicture {
    #[serde(default)]
    pub jtacs: Vec<CasJtac>,
    #[serde(default)]
    pub flights: Vec<CasFlight>,
    /// Coalition bullseye, for position reports.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bullseye: Option<(f64, f64)>,
}
