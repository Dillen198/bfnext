use compact_str::CompactString;
use dcso3::{coalition::Side, net::Ucid, Vector2};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU64, Ordering};

/// Unique mission identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct MissionId(u64);

static NEXT_MISSION_ID: AtomicU64 = AtomicU64::new(1);

impl MissionId {
    pub fn new() -> Self {
        Self(NEXT_MISSION_ID.fetch_add(1, Ordering::Relaxed))
    }
}

/// Type of planned mission
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MissionType {
    Strike,
    FighterSweep,
    Escort,
    Sead,
    Cap,
    Reconnaissance,
    Transport,
    Refueling,
    Cas,
}

impl std::fmt::Display for MissionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Strike => write!(f, "Strike"),
            Self::FighterSweep => write!(f, "Fighter Sweep"),
            Self::Escort => write!(f, "Escort"),
            Self::Sead => write!(f, "SEAD"),
            Self::Cap => write!(f, "CAP"),
            Self::Reconnaissance => write!(f, "Recon"),
            Self::Transport => write!(f, "Transport"),
            Self::Refueling => write!(f, "Refueling"),
            Self::Cas => write!(f, "CAS"),
        }
    }
}

/// Status of a planned mission
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MissionStatus {
    Planning,
    Briefing,
    Active,
    Completed,
    Cancelled,
}

/// A waypoint in a flight plan
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlanWaypoint {
    pub name: CompactString,
    pub pos: Vector2,
    pub altitude_m: f64,
    pub speed_mps: f64,
    pub tot: Option<DateTime<Utc>>,
}

/// A flight within a planned mission
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlightPlan {
    pub callsign: CompactString,
    pub aircraft_type: CompactString,
    pub num_aircraft: u8,
    pub role: MissionType,
    pub waypoints: Vec<PlanWaypoint>,
    pub assigned_players: Vec<Ucid>,
    pub loadout_suggestion: Option<CompactString>,
}

/// A player-created planned mission
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlannedMission {
    pub id: MissionId,
    pub name: CompactString,
    pub mission_type: MissionType,
    pub creator: Ucid,
    pub side: Side,
    pub created_at: DateTime<Utc>,
    pub scheduled_time: Option<DateTime<Utc>>,
    pub status: MissionStatus,
    pub flights: Vec<FlightPlan>,
    pub target_pos: Option<Vector2>,
    pub briefing: CompactString,
    /// Set to true after mission completion rewards have been distributed.
    /// Guards against double-awarding.
    #[serde(default)]
    pub rewarded: bool,
}
