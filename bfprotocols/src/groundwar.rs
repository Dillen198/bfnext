//! The ground war as one coalition sees it (`query-ground-war`), and the
//! commands the dashboard sends back (`ground-command`).
//!
//! The picture is fog-of-war by construction: the engine builds it for one
//! side, with that side's formations in full, enemy formations only where
//! that side's forces are in contact with them, and the battles (which both
//! sides are in, so both see).

use serde_derive::{Deserialize, Serialize};

/// A point on the map: latitude, longitude.
pub type LatLon = [f64; 2];

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroundPicture {
    /// "Blue" / "Red": whose picture this is.
    pub side: String,
    pub enabled: bool,
    pub max_formations: u32,
    /// Formations both sides have in DCS right now, and the most there can be.
    pub live: u32,
    pub max_live: u32,
    /// Seconds a player's order keeps the AI off a formation.
    pub player_lock_secs: u32,
    pub formations: Vec<FormationInfo>,
    pub enemy: Vec<EnemyContact>,
    pub battles: Vec<BattleInfo>,
    pub objectives: Vec<GroundObjective>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormationInfo {
    pub id: u32,
    pub name: String,
    pub pos: LatLon,
    /// Degrees true.
    pub heading: f64,
    /// "hold" | "attack" | "defend" | "withdraw"
    pub order: String,
    pub target: Option<u64>,
    pub target_name: Option<String>,
    /// "moving" | "holding" | "assaulting"
    pub posture: String,
    pub alive: u32,
    pub total: u32,
    pub has_infantry: bool,
    /// Real DCS units right now (a player is near, or it is fighting).
    pub live: bool,
    /// Held at the edge of a fight the server has no room to simulate.
    pub halted: bool,
    /// In contact with the enemy.
    pub engaged: bool,
    pub home: u64,
    pub home_name: String,
    /// Player whose order it is following; `None` = the AI's.
    pub commander: Option<String>,
    /// Minutes until the AI may take it back.
    pub locked_mins: Option<u32>,
    /// The road ahead, destination last (thinned).
    pub path: Vec<LatLon>,
    pub km_to_go: f64,
    pub eta_mins: Option<u32>,
}

/// An enemy formation one of our formations or bases is in contact with.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnemyContact {
    pub pos: LatLon,
    /// "armour" | "mechanised" | "infantry"
    pub kind: String,
    /// Rough size, to the nearest 5 vehicles.
    pub approx_vehicles: u32,
    pub heading: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BattleInfo {
    pub id: u32,
    pub pos: LatLon,
    pub radius_m: f64,
    /// The nearest objective, for a name.
    pub near: Option<String>,
    /// Unix seconds.
    pub since: i64,
    /// Being fought in DCS (else on the map only).
    pub live: bool,
    /// Our formations in it.
    pub ours: Vec<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroundObjective {
    pub id: u64,
    pub name: String,
    pub pos: LatLon,
    /// "Blue" / "Red" / "Neutral"
    pub owner: String,
    pub kind: String,
    /// Our own objectives only.
    pub health: Option<u8>,
    pub threatened: Option<bool>,
    /// Garrison groups it could send out as a new formation (ours only).
    pub can_raise: Option<u32>,
    pub being_captured: bool,
}

/// What the dashboard asks the engine to do with a side's formations. The
/// player is identified separately (their ucid, resolved by bfdb from their
/// login), and the engine checks the formation is on that player's side.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum GroundCommand {
    Attack { formation: u32, objective: u64 },
    Defend { formation: u32, objective: u64 },
    Withdraw { formation: u32, objective: u64 },
    Hold { formation: u32 },
    Release { formation: u32 },
    Raise { objective: u64 },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroundCommandReply {
    pub ok: bool,
    pub message: String,
    /// The formation the command made or moved.
    pub formation: Option<u32>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn commands_parse_from_dashboard_json() {
        let c: GroundCommand =
            serde_json::from_str(r#"{"kind":"attack","formation":3,"objective":17}"#).unwrap();
        assert!(matches!(c, GroundCommand::Attack { formation: 3, objective: 17 }));
        let c: GroundCommand = serde_json::from_str(r#"{"kind":"raise","objective":5}"#).unwrap();
        assert!(matches!(c, GroundCommand::Raise { objective: 5 }));
        assert!(serde_json::from_str::<GroundCommand>(r#"{"kind":"nuke","formation":1}"#).is_err());
    }
}
