//! The ground war as one coalition sees it (`query-ground-war`), and the
//! commands the dashboard sends back (`ground-command`).
//!
//! The picture is fog-of-war by construction: the engine builds it for one
//! side, with that side's formations in full, enemy formations only where
//! that side's forces have spotted them (and, for a while after, where they
//! were last seen), and the battles (which both sides are in, so both see).
//! The side's own human players are in it too, live, so the dashboard can
//! show them on the battlefield.

use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;

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
    /// The side's human players in a slot right now, live.
    #[serde(default)]
    pub players: Vec<LivePlayer>,
    /// The side's recent ground-war events, newest first.
    #[serde(default)]
    pub events: Vec<GroundEvent>,
    /// Server time the picture was built, unix seconds.
    #[serde(default)]
    pub time: i64,
    /// How far our forces see enemy ground forces, metres.
    #[serde(default)]
    pub spot_m: f64,
    /// How close two forces have to be to fight, metres.
    #[serde(default)]
    pub engage_m: f64,
}

/// One vehicle (or infantry team) of a formation, where it actually is.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroundUnit {
    pub pos: LatLon,
    /// Degrees true.
    pub heading: f64,
    /// "tank" | "ifv" | "apc" | "recon" | "aaa" | "sam" | "artillery" | "infantry" | "truck"
    pub role: String,
    /// The DCS type name.
    pub typ: String,
}

/// One of the side's human players.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LivePlayer {
    pub name: String,
    /// The DCS type name of what they are in.
    pub typ: String,
    /// "plane" | "helicopter" | "ground" | "ship"
    pub category: String,
    pub pos: LatLon,
    pub alt_m: f64,
    /// Degrees true.
    pub heading: f64,
    pub speed_kts: f64,
    pub in_air: bool,
    /// Set by bfdb for the viewer's own unit.
    #[serde(default)]
    pub is_self: bool,
    /// The player's ucid. The engine fills it in so bfdb can find the
    /// viewer; bfdb removes it before the picture leaves the server.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ucid: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroundEvent {
    /// Unix seconds.
    pub at: i64,
    /// "contact" | "battle" | "loss" | "kill" | "assault" | "capture" |
    /// "broken" | "supply" | "arrived" | "raised" | "order" | "destroyed"
    pub kind: String,
    pub text: String,
    #[serde(default)]
    pub pos: Option<LatLon>,
    #[serde(default)]
    pub formation: Option<u32>,
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
    /// "armour" | "mechanised" | "motorised" | "infantry"
    #[serde(default)]
    pub kind: String,
    /// Every vehicle, where it is.
    #[serde(default)]
    pub units: Vec<GroundUnit>,
    /// Vehicles alive by role.
    #[serde(default)]
    pub make_up: BTreeMap<String, u32>,
    /// Combat power now (strength x supply x morale x deployment), and at
    /// full strength.
    #[serde(default)]
    pub power: f64,
    #[serde(default)]
    pub power_full: f64,
    /// Fuel and ammunition carried, percent.
    #[serde(default)]
    pub supply_pct: u8,
    /// A friendly base is close enough to keep it supplied.
    #[serde(default)]
    pub in_supply: bool,
    #[serde(default)]
    pub morale_pct: u8,
    /// Morale has collapsed: it is falling back whatever its orders.
    #[serde(default)]
    pub broken: bool,
    /// "column" | "deploying" | "deployed" | "dug_in"
    #[serde(default)]
    pub deployment: String,
    /// Current speed, km/h (0 when stopped).
    #[serde(default)]
    pub speed_kph: f64,
    /// Road it has covered recently, oldest first.
    #[serde(default)]
    pub trail: Vec<LatLon>,
    #[serde(default)]
    pub losses: u32,
    #[serde(default)]
    pub kills: u32,
}

/// An enemy formation our forces have spotted, now or recently.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnemyContact {
    pub pos: LatLon,
    /// "armour" | "mechanised" | "motorised" | "infantry"
    pub kind: String,
    /// Rough size, to the nearest 5 vehicles.
    pub approx_vehicles: u32,
    pub heading: f64,
    /// Stable per enemy formation, so the dashboard can follow it.
    #[serde(default)]
    pub id: u32,
    /// Seconds since we last saw it: 0 = in sight now, otherwise `pos` is
    /// where it was then.
    #[serde(default)]
    pub last_seen_secs: u32,
    /// Moving when last seen.
    #[serde(default)]
    pub moving: bool,
    /// Its vehicles, only while in sight and close.
    #[serde(default)]
    pub units: Vec<GroundUnit>,
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
    /// How hard it is being fought, 0..1.
    #[serde(default)]
    pub intensity: f64,
    /// Vehicles each side has lost in it.
    #[serde(default)]
    pub our_losses: u32,
    #[serde(default)]
    pub enemy_losses: u32,
    /// "meeting" (two formations) | "assault" (on a base)
    #[serde(default)]
    pub kind: String,
    /// The objective being fought for, if it is an assault.
    #[serde(default)]
    pub objective: Option<u64>,
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
    /// Supply on hand, percent (ours only).
    #[serde(default)]
    pub supply: Option<u8>,
    /// Garrison vehicles alive (ours only).
    #[serde(default)]
    pub garrison: Option<u32>,
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
