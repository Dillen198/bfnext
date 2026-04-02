use crate::db::{
    group::{GroupId, UnitId},
    objective::ObjectiveId,
};
use dcso3::{coalition::Side, net::Ucid};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// Use std String for JSON serialization compatibility
type String = std::string::String;

/// Information about a campaign objective (airbase, FARP, FOB, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectiveInfo {
    pub id: ObjectiveId,
    pub name: String,
    pub kind: String,
    pub owner: Side,
    /// Position as (x, z) in DCS coordinates
    pub pos: (f64, f64),
    /// Health percentage (0-100)
    pub health: u8,
    /// Logistics health percentage (0-100)
    pub logi: u8,
    /// Supply level (0-100)
    pub supply: u8,
    /// Fuel level (0-100)
    pub fuel: u8,
    /// Whether the objective is currently threatened
    pub threatened: bool,
    /// Number of groups stationed at this objective per side
    pub group_count: HashMap<String, usize>,
}

/// Detailed objective info including inventory
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectiveDetails {
    #[serde(flatten)]
    pub info: ObjectiveInfo,
    /// Equipment inventory (vehicle type -> count)
    pub equipment: HashMap<String, u32>,
    /// Liquid inventory (fuel type -> amount)
    pub liquids: HashMap<String, u32>,
    /// Points accumulated at this objective
    pub points: i32,
}

/// Information about a player
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerInfo {
    pub ucid: Ucid,
    pub name: String,
    pub side: Side,
    pub points: i32,
    /// Lives per life type (e.g., "Standard" -> 3)
    pub lives: HashMap<String, u8>,
    /// Current slot name if slotted
    pub current_slot: Option<String>,
    /// Unit type if in a unit
    pub current_unit_type: Option<String>,
    /// Whether the player is currently in the air
    pub in_air: bool,
    /// Position if in a unit (x, z)
    pub position: Option<(f64, f64)>,
}

/// Information about a spawned group
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroupInfo {
    pub id: GroupId,
    pub name: String,
    pub side: Side,
    /// Group category (Plane, Helicopter, Vehicle, Ship, Static)
    pub kind: Option<String>,
    /// Group class (Logi, Aaa, Lr, Mr, Sr, Armor, etc.)
    pub class: String,
    /// How the group was spawned (Objective, Deployed, Troop, Crate, Action)
    pub origin_type: String,
    /// Player who deployed this group (if applicable)
    pub deployed_by: Option<Ucid>,
    /// Number of alive units
    pub alive_count: usize,
    /// Total units in group
    pub total_count: usize,
    /// Center position of the group (x, z)
    pub center: Option<(f64, f64)>,
}

/// Information about a unit
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnitInfo {
    pub id: UnitId,
    pub group_id: GroupId,
    pub name: String,
    /// Vehicle type name
    pub typ: String,
    pub side: Side,
    /// Position (x, z)
    pub pos: (f64, f64),
    /// Heading in radians
    pub heading: f64,
    /// Whether the unit is alive
    pub alive: bool,
}

/// Warehouse/inventory information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WarehouseInfo {
    pub objective_id: ObjectiveId,
    pub objective_name: String,
    /// Equipment inventory (vehicle type -> count)
    pub equipment: HashMap<String, u32>,
    /// Liquid inventory (fuel type -> amount)
    pub liquids: HashMap<String, u32>,
}

/// Logistics state information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogisticsInfo {
    /// Current logistics stage
    pub stage: String,
    /// Time until next logistics tick (seconds)
    pub next_tick_seconds: Option<i64>,
    /// Pending supply transfers
    pub pending_transfers: Vec<TransferInfo>,
}

/// Supply transfer information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransferInfo {
    pub from: ObjectiveId,
    pub from_name: String,
    pub to: ObjectiveId,
    pub to_name: String,
    pub side: Side,
}

/// Available action information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionInfo {
    pub name: String,
    pub kind: String,
    pub cost: u32,
    /// Number of times this action has been used this round
    pub times_used: usize,
    /// Maximum number of times this action can be used
    pub max_uses: Option<usize>,
}

/// Result of a spawn action
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnResult {
    pub success: bool,
    pub group_id: Option<GroupId>,
    pub message: String,
}

/// Campaign state summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CampaignState {
    /// Total objectives per side
    pub objectives_by_side: HashMap<String, usize>,
    /// Total players online per side
    pub players_by_side: HashMap<String, usize>,
    /// Total points available per side
    pub points_by_side: HashMap<String, i64>,
}
