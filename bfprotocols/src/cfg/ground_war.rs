//! Configuration for the dynamic ground war (`Cfg::ground_war`): AI ground
//! formations that leave their objectives and fight for the map, commanded by
//! an AI commander and by players. Off unless the block is present.
//!
//! A formation is not spawned from nothing. It is made of an objective's own
//! garrison groups -- armour, infantry and (optionally) light air defence --
//! that leave the base, which is correspondingly weaker while they are away.
//! Its losses are made good the way the garrison's always were: it goes home
//! and rejoins it, and the base's repair and reinforcement rebuild the dead.

use super::{default_true, Rule};
use dcso3::{coalition::Side, String};
use fxhash::FxHashMap;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct GroundWarCfg {
    /// Off switch that keeps the block around. Default true.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Most formations a side can have in the field at once. Default 6.
    #[serde(default = "default_max_formations")]
    pub max_formations_per_side: u32,
    /// Most formations, both sides together, that exist as real DCS units at
    /// once. The rest move on the engine's map only. Every live formation is
    /// a few moving DCS groups, and moving ground units are what costs the
    /// server frame time. Default 6.
    #[serde(default = "default_max_live")]
    pub max_live_formations: u32,
    /// Most garrison groups one formation takes from its objective. Default 3.
    #[serde(default = "default_groups_per_formation")]
    pub groups_per_formation: u32,
    /// Combat groups (armour and infantry) an objective always keeps for its
    /// own defence. Its last infantry group never leaves either way: a base
    /// with no infantry reads 0% infantry and can be taken. Default 1.
    #[serde(default = "default_keep_home")]
    pub keep_home_combat_groups: u32,
    /// Take the garrison's AAA along as the formation's air defence. Default
    /// true.
    #[serde(default = "default_true")]
    pub take_aaa: bool,
    /// Road march speed, km/h. Off-road legs are driven at half of it.
    /// Default 30.
    #[serde(default = "default_speed_kph")]
    pub speed_kph: f64,
    /// A formation becomes real DCS units when a player is this close.
    /// Default 30000 m.
    #[serde(default = "default_bubble_m")]
    pub player_bubble_m: f64,
    /// ... or when an enemy formation, or the enemy objective it is
    /// attacking, is this close. Default 10000 m.
    #[serde(default = "default_contact_m")]
    pub contact_m: f64,
    /// A live formation with no reason to be live any more is despawned
    /// after this long. Default 300.
    #[serde(default = "default_despawn_grace")]
    pub despawn_grace_secs: u32,
    /// Within this of its destination a formation has arrived. Default 1500 m.
    #[serde(default = "default_arrive_m")]
    pub arrive_m: f64,
    /// A formation below this share of its starting strength (percent) is
    /// pulled back to refit. Default 40.
    #[serde(default = "default_withdraw_pct")]
    pub withdraw_strength_pct: u8,
    /// While two enemy formations are in contact and neither can be made
    /// live (the live budget is spent), the engine fights it out on the map:
    /// every `attrition_secs`, each side loses this fraction of the other's
    /// live strength in vehicles (at least one). Default 0.1 every 300 s.
    #[serde(default = "default_attrition_rate")]
    pub attrition_rate: f64,
    #[serde(default = "default_attrition_secs")]
    pub attrition_secs: u32,
    /// The AI commander. Absent = players command alone.
    #[serde(default)]
    pub ai: Option<GroundAiCfg>,
    /// Who may command formations in game and on the dashboard. Default:
    /// everyone.
    #[serde(default)]
    pub command_rule: Rule,
    /// An order given by a player keeps the AI off that formation for this
    /// long, unless the player hands it back first. Default 3600.
    #[serde(default = "default_player_lock")]
    pub player_order_lock_secs: u32,
    /// A player can give an order this often. Default 30.
    #[serde(default = "default_order_cooldown")]
    pub player_order_cooldown_secs: u32,
    /// Points it costs a player to raise a new formation. Orders are free.
    /// Default 0.
    #[serde(default)]
    pub raise_cost: u32,
    /// How hard formations bend the F10 front line, as a fraction of an
    /// objective's weight. 0 = the line follows objectives only. Default 0.6.
    #[serde(default = "default_frontline_weight")]
    pub frontline_weight: f64,
    /// The front line is redrawn for formation movement at most this often.
    /// A redraw is a few hundred F10 map commands. Default 900.
    #[serde(default = "default_frontline_redraw")]
    pub frontline_redraw_secs: u32,
    /// Coalition-only F10 pins and attack arrows for each side's own
    /// formations. Default true.
    #[serde(default = "default_true")]
    pub map_pins: bool,
    /// A ring and a label on the F10 map, for both sides, wherever a battle
    /// is being fought. Default true.
    #[serde(default = "default_true")]
    pub battle_marks: bool,
    /// A column of smoke over every battle being fought in DCS, and a fire
    /// where each formation vehicle killed in DCS died. Default true.
    #[serde(default = "default_true")]
    pub smoke: bool,
    /// Most burning wrecks at once; the oldest goes out first. Default 12.
    #[serde(default = "default_max_wreck_fires")]
    pub max_wreck_fires: u32,
    /// How long a wreck burns. Default 900.
    #[serde(default = "default_wreck_fire_secs")]
    pub wreck_fire_secs: u32,
    /// Tell each side when its formations start, finish or fail an order.
    /// Default true.
    #[serde(default = "default_true")]
    pub announce: bool,
    /// Per-side display names for a formation by its make-up, e.g.
    /// `{"Blue": "Mech Coy"}`. Absent = "Armd Coy" / "Mech Coy" / "Inf Coy".
    #[serde(default)]
    pub unit_names: FxHashMap<Side, String>,
}

/// The AI ground commander.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct GroundAiCfg {
    /// Sides the AI commands. Default both.
    #[serde(default = "default_ai_sides")]
    pub sides: Vec<Side>,
    /// How often the AI reviews its formations. Default 300.
    #[serde(default = "default_ai_think")]
    pub think_secs: u32,
    /// The AI attacks objectives at most this far from ground its side holds.
    /// Default 60000 m.
    #[serde(default = "default_ai_reach")]
    pub max_attack_m: f64,
    /// Formations the AI sends against one objective. Default 2.
    #[serde(default = "default_ai_concentration")]
    pub concentration: u32,
    /// Formations the AI keeps back to counter-attack, per side. Default 1.
    #[serde(default = "default_ai_reserve")]
    pub reserve: u32,
    /// The AI only raises formations (and only attacks) while its side holds
    /// at least this many objectives. Default 3.
    #[serde(default = "default_ai_min_objectives")]
    pub min_objectives: u32,
    /// When `modern_war.tempo` is on, the AI only attacks during its side's
    /// offensive phases and aims at the offensive's axis. Default true.
    #[serde(default = "default_true")]
    pub follow_tempo: bool,
    /// No new attacks while no player is in a slot, so an empty server isn't
    /// rolled up overnight. Attacks already under way carry on, and bases
    /// under threat are still reinforced. Default true.
    #[serde(default = "default_true")]
    pub pause_when_empty: bool,
}

fn default_max_formations() -> u32 {
    6
}
fn default_max_live() -> u32 {
    6
}
fn default_groups_per_formation() -> u32 {
    3
}
fn default_keep_home() -> u32 {
    1
}
fn default_speed_kph() -> f64 {
    30.
}
fn default_bubble_m() -> f64 {
    30_000.
}
fn default_contact_m() -> f64 {
    10_000.
}
fn default_despawn_grace() -> u32 {
    300
}
fn default_arrive_m() -> f64 {
    1_500.
}
fn default_withdraw_pct() -> u8 {
    40
}
fn default_attrition_rate() -> f64 {
    0.1
}
fn default_attrition_secs() -> u32 {
    300
}
fn default_player_lock() -> u32 {
    3600
}
fn default_order_cooldown() -> u32 {
    30
}
fn default_frontline_weight() -> f64 {
    0.6
}
fn default_frontline_redraw() -> u32 {
    900
}
fn default_max_wreck_fires() -> u32 {
    12
}
fn default_wreck_fire_secs() -> u32 {
    900
}
fn default_ai_sides() -> Vec<Side> {
    vec![Side::Blue, Side::Red]
}
fn default_ai_think() -> u32 {
    300
}
fn default_ai_reach() -> f64 {
    60_000.
}
fn default_ai_concentration() -> u32 {
    2
}
fn default_ai_reserve() -> u32 {
    1
}
fn default_ai_min_objectives() -> u32 {
    3
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_block_takes_defaults() {
        let c: GroundWarCfg = serde_json::from_str("{}").unwrap();
        assert!(c.enabled);
        assert_eq!(c.max_formations_per_side, 6);
        assert_eq!(c.max_live_formations, 6);
        assert!(c.ai.is_none());
        assert!(matches!(c.command_rule, Rule::AlwaysAllowed));
        let a: GroundAiCfg = serde_json::from_str("{}").unwrap();
        assert_eq!(a.sides, vec![Side::Blue, Side::Red]);
        assert!(a.follow_tempo);
        assert!(a.pause_when_empty);
    }
}
