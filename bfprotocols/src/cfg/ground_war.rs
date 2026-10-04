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
    /// How the fighting on the map works: firepower by vehicle type, supply,
    /// morale, deployment, spotting. See `GroundCombatCfg`.
    #[serde(default)]
    pub combat: GroundCombatCfg,
}

/// The ground war's combat model, for formations the server is not
/// simulating in DCS (and for what a formation carries and sees either way).
///
/// - **Firepower** depends on what a formation is made of: a tank is worth
///   several trucks, and infantry are hard to kill but hit little at range.
/// - **Speed** is the slowest vehicle's, on road or across country.
/// - **Supply** (fuel and ammunition) runs down on the march and in a fight,
///   and is made good only near a friendly base that has supply itself. Out
///   of supply a formation fights at a fraction of its power and crawls.
/// - **Morale** falls with losses and isolation. A formation whose morale
///   breaks falls back to the nearest friendly base, whatever its orders.
/// - **Deployment**: a column that runs into the enemy has to stop and
///   deploy into line before it can fight properly, and a formation that
///   has held a position for a while digs in.
/// - **Spotting**: enemy formations are only seen within `spot_m` of our
///   formations and bases (less if they are dug in), and are remembered where
///   they were last seen.
/// - **Garrisons fight**: an attack on a base fights its garrison, dug in,
///   and the garrison's losses are the base's.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct GroundCombatCfg {
    /// Two forces this close exchange fire. Default 3000 m.
    #[serde(default = "default_engage_m")]
    pub engage_m: f64,
    /// Enemy formations are seen this far from ours (and 3/4 of it from our
    /// bases). Default 7000 m.
    #[serde(default = "default_spot_m")]
    pub spot_m: f64,
    /// An enemy formation out of sight is remembered where it was last seen
    /// for this long. Default 900.
    #[serde(default = "default_remember_secs")]
    pub remember_secs: u32,
    /// Seconds between rounds of fighting on the map. Default 60.
    #[serde(default = "default_combat_secs")]
    pub combat_secs: u32,
    /// Damage dealt per point of firepower per minute. Higher is bloodier
    /// and quicker. Default 0.006: two equal tank companies fight for about
    /// half an hour before one breaks.
    #[serde(default = "default_lethality")]
    pub lethality: f64,
    /// A garrison fights from prepared positions: damage it takes is divided
    /// by this. Default 1.6.
    #[serde(default = "default_garrison_cover")]
    pub garrison_cover: f64,
    /// Same for a dug-in formation. Default 1.4.
    #[serde(default = "default_dug_in_cover")]
    pub dug_in_cover: f64,
    /// A column caught on the march fights at this share of its power until
    /// it has deployed. Default 0.6.
    #[serde(default = "default_column_power")]
    pub column_power: f64,
    /// Seconds a column takes to deploy into line on contact. Default 90.
    #[serde(default = "default_deploy_secs")]
    pub deploy_secs: u32,
    /// Seconds holding a position before a formation is dug in. Default 600.
    #[serde(default = "default_dig_in_secs")]
    pub dig_in_secs: u32,
    /// A deployed formation advancing in contact moves at this share of its
    /// march speed. Default 0.35.
    #[serde(default = "default_contact_speed")]
    pub contact_speed: f64,
    /// A formation within this of a friendly base with supply of its own is
    /// in supply. Default 25000 m.
    #[serde(default = "default_supply_range")]
    pub supply_range_m: f64,
    /// The base has to have at least this much supply (percent) to keep a
    /// formation supplied. Default 25.
    #[serde(default = "default_supply_base_pct")]
    pub supply_base_pct: u8,
    /// Share of its supply a formation uses per 100 km driven. Default 0.25.
    #[serde(default = "default_supply_per_100km")]
    pub supply_per_100km: f64,
    /// Share of its supply used per minute of fighting. Default 0.03.
    #[serde(default = "default_supply_per_combat_min")]
    pub supply_per_combat_min: f64,
    /// Share of its supply made good per minute while in supply and not
    /// fighting. Default 0.05.
    #[serde(default = "default_resupply_per_min")]
    pub resupply_per_min: f64,
    /// Below this supply (0..1) a formation is out of fuel and ammunition:
    /// half speed. Default 0.15.
    #[serde(default = "default_low_supply")]
    pub low_supply: f64,
    /// Morale lost per share of its firepower lost. Default 1.5 (half its
    /// power gone = 0.75 morale lost).
    #[serde(default = "default_morale_per_loss")]
    pub morale_per_loss: f64,
    /// Morale (0..1) below which a formation breaks and falls back. Default
    /// 0.25.
    #[serde(default = "default_break_morale")]
    pub break_morale: f64,
    /// Morale regained per minute out of contact and in supply. Default 0.01.
    #[serde(default = "default_morale_recovery")]
    pub morale_recovery_per_min: f64,
    /// Firepower per vehicle, by role. Unlisted roles take the built-in
    /// values (tank 10, ifv 6, apc 3, artillery 5, aaa 2, sam 1, infantry
    /// 1, truck 0.2).
    #[serde(default)]
    pub firepower: FxHashMap<String, f64>,
    /// Resupply comes out of the base's warehouse: share of the base's
    /// stock one vehicle's full load costs. Default 0.0015 (a 15-vehicle
    /// company refilling from empty takes about 2% of a base's stores).
    /// With the materiel commodity on, `materiel_per_vehicle` units instead.
    #[serde(default = "default_base_drain")]
    pub base_drain_per_vehicle: f64,
    #[serde(default = "default_materiel_per_vehicle")]
    pub materiel_per_vehicle: f64,
    /// An enemy formation this close to the road between a formation and
    /// its supplying base cuts that supply line (as does an enemy base on
    /// it). Default 3000 m.
    #[serde(default = "default_line_cut")]
    pub supply_line_cut_m: f64,
    /// Friendly artillery within range fires in support of formations in
    /// contact: on the map as part of the fighting, and as real fire
    /// missions on enemies the server is simulating in DCS. Default true.
    #[serde(default = "default_true")]
    pub artillery_support: bool,
    /// Range of supporting artillery when the campaign's `artillery` block
    /// doesn't say. Default 25000 m.
    #[serde(default = "default_arty_support_m")]
    pub artillery_support_m: f64,
    /// Share of the daytime spotting range left at night (thermal sights
    /// and flares help, but not much). Default 0.45.
    #[serde(default = "default_night_spot")]
    pub night_spot: f64,
    /// Our aircraft in the air below `air_spot_agl_m` see enemy ground
    /// formations within this much. Default 10000 m.
    #[serde(default = "default_air_spot")]
    pub air_spot_m: f64,
    #[serde(default = "default_air_spot_agl")]
    pub air_spot_agl_m: f64,
    /// Check line of sight over the terrain between observer and target.
    /// Default true.
    #[serde(default = "default_true")]
    pub line_of_sight: bool,
}

impl Default for GroundCombatCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("GroundCombatCfg defaults")
    }
}

fn default_engage_m() -> f64 {
    3_000.
}
fn default_spot_m() -> f64 {
    7_000.
}
fn default_remember_secs() -> u32 {
    900
}
fn default_combat_secs() -> u32 {
    60
}
fn default_lethality() -> f64 {
    0.006
}
fn default_garrison_cover() -> f64 {
    1.6
}
fn default_dug_in_cover() -> f64 {
    1.4
}
fn default_column_power() -> f64 {
    0.6
}
fn default_deploy_secs() -> u32 {
    90
}
fn default_dig_in_secs() -> u32 {
    600
}
fn default_contact_speed() -> f64 {
    0.35
}
fn default_supply_range() -> f64 {
    25_000.
}
fn default_supply_base_pct() -> u8 {
    25
}
fn default_supply_per_100km() -> f64 {
    0.25
}
fn default_supply_per_combat_min() -> f64 {
    0.03
}
fn default_resupply_per_min() -> f64 {
    0.05
}
fn default_low_supply() -> f64 {
    0.15
}
fn default_morale_per_loss() -> f64 {
    1.5
}
fn default_break_morale() -> f64 {
    0.25
}
fn default_morale_recovery() -> f64 {
    0.01
}
fn default_base_drain() -> f64 {
    0.0015
}
fn default_materiel_per_vehicle() -> f64 {
    2.
}
fn default_line_cut() -> f64 {
    3_000.
}
fn default_arty_support_m() -> f64 {
    25_000.
}
fn default_night_spot() -> f64 {
    0.45
}
fn default_air_spot() -> f64 {
    10_000.
}
fn default_air_spot_agl() -> f64 {
    3_000.
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
        assert_eq!(c.combat.engage_m, 3_000.);
        assert!(c.combat.break_morale > 0. && c.combat.break_morale < 1.);
    }
}
