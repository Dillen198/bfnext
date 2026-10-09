//! Configuration for the theatre HQ (`smart_commander.hq`): an AI commander
//! per coalition that reads the war the way that coalition can see it, picks
//! a posture and a main effort, and runs the war's missions and logistics
//! itself -- air packages, artillery and missile fires, convoy and helo
//! resupply, troop insertions, reinforcement convoys -- out of the side's
//! Smart Commander treasury. It points the ground war's AI at its main effort
//! and posts its asks to the tasking board for players.
//!
//! It fills gaps rather than replacing people: the more humans a side has
//! doing a job, the less the HQ spends on that job itself.
//!
//! Strategy comes from three places, the first that has an opinion winning:
//! a human commander's override (dashboard / chat), a directive from the
//! language-model strategist bfdb runs (optional), and the HQ's own rules.
//! Either of the first two can be absent and the HQ still runs the war.

use super::{default_true, AiPlaneCfg, Rule};
use dcso3::{coalition::Side, String};
use fxhash::FxHashMap;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HqCfg {
    /// Off switch that keeps the block around. Default true.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// The coalitions the HQ commands. Default both.
    #[serde(default = "default_sides")]
    pub sides: Vec<Side>,
    /// Seconds between planning passes. Default 120.
    #[serde(default = "default_think_secs")]
    pub think_secs: u32,
    /// Seconds between planning passes while one of the side's objectives is
    /// being captured. Default 30.
    #[serde(default = "default_emergency_think_secs")]
    pub emergency_think_secs: u32,
    /// Most operations the HQ starts in one planning pass. Default 2.
    #[serde(default = "default_max_new_ops")]
    pub max_new_ops_per_think: u32,
    /// Most of the side's treasury (above `smart_commander.action_reserve`)
    /// one planning pass may commit, 0..1. Default 0.4.
    #[serde(default = "default_spend_fraction")]
    pub max_spend_fraction: f64,
    /// Multiplies every op cost below. A campaign whose treasury runs in
    /// hundreds of thousands sets this instead of rewriting each cost.
    /// Default 1.
    #[serde(default = "default_one")]
    pub cost_scale: f64,
    /// Share of the treasury income the Smart Commander's passive
    /// objective funding (the point drip into damaged bases) may use while
    /// the HQ runs, 0..1. The drip otherwise takes every point of income
    /// whenever a side has damaged bases, and the HQ never gets to act.
    /// Default 0.5.
    #[serde(default = "default_funding_share")]
    pub objective_funding_share: f64,
    /// Treasury cost of each kind of operation (before `cost_scale`).
    #[serde(default)]
    pub costs: HqCostsCfg,
    /// Most of each kind the side runs at once.
    #[serde(default)]
    pub limits: HqLimitsCfg,
    /// How strike packages are put together: escorts and SEAD flying with
    /// the bombers and attack aircraft.
    #[serde(default)]
    pub packages: HqPackagesCfg,
    /// How a side's human players scale the HQ down.
    #[serde(default)]
    pub gap_fill: HqGapFillCfg,
    /// Seconds an AI air package flies before it is sent home. Default 2700.
    #[serde(default = "default_package_secs")]
    pub package_lifetime_secs: u32,
    /// The HQ's own air packages, per side: the .miz plane templates it
    /// flies CAP, strike and SEAD with, written like the plane block of a
    /// Fighters / Attackers / Sead action (`{"kind": "FixedWing", "template":
    /// "BFIGHTERS", "duration": 3600, "altitude": 6000, "altitude_typ":
    /// "BARO", "speed": 250}`). Players never see these. They are what the HQ
    /// flies first; without one it falls back to the side's own action of
    /// that kind, and without either it runs no package of that kind.
    #[serde(default)]
    pub air: FxHashMap<Side, HqAirCfg>,
    /// Names of the side's Fighters / Attackers / SEAD / Drone / Artillery /
    /// Reinforce actions the HQ may use, per side. Empty = any of the side's
    /// actions of the right kind.
    #[serde(default)]
    pub actions_blue: Vec<String>,
    #[serde(default)]
    pub actions_red: Vec<String>,
    /// AI air packages only go at targets this close to a friendly airbase.
    /// Default 180000 m.
    #[serde(default = "default_air_range")]
    pub max_air_range_m: f64,
    /// Ground and helo operations only go at objectives this close to ground
    /// the side holds. Default 60000 m.
    #[serde(default = "default_ground_range")]
    pub max_ground_range_m: f64,
    /// Resupply is wanted when an objective's supply or fuel falls below
    /// this percent. Default 60.
    #[serde(default = "default_resupply_pct")]
    pub resupply_below_pct: u8,
    /// Post the HQ's asks to the coalition tasking board (needs an AddTask
    /// action on that side to borrow the task types from). Default true.
    #[serde(default = "default_true")]
    pub post_tasks: bool,
    /// Most open tasks the HQ keeps on a side's board. Default 3.
    #[serde(default = "default_max_tasks")]
    pub max_tasks: u32,
    /// Panel message to the side when an operation launches, and when the
    /// main effort changes. Default true.
    #[serde(default = "default_true")]
    pub announce: bool,
    /// Point the ground war's AI commander at the HQ's main effort and
    /// posture (instead of the campaign tempo's axis). Default true.
    #[serde(default = "default_true")]
    pub steer_ground_war: bool,
    /// Keep planning (not just running what is under way) with nobody on
    /// the server. Default false: an empty server is not rolled up.
    #[serde(default)]
    pub run_when_empty: bool,
    /// Players' support requests (F10 > Info > HQ, `-request`).
    #[serde(default)]
    pub requests: HqRequestsCfg,
    /// Directives from bfdb's language-model strategist.
    #[serde(default)]
    pub strategist: HqStrategistCfg,
    /// Who may override the HQ -- set its posture and main effort, pause
    /// it, cancel its operations -- from the dashboard or `-hq` in chat.
    /// Server admins always may. Default: nobody else.
    #[serde(default = "default_never")]
    pub override_rule: Rule,
    /// Longest a human override stands before the HQ goes back to its own
    /// judgement, seconds. Default 7200.
    #[serde(default = "default_override_secs")]
    pub override_max_secs: u32,
}

impl Default for HqCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("HqCfg defaults")
    }
}

/// A side's air rosters: every aircraft the HQ may fly for each job, each
/// with the situations it suits. For every package the HQ picks, from the
/// entries that fit the target -- the enemy air around it, its air defence,
/// the light, the distance, what is being hit -- the most specialised one
/// (weighted at random among equals).
#[derive(Debug, Clone, Default, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HqAirCfg {
    /// Fighters: CAP and strike escorts.
    #[serde(default)]
    pub cap: Vec<HqAirTemplate>,
    /// Attack aircraft and attack helicopters.
    #[serde(default)]
    pub strike: Vec<HqAirTemplate>,
    #[serde(default)]
    pub sead: Vec<HqAirTemplate>,
}

/// One aircraft in a roster and when it is the right one to send.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HqAirTemplate {
    /// The .miz template and how it flies, as in an action's plane block.
    pub plane: AiPlaneCfg,
    /// Shown in the HQ's log and on the dashboard, e.g. "F-15C pair".
    /// Defaults to the template name.
    #[serde(default)]
    pub label: Option<String>,
    /// Relative chance among entries that fit equally well. Default 1.
    #[serde(default = "default_one")]
    pub weight: f64,
    /// Only when the side's radars hold at least / at most this many enemy
    /// aircraft within 80 km of the target. Default any.
    #[serde(default)]
    pub min_threat: u32,
    #[serde(default)]
    pub max_threat: Option<u32>,
    /// Most known air defence sites within 15 km of the target it may be
    /// sent into: 0 for an attack helicopter, unset for a jet that can
    /// handle itself. Default any.
    #[serde(default)]
    pub max_air_defence: Option<u32>,
    /// Can fly at night. Default true.
    #[serde(default = "default_true")]
    pub night: bool,
    /// Farthest from the field it launches from. Default: 120 km for a
    /// helicopter, any for a jet.
    #[serde(default)]
    pub max_range_m: Option<f64>,
    /// What it is good against: "armor" (formations and vehicles in the
    /// field), "base" (objectives), "sam" (air defence). Empty = anything.
    #[serde(default)]
    pub targets: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HqCostsCfg {
    #[serde(default = "c_cap")]
    pub cap: i64,
    #[serde(default = "c_strike")]
    pub strike: i64,
    #[serde(default = "c_sead")]
    pub sead: i64,
    #[serde(default = "c_recon")]
    pub recon: i64,
    #[serde(default = "c_artillery")]
    pub artillery: i64,
    #[serde(default = "c_missile")]
    pub missile_strike: i64,
    #[serde(default = "c_ambush")]
    pub ambush: i64,
    #[serde(default = "c_convoy")]
    pub convoy: i64,
    #[serde(default = "c_helo_supply")]
    pub helo_supply: i64,
    #[serde(default = "c_helo_troops")]
    pub helo_troops: i64,
    #[serde(default = "c_reinforce")]
    pub reinforce: i64,
    #[serde(default = "c_bomber")]
    pub bomber: i64,
    #[serde(default = "c_awacs")]
    pub awacs: i64,
    #[serde(default = "c_tanker")]
    pub tanker: i64,
    #[serde(default = "c_naval")]
    pub naval_strike: i64,
    #[serde(default = "c_air_repair")]
    pub air_repair: i64,
}

/// When a strike flies with company.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum EscortPolicy {
    /// Every time.
    Always,
    /// When the side's radars hold enemy aircraft within reach of the
    /// target or the field it launches from.
    Threatened,
    Never,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HqPackagesCfg {
    /// Fighter escort for bomber strikes. Default always.
    #[serde(default = "default_always")]
    pub escort_bombers: EscortPolicy,
    /// Fighter escort for attack-aircraft strikes. Default threatened.
    #[serde(default = "default_threatened")]
    pub escort_strikes: EscortPolicy,
    /// Escort flights per package. Default 1.
    #[serde(default = "l_one")]
    pub escort_flights: u32,
    /// Send SEAD ahead of a strike whose target has known air defence near
    /// it. Default true.
    #[serde(default = "default_true")]
    pub sead_with_strikes: bool,
    /// Escorts engage enemy aircraft this far from the flight they escort.
    /// Default 60000 m.
    #[serde(default = "default_escort_engage")]
    pub escort_engage_m: f64,
}

impl Default for HqPackagesCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("HqPackagesCfg defaults")
    }
}

fn default_always() -> EscortPolicy {
    EscortPolicy::Always
}
fn default_threatened() -> EscortPolicy {
    EscortPolicy::Threatened
}
fn default_escort_engage() -> f64 {
    60_000.
}
fn c_bomber() -> i64 {
    350
}
fn c_awacs() -> i64 {
    200
}
fn c_tanker() -> i64 {
    150
}
fn c_naval() -> i64 {
    250
}
fn c_air_repair() -> i64 {
    150
}

impl Default for HqCostsCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("HqCostsCfg defaults")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HqLimitsCfg {
    /// AI air packages up at once (CAP, strike, SEAD together). Default 3.
    #[serde(default = "l_air")]
    pub air: u32,
    /// Recon drones up at once. Default 1.
    #[serde(default = "l_one")]
    pub recon: u32,
    /// Convoys and helo supply runs the HQ has out at once. Default 3.
    #[serde(default = "l_three")]
    pub logistics: u32,
    /// Helo troop insertions in flight at once. Default 2.
    #[serde(default = "l_two")]
    pub troops: u32,
    /// Reinforcement convoys out at once. Default 1.
    #[serde(default = "l_one")]
    pub reinforce: u32,
    /// Bomber strikes in the air at once. Default 1.
    #[serde(default = "l_one")]
    pub bomber: u32,
    /// AWACS and tankers kept on station, each. Default 1.
    #[serde(default = "l_one")]
    pub awacs: u32,
    #[serde(default = "l_one")]
    pub tanker: u32,
    /// Seconds before the HQ fires on the same target again. Default 600.
    #[serde(default = "l_fires_cooldown")]
    pub fires_cooldown_secs: u32,
}

impl Default for HqLimitsCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("HqLimitsCfg defaults")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HqGapFillCfg {
    /// At or below this many humans on the side, the HQ runs at full
    /// strength. Default 2.
    #[serde(default = "l_two")]
    pub full_until_players: u32,
    /// From this many humans on the side, the HQ runs at `min_factor`.
    /// Default 16.
    #[serde(default = "g_fade")]
    pub fade_out_players: u32,
    /// The least the HQ ever does, 0..1. Default 0.25.
    #[serde(default = "g_min")]
    pub min_factor: f64,
    /// Each human already doing a job (fixed-wing in the air for the air
    /// war, helicopters for logistics and troops) cuts what the HQ spends
    /// on it by this share. Default 0.35.
    #[serde(default = "g_per_human")]
    pub per_human_cut: f64,
}

impl Default for HqGapFillCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("HqGapFillCfg defaults")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HqRequestsCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Seconds a player waits between requests. Default 300.
    #[serde(default = "default_request_cooldown")]
    pub cooldown_secs: u32,
    /// Seconds an unanswered request stays open. Default 1200.
    #[serde(default = "default_request_ttl")]
    pub ttl_secs: u32,
    /// Open requests per side. Default 6.
    #[serde(default = "default_request_max")]
    pub max_open_per_side: u32,
    /// How much a request raises the value of the operation that answers
    /// it. Default 1.6.
    #[serde(default = "default_request_bonus")]
    pub bonus: f64,
}

impl Default for HqRequestsCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("HqRequestsCfg defaults")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HqStrategistCfg {
    /// Accept directives from bfdb's strategist. Default true; bfdb only
    /// sends them when it has a language model configured.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Longest a directive stands without a fresh one, seconds. Default 2400.
    #[serde(default = "default_directive_secs")]
    pub max_directive_secs: u32,
    /// Largest weight a directive may put on a line of effort. Default 2.5.
    #[serde(default = "default_max_weight")]
    pub max_weight: f64,
}

impl Default for HqStrategistCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("HqStrategistCfg defaults")
    }
}

fn default_sides() -> Vec<Side> {
    vec![Side::Blue, Side::Red]
}
fn default_think_secs() -> u32 {
    120
}
fn default_emergency_think_secs() -> u32 {
    30
}
fn default_max_new_ops() -> u32 {
    2
}
fn default_spend_fraction() -> f64 {
    0.4
}
fn default_funding_share() -> f64 {
    0.5
}
fn default_one() -> f64 {
    1.
}
fn default_package_secs() -> u32 {
    2700
}
fn default_air_range() -> f64 {
    180_000.
}
fn default_ground_range() -> f64 {
    60_000.
}
fn default_resupply_pct() -> u8 {
    60
}
fn default_max_tasks() -> u32 {
    3
}
fn default_never() -> Rule {
    Rule::NeverAllowed
}
fn default_override_secs() -> u32 {
    7200
}
fn c_cap() -> i64 {
    250
}
fn c_strike() -> i64 {
    300
}
fn c_sead() -> i64 {
    300
}
fn c_recon() -> i64 {
    120
}
fn c_artillery() -> i64 {
    150
}
fn c_missile() -> i64 {
    225
}
fn c_ambush() -> i64 {
    100
}
fn c_convoy() -> i64 {
    80
}
fn c_helo_supply() -> i64 {
    120
}
fn c_helo_troops() -> i64 {
    180
}
fn c_reinforce() -> i64 {
    250
}
fn l_air() -> u32 {
    3
}
fn l_one() -> u32 {
    1
}
fn l_two() -> u32 {
    2
}
fn l_three() -> u32 {
    3
}
fn l_fires_cooldown() -> u32 {
    600
}
fn g_fade() -> u32 {
    16
}
fn g_min() -> f64 {
    0.25
}
fn g_per_human() -> f64 {
    0.35
}
fn default_request_cooldown() -> u32 {
    300
}
fn default_request_ttl() -> u32 {
    1200
}
fn default_request_max() -> u32 {
    6
}
fn default_request_bonus() -> f64 {
    1.6
}
fn default_directive_secs() -> u32 {
    2400
}
fn default_max_weight() -> f64 {
    2.5
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_empty_block_is_a_working_hq() {
        let c: HqCfg = serde_json::from_str("{}").unwrap();
        assert!(c.enabled);
        assert_eq!(c.sides, vec![Side::Blue, Side::Red]);
        assert_eq!(c.costs.cap, 250);
        assert_eq!(c.limits.air, 3);
        assert!(matches!(c.override_rule, Rule::NeverAllowed));
        assert!(c.strategist.enabled);
    }

    #[test]
    fn hq_air_rosters_parse() {
        let c: HqCfg = serde_json::from_str(
            r#"{"air": {"Blue": {"cap": [
                {"plane": {"kind": "FixedWing", "duration": 3600, "template": "BF16",
                   "altitude": 6000, "altitude_typ": "BARO", "speed": 250}, "max_threat": 2},
                {"plane": {"kind": "FixedWing", "duration": 3600, "template": "BF15",
                   "altitude": 8000, "altitude_typ": "BARO", "speed": 260}, "min_threat": 2, "weight": 2.5,
                 "label": "F-15C pair"}
            ]}}}"#,
        )
        .unwrap();
        let blue = c.air.get(&Side::Blue).unwrap();
        assert_eq!(blue.cap.len(), 2);
        assert_eq!(blue.cap[0].plane.template.as_str(), "BF16");
        assert_eq!(blue.cap[0].max_threat, Some(2));
        assert!(blue.cap[0].night);
        assert_eq!(blue.cap[1].weight, 2.5);
        assert!(blue.strike.is_empty());
    }
}
