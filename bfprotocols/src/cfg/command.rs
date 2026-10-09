//! Who may command (`Cfg::command`).
//!
//! Commanding -- ground formations, the theatre HQ's plan, the command map's
//! orders to air, logistics and naval assets -- is earned. A pilot's rank
//! comes from their campaign score (the leaderboard's, all servers, all
//! time); reaching `commander_rank` makes them a commander of whichever
//! coalition they are on. Dashboard admins can grant it to anyone or take it
//! from anyone, whatever their rank.
//!
//! bfdb works out who is a commander (it has the scores) and tells the engine
//! (`set-commanders`), which gates the in-game F10 and chat orders the same
//! way. Until bfdb has told it, the engine falls back to the old rules so a
//! server without bfdb isn't locked out of its own ground war.

use super::default_true;
use serde_derive::{Deserialize, Serialize};

/// The rank ladder: the campaign score each tier starts at. Tier 1 is 0.
/// Mirrors `bfweb/src/ranks.ts`.
pub const RANK_MIN_SCORE: [f64; 8] = [0., 10., 25., 50., 100., 200., 400., 800.];

/// The rank tier (1-8) a campaign score has earned.
pub fn rank_tier(score: f64) -> u8 {
    RANK_MIN_SCORE.iter().rposition(|m| score >= *m).map_or(1, |i| i as u8 + 1)
}

/// The score a tier starts at (tiers outside 1-8 are clamped).
pub fn rank_min_score(tier: u8) -> f64 {
    RANK_MIN_SCORE[(tier.clamp(1, 8) - 1) as usize]
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CommandCfg {
    /// Orders need a commander: ground formations from the F10 menu or the
    /// dashboard, the theatre HQ's override, and the command map. Off = any
    /// pilot registered on a side may order the ground war, as before (the
    /// HQ override still needs `smart_commander.hq.override_rule`). Default
    /// true.
    #[serde(default = "default_true")]
    pub require_commander: bool,
    /// The rank tier (1-8) that makes a pilot a commander. Default 4 (Major,
    /// a campaign score of 50).
    #[serde(default = "default_commander_rank")]
    pub commander_rank: u8,
    /// Naval hunter groups a commander can send after the enemy fleet: real
    /// DCS ships that sail out from a friendly naval base or carrier group
    /// and fire their own anti-ship missiles at what they find.
    #[serde(default)]
    pub hunters: HuntersCfg,
    /// How far from one of our bases a deployment by road may go, metres.
    /// Default 25000.
    #[serde(default = "default_deploy_range_m")]
    pub deploy_range_m: f64,
}

/// `CommandCfg::hunters`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HuntersCfg {
    /// DCS ship types of a Red hunter group, one unit each. Default a
    /// Type 093 submarine (it fires anti-ship missiles in DCS).
    #[serde(default = "default_hunters_red")]
    pub red: Vec<String>,
    /// Blue's: DCS has no modern Western submarine, so a surface action
    /// group. Default two Arleigh Burke IIa destroyers.
    #[serde(default = "default_hunters_blue")]
    pub blue: Vec<String>,
    /// Treasury points before the HQ's cost scale. Default 300.
    #[serde(default = "default_hunter_cost")]
    pub cost: i64,
    /// Hunter groups at sea per side. Default 2.
    #[serde(default = "default_hunter_max")]
    pub max_per_side: u8,
    /// Knots. Default 18.
    #[serde(default = "default_hunter_kts")]
    pub speed_kts: f64,
    /// Farthest from the base it sails from, metres. Default 400000.
    #[serde(default = "default_hunter_range_m")]
    pub range_m: f64,
    /// Seconds at sea before it heads home and leaves. Default 5400.
    #[serde(default = "default_hunter_lifetime_secs")]
    pub lifetime_secs: u32,
}

fn default_commander_rank() -> u8 {
    4
}
fn default_deploy_range_m() -> f64 {
    25_000.
}
fn default_hunters_red() -> Vec<String> {
    vec!["Type_093".into()]
}
fn default_hunters_blue() -> Vec<String> {
    vec!["USS_Arleigh_Burke_IIa".into(), "USS_Arleigh_Burke_IIa".into()]
}
fn default_hunter_cost() -> i64 {
    300
}
fn default_hunter_max() -> u8 {
    2
}
fn default_hunter_kts() -> f64 {
    18.
}
fn default_hunter_range_m() -> f64 {
    400_000.
}
fn default_hunter_lifetime_secs() -> u32 {
    5_400
}

impl Default for HuntersCfg {
    fn default() -> Self {
        Self {
            red: default_hunters_red(),
            blue: default_hunters_blue(),
            cost: default_hunter_cost(),
            max_per_side: default_hunter_max(),
            speed_kts: default_hunter_kts(),
            range_m: default_hunter_range_m(),
            lifetime_secs: default_hunter_lifetime_secs(),
        }
    }
}

impl Default for CommandCfg {
    fn default() -> Self {
        Self {
            require_commander: true,
            commander_rank: default_commander_rank(),
            hunters: HuntersCfg::default(),
            deploy_range_m: default_deploy_range_m(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tiers() {
        assert_eq!(rank_tier(-5.), 1);
        assert_eq!(rank_tier(0.), 1);
        assert_eq!(rank_tier(9.9), 1);
        assert_eq!(rank_tier(10.), 2);
        assert_eq!(rank_tier(50.), 4);
        assert_eq!(rank_tier(99.), 4);
        assert_eq!(rank_tier(800.), 8);
        assert_eq!(rank_tier(1e9), 8);
        assert_eq!(rank_min_score(4), 50.);
        assert_eq!(rank_min_score(0), 0.);
        assert_eq!(rank_min_score(12), 800.);
    }

    #[test]
    fn defaults() {
        let c: CommandCfg = serde_json::from_str("{}").unwrap();
        assert!(c.require_commander);
        assert_eq!(c.commander_rank, 4);
    }
}
