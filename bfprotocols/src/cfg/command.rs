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
}

fn default_commander_rank() -> u8 {
    4
}

impl Default for CommandCfg {
    fn default() -> Self {
        Self { require_commander: true, commander_rank: default_commander_rank() }
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
