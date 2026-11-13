//! AI fill-in for a PvP server (`Cfg::ai_fill`): how hard each side's AI
//! fights, from both sides' head counts.
//!
//! Without it the AI looks at its own side only (the HQ's `gap_fill`), so a
//! side with nobody in it runs at full strength whether the other side has
//! one pilot or thirty. With it, each side's AI pressure is
//!
//! ```text
//! own_gap(own pilots) * (floor + (1 - floor) * min(enemy pilots / full_at_enemy, 1))
//! ```
//!
//! where `own_gap` is the HQ's own-side fade (`smart_commander.hq.gap_fill`):
//! a side steps back as its own players arrive, and steps up as the other
//! side's do. One lone pilot meets an enemy AI their size; a full server is
//! decided by people.
//!
//! The rule is a pure function of the config and the head counts, here so it
//! can be tested without a mission. bflib's `ai_fill` counts the heads and
//! applies it to the HQ, the AI packages and the ground war's AI.

use super::{default_true, HqGapFillCfg};
use serde_derive::{Deserialize, Serialize};

/// Below this pressure the AI starts nothing new.
pub const AI_FILL_MIN_PRESSURE: f64 = 0.05;

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct AiFillCfg {
    /// Off switch that keeps the block around. Default true.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// The share of a full AI a side runs while the other side has nobody
    /// in a slot, 0..1. Default 0.35.
    #[serde(default = "default_floor")]
    pub floor: f64,
    /// From this many enemy pilots in a slot, the enemy match is 1 (only the
    /// side's own players hold its AI back). Default 8.
    #[serde(default = "default_full_at_enemy")]
    pub full_at_enemy: u32,
    /// Scale the theatre HQ (`smart_commander.hq`): its candidate scores,
    /// the share of the treasury a pass may spend and the new operations a
    /// pass may start. Default true.
    #[serde(default = "default_true")]
    pub hq: bool,
    /// Scale air_life's AI packages (`modern_war.air_life.packages`): the
    /// launch interval is divided by the pressure and `max_active_per_side`
    /// multiplied by it. Default true.
    #[serde(default = "default_true")]
    pub packages: bool,
    /// Scale the ground war AI's new attacks (`ground_war.ai`): waves of
    /// attacks are spaced `think_secs / pressure` apart. Defence,
    /// counter-attacks and resupply are not scaled. Default true.
    #[serde(default = "default_true")]
    pub ground_ai: bool,
    /// Empty-side protection (`population_scaling`) counts the AI as a
    /// defender: a side with too few pilots in a slot is not undefended
    /// while the other side has anyone in a slot, so a lone pilot captures
    /// at normal speed. With nobody in a slot on either side the protection
    /// still applies. Default true.
    #[serde(default = "default_true")]
    pub protection: bool,
}

fn default_floor() -> f64 {
    0.35
}

fn default_full_at_enemy() -> u32 {
    8
}

impl Default for AiFillCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("AiFillCfg defaults")
    }
}

impl AiFillCfg {
    /// How much of a full AI the other side's `enemy` pilots call for, 0..1:
    /// `floor` with nobody there, 1 from `full_at_enemy`. 1 with the block
    /// switched off.
    pub fn enemy_match(&self, enemy: u32) -> f64 {
        if !self.enabled {
            return 1.;
        }
        let floor = if self.floor.is_finite() { self.floor.clamp(0., 1.) } else { 1. };
        let t = (enemy as f64 / self.full_at_enemy.max(1) as f64).min(1.);
        floor + (1. - floor) * t
    }

    /// A side's AI pressure, 0..1, with `own` pilots of its own and `enemy`
    /// on the other side: the own-side fade times the enemy match.
    pub fn pressure(&self, gap: &HqGapFillCfg, own: u32, enemy: u32) -> f64 {
        gap.factor(own) * self.enemy_match(enemy)
    }

    /// Is a side with `own` pilots in a slot run mostly by its AI? True
    /// while its own players don't hold the AI back at all.
    pub fn ai_led(gap: &HqGapFillCfg, own: u32) -> bool {
        own <= gap.full_until_players
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn near(a: f64, b: f64) -> bool {
        (a - b).abs() < 1e-3
    }

    /// (blue, red) -> (blue pressure, red pressure) with every default.
    fn both(blue: u32, red: u32) -> (f64, f64) {
        let c = AiFillCfg::default();
        let g = HqGapFillCfg::default();
        (c.pressure(&g, blue, red), c.pressure(&g, red, blue))
    }

    #[test]
    fn defaults() {
        let c: AiFillCfg = serde_json::from_str("{}").unwrap();
        assert!(c.enabled && c.hq && c.packages && c.ground_ai && c.protection);
        assert_eq!(c.floor, 0.35);
        assert_eq!(c.full_at_enemy, 8);
    }

    #[test]
    fn nobody_on_either_side() {
        // The floor; the empty-server pauses are separate and still hold.
        let (b, r) = both(0, 0);
        assert!(near(b, 0.35) && near(r, 0.35), "{b} {r}");
    }

    #[test]
    fn one_blue_pilot_against_nobody() {
        let (b, r) = both(1, 0);
        // Red's AI is matched to one pilot, well short of a full HQ.
        assert!(near(r, 0.35 + 0.65 / 8.), "{r}");
        assert!(r > 0.4 && r < 0.45);
        // Blue's AI helps its pilot, at about the same weight.
        assert!(near(b, 0.35), "{b}");
    }

    #[test]
    fn eight_blue_against_nobody_meets_a_full_red() {
        let (b, r) = both(8, 0);
        assert!(near(r, 1.), "{r}");
        // Blue's own eight hold its AI back, and Red has no players to match.
        assert!(near(b, (1. - 6. / 14. * 0.75) * 0.35), "{b}");
    }

    #[test]
    fn ten_against_eight_both_stand_back() {
        let (b, r) = both(10, 8);
        let g = HqGapFillCfg::default();
        // Enemy match is full both ways: only the own-side fade is left.
        assert!(near(b, g.factor(10)) && near(r, g.factor(8)), "{b} {r}");
        assert!(b < 0.6 && r < 0.7);
    }

    #[test]
    fn twenty_against_three_fills_the_gap() {
        let (b, r) = both(20, 3);
        assert!(near(r, 1. - 1. / 14. * 0.75), "{r}");
        assert!(r > 0.9);
        assert!(near(b, 0.25 * (0.35 + 0.65 * 3. / 8.)), "{b}");
        assert!(b < 0.2);
    }

    #[test]
    fn switched_off_is_the_own_side_fade_alone() {
        let c = AiFillCfg { enabled: false, ..Default::default() };
        let g = HqGapFillCfg::default();
        for (own, enemy) in [(0, 0), (1, 0), (8, 0), (10, 8), (20, 3)] {
            assert_eq!(c.enemy_match(enemy), 1.);
            assert_eq!(c.pressure(&g, own, enemy), g.factor(own));
        }
    }

    #[test]
    fn bad_config_is_tamed() {
        let c = AiFillCfg { floor: f64::NAN, full_at_enemy: 0, ..Default::default() };
        assert_eq!(c.enemy_match(0), 1.);
        let c = AiFillCfg { floor: -1., full_at_enemy: 0, ..Default::default() };
        assert_eq!(c.enemy_match(0), 0.);
        assert_eq!(c.enemy_match(1), 1.);
        let c = AiFillCfg { floor: 3., ..Default::default() };
        assert_eq!(c.enemy_match(0), 1.);
    }

    #[test]
    fn ai_led_until_the_side_has_its_own_pilots() {
        let g = HqGapFillCfg::default();
        assert!(AiFillCfg::ai_led(&g, 0));
        assert!(AiFillCfg::ai_led(&g, 2));
        assert!(!AiFillCfg::ai_led(&g, 3));
    }
}
