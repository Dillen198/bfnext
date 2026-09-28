//! Configuration for two "keep the fight fair" systems: the emergency repair
//! crate (`Cfg::emergency_repair`) and empty-server protection
//! (`Cfg::population_scaling`). Both are off unless their block is present.
//!
//! The multipliers population scaling applies are pure functions of the
//! config and the head counts, kept here so they can be unit tested without a
//! running mission.

use super::{default_true, Crate};
use serde_derive::{Deserialize, Serialize};

/// A logistics crate that makes ONE immediate repair step at a damaged
/// friendly base, instead of waiting out the `repair_time / logi` countdown.
/// Delivered like any base-supply crate (sling, C-130 airdrop, or ground-crew
/// dynamic cargo) and unpacked inside the target base's zone.
///
/// It only works on a base that is left alone: not threatened, not under a
/// running capture timer, not consolidating after a capture, not Neutral, and
/// not the base the crate was loaded at. The repair is paid for out of the
/// target base's own stores (materiel or supply) exactly like an automatic
/// repair pulse, and restarts the base's repair countdown the same way.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct EmergencyRepairCfg {
    /// Off switch that keeps the block around. Default true.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// The crate, shared by both sides. Its name must not collide with any
    /// other crate a side can spawn -- crates are matched by name.
    #[serde(rename = "crate")]
    pub crate_def: Crate,
    /// Once an emergency repair lands at a base, another one can't for this
    /// many seconds, so a stack of crates can't be chained into an instant
    /// full rebuild. Default 600.
    #[serde(default = "default_emergency_repair_cooldown")]
    pub cooldown_secs_per_objective: u32,
    /// Points taken from the delivering player when the repair goes through
    /// (not when the crate is spawned, and not when it is refused). 0 = free.
    #[serde(default)]
    pub cost_points: u32,
}

fn default_emergency_repair_cooldown() -> u32 {
    600
}

/// Empty-server protection: while the side that owns a base has fewer than
/// `min_defenders` players in a slot, capturing that base takes longer (and,
/// optionally, the base repairs faster), so a side that is offline overnight
/// isn't rolled up unopposed. Busy servers, where both sides have players up,
/// run on the normal rules.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct PopulationScalingCfg {
    /// Off switch that keeps the block around. Default true.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// A side with fewer active players than this counts as undefended.
    /// Active = in a slot (spectators don't count). Default 1, i.e. only a
    /// side with nobody flying at all.
    #[serde(default = "default_min_defenders")]
    pub min_defenders: u32,
    /// Capture timer multiplier against an undefended side's base. Neutral
    /// bases are never scaled. Values under 1 are treated as 1. Default 2.
    #[serde(default = "default_capture_time_mult")]
    pub capture_time_mult_when_undefended: f64,
    /// When true, an undefended base's multiplier grows with how badly the
    /// attackers outnumber the defenders: max(capture_time_mult_when_undefended,
    /// attackers / max(defenders, 1)). Default false.
    #[serde(default)]
    pub scale_by_imbalance: bool,
    /// Ceiling on the capture multiplier. Default 4.
    #[serde(default = "default_max_capture_time_mult")]
    pub max_capture_time_mult: f64,
    /// Auto-repair speed multiplier for an undefended side's bases (2 = the
    /// repair pulse comes twice as often). Values under 1 are treated as 1.
    /// Default 1 (off). Keep it small: it stacks with logistics.
    #[serde(default = "default_repair_speed_mult")]
    pub repair_speed_mult_when_undefended: f64,
}

fn default_min_defenders() -> u32 {
    1
}

fn default_capture_time_mult() -> f64 {
    2.
}

fn default_max_capture_time_mult() -> f64 {
    4.
}

fn default_repair_speed_mult() -> f64 {
    1.
}

impl Default for PopulationScalingCfg {
    fn default() -> Self {
        Self {
            enabled: true,
            min_defenders: default_min_defenders(),
            capture_time_mult_when_undefended: default_capture_time_mult(),
            scale_by_imbalance: false,
            max_capture_time_mult: default_max_capture_time_mult(),
            repair_speed_mult_when_undefended: default_repair_speed_mult(),
        }
    }
}

/// A multiplier from config that must never shrink anything: NaN, negative
/// and sub-1 values all mean "no effect".
fn at_least_one(m: f64) -> f64 {
    if m.is_finite() && m > 1. { m } else { 1. }
}

impl PopulationScalingCfg {
    /// Is a side with `active` players in a slot undefended?
    pub fn undefended(&self, active: u32) -> bool {
        self.enabled && active < self.min_defenders
    }

    /// How many times longer a capture of a base takes when its owner has
    /// `defenders` active players and the capturing side has `attackers`.
    /// 1.0 whenever the owner is defended (or the feature is off). The caller
    /// is responsible for not scaling Neutral bases.
    pub fn capture_time_mult(&self, defenders: u32, attackers: u32) -> f64 {
        if !self.undefended(defenders) {
            return 1.;
        }
        let base = at_least_one(self.capture_time_mult_when_undefended);
        let mult = if self.scale_by_imbalance {
            base.max(attackers as f64 / defenders.max(1) as f64)
        } else {
            base
        };
        // The cap can only lower the multiplier, never push it under 1.
        mult.min(at_least_one(self.max_capture_time_mult))
    }

    /// How many times faster an undefended side's bases self-repair.
    pub fn repair_speed_mult(&self, owner_active: u32) -> f64 {
        if self.undefended(owner_active) {
            at_least_one(self.repair_speed_mult_when_undefended)
        } else {
            1.
        }
    }
}

/// "2x", "2.5x" -- for player messages.
pub fn fmt_mult(m: f64) -> std::string::String {
    let r = (m * 10.).round() / 10.;
    if (r - r.trunc()).abs() < 1e-9 {
        format!("{}x", r as i64)
    } else {
        format!("{r:.1}x")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defended_side_keeps_normal_rules() {
        let c = PopulationScalingCfg::default();
        assert_eq!(c.capture_time_mult(1, 20), 1.);
        assert_eq!(c.capture_time_mult(5, 5), 1.);
        assert_eq!(c.repair_speed_mult(1), 1.);
    }

    #[test]
    fn undefended_side_gets_the_flat_multiplier() {
        let c = PopulationScalingCfg::default();
        assert_eq!(c.capture_time_mult(0, 0), 2.);
        assert_eq!(c.capture_time_mult(0, 12), 2.);
        let c = PopulationScalingCfg { min_defenders: 3, ..Default::default() };
        assert_eq!(c.capture_time_mult(2, 1), 2.);
        assert_eq!(c.capture_time_mult(3, 1), 1.);
    }

    #[test]
    fn imbalance_scaling_is_floored_by_base_and_capped() {
        let c = PopulationScalingCfg {
            min_defenders: 3,
            scale_by_imbalance: true,
            ..Default::default()
        };
        // 1 attacker vs nobody: the base multiplier still applies.
        assert_eq!(c.capture_time_mult(0, 1), 2.);
        // 3 attackers vs nobody: 3x.
        assert_eq!(c.capture_time_mult(0, 3), 3.);
        // 6 vs 2: 3x.
        assert_eq!(c.capture_time_mult(2, 6), 3.);
        // 10 vs nobody: capped at 4x.
        assert_eq!(c.capture_time_mult(0, 10), 4.);
    }

    #[test]
    fn nonsense_multipliers_never_speed_a_capture_up() {
        let c = PopulationScalingCfg {
            capture_time_mult_when_undefended: 0.5,
            max_capture_time_mult: 0.,
            repair_speed_mult_when_undefended: -3.,
            ..Default::default()
        };
        assert_eq!(c.capture_time_mult(0, 0), 1.);
        assert_eq!(c.repair_speed_mult(0), 1.);
        let c = PopulationScalingCfg {
            capture_time_mult_when_undefended: f64::NAN,
            ..Default::default()
        };
        assert_eq!(c.capture_time_mult(0, 0), 1.);
    }

    #[test]
    fn disabled_block_is_inert() {
        let c = PopulationScalingCfg {
            enabled: false,
            repair_speed_mult_when_undefended: 2.,
            ..Default::default()
        };
        assert_eq!(c.capture_time_mult(0, 10), 1.);
        assert_eq!(c.repair_speed_mult(0), 1.);
    }

    #[test]
    fn repair_speed_up_only_when_undefended() {
        let c = PopulationScalingCfg {
            repair_speed_mult_when_undefended: 1.5,
            ..Default::default()
        };
        assert_eq!(c.repair_speed_mult(0), 1.5);
        assert_eq!(c.repair_speed_mult(1), 1.);
    }

    #[test]
    fn empty_blocks_take_defaults() {
        let p: PopulationScalingCfg = serde_json::from_value(serde_json::json!({})).unwrap();
        assert!(p.enabled);
        assert_eq!(p.min_defenders, 1);
        assert_eq!(p.capture_time_mult_when_undefended, 2.);
        assert_eq!(p.max_capture_time_mult, 4.);
        assert_eq!(p.repair_speed_mult_when_undefended, 1.);
        let e: EmergencyRepairCfg = serde_json::from_value(serde_json::json!({
            "crate": {
                "name": "Emergency Repair", "weight": 1000, "required": 1,
                "pos_unit": null, "max_drop_height_agl": 10, "max_drop_speed": 13
            }
        }))
        .unwrap();
        assert!(e.enabled);
        assert_eq!(e.cooldown_secs_per_objective, 600);
        assert_eq!(e.cost_points, 0);
        assert_eq!(e.crate_def.name.as_str(), "Emergency Repair");
    }

    #[test]
    fn emergency_crate_name_must_be_unique() {
        let mut cfg = super::super::Cfg::default();
        let taken = cfg.repair_crate.values().next().expect("default repair crate").clone();
        let mut own = taken.clone();
        own.name = "Emergency Repair".into();
        cfg.emergency_repair = Some(EmergencyRepairCfg {
            enabled: true,
            crate_def: own,
            cooldown_secs_per_objective: 600,
            cost_points: 0,
        });
        cfg.validate().unwrap();
        if let Some(er) = cfg.emergency_repair.as_mut() {
            er.crate_def.name = taken.name.clone();
        }
        assert!(cfg.validate().is_err());
    }

    #[test]
    fn mult_formatting() {
        assert_eq!(fmt_mult(2.), "2x");
        assert_eq!(fmt_mult(2.5), "2.5x");
        assert_eq!(fmt_mult(3.333), "3.3x");
    }
}
