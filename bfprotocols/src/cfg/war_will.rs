//! War will (`Cfg::war_will`): each side's will to fight, 0-100.
//!
//! Everything that happens moves it: losses lower the side's will, and give
//! the other side `enemy_loss_share` of the same value; captures raise the
//! captor's. It recovers slowly while nothing bad happens, and drains while
//! the side's fuel stocks are low. A side whose will stays below
//! `threshold` for `hold_hours` asks for terms: the round ends and the other
//! side wins.
//!
//! All weights are here so a campaign decides how much a carrier is worth
//! against an airbase. The block absent means no will at all, which is the
//! engine's behaviour before it existed. The state machine lives in bflib
//! (`db::will`); this module holds the weights and the pure lookups.

use super::{default_true, UnitTag, UnitTags};
use crate::db::objective::ObjectiveKind;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct WarWillCfg {
    /// Off switch that keeps the block around. Default true.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Each side's will at the start of a round, 0-100. Default 70.
    #[serde(default = "default_start")]
    pub start: f64,
    /// Below this a side is breaking. Default 20.
    #[serde(default = "default_threshold")]
    pub threshold: f64,
    /// Hours a side's will must stay below `threshold` before it asks for
    /// terms and the other side wins. Campaign time. Default 6.
    #[serde(default = "default_hold_hours")]
    pub hold_hours: f64,
    /// Will regained per hour while the side has had no loss for
    /// `quiet_hours` and its fuel is not low, up to `start` (never past
    /// it). Default 1.
    #[serde(default = "default_recovery_per_hour")]
    pub recovery_per_hour: f64,
    /// How long after a loss recovery waits. Default 1.
    #[serde(default = "default_quiet_hours")]
    pub quiet_hours: f64,
    /// What a side gains from an enemy loss, as a share of what the enemy
    /// lost. Default 0.5.
    #[serde(default = "default_enemy_loss_share")]
    pub enemy_loss_share: f64,
    /// A carrier group is "damaged" when its health falls below this
    /// percentage. Default 75.
    #[serde(default = "default_carrier_damaged_pct")]
    pub carrier_damaged_pct: u8,
    /// Carrier group damaged below `carrier_damaged_pct`. Counted again only
    /// after it has been repaired above the line. Default 8.
    #[serde(default = "default_carrier_damaged")]
    pub carrier_damaged: f64,
    /// Carrier group destroyed (health 0). Default 25.
    #[serde(default = "default_carrier_destroyed")]
    pub carrier_destroyed: f64,
    /// Airbase captured by the enemy. Default 4.
    #[serde(default = "default_airbase_lost")]
    pub airbase_lost: f64,
    /// Command centre captured by the enemy, or destroyed. Default 10.
    #[serde(default = "default_command_center_lost")]
    pub command_center_lost: f64,
    /// Logistics hub captured by the enemy. Default 3.
    #[serde(default = "default_logistics_hub_lost")]
    pub logistics_hub_lost: f64,
    /// Factory captured by the enemy, or destroyed (health 0). Default 5.
    #[serde(default = "default_factory_lost")]
    pub factory_lost: f64,
    /// Any other objective captured by the enemy (FOB, FARP, naval base,
    /// SAM site, carrier group). Default 2.
    #[serde(default = "default_objective_lost")]
    pub objective_lost: f64,
    /// What the captor gains for an objective taken from the enemy, at
    /// least (it gains `enemy_loss_share` of the loser's loss if that is
    /// more). Default 2.
    #[serde(default = "default_objective_captured")]
    pub objective_captured: f64,
    /// An aircraft or helicopter lost, player or AI. Default 0.3.
    #[serde(default = "default_aircraft_lost")]
    pub aircraft_lost: f64,
    /// A bomber or other large aircraft lost (`large_aircraft_tags` or
    /// `large_aircraft_types`). Default 1.
    #[serde(default = "default_large_aircraft_lost")]
    pub large_aircraft_lost: f64,
    /// Unit tags (`unit_classification`) that make an aircraft large.
    /// Default AWACS and ALCM.
    #[serde(default = "default_large_aircraft_tags")]
    pub large_aircraft_tags: UnitTags,
    /// DCS type names that make an aircraft large: bombers, tankers,
    /// transports. Default a list of the common ones.
    #[serde(default = "default_large_aircraft_types")]
    pub large_aircraft_types: Vec<String>,
    /// A ground war formation (`ground_war`) destroyed. Default 2.
    #[serde(default = "default_formation_destroyed")]
    pub formation_destroyed: f64,
    /// A ship lost. Default 1.
    #[serde(default = "default_naval_unit_lost")]
    pub naval_unit_lost: f64,
    /// A side's fuel stock (all liquids in the warehouses of the bases it
    /// holds, against their capacity) below this percentage costs
    /// `fuel_low_per_hour`. Default 30.
    #[serde(default = "default_fuel_low_pct")]
    pub fuel_low_pct: f64,
    /// Default 1.
    #[serde(default = "default_fuel_low_per_hour")]
    pub fuel_low_per_hour: f64,
    /// Below this percentage it costs `fuel_critical_per_hour` instead.
    /// Default 10.
    #[serde(default = "default_fuel_critical_pct")]
    pub fuel_critical_pct: f64,
    /// Default 3.
    #[serde(default = "default_fuel_critical_per_hour")]
    pub fuel_critical_per_hour: f64,
}

fn default_start() -> f64 {
    70.
}
fn default_threshold() -> f64 {
    20.
}
fn default_hold_hours() -> f64 {
    6.
}
fn default_recovery_per_hour() -> f64 {
    1.
}
fn default_quiet_hours() -> f64 {
    1.
}
fn default_enemy_loss_share() -> f64 {
    0.5
}
fn default_carrier_damaged_pct() -> u8 {
    75
}
fn default_carrier_damaged() -> f64 {
    8.
}
fn default_carrier_destroyed() -> f64 {
    25.
}
fn default_airbase_lost() -> f64 {
    4.
}
fn default_command_center_lost() -> f64 {
    10.
}
fn default_logistics_hub_lost() -> f64 {
    3.
}
fn default_factory_lost() -> f64 {
    5.
}
fn default_objective_lost() -> f64 {
    2.
}
fn default_objective_captured() -> f64 {
    2.
}
fn default_aircraft_lost() -> f64 {
    0.3
}
fn default_large_aircraft_lost() -> f64 {
    1.
}
fn default_large_aircraft_tags() -> UnitTags {
    UnitTags::from(vec![UnitTag::AWACS, UnitTag::ALCM])
}
fn default_large_aircraft_types() -> Vec<String> {
    [
        "B-52H",
        "B-1B",
        "Tu-22M3",
        "Tu-95MS",
        "Tu-142",
        "Tu-160",
        "H-6J",
        "IL-76MD",
        "IL-78M",
        "A-50",
        "E-3A",
        "E-2C",
        "KJ-2000",
        "KC-135",
        "KC135MPRS",
        "KC130",
        "C-130",
        "C-17A",
        "An-26B",
        "An-30M",
    ]
    .into_iter()
    .map(String::from)
    .collect()
}
fn default_formation_destroyed() -> f64 {
    2.
}
fn default_naval_unit_lost() -> f64 {
    1.
}
fn default_fuel_low_pct() -> f64 {
    30.
}
fn default_fuel_low_per_hour() -> f64 {
    1.
}
fn default_fuel_critical_pct() -> f64 {
    10.
}
fn default_fuel_critical_per_hour() -> f64 {
    3.
}

impl Default for WarWillCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("WarWillCfg defaults")
    }
}

/// A finite weight, never negative (a bad config value counts as 0).
fn w(v: f64) -> f64 {
    if v.is_finite() {
        v.max(0.)
    } else {
        0.
    }
}

impl WarWillCfg {
    /// `start`, kept inside 0-100.
    pub fn start(&self) -> f64 {
        if self.start.is_finite() {
            self.start.clamp(0., 100.)
        } else {
            70.
        }
    }

    /// `threshold`, kept inside 0-100.
    pub fn threshold(&self) -> f64 {
        if self.threshold.is_finite() {
            self.threshold.clamp(0., 100.)
        } else {
            20.
        }
    }

    /// `hold_hours` in seconds, at least one minute.
    pub fn hold_secs(&self) -> i64 {
        let h = if self.hold_hours.is_finite() { self.hold_hours } else { 6. };
        ((h * 3600.) as i64).max(60)
    }

    /// `quiet_hours` in seconds.
    pub fn quiet_secs(&self) -> i64 {
        (w(self.quiet_hours) * 3600.) as i64
    }

    pub fn recovery_per_hour(&self) -> f64 {
        w(self.recovery_per_hour)
    }

    pub fn enemy_loss_share(&self) -> f64 {
        w(self.enemy_loss_share)
    }

    pub fn objective_captured(&self) -> f64 {
        w(self.objective_captured)
    }

    pub fn formation_destroyed(&self) -> f64 {
        w(self.formation_destroyed)
    }

    pub fn naval_unit_lost(&self) -> f64 {
        w(self.naval_unit_lost)
    }

    pub fn carrier_damaged(&self) -> f64 {
        w(self.carrier_damaged)
    }

    pub fn carrier_destroyed(&self) -> f64 {
        w(self.carrier_destroyed)
    }

    pub fn factory_lost(&self) -> f64 {
        w(self.factory_lost)
    }

    pub fn command_center_lost(&self) -> f64 {
        w(self.command_center_lost)
    }

    /// What the owner loses when an objective of `kind` is captured.
    pub fn captured_loss(&self, kind: &ObjectiveKind) -> f64 {
        w(match kind {
            ObjectiveKind::Airbase => self.airbase_lost,
            ObjectiveKind::CommandCenter => self.command_center_lost,
            ObjectiveKind::Logistics => self.logistics_hub_lost,
            ObjectiveKind::Factory { .. } => self.factory_lost,
            ObjectiveKind::Fob
            | ObjectiveKind::Farp { .. }
            | ObjectiveKind::NavalBase
            | ObjectiveKind::CarrierGroup { .. }
            | ObjectiveKind::SpecialSamSite => self.objective_lost,
        })
    }

    /// What the captor gains for taking an objective whose owner lost
    /// `loss`: `objective_captured`, or the enemy share of the loss if that
    /// is more.
    pub fn capture_gain(&self, loss: f64) -> f64 {
        self.objective_captured().max(loss * self.enemy_loss_share())
    }

    /// What losing an aircraft of type `typ` with `tags` costs.
    pub fn aircraft_loss(&self, typ: &str, tags: UnitTags) -> f64 {
        let large = tags.intersects(self.large_aircraft_tags.0)
            || self.large_aircraft_types.iter().any(|t| t.eq_ignore_ascii_case(typ));
        if large {
            w(self.large_aircraft_lost)
        } else {
            w(self.aircraft_lost)
        }
    }

    /// Will lost per hour with the side's fuel at `pct` percent of capacity
    /// (None: no warehouse fuel is tracked, so nothing). 0 when fuel is fine.
    pub fn fuel_drain_per_hour(&self, pct: Option<f64>) -> f64 {
        let Some(pct) = pct.filter(|p| p.is_finite()) else { return 0. };
        if pct < self.fuel_critical_pct {
            w(self.fuel_critical_per_hour)
        } else if pct < self.fuel_low_pct {
            w(self.fuel_low_per_hour)
        } else {
            0.
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults() {
        let c: WarWillCfg = serde_json::from_str("{}").unwrap();
        assert!(c.enabled);
        assert_eq!((c.start, c.threshold, c.hold_hours), (70., 20., 6.));
        assert_eq!(
            (c.recovery_per_hour, c.quiet_hours, c.enemy_loss_share),
            (1., 1., 0.5)
        );
        assert_eq!(c.carrier_damaged_pct, 75);
        assert_eq!((c.carrier_damaged, c.carrier_destroyed), (8., 25.));
        assert_eq!(c.captured_loss(&ObjectiveKind::Airbase), 4.);
        assert_eq!(c.captured_loss(&ObjectiveKind::CommandCenter), 10.);
        assert_eq!(c.captured_loss(&ObjectiveKind::Logistics), 3.);
        assert_eq!(
            c.captured_loss(&ObjectiveKind::Factory {
                production_rate: 0,
                last_production_ts: None
            }),
            5.
        );
        assert_eq!(c.captured_loss(&ObjectiveKind::Fob), 2.);
        assert_eq!((c.formation_destroyed, c.naval_unit_lost), (2., 1.));
        assert_eq!((c.fuel_low_pct, c.fuel_low_per_hour), (30., 1.));
        assert_eq!((c.fuel_critical_pct, c.fuel_critical_per_hour), (10., 3.));
    }

    #[test]
    fn capture_gain_is_the_larger_of_the_two() {
        let c = WarWillCfg::default();
        assert_eq!(c.capture_gain(2.), 2.);
        assert_eq!(c.capture_gain(4.), 2.);
        assert_eq!(c.capture_gain(10.), 5.);
    }

    #[test]
    fn aircraft_by_tag_and_type() {
        let c = WarWillCfg::default();
        let fighter = UnitTags::from(vec![UnitTag::Aircraft]);
        let awacs = UnitTags::from(vec![UnitTag::Aircraft, UnitTag::AWACS]);
        assert_eq!(c.aircraft_loss("F-16C_50", fighter), 0.3);
        assert_eq!(c.aircraft_loss("E-3A", awacs), 1.);
        assert_eq!(c.aircraft_loss("tu-22m3", fighter), 1.);
    }

    #[test]
    fn fuel_bands() {
        let c = WarWillCfg::default();
        assert_eq!(c.fuel_drain_per_hour(None), 0.);
        assert_eq!(c.fuel_drain_per_hour(Some(55.)), 0.);
        assert_eq!(c.fuel_drain_per_hour(Some(30.)), 0.);
        assert_eq!(c.fuel_drain_per_hour(Some(29.9)), 1.);
        assert_eq!(c.fuel_drain_per_hour(Some(9.)), 3.);
        assert_eq!(c.fuel_drain_per_hour(Some(f64::NAN)), 0.);
    }

    #[test]
    fn bad_values_are_tamed() {
        let c: WarWillCfg = serde_json::from_str(
            r#"{"start": 300, "threshold": -5, "hold_hours": 0, "airbase_lost": -4}"#,
        )
        .unwrap();
        assert_eq!(c.start(), 100.);
        assert_eq!(c.threshold(), 0.);
        assert_eq!(c.hold_secs(), 60);
        assert_eq!(c.captured_loss(&ObjectiveKind::Airbase), 0.);
    }
}
