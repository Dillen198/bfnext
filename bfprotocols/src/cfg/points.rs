/*
Copyright 2024 Eric Stokes.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
for more details.
*/

//! Points economy and lifelines.

use super::*;

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct PointsCfg {
    /// Bonus issued to new players when they register
    pub new_player_join: u32,
    /// Points awarded for each air kill
    pub air_kill: u32,
    /// Base points awared for each ground kill
    pub ground_kill: u32,
    /// Bonus points awarded to heavy sam kills
    pub lr_sam_bonus: u32,
    /// Points awarded for repairing base logistics
    pub logistics_repair: u32,
    /// Points awarded for logistics transfers
    pub logistics_transfer: u32,
    /// Points awarded for base capture
    pub capture: u32,
    /// How many hours before previous team kills are forgotten for
    /// the purposes of computing the penalty of a team kill.
    #[serde(default = "default_tk_window")]
    pub tk_window: u32,
    /// If provisional is true then points earned in a sortie are only
    /// committed to the player's points balance when they land at a
    /// friendly objective
    #[serde(default)]
    pub provisional: bool,
    /// If strict is true then the player cannot take off when their
    /// loadout or airframe costs more points than they have. They
    /// will be deleted on takeoff, and no points or lives will be
    /// deducted. If struct is false then the player's points will go
    /// negative if they take off with an airframe/loadout that
    /// exceeds their current balance.
    #[serde(default)]
    pub strict: bool,
    /// How many points does it cost to slot in a given airframe. This
    /// need not cover all airframes on the server, and the default is 0.
    #[serde(default)]
    pub airframe_cost: FxHashMap<Vehicle, u32>,
    /// How many points does it cost to load a given weapon. This need
    /// not cover all weapons, and the default is zero.
    #[serde(default)]
    pub weapon_cost: FxHashMap<String, u32>,
    /// How many points do players in a slot (not spectators) automatically gain per
    /// time interval. This is a pair of the number of points with the
    /// interval in seconds. The number of points CAN be negative, the
    /// interval must be positive. The default is (0, 0)
    #[serde(default)]
    pub periodic_point_gain: (i32, u32),
    /// Whether to award points for kills. If false, no points are awarded
    /// for air or ground kills. Default is true.
    #[serde(default = "default_true")]
    pub award_kill_points: bool,
    /// Points awarded for destroying an enemy supply convoy unit. Default: 10.
    #[serde(default = "default_convoy_interdiction")]
    pub convoy_interdiction_points: u32,
    /// Kill streak bonus thresholds. Each entry is (minimum_streak, bonus_multiplier).
    /// e.g. [(3, 1.5), (5, 2.0)] means after 3 kills in a single sortie the base points
    /// are multiplied by 1.5, and after 5 kills by 2.0. The streak counts
    /// kills since the pilot's last takeoff.
    /// Default: empty (no bonus).
    #[serde(default)]
    pub kill_streak_bonuses: Vec<(u8, f64)>,
    /// Keeps a pilot who has run out of points flying. Absent = off. See
    /// `LifelineCfg`.
    #[serde(default)]
    pub lifeline: Option<LifelineCfg>,
}

impl PointsCfg {
    /// Whether a pilot with `points` of their own flies `typ` on the
    /// lifeline: free airframe, stores free up to the budget.
    pub fn lifeline_applies(&self, points: i32, typ: &Vehicle) -> bool {
        let Some(l) = &self.lifeline else { return false };
        if !l.airframes.is_empty() && !l.airframes.contains(typ) {
            return false;
        }
        let cost = self.airframe_cost.get(typ).copied().unwrap_or(0);
        if l.max_airframe_cost.is_some_and(|max| cost > max) {
            return false;
        }
        let below = l
            .below
            .unwrap_or_else(|| self.airframe_cost.get(typ).copied().unwrap_or(0) as i32);
        points < below
    }

    /// What a lifeline flight still charges for `stores` points of weapons:
    /// whatever is over the free budget.
    pub fn lifeline_store_charge(&self, stores: u32) -> u32 {
        match self.lifeline.as_ref().and_then(|l| l.store_budget) {
            None => 0,
            Some(budget) => stores.saturating_sub(budget),
        }
    }
}

/// Keeps a pilot who has spent everything in the air. With strict points, a
/// pilot who burned through their balance could fly nothing but the free
/// transports -- whatever modules they own -- and a pilot who doesn't do
/// logistics then has no way to earn anything back. On the lifeline the
/// airframe costs nothing and a small loadout (`store_budget`) is free, so
/// they fly the aircraft they have, just not with a full rack of the
/// expensive stuff, until they have earned their way back above `below`.
#[derive(Debug, Clone, Default, Serialize, Deserialize, schemars::JsonSchema)]
pub struct LifelineCfg {
    /// Aircraft types it covers (DCS type names). Empty = every airframe.
    #[serde(default)]
    pub airframes: Vec<Vehicle>,
    /// The lifeline applies while the pilot's OWN points are below this --
    /// base funds don't count, it is the pilot who is broke. Unset: whenever
    /// their own points can't cover that airframe's cost.
    #[serde(default)]
    pub below: Option<i32>,
    /// Points' worth of weapons a lifeline flight carries free; anything
    /// over it is charged as usual (so under strict points: unload it or
    /// don't take off). Unset = all weapons free. Guns and unpriced stores
    /// are always free.
    #[serde(default)]
    pub store_budget: Option<u32>,
    /// Airframes priced above this are never covered. Campaigns price an
    /// airframe out of the era (99999) rather than removing it, and a free
    /// ride would walk straight past that. Unset = no cap.
    #[serde(default)]
    pub max_airframe_cost: Option<u32>,
}

#[cfg(test)]
mod lifeline_tests {
    use super::*;

    fn points(below: Option<i32>) -> PointsCfg {
        let mut airframe_cost = FxHashMap::default();
        airframe_cost.insert(Vehicle::from("Su-25T"), 560);
        PointsCfg {
            new_player_join: 0,
            air_kill: 0,
            ground_kill: 0,
            lr_sam_bonus: 0,
            logistics_repair: 0,
            logistics_transfer: 0,
            capture: 0,
            tk_window: 5,
            provisional: false,
            strict: true,
            airframe_cost,
            weapon_cost: FxHashMap::default(),
            periodic_point_gain: (0, 0),
            award_kill_points: true,
            convoy_interdiction_points: 0,
            kill_streak_bonuses: vec![],
            lifeline: Some(LifelineCfg {
                airframes: vec![Vehicle::from("Su-25T"), Vehicle::from("TF-51D")],
                below,
                store_budget: None,
                max_airframe_cost: None,
            }),
        }
    }

    #[test]
    fn lifeline_defaults_to_cant_afford_the_airframe() {
        let p = points(None);
        let su25 = Vehicle::from("Su-25T");
        assert!(p.lifeline_applies(559, &su25));
        assert!(p.lifeline_applies(-40, &su25));
        assert!(!p.lifeline_applies(560, &su25));
        // unpriced airframe: already free, nobody is "below" 0 unless in debt
        assert!(!p.lifeline_applies(0, &Vehicle::from("TF-51D")));
        assert!(p.lifeline_applies(-1, &Vehicle::from("TF-51D")));
        // not on the list
        assert!(!p.lifeline_applies(-1000, &Vehicle::from("A-10C")));
    }

    #[test]
    fn lifeline_explicit_threshold() {
        let p = points(Some(1000));
        assert!(p.lifeline_applies(999, &Vehicle::from("Su-25T")));
        assert!(!p.lifeline_applies(1000, &Vehicle::from("Su-25T")));
        assert!(p.lifeline_applies(999, &Vehicle::from("TF-51D")));
        let mut off = points(None);
        off.lifeline = None;
        assert!(!off.lifeline_applies(-5000, &Vehicle::from("Su-25T")));
    }

    #[test]
    fn lifeline_any_airframe_with_store_budget() {
        let mut p = points(Some(500));
        let l = p.lifeline.as_mut().unwrap();
        l.airframes.clear();
        l.store_budget = Some(300);
        assert!(p.lifeline_applies(0, &Vehicle::from("F-16C_50")));
        assert!(!p.lifeline_applies(500, &Vehicle::from("F-16C_50")));
        assert_eq!(p.lifeline_store_charge(250), 0);
        assert_eq!(p.lifeline_store_charge(420), 120);
        p.lifeline.as_mut().unwrap().store_budget = None;
        assert_eq!(p.lifeline_store_charge(5000), 0);
        // priced out of the era: never a free ride
        p.airframe_cost.insert(Vehicle::from("TF-51D"), 99999);
        p.lifeline.as_mut().unwrap().max_airframe_cost = Some(1000);
        assert!(!p.lifeline_applies(0, &Vehicle::from("TF-51D")));
        assert!(p.lifeline_applies(0, &Vehicle::from("Su-25T")));
    }
}
