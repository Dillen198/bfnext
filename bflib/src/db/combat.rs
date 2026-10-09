/*
Copyright 2026 Dillen Weerasinghe.

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

//! The arithmetic of the ground war's fighting (`GroundCombatCfg`), kept
//! apart from the formations themselves so it can be tested on its own.
//!
//! Every vehicle has a role, from its DCS type's tags. A role has a
//! firepower (what it does to the enemy), a toughness (how much damage kills
//! it) and a road and cross-country speed. A formation's power is the sum of
//! its vehicles' firepower, scaled by its supply, morale and how it is
//! deployed. In a round of fighting each side deals `lethality x power x
//! minutes` of damage, split across what it is fighting in proportion to
//! their power, divided by the target's cover; damage kills vehicles once it
//! adds up to their toughness.

use bfprotocols::cfg::{GroundCombatCfg, UnitTag};
use enumflags2::BitFlags;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Role {
    Tank,
    Ifv,
    Apc,
    Aaa,
    Sam,
    Artillery,
    Infantry,
    Truck,
}

impl Role {
    pub fn name(self) -> &'static str {
        match self {
            Self::Tank => "tank",
            Self::Ifv => "ifv",
            Self::Apc => "apc",
            Self::Aaa => "aaa",
            Self::Sam => "sam",
            Self::Artillery => "artillery",
            Self::Infantry => "infantry",
            Self::Truck => "truck",
        }
    }

    /// From a DCS type's tags. Anything armed and armoured that isn't a
    /// troop carrier is a tank; an armed troop carrier is an IFV.
    pub fn of(tags: BitFlags<UnitTag>) -> Self {
        if tags.contains(UnitTag::Infantry) {
            Self::Infantry
        } else if tags.contains(UnitTag::SAM) {
            Self::Sam
        } else if tags.contains(UnitTag::AAA) {
            Self::Aaa
        } else if tags.contains(UnitTag::Artillery) {
            Self::Artillery
        } else if tags.contains(UnitTag::APC) {
            if tags.intersects(UnitTag::ATGM | UnitTag::LightCannon | UnitTag::HeavyCannon) {
                Self::Ifv
            } else {
                Self::Apc
            }
        } else if tags.contains(UnitTag::Armor) {
            Self::Tank
        } else {
            Self::Truck
        }
    }

    /// Built-in firepower, before `GroundCombatCfg::firepower` overrides.
    pub fn base_firepower(self) -> f64 {
        match self {
            Self::Tank => 10.,
            Self::Ifv => 6.,
            Self::Apc => 3.,
            Self::Artillery => 5.,
            Self::Aaa => 2.,
            Self::Sam => 1.,
            Self::Infantry => 1.,
            Self::Truck => 0.2,
        }
    }

    pub fn firepower(self, cfg: &GroundCombatCfg) -> f64 {
        cfg.firepower.get(self.name()).copied().unwrap_or_else(|| self.base_firepower())
    }

    /// Damage it takes to kill one.
    pub fn toughness(self) -> f64 {
        match self {
            Self::Tank => 3.,
            Self::Ifv => 2.,
            Self::Apc => 1.5,
            Self::Infantry => 1.,
            Self::Aaa | Self::Sam | Self::Artillery => 1.,
            Self::Truck => 0.5,
        }
    }

    /// How likely it is to be the one hit, relative to the others: the soft
    /// and the tall draw fire, tanks hull-down less so.
    pub fn exposure(self) -> f64 {
        match self {
            Self::Truck => 1.6,
            Self::Apc | Self::Aaa | Self::Sam => 1.3,
            Self::Ifv | Self::Artillery => 1.1,
            Self::Infantry => 0.9,
            Self::Tank => 0.8,
        }
    }

    /// (road, cross country) km/h. Infantry is on foot unless something in
    /// the formation can carry it (`Speed::of`).
    pub fn speed_kph(self) -> (f64, f64) {
        match self {
            Self::Tank => (50., 30.),
            Self::Ifv => (60., 35.),
            Self::Apc => (70., 35.),
            Self::Aaa | Self::Sam => (55., 30.),
            Self::Artillery => (45., 25.),
            Self::Truck => (70., 30.),
            Self::Infantry => (6., 5.),
        }
    }

    pub fn is_vehicle(self) -> bool {
        self != Self::Infantry
    }
}

/// How a formation is laid out, which decides how well it fights.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum Deployment {
    /// On the march, nose to tail on the road.
    #[default]
    Column,
    /// Shaking out into line after running into the enemy.
    Deploying,
    /// In line, ready to fight.
    Deployed,
    /// Holding a position it has had time to prepare.
    DugIn,
}

impl Deployment {
    pub fn name(self) -> &'static str {
        match self {
            Self::Column => "column",
            Self::Deploying => "deploying",
            Self::Deployed => "deployed",
            Self::DugIn => "dug_in",
        }
    }

    /// Share of its power it can bring to bear.
    pub fn power(self, cfg: &GroundCombatCfg) -> f64 {
        match self {
            Self::Column => cfg.column_power.clamp(0.1, 1.),
            Self::Deploying => (cfg.column_power + 1.) / 2.,
            Self::Deployed | Self::DugIn => 1.,
        }
    }

    /// Damage it takes is divided by this.
    pub fn cover(self, cfg: &GroundCombatCfg) -> f64 {
        match self {
            Self::DugIn => cfg.dug_in_cover.max(1.),
            _ => 1.,
        }
    }
}

/// What scales a force's raw firepower.
#[derive(Debug, Clone, Copy)]
pub struct Condition {
    /// Fuel and ammunition, 0..1.
    pub supply: f64,
    /// 0..1.
    pub morale: f64,
    pub deployment: Deployment,
    pub broken: bool,
}

impl Condition {
    /// Share of its raw firepower the force can use, 0..1.
    pub fn factor(&self, cfg: &GroundCombatCfg) -> f64 {
        // An empty magazine still fires the odd round; a force with no
        // morale at all still defends itself a little.
        let supply = 0.3 + 0.7 * self.supply.clamp(0., 1.);
        let morale = 0.5 + 0.5 * self.morale.clamp(0., 1.);
        let broken = if self.broken { 0.3 } else { 1. };
        supply * morale * broken * self.deployment.power(cfg)
    }
}

/// Raw firepower of a set of roles.
pub fn raw_power<'a>(cfg: &GroundCombatCfg, roles: impl IntoIterator<Item = &'a Role>) -> f64 {
    roles.into_iter().map(|r| r.firepower(cfg)).sum()
}

/// March speed of a set of roles, (road, cross country) km/h: the slowest
/// vehicle's, or the infantry's on foot when there is nothing to ride in.
/// `cap` is the server's `speed_kph`, a ceiling on the road speed.
pub fn speed_of<'a>(roles: impl IntoIterator<Item = &'a Role> + Clone, cap: f64) -> (f64, f64) {
    let vehicles = roles.clone().into_iter().filter(|r| r.is_vehicle());
    let slowest = vehicles.fold(None::<(f64, f64)>, |acc, r| {
        let (a, b) = r.speed_kph();
        Some(match acc {
            None => (a, b),
            Some((x, y)) => (x.min(a), y.min(b)),
        })
    });
    let (road, off) = match slowest {
        Some(s) => s,
        None if roles.into_iter().next().is_some() => Role::Infantry.speed_kph(),
        None => (0., 0.),
    };
    let cap = cap.max(1.);
    (road.min(cap), off.min(cap * 0.6))
}

/// Damage `power` deals in `minutes`, split across `targets` (their power),
/// each divided by its cover: one figure per target.
pub fn damage_dealt(cfg: &GroundCombatCfg, power: f64, minutes: f64, targets: &[(f64, f64)]) -> Vec<f64> {
    let total: f64 = targets.iter().map(|(p, _)| p.max(0.01)).sum();
    targets
        .iter()
        .map(|(p, cover)| cfg.lethality * power * minutes * (p.max(0.01) / total) / cover.max(1.))
        .collect()
}

/// Which of `units` (role, a stable sort key) die to `damage`: picked by
/// exposure, deterministically from `seed`, until the damage left can't kill
/// the next one. At most `cap` of them in one round. Returns (indices
/// killed, damage left over to carry into the next round).
pub fn casualties(units: &[(Role, u64)], damage: f64, cap: usize, seed: u64) -> (Vec<usize>, f64) {
    let mut left = damage;
    let mut alive: Vec<usize> = (0..units.len()).collect();
    let mut dead = vec![];
    let mut x = seed | 1;
    while !alive.is_empty() && dead.len() < cap {
        // xorshift: cheap, deterministic, good enough to spread the hits.
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        let total: f64 = alive.iter().map(|i| units[*i].0.exposure()).sum();
        let mut pick = (x % 10_000) as f64 / 10_000. * total;
        let mut chosen = alive[alive.len() - 1];
        for i in &alive {
            pick -= units[*i].0.exposure();
            if pick <= 0. {
                chosen = *i;
                break;
            }
        }
        let t = units[chosen].0.toughness();
        if left < t {
            break;
        }
        left -= t;
        dead.push(chosen);
        alive.retain(|i| *i != chosen);
    }
    // Damage that can't kill anything any more is spent.
    if alive.is_empty() {
        left = 0.;
    }
    (dead, left)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cfg() -> GroundCombatCfg {
        GroundCombatCfg::default()
    }

    #[test]
    fn roles_come_from_tags() {
        assert_eq!(Role::of(UnitTag::Armor | UnitTag::HeavyCannon), Role::Tank);
        assert_eq!(Role::of(UnitTag::Armor | UnitTag::APC | UnitTag::ATGM), Role::Ifv);
        assert_eq!(Role::of(UnitTag::APC.into()), Role::Apc);
        assert_eq!(Role::of(UnitTag::Infantry | UnitTag::SmallArms), Role::Infantry);
        assert_eq!(Role::of(UnitTag::AAA | UnitTag::Armor), Role::Aaa);
        assert_eq!(Role::of(UnitTag::Unarmed | UnitTag::Logistics), Role::Truck);
    }

    #[test]
    fn a_column_is_as_slow_as_its_slowest_vehicle() {
        let (road, off) = speed_of(&[Role::Truck, Role::Tank, Role::Infantry], 100.);
        assert_eq!((road, off), (50., 30.));
        // Nothing to ride in: the infantry walks.
        assert_eq!(speed_of(&[Role::Infantry], 100.).0, 6.);
        // The server's cap still wins.
        assert_eq!(speed_of(&[Role::Truck], 30.).0, 30.);
    }

    #[test]
    fn supply_morale_and_deployment_cut_power() {
        let c = cfg();
        let fresh = Condition { supply: 1., morale: 1., deployment: Deployment::Deployed, broken: false };
        assert!((fresh.factor(&c) - 1.).abs() < 1e-9);
        let column = Condition { deployment: Deployment::Column, ..fresh };
        assert!(column.factor(&c) < fresh.factor(&c));
        let dry = Condition { supply: 0., ..fresh };
        assert!(dry.factor(&c) < 0.5);
        let broken = Condition { broken: true, ..fresh };
        assert!(broken.factor(&c) < 0.5);
    }

    #[test]
    fn equal_tank_companies_take_a_while() {
        // Ten tanks each, deployed: how long until one has lost half?
        let c = cfg();
        let power = raw_power(&c, &[Role::Tank; 10]);
        let per_min = damage_dealt(&c, power, 1., &[(power, 1.)])[0];
        let half = 5. * Role::Tank.toughness();
        let mins = half / per_min;
        assert!(mins > 15. && mins < 45., "{mins} minutes");
    }

    #[test]
    fn damage_is_split_by_target_power_and_cover() {
        let c = cfg();
        let d = damage_dealt(&c, 100., 1., &[(30., 1.), (10., 1.), (30., 2.)]);
        assert!((d[0] / d[1] - 3.).abs() < 1e-9);
        assert!((d[0] / d[2] - 2.).abs() < 1e-9);
    }

    #[test]
    fn casualties_spend_damage_and_carry_the_rest() {
        let units: Vec<(Role, u64)> = vec![(Role::Tank, 1), (Role::Truck, 2), (Role::Truck, 3)];
        // Too little to kill anything: carried over whole.
        let (dead, left) = casualties(&units, 0.4, 10, 7);
        assert!(dead.is_empty());
        assert!((left - 0.4).abs() < 1e-9);
        // Plenty: everyone dies, nothing carries.
        let (dead, left) = casualties(&units, 100., 10, 7);
        assert_eq!(dead.len(), 3);
        assert_eq!(left, 0.);
        // The cap holds.
        let (dead, _) = casualties(&units, 100., 1, 7);
        assert_eq!(dead.len(), 1);
    }
}
