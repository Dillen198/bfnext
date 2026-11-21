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

//! The war-effort bar: one 0-100 tug of war per round, 50 at the start.
//! Every capture moves it `CAPTURE_STEP` toward the side that took the base
//! (doubled after `STALEMATE_HOURS` without one, so a frozen front breaks),
//! and the side it favours gets perks that make the push visible in play:
//!
//! | effort | perk                                  |
//! |--------|---------------------------------------|
//! | 60+    | takeoff hold 25% shorter              |
//! | 75+    | actions cost 15% less                 |
//! | 90+    | takeoff hold halved                   |
//!
//! It is shown on the slot-entry briefing and announced when a side crosses
//! a tier. Saved with the campaign; a new round starts it at 50 again.

use super::Db;
use chrono::{DateTime, Duration, Utc};
use compact_str::{CompactString, format_compact};
use dcso3::coalition::Side;
use serde_derive::{Deserialize, Serialize};

const CAPTURE_STEP: f64 = 4.;
const STALEMATE_HOURS: i64 = 2;
/// Effort at which each perk tier starts.
const TIERS: [f64; 3] = [60., 75., 90.];

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WarEffort {
    /// 100 = all Blue, 0 = all Red.
    pub value: f64,
    #[serde(default)]
    pub last_capture: Option<DateTime<Utc>>,
}

impl Default for WarEffort {
    fn default() -> Self {
        Self { value: 50., last_capture: None }
    }
}

impl WarEffort {
    /// 0-100 from `side`'s point of view.
    pub fn for_side(&self, side: Side) -> f64 {
        match side {
            Side::Blue => self.value,
            Side::Red => 100. - self.value,
            Side::Neutral => 50.,
        }
    }

    /// 0 (none) to 3.
    pub fn tier(&self, side: Side) -> usize {
        let v = self.for_side(side);
        TIERS.iter().filter(|t| v >= **t).count()
    }

    /// Multiplier on the post-slot takeoff hold for `side`.
    pub fn hold_factor(&self, side: Side) -> f64 {
        match self.tier(side) {
            0 => 1.,
            1 | 2 => 0.75,
            _ => 0.5,
        }
    }

    /// Multiplier on action costs for `side`.
    pub fn action_cost_factor(&self, side: Side) -> f64 {
        if self.tier(side) >= 2 { 0.85 } else { 1. }
    }

    /// One line for the slot briefing.
    pub fn briefing_line(&self, side: Side) -> CompactString {
        let us = self.for_side(side).round();
        let perk = match self.tier(side) {
            0 => "no perks yet -- captures push the bar",
            1 => "perk: takeoff hold 25% shorter",
            2 => "perks: takeoff hold 25% shorter, actions 15% cheaper",
            _ => "perks: takeoff hold halved, actions 15% cheaper",
        };
        format_compact!("WAR EFFORT: {us}/100 for us ({perk})")
    }
}

fn tier_text(t: usize) -> &'static str {
    match t {
        1 => "shorter takeoff holds",
        2 => "shorter takeoff holds and cheaper actions",
        _ => "halved takeoff holds and cheaper actions",
    }
}

impl Db {
    /// `captor` took a base at `now`: move the bar, and announce a side
    /// crossing into a new perk tier.
    pub(crate) fn war_effort_capture(&mut self, captor: Side, now: DateTime<Utc>) {
        if captor == Side::Neutral {
            return;
        }
        let we = &mut self.persisted.war_effort;
        let before = (we.tier(Side::Blue), we.tier(Side::Red));
        let stalemate =
            we.last_capture.is_some_and(|t| now - t >= Duration::hours(STALEMATE_HOURS));
        let step = CAPTURE_STEP * if stalemate { 2. } else { 1. };
        we.value = match captor {
            Side::Blue => we.value + step,
            _ => we.value - step,
        }
        .clamp(0., 100.);
        we.last_capture = Some(now);
        let after = (we.tier(Side::Blue), we.tier(Side::Red));
        let blue_value = we.value;
        self.ephemeral.dirty();
        for (side, b, a) in
            [(Side::Blue, before.0, after.0), (Side::Red, before.1, after.1)]
        {
            if a > b {
                let v = if side == Side::Blue { blue_value } else { 100. - blue_value };
                self.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side,
                    format_compact!(
                        "WAR EFFORT {:.0}/100: our push has earned {}.",
                        v,
                        tier_text(a)
                    ),
                );
                self.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side.opposite(),
                    format_compact!(
                        "The enemy's war effort is at {:.0}/100 -- they now have {}. Take bases back to stop it.",
                        v,
                        tier_text(a)
                    ),
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tiers_mirror_between_sides() {
        let we = WarEffort { value: 80., last_capture: None };
        assert_eq!(we.tier(Side::Blue), 2);
        assert_eq!(we.tier(Side::Red), 0);
        let we = WarEffort { value: 5., last_capture: None };
        assert_eq!(we.tier(Side::Red), 3);
        assert_eq!(we.hold_factor(Side::Red), 0.5);
        assert_eq!(we.action_cost_factor(Side::Blue), 1.);
    }
}
