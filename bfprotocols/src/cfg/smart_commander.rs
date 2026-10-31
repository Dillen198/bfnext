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

//! The smart commander (treasury, objective funding, HQ).

use super::*;

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct SmartCommanderCfg {
    /// Seconds between commander ticks (holding bonuses, objective funding).
    #[serde(default = "default_commander_period")]
    pub tick_period_secs: u32,
    /// Starting treasury for each side on a fresh map. Default: 0.
    #[serde(default)]
    pub treasury_start: i64,
    /// Points added to each side's treasury every `treasury_income_period_secs`.
    #[serde(default = "default_treasury_income_amount")]
    pub treasury_income_amount: i64,
    /// Interval in seconds between treasury income deposits. Default: 300.
    #[serde(default = "default_treasury_income_period")]
    pub treasury_income_period_secs: u32,
    /// Max points injected per objective per funding pass (scales with damage). Default: 200.
    #[serde(default = "default_obj_fund_max")]
    pub objective_fund_max_per_tick: i32,
    /// Seconds between objective funding passes. Default: 120.
    #[serde(default = "default_obj_fund_period")]
    pub objective_fund_period_secs: u32,
    /// Territory pay: each commander tick every player of a side who is in a
    /// slot (not spectating) gets this many points times the side's share of
    /// all objectives, rounded to the nearest point, so holding the whole map
    /// pays the full amount and holding a sliver pays at least 1. Scaled by
    /// the economy's underdog multiplier and wealth taper. Default: 5.
    #[serde(default = "default_holding_bonus")]
    pub holding_bonus_per_objective: i32,
    /// Points seeded into each owned objective on a fresh map init. Default: 500.
    #[serde(default = "default_objective_start_points")]
    pub objective_start_points: i32,
    /// Treasury cost per side to order an artillery barrage. Default: 150.
    #[serde(default = "default_barrage_cost")]
    pub barrage_cost: i64,
    /// Treasury cost per side to set a convoy ambush. Default: 100.
    #[serde(default = "default_ambush_cost")]
    pub ambush_cost: i64,
    /// Treasury cost per side to scramble an enemy CAP. Default: 250.
    #[serde(default = "default_cap_cost")]
    pub cap_cost: i64,
    /// Minimum air-superiority gap (enemy_in_air − friendly_in_air) required
    /// before the commander dispatches a CAP flight to balance the skies.
    /// 1 = dispatch as soon as enemy has even one more aircraft airborne;
    /// 2+ = only dispatch when significantly outnumbered. Default: 1.
    #[serde(default = "default_cap_min_friendly_pilots")]
    pub cap_min_friendly_pilots: u32,
    /// Seconds the commander waits after a dispatched CAP flight ends before
    /// spawning another one. Default: 300 (5 min).
    #[serde(default = "default_cap_cooldown_secs")]
    pub cap_cooldown_secs: u32,
    /// Treasury each side keeps in reserve for military actions -- the passive
    /// objective point-funding pass will not spend a side's treasury below
    /// this floor. Without it, a losing side with many damaged/threatened
    /// objectives drains its entire income into point-drips every tick and can
    /// never accumulate enough for a counterattack, barrage, ambush or CAP
    /// (observed: Red pinned at treasury 0 for a whole session, "no affordable
    /// action" every tick). Default: 300 (covers the priciest default action).
    #[serde(default = "default_commander_action_reserve")]
    pub action_reserve: i64,
    /// The theatre HQ: an AI commander per coalition that plans and
    /// dispatches missions and logistics out of this treasury (see
    /// `cfg::hq`). Absent = the treasury only funds objectives and the four
    /// campaign-event actions.
    #[serde(default)]
    pub hq: Option<HqCfg>,
}

pub(super) fn default_commander_action_reserve() -> i64 {
    300
}
