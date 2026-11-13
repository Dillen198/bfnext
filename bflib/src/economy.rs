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

//! The war economy: what flows *into* each side's treasury and its bases'
//! funds -- treasury income, objective funding and holding bonuses -- run
//! every `smart_commander.tick_period_secs`. (This was the economy half of
//! `commander.rs`; its other half, the event picker, is the HQ's rules mode,
//! `crate::hq::rules`. Spending goes through `crate::hq::treasury`.)

use crate::db::Db;
use bfprotocols::{cfg::SmartCommanderCfg, db::objective::ObjectiveId};
use chrono::{DateTime, Utc};
use compact_str::CompactString;
use dcso3::{coalition::Side, net::Ucid};
use fxhash::FxHashMap;
use log::{debug, info};

/// Run the economy for this tick.
pub fn tick(
    db: &mut Db,
    cfg: &SmartCommanderCfg,
    ts: DateTime<Utc>,
    ucids_by_side: &FxHashMap<Side, Vec<Ucid>>,
) {
    tick_treasury_income(db, cfg, ts);
    tick_objective_funding(db, cfg, ts);
    tick_holding_bonuses(db, cfg, ucids_by_side);
}

fn tick_treasury_income(db: &mut Db, cfg: &SmartCommanderCfg, ts: DateTime<Utc>) {
    // The clock starts at the epoch, so the first tick after every restart
    // used to pay a deposit straight away -- frequent restarts were income.
    // Start it instead.
    if db.ephemeral.last_treasury_income == DateTime::<Utc>::default() {
        db.ephemeral.last_treasury_income = ts;
        return;
    }
    let elapsed = (ts - db.ephemeral.last_treasury_income).num_seconds();
    if elapsed < cfg.treasury_income_period_secs as i64 {
        return;
    }
    db.ephemeral.last_treasury_income = ts;
    let underdog = db.ephemeral.cfg.economy.underdog_treasury;
    for side in [Side::Blue, Side::Red] {
        // A side behind on ground or outnumbered gets more to fight back
        // with (see `EconomyCfg::underdog_multiplier`).
        let mult = if underdog { db.underdog_multiplier(side) } else { 1. };
        let amount = (cfg.treasury_income_amount as f64 * mult).round() as i64;
        let bal = db.persisted.adjust_treasury(side, amount);
        info!(
            "[Commander] {:?} treasury income +{} (x{:.2}) → {}",
            side, amount, mult, bal
        );
        db.ephemeral.dirty();
    }
}

/// With no `objective_start_points` to size a base's fund by, the drip tops
/// a fund up to this many full grants (`objective_fund_max_per_tick`).
const OBJECTIVE_FUND_CEILING_PASSES: i64 = 10;

fn tick_objective_funding(db: &mut Db, cfg: &SmartCommanderCfg, ts: DateTime<Utc>) {
    let elapsed = (ts - db.ephemeral.last_objective_fund).num_seconds();
    if elapsed < cfg.objective_fund_period_secs as i64 {
        return;
    }
    db.ephemeral.last_objective_fund = ts;

    // What income actually delivers over one funding window, and therefore
    // what this pass is allowed to hand out in total.
    //
    // Funding used to ask for up to `objective_fund_max_per_tick` for EVERY
    // damaged objective, with nothing but the reserve floor to stop it. On the
    // live config that is 200 per damaged base per 2 min against 200 of income
    // per 2 min, so the ask outran income by an order of magnitude the moment a
    // side held more than one damaged base. Observed over a 3.7h session: the
    // side holding 94 objectives sat pinned on the reserve floor the whole
    // time while the side holding 62 banked 272,000 points it had nothing to
    // spend on.
    //
    // And because the grants were handed out in map iteration order until the
    // floor was hit, *which* bases got funded was arbitrary -- the first few in
    // the map took the full 200 each and a base actually under attack further
    // down the list got nothing. Budget the pass, then share the budget out by
    // need, so a side that is not under pressure spends less than it earns and
    // accumulates for the actions the treasury exists to pay for.
    let income_per_window = if cfg.treasury_income_period_secs == 0 {
        cfg.treasury_income_amount
    } else {
        (cfg.treasury_income_amount * cfg.objective_fund_period_secs as i64)
            / cfg.treasury_income_period_secs as i64
    };
    // With the theatre HQ running, the drip only gets its share of the
    // income; the rest is the HQ's to spend on operations.
    let income_per_window = match cfg.hq.as_ref().filter(|h| h.enabled) {
        // No share at all: no drip (rather than the no-income fallback below,
        // which would fund out of the whole treasury).
        Some(h) if h.objective_funding_share <= 0. => return,
        Some(h) => (income_per_window as f64 * h.objective_funding_share.min(1.)) as i64,
        None => income_per_window,
    };

    // Collect asks before mutating (avoids borrow conflict on db.persisted).
    // `need` is what drives both the size of the ask and the priority when the
    // budget cannot cover every ask.
    let mut asks: Vec<(Side, f64, i64, ObjectiveId, CompactString)> = vec![];
    // Each fund is topped up to a ceiling, never past it -- see
    // `Db::objective_fund_ceilings`. A campaign with no start budget caps at
    // a fixed number of full passes instead.
    let ceilings = db.objective_fund_ceilings();
    let fallback_ceiling =
        cfg.objective_fund_max_per_tick.max(0) as i64 * OBJECTIVE_FUND_CEILING_PASSES;
    for (oid, obj) in &db.persisted.objectives {
        let side = obj.owner();
        if matches!(side, Side::Neutral) {
            continue;
        }
        let dmg = (100u32.saturating_sub(obj.health() as u32)) as f64 / 100.0;
        let weight = if obj.threatened() { 1.5_f64 } else { 1.0_f64 };
        let need = dmg * weight;
        let want = ((cfg.objective_fund_max_per_tick as f64) * need)
            .round()
            .min(cfg.objective_fund_max_per_tick as f64) as i64;
        let ceiling = ceilings.get(oid).copied().unwrap_or(fallback_ceiling);
        let want = want.min(ceiling - obj.points as i64);
        if want > 0 {
            asks.push((side, need, want, *oid, CompactString::from(obj.name())));
        }
    }
    // Neediest first: they get the whole ask while the budget lasts, and the
    // rounding remainder when it has to be shared.
    asks.sort_by(|a, b| b.1.total_cmp(&a.1));

    // Never let passive point-funding pull a side below the action reserve --
    // otherwise a losing side spends its whole income on drips and can never
    // afford a counterattack / barrage / CAP.
    let reserve = cfg.action_reserve.max(0);
    for side in [Side::Blue, Side::Red] {
        let spendable = db.persisted.treasury(side) - reserve;
        // A campaign configured with no recurring income funds out of
        // treasury_start instead, so there is no income window to budget
        // against -- fall back to the old ceiling there. Everything below
        // still applies: the spend is shared out by need rather than handed to
        // whoever sorts first.
        let budget = if income_per_window <= 0 {
            spendable
        } else {
            income_per_window.min(spendable)
        };
        if budget <= 0 {
            continue;
        }
        let total_want: i64 =
            asks.iter().filter(|(s, ..)| *s == side).map(|(_, _, want, ..)| *want).sum();
        if total_want <= 0 {
            continue;
        }
        let mut left = budget;
        let mut funded = 0usize;
        let mut spent = 0i64;
        for (s, _, want, oid, name) in asks.iter() {
            if *s != side || left <= 0 {
                continue;
            }
            // Scale every ask by the same factor when the budget is short, so a
            // shortfall thins the whole side's funding evenly instead of
            // starving whatever happens to sort last. At least 1 point, so a
            // heavily contested side still moves every damaged base forward.
            let share = if total_want <= budget {
                *want
            } else {
                ((*want * budget) / total_want).max(1)
            };
            let actual = share.min(left);
            if actual <= 0 {
                continue;
            }
            left -= actual;
            spent += actual;
            funded += 1;
            db.persisted.adjust_treasury(side, -actual);
            if let Some(obj) = db.persisted.objectives.get_mut_cow(oid) {
                obj.points += actual.min(i32::MAX as i64) as i32;
            }
            debug!("[Commander] {side:?} funded {name} +{actual} pts");
        }
        if funded > 0 {
            // One line per side per pass, not one per objective: this used to
            // be ~100 lines a session of "funded X +200" with no way to see the
            // shape of the spend.
            info!(
                "[Commander] {side:?} funded {funded} objective(s) for {spent} pts (asked {total_want}, budget {budget} = income over the funding window, treasury → {})",
                db.persisted.treasury(side)
            );
            db.ephemeral.dirty();
        }
    }
}

fn tick_holding_bonuses(
    db: &mut Db,
    cfg: &SmartCommanderCfg,
    ucids_by_side: &FxHashMap<Side, Vec<Ucid>>,
) {
    if cfg.holding_bonus_per_objective == 0 {
        return;
    }
    let total = db.persisted.objectives.len();
    if total == 0 {
        return;
    }
    for side in [Side::Blue, Side::Red] {
        let owned = db
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, o)| o.owner() == side)
            .count();
        // Rounded, and at least 1 for a side that holds anything: flooring
        // paid a side under 20% of the map (at the default 5) nothing at
        // all, exactly when it most needed the income.
        let bonus = ((cfg.holding_bonus_per_objective as f64) * owned as f64
            / total as f64)
            .round() as i32;
        let bonus = if owned > 0 { bonus.max(1) } else { bonus };
        if bonus <= 0 {
            continue;
        }
        let ucids = match ucids_by_side.get(&side) {
            Some(v) => v,
            None => continue,
        };
        for ucid in ucids {
            db.earn_points_silent(ucid, bonus, "holding bonus");
        }
        info!(
            "[Commander] {:?} holding bonus +{} pts ({}/{} objectives)",
            side, bonus, owned, total
        );
    }
}
