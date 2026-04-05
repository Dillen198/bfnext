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

use crate::db::Db;
use bfprotocols::cfg::SmartCommanderCfg;
use chrono::{DateTime, Utc};
use compact_str::format_compact;
use dcso3::{coalition::Side, net::Ucid};
use fxhash::FxHashMap;

/// Run all Smart Commander subsystems for this tick.
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
    let elapsed = (ts - db.ephemeral.last_treasury_income).num_seconds();
    if elapsed < cfg.treasury_income_period_secs as i64 {
        return;
    }
    db.ephemeral.last_treasury_income = ts;
    for side in [Side::Blue, Side::Red] {
        let bal = db.persisted.adjust_treasury(side, cfg.treasury_income_amount);
        db.ephemeral.msgs().panel_to_side(
            10,
            false,
            side,
            format_compact!(
                "[Commander] Treasury income +{} → {}",
                cfg.treasury_income_amount,
                bal
            ),
        );
        db.ephemeral.dirty();
    }
}

fn tick_objective_funding(db: &mut Db, cfg: &SmartCommanderCfg, ts: DateTime<Utc>) {
    let elapsed = (ts - db.ephemeral.last_objective_fund).num_seconds();
    if elapsed < cfg.objective_fund_period_secs as i64 {
        return;
    }
    db.ephemeral.last_objective_fund = ts;

    // Collect grants before mutating (avoids borrow conflict on db.persisted).
    let grants: Vec<_> = db
        .persisted
        .objectives
        .into_iter()
        .filter_map(|(oid, obj)| {
            let side = obj.owner();
            if matches!(side, Side::Neutral) {
                return None;
            }
            let dmg = (100u32.saturating_sub(obj.health() as u32)) as f64 / 100.0;
            let weight = if obj.threatened() { 1.5_f64 } else { 1.0_f64 };
            let grant = ((cfg.objective_fund_max_per_tick as f64) * dmg * weight)
                .round()
                .min(cfg.objective_fund_max_per_tick as f64) as i32;
            if grant > 0 {
                Some((*oid, side, grant, obj.name().to_owned()))
            } else {
                None
            }
        })
        .collect();

    for (oid, side, grant, name) in grants {
        let available = db.persisted.treasury(side);
        if available <= 0 {
            continue;
        }
        let actual = grant.min(available.min(i32::MAX as i64) as i32);
        db.persisted.adjust_treasury(side, -(actual as i64));
        if let Some(obj) = db.persisted.objectives.get_mut_cow(&oid) {
            obj.points += actual;
        }
        db.ephemeral.msgs().panel_to_side(
            10,
            false,
            side,
            format_compact!(
                "[Commander] Funded {} +{} pts (treasury → {})",
                name,
                actual,
                db.persisted.treasury(side)
            ),
        );
        db.ephemeral.dirty();
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
        let bonus = ((cfg.holding_bonus_per_objective as f64) * owned as f64 / total as f64)
            .floor() as i32;
        if bonus <= 0 {
            continue;
        }
        let ucids = match ucids_by_side.get(&side) {
            Some(v) => v,
            None => continue,
        };
        for ucid in ucids {
            db.adjust_points(ucid, bonus, "holding bonus");
        }
        db.ephemeral.msgs().panel_to_side(
            10,
            false,
            side,
            format_compact!(
                "[Commander] Holding bonus +{} pts ({}/{} objectives)",
                bonus,
                owned,
                total
            ),
        );
    }
}
