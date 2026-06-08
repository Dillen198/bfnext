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

use crate::db::{
    events::{CampaignEvent, EventEffect, EventScheduler},
    objective::ObjGroupClass,
    Db,
};
use bfprotocols::{
    cfg::{CampaignEventsCfg, SmartCommanderCfg},
    db::objective::ObjectiveId,
};
use chrono::{DateTime, Utc};
use compact_str::CompactString;
use dcso3::{coalition::Side, net::Ucid, Vector2};
use fxhash::FxHashMap;
use log::info;
use rand::Rng;
use smallvec::SmallVec;
use std::cmp::Ordering;

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

// ---------------------------------------------------------------------------
// Battlefield assessment types
// ---------------------------------------------------------------------------

/// Full state snapshot of one objective for commander decision-making.
#[derive(Debug, Clone)]
struct ObjAssessment {
    oid: ObjectiveId,
    pos: Vector2,
    health: u8,
    _logi: u8,
    supply: u8,

    threatened: bool,
    /// Enemy troops actively in the capture zone right now.
    being_captured: bool,
    /// logi == 0 — no logistics units, can be captured immediately.
    capturable: bool,
    /// This objective is a logistics hub — extra strategic value.
    is_logi_hub: bool,
    /// This objective is a factory — extra strategic value.
    is_factory: bool,
    /// Objective has alive Armor/Mr/Lr groups that can fire on enemies.
    has_fire_groups: bool,

    /// Objective display name (for intel messages).
    name: CompactString,
}



/// Collect ALCM/missile-capable deployed group IDs for `side`.
/// These are groups tagged with `UnitTag::ALCM` from both deployed and actions sets.
fn collect_alcm_groups(
    db: &Db,
    side: Side,
) -> SmallVec<[bfprotocols::db::group::GroupId; 4]> {
    use bfprotocols::cfg::UnitTag;
    db.deployed()
        .chain(db.actions())
        .filter(|g| g.side == side && g.tags.contains(UnitTag::ALCM))
        .map(|g| g.id)
        .collect()
}

/// Build a full objective assessment for a single side.
fn build_side_assessment(db: &Db, side: Side) -> Vec<ObjAssessment> {
    let being_captured: Vec<ObjectiveId> = db.objectives_being_captured_by(side);



    db.persisted
        .objectives
        .into_iter()
        .filter(|(_, o)| o.owner() == side)
        .map(|(id, obj)| {
            let pos = obj.pos();



            // Does this objective have alive fire-capable groups?
            let has_fire_groups = obj
                .groups()
                .get(&side)
                .map(|gids| {
                    gids.into_iter().any(|gid| {
                        db.persisted.groups.get(gid).map(|g| {
                            matches!(
                                g.class,
                                ObjGroupClass::Armor | ObjGroupClass::Mr | ObjGroupClass::Lr
                            ) && g.units.into_iter().any(|uid| {
                                db.persisted.units.get(uid).map(|u| !u.dead).unwrap_or(false)
                            })
                        }).unwrap_or(false)
                    })
                })
                .unwrap_or(false);



            ObjAssessment {
                oid: *id,
                pos,
                health: obj.health(),
                _logi: obj.logi(),
                supply: obj.supply(),

                threatened: obj.threatened(),
                being_captured: being_captured.contains(id),
                capturable: obj.captureable(),
                is_logi_hub: matches!(obj.kind(), bfprotocols::db::objective::ObjectiveKind::Logistics),
                is_factory: matches!(obj.kind(), bfprotocols::db::objective::ObjectiveKind::Factory { .. }),
                has_fire_groups,

                name: CompactString::from(obj.name()),
            }
        })
        .collect()
}

fn opposite_side(side: Side) -> Side {
    match side {
        Side::Blue => Side::Red,
        Side::Red => Side::Blue,
        Side::Neutral => Side::Neutral,
    }
}

// ---------------------------------------------------------------------------
// Action scoring
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum CommanderAction {
    /// Pre-selected barrage: source arty objective → target enemy objective.
    Barrage {
        src_oid: ObjectiveId,
        src_side: Side,
        target_oid: ObjectiveId,
        target_pos: Vector2,
        target_name: CompactString,
    },
    /// Pre-selected missile strike: ALCM/Scud/HIMARS groups fire at a high-value target.
    MissileStrike {
        side: Side,
        shooter_gids: SmallVec<[bfprotocols::db::group::GroupId; 4]>,
        target_pos: Vector2,
        target_name: CompactString,
    },
    Ambush,
    /// Dispatch a friendly AI CAP flight when there aren't enough human pilots
    /// in the air for this side.
    DispatchCap,
}

/// Count active fixed-wing players for `side` (excludes helicopters).
fn fixedwing_pilots_active(db: &Db, side: Side) -> u32 {
    use bfprotocols::cfg::UnitTag;
    db.persisted
        .players()
        .into_iter()
        .filter(|(_, p)| {
            if p.side != side {
                return false;
            }
            if let Some((_, Some(inst))) = &p.current_slot {
                let tags = db.ephemeral.cfg.unit_classification.get(&inst.typ);
                if let Some(tags) = tags {
                    // Must be aircraft and NOT helicopter
                    tags.contains(UnitTag::Aircraft) && !tags.contains(UnitTag::Helicopter)
                } else {
                    true
                }
            } else {
                false
            }
        })
        .count() as u32
}

/// Score all affordable actions for a side and return (action, cost, value).
fn score_actions(
    db: &Db,
    side: Side,
    sc_cfg: &SmartCommanderCfg,
    events_cfg: &CampaignEventsCfg,
    friendly: &[ObjAssessment],
    enemy: &[ObjAssessment],
    scheduler: &EventScheduler,
    alcm_groups: &[bfprotocols::db::group::GroupId],
    ts: DateTime<Utc>,
) -> Vec<(CommanderAction, i64, f64)> {
    let treasury = db.persisted.treasury(side);
    let mut actions: Vec<(CommanderAction, i64, f64)> = Vec::new();
    let total = (friendly.len() + enemy.len()).max(1);
    let _territory_ratio = friendly.len() as f64 / total as f64;

    // --- Barrage ---
    // Find the best (friendly arty obj, enemy target) pair using real gun-to-target
    // range against artillery_mission_range. Score by enemy weakness + proximity.
    if events_cfg.barrage_enabled && treasury >= sc_cfg.barrage_cost {
        let arty_range = db.ephemeral.cfg.artillery_mission_range as f64;
        let mut best_barrage: Option<(CommanderAction, f64)> = None;

        for src in friendly.iter().filter(|o| o.has_fire_groups) {
            for tgt in enemy.iter() {
                let dist = na::distance(&src.pos.into(), &tgt.pos.into());
                if dist > arty_range || dist < 5_000.0 {
                    continue;
                }
                // Score: enemy weakness + proximity bonus + strategic value.
                let proximity_bonus = if dist < arty_range * 0.5 { 1.5 } else { 1.0 };
                let weakness = (100u8.saturating_sub(tgt.health)) as f64 * 0.8
                    + (100u8.saturating_sub(tgt.supply)) as f64 * 0.4;
                let strategic = if tgt.is_logi_hub { 1.5 } else if tgt.is_factory { 1.2 } else { 1.0 };
                let capturable_bonus = if tgt.capturable { 40.0 } else { 0.0 };
                let value = (20.0 + weakness + capturable_bonus) * proximity_bonus * strategic;

                let better = best_barrage.as_ref().map(|(_, v)| value > *v).unwrap_or(true);
                if better {
                    best_barrage = Some((
                        CommanderAction::Barrage {
                            src_oid: src.oid,
                            src_side: side,
                            target_oid: tgt.oid,
                            target_pos: tgt.pos,
                            target_name: tgt.name.clone(),
                        },
                        value,
                    ));
                }
            }
        }

        if let Some((action, value)) = best_barrage {
            actions.push((action, sc_cfg.barrage_cost, value));
        }
    }

    // --- Missile Strike (ALCM / Scud / HIMARS) ---
    // Valuable when we have deployed missile groups and a high-value or hard-to-reach
    // enemy target exists within their range. Missiles have extreme range so they can
    // strike deep — score strategic targets (logi hubs, factories, capturable objs) highly.
    if events_cfg.barrage_enabled
        && treasury >= sc_cfg.barrage_cost
        && !alcm_groups.is_empty()
        && !scheduler.active_events.iter().any(|e| matches!(
            e, CampaignEvent::MissileStrike { side: s, .. } if *s == side
        ))
    {
        let alcm_range = db.ephemeral.cfg.alcm_mission_range as f64;

        // Find the most valuable enemy target any shooter can reach.
        let mut best_strike: Option<(CommanderAction, f64)> = None;
        for tgt in enemy.iter() {
            // Check if at least one shooter is within range.
            let reachable = alcm_groups.iter().any(|gid| {
                db.group_center(gid)
                    .map(|gpos| na::distance(&gpos.into(), &tgt.pos.into()) <= alcm_range)
                    .unwrap_or(false)
            });
            if !reachable { continue; }

            // Score: prioritise strategic targets and weak/capturable ones.
            let weakness = (100u8.saturating_sub(tgt.health)) as f64 * 0.8
                + (100u8.saturating_sub(tgt.supply)) as f64 * 0.4;
            let strategic = if tgt.is_logi_hub { 2.0 } else if tgt.is_factory { 1.6 } else { 1.0 };
            let capturable_bonus = if tgt.capturable { 60.0 } else { 0.0 };
            // Missiles are effective at long range — no proximity penalty.
            let value = (30.0 + weakness + capturable_bonus) * strategic;

            let shooters: SmallVec<[bfprotocols::db::group::GroupId; 4]> = alcm_groups
                .iter()
                .filter(|gid| {
                    db.group_center(gid)
                        .map(|gpos| na::distance(&gpos.into(), &tgt.pos.into()) <= alcm_range)
                        .unwrap_or(false)
                })
                .copied()
                .collect();

            let better = best_strike.as_ref().map(|(_, v)| value > *v).unwrap_or(true);
            if better {
                best_strike = Some((
                    CommanderAction::MissileStrike {
                        side,
                        shooter_gids: shooters,
                        target_pos: tgt.pos,
                        target_name: tgt.name.clone(),
                    },
                    value,
                ));
            }
        }

        if let Some((action, value)) = best_strike {
            // Missile strikes cost slightly more than conventional barrages.
            let cost = (sc_cfg.barrage_cost as f64 * 1.5).round() as i64;
            if treasury >= cost {
                actions.push((action, cost, value));
            }
        }
    }

    // --- Ambush ---
    // Most valuable when enemy convoys are running AND their supply is already stretched.
    if events_cfg.ambush_enabled && treasury >= sc_cfg.ambush_cost {
        let enemy_convoy_count = db.convoy_count_for_side(opposite_side(side));
        if enemy_convoy_count > 0 {
            let enemy_avg_supply = if enemy.is_empty() {
                100.0
            } else {
                enemy.iter().map(|o| o.supply as f64).sum::<f64>() / enemy.len() as f64
            };
            // More convoys + lower enemy supply = higher value.
            let value = 30.0
                + enemy_convoy_count as f64 * 20.0
                + (100.0 - enemy_avg_supply) * 0.6;
            actions.push((CommanderAction::Ambush, sc_cfg.ambush_cost, value));
        }
    }

    // --- Dispatch CAP ---
    // Only when cap_cost > 0, treasury covers it, there is no active commander
    // CAP for this side already, no reactive EnemyCap is already covering this side
    // (prevents stacking friendly + reactive), the post-expiry cooldown has passed,
    // and friendly in-air pilot count is below the configured threshold.
    if sc_cfg.cap_cost > 0
        && treasury >= sc_cfg.cap_cost
        && events_cfg.enemy_cap_enabled
        && !scheduler
            .active_events
            .iter()
            .any(|e| match e {
                // Block if a CommanderCap OR an EnemyCap for this defending side already exists.
                // EnemyCap{cap_side} = the side that owns the CAP (defending side) = our side.
                CampaignEvent::CommanderCap { cap_side: s, .. }
                | CampaignEvent::EnemyCap { cap_side: s, .. } => *s == side,
                _ => false,
            })
    {
        let last_ended = match side {
            Side::Blue => scheduler.last_commander_cap_ended_blue,
            Side::Red => scheduler.last_commander_cap_ended_red,
            Side::Neutral => None,
        };
        let cooldown_elapsed = last_ended
            .map(|t| (ts - t).num_seconds() >= sc_cfg.cap_cooldown_secs as i64)
            .unwrap_or(true); // no prior CAP → no cooldown

        if cooldown_elapsed {
            let friendly_air = fixedwing_pilots_active(db, side);
            let enemy_air = fixedwing_pilots_active(db, opposite_side(side));
            // Only balance if the server has enough players (ignore 1 vs 2 situations)
            let max_pilots = friendly_air.max(enemy_air);
            if max_pilots >= 3 {
                // Dispatch when enemy has more aircraft airborne than we do.
                // `cap_min_friendly_pilots` acts as a minimum gap threshold:
                // we only scramble if enemy_air - friendly_air >= that value.
                let gap = enemy_air.saturating_sub(friendly_air);
                if gap >= sc_cfg.cap_min_friendly_pilots {
                    // Value scales with how lopsided the air balance is and how
                    // many objectives are under threat.
                    let threatened_count =
                        friendly.iter().filter(|o| o.threatened || o.capturable).count();
                    let value = 40.0 + gap as f64 * 15.0 + threatened_count as f64 * 10.0;
                    actions.push((CommanderAction::DispatchCap, sc_cfg.cap_cost, value));
                }
            }
        }
    }


    actions
}

/// Select the best action: emergency override for active captures, otherwise
/// highest cost-efficiency (value / cost).
fn select_best_action(
    actions: &[(CommanderAction, i64, f64)],
    _friendly: &[ObjAssessment],
) -> Option<(CommanderAction, i64)> {
    if actions.is_empty() {
        return None;
    }
    // Pick highest cost-efficiency.
    actions
        .iter()
        .max_by(|(_, ca, va), (_, cb, vb)| {
            let ea = va / (*ca).max(1) as f64;
            let eb = vb / (*cb).max(1) as f64;
            ea.partial_cmp(&eb).unwrap_or(Ordering::Equal)
        })
        .map(|(action, cost, _)| (action.clone(), *cost))
}

// ---------------------------------------------------------------------------
// Per-side check interval (emergency vs normal)
// ---------------------------------------------------------------------------

fn side_should_check(
    scheduler: &EventScheduler,
    side: Side,
    ts: DateTime<Utc>,
    normal_interval: u32,
    has_emergency: bool,
) -> bool {
    const EMERGENCY_INTERVAL_SECS: i64 = 30;
    let last = match side {
        Side::Blue => scheduler.last_commander_check_blue,
        Side::Red => scheduler.last_commander_check_red,
        Side::Neutral => return false,
    };
    let elapsed = match last {
        None => i64::MAX,
        Some(t) => (ts - t).num_seconds(),
    };
    if has_emergency {
        elapsed >= EMERGENCY_INTERVAL_SECS
    } else {
        elapsed >= normal_interval as i64
    }
}

fn set_side_check_time(scheduler: &mut EventScheduler, side: Side, ts: DateTime<Utc>) {
    match side {
        Side::Blue => scheduler.last_commander_check_blue = Some(ts),
        Side::Red => scheduler.last_commander_check_red = Some(ts),
        Side::Neutral => {}
    }
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run smart commander strategic event decisions.
/// Returns (messages, effects) to be applied by the caller.
pub fn tick_events(
    db: &mut Db,
    sc_cfg: &SmartCommanderCfg,
    events_cfg: &CampaignEventsCfg,
    ts: DateTime<Utc>,
    scheduler: &mut EventScheduler,
    _player_count: usize,
) -> (Vec<CompactString>, Vec<EventEffect>) {
    let mut messages: Vec<CompactString> = Vec::new();
    let mut effects: Vec<EventEffect> = Vec::new();

    let candidates = scheduler.build_candidates(db);

    // Per-side strategic decisions.
    let red_count = candidates.iter().filter(|(_, s, ..)| *s == Side::Red).count();
    let blue_count = candidates.iter().filter(|(_, s, ..)| *s == Side::Blue).count();
    let total_obj = (red_count + blue_count).max(1);

    // Randomise which side gets to act first so neither side is systematically favoured.
    let mut sides = [Side::Blue, Side::Red];
    if rand::thread_rng().r#gen::<bool>() {
        sides.swap(0, 1);
    }

    for side in sides {
        // Build full assessment first so we can check for a capture emergency before
        // applying the concurrent event cap — an objective being actively captured must
        // be able to trigger reinforcement even when other events fill the slots.
        let friendly = build_side_assessment(db, side);
        let enemy = build_side_assessment(db, opposite_side(side));
        let alcm_groups = collect_alcm_groups(db, side);

        let has_emergency = friendly.iter().any(|o| o.being_captured || o.capturable);
        let capture_emergency = friendly.iter().any(|o| o.being_captured);

        // Enforce the concurrent event cap — except when an objective is actively being
        // captured, in which case we must allow a reinforcement wave through regardless.
        if scheduler.active_events.len() >= events_cfg.max_concurrent_events as usize
            && !capture_emergency
        {
            break;
        }

        if !side_should_check(scheduler, side, ts, events_cfg.check_interval_secs, has_emergency) {
            continue;
        }

        let territory_ratio = friendly.len() as f64 / total_obj as f64;
        info!(
            "[Commander] {:?} assessment: {} friendly objs, {} enemy, {:.0}% territory, \
             emergency={}, treasury={}",
            side,
            friendly.len(),
            enemy.len(),
            territory_ratio * 100.0,
            has_emergency,
            db.persisted.treasury(side),
        );

        let scored = score_actions(db, side, sc_cfg, events_cfg, &friendly, &enemy, scheduler, &alcm_groups, ts);
        if let Some((action, cost)) = select_best_action(&scored, &friendly) {
            set_side_check_time(scheduler, side, ts);

            let action_label = match &action {
                CommanderAction::Barrage { .. } => "barrage",
                CommanderAction::MissileStrike { .. } => "missile strike",
                CommanderAction::Ambush => "ambush",
                CommanderAction::DispatchCap => "dispatch CAP",
            };
            info!(
                "[Commander] {:?} spending {} treasury on {} (emergency={})",
                side, cost, action_label, has_emergency
            );
            db.persisted.adjust_treasury(side, -cost);

            match action {
                CommanderAction::Barrage { src_oid, src_side, target_oid, target_pos, target_name } => {
                    scheduler.spawn_barrage_event(
                        events_cfg,
                        ts,
                        src_oid,
                        src_side,
                        target_oid,
                        target_pos,
                        target_name,
                        &mut messages,
                    )
                }
                CommanderAction::MissileStrike { side, shooter_gids, target_pos, target_name } => {
                    scheduler.spawn_missile_strike_event(
                        events_cfg,
                        ts,
                        side,
                        shooter_gids,
                        target_pos,
                        target_name,
                        &mut messages,
                    )
                }
                CommanderAction::Ambush => scheduler.spawn_convoy_ambush(
                    db,
                    events_cfg,
                    ts,
                    &candidates,
                    &mut messages,
                    &mut effects,
                ),
                CommanderAction::DispatchCap => scheduler.spawn_commander_cap(
                    db,
                    events_cfg,
                    ts,
                    side,
                    &mut messages,
                ),
            }
            db.ephemeral.dirty();
            // One event per tick — stop after the first successful spawn.
            break;
        } else {
            info!(
                "[Commander] {:?} no affordable action (treasury={})",
                side,
                db.persisted.treasury(side)
            );
        }
    }

    (messages, effects)
}

fn tick_treasury_income(db: &mut Db, cfg: &SmartCommanderCfg, ts: DateTime<Utc>) {
    let elapsed = (ts - db.ephemeral.last_treasury_income).num_seconds();
    if elapsed < cfg.treasury_income_period_secs as i64 {
        return;
    }
    db.ephemeral.last_treasury_income = ts;
    for side in [Side::Blue, Side::Red] {
        let bal = db.persisted.adjust_treasury(side, cfg.treasury_income_amount);
        info!(
            "[Commander] {:?} treasury income +{} → {}",
            side, cfg.treasury_income_amount, bal
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
        info!(
            "[Commander] {:?} funded {} +{} pts (treasury → {})",
            side, name, actual, db.persisted.treasury(side)
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
            db.adjust_points_silent(ucid, bonus, "holding bonus");
        }
        info!(
            "[Commander] {:?} holding bonus +{} pts ({}/{} objectives)",
            side, bonus, owned, total
        );
    }
}
