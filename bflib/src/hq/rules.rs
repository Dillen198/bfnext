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

//! The HQ's rules mode: what the AI buys with a side's treasury on a server
//! that runs the Smart Commander without the theatre HQ
//! (`smart_commander.hq` off or absent).
//!
//! This was the Smart Commander's own event picker (`commander::tick_events`),
//! a second planner next to the HQ's. It is now the HQ's mode for that case:
//! `hq::tick` runs it instead of the full planning pass, it reads its
//! objectives from `hq::picture`, and it pays through `hq::treasury` like
//! every other AI or human purchase. What it decides is unchanged: one
//! strategic campaign event per check (barrage, missile strike, ambush or a
//! CAP flight), the best value per treasury point, on the same config keys
//! (`smart_commander.tick_period_secs`, `barrage_cost`, `ambush_cost`,
//! `cap_cost`, `cap_cooldown_secs`, `cap_min_friendly_pilots`;
//! `campaign_events.enabled`, `check_interval_secs`,
//! `max_concurrent_events`, `barrage_enabled`, `ambush_enabled`,
//! `enemy_cap_enabled`).

use super::picture::{self, ObjSummary};
use crate::{
    Context,
    db::events::{CampaignEvent, EventScheduler},
};
use bfprotocols::{
    cfg::{CampaignEventsCfg, SmartCommanderCfg},
    db::{group::GroupId, objective::ObjectiveId},
};
use chrono::prelude::*;
use compact_str::CompactString;
use dcso3::{Vector2, coalition::Side};
use log::info;
use rand::Rng;
use smallvec::SmallVec;
use std::cmp::Ordering;

/// How often a side with an objective in danger is looked at, whatever
/// `check_interval_secs` says.
const EMERGENCY_INTERVAL_SECS: i64 = 30;

/// Cruise missile strikes cost this much more than a barrage.
const MISSILE_COST_FACTOR: f64 = 1.5;

/// What the rules mode can buy, with what it needs to create it.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Buy {
    /// Source artillery objective -> target enemy objective.
    Barrage {
        src_oid: ObjectiveId,
        src_side: Side,
        target_oid: ObjectiveId,
        target_pos: Vector2,
        target_name: CompactString,
    },
    /// ALCM / Scud / HIMARS groups fire at a high-value target.
    MissileStrike {
        side: Side,
        shooter_gids: SmallVec<[GroupId; 4]>,
        target_pos: Vector2,
        target_name: CompactString,
    },
    Ambush,
    /// A friendly AI CAP flight when the enemy has more jets up.
    DispatchCap,
}

impl Buy {
    fn label(&self) -> &'static str {
        match self {
            Buy::Barrage { .. } => "barrage",
            Buy::MissileStrike { .. } => "missile strike",
            Buy::Ambush => "ambush",
            Buy::DispatchCap => "dispatch CAP",
        }
    }
}

/// Everything the scorer reads besides the two sides' objectives, gathered
/// from the db first so that the scoring itself is a pure function.
#[derive(Debug, Clone, Default)]
pub(crate) struct Inputs {
    pub(crate) treasury: i64,
    /// `artillery_mission_range` and `alcm_mission_range`, metres.
    pub(crate) arty_range: f64,
    pub(crate) alcm_range: f64,
    /// The side's missile groups and where they are.
    pub(crate) missile_groups: SmallVec<[(GroupId, Option<Vector2>); 4]>,
    /// The side already has a missile strike running.
    pub(crate) missile_strike_running: bool,
    /// The enemy's convoys on the road.
    pub(crate) enemy_convoys: usize,
    /// A Smart Commander CAP, or a fixed-wing reactive CAP, already covers
    /// the side.
    pub(crate) cap_covered: bool,
    /// When the side's last Smart Commander CAP ended.
    pub(crate) cap_last_ended: Option<DateTime<Utc>>,
    /// Players in fixed-wing aircraft, ours and theirs.
    pub(crate) friendly_air: u32,
    pub(crate) enemy_air: u32,
}

impl Inputs {
    pub(crate) fn gather(ctx: &Context, side: Side) -> Self {
        let db = &ctx.db;
        let scheduler = &ctx.event_scheduler;
        Inputs {
            treasury: db.persisted.treasury(side),
            arty_range: db.ephemeral.cfg.artillery_mission_range as f64,
            alcm_range: db.ephemeral.cfg.alcm_mission_range as f64,
            missile_groups: picture::missile_groups(db, side),
            missile_strike_running: scheduler
                .active_events
                .iter()
                .any(|e| matches!(e, CampaignEvent::MissileStrike { side: s, .. } if *s == side)),
            enemy_convoys: db.convoy_count_for_side(side.opposite()),
            cap_covered: scheduler.active_events.iter().any(|e| match e {
                // The CAP side of an EnemyCap is the side that owns (flies)
                // it, i.e. the defending side: ours.
                CampaignEvent::CommanderCap { cap_side: s, .. } => *s == side,
                // A rotary patrol is not CAP -- it answers helicopters and
                // cannot cover jets, so it must not block a fixed-wing flight.
                CampaignEvent::EnemyCap { cap_side: s, rotary, .. } => *s == side && !*rotary,
                _ => false,
            }),
            cap_last_ended: match side {
                Side::Blue => scheduler.last_commander_cap_ended_blue,
                Side::Red => scheduler.last_commander_cap_ended_red,
                Side::Neutral => None,
            },
            friendly_air: picture::fixedwing_pilots(db, side),
            enemy_air: picture::fixedwing_pilots(db, side.opposite()),
        }
    }
}

/// Damage and shortage at a target, the core of every strike's value.
fn weakness(t: &ObjSummary) -> f64 {
    (100u8.saturating_sub(t.health)) as f64 * 0.8
        + (100u8.saturating_sub(t.supply)) as f64 * 0.4
}

/// Score everything `side` could buy now: (what, cost, value).
pub(crate) fn score(
    side: Side,
    sc: &SmartCommanderCfg,
    ev: &CampaignEventsCfg,
    inp: &Inputs,
    friendly: &[ObjSummary],
    enemy: &[ObjSummary],
    now: DateTime<Utc>,
) -> Vec<(Buy, i64, f64)> {
    let mut out: Vec<(Buy, i64, f64)> = Vec::new();

    // Barrage: the best (friendly artillery base, enemy target) pair within
    // gun range, by enemy weakness, proximity and strategic value.
    if ev.barrage_enabled && inp.treasury >= sc.barrage_cost {
        let mut best: Option<(Buy, f64)> = None;
        for src in friendly.iter().filter(|o| o.has_fire_groups) {
            for tgt in enemy {
                let d = super::dist(src.pos, tgt.pos);
                if d > inp.arty_range || d < 5_000.0 {
                    continue;
                }
                let proximity = if d < inp.arty_range * 0.5 { 1.5 } else { 1.0 };
                let strategic = if tgt.is_logi_hub {
                    1.5
                } else if tgt.is_factory {
                    1.2
                } else {
                    1.0
                };
                let capturable = if tgt.capturable { 40.0 } else { 0.0 };
                let value = (20.0 + weakness(tgt) + capturable) * proximity * strategic;
                if best.as_ref().is_none_or(|(_, v)| value > *v) {
                    best = Some((
                        Buy::Barrage {
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
        if let Some((b, value)) = best {
            out.push((b, sc.barrage_cost, value));
        }
    }

    // Missile strike: missiles reach deep, so no proximity term; strategic
    // and capturable targets are worth more.
    if ev.barrage_enabled
        && inp.treasury >= sc.barrage_cost
        && !inp.missile_groups.is_empty()
        && !inp.missile_strike_running
    {
        let in_range = |gpos: &Option<Vector2>, tpos: Vector2| {
            gpos.is_some_and(|g| super::dist(g, tpos) <= inp.alcm_range)
        };
        let mut best: Option<(Buy, f64)> = None;
        for tgt in enemy {
            let shooters: SmallVec<[GroupId; 4]> = inp
                .missile_groups
                .iter()
                .filter(|(_, g)| in_range(g, tgt.pos))
                .map(|(gid, _)| *gid)
                .collect();
            if shooters.is_empty() {
                continue;
            }
            let strategic = if tgt.is_logi_hub {
                2.0
            } else if tgt.is_factory {
                1.6
            } else {
                1.0
            };
            let capturable = if tgt.capturable { 60.0 } else { 0.0 };
            let value = (30.0 + weakness(tgt) + capturable) * strategic;
            if best.as_ref().is_none_or(|(_, v)| value > *v) {
                best = Some((
                    Buy::MissileStrike {
                        side,
                        shooter_gids: shooters,
                        target_pos: tgt.pos,
                        target_name: tgt.name.clone(),
                    },
                    value,
                ));
            }
        }
        if let Some((b, value)) = best {
            let cost = (sc.barrage_cost as f64 * MISSILE_COST_FACTOR).round() as i64;
            if inp.treasury >= cost {
                out.push((b, cost, value));
            }
        }
    }

    // Ambush: worth most when the enemy has convoys out and is short of
    // supply already.
    if ev.ambush_enabled && inp.treasury >= sc.ambush_cost && inp.enemy_convoys > 0 {
        let enemy_avg_supply = if enemy.is_empty() {
            100.0
        } else {
            enemy.iter().map(|o| o.supply as f64).sum::<f64>() / enemy.len() as f64
        };
        let value =
            30.0 + inp.enemy_convoys as f64 * 20.0 + (100.0 - enemy_avg_supply) * 0.6;
        out.push((Buy::Ambush, sc.ambush_cost, value));
    }

    // CAP: only when it is priced, affordable and allowed, nothing covers
    // the side already, the cooldown since the last one is over, at least
    // three jets are up on one side, and the enemy has at least
    // `cap_min_friendly_pilots` more than we do.
    if sc.cap_cost > 0
        && inp.treasury >= sc.cap_cost
        && ev.enemy_cap_enabled
        && !inp.cap_covered
    {
        let cooled = inp
            .cap_last_ended
            .is_none_or(|t| (now - t).num_seconds() >= sc.cap_cooldown_secs as i64);
        if cooled && inp.friendly_air.max(inp.enemy_air) >= 3 {
            let gap = inp.enemy_air.saturating_sub(inp.friendly_air);
            if gap >= sc.cap_min_friendly_pilots {
                let threatened =
                    friendly.iter().filter(|o| o.threatened || o.capturable).count();
                let value = 40.0 + gap as f64 * 15.0 + threatened as f64 * 10.0;
                out.push((Buy::DispatchCap, sc.cap_cost, value));
            }
        }
    }
    out
}

/// The best value per treasury point.
pub(crate) fn best(scored: &[(Buy, i64, f64)]) -> Option<(Buy, i64)> {
    scored
        .iter()
        .max_by(|(_, ca, va), (_, cb, vb)| {
            let ea = va / (*ca).max(1) as f64;
            let eb = vb / (*cb).max(1) as f64;
            ea.partial_cmp(&eb).unwrap_or(Ordering::Equal)
        })
        .map(|(b, cost, _)| (b.clone(), *cost))
}

/// Is `side` due a look? Every `interval` seconds, every
/// `EMERGENCY_INTERVAL_SECS` while one of its objectives is in danger, and at
/// once if it was never looked at.
pub(crate) fn due(
    last: Option<DateTime<Utc>>,
    now: DateTime<Utc>,
    interval: u32,
    emergency: bool,
) -> bool {
    let elapsed = last.map_or(i64::MAX, |t| (now - t).num_seconds());
    elapsed >= if emergency { EMERGENCY_INTERVAL_SECS } else { interval as i64 }
}

fn last_check(
    scheduler: &mut EventScheduler,
    side: Side,
) -> Option<&mut Option<DateTime<Utc>>> {
    match side {
        Side::Blue => Some(&mut scheduler.last_commander_check_blue),
        Side::Red => Some(&mut scheduler.last_commander_check_red),
        Side::Neutral => None,
    }
}

/// The rules mode's pass: at most one event bought, for one side, per
/// `smart_commander.tick_period_secs` (the gate is `ctx.hq.rules_last`).
/// Messages go to everyone, effects to the campaign event queue.
pub(crate) fn tick(ctx: &mut Context, now: DateTime<Utc>) {
    let Some(sc) = ctx.db.ephemeral.cfg.smart_commander.clone() else { return };
    if (now - ctx.hq.rules_last).num_seconds() < sc.tick_period_secs as i64 {
        return;
    }
    ctx.hq.rules_last = now;
    let Some(ev) = ctx.db.ephemeral.cfg.campaign_events.clone().filter(|e| e.enabled)
    else {
        return;
    };
    let mut messages: Vec<CompactString> = Vec::new();
    let mut effects = Vec::new();
    let candidates = ctx.event_scheduler.build_candidates(&ctx.db);
    let red = candidates.iter().filter(|(_, s, ..)| *s == Side::Red).count();
    let blue = candidates.iter().filter(|(_, s, ..)| *s == Side::Blue).count();
    let total = (red + blue).max(1);
    // Neither side always goes first.
    let mut sides = [Side::Blue, Side::Red];
    if rand::thread_rng().r#gen::<bool>() {
        sides.swap(0, 1);
    }
    for side in sides {
        let friendly = picture::side_objectives(&ctx.db, side);
        let enemy = picture::side_objectives(&ctx.db, side.opposite());
        let emergency = friendly.iter().any(|o| o.being_captured || o.capturable);
        let capture = friendly.iter().any(|o| o.being_captured);
        // The cap on concurrent events is per side, and a base actually
        // being captured gets through it.
        let running =
            ctx.event_scheduler.active_events.iter().filter(|e| e.side() == side).count();
        if running >= ev.max_concurrent_events as usize && !capture {
            continue;
        }
        let Some(last) = last_check(&mut ctx.event_scheduler, side) else { continue };
        if !due(*last, now, ev.check_interval_secs, emergency) {
            continue;
        }
        // The check counts whether or not it buys anything, so a side with
        // nothing to buy waits out its interval like any other.
        *last = Some(now);
        info!(
            "[Commander] {:?} assessment: {} friendly objs, {} enemy, {:.0}% territory, \
             emergency={}, treasury={}",
            side,
            friendly.len(),
            enemy.len(),
            friendly.len() as f64 / total as f64 * 100.0,
            emergency,
            ctx.db.persisted.treasury(side),
        );
        let inp = Inputs::gather(ctx, side);
        let scored = score(side, &sc, &ev, &inp, &friendly, &enemy, now);
        let Some((buy, cost)) = best(&scored) else {
            info!(
                "[Commander] {:?} scored no candidate actions (treasury={}) -- \
                 barrage: enabled={} cost={}; missile: needs barrage_enabled={} cost={} \
                 and a deployed ALCM/Scud group ({} available); ambush: enabled={} cost={}; \
                 cap: enabled={} cost={}. Treasury is not the limit where it exceeds the cost -- \
                 there were no valid targets.",
                side,
                inp.treasury,
                ev.barrage_enabled,
                sc.barrage_cost,
                ev.barrage_enabled,
                sc.barrage_cost,
                inp.missile_groups.len(),
                ev.ambush_enabled,
                sc.ambush_cost,
                ev.enemy_cap_enabled,
                sc.cap_cost,
            );
            continue;
        };
        let label = buy.label();
        // Paid only for an event that was actually created.
        let scheduler = &mut ctx.event_scheduler;
        let created = match buy {
            Buy::Barrage { src_oid, src_side, target_oid, target_pos, target_name } => {
                scheduler.spawn_barrage_event(
                    &ev,
                    now,
                    src_oid,
                    src_side,
                    target_oid,
                    target_pos,
                    target_name,
                    &mut messages,
                );
                true
            }
            Buy::MissileStrike { side, shooter_gids, target_pos, target_name } => {
                scheduler.spawn_missile_strike_event(
                    &ev,
                    now,
                    side,
                    shooter_gids,
                    target_pos,
                    target_name,
                    &mut messages,
                );
                true
            }
            Buy::Ambush => scheduler.spawn_convoy_ambush(
                &ctx.db,
                &ev,
                now,
                side,
                &candidates,
                &mut messages,
                &mut effects,
                None,
            ),
            Buy::DispatchCap => {
                scheduler.spawn_commander_cap(&ctx.db, &ev, now, side, &mut messages)
            }
        };
        if !created {
            info!(
                "[Commander] {:?} {} found nothing to act on, treasury not charged",
                side, label
            );
            continue;
        }
        info!(
            "[Commander] {:?} spending {} treasury on {} (emergency={})",
            side, cost, label, emergency
        );
        super::treasury::pay(&mut ctx.db, side, cost);
        // One event per pass.
        break;
    }
    for msg in messages {
        ctx.db.ephemeral.msgs().panel_to_all(15, false, msg);
    }
    ctx.event_scheduler.pending_effects.extend(effects);
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Duration;

    fn t0() -> DateTime<Utc> {
        Utc.with_ymd_and_hms(2026, 10, 27, 12, 0, 0).unwrap()
    }

    fn obj(id: i64, x: f64, health: u8, supply: u8) -> ObjSummary {
        ObjSummary {
            oid: ObjectiveId::from(id),
            pos: Vector2::new(x, 0.),
            health,
            supply,
            threatened: false,
            being_captured: false,
            capturable: false,
            is_logi_hub: false,
            is_factory: false,
            has_fire_groups: false,
            name: CompactString::from(format!("obj{id}")),
        }
    }

    fn cfgs() -> (SmartCommanderCfg, CampaignEventsCfg) {
        // every key has a serde default
        let mut sc: SmartCommanderCfg = serde_json::from_str("{}").unwrap();
        sc.barrage_cost = 100;
        sc.ambush_cost = 100;
        sc.cap_cost = 100;
        sc.cap_cooldown_secs = 600;
        sc.cap_min_friendly_pilots = 2;
        let mut ev = CampaignEventsCfg::default();
        ev.enabled = true;
        ev.barrage_enabled = true;
        ev.ambush_enabled = true;
        ev.enemy_cap_enabled = true;
        (sc, ev)
    }

    fn inputs() -> Inputs {
        Inputs {
            treasury: 1000,
            arty_range: 30_000.,
            alcm_range: 200_000.,
            ..Default::default()
        }
    }

    #[test]
    fn barrage_picks_the_weakest_target_in_gun_range() {
        let (sc, ev) = cfgs();
        let mut src = obj(1, 0., 100, 100);
        src.has_fire_groups = true;
        // too close (< 5 km), weak; in range, healthy; in range, weak; out of range
        let enemy = [
            obj(10, 4_000., 0, 0),
            obj(11, 20_000., 100, 100),
            obj(12, 25_000., 10, 50),
            obj(13, 40_000., 0, 0),
        ];
        let scored = score(Side::Blue, &sc, &ev, &inputs(), &[src], &enemy, t0());
        let barrage =
            scored.iter().find(|(b, ..)| matches!(b, Buy::Barrage { .. })).unwrap();
        match &barrage.0 {
            Buy::Barrage { target_oid, .. } => {
                assert_eq!(*target_oid, ObjectiveId::from(12))
            }
            _ => unreachable!(),
        }
        // (20 + 90*0.8 + 50*0.4) * 1.0 (not within half range) * 1.0
        assert!((barrage.2 - 112.0).abs() < 1e-9);
        assert_eq!(barrage.1, 100);
    }

    #[test]
    fn nothing_without_the_money_or_the_switch() {
        let (sc, mut ev) = cfgs();
        let mut src = obj(1, 0., 100, 100);
        src.has_fire_groups = true;
        let enemy = [obj(12, 25_000., 10, 50)];
        let poor = Inputs { treasury: 99, ..inputs() };
        assert!(
            score(Side::Blue, &sc, &ev, &poor, &[src.clone()], &enemy, t0()).is_empty()
        );
        ev.barrage_enabled = false;
        assert!(score(Side::Blue, &sc, &ev, &inputs(), &[src], &enemy, t0()).is_empty());
    }

    #[test]
    fn missile_strike_needs_a_shooter_in_range_and_costs_more() {
        let (sc, ev) = cfgs();
        let enemy = [obj(20, 150_000., 50, 50), obj(21, 300_000., 0, 0)];
        let gid = GroupId::from(7);
        let mut inp = inputs();
        inp.missile_groups.push((gid, Some(Vector2::new(0., 0.))));
        let scored = score(Side::Red, &sc, &ev, &inp, &[], &enemy, t0());
        let (b, cost, _) =
            scored.iter().find(|(b, ..)| matches!(b, Buy::MissileStrike { .. })).unwrap();
        assert_eq!(*cost, 150);
        match b {
            Buy::MissileStrike { shooter_gids, target_pos, .. } => {
                assert_eq!(shooter_gids.as_slice(), &[gid]);
                assert_eq!(*target_pos, Vector2::new(150_000., 0.));
            }
            _ => unreachable!(),
        }
        // one already running: none
        inp.missile_strike_running = true;
        assert!(score(Side::Red, &sc, &ev, &inp, &[], &enemy, t0()).is_empty());
        // a group with no known position can't shoot
        inp.missile_strike_running = false;
        inp.missile_groups[0].1 = None;
        assert!(score(Side::Red, &sc, &ev, &inp, &[], &enemy, t0()).is_empty());
    }

    #[test]
    fn ambush_needs_enemy_convoys() {
        let (sc, ev) = cfgs();
        let enemy = [obj(30, 0., 100, 40), obj(31, 0., 100, 60)];
        assert!(score(Side::Blue, &sc, &ev, &inputs(), &[], &enemy, t0()).is_empty());
        let inp = Inputs { enemy_convoys: 2, ..inputs() };
        let scored = score(Side::Blue, &sc, &ev, &inp, &[], &enemy, t0());
        assert_eq!(scored.len(), 1);
        // 30 + 2*20 + (100 - 50)*0.6
        assert_eq!(scored[0].0, Buy::Ambush);
        assert!((scored[0].2 - 100.0).abs() < 1e-9);
    }

    #[test]
    fn cap_balances_the_air_and_waits_out_its_cooldown() {
        let (sc, ev) = cfgs();
        let mut threatened = obj(1, 0., 100, 100);
        threatened.threatened = true;
        let mut inp = Inputs { friendly_air: 1, enemy_air: 4, ..inputs() };
        let scored = score(Side::Blue, &sc, &ev, &inp, &[threatened.clone()], &[], t0());
        assert_eq!(scored.len(), 1);
        // 40 + 3*15 + 1*10
        assert!((scored[0].2 - 95.0).abs() < 1e-9);
        // gap below cap_min_friendly_pilots
        inp.friendly_air = 3;
        assert!(score(Side::Blue, &sc, &ev, &inp, &[], &[], t0()).is_empty());
        // fewer than three jets up anywhere: not worth balancing
        let small = Inputs { friendly_air: 0, enemy_air: 2, ..inputs() };
        assert!(score(Side::Blue, &sc, &ev, &small, &[], &[], t0()).is_empty());
        // covered already, or inside the cooldown
        inp.friendly_air = 1;
        inp.cap_covered = true;
        assert!(score(Side::Blue, &sc, &ev, &inp, &[], &[], t0()).is_empty());
        inp.cap_covered = false;
        inp.cap_last_ended = Some(t0() - Duration::seconds(599));
        assert!(score(Side::Blue, &sc, &ev, &inp, &[], &[], t0()).is_empty());
        inp.cap_last_ended = Some(t0() - Duration::seconds(600));
        assert_eq!(score(Side::Blue, &sc, &ev, &inp, &[], &[], t0()).len(), 1);
    }

    #[test]
    fn best_is_value_per_point() {
        let scored = vec![(Buy::Ambush, 100, 100.0), (Buy::DispatchCap, 50, 60.0)];
        assert_eq!(best(&scored), Some((Buy::DispatchCap, 50)));
        assert_eq!(best(&[]), None);
        // a free buy doesn't divide by zero
        assert_eq!(best(&[(Buy::Ambush, 0, 1.0)]), Some((Buy::Ambush, 0)));
    }

    #[test]
    fn due_every_interval_or_sooner_in_an_emergency() {
        let now = t0();
        assert!(due(None, now, 300, false));
        assert!(!due(Some(now - Duration::seconds(299)), now, 300, false));
        assert!(due(Some(now - Duration::seconds(300)), now, 300, false));
        assert!(!due(Some(now - Duration::seconds(29)), now, 300, true));
        assert!(due(Some(now - Duration::seconds(30)), now, 300, true));
    }
}
