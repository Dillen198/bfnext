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

//! The AI ground commander. Every `think_secs`, for each side it commands,
//! in order of urgency:
//!
//! 1. Pull back what is too battered to fight (below `withdraw_strength_pct`).
//! 2. Counter-attack: send the nearest idle formation to a friendly base
//!    that is being captured, or that has enemy armour closing on it.
//! 3. Raise a new formation from the garrison nearest the front, if the side
//!    is below its cap.
//! 4. Attack: during an offensive (always, without `modern_war.tempo`), pick
//!    the objective -- the tempo's axis when there is one -- and send up to
//!    `concentration` formations at it, infantry-carrying ones first, keeping
//!    `reserve` back.
//! 5. Screen: whatever is still idle at home moves up to the friendly base
//!    nearest the enemy.
//!
//! It never touches a formation a player is commanding (the order call
//! refuses), and it only re-tasks formations that have finished or lost
//! their order, so it doesn't flip-flop.

use super::at_sea;
use crate::{
    db::formation::{FormationId, Order, Posture},
    modern_war::tempo::{phase_at, Phase},
    Context,
};
use bfprotocols::{
    cfg::{GroundAiCfg, GroundWarCfg},
    db::objective::{ObjectiveId, ObjectiveKind},
};
use chrono::prelude::*;
use compact_str::format_compact;
use dcso3::{coalition::Side, MizLua, Vector2};
use log::{debug, info};

fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

/// An enemy formation this close to a friendly base is a threat to it.
const THREAT_M: f64 = 15_000.;
/// How far the AI will send a formation to answer a threat.
const RESPONSE_M: f64 = 60_000.;
/// Fresh enough to attack (percent of starting strength).
const ATTACK_MIN_PCT: u32 = 60;
/// Most counter-attacks ordered per side per think.
const MAX_RESPONSES: usize = 2;

#[derive(Debug, Clone)]
struct Obj {
    id: ObjectiveId,
    name: dcso3::String,
    pos: Vector2,
    owner: Side,
    kind: ObjectiveKind,
    threatened: bool,
    health: u8,
    being_captured: bool,
}

#[derive(Debug, Clone)]
struct Form {
    id: FormationId,
    pos: Vector2,
    order: Order,
    posture: Posture,
    pct: u32,
    ai: bool,
    infantry: bool,
    home: ObjectiveId,
}

impl Form {
    /// Done with what it was told, or never told anything.
    fn idle(&self) -> bool {
        self.posture == Posture::Holding && !matches!(self.order, Order::Withdraw(_))
    }
}

fn objs(ctx: &Context) -> Vec<Obj> {
    ctx.db
        .objectives()
        .filter(|(_, o)| !at_sea(o.kind()))
        .map(|(id, o)| Obj {
            id: *id,
            name: o.name.clone(),
            pos: o.pos(),
            owner: o.owner(),
            kind: o.kind().clone(),
            threatened: o.threatened(),
            health: o.health(),
            being_captured: ctx.db.capture_in_progress(id),
        })
        .collect()
}

fn forms(ctx: &Context, side: Side, now: DateTime<Utc>) -> Vec<Form> {
    ctx.db
        .formations()
        .filter(|f| f.side == side)
        .map(|f| Form {
            id: f.id,
            pos: f.pos,
            order: f.order,
            posture: f.posture,
            pct: ctx.db.formation_strength_pct(f),
            ai: f.ai_controlled(now),
            infantry: ctx.db.formation_has_infantry(f),
            home: f.home,
        })
        .collect()
}

fn order(lua: MizLua, ctx: &mut Context, id: FormationId, order: Order, now: DateTime<Utc>) -> bool {
    match ctx.db.order_formation(&mut ctx.groundwar.rt, lua, id, order, None, now) {
        Ok(what) => {
            info!("ground war AI: {what}");
            true
        }
        Err(e) => {
            debug!("ground war AI: order {order:?} for {id} refused: {e:?}");
            false
        }
    }
}

/// Worth of taking an objective, for target choice.
fn value(kind: &ObjectiveKind) -> f64 {
    match kind {
        ObjectiveKind::Logistics => 1.6,
        ObjectiveKind::Factory { .. } => 1.4,
        ObjectiveKind::Airbase => 1.3,
        ObjectiveKind::CommandCenter => 1.2,
        ObjectiveKind::Fob | ObjectiveKind::Farp { .. } => 1.0,
        ObjectiveKind::NavalBase => 0.9,
        // A SAM site is a strike target, not a ground objective.
        ObjectiveKind::SpecialSamSite { .. } => 0.4,
        ObjectiveKind::CarrierGroup { .. } => 0.,
    }
}

/// The objective `side`'s ground forces should go for: the tempo axis when
/// there is one in reach, otherwise the best value within `max_attack_m` of
/// ground the side holds, weak ones first.
fn pick_target(objs: &[Obj], side: Side, axis: Option<ObjectiveId>, max_m: f64) -> Option<ObjectiveId> {
    let own: Vec<Vector2> = objs.iter().filter(|o| o.owner == side).map(|o| o.pos).collect();
    let gap = |p: Vector2| own.iter().map(|q| dist(*q, p)).fold(f64::INFINITY, f64::min);
    if let Some(a) = axis.and_then(|a| objs.iter().find(|o| o.id == a)) {
        if a.owner != side && gap(a.pos) <= max_m {
            return Some(a.id);
        }
    }
    objs.iter()
        .filter(|o| o.owner != side && value(&o.kind) > 0.)
        .filter_map(|o| {
            let g = gap(o.pos);
            (g <= max_m).then(|| {
                // A wrecked base is a cheaper fight.
                let weakness = 0.5 + o.health as f64 / 200.;
                (o.id, g * weakness / value(&o.kind))
            })
        })
        .min_by(|a, b| a.1.total_cmp(&b.1))
        .map(|(id, _)| id)
}

pub(super) fn think(lua: MizLua, ctx: &mut Context, cfg: &GroundWarCfg, ai: &GroundAiCfg, now: DateTime<Utc>) {
    let tempo = ctx
        .db
        .ephemeral
        .cfg
        .modern_war
        .as_ref()
        .and_then(|m| m.tempo.clone())
        .filter(|t| t.enabled);
    // Nobody online: no new offensives, so the map isn't rolled up
    // overnight. What is already under way carries on, and threatened bases
    // are still defended.
    let empty = ai.pause_when_empty && ctx.db.instanced_players().next().is_none();
    for side in ai.sides.iter().copied().filter(|s| *s != Side::Neutral) {
        let (offensive, axis) = match (&tempo, ai.follow_tempo) {
            (Some(t), true) => (
                phase_at(t, side, now).0 == Phase::Offensive,
                ctx.modern_war.tempo.axis(side),
            ),
            _ => (true, None),
        };
        let offensive = offensive && !empty;
        think_side(lua, ctx, cfg, ai, side, offensive, axis, now);
    }
}

#[allow(clippy::too_many_arguments)]
fn think_side(
    lua: MizLua,
    ctx: &mut Context,
    cfg: &GroundWarCfg,
    ai: &GroundAiCfg,
    side: Side,
    offensive: bool,
    axis: Option<ObjectiveId>,
    now: DateTime<Utc>,
) {
    let objs = objs(ctx);
    let owned: Vec<&Obj> = objs.iter().filter(|o| o.owner == side).collect();
    let nearest_owned = |p: Vector2| {
        owned
            .iter()
            .min_by(|a, b| dist(a.pos, p).total_cmp(&dist(b.pos, p)))
            .map(|o| o.id)
    };
    let front_dist = |p: Vector2| {
        objs.iter()
            .filter(|o| o.owner == side.opposite())
            .map(|o| dist(o.pos, p))
            .fold(f64::INFINITY, f64::min)
    };

    // 1. Pull back the battered.
    for f in forms(ctx, side, now) {
        if f.ai && f.pct < cfg.withdraw_strength_pct as u32 && !matches!(f.order, Order::Withdraw(_)) {
            let home_ok = owned.iter().any(|o| o.id == f.home);
            let dest = if home_ok { Some(f.home) } else { nearest_owned(f.pos) };
            if let Some(dest) = dest {
                order(lua, ctx, f.id, Order::Withdraw(dest), now);
            }
        }
    }

    // 2. Counter-attack bases under threat.
    let enemy_forms: Vec<Vector2> =
        ctx.db.formations().filter(|f| f.side != side).map(|f| f.pos).collect();
    let mut responses = 0;
    for o in owned.iter() {
        if responses >= MAX_RESPONSES {
            break;
        }
        let armour_near = enemy_forms.iter().any(|p| dist(*p, o.pos) <= THREAT_M);
        if !(o.being_captured || (o.threatened && armour_near)) {
            continue;
        }
        let fs = forms(ctx, side, now);
        let covered = fs.iter().any(|f| {
            f.order == Order::Defend(o.id) || (f.idle() && dist(f.pos, o.pos) <= cfg.arrive_m * 2.)
        });
        if covered {
            continue;
        }
        let pick = fs
            .iter()
            .filter(|f| f.ai && f.idle() && f.pct >= cfg.withdraw_strength_pct as u32)
            .filter(|f| dist(f.pos, o.pos) <= RESPONSE_M)
            .min_by(|a, b| dist(a.pos, o.pos).total_cmp(&dist(b.pos, o.pos)));
        if let Some(f) = pick {
            if order(lua, ctx, f.id, Order::Defend(o.id), now) {
                responses += 1;
                if cfg.announce {
                    ctx.db.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        side,
                        format_compact!("GROUND COMMAND: counter-attacking at {}.", o.name),
                    );
                }
            }
        }
    }

    if owned.len() < ai.min_objectives as usize {
        return;
    }

    // 3. Raise a formation from the garrison nearest the front.
    let in_field = ctx.db.formations().filter(|f| f.side == side).count();
    let want_more = in_field < cfg.max_formations_per_side as usize
        && (offensive || in_field <= ai.reserve as usize);
    if want_more {
        let mut cands: Vec<(&Obj, f64)> = owned
            .iter()
            .filter(|o| !o.threatened && !o.being_captured)
            .map(|o| (*o, front_dist(o.pos)))
            .filter(|(_, d)| d.is_finite())
            .collect();
        cands.sort_by(|a, b| a.1.total_cmp(&b.1));
        for (o, _) in cands {
            let ok = ctx
                .db
                .formation_candidates(cfg, &o.id, side)
                .map_or(false, |g| !g.is_empty());
            if !ok {
                continue;
            }
            match ctx.db.raise_formation(&mut ctx.groundwar.rt, lua, side, o.id, None, now) {
                Ok(id) => {
                    let name = ctx.db.formation(id).map(|f| f.name.clone()).unwrap_or_default();
                    info!("ground war AI: {side:?} raised {name} at {}", o.name);
                    if cfg.announce {
                        ctx.db.ephemeral.msgs().panel_to_side(
                            15,
                            false,
                            side,
                            format_compact!("GROUND COMMAND: {name} formed up at {}.", o.name),
                        );
                    }
                    break;
                }
                Err(e) => debug!("ground war AI: raising at {}: {e:?}", o.name),
            }
        }
    }

    // 4. Attack.
    if offensive {
        if let Some(target) = pick_target(&objs, side, axis, ai.max_attack_m) {
            let tpos = objs.iter().find(|o| o.id == target).map(|o| o.pos).unwrap_or_default();
            let tname = objs.iter().find(|o| o.id == target).map(|o| o.name.clone()).unwrap_or_default();
            let fs = forms(ctx, side, now);
            let attacking = fs.iter().filter(|f| f.order == Order::Attack(target)).count();
            let need = (ai.concentration as usize).saturating_sub(attacking);
            let mut idle: Vec<&Form> = fs
                .iter()
                .filter(|f| f.ai && f.idle() && f.pct >= ATTACK_MIN_PCT)
                .filter(|f| !matches!(f.order, Order::Attack(_)))
                .collect();
            // Infantry first -- only infantry can take the place -- then
            // nearest.
            idle.sort_by(|a, b| {
                (!a.infantry, dist(a.pos, tpos))
                    .partial_cmp(&(!b.infantry, dist(b.pos, tpos)))
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            let spare = idle.len().saturating_sub(ai.reserve as usize);
            let mut sent = 0;
            for f in idle.into_iter().take(need.min(spare)) {
                if order(lua, ctx, f.id, Order::Attack(target), now) {
                    sent += 1;
                }
            }
            if sent > 0 && cfg.announce {
                ctx.db.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side,
                    format_compact!("GROUND COMMAND: {sent} formation(s) advancing on {tname}."),
                );
            }
        }
    }

    // 5. Screen: idle formations still sitting at home move up to the
    //    friendly base nearest the enemy around them.
    for f in forms(ctx, side, now) {
        if !(f.ai && f.idle() && f.order == Order::Hold) {
            continue;
        }
        let screen = owned
            .iter()
            .filter(|o| dist(o.pos, f.pos) <= ai.max_attack_m)
            .min_by(|a, b| front_dist(a.pos).total_cmp(&front_dist(b.pos)));
        if let Some(o) = screen {
            if dist(o.pos, f.pos) > cfg.arrive_m * 2. {
                order(lua, ctx, f.id, Order::Defend(o.id), now);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn obj(id: i64, x: f64, owner: Side, health: u8) -> Obj {
        Obj {
            id: ObjectiveId::from(id),
            name: "o".into(),
            pos: Vector2::new(x, 0.),
            owner,
            kind: ObjectiveKind::Fob,
            threatened: false,
            health,
            being_captured: false,
        }
    }

    #[test]
    fn target_is_the_nearest_reachable_enemy() {
        let objs = vec![
            obj(1, 0., Side::Blue, 100),
            obj(2, 20_000., Side::Red, 100),
            obj(3, 50_000., Side::Red, 100),
            obj(4, 200_000., Side::Red, 0),
        ];
        assert_eq!(pick_target(&objs, Side::Blue, None, 60_000.), Some(ObjectiveId::from(2)));
        // Out of reach is never picked, however weak.
        assert_eq!(pick_target(&objs, Side::Blue, None, 10_000.), None);
        // The axis wins when it is in reach...
        assert_eq!(
            pick_target(&objs, Side::Blue, Some(ObjectiveId::from(3)), 60_000.),
            Some(ObjectiveId::from(3))
        );
        // ...and is ignored when it isn't.
        assert_eq!(
            pick_target(&objs, Side::Blue, Some(ObjectiveId::from(4)), 60_000.),
            Some(ObjectiveId::from(2))
        );
    }

    #[test]
    fn a_wrecked_base_is_preferred() {
        let objs = vec![
            obj(1, 0., Side::Blue, 100),
            obj(2, 20_000., Side::Red, 100),
            obj(3, 25_000., Side::Red, 0),
        ];
        assert_eq!(pick_target(&objs, Side::Blue, None, 60_000.), Some(ObjectiveId::from(3)));
    }
}
