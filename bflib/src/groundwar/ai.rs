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
/// See `route_ok`.
const MAX_DETOUR: f64 = 1.6;
const MIN_DETOUR_M: f64 = 15_000.;
/// Targets tried per formation per think (each costs a road search).
const TRIES_PER_FORMATION: usize = 3;

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
    /// Can take a base: infantry, or IFVs / APCs carrying a squad.
    infantry: bool,
    home: ObjectiveId,
    /// Road left to drive, metres.
    to_go: f64,
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
            infantry: ctx.db.formation_can_assault(f),
            home: f.home,
            to_go: f.path_len_m(),
        })
        .collect()
}

/// Longest road route the AI accepts for a march of `straight` metres: the
/// road may wind (1.6x, or 15 km more on a short hop), but a road that goes
/// round half the map is no way to get there.
fn route_ok(straight: f64, road: f64) -> bool {
    road <= (straight * MAX_DETOUR).max(straight + MIN_DETOUR_M)
}

/// Give an order, refusing one whose road route (from `from` to `to`) is a
/// detour `route_ok` rejects. Withdrawals are always allowed: the formation
/// has to get home somehow.
fn order_checked(
    lua: MizLua,
    ctx: &mut Context,
    id: FormationId,
    order_: Order,
    from: Vector2,
    to: Option<Vector2>,
    now: DateTime<Utc>,
) -> bool {
    if let (Some(to), false) = (to, matches!(order_, Order::Withdraw(_))) {
        match ctx.db.route_km(lua, from, to) {
            Ok(km) if !route_ok(dist(from, to), km * 1000.) => {
                info!(
                    "ground war AI: not sending formation {id} on {order_:?}: {km:.0} km by road for \
                     {:.0} km as the crow flies",
                    dist(from, to) / 1000.
                );
                return false;
            }
            Ok(_) => (),
            Err(e) => {
                debug!("ground war AI: planning formation {id}'s route: {e:?}");
                return false;
            }
        }
    }
    order(lua, ctx, id, order_, now)
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

/// The objectives `side` could attack, each with its score (lower is
/// better): within `max_m` of ground the side holds, closer and weaker first,
/// weighted by what it is worth.
fn rank_targets(objs: &[Obj], side: Side, max_m: f64) -> Vec<(ObjectiveId, Vector2, f64)> {
    let own: Vec<Vector2> = objs.iter().filter(|o| o.owner == side).map(|o| o.pos).collect();
    let gap = |p: Vector2| own.iter().map(|q| dist(*q, p)).fold(f64::INFINITY, f64::min);
    let mut out: Vec<(ObjectiveId, Vector2, f64)> = objs
        .iter()
        .filter(|o| o.owner != side && value(&o.kind) > 0.)
        .filter_map(|o| {
            let g = gap(o.pos);
            (g <= max_m).then(|| {
                // A wrecked base is a cheaper fight.
                let weakness = 0.5 + o.health as f64 / 200.;
                (o.id, o.pos, g * weakness / value(&o.kind))
            })
        })
        .collect();
    out.sort_by(|a, b| a.2.total_cmp(&b.2));
    out
}

/// The targets a formation at `pos` should try, best first: only ones it can
/// reach (`reach` metres as the crow flies) that don't already have
/// `concentration` attackers; the tempo axis first, then by score plus
/// distance -- the nearest good target, not the best one on the map.
fn targets_for(
    pos: Vector2,
    ranked: &[(ObjectiveId, Vector2, f64)],
    axis: Option<ObjectiveId>,
    reach: f64,
    attackers: &fxhash::FxHashMap<ObjectiveId, usize>,
    concentration: usize,
) -> Vec<(ObjectiveId, Vector2)> {
    let mut out: Vec<(bool, f64, ObjectiveId, Vector2)> = ranked
        .iter()
        .filter(|(_, p, _)| dist(*p, pos) <= reach)
        .filter(|(id, _, _)| attackers.get(id).copied().unwrap_or(0) < concentration)
        .map(|(id, p, score)| (Some(*id) != axis, score + dist(*p, pos) / 2., *id, *p))
        .collect();
    out.sort_by(|a, b| (a.0, a.1).partial_cmp(&(b.0, b.1)).unwrap_or(std::cmp::Ordering::Equal));
    out.into_iter().map(|(_, _, id, p)| (id, p)).collect()
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

    // 0. Recall marches the old AI (or a road gone wrong) sent half way
    //    across the map: anything still more than twice the attack reach
    //    from where it is going holds at the nearest friendly base instead.
    for f in forms(ctx, side, now) {
        if f.ai && f.posture == Posture::Moving && f.to_go > ai.max_attack_m * 2. {
            if let Some(dest) = nearest_owned(f.pos) {
                info!(
                    "ground war AI: formation {} is {:.0} km from its objective, recalling it",
                    f.id,
                    f.to_go / 1000.
                );
                let to = owned.iter().find(|o| o.id == dest).map(|o| o.pos);
                if !order_checked(lua, ctx, f.id, Order::Defend(dest), f.pos, to, now) {
                    order(lua, ctx, f.id, Order::Hold, now);
                }
            }
        }
    }

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
            if order_checked(lua, ctx, f.id, Order::Defend(o.id), f.pos, Some(o.pos), now) {
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

    // 4. Attack. Each idle formation goes for the best target it can
    //    actually reach -- within `max_attack_m` of where it is, by a road
    //    that isn't a detour round half the map -- up to `concentration`
    //    formations per target, `reserve` kept back.
    if offensive {
        let ranked = rank_targets(&objs, side, ai.max_attack_m);
        let fs = forms(ctx, side, now);
        let mut attackers: fxhash::FxHashMap<ObjectiveId, usize> = fxhash::FxHashMap::default();
        for f in &fs {
            if let Order::Attack(t) = f.order {
                *attackers.entry(t).or_default() += 1;
            }
        }
        let mut idle: Vec<&Form> = fs
            .iter()
            .filter(|f| f.ai && f.idle() && f.pct >= ATTACK_MIN_PCT)
            .filter(|f| !matches!(f.order, Order::Attack(_)))
            .collect();
        // Those that can take a base first, then those nearest the enemy.
        idle.sort_by(|a, b| {
            (!a.infantry, front_dist(a.pos))
                .partial_cmp(&(!b.infantry, front_dist(b.pos)))
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        let spare = idle.len().saturating_sub(ai.reserve as usize);
        let mut sent: fxhash::FxHashMap<ObjectiveId, usize> = fxhash::FxHashMap::default();
        for f in idle.into_iter().take(spare) {
            let opts = targets_for(
                f.pos,
                &ranked,
                axis,
                ai.max_attack_m,
                &attackers,
                ai.concentration as usize,
            );
            for (target, tpos) in opts.into_iter().take(TRIES_PER_FORMATION) {
                if order_checked(lua, ctx, f.id, Order::Attack(target), f.pos, Some(tpos), now) {
                    *attackers.entry(target).or_default() += 1;
                    *sent.entry(target).or_default() += 1;
                    break;
                }
            }
        }
        if cfg.announce {
            for (target, n) in sent {
                let tname = objs.iter().find(|o| o.id == target).map(|o| o.name.clone()).unwrap_or_default();
                ctx.db.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side,
                    format_compact!("GROUND COMMAND: {n} formation(s) advancing on {tname}."),
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
                order_checked(lua, ctx, f.id, Order::Defend(o.id), f.pos, Some(o.pos), now);
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

    fn none() -> fxhash::FxHashMap<ObjectiveId, usize> {
        fxhash::FxHashMap::default()
    }

    #[test]
    fn only_targets_near_our_ground_are_ranked() {
        let objs = vec![
            obj(1, 0., Side::Blue, 100),
            obj(2, 20_000., Side::Red, 100),
            obj(3, 50_000., Side::Red, 100),
            obj(4, 200_000., Side::Red, 0),
        ];
        let r = rank_targets(&objs, Side::Blue, 60_000.);
        let ids: Vec<_> = r.iter().map(|(id, ..)| *id).collect();
        assert_eq!(ids, vec![ObjectiveId::from(2), ObjectiveId::from(3)]);
        assert!(rank_targets(&objs, Side::Blue, 10_000.).is_empty());
    }

    #[test]
    fn a_wrecked_base_ranks_first() {
        let objs = vec![
            obj(1, 0., Side::Blue, 100),
            obj(2, 20_000., Side::Red, 100),
            obj(3, 25_000., Side::Red, 0),
        ];
        assert_eq!(rank_targets(&objs, Side::Blue, 60_000.)[0].0, ObjectiveId::from(3));
    }

    #[test]
    fn a_formation_only_goes_for_what_it_can_reach() {
        // Our ground runs along x; a formation sits far east of it all.
        let objs = vec![
            obj(1, 0., Side::Blue, 100),
            obj(2, 20_000., Side::Red, 100),
            obj(5, 300_000., Side::Blue, 100),
            obj(6, 330_000., Side::Red, 100),
        ];
        let ranked = rank_targets(&objs, Side::Blue, 60_000.);
        let far_east = Vector2::new(310_000., 0.);
        let t = targets_for(far_east, &ranked, None, 60_000., &none(), 2);
        assert_eq!(t.iter().map(|(id, _)| *id).collect::<Vec<_>>(), vec![ObjectiveId::from(6)]);
        // The axis comes first when it is in reach, and is skipped when not.
        let west = Vector2::new(5_000., 0.);
        let t = targets_for(west, &ranked, Some(ObjectiveId::from(6)), 60_000., &none(), 2);
        assert_eq!(t[0].0, ObjectiveId::from(2));
        // A target with its full complement of attackers is passed over.
        let mut full = none();
        full.insert(ObjectiveId::from(6), 2);
        assert!(targets_for(far_east, &ranked, None, 60_000., &full, 2).is_empty());
    }

    #[test]
    fn detours_are_refused() {
        // Tskhinvali to Sochi, Oct 1: 490 km of road for ~290 km.
        assert!(!route_ok(290_000., 490_000.));
        assert!(route_ok(40_000., 60_000.));
        // A short hop may wind a lot.
        assert!(route_ok(5_000., 19_000.));
        assert!(!route_ok(5_000., 25_000.));
    }
}
