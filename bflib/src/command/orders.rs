/*
Copyright 2024 Eric Stokes.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Affero Public License for more
details.
*/

//! The command map's order catalogue: everything a side can do, at the
//! place the commander picks, paid from the treasury.
//!
//! The command map used to reach only the HQ's own candidates -- wherever
//! the planner happened to want a strike, a tanker or a drone -- and no
//! deployment, ambush, paratroop drop or transfer at all. Every entry here
//! runs the same engine code the F10 menu does, so what is ordered happens
//! in DCS: aircraft take off and fly there, batteries fire, deployments
//! drive out of a base by road (and can be caught on the way), ambush
//! forces drive to the convoy, ships sail and shoot. Nothing is decided on
//! the map instead of in DCS.

use super::{afford, from_ll, pay, to_ll};
use crate::{
    db::{
        actions::{ActionArgs, ActionCmd, WithFromTo, WithJtac, WithObj, WithPos},
        group::DeployKind,
        markup::objective_visible_to,
    },
    spawnctx::SpawnCtx,
    Context,
};
use bfprotocols::{
    cfg::{Action, ActionKind, DeployableKind, UnitTag},
    command::{Asset, AssetKind, AssetUnit, LatLon, OrderOption, OrderTarget, Verb},
    db::{group::GroupId, objective::ObjectiveId},
    hq::OpKind,
    perf::PerfInner,
};
use chrono::{DateTime, Duration, Utc};
use compact_str::{format_compact, CompactString};
use dcso3::{
    coalition::{Coalition, Side},
    controller::{AiOption, GroundRoe, MissionPoint, NavalOption, PointType, Task},
    coord::Coord,
    country::Country,
    env::miz,
    group::{Group, GroupCategory},
    land::{Land, SurfaceType},
    net::Ucid,
    LuaEnv, LuaVec2, MizLua, Vector2,
};
use enumflags2::BitFlags;
use log::{info, warn};
use mlua::{FromLua, Value};
use smallvec::SmallVec;

/// Aircraft launched for an order must have a friendly airfield this close
/// to where they are sent.
const AIR_REACH_M: f64 = 250_000.;
/// A bomber order takes the JTAC target nearest the point within this.
const JTAC_NEAR_M: f64 = 15_000.;
/// Road deployments drive at this speed, m/s.
const DEPLOY_MPS: f64 = 10.;
const KTS: f64 = 0.514_444;

fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

fn err(e: impl std::fmt::Display) -> CompactString {
    format_compact!("{e}")
}

/// (category, target, what it does) for an action kind the catalogue
/// offers; None for the ones driven from an asset instead (waypoints,
/// Move, RTB, sailing, tasks).
fn meta(kind: &ActionKind) -> Option<(&'static str, OrderTarget, &'static str)> {
    use OrderTarget::*;
    Some(match kind {
        ActionKind::Tanker(_) => ("Air", Point, "a tanker flies out and holds over the point"),
        ActionKind::Awacs(_) => ("Air", Point, "an AWACS flies out and orbits over the point"),
        ActionKind::Fighters(_) => ("Air", Point, "fighters take off and patrol over the point"),
        ActionKind::Attackers(_) => ("Air", Point, "an attack flight takes off and works the area"),
        ActionKind::Sead(_) => ("Air", Point, "a SEAD flight takes off and hunts air defences there"),
        ActionKind::Bomber(_) => ("Air", Point, "heavy bombers on the JTAC target nearest the point"),
        ActionKind::Drone(_) => ("Intel", Point, "a JTAC drone flies out and lases targets there"),
        ActionKind::Recon(_) => ("Intel", Point, "a recon flight photographs the area"),
        ActionKind::CruiseMissileSpawn(_) => ("Fires", Point, "a bomber launches cruise missiles at the point"),
        ActionKind::Nuke(_) => ("Fires", Point, "a nuclear strike on the point"),
        ActionKind::Artillery(_) => ("Fires", Point, "every battery of ours in range fires on the point"),
        ActionKind::Paratrooper(_) => ("Ground", Land, "a transport drops paratroopers at the point"),
        ActionKind::Deployable(_) => ("Ground", Land, "units delivered to the point"),
        ActionKind::Reinforce(_) => ("Ground", OwnBase, "a transporter convoy rebuilds the base's lost units"),
        ActionKind::LogisticsRepair(_) => ("Logistics", OwnBase, "a transport flies repair supplies to the base"),
        ActionKind::LogisticsTransfer(_) => ("Logistics", Transfer, "a transport flies supplies from one base to another"),
        ActionKind::CarrierRepair => ("Naval", OwnBase, "repairs a damaged carrier group"),
        ActionKind::CarrierRespawn => ("Naval", OwnBase, "a sunk carrier group sails again from its naval base"),
        ActionKind::NavalCruiseMissileStrike(_) => ("Naval", EnemyBase, "our ships launch cruise missiles at the base"),
        _ => return None,
    })
}

fn scaled(ctx: &Context, raw: i64) -> i64 {
    ((raw as f64) * crate::hq::cost_scale(ctx)).round() as i64
}

fn why_not_cost(ctx: &Context, side: Side, cost: i64) -> String {
    if ctx.db.persisted.treasury(side) < cost {
        format!("the treasury has {} of {cost}", ctx.db.persisted.treasury(side))
    } else {
        String::new()
    }
}

/// Everything `side` can order now, for the command map.
pub(super) fn catalog(ctx: &Context, side: Side) -> Vec<OrderOption> {
    let mut out = vec![];
    if let Some(actions) = ctx.db.ephemeral.cfg.actions.get(&side) {
        for (name, action) in actions {
            let Some((category, target, detail)) = meta(&action.kind) else { continue };
            let cost = scaled(ctx, action.cost as i64);
            out.push(OrderOption {
                key: format!("action:{name}"),
                label: name.to_string(),
                category: category.into(),
                target,
                cost,
                detail: detail.into(),
                why_not: why_not_cost(ctx, side, cost),
            });
        }
    }
    if let Some(deps) = ctx.db.ephemeral.cfg.deployables.get(&side) {
        for d in deps {
            if !d.kind.is_group() {
                continue;
            }
            let name = d.path.join(" / ");
            let cost = scaled(ctx, (d.cost as i64).max(10));
            let out_now = deployed_count(ctx, side, &d.path);
            let mut why = why_not_cost(ctx, side, cost);
            if d.limit > 0 && out_now >= d.limit as usize && why.is_empty() {
                why = format!("{out_now} of {} already deployed", d.limit);
            }
            out.push(OrderOption {
                key: format!("deploy:{name}"),
                label: d.path.last().map(|s| s.to_string()).unwrap_or_else(|| name.clone()),
                category: d.path.first().map(|s| s.to_string()).unwrap_or_else(|| "Ground".into()),
                target: OrderTarget::Land,
                cost,
                detail: format!(
                    "drives out from our nearest base, by road, to the point (within {:.0} km of a base)",
                    ctx.db.ephemeral.cfg.command.deploy_range_m / 1000.
                ),
                why_not: why,
            });
        }
    }
    let events = ctx.db.ephemeral.cfg.campaign_events.is_some();
    if events {
        if let Some(cost) = crate::hq::cost_of(ctx, OpKind::Ambush) {
            out.push(OrderOption {
                key: "op:ambush".into(),
                label: "Ambush convoy".into(),
                category: "Ground".into(),
                target: OrderTarget::Point,
                cost,
                detail: "a force drives out from our nearest base to cut off the enemy convoy nearest the point".into(),
                why_not: why_not_cost(ctx, side, cost),
            });
        }
        if let Some(cost) = crate::hq::cost_of(ctx, OpKind::MissileStrike) {
            let have = missile_groups(ctx, side).len();
            let mut why = why_not_cost(ctx, side, cost);
            if have == 0 && why.is_empty() {
                why = "we have no missile launchers deployed".into();
            }
            out.push(OrderOption {
                key: "op:missile".into(),
                label: "Missile strike".into(),
                category: "Fires".into(),
                target: OrderTarget::Point,
                cost,
                detail: "our deployed missile launchers in range fire on the point".into(),
                why_not: why,
            });
        }
    }
    let h = &ctx.db.ephemeral.cfg.command.hunters;
    let types = match side {
        Side::Red => &h.red,
        _ => &h.blue,
    };
    if !types.is_empty() && has_harbour(ctx, side) {
        let cost = scaled(ctx, h.cost);
        let at_sea = ctx.hunters.list.iter().filter(|x| x.side == side).count();
        let mut why = why_not_cost(ctx, side, cost);
        if at_sea >= h.max_per_side as usize && why.is_empty() {
            why = format!("{at_sea} hunter groups already at sea");
        }
        out.push(OrderOption {
            key: "op:hunt".into(),
            label: if side == Side::Red { "Submarine hunt".into() } else { "Surface action group".into() },
            category: "Naval".into(),
            target: OrderTarget::Sea,
            cost,
            detail: format!(
                "{} sail out from our nearest naval base or carrier group and attack the enemy ships they find there",
                types.join(", ")
            ),
            why_not: why,
        });
    }
    out
}

fn deployed_count(ctx: &Context, side: Side, path: &[dcso3::String]) -> usize {
    ctx.db
        .deployed()
        .filter(|g| g.side == side)
        .filter(|g| matches!(&g.origin, DeployKind::Deployed { spec, .. } if spec.path.as_slice() == path))
        .count()
}

fn missile_groups(ctx: &Context, side: Side) -> SmallVec<[(GroupId, Vector2); 4]> {
    ctx.db
        .deployed()
        .chain(ctx.db.actions())
        .filter(|g| g.side == side && g.tags.contains(UnitTag::ALCM))
        .filter_map(|g| ctx.db.group_center(&g.id).ok().map(|p| (g.id, p)))
        .collect()
}

fn has_harbour(ctx: &Context, side: Side) -> bool {
    ctx.db
        .objectives()
        .any(|(_, o)| o.owner() == side && (o.kind().is_naval_base() || o.kind().is_carrier_group()))
}

fn wet(lua: MizLua, p: Vector2) -> Result<bool, CompactString> {
    let land = Land::singleton(lua).map_err(err)?;
    Ok(matches!(
        land.get_surface_type(LuaVec2(p)),
        Ok(SurfaceType::Water | SurfaceType::ShallowWater)
    ))
}

fn in_enemy_base(ctx: &Context, side: Side, p: Vector2) -> Option<String> {
    ctx.db
        .objectives()
        .find(|(_, o)| o.owner() != side && o.owner() != Side::Neutral && o.contains(p))
        .map(|(_, o)| o.name.to_string())
}

fn near_airfield(ctx: &Context, side: Side, p: Vector2) -> bool {
    ctx.db
        .objectives()
        .any(|(_, o)| o.owner() == side && o.is_airbase() && dist(o.pos(), p) <= AIR_REACH_M)
}

fn objective_name(ctx: &Context, oid: &ObjectiveId) -> String {
    ctx.db.persisted.objectives.get(oid).map(|o| o.name.to_string()).unwrap_or_default()
}

fn own_base(ctx: &Context, side: Side, id: Option<i64>) -> Result<ObjectiveId, CompactString> {
    let id = id.ok_or_else(|| CompactString::from("pick one of our bases"))?;
    let oid = ObjectiveId::from(id);
    match ctx.db.persisted.objectives.get(&oid) {
        Some(o) if o.owner() == side => Ok(oid),
        Some(o) => Err(format_compact!("{} isn't ours", o.name)),
        None => Err("no such base".into()),
    }
}

fn enemy_base(ctx: &Context, side: Side, id: Option<i64>) -> Result<ObjectiveId, CompactString> {
    let id = id.ok_or_else(|| CompactString::from("pick an enemy base"))?;
    let oid = ObjectiveId::from(id);
    match ctx.db.persisted.objectives.get(&oid) {
        // Fog of war: a base the enemy keeps off our map can't be aimed at.
        Some(o) if o.owner() != side && o.owner() != Side::Neutral && objective_visible_to(o, side) => Ok(oid),
        Some(o) if o.owner() == side => Err(format_compact!("{} is ours", o.name)),
        _ => Err("no enemy base we know of there".into()),
    }
}

/// Run a configured action through the engine as the side, with its real
/// name (so its limit counts) and no player charged: the caller pays.
fn start(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    side: Side,
    name: &str,
    action: Action,
    args: ActionArgs,
) -> Result<(), CompactString> {
    let spctx = SpawnCtx::new(lua).map_err(err)?;
    let cmd = ActionCmd { name: name.into(), action, args };
    ctx.db.start_action(lua, perf, &spctx, &ctx.idx, &ctx.jtac, side, None, cmd).map_err(err)
}

/// Carry out a catalogue order. Returns what to announce; the caller has
/// checked the commander and the rate limit.
#[allow(clippy::too_many_arguments)]
pub(super) fn order(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    side: Side,
    key: &str,
    at: Option<LatLon>,
    objective: Option<i64>,
    to_objective: Option<i64>,
    now: DateTime<Utc>,
) -> Result<CompactString, CompactString> {
    let point = match at {
        Some(ll) => Some(from_ll(lua, ll)?),
        None => None,
    };
    let need_point = || point.ok_or_else(|| CompactString::from("pick a point on the map"));
    if let Some(name) = key.strip_prefix("action:") {
        let action = ctx
            .db
            .ephemeral
            .cfg
            .actions
            .get(&side)
            .and_then(|a| a.get(name))
            .cloned()
            .ok_or_else(|| format_compact!("{side:?} has no action called {name}"))?;
        let Some((_, target, _)) = meta(&action.kind) else {
            return Err("that is ordered from the asset on the map".into());
        };
        let cost = scaled(ctx, action.cost as i64);
        afford(ctx, side, cost)?;
        let air = matches!(
            action.kind,
            ActionKind::Tanker(_)
                | ActionKind::Awacs(_)
                | ActionKind::Fighters(_)
                | ActionKind::Attackers(_)
                | ActionKind::Sead(_)
                | ActionKind::Drone(_)
                | ActionKind::Recon(_)
                | ActionKind::CruiseMissileSpawn(_)
                | ActionKind::Paratrooper(_)
        ) || matches!(&action.kind, ActionKind::Deployable(d) if d.plane.is_some());
        let (args, what) = match (&action.kind, target) {
            (_, OrderTarget::Point | OrderTarget::Land | OrderTarget::Sea) => {
                let pos = need_point()?;
                if target == OrderTarget::Land && wet(lua, pos)? {
                    return Err("that point is in the water".into());
                }
                if air && !near_airfield(ctx, side, pos) {
                    return Err(format_compact!(
                        "none of our airfields is within {:.0} km of that point",
                        AIR_REACH_M / 1000.
                    ));
                }
                if matches!(action.kind, ActionKind::Deployable(_)) {
                    if let Some(b) = in_enemy_base(ctx, side, pos) {
                        return Err(format_compact!("that is inside {b}"));
                    }
                }
                let args = match &action.kind {
                    ActionKind::Tanker(c) => ActionArgs::Tanker(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Awacs(c) => ActionArgs::Awacs(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Fighters(c) => ActionArgs::Fighters(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Attackers(c) => ActionArgs::Attackers(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Sead(c) => ActionArgs::Sead(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Drone(c) => ActionArgs::Drone(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Recon(c) => ActionArgs::Recon(WithPos { cfg: c.clone(), pos }),
                    ActionKind::CruiseMissileSpawn(c) => ActionArgs::CruiseMissileSpawn(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Nuke(c) => ActionArgs::Nuke(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Artillery(c) => ActionArgs::Artillery(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Paratrooper(c) => ActionArgs::Paratrooper(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Deployable(c) => ActionArgs::Deployable(WithPos { cfg: c.clone(), pos }),
                    ActionKind::Bomber(c) => {
                        let jtac = ctx
                            .jtac
                            .jtacs()
                            .filter(|j| j.side() == side && j.target().is_some())
                            .map(|j| (j.gid(), dist(j.location().pos, pos)))
                            .filter(|(_, d)| *d <= JTAC_NEAR_M)
                            .min_by(|a, b| a.1.total_cmp(&b.1))
                            .map(|(g, _)| g)
                            .ok_or_else(|| {
                                format_compact!(
                                    "bombers need a JTAC of ours lasing a target within {:.0} km of the point",
                                    JTAC_NEAR_M / 1000.
                                )
                            })?;
                        ActionArgs::Bomber(WithJtac { cfg: c.clone(), jtac })
                    }
                    _ => return Err("that can't be aimed at a point".into()),
                };
                (args, "at the point".to_string())
            }
            (kind, OrderTarget::OwnBase) => {
                let oid = own_base(ctx, side, objective)?;
                let args = match kind {
                    ActionKind::Reinforce(c) => ActionArgs::Reinforce(WithObj { cfg: c.clone(), oid }),
                    ActionKind::LogisticsRepair(c) => ActionArgs::LogisticsRepair(WithObj { cfg: c.clone(), oid }),
                    ActionKind::CarrierRepair => ActionArgs::CarrierRepair(WithObj { cfg: (), oid }),
                    ActionKind::CarrierRespawn => ActionArgs::CarrierRespawn(WithObj { cfg: (), oid }),
                    _ => return Err("that can't be aimed at a base".into()),
                };
                (args, format!("at {}", objective_name(ctx, &oid)))
            }
            (ActionKind::NavalCruiseMissileStrike(c), OrderTarget::EnemyBase) => {
                let oid = enemy_base(ctx, side, objective)?;
                (
                    ActionArgs::NavalCruiseMissileStrike(WithObj { cfg: c.clone(), oid }),
                    format!("on {}", objective_name(ctx, &oid)),
                )
            }
            (ActionKind::LogisticsTransfer(c), OrderTarget::Transfer) => {
                let from = own_base(ctx, side, objective)?;
                let to = own_base(ctx, side, to_objective)?;
                if from == to {
                    return Err("pick two different bases".into());
                }
                (
                    ActionArgs::LogisticsTransfer(WithFromTo { cfg: c.clone(), from, to }),
                    format!("from {} to {}", objective_name(ctx, &from), objective_name(ctx, &to)),
                )
            }
            _ => return Err("that order needs a different target".into()),
        };
        start(lua, ctx, perf, side, name, action, args)?;
        pay(ctx, side, cost);
        return Ok(format_compact!("{name} {what} ({cost} from the treasury)"));
    }
    if let Some(name) = key.strip_prefix("deploy:") {
        return deploy(lua, ctx, side, name, need_point()?);
    }
    match key {
        "op:ambush" => {
            let pos = need_point()?;
            let cost = crate::hq::cost_of(ctx, OpKind::Ambush).ok_or("this server has no theatre HQ to pay for that")?;
            afford(ctx, side, cost)?;
            let ecfg = ctx
                .db
                .ephemeral
                .cfg
                .campaign_events
                .clone()
                .ok_or_else(|| CompactString::from("campaign events are off"))?;
            let cands = ctx.event_scheduler.build_candidates(&ctx.db);
            let (mut msgs, mut effects) = (vec![], vec![]);
            let ok = ctx
                .event_scheduler
                .spawn_convoy_ambush(&ctx.db, &ecfg, now, side, &cands, &mut msgs, &mut effects, Some(pos));
            ctx.event_scheduler.pending_effects.extend(effects);
            if !ok {
                return Err("no enemy convoy we can get ahead of within 20 km of that point".into());
            }
            pay(ctx, side, cost);
            Ok(format_compact!("ambush force on its way to the convoy ({cost} from the treasury)"))
        }
        "op:missile" => {
            let pos = need_point()?;
            let cost = crate::hq::cost_of(ctx, OpKind::MissileStrike)
                .ok_or("this server has no theatre HQ to pay for that")?;
            afford(ctx, side, cost)?;
            let ecfg = ctx
                .db
                .ephemeral
                .cfg
                .campaign_events
                .clone()
                .ok_or_else(|| CompactString::from("campaign events are off"))?;
            let range = ctx.db.ephemeral.cfg.alcm_mission_range as f64;
            let shooters: SmallVec<[GroupId; 4]> = missile_groups(ctx, side)
                .into_iter()
                .filter(|(_, p)| dist(*p, pos) <= range)
                .map(|(g, _)| g)
                .collect();
            if shooters.is_empty() {
                return Err(format_compact!("no launcher of ours within {:.0} km of the point", range / 1000.));
            }
            let n = shooters.len();
            let mut msgs = vec![];
            ctx.event_scheduler.spawn_missile_strike_event(
                &ecfg,
                now,
                side,
                shooters,
                pos,
                "the commander's target".into(),
                &mut msgs,
            );
            for m in msgs {
                ctx.db.ephemeral.msgs().panel_to_all(15, false, m);
            }
            pay(ctx, side, cost);
            Ok(format_compact!("{n} launcher(s) firing ({cost} from the treasury)"))
        }
        "op:hunt" => hunt(lua, ctx, side, need_point()?, now),
        _ => Err(format_compact!("no such order: {key}")),
    }
}

/// A deployment by road: the units are put together at our nearest base in
/// range and drive to the point, where they set up. On the way they are a
/// convoy anyone can find and kill.
fn deploy(lua: MizLua, ctx: &mut Context, side: Side, name: &str, pos: Vector2) -> Result<CompactString, CompactString> {
    let spec = ctx
        .db
        .ephemeral
        .cfg
        .deployables
        .get(&side)
        .and_then(|d| d.iter().find(|d| d.path.join(" / ") == name))
        .cloned()
        .ok_or_else(|| format_compact!("{side:?} has no deployable called {name}"))?;
    let DeployableKind::Group { template } = &spec.kind else {
        return Err("bases are built by their crates, not ordered".into());
    };
    let template = template.clone();
    let cost = scaled(ctx, (spec.cost as i64).max(10));
    afford(ctx, side, cost)?;
    if spec.limit > 0 && deployed_count(ctx, side, &spec.path) >= spec.limit as usize {
        return Err(format_compact!("all {} allowed are already deployed", spec.limit));
    }
    if wet(lua, pos)? {
        return Err("that point is in the water".into());
    }
    if let Some(b) = in_enemy_base(ctx, side, pos) {
        return Err(format_compact!("that is inside {b}"));
    }
    let reach = ctx.db.ephemeral.cfg.command.deploy_range_m;
    let mut bases: Vec<(ObjectiveId, Vector2, f64)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| {
            o.owner() == side
                && !o.threatened()
                && !o.kind().is_carrier_group()
                && !o.kind().is_naval_base()
                && !o.kind().is_special_sam_site()
        })
        .map(|(id, o)| (*id, o.pos(), dist(o.pos(), pos)))
        .filter(|(_, _, d)| *d <= reach)
        .collect();
    if bases.is_empty() {
        return Err(format_compact!("none of our bases is within {:.0} km of that point", reach / 1000.));
    }
    bases.sort_by(|a, b| a.2.total_cmp(&b.2));
    let (from, from_pos, _) = bases[0];
    let plan = ctx
        .db
        .plan_drive(lua, from_pos, pos, true)
        .ok_or_else(|| CompactString::from("no way to drive there"))?;
    let spctx = SpawnCtx::new(lua).map_err(err)?;
    let origin = DeployKind::Deployed {
        player: Ucid::default(),
        moved_by: None,
        spec: spec.clone(),
        cost_fraction: 1.0,
        origin: None,
        jtac: None,
    };
    ctx.db
        .queue_drive(&spctx, &ctx.idx, side, origin, &template, &plan, DEPLOY_MPS, BitFlags::empty())
        .map_err(err)?;
    pay(ctx, side, cost);
    let from_name = objective_name(ctx, &from);
    info!("command: {side:?} deploying {name} from {from_name}, {:.1} km by {}", plan.route_m / 1000., if plan.by_road { "road" } else { "cross country" });
    Ok(format_compact!(
        "{} leaving {from_name}, {:.0} km {} ({cost} from the treasury)",
        spec.path.last().map(|s| s.as_str()).unwrap_or(name),
        plan.route_m / 1000.,
        if plan.by_road { "by road" } else { "across country" }
    ))
}

/// A naval hunter group at sea: real DCS ships, outside the campaign db.
#[derive(Debug, Clone)]
pub(crate) struct Hunter {
    id: u32,
    side: Side,
    name: String,
    home: Vector2,
    target: Vector2,
    until: DateTime<Utc>,
    going_home: bool,
    speed: f64,
    label: String,
}

#[derive(Debug, Default)]
pub(crate) struct Hunters {
    pub(crate) list: Vec<Hunter>,
    seq: u32,
    last_tick: Option<DateTime<Utc>>,
}

fn sea_point<'lua>(p: Vector2, speed: f64) -> MissionPoint<'lua> {
    MissionPoint {
        typ: PointType::TurningPoint,
        airdrome_id: None,
        time_re_fu_ar: None,
        helipad: None,
        link_unit: None,
        action: None,
        pos: LuaVec2(p),
        alt: 0.,
        alt_typ: None,
        speed,
        speed_locked: None,
        eta: None,
        eta_locked: None,
        name: None,
        task: Box::new(Task::ComboTask(vec![])),
    }
}

fn sail(lua: MizLua, name: &str, from: Vector2, to: Vector2, speed: f64) -> anyhow::Result<()> {
    let group = Group::get_by_name(lua, name)?;
    let con = group.get_controller()?;
    con.set_option(AiOption::Naval(NavalOption::Roe(GroundRoe::WeaponFree)))?;
    con.set_task(Task::Mission { airborne: Some(false), route: vec![sea_point(from, speed), sea_point(to, speed)] })?;
    Ok(())
}

fn spawn_hunter(lua: MizLua, side: Side, name: &str, types: &[String], at: Vector2, to: Vector2, speed: f64) -> anyhow::Result<()> {
    let l = lua.inner();
    let pts = l.create_table()?;
    pts.raw_set(1, sea_point(at, speed))?;
    pts.raw_set(2, sea_point(to, speed))?;
    let route = l.create_table()?;
    route.raw_set("points", pts)?;
    let units = l.create_table()?;
    let dir = (to - at).normalize();
    let heading = dir.y.atan2(dir.x);
    let across = Vector2::new(-dir.y, dir.x);
    for (i, typ) in types.iter().enumerate() {
        // Line abreast, 600 m apart.
        let p = at + across * ((i as f64 - (types.len() as f64 - 1.) / 2.) * 600.);
        let unit = l.create_table()?;
        unit.raw_set("name", format_compact!("{name}-{}", i + 1).as_str())?;
        unit.raw_set("type", typ.as_str())?;
        unit.raw_set("x", p.x)?;
        unit.raw_set("y", p.y)?;
        unit.raw_set("heading", heading)?;
        unit.raw_set("skill", "Excellent")?;
        units.raw_set(i + 1, unit)?;
    }
    let group = l.create_table()?;
    group.raw_set("name", name)?;
    group.raw_set("route", route)?;
    group.raw_set("units", units)?;
    let group = miz::Group::from_lua(Value::Table(group), l)?;
    let country = match side {
        Side::Blue => Country::CJTF_BLUE,
        _ => Country::CJTF_RED,
    };
    Coalition::singleton(lua)?.add_group(country, GroupCategory::Ship, group)?;
    Ok(())
}

fn hunt(lua: MizLua, ctx: &mut Context, side: Side, to: Vector2, now: DateTime<Utc>) -> Result<CompactString, CompactString> {
    let h = ctx.db.ephemeral.cfg.command.hunters.clone();
    let types = match side {
        Side::Red => h.red.clone(),
        _ => h.blue.clone(),
    };
    if types.is_empty() {
        return Err("this side has no hunter ships configured".into());
    }
    let at_sea = ctx.hunters.list.iter().filter(|x| x.side == side).count();
    if at_sea >= h.max_per_side as usize {
        return Err(format_compact!("{at_sea} hunter groups are already at sea"));
    }
    let cost = scaled(ctx, h.cost);
    afford(ctx, side, cost)?;
    let land = Land::singleton(lua).map_err(err)?;
    if !matches!(land.get_surface_type(LuaVec2(to)), Ok(SurfaceType::Water)) {
        return Err("pick a point in open water".into());
    }
    // Sail from the nearest naval base or carrier group in range.
    let mut homes: Vec<(String, Vector2, f64)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == side && (o.kind().is_naval_base() || o.kind().is_carrier_group()))
        .map(|(_, o)| (o.name.to_string(), o.pos(), dist(o.pos(), to)))
        .filter(|(_, _, d)| *d <= h.range_m)
        .collect();
    homes.sort_by(|a, b| a.2.total_cmp(&b.2));
    let Some((home_name, home, _)) = homes.into_iter().next() else {
        return Err(format_compact!("no naval base or carrier group of ours within {:.0} km", h.range_m / 1000.));
    };
    let start = crate::modern_war::water_near(&land, home, 15_000.)
        .ok_or_else(|| format_compact!("no open water near {home_name} to sail from"))?;
    ctx.hunters.seq += 1;
    let id = ctx.hunters.seq;
    let name = format!("HUNTER {side:?} {id}");
    let speed = h.speed_kts.max(5.) * KTS;
    spawn_hunter(lua, side, &name, &types, start, to, speed).map_err(err)?;
    let label = if side == Side::Red { format!("Submarine group {id}") } else { format!("Surface action group {id}") };
    ctx.hunters.list.push(Hunter {
        id,
        side,
        name: name.clone(),
        home: start,
        target: to,
        until: now + Duration::seconds(h.lifetime_secs as i64),
        going_home: false,
        speed,
        label: label.clone(),
    });
    pay(ctx, side, cost);
    info!("command: {side:?} {label} ({}) sailing from {home_name}", types.join(", "));
    Ok(format_compact!(
        "{label} sailing from {home_name}, {:.0} km at {:.0} kts ({cost} from the treasury)",
        dist(start, to) / 1000.,
        h.speed_kts
    ))
}

fn lead_pos(lua: MizLua, name: &str) -> Option<Vector2> {
    let g = Group::get_by_name(lua, name).ok()?;
    let u = g.get_units().ok()?.into_iter().filter_map(|u| u.ok()).next()?;
    let p = u.get_point().ok()?;
    Some(Vector2::new(p.x, p.z))
}

/// Keep the hunter groups honest: forget the sunk, bring the old home.
pub(crate) fn tick_hunters(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    if ctx.hunters.last_tick.map_or(false, |t| now - t < Duration::seconds(20)) {
        return;
    }
    ctx.hunters.last_tick = Some(now);
    let mut gone: SmallVec<[usize; 4]> = SmallVec::new();
    for (i, h) in ctx.hunters.list.iter_mut().enumerate() {
        let Some(pos) = lead_pos(lua, &h.name) else {
            info!("command: {:?} {} lost", h.side, h.label);
            ctx.groundwar.rt.event(h.side, "destroyed", format_compact!("{} has been lost", h.label), None, None, now);
            ctx.db.ephemeral.msgs().panel_to_side(15, false, h.side, format_compact!("COMMAND: {} has been lost.", h.label));
            gone.push(i);
            continue;
        };
        if !h.going_home && now >= h.until {
            h.going_home = true;
            if let Err(e) = sail(lua, &h.name, pos, h.home, h.speed) {
                warn!("command: {} heading home: {e:?}", h.label);
            }
        }
        if h.going_home && dist(pos, h.home) < 3_000. {
            if let Ok(g) = Group::get_by_name(lua, &h.name) {
                let _ = g.destroy();
            }
            gone.push(i);
        }
    }
    for i in gone.into_iter().rev() {
        ctx.hunters.list.remove(i);
    }
}

/// Send a hunter group somewhere else (the command map's Sail on one).
pub(super) fn retask_hunter(lua: MizLua, ctx: &mut Context, side: Side, id: u32, to: Vector2) -> Result<CompactString, CompactString> {
    let h = ctx
        .hunters
        .list
        .iter_mut()
        .find(|h| h.id == id && h.side == side)
        .ok_or_else(|| CompactString::from("no such hunter group"))?;
    let pos = lead_pos(lua, &h.name).ok_or_else(|| CompactString::from("that group has been lost"))?;
    sail(lua, &h.name, pos, to, h.speed).map_err(err)?;
    h.target = to;
    h.going_home = false;
    Ok(format_compact!("{} heading for the new point", h.label))
}

/// Hunter groups as command-map assets. Ids are negative: they aren't
/// campaign groups.
pub(super) fn hunter_assets(lua: MizLua, ctx: &Context, side: Side, coord: Option<&Coord>) -> Vec<Asset> {
    ctx.hunters
        .list
        .iter()
        .filter(|h| h.side == side)
        .filter_map(|h| {
            let g = Group::get_by_name(lua, &h.name).ok()?;
            let mut units = vec![];
            let mut lead: Option<(Vector2, String, f64, f64)> = None;
            for u in g.get_units().ok()?.into_iter().filter_map(|u| u.ok()) {
                let Ok(p) = u.get_point() else { continue };
                let pos = Vector2::new(p.x, p.z);
                let typ = u.get_type_name().map(|t| t.to_string()).unwrap_or_default();
                let v = u.get_velocity().map(|v| Vector2::new(v.0.x, v.0.z)).unwrap_or_default();
                let heading = (v.y.atan2(v.x).to_degrees() + 360.) % 360.;
                let kts = v.norm() / KTS;
                lead.get_or_insert((pos, typ.clone(), heading, kts));
                units.push(AssetUnit { typ, pos: to_ll(coord, pos, 0.), heading, alt_m: 0., speed_kts: kts });
            }
            let (pos, typ, heading, kts) = lead?;
            let n = units.len() as u32;
            Some(Asset {
                id: -(h.id as i64),
                name: h.label.clone(),
                kind: AssetKind::Naval,
                role: "Hunter group".into(),
                typ,
                pos: to_ll(coord, pos, 0.),
                heading,
                alt_m: 0.,
                speed_kts: kts,
                alive: n,
                total: n,
                live: true,
                task: Some(if h.going_home { "returning to port".into() } else { "hunting enemy ships".into() }),
                dest: Some(to_ll(coord, if h.going_home { h.home } else { h.target }, 0.)),
                base: None,
                range_m: None,
                orders: vec![Verb::Sail],
                units,
            })
        })
        .collect()
}
