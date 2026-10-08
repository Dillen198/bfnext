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

//! Who commands (`Cfg::command`).
//!
//! Commander access is earned by rank, which bfdb works out from the pilots'
//! campaign scores, or granted by an admin. bfdb pushes the result here with
//! the `set-commanders` RPC; every order the engine takes from a player --
//! ground formations from F10, the theatre HQ override from chat -- checks it.
//! Until bfdb has pushed a list the engine doesn't know who has earned it, so
//! it lets orders through on the older per-feature rules instead of locking
//! everyone out of a server running without bfdb.

use crate::Context;
use bfprotocols::{cfg::rank_min_score, command::Commanders};
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, net::Ucid};
use log::info;

/// Rank titles by tier, for messages (the NATO service's; the VKS ladder has
/// the same tiers, and the dashboard shows each side its own).
const RANK_TITLES: [&str; 8] = [
    "2nd Lieutenant",
    "1st Lieutenant",
    "Captain",
    "Major",
    "Lieutenant Colonel",
    "Colonel",
    "Brigadier General",
    "Major General",
];

pub(crate) fn rank_title(tier: u8) -> &'static str {
    RANK_TITLES[(tier.clamp(1, 8) - 1) as usize]
}

/// bfdb's latest roster.
pub(crate) fn set(ctx: &mut Context, commanders: Commanders) {
    if ctx.commanders.as_ref() != Some(&commanders) {
        info!(
            "command: {} Blue and {} Red commander(s) from bfdb",
            commanders.blue.len(),
            commanders.red.len()
        );
    }
    ctx.commanders = Some(commanders);
}

/// Is `ucid` a commander of `side`? None if bfdb hasn't said who is.
pub(crate) fn is_commander(ctx: &Context, ucid: &Ucid, side: Side) -> Option<bool> {
    ctx.commanders.as_ref().map(|c| c.side_of(ucid) == Some(side))
}

/// May `ucid`, on `side`, give orders? Server admins always may; so does
/// everyone when the server doesn't require commanders, or when bfdb hasn't
/// told the engine who they are. Err is the reason, for the player.
pub(crate) fn may_order(ctx: &Context, ucid: &Ucid, side: Side) -> Result<(), CompactString> {
    let cfg = &ctx.db.ephemeral.cfg;
    if !cfg.command.require_commander || cfg.admins.contains_key(ucid) {
        return Ok(());
    }
    match is_commander(ctx, ucid, side) {
        None | Some(true) => Ok(()),
        Some(false) => Err(not_commander(ctx)),
    }
}

/// What to tell a pilot who isn't a commander.
pub(crate) fn not_commander(ctx: &Context) -> CompactString {
    let tier = ctx.db.ephemeral.cfg.command.commander_rank;
    format_compact!(
        "orders need a commander: reach {} (a campaign score of {:.0}), or ask an admin",
        rank_title(tier),
        rank_min_score(tier)
    )
}

// ── The command map ──────────────────────────────────────────────────────
//
// A commander sees their side's own assets (AI flights, convoys, deployed
// units and troops, batteries, carrier groups) and orders them. Formations
// have their own picture and orders (`crate::groundwar`); the enemy is only
// ever what the side's fog-of-war pictures hold (`query-tacmap`,
// `query-ground-war`), never anything here.
//
// Nothing a commander does is free or unchecked:
// - every order is for an asset of the commander's own side, which the engine
//   looks up itself;
// - anything that creates firepower or supplies (fires, barrages, convoys,
//   helo runs, the HQ's operations) is paid from the side's treasury at the
//   HQ's prices, and batteries keep their reload; moving a player's deployed
//   units or troops costs what the server's Move action charges, by distance;
// - an HQ operation can only be one the HQ itself could run right now;
// - orders are rate limited per commander;
// - the whole side is told who ordered what.

use crate::{
    db::{
        actions::{ActionArgs, ActionCmd, WithPos, WithPosAndGroup},
        group::DeployKind,
    },
    spawnctx::SpawnCtx,
};
use bfprotocols::{
    cfg::{Action, ActionKind},
    command::{Asset, AssetKind, AssetUnit, CommandOrder, CommandPicture, CommandReply, LatLon, LaunchOption, Verb},
    db::{group::GroupId, objective::ObjectiveId},
    hq::OpKind,
    perf::PerfInner,
};
use chrono::{DateTime, Duration, Utc};
use dcso3::{
    azumith3d,
    coord::{Coord, LLPos},
    group::GroupCategory,
    land::{Land, SurfaceType},
    object::DcsObject as _,
    unit::Unit,
    LuaVec2, LuaVec3, MizLua, Vector2, Vector3,
};

/// At most this many orders per commander per minute, and this long between
/// two of them.
mod orders;
pub(crate) use orders::{tick_hunters, Hunters};

const ORDERS_PER_MIN: usize = 20;
const ORDER_GAP_MS: i64 = 1500;
const MS_TO_KTS: f64 = 1.943_844;

fn to_ll(coord: Option<&Coord>, p: Vector2, alt: f64) -> LatLon {
    coord
        .and_then(|c| c.lo_to_ll(LuaVec3(Vector3::new(p.x, alt, p.y))).ok())
        .map(|ll| [ll.latitude, ll.longitude])
        .unwrap_or([0., 0.])
}

fn from_ll(lua: MizLua, ll: LatLon) -> Result<Vector2, CompactString> {
    if !(ll[0].is_finite() && ll[1].is_finite()) || ll[0].abs() > 90. || ll[1].abs() > 180. {
        return Err("that isn't a place on the map".into());
    }
    let c = Coord::singleton(lua).map_err(|e| format_compact!("{e}"))?;
    let p = c
        .ll_to_lo(LLPos { latitude: ll[0], longitude: ll[1], altitude: 0. })
        .map_err(|e| format_compact!("{e}"))?;
    Ok(Vector2::new(p.0.x, p.0.z))
}

/// What an AI flight is, and whether it takes a Station order.
fn air_role(kind: &ActionKind) -> (&'static str, bool) {
    match kind {
        ActionKind::Tanker(_) => ("Tanker", true),
        ActionKind::Awacs(_) => ("AWACS", true),
        ActionKind::Drone(_) => ("Drone", true),
        ActionKind::Fighters(_) => ("Fighters", true),
        ActionKind::Attackers(_) => ("Attackers", true),
        ActionKind::Sead(_) => ("SEAD", true),
        ActionKind::Bomber(_) => ("Bomber", false),
        ActionKind::Recon(_) => ("Recon", false),
        ActionKind::LogisticsRepair(_) | ActionKind::LogisticsTransfer(_) => ("Air logistics", false),
        ActionKind::Paratrooper(_) => ("Paratroop drop", false),
        ActionKind::CruiseMissileSpawn(_) => ("Cruise missile", false),
        _ => ("AI flight", false),
    }
}

/// The waypoint action that retasks a flight of `kind`.
fn station_args(kind: &ActionKind, args: WithPosAndGroup<()>) -> Option<(ActionKind, ActionArgs)> {
    Some(match kind {
        ActionKind::Tanker(_) => (ActionKind::TankerWaypoint, ActionArgs::TankerWaypoint(args)),
        ActionKind::Awacs(_) => (ActionKind::AwacsWaypoint, ActionArgs::AwacsWaypoint(args)),
        ActionKind::Drone(_) => (ActionKind::DroneWaypoint, ActionArgs::DroneWaypoint(args)),
        ActionKind::Fighters(_) => (ActionKind::FighersWaypoint, ActionArgs::FightersWaypoint(args)),
        ActionKind::Attackers(_) => (ActionKind::AttackersWaypoint, ActionArgs::AttackersWaypoint(args)),
        ActionKind::Sead(_) => (ActionKind::SeadWaypoint, ActionArgs::SeadWaypoint(args)),
        _ => return None,
    })
}

/// One group as an asset: its units where DCS has them (live) or where the
/// engine last placed them. `read_live` false skips asking DCS, for groups
/// that never move (a base's battery): the query runs inside the DCS frame.
fn asset_of(
    ctx: &Context,
    lua: MizLua,
    coord: Option<&Coord>,
    gid: GroupId,
    kind: AssetKind,
    role: &str,
    orders: Vec<Verb>,
    read_live: bool,
) -> Option<Asset> {
    let g = ctx.db.persisted.groups.get(&gid)?;
    let mut units: Vec<AssetUnit> = vec![];
    let mut total = 0u32;
    let mut live = false;
    for uid in &g.units {
        let Some(u) = ctx.db.persisted.units.get(uid) else { continue };
        total += 1;
        if u.dead {
            continue;
        }
        let spawned = ctx.db.ephemeral.get_object_id_by_uid(uid).is_some();
        let read = ctx
            .db
            .ephemeral
            .get_object_id_by_uid(uid)
            .filter(|_| read_live)
            .and_then(|oid| Unit::get_instance(lua, oid).ok())
            .and_then(|unit| {
                let pos = unit.get_position().ok()?;
                let v = unit.get_velocity().map(|v| v.0).unwrap_or_default();
                Some((Vector2::new(pos.p.x, pos.p.z), pos.p.y, azumith3d(pos.x.0).to_degrees(), v.magnitude()))
            });
        let (p, alt, hdg, speed) = match read {
            Some(r) => {
                live = true;
                r
            }
            None => {
                live |= spawned;
                (u.pos, u.position.p.y, u.heading.to_degrees(), 0.)
            }
        };
        units.push(AssetUnit {
            typ: u.typ.0.to_string(),
            pos: to_ll(coord, p, alt),
            heading: hdg,
            alt_m: alt,
            speed_kts: speed * MS_TO_KTS,
        });
    }
    let lead = units.first()?.clone();
    Some(Asset {
        id: gid.inner(),
        name: g.name.to_string(),
        kind,
        role: role.to_string(),
        typ: lead.typ.clone(),
        pos: lead.pos,
        heading: lead.heading,
        alt_m: lead.alt_m,
        speed_kts: lead.speed_kts,
        alive: units.len() as u32,
        total,
        live,
        task: None,
        dest: None,
        base: None,
        range_m: None,
        orders,
        units,
    })
}

/// `side`'s own assets and what a commander can launch, for the command map.
pub(crate) fn picture(lua: MizLua, ctx: &mut Context, side: Side, now: DateTime<Utc>) -> CommandPicture {
    let coord = Coord::singleton(lua).ok();
    let coord = coord.as_ref();
    let mut assets: Vec<Asset> = vec![];
    {
        let ctx: &Context = ctx;
        // AI flights.
        for g in ctx.db.actions() {
            if g.side != side || !matches!(g.kind, Some(GroupCategory::Airplane | GroupCategory::Helicopter)) {
                continue;
            }
            let DeployKind::Action { spec, .. } = &g.origin else { continue };
            let (role, station) = air_role(&spec.kind);
            let orders = if station { vec![Verb::Station, Verb::Rtb] } else { vec![Verb::Rtb] };
            if let Some(a) = asset_of(ctx, lua, coord, g.id, AssetKind::Air, role, orders, true) {
                assets.push(a);
            }
        }
        // Supply convoys.
        for c in ctx.db.convoys_for_side(side) {
            if let Some(mut a) = asset_of(ctx, lua, coord, c.group_id, AssetKind::Convoy, "Supply convoy", vec![], true) {
                if let Some(o) = ctx.db.persisted.objectives.get(&c.destination) {
                    a.dest = Some(to_ll(coord, o.pos(), 0.));
                    a.base = Some(o.name.to_string());
                    a.task = Some(format!("Supplies to {}", o.name));
                }
                assets.push(a);
            }
        }
        // Players' deployed units and troops.
        for g in ctx.db.deployed() {
            if g.side != side {
                continue;
            }
            let kind = match &g.origin {
                DeployKind::Troop { .. } => AssetKind::Troops,
                DeployKind::Deployed { .. } => AssetKind::Deployed,
                _ => continue,
            };
            let role = match &g.origin {
                DeployKind::Troop { spec, .. } => spec.name.to_string(),
                DeployKind::Deployed { spec, .. } => spec.path.last().map(|s| s.to_string()).unwrap_or_default(),
                _ => String::new(),
            };
            let mut orders = vec![];
            if ctx.db.move_price(side, &g.id, Vector2::zeros()).ok().flatten().is_some() {
                orders.push(Verb::Move);
            }
            let range = ctx.db.battery_range(&g.id);
            if range.is_some() {
                orders.push(Verb::Fire);
            }
            if let Some(mut a) = asset_of(ctx, lua, coord, g.id, kind, &role, orders, true) {
                a.range_m = range;
                assets.push(a);
            }
        }
        // Batteries in our bases' garrisons, and carrier groups.
        for (_, o) in ctx.db.objectives() {
            if o.owner() != side {
                continue;
            }
            let Some(groups) = o.groups().get(&side) else { continue };
            if let bfprotocols::db::objective::ObjectiveKind::CarrierGroup { .. } = o.kind() {
                if let Some(gid) = groups.into_iter().next() {
                    if let Some(mut a) = asset_of(ctx, lua, coord, *gid, AssetKind::Naval, "Carrier group", vec![Verb::Sail], true) {
                        a.name = o.name.to_string();
                        a.base = Some(o.name.to_string());
                        assets.push(a);
                    }
                }
                continue;
            }
            for gid in groups {
                let Some(range) = ctx.db.battery_range(gid) else { continue };
                if let Some(mut a) = asset_of(ctx, lua, coord, *gid, AssetKind::Artillery, "Artillery", vec![Verb::Fire], false) {
                    a.range_m = Some(range);
                    a.base = Some(o.name.to_string());
                    assets.push(a);
                }
            }
        }
    }
    assets.extend(orders::hunter_assets(lua, ctx, side, coord));
    let menu = crate::hq::launch_menu_cached(ctx, side, 24, now);
    let launch = menu
        .iter()
        .map(|c| LaunchOption {
            kind: c.kind,
            objective: c.target.unwrap_or(c.anchor).inner(),
            objective_name: c.name.to_string(),
            pos: to_ll(coord, c.pos, 0.),
            cost: c.cost,
            why: c.why.to_string(),
            ready: crate::hq::launch_ready(ctx, side, c.kind, c.cost),
        })
        .collect();
    CommandPicture {
        side: format!("{side:?}"),
        time: now.timestamp(),
        treasury: ctx.db.persisted.treasury(side),
        assets,
        launch,
        hq: crate::hq::cfg(ctx).is_some(),
        orders: orders::catalog(ctx, side),
    }
}

/// The issuer's recent orders, for the rate limit. `None` keys an admin.
fn rate_limited(ctx: &mut Context, who: Option<Ucid>, now: DateTime<Utc>) -> Result<(), CompactString> {
    let q = ctx.command_orders.entry(who).or_default();
    while q.front().map_or(false, |t| now - *t > Duration::seconds(60)) {
        q.pop_front();
    }
    if let Some(last) = q.back() {
        if now - *last < Duration::milliseconds(ORDER_GAP_MS) {
            return Err("one order at a time".into());
        }
    }
    if q.len() >= ORDERS_PER_MIN {
        return Err(format_compact!("that is {ORDERS_PER_MIN} orders this minute: wait a moment"));
    }
    q.push_back(now);
    Ok(())
}

/// Can the treasury pay `cost`? Admin orders are paid like anyone's.
fn afford(ctx: &Context, side: Side, cost: i64) -> Result<(), CompactString> {
    let t = ctx.db.persisted.treasury(side);
    if cost > t {
        return Err(format_compact!("that costs {cost} and the treasury has {t}"));
    }
    Ok(())
}

fn pay(ctx: &mut Context, side: Side, cost: i64) {
    if cost > 0 {
        ctx.db.persisted.adjust_treasury(side, -cost);
        ctx.db.ephemeral.dirty();
    }
}

/// The price of an order the HQ prices, or a refusal if the server has no HQ
/// to price it: nothing that makes firepower or supplies is free.
fn price(ctx: &Context, kind: OpKind) -> Result<i64, CompactString> {
    crate::hq::cost_of(ctx, kind).ok_or_else(|| "this server has no theatre HQ to pay for that".into())
}

fn own_group(ctx: &Context, side: Side, id: i64) -> Result<GroupId, CompactString> {
    let gid = GroupId::from(id);
    match ctx.db.persisted.groups.get(&gid) {
        Some(g) if g.side == side => Ok(gid),
        Some(_) => Err("that isn't ours".into()),
        None => Err("no such group".into()),
    }
}

fn own_objective(ctx: &Context, side: Side, id: i64) -> Result<ObjectiveId, CompactString> {
    let oid = ObjectiveId::from(id);
    match ctx.db.persisted.objectives.get(&oid) {
        Some(o) if o.owner() == side => Ok(oid),
        Some(o) => Err(format_compact!("{} isn't ours", o.name)),
        None => Err("no such objective".into()),
    }
}

fn group_name(ctx: &Context, gid: &GroupId) -> String {
    ctx.db.persisted.groups.get(gid).map(|g| g.name.to_string()).unwrap_or_default()
}

fn run_action(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    side: Side,
    kind: ActionKind,
    args: ActionArgs,
) -> Result<(), CompactString> {
    let spctx = SpawnCtx::new(lua).map_err(|e| format_compact!("{e}"))?;
    let cmd = ActionCmd {
        name: "command".into(),
        action: Action { kind, cost: 0, penalty: None, limit: None, geo_limit: Default::default() },
        args,
    };
    ctx.db
        .start_action(lua, perf, &spctx, &ctx.idx, &ctx.jtac, side, None, cmd)
        .map_err(|e| format_compact!("{e}"))
}

/// Carry out a commander's order for `side`. `ucid` is the commander (None =
/// a server admin, through bfdb).
pub(crate) fn order(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    side: Side,
    ucid: Option<Ucid>,
    o: CommandOrder,
    now: DateTime<Utc>,
) -> CommandReply {
    // A formation's own order already goes in the log, by the ground war.
    let logged = !matches!(o, CommandOrder::MoveFormation { .. });
    match order_inner(lua, ctx, perf, side, ucid, o, now) {
        Ok(text) => {
            let who = ucid
                .and_then(|u| ctx.db.player(&u).map(|p| p.name.to_string()))
                .unwrap_or_else(|| "Command".into());
            info!("command: {side:?} {who}: {text}");
            if logged {
                ctx.groundwar.rt.event(side, "order", format_compact!("{who}: {text}"), None, None, now);
            }
            ctx.db.ephemeral.msgs().panel_to_side(10, false, side, format_compact!("COMMAND {who}: {text}"));
            CommandReply { ok: true, message: text.to_string() }
        }
        Err(e) => CommandReply { ok: false, message: e.to_string() },
    }
}

fn order_inner(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    side: Side,
    ucid: Option<Ucid>,
    o: CommandOrder,
    now: DateTime<Utc>,
) -> Result<CompactString, CompactString> {
    if !matches!(side, Side::Blue | Side::Red) {
        return Err("no such side".into());
    }
    if let Some(u) = ucid.as_ref() {
        let Some(p) = ctx.db.player(u) else { return Err("unknown player".into()) };
        if p.side != side {
            return Err("you can only command your own side".into());
        }
        may_order(ctx, u, side)?;
    }
    rate_limited(ctx, ucid, now)?;
    let who = ucid
        .and_then(|u| ctx.db.player(&u).map(|p| p.name.to_string()))
        .unwrap_or_else(|| "Command".into());
    match o {
        CommandOrder::Move { group, to } => {
            let gid = own_group(ctx, side, group)?;
            let pos = from_ll(lua, to)?;
            let land = Land::singleton(lua).map_err(|e| format_compact!("{e}"))?;
            if matches!(land.get_surface_type(LuaVec2(pos)), Ok(SurfaceType::Water | SurfaceType::ShallowWater)) {
                return Err("that point is in the water".into());
            }
            let base = ctx
                .db
                .move_price(side, &gid, pos)
                .map_err(|e| format_compact!("{e}"))?
                .ok_or_else(|| CompactString::from("that can't be moved on this server"))?;
            let cost = ((base as f64) * crate::hq::cost_scale(ctx)).round() as i64;
            afford(ctx, side, cost)?;
            let spctx = SpawnCtx::new(lua).map_err(|e| format_compact!("{e}"))?;
            ctx.db.command_move_group(&spctx, side, gid, pos).map_err(|e| format_compact!("{e}"))?;
            pay(ctx, side, cost);
            Ok(format_compact!("{} moving ({cost} from the treasury)", group_name(ctx, &gid)))
        }
        CommandOrder::MoveFormation { formation, to } => {
            let id: crate::db::formation::FormationId = formation;
            match ctx.db.formation(id) {
                Some(f) if f.side == side => (),
                Some(_) => return Err("that formation isn't ours".into()),
                None => return Err("no such formation".into()),
            }
            if let Some(u) = ucid.as_ref() {
                let cool = ctx.db.ground_war_cfg().map(|c| c.player_order_cooldown_secs).unwrap_or(30);
                if ctx.groundwar.last_order.get(u).map_or(false, |t| now - *t < Duration::seconds(cool as i64)) {
                    return Err("wait a moment before your next ground order".into());
                }
            }
            let pos = from_ll(lua, to)?;
            let what = ctx
                .db
                .move_formation_to(&mut ctx.groundwar.rt, lua, id, pos, ucid, now)
                .map_err(|e| format_compact!("{e}"))?;
            if let Some(u) = ucid {
                ctx.groundwar.last_order.insert(u, now);
            }
            Ok(what)
        }
        CommandOrder::Fire { group, at } => {
            let gid = own_group(ctx, side, group)?;
            let cost = price(ctx, OpKind::Artillery)?;
            afford(ctx, side, cost)?;
            let pos = from_ll(lua, at)?;
            let what = ctx.db.command_fire(lua, side, gid, pos).map_err(|e| format_compact!("{e}"))?;
            pay(ctx, side, cost);
            Ok(format_compact!("{what} ({cost} from the treasury)"))
        }
        CommandOrder::Barrage { at } => {
            let cost = price(ctx, OpKind::Artillery)?;
            afford(ctx, side, cost)?;
            let pos = from_ll(lua, at)?;
            let cfg = ctx
                .db
                .ephemeral
                .cfg
                .artillery
                .clone()
                .ok_or_else(|| CompactString::from("artillery isn't enabled on this server"))?;
            ctx.db
                .artillery_strike(lua, side, ucid, WithPos { cfg, pos })
                .map_err(|e| format_compact!("{e}"))?;
            pay(ctx, side, cost);
            Ok(format_compact!("barrage on the marked point ({cost} from the treasury)"))
        }
        CommandOrder::Station { group, at } => {
            let gid = own_group(ctx, side, group)?;
            let pos = from_ll(lua, at)?;
            let origin = ctx.db.persisted.groups.get(&gid).map(|g| g.origin.clone());
            let (kind, args) = match &origin {
                Some(DeployKind::Action { spec, .. }) => station_args(&spec.kind, WithPosAndGroup { cfg: (), pos, group: gid })
                    .ok_or_else(|| CompactString::from("that flight can't be retasked"))?,
                _ => return Err("that isn't an AI flight".into()),
            };
            run_action(lua, ctx, perf, side, kind, args)?;
            Ok(format_compact!("{} retasked", group_name(ctx, &gid)))
        }
        CommandOrder::Rtb { group } => {
            let gid = own_group(ctx, side, group)?;
            match ctx.db.persisted.groups.get(&gid) {
                Some(g)
                    if matches!(g.origin, DeployKind::Action { .. })
                        && matches!(g.kind, Some(GroupCategory::Airplane | GroupCategory::Helicopter)) => {}
                _ => return Err("that isn't an AI flight".into()),
            }
            let pos = ctx.db.group_center(&gid).map_err(|e| format_compact!("{e}"))?;
            run_action(lua, ctx, perf, side, ActionKind::Rtb, ActionArgs::Rtb(WithPosAndGroup { cfg: (), pos, group: gid }))?;
            Ok(format_compact!("{} returning to base", group_name(ctx, &gid)))
        }
        CommandOrder::Sail { group, to } => {
            let pos = from_ll(lua, to)?;
            let land = Land::singleton(lua).map_err(|e| format_compact!("{e}"))?;
            if !matches!(land.get_surface_type(LuaVec2(pos)), Ok(SurfaceType::Water)) {
                return Err("ships need deep water".into());
            }
            if group < 0 {
                return orders::retask_hunter(lua, ctx, side, (-group) as u32, pos);
            }
            let gid = own_group(ctx, side, group)?;
            run_action(
                lua,
                ctx,
                perf,
                side,
                ActionKind::CarrierWaypoint,
                ActionArgs::CarrierWaypoint(WithPosAndGroup { cfg: (), pos, group: gid }),
            )?;
            Ok("carrier group under way".into())
        }
        CommandOrder::Convoy { to } => {
            let oid = own_objective(ctx, side, to)?;
            let cost = price(ctx, OpKind::Convoy)?;
            afford(ctx, side, cost)?;
            let (_, how) = ctx.db.hq_dispatch_supply(lua, side, oid, now).map_err(|e| format_compact!("{e}"))?;
            pay(ctx, side, cost);
            let name = ctx.db.persisted.objectives.get(&oid).map(|o| o.name.to_string()).unwrap_or_default();
            Ok(format_compact!("supplies to {name} by {how} ({cost} from the treasury)"))
        }
        CommandOrder::HeloSupply { to } => {
            let oid = own_objective(ctx, side, to)?;
            let cost = price(ctx, OpKind::HeloSupply)?;
            afford(ctx, side, cost)?;
            ctx.db.call_helo_resource_delivery(lua, side, None, oid, now).map_err(|e| format_compact!("{e}"))?;
            pay(ctx, side, cost);
            let name = ctx.db.persisted.objectives.get(&oid).map(|o| o.name.to_string()).unwrap_or_default();
            Ok(format_compact!("helicopter supplies to {name} ({cost} from the treasury)"))
        }
        CommandOrder::HeloTroops { to } => {
            let oid = ObjectiveId::from(to);
            let name = ctx
                .db
                .persisted
                .objectives
                .get(&oid)
                .map(|o| o.name.to_string())
                .ok_or_else(|| CompactString::from("no such objective"))?;
            let cost = price(ctx, OpKind::HeloTroops)?;
            afford(ctx, side, cost)?;
            ctx.db.call_helo_troop_insertion(lua, side, None, oid, now).map_err(|e| format_compact!("{e}"))?;
            pay(ctx, side, cost);
            Ok(format_compact!("helicopter troops to {name} ({cost} from the treasury)"))
        }
        CommandOrder::Launch { kind, objective } => {
            // Paid, tracked and announced by the HQ itself.
            crate::hq::commander_launch(lua, ctx, perf, side, kind, ObjectiveId::from(objective), &who, now)
        }
        CommandOrder::Order { key, at, objective, to_objective } => {
            orders::order(lua, ctx, perf, side, &key, at, objective, to_objective, now)
        }
    }
}
