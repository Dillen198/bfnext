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

//! Air activity for thin servers (`Cfg::air_life`).
//!
//! Three independent parts:
//! - **Wingman**: F10 > Wingman starts an AI flight up at the friendly field
//!   nearest the requesting player, which takes off and flies DCS's Escort
//!   task on their aircraft.
//! - **Packages**: while a side has few human pilots, the engine launches that
//!   side's own Fighters / Attackers / SEAD actions against the front.
//! - **Civil traffic**: neutral airliners overfly the map and use airports
//!   well behind the front. They are spawned straight into DCS, never enter
//!   the campaign db, and every name starts with `CIV_PREFIX` -- which is what
//!   keeps the birth handler from mistaking one for a player slot.
//!
//! Everything here is session state: nothing is saved, and the military
//! flights are tagged `EventSpawn` so a restart drops them.

use crate::{Context, spawnctx::SpawnCtx};
use anyhow::{Context as AnyhowContext, Result, anyhow, bail};
use bfprotocols::{
    cfg::{
        Action, ActionKind, AiPackagesCfg, CivilAircraftCfg, CivilTrafficCfg, UnitTag,
        WingmanCfg, WingmanRole,
    },
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::PerfInner,
};
use chrono::{Duration, prelude::*};
use compact_str::{CompactString, format_compact};
use dcso3::{
    LuaEnv, LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3,
    airbase::AirbaseCategory,
    attribute::Attribute,
    coalition::{Coalition, Side},
    controller::{
        ActionTyp, AiOption, AirOption, AirReactionToThreat, AirRoe, AltType, Command,
        FollowParams, MissionPoint, OrbitPattern, PointType, Task, TurnMethod,
    },
    country::Country,
    env::miz,
    group::{Group, GroupCategory},
    land::Land,
    net::Ucid,
    unit::Unit,
    world::World,
};
use fxhash::FxHashMap;
use log::{debug, error, info, warn};
use mlua::{FromLua, Value};
use rand::{Rng, seq::SliceRandom, thread_rng};

/// Every civilian group and unit name starts with this.
pub(crate) const CIV_PREFIX: &str = "CIV ";

/// A civilian shot at more than this long before it went down is not charged
/// to the shooter -- it crashed on its own.
const CIV_HIT_MEMORY_SECS: i64 = 180;
/// A flight sent home is removed wherever it is after this long.
const RTB_GIVEUP_SECS: i64 = 1200;
/// A wingman whose player has been on the ground this long is sent home.
const WINGMAN_GROUND_GRACE_SECS: i64 = 60;
/// A wingman called on the ground waits this long for its player to take
/// off -- start-up and taxi -- before it is sent home.
const WINGMAN_TAKEOFF_WAIT_SECS: i64 = 20 * 60;
/// An arriving airliner is removed this long after it touches down.
const CIV_ROLLOUT_SECS: i64 = 45;
/// Overflights and departures are removed within this distance of their exit.
const CIV_EXIT_RADIUS_M: f64 = 8_000.;

pub(crate) fn is_civil(name: &str) -> bool {
    name.starts_with(CIV_PREFIX)
}

/// Which wingman F10 > Wingman > Request asked for.
#[derive(Debug, Clone, Copy)]
pub(crate) enum WingmanPick {
    /// The single wingman of a config with no `types`.
    Standard,
    /// A copy of the player's own aircraft.
    Clone,
    /// `WingmanCfg::types[i]`.
    Type(usize),
}

/// F10 > Wingman > Orders.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WingmanOrder {
    CoverMe,
    EngageAir,
    AttackNearMe,
    AttackMark,
    HoldHere,
    WeaponsFree,
    ReturnFire,
    WeaponsHold,
    Close,
    Spread,
}

impl WingmanOrder {
    /// In menu order.
    pub(crate) const ALL: [WingmanOrder; 10] = [
        WingmanOrder::CoverMe,
        WingmanOrder::EngageAir,
        WingmanOrder::AttackNearMe,
        WingmanOrder::AttackMark,
        WingmanOrder::HoldHere,
        WingmanOrder::WeaponsFree,
        WingmanOrder::ReturnFire,
        WingmanOrder::WeaponsHold,
        WingmanOrder::Close,
        WingmanOrder::Spread,
    ];

    pub(crate) fn label(self) -> &'static str {
        match self {
            WingmanOrder::CoverMe => "Cover me / rejoin",
            WingmanOrder::EngageAir => "Engage air threats around me",
            WingmanOrder::AttackNearMe => "Attack ground targets near me",
            WingmanOrder::AttackMark => "Attack my latest map mark",
            WingmanOrder::HoldHere => "Hold here (orbit)",
            WingmanOrder::WeaponsFree => "Weapons free",
            WingmanOrder::ReturnFire => "Return fire only",
            WingmanOrder::WeaponsHold => "Weapons hold",
            WingmanOrder::Close => "Formation: close",
            WingmanOrder::Spread => "Formation: spread",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum WingmanCmd {
    Request(WingmanPick),
    Order(WingmanOrder),
    Release,
    Status,
}

#[derive(Debug)]
struct Wingman {
    gid: GroupId,
    /// The player's unit when the wingman was called. A different unit (they
    /// died, respawned or changed aircraft) means the escort is over.
    unit_name: String,
    typ: String,
    /// "Fighter escort", "Copy of my F-16C_50", ...
    label: String,
    role: WingmanRole,
    /// The player's DCS group, which the escort follows.
    escorted: miz::GroupId,
    /// How far it goes from the player to engage while covering.
    engage: f64,
    expires_at: DateTime<Utc>,
    landed_since: Option<DateTime<Utc>>,
    /// When the wingman was called on the ground, the player has not taken
    /// off yet: until they do, being on the ground is not "landed", and the
    /// wingman holds over the field instead of escorting a parked aircraft.
    waiting_since: Option<DateTime<Utc>>,
    /// Off the ground and given its first task. Until then orders other than
    /// ROE and formation wait: a task on the takeoff waypoint is thrown away
    /// when the takeoff starts, and the wingman just flew its route.
    joined: bool,
    roe: AirRoe,
    spread: bool,
    /// What it is doing, for Status.
    doing: CompactString,
}

#[derive(Debug)]
struct Package {
    gid: GroupId,
    side: Side,
    label: CompactString,
    expires_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CivKind {
    Overflight,
    Departure,
    Arrival,
    /// A merchant ship on a sea lane.
    Ship,
}

#[derive(Debug)]
struct CivFlight {
    unit_name: String,
    callsign: CompactString,
    typ: String,
    kind: CivKind,
    spawned_at: DateTime<Utc>,
    /// Map-edge exit for overflights and departures, lane end for ships.
    exit: Option<Vector2>,
    /// Ships only: removed after this, wherever they are (a voyage takes
    /// hours, far past `max_flight_secs`).
    expires: Option<DateTime<Utc>>,
    on_ground_since: Option<DateTime<Utc>>,
}

#[derive(Debug, Default)]
pub(crate) struct AirLife {
    /// F10 wingman commands, queued by the menu callback and handled in the
    /// 1 Hz tick where spawning is safe (see `DeferEvents` in lib.rs).
    requests: Vec<(WingmanCmd, miz::GroupId, Ucid)>,
    wingmen: FxHashMap<Ucid, Wingman>,
    wingman_lost_at: FxHashMap<Ucid, DateTime<Utc>>,
    packages: Vec<Package>,
    last_package_check: Option<DateTime<Utc>>,
    last_package_launch: FxHashMap<Side, DateTime<Utc>>,
    package_seq: u32,
    /// Military flights sent home: GroupId -> when. Deleted once down, gone,
    /// or `RTB_GIVEUP_SECS` later.
    rtb: FxHashMap<GroupId, DateTime<Utc>>,
    /// Civilian flights by group name.
    civ: FxHashMap<String, CivFlight>,
    next_civ_spawn: Option<DateTime<Utc>>,
    next_ship_spawn: Option<DateTime<Utc>>,
    /// The neutral country airliners fly for, resolved once per mission.
    civ_country: Option<Country>,
    civ_country_resolved: bool,
    /// Last player to hit each civilian unit, by unit name.
    civ_hits: FxHashMap<String, (Ucid, DateTime<Utc>)>,
    /// Sides already told they have no action a package can fly.
    no_package_actions_said: Vec<Side>,
}

impl AirLife {
    pub(crate) fn queue(&mut self, cmd: WingmanCmd, menu_gid: miz::GroupId, ucid: Ucid) {
        self.requests.push((cmd, menu_gid, ucid));
    }
}

fn wingman_cfg(ctx: &Context) -> Option<WingmanCfg> {
    ctx.db
        .ephemeral
        .cfg
        .air_life
        .as_ref()
        .and_then(|a| a.wingman.clone())
        .filter(|w| w.enabled)
}

pub(crate) fn wingman_offered(cfg: &bfprotocols::cfg::Cfg) -> bool {
    cfg.air_life
        .as_ref()
        .and_then(|a| a.wingman.as_ref())
        .map(|w| w.enabled)
        .unwrap_or(false)
}

/// Human pilots of `side` currently in a slot.
fn side_pilots(ctx: &Context, side: Side) -> u32 {
    ctx.db.instanced_players().filter(|(_, p, _)| p.side == side).count() as u32
}

pub(crate) fn heading_of(v: Vector2) -> f64 {
    v.y.atan2(v.x)
}

pub(crate) fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

pub(crate) fn air_point(pos: Vector2, alt: f64, speed: f64, task: Task) -> MissionPoint {
    MissionPoint {
        typ: PointType::TurningPoint,
        airdrome_id: None,
        time_re_fu_ar: None,
        helipad: None,
        link_unit: None,
        action: Some(ActionTyp::Air(TurnMethod::FlyOverPoint)),
        pos: LuaVec2(pos),
        alt,
        alt_typ: Some(AltType::BARO),
        speed,
        speed_locked: None,
        eta: None,
        eta_locked: None,
        name: None,
        task: Box::new(task),
    }
}

// ---------------------------------------------------------------------------
// Sending military flights home
// ---------------------------------------------------------------------------

/// Order `gid` to land at the nearest friendly airbase and hand it to the RTB
/// sweep, which deletes it once it is down. Deletes it outright if it can't
/// be ordered (not in DCS, no friendly field).
pub(crate) fn send_home(
    lua: MizLua,
    ctx: &mut Context,
    gid: GroupId,
    now: DateTime<Utc>,
) {
    let Some(group) = ctx.db.persisted.groups.get(&gid) else { return };
    let (name, side) = (group.name.clone(), group.side);
    let ordered = (|| -> Result<()> {
        let g = Group::get_by_name(lua, name.as_str())?;
        let p = g.get_unit(1)?.get_point()?;
        let here = Vector2::new(p.x, p.z);
        let home = ctx
            .db
            .objectives()
            .filter(|(_, o)| o.owner() == side && o.is_airbase())
            .map(|(_, o)| o.pos())
            .min_by(|a, b| dist(*a, here).total_cmp(&dist(*b, here)))
            .ok_or_else(|| anyhow!("no friendly airbase"))?;
        g.get_controller()?
            .set_task(Task::Land { point: LuaVec2(home), duration: None })?;
        Ok(())
    })();
    match ordered {
        Ok(()) => {
            ctx.airlife.rtb.insert(gid, now);
        }
        Err(e) => {
            debug!("air_life: could not send {name} home ({e:?}), removing it");
            if let Err(e) = ctx.db.delete_group(&gid) {
                error!("air_life: could not delete {name}: {e:?}");
            }
        }
    }
}

impl AirLife {
    /// Has `gid` been sent home (and is on its way to be removed)?
    pub(crate) fn sent_home(&self, gid: &GroupId) -> bool {
        self.rtb.contains_key(gid)
    }
}

/// Remove flights that have finished flying home -- the same sweep as
/// `flush_cap_rtb`, for air_life's own flights.
fn flush_rtb(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let pending: Vec<(GroupId, DateTime<Utc>)> =
        ctx.airlife.rtb.iter().map(|(g, t)| (*g, *t)).collect();
    for (gid, ordered_at) in pending {
        let Some(name) = ctx.db.persisted.groups.get(&gid).map(|g| g.name.clone()) else {
            ctx.airlife.rtb.remove(&gid);
            continue;
        };
        let down = match Group::get_by_name(lua, name.as_str()) {
            Err(_) => true,
            Ok(g) => g.get_unit(1).and_then(|u| u.in_air()).map(|a| !a).unwrap_or(true),
        };
        if down || (now - ordered_at).num_seconds() >= RTB_GIVEUP_SECS {
            if let Err(e) = ctx.db.delete_group(&gid) {
                error!("air_life: could not delete {name} after RTB: {e:?}");
            }
            ctx.airlife.rtb.remove(&gid);
        }
    }
}

/// True if any unit of `gid` is still alive. A group the db no longer knows
/// counts as dead.
pub(crate) fn alive(ctx: &Context, gid: &GroupId) -> bool {
    ctx.db.group_health(gid).map(|(alive, _)| alive > 0).unwrap_or(false)
}

// ---------------------------------------------------------------------------
// Wingman
// ---------------------------------------------------------------------------

/// Handle the F10 wingman commands queued since the last tick. Runs at 1 Hz.
pub(crate) fn process_requests(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    now: DateTime<Utc>,
) {
    let requests = std::mem::take(&mut ctx.airlife.requests);
    for (cmd, menu_gid, ucid) in requests {
        let msg = match cmd {
            WingmanCmd::Request(pick) => request_wingman(lua, ctx, perf, ucid, pick, now),
            WingmanCmd::Order(order) => order_wingman(lua, ctx, ucid, order),
            WingmanCmd::Release => release_wingman(lua, ctx, ucid, now),
            WingmanCmd::Status => wingman_status(lua, ctx, ucid, now),
        };
        let msg = msg.unwrap_or_else(|e| {
            warn!("air_life: wingman {cmd:?} for {ucid} failed: {e:?}");
            format_compact!("Wingman request failed: {e}")
        });
        ctx.db.ephemeral.msgs().panel_to_group(12, false, menu_gid, msg);
    }
}

fn wingman_templates(
    ctx: &Context,
    cfg: &WingmanCfg,
    side: Side,
    rotary: bool,
) -> Vec<String> {
    let own = match (side, rotary) {
        (Side::Blue, false) => &cfg.templates_blue,
        (Side::Blue, true) => &cfg.rotary_templates_blue,
        (_, false) => &cfg.templates_red,
        (_, true) => &cfg.rotary_templates_red,
    };
    let mut out: Vec<String> = own.iter().map(|s| String::from(s.as_str())).collect();
    if out.is_empty()
        && let Some(ce) = ctx.db.ephemeral.cfg.campaign_events.as_deref()
    {
        let roster = match (side, rotary) {
            (Side::Blue, false) => &ce.cap_templates_blue,
            (Side::Blue, true) => &ce.helo_templates_blue,
            (_, false) => &ce.cap_templates_red,
            (_, true) => &ce.helo_templates_red,
        };
        out = roster.iter().map(|t| String::from(t.template.as_str())).collect();
    }
    out.shuffle(&mut thread_rng());
    out
}

/// What a wingman of `role` engages while it covers the player.
fn cover_targets(role: WingmanRole) -> Vec<Attribute> {
    match role {
        WingmanRole::AttackHelo => vec![Attribute::Helicopters, Attribute::GroundUnits],
        // a SEAD escort's job is the SAM shooting at you
        WingmanRole::Sead => vec![Attribute::AirDefence],
        WingmanRole::Fighter
        | WingmanRole::Strike
        | WingmanRole::Cas
        | WingmanRole::Multirole => vec![Attribute::Air],
    }
}

/// What "attack" means for a wingman of `role`.
fn attack_targets(role: WingmanRole) -> Vec<Attribute> {
    match role {
        WingmanRole::Fighter => vec![Attribute::Air],
        WingmanRole::Sead => vec![Attribute::AirDefence],
        WingmanRole::AttackHelo => vec![Attribute::GroundUnits, Attribute::Helicopters],
        WingmanRole::Strike | WingmanRole::Cas | WingmanRole::Multirole => {
            vec![Attribute::GroundUnits]
        }
    }
}

fn role_name(role: WingmanRole) -> &'static str {
    match role {
        WingmanRole::Fighter => "fighter",
        WingmanRole::Strike => "strike",
        WingmanRole::Sead => "SEAD",
        WingmanRole::Cas => "CAS",
        WingmanRole::AttackHelo => "attack helicopter",
        WingmanRole::Multirole => "multirole",
    }
}

fn roe_name(roe: AirRoe) -> &'static str {
    match roe {
        AirRoe::WeaponFree | AirRoe::OpenFireWeaponFree | AirRoe::OpenFire => {
            "weapons free"
        }
        AirRoe::ReturnFire => "return fire",
        AirRoe::WeaponHold => "weapons hold",
    }
}

/// The options a wingman flies with: unlimited fuel (it is there for the
/// whole sortie; going bingo halfway and leaving defeats the point), its
/// ROE, and evading fire.
fn wingman_options(roe: AirRoe) -> Vec<Task> {
    vec![
        Task::WrappedCommand(Command::SetUnlimitedFuel(true)),
        Task::WrappedOption(AiOption::Air(AirOption::Roe(roe))),
        Task::WrappedOption(AiOption::Air(AirOption::ReactionOnThreat(
            AirReactionToThreat::EvadeFire,
        ))),
    ]
}

/// Covering the player: DCS's Escort task on their group, at a close or a
/// spread station.
fn escort_task(w: &Wingman) -> Task {
    let rotary = w.role.rotary();
    let station = match (rotary, w.spread) {
        (true, false) => Vector3::new(-100., 20., 120.),
        (true, true) => Vector3::new(-300., 30., 400.),
        (false, false) => Vector3::new(-200., 50., 300.),
        (false, true) => Vector3::new(-600., 150., 1200.),
    };
    Task::Escort {
        engagement_dist_max: w.engage,
        target_types: cover_targets(w.role),
        params: FollowParams {
            group: w.escorted,
            pos: LuaVec3(station),
            last_waypoint_index: None,
        },
    }
}

/// Orbit over `at`, engaging `targets` within `radius` of it.
fn orbit_and_engage(
    at: Vector2,
    alt: f64,
    speed: f64,
    radius: f64,
    targets: Vec<Attribute>,
) -> Task {
    Task::ComboTask(vec![
        Task::Orbit {
            pattern: OrbitPattern::Circle,
            point: Some(LuaVec2(at)),
            point2: None,
            speed: Some(speed),
            altitude: Some(alt),
        },
        Task::EngageTargetsInZone {
            point: LuaVec2(at),
            zone_radius: radius,
            target_types: targets,
            priority: None,
        },
    ])
}

/// The newest F10 map mark the player dropped from `unit_name`.
fn latest_mark(lua: MizLua, unit_name: &str) -> Option<Vector2> {
    let mut best: Option<(dcso3::Time, Vector2)> = None;
    for mk in World::singleton(lua).ok()?.get_mark_panels().ok()? {
        let Ok(mk) = mk else { continue };
        let mine = mk
            .initiator
            .as_ref()
            .and_then(|u| u.get_name().ok())
            .is_some_and(|n| n.as_str() == unit_name);
        if mine && best.as_ref().is_none_or(|(t, _)| mk.time > *t) {
            best = Some((mk.time, Vector2::new(mk.pos.0.x, mk.pos.0.z)));
        }
    }
    best.map(|(_, p)| p)
}

fn request_wingman(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    ucid: Ucid,
    pick: WingmanPick,
    now: DateTime<Utc>,
) -> Result<CompactString> {
    let Some(cfg) = wingman_cfg(ctx) else {
        return Ok("Wingmen are not enabled on this server.".into());
    };
    if let Some(w) = ctx.airlife.wingmen.get(&ucid) {
        if alive(ctx, &w.gid) {
            return Ok(format_compact!(
                "You already have a wingman ({}). Release it first (F10 > Wingman > Release).",
                w.label
            ));
        }
        ctx.airlife.wingmen.remove(&ucid);
    }
    if let Some(lost) = ctx.airlife.wingman_lost_at.get(&ucid) {
        let wait = cfg.cooldown_secs as i64 - (now - *lost).num_seconds();
        if wait > 0 {
            return Ok(format_compact!(
                "Your last wingman was lost. A new one is available in {}m {:02}s.",
                wait / 60,
                wait % 60
            ));
        }
    }
    let player = ctx.db.player(&ucid).ok_or_else(|| anyhow!("unknown player"))?;
    let side = player.side;
    let points = player.points;
    let Some(inst) = player.current_slot.as_ref().and_then(|(_, i)| i.as_ref()) else {
        return Ok("Get in an aircraft first.".into());
    };
    // On the ground is fine: the wingman starts up at the nearest field,
    // which is usually the player's own, and holds overhead until they are
    // up. It used to have to be called in the air, from when it spawned in
    // the air; now it taxis and takes off like the player does.
    let on_ground = !inst.in_air;
    let pilots = side_pilots(ctx, side);
    if cfg.max_side_pilots > 0 && pilots > cfg.max_side_pilots {
        return Ok(format_compact!(
            "Wingmen are for quiet servers: available while your side has {} or fewer pilots up \
             (it has {pilots}).",
            cfg.max_side_pilots
        ));
    }
    let my_typ = String::from(inst.typ.as_str());
    let unit_name = inst.unit_name.clone();
    let here = Vector2::new(inst.position.p.x, inst.position.p.z);
    let player_alt = inst.position.p.y;
    let my_speed = inst.velocity.norm();
    let rotary = ctx
        .db
        .ephemeral
        .cfg
        .unit_classification
        .get(&inst.typ)
        .map(|t| t.contains(UnitTag::Helicopter))
        .unwrap_or(false);

    // What was asked for: its aircraft, role, price and name.
    let (templates, role, cost, label): (Vec<String>, WingmanRole, i32, String) =
        match pick {
            WingmanPick::Standard => (
                wingman_templates(ctx, &cfg, side, rotary),
                if rotary { WingmanRole::AttackHelo } else { WingmanRole::Fighter },
                cfg.cost,
                String::from("Standard wingman"),
            ),
            WingmanPick::Type(i) => {
                let Some(t) = cfg.types.get(i) else {
                    return Ok("That wingman is no longer offered.".into());
                };
                if t.role.rotary() != rotary {
                    return Ok(format_compact!(
                        "A {} can't fly with a {}.",
                        t.name,
                        if rotary { "helicopter" } else { "jet" }
                    ));
                }
                let mut list: Vec<String> = match side {
                    Side::Blue => &t.templates_blue,
                    _ => &t.templates_red,
                }
                .iter()
                .map(|s| String::from(s.as_str()))
                .collect();
                list.shuffle(&mut thread_rng());
                (list, t.role, t.cost, String::from(t.name.as_str()))
            }
            WingmanPick::Clone => {
                let Some(cost) =
                    cfg.clone.as_ref().and_then(|c| c.price(my_typ.as_str()))
                else {
                    return Ok(format_compact!(
                        "A {my_typ} can't be copied on this server."
                    ));
                };
                // Its own slot first (same loadout and paint as briefed), then
                // any other slot of the type on this side.
                let mut list: Vec<String> = vec![];
                if let Ok(own) = Unit::get_by_name(lua, unit_name.as_str())
                    .and_then(|u| u.get_group())
                    .and_then(|g| g.get_name())
                {
                    list.push(own);
                }
                if let Some(t) =
                    ctx.db.ephemeral.clone_templates.get(&(side, my_typ.clone()))
                    && !list.contains(t)
                {
                    list.push(t.clone());
                }
                (
                    list,
                    if rotary { WingmanRole::AttackHelo } else { WingmanRole::Multirole },
                    cost,
                    String::from(format_compact!("Copy of my {my_typ}")),
                )
            }
        };
    if cost > 0 && points < cost {
        return Ok(format_compact!("{label} costs {cost} points and you have {points}."));
    }
    if templates.is_empty() {
        return Ok(format_compact!(
            "No aircraft is configured for {label} on your side."
        ));
    }
    let speed = if rotary { my_speed.max(40.) } else { my_speed.max(130.) };
    // It comes up from the nearest friendly field that can launch it -- a
    // jet needs free parking at an airbase, a helicopter any airbase, FARP
    // or FOB -- starts up there and flies out to join, instead of appearing
    // in the air behind the player.
    let Some(field) = ctx.db.pick_launch_field(lua, side, here, rotary, false, 0.) else {
        return Ok(format_compact!(
            "No friendly {} near you has room to launch a wingman right now.",
            if rotary { "airbase, FARP or FOB" } else { "airfield" }
        ));
    };
    let origin: ObjectiveId = field.oid;
    // The player's DCS group is what the Escort task follows.
    let escorted = Unit::get_by_name(lua, unit_name.as_str())
        .and_then(|u| u.get_group())
        .and_then(|g| g.id())
        .context("finding your aircraft")?;

    // Heads for where the player is now at the player's altitude, never
    // inside the hill under them, until the escort task picks them up.
    let floor = if rotary { 60. } else { 150. };
    let land = Land::singleton(lua).ok();
    let altitude = land
        .as_ref()
        .and_then(|l| l.get_height(LuaVec2(here)).ok())
        .map(|g| player_alt.max(g + floor))
        .unwrap_or(player_alt);
    let field_elev =
        land.as_ref().and_then(|l| l.get_height(LuaVec2(field.pos)).ok()).unwrap_or(0.);
    // Waypoint 0 is the field: the spawn turns it into the takeoff. The
    // route then points it at the player until it is up to take the escort.
    // Called on the ground, it circles over the player until they are up
    // (`join_up` waits for both) instead of flying past the end of its route.
    let hold = if on_ground {
        vec![Task::Orbit {
            pattern: OrbitPattern::Circle,
            point: Some(LuaVec2(here)),
            point2: None,
            speed: Some(speed),
            altitude: Some(altitude),
        }]
    } else {
        vec![]
    };
    let mission = vec![
        air_point(
            field.pos,
            field_elev,
            speed,
            Task::ComboTask(wingman_options(AirRoe::WeaponFree)),
        ),
        air_point(here, altitude, speed, Task::ComboTask(hold)),
    ];

    let spctx = SpawnCtx::new(lua)?;
    let mut spawned = None;
    let mut last_err = None;
    for template in &templates {
        match ctx.db.spawn_air_flight(
            perf,
            &spctx,
            &ctx.idx,
            side,
            template.as_str(),
            origin,
            field.pos,
            heading_of(here - field.pos),
            false,
            mission.clone(),
        ) {
            Ok(gid) => {
                spawned = Some(gid);
                break;
            }
            Err(e) => {
                warn!("air_life: wingman template {template} would not spawn: {e:?}");
                last_err = Some(e);
            }
        }
    }
    let Some(gid) = spawned else {
        return Err(last_err.unwrap_or_else(|| anyhow!("no template spawned")));
    };
    let typ = ctx
        .db
        .persisted
        .groups
        .get(&gid)
        .and_then(|g| g.units.into_iter().next().copied())
        .and_then(|uid| ctx.db.persisted.units.get(&uid))
        .map(|u| String::from(u.typ.0.as_str()))
        .unwrap_or_default();
    // Charged only once it exists: a wingman that would not spawn is free.
    if cost > 0 {
        ctx.db.adjust_points(&ucid, -cost, &format_compact!("for a wingman ({label})"));
    }
    info!("air_life: {ucid} called {label}: {typ} ({gid:?}) for {unit_name}");
    let engage = if rotary { cfg.rotary_engage_dist_m } else { cfg.engage_dist_m };
    ctx.airlife.wingmen.insert(
        ucid,
        Wingman {
            gid,
            unit_name,
            typ: typ.clone(),
            label: label.clone(),
            role,
            escorted,
            engage,
            expires_at: now + Duration::seconds(cfg.lifetime_secs as i64),
            landed_since: None,
            waiting_since: on_ground.then_some(now),
            joined: false,
            roe: AirRoe::WeaponFree,
            spread: false,
            doing: if on_ground {
                "taking off, then waiting over you".into()
            } else {
                "taking off to join you".into()
            },
        },
    );
    let joins = if on_ground {
        "It takes off, holds over you until you are airborne, then joins on your wing"
    } else {
        "It takes off and joins on your wing"
    };
    Ok(format_compact!(
        "{label}: a {typ} ({}) is starting up at {} ({:.0} km from you){}. {joins}, and \
         stays for {} min, or until you land or release it. F10 > Wingman > Orders to direct \
         it.",
        role_name(role),
        field.name,
        dist(field.pos, here) / 1000.,
        if cost > 0 { format_compact!(", {cost} points") } else { "".into() },
        cfg.lifetime_secs / 60
    ))
}

/// F10 > Wingman > Orders.
fn order_wingman(
    lua: MizLua,
    ctx: &mut Context,
    ucid: Ucid,
    order: WingmanOrder,
) -> Result<CompactString> {
    let Some(w) = ctx.airlife.wingmen.get(&ucid) else {
        return Ok("You have no wingman. F10 > Wingman > Request calls one.".into());
    };
    if !alive(ctx, &w.gid) {
        return Ok("Your wingman is gone.".into());
    }
    let name = ctx
        .db
        .persisted
        .groups
        .get(&w.gid)
        .map(|g| g.name.clone())
        .ok_or_else(|| anyhow!("wingman group missing"))?;
    let rotary = w.role.rotary();
    let joined = w.joined;
    let (here, alt) = match ctx
        .db
        .player(&ucid)
        .and_then(|p| p.current_slot.as_ref())
        .and_then(|(_, i)| i.as_ref())
    {
        Some(i) => (Vector2::new(i.position.p.x, i.position.p.z), i.position.p.y),
        None => return Ok("Get back in your aircraft first.".into()),
    };
    let ground = Land::singleton(lua).ok().and_then(|l| l.get_height(LuaVec2(here)).ok());
    let (floor, speed, attack_radius) =
        if rotary { (100., 50., 4_000.) } else { (900., 190., 10_000.) };
    let orbit_alt = ground.map(|g| alt.max(g + floor)).unwrap_or(alt);
    let group = Group::get_by_name(lua, name.as_str()).context("finding your wingman")?;
    let ctrl = group.get_controller()?;

    // ROE and formation can be set any time; a task has to wait until it
    // is off the ground (see `Wingman::joined`).
    let roe = match order {
        WingmanOrder::WeaponsFree => Some(AirRoe::WeaponFree),
        WingmanOrder::ReturnFire => Some(AirRoe::ReturnFire),
        WingmanOrder::WeaponsHold => Some(AirRoe::WeaponHold),
        _ => None,
    };
    if let Some(roe) = roe {
        ctrl.set_option(AiOption::Air(AirOption::Roe(roe)))?;
        if let Some(w) = ctx.airlife.wingmen.get_mut(&ucid) {
            w.roe = roe;
        }
        return Ok(format_compact!("Wingman: {}, copy.", roe_name(roe)));
    }
    if matches!(order, WingmanOrder::Close | WingmanOrder::Spread) {
        let spread = order == WingmanOrder::Spread;
        let w = ctx.airlife.wingmen.get_mut(&ucid).expect("present");
        w.spread = spread;
        let covering = w.joined && w.doing.as_str() == "covering you";
        if covering {
            ctrl.set_task(escort_task(w))?;
        }
        return Ok(format_compact!(
            "Wingman: {} formation{}.",
            if spread { "spread" } else { "close" },
            if covering { "" } else { " when it next covers you" }
        ));
    }
    if !joined {
        return Ok(
            "Your wingman is still taking off. It joins you first, then takes orders."
                .into(),
        );
    }
    let w = ctx.airlife.wingmen.get(&ucid).expect("present");
    let (task, doing, reply): (Task, CompactString, CompactString) = match order {
        WingmanOrder::CoverMe => (
            escort_task(w),
            "covering you".into(),
            "Wingman: rejoining, covering you.".into(),
        ),
        WingmanOrder::EngageAir => (
            orbit_and_engage(here, orbit_alt, speed, w.engage, vec![Attribute::Air]),
            "engaging air threats around you".into(),
            "Wingman: engaging air threats around you.".into(),
        ),
        WingmanOrder::AttackNearMe => (
            orbit_and_engage(
                here,
                orbit_alt,
                speed,
                attack_radius,
                attack_targets(w.role),
            ),
            "attacking targets near you".into(),
            format_compact!(
                "Wingman: attacking targets within {:.0} km of you.",
                attack_radius / 1000.
            ),
        ),
        WingmanOrder::AttackMark => {
            let Some(mark) = latest_mark(lua, w.unit_name.as_str()) else {
                return Ok(
                    "Drop a mark on the F10 map first: your wingman attacks around your newest one."
                        .into(),
                );
            };
            let mark_alt = Land::singleton(lua)
                .ok()
                .and_then(|l| l.get_height(LuaVec2(mark)).ok())
                .map(|g| orbit_alt.max(g + floor))
                .unwrap_or(orbit_alt);
            (
                orbit_and_engage(
                    mark,
                    mark_alt,
                    speed,
                    attack_radius,
                    attack_targets(w.role),
                ),
                "attacking your map mark".into(),
                format_compact!(
                    "Wingman: on the way to your mark, {:.0} km out.",
                    dist(mark, here) / 1000.
                ),
            )
        }
        WingmanOrder::HoldHere => (
            orbit_and_engage(here, orbit_alt, speed, w.engage, cover_targets(w.role)),
            "holding where you ordered".into(),
            "Wingman: holding here.".into(),
        ),
        WingmanOrder::WeaponsFree
        | WingmanOrder::ReturnFire
        | WingmanOrder::WeaponsHold
        | WingmanOrder::Close
        | WingmanOrder::Spread => unreachable!("handled above"),
    };
    ctrl.set_task(task)?;
    if let Some(w) = ctx.airlife.wingmen.get_mut(&ucid) {
        w.doing = doing;
    }
    Ok(reply)
}

fn release_wingman(
    lua: MizLua,
    ctx: &mut Context,
    ucid: Ucid,
    now: DateTime<Utc>,
) -> Result<CompactString> {
    match ctx.airlife.wingmen.remove(&ucid) {
        Some(w) if alive(ctx, &w.gid) => {
            send_home(lua, ctx, w.gid, now);
            Ok(format_compact!("{} released, RTB.", w.label))
        }
        _ => Ok("You have no wingman.".into()),
    }
}

fn wingman_status(
    lua: MizLua,
    ctx: &mut Context,
    ucid: Ucid,
    now: DateTime<Utc>,
) -> Result<CompactString> {
    let Some(w) = ctx.airlife.wingmen.get(&ucid) else {
        return Ok("You have no wingman. F10 > Wingman > Request calls one.".into());
    };
    let (alive_n, total) = ctx.db.group_health(&w.gid).unwrap_or((0, 0));
    let left = (w.expires_at - now).num_minutes().max(0);
    let range = (|| -> Option<f64> {
        let name = ctx.db.persisted.groups.get(&w.gid)?.name.clone();
        let p = Group::get_by_name(lua, name.as_str())
            .ok()?
            .get_unit(1)
            .ok()?
            .get_point()
            .ok()?;
        let me = Unit::get_by_name(lua, w.unit_name.as_str()).ok()?.get_point().ok()?;
        Some(dist(Vector2::new(p.x, p.z), Vector2::new(me.x, me.z)) / 1852.)
    })();
    let range =
        range.map(|nm| format_compact!(", {nm:.1} nm from you")).unwrap_or_default();
    Ok(format_compact!(
        "{} ({}, {}): {alive_n}/{total} aircraft{range}, {}, {}, {} formation, {left} min left.",
        w.label,
        w.typ,
        role_name(w.role),
        w.doing,
        roe_name(w.roe),
        if w.spread { "spread" } else { "close" },
    ))
}

/// Give a wingman that has just got off the ground its first task: cover
/// the player, at the ROE they have set meanwhile.
fn join_up(lua: MizLua, ctx: &mut Context, ucid: Ucid) {
    let Some(w) = ctx.airlife.wingmen.get(&ucid) else { return };
    // Called on the ground: it holds overhead until its player is up too.
    if w.joined || w.waiting_since.is_some() {
        return;
    }
    let mut tasks = wingman_options(w.roe);
    tasks.push(escort_task(w));
    let Some(name) = ctx.db.persisted.groups.get(&w.gid).map(|g| g.name.clone()) else {
        return;
    };
    let joined = (|| -> Result<bool> {
        let g = Group::get_by_name(lua, name.as_str())?;
        if !g.get_unit(1)?.in_air()? {
            return Ok(false);
        }
        g.get_controller()?.set_task(Task::ComboTask(tasks))?;
        Ok(true)
    })();
    match joined {
        Ok(true) => {
            info!("air_life: wingman {name} airborne, escorting {ucid}");
            if let Some(w) = ctx.airlife.wingmen.get_mut(&ucid) {
                w.joined = true;
                w.doing = "covering you".into();
            }
        }
        Ok(false) => (),
        Err(e) => warn!("air_life: tasking wingman {name} to escort {ucid}: {e:?}"),
    }
}

/// Keep every wingman honest: gone when shot down, home when the player is
/// gone, down, or out of time.
fn tick_wingmen(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let ucids: Vec<Ucid> = ctx.airlife.wingmen.keys().copied().collect();
    for ucid in ucids {
        let Some(w) = ctx.airlife.wingmen.get(&ucid) else { continue };
        let gid = w.gid;
        if !alive(ctx, &gid) {
            let label = w.label.clone();
            ctx.airlife.wingmen.remove(&ucid);
            ctx.airlife.wingman_lost_at.insert(ucid, now);
            let _ = ctx.db.delete_group(&gid);
            ctx.db.ephemeral.panel_to_player(
                &ctx.db.persisted,
                12,
                &ucid,
                format_compact!("Your wingman ({label}) is down."),
            );
            continue;
        }
        let inst = ctx
            .db
            .player(&ucid)
            .and_then(|p| p.current_slot.as_ref())
            .and_then(|(_, i)| i.as_ref())
            .map(|i| (i.unit_name.clone(), i.in_air));
        let why: Option<&str> = match inst {
            Some((unit, _)) if unit != w.unit_name => Some(""),
            None => Some(""),
            // Called on the ground and the player has not taken off yet.
            Some((_, false)) if w.waiting_since.is_some() => w
                .waiting_since
                .filter(|since| (now - *since).num_seconds() >= WINGMAN_TAKEOFF_WAIT_SECS)
                .map(|_| "you did not take off"),
            Some((_, false)) => {
                let since = *ctx
                    .airlife
                    .wingmen
                    .get_mut(&ucid)
                    .expect("present")
                    .landed_since
                    .get_or_insert(now);
                ((now - since).num_seconds() >= WINGMAN_GROUND_GRACE_SECS)
                    .then_some("you are on the ground")
            }
            Some((_, true)) => {
                if let Some(w) = ctx.airlife.wingmen.get_mut(&ucid) {
                    w.landed_since = None;
                    w.waiting_since = None;
                }
                (now >= ctx.airlife.wingmen[&ucid].expires_at)
                    .then_some("it is out of time")
            }
        };
        if let Some(why) = why {
            let w = ctx.airlife.wingmen.remove(&ucid).expect("present");
            send_home(lua, ctx, w.gid, now);
            if !why.is_empty() {
                ctx.db.ephemeral.panel_to_player(
                    &ctx.db.persisted,
                    12,
                    &ucid,
                    format_compact!("Your wingman ({}) is RTB: {why}.", w.label),
                );
            }
            continue;
        }
        join_up(lua, ctx, ucid);
    }
    // Cooldowns only matter for a while.
    ctx.airlife.wingman_lost_at.retain(|_, t| (now - *t).num_seconds() < 3600);
}

#[cfg(test)]
mod wingman_tests {
    use super::*;

    #[test]
    fn every_order_has_a_label_and_a_menu_slot() {
        let mut seen = std::collections::HashSet::new();
        for o in WingmanOrder::ALL {
            assert!(seen.insert(o.label()), "{o:?} shares a label");
        }
        assert_eq!(seen.len(), 10);
    }

    #[test]
    fn roles_engage_what_they_are_for() {
        assert_eq!(cover_targets(WingmanRole::Fighter), vec![Attribute::Air]);
        assert_eq!(cover_targets(WingmanRole::Sead), vec![Attribute::AirDefence]);
        assert_eq!(attack_targets(WingmanRole::Sead), vec![Attribute::AirDefence]);
        assert_eq!(attack_targets(WingmanRole::Strike), vec![Attribute::GroundUnits]);
        assert_eq!(attack_targets(WingmanRole::Fighter), vec![Attribute::Air]);
        assert!(
            attack_targets(WingmanRole::AttackHelo).contains(&Attribute::Helicopters)
        );
        // a copy of your jet covers you in the air and attacks on the ground
        assert_eq!(cover_targets(WingmanRole::Multirole), vec![Attribute::Air]);
        assert_eq!(attack_targets(WingmanRole::Multirole), vec![Attribute::GroundUnits]);
    }
}

// ---------------------------------------------------------------------------
// Front geometry
// ---------------------------------------------------------------------------

/// Points along the front: the midpoint between every red/blue objective and
/// the nearest objective of the other side. Crude, but it follows the real
/// contact line wherever it bends, and it needs nothing but objective owners.
pub(crate) fn front_points(ctx: &Context) -> Vec<Vector2> {
    let mut red = vec![];
    let mut blue = vec![];
    for (_, o) in ctx.db.objectives() {
        match o.owner() {
            Side::Red => red.push(o.pos()),
            Side::Blue => blue.push(o.pos()),
            Side::Neutral => (),
        }
    }
    let mut out = vec![];
    for (mine, theirs) in [(&red, &blue), (&blue, &red)] {
        for p in mine.iter() {
            if let Some(q) =
                theirs.iter().min_by(|a, b| dist(**a, *p).total_cmp(&dist(**b, *p)))
            {
                out.push((*p + *q) * 0.5);
            }
        }
    }
    out
}

pub(crate) fn front_dist(front: &[Vector2], p: Vector2) -> f64 {
    front.iter().map(|f| dist(*f, p)).fold(f64::INFINITY, f64::min)
}

/// Closest approach of the segment a-b to the front.
fn segment_front_dist(front: &[Vector2], a: Vector2, b: Vector2) -> f64 {
    let ab = b - a;
    let len2 = ab.norm_squared().max(1e-6);
    front
        .iter()
        .map(|p| {
            let t = ((*p - a).dot(&ab) / len2).clamp(0., 1.);
            dist(a + ab * t, *p)
        })
        .fold(f64::INFINITY, f64::min)
}

// ---------------------------------------------------------------------------
// Autonomous packages
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PackageKind {
    Cap,
    Strike,
    Sead,
}

impl PackageKind {
    fn label(self) -> &'static str {
        match self {
            Self::Cap => "CAP",
            Self::Strike => "Strike",
            Self::Sead => "SEAD",
        }
    }

    fn of(kind: &ActionKind) -> Option<Self> {
        match kind {
            ActionKind::Fighters(_) => Some(Self::Cap),
            ActionKind::Attackers(_) => Some(Self::Strike),
            ActionKind::Sead(_) => Some(Self::Sead),
            _ => None,
        }
    }

    fn weight(self) -> u32 {
        match self {
            Self::Strike => 3,
            Self::Cap => 2,
            Self::Sead => 1,
        }
    }
}

/// The side's actions air_life may launch, by package kind.
fn package_actions(
    ctx: &Context,
    cfg: &AiPackagesCfg,
    side: Side,
) -> Vec<(PackageKind, Action)> {
    let names = match side {
        Side::Blue => &cfg.actions_blue,
        _ => &cfg.actions_red,
    };
    let Some(actions) = ctx.db.ephemeral.cfg.actions.get(&side) else { return vec![] };
    actions
        .iter()
        .filter(|(name, _)| {
            names.is_empty() || names.iter().any(|n| n.as_str() == name.as_str())
        })
        .filter_map(|(_, a)| PackageKind::of(&a.kind).map(|k| (k, a.clone())))
        .collect()
}

/// Where a package of each kind would go right now, for `side`: the front
/// segment nearest a friendly pilot in the air (so the AI turns up where the
/// player is), or the tightest one when nobody is up.
fn package_targets(
    ctx: &Context,
    cfg: &AiPackagesCfg,
    side: Side,
) -> Vec<(PackageKind, Vector2, CompactString)> {
    let enemy = side.opposite();
    let friendly: Vec<(Vector2, bool)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == side)
        .map(|(_, o)| (o.pos(), o.is_airbase()))
        .collect();
    if friendly.is_empty() {
        return vec![];
    }
    let airbases: Vec<Vector2> =
        friendly.iter().filter(|(_, ab)| *ab).map(|(p, _)| *p).collect();
    let in_range =
        |p: Vector2| airbases.iter().any(|ab| dist(*ab, p) <= cfg.max_target_range_m);
    // (enemy objective pos, name, kind, nearest friendly pos, gap)
    let mut pairs: Vec<(Vector2, CompactString, ObjectiveKind, Vector2, f64)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == enemy)
        .filter_map(|(_, o)| {
            let e = o.pos();
            let (f, d) = friendly
                .iter()
                .map(|(p, _)| (*p, dist(*p, e)))
                .min_by(|a, b| a.1.total_cmp(&b.1))?;
            Some((e, CompactString::from(o.name()), o.kind().clone(), f, d))
        })
        .collect();
    pairs.sort_by(|a, b| a.4.total_cmp(&b.4));
    // The front is the handful of tightest pairs; from those pick the one
    // nearest a friendly pilot in the air, or a random one of the three
    // tightest when nobody is up.
    let front: Vec<_> = pairs.iter().take(6).cloned().collect();
    // A friendly pilot in the air wins; otherwise the side's offensive axis
    // (modern_war tempo), so packages follow the operational plan.
    let anchor: Option<Vector2> = ctx
        .db
        .instanced_players()
        .filter(|(_, p, i)| p.side == side && i.in_air)
        .map(|(_, _, i)| Vector2::new(i.position.p.x, i.position.p.z))
        .next()
        .or_else(|| {
            ctx.modern_war
                .tempo
                .axis(side)
                .and_then(|a| ctx.db.persisted.objectives.get(&a))
                .map(|o| o.pos())
        });
    let pick = match anchor {
        Some(a) => front.iter().min_by(|x, y| {
            dist((x.0 + x.3) * 0.5, a).total_cmp(&dist((y.0 + y.3) * 0.5, a))
        }),
        None => {
            let n = front.len().min(3);
            (n > 0).then(|| &front[thread_rng().gen_range(0..n)])
        }
    };
    let Some((e_pos, e_name, _, f_pos, _)) = pick.cloned() else { return vec![] };
    let mut out = vec![];
    let mid = (e_pos + f_pos) * 0.5;
    if in_range(mid) {
        out.push((PackageKind::Cap, mid, e_name.clone()));
    }
    // Strike the nearest enemy objective to that front segment that is not a
    // SAM site or a carrier -- an attack flight sent at either just dies.
    if let Some((p, n, ..)) = pairs
        .iter()
        .filter(|(_, _, k, _, _)| {
            !matches!(
                k,
                ObjectiveKind::SpecialSamSite | ObjectiveKind::CarrierGroup { .. }
            )
        })
        .min_by(|a, b| dist(a.0, mid).total_cmp(&dist(b.0, mid)))
        .filter(|(p, ..)| in_range(*p))
    {
        out.push((PackageKind::Strike, *p, n.clone()));
    }
    // SEAD goes after the SAM site covering that segment, if there is one.
    if let Some((p, n, ..)) = pairs
        .iter()
        .filter(|(_, _, k, _, _)| matches!(k, ObjectiveKind::SpecialSamSite))
        .filter(|(p, ..)| dist(*p, mid) <= 60_000.)
        .min_by(|a, b| dist(a.0, mid).total_cmp(&dist(b.0, mid)))
        .filter(|(p, ..)| in_range(*p))
    {
        out.push((PackageKind::Sead, *p, n.clone()));
    }
    out
}

fn tick_packages(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    now: DateTime<Utc>,
) {
    let Some(cfg) = ctx
        .db
        .ephemeral
        .cfg
        .air_life
        .as_ref()
        .and_then(|a| a.packages.clone())
        .filter(|p| p.enabled)
    else {
        return;
    };
    // Maintain the packages already up.
    let mut i = 0;
    while i < ctx.airlife.packages.len() {
        let p = &ctx.airlife.packages[i];
        let gid = p.gid;
        if !alive(ctx, &gid) {
            info!("air_life: {:?} {} package {gid:?} lost", p.side, p.label);
            ctx.airlife.packages.swap_remove(i);
            let _ = ctx.db.delete_group(&gid);
            continue;
        }
        if now >= p.expires_at {
            info!("air_life: {:?} {} package {gid:?} done, RTB", p.side, p.label);
            ctx.airlife.packages.swap_remove(i);
            send_home(lua, ctx, gid, now);
            continue;
        }
        i += 1;
    }
    if ctx
        .airlife
        .last_package_check
        .map(|t| (now - t).num_seconds() < cfg.check_interval_secs as i64)
        .unwrap_or(false)
    {
        return;
    }
    // The theatre HQ flies the air war when it is on -- the same packages,
    // aimed by its plan and bought out of the same treasury -- so these
    // stand down rather than double it.
    if crate::hq::active(&ctx.db.ephemeral.cfg) {
        return;
    }
    ctx.airlife.last_package_check = Some(now);
    let total = ctx.db.instanced_players().count();
    if total == 0 && !cfg.run_when_empty {
        return;
    }
    let mut sides = [Side::Blue, Side::Red];
    sides.shuffle(&mut thread_rng());
    for side in sides {
        let pilots = side_pilots(ctx, side);
        if pilots > cfg.max_side_pilots {
            continue;
        }
        // ai_fill: fewer packages up, and further apart, for a side matched
        // to a handful of enemy pilots (or with its own pilots up).
        let fill = crate::ai_fill::packages(ctx, side);
        if fill.is_some_and(|f| f < bfprotocols::cfg::AI_FILL_MIN_PRESSURE) {
            continue;
        }
        let fill = fill.unwrap_or(1.);
        let max_active = if fill < 1. {
            (cfg.max_active_per_side as f64 * fill).ceil() as usize
        } else {
            cfg.max_active_per_side as usize
        };
        let active = ctx.airlife.packages.iter().filter(|p| p.side == side).count();
        if active >= max_active {
            continue;
        }
        if ctx
            .airlife
            .last_package_launch
            .get(&side)
            .map(|t| {
                let tempo = ctx
                    .db
                    .ephemeral
                    .cfg
                    .modern_war
                    .as_ref()
                    .and_then(|m| m.tempo.as_ref());
                let factor = ctx.modern_war.tempo.factor(tempo, side, now) / fill;
                (now - *t).num_seconds()
                    < (cfg.launch_interval_secs as f64 * factor) as i64
            })
            .unwrap_or(false)
        {
            continue;
        }
        if cfg.treasury_cost > 0 && ctx.db.persisted.treasury(side) < cfg.treasury_cost {
            debug!("air_life: {side:?} can't afford a package");
            continue;
        }
        let actions = package_actions(ctx, &cfg, side);
        if actions.is_empty() {
            // Packages fly the side's Fighters / Attackers / SEAD actions. A
            // config that sells none of them gets no packages at all, and this
            // used to skip without a word.
            if !ctx.airlife.no_package_actions_said.contains(&side) {
                ctx.airlife.no_package_actions_said.push(side);
                info!(
                    "air_life: {side:?} has no Fighters, Attackers or SEAD action configured -- AI packages can't launch for it"
                );
            }
            continue;
        }
        let targets = package_targets(ctx, &cfg, side);
        // Every (action, target) the side could fly, weighted by kind.
        let options: Vec<(&Action, PackageKind, Vector2, &CompactString)> = actions
            .iter()
            .flat_map(|(k, a)| {
                targets
                    .iter()
                    .filter(move |(tk, ..)| tk == k)
                    .map(move |(tk, pos, name)| (a, *tk, *pos, name))
            })
            .collect();
        let Ok(&(action, kind, target, target_name)) =
            options.choose_weighted(&mut thread_rng(), |o| o.1.weight())
        else {
            debug!("air_life: {side:?} has no package target");
            continue;
        };
        ctx.airlife.package_seq += 1;
        let name = String::from(format_compact!(
            "AI {} {}",
            kind.label(),
            ctx.airlife.package_seq
        ));
        let spctx = match SpawnCtx::new(lua) {
            Ok(s) => s,
            Err(e) => {
                error!("air_life: no spawn ctx {e:?}");
                return;
            }
        };
        match ctx.db.spawn_auto_package(
            perf,
            &spctx,
            &ctx.idx,
            side,
            name.clone(),
            action.clone(),
            target,
        ) {
            Ok(gid) => {
                info!("air_life: {side:?} launched {name} ({gid:?}) over {target_name}");
                ctx.airlife.last_package_launch.insert(side, now);
                if cfg.treasury_cost > 0 {
                    ctx.db.persisted.adjust_treasury(side, -cfg.treasury_cost);
                    ctx.db.ephemeral.dirty();
                }
                ctx.airlife.packages.push(Package {
                    gid,
                    side,
                    label: kind.label().into(),
                    expires_at: now + Duration::seconds(cfg.lifetime_secs as i64),
                });
                if cfg.announce {
                    ctx.db.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        side,
                        format_compact!(
                            "Friendly AI {} flight launching, tasked over {target_name}.",
                            kind.label()
                        ),
                    );
                }
            }
            Err(e) => {
                warn!(
                    "air_life: {side:?} {name} over {target_name} would not launch: {e:?}"
                );
                // Don't retry the same failure every check.
                ctx.airlife.last_package_launch.insert(side, now);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Civil traffic
// ---------------------------------------------------------------------------

struct Airport {
    id: dcso3::airbase::AirbaseId,
    name: String,
    pos: Vector2,
    elevation: f64,
}

/// The first country in the mission's neutral coalition. DCS only lets a
/// group into a coalition its country belongs to, so this is what makes the
/// airliners neutral rather than red or blue.
fn mission_neutral_country(lua: MizLua) -> Result<Option<Country>> {
    let miz = miz::Miz::singleton(lua)?;
    let coalitions: Option<mlua::Table> = miz.raw_get("coalitions")?;
    let Some(coalitions) = coalitions else { return Ok(None) };
    let neutrals: Option<mlua::Table> = coalitions.raw_get("neutrals")?;
    let Some(neutrals) = neutrals else { return Ok(None) };
    for v in neutrals.sequence_values::<Value>() {
        let v = v?;
        if let Ok(c) = Country::from_lua(v, lua.inner()) {
            return Ok(Some(c));
        }
    }
    Ok(None)
}

fn airports(lua: MizLua) -> Result<Vec<Airport>> {
    let mut out = vec![];
    for ab in World::singleton(lua)?.get_airbases()? {
        let ab = ab?;
        if !matches!(ab.get_category(), Ok(AirbaseCategory::Airdrome)) {
            continue;
        }
        let (Ok(id), Ok(p)) = (ab.get_id(), ab.get_point()) else { continue };
        out.push(Airport {
            id,
            name: ab.get_callsign().unwrap_or_default(),
            pos: Vector2::new(p.x, p.z),
            elevation: p.y,
        });
    }
    Ok(out)
}

/// A free airliner stand at airdrome `id` for a departure to start on: an
/// open-big spot (the only kind every airliner fits), nearest the runway
/// first. `None` if there isn't one free.
fn civil_departure_spot(
    lua: MizLua,
    id: dcso3::airbase::AirbaseId,
) -> Option<dcso3::airbase::ParkingSpot> {
    use dcso3::airbase::term_type;
    let ab = World::singleton(lua)
        .ok()?
        .get_airbases()
        .ok()?
        .into_iter()
        .filter_map(|ab| ab.ok())
        .find(|ab| ab.get_id().map(|i| i == id).unwrap_or(false))?;
    let mut spots: Vec<_> = ab
        .get_parking_spots(true)
        .ok()?
        .into_iter()
        .filter(|s| s.usable_by(false) && s.term_type == term_type::OPEN_BIG)
        .collect();
    spots.sort_by(|a, b| a.dist_to_rw.total_cmp(&b.dist_to_rw));
    spots.into_iter().next()
}

/// A random point on the edge of the box `lo`-`hi`, and which edge (0-3).
fn edge_point(lo: Vector2, hi: Vector2) -> (Vector2, u8) {
    let mut rng = thread_rng();
    let edge: u8 = rng.gen_range(0..4);
    let tx = rng.gen_range(lo.x..=hi.x);
    let ty = rng.gen_range(lo.y..=hi.y);
    let p = match edge {
        0 => Vector2::new(lo.x, ty),
        1 => Vector2::new(hi.x, ty),
        2 => Vector2::new(tx, lo.y),
        _ => Vector2::new(tx, hi.y),
    };
    (p, edge)
}

fn pick_aircraft(cfg: &CivilTrafficCfg) -> Option<&CivilAircraftCfg> {
    cfg.aircraft.choose_weighted(&mut thread_rng(), |a| a.weight.max(1)).ok()
}

fn tick_civil(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let Some(cfg) = ctx
        .db
        .ephemeral
        .cfg
        .air_life
        .as_ref()
        .and_then(|a| a.civil_traffic.clone())
        .filter(|c| c.enabled)
    else {
        return;
    };
    // Maintain the flights already up.
    let names: Vec<String> = ctx.airlife.civ.keys().cloned().collect();
    for gname in names {
        let Some(f) = ctx.airlife.civ.get(&gname) else { continue };
        let group = Group::get_by_name(lua, gname.as_str()).ok();
        let Some(group) = group else {
            // Gone from DCS: shot down (handled by `civil_down`) or crashed.
            ctx.airlife.civ.remove(&gname);
            continue;
        };
        let unit = group.get_unit(1).ok();
        let pos = unit
            .as_ref()
            .and_then(|u| u.get_point().ok())
            .map(|p| Vector2::new(p.x, p.z));
        let in_air = unit.as_ref().and_then(|u| u.in_air().ok()).unwrap_or(true);
        let overdue = match f.expires {
            Some(t) => now >= t,
            None => (now - f.spawned_at).num_seconds() >= cfg.max_flight_secs as i64,
        };
        let exited = match (f.exit, pos) {
            (Some(exit), Some(p)) => dist(exit, p) <= CIV_EXIT_RADIUS_M,
            _ => false,
        };
        let rolled_out = if f.kind == CivKind::Arrival && !in_air {
            let since = *ctx
                .airlife
                .civ
                .get_mut(&gname)
                .expect("present")
                .on_ground_since
                .get_or_insert(now);
            (now - since).num_seconds() >= CIV_ROLLOUT_SECS
        } else {
            false
        };
        if overdue || exited || rolled_out {
            let f = ctx.airlife.civ.remove(&gname).expect("present");
            ctx.airlife.civ_hits.remove(&f.unit_name);
            debug!(
                "air_life: civil {} {} ({})",
                f.callsign,
                if rolled_out {
                    "landed"
                } else if exited {
                    "left the map"
                } else {
                    "timed out"
                },
                f.typ
            );
            if let Err(e) = group.destroy() {
                warn!("air_life: could not remove civil {gname}: {e:?}");
            }
        }
    }
    ctx.airlife
        .civ_hits
        .retain(|_, (_, t)| (now - *t).num_seconds() < CIV_HIT_MEMORY_SECS);

    // Spawn more.
    if (ctx.db.instanced_players().count() as u32) < cfg.min_players {
        return;
    }
    // Merchant shipping, on its own count and clock.
    let ships = ctx.airlife.civ.values().filter(|f| f.kind == CivKind::Ship).count();
    if !cfg.ships.is_empty()
        && ships < cfg.max_ships as usize
        && ctx.airlife.next_ship_spawn.map(|t| now >= t).unwrap_or(true)
    {
        let jitter = thread_rng().gen_range(0.7..1.3);
        ctx.airlife.next_ship_spawn = Some(
            now + Duration::seconds(
                (cfg.ship_spawn_interval_secs as f64 * jitter) as i64,
            ),
        );
        if let Err(e) = spawn_civil_ship(lua, ctx, &cfg, now) {
            warn!("air_life: merchant ship not spawned: {e:?}");
        }
    }
    let airliners = ctx.airlife.civ.len()
        - ctx.airlife.civ.values().filter(|f| f.kind == CivKind::Ship).count();
    if airliners >= cfg.max_active as usize {
        return;
    }
    if ctx.airlife.next_civ_spawn.map(|t| now < t).unwrap_or(false) {
        return;
    }
    let jitter = thread_rng().gen_range(0.7..1.3);
    ctx.airlife.next_civ_spawn =
        Some(now + Duration::seconds((cfg.spawn_interval_secs as f64 * jitter) as i64));
    if let Err(e) = spawn_civil(lua, ctx, &cfg, now) {
        warn!("air_life: civil flight not spawned: {e:?}");
    }
}

const SHIP_NAMES: &[&str] = &[
    "Nordic Star",
    "Baltic Trader",
    "Sea Pearl",
    "Black Sea Carrier",
    "Caspian Spirit",
    "Gulf Horizon",
    "Anatolia",
    "Odessa Star",
    "Bosporus",
    "Aegean Wind",
    "Levant Pride",
    "Hormuz Venture",
    "Kharg Trader",
    "Persian Dawn",
    "Crimea Bay",
    "Danube Queen",
];

/// A sea lane: two open-water points at least 50 km apart with open water
/// every 2 km between them, so a ship sailing straight never runs aground.
fn sea_lane(lua: MizLua, lo: Vector2, hi: Vector2) -> Option<(Vector2, Vector2)> {
    use dcso3::land::SurfaceType;
    let land = Land::singleton(lua).ok()?;
    let water =
        |p: Vector2| matches!(land.get_surface_type(LuaVec2(p)), Ok(SurfaceType::Water));
    let mut rng = thread_rng();
    let points: Vec<Vector2> = (0..60)
        .map(|_| Vector2::new(rng.gen_range(lo.x..=hi.x), rng.gen_range(lo.y..=hi.y)))
        .filter(|p| water(*p))
        .collect();
    for _ in 0..40 {
        let (Some(a), Some(b)) = (points.choose(&mut rng), points.choose(&mut rng))
        else {
            return None;
        };
        let d = dist(*a, *b);
        if d < 50_000. {
            continue;
        }
        let steps = (d / 2_000.).ceil() as usize;
        if (1..steps).all(|i| water(*a + (*b - *a) * (i as f64 / steps as f64))) {
            return Some((*a, *b));
        }
    }
    None
}

fn spawn_civil_ship(
    lua: MizLua,
    ctx: &mut Context,
    cfg: &CivilTrafficCfg,
    now: DateTime<Utc>,
) -> Result<()> {
    ensure_civ_country(lua, ctx, cfg);
    let Some(country) = ctx.airlife.civ_country else { return Ok(()) };
    let mut lo = Vector2::new(f64::INFINITY, f64::INFINITY);
    let mut hi = Vector2::new(f64::NEG_INFINITY, f64::NEG_INFINITY);
    for p in ctx.db.objectives().map(|(_, o)| o.pos()) {
        lo = Vector2::new(lo.x.min(p.x), lo.y.min(p.y));
        hi = Vector2::new(hi.x.max(p.x), hi.y.max(p.y));
    }
    if !lo.x.is_finite() {
        bail!("no objectives")
    }
    let pad = Vector2::new(cfg.map_margin_m + 60_000., cfg.map_margin_m + 60_000.);
    let Some((a, b)) = sea_lane(lua, lo - pad, hi + pad) else {
        debug!("air_life: no open-water lane found this time");
        return Ok(());
    };
    let mut rng = thread_rng();
    let ship = cfg
        .ships
        .choose_weighted(&mut rng, |s| s.weight.max(1))
        .map_err(|e| anyhow!("no ship type: {e}"))?;
    let callsign = format_compact!(
        "MV {} {}",
        SHIP_NAMES.choose(&mut rng).copied().unwrap_or("Trader"),
        rng.gen_range(1..99)
    );
    let gname = String::from(format_compact!("{CIV_PREFIX}{callsign}"));
    let uname = String::from(format_compact!("{CIV_PREFIX}{callsign}-1"));
    if ctx.airlife.civ.contains_key(&gname) {
        return Ok(());
    }
    let wp = |p: Vector2| MissionPoint {
        typ: PointType::TurningPoint,
        airdrome_id: None,
        time_re_fu_ar: None,
        helipad: None,
        link_unit: None,
        action: None,
        pos: LuaVec2(p),
        alt: 0.,
        alt_typ: None,
        speed: ship.speed_ms,
        speed_locked: None,
        eta: None,
        eta_locked: None,
        name: None,
        task: Box::new(Task::ComboTask(vec![])),
    };
    let l = lua.inner();
    let pts = l.create_table()?;
    pts.raw_set(1, wp(a))?;
    pts.raw_set(2, wp(b))?;
    let route = l.create_table()?;
    route.raw_set("points", pts)?;
    let unit = l.create_table()?;
    unit.raw_set("name", uname.as_str())?;
    unit.raw_set("type", ship.typ.as_str())?;
    unit.raw_set("x", a.x)?;
    unit.raw_set("y", a.y)?;
    unit.raw_set("heading", heading_of(b - a))?;
    unit.raw_set("skill", "Average")?;
    let units = l.create_table()?;
    units.raw_set(1, unit)?;
    let group = l.create_table()?;
    group.raw_set("name", gname.as_str())?;
    group.raw_set("route", route)?;
    group.raw_set("units", units)?;
    let group = miz::Group::from_lua(Value::Table(group), l)?;
    Coalition::singleton(lua)?
        .add_group(country, GroupCategory::Ship, group)
        .with_context(|| {
            format_compact!("spawning merchant {callsign} ({})", ship.typ)
        })?;
    let voyage = dist(a, b) / ship.speed_ms.max(1.) * 1.3 + 600.;
    info!(
        "air_life: merchant {callsign} ({}) sailing {:.0} km",
        ship.typ,
        dist(a, b) / 1000.
    );
    ctx.airlife.civ.insert(
        gname,
        CivFlight {
            unit_name: uname,
            callsign,
            typ: String::from(ship.typ.as_str()),
            kind: CivKind::Ship,
            spawned_at: now,
            exit: Some(b),
            expires: Some(now + Duration::seconds(voyage as i64)),
            on_ground_since: None,
        },
    );
    Ok(())
}

/// Work out, once per mission, the neutral country civil traffic flies for.
fn ensure_civ_country(lua: MizLua, ctx: &mut Context, cfg: &CivilTrafficCfg) {
    if ctx.airlife.civ_country_resolved {
        return;
    }
    ctx.airlife.civ_country_resolved = true;
    ctx.airlife.civ_country = match cfg.country {
        Some(c) => Some(c),
        None => mission_neutral_country(lua).unwrap_or_else(|e| {
            warn!("air_life: reading the mission's neutral countries: {e:?}");
            None
        }),
    };
    match ctx.airlife.civ_country {
        Some(c) => info!("air_life: civil traffic flies for {c:?}"),
        None => warn!(
            "air_life: the mission has no neutral country and civil_traffic.country is not \
             set -- civil traffic is off"
        ),
    }
}

fn spawn_civil(
    lua: MizLua,
    ctx: &mut Context,
    cfg: &CivilTrafficCfg,
    now: DateTime<Utc>,
) -> Result<()> {
    ensure_civ_country(lua, ctx, cfg);
    let Some(country) = ctx.airlife.civ_country else { return Ok(()) };
    let fields = airports(lua)?;
    if fields.is_empty() {
        bail!("no airdromes on this map")
    }
    // The box everything happens in, padded out to where airliners enter.
    let mut lo = Vector2::new(f64::INFINITY, f64::INFINITY);
    let mut hi = Vector2::new(f64::NEG_INFINITY, f64::NEG_INFINITY);
    for p in fields.iter().map(|a| a.pos).chain(ctx.db.objectives().map(|(_, o)| o.pos()))
    {
        lo = Vector2::new(lo.x.min(p.x), lo.y.min(p.y));
        hi = Vector2::new(hi.x.max(p.x), hi.y.max(p.y));
    }
    let pad = Vector2::new(cfg.map_margin_m, cfg.map_margin_m);
    let (lo, hi) = (lo - pad, hi + pad);
    let span = (hi - lo).x.min((hi - lo).y);
    let front = front_points(ctx);
    let rear: Vec<&Airport> = fields
        .iter()
        .filter(|a| front_dist(&front, a.pos) >= cfg.rear_airport_min_front_m)
        .collect();

    let mut rng = thread_rng();
    let mut kinds = vec![(CivKind::Overflight, cfg.overflight_weight)];
    if !rear.is_empty() {
        kinds.push((CivKind::Departure, cfg.departure_weight));
        kinds.push((CivKind::Arrival, cfg.arrival_weight));
    }
    let kind = kinds
        .choose_weighted(&mut rng, |k| k.1)
        .map(|k| k.0)
        .map_err(|e| anyhow!("no flight kind to pick: {e}"))?;
    let ac = pick_aircraft(cfg).ok_or_else(|| anyhow!("no civil aircraft"))?;
    let cruise = rng.gen_range(ac.cruise_alt_min_m..=ac.cruise_alt_max_m);
    let speed = ac.cruise_speed_ms;
    let code = cfg.airline_codes.choose(&mut rng).cloned().unwrap_or_default();
    let callsign = format_compact!("{code}{}", rng.gen_range(100..9999));
    let gname = String::from(format_compact!("{CIV_PREFIX}{callsign}"));
    let uname = String::from(format_compact!("{CIV_PREFIX}{callsign}-1"));
    if ctx.airlife.civ.contains_key(&gname) {
        return Ok(()); // flight number collision, try again next time
    }

    // An exit on the map edge, the one of a few candidates that keeps the
    // leg from `from` furthest from the fighting.
    let best_edge = |from: Vector2, min_len: f64| -> Vector2 {
        let mut best: Option<(Vector2, f64)> = None;
        for _ in 0..12 {
            let (p, _) = edge_point(lo, hi);
            if dist(p, from) < min_len {
                continue;
            }
            let score = segment_front_dist(&front, from, p);
            if score >= cfg.front_standoff_m {
                return p;
            }
            if best.map(|(_, s)| score > s).unwrap_or(true) {
                best = Some((p, score));
            }
        }
        best.map(|(p, _)| p).unwrap_or_else(|| edge_point(lo, hi).0)
    };

    let base_task = || {
        Task::ComboTask(vec![
            Task::WrappedCommand(Command::SetUnlimitedFuel(true)),
            Task::WrappedOption(AiOption::Air(AirOption::Roe(AirRoe::WeaponHold))),
            Task::WrappedOption(AiOption::Air(AirOption::ReactionOnThreat(
                AirReactionToThreat::NoReaction,
            ))),
        ])
    };
    // The stand a departure starts on (see below).
    let mut parking: Option<i64> = None;
    let (start, start_alt, points, exit, desc) = match kind {
        CivKind::Ship => bail!("ships are spawned by spawn_civil_ship"),
        CivKind::Overflight => {
            let mut chosen: Option<(Vector2, Vector2, f64)> = None;
            for _ in 0..12 {
                let (a, ea) = edge_point(lo, hi);
                let (b, eb) = edge_point(lo, hi);
                if ea == eb || dist(a, b) < span * 0.6 {
                    continue;
                }
                let score = segment_front_dist(&front, a, b);
                if score >= cfg.front_standoff_m {
                    chosen = Some((a, b, score));
                    break;
                }
                if chosen.map(|(_, _, s)| score > s).unwrap_or(true) {
                    chosen = Some((a, b, score));
                }
            }
            let (a, b, _) = chosen.ok_or_else(|| anyhow!("no overflight route"))?;
            let pts = vec![
                air_point(a, cruise, speed, base_task()),
                air_point(b, cruise, speed, Task::ComboTask(vec![])),
            ];
            (a, cruise, pts, Some(b), CompactString::from("overflight"))
        }
        CivKind::Departure => {
            let dep = rear.choose(&mut rng).ok_or_else(|| anyhow!("no rear airport"))?;
            let exit = best_edge(dep.pos, 80_000.);
            let dir = (exit - dep.pos).normalize();
            // A departure leaves from the airport's ramp like any other
            // aircraft, engines running. It used to appear 5 km out and
            // already climbing. No free airliner stand, no departure.
            let Some(spot) = civil_departure_spot(lua, dep.id) else {
                debug!(
                    "air_life: no free stand at {} for a departure this time",
                    dep.name
                );
                return Ok(());
            };
            parking = Some(spot.term_index);
            let start = Vector2::new(spot.pos.0.x, spot.pos.0.z);
            let start_alt = spot.pos.0.y;
            let pts = vec![
                MissionPoint {
                    typ: PointType::TakeOffParkingHot,
                    airdrome_id: Some(dep.id),
                    time_re_fu_ar: None,
                    helipad: None,
                    link_unit: None,
                    action: Some(ActionTyp::Air(TurnMethod::FromParkingAreaHot)),
                    pos: LuaVec2(start),
                    alt: dep.elevation,
                    alt_typ: Some(AltType::BARO),
                    speed: 0.,
                    speed_locked: Some(true),
                    eta: Some(dcso3::Time(0.)),
                    eta_locked: Some(true),
                    name: None,
                    task: Box::new(base_task()),
                },
                air_point(
                    dep.pos + dir * 40_000.,
                    cruise,
                    speed,
                    Task::ComboTask(vec![]),
                ),
                air_point(exit, cruise, speed, Task::ComboTask(vec![])),
            ];
            (start, start_alt, pts, Some(exit), format_compact!("departing {}", dep.name))
        }
        CivKind::Arrival => {
            let arr = rear.choose(&mut rng).ok_or_else(|| anyhow!("no rear airport"))?;
            let entry = best_edge(arr.pos, 80_000.);
            let dir = (arr.pos - entry).normalize();
            let pts = vec![
                air_point(entry, cruise, speed, base_task()),
                air_point(
                    arr.pos - dir * 35_000.,
                    arr.elevation + 1_500.,
                    speed * 0.6,
                    Task::ComboTask(vec![]),
                ),
                MissionPoint {
                    typ: PointType::Land,
                    airdrome_id: Some(arr.id),
                    time_re_fu_ar: None,
                    helipad: None,
                    link_unit: None,
                    action: Some(ActionTyp::Air(TurnMethod::Landing)),
                    pos: LuaVec2(arr.pos),
                    alt: arr.elevation,
                    alt_typ: Some(AltType::BARO),
                    speed: 70.,
                    speed_locked: None,
                    eta: None,
                    eta_locked: None,
                    name: None,
                    task: Box::new(Task::ComboTask(vec![])),
                },
            ];
            (entry, cruise, pts, None, format_compact!("inbound to {}", arr.name))
        }
    };
    let heading = {
        let next = points.get(1).map(|p| p.pos.0).unwrap_or(start);
        heading_of(next - start)
    };

    // The group table, built by hand: there is no template to clone.
    let l = lua.inner();
    let route = l.create_table()?;
    let pts = l.create_table()?;
    for (i, p) in points.into_iter().enumerate() {
        pts.raw_set(i + 1, p)?;
    }
    route.raw_set("points", pts)?;
    let unit = l.create_table()?;
    unit.raw_set("name", uname.as_str())?;
    unit.raw_set("type", ac.typ.as_str())?;
    unit.raw_set("x", start.x)?;
    unit.raw_set("y", start.y)?;
    unit.raw_set("alt", start_alt)?;
    unit.raw_set("alt_type", "BARO")?;
    match parking {
        // On its stand, stopped: DCS only honours the parking start if the
        // unit is really there and `parking` names the spot.
        Some(term_index) => {
            unit.raw_set("speed", 0.)?;
            unit.raw_set("parking", term_index)?;
        }
        None => unit.raw_set("speed", speed)?,
    }
    unit.raw_set("heading", heading)?;
    unit.raw_set("skill", "Average")?;
    if let Some(livery) = ac.liveries.choose(&mut rng) {
        unit.raw_set("livery_id", livery.as_str())?;
    }
    let payload = l.create_table()?;
    // Unlimited fuel is set on the first waypoint; this only has to be a
    // load every airliner can carry.
    payload.raw_set("fuel", 2_000.)?;
    payload.raw_set("flare", 0)?;
    payload.raw_set("chaff", 0)?;
    payload.raw_set("gun", 0)?;
    payload.raw_set("pylons", l.create_table()?)?;
    unit.raw_set("payload", payload)?;
    let units = l.create_table()?;
    units.raw_set(1, unit)?;
    let group = l.create_table()?;
    group.raw_set("name", gname.as_str())?;
    group.raw_set("task", "Transport")?;
    group.raw_set("uncontrolled", false)?;
    if parking.is_some() {
        // A parking start's group position has to be the unit's stand too,
        // or DCS throws the ground start away and spawns it airborne.
        group.raw_set("x", start.x)?;
        group.raw_set("y", start.y)?;
        group.raw_set("start_time", 0)?;
    }
    group.raw_set("route", route)?;
    group.raw_set("units", units)?;
    let group = miz::Group::from_lua(Value::Table(group), l)?;
    Coalition::singleton(lua)?
        .add_group(country, GroupCategory::Airplane, group)
        .with_context(|| format_compact!("spawning civil {callsign} ({})", ac.typ))?;
    info!("air_life: civil {callsign} ({}) {desc} at FL{:.0}", ac.typ, cruise / 30.48);
    ctx.airlife.civ.insert(
        gname,
        CivFlight {
            unit_name: uname,
            callsign,
            typ: String::from(ac.typ.as_str()),
            kind,
            spawned_at: now,
            exit,
            expires: None,
            on_ground_since: None,
        },
    );
    Ok(())
}

/// A player's weapon hit civilian unit `unit_name`.
pub(crate) fn civil_hit(
    ctx: &mut Context,
    unit_name: &str,
    shooter: Option<Ucid>,
    now: DateTime<Utc>,
) {
    if let Some(ucid) = shooter {
        ctx.airlife.civ_hits.insert(String::from(unit_name), (ucid, now));
    }
}

/// Civilian unit `unit_name` is dead. If a player hit it recently, that
/// player and their side pay for it, and everyone hears about it.
pub(crate) fn civil_down(ctx: &mut Context, unit_name: &str, now: DateTime<Utc>) {
    let flight = ctx
        .airlife
        .civ
        .iter()
        .find(|(_, f)| f.unit_name.as_str() == unit_name)
        .map(|(g, _)| g.clone())
        .and_then(|g| ctx.airlife.civ.remove(&g));
    let hit = ctx
        .airlife
        .civ_hits
        .remove(unit_name)
        .filter(|(_, t)| (now - *t).num_seconds() < CIV_HIT_MEMORY_SECS);
    let ship = flight.as_ref().map(|f| f.kind == CivKind::Ship).unwrap_or(false);
    let (callsign, typ) = match &flight {
        Some(f) => (f.callsign.clone(), f.typ.clone()),
        None => (
            CompactString::from(
                unit_name.trim_start_matches(CIV_PREFIX).trim_end_matches("-1"),
            ),
            String::from("airliner"),
        ),
    };
    let Some((ucid, _)) = hit else {
        if flight.is_some() {
            info!("air_life: civil {callsign} ({typ}) lost with no player hit on it");
        }
        return;
    };
    let cfg = ctx
        .db
        .ephemeral
        .cfg
        .air_life
        .as_ref()
        .and_then(|a| a.civil_traffic.clone())
        .unwrap_or_default();
    let Some((name, side)) = ctx.db.player(&ucid).map(|p| (p.name.clone(), p.side))
    else {
        return;
    };
    warn!("air_life: civil {callsign} ({typ}) shot down by {name} ({ucid}, {side:?})");
    if cfg.shootdown_penalty_points != 0 {
        ctx.db.adjust_points(
            &ucid,
            -cfg.shootdown_penalty_points,
            &format!("for shooting down civilian flight {callsign}"),
        );
    }
    if cfg.shootdown_treasury_penalty != 0 {
        ctx.db.persisted.adjust_treasury(side, -cfg.shootdown_treasury_penalty);
        ctx.db.ephemeral.dirty();
    }
    let msg = if cfg.shootdown_treasury_penalty != 0 {
        format_compact!(
            "{}: {callsign} ({typ}) was {} by {name} ({side:?}). {side:?} pays {} from its treasury.",
            if ship { "CIVILIAN SHIP SUNK" } else { "CIVILIAN AIRLINER DOWN" },
            if ship { "sunk" } else { "shot down" },
            cfg.shootdown_treasury_penalty
        )
    } else {
        format_compact!(
            "{}: {callsign} ({typ}) was {} by {name} ({side:?}).",
            if ship { "CIVILIAN SHIP SUNK" } else { "CIVILIAN AIRLINER DOWN" },
            if ship { "sunk" } else { "shot down" }
        )
    };
    ctx.db.ephemeral.msgs().panel_to_all(20, true, msg);
}

// ---------------------------------------------------------------------------
// Tick
// ---------------------------------------------------------------------------

/// The slow-tick half: wingman upkeep, packages, civil traffic, RTB sweep.
pub(crate) fn tick(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    now: DateTime<Utc>,
) {
    if ctx.db.ephemeral.cfg.air_life.is_none()
        && ctx.airlife.wingmen.is_empty()
        && ctx.airlife.packages.is_empty()
        && ctx.airlife.civ.is_empty()
        && ctx.airlife.rtb.is_empty()
    {
        return;
    }
    tick_wingmen(lua, ctx, now);
    tick_packages(lua, ctx, perf, now);
    tick_civil(lua, ctx, now);
    flush_rtb(lua, ctx, now);
}
