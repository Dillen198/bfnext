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

mod admin;
mod atis;
mod bg;
mod chatcmd;
mod commander;
mod db;
mod ewr;
mod frontline;
mod jtac;
mod landcache;
mod menu;
mod msgq;
mod shots;
mod spawnctx;

extern crate nalgebra as na;
use crate::db::{events::{EventEffect, EventScheduler}, player::SlotAuth};
use admin::{run_admin_commands, AdminCommand, AdminResult};
use anyhow::{anyhow, bail, Context as AnyhowContext, Result};
use bfprotocols::{
    cfg::{Cfg, LifeType, UnitTag, Vehicle},
    db::{group::GroupId, objective::ObjectiveId},
    perf::{Perf, PerfInner},
    stats::Stat,
};
use bg::Task;
use chatcmd::{run_action_commands, run_jtac_commands};
use chrono::{prelude::*, Duration};
use compact_str::{format_compact, CompactString};
use crossbeam::queue::SegQueue;
use db::{
    group::BirthRes,
    player::{RegErr, TakeoffRes},
    Db,
};
use dcso3::{
    coalition::Side,
    coord::Coord,
    env::{
        self,
        miz::{Miz, UnitId},
        Env,
    },
    event::Event,
    hooks::UserHooks,
    lfs::Lfs,
    net::{DcsLuaEnvironment, Net, PlayerId, SlotId, Ucid},
    object::{DcsObject, DcsOid},
    perf::record_perf,
    timer::Timer,
    trigger::Trigger,
    unit::{ClassUnit, Unit},
    world::{HandlerId, MarkPanel, World},
    HooksLua, LuaEnv, LuaVec3, MizLua, String, Vector3,
};
use ewr::Ewr;
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use indexmap::IndexSet;
use jtac::{JtId, Jtacs};
use landcache::LandCache;
use log::{debug, error, info, warn};
use mlua::prelude::*;
use msgq::MsgTyp;
use netidx::publisher::Value;
use shots::ShotDb;
use smallvec::{smallvec, SmallVec};
use spawnctx::SpawnCtx;
use std::{
    backtrace::Backtrace,
    panic::{catch_unwind, AssertUnwindSafe},
    path::PathBuf,
    sync::Arc,
};
use tokio::sync::{mpsc::UnboundedSender, oneshot};

#[derive(Debug, Clone)]
struct PlayerInfo {
    name: String,
    addr: Option<String>,
    ucid: Ucid,
}

#[derive(Debug, Default)]
struct Connected {
    info_by_player_id: FxHashMap<PlayerId, PlayerInfo>,
    id_by_ucid: FxHashMap<Ucid, PlayerId>,
    id_by_name: FxHashMap<String, PlayerId>,
    id_by_addr: FxHashMap<Option<String>, PlayerId>,
}

impl Connected {
    pub fn len(&self) -> usize {
        self.info_by_player_id.len()
    }

    pub fn get(&self, id: &PlayerId) -> Option<&PlayerInfo> {
        self.info_by_player_id.get(id)
    }

    pub fn get_by_name(&self, name: &str) -> Option<&PlayerInfo> {
        self.id_by_name.get(name).and_then(|id| self.info_by_player_id.get(id))
    }

    fn get_or_lookup_player_info<'a, 'lua, L: LuaEnv<'lua>>(
        &'a mut self,
        lua: L,
        id: PlayerId,
    ) -> Result<&'a PlayerInfo> {
        if self.info_by_player_id.contains_key(&id) {
            Ok(&self.info_by_player_id[&id])
        } else {
            let net = Net::singleton(lua)?;
            let ifo = net.get_player_info(id)?;
            let ucid =
                ifo.ucid()?.ok_or_else(|| anyhow!("player {:?} has no ucid", ifo))?;
            let name = ifo.name()?;
            let addr = ifo.ip()?;
            info!("player name: '{}', id: {:?}, ucid: {:?}", name, id, ucid);
            self.player_connected(id, PlayerInfo { name, addr, ucid })?;
            Ok(&self.info_by_player_id[&id])
        }
    }

    pub fn player_connected(&mut self, id: PlayerId, ifo: PlayerInfo) -> Result<()> {
        if let Some(id) = self.id_by_ucid.remove(&ifo.ucid) {
            self.player_disconnected(id);
        }
        if self.id_by_name.contains_key(&ifo.name) {
            bail!("your callsign is already taken by another player")
        }
        if self.id_by_addr.contains_key(&ifo.addr) {
            bail!("another player is already connected from your ip address")
        }
        self.id_by_ucid.insert(ifo.ucid, id);
        self.id_by_name.insert(ifo.name.clone(), id);
        self.id_by_addr.insert(ifo.addr.clone(), id);
        self.info_by_player_id.insert(id, ifo);
        Ok(())
    }

    pub fn player_disconnected(&mut self, id: PlayerId) -> Option<PlayerInfo> {
        self.info_by_player_id.remove(&id).map(|ifo| {
            self.id_by_name.remove(&ifo.name);
            self.id_by_ucid.remove(&ifo.ucid);
            self.id_by_addr.remove(&ifo.addr);
            ifo
        })
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct AutoShutdown {
    when: DateTime<Utc>,
    thirty_minute_warning: bool,
    ten_minute_warning: bool,
    five_minute_warning: bool,
    one_minute_warning: bool,
}

impl AutoShutdown {
    fn new(ts: DateTime<Utc>) -> Self {
        let mut t = Self::default();
        t.when = ts;
        t
    }
}

#[derive(Debug, Clone, Copy)]
enum LoadState {
    Init,
    MissionLoaded { time: DateTime<Utc> },
    Running,
}

impl Default for LoadState {
    fn default() -> Self {
        Self::Init
    }
}

impl LoadState {
    fn login_ok(&self) -> Option<String> {
        match self {
            Self::Running => None,
            Self::Init => {
                Some(String::from("The server is not finished loading the mission"))
            }
            Self::MissionLoaded { time } => {
                let remains = (Duration::seconds(62) - (Utc::now() - time)).num_seconds();
                Some(format_compact!("The server is initializing ETA {remains}s").into())
            }
        }
    }

    fn init_ok(&self) -> bool {
        match self {
            Self::Init => false,
            Self::MissionLoaded { time } => Utc::now() - *time > Duration::seconds(1),
            Self::Running => true,
        }
    }

    fn step(&mut self) {
        match self {
            Self::Running | Self::Init => (),
            Self::MissionLoaded { time } => {
                if Utc::now() - *time >= Duration::minutes(1) {
                    *self = Self::Running;
                }
            }
        }
    }
}

#[derive(Debug, Default)]
struct JtacSlotIfo {
    subscribed_objectives: FxHashSet<ObjectiveId>,
    pinned: FxHashSet<JtId>,
}

#[derive(Debug, Default)]
struct Context {
    sortie: String,
    event_handler_id: Option<HandlerId>,
    miz_state_path: PathBuf,
    shutdown: Option<AutoShutdown>,
    last_perf_log: DateTime<Utc>,
    load_state: LoadState,
    idx: env::miz::MizIndex,
    db: Db,
    external_admin_commands: Arc<SegQueue<(AdminCommand, oneshot::Sender<Value>)>>,
    admin_commands: Vec<(admin::Caller, AdminCommand)>,
    action_commands: Vec<(PlayerId, String)>,
    jtac_commands: Vec<(PlayerId, JtId, String)>,
    to_background: Option<UnboundedSender<bg::Task>>,
    recently_landed: FxHashMap<DcsOid<ClassUnit>, DateTime<Utc>>,
    recently_born: FxHashMap<DcsOid<ClassUnit>, DateTime<Utc>>,
    airborne: FxHashSet<DcsOid<ClassUnit>>,
    captureable: FxHashMap<ObjectiveId, usize>,
    shots_out: ShotDb,
    menu_init_queue: IndexSet<SlotId, FxBuildHasher>,
    last_frame: Option<DateTime<Utc>>,
    last_slow_timed_events: DateTime<Utc>,
    last_periodic_points: DateTime<Utc>,
    last_commander_tick: DateTime<Utc>,
    last_unit_position: usize,
    last_player_position: usize,
    subscribed_jtac_menus: FxHashMap<SlotId, JtacSlotIfo>,
    subscribed_action_menus: FxHashSet<SlotId>,
    connected: Connected,
    landcache: LandCache,
    ewr: Ewr,
    jtac: Jtacs,
    frontline: Option<frontline::FrontLine>,
    last_frontline_update: DateTime<Utc>,
    event_scheduler: EventScheduler,
    last_junk_removal: DateTime<Utc>,
    last_weather_publish: DateTime<Utc>,
}

impl Context {
    // this must be used cautiously. Reasons why it's not totally nuts,
    // - the dcs scripting api is single threaded
    // - the event handlers can be triggerred by api calls, making refcells and mutexes error prone
    // - as long as an event handler doesn't step on state in an api call it's ok, since concurrency never happens
    //   that isn't so hard to guarantee
    unsafe fn get_mut() -> &'static mut Self {
        static mut SELF: Option<Context> = None;
        #[allow(static_mut_refs)]
        let t = unsafe { SELF.as_mut() };
        match t {
            Some(ctx) => ctx,
            None => {
                unsafe { SELF = Some(Context::default()) };
                #[allow(static_mut_refs)]
                unsafe {
                    SELF.as_mut().unwrap()
                }
            }
        }
    }

    unsafe fn _get() -> &'static Context {
        unsafe { Context::get_mut() }
    }

    unsafe fn reset() {
        unsafe {
            *Self::get_mut() = Self::default();
        }
    }

    fn do_bg_task(&self, task: bg::Task) {
        if let Some(to_bg) = &self.to_background {
            match to_bg.send(task) {
                Ok(()) => (),
                Err(e) => log::error!("background thread is dead, task dropped: {e}"),
            }
        }
    }

    fn init_async_bg(&mut self, lua: &Lua) -> Result<()> {
        if self.to_background.is_none() {
            let write_dir = PathBuf::from(Lfs::singleton(lua)?.writedir()?.as_str());
            self.to_background = Some(bg::init(write_dir));
        }
        Ok(())
    }

    fn respawn_groups(&mut self, lua: MizLua, miz: &Miz) -> Result<()> {
        let spctx = SpawnCtx::new(lua)?;
        let perf = Arc::make_mut(&mut unsafe { Perf::get_mut() }.inner);
        self.db.respawn_after_load(lua, perf, &self.idx, miz, &mut self.landcache, &spctx)
    }

    fn log_perf(&mut self, now: DateTime<Utc>) {
        if now - self.last_perf_log > Duration::seconds(60) {
            self.last_perf_log = now;
            self.do_bg_task(bg::Task::LogPerf {
                players: self.connected.len(),
                perf: unsafe { Perf::get_mut() }.clone(),
                api_perf: unsafe { dcso3::perf::Perf::get_mut() }.clone(),
            });
            info!("landcache {}", self.landcache.stats())
        }
    }
}

fn on_player_try_connect(
    _: HooksLua,
    addr: String,
    name: String,
    ucid: Ucid,
    id: PlayerId,
) -> Result<Option<String>> {
    let ts = Utc::now();
    info!(
        "onPlayerTryConnect addr: {:?}, name: {:?}, ucid: {:?}, id: {:?}",
        addr, name, ucid, id
    );
    let ctx = unsafe { Context::get_mut() };
    if let Some(msg) = ctx.load_state.login_ok() {
        return Ok(Some(msg));
    }
    if let Some(filter) = &ctx.db.ephemeral.cfg.name_filter {
        if !filter.check(&name) {
            let msg = format_compact!("name must match {}", filter.as_str());
            return Ok(Some(msg.into()));
        }
    }
    if let Some((until, _)) = ctx.db.ephemeral.cfg.banned.get(&ucid) {
        match until {
            None => return Ok(Some("you are banned forever".into())),
            Some(until) if until >= &Utc::now() => {
                return Ok(Some(
                    format_compact!("you are banned until {}", until).into(),
                ));
            }
            Some(_) => {
                let path = ctx.miz_state_path.clone();
                {
                    let cfg = Arc::make_mut(&mut ctx.db.ephemeral.cfg);
                    cfg.banned.remove(&ucid);
                }
                let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
                ctx.do_bg_task(bg::Task::SaveConfig(path, cfg))
            }
        }
    }
    if let Err(e) = ctx.connected.player_connected(
        id,
        PlayerInfo { name: name.clone(), addr: Some(addr.clone()), ucid },
    ) {
        return Ok(Some(String::from(format_compact!("{e}"))));
    }
    ctx.db.player_connected(ucid, name.clone());
    ctx.do_bg_task(Task::Stat(Stat::Connect { id: ucid, addr, name }));
    record_perf(&mut Arc::make_mut(&mut unsafe { Perf::get_mut() }.inner).dcs_hooks, ts);
    Ok(None)
}

fn on_player_try_send_chat(
    lua: HooksLua,
    id: PlayerId,
    msg: String,
    all: bool,
) -> Result<Option<String>> {
    let start_ts = Utc::now();
    let ctx = unsafe { Context::get_mut() };
    let perf = &mut Arc::make_mut(&mut unsafe { Perf::get_mut() }.inner).dcs_hooks;
    info!("onPlayerTrySendChat id: {:?}, msg: {:?}, all: {:?}", id, msg, all);
    let r = chatcmd::process(ctx, lua, start_ts, id, msg);
    record_perf(perf, start_ts);
    match r {
        Ok(_) => Ok(None),
        Err(e) => {
            ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), format_compact!("{e}"));
            Ok(Some("".into()))
        }
    }
}

fn process_slot_rejection(ctx: &mut Context, id: PlayerId, ucid: Ucid, rej: SlotAuth) {
    match rej {
        SlotAuth::Denied => {
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!("access to slot is denied"),
            );
        }
        SlotAuth::NoPoints { vehicle, cost, balance } => {
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!("{vehicle} costs {cost}, you have {balance}"),
            );
        }
        SlotAuth::NoLives(typ) => {
            let msg = match lives(&mut ctx.db, &ucid, Some(typ)) {
                Ok(s) => s,
                Err(e) => {
                    error!("failed to get lives for {} {:?}", ucid, e);
                    "".into()
                }
            };
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!("you have no {:?} lives remaining. {}", typ, msg),
            );
        }
        SlotAuth::VehicleNotAvailable(vehicle) => {
            let msg =
                format_compact!("Objective does not have any {} in stock", vehicle.0);
            ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg);
        }
        SlotAuth::ObjectiveHasNoLogistics => {
            let msg = format_compact!("Objective is capturable");
            ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg);
        }
        SlotAuth::ObjectiveNotOwned(side) => {
            let msg = String::from(format_compact!(
                "{:?} does not own the objective associated with this slot",
                side
            ));
            ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg);
        }
        SlotAuth::NotRegistered(_) => warn!("unexpected NotRegistered"),
        SlotAuth::Yes(_) => warn!("slot was not rejected!"),
        SlotAuth::EraRestricted { vehicle, era } => {
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!("{vehicle} is not available in the current era ({era})"),
            );
        }
    }
}

fn try_occupy_slot(
    ctx: &mut Context,
    lua: HooksLua,
    id: PlayerId,
    ifo: PlayerInfo,
    side: Side,
    slot: SlotId,
) -> Result<bool> {
    let now = Utc::now();
    match ctx.db.try_occupy_slot(now, side, slot, &ifo.ucid) {
        SlotAuth::NotRegistered(side) => {
            let name = ifo.name.clone();
            match ctx.db.register_player(ifo.ucid, name.clone(), side) {
                Ok(()) => {
                    chatcmd::register_success(ctx, id, name, side);
                    try_occupy_slot(ctx, lua, id, ifo, side, slot)
                }
                Err(RegErr::AlreadyRegistered(_, _)) => {
                    warn!(
                        "{:?} try_occupy_slot says NotRegistered but register_player says AlreadyRegistered",
                        ifo.ucid
                    );
                    Ok(false)
                }
                Err(RegErr::AlreadyOn(_)) => {
                    warn!(
                        "{:?} try_occupy_slot says NotRegistered but register_player says AlreadyOn",
                        ifo.ucid
                    );
                    Ok(false)
                }
            }
        }
        SlotAuth::Yes(typ) => {
            ctx.db.ephemeral.cancel_force_to_spectators(&ifo.ucid);
            ctx.subscribed_jtac_menus.remove(&slot);
            ctx.do_bg_task(Task::Stat(Stat::Slot { id: ifo.ucid, slot, typ }));
            Ok(true)
        }
        rej => {
            process_slot_rejection(ctx, id, ifo.ucid, rej);
            Ok(false)
        }
    }
}

fn on_player_try_change_slot(
    lua: HooksLua,
    id: PlayerId,
    side: Side,
    slot: SlotId,
) -> Result<Option<bool>> {
    info!("onPlayerTryChangeSlot: {:?} {:?} {:?}", id, side, slot);
    let start_ts = Utc::now();
    let ctx = unsafe { Context::get_mut() };
    let res = match ctx.connected.get_or_lookup_player_info(lua, id) {
        Err(e) => {
            error!("failed to get player info for {:?} {:?}", id, e);
            Ok(Some(false))
        }
        Ok(ifo) => {
            let ifo = ifo.clone();
            match try_occupy_slot(ctx, lua, id, ifo, side, slot.clone()) {
                Err(e) => {
                    error!("error checking slot {:?}", e);
                    Ok(Some(false))
                }
                Ok(false) => Ok(Some(false)),
                Ok(true) => Ok(None),
            }
        }
    };
    record_perf(
        &mut Arc::make_mut(&mut unsafe { Perf::get_mut() }.inner).dcs_hooks,
        start_ts,
    );
    res
}

struct CsarPilotInfo {
    ucid: Ucid,
    name: dcso3::String,
    side: Side,
    life_type: LifeType,
    pos: dcso3::Vector2,
}

fn try_capture_csar_info(
    _lua: MizLua,
    ctx: &Context,
    unit: &Unit,
) -> Option<CsarPilotInfo> {
    let csar = ctx.db.ephemeral.cfg.csar.as_ref()?;
    if !csar.enabled {
        return None;
    }
    let slot = unit.slot().ok()?;
    let ucid = ctx.db.ephemeral.player_in_slot(&slot).cloned()?;
    let player = ctx.db.player(&ucid)?;
    let life_type = player.airborne?;
    let name = player.name.clone();
    let side = player.side;
    let pos3 = unit.get_position().ok()?;
    let pos = dcso3::Vector2::new(pos3.p.x, pos3.p.z);
    Some(CsarPilotInfo { ucid, name, side, life_type, pos })
}

fn spawn_csar_pilot(lua: MizLua, ctx: &mut Context, info: Option<CsarPilotInfo>) {
    let Some(info) = info else { return };
    if let Err(e) = ctx.db.spawn_downed_pilot(
        lua,
        &ctx.idx,
        info.ucid,
        info.name.into(),
        info.side,
        info.life_type,
        info.pos,
    ) {
        error!("failed to spawn downed pilot: {:?}", e)
    }
}

struct DismountInfo {
    vehicle_typ: Vehicle,
    side: Side,
    pos: dcso3::Vector2,
    heading: f64,
    from_group: GroupId,
}

fn try_capture_dismount_info(ctx: &Context, unit: &Unit) -> Option<DismountInfo> {
    let id = unit.object_id().ok()?;
    let uid = ctx.db.ephemeral.get_uid_by_object_id(&id)?;
    let su = ctx.db.persisted.units.get(uid)?;
    // Skip aircraft — only ground vehicles dismount
    if su.tags.contains(UnitTag::Helicopter) || su.tags.contains(UnitTag::Aircraft) {
        return None;
    }
    // Skip if no dismount config for this vehicle type
    ctx.db.ephemeral.cfg.dismount.get(&su.typ)?;
    let pos3 = unit.get_position().ok()?;
    let pos = dcso3::Vector2::new(pos3.p.x, pos3.p.z);
    Some(DismountInfo {
        vehicle_typ: su.typ.clone(),
        side: su.side,
        pos,
        heading: su.heading,
        from_group: su.group.clone(),
    })
}

fn spawn_dismount(lua: MizLua, ctx: &mut Context, info: Option<DismountInfo>) {
    let Some(info) = info else { return };
    if let Err(e) = ctx.db.spawn_dismount_group(
        lua,
        &ctx.idx,
        &info.vehicle_typ,
        info.side,
        info.pos,
        info.heading,
        info.from_group,
    ) {
        error!("failed to spawn dismount group: {:?}", e)
    }
}

/// If the destroyed unit was carrying troops in the ground vehicle transport system,
/// spawn any survivors at the wreck position.
fn try_gv_passenger_eject(lua: MizLua, ctx: &mut Context, unit: &Unit) {
    let id = match unit.object_id() {
        Ok(id) => id,
        Err(_) => return,
    };
    let uid = match ctx.db.ephemeral.get_uid_by_object_id(&id) {
        Some(u) => *u,
        None => return,
    };
    if !ctx.db.ephemeral.ground_vehicle_passengers.contains_key(&uid) {
        return;
    }
    let wreck_pos = match unit.get_position() {
        Ok(p) => dcso3::Vector2::new(p.p.x, p.p.z),
        Err(_) => return,
    };
    if let Err(e) = ctx.db.on_ground_vehicle_destroyed(lua, &ctx.idx, uid, wreck_pos) {
        error!("ground vehicle passenger eject failed: {:?}", e)
    }
}

fn unit_killed(
    lua: MizLua,
    ctx: &mut Context,
    id: DcsOid<ClassUnit>,
    now: DateTime<Utc>,
) -> Result<()> {
    ctx.recently_landed.remove(&id);
    ctx.shots_out.dead(id.clone(), now);
    if let Err(e) = ctx.jtac.unit_dead(lua, &mut ctx.db, &id) {
        error!("jtac unit dead failed for {:?} {:?}", id, e)
    }
    if let Err(e) = ctx.db.unit_dead(&id, Utc::now()) {
        error!("unit dead failed for {:?} {:?}", id, e);
    }
    Ok(())
}

fn on_event(lua: MizLua, ev: Event) -> Result<()> {
    let start_ts = Utc::now();
    let ctx = unsafe { Context::get_mut() };
    let perf = Arc::make_mut(&mut unsafe { Perf::get_mut() }.inner);
    match &ev {
        Event::MarkAdded(e) | Event::MarkChange(e) | Event::MarkRemoved(e)
            if e.initiator.is_none() =>
        {
            ()
        }
        ev => info!("onEvent: {:?}", ev),
    }
    match ev {
        Event::Birth(b) => {
            if let Ok(unit) = b.initiator.as_unit() {
                ctx.recently_born.insert(unit.object_id()?, Utc::now());
                match ctx.db.unit_born(lua, &unit, &ctx.connected) {
                    Ok(BirthRes::None) => (),
                    Ok(BirthRes::OccupiedSlot(slot)) => {
                        ctx.menu_init_queue.insert(slot.clone());
                        if let Err(e) = atis::schedule_atis(lua, slot) {
                            error!("could not schedule atis: {:?}", e);
                        }
                    }
                    Ok(BirthRes::DynamicSlotDenied(ucid, rej)) => {
                        if let Some(id) = ctx.connected.id_by_ucid.get(&ucid) {
                            process_slot_rejection(ctx, *id, ucid, rej)
                        }
                        // just in case destroying the unit didn't work
                        ctx.db.ephemeral.force_player_to_spectators(&ucid);
                    }
                    Err(e) => {
                        error!("unit born failed {:?} {:?}", unit, e);
                    }
                }
            } else if let Ok(st) = b.initiator.as_static() {
                if let Err(e) = ctx.db.static_born(&st) {
                    error!("static born failed {:?} {:?}", st, e);
                }
            }
        }
        Event::PlayerLeaveUnit(e) => {
            if let Some(initiator) = e.initiator {
                if let Some(ucid) = ctx.db.player_in_unit(false, &initiator) {
                    if let Some(player) = ctx.db.player(&ucid) {
                        if let Some((_, Some(inst))) = player.current_slot.as_ref() {
                            if inst.landed_at_objective.is_none() {
                                ctx.shots_out.dead(initiator.clone(), start_ts)
                            }
                        }
                    }
                }
                if let Err(e) = ctx.db.player_left_unit(lua, start_ts, &initiator) {
                    error!("player left unit failed {:?}", e)
                }
            } else {
                error!("player leave unit with no unit")
            }
        }
        Event::Hit(e) | Event::Kill(e) => {
            if let Some(target) = e.target.as_ref().and_then(|t| t.as_unit().ok()) {
                let dead = target.get_life()? < 1;
                if let Some(shooter) = e.initiator.and_then(|u| u.as_unit().ok()) {
                    if let Err(e) = ctx.shots_out.hit(
                        &ctx.db,
                        start_ts,
                        dead,
                        &target,
                        &shooter,
                        e.weapon_name,
                    ) {
                        error!("error processing hit event {:?}", e)
                    }
                }
                if dead {
                    let dismount = try_capture_dismount_info(ctx, &target);
                    try_gv_passenger_eject(lua, ctx, &target);
                    if let Err(e) = unit_killed(lua, ctx, target.object_id()?, start_ts) {
                        error!("0 unit killed failed {:?}", e)
                    }
                    spawn_dismount(lua, ctx, dismount);
                }
            } else if let Some(target) =
                e.target.as_ref().and_then(|t| t.as_static().ok())
            {
                if target.get_life()? < 1 {
                    if let Err(e) = ctx.db.static_dead(&target.object_id()?, start_ts) {
                        error!("static dead failed {e:?}")
                    }
                }
            }
        }
        Event::Shot(e) => {
            if let Err(e) = ctx.shots_out.shot(&ctx.db, start_ts, &e) {
                error!("error processing shot event {:?}", e)
            }
            // Record shot position for artillery/launcher units only so nearby enemy
            // objectives stay awake while shells/missiles are inbound.
            if let Ok(obj_id) = e.initiator.object_id() {
                let shooter_info = ctx.db.ephemeral.get_uid_by_object_id(&obj_id)
                    .and_then(|uid| ctx.db.unit(uid).ok())
                    .map(|u| (u.side, u.tags.0, u.pos));
                if let Some((side, tags, pos)) = shooter_info {
                    if tags.contains(UnitTag::Artillery) || tags.contains(UnitTag::Launcher) {
                        ctx.db.ephemeral.recent_shots.push((pos, side, start_ts));
                    }
                }
            }
            // Counter-battery detection
            let cb_params = ctx.db.ephemeral.cfg.counter_battery.as_ref()
                .map(|c| (c.grid_resolution_m, c.cooldown_secs));
            if let Some((cb_res, cb_cooldown)) = cb_params {
                if let Ok(obj_id) = e.initiator.object_id() {
                    let shooter_info = ctx.db.ephemeral.get_uid_by_object_id(&obj_id)
                        .and_then(|uid| ctx.db.unit(uid).ok())
                        .map(|u| (u.side, u.tags.0, u.pos));
                    if let Some((side, tags, pos2)) = shooter_info {
                        if tags.contains(UnitTag::Artillery) || tags.contains(UnitTag::Launcher) {
                            let res = cb_res.max(1.0);
                            let cell = ((pos2.x / res) as i64, (pos2.y / res) as i64);
                            let cooldown = Duration::seconds(cb_cooldown as i64);
                            let last = ctx.db.ephemeral.counter_battery_reports.get(&cell).copied();
                            if last.map(|t| start_ts - t >= cooldown).unwrap_or(true) {
                                ctx.db.ephemeral.counter_battery_reports.insert(cell, start_ts);
                                let friendly = side.opposite();
                                ctx.db.ephemeral.on_counter_battery(pos2, friendly, start_ts);
                            }
                        }
                    }
                }
            }
            ()
        }
        Event::Dead(e) | Event::UnitLost(e) => {
            if let Some(unit) = e.initiator.as_ref().and_then(|u| u.as_unit().ok()) {
                let dismount = try_capture_dismount_info(ctx, &unit);
                try_gv_passenger_eject(lua, ctx, &unit);
                let id = unit.object_id()?;
                if let Err(e) = unit_killed(lua, ctx, id, start_ts) {
                    error!("1 unit killed failed {:?}", e)
                }
                spawn_dismount(lua, ctx, dismount);
            } else if let Some(st) = e.initiator.as_ref().and_then(|s| s.as_static().ok())
            {
                if let Err(e) = ctx.db.static_dead(&st.object_id()?, start_ts) {
                    error!("static killed failed {e:?}")
                }
            }
        }
        Event::PilotDead(e) => {
            if let Some(unit) = e.initiator.as_ref().and_then(|u| u.as_unit().ok()) {
                let csar_pilot = try_capture_csar_info(lua, ctx, &unit);
                let id = unit.object_id()?;
                if let Err(e) = unit_killed(lua, ctx, id, start_ts) {
                    error!("1 unit killed failed {:?}", e)
                }
                spawn_csar_pilot(lua, ctx, csar_pilot);
            } else if let Some(st) = e.initiator.as_ref().and_then(|s| s.as_static().ok())
            {
                if let Err(e) = ctx.db.static_dead(&st.object_id()?, start_ts) {
                    error!("static killed failed {e:?}")
                }
            }
        }
        Event::Ejection(e) => {
            if let Ok(unit) = e.initiator.as_unit() {
                let csar_pilot = try_capture_csar_info(lua, ctx, &unit);
                let id = unit.object_id()?;
                if let Err(e) = unit_killed(lua, ctx, id, start_ts) {
                    error!("2 unit killed failed {}", e)
                }
                spawn_csar_pilot(lua, ctx, csar_pilot);
            }
        }
        Event::Takeoff(e) | Event::PostponedTakeoff(e) => {
            if let Ok(unit) = e.initiator.as_unit() {
                let id = unit.object_id()?;
                if !ctx.recently_born.contains_key(&id)
                    && ctx.airborne.insert(id.clone())
                    && ctx.recently_landed.remove(&id).is_none()
                {
                    let slot = unit.slot()?;
                    let position = unit.get_ground_position()?.0;
                    match ctx.db.takeoff(Utc::now(), slot, &unit, position) {
                        Err(e) => error!("could not process takeoff, {:?}", e),
                        Ok(TakeoffRes::NoLifeTaken) => (),
                        Ok(TakeoffRes::TookLife(typ)) => {
                            if let Err(e) =
                                message_life(ctx, &slot, Some(typ), "life taken\n")
                            {
                                error!("could not display life taken message {:?}", e)
                            }
                            let _ = menu::cargo::list_cargo_for_slot(ctx, &slot);
                        }
                        Ok(TakeoffRes::OutOfLives | TakeoffRes::OutOfPoints) => {
                            if let Err(e) = unit.destroy() {
                                error!(
                                    "failed to destroy unit that took off without lives or points {e:?}"
                                )
                            }
                        }
                    }
                }
            }
        }
        Event::Land(e) | Event::PostponedLand(e) => {
            if let Ok(unit) = e.initiator.as_unit() {
                let id = unit.object_id()?;
                if !ctx.recently_born.contains_key(&id) && ctx.airborne.remove(&id) {
                    ctx.recently_landed.insert(id, Utc::now());
                }
            }
        }
        Event::MarkAdded(MarkPanel { initiator: Some(unit), .. }) => {
            let oid = unit.object_id()?;
            if let Some(slot) = ctx.db.ephemeral.get_slot_by_object_id(&oid) {
                let slot = *slot;
                if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot) {
                    let ucid = *ucid;
                    if ctx.subscribed_action_menus.contains(&slot) {
                        if let Err(e) = menu::action::init_action_menu_for_slot(
                            ctx, lua, &slot, &ucid,
                        ) {
                            error!("failed to init action menu for {ucid} {slot} {e:?}")
                        }
                    }
                }
            }
        }
        Event::MissionEnd => unsafe {
            Context::reset();
            Perf::reset();
            Context::get_mut().init_async_bg(lua.inner())?;
            return Ok(()); // avoid record perf with a reset perf context
        },
        _ => (),
    }
    record_perf(&mut perf.dcs_events, start_ts);
    Ok(())
}

fn lives(db: &mut Db, ucid: &Ucid, typfilter: Option<LifeType>) -> Result<CompactString> {
    db.maybe_reset_lives(ucid, Utc::now())?;
    let player = db.player(ucid).ok_or_else(|| anyhow!("no such player {:?}", ucid))?;
    let cfg = &db.ephemeral.cfg;
    let lives = &player.lives;
    let mut msg = CompactString::new("");
    let now = Utc::now();
    for (typ, (n, reset_after)) in &cfg.default_lives {
        if typfilter.is_none() || Some(*typ) == typfilter {
            match lives.get(typ) {
                None => msg.push_str(&format_compact!("{typ} {n}/{n}\n")),
                Some((reset, cur)) => {
                    let since_reset = now - *reset;
                    let reset = chatcmd::format_duration(
                        Duration::seconds(*reset_after as i64) - since_reset,
                    );
                    msg.push_str(&format_compact!(
                        "{typ} {cur}/{n} resetting in {reset}\n"
                    ));
                }
            }
        }
    }
    Ok(msg)
}

fn message_life(
    ctx: &mut Context,
    slot: &SlotId,
    typ: Option<LifeType>,
    msg: &str,
) -> Result<()> {
    let uid = slot.as_unit_id().ok_or_else(|| anyhow!("not a unit"))?;
    let ucid = ctx
        .db
        .ephemeral
        .player_in_slot(slot)
        .ok_or_else(|| anyhow!("no player in slot {:?}", slot))?
        .clone();
    let mut msg = CompactString::new(msg);
    if let Ok(lives) = lives(&mut ctx.db, &ucid, typ) {
        msg.push_str(&lives)
    }
    ctx.db.ephemeral.msgs().panel_to_unit(10, false, uid, msg);
    Ok(())
}

fn return_lives(lua: MizLua, ctx: &mut Context, ts: DateTime<Utc>) {
    macro_rules! or_false {
        ($e:expr) => {
            match $e {
                Ok(r) => r,
                Err(_) => return false,
            }
        };
    }
    let db = &mut ctx.db;
    let mut returned: SmallVec<[(LifeType, SlotId); 4]> = smallvec![];
    ctx.recently_landed.retain(|id, landed_ts| {
        if ts - *landed_ts >= Duration::seconds(10) {
            let unit = or_false!(Unit::get_instance(lua, id));
            let pos = or_false!(unit.get_ground_position());
            let slot = or_false!(unit.slot());
            if let Some(typ) = db.land(slot.clone(), pos.0, &unit) {
                returned.push((typ, slot));
                return false;
            }
        }
        true
    });
    for (typ, slot) in returned {
        if let Err(e) = message_life(ctx, &slot, Some(typ), "life returned\n") {
            error!("failed to send life returned message to {:?} {}", slot, e);
        }
    }
}

fn advise_captureable(ctx: &mut Context) -> Result<()> {
    let cur_cap = ctx.db.capturable_objectives();
    for oid in &cur_cap {
        let dur = ctx.captureable.entry(*oid).or_default();
        *dur += 1;
        if *dur == 10 {
            let m =
                format_compact!("{} is now capturable", ctx.db.objective(oid)?.name());
            ctx.db.ephemeral.msgs().panel_to_all(30, false, m);
        }
    }
    ctx.captureable.retain(|oid, _| cur_cap.contains(oid));
    Ok(())
}

fn advise_captured(ctx: &mut Context, lua: MizLua, ts: DateTime<Utc>) -> Result<bool> {
    let mut has_captures = false;
    for (side, oid) in ctx.db.check_capture(lua, ts)? {
        has_captures = true;
        ctx.event_scheduler.owned_cache_dirty = true;
        let (name, pos) = {
            let obj = ctx.db.objective(&oid)?;
            (obj.name().to_owned(), obj.pos())
        };
        let mark_text = format_compact!("{} captured by {:?}", name, side);
        ctx.db.ephemeral.msgs().mark_to_all(pos, true, mark_text);
        ctx.captureable.remove(&oid);
    }
    Ok(has_captures)
}

fn generate_ewr_reports(ctx: &mut Context, now: DateTime<Utc>) -> Result<()> {
    use std::fmt::Write;
    let mut msgs: SmallVec<[(UnitId, CompactString); 64]> = smallvec![];
    for (ucid, player, inst) in ctx.db.instanced_players() {
        let uid = match player.current_slot.as_ref().and_then(|(sl, _)| sl.as_unit_id()) {
            Some(uid) => uid,
            None => continue,
        };
        let braa_to_chickens = ctx.ewr.where_chicken(
            now,
            false,
            false,
            ucid,
            player,
            inst,
            ctx.db.ephemeral.cfg.ewr_mode,
            ctx.db.ephemeral.cfg.ewr_delay,
        );
        if !braa_to_chickens.is_empty() {
            let mut report = format_compact!("Bandits BRAA\n");
            write!(report, "{}\n", ewr::HEADER)?;
            for gibbraa in braa_to_chickens {
                write!(report, "{gibbraa}\n")?;
            }
            msgs.push((uid, report));
        }
        let spikes = ctx.ewr.spike_warnings(now, ucid, player, inst);
        for warning in spikes {
            msgs.push((uid, warning));
        }
    }
    for (uid, msg) in msgs {
        ctx.db.ephemeral.msgs().panel_to_unit(10, false, uid, msg)
    }
    Ok(())
}

fn check_auto_shutdown(
    ctx: &mut Context,
    lua: MizLua,
    now: DateTime<Utc>,
) -> Result<AdminResult> {
    if let Some(asd) = ctx.shutdown.as_mut() {
        if asd.when - now <= Duration::minutes(30) && !asd.thirty_minute_warning {
            asd.thirty_minute_warning = true;
            ctx.db.ephemeral.msgs().panel_to_all(
                60,
                false,
                "The server will restart in 30 minutes",
            );
        }
        if asd.when - now <= Duration::minutes(10) && !asd.ten_minute_warning {
            asd.ten_minute_warning = true;
            ctx.db.ephemeral.msgs().panel_to_all(
                60,
                true,
                "The server will restart in 10 minutes",
            );
        }
        if asd.when - now <= Duration::minutes(5) && !asd.five_minute_warning {
            asd.five_minute_warning = true;
            ctx.db.ephemeral.msgs().panel_to_all(
                60,
                true,
                "The server will restart in 5 minutes",
            )
        }
        if asd.when - now <= Duration::minutes(1) && !asd.one_minute_warning {
            asd.one_minute_warning = true;
            ctx.db.ephemeral.msgs().panel_to_all(
                60,
                true,
                "The server will restart in one minute",
            )
        }
        if now > asd.when {
            return admin::admin_shutdown(ctx, lua, None);
        }
    }
    if let Some(victor) = ctx.db.check_victory(now) {
        return admin::admin_shutdown(ctx, lua, Some(Some(victor)));
    }
    Ok(AdminResult::Continue)
}

fn force_players_to_spectators(ctx: &mut Context, net: &Net, ts: DateTime<Utc>) {
    for (_, ids) in ctx.db.ephemeral.players_to_force_to_spectators(ts) {
        for ucid in ids {
            match ctx.connected.id_by_ucid.get(&ucid) {
                None => warn!("no id for player ucid {:?}", ucid),
                Some(id) => {
                    info!("forcing player {} to spectators", ucid);
                    if let Err(e) =
                        net.force_player_slot(*id, Side::Neutral, SlotId::Spectator)
                    {
                        error!("error forcing player {:?} to spectators {:?}", id, e);
                    }
                    match net.get_slot(*id) {
                        Err(_) => ctx.db.ephemeral.force_player_to_spectators(&ucid),
                        Ok((side, slot)) => {
                            if side != Side::Neutral || !slot.is_spectator() {
                                ctx.db.ephemeral.force_player_to_spectators(&ucid)
                            }
                        }
                    }
                }
            }
        }
    }
}

fn update_jtac_contacts(ctx: &mut Context, lua: MizLua) {
    match ctx.jtac.update_contacts(lua, &mut ctx.landcache, &mut ctx.db) {
        Err(e) => error!("could not update jtac contacts {e}"),
        Ok(dirty_menus) => {
            let mut dirty_slots: SmallVec<[SlotId; 16]> = smallvec![];
            for (side, oids) in dirty_menus {
                for (_, player, _) in ctx.db.instanced_players() {
                    if player.side == side {
                        if let Some((slot, _)) = player.current_slot.as_ref() {
                            let mut dead: SmallVec<[JtId; 4]> = smallvec![];
                            let mut expunge = false;
                            if let Some(subd) = ctx.subscribed_jtac_menus.get_mut(&slot) {
                                let pinned: SmallVec<[ObjectiveId; 16]> = subd
                                    .pinned
                                    .iter()
                                    .filter_map(|jt| match ctx.jtac.get(jt) {
                                        Ok(jt) => Some(jt.location().oid),
                                        Err(_) => {
                                            dead.push(*jt);
                                            None
                                        }
                                    })
                                    .collect();
                                for oid in &oids {
                                    if subd.subscribed_objectives.contains(oid) {
                                        if !dirty_slots.contains(slot) {
                                            dirty_slots.push(*slot);
                                        }
                                    }
                                    if !pinned.contains(oid) {
                                        subd.subscribed_objectives.remove(oid);
                                    }
                                }
                                expunge = subd.subscribed_objectives.is_empty();
                            }
                            if dead.len() > 0 {
                                let dead = dead.drain(..);
                                if let Some(subd) =
                                    ctx.subscribed_jtac_menus.get_mut(slot)
                                {
                                    for jtid in dead {
                                        subd.pinned.remove(&jtid);
                                    }
                                }
                            }
                            if expunge {
                                ctx.subscribed_jtac_menus.remove(slot);
                            }
                        }
                    }
                }
            }
            for slot in dirty_slots {
                if let Err(e) = menu::jtac::init_jtac_menu_for_slot(ctx, lua, &slot) {
                    error!("could not init jtac menu for slot {slot}, {e:?}")
                }
            }
        }
    }
}

fn award_periodic_points(ctx: &mut Context, ts: DateTime<Utc>) {
    if let Some(points) = ctx.db.ephemeral.cfg.points.as_ref() {
        let (award, period) = points.periodic_point_gain;
        if award != 0 && period > 0 {
            let elapsed = (ts - ctx.last_periodic_points).num_seconds();
            if elapsed >= period as i64 {
                ctx.last_periodic_points = ts;
                for ifo in ctx.connected.info_by_player_id.values() {
                    ctx.db.adjust_points(&ifo.ucid, award, "periodic award")
                }
            }
        }
    }
}

fn tick_smart_commander(_lua: MizLua, ctx: &mut Context, ts: DateTime<Utc>) {
    let cfg = match ctx.db.ephemeral.cfg.smart_commander.as_ref() {
        Some(c) => c.clone(),
        None => return,
    };
    let elapsed = (ts - ctx.last_commander_tick).num_seconds();
    if elapsed < cfg.tick_period_secs as i64 {
        return;
    }
    ctx.last_commander_tick = ts;
    let mut ucids_by_side: fxhash::FxHashMap<dcso3::coalition::Side, Vec<dcso3::net::Ucid>> =
        fxhash::FxHashMap::default();
    for ifo in ctx.connected.info_by_player_id.values() {
        if let Some(player) = ctx.db.persisted.players.get(&ifo.ucid) {
            ucids_by_side
                .entry(player.side)
                .or_default()
                .push(ifo.ucid);
        }
    }
    commander::tick(&mut ctx.db, &cfg, ts, &ucids_by_side);

    // Strategic events — only when campaign_events is also configured.
    if let Some(events_cfg) = ctx.db.ephemeral.cfg.campaign_events.clone() {
        if events_cfg.enabled {
            let player_count = ctx.connected.len();
            let (messages, effects) =
                commander::tick_events(&mut ctx.db, &cfg, &events_cfg, ts, &mut ctx.event_scheduler, player_count);
            for msg in messages {
                ctx.db.ephemeral.msgs().panel_to_all(15, false, msg);
            }
            ctx.event_scheduler.pending_effects.extend(effects);
        }
    }
}

fn update_frontline(ctx: &mut Context, ts: DateTime<Utc>, force_update: bool) {
    // Check if frontline feature is enabled
    let frontline_cfg = match &ctx.db.ephemeral.cfg.frontline {
        Some(cfg) if cfg.enabled => cfg.clone(),
        _ => {
            // If disabled and we have a frontline instance, remove it
            if let Some(fl) = ctx.frontline.take() {
                info!("Frontline disabled, removing markers");
                fl.remove(ctx.db.ephemeral.msgs());
            }
            return;
        }
    };

    // Initialize frontline if not already present
    if ctx.frontline.is_none() {
        info!("Initializing dynamic frontline system");
        ctx.frontline = Some(frontline::FrontLine::new(frontline_cfg.clone()));
    }

    // Only update when forced (on objective change) since update_on_objective_change_only is always enabled
    if !force_update {
        return;
    }

    ctx.last_frontline_update = ts;

    // Update the frontline drawing
    if let Some(fl) = &mut ctx.frontline {
        // Collect current unit positions for pressure calculation
        fl.collect_unit_pressure(&ctx.db.persisted, ts);

        if fl.update(&ctx.db.persisted, ctx.db.ephemeral.msgs(), ts) {
            info!("Frontline updated successfully");
        }
    }
}

/// Find a ground/armor template name for the given side at an objective.
fn find_ground_template(
    db: &db::Db,
    objective: bfprotocols::db::objective::ObjectiveId,
    side: dcso3::coalition::Side,
) -> Option<dcso3::String> {
    use crate::db::objective::ObjGroupClass;
    let is_ground = |class: &ObjGroupClass| matches!(
        class,
        ObjGroupClass::Armor | ObjGroupClass::Mr | ObjGroupClass::Sr | ObjGroupClass::Lr
    );

    // First try the requested objective's own groups
    if let Some(obj) = db.persisted.objectives.get(&objective) {
        if let Some(gids) = obj.groups().get(&side) {
            let found = gids.into_iter().find_map(|gid| {
                db.persisted.groups.get(gid).and_then(|g| {
                    if is_ground(&g.class) { Some(g.template_name.clone()) } else { None }
                })
            });
            if found.is_some() {
                return found;
            }
        }
    }

    // Fallback: find any ground template owned by this side (e.g. carrier objectives have no armor)
    db.persisted.objectives.into_iter()
        .filter(|(_, o)| o.owner() == side)
        .find_map(|(_, o)| {
            o.groups().get(&side)?.into_iter().find_map(|gid| {
                db.persisted.groups.get(gid).and_then(|g| {
                    if is_ground(&g.class) { Some(g.template_name.clone()) } else { None }
                })
            })
        })
}

fn drain_event_effects(lua: MizLua, ctx: &mut Context) {
    use crate::db::events::EventScheduler;
    let budget = EventScheduler::EFFECTS_PER_TICK.min(ctx.event_scheduler.pending_effects.len());
    let to_apply: Vec<_> = ctx.event_scheduler.pending_effects.drain(..budget).collect();
    if to_apply.is_empty() {
        return;
    }
    apply_event_effects(lua, ctx, to_apply);
}

fn apply_event_effects(lua: MizLua, ctx: &mut Context, effects: Vec<EventEffect>) {
    use crate::db::group::DeployKind;
    use crate::db::events::EventEffect;
    use crate::db::objective::ObjGroupClass;
    use crate::spawnctx::{SpawnCtx, SpawnLoc};
    use dcso3::Color;
    use dcso3::controller::{ActionTyp, AltType, MissionPoint, PointType, Task, VehicleFormation};
    use dcso3::group::Group;
    use dcso3::land::Land;
    use dcso3::LuaVec2;
    use dcso3::trigger::{CircleSpec, LineType, SideFilter, TextSpec};
    use enumflags2::BitFlags;

    let spctx = match SpawnCtx::new(lua) {
        Ok(s) => s,
        Err(e) => {
            error!("event effects: could not create SpawnCtx: {e}");
            return;
        }
    };

    for effect in effects {
        match effect {
            EventEffect::MarkInbound { event_id, side, obj_pos, obj_name } => {
                let color = side_color(side);
                let mid = dcso3::trigger::MarkId::new();
                ctx.db.ephemeral.msgs().circle_to_all(
                    SideFilter::All,
                    mid,
                    CircleSpec {
                        center: dcso3::LuaVec3(dcso3::Vector3::new(obj_pos.x, 0., obj_pos.y)),
                        radius: 1500.,
                        color,
                        fill_color: Color::new(0., 0., 0., 0.),
                        line_type: LineType::Dashed,
                        read_only: true,
                    },
                    Some(format_compact!("Reinforcements Inbound [{:?}] → {}", side, obj_name).into()),
                );
                ctx.event_scheduler.register_mark(event_id, mid);
            }

            EventEffect::SpawnReinforcements { event_id, side, objective, obj_pos } => {
                let template = find_ground_template(&ctx.db, objective, side);
                let template = match template {
                    Some(t) => t,
                    None => {
                        info!("SpawnReinforcements: no suitable template at {:?}", objective);
                        continue;
                    }
                };

                // Re-query the source objective fresh at spawn time — more accurate than
                // the position stored at event-creation time, and handles the case where
                // ownership changed during the ETA countdown.
                let enemy_side = match side {
                    dcso3::coalition::Side::Red => dcso3::coalition::Side::Blue,
                    dcso3::coalition::Side::Blue => dcso3::coalition::Side::Red,
                    dcso3::coalition::Side::Neutral => dcso3::coalition::Side::Neutral,
                };
                let fresh_source = crate::db::events::find_nearest_friendly_objective(
                    &ctx.db, side, obj_pos, Some(objective),
                );

                // Determine spawn origin: friendly source objective if one exists,
                // otherwise generate a fallback point 3 km from the destination in the
                // direction AWAY from the nearest enemy so units always march in visibly.
                let spawn_origin: dcso3::Vector2 = match fresh_source {
                    Some(src) => {
                        info!(
                            "SpawnReinforcements: {:?} sourcing from nearest friendly at {:?}",
                            side, src
                        );
                        src
                    }
                    None => {
                        // Compute a fallback spawn 3 km behind the objective.
                        let fallback = match crate::db::events::find_nearest_friendly_objective(
                            &ctx.db, enemy_side, obj_pos, None,
                        ) {
                            Some(enemy_pos) => {
                                let to_enemy = enemy_pos - obj_pos;
                                let dist = to_enemy.norm();
                                if dist > 1.0 {
                                    // Move 3 km in the direction AWAY from the enemy
                                    obj_pos + (-to_enemy / dist) * 3000.0
                                } else {
                                    obj_pos + dcso3::Vector2::new(3000.0, 0.0)
                                }
                            }
                            None => obj_pos + dcso3::Vector2::new(3000.0, 0.0),
                        };
                        info!(
                            "SpawnReinforcements: {:?} no friendly source — using fallback at {:?}",
                            side, fallback
                        );
                        fallback
                    }
                };

                match ctx.db.add_and_queue_group(
                    &spctx,
                    &ctx.idx,
                    side,
                    SpawnLoc::AtPos {
                        pos: spawn_origin + dcso3::Vector2::new(200., 200.),
                        offset_direction: dcso3::Vector2::new(1., 0.),
                        group_heading: 0.,
                    },
                    &template,
                    DeployKind::Objective { origin: objective },
                    BitFlags::empty(),
                    None,
                ) {
                    Ok(gid) => {
                        // Always build a road route — units march from the spawn origin
                        // through any intermediate friendly objectives to the destination.
                        let route = crate::db::events::build_reinforcement_route(
                            &ctx.db, side, spawn_origin, obj_pos,
                        );
                        info!(
                            "SpawnReinforcements: spawned {:?} for {:?}, marching via {} waypoints → {:?}",
                            gid, side, route.len(), obj_pos
                        );
                        ctx.event_scheduler.pending_moves.insert(gid, route);
                    }
                    Err(e) => error!("SpawnReinforcements: {e:?}"),
                }
                ctx.db.ephemeral.on_reinforcements_arrived(obj_pos, side, Utc::now());
                let _ = event_id;
            }

            EventEffect::SpawnHvt { event_id, side, objective, obj_pos, reward_points, template, circle_radius_m, escape_route } => {
                let gid = match ctx.db.add_and_queue_group(
                    &spctx,
                    &ctx.idx,
                    side,
                    SpawnLoc::AtPos {
                        pos: obj_pos,
                        offset_direction: dcso3::Vector2::new(0., 1.),
                        group_heading: 0.,
                    },
                    &template,
                    DeployKind::Objective { origin: objective },
                    BitFlags::empty(),
                    None,
                ) {
                    Ok(gid) => {
                        info!("SpawnHvt: spawned {:?} for {:?}", gid, side);
                        ctx.event_scheduler.hvt_groups.insert(gid, (event_id, reward_points));
                        ctx.event_scheduler.hvt_group_by_event.insert(event_id, gid);
                        gid
                    }
                    Err(e) => { error!("SpawnHvt: {e:?}"); continue; }
                };
                // Build escape route through TOWN_ trigger zones (nearest-first chain).
                // Falls back to the objective-based escape_route if no TOWN_ zones exist.
                let town_route = build_hvt_town_route(&spctx, obj_pos, 3);
                let route = if !town_route.is_empty() { town_route } else { escape_route };
                if !route.is_empty() {
                    ctx.event_scheduler.pending_moves.insert(gid, route);
                }
                // Persist the gid so DespawnHvt can find it after a server restart.
                if let Some(evt) = ctx.event_scheduler.active_events.iter_mut().find(|e| e.id() == event_id) {
                    if let crate::db::events::CampaignEvent::HighValueTarget { spawned_gid, .. } = evt {
                        *spawned_gid = Some(gid);
                    }
                }
                // F10 circle + text label for the HVT (mirrors objective map markers)
                let hvt_color = match side {
                    dcso3::coalition::Side::Red => Color::red(1.),
                    _ => Color::new(0., 0., 0., 1.),
                };
                let circle_mid = dcso3::trigger::MarkId::new();
                ctx.db.ephemeral.msgs().circle_to_all(
                    SideFilter::All,
                    circle_mid,
                    CircleSpec {
                        center: dcso3::LuaVec3(dcso3::Vector3::new(obj_pos.x, 0., obj_pos.y)),
                        radius: circle_radius_m,
                        color: hvt_color,
                        fill_color: Color::new(0., 0., 0., 0.),
                        line_type: LineType::Solid,
                        read_only: true,
                    },
                    None,
                );
                ctx.event_scheduler.register_mark(event_id, circle_mid);
                let text_mid = dcso3::trigger::MarkId::new();
                ctx.db.ephemeral.msgs().text_to_all(
                    SideFilter::All,
                    text_mid,
                    TextSpec {
                        pos: dcso3::LuaVec3(dcso3::Vector3::new(obj_pos.x, 0., obj_pos.y)),
                        color: hvt_color,
                        fill_color: Color::new(0., 0., 0., 0.6),
                        font_size: 12,
                        read_only: true,
                        text: format_compact!("HVT +{} pts", reward_points).into(),
                    },
                );
                ctx.event_scheduler.register_mark(event_id, text_mid);
            }

            EventEffect::OrderAttack { event_id, attacking_side, target_positions } => {
                let land = match Land::singleton(lua) {
                    Ok(l) => l,
                    Err(e) => {
                        error!("OrderAttack: could not get Land singleton: {e}");
                        continue;
                    }
                };

                use crate::db::objective::ObjGroupClass;
                // Cap per target — reuse barrage_max_groups so it's configurable.
                let max_groups_per_target = ctx.db.ephemeral.cfg.campaign_events
                    .as_ref().map(|c| c.barrage_max_groups).unwrap_or(5);

                // Collect alive Armor/Mr/Sr/Lr groups for the attacking side with positions.
                let mut candidates: Vec<(bfprotocols::db::group::GroupId, dcso3::String, dcso3::Vector2)> = ctx
                    .db
                    .persisted
                    .groups_by_side
                    .get(&attacking_side)
                    .map(|s| s.into_iter().copied().collect::<Vec<_>>())
                    .unwrap_or_default()
                    .into_iter()
                    .filter_map(|gid| {
                        let g = ctx.db.persisted.groups.get(&gid)?;
                        match g.class {
                            ObjGroupClass::Armor | ObjGroupClass::Mr | ObjGroupClass::Sr | ObjGroupClass::Lr => {}
                            _ => return None,
                        }
                        let alive = g.units.into_iter().any(|uid| {
                            ctx.db.persisted.units.get(uid).map(|u| !u.dead).unwrap_or(false)
                        });
                        if !alive { return None; }
                        let pos = ctx.db.group_center(&gid).ok()?;
                        Some((gid, g.name.clone(), pos))
                    })
                    .collect();

                // For each target position, assign the closest available groups (up to cap),
                // then remove them from the pool so they aren't double-ordered.
                for target_pos in &target_positions {
                    candidates.sort_by(|(_, _, pa), (_, _, pb)| {
                        let da = na::distance_squared(&(*pa).into(), &(*target_pos).into());
                        let db_d = na::distance_squared(&(*pb).into(), &(*target_pos).into());
                        da.partial_cmp(&db_d).unwrap_or(std::cmp::Ordering::Equal)
                    });

                    let alt = land.get_height(LuaVec2(*target_pos)).unwrap_or(0.);
                    let mut ordered = 0usize;
                    let mut used_indices = vec![];

                    for (idx, (_, group_name, _)) in candidates.iter().enumerate() {
                        if ordered >= max_groups_per_target { break; }
                        let dcs_group = match Group::get_by_name(lua, group_name.as_str()) {
                            Ok(g) => g,
                            Err(_) => continue,
                        };
                        let controller = match dcs_group.get_controller() {
                            Ok(c) => c,
                            Err(e) => { error!("OrderAttack: get_controller {group_name}: {e}"); continue; }
                        };
                        let task = Task::Mission {
                            airborne: Some(false),
                            route: vec![MissionPoint {
                                typ: PointType::TurningPoint,
                                airdrome_id: None,
                                time_re_fu_ar: None,
                                helipad: None,
                                link_unit: None,
                                action: Some(ActionTyp::Ground(VehicleFormation::OnRoad)),
                                pos: LuaVec2(*target_pos),
                                alt,
                                alt_typ: Some(AltType::BARO),
                                speed: 8.,
                                speed_locked: Some(false),
                                eta: None,
                                eta_locked: None,
                                name: None,
                                task: Box::new(Task::Hold),
                            }],
                        };
                        if let Err(e) = controller.set_task(task) {
                            error!("OrderAttack: set_task {group_name}: {e}");
                        } else {
                            info!("OrderAttack: {:?} group {group_name} → target {:?}", attacking_side, target_pos);
                            ordered += 1;
                            used_indices.push(idx);
                        }
                    }
                    // Remove used groups from pool (reverse order to preserve indices).
                    for idx in used_indices.into_iter().rev() {
                        candidates.remove(idx);
                    }
                }

                // F10 dashed circles at each target.
                let atk_color = side_color(attacking_side);
                for tpos in &target_positions {
                    let mid = dcso3::trigger::MarkId::new();
                    ctx.db.ephemeral.msgs().circle_to_all(
                        SideFilter::All,
                        mid,
                        CircleSpec {
                            center: dcso3::LuaVec3(dcso3::Vector3::new(tpos.x, 0., tpos.y)),
                            radius: 2000.,
                            color: atk_color,
                            fill_color: Color::new(0., 0., 0., 0.),
                            line_type: LineType::Dashed,
                            read_only: true,
                        },
                        Some(format_compact!("Counter-Offensive [{:?}]", attacking_side).into()),
                    );
                    ctx.event_scheduler.register_mark(event_id, mid);
                }
            }

            // C: Artillery/armor barrage — move Armor/Mr/Lr groups into firing range then fire.
            // Groups already in range fire immediately; out-of-range groups are given a
            // waypoint along the src→target vector at (arty_range * 0.85) from the target
            // with a FireAtPoint task embedded, so DCS AI drives them into position and fires.
            EventEffect::FireBarrage { event_id, side, source_objective, target_pos } => {
                let land = match Land::singleton(lua) {
                    Ok(l) => l,
                    Err(e) => { error!("FireBarrage: Land singleton: {e}"); continue; }
                };

                let obj = ctx.db.persisted.objectives.get(&source_objective);
                let gids: Vec<_> = obj
                    .and_then(|o| o.groups().get(&side))
                    .map(|gs| gs.into_iter().copied().collect())
                    .unwrap_or_default();

                let barrage_radius = ctx.db.ephemeral.cfg.campaign_events
                    .as_ref().map(|c| c.barrage_radius_m).unwrap_or(500.0);
                let barrage_max_groups = ctx.db.ephemeral.cfg.campaign_events
                    .as_ref().map(|c| c.barrage_max_groups).unwrap_or(5);
                // Effective weapon range — stay 15% inside it to ensure the AI can engage.
                let arty_range = ctx.db.ephemeral.cfg.artillery_mission_range as f64 * 0.85;

                let alt = land.get_height(LuaVec2(target_pos)).unwrap_or(0.);
                let fire_task = Task::FireAtPoint {
                    point: LuaVec2(target_pos),
                    radius: Some(barrage_radius),
                    expend_qty: None,
                    weapon_type: None,
                    altitude: Some(alt),
                    altitude_type: Some(AltType::BARO),
                };

                let mut fired = 0u32;
                for gid in gids.iter().take(barrage_max_groups) {
                    let group = match ctx.db.persisted.groups.get(gid) {
                        Some(g) => g,
                        None => continue,
                    };
                    match group.class {
                        ObjGroupClass::Armor | ObjGroupClass::Mr | ObjGroupClass::Lr => {}
                        _ => continue,
                    }
                    let alive = group.units.into_iter().any(|uid| {
                        ctx.db.persisted.units.get(uid).map(|u| !u.dead).unwrap_or(false)
                    });
                    if !alive { continue; }
                    let group_name = group.name.clone();
                    let group_pos = ctx.db.group_center(gid).unwrap_or(target_pos);
                    let dist = na::distance(&group_pos.into(), &target_pos.into());

                    // Compute the waypoint the group should move to before firing.
                    // If already in range, waypoint = current position (fire in place).
                    let waypoint_pos = if dist <= arty_range {
                        group_pos
                    } else {
                        // Step from target toward group at arty_range distance.
                        let dir = (group_pos - target_pos).normalize();
                        target_pos + dir * arty_range
                    };

                    let mission = Task::Mission {
                        airborne: Some(false),
                        route: vec![MissionPoint {
                            action: Some(ActionTyp::Ground(VehicleFormation::OffRoad)),
                            typ: PointType::TurningPoint,
                            airdrome_id: None,
                            helipad: None,
                            time_re_fu_ar: None,
                            link_unit: None,
                            pos: LuaVec2(waypoint_pos),
                            alt: 0.,
                            alt_typ: Some(AltType::RADIO),
                            speed: 0.,
                            speed_locked: None,
                            eta: None,
                            eta_locked: None,
                            name: None,
                            task: Box::new(fire_task.clone()),
                        }],
                    };

                    if let Ok(dcs_group) = Group::get_by_name(lua, group_name.as_str()) {
                        if let Ok(controller) = dcs_group.get_controller() {
                            if let Err(e) = controller.set_task(mission) {
                                error!("FireBarrage: set_task {group_name}: {e}");
                            } else {
                                fired += 1;
                                let action = if dist <= arty_range { "firing in place" } else { "moving to firing position" };
                                info!("FireBarrage: {:?} group {group_name} {action}, dist={:.0}m", side, dist);
                            }
                        }
                    }
                }

                // F10 mark at target
                if fired > 0 {
                    let mid = dcso3::trigger::MarkId::new();
                    ctx.db.ephemeral.msgs().circle_to_all(
                        SideFilter::All,
                        mid,
                        CircleSpec {
                            center: dcso3::LuaVec3(dcso3::Vector3::new(target_pos.x, 0., target_pos.y)),
                            radius: 2000.,
                            color: side_color(side),
                            fill_color: Color::new(0., 0., 0., 0.),
                            line_type: LineType::Dashed,
                            read_only: true,
                        },
                        Some(format_compact!("Fire Support [{:?}] — {} units firing", side, fired).into()),
                    );
                    ctx.event_scheduler.register_mark(event_id, mid);
                }
            }

            // D: ALCM / Scud / HIMARS missile strike — fire pre-selected groups at target.
            EventEffect::FireMissileStrike { event_id, side, shooter_gids, target_pos } => {
                let land = match Land::singleton(lua) {
                    Ok(l) => l,
                    Err(e) => { error!("FireMissileStrike: Land singleton: {e}"); continue; }
                };
                let alt = land.get_height(LuaVec2(target_pos)).unwrap_or(0.);
                let fire_task = Task::FireAtPoint {
                    point: LuaVec2(target_pos),
                    radius: Some(500.0),
                    expend_qty: None,
                    weapon_type: None,
                    altitude: Some(alt),
                    altitude_type: Some(AltType::BARO),
                };

                let mut fired = 0u32;
                for gid in &shooter_gids {
                    let group = match ctx.db.persisted.groups.get(gid) {
                        Some(g) => g,
                        None => continue,
                    };
                    let alive = group.units.into_iter().any(|uid| {
                        ctx.db.persisted.units.get(uid).map(|u| !u.dead).unwrap_or(false)
                    });
                    if !alive { continue; }
                    let group_name = group.name.clone();
                    if let Ok(dcs_group) = Group::get_by_name(lua, group_name.as_str()) {
                        if let Ok(controller) = dcs_group.get_controller() {
                            if let Err(e) = controller.set_task(fire_task.clone()) {
                                error!("FireMissileStrike: set_task {group_name}: {e}");
                            } else {
                                fired += 1;
                                info!("FireMissileStrike: {:?} group {group_name} launching at {:?}", side, target_pos);
                            }
                        }
                    }
                }

                if fired > 0 {
                    let mid = dcso3::trigger::MarkId::new();
                    ctx.db.ephemeral.msgs().circle_to_all(
                        SideFilter::All,
                        mid,
                        CircleSpec {
                            center: dcso3::LuaVec3(dcso3::Vector3::new(target_pos.x, 0., target_pos.y)),
                            radius: 3000.,
                            color: side_color(side),
                            fill_color: Color::new(0., 0., 0., 0.),
                            line_type: LineType::Dashed,
                            read_only: true,
                        },
                        Some(format_compact!("Missile Strike [{:?}] — {} launchers firing", side, fired).into()),
                    );
                    ctx.event_scheduler.register_mark(event_id, mid);
                }
            }

            // E: Spawn ambush force near convoy position and issue attack order.
            EventEffect::SpawnAmbush { event_id, ambush_side, spawn_pos, source_objective, convoy_group_id, convoy_pos } => {
                let template = find_ground_template(&ctx.db, source_objective, ambush_side);
                let template = match template {
                    Some(t) => t,
                    None => {
                        info!("SpawnAmbush: no suitable template for {:?} at {:?}", ambush_side, source_objective);
                        continue;
                    }
                };
                match ctx.db.add_and_queue_group(
                    &spctx,
                    &ctx.idx,
                    ambush_side,
                    SpawnLoc::AtPos {
                        pos: spawn_pos,
                        offset_direction: dcso3::Vector2::new(1., 0.),
                        group_heading: 0.,
                    },
                    &template,
                    DeployKind::Objective { origin: source_objective },
                    BitFlags::empty(),
                    None,
                ) {
                    Ok(gid) => {
                        info!("SpawnAmbush: spawned {:?} for {:?}", gid, ambush_side);
                        ctx.event_scheduler.ambush_groups.insert(event_id, gid);

                        // Issue AttackGroup toward the convoy. The ambush group is not in DCS
                        // yet (spawn queue lag), so queue a move toward the convoy's last position
                        // as a fallback — the pending_moves system will retry until it appears.
                        // Try to get the convoy group name directly; if it works, AttackGroup
                        // is more accurate as it tracks the moving convoy.
                        let convoy_group_name = ctx.db.persisted.groups.get(&convoy_group_id)
                            .map(|g| g.name.clone());
                        if let Some(ref _name) = convoy_group_name {
                            // Queue a move to the convoy's last known position; the ambush
                            // group will intercept when it arrives and engage via its ROE.
                            ctx.event_scheduler.pending_moves.insert(gid, vec![convoy_pos]);
                        } else {
                            ctx.event_scheduler.pending_moves.insert(gid, vec![convoy_pos]);
                        }
                    }
                    Err(e) => error!("SpawnAmbush: {e:?}"),
                }
                // F10 warning mark
                let mid = dcso3::trigger::MarkId::new();
                ctx.db.ephemeral.msgs().circle_to_all(
                    SideFilter::All,
                    mid,
                    CircleSpec {
                        center: dcso3::LuaVec3(dcso3::Vector3::new(spawn_pos.x, 0., spawn_pos.y)),
                        radius: 1500.,
                        color: side_color(ambush_side),
                        fill_color: Color::new(0., 0., 0., 0.),
                        line_type: LineType::Dashed,
                        read_only: true,
                    },
                    Some(format_compact!("AMBUSH [{:?}]", ambush_side).into()),
                );
                ctx.event_scheduler.register_mark(event_id, mid);
            }

            EventEffect::DeleteMarks { ids } => {
                for id in ids {
                    ctx.db.ephemeral.msgs().delete_mark(id);
                }
            }

            // E: Spawn a CAP aircraft patrol over/near an objective
            EventEffect::SpawnCap { event_id, cap_side, objective, obj_pos } => {
                let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
                // Find a Fighter/Attackers template for this side from existing groups
                // Template is determined from config (see fallback below).
                let template: Option<dcso3::String> = None;
                // Fall back to configured CAP template name
                let template = template.unwrap_or_else(|| {
                    let cap_tmpl = match cap_side {
                        dcso3::coalition::Side::Red => cfg.campaign_events.as_ref()
                            .map(|c| c.cap_template_red.as_str()).unwrap_or("RCAP"),
                        dcso3::coalition::Side::Blue => cfg.campaign_events.as_ref()
                            .map(|c| c.cap_template_blue.as_str()).unwrap_or("BCAP"),
                        dcso3::coalition::Side::Neutral => "RCAP",
                    };
                    dcso3::String::from(cap_tmpl)
                });
                // Offset spawn slightly from objective center to avoid ground collisions
                let spawn_pos = obj_pos + dcso3::Vector2::new(0., 2000.);
                match ctx.db.add_and_queue_group(
                    &spctx,
                    &ctx.idx,
                    cap_side,
                    SpawnLoc::AtPos {
                        pos: spawn_pos,
                        offset_direction: dcso3::Vector2::new(0., 1.),
                        group_heading: 0.,
                    },
                    &template,
                    DeployKind::Objective { origin: objective },
                    BitFlags::empty(),
                    None,
                ) {
                    Ok(gid) => {
                        info!("SpawnCap: spawned CAP {:?} for {:?} over {:?}", gid, cap_side, objective);
                        ctx.event_scheduler.cap_groups
                            .entry(event_id)
                            .or_default()
                            .push(gid);
                        // Record which side owns this CAP event (needed for retargeting).
                        ctx.event_scheduler.cap_side_by_event.insert(event_id, cap_side);
                        // Queue initial task — deferred until DCS reports the group alive.
                        ctx.event_scheduler.pending_cap_tasks.insert(gid, obj_pos);
                        // F10 mark so players can see the CAP threat
                        let enemy = match cap_side {
                            dcso3::coalition::Side::Red => dcso3::coalition::Side::Blue,
                            dcso3::coalition::Side::Blue => dcso3::coalition::Side::Red,
                            s => s,
                        };
                        let mid = dcso3::trigger::MarkId::new();
                        ctx.db.ephemeral.msgs().circle_to_all(
                            match enemy {
                                dcso3::coalition::Side::Red => SideFilter::Red,
                                dcso3::coalition::Side::Blue => SideFilter::Blue,
                                _ => SideFilter::All,
                            },
                            mid,
                            CircleSpec {
                                center: dcso3::LuaVec3(dcso3::Vector3::new(obj_pos.x, 5000., obj_pos.y)),
                                radius: 25_000.,
                                color: side_color(cap_side),
                                fill_color: Color::new(0., 0., 0., 0.),
                                line_type: LineType::Solid,
                                read_only: true,
                            },
                            Some(format_compact!("Enemy CAP [{:?}] — ACTIVE", cap_side).into()),
                        );
                        ctx.event_scheduler.register_mark(event_id, mid);
                    }
                    Err(e) => {
                        warn!(
                            "SpawnCap: CAP template '{}' not found — add a group named '{}' to your mission file. Cancelling event. ({e:?})",
                            template, template
                        );
                        // Cancel the event so it doesn't keep retrying
                        ctx.event_scheduler.active_events.retain(|ev| ev.id() != event_id);
                        // Clean up any F10 marks that may have been placed
                        if let Some(marks) = ctx.event_scheduler.event_marks.remove(&event_id) {
                            for mid in marks {
                                ctx.db.ephemeral.msgs().delete_mark(mid);
                            }
                        }
                    }
                }
            }

            EventEffect::DespawnHvt { event_id, gid } => {
                // Resolve gid: prefer the value passed from the persisted event, fall back
                // to the in-session map (populated in the same tick the group spawned).
                let resolved = gid.or_else(|| ctx.event_scheduler.hvt_group_by_event.remove(&event_id));
                if let Some(gid) = resolved {
                    ctx.event_scheduler.hvt_group_by_event.remove(&event_id);
                    ctx.event_scheduler.hvt_groups.remove(&gid);
                    ctx.event_scheduler.pending_moves.remove(&gid);
                    if let Err(e) = ctx.db.delete_group(&gid) {
                        error!("DespawnHvt: could not delete group {:?}: {e:?}", gid);
                    } else {
                        info!("DespawnHvt: HVT event {:?} expired, unit removed", event_id);
                    }
                } else {
                    warn!("DespawnHvt: no group found for event {:?}", event_id);
                }
            }

            EventEffect::DespawnAmbush { event_id } => {
                if let Some(gid) = ctx.event_scheduler.ambush_groups.remove(&event_id) {
                    if let Err(e) = ctx.db.delete_group(&gid) {
                        error!("DespawnAmbush: could not delete group {:?}: {e:?}", gid);
                    } else {
                        info!("DespawnAmbush: ambush event {:?} expired, units removed", event_id);
                    }
                }
            }

            // E: Remove all CAP groups when the event expires
            EventEffect::DespawnCap { event_id } => {
                let now = Utc::now();
                // Determine if this was a shootdown (all aircraft dead) or natural expiry.
                // If shot down, record the time so check_air_threats can enforce a cooldown.
                let was_shot_down = ctx.event_scheduler.cap_groups.get(&event_id)
                    .map(|gids| {
                        gids.iter().all(|gid| {
                            // group_health returns (alive_count, total_count)
                            ctx.db.group_health(gid)
                                .map(|(alive, _)| alive == 0)
                                .unwrap_or(true) // missing group = dead
                        })
                    })
                    .unwrap_or(false); // no groups registered = natural expiry

                if was_shot_down {
                    // Look up the (side, objective) for this reactive CAP event.
                    if let Some((side, oid)) = ctx.event_scheduler.reactive_cap_objective.remove(&event_id) {
                        ctx.event_scheduler.last_reactive_cap_died_at.insert((side, oid), now);
                        info!(
                            "DespawnCap: {:?} reactive CAP at {:?} was shot down \
                             — respawn cooldown started",
                            side, oid
                        );
                    }
                } else {
                    // Clean up the objective registration (normal expiry, no cooldown).
                    ctx.event_scheduler.reactive_cap_objective.remove(&event_id);
                }

                if let Some(gids) = ctx.event_scheduler.cap_groups.remove(&event_id) {
                    for gid in gids {
                        if let Err(e) = ctx.db.delete_group(&gid) {
                            error!("DespawnCap: could not delete group {:?}: {e:?}", gid);
                        }
                    }
                }
                info!("DespawnCap: CAP event {:?} expired, aircraft removed", event_id);
            }


        }
    }
}

/// Build a multi-hop escape route for an HVT through TOWN_ trigger zones,
/// chaining up to `max_hops` nearest unvisited zones starting from `start_pos`.
fn build_hvt_town_route(spctx: &SpawnCtx, start_pos: dcso3::Vector2, max_hops: usize) -> Vec<dcso3::Vector2> {
    let all_towns = spctx.town_zone_positions();
    if all_towns.is_empty() {
        return vec![];
    }
    let mut route = Vec::with_capacity(max_hops);
    let mut remaining: Vec<dcso3::Vector2> = all_towns;
    let mut current = start_pos;
    for _ in 0..max_hops {
        let nearest = remaining
            .iter()
            .enumerate()
            .min_by(|(_, a), (_, b)| {
                let da = na::distance(&na::Point2::from(**a), &na::Point2::from(current));
                let db = na::distance(&na::Point2::from(**b), &na::Point2::from(current));
                da.partial_cmp(&db).unwrap_or(std::cmp::Ordering::Equal)
            })
            .map(|(i, &p)| (i, p));
        if let Some((i, pos)) = nearest {
            route.push(pos);
            current = pos;
            remaining.swap_remove(i);
        } else {
            break;
        }
    }
    route
}

/// Retry deferred move orders each slow tick until the group appears in DCS.
fn flush_pending_moves(lua: MizLua, ctx: &mut Context) {
    use dcso3::controller::{ActionTyp, AltType, MissionPoint, PointType, Task, VehicleFormation};
    use dcso3::group::Group;
    use dcso3::land::Land;
    use dcso3::LuaVec2;

    let land = match Land::singleton(lua) {
        Ok(l) => l,
        Err(e) => { error!("flush_pending_moves: Land singleton: {e}"); return; }
    };

    // Process at most one pending move per tick to avoid stalling DCS Lua.
    let pending: Vec<_> = ctx.event_scheduler.pending_moves.iter()
        .map(|(gid, route)| (*gid, route.clone()))
        .take(1)
        .collect();

    for (gid, target_route) in pending {
        let group_name = match ctx.db.persisted.groups.get(&gid) {
            Some(g) => g.name.clone(),
            None => {
                // Group was deleted; remove from pending
                ctx.event_scheduler.pending_moves.remove(&gid);
                continue;
            }
        };
        if target_route.is_empty() {
            ctx.event_scheduler.pending_moves.remove(&gid);
            continue;
        }
        let dcs_group = match Group::get_by_name(lua, group_name.as_str()) {
            Ok(g) => g,
            Err(_) => continue, // Not in DCS yet — try next tick
        };
        let controller = match dcs_group.get_controller() {
            Ok(c) => c,
            Err(e) => {
                error!("flush_pending_moves: get_controller for {group_name}: {e}");
                ctx.event_scheduler.pending_moves.remove(&gid);
                continue;
            }
        };
        let mut route_points = Vec::with_capacity(target_route.len());
        for wp in &target_route {
            let alt = land.get_height(LuaVec2(*wp)).unwrap_or(0.);
            route_points.push(MissionPoint {
                typ: PointType::TurningPoint,
                airdrome_id: None,
                time_re_fu_ar: None,
                helipad: None,
                link_unit: None,
                action: Some(ActionTyp::Ground(VehicleFormation::OnRoad)),
                pos: LuaVec2(*wp),
                alt,
                alt_typ: Some(AltType::BARO),
                speed: 10.,
                speed_locked: Some(false),
                eta: None,
                eta_locked: None,
                name: None,
                task: Box::new(Task::Hold),
            });
        }
        let task = Task::Mission {
            airborne: Some(false),
            route: route_points,
        };
        let target_pos = target_route.last().copied().unwrap_or_default();
        if let Err(e) = controller.set_task(task) {
            error!("flush_pending_moves: set_task for {group_name}: {e}");
        } else {
            info!("flush_pending_moves: ordered {group_name} to move ({} waypoints) → {:?}",
                  target_route.len(), target_pos);
        }
        // Order issued (success or terminal failure) — remove from pending
        ctx.event_scheduler.pending_moves.remove(&gid);
    }

    // Flush pending CAP orbit tasks (one per tick, same retry pattern).
    let pending_cap: Vec<_> = ctx.event_scheduler.pending_cap_tasks.iter()
        .map(|(gid, pos)| (*gid, *pos))
        .take(1)
        .collect();

    for (gid, orbit_center) in pending_cap {
        use dcso3::controller::Task;
        use dcso3::attribute::Attribute;
        let group_name = match ctx.db.persisted.groups.get(&gid) {
            Some(g) => g.name.clone(),
            None => {
                ctx.event_scheduler.pending_cap_tasks.remove(&gid);
                continue;
            }
        };
        let dcs_group = match Group::get_by_name(lua, group_name.as_str()) {
            Ok(g) => g,
            Err(_) => continue, // Not in DCS yet — retry next tick
        };
        let controller = match dcs_group.get_controller() {
            Ok(c) => c,
            Err(e) => {
                error!("flush_pending_cap_tasks: get_controller {group_name}: {e}");
                ctx.event_scheduler.pending_cap_tasks.remove(&gid);
                continue;
            }
        };
        // Initial task: broad hunt centred on the spawn position.
        // Dynamic retargeting each slow tick will redirect to actual enemy positions.
        let hunt = Task::EngageTargetsInZone {
            point: LuaVec2(orbit_center),
            zone_radius: 200_000.0, // 200 km — effectively "hunt everywhere nearby"
            target_types: vec![Attribute::Air],
            priority: None,
        };
        if let Err(e) = controller.set_task(hunt) {
            error!("flush_pending_cap_tasks: set_task {group_name}: {e}");
        } else {
            info!("flush_pending_cap_tasks: CAP {group_name} initial hunt from {:?}", orbit_center);
        }
        ctx.event_scheduler.pending_cap_tasks.remove(&gid);
    }

}

/// Each slow tick, redirect all active CAP groups toward the centroid of the
/// nearest enemy aircraft cluster.  If no enemies are airborne, fall back to
/// a wide EngageTargets sweep so they don't just hover stationary.
fn retarget_cap_groups(lua: MizLua, ctx: &mut Context) {
    use dcso3::controller::Task;
    use dcso3::attribute::Attribute;
    use dcso3::coalition::Side;
    use dcso3::Vector2;
    use crate::db::events::CampaignEvent;

    // Collect enemy in-air positions per side.
    let mut enemy_positions: fxhash::FxHashMap<Side, Vec<Vector2>> =
        fxhash::FxHashMap::default();
    for (_, player) in ctx.db.persisted.players() {
        if let Some((_, Some(inst))) = &player.current_slot {
            if inst.in_air {
                let pos = Vector2::new(inst.position.p.x, inst.position.p.z);
                // Enemies of blue are red, and vice versa.
                let enemy_side = match player.side {
                    Side::Blue => Side::Red,
                    Side::Red => Side::Blue,
                    s => s,
                };
                enemy_positions.entry(enemy_side).or_default().push(pos);
            }
        }
    }

    // Build list of (event_id, cap_side) for all active CAP events.
    let cap_events: Vec<_> = ctx
        .event_scheduler
        .active_events
        .iter()
        .filter_map(|e| match e {
            CampaignEvent::EnemyCap { id, cap_side, .. }
            | CampaignEvent::CommanderCap { id, cap_side, .. } => Some((*id, *cap_side)),
            _ => None,
        })
        .collect();

    for (event_id, cap_side) in cap_events {
        let gids = match ctx.event_scheduler.cap_groups.get(&event_id) {
            Some(v) => v.clone(),
            None => continue,
        };
        // Compute centroid of enemy aircraft (enemies of cap_side).
        let target_pos: Option<Vector2> = enemy_positions
            .get(&cap_side) // positions of players that are enemies of cap_side
            .filter(|v| !v.is_empty())
            .map(|positions| {
                let n = positions.len() as f64;
                let sum = positions
                    .iter()
                    .fold(Vector2::new(0., 0.), |acc, p| {
                        Vector2::new(acc.x + p.x, acc.y + p.y)
                    });
                Vector2::new(sum.x / n, sum.y / n)
            });

        for gid in gids {
            let group_name = match ctx.db.persisted.groups.get(&gid) {
                Some(g) => g.name.clone(),
                None => continue,
            };
            let dcs_group = match dcso3::group::Group::get_by_name(lua, group_name.as_str()) {
                Ok(g) => g,
                Err(_) => continue, // not in DCS yet
            };
            let controller = match dcs_group.get_controller() {
                Ok(c) => c,
                Err(e) => {
                    error!("retarget_cap_groups: get_controller {group_name}: {e}");
                    continue;
                }
            };

            let task = match target_pos {
                Some(center) => {
                    // Push towards the enemy cluster with a 150 km zone so they
                    // actively manoeuvre rather than staying put.
                    Task::EngageTargetsInZone {
                        point: dcso3::LuaVec2(center),
                        zone_radius: 150_000.0,
                        target_types: vec![Attribute::Air],
                        priority: None,
                    }
                }
                None => {
                    // No enemies airborne — broad area sweep so they keep moving.
                    Task::EngageTargets {
                        target_types: vec![Attribute::Air],
                        max_dist: Some(200_000.0),
                        priority: None,
                    }
                }
            };

            if let Err(e) = controller.set_task(task) {
                error!("retarget_cap_groups: set_task {group_name}: {e}");
            }
        }
    }
}

/// Reactive CAP: detect in-air enemy players near owned objectives and spawn CAP intercepts.
/// CAP is no longer an economic commander action — it fires automatically when real threats appear.
fn check_air_threats(ctx: &mut Context, now: DateTime<Utc>) {
    use crate::db::events::{bearing_to_compass, CampaignEvent, EventId};
    use dcso3::coalition::Side;
    use dcso3::Vector2;

    let events_cfg = match ctx.db.ephemeral.cfg.campaign_events.as_ref() {
        Some(c) if c.enabled && c.enemy_cap_enabled => c.clone(),
        _ => return,
    };

    let cluster_radius_sq = events_cfg.cap_trigger_radius_m.powi(2);
    let max_concurrent = events_cfg.cap_max_concurrent;
    let max_per_side = events_cfg.cap_max_per_side;
    let min_threat = events_cfg.cap_min_threat_count as usize;
    let respawn_cooldown = chrono::Duration::seconds(events_cfg.cap_respawn_cooldown_secs as i64);

    // Count active EnemyCap events per side — CommanderCap is excluded (it's friendly support).
    let mut active_cap_red: fxhash::FxHashSet<bfprotocols::db::objective::ObjectiveId> =
        fxhash::FxHashSet::default();
    let mut active_cap_blue: fxhash::FxHashSet<bfprotocols::db::objective::ObjectiveId> =
        fxhash::FxHashSet::default();
    for e in &ctx.event_scheduler.active_events {
        if let CampaignEvent::EnemyCap { objective, cap_side, .. } = e {
            match cap_side {
                Side::Red  => { active_cap_red.insert(*objective); }
                Side::Blue => { active_cap_blue.insert(*objective); }
                _ => {}
            }
        }
    }

    let total_active = active_cap_red.len() + active_cap_blue.len();
    if total_active >= max_concurrent {
        return; // global cap reached
    }
    if active_cap_red.len() >= max_per_side && active_cap_blue.len() >= max_per_side {
        return; // both sides at per-side limit
    }

    // Compute battlefield centroid for compass reporting.
    let (centroid_sum, centroid_n) = ctx
        .db
        .persisted
        .objectives
        .into_iter()
        .filter(|(_, o)| o.owner() != Side::Neutral)
        .fold((Vector2::new(0., 0.), 0usize), |(acc, n), (_, o)| {
            let p = o.pos();
            (Vector2::new(acc.x + p.x, acc.y + p.y), n + 1)
        });
    let centroid = if centroid_n == 0 {
        Vector2::new(0., 0.)
    } else {
        Vector2::new(centroid_sum.x / centroid_n as f64, centroid_sum.y / centroid_n as f64)
    };

    // For each side that still has room for more CAP, check EWR detections and cluster them.
    // We iterate Red-defends-against-Blue and Blue-defends-against-Red.
    for defending_side in [Side::Red, Side::Blue] {
        let side_active_count = match defending_side {
            Side::Red  => active_cap_red.len(),
            Side::Blue => active_cap_blue.len(),
            _ => continue,
        };
        if side_active_count >= max_per_side {
            continue; // this side is already at its per-side limit
        }
        if active_cap_red.len() + active_cap_blue.len() >= max_concurrent {
            break; // global cap hit mid-loop
        }

        // ── Step 1: gate on detected fixed-wing PLAYER count ─────────────────────
        // Only enemy PLAYERS in fixed-wing aircraft trigger reactive CAP.
        // AI aircraft, scouts, and helicopter players are excluded:
        //   - helicopters are low-altitude threats handled by SAMs
        //   - AI aircraft (e.g. transports) should not trigger CAP
        // Require at least `min_threat` (default: 2) qualifying player contacts.
        let player_fw_count =
            ctx.ewr.detected_enemy_fixedwing_player_count(defending_side, now, &ctx.db);
        if player_fw_count < min_threat {
            debug!(
                "Reactive CAP: {:?} side has only {} fixed-wing player contact(s) — below threshold {}, skipping",
                defending_side, player_fw_count, min_threat
            );
            continue; // not enough real fixed-wing players to justify CAP
        }

        // ── Step 2: get EWR-detected enemy positions for cluster geometry ────────
        // ALL detected aircraft (players + AI) are used for spatial clustering
        // so the CAP is placed nearest the actual incursion area, but only
        // PLAYER fixed-wing count was used to decide *whether* to scramble.
        let detected = ctx.ewr.detected_enemy_positions(defending_side, now);
        if detected.is_empty() {
            continue; // no radar contacts → no CAP (shouldn't happen if player count > 0)
        }

        // ── Step 3 (was 2): greedy spatial clustering ─────────────────────────────
        // Group the detected contacts: if two contacts are within cap_trigger_radius_m
        // of each other they belong to the same incursion. One CAP handles one cluster.
        // This prevents 5 aircraft spread over 3 objectives from spawning 3 CAP flights.
        let mut assigned = vec![false; detected.len()];
        let mut clusters: Vec<(Vector2, usize)> = Vec::new(); // (centroid, count)
        for i in 0..detected.len() {
            if assigned[i] { continue; }
            assigned[i] = true;
            let mut cx = detected[i].x;
            let mut cz = detected[i].y;
            let mut count = 1usize;
            for j in (i + 1)..detected.len() {
                if assigned[j] { continue; }
                let dx = detected[j].x - detected[i].x;
                let dz = detected[j].y - detected[i].y;
                if dx * dx + dz * dz <= cluster_radius_sq {
                    assigned[j] = true;
                    cx += detected[j].x;
                    cz += detected[j].y;
                    count += 1;
                }
            }
            clusters.push((Vector2::new(cx / count as f64, cz / count as f64), count));
        }

        // ── Step 4 (was 3): filter clusters below the minimum threat threshold ─────
        // Even though we already checked player_fw_count, filter any cluster whose
        // raw position count is below min_threat (edge case: positions from AI only).
        clusters.retain(|(_, count)| *count >= min_threat);
        if clusters.is_empty() {
            continue;
        }

        // Sort clusters largest → smallest so the most dangerous incursion gets covered first.
        clusters.sort_unstable_by(|a, b| b.1.cmp(&a.1));

        // ── Step 5 (was 4): for each cluster find the nearest friendly objective ───
        let slots_remaining = (max_per_side - side_active_count)
            .min(max_concurrent - (active_cap_red.len() + active_cap_blue.len()));

        for (cluster_center, cluster_count) in clusters.into_iter().take(slots_remaining) {
            // Find the closest friendly AIRBASE or FARP that doesn't already have active CAP.
            // CAP cannot spawn from FOBs, logistics hubs, factories, etc. — only from
            // objectives that have a runway or FARP pad. Neutral objectives are excluded
            // (obj.owner() == defending_side already ensures non-neutral).
            let best_obj = ctx.db.persisted.objectives.into_iter()
                .filter(|(oid, obj)| {
                    obj.owner() == defending_side
                        && (obj.is_airbase() || obj.is_farp())
                        && {
                            let already = match defending_side {
                                Side::Red  => active_cap_red.contains(oid),
                                Side::Blue => active_cap_blue.contains(oid),
                                _ => true,
                            };
                            !already
                        }
                })
                .min_by(|(_, a), (_, b)| {
                    let ap = a.pos();
                    let bp = b.pos();
                    let da = {
                        let dx = ap.x - cluster_center.x;
                        let dy = ap.y - cluster_center.y;
                        dx * dx + dy * dy
                    };
                    let db = {
                        let dx = bp.x - cluster_center.x;
                        let dy = bp.y - cluster_center.y;
                        dx * dx + dy * dy
                    };
                    da.partial_cmp(&db).unwrap_or(std::cmp::Ordering::Equal)
                });

            let (oid, obj) = match best_obj {
                Some(o) => o,
                None => {
                    debug!(
                        "Reactive CAP: no friendly airbase/FARP available for {:?} — skipping cluster",
                        defending_side
                    );
                    continue;
                }
            };

            // ── Step 6: respawn cooldown check ───────────────────────────────────
            // If the previous reactive CAP for this (side, objective) was destroyed
            // (not just expired), enforce a cool-down before re-scrambling.
            let cooldown_key = (defending_side, *oid);
            if let Some(&died_at) = ctx.event_scheduler.last_reactive_cap_died_at.get(&cooldown_key) {
                if now - died_at < respawn_cooldown {
                    let secs_remaining = (respawn_cooldown - (now - died_at)).num_seconds();
                    debug!(
                        "Reactive CAP: {:?} cooldown active for {:?} — {}s remaining, skipping",
                        defending_side, oid, secs_remaining
                    );
                    continue;
                }
            }

            let _obj_pos = obj.pos();
            let obj_name = dcso3::String::from(obj.name.as_str());
            let direction = bearing_to_compass(centroid, cluster_center);
            let attacking_side = match defending_side {
                Side::Red  => Side::Blue,
                Side::Blue => Side::Red,
                _ => continue,
            };

            let event_id = EventId::new();
            let event = CampaignEvent::EnemyCap {
                id: event_id,
                cap_side: defending_side,
                objective: *oid,
                expires_at: now + chrono::Duration::seconds(events_cfg.cap_duration_secs as i64),
                spawned: false,
            };
            ctx.event_scheduler.active_events.push(event);
            ctx.event_scheduler.total_events_spawned += 1;
            // Register the (side, objective) so DespawnCap can update the cooldown
            // if the CAP aircraft are shot down before the event naturally expires.
            ctx.event_scheduler.reactive_cap_objective.insert(event_id, (defending_side, *oid));

            // Track locally so this loop iteration's later clusters don't double-book.
            match defending_side {
                Side::Red  => { active_cap_red.insert(*oid); }
                Side::Blue => { active_cap_blue.insert(*oid); }
                _ => {}
            }

            info!(
                "Reactive CAP: {:?} scrambled over {} — EWR cluster of {} contacts to the {}",
                defending_side, obj_name, cluster_count, direction
            );

            ctx.db.ephemeral.msgs().panel_to_side(
                20,
                false,
                attacking_side,
                format_compact!(
                    "⚠ THREAT: Enemy CAP scrambled to the {} — {} aircraft detected by EWR!",
                    direction,
                    cluster_count
                ),
            );
        }
    }
}


/// Check all deployed SF teams for HVT capture and extraction timeout.
fn check_sf_missions(ctx: &mut Context) {
    use bfprotocols::db::group::GroupId;
    use dcso3::trigger::{CircleSpec, LineType, SideFilter};
    use crate::db::ephemeral::SfPhase;
    use crate::db::events::CampaignEvent;
    use crate::db::objective::ObjGroupClass;
    let now = Utc::now();
    let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
    let ev_cfg = match cfg.campaign_events.as_ref() {
        Some(c) => c,
        None => return,
    };
    let capture_radius_sq = ev_cfg.hvt_capture_radius_m.powi(2);
    let timeout_dur = chrono::Duration::seconds(ev_cfg.hvt_extraction_timeout_secs as i64);

    // Collect which SF teams need action this tick
    let mut to_capture: Vec<GroupId> = Vec::new();
    let mut to_timeout: Vec<GroupId> = Vec::new();

    for (gid, mission) in &ctx.db.ephemeral.sf_missions {
        match mission.phase {
            SfPhase::MovingToHvt => {
                // Resolve the HVT's CURRENT position from the live group state, not the
                // original spawn point — the HVT vehicle may have driven away since spawning.
                let live_hvt_pos = ctx.event_scheduler.hvt_groups
                    .iter()
                    .find(|(_, (eid, _))| *eid == mission.event_id)
                    .and_then(|(hvt_gid, _)| {
                        ctx.db.persisted.groups.get(hvt_gid).and_then(|g| {
                            g.units.into_iter()
                                .find_map(|uid| ctx.db.persisted.units.get(uid).map(|u| u.pos))
                        })
                    })
                    .unwrap_or(mission.hvt_pos); // fall back to spawn pos if unit gone

                // Re-issue move order every tick so the SF team chases the HVT as it moves
                ctx.event_scheduler.pending_moves.insert(*gid, vec![live_hvt_pos]);

                // Check if SF group has reached the HVT's live position
                let sf_pos = ctx.db.persisted.groups.get(gid).and_then(|g| {
                    g.units.into_iter()
                        .find_map(|uid| ctx.db.persisted.units.get(uid).map(|u| u.pos))
                });
                if let Some(pos) = sf_pos {
                    let dx = pos.x - live_hvt_pos.x;
                    let dy = pos.y - live_hvt_pos.y;
                    if dx * dx + dy * dy <= capture_radius_sq {
                        to_capture.push(*gid);
                    }
                }
            }
            SfPhase::Captured => {
                // Check extraction timeout
                if let Some(cap_at) = mission.captured_at {
                    if now - cap_at >= timeout_dur {
                        to_timeout.push(*gid);
                    }
                }
            }
        }
    }

    // Process captures
    for gid in to_capture {
        let mission = match ctx.db.ephemeral.sf_missions.get_mut(&gid) {
            Some(m) => m,
            None => continue,
        };
        mission.phase = SfPhase::Captured;
        mission.captured_at = Some(now);

        let side = mission.side;
        let drop_pos = mission.drop_pos;
        let event_id = mission.event_id;

        // Despawn the HVT groups for this event
        let hvt_gids: Vec<GroupId> = ctx
            .event_scheduler
            .hvt_groups
            .iter()
            .filter(|(_, (eid, _))| *eid == event_id)
            .map(|(g, _)| *g)
            .collect();
        for hvt_gid in hvt_gids {
            ctx.event_scheduler.hvt_groups.remove(&hvt_gid);
            if let Err(e) = ctx.db.delete_group(&hvt_gid) {
                error!("SF capture: failed to delete HVT group {:?}: {e:?}", hvt_gid);
            }
        }
        // Delete HVT map marks
        if let Some(marks) = ctx.event_scheduler.event_marks.remove(&event_id) {
            for mid in marks {
                ctx.db.ephemeral.msgs().delete_mark(mid);
            }
        }

        // Order SF team to retreat back to drop position for extraction
        ctx.event_scheduler.pending_moves.insert(gid, vec![drop_pos]);

        // Add F10 extraction mark at SF current position
        let sf_pos = ctx.db.persisted.groups.get(&gid).and_then(|g| {
            g.units.into_iter()
                .find_map(|uid| ctx.db.persisted.units.get(uid).map(|u| u.pos))
        });
        if let Some(sf_pos) = sf_pos {
            let mid = dcso3::trigger::MarkId::new();
            let side_filter = match side {
                dcso3::coalition::Side::Red => SideFilter::Red,
                dcso3::coalition::Side::Blue => SideFilter::Blue,
                _ => SideFilter::All,
            };
            ctx.db.ephemeral.msgs().circle_to_all(
                side_filter,
                mid,
                CircleSpec {
                    center: LuaVec3(Vector3::new(sf_pos.x, 0., sf_pos.y)),
                    radius: 500.,
                    color: dcso3::Color::new(0.1, 1., 0.1, 1.),
                    fill_color: dcso3::Color::new(0., 1., 0., 0.1),
                    line_type: LineType::Dashed,
                    read_only: true,
                },
                Some(format_compact!("⬇ SF TEAM — HVT SECURED — EXTRACT NOW!").into()),
            );
            ctx.event_scheduler.register_mark(event_id, mid);
        }

        info!("SF team {:?} captured HVT {:?} — retreating for extraction", gid, event_id);
        ctx.db.ephemeral.msgs().panel_to_side(
            45,
            false,
            side,
            format_compact!("INTEL: Special Forces have secured the HVT! Pilot — return and extract your team!"),
        );

        // Generate intel report: find the enemy objective this HVT was associated with
        // and enumerate its deployed groups by class.
        let enemy_obj_id = ctx.event_scheduler.active_events.iter().find_map(|e| {
            if let CampaignEvent::HighValueTarget { id, objective, .. } = e {
                if *id == event_id { Some(*objective) } else { None }
            } else {
                None
            }
        });
        if let Some(oid) = enemy_obj_id {
            if let Some(obj) = ctx.db.persisted.objectives.get(&oid) {
                let enemy_side = side.opposite();
                let obj_pos = obj.pos();
                let obj_name = CompactString::from(obj.name());
                let mut armor = 0u32;
                let mut aaa = 0u32;
                let mut lr = 0u32;
                let mut mr = 0u32;
                let mut sr = 0u32;
                let mut other = 0u32;
                if let Some(gids) = obj.groups().get(&enemy_side) {
                    for gid in gids {
                        if let Some(g) = ctx.db.persisted.groups.get(gid) {
                            match g.class {
                                ObjGroupClass::Armor => armor += 1,
                                ObjGroupClass::Aaa => aaa += 1,
                                ObjGroupClass::Lr => lr += 1,
                                ObjGroupClass::Mr => mr += 1,
                                ObjGroupClass::Sr => sr += 1,
                                ObjGroupClass::Logi
                                | ObjGroupClass::Services
                                | ObjGroupClass::Naval
                                | ObjGroupClass::Infantry
                                | ObjGroupClass::Other => other += 1,
                            }
                        }
                    }
                }
                let mut mark_text = format_compact!(
                    "INTEL: {} [{}]\n",
                    obj_name,
                    enemy_side
                );
                if armor > 0 { mark_text.push_str(&format_compact!("Armor: {}\n", armor)); }
                if aaa > 0   { mark_text.push_str(&format_compact!("AAA/SAM: {}\n", aaa)); }
                if lr > 0    { mark_text.push_str(&format_compact!("LR SAM: {}\n", lr)); }
                if mr > 0    { mark_text.push_str(&format_compact!("MR SAM: {}\n", mr)); }
                if sr > 0    { mark_text.push_str(&format_compact!("SR SAM: {}\n", sr)); }
                if other > 0 { mark_text.push_str(&format_compact!("Other: {}\n", other)); }
                if armor == 0 && aaa == 0 && lr == 0 && mr == 0 && sr == 0 && other == 0 {
                    mark_text.push_str("No active units detected.\n");
                }
                let mid = ctx.db.ephemeral.msgs().mark_to_side(
                    side,
                    obj_pos,
                    true,
                    mark_text.as_str(),
                );
                ctx.event_scheduler.register_mark(event_id, mid);
                ctx.db.ephemeral.msgs().panel_to_side(60, false, side, mark_text);
            }
        }
    }

    // Process extraction timeouts — warn pilot; if still no extract, remove mission
    for gid in to_timeout {
        let mission = match ctx.db.ephemeral.sf_missions.get(&gid) {
            Some(m) => m,
            None => continue,
        };
        let side = mission.side;
        ctx.db.ephemeral.msgs().panel_to_side(
            20,
            false,
            side,
            format_compact!("⚠ SF team extraction window has passed — the team is retreating. Hurry!"),
        );
        // Mission stays active but team retreats on its own
    }
}

fn side_color(side: dcso3::coalition::Side) -> dcso3::Color {
    match side {
        dcso3::coalition::Side::Blue => dcso3::Color::blue(1.),
        dcso3::coalition::Side::Red => dcso3::Color::red(1.),
        dcso3::coalition::Side::Neutral => dcso3::Color::white(1.),
    }
}

fn remove_junk_periodic(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    use dcso3::world::{SearchVolume, World};
    use dcso3::LuaVec3;

    let cfg = match ctx.db.ephemeral.cfg.campaign_events.as_ref() {
        Some(c) if c.enabled && c.junk_removal_interval_secs > 0 => c.clone(),
        _ => return,
    };

    let elapsed = (now - ctx.last_junk_removal).num_seconds();
    if elapsed < cfg.junk_removal_interval_secs as i64 {
        return;
    }
    ctx.last_junk_removal = now;

    let world = match World::singleton(lua) {
        Ok(w) => w,
        Err(e) => { error!("remove_junk: World singleton: {e}"); return; }
    };
    let volume = SearchVolume::Sphere {
        point: LuaVec3(dcso3::Vector3::new(0., 0., 0.)),
        radius: cfg.junk_removal_radius_m,
    };
    match world.remove_junk(volume) {
        Ok(n) => { if n > 0 { info!("remove_junk: removed {} objects", n); } }
        Err(e) => error!("remove_junk: {e}"),
    }
}

fn run_slow_timed_events(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    path: &PathBuf,
    ts: DateTime<Utc>,
) -> Result<AdminResult> {
    let freq = Duration::seconds(ctx.db.ephemeral.cfg.slow_timed_events_freq as i64);
    if ts - ctx.last_slow_timed_events >= freq {
        let start_ts = Utc::now();
        ctx.last_slow_timed_events = start_ts;
        match check_auto_shutdown(ctx, lua, ts) {
            Ok(AdminResult::Continue) => (),
            Ok(AdminResult::Shutdown) => return Ok(AdminResult::Shutdown),
            Err(e) => error!("failed to check for auto shutdown {e:?}"),
        }
        for (oid, vh) in ctx.db.ephemeral.warehouses_to_sync() {
            if let Err(e) = ctx.db.sync_vehicle_at_obj(lua, oid, vh.clone()) {
                error!(
                    "failed to sync warehouse at objective {:?} vehicle {:?} {:?}",
                    oid, vh, e
                )
            }
        }
        return_lives(lua, ctx, ts);
        ctx.recently_born.retain(|_, ts| start_ts - *ts <= Duration::seconds(5));
        {
            // report kills
            let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
            for dead in ctx.shots_out.bring_out_your_dead(ts) {
                info!("kill {:?}", dead);
                if let Some(points) = cfg.points.as_ref() {
                    ctx.db.award_kill_points(points, &dead)
                }
                // Detect convoy interdiction
                if let bfprotocols::shots::Who::AI { gid, side, .. } = &dead.victim {
                    if let Some(convoy_info) = ctx.db.convoy_info_for_group(gid) {
                        let killer_ucid = dead.shots.iter().find_map(|s| match &s.shooter {
                            bfprotocols::shots::Who::Player { ucid, .. } => Some(*ucid),
                            _ => None,
                        });
                        info!("Convoy unit destroyed! Side: {:?}, GroupId: {:?}", side, gid);
                        ctx.do_bg_task(Task::Stat(Stat::ConvoyDestroyed {
                            from: convoy_info.0,
                            to: convoy_info.1,
                            side: *side,
                            killer: killer_ucid,
                        }));
                        // Award interdiction points to the killing player
                        if let (Some(ucid), Some(points)) = (killer_ucid, cfg.points.as_ref()) {
                            let award = points.convoy_interdiction_points as i32;
                            if award > 0 {
                                ctx.db.adjust_points(
                                    &ucid,
                                    award,
                                    "convoy interdiction",
                                );
                            }
                        }
                    }
                }
                // Detect HVT kill and award/penalize bonus points
                if let bfprotocols::shots::Who::AI { gid, .. } = &dead.victim {
                    if let Some((event_id, reward)) = ctx.event_scheduler.hvt_groups.remove(gid) {
                        // Look up the HVT's owning side and departed objective.
                        let hvt_info = ctx.event_scheduler.active_events.iter()
                            .find(|e| e.id() == event_id)
                            .and_then(|e| match e {
                                crate::db::events::CampaignEvent::HighValueTarget { side, objective, .. } => Some((*side, *objective)),
                                _ => None,
                            });
                        let killer_ucid = dead.shots.iter().find_map(|s| match &s.shooter {
                            bfprotocols::shots::Who::Player { ucid, .. } => Some(*ucid),
                            _ => None,
                        });
                        // Killing the HVT is always bad — the killer loses personal points
                        // (should have captured it) and the HVT's departed objective loses points.
                        if let Some(ucid) = killer_ucid {
                            let killer_side = ctx.db.persisted.players.get(&ucid).map(|p| p.side);
                            let msg = if hvt_info.map(|(s, _)| s) == killer_side {
                                // Friendly fire on own HVT
                                format_compact!("Friendly HVT destroyed! -{reward} points penalty")
                            } else {
                                // Enemy killed HVT instead of capturing it
                                format_compact!("HVT destroyed! -{reward} points penalty (capture for positive score)")
                            };
                            ctx.db.adjust_points(&ucid, -reward, "hvt killed (should have captured)");
                            if let Some(side) = killer_side {
                                ctx.db.ephemeral.msgs().panel_to_side(15, false, side, msg);
                            }
                        }
                        // HVT's departed objective loses points regardless of who killed it
                        if let Some((_, hvt_oid)) = hvt_info {
                            if let Some(obj) = ctx.db.persisted.objectives.get_mut_cow(&hvt_oid) {
                                obj.points = (obj.points - reward).max(0);
                                ctx.db.ephemeral.dirty();
                            }
                        }
                        // E: Revenge — enemy side launches a counter-offensive after a delay
                        let events_cfg = ctx.db.ephemeral.cfg.campaign_events.clone();
                        if let Some(ref ecfg) = events_cfg {
                            if ecfg.escalation_enabled {
                                if let Some((hvt_side, _)) = hvt_info {
                                    let trigger_at = dead.time + chrono::Duration::seconds(ecfg.revenge_delay_secs as i64);
                                    ctx.event_scheduler.schedule_revenge(hvt_side, trigger_at);
                                    ctx.db.ephemeral.msgs().panel_to_all(
                                        15, false,
                                        format_compact!("INTEL: {:?} forces will retaliate for the loss of their HVT!", hvt_side),
                                    );
                                }
                            }
                        }
                        // Remove HVT marks and expire the event
                        if let Some(marks) = ctx.event_scheduler.event_marks.remove(&event_id) {
                            for mid in marks {
                                ctx.db.ephemeral.msgs().delete_mark(mid);
                            }
                        }
                        ctx.event_scheduler.active_events.retain(|e| e.id() != event_id);
                    }
                }
                ctx.do_bg_task(Task::Stat(Stat::Kill(dead)));
            }
        }
        if let Err(e) = ctx.db.maybe_do_repairs(ts) {
            error!("error doing repairs {:?}", e)
        }
        record_perf(&mut perf.do_repairs, start_ts);

        // Process C-130 physical cargo spawn queue
        let slots: Vec<_> = ctx.db.instanced_players()
            .filter_map(|(_, player, _)| player.current_slot.as_ref().map(|(s, _)| *s))
            .collect();
        for slot in slots {
            if let Err(e) = ctx.db.process_c130_spawn_queue(lua, &ctx.idx, &slot) {
                error!("error processing C-130 spawn queue for slot {:?}: {:?}", slot, e)
            }
        }

        // Update C-130 physical crates (track airdrops and auto-unpack)
        if let Err(e) = ctx.db.update_c130_crates(lua, &ctx.idx) {
            error!("error updating C-130 crates: {:?}", e)
        }

        if let Err(e) = ctx.db.advance_actions(lua, &ctx.idx, &ctx.jtac, start_ts) {
            error!("could not advance actions {e:?}")
        }
        let ts = Utc::now();
        if let Err(e) = ctx.ewr.update_tracks(
            lua,
            &mut ctx.landcache,
            &ctx.db,
            ts,
            ctx.db.ephemeral.cfg.ewr_mode,
            ctx.db.ephemeral.cfg.ewr_delay,
        ) {
            error!("could not update ewr tracks {e}")
        }
        record_perf(&mut perf.ewr_tracks, ts);

        // ELINT/SIGINT: decay intel contacts and refresh/remove their F10 marks.
        ctx.db.ephemeral.tick_intel_decay(ts);

        let ts = Utc::now();
        if let Err(e) = generate_ewr_reports(ctx, ts) {
            error!("could not generate ewr reports {e}")
        }
        record_perf(&mut perf.ewr_reports, ts);
        let ts = Utc::now();
        match ctx.db.cull_or_respawn_objectives(lua, &mut ctx.landcache, ts) {
            Err(e) => error!("could not cull or respawn objectives {e}"),
            Ok((threatened, cleared)) => {
                for oid in threatened {
                    if ctx.db.ephemeral.threat_notified.insert(oid) {
                        let obj = ctx.db.objective(&oid)?;
                        let (owner, pos, name) = (obj.owner(), obj.pos(), obj.name().to_string());
                        ctx.db.ephemeral.on_objective_threatened(pos, owner, &name, ts);
                    }
                    // Under-attack notification with cooldown
                    let ua_cooldown = ctx.db.ephemeral.cfg.under_attack.as_ref()
                        .map(|c| c.cooldown_secs);
                    if let Some(cooldown_secs) = ua_cooldown {
                        let cooldown = chrono::Duration::seconds(cooldown_secs as i64);
                        let last = ctx.db.ephemeral.last_under_attack_notif.get(&oid).copied();
                        if last.map(|t| ts - t >= cooldown).unwrap_or(true) {
                            ctx.db.ephemeral.last_under_attack_notif.insert(oid, ts);
                            if let Ok(obj) = ctx.db.objective(&oid) {
                                let (owner, pos, name) = (obj.owner(), obj.pos(), obj.name().to_string());
                                ctx.db.ephemeral.on_objective_under_attack(pos, owner, &name, cooldown_secs as i64, ts);
                            }
                        }
                    }
                }
                let _ = cleared;
            }
        }
        // Mercy timer check
        if let Some(losing_side) = ctx.db.check_last_stand(ts) {
            ctx.db.trigger_last_stand_victory(ts, losing_side.opposite());
        }
        record_perf(&mut perf.unit_culling, ts);
        let ts = Utc::now();
        if let Err(e) = ctx.db.update_objectives_markup() {
            error!("could not remark objectives {e}")
        }
        record_perf(&mut perf.remark_objectives, ts);
        let ts = Utc::now();
        if let Err(e) = ctx.db.run_factory_production(ts) {
            error!("could not run factory production {e}")
        }
        record_perf(&mut perf.slow_timed, ts);
        let ts = Utc::now();
        match ctx.db.check_carrier_repairs(ts) {
            Ok(completed) => {
                for (oid, name) in completed {
                    if let Ok(obj) = ctx.db.objective(&oid) {
                        let owner = obj.owner();
                        let msg = format_compact!("{} has been fully repaired and is operational", name);
                        ctx.db.ephemeral.msgs().panel_to_side(15, false, owner, msg);
                    }
                }
            }
            Err(e) => error!("could not check carrier repairs {e}")
        }
        record_perf(&mut perf.slow_timed, ts);
        let ts = Utc::now();
        match ctx.db.check_carrier_group_capture(lua, ts) {
            Ok(captures) => {
                for (oid, old_owner, new_owner) in captures {
                    ctx.event_scheduler.owned_cache_dirty = true;
                    if let Ok(obj) = ctx.db.objective(&oid) {
                        let msg_old = format_compact!("{} has been captured by the enemy!", obj.name());
                        let msg_new = format_compact!("You have captured {} with its aircraft! Carrier at 50% health", obj.name());
                        ctx.db.ephemeral.msgs().panel_to_side(20, true, old_owner, msg_old);
                        ctx.db.ephemeral.msgs().panel_to_side(20, true, new_owner, msg_new);
                    }
                }
            }
            Err(e) => error!("could not check carrier captures {e}")
        }
        // Auto-repair damaged carriers near naval bases
        match ctx.db.check_carrier_auto_repair(ts) {
            Ok(messages) => {
                for (side, msg) in messages {
                    ctx.db.ephemeral.msgs().panel_to_side(15, false, side, msg);
                }
            }
            Err(e) => error!("could not check carrier auto repair {e}")
        }
        record_perf(&mut perf.slow_timed, ts);
        let ts = Utc::now();
        update_frontline(ctx, ts, false);
        record_perf(&mut perf.frontline, ts);
        let ts = Utc::now();
        ctx.db.ephemeral.update_map_layer(&ctx.db.persisted, ts);
        update_jtac_contacts(ctx, lua);
        record_perf(&mut perf.update_jtac_contacts, ts);
        let now = Utc::now();
        if let Some(snap) = ctx.db.maybe_snapshot() {
            ctx.do_bg_task(bg::Task::SaveState(path.clone(), snap));
        }
        record_perf(&mut perf.snapshot, now);
        award_periodic_points(ctx, start_ts);
        tick_smart_commander(lua, ctx, start_ts);
        record_perf(&mut perf.slow_timed, start_ts);

        // Tick campaign events — active event processing (expiry, effects, escalation).
        // New event spawning is now handled by tick_smart_commander above.
        if let Some(events_cfg) = ctx.db.ephemeral.cfg.campaign_events.as_ref() {
            if events_cfg.enabled {
                let events_cfg = events_cfg.clone();
                match ctx.event_scheduler.tick(&ctx.db, &events_cfg, start_ts) {
                    Ok((messages, effects)) => {
                        for msg in messages {
                            ctx.db.ephemeral.msgs().panel_to_all(15, false, msg);
                        }
                        // Enqueue effects; drain_event_effects applies EFFECTS_PER_TICK per tick.
                        ctx.event_scheduler.pending_effects.extend(effects);
                    }
                    Err(e) => error!("error ticking campaign events: {e:?}"),
                }
                drain_event_effects(lua, ctx);
                // Retry deferred move orders for newly-spawned groups (1 per tick max).
                flush_pending_moves(lua, ctx);
                // Reactive CAP: spawn intercepts wherever enemy aircraft are detected
                check_air_threats(ctx, start_ts);
                // Dynamic CAP retargeting: redirect active CAP groups toward enemy aircraft
                retarget_cap_groups(lua, ctx);
                // Check SF HVT capture missions (proximity + timeout)
                check_sf_missions(ctx);
            }
        }
        remove_junk_periodic(lua, ctx, start_ts);
        // Publish weather to dashboard every 5 minutes
        if (start_ts - ctx.last_weather_publish).num_seconds() >= 300 {
            ctx.last_weather_publish = start_ts;
            if let Err(e) = atis::publish_weather(lua, ctx) {
                error!("failed to publish weather: {e:?}");
            }
        }
    }
    Ok(AdminResult::Continue)
}

fn run_timed_events(
    ctx: &mut Context,
    lua: MizLua,
    path: &PathBuf,
) -> Result<AdminResult> {
    let ts = Utc::now();
    let perf = Arc::make_mut(&mut unsafe { Perf::get_mut() }.inner);
    let net = Net::singleton(lua)?;
    let act = Trigger::singleton(lua)?.action()?;
    force_players_to_spectators(ctx, &net, ts);
    match ctx.db.update_unit_positions_incremental(lua, ts, ctx.last_unit_position) {
        Err(e) => error!("could not update unit positions {e}"),
        Ok((i, dead)) => {
            ctx.last_unit_position = i;
            for id in dead {
                if let Err(e) = unit_killed(lua, ctx, id.clone(), ts) {
                    error!("unit killed failed {:?} {:?}", id, e)
                }
            }
        }
    }
    record_perf(&mut perf.unit_positions, ts);
    let ts = Utc::now();
    match ctx.db.update_player_positions_incremental(lua, ts, ctx.last_player_position) {
        Err(e) => error!("could not update player positions {e}"),
        Ok((i, dead)) => {
            ctx.last_player_position = i;
            for id in dead {
                if let Err(e) = unit_killed(lua, ctx, id.clone(), ts) {
                    error!("unit killed failed {:?} {:?}", id, e)
                }
            }
        }
    }
    record_perf(&mut perf.player_positions, ts);

    match run_slow_timed_events(lua, ctx, perf, path, ts) {
        Ok(AdminResult::Continue) => (),
        Ok(AdminResult::Shutdown) => return Ok(AdminResult::Shutdown),
        Err(e) => error!("error running slow timed events {:?}", e),
    }
    if let Some(slot) = ctx.menu_init_queue.shift_remove_index(0) {
        if let Err(e) = menu::init_for_slot(ctx, lua, &slot) {
            error!("could not init menus for slot {:?} {:?}", slot, e)
        }
    }
    let now = Utc::now();
    let spctx = SpawnCtx::new(lua)?;
    if let Err(e) = ctx.db.ephemeral.process_spawn_queue(
        perf,
        &ctx.db.persisted,
        ts,
        &ctx.idx,
        &spctx,
    ) {
        error!("error processing spawn queue {:?}", e)
    }
    record_perf(&mut perf.spawn_queue, now);
    if let Err(e) = ctx.db.tick_csar(lua) {
        error!("csar tick failed: {:?}", e)
    }
    let now = Utc::now();
    let has_captures = match advise_captured(ctx, lua, ts) {
        Ok(captures) => captures,
        Err(e) => {
            error!("error advise captured {:?}", e);
            false
        }
    };
    record_perf(&mut perf.advise_captured, now);

    // Update frontline when objectives are captured
    if has_captures {
        update_frontline(ctx, ts, true);
    }
    let now = Utc::now();
    if let Err(e) = advise_captureable(ctx) {
        error!("error advise capturable {:?}", e)
    }
    record_perf(&mut perf.advise_capturable, now);
    let now = Utc::now();
    match ctx.jtac.update_target_positions(lua, now, &mut ctx.db) {
        Err(e) => error!("error updating jtac target positions {:?}", e),
        Ok(dead) => {
            for id in dead {
                if let Err(e) = unit_killed(lua, ctx, id.clone(), now) {
                    error!("unit killed failed {:?} {:?}", id, e)
                }
            }
        }
    }
    record_perf(&mut perf.jtac_target_positions, now);
    let now = Utc::now();
    let max_rate = ctx.db.ephemeral.cfg.max_msgs_per_second;
    ctx.db.ephemeral.msgs().process(max_rate, &net, &act);
    record_perf(&mut perf.process_messages, now);
    if let Err(e) = ctx.db.logistics_step(lua, perf, ts) {
        error!("error running logistics events {e:?}")
    }
    match run_admin_commands(ctx, lua) {
        Err(e) => error!("failed to run admin commands {e:?}"),
        Ok(AdminResult::Continue) => (),
        Ok(AdminResult::Shutdown) => return Ok(AdminResult::Shutdown),
    }
    if let Err(e) = run_action_commands(ctx, perf, lua) {
        error!("failed to run action commands {e:?}")
    }
    if let Err(e) = run_jtac_commands(ctx, lua) {
        error!("failed to run jtac commands {e:?}")
    }
    ctx.load_state.step();
    record_perf(&mut perf.timed_events, ts);
    ctx.log_perf(now);
    Ok(AdminResult::Continue)
}

fn start_timed_events(ctx: &mut Context, lua: MizLua, path: PathBuf) -> Result<()> {
    ctx.last_slow_timed_events = Utc::now();
    let timer = Timer::singleton(lua)?;
    timer.schedule_function(timer.get_time()? + 1., mlua::Value::Nil, {
        let path = path.clone();
        move |lua, _, now| {
            let ctx = unsafe { Context::get_mut() };
            match catch_unwind(AssertUnwindSafe(|| run_timed_events(ctx, lua, &path))) {
                Ok(Ok(AdminResult::Continue)) => (),
                Ok(Err(e)) => error!("failed to run timed events {:?}", e),
                Ok(Ok(AdminResult::Shutdown)) => {
                    println!("initiating DCS shutdown");
                    if let Some(id) = ctx.event_handler_id.take() {
                        World::singleton(lua)?
                            .remove_event_handler(id)
                            .context("removing event handler")?
                    }
                    Net::singleton(lua)?.dostring_in(
                        DcsLuaEnvironment::Server,
                        "DCS.setUserCallbacks({}); DCS.exitProcess()".into(),
                    )?;
                    println!("removing timer event");
                    return Ok(None);
                }
                Err(e) => match e.downcast_ref::<anyhow::Error>() {
                    Some(e) => {
                        error!("run_timed_events panicked {e:?} {}", Backtrace::capture())
                    }
                    None => {
                        error!("run_timed_events panicked {e:?} {}", Backtrace::capture())
                    }
                },
            }
            Ok(Some(now + 1.))
        }
    })?;
    Ok(())
}

fn delayed_init_miz(lua: MizLua) -> Result<()> {
    info!("init_miz: welcome to blue flag v3");
    let ctx = unsafe { Context::get_mut() };
    info!("indexing the miz");
    let miz = Miz::singleton(lua)?;
    ctx.idx = miz.index().context("indexing the mission")?;
    info!("adding event handlers");
    ctx.event_handler_id = Some(
        World::singleton(lua)?
            .add_event_handler(on_event)
            .context("adding event handlers")?,
    );
    let sortie = miz.sortie().context("getting the sortie")?;
    let path = {
        let s = Env::singleton(lua)?.get_value_dict_by_key(sortie)?;
        if s.is_empty() {
            bail!("missing sortie in miz file")
        }
        ctx.sortie = s;
        ctx.miz_state_path = PathBuf::from(Lfs::singleton(lua)?.writedir()?.as_str())
            .join(ctx.sortie.as_str());
        ctx.miz_state_path.clone()
    };
    debug!("sortie is {:?}", ctx.sortie);
    let cfg = Arc::new(Cfg::load(&path)?);
    ctx.do_bg_task(Task::CfgLoaded {
        sortie: ctx.sortie.clone(),
        cfg: Arc::clone(&cfg),
        admin_channel: Arc::clone(&ctx.external_admin_commands),
    });
    debug!("path to saved state is {:?}", path);
    info!("initializing db");
    let to_bg = ctx.to_background.as_ref().unwrap().clone();
    ctx.do_bg_task(Task::Stat(Stat::NewRound { sortie: ctx.sortie.clone() }));
    if !path.exists() {
        debug!("saved state doesn't exist, starting from default");
        ctx.db = Db::init(lua, cfg, &ctx.idx, &miz, to_bg)
            .context("initalizing the mission")?;
    } else {
        debug!("saved state exists, loading it");
        ctx.db = Db::load(&miz, &ctx.idx, to_bg, cfg, &path)
            .context("loading the saved state")?;
    }
    ctx.shutdown = ctx
        .db
        .ephemeral
        .cfg
        .shutdown
        .map(|hrs| AutoShutdown::new(Utc::now() + Duration::hours(hrs as i64)));
    ctx.do_bg_task(Task::Stat(Stat::SessionStart {
        stop: ctx.shutdown.map(|a| a.when),
        cfg: Box::new((*ctx.db.ephemeral.cfg).clone()),
    }));
    info!("spawning units");
    ctx.respawn_groups(lua, &miz).context("setting up the mission after load")?;

    // Publish all objectives as stats (for bfdb JSONL ingestion after saved state load)
    {
        let coord = Coord::singleton(lua)?;
        for (oid, obj) in ctx.db.persisted.objectives.into_iter() {
            let pos = obj.pos();
            match coord.lo_to_ll(LuaVec3(Vector3::new(pos.x, 0., pos.y))) {
                Ok(llpos) => {
                    ctx.db.ephemeral.stat(Stat::Objective {
                        name: obj.name.clone(),
                        id: *oid,
                        kind: obj.kind().clone(),
                        owner: obj.owner,
                        pos: llpos,
                    });
                }
                Err(e) => error!("failed to convert objective position for {}: {e:?}", obj.name),
            }
        }
        info!("published {} objectives as stats", ctx.db.persisted.objectives.len());
    }

    // Initialize dynamic frontline system if frontline is enabled
    if let Some(frontline_cfg) = &ctx.db.ephemeral.cfg.frontline {
        if frontline_cfg.enabled {
            info!("Initializing dynamic frontline system");
            let frontline = frontline::FrontLine::new(frontline_cfg.clone());
            ctx.frontline = Some(frontline);
            // Perform initial frontline calculation
            update_frontline(ctx, Utc::now(), true);
        }
    }

    info!("starting timed events");
    start_timed_events(ctx, lua, path).context("starting the timed events loop")?;
    Ok(())
}

fn on_mission_load_end(_lua: HooksLua) -> Result<()> {
    unsafe {
        Context::get_mut().load_state = LoadState::MissionLoaded { time: Utc::now() }
    };
    info!("mission loaded");
    Ok(())
}

fn on_player_disconnect(_: HooksLua, id: PlayerId) -> Result<()> {
    info!("onPlayerDisconnect({id})");
    let start_ts = Utc::now();
    let ctx = unsafe { Context::get_mut() };
    if let Some(ifo) = ctx.connected.player_disconnected(id) {
        info!("deslotting disconnected player {}", ifo.ucid);
        ctx.db.player_disconnected(&ifo.ucid)
    }
    record_perf(
        &mut Arc::make_mut(&mut unsafe { Perf::get_mut() }.inner).dcs_hooks,
        start_ts,
    );
    Ok(())
}

fn on_simulation_frame(_: HooksLua) -> Result<()> {
    let frame = Arc::make_mut(&mut unsafe { Perf::get_mut() }.frame);
    let now = Utc::now();
    let ctx = unsafe { Context::get_mut() };
    match &mut ctx.last_frame {
        Some(last) => {
            if let Some(ns) = (now - *last).num_nanoseconds() {
                if ns >= 1 && ns <= 1_000_000_000 {
                    **frame += ns as u64;
                }
            }
            *last = now;
        }
        None => {
            ctx.last_frame = Some(now);
        }
    }
    Ok(())
}

fn init_hooks(lua: HooksLua) -> Result<()> {
    info!("setting user hooks");
    UserHooks::new(lua)
        .on_player_try_change_slot(on_player_try_change_slot)?
        .on_mission_load_end(on_mission_load_end)?
        .on_player_try_connect(on_player_try_connect)?
        .on_player_try_send_chat(on_player_try_send_chat)?
        .on_player_disconnect(on_player_disconnect)?
        .on_simulation_frame(on_simulation_frame)?
        .register()?;
    Ok(())
}

fn init_miz(lua: MizLua) -> Result<()> {
    info!("initializing mission");
    let timer = Timer::singleton(lua)?;
    let when = timer.get_time()? + 1.;
    timer.schedule_function(when, mlua::Value::Nil, move |lua, _, now| {
        let ctx = unsafe { Context::get_mut() };
        if ctx.load_state.init_ok() {
            if let Err(e) = delayed_init_miz(lua) {
                error!("THE MISSION CANNOT START: {:?}", e);
                let timer = Timer::singleton(lua)?;
                timer.schedule_function(
                    now + 1.,
                    mlua::Value::Nil,
                    move |lua, _, now| {
                        let ctx = unsafe { Context::get_mut() };
                        let _ = Trigger::singleton(lua)?.action()?.out_text(
                            format_compact!(
                                "THE MISSION CANNOT START BECAUSE OF AN ERROR\n\n{:?}",
                                e
                            )
                            .into(),
                            3600,
                            true,
                        );
                        ctx.load_state.step();
                        Ok(Some(now + 10.))
                    },
                )?;
            }
            Ok(None)
        } else {
            info!("waiting for the mission to finish loading");
            Ok(Some(now + 1.))
        }
    })?;
    Ok(())
}

#[mlua::lua_module]
fn bflib(lua: &Lua) -> LuaResult<LuaTable<'_>> {
    // ensure we capture backtraces on panic
    let _ = unsafe { 
        std::env::set_var("RUST_BACKTRACE", "1"); // bactrace for panics
        std::env::set_var("RUST_LIB_BACKTRACE", "0"); // no backtrace for Error
    };
    unsafe { Context::get_mut() }.init_async_bg(lua.inner()).map_err(dcso3::lua_err)?;
    dcso3::create_root_module(lua, init_hooks, init_miz)
}
