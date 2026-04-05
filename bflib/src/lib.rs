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
    cfg::{Cfg, LifeType},
    db::objective::ObjectiveId,
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
) -> Result<String> {
    let start_ts = Utc::now();
    let ctx = unsafe { Context::get_mut() };
    let perf = &mut Arc::make_mut(&mut unsafe { Perf::get_mut() }.inner).dcs_hooks;
    info!("onPlayerTrySendChat id: {:?}, msg: {:?}, all: {:?}", id, msg, all);
    let r = chatcmd::process(ctx, lua, start_ts, id, msg);
    record_perf(perf, start_ts);
    match r {
        Ok(s) => Ok(s),
        Err(e) => {
            ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), format_compact!("{e}"));
            Ok("".into())
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
                        ctx.menu_init_queue.insert(slot);
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
                    if let Err(e) = unit_killed(lua, ctx, target.object_id()?, start_ts) {
                        error!("0 unit killed failed {:?}", e)
                    }
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
            ()
        }
        Event::Dead(e) | Event::UnitLost(e) => {
            if let Some(unit) = e.initiator.as_ref().and_then(|u| u.as_unit().ok()) {
                let id = unit.object_id()?;
                if let Err(e) = unit_killed(lua, ctx, id, start_ts) {
                    error!("1 unit killed failed {:?}", e)
                }
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
        let name = ctx.db.objective(&oid)?.name();
        let mcap = format_compact!("our forces have captured {}", name);
        let mlost = format_compact!("we have lost {}", name);
        ctx.db.ephemeral.msgs().panel_to_side(15, false, side, mcap);
        ctx.db.ephemeral.msgs().panel_to_side(15, false, side.opposite(), mlost);
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

fn tick_smart_commander(ctx: &mut Context, ts: DateTime<Utc>) {
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
    use dcso3::trigger::{CircleSpec, LineType, SideFilter};
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
                match ctx.db.add_and_queue_group(
                    &spctx,
                    &ctx.idx,
                    side,
                    SpawnLoc::AtPos {
                        pos: obj_pos + dcso3::Vector2::new(200., 200.),
                        offset_direction: dcso3::Vector2::new(1., 0.),
                        group_heading: 0.,
                    },
                    &template,
                    DeployKind::Objective { origin: objective },
                    BitFlags::empty(),
                    None,
                ) {
                    Ok(gid) => {
                        info!("SpawnReinforcements: spawned {:?} for {:?}", gid, side);
                        // Queue the group to advance toward the nearest enemy objective
                        // after it appears in DCS (spawn queue lag).
                        let enemy_side = match side {
                            dcso3::coalition::Side::Red => dcso3::coalition::Side::Blue,
                            dcso3::coalition::Side::Blue => dcso3::coalition::Side::Red,
                            dcso3::coalition::Side::Neutral => dcso3::coalition::Side::Neutral,
                        };
                        let advance_target = crate::db::events::find_nearest_friendly_objective(
                            &ctx.db, enemy_side, obj_pos, None,
                        );
                        if let Some(target) = advance_target {
                            ctx.event_scheduler.pending_moves.insert(gid, target);
                        }
                    }
                    Err(e) => error!("SpawnReinforcements: {e:?}"),
                }
                // Brief "arrived" mark
                let mid = dcso3::trigger::MarkId::new();
                ctx.db.ephemeral.msgs().circle_to_all(
                    SideFilter::All,
                    mid,
                    CircleSpec {
                        center: dcso3::LuaVec3(dcso3::Vector3::new(obj_pos.x, 0., obj_pos.y)),
                        radius: 1000.,
                        color: side_color(side),
                        fill_color: Color::new(0., 0., 0., 0.),
                        line_type: LineType::Solid,
                        read_only: true,
                    },
                    Some(format_compact!("Reinforcements Arrived [{:?}]", side).into()),
                );
                // Store it so it can be cleaned up — event is expiring so we delete it now
                ctx.db.ephemeral.msgs().delete_mark(mid);
                let _ = event_id;
            }

            EventEffect::SpawnHvt { event_id, side, objective, obj_pos, reward_points, escape_pos } => {
                let template = find_ground_template(&ctx.db, objective, side);
                let template = match template {
                    Some(t) => t,
                    None => {
                        info!("SpawnHvt: no suitable template at {:?}", objective);
                        continue;
                    }
                };
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
                        gid
                    }
                    Err(e) => { error!("SpawnHvt: {e:?}"); continue; }
                };
                // B: Queue escape move — group not in DCS yet (spawn queue lag).
                // pending_moves is retried each slow tick until the group appears.
                if let Some(epos) = escape_pos {
                    ctx.event_scheduler.pending_moves.insert(gid, epos);
                }
                // F10 mark for the HVT
                let mid = dcso3::trigger::MarkId::new();
                ctx.db.ephemeral.msgs().circle_to_all(
                    SideFilter::All,
                    mid,
                    CircleSpec {
                        center: dcso3::LuaVec3(dcso3::Vector3::new(obj_pos.x, 0., obj_pos.y)),
                        radius: 1000.,
                        color: side_color(side),
                        fill_color: Color::new(0., 0., 0., 0.),
                        line_type: LineType::Solid,
                        read_only: true,
                    },
                    Some(format_compact!("HVT [{:?}] +{} pts — EVACUATING", side, reward_points).into()),
                );
                ctx.event_scheduler.register_mark(event_id, mid);
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
                let group_ids: Vec<bfprotocols::db::group::GroupId> = ctx
                    .db
                    .persisted
                    .groups_by_side
                    .get(&attacking_side)
                    .map(|s| s.into_iter().copied().collect())
                    .unwrap_or_default();

                let mut to_order: Vec<(dcso3::String, dcso3::Vector2)> = Vec::new();
                for gid in &group_ids {
                    if let Some(g) = ctx.db.persisted.groups.get(gid) {
                        match g.class {
                            ObjGroupClass::Armor | ObjGroupClass::Mr | ObjGroupClass::Sr | ObjGroupClass::Lr => {}
                            _ => continue,
                        }
                        let alive = g.units.into_iter().any(|uid| {
                            ctx.db.persisted.units.get(uid).map(|u| !u.dead).unwrap_or(false)
                        });
                        if !alive {
                            continue;
                        }
                        let pos = ctx.db.group_center(gid).unwrap_or_default();
                        to_order.push((g.name.clone(), pos));
                    }
                }

                for (group_name, group_pos) in to_order {
                    let closest = target_positions
                        .iter()
                        .min_by(|a, b| {
                            let da = na::distance_squared(&(**a).into(), &group_pos.into());
                            let db_d = na::distance_squared(&(**b).into(), &group_pos.into());
                            da.partial_cmp(&db_d).unwrap_or(std::cmp::Ordering::Equal)
                        })
                        .copied();
                    let target_pos = match closest {
                        Some(p) => p,
                        None => continue,
                    };
                    let dcs_group = match Group::get_by_name(lua, group_name.as_str()) {
                        Ok(g) => g,
                        Err(_) => continue,
                    };
                    let controller = match dcs_group.get_controller() {
                        Ok(c) => c,
                        Err(e) => {
                            error!("OrderAttack: get_controller for {group_name}: {e}");
                            continue;
                        }
                    };
                    let alt = land.get_height(LuaVec2(target_pos)).unwrap_or(0.);
                    let task = Task::Mission {
                        airborne: Some(false),
                        route: vec![MissionPoint {
                            typ: PointType::TurningPoint,
                            airdrome_id: None,
                            time_re_fu_ar: None,
                            helipad: None,
                            link_unit: None,
                            action: Some(ActionTyp::Ground(VehicleFormation::OnRoad)),
                            pos: LuaVec2(target_pos),
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
                        error!("OrderAttack: set_task for {group_name}: {e}");
                    } else {
                        info!("OrderAttack: ordered {:?} group {group_name}", attacking_side);
                    }
                }

                // F10 dashed circles at each target
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

            // C: Artillery/armor barrage — order alive Armor/Mr/Lr groups to fire at target
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

                let mut fired = 0u32;
                for gid in &gids {
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
                    if let Ok(dcs_group) = Group::get_by_name(lua, group_name.as_str()) {
                        if let Ok(controller) = dcs_group.get_controller() {
                            let alt = land.get_height(LuaVec2(target_pos)).unwrap_or(0.);
                            let fire_task = Task::FireAtPoint {
                                point: LuaVec2(target_pos),
                                radius: Some(500.),
                                expend_qty: None,
                                weapon_type: None,
                                altitude: Some(alt),
                                altitude_type: Some(AltType::BARO),
                            };
                            if let Err(e) = controller.set_task(fire_task) {
                                error!("FireBarrage: set_task {group_name}: {e}");
                            } else {
                                fired += 1;
                                info!("FireBarrage: ordered {:?} group {group_name} to fire at {:?}", side, target_pos);
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

            // D: Spawn ambush force near convoy position
            EventEffect::SpawnAmbush { event_id, ambush_side, spawn_pos, source_objective } => {
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
                    Ok(gid) => info!("SpawnAmbush: spawned {:?} for {:?}", gid, ambush_side),
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
                    Err(e) => error!("SpawnCap: failed to spawn CAP template {template}: {e:?}"),
                }
            }

            // E: Remove all CAP groups when the event expires
            EventEffect::DespawnCap { event_id } => {
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

    let pending: Vec<_> = ctx.event_scheduler.pending_moves.iter()
        .map(|(gid, pos)| (*gid, *pos))
        .collect();

    for (gid, target_pos) in pending {
        let group_name = match ctx.db.persisted.groups.get(&gid) {
            Some(g) => g.name.clone(),
            None => {
                // Group was deleted; remove from pending
                ctx.event_scheduler.pending_moves.remove(&gid);
                continue;
            }
        };
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
        let alt = land.get_height(LuaVec2(target_pos)).unwrap_or(0.);
        let task = Task::Mission {
            airborne: Some(false),
            route: vec![MissionPoint {
                typ: PointType::TurningPoint,
                airdrome_id: None,
                time_re_fu_ar: None,
                helipad: None,
                link_unit: None,
                action: Some(ActionTyp::Ground(VehicleFormation::OnRoad)),
                pos: LuaVec2(target_pos),
                alt,
                alt_typ: Some(AltType::BARO),
                speed: 10.,
                speed_locked: Some(false),
                eta: None,
                eta_locked: None,
                name: None,
                task: Box::new(Task::Hold),
            }],
        };
        if let Err(e) = controller.set_task(task) {
            error!("flush_pending_moves: set_task for {group_name}: {e}");
        } else {
            info!("flush_pending_moves: ordered {group_name} to move to {:?}", target_pos);
        }
        // Order issued (success or terminal failure) — remove from pending
        ctx.event_scheduler.pending_moves.remove(&gid);
    }
}

fn side_color(side: dcso3::coalition::Side) -> dcso3::Color {
    match side {
        dcso3::coalition::Side::Blue => dcso3::Color::blue(1.),
        dcso3::coalition::Side::Red => dcso3::Color::red(1.),
        dcso3::coalition::Side::Neutral => dcso3::Color::white(1.),
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
                // Detect HVT kill and award bonus points
                if let bfprotocols::shots::Who::AI { gid, .. } = &dead.victim {
                    if let Some((event_id, reward)) = ctx.event_scheduler.hvt_groups.remove(gid) {
                        let killer_ucid = dead.shots.iter().find_map(|s| match &s.shooter {
                            bfprotocols::shots::Who::Player { ucid, .. } => Some(*ucid),
                            _ => None,
                        });
                        if let Some(ucid) = killer_ucid {
                            if let Some(player) = ctx.db.persisted.players.get_mut_cow(&ucid) {
                                player.points += reward;
                                let side = player.side;
                                let total = player.points;
                                ctx.db.ephemeral.dirty();
                                ctx.db.ephemeral.msgs().panel_to_side(
                                    15, false, side,
                                    format_compact!("HVT eliminated! +{reward} bonus points (total: {total})"),
                                );
                            }
                        }
                        // E: Revenge — enemy side launches a counter-offensive after a delay
                        let events_cfg = ctx.db.ephemeral.cfg.campaign_events.clone();
                        if let Some(ref ecfg) = events_cfg {
                            if ecfg.escalation_enabled {
                                // The HVT belonged to some side; the enemy of that side killed it.
                                // Trigger revenge for the HVT's side.
                                if let Some(event) = ctx.event_scheduler.active_events.iter()
                                    .find(|e| e.id() == event_id)
                                {
                                    let hvt_side = match event {
                                        crate::db::events::CampaignEvent::HighValueTarget { side, .. } => Some(*side),
                                        _ => None,
                                    };
                                    if let Some(side) = hvt_side {
                                        let trigger_at = dead.time + chrono::Duration::seconds(ecfg.revenge_delay_secs as i64);
                                        ctx.event_scheduler.schedule_revenge(side, trigger_at);
                                        ctx.db.ephemeral.msgs().panel_to_all(
                                            15, false,
                                            format_compact!("INTEL: {:?} forces will retaliate for the loss of their HVT!", side),
                                        );
                                    }
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
                    let obj = ctx.db.objective(&oid)?;
                    let owner = obj.owner();
                    let msg = format_compact!("enemies spotted near {}", obj.name());
                    ctx.db.ephemeral.msgs().panel_to_side(10, false, owner, msg)
                }
                for oid in cleared {
                    let obj = ctx.db.objective(&oid)?;
                    let owner = obj.owner();
                    let msg = format_compact!("{} is no longer threatened", obj.name());
                    ctx.db.ephemeral.msgs().panel_to_side(10, false, owner, msg)
                }
            }
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
        match ctx.db.check_carrier_group_capture(ts) {
            Ok(captures) => {
                for (oid, old_owner, new_owner) in captures {
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
        update_jtac_contacts(ctx, lua);
        record_perf(&mut perf.update_jtac_contacts, ts);
        let now = Utc::now();
        if let Some(snap) = ctx.db.maybe_snapshot() {
            ctx.do_bg_task(bg::Task::SaveState(path.clone(), snap));
        }
        record_perf(&mut perf.snapshot, now);
        award_periodic_points(ctx, start_ts);
        tick_smart_commander(ctx, start_ts);
        record_perf(&mut perf.slow_timed, start_ts);

        // Tick campaign events system
        if let Some(events_cfg) = ctx.db.ephemeral.cfg.campaign_events.as_ref() {
            if events_cfg.enabled {
                let events_cfg = events_cfg.clone();
                match ctx.event_scheduler.tick(&ctx.db, &events_cfg, start_ts) {
                    Ok((messages, effects)) => {
                        for msg in messages {
                            ctx.db.ephemeral.msgs().panel_to_all(15, false, msg);
                        }
                        apply_event_effects(lua, ctx, effects);
                    }
                    Err(e) => error!("error ticking campaign events: {e:?}"),
                }
                // Retry deferred move orders for newly-spawned groups
                flush_pending_moves(lua, ctx);
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
