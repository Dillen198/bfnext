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
mod api;
mod atis;
mod bg;
mod carp;
mod chatcmd;
mod cockpit;
mod commander;
mod db;
mod ewr;
mod frontline;
mod intel_marks;
mod jtac;
mod landcache;
mod mapcolor;
mod menu;
mod msgq;
mod navaids;
mod shots;
mod situation;
mod spawnctx;
mod unitdb;

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
    dcs::Dcs,
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
    HooksLua, LuaEnv, LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3,
};
use ewr::Ewr;
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use indexmap::IndexSet;
use jtac::{aim_and_fire_route, group_facing, JtId, Jtacs};
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

/// Build identity, embedded at compile time by `build.rs`.
pub const BUILD_GIT: &str = env!("BFNEXT_BUILD_GIT");
pub const BUILD_EPOCH: &str = env!("BFNEXT_BUILD_EPOCH");
pub const BUILD_VERSION: &str = env!("CARGO_PKG_VERSION");

/// RFC3339 UTC build time from the embedded epoch.
fn build_time() -> std::string::String {
    BUILD_EPOCH
        .parse::<i64>()
        .ok()
        .and_then(|s| chrono::DateTime::from_timestamp(s, 0))
        .map(|dt| dt.to_rfc3339_opts(chrono::SecondsFormat::Secs, true))
        .unwrap_or_else(|| "unknown".to_string())
}

/// Log this build's identity and drop a `Logs/bfnext-bflib-build.json` sidecar
/// next to the DCS logs so the DCSServerBot plugin can show which engine is
/// actually loaded (vs. the file staged on disk).
fn report_build(write_dir: &std::path::Path) {
    let built = build_time();
    info!("[BUILD] bflib v{BUILD_VERSION} git:{BUILD_GIT} built:{built}");
    let path = write_dir.join("Logs").join("bfnext-bflib-build.json");
    let body = format!(
        r#"{{"name":"bflib","version":"{BUILD_VERSION}","git":"{BUILD_GIT}","built":"{built}"}}"#
    );
    if let Err(e) = std::fs::write(&path, body) {
        warn!("could not write build sidecar {path:?}: {e:?}");
    }
}

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

/// The restart warnings, as (minutes remaining, wording). Ordered from the
/// earliest to the latest, which `check_auto_shutdown` relies on.
const SHUTDOWN_WARNINGS: [(i64, &str); 4] = [
    (30, "30 minutes"),
    (10, "10 minutes"),
    (5, "5 minutes"),
    (1, "one minute"),
];

#[derive(Debug, Clone, Copy, Default)]
struct AutoShutdown {
    when: DateTime<Utc>,
    /// Which entries of `SHUTDOWN_WARNINGS` have already been announced.
    ///
    /// A warning whose threshold has ALREADY passed when the restart time is
    /// set counts as announced (see `scheduled`), so a restart that is eleven
    /// minutes out is never announced as "30 minutes" -- it simply waits for
    /// the ten minute mark. That, plus the fact that setting the same time
    /// again keeps the existing flags (see `AdminCommand::SetServerInfo`), is
    /// what stops the countdown repeating itself.
    warned: [bool; SHUTDOWN_WARNINGS.len()],
}

/// Surface weather pushed in from bfdb (which reads it from DCSServerBot's
/// RestAPI). Used by the F10 "Weather" menu when present, so it agrees with
/// the dashboard instead of reporting whatever `atmosphere.getWind` returns
/// for a mission whose live-weather sync isn't running.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct BotWeather {
    pub(crate) temp_c: f64,
    pub(crate) wind_speed_kts: f64,
    pub(crate) wind_from_deg: f64,
    pub(crate) qnh_hpa: f64,
    pub(crate) cloud_base_m: f64,
    pub(crate) visibility_m: f64,
    /// Cloud cover, 0-10.
    pub(crate) cloud_density: f64,
}

impl AutoShutdown {
    /// Schedule a restart for `ts`, suppressing every warning whose threshold
    /// has already gone by at `now`.
    fn scheduled(ts: DateTime<Utc>, now: DateTime<Utc>) -> Self {
        let left = ts - now;
        let mut t = Self::default();
        t.when = ts;
        for (i, (mins, _)) in SHUTDOWN_WARNINGS.iter().enumerate() {
            t.warned[i] = left <= Duration::minutes(*mins);
        }
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
    /// on-disk path of the currently loaded .miz, captured from
    /// DCS.getMissionFilename() in on_mission_load_end. Used to rewrite the
    /// mission's weather/time in place before a live-weather-enabled
    /// restart, since DCS only picks up mission file edits on next load.
    mission_file_path: Option<PathBuf>,
    shutdown: Option<AutoShutdown>,
    last_perf_log: DateTime<Utc>,
    load_state: LoadState,
    idx: env::miz::MizIndex,
    db: Db,
    external_admin_commands: Arc<SegQueue<(AdminCommand, oneshot::Sender<Value>)>>,
    admin_commands: Vec<(admin::Caller, AdminCommand)>,
    action_commands: Vec<(PlayerId, String)>,
    /// `-weather` requests, answered from the mission Lua state: the chat
    /// hook runs in the hooks state, which has no `atmosphere` or mission
    /// weather to read.
    weather_requests: Vec<(PlayerId, SlotId)>,
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
    /// Fingerprint of the objective set (id+owner+kind) at the last navaid
    /// reallocation. When it changes, `crate::navaids::reallocate` reruns.
    navaid_sig: Option<u64>,
    /// Carrier-group objectives whose deck navaids are currently lit. Cleared
    /// when the objective set changes (a capture may have flipped the deck),
    /// so the sweep re-lights them; retried each tick until the deck's
    /// airbase resolves.
    navaid_carriers_lit: FxHashSet<ObjectiveId>,
    /// Surface weather from DCSServerBot, pushed in by bfdb via the
    /// `set-server-info` RPC. `None` until the first push.
    bot_weather: Option<BotWeather>,
    last_periodic_points: DateTime<Utc>,
    last_commander_tick: DateTime<Utc>,
    last_unit_position: usize,
    last_player_position: usize,
    /// Page cursor for the paged Objectives status reports, per menu group.
    /// DCS radio menus can't be relabeled in place, so the current page lives
    /// here and the Next/Prev Page commands move it, instead of the menu
    /// carrying one command per page.
    objective_pages: FxHashMap<dcso3::env::miz::GroupId, menu::objectives::StatusPages>,
    subscribed_jtac_menus: FxHashMap<SlotId, JtacSlotIfo>,
    /// Objectives that currently have at least one JTAC on them, per side, as of
    /// the last contact update. The JTAC menu's location list is built once when
    /// a player opens it and then only refreshed for the locations they have
    /// already expanded, so a JTAC that appears somewhere new -- a freshly
    /// deployed Reaper, most often -- used to be invisible until the player
    /// happened to hit "Refresh Locations". Comparing this against the live set
    /// each tick catches exactly that, and nothing else.
    jtac_locations: FxHashMap<Side, FxHashSet<ObjectiveId>>,
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
        // Every 10 minutes: at 60s the cumulative timing tables were 59-75%
        // of every engine log line, and they are running totals anyway.
        if now - self.last_perf_log > Duration::seconds(600) {
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
        SlotAuth::Consolidating(secs) => {
            let msg = format_compact!(
                "Objective is still consolidating ({}s left) -- hold it with troops,                  or land a logistics crate to speed it up",
                secs
            );
            ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg);
        }
        SlotAuth::CapturedNotReady(vehicle) => {
            let msg = format_compact!(
                "{} was captured here -- it will be flyable once this objective is repaired",
                vehicle.0
            );
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
                Err(RegErr::AlreadyRegistered(switches, on)) => {
                    warn!(
                        "{:?} try_occupy_slot says NotRegistered but register_player says AlreadyRegistered to {on:?} ({switches:?} switches left)",
                        ifo.ucid
                    );
                    Ok(false)
                }
                Err(RegErr::AlreadyOn(on)) => {
                    warn!(
                        "{:?} try_occupy_slot says NotRegistered but register_player says AlreadyOn {on:?}",
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
        // DCS re-fires a batch of event ids (dynamic cargo, sim freeze, human
        // repair start/stop, ...) that campaign logic doesn't consume -- and it
        // fires several of them twice per tick. Don't spam the log with them.
        Event::Invalid
        | Event::SimulationFreeze
        | Event::SimulationUnfreeze
        | Event::HumanAircraftRepairStart
        | Event::HumanAircraftRepairFinish
        | Event::GroupChangeOption
        | Event::MacLmsRestart => (),
        // High-frequency trace (Birth/Hit/Bda alone are ~1500 lines in a two
        // hour session). The events campaign logic actually consumes are
        // matched and logged on their own below where it matters -- keep this
        // catch-all at debug.
        ev => debug!("onEvent: {:?}", ev),
    }
    match ev {
        Event::Birth(b) => {
            if let Ok(unit) = b.initiator.as_unit() {
                ctx.recently_born.insert(unit.object_id()?, Utc::now());
                match ctx.db.unit_born(lua, &unit, &ctx.connected) {
                    Ok(BirthRes::None) => (),
                    Ok(BirthRes::OccupiedSlot(slot)) => {
                        ctx.menu_init_queue.insert(slot.clone());
                        if let Err(e) = atis::schedule_atis(lua, slot.clone()) {
                            error!("could not schedule atis: {:?}", e);
                        }
                        // The auto-generated situational briefing, sequenced
                        // after the ATIS so they don't overwrite each other.
                        if let Err(e) = situation::schedule_slot_briefing(lua, slot) {
                            error!("could not schedule situation briefing: {:?}", e);
                        }
                        // Force EPLRS on for every player group so fixed-wing
                        // slots show up on the coalition F10 map / datalink the
                        // same way helicopters (which usually have it enabled in
                        // the .miz) already do. Best effort -- a failure here
                        // just means that slot relies on the .miz setting.
                        if let Err(e) = unit
                            .get_controller()
                            .and_then(|c| c.set_command(dcso3::controller::Command::EPLRS {
                                enable: true,
                                group: None,
                            }))
                        {
                            debug!("could not enable EPLRS for born slot {slot}: {e:?}");
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
                // Snapshot what we need before the deslot invalidates it.
                let leave_info = ctx.db.player_in_unit(false, &initiator).and_then(|ucid| {
                    ctx.db.player(&ucid).and_then(|p| {
                        p.current_slot
                            .as_ref()
                            .and_then(|(_, i)| i.as_ref())
                            .filter(|inst| inst.landed_at_objective.is_none())
                            .map(|inst| {
                                (p.side, Vector2::new(inst.position.p.x, inst.position.p.z))
                            })
                    })
                });
                if let Some((my_side, my_pos)) = leave_info {
                    ctx.shots_out.dead(initiator.clone(), start_ts);
                    // Anti-abuse: bailing a losing fight. If an enemy aircraft
                    // is close, credit them with the kill.
                    let radius = ctx.db.ephemeral.cfg.slot_leave_kill_radius_m;
                    if radius > 0.0 {
                        if let Some(enemy_oid) =
                            nearest_enemy_player_in_air(&ctx.db, my_side, my_pos, radius)
                        {
                            let shooter = crate::shots::who_for(&ctx.db, enemy_oid.clone());
                            let target = crate::shots::who_for(&ctx.db, initiator.clone());
                            let s_typ = ctx
                                .db
                                .ephemeral
                                .get_slot_by_object_id(&enemy_oid)
                                .and_then(|sl| ctx.db.ephemeral.get_slot_info(sl))
                                .map(|si| String::from(si.typ.as_str()));
                            let t_typ = ctx
                                .db
                                .ephemeral
                                .get_slot_by_object_id(&initiator)
                                .and_then(|sl| ctx.db.ephemeral.get_slot_info(sl))
                                .map(|si| String::from(si.typ.as_str()))
                                .unwrap_or_else(|| String::from("aircraft"));
                            if let (Some(shooter), Some(target)) = (shooter, target) {
                                ctx.shots_out.abandoned_under_threat(
                                    initiator.clone(),
                                    shooter,
                                    target,
                                    s_typ,
                                    t_typ,
                                    start_ts,
                                );
                            }
                        }
                    }
                }
                if let Err(e) = ctx.db.player_left_unit(lua, start_ts, &initiator) {
                    error!("player left unit failed {:?}", e)
                }
            } else {
                // DCS fires PlayerLeaveUnit with no initiator when the slot it
                // left can't be resolved to a unit any more (spectator
                // transition, unit already despawned). Nothing to do -- not an
                // error, just noise in the ERROR feed / bot alert relay.
                debug!("PlayerLeaveUnit with no initiator (benign)")
            }
        }
        Event::Hit(e) | Event::Kill(e) => {
            if let Some(target) = e.target.as_ref().and_then(|t| t.as_unit().ok()) {
                let dead = target.get_life()? < 1.;
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
                    let id = target.object_id()?;
                    if let Err(e) = ctx.db.respawn_protected_static(lua, &ctx.idx, &id) {
                        error!("respawn protected static failed {e:?}")
                    }
                    if let Err(e) = ctx.db.static_dead(&id, start_ts) {
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
                    // Live voice GCI: an enemy SAM firing -> "SAM launch, defend"
                    // for nearby friendly flights (see admin::query_gci).
                    if side != Side::Neutral
                        && (tags.contains(UnitTag::SAM) || tags.contains(UnitTag::Launcher))
                        && !tags.contains(UnitTag::Aircraft)
                    {
                        ctx.ewr.record_sam_launch(pos, side, start_ts);
                    }
                }
            }
            // IADN HARM defense: if this shot is a configured anti-radiation
            // weapon, start tracking it so nearby SAM sites on the opposite
            // side can be warned to go dark before it arrives.
            if let Some(iadn) = ctx.db.ephemeral.cfg.iadn.as_ref() {
                let arm_name = e.weapon_name.as_ref().map(|n| n.as_str()).unwrap_or("");
                if iadn.anti_radiation_weapons.contains(arm_name) {
                    let shooter_side = e.initiator.object_id().ok()
                        .and_then(|obj_id| ctx.db.ephemeral.get_uid_by_object_id(&obj_id))
                        .and_then(|uid| ctx.db.unit(uid).ok())
                        .map(|u| u.side);
                    if let (Some(shooter_side), Ok(weapon_oid)) = (shooter_side, e.weapon.object_id()) {
                        ctx.ewr.track_potential_arm(weapon_oid, shooter_side.opposite(), start_ts);
                    }
                }
            }
            // (Counter-battery map cue removed -- it only ever drew an
            // "ARTY / COUNTER-BATTERY" text mark with no gameplay behind it,
            // and repositioning batteries stacked overlapping copies. The
            // `counter_battery` cfg key is now ignored.)
            ()
        }
        Event::Dead(e) | Event::UnitLost(e) => {
            if let Some(unit) = e.initiator.as_ref().and_then(|u| u.as_unit().ok()) {
                let dismount = try_capture_dismount_info(ctx, &unit);
                try_gv_passenger_eject(lua, ctx, &unit);
                let id = unit.object_id()?;
                // Live voice GCI: hostile air killed -> "splash".
                if let Some((vside, vtags, vpos)) = ctx
                    .db
                    .ephemeral
                    .get_uid_by_object_id(&id)
                    .and_then(|uid| ctx.db.unit(uid).ok())
                    .map(|u| (u.side, u.tags.0, u.pos))
                {
                    if vside != Side::Neutral
                        && (vtags.contains(UnitTag::Aircraft) || vtags.contains(UnitTag::Helicopter))
                    {
                        ctx.ewr.record_air_kill(vpos, vside, start_ts);
                    }
                }
                if let Err(e) = unit_killed(lua, ctx, id, start_ts) {
                    error!("1 unit killed failed {:?}", e)
                }
                spawn_dismount(lua, ctx, dismount);
            } else if let Some(st) = e.initiator.as_ref().and_then(|s| s.as_static().ok())
            {
                let id = st.object_id()?;
                if let Err(e) = ctx.db.respawn_protected_static(lua, &ctx.idx, &id) {
                    error!("respawn protected static failed {e:?}")
                }
                if let Err(e) = ctx.db.static_dead(&id, start_ts) {
                    error!("static killed failed {e:?}")
                }
            }
        }
        Event::PilotDead(e) => {
            if let Some(unit) = e.initiator.as_ref().and_then(|u| u.as_unit().ok()) {
                // CSAR is only for a pilot who actually got out -- that's the
                // Ejection event. PilotDead also fires when the pilot is killed
                // in the aircraft (no ejection), and there's nobody on the
                // ground to rescue in that case, so don't spawn a downed pilot
                // here.
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
        Event::Ejection(e) => {
            if let Ok(unit) = e.initiator.as_unit() {
                let csar_pilot = try_capture_csar_info(lua, ctx, &unit);
                let id = unit.object_id()?;
                // Live voice GCI: "chute observed" for the ejected pilot's side.
                if let Some((eside, epos)) = ctx
                    .db
                    .ephemeral
                    .get_uid_by_object_id(&id)
                    .and_then(|uid| ctx.db.unit(uid).ok())
                    .map(|u| (u.side, u.pos))
                {
                    if eside != Side::Neutral {
                        ctx.ewr.record_ejection(epos, eside, start_ts);
                    }
                }
                if let Err(e) = unit_killed(lua, ctx, id, start_ts) {
                    error!("2 unit killed failed {}", e)
                }
                spawn_csar_pilot(lua, ctx, csar_pilot);
            }
        }
        // RunwayTakeoff/RunwayTouch (ids 54/55) are deliberately not handled
        // here: Takeoff and Land already fire for the same moments, and a
        // carrier bolter (touch then runway-takeoff) must not end a sortie.
        Event::Takeoff(e) => {
            if let Ok(unit) = e.initiator.as_unit() {
                let id = unit.object_id()?;
                if !ctx.recently_born.contains_key(&id)
                    && ctx.airborne.insert(id.clone())
                    && ctx.recently_landed.remove(&id).is_none()
                {
                    let slot = unit.slot()?;
                    let position = unit.get_ground_position()?.0;
                    warn_taxiway_takeoff(lua, ctx, &unit, e.place.as_ref(), position, &slot);
                    match ctx.db.takeoff(Utc::now(), slot, &unit, position) {
                        Err(e) => error!("could not process takeoff, {:?}", e),
                        Ok(TakeoffRes::NoLifeTaken | TakeoffRes::NotPlayerSlot) => (),
                        Ok(TakeoffRes::TookLife(typ)) => {
                            if let Err(e) =
                                message_life(ctx, &slot, Some(typ), "life taken\n")
                            {
                                error!("could not display life taken message {:?}", e)
                            }
                            let _ = menu::cargo::list_cargo_for_slot(ctx, &slot);
                        }
                        Ok(r @ (TakeoffRes::OutOfLives | TakeoffRes::OutOfPoints)) => {
                            // Say why -- the aircraft just vanished before.
                            if let Some(uid) = slot.as_unit_id() {
                                let why = match r {
                                    TakeoffRes::OutOfPoints => {
                                        "Not enough points to pay for this flight (see the FLIGHT COST panel from taxi) -- aircraft removed."
                                    }
                                    _ => "No lives left for this aircraft type -- aircraft removed.",
                                };
                                ctx.db.ephemeral.msgs().panel_to_unit(15, true, uid, why);
                            }
                            if let Err(e) = unit.destroy() {
                                error!(
                                    "failed to destroy unit that took off without lives or points {e:?}"
                                )
                            }
                        }
                        Ok(TakeoffRes::TooEarly(remaining)) => {
                            let ucid = ctx.db.ephemeral.player_in_slot(&slot).cloned();
                            if let Some(uid) = slot.as_unit_id() {
                                ctx.db.ephemeral.msgs().panel_to_unit(
                                    15,
                                    true,
                                    uid,
                                    format_compact!(
                                        "You took off {remaining}s before your ground hold expired -- returning to spectators."
                                    ),
                                );
                            }
                            if let Err(e) = unit.destroy() {
                                error!("failed to destroy unit that took off during the hold {e:?}")
                            }
                            if let Some(ucid) = ucid {
                                ctx.db.ephemeral.force_player_to_spectators(&ucid);
                            }
                        }
                    }
                }
            }
        }
        Event::Land(e) => {
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

pub(crate) fn lives(db: &mut Db, ucid: &Ucid, typfilter: Option<LifeType>) -> Result<CompactString> {
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

/// Did this departure break ground somewhere other than a runway of
/// `airbase_name`? Projects the lift-off point onto each runway's axis and
/// checks it's within the runway rectangle (plus a generous line-up / shoulder
/// margin). `None` when we can't tell -- no departure airbase, or the airbase /
/// runway lookup failed.
///
/// DCS's `getRunways()` `course` field has an inconsistent sign convention
/// across terrains/versions (sometimes the compass heading in radians,
/// sometimes its negation), which was rotating the along/across axes on
/// diagonal runways and flagging legitimate runway departures. So the rectangle
/// test is run for BOTH `course` and `-course` and the point counts as "on the
/// runway" if either orientation accepts it.
fn took_off_off_runway(lua: MizLua, airbase_name: &str, pos: Vector2) -> Option<bool> {
    // Trust DCS first: if the terrain says the lift-off point is runway
    // surface, it was a runway departure. The rectangle projection below has
    // been over-eager -- getRunways() reports an odd centre/course on some
    // diagonal or multi-runway fields, which flagged legitimate takeoffs.
    if let Ok(land) = dcso3::land::Land::singleton(lua) {
        if land.get_surface_type(LuaVec2(pos)).ok() == Some(dcso3::land::SurfaceType::Runway) {
            return Some(false);
        }
    }
    let ab = dcso3::airbase::Airbase::get_by_name(lua, airbase_name.into()).ok()?;
    let runways = ab.get_runways().ok()?;
    let mut saw_runway = false;
    for rwy in runways {
        let Ok(rwy) = rwy else { continue };
        let (Ok(center), Ok(course), Ok(length), Ok(width)) =
            (rwy.position(), rwy.course(), rwy.length(), rwy.width())
        else {
            continue;
        };
        // Carriers / LHAs (and the odd heliport) come back from getRunways()
        // with length == 0, width == 0 and a junk course. There is no runway
        // rectangle to be off of -- a catapult shot or a ski-jump launch is
        // never a "taxiway departure". Treat any such field as on-runway.
        if length < 1.0 || width < 1.0 {
            return Some(false);
        }
        saw_runway = true;
        // DCS world frame: x = north, z = east. get_ground_position gives a
        // Vector2 of (x = north, y = east).
        let dn = pos.x - center.0.x;
        let de = pos.y - center.0.z;
        // Fast jets float well past the far threshold before S_EVENT_TAKEOFF
        // fires, and getRunways()'s reported length/centre isn't always the
        // full paved surface -- be very generous along the axis. The side
        // margin stays tighter so a parallel-taxiway departure is still caught,
        // but with enough room for line-up + shoulder.
        let end_margin = length / 2.0 + 1500.0;
        // The lateral margin has to grow with distance along the runway.
        // `getRunways()` reports a course that is a few degrees off the real
        // axis on some fields, and an angular error turns into a lateral error
        // proportional to how far down the runway the wheels left the ground --
        // at ~1500 m along (a fast jet lifting off past the far threshold) six
        // degrees is ~160 m, well outside a flat 90 m box. That is what has
        // been flagging legitimate departures. 0.06 ~= 3.4 degrees of slop.
        let side_margin = |along: f64| width / 2.0 + 60.0 + along.abs() * 0.06;
        let mut best: Option<(f64, f64)> = None;
        for course in [course, -course] {
            let (s, c) = course.sin_cos();
            let along = dn * c + de * s;
            let across = -dn * s + de * c;
            if along.abs() <= end_margin && across.abs() <= side_margin(along) {
                return Some(false);
            }
            // Keep the *closest* projection for the diagnostic. This compared
            // `across.abs() < a` against the stored raw `across`, so once the
            // first candidate stored a negative value nothing could ever beat
            // it -- the log then reported the discarded projection. That is how
            // a departure 165 m off centreline came out in the log as 1536 m.
            if best.map(|(_, a)| across.abs() < a.abs()).unwrap_or(true) {
                best = Some((along, across));
            }
        }
        if let Some((along, across)) = best {
            info!(
                "[TAXIWAY_CHK] {airbase_name}: liftoff off runway (name={:?} len={length:.0} \
                 width={width:.0} course={course:.3}rad) along={along:.0} across={across:.0} \
                 (limits {end_margin:.0}/{:.0})",
                rwy.name().ok(),
                side_margin(along)
            );
        }
    }
    saw_runway.then_some(true)
}

/// Warn a fixed-wing player who got airborne from a taxiway or apron instead of
/// the runway. Helicopters and the AV-8B are exempt -- a pad departure is normal
/// for them.
fn warn_taxiway_takeoff(lua: MizLua, ctx: &mut Context, unit: &Unit, place: Option<&dcso3::object::Object>, pos: Vector2, slot: &SlotId) {
    let Some(place) = place else { return };
    let Some(sifo) = ctx.db.ephemeral.get_slot_info(slot) else { return };
    let is_helo = ctx
        .db
        .ephemeral
        .cfg
        .unit_classification
        .get(&sifo.typ)
        .map(|tags| tags.contains(UnitTag::Helicopter))
        .unwrap_or(false);
    if is_helo || sifo.typ.as_str() == "AV8BNA" {
        return;
    }
    let Ok(airbase_name) = place.get_name() else { return };
    if took_off_off_runway(lua, airbase_name.as_str(), pos) == Some(true) {
        if let Some(uid) = slot.as_unit_id() {
            ctx.db.ephemeral.msgs().panel_to_unit(
                15,
                false,
                uid,
                "You departed from a taxiway or apron. Fixed-wing aircraft must take off from the active runway.",
            );
        }
        if let Ok(name) = unit.get_player_name() {
            info!("taxiway/apron takeoff by {:?} from {airbase_name}", name);
        }
    }
}

/// Nearest airborne enemy *player*'s unit object id within `radius` metres of
/// `my_pos`, or None. Used to attribute a kill when someone bails a slot mid-
/// fight.
fn nearest_enemy_player_in_air(
    db: &db::Db,
    my_side: Side,
    my_pos: Vector2,
    radius: f64,
) -> Option<DcsOid<ClassUnit>> {
    let mut best: Option<(DcsOid<ClassUnit>, f64)> = None;
    for (_ucid, p, inst) in db.instanced_players() {
        if p.side == my_side || !inst.in_air {
            continue;
        }
        let Some((slot, _)) = p.current_slot.as_ref() else {
            continue;
        };
        let Some(oid) = db.ephemeral.get_object_id_by_slot(slot) else {
            continue;
        };
        let epos = Vector2::new(inst.position.p.x, inst.position.p.z);
        let d = na::distance(&my_pos.into(), &epos.into());
        if d <= radius && best.as_ref().map(|(_, bd)| d < *bd).unwrap_or(true) {
            best = Some((oid.clone(), d));
        }
    }
    best.map(|(oid, _)| oid)
}

/// Once-a-second "time remaining" nag for players still inside their
/// post-slot-entry takeoff hold. Called from the 1 Hz timed-events loop.
fn announce_takeoff_holds(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let mut nags: Vec<(UnitId, i64)> = vec![];
    // (ucid, slot, uid, seconds still remaining). Anyone who is already airborne
    // with time left on the hold gets booted -- this is the reliable check
    // (position, every tick), not just the S_EVENT_TAKEOFF handler.
    let mut boot: Vec<(Ucid, SlotId, UnitId, i64)> = vec![];
    // (ucid, uid): hold just expired -- one "cleared" message, then forget them.
    let mut cleared: Vec<(Ucid, UnitId)> = vec![];
    for (ucid, player, inst) in ctx.db.instanced_players() {
        let Some(ok_at) = inst.takeoff_ok_at else { continue };
        let Some((slot, _)) = player.current_slot.as_ref() else { continue };
        let Some(uid) = slot.as_unit_id() else { continue };
        if now >= ok_at {
            cleared.push((*ucid, uid));
            continue;
        }
        let secs = (ok_at - now).num_seconds().max(1);
        if inst.in_air {
            boot.push((*ucid, slot.clone(), uid, secs));
        } else {
            nags.push((uid, secs));
        }
    }
    for (uid, secs) in nags {
        ctx.db.ephemeral.msgs().panel_to_unit(
            1,
            false,
            uid,
            format_compact!("Takeoff hold: {secs}s remaining"),
        );
    }
    for (ucid, uid) in cleared {
        ctx.db.ephemeral.msgs().panel_to_unit(
            30,
            false,
            uid,
            format_compact!("Takeoff hold cleared -- happy hunting!"),
        );
        if let Some(p) = ctx.db.persisted.players.get_mut_cow(&ucid) {
            if let Some((_, Some(inst))) = p.current_slot.as_mut() {
                inst.takeoff_ok_at = None;
            }
        }
    }
    for (ucid, slot, uid, secs) in boot {
        info!("takeoff-hold violation: {ucid} airborne with {secs}s left -> spectators");
        ctx.db.ephemeral.msgs().panel_to_unit(
            15,
            true,
            uid,
            format_compact!(
                "You got airborne {secs}s before your ground hold expired -- returning to spectators."
            ),
        );
        // Clear the flag so this doesn't re-fire while the deslot settles.
        if let Some(p) = ctx.db.persisted.players.get_mut_cow(&ucid) {
            if let Some((_, Some(inst))) = p.current_slot.as_mut() {
                inst.takeoff_ok_at = None;
            }
        }
        if let Some(oid) = ctx.db.ephemeral.get_object_id_by_slot(&slot).cloned() {
            if let Ok(u) = Unit::get_instance(lua, &oid) {
                let _ = u.destroy();
            }
        }
        ctx.db.ephemeral.force_player_to_spectators(&ucid);
    }
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
        if ts - *landed_ts < Duration::seconds(10) {
            return true;
        }
        let unit = or_false!(Unit::get_instance(lua, id));
        let pos = or_false!(unit.get_ground_position());
        let slot = or_false!(unit.slot());
        if let Some(typ) = db.land(slot.clone(), pos.0, &unit) {
            returned.push((typ, slot));
        }
        // The landing is processed either way, so always drop the entry.
        // Keeping it whenever no life came back -- landed outside an owned
        // objective, or already at full lives -- re-ran `db.land` on every slow
        // tick and left the unit permanently "recently landed", which silently
        // swallowed its next takeoff.
        false
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
            let obj = ctx.db.objective(oid)?;
            let m = match obj.owner {
                Side::Neutral => format_compact!("{} (neutral) is now capturable", obj.name()),
                owner => format_compact!(
                    "{} ({owner:?}) is now capturable by {:?}",
                    obj.name(),
                    owner.opposite()
                ),
            };
            ctx.db.ephemeral.msgs().panel_to_all(30, false, m);
        }
    }
    ctx.captureable.retain(|oid, _| cur_cap.contains(oid));
    Ok(())
}

fn advise_captured(ctx: &mut Context, lua: MizLua, ts: DateTime<Utc>) -> Result<bool> {
    let mut has_captures = false;
    if let Err(e) = ctx.db.check_capture_hold(ts) {
        error!("check_capture_hold failed: {e:?}");
    }
    for (side, oid) in ctx.db.check_capture(lua, ts)? {
        has_captures = true;
        ctx.event_scheduler.owned_cache_dirty = true;
        let (name, pos) = {
            let obj = ctx.db.objective(&oid)?;
            (obj.name().to_owned(), obj.pos())
        };
        let mark_text = format_compact!("{} captured by {:?}", name, side);
        ctx.db.ephemeral.msgs().mark_to_all(pos, true, mark_text.clone());
        crate::api::dispatch_event(lua, "alert", &mark_text);
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
        // Announce at most one warning per tick, the tightest threshold that
        // has just been crossed. If the restart time moves in (or the engine
        // was busy), crossing three thresholds at once says "5 minutes" once
        // rather than stacking three lines that contradict each other.
        let left = asd.when - now;
        let mut due: Option<usize> = None;
        for (i, (mins, _)) in SHUTDOWN_WARNINGS.iter().enumerate() {
            if !asd.warned[i] && left <= Duration::minutes(*mins) {
                asd.warned[i] = true;
                due = Some(i);
            }
        }
        if let Some(i) = due {
            let msg = format_compact!("The server will restart in {}", SHUTDOWN_WARNINGS[i].1);
            ctx.db.ephemeral.msgs().panel_to_all(60, i > 0, msg);
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
                // Expected when the player disconnected between being queued
                // for this and the scheduled time arriving -- nothing to
                // enforce on someone who already left, and this entry isn't
                // retried (players_to_force_to_spectators consumes it via
                // split_off regardless), so it's a one-shot no-op, not a bug.
                None => debug!("no id for player ucid {:?} (already disconnected)", ucid),
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
            // A JTAC appearing at (or leaving) a location that is not on any
            // player's expanded list still changes the location list itself, so
            // every player on that side needs the root rebuilt. That is three
            // DCS calls per slot and the set only moves when a JTAC is created,
            // destroyed, or crosses into another objective's area, so it is far
            // cheaper than it looks.
            let mut live: FxHashMap<Side, FxHashSet<ObjectiveId>> = FxHashMap::default();
            for jtac in ctx.jtac.jtacs() {
                live.entry(jtac.side())
                    .or_default()
                    .insert(jtac.location().oid);
            }
            let empty: FxHashSet<ObjectiveId> = FxHashSet::default();
            let moved: SmallVec<[Side; 3]> = [Side::Blue, Side::Red, Side::Neutral]
                .into_iter()
                .filter(|side| {
                    live.get(side).unwrap_or(&empty) != ctx.jtac_locations.get(side).unwrap_or(&empty)
                })
                .collect();
            if !moved.is_empty() {
                ctx.jtac_locations = live;
                for (_, player, _) in ctx.db.instanced_players() {
                    if !moved.contains(&player.side) {
                        continue;
                    }
                    if let Some((slot, _)) = player.current_slot.as_ref() {
                        if !dirty_slots.contains(slot) {
                            dirty_slots.push(*slot);
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

                    // If already in range, nudge the battery to face the target then
                    // fire in place (aim_and_fire_route). If out of range, drive it to
                    // arty_range from the target along the target->group vector; that
                    // long leg already aligns the hull, so fire on arrival.
                    let mission = if dist <= arty_range {
                        aim_and_fire_route(
                            group_pos,
                            target_pos,
                            group_facing(&ctx.db, gid),
                            fire_task.clone(),
                        )
                    } else {
                        // Step from target toward group at arty_range distance.
                        let dir = (group_pos - target_pos).normalize();
                        let waypoint_pos = target_pos + dir * arty_range;
                        Task::Mission {
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
                        }
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
                    let center = ctx.db.group_center(gid).unwrap_or(target_pos);
                    let facing = group_facing(&ctx.db, gid);
                    if let Ok(dcs_group) = Group::get_by_name(lua, group_name.as_str()) {
                        if let Ok(controller) = dcs_group.get_controller() {
                            let task =
                                aim_and_fire_route(center, target_pos, facing, fire_task.clone());
                            if let Err(e) = controller.set_task(task) {
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
            EventEffect::SpawnCap { event_id, cap_side, objective, obj_pos, rotary, threat } => {
                // CAP must ground-start from a real airbase (startup + taxi +
                // takeoff is what gives flights their time separation). If the
                // objective has no resolvable runtime airbase, don't spawn an
                // air-started flight as a fallback -- cancel the event instead.
                // Exactly the resolver spawn_group uses, so a field can't pass
                // here and then fail to produce a parking start at spawn time.
                //
                // Helicopters skip this gate. Most FOBs have no DCS airbase or
                // FARP pad object inside them at all, and spawn_group's
                // TakeOffGroundHot fallback lifts a rotary flight off open
                // ground there perfectly well -- the same discovery that let AI
                // logistics helos launch from the fields nearest the fight
                // instead of 150 km away.
                let airbase_resolvable = rotary
                    || ctx
                        .db
                        .ephemeral
                        .resolve_airbase(lua, &ctx.db.persisted, &objective)
                        .is_some();
                if !airbase_resolvable {
                    warn!(
                        "SpawnCap: objective {:?} has no resolvable airbase -- cancelling CAP event, not spawning",
                        objective
                    );
                    ctx.event_scheduler.active_events.retain(|ev| ev.id() != event_id);
                    if let Some(marks) = ctx.event_scheduler.event_marks.remove(&event_id) {
                        for mid in marks {
                            ctx.db.ephemeral.msgs().delete_mark(mid);
                        }
                    }
                    continue;
                }
                let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
                // Candidate templates in pick order. A configured roster gives
                // several, weighted and threat-tiered; with no roster this is
                // just the one legacy name. Trying them in order means a
                // missing group in the .miz costs that entry, not the scramble.
                let candidates =
                    cap_template_candidates(cfg.campaign_events.as_deref(), cap_side, rotary, threat);
                // Spawn directly at the airbase center; template should be a ground hotstart
                let spawn_pos = obj_pos;
                let mut spawned: Option<(GroupId, dcso3::String)> = None;
                for template in &candidates {
                    match ctx.db.add_and_queue_group(
                        &spctx,
                        &ctx.idx,
                        cap_side,
                        SpawnLoc::AtPos {
                            pos: spawn_pos,
                            offset_direction: dcso3::Vector2::new(1., 0.),
                            group_heading: 0.,
                        },
                        template,
                        DeployKind::Objective { origin: objective },
                        {
                            // Start on the ramp with the engines OFF and let the AI
                            // run its own startup. The minutes of startup, taxi and
                            // takeoff are the point: they are the defending side's
                            // reaction delay and the attacking side's warning. Both
                            // tags take the same waypoint-0 rewrite in spawn_group;
                            // they differ only in engines-running.
                            let cold = cfg
                                .campaign_events
                                .as_ref()
                                .map(|c| if rotary { c.helo_cold_start } else { c.cap_cold_start })
                                .unwrap_or(true);
                            let mut tags: enumflags2::BitFlags<bfprotocols::cfg::UnitTag> =
                                bfprotocols::cfg::UnitTag::CAP.into();
                            if cold {
                                tags |= bfprotocols::cfg::UnitTag::ColdStart;
                            }
                            tags
                        },
                        None,
                    ) {
                        Ok(gid) => {
                            spawned = Some((gid, template.clone()));
                            break;
                        }
                        Err(e) => warn!(
                            "SpawnCap: {} template '{}' not found -- add a {} group named '{}' to your mission file. ({e:?})",
                            if rotary { "helo patrol" } else { "CAP" },
                            template,
                            if rotary { "helicopter-section" } else { "plane-section" },
                            template
                        ),
                    }
                }
                match spawned {
                    Some((gid, template)) => {
                        info!(
                            "SpawnCap: spawned {} {:?} ('{}') for {:?} over {:?} (threat {})",
                            if rotary { "helo patrol" } else { "CAP" },
                            gid,
                            template,
                            cap_side,
                            objective,
                            threat
                        );
                        ctx.event_scheduler.cap_spawn_ts.insert(
                            gid,
                            crate::db::events::CapSpawnWatch::new(Utc::now()),
                        );
                        ctx.event_scheduler.cap_groups
                            .entry(event_id)
                            .or_default()
                            .push(gid);
                        // Record which side owns this CAP event (needed for retargeting).
                        ctx.event_scheduler.cap_side_by_event.insert(event_id, cap_side);
                        // Ceiling on in-contact extensions: a flight may earn at
                        // most its own duration again in overtime, then it goes
                        // home whatever is happening around it.
                        let duration = cfg
                            .campaign_events
                            .as_ref()
                            .map(|c| if rotary { c.helo_duration_secs } else { c.cap_duration_secs })
                            .unwrap_or(600);
                        ctx.event_scheduler.cap_hard_expiry.insert(
                            event_id,
                            Utc::now() + chrono::Duration::seconds(2 * duration as i64),
                        );
                        // Queue initial task -- deferred until DCS reports the group alive.
                        ctx.event_scheduler.pending_cap_tasks.insert(gid, (obj_pos, rotary));
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
                                radius: if rotary { 12_000. } else { 25_000. },
                                color: side_color(cap_side),
                                fill_color: Color::new(0., 0., 0., 0.),
                                line_type: LineType::Solid,
                                read_only: true,
                            },
                            Some(
                                format_compact!(
                                    "Enemy {} [{:?}] — ACTIVE",
                                    if rotary { "helo patrol" } else { "CAP" },
                                    cap_side
                                )
                                .into(),
                            ),
                        );
                        ctx.event_scheduler.register_mark(event_id, mid);
                    }
                    None => {
                        if candidates.is_empty() {
                            warn!(
                                "SpawnCap: {:?} has no {} template roster configured -- \
                                 cancelling event (set {} in campaign_events)",
                                cap_side,
                                if rotary { "helo patrol" } else { "CAP" },
                                if rotary { "helo_templates_red/_blue" } else { "cap_templates_red/_blue" }
                            );
                        } else {
                            warn!(
                                "SpawnCap: none of the {} {} template(s) {:?} could be spawned -- cancelling event",
                                candidates.len(),
                                if rotary { "helo patrol" } else { "CAP" },
                                candidates
                            );
                        }
                        // Cancel the event so it doesn't keep retrying
                        ctx.event_scheduler.active_events.retain(|ev| ev.id() != event_id);
                        ctx.event_scheduler.cap_hard_expiry.remove(&event_id);
                        // Clean up any F10 marks that may have been placed
                        if let Some(marks) = ctx.event_scheduler.event_marks.remove(&event_id) {
                            for mid in marks {
                                ctx.db.ephemeral.msgs().delete_mark(mid);
                            }
                        }
                    }
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
            EventEffect::DespawnCap { event_id, cap_side, objective, rotary } => {
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

                // Start the between-waves cooldown for THE FIELD THIS WAVE
                // LAUNCHED FROM whenever the wave ends -- shot down OR flown
                // its full duration and RTB'd. Per field, not per side: one
                // wave ending on a quiet flank used to ground the entire
                // coalition for half an hour, which made the second per-side
                // slot unreachable in practice. The side-wide throttle is now
                // the sortie budget in check_air_threats.
                ctx.event_scheduler
                    .cap_field_cooldown
                    .insert((objective, rotary), now);
                // The commander's own CAP cooldown still reads these, so keep
                // them current for the fixed-wing case.
                match (cap_side, rotary) {
                    (Side::Blue, false) => ctx.event_scheduler.last_commander_cap_ended_blue = Some(now),
                    (Side::Red, false) => ctx.event_scheduler.last_commander_cap_ended_red = Some(now),
                    (Side::Blue, true) => ctx.event_scheduler.last_helo_patrol_ended_blue = Some(now),
                    (Side::Red, true) => ctx.event_scheduler.last_helo_patrol_ended_red = Some(now),
                    _ => {}
                }
                ctx.event_scheduler.cap_hard_expiry.remove(&event_id);
                info!(
                    "DespawnCap: {:?} {} wave from {:?} ended ({}) — field cooldown started",
                    cap_side,
                    if rotary { "helo patrol" } else { "CAP" },
                    objective,
                    if was_shot_down { "shot down" } else { "timed out / RTB" }
                );



                ctx.event_scheduler.cap_station_by_event.remove(&event_id);
                if let Some(gids) = ctx.event_scheduler.cap_groups.remove(&event_id) {
                    for gid in gids {
                        ctx.event_scheduler.cap_spawn_ts.remove(&gid);
                        let group_name = match ctx.db.persisted.groups.get(&gid) {
                            Some(g) => g.name.clone(),
                            None => continue,
                        };
                        let dcs_group = match dcso3::group::Group::get_by_name(lua, group_name.as_str()) {
                            Ok(g) => g,
                            Err(_) => {
                                // Not in DCS, delete the tracked group immediately
                                let _ = ctx.db.delete_group(&gid);
                                continue;
                            }
                        };
                        
                        let g_pos = match dcs_group.get_unit(1).and_then(|u| u.get_point()) {
                            Ok(p) => dcso3::Vector2::new(p.x, p.z),
                            Err(_) => {
                                let _ = ctx.db.delete_group(&gid);
                                continue;
                            }
                        };

                        let best_obj = ctx.db.persisted.objectives.into_iter()
                            .filter(|(_, obj)| obj.owner == cap_side && is_launch_field(obj.kind(), rotary))
                            .min_by(|(_, a), (_, b)| {
                                let ap = a.pos();
                                let bp = b.pos();
                                let da = {
                                    let dx = ap.x - g_pos.x;
                                    let dy = ap.y - g_pos.y;
                                    dx * dx + dy * dy
                                };
                                let db = {
                                    let dx = bp.x - g_pos.x;
                                    let dy = bp.y - g_pos.y;
                                    dx * dx + dy * dy
                                };
                                da.partial_cmp(&db).unwrap_or(std::cmp::Ordering::Equal)
                            });

                        if let Some((_, obj)) = best_obj {
                            if let Ok(controller) = dcs_group.get_controller() {
                                let _ = controller.set_task(dcso3::controller::Task::Land {
                                    point: dcso3::LuaVec2(obj.pos()),
                                    duration: None,
                                });
                                info!("DespawnCap: sent {} to RTB at {}", group_name, obj.name());
                                // Hand the flight to flush_cap_rtb, which deletes
                                // it once it is down. Without this hand-off the
                                // RTB order was the last anyone ever heard of it:
                                // it landed, parked, and stayed alive and
                                // weapons-free at the field for the rest of the
                                // campaign, save file included.
                                ctx.event_scheduler.cap_rtb.insert(gid, now);
                            } else {
                                let _ = ctx.db.delete_group(&gid);
                            }
                        } else {
                            // If no friendly airbase is available, just despawn
                            let _ = ctx.db.delete_group(&gid);
                        }
                    }
                }
                info!(
                    "DespawnCap: {} event {:?} expired, aircraft commanded to RTB",
                    if rotary { "helo patrol" } else { "CAP" },
                    event_id
                );
            }


        }
    }
}



/// The templates a scramble may spawn, best candidate first.
///
/// The side's roster (`cap_templates_red`/`_blue`, or the helo pair) filtered
/// to the entries this incursion qualifies for -- `min_threat` at or below
/// `threat` -- shuffled by weight, so the same airfield does not answer every
/// push with the same jets. If the filter leaves nothing (every entry wants a
/// bigger fight than actually showed up) the unfiltered roster is used:
/// something scrambling beats nothing.
///
/// Returning the whole ordered list rather than one pick is what lets the
/// caller fall through to the next candidate when a group is missing from the
/// .miz -- a typo then costs that entry, not every scramble the side makes.
///
/// Empty only when the roster is, which `Cfg::load` already refuses for an
/// enabled system; the caller cancels the event and says so.
fn cap_template_candidates(
    cfg: Option<&bfprotocols::cfg::CampaignEventsCfg>,
    side: Side,
    rotary: bool,
    threat: u32,
) -> SmallVec<[dcso3::String; 4]> {
    use rand::Rng;
    let Some(cfg) = cfg else { return SmallVec::new() };
    let roster = match (side, rotary) {
        (Side::Red, false) | (Side::Neutral, false) => &cfg.cap_templates_red,
        (Side::Blue, false) => &cfg.cap_templates_blue,
        (Side::Red, true) | (Side::Neutral, true) => &cfg.helo_templates_red,
        (Side::Blue, true) => &cfg.helo_templates_blue,
    };
    let mut pool: Vec<&bfprotocols::cfg::CapTemplateCfg> =
        roster.iter().filter(|t| t.min_threat <= threat).collect();
    if pool.is_empty() {
        pool = roster.iter().collect();
    }
    // Weighted sampling without replacement: repeatedly draw from the total
    // remaining weight. The order matters beyond the first pick, because the
    // caller walks it as a fallback chain.
    let mut rng = rand::thread_rng();
    let mut out = SmallVec::new();
    while !pool.is_empty() {
        let total: u32 = pool.iter().map(|t| t.weight.max(1)).sum();
        let mut roll = rng.gen_range(0..total);
        let mut chosen = pool.len() - 1;
        for (i, t) in pool.iter().enumerate() {
            let w = t.weight.max(1);
            if roll < w {
                chosen = i;
                break;
            }
            roll -= w;
        }
        out.push(dcso3::String::from(pool.remove(chosen).template.as_str()));
    }
    out
}

/// Build the on-station task for one flight of a reactive air response.
///
/// Fixed-wing CAP orbits an absolute barometric block and engages air targets.
/// A helicopter patrol orbits low over the ground at `helo_altitude_agl_m`
/// ABOVE the terrain under its station -- an absolute altitude that works at
/// the coast puts a helo inside a ridge inland, which is the same trap the AI
/// logistics helos fell into -- and engages helicopters and ground units.
/// Fixed-wing is deliberately absent from its target list: an attack helo told
/// to chase a fast mover only flies itself somewhere it dies.
fn air_station_task<'lua>(
    station: dcso3::Vector2,
    rotary: bool,
    cfg: &bfprotocols::cfg::CampaignEventsCfg,
    land: Option<&dcso3::land::Land>,
) -> dcso3::controller::Task<'lua> {
    use dcso3::attribute::Attribute;
    use dcso3::controller::{OrbitPattern, Task};
    let (speed, altitude, engage, targets) = if rotary {
        let ground = land
            .and_then(|l| l.get_height(dcso3::LuaVec2(station)).ok())
            .unwrap_or(0.);
        (
            cfg.helo_speed_ms,
            ground + cfg.helo_altitude_agl_m,
            cfg.helo_engage_radius_m,
            vec![Attribute::Helicopters, Attribute::GroundUnits],
        )
    } else {
        (
            cfg.cap_speed_ms,
            cfg.cap_altitude_m,
            cfg.cap_engage_radius_m,
            vec![Attribute::Air],
        )
    };
    // ComboTask keeps them on station (break to engage, then return) instead of
    // completing a one-shot task and RTBing.
    Task::ComboTask(vec![
        Task::Orbit {
            pattern: OrbitPattern::Circle,
            point: Some(dcso3::LuaVec2(station)),
            point2: None,
            speed: Some(speed),
            altitude: Some(altitude),
        },
        Task::EngageTargetsInZone {
            point: dcso3::LuaVec2(station),
            zone_radius: engage,
            target_types: targets,
            priority: None,
        },
    ])
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

    // Flush pending CAP orbit tasks (same retry pattern as moves above).
    // Look at every pending flight, not just one: a flight that is still
    // sitting on the ramp is skipped without doing any Lua work beyond a name
    // lookup, and with cold starts that wait is minutes long -- taking only the
    // first would let one starting flight starve every other flight's initial
    // task for its whole startup. The expensive part, set_task, is still capped
    // per tick.
    const CAP_TASKS_PER_TICK: usize = 2;
    let mut cap_tasks_issued = 0usize;
    let pending_cap: Vec<_> = ctx.event_scheduler.pending_cap_tasks.iter()
        .map(|(gid, pos)| (*gid, *pos))
        .collect();

    let events_cfg = ctx
        .db
        .ephemeral
        .cfg
        .campaign_events
        .clone()
        .unwrap_or_default();
    for (gid, (orbit_center, rotary)) in pending_cap {
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
        // Don't task the flight until it's actually airborne. A CAP group
        // ground-starts with a TakeOffParkingHot first waypoint; issuing an
        // Orbit/EngageTargetsInZone task with `set_task` while it's still parked
        // wipes that waypoint, so DCS abandons startup/taxi/takeoff and the
        // flight pops into the air. Leave it pending and retry next tick --
        // `retarget_cap_groups` guards the same way.
        if !dcs_group.get_unit(1).and_then(|u| u.in_air()).unwrap_or(false) {
            continue;
        }
        let controller = match dcs_group.get_controller() {
            Ok(c) => c,
            Err(e) => {
                error!("flush_pending_cap_tasks: get_controller {group_name}: {e}");
                ctx.event_scheduler.pending_cap_tasks.remove(&gid);
                continue;
            }
        };
        // Initial task: climb to the patrol block and orbit the spawn point
        // with a MODEST engage leash. `retarget_cap_groups` re-stations it
        // toward the real threat on the next slow tick. The old 200 km engage
        // radius meant a fresh flight immediately bolted cross-map at the
        // nearest contact, straight through enemy SAM belts -- spawn, die,
        // respawn on a loop.
        let hunt = air_station_task(orbit_center, rotary, &events_cfg, Some(&land));
        if let Err(e) = controller.set_task(hunt) {
            error!("flush_pending_cap_tasks: set_task {group_name}: {e}");
        } else {
            info!(
                "flush_pending_cap_tasks: {} {group_name} initial hunt from {:?}",
                if rotary { "helo patrol" } else { "CAP" },
                orbit_center
            );
        }
        ctx.event_scheduler.pending_cap_tasks.remove(&gid);
        cap_tasks_issued += 1;
        if cap_tasks_issued >= CAP_TASKS_PER_TICK {
            break;
        }
    }

}

/// Each slow tick, station every active CAP flight over the air threat its
/// side's radar network is actually painting -- the full EWR + AWACS + SAM
/// search-radar picture (`Ewr::detected_enemy_positions`), not just human
/// players. Each flight is sent to the detected hostile nearest the objective
/// it defends and holds a CAP station there (orbit + engage-in-zone), so it
/// shows up to the fight instead of circling home until the event expires and
/// it RTBs. With nothing detected it orbits the defended objective rather than
/// running a one-shot sweep that completes and sends it home.
fn retarget_cap_groups(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    use dcso3::land::Land;
    use dcso3::Vector2;
    use crate::db::events::CampaignEvent;

    // Restation only when the target has moved meaningfully -- re-issuing an
    // identical task every tick interrupts an in-progress intercept. A
    // helicopter patrol works a much smaller box, so its threshold is smaller
    // too: 15 km of drift is most of a rotary patrol's whole area.
    const RESTATION_THRESHOLD_M: f64 = 15_000.0;
    const ROTARY_RESTATION_THRESHOLD_M: f64 = 5_000.0;
    let events_cfg = ctx
        .db
        .ephemeral
        .cfg
        .campaign_events
        .clone()
        .unwrap_or_default();
    // Terrain lookup for rotary orbit altitudes. Not fatal if it fails -- the
    // patrol just falls back to an orbit measured from sea level.
    let land = Land::singleton(lua).ok();

    let cap_events: Vec<_> = ctx
        .event_scheduler
        .active_events
        .iter()
        .filter_map(|e| match e {
            CampaignEvent::EnemyCap { id, cap_side, objective, rotary, .. } => {
                Some((*id, *cap_side, *objective, *rotary))
            }
            CampaignEvent::CommanderCap { id, cap_side, objective, .. } => {
                Some((*id, *cap_side, *objective, false))
            }
            _ => None,
        })
        .collect();

    for (event_id, cap_side, objective, rotary) in cap_events {
        let (cap_push, cap_idle_rtb, engage_radius) = if rotary {
            (
                events_cfg.helo_max_push_m,
                events_cfg.helo_idle_rtb_secs,
                events_cfg.helo_engage_radius_m,
            )
        } else {
            (
                events_cfg.cap_max_push_m,
                events_cfg.cap_idle_rtb_secs,
                events_cfg.cap_engage_radius_m,
            )
        };
        let gids = match ctx.event_scheduler.cap_groups.get(&event_id) {
            Some(v) => v.clone(),
            None => continue,
        };

        // The objective this CAP is defending -- its fallback station.
        let home = ctx.db.persisted.objectives.get(&objective).map(|o| o.pos());

        // Everything cap_side's radar network sees, plus any airborne enemy
        // player (covers radar gaps) -- narrowed to helicopters for a rotary
        // patrol, which cannot work a jet track and should not be sent chasing
        // one.
        let mut threats = if rotary {
            ctx.ewr.detected_enemy_helo_positions(cap_side, now, &ctx.db)
        } else {
            ctx.ewr.detected_enemy_positions(cap_side, now)
        };
        threats.extend(enemy_player_positions(&ctx.db, cap_side.opposite(), rotary));

        // Idle CAP -> RTB. If this flight's side has painted nothing to work for
        // `cap_idle_rtb` seconds, expire the event now: the DespawnCap effect
        // already sends the group home, and the picture stops accumulating
        // flights that have no job.
        if !threats.is_empty() {
            ctx.event_scheduler.cap_last_threat_seen.insert(event_id, now);
        } else {
            let since = *ctx
                .event_scheduler
                .cap_last_threat_seen
                .entry(event_id)
                .or_insert(now);
            if (now - since).num_seconds() >= cap_idle_rtb as i64 {
                for e in ctx.event_scheduler.active_events.iter_mut() {
                    match e {
                        CampaignEvent::EnemyCap { id, expires_at, .. }
                        | CampaignEvent::CommanderCap { id, expires_at, .. }
                            if *id == event_id =>
                        {
                            *expires_at = now;
                        }
                        _ => {}
                    }
                }
                ctx.event_scheduler.cap_last_threat_seen.remove(&event_id);
                info!(
                    "retarget_cap_groups: {} {:?} idle {}s -> RTB",
                    if rotary { "helo patrol" } else { "CAP" },
                    event_id,
                    cap_idle_rtb
                );
                continue;
            }
        }

        let station: Option<Vector2> = match (home, threats.is_empty()) {
            // Nearest detected threat to the defended objective.
            (Some(h), false) => threats.iter().copied().min_by(|a, b| {
                na::distance_squared(&(*a).into(), &h.into())
                    .partial_cmp(&na::distance_squared(&(*b).into(), &h.into()))
                    .unwrap_or(std::cmp::Ordering::Equal)
            }),
            // Threats but no known objective -- go to their centroid.
            (None, false) => {
                let n = threats.len() as f64;
                let sum = threats
                    .iter()
                    .fold(Vector2::new(0., 0.), |acc, p| Vector2::new(acc.x + p.x, acc.y + p.y));
                Some(Vector2::new(sum.x / n, sum.y / n))
            }
            // Nothing detected -- hold over the objective we're defending.
            (Some(h), true) => Some(h),
            (None, true) => None,
        };
        let Some(station) = station else { continue };

        // Keep the flight from chasing a contact deep across the front: never
        // push more than `cap_push` metres from the objective it's defending.
        let station = match home {
            Some(h) => {
                let d = na::distance(&h.into(), &station.into());
                if d > cap_push && d > 1.0 {
                    let t = cap_push / d;
                    Vector2::new(h.x + (station.x - h.x) * t, h.y + (station.y - h.y) * t)
                } else {
                    station
                }
            }
            None => station,
        };

        // A flight still in contact does not get sent home mid-fight. While
        // something it can work is inside its engage radius, its clock keeps
        // getting pushed out -- bounded by cap_hard_expiry, set at spawn, so a
        // stream of players can't keep one flight up forever. Without this the
        // wall clock wins an argument with a merge in progress, and the AI
        // breaks off for no reason anyone in the air can see.
        let extension = events_cfg.cap_engaged_extension_secs as i64;
        if extension > 0 {
            let in_contact = threats.iter().any(|t| {
                na::distance(&(*t).into(), &station.into()) <= engage_radius
            });
            if in_contact {
                let hard = ctx.event_scheduler.cap_hard_expiry.get(&event_id).copied();
                for e in ctx.event_scheduler.active_events.iter_mut() {
                    match e {
                        CampaignEvent::EnemyCap { id, expires_at, .. }
                        | CampaignEvent::CommanderCap { id, expires_at, .. }
                            if *id == event_id =>
                        {
                            let want = now + chrono::Duration::seconds(extension);
                            let want = match hard {
                                Some(h) if want > h => h,
                                _ => want,
                            };
                            if want > *expires_at {
                                *expires_at = want;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        // Skip if we're already stationed here (within threshold).
        let threshold = if rotary {
            ROTARY_RESTATION_THRESHOLD_M
        } else {
            RESTATION_THRESHOLD_M
        };
        let restation = ctx
            .event_scheduler
            .cap_station_by_event
            .get(&event_id)
            .map_or(true, |prev| na::distance(&(*prev).into(), &station.into()) > threshold);
        if !restation {
            continue;
        }

        let task = air_station_task(station, rotary, &events_cfg, land.as_ref());

        let mut any_tasked = false;
        for gid in gids {
            let group_name = match ctx.db.persisted.groups.get(&gid) {
                Some(g) => g.name.clone(),
                None => continue,
            };
            let dcs_group = match dcso3::group::Group::get_by_name(lua, group_name.as_str()) {
                Ok(g) => g,
                Err(_) => continue, // not in DCS yet
            };
            // Wait until the group takes off before retargeting, otherwise
            // set_task interrupts their TakeOffParkingHot task.
            if !dcs_group.get_unit(1).and_then(|u| u.in_air()).unwrap_or(false) {
                continue;
            }
            let controller = match dcs_group.get_controller() {
                Ok(c) => c,
                Err(e) => {
                    error!("retarget_cap_groups: get_controller {group_name}: {e}");
                    continue;
                }
            };
            if let Err(e) = controller.set_task(task.clone()) {
                error!("retarget_cap_groups: set_task {group_name}: {e}");
            } else {
                any_tasked = true;
            }
        }
        if any_tasked {
            ctx.event_scheduler.cap_station_by_event.insert(event_id, station);
        }
    }
}

/// How long a flight sent home gets to actually get there before it is removed
/// wherever it is. Generous enough for a helo crossing half a front at 60 m/s.
const CAP_RTB_GIVEUP_SECS: i64 = 1200;

/// Delete CAP flights that have finished their RTB.
///
/// `DespawnCap` orders a flight home and registers it here; this sweep removes
/// each one once its lead unit is on the ground (landed), once DCS no longer
/// knows the group (crashed or shot down on the way home), or once it has had
/// `CAP_RTB_GIVEUP_SECS` to make the trip. Until this existed nothing ever
/// removed them: every wave that timed out left a live AI flight parked at its
/// field for the rest of the campaign, eating parking and riding along in the
/// save file.
fn flush_cap_rtb(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let pending: Vec<(GroupId, DateTime<Utc>)> = ctx
        .event_scheduler
        .cap_rtb
        .iter()
        .map(|(g, t)| (*g, *t))
        .collect();
    for (gid, ordered_at) in pending {
        let overdue = (now - ordered_at).num_seconds() >= CAP_RTB_GIVEUP_SECS;
        let group_name = match ctx.db.persisted.groups.get(&gid) {
            Some(g) => g.name.clone(),
            None => {
                // Already gone from the campaign db -- nothing left to do.
                ctx.event_scheduler.cap_rtb.remove(&gid);
                continue;
            }
        };
        let down = match dcso3::group::Group::get_by_name(lua, group_name.as_str()) {
            // No group in DCS: it died on the way home, or was cleaned up
            // elsewhere. Either way the db entry should follow it.
            Err(_) => true,
            Ok(g) => match g.get_unit(1).and_then(|u| u.in_air()) {
                Ok(in_air) => !in_air,
                // Can't read the lead unit (destroyed mid-query) -- treat as down.
                Err(_) => true,
            },
        };
        if down || overdue {
            if let Err(e) = ctx.db.delete_group(&gid) {
                error!("flush_cap_rtb: could not delete {group_name}: {e:?}");
            } else {
                info!(
                    "flush_cap_rtb: removed {group_name} after RTB ({})",
                    if down { "landed" } else { "overdue" }
                );
            }
            ctx.event_scheduler.cap_rtb.remove(&gid);
        }
    }
}

/// Below this age, one airborne sighting with no prior ground sighting is
/// already conclusive -- nothing hot-starts and gets its wheels up this fast.
const CAP_FAST_AIRSTART_SECS: i64 = 40;
/// Hard stop: stop watching a CAP group we still can't query after this long
/// (its spawn failed or it was cleaned up some other way).
const CAP_WATCH_GIVEUP_SECS: i64 = 240;

/// Despawn any CAP event whose flight air-started instead of taxiing out from
/// a runway (bad template WP0, or DCS ignoring the parking start).
///
/// Not time-boxed: the watch is dropped the instant the lead unit is seen on
/// the ground (a real ground start). A flight that is only ever seen airborne
/// -- fast, or over two consecutive checks -- air-started and the whole event
/// is scrapped. Spawn-queue lag therefore can't wave one through the way the
/// old wall-clock grace window did.
fn enforce_cap_ground_start(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let checks: Vec<(GroupId, crate::db::events::CapSpawnWatch)> = ctx
        .event_scheduler
        .cap_spawn_ts
        .iter()
        .map(|(g, w)| (*g, *w))
        .collect();
    for (gid, watch) in checks {
        let age = (now - watch.spawned_at).num_seconds();
        let group_name = match ctx.db.persisted.groups.get(&gid) {
            Some(g) => g.name.clone(),
            None => {
                ctx.event_scheduler.cap_spawn_ts.remove(&gid);
                continue;
            }
        };
        // None = couldn't query the unit yet (not spawned / not alive). Keep
        // waiting -- do NOT treat "unknown" as "fine".
        let in_air: Option<bool> = dcso3::group::Group::get_by_name(lua, group_name.as_str())
            .ok()
            .and_then(|g| g.get_unit(1).ok())
            .and_then(|u| u.in_air().ok());
        match in_air {
            None => {
                if age >= CAP_WATCH_GIVEUP_SECS {
                    ctx.event_scheduler.cap_spawn_ts.remove(&gid);
                }
                continue;
            }
            Some(false) => {
                // On the ground -- genuine ground start. Stop watching; it will
                // taxi and take off normally.
                ctx.event_scheduler.cap_spawn_ts.remove(&gid);
                continue;
            }
            Some(true) => {
                let strikes = watch.airborne_strikes + 1;
                let conclusive = age < CAP_FAST_AIRSTART_SECS || strikes >= 2;
                if !conclusive {
                    // First airborne sighting, and late enough that it *might*
                    // have hot-started and taken off inside a gap between
                    // checks. Give it one more look before scrapping.
                    if let Some(w) = ctx.event_scheduler.cap_spawn_ts.get_mut(&gid) {
                        w.airborne_strikes = strikes;
                    }
                    continue;
                }
            }
        }
        // Fell through => air-started. Tear down the owning event entirely.
        {
            let event_id = ctx
                .event_scheduler
                .cap_groups
                .iter()
                .find(|(_, gids)| gids.contains(&gid))
                .map(|(eid, _)| *eid);
            warn!(
                "[CAP_SPAWN] {group_name} air-started ({age}s after spawn, never seen on the \
                 ground) -- despawning the CAP event",
            );
            if let Some(eid) = event_id {
                for g in ctx.event_scheduler.cap_groups.remove(&eid).unwrap_or_default() {
                    let _ = ctx.db.delete_group(&g);
                    ctx.event_scheduler.pending_cap_tasks.remove(&g);
                    ctx.event_scheduler.cap_spawn_ts.remove(&g);
                }
                ctx.event_scheduler.active_events.retain(|ev| ev.id() != eid);
                ctx.event_scheduler.cap_side_by_event.remove(&eid);
                ctx.event_scheduler.cap_station_by_event.remove(&eid);
                if let Some(marks) = ctx.event_scheduler.event_marks.remove(&eid) {
                    for mid in marks {
                        ctx.db.ephemeral.msgs().delete_mark(mid);
                    }
                }
            } else {
                let _ = ctx.db.delete_group(&gid);
                ctx.event_scheduler.cap_spawn_ts.remove(&gid);
            }
        }
    }
}

/// Count of `enemy_side` players airborne in one airframe class, straight from
/// the DB (radar-independent). `rotary` selects helicopters instead of
/// fixed-wing. Backs `cap_trigger_on_known_players` /
/// `helo_trigger_on_known_players` and both balance gaps.
fn enemy_players_airborne(db: &db::Db, enemy_side: Side, rotary: bool) -> usize {
    use bfprotocols::cfg::UnitTag;
    db.instanced_players()
        .filter(|(_, p, inst)| {
            p.side == enemy_side
                && inst.in_air
                && db
                    .ephemeral
                    .cfg
                    .unit_classification
                    .get(&inst.typ)
                    .map(|t| {
                        if rotary {
                            t.contains(UnitTag::Helicopter)
                        } else {
                            t.contains(UnitTag::Aircraft) && !t.contains(UnitTag::Helicopter)
                        }
                    })
                    // Unknown type: assume a jet. Counting it as a helo would
                    // scramble the rotary response at something it can't catch.
                    .unwrap_or(!rotary)
        })
        .count()
}

/// Positions of `enemy_side` players of one airframe class in the air
/// (radar-independent).
fn enemy_player_positions(db: &db::Db, enemy_side: Side, rotary: bool) -> Vec<Vector2> {
    use bfprotocols::cfg::UnitTag;
    db.instanced_players()
        .filter_map(|(_, p, inst)| {
            if p.side != enemy_side || !inst.in_air {
                return None;
            }
            let ok = db
                .ephemeral
                .cfg
                .unit_classification
                .get(&inst.typ)
                .map(|t| {
                    if rotary {
                        t.contains(UnitTag::Helicopter)
                    } else {
                        t.contains(UnitTag::Aircraft) && !t.contains(UnitTag::Helicopter)
                    }
                })
                .unwrap_or(!rotary);
            ok.then(|| Vector2::new(inst.position.p.x, inst.position.p.z))
        })
        .collect()
}

/// The knobs one reactive-air-response pass reads, resolved for either the
/// fixed-wing or the rotary side of `CampaignEventsCfg`. Everything downstream
/// of this struct is shared, which is the whole point: a helicopter patrol is
/// reactive CAP with rotary numbers, rotary templates and rotary launch
/// fields, not a second copy of the machinery.
struct ThreatResponseCfg {
    /// Label for logs and the threat warning players receive.
    label: &'static str,
    trigger_radius_m: f64,
    max_concurrent: usize,
    max_per_side: usize,
    min_threat: u32,
    /// Per-FIELD cooldown between waves, not per side.
    respawn_cooldown_secs: u64,
    duration_secs: u32,
    trigger_on_known_players: bool,
    balance_gap: u32,
    /// Side-wide launches allowed per rolling hour. 0 = unlimited.
    max_sorties_per_hour: u32,
}

impl ThreatResponseCfg {
    fn resolve(c: &bfprotocols::cfg::CampaignEventsCfg, rotary: bool) -> Self {
        if rotary {
            Self {
                label: "Reactive helo patrol",
                trigger_radius_m: c.helo_trigger_radius_m,
                max_concurrent: c.helo_max_concurrent,
                max_per_side: c.helo_max_per_side,
                min_threat: c.helo_min_threat_count,
                respawn_cooldown_secs: c.helo_respawn_cooldown_secs,
                duration_secs: c.helo_duration_secs,
                trigger_on_known_players: c.helo_trigger_on_known_players,
                balance_gap: c.helo_balance_gap,
                max_sorties_per_hour: c.helo_max_sorties_per_hour,
            }
        } else {
            Self {
                label: "Reactive CAP",
                trigger_radius_m: c.cap_trigger_radius_m,
                max_concurrent: c.cap_max_concurrent,
                max_per_side: c.cap_max_per_side,
                min_threat: c.cap_min_threat_count,
                respawn_cooldown_secs: c.cap_respawn_cooldown_secs,
                duration_secs: c.cap_duration_secs,
                trigger_on_known_players: c.cap_trigger_on_known_players,
                balance_gap: c.cap_balance_gap,
                max_sorties_per_hour: c.cap_max_sorties_per_hour,
            }
        }
    }
}

/// True if `kind` is a field the response can launch from. Fixed-wing CAP needs
/// a runway; helicopters are happy with a FARP pad or, thanks to the
/// open-ground hot start, a FOB with no pad object at all.
fn is_launch_field(kind: &bfprotocols::db::objective::ObjectiveKind, rotary: bool) -> bool {
    use bfprotocols::db::objective::ObjectiveKind as K;
    if rotary {
        matches!(kind, K::Airbase | K::Farp { .. } | K::Fob)
    } else {
        kind.is_airbase()
    }
}

/// Reactive air response: detect in-air enemy players near owned objectives and
/// scramble AI to meet them. Not an economic commander action -- it fires
/// automatically when real threats appear.
///
/// Called once per slow tick for each airframe class. `rotary == false` is the
/// original fixed-wing CAP: jets answering jets. `rotary == true` is the
/// helicopter patrol: armed AI helos answering enemy helicopter players, which
/// CAP deliberately ignores. Both paths share every step below -- the class
/// only changes which config block, which detection filter, which launch
/// fields and which cooldown are used.
fn check_air_threats(ctx: &mut Context, now: DateTime<Utc>, rotary: bool) {
    use crate::db::events::{bearing_to_compass, CampaignEvent, EventId};
    use dcso3::coalition::Side;
    use dcso3::Vector2;

    let events_cfg = match ctx.db.ephemeral.cfg.campaign_events.as_ref() {
        Some(c)
            if c.enabled && if rotary { c.enemy_helo_enabled } else { c.enemy_cap_enabled } =>
        {
            c.clone()
        }
        _ => return,
    };
    let rc = ThreatResponseCfg::resolve(&events_cfg, rotary);

    let cluster_radius_sq = rc.trigger_radius_m.powi(2);
    let max_concurrent = rc.max_concurrent;
    let max_per_side = rc.max_per_side;
    let min_threat = rc.min_threat as usize;
    let respawn_cooldown = chrono::Duration::seconds(rc.respawn_cooldown_secs as i64);

    // Count active events per side, for THIS class only -- CommanderCap is
    // excluded (it's friendly support), and a jet wave never consumes a
    // helicopter slot or vice versa.
    let mut active_red: fxhash::FxHashSet<bfprotocols::db::objective::ObjectiveId> =
        fxhash::FxHashSet::default();
    let mut active_blue: fxhash::FxHashSet<bfprotocols::db::objective::ObjectiveId> =
        fxhash::FxHashSet::default();
    for e in &ctx.event_scheduler.active_events {
        if let CampaignEvent::EnemyCap { objective, cap_side, rotary: r, .. } = e {
            if *r != rotary {
                continue;
            }
            match cap_side {
                Side::Red  => { active_red.insert(*objective); }
                Side::Blue => { active_blue.insert(*objective); }
                _ => {}
            }
        }
    }

    let total_active = active_red.len() + active_blue.len();
    if total_active >= max_concurrent {
        return; // global cap reached
    }
    if active_red.len() >= max_per_side && active_blue.len() >= max_per_side {
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

    // For each side that still has room, check detections and cluster them.
    // We iterate Red-defends-against-Blue and Blue-defends-against-Red.
    for defending_side in [Side::Red, Side::Blue] {
        let side_active_count = match defending_side {
            Side::Red  => active_red.len(),
            Side::Blue => active_blue.len(),
            _ => continue,
        };
        if side_active_count >= max_per_side {
            continue; // this side is already at its per-side limit
        }
        if active_red.len() + active_blue.len() >= max_concurrent {
            break; // global cap hit mid-loop
        }

        // ── Step 1: gate on enemy PLAYER count in this airframe class ───────────
        // Only enemy PLAYERS trigger a reactive response (AI excluded), and
        // only in the class this pass answers: jets for CAP, helicopters for
        // the helo patrol. Normally this is what the defending side's radar
        // network has actually painted; with `*_trigger_on_known_players` it's
        // the real count of enemy players in the air, so a radar-blind / EMCON
        // side still scrambles. Require at least `min_threat` (default 2).
        let attacking = match defending_side {
            Side::Red => Side::Blue,
            Side::Blue => Side::Red,
            _ => continue,
        };
        let player_threat_count = if rc.trigger_on_known_players {
            enemy_players_airborne(&ctx.db, attacking, rotary)
        } else {
            ctx.ewr
                .detected_enemy_player_count(defending_side, now, &ctx.db, rotary)
        };
        // Air-balance: scramble for the outnumbered side even without a
        // detected incursion (Blue 4 up, Red 1 up → Red gets one). This is the
        // path that covers "tonight only helo pilots showed up".
        let my_air = enemy_players_airborne(&ctx.db, defending_side, rotary);
        let their_air = enemy_players_airborne(&ctx.db, attacking, rotary);
        let outnumbered =
            rc.balance_gap > 0 && their_air >= my_air + rc.balance_gap as usize;
        if player_threat_count < min_threat && !outnumbered {
            // This runs every 10s per side, and on a quiet server the answer is
            // the same every time -- unthrottled it was 13% of the whole engine
            // log. Say it when the picture actually changes, and otherwise once
            // every 10 minutes so it still reads as a heartbeat.
            use std::sync::atomic::{AtomicI64, AtomicU32, Ordering};
            static LAST_LOG: [AtomicI64; 4] = [
                AtomicI64::new(i64::MIN),
                AtomicI64::new(i64::MIN),
                AtomicI64::new(i64::MIN),
                AtomicI64::new(i64::MIN),
            ];
            static LAST_PIC: [AtomicU32; 4] = [
                AtomicU32::new(u32::MAX),
                AtomicU32::new(u32::MAX),
                AtomicU32::new(u32::MAX),
                AtomicU32::new(u32::MAX),
            ];
            // One slot per (side, class) so the jet and helo heartbeats don't
            // overwrite each other's "did the picture change" state.
            let slot = if defending_side == Side::Red { 0 } else { 1 } + if rotary { 2 } else { 0 };
            let pic = ((player_threat_count.min(255) as u32) << 16)
                | ((my_air.min(255) as u32) << 8)
                | their_air.min(255) as u32;
            let secs = now.timestamp();
            let changed = LAST_PIC[slot].swap(pic, Ordering::Relaxed) != pic;
            let stale = secs.saturating_sub(LAST_LOG[slot].load(Ordering::Relaxed)) >= 600;
            if changed || stale {
                LAST_LOG[slot].store(secs, Ordering::Relaxed);
                info!(
                    "{}: {:?} not scrambling — {} enemy {} player(s) {} (need {}), \
                     balance {}v{} (gap {})",
                    rc.label,
                    defending_side,
                    player_threat_count,
                    if rotary { "helicopter" } else { "fixed-wing" },
                    if rc.trigger_on_known_players { "airborne" } else { "on radar" },
                    min_threat,
                    my_air,
                    their_air,
                    rc.balance_gap
                );
            }
            continue;
        }
        if outnumbered && player_threat_count < min_threat {
            info!(
                "{}: {:?} scrambling to balance ({}v{} {} players)",
                rc.label,
                defending_side,
                my_air,
                their_air,
                if rotary { "helicopter" } else { "fixed-wing" }
            );
        }

        // ── Step 2: enemy positions for cluster geometry ────────────────────────
        // Normally the radar picture (players + AI) so the response is placed
        // nearest the real incursion -- filtered to helicopter contacts on the
        // rotary pass, since stationing a helo on a jet track sends it
        // somewhere it cannot fight. With `*_trigger_on_known_players`, fall
        // back to the actual airborne-player positions when radar shows
        // nothing -- otherwise a radar-blind side gates out here. Helicopters
        // flying NOE are rarely painted at all, which is exactly why
        // `helo_trigger_on_known_players` defaults to true.
        let mut detected = if rotary {
            ctx.ewr.detected_enemy_helo_positions(defending_side, now, &ctx.db)
        } else {
            ctx.ewr.detected_enemy_positions(defending_side, now)
        };
        if detected.is_empty() && (rc.trigger_on_known_players || outnumbered) {
            detected = enemy_player_positions(&ctx.db, attacking, rotary);
        }
        if detected.is_empty() {
            continue;
        }

        // ── Step 3: greedy spatial clustering ─────────────────────────────────────
        // Group the detected contacts: if two contacts are within the trigger
        // radius of each other they belong to the same incursion. One response
        // handles one cluster. This prevents 5 aircraft spread over 3
        // objectives from spawning 3 flights.
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

        // ── Step 4: filter clusters below the minimum threat threshold ────────────
        // Even though we already checked player_threat_count, filter any cluster whose
        // raw position count is below min_threat (edge case: positions from AI only).
        // When scrambling purely to balance, a single-contact cluster is fine.
        let cluster_floor = if outnumbered { 1 } else { min_threat };
        clusters.retain(|(_, count)| *count >= cluster_floor);
        if clusters.is_empty() {
            continue;
        }

        // Sort clusters largest → smallest so the most dangerous incursion gets covered first.
        clusters.sort_unstable_by(|a, b| b.1.cmp(&a.1));

        // ── Step 4b: this side's sortie budget for the last hour ────────────
        // The between-waves cooldown is per field now, so nothing side-wide
        // would otherwise stop a coalition with a dozen airbases from running
        // an endless conveyor of AI fighters. This is that limit: N launches
        // per rolling hour, counted per class.
        let hour_ago = now - chrono::Duration::hours(1);
        ctx.event_scheduler
            .cap_sortie_log
            .retain(|(_, _, when)| *when > hour_ago);
        let mut sorties_left = if rc.max_sorties_per_hour == 0 {
            usize::MAX
        } else {
            let used = ctx
                .event_scheduler
                .cap_sortie_log
                .iter()
                .filter(|(s, r, _)| *s == defending_side && *r == rotary)
                .count();
            (rc.max_sorties_per_hour as usize).saturating_sub(used)
        };
        if sorties_left == 0 {
            info!(
                "{}: {:?} has used its whole sortie budget ({}/hour) — not scrambling",
                rc.label, defending_side, rc.max_sorties_per_hour
            );
            continue;
        }

        // ── Step 5: for each cluster find the nearest friendly launch field ───────
        let slots_remaining = (max_per_side - side_active_count)
            .min(max_concurrent - (active_red.len() + active_blue.len()));

        for (cluster_center, cluster_count) in clusters.into_iter().take(slots_remaining) {
            if sorties_left == 0 {
                break; // budget spent on an earlier cluster this pass
            }
            // Closest friendly field that doesn't already have an active
            // response of this class. Fixed-wing CAP needs a runway; the helo
            // patrol also accepts FARPs and FOBs, which is usually much closer
            // to the rotary fight. Neutral objectives are excluded (owner ==
            // defending_side already ensures non-neutral).
            let best_obj = ctx.db.persisted.objectives.into_iter()
                .filter(|(oid, obj)| {
                    obj.owner() == defending_side
                        && is_launch_field(obj.kind(), rotary)
                        && {
                            let already = match defending_side {
                                Side::Red  => active_red.contains(oid),
                                Side::Blue => active_blue.contains(oid),
                                _ => true,
                            };
                            !already
                        }
                        // Still turning its last wave around. Checked HERE and
                        // not after the pick, so the cooldown costs this field
                        // the job rather than costing the coalition the
                        // response -- the neighbouring base answers instead.
                        && ctx
                            .event_scheduler
                            .cap_field_cooldown
                            .get(&(**oid, rotary))
                            .map_or(true, |ended| now - *ended >= respawn_cooldown)
                })
                // Closest available friendly field to the incursion -- the
                // comment always said "closest", the code was scrambling from
                // the furthest one (deep rear), which is why intercepts never
                // showed up and one flank got all the coverage.
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
                    info!(
                        "{}: {:?} has no owned {} near the incursion that is free \
                         (already flying this class, or still on its post-wave cooldown) — skipping",
                        rc.label,
                        defending_side,
                        if rotary { "airbase/FARP/FOB" } else { "airbase" }
                    );
                    continue;
                }
            };

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
                expires_at: now + chrono::Duration::seconds(rc.duration_secs as i64),
                spawned: false,
                rotary,
                // Cluster size decides which roster templates are eligible, so
                // a two-ship probe and a full package are not met by the same
                // flight.
                threat: cluster_count as u32,
            };
            ctx.event_scheduler.active_events.push(event);
            ctx.event_scheduler.total_events_spawned += 1;
            ctx.event_scheduler.cap_sortie_log.push((defending_side, rotary, now));
            sorties_left -= 1;

            // Track locally so this loop iteration's later clusters don't double-book.
            match defending_side {
                Side::Red  => { active_red.insert(*oid); }
                Side::Blue => { active_blue.insert(*oid); }
                _ => {}
            }

            info!(
                "{}: {:?} scrambled from {} — cluster of {} contacts to the {}",
                rc.label, defending_side, obj_name, cluster_count, direction
            );

            ctx.db.ephemeral.msgs().panel_to_side(
                20,
                false,
                attacking_side,
                if rotary {
                    format_compact!(
                        "THREAT: Enemy attack helicopters inbound from the {} - {} contact(s)!",
                        direction,
                        cluster_count
                    )
                } else {
                    format_compact!(
                        "THREAT: Enemy CAP scrambled to the {} - {} aircraft detected by EWR!",
                        direction,
                        cluster_count
                    )
                },
            );
        }
    }
}


fn side_color(side: dcso3::coalition::Side) -> dcso3::Color {
    crate::mapcolor::side_color(side, 1.)
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

/// Rebuild the navaid table when the objective set has changed since last time,
/// then (re)broadcast the beacons for any objective whose assignment moved and
/// whose host group is currently spawned. Carrier navaids are lit from the
/// carrier spawn path instead, so they're skipped here. Entirely best-effort --
/// a DCS command failure is logged, never propagated.
fn maybe_reallocate_navaids(lua: MizLua, ctx: &mut Context) {
    if !ctx.db.ephemeral.cfg.navaids.enabled {
        return;
    }
    use std::hash::{Hash, Hasher};
    let mut sig: u64 = 0;
    for (oid, obj) in ctx.db.objectives() {
        let mut h = std::collections::hash_map::DefaultHasher::new();
        oid.hash(&mut h);
        format!("{:?}", obj.owner()).hash(&mut h);
        format!("{:?}", std::mem::discriminant(obj.kind())).hash(&mut h);
        sig ^= h.finish();
    }
    let first_run = ctx.navaid_sig.is_none();
    if ctx.navaid_sig != Some(sig) {
        ctx.navaid_sig = Some(sig);
        // A capture may have flipped a carrier deck -- re-light carriers.
        ctx.navaid_carriers_lit.clear();

        let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
        let changed = navaids::reallocate(&mut ctx.db.persisted, &cfg.navaids);
        // On the first tick of a session the spawn hooks have usually already
        // lit the beacons, but a group that spawned before allocation (or a
        // reload) would be dark -- so sweep every live host once.
        let targets: Vec<_> = if first_run {
            ctx.db
                .persisted
                .navaids
                .into_iter()
                .map(|(oid, _)| *oid)
                .collect()
        } else {
            changed
        };
        if !targets.is_empty() {
            info!("navaids: refreshing {} objective(s)", targets.len());
            for oid in targets {
                let Some(navs) = ctx.db.persisted.navaids.get(&oid).cloned() else { continue };
                for nav in &navs {
                    let Some(host) = nav.host_gid else { continue };
                    let Ok(group) = ctx.db.group(&host).map(|g| g.name.clone()) else { continue };
                    match dcso3::group::Group::get_by_name(lua, group.as_str()) {
                        Ok(g) => {
                            if let Err(e) = navaids::activate_on_group(&g, nav) {
                                warn!("navaids: activate {oid:?} on {group} failed: {e:?}");
                            }
                        }
                        Err(_) => { /* culled -- will light on next spawn */ }
                    }
                }
            }
        }
    }

    // Carrier decks are registered as airbases a beat after their group spawns,
    // so light them from here and keep retrying each tick until every ship in
    // the task force resolves (or the group is culled / captured).
    let jobs: Vec<(ObjectiveId, Vec<navaids::Navaid>, dcso3::String)> = ctx
        .db
        .persisted
        .carrier_groups
        .into_iter()
        .copied()
        .filter(|oid| !ctx.navaid_carriers_lit.contains(oid))
        .filter_map(|oid| {
            let navs = ctx.db.persisted.navaids.get(&oid)?.clone();
            if navs.is_empty() {
                return None;
            }
            let obj = ctx.db.persisted.objectives.get(&oid)?;
            let gid = obj.groups().get(&obj.owner)?.into_iter().next().copied()?;
            let gname = ctx.db.group(&gid).ok()?.name.clone();
            Some((oid, navs, gname))
        })
        .collect();
    for (oid, navs, gname) in jobs {
        let Ok(g) = dcso3::group::Group::get_by_name(lua, gname.as_str()) else { continue };
        let Ok(units) = g.get_units() else { continue };
        let live: Vec<_> = units.into_iter().filter_map(|u| u.ok()).collect();
        let mut all_lit = true;
        for nav in &navs {
            // Match this deck-navaid to its ship by unit name.
            let want = nav.deck_unit.as_deref();
            let deck = live.iter().find(|u| match (u.get_name(), want) {
                (Ok(n), Some(w)) => n.as_str() == w,
                _ => false,
            });
            let Some(deck) = deck else { all_lit = false; continue };
            if dcso3::airbase::Airbase::get_by_name(lua, deck.get_name().unwrap_or_default()).is_err() {
                all_lit = false; // deck not registered as an airbase yet -- retry
                continue;
            }
            match navaids::activate_carrier(deck, nav) {
                Ok(()) => info!("navaids: lit carrier deck {:?} for {oid:?}", nav.deck_unit),
                Err(e) => {
                    warn!("navaids: carrier {oid:?} deck {:?} activate failed: {e:?}", nav.deck_unit);
                    all_lit = false;
                }
            }
        }
        if all_lit {
            ctx.navaid_carriers_lit.insert(oid);
        }
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

        // DCS doesn't reliably fire onPlayerDisconnect for abrupt
        // disconnects (client crash, network drop), which would otherwise
        // leave a player stuck "connected" forever in the persisted db.
        // Reconcile against the live player list and force-disconnect
        // anyone we think is connected that DCS no longer reports.
        match Net::singleton(lua).and_then(|net| net.get_player_list()) {
            Ok(live) => {
                let mut live_ids: FxHashSet<PlayerId> = FxHashSet::default();
                for id in live {
                    match id {
                        Ok(id) => {
                            live_ids.insert(id);
                        }
                        Err(e) => warn!("bad player id in live player list {e:?}"),
                    }
                }
                let stale: SmallVec<[PlayerId; 4]> = ctx
                    .connected
                    .info_by_player_id
                    .keys()
                    .copied()
                    .filter(|id| !live_ids.contains(id))
                    .collect();
                for id in stale {
                    if let Some(ifo) = ctx.connected.player_disconnected(id) {
                        warn!(
                            "player {} ({}) missing from live player list, forcing disconnect \
                             (DCS likely missed onPlayerDisconnect)",
                            ifo.name, ifo.ucid
                        );
                        ctx.db.player_disconnected(&ifo.ucid)
                    }
                }
            }
            Err(e) => warn!("failed to get live player list for connection reconciliation {e:?}"),
        }

        // Dispatch pending achievements
        let achievements = std::mem::take(&mut ctx.db.ephemeral.pending_achievements);
        for achievement in achievements {
            crate::api::dispatch_event(lua, "achievement", &achievement);
        }

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

                ctx.do_bg_task(Task::Stat(Stat::Kill(dead)));
            }
        }
        if let Err(e) = ctx.db.maybe_do_repairs(ts) {
            error!("error doing repairs {:?}", e)
        }
        record_perf(&mut perf.do_repairs, start_ts);

        maybe_reallocate_navaids(lua, ctx);

        // Process C-130 physical cargo spawn queue (one shared queue for all
        // players, not per-slot -- each queued crate carries its own frozen
        // spawn anchor from when it was queued)
        if let Err(e) = ctx.db.process_c130_spawn_queue(lua, &ctx.idx) {
            error!("error processing C-130 spawn queue: {:?}", e)
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

        // Player recon passes: advance timers, run scans, reveal contacts.
        if let Err(e) = ctx.db.tick_recon_sessions(lua, &mut ctx.landcache, ts) {
            error!("could not tick recon sessions {e}")
        }

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
                    // Special SAM sites are position-classified: no F10 label,
                    // no rings (see create_objective_markup). Drawing an
                    // "ENEMY CONTACT" / "UNDER ATTACK" label with their name at
                    // their position would hand the enemy the exact thing the
                    // classification withholds, so skip the map draw for them.
                    let is_hidden_sam = ctx
                        .db
                        .objective(&oid)
                        .map(|o| o.kind().is_special_sam_site())
                        .unwrap_or(false);
                    if is_hidden_sam {
                        continue;
                    }
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
        if let Err(e) = ctx.db.check_scenery_buildings(lua, ts) {
            error!("could not check scenery buildings {e}")
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
        match ctx.db.check_carrier_group_capture(lua, &ctx.idx, ts) {
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
        ctx.db.tick_tasks(ts);
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
                check_air_threats(ctx, start_ts, false);
                // Same pass for helicopters, which CAP deliberately ignores --
                // otherwise a night where only helo pilots show up gets no AI
                // response at all, on either side.
                check_air_threats(ctx, start_ts, true);
                // Kill any CAP that air-started instead of taxiing out.
                enforce_cap_ground_start(lua, ctx, start_ts);
                // Remove flights that have finished flying home.
                flush_cap_rtb(lua, ctx, start_ts);
                // AI helo missions: poll in-flight troop-insertion / resource-delivery
                // helos, apply the payoff and despawn the ones that have landed.
                if let Err(e) = ctx.db.tick_helo_missions(lua, start_ts) {
                    error!("error ticking helo missions {e:?}");
                }
                // Dynamic CAP retargeting: redirect active CAP groups toward enemy aircraft.
                // Isolated in its own panic boundary -- a panic in here must not
                // take down the rest of this tick (positions, logistics, stats
                // publishing) along with it; the outer run_timed_events catch_unwind
                // would otherwise unwind straight through all of that too.
                if let Err(e) =
                    catch_unwind(AssertUnwindSafe(|| retarget_cap_groups(lua, ctx, start_ts)))
                {
                    error!("retarget_cap_groups panicked: {}", panic_msg(&e))
                }
                // Check SF HVT capture missions (proximity + timeout)

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

    announce_takeoff_holds(lua, ctx, ts);

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
    {
        // Every F10 map draw goes through this one queue, drained once a
        // second at `max_msgs_per_second` commands. One objective's markup
        // rebuild is a label, three rings, a kind symbol (several line
        // segments) and its supply arrows -- so with 150+ objectives a full
        // remark is thousands of commands, and at the default 3/s the map a
        // player is looking at can be many minutes behind the campaign. That
        // shows up as labels that contradict themselves (a base reading
        // "Health: 100" and ">> CAPTURABLE" at once, because the two came from
        // different renders) and as old marks still drawn under new ones,
        // because their deletes are also still queued. Nothing drops -- it is
        // pure latency -- so report the depth instead of leaving it to be
        // inferred from garbled map text.
        use std::sync::atomic::{AtomicUsize, Ordering};
        static TICKS: AtomicUsize = AtomicUsize::new(0);
        let depth = ctx.db.ephemeral.msgs().len();
        if TICKS.fetch_add(1, Ordering::Relaxed) % 60 == 0 && depth > 0 {
            let secs = depth / max_rate.max(1);
            // The per-priority split and the kind histogram are the actionable
            // part: a backlog that is all `Mark`/`DeleteMark` is something
            // re-pinning itself, one that is all `SetMarkupText` is objective
            // labels churning, and either answer beats guessing at the rate.
            let ([chat, marks, markup], by_kind) = ctx.db.ephemeral.msgs().depth_report();
            if secs > 60 {
                warn!(
                    "[MSGQ] {depth} map/chat commands queued (chat {chat}, marks {marks}, \
                     markup {markup}), draining {max_rate}/s -- ~{secs}s to clear. The F10 \
                     map is that far behind the campaign; raise max_msgs_per_second or draw \
                     less. Queued: {by_kind}"
                );
            } else {
                info!(
                    "[MSGQ] {depth} commands queued (chat {chat}, marks {marks}, markup \
                     {markup}), draining {max_rate}/s (~{secs}s to clear). Queued: {by_kind}"
                );
            }
        }
    }
    // The queue is drained by its own timer at MSGQ_DRAIN_HZ, not here -- see
    // start_msgq_drain. Draining once per second meant the whole per-second
    // budget landed in a single DCS frame, which is why the budget had to stay
    // tiny and the map ran minutes behind. `max_rate` is still read above for
    // the [MSGQ] report.
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
    for (id, slot) in std::mem::take(&mut ctx.weather_requests) {
        if let Err(e) = atis::send_full_weather(lua, slot) {
            error!("full weather report failed for {:?}: {:?}", id, e);
        }
    }
    ctx.load_state.step();
    record_perf(&mut perf.timed_events, ts);
    ctx.log_perf(now);
    Ok(AdminResult::Continue)
}

/// How many times a second the message queue is drained.
///
/// `max_msgs_per_second` is a SUSTAINED rate; this is what keeps it from
/// arriving as one lump. Each pass sends at most `rate / MSGQ_DRAIN_HZ`
/// commands, so the work landing in any single DCS frame stays small however
/// high the configured rate goes -- which is the thing the limit is actually
/// protecting.
const MSGQ_DRAIN_HZ: usize = 5;

/// Drain the message queue on its own fast timer, independently of the
/// once-per-second event tick.
fn start_msgq_drain(lua: MizLua) -> Result<()> {
    let timer = Timer::singleton(lua)?;
    let period = 1f32 / MSGQ_DRAIN_HZ as f32;
    timer.schedule_function(timer.get_time()? + period, mlua::Value::Nil, move |lua, _, now| {
        let ctx = unsafe { Context::get_mut() };
        // Round up so a rate that does not divide evenly is never throttled
        // below its configured value.
        let per_pass = ctx
            .db
            .ephemeral
            .cfg
            .max_msgs_per_second
            .div_ceil(MSGQ_DRAIN_HZ)
            .max(1);
        match (Net::singleton(lua), Trigger::singleton(lua).and_then(|t| t.action())) {
            (Ok(net), Ok(act)) => {
                if let Err(e) = catch_unwind(AssertUnwindSafe(|| {
                    ctx.db.ephemeral.msgs().process(per_pass, &net, &act)
                })) {
                    error!("msgq drain panicked: {}", panic_msg(&e))
                }
            }
            (net, act) => {
                if let Err(e) = net {
                    error!("msgq drain: no net singleton {e:?}")
                }
                if let Err(e) = act {
                    error!("msgq drain: no trigger action {e:?}")
                }
            }
        }
        Ok(Some(now + period))
    })?;
    Ok(())
}

/// The message out of a caught panic payload.
///
/// `catch_unwind` hands back `Box<dyn Any + Send>`, whose `Debug` is the
/// useless `Any { .. }` for every payload there is -- which is what every
/// "panicked Any { .. }" line in the engine log was. The payload of a normal
/// `panic!` is a `&str` or a `String`; pull it out.
fn panic_msg(e: &Box<dyn std::any::Any + Send>) -> &str {
    e.downcast_ref::<&'static str>()
        .copied()
        .or_else(|| e.downcast_ref::<std::string::String>().map(|s| s.as_str()))
        .unwrap_or("<panic payload was not a string>")
}

/// Log every panic with its location and a real backtrace, once, at the point
/// it happens.
///
/// The `catch_unwind`s below keep a panicking tick from taking the server
/// down, and that part works -- but what they can report is next to nothing.
/// The payload alone has no location, and `Backtrace::capture()` at the catch
/// site unwinds from the catch, not from the panic, and is a no-op unless
/// `RUST_BACKTRACE` was already set when the runtime first looked at it (the
/// `set_var` in `bflib()` is too late for that, which is why the live log says
/// "disabled backtrace"). A panic hook runs *at the panic site*, before
/// unwinding, and `force_capture` ignores the environment -- between them a
/// panic finally names the file and line it came from.
fn install_panic_hook() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| {
        std::panic::set_hook(Box::new(|info| {
            // The hook logs, and the logger can itself panic (it formats, it
            // allocates, it publishes over netidx). Recursing into the hook
            // from inside it would replace one legible panic with a stack
            // overflow, so report the inner one to stderr and stop.
            thread_local! {
                static IN_HOOK: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
            }
            if IN_HOOK.with(|f| f.replace(true)) {
                eprintln!("bflib: panic while reporting a panic: {info}");
                return;
            }
            struct Reset;
            impl Drop for Reset {
                fn drop(&mut self) {
                    IN_HOOK.with(|f| f.set(false));
                }
            }
            let _reset = Reset;
            let msg = info
                .payload()
                .downcast_ref::<&'static str>()
                .copied()
                .or_else(|| {
                    info.payload()
                        .downcast_ref::<std::string::String>()
                        .map(|s| s.as_str())
                })
                .unwrap_or("<panic payload was not a string>");
            let loc = match info.location() {
                Some(l) => format!("{}:{}:{}", l.file(), l.line(), l.column()),
                None => std::string::String::from("<unknown location>"),
            };
            error!("PANIC at {loc}: {msg}\n{}", Backtrace::force_capture());
        }));
    });
}

fn start_timed_events(ctx: &mut Context, lua: MizLua, path: PathBuf) -> Result<()> {
    ctx.last_slow_timed_events = Utc::now();
    start_msgq_drain(lua)?;
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
                // The hook has already logged the location and backtrace;
                // this says which tick died, so the two can be paired up.
                Err(e) => error!("run_timed_events panicked: {}", panic_msg(&e)),
            }
            Ok(Some(now + 1.))
        }
    })?;
    Ok(())
}

fn delayed_init_miz(lua: MizLua) -> Result<()> {
    info!("init_miz: welcome to blue flag v3");
    if let Ok(wd) = Lfs::singleton(lua).and_then(|l| l.writedir()) {
        report_build(std::path::Path::new(wd.as_str()));
    }
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
    // Computed once, up front, so both the netidx stats publisher (via
    // Task::CfgLoaded) and the NewRound stat below agree on whether this is
    // a genuinely new round or a resume of saved state -- sending a
    // NewRound signal unconditionally on every restart (crash recovery,
    // bot-triggered restart) made bfdb close and reopen the round even when
    // resuming, not just on real campaign resets.
    let fresh = !path.exists();
    ctx.do_bg_task(Task::CfgLoaded {
        sortie: ctx.sortie.clone(),
        cfg: Arc::clone(&cfg),
        admin_channel: Arc::clone(&ctx.external_admin_commands),
        fresh,
    });
    debug!("path to saved state is {:?}", path);
    info!("initializing db");
    let to_bg = ctx.to_background.as_ref().unwrap().clone();
    if fresh {
        debug!("saved state doesn't exist, starting from default");
        ctx.do_bg_task(Task::Stat(Stat::NewRound { sortie: ctx.sortie.clone() }));
        ctx.db = Db::init(lua, cfg, &ctx.idx, &miz, to_bg)
            .context("initalizing the mission")?;
    } else {
        debug!("saved state exists, loading it");
        ctx.db = Db::load(&miz, &ctx.idx, to_bg, cfg, &path)
            .context("loading the saved state")?;
        // Db::init runs this on a fresh campaign start, but a loaded save
        // never goes through init at all -- run it here too so an objective
        // that's missing its logistics-defense group (e.g. saved before a
        // mission version added a base, or before this check existed) gets
        // fixed on the next restart without requiring a full campaign reset.
        // Idempotent: it only adds a group where none exists at all, so it
        // can't touch objectives with real (even fully dead) combat damage.
        let spctx = SpawnCtx::new(lua).context("building spawn ctx for logi coverage check")?;
        ctx.db
            .ensure_default_logi_coverage(&spctx, &ctx.idx)
            .context("ensure_default_logi_coverage (load path) failed")?;
    }
    ctx.shutdown = ctx
        .db
        .ephemeral
        .cfg
        .shutdown
        .map(|hrs| {
            let now = Utc::now();
            AutoShutdown::scheduled(now + Duration::hours(hrs as i64), now)
        });
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
                    // bfdb's Stat::Objective handler seeds health/logi/supply/fuel
                    // at 100. Follow up with the real persisted values so a
                    // dashboard reload after a server restart doesn't show every
                    // battered base as pristine until its health next changes.
                    ctx.db.ephemeral.stat(Stat::ObjectiveHealth {
                        id: *oid,
                        last_change: obj.last_change(),
                        health: obj.health(),
                        logi: obj.logi(),
                    });
                    ctx.db.ephemeral.stat(Stat::ObjectiveSupply {
                        id: *oid,
                        supply: obj.supply(),
                        fuel: obj.fuel(),
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

fn on_mission_load_end(lua: HooksLua) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    ctx.load_state = LoadState::MissionLoaded { time: Utc::now() };
    match Dcs::singleton(lua).and_then(|dcs| dcs.get_mission_filename()) {
        Ok(path) => ctx.mission_file_path = Some(PathBuf::from(path.as_str())),
        Err(e) => warn!("could not get mission filename {e:?}"),
    }
    if !unitdb::is_loaded() {
        if let Err(e) = unitdb::init(lua) {
            warn!("could not harvest the unit db, using config ranges only: {e:?}");
        }
    }
    // Terrain airdrome data (code, freqs, runway names) for the ATIS; only
    // this (hooks) lua state can require('terrain').
    if let Err(e) = atis::harvest_airdromes(lua) {
        warn!("could not harvest the terrain airdromes for the ATIS: {e:?}");
    }
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
    // The hooks lua state still has _G.db, which the mission scripting state
    // hasn't since 2.7. Harvest the installed unit ranges here; if db isn't
    // populated this early, on_mission_load_end retries.
    if let Err(e) = unitdb::init(lua) {
        warn!("could not harvest the unit db at init: {e:?}");
    }
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

/// Print DCS's own `world.event` id table once at mission start.
///
/// dcso3 translates event ids through a hardcoded table, and DCS renumbers and
/// appends to that enum between versions -- which is silent: a shifted id is
/// handled as whatever we think it is, or dropped. This session's log had 20
/// events arriving as id 54 ("mission winner") carrying an
/// {initiator, place, subPlace} payload, which is the takeoff/landing shape,
/// with no way to tell from the outside whether real events were being
/// discarded. One table at startup makes that answerable from the log instead
/// of from guesswork.
fn log_event_ids(lua: MizLua) {
    let ids = (|| -> Result<CompactString> {
        use std::fmt::Write;
        let world: mlua::Table = lua.inner().globals().raw_get("world")?;
        let events: mlua::Table = world.raw_get("event")?;
        let mut v: Vec<(i64, std::string::String)> = vec![];
        for pair in events.pairs::<std::string::String, i64>() {
            let (name, id) = pair?;
            v.push((id, name));
        }
        v.sort();
        let mut s = CompactString::default();
        for (id, name) in v {
            if !s.is_empty() {
                s.push_str(", ");
            }
            let _ = write!(s, "{id}={name}");
        }
        Ok(s)
    })();
    match ids {
        Ok(ids) => info!("[EVENT_IDS] DCS world.event: {ids}"),
        Err(e) => warn!("[EVENT_IDS] could not read world.event from DCS: {e:?}"),
    }
}

fn init_miz(lua: MizLua) -> Result<()> {
    info!("initializing mission");
    log_event_ids(lua);
    let timer = Timer::singleton(lua)?;
    
    // Register the Lua API
    api::register(lua)?;
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
    // Must come after the env vars and before anything can panic: the hook is
    // the only thing that sees a panic's location.
    install_panic_hook();
    unsafe { Context::get_mut() }.init_async_bg(lua.inner()).map_err(dcso3::lua_err)?;
    dcso3::create_root_module(lua, init_hooks, init_miz)
}
