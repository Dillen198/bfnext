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
mod airlife;
mod api;
mod atis;
mod bg;
mod carp;
mod chatcmd;
mod clock;
mod cockpit;
mod command;
mod db;
mod economy;
mod ewr;
mod frontline;
mod groundwar;
mod hq;
mod intel_marks;
mod jtac;
mod landcache;
mod mapcolor;
mod menu;
mod modern_war;
mod msgq;
mod navaids;
mod rules;
mod shots;
mod situation;
mod spawnctx;
mod unitdb;

extern crate nalgebra as na;
use crate::db::{
    events::{EventEffect, EventScheduler},
    player::SlotAuth,
};
use admin::{AdminCommand, AdminResult, run_admin_commands};
use anyhow::{Context as AnyhowContext, Result, anyhow, bail};
use bfprotocols::{
    cfg::{Cfg, LifeType, UnitTag, Vehicle},
    db::{group::GroupId, objective::ObjectiveId},
    perf::{Perf, PerfInner},
    stats::Stat,
};
use bg::Task;
use chatcmd::{run_action_commands, run_jtac_commands};
use chrono::{Duration, prelude::*};
use compact_str::{CompactString, format_compact};
use crossbeam::queue::SegQueue;
use db::{
    Db,
    group::BirthRes,
    player::{RegErr, TakeoffRes},
};
use dcso3::{
    HooksLua, LuaEnv, LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3,
    coalition::Side,
    coord::Coord,
    dcs::Dcs,
    env::{
        self, Env,
        miz::{Miz, UnitId},
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
    world::MarkPanel,
};
use ewr::Ewr;
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use indexmap::IndexSet;
use jtac::{JtId, Jtacs, aim_and_fire_route, group_facing};
use landcache::LandCache;
use log::{debug, error, info, warn};
use mlua::prelude::*;
use msgq::MsgTyp;
use netidx::publisher::Value;
use shots::ShotDb;
use smallvec::{SmallVec, smallvec};
use spawnctx::SpawnCtx;
use std::{
    backtrace::Backtrace,
    panic::{AssertUnwindSafe, catch_unwind},
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
    /// Only players whose address is known. This was keyed by
    /// `Option<String>`, so every player DCS reported without an address
    /// collided on `None`: the second of them was refused as "already
    /// connected from your ip address".
    id_by_addr: FxHashMap<String, PlayerId>,
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
        if !self.info_by_player_id.contains_key(&id) {
            let net = Net::singleton(lua)?;
            let ifo = net.get_player_info(id)?;
            let ucid =
                ifo.ucid()?.ok_or_else(|| anyhow!("player {:?} has no ucid", ifo))?;
            let name = ifo.name()?.unwrap_or_default();
            let addr = ifo.ip()?;
            info!("player name: '{}', id: {:?}, ucid: {:?}", name, id, ucid);
            // No IP check here: this player is already on the server, and
            // refusing them now can't disconnect them -- it only locked them
            // out of every slot.
            self.player_connected(id, PlayerInfo { name, addr, ucid }, false)?;
        }
        self.info_by_player_id
            .get(&id)
            .ok_or_else(|| anyhow!("player {id:?} vanished while connecting"))
    }

    pub fn player_connected(
        &mut self,
        id: PlayerId,
        ifo: PlayerInfo,
        one_player_per_ip: bool,
    ) -> Result<()> {
        if let Some(id) = self.id_by_ucid.remove(&ifo.ucid) {
            self.player_disconnected(id);
        }
        if self.id_by_name.contains_key(&ifo.name) {
            bail!("your callsign is already taken by another player")
        }
        if let Some(addr) = &ifo.addr {
            if one_player_per_ip && self.id_by_addr.contains_key(addr) {
                bail!("another player is already connected from your ip address")
            }
            // with sharing allowed the first player on an address keeps the
            // entry; it's only consulted by the check above
            self.id_by_addr.entry(addr.clone()).or_insert(id);
        }
        self.id_by_ucid.insert(ifo.ucid, id);
        self.id_by_name.insert(ifo.name.clone(), id);
        self.info_by_player_id.insert(id, ifo);
        Ok(())
    }

    pub fn player_disconnected(&mut self, id: PlayerId) -> Option<PlayerInfo> {
        self.info_by_player_id.remove(&id).inspect(|ifo| {
            self.id_by_name.remove(&ifo.name);
            self.id_by_ucid.remove(&ifo.ucid);
            if let Some(addr) = &ifo.addr {
                // only if it's ours -- with shared addresses allowed it may
                // belong to another player still connected from there
                if self.id_by_addr.get(addr) == Some(&id) {
                    self.id_by_addr.remove(addr);
                }
            }
        })
    }
}

/// The restart warnings, as (minutes remaining, wording). Ordered from the
/// earliest to the latest, which `check_auto_shutdown` relies on.
const SHUTDOWN_WARNINGS: [(i64, &str); 4] =
    [(30, "30 minutes"), (10, "10 minutes"), (5, "5 minutes"), (1, "one minute")];

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

#[derive(Debug, Clone, Copy, Default)]
enum LoadState {
    #[default]
    Init,
    MissionLoaded {
        time: DateTime<Utc>,
    },
    Running,
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

/// A Takeoff event held until the aircraft proves it is flying, see
/// `Context::pending_takeoffs`.
#[derive(Debug, Clone)]
struct PendingTakeoff {
    /// Where it lifted off (the latest Takeoff event's position).
    pos: Vector2,
    /// The airbase the Takeoff event named, for the taxiway check.
    place: Option<String>,
    /// When it was first seen in the air without touching down since.
    air_since: Option<DateTime<Utc>>,
}

#[derive(Debug, Default)]
struct JtacSlotIfo {
    subscribed_objectives: FxHashSet<ObjectiveId>,
    pinned: FxHashSet<JtId>,
}

#[derive(Debug, Default)]
struct Context {
    sortie: String,
    event_handler_id: Option<mlua::RegistryKey>,
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
    /// Cockpit menu clicks waiting for the admin drain to let go of the state
    /// (`admin::run_menu_invokes`).
    pending_menu_invokes: Vec<(admin::Caller, Ucid, Vec<std::string::String>)>,
    action_commands: Vec<(PlayerId, String)>,
    /// `-weather` requests, answered from the mission Lua state: the chat
    /// hook runs in the hooks state, which has no `atmosphere` or mission
    /// weather to read.
    weather_requests: Vec<(PlayerId, SlotId)>,
    /// `-brief` requests, answered from the mission Lua state for the same
    /// reason as `weather_requests`.
    brief_requests: Vec<PlayerId>,
    jtac_commands: Vec<(PlayerId, JtId, String)>,
    to_background: Option<UnboundedSender<bg::Task>>,
    recently_landed: FxHashMap<DcsOid<ClassUnit>, DateTime<Utc>>,
    recently_born: FxHashMap<DcsOid<ClassUnit>, DateTime<Utc>>,
    airborne: FxHashSet<DcsOid<ClassUnit>>,
    /// Aircraft whose landing has been settled (`return_lives`) and that have
    /// not really taken off since. A Takeoff event for one of these is held in
    /// `pending_takeoffs` instead of being acted on.
    landed: FxHashSet<DcsOid<ClassUnit>>,
    /// Takeoff events after a settled landing, waiting for the aircraft to
    /// actually climb away (`confirm_takeoffs`). DCS fires Takeoff when a fast
    /// rollout skips over a bump in the runway, often with no Land after it;
    /// acted on at once, that undid the landing the pilot had just been told
    /// was registered (Discord, Oct 24).
    pending_takeoffs: FxHashMap<DcsOid<ClassUnit>, PendingTakeoff>,
    captureable: FxHashMap<ObjectiveId, usize>,
    shots_out: ShotDb,
    menu_init_queue: IndexSet<SlotId, FxBuildHasher>,
    /// Failed menu-build attempts per slot, see `menu::process_init_queue`.
    menu_init_retries: FxHashMap<SlotId, u8>,
    last_frame: Option<DateTime<Utc>>,
    last_slow_timed_events: DateTime<Utc>,
    /// The slow pass in progress, if it is spread over several ticks: the
    /// next bucket to run, how many there are, and when the pass started.
    slow_pass: Option<(usize, usize, DateTime<Utc>)>,
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
    /// Who commands each side, pushed by bfdb (`set-commanders`). None until
    /// the first push -- see `command`.
    commanders: Option<bfprotocols::command::Commanders>,
    /// Each commander's recent command-map orders, for the rate limit
    /// (`command::order`). `None` is a server admin through bfdb.
    command_orders: FxHashMap<Option<Ucid>, std::collections::VecDeque<DateTime<Utc>>>,
    /// Naval hunter groups sent out from the command map.
    hunters: command::Hunters,
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
    /// On-call wingmen, autonomous AI packages and civil traffic
    /// (`Cfg::air_life`). Session state only.
    airlife: airlife::AirLife,
    /// Electronic warfare, SAM magazines, strike raids, sea drones and
    /// campaign tempo (`Cfg::modern_war`). Session state only.
    modern_war: modern_war::ModernWar,
    /// Ground formations' session state and the player command queue
    /// (`Cfg::ground_war`). The formations themselves are in the save.
    groundwar: groundwar::GroundWar,
    /// Each side's theatre HQ (`smart_commander.hq`): its plan, the
    /// operations it is running and players' support requests. What has to
    /// outlive a restart is in `Persisted::hq`.
    hq: hq::Hq,
    last_junk_removal: DateTime<Utc>,
    last_weather_publish: DateTime<Utc>,
    /// When each held player last got the takeoff-hold countdown panel.
    takeoff_nag_last: FxHashMap<UnitId, DateTime<Utc>>,
    /// Rear-base protection: seconds each pilot has spent inside an enemy
    /// protected zone this sortie, and when they were last warned.
    rear_base_time: FxHashMap<Ucid, (u32, Option<DateTime<Utc>>)>,
    /// Era weapon rules: pilots who took off with a banned weapon, the
    /// deadline to land, their slot and what they carry.
    banned_loadouts: FxHashMap<Ucid, (DateTime<Utc>, SlotId, std::string::String)>,
    /// True once `db` holds a real campaign (initialized or loaded). Until
    /// then it is `Db::default()`, and saving it would overwrite the save
    /// file with an empty campaign.
    db_loaded: bool,
    /// Set once the timed-events loop has started shutting DCS down: counts
    /// the attempts to call `DCS.exitProcess`. Nothing may save after this --
    /// a campaign reset has already removed the save file on purpose.
    exit_attempts: Option<u32>,
    /// After a shutdown: the background thread's "save and logs flushed"
    /// flag, and when we started waiting for it. `try_exit_dcs` holds off
    /// exiting until it is set (or `SHUTDOWN_WAIT` has passed).
    shutdown_wait: Option<(
        Arc<(parking_lot::Mutex<bool>, parking_lot::Condvar)>,
        std::time::Instant,
    )>,
    /// DCS model time and the wall clock at the last 1 s tick, to notice the
    /// simulation stopping while the campaign's (wall-clock) timers ran on.
    last_tick_clocks: Option<(f64, DateTime<Utc>)>,
    last_bg_check: DateTime<Utc>,
}

thread_local! {
    /// The campaign state, created on first use and never freed. Only ever
    /// reached through a `CtxGuard`.
    static CTX_SLOT: std::cell::Cell<*mut Context> = const { std::cell::Cell::new(std::ptr::null_mut()) };
    /// True while a `CtxGuard` is alive on this thread.
    static CTX_HELD: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// The one live `&mut Context`. All campaign state is in a single `Context`
/// and DCS calls back into us synchronously from inside our own API calls
/// (`coalition.addGroup` fires Birth before it returns, `destroy()` fires
/// Dead, ...), so the state used to be a `static mut` handed out as
/// `&'static mut` from every entry point -- and a callback that landed while
/// a tick held it took a second one. Two live mutable references to the same
/// data is undefined behaviour however carefully each side is written.
///
/// Now there is at most one: a guard sets a thread-local flag, and the state
/// is only reachable through it. A second borrow while one is held is a bug:
/// it panics in debug builds and tests, and in release it is refused
/// (`Context::borrow` returns an error, `try_borrow` returns `None`). The
/// callbacks DCS makes from inside our calls never ask: while the state is
/// held, `on_event` queues the event and the holder handles it later
/// (`drain_deferred_events`), with nothing else borrowed. See
/// docs/plans/bflib-context-aliasing.md.
pub(crate) struct CtxGuard {
    // not Send: the flag and the slot are per thread
    _not_send: std::marker::PhantomData<*mut ()>,
}

impl Drop for CtxGuard {
    fn drop(&mut self) {
        CTX_HELD.with(|h| h.set(false));
    }
}

impl std::ops::Deref for CtxGuard {
    type Target = Context;
    fn deref(&self) -> &Context {
        // SAFETY: the slot is set before a guard is handed out and never
        // freed; the guard is the only way to reach it and at most one is
        // alive per thread (`CTX_HELD`), so nothing else aliases it.
        unsafe { &*CTX_SLOT.with(|s| s.get()) }
    }
}

impl std::ops::DerefMut for CtxGuard {
    fn deref_mut(&mut self) -> &mut Context {
        // SAFETY: as for `deref`; `&mut self` makes the borrow unique.
        unsafe { &mut *CTX_SLOT.with(|s| s.get()) }
    }
}

impl Context {
    /// The campaign state, or `None` when something on this thread already
    /// holds it (a callback DCS made from inside one of our own calls).
    pub(crate) fn try_borrow() -> Option<CtxGuard> {
        if CTX_HELD.with(|h| h.replace(true)) {
            return None;
        }
        CTX_SLOT.with(|s| {
            if s.get().is_null() {
                s.set(Box::into_raw(Box::new(Context::default())));
            }
        });
        Some(CtxGuard { _not_send: std::marker::PhantomData })
    }

    /// Campaign time now (`crate::clock`): what campaign timers are measured
    /// on. Not for log or perf stamps, stats records or real-world schedules.
    fn now(&self) -> DateTime<Utc> {
        self.db.ephemeral.now()
    }

    /// `borrow` for a timer callback, which can't return the error: logs it.
    pub(crate) fn borrow_logged(what: &str) -> Option<CtxGuard> {
        Self::borrow(what).map_err(|e| error!("{e:?}")).ok()
    }

    /// The campaign state for an entry point DCS calls directly. Asking while
    /// it is already held is a bug (two `&mut Context`): debug builds and
    /// tests panic, release refuses with an error naming `what`.
    pub(crate) fn borrow(what: &str) -> Result<CtxGuard> {
        match Self::try_borrow() {
            Some(g) => Ok(g),
            None => {
                debug_assert!(false, "{what}: re-entrant borrow of the campaign state");
                bail!(
                    "{what}: called while the engine holds the campaign state (re-entrant call), refused"
                )
            }
        }
    }

    fn do_bg_task(&self, task: bg::Task) {
        if let Some(to_bg) = &self.to_background {
            match to_bg.send(task) {
                Ok(()) => (),
                // log::error! would go to the same dead thread
                Err(e) => bg::fallback_log(
                    format!("bflib: background thread is dead, task dropped: {e}\n")
                        .as_bytes(),
                ),
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
        let mut perf_guard = Perf::get_mut();
        let perf = Arc::make_mut(&mut perf_guard.inner);
        self.db.respawn_after_load(lua, perf, &self.idx, miz, &mut self.landcache, &spctx)
    }

    fn log_perf(&mut self, now: DateTime<Utc>) {
        // Every 10 minutes: at 60s the cumulative timing tables were 59-75%
        // of every engine log line, and they are running totals anyway.
        if now - self.last_perf_log > Duration::seconds(600) {
            self.last_perf_log = now;
            self.do_bg_task(bg::Task::LogPerf {
                players: self.connected.len(),
                perf: Perf::get_mut().clone(),
                api_perf: dcso3::perf::Perf::get_mut().clone(),
            });
            info!("landcache {}", self.landcache.stats())
        }
    }

    /// Once a minute: ping the background thread and complain -- to the
    /// fallback log if need be -- when it is dead or not keeping up. It does
    /// every save, and nothing else would notice it had stopped.
    fn check_bg_health(&mut self, now: DateTime<Utc>) {
        if now - self.last_bg_check < Duration::seconds(60) {
            return;
        }
        self.last_bg_check = now;
        let Some(to_bg) = &self.to_background else { return };
        if to_bg.is_closed() {
            error!(
                "the background thread is DEAD: the campaign is not being saved and stats \
                 are not being published. Restart the mission."
            );
            return;
        }
        if let Some(age) = bg::heartbeat_age(now)
            && age > 180
        {
            warn!(
                "the background thread has not finished a task in {age}s (saves, stats \
                     and logging are stalled behind it)"
            );
        }
        self.do_bg_task(Task::Ping);
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
    let mut ctx_guard = Context::borrow("onPlayerTryConnect")?;
    let ctx = &mut *ctx_guard;
    if let Some(msg) = ctx.load_state.login_ok() {
        return Ok(Some(msg));
    }
    if let Some(filter) = &ctx.db.ephemeral.cfg.name_filter
        && !filter.check(&name)
    {
        let msg = format_compact!("name must match {}", filter.as_str());
        return Ok(Some(msg.into()));
    }
    if let Some((until, _)) = ctx.db.ephemeral.cfg.banned.get(&ucid) {
        match until {
            None => return Ok(Some("you are banned forever".into())),
            // a ban is a real-world date: wall clock
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
        ctx.db.ephemeral.cfg.one_player_per_ip,
    ) {
        return Ok(Some(String::from(format_compact!("{e}"))));
    }
    ctx.db.player_connected(ucid, name.clone());
    ctx.do_bg_task(Task::Stat(Stat::Connect { id: ucid, addr, name }));
    record_perf(&mut Arc::make_mut(&mut Perf::get_mut().inner).dcs_hooks, ts);
    Ok(None)
}

fn on_player_try_send_chat(
    lua: HooksLua,
    id: PlayerId,
    msg: String,
    all: bool,
) -> Result<Option<String>> {
    let start_ts = Utc::now();
    // A message the server sends itself (net.send_chat from the message
    // drain) may come back through here synchronously: let it through.
    let Some(mut ctx_guard) = Context::try_borrow() else {
        debug!("onPlayerTrySendChat inside one of our calls, passed through");
        return Ok(None);
    };
    let ctx = &mut *ctx_guard;
    let mut perf_guard = Perf::get_mut();
    let perf = &mut Arc::make_mut(&mut perf_guard.inner).dcs_hooks;
    // The cockpit overlay's key registration: a credential, so it is neither
    // logged nor shown in chat (Some("") swallows the line for everybody).
    if let Some(key) = msg.strip_prefix(cockpit::KEY_CMD) {
        match cockpit::register_key(ctx, id, key.trim()) {
            Ok(()) => {
                info!("onPlayerTrySendChat id: {id:?} registered a cockpit overlay key")
            }
            Err(e) => warn!("onPlayerTrySendChat id: {id:?} cockpit key rejected: {e}"),
        }
        record_perf(perf, start_ts);
        return Ok(Some("".into()));
    }
    info!("onPlayerTrySendChat id: {:?}, msg: {:?}, all: {:?}", id, msg, all);
    let now = ctx.now();
    let r = chatcmd::process(ctx, lua, now, id, msg);
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
            // Point a broke pilot at what they CAN fly, not just at the wall.
            let lifeline = ctx
                .db
                .ephemeral
                .cfg
                .points
                .as_ref()
                .and_then(|p| p.lifeline.as_ref())
                .filter(|l| !l.airframes.is_empty())
                .map(|l| {
                    let types: Vec<&str> =
                        l.airframes.iter().map(|v| v.as_str()).collect();
                    format_compact!(
                        " -- lifeline: {} fly free while you're short",
                        types.join(", ")
                    )
                })
                .unwrap_or_default();
            // An empty list covers every airframe, so a NoPoints here means
            // the pilot is above the lifeline threshold -- no hint to give.
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!("{vehicle} costs {cost}, you have {balance}{lifeline}"),
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
                "Objective is still consolidating ({}s left) -- hold it with troops, or land a logistics crate to speed it up",
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
        SlotAuth::WrongSide { side, locked } => {
            let why = if locked {
                "sides are locked on this server"
            } else {
                "you have no side switches left"
            };
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!(
                    "You are on {side:?} in this campaign and {why} -- pick a {side:?} slot"
                ),
            );
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
    let now = ctx.now();
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
            if matches!(
                slot,
                SlotId::ArtilleryCommander(..)
                    | SlotId::ForwardObserver(..)
                    | SlotId::Observer(..)
            ) {
                chatcmd::commander_guide(ctx, id);
            }
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
    // A slot change we force ourselves (net.force_player_slot) may ask
    // here from inside that call: our own move needs no permission.
    let Some(mut ctx_guard) = Context::try_borrow() else {
        info!("onPlayerTryChangeSlot inside one of our calls (a forced move), allowed");
        return Ok(None);
    };
    let ctx = &mut *ctx_guard;
    let res = match ctx.connected.get_or_lookup_player_info(lua, id) {
        Err(e) => {
            error!("failed to get player info for {:?} {:?}", id, e);
            Ok(Some(false))
        }
        Ok(ifo) => {
            let ifo = ifo.clone();
            match try_occupy_slot(ctx, lua, id, ifo, side, slot) {
                Err(e) => {
                    error!("error checking slot {:?}", e);
                    Ok(Some(false))
                }
                Ok(false) => Ok(Some(false)),
                Ok(true) => Ok(None),
            }
        }
    };
    record_perf(&mut Arc::make_mut(&mut Perf::get_mut().inner).dcs_hooks, start_ts);
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
        info.name,
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
    let pos = unit.get_position().ok().map(|p| Vector2::new(p.p.x, p.p.z));
    dismount_info_for(ctx, &id, pos)
}

/// The dismount a dying vehicle leaves, from its id and the ground position
/// it died at (x north, y east).
fn dismount_info_for(
    ctx: &Context,
    id: &DcsOid<ClassUnit>,
    pos: Option<Vector2>,
) -> Option<DismountInfo> {
    let uid = ctx.db.ephemeral.get_uid_by_object_id(id)?;
    let su = ctx.db.persisted.units.get(uid)?;
    // Skip aircraft — only ground vehicles dismount
    if su.tags.contains(UnitTag::Helicopter) || su.tags.contains(UnitTag::Aircraft) {
        return None;
    }
    // Skip if no dismount config for this vehicle type
    ctx.db.ephemeral.cfg.dismount.get(&su.typ)?;
    let pos = pos?;
    Some(DismountInfo {
        vehicle_typ: su.typ.clone(),
        side: su.side,
        pos,
        heading: su.heading,
        from_group: su.group,
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

fn unit_killed(
    lua: MizLua,
    ctx: &mut Context,
    id: DcsOid<ClassUnit>,
    now: DateTime<Utc>,
) -> Result<()> {
    ctx.recently_landed.remove(&id);
    // a dead unit never lands, so nothing else would ever take it out
    ctx.airborne.remove(&id);
    ctx.landed.remove(&id);
    ctx.pending_takeoffs.remove(&id);
    ctx.shots_out.dead(id.clone(), now);
    if let Err(e) = ctx.jtac.unit_dead(lua, &mut ctx.db, &id) {
        error!("jtac unit dead failed for {:?} {:?}", id, e)
    }
    menu::jtac::flush_jtac_notices(ctx);
    if let Err(e) = ctx.db.unit_dead(&id, ctx.now()) {
        error!("unit dead failed for {:?} {:?}", id, e);
    }
    Ok(())
}

thread_local! {
    /// Events that arrived while the campaign state was held, in arrival
    /// order. See `CtxGuard` and `drain_deferred_events`.
    static DEFERRED: std::cell::RefCell<Vec<DeferredEvent>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// What a Dead / UnitLost handler needs from the object, taken while it still
/// exists: by the end of the call that destroyed it, it doesn't.
#[derive(Debug, Clone)]
enum DeathObj {
    Unit {
        oid: DcsOid<ClassUnit>,
        name: Option<String>,
        /// Ground position (x north, y east), for dismounts.
        pos: Option<Vector2>,
    },
    Static(DcsOid<dcso3::static_object::ClassStatic>),
    /// No initiator, or one that is neither a unit nor a static.
    Other,
}

impl DeathObj {
    fn capture(initiator: Option<&dcso3::object::Object>) -> Result<Self> {
        let Some(obj) = initiator else { return Ok(Self::Other) };
        if let Ok(unit) = obj.as_unit() {
            return Ok(Self::Unit {
                oid: unit.object_id()?,
                name: unit.get_name().ok(),
                pos: unit.get_position().ok().map(|p| Vector2::new(p.p.x, p.p.z)),
            });
        }
        if let Ok(st) = obj.as_static() {
            return Ok(Self::Static(st.object_id()?));
        }
        Ok(Self::Other)
    }
}

/// A DCS callback that arrived while the campaign state was held, in a form
/// that outlives the callback.
#[derive(Debug)]
enum DeferredEvent {
    /// The unit still exists afterwards; re-resolved from its id.
    Birth(DcsOid<dcso3::object::ClassObject>),
    /// Dead (`lost == false`) or UnitLost, with the object captured: by the
    /// end of the call that destroyed it, it no longer exists.
    Death { lost: bool, obj: DeathObj },
    /// Already nothing but an object id.
    PlayerLeaveUnit(dcso3::event::LeaveUnit),
    /// Any other event: DCS's own event table, kept in the Lua registry and
    /// parsed again when it is handled. For Hit and Kill, whether the target
    /// was dead when the event fired (`on_hit` would otherwise ask an object
    /// that may be gone by then).
    Raw { ev: mlua::RegistryKey, dead: Option<bool> },
    /// `onPlayerDisconnect`, if it ever lands inside one of our calls.
    Disconnect(PlayerId),
}

/// Whether the target of a Hit or Kill is dead, asked while the event is
/// still current. `None` when DCS can't say (the target is already gone).
fn target_dead(e: &dcso3::event::WeaponUse) -> Option<bool> {
    let target = e.target.as_ref()?;
    if let Ok(unit) = target.as_unit() {
        return unit.get_life().ok().map(|life| life < 1.);
    }
    if let Ok(st) = target.as_static() {
        return st.get_life().ok().map(|life| life < 1);
    }
    None
}

impl DeferredEvent {
    /// The deferred form of `ev` (`raw` is the same event as DCS sent it), or
    /// `None` for an event there is nothing to do with.
    fn capture(lua: &Lua, raw: mlua::Value, ev: &Event) -> Result<Option<Self>> {
        Ok(match ev {
            // nothing to handle (see the same cases in handle_event); no
            // initiator on a Birth means the object is already gone
            Event::Birth(dcso3::event::Birth { initiator: None, .. })
            | Event::MarkAdded(MarkPanel { initiator: None, .. })
            | Event::MarkChange(MarkPanel { initiator: None, .. })
            | Event::MarkRemoved(MarkPanel { initiator: None, .. })
            | Event::Invalid
            | Event::SimulationFreeze
            | Event::SimulationUnfreeze
            | Event::HumanAircraftRepairStart
            | Event::HumanAircraftRepairFinish
            | Event::GroupChangeOption
            | Event::MacLmsRestart => None,
            Event::Birth(dcso3::event::Birth { initiator: Some(initiator), .. }) => {
                Some(Self::Birth(initiator.object_id()?))
            }
            Event::Dead(e) => Some(Self::Death {
                lost: false,
                obj: DeathObj::capture(e.initiator.as_ref())?,
            }),
            Event::UnitLost(e) => Some(Self::Death {
                lost: true,
                obj: DeathObj::capture(e.initiator.as_ref())?,
            }),
            Event::PlayerLeaveUnit(e) => Some(Self::PlayerLeaveUnit(e.clone())),
            Event::Hit(e) | Event::Kill(e) => {
                debug!("deferring a Hit/Kill that fired inside one of our calls");
                Some(Self::Raw {
                    dead: target_dead(e),
                    ev: lua.create_registry_value(raw)?,
                })
            }
            ev => {
                // Not expected from inside our own calls (these come from the
                // simulation), but handled the same way if they do.
                debug!("deferring an event that fired inside one of our calls: {ev:?}");
                Some(Self::Raw { dead: None, ev: lua.create_registry_value(raw)? })
            }
        })
    }
}

/// The campaign state for an F10 menu handler (DCS calls them directly, from
/// the simulation). Derefs to the `Context`. While it is held, events the
/// handler's own DCS calls fire (the Birth of every crate or group it spawns)
/// are queued, and they are handled when it is dropped.
///
/// Code that needs a handler's work from somewhere that already has the
/// state calls the handler's `&mut Context` half instead: entering twice is
/// refused (see `CtxGuard`).
pub(crate) struct Entered<'lua> {
    lua: MizLua<'lua>,
    ctx: CtxGuard,
}

impl<'lua> Entered<'lua> {
    pub(crate) fn enter(lua: MizLua<'lua>) -> Result<Self> {
        Ok(Entered { lua, ctx: Context::borrow("F10 menu command")? })
    }
}

impl std::ops::Deref for Entered<'_> {
    type Target = Context;
    fn deref(&self) -> &Context {
        &self.ctx
    }
}

impl std::ops::DerefMut for Entered<'_> {
    fn deref_mut(&mut self) -> &mut Context {
        &mut self.ctx
    }
}

impl Drop for Entered<'_> {
    fn drop(&mut self) {
        // the handler that had the state has returned
        let (lua, ctx) = (self.lua, &mut *self.ctx);
        if let Err(e) = catch_unwind(AssertUnwindSafe(|| drain_deferred_events(lua, ctx)))
        {
            error!(
                "handling deferred events after a menu command panicked: {}",
                bg::panic_msg(&e)
            )
        }
    }
}

/// Run `f` with the campaign state from an entry point DCS calls directly (a
/// timer, a scheduled function). Events our own DCS calls raise inside `f`
/// are queued and handled before the state is let go. Asking while the state
/// is held is a bug, see `Context::borrow`.
pub(crate) fn with_ctx<R>(
    lua: MizLua,
    what: &str,
    f: impl FnOnce(&mut Context) -> R,
) -> Result<R> {
    let mut ctx = Context::borrow(what)?;
    let res = f(&mut ctx);
    drain_deferred_events(lua, &mut ctx);
    Ok(res)
}

/// Run `f` with the campaign state, from a callback DCS makes outside our own
/// ticks -- a mission script calling the `vector_strike` API. Refused when
/// something on this thread already holds the state (a tick, or an API call
/// further up the stack). A mission script may well try that from its own
/// event handler, so this is an error, not a bug. Events our own DCS calls
/// raise inside `f` are deferred and handled before returning, as in a tick.
pub(crate) fn with_context<R>(
    lua: MizLua,
    what: &str,
    f: impl FnOnce(&mut Context) -> Result<R>,
) -> Result<R> {
    let Some(mut ctx) = Context::try_borrow() else {
        bail!(
            "{what}: called while the engine holds the campaign state (re-entrant call), refused"
        )
    };
    let res = f(&mut ctx);
    drain_deferred_events(lua, &mut ctx);
    res
}

/// Handle every event queued while the state was held. Called by the holder,
/// between subsystems, with nothing else borrowed. Handlers can spawn and
/// destroy in turn (queueing more), so keep going until the queue stays
/// empty -- bounded, in case something spawns on every birth.
fn drain_deferred_events(lua: MizLua, ctx: &mut Context) {
    for _ in 0..16 {
        let events = DEFERRED.with(|q| std::mem::take(&mut *q.borrow_mut()));
        if events.is_empty() {
            return;
        }
        let now = ctx.now();
        for ev in events {
            match ev {
                DeferredEvent::Birth(oid) => {
                    match dcso3::object::Object::get_instance(lua, &oid) {
                        Ok(obj) => {
                            if let Err(e) = handle_birth(lua, ctx, &obj) {
                                error!("deferred birth of {oid:?} failed {e:?}")
                            }
                        }
                        // spawned and removed again within the same tick
                        Err(e) => {
                            debug!("deferred birth: {oid:?} no longer exists {e:?}")
                        }
                    }
                }
                DeferredEvent::Death { lost, obj } => {
                    if let Err(e) = handle_death(lua, ctx, lost, obj, now) {
                        error!("deferred death failed {e:?}")
                    }
                }
                DeferredEvent::PlayerLeaveUnit(e) => {
                    if let Err(e) = handle_event(lua, ctx, Event::PlayerLeaveUnit(e), now)
                    {
                        error!("deferred player leave failed {e:?}")
                    }
                }
                DeferredEvent::Raw { ev, dead } => {
                    if let Err(e) = handle_raw_event(lua, ctx, ev, dead, now) {
                        error!("deferred event failed {e:?}")
                    }
                }
                DeferredEvent::Disconnect(id) => player_disconnected(ctx, id),
            }
        }
    }
    warn!(
        "deferred events still arriving after 16 passes, leaving the rest for the next drain"
    );
}

/// A `DeferredEvent::Raw`: parse DCS's event table again and handle it.
fn handle_raw_event(
    lua: MizLua,
    ctx: &mut Context,
    key: mlua::RegistryKey,
    dead: Option<bool>,
    now: DateTime<Utc>,
) -> Result<()> {
    let raw = lua.inner().registry_value::<mlua::Value>(&key);
    let _ = lua.inner().remove_registry_value(key);
    match Event::from_lua(raw?, lua.inner())? {
        Event::Hit(e) => on_hit(lua, ctx, e, false, dead, now),
        Event::Kill(e) => on_hit(lua, ctx, e, true, dead, now),
        Event::MissionEnd => {
            // nothing queued may run against the state that is about to go
            DEFERRED.with(|q| q.borrow_mut().clear());
            mission_end(lua, ctx)
        }
        ev => handle_event(lua, ctx, ev, now),
    }
}

fn handle_birth(
    lua: MizLua,
    ctx: &mut Context,
    initiator: &dcso3::object::Object,
) -> Result<()> {
    if let Ok(unit) = initiator.as_unit() {
        // Civil airliners never enter the campaign db. Without this, an
        // unknown unit being born is taken for a dynamic player slot with
        // nobody in it -- and destroyed on the spot.
        if unit
            .get_name()
            .map(|n| {
                airlife::is_civil(n.as_str()) || modern_war::is_unmanaged(n.as_str())
            })
            .unwrap_or(false)
        {
            return Ok(());
        }
        ctx.recently_born.insert(unit.object_id()?, ctx.now());
        match ctx.db.unit_born(lua, &unit, &ctx.connected) {
            Ok(BirthRes::None) => (),
            Ok(BirthRes::OccupiedSlot(slot)) => {
                ctx.menu_init_queue.insert(slot);
                // EWR text units follow the airframe (metric for the
                // Russian types) unless the player picked their own.
                if let (Some(ucid), Ok(typ)) = (
                    ctx.db.ephemeral.player_in_slot(&slot).copied(),
                    unit.get_type_name(),
                ) {
                    ctx.ewr.slot_airframe(&ucid, typ.as_str());
                }
                if let Err(e) = atis::schedule_atis(lua, ctx, slot) {
                    error!("could not schedule atis: {:?}", e);
                }
                // The auto-generated situational briefing, sequenced
                // after the ATIS so they don't overwrite each other.
                if let Err(e) = situation::schedule_slot_briefing(lua, ctx, slot) {
                    error!("could not schedule situation briefing: {:?}", e);
                }
                // Force EPLRS on for every player group so fixed-wing
                // slots show up on the coalition F10 map / datalink the
                // same way helicopters (which usually have it enabled in
                // the .miz) already do. Best effort -- a failure here
                // just means that slot relies on the .miz setting.
                if let Err(e) = unit.get_controller().and_then(|c| {
                    c.set_command(dcso3::controller::Command::EPLRS {
                        enable: true,
                        group: None,
                    })
                }) {
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
    } else if let Ok(st) = initiator.as_static()
        && let Err(e) = ctx.db.static_born(&st)
    {
        error!("static born failed {:?} {:?}", st, e);
    }
    Ok(())
}

/// Hit and Kill. A Kill is final even when DCS has already removed the target
/// by the time we ask about it: `get_life()` then errors, and the old `?` on
/// it threw the whole event away -- so the kill lost its attribution (a gun or
/// rocket kill, most often, where Kill is the only event carrying the
/// shooter) and the unit was never marked dead here.
fn on_hit(
    lua: MizLua,
    ctx: &mut Context,
    e: dcso3::event::WeaponUse,
    is_kill: bool,
    // whether the target was dead when the event fired, for an event handled
    // after the fact (`DeferredEvent::Raw`); asked of the target otherwise
    dead_then: Option<bool>,
    start_ts: DateTime<Utc>,
) -> Result<()> {
    if let Some(target) = e.target.as_ref().and_then(|t| t.as_unit().ok()) {
        let dead = match dead_then
            .map(Ok)
            .unwrap_or_else(|| target.get_life().map(|life| life < 1.))
        {
            Ok(dead) => dead,
            Err(_) => is_kill,
        };
        // A civil airliner is not in the campaign db: remember which player
        // hit it (they pay if it goes down) and keep it away from the kill
        // bookkeeping, which only knows campaign units.
        if let Ok(name) = target.get_name()
            && airlife::is_civil(name.as_str())
        {
            let shooter = e
                .initiator
                .as_ref()
                .and_then(|u| u.as_unit().ok())
                .and_then(|u| u.object_id().ok())
                .and_then(|oid| ctx.db.player_in_unit(false, &oid));
            airlife::civil_hit(ctx, name.as_str(), shooter, start_ts);
            if dead {
                airlife::civil_down(ctx, name.as_str(), start_ts);
            }
            return Ok(());
        }
        // Sea drones aren't campaign units either.
        if target
            .get_name()
            .map(|n| modern_war::is_unmanaged(n.as_str()))
            .unwrap_or(false)
        {
            return Ok(());
        }
        // An AI helo mission taking hits re-routes (no-op for anything else).
        if let Some(uid) = target
            .object_id()
            .ok()
            .and_then(|oid| ctx.db.ephemeral.get_uid_by_object_id(&oid).copied())
        {
            let shooter = e
                .initiator
                .as_ref()
                .and_then(|u| u.as_unit().ok())
                .and_then(|u| u.object_id().ok())
                .and_then(|oid| ctx.db.ephemeral.get_uid_by_object_id(&oid).copied());
            ctx.db.note_helo_hit(&uid, shooter, start_ts);
        }
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
            // A base under fire from the ground learns where the shooter is.
            ctx.db.garrison_hit(
                &mut ctx.groundwar.garrison,
                lua,
                &target,
                &shooter,
                start_ts,
            );
        }
        if dead {
            let dismount = try_capture_dismount_info(ctx, &target);
            if let Err(e) = unit_killed(lua, ctx, target.object_id()?, start_ts) {
                error!("0 unit killed failed {:?}", e)
            }
            spawn_dismount(lua, ctx, dismount);
        }
    } else if let Some(target) = e.target.as_ref().and_then(|t| t.as_static().ok()) {
        let dead = match dead_then
            .map(Ok)
            .unwrap_or_else(|| target.get_life().map(|life| life < 1))
        {
            Ok(dead) => dead,
            Err(_) => is_kill,
        };
        if dead {
            let id = target.object_id()?;
            if let Err(e) = ctx.db.respawn_protected_static(lua, &ctx.idx, &id) {
                error!("respawn protected static failed {e:?}")
            }
            if let Err(e) = ctx.db.static_dead(&id, start_ts) {
                error!("static dead failed {e:?}")
            }
        }
    }
    Ok(())
}

/// Dead (`lost == false`) and UnitLost, live or deferred (see `DeathObj`).
fn handle_death(
    lua: MizLua,
    ctx: &mut Context,
    lost: bool,
    obj: DeathObj,
    start_ts: DateTime<Utc>,
) -> Result<()> {
    match obj {
        DeathObj::Other => (),
        DeathObj::Unit { oid: id, name, pos } => {
            // UNIT_LOST fires for any unit leaving the world, not only for
            // deaths -- a despawn (ours, via destroy()) or a player's aircraft
            // going away with them. Handled exactly like Dead it ran
            // unit_killed on those too, which credits a kill to whoever last
            // hit the unit and calls "splash" on GCI. Our despawns drop the
            // unit's object-id mapping before destroying it, and a real death
            // normally reaches Dead / Kill first (which drop it too), so a
            // unit that is still mapped here is one that died without either
            // -- the case UnitLost exists to catch. Anything else is a
            // despawn: ignore it.
            if lost
                && ctx.db.ephemeral.get_uid_by_object_id(&id).is_none()
                && ctx.db.ephemeral.get_slot_by_object_id(&id).is_none()
            {
                debug!("UnitLost for a unit we no longer track (despawn), ignored");
                return Ok(());
            }
            let unmanaged = name.as_ref().filter(|n| {
                airlife::is_civil(n.as_str()) || modern_war::is_unmanaged(n.as_str())
            });
            if let Some(name) = unmanaged {
                if airlife::is_civil(name.as_str()) {
                    airlife::civil_down(ctx, name.as_str(), start_ts);
                }
                return Ok(());
            }
            let dismount = dismount_info_for(ctx, &id, pos);
            // Live voice GCI: hostile air killed -> "splash".
            if let Some((vside, vtags, vpos)) = ctx
                .db
                .ephemeral
                .get_uid_by_object_id(&id)
                .and_then(|uid| ctx.db.unit(uid).ok())
                .map(|u| (u.side, u.tags.0, u.pos))
                && vside != Side::Neutral
                && (vtags.contains(UnitTag::Aircraft)
                    || vtags.contains(UnitTag::Helicopter))
            {
                ctx.ewr.record_air_kill(vpos, vside, start_ts);
            }
            if let Err(e) = unit_killed(lua, ctx, id, start_ts) {
                error!("1 unit killed failed {:?}", e)
            }
            spawn_dismount(lua, ctx, dismount);
        }
        DeathObj::Static(id) => {
            if let Err(e) = ctx.db.respawn_protected_static(lua, &ctx.idx, &id) {
                error!("respawn protected static failed {e:?}")
            }
            if let Err(e) = ctx.db.static_dead(&id, start_ts) {
                error!("static killed failed {e:?}")
            }
        }
    }
    Ok(())
}

/// DCS's `onEvent`. While something holds the campaign state -- the event
/// fired inside one of our own calls (a spawn's Birth, a `destroy()`'s Dead)
/// -- it is queued for the holder to handle (`DeferredEvent`); otherwise it
/// is handled here, and whatever the handler's own calls fire after it.
fn on_event_raw(lua: MizLua, raw: mlua::Value) -> Result<()> {
    let ev = match Event::from_lua(raw.clone(), lua.inner()) {
        Ok(ev) => ev,
        Err(e) => {
            warn!("error translating event: {:?}", e);
            return Ok(());
        }
    };
    match Context::try_borrow() {
        // Don't touch Context (or Perf) here at all: whoever made the call
        // that fired this event is holding them.
        None => {
            if let Some(deferred) = DeferredEvent::capture(lua.inner(), raw, &ev)? {
                DEFERRED.with(|q| q.borrow_mut().push(deferred));
            }
            Ok(())
        }
        Some(mut ctx) => on_event(lua, &mut ctx, ev),
    }
}

fn on_event(lua: MizLua, ctx: &mut Context, ev: Event) -> Result<()> {
    let perf_ts = Utc::now();
    if let Event::MissionEnd = ev {
        DEFERRED.with(|q| q.borrow_mut().clear());
        return mission_end(lua, ctx);
    }
    // between ticks the clock would be up to 100 ms stale
    let start_ts = match Timer::singleton(lua).and_then(|t| t.get_time()) {
        Ok(t) => ctx.db.ephemeral.clock.advance(t.0),
        Err(_) => ctx.now(),
    };
    let res = handle_event(lua, ctx, ev, start_ts);
    drain_deferred_events(lua, ctx);
    record_perf(&mut Arc::make_mut(&mut Perf::get_mut().inner).dcs_events, perf_ts);
    res
}

/// Register `on_event_raw` as a DCS event handler. The handler table is kept
/// in the Lua registry until `remove_event_handler`. (dcso3's
/// `World::add_event_handler` hands the callback a parsed `Event` only, and a
/// deferred event needs DCS's own table.)
fn add_event_handler(lua: MizLua) -> Result<mlua::RegistryKey> {
    let l = lua.inner();
    let tbl = l.create_table()?;
    tbl.set(
        "onEvent",
        l.create_function(|lua, (_, ev): (mlua::Value, mlua::Value)| {
            dcso3::wrap_f("event handler", MizLua(lua), |lua| on_event_raw(lua, ev))
        })?,
    )?;
    let world: mlua::Table = l.globals().raw_get("world")?;
    world.get::<mlua::Function>("addEventHandler")?.call::<()>(tbl.clone())?;
    Ok(l.create_registry_value(tbl)?)
}

fn remove_event_handler(lua: MizLua, key: mlua::RegistryKey) -> Result<()> {
    let l = lua.inner();
    let tbl: mlua::Table = l.registry_value(&key)?;
    let world: mlua::Table = l.globals().raw_get("world")?;
    world.get::<mlua::Function>("removeEventHandler")?.call::<()>(tbl)?;
    l.remove_registry_value(key)?;
    Ok(())
}

/// The mission is ending: save, then reset the state for the next one.
fn mission_end(lua: MizLua, ctx: &mut Context) -> Result<()> {
    // The Db is about to be dropped. Everything since the last slow-tick
    // snapshot used to go with it, along with every open sortie. Not after a
    // shutdown (a campaign reset has deleted the save on purpose, and
    // admin_shutdown already saved otherwise), and never before a campaign
    // was actually loaded -- that would write an empty Db over the real save.
    if ctx.db_loaded && ctx.exit_attempts.is_none() {
        info!("mission ending, saving the campaign");
        return_lives(lua, ctx, DateTime::<Utc>::MAX_UTC);
        ctx.db.close_open_sorties();
        let snap = ctx.db.snapshot();
        ctx.do_bg_task(Task::SaveState(ctx.miz_state_path.clone(), snap));
    }
    *ctx = Context::default();
    Perf::reset();
    ctx.init_async_bg(lua.inner())?;
    // no perf record: the perf context was just reset
    Ok(())
}

/// Handle one DCS event, from `on_event` or from the deferred queue.
fn handle_event(
    lua: MizLua,
    ctx: &mut Context,
    ev: Event,
    start_ts: DateTime<Utc>,
) -> Result<()> {
    match &ev {
        Event::MarkAdded(e) | Event::MarkChange(e) | Event::MarkRemoved(e)
            if e.initiator.is_none() => {}
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
        Event::Birth(b) => match b.initiator.as_ref() {
            Some(initiator) => handle_birth(lua, ctx, initiator)?,
            None => debug!("Birth with no initiator (object already gone)"),
        },
        Event::PlayerEnterUnit(e) => {
            // Combined Arms: a commander jumping into one of our vehicles.
            if let Some(unit) = e.initiator.as_ref().and_then(|o| o.as_unit().ok())
                && let Ok(id) = unit.object_id()
            {
                // Who is driving, and from which commander slot -- so what the
                // vehicle kills is theirs.
                let driver = unit
                    .get_player_name()
                    .ok()
                    .flatten()
                    .and_then(|name| ctx.connected.get_by_name(&name))
                    .map(|ifo| ifo.ucid)
                    .and_then(|ucid| {
                        let pid = *ctx.connected.id_by_ucid.get(&ucid)?;
                        match Net::singleton(lua).and_then(|net| net.get_slot(pid)) {
                            Ok((_, slot)) => Some((ucid, slot)),
                            Err(e) => {
                                warn!("no slot for CA driver {ucid}: {e:?}");
                                None
                            }
                        }
                    });
                ctx.db.player_entered_unit(&id, driver);
            }
        }
        Event::PlayerLeaveUnit(e) => {
            if let Some(initiator) = e.initiator {
                // Landing and leaving the slot inside the ten seconds
                // `return_lives` waits lost the landing with the unit: no
                // airframe and stores credit, no "landed" on the dashboard.
                // Still on the ground, it is a landing -- settle it now.
                if ctx.recently_landed.remove(&initiator).is_some()
                    && let Ok(unit) = Unit::get_instance(lua, &initiator)
                    && unit.in_air().ok() == Some(false)
                    && let (Ok(pos), Ok(slot)) = (unit.get_ground_position(), unit.slot())
                {
                    ctx.db.land(slot, pos.0, &unit);
                }
                ctx.airborne.remove(&initiator);
                ctx.landed.remove(&initiator);
                ctx.pending_takeoffs.remove(&initiator);
                // Snapshot what we need before the deslot invalidates it.
                let leave_info =
                    ctx.db.player_in_unit(false, &initiator).and_then(|ucid| {
                        ctx.db.player(&ucid).and_then(|p| {
                            p.current_slot
                                .as_ref()
                                .and_then(|(_, i)| i.as_ref())
                                .filter(|inst| inst.landed_at_objective.is_none())
                                .map(|inst| {
                                    (
                                        p.side,
                                        Vector2::new(
                                            inst.position.p.x,
                                            inst.position.p.z,
                                        ),
                                        inst.in_air,
                                    )
                                })
                        })
                    });
                if let Some((my_side, my_pos, in_air)) = leave_info
                    && ctx.shots_out.left_slot(initiator.clone(), in_air, start_ts)
                {
                    // Anti-abuse: bailing a losing fight. If an enemy aircraft
                    // is close, credit them with the kill -- airborne only; a
                    // pilot on the ground is not escaping a dogfight.
                    let radius = ctx.db.ephemeral.cfg.slot_leave_kill_radius_m;
                    if radius > 0.0
                        && in_air
                        && let Some(enemy_oid) =
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
        Event::Hit(e) => on_hit(lua, ctx, e, false, None, start_ts)?,
        Event::Kill(e) => on_hit(lua, ctx, e, true, None, start_ts)?,
        Event::Shot(e) => {
            if let Err(e) = ctx.shots_out.shot(&ctx.db, start_ts, &e) {
                error!("error processing shot event {:?}", e)
            }
            buddy_shot(ctx, &e, start_ts);
            banned_weapon_fired(lua, ctx, &e);
            if let Some(ucid) = e
                .initiator
                .as_ref()
                .and_then(|u| u.object_id().ok())
                .and_then(|oid| ctx.db.player_in_unit(false, &oid))
            {
                ctx.db.note_shot(&ucid);
            }
            // (Artillery/launcher shots used to be pushed onto
            // `ephemeral.recent_shots` here "to keep nearby objectives awake",
            // but nothing ever read that list -- it just grew by one entry per
            // shell for the whole session.)
            if let Some(Ok(obj_id)) = e.initiator.as_ref().map(|u| u.object_id()) {
                let shooter_info = ctx
                    .db
                    .ephemeral
                    .get_uid_by_object_id(&obj_id)
                    .and_then(|uid| ctx.db.unit(uid).ok())
                    .map(|u| (u.side, u.tags.0, u.pos, u.group));
                if let Some((side, tags, pos, gid)) = shooter_info {
                    // Live voice GCI: an enemy SAM firing -> "SAM launch, defend"
                    // for nearby friendly flights (see admin::query_gci).
                    if side != Side::Neutral
                        && (tags.contains(UnitTag::SAM)
                            || tags.contains(UnitTag::Launcher))
                        && !tags.contains(UnitTag::Aircraft)
                    {
                        ctx.ewr.record_sam_launch(pos, side, start_ts);
                    }
                    // A SAM launch spends one from that site's magazine.
                    if side != Side::Neutral
                        && tags.contains(UnitTag::SAM)
                        && !tags.contains(UnitTag::Aircraft)
                    {
                        modern_war::on_sam_shot(ctx, gid, start_ts);
                        // ... and gives its position away: it moves after.
                        ctx.groundwar.garrison.note_sam_shot(gid, start_ts);
                    }
                }
                // AI helo missions near an air-defence launch re-route.
                if let Some(uid) = ctx.db.ephemeral.get_uid_by_object_id(&obj_id).copied()
                {
                    ctx.db.note_air_defence_fire(&uid, start_ts);
                }
            }
            // IADN HARM defense: if this shot is a configured anti-radiation
            // weapon, start tracking it so nearby SAM sites on the opposite
            // side can be warned to go dark before it arrives.
            if let Some(iadn) = ctx.db.ephemeral.cfg.iadn.as_ref() {
                let arm_name = e.weapon_name.as_ref().map(|n| n.as_str()).unwrap_or("");
                let decoy = iadn.decoy_weapons.contains(arm_name);
                if decoy || iadn.anti_radiation_weapons.contains(arm_name) {
                    // Players' aircraft aren't in the unit db, so their side
                    // comes from DCS: every player-fired HARM used to go
                    // untracked here.
                    let shooter_side = e
                        .initiator
                        .as_ref()
                        .and_then(|u| u.object_id().ok())
                        .and_then(|obj_id| ctx.db.ephemeral.get_uid_by_object_id(&obj_id))
                        .and_then(|uid| ctx.db.unit(uid).ok())
                        .map(|u| u.side)
                        .or_else(|| {
                            e.initiator.as_ref().and_then(|u| u.get_coalition().ok())
                        })
                        .filter(|s| *s != Side::Neutral);
                    if let (Some(shooter_side), Ok(weapon_oid)) =
                        (shooter_side, e.weapon.object_id())
                    {
                        ctx.ewr.track_potential_arm(
                            weapon_oid,
                            shooter_side.opposite(),
                            start_ts,
                            decoy,
                        );
                    }
                }
            }
            // (Counter-battery map cue removed -- it only ever drew an
            // "ARTY / COUNTER-BATTERY" text mark with no gameplay behind it,
            // and repositioning batteries stacked overlapping copies. The
            // `counter_battery` cfg key is now ignored.)
        }
        Event::Crash(e) => {
            // Only civil airliners are handled here (campaign units are done
            // on Dead/UnitLost); a crash is how a shot-up airliner usually ends.
            if let Some(name) = e
                .initiator
                .as_ref()
                .and_then(|u| u.as_unit().ok())
                .and_then(|u| u.get_name().ok())
                .filter(|n| airlife::is_civil(n.as_str()))
            {
                airlife::civil_down(ctx, name.as_str(), start_ts);
            }
        }
        Event::Dead(e) => handle_death(
            lua,
            ctx,
            false,
            DeathObj::capture(e.initiator.as_ref())?,
            start_ts,
        )?,
        Event::UnitLost(e) => handle_death(
            lua,
            ctx,
            true,
            DeathObj::capture(e.initiator.as_ref())?,
            start_ts,
        )?,
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
                && let Err(e) = ctx.db.static_dead(&st.object_id()?, start_ts)
            {
                error!("static killed failed {e:?}")
            }
        }
        Event::Ejection(e) => {
            if let Some(unit) = e.initiator.as_ref().and_then(|o| o.as_unit().ok()) {
                let csar_pilot = try_capture_csar_info(lua, ctx, &unit);
                let id = unit.object_id()?;
                // Live voice GCI: "chute observed" for the ejected pilot's side.
                if let Some((eside, epos)) = ctx
                    .db
                    .ephemeral
                    .get_uid_by_object_id(&id)
                    .and_then(|uid| ctx.db.unit(uid).ok())
                    .map(|u| (u.side, u.pos))
                    && eside != Side::Neutral
                {
                    ctx.ewr.record_ejection(epos, eside, start_ts);
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
            if let Some(unit) = e.initiator.as_ref().and_then(|o| o.as_unit().ok()) {
                let id = unit.object_id()?;
                let place = e.place.as_ref().and_then(|p| p.get_name().ok());
                if ctx.recently_born.contains_key(&id) {
                    // spawned in the air
                } else if let Some(p) = ctx.pending_takeoffs.get_mut(&id) {
                    // Another takeoff before the held one was confirmed: this
                    // lift-off point is the newer, so the likelier real one.
                    p.pos = unit.get_ground_position()?.0;
                    p.place = place;
                } else if ctx.recently_landed.contains_key(&id) {
                    // Touchdown not settled yet. `return_lives` tells a
                    // touch-and-go from a bounce on the rollout by whether the
                    // aircraft is still in the air; either way it is the same
                    // flight, so nothing is charged here.
                    ctx.airborne.insert(id);
                } else if ctx.landed.contains(&id) {
                    // Back off the ground after a settled landing. Hold it
                    // until the aircraft climbs away: a fast rollout over a
                    // bumpy runway fires this too, and acting on it at once
                    // took the registered landing (and with it the airframe
                    // and stores credit and the dashboard's "landed") away.
                    let pos = unit.get_ground_position()?.0;
                    ctx.airborne.insert(id.clone());
                    info!(
                        "[LANDING] {} takeoff event after its landing at ({:.0},{:.0}), holding until it climbs away",
                        unit.get_player_name()
                            .ok()
                            .flatten()
                            .map(|n| n.to_string())
                            .unwrap_or_default(),
                        pos.x,
                        pos.y
                    );
                    ctx.pending_takeoffs
                        .insert(id, PendingTakeoff { pos, place, air_since: None });
                } else if ctx.airborne.insert(id.clone()) {
                    let position = unit.get_ground_position()?.0;
                    process_takeoff(lua, ctx, &unit, place.as_ref(), position, start_ts)?;
                }
            }
        }
        Event::Land(e) => {
            if let Some(unit) = e.initiator.as_ref().and_then(|o| o.as_unit().ok()) {
                let id = unit.object_id()?;
                if ctx.recently_born.contains_key(&id) {
                    // spawned on the ground
                } else if ctx.pending_takeoffs.remove(&id).is_some() {
                    // Down again before the held takeoff was confirmed: it was
                    // the rollout, not a departure. The landing already
                    // settled stands as it is.
                    ctx.airborne.remove(&id);
                    info!(
                        "[LANDING] {} touched down again, held takeoff dropped, landing stands",
                        unit.get_player_name()
                            .ok()
                            .flatten()
                            .map(|n| n.to_string())
                            .unwrap_or_default()
                    );
                } else if ctx.airborne.remove(&id) {
                    ctx.recently_landed.insert(id, ctx.now());
                }
            }
        }
        // Guns don't raise Shot, only this: AAA opening up near an AI helo
        // mission counts as it being under fire.
        Event::ShootingStart(e) => {
            if let Some(uid) = e
                .initiator
                .as_ref()
                .and_then(|u| u.as_unit().ok())
                .and_then(|u| u.object_id().ok())
                .and_then(|oid| ctx.db.ephemeral.get_uid_by_object_id(&oid).copied())
            {
                ctx.db.note_air_defence_fire(&uid, start_ts);
            }
        }
        Event::MarkAdded(MarkPanel { initiator: Some(unit), .. }) => {
            let oid = unit.object_id()?;
            if let Some(slot) = ctx.db.ephemeral.get_slot_by_object_id(&oid) {
                let slot = *slot;
                if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot) {
                    let ucid = *ucid;
                    if ctx.subscribed_action_menus.contains(&slot)
                        && let Err(e) = menu::action::init_action_menu_for_slot(
                            ctx, lua, &slot, &ucid,
                        )
                    {
                        error!("failed to init action menu for {ucid} {slot} {e:?}")
                    }
                }
            }
        }
        // handled in on_event, before this
        Event::MissionEnd => (),
        _ => (),
    }
    Ok(())
}

pub(crate) fn lives(
    db: &mut Db,
    ucid: &Ucid,
    typfilter: Option<LifeType>,
) -> Result<CompactString> {
    db.maybe_reset_lives(ucid, db.ephemeral.now())?;
    let player = db.player(ucid).ok_or_else(|| anyhow!("no such player {:?}", ucid))?;
    let cfg = &db.ephemeral.cfg;
    let lives = &player.lives;
    let mut msg = CompactString::new("");
    let now = db.ephemeral.now();
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

/// Act on a takeoff: charge the flight, take a life, and enforce the ground
/// hold, lives and points. `position` is where the wheels left the ground.
fn process_takeoff(
    lua: MizLua,
    ctx: &mut Context,
    unit: &Unit,
    place: Option<&String>,
    position: Vector2,
    now: DateTime<Utc>,
) -> Result<()> {
    let slot = unit.slot()?;
    warn_taxiway_takeoff(lua, ctx, unit, place, position, &slot);
    check_banned_loadout(ctx, unit, &slot, now);
    match ctx.db.takeoff(ctx.now(), slot, unit, position) {
        Err(e) => error!("could not process takeoff, {:?}", e),
        Ok(TakeoffRes::NoLifeTaken | TakeoffRes::NotPlayerSlot) => (),
        Ok(TakeoffRes::TookLife(typ)) => {
            if let Err(e) = message_life(ctx, &slot, Some(typ), "life taken\n") {
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
            if let Err(e) = unit.clone().destroy() {
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
            if let Err(e) = unit.clone().destroy() {
                error!("failed to destroy unit that took off during the hold {e:?}")
            }
            if let Some(ucid) = ucid {
                ctx.db.ephemeral.force_player_to_spectators(&ucid);
            }
        }
    }
    Ok(())
}

/// Act on the takeoffs held after a landing (`Context::pending_takeoffs`) once
/// the aircraft is plainly flying: high enough above the ground, or in the
/// air long enough. Until then the landing it made stands, and leaving the
/// slot settles it as a landing. One that never climbs away is never charged.
fn confirm_takeoffs(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    if ctx.pending_takeoffs.is_empty() {
        return;
    }
    let land = dcso3::land::Land::singleton(lua).ok();
    let mut confirmed: SmallVec<[(DcsOid<ClassUnit>, PendingTakeoff); 2]> = smallvec![];
    ctx.pending_takeoffs.retain(|id, p| {
        let Ok(unit) = Unit::get_instance(lua, id) else { return false };
        if !unit.in_air().unwrap_or(false) {
            p.air_since = None;
            return true;
        }
        let since = *p.air_since.get_or_insert(now);
        let agl = unit
            .get_point()
            .ok()
            .and_then(|pt| {
                let ground = land
                    .as_ref()?
                    .get_height(LuaVec2(Vector2::new(pt.0.x, pt.0.z)))
                    .ok()?;
                Some(pt.0.y - ground)
            })
            .unwrap_or(0.);
        if rules::takeoff_confirmed(agl, now - since) {
            confirmed.push((id.clone(), p.clone()));
            false
        } else {
            true
        }
    });
    for (id, p) in confirmed {
        ctx.landed.remove(&id);
        let Ok(unit) = Unit::get_instance(lua, &id) else { continue };
        info!(
            "[LANDING] {} climbed away, takeoff confirmed",
            unit.get_player_name()
                .ok()
                .flatten()
                .map(|n| n.to_string())
                .unwrap_or_default()
        );
        if let Err(e) = process_takeoff(lua, ctx, &unit, p.place.as_ref(), p.pos, now) {
            error!("could not process confirmed takeoff {e:?}")
        }
    }
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
    if let Ok(land) = dcso3::land::Land::singleton(lua)
        && land.get_surface_type(LuaVec2(pos)).ok()
            == Some(dcso3::land::SurfaceType::Runway)
    {
        return Some(false);
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
        // Vector2 of (x = north, y = east). See `rules::runway_fit` for the
        // margins and the course sign problem.
        let dn = pos.x - center.0.x;
        let de = pos.y - center.0.z;
        match rules::runway_fit(dn, de, course, length, width) {
            rules::RunwayFit::On => return Some(false),
            rules::RunwayFit::Off { along, across } => info!(
                "[TAXIWAY_CHK] {airbase_name}: liftoff off runway (name={:?} len={length:.0}                  width={width:.0} course={course:.3}rad) along={along:.0} across={across:.0}",
                rwy.name().ok(),
            ),
        }
    }
    saw_runway.then_some(true)
}

/// Warn a fixed-wing player who got airborne from a taxiway or apron instead of
/// the runway. Helicopters and the AV-8B are exempt -- a pad departure is normal
/// for them.
fn warn_taxiway_takeoff(
    lua: MizLua,
    ctx: &mut Context,
    unit: &Unit,
    place: Option<&String>,
    pos: Vector2,
    slot: &SlotId,
) {
    let Some(airbase_name) = place else { return };
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
            boot.push((*ucid, *slot, uid, secs));
        } else {
            nags.push((uid, secs));
        }
    }
    // Every five seconds, not every second: a one-second panel per held
    // player per tick was a steady stream of priority-0 commands competing
    // with every chat line and alert for the same send budget.
    const NAG_EVERY: i64 = 5;
    ctx.takeoff_nag_last.retain(|uid, _| nags.iter().any(|(u, _)| u == uid));
    for (uid, secs) in nags {
        let due = ctx
            .takeoff_nag_last
            .get(&uid)
            .map(|last| now - *last >= Duration::seconds(NAG_EVERY))
            .unwrap_or(true);
        if due {
            ctx.takeoff_nag_last.insert(uid, now);
            ctx.db.ephemeral.msgs().panel_to_unit(
                NAG_EVERY.min(secs),
                false,
                uid,
                format_compact!("Takeoff hold: {secs}s remaining"),
            );
        }
    }
    for (ucid, uid) in cleared {
        ctx.db.ephemeral.msgs().panel_to_unit(
            30,
            false,
            uid,
            format_compact!("Takeoff hold cleared -- happy hunting!"),
        );
        if let Some(p) = ctx.db.persisted.players.get_mut_cow(&ucid)
            && let Some((_, Some(inst))) = p.current_slot.as_mut()
        {
            inst.takeoff_ok_at = None;
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
        if let Some(p) = ctx.db.persisted.players.get_mut_cow(&ucid)
            && let Some((_, Some(inst))) = p.current_slot.as_mut()
        {
            inst.takeoff_ok_at = None;
        }
        if let Some(oid) = ctx.db.ephemeral.get_object_id_by_slot(&slot).cloned()
            && let Ok(u) = Unit::get_instance(lua, &oid)
        {
            let _ = u.destroy();
        }
        ctx.db.ephemeral.force_player_to_spectators(&ucid);
    }
}

/// Era weapon rules (`EraCfg::banned_weapons`): a pilot who took off with a
/// banned store is told what it is and how long they have to land.
fn check_banned_loadout(
    ctx: &mut Context,
    unit: &Unit,
    slot: &SlotId,
    now: DateTime<Utc>,
) {
    let Some(era) = ctx.db.ephemeral.cfg.era.clone() else { return };
    let Some(ucid) = ctx.db.ephemeral.player_in_slot(slot).copied() else { return };
    let Ok(ammo) = unit.get_ammo() else { return };
    let found = rules::banned_weapon_labels(
        ammo.into_iter().filter_map(|a| a.ok()).map(|a| {
            (
                a.type_name().map(|s| s.to_string()).unwrap_or_default(),
                a.display_name().map(|s| s.to_string()).unwrap_or_default(),
            )
        }),
        |name| era.banned(name).is_some(),
    );
    if found.is_empty() {
        return;
    }
    let list = found.join(", ");
    let secs = rules::banned_land_secs(era.banned_weapon_land_secs);
    info!("era rules: {ucid} took off with banned {list}");
    if let Some(uid) = slot.as_unit_id() {
        ctx.db.ephemeral.msgs().panel_to_unit(
            30,
            true,
            uid,
            format_compact!(
                "BANNED IN THE {} ERA: {list}. Land and remove it within {}:{:02}, and do not \
                 fire it -- or you will be removed.",
                era.current.to_uppercase(),
                secs / 60,
                secs % 60
            ),
        );
    }
    ctx.banned_loadouts.insert(ucid, (now + Duration::seconds(secs as i64), *slot, list));
}

/// Firing a banned weapon: the aircraft is removed on the spot.
fn banned_weapon_fired(lua: MizLua, ctx: &mut Context, e: &dcso3::event::Shot) {
    let Some(era) = ctx.db.ephemeral.cfg.era.as_ref() else { return };
    let Some(name) = e.weapon_name.as_ref() else { return };
    if era.banned(name.as_str()).is_none() {
        return;
    }
    let Some(unit) = e.initiator.as_ref() else { return };
    let Some(ucid) =
        unit.object_id().ok().and_then(|oid| ctx.db.player_in_unit(false, &oid))
    else {
        return;
    };
    let Ok(slot) = unit.slot() else { return };
    warn!("[ERA_RULES] {ucid} fired banned weapon {name} -- removed");
    remove_for_era_rules(
        lua,
        ctx,
        &ucid,
        &slot,
        &format_compact!("You fired {name}, which is banned in this era"),
    );
}

/// Pilots past their deadline to land with a banned weapon are removed;
/// those who landed are forgiven (the next takeoff checks again).
fn enforce_banned_loadouts(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    if ctx.banned_loadouts.is_empty() {
        return;
    }
    let entries: Vec<(Ucid, DateTime<Utc>, SlotId, std::string::String)> = ctx
        .banned_loadouts
        .iter()
        .map(|(u, (t, s, l))| (*u, *t, *s, l.clone()))
        .collect();
    for (ucid, deadline, slot, list) in entries {
        let in_air = ctx
            .db
            .persisted
            .players
            .get(&ucid)
            .and_then(|p| p.current_slot.as_ref())
            .filter(|(s, _)| *s == slot)
            .and_then(|(_, i)| i.as_ref())
            .map(|i| i.in_air);
        match rules::banned_loadout_verdict(in_air, now, deadline) {
            // Left the slot, or landed: nothing to enforce.
            rules::BannedVerdict::Forgive => {
                ctx.banned_loadouts.remove(&ucid);
            }
            rules::BannedVerdict::Remove => {
                warn!(
                    "[ERA_RULES] {ucid} still airborne with banned {list} after the deadline -- removed"
                );
                remove_for_era_rules(
                    lua,
                    ctx,
                    &ucid,
                    &slot,
                    &format_compact!(
                        "You did not land to remove {list}, which is banned in this era"
                    ),
                );
            }
            rules::BannedVerdict::Wait => (),
        }
    }
}

fn remove_for_era_rules(
    lua: MizLua,
    ctx: &mut Context,
    ucid: &Ucid,
    slot: &SlotId,
    why: &str,
) {
    ctx.banned_loadouts.remove(ucid);
    if let Some(uid) = slot.as_unit_id() {
        ctx.db.ephemeral.msgs().panel_to_unit(
            15,
            true,
            uid,
            format_compact!("{why} -- returning to spectators."),
        );
    }
    if let Some(oid) = ctx.db.ephemeral.get_object_id_by_slot(slot).cloned()
        && let Ok(u) = Unit::get_instance(lua, &oid)
    {
        let _ = u.destroy();
    }
    ctx.db.ephemeral.force_player_to_spectators(ucid);
}

/// Rear-base protection (`Cfg::rear_base_protection`): an enemy aircraft
/// near a side's protected airfield is warned every 10 s, and once it has
/// spent `grace_secs` inside in total this sortie it is destroyed and sent
/// to spectators. Spawn camping a rear field was the PvP griefing nothing
/// else stopped. Called from the 1 Hz loop, so each call is one second.
fn enforce_rear_bases(lua: MizLua, ctx: &mut Context, now: DateTime<Utc>) {
    let Some(cfg) =
        ctx.db.ephemeral.cfg.rear_base_protection.clone().filter(|c| c.enabled)
    else {
        return;
    };
    let bases: Vec<(Side, Vector2, String)> = cfg
        .bases
        .iter()
        .flat_map(|(side, names)| names.iter().map(move |n| (*side, n)))
        .filter_map(|(side, n)| {
            let oid = ctx.db.persisted.objectives_by_name.get(n)?;
            let o = ctx.db.persisted.objectives.get(oid)?;
            (o.owner == side).then(|| (side, o.pos(), o.name.clone()))
        })
        .collect();
    if bases.is_empty() {
        ctx.rear_base_time.clear();
        return;
    }
    let land =
        (cfg.min_agl_m > 0.).then(|| dcso3::land::Land::singleton(lua).ok()).flatten();
    // (ucid, name, side, slot, uid, base)
    let mut inside: Vec<(Ucid, String, Side, SlotId, UnitId, String)> = vec![];
    let mut flying: FxHashSet<Ucid> = FxHashSet::default();
    for (ucid, player, inst) in ctx.db.instanced_players() {
        flying.insert(*ucid);
        if !inst.in_air {
            continue;
        }
        let Some((slot, _)) = player.current_slot.as_ref() else { continue };
        let Some(uid) = slot.as_unit_id() else { continue };
        let pos = Vector2::new(inst.position.p.x, inst.position.p.z);
        if let Some(land) = land.as_ref() {
            let ground = land.get_height(LuaVec2(pos)).unwrap_or(0.);
            if inst.position.p.y - ground <= cfg.min_agl_m {
                continue;
            }
        }
        let hit = bases.iter().find(|(owner, bpos, _)| {
            *owner != player.side
                && na::distance(&pos.into(), &(*bpos).into()) <= cfg.radius_m
        });
        if let Some((_, _, bname)) = hit {
            inside.push((
                *ucid,
                player.name.clone(),
                player.side,
                *slot,
                uid,
                bname.clone(),
            ));
        }
    }
    // A new sortie starts with a clean count.
    ctx.rear_base_time.retain(|u, _| flying.contains(u));
    const WARN_EVERY: i64 = 10;
    for (ucid, name, side, slot, uid, base) in inside {
        let e = ctx.rear_base_time.entry(ucid).or_insert((0, None));
        e.0 += 1;
        let spent = e.0;
        if spent >= cfg.grace_secs {
            warn!(
                "[REAR_BASE] {name} ({ucid}, {side:?}) removed after {spent}s inside {base}'s protected zone"
            );
            ctx.rear_base_time.remove(&ucid);
            ctx.db.ephemeral.msgs().panel_to_unit(
                15,
                true,
                uid,
                format_compact!(
                    "You stayed in {base}'s protected airspace -- returning to spectators. Staff have been notified."
                ),
            );
            if let Some(oid) = ctx.db.ephemeral.get_object_id_by_slot(&slot).cloned()
                && let Ok(u) = Unit::get_instance(lua, &oid)
            {
                let _ = u.destroy();
            }
            ctx.db.ephemeral.force_player_to_spectators(&ucid);
            continue;
        }
        let due = e.1.is_none_or(|t| now - t >= Duration::seconds(WARN_EVERY));
        if due {
            e.1 = Some(now);
            let left = cfg.grace_secs - spent;
            ctx.db.ephemeral.msgs().panel_to_unit(
                WARN_EVERY,
                true,
                uid,
                format_compact!(
                    "PROTECTED AIRSPACE: {base} is a rear base. Leave now -- {left}s before you are removed."
                ),
            );
        }
    }
}

fn message_life(
    ctx: &mut Context,
    slot: &SlotId,
    typ: Option<LifeType>,
    msg: &str,
) -> Result<()> {
    let uid = slot.as_unit_id().ok_or_else(|| anyhow!("not a unit"))?;
    let ucid = *ctx
        .db
        .ephemeral
        .player_in_slot(slot)
        .ok_or_else(|| anyhow!("no player in slot {:?}", slot))?;
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
    let airborne = &mut ctx.airborne;
    let landed = &mut ctx.landed;
    let mut reports: SmallVec<[(SlotId, crate::db::player::LandReport); 4]> = smallvec![];
    ctx.recently_landed.retain(|id, landed_ts| {
        if ts - *landed_ts < Duration::seconds(10) {
            return true;
        }
        let unit = or_false!(Unit::get_instance(lua, id));
        // Still in the air ten seconds on: a touch-and-go, the flight goes
        // on. On the ground, any Takeoff in between was a bounce on the
        // rollout -- it used to cancel the landing outright.
        if or_false!(unit.in_air()) {
            airborne.insert(id.clone());
            return false;
        }
        airborne.remove(id);
        let pos = or_false!(unit.get_ground_position());
        let slot = or_false!(unit.slot());
        if let Some(report) = db.land(slot, pos.0, &unit) {
            landed.insert(id.clone());
            reports.push((slot, report));
        }
        // The landing is processed either way, so always drop the entry.
        // Keeping it whenever no life came back -- landed outside an owned
        // objective, or already at full lives -- re-ran `db.land` on every slow
        // tick and left the unit permanently "recently landed", which silently
        // swallowed its next takeoff.
        false
    });
    for (slot, report) in reports {
        let Some(uid) = slot.as_unit_id() else { continue };
        let msg = land_debrief(ctx, &report, ts);
        ctx.db.ephemeral.msgs().panel_to_unit(20, false, uid, msg);
    }
}

/// The debrief a pilot gets once a landing is processed: whether it counted,
/// what the flight did, and what the landing settled. Before it, only a
/// returned life said anything at all -- on a server without lives a landing
/// was silent, and pilots logged off not knowing whether it had registered
/// (Discord, Sept 25).
fn land_debrief(
    ctx: &mut Context,
    r: &crate::db::player::LandReport,
    now: DateTime<Utc>,
) -> CompactString {
    use std::fmt::Write;
    let name = |db: &Db, oid: ObjectiveId| {
        db.objective(&oid).ok().map(|o| format_compact!("{}", o.name()))
    };
    let mut m = CompactString::new("");
    match r.at.and_then(|oid| name(&ctx.db, oid)) {
        Some(at) => {
            let _ = writeln!(
                m,
                "LANDED at {at} -- landing registered, safe to leave the slot"
            );
        }
        None => {
            let _ = writeln!(
                m,
                "LANDED outside a friendly objective -- no landing credit here"
            );
        }
    }
    if let Some(took_off) = r.tally.took_off {
        let mins = (now - took_off).num_minutes().max(0);
        match r.tally.from.and_then(|oid| name(&ctx.db, oid)) {
            Some(from) => {
                let _ = writeln!(m, "Sortie: {mins} min from {from}");
            }
            None => {
                let _ = writeln!(m, "Sortie: {mins} min");
            }
        }
    }
    if r.tally.fired > 0 {
        let _ = writeln!(m, "Weapons fired: {}", r.tally.fired);
    }
    if let Some(line) = r.tally.contributions_line() {
        let _ = writeln!(m, "Contributed: {line}");
    }
    let kills = r.tally.kill_count();
    if kills == 0 {
        let _ = writeln!(m, "Kills: none");
    } else {
        let list: Vec<std::string::String> =
            r.tally.kills.iter().map(|(typ, n)| format!("{n}x {typ}")).collect();
        let _ = writeln!(
            m,
            "Kills: {kills} (+{} pts): {}",
            r.tally.kill_points,
            list.join(", ")
        );
    }
    if r.at.is_some() {
        if r.charged > 0 {
            let _ = writeln!(
                m,
                "Airframe/stores: {} charged at takeoff, {} refunded for what's still aboard",
                r.charged, r.refunded
            );
        }
        if r.banked > 0 {
            let _ = writeln!(m, "Kill points banked: +{}", r.banked);
        }
    } else if r.pending > 0 {
        let _ = writeln!(
            m,
            "Kill points pending: {} -- land at a friendly objective to bank them",
            r.pending
        );
    }
    let _ = writeln!(m, "Balance: {} pts", r.points);
    if ctx.db.ephemeral.cfg.limited_lives
        && let Some(lt) = r.life_type
    {
        let left = lives(&mut ctx.db, &r.ucid, Some(lt)).unwrap_or_default();
        let left = left.trim_end();
        if r.life_returned {
            let _ = writeln!(m, "Lives: {left} (life returned)");
        } else if r.at.is_some() {
            let _ = writeln!(m, "Lives: {left}");
        } else {
            let _ = writeln!(
                m,
                "Lives: {left} (no life back away from a friendly objective)"
            );
        }
    }
    if r.at.is_none() {
        let _ = write!(
            m,
            "Leaving the slot here only counts as a loss if you were hit in the last 10 minutes."
        );
    }
    m
}

fn advise_captureable(ctx: &mut Context) -> Result<()> {
    let cur_cap = ctx.db.capturable_objectives();
    for oid in &cur_cap {
        let dur = ctx.captureable.entry(*oid).or_default();
        *dur += 1;
        if *dur == 10 {
            let obj = ctx.db.objective(oid)?;
            let m = match obj.owner {
                Side::Neutral => {
                    format_compact!("{} (neutral) is now capturable", obj.name())
                }
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
        let (name, pos, owner_only) = {
            let obj = ctx.db.objective(&oid)?;
            (
                obj.name().to_owned(),
                obj.pos(),
                crate::db::markup::owner_only_kind(obj.kind()),
            )
        };
        let mark_text = format_compact!("{} captured by {:?}", name, side);
        let (map_layer, msgs) = ctx.db.ephemeral.map_layer_and_msgs();
        map_layer.on_objective_captured(pos, side, owner_only, &mark_text, ts, msgs);
        crate::api::dispatch_event("alert", &mark_text);
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
            &ctx.db,
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
            writeln!(report, "{}", ewr::HEADER)?;
            for gibbraa in braa_to_chickens {
                writeln!(report, "{gibbraa}")?;
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

/// A friendly aircraft one of our players has locked or shot at, as seen
/// from the DCS side: (unit name, position, type, pilot name if a player).
fn friendly_air_target(
    target: &dcso3::unit::Unit,
    side: Side,
) -> Option<(CompactString, Vector3, Option<CompactString>, Option<CompactString>)> {
    use dcso3::unit::UnitCategory;
    match target.get_category().ok()? {
        UnitCategory::Airplane | UnitCategory::Helicopter => (),
        _ => return None,
    }
    if target.get_coalition().ok()? != side {
        return None;
    }
    let key = CompactString::from(target.get_name().ok()?.as_str());
    let pos = target.get_point().ok()?.0;
    let typ = target.get_type_name().ok().map(|t| CompactString::from(t.as_str()));
    let pilot =
        target.get_player_name().ok().flatten().map(|n| CompactString::from(n.as_str()));
    Some((key, pos, typ, pilot))
}

/// Text "buddy spike" warning to a player who has a friendly locked or
/// fired on, when `note_buddy_lock` says it is due. The GCI voice picks the
/// same lock up from `admin::query_gci`.
fn warn_buddy_lock(
    ctx: &mut Context,
    ucid: &Ucid,
    target: (CompactString, Vector3, Option<CompactString>, Option<CompactString>),
    shot: bool,
    now: DateTime<Utc>,
) {
    let (key, pos, typ, pilot) = target;
    if !ctx.ewr.note_buddy_lock(ucid, key, pos, typ, pilot, shot, now) {
        return;
    }
    let Some(player) = ctx.db.player(ucid) else { return };
    let Some((slot, Some(inst))) = player.current_slot.as_ref() else { return };
    let Some(uid) = slot.as_unit_id() else { return };
    let own = Vector2::new(inst.position.p.x, inst.position.p.z);
    if let Some(msg) = ctx.ewr.buddy_warning(ucid, own, now) {
        ctx.db.ephemeral.msgs().panel_to_unit(10, false, uid, msg)
    }
}

/// Buddy spike from the radar: poll every airborne player's radar
/// (`Unit.getRadar`) for a lock on a friendly aircraft. Every 2s at most.
fn check_buddy_spikes(ctx: &mut Context, lua: MizLua, now: DateTime<Utc>) {
    if !ctx.db.ephemeral.cfg.buddy_spike_radar || !ctx.ewr.buddy_poll_due(now) {
        return;
    }
    let mut locks: SmallVec<
        [(Ucid, (CompactString, Vector3, Option<CompactString>, Option<CompactString>));
            4],
    > = smallvec![];
    for (ucid, player, inst) in ctx.db.instanced_players() {
        if !inst.in_air {
            continue;
        }
        let Some((slot, _)) = player.current_slot.as_ref() else { continue };
        let Ok(unit) = ctx.db.ephemeral.slot_instance_unit(lua, slot) else { continue };
        let target = match unit.get_radar() {
            Ok((_, Some(target))) => target,
            Ok((_, None)) => continue,
            Err(e) => {
                debug!("buddy spike: getRadar failed for {ucid}: {e:?}");
                continue;
            }
        };
        let Ok(target) = target.as_unit() else { continue };
        if let Some(t) = friendly_air_target(&target, player.side) {
            locks.push((*ucid, t));
        }
    }
    for (ucid, t) in locks {
        warn_buddy_lock(ctx, &ucid, t, false, now);
    }
}

/// Buddy spike from a launch: a player fired an air-to-air missile guided on
/// a friendly aircraft.
fn buddy_shot(ctx: &mut Context, e: &dcso3::event::Shot, now: DateTime<Utc>) {
    use dcso3::weapon::MissileCategory;
    let Some(oid) = e.initiator.as_ref().and_then(|u| u.object_id().ok()) else { return };
    let Some(ucid) = ctx.db.player_in_unit(false, &oid) else { return };
    let Some(side) = ctx.db.player(&ucid).map(|p| p.side) else { return };
    // Air-to-air missiles only. They are guided on a unit or on nothing,
    // never on a map point -- `get_target()` on a point-targeted weapon
    // takes the server down (see `ShotDb::shot`), so nothing else gets near it.
    match e.weapon.get_weapon_desc() {
        Ok(d) if d.missile_category == Some(MissileCategory::Aam) => (),
        _ => return,
    }
    let Ok(Some(target)) = e.weapon.get_target() else { return };
    let Ok(target) = target.as_unit() else { return };
    if let Some(t) = friendly_air_target(&target, side) {
        warn_buddy_lock(ctx, &ucid, t, true, now);
    }
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
        // rather than stacking three lines that contradict each other. The
        // restart is a real-world time: wall clock, not campaign time.
        let wall = Utc::now();
        let left = asd.when - wall;
        let mut due: Option<usize> = None;
        for (i, (mins, _)) in SHUTDOWN_WARNINGS.iter().enumerate() {
            if !asd.warned[i] && left <= Duration::minutes(*mins) {
                asd.warned[i] = true;
                due = Some(i);
            }
        }
        if let Some(i) = due {
            let msg =
                format_compact!("The server will restart in {}", SHUTDOWN_WARNINGS[i].1);
            ctx.db.ephemeral.msgs().panel_to_all(60, i > 0, msg);
        }
        if wall > asd.when {
            return admin::admin_shutdown(ctx, lua, None);
        }
    }
    if let Some(victor) = ctx.db.check_victory(now) {
        // `Neutral` is a draw: the round ran `max_round_days`.
        let winner = (victor != Side::Neutral).then_some(victor);
        return admin::admin_shutdown(ctx, lua, Some(winner));
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
                    if player.side == side
                        && let Some((slot, _)) = player.current_slot.as_ref()
                    {
                        let mut dead: SmallVec<[JtId; 4]> = smallvec![];
                        let mut expunge = false;
                        if let Some(subd) = ctx.subscribed_jtac_menus.get_mut(slot) {
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
                                if subd.subscribed_objectives.contains(oid)
                                    && !dirty_slots.contains(slot)
                                {
                                    dirty_slots.push(*slot);
                                }
                                if !pinned.contains(oid) {
                                    subd.subscribed_objectives.remove(oid);
                                }
                            }
                        }
                        if !dead.is_empty() {
                            let dead = dead.drain(..);
                            if let Some(subd) = ctx.subscribed_jtac_menus.get_mut(slot) {
                                for jtid in dead {
                                    subd.pinned.remove(&jtid);
                                }
                            }
                        }
                        // Only drop the entry once it holds nothing: it
                        // also carries the slot's pins, which were lost
                        // whenever the last expanded location closed.
                        if let Some(subd) = ctx.subscribed_jtac_menus.get(slot) {
                            expunge = subd.subscribed_objectives.is_empty()
                                && subd.pinned.is_empty();
                        }
                        if expunge {
                            ctx.subscribed_jtac_menus.remove(slot);
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
                live.entry(jtac.side()).or_default().insert(jtac.location().oid);
            }
            let empty: FxHashSet<ObjectiveId> = FxHashSet::default();
            let moved: SmallVec<[Side; 3]> = [Side::Blue, Side::Red, Side::Neutral]
                .into_iter()
                .filter(|side| {
                    live.get(side).unwrap_or(&empty)
                        != ctx.jtac_locations.get(side).unwrap_or(&empty)
                })
                .collect();
            if !moved.is_empty() {
                ctx.jtac_locations = live;
                for (_, player, _) in ctx.db.instanced_players() {
                    if !moved.contains(&player.side) {
                        continue;
                    }
                    if let Some((slot, _)) = player.current_slot.as_ref()
                        && !dirty_slots.contains(slot)
                    {
                        dirty_slots.push(*slot);
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
    menu::jtac::flush_jtac_notices(ctx);
}

fn award_periodic_points(ctx: &mut Context, ts: DateTime<Utc>) {
    if let Some(points) = ctx.db.ephemeral.cfg.points.as_ref() {
        let (award, period) = points.periodic_point_gain;
        if award != 0 && period > 0 {
            let elapsed = (ts - ctx.last_periodic_points).num_seconds();
            if elapsed >= period as i64 {
                ctx.last_periodic_points = ts;
                // Only players in a slot: paying every connection paid AFK
                // spectators to sit there.
                for ifo in ctx.connected.info_by_player_id.values() {
                    if ctx.db.is_slotted(&ifo.ucid) {
                        ctx.db.earn_points(&ifo.ucid, award, "periodic award");
                    }
                }
            }
        }
    }
}

/// The economy (`economy`): income into the treasuries and the bases' funds.
/// What the AI buys with it is the HQ's (`hq::tick`, which runs the rules
/// mode when the theatre HQ is off).
fn tick_economy(ctx: &mut Context, ts: DateTime<Utc>) {
    let cfg = match ctx.db.ephemeral.cfg.smart_commander.as_ref() {
        Some(c) => c.clone(),
        None => return,
    };
    let elapsed = (ts - ctx.last_commander_tick).num_seconds();
    if elapsed < cfg.tick_period_secs as i64 {
        return;
    }
    ctx.last_commander_tick = ts;
    let mut ucids_by_side: fxhash::FxHashMap<
        dcso3::coalition::Side,
        Vec<dcso3::net::Ucid>,
    > = fxhash::FxHashMap::default();
    // Holding pay goes to players in a slot, not every connection.
    for ifo in ctx.connected.info_by_player_id.values() {
        if let Some(player) = ctx.db.persisted.players.get(&ifo.ucid)
            && player.current_slot.is_some()
        {
            ucids_by_side.entry(player.side).or_default().push(ifo.ucid);
        }
    }
    economy::tick(&mut ctx.db, &cfg, ts, &ucids_by_side);
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
    // A special SAM site's group is built from inline units under an
    // "@synthetic:<gid>" name, with no .miz template behind it, so it can't
    // be copied into an ambush force ("no such template @synthetic:8").
    let is_ground = |g: &db::group::SpawnedGroup| {
        matches!(
            g.class,
            ObjGroupClass::Armor
                | ObjGroupClass::Mr
                | ObjGroupClass::Sr
                | ObjGroupClass::Lr
        ) && !g.template_name.starts_with("@synthetic:")
    };

    // First try the requested objective's own groups
    if let Some(obj) = db.persisted.objectives.get(&objective)
        && let Some(gids) = obj.groups().get(&side)
    {
        let found = gids.into_iter().find_map(|gid| {
            db.persisted.groups.get(gid).and_then(|g| {
                if is_ground(g) { Some(g.template_name.clone()) } else { None }
            })
        });
        if found.is_some() {
            return found;
        }
    }

    // Fallback: find any ground template owned by this side (e.g. carrier objectives have no armor)
    db.persisted.objectives.into_iter().filter(|(_, o)| o.owner() == side).find_map(
        |(_, o)| {
            o.groups().get(&side)?.into_iter().find_map(|gid| {
                db.persisted.groups.get(gid).and_then(|g| {
                    if is_ground(g) { Some(g.template_name.clone()) } else { None }
                })
            })
        },
    )
}

fn drain_event_effects(lua: MizLua, ctx: &mut Context) {
    use crate::db::events::EventScheduler;
    let budget =
        EventScheduler::EFFECTS_PER_TICK.min(ctx.event_scheduler.pending_effects.len());
    let to_apply: Vec<_> = ctx.event_scheduler.pending_effects.drain(..budget).collect();
    if to_apply.is_empty() {
        return;
    }
    apply_event_effects(lua, ctx, to_apply);
}

fn apply_event_effects(lua: MizLua, ctx: &mut Context, effects: Vec<EventEffect>) {
    use crate::db::events::EventEffect;
    use crate::db::group::DeployKind;
    use crate::db::objective::ObjGroupClass;
    use crate::spawnctx::{SpawnCtx, SpawnLoc};
    use dcso3::Color;
    use dcso3::LuaVec2;
    use dcso3::controller::{
        ActionTyp, AltType, MissionPoint, PointType, Task, VehicleFormation,
    };
    use dcso3::group::Group;
    use dcso3::land::Land;
    use dcso3::trigger::{CircleSpec, LineType, SideFilter};

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
                    Err(e) => {
                        error!("FireBarrage: Land singleton: {e}");
                        continue;
                    }
                };

                let obj = ctx.db.persisted.objectives.get(&source_objective);
                let gids: Vec<_> = obj
                    .and_then(|o| o.groups().get(&side))
                    .map(|gs| gs.into_iter().copied().collect())
                    .unwrap_or_default();

                let barrage_radius = ctx
                    .db
                    .ephemeral
                    .cfg
                    .campaign_events
                    .as_ref()
                    .map(|c| c.barrage_radius_m)
                    .unwrap_or(500.0);
                let barrage_max_groups = ctx
                    .db
                    .ephemeral
                    .cfg
                    .campaign_events
                    .as_ref()
                    .map(|c| c.barrage_max_groups)
                    .unwrap_or(5);
                // Effective weapon range — stay 15% inside it to ensure the AI can engage.
                let arty_range =
                    ctx.db.ephemeral.cfg.artillery_mission_range as f64 * 0.85;

                let alt = land.get_height(LuaVec2(target_pos)).unwrap_or(0.);
                let fire_task = Task::FireAtPoint {
                    point: LuaVec2(target_pos),
                    radius: Some(barrage_radius),
                    expend_qty: None,
                    weapon_type: None,
                    altitude: Some(alt),
                    altitude_type: Some(AltType::BARO),
                    counter_battery_radius: shoot_and_scoot(&ctx.db.ephemeral.cfg),
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
                    if !alive {
                        continue;
                    }
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
                                action: Some(ActionTyp::Ground(
                                    VehicleFormation::OffRoad,
                                )),
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

                    if let Ok(dcs_group) = Group::get_by_name(lua, group_name.as_str())
                        && let Ok(controller) = dcs_group.get_controller()
                    {
                        if let Err(e) = controller.set_task(mission) {
                            error!("FireBarrage: set_task {group_name}: {e}");
                        } else {
                            fired += 1;
                            let action = if dist <= arty_range {
                                "firing in place"
                            } else {
                                "moving to firing position"
                            };
                            info!(
                                "FireBarrage: {:?} group {group_name} {action}, dist={:.0}m",
                                side, dist
                            );
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
                            center: dcso3::LuaVec3(dcso3::Vector3::new(
                                target_pos.x,
                                0.,
                                target_pos.y,
                            )),
                            radius: 2000.,
                            color: side_color(side),
                            fill_color: Color::new(0., 0., 0., 0.),
                            line_type: LineType::Dashed,
                            read_only: true,
                        },
                        Some(
                            format_compact!(
                                "Fire Support [{:?}] — {} units firing",
                                side,
                                fired
                            )
                            .into(),
                        ),
                    );
                    ctx.event_scheduler.register_mark(event_id, mid);
                }
            }

            // D: ALCM / Scud / HIMARS missile strike — fire pre-selected groups at target.
            EventEffect::FireMissileStrike {
                event_id,
                side,
                shooter_gids,
                target_pos,
            } => {
                let land = match Land::singleton(lua) {
                    Ok(l) => l,
                    Err(e) => {
                        error!("FireMissileStrike: Land singleton: {e}");
                        continue;
                    }
                };
                let alt = land.get_height(LuaVec2(target_pos)).unwrap_or(0.);
                let fire_task = Task::FireAtPoint {
                    point: LuaVec2(target_pos),
                    radius: Some(500.0),
                    expend_qty: None,
                    weapon_type: None,
                    altitude: Some(alt),
                    altitude_type: Some(AltType::BARO),
                    counter_battery_radius: shoot_and_scoot(&ctx.db.ephemeral.cfg),
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
                    if !alive {
                        continue;
                    }
                    let group_name = group.name.clone();
                    let center = ctx.db.group_center(gid).unwrap_or(target_pos);
                    let facing = group_facing(&ctx.db, gid);
                    if let Ok(dcs_group) = Group::get_by_name(lua, group_name.as_str())
                        && let Ok(controller) = dcs_group.get_controller()
                    {
                        let task = aim_and_fire_route(
                            center,
                            target_pos,
                            facing,
                            fire_task.clone(),
                        );
                        if let Err(e) = controller.set_task(task) {
                            error!("FireMissileStrike: set_task {group_name}: {e}");
                        } else {
                            fired += 1;
                            info!(
                                "FireMissileStrike: {:?} group {group_name} launching at {:?}",
                                side, target_pos
                            );
                        }
                    }
                }

                if fired > 0 {
                    let mid = dcso3::trigger::MarkId::new();
                    ctx.db.ephemeral.msgs().circle_to_all(
                        SideFilter::All,
                        mid,
                        CircleSpec {
                            center: dcso3::LuaVec3(dcso3::Vector3::new(
                                target_pos.x,
                                0.,
                                target_pos.y,
                            )),
                            radius: 3000.,
                            color: side_color(side),
                            fill_color: Color::new(0., 0., 0., 0.),
                            line_type: LineType::Dashed,
                            read_only: true,
                        },
                        Some(
                            format_compact!(
                                "Missile Strike [{:?}] — {} launchers firing",
                                side,
                                fired
                            )
                            .into(),
                        ),
                    );
                    ctx.event_scheduler.register_mark(event_id, mid);
                }
            }

            // E: Send the ambush force out of its objective by road to an
            // intercept ahead of the convoy. It used to appear next to the
            // convoy out of nowhere.
            EventEffect::SpawnAmbush {
                event_id,
                ambush_side,
                spawn_pos,
                source_objective,
                convoy_group_id,
                convoy_pos,
            } => {
                let template =
                    find_ground_template(&ctx.db, source_objective, ambush_side);
                let template = match template {
                    Some(t) => t,
                    None => {
                        info!(
                            "SpawnAmbush: no suitable template for {:?} at {:?}",
                            ambush_side, source_objective
                        );
                        continue;
                    }
                };
                // Where the force sets up; the warning mark goes there.
                let mut intercept = spawn_pos;
                let launched = match ctx.db.launch_ambush(
                    &spctx,
                    &ctx.idx,
                    ambush_side,
                    source_objective,
                    &template,
                    convoy_group_id,
                    spawn_pos,
                ) {
                    Ok(Some((gid, at))) => {
                        info!(
                            "SpawnAmbush: {:?} force {:?} on its way to {:?}",
                            ambush_side, gid, at
                        );
                        ctx.event_scheduler.ambush_groups.insert(event_id, gid);
                        intercept = at;
                        true
                    }
                    Ok(None) => false,
                    Err(e) => {
                        error!("SpawnAmbush: {e:?}");
                        false
                    }
                };
                if !launched {
                    // No road gets a force there in time: the ambush is off.
                    // The treasury was charged when the event was set (see
                    // spawn_convoy_ambush) and isn't refunded here.
                    warn!(
                        "SpawnAmbush: {:?} ambush {:?} called off, no force could reach the convoy (last seen {:?})",
                        ambush_side, event_id, convoy_pos
                    );
                    ctx.event_scheduler.active_events.retain(|ev| ev.id() != event_id);
                    if let Some(marks) = ctx.event_scheduler.event_marks.remove(&event_id)
                    {
                        for mid in marks {
                            ctx.db.ephemeral.msgs().delete_mark(mid);
                        }
                    }
                    continue;
                }
                // F10 warning mark
                let mid = dcso3::trigger::MarkId::new();
                ctx.db.ephemeral.msgs().circle_to_all(
                    SideFilter::All,
                    mid,
                    CircleSpec {
                        center: dcso3::LuaVec3(dcso3::Vector3::new(
                            intercept.x,
                            0.,
                            intercept.y,
                        )),
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
            EventEffect::SpawnCap {
                event_id,
                cap_side,
                objective,
                obj_pos,
                rotary,
                threat,
            } => {
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
                    if let Some(marks) = ctx.event_scheduler.event_marks.remove(&event_id)
                    {
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
                let candidates = cap_template_candidates(
                    cfg.campaign_events.as_deref(),
                    cap_side,
                    rotary,
                    threat,
                );
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
                                .map(|c| {
                                    if rotary {
                                        c.helo_cold_start
                                    } else {
                                        c.cap_cold_start
                                    }
                                })
                                .unwrap_or(true);
                            let mut tags: enumflags2::BitFlags<
                                bfprotocols::cfg::UnitTag,
                            > = bfprotocols::cfg::UnitTag::CAP.into();
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
                        Err(e) if warn_missing_template_once(template.as_str()) => warn!(
                            "SpawnCap: {} template '{}' not found -- add a {} group named '{}' to your mission file, or drop it from the roster. Trying the next one; not repeating this warning. ({e:?})",
                            if rotary { "helo patrol" } else { "CAP" },
                            template,
                            if rotary { "helicopter-section" } else { "plane-section" },
                            template
                        ),
                        Err(e) => debug!(
                            "SpawnCap: template '{}' still missing ({e:?})",
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
                            crate::db::events::CapSpawnWatch::new(ctx.now()),
                        );
                        ctx.event_scheduler
                            .cap_groups
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
                            .map(|c| {
                                if rotary {
                                    c.helo_duration_secs
                                } else {
                                    c.cap_duration_secs
                                }
                            })
                            .unwrap_or(600);
                        ctx.event_scheduler.cap_hard_expiry.insert(
                            event_id,
                            ctx.now() + chrono::Duration::seconds(2 * duration as i64),
                        );
                        // Queue initial task -- deferred until DCS reports the group alive.
                        ctx.event_scheduler
                            .pending_cap_tasks
                            .insert(gid, (obj_pos, rotary));
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
                                center: dcso3::LuaVec3(dcso3::Vector3::new(
                                    obj_pos.x, 5000., obj_pos.y,
                                )),
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
                                if rotary {
                                    "helo_templates_red/_blue"
                                } else {
                                    "cap_templates_red/_blue"
                                }
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
                        ctx.event_scheduler
                            .active_events
                            .retain(|ev| ev.id() != event_id);
                        ctx.event_scheduler.cap_hard_expiry.remove(&event_id);
                        // Clean up any F10 marks that may have been placed
                        if let Some(marks) =
                            ctx.event_scheduler.event_marks.remove(&event_id)
                        {
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
                        info!(
                            "DespawnAmbush: ambush event {:?} expired, units removed",
                            event_id
                        );
                    }
                }
            }

            // E: Remove all CAP groups when the event expires
            EventEffect::DespawnCap { event_id, cap_side, objective, rotary } => {
                let now = ctx.now();
                // Determine if this was a shootdown (all aircraft dead) or natural expiry.
                // If shot down, record the time so check_air_threats can enforce a cooldown.
                let was_shot_down = ctx
                    .event_scheduler
                    .cap_groups
                    .get(&event_id)
                    .map(|gids| {
                        gids.iter().all(|gid| {
                            // group_health returns (alive_count, total_count)
                            ctx.db
                                .group_health(gid)
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
                ctx.event_scheduler.cap_field_cooldown.insert((objective, rotary), now);
                // The commander's own CAP cooldown still reads these, so keep
                // them current for the fixed-wing case.
                match (cap_side, rotary) {
                    (Side::Blue, false) => {
                        ctx.event_scheduler.last_commander_cap_ended_blue = Some(now)
                    }
                    (Side::Red, false) => {
                        ctx.event_scheduler.last_commander_cap_ended_red = Some(now)
                    }
                    (Side::Blue, true) => {
                        ctx.event_scheduler.last_helo_patrol_ended_blue = Some(now)
                    }
                    (Side::Red, true) => {
                        ctx.event_scheduler.last_helo_patrol_ended_red = Some(now)
                    }
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
                // per-event bookkeeping that nothing else ever removed -- one
                // entry leaked per CAP wave for the life of the mission
                ctx.event_scheduler.cap_side_by_event.remove(&event_id);
                ctx.event_scheduler.cap_last_threat_seen.remove(&event_id);
                if let Some(gids) = ctx.event_scheduler.cap_groups.remove(&event_id) {
                    for gid in gids {
                        ctx.event_scheduler.cap_spawn_ts.remove(&gid);
                        let group_name = match ctx.db.persisted.groups.get(&gid) {
                            Some(g) => g.name.clone(),
                            None => continue,
                        };
                        let dcs_group = match dcso3::group::Group::get_by_name(
                            lua,
                            group_name.as_str(),
                        ) {
                            Ok(g) => g,
                            Err(_) => {
                                // Not in DCS, delete the tracked group immediately
                                let _ = ctx.db.delete_group(&gid);
                                continue;
                            }
                        };

                        let g_pos =
                            match dcs_group.get_unit(1).and_then(|u| u.get_point()) {
                                Ok(p) => dcso3::Vector2::new(p.x, p.z),
                                Err(_) => {
                                    let _ = ctx.db.delete_group(&gid);
                                    continue;
                                }
                            };

                        let best_obj = ctx
                            .db
                            .persisted
                            .objectives
                            .into_iter()
                            .filter(|(_, obj)| {
                                obj.owner == cap_side
                                    && is_launch_field(obj.kind(), rotary)
                            })
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
                                let _ =
                                    controller.set_task(dcso3::controller::Task::Land {
                                        point: dcso3::LuaVec2(obj.pos()),
                                        duration: None,
                                    });
                                info!(
                                    "DespawnCap: sent {} to RTB at {}",
                                    group_name,
                                    obj.name()
                                );
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

/// Shoot and scoot distance for artillery and launcher fire missions
/// (`artillery.shoot_and_scoot_m`), when configured.
pub(crate) fn shoot_and_scoot(cfg: &Cfg) -> Option<f64> {
    cfg.artillery.as_ref().and_then(|a| a.shoot_and_scoot_m)
}

/// The templates a scramble may spawn, best candidate first.
///
/// The side's roster (`cap_templates_red`/`_blue`, or the helo pair) filtered
/// to the entries this incursion qualifies for -- `min_threat` at or below
/// `threat` -- shuffled by weight, so the same airfield does not answer every
/// push with the same jets. The rest of the roster follows, lowest
/// `min_threat` first, as a last resort: something scrambling beats nothing,
/// and a single qualifying template missing from the .miz (as RCAPMIG29 was
/// on the live Modern mission) used to cancel every low-threat scramble.
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
    let mut rest: Vec<&bfprotocols::cfg::CapTemplateCfg> =
        roster.iter().filter(|t| t.min_threat > threat).collect();
    rest.sort_by(|a, b| a.min_threat.cmp(&b.min_threat).then(b.weight.cmp(&a.weight)));
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
    out.extend(rest.into_iter().map(|t| dcso3::String::from(t.template.as_str())));
    out
}

/// Warn about a missing CAP/helo template once per mission load, then quietly.
/// Every scramble walks the same roster, so a template missing from the .miz
/// used to put the same WARN in the log a dozen times an hour.
fn warn_missing_template_once(template: &str) -> bool {
    use std::{collections::HashSet, sync::Mutex};
    static WARNED: Mutex<Option<HashSet<std::string::String>>> = Mutex::new(None);
    let mut warned = WARNED.lock().unwrap_or_else(|e| e.into_inner());
    warned.get_or_insert_with(HashSet::new).insert(template.to_owned())
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
fn air_station_task(
    station: dcso3::Vector2,
    rotary: bool,
    cfg: &bfprotocols::cfg::CampaignEventsCfg,
    land: Option<&dcso3::land::Land>,
) -> dcso3::controller::Task {
    use dcso3::attribute::Attribute;
    use dcso3::controller::{OrbitPattern, Task};
    let (speed, altitude, engage, targets) = if rotary {
        let ground =
            land.and_then(|l| l.get_height(dcso3::LuaVec2(station)).ok()).unwrap_or(0.);
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
    use dcso3::LuaVec2;
    use dcso3::controller::{
        ActionTyp, AltType, MissionPoint, PointType, Task, VehicleFormation,
    };
    use dcso3::group::Group;
    use dcso3::land::Land;

    let land = match Land::singleton(lua) {
        Ok(l) => l,
        Err(e) => {
            error!("flush_pending_moves: Land singleton: {e}");
            return;
        }
    };

    // Process at most one pending move per tick to avoid stalling DCS Lua.
    let pending: Vec<_> = ctx
        .event_scheduler
        .pending_moves
        .iter()
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
        let task = Task::Mission { airborne: Some(false), route: route_points };
        let target_pos = target_route.last().copied().unwrap_or_default();
        if let Err(e) = controller.set_task(task) {
            error!("flush_pending_moves: set_task for {group_name}: {e}");
        } else {
            info!(
                "flush_pending_moves: ordered {group_name} to move ({} waypoints) → {:?}",
                target_route.len(),
                target_pos
            );
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
    let pending_cap: Vec<_> = ctx
        .event_scheduler
        .pending_cap_tasks
        .iter()
        .map(|(gid, pos)| (*gid, *pos))
        .collect();

    let events_cfg = ctx.db.ephemeral.cfg.campaign_events.clone().unwrap_or_default();
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
    use crate::db::events::CampaignEvent;
    use dcso3::Vector2;
    use dcso3::land::Land;

    // Restation only when the target has moved meaningfully -- re-issuing an
    // identical task every tick interrupts an in-progress intercept. A
    // helicopter patrol works a much smaller box, so its threshold is smaller
    // too: 15 km of drift is most of a rotary patrol's whole area.
    const RESTATION_THRESHOLD_M: f64 = 15_000.0;
    const ROTARY_RESTATION_THRESHOLD_M: f64 = 5_000.0;
    let events_cfg = ctx.db.ephemeral.cfg.campaign_events.clone().unwrap_or_default();
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
            let since =
                *ctx.event_scheduler.cap_last_threat_seen.entry(event_id).or_insert(now);
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
                let sum = threats.iter().fold(Vector2::new(0., 0.), |acc, p| {
                    Vector2::new(acc.x + p.x, acc.y + p.y)
                });
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
            let in_contact = threats
                .iter()
                .any(|t| na::distance(&(*t).into(), &station.into()) <= engage_radius);
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
        let threshold =
            if rotary { ROTARY_RESTATION_THRESHOLD_M } else { RESTATION_THRESHOLD_M };
        let restation =
            ctx.event_scheduler.cap_station_by_event.get(&event_id).is_none_or(|prev| {
                na::distance(&(*prev).into(), &station.into()) > threshold
            });
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
            let dcs_group =
                match dcso3::group::Group::get_by_name(lua, group_name.as_str()) {
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
    let pending: Vec<(GroupId, DateTime<Utc>)> =
        ctx.event_scheduler.cap_rtb.iter().map(|(g, t)| (*g, *t)).collect();
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
    let checks: Vec<(GroupId, crate::db::events::CapSpawnWatch)> =
        ctx.event_scheduler.cap_spawn_ts.iter().map(|(g, w)| (*g, *w)).collect();
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
        let in_air: Option<bool> =
            dcso3::group::Group::get_by_name(lua, group_name.as_str())
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
                            t.contains(UnitTag::Aircraft)
                                && !t.contains(UnitTag::Helicopter)
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
    /// Chance an incursion is answered at once; otherwise the answer comes
    /// 2-8 minutes later (`cap_probability`; helos always at once).
    probability: f64,
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
                probability: 1.,
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
                probability: c.cap_probability,
            }
        }
    }
}

/// True if `kind` is a field the response can launch from. Fixed-wing CAP needs
/// a runway; helicopters are happy with a FARP pad or, thanks to the
/// open-ground hot start, a FOB with no pad object at all.
fn is_launch_field(
    kind: &bfprotocols::db::objective::ObjectiveKind,
    rotary: bool,
) -> bool {
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
    use crate::db::events::{CampaignEvent, EventId, bearing_to_compass};
    use dcso3::Vector2;
    use dcso3::coalition::Side;

    let events_cfg = match ctx.db.ephemeral.cfg.campaign_events.as_ref() {
        Some(c)
            if c.enabled
                && if rotary { c.enemy_helo_enabled } else { c.enemy_cap_enabled } =>
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
                Side::Red => {
                    active_red.insert(*objective);
                }
                Side::Blue => {
                    active_blue.insert(*objective);
                }
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
    let centroid = rules::centroid(
        ctx.db
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, o)| o.owner() != Side::Neutral)
            .map(|(_, o)| o.pos()),
    );

    // For each side that still has room, check detections and cluster them.
    // We iterate Red-defends-against-Blue and Blue-defends-against-Red.
    for defending_side in [Side::Red, Side::Blue] {
        let side_active_count = match defending_side {
            Side::Red => active_red.len(),
            Side::Blue => active_blue.len(),
            _ => continue,
        };
        if active_red.len() + active_blue.len() >= max_concurrent {
            break; // global cap hit mid-loop
        }
        if !rules::wave_has_room(
            side_active_count,
            active_red.len() + active_blue.len(),
            max_per_side,
            max_concurrent,
        ) {
            continue; // this side is already at its per-side limit
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
            ctx.ewr.detected_enemy_player_count(defending_side, now, &ctx.db, rotary)
        };
        // Air-balance: scramble for the outnumbered side even without a
        // detected incursion (Blue 4 up, Red 1 up → Red gets one). This is the
        // path that covers "tonight only helo pilots showed up".
        let my_air = enemy_players_airborne(&ctx.db, defending_side, rotary);
        let their_air = enemy_players_airborne(&ctx.db, attacking, rotary);
        let outnumbered = rules::outnumbered(my_air, their_air, rc.balance_gap);
        let scramble = rules::scramble_reason(
            player_threat_count,
            min_threat,
            my_air,
            their_air,
            rc.balance_gap,
        );
        if scramble == rules::Scramble::No {
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
            let slot = if defending_side == Side::Red { 0 } else { 1 }
                + if rotary { 2 } else { 0 };
            let pic = ((player_threat_count.min(255) as u32) << 16)
                | ((my_air.min(255) as u32) << 8)
                | their_air.min(255) as u32;
            let secs = now.timestamp();
            let changed = LAST_PIC[slot].swap(pic, Ordering::Relaxed) != pic;
            let stale =
                secs.saturating_sub(LAST_LOG[slot].load(Ordering::Relaxed)) >= 600;
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
        if scramble == rules::Scramble::Balance {
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
            if assigned[i] {
                continue;
            }
            assigned[i] = true;
            let mut cx = detected[i].x;
            let mut cz = detected[i].y;
            let mut count = 1usize;
            for j in (i + 1)..detected.len() {
                if assigned[j] {
                    continue;
                }
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
        ctx.event_scheduler.cap_sortie_log.retain(|(_, _, when)| *when > hour_ago);
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

        for (cluster_center, cluster_count) in clusters.into_iter().take(slots_remaining)
        {
            if sorties_left == 0 {
                break; // budget spent on an earlier cluster this pass
            }
            // Closest friendly field that doesn't already have an active
            // response of this class. Fixed-wing CAP needs a runway; the helo
            // patrol also accepts FARPs and FOBs, which is usually much closer
            // to the rotary fight. Neutral objectives are excluded (owner ==
            // defending_side already ensures non-neutral).
            let best_obj = ctx
                .db
                .persisted
                .objectives
                .into_iter()
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
                            .is_none_or(|ended| now - *ended >= respawn_cooldown)
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

            // `cap_probability` was declared and documented but never read,
            // so every incursion met fighters on the same beat. Now a failed
            // roll doesn't cancel the response, it delays it: the fighters
            // turn up 2-8 minutes into the attack, or not before it's over.
            if rc.probability < 1. {
                let key = (defending_side, rotary);
                match ctx.event_scheduler.cap_hold_until.get(&key).copied() {
                    Some(t) if now < t => continue,
                    Some(_) => {
                        ctx.event_scheduler.cap_hold_until.remove(&key);
                    }
                    None => {
                        let mut rng = rand::thread_rng();
                        if !rand::Rng::gen_bool(&mut rng, rc.probability.clamp(0., 1.)) {
                            let wait = rand::Rng::gen_range(&mut rng, 120..=480);
                            ctx.event_scheduler
                                .cap_hold_until
                                .insert(key, now + chrono::Duration::seconds(wait));
                            info!(
                                "{}: {defending_side:?} answers this incursion in {wait}s",
                                rc.label
                            );
                            continue;
                        }
                    }
                }
            }
            let obj_name = dcso3::String::from(obj.name.as_str());
            let direction = bearing_to_compass(centroid, cluster_center);
            let attacking_side = match defending_side {
                Side::Red => Side::Blue,
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
                Side::Red => {
                    active_red.insert(*oid);
                }
                Side::Blue => {
                    active_blue.insert(*oid);
                }
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
    use dcso3::LuaVec3;
    use dcso3::world::{SearchVolume, World};

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
        Err(e) => {
            error!("remove_junk: World singleton: {e}");
            return;
        }
    };
    let volume = SearchVolume::Sphere {
        point: LuaVec3(dcso3::Vector3::new(0., 0., 0.)),
        radius: cfg.junk_removal_radius_m,
    };
    match world.remove_junk(volume) {
        Ok(n) => {
            if n > 0 {
                info!("remove_junk: removed {} objects", n);
            }
        }
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
            ctx.db.persisted.navaids.into_iter().map(|(oid, _)| *oid).collect()
        } else {
            changed
        };
        if !targets.is_empty() {
            info!("navaids: refreshing {} objective(s)", targets.len());
            for oid in targets {
                let Some(navs) = ctx.db.persisted.navaids.get(&oid).cloned() else {
                    continue;
                };
                for nav in &navs {
                    let Some(host) = nav.host_gid else { continue };
                    let Ok(group) = ctx.db.group(&host).map(|g| g.name.clone()) else {
                        continue;
                    };
                    match dcso3::group::Group::get_by_name(lua, group.as_str()) {
                        Ok(g) => {
                            if let Err(e) = navaids::activate_on_group(&g, nav) {
                                warn!(
                                    "navaids: activate {oid:?} on {group} failed: {e:?}"
                                );
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
        let Ok(g) = dcso3::group::Group::get_by_name(lua, gname.as_str()) else {
            continue;
        };
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
            let Some(deck) = deck else {
                all_lit = false;
                continue;
            };
            if dcso3::airbase::Airbase::get_by_name(
                lua,
                deck.get_name().unwrap_or_default(),
            )
            .is_err()
            {
                all_lit = false; // deck not registered as an airbase yet -- retry
                continue;
            }
            match navaids::activate_carrier(deck, nav) {
                Ok(()) => {
                    info!("navaids: lit carrier deck {:?} for {oid:?}", nav.deck_unit)
                }
                Err(e) => {
                    warn!(
                        "navaids: carrier {oid:?} deck {:?} activate failed: {e:?}",
                        nav.deck_unit
                    );
                    all_lit = false;
                }
            }
        }
        if all_lit {
            ctx.navaid_carriers_lit.insert(oid);
        }
    }
}

/// Run one subsystem of the timed events inside its own panic boundary, then
/// handle the DCS events it caused (see `CtxGuard`).
///
/// The whole tick used to share one `catch_unwind`, so a panic in any
/// subsystem -- or an early `?` -- skipped everything after it, including the
/// save. One broken subsystem now costs only itself.
fn step<R>(
    lua: MizLua,
    ctx: &mut Context,
    what: &str,
    f: impl FnOnce(&mut Context) -> R,
) -> Option<R> {
    let r = match catch_unwind(AssertUnwindSafe(|| f(ctx))) {
        Ok(r) => Some(r),
        Err(e) => {
            error!("{what} panicked: {}", bg::panic_msg(&e));
            None
        }
    };
    if let Err(e) = catch_unwind(AssertUnwindSafe(|| drain_deferred_events(lua, ctx))) {
        error!("handling deferred events after {what} panicked: {}", bg::panic_msg(&e))
    }
    r
}

/// The phases of the slow pass, in order. Each runs once per pass, and a
/// later phase never runs before an earlier one of the same pass (the HQ
/// still plans before the campaign events apply its effects, and both
/// before the ground war follows it).
const SLOW_PHASES: [fn(
    MizLua,
    &mut Context,
    &mut PerfInner,
    &PathBuf,
    DateTime<Utc>,
    DateTime<Utc>,
) -> Result<AdminResult>; 5] =
    [slow_phase_0, slow_phase_1, slow_phase_2, slow_phase_3, slow_phase_4];

/// Environment variable: how many consecutive 1 s ticks the slow pass is
/// spread over (1 to 5). Unset means as many as fit in
/// `slow_timed_events_freq`, up to 5. `1` restores the old behaviour, the
/// whole pass in one frame.
const SLOW_TICK_SPREAD_ENV: &str = "FOWL_SLOW_TICK_SPREAD";

/// How many 1 s ticks one slow pass is spread over.
fn slow_tick_buckets(freq_secs: u64) -> usize {
    let max = SLOW_PHASES.len();
    let spread = std::env::var(SLOW_TICK_SPREAD_ENV)
        .ok()
        .and_then(|v| v.trim().parse::<usize>().ok())
        .unwrap_or(max);
    spread.clamp(1, max).min((freq_secs as usize).max(1))
}

/// The phases bucket `bucket` (of `buckets`) runs: consecutive, in order,
/// every phase exactly once across the buckets.
fn slow_bucket_phases(bucket: usize, buckets: usize) -> std::ops::Range<usize> {
    let n = SLOW_PHASES.len();
    (bucket * n / buckets)..((bucket + 1) * n / buckets)
}

/// The slow pass: every `slow_timed_events_freq` seconds, every subsystem in
/// `SLOW_PHASES` runs once.
///
/// All ~50 subsystems used to run in the same DCS frame, every 10 s -- a
/// visible hitch on a busy server. Now the pass is spread over the next few
/// 1 s ticks (`slow_tick_buckets`), a share of the phases per tick, keeping
/// their order and the pass's start time. The cadence is unchanged: a pass
/// starts `slow_timed_events_freq` after the previous one started, and not
/// before the previous one has finished. `perf.slow_timed` is now the time
/// spent in one tick's share, which is what the frame actually pays.
fn run_slow_timed_events(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    path: &PathBuf,
    ts: DateTime<Utc>,
) -> Result<AdminResult> {
    let (bucket, buckets, start_ts) = match ctx.slow_pass {
        Some(p) => p,
        None => {
            let freq_secs = ctx.db.ephemeral.cfg.slow_timed_events_freq as u64;
            let freq = Duration::seconds(freq_secs as i64);
            if ts - ctx.last_slow_timed_events < freq {
                return Ok(AdminResult::Continue);
            }
            let start_ts = ctx.now();
            ctx.last_slow_timed_events = start_ts;
            (0, slow_tick_buckets(freq_secs), start_ts)
        }
    };
    let t0 = Utc::now();
    ctx.slow_pass = (bucket + 1 < buckets).then_some((bucket + 1, buckets, start_ts));
    for phase in slow_bucket_phases(bucket, buckets) {
        if let AdminResult::Shutdown =
            SLOW_PHASES[phase](lua, ctx, perf, path, ts, start_ts)?
        {
            ctx.slow_pass = None;
            return Ok(AdminResult::Shutdown);
        }
    }
    record_perf(&mut perf.slow_timed, t0);
    Ok(AdminResult::Continue)
}

/// Slow pass, phase 0: save, players, lives, kills, repairs, C-130 and actions.
#[allow(unused_variables)]
fn slow_phase_0(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    path: &PathBuf,
    ts: DateTime<Utc>,
    start_ts: DateTime<Utc>,
) -> Result<AdminResult> {
    // Save first. The snapshot used to be taken near the end of this tick,
    // after two dozen subsystems, so any one of them panicking or bailing out
    // early skipped the save -- every tick, for as long as it kept failing.
    // Whatever this tick changes is simply in the next snapshot.
    step(lua, ctx, "snapshot", |ctx| {
        let now = Utc::now();
        if let Some(snap) = ctx.db.maybe_snapshot() {
            ctx.do_bg_task(bg::Task::SaveState(path.clone(), snap));
        }
        record_perf(&mut perf.snapshot, now);
    });

    // DCS doesn't reliably fire onPlayerDisconnect for abrupt
    // disconnects (client crash, network drop), which would otherwise
    // leave a player stuck "connected" forever in the persisted db.
    // Reconcile against the live player list and force-disconnect
    // anyone we think is connected that DCS no longer reports.
    step(lua, ctx, "player reconciliation", |ctx| {
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
            Err(e) => warn!(
                "failed to get live player list for connection reconciliation {e:?}"
            ),
        }
    });

    // Dispatch pending achievements
    step(lua, ctx, "achievements", |ctx| {
        let achievements = std::mem::take(&mut ctx.db.ephemeral.pending_achievements);
        for achievement in achievements {
            crate::api::dispatch_event("achievement", &achievement);
        }
    });

    match step(lua, ctx, "auto shutdown", |ctx| check_auto_shutdown(ctx, lua, ts)) {
        Some(Ok(AdminResult::Continue)) | None => (),
        Some(Ok(AdminResult::Shutdown)) => return Ok(AdminResult::Shutdown),
        Some(Err(e)) => error!("failed to check for auto shutdown {e:?}"),
    }
    step(lua, ctx, "warehouse sync", |ctx| {
        for (oid, vh) in ctx.db.ephemeral.warehouses_to_sync() {
            if let Err(e) = ctx.db.sync_vehicle_at_obj(lua, oid, vh.clone()) {
                error!(
                    "failed to sync warehouse at objective {:?} vehicle {:?} {:?}",
                    oid, vh, e
                )
            }
        }
    });
    step(lua, ctx, "return lives", |ctx| {
        return_lives(lua, ctx, ts);
        ctx.recently_born.retain(|_, ts| start_ts - *ts <= Duration::seconds(5));
    });
    step(lua, ctx, "report kills", |ctx| {
        let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
        if let Ok(t) = Timer::singleton(lua).and_then(|t| t.get_abs_time()) {
            ctx.db.ephemeral.mission_hour = Some(((t.0 % 86_400.) / 3_600.) as u8);
        }
        for dead in ctx.shots_out.bring_out_your_dead(ts) {
            info!("kill {:?}", dead);
            if let Some(points) = cfg.points.as_ref() {
                ctx.db.award_kill_points(points, &dead)
            }
            // Detect convoy interdiction
            if let bfprotocols::shots::Who::AI { gid, side, .. } = &dead.victim
                && let Some(convoy_info) = ctx.db.convoy_info_for_group(gid)
            {
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
                        ctx.db.earn_points(&ucid, award, "convoy interdiction");
                    }
                }
            }

            ctx.do_bg_task(Task::Stat(Stat::Kill(dead)));
        }
    });
    // Before repairs: a ghost enemy unit parked by a base keeps it
    // "threatened", and a threatened base never repairs.
    step(lua, ctx, "ghost units", |ctx| ctx.db.reconcile_ghosts(lua, ts));
    step(lua, ctx, "repairs", |ctx| {
        let perf_ts = Utc::now();
        if let Err(e) = ctx.db.maybe_do_repairs(ts) {
            error!("error doing repairs {:?}", e)
        }
        record_perf(&mut perf.do_repairs, perf_ts);
    });

    step(lua, ctx, "navaids", |ctx| maybe_reallocate_navaids(lua, ctx));

    // Process C-130 physical cargo spawn queue (one shared queue for all
    // players, not per-slot -- each queued crate carries its own frozen
    // spawn anchor from when it was queued)
    step(lua, ctx, "C-130 spawn queue", |ctx| {
        if let Err(e) = ctx.db.process_c130_spawn_queue(lua, &ctx.idx) {
            error!("error processing C-130 spawn queue: {:?}", e)
        }
    });

    // Update C-130 physical crates (track airdrops and auto-unpack)
    step(lua, ctx, "C-130 crates", |ctx| {
        if let Err(e) = ctx.db.update_c130_crates(lua, &ctx.idx) {
            error!("error updating C-130 crates: {:?}", e)
        }
    });

    step(lua, ctx, "actions", |ctx| {
        if let Err(e) = ctx.db.advance_actions(lua, &ctx.idx, &ctx.jtac, start_ts) {
            error!("could not advance actions {e:?}")
        }
    });
    Ok(AdminResult::Continue)
}

/// Slow pass, phase 1: EWR, intel, recon and unit culling.
#[allow(unused_variables)]
fn slow_phase_1(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    path: &PathBuf,
    ts: DateTime<Utc>,
    start_ts: DateTime<Utc>,
) -> Result<AdminResult> {
    step(lua, ctx, "ewr tracks", |ctx| {
        let (ts, perf_ts) = (ctx.now(), Utc::now());
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
        record_perf(&mut perf.ewr_tracks, perf_ts);
    });

    // ELINT/SIGINT: decay intel contacts and refresh/remove their F10 marks.
    // JTAC eyes-on contacts whose units died go first, so their pins come off
    // in the same pass instead of fading out over the JTAC half-life.
    step(lua, ctx, "intel decay", |ctx| {
        ctx.db.retire_dead_jtac_intel();
        let now = ctx.now();
        ctx.db.ephemeral.tick_intel_decay(now)
    });

    // Player recon passes: advance timers, run scans, reveal contacts.
    step(lua, ctx, "recon sessions", |ctx| {
        let now = ctx.now();
        if let Err(e) = ctx.db.tick_recon_sessions(lua, &mut ctx.landcache, now) {
            error!("could not tick recon sessions {e}")
        }
    });

    step(lua, ctx, "ewr reports", |ctx| {
        let (ts, perf_ts) = (ctx.now(), Utc::now());
        if let Err(e) = generate_ewr_reports(ctx, ts) {
            error!("could not generate ewr reports {e}")
        }
        record_perf(&mut perf.ewr_reports, perf_ts);
    });
    step(lua, ctx, "buddy spikes", |ctx| {
        let now = ctx.now();
        check_buddy_spikes(ctx, lua, now)
    });
    step(lua, ctx, "unit culling", |ctx| {
        let (ts, perf_ts) = (ctx.now(), Utc::now());
        match ctx.db.cull_or_respawn_objectives(lua, &mut ctx.landcache, ts) {
            Err(e) => error!("could not cull or respawn objectives {e}"),
            Ok((threatened, cleared)) => {
                // An objective that has gone quiet can be threatened again.
                // `threat_notified` used to only ever grow, so each objective
                // announced "enemy contact" once per mission and never again.
                for oid in cleared {
                    ctx.db.ephemeral.threat_notified.remove(&oid);
                }
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
                        // was `?`, which abandoned the rest of the tick --
                        // save included -- over one missing objective
                        if let Ok(obj) = ctx.db.objective(&oid) {
                            let (owner, pos, name) =
                                (obj.owner(), obj.pos(), obj.name().to_string());
                            ctx.db
                                .ephemeral
                                .on_objective_threatened(pos, owner, &name, ts);
                        }
                    }
                    // Under-attack notification with cooldown
                    let ua_cooldown = ctx
                        .db
                        .ephemeral
                        .cfg
                        .under_attack
                        .as_ref()
                        .map(|c| c.cooldown_secs);
                    if let Some(cooldown_secs) = ua_cooldown {
                        let cooldown = chrono::Duration::seconds(cooldown_secs as i64);
                        let last =
                            ctx.db.ephemeral.last_under_attack_notif.get(&oid).copied();
                        if last.map(|t| ts - t >= cooldown).unwrap_or(true) {
                            ctx.db.ephemeral.last_under_attack_notif.insert(oid, ts);
                            if let Ok(obj) = ctx.db.objective(&oid) {
                                let (owner, pos, name) =
                                    (obj.owner(), obj.pos(), obj.name().to_string());
                                ctx.db.ephemeral.on_objective_under_attack(
                                    pos,
                                    owner,
                                    &name,
                                    cooldown_secs as i64,
                                    ts,
                                );
                            }
                        }
                    }
                }
            }
        }
        // Mercy timer check
        if let Some(losing_side) = ctx.db.check_last_stand(ts) {
            ctx.db.trigger_last_stand_victory(ts, losing_side.opposite());
        }
        record_perf(&mut perf.unit_culling, perf_ts);
    });
    Ok(AdminResult::Continue)
}

/// Slow pass, phase 2: markup, production, carriers, frontline, tasks and JTAC.
#[allow(unused_variables)]
fn slow_phase_2(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    path: &PathBuf,
    ts: DateTime<Utc>,
    start_ts: DateTime<Utc>,
) -> Result<AdminResult> {
    step(lua, ctx, "objective markup", |ctx| {
        let ts = Utc::now();
        if let Err(e) = ctx.db.update_objectives_markup() {
            error!("could not remark objectives {e}")
        }
        record_perf(&mut perf.remark_objectives, ts);
    });
    step(lua, ctx, "production", |ctx| {
        let ts = ctx.now();
        if let Err(e) = ctx.db.run_factory_production(ts) {
            error!("could not run factory production {e}")
        }
        if let Err(e) = ctx.db.check_scenery_buildings(lua, ts) {
            error!("could not check scenery buildings {e}")
        }
    });
    step(lua, ctx, "carrier repairs", |ctx| {
        let ts = ctx.now();
        match ctx.db.check_carrier_repairs(ts) {
            Ok(completed) => {
                for (oid, name) in completed {
                    if let Ok(obj) = ctx.db.objective(&oid) {
                        let owner = obj.owner();
                        let msg = format_compact!(
                            "{} has been fully repaired and is operational",
                            name
                        );
                        ctx.db.ephemeral.msgs().panel_to_side(15, false, owner, msg);
                    }
                }
            }
            Err(e) => error!("could not check carrier repairs {e}"),
        }
    });
    step(lua, ctx, "carrier capture", |ctx| {
        let ts = ctx.now();
        match ctx.db.check_carrier_group_capture(lua, &ctx.idx, ts) {
            Ok(captures) => {
                for (oid, old_owner, new_owner) in captures {
                    ctx.event_scheduler.owned_cache_dirty = true;
                    if let Ok(obj) = ctx.db.objective(&oid) {
                        let msg_old = format_compact!(
                            "{} has been captured by the enemy!",
                            obj.name()
                        );
                        let msg_new = format_compact!(
                            "You have captured {} with its aircraft! Carrier at 50% health",
                            obj.name()
                        );
                        ctx.db
                            .ephemeral
                            .msgs()
                            .panel_to_side(20, true, old_owner, msg_old);
                        ctx.db
                            .ephemeral
                            .msgs()
                            .panel_to_side(20, true, new_owner, msg_new);
                    }
                }
            }
            Err(e) => error!("could not check carrier captures {e}"),
        }
        // Auto-repair damaged carriers near naval bases
        match ctx.db.check_carrier_auto_repair(ts) {
            Ok(messages) => {
                for (side, msg) in messages {
                    ctx.db.ephemeral.msgs().panel_to_side(15, false, side, msg);
                }
            }
            Err(e) => error!("could not check carrier auto repair {e}"),
        }
    });
    step(lua, ctx, "frontline", |ctx| {
        let (ts, perf_ts) = (ctx.now(), Utc::now());
        update_frontline(ctx, ts, false);
        record_perf(&mut perf.frontline, perf_ts);
    });
    let now = ctx.now();
    step(lua, ctx, "tasks", |ctx| ctx.db.tick_tasks(now));
    step(lua, ctx, "map layer", |ctx| {
        ctx.db.ephemeral.update_map_layer(&ctx.db.persisted, now)
    });
    // `slow_timed` is the whole pass (recorded once, below); the per-step
    // histograms time only their own step.
    let ts = Utc::now();
    step(lua, ctx, "jtac contacts", |ctx| update_jtac_contacts(ctx, lua));
    step(lua, ctx, "jtac marks", |ctx| ctx.jtac.reconcile_marks(lua, &mut ctx.db));
    record_perf(&mut perf.update_jtac_contacts, ts);
    Ok(AdminResult::Continue)
}

/// Slow pass, phase 3: points, commander, HQ and campaign events.
#[allow(unused_variables)]
fn slow_phase_3(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    path: &PathBuf,
    ts: DateTime<Utc>,
    start_ts: DateTime<Utc>,
) -> Result<AdminResult> {
    step(lua, ctx, "periodic points", |ctx| award_periodic_points(ctx, start_ts));
    step(lua, ctx, "economy", |ctx| tick_economy(ctx, start_ts));
    // The AI plans and buys the war's missions and logistics: the theatre
    // HQ (`smart_commander.hq`), or without it the rules mode's one
    // campaign event per check. Before the campaign events below so the
    // effects it queues are applied this tick, and before the ground war so
    // the ground AI follows this pass's main effort.
    step(lua, ctx, "hq", |ctx| hq::tick(lua, ctx, perf, start_ts));

    // Tick campaign events: active event processing (expiry, effects,
    // escalation). This, and the upkeep of the helo missions, CAP flights and
    // queued effects below, runs whether or not `campaign_events.enabled` is
    // set: the commander and the HQ launch helo missions and ambushes either
    // way, and those must still be flown, paid off and cleaned up. Only new
    // events are gated on `enabled` (the reactive CAP in check_air_threats;
    // other new event spawning is the HQ's, above).
    let events_cfg = ctx.db.ephemeral.cfg.campaign_events.clone().unwrap_or_default();
    {
        step(lua, ctx, "campaign events", |ctx| {
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
        });
        step(lua, ctx, "event effects", |ctx| drain_event_effects(lua, ctx));
        // Retry deferred move orders for newly-spawned groups (1 per tick max).
        step(lua, ctx, "pending moves", |ctx| flush_pending_moves(lua, ctx));
        // Reactive CAP: spawn intercepts wherever enemy aircraft are detected
        step(lua, ctx, "air threats", |ctx| check_air_threats(ctx, start_ts, false));
        // Same pass for helicopters, which CAP deliberately ignores --
        // otherwise a night where only helo pilots show up gets no AI
        // response at all, on either side.
        step(lua, ctx, "helo threats", |ctx| check_air_threats(ctx, start_ts, true));
        // Kill any CAP that air-started instead of taxiing out.
        step(lua, ctx, "cap ground start", |ctx| {
            enforce_cap_ground_start(lua, ctx, start_ts)
        });
        // Remove flights that have finished flying home.
        step(lua, ctx, "cap rtb", |ctx| flush_cap_rtb(lua, ctx, start_ts));
        // AI helo missions: poll in-flight troop-insertion / resource-delivery
        // helos, apply the payoff and despawn the ones that have landed.
        step(lua, ctx, "helo missions", |ctx| {
            if let Err(e) = ctx.db.tick_helo_missions(lua, start_ts) {
                error!("error ticking helo missions {e:?}");
            }
            // AI Fat Cows: set up on landing, pack up when their time is up.
            if let Err(e) = ctx.db.tick_fat_cow_missions(lua, &ctx.idx, start_ts) {
                error!("error ticking fat cows {e:?}");
            }
            // Troop insertions whose helo failed, going in by road instead.
            if let Err(e) = ctx.db.tick_ground_insertions(lua, start_ts) {
                error!("error ticking road troop insertions {e:?}");
            }
        });
        // Dynamic CAP retargeting: redirect active CAP groups toward enemy aircraft.
        step(lua, ctx, "retarget_cap_groups", |ctx| {
            retarget_cap_groups(lua, ctx, start_ts)
        });
    }
    Ok(AdminResult::Continue)
}

/// Slow pass, phase 4: air life, modern war, ground war and housekeeping.
#[allow(unused_variables)]
fn slow_phase_4(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    path: &PathBuf,
    ts: DateTime<Utc>,
    start_ts: DateTime<Utc>,
) -> Result<AdminResult> {
    // Wingmen, autonomous AI packages and civil traffic (`Cfg::air_life`).
    step(lua, ctx, "air life", |ctx| airlife::tick(lua, ctx, perf, start_ts));
    // EW, SAM magazines, strike raids, sea drones, tempo (`Cfg::modern_war`).
    step(lua, ctx, "modern war", |ctx| modern_war::tick(lua, ctx, perf, start_ts));
    step(lua, ctx, "hunters", |ctx| command::tick_hunters(lua, ctx, start_ts));
    step(lua, ctx, "ramp watch", |ctx| ctx.db.check_ramp_watch(lua, start_ts));
    step(lua, ctx, "air stations", |ctx| ctx.db.flush_pending_air_stations(lua));
    step(lua, ctx, "last defenders", |ctx| ctx.db.reveal_last_defenders(start_ts));
    step(lua, ctx, "contributions", |ctx| ctx.db.drain_contributions());
    step(lua, ctx, "task zones", |ctx| ctx.db.expire_tasks(start_ts));
    // Ground formations, the AI ground commander and players' ground orders
    // (`Cfg::ground_war`).
    step(lua, ctx, "ground war", |ctx| groundwar::tick(lua, ctx, perf, start_ts));
    step(lua, ctx, "junk removal", |ctx| remove_junk_periodic(lua, ctx, start_ts));
    // Publish weather to dashboard every 5 minutes
    if (start_ts - ctx.last_weather_publish).num_seconds() >= 300 {
        ctx.last_weather_publish = start_ts;
        step(lua, ctx, "weather publish", |ctx| {
            if let Err(e) = atis::publish_weather(lua, ctx) {
                error!("failed to publish weather: {e:?}");
            }
        });
    }
    Ok(AdminResult::Continue)
}

fn run_timed_events(
    ctx: &mut Context,
    lua: MizLua,
    path: &PathBuf,
) -> Result<AdminResult> {
    // campaign time for the campaign, the wall clock for the perf records
    let ts = ctx.now();
    let perf_ts = Utc::now();
    let mut perf_guard = Perf::get_mut();
    let perf = Arc::make_mut(&mut perf_guard.inner);
    step(lua, ctx, "force to spectators", |ctx| match Net::singleton(lua) {
        Ok(net) => force_players_to_spectators(ctx, &net, ts),
        Err(e) => error!("no net singleton {e:?}"),
    });
    step(lua, ctx, "unit positions", |ctx| {
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
        record_perf(&mut perf.unit_positions, perf_ts);
    });
    step(lua, ctx, "player positions", |ctx| {
        let (ts, perf_ts) = (ctx.now(), Utc::now());
        match ctx.db.update_player_positions_incremental(
            lua,
            ts,
            ctx.last_player_position,
        ) {
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
        record_perf(&mut perf.player_positions, perf_ts);
    });

    step(lua, ctx, "takeoff holds", |ctx| announce_takeoff_holds(lua, ctx, ts));
    step(lua, ctx, "confirm takeoffs", |ctx| confirm_takeoffs(lua, ctx, ts));
    step(lua, ctx, "rear bases", |ctx| enforce_rear_bases(lua, ctx, ts));
    step(lua, ctx, "banned loadouts", |ctx| enforce_banned_loadouts(lua, ctx, ts));

    // Admin commands (every bfdb RPC) are not run here: they have their own
    // fast timer, see `start_admin_drain`.

    // Its subsystems are guarded one by one inside; this catches the rest.
    match step(lua, ctx, "slow timed events", |ctx| {
        run_slow_timed_events(lua, ctx, perf, path, ts)
    }) {
        Some(Ok(AdminResult::Continue)) | None => (),
        Some(Ok(AdminResult::Shutdown)) => return Ok(AdminResult::Shutdown),
        Some(Err(e)) => error!("error running slow timed events {:?}", e),
    }
    step(lua, ctx, "menu init", |ctx| menu::process_init_queue(ctx, lua));
    // Contested bases flash on the F10 map (once a second, see the pass).
    step(lua, ctx, "flash contested bases", |ctx| ctx.db.flash_contested_objectives(ts));
    // F10 wingman requests, answered within a second of the click.
    step(lua, ctx, "air life requests", |ctx| {
        airlife::process_requests(lua, ctx, perf, ts)
    });
    step(lua, ctx, "spawn queue", |ctx| {
        let now = Utc::now();
        match SpawnCtx::new(lua) {
            Err(e) => error!("could not build a spawn ctx {e:?}"),
            Ok(spctx) => {
                if let Err(e) = ctx.db.ephemeral.process_spawn_queue(
                    perf,
                    &ctx.db.persisted,
                    ts,
                    &ctx.idx,
                    &spctx,
                ) {
                    error!("error processing spawn queue {:?}", e)
                }
            }
        }
        record_perf(&mut perf.spawn_queue, now);
    });
    step(lua, ctx, "csar", |ctx| {
        if let Err(e) = ctx.db.tick_csar(lua) {
            error!("csar tick failed: {:?}", e)
        }
    });
    let now = Utc::now();
    let has_captures =
        step(lua, ctx, "advise captured", |ctx| match advise_captured(ctx, lua, ts) {
            Ok(captures) => captures,
            Err(e) => {
                error!("error advise captured {:?}", e);
                false
            }
        })
        .unwrap_or(false);
    record_perf(&mut perf.advise_captured, now);

    // Update frontline when objectives are captured
    if has_captures {
        step(lua, ctx, "frontline", |ctx| update_frontline(ctx, ts, true));
    }
    let now = Utc::now();
    step(lua, ctx, "advise capturable", |ctx| {
        if let Err(e) = advise_captureable(ctx) {
            error!("error advise capturable {:?}", e)
        }
    });
    record_perf(&mut perf.advise_capturable, now);
    let (now, perf_now) = (ctx.now(), Utc::now());
    step(lua, ctx, "jtac target positions", |ctx| {
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
    });
    record_perf(&mut perf.jtac_target_positions, perf_now);
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
        if TICKS.fetch_add(1, Ordering::Relaxed).is_multiple_of(60) && depth > 0 {
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
    step(lua, ctx, "logistics", |ctx| {
        if let Err(e) = ctx.db.logistics_step(lua, perf, ts) {
            error!("error running logistics events {e:?}")
        }
    });
    // `-hq` / `-request` from chat, which arrive in the hooks state.
    step(lua, ctx, "hq chat", |ctx| hq::run_chat(lua, ctx));
    step(lua, ctx, "action commands", |ctx| {
        if let Err(e) = run_action_commands(ctx, perf, lua) {
            error!("failed to run action commands {e:?}")
        }
    });
    step(lua, ctx, "jtac commands", |ctx| {
        if let Err(e) = run_jtac_commands(ctx, lua) {
            error!("failed to run jtac commands {e:?}")
        }
    });
    step(lua, ctx, "weather requests", |ctx| {
        for (id, slot) in std::mem::take(&mut ctx.weather_requests) {
            if let Err(e) = atis::send_full_weather(lua, ctx, slot) {
                error!("full weather report failed for {:?}: {:?}", id, e);
            }
        }
    });
    step(lua, ctx, "brief requests", |ctx| chatcmd::run_brief_requests(ctx, lua));
    ctx.load_state.step();
    record_perf(&mut perf.timed_events, perf_ts);
    ctx.log_perf(now);
    ctx.check_bg_health(now);
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
    let period = 1f64 / MSGQ_DRAIN_HZ as f64;
    timer.schedule_function(
        timer.get_time()? + period,
        mlua::Value::Nil,
        move |lua, _, now| {
            // Marks and messages can fire events (mark events with no
            // initiator); they are queued while this holds the state.
            let Some(mut ctx_guard) = Context::borrow_logged("msgq drain") else {
                return Ok(Some(now + period));
            };
            let ctx = &mut *ctx_guard;
            ctx.db.ephemeral.clock.advance(now.0);
            // Round up so a rate that does not divide evenly is never throttled
            // below its configured value.
            let per_pass =
                ctx.db.ephemeral.cfg.max_msgs_per_second.div_ceil(MSGQ_DRAIN_HZ).max(1);
            match (Net::singleton(lua), Trigger::singleton(lua).and_then(|t| t.action()))
            {
                (Ok(net), Ok(act)) => {
                    if let Err(e) = catch_unwind(AssertUnwindSafe(|| {
                        ctx.db.ephemeral.msgs().process(per_pass, &net, &act)
                    })) {
                        error!("msgq drain panicked: {}", bg::panic_msg(&e))
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
            drain_deferred_events(lua, ctx);
            Ok(Some(now + period))
        },
    )?;
    Ok(())
}

/// How often admin commands -- every bfdb RPC -- are run.
///
/// They used to run on the 1 s event tick, and that capped each bfdb at about
/// one RPC a second, whatever the queue limit said: the netidx publisher reads
/// nothing more from a connection while an RPC on it is still unanswered, and
/// bfdb's pollers share one connection per engine. So each call waited a tick
/// for every call ahead of it, and replies came back after 3, 4, 5, 6 s -- the
/// thousands of "engine slow" lines, and the breaker trips behind them. At
/// 10 Hz a call waits about 100 ms per call ahead. `ADMIN_COMMANDS_PER_TICK`
/// still bounds the work landing in any one DCS frame.
const ADMIN_DRAIN_HZ: f64 = 10.;

/// Run queued admin commands on their own fast timer.
fn start_admin_drain(lua: MizLua) -> Result<()> {
    let timer = Timer::singleton(lua)?;
    let period = 1. / ADMIN_DRAIN_HZ;
    timer.schedule_function(
        timer.get_time()? + period,
        mlua::Value::Nil,
        move |lua, _, now| {
            {
                let Some(mut ctx) = Context::borrow_logged("admin drain") else {
                    return Ok(Some(now + period));
                };
                if ctx.exit_attempts.is_some() {
                    // shutting down: the 1 s tick owns the exit retries
                    return Ok(None);
                }
                ctx.db.ephemeral.clock.advance(now.0);
                let res = step(lua, &mut ctx, "admin commands", |ctx| {
                    run_admin_commands(ctx, lua)
                });
                match res {
                    Some(Err(e)) => error!("failed to run admin commands {e:?}"),
                    Some(Ok(AdminResult::Continue)) | None => (),
                    Some(Ok(AdminResult::Shutdown)) => {
                        println!("initiating DCS shutdown");
                        ctx.exit_attempts = Some(0);
                        try_exit_dcs(lua, &mut ctx);
                        return Ok(None);
                    }
                }
            }
            // Last, with the state let go of: the handlers take it themselves.
            if let Err(e) =
                catch_unwind(AssertUnwindSafe(|| admin::run_menu_invokes(lua)))
            {
                error!("cockpit menu clicks panicked: {}", bg::panic_msg(&e))
            }
            Ok(Some(now + period))
        },
    )?;
    Ok(())
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
            let msg = bg::panic_payload_msg(info.payload());
            let loc = match info.location() {
                Some(l) => format!("{}:{}:{}", l.file(), l.line(), l.column()),
                None => std::string::String::from("<unknown location>"),
            };
            error!("PANIC at {loc}: {msg}\n{}", Backtrace::force_capture());
        }));
    });
}

/// One attempt at ending the DCS process after a shutdown. Returns true if
/// the timer should call again (the attempt failed).
///
/// Every step is best effort. These used to be `?`s: one failure (a missing
/// singleton, the event handler already gone) returned an error out of the
/// timer callback, which DCS takes as "unschedule" -- so the tick loop was
/// gone, the background thread already shut down, and `exitProcess` never
/// called: a server that looked up but saved nothing and never restarted.
fn try_exit_dcs(lua: MizLua, ctx: &mut Context) -> bool {
    const GIVE_UP_AFTER: u32 = 10;
    // The longest we wait for the background thread to finish writing the
    // save and flushing the logs before exiting regardless.
    const SHUTDOWN_WAIT: std::time::Duration = std::time::Duration::from_secs(60);
    // Nothing should touch the state once it has been written out.
    if let Some(id) = ctx.event_handler_id.take()
        && let Err(e) = remove_event_handler(lua, id)
    {
        error!("shutdown: could not remove the event handler {e:?}")
    }
    if let Some((wait, since)) = &ctx.shutdown_wait {
        let done = *wait.0.lock();
        if !done && since.elapsed() < SHUTDOWN_WAIT {
            // ask again on the next tick
            return true;
        }
        if !done {
            error!(
                "shutdown: the background thread did not finish in {SHUTDOWN_WAIT:?}, exiting anyway"
            );
        }
        ctx.shutdown_wait = None;
    }
    let attempt = ctx.exit_attempts.map(|n| n.saturating_add(1)).unwrap_or(1);
    ctx.exit_attempts = Some(attempt);
    match Net::singleton(lua).and_then(|net| {
        net.dostring_in(
            DcsLuaEnvironment::Server,
            "DCS.setUserCallbacks({}); DCS.exitProcess()".into(),
        )
    }) {
        Ok(_) => {
            println!("removing timer event");
            false
        }
        Err(e) if attempt < GIVE_UP_AFTER => {
            error!(
                "shutdown: DCS.exitProcess failed (attempt {attempt}), retrying {e:?}"
            );
            true
        }
        Err(e) => {
            // The state is saved (or deliberately reset) and the background
            // thread has been shut down; a process that won't exit is worse
            // than one that exits abruptly -- the bot restarts it either way.
            error!(
                "shutdown: DCS.exitProcess failed {attempt} times, exiting the process {e:?}"
            );
            bg::fallback_log(
                b"bflib: DCS.exitProcess kept failing, calling process::exit\n",
            );
            std::process::exit(0)
        }
    }
}

fn start_timed_events(ctx: &mut Context, lua: MizLua, path: PathBuf) -> Result<()> {
    ctx.last_slow_timed_events = ctx.now();
    start_msgq_drain(lua)?;
    start_admin_drain(lua)?;
    let timer = Timer::singleton(lua)?;
    timer.schedule_function(timer.get_time()? + 1., mlua::Value::Nil, {
        let path = path.clone();
        move |lua, _, now| {
            {
            let Some(mut ctx_guard) = Context::borrow_logged("1 s tick") else {
                return Ok(Some(now + 1.));
            };
            let ctx = &mut *ctx_guard;
            ctx.db.ephemeral.clock.advance(now.0);
            if ctx.exit_attempts.is_some() {
                // already shutting down and the last exit call failed; don't
                // run the campaign any further, just try to exit again
                return Ok(try_exit_dcs(lua, ctx).then_some(now + 1.));
            }
            let wall = Utc::now();
            if let Some((model_then, wall_then)) = ctx.last_tick_clocks.replace((now.0, wall))
                && let Some(gap) = rules::clock_gap_secs(model_then, now.0, wall_then, wall) {
                    warn!(
                        "the simulation stood still for about {gap} s while the wall clock ran \
                         (paused, loading or hung). Campaign timers run on mission time and \
                         stood still with it (see crate::clock)"
                    );
                }
            let res = catch_unwind(AssertUnwindSafe(|| run_timed_events(ctx, lua, &path)));
            if let Err(e) = catch_unwind(AssertUnwindSafe(|| drain_deferred_events(lua, ctx))) {
                error!("handling deferred events panicked: {}", bg::panic_msg(&e))
            }
            match res {
                Ok(Ok(AdminResult::Continue)) => (),
                Ok(Err(e)) => error!("failed to run timed events {:?}", e),
                Ok(Ok(AdminResult::Shutdown)) => {
                    println!("initiating DCS shutdown");
                    ctx.exit_attempts = Some(0);
                    return Ok(try_exit_dcs(lua, ctx).then_some(now + 1.));
                }
                // The hook has already logged the location and backtrace;
                // this says which tick died, so the two can be paired up.
                Err(e) => error!("run_timed_events panicked: {}", bg::panic_msg(&e)),
            }
            }
            // The tick has let go of the state: the mission's on_event hook
            // may now call back into the vector_strike API.
            crate::api::flush_events(lua);
            Ok(Some(now + 1.))
        }
    })?;
    Ok(())
}

/// Start the campaign clock (`crate::clock`) at the save's clock plus the
/// downtime, or the wall clock for a new campaign or an older save.
fn start_clock(lua: MizLua, ctx: &mut Context) -> Result<()> {
    let wall = Utc::now();
    let campaign = clock::SavedClock::resume(ctx.db.persisted.clock, wall);
    let model = Timer::singleton(lua)?.get_time()?.0;
    ctx.db.ephemeral.clock.start(campaign, model);
    let behind = wall - campaign;
    if behind > Duration::seconds(1) {
        info!(
            "campaign clock resumed {}s behind the wall clock (time the simulation stood still)",
            behind.num_seconds()
        );
    }
    Ok(())
}

fn delayed_init_miz(lua: MizLua, ctx: &mut Context) -> Result<()> {
    info!("init_miz: welcome to blue flag v3");
    if let Ok(wd) = Lfs::singleton(lua).and_then(|l| l.writedir()) {
        report_build(std::path::Path::new(wd.as_str()));
    }
    info!("indexing the miz");
    let miz = Miz::singleton(lua)?;
    ctx.idx = miz.index().context("indexing the mission")?;
    info!("adding event handlers");
    ctx.event_handler_id = Some(add_event_handler(lua).context("adding event handlers")?);
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
    // Fat Cow pads and layout are generated rather than placed in the
    // mission editor; they have to be in the mission before anything looks
    // templates up, so the index is rebuilt with them in it.
    if db::fat_cow::inject_templates(lua, &miz, &ctx.idx, &cfg)
        .context("generating fat cow templates")?
        > 0
    {
        ctx.idx = miz.index().context("re-indexing the mission")?;
    }
    // Computed once, up front, so both the netidx stats publisher (via
    // Task::CfgLoaded) and the NewRound stat below agree on whether this is
    // a genuinely new round or a resume of saved state -- sending a
    // NewRound signal unconditionally on every restart (crash recovery,
    // bot-triggered restart) made bfdb close and reopen the round even when
    // resuming, not just on real campaign resets.
    //
    // A missing save file is NOT, on its own, a new round. The save used to be
    // renamed away before its replacement was moved in, and a hard kill in
    // between left no live save -- and the next start wiped the campaign.
    // bg::save no longer has that window, but a save can still go missing
    // (an old build, a botched copy), and starting over is the one outcome
    // that can't be undone. So unless an admin reset left its marker, resume
    // from the newest readable backup instead (Db::load falls back to the
    // backups when the live file can't be opened), and say so loudly. If none
    // of them can be read the mission refuses to start rather than silently
    // beginning a new round.
    let live_save = path.exists();
    let recover = !live_save
        && {
            let backups = bg::backup_saves(&path);
            if backups.is_empty() {
                false
            } else {
                error!(
                    "THE SAVE FILE {path:?} IS MISSING, but {} backup save file(s) exist: resuming \
                 the campaign from the newest readable one instead of starting a new round. To \
                 really start over use the admin reset, or create an empty {:?}.",
                    backups.len(),
                    bg::reset_marker_path(&path)
                );
                true
            }
        };
    let fresh = !live_save && !recover;
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
        // Resume the campaign clock where the save left it (plus the
        // downtime), before anything below stamps a time.
        start_clock(lua, ctx)?;
        // Db::init runs this on a fresh campaign start, but a loaded save
        // never goes through init at all -- run it here too so an objective
        // that's missing its logistics-defense group (e.g. saved before a
        // mission version added a base, or before this check existed) gets
        // fixed on the next restart without requiring a full campaign reset.
        // Idempotent: it only adds a group where none exists at all, so it
        // can't touch objectives with real (even fully dead) combat damage.
        let spctx =
            SpawnCtx::new(lua).context("building spawn ctx for logi coverage check")?;
        ctx.db
            .ensure_default_logi_coverage(&spctx, &ctx.idx)
            .context("ensure_default_logi_coverage (load path) failed")?;
    }
    ctx.shutdown = ctx.db.ephemeral.cfg.shutdown.map(|hrs| {
        // a real-world schedule: wall clock
        let now = Utc::now();
        AutoShutdown::scheduled(now + Duration::hours(hrs as i64), now)
    });
    ctx.do_bg_task(Task::Stat(Stat::SessionStart {
        stop: ctx.shutdown.map(|a| a.when),
        cfg: Box::new((*ctx.db.ephemeral.cfg).clone()),
    }));
    if !ctx.db.ephemeral.clock.started() {
        start_clock(lua, ctx)?;
    }
    ctx.db_loaded = true;
    info!("spawning units");
    {
        // every group spawned here fires Birth from inside addGroup, queued
        // while we hold the state (see CtxGuard)
        let res = ctx.respawn_groups(lua, &miz);
        drain_deferred_events(lua, ctx);
        res.context("setting up the mission after load")?;
    }

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
                Err(e) => {
                    error!("failed to convert objective position for {}: {e:?}", obj.name)
                }
            }
        }
        info!("published {} objectives as stats", ctx.db.persisted.objectives.len());
    }

    // Initialize dynamic frontline system if frontline is enabled
    if let Some(frontline_cfg) = &ctx.db.ephemeral.cfg.frontline
        && frontline_cfg.enabled
    {
        info!("Initializing dynamic frontline system");
        let frontline = frontline::FrontLine::new(frontline_cfg.clone());
        ctx.frontline = Some(frontline);
        // Perform initial frontline calculation
        let now = ctx.now();
        update_frontline(ctx, now, true);
    }

    info!("starting timed events");
    start_timed_events(ctx, lua, path).context("starting the timed events loop")?;
    Ok(())
}

fn on_mission_load_end(lua: HooksLua) -> Result<()> {
    let mut ctx_guard = Context::borrow("onMissionLoadEnd")?;
    let ctx = &mut *ctx_guard;
    ctx.load_state = LoadState::MissionLoaded { time: Utc::now() };
    match Dcs::singleton(lua).and_then(|dcs| dcs.get_mission_filename()) {
        Ok(path) => ctx.mission_file_path = Some(PathBuf::from(path.as_str())),
        Err(e) => warn!("could not get mission filename {e:?}"),
    }
    if !unitdb::is_loaded()
        && let Err(e) = unitdb::init(lua)
    {
        warn!("could not harvest the unit db, using config ranges only: {e:?}");
    }
    // Terrain airdrome data (code, freqs, runway names) for the ATIS; only
    // this (hooks) lua state can require('terrain').
    if let Err(e) = atis::harvest_airdromes(lua) {
        warn!("could not harvest the terrain airdromes for the ATIS: {e:?}");
    }
    info!("mission loaded");
    Ok(())
}

fn player_disconnected(ctx: &mut Context, id: PlayerId) {
    if let Some(ifo) = ctx.connected.player_disconnected(id) {
        info!("deslotting disconnected player {}", ifo.ucid);
        ctx.db.player_disconnected(&ifo.ucid)
    }
}

fn on_player_disconnect(_: HooksLua, id: PlayerId) -> Result<()> {
    info!("onPlayerDisconnect({id})");
    let start_ts = Utc::now();
    match Context::try_borrow() {
        Some(mut ctx) => player_disconnected(&mut ctx, id),
        // a kick from inside one of our calls: the holder handles it
        None => DEFERRED.with(|q| q.borrow_mut().push(DeferredEvent::Disconnect(id))),
    }
    record_perf(&mut Arc::make_mut(&mut Perf::get_mut().inner).dcs_hooks, start_ts);
    Ok(())
}

fn on_simulation_frame(_: HooksLua) -> Result<()> {
    // Never inside one of our calls, but if it were, a frame's time is not
    // worth a second borrow.
    let Some(mut ctx) = Context::try_borrow() else { return Ok(()) };
    let mut perf_guard = Perf::get_mut();
    let frame = Arc::make_mut(&mut perf_guard.frame);
    let now = Utc::now();
    match &mut ctx.last_frame {
        Some(last) => {
            if let Some(ns) = (now - *last).num_nanoseconds()
                && (1..=1_000_000_000).contains(&ns)
            {
                **frame += ns as u64;
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
    // A failed scheduled function is run again a second later, and a
    // delayed_init_miz that panicked part way through must not run twice.
    let init_tried = std::cell::Cell::new(false);
    timer.schedule_function(when, mlua::Value::Nil, move |lua, _, now| {
        let Some(mut ctx_guard) = Context::borrow_logged("mission init") else {
            return Ok(Some(now + 1.));
        };
        let ctx = &mut *ctx_guard;
        if init_tried.get() {
            return Ok(None);
        }
        if ctx.load_state.init_ok() {
            init_tried.set(true);
            let res = delayed_init_miz(lua, ctx);
            drain_deferred_events(lua, ctx);
            if let Err(e) = res {
                error!("THE MISSION CANNOT START: {:?}", e);
                let timer = Timer::singleton(lua)?;
                timer.schedule_function(
                    now + 1.,
                    mlua::Value::Nil,
                    move |lua, _, now| {
                        let Some(mut ctx) = Context::borrow_logged("mission init error")
                        else {
                            return Ok(Some(now + 10.));
                        };
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
fn bflib(lua: &Lua) -> LuaResult<LuaTable> {
    // ensure we capture backtraces on panic
    unsafe {
        std::env::set_var("RUST_BACKTRACE", "1"); // bactrace for panics
        std::env::set_var("RUST_LIB_BACKTRACE", "0"); // no backtrace for Error
    };
    // Must come after the env vars and before anything can panic: the hook is
    // the only thing that sees a panic's location.
    install_panic_hook();
    Context::borrow("module load")
        .and_then(|mut ctx| ctx.init_async_bg(lua.inner()))
        .map_err(dcso3::lua_err)?;
    dcso3::create_root_module(lua, init_hooks, init_miz)
}

#[cfg(test)]
mod ctx_guard_tests {
    use super::*;

    // Each test runs on its own thread, and the state is per thread.

    #[test]
    fn one_borrow_at_a_time() {
        let g = Context::try_borrow().expect("nothing holds it yet");
        assert!(Context::try_borrow().is_none());
        drop(g);
        let mut g = Context::try_borrow().expect("let go");
        // the same state each time
        g.sortie = "s".into();
        drop(g);
        assert_eq!(Context::try_borrow().unwrap().sortie.as_str(), "s");
    }

    #[test]
    fn a_second_borrow_is_a_bug() {
        let _outer = Context::borrow("outer").unwrap();
        match catch_unwind(AssertUnwindSafe(|| Context::borrow("inner").is_err())) {
            // debug builds and tests panic ...
            Err(_) => assert!(cfg!(debug_assertions)),
            // ... release refuses
            Ok(refused) => assert!(refused && !cfg!(debug_assertions)),
        }
        assert!(Context::try_borrow().is_none(), "the outer borrow is still held");
    }

    #[test]
    fn a_panic_lets_go() {
        let r = catch_unwind(|| {
            let _g = Context::try_borrow().unwrap();
            panic!("handler died");
        });
        assert!(r.is_err());
        assert!(Context::try_borrow().is_some());
    }
}

#[cfg(test)]
mod slow_tick_tests {
    use super::*;

    #[test]
    fn every_phase_runs_once_in_order_however_the_pass_is_split() {
        for buckets in 1..=SLOW_PHASES.len() {
            let ran: Vec<usize> =
                (0..buckets).flat_map(|b| slow_bucket_phases(b, buckets)).collect();
            assert_eq!(
                ran,
                (0..SLOW_PHASES.len()).collect::<Vec<_>>(),
                "{buckets} buckets"
            );
            // no tick is left with nothing to do
            assert!((0..buckets).all(|b| !slow_bucket_phases(b, buckets).is_empty()));
        }
    }

    #[test]
    fn a_short_slow_period_spreads_less() {
        if std::env::var(SLOW_TICK_SPREAD_ENV).is_err() {
            assert_eq!(slow_tick_buckets(10), SLOW_PHASES.len());
            assert_eq!(slow_tick_buckets(3), 3);
            assert_eq!(slow_tick_buckets(0), 1);
        }
    }
}
