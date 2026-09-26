// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Vector Strike training range engine.
//!
//! A DCS World mission script DLL, loaded like bflib (hooks state +
//! mission trigger), that runs a practice server instead of a campaign:
//! air-to-ground ranges, a missile trainer and air-to-air set-ups, tankers,
//! carrier ops with LSO grading, anti-ship, helicopter drills and CAS drills.
//! Every graded event becomes a `RangeRecord` in `Logs/range.jsonl`, which
//! bfdb turns into the result cards on range.vectorstrike.org and Discord.
//!
//! No slot gating, no lives, no points: every slot is open to everyone.

mod aa;
mod aar;
mod ag;
mod antiship;
mod bg;
mod carrier;
mod catalog;
mod ground;
mod harvest;
mod helo;
mod menu;
mod players;
mod records;
mod sectors;
mod spawn;
mod util;
mod weapons;

use aa::AirToAir;
use aar::Aar;
use ag::{AirToGround, StrafeRun};
use antiship::AntiShip;
use anyhow::{anyhow, bail, Result};
use bfprotocols::{
    range::{
        cfg::RangeCfg, LiveArena, LivePlayer, LiveSpawn, RangeLive, SpawnReply, SpawnRequest,
        WindInfo,
    },
    stats::Stat,
};
use carrier::Carriers;
use chrono::Utc;
use crossbeam::queue::SegQueue;
use dcso3::{
    coalition::Side,
    env::miz::GroupId,
    event::Event,
    hooks::UserHooks,
    lfs::Lfs,
    net::{Net, PlayerId, Ucid},
    object::{DcsObject, DcsOid},
    timer::Timer,
    trigger::{SmokeColor, Trigger},
    unit::{ClassUnit, Unit, UnitCategory},
    world::{HandlerId, World},
    HooksLua, LuaEnv, LuaVec3, MizLua, String,
};
use fxhash::{FxHashMap, FxHashSet};
use ground::Ground;
use helo::Helo;
use log::{error, info, warn};
use mlua::prelude::*;
use netidx::publisher::Value;
use players::Players;
use records::Recorder;
use serde_json::json;
use spawn::Spawns;
use std::{collections::BTreeMap, path::PathBuf, str::FromStr, sync::Arc};
use util::V3;
use weapons::{Purpose, Shooter, Tracker};

pub const BUILD_GIT: &str = env!("BFNEXT_BUILD_GIT");

/// The player's current air-to-air menu selection.
#[derive(Debug, Clone)]
struct Selection {
    setup: std::string::String,
    adversary: std::string::String,
    weapons: std::string::String,
    skill: std::string::String,
    range_nm: f64,
    count: u32,
}

impl Default for Selection {
    fn default() -> Self {
        Self {
            setup: "offensive".into(),
            adversary: "MiG-29S".into(),
            weapons: "guns".into(),
            skill: "High".into(),
            range_nm: 30.,
            count: 1,
        }
    }
}

#[derive(Default)]
struct Ctx {
    started: bool,
    write_dir: PathBuf,
    cfg: Arc<RangeCfg>,
    sortie: std::string::String,
    cmds: bg::CmdQueue,
    players: Players,
    rec: Recorder,
    spawns: Spawns,
    tracker: Tracker,
    ag: AirToGround,
    aa: AirToAir,
    aar: Aar,
    cv: Carriers,
    helo: Helo,
    ship: AntiShip,
    ground: Ground,
    sectors: sectors::Sectors,
    handler: Option<HandlerId>,
    menus: FxHashSet<GroupId>,
    menu_queue: Vec<GroupId>,
    sel: FxHashMap<GroupId, Selection>,
    oids: FxHashMap<DcsOid<ClassUnit>, std::string::String>,
    arenas: Vec<(bfprotocols::range::cfg::ArenaCfg, V3)>,
    last_slow: f64,
    last_aar: f64,
    last_helo: f64,
    last_strafe: f64,
    last_cv_slow: f64,
    last_tanker: f64,
    last_ship: f64,
    live_json: Option<std::string::String>,
    catalog_json: Option<std::string::String>,
    /// disconnects seen in the hooks state, handled on the next mission tick
    disconnects: Vec<Ucid>,
}

impl Ctx {
    /// Single-threaded by construction: DCS runs both Lua states on one
    /// thread and never re-enters a callback, the same reasoning as bflib's
    /// `Context::get_mut`.
    unsafe fn get() -> &'static mut Ctx {
        static mut CTX: Option<Ctx> = None;
        #[allow(static_mut_refs)]
        unsafe {
            if CTX.is_none() {
                CTX = Some(Ctx::default());
            }
            CTX.as_mut().unwrap()
        }
    }
}

fn now_of(lua: MizLua) -> f64 {
    Timer::singleton(lua).and_then(|t| t.get_time()).map(|t| t.0 as f64).unwrap_or(0.)
}

fn abs_of(lua: MizLua) -> f64 {
    Timer::singleton(lua).and_then(|t| t.get_abs_time()).map(|t| t.0 as f64).unwrap_or(0.)
}

// ------------------------------------------------------------------ hooks

fn on_player_try_connect(_: HooksLua, addr: String, name: String, ucid: Ucid, id: PlayerId) -> Result<Option<String>> {
    let ctx = unsafe { Ctx::get() };
    ctx.players.connected(id, ucid, name.to_string(), addr.to_string());
    Ok(None)
}

fn on_player_disconnect(_: HooksLua, id: PlayerId) -> Result<()> {
    let ctx = unsafe { Ctx::get() };
    if let Some(ucid) = ctx.players.disconnected(id) {
        ctx.aa.cancel_duels(&ucid);
        ctx.disconnects.push(ucid);
    }
    Ok(())
}

fn on_player_try_send_chat(lua: HooksLua, id: PlayerId, msg: String, _all: bool) -> Result<Option<String>> {
    let m = msg.as_str().trim();
    if !m.starts_with("-range") {
        return Ok(None);
    }
    let ctx = unsafe { Ctx::get() };
    let reply = match m.split_whitespace().nth(1).unwrap_or("help") {
        "trainer" => {
            let on = !matches!(m.split_whitespace().nth(2), Some("off"));
            if let Some(p) = ctx.players.pilot_by_id(id).map(|p| p.ucid) {
                ctx.players.set_trainer(p, on);
            }
            format!("Missile trainer {}", if on { "ON: missiles that would kill you are removed" } else { "OFF: missiles are live" })
        }
        _ => "Range: use F10 > Range for everything. -range trainer on|off. Results, debriefs and spawning: https://range.vectorstrike.org".into(),
    };
    let net = Net::singleton(lua)?;
    net.send_chat_to(reply.as_str().into(), id, None)?;
    Ok(Some(String::from("")))
}

fn on_mission_load_end(lua: HooksLua) -> Result<()> {
    if harvest::get().is_none() {
        if let Err(e) = harvest::init(lua) {
            warn!("DCS database harvest failed: {e:?}")
        }
    }
    Ok(())
}

fn init_hooks(lua: HooksLua) -> Result<()> {
    info!("bfrange {BUILD_GIT}: setting user hooks");
    if let Err(e) = harvest::init(lua) {
        info!("DCS databases not ready at init ({e}); retrying at mission load end");
    }
    UserHooks::new(lua)
        .on_player_try_connect(on_player_try_connect)?
        .on_player_disconnect(on_player_disconnect)?
        .on_player_try_send_chat(on_player_try_send_chat)?
        .on_mission_load_end(on_mission_load_end)?
        .register()?;
    Ok(())
}

// ------------------------------------------------------------------ init

fn read_sortie(lua: MizLua) -> std::string::String {
    (|| -> Result<std::string::String> {
        let env: LuaTable = lua.inner().globals().raw_get("env")?;
        let m: LuaTable = env.raw_get("mission")?;
        let s: Option<std::string::String> = m.raw_get("sortie")?;
        // the ME stores the sortie name as a dictionary key; resolve it
        let s = s.unwrap_or_default();
        if s.starts_with("DictKey_") {
            let f: LuaFunction = env.raw_get("getValueDictByKey")?;
            return Ok(f.call::<_, std::string::String>(s)?);
        }
        Ok(s)
    })()
    .ok()
    .filter(|s| !s.trim().is_empty())
    .unwrap_or_else(|| "RANGE".into())
}

fn delayed_init(lua: MizLua) -> Result<()> {
    let ctx = unsafe { Ctx::get() };
    let sortie = read_sortie(lua);
    let cfg_path = ctx.write_dir.join(format!("{sortie}_RANGE"));
    let cfg = if cfg_path.exists() {
        RangeCfg::load(&cfg_path).map_err(|e| anyhow!("{cfg_path:?} does not parse: {e}"))?
    } else {
        warn!("no range config at {cfg_path:?}: writing an empty one (edit it and restart)");
        let c = RangeCfg::default();
        if let Err(e) = c.save(&cfg_path) {
            warn!("could not write {cfg_path:?}: {e:?}")
        }
        c
    };
    cfg.validate()?;
    info!(
        "range {:?} (sortie {sortie}): {} stations, {} tankers, {} carriers, {} pads, {} JTACs",
        cfg.name,
        cfg.stations.len(),
        cfg.tankers.len(),
        cfg.carriers.len(),
        cfg.helo.pads.len(),
        cfg.jtacs.len()
    );
    let cfg = Arc::new(cfg);
    ctx.cfg = cfg.clone();
    ctx.sortie = sortie.clone();
    ctx.rec = Recorder::new(sortie.clone(), util::MissionClock::read(lua), cfg.record_tracks);
    ctx.cmds = Arc::new(SegQueue::new());
    // one stats round per range, not per restart: the marker keeps pilots'
    // identity stats attached to the same round across restarts
    let marker = ctx.write_dir.join(format!("{sortie}_RANGE.round"));
    if !marker.exists() {
        bg::send(bg::Task::Stat(Stat::NewRound { sortie: sortie.as_str().into() }));
        let _ = std::fs::write(&marker, Utc::now().to_rfc3339());
    }
    let net = bg::NetCfg {
        base: cfg.netidx_base.clone(),
        config: cfg.netidx_config.clone(),
        bind: cfg.netidx_bind.clone(),
    };
    bg::send(bg::Task::Start { sortie: sortie.clone(), net, q: ctx.cmds.clone() });
    let now = now_of(lua);
    ctx.ag.init(lua, &cfg, &mut ctx.spawns, now);
    ctx.aar.init(lua, &cfg, &mut ctx.spawns, now);
    ctx.cv.init(lua, &cfg);
    ctx.helo.init(lua, &cfg);
    ctx.ground.init(lua, &cfg);
    ctx.sectors.init(lua, &cfg);
    for a in &cfg.air_to_air.arenas {
        match ag::resolve(lua, &a.loc) {
            Ok(p) => ctx.arenas.push((a.clone(), p)),
            Err(e) => warn!("arena {}: {e:?}", a.id),
        }
    }
    ctx.catalog_json = serde_json::to_string(&catalog::build(&cfg, &ctx.helo, &ctx.ground)).ok();
    let world = World::singleton(lua)?;
    ctx.handler = Some(world.add_event_handler(on_event)?);
    let timer = Timer::singleton(lua)?;
    timer.schedule_function(dcso3::Time((now + 0.5) as f32), mlua::Value::Nil, move |lua, _, t| {
        Ok(Some(dcso3::Time(fast_tick(lua, t.0 as f64) as f32)))
    })?;
    timer.schedule_function(dcso3::Time((now + 1.) as f32), mlua::Value::Nil, move |lua, _, t| {
        slow_tick(lua, t.0 as f64);
        Ok(Some(dcso3::Time(t.0 + 1.)))
    })?;
    ctx.started = true;
    info!("range engine running");
    Ok(())
}

fn init_miz(lua: MizLua) -> Result<()> {
    info!("bfrange {BUILD_GIT}: initializing mission");
    let timer = Timer::singleton(lua)?;
    let when = dcso3::Time(timer.get_time()?.0 + 1.);
    timer.schedule_function(when, mlua::Value::Nil, move |lua, _, _now| {
        if let Err(e) = delayed_init(lua) {
            error!("THE RANGE CANNOT START: {e:?}");
            let _ = Trigger::singleton(lua)
                .and_then(|t| t.action())
                .and_then(|a| a.out_text(format!("THE RANGE CANNOT START\n\n{e:?}").as_str().into(), 3600, true));
        }
        Ok(None)
    })?;
    Ok(())
}

#[mlua::lua_module]
fn bfrange(lua: &Lua) -> LuaResult<LuaTable<'_>> {
    unsafe {
        std::env::set_var("RUST_BACKTRACE", "1");
        std::env::set_var("RUST_LIB_BACKTRACE", "0");
    }
    let ctx = unsafe { Ctx::get() };
    if ctx.write_dir.as_os_str().is_empty() {
        let wd = Lfs::singleton(lua).and_then(|l| l.writedir()).map_err(dcso3::lua_err)?;
        ctx.write_dir = PathBuf::from(wd.as_str());
        bg::init(ctx.write_dir.clone());
    }
    dcso3::create_root_module(lua, init_hooks, init_miz)
}

// ------------------------------------------------------------------ helpers

fn ucid_for_name(lua: MizLua, ctx: &Ctx, name: &str) -> Option<Ucid> {
    if let Some(u) = ctx.players.ucid_by_name(name) {
        return Some(u);
    }
    let net = Net::singleton(lua).ok()?;
    for id in net.get_player_list().ok()?.into_iter().flatten() {
        if let Ok(info) = net.get_player_info(id) {
            if info.name().ok().map(|n| n.as_str() == name).unwrap_or(false) {
                return info.ucid().ok().flatten();
            }
        }
    }
    None
}

fn shooter_of(ctx: &Ctx, u: &Unit) -> Option<(Shooter, V3, V3)> {
    let name = u.get_name().ok()?.to_string();
    let pos = u.get_point().ok()?.0;
    let vel = u.get_velocity().ok()?.0;
    if let Some(f) = ctx.players.flying.get(&name) {
        return Some((
            Shooter {
                ucid: Some(f.ucid),
                name: f.name.clone(),
                unit_name: name,
                group_id: Some(f.group_id),
                typ: f.typ.clone(),
                side: f.side,
                callsign: f.group_name.clone(),
            },
            pos,
            vel,
        ));
    }
    let typ = u.get_type_name().ok()?.to_string();
    Some((
        Shooter {
            ucid: None,
            name: typ.clone(),
            unit_name: name,
            group_id: u.get_group().ok().and_then(|g| g.id().ok()),
            typ,
            side: u.get_coalition().unwrap_or(Side::Red),
            callsign: std::string::String::new(),
        },
        pos,
        vel,
    ))
}

fn gun_ammo(u: &Unit) -> u32 {
    let mut n = 0;
    if let Ok(seq) = u.get_ammo() {
        for a in seq.into_iter().flatten() {
            let cat = a
                .raw_get::<_, Option<LuaTable>>("desc")
                .ok()
                .flatten()
                .and_then(|d| d.raw_get::<_, Option<i64>>("category").ok().flatten());
            if cat == Some(0) {
                n += a.count().unwrap_or(0);
            }
        }
    }
    n
}

fn is_instructor(ctx: &Ctx, ucid: &str, dashboard_admin: bool) -> bool {
    dashboard_admin || ctx.cfg.spawn.instructors.iter().any(|i| i == ucid)
}

/// A player is out of their aircraft (death, slot change, disconnect).
fn player_out(lua: MizLua, ctx: &mut Ctx, unit: &str, now: f64) {
    let Some(f) = ctx.players.left(unit) else { return };
    ctx.aa.player_left(unit);
    let msg_s = ctx.cfg.message_s;
    ctx.aar.player_left(lua, &mut ctx.rec, unit, msg_s, now);
    ctx.cv.player_left(unit, now);
    ctx.helo.player_left(unit);
    ctx.ground.player_left(unit);
    if let Some(run) = ctx.ag.strafe.remove(unit) {
        let _ = run;
    }
    info!("{} left {} ({})", f.name, unit, f.typ);
}

// ------------------------------------------------------------------ events

fn on_event(lua: MizLua, ev: Event) -> Result<()> {
    let ctx = unsafe { Ctx::get() };
    if !ctx.started {
        return Ok(());
    }
    let now = now_of(lua);
    match ev {
        Event::Birth(b) => {
            let Ok(u) = b.initiator.as_unit() else {
                // DCS dynamic cargo arrives as a static object's birth
                if let Ok(n) = b.initiator.get_name() {
                    ctx.helo.static_born(lua, n.as_str(), now);
                }
                return Ok(());
            };
            let Ok(Some(pname)) = u.get_player_name() else { return Ok(()) };
            let pname = pname.to_string();
            let Some(ucid) = ucid_for_name(lua, ctx, &pname) else {
                warn!("birth of {pname}: no ucid known");
                return Ok(());
            };
            let unit_name = u.get_name()?.to_string();
            let g = u.get_group()?;
            let gid = g.id()?;
            let cat = u.get_category().unwrap_or(UnitCategory::Airplane);
            let pos = u.get_point()?.0;
            ctx.players.born(
                ucid,
                pname.clone(),
                unit_name.clone(),
                u.id()?,
                g.get_name()?.to_string(),
                gid,
                u.get_type_name()?.to_string(),
                u.get_coalition().unwrap_or(Side::Blue),
                cat == UnitCategory::Helicopter,
                matches!(cat, UnitCategory::GroundUnit | UnitCategory::Ship),
                pos,
                now,
            );
            if let Ok(oid) = u.object_id() {
                ctx.oids.insert(oid, unit_name);
            }
            if !ctx.menus.contains(&gid) && !ctx.menu_queue.contains(&gid) {
                ctx.menu_queue.push(gid);
            }
            let welcome = ctx.cfg.welcome.clone().unwrap_or_else(|| {
                format!(
                    "Welcome to {}, {pname}. F10 > Range for ranges, set-ups, tankers and drills. Missile trainer is ON. Debriefs: range.vectorstrike.org",
                    ctx.cfg.name
                )
            });
            records::to_group(lua, gid, &welcome, 15);
        }
        Event::PlayerLeaveUnit(l) => {
            if let Some(oid) = l.initiator {
                if let Some(name) = ctx.oids.remove(&oid) {
                    player_out(lua, ctx, &name, now);
                }
            }
        }
        Event::Dead(e) | Event::Crash(e) | Event::PilotDead(e) | Event::UnitLost(e) => {
            if let Some(o) = e.initiator {
                if let Ok(name) = o.get_name() {
                    let name = name.to_string();
                    ctx.aa.unit_dead(&name);
                    ctx.ag.object_dead(&name, now);
                    if ctx.players.flying.contains_key(&name) {
                        player_out(lua, ctx, &name, now);
                    }
                }
            }
        }
        Event::Ejection(e) => {
            if let Ok(name) = e.initiator.get_name() {
                let name = name.to_string();
                ctx.aa.unit_dead(&name);
                if ctx.players.flying.contains_key(&name) {
                    player_out(lua, ctx, &name, now);
                }
            }
        }
        Event::Shot(s) => {
            let Some((sh, pos, vel)) = shooter_of(ctx, &s.initiator) else { return Ok(()) };
            let desc = s.weapon.get_weapon_desc()?;
            let (purpose, class) = weapons::classify(&desc);
            ctx.ground.lane_shot(&sh.unit_name);
            match purpose {
                Purpose::AirToAir => {
                    let cfg = ctx.cfg.clone();
                    ctx.aa.on_shot(lua, &cfg.air_to_air.missile_trainer, &ctx.players, &s.weapon, &desc, sh, pos, now);
                }
                Purpose::AirToGround | Purpose::AntiShip if sh.is_player() => {
                    ctx.tracker.start(&s.weapon, desc, purpose, class, sh, pos, vel, now)?;
                }
                _ => (),
            }
        }
        Event::ShootingStart(e) => {
            let Some(o) = e.initiator else { return Ok(()) };
            let Ok(u) = o.as_unit() else { return Ok(()) };
            let name = u.get_name()?.to_string();
            ctx.ground.lane_shot(&name);
            let Some(f) = ctx.players.flying.get(&name).cloned() else { return Ok(()) };
            if let Some(run) = ctx.ag.strafe.get_mut(&name) {
                run.firing = true;
                return Ok(());
            }
            if let Some((si, pit, tpos)) = ctx.ag.in_pit(f.pos, f.vel, f.alt_agl) {
                let d = util::dist3(f.pos, tpos);
                ctx.ag.start_strafe(
                    &name,
                    StrafeRun {
                        station: si,
                        ucid: f.ucid.to_string(),
                        pilot: records::pilot_of(&f),
                        unit_type: f.typ.clone(),
                        side: f.side,
                        callsign: f.group_name.clone(),
                        group_id: Some(f.group_id),
                        gun: e.weapon_name.map(|w| w.to_string()).unwrap_or_else(|| "gun".into()),
                        ammo_start: gun_ammo(&u),
                        hits: 0,
                        firing: true,
                        fired: true,
                        foul: d < pit.foul_line_m,
                        min_range: d,
                        run_in_hdg: util::hdg(f.vel),
                        entry_agl: f.alt_agl,
                        started: now,
                    },
                );
            }
        }
        Event::ShootingEnd(e) => {
            if let Ok(name) = e.initiator.get_name() {
                if let Some(run) = ctx.ag.strafe.get_mut(name.as_str()) {
                    run.firing = false;
                }
            }
        }
        Event::Hit(h) => {
            let target = h.target.as_ref().and_then(|t| t.get_name().ok()).map(|s| s.to_string());
            let shooter = h.initiator.as_ref().and_then(|t| t.get_name().ok()).map(|s| s.to_string());
            match (&h.weapon, &target) {
                (Some(w), Some(t)) => {
                    if let Ok(oid) = w.object_id() {
                        ctx.tracker.record_hit(&oid, t.clone());
                        ctx.aa.record_hit(&oid);
                    }
                }
                (None, Some(t)) => {
                    if let Some(s) = &shooter {
                        ctx.ag.strafe_hit(s, t);
                        ctx.aa.gun_hit(s, t);
                    }
                }
                _ => (),
            }
            if let (Some(s), Some(t)) = (&shooter, &target) {
                if let Some(f) = ctx.players.flying.get(s).cloned() {
                    if f.is_ground {
                        ctx.ground.lane_event(&ctx.ag, &f, t, false, now);
                    }
                }
            }
        }
        Event::Kill(k) => {
            let target = k.target.as_ref().and_then(|t| t.get_name().ok()).map(|s| s.to_string());
            let shooter = k.initiator.as_ref().and_then(|t| t.get_name().ok()).map(|s| s.to_string());
            if let Some(t) = &target {
                ctx.ag.object_dead(t, now);
                if let Some(s) = &shooter {
                    if let Some(f) = ctx.players.flying.get(s).cloned() {
                        if f.is_ground {
                            ctx.ground.lane_event(&ctx.ag, &f, t, true, now);
                        }
                    }
                }
            }
        }
        Event::Takeoff(e) => {
            if let Ok(name) = e.initiator.get_name() {
                if let Some(f) = ctx.players.flying.get(name.as_str()) {
                    ctx.players.takeoff(f.ucid);
                }
            }
        }
        Event::Land(e) => {
            let Ok(name) = e.initiator.get_name() else { return Ok(()) };
            let name = name.to_string();
            let place = e.place.as_ref().and_then(|p| p.get_name().ok()).map(|s| s.to_string()).unwrap_or_default();
            if let Some(f) = ctx.players.flying.get(&name).cloned() {
                ctx.players.land(f.ucid);
                if carrier::carrier_by_place(&ctx.cv, &place) {
                    ctx.cv.touch(lua, &name, &place, true, now);
                }
                if f.is_helo {
                    let cfg = ctx.cfg.clone();
                    let mut f = f;
                    if let Ok(p) = e.initiator.get_point() {
                        f.pos = p.0;
                    }
                    ctx.helo.landed(lua, &cfg, &mut ctx.rec, &f, now);
                }
            }
        }
        Event::RunwayTouch(e) => {
            let Ok(name) = e.initiator.get_name() else { return Ok(()) };
            let place = e.place.as_ref().and_then(|p| p.get_name().ok()).map(|s| s.to_string()).unwrap_or_default();
            if carrier::carrier_by_place(&ctx.cv, &place) {
                ctx.cv.touch(lua, name.as_str(), &place, false, now);
            }
        }
        Event::RunwayTakeoff(e) => {
            let Ok(name) = e.initiator.get_name() else { return Ok(()) };
            let place = e.place.as_ref().and_then(|p| p.get_name().ok()).map(|s| s.to_string()).unwrap_or_default();
            if carrier::carrier_by_place(&ctx.cv, &place) {
                ctx.cv.runway_takeoff(name.as_str(), &place, now);
            }
        }
        Event::Refueling(r) => {
            if let Some(n) = r.initiator.and_then(|o| o.get_name().ok()) {
                ctx.aar.refuel_event(lua, n.as_str(), true);
            }
        }
        Event::RefuelingStop(r) => {
            if let Some(n) = r.initiator.and_then(|o| o.get_name().ok()) {
                ctx.aar.refuel_event(lua, n.as_str(), false);
            }
        }
        Event::LandingQualityMark(m) => {
            if let (Some(n), Some(c)) = (m.initiator.and_then(|o| o.get_name().ok()), m.comment) {
                ctx.cv.landing_quality_mark(n.as_str(), c.as_str(), now);
            }
        }
        _ => (),
    }
    Ok(())
}

// ------------------------------------------------------------------ ticks

fn fast_tick(lua: MizLua, now: f64) -> f64 {
    let ctx = unsafe { Ctx::get() };
    let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| fast_tick_inner(lua, ctx, now)));
    match r {
        Ok(Ok(next)) => next,
        Ok(Err(e)) => {
            error!("fast tick: {e:?}");
            now + 0.25
        }
        Err(_) => {
            error!("fast tick panicked");
            now + 1.
        }
    }
}

fn fast_tick_inner(lua: MizLua, ctx: &mut Ctx, now: f64) -> Result<f64> {
    let cfg = ctx.cfg.clone();
    // RPCs
    while let Some((cmd, tx)) = ctx.cmds.pop() {
        let v = handle_cmd(lua, ctx, cmd, now);
        let _ = tx.send(v);
    }
    // weapons in flight
    if ctx.tracker.active() {
        for imp in ctx.tracker.tick(lua, now) {
            let imp = match ctx.ship.score_impact(lua, &cfg, &ctx.ag, &mut ctx.rec, imp) {
                Some(i) => i,
                None => continue,
            };
            ctx.ground.impact(lua, &cfg, &ctx.ag, &mut ctx.rec, &imp, now);
            let _ = ctx.ag.score_impact(lua, &cfg, &mut ctx.rec, imp, now);
        }
    }
    // missile trainer
    let mut next = now + 0.25;
    if ctx.aa.active() {
        let kills = ctx.aa.tick(lua, &cfg.air_to_air.missile_trainer, &ctx.players, &ctx.spawns, &mut ctx.rec, cfg.message_s, now);
        for k in kills {
            ctx.aa.trainer_kill(lua, &k);
        }
        if let Some(d) = ctx.aa.next_due() {
            next = next.min(d.max(now + 0.02));
        }
    }
    // carrier passes at 10 Hz
    if ctx.cv.tick(lua, &cfg, &mut ctx.players, &ctx.rec.clock.clone(), &mut ctx.rec, now) {
        next = next.min(now + 0.1);
    }
    // AAR at 5 Hz
    if now - ctx.last_aar >= 0.2 {
        ctx.last_aar = now;
        ctx.aar.tick(lua, &mut ctx.players, &mut ctx.rec, cfg.message_s, now);
        if ctx.aar.active() {
            next = next.min(now + 0.2);
        }
    }
    // helo pads and cargo at 5 Hz
    if now - ctx.last_helo >= 0.2 {
        ctx.last_helo = now;
        ctx.helo.tick(lua, &cfg, &ctx.players, &mut ctx.spawns, &mut ctx.rec, now);
        if ctx.helo.active() {
            next = next.min(now + 0.2);
        }
    }
    // strafe passes at 4 Hz
    if now - ctx.last_strafe >= 0.25 && !ctx.ag.strafe.is_empty() {
        ctx.last_strafe = now;
        strafe_tick(lua, ctx, now);
        next = next.min(now + 0.25);
    }
    if ctx.tracker.active() {
        next = next.min(now + 0.05);
    }
    Ok(next.max(now + 0.02))
}

fn strafe_tick(lua: MizLua, ctx: &mut Ctx, now: f64) {
    let cfg = ctx.cfg.clone();
    let units: Vec<std::string::String> = ctx.ag.strafe.keys().cloned().collect();
    for unit in units {
        let Ok(u) = Unit::get_by_name(lua, &unit) else {
            ctx.ag.strafe.remove(&unit);
            continue;
        };
        let (Ok(p), Ok(v)) = (u.get_point(), u.get_velocity()) else { continue };
        let (p, v) = (p.0, v.0);
        let Some(run) = ctx.ag.strafe.get(&unit).cloned() else { continue };
        let st = &ctx.ag.stations[run.station];
        let pit = st.cfg.strafe.clone().unwrap_or_default();
        let Some(t) = st.nearest_target(p) else { continue };
        let tpos = t.pos;
        let d = util::dist3(p, tpos);
        if let Some(r) = ctx.ag.strafe.get_mut(&unit) {
            if r.firing {
                r.min_range = r.min_range.min(d);
                if d < pit.foul_line_m {
                    r.foul = true;
                }
            }
        }
        let receding = v.dot(&(tpos - p)) < 0.;
        let done = !run.firing && (d > pit.box_length_m || (receding && d > 300.) || now - run.started > 120.);
        if done {
            let ammo = gun_ammo(&u);
            ctx.ag.finish_strafe(lua, &cfg, &mut ctx.rec, &unit, ammo, tpos);
        }
    }
}

fn slow_tick(lua: MizLua, now: f64) {
    let ctx = unsafe { Ctx::get() };
    if let Err(e) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| slow_tick_inner(lua, ctx, now)))
        .map_err(|_| anyhow!("panicked"))
        .and_then(|r| r)
    {
        error!("slow tick: {e:?}");
    }
}

fn slow_tick_inner(lua: MizLua, ctx: &mut Ctx, now: f64) -> Result<()> {
    let cfg = ctx.cfg.clone();
    // disconnects queued from the hooks state
    let gone: Vec<Ucid> = std::mem::take(&mut ctx.disconnects);
    for ucid in gone {
        if let Some(f) = ctx.players.by_ucid(&ucid).cloned() {
            player_out(lua, ctx, &f.unit_name, now);
        }
        if cfg.spawn.despawn_on_leave {
            despawn_owned(lua, ctx, &ucid.to_string());
        }
    }
    // player positions
    let names: Vec<std::string::String> = ctx.players.flying.keys().cloned().collect();
    for n in names {
        match Unit::get_by_name(lua, &n) {
            Ok(u) => {
                if let (Ok(pos), Ok(v)) = (u.get_position(), u.get_velocity()) {
                    let p = pos.p.0;
                    let agl = p.y - util::ground_height(lua, p);
                    let in_air = u.in_air().unwrap_or(false);
                    if let Some(f) = ctx.players.flying.get_mut(&n) {
                        f.pos = p;
                        f.vel = v.0;
                        f.hdg = util::hdg(pos.x.0);
                        f.alt_agl = agl;
                        f.in_air = in_air;
                        f.updated = now;
                    }
                }
            }
            Err(_) => player_out(lua, ctx, &n, now),
        }
    }
    ctx.sectors.tick(lua, &ctx.players, now);
    // menus, one group per tick
    if let Some(gid) = ctx.menu_queue.pop() {
        let d = menu_data(ctx, ctx.players.by_group(gid).map(|f| f.is_helo).unwrap_or(false));
        match menu::build(lua, gid, &d) {
            Ok(()) => {
                ctx.menus.insert(gid);
            }
            Err(e) => warn!("F10 menu for group {gid:?}: {e:?}"),
        }
    }
    ctx.ag.slow_tick(lua, &mut ctx.spawns, now);
    ctx.ag.flush_salvos(lua, &cfg, &mut ctx.rec, now);
    ctx.aa.slow_tick(lua, &cfg, &mut ctx.spawns, &mut ctx.rec, now);
    if now - ctx.last_tanker >= 5. {
        ctx.last_tanker = now;
        ctx.aar.slow_tick(lua, &mut ctx.spawns, now);
    }
    if now - ctx.last_cv_slow >= 30. {
        ctx.last_cv_slow = now;
        ctx.cv.slow_tick(lua, &mut ctx.spawns, &mut ctx.aar, now);
    }
    if now - ctx.last_ship >= 5. {
        ctx.last_ship = now;
        ctx.ship.slow_tick(lua, &ctx.ag);
    }
    ctx.ground.slow_tick(lua, &ctx.ag, &mut ctx.rec, now);
    ctx.tracker.reap(now);
    for s in ctx.spawns.tick(lua, now) {
        forget_spawn(ctx, &s);
    }
    for id in ctx.spawns.dead(lua) {
        if let Some(s) = ctx.spawns.active.remove(&id) {
            forget_spawn(ctx, &s);
        }
    }
    // live picture for the website, refreshed every second
    ctx.live_json = serde_json::to_string(&live(lua, ctx, now)).ok();
    ctx.last_slow = now;
    Ok(())
}

fn forget_spawn(ctx: &mut Ctx, s: &spawn::Spawn) {
    match &s.kind {
        spawn::SpawnKind::Adversary { engagement } => ctx.aa.abort(*engagement),
        spawn::SpawnKind::Tanker => ctx.aar.remove_spawned(&s.id),
        spawn::SpawnKind::Ship => {
            for g in &s.groups {
                ctx.ship.forget(g)
            }
        }
        _ => (),
    }
}

fn despawn_owned(lua: MizLua, ctx: &mut Ctx, ucid: &str) -> usize {
    let gone = ctx.spawns.remove_owned(lua, ucid);
    for s in &gone {
        forget_spawn(ctx, s);
    }
    gone.len()
}

fn menu_data(ctx: &Ctx, is_helo: bool) -> menu::MenuData {
    let advs = if ctx.cfg.air_to_air.adversaries.is_empty() {
        aa::default_adversaries()
    } else {
        ctx.cfg.air_to_air.adversaries.clone()
    };
    let pair = |a: &str, b: &str| (a.to_string(), b.to_string());
    menu::MenuData {
        stations: ctx.ag.stations.iter().map(|s| (s.cfg.id.clone(), s.cfg.name.clone())).collect(),
        adversaries: advs.iter().map(|a| (a.typ.clone(), a.label.clone())).collect(),
        tanker_types: bfprotocols::range::cfg::TANKER_TYPES.iter().map(|t| pair(t.typ, t.typ)).collect(),
        sling: ctx.helo.sling_courses(),
        troops: ctx.helo.troop_lzs(),
        jtacs: ctx.ground.jtac_list(),
        ships: antiship::TARGET_SHIPS.iter().map(|(a, b)| pair(a, b)).collect(),
        compositions: catalog::GROUND_COMPOSITIONS.iter().map(|(k, l, _)| pair(k, l)).collect(),
        sams: catalog::SAMS.iter().map(|(k, l, _)| pair(k, l)).collect(),
        is_helo,
    }
}

// ------------------------------------------------------------------ live

fn live(lua: MizLua, ctx: &Ctx, now: f64) -> RangeLive {
    let abs = abs_of(lua);
    let (date, time) = ctx.rec.clock.stamp(abs);
    let reference = ctx
        .cv
        .carriers
        .first()
        .map(|c| c.op_center)
        .or_else(|| ctx.ag.stations.first().map(|s| s.center))
        .unwrap_or_else(V3::zeros);
    let g = util::geo(lua, reference);
    let gh = util::ground_height(lua, reference);
    let (sf, sk, _) = util::wind_at(lua, V3::new(reference.x, gh + 10., reference.z));
    let (af, ak, _) = util::wind_at(lua, V3::new(reference.x, 2000., reference.z));
    let (temp, qnh) = dcso3::atmosphere::Atmosphere::singleton(lua)
        .and_then(|a| a.get_temperature_and_pressure(LuaVec3(V3::new(reference.x, gh + 2., reference.z))))
        .map(|t| (t.temperature_k - 273.15, t.pressure_pa / 100.))
        .unwrap_or((15., 1013.));
    let players = ctx
        .players
        .flying
        .values()
        .map(|f| LivePlayer {
            ucid: f.ucid.to_string(),
            name: f.name.clone(),
            unit_type: f.typ.clone(),
            callsign: f.group_name.clone(),
            side: records::side_str(f.side).into(),
            pos: util::geo(lua, f.pos),
            heading_deg: f.hdg,
            speed_kts: f.vel.norm() * util::MS_TO_KTS,
            in_air: f.in_air,
            activity: f.activity.clone(),
        })
        .collect();
    let spawns = ctx
        .spawns
        .active
        .values()
        .map(|s| LiveSpawn {
            id: s.id.clone(),
            item: s.item.clone(),
            label: s.label.clone(),
            owner_name: s.owner_name.clone(),
            owner_ucid: s.owner.clone(),
            pos: util::geo(lua, s.pos),
            created: s.created,
            expires: s.expires,
            units: s.units,
        })
        .collect();
    let duels = ctx.aa.duels();
    let arenas = ctx
        .arenas
        .iter()
        .map(|(a, p)| {
            let occ: Vec<std::string::String> = ctx
                .players
                .flying
                .values()
                .filter(|f| util::dist2(f.pos, *p) < a.radius_nm * util::NM)
                .map(|f| f.name.clone())
                .collect();
            let status = duels
                .iter()
                .find(|(x, y)| occ.contains(x) || occ.contains(y))
                .map(|(x, y)| format!("duel: {x} vs {y}"))
                .unwrap_or_else(|| "open".into());
            LiveArena {
                id: a.id.clone(),
                name: a.name.clone(),
                pos: util::geo(lua, *p),
                radius_m: a.radius_nm * util::NM,
                occupants: occ,
                status,
            }
        })
        .collect();
    RangeLive {
        server_time: Utc::now(),
        theatre: ctx.rec.clock.theatre.clone(),
        mission_time: time,
        mission_date: date,
        night: ctx.rec.clock.is_night(g.lat, g.lon, abs),
        wind: WindInfo {
            layers: util::atmo_profile(lua, reference.x, reference.z, gh + 10., 10000., 1000.),
            surface_from_deg: sf,
            surface_kts: sk,
            alt_from_deg: af,
            alt_kts: ak,
            temperature_c: temp,
            qnh_hpa: qnh,
        },
        players,
        stations: ctx.ag.live(lua),
        tankers: ctx.aar.live(lua),
        carriers: ctx.cv.live(lua, &ctx.rec.clock, abs, &ctx.aar, now),
        spawns,
        arenas,
        sectors: ctx.cfg.sectors.clone(),
        uptime_s: now,
    }
}

// ------------------------------------------------------------------ commands

fn reply_json<T: serde::Serialize>(t: &T) -> Value {
    match serde_json::to_string(t) {
        Ok(s) => Value::String(s.into()),
        Err(e) => Value::Error(format!("encode: {e}").into()),
    }
}

fn handle_cmd(lua: MizLua, ctx: &mut Ctx, cmd: bg::Cmd, now: f64) -> Value {
    match cmd {
        bg::Cmd::QueryRange => match &ctx.live_json {
            Some(s) => Value::String(s.clone().into()),
            None => Value::Error("the range is starting".into()),
        },
        bg::Cmd::QueryCatalog => match &ctx.catalog_json {
            Some(s) => Value::String(s.clone().into()),
            None => Value::Error("the range is starting".into()),
        },
        bg::Cmd::QueryWeapons => match harvest::get() {
            Some(h) => reply_json(&h.weapons),
            None => Value::Error("the DCS weapon database has not been read yet".into()),
        },
        bg::Cmd::Spawn(req) => reply_json(&spawn_request(lua, ctx, req, now)),
        bg::Cmd::Despawn { ucid, spawn_id, instructor } => {
            let r = despawn_request(lua, ctx, &ucid, &spawn_id, instructor);
            reply_json(&json!({ "ok": r.is_ok(), "message": match r { Ok(m) => m, Err(e) => e.to_string() } }))
        }
        bg::Cmd::ResetStation { ucid, station, instructor } => {
            let r = (|| -> Result<std::string::String> {
                let i = ctx.ag.station_by_id(&station).ok_or_else(|| anyhow!("no station {station}"))?;
                if ctx.ag.stations[i].cfg.locked && !is_instructor(ctx, &ucid, instructor) {
                    bail!("that station can only be reset by an instructor")
                }
                ctx.ag.reset(lua, i, &mut ctx.spawns, now)?;
                Ok(format!("{} reset", ctx.ag.stations[i].cfg.name))
            })();
            reply_json(&json!({ "ok": r.is_ok(), "message": match r { Ok(m) => m, Err(e) => e.to_string() } }))
        }
    }
}

fn despawn_request(lua: MizLua, ctx: &mut Ctx, ucid: &str, spawn_id: &str, instructor: bool) -> Result<std::string::String> {
    if spawn_id == "all" {
        let n = despawn_owned(lua, ctx, ucid);
        return Ok(format!("{n} spawn(s) removed"));
    }
    let s = ctx.spawns.active.get(spawn_id).ok_or_else(|| anyhow!("no spawn {spawn_id}"))?;
    if s.owner.as_deref() != Some(ucid) && !is_instructor(ctx, ucid, instructor) {
        bail!("that spawn belongs to {}", s.owner_name)
    }
    if let Some(s) = ctx.spawns.remove(lua, spawn_id) {
        forget_spawn(ctx, &s);
        return Ok(format!("{} removed", s.label));
    }
    bail!("no spawn {spawn_id}")
}

fn param<'a>(p: &'a BTreeMap<std::string::String, std::string::String>, k: &str, d: &'a str) -> &'a str {
    p.get(k).map(|s| s.as_str()).unwrap_or(d)
}

fn param_f(p: &BTreeMap<std::string::String, std::string::String>, k: &str, d: f64) -> f64 {
    p.get(k).and_then(|s| s.parse::<f64>().ok()).unwrap_or(d)
}

fn spawn_request(lua: MizLua, ctx: &mut Ctx, req: SpawnRequest, now: f64) -> SpawnReply {
    match spawn_inner(lua, ctx, &req, now) {
        Ok((msg, id)) => SpawnReply { ok: true, message: msg, spawn_id: id },
        Err(e) => SpawnReply { ok: false, message: e.to_string(), spawn_id: None },
    }
}

fn spawn_inner(
    lua: MizLua,
    ctx: &mut Ctx,
    req: &SpawnRequest,
    now: f64,
) -> Result<(std::string::String, Option<std::string::String>)> {
    let cfg = ctx.cfg.clone();
    let ucid = Ucid::from_str(&req.ucid).map_err(|_| anyhow!("bad ucid"))?;
    let f = ctx
        .players
        .by_ucid(&ucid)
        .cloned()
        .ok_or_else(|| anyhow!("you need to be in an aircraft on the range server to spawn"))?;
    let instructor = is_instructor(ctx, &req.ucid, req.instructor);
    if cfg.spawn.instructor_only.iter().any(|i| i == &req.item) && !instructor {
        bail!("{} is instructor-only", req.item)
    }
    let is_drill = matches!(req.item.as_str(), "cas_drill");
    if !is_drill && !instructor {
        if ctx.spawns.owned_by(&req.ucid) as u32 >= cfg.spawn.max_active_per_player {
            bail!(
                "you already have {} spawns (max {}); despawn one first",
                ctx.spawns.owned_by(&req.ucid),
                cfg.spawn.max_active_per_player
            )
        }
        if ctx.spawns.ai_units() >= cfg.spawn.max_ai_units {
            bail!("the server is at its AI limit ({}); try again later", cfg.spawn.max_ai_units)
        }
        ctx.spawns.cooldown_ok(&req.ucid, cfg.spawn.cooldown_s)?;
    }
    let p = &req.params;
    let id = match req.item.as_str() {
        "bfm" | "bvr" | "missile_drill" => {
            let kind = if req.item == "missile_drill" { "bvr" } else { req.item.as_str() };
            let setup = param(p, if kind == "bfm" { "setup" } else { "aspect" }, if kind == "bfm" { "offensive" } else { "hot" });
            let count = if req.item == "missile_drill" { 1 } else { param_f(p, "count", 1.) as u32 };
            let skill = param(p, "skill", if req.item == "missile_drill" { "Excellent" } else { "High" });
            let weapons = param(p, "weapons", if kind == "bfm" { "guns" } else { "fox3" });
            let adv = param(p, "adversary", "MiG-29S");
            ctx.aa.spawn_setup(lua, &cfg, &mut ctx.spawns, &f, kind, setup, adv, skill, weapons, count, param_f(p, "range_nm", 30.), now)?
        }
        "tanker" => ctx.aar.spawn_for(
            lua,
            &cfg,
            &mut ctx.spawns,
            &f,
            param(p, "type", "KC-135"),
            param_f(p, "alt_ft", 20000.),
            param_f(p, "leg_nm", 20.),
            now,
        )?,
        "ship_target" | "naval_group" => ctx.ship.spawn_for(
            lua,
            &cfg,
            &mut ctx.spawns,
            &f,
            param(p, "type", antiship::TARGET_SHIPS[0].0),
            param_f(p, "count", 1.) as u32,
            param_f(p, "dist_nm", 30.),
            param(p, "moving", "yes") == "yes",
            param(p, "weapons_free", "no") == "yes",
            now,
        )?,
        "ground_targets" | "sam_site" => spawn_ground(lua, ctx, &f, &req.item, p, now)?,
        "sling_course" => ctx.helo.start_sling(lua, &cfg, &mut ctx.spawns, &f, param(p, "course", ""), now)?,
        "cas_drill" => {
            ctx.ground.require_jtac()?;
            let nine = ctx.ground.request_cas(lua, &mut ctx.spawns, &ctx.ag, &f, param(p, "jtac", ""), now)?;
            records::to_group(lua, f.group_id, &nine, 90);
            return Ok(("nine-line sent to your cockpit".into(), None));
        }
        other => bail!("unknown catalog item {other}"),
    };
    Ok((format!("{} spawned", ctx.spawns.active.get(&id).map(|s| s.label.clone()).unwrap_or(id.clone())), Some(id)))
}

fn spawn_ground(
    lua: MizLua,
    ctx: &mut Ctx,
    f: &players::Flying,
    item: &str,
    p: &BTreeMap<std::string::String, std::string::String>,
    now: f64,
) -> Result<std::string::String> {
    let cfg = ctx.cfg.clone();
    let hdg = if f.vel.norm() > 5. { util::hdg(f.vel) } else { f.hdg };
    let dist = param_f(p, "dist_nm", if item == "sam_site" { 20. } else { 10. });
    let mut pos = util::offset(f.pos, hdg, dist * util::NM, 0.);
    pos.y = util::ground_height(lua, pos);
    if util::is_water(lua, pos) {
        bail!("{dist:.0} nm ahead of you is water - point at land and try again")
    }
    let (types, label, weapons_free): (Vec<&str>, std::string::String, bool) = if item == "sam_site" {
        let k = param(p, "type", "sa8");
        let (_, l, t) = catalog::SAMS.iter().find(|(x, ..)| *x == k).ok_or_else(|| anyhow!("unknown SAM {k}"))?;
        (t.to_vec(), l.to_string(), param(p, "weapons_free", "no") == "yes")
    } else {
        let k = param(p, "composition", "armor");
        let (_, l, t) = catalog::GROUND_COMPOSITIONS.iter().find(|(x, ..)| *x == k).ok_or_else(|| anyhow!("unknown composition {k}"))?;
        (t.to_vec(), l.to_string(), k == "shilka")
    };
    let name = ctx.spawns.next_name(if item == "sam_site" { "SAM" } else { "GND" });
    let units: Vec<(std::string::String, V3, f64)> = types
        .iter()
        .enumerate()
        .map(|(i, t)| {
            let ring = if item == "sam_site" { 60. } else { 25. };
            let a = i as f64 * 360. / types.len().max(1) as f64;
            (t.to_string(), util::offset(pos, a, if i == 0 { 0. } else { ring }, 0.), hdg + 180.)
        })
        .collect();
    let moving = param(p, "moving", "no") == "yes";
    let route = if moving {
        let p2 = util::offset(pos, hdg + 90., 5000., 0.);
        vec![
            spawn::ground_waypoint(pos, 8., true, vec![]),
            spawn::ground_waypoint(p2, 8., true, vec![spawn::wrapped(1, json!({ "id": "SwitchWaypoint", "params": { "fromWaypointIndex": 2, "goToWaypointIndex": 1 } }))]),
        ]
    } else {
        vec![spawn::ground_waypoint(pos, 0., false, vec![])]
    };
    let g = spawn::surface_group(&name, &units, "High", route);
    let side = f.side.opposite();
    spawn::add_group(lua, spawn::country_id(spawn::default_country(side))?, spawn::GROUND, &g)?;
    // ROE: 2 open fire / 4 weapon hold; alarm state red keeps radars up
    ctx.spawns.defer(now + 2., spawn::Pending::GroupOption { group: name.clone(), id: 0, value: json!(if weapons_free { 2 } else { 4 }) });
    ctx.spawns.defer(now + 2., spawn::Pending::GroupOption { group: name.clone(), id: 9, value: json!(2) });
    ctx.spawns.insert(spawn::Spawn {
        id: name.clone(),
        item: item.into(),
        label: format!("{label}{}", if weapons_free { " (weapons free)" } else { "" }),
        owner: Some(f.ucid.to_string()),
        owner_name: f.name.clone(),
        groups: vec![name.clone()],
        statics: vec![],
        created: Utc::now(),
        expires: Some(Utc::now() + chrono::Duration::seconds(cfg.spawn.despawn_after_s as i64)),
        units: units.len() as u32,
        pos,
        kind: if item == "sam_site" { spawn::SpawnKind::Sam } else { spawn::SpawnKind::Targets },
    });
    records::to_group(lua, f.group_id, &format!("{label} spawned {:03.0} for {dist:.0} nm", hdg), 15);
    Ok(name)
}

// ------------------------------------------------------------------ F10

pub(crate) fn on_menu(lua: MizLua, gid: GroupId, action: &str) -> Result<()> {
    let ctx = unsafe { Ctx::get() };
    let now = now_of(lua);
    let Some(f) = ctx.players.by_group(gid).cloned() else {
        records::to_group(lua, gid, "Range: slot in first.", 5);
        return Ok(());
    };
    let say = |t: &str| records::to_group(lua, gid, t, 20);
    let mut parts = action.splitn(3, ':');
    let a0 = parts.next().unwrap_or("");
    let a1 = parts.next().unwrap_or("");
    let a2 = parts.next().unwrap_or("");
    let res: Result<()> = (|| {
        match a0 {
            "sectors" => {
                let mut t = vec![format!("{} sectors (nearest first):", ctx.cfg.name)];
                t.extend(ctx.sectors.describe(f.pos, f.side));
                records::to_group(lua, gid, &t.join("\n"), 40);
            }
            "status" => {
                let mut t = vec![format!("{} - {} players flying", ctx.cfg.name, ctx.players.flying.len())];
                if let Some(i) = ctx.sectors.at(f.pos) {
                    t.push(format!("You are in {}", ctx.sectors.list[i].title()));
                }
                t.push("Stations:".into());
                t.extend(ctx.ag.describe(lua, Some(f.pos)).into_iter().map(|s| format!("  {s}")));
                t.push("Tankers:".into());
                t.extend(ctx.aar.describe().into_iter().map(|s| format!("  {s}")));
                t.extend(ctx.cv.describe(lua));
                say(&t.join("\n"));
            }
            "ag" => {
                let i = ctx.ag.station_by_id(a2).ok_or_else(|| anyhow!("no station {a2}"))?;
                let st = &ctx.ag.stations[i];
                match a1 {
                    "info" => say(&format!(
                        "{}: {:03.0} for {:.1} nm, elevation {:.0} ft. {} of {} targets up.{}{}",
                        st.cfg.name,
                        util::bearing(f.pos, st.center),
                        util::dist2(f.pos, st.center) / util::NM,
                        st.center.y * util::M_TO_FT,
                        st.alive(),
                        st.targets.len(),
                        st.cfg.laser_code.map(|c| format!(" Laser code {c}.")).unwrap_or_default(),
                        st.cfg.note.as_ref().map(|n| format!(" {n}")).unwrap_or_default()
                    )),
                    "smoke" => {
                        let p = st.targets.iter().find(|t| t.alive).map(|t| t.pos).unwrap_or(st.center);
                        Trigger::singleton(lua)?.action()?.smoke(LuaVec3(p), SmokeColor::Red)?;
                        say(&format!("{}: red smoke on the target", st.cfg.name));
                    }
                    "reset" => {
                        if st.cfg.locked && !is_instructor(ctx, &f.ucid.to_string(), false) {
                            bail!("only an instructor can reset {}", st.cfg.name)
                        }
                        ctx.ag.reset(lua, i, &mut ctx.spawns, now)?;
                        say("Targets respawned");
                    }
                    _ => (),
                }
            }
            "spawn" => {
                let item = a1.to_string();
                let params: BTreeMap<std::string::String, std::string::String> = a2
                    .split('&')
                    .filter_map(|kv| kv.split_once('=').map(|(k, v)| (k.to_string(), v.to_string())))
                    .collect();
                let r = spawn_request(lua, ctx, SpawnRequest { ucid: f.ucid.to_string(), item, params, instructor: false }, now);
                if !r.ok {
                    say(&format!("Unable: {}", r.message));
                }
            }
            "sel" => {
                let s = ctx.sel.entry(gid).or_default();
                match a1 {
                    "setup" => s.setup = a2.into(),
                    "adv" => s.adversary = a2.into(),
                    "weapons" => s.weapons = a2.into(),
                    "skill" => s.skill = a2.into(),
                    "range" => s.range_nm = a2.parse().unwrap_or(30.),
                    "count" => s.count = a2.parse().unwrap_or(1),
                    _ => (),
                }
                let s = s.clone();
                say(&format!(
                    "Selection: {} vs {} x{}, {} ({}), BVR range {:.0} nm. F10 Range > Air-to-Air > FIGHT'S ON to spawn.",
                    s.setup, s.adversary, s.count, s.weapons, s.skill, s.range_nm
                ));
            }
            "aa" => {
                let s = ctx.sel.get(&gid).cloned().unwrap_or_default();
                let mut params = BTreeMap::new();
                let item = if let Some(asp) = s.setup.strip_prefix("bvr:") {
                    params.insert("aspect".into(), asp.to_string());
                    "bvr"
                } else if s.setup == "drill" {
                    params.insert("aspect".into(), "hot".into());
                    "missile_drill"
                } else {
                    params.insert("setup".into(), s.setup.clone());
                    "bfm"
                };
                params.insert("adversary".into(), s.adversary.clone());
                params.insert("weapons".into(), if item != "bfm" && s.weapons == "guns" { "fox3".into() } else { s.weapons.clone() });
                params.insert("skill".into(), s.skill.clone());
                params.insert("count".into(), s.count.to_string());
                params.insert("range_nm".into(), s.range_nm.to_string());
                let r = spawn_request(lua, ctx, SpawnRequest { ucid: f.ucid.to_string(), item: item.into(), params, instructor: false }, now);
                if !r.ok {
                    say(&format!("Unable: {}", r.message));
                }
            }
            "duel" => match a1 {
                "challenge" => ctx.aa.challenge(lua, &ctx.players, &f, now)?,
                "accept" => ctx.aa.accept(lua, &ctx.players, &f, now)?,
                _ => {
                    ctx.aa.cancel_duels(&f.ucid);
                    say("Knock it off: duel cancelled");
                }
            },
            "trainer" => {
                let on = a1 == "on";
                ctx.players.set_trainer(f.ucid, on);
                say(if on {
                    "Missile trainer ON: a missile that would kill you is removed and scored"
                } else {
                    "Missile trainer OFF: missiles are LIVE"
                });
            }
            "tk" => say(&format!("Tankers:\n{}", ctx.aar.describe().join("\n"))),
            "cv" => {
                let d = ctx.cv.describe(lua);
                say(&if d.is_empty() { "No carriers on this range".to_string() } else { d.join("\n") });
            }
            "helo" => match a1 {
                "pads" => say(&format!("Landing pads:\n{}", ctx.helo.pad_list().join("\n"))),
                "cargo" => say(&ctx.helo.dyn_cargo_help()),
                "load" => {
                    ctx.helo.mark_troop_start(&f.unit_name, now);
                    let m = ctx.helo.load_troops(lua, &f, a2, now)?;
                    say(&m);
                }
                "unload" => {
                    let cfg = ctx.cfg.clone();
                    let m = ctx.helo.unload_troops(lua, &cfg, &mut ctx.rec, &f, now)?;
                    say(&m);
                }
                _ => (),
            },
            "my" => match a1 {
                "clear" => {
                    let n = despawn_owned(lua, ctx, &f.ucid.to_string());
                    say(&format!("{n} spawn(s) removed"));
                }
                _ => {
                    let mine: Vec<std::string::String> = ctx
                        .spawns
                        .active
                        .values()
                        .filter(|s| s.owner.as_deref() == Some(f.ucid.to_string().as_str()))
                        .map(|s| s.label.clone())
                        .collect();
                    say(&if mine.is_empty() { "You have no spawns".to_string() } else { mine.join("\n") });
                }
            },
            "res" => match a1 {
                "last" => {
                    let l = ctx.rec.last.get(&f.ucid.to_string()).cloned().unwrap_or_default();
                    say(&if l.is_empty() { "No results yet this session".to_string() } else { l.join("\n") });
                }
                _ => say("Range help: every bomb, strafe pass, carrier pass, tanker session, missile shot and drill is graded and posted to range.vectorstrike.org with a full debrief. Chat: -range trainer on|off."),
            },
            _ => (),
        }
        Ok(())
    })();
    if let Err(e) = res {
        say(&format!("Unable: {e}"));
    }
    Ok(())
}
