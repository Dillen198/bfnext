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

use crate::{
    Context,
    bg::Task,
    db::{Db, SetS, group::DeployKind},
    msgq::MsgTyp,
    objective_mut, return_lives,
    spawnctx::{SpawnCtx, SpawnLoc},
};
use anyhow::{Context as AnyhowContext, Result, anyhow, bail};
use bfprotocols::{
    api::{
        ArtilleryEntry, Briefing, CampaignState, DeployableEntry, GroupInfo, LogisticsInfo,
        NavaidEntry, ObjectiveDetails, ObjectiveInfo, PlayerInfo, RadioEntry, ThreatEntry,
        UnitInfo, WarehouseInfo,
    },
    cfg::{ActionKind, AwacsCfg, Cfg, DeployableKind, Rule, UnitTag},
    db::{group::GroupId, objective::ObjectiveId},
    perf::Perf,
    stats::Stat,
};
use std::collections::HashMap;
use chrono::prelude::*;
use compact_str::format_compact;
use dcso3::{
    MizLua, String, Vector2,
    coalition::Side,
    degrees_to_radians,
    net::{Net, PlayerId, Ucid},
    object::DcsObject,
    perf::Perf as ApiPerf,
    pointing_towards2,
    trigger::{MarkId, Trigger},
    unit::Unit,
    value_to_json,
    world::World,
};
use enumflags2::BitFlags;
use log::warn;
use mlua::Value;
use netidx::publisher::Value as NetIdxValue;
use parking_lot::{Condvar, Mutex};
use regex::{Regex, RegexBuilder};
use smallvec::{SmallVec, smallvec};
use std::{
    mem,
    str::FromStr,
    sync::Arc,
    time::{Duration, Instant},
};
use tokio::sync::oneshot;

#[derive(Debug, Clone, Copy)]
pub enum WarehouseKind {
    Objective,
    DCS,
}

impl FromStr for WarehouseKind {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        match s {
            "objective" => Ok(Self::Objective),
            "dcs" => Ok(Self::DCS),
            x => bail!("unknown warehouse kind {x}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AdminResult {
    Shutdown,
    Continue,
}

#[derive(Debug, Clone)]
pub enum AdminCommand {
    Help,
    ReduceInventory {
        airbase: String,
        amount: u8,
    },
    TransferSupply {
        from: String,
        to: String,
    },
    LogisticsTickNow,
    LogisticsDeliverNow,
    Repair {
        airbase: String,
    },
    Capture {
        objective: String,
        side: Side,
    },
    Tim {
        key: String,
        size: usize,
        alt: Option<isize>,
    },
    Spawn {
        key: String,
    },
    SideSwitch {
        side: Side,
        player: String,
    },
    Ban {
        player: String,
        until: Option<DateTime<Utc>>,
    },
    Unban {
        player: String,
    },
    Kick {
        player: String,
    },
    Connected,
    Banned,
    Search {
        expr: Regex,
    },
    LogWarehouse {
        kind: WarehouseKind,
        airbase: String,
    },
    Logdesc,
    ResetLives {
        player: String,
    },
    AddAdmin {
        player: String,
    },
    RemoveAdmin {
        player: String,
    },
    Balance {
        player: String,
    },
    SetPoints {
        amount: i32,
        player: String,
    },
    Delete {
        group: GroupId,
    },
    Deslot {
        player: String,
    },
    Remark {
        objective: String,
    },
    Reset {
        winner: Option<Side>,
    },
    Shutdown,
    // Query API commands
    QueryObjectives,
    QueryObjective {
        name: String,
    },
    QueryPlayers,
    QueryPlayer {
        player: String,
    },
    QueryGroups {
        side: Option<Side>,
    },
    QueryGroup {
        id: GroupId,
    },
    QueryUnits {
        group: GroupId,
    },
    QueryWarehouse {
        objective: String,
    },
    QueryLogistics,
    QueryCampaignState,
    QueryPerf,
    QueryBriefing {
        side: Side,
    },
    // Action API commands
    SpawnDeployable {
        side: Side,
        name: String,
        pos: Vector2,
        heading: f64,
    },
    SpawnTroop {
        side: Side,
        name: String,
        pos: Vector2,
        heading: f64,
    },
    MoveGroup {
        id: GroupId,
        pos: Vector2,
    },
    AddPoints {
        player: String,
        amount: i32,
        reason: String,
    },
    Blacklist {
        rule: String,
        player: String,
    },
    Whitelist {
        rule: String,
        player: String,
    },
    ReinitWarehouse {
        airbase: String,
    },
    SetObjectivePriority {
        objective: String,
        priority: bool,
    },
    // Cockpit UI API commands -- scoped to the calling player's own ucid
    // (resolved and trusted by bfdb from their linked session, not
    // client-supplied), not admin-wide like the commands above.
    //
    // The in-DCS cockpit overlay (bflib/lua/cockpit.lua) identifies itself
    // with net.get_my_player_id(), a per-connection id local to each
    // player's own DCS client -- this resolves that id to a ucid using the
    // live connected-player table, so the overlay works the instant a
    // player joins with no manual pairing step.
    ResolvePlayerId {
        id: PlayerId,
    },
    EwrToggle {
        ucid: Ucid,
    },
    EwrReport {
        ucid: Ucid,
        friendly: bool,
    },
    EwrSetUnits {
        ucid: Ucid,
        imperial: bool,
    },
    EwrGroundIntel {
        ucid: Ucid,
    },
    CarpSolve {
        mark_key: String,
        drop_altitude_agl_ft: f64,
    },
    CarpSolveLatLon {
        lat: f64,
        lon: f64,
        drop_altitude_agl_ft: f64,
    },
    CockpitSpawnCrate {
        ucid: Ucid,
        crate_name: String,
        qty: u32,
        c130: bool,
    },
    /// DCSServerBot-derived server state, pushed in from bfdb: the scheduled
    /// restart time and the current surface weather, for the F10 Info menu.
    SetServerInfo {
        restart_at: Option<DateTime<Utc>>,
        weather: Option<crate::BotWeather>,
    },
}

impl AdminCommand {
    pub fn help() -> &'static [&'static str] {
        &[
            "reduce <objective> <percent>: reduce supplies at objective by <percent>",
            "transfer <from-objective> <to-objective>: transfer supplies between two objectives",
            "tick: execute a logistics tick now",
            "deliver: execute a logistics delivery now",
            "repair <airbase>: repair one step at the specified airbase",
            "capture <objective> <blue|red|neutral>: force an objective to change hands",
            "tim <key> [size] [alt]: create explosions of [size] default 3000 at every f10 mark with text <key>",
            "spawn <key>: spawn at f10 mark. <key> <troop|deployable> <side> <heading> <name>",
            "switch <side> <alias|playerid|ucid>: force side switch a player",
            "ban <duration|forever> <alias|playerid|ucid>: kick a player and ban them. e.g. ban 10days D4n",
            "unban <alias|ucid>: unban a player",
            "kick <alias|playerid|ucid>: kick a player",
            "reset-lives <alias|playerid|ucid>",
            "connected: list connected players",
            "banned: list banned players",
            "search <regex>: search the player list by regular expression",
            "log-warehouse <objective|dcs> <airbase>: write the contents of the selected warehouse to the log file",
            "log-desc: write the getDesc of the plane you are currently in to the log file",
            "add-admin <player>: make the specified player a server admin",
            "remove-admin <player>: remove the specified player from the admin list",
            "balance <player>: show <player>'s point balance",
            "set-points <n> <player>: set <player>'s point balance to <n>",
            "delete <groupid>: delete deployed group, now with 100% less mess",
            "deslot <player>: force <player> to spectators",
            "remark <obj>: force refresh the markup on objective",
            "reset [winner]: shutdown the server and reset the campaign state",
            "shutdown: shutdown the server",
            "blacklist <rule> <player>: deny <player> access to <rule> (actions|cargo|troops|jtac|ca)",
            "whitelist <rule> <player>: allow <player> access to <rule> (actions|cargo|troops|jtac|ca)",
            "reinit-warehouse <airbase>: reinitialize the warehouse for the given airbase",
        ]
    }
}

impl FromStr for AdminCommand {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        if s.trim() == "help" {
            Ok(Self::Help)
        } else if let Some(s) = s.strip_prefix("reduce ") {
            match s.split_once(" ") {
                None => bail!("reduce <airbase> <amount>"),
                Some((airbase, amount)) => {
                    let amount = amount.parse::<u8>()?;
                    Ok(Self::ReduceInventory {
                        airbase: String::from(airbase),
                        amount,
                    })
                }
            }
        } else if let Some(s) = s.strip_prefix("transfer ") {
            match s.split_once(" ") {
                None => bail!("transfer <from> <to>"),
                Some((from, to)) => Ok(Self::TransferSupply {
                    from: from.into(),
                    to: to.into(),
                }),
            }
        } else if let Some(_) = s.strip_prefix("tick") {
            Ok(Self::LogisticsTickNow)
        } else if let Some(_) = s.strip_prefix("deliver") {
            Ok(Self::LogisticsDeliverNow)
        } else if let Some(s) = s.strip_prefix("repair ") {
            Ok(Self::Repair { airbase: s.into() })
        } else if let Some(s) = s.strip_prefix("capture ") {
            match s.rsplit_once(' ') {
                None => bail!("capture <objective> <blue|red|neutral>"),
                Some((objective, side)) => Ok(Self::Capture {
                    objective: objective.trim().into(),
                    side: side.trim().parse::<Side>()?,
                }),
            }
        } else if let Some(s) = s.strip_prefix("tim ") {
            match &s.split(" ").collect::<SmallVec<[&str; 4]>>()[..] {
                [] | [""] => bail!("tim <mark> [size] [alt]"),
                [key] => Ok(Self::Tim {
                    key: String::from(*key),
                    size: 3000,
                    alt: None,
                }),
                [key, size] => {
                    let size = size.parse::<usize>()?;
                    Ok(Self::Tim {
                        key: String::from(*key),
                        size,
                        alt: None,
                    })
                }
                [key, size, alt] => {
                    let size = size.parse::<usize>()?;
                    let alt = alt.parse::<isize>()?;
                    Ok(Self::Tim {
                        key: String::from(*key),
                        size,
                        alt: Some(alt),
                    })
                }
                _ => bail!("tim <mark> [size] [alt]"),
            }
        } else if let Some(s) = s.strip_prefix("spawn ") {
            Ok(Self::Spawn { key: s.into() })
        } else if let Some(s) = s.strip_prefix("switch ") {
            match s.split_once(" ") {
                None => bail!("switch <side> <player>"),
                Some((side, player)) => {
                    let side = side.parse::<Side>()?;
                    Ok(Self::SideSwitch {
                        side,
                        player: player.into(),
                    })
                }
            }
        } else if let Some(s) = s.strip_prefix("ban ") {
            match s.split_once(" ") {
                None => bail!("ban <duration|forever> <alias|id|ucid>"),
                Some((dur, player)) => {
                    let until = if dur == "forever" {
                        None
                    } else {
                        let dur = humantime::Duration::from_str(dur)?;
                        Some(Utc::now() + chrono::Duration::seconds(dur.as_secs() as i64))
                    };
                    Ok(Self::Ban {
                        player: player.into(),
                        until,
                    })
                }
            }
        } else if let Some(s) = s.strip_prefix("unban ") {
            Ok(Self::Unban { player: s.into() })
        } else if let Some(s) = s.strip_prefix("kick ") {
            Ok(Self::Kick { player: s.into() })
        } else if let Some(_) = s.strip_prefix("connected") {
            Ok(Self::Connected)
        } else if let Some(_) = s.strip_prefix("banned") {
            Ok(Self::Banned)
        } else if let Some(s) = s.strip_prefix("search ") {
            Ok(Self::Search {
                expr: RegexBuilder::new(s).case_insensitive(true).build()?,
            })
        } else if let Some(s) = s.strip_prefix("log-warehouse ") {
            match s.split_once(" ") {
                None => bail!("log-warehouse <objective|dcs> <airbase>"),
                Some((kind, airbase)) => Ok(Self::LogWarehouse {
                    kind: kind.parse()?,
                    airbase: String::from(airbase),
                }),
            }
        } else if let Some(_) = s.strip_prefix("log-desc") {
            Ok(Self::Logdesc)
        } else if let Some(s) = s.strip_prefix("reset-lives ") {
            Ok(Self::ResetLives { player: s.into() })
        } else if let Some(_) = s.strip_prefix("shutdown") {
            Ok(Self::Shutdown)
        } else if let Some(s) = s.strip_prefix("add-admin ") {
            Ok(Self::AddAdmin { player: s.into() })
        } else if let Some(s) = s.strip_prefix("remove-admin ") {
            Ok(Self::RemoveAdmin { player: s.into() })
        } else if let Some(s) = s.strip_prefix("balance ") {
            Ok(Self::Balance { player: s.into() })
        } else if let Some(s) = s.strip_prefix("set-points ") {
            match s.split_once(" ") {
                None => bail!("set-points: <amount> <player>"),
                Some((amount, player)) => Ok(Self::SetPoints {
                    amount: amount.parse::<i32>()?,
                    player: player.into(),
                }),
            }
        } else if let Some(s) = s.strip_prefix("delete ") {
            Ok(Self::Delete { group: s.parse()? })
        } else if let Some(s) = s.strip_prefix("deslot ") {
            Ok(Self::Deslot { player: s.into() })
        } else if let Some(s) = s.strip_prefix("remark ") {
            Ok(Self::Remark {
                objective: s.into(),
            })
        } else if let Some(s) = s.strip_prefix("reset") {
            let winner = if s == "" {
                None
            } else {
                Some(Side::from_str(s)?)
            };
            Ok(Self::Reset { winner })
        } else if let Some(s) = s.strip_prefix("blacklist ") {
            match s.split_once(" ") {
                None => bail!("blacklist <rule> <player>"),
                Some((rule, player)) => Ok(Self::Blacklist {
                    rule: rule.into(),
                    player: player.into(),
                }),
            }
        } else if let Some(s) = s.strip_prefix("whitelist ") {
            match s.split_once(" ") {
                None => bail!("whitelist <rule> <player>"),
                Some((rule, player)) => Ok(Self::Whitelist {
                    rule: rule.into(),
                    player: player.into(),
                }),
            }
        } else if let Some(s) = s.strip_prefix("reinit-warehouse ") {
            Ok(Self::ReinitWarehouse { airbase: s.into() })
        } else {
            bail!("unknown command {s}")
        }
    }
}

fn admin_spawn(ctx: &mut Context, lua: MizLua, id: Option<PlayerId>, key: String) -> Result<()> {
    let mut to_remove: SmallVec<[MarkId; 8]> = smallvec![];
    let act = Trigger::singleton(lua)?.action()?;
    let spctx = SpawnCtx::new(lua)?;
    let key = format_compact!("{} ", key);
    let ucid = match id {
        None => Ucid::default(),
        Some(id) => {
            ctx.connected
                .get(&id)
                .ok_or_else(|| anyhow!("unknown admin"))?
                .ucid
        }
    };
    enum Kind {
        Troop,
        Deployable,
    }
    impl FromStr for Kind {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
            match s {
                "troop" => Ok(Kind::Troop),
                "deployable" => Ok(Kind::Deployable),
                s => bail!("invalid kind, expected troop or deployable got {s}"),
            }
        }
    }
    for mk in World::singleton(lua)?
        .get_mark_panels()
        .context("getting marks")?
    {
        let mk = mk?;
        if mk.text.starts_with(key.as_str()) {
            to_remove.push(mk.id);
            let spec = mk.text.as_str().strip_prefix(key.as_str())
                .ok_or_else(|| anyhow!("mark text missing expected prefix"))?;
            let mut iter = spec.splitn(4, " ");
            let kind = iter
                .next()
                .ok_or_else(|| {
                    anyhow!(
                        "spawn mark '{}' missing kind expected troop or deployable",
                        spec
                    )
                })?
                .parse::<Kind>()?;
            let side = iter
                .next()
                .ok_or_else(|| anyhow!("spawn mark {} missing side", spec))?;
            let side = side.parse::<Side>().with_context(|| {
                format_compact!("error parsing {} as a side in mark {}", side, spec)
            })?;
            let heading = iter
                .next()
                .ok_or_else(|| anyhow!("spawn mark {} missing heading", spec))?;
            let heading = degrees_to_radians(heading.parse::<u32>().with_context(|| {
                format_compact!("error parsing {} as a heading in mark {}", heading, spec)
            })? as f64);
            let name = iter
                .next()
                .ok_or_else(|| anyhow!("spawn mark {} missing name of the thing to spawn", spec))?;
            let pos = Vector2::new(mk.pos.x, mk.pos.z);
            let loc = SpawnLoc::AtPos {
                pos,
                offset_direction: pointing_towards2(heading),
                group_heading: heading,
            };
            match kind {
                Kind::Troop => {
                    let specs = ctx
                        .db
                        .ephemeral
                        .cfg
                        .troops
                        .get(&side)
                        .ok_or_else(|| anyhow!("no troops on {side}"))?;
                    let spec = specs
                        .iter()
                        .find(|tr| tr.name.as_str() == name)
                        .ok_or_else(|| anyhow!("no troop called {name} on {side}"))?
                        .clone();
                    let origin = DeployKind::Troop {
                        player: ucid.clone(),
                        moved_by: None,
                        spec: spec.clone(),
                        origin: None,
                        cost_fraction: 1.,
                        jtac: None,
                    };
                    ctx.db
                        .add_and_queue_group(
                            &spctx,
                            &ctx.idx,
                            side,
                            loc,
                            &spec.template,
                            origin,
                            BitFlags::empty(),
                            None,
                        )
                        .context("adding group")?;
                }
                Kind::Deployable => {
                    let specs = ctx
                        .db
                        .ephemeral
                        .cfg
                        .deployables
                        .get(&side)
                        .ok_or_else(|| anyhow!("no deployables on {side}"))?;
                    let spec = specs
                        .iter()
                        .find(|dp| dp.path.ends_with(&[String::from(name)]))
                        .ok_or_else(|| anyhow!("no deployable called {name} on {side}"))?
                        .clone();
                    match &spec.kind {
                        DeployableKind::Objective(parts) => {
                            ctx.db
                                .add_farp(lua, &spctx, &ctx.idx, side, pos, &spec, parts)
                                .context("adding farp")?;
                        }
                        DeployableKind::Group { template } => {
                            let origin = DeployKind::Deployed {
                                player: ucid.clone(),
                                moved_by: None,
                                spec: spec.clone(),
                                origin: None,
                                cost_fraction: 1.,
                                jtac: None,
                            };
                            ctx.db
                                .add_and_queue_group(
                                    &spctx,
                                    &ctx.idx,
                                    side,
                                    loc,
                                    &template,
                                    origin,
                                    BitFlags::empty(),
                                    None,
                                )
                                .context("adding group")?;
                        }
                    }
                }
            }
        }
    }
    for id in to_remove {
        act.remove_mark(id).context("removing mark")?;
    }
    Ok(())
}

pub(super) fn get_player_ucid<'a>(ctx: &'a Context, key: &str) -> Result<Ucid> {
    if let Ok(id) = key.parse::<PlayerId>() {
        if let Some(ifo) = ctx.connected.get(&id) {
            return Ok(ifo.ucid.clone());
        }
    }
    if let Ok(ucid) = key.parse::<Ucid>() {
        if ctx.db.player(&ucid).is_some() {
            return Ok(ucid);
        }
    }
    enum Matcher<'a> {
        Re(Regex),
        Exact(&'a str),
    }
    impl<'a> Matcher<'a> {
        fn is_match(&self, candidate: &str) -> bool {
            match self {
                Self::Re(re) => re.is_match(candidate),
                Self::Exact(s) => *s == candidate,
            }
        }
    }
    let expr = match RegexBuilder::new(key).case_insensitive(true).build() {
        Ok(re) => Matcher::Re(re),
        Err(_) => Matcher::Exact(key),
    };
    let mut candidates: SmallVec<[(&Ucid, &String); 32]> = {
        ctx.db
            .persisted
            .players()
            .into_iter()
            .filter(|(_, player)| {
                player
                    .alts
                    .into_iter()
                    .any(|alt| expr.is_match(alt.as_str()))
            })
            .map(|(ucid, player)| (ucid, &player.name))
            .collect()
    };
    if candidates.len() == 1 {
        return Ok(candidates.pop()
            .ok_or_else(|| anyhow!("no candidates"))?.0.clone());
    } else if candidates.len() > 1 {
        bail!("multiple matching candidates {:?}", candidates)
    }
    bail!("no player found for alias, player id, or ucid \"{}\"", key)
}

pub fn get_airbase(db: &Db, name: &str) -> Result<ObjectiveId> {
    for (oid, obj) in db.objectives() {
        if obj.name.as_str() == name {
            return Ok(*oid);
        }
    }
    let re = RegexBuilder::new(name)
        .case_insensitive(true)
        .build()
        .context("building regex")?;
    let mut candidates: SmallVec<[(ObjectiveId, String); 32]> = smallvec![];
    for (oid, obj) in db.objectives() {
        if re.is_match(obj.name.as_str()) {
            candidates.push((*oid, obj.name.clone()));
        }
    }
    if candidates.len() == 0 {
        bail!("no objective name matches {name}")
    } else if candidates.len() == 1 {
        Ok(candidates[0].0)
    } else {
        bail!("multiple objectives match {name}, matches: {candidates:?}")
    }
}

fn admin_sideswitch(ctx: &mut Context, side: Side, name: String) -> Result<()> {
    let ucid = get_player_ucid(ctx, name.as_str())?;
    ctx.db.force_sideswitch_player(&ucid, side)
}

fn with_mut_cfg<F: FnOnce(&mut Cfg) -> Result<()>>(ctx: &mut Context, f: F) -> Result<()> {
    {
        let cfg = Arc::make_mut(&mut ctx.db.ephemeral.cfg);
        f(cfg)?
    }
    let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
    ctx.do_bg_task(Task::SaveConfig(ctx.miz_state_path.clone(), cfg));
    Ok(())
}

fn admin_ban(
    ctx: &mut Context,
    lua: MizLua,
    until: Option<DateTime<Utc>>,
    name: &String,
) -> Result<()> {
    let ucid = get_player_ucid(ctx, name.as_str())?;
    let name = ctx
        .db
        .player(&ucid)
        .map(|p| p.name.clone())
        .unwrap_or_else(|| name.clone());
    with_mut_cfg(ctx, |cfg| {
        cfg.banned.insert(ucid.clone(), (until, name));
        Ok(())
    })?;
    if let Some(id) = ctx.connected.id_by_ucid.get(&ucid) {
        let msg = match until {
            None => format_compact!("you are banned forever"),
            Some(ts) => format_compact!("you are banned until {}", ts),
        };
        Net::singleton(lua)?.kick(*id, msg.into())?;
    }
    Ok(())
}

fn admin_kick(ctx: &mut Context, lua: MizLua, name: &String) -> Result<()> {
    let ucid = get_player_ucid(ctx, name.as_str())?;
    let id = match ctx.connected.id_by_ucid.get(&ucid) {
        None => bail!("no connected player found, is {name} on the server?"),
        Some(id) => *id,
    };
    Net::singleton(lua)?.kick(id, "you have been kicked by an admin".into())
}

// FreeDanielUnjustifiedBan
fn admin_unban(ctx: &mut Context, name: &String) -> Result<()> {
    let ucid = get_player_ucid(ctx, name.as_str())?;
    with_mut_cfg(ctx, |cfg| match cfg.banned.remove(&ucid) {
        None => bail!("was not banned"),
        Some(_) => Ok(()),
    })
}

fn admin_list_banned(ctx: &Context) -> SmallVec<[(Ucid, String, Option<DateTime<Utc>>); 16]> {
    ctx.db
        .ephemeral
        .cfg
        .banned
        .iter()
        .map(|(ucid, (until, name))| (ucid.clone(), name.clone(), *until))
        .collect()
}

fn admin_list_connected(ctx: &Context) -> SmallVec<[(PlayerId, Ucid, String); 64]> {
    ctx.connected
        .info_by_player_id
        .iter()
        .map(|(id, ifo)| (*id, ifo.ucid.clone(), ifo.name.clone()))
        .collect()
}

fn admin_search(
    ctx: &Context,
    expr: Regex,
) -> SmallVec<[(Option<PlayerId>, Ucid, SetS<String>); 64]> {
    ctx.db
        .persisted
        .players()
        .into_iter()
        .filter_map(|(ucid, player)| {
            if player
                .alts
                .into_iter()
                .any(|name| expr.is_match(name.as_str()))
            {
                Some((
                    ctx.connected.id_by_ucid.get(ucid).map(|id| *id),
                    ucid.clone(),
                    player.alts.clone(),
                ))
            } else {
                None
            }
        })
        .collect()
}

fn admin_log_desc(ctx: &Context, lua: MizLua, ucid: &Ucid) -> Result<()> {
    let slot = &ctx
        .db
        .player(ucid)
        .ok_or_else(|| anyhow!("no such player {ucid}"))?
        .current_slot
        .as_ref()
        .ok_or_else(|| anyhow!("player {ucid} isn't in a slot"))?
        .0;
    let id = ctx
        .db
        .ephemeral
        .get_object_id_by_slot(&slot)
        .ok_or_else(|| anyhow!("player {ucid} unit not found"))?;
    let unit = Unit::get_instance(lua, &id).context("getting unit")?;
    let desc = Value::Table(unit.get_desc().context("getting desc")?);
    let desc = value_to_json(&desc);
    let ammo = Value::Table(unit.get_ammo().context("getting ammo")?.into_inner());
    let ammo = value_to_json(&ammo);
    warn!("{desc}\n{ammo}");
    Ok(())
}

fn admin_reset_lives(ctx: &mut Context, player: &String) -> Result<()> {
    let ucid = get_player_ucid(ctx, player)?;
    ctx.db.player_reset_lives(&ucid)
}

pub(super) fn admin_shutdown(
    ctx: &mut Context,
    lua: MizLua,
    reset: Option<Option<Side>>,
) -> Result<AdminResult> {
    let wait = Arc::new((Mutex::new(false), Condvar::new()));
    let se = {
        let perf = unsafe { Perf::get_mut() };
        let api_perf = unsafe { ApiPerf::get_mut() };
        Stat::SessionEnd {
            perf: (*perf.inner).clone(),
            frame: (*perf.frame).clone(),
            api_perf: (*api_perf.0).clone(),
        }
    };
    ctx.db.ephemeral.remove_map_layer();
    if let Some(winner) = reset {
        ctx.do_bg_task(Task::ResetState(ctx.miz_state_path.clone()));
        ctx.do_bg_task(Task::Stat(se));
        ctx.do_bg_task(Task::Stat(Stat::RoundEnd { winner }));
    } else {
        return_lives(lua, ctx, DateTime::<Utc>::MAX_UTC);
        ctx.do_bg_task(Task::SaveState(
            ctx.miz_state_path.clone(),
            ctx.db.persisted.clone(),
        ));
        ctx.do_bg_task(Task::Stat(se));
    }
    if let Some(cfg) = ctx.db.ephemeral.cfg.live_weather.clone() {
        match ctx.mission_file_path.clone() {
            Some(miz_path) => ctx.do_bg_task(Task::RewriteMissionWeather { miz_path, cfg }),
            None => warn!("live_weather is configured but no mission file path is known, skipping"),
        }
    }
    ctx.do_bg_task(Task::Shutdown(Arc::clone(&wait)));
    let start = Instant::now();
    let wait_for = Duration::from_secs(60);
    let &(ref lock, ref cvar) = &*wait;
    let mut synced = lock.lock();
    while !*synced && start.elapsed() < wait_for {
        cvar.wait_for(&mut synced, wait_for - start.elapsed());
    }
    Ok(AdminResult::Shutdown)
}

fn add_admin(ctx: &mut Context, player: &String) -> Result<()> {
    let ucid = get_player_ucid(ctx, player)?;
    let name = ctx
        .db
        .player(&ucid)
        .ok_or_else(|| anyhow!("missing info for admin {ucid}"))?
        .name
        .clone();
    with_mut_cfg(ctx, move |cfg| {
        cfg.admins.insert(ucid, name);
        Ok(())
    })
}

fn remove_admin(ctx: &mut Context, player: &String) -> Result<()> {
    let ucid = get_player_ucid(ctx, player)?;
    with_mut_cfg(ctx, |cfg| {
        cfg.admins.remove(&ucid);
        Ok(())
    })
}

fn balance(ctx: &Context, player: &String) -> Result<i32> {
    let ucid = get_player_ucid(ctx, player)?;
    let player = ctx
        .db
        .player(&ucid)
        .ok_or_else(|| anyhow!("no such player {player}"))?;
    Ok(player.points)
}

fn set_points(ctx: &mut Context, player: &String, amount: i32) -> Result<()> {
    match player.strip_prefix("objective:") {
        None => {
            let ucid = get_player_ucid(ctx, player)?;
            let player = ctx
                .db
                .player_mut(&ucid)
                .ok_or_else(|| anyhow!("no such player {player}"))?;
            player.points = amount;
            ctx.db.ephemeral.dirty();
            Ok(())
        }
        Some(target) => {
            let oid = get_airbase(&ctx.db, target)?;
            let obj = objective_mut!(&mut ctx.db, oid)?;
            obj.points = amount;
            ctx.db.ephemeral.dirty();
            Ok(())
        }
    }
}

fn delete(ctx: &mut Context, id: &GroupId) -> Result<()> {
    match &ctx.db.group(id)?.origin {
        DeployKind::Objective { .. } | DeployKind::ObjectiveDeprecated => {
            bail!("you can't delete objective groups")
        }
        DeployKind::Crate { .. }
        | DeployKind::Deployed { .. }
        | DeployKind::Troop { .. }
        | DeployKind::Action { .. }
        | DeployKind::DownedPilot { .. }
        | DeployKind::Dismount { .. } => ctx.db.delete_group(id),
    }
}

fn deslot(ctx: &mut Context, player: &String) -> Result<()> {
    let ucid = get_player_ucid(ctx, player)?;
    ctx.db.ephemeral.force_player_to_spectators(&ucid);
    Ok(())
}

fn remark(ctx: &mut Context, objective: &String) -> Result<()> {
    let oid = get_airbase(&ctx.db, objective)?;
    let obj = ctx
        .db
        .persisted
        .objectives
        .get(&oid)
        .ok_or_else(|| anyhow!("no such objective {oid}"))?;
    ctx.db
        .ephemeral
        .create_objective_markup(&ctx.db.persisted, obj);
    Ok(())
}

// ==================== Query API Functions ====================

pub(crate) fn query_objectives(ctx: &Context) -> Vec<ObjectiveInfo> {
    ctx.db
        .objectives()
        .map(|(_, obj)| {
            let mut group_count = HashMap::new();
            for (side, groups) in obj.groups() {
                group_count.insert(format!("{:?}", side), groups.len());
            }
            ObjectiveInfo {
                id: obj.id,
                name: obj.name.to_string(),
                kind: obj.kind().name().to_string(),
                owner: obj.owner,
                pos: (obj.pos().x, obj.pos().y),
                health: obj.health(),
                logi: obj.logi(),
                supply: obj.supply(),
                fuel: obj.fuel(),
                threatened: obj.threatened(),
                captureable: obj.captureable(),
                group_count,
                priority: obj.priority(),
            }
        })
        .collect()
}

pub(crate) fn query_objective_details(ctx: &Context, name: &str) -> Result<ObjectiveDetails> {
    let oid = get_airbase(&ctx.db, name)?;
    let obj = ctx
        .db
        .persisted
        .objectives
        .get(&oid)
        .ok_or_else(|| anyhow!("no such objective {oid}"))?;

    let mut group_count = HashMap::new();
    for (side, groups) in obj.groups() {
        group_count.insert(format!("{:?}", side), groups.len());
    }

    let mut equipment = HashMap::new();
    for (item, inv) in obj.warehouse().equipment() {
        equipment.insert(item.to_string(), inv.stored);
    }

    let mut liquids = HashMap::new();
    for (liquid_type, inv) in obj.warehouse().liquids() {
        liquids.insert(format!("{:?}", liquid_type), inv.stored);
    }

    Ok(ObjectiveDetails {
        info: ObjectiveInfo {
            id: obj.id,
            name: obj.name.to_string(),
            kind: obj.kind().name().to_string(),
            owner: obj.owner,
            pos: (obj.pos().x, obj.pos().y),
            health: obj.health(),
            logi: obj.logi(),
            supply: obj.supply(),
            fuel: obj.fuel(),
            threatened: obj.threatened(),
            captureable: obj.captureable(),
            group_count,
            priority: obj.priority(),
        },
        equipment,
        liquids,
        points: obj.points(),
    })
}

pub(crate) fn query_players(ctx: &Context) -> Vec<PlayerInfo> {
    ctx.db
        .persisted
        .players()
        .into_iter()
        .map(|(ucid, player)| {
            let mut lives = HashMap::new();
            for (life_type, (_, count)) in &player.lives {
                lives.insert(format!("{:?}", life_type), *count);
            }

            let (current_slot, current_unit_type, in_air, position) =
                match &player.current_slot {
                    Some((slot, Some(instanced))) => (
                        Some(slot.to_string()),
                        Some(instanced.typ.to_string()),
                        instanced.in_air,
                        Some((instanced.position.p.0.x, instanced.position.p.0.z)),
                    ),
                    Some((slot, None)) => (Some(slot.to_string()), None, false, None),
                    None => (None, None, false, None),
                };

            PlayerInfo {
                ucid: ucid.clone(),
                name: player.name.to_string(),
                side: player.side,
                points: player.points,
                lives,
                current_slot,
                current_unit_type,
                in_air,
                position,
            }
        })
        .collect()
}

pub(crate) fn query_player_details(ctx: &Context, name: &str) -> Result<PlayerInfo> {
    let ucid = get_player_ucid(ctx, name)?;
    let player = ctx
        .db
        .player(&ucid)
        .ok_or_else(|| anyhow!("no such player {name}"))?;

    let mut lives = HashMap::new();
    for (life_type, (_, count)) in &player.lives {
        lives.insert(format!("{:?}", life_type), *count);
    }

    let (current_slot, current_unit_type, in_air, position) = match &player.current_slot {
        Some((slot, Some(instanced))) => (
            Some(slot.to_string()),
            Some(instanced.typ.to_string()),
            instanced.in_air,
            Some((instanced.position.p.0.x, instanced.position.p.0.z)),
        ),
        Some((slot, None)) => (Some(slot.to_string()), None, false, None),
        None => (None, None, false, None),
    };

    Ok(PlayerInfo {
        ucid,
        name: player.name.to_string(),
        side: player.side,
        points: player.points,
        lives,
        current_slot,
        current_unit_type,
        in_air,
        position,
    })
}

pub(crate) fn query_groups(ctx: &Context, side_filter: Option<Side>) -> Vec<GroupInfo> {
    ctx.db
        .persisted
        .groups
        .into_iter()
        .filter(|(_, group)| side_filter.map_or(true, |s| group.side == s))
        .map(|(_, group)| {
            let alive_count = group
                .units
                .into_iter()
                .filter(|uid| {
                    ctx.db
                        .persisted
                        .units
                        .get(uid)
                        .map_or(false, |u| !u.dead)
                })
                .count();

            let center = ctx.db.group_center(&group.id).ok().map(|c| (c.x, c.y));

            let (origin_type, deployed_by) = match &group.origin {
                DeployKind::Objective { .. } | DeployKind::ObjectiveDeprecated => {
                    ("Objective".to_string(), None)
                }
                DeployKind::Deployed { player, .. } => {
                    ("Deployed".to_string(), Some(player.clone()))
                }
                DeployKind::Troop { player, .. } => ("Troop".to_string(), Some(player.clone())),
                DeployKind::Crate { player, .. } => ("Crate".to_string(), Some(player.clone())),
                DeployKind::Action { player, .. } => ("Action".to_string(), player.clone()),
                DeployKind::DownedPilot { ucid, .. } => {
                    ("DownedPilot".to_string(), Some(ucid.clone()))
                }
                DeployKind::Dismount { .. } => ("Dismount".to_string(), None),
            };

            GroupInfo {
                id: group.id,
                name: group.name.to_string(),
                side: group.side,
                kind: group.kind.map(|k| format!("{:?}", k)),
                class: format!("{:?}", group.class),
                origin_type,
                deployed_by,
                alive_count,
                total_count: group.units.len(),
                center,
            }
        })
        .collect()
}

pub(crate) fn query_group_details(ctx: &Context, id: &GroupId) -> Result<GroupInfo> {
    let group = ctx.db.group(id)?;
    let alive_count = group
        .units
        .into_iter()
        .filter(|uid| {
            ctx.db
                .persisted
                .units
                .get(uid)
                .map_or(false, |u| !u.dead)
        })
        .count();

    let center = ctx.db.group_center(&group.id).ok().map(|c| (c.x, c.y));

    let (origin_type, deployed_by) = match &group.origin {
        DeployKind::Objective { .. } | DeployKind::ObjectiveDeprecated => {
            ("Objective".to_string(), None)
        }
        DeployKind::Deployed { player, .. } => ("Deployed".to_string(), Some(player.clone())),
        DeployKind::Troop { player, .. } => ("Troop".to_string(), Some(player.clone())),
        DeployKind::Crate { player, .. } => ("Crate".to_string(), Some(player.clone())),
        DeployKind::Action { player, .. } => ("Action".to_string(), player.clone()),
        DeployKind::DownedPilot { ucid, .. } => {
            ("DownedPilot".to_string(), Some(ucid.clone()))
        }
        DeployKind::Dismount { .. } => ("Dismount".to_string(), None),
    };

    Ok(GroupInfo {
        id: group.id,
        name: group.name.to_string(),
        side: group.side,
        kind: group.kind.map(|k| format!("{:?}", k)),
        class: format!("{:?}", group.class),
        origin_type,
        deployed_by,
        alive_count,
        total_count: group.units.len(),
        center,
    })
}

pub(crate) fn query_units(ctx: &Context, group_id: &GroupId) -> Result<Vec<UnitInfo>> {
    let group = ctx.db.group(group_id)?;
    let units: Vec<UnitInfo> = group
        .units
        .into_iter()
        .filter_map(|uid| ctx.db.persisted.units.get(uid))
        .map(|unit| UnitInfo {
            id: unit.id,
            group_id: unit.group,
            name: unit.name.to_string(),
            typ: unit.typ.to_string(),
            side: unit.side,
            pos: (unit.pos.x, unit.pos.y),
            heading: unit.heading,
            alive: !unit.dead,
        })
        .collect();
    Ok(units)
}

pub(crate) fn query_warehouse(ctx: &Context, objective_name: &str) -> Result<WarehouseInfo> {
    let oid = get_airbase(&ctx.db, objective_name)?;
    let obj = ctx
        .db
        .persisted
        .objectives
        .get(&oid)
        .ok_or_else(|| anyhow!("no such objective {oid}"))?;

    let mut equipment = HashMap::new();
    for (item, inv) in obj.warehouse().equipment() {
        equipment.insert(item.to_string(), inv.stored);
    }

    let mut liquids = HashMap::new();
    for (liquid_type, inv) in obj.warehouse().liquids() {
        liquids.insert(format!("{:?}", liquid_type), inv.stored);
    }

    Ok(WarehouseInfo {
        objective_id: oid,
        objective_name: obj.name.to_string(),
        equipment,
        liquids,
    })
}

pub(crate) fn query_logistics(ctx: &Context) -> LogisticsInfo {
    let stage = format!("{:?}", ctx.db.ephemeral.logistics_stage());
    LogisticsInfo {
        stage,
        next_tick_seconds: None, // Could be computed from logistics timing if needed
        pending_transfers: vec![], // Would need to track pending transfers
    }
}

pub(crate) fn query_campaign_state(ctx: &Context) -> CampaignState {
    let mut objectives_by_side: HashMap<std::string::String, usize> = HashMap::new();
    let mut players_by_side: HashMap<std::string::String, usize> = HashMap::new();
    let mut points_by_side: HashMap<std::string::String, i64> = HashMap::new();

    for (_, obj) in ctx.db.objectives() {
        *objectives_by_side
            .entry(format!("{:?}", obj.owner))
            .or_insert(0) += 1;
    }

    for (_, player) in ctx.db.persisted.players() {
        *players_by_side
            .entry(format!("{:?}", player.side))
            .or_insert(0) += 1;
        *points_by_side
            .entry(format!("{:?}", player.side))
            .or_insert(0) += player.points as i64;
    }

    CampaignState {
        objectives_by_side,
        players_by_side,
        points_by_side,
    }
}

/// Built-in AGM-88 ALIC / threat codes by DCS emitter type. `cfg.harm_codes`
/// (exact key match) overrides these; entries here are tried exact-first then
/// as a substring so a family key ("S-300PS", "Pantsir") still resolves.
/// Ordered most-specific first. Source: DimOn F/A-18C RWR/HARM reference.
const DEFAULT_HARM_CODES: &[(&str, &str)] = &[
    ("SNR_75V", "126"),                 // SA-2 Fan Song
    ("snr s-125 tr", "123"),            // SA-3 Low Blow
    ("p-19 s-125 sr", "123"),           // SA-3 / P-19 flat face
    ("RPC_5N62V", "129"),               // SA-5 Square Pair
    ("RLS_19J6", "129"),                // SA-5 Tall King
    ("Kub 1S91 str", "108"),            // SA-6 Straight Flush
    ("1S91", "108"),
    ("Osa 9A33 ln", "117"),             // SA-8 Gecko
    ("S-300PS 40B6MD sr", "103"),       // SA-10 Clam Shell  (before 40B6M!)
    ("S-300PS 64H6E sr", "104"),        // SA-10 Big Bird
    ("S-300PS 40B6M tr", "110"),        // SA-10 Flap Lid
    ("S-300PS", "110"),
    ("SA-11 Buk LN 9A310M1", "115"),    // SA-11 Fire Dome
    ("SA-11 Buk SR 9S18M1", "107"),     // SA-11 Snow Drift
    ("Strela-10M3", "118"),             // SA-13 Gopher
    ("Tor 9A331", "119"),               // SA-15 Gauntlet
    ("2S6 Tunguska", "120"),            // SA-19 Grison
    ("Pantsir", "134"),                 // SA-22 Greyhound
    ("HQ-7_STR_SP", "128"),
    ("HQ-7_LN_SP", "127"),
    ("Gepard", "207"),
    ("Vulcan", "208"),
    ("ZSU-23-4 Shilka", "121"),
    ("Roland ADS", "201"),
    ("Roland Radar", "205"),
    ("rapier_fsa_blindfire_radar", "124"),
    ("rapier_fsa_launcher", "125"),
    ("Hawk cwar", "206"),
    ("Hawk sr", "203"),
    ("Hawk tr", "204"),
    ("Patriot str", "202"),
    ("NASAMS_Radar_MPQ64F1", "209"),
    ("NASAMS", "209"),
    ("IRIS-T", "135"),
];

fn harm_code_for(typ: &str, cfg: &Cfg) -> Option<std::string::String> {
    if let Some(c) = cfg.harm_codes.get(typ) {
        return Some(c.to_string());
    }
    DEFAULT_HARM_CODES
        .iter()
        .find(|(needle, _)| typ == *needle || typ.contains(needle))
        .map(|(_, code)| code.to_string())
}

/// Assemble the per-side kneeboard briefing (navaids, radios, artillery,
/// deployables, threats). Positions are converted to lat/lon here since bfdb
/// has no DCS coord library of its own.
pub(crate) fn query_briefing(ctx: &Context, lua: MizLua, side: Side) -> Briefing {
    let db = &ctx.db;
    let cfg = &db.ephemeral.cfg;
    let coord = dcso3::coord::Coord::singleton(lua).ok();
    let to_ll = |p: Vector2| -> (f64, f64) {
        coord
            .as_ref()
            .and_then(|c| {
                c.lo_to_ll(dcso3::LuaVec3(dcso3::Vector3::new(p.x, 0.0, p.y)))
                    .ok()
            })
            .map(|ll| (ll.latitude, ll.longitude))
            .unwrap_or((0.0, 0.0))
    };

    // ── navaids (one row per entry -- one per ground objective, one per ship
    //    for a carrier task force) ──
    let mut navaids = vec![];
    for (oid, navs) in &db.persisted.navaids {
        let Some(obj) = db.persisted.objectives.get(oid) else { continue };
        if obj.owner() != side {
            continue;
        }
        let (lat, lon) = to_ll(obj.pos());
        let brc = if obj.kind().is_carrier_group() {
            Some(crate::atis::carrier_brc(db, obj.kind()))
        } else {
            None
        };
        for nav in navs {
            let tacan = nav
                .tacan_channel
                .map(|ch| format!("{ch}{} {}", nav.tacan_band, nav.morse));
            navaids.push(NavaidEntry {
                objective: obj.name().to_string(),
                kind: obj.kind().name().to_string(),
                deck: nav.label.as_ref().map(|s| s.to_string()),
                lat,
                lon,
                tacan,
                ndb_khz: nav.ndb_khz,
                icls: nav.icls_channel,
                link4_mhz: nav.link4_mhz,
                acls: nav.acls,
                brc,
            });
        }
    }
    navaids.sort_by(|a, b| (a.objective.as_str(), a.deck.as_deref()).cmp(&(b.objective.as_str(), b.deck.as_deref())));

    // ── radios (AWACS / tankers / JTACs) ──
    let mut radios = vec![];
    for (_, group) in &db.persisted.groups {
        if group.side != side {
            continue;
        }
        let DeployKind::Action { spec, name, .. } = &group.origin else { continue };
        let (kind, plane) = match &spec.kind {
            ActionKind::Awacs(AwacsCfg { plane, .. }) => ("AWACS", plane),
            ActionKind::Tanker(plane) => ("TANKER", plane),
            _ => continue,
        };
        let tacan = plane.tacan_channel.map(|ch| {
            let band = plane
                .tacan_band
                .as_ref()
                .map(|b| format!("{b:?}"))
                .unwrap_or_else(|| "X".to_string());
            let cs = plane
                .tacan_callsign
                .as_ref()
                .map(|c| format!(" {c}"))
                .unwrap_or_default();
            format!("{ch}{band}{cs}")
        });
        radios.push(RadioEntry {
            label: format!("{kind} {name}"),
            kind: kind.to_string(),
            freq_mhz: plane.freq.map(|f| f as f64 / 1_000_000.0),
            tacan,
            extra: None,
        });
    }
    for j in ctx.jtac.jtacs().filter(|j| j.side() == side) {
        let loc = j.location();
        let near = db
            .persisted
            .objectives
            .get(&loc.oid)
            .map(|o| o.name().to_string())
            .unwrap_or_default();
        radios.push(RadioEntry {
            label: format!("JTAC {:?}", j.gid()),
            kind: "JTAC".to_string(),
            freq_mhz: None,
            tacan: None,
            extra: Some(format!("laser {} near {near}", j.code())),
        });
    }

    // ── artillery ──
    let mut artillery = vec![];
    let art_cfg = cfg.artillery.as_ref();
    for (gid, group) in &db.persisted.groups {
        if group.side != side {
            continue;
        }
        let live: Vec<_> = group
            .units
            .into_iter()
            .filter_map(|uid| db.persisted.units.get(uid))
            .filter(|u| !u.dead)
            .collect();
        let Some(arty_unit) = live
            .iter()
            .find(|u| u.tags.contains(UnitTag::Artillery))
        else {
            continue
        };
        let typ = arty_unit.typ.0.to_string();
        let (min_r, max_r) = art_cfg
            .and_then(|a| a.units.get(typ.as_str()).map(|u| (u.min_range_m, u.max_range_m)))
            .or_else(|| art_cfg.map(|a| (a.default_min_range_m, a.default_max_range_m)))
            .unwrap_or((4000.0, 30000.0));
        let center = db.group_center(gid).unwrap_or(arty_unit.pos);
        let (lat, lon) = to_ll(center);
        artillery.push(ArtilleryEntry {
            group: group.name.to_string(),
            typ,
            lat,
            lon,
            min_range_m: min_r,
            max_range_m: max_r,
            alive: live.len(),
        });
    }
    artillery.sort_by(|a, b| a.group.cmp(&b.group));

    // ── deployables ──
    let mut deployables = vec![];
    if let Some(list) = cfg.deployables.get(&side) {
        for d in list {
            let full = d.path.join(" / ");
            let deployed = db
                .persisted
                .deployed
                .into_iter()
                .filter(|gid| {
                    db.persisted.groups.get(gid).map_or(false, |g| {
                        matches!(&g.origin, DeployKind::Deployed { spec, .. } if spec.path == d.path)
                    })
                })
                .count() as u32;
            let mut tags = vec![];
            if d.ewr.is_some() {
                tags.push("EWR".to_string());
            }
            if d.jtac.is_some() {
                tags.push("JTAC".to_string());
            }
            if d.gci.is_some() {
                tags.push("GCI".to_string());
            }
            // Total physical crates to build it = sum of each crate type's
            // `required` count (a deployable may need several of several types).
            deployables.push(DeployableEntry {
                name: full,
                cost: d.cost,
                crates_required: d.crates.iter().map(|c| c.required.max(1) as usize).sum(),
                limit: d.limit,
                deployed,
                tags,
            });
        }
    }

    // ── threats (enemy SAM / radar types in play) ──
    let enemy = side.opposite();
    let mut threat_counts: HashMap<std::string::String, usize> = HashMap::new();
    for (_, group) in &db.persisted.groups {
        if group.side != enemy {
            continue;
        }
        if !matches!(
            group.class,
            crate::db::objective::ObjGroupClass::Lr
                | crate::db::objective::ObjGroupClass::Mr
                | crate::db::objective::ObjGroupClass::Sr
                | crate::db::objective::ObjGroupClass::Aaa
        ) {
            continue;
        }
        for uid in group.units.into_iter() {
            if let Some(u) = db.persisted.units.get(uid) {
                if u.dead {
                    continue;
                }
                let is_radar = u.tags.contains(UnitTag::SearchRadar)
                    || u.tags.contains(UnitTag::TrackRadar);
                if is_radar {
                    *threat_counts.entry(u.typ.0.to_string()).or_insert(0) += 1;
                }
            }
        }
    }
    let mut threats: Vec<ThreatEntry> = threat_counts
        .into_iter()
        .map(|(typ, count)| {
            let ewr = cfg.ground_radar_ewrs.get(typ.as_str());
            ThreatEntry {
                harm_code: harm_code_for(&typ, cfg),
                band: ewr.map(|e| format!("{:?}", e.frequency_band)),
                max_range_km: ewr.map(|e| e.range as f64 / 1000.0),
                typ,
                count,
            }
        })
        .collect();
    threats.sort_by(|a, b| b.count.cmp(&a.count).then(a.typ.cmp(&b.typ)));

    Briefing {
        side,
        generated: Utc::now().to_rfc3339(),
        navaids,
        radios,
        artillery,
        deployables,
        threats,
    }
}

/// Snapshots the engine/API perf counters accumulated so far *this session*
/// (the same globals admin_shutdown reads to build Stat::SessionEnd), so
/// bfdb's perf endpoint can show live numbers throughout an active round
/// instead of only after a session ends. Field names deliberately match
/// bfdb's own `SessionEnd` struct (time/frame/api/engine) so bfdb can
/// deserialize this JSON straight into it with no translation step.
pub(crate) fn query_perf() -> serde_json::Value {
    let perf = unsafe { Perf::get_mut() };
    let api_perf = unsafe { ApiPerf::get_mut() };
    let engine = (*perf.inner).clone();
    let frame = (*perf.frame).clone();
    let api = (*api_perf.0).clone();
    serde_json::json!({
        "time": Utc::now(),
        "frame": frame,
        "api": api,
        "engine": engine,
    })
}

// ==================== Action API Functions ====================

pub(crate) fn api_spawn_deployable(
    ctx: &mut Context,
    lua: MizLua,
    side: Side,
    name: &str,
    pos: Vector2,
    heading: f64,
) -> Result<GroupId> {
    let spctx = SpawnCtx::new(lua)?;
    let specs = ctx
        .db
        .ephemeral
        .cfg
        .deployables
        .get(&side)
        .ok_or_else(|| anyhow!("no deployables on {side}"))?;

    let spec = specs
        .iter()
        .find(|dp| dp.path.iter().any(|p| p.as_str() == name))
        .ok_or_else(|| anyhow!("no deployable called {name} on {side}"))?
        .clone();

    let loc = SpawnLoc::AtPos {
        pos,
        offset_direction: pointing_towards2(heading),
        group_heading: heading,
    };

    match &spec.kind {
        DeployableKind::Objective(_) => {
            bail!("cannot spawn objective deployables via API, use spawn-troop or spawn mark")
        }
        DeployableKind::Group { template } => {
            let origin = DeployKind::Deployed {
                player: Ucid::default(),
                moved_by: None,
                spec: spec.clone(),
                cost_fraction: 1.0,
                origin: None,
                jtac: None,
            };
            let gid = ctx.db.add_and_queue_group(
                &spctx,
                &ctx.idx,
                side,
                loc,
                template,
                origin,
                BitFlags::empty(),
                None,
            )?;
            Ok(gid)
        }
    }
}

pub(crate) fn api_spawn_troop(
    ctx: &mut Context,
    lua: MizLua,
    side: Side,
    name: &str,
    pos: Vector2,
    heading: f64,
) -> Result<GroupId> {
    let spctx = SpawnCtx::new(lua)?;
    let specs = ctx
        .db
        .ephemeral
        .cfg
        .troops
        .get(&side)
        .ok_or_else(|| anyhow!("no troops on {side}"))?;

    let spec = specs
        .iter()
        .find(|tr| tr.name.as_str() == name)
        .ok_or_else(|| anyhow!("no troop called {name} on {side}"))?
        .clone();

    let loc = SpawnLoc::AtPos {
        pos,
        offset_direction: pointing_towards2(heading),
        group_heading: heading,
    };

    let origin = DeployKind::Troop {
        player: Ucid::default(),
        moved_by: None,
        spec: spec.clone(),
        origin: None,
        cost_fraction: 1.,
        jtac: None,
    };

    let gid = ctx.db.add_and_queue_group(
        &spctx,
        &ctx.idx,
        side,
        loc,
        &spec.template,
        origin,
        BitFlags::empty(),
        None,
    )?;
    Ok(gid)
}

pub(crate) fn api_move_group(ctx: &mut Context, lua: MizLua, id: &GroupId, pos: Vector2) -> Result<()> {
    use dcso3::controller::{MissionPoint, PointType, ActionTyp, AltType, Task, VehicleFormation};
    use dcso3::group::Group;
    use dcso3::LuaVec2;

    let group = ctx.db.group(id)?;
    let dcs_group = Group::get_by_name(lua, group.name.as_str())?;
    let controller = dcs_group.get_controller()?;

    // Create a simple move waypoint for ground units
    let waypoint = MissionPoint {
        typ: PointType::TurningPoint,
        airdrome_id: None,
        time_re_fu_ar: None,
        helipad: None,
        link_unit: None,
        action: Some(ActionTyp::Ground(VehicleFormation::OnRoad)),
        pos: LuaVec2(pos),
        alt: 0., // Ground level
        alt_typ: Some(AltType::RADIO),
        speed: 10., // 10 m/s default
        speed_locked: Some(false),
        eta: None,
        eta_locked: None,
        name: None,
        task: Box::new(Task::Hold),
    };

    let task = Task::Mission {
        airborne: Some(false),
        route: vec![waypoint],
    };
    controller.set_task(task)?;
    Ok(())
}

pub(crate) fn api_add_points(ctx: &mut Context, player: &str, amount: i32, reason: &str) -> Result<()> {
    let ucid = get_player_ucid(ctx, player)?;
    let player_data = ctx
        .db
        .player_mut(&ucid)
        .ok_or_else(|| anyhow!("no such player {player}"))?;

    player_data.points += amount;
    ctx.db.ephemeral.dirty();

    // Log the points change
    ctx.db.ephemeral.stat(Stat::Points {
        id: ucid,
        points: amount,
        reason: String::from(reason),
    });

    Ok(())
}

#[derive(Debug)]
pub(super) enum Caller {
    Player(PlayerId),
    External(oneshot::Sender<NetIdxValue>),
}

pub(super) fn run_admin_commands(ctx: &mut Context, lua: MizLua) -> Result<AdminResult> {
    let mut cmds = mem::take(&mut ctx.admin_commands);
    while let Some((cmd, ch)) = ctx.external_admin_commands.pop() {
        cmds.push((Caller::External(ch), cmd));
    }
    let mut result = AdminResult::Continue;
    for (caller, cmd) in cmds.drain(..) {
        let mut replies: SmallVec<[NetIdxValue; 4]> = smallvec![];
        macro_rules! reply_ok {
            ($($arg:expr),+) => {
                match caller {
                    Caller::Player(id) => {
                        ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), format_compact!($($arg),+))
                    },
                    Caller::External(_) => {
                        replies.push(NetIdxValue::from(format!($($arg),+)));
                    }
                }

            }
        }
        macro_rules! reply_err {
            ($($arg:expr),+) => {
                match caller {
                    Caller::Player(id) => {
                        ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), format_compact!($($arg),+))
                    },
                    Caller::External(_) => {
                        replies.push(NetIdxValue::Error(format!($($arg),+).into()));
                    }
                }

            }
        }
        macro_rules! airbase {
            ($name:expr) => {
                match get_airbase(&ctx.db, $name) {
                    Ok(oid) => oid,
                    Err(e) => {
                        reply_err!("{e:?}");
                        continue;
                    }
                }
            };
        }
        match cmd {
            AdminCommand::Help => (),
            AdminCommand::ReduceInventory { airbase, amount } => {
                match ctx
                    .db
                    .admin_reduce_inventory(lua, airbase!(&airbase), amount)
                {
                    Err(e) => reply_err!("reduce inventory failed: {:?}", e),
                    Ok(()) => reply_ok!("inventory reduced"),
                }
            }
            AdminCommand::TransferSupply { from, to } => {
                let from = airbase!(&from);
                let to = airbase!(&to);
                match ctx.db.transfer_supplies(lua, from, to) {
                    Err(e) => reply_err!("transfer inventory failed {:?}", e),
                    Ok(()) => reply_ok!("transfer complete. disconnect"),
                }
            }
            AdminCommand::LogisticsTickNow => {
                ctx.db.admin_tick_now();
                reply_ok!("tick scheduled")
            }
            AdminCommand::LogisticsDeliverNow => {
                ctx.db.admin_deliver_now();
                reply_ok!("delivery scheduled")
            }
            AdminCommand::Repair { airbase } => {
                match ctx.db.repair_objective(airbase!(&airbase), Utc::now()) {
                    Ok(()) => reply_ok!("repaired {airbase}"),
                    Err(e) => reply_ok!("failed to repair {e:?}"),
                }
            }
            AdminCommand::Capture { objective, side } => {
                let oid = airbase!(&objective);
                match ctx.db.force_capture(lua, &ctx.idx, oid, side, Utc::now()) {
                    Ok(prev) => reply_ok!("{objective}: {prev:?} -> {side:?}"),
                    Err(e) => reply_err!("capture failed: {e:?}"),
                }
            }
            AdminCommand::Tim { key, size, alt } => {
                let mut to_remove: SmallVec<[MarkId; 8]> = smallvec![];
                let act = Trigger::singleton(lua)?.action()?;
                for mk in World::singleton(lua)?
                    .get_mark_panels()
                    .context("getting marks")?
                {
                    let mut mk = mk?;
                    if mk.text == key {
                        to_remove.push(mk.id);
                        if let Some(alt) = alt {
                            mk.pos.y = alt as f64;
                        }
                        act.explosion(mk.pos, size as f32)
                            .context("making boom beserker!")?;
                    }
                }
                for id in to_remove {
                    ctx.db.ephemeral.msgs().delete_mark(id);
                }
            }
            AdminCommand::Spawn { key } => {
                let id = match &caller {
                    Caller::Player(id) => Some(*id),
                    Caller::External(_) => None,
                };
                if let Err(e) = admin_spawn(ctx, lua, id, key) {
                    reply_ok!("could not spawn {:?}", e)
                }
            }
            AdminCommand::SideSwitch { side, player } => {
                if let Err(e) = admin_sideswitch(ctx, side, player.clone()) {
                    reply_err!("could not sideswitch {:?}", e)
                } else {
                    reply_ok!("{player} sideswitched to {side}")
                }
            }
            AdminCommand::Ban { player, until } => match admin_ban(ctx, lua, until, &player) {
                Ok(()) => reply_ok!("{player} banned until {:?}", until),
                Err(e) => reply_err!("could not ban {player}, {:?}", e),
            },
            AdminCommand::Unban { player } => match admin_unban(ctx, &player) {
                Ok(()) => reply_ok!("{player} unbanned"),
                Err(e) => reply_err!("could not unban {}, {:?}", player, e),
            },
            AdminCommand::Kick { player } => match admin_kick(ctx, lua, &player) {
                Ok(()) => reply_ok!("{player} kicked"),
                Err(e) => reply_err!("could not kick {player}, {:?}", e),
            },
            AdminCommand::Banned => {
                for (ucid, name, until) in admin_list_banned(ctx) {
                    reply_ok!("{ucid} \"{name}\" {:?}", until)
                }
            }
            AdminCommand::Connected => {
                for (pid, ucid, name) in admin_list_connected(ctx) {
                    reply_ok!("{pid} {ucid} {name}")
                }
            }
            AdminCommand::Search { expr } => {
                for (pid, ucid, names) in admin_search(ctx, expr) {
                    match pid {
                        None => reply_err!("{ucid} {:?}", names),
                        Some(pid) => reply_ok!("{pid} {ucid} {:?}", names),
                    }
                }
            }
            AdminCommand::LogWarehouse { kind, airbase } => {
                match ctx.db.admin_log_inventory(lua, kind, airbase!(&airbase)) {
                    Ok(()) => reply_err!("{airbase} inventory logged"),
                    Err(e) => reply_ok!("could not log {airbase} inventory {:?}", e),
                }
            }
            AdminCommand::Logdesc => match &caller {
                Caller::External(_) => reply_err!("external clients can't be in a plane"),
                Caller::Player(id) => match ctx.connected.get(&id) {
                    None => reply_err!("no player {id}"),
                    Some(ifo) => match admin_log_desc(ctx, lua, &ifo.ucid) {
                        Ok(()) => reply_ok!("{} desc logged", ifo.ucid),
                        Err(e) => reply_err!("could not log admin desc {:?}", e),
                    },
                },
            },
            AdminCommand::ResetLives { player } => match admin_reset_lives(ctx, &player) {
                Ok(()) => reply_ok!("{player} lives reset"),
                Err(e) => reply_err!("could not reset {player} lives {:?}", e),
            },
            AdminCommand::Shutdown => match admin_shutdown(ctx, lua, None) {
                Ok(s) => {
                    result = s;
                    reply_ok!("shutting down")
                }
                Err(e) => reply_err!("failed to shutdown {:?}", e),
            },
            AdminCommand::AddAdmin { player } => match add_admin(ctx, &player) {
                Ok(()) => reply_ok!("{player} is now an admin"),
                Err(e) => reply_err!("failed to make {player} an admin {e:?}"),
            },
            AdminCommand::RemoveAdmin { player } => match remove_admin(ctx, &player) {
                Ok(()) => reply_ok!("{player} is no longer an admin"),
                Err(e) => reply_err!("failed to remove {player} from the admin list {e:?}"),
            },
            AdminCommand::Balance { player } => match balance(ctx, &player) {
                Ok(b) => reply_ok!("{player}'s balance is {b}"),
                Err(e) => reply_err!("could not get {player}'s balance {e:?}"),
            },
            AdminCommand::SetPoints { amount, player } => match set_points(ctx, &player, amount) {
                Ok(()) => reply_ok!("{player}'s points set to {amount}"),
                Err(e) => reply_err!("could not set {player}'s points {e:?}"),
            },
            AdminCommand::Delete { group } => match delete(ctx, &group) {
                Ok(()) => reply_ok!("{group} deleted"),
                Err(e) => reply_err!("could not delete group {e:?}"),
            },
            AdminCommand::Deslot { player } => match deslot(ctx, &player) {
                Ok(()) => reply_ok!("{player} deslotted"),
                Err(e) => reply_err!("could not deslot {player} {e:?}"),
            },
            AdminCommand::Remark { objective } => match remark(ctx, &objective) {
                Ok(()) => reply_ok!("{objective} remark queued"),
                Err(e) => reply_err!("could not remark {objective} {e:?}"),
            },
            AdminCommand::Reset { winner } => match admin_shutdown(ctx, lua, Some(winner)) {
                Ok(s) => {
                    result = s;
                    reply_ok!("the state has been reset");
                }
                Err(e) => reply_err!("the state could not be reset {e:?}"),
            },
            // Query API commands
            AdminCommand::QueryObjectives => {
                let objectives = query_objectives(ctx);
                match serde_json::to_string(&objectives) {
                    Ok(json) => replies.push(NetIdxValue::from(json)),
                    Err(e) => reply_err!("failed to serialize objectives: {e:?}"),
                }
            }
            AdminCommand::QueryObjective { name } => {
                match query_objective_details(ctx, &name) {
                    Ok(details) => match serde_json::to_string(&details) {
                        Ok(json) => replies.push(NetIdxValue::from(json)),
                        Err(e) => reply_err!("failed to serialize objective: {e:?}"),
                    },
                    Err(e) => reply_err!("failed to query objective: {e:?}"),
                }
            }
            AdminCommand::QueryPlayers => {
                let players = query_players(ctx);
                match serde_json::to_string(&players) {
                    Ok(json) => replies.push(NetIdxValue::from(json)),
                    Err(e) => reply_err!("failed to serialize players: {e:?}"),
                }
            }
            AdminCommand::QueryPlayer { player } => {
                match query_player_details(ctx, &player) {
                    Ok(details) => match serde_json::to_string(&details) {
                        Ok(json) => replies.push(NetIdxValue::from(json)),
                        Err(e) => reply_err!("failed to serialize player: {e:?}"),
                    },
                    Err(e) => reply_err!("failed to query player: {e:?}"),
                }
            }
            AdminCommand::QueryGroups { side } => {
                let groups = query_groups(ctx, side);
                match serde_json::to_string(&groups) {
                    Ok(json) => replies.push(NetIdxValue::from(json)),
                    Err(e) => reply_err!("failed to serialize groups: {e:?}"),
                }
            }
            AdminCommand::QueryGroup { id } => {
                match query_group_details(ctx, &id) {
                    Ok(details) => match serde_json::to_string(&details) {
                        Ok(json) => replies.push(NetIdxValue::from(json)),
                        Err(e) => reply_err!("failed to serialize group: {e:?}"),
                    },
                    Err(e) => reply_err!("failed to query group: {e:?}"),
                }
            }
            AdminCommand::QueryUnits { group } => {
                match query_units(ctx, &group) {
                    Ok(units) => match serde_json::to_string(&units) {
                        Ok(json) => replies.push(NetIdxValue::from(json)),
                        Err(e) => reply_err!("failed to serialize units: {e:?}"),
                    },
                    Err(e) => reply_err!("failed to query units: {e:?}"),
                }
            }
            AdminCommand::QueryWarehouse { objective } => {
                match query_warehouse(ctx, &objective) {
                    Ok(warehouse) => match serde_json::to_string(&warehouse) {
                        Ok(json) => replies.push(NetIdxValue::from(json)),
                        Err(e) => reply_err!("failed to serialize warehouse: {e:?}"),
                    },
                    Err(e) => reply_err!("failed to query warehouse: {e:?}"),
                }
            }
            AdminCommand::QueryLogistics => {
                let logistics = query_logistics(ctx);
                match serde_json::to_string(&logistics) {
                    Ok(json) => replies.push(NetIdxValue::from(json)),
                    Err(e) => reply_err!("failed to serialize logistics: {e:?}"),
                }
            }
            AdminCommand::QueryCampaignState => {
                let state = query_campaign_state(ctx);
                match serde_json::to_string(&state) {
                    Ok(json) => replies.push(NetIdxValue::from(json)),
                    Err(e) => reply_err!("failed to serialize campaign state: {e:?}"),
                }
            }
            AdminCommand::QueryPerf => {
                match serde_json::to_string(&query_perf()) {
                    Ok(json) => replies.push(NetIdxValue::from(json)),
                    Err(e) => reply_err!("failed to serialize perf: {e:?}"),
                }
            }
            AdminCommand::QueryBriefing { side } => {
                let briefing = query_briefing(ctx, lua, side);
                match serde_json::to_string(&briefing) {
                    Ok(json) => replies.push(NetIdxValue::from(json)),
                    Err(e) => reply_err!("failed to serialize briefing: {e:?}"),
                }
            }
            // Action API commands
            AdminCommand::SpawnDeployable { side, name, pos, heading } => {
                match api_spawn_deployable(ctx, lua, side, &name, pos, heading) {
                    Ok(gid) => reply_ok!("{{\"success\":true,\"group_id\":{}}}", gid),
                    Err(e) => reply_err!("failed to spawn deployable: {e:?}"),
                }
            }
            AdminCommand::SpawnTroop { side, name, pos, heading } => {
                match api_spawn_troop(ctx, lua, side, &name, pos, heading) {
                    Ok(gid) => reply_ok!("{{\"success\":true,\"group_id\":{}}}", gid),
                    Err(e) => reply_err!("failed to spawn troop: {e:?}"),
                }
            }
            AdminCommand::MoveGroup { id, pos } => {
                match api_move_group(ctx, lua, &id, pos) {
                    Ok(()) => reply_ok!("{{\"success\":true}}"),
                    Err(e) => reply_err!("failed to move group: {e:?}"),
                }
            }
            AdminCommand::AddPoints { player, amount, reason } => {
                match api_add_points(ctx, &player, amount, &reason) {
                    Ok(()) => {
                        let new_balance = ctx.db.player(&get_player_ucid(ctx, &player).unwrap_or_default())
                            .map_or(0, |p| p.points);
                        reply_ok!("{{\"success\":true,\"new_balance\":{}}}", new_balance)
                    },
                    Err(e) => reply_err!("failed to add points: {e:?}"),
                }
            }
            AdminCommand::SetObjectivePriority { objective, priority } => {
                match get_airbase(&ctx.db, &objective) {
                    Err(e) => reply_err!("objective not found: {e:?}"),
                    Ok(oid) => match ctx.db.set_objective_priority(&oid, priority) {
                        Ok(()) => reply_ok!("{{\"success\":true,\"priority\":{}}}", priority),
                        Err(e) => reply_err!("failed to set priority: {e:?}"),
                    },
                }
            }
            AdminCommand::Blacklist { rule, player } => {
                match get_player_ucid(ctx, &player) {
                    Err(e) => reply_err!("player not found: {e:?}"),
                    Ok(ucid) => {
                        let name = ctx.db.player(&ucid).map(|p| p.name.clone()).unwrap_or_default();
                        let cfg = Arc::make_mut(&mut ctx.db.ephemeral.cfg);
                        let rules = &mut cfg.rules;
                        let target: Option<&mut Rule> = match rule.as_str() {
                            "actions" => Some(&mut rules.actions),
                            "cargo" => Some(&mut rules.cargo),
                            "troops" => Some(&mut rules.troops),
                            "jtac" => Some(&mut rules.jtac),
                            "ca" => Some(&mut rules.ca),
                            _ => None,
                        };
                        match target {
                            None => reply_err!("unknown rule {rule}, expected: actions|cargo|troops|jtac|ca"),
                            Some(r) => { r.blacklist(ucid, name.into()); reply_ok!("{player} blacklisted from {rule}") }
                        }
                    }
                }
            }
            AdminCommand::Whitelist { rule, player } => {
                match get_player_ucid(ctx, &player) {
                    Err(e) => reply_err!("player not found: {e:?}"),
                    Ok(ucid) => {
                        let name = ctx.db.player(&ucid).map(|p| p.name.clone()).unwrap_or_default();
                        let cfg = Arc::make_mut(&mut ctx.db.ephemeral.cfg);
                        let rules = &mut cfg.rules;
                        let target: Option<&mut Rule> = match rule.as_str() {
                            "actions" => Some(&mut rules.actions),
                            "cargo" => Some(&mut rules.cargo),
                            "troops" => Some(&mut rules.troops),
                            "jtac" => Some(&mut rules.jtac),
                            "ca" => Some(&mut rules.ca),
                            _ => None,
                        };
                        match target {
                            None => reply_err!("unknown rule {rule}, expected: actions|cargo|troops|jtac|ca"),
                            Some(r) => { r.whitelist(ucid, name.into()); reply_ok!("{player} whitelisted for {rule}") }
                        }
                    }
                }
            }
            AdminCommand::ReinitWarehouse { airbase } => {
                match get_airbase(&ctx.db, &airbase) {
                    Err(e) => reply_err!("airbase not found: {e:?}"),
                    Ok(oid) => match ctx.db.reinit_objective_warehouse(oid) {
                        Ok(()) => reply_ok!("warehouse reinitialized for {airbase}"),
                        Err(e) => reply_err!("failed to reinit warehouse: {e:?}"),
                    }
                }
            }
            // Cockpit UI API commands
            AdminCommand::ResolvePlayerId { id } => match ctx.connected.get(&id) {
                Some(ifo) => reply_ok!("{}", ifo.ucid),
                None => reply_err!("player {id} is not currently connected"),
            },
            AdminCommand::EwrToggle { ucid } => {
                let enabled = crate::menu::ewr::ewr_toggle_for(ctx, &ucid);
                reply_ok!("{}", if enabled { "enabled" } else { "disabled" })
            }
            AdminCommand::EwrReport { ucid, friendly } => {
                let report = crate::menu::ewr::build_braa_report(ctx, &ucid, friendly);
                reply_ok!("{report}")
            }
            AdminCommand::EwrSetUnits { ucid, imperial } => {
                crate::menu::ewr::ewr_set_units_for(ctx, &ucid, imperial);
                reply_ok!("{}", if imperial { "Imperial" } else { "Metric" })
            }
            AdminCommand::EwrGroundIntel { ucid } => {
                let report = crate::menu::ewr::build_ground_intel_report(ctx, &ucid);
                reply_ok!("{report}")
            }
            AdminCommand::CarpSolve { mark_key, drop_altitude_agl_ft } => {
                match crate::carp::build_carp_solution_from_mark(lua, &mark_key, drop_altitude_agl_ft) {
                    Ok(solution) => match serde_json::to_string(&solution) {
                        Ok(json) => reply_ok!("{json}"),
                        Err(e) => reply_err!("failed to serialize carp solution: {e:?}"),
                    },
                    Err(e) => reply_err!("carp solve failed: {:?}", e),
                }
            }
            AdminCommand::CarpSolveLatLon { lat, lon, drop_altitude_agl_ft } => {
                match crate::carp::build_carp_solution_from_latlon(lua, lat, lon, drop_altitude_agl_ft) {
                    Ok(solution) => match serde_json::to_string(&solution) {
                        Ok(json) => reply_ok!("{json}"),
                        Err(e) => reply_err!("failed to serialize carp solution: {e:?}"),
                    },
                    Err(e) => reply_err!("carp solve failed: {:?}", e),
                }
            }
            AdminCommand::CockpitSpawnCrate { ucid, crate_name, qty, c130 } => {
                let auto_unpack = if c130 {
                    true
                } else {
                    ctx.db.ephemeral.cfg.helo_cargo.as_ref().map(|c| c.auto_unpack).unwrap_or(false)
                };
                match crate::menu::cargo::spawn_crates_for_ucid(ctx, lua, &ucid, &crate_name, qty, auto_unpack) {
                    Ok(msg) => reply_ok!("{msg}"),
                    Err(e) => reply_err!("{e:?}"),
                }
            }
            AdminCommand::SetServerInfo { restart_at, weather } => {
                ctx.shutdown = restart_at.map(crate::AutoShutdown::new);
                ctx.bot_weather = weather;
                reply_ok!("server info updated");
            }
        }
        match caller {
            Caller::Player(_) => (),
            Caller::External(ch) => {
                if replies.len() == 1 {
                    if let Some(reply) = replies.pop() {
                        let _ = ch.send(reply);
                    }
                } else {
                    let _ = ch.send(NetIdxValue::from(replies));
                }
            }
        }
    }
    ctx.admin_commands = cmds;
    Ok(result)
}
