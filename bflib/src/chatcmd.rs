use crate::{
    Context,
    admin::{self, AdminCommand, Caller},
    bg::Task,
    db::{actions::ActionCmd, group::DeployKind},
    jtac::JtId,
    lives,
    menu::{self, ArgQuad, ArgTriple, ArgTuple},
    msgq::MsgTyp,
    spawnctx::SpawnCtx,
};
use anyhow::{Context as ErrContext, Result, anyhow, bail};
use bfprotocols::{
    cfg::{Action, ActionKind},
    db::group::GroupId,
    perf::PerfInner,
    stats::Stat,
};
use chrono::{Duration, prelude::*};
use compact_str::{CompactString, format_compact};
use dcso3::{
    HooksLua, MizLua, String, Vector2,
    coalition::Side,
    net::{Net, PlayerId, Ucid},
    world::World,
};
use fxhash::FxBuildHasher;
use indexmap::IndexMap;
use log::{error, info};
use netidx::utils::Either;
use regex::Regex;
use smallvec::{SmallVec, smallvec};
use std::{mem, sync::Arc, sync::OnceLock};

pub(crate) fn register_success(ctx: &mut Context, id: PlayerId, name: String, side: Side) {
    let msg = String::from(format_compact!(
        "Welcome to the {:?} team. You may only occupy slots belonging to your team. Good luck!",
        side
    ));
    ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg);
    ctx.db.ephemeral.msgs().send(
        MsgTyp::Chat(None),
        format_compact!("{} has joined {:?} team", name, side),
    );
}

pub(crate) fn sideswitch_success(ctx: &mut Context, name: String, side: Side) {
    let msg = String::from(format_compact!("{} has switched to {:?}", name, side));
    ctx.db.ephemeral.msgs().send(MsgTyp::Chat(None), msg);
}

fn sideswitch_player(
    ctx: &mut Context,
    lua: HooksLua,
    id: PlayerId,
    msg: String,
) -> Result<String> {
    let ifo = ctx.connected.get_or_lookup_player_info(lua, id)?;
    let (_, slot) = Net::singleton(lua)?.get_slot(id)?;
    if !slot.is_spectator() {
        bail!("you must be in spectators to switch sides")
    }
    let side = if msg.eq_ignore_ascii_case("-switch blue") {
        Side::Blue
    } else if msg.eq_ignore_ascii_case("-switch red") {
        Side::Red
    } else {
        bail!("side must be blue or red \"{msg}\"");
    };
    match ctx.db.sideswitch_player(&ifo.ucid, side) {
        Ok(()) => {
            let name = ifo.name.clone();
            sideswitch_success(ctx, name, side);
        }
        Err(e) => ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), e),
    }
    Ok("".into())
}

fn lives_command(ctx: &mut Context, id: PlayerId) -> Result<()> {
    let ifo = ctx
        .connected
        .get(&id)
        .ok_or_else(|| anyhow!("missing info for player {:?}", id))?;
    let msg = lives(&mut ctx.db, &ifo.ucid, None)?;
    ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg);
    Ok(())
}

fn gci_command(ctx: &mut Context, id: PlayerId, arg: &str) {
    use crate::ewr::EwrUnits;
    let Some(ifo) = ctx.connected.get(&id) else { return };
    let ucid = ifo.ucid.clone();
    let reply = |ctx: &mut Context, m: CompactString| {
        ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), m);
    };
    match arg.trim().to_lowercase().as_str() {
        "" | "status" => {
            let (enabled, units, refm) = ctx.ewr.gci_prefs(&ucid);
            let auto = ctx.ewr.gci_auto(&ucid);
            let u = match units {
                Some(EwrUnits::Metric) => "metric",
                Some(EwrUnits::Imperial) => "imperial",
                None => "server default",
            };
            let r = match refm {
                Some(0) => "BRAA (own jet)",
                Some(1) => "bullseye",
                Some(2) => "clock",
                _ => "server default",
            };
            reply(
                ctx,
                format_compact!(
                    "GCI voice: {} | auto callouts: {} | units: {} | reference: {}
  -gci on | off              GCI on or off entirely
  -gci callouts | quiet      unprompted calls on or off
  -gci metric | imperial     spoken units
  -gci braa | bulls | clock   position reference
  -gci auto                  follow the server defaults",
                    if enabled { "ON" } else { "OFF" },
                    if auto { "ON" } else { "OFF" },
                    u,
                    r
                ),
            );
        }
        "on" | "off" => {
            let want_on = arg.trim().eq_ignore_ascii_case("on");
            let now = ctx.ewr.gci_prefs(&ucid).0;
            if now != want_on {
                ctx.ewr.gci_toggle(&ucid);
            }
            reply(
                ctx,
                format_compact!("GCI voice calls {}", if want_on { "enabled" } else { "disabled" }),
            );
        }
        "metric" => {
            ctx.ewr.gci_set_units(&ucid, Some(EwrUnits::Metric));
            reply(ctx, "GCI calls will use metric units".into());
        }
        "imperial" => {
            ctx.ewr.gci_set_units(&ucid, Some(EwrUnits::Imperial));
            reply(ctx, "GCI calls will use imperial units".into());
        }
        "braa" | "self" => {
            ctx.ewr.gci_set_reference(&ucid, Some(0));
            reply(ctx, "GCI calls will use BRAA from your aircraft".into());
        }
        "bulls" | "bullseye" => {
            ctx.ewr.gci_set_reference(&ucid, Some(1));
            reply(ctx, "GCI calls will use bullseye reference".into());
        }
        "clock" => {
            ctx.ewr.gci_set_reference(&ucid, Some(2));
            reply(ctx, "GCI calls will use clock position".into());
        }
        "auto" | "default" => {
            ctx.ewr.gci_set_units(&ucid, None);
            ctx.ewr.gci_set_reference(&ucid, None);
            reply(ctx, "GCI calls will use the server default units and reference".into());
        }
        // Unprompted calls. Separate from on/off: with callouts off the
        // controller still answers when you key up, it just never speaks first.
        "callouts" | "callouts on" | "loud" => {
            ctx.ewr.gci_set_auto(&ucid, true);
            reply(ctx, "GCI will call you unprompted".into());
        }
        "callouts off" | "quiet" | "silent" => {
            ctx.ewr.gci_set_auto(&ucid, false);
            reply(
                ctx,
                "GCI will stay quiet unless you call it - key up and ask for a bogey dope or picture"
                    .into(),
            );
        }
        other => {
            reply(ctx, format_compact!("unknown -gci option '{other}' (try: on, off, metric, imperial, braa, bulls, clock, auto, callouts, quiet)"));
        }
    }
}

/// How long a chat `reset`/`shutdown` waits for its `confirm`.
const CONFIRM_WINDOW_SECS: i64 = 30;

/// Chat `reset`/`shutdown` commands waiting for their `confirm`: (admin, the
/// normalised command text, when it was asked). Chat is handled on the single
/// DCS thread; the mutex only makes the static safe to hold.
static PENDING_CONFIRM: std::sync::Mutex<Vec<(Ucid, CompactString, DateTime<Utc>)>> =
    std::sync::Mutex::new(Vec::new());

/// Split a trailing `confirm` off an admin command.
fn strip_confirm(cmd: &str) -> (&str, bool) {
    let t = cmd.trim();
    match t.rsplit_once(char::is_whitespace) {
        Some((head, last)) if last.eq_ignore_ascii_case("confirm") => (head.trim_end(), true),
        _ => (t, false),
    }
}

fn admin_command(ctx: &mut Context, id: PlayerId, cmd: &str, now: DateTime<Utc>) {
    let ifo = match ctx.connected.get(&id) {
        Some(ifo) => ifo,
        None => return,
    };
    if !ctx.db.ephemeral.cfg.admins.contains_key(&ifo.ucid) {
        return;
    }
    // `confirm` only means something on reset/shutdown; anywhere else it is
    // just part of the arguments.
    let (text, confirmed, parsed) = match strip_confirm(cmd) {
        (text, true) => match text.parse::<AdminCommand>() {
            p @ Ok(AdminCommand::Reset { .. } | AdminCommand::Shutdown) => (text, true, p),
            _ => (cmd.trim(), false, cmd.parse::<AdminCommand>()),
        },
        (text, false) => (text, false, text.parse::<AdminCommand>()),
    };
    match parsed {
        Err(e) => ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            format_compact!("parse error {:?}", e),
        ),
        Ok(AdminCommand::Help) => {
            for cmd in AdminCommand::help() {
                ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), *cmd);
            }
        }
        // A one-word typo away from ending the round for everyone, so these
        // two need saying twice. Only from chat: bfdb and the bot send them
        // over RPC on purpose, from their own confirmed UI.
        Ok(cmd @ (AdminCommand::Reset { .. } | AdminCommand::Shutdown)) => {
            let ucid = ifo.ucid;
            let key = CompactString::from(
                text.split_whitespace().collect::<Vec<_>>().join(" ").to_ascii_lowercase(),
            );
            let mut pending = PENDING_CONFIRM.lock().unwrap_or_else(|e| e.into_inner());
            pending.retain(|(_, _, at)| now - *at <= Duration::seconds(CONFIRM_WINDOW_SECS));
            if confirmed {
                match pending.iter().position(|(u, k, _)| u == &ucid && k == &key) {
                    Some(i) => {
                        pending.remove(i);
                        drop(pending);
                        info!("queueing confirmed admin command {:?} from {:?}", cmd, ifo);
                        ctx.admin_commands.push((Caller::Player(id), cmd))
                    }
                    None => ctx.db.ephemeral.msgs().send(
                        MsgTyp::Chat(Some(id)),
                        format_compact!(
                            "nothing to confirm -- send -admin {key} first, then -admin {key} confirm within {CONFIRM_WINDOW_SECS}s"
                        ),
                    ),
                }
            } else {
                pending.retain(|(u, _, _)| u != &ucid);
                pending.push((ucid, key.clone(), now));
                drop(pending);
                let what = match cmd {
                    AdminCommand::Shutdown => "shut the server down",
                    _ => "shut the server down AND reset the whole campaign",
                };
                ctx.db.ephemeral.msgs().send(
                    MsgTyp::Chat(Some(id)),
                    format_compact!(
                        "-admin {key} will {what}. Send -admin {key} confirm within {CONFIRM_WINDOW_SECS}s to go ahead"
                    ),
                )
            }
        }
        Ok(cmd) => {
            info!("queueing admin command {:?} from {:?}", cmd, ifo);
            ctx.admin_commands.push((Caller::Player(id), cmd))
        }
    }
}

pub(super) fn format_duration(d: Duration) -> CompactString {
    let hrs = d.num_hours();
    let min = d.num_minutes() - hrs * 60;
    let sec = d.num_seconds() - hrs * 3600 - min * 60;
    format_compact!("{:02}:{:02}:{:02}", hrs, min, sec)
}

fn time_command(ctx: &mut Context, id: PlayerId, now: DateTime<Utc>) {
    match ctx.shutdown.as_ref() {
        None => ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            "The server isn't configured to restart automatically",
        ),
        Some(asd) => {
            let remains = format_duration(asd.when - now);
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!("The server will shutdown in {remains}"),
            )
        }
    }
}

fn weather_command(ctx: &mut Context, _lua: HooksLua, id: PlayerId) {
    if let Some(bw) = ctx.bot_weather {
        let cover = if bw.cloud_density > 0.0 { format!("{:.0}/10", bw.cloud_density) } else { "clear".to_string() };
        let msg = format!(
            "SERVER WEATHER\nTemp: {:.0}\u{b0}C / {:.0}\u{b0}F\nSurface wind: {:03}\u{b0} at {:.0} kt\nVisibility: {:.0} km / {:.0} SM\nClouds: base {:.0} ft AGL, {}\nQNH: {:.0} hPa / {:.2} inHg",
            bw.temp_c, bw.temp_c * 1.8 + 32.0,
            bw.wind_from_deg as u32, bw.wind_speed_kts,
            bw.visibility_m / 1000.0, bw.visibility_m / 1609.34,
            bw.cloud_base_m * 3.281, cover,
            bw.qnh_hpa, bw.qnh_hpa / 33.8639,
        );
        ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg);
        return;
    }
    let Some(ifo) = ctx.connected.get(&id) else { return };
    let Some(player) = ctx.db.player(&ifo.ucid) else { return };
    let Some(slot) = player.current_slot.as_ref().map(|(slot, _)| *slot) else {
        ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            "You must be in a slot to request a weather report",
        );
        return;
    };
    ctx.weather_requests.push((id, slot));
}

fn balance_command(ctx: &mut Context, id: PlayerId) {
    if let Some(ifo) = ctx.connected.get(&id) {
        if let Some(player) = ctx.db.player(&ifo.ucid) {
            let points = player.points;
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!("You have {points} points"),
            );
        }
    }
}

fn status_command(ctx: &mut Context, id: PlayerId) {
    use std::fmt::Write;
    let Some(ifo) = ctx.connected.get(&id) else { return };
    let Some(player) = ctx.db.player(&ifo.ucid) else { return };
    let side = player.side;
    let points = player.points;
    let streak = player.kill_streak;
    let total_kills = player.total_kills;

    // Count objective ownership for both sides
    let (mut blue_owned, mut red_owned) = (0u32, 0u32);
    for (_, obj) in ctx.db.objectives() {
        match obj.owner {
            dcso3::coalition::Side::Blue => blue_owned += 1,
            dcso3::coalition::Side::Red => red_owned += 1,
            _ => {}
        }
    }

    // Count active convoys for the player's side
    let convoy_count = ctx.db.convoy_count_for_side(side);

    let mut msg = CompactString::new("");
    let _ = write!(
        msg,
        "=== CAMPAIGN STATUS ===\nSide: {:?} | Points: {} | Streak: {} | Career Kills: {}\nObjectives — Blue: {} | Red: {}\nActive {:?} Convoys: {}",
        side, points, streak, total_kills,
        blue_owned, red_owned,
        side, convoy_count
    );
    ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg);
}

/// `-brief` -- the condensed situational briefing on demand, the same text the
/// slot-entry panel shows. The full paged report is F10 > Info > Situation.
///
/// Only queued here. Chat runs in the hooks Lua state, which has no mission
/// `coord`/atmosphere/timer singletons; wrapping the hooks state as a MizLua
/// (what this used to do) built the report against globals that aren't there.
/// `run_brief_requests` answers it from the mission state, as `-weather` does.
fn brief_command(ctx: &mut Context, id: PlayerId) {
    let Some(ifo) = ctx.connected.get(&id) else { return };
    if ctx.db.player(&ifo.ucid).is_none() {
        ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            " you aren't registered yet -- take any slot on the side you want to fly",
        );
        return;
    }
    ctx.brief_requests.push(id);
}

/// Answer the queued `-brief` requests. Must be called with the mission Lua
/// state.
pub(super) fn run_brief_requests(ctx: &mut Context, lua: MizLua) {
    for id in mem::take(&mut ctx.brief_requests) {
        brief_reply(ctx, lua, id)
    }
}

fn brief_reply(ctx: &mut Context, lua: MizLua, id: PlayerId) {
    // The player may have left between asking and the next tick.
    let Some(ifo) = ctx.connected.get(&id) else { return };
    let ucid = ifo.ucid;
    let Some(side) = ctx.db.player(&ucid).map(|p| p.side) else { return };
    let from = ctx
        .db
        .player(&ucid)
        .and_then(|p| p.current_slot.as_ref().map(|(s, _)| s.clone()))
        .and_then(|slot| crate::menu::player_world_pos(ctx, &slot));
    let rep = crate::situation::build(
        ctx,
        lua,
        side,
        crate::situation::Opts { include_map: false, from },
    );
    let panel_tasks = ctx
        .db
        .ephemeral
        .cfg
        .situation_briefing
        .as_ref()
        .map(|c| c.panel_tasks)
        .unwrap_or(3);
    let text = crate::situation::render_panel(&rep, panel_tasks, None);
    for line in text.lines() {
        ctx.db
            .ephemeral
            .msgs()
            .send(MsgTyp::Chat(Some(id)), format_compact!(" {line}"));
    }
}

fn transfer_command(ctx: &mut Context, id: PlayerId, s: &str) {
    macro_rules! reply {
        ($msg:tt) => {
            ctx.db
                .ephemeral
                .msgs()
                .send(MsgTyp::Chat(Some(id)), format_compact!($msg))
        };
    }
    if let Some(ifo) = ctx.connected.get(&id) {
        let Some(side) = ctx.db.player(&ifo.ucid).map(|p| p.side) else {
            return reply!("you aren't registered yet -- take a slot first");
        };
        let is_admin = ctx.db.ephemeral.cfg.admins.contains_key(&ifo.ucid);
        match s.trim().split_once(" ") {
            None => reply!("transfer expected amount and target"),
            Some((amount, target)) => match amount.parse::<u32>() {
                Err(e) => reply!("transfer expected a number {e:?}"),
                Ok(amount) => match target.trim().strip_prefix("objective:") {
                    Some(objective_name) => match admin::get_airbase(&ctx.db, objective_name) {
                        Err(e) => reply!("could not transfer to {objective_name}, {e:?}"),
                        // Points banked at an enemy objective fund the enemy.
                        Ok(oid) if ctx.db.objective(&oid).ok().map(|o| o.owner) != Some(side) => {
                            reply!("{objective_name} isn't held by your side")
                        }
                        Ok(oid) => {
                            match ctx
                                .db
                                .transfer_points(&ifo.ucid, Either::Right(oid), amount)
                            {
                                Err(e) => reply!("transfer failed {e:?}"),
                                Ok(()) => reply!("transfer complete"),
                            }
                        }
                    },
                    None => {
                        let target = target.trim();
                        // Players get plain name matching; the admin resolver
                        // takes a regex and answers an ambiguous one with every
                        // candidate's ucid, which is not something to hand to
                        // anyone who types `-transfer 1 a`.
                        let resolved = if is_admin {
                            admin::get_player_ucid(ctx, target)
                        } else {
                            admin::find_player_by_name(ctx, target)
                        };
                        match resolved {
                            Err(e) => reply!("could not transfer to {target}, {e}"),
                            Ok(ucid) if ctx.db.player(&ucid).map(|p| p.side) != Some(side) => {
                                reply!("{target} isn't on your side")
                            }
                            Ok(ucid) => {
                                match ctx
                                    .db
                                    .transfer_points(&ifo.ucid, Either::Left(&ucid), amount)
                                {
                                    Err(e) => reply!("transfer failed {e:?}"),
                                    Ok(()) => reply!("transfer complete"),
                                }
                            }
                        }
                    }
                },
            },
        }
    }
}

fn delete_command(ctx: &mut Context, id: PlayerId, s: &str) {
    macro_rules! reply {
        ($msg:tt) => {
            ctx.db
                .ephemeral
                .msgs()
                .send(MsgTyp::Chat(Some(id)), format_compact!($msg))
        };
    }
    if let Some(ifo) = ctx.connected.get(&id) {
        let Some(side) = ctx.db.player(&ifo.ucid).map(|p| p.side) else {
            return reply!("you aren't registered yet -- take a slot first");
        };
        match s.trim().parse::<GroupId>() {
            Err(e) => reply!("delete expected a group id {e:?}"),
            Ok(id) => match ctx.db.group(&id) {
                Err(e) => reply!("could not get group {id} {e:?}"),
                // Ownership is by ucid and survives a side switch, so without
                // this a player who switched could reclaim the SAMs and JTACs
                // they left behind -- a free 50% refund that also strips the
                // side they just abandoned.
                Ok(group) if group.side != side => {
                    reply!("group {id} belongs to the other side")
                }
                Ok(group) => match &group.origin {
                    DeployKind::Crate { player, .. }
                    | DeployKind::Deployed { player, .. }
                    | DeployKind::Troop { player, .. }
                        if player != &ifo.ucid =>
                    {
                        reply!("group {id} wasn't deployed by you")
                    }
                    DeployKind::Action { .. } => reply!("can't delete an action group"),
                    DeployKind::Objective { .. } | DeployKind::ObjectiveDeprecated => {
                        reply!("can't delete an objective group")
                    }
                    DeployKind::Crate { .. } => match ctx.db.delete_group(&id) {
                        Err(e) => reply!("could not delete group {id} {e:?}"),
                        Ok(()) => reply!("deleted {id}"),
                    },
                    DeployKind::Deployed {
                        player,
                        spec,
                        moved_by: _,
                        cost_fraction,
                        origin,
                        jtac: _,
                    } => {
                        let player = player.clone();
                        let points = (spec.cost as f32 / 2.).ceil() as i32;
                        let cost_fraction = *cost_fraction;
                        let origin = *origin;
                        match ctx.db.delete_group(&id) {
                            Err(e) => reply!("could not delete group {id} {e:?}"),
                            Ok(()) => match origin {
                                None => {
                                    ctx.db.adjust_points(
                                        &player,
                                        points,
                                        &format_compact!("reclaimed {id}"),
                                    );
                                    reply!("deleted {id}")
                                }
                                Some(oid) => {
                                    ctx.db.refund_points(
                                        &player,
                                        oid,
                                        points as u32,
                                        cost_fraction,
                                        &format_compact!("reclaimed {id}"),
                                    );
                                    reply!("deleted {id}")
                                }
                            },
                        }
                    }
                    DeployKind::DownedPilot { .. } => {
                        reply!("can't delete a downed pilot this way")
                    }
                    DeployKind::Dismount { .. } => {
                        reply!("can't delete a dismount group this way")
                    }
                    DeployKind::Troop {
                        player,
                        spec,
                        moved_by: _,
                        origin,
                        cost_fraction,
                        ..
                    } => {
                        let player = player.clone();
                        let points = (spec.cost as f32 / 2.).ceil() as i32;
                        let cost_fraction = *cost_fraction;
                        let origin = *origin;
                        match ctx.db.delete_group(&id) {
                            Err(e) => reply!("could not delete group {id} {e:?}"),
                            Ok(()) => match origin {
                                None => {
                                    ctx.db.adjust_points(
                                        &player,
                                        points,
                                        &format_compact!("reclaimed {id}"),
                                    );
                                    reply!("deleted {id}")
                                }
                                Some(oid) => {
                                    ctx.db.refund_points(
                                        &player,
                                        oid,
                                        points as u32,
                                        cost_fraction,
                                        &format_compact!("reclaimed {id}"),
                                    );
                                    reply!("deleted {id}")
                                }
                            },
                        }
                    }
                },
            },
        }
    }
}

fn action_help(ctx: &mut Context, actions: &IndexMap<String, Action, FxBuildHasher>, id: PlayerId) {
    // Printed as the literal command, because players copy these lines: the
    // old `JTAC Drone: <key>` form was typed back as `-action "JTAC Drone: 1"`.
    ctx.db.ephemeral.msgs().send(
        MsgTyp::Chat(Some(id)),
        "<key> is the text of an F10 map mark you placed, e.g. -action JTAC Drone M1",
    );
    for (name, action) in actions {
        let msg = match &action.kind {
            ActionKind::Attackers(_) => Some(format_compact!(
                "-action {name} <key> | Spawn ai attackers. cost {}",
                action.cost
            )),
            ActionKind::AttackersWaypoint => Some(format_compact!(
                "-action {name} <group> <key> | Move ai attackers. cost {}",
                action.cost
            )),
            ActionKind::Sead(_) => Some(format_compact!(
                "-action {name} <key> | Spawn ai sead units. cost {}",
                action.cost
            )),
            ActionKind::SeadWaypoint => Some(format_compact!(
                "-action {name} <group> <key> | Move ai sead units. cost {}",
                action.cost
            )),
            ActionKind::Move(_) => Some(format_compact!(
                "-action {name} <group> <key> | Move a ground unit. cost {}",
                action.cost
            )),
            ActionKind::Rtb => Some(format_compact!(
                "-action {name} <group> <key> | RTB an air asset manually. cost {}",
                action.cost
            )),
            ActionKind::Awacs(_) => Some(format_compact!(
                "-action {name} <key> | Spawn an awacs at key, a mark point. cost {}",
                action.cost
            )),
            ActionKind::AwacsWaypoint => Some(format_compact!(
                "-action {name} <group> <key> | Move an awacs to key, a mark point. Group is the awacs group. cost {}",
                action.cost
            )),
            ActionKind::Bomber(_) => None,
            ActionKind::CruiseMissileSpawn(_) => Some(format_compact!(
                "-action {name} <key> | Spawn a cruise missile bomber at key, a mark point. cost {}",
                action.cost
            )),
            ActionKind::CruiseMissileWaypoint => Some(format_compact!(
                "-action {name} <group> <key> | Move a cruise missile bomber to key, a mark point. Group is the bomber group. cost {}",
                action.cost
            )),
            ActionKind::Deployable(d) => Some(format_compact!(
                "-action {name} <key> | Ai deploy a {} at key a mark point. cost {}",
                d.name,
                action.cost
            )),
            ActionKind::Drone(_) => Some(format_compact!(
                "-action {name} <key> | Spawn a drone at key a mark point. cost {}",
                action.cost
            )),
            ActionKind::DroneWaypoint => Some(format_compact!(
                "-action {name} <group> <key> | Move a drone to key, a mark point. Group is the drone group. cost {}",
                action.cost
            )),
            ActionKind::FighersWaypoint => Some(format_compact!(
                "-action {name} <group> <key> | Move an a figher group to key, a mark point. Group is the fighter group. cost {}",
                action.cost
            )),
            ActionKind::Fighters(_) => Some(format_compact!(
                "-action {name} <key> | Spawn ai fighters at key, a mark point. cost {}",
                action.cost
            )),
            ActionKind::LogisticsRepair(_) => Some(format_compact!(
                "-action {name} <objective> | Start a logistics repair mission to objective. cost {}",
                action.cost
            )),
            ActionKind::LogisticsTransfer(_) => Some(format_compact!(
                "-action {name} <from> <to> | Start a logistics transfer mission between from and to. cost {}",
                action.cost
            )),
            ActionKind::Nuke(_) => Some(format_compact!(
                "-action {name} <key> | Nuke key, a mark point. cost {}",
                action.cost
            )),
            ActionKind::Paratrooper(d) => Some(format_compact!(
                "-action {name} <key> | Drop {} troops at key, a mark point. cost {}",
                d.name,
                action.cost
            )),
            ActionKind::Tanker(_) => Some(format_compact!(
                "-action {name} <key> | Spawn a tanker at key, a mark point. cost {}",
                action.cost
            )),
            ActionKind::TankerWaypoint => Some(format_compact!(
                "-action {name} <group> <key> | Move a tanker to key. Group is the tanker group. cost {}",
                action.cost
            )),
            ActionKind::CarrierWaypoint => None,
            ActionKind::CarrierRepair => None,
            ActionKind::CarrierRespawn => None,
            ActionKind::NavalCruiseMissileStrike(_) => None,
            ActionKind::Artillery(_) => Some(format_compact!(
                "-action {name} <key> | Request artillery fire support at key, a mark point. cost {}",
                action.cost
            )),
            ActionKind::Recon(_) => Some(format_compact!(
                "-action {name} <key> | Dispatch a recon flight over key, a mark point. cost {}",
                action.cost
            )),
            ActionKind::AddTask(c) => Some(format_compact!(
                "-action {name} <type> <key> | Post a task at key, a mark point. types: {}. cost {}",
                c.types
                    .iter()
                    .map(|t| t.name.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
                action.cost
            )),
            ActionKind::RemoveTask(_) => Some(format_compact!(
                "-action {name} <task id> | Remove a task from the coalition board. cost {}",
                action.cost
            )),
        };
        if let Some(msg) = msg {
            ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg)
        }
    }
}

fn action_command(ctx: &mut Context, id: PlayerId, cmd: &str) {
    if cmd.trim().eq_ignore_ascii_case("help") {
        if let Some(ifo) = ctx.connected.get(&id) {
            if let Some(player) = ctx.db.player(&ifo.ucid) {
                let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
                if let Some(actions) = cfg.actions.get(&player.side) {
                    action_help(ctx, actions, id)
                }
            }
        }
    } else {
        ctx.action_commands.push((id, String::from(cmd)))
    }
}

pub(super) fn run_action_commands(
    ctx: &mut Context,
    perf: &mut PerfInner,
    lua: MizLua,
) -> Result<()> {
    let spctx = SpawnCtx::new(lua).context("creating spawn ctx")?;
    for (id, s) in ctx.action_commands.drain(..) {
        if let Some(ifo) = ctx.connected.get(&id) {
            if let Some(player) = ctx.db.player(&ifo.ucid) {
                let ucid = ifo.ucid.clone();
                let side = player.side;
                // The admin blacklist/whitelist only hid the F10 Actions menu;
                // the chat command went straight through.
                if !ctx.db.ephemeral.cfg.rules.actions.check(&ucid) {
                    ctx.db.ephemeral.msgs().send(
                        MsgTyp::Chat(Some(id)),
                        "you are not permitted to use actions on this server",
                    );
                    continue;
                }
                let r = match ActionCmd::parse(&mut ctx.db, lua, side, &s) {
                    Err(e) => Err(e),
                    Ok(cmd) => ctx.db.start_action(
                        lua,
                        perf,
                        &spctx,
                        &ctx.idx,
                        &ctx.jtac,
                        side,
                        Some(ucid),
                        cmd,
                    ),
                };
                let msg = match r {
                    Err(e) => format_compact!("could not run action {s}: {e:?}"),
                    Ok(()) => format_compact!("action {s} started"),
                };
                ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), msg)
            }
        }
    }
    Ok(())
}

fn bind_command(ctx: &mut Context, id: PlayerId, s: &str) {
    static RX: OnceLock<Regex> = OnceLock::new();
    match ctx.connected.get(&id) {
        None => ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            "I don't have your player info yet. Take a slot and try again.",
        ),
        Some(ifo) => {
            let rx = RX.get_or_init(|| {
                Regex::new("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
                    .unwrap()
            });
            let s = s.trim();
            if !rx.is_match(s) {
                // A player tried `-bind <group id>` expecting it to bind a
                // deployed group to the menu, got a bare "Invalid token", and
                // had no way to tell what it actually wanted. Say both halves.
                ctx.db.ephemeral.msgs().send(
                    MsgTyp::Chat(Some(id)),
                    "Invalid token -- -bind takes the UUID from the web dashboard login page",
                );
                ctx.db.ephemeral.msgs().send(
                    MsgTyp::Chat(Some(id)),
                    "it does not bind groups. Your own groups are listed first under F10 > Actions",
                )
            } else {
                // The bind is a fire-and-forget stat to bfdb; nothing comes
                // back to say whether the token matched, so don't claim it did.
                ctx.db.ephemeral.msgs().send(
                    MsgTyp::Chat(Some(id)),
                    "Bind request sent -- reload the dashboard to confirm your account is linked",
                );
                ctx.do_bg_task(Task::Stat(Stat::Bind {
                    id: ifo.ucid,
                    token: s.into(),
                }))
            }
        }
    }
}

fn jtac_command(ctx: &mut Context, id: PlayerId, s: &str) {
    if s.trim().eq_ignore_ascii_case("help") {
        ctx.db
            .ephemeral
            .msgs()
            .send(MsgTyp::Chat(Some(id)), " -jtac <id> autoshift");
        ctx.db
            .ephemeral
            .msgs()
            .send(MsgTyp::Chat(Some(id)), " -jtac <id> pointer");
        ctx.db
            .ephemeral
            .msgs()
            .send(MsgTyp::Chat(Some(id)), " -jtac <id> shift");
        ctx.db
            .ephemeral
            .msgs()
            .send(MsgTyp::Chat(Some(id)), " -jtac <id> status");
        ctx.db
            .ephemeral
            .msgs()
            .send(MsgTyp::Chat(Some(id)), " -jtac <id> smoke");
        ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            " -jtac <id> focus [<mark text>|clear]: lase near your latest (or the named) map mark",
        );
        ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            " -jtac <id> code <code>: a full code 1111-1788, e.g. code 1688",
        );
        ctx.db
            .ephemeral
            .msgs()
            .send(MsgTyp::Chat(Some(id)), " -jtac <id> arty <id|all> <n>");
        ctx.db
            .ephemeral
            .msgs()
            .send(MsgTyp::Chat(Some(id)), " -jtac <id> bomber [mission]");
    } else if let Some((jtid, cmd)) = s.trim().split_once(" ") {
        if let Ok(jtid) = jtid.parse::<JtId>() {
            ctx.jtac_commands.push((id, jtid, cmd.trim().into()));
        } else {
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!("invalid jtac id {jtid}"),
            );
        }
    } else {
        ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            "expected -jtac <id> <cmd>, see -jtac <help>",
        );
    }
}

fn run_jtac_command(
    ctx: &mut Context,
    lua: MizLua,
    id: PlayerId,
    jtid: JtId,
    cmd: String,
) -> Result<()> {
    macro_rules! error {
        ($msg:literal) => {error!($msg,)};
        ($msg:literal, $($arg:expr),*) => {{
            ctx.db
                .ephemeral
                .msgs()
                .send(MsgTyp::Chat(Some(id)), format_compact!($msg, $($arg),*));
            return Ok(());
        }};
    }
    let ucid = ctx
        .connected
        .get(&id)
        .ok_or_else(|| anyhow!("unknown player"))?
        .ucid;
    let side = match ctx.db.player(&ucid) {
        Some(player) => player.side,
        None => error!("no such player {ucid}"),
    };
    // Rules used to be applied only when the F10 JTAC menu was built, so a
    // blacklisted player could still drive every JTAC from chat.
    if !ctx.db.ephemeral.cfg.rules.jtac.check(&ucid) {
        error!("you are not permitted to command JTACs on this server")
    }
    let jtac = match ctx.jtac.get(&jtid) {
        Err(_) => error!("no such jtac {jtid}"),
        Ok(jtac) => {
            if jtac.side() != side {
                error!("you can't give orders to enemy jtacs")
            }
            jtac
        }
    };
    if let Some(_) = cmd.strip_prefix("autoshift") {
        let arg = ArgTuple {
            fst: ucid,
            snd: jtid,
        };
        menu::jtac::jtac_toggle_auto_shift(lua, arg)?;
    } else if let Some(_) = cmd.strip_prefix("shift") {
        let arg = ArgTuple {
            fst: ucid,
            snd: jtid,
        };
        menu::jtac::jtac_shift(lua, arg)?;
    } else if let Some(_) = cmd.strip_prefix("status") {
        let panel_to_side = ctx
            .db
            .player(&ucid)
            .map(|p| p.jtac_or_spectators)
            .unwrap_or(true);
        let arg = ArgTuple {
            fst: (!panel_to_side).then_some(ucid),
            snd: jtid,
        };
        menu::jtac::jtac_status(lua, arg)?
    } else if let Some(_) = cmd.strip_prefix("smoke") {
        let arg = ArgTuple {
            fst: ucid,
            snd: jtid,
        };
        menu::jtac::jtac_smoke_target(lua, arg)?
    } else if let Some(_) = cmd.strip_prefix("pointer") {
        let arg = ArgTuple {
            fst: ucid,
            snd: jtid,
        };
        menu::jtac::jtac_toggle_ir_pointer(lua, arg)?
    } else if let Some(s) = cmd.strip_prefix("bomber") {
        let name = s.trim();
        let name = if name != "" {
            Some(String::from(name))
        } else {
            let bomber_missions = ctx.db.ephemeral.cfg.actions.get(&side);
            bomber_missions.iter().find_map(|acts| {
                acts.iter().find_map(|(n, a)| match a.kind {
                    ActionKind::Bomber(_) => Some(n.clone()),
                    _ => None,
                })
            })
        };
        match name {
            None => error!("no bomber mission(s)"),
            Some(name) => {
                let arg = ArgTriple {
                    fst: jtid,
                    snd: ucid,
                    trd: name,
                };
                menu::jtac::call_bomber(lua, arg)?
            }
        }
    } else if let Some(s) = cmd.strip_prefix("focus") {
        // `focus` = my latest map mark, `focus clear`, or `focus <key>` = the
        // side's map mark with that text.
        let key = s.trim();
        let pos = if key.eq_ignore_ascii_case("clear") {
            None
        } else if key.is_empty() {
            match menu::jtac::latest_player_mark(ctx, lua, &ucid)? {
                Some(p) => Some(p),
                None => error!("place an F10 map mark first, or -jtac {jtid} focus <mark text>"),
            }
        } else {
            let mut found: SmallVec<[Vector2; 2]> = smallvec![];
            for mk in World::singleton(lua)?.get_mark_panels()? {
                let mk = mk?;
                if mk.side.is_match(&side) && mk.text.trim() == key {
                    found.push(Vector2::new(mk.pos.0.x, mk.pos.0.z));
                }
            }
            match found.len() {
                1 => Some(found[0]),
                0 => error!("no map mark with the text {key}"),
                n => error!("{n} map marks say {key}, make it unique"),
            }
        };
        menu::jtac::jtac_set_focus(lua, &ucid, jtid, pos)?;
    } else if let Some(s) = cmd.strip_prefix("code ") {
        let s = s.trim();
        let code = match s.parse::<u16>() {
            Ok(c) => c,
            Err(_) => error!("invalid laser code {s}, expected a code like 1688"),
        };
        // The JTAC takes its code one digit position at a time -- that is how
        // the F10 menu picks it (thousands, hundreds, tens, ones) -- so a whole
        // code, which is what players actually type, used to be rejected as
        // "mixed scales". Apply the upper three positions quietly and send the
        // last through the menu path, which announces the finished code.
        let (quiet, last): (SmallVec<[u16; 3]>, u16) = if s.len() == 4 {
            if !valid_laser_code(code) {
                error!(
                    "invalid laser code {code}: codes run 1111-1788 (first digit 1, second 1-7, third and fourth 1-8)"
                )
            }
            (
                smallvec![code / 1000 * 1000, code / 100 % 10 * 100, code / 10 % 10 * 10],
                code % 10,
            )
        } else if valid_code_part(code) {
            (smallvec![], code)
        } else {
            error!("invalid laser code {s}, expected a code 1111-1788 like 1688")
        };
        for part in quiet {
            ctx.jtac.set_code_part(&mut ctx.db, lua, &jtid, part)?;
        }
        let arg = ArgTriple {
            fst: jtid,
            snd: last,
            trd: ucid,
        };
        menu::jtac::jtac_set_code(lua, arg)?
    } else if let Some(arty) = cmd.strip_prefix("arty ") {
        if let Some((aid, n)) = arty.trim().split_once(" ") {
            let aids: SmallVec<[GroupId; 8]> = match aid.parse::<GroupId>() {
                Ok(id) => smallvec![id],
                Err(_) => {
                    if aid.eq_ignore_ascii_case("all") {
                        SmallVec::from_iter(jtac.nearby_artillery().into_iter().copied())
                    } else {
                        error!("invalid arty group id {aid}")
                    }
                }
            };
            let n = match n.trim().parse::<u8>() {
                Ok(n) => n,
                Err(_) => error!("expected a number of shots between 0 and 255"),
            };
            for aid in aids {
                let arg = ArgQuad {
                    fst: jtid,
                    snd: aid,
                    trd: n,
                    fth: ucid,
                };
                menu::jtac::jtac_artillery_mission(lua, arg)?
            }
        } else {
            error!("arty expected <id> and <n>")
        }
    } else {
        error!("invalid jtac command {cmd}")
    }
    Ok(())
}

/// A valid NATO laser code: first digit 1, second 1-7, third and fourth 1-8
/// (1111-1788).
fn valid_laser_code(code: u16) -> bool {
    let (d1, d2, d3, d4) = (code / 1000, code / 100 % 10, code / 10 % 10, code % 10);
    code <= 9999 && d1 == 1 && (1..=7).contains(&d2) && (1..=8).contains(&d3) && (1..=8).contains(&d4)
}

/// One digit position of a code, as the F10 menu sends it (1000, 100-700,
/// 10-80, 1-8). Anything else would leave the JTAC on an invalid code.
fn valid_code_part(part: u16) -> bool {
    part == 1000
        || (part % 100 == 0 && (1..=7).contains(&(part / 100)))
        || (part % 10 == 0 && (1..=8).contains(&(part / 10)))
        || (1..=8).contains(&part)
}

pub(super) fn run_jtac_commands(ctx: &mut Context, lua: MizLua) -> Result<()> {
    let cmds = mem::take(&mut ctx.jtac_commands);
    for (id, jtid, cmd) in cmds {
        // One bad command used to `?` out of the loop and silently drop every
        // other player's queued command along with it.
        if let Err(e) = run_jtac_command(ctx, lua, id, jtid, cmd.clone()) {
            error!("jtac command {jtid} {cmd} from {id:?} failed: {e:?}");
            ctx.db.ephemeral.msgs().send(
                MsgTyp::Chat(Some(id)),
                format_compact!("jtac {jtid} {cmd} failed: {e}"),
            );
        }
    }
    Ok(())
}

fn help_command(ctx: &mut Context, id: PlayerId) {
    let admin = match ctx.connected.get(&id) {
        None => false,
        Some(ifo) => ctx.db.ephemeral.cfg.admins.contains_key(&ifo.ucid),
    };
    for cmd in [
        " -switch <color>: side switch to <color>",
        " -lives: display your current lives",
        " -time: how long until server restart",
        " -weather: full weather report for your slot, including winds/temp aloft",
        " -balance: show your points balance",
        " -status: show campaign status (objectives, convoys, streak)",
        " -brief: auto-generated situational briefing (full report: F10 > Info > Situation)",
        " -transfer <amount> [<player> | objective:<objective>]: transfer points to another player or objective",
        " -delete <groupid>: delete a group you deployed for a partial refund",
        " -action <name> <args>: perform an action, -action help for a list of actions",
        " -bind <uuid>: link your account to the web dashboard (uuid from its login page)",
        " -jtac <jtid> <cmd>",
        " -gci [on|off|metric|imperial|auto]: control your live GCI voice calls",
        " -help: show this help message",
    ] {
        ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), cmd)
    }
    if admin {
        ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            " -admin <command>: run admin commands, -admin help for details",
        );
    }
}

/// A mistyped command (`-hepl`, `-Lives`), as opposed to chat that merely
/// starts with a dash (`-_-`, `--`, `-10 degrees out here`), which used to be
/// answered with the whole help text.
fn looks_like_command(msg: &str) -> bool {
    msg.strip_prefix('-')
        .and_then(|s| s.chars().next())
        .map_or(false, |c| c.is_ascii_alphabetic())
}

pub(super) fn process(
    ctx: &mut Context,
    lua: HooksLua,
    now: DateTime<Utc>,
    id: PlayerId,
    msg: String,
) -> Result<String> {
    if msg.eq_ignore_ascii_case("-switch blue") || msg.eq_ignore_ascii_case("-switch red") {
        sideswitch_player(ctx, lua, id, msg)
    } else if msg.eq_ignore_ascii_case("-lives") {
        if let Err(e) = lives_command(ctx, id) {
            error!("lives command failed for player {:?} {:?}", id, e);
        }
        Ok("".into())
    } else if msg.eq_ignore_ascii_case("-time") {
        time_command(ctx, id, now);
        Ok("".into())
    } else if msg.eq_ignore_ascii_case("-weather") {
        weather_command(ctx, lua, id);
        Ok("".into())
    } else if let Some(msg) = msg.strip_prefix("-admin ") {
        admin_command(ctx, id, msg, now);
        Ok("".into())
    } else if let Some(msg) = msg.strip_prefix("-action ") {
        action_command(ctx, id, msg);
        Ok("".into())
    } else if msg.eq_ignore_ascii_case("-gci") {
        gci_command(ctx, id, "");
        Ok("".into())
    } else if let Some(s) = msg.strip_prefix("-gci ") {
        gci_command(ctx, id, s);
        Ok("".into())
    } else if msg.starts_with("-balance") {
        balance_command(ctx, id);
        Ok("".into())
    } else if msg.starts_with("-status") {
        status_command(ctx, id);
        Ok("".into())
    } else if msg.starts_with("-brief") {
        brief_command(ctx, id);
        Ok("".into())
    } else if let Some(s) = msg.strip_prefix("-transfer ") {
        transfer_command(ctx, id, s);
        Ok("".into())
    } else if let Some(s) = msg.strip_prefix("-delete ") {
        delete_command(ctx, id, s);
        Ok("".into())
    } else if let Some(s) = msg.strip_prefix("-bind ") {
        bind_command(ctx, id, s);
        Ok("".into())
    } else if let Some(s) = msg.strip_prefix("-jtac ") {
        jtac_command(ctx, id, s);
        Ok("".into())
    } else if msg.starts_with("-help") {
        help_command(ctx, id);
        Ok("".into())
    } else if looks_like_command(&msg)
        || msg.as_str() == "help"
        || msg.as_str() == "points"
        || msg.as_str() == "credits"
    {
        ctx.db.ephemeral.msgs().send(
            MsgTyp::Chat(Some(id)),
            format_compact!(" {msg} is not a valid command. Valid commands follow."),
        );
        help_command(ctx, id);
        Ok("".into())
    } else {
        Ok(msg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn laser_codes() {
        assert!(valid_laser_code(1688));
        assert!(valid_laser_code(1111));
        assert!(valid_laser_code(1788));
        assert!(!valid_laser_code(1789));
        assert!(!valid_laser_code(1811));
        assert!(!valid_laser_code(2111));
        assert!(!valid_laser_code(1601));
        assert!(!valid_laser_code(688));
        assert!(valid_code_part(1000));
        assert!(valid_code_part(700));
        assert!(!valid_code_part(800));
        assert!(valid_code_part(80));
        assert!(!valid_code_part(90));
        assert!(valid_code_part(8));
        assert!(!valid_code_part(0));
        assert!(!valid_code_part(1688));
    }

    #[test]
    fn only_command_shaped_chat_gets_help() {
        assert!(looks_like_command("-hepl"));
        assert!(looks_like_command("-Lives"));
        assert!(!looks_like_command("-_-"));
        assert!(!looks_like_command("--"));
        assert!(!looks_like_command("-"));
        assert!(!looks_like_command("-10 degrees"));
        assert!(!looks_like_command("hello -x"));
    }

    #[test]
    fn confirm_is_split_off() {
        assert_eq!(strip_confirm("reset blue confirm"), ("reset blue", true));
        assert_eq!(strip_confirm(" shutdown  CONFIRM "), ("shutdown", true));
        assert_eq!(strip_confirm("reset blue"), ("reset blue", false));
        assert_eq!(strip_confirm("confirm"), ("confirm", false));
    }
}