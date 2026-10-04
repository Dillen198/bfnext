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

//! The dynamic ground war (`Cfg::ground_war`). Formations themselves -- what
//! they are, how they move, fight and go home -- live in
//! `crate::db::formation`. This is who commands them:
//!
//! - `ai`: each side's AI ground commander, which raises formations from
//!   the garrisons nearest the front, sends them along the offensive's axis,
//!   counter-attacks bases under threat and pulls the battered ones back.
//! - Players, from F10 > Ground Forces (and the dashboard): any formation can
//!   be ordered, and a player's order keeps the AI off that formation for
//!   `player_order_lock_secs`, so the AI fills in for whatever nobody is
//!   commanding.
//!
//! Menu callbacks only queue a `Cmd`; it runs on the slow tick, where there is
//! a Lua handle and nothing else holds the context.

mod ai;
mod picture;

pub(crate) use picture::picture;

use crate::{
    db::formation::{FormationId, FormationRt, Order},
    Context,
};
use bfprotocols::{
    cfg::GroundWarCfg,
    db::objective::ObjectiveId,
    groundwar::{GroundCommand, GroundCommandReply},
    perf::PerfInner,
};
use chrono::{prelude::*, Duration};
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, env::miz::GroupId as MizGroupId, net::Ucid, MizLua};
use fxhash::FxHashMap;
use log::info;
use std::mem;

#[derive(Debug, Clone)]
pub(crate) enum CmdKind {
    Status,
    Raise(ObjectiveId),
    Order(FormationId, Order),
    Release(FormationId),
}

/// A player's ground-war command, from the F10 menu.
#[derive(Debug, Clone)]
pub(crate) struct Cmd {
    pub(crate) ucid: Ucid,
    /// The player's DCS group, for the reply.
    pub(crate) group: MizGroupId,
    pub(crate) kind: CmdKind,
}

#[derive(Debug, Default)]
pub(crate) struct GroundWar {
    pub(crate) rt: FormationRt,
    cmds: Vec<Cmd>,
    last_think: Option<DateTime<Utc>>,
    last_order: FxHashMap<Ucid, DateTime<Utc>>,
    frontline_hash: u64,
    frontline_drawn: Option<DateTime<Utc>>,
}

impl GroundWar {
    pub(crate) fn queue(&mut self, cmd: Cmd) {
        self.cmds.push(cmd);
    }
}

fn cfg(ctx: &Context) -> Option<GroundWarCfg> {
    ctx.db.ephemeral.cfg.ground_war.clone().filter(|c| c.enabled)
}

fn reply(ctx: &mut Context, group: MizGroupId, text: impl Into<dcso3::String>) {
    ctx.db.ephemeral.msgs().panel_to_group(15, false, group, text.into());
}

/// Carry out a player's order or raise -- from F10 or the dashboard --
/// after the checks every route must pass: the server lets them command,
/// the formation is on their side, they are not ordering faster than the
/// cooldown, and they can pay for a raise. `via` names the route in the
/// side's announcement. Ok is (what happened, the formation it concerned).
pub(crate) fn apply(
    lua: MizLua,
    ctx: &mut Context,
    cfg: &GroundWarCfg,
    ucid: Ucid,
    kind: CmdKind,
    via: &str,
    now: DateTime<Utc>,
) -> Result<(CompactString, Option<FormationId>), CompactString> {
    let Some(player) = ctx.db.player(&ucid) else {
        return Err("unknown player".into());
    };
    let (side, pname, points) = (player.side, player.name.clone(), player.points);
    if !matches!(side, Side::Blue | Side::Red) {
        return Err("pick a side before commanding ground forces".into());
    }
    if !cfg.command_rule.check(&ucid) {
        return Err("you are not cleared to command ground forces on this server".into());
    }
    let cooldown = Duration::seconds(cfg.player_order_cooldown_secs as i64);
    let cooling = ctx
        .groundwar
        .last_order
        .get(&ucid)
        .map_or(false, |t| now - *t < cooldown);
    let mine = |ctx: &Context, id: FormationId| ctx.db.formation(id).map_or(false, |f| f.side == side);
    match kind {
        CmdKind::Status => Err("status is not an order".into()),
        CmdKind::Raise(oid) => {
            if cooling {
                return Err("wait a moment before your next ground order".into());
            }
            if cfg.raise_cost > 0 && (points as i64) < cfg.raise_cost as i64 {
                return Err(format_compact!("raising a formation costs {} points", cfg.raise_cost));
            }
            let id = ctx
                .db
                .raise_formation(&mut ctx.groundwar.rt, lua, side, oid, Some(ucid), now)
                .map_err(|e| format_compact!("can't raise a formation: {e}"))?;
            ctx.groundwar.last_order.insert(ucid, now);
            let name = ctx.db.formation(id).map(|f| f.name.clone()).unwrap_or_default();
            if cfg.raise_cost > 0 {
                ctx.db.adjust_points(
                    &ucid,
                    -(cfg.raise_cost.min(i32::MAX as u32) as i32),
                    &format!("for raising {name}"),
                );
            }
            ctx.db.ephemeral.msgs().panel_to_side(
                15,
                false,
                side,
                format_compact!("{pname}{via} has raised {name}. It awaits orders."),
            );
            Ok((format_compact!("{name} raised and awaiting orders"), Some(id)))
        }
        CmdKind::Order(id, order) => {
            if !mine(ctx, id) {
                return Err("that formation is not ours".into());
            }
            if cooling {
                return Err("wait a moment before your next ground order".into());
            }
            let what = ctx
                .db
                .order_formation(&mut ctx.groundwar.rt, lua, id, order, Some(ucid), now)
                .map_err(|e| format_compact!("order refused: {e}"))?;
            ctx.groundwar.last_order.insert(ucid, now);
            ctx.db
                .ephemeral
                .msgs()
                .panel_to_side(10, false, side, format_compact!("{pname}{via}: {what}"));
            let mins = cfg.player_order_lock_secs / 60;
            Ok((format_compact!("{what}. Yours for {mins} min, then the AI takes it back"), Some(id)))
        }
        CmdKind::Release(id) => {
            if !mine(ctx, id) {
                return Err("that formation is not ours".into());
            }
            ctx.db.release_formation(id).map_err(|e| format_compact!("{e}"))?;
            let name = ctx.db.formation(id).map(|f| f.name.clone()).unwrap_or_default();
            Ok((format_compact!("{name} is back under AI command"), Some(id)))
        }
    }
}

fn run_cmd(lua: MizLua, ctx: &mut Context, cfg: &GroundWarCfg, cmd: Cmd, now: DateTime<Utc>) {
    let Some(side) = ctx.db.player(&cmd.ucid).map(|p| p.side) else { return };
    match cmd.kind {
        CmdKind::Status => {
            let mut lines: Vec<String> = ctx
                .db
                .formations()
                .filter(|f| f.side == side)
                .map(|f| {
                    let mut l = ctx.db.formation_status(f, now).to_string();
                    if ctx.groundwar.rt.is_halted(f.id) {
                        l.push_str(" [halted at contact]");
                    }
                    l
                })
                .collect();
            if lines.is_empty() {
                lines.push("No formations in the field.".into());
            }
            let battles = ctx.groundwar.rt.battles().len();
            let n = cfg.max_formations_per_side;
            let text = format_compact!(
                "GROUND FORCES ({} of {n}), {battles} battle(s) on the map\n{}",
                lines.len(),
                lines.join("\n")
            );
            ctx.db.ephemeral.msgs().panel_to_group(30, false, cmd.group, text);
        }
        kind => {
            let text = match apply(lua, ctx, cfg, cmd.ucid, kind, "", now) {
                Ok((msg, _)) => format_compact!("{msg}."),
                Err(e) => format_compact!("{e}."),
            };
            reply(ctx, cmd.group, text);
        }
    }
}

/// A command from the dashboard (`ground-command`), for player `ucid` as
/// bfdb resolved them from their login.
pub(crate) fn dashboard_command(
    lua: MizLua,
    ctx: &mut Context,
    ucid: Ucid,
    cmd: GroundCommand,
) -> GroundCommandReply {
    let now = Utc::now();
    let Some(cfg) = cfg(ctx) else {
        return GroundCommandReply {
            ok: false,
            message: "the ground war is not enabled on this server".into(),
            formation: None,
        };
    };
    let id = |o: u64| ObjectiveId::from(o as i64);
    let kind = match cmd {
        GroundCommand::Attack { formation, objective } => CmdKind::Order(formation, Order::Attack(id(objective))),
        GroundCommand::Defend { formation, objective } => CmdKind::Order(formation, Order::Defend(id(objective))),
        GroundCommand::Withdraw { formation, objective } => {
            CmdKind::Order(formation, Order::Withdraw(id(objective)))
        }
        GroundCommand::Hold { formation } => CmdKind::Order(formation, Order::Hold),
        GroundCommand::Release { formation } => CmdKind::Release(formation),
        GroundCommand::Raise { objective } => CmdKind::Raise(id(objective)),
    };
    match apply(lua, ctx, &cfg, ucid, kind, " (dashboard)", now) {
        Ok((msg, formation)) => GroundCommandReply { ok: true, message: msg.to_string(), formation },
        Err(e) => GroundCommandReply { ok: false, message: e.to_string(), formation: None },
    }
}

/// Redraw the F10 front line when the formations have moved it, at most
/// every `frontline_redraw_secs` (a redraw costs a few hundred map
/// commands out of a budget shared with every label on the map).
fn frontline(ctx: &mut Context, cfg: &GroundWarCfg, now: DateTime<Utc>) {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let Some(fl) = ctx.frontline.as_mut() else { return };
    fl.set_pressure_weight(cfg.frontline_weight);
    if cfg.frontline_weight <= 0. {
        return;
    }
    let mut h = DefaultHasher::new();
    for (x, y, w) in crate::db::formation::pressure(&ctx.db.persisted, cfg.frontline_weight) {
        ((x / 5_000.).round() as i64).hash(&mut h);
        ((y / 5_000.).round() as i64).hash(&mut h);
        ((w * 4.).round() as i64).hash(&mut h);
    }
    let hash = h.finish();
    let gw = &mut ctx.groundwar;
    let due = gw
        .frontline_drawn
        .map_or(true, |t| now - t >= Duration::seconds(cfg.frontline_redraw_secs as i64));
    if hash == gw.frontline_hash || !due {
        return;
    }
    gw.frontline_hash = hash;
    gw.frontline_drawn = Some(now);
    info!("ground war: formations have moved the front, redrawing it");
    crate::update_frontline(ctx, now, true);
}

/// The slow-tick entry point.
pub(crate) fn tick(lua: MizLua, ctx: &mut Context, perf: &mut PerfInner, now: DateTime<Utc>) {
    let Some(cfg) = cfg(ctx) else {
        // Switched off with formations still out: send them home, or their
        // groups stay out of their bases' garrisons for good.
        if ctx.db.formations().next().is_some() {
            info!("ground war: disabled, returning every formation to its garrison");
            ctx.db.disband_all_formations(&mut ctx.groundwar.rt, now);
        }
        if !ctx.groundwar.rt.battles().is_empty() {
            ctx.db.clear_battles(&mut ctx.groundwar.rt, lua);
        }
        return;
    };
    for cmd in mem::take(&mut ctx.groundwar.cmds) {
        run_cmd(lua, ctx, &cfg, cmd, now);
    }
    if let Some(ai_cfg) = cfg.ai.as_ref() {
        let every = Duration::seconds(ai_cfg.think_secs.max(30) as i64);
        if ctx.groundwar.last_think.map_or(true, |t| now - t >= every) {
            ctx.groundwar.last_think = Some(now);
            ai::think(lua, ctx, &cfg, ai_cfg, now);
        }
    }
    let vis = visibility(lua, ctx, &cfg);
    ctx.groundwar.rt.set_visibility(vis);
    ctx.db.tick_formations(&mut ctx.groundwar.rt, lua, &ctx.idx, perf, now);
    frontline(ctx, &cfg, now);
}

/// How far anyone can see right now, as a share of a clear day: the light
/// (from the mission's time of day; `night_spot` of it in the dark, ramping
/// through dawn and dusk) times the weather's visibility.
fn visibility(lua: MizLua, ctx: &Context, cfg: &GroundWarCfg) -> f64 {
    let night = cfg.combat.night_spot.clamp(0.05, 1.);
    let light = match dcso3::timer::Timer::singleton(lua).and_then(|t| t.get_abs_time()) {
        Ok(t) => night + (1. - night) * daylight((t.0 % 86_400.) / 3_600.),
        Err(_) => 1.,
    };
    let weather = ctx
        .bot_weather
        .as_ref()
        .map(|w| w.visibility_m)
        .filter(|v| *v > 0.)
        .map_or(1., |v| (v / cfg.combat.spot_m.max(1.)).min(1.));
    (light * weather).clamp(0.15, 1.)
}

/// How light it is over the theatre now, 0..1 (1 if the mission clock
/// can't be read).
pub(crate) fn daylight_now(lua: MizLua) -> f64 {
    dcso3::timer::Timer::singleton(lua)
        .and_then(|t| t.get_abs_time())
        .map(|t| daylight((t.0 % 86_400.) / 3_600.))
        .unwrap_or(1.)
}

/// 0 at night, 1 by day, ramping over dawn (05:00-07:00) and dusk
/// (18:30-20:30) local mission time.
fn daylight(hour: f64) -> f64 {
    match hour {
        h if h < 5. => 0.,
        h if h < 7. => (h - 5.) / 2.,
        h if h < 18.5 => 1.,
        h if h < 20.5 => 1. - (h - 18.5) / 2.,
        _ => 0.,
    }
}

/// Shared by the AI and the menus: an objective nobody can drive to.
pub(crate) fn at_sea(kind: &bfprotocols::db::objective::ObjectiveKind) -> bool {
    matches!(kind, bfprotocols::db::objective::ObjectiveKind::CarrierGroup { .. })
}

#[cfg(test)]
mod tests {
    use super::daylight;

    #[test]
    fn daylight_ramps_through_dawn_and_dusk() {
        assert_eq!(daylight(2.), 0.);
        assert_eq!(daylight(6.), 0.5);
        assert_eq!(daylight(12.), 1.);
        assert_eq!(daylight(19.5), 0.5);
        assert_eq!(daylight(23.), 0.);
    }
}

