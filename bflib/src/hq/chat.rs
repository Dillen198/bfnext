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

//! `-hq` and `-request` in chat. Chat arrives in the hooks Lua state, which
//! can't order units about, so anything that acts is queued here and run on
//! the next 1 Hz tick from the mission state (`run_chat`).

use super::{cfg, command, intent_text, ops_text};
use crate::{msgq::MsgTyp, Context};
use bfprotocols::{
    db::objective::ObjectiveId,
    hq::{Directive, HqCommand, Posture, RequestKind},
};
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, net::PlayerId, MizLua, Vector2};

const HELP: [&str; 9] = [
    " -hq: your side's commander's intent",
    " -hq ops: what the HQ has under way, and the requests waiting",
    " -request <cas|cap|sead|recon|fires|supply|troops> [objective]: ask HQ for support (nearest objective if none named)",
    " -request cancel <id>: withdraw a request",
    "HQ commanders only:",
    " -hq posture <offensive|balanced|defensive>, -hq effort <objective>, -hq defend <objective>",
    " -hq pause | resume: stop / restart the HQ's own planning (orders under way carry on)",
    " -hq cancel <op id>: call off an operation",
    " -hq clear: hand command back to the HQ",
];

fn say(ctx: &mut Context, id: PlayerId, text: impl Into<CompactString>) {
    let text: CompactString = text.into();
    for line in text.lines() {
        ctx.db.ephemeral.msgs().send(MsgTyp::Chat(Some(id)), line);
    }
}

fn player_side(ctx: &Context, id: PlayerId) -> Option<(dcso3::net::Ucid, Side)> {
    let ifo = ctx.connected.get(&id)?;
    let p = ctx.db.player(&ifo.ucid)?;
    Some((ifo.ucid, p.side))
}

/// The objective whose name best matches `s`: an exact match, else the one
/// name it starts, else the one name it is part of.
fn find_objective(ctx: &Context, s: &str) -> Result<ObjectiveId, CompactString> {
    let want = s.trim().to_lowercase();
    if want.is_empty() {
        return Err("name an objective".into());
    }
    let all: Vec<(ObjectiveId, String)> =
        ctx.db.objectives().map(|(id, o)| (*id, o.name().to_lowercase())).collect();
    if let Some((id, _)) = all.iter().find(|(_, n)| *n == want) {
        return Ok(*id);
    }
    for pred in [
        &(|n: &str| n.starts_with(want.as_str())) as &dyn Fn(&str) -> bool,
        &(|n: &str| n.contains(want.as_str())),
    ] {
        let hits: Vec<&(ObjectiveId, String)> = all.iter().filter(|(_, n)| pred(n)).collect();
        match hits.len() {
            0 => continue,
            1 => return Ok(hits[0].0),
            n => return Err(format_compact!("{n} objectives match '{s}', be more specific")),
        }
    }
    Err(format_compact!("no objective called '{s}'"))
}

/// Chat entry point (hooks state). `-hq` and `-hq ops` answer at once;
/// everything else is queued for `run_chat`.
pub(crate) fn chat(ctx: &mut Context, id: PlayerId, rest: &str, request: bool) {
    let rest = rest.trim();
    let Some((_, side)) = player_side(ctx, id) else { return };
    if cfg(ctx).is_none() {
        say(ctx, id, "there is no HQ on this server");
        return;
    }
    if !request {
        match rest.to_ascii_lowercase().as_str() {
            "" | "intent" => {
                let t = intent_text(ctx, side);
                return say(ctx, id, t);
            }
            "ops" | "operations" | "status" => {
                let t = ops_text(ctx, side);
                return say(ctx, id, t);
            }
            "help" => {
                for l in HELP {
                    say(ctx, id, l);
                }
                return;
            }
            _ => (),
        }
    }
    ctx.hq.chat.push((id, request, rest.into()));
}

fn parse(ctx: &Context, request: bool, s: &str) -> Result<HqCommand, CompactString> {
    let mut words = s.split_whitespace();
    let verb = words.next().unwrap_or("").to_ascii_lowercase();
    let rest: Vec<&str> = words.collect();
    let rest = rest.join(" ");
    if request {
        if verb == "cancel" {
            let id = rest.trim().trim_start_matches('#').parse::<u64>().map_err(|_| CompactString::from("-request cancel <id>"))?;
            return Ok(HqCommand::CancelRequest { request_id: id });
        }
        let kind = RequestKind::parse(&verb)
            .ok_or_else(|| format_compact!("unknown request '{verb}', try cas cap sead recon fires supply troops"))?;
        let objective = if rest.trim().is_empty() {
            None
        } else {
            Some(find_objective(ctx, &rest)?.inner() as u64)
        };
        return Ok(HqCommand::Request { request: kind, objective });
    }
    // Orders build on whatever human orders already stand (`merge`), so
    // `-hq effort X` after `-hq posture defensive` keeps the posture.
    let mut d = Directive::default();
    let mut paused = false;
    match verb.as_str() {
        "posture" => {
            d.posture = Some(match rest.trim().to_ascii_lowercase().as_str() {
                "offensive" | "attack" | "offence" | "offense" => Posture::Offensive,
                "balanced" | "hold" => Posture::Balanced,
                "defensive" | "defend" | "defence" | "defense" => Posture::Defensive,
                _ => return Err("-hq posture <offensive|balanced|defensive>".into()),
            })
        }
        "effort" | "main" | "attack" => d.main_effort = Some(find_objective(ctx, &rest)?.inner() as u64),
        "defend" | "hold" => d.defend = vec![find_objective(ctx, &rest)?.inner() as u64],
        "supply" => d.supply_priority = vec![find_objective(ctx, &rest)?.inner() as u64],
        "avoid" => d.avoid = vec![find_objective(ctx, &rest)?.inner() as u64],
        "pause" => paused = true,
        "resume" => (),
        "clear" | "release" => return Ok(HqCommand::ClearOverride),
        "cancel" => {
            let op = rest.trim().trim_start_matches('#').parse::<u64>().map_err(|_| CompactString::from("-hq cancel <op id>"))?;
            return Ok(HqCommand::CancelOp { op });
        }
        _ => return Err(format_compact!("unknown HQ command '{verb}', -hq help for the list")),
    }
    Ok(HqCommand::Override { directive: d, disabled_ops: vec![], paused })
}

/// Lay a new partial order over the human orders already standing.
fn merge(old: &super::Override, cmd: HqCommand) -> HqCommand {
    match cmd {
        HqCommand::Override { directive: n, disabled_ops, paused } => {
            let mut d = old.directive.clone();
            d.posture = n.posture.or(d.posture);
            d.main_effort = n.main_effort.or(d.main_effort);
            for (field, new) in [
                (&mut d.defend, n.defend),
                (&mut d.supply_priority, n.supply_priority),
                (&mut d.avoid, n.avoid),
            ] {
                for id in new {
                    if !field.contains(&id) {
                        field.insert(0, id);
                    }
                }
            }
            d.ttl_secs = None;
            let mut disabled = old.disabled_ops.clone();
            disabled.extend(disabled_ops);
            HqCommand::Override { directive: d, disabled_ops: disabled, paused }
        }
        c => c,
    }
}

/// Run the queued chat commands, from the mission state.
pub(crate) fn run_chat(lua: MizLua, ctx: &mut Context) {
    for (id, request, s) in std::mem::take(&mut ctx.hq.chat) {
        let Some((ucid, side)) = player_side(ctx, id) else { continue };
        let cmd = match parse(ctx, request, &s) {
            Ok(c) => c,
            Err(e) => {
                say(ctx, id, e);
                continue;
            }
        };
        let cmd = match ctx.db.persisted.hq.side(side).human.clone() {
            Some(old) => merge(&old, cmd),
            None => cmd,
        };
        let from: Option<Vector2> = ctx
            .db
            .instanced_players()
            .find(|(u, _, _)| **u == ucid)
            .map(|(_, _, i)| Vector2::new(i.position.p.x, i.position.p.z));
        let reply = command(lua, ctx, side, Some(ucid), cmd, from);
        say(ctx, id, reply.message.as_str());
    }
}
