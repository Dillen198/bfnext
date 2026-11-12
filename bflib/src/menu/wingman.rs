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

//! F10 > Wingman. The callbacks only queue the command: spawning a group from
//! inside a menu callback would fire Birth into a second `&mut Context`, so
//! `airlife::process_requests` does the work on the next 1 Hz tick.
//!
//! ```text
//! Wingman
//!   Request >      one entry per wingman type this aircraft can have,
//!                  "Copy of my <type>", each with its price
//!   Orders >       cover me, engage air, attack near me / my mark, hold,
//!                  ROE, formation
//!   Status
//!   Release (RTB)
//! ```

use super::{ArgTuple, Pager, slot_for_group};
use crate::airlife::{WingmanCmd, WingmanOrder, WingmanPick};
use anyhow::{Context as ErrContext, Result};
use bfprotocols::cfg::{Cfg, UnitTag};
use compact_str::format_compact;
use dcso3::{
    MizLua, String,
    coalition::Side,
    env::miz::GroupId,
    mission_commands::{GroupSubMenu, MissionCommands},
};

fn queue(lua: MizLua, gid: GroupId, cmd: WingmanCmd) -> Result<()> {
    let mut entered = crate::Entered::enter(lua)?;
    let ctx = &mut *entered;
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        ctx.airlife.queue(cmd, gid, ucid);
    }
    Ok(())
}

/// `snd` is the pick: 0 = the standard wingman, 1 = a copy of my aircraft,
/// 2 + i = `types[i]`.
fn request(lua: MizLua, a: ArgTuple<GroupId, u32>) -> Result<()> {
    let pick = match a.snd {
        0 => WingmanPick::Standard,
        1 => WingmanPick::Clone,
        n => WingmanPick::Type(n as usize - 2),
    };
    queue(lua, a.fst, WingmanCmd::Request(pick))
}

fn order(lua: MizLua, a: ArgTuple<GroupId, u32>) -> Result<()> {
    match WingmanOrder::ALL.get(a.snd as usize) {
        Some(o) => queue(lua, a.fst, WingmanCmd::Order(*o)),
        None => Ok(()),
    }
}

fn release(lua: MizLua, gid: GroupId) -> Result<()> {
    queue(lua, gid, WingmanCmd::Release)
}

fn status(lua: MizLua, gid: GroupId) -> Result<()> {
    queue(lua, gid, WingmanCmd::Status)
}

fn priced(label: &str, cost: i32) -> String {
    if cost > 0 {
        String::from(format_compact!("{label} ({cost} pts)"))
    } else {
        String::from(label)
    }
}

pub(super) fn add_wingman_menu_for_group(
    mc: &MissionCommands,
    cfg: &Cfg,
    side: Side,
    typ: &str,
    group: GroupId,
    parent: Option<GroupSubMenu>,
) -> Result<()> {
    let Some(w) = cfg.air_life.as_ref().and_then(|a| a.wingman.as_ref()) else {
        return Ok(());
    };
    let rotary = cfg
        .unit_classification
        .get(typ)
        .map(|t| t.contains(UnitTag::Helicopter))
        .unwrap_or(false);
    let root = mc.add_submenu_for_group(group, "Wingman".into(), parent)?;
    let mut p = Pager::new(group, root);

    // What this aircraft can call: the configured types for its kind (jet
    // or helicopter) that have aircraft for this side, and a copy of itself.
    let mut picks: Vec<(String, u32)> = vec![];
    if w.types.is_empty() {
        picks.push((priced("Standard wingman", w.cost), 0));
    }
    for (i, t) in w.types.iter().enumerate() {
        let templates = match side {
            Side::Blue => &t.templates_blue,
            _ => &t.templates_red,
        };
        if t.role.rotary() == rotary && !templates.is_empty() {
            picks.push((priced(t.name.as_str(), t.cost), i as u32 + 2));
        }
    }
    if let Some(cost) = w.clone.as_ref().and_then(|c| c.price(typ)) {
        picks.push((priced(&format!("Copy of my {typ}"), cost), 1));
    }
    let req = p.submenu(mc, "Request".into())?;
    let mut rp = Pager::new(group, req);
    for (label, n) in picks {
        rp.command(mc, label, request, ArgTuple { fst: group, snd: n })?;
    }

    let orders = p.submenu(mc, "Orders".into())?;
    let mut op = Pager::new(group, orders);
    for (i, o) in WingmanOrder::ALL.iter().enumerate() {
        op.command(mc, o.label().into(), order, ArgTuple { fst: group, snd: i as u32 })?;
    }
    p.command(mc, "Status".into(), status, group)?;
    p.command(mc, "Release (RTB)".into(), release, group)?;
    Ok(())
}
