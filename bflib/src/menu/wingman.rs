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

use super::{slot_for_group, Pager};
use crate::{airlife::WingmanCmd, Context};
use anyhow::{Context as ErrContext, Result};
use dcso3::{env::miz::GroupId, mission_commands::MissionCommands, MizLua};

fn queue(lua: MizLua, gid: GroupId, cmd: WingmanCmd) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        ctx.airlife.queue(cmd, gid, ucid);
    }
    Ok(())
}

fn request(lua: MizLua, gid: GroupId) -> Result<()> {
    queue(lua, gid, WingmanCmd::Request)
}

fn release(lua: MizLua, gid: GroupId) -> Result<()> {
    queue(lua, gid, WingmanCmd::Release)
}

fn status(lua: MizLua, gid: GroupId) -> Result<()> {
    queue(lua, gid, WingmanCmd::Status)
}

pub(super) fn add_wingman_menu_for_group(mc: &MissionCommands, group: GroupId) -> Result<()> {
    let root = mc.add_submenu_for_group(group, "Wingman".into(), None)?;
    let mut p = Pager::new(group, root);
    for (label, cb) in [
        ("Request Wingman", request as fn(MizLua, GroupId) -> Result<()>),
        ("Release Wingman", release),
        ("Wingman Status", status),
    ] {
        p.command(mc, label.into(), cb, group)?;
    }
    Ok(())
}
