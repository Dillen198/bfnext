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

use super::slot_for_group;
use crate::Context;
use anyhow::{Context as ErrContext, Result};
use chrono::prelude::*;
use dcso3::{env::miz::GroupId, mission_commands::MissionCommands, MizLua};

fn start_recon(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        let msg = ctx.db.recon_start(&ucid, Utc::now());
        ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg);
    }
    Ok(())
}

fn cancel_recon(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        let msg = ctx.db.recon_cancel(&ucid, Utc::now());
        ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg);
    }
    Ok(())
}

fn recon_status(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        let msg = ctx.db.recon_status(&ucid, Utc::now());
        ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg);
    }
    Ok(())
}

pub(super) fn add_recon_menu_for_group(mc: &MissionCommands, group: GroupId) -> Result<()> {
    let root = mc.add_submenu_for_group(group, "Recon".into(), None)?;
    mc.add_command_for_group(
        group,
        "Start Recon Pass".into(),
        Some(root.clone()),
        start_recon,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Cancel Recon Pass".into(),
        Some(root.clone()),
        cancel_recon,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Recon Status".into(),
        Some(root.clone()),
        recon_status,
        group,
    )?;
    Ok(())
}
