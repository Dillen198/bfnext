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
use crate::{
    ewr::{self, EwrUnits},
    Context,
};
use anyhow::{Context as ErrContext, Result};
use chrono::prelude::*;
use compact_str::{format_compact, CompactString};
use dcso3::{net::Ucid, Vector2, env::miz::GroupId, mission_commands::MissionCommands, MizLua};
use std::fmt::Write;

// ─── Shared report-building logic ──────────────────────────────────────────
// Used by both the F10 menu closures below and the cockpit-UI RPC handlers
// in bflib/src/admin.rs (AdminCommand::Ewr*), so the two UIs can never drift
// out of sync with each other.

pub(crate) fn ewr_toggle_for(ctx: &mut Context, ucid: &Ucid) -> bool {
    ctx.ewr.toggle(ucid)
}

pub(crate) fn ewr_set_units_for(ctx: &mut Context, ucid: &Ucid, imperial: bool) {
    ctx.ewr.set_units(
        ucid,
        if imperial { EwrUnits::Imperial } else { EwrUnits::Metric },
    );
}

pub(crate) fn build_braa_report(ctx: &mut Context, ucid: &Ucid, friendly: bool) -> CompactString {
    let mut report = format_compact!("{} BRAA\n", if friendly { "Friendlies" } else { "Bandits" });
    let mode = ctx.db.ephemeral.cfg.ewr_mode;
    let delay = ctx.db.ephemeral.cfg.ewr_delay;
    if let Some(player) = ctx.db.player(ucid) {
        if let Some((_, Some(inst))) = &player.current_slot {
            let contacts = ctx
                .ewr
                .where_chicken(Utc::now(), friendly, true, ucid, player, inst, mode, delay);
            let _ = write!(report, "{}\n", ewr::HEADER);
            for braa in contacts {
                let _ = write!(report, "{braa}\n");
            }
        }
    }
    report
}

pub(crate) fn build_ground_intel_report(ctx: &mut Context, ucid: &Ucid) -> CompactString {
    let mut report = format_compact!("Ground Intel\n");
    if let Some(player) = ctx.db.player(ucid) {
        let side = player.side;
        if let Some((_, Some(inst))) = &player.current_slot {
            let pos = Vector2::new(inst.position.p.x, inst.position.p.z);
            let lines = ctx.ewr.intel_picture(side, pos, &ctx.db.ephemeral.intel_db);
            for line in lines {
                let _ = write!(report, "{line}\n");
            }
        } else {
            let _ = write!(report, "Not in a slot");
        }
    }
    report
}

// ─── F10 menu glue ──────────────────────────────────────────────────────────

fn toggle_ewr(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        let st = if ewr_toggle_for(ctx, &ucid) { "enabled" } else { "disabled" };
        ctx.db.ephemeral.msgs().panel_to_group(
            5,
            false,
            gid,
            format_compact!("ewr reports are {st}"),
        )
    }
    Ok(())
}

fn ewr_report(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        let report = build_braa_report(ctx, &ucid, false);
        ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, report);
    }
    Ok(())
}

fn friendly_ewr_report(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        let report = build_braa_report(ctx, &ucid, true);
        ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, report);
    }
    Ok(())
}

fn ewr_units_imperial(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        ewr_set_units_for(ctx, &ucid, true);
        ctx.db
            .ephemeral
            .msgs()
            .panel_to_group(5, false, gid, "EWR units are now Imperial");
    }
    Ok(())
}

fn ewr_units_metric(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        ewr_set_units_for(ctx, &ucid, false);
        ctx.db
            .ephemeral
            .msgs()
            .panel_to_group(5, false, gid, "EWR units are now Metric");
    }
    Ok(())
}

fn ground_intel_report(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Some(ucid) = ctx.db.ephemeral.player_in_slot(&slot).copied() {
        let report = build_ground_intel_report(ctx, &ucid);
        ctx.db.ephemeral.msgs().panel_to_group(15, false, gid, report);
    }
    Ok(())
}

pub(super) fn add_ewr_menu_for_group(mc: &MissionCommands, group: GroupId) -> Result<()> {
    let root = mc.add_submenu_for_group(group, "EWR".into(), None)?;
    mc.add_command_for_group(
        group,
        "Report".into(),
        Some(root.clone()),
        ewr_report,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Toggle".into(),
        Some(root.clone()),
        toggle_ewr,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Friendly Report".into(),
        Some(root.clone()),
        friendly_ewr_report,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Units to Imperial".into(),
        Some(root.clone()),
        ewr_units_imperial,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Units to Metric".into(),
        Some(root.clone()),
        ewr_units_metric,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Ground Intel".into(),
        Some(root.clone()),
        ground_intel_report,
        group,
    )?;
    Ok(())
}
