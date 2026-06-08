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

use super::{cargo, player_name, slot_for_group, ArgTuple};
use crate::{
    jtac::JtId,
    Context,
};
use anyhow::{Context as ErrContext, Result};
use bfprotocols::cfg::{Cfg, LimitEnforceTyp};
use compact_str::format_compact;
use dcso3::{
    coalition::Side, env::miz::GroupId, mission_commands::MissionCommands, MizLua, String,

};

use std::sync::Arc;

fn load_troops(lua: MizLua, arg: ArgTuple<GroupId, String>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &arg.fst).context("getting slot for group")?;
    match ctx.db.load_troops(lua, &slot, &arg.snd) {
        Ok((tr, oid)) => {
            let (n, oldest) = ctx
                .db
                .number_troops_deployed(side, &tr.name)
                .context("getting number of deployed troops")?;
            let player = player_name(&ctx.db, &slot);
            let sub = ctx.subscribed_jtac_menus.entry(slot).or_default();
            sub.pinned.insert(JtId::Slot(slot));
            sub.subscribed_objectives.insert(oid);
            super::jtac::init_jtac_menu_for_slot(ctx, lua, &slot)?;
            let enforce = match tr.limit_enforce {
                LimitEnforceTyp::DenyCrate => {
                    format_compact!("unloading will be denied when the limit is exceeded")
                }
                LimitEnforceTyp::DeleteOldest => match oldest {
                    Some(gid) => {
                        format_compact!(
                            "unloading will delete oldest, {gid}, when the limit is exceeded"
                        )
                    }
                    None => {
                        format_compact!("unloading will delete oldest when the limit is exceeded")
                    }
                },
            };
            let msg = format_compact!(
                "{player} loaded {}\n{n} of {} {} deployed, {}",
                tr.name,
                tr.limit,
                tr.name,
                enforce
            );
            ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg)
        }
        Err(e) => {
            ctx.db
                .ephemeral
                .msgs()
                .panel_to_group(10, false, arg.fst, format_compact!("{e}"))
        }
    }
    Ok(())
}

fn unload_troops(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    match ctx.db.unload_troops(lua, &ctx.idx, &slot) {
        Ok((tr, tgid, oid)) => {
            let player = player_name(&ctx.db, &slot);
            let sub = ctx.subscribed_jtac_menus.entry(slot.clone()).or_default();
            sub.pinned.insert(JtId::Group(tgid));
            if let Some(oid) = oid {
                sub.subscribed_objectives.insert(oid);
            }
            super::jtac::init_jtac_menu_for_slot(ctx, lua, &slot)?;

            let msg = format_compact!("{player} dropped {} troops into the field", tr.name);
            ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg);
        }
        Err(e) => ctx
            .db
            .ephemeral
            .msgs()
            .panel_to_group(10, false, gid, format_compact!("{e}")),
    }
    Ok(())
}

fn extract_troops(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    match ctx.db.extract_troops(lua, &ctx.jtac, &slot) {
        Ok((tr, _extracted_gid)) => {
            let player = player_name(&ctx.db, &slot);


            let msg = format_compact!("{player} extracted {} troops from the field", tr.name);
            ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg)
        }
        Err(e) => ctx
            .db
            .ephemeral
            .msgs()
            .panel_to_group(10, false, gid, format_compact!("{e}")),
    }
    Ok(())
}

fn return_troops(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    match ctx.db.return_troops(lua, &slot) {
        Ok(tr) => {
            let player = player_name(&ctx.db, &slot);


            let msg = format_compact!("{player} returned {} troops", tr.name);
            ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg)
        }
        Err(e) => ctx
            .db
            .ephemeral
            .msgs()
            .panel_to_group(10, false, gid, format_compact!("{e}")),
    }
    Ok(())
}

// ─── Ground Vehicle Troop Transport ──────────────────────────────────────────

fn board_ground_vehicle(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    match ctx.db.board_ground_vehicle(lua, &slot) {
        Ok((troop, _vehicle_uid)) => {
            let player = player_name(&ctx.db, &slot);
            ctx.db.ephemeral.msgs().panel_to_side(
                10,
                false,
                side,
                format_compact!(
                    "{player} boarded {} into a ground vehicle",
                    troop.name
                ),
            )
        }
        Err(e) => ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, format_compact!("{e}")),
    }
    Ok(())
}

fn disembark_ground_vehicle(lua: MizLua, arg: ArgTuple<GroupId, bfprotocols::db::group::UnitId>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &arg.fst).context("getting slot for group")?;
    match ctx.db.disembark_ground_vehicle(lua, &ctx.idx, arg.snd, &slot) {
        Ok((troop, _tgid)) => {
            let player = player_name(&ctx.db, &slot);
            ctx.db.ephemeral.msgs().panel_to_side(
                10,
                false,
                side,
                format_compact!("{player} dismounted {} from vehicle", troop.name),
            )
        }
        Err(e) => ctx.db.ephemeral.msgs().panel_to_group(10, false, arg.fst, format_compact!("{e}")),
    }
    Ok(())
}

fn list_ground_vehicle_passengers(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, _slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
    if cfg.ground_vehicle_cargo.is_empty() {
        ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, format_compact!("No ground vehicle cargo configured"));
        return Ok(());
    }
    // Find a nearby boardable vehicle and show its manifest.
    let has_pax = ctx
        .db
        .ephemeral
        .ground_vehicle_passengers
        .values()
        .any(|p| p.side == side && !p.troops.is_empty());
    if !has_pax {
        ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, format_compact!("No troops aboard friendly vehicles"));
    } else {
        let now = chrono::Utc::now();
        let manifests: Vec<_> = ctx.db.ephemeral.ground_vehicle_passengers.values()
            .filter(|p| p.side == side && !p.troops.is_empty())
            .map(|pax| {
                let names: Vec<_> = pax.troops.iter().map(|t| t.troop.name.as_str()).collect();
                let age_min = (now - pax.loaded_at).num_minutes();
                format_compact!(
                    "Vehicle {} (ID:{:?}): {} squad(s) [{age_min}m ago]: {}",
                    pax.vehicle_name,
                    pax.vehicle_unit_id,
                    pax.troops.len(),
                    names.join(", ")
                )
            })
            .collect();
        for msg in manifests {
            ctx.db.ephemeral.msgs().panel_to_group(15, false, gid, msg);
        }
    }
    Ok(())
}

pub(super) fn add_troops_menu_for_group(
    cfg: &Cfg,
    mc: &MissionCommands,
    side: &Side,
    group: GroupId,
) -> Result<()> {
    if let Some(squads) = cfg.troops.get(side) {
        let root = mc.add_submenu_for_group(group, "Troops".into(), None)?;
        mc.add_command_for_group(
            group,
            "Unload".into(),
            Some(root.clone()),
            unload_troops,
            group,
        )?;
        mc.add_command_for_group(
            group,
            "Extract".into(),
            Some(root.clone()),
            extract_troops,
            group,
        )?;
        mc.add_command_for_group(
            group,
            "List".into(),
            Some(root.clone()),
            cargo::list_current_cargo,
            group,
        )?;
        mc.add_command_for_group(
            group,
            "Return".into(),
            Some(root.clone()),
            return_troops,
            group,
        )?;
        let root = mc.add_submenu_for_group(group, "Squads".into(), Some(root))?;
        for sq in squads {
            let item = if sq.cost > 0 {
                format_compact!("Load {} squad ({} pts)", sq.name, sq.cost)
            } else {
                format_compact!("Load {} squad", sq.name)
            };
            mc.add_command_for_group(
                group,
                item.into(),
                Some(root.clone()),
                load_troops,
                ArgTuple {
                    fst: group,
                    snd: sq.name.clone(),
                },
            )?;
        }
    }

    // Ground vehicle transport menu — only shown when any vehicle types are configured.
    if !cfg.ground_vehicle_cargo.is_empty() {
        let gv_root = mc.add_submenu_for_group(group, "Ground Vehicle".into(), None)?;
        mc.add_command_for_group(
            group,
            "Board Vehicle (transfer squad)".into(),
            Some(gv_root.clone()),
            board_ground_vehicle,
            group,
        )?;
        mc.add_command_for_group(
            group,
            "Dismount Squad from Vehicle".into(),
            Some(gv_root.clone()),
            disembark_ground_vehicle,
            ArgTuple { fst: group, snd: bfprotocols::db::group::UnitId::default() },
        )?;
        mc.add_command_for_group(
            group,
            "Vehicle Passenger Manifest".into(),
            Some(gv_root.clone()),
            list_ground_vehicle_passengers,
            group,
        )?;
    }
    Ok(())
}
