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

use super::ArgTriple;
use crate::{db::Db, Context};
use anyhow::{Context as ErrContext, Result};
use bfprotocols::db::objective::ObjectiveKind;
use compact_str::format_compact;
use dcso3::{
    coalition::Side,
    env::miz::GroupId,
    mission_commands::{GroupSubMenu, MissionCommands},
    net::SlotId,
    MizLua,
};
use std::fmt::Write;

const PAGE_SIZE: usize = 10;

fn fmt_kind(kind: &ObjectiveKind) -> &'static str {
    match kind {
        ObjectiveKind::Airbase => "AB",
        ObjectiveKind::Fob => "FOB",
        ObjectiveKind::Farp { .. } => "FARP",
        ObjectiveKind::Logistics => "LOGI",
        ObjectiveKind::NavalBase => "NAVAL",
        ObjectiveKind::CarrierGroup { .. } => "CVN",
        ObjectiveKind::Factory { .. } => "FACT",
        ObjectiveKind::SpecialSamSite => "SAM",
    }
}

/// Returns objectives for `owner` sorted by name, sliced to the requested page (0-indexed).
fn build_friendly_report(db: &Db, side: Side, page: usize) -> compact_str::CompactString {
    let mut objectives: Vec<_> = db
        .objectives()
        .filter(|(_, obj)| obj.owner() == side)
        .collect();
    objectives.sort_by(|(_, a), (_, b)| a.name().cmp(b.name()));
    let total = objectives.len();
    let total_pages = total.div_ceil(PAGE_SIZE).max(1);
    let start = page * PAGE_SIZE;
    let slice = objectives
        .into_iter()
        .skip(start)
        .take(PAGE_SIZE)
        .collect::<Vec<_>>();
    let mut report = format_compact!(
        "=== Friendly Objectives (pg {}/{}) ===\n",
        page + 1,
        total_pages
    );
    for (_, obj) in &slice {
        let threatened = if obj.threatened() { " [THREAT]" } else { "" };
        let capturable = if obj.captureable() { " [CAP]" } else { "" };
        let _ = write!(
            report,
            "{} [{}] HP:{:3}% L:{:3}% S:{:3}% F:{:3}%{}{}\n",
            obj.name(),
            fmt_kind(obj.kind()),
            obj.health(),
            obj.logi(),
            obj.supply(),
            obj.fuel(),
            threatened,
            capturable,
        );
    }
    if slice.is_empty() {
        let _ = write!(report, "No friendly objectives.\n");
    }
    report
}

fn build_enemy_report(db: &Db, side: Side, page: usize) -> compact_str::CompactString {
    let enemy_side = match side {
        Side::Blue => Side::Red,
        Side::Red => Side::Blue,
        Side::Neutral => Side::Neutral,
    };
    let mut objectives: Vec<_> = db
        .objectives()
        .filter(|(_, obj)| obj.owner() == enemy_side)
        .collect();
    objectives.sort_by(|(_, a), (_, b)| a.name().cmp(b.name()));
    let total = objectives.len();
    let total_pages = total.div_ceil(PAGE_SIZE).max(1);
    let start = page * PAGE_SIZE;
    let slice = objectives
        .into_iter()
        .skip(start)
        .take(PAGE_SIZE)
        .collect::<Vec<_>>();
    let mut report = format_compact!(
        "=== Enemy Objectives (pg {}/{}) ===\n",
        page + 1,
        total_pages
    );
    for (_, obj) in &slice {
        let threatened = if obj.threatened() { " [THREAT]" } else { "" };
        let capturable = if obj.captureable() { " [CAP]" } else { "" };
        let _ = write!(
            report,
            "{} [{}] HP:{:3}% L:{:3}%{}{}\n",
            obj.name(),
            fmt_kind(obj.kind()),
            obj.health(),
            obj.logi(),
            threatened,
            capturable,
        );
    }
    if slice.is_empty() {
        let _ = write!(report, "No enemy objectives.\n");
    }
    report
}

// ArgTriple<GroupId, Side, u8>: group, player side, page index (0-based)

fn friendly_status_page(_lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let report = build_friendly_report(&ctx.db, arg.snd, arg.trd as usize);
    ctx.db
        .ephemeral
        .msgs()
        .panel_to_group(20, false, arg.fst, report);
    Ok(())
}

fn enemy_status_page(_lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let report = build_enemy_report(&ctx.db, arg.snd, arg.trd as usize);
    ctx.db
        .ephemeral
        .msgs()
        .panel_to_group(20, false, arg.fst, report);
    Ok(())
}

pub(super) fn init_objectives_menu_for_slot(
    ctx: &mut Context,
    lua: MizLua,
    slot: &SlotId,
) -> Result<()> {
    let mc = MissionCommands::singleton(lua)?;
    let si = ctx
        .db
        .ephemeral
        .get_slot_info(slot)
        .context("getting slot info")?;
    let miz_gid = si.miz_gid;
    let side = si.side;

    // Count pages for each side from the current objective list.
    let friendly_count = ctx
        .db
        .objectives()
        .filter(|(_, obj)| obj.owner() == side)
        .count();
    let enemy_side = match side {
        Side::Blue => Side::Red,
        Side::Red => Side::Blue,
        Side::Neutral => Side::Neutral,
    };
    let enemy_count = ctx
        .db
        .objectives()
        .filter(|(_, obj)| obj.owner() == enemy_side)
        .count();

    let friendly_pages = friendly_count.div_ceil(PAGE_SIZE).max(1);
    let enemy_pages = enemy_count.div_ceil(PAGE_SIZE).max(1);

    mc.remove_submenu_for_group(miz_gid, GroupSubMenu::from(vec!["Objectives".into()]))?;
    let root = mc.add_submenu_for_group(miz_gid, "Objectives".into(), None)?;

    if friendly_pages == 1 {
        mc.add_command_for_group(
            miz_gid,
            "Friendly Status".into(),
            Some(root.clone()),
            friendly_status_page,
            ArgTriple { fst: miz_gid, snd: side, trd: 0u8 },
        )?;
    } else {
        let friendly_root =
            mc.add_submenu_for_group(miz_gid, "Friendly Status".into(), Some(root.clone()))?;
        for page in 0..friendly_pages {
            let start = page * PAGE_SIZE + 1;
            let end = ((page + 1) * PAGE_SIZE).min(friendly_count);
            mc.add_command_for_group(
                miz_gid,
                format_compact!("Page {} ({}-{})", page + 1, start, end).into(),
                Some(friendly_root.clone()),
                friendly_status_page,
                ArgTriple { fst: miz_gid, snd: side, trd: page as u8 },
            )?;
        }
    }

    if enemy_pages == 1 {
        mc.add_command_for_group(
            miz_gid,
            "Enemy Status".into(),
            Some(root.clone()),
            enemy_status_page,
            ArgTriple { fst: miz_gid, snd: side, trd: 0u8 },
        )?;
    } else {
        let enemy_root =
            mc.add_submenu_for_group(miz_gid, "Enemy Status".into(), Some(root.clone()))?;
        for page in 0..enemy_pages {
            let start = page * PAGE_SIZE + 1;
            let end = ((page + 1) * PAGE_SIZE).min(enemy_count);
            mc.add_command_for_group(
                miz_gid,
                format_compact!("Page {} ({}-{})", page + 1, start, end).into(),
                Some(enemy_root.clone()),
                enemy_status_page,
                ArgTriple { fst: miz_gid, snd: side, trd: page as u8 },
            )?;
        }
    }

    Ok(())
}
