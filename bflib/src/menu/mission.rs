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

use super::{ArgTuple, ArgTriple};
use crate::Context;
use anyhow::Result;
use bfprotocols::db::mission::MissionType;
use compact_str::format_compact;
use dcso3::{
    env::miz::GroupId,
    mission_commands::{GroupSubMenu, MissionCommands},
    net::Ucid,
    MizLua,
};

const MISSION_TYPES: &[MissionType] = &[
    MissionType::Strike,
    MissionType::Cap,
    MissionType::Cas,
    MissionType::Sead,
    MissionType::FighterSweep,
    MissionType::Escort,
    MissionType::Reconnaissance,
    MissionType::Transport,
    MissionType::Refueling,
];

/// Callback: open the mission planning root menu
fn open_mission_menu(lua: MizLua, arg: ArgTuple<Ucid, GroupId>) -> Result<()> {
    let _ctx = unsafe { Context::get_mut() };
    let mc = MissionCommands::singleton(lua)?;
    let ucid = arg.fst;
    let gid = arg.snd;

    // Remove the entry point command and rebuild as submenu
    mc.remove_command_for_group(gid, vec!["Missions>>".into()].into())?;
    let root = mc.add_submenu_for_group(gid, "Missions".into(), None)?;

    // "Create Mission" submenu with mission types
    let create_root = mc.add_submenu_for_group(gid, "Create Mission".into(), Some(root.clone()))?;
    for (i, mt) in MISSION_TYPES.iter().enumerate() {
        mc.add_command_for_group(
            gid,
            format!("{mt}").into(),
            Some(create_root.clone()),
            create_mission_callback,
            ArgTriple {
                fst: ucid,
                snd: gid,
                trd: i as u8,
            },
        )?;
    }

    // "View Missions" command
    mc.add_command_for_group(
        gid,
        "View Active Missions".into(),
        Some(root.clone()),
        view_missions_callback,
        ArgTuple { fst: ucid, snd: gid },
    )?;

    // "Join Mission" command
    mc.add_command_for_group(
        gid,
        "Join Latest Mission".into(),
        Some(root.clone()),
        join_mission_callback,
        ArgTuple { fst: ucid, snd: gid },
    )?;

    // "Cancel My Mission" command
    mc.add_command_for_group(
        gid,
        "Cancel My Mission".into(),
        Some(root.clone()),
        cancel_mission_callback,
        ArgTuple { fst: ucid, snd: gid },
    )?;

    // "Complete My Mission" command
    mc.add_command_for_group(
        gid,
        "Complete My Mission".into(),
        Some(root),
        complete_mission_callback,
        ArgTuple { fst: ucid, snd: gid },
    )?;

    Ok(())
}

/// Callback: create a new mission of the selected type
fn create_mission_callback(_lua: MizLua, arg: ArgTriple<Ucid, GroupId, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let ucid = arg.fst;
    let mt_idx = arg.trd;

    let mission_type = MISSION_TYPES.get(mt_idx as usize).copied().unwrap_or(MissionType::Strike);
    let player_name = ctx.db.persisted.players.get(&ucid)
        .map(|p| p.name.clone())
        .unwrap_or_else(|| "Unknown".into());

    let side = ctx.db.persisted.players.get(&ucid)
        .map(|p| p.side)
        .unwrap_or(dcso3::coalition::Side::Neutral);

    let name = format_compact!("{} - {}", mission_type, player_name);
    let briefing = format_compact!("{} mission created by {}", mission_type, player_name);

    let _id = ctx.db.create_mission(name.clone(), mission_type, ucid, side, briefing);

    ctx.db.ephemeral.panel_to_player(
        &ctx.db.persisted,
        10,
        &ucid,
        format_compact!("Mission created: {}", name),
    );

    // Announce to side
    ctx.db.ephemeral.msgs().panel_to_side(
        10,
        false,
        side,
        format_compact!("NEW MISSION: {} (type: {})", name, mission_type),
    );

    Ok(())
}

/// Callback: view active missions for the player's side
fn view_missions_callback(_lua: MizLua, arg: ArgTuple<Ucid, GroupId>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let ucid = arg.fst;

    let side = ctx.db.persisted.players.get(&ucid)
        .map(|p| p.side)
        .unwrap_or(dcso3::coalition::Side::Neutral);

    // Collect mission info before borrowing ephemeral mutably
    use bfprotocols::db::mission::MissionStatus;
    let mission_summaries: Vec<_> = ctx.db.ephemeral.planned_missions().iter()
        .filter(|m| m.side == side && !matches!(m.status, MissionStatus::Completed | MissionStatus::Cancelled))
        .map(|m| {
            let player_count: usize = m.flights.iter().map(|f| f.assigned_players.len()).sum();
            let creator_name = ctx.db.persisted.players.get(&m.creator)
                .map(|p| p.name.clone())
                .unwrap_or_else(|| "Unknown".into());
            format_compact!(
                "[{}] {} | by {} | {} pilots",
                m.mission_type,
                m.name,
                creator_name,
                player_count,
            )
        })
        .collect();

    if mission_summaries.is_empty() {
        ctx.db.ephemeral.panel_to_player(
            &ctx.db.persisted,
            10,
            &ucid,
            format_compact!("No active missions for your side"),
        );
    } else {
        for summary in mission_summaries {
            ctx.db.ephemeral.panel_to_player(
                &ctx.db.persisted,
                15,
                &ucid,
                summary,
            );
        }
    }

    Ok(())
}

/// Callback: join the most recent active mission for the player's side
fn join_mission_callback(_lua: MizLua, arg: ArgTuple<Ucid, GroupId>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let ucid = arg.fst;

    let side = ctx.db.persisted.players.get(&ucid)
        .map(|p| p.side)
        .unwrap_or(dcso3::coalition::Side::Neutral);

    // Find the latest mission for this side
    use bfprotocols::db::mission::MissionStatus;
    let latest_id = ctx.db.ephemeral.planned_missions().iter()
        .filter(|m| m.side == side && !matches!(m.status, MissionStatus::Completed | MissionStatus::Cancelled))
        .last()
        .map(|m| m.id);

    match latest_id {
        None => {
            ctx.db.ephemeral.panel_to_player(
                &ctx.db.persisted,
                10,
                &ucid,
                format_compact!("No active missions to join"),
            );
        }
        Some(mid) => {
            match ctx.db.join_mission(mid, ucid) {
                Ok(()) => {
                    let name = ctx.db.ephemeral.planned_missions().iter()
                        .find(|m| m.id == mid)
                        .map(|m| m.name.clone())
                        .unwrap_or_else(|| "Unknown".into());
                    ctx.db.ephemeral.panel_to_player(
                        &ctx.db.persisted,
                        10,
                        &ucid,
                        format_compact!("Joined mission: {}", name),
                    );
                }
                Err(e) => {
                    ctx.db.ephemeral.panel_to_player(
                        &ctx.db.persisted,
                        10,
                        &ucid,
                        format_compact!("Could not join mission: {}", e),
                    );
                }
            }
        }
    }

    Ok(())
}

/// Callback: cancel the player's most recent mission
fn cancel_mission_callback(_lua: MizLua, arg: ArgTuple<Ucid, GroupId>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let ucid = arg.fst;

    let side = ctx.db.persisted.players.get(&ucid)
        .map(|p| p.side)
        .unwrap_or(dcso3::coalition::Side::Neutral);

    // Find the latest mission created by this player
    let my_mission_id = ctx.db.ephemeral.planned_missions().iter()
        .filter(|m| m.creator == ucid && m.side == side)
        .last()
        .map(|m| m.id);

    match my_mission_id {
        None => {
            ctx.db.ephemeral.panel_to_player(
                &ctx.db.persisted,
                10,
                &ucid,
                format_compact!("You have no missions to cancel"),
            );
        }
        Some(mid) => {
            match ctx.db.cancel_mission(mid, &ucid) {
                Ok(()) => {
                    ctx.db.ephemeral.panel_to_player(
                        &ctx.db.persisted,
                        10,
                        &ucid,
                        format_compact!("Mission cancelled"),
                    );
                    ctx.db.ephemeral.msgs().panel_to_side(
                        10,
                        false,
                        side,
                        format_compact!("MISSION CANCELLED by commander"),
                    );
                }
                Err(e) => {
                    ctx.db.ephemeral.panel_to_player(
                        &ctx.db.persisted,
                        10,
                        &ucid,
                        format_compact!("Could not cancel: {}", e),
                    );
                }
            }
        }
    }

    Ok(())
}

/// Callback: creator marks their latest active mission as complete and triggers reward distribution
fn complete_mission_callback(_lua: MizLua, arg: ArgTuple<Ucid, GroupId>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let ucid = arg.fst;

    let my_mission_id = ctx.db.ephemeral.planned_missions().iter()
        .filter(|m| m.creator == ucid && !m.rewarded)
        .last()
        .map(|m| m.id);

    let mid = match my_mission_id {
        None => {
            ctx.db.ephemeral.panel_to_player(
                &ctx.db.persisted,
                10,
                &ucid,
                format_compact!("You have no active missions to complete"),
            );
            return Ok(());
        }
        Some(id) => id,
    };

    match ctx.db.complete_mission(mid, &ucid) {
        Err(e) => {
            ctx.db.ephemeral.panel_to_player(
                &ctx.db.persisted,
                10,
                &ucid,
                format_compact!("Could not complete mission: {e}"),
            );
        }
        Ok((side, mission_type, name, participants)) => {
            let reward = ctx
                .db
                .ephemeral
                .cfg
                .smart_commander
                .as_ref()
                .map(|sc| sc.mission_rewards.reward_for(mission_type))
                .unwrap_or(0);
            for p_ucid in &participants {
                if reward > 0 {
                    ctx.db.adjust_points(p_ucid, reward, "mission complete");
                }
            }
            ctx.db.ephemeral.msgs().panel_to_side(
                15,
                false,
                side,
                format_compact!(
                    "MISSION COMPLETE: {} | {} pilots awarded {} pts each",
                    name,
                    participants.len(),
                    reward,
                ),
            );
        }
    }
    Ok(())
}

/// Add the mission planning entry point menu for a player group
pub(crate) fn add_mission_menu_for_group(
    mc: &MissionCommands,
    gid: GroupId,
    ucid: Ucid,
) -> Result<()> {
    mc.remove_submenu_for_group(gid, GroupSubMenu::from(vec!["Missions".into()]))?;
    mc.remove_command_for_group(gid, vec!["Missions>>".into()].into())?;
    mc.add_command_for_group(
        gid,
        "Missions>>".into(),
        None,
        open_mission_menu,
        ArgTuple { fst: ucid, snd: gid },
    )?;
    Ok(())
}
