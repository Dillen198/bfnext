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
    db::{
        ephemeral::{SfMission, SfPhase},
        events::CampaignEvent,
    },
    jtac::JtId,
    Context,
};
use anyhow::{Context as ErrContext, Result};
use bfprotocols::cfg::{Cfg, LimitEnforceTyp};
use compact_str::format_compact;
use dcso3::{
    coalition::Side, env::miz::GroupId, mission_commands::MissionCommands, MizLua, String,
    Vector2,
};
use log::info;
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
            let msg = if tr.special_forces {
                format_compact!(
                    "{player} loaded {} (Special Forces — deploy near an HVT to capture it)",
                    tr.name
                )
            } else {
                format_compact!(
                    "{player} loaded {}\n{n} of {} {} deployed, {}",
                    tr.name,
                    tr.limit,
                    tr.name,
                    enforce
                )
            };
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

            // ── Special Forces HVT capture mission setup ──────────────────────
            if tr.special_forces {
                let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
                let ev_cfg = cfg.campaign_events.as_ref();
                if let Some(ev_cfg) = ev_cfg {
                    // Get helo position for proximity check
                    let helo_pos = ctx
                        .db
                        .ephemeral
                        .slot_instance_pos(lua, &slot)
                        .ok()
                        .map(|p| Vector2::new(p.p.x, p.p.z));

                    if let Some(helo_pos) = helo_pos {
                        let detection_radius_sq =
                            ev_cfg.hvt_sf_detection_radius_m.powi(2);

                        // Find a nearby active HVT event that belongs to the ENEMY side
                        // (pilots capture enemy HVTs, not their own side's)
                        let hvt_match = ctx
                            .event_scheduler
                            .active_events
                            .iter()
                            .find_map(|e| {
                                if let CampaignEvent::HighValueTarget {
                                    id,
                                    side: hvt_side,
                                    objective: hvt_obj,
                                    spawn_pos: Some(sp),
                                    reward_points,
                                    ..
                                } = e
                                {
                                    // Skip HVTs belonging to the pilot's own side
                                    if *hvt_side == side {
                                        return None;
                                    }
                                    let dx = sp.x - helo_pos.x;
                                    let dy = sp.y - helo_pos.y;
                                    if dx * dx + dy * dy <= detection_radius_sq {
                                        Some((*id, *sp, *reward_points, *hvt_obj, *hvt_side))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            });

                        if let Some((event_id, hvt_pos, reward_points, hvt_objective, hvt_side)) = hvt_match {
                            let ucid = ctx
                                .db
                                .ephemeral
                                .player_in_slot(&slot)
                                .cloned()
                                .unwrap_or_default();
                            ctx.db.ephemeral.sf_missions.insert(
                                tgid,
                                SfMission {
                                    event_id,
                                    side,
                                    hvt_pos,
                                    drop_pos: helo_pos,
                                    phase: SfPhase::MovingToHvt,
                                    captured_at: None,
                                    ucid,
                                    reward_points,
                                    hvt_objective,
                                    hvt_side,
                                },
                            );
                            // Issue move order toward the HVT position
                            ctx.event_scheduler.pending_moves.insert(tgid, vec![hvt_pos]);
                            info!(
                                "SF team {:?} deployed by {:?}, moving to HVT {:?}",
                                tgid, slot, hvt_pos
                            );
                            ctx.db.ephemeral.msgs().panel_to_group(
                                30,
                                false,
                                gid,
                                format_compact!(
                                    "SF team deployed — moving to HVT position. Come back to extract them after capture!"
                                ),
                            );
                            ctx.db.ephemeral.msgs().panel_to_side(
                                20,
                                false,
                                side,
                                format_compact!(
                                    "{player} deployed Special Forces on HVT capture mission!"
                                ),
                            );
                        } else {
                            ctx.db.ephemeral.msgs().panel_to_group(
                                20,
                                false,
                                gid,
                                format_compact!(
                                    "No HVT target in range ({:.0}km). SF team deployed as regular troops.",
                                    ev_cfg.hvt_sf_detection_radius_m / 1000.0
                                ),
                            );
                            let msg = format_compact!(
                                "{player} dropped {} troops into the field",
                                tr.name
                            );
                            ctx.db
                                .ephemeral
                                .msgs()
                                .panel_to_side(10, false, side, msg);
                        }
                    } else {
                        let msg =
                            format_compact!("{player} dropped {} troops into the field", tr.name);
                        ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg);
                    }
                } else {
                    let msg =
                        format_compact!("{player} dropped {} troops into the field", tr.name);
                    ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg);
                }
            } else {
                // Normal troop deployment message
                let msg = format_compact!("{player} dropped {} troops into the field", tr.name);
                ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg);
            }
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
    match ctx.db.extract_troops(lua, &slot) {
        Ok((tr, extracted_gid)) => {
            let player = player_name(&ctx.db, &slot);

            // ── If the extracted group was an SF mission, move it to sf_cargo ──
            if tr.special_forces {
                if let Some(mission) = ctx.db.ephemeral.sf_missions.remove(&extracted_gid) {
                    if mission.phase == SfPhase::Captured {
                        ctx.db.ephemeral.sf_cargo.insert(slot.clone(), mission);
                        ctx.db.ephemeral.msgs().panel_to_group(
                            30,
                            false,
                            gid,
                            format_compact!(
                                "SF team aboard — return to any friendly base to complete the mission and reveal enemy intel!"
                            ),
                        );
                    } else {
                        // SF extracted before capturing — mission aborted
                        ctx.db.ephemeral.msgs().panel_to_group(
                            15,
                            false,
                            gid,
                            format_compact!(
                                "SF team extracted before reaching the HVT — mission aborted."
                            ),
                        );
                    }
                }
            }

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

            // ── SF mission completion: reveal intel, award points ─────────────
            if tr.special_forces {
                if let Some(mission) = ctx.db.ephemeral.sf_cargo.remove(&slot) {
                    if mission.phase == SfPhase::Captured {
                        let reward = mission.reward_points;

                        // HVT's departed objective loses points (HVT side penalized)
                        if let Some(hvt_obj) = ctx.db.persisted.objectives.get_mut_cow(&mission.hvt_objective) {
                            hvt_obj.points = (hvt_obj.points - reward).max(0);
                            ctx.db.ephemeral.dirty();
                        }
                        // Find the nearest capturing-side objective to the capture position
                        // and award it the points
                        let capture_pos = mission.hvt_pos;
                        if let Some((cap_oid, _)) = crate::db::events::find_nearest_friendly_objective_id(
                            &ctx.db, mission.side, capture_pos, &[],
                        ) {
                            if let Some(cap_obj) = ctx.db.persisted.objectives.get_mut_cow(&cap_oid) {
                                cap_obj.points += reward;
                                ctx.db.ephemeral.dirty();
                            }
                        }
                        // Pilot also gets personal points for the mission
                        ctx.db.adjust_points(
                            &mission.ucid,
                            reward,
                            "for SF HVT capture",
                        );

                        // Expire the HVT campaign event
                        ctx.event_scheduler
                            .active_events
                            .retain(|e| e.id() != mission.event_id);

                        // Intel reveal: place F10 marks on all known enemy group positions
                        let enemy_side = match mission.side {
                            Side::Red => Side::Blue,
                            Side::Blue => Side::Red,
                            s => s,
                        };
                        let side_filter = match mission.side {
                            Side::Red => dcso3::trigger::SideFilter::Red,
                            Side::Blue => dcso3::trigger::SideFilter::Blue,
                            _ => dcso3::trigger::SideFilter::All,
                        };
                        // Collect enemy group positions from persisted state
                        let enemy_groups: Vec<(String, Vector2)> = ctx
                            .db
                            .persisted
                            .groups
                            .into_iter()
                            .filter(|(_, g)| g.side == enemy_side)
                            .filter_map(|(_, g)| {
                                // Get position of first unit in the group
                                g.units
                                    .into_iter()
                                    .find_map(|uid| {
                                        ctx.db.persisted.units.get(uid).map(|u| {
                                            (g.name.clone(), u.pos)
                                        })
                                    })
                            })
                            .collect();

                        for (name, pos) in &enemy_groups {
                            let mid = dcso3::trigger::MarkId::new();
                            ctx.db.ephemeral.msgs().circle_to_all(
                                side_filter,
                                mid,
                                dcso3::trigger::CircleSpec {
                                    center: dcso3::LuaVec3(dcso3::Vector3::new(
                                        pos.x, 50., pos.y,
                                    )),
                                    radius: 200.,
                                    color: dcso3::Color::new(1., 0.1, 0.1, 0.8),
                                    fill_color: dcso3::Color::new(1., 0., 0., 0.15),
                                    line_type: dcso3::trigger::LineType::Solid,
                                    read_only: true,
                                },
                                Some(
                                    format_compact!("INTEL: {name}").into(),
                                ),
                            );
                            // Track for expiry — register under the event id
                            ctx.event_scheduler
                                .register_mark(mission.event_id, mid);
                        }

                        info!(
                            "SF mission complete: {} enemy groups revealed to {:?}",
                            enemy_groups.len(),
                            mission.side
                        );
                        ctx.db.ephemeral.msgs().panel_to_side(
                            45,
                            false,
                            mission.side,
                            format_compact!(
                                "HVT CAPTURED! {player} extracted the SF team. {:?} forces lose {} points. {} enemy positions revealed!",
                                mission.hvt_side, reward, enemy_groups.len()
                            ),
                        );
                        // Schedule intel mark cleanup after 5 minutes
                        // (marks are tied to mission.event_id in event_marks — we schedule a DespawnCap-like
                        // effect by just adding a short-lived expiry event to clean them up)
                        // For simplicity the marks will be cleaned up when their MarkIds are garbage collected
                        // or on server restart. A future improvement can add a timed cleanup event.
                        return Ok(());
                    }
                }
            }

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
    Ok(())
}
