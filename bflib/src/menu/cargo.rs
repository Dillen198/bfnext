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

use super::{ArgTriple, ArgTuple, Pager, player_name, slot_for_group};
use crate::{
    Context,
    db::cargo::{Cargo, Oldest, SlotStats},
};
use anyhow::{Context as ErrContext, Result, anyhow};
use bfprotocols::cfg::{Cfg, LimitEnforceTyp};
use chrono;
use compact_str::{CompactString, ToCompactString, format_compact};
use dcso3::{
    MizLua, String,
    coalition::Side,
    env::miz::GroupId,
    mission_commands::{GroupSubMenu, MissionCommands},
    net::{SlotId, Ucid},
};
use fxhash::FxHashMap;

fn unpakistan(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    match ctx.db.unpakistan(lua, &ctx.idx, &slot) {
        Ok(unpakistan) => {
            let player = player_name(&ctx.db, &slot);
            let msg = format_compact!("{player} {unpakistan}");
            ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg);
        }
        Err(e) => {
            let msg = format_compact!("{}", e);
            ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg)
        }
    }
    Ok(())
}

fn load_crate(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    match ctx.db.load_nearby_crate(lua, &slot) {
        Ok(cr) => {
            let (dep_name, limit_enforce, limit) = match ctx.db.deployable_by_crate(&side, &cr.name)
            {
                Some((dep_name, dep)) => (dep_name, &dep.limit_enforce, Some(dep.limit)),
                None => (&cr.name, &LimitEnforceTyp::DenyCrate, None),
            };
            let (n, oldest) = ctx
                .db
                .number_deployed(side, dep_name.as_str())
                .with_context(|| format_compact!("getting number of {} deployed", dep_name))?;
            let enforce = match limit_enforce {
                LimitEnforceTyp::DenyCrate => {
                    format_compact!("unpacking will be denied when the limit is exceeded")
                }
                LimitEnforceTyp::DeleteOldest => match oldest {
                    Some(Oldest::Group(gid)) => {
                        format_compact!(
                            "unpacking will delete oldest, {}, when the limit is exceeded",
                            gid
                        )
                    }
                    Some(Oldest::Objective(oid)) => {
                        format_compact!(
                            "unpacking will delete oldest, {}, when the limit is exceeded",
                            oid
                        )
                    }
                    None => {
                        format_compact!("unpacking will delete oldest when the limit is exceeded")
                    }
                },
            };
            let limit = limit
                .map(|i| i.to_compact_string())
                .unwrap_or_else(|| format_compact!("unlimited"));
            let msg = format_compact!(
                "{} crate loaded\n{n} of {} {} deployed, {}",
                cr.name,
                limit,
                dep_name,
                enforce
            );
            ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg)
        }
        Err(e) => {
            let msg = format_compact!("crate could not be loaded: {}", e);
            ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg)
        }
    }
    Ok(())
}

fn unload_crate(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    match ctx.db.unload_crate(lua, &ctx.idx, &slot) {
        Ok(cr) => {
            let msg = format_compact!("{} crate unloaded", cr.name);
            ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg)
        }
        Err(e) => {
            let msg = format_compact!("{}", e);
            ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg)
        }
    }
    Ok(())
}

pub(crate) fn list_cargo_for_slot(ctx: &mut Context, slot: &SlotId) -> Result<()> {
    let cargo = Cargo::default();
    let cargo = ctx.db.list_cargo(&slot).unwrap_or(&cargo);
    let sifo = ctx
        .db
        .ephemeral
        .get_slot_info(slot)
        .ok_or_else(|| anyhow!("invalid slot"))?;
    let capacity = ctx
        .db
        .cargo_capacity(&sifo.typ)
        .context("getting unit cargo capacity")?;
    let mut msg = CompactString::new("Current Cargo\n----------------------------\n");
    msg.push_str(&format_compact!(
        "troops: {} of {}\n",
        cargo.num_troops(),
        capacity.troop_slots
    ));
    msg.push_str(&format_compact!(
        "crates: {} of {}\n",
        cargo.num_crates(),
        capacity.crate_slots
    ));
    if capacity.pilot_slots > 0 {
        msg.push_str(&format_compact!(
            "pilots: {} of {}\n",
            cargo.num_pilots(),
            capacity.pilot_slots
        ));
    }
    msg.push_str(&format_compact!(
        "total : {} of {}\n",
        cargo.num_total(),
        capacity.total_slots
    ));
    msg.push_str("----------------------------\n");
    let mut total = 0;
    for (_, cr) in &cargo.crates {
        msg.push_str(&format_compact!(
            "{} crate weighing {} kg\n",
            cr.name,
            cr.weight
        ));
        total += cr.weight
    }
    for it in &cargo.troops {
        msg.push_str(&format_compact!(
            "{} troop weighing {} kg\n",
            it.troop.name,
            it.troop.weight
        ));
        total += it.troop.weight
    }
    for p in &cargo.pilots {
        msg.push_str(&format_compact!("downed pilot: {}\n", p.name));
    }
    if total > 0 {
        msg.push_str("----------------------------\n");
    }
    msg.push_str(&format_compact!("total cargo weight: {} kg", total as u32));
    ctx.db
        .ephemeral
        .msgs()
        .panel_to_unit(15, false, slot.as_unit_id().unwrap(), msg);
    Ok(())
}

pub fn list_current_cargo(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    list_cargo_for_slot(ctx, &slot)
}

fn list_nearby_crates(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    let st = SlotStats::get(&ctx.db, lua, &slot).context("getting slot stats")?;
    let nearby = ctx
        .db
        .list_nearby_crates(&st)
        .context("listing nearby crates")?;
    if nearby.len() > 0 {
        let mut msg = CompactString::new("");
        for nc in nearby {
            msg.push_str(&format_compact!(
                "{} crate, bearing {}, {} meters away\n",
                nc.crate_def.name,
                nc.heading as u32,
                nc.distance as u32
            ));
        }
        ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg)
    } else {
        drop(nearby);
        ctx.db
            .ephemeral
            .msgs()
            .panel_to_group(10, false, gid, "No nearby crates")
    }
    Ok(())
}

fn destroy_nearby_crate(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    if let Err(e) = ctx.db.destroy_nearby_crate(lua, &slot) {
        ctx.db
            .ephemeral
            .msgs()
            .panel_to_group(10, false, gid, format_compact!("{}", e))
    }
    Ok(())
}

fn destroy_all_nearby_crates(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    match ctx.db.destroy_all_nearby_crates(lua, &slot) {
        Ok(n) => {
            let msg = format_compact!("destroyed {} nearby crate{}", n, if n == 1 { "" } else { "s" });
            ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg)
        }
        Err(e) => ctx
            .db
            .ephemeral
            .msgs()
            .panel_to_group(10, false, gid, format_compact!("{}", e)),
    }
    Ok(())
}

fn spawn_crate(lua: MizLua, arg: ArgTuple<GroupId, String>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_side, slot) = slot_for_group(lua, ctx, &arg.fst).context("getting slot for group")?;
    match ctx.db.spawn_crate(lua, &ctx.idx, &slot, &arg.snd) {
        Err(e) => {
            ctx.db
                .ephemeral
                .msgs()
                .panel_to_group(10, false, arg.fst, format_compact!("{e}"))
        }
        Ok(st) => {
            if let Some(max_crates) = ctx.db.ephemeral.cfg.max_crates {
                let (n, oldest) = ctx
                    .db
                    .number_crates_deployed(&st)
                    .context("getting number of deployed crates")?;
                let msg = match oldest {
                    None => format_compact!("{n} of {max_crates} crates spawned"),
                    Some(gid) => format_compact!(
                        "{n} of {max_crates} crates spawned, {gid} will be deleted if the limit is exceeded"
                    ),
                };
                ctx.db
                    .ephemeral
                    .msgs()
                    .panel_to_group(10, false, arg.fst, msg)
            }
        }
    }
    Ok(())
}

// C-130 Physical Cargo Menu Handlers
fn spawn_c130_crate(lua: MizLua, arg: ArgTuple<GroupId, String>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &arg.fst).context("getting slot for group")?;
    let origin = ctx.db.player_current_objective_id(&slot)?;

    match ctx.db.spawn_c130_crate(lua, &ctx.idx, &slot, arg.snd.clone(), side, origin, true) {
        Ok(msg) => {
            ctx.db.ephemeral.msgs().panel_to_group(10, false, arg.fst, msg);
        }
        Err(e) => {
            let msg = format_compact!("Failed to spawn crate: {}", e);
            ctx.db.ephemeral.msgs().panel_to_group(10, false, arg.fst, msg);
        }
    }
    Ok(())
}

fn spawn_c130_vehicle(lua: MizLua, arg: ArgTuple<GroupId, String>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &arg.fst).context("getting slot for group")?;
    let origin = ctx.db.player_current_objective_id(&slot)?;

    match ctx.db.spawn_c130_vehicle(lua, &ctx.idx, &slot, arg.snd.clone(), side, origin) {
        Ok(msg) => {
            ctx.db.ephemeral.msgs().panel_to_group(10, false, arg.fst, msg);
        }
        Err(e) => {
            let msg = format_compact!("Failed to spawn vehicle: {}", e);
            ctx.db.ephemeral.msgs().panel_to_group(10, false, arg.fst, msg);
        }
    }
    Ok(())
}

/// Shared by the F10 menu closures below *and* the cockpit-UI RPC handler
/// (`AdminCommand::CockpitSpawnCrate` in admin.rs): queue `qty` copies of a
/// named crate for whichever slot `ucid` currently occupies, staggered the
/// same way "Spawn All Crates" already does. Returns the status message
/// either UI surfaces to the player.
pub(crate) fn spawn_crates_for_ucid(
    ctx: &mut Context,
    lua: MizLua,
    ucid: &Ucid,
    crate_name: &str,
    qty: u32,
    auto_unpack: bool,
) -> Result<String> {
    let player = ctx.db.player(ucid).ok_or_else(|| anyhow!("unknown player"))?;
    let side = player.side;
    let slot = player
        .current_slot
        .as_ref()
        .map(|(slot, _)| slot.clone())
        .ok_or_else(|| anyhow!("you must be in a slot to spawn crates"))?;
    let origin = ctx.db.player_current_objective_id(&slot)?;

    // Resolve the crate def from every source the single-spawn path knows
    // about -- base-supply crates (fuel/weapons resupply, carrier repair,
    // logistics repair kit) as well as deployables -- so "spawn N" works for
    // all of them, not just deployable crates.
    let cfg = &ctx.db.ephemeral.cfg;
    let whcfg = cfg.warehouse.as_ref();
    let crate_def = whcfg
        .and_then(|w| w.supply_transfer_fuel_crate.get(&side))
        .filter(|cr| cr.name.as_str() == crate_name)
        .or_else(|| {
            whcfg
                .and_then(|w| w.supply_transfer_weapons_crate.get(&side))
                .filter(|cr| cr.name.as_str() == crate_name)
        })
        .or_else(|| {
            whcfg
                .and_then(|w| w.carrier_repair_crate.get(&side))
                .filter(|cr| cr.name.as_str() == crate_name)
        })
        .or_else(|| {
            cfg.repair_crate
                .get(&side)
                .filter(|cr| cr.name.as_str() == crate_name)
        })
        .or_else(|| {
            cfg.deployables
                .get(&side)
                .and_then(|deps| {
                    deps.iter().flat_map(|d| &d.crates).find(|cr| cr.name.as_str() == crate_name)
                })
        })
        .cloned()
        .ok_or_else(|| anyhow!("crate {} not found", crate_name))?;

    let name = String::from(crate_name);
    let crate_list: Vec<_> = std::iter::repeat((name, crate_def)).take(qty as usize).collect();
    ctx.db.queue_c130_crate_spawns(lua, &slot, crate_list, side, origin, auto_unpack)
}

/// Shared by `spawn_n_c130_crates`/`spawn_n_helo_crates`: resolves the
/// player behind `arg.fst` (a group id) to a ucid and delegates to
/// `spawn_crates_for_ucid`.
fn spawn_n_crates(
    lua: MizLua,
    arg: &ArgTriple<GroupId, String, u32>,
    auto_unpack: bool,
) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_side, slot) = slot_for_group(lua, ctx, &arg.fst).context("getting slot for group")?;
    let ucid = *ctx
        .db
        .ephemeral
        .player_in_slot(&slot)
        .ok_or_else(|| anyhow!("no player in slot"))?;

    match spawn_crates_for_ucid(ctx, lua, &ucid, arg.snd.as_str(), arg.trd, auto_unpack) {
        Ok(msg) => {
            ctx.db.ephemeral.msgs().panel_to_group(10, false, arg.fst, msg);
        }
        Err(e) => {
            let msg = format_compact!("Failed to queue crates: {}", e);
            ctx.db.ephemeral.msgs().panel_to_group(10, false, arg.fst, msg);
        }
    }
    Ok(())
}

fn spawn_n_c130_crates(lua: MizLua, arg: ArgTriple<GroupId, String, u32>) -> Result<()> {
    spawn_n_crates(lua, &arg, true)
}

fn spawn_n_helo_crates(lua: MizLua, arg: ArgTriple<GroupId, String, u32>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let auto_unpack = ctx.db.ephemeral.cfg.helo_cargo.as_ref().map(|c| c.auto_unpack).unwrap_or(false);
    spawn_n_crates(lua, &arg, auto_unpack)
}

fn list_downed_pilots(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    let st = SlotStats::get(&ctx.db, lua, &slot).context("getting slot stats")?;
    let pilots: Vec<_> = ctx
        .db
        .persisted
        .downed_pilots
        .into_iter()
        .filter_map(|pgid| {
            let group = ctx.db.persisted.groups.get(pgid)?;
            if group.side != side {
                return None;
            }
            let pos = group
                .units
                .into_iter()
                .filter_map(|uid| ctx.db.persisted.units.get(uid))
                .filter(|u| !u.dead)
                .map(|u| u.pos)
                .next()?;
            let name = match &group.origin {
                crate::db::group::DeployKind::DownedPilot { name, .. } => name.clone(),
                _ => return None,
            };
            let dx = pos.x - st.point.x;
            let dy = pos.y - st.point.y;
            let dist = (dx * dx + dy * dy).sqrt() as u32;
            let bearing = {
                use dcso3::azumith2d_to;
                (azumith2d_to(st.point, pos).to_degrees() as u32 + 360) % 360
            };
            Some((name, dist, bearing))
        })
        .collect();
    if pilots.is_empty() {
        ctx.db
            .ephemeral
            .msgs()
            .panel_to_group(10, false, gid, "No downed pilots on your side");
    } else {
        let mut msg = CompactString::new("Downed Pilots\n----------------------------\n");
        for (name, dist, bearing) in &pilots {
            msg.push_str(&format_compact!(
                "{name}: {bearing}° / {dist}m\n"
            ));
        }
        ctx.db.ephemeral.msgs().panel_to_group(15, false, gid, msg);
    }
    Ok(())
}

fn request_smoke(lua: MizLua, gid: GroupId) -> Result<()> {
    use dcso3::trigger::{SmokeColor, Trigger};
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    let st = SlotStats::get(&ctx.db, lua, &slot).context("getting slot stats")?;
    // Find the nearest downed pilot of the player's side
    let nearest = ctx
        .db
        .persisted
        .downed_pilots
        .into_iter()
        .filter_map(|pgid| {
            let group = ctx.db.persisted.groups.get(pgid)?;
            if group.side != side {
                return None;
            }
            let pos = group
                .units
                .into_iter()
                .filter_map(|uid| ctx.db.persisted.units.get(uid))
                .filter(|u| !u.dead)
                .map(|u| u.pos)
                .next()?;
            let name = match &group.origin {
                crate::db::group::DeployKind::DownedPilot { name, .. } => name.clone(),
                _ => return None,
            };
            let dx = pos.x - st.point.x;
            let dy = pos.y - st.point.y;
            Some((*pgid, name, pos, dx * dx + dy * dy))
        })
        .min_by(|a, b| a.3.partial_cmp(&b.3).unwrap());
    match nearest {
        None => {
            ctx.db
                .ephemeral
                .msgs()
                .panel_to_group(10, false, gid, "No downed pilots nearby");
        }
        Some((pgid, name, pos, dist2)) => {
            let dist = dist2.sqrt() as u32;
            // Check smoke cooldown
            let cooldown_secs = ctx
                .db
                .ephemeral
                .cfg
                .csar
                .as_ref()
                .map(|c| c.smoke_cooldown as i64)
                .unwrap_or(300);
            let now: chrono::DateTime<chrono::Utc> = chrono::Utc::now();
            let last_smoke = ctx
                .db
                .ephemeral
                .csar_smoke_cooldown
                .get(&pgid)
                .copied();
            if let Some(last) = last_smoke {
                let elapsed = now - last;
                if elapsed < chrono::Duration::seconds(cooldown_secs) {
                    let remaining = cooldown_secs - elapsed.num_seconds();
                    let msg = format_compact!(
                        "{name} smoke on cooldown — {remaining}s remaining"
                    );
                    ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg);
                    return Ok(());
                }
            }
            let act = Trigger::singleton(lua)?.action()?;
            use dcso3::land::Land;
            let alt = Land::singleton(lua)?
                .get_height(dcso3::LuaVec2(pos))
                .unwrap_or(0.);
            let smoke_pos = dcso3::LuaVec3(dcso3::Vector3::new(pos.x, alt + 1., pos.y));
            if let Err(e) = act.smoke(smoke_pos, SmokeColor::Green) {
                let msg = format_compact!("smoke failed: {e}");
                ctx.db.ephemeral.msgs().panel_to_group(10, false, gid, msg);
            } else {
                ctx.db.ephemeral.csar_smoke_cooldown.insert(pgid, now);
                let msg =
                    format_compact!("{name} popped green smoke — {dist}m away");
                ctx.db.ephemeral.msgs().panel_to_group(15, false, gid, msg);
            }
        }
    }
    Ok(())
}

fn pickup_pilot(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    match ctx.db.pickup_pilot(lua, &slot) {
        Ok(pilot_name) => {
            let player = player_name(&ctx.db, &slot);
            let msg = format_compact!("{player} picked up downed pilot {pilot_name}");
            ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg);
        }
        Err(e) => {
            ctx.db
                .ephemeral
                .msgs()
                .panel_to_group(10, false, gid, format_compact!("{e}"))
        }
    }
    Ok(())
}

fn deliver_pilots(lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &gid).context("getting slot for group")?;
    let player = player_name(&ctx.db, &slot);
    match ctx.db.deliver_pilots(lua, &slot) {
        Err(e) => {
            ctx.db
                .ephemeral
                .msgs()
                .panel_to_group(10, false, gid, format_compact!("{e}"))
        }
        Ok(pilots) => {
            let rescue_reward = ctx
                .db
                .ephemeral
                .cfg
                .csar
                .as_ref()
                .map(|c| c.rescue_reward)
                .unwrap_or(0);
            let rescuer_ucid = ctx.db.ephemeral.player_in_slot(&slot).cloned();
            for pilot in &pilots {
                if let Some(new_count) = ctx.db.restore_life(&pilot.ucid, pilot.life_type) {
                    let msg = format_compact!(
                        "your pilot {} was rescued by {player} and delivered safely — you now have {new_count} {} lives",
                        pilot.name,
                        pilot.life_type,
                    );
                    ctx.db.ephemeral.panel_to_player(
                        &ctx.db.persisted,
                        15,
                        &pilot.ucid,
                        msg,
                    );
                }
                if rescue_reward > 0 {
                    if let Some(ucid) = &rescuer_ucid {
                        ctx.db.adjust_points(
                            ucid,
                            rescue_reward as i32,
                            &format_compact!("for CSAR rescue of {}", pilot.name),
                        );
                    }
                }
            }
            let n = pilots.len();
            let msg = format_compact!(
                "{player} delivered {n} rescued pilot{} to safety",
                if n == 1 { "" } else { "s" }
            );
            ctx.db.ephemeral.msgs().panel_to_side(10, false, side, msg);
        }
    }
    Ok(())
}

/// The deployable category tree under "Crates". Category names are unique
/// across the config (see the "Base Supply" note below), so one map keyed by
/// name is enough to find any node again.
struct CrateTree {
    group: GroupId,
    menus: FxHashMap<String, GroupSubMenu>,
    /// The pager filling each node. A node's children -- sub-categories and
    /// crate entries alike -- all claim their slot from it, so no level of the
    /// tree can quietly pass DCS's hard limit of 10 entries per menu.
    pagers: FxHashMap<String, Pager>,
    /// The pager filling "Crates" itself, which the top-level categories sit on.
    root: Pager,
}

impl CrateTree {
    /// `used` is how many entries the caller already put on `root` before the
    /// category list starts.
    fn new(group: GroupId, root: GroupSubMenu, used: u32) -> Self {
        CrateTree {
            group,
            menus: FxHashMap::default(),
            pagers: FxHashMap::default(),
            root: Pager::with_used(group, root, used),
        }
    }

    /// The page a new entry should go on directly under the tree's own root.
    fn root_page(&mut self, mc: &MissionCommands) -> Result<GroupSubMenu> {
        self.root.page(mc)
    }

    /// The page a new entry should go on inside category `name`.
    fn page_in(&mut self, mc: &MissionCommands, name: &String) -> Result<GroupSubMenu> {
        let menu = self
            .menus
            .get(name)
            .ok_or_else(|| anyhow!("no category menu {name}"))?
            .clone();
        let group = self.group;
        self.pagers
            .entry(name.clone())
            .or_insert_with(|| Pager::new(group, menu))
            .page(mc)
    }

    /// Resolve `path` to its leaf submenu, creating intermediate submenus on
    /// demand. When the leaf is the deployable's own priced entry it gets a
    /// "(N pts)" suffix.
    fn category(
        &mut self,
        mc: &MissionCommands,
        path: &[String],
        cost: u32,
    ) -> Result<GroupSubMenu> {
        let leaf = path.last().unwrap();
        for (i, p) in path.iter().enumerate() {
            if self.menus.contains_key(p) {
                continue;
            }
            let parent = if i == 0 {
                self.root.page(mc)?
            } else {
                self.page_in(mc, &path[i - 1])?
            };
            let item = if p == leaf && cost > 0 {
                String::from(format_compact!("{p}({cost} pts)"))
            } else {
                p.clone()
            };
            let menu = mc.add_submenu_for_group(self.group, item, Some(parent))?;
            self.menus.insert(p.clone(), menu);
        }
        Ok(self.menus[leaf].clone())
    }
}

pub(super) fn add_cargo_menu_for_group(
    cfg: &Cfg,
    mc: &MissionCommands,
    side: &Side,
    group: GroupId,
) -> Result<()> {
    let root = mc.add_submenu_for_group(group, "Cargo".into(), None)?;
    mc.add_command_for_group(
        group,
        "Unpack Nearby Crate(s)".into(),
        Some(root.clone()),
        unpakistan,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Load Nearby Crate".into(),
        Some(root.clone()),
        load_crate,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Unload Crate".into(),
        Some(root.clone()),
        unload_crate,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "List Nearby Crates".into(),
        Some(root.clone()),
        list_nearby_crates,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "List Cargo".into(),
        Some(root.clone()),
        list_current_cargo,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Destroy Nearby Crate".into(),
        Some(root.clone()),
        destroy_nearby_crate,
        group,
    )?;
    let root = mc.add_submenu_for_group(group, "Crates".into(), Some(root.clone()))?;
    let rep = &cfg.repair_crate[side];
    // "Base Supply", not "Logistics" -- a deployable's own category path can be
    // named "Logistics" too (e.g. Ammo Truck), and two addSubMenuForGroup calls
    // with the same name+parent collide in DCS, breaking both menus.
    let logi = mc.add_submenu_for_group(group, "Base Supply".into(), Some(root.clone()))?;
    mc.add_command_for_group(
        group,
        rep.name.clone(),
        Some(logi.clone()),
        spawn_crate,
        ArgTuple {
            fst: group,
            snd: rep.name.clone(),
        },
    )?;
    if let Some(whcfg) = &cfg.warehouse {
        // Add fuel transfer crate menu item
        if let Some(fuel_cr) = whcfg.supply_transfer_fuel_crate.get(&side) {
            mc.add_command_for_group(
                group,
                fuel_cr.name.clone(),
                Some(logi.clone()),
                spawn_crate,
                ArgTuple {
                    fst: group,
                    snd: fuel_cr.name.clone(),
                },
            )?;
        }
        // Add weapons transfer crate menu item
        if let Some(weapons_cr) = whcfg.supply_transfer_weapons_crate.get(&side) {
            mc.add_command_for_group(
                group,
                weapons_cr.name.clone(),
                Some(logi.clone()),
                spawn_crate,
                ArgTuple {
                    fst: group,
                    snd: weapons_cr.name.clone(),
                },
            )?;
        }
    }
    // "Base Supply" already occupies one slot directly under "Crates".
    let mut tree = CrateTree::new(group, root.clone(), 1);
    for dep in cfg.deployables.get(side).unwrap_or(&vec![]) {
        if dep.crates.is_empty() && dep.repair_crate.is_none() {
            continue;
        }
        tree.category(mc, &dep.path, dep.cost)?;
        let leaf = dep.path.last().unwrap().clone();
        for cr in dep.crates.iter().chain(dep.repair_crate.iter()) {
            let title = if cr.required > 1 {
                String::from(format_compact!("{}({})", cr.name, cr.required))
            } else {
                cr.name.clone()
            };
            let page = tree.page_in(mc, &leaf)?;
            mc.add_command_for_group(
                group,
                title,
                Some(page),
                spawn_crate,
                ArgTuple {
                    fst: group,
                    snd: cr.name.clone(),
                },
            )?;
        }
    }
    Ok(())
}

pub(super) fn add_csar_menu_for_group(mc: &MissionCommands, group: GroupId) -> Result<()> {
    let root = mc.add_submenu_for_group(group, "CSAR".into(), None)?;
    mc.add_command_for_group(
        group,
        "List Downed Pilots".into(),
        Some(root.clone()),
        list_downed_pilots,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Request Nearest Pilot Smoke".into(),
        Some(root.clone()),
        request_smoke,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Pick Up Downed Pilot (manual)".into(),
        Some(root.clone()),
        pickup_pilot,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Deliver Rescued Pilots (manual)".into(),
        Some(root.clone()),
        deliver_pilots,
        group,
    )?;
    Ok(())
}

pub(super) fn add_c130_cargo_menu_for_group(
    cfg: &Cfg,
    mc: &MissionCommands,
    side: &Side,
    group: GroupId,
) -> Result<()> {
    let root = mc.add_submenu_for_group(group, "C-130 Cargo".into(), None)?;

    // Add utility commands at top level
    mc.add_command_for_group(
        group,
        "List Nearby Crates".into(),
        Some(root.clone()),
        list_nearby_crates,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Delete Nearby Crate".into(),
        Some(root.clone()),
        destroy_nearby_crate,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Delete All Nearby Crates".into(),
        Some(root.clone()),
        destroy_all_nearby_crates,
        group,
    )?;

    let crates_menu = mc.add_submenu_for_group(group, "Crates".into(), Some(root.clone()))?;

    // Add logistics submenu (base repair, supply transfer, and carrier repair).
    // "Base Supply", not "Logistics" -- a deployable's own category path can be
    // named "Logistics" too (e.g. Ammo Truck), and two addSubMenuForGroup calls
    // with the same name+parent collide in DCS, breaking both menus.
    if cfg.warehouse.is_some() || !cfg.repair_crate.is_empty() {
        let logi = mc.add_submenu_for_group(group, "Base Supply".into(), Some(crates_menu.clone()))?;
        // One base-supply crate: the single "spawn 1" command plus a "xN"
        // submenu (1-9) so a poor base can be topped off / a wreck fully
        // repaired in one trip, same as deployable crates.
        // Two entries per crate (the command and its xN submenu), so four
        // base-supply crates already fill most of a page -- pager it.
        let mut supply = Pager::new(group, logi);
        let mut add_supply = |name: &str| -> Result<()> {
            supply.command(
                mc,
                String::from(name),
                spawn_c130_crate,
                ArgTuple { fst: group, snd: String::from(name) },
            )?;
            let qty = supply.submenu(mc, String::from(format_compact!("{name} xN")))?;
            for n in 1..=9u32 {
                mc.add_command_for_group(
                    group,
                    String::from(format_compact!("{n}")),
                    Some(qty.clone()),
                    spawn_n_c130_crates,
                    ArgTriple { fst: group, snd: String::from(name), trd: n },
                )?;
            }
            Ok(())
        };
        if let Some(rep) = cfg.repair_crate.get(side) {
            add_supply(rep.name.as_str())?;
        }
        if let Some(whcfg) = &cfg.warehouse {
            if let Some(fuel_cr) = whcfg.supply_transfer_fuel_crate.get(side) {
                add_supply(fuel_cr.name.as_str())?;
            }
            if let Some(weapons_cr) = whcfg.supply_transfer_weapons_crate.get(side) {
                add_supply(weapons_cr.name.as_str())?;
            }
            if !whcfg.carrier_repair_crate.is_empty() {
                add_supply(whcfg.carrier_repair_crate[&side].name.as_str())?;
            }
        }
    }

    // Add all deployable crates (organized by path, excluding repair crates)
    let base_items = if cfg.warehouse.is_some() || !cfg.repair_crate.is_empty() { 1 } else { 0 };
    let mut tree = CrateTree::new(group, crates_menu.clone(), base_items);
    for dep in cfg.deployables.get(side).unwrap_or(&vec![]) {
        if dep.crates.is_empty() && dep.repair_crate.is_none() {
            continue;
        }

        tree.category(mc, &dep.path, dep.cost)?;
        let leaf = dep.path.last().unwrap().clone();

        // The per-deployable repair crate (revives that deployed group after
        // it's been shot up) is a plain single command -- no xN quantity menu,
        // same as the helo cargo menu.
        if let Some(rc) = &dep.repair_crate {
            let title = if rc.required > 1 {
                String::from(format_compact!("{}({})", rc.name, rc.required))
            } else {
                rc.name.clone()
            };
            mc.add_command_for_group(
                group,
                title,
                Some(tree.page_in(mc, &leaf)?),
                spawn_c130_crate,
                ArgTuple { fst: group, snd: rc.name.clone() },
            )?;
        }

        for cr in &dep.crates {
            let title = if cr.required > 1 {
                String::from(format_compact!("{}({})", cr.name, cr.required))
            } else {
                cr.name.clone()
            };
            mc.add_command_for_group(
                group,
                title,
                Some(tree.page_in(mc, &leaf)?),
                spawn_c130_crate,
                ArgTuple {
                    fst: group,
                    snd: cr.name.clone(),
                },
            )?;
            // Per-crate quantity submenu: pick 1-9 of this crate to spawn at
            // once. The name must include the crate name -- a leaf category
            // with more than one crate (e.g. "SA 3") would otherwise call
            // add_submenu_for_group with the same name+parent once per crate,
            // which collides in DCS and corrupts the menu / binds the quantity
            // commands to the wrong crate.
            let qty_menu = mc.add_submenu_for_group(
                group,
                String::from(format_compact!("{} xN", cr.name)),
                Some(tree.page_in(mc, &leaf)?),
            )?;
            for n in 1..=9u32 {
                mc.add_command_for_group(
                    group,
                    String::from(format_compact!("{n}")),
                    Some(qty_menu.clone()),
                    spawn_n_c130_crates,
                    ArgTriple { fst: group, snd: cr.name.clone(), trd: n },
                )?;
            }
        }
    }

    // Add Vehicles submenu for loadable vehicles (if configured)
    if let Some(c130_cfg) = &cfg.c130_cargo {
        if let Some(vehicles) = c130_cfg.loadable_vehicles.get(side) {
            if !vehicles.is_empty() {
                let vehicles_menu =
                    mc.add_submenu_for_group(group, "Vehicles".into(), Some(root.clone()))?;
                // Same tree as the crate categories: an optional path per
                // vehicle, every level of it paged.
                let mut tree = CrateTree::new(group, vehicles_menu, 0);

                for vehicle in vehicles {
                    let page = if vehicle.path.is_empty() {
                        tree.root_page(mc)?
                    } else {
                        tree.category(mc, &vehicle.path, 0)?;
                        tree.page_in(mc, vehicle.path.last().unwrap())?
                    };

                    // Create menu item with cost if applicable
                    let title = if vehicle.cost > 0 {
                        String::from(format_compact!("{} ({} pts)", vehicle.name, vehicle.cost))
                    } else {
                        String::from(vehicle.name.clone())
                    };

                    mc.add_command_for_group(
                        group,
                        title,
                        Some(page),
                        spawn_c130_vehicle,
                        ArgTuple {
                            fst: group,
                            snd: String::from(vehicle.name.clone()),
                        },
                    )?;
                }
            }
        }
    }

    Ok(())
}

// Helicopter Dynamic Cargo Menu Handlers

fn spawn_helo_crate(lua: MizLua, arg: ArgTuple<GroupId, String>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (side, slot) = slot_for_group(lua, ctx, &arg.fst).context("getting slot for group")?;
    let origin = ctx.db.player_current_objective_id(&slot)?;
    let auto_unpack = ctx.db.ephemeral.cfg.helo_cargo.as_ref().map(|c| c.auto_unpack).unwrap_or(false);

    match ctx.db.spawn_c130_crate(lua, &ctx.idx, &slot, arg.snd.clone(), side, origin, auto_unpack) {
        Ok(msg) => {
            ctx.db.ephemeral.msgs().panel_to_group(10, false, arg.fst, msg);
        }
        Err(e) => {
            let msg = format_compact!("Failed to spawn crate: {}", e);
            ctx.db.ephemeral.msgs().panel_to_group(10, false, arg.fst, msg);
        }
    }
    Ok(())
}

fn unpack_helo_crates(lua: MizLua, arg: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (_, slot) = slot_for_group(lua, ctx, &arg).context("getting slot for group")?;
    match ctx.db.unpack_nearby_helo_crates(lua, &ctx.idx, &slot) {
        Ok(msg) => ctx.db.ephemeral.msgs().panel_to_group(10, false, arg, msg),
        Err(e) => ctx.db.ephemeral.msgs().panel_to_group(10, false, arg, format_compact!("{e}")),
    }
    Ok(())
}

pub(super) fn add_helo_cargo_menu_for_group(
    cfg: &Cfg,
    mc: &MissionCommands,
    side: &Side,
    group: GroupId,
) -> Result<()> {
    let root = mc.add_submenu_for_group(group, "Cargo".into(), None)?;

    mc.add_command_for_group(
        group,
        "Unpack Nearby Crate(s)".into(),
        Some(root.clone()),
        unpack_helo_crates,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "List Nearby Crates".into(),
        Some(root.clone()),
        list_nearby_crates,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "List Cargo".into(),
        Some(root.clone()),
        list_current_cargo,
        group,
    )?;
    mc.add_command_for_group(
        group,
        "Destroy Nearby Crate".into(),
        Some(root.clone()),
        destroy_nearby_crate,
        group,
    )?;

    let crates_menu = mc.add_submenu_for_group(group, "Crates".into(), Some(root.clone()))?;

    // "Base Supply", not "Logistics" -- a deployable's own category path can be
    // named "Logistics" too (e.g. Ammo Truck), and two addSubMenuForGroup calls
    // with the same name+parent collide in DCS, breaking both menus. Build it
    // once and hang every base-supply crate off it.
    if cfg.warehouse.is_some() || !cfg.repair_crate.is_empty() {
        let logi = mc.add_submenu_for_group(group, "Base Supply".into(), Some(crates_menu.clone()))?;
        // "spawn 1" command + a "xN" submenu (1-9) per base-supply crate.
        // Two entries per crate (the command and its xN submenu), so four
        // base-supply crates already fill most of a page -- pager it.
        let mut supply = Pager::new(group, logi);
        let mut add_supply = |name: &str| -> Result<()> {
            supply.command(
                mc,
                String::from(name),
                spawn_helo_crate,
                ArgTuple { fst: group, snd: String::from(name) },
            )?;
            let qty = supply.submenu(mc, String::from(format_compact!("{name} xN")))?;
            for n in 1..=9u32 {
                mc.add_command_for_group(
                    group,
                    String::from(format_compact!("{n}")),
                    Some(qty.clone()),
                    spawn_n_helo_crates,
                    ArgTriple { fst: group, snd: String::from(name), trd: n },
                )?;
            }
            Ok(())
        };
        if let Some(rep) = cfg.repair_crate.get(side) {
            add_supply(rep.name.as_str())?;
        }
        if let Some(whcfg) = &cfg.warehouse {
            if let Some(fuel_cr) = whcfg.supply_transfer_fuel_crate.get(side) {
                add_supply(fuel_cr.name.as_str())?;
            }
            if let Some(weapons_cr) = whcfg.supply_transfer_weapons_crate.get(side) {
                add_supply(weapons_cr.name.as_str())?;
            }
            if !whcfg.carrier_repair_crate.is_empty() {
                add_supply(whcfg.carrier_repair_crate[side].name.as_str())?;
            }
        }
    }

    let base_items = if cfg.warehouse.is_some() || !cfg.repair_crate.is_empty() { 1 } else { 0 };
    let mut tree = CrateTree::new(group, crates_menu.clone(), base_items);
    for dep in cfg.deployables.get(side).unwrap_or(&vec![]) {
        if dep.crates.is_empty() && dep.repair_crate.is_none() {
            continue;
        }

        tree.category(mc, &dep.path, dep.cost)?;
        let leaf = dep.path.last().unwrap().clone();

        // Per-deployable repair crate (revives that deployed group after it's
        // been shot up) -- plain single command, no xN menu.
        if let Some(rc) = &dep.repair_crate {
            let title = if rc.required > 1 {
                String::from(format_compact!("{}({})", rc.name, rc.required))
            } else {
                rc.name.clone()
            };
            mc.add_command_for_group(
                group,
                title,
                Some(tree.page_in(mc, &leaf)?),
                spawn_helo_crate,
                ArgTuple { fst: group, snd: rc.name.clone() },
            )?;
        }

        for cr in &dep.crates {
            let title = if cr.required > 1 {
                String::from(format_compact!("{}({})", cr.name, cr.required))
            } else {
                cr.name.clone()
            };
            mc.add_command_for_group(
                group,
                title,
                Some(tree.page_in(mc, &leaf)?),
                spawn_helo_crate,
                ArgTuple { fst: group, snd: cr.name.clone() },
            )?;
            // Per-crate quantity submenu: pick 1-9 of this crate to spawn at
            // once. The name must include the crate name -- a leaf category
            // with more than one crate (e.g. "SA 3") would otherwise call
            // add_submenu_for_group with the same name+parent once per crate,
            // which collides in DCS and corrupts the menu / binds the quantity
            // commands to the wrong crate.
            let qty_menu = mc.add_submenu_for_group(
                group,
                String::from(format_compact!("{} xN", cr.name)),
                Some(tree.page_in(mc, &leaf)?),
            )?;
            for n in 1..=9u32 {
                mc.add_command_for_group(
                    group,
                    String::from(format_compact!("{n}")),
                    Some(qty_menu.clone()),
                    spawn_n_helo_crates,
                    ArgTriple { fst: group, snd: cr.name.clone(), trd: n },
                )?;
            }
        }
    }

    Ok(())
}
