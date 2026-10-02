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

pub mod action;
pub mod cargo;
pub(crate) mod ewr;
mod ground;
mod info;
pub mod jtac;
pub(crate) mod objectives;
mod recon;
mod troop;
mod wingman;

use crate::{db::Db, Context};
use anyhow::{anyhow, bail, Context as AnyhowContext, Result};
use bfprotocols::cfg::Cfg;
use compact_str::format_compact;
use dcso3::{
    as_tbl,
    coalition::Side,
    env::miz::{GroupId, Miz},
    lua_err,
    mission_commands::{GroupCommandItem, GroupSubMenu, MissionCommands},
    net::SlotId,
    MizLua, String,
};

use log::{debug, error, warn};
use mlua::{prelude::*, Value};
use std::sync::Arc;

/// DCS caps every F10 menu at **10 entries**. Anything past that is silently
/// dropped by the sim -- the eleventh deployable, the eleventh JTAC, the
/// eleventh objective simply is not there, with no error anywhere.
///
/// `Pager` makes a menu grow instead: hand it items and it fills the current
/// page, then spends the last slot on a `More >>` submenu and carries on
/// inside that. Chains as deep as needed, so a list of any length stays
/// reachable.
///
/// ```ignore
/// let mut p = Pager::new(group, root.clone());
/// for obj in objectives {
///     p.command(mc, obj.name.clone(), show_objective, obj.id)?;
/// }
/// ```
pub(crate) struct Pager {
    group: GroupId,
    /// The page items are currently being added to.
    cur: GroupSubMenu,
    /// Entries used on `cur`.
    used: u32,
}

/// Items per DCS menu page. The last slot on a full page becomes `More >>`,
/// so a page carries at most `PAGE_ITEMS - 1` real entries before spilling.
const PAGE_ITEMS: u32 = 10;

/// The 10-entry cap applies to the group's F10 root as well, and the root is
/// deliberately *not* paged: every top-level menu is removed and rebuilt by its
/// own fixed path ("Cargo", "JTAC>>", "Info", ...) from several places, and a
/// `More >>` page would move those paths out from under the rebuild. So the
/// root gets a budget instead. With Wingman, ten can be in use at once (only
/// for an airframe that is recon-capable AND carries cargo and troops, which
/// none does today); an eleventh top-level menu means restructuring, not just
/// adding a line to `init_for_slot`.
const ROOT_MENU_BUDGET: u32 = PAGE_ITEMS;

impl Pager {
    /// Page inside `root`, which is assumed empty.
    pub(crate) fn new(group: GroupId, root: GroupSubMenu) -> Self {
        Pager {
            group,
            cur: root,
            used: 0,
        }
    }

    /// Page inside `root`, which already has `used` entries on it (fixed
    /// commands added before the variable-length list starts).
    pub(crate) fn with_used(group: GroupId, root: GroupSubMenu, used: u32) -> Self {
        Pager {
            group,
            cur: root,
            used,
        }
    }

    /// The menu the next entry should go on, spilling to a new `More >>` page
    /// first if the current one is full. Reserves the slot, so callers that
    /// build their own submenu inside the page still keep the count honest.
    pub(crate) fn page(&mut self, mc: &MissionCommands<'_>) -> Result<GroupSubMenu> {
        if self.used + 1 >= PAGE_ITEMS {
            self.cur = mc
                .add_submenu_for_group(self.group, "More >>".into(), Some(self.cur.clone()))
                .context("adding More >> page")?;
            self.used = 0;
        }
        self.used += 1;
        Ok(self.cur.clone())
    }

    /// Add a command, paging as needed.
    pub(crate) fn command<'lua, F, A>(
        &mut self,
        mc: &MissionCommands<'lua>,
        name: String,
        f: F,
        arg: A,
    ) -> Result<GroupCommandItem>
    where
        F: Fn(MizLua, A) -> Result<()> + 'static,
        A: IntoLua<'lua> + FromLua<'lua>,
    {
        let parent = self.page(mc)?;
        mc.add_command_for_group(self.group, name, Some(parent), f, arg)
    }

    /// Add a submenu, paging as needed. Entries inside it are not this pager's
    /// concern -- give the returned menu its own `Pager` if it can also be long.
    pub(crate) fn submenu(
        &mut self,
        mc: &MissionCommands<'_>,
        name: String,
    ) -> Result<GroupSubMenu> {
        let parent = self.page(mc)?;
        mc.add_submenu_for_group(self.group, name, Some(parent))
    }
}

#[derive(Debug)]
pub struct ArgTuple<T, U> {
    pub fst: T,
    pub snd: U,
}

impl<'lua, T, U> IntoLua<'lua> for ArgTuple<T, U>
where
    T: IntoLua<'lua>,
    U: IntoLua<'lua>,
{
    fn into_lua(self, lua: &'lua Lua) -> LuaResult<LuaValue<'lua>> {
        let tbl = lua.create_table()?;
        tbl.raw_set(1, self.fst)?;
        tbl.raw_set(2, self.snd)?;
        Ok(Value::Table(tbl))
    }
}

impl<'lua, T, U> FromLua<'lua> for ArgTuple<T, U>
where
    T: FromLua<'lua>,
    U: FromLua<'lua>,
{
    fn from_lua(value: LuaValue<'lua>, _lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("ArgTuple", None, value).map_err(lua_err)?;
        Ok(Self {
            fst: tbl.raw_get(1)?,
            snd: tbl.raw_get(2)?,
        })
    }
}

#[derive(Debug)]
pub struct ArgTriple<T, U, V> {
    pub fst: T,
    pub snd: U,
    pub trd: V,
}

impl<'lua, T, U, V> IntoLua<'lua> for ArgTriple<T, U, V>
where
    T: IntoLua<'lua>,
    U: IntoLua<'lua>,
    V: IntoLua<'lua>,
{
    fn into_lua(self, lua: &'lua Lua) -> LuaResult<LuaValue<'lua>> {
        let tbl = lua.create_table()?;
        tbl.raw_set(1, self.fst)?;
        tbl.raw_set(2, self.snd)?;
        tbl.raw_set(3, self.trd)?;
        Ok(Value::Table(tbl))
    }
}

impl<'lua, T, U, V> FromLua<'lua> for ArgTriple<T, U, V>
where
    T: FromLua<'lua>,
    U: FromLua<'lua>,
    V: FromLua<'lua>,
{
    fn from_lua(value: LuaValue<'lua>, _lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("ArgTriple", None, value).map_err(lua_err)?;
        Ok(Self {
            fst: tbl.raw_get(1)?,
            snd: tbl.raw_get(2)?,
            trd: tbl.raw_get(3)?,
        })
    }
}

#[derive(Debug)]
pub struct ArgQuad<T, U, V, W> {
    pub fst: T,
    pub snd: U,
    pub trd: V,
    pub fth: W,
}

impl<'lua, T, U, V, W> IntoLua<'lua> for ArgQuad<T, U, V, W>
where
    T: IntoLua<'lua>,
    U: IntoLua<'lua>,
    V: IntoLua<'lua>,
    W: IntoLua<'lua>,
{
    fn into_lua(self, lua: &'lua Lua) -> LuaResult<LuaValue<'lua>> {
        let tbl = lua.create_table()?;
        tbl.raw_set(1, self.fst)?;
        tbl.raw_set(2, self.snd)?;
        tbl.raw_set(3, self.trd)?;
        tbl.raw_set(4, self.fth)?;
        Ok(Value::Table(tbl))
    }
}

impl<'lua, T, U, V, W> FromLua<'lua> for ArgQuad<T, U, V, W>
where
    T: FromLua<'lua>,
    U: FromLua<'lua>,
    V: FromLua<'lua>,
    W: FromLua<'lua>,
{
    fn from_lua(value: LuaValue<'lua>, _lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("ArgQuad", None, value).map_err(lua_err)?;
        Ok(Self {
            fst: tbl.raw_get(1)?,
            snd: tbl.raw_get(2)?,
            trd: tbl.raw_get(3)?,
            fth: tbl.raw_get(4)?,
        })
    }
}

#[derive(Debug)]
struct ArgPent<T, U, V, W, X> {
    fst: T,
    snd: U,
    trd: V,
    fth: W,
    pnt: X,
}

impl<'lua, T, U, V, W, X> IntoLua<'lua> for ArgPent<T, U, V, W, X>
where
    T: IntoLua<'lua>,
    U: IntoLua<'lua>,
    V: IntoLua<'lua>,
    W: IntoLua<'lua>,
    X: IntoLua<'lua>,
{
    fn into_lua(self, lua: &'lua Lua) -> LuaResult<LuaValue<'lua>> {
        let tbl = lua.create_table()?;
        tbl.raw_set(1, self.fst)?;
        tbl.raw_set(2, self.snd)?;
        tbl.raw_set(3, self.trd)?;
        tbl.raw_set(4, self.fth)?;
        tbl.raw_set(5, self.pnt)?;
        Ok(Value::Table(tbl))
    }
}

impl<'lua, T, U, V, W, X> FromLua<'lua> for ArgPent<T, U, V, W, X>
where
    T: FromLua<'lua>,
    U: FromLua<'lua>,
    V: FromLua<'lua>,
    W: FromLua<'lua>,
    X: FromLua<'lua>,
{
    fn from_lua(value: LuaValue<'lua>, _lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("ArgPnt", None, value).map_err(lua_err)?;
        Ok(Self {
            fst: tbl.raw_get(1)?,
            snd: tbl.raw_get(2)?,
            trd: tbl.raw_get(3)?,
            fth: tbl.raw_get(4)?,
            pnt: tbl.raw_get(5)?,
        })
    }
}

fn slot_for_group(lua: MizLua, ctx: &Context, gid: &GroupId) -> Result<(Side, SlotId)> {
    let miz = Miz::singleton(lua)?;
    // dynamic slot
    if let Some((slot, si)) = ctx.db.ephemeral.get_slot_info_by_miz_gid(gid) {
        return Ok((si.side, slot));
    }
    let group = miz
        .get_group(&ctx.idx, gid)
        .with_context(|| format_compact!("getting group {:?} from miz", gid))?
        .ok_or_else(|| anyhow!("no such group {:?}", gid))?;
    let units = group.group.units().context("getting units")?;
    if units.len() > 1 {
        bail!(
            "groups with more than one member can't spawn crates {:?}",
            gid
        )
    }
    let unit = units.first().context("getting first unit")?;
    Ok((group.side, unit.slot().context("getting unit slot")?))
}

fn player_name(db: &Db, slot: &SlotId) -> String {
    db.ephemeral
        .player_in_slot(&slot)
        .and_then(|ucid| db.player(ucid).map(|p| p.name.clone()))
        .unwrap_or_default()
}

/// The requesting player's current world position (north, east), if they are
/// sitting in an instanced aircraft. Used by the Objectives/Info menus to add
/// bearing/range annotations to their reports.
pub(crate) fn player_world_pos(ctx: &Context, slot: &SlotId) -> Option<dcso3::Vector2> {
    let ucid = ctx.db.ephemeral.player_in_slot(slot)?;
    let player = ctx.db.player(ucid)?;
    let (_, inst) = player.current_slot.as_ref()?;
    let inst = inst.as_ref()?;
    Some(dcso3::Vector2::new(inst.position.p.x, inst.position.p.z))
}

/// True-north bearing (deg) and range (nm) from `from` to `to`.
pub(super) fn brg_rng(from: dcso3::Vector2, to: dcso3::Vector2) -> (u32, f64) {
    let d = to - from;
    let brg = d.y.atan2(d.x).to_degrees().rem_euclid(360.0).round() as u32;
    (brg % 360, d.norm() / 1852.0)
}

#[derive(Debug, Clone, Copy, Default)]
struct CarryCap {
    troops: bool,
    crates: bool,
}

impl CarryCap {
    fn from_typ(cfg: &Cfg, typ: &str) -> CarryCap {
        cfg.cargo
            .get(&*typ)
            .map(|c| CarryCap {
                troops: c.troop_slots > 0 && c.total_slots > 0,
                crates: c.crate_slots > 0 && c.total_slots > 0,
            })
            .unwrap_or_default()
    }
}

pub(super) fn init_for_slot(ctx: &mut Context, lua: MizLua, slot: &SlotId) -> Result<()> {
    debug!("initializing menus for {slot:?}");
    let cfg = Arc::clone(&ctx.db.ephemeral.cfg);
    let mc = MissionCommands::singleton(lua)?;
    match slot {
        SlotId::Spectator => Ok(()),
        SlotId::ArtilleryCommander(_, _)
        | SlotId::ForwardObserver(_, _)
        | SlotId::Instructor(_, _)
        | SlotId::Observer(_, _) => Ok(()),
        SlotId::Unit(_) | SlotId::MultiCrew(_, _) => {
            let ucid = match ctx.db.ephemeral.player_in_slot(slot) {
                Some(ucid) => *ucid,
                None => return Ok(()),
            };
            let si = ctx
                .db
                .ephemeral
                .get_slot_info(slot)
                .context("getting slot info")?;
            // Copy values from si before mutable borrows of ctx
            let miz_gid = si.miz_gid;
            let si_side = si.side;
            let si_typ = si.typ.clone();
            for name in ["GCI/EWR", "Cargo", "C-130 Cargo", "CSAR", "Troops", "Actions", "Recon", "Wingman"] {
                if let Err(e) = mc.remove_submenu_for_group(miz_gid, GroupSubMenu::from(vec![name.into()])) {
                    warn!("slot {slot:?}: could not remove the old {name} menu: {e:?}");
                }
            }
            // Every top-level menu below costs one slot at the group's F10
            // root -- see ROOT_MENU_BUDGET.
            let mut root_menus = 0u32;
            // Each menu is built independently. They used to be chained with
            // `?`, so one failing builder (a bad cargo config, say) silently
            // skipped every menu after it -- Actions, Objectives and Info
            // included. Now a failure is logged, the rest still get built, and
            // the error goes back to `process_init_queue`, which retries the
            // slot on a later tick (every builder removes its own old menu
            // first, so a rebuild is safe).
            let mut failed: Vec<&'static str> = vec![];
            let mut built = |name: &'static str, res: Result<()>| match res {
                Ok(()) => root_menus += 1,
                Err(e) => {
                    error!("slot {slot:?}: could not build the {name} menu: {e:?}");
                    failed.push(name);
                }
            };
            built("GCI/EWR", ewr::add_ewr_menu_for_group(&mc, miz_gid));
            if ctx.db.recon_capable(&si_typ) {
                built("Recon", recon::add_recon_menu_for_group(&mc, miz_gid));
            }
            if crate::airlife::wingman_offered(&cfg) {
                built("Wingman", wingman::add_wingman_menu_for_group(&mc, miz_gid));
            }
            let cap = CarryCap::from_typ(&cfg, si_typ.as_str());

            let is_c130 = ctx.db.ephemeral.cfg.c130_cargo
                .as_ref()
                .map(|c| c.enabled_vehicles.contains(&si_typ))
                .unwrap_or(false);
            let is_helo_dynamic = ctx.db.ephemeral.cfg.helo_cargo
                .as_ref()
                .map(|c| c.enabled_vehicles.contains(&si_typ))
                .unwrap_or(false);

            if is_c130 && ctx.db.ephemeral.cfg.rules.cargo.check(&ucid) {
                built("C-130 Cargo", cargo::add_c130_cargo_menu_for_group(&cfg, &mc, &si_side, miz_gid));
            } else if is_helo_dynamic && ctx.db.ephemeral.cfg.rules.cargo.check(&ucid) {
                built("Cargo", cargo::add_helo_cargo_menu_for_group(&cfg, &mc, &si_side, miz_gid));
            } else if cap.crates && ctx.db.ephemeral.cfg.rules.cargo.check(&ucid) {
                built("Cargo", cargo::add_cargo_menu_for_group(&cfg, &mc, &si_side, miz_gid));
            }
            if ctx.db.ephemeral.cfg.csar.as_ref().map(|c| c.enabled).unwrap_or(false)
                && ctx.db.ephemeral.cfg.rules.cargo.check(&ucid)
            {
                built("CSAR", cargo::add_csar_menu_for_group(&mc, miz_gid));
            }
            if cap.troops && ctx.db.ephemeral.cfg.rules.troops.check(&ucid) {
                built("Troops", troop::add_troops_menu_for_group(&cfg, &mc, &si_side, miz_gid));
            }
            if ctx.db.ephemeral.cfg.rules.jtac.check(&ucid) {
                built("JTAC", jtac::init_jtac_menu_for_slot(ctx, lua, slot));
            }
            if ctx.db.ephemeral.cfg.rules.actions.check(&ucid) {
                built("Actions", action::init_action_menu_for_slot(ctx, lua, slot, &ucid));
            }
            if cfg
                .ground_war
                .as_ref()
                .map_or(false, |g| g.enabled && g.command_rule.check(&ucid))
            {
                built("Ground Forces", ground::init_ground_menu_for_slot(&mc, miz_gid, ucid));
            }
            built("Objectives", objectives::init_objectives_menu_for_slot(ctx, lua, slot));
            built("Info", info::init_info_menu_for_slot(ctx, lua, slot));
            if root_menus > ROOT_MENU_BUDGET {
                error!(
                    "slot {slot:?}: {root_menus} top-level F10 menus but DCS shows only \
                     {ROOT_MENU_BUDGET} -- the last one(s) are being dropped silently"
                );
            }
            if failed.is_empty() {
                Ok(())
            } else {
                bail!("menus failed to build: {}", failed.join(", "))
            }
        }
    }
}

/// Slots whose F10 menus are built per 1 Hz tick. It used to be one, so after
/// a restart (or a mass slot-in at mission start) the last player in a queue
/// of 30 waited half a minute for any menu at all. Building a slot's menus is
/// a few dozen Lua calls, so a handful per tick stays well inside the frame.
const MENU_INITS_PER_TICK: usize = 4;

/// How many times a slot whose menus did not all build is retried, one tick
/// apart, before we give up and leave it with what it has.
const MENU_INIT_RETRIES: u8 = 3;

/// Drain up to `MENU_INITS_PER_TICK` slots from `ctx.menu_init_queue`. A slot
/// whose menus only partly built goes back on the END of the queue (so it
/// can't starve the others) and is retried on a later tick.
pub(super) fn process_init_queue(ctx: &mut Context, lua: MizLua) {
    let mut retry: Vec<SlotId> = vec![];
    for _ in 0..MENU_INITS_PER_TICK {
        let Some(slot) = ctx.menu_init_queue.shift_remove_index(0) else {
            break;
        };
        match init_for_slot(ctx, lua, &slot) {
            Ok(()) => {
                ctx.menu_init_retries.remove(&slot);
            }
            Err(e) => {
                let tries = ctx.menu_init_retries.entry(slot.clone()).or_default();
                *tries = tries.saturating_add(1);
                if *tries <= MENU_INIT_RETRIES {
                    warn!("menus for slot {slot:?} incomplete (attempt {tries}), will retry: {e:?}");
                    retry.push(slot);
                } else {
                    error!("giving up on menus for slot {slot:?} after {tries} attempts: {e:?}");
                    ctx.menu_init_retries.remove(&slot);
                }
            }
        }
    }
    // Re-queued after the loop, so a failing slot is not retried within the
    // same tick. A slot re-queued meanwhile by a fresh birth is already there,
    // and `insert` leaves it where it is.
    for slot in retry {
        ctx.menu_init_queue.insert(slot);
    }
}
