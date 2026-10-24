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

//! The Fat Cow layout: a forward refuelling point laid out around the heavy
//! lift helicopter that brought it, after Don Rudi's CG Fat Cow script. The
//! pad is 100 m aft of the helicopter, the fuel point 35 m aft, the fire
//! point 70 m aft, and a guard detail stands around the helicopter.
//!
//! Everything bflib spawns comes from a mission template, so rather than make
//! every mission carry these, they are written into the in-memory copy of the
//! mission before it is indexed (`inject_templates`). That copy is only ever
//! read by bflib; DCS sees nothing until a Fat Cow is actually set up.
//!
//! Positions here are in the helicopter's frame: x forward, y to the side,
//! heading 0 along x. `place` turns them into the world.

use anyhow::{Context, Result, anyhow, bail};
use bfprotocols::cfg::{Cfg, DeployableKind, FatCowCfg};
use compact_str::format_compact;
use dcso3::{
    LuaEnv, MizLua, String, Vector2,
    coalition::Side,
    env::miz::{GroupKind, Miz, MizIndex},
};
use fxhash::FxHashSet;
use log::info;
use mlua::Table;
use std::f64::consts::PI;

/// Where the pad sits relative to the helicopter: 100 m aft.
pub fn pad_offset() -> Vector2 {
    Vector2::new(-100., 0.)
}

/// `local`, a point in the frame of something at `anchor` facing `heading`,
/// in world coordinates.
pub fn place(anchor: Vector2, heading: f64, local: Vector2) -> Vector2 {
    let (sin, cos) = heading.sin_cos();
    anchor + Vector2::new(local.x * cos - local.y * sin, local.x * sin + local.y * cos)
}

fn side_letter(side: Side) -> Option<&'static str> {
    match side {
        Side::Blue => Some("B"),
        Side::Red => Some("R"),
        Side::Neutral => None,
    }
}

struct StaticPart {
    name: &'static str,
    category: &'static str,
    typ: &'static str,
    shape: Option<&'static str>,
    pos: (f64, f64),
}

/// The statics of the layout. The two fuel points are named as logistics
/// (`<side>DEPFARP...`), so they are the Fat Cow's logi: destroy them and it
/// closes. `{}` is the side letter.
const STATICS: [StaticPart; 4] = [
    StaticPart {
        name: "{}DEPFARPCOWFUEL",
        category: "Fortifications",
        typ: "FARP Fuel Depot",
        shape: None,
        pos: (-35., 30.),
    },
    StaticPart {
        name: "{}DEPFARPCOWAPFC",
        category: "Fortifications",
        typ: "APFC fuel",
        shape: None,
        pos: (-35., 0.),
    },
    StaticPart {
        name: "DEP{}COWEXT",
        category: "Fortifications",
        typ: "FireExtinguisher01",
        shape: Some("M92_FireExtinguisher01"),
        pos: (-70., 0.),
    },
    StaticPart {
        name: "DEP{}COWHOSE",
        category: "Fortifications",
        typ: "M32-10C_01",
        shape: Some("M92_M32-10C_01"),
        pos: (-50., 0.),
    },
];

/// The two crew at the fire point, facing away from the helicopter.
const CREW: [(f64, f64, f64); 2] = [(-70., -2., PI), (-70., 2., PI)];

/// The guard detail around the helicopter; the last one is the MANPAD.
const GUARDS: [(f64, f64); 6] = [
    (-20., -5.),
    (-20., 5.),
    (5., 20.),
    (-5., 20.),
    (20., 10.),
    (20., -10.),
];
const MANPAD: (f64, f64) = (50., 0.);

fn named(pattern: &str, letter: &str) -> String {
    String::from(pattern.replace("{}", letter))
}

fn crew_name(letter: &str) -> String {
    String::from(format_compact!("DEP{letter}COWCREW"))
}

fn guard_name(letter: &str) -> String {
    String::from(format_compact!("DEP{letter}COWGUARD"))
}

/// The templates a `side` Fat Cow spawns besides its pad, in spawn order.
pub fn part_templates(side: Side) -> Result<Vec<String>> {
    let letter = side_letter(side).ok_or_else(|| anyhow!("neutral can't have a fat cow"))?;
    let mut names: Vec<String> = STATICS.iter().map(|s| named(s.name, letter)).collect();
    names.push(crew_name(letter));
    names.push(guard_name(letter));
    Ok(names)
}

fn soldiers(side: Side, cfg: &FatCowCfg) -> (String, String) {
    let (soldier, manpad) = match side {
        Side::Red => ("Paratrooper AKS-74", "SA-18 Igla-S manpad"),
        Side::Blue | Side::Neutral => ("Soldier M4 GRG", "Soldier stinger"),
    };
    (
        cfg.soldier.clone().unwrap_or_else(|| String::from(soldier)),
        cfg.manpad.clone().unwrap_or_else(|| String::from(manpad)),
    )
}

struct Ids {
    gid: i64,
    uid: i64,
}

impl Ids {
    fn group(&mut self) -> i64 {
        self.gid += 1;
        self.gid
    }

    fn unit(&mut self) -> i64 {
        self.uid += 1;
        self.uid
    }
}

/// `country.<category>.group`, created if the mission has none of that
/// category for this country yet.
fn group_list<'lua>(lua: &'lua mlua::Lua, country: &Table<'lua>, category: &str) -> Result<Table<'lua>> {
    let cat: Table = match country.raw_get::<_, Option<Table>>(category)? {
        Some(t) => t,
        None => {
            let t = lua.create_table()?;
            country.raw_set(category, t.clone())?;
            t
        }
    };
    Ok(match cat.raw_get::<_, Option<Table>>("group")? {
        Some(t) => t,
        None => {
            let t = lua.create_table()?;
            cat.raw_set("group", t.clone())?;
            t
        }
    })
}

fn push(list: &Table, t: Table) -> Result<()> {
    Ok(list.raw_set(list.raw_len() + 1, t)?)
}

fn static_group<'lua>(
    lua: &'lua mlua::Lua,
    ids: &mut Ids,
    name: &str,
    category: &str,
    typ: &str,
    shape: Option<&str>,
    pos: (f64, f64),
) -> Result<Table<'lua>> {
    let unit = lua.create_table()?;
    unit.raw_set("name", name)?;
    unit.raw_set("unitId", ids.unit())?;
    unit.raw_set("category", category)?;
    unit.raw_set("type", typ)?;
    if let Some(shape) = shape {
        unit.raw_set("shape_name", shape)?;
    }
    unit.raw_set("x", pos.0)?;
    unit.raw_set("y", pos.1)?;
    unit.raw_set("heading", 0.)?;
    unit.raw_set("rate", 100)?;
    let units = lua.create_table()?;
    units.raw_set(1, unit)?;
    let group = lua.create_table()?;
    group.raw_set("name", name)?;
    group.raw_set("groupId", ids.group())?;
    group.raw_set("x", pos.0)?;
    group.raw_set("y", pos.1)?;
    group.raw_set("heading", 0.)?;
    group.raw_set("dead", false)?;
    group.raw_set("hidden", true)?;
    group.raw_set("units", units)?;
    Ok(group)
}

fn pad_group<'lua>(lua: &'lua mlua::Lua, ids: &mut Ids, name: &str) -> Result<Table<'lua>> {
    let group = static_group(lua, ids, name, "Heliports", "Invisible FARP", Some("invisiblefarp"), (0., 0.))?;
    let unit: Table = group.raw_get::<_, Table>("units")?.raw_get(1)?;
    unit.raw_set("heliport_callsign_id", 1)?;
    unit.raw_set("heliport_frequency", "127.5")?;
    unit.raw_set("heliport_modulation", 0)?;
    Ok(group)
}

fn infantry_group<'lua>(
    lua: &'lua mlua::Lua,
    ids: &mut Ids,
    name: &str,
    members: &[(&str, f64, f64, f64)],
) -> Result<Table<'lua>> {
    let units = lua.create_table()?;
    for (i, (typ, x, y, heading)) in members.iter().enumerate() {
        let unit = lua.create_table()?;
        unit.raw_set("name", format_compact!("{name}-{}", i + 1).as_str())?;
        unit.raw_set("unitId", ids.unit())?;
        unit.raw_set("type", *typ)?;
        unit.raw_set("skill", "Average")?;
        unit.raw_set("x", *x)?;
        unit.raw_set("y", *y)?;
        unit.raw_set("heading", *heading)?;
        unit.raw_set("playerCanDrive", false)?;
        units.raw_set(i + 1, unit)?;
    }
    let (x, y) = (members[0].1, members[0].2);
    let task = lua.create_table()?;
    task.raw_set("id", "ComboTask")?;
    let params = lua.create_table()?;
    params.raw_set("tasks", lua.create_table()?)?;
    task.raw_set("params", params)?;
    let point = lua.create_table()?;
    point.raw_set("x", x)?;
    point.raw_set("y", y)?;
    point.raw_set("alt", 0.)?;
    point.raw_set("type", "Turning Point")?;
    point.raw_set("action", "Off Road")?;
    point.raw_set("speed", 0.)?;
    point.raw_set("task", task)?;
    let points = lua.create_table()?;
    points.raw_set(1, point)?;
    let route = lua.create_table()?;
    route.raw_set("points", points)?;
    let group = lua.create_table()?;
    group.raw_set("name", name)?;
    group.raw_set("groupId", ids.group())?;
    group.raw_set("x", x)?;
    group.raw_set("y", y)?;
    group.raw_set("lateActivation", true)?;
    group.raw_set("hidden", false)?;
    group.raw_set("visible", false)?;
    group.raw_set("task", "Ground Nothing")?;
    group.raw_set("start_time", 0)?;
    group.raw_set("route", route)?;
    group.raw_set("units", units)?;
    Ok(group)
}

/// Write the Fat Cow pads and layout templates for every side whose config
/// has a Fat Cow into the in-memory mission. Pads already placed in the
/// mission editor are kept; the layout names are reserved and must not be.
/// Returns how many groups were added; the mission has to be re-indexed
/// after any.
pub fn inject_templates(lua: MizLua, miz: &Miz, idx: &MizIndex, cfg: &Cfg) -> Result<usize> {
    let lua = lua.inner();
    let mut ids = Ids { gid: idx.max_gid().inner(), uid: idx.max_uid().inner() };
    let mut added = 0;
    for side in [Side::Blue, Side::Red] {
        let Some(letter) = side_letter(side) else { continue };
        let fat_cows: Vec<(&Vec<String>, &FatCowCfg)> = cfg
            .deployables
            .get(&side)
            .into_iter()
            .flatten()
            .filter_map(|d| match &d.kind {
                DeployableKind::Objective(o) => o.fat_cow.as_ref().map(|fc| (&o.pad_templates, fc)),
                DeployableKind::Group { .. } => None,
            })
            .collect();
        let Some((_, first)) = fat_cows.first() else { continue };
        let country = miz
            .coalition(side)?
            .countries()?
            .first()
            .with_context(|| format_compact!("{side:?} has no country in the mission to own its fat cow"))?;
        let exists = |name: &str| -> Result<bool> {
            Ok(miz.get_group_by_name(idx, GroupKind::Any, side, name)?.is_some())
        };
        let statics = group_list(lua, &country, "static")?;
        let vehicles = group_list(lua, &country, "vehicle")?;
        let mut pads: FxHashSet<&str> = FxHashSet::default();
        for (pad_names, _) in &fat_cows {
            for pad in pad_names.iter() {
                if pads.insert(pad.as_str()) && !exists(pad)? {
                    push(&statics, pad_group(lua, &mut ids, pad)?)?;
                    added += 1;
                }
            }
        }
        for name in part_templates(side)? {
            if exists(&name)? {
                bail!(
                    "the mission has a group named {name}, which is reserved for the fat cow layout -- rename it"
                )
            }
        }
        for part in &STATICS {
            let name = named(part.name, letter);
            push(&statics, static_group(lua, &mut ids, &name, part.category, part.typ, part.shape, part.pos)?)?;
        }
        let (soldier, manpad) = soldiers(side, first);
        let crew: Vec<(&str, f64, f64, f64)> =
            CREW.iter().map(|(x, y, h)| (soldier.as_str(), *x, *y, *h)).collect();
        push(&vehicles, infantry_group(lua, &mut ids, &crew_name(letter), &crew)?)?;
        let mut guards: Vec<(&str, f64, f64, f64)> =
            GUARDS.iter().map(|(x, y)| (soldier.as_str(), *x, *y, 0.)).collect();
        guards.push((manpad.as_str(), MANPAD.0, MANPAD.1, 0.));
        push(&vehicles, infantry_group(lua, &mut ids, &guard_name(letter), &guards)?)?;
        added += STATICS.len() + 2;
        info!("[FAT_COW] {side:?}: generated the fat cow layout templates and {} pad(s)", pads.len());
    }
    Ok(added)
}

// The AI Fat Cow: the FatCow action. A helo mission (see `logistics`) flies a
// heavy-lift helicopter from the nearest friendly field to the mark; once it
// is down the Fat Cow is set up around it, and when its time is up it is
// packed up and the helicopter flies home.

use super::{
    Db,
    logistics::{HeloMissionId, HeloMissionKind, HeloMissionState, HeloTo, plan_helo_route},
};
use crate::{objective, spawnctx::SpawnCtx};
use bfprotocols::{
    cfg::{FatCowActionCfg, HeloInsertionCfg},
    db::objective::ObjectiveId,
    stats::Stat,
};
use chrono::{DateTime, Duration, Utc};
use compact_str::CompactString;
use dcso3::{
    LuaVec2, azumith2d,
    controller::{ActionTyp, AltType, MissionPoint, PointType, Task, TurnMethod},
    group::Group,
    net::Ucid,
};
use log::{error, warn};

/// A Fat Cow helo that hasn't got airborne by now never will.
const FAT_COW_STARTUP_LIMIT_SECS: i64 = 15 * 60;

/// What a Fat Cow mission is doing, from its kind.
#[derive(Clone)]
struct OnStation {
    deployable: String,
    pos: Vector2,
    duration_secs: i64,
    farp: Option<ObjectiveId>,
    until: Option<DateTime<Utc>>,
    returning: bool,
}

impl Db {
    /// Start an AI Fat Cow to `pos` (the FatCow action). `cost` is what the
    /// caller paid, refunded if it never gets set up.
    pub fn call_ai_fat_cow(
        &mut self,
        lua: MizLua,
        side: Side,
        ucid: Option<Ucid>,
        action: &str,
        cost: u32,
        cfg: &FatCowActionCfg,
        pos: Vector2,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let helo_cfg = self.ephemeral.cfg.helo_insertion.clone().ok_or_else(|| {
            anyhow!("the AI Fat Cow flies with the helo_insertion settings, which are not configured")
        })?;
        let spec = self
            .ephemeral
            .deployable_idx
            .get(&side)
            .and_then(|idx| idx.deployables_by_name.get(&cfg.deployable))
            .ok_or_else(|| anyhow!("no deployable {} for {side:?}", cfg.deployable))?
            .clone();
        match &spec.kind {
            DeployableKind::Objective(o) if o.fat_cow.is_some() => (),
            _ => bail!("{} is not a fat cow", cfg.deployable),
        }
        if let Some((name, dist)) = self.unpack_too_close(side, pos, true, &[]) {
            bail!(
                "a Fat Cow can't be set up there -- {dist:.0}m from {name}, it has to be {}m from friendly objectives",
                self.ephemeral.cfg.logistics_exclusion
            )
        }
        let (n, _) = self.number_deployed(side, &cfg.deployable)?;
        if n >= spec.limit as usize {
            bail!("{side:?} already has {n} {} up", cfg.deployable)
        }
        let origin = self
            .nearest_helo_launch_field(lua, side, pos, None)
            .ok_or_else(|| anyhow!("no friendly field able to launch a Fat Cow"))?;
        let origin_obj = objective!(self, &origin)?;
        let range = (origin_obj.pos() - pos).norm();
        if range > helo_cfg.max_range_m {
            bail!(
                "the nearest friendly field ({}) is {:.0}km away, past the {:.0}km max range",
                origin_obj.name,
                range / 1000.,
                helo_cfg.max_range_m / 1000.
            )
        }
        let origin_name = origin_obj.name.clone();
        let duration_secs = cfg.duration_mins as i64 * 60;
        let id = self.spawn_helo_mission(
            lua,
            origin,
            origin,
            ucid.unwrap_or_default(),
            cost as i32,
            HeloMissionKind::FatCow {
                action: String::from(action),
                deployable: cfg.deployable.clone(),
                pos,
                duration_secs,
                farp: None,
                until: None,
                returning: false,
            },
            now,
            Some(HeloTo {
                pos,
                name: String::from("the Fat Cow site"),
                // Longer than it stays, so it is never the one to leave.
                stay_secs: (duration_secs + 600) as u32,
                template: cfg.aircraft_template.clone(),
            }),
        )?;
        info!("[FAT_COW] {id} launched from {origin_name} for {} minutes", cfg.duration_mins);
        self.ephemeral.msgs().panel_to_side(
            15,
            false,
            side,
            format_compact!(
                "A Fat Cow is lifting from {origin_name} for the marked site; it will be set up for {} minutes.",
                cfg.duration_mins
            ),
        );
        Ok(())
    }

    /// How many AI Fat Cows `action` has out for `side`, for its `limit`.
    pub fn ai_fat_cows_out(&self, side: Side, action: &str) -> u32 {
        self.ephemeral
            .active_helo_missions
            .values()
            .filter(|m| {
                m.side == side
                    && matches!(&m.kind, HeloMissionKind::FatCow { action: a, .. } if a.as_str() == action)
            })
            .count() as u32
    }

    /// Run the AI Fat Cows, and close the ones whose time ran out while
    /// nothing was there to pack them up (across a restart).
    pub fn tick_fat_cow_missions(&mut self, lua: MizLua, idx: &MizIndex, now: DateTime<Utc>) -> Result<()> {
        if let Some(helo_cfg) = self.ephemeral.cfg.helo_insertion.clone() {
            let ids: Vec<HeloMissionId> = self
                .ephemeral
                .active_helo_missions
                .iter()
                .filter(|(_, m)| matches!(m.kind, HeloMissionKind::FatCow { .. }))
                .map(|(id, _)| id.clone())
                .collect();
            for id in ids {
                if let Err(e) = self.tick_fat_cow(lua, idx, now, &helo_cfg, &id) {
                    error!("[FAT_COW] {id}: {e:?}");
                }
            }
        }
        let expired: Vec<ObjectiveId> = self
            .persisted
            .fat_cow_expiry
            .into_iter()
            .filter(|(_, at)| now >= **at)
            .map(|(oid, _)| *oid)
            .collect();
        for oid in expired {
            info!("[FAT_COW] {oid} reached its time with no helicopter, packing it up");
            self.close_fat_cow(lua, oid);
        }
        Ok(())
    }

    /// Empty a Fat Cow's pad and remove it.
    fn close_fat_cow(&mut self, lua: MizLua, oid: ObjectiveId) {
        let owner = self.persisted.objectives.get(&oid).map(|o| (o.owner, o.name.clone()));
        if let Err(e) = self.close_fat_cow_warehouse(lua, oid) {
            warn!("[FAT_COW] failed to empty {oid}'s pad: {e:?}");
        }
        if let Err(e) = self.delete_objective(&oid) {
            warn!("[FAT_COW] failed to remove {oid}: {e:?}");
        }
        if let Some((side, name)) = owner {
            self.ephemeral.msgs().panel_to_side(
                15,
                false,
                side,
                format_compact!("The Fat Cow at {name} has packed up."),
            );
        }
    }

    fn tick_fat_cow(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        now: DateTime<Utc>,
        helo_cfg: &HeloInsertionCfg,
        id: &HeloMissionId,
    ) -> Result<()> {
        let Some(m) = self.ephemeral.active_helo_missions.get_mut(id) else {
            return Ok(());
        };
        if (now - m.last_check).num_seconds() < 10 {
            return Ok(());
        }
        m.last_check = now;
        let HeloMissionKind::FatCow { deployable, pos, duration_secs, farp, until, returning, .. } =
            m.kind.clone()
        else {
            return Ok(());
        };
        let st = OnStation { deployable, pos, duration_secs, farp, until, returning };
        let (gid, side, player, cost, origin) = (m.group_id, m.side, m.player, m.cost, m.origin);
        let Some(group_name) = self.persisted.groups.get(&gid).map(|g| g.name.clone()) else {
            if let Some(oid) = st.farp {
                self.close_fat_cow(lua, oid);
            }
            self.ephemeral.active_helo_missions.remove(id);
            return Ok(());
        };
        let home = self.persisted.objectives.get(&origin).map(|o| o.pos()).unwrap_or(st.pos);
        let radius = helo_cfg.landing_radius_m;
        let Some(m) = self.ephemeral.active_helo_missions.get_mut(id) else {
            return Ok(());
        };
        let target = if st.returning { home } else { st.pos };
        let state = m.poll(lua, now, &group_name, target, radius);
        let never_launched =
            m.airborne_at.is_none() && (now - m.spawn_time).num_seconds() > FAT_COW_STARTUP_LIMIT_SECS;
        let overdue = m.deadline.is_some_and(|d| now > d);
        match (st.farp, st.returning, state) {
            (farp, returning, HeloMissionState::Destroyed) => {
                info!("[FAT_COW] {id} helicopter lost");
                match farp {
                    // The cow carries the fuel; the FARP goes with it.
                    Some(oid) => self.close_fat_cow(lua, oid),
                    None if !returning => {
                        self.refund_fat_cow(&player, cost, "your Fat Cow was lost before it was set up")
                    }
                    None => (),
                }
                let _ = self.delete_group(&gid);
                self.ephemeral.active_helo_missions.remove(id);
            }
            (None, false, HeloMissionState::Delivered) => {
                match self.set_up_ai_fat_cow(lua, idx, &group_name, side, &player, &st, now) {
                    Ok((oid, until)) => {
                        if let Some(m) = self.ephemeral.active_helo_missions.get_mut(id)
                            && let HeloMissionKind::FatCow { farp, until: u, .. } = &mut m.kind
                        {
                            *farp = Some(oid);
                            *u = Some(until);
                        }
                    }
                    Err(e) => {
                        warn!("[FAT_COW] {id} could not set up: {e:?}");
                        self.refund_fat_cow(&player, cost, &format!("your Fat Cow could not set up: {e}"));
                        self.send_fat_cow_home(lua, id, &group_name, home, helo_cfg, now)?;
                    }
                }
            }
            (None, false, HeloMissionState::InTransit) => {
                if never_launched || overdue {
                    info!("[FAT_COW] {id} recalled, never_launched={never_launched}");
                    self.refund_fat_cow(
                        &player,
                        cost,
                        "your Fat Cow never made it to the site and was recalled",
                    );
                    let _ = self.delete_group(&gid);
                    self.ephemeral.active_helo_missions.remove(id);
                }
            }
            (Some(oid), false, _) => {
                let gone = self.persisted.objectives.get(&oid).is_none();
                if gone || st.until.is_some_and(|u| now >= u) {
                    if !gone {
                        self.close_fat_cow(lua, oid);
                    }
                    self.send_fat_cow_home(lua, id, &group_name, home, helo_cfg, now)?;
                }
            }
            (_, true, HeloMissionState::Delivered) => {
                info!("[FAT_COW] {id} is home");
                let _ = self.delete_group(&gid);
                self.ephemeral.active_helo_missions.remove(id);
            }
            (_, true, HeloMissionState::InTransit) => {
                if overdue {
                    let _ = self.delete_group(&gid);
                    self.ephemeral.active_helo_missions.remove(id);
                }
            }
        }
        Ok(())
    }

    fn refund_fat_cow(&mut self, player: &Ucid, cost: i32, why: &str) {
        if cost > 0 {
            self.ephemeral.panel_to_player(
                &self.persisted,
                15,
                player,
                format_compact!("{why}, refunding {cost} points"),
            );
            self.adjust_points(player, cost, why);
        } else {
            self.ephemeral
                .panel_to_player(&self.persisted, 15, player, CompactString::from(why));
        }
    }

    /// The helicopter is down at the site: lay the Fat Cow out around it.
    fn set_up_ai_fat_cow(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        group_name: &str,
        side: Side,
        player: &Ucid,
        st: &OnStation,
        now: DateTime<Utc>,
    ) -> Result<(ObjectiveId, DateTime<Utc>)> {
        let unit = Group::get_by_name(lua, group_name)?.get_units()?.get(1)?;
        let p = unit.get_position()?;
        let cow = Vector2::new(p.p.0.x, p.p.0.z);
        let heading = azumith2d(Vector2::new(p.x.0.x, p.x.0.z));
        let spec = self
            .ephemeral
            .deployable_idx
            .get(&side)
            .and_then(|idx| idx.deployables_by_name.get(&st.deployable))
            .ok_or_else(|| anyhow!("no deployable {}", st.deployable))?
            .clone();
        let DeployableKind::Objective(parts) = &spec.kind else {
            bail!("{} is not a fat cow", st.deployable)
        };
        let (n, _) = self.number_deployed(side, &st.deployable)?;
        if n >= spec.limit as usize {
            bail!("{side:?} already has {n} {} up", st.deployable)
        }
        let oid = self.add_farp_facing(lua, &SpawnCtx::new(lua)?, idx, side, cow, heading, &spec, parts)?;
        let until = now + Duration::seconds(st.duration_secs);
        self.persisted.fat_cow_expiry.insert_cow(oid, until);
        self.ephemeral.stat(Stat::DeployFarp { by: *player, oid, deployable: st.deployable.clone() });
        let name = objective!(self, &oid)?.name.clone();
        info!("[FAT_COW] set up {name} until {until}");
        self.ephemeral.msgs().panel_to_side(
            20,
            false,
            side,
            format_compact!(
                "The Fat Cow is set up at {name} for {} minutes. Land on the pad 100m behind the helicopter to rearm and refuel.",
                st.duration_secs / 60
            ),
        );
        self.ephemeral.dirty();
        Ok((oid, until))
    }

    /// Fly the helicopter back to its field, where `tick_fat_cow` despawns it.
    fn send_fat_cow_home(
        &mut self,
        lua: MizLua,
        id: &HeloMissionId,
        group_name: &str,
        home: Vector2,
        helo_cfg: &HeloInsertionCfg,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let group = Group::get_by_name(lua, group_name)?;
        let from = {
            let p = group.get_units()?.get(1)?.get_position()?;
            Vector2::new(p.p.0.x, p.p.0.z)
        };
        let land = dcso3::land::Land::singleton(lua)?;
        let plan = plan_helo_route(&land, from, home, helo_cfg, None);
        let speed = helo_cfg.speed_kph / 3.6;
        let point = |typ: PointType, action: TurnMethod, pos: Vector2, alt: f64, stay: Option<i64>| {
            MissionPoint {
                action: Some(ActionTyp::Air(action)),
                airdrome_id: None,
                helipad: None,
                typ,
                link_unit: None,
                pos: LuaVec2(pos),
                alt,
                alt_typ: Some(AltType::BARO),
                time_re_fu_ar: stay,
                eta: None,
                eta_locked: None,
                speed,
                speed_locked: None,
                name: None,
                task: Box::new(Task::ComboTask(vec![])),
            }
        };
        let mut route = vec![point(
            PointType::TurningPoint,
            TurnMethod::FlyOverPoint,
            from,
            plan.departure_alt.max(helo_cfg.altitude_m),
            None,
        )];
        for (pos, alt) in plan.cruise.iter().copied() {
            route.push(point(PointType::TurningPoint, TurnMethod::FlyOverPoint, pos, alt, None));
        }
        route.push(point(
            PointType::TurningPoint,
            TurnMethod::FlyOverPoint,
            plan.approach.0,
            plan.approach.1,
            None,
        ));
        let home_alt = land.get_height(LuaVec2(home)).unwrap_or(0.);
        route.push(point(PointType::Land, TurnMethod::Landing, home, home_alt, Some(600)));
        group.get_controller()?.set_task(Task::Mission { airborne: None, route })?;
        let flight_secs = (home - from).norm() / speed.max(1.) * 1.5;
        if let Some(m) = self.ephemeral.active_helo_missions.get_mut(id) {
            m.state = HeloMissionState::InTransit;
            m.deadline = Some(now + Duration::seconds(flight_secs as i64 + 15 * 60));
            if let HeloMissionKind::FatCow { returning, farp, .. } = &mut m.kind {
                *returning = true;
                *farp = None;
            }
        }
        info!("[FAT_COW] {id} heading home");
        Ok(())
    }
}
