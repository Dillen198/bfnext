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

use super::{Db, ephemeral::DeployableIndex, group::SpawnedGroup, objective::Objective};
use anyhow::Context as _;
use crate::{
    db::group::DeployKind,
    group, maybe, objective,
    spawnctx::{SpawnCtx, SpawnLoc},
    unit, unit_mut,
};
use anyhow::{Result, anyhow, bail};
use bfprotocols::{
    cfg::{CargoConfig, Crate, Deployable, DeployableKind, LimitEnforceTyp, Troop, Vehicle},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    stats::Stat,
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{
    LuaVec2, MizLua, Position3, String, Vector2, azumith2d, azumith2d_to, azumith3d, centroid2d,
    coalition::Side,
    env::miz::MizIndex,
    land::Land,
    net::{SlotId, Ucid},
    object::DcsObject,
    radians_to_degrees,
    trigger::Trigger,
};
use enumflags2::BitFlags;
use fxhash::FxHashMap;
use log::{debug, error, info};
use serde_derive::{Deserialize, Serialize};
use smallvec::{SmallVec, smallvec};
use std::{cmp::max, fmt, sync::Arc};

#[derive(Debug, Clone, Copy)]
pub struct NearbyCrate<'a> {
    pub group: &'a SpawnedGroup,
    pub origin: ObjectiveId,
    pub crate_def: &'a Crate,
    pub pos: Vector2,
    pub heading: f64,
    pub distance: f64,
}

#[derive(Debug, Clone)]
pub enum Unpakistan {
    Unpacked(String),
    UnpackedFarp(String),
    Repaired(String),
    RepairedBase(String, u8),
    TransferedSupplies(String, String),
}

#[derive(Debug, Clone, Copy)]
pub enum Oldest {
    Group(GroupId),
    Objective(ObjectiveId),
}

impl fmt::Display for Unpakistan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unpacked(unit) => write!(f, "unpacked a {unit}"),
            Self::UnpackedFarp(loc) => write!(
                f,
                "unpacked {loc}, units will spawn in 60 seconds get clear"
            ),
            Self::Repaired(unit) => write!(f, "repaired a {unit}"),
            Self::RepairedBase(base, logi) => write!(f, "repaired logistics at {base} to %{logi}"),
            Self::TransferedSupplies(from, to) => {
                write!(f, "transfered supplies from {from} to {to}")
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InternalTroop {
    pub player: Ucid,
    pub origin: Option<ObjectiveId>,
    pub cost_fraction: f32,
    pub troop: Troop,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Cargo {
    pub troops: SmallVec<[InternalTroop; 2]>,
    pub crates: SmallVec<[(ObjectiveId, Crate); 1]>,
}

impl Cargo {
    pub fn num_troops(&self) -> usize {
        self.troops.len()
    }

    pub fn num_crates(&self) -> usize {
        self.crates.len()
    }

    pub fn num_total(&self) -> usize {
        self.num_crates() + self.num_troops()
    }

    pub fn weight(&self) -> i64 {
        let cr = self
            .crates
            .iter()
            .fold(0, |acc, (_, cr)| acc + cr.weight as i64);
        self.troops
            .iter()
            .fold(cr, |acc, it| acc + it.troop.weight as i64)
    }
}

// C-130 Physical Cargo System
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum C130CargoState {
    /// Crate spawned near aircraft, waiting to be loaded
    Spawned,
    /// Crate loaded into aircraft (DCS native cargo)
    Loaded,
    /// Crate in the air (after airdrop)
    Airborne,
    /// Crate landed on ground (ready for auto-unpack)
    Landed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum C130CargoType {
    /// Deployable crate (name maps to deployable)
    Deployable { name: String },
    /// Supply transfer crate
    SupplyTransfer,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct C130Cargo {
    /// DCS group name of the crate object (persists even when DCS changes object ID on load/drop)
    pub name: String,
    /// DCS group ID of the crate object (changes when loaded/dropped, updated via static_born)
    pub group_id: GroupId,
    /// Type of crate
    pub crate_type: C130CargoType,
    /// Current state
    pub state: C130CargoState,
    /// Origin objective where crate was spawned
    pub origin: ObjectiveId,
    /// Player who spawned the crate
    pub player: Ucid,
    /// Side
    pub side: Side,
    /// Last known position
    pub last_pos: Vector2,
    /// Time when crate was spawned
    pub spawn_time: DateTime<Utc>,
    /// Time when crate entered airborne state (for tracking)
    pub airborne_time: Option<DateTime<Utc>>,
    /// The actual crate definition from config
    pub crate_def: Crate,
}

impl C130Cargo {
    pub fn new(
        name: String,
        group_id: GroupId,
        crate_type: C130CargoType,
        origin: ObjectiveId,
        player: Ucid,
        side: Side,
        pos: Vector2,
        crate_def: Crate,
    ) -> Self {
        Self {
            name,
            group_id,
            crate_type,
            state: C130CargoState::Spawned,
            origin,
            player,
            side,
            last_pos: pos,
            spawn_time: Utc::now(),
            airborne_time: None,
            crate_def,
        }
    }

}

#[derive(Debug, Clone)]
pub struct SlotStats {
    pub name: String,
    pub side: Side,
    pub agl: f64,
    pub speed: f64,
    pub in_air: bool,
    pub pos: Position3,
    pub point: Vector2,
    pub ucid: Ucid,
}

impl SlotStats {
    pub fn get(db: &Db, lua: MizLua, slot: &SlotId) -> Result<Self> {
        let ucid = maybe!(db.ephemeral.players_by_slot, *slot, "no such player")?.clone();
        let side = maybe!(db.persisted.players, ucid, "no player for ucid")?.side;
        let unit = db.ephemeral.slot_instance_unit(lua, slot)?;
        let in_air = unit.in_air()?;
        let name = unit.get_name()?;
        let pos = unit.get_position()?;
        let point = Vector2::new(pos.p.x, pos.p.z);
        let ground_alt = Land::singleton(lua)?.get_height(LuaVec2(point))?;
        let agl = pos.p.y - ground_alt;
        let speed = unit.get_velocity()?.0.magnitude() * 3600. / 1000.;
        Ok(Self {
            name,
            side,
            agl,
            speed,
            in_air,
            pos,
            point,
            ucid,
        })
    }
}

impl Db {
    fn point_near_logistics(
        &self,
        side: Side,
        point: Vector2,
    ) -> Result<(ObjectiveId, &Objective)> {
        let obj = self
            .persisted
            .objectives
            .into_iter()
            .find_map(|(oid, obj)| {
                if obj.owner == side && obj.logi() > 0 && obj.zone.contains(point) {
                    return Some((oid, obj));
                }
                None
            });
        match obj {
            Some((oid, obj)) => Ok((*oid, obj)),
            None => bail!("not near friendly logistics"),
        }
    }

    pub fn spawn_crate(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        slot: &SlotId,
        name: &str,
    ) -> Result<SlotStats> {
        debug!("db spawning crate");
        let st = SlotStats::get(self, lua, slot)?;
        if st.in_air {
            bail!("you must land to spawn crates")
        }
        let dir = Vector2::new(st.pos.x.x, st.pos.x.z);
        let approx_spawn_pos = st.point + dir * 20.;
        if !self
            .list_crates_near_point(approx_spawn_pos, 10.)?
            .is_empty()
        {
            bail!("move away from other crates or pick up the existing crate")
        }
        let to_delete = self.ephemeral.cfg.max_crates.and_then(|max_crates| {
            let crates = &self.persisted.players[&st.ucid].crates;
            if crates.len() < max_crates as usize {
                None
            } else {
                crates.into_iter().next().map(|id| *id)
            }
        });
        let (oid, _) = self.point_near_logistics(st.side, st.point)?;
        let dep_idx = self
            .ephemeral
            .deployable_idx
            .get(&st.side)
            .ok_or_else(|| anyhow!("{} doesn't have any deployables", st.side))?;
        let crate_cfg = dep_idx
            .crates_by_name
            .get(name)
            .ok_or_else(|| anyhow!("no such crate {name}"))?
            .clone();
        if let Some((dep, player)) = dep_idx
            .deployables_by_crates
            .get(&crate_cfg.name)
            .and_then(|n| dep_idx.deployables_by_name.get(n))
            .and_then(|d| self.persisted.players.get(&st.ucid).map(|p| (d, p)))
        {
            if player.points < dep.cost as i32 {
                if let Some(si) = self.ephemeral.slot_info.get(slot) {
                    let gid = si.miz_gid;
                    let msg = format_compact!(
                        "WARNING: you have {} points, and this deployable costs {} points",
                        player.points,
                        dep.cost
                    );
                    self.ephemeral.msgs().panel_to_group(10, false, gid, msg);
                }
            }
        }
        let template = self
            .ephemeral
            .cfg
            .crate_template
            .get(&st.side)
            .ok_or_else(|| anyhow!("missing crate template for {:?} side", st.side))?
            .clone();
        let spawnpos = SpawnLoc::AtPos {
            pos: st.point,
            offset_direction: dir,
            group_heading: azumith2d(dir),
        };
        let dk = DeployKind::Crate {
            origin: oid,
            player: st.ucid.clone(),
            spec: crate_cfg.clone(),
        };
        if let Some(gid) = to_delete {
            self.delete_group(&gid)?;
        }
        self.add_and_queue_group(
            &SpawnCtx::new(lua)?,
            idx,
            st.side,
            spawnpos,
            &template,
            dk,
            BitFlags::empty(),
            None,
        )?;
        Ok(st)
    }

    fn list_crates_near_point<'a>(
        &'a self,
        point: Vector2,
        max_dist: f64,
    ) -> Result<SmallVec<[NearbyCrate<'a>; 4]>> {
        let mut res: SmallVec<[NearbyCrate; 4]> = smallvec![];
        for gid in &self.persisted.crates {
            let group = group!(self, gid)?;
            let (oid, crate_def) = match &group.origin {
                DeployKind::Crate {
                    origin: oid,
                    spec: crt,
                    ..
                } => (oid, crt),
                DeployKind::Deployed { .. }
                | DeployKind::Troop { .. }
                | DeployKind::Objective { .. }
                | DeployKind::ObjectiveDeprecated
                | DeployKind::Action { .. } => {
                    bail!("group {:?} is listed in crates but isn't a crate", gid)
                }
            };
            for uid in &group.units {
                let unit = &unit!(self, uid)?;
                let distance = na::distance(&point.into(), &unit.pos.into());
                if distance <= max_dist {
                    let heading = radians_to_degrees(azumith2d_to(point, unit.pos));
                    res.push(NearbyCrate {
                        group,
                        origin: *oid,
                        crate_def,
                        pos: unit.pos,
                        heading,
                        distance,
                    })
                }
            }
        }
        res.sort_by_key(|nc| (nc.distance * 1000.) as u32);
        Ok(res)
    }

    pub fn list_nearby_crates<'a>(
        &'a self,
        st: &SlotStats,
    ) -> Result<SmallVec<[NearbyCrate<'a>; 4]>> {
        let max_dist = self.ephemeral.cfg.crate_load_distance as f64;
        self.list_crates_near_point(st.point, max_dist)
    }

    pub fn destroy_nearby_crate(&mut self, lua: MizLua, slot: &SlotId) -> Result<()> {
        let st = SlotStats::get(self, lua, slot)?;
        if st.in_air {
            bail!("you must land to destroy crates")
        }
        let nearby = self.list_nearby_crates(&st)?;
        let closest = nearby
            .into_iter()
            .next()
            .ok_or_else(|| anyhow!("no nearby crates"))?;
        let gid = closest.group.id;
        self.delete_group(&gid)
    }

    pub fn list_cargo(&self, slot: &SlotId) -> Option<&Cargo> {
        self.ephemeral.cargo.get(slot)
    }

    #[allow(dead_code)]
    pub fn is_player_deployed(&self, gid: &GroupId) -> bool {
        self.persisted.deployed.contains(gid)
    }

    pub fn cargo_capacity(&self, vehicle: &Vehicle) -> Result<CargoConfig> {
        let cargo_capacity = self
            .ephemeral
            .cfg
            .cargo
            .get(vehicle)
            .ok_or_else(|| anyhow!("{:?} can't carry cargo", vehicle))
            .map(|c| *c)?;
        Ok(cargo_capacity)
    }

    pub fn number_deployed(&self, side: Side, name: &str) -> Result<(usize, Option<Oldest>)> {
        let mut n = 0;
        let mut oldest = None;
        for gid in &self.persisted.deployed {
            let group = &group!(self, gid)?;
            if let DeployKind::Deployed { spec: d, .. } = &group.origin {
                if let Some(d_name) = d.path.last() {
                    if group.side == side && d_name.as_str() == name {
                        if oldest.is_none() {
                            oldest = Some(Oldest::Group(*gid));
                        }
                        n += 1;
                    }
                }
            }
        }
        for oid in &self.persisted.farps {
            let obj = objective!(self, oid)?;
            if let ObjectiveKind::Farp {
                spec,
                pad_template: _,
                mobile: _,
            } = &obj.kind
            {
                if let Some(d_name) = spec.path.last() {
                    if obj.owner == side && d_name.as_str() == name {
                        if oldest.is_none() {
                            oldest = Some(Oldest::Objective(*oid));
                        }
                        n += 1;
                    }
                }
            }
        }
        Ok((n, oldest))
    }

    pub fn deployable_by_crate<'a>(
        &'a self,
        side: &Side,
        name: &str,
    ) -> Option<(&'a String, &'a Deployable)> {
        self.ephemeral.deployable_idx.get(side).and_then(|idx| {
            idx.deployables_by_crates
                .get(name)
                .and_then(|name| idx.deployables_by_name.get(name).map(|dep| (name, dep)))
        })
    }

    pub fn number_troops_deployed(
        &self,
        side: Side,
        name: &str,
    ) -> Result<(usize, Option<GroupId>)> {
        let mut n = 0;
        let mut oldest = None;
        for gid in &self.persisted.troops {
            let group = group!(self, gid)?;
            if let DeployKind::Troop { spec: tr, .. } = &group.origin {
                if group.side == side && name == tr.name.as_str() {
                    if oldest.is_none() {
                        oldest = Some(*gid);
                    }
                    n += 1;
                }
            }
        }
        Ok((n, oldest))
    }

    pub fn number_crates_deployed(&self, st: &SlotStats) -> Result<(usize, Option<GroupId>)> {
        let player = maybe!(self.persisted.players, &st.ucid, "no such player")?;
        let n = player.crates.len();
        let oldest = player.crates.into_iter().next().map(|id| *id);
        Ok((n, oldest))
    }

    pub fn unpakistan(&mut self, lua: MizLua, idx: &MizIndex, slot: &SlotId) -> Result<Unpakistan> {
        #[derive(Clone)]
        struct Cifo {
            pos: Vector2,
            group: GroupId,
            origin: ObjectiveId,
            crate_def: Crate,
        }
        impl<'a> From<NearbyCrate<'a>> for Cifo {
            fn from(nc: NearbyCrate<'a>) -> Self {
                Self {
                    pos: nc.pos,
                    group: nc.group.id,
                    origin: nc.origin,
                    crate_def: nc.crate_def.clone(),
                }
            }
        }
        fn nearby(db: &Db, st: &SlotStats) -> Result<SmallVec<[Cifo; 8]>> {
            let nearby_player = db
                .list_nearby_crates(st)?
                .into_iter()
                .map(Cifo::from)
                .collect::<SmallVec<[Cifo; 8]>>();
            if nearby_player.is_empty() {
                Ok(nearby_player)
            } else {
                let sp = db.ephemeral.cfg.crate_spread as f64;
                let mut crates = FxHashMap::default();
                for cr in &nearby_player {
                    for cr in db
                        .list_crates_near_point(cr.pos, sp)?
                        .into_iter()
                        .map(Cifo::from)
                    {
                        crates.entry(cr.group).or_insert(cr);
                    }
                }
                Ok(crates.into_iter().map(|(_, cr)| cr).collect())
            }
        }
        fn buildable(
            nearby: &SmallVec<[Cifo; 8]>,
            didx: &DeployableIndex,
        ) -> std::result::Result<
            FxHashMap<String, FxHashMap<String, Vec<Cifo>>>,
            SmallVec<[CompactString; 2]>,
        > {
            let mut candidates: FxHashMap<String, FxHashMap<String, Vec<Cifo>>> =
                FxHashMap::default();
            let mut reasons = smallvec![];
            for cr in nearby {
                if let Some(dep) = didx.deployables_by_crates.get(&cr.crate_def.name) {
                    candidates
                        .entry(dep.clone())
                        .or_default()
                        .entry(cr.crate_def.name.clone())
                        .or_default()
                        .push(cr.clone());
                }
            }
            candidates.retain(|dep, have| {
                let spec = &didx.deployables_by_name[dep];
                for req in &spec.crates {
                    match have.get_mut(&req.name) {
                        Some(ids) if ids.len() >= req.required as usize => {
                            while ids.len() > req.required as usize {
                                ids.pop();
                            }
                        }
                        Some(_) | None => {
                            reasons
                                .push(format_compact!("can't spawn {dep} missing {}\n", req.name));
                            return false;
                        }
                    }
                }
                true
            });
            if candidates.is_empty() {
                Err(reasons)
            } else {
                Ok(candidates)
            }
        }
        fn base_repairable(
            db: &Db,
            side: Side,
            nearby: &SmallVec<[Cifo; 8]>,
        ) -> FxHashMap<GroupId, Cifo> {
            let cr = &db.ephemeral.cfg.repair_crate[&side];
            nearby
                .iter()
                .filter(|ci| ci.crate_def.name == cr.name)
                .map(|ci| (ci.group, ci.clone()))
                .collect()
        }
        fn supply_transferrable(
            db: &Db,
            side: Side,
            nearby: &SmallVec<[Cifo; 8]>,
        ) -> SmallVec<[(GroupId, Cifo); 2]> {
            if let Some(whcfg) = db.ephemeral.cfg.warehouse.as_ref() {
                let cr = &whcfg.supply_transfer_crate[&side];
                nearby
                    .iter()
                    .filter(|ci| ci.crate_def.name == cr.name)
                    .map(|ci| (ci.group, ci.clone()))
                    .collect()
            } else {
                smallvec![]
            }
        }
        fn repairable(
            db: &Db,
            nearby: &SmallVec<[Cifo; 8]>,
            didx: &DeployableIndex,
            max_dist: f64,
        ) -> std::result::Result<
            FxHashMap<String, (GroupId, Vec<Cifo>)>,
            SmallVec<[CompactString; 2]>,
        > {
            let mut repairs: FxHashMap<String, (GroupId, Vec<Cifo>)> = FxHashMap::default();
            let mut reasons = smallvec![];
            let max_dist = max_dist.powi(2);
            for cr in nearby {
                if let Some(dep) = didx.deployables_by_repair.get(&cr.crate_def.name) {
                    let mut group_to_repair = None;
                    for gid in &db.persisted.deployed {
                        let group = &db.persisted.groups[gid];
                        match &group.origin {
                            DeployKind::Deployed { spec: d, .. } if d.path.last() == Some(&dep) => {
                                for uid in &group.units {
                                    let unit_pos = db.persisted.units[uid].pos;
                                    if na::distance_squared(&unit_pos.into(), &cr.pos.into())
                                        <= max_dist
                                    {
                                        group_to_repair = Some(*gid);
                                        break;
                                    }
                                }
                                reasons.push(format_compact!("not close enough to repair {dep}"));
                            }
                            DeployKind::Deployed { .. }
                            | DeployKind::Crate { .. }
                            | DeployKind::Objective { .. }
                            | DeployKind::ObjectiveDeprecated
                            | DeployKind::Troop { .. }
                            | DeployKind::Action { .. } => (),
                        }
                    }
                    if let Some(gid) = group_to_repair {
                        let (_, crates) =
                            repairs.entry(dep.clone()).or_insert_with(|| (gid, vec![]));
                        crates.push(cr.clone())
                    }
                }
            }
            repairs.retain(|dep, (_, have)| {
                let required = have[0].crate_def.required as usize;
                if have.len() < required {
                    reasons.push(format_compact!("not enough crates to repair {dep}\n"));
                    false
                } else {
                    while have.len() > required {
                        have.pop();
                    }
                    true
                }
            });
            if repairs.is_empty() {
                Err(reasons)
            } else {
                Ok(repairs)
            }
        }
        fn too_close<'a, I: Iterator<Item = &'a Cifo>, F: Fn() -> I>(
            db: &Db,
            side: Side,
            centroid: Vector2,
            logistics: bool,
            iter: F,
        ) -> bool {
            let excl_dist_sq = (db.ephemeral.cfg.logistics_exclusion as f64).powi(2);
            db.persisted.objectives.into_iter().any(|(oid, obj)| {
                let mut check = false;
                for cr in iter() {
                    check |= oid == &cr.origin;
                }
                check |= logistics || obj.owner == side;
                check && (logistics || obj.threatened) && {
                    let dist = na::distance_squared(&obj.zone.pos().into(), &centroid.into());
                    dist <= excl_dist_sq || obj.zone.scale(1.1).contains(centroid.into())
                }
            })
        }
        fn close_enough_to_repair<'a, I: Iterator<Item = &'a Cifo>, F: Fn() -> I>(
            db: &Db,
            side: Side,
            centroid: Vector2,
            iter: F,
        ) -> Option<ObjectiveId> {
            db.persisted.objectives.into_iter().find_map(|(oid, obj)| {
                let mut is_origin = false;
                for cr in iter() {
                    is_origin |= oid == &cr.origin;
                }
                if obj.owner == side && !is_origin && obj.zone.contains(centroid) {
                    Some(*oid)
                } else {
                    None
                }
            })
        }
        fn compute_positions(
            db: &mut Db,
            have: &FxHashMap<String, Vec<Cifo>>,
            centroid: Vector2,
            group_heading: f64,
        ) -> Result<SpawnLoc> {
            let mut num_by_typ: FxHashMap<String, usize> = FxHashMap::default();
            let mut pos_by_typ: FxHashMap<String, Vector2> = FxHashMap::default();
            for cr in have.iter().flat_map(|(_, cr)| cr.iter()) {
                let group = &group!(db, cr.group)?;
                if let DeployKind::Crate { spec, .. } = &group.origin {
                    if let Some(typ) = spec.pos_unit.as_ref() {
                        let uid = group
                            .units
                            .into_iter()
                            .next()
                            .ok_or_else(|| anyhow!("{:?} has no units", cr.group))?;
                        *pos_by_typ.entry(typ.clone()).or_default() += unit!(db, uid)?.pos;
                        *num_by_typ.entry(typ.clone()).or_default() += 1;
                    }
                }
            }
            for (typ, pos) in pos_by_typ.iter_mut() {
                if let Some(n) = num_by_typ.get(typ) {
                    *pos /= *n as f64
                }
            }
            let spawnloc = if pos_by_typ.is_empty() {
                SpawnLoc::AtPos {
                    pos: centroid,
                    offset_direction: Vector2::default(),
                    group_heading,
                }
            } else {
                SpawnLoc::AtPosWithComponents {
                    pos: centroid,
                    group_heading,
                    component_pos: pos_by_typ,
                }
            };
            Ok(spawnloc)
        }
        fn enforce_deploy_limits(
            db: &mut Db,
            side: Side,
            spec: &Deployable,
            dep: &String,
            origin: ObjectiveId,
            ucid: &Ucid,
        ) -> Result<ObjectiveId> {
            if let Some(player) = db.persisted.players.get(ucid)
                && let Some(obj) = db.persisted.objectives.get(&origin)
            {
                let player_points = max(0, player.points);
                if spec.cost as i32 > player_points + obj.points {
                    bail!(
                        "there are {} available points, this deployable costs {} points to unpack",
                        player_points,
                        spec.cost
                    )
                }
            }
            let (n, oldest) = db.number_deployed(side, &**dep)?;
            if n >= spec.limit as usize {
                match spec.limit_enforce {
                    LimitEnforceTyp::DenyCrate => {
                        bail!("the max number of {:?} are already deployed", dep)
                    }
                    LimitEnforceTyp::DeleteOldest => match oldest {
                        Some(Oldest::Group(gid)) => db.delete_group(&gid)?,
                        Some(Oldest::Objective(oid)) => db.delete_objective(&oid)?,
                        None => (),
                    },
                }
            }
            Ok(origin)
        }
        let st = SlotStats::get(self, lua, slot)?;
        if st.in_air {
            bail!("you must land to unpack crates")
        }
        let max_dist = self.ephemeral.cfg.crate_spread as f64;
        let nearby = nearby(self, &st)?;
        let didx = Arc::clone(
            self.ephemeral
                .deployable_idx
                .get(&st.side)
                .ok_or_else(|| anyhow!("{:?} can't deploy anything", st.side))?,
        );
        if nearby.is_empty() {
            bail!("no nearby crates")
        }
        let mut reasons: SmallVec<[CompactString; 2]> = smallvec![];
        let base_repairs = base_repairable(self, st.side, &nearby);
        let supply_transfer = supply_transferrable(self, st.side, &nearby);
        if !base_repairs.is_empty() {
            let centroid = centroid2d(base_repairs.iter().map(|(_, c)| c.pos));
            let oid = close_enough_to_repair(self, st.side, centroid, || {
                base_repairs.iter().map(|(_, c)| c)
            });
            if let Some(oid) = oid {
                let obj = objective!(self, oid)?;
                if obj.logi == 100 {
                    reasons.push("objective logistics are completely repaired".into());
                } else {
                    self.repair_one_logi_step(st.side, Utc::now(), oid)?;
                    self.delete_group(base_repairs.keys().next().unwrap())?;
                    self.ephemeral.stat(Stat::Repair {
                        id: oid,
                        by: st.ucid,
                    });
                    if let Some(amount) = self
                        .ephemeral
                        .cfg
                        .points
                        .as_ref()
                        .map(|p| p.logistics_repair)
                    {
                        self.adjust_points(&st.ucid, amount as i32, "for logistics repair");
                    }
                    let obj = objective!(self, oid)?;
                    return Ok(Unpakistan::RepairedBase(obj.name.clone(), obj.logi()));
                }
            } else {
                reasons.push("not close enough to a friendly objective".into());
            }
        }
        if !supply_transfer.is_empty() {
            let centroid = centroid2d(supply_transfer.iter().map(|(_, c)| c.pos));
            let oid = close_enough_to_repair(self, st.side, centroid, || {
                supply_transfer.iter().map(|(_, c)| c)
            });
            if let Some(to) = oid {
                let (gid, _) = supply_transfer.into_iter().next().unwrap();
                if let DeployKind::Crate {
                    origin: from,
                    player: _,
                    spec: _,
                } = self.persisted.groups[&gid].origin
                {
                    self.transfer_supplies(lua, from, to)?;
                    self.delete_group(&gid)?;
                    self.ephemeral.stat(Stat::SupplyTransfer {
                        from,
                        to,
                        by: st.ucid,
                    });
                    if let Some(amount) = self
                        .ephemeral
                        .cfg
                        .points
                        .as_ref()
                        .map(|p| p.logistics_transfer)
                    {
                        self.adjust_points(&st.ucid, amount as i32, "for supply transfer");
                    }
                    return Ok(Unpakistan::TransferedSupplies(
                        objective!(self, from)?.name.clone(),
                        objective!(self, to)?.name.clone(),
                    ));
                }
            } else {
                reasons.push("not close enough to a friendly objective".into());
            }
        }
        match buildable(&nearby, &didx) {
            Err(mut build_reasons) => reasons.append(&mut build_reasons),
            Ok(mut candidates) => {
                let (dep, have) = candidates.drain().next().unwrap();
                let spec = maybe!(didx.deployables_by_name, dep, "deployable")?.clone();
                let centroid = centroid2d(have.values().flat_map(|c| c.iter()).map(|c| c.pos));
                let too_close =
                    too_close(self, st.side, centroid, spec.kind.is_objective(), || {
                        have.values().flat_map(|c| c.iter())
                    });
                if too_close {
                    if spec.kind.is_group() {
                        reasons.push("can't unpack that here while enemies are close".into());
                    } else {
                        reasons.push("can't unpack that here".into())
                    }
                } else {
                    let spctx = SpawnCtx::new(lua)?;
                    let origins = {
                        let mut oids = have
                            .values()
                            .flat_map(|crs| crs.iter())
                            .map(|cr| cr.origin)
                            .collect::<SmallVec<[_; 8]>>();
                        oids.sort();
                        oids.dedup();
                        oids
                    };
                    let can_deploy = origins.iter().fold(Err(anyhow!("")), |res, oid| match res {
                        Ok(oid) => Ok(oid),
                        Err(_) => enforce_deploy_limits(self, st.side, &spec, &dep, *oid, &st.ucid),
                    });
                    match can_deploy {
                        Err(e) => reasons.push(format_compact!("{e}")),
                        Ok(from_obj) => match &spec.kind {
                            DeployableKind::Objective(parts) => {
                                for cr in have.values().flat_map(|c| c.iter()) {
                                    self.delete_group(&cr.group)?
                                }
                                let oid = self
                                    .add_farp(lua, &spctx, idx, st.side, centroid, &spec, parts)?;
                                self.ephemeral.stat(Stat::DeployFarp {
                                    oid,
                                    by: st.ucid,
                                    deployable: dep,
                                });
                                self.charge_for_item(
                                    &st.ucid,
                                    from_obj,
                                    spec.cost,
                                    "for farp spawn",
                                );
                                let name = objective!(self, oid)?.name.clone();
                                return Ok(Unpakistan::UnpackedFarp(name));
                            }
                            DeployableKind::Group { template } => {
                                let pos = self.ephemeral.slot_instance_pos(lua, slot)?;
                                let spawnloc =
                                    compute_positions(self, &have, centroid, azumith3d(pos.x.0))?;
                                let origin = DeployKind::Deployed {
                                    player: st.ucid.clone(),
                                    moved_by: None,
                                    spec: spec.clone(),
                                    cost_fraction: 1.,
                                    origin: Some(from_obj),
                                };
                                let gid = self.add_and_queue_group(
                                    &spctx,
                                    idx,
                                    st.side,
                                    spawnloc,
                                    template,
                                    origin,
                                    BitFlags::empty(),
                                    None,
                                )?;
                                for cr in have.values().flat_map(|c| c.iter()) {
                                    self.delete_group(&cr.group)?
                                }
                                self.ephemeral.stat(Stat::DeployGroup {
                                    gid,
                                    by: st.ucid,
                                    deployable: dep.clone(),
                                });
                                let frac = self.charge_for_item(
                                    &st.ucid,
                                    from_obj,
                                    spec.cost,
                                    &format_compact!("for {dep} unpack"),
                                );
                                if let DeployKind::Deployed { cost_fraction, .. } =
                                    &mut self.persisted.groups[&gid].origin
                                {
                                    *cost_fraction = frac;
                                }
                                return Ok(Unpakistan::Unpacked(dep));
                            }
                        },
                    }
                }
            }
        }
        match repairable(self, &nearby, &didx, max_dist) {
            Err(mut rep_reasons) => reasons.append(&mut rep_reasons),
            Ok(mut candidates) => {
                let (dep, (gid, have)) = candidates.drain().next().unwrap();
                let spec = maybe!(didx.deployables_by_name, dep, "deployable")?.clone();
                let player = maybe!(self.persisted.players, &st.ucid, "player")?;
                let centroid = centroid2d(have.iter().map(|c| c.pos));
                if spec.repair_cost > 0 && spec.repair_cost as i32 > player.points {
                    reasons.push(format_compact!(
                        "Repairing {dep} costs {}, you have {}",
                        spec.repair_cost,
                        player.points
                    ));
                } else if too_close(self, st.side, centroid, false, || have.iter()) {
                    reasons.push("can't repair that here while enemies are close".into())
                } else {
                    let group = group!(self, gid)?;
                    for uid in &group.units {
                        let unit = unit_mut!(self, uid)?;
                        unit.dead = false;
                    }
                    for cr in &have {
                        self.delete_group(&cr.group)?
                    }
                    self.ephemeral.push_spawn(gid);
                    if spec.repair_cost > 0 {
                        self.adjust_points(
                            &st.ucid,
                            -(spec.repair_cost as i32),
                            &format_compact!("for {dep} repair"),
                        );
                    }
                    self.ephemeral.dirty();
                    return Ok(Unpakistan::Repaired(dep));
                }
            }
        }
        bail!(
            reasons
                .into_iter()
                .fold(CompactString::new(""), |mut acc, r| {
                    if acc.is_empty() {
                        acc.push_str(r.as_str());
                    } else {
                        acc.push('\n');
                        acc.push_str(r.as_str());
                    }
                    acc
                })
        )
    }

    pub fn unload_crate(&mut self, lua: MizLua, idx: &MizIndex, slot: &SlotId) -> Result<Crate> {
        let st = SlotStats::get(self, lua, slot)?;
        let cargo = self.ephemeral.cargo.get(slot);
        if cargo.map(|c| c.crates.is_empty()).unwrap_or(true) {
            bail!("no crates onboard")
        }
        let cargo = self.ephemeral.cargo.get_mut(slot).unwrap();
        let (oid, crate_cfg) = cargo.crates.pop().unwrap();
        let weight = cargo.weight();
        if st.in_air && st.speed > crate_cfg.max_drop_speed as f64 {
            let max_sp = (crate_cfg.max_drop_speed * 3600) / 1000;
            let max_al = crate_cfg.max_drop_height_agl;
            cargo.crates.push((oid, crate_cfg));
            bail!(
                "you are going too fast to unload your cargo, speed must be at or below {} km/h, and altitude agl must be at or below {} m",
                max_sp,
                max_al
            )
        }
        if st.in_air && st.agl > crate_cfg.max_drop_height_agl as f64 {
            let max_sp = (crate_cfg.max_drop_speed * 3600) / 1000;
            let max_al = crate_cfg.max_drop_height_agl;
            cargo.crates.push((oid, crate_cfg));
            bail!(
                "you are too high to unload your cargo, altitude agl must be at or below {} m, and speed must be at or below {} km/h",
                max_al,
                max_sp
            )
        }
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(st.name, weight)?;
        let template = self
            .ephemeral
            .cfg
            .crate_template
            .get(&st.side)
            .ok_or_else(|| anyhow!("missing crate template for {:?}", st.side))?
            .clone();
        let spawnpos = SpawnLoc::AtPos {
            pos: st.point,
            offset_direction: Vector2::new(st.pos.x.x, st.pos.x.z),
            group_heading: azumith3d(st.pos.x.0),
        };
        let dk = DeployKind::Crate {
            origin: oid,
            player: st.ucid,
            spec: crate_cfg.clone(),
        };
        let spctx = SpawnCtx::new(lua)?;
        if let Err(e) = self.add_and_queue_group(
            &spctx,
            idx,
            st.side,
            spawnpos,
            &template,
            dk,
            BitFlags::empty(),
            None,
        ) {
            self.ephemeral
                .cargo
                .get_mut(slot)
                .unwrap()
                .crates
                .push((oid, crate_cfg));
            return Err(e);
        }
        Ok(crate_cfg)
    }

    pub fn unit_cargo_cfg(&self, slot: &SlotId) -> Result<(CargoConfig, Side, String)> {
        let si = self
            .ephemeral
            .get_slot_info(slot)
            .ok_or_else(|| anyhow!("no such slot"))?;
        let side = si.side;
        let unit_name = si.unit_name.clone();
        let cargo_capacity = self.cargo_capacity(&si.typ)?;
        Ok((cargo_capacity, side, unit_name))
    }

    pub fn load_nearby_crate(&mut self, lua: MizLua, slot: &SlotId) -> Result<Crate> {
        let st = SlotStats::get(self, lua, slot)?;
        let (cargo_capacity, side, unit_name) = self.unit_cargo_cfg(slot)?;
        let cargo = self.ephemeral.cargo.entry(slot.clone()).or_default();
        if cargo_capacity.crate_slots as usize <= cargo.num_crates()
            || cargo_capacity.total_slots as usize <= cargo.num_total()
        {
            bail!("you already have a full load onboard")
        }
        let (gid, oid, crate_def) = {
            let mut nearby = self.list_nearby_crates(&st)?;
            nearby.retain(|nc| nc.group.side == side);
            if nearby.is_empty() {
                bail!(
                    "no friendly crates within {} meters",
                    self.ephemeral.cfg.crate_load_distance
                );
            }
            let the_crate = nearby.first().unwrap();
            let gid = the_crate.group.id;
            let crate_def = the_crate.crate_def.clone();
            let oid = the_crate.origin;
            (gid, oid, crate_def)
        };
        let cargo = self.ephemeral.cargo.get_mut(slot).unwrap();
        cargo.crates.push((oid, crate_def.clone()));
        let weight = cargo.weight();
        self.delete_group(&gid)?;
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(unit_name, weight as i64)?;
        Ok(crate_def)
    }

    pub fn load_troops(
        &mut self,
        lua: MizLua,
        slot: &SlotId,
        name: &str,
    ) -> Result<(Troop, ObjectiveId)> {
        let (cargo_capacity, side, unit_name) = self.unit_cargo_cfg(slot)?;
        let pos = self.ephemeral.slot_instance_pos(lua, slot)?;
        let point = Vector2::new(pos.p.x, pos.p.z);
        let (origin, _) = self.point_near_logistics(side, point)?;
        let troop_cfg = self
            .ephemeral
            .deployable_idx
            .get(&side)
            .and_then(|idx| idx.squads_by_name.get(name))
            .ok_or_else(|| anyhow!("no such squad {name}"))?
            .clone();
        let ucid = self
            .ephemeral
            .player_in_slot(slot)
            .cloned()
            .ok_or_else(|| anyhow!("can't find player in slot {slot:?}"))?;
        if self.ephemeral.cfg.points.is_some() {
            if let Some(player) = self.persisted.players.get(&ucid)
                && let Some(obj) = self.persisted.objectives.get(&origin)
            {
                let points = max(0, player.points) + obj.points;
                if troop_cfg.cost > 0 && points < troop_cfg.cost as i32 {
                    bail!(
                        "there are {} points available, this troop costs {} points",
                        points,
                        troop_cfg.cost
                    )
                }
            }
        }
        let cargo = self.ephemeral.cargo.entry(slot.clone()).or_default();
        if cargo_capacity.troop_slots as usize <= cargo.num_troops()
            || cargo_capacity.total_slots as usize <= cargo.num_total()
        {
            bail!("you already have a full load onboard")
        }
        let cost_fraction = self.charge_for_item(
            &ucid,
            origin,
            troop_cfg.cost,
            &format_compact!("for {name} troop"),
        );
        let cargo = self.ephemeral.cargo.entry(slot.clone()).or_default();
        cargo.troops.push(InternalTroop {
            player: ucid,
            origin: Some(origin),
            cost_fraction,
            troop: troop_cfg.clone(),
        });
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(unit_name, cargo.weight() as i64)?;
        Ok((troop_cfg, origin))
    }

    pub fn unload_troops(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        slot: &SlotId,
    ) -> Result<(Troop, GroupId, Option<ObjectiveId>)> {
        let cargo = self.ephemeral.cargo.get(slot);
        if cargo.map(|c| c.troops.is_empty()).unwrap_or(true) {
            bail!("no troops onboard")
        }
        let unit = self.ephemeral.slot_instance_unit(lua, slot)?;
        if unit.in_air()? {
            bail!("you must land to unload troops")
        }
        let unit_name = unit.get_name()?;
        let side = self
            .ephemeral
            .get_slot_info(slot)
            .ok_or_else(|| anyhow!("no slot info for {slot:?}"))?
            .side;
        let pos = unit.get_position()?;
        let oid = Db::objective_near_point(
            &self.persisted.objectives,
            Vector2::new(pos.p.0.x, pos.p.0.z),
            |_| true,
        )
        .map(|(_, _, o)| o.id);
        let point = Vector2::new(pos.p.x, pos.p.z);
        match self.point_near_logistics(side, point) {
            Ok((_, obj)) if obj.threatened => {
                bail!("you can't deploy troops here while enemies are near")
            }
            Ok(_) | Err(_) => (),
        }
        let cargo = self.ephemeral.cargo.get(slot).unwrap();
        let it = cargo.troops.last().unwrap();
        let (n, oldest) = self.number_troops_deployed(side, it.troop.name.as_str())?;
        let to_delete = if n < it.troop.limit as usize {
            None
        } else {
            match it.troop.limit_enforce {
                LimitEnforceTyp::DeleteOldest => oldest,
                LimitEnforceTyp::DenyCrate => {
                    bail!(
                        "the maximum number of {} troops are already deployed",
                        it.troop.name
                    )
                }
            }
        };
        let cargo = self.ephemeral.cargo.get_mut(slot).unwrap();
        let it = cargo.troops.pop().unwrap();
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(unit_name, cargo.weight())?;
        let spawnpos = SpawnLoc::AtPos {
            pos: point,
            offset_direction: Vector2::new(pos.x.x, pos.x.z),
            group_heading: azumith3d(pos.x.0),
        };
        let dk = DeployKind::Troop {
            player: it.player,
            moved_by: None,
            spec: it.troop.clone(),
            origin: it.origin,
            cost_fraction: it.cost_fraction,
        };
        let spctx = SpawnCtx::new(lua)?;
        if let Some(gid) = to_delete {
            self.delete_group(&gid)?
        }
        match self.add_and_queue_group(
            &spctx,
            idx,
            side,
            spawnpos,
            &*it.troop.template,
            dk,
            BitFlags::empty(),
            None,
        ) {
            Ok(gid) => {
                self.ephemeral.stat(Stat::DeployTroop {
                    gid,
                    troop: it.troop.name.clone(),
                    by: it.player,
                });
                Ok((it.troop, gid, oid))
            }
            Err(e) => {
                self.ephemeral.cargo.get_mut(slot).unwrap().troops.push(it);
                Err(e)
            }
        }
    }

    pub fn return_troops(&mut self, lua: MizLua, slot: &SlotId) -> Result<Troop> {
        let cargo = self.ephemeral.cargo.get(slot);
        if cargo.map(|c| c.troops.is_empty()).unwrap_or(true) {
            bail!("no troops onboard")
        }
        let unit = self.ephemeral.slot_instance_unit(lua, slot)?;
        if unit.in_air()? {
            bail!("you must land to return your troops")
        }
        let unit_name = unit.get_name()?;
        let side = self
            .ephemeral
            .get_slot_info(slot)
            .ok_or_else(|| anyhow!("no slot info for {slot:?}"))?
            .side;
        let pos = unit.get_position()?;
        let point = Vector2::new(pos.p.x, pos.p.z);
        if self.point_near_logistics(side, point).is_err() {
            bail!("you are not close enough to friendly logistics to return troops")
        }
        let cargo = self.ephemeral.cargo.get_mut(slot).unwrap();
        let it = cargo.troops.pop().unwrap();
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(unit_name, cargo.weight())?;
        match it.origin {
            None => self.adjust_points(&it.player, it.troop.cost as i32, "for troop return"),
            Some(oid) => {
                self.refund_points(
                    &it.player,
                    oid,
                    it.troop.cost,
                    it.cost_fraction,
                    "for troop return",
                );
            }
        }
        Ok(it.troop)
    }

    pub fn extract_troops(&mut self, lua: MizLua, slot: &SlotId) -> Result<Troop> {
        let (cargo_capacity, side, unit_name) = self.unit_cargo_cfg(slot)?;
        let pos = self.ephemeral.slot_instance_pos(lua, slot)?;
        let point = Vector2::new(pos.p.x, pos.p.z);
        let (gid, it) = {
            let max_dist = (self.ephemeral.cfg.crate_load_distance as f64).powi(2);
            self.persisted
                .troops
                .into_iter()
                .filter_map(|gid| self.persisted.groups.get(gid).map(|g| (*gid, g)))
                .find_map(|(gid, g)| {
                    if let DeployKind::Troop {
                        spec,
                        player,
                        origin,
                        moved_by: _,
                        cost_fraction,
                    } = &g.origin
                    {
                        if g.side == side {
                            let in_range = g
                                .units
                                .into_iter()
                                .filter_map(|uid| self.persisted.units.get(uid))
                                .any(|u| {
                                    na::distance_squared(&u.pos.into(), &point.into()) <= max_dist
                                });
                            if in_range {
                                return Some((
                                    gid,
                                    InternalTroop {
                                        player: *player,
                                        origin: *origin,
                                        cost_fraction: *cost_fraction,
                                        troop: spec.clone(),
                                    },
                                ));
                            }
                        }
                    }
                    None
                })
                .ok_or_else(|| anyhow!("no troops in range"))?
        };
        let cargo = self.ephemeral.cargo.entry(slot.clone()).or_default();
        if cargo_capacity.troop_slots as usize <= cargo.num_troops()
            || cargo_capacity.total_slots as usize <= cargo.num_total()
        {
            bail!("you already have a full load onboard")
        }
        let troop_cfg = it.troop.clone();
        cargo.troops.push(it);
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(unit_name, cargo.weight() as i64)?;
        self.delete_group(&gid)?;
        Ok(troop_cfg)
    }

    // ===== C-130 Physical Cargo System =====

    /// Helper method to get deployable index for a side
    #[allow(private_interfaces)]
    pub(crate) fn deployable_idx(&self, side: Side) -> Result<&Arc<super::ephemeral::DeployableIndex>> {
        self.ephemeral
            .deployable_idx
            .get(&side)
            .ok_or_else(|| anyhow!("{:?} doesn't have any deployables", side))
    }

    /// Get the current objective ID for a player in a slot
    pub fn player_current_objective_id(&self, slot: &SlotId) -> Result<ObjectiveId> {
        let si = self
            .ephemeral
            .get_slot_info(slot)
            .ok_or_else(|| anyhow!("no such slot"))?;
        Ok(si.objective)
    }

    /// Spawn a single physical crate near the player's aircraft
    pub fn spawn_c130_crate(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        slot: &SlotId,
        crate_name: String,
        side: Side,
        origin: ObjectiveId,
    ) -> Result<String> {
        debug!("[C130_CARGO] spawn_c130_crate called: crate={}, side={:?}, origin={:?}, slot={:?}",
            crate_name, side, origin, slot);

        let ucid = maybe!(self.ephemeral.players_by_slot, *slot, "no such player")?.clone();
        let dep_idx = self.deployable_idx(side)?;

        // Get crate definition (either from deployable or supply transfer)
        let (crate_def, crate_type) = if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
            debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
            (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
        } else if let Some(whcfg) = &self.ephemeral.cfg.warehouse {
            let supply_crate = &whcfg.supply_transfer_crate[&side];
            if supply_crate.name == crate_name {
                debug!("[C130_CARGO] Found supply transfer crate: {}, weight={}kg", crate_name, supply_crate.weight);
                (supply_crate.clone(), C130CargoType::SupplyTransfer)
            } else {
                error!("[C130_CARGO] Crate not found: {}", crate_name);
                bail!("crate {} not found", crate_name)
            }
        } else {
            error!("[C130_CARGO] Warehouse not configured, crate {} not found", crate_name);
            bail!("crate {} not found", crate_name)
        };

        // Get player position and direction (using same method as regular cargo system)
        let unit = self.ephemeral.slot_instance_unit(lua, slot)?;
        let pos = unit.get_position()?;
        let point = Vector2::new(pos.p.x, pos.p.z);

        // Get direction vector from position matrix (same as regular cargo system)
        let dir = Vector2::new(pos.x.x, pos.x.z);
        debug!("[C130_CARGO] Player position: x={:.2}, z={:.2}, dir=({:.2}, {:.2})",
               point.x, point.y, dir.x, dir.y);

        // Spawn physical crate using spawn system (use C-130 specific template)
        let template = self
            .ephemeral
            .cfg
            .c130_cargo_template
            .get(&side)
            .ok_or_else(|| anyhow!("missing c130_cargo_template for {:?}", side))?
            .clone();

        // Use same spawn location approach as regular cargo system
        let spawnpos = SpawnLoc::AtPos {
            pos: point,
            offset_direction: dir,  // Same as regular cargo system
            group_heading: azumith2d(dir),  // Same as regular cargo system
        };

        let dk = DeployKind::Crate {
            origin,
            player: ucid.clone(),
            spec: crate_def.clone(),
        };

        debug!("[C130_CARGO] Spawning with template='{}', dir=({:.2}, {:.2})",
               template, dir.x, dir.y);

        let group_id = self.add_and_queue_group(
            &SpawnCtx::new(lua)?,
            idx,
            side,
            spawnpos,
            &template,
            dk,
            BitFlags::empty(),
            None,
        )?;

        debug!("[C130_CARGO] Crate spawned successfully: group_id={:?}", group_id);

        // Get the group name for tracking (persists across DCS cargo load/drop)
        let group_name = match self.persisted.groups.get(&group_id) {
            Some(g) => g.name.clone(),
            None => {
                error!("[C130_CARGO] Failed to get group name for {:?}", group_id);
                bail!("Failed to get group name for spawned crate")
            }
        };

        // Create C130Cargo tracking entry
        let c130_cargo = C130Cargo::new(
            group_name.clone(),
            group_id,
            crate_type,
            origin,
            ucid,
            side,
            point,
            crate_def,
        );

        self.ephemeral.c130_crates.insert(group_name.clone(), c130_cargo);
        debug!("[C130_CARGO] Crate tracking added: name='{}', group_id={:?}, total_tracked={}",
            group_name, group_id, self.ephemeral.c130_crates.len());

        Ok(String::from(format!("Spawned {} crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it.", crate_name)))
    }

    /// Queue multiple crates for staggered spawning (used by "Spawn All" command)
    pub fn queue_c130_crate_spawns(
        &mut self,
        slot: &SlotId,
        crate_list: Vec<(String, Crate)>,
        side: Side,
        origin: ObjectiveId,
    ) -> Result<String> {
        let ucid = maybe!(self.ephemeral.players_by_slot, *slot, "no such player")?.clone();

        let spawn_delay = self.ephemeral.cfg.c130_cargo
            .as_ref()
            .map(|c| c.spawn_delay)
            .unwrap_or(1);

        let max_spawn = self.ephemeral.cfg.c130_cargo
            .as_ref()
            .map(|c| c.max_spawn_all as usize)
            .unwrap_or(50);

        let num_to_spawn = crate_list.len().min(max_spawn);
        let mut spawn_time = Utc::now();

        for (idx, (crate_name, crate_def)) in crate_list.into_iter().take(num_to_spawn).enumerate() {
            spawn_time = spawn_time + chrono::Duration::seconds(spawn_delay as i64);

            self.ephemeral
                .c130_spawn_queue
                .entry(spawn_time)
                .or_insert_with(Vec::new)
                .push((side, crate_name, origin, ucid.clone(), crate_def, idx));
        }

        Ok(String::from(format!(
            "Queued {} crates for spawning ({} second intervals). They will appear behind your aircraft.",
            num_to_spawn, spawn_delay
        )))
    }

    /// Process the spawn queue (called from slow_timed_events)
    pub fn process_c130_spawn_queue(&mut self, lua: MizLua, idx: &MizIndex, slot: &SlotId) -> Result<()> {
        let now = Utc::now();
        let to_spawn: Vec<_> = self.ephemeral
            .c130_spawn_queue
            .range(..=now)
            .map(|(k, _)| *k)
            .collect();

        for spawn_time in to_spawn {
            if let Some(crates) = self.ephemeral.c130_spawn_queue.remove(&spawn_time) {
                for (side, crate_name, origin, ucid, crate_def, _crate_idx) in crates {
                    // Get player's current position and direction (might have moved)
                    let unit_result = self.ephemeral.slot_instance_unit(lua, slot);
                    let (point, dir) = match unit_result {
                        Ok(unit) => {
                            let pos = unit.get_position()?;
                            let pt = Vector2::new(pos.p.x, pos.p.z);
                            let direction = Vector2::new(pos.x.x, pos.x.z);
                            (pt, direction)
                        }
                        Err(_) => continue, // Player might have disconnected
                    };

                    // Spawn the crate (use C-130 specific template)
                    let template = match self.ephemeral.cfg.c130_cargo_template.get(&side).cloned() {
                        Some(name) => name,
                        None => continue,
                    };

                    let crate_type = if crate_name.contains("Supply Transfer") {
                        C130CargoType::SupplyTransfer
                    } else {
                        C130CargoType::Deployable { name: crate_name.clone() }
                    };

                    // Use same spawn location approach as regular cargo system
                    // Note: crate_idx is kept for future use but not currently used for positioning
                    let spawnpos = SpawnLoc::AtPos {
                        pos: point,
                        offset_direction: dir,  // Same as regular cargo system
                        group_heading: azumith2d(dir),  // Same as regular cargo system
                    };

                    let dk = DeployKind::Crate {
                        origin,
                        player: ucid,
                        spec: crate_def.clone(),
                    };

                    match self.add_and_queue_group(
                        &SpawnCtx::new(lua)?,
                        idx,
                        side,
                        spawnpos,
                        &template,
                        dk,
                        BitFlags::empty(),
                        None,
                    ) {
                        Ok(group_id) => {
                            // Get the group name for tracking (persists across DCS cargo load/drop)
                            let group_name = match self.persisted.groups.get(&group_id) {
                                Some(g) => g.name.clone(),
                                None => {
                                    error!("[C130_CARGO] Failed to get group name for {:?}", group_id);
                                    continue;
                                }
                            };

                            let physical_crate = C130Cargo::new(
                                group_name.clone(),
                                group_id,
                                crate_type,
                                origin,
                                ucid,
                                side,
                                point,
                                crate_def,
                            );

                            debug!("[C130_CARGO] Spawned crate: name='{}', group_id={:?}", group_name, group_id);
                            self.ephemeral.c130_crates.insert(group_name, physical_crate);
                        }
                        Err(_) => continue,
                    }
                }
            }
        }

        Ok(())
    }

    /// Track physical crate state changes and implement auto-unpack
    pub fn update_c130_crates(&mut self, lua: MizLua, idx: &MizIndex) -> Result<()> {
        let mut to_unpack = Vec::new();
        let mut groups_to_mark = Vec::new();

        debug!("[C130_CARGO] update_c130_crates: tracking {} crates", self.ephemeral.c130_crates.len());
        debug!("[C130_CARGO] update_c130_crates: object_id_by_gid has {} entries", self.ephemeral.object_id_by_gid.len());

        // Update positions and detect landed crates
        for (crate_name, crate_data) in self.ephemeral.c130_crates.iter_mut() {
            debug!("[C130_CARGO] update_c130_crates: checking crate '{}' with group_id {:?}", crate_name, crate_data.group_id);
            let group_oid = match self.ephemeral.object_id_by_gid.get(&crate_data.group_id) {
                Some(oid) => oid.clone(),
                None => {
                    debug!("[C130_CARGO] crate '{}' group_id {:?} has no object_id in map (map has {} total entries), skipping",
                        crate_name, crate_data.group_id, self.ephemeral.object_id_by_gid.len());
                    continue;
                }
            };

            // Get the object directly (crates are static objects, stored as Object oids)
            let obj = match dcso3::object::Object::get_instance(lua, &group_oid) {
                Ok(o) => o,
                Err(e) => {
                    debug!("[C130_CARGO] Failed to get object instance for crate '{}' ({:?}): {:?}", crate_name, crate_data.group_id, e);
                    continue;
                }
            };

            let pos = obj.get_position()?;
            let in_air = obj.in_air()?;
            let velocity = obj.get_velocity()?;
            let speed = (velocity.0.x.powi(2) + velocity.0.y.powi(2) + velocity.0.z.powi(2)).sqrt();

            debug!("[C130_CARGO] Crate '{}' state={:?}, in_air={}, speed={:.2}m/s, pos=({:.2}, {:.2})",
                crate_name, crate_data.state, in_air, speed, pos.p.x, pos.p.z);

            let new_pos = Vector2::new(pos.p.x, pos.p.z);

            // Update marker if crate has moved significantly (> 10 meters)
            if na::distance(&new_pos.into(), &crate_data.last_pos.into()) > 10.0 {
                crate_data.last_pos = new_pos;

                // Update the unit position in the database so mark_group can use the correct position
                if let Some(group) = self.persisted.groups.get(&crate_data.group_id) {
                    for uid in &group.units {
                        if let Some(unit) = self.persisted.units.get_mut_cow(uid) {
                            unit.pos = new_pos;
                            unit.position = pos;
                            unit.heading = azumith3d(pos.x.0);
                        }
                    }
                }

                groups_to_mark.push(crate_data.group_id);
            }

            // State machine for crate tracking
            match crate_data.state {
                C130CargoState::Spawned | C130CargoState::Loaded => {
                    // If crate is moving fast, it's been airdropped
                    // Note: Static objects report in_air=false even when falling, so check speed only
                    if speed > 5.0 {
                        info!("[C130_CARGO] Crate '{}' transitioned to Airborne (speed={:.2}m/s)", crate_name, speed);
                        crate_data.state = C130CargoState::Airborne;
                        crate_data.airborne_time = Some(Utc::now());
                    }
                }
                C130CargoState::Airborne => {
                    // If crate has landed (low speed)
                    if speed < 1.0 {
                        info!("[C130_CARGO] Crate '{}' transitioned to Landed (speed={:.2}m/s) - queuing for auto-unpack", crate_name, speed);
                        crate_data.state = C130CargoState::Landed;
                        to_unpack.push(crate_name.clone());
                    }
                }
                C130CargoState::Landed => {
                    // Already landed, will be unpacked
                }
            }
        }

        debug!("[C130_CARGO] update_c130_crates: {} crates to mark, {} crates to unpack", groups_to_mark.len(), to_unpack.len());

        // Update markers for moved crates
        for group_id in groups_to_mark {
            if let Err(e) = self.mark_group(&group_id) {
                error!("Failed to update marker for C-130 crate group {:?}: {:?}", group_id, e);
            }
        }

        // Auto-unpack landed crates
        for crate_name in to_unpack {
            if let Some(crate_data) = self.ephemeral.c130_crates.get(&crate_name).cloned() {
                info!("[C130_CARGO] Auto-unpacking landed crate: '{}'", crate_name);
                let result = self.unpack_c130_crate(lua, idx, &crate_data, &crate_name);
                match result {
                    Ok(msg) => {
                        info!("[C130_CARGO] Auto-unpacked physical crate '{}': {}", crate_name, msg);
                        self.ephemeral.msgs().panel_to_side(10, false, crate_data.side, msg);
                    }
                    Err(e) => {
                        error!("[C130_CARGO] Failed to auto-unpack physical crate '{}': {}", crate_name, e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Unpack a physical crate (spawns deployable if enough crates are present)
    fn unpack_c130_crate(&mut self, lua: MizLua, idx: &MizIndex, crate_data: &C130Cargo, crate_name: &str) -> Result<String> {
        match &crate_data.crate_type {
            C130CargoType::Deployable { name } => {
                // Check if this crate was already removed (processed by another crate in the same batch)
                if !self.ephemeral.c130_crates.contains_key(crate_name) {
                    return Ok(String::from("Crate already processed"));
                }

                // Get deployable spec
                let dep_idx = self.deployable_idx(crate_data.side)?;

                // Two-step lookup: crate name -> deployable name -> deployable spec
                let deployable_name = dep_idx.deployables_by_crates.get(name)
                    .ok_or_else(|| anyhow!("Deployable not found for crate: {}", name))?
                    .clone();
                let deployable = dep_idx.deployables_by_name.get(&deployable_name)
                    .ok_or_else(|| anyhow!("Deployable spec not found: {}", deployable_name))?
                    .clone();

                // Find all landed crates nearby (within 100m) for this deployable
                let mut nearby_crates: FxHashMap<String, Vec<String>> = FxHashMap::default();
                let crate_pos = crate_data.last_pos;

                info!("[C130_CARGO] Searching for nearby crates for deployable '{}' from crate '{}' at pos ({:.2}, {:.2})",
                    deployable_name, crate_name, crate_pos.x, crate_pos.y);

                for (other_name, other_data) in &self.ephemeral.c130_crates {
                    // Only consider landed crates on the same side
                    if other_data.state == C130CargoState::Landed && other_data.side == crate_data.side {
                        // Check if it's for the same deployable
                        if let C130CargoType::Deployable { name: other_crate_name } = &other_data.crate_type {
                            if let Some(other_dep_name) = dep_idx.deployables_by_crates.get(other_crate_name) {
                                if other_dep_name == &deployable_name {
                                    // Check distance
                                    let dist = na::distance(&crate_pos.into(), &other_data.last_pos.into());
                                    info!("[C130_CARGO]   - Found potential crate '{}' for same deployable, distance={:.2}m",
                                        other_name, dist);
                                    if dist < 100.0 {
                                        nearby_crates
                                            .entry(other_crate_name.clone())
                                            .or_default()
                                            .push(other_name.clone());
                                    }
                                } else {
                                    info!("[C130_CARGO]   - Skipping crate '{}' (different deployable: '{}')",
                                        other_name, other_dep_name);
                                }
                            }
                        }
                    }
                }

                info!("[C130_CARGO] Found {} nearby crate types for '{}'", nearby_crates.len(), deployable_name);

                // Check if we have enough of each required crate type
                let mut have_all_required = true;
                let mut missing_crates = Vec::new();

                for req in &deployable.crates {
                    let count = nearby_crates.get(&req.name).map(|v| v.len()).unwrap_or(0);
                    if count < req.required as usize {
                        have_all_required = false;
                        missing_crates.push(format!("{} (need {}, have {})", req.name, req.required, count));
                    }
                }

                if !have_all_required {
                    info!("[C130_CARGO] Not enough crates for {}: missing {}", deployable_name, missing_crates.join(", "));
                    return Ok(String::from(format!("Crate landed, need more crates for {}", deployable_name)));
                }

                // We have enough! Spawn the deployable
                let spawnpos = SpawnLoc::AtPos {
                    pos: crate_pos,
                    offset_direction: Vector2::new(0., 0.),
                    group_heading: 0.,
                };

                // Only support Group deployables for C-130 airdrops
                match &deployable.kind {
                    DeployableKind::Group { template } => {
                        let template = template.clone();
                        let dk = DeployKind::Deployed {
                            player: crate_data.player,
                            moved_by: None,
                            spec: deployable.clone(),
                            cost_fraction: 1.0,
                            origin: Some(crate_data.origin),
                        };

                        match self.add_and_queue_group(
                            &SpawnCtx::new(lua)?,
                            idx,
                            crate_data.side,
                            spawnpos,
                            &template,
                            dk,
                            BitFlags::empty(),
                            None,
                        ) {
                            Ok(_) => {
                                // Delete all the crates used for this deployable
                                let mut crates_to_delete = Vec::new();
                                for req in &deployable.crates {
                                    if let Some(crate_names) = nearby_crates.get(&req.name) {
                                        for (i, crate_name) in crate_names.iter().enumerate() {
                                            if i < req.required as usize {
                                                crates_to_delete.push(crate_name.clone());
                                            }
                                        }
                                    }
                                }

                                for crate_name in &crates_to_delete {
                                    if let Some(crate_to_delete) = self.ephemeral.c130_crates.remove(crate_name) {
                                        if let Err(e) = self.delete_group(&crate_to_delete.group_id) {
                                            error!("[C130_CARGO] Failed to delete crate '{}': {:?}", crate_name, e);
                                        }
                                    }
                                }

                                info!("[C130_CARGO] Deployed {} using {} crates", deployable_name, crates_to_delete.len());
                                Ok(String::from(format!("Airdropped {} deployed", deployable_name)))
                            }
                            Err(e) => {
                                error!("[C130_CARGO] Failed to spawn group '{}' from crate: {:?}", template, e);
                                Err(anyhow!("Failed to spawn deployable: {:?}", e))
                            }
                        }
                    }
                    DeployableKind::Objective(_) => {
                        Err(anyhow!("C-130 airdrops don't support objective deployables"))
                    }
                }
            }
            C130CargoType::SupplyTransfer => {
                // Handle supply transfer - find nearest objective and add supplies directly
                let objectives: Vec<(ObjectiveId, Vector2)> = self.persisted
                    .objectives
                    .into_iter()
                    .filter_map(|(oid, obj)| {
                        if obj.owner == crate_data.side {
                            Some((*oid, obj.zone.pos()))
                        } else {
                            None
                        }
                    })
                    .collect();

                let nearest_oid = objectives
                    .iter()
                    .min_by_key(|(_, pos)| {
                        let dist = na::distance(&(*pos).into(), &crate_data.last_pos.into());
                        (dist * 1000.0) as i64
                    })
                    .map(|(oid, _)| *oid);

                if let Some(oid) = nearest_oid {
                    // Get warehouse config and extract needed data before mutable borrows
                    let (transfer_amount, exempt_airframes) = {
                        let whcfg = self.ephemeral.cfg.warehouse.as_ref()
                            .ok_or_else(|| anyhow!("Warehouse not configured"))?;
                        (whcfg.supply_transfer_size, whcfg.exempt_airframes.clone())
                    };

                    // Sync warehouse to get current state
                    let (obj_mut, wh) = self.sync_warehouse_to_objective(lua, oid)
                        .context("syncing warehouse for supply transfer")?;

                    // Add supplies directly to warehouse (transfer_amount% of capacity for each item)
                    let mut added_items = Vec::new();

                    // Add equipment (non-exempt items)
                    for (name, inv) in obj_mut.warehouse.equipment.iter_mut_cow() {
                        if exempt_airframes.contains(name.as_str()) {
                            continue; // Skip airframes
                        }

                        if inv.capacity > inv.stored {
                            let available_space = inv.capacity - inv.stored;
                            let amount = ((inv.capacity as f32 * (transfer_amount as f32 / 100.0)) as u32).max(1);
                            let to_add = amount.min(available_space);

                            if to_add > 0 {
                                inv.stored += to_add;
                                added_items.push(format!("{}: +{}", name, to_add));
                                info!("[SUPPLY_TRANSFER] Added {} x{} to {:?}", name, to_add, oid);
                            }
                        }
                    }

                    // Add liquids (fuel)
                    for (liq_type, inv) in obj_mut.warehouse.liquids.iter_mut_cow() {
                        if inv.capacity > inv.stored {
                            let available_space = inv.capacity - inv.stored;
                            let amount = ((inv.capacity as f32 * (transfer_amount as f32 / 100.0)) as u32).max(1);
                            let to_add = amount.min(available_space);

                            if to_add > 0 {
                                inv.stored += to_add;
                                added_items.push(format!("{:?}: +{}", liq_type, to_add));
                                info!("[SUPPLY_TRANSFER] Added {:?} x{} to {:?}", liq_type, to_add, oid);
                            }
                        }
                    }

                    // Sync back to DCS warehouse
                    use crate::db::logistics::sync_obj_to_warehouse;
                    sync_obj_to_warehouse(&obj_mut, &wh)?;

                    // Delete the physical crate
                    self.delete_group(&crate_data.group_id)?;

                    // Get objective name for message (without holding a borrow)
                    let obj_name = self.persisted.objectives.get(&oid)
                        .map(|o| o.name.clone())
                        .unwrap_or_else(|| String::from("Unknown"));

                    let msg = if added_items.is_empty() {
                        String::from(format!("Supply transfer crate delivered to {} (warehouse full)", obj_name))
                    } else {
                        String::from(format!("Supply transfer crate delivered to {}: {}", obj_name, added_items.join(", ")))
                    };

                    info!("[SUPPLY_TRANSFER] {}", msg);
                    Ok(msg)
                } else {
                    bail!("No friendly objectives found for supply transfer")
                }
            }
        }
    }
}
