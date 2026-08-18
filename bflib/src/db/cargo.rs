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

use super::{Db, ephemeral::DeployableIndex, group::{SpawnedGroup, SpawnedUnit}, objective::Objective};
use anyhow::Context as _;
use crate::{
    db::group::DeployKind,
    group, maybe, objective,
    spawnctx::{SpawnCtx, SpawnLoc},
    unit, unit_mut,
};
use anyhow::{Result, anyhow, bail};
use bfprotocols::{
    cfg::{C130Vehicle, CargoConfig, Crate, Deployable, DeployableKind, DismountSpec, GroundVehicleCargo, LifeType, LimitEnforceTyp, Troop, Vehicle},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    stats::Stat,
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{
    LuaVec2, LuaVec3, MizLua, Position3, String, Vector2, Vector3, azumith2d, azumith2d_to,
    azumith3d, centroid2d,
    coalition::Side,
    controller::{ActionTyp, AltType, MissionPoint, PointType, Task, VehicleFormation},
    env::miz::MizIndex,
    group::Group,
    land::Land,
    net::{SlotId, Ucid},
    object::DcsObject,
    radians_to_degrees,
    trigger::{FlareColor, Trigger},
    unit::Unit,
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
    #[serde(default)]
    pub jtac: Option<bfprotocols::cfg::JtacState>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InternalPilot {
    pub ucid: Ucid,
    pub name: String,
    pub life_type: LifeType,
}

/// Troops being transported inside a ground vehicle (IFV/APC).
/// Stored in `Ephemeral.ground_vehicle_passengers` keyed by vehicle UnitId.
#[derive(Debug, Clone)]
pub struct GroundVehiclePassengers {
    pub vehicle_unit_id: bfprotocols::db::group::UnitId,
    /// DCS unit name of the carrier vehicle (for cargo-weight API calls).
    pub vehicle_name: dcso3::String,
    pub side: Side,
    pub troops: SmallVec<[InternalTroop; 4]>,
    pub loaded_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Cargo {
    pub troops: SmallVec<[InternalTroop; 2]>,
    pub crates: SmallVec<[(ObjectiveId, Crate); 1]>,
    #[serde(default)]
    pub pilots: SmallVec<[InternalPilot; 1]>,
}

impl Cargo {
    pub fn num_troops(&self) -> usize {
        self.troops.len()
    }

    pub fn num_crates(&self) -> usize {
        self.crates.len()
    }

    pub fn num_pilots(&self) -> usize {
        self.pilots.len()
    }

    pub fn num_total(&self) -> usize {
        self.num_crates() + self.num_troops() + self.num_pilots()
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
    /// Supply transfer crate for fuel
    SupplyTransferFuel,
    /// Supply transfer crate for weapons/equipment
    SupplyTransferWeapons,
    /// Carrier repair crate
    CarrierRepair,
    /// Vehicle that can be loaded and airdropped
    Vehicle { name: String, template: String },
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
    /// Position where the crate was originally spawned (used to detect slingload delivery)
    pub spawn_pos: Vector2,
    /// Time when crate was spawned
    pub spawn_time: DateTime<Utc>,
    /// Time when crate entered airborne state (for tracking)
    pub airborne_time: Option<DateTime<Utc>>,
    /// The actual crate definition from config (for crates)
    pub crate_def: Crate,
    /// The vehicle definition from config (for vehicles)
    pub vehicle_def: Option<C130Vehicle>,
    /// If false the crate must be manually unpacked (helicopter dynamic cargo)
    pub auto_unpack: bool,
    /// Whether the "need more crates" panel message has already been sent for
    /// this crate while landed but incomplete -- auto-unpack retries every
    /// tick (so a late-arriving sibling still triggers unpack promptly), but
    /// the message itself should only ever be shown once, not every tick.
    pub notified_missing: bool,
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
        auto_unpack: bool,
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
            spawn_pos: pos,
            spawn_time: Utc::now(),
            airborne_time: None,
            crate_def,
            vehicle_def: None,
            auto_unpack,
            notified_missing: false,
        }
    }

    pub fn new_vehicle(
        name: String,
        group_id: GroupId,
        crate_type: C130CargoType,
        origin: ObjectiveId,
        player: Ucid,
        side: Side,
        pos: Vector2,
        crate_def: Crate,
        vehicle_def: C130Vehicle,
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
            spawn_pos: pos,
            spawn_time: Utc::now(),
            airborne_time: None,
            crate_def,
            vehicle_def: Some(vehicle_def),
            auto_unpack: true,
            notified_missing: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SlotStats {
    pub name: String,
    pub typ: String,
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
        let typ = unit.get_type_name()?.clone();
        let pos = unit.get_position()?;
        let point = Vector2::new(pos.p.x, pos.p.z);
        let ground_alt = Land::singleton(lua)?.get_height(LuaVec2(point))?;
        let agl = pos.p.y - ground_alt;
        let speed = unit.get_velocity()?.0.magnitude() * 3600. / 1000.;
        Ok(Self {
            name,
            typ,
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
        info!("[CRATE_SPAWN] Spawning crate '{}' for slot {:?}", name, slot);
        let st = SlotStats::get(self, lua, slot)?;
        info!("[CRATE_SPAWN] Player pos=({:.0},{:.0}), side={:?}, in_air={}", st.point.x, st.point.y, st.side, st.in_air);
        if st.in_air {
            bail!("you must land to spawn crates")
        }
        let dir = Vector2::new(st.pos.x.x, st.pos.x.z);
        
        let is_c130 = self.ephemeral.cfg.c130_cargo.as_ref()
            .map(|c| c.loadable_vehicles.values()
                .flat_map(|v| v.iter())
                .any(|v| v.name == st.typ))
            .unwrap_or(false);

        let cargo_cfg = self.ephemeral.cfg.cargo.get(&Vehicle(st.typ.clone()));

        let spawn_distance = cargo_cfg
            .and_then(|cc| cc.spawn_distance)
            .unwrap_or_else(|| {
                if is_c130 {
                    self.ephemeral.cfg.c130_cargo.as_ref()
                        .and_then(|c| c.spawn_distance)
                        .unwrap_or(-45.0)
                } else {
                    self.ephemeral.cfg.helo_cargo.as_ref()
                        .and_then(|c| c.spawn_distance)
                        .unwrap_or(20.0)
                }
            });

        let forward = dir;
        let right = Vector2::new(-dir.y, dir.x);

        let mut approx_spawn_pos = st.point + forward * spawn_distance;
        let mut found_spot = false;

        let row_sign = if spawn_distance < 0.0 { -1.0 } else { 1.0 };

        // 3-wide grid: 3 rows, 3 columns
        for i in 0..9 {
            let row = (i / 3) as f64;
            let col = (i % 3) as f64 - 1.0;
            let test_pos = st.point + forward * (spawn_distance + row_sign * row * 5.0) + right * (col * 5.0);
            
            if self.list_crates_near_point(test_pos, 4.0)?.is_empty() {
                approx_spawn_pos = test_pos;
                found_spot = true;
                break;
            }
        }

        if !found_spot {
            bail!("no clear space to spawn crate, move away from other crates")
        }
        let to_delete = self.ephemeral.cfg.max_crates.and_then(|max_crates| {
            let crates = &self.persisted.players[&st.ucid].crates;
            if crates.len() < max_crates as usize {
                None
            } else {
                crates.into_iter().next().map(|id| *id)
            }
        });
        let (oid, _) = self.point_near_logistics(st.side, st.point)
            .map_err(|e| {
                // Log nearby objectives for debugging
                for (oid, obj) in &self.persisted.objectives {
                    let dist = na::distance(&st.point.into(), &obj.zone.pos().into());
                    if dist < 10000.0 {
                        info!("[CRATE_SPAWN] Nearby objective {:?} '{}' owner={:?} logi={} dist={:.0}m contains={}",
                              oid, obj.name, obj.owner, obj.logi(), dist, obj.zone.contains(st.point));
                    }
                }
                e
            })?;
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
        let spawnpos = SpawnLoc::AtPosWithCenter {
            pos: approx_spawn_pos,
            center: approx_spawn_pos,
        };
        let dk = DeployKind::Crate {
            origin: oid,
            player: st.ucid.clone(),
            spec: crate_cfg.clone(),
        };
        if let Some(gid) = to_delete {
            self.delete_group(&gid)?;
        }
        // Check if player is on a carrier - if so, link the crate to the carrier unit
        let carrier_link = self.find_carrier_unit_at_position(lua, st.point, st.side)?;
        info!("[CRATE_SPAWN] Carrier link result: {:?}", carrier_link);
        let group_id = self.add_and_queue_group(
            &SpawnCtx::new(lua)?,
            idx,
            st.side,
            spawnpos,
            &template,
            dk,
            BitFlags::empty(),
            None,
        )?;
        info!("[CRATE_SPAWN] Crate group created: {:?}, template='{}'", group_id, template);
        if let Some(link_name) = carrier_link {
            info!("[CRATE_SPAWN] Linking crate {:?} to carrier unit '{}'", group_id, link_name);
            self.ephemeral.carrier_linked_groups.insert(group_id, link_name);
        } else {
            info!("[CRATE_SPAWN] No carrier detected, crate will spawn without ship link");
        }
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
                | DeployKind::Action { .. }
                | DeployKind::DownedPilot { .. }
                | DeployKind::Dismount { .. } => {
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

    /// Find a spawn point offset `spawn_distance` from `point` along `dir`
    /// (negative = behind, positive = ahead), scanning a 3-wide grid of rows
    /// further out along that offset so successive crates land next to each
    /// other instead of stacking on top of one another or on other players'
    /// crates. Returns `None` if every cell in the grid is occupied.
    fn find_crate_spawn_point(
        &self,
        point: Vector2,
        dir: Vector2,
        spawn_distance: f64,
    ) -> Result<Option<Vector2>> {
        let forward = dir;
        let right = Vector2::new(-dir.y, dir.x);
        let row_sign = if spawn_distance < 0.0 { -1.0 } else { 1.0 };
        for i in 0..9 {
            let row = (i / 3) as f64;
            let col = (i % 3) as f64 - 1.0;
            let test_pos = point + forward * (spawn_distance + row_sign * row * 5.0) + right * (col * 5.0);
            if self.list_crates_near_point(test_pos, 4.0)?.is_empty() {
                return Ok(Some(test_pos));
            }
        }
        Ok(None)
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

    pub fn destroy_all_nearby_crates(&mut self, lua: MizLua, slot: &SlotId) -> Result<usize> {
        let st = SlotStats::get(self, lua, slot)?;
        if st.in_air {
            bail!("you must land to destroy crates")
        }
        let nearby = self.list_nearby_crates(&st)?;
        if nearby.is_empty() {
            bail!("no nearby crates")
        }
        let gids: SmallVec<[GroupId; 4]> = nearby.iter().map(|nc| nc.group.id).collect();
        drop(nearby);
        let n = gids.len();
        for gid in gids {
            self.delete_group(&gid)?;
        }
        Ok(n)
    }

    pub fn list_cargo(&self, slot: &SlotId) -> Option<&Cargo> {
        self.ephemeral.cargo.get(slot)
    }

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
                // Check both fuel and weapons transfer crates
                let mut valid_names = Vec::new();
                if let Some(fuel_crate) = whcfg.supply_transfer_fuel_crate.get(&side) {
                    valid_names.push(&fuel_crate.name);
                }
                if let Some(weapons_crate) = whcfg.supply_transfer_weapons_crate.get(&side) {
                    valid_names.push(&weapons_crate.name);
                }
                nearby
                    .iter()
                    .filter(|ci| valid_names.contains(&&ci.crate_def.name))
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
                            | DeployKind::Action { .. }
                            | DeployKind::DownedPilot { .. }
                            | DeployKind::Dismount { .. } => (),
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
        // Returns Some((name, dist_m)) of the blocking objective, or None if clear to unpack.
        fn too_close<'a, I: Iterator<Item = &'a Cifo>, F: Fn() -> I>(
            db: &Db,
            side: Side,
            centroid: Vector2,
            logistics: bool,
            iter: F,
        ) -> Option<(String, f64)> {
            let excl_dist = db.ephemeral.cfg.logistics_exclusion as f64;
            let excl_dist_sq = excl_dist.powi(2);
            db.persisted.objectives.into_iter().find_map(|(oid, obj)| {
                let _is_enemy = obj.owner != side && obj.owner != Side::Neutral;
                let mut check = false;
                if logistics {
                    for cr in iter() {
                        check |= oid == &cr.origin;
                    }
                    check |= obj.owner == side;
                } else {
                    // Block unpacking inside friendly objectives (prevent base-stuffing).
                    // Enemy objectives are always allowed — players need to build there to capture.
                    check = obj.owner == side;
                }
                if check && (logistics || obj.owner == side) {
                    let dist_sq = na::distance_squared(&obj.zone.pos().into(), &centroid.into());
                    if dist_sq <= excl_dist_sq || obj.zone.scale(1.1).contains(centroid.into()) {
                        let dist = dist_sq.sqrt();
                        return Some((obj.name.clone().into(), dist));
                    }
                }
                None
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
                    let gid = base_repairs.keys().next()
                        .ok_or_else(|| anyhow!("no base repair crates found"))?;
                    self.delete_group(gid)?;
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
                let (gid, _) = supply_transfer.into_iter().next()
                    .ok_or_else(|| anyhow!("no supply transfer crates found"))?;
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
                let (dep, have) = candidates.drain().next()
                    .ok_or_else(|| anyhow!("no deployable candidates found"))?;
                let spec = maybe!(didx.deployables_by_name, dep, "deployable")?.clone();
                let centroid = centroid2d(have.values().flat_map(|c| c.iter()).map(|c| c.pos));
                let blocking =
                    too_close(self, st.side, centroid, spec.kind.is_objective(), || {
                        have.values().flat_map(|c| c.iter())
                    });
                if let Some((obj_name, dist)) = blocking {
                    let needed = (self.ephemeral.cfg.logistics_exclusion as f64 - dist).max(50.0);
                    reasons.push(format_compact!(
                        "can't unpack here — too close to friendly objective '{}', move {:.0}m away",
                        obj_name, needed
                    ));
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
                                    jtac: None,
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
                let (dep, (gid, have)) = candidates.drain().next()
                    .ok_or_else(|| anyhow!("no repairable candidates found"))?;
                let spec = maybe!(didx.deployables_by_name, dep, "deployable")?.clone();
                let player = maybe!(self.persisted.players, &st.ucid, "player")?;
                let centroid = centroid2d(have.iter().map(|c| c.pos));
                if spec.repair_cost > 0 && spec.repair_cost as i32 > player.points {
                    reasons.push(format_compact!(
                        "Repairing {dep} costs {}, you have {}",
                        spec.repair_cost,
                        player.points
                    ));
                } else if let Some((obj_name, dist)) = too_close(self, st.side, centroid, false, || have.iter()) {
                    let needed = (self.ephemeral.cfg.logistics_exclusion as f64 - dist).max(50.0);
                    reasons.push(format_compact!(
                        "can't repair here — too close to friendly objective '{}', move {:.0}m away",
                        obj_name, needed
                    ))
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
        let cargo = self.ephemeral.cargo.get_mut(slot)
            .ok_or_else(|| anyhow!("no cargo state for slot"))?;
        let (oid, crate_cfg) = cargo.crates.pop()
            .ok_or_else(|| anyhow!("no crates onboard"))?;
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
            if let Some(cargo) = self.ephemeral.cargo.get_mut(slot) {
                cargo.crates.push((oid, crate_cfg));
            }
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
            // Filter out physical/dynamic crates — those must be loaded via DCS slingload,
            // not the old slot-based system which would destroy them
            nearby.retain(|nc| {
                !self.ephemeral.c130_crates.values().any(|c| c.group_id == nc.group.id)
            });
            if nearby.is_empty() {
                bail!(
                    "no friendly crates within {} meters",
                    self.ephemeral.cfg.crate_load_distance
                );
            }
            let the_crate = nearby.first()
                .ok_or_else(|| anyhow!("no nearby crates found"))?;
            let gid = the_crate.group.id;
            let crate_def = the_crate.crate_def.clone();
            let oid = the_crate.origin;
            (gid, oid, crate_def)
        };
        let cargo = self.ephemeral.cargo.get_mut(slot)
            .ok_or_else(|| anyhow!("no cargo state for slot"))?;
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
            jtac: None,
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
        let cargo = self.ephemeral.cargo.get(slot)
            .ok_or_else(|| anyhow!("no cargo state for slot"))?;
        let it = cargo.troops.last()
            .ok_or_else(|| anyhow!("no troops onboard"))?;
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
        let cargo = self.ephemeral.cargo.get_mut(slot)
            .ok_or_else(|| anyhow!("no cargo state for slot"))?;
        let it = cargo.troops.pop()
            .ok_or_else(|| anyhow!("no troops onboard"))?;
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
            jtac: it.jtac.clone(),
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
                if let Some(cargo) = self.ephemeral.cargo.get_mut(slot) {
                    cargo.troops.push(it);
                }
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
        let cargo = self.ephemeral.cargo.get_mut(slot)
            .ok_or_else(|| anyhow!("no cargo state for slot"))?;
        let it = cargo.troops.pop()
            .ok_or_else(|| anyhow!("no troops onboard"))?;
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

    pub fn extract_troops(&mut self, lua: MizLua, jtacs: &crate::jtac::Jtacs, slot: &SlotId) -> Result<(Troop, GroupId)> {
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
                        jtac: _,
                        ..
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
                                        jtac: jtacs.get(&crate::jtac::JtId::Group(gid)).ok().map(|j| j.state()),
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
        Ok((troop_cfg, gid))
    }

    // ===== Ground Vehicle Troop Transport =====

    /// Find a friendly ground vehicle with GroundVehicleCargo config within board radius.
    /// Returns (UnitId, vehicle_name, cfg) if found.
    fn nearby_boardable_vehicle(
        &self,
        lua: MizLua,
        slot: &dcso3::net::SlotId,
    ) -> Result<Option<(bfprotocols::db::group::UnitId, dcso3::String, GroundVehicleCargo)>> {
        let player_pos_3d = self.ephemeral.slot_instance_pos(lua, slot)?;
        let player_2d = Vector2::new(player_pos_3d.p.x, player_pos_3d.p.z);
        let side = self.ephemeral.get_slot_info(slot)
            .ok_or_else(|| anyhow!("no slot info for {slot:?}"))?.side;

        for (uid, unit) in self.persisted.units.into_iter() {
            if unit.dead || unit.side != side {
                continue;
            }
            let cfg = match self.ephemeral.cfg.ground_vehicle_cargo.get(&unit.typ) {
                Some(c) => c.clone(),
                None => continue,
            };
            let unit_2d = unit.pos;
            let dist_sq = na::distance_squared(&player_2d.into(), &unit_2d.into());
            if dist_sq <= cfg.board_radius_m.powi(2) {
                return Ok(Some((*uid, unit.name.clone(), cfg)));
            }
        }
        Ok(None)
    }

    /// Board an infantry squad into a friendly ground vehicle.
    /// The slot must belong to a player who previously loaded troops into their aircraft/unit.
    /// Troops are moved from the aircraft cargo into the vehicle's passenger manifest.
    pub fn board_ground_vehicle(
        &mut self,
        lua: MizLua,
        slot: &dcso3::net::SlotId,
    ) -> Result<(Troop, bfprotocols::db::group::UnitId)> {
        let cargo = self.ephemeral.cargo.get(slot);
        if cargo.map(|c| c.troops.is_empty()).unwrap_or(true) {
            bail!("no troops onboard your transport to transfer to a ground vehicle")
        }
        let side = self.ephemeral.get_slot_info(slot)
            .ok_or_else(|| anyhow!("no slot info for {slot:?}"))?.side;
        let (vehicle_uid, vehicle_name, gv_cfg) = self
            .nearby_boardable_vehicle(lua, slot)?
            .ok_or_else(|| anyhow!("no boardable vehicle within range"))?;

        // Enforce capacity.
        let current = self.ephemeral.ground_vehicle_passengers
            .get(&vehicle_uid)
            .map(|p| p.troops.len())
            .unwrap_or(0);
        if current >= gv_cfg.troop_capacity as usize {
            bail!("that vehicle is at full troop capacity ({} squads)", gv_cfg.troop_capacity)
        }

        // Pop troop from aircraft cargo.
        let troop = {
            let cargo = self.ephemeral.cargo.get_mut(slot)
                .ok_or_else(|| anyhow!("no cargo state"))?;
            cargo.troops.pop().ok_or_else(|| anyhow!("no troops onboard"))?
        };
        let troop_name = troop.troop.name.clone();

        // Insert into vehicle passengers.
        let pax = self.ephemeral.ground_vehicle_passengers
            .entry(vehicle_uid)
            .or_insert_with(|| GroundVehiclePassengers {
                vehicle_unit_id: vehicle_uid,
                vehicle_name: vehicle_name.clone(),
                side,
                troops: SmallVec::new(),
                loaded_at: Utc::now(),
            });
        let troop_cfg = troop.troop.clone();
        pax.troops.push(troop);

        log::info!("player loaded {troop_name} into ground vehicle {vehicle_name}");
        Ok((troop_cfg, vehicle_uid))
    }

    /// Dismount troops from a ground vehicle — player-commanded.
    /// Spawns the infantry group at the vehicle's current position.
    pub fn disembark_ground_vehicle(
        &mut self,
        lua: MizLua,
        idx: &dcso3::env::miz::MizIndex,
        mut vehicle_uid: bfprotocols::db::group::UnitId,
        slot: &dcso3::net::SlotId,
    ) -> Result<(Troop, GroupId)> {
        let side = self.ephemeral.get_slot_info(slot)
            .ok_or_else(|| anyhow!("no slot info for {slot:?}"))?.side;

        if vehicle_uid.inner() == 0 {
            // Find nearest friendly vehicle with troops
            let player_pos_3d = self.ephemeral.slot_instance_pos(lua, slot)?;
            let p2d = Vector2::new(player_pos_3d.p.x, player_pos_3d.p.z);
            let mut nearest = None;
            let mut nearest_dist = f64::MAX;
            for pax in self.ephemeral.ground_vehicle_passengers.values() {
                if pax.side != side || pax.troops.is_empty() { continue; }
                if let Some(unit) = self.persisted.units.get(&pax.vehicle_unit_id) {
                    let u2d = Vector2::new(unit.pos.x, unit.pos.y);
                    let dist = na::distance_squared(&p2d.into(), &u2d.into());
                    if dist < nearest_dist {
                        nearest_dist = dist;
                        nearest = Some(pax.vehicle_unit_id);
                    }
                }
            }
            if let Some(uid) = nearest {
                vehicle_uid = uid;
            } else {
                bail!("no friendly ground vehicles with troops found nearby");
            }
        }

        let pax = self.ephemeral.ground_vehicle_passengers
            .get_mut(&vehicle_uid)
            .filter(|p| !p.troops.is_empty())
            .ok_or_else(|| anyhow!("no troops aboard that vehicle"))?;
        let it = pax.troops.pop().ok_or_else(|| anyhow!("no troops aboard"))?;
        let side = pax.side;

        // Find vehicle position from persisted units.
        let unit_pos = self.persisted.units.get(&vehicle_uid)
            .map(|u| u.position)
            .ok_or_else(|| anyhow!("vehicle unit not found"))?;
        let point = Vector2::new(unit_pos.p.x, unit_pos.p.z);
        let spawnpos = SpawnLoc::AtPos {
            pos: point,
            offset_direction: Vector2::new(unit_pos.x.x, unit_pos.x.z),
            group_heading: azumith3d(unit_pos.x.0),
        };
        let dk = DeployKind::Troop {
            player: it.player,
            moved_by: None,
            spec: it.troop.clone(),
            origin: it.origin,
            cost_fraction: it.cost_fraction,
            jtac: it.jtac.clone(),
        };
        let spctx = SpawnCtx::new(lua)?;
        let gid = match self.add_and_queue_group(
            &spctx,
            idx,
            side,
            spawnpos,
            &*it.troop.template,
            dk,
            BitFlags::empty(),
            None,
        ) {
            Ok(gid) => gid,
            Err(e) => {
                // Re-push on failure.
                if let Some(pax) = self.ephemeral.ground_vehicle_passengers.get_mut(&vehicle_uid) {
                    pax.troops.push(it);
                }
                return Err(e);
            }
        };
        let troop_cfg = it.troop.clone();
        self.ephemeral.stat(bfprotocols::stats::Stat::DeployTroop {
            gid,
            troop: it.troop.name.clone(),
            by: it.player,
        });
        Ok((troop_cfg, gid))
    }

    /// Called when a ground vehicle with passengers is destroyed.
    /// Survivors (if any) are spawned at the wreck position.
    /// A casualty roll is applied: each squad has a 50 % chance of surviving.
    pub fn on_ground_vehicle_destroyed(
        &mut self,
        lua: MizLua,
        idx: &dcso3::env::miz::MizIndex,
        vehicle_uid: bfprotocols::db::group::UnitId,
        wreck_pos: Vector2,
    ) -> Result<()> {
        let pax = match self.ephemeral.ground_vehicle_passengers.remove(&vehicle_uid) {
            Some(p) if !p.troops.is_empty() => p,
            _ => return Ok(()),
        };
        let side = pax.side;
        let mut rng = rand::thread_rng();
        for it in pax.troops {
            // 50 % survival chance per squad.
            if rand::Rng::r#gen::<f32>(&mut rng) < 0.5 {
                let spawnpos = SpawnLoc::AtPos {
                    pos: wreck_pos,
                    offset_direction: Vector2::new(1.0, 0.0),
                    group_heading: 0.0,
                };
                let dk = DeployKind::Troop {
                    player: it.player,
                    moved_by: None,
                    spec: it.troop.clone(),
                    origin: it.origin,
                    cost_fraction: it.cost_fraction,
                    jtac: it.jtac.clone(),
                };
                let spctx = match SpawnCtx::new(lua) {
                    Ok(s) => s,
                    Err(e) => { log::error!("spawn ctx error for dismount: {e}"); continue; }
                };
                if let Err(e) = self.add_and_queue_group(
                    &spctx, idx, side, spawnpos,
                    &*it.troop.template, dk, BitFlags::empty(), None,
                ) {
                    log::error!("failed to spawn survivor dismount: {e}");
                }
            }
        }
        Ok(())
    }

    // ===== CSAR System =====

    pub fn spawn_downed_pilot(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        ucid: Ucid,
        name: String,
        side: Side,
        life_type: LifeType,
        pos: Vector2,
    ) -> Result<GroupId> {
        let template = self
            .ephemeral
            .cfg
            .csar
            .as_ref()
            .ok_or_else(|| anyhow!("CSAR is not configured"))?
            .pilot_template
            .get(&side)
            .cloned()
            .ok_or_else(|| anyhow!("CSAR pilot_template not configured for side {:?}", side))?;
        let spawnpos = SpawnLoc::AtPos {
            pos,
            offset_direction: Vector2::new(1., 0.),
            group_heading: 0.,
        };
        let dk = DeployKind::DownedPilot {
            ucid,
            name: name.into(),
            life_type,
        };
        let spctx = SpawnCtx::new(lua)?;
        let gid = self.add_and_queue_group(
            &spctx,
            idx,
            side,
            spawnpos,
            &*template,
            dk,
            BitFlags::empty(),
            None,
        )?;
        self.persisted.downed_pilots.insert_cow(gid);
        self.persisted
            .downed_pilot_spawn_times
            .insert_cow(gid, Utc::now());

        // Spawn enemy search parties if configured
        let enemy_side = match side {
            Side::Blue => Side::Red,
            Side::Red => Side::Blue,
            Side::Neutral => Side::Neutral,
        };
        let (search_template, search_count) = {
            let c = self.ephemeral.cfg.csar.as_ref();
            let tmpl = c
                .and_then(|c| c.search_party_template.get(&enemy_side).cloned())
                .unwrap_or_default();
            let count = c.map(|c| c.search_party_size).unwrap_or(0);
            (tmpl, count)
        };
        if search_count > 0 && !search_template.is_empty() {
            // Find the nearest enemy objective to use as the origin
            let origin_oid = self
                .persisted
                .objectives
                .into_iter()
                .filter_map(|(oid, obj)| {
                    if obj.owner != enemy_side {
                        return None;
                    }
                    let dx = obj.pos().x - pos.x;
                    let dy = obj.pos().y - pos.y;
                    Some((*oid, dx * dx + dy * dy))
                })
                .min_by(|a, b| a.1.partial_cmp(&b.1).unwrap())
                .map(|(oid, _)| oid);
            if let Some(origin) = origin_oid {
                // Spawn each search party at an offset from the pilot so they approach from different angles
                for i in 0..search_count {
                    let angle = (i as f64) * std::f64::consts::TAU / (search_count as f64);
                    let offset_dist = 500.0 + (i as f64) * 100.0;
                    let search_pos = Vector2::new(
                        pos.x + angle.sin() * offset_dist,
                        pos.y + angle.cos() * offset_dist,
                    );
                    let spawn = SpawnLoc::AtPos {
                        pos: search_pos,
                        offset_direction: Vector2::new(1., 0.),
                        group_heading: 0.,
                    };
                    if let Err(e) = self.add_and_queue_group(
                        &spctx,
                        idx,
                        enemy_side,
                        spawn,
                        &*search_template,
                        DeployKind::Objective { origin },
                        BitFlags::empty(),
                        None,
                    ) {
                        error!("csar: failed to spawn search party {i}: {e:?}");
                    }
                }
            }
        }

        Ok(gid)
    }

    pub fn pickup_pilot(&mut self, lua: MizLua, slot: &SlotId) -> Result<String> {
        let (cargo_capacity, side, unit_name) = self.unit_cargo_cfg(slot)?;
        if cargo_capacity.pilot_slots == 0 {
            bail!("this aircraft cannot carry downed pilots")
        }
        let pos = self.ephemeral.slot_instance_pos(lua, slot)?;
        let point = Vector2::new(pos.p.x, pos.p.z);
        let max_dist = (self.ephemeral.cfg.crate_load_distance as f64).powi(2);
        let unit = self.ephemeral.slot_instance_unit(lua, slot)?;
        if unit.in_air()? {
            bail!("you must land to pick up a downed pilot")
        }
        let (gid, pilot) = self
            .persisted
            .downed_pilots
            .into_iter()
            .filter_map(|gid| self.persisted.groups.get(gid).map(|g| (*gid, g)))
            .find_map(|(gid, g)| {
                if g.side == side {
                    let in_range = g
                        .units
                        .into_iter()
                        .filter_map(|uid| self.persisted.units.get(uid))
                        .any(|u| {
                            na::distance_squared(&u.pos.into(), &point.into()) <= max_dist
                        });
                    if in_range {
                        if let DeployKind::DownedPilot { ucid, name, life_type } = &g.origin {
                            return Some((
                                gid,
                                InternalPilot {
                                    ucid: *ucid,
                                    name: name.clone().into(),
                                    life_type: *life_type,
                                },
                            ));
                        }
                    }
                }
                None
            })
            .ok_or_else(|| anyhow!("no downed pilots in range"))?;
        let cargo = self.ephemeral.cargo.entry(slot.clone()).or_default();
        if cargo_capacity.pilot_slots as usize <= cargo.num_pilots()
            || cargo_capacity.total_slots as usize <= cargo.num_total()
        {
            bail!("you already have a full load onboard")
        }
        let pilot_name = pilot.name.clone();
        cargo.pilots.push(pilot);
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(unit_name, cargo.weight() as i64)?;
        self.delete_group(&gid)?;
        Ok(pilot_name)
    }

    pub fn deliver_pilots(&mut self, lua: MizLua, slot: &SlotId) -> Result<Vec<InternalPilot>> {
        let cargo = self.ephemeral.cargo.get(slot);
        if cargo.map(|c| c.pilots.is_empty()).unwrap_or(true) {
            bail!("no downed pilots onboard")
        }
        let unit = self.ephemeral.slot_instance_unit(lua, slot)?;
        if unit.in_air()? {
            bail!("you must land to deliver pilots")
        }
        let unit_name = unit.get_name()?;
        let pos = unit.get_position()?;
        let point = Vector2::new(pos.p.x, pos.p.z);
        let side = self
            .ephemeral
            .get_slot_info(slot)
            .ok_or_else(|| anyhow!("no slot info for {slot:?}"))?
            .side;
        self.persisted
            .objectives
            .into_iter()
            .find(|(_, o)| o.owner == side && o.zone.contains(point))
            .ok_or_else(|| anyhow!("you must be at a friendly objective to deliver pilots"))?;
        let cargo = self.ephemeral.cargo.get_mut(slot)
            .ok_or_else(|| anyhow!("no cargo state for slot"))?;
        let pilots: Vec<InternalPilot> = cargo.pilots.drain(..).collect();
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(unit_name, cargo.weight())?;
        Ok(pilots)
    }

    fn move_pilot_toward(&self, lua: MizLua, gid: &GroupId, target: Vector2) -> Result<()> {
        let group = match self.persisted.groups.get(gid) {
            None => return Ok(()),
            Some(g) => g,
        };
        let dcs_group = Group::get_by_name(lua, group.name.as_str())?;
        let controller = dcs_group.get_controller()?;
        let land = Land::singleton(lua)?;
        let alt = land.get_height(LuaVec2(target)).unwrap_or(0.);
        let task = Task::Mission {
            airborne: Some(false),
            route: vec![MissionPoint {
                typ: PointType::TurningPoint,
                airdrome_id: None,
                time_re_fu_ar: None,
                helipad: None,
                link_unit: None,
                action: Some(ActionTyp::Ground(VehicleFormation::OffRoad)),
                pos: LuaVec2(target),
                alt,
                alt_typ: Some(AltType::BARO),
                speed: 3.5, // ~12 km/h — running pace for infantry
                speed_locked: Some(true),
                eta: None,
                eta_locked: None,
                name: None,
                task: Box::new(Task::Hold),
            }],
        };
        controller.set_task(task)?;
        Ok(())
    }

    /// Main CSAR tick — called every second from run_timed_events.
    /// Handles: flare on approach, AI movement, auto-board, auto-deliver.
    pub fn tick_csar(&mut self, lua: MizLua) -> Result<()> {
        let csar = match self.ephemeral.cfg.csar.as_ref() {
            Some(c) if c.enabled => c.clone(),
            _ => return Ok(()),
        };
        let pickup_r2 = (csar.pickup_radius as f64).powi(2);
        let board_r2 = (csar.board_radius as f64).powi(2);
        let enemy_cap_r2 = if csar.enemy_capture_radius > 0 {
            Some((csar.enemy_capture_radius as f64).powi(2))
        } else {
            None
        };
        let capture_timeout = if csar.capture_timer > 0 {
            Some(chrono::Duration::minutes(csar.capture_timer as i64))
        } else {
            None
        };
        let renotify_interval = if csar.renotify_interval > 0 {
            Some(chrono::Duration::minutes(csar.renotify_interval as i64))
        } else {
            None
        };
        let rescue_reward = csar.rescue_reward;
        let now = Utc::now();
        let move_interval = chrono::Duration::seconds(3);

        // ---- Collect ALL helicopters with pilot slots (airborne + ground) ----
        // Used for: flare proximity and new-pilot notifications.
        // (ucid, slot, position, side)
        let mut all_helos: Vec<(Ucid, SlotId, Vector2, Side)> = vec![];
        for (slot, ucid) in &self.ephemeral.players_by_slot {
            let ucid = *ucid;
            let player = match self.persisted.players.get(&ucid) {
                None => continue,
                Some(p) => p,
            };
            let inst = match player.current_slot.as_ref().and_then(|(_, i)| i.as_ref()) {
                None => continue,
                Some(i) => i,
            };
            let cargo_cfg = match self.ephemeral.cfg.cargo.get(&inst.typ) {
                None => continue,
                Some(c) => *c,
            };
            if cargo_cfg.pilot_slots == 0 {
                continue;
            }
            let pos = Vector2::new(inst.position.p.x, inst.position.p.z);
            all_helos.push((ucid, slot.clone(), pos, player.side));
        }

        // ---- Collect on-ground helicopters with available pilot capacity ----
        // (ucid, slot, position, available_pilot_slots_remaining)
        let mut helo_candidates: Vec<(Ucid, SlotId, Vector2, usize)> = vec![];
        for (slot, ucid) in &self.ephemeral.players_by_slot {
            let ucid = *ucid;
            let player = match self.persisted.players.get(&ucid) {
                None => continue,
                Some(p) => p,
            };
            let inst = match player.current_slot.as_ref().and_then(|(_, i)| i.as_ref()) {
                None => continue,
                Some(i) => i,
            };
            if inst.in_air {
                continue;
            }
            let cargo_cfg = match self.ephemeral.cfg.cargo.get(&inst.typ) {
                None => continue,
                Some(c) => *c,
            };
            if cargo_cfg.pilot_slots == 0 {
                continue;
            }
            let cargo = self.ephemeral.cargo.entry(slot.clone()).or_default();
            let available = cargo_cfg.pilot_slots as usize - cargo.num_pilots();
            let total_available = cargo_cfg.total_slots as usize - cargo.num_total();
            if available == 0 || total_available == 0 {
                continue;
            }
            let pos = Vector2::new(inst.position.p.x, inst.position.p.z);
            helo_candidates.push((ucid, slot.clone(), pos, available.min(total_available)));
        }

        // ---- Auto-deliver pilots at friendly objectives ----
        let slots_to_deliver: Vec<SlotId> = self
            .ephemeral
            .players_by_slot
            .keys()
            .filter(|slot| {
                self.ephemeral
                    .cargo
                    .get(*slot)
                    .map(|c| !c.pilots.is_empty())
                    .unwrap_or(false)
            })
            .filter(|slot| {
                let ucid = match self.ephemeral.players_by_slot.get(*slot) {
                    None => return false,
                    Some(u) => u,
                };
                let player = match self.persisted.players.get(ucid) {
                    None => return false,
                    Some(p) => p,
                };
                let inst = match player.current_slot.as_ref().and_then(|(_, i)| i.as_ref()) {
                    None => return false,
                    Some(i) => i,
                };
                !inst.in_air && inst.landed_at_objective.is_some()
            })
            .cloned()
            .collect();

        for slot in slots_to_deliver {
            let ucid_rescuer = self.ephemeral.players_by_slot.get(&slot).cloned();
            let rescuer_name = ucid_rescuer
                .as_ref()
                .and_then(|u| self.persisted.players.get(u))
                .map(|p| p.name.clone())
                .unwrap_or_else(|| String::from("unknown"));
            let side = ucid_rescuer
                .as_ref()
                .and_then(|u| self.persisted.players.get(u))
                .map(|p| p.side)
                .unwrap_or(Side::Blue);
            let pilots: Vec<InternalPilot> = match self.ephemeral.cargo.get_mut(&slot) {
                None => continue,
                Some(c) => c.pilots.drain(..).collect(),
            };
            for pilot in &pilots {
                if let Some(new_count) = self.restore_life(&pilot.ucid, pilot.life_type) {
                    let msg = format_compact!(
                        "your pilot was rescued by {rescuer_name} and delivered — you have {new_count} {} lives",
                        pilot.life_type,
                    );
                    self.ephemeral.panel_to_player(&self.persisted, 15, &pilot.ucid, msg);
                }
                if rescue_reward > 0 {
                    if let Some(ucid) = &ucid_rescuer {
                        self.adjust_points(
                            ucid,
                            rescue_reward as i32,
                            &format_compact!("CSAR rescue of {}", pilot.name),
                        );
                    }
                }
            }
            let n = pilots.len();
            let msg = format_compact!(
                "{rescuer_name} auto-delivered {n} rescued pilot{} at base",
                if n == 1 { "" } else { "s" }
            );
            self.ephemeral.msgs().panel_to_side(10, false, side, msg);
        }

        // ---- Process each downed pilot ----
        let pilot_gids: Vec<GroupId> =
            self.persisted.downed_pilots.into_iter().copied().collect();
        let act = Trigger::singleton(lua)?.action()?;
        let land = Land::singleton(lua)?;

        for gid in pilot_gids {
            // Get pilot centroid position
            let pilot_pos = {
                let group = match self.persisted.groups.get(&gid) {
                    None => continue,
                    Some(g) => g,
                };
                let positions: Vec<Vector2> = group
                    .units
                    .into_iter()
                    .filter_map(|uid| self.persisted.units.get(uid))
                    .filter(|u| !u.dead)
                    .map(|u| u.pos)
                    .collect();
                if positions.is_empty() {
                    continue;
                }
                centroid2d(positions.into_iter())
            };
            let (pilot_side, pilot_name) = match self.persisted.groups.get(&gid) {
                None => continue,
                Some(g) => {
                    let name = match &g.origin {
                        DeployKind::DownedPilot { name, .. } => name.clone(),
                        _ => continue,
                    };
                    (g.side, name)
                }
            };

            // ---- Capture timer: auto-capture if unrescued too long ----
            if let Some(timeout) = capture_timeout {
                let spawn_time = self
                    .persisted
                    .downed_pilot_spawn_times
                    .get(&gid)
                    .copied()
                    .unwrap_or(now);
                if now - spawn_time >= timeout {
                    // Notify downed player
                    if let Some(DeployKind::DownedPilot { ucid, life_type, .. }) =
                        self.persisted.groups.get(&gid).map(|g| g.origin.clone())
                    {
                        let msg = format_compact!(
                            "Your downed pilot was captured — no {} life restored",
                            life_type
                        );
                        self.ephemeral.panel_to_player(&self.persisted, 20, &ucid, msg);
                        let side_msg = format_compact!(
                            "CSAR: {pilot_name} was captured — rescue window expired"
                        );
                        self.ephemeral
                            .msgs()
                            .panel_to_side(15, false, pilot_side, side_msg);
                    }
                    self.delete_group(&gid)?;
                    continue;
                }
            }

            // ---- Enemy proximity: capture if enemy unit is close ----
            if let Some(cap_r2) = enemy_cap_r2 {
                let enemy_side = match pilot_side {
                    Side::Blue => Side::Red,
                    Side::Red => Side::Blue,
                    Side::Neutral => Side::Neutral,
                };
                let captured = self
                    .persisted
                    .groups_by_side
                    .get(&enemy_side)
                    .into_iter()
                    .flat_map(|s| s.into_iter())
                    .filter_map(|gid2| self.persisted.groups.get(gid2))
                    .flat_map(|g| g.units.into_iter())
                    .filter_map(|uid| self.persisted.units.get(uid))
                    .filter(|u| !u.dead)
                    .any(|u| {
                        let dx = pilot_pos.x - u.pos.x;
                        let dy = pilot_pos.y - u.pos.y;
                        dx * dx + dy * dy <= cap_r2
                    });
                if captured {
                    if let Some(DeployKind::DownedPilot { ucid, life_type, .. }) =
                        self.persisted.groups.get(&gid).map(|g| g.origin.clone())
                    {
                        let msg = format_compact!(
                            "Your downed pilot was captured by enemy forces — no {} life restored",
                            life_type
                        );
                        self.ephemeral.panel_to_player(&self.persisted, 20, &ucid, msg);
                        let side_msg = format_compact!(
                            "CSAR: {pilot_name} was captured by enemy forces!"
                        );
                        self.ephemeral
                            .msgs()
                            .panel_to_side(15, false, pilot_side, side_msg);
                    }
                    self.delete_group(&gid)?;
                    continue;
                }
            }

            // ---- Broadcast helper: send bearing/distance to all friendly helo pilots ----
            let broadcast_pilot_location = |helos: &[(Ucid, SlotId, Vector2, Side)]| {
                helos
                    .iter()
                    .filter(|(_, _, _, hside)| *hside == pilot_side)
                    .map(|(hucid, _, hpos, _)| {
                        let dx = pilot_pos.x - hpos.x;
                        let dy = pilot_pos.y - hpos.y;
                        let dist = (dx * dx + dy * dy).sqrt() as u32;
                        let bearing = ((dx.atan2(dy).to_degrees() + 360.) % 360.) as u32;
                        (*hucid, dist, bearing)
                    })
                    .collect::<Vec<_>>()
            };

            // ---- Initial notification: tell all friendly helo pilots about this downed pilot ----
            if !self.ephemeral.csar_notified.contains(&gid) {
                for (hucid, dist, bearing) in broadcast_pilot_location(&all_helos) {
                    let msg = format_compact!(
                        "CSAR: {pilot_name} is down — bearing {bearing}°, {dist}m"
                    );
                    self.ephemeral.panel_to_player(&self.persisted, 20, &hucid, msg);
                }
                self.ephemeral.csar_notified.insert(gid);
                self.ephemeral.csar_last_renotify.insert(gid, now);
            }

            // ---- Periodic renotify: remind helo pilots of still-active downed pilots ----
            if let Some(interval) = renotify_interval {
                let last = self
                    .ephemeral
                    .csar_last_renotify
                    .get(&gid)
                    .copied()
                    .unwrap_or(now);
                if now - last >= interval {
                    for (hucid, dist, bearing) in broadcast_pilot_location(&all_helos) {
                        let msg = format_compact!(
                            "CSAR reminder: {pilot_name} still down — bearing {bearing}°, {dist}m"
                        );
                        self.ephemeral.panel_to_player(&self.persisted, 20, &hucid, msg);
                    }
                    self.ephemeral.csar_last_renotify.insert(gid, now);
                }
            }

            // ---- Fire approach flare when any friendly helo (airborne or ground) is within pickup_radius ----
            if !self.ephemeral.csar_flared.contains(&gid) {
                let any_helo_close = all_helos.iter().any(|(_, _, hpos, hside)| {
                    *hside == pilot_side && {
                        let dx = pilot_pos.x - hpos.x;
                        let dy = pilot_pos.y - hpos.y;
                        dx * dx + dy * dy <= pickup_r2
                    }
                });
                if any_helo_close {
                    let alt = land.get_height(LuaVec2(pilot_pos)).unwrap_or(0.);
                    let flare_pos = LuaVec3(Vector3::new(pilot_pos.x, alt + 10., pilot_pos.y));
                    if let Err(e) = act.signal_flare(flare_pos, FlareColor::Green, 90) {
                        error!("csar: signal_flare failed: {e:?}");
                    }
                    self.ephemeral.csar_flared.insert(gid);
                }
            }

            // Find the closest suitable on-ground helicopter within pickup_radius
            let closest = helo_candidates
                .iter()
                .filter(|(_, _, _, avail)| *avail > 0)
                .filter(|(_, _, hpos, _)| {
                    let dx = pilot_pos.x - hpos.x;
                    let dy = pilot_pos.y - hpos.y;
                    dx * dx + dy * dy <= pickup_r2
                })
                .min_by(|a, b| {
                    let da = {
                        let dx = pilot_pos.x - a.2.x;
                        let dy = pilot_pos.y - a.2.y;
                        dx * dx + dy * dy
                    };
                    let db = {
                        let dx = pilot_pos.x - b.2.x;
                        let dy = pilot_pos.y - b.2.y;
                        dx * dx + dy * dy
                    };
                    da.partial_cmp(&db).unwrap()
                });

            if let Some((helo_ucid, helo_slot, helo_pos, _)) = closest {
                let helo_slot = helo_slot.clone();
                let helo_ucid = *helo_ucid;
                let helo_pos = *helo_pos;

                let dx = pilot_pos.x - helo_pos.x;
                let dy = pilot_pos.y - helo_pos.y;
                let dist2 = dx * dx + dy * dy;

                if dist2 <= board_r2 {
                    // Auto-board
                    let cargo = self.ephemeral.cargo.entry(helo_slot.clone()).or_default();
                    let pilot_info = match self.persisted.groups.get(&gid) {
                        None => continue,
                        Some(g) => match &g.origin {
                            DeployKind::DownedPilot { ucid, name, life_type } => InternalPilot {
                                ucid: *ucid,
                                name: name.clone().into(),
                                life_type: *life_type,
                            },
                            _ => continue,
                        },
                    };
                    let helo_cfg = self
                        .ephemeral
                        .cfg
                        .cargo
                        .get(
                            &self
                                .persisted
                                .players
                                .get(&helo_ucid)
                                .and_then(|p| {
                                    p.current_slot
                                        .as_ref()
                                        .and_then(|(_, i)| i.as_ref())
                                        .map(|i| i.typ.clone())
                                })
                                .unwrap_or_default(),
                        )
                        .copied();
                    let (pilot_cap, total_cap) = match helo_cfg {
                        None => continue,
                        Some(c) => (c.pilot_slots as usize, c.total_slots as usize),
                    };
                    if cargo.num_pilots() < pilot_cap
                        && cargo.num_total() < total_cap
                    {
                        let pilot_name = pilot_info.name.clone();
                        cargo.pilots.push(pilot_info);
                        // Update internal cargo weight display
                        let weight = cargo.weight();
                        if let Some(unit_name) = self
                            .ephemeral
                            .slot_info
                            .get(&helo_slot)
                            .map(|s| s.unit_name.clone())
                        {
                            if let Err(e) = act
                                .set_unit_internal_cargo(unit_name.as_str().into(), weight)
                            {
                                error!("csar auto-board: set_unit_internal_cargo: {e:?}");
                            }
                        }
                        self.delete_group(&gid)?;
                        self.ephemeral.csar_flared.remove(&gid);
                        self.ephemeral.csar_moving.remove(&gid);
                        self.ephemeral.csar_notified.remove(&gid);
                        // Decrement availability for subsequent pilots this tick
                        if let Some(cand) = helo_candidates.iter_mut().find(|(_, s, _, _)| *s == helo_slot) {
                            cand.3 = cand.3.saturating_sub(1);
                        }
                        let side = pilot_side;
                        let rescuer_name = self
                            .persisted
                            .players
                            .get(&helo_ucid)
                            .map(|p| p.name.clone())
                            .unwrap_or_else(|| String::from("unknown"));
                        let msg = format_compact!(
                            "{rescuer_name} picked up downed pilot {pilot_name} — auto-boarding!"
                        );
                        self.ephemeral.msgs().panel_to_side(10, false, side, msg);
                    }
                } else {
                    // Issue movement order toward helicopter (rate-limited)
                    let should_move = self
                        .ephemeral
                        .csar_moving
                        .get(&gid)
                        .map(|last| now - *last >= move_interval)
                        .unwrap_or(true);
                    if should_move {
                        if let Err(e) = self.move_pilot_toward(lua, &gid, helo_pos) {
                            error!("csar: move_pilot_toward: {e:?}");
                        } else {
                            self.ephemeral.csar_moving.insert(gid, now);
                        }
                    }
                }
            }
        }
        Ok(())
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

    /// Find the carrier unit at a given position if the position is near a carrier group.
    /// Returns the DCS unit name of the nearest carrier unit if found within range.
    /// This is used to link static objects (like crates) to the carrier so they move with it.
    ///
    /// Uses actual tracked unit positions instead of the static objective zone,
    /// so it works correctly even after the carrier has moved from its initial position.
    fn find_carrier_unit_at_position(&self, lua: MizLua, pos: Vector2, side: Side) -> Result<Option<String>> {
        // Maximum distance from a carrier unit to consider the player "on" the carrier.
        // Carrier decks are ~300m long, so 500m gives comfortable margin.
        const MAX_CARRIER_DISTANCE: f64 = 500.0;

        for (_, obj) in &self.persisted.objectives {
            if let ObjectiveKind::CarrierGroup { carrier_template, .. } = &obj.kind {
                if obj.owner != side || carrier_template.is_empty() {
                    continue;
                }

                // Find carrier groups matching this template and check actual unit positions
                for (_, group) in &self.persisted.groups {
                    if group.template_name.starts_with(carrier_template.as_str()) && group.side == side {
                        // Check if any live unit in this group is near the given position
                        let mut nearest: Option<(f64, &SpawnedUnit)> = None;
                        for uid in group.units.into_iter() {
                            if let Some(unit) = self.persisted.units.get(uid) {
                                if !unit.dead {
                                    // Use the tracked unit position (updated every tick)
                                    let dist = na::distance(&pos.into(), &unit.pos.into());
                                    if dist <= MAX_CARRIER_DISTANCE {
                                        if nearest.as_ref().map_or(true, |(d, _)| dist < *d) {
                                            nearest = Some((dist, unit));
                                        }
                                    }
                                }
                            }
                        }

                        if let Some((dist, unit)) = nearest {
                            info!("[CARRIER_LINK] Position is {:.0}m from carrier unit '{}' in group '{}' (template: '{}')",
                                  dist, unit.template_name, group.name, group.template_name);
                            // Try template_name first (original miz name), then bflib name
                            if Unit::get_by_name(lua, &unit.template_name).is_ok() {
                                info!("[CARRIER_LINK] Found carrier unit '{}' via template_name", unit.template_name);
                                return Ok(Some(unit.template_name.clone()));
                            }
                            if Unit::get_by_name(lua, &unit.name).is_ok() {
                                info!("[CARRIER_LINK] Found carrier unit '{}' via bflib name", unit.name);
                                return Ok(Some(unit.name.clone()));
                            }
                            info!("[CARRIER_LINK] Unit '{}' (bflib '{}') not found in DCS by either name — carrier may not be spawned yet",
                                  unit.template_name, unit.name);
                        }
                    }
                }
            }
        }
        info!("[CARRIER_LINK] No carrier found near position ({:.0},{:.0}) for side {:?}", pos.x, pos.y, side);
        Ok(None)
    }

    /// Find a pre-placed cargo spawn point marker (configured via
    /// cfg.carrier_cargo_spawn_point) within a carrier group near `pos`.
    /// Returns the marker's current live position and DCS unit name (for
    /// linking a newly spawned crate to move with the ship). Returns None
    /// if no marker is configured for this side, or none is found nearby.
    fn find_carrier_cargo_spawn_point(
        &self,
        lua: MizLua,
        pos: Vector2,
        side: Side,
    ) -> Result<Option<(Vector2, String)>> {
        let marker = match self.ephemeral.cfg.carrier_cargo_spawn_point.get(&side) {
            Some(m) if !m.is_empty() => m,
            _ => return Ok(None),
        };
        const MAX_CARRIER_DISTANCE: f64 = 500.0;
        for (_, obj) in &self.persisted.objectives {
            if let ObjectiveKind::CarrierGroup { carrier_template, .. } = &obj.kind {
                if obj.owner != side || carrier_template.is_empty() {
                    continue;
                }
                for (_, group) in &self.persisted.groups {
                    if group.template_name.starts_with(carrier_template.as_str()) && group.side == side {
                        for uid in group.units.into_iter() {
                            let Some(unit) = self.persisted.units.get(uid) else { continue };
                            if unit.dead || !unit.template_name.starts_with(marker.as_str()) {
                                continue;
                            }
                            if na::distance(&pos.into(), &unit.pos.into()) > MAX_CARRIER_DISTANCE {
                                continue;
                            }
                            let link_name = if Unit::get_by_name(lua, &unit.template_name).is_ok() {
                                unit.template_name.clone()
                            } else if Unit::get_by_name(lua, &unit.name).is_ok() {
                                unit.name.clone()
                            } else {
                                continue;
                            };
                            // Use the marker's live DCS position, not the
                            // possibly-stale DB-tracked one, so the crate
                            // lands where the marker actually is right now.
                            let live_pos = Unit::get_by_name(lua, &link_name)
                                .and_then(|u| u.get_position())
                                .map(|p| Vector2::new(p.p.x, p.p.z))
                                .unwrap_or(unit.pos);
                            info!("[CARRIER_LINK] Found cargo spawn point marker '{}' at ({:.0},{:.0})",
                                  link_name, live_pos.x, live_pos.y);
                            return Ok(Some((live_pos, link_name)));
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    /// Manually unpack nearby dynamic crates for helicopters (no auto-unpack on landing).
    /// Finds all tracked c130-style crates within `crate_load_distance` of the player
    /// that have `auto_unpack: false`, then calls `unpack_c130_crate` on each one.
    pub fn unpack_nearby_helo_crates(&mut self, lua: MizLua, idx: &MizIndex, slot: &SlotId) -> Result<String> {
        let st = SlotStats::get(self, lua, slot)?;
        let radius = self.ephemeral.cfg.crate_load_distance as f64;

        let nearby: Vec<(String, C130Cargo)> = self.ephemeral.c130_crates
            .iter()
            .filter(|(_, c)| {
                !c.auto_unpack
                    && c.side == st.side
                    && na::distance(&c.last_pos.into(), &st.point.into()) <= radius
            })
            .map(|(name, c)| (name.clone(), c.clone()))
            .collect();

        if nearby.is_empty() {
            return Ok(String::from(format_compact!(
                "No friendly dynamic crates within {} meters to unpack",
                self.ephemeral.cfg.crate_load_distance
            )));
        }

        let mut msgs: Vec<compact_str::CompactString> = Vec::new();
        for (name, crate_data) in nearby {
            match self.unpack_c130_crate(lua, idx, &crate_data, &name) {
                Ok(msg) => msgs.push(compact_str::CompactString::from(msg.as_str())),
                Err(e) => msgs.push(format_compact!("Failed to unpack {}: {}", name, e)),
            }
        }

        Ok(String::from(msgs.join("\n").as_str()))
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
        auto_unpack: bool,
    ) -> Result<String> {
        debug!("[C130_CARGO] spawn_c130_crate called: crate={}, side={:?}, origin={:?}, slot={:?}",
            crate_name, side, origin, slot);

        let ucid = maybe!(self.ephemeral.players_by_slot, *slot, "no such player")?.clone();
        let dep_idx = self.deployable_idx(side)?;

        // Get crate definition (check supply transfer and carrier repair FIRST to avoid name conflicts)
        let (crate_def, crate_type) = if let Some(whcfg) = &self.ephemeral.cfg.warehouse {
            // Check fuel transfer crate
            if let Some(fuel_crate) = whcfg.supply_transfer_fuel_crate.get(&side) {
                if fuel_crate.name == crate_name {
                    debug!("[C130_CARGO] Found fuel transfer crate: {}, weight={}kg", crate_name, fuel_crate.weight);
                    (fuel_crate.clone(), C130CargoType::SupplyTransferFuel)
                } else if let Some(weapons_crate) = whcfg.supply_transfer_weapons_crate.get(&side) {
                    if weapons_crate.name == crate_name {
                        debug!("[C130_CARGO] Found weapons transfer crate: {}, weight={}kg", crate_name, weapons_crate.weight);
                        (weapons_crate.clone(), C130CargoType::SupplyTransferWeapons)
                    } else if let Some(carrier_repair_crate) = whcfg.carrier_repair_crate.get(&side) {
                        if carrier_repair_crate.name == crate_name {
                            debug!("[C130_CARGO] Found carrier repair crate: {}, weight={}kg", crate_name, carrier_repair_crate.weight);
                            (carrier_repair_crate.clone(), C130CargoType::CarrierRepair)
                        } else if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
                            debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
                            (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
                        } else {
                            error!("[C130_CARGO] Crate not found: {}", crate_name);
                            bail!("crate {} not found", crate_name)
                        }
                    } else if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
                        debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
                        (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
                    } else {
                        error!("[C130_CARGO] Crate not found: {}", crate_name);
                        bail!("crate {} not found", crate_name)
                    }
                } else if let Some(carrier_repair_crate) = whcfg.carrier_repair_crate.get(&side) {
                    if carrier_repair_crate.name == crate_name {
                        debug!("[C130_CARGO] Found carrier repair crate: {}, weight={}kg", crate_name, carrier_repair_crate.weight);
                        (carrier_repair_crate.clone(), C130CargoType::CarrierRepair)
                    } else if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
                        debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
                        (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
                    } else {
                        error!("[C130_CARGO] Crate not found: {}", crate_name);
                        bail!("crate {} not found", crate_name)
                    }
                } else if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
                    debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
                    (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
                } else {
                    error!("[C130_CARGO] Crate not found: {}", crate_name);
                    bail!("crate {} not found", crate_name)
                }
            } else if let Some(weapons_crate) = whcfg.supply_transfer_weapons_crate.get(&side) {
                if weapons_crate.name == crate_name {
                    debug!("[C130_CARGO] Found weapons transfer crate: {}, weight={}kg", crate_name, weapons_crate.weight);
                    (weapons_crate.clone(), C130CargoType::SupplyTransferWeapons)
                } else if let Some(carrier_repair_crate) = whcfg.carrier_repair_crate.get(&side) {
                    if carrier_repair_crate.name == crate_name {
                        debug!("[C130_CARGO] Found carrier repair crate: {}, weight={}kg", crate_name, carrier_repair_crate.weight);
                        (carrier_repair_crate.clone(), C130CargoType::CarrierRepair)
                    } else if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
                        debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
                        (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
                    } else {
                        error!("[C130_CARGO] Crate not found: {}", crate_name);
                        bail!("crate {} not found", crate_name)
                    }
                } else if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
                    debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
                    (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
                } else {
                    error!("[C130_CARGO] Crate not found: {}", crate_name);
                    bail!("crate {} not found", crate_name)
                }
            } else if let Some(carrier_repair_crate) = whcfg.carrier_repair_crate.get(&side) {
                if carrier_repair_crate.name == crate_name {
                    debug!("[C130_CARGO] Found carrier repair crate: {}, weight={}kg", crate_name, carrier_repair_crate.weight);
                    (carrier_repair_crate.clone(), C130CargoType::CarrierRepair)
                } else if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
                    debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
                    (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
                } else {
                    error!("[C130_CARGO] Crate not found: {}", crate_name);
                    bail!("crate {} not found", crate_name)
                }
            } else if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
                debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
                (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
            } else {
                error!("[C130_CARGO] Crate not found: {}", crate_name);
                bail!("crate {} not found", crate_name)
            }
        } else {
            // No warehouse config, just try deployable
            if let Some(crate_def) = dep_idx.crates_by_name.get(&crate_name) {
                debug!("[C130_CARGO] Found deployable crate: {}, weight={}kg", crate_name, crate_def.weight);
                (crate_def.clone(), C130CargoType::Deployable { name: crate_name.clone() })
            } else {
                error!("[C130_CARGO] Crate not found: {}", crate_name);
                bail!("crate {} not found", crate_name)
            }
        };

        // Get player position and direction (using same method as regular cargo system)
        let unit = self.ephemeral.slot_instance_unit(lua, slot)?;
        let pos = unit.get_position()?;
        let point = Vector2::new(pos.p.x, pos.p.z);

        // Get direction vector from position matrix (same as regular cargo system)
        let dir = Vector2::new(pos.x.x, pos.x.z);
        debug!("[C130_CARGO] Player position: x={:.2}, z={:.2}, dir=({:.2}, {:.2})",
               point.x, point.y, dir.x, dir.y);

        // Offset each crate 5m further in the forward direction so they don't stack
        // Per-vehicle spawn_distance takes priority, then global c130/helo config
        let unit_typ = unit.get_type_name()?;
        let cargo_cfg = self.ephemeral.cfg.cargo.get(&Vehicle(unit_typ.clone()));
        
        let spawn_distance = cargo_cfg
            .and_then(|cc| cc.spawn_distance)
            .unwrap_or_else(|| {
                if auto_unpack {
                    self.ephemeral.cfg.c130_cargo.as_ref()
                        .and_then(|c| c.spawn_distance)
                        .unwrap_or(-45.0)
                } else {
                    self.ephemeral.cfg.helo_cargo.as_ref()
                        .and_then(|c| c.spawn_distance)
                        .unwrap_or(4.0)
                }
            });

        // If the player is on a carrier with a configured cargo spawn point
        // marker, anchor the grid scan on the marker's live position instead
        // of a player-relative offset -- an offset computed from the player
        // can easily land off the edge of a small, moving deck.
        let carrier_marker = self.find_carrier_cargo_spawn_point(lua, point, side)?;
        let (spawn_base, spawn_base_offset) = match &carrier_marker {
            Some((marker_pos, _)) => (*marker_pos, 0.0),
            None => (point, spawn_distance),
        };

        // Scan for a spot clear of any existing crate, from any player, so two
        // players dropping cargo near the same spot don't compute overlapping
        // spawn points and destroy each other's crates
        let spawn_point = self
            .find_crate_spawn_point(spawn_base, dir, spawn_base_offset)?
            .ok_or_else(|| anyhow!("no clear space to spawn crate, move away from other crates"))?;

        // Pick template: helo dynamic cargo uses helo_cargo_template (fallback to c130_cargo_template)
        let template = if !auto_unpack {
            self.ephemeral.cfg.helo_cargo_template
                .get(&side)
                .or_else(|| self.ephemeral.cfg.c130_cargo_template.get(&side))
                .ok_or_else(|| anyhow!("missing helo_cargo_template or c130_cargo_template for {:?}", side))?
                .clone()
        } else {
            self.ephemeral.cfg.c130_cargo_template
                .get(&side)
                .ok_or_else(|| anyhow!("missing c130_cargo_template for {:?}", side))?
                .clone()
        };

        let spawnpos = SpawnLoc::AtPosExact {
            pos: spawn_point,
            group_heading: azumith2d(dir),
        };

        let dk = DeployKind::Crate {
            origin,
            player: ucid.clone(),
            spec: crate_def.clone(),
        };

        debug!("[C130_CARGO] Spawning with template='{}', dir=({:.2}, {:.2})",
               template, dir.x, dir.y);

        // Check if player is on a carrier - if so, link the crate to the carrier unit
        let carrier_link_id = match carrier_marker {
            Some((_, link_name)) => Some(link_name),
            None => self.find_carrier_unit_at_position(lua, point, side)?,
        };
        if carrier_link_id.is_some() {
            debug!("[C130_CARGO] Player is on carrier, will link crate to carrier unit");
        }

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

        // Register carrier link if spawning on a carrier
        if let Some(link_id) = carrier_link_id {
            debug!("[C130_CARGO] Registering carrier link for group {:?} to unit {}", group_id, link_id);
            self.ephemeral.carrier_linked_groups.insert(group_id, link_id);
        }

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
            auto_unpack,
        );

        self.ephemeral.c130_crates.insert(group_name.clone(), c130_cargo);
        debug!("[C130_CARGO] Crate tracking added: name='{}', group_id={:?}, total_tracked={}",
            group_name, group_id, self.ephemeral.c130_crates.len());

        Ok(String::from(format!("Spawned {} crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it.", crate_name)))
    }

    /// Spawn a vehicle as physical cargo near the player's aircraft
    /// Vehicles can be loaded using DCS's F8 Ground Crew cargo menu
    pub fn spawn_c130_vehicle(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        slot: &SlotId,
        vehicle_name: String,
        side: Side,
        origin: ObjectiveId,
    ) -> Result<String> {
        debug!("[C130_CARGO] spawn_c130_vehicle called: vehicle={}, side={:?}, origin={:?}, slot={:?}",
            vehicle_name, side, origin, slot);

        // Get player UCID
        let ucid = maybe!(self.ephemeral.players_by_slot, *slot, "no such player")?.clone();

        // Get vehicle configuration from c130_cargo.loadable_vehicles
        let vehicle_cfg = self.ephemeral.cfg.c130_cargo
            .as_ref()
            .ok_or_else(|| anyhow!("C-130 cargo not configured"))?
            .loadable_vehicles
            .get(&side)
            .ok_or_else(|| anyhow!("No loadable vehicles configured for {:?}", side))?
            .iter()
            .find(|v| v.name == vehicle_name)
            .ok_or_else(|| anyhow!("Vehicle {} not found in loadable vehicles", vehicle_name))?
            .clone();

        debug!("[C130_CARGO] Found vehicle config: {}, template={}, weight={}kg",
            vehicle_cfg.name, vehicle_cfg.template, vehicle_cfg.weight);

        // Check points cost if enabled
        if self.ephemeral.cfg.points.is_some() && vehicle_cfg.cost > 0 {
            if let Some(player) = self.persisted.players.get(&ucid) {
                if let Some(obj) = self.persisted.objectives.get(&origin) {
                    let points = max(0, player.points) + obj.points;
                    if points < vehicle_cfg.cost as i32 {
                        bail!("Insufficient points. Need {} points, have {}", vehicle_cfg.cost, points);
                    }
                }
            }
        }

        // Get player position and direction (same method as regular cargo system)
        let unit = self.ephemeral.slot_instance_unit(lua, slot)?;
        let pos = unit.get_position()?;
        let point = Vector2::new(pos.p.x, pos.p.z);
        let dir = Vector2::new(pos.x.x, pos.x.z);
        debug!("[C130_CARGO] Player position: x={:.2}, z={:.2}, dir=({:.2}, {:.2})",
               point.x, point.y, dir.x, dir.y);

        // Spawn the vehicle using C-130 cargo template (static cargo object for DCS loading)
        let template = self
            .ephemeral
            .cfg
            .c130_cargo_template
            .get(&side)
            .ok_or_else(|| anyhow!("missing c130_cargo_template for {:?}", side))?
            .clone();

        let spawn_distance = self.ephemeral.cfg.c130_cargo.as_ref()
            .and_then(|c| c.spawn_distance)
            .unwrap_or(-45.0);

        // If the player is on a carrier with a configured cargo spawn point
        // marker, anchor the grid scan on the marker's live position instead
        // of a player-relative offset, which can easily land off a small,
        // moving deck.
        let carrier_marker = self.find_carrier_cargo_spawn_point(lua, point, side)?;
        let (spawn_base, spawn_base_offset) = match &carrier_marker {
            Some((marker_pos, _)) => (*marker_pos, 0.0),
            None => (point, spawn_distance),
        };

        // Scan for a spot clear of any existing crate, from any player, so
        // two players dropping cargo near the same spot don't spawn on top
        // of each other and destroy each other's cargo
        let spawn_point = self
            .find_crate_spawn_point(spawn_base, dir, spawn_base_offset)?
            .ok_or_else(|| anyhow!("no clear space to spawn vehicle cargo, move away from other crates"))?;

        let spawnpos = SpawnLoc::AtPosExact {
            pos: spawn_point,
            group_heading: azumith2d(dir),
        };

        // Create a dummy crate for tracking (weight matches vehicle)
        let dummy_crate = Crate {
            name: vehicle_cfg.name.clone().into(),
            weight: vehicle_cfg.weight,
            required: 1,
            pos_unit: None,
            max_drop_height_agl: 1000,
            max_drop_speed: 150,
        };

        let dk = DeployKind::Crate {
            origin,
            player: ucid.clone(),
            spec: dummy_crate.clone(),
        };

        debug!("[C130_CARGO] Spawning vehicle cargo with template='{}'", template);

        // Check if player is on a carrier - if so, link the cargo to the carrier unit
        let carrier_link_id = match carrier_marker {
            Some((_, link_name)) => Some(link_name),
            None => self.find_carrier_unit_at_position(lua, point, side)?,
        };
        if carrier_link_id.is_some() {
            debug!("[C130_CARGO] Player is on carrier, will link vehicle cargo to carrier unit");
        }

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

        // Register carrier link if spawning on a carrier
        if let Some(link_id) = carrier_link_id {
            debug!("[C130_CARGO] Registering carrier link for vehicle group {:?} to unit {}", group_id, link_id);
            self.ephemeral.carrier_linked_groups.insert(group_id, link_id);
        }

        debug!("[C130_CARGO] Vehicle cargo spawned successfully: group_id={:?}", group_id);

        // Get the group name for tracking
        let group_name = match self.persisted.groups.get(&group_id) {
            Some(g) => g.name.clone(),
            None => {
                error!("[C130_CARGO] Failed to get group name for {:?}", group_id);
                bail!("Failed to get group name for spawned vehicle cargo")
            }
        };

        // Create C130Cargo tracking entry with Vehicle type
        let crate_type = C130CargoType::Vehicle {
            name: vehicle_cfg.name.clone(),
            template: vehicle_cfg.template.clone(),
        };

        let c130_cargo = C130Cargo::new_vehicle(
            group_name.clone(),
            group_id,
            crate_type,
            origin,
            ucid.clone(),
            side,
            point,
            dummy_crate,
            vehicle_cfg.clone(),
        );

        self.ephemeral.c130_crates.insert(group_name.clone(), c130_cargo);
        debug!("[C130_CARGO] Vehicle tracking added: name='{}', group_id={:?}, total_tracked={}",
            group_name, group_id, self.ephemeral.c130_crates.len());

        // Charge points if enabled
        if self.ephemeral.cfg.points.is_some() && vehicle_cfg.cost > 0 {
            self.charge_for_item(
                &ucid,
                origin,
                vehicle_cfg.cost,
                &format_compact!("for {} vehicle", vehicle_cfg.name),
            );
        }

        Ok(String::from(format!(
            "Spawned {} vehicle cargo. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it.",
            vehicle_cfg.name
        )))
    }

    /// Queue multiple crates for staggered spawning (used by "Spawn All" command)
    pub fn queue_c130_crate_spawns(
        &mut self,
        lua: MizLua,
        slot: &SlotId,
        crate_list: Vec<(String, Crate)>,
        side: Side,
        origin: ObjectiveId,
        auto_unpack: bool,
    ) -> Result<String> {
        let ucid = maybe!(self.ephemeral.players_by_slot, *slot, "no such player")?.clone();

        let (spawn_delay, max_spawn) = if auto_unpack {
            let delay = self.ephemeral.cfg.c130_cargo.as_ref().map(|c| c.spawn_delay).unwrap_or(1);
            let max = self.ephemeral.cfg.c130_cargo.as_ref().map(|c| c.max_spawn_all as usize).unwrap_or(50);
            (delay, max)
        } else {
            let delay = self.ephemeral.cfg.helo_cargo.as_ref().map(|c| c.spawn_delay).unwrap_or(1);
            let max = self.ephemeral.cfg.helo_cargo.as_ref().map(|c| c.max_spawn_all as usize).unwrap_or(50);
            (delay, max)
        };

        // Resolve the grid anchor and carrier link ONCE for the whole batch,
        // not per crate per tick. A stationary player still gets identical
        // behavior to before; a player parked on a moving carrier deck no
        // longer has the reference point drag out from under already-spawned
        // crates as the ship moves during the staggered spawn, which used to
        // make every crate after the first collapse back onto the same grid
        // cell instead of spreading out.
        let unit = self.ephemeral.slot_instance_unit(lua, slot)?;
        let pos = unit.get_position()?;
        let point = Vector2::new(pos.p.x, pos.p.z);
        let dir = Vector2::new(pos.x.x, pos.x.z);
        let unit_typ = unit.get_type_name()?;
        let cargo_cfg = self.ephemeral.cfg.cargo.get(&Vehicle(unit_typ));
        let spawn_distance = cargo_cfg
            .and_then(|cc| cc.spawn_distance)
            .unwrap_or_else(|| {
                if auto_unpack {
                    self.ephemeral.cfg.c130_cargo.as_ref()
                        .and_then(|c| c.spawn_distance)
                        .unwrap_or(-45.0)
                } else {
                    self.ephemeral.cfg.helo_cargo.as_ref()
                        .and_then(|c| c.spawn_distance)
                        .unwrap_or(4.0)
                }
            });
        let carrier_marker = self.find_carrier_cargo_spawn_point(lua, point, side)?;
        let (anchor_point, anchor_offset) = match &carrier_marker {
            Some((marker_pos, _)) => (*marker_pos, 0.0),
            None => (point, spawn_distance),
        };
        let carrier_link_id = match carrier_marker {
            Some((_, link_name)) => Some(link_name),
            None => self.find_carrier_unit_at_position(lua, point, side)?,
        };

        let num_to_spawn = crate_list.len().min(max_spawn);
        let mut spawn_time = Utc::now();

        for (idx, (crate_name, crate_def)) in crate_list.into_iter().take(num_to_spawn).enumerate() {
            spawn_time = spawn_time + chrono::Duration::seconds(spawn_delay as i64);

            self.ephemeral
                .c130_spawn_queue
                .entry(spawn_time)
                .or_insert_with(Vec::new)
                .push((
                    side,
                    crate_name,
                    origin,
                    ucid.clone(),
                    crate_def,
                    idx,
                    auto_unpack,
                    anchor_point,
                    dir,
                    anchor_offset,
                    carrier_link_id.clone(),
                ));
        }

        Ok(String::from(format!(
            "Queued {} crates for spawning ({} second intervals). They will appear behind your aircraft.",
            num_to_spawn, spawn_delay
        )))
    }

    /// Process the spawn queue (called from slow_timed_events). Operates on
    /// the single shared c130_spawn_queue for all players -- no per-player
    /// parameter needed, since each queued crate already carries its own
    /// frozen spawn anchor from when it was queued.
    pub fn process_c130_spawn_queue(&mut self, lua: MizLua, idx: &MizIndex) -> Result<()> {
        let now = Utc::now();
        let to_spawn: Vec<_> = self.ephemeral
            .c130_spawn_queue
            .range(..=now)
            .map(|(k, _)| *k)
            .collect();

        for spawn_time in to_spawn {
            if let Some(crates) = self.ephemeral.c130_spawn_queue.remove(&spawn_time) {
                for (side, crate_name, origin, ucid, crate_def, _crate_idx, auto_unpack, anchor_point, dir, anchor_offset, carrier_link_id) in crates {
                    // anchor_point/dir/anchor_offset were resolved once for
                    // the whole batch when it was queued (see
                    // queue_c130_crate_spawns) -- deliberately NOT re-reading
                    // the player's live position here, so a moving carrier
                    // deck can't drag the reference point out from under
                    // crates already spawned earlier in this same batch.
                    // The grid scan itself still runs fresh per crate, since
                    // it needs to see whatever this batch has already placed.
                    let point = match self.find_crate_spawn_point(anchor_point, dir, anchor_offset) {
                        Ok(Some(p)) => p,
                        Ok(None) | Err(_) => continue, // no clear space, skip this crate
                    };

                    let template = if !auto_unpack {
                        self.ephemeral.cfg.helo_cargo_template
                            .get(&side)
                            .or_else(|| self.ephemeral.cfg.c130_cargo_template.get(&side))
                            .cloned()
                    } else {
                        self.ephemeral.cfg.c130_cargo_template.get(&side).cloned()
                    };
                    let template = match template {
                        Some(name) => name,
                        None => continue,
                    };

                    let crate_type = if crate_name.contains("Fuel Transfer") {
                        C130CargoType::SupplyTransferFuel
                    } else if crate_name.contains("Weapons Transfer") {
                        C130CargoType::SupplyTransferWeapons
                    } else if crate_name.contains("Carrier Repair") {
                        C130CargoType::CarrierRepair
                    } else {
                        C130CargoType::Deployable { name: crate_name.clone() }
                    };

                    let spawnpos = SpawnLoc::AtPosExact {
                        pos: point,
                        group_heading: azumith2d(dir),
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
                            if let Some(link_id) = carrier_link_id {
                                self.ephemeral.carrier_linked_groups.insert(group_id, link_id);
                            }

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
                                auto_unpack,
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
                    // Airdrop detection: crate moving very fast means it was dropped from fixed-wing
                    if speed > 50.0 {
                        info!("[C130_CARGO] Crate '{}' transitioned to Airborne (speed={:.2}m/s)", crate_name, speed);
                        crate_data.state = C130CargoState::Airborne;
                        crate_data.airborne_time = Some(Utc::now());
                    } else if !crate_data.auto_unpack && speed < 1.0 {
                        // Slingload delivery detection: helo crates don't go airborne independently —
                        // when a CH-47 slingloads a crate and releases it the static object is just
                        // placed at the new position at near-zero speed. Detect this by checking if
                        // the crate has moved more than 100 m from where it was spawned.
                        let dist = na::distance(&new_pos.into(), &crate_data.spawn_pos.into());
                        if dist > 100.0 {
                            info!("[C130_CARGO] Crate '{}' slingload-delivered (moved {:.0}m from spawn, speed={:.2}m/s) - manual unpack required",
                                crate_name, dist, speed);
                            crate_data.state = C130CargoState::Landed;
                        }
                    }
                }
                C130CargoState::Airborne => {
                    // If crate has landed (low speed) and been airborne for at least 3 seconds
                    if speed < 1.0 {
                        if let Some(airborne_time) = crate_data.airborne_time {
                            let airborne_duration = Utc::now().signed_duration_since(airborne_time);
                            if airborne_duration.num_seconds() >= 3 {
                                crate_data.state = C130CargoState::Landed;
                                if crate_data.auto_unpack {
                                    info!("[C130_CARGO] Crate '{}' transitioned to Landed (speed={:.2}m/s) - queuing for auto-unpack", crate_name, speed);
                                    to_unpack.push(crate_name.clone());
                                } else {
                                    info!("[C130_CARGO] Crate '{}' transitioned to Landed (speed={:.2}m/s) - manual unpack required", crate_name, speed);
                                }
                            }
                        }
                    }
                }
                C130CargoState::Landed => {
                    // Retry auto-unpack each tick — a previous attempt may have failed
                    // because sibling crates hadn't landed yet (parachute drift spreads
                    // landing times across multiple ticks). Unpack is idempotent on failure.
                    if crate_data.auto_unpack {
                        to_unpack.push(crate_name.clone());
                    }
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
                        // If the crate is still tracked, unpack didn't consume it -- it's
                        // still waiting on missing sibling crates. Auto-unpack keeps
                        // retrying every tick (a sibling may land late from parachute
                        // drift), but the "need more crates" message would otherwise be
                        // re-sent every tick forever if a sibling never lands (e.g. it
                        // was destroyed when the delivering aircraft was shot down).
                        // Send it at most once per crate.
                        let still_waiting = self.ephemeral.c130_crates.contains_key(&crate_name);
                        if !still_waiting {
                            self.ephemeral.msgs().panel_to_side(10, false, crate_data.side, msg);
                        } else if !crate_data.notified_missing {
                            if let Some(c) = self.ephemeral.c130_crates.get_mut(&crate_name) {
                                c.notified_missing = true;
                            }
                            self.ephemeral.msgs().panel_to_side(10, false, crate_data.side, msg);
                        }
                    }
                    Err(e) => {
                        error!("[C130_CARGO] Failed to auto-unpack physical crate '{}': {}", crate_name, e);
                    }
                }
            } else {
                debug!("[C130_CARGO] Crate '{}' already consumed by a previous deployment in this batch, skipping", crate_name);
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
                                    if dist < 500.0 {
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
                            jtac: None,
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
                                let mut crates_to_delete: Vec<String> = Vec::new();
                                for req in &deployable.crates {
                                    if let Some(crate_names) = nearby_crates.get(&req.name) {
                                        info!("[C130_CARGO] Crate type '{}': need {}, found {} nearby: {:?}",
                                              req.name, req.required, crate_names.len(), crate_names);
                                        for (i, cn) in crate_names.iter().enumerate() {
                                            if i < req.required as usize {
                                                crates_to_delete.push(cn.clone());
                                            }
                                        }
                                    }
                                }

                                // Ensure the trigger crate itself is also in the deletion list
                                // (it should already be, but this guarantees it in edge cases)
                                if !crates_to_delete.iter().any(|n| n.as_str() == crate_name) {
                                    info!("[C130_CARGO] Adding trigger crate '{}' to deletion list (was not in nearby_crates)", crate_name);
                                    crates_to_delete.push(String::from(crate_name));
                                }

                                info!("[C130_CARGO] Deleting {} crates for deployment: {:?}", crates_to_delete.len(), crates_to_delete);

                                for cn in &crates_to_delete {
                                    if let Some(crate_to_delete) = self.ephemeral.c130_crates.remove(cn) {
                                        info!("[C130_CARGO] Removing crate '{}' (group_id={:?}) from tracking and despawning",
                                              cn, crate_to_delete.group_id);
                                        if let Err(e) = self.delete_group(&crate_to_delete.group_id) {
                                            error!("[C130_CARGO] Failed to delete crate group '{}' (group_id={:?}): {:?}",
                                                   cn, crate_to_delete.group_id, e);
                                        }
                                    } else {
                                        debug!("[C130_CARGO] Crate '{}' already removed from tracking (likely processed by earlier crate in batch)", cn);
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
            C130CargoType::SupplyTransferFuel => {
                // Handle fuel-only transfer
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
                    let transfer_amount = self.ephemeral.cfg.warehouse.as_ref()
                        .ok_or_else(|| anyhow!("Warehouse not configured"))?
                        .supply_transfer_size;

                    let source_warehouse = self.persisted.objectives.get(&crate_data.origin)
                        .map(|obj| obj.warehouse.clone());

                    let (obj_mut, wh) = self.sync_warehouse_to_objective(lua, oid)
                        .context("syncing warehouse for fuel transfer")?;

                    let mut added_items = Vec::new();

                    // ONLY add liquids (fuel)
                    for (liq_type, inv) in obj_mut.warehouse.liquids.iter_mut_cow() {
                        if inv.capacity == 0 {
                            if let Some(ref src_wh) = source_warehouse {
                                if let Some(source_inv) = src_wh.liquids.get(liq_type) {
                                    if source_inv.capacity > 0 {
                                        inv.capacity = source_inv.capacity;
                                        let amount = ((inv.capacity as f32 * (transfer_amount as f32 / 100.0)) as u32).max(1);
                                        inv.stored = amount;
                                        added_items.push(format!("{:?}: +{}", liq_type, amount));
                                        info!("[FUEL_TRANSFER] Initialized {:?} with capacity {}, added {}", liq_type, inv.capacity, amount);
                                    }
                                }
                            }
                        } else if inv.capacity > inv.stored {
                            let available_space = inv.capacity - inv.stored;
                            let amount = ((inv.capacity as f32 * (transfer_amount as f32 / 100.0)) as u32).max(1);
                            let to_add = amount.min(available_space);

                            if to_add > 0 {
                                inv.stored += to_add;
                                added_items.push(format!("{:?}: +{}", liq_type, to_add));
                                info!("[FUEL_TRANSFER] Added {:?} x{} to {:?}", liq_type, to_add, oid);
                            }
                        }
                    }

                    use crate::db::logistics::sync_obj_to_warehouse;
                    sync_obj_to_warehouse(&obj_mut, &wh)?;
                    self.ephemeral.dirty();
                    self.ephemeral.c130_crates.remove(crate_name);
                    self.delete_group(&crate_data.group_id)?;

                    let obj_name = self.persisted.objectives.get(&oid)
                        .map(|o| o.name.clone())
                        .unwrap_or_else(|| String::from("Unknown"));

                    let msg = if added_items.is_empty() {
                        String::from(format!("Fuel transfer crate delivered to {} (tanks full)", obj_name))
                    } else {
                        String::from(format!("Fuel transfer crate delivered to {}", obj_name))
                    };

                    info!("[FUEL_TRANSFER] {}", msg);
                    Ok(msg)
                } else {
                    bail!("No friendly objectives found for fuel transfer")
                }
            }
            C130CargoType::SupplyTransferWeapons => {
                // Handle weapons/equipment-only transfer
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
                    let (transfer_amount, exempt_airframes) = {
                        let whcfg = self.ephemeral.cfg.warehouse.as_ref()
                            .ok_or_else(|| anyhow!("Warehouse not configured"))?;
                        (whcfg.supply_transfer_size, whcfg.exempt_airframes.clone())
                    };

                    let source_warehouse = self.persisted.objectives.get(&crate_data.origin)
                        .map(|obj| obj.warehouse.clone());

                    let (obj_mut, wh) = self.sync_warehouse_to_objective(lua, oid)
                        .context("syncing warehouse for weapons transfer")?;

                    let mut added_items = Vec::new();

                    // ONLY add equipment (non-exempt items, no airframes)
                    for (name, inv) in obj_mut.warehouse.equipment.iter_mut_cow() {
                        let is_airframe = !name.starts_with("weapons.")
                            && !name.starts_with("vehicles.")
                            && !name.starts_with("Fortifications.");

                        if is_airframe || exempt_airframes.contains(name.as_str()) {
                            continue;
                        }

                        if inv.capacity == 0 {
                            if let Some(ref src_wh) = source_warehouse {
                                if let Some(source_inv) = src_wh.equipment.get(name) {
                                    if source_inv.capacity > 0 {
                                        inv.capacity = source_inv.capacity;
                                        let amount = ((inv.capacity as f32 * (transfer_amount as f32 / 100.0)) as u32).max(1);
                                        inv.stored = amount;
                                        added_items.push(format!("{}: +{}", name, amount));
                                        info!("[WEAPONS_TRANSFER] Initialized {} with capacity {}, added {}", name, inv.capacity, amount);
                                    }
                                }
                            }
                        } else if inv.capacity > inv.stored {
                            let available_space = inv.capacity - inv.stored;
                            let amount = ((inv.capacity as f32 * (transfer_amount as f32 / 100.0)) as u32).max(1);
                            let to_add = amount.min(available_space);

                            if to_add > 0 {
                                inv.stored += to_add;
                                added_items.push(format!("{}: +{}", name, to_add));
                                info!("[WEAPONS_TRANSFER] Added {} x{} to {:?}", name, to_add, oid);
                            }
                        }
                    }

                    use crate::db::logistics::sync_obj_to_warehouse;
                    sync_obj_to_warehouse(&obj_mut, &wh)?;
                    self.ephemeral.dirty();
                    self.ephemeral.c130_crates.remove(crate_name);
                    self.delete_group(&crate_data.group_id)?;

                    let obj_name = self.persisted.objectives.get(&oid)
                        .map(|o| o.name.clone())
                        .unwrap_or_else(|| String::from("Unknown"));

                    let msg = if added_items.is_empty() {
                        String::from(format!("Weapons transfer crate delivered to {} (warehouse full)", obj_name))
                    } else {
                        String::from(format!("Weapons transfer crate delivered to {}", obj_name))
                    };

                    info!("[WEAPONS_TRANSFER] {}", msg);
                    Ok(msg)
                } else {
                    bail!("No friendly objectives found for weapons transfer")
                }
            }
            C130CargoType::CarrierRepair => {
                use bfprotocols::db::objective::ObjectiveKind;

                // Find nearest carrier group
                let carriers: Vec<(ObjectiveId, Vector2)> = self.persisted
                    .objectives
                    .into_iter()
                    .filter_map(|(oid, obj)| {
                        if obj.owner == crate_data.side {
                            if let ObjectiveKind::CarrierGroup { .. } = obj.kind {
                                Some((*oid, obj.zone.pos()))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect();

                let nearest_carrier = carriers
                    .iter()
                    .min_by_key(|(_, pos)| {
                        let dist = na::distance(&(*pos).into(), &crate_data.last_pos.into());
                        (dist * 1000.0) as i64
                    })
                    .map(|(oid, _)| *oid);

                if let Some(carrier_id) = nearest_carrier {
                    use chrono::Utc;

                    // Get carrier name before mutation
                    let carrier_name = self.persisted.objectives.get(&carrier_id)
                        .map(|o| o.name.clone())
                        .unwrap_or_else(|| String::from("Unknown"));

                    let now = Utc::now();
                    let repair_time_secs = self.ephemeral.cfg.carrier
                        .as_ref()
                        .map(|c| c.repair_time)
                        .unwrap_or(600);

                    // Start the repair process
                    if let Some(carrier_obj) = self.persisted.objectives.get_mut_cow(&carrier_id) {
                        if let ObjectiveKind::CarrierGroup { repair_start_time, .. } = &mut carrier_obj.kind {
                            *repair_start_time = Some(now);
                            info!("[CARRIER_REPAIR] Started repairing {} - will complete in {} seconds ({} minutes)",
                                  carrier_name, repair_time_secs, repair_time_secs / 60);
                        }
                    } else {
                        bail!("Carrier objective not found")
                    }

                    // Mark database as changed
                    self.ephemeral.dirty();

                    // Remove from tracking map
                    self.ephemeral.c130_crates.remove(crate_name);

                    // Delete the physical crate
                    self.delete_group(&crate_data.group_id)?;

                    let msg = String::from(format!(
                        "Carrier repair crate delivered to {} - repair in progress ({}m)",
                        carrier_name,
                        repair_time_secs / 60
                    ));
                    info!("[CARRIER_REPAIR] {}", msg);
                    Ok(msg)
                } else {
                    bail!("No friendly carrier groups found for repair")
                }
            }
            C130CargoType::Vehicle { name, template } => {
                info!("[C130_CARGO] Processing landed vehicle cargo: {} (template: {})", name, template);

                // Get vehicle config from the cargo data
                let vehicle_cfg = crate_data.vehicle_def.clone()
                    .ok_or_else(|| anyhow!("Vehicle config not found for {}", name))?;

                // Spawn location for the vehicle
                let spawnpos = SpawnLoc::AtPos {
                    pos: crate_data.last_pos,
                    offset_direction: Vector2::new(0., 0.),
                    group_heading: 0.,
                };

                // Create a synthetic deployable for tracking
                let synthetic_deployable = Deployable {
                    path: vehicle_cfg.path.clone(),
                    kind: DeployableKind::Group { template: template.clone() },
                    persist: bfprotocols::cfg::PersistTyp::Forever,
                    limit: vehicle_cfg.limit,
                    limit_enforce: vehicle_cfg.limit_enforce.clone(),
                    crates: vec![],
                    repair_crate: None,
                    repair_cost: 0,
                    cost: vehicle_cfg.cost,
                    jtac: None,
                    ewr: None,
                    deprecated_template: None,
                    deprecated_logistics: None,
                };

                let dk = DeployKind::Deployed {
                    player: crate_data.player,
                    moved_by: None,
                    spec: synthetic_deployable,
                    cost_fraction: 1.0,
                    origin: Some(crate_data.origin),
                    jtac: None,
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
                    Ok(gid) => {
                        info!("[C130_CARGO] Vehicle {} spawned successfully as group {:?}", name, gid);

                        // Remove the cargo tracking entry
                        self.ephemeral.c130_crates.remove(crate_name);

                        // Delete the physical cargo crate
                        if let Err(e) = self.delete_group(&crate_data.group_id) {
                            error!("[C130_CARGO] Failed to delete cargo crate for vehicle '{}': {:?}", name, e);
                        }

                        Ok(String::from(format!("Vehicle {} airdropped and deployed", name)))
                    }
                    Err(e) => {
                        error!("[C130_CARGO] Failed to spawn vehicle '{}' from template '{}': {:?}", name, template, e);
                        Err(anyhow!("Failed to spawn vehicle: {:?}", e))
                    }
                }
            }
        }
    }

    // ===== Dismount System =====

    /// Spawn an infantry dismount group at a destroyed vehicle's position.
    /// Returns Ok(()) silently if the vehicle type has no dismount config,
    /// or if the side has no template configured.
    pub fn spawn_dismount_group(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        vehicle_typ: &Vehicle,
        side: Side,
        pos: Vector2,
        heading: f64,
        from_group: GroupId,
    ) -> Result<()> {
        let spec: DismountSpec = match self.ephemeral.cfg.dismount.get(vehicle_typ) {
            None => return Ok(()),
            Some(s) => s.clone(),
        };
        if spec.max_concurrent > 0
            && self.persisted.dismounts.len() as u32 >= spec.max_concurrent
        {
            return Ok(());
        }
        let template = match spec.template.get(&side) {
            None => return Ok(()),
            Some(t) => t.clone(),
        };
        let spawnpos = SpawnLoc::AtPos {
            pos,
            offset_direction: Vector2::new(heading.sin(), heading.cos()),
            group_heading: heading,
        };
        let dk = DeployKind::Dismount {
            from_group,
            can_capture: spec.can_capture,
        };
        let spctx = SpawnCtx::new(lua)?;
        self.add_and_queue_group(
            &spctx,
            idx,
            side,
            spawnpos,
            &*template,
            dk,
            BitFlags::empty(),
            None,
        )?;
        Ok(())
    }
}
