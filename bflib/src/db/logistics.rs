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

use super::{
    ephemeral::{Equipment, Production},
    objective::Objective,
    persisted::Persisted,
    Db, Map, MapS, SetS,
};
use crate::{admin::WarehouseKind, maybe, objective, objective_mut, group, Task};
use anyhow::{anyhow, bail, Context, Result};
use bfprotocols::{
    cfg::Vehicle,
    db::objective::{ObjectiveId, ObjectiveKind},
    perf::{Perf, PerfInner},
    stats::Stat,
};
use chrono::{prelude::*, Duration};
use compact_str::{format_compact, CompactString};
use dcso3::{
    airbase::Airbase,
    coalition::Side,
    object::DcsObject,
    perf::record_perf,
    warehouse::{self, LiquidType},
    world::World,
    MizLua, String, Vector2,
};
use fxhash::FxHashMap;
use log::{debug, error, info, warn};
use serde_derive::{Deserialize, Serialize};
use smallvec::{smallvec, SmallVec};
use std::{
    cmp::{max, min},
    collections::hash_map::Entry,
    mem,
    ops::{AddAssign, SubAssign},
    sync::Arc,
};
use tokio::sync::mpsc::UnboundedSender;

#[derive(Debug, Clone)]
pub enum LogiStage {
    Complete {
        last_tick: DateTime<Utc>,
    },
    SyncFromWarehouses {
        objectives: SmallVec<[ObjectiveId; 128]>,
    },
    SyncToWarehouses {
        objectives: SmallVec<[ObjectiveId; 128]>,
    },
    ExecuteTransfers {
        transfers: Vec<Transfer>,
    },
    ManageConvoys,
    ManageAirRoutes,
    ManageSeaRoutes,
    Init,
}

impl Default for LogiStage {
    fn default() -> Self {
        Self::Init
    }
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct Inventory {
    pub stored: u32,
    pub capacity: u32,
}

impl Inventory {
    pub fn percent(&self) -> Option<u8> {
        if self.capacity == 0 {
            None
        } else {
            let stored: f32 = self.stored as f32;
            let capacity: f32 = self.capacity as f32;
            Some(min(100, ((stored / capacity) * 100.) as u32) as u8)
        }
    }

    pub fn reduce(&mut self, percent: f32) -> u32 {
        if self.stored == 0 {
            0
        } else {
            let taken = max(1, (self.stored as f32 * percent) as u32);
            self.stored -= taken;
            taken
        }
    }
}

impl AddAssign<u32> for Inventory {
    fn add_assign(&mut self, rhs: u32) {
        let qty = self.stored + rhs;
        if qty > self.capacity {
            self.stored = self.capacity
        } else {
            self.stored = qty
        }
    }
}

impl SubAssign<u32> for Inventory {
    fn sub_assign(&mut self, rhs: u32) {
        if rhs > self.stored {
            self.stored = 0
        } else {
            self.stored = self.stored - rhs;
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum TransferItem {
    Equipment(String),
    Liquid(LiquidType),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transfer {
    source: ObjectiveId,
    target: ObjectiveId,
    amount: u32,
    item: TransferItem,
}

impl Transfer {
    fn execute(&self, db: &mut Persisted, to_bg: &Option<UnboundedSender<Task>>) -> Result<()> {
        // Get source capacity for initializing destination if needed
        let src_capacity = match &self.item {
            TransferItem::Equipment(name) => {
                db.objectives.get(&self.source)
                    .and_then(|src| src.warehouse.equipment.get(name))
                    .map(|inv| inv.capacity)
            }
            TransferItem::Liquid(name) => {
                db.objectives.get(&self.source)
                    .and_then(|src| src.warehouse.liquids.get(name))
                    .map(|inv| inv.capacity)
            }
        };

        let src = db
            .objectives
            .get_mut_cow(&self.source)
            .ok_or_else(|| anyhow!("no such objective {:?}", self.source))?;
        match &self.item {
            TransferItem::Equipment(name) => {
                let d = &mut src.warehouse.equipment[name].stored;
                *d -= self.amount;
                if let Some(to_bg) = to_bg.as_ref() {
                    let _ = to_bg.send(Task::Stat(Stat::EquipmentInventory {
                        id: src.id,
                        item: name.clone(),
                        amount: *d,
                    }));
                }
            }
            TransferItem::Liquid(name) => {
                let d = &mut src.warehouse.liquids[name].stored;
                *d -= self.amount;
                if let Some(to_bg) = to_bg.as_ref() {
                    let _ = to_bg.send(Task::Stat(Stat::LiquidInventory {
                        id: src.id,
                        item: *name,
                        amount: *d,
                    }));
                }
            }
        }
        let dst = db
            .objectives
            .get_mut_cow(&self.target)
            .ok_or_else(|| anyhow!("no such objective {:?}", self.target))?;
        match &self.item {
            TransferItem::Equipment(name) => {
                let inv = dst
                    .warehouse
                    .equipment
                    .get_or_default_cow(name.clone());
                // If destination has 0 capacity, initialize from source
                if inv.capacity == 0 {
                    if let Some(cap) = src_capacity {
                        inv.capacity = cap;
                    }
                }
                inv.stored += self.amount;
                if let Some(to_bg) = to_bg.as_ref() {
                    let _ = to_bg.send(Task::Stat(Stat::EquipmentInventory {
                        id: dst.id,
                        item: name.clone(),
                        amount: inv.stored,
                    }));
                }
            }
            TransferItem::Liquid(name) => {
                let inv = dst.warehouse.liquids.get_or_default_cow(*name);
                // If destination has 0 capacity, initialize from source
                if inv.capacity == 0 {
                    if let Some(cap) = src_capacity {
                        inv.capacity = cap;
                    }
                }
                inv.stored += self.amount;
                if let Some(to_bg) = to_bg.as_ref() {
                    let _ = to_bg.send(Task::Stat(Stat::LiquidInventory {
                        id: dst.id,
                        item: *name,
                        amount: inv.stored,
                    }));
                }
            }
        }
        Ok(())
    }
}

// ============================================================================
// CONVOY SYSTEM
// ============================================================================

/// Unique convoy identifier
pub type ConvoyId = CompactString;

/// What type of supplies the convoy carries
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConvoyCargoType {
    Fuel,
    Weapons,
    /// Auto-dispatched convoy carrying a mix of whatever the hub has available.
    Mixed,
}

impl ConvoyCargoType {
    pub fn as_str(&self) -> &'static str {
        match self {
            ConvoyCargoType::Fuel => "fuel",
            ConvoyCargoType::Weapons => "weapons",
            ConvoyCargoType::Mixed => "mixed supplies",
        }
    }
}

/// Current state of a supply convoy
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConvoyState {
    /// Convoy is in transit to destination
    InTransit,
    /// Convoy successfully reached destination and delivered supplies
    Delivered,
    /// Convoy was destroyed en route, supplies lost
    Destroyed,
}

/// A supply convoy transporting goods between objectives
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplyConvoy {
    /// Unique convoy identifier
    pub id: ConvoyId,
    /// DCS group ID for the truck group
    pub group_id: bfprotocols::db::group::GroupId,
    /// Source logistics hub
    pub origin: ObjectiveId,
    /// Destination objective
    pub destination: ObjectiveId,
    /// What supplies are being transported
    pub cargo_type: ConvoyCargoType,
    /// The actual transfers this convoy will execute (can be multiple items)
    pub transfers: Vec<Transfer>,
    /// When convoy spawned
    pub spawn_time: DateTime<Utc>,
    /// Current state
    pub state: ConvoyState,
    /// Side
    pub side: Side,
    /// Last known position (for tracking)
    pub last_pos: Vector2,
    /// When we last checked the convoy status
    pub last_check: DateTime<Utc>,
}

impl SupplyConvoy {
    /// Check if convoy is still alive by checking if group exists in DCS
    pub fn check_status(&mut self, lua: MizLua, group_name: &str) -> ConvoyState {
        use dcso3::group::Group;

        match Group::get_by_name(lua, group_name) {
            Ok(group) => {
                match group.get_units() {
                    Ok(units) => {
                        if units.len() == 0 {
                            // No units left - destroyed
                            self.state = ConvoyState::Destroyed;
                            ConvoyState::Destroyed
                        } else {
                            // Update last known position
                            if let Ok(unit) = units.get(1) {
                                if let Ok(pos) = unit.get_point() {
                                    self.last_pos = Vector2::new(pos.x, pos.z);
                                }
                            }
                            self.state
                        }
                    }
                    Err(_) => {
                        // Can't get units - assume destroyed
                        self.state = ConvoyState::Destroyed;
                        ConvoyState::Destroyed
                    }
                }
            }
            Err(_) => {
                // Group doesn't exist anymore - destroyed
                self.state = ConvoyState::Destroyed;
                ConvoyState::Destroyed
            }
        }
    }

    /// Check if convoy has reached destination
    pub fn check_delivery(&mut self, destination_pos: Vector2, delivery_distance: f64) -> bool {
        let dist = (self.last_pos - destination_pos).norm();
        if dist <= delivery_distance {
            self.state = ConvoyState::Delivered;
            true
        } else {
            false
        }
    }

    /// Execute all transfers for this convoy
    pub fn execute_transfers(&self, db: &mut Persisted, to_bg: &Option<UnboundedSender<Task>>) -> Result<()> {
        for transfer in &self.transfers {
            transfer.execute(db, to_bg)?;
        }
        Ok(())
    }
}

/// Unique identifier for air and sea logistics routes
pub type LogiRouteId = CompactString;

/// Current state of an air or sea logistics route
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum LogiRouteState {
    InTransit,
    Delivered,
    Destroyed,
}

/// An AI cargo aircraft flying supplies from a logistics hub to a destination objective
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AirLogisticsRoute {
    pub id: LogiRouteId,
    pub group_id: bfprotocols::db::group::GroupId,
    pub origin: ObjectiveId,
    pub destination: ObjectiveId,
    pub cargo_type: ConvoyCargoType,
    pub transfers: Vec<Transfer>,
    pub spawn_time: DateTime<Utc>,
    pub state: LogiRouteState,
    pub side: Side,
    pub last_pos: Vector2,
    pub last_check: DateTime<Utc>,
}

impl AirLogisticsRoute {
    pub fn check_status(&mut self, lua: MizLua, group_name: &str) -> LogiRouteState {
        use dcso3::group::Group;
        match Group::get_by_name(lua, group_name) {
            Ok(group) => match group.get_units() {
                Ok(units) => {
                    if units.len() == 0 {
                        self.state = LogiRouteState::Destroyed;
                        LogiRouteState::Destroyed
                    } else {
                        if let Ok(unit) = units.get(1) {
                            if let Ok(pos) = unit.get_point() {
                                self.last_pos = Vector2::new(pos.x, pos.z);
                            }
                        }
                        self.state
                    }
                }
                Err(_) => {
                    self.state = LogiRouteState::Destroyed;
                    LogiRouteState::Destroyed
                }
            },
            Err(_) => {
                self.state = LogiRouteState::Destroyed;
                LogiRouteState::Destroyed
            }
        }
    }

    pub fn check_delivery(&mut self, destination_pos: Vector2, delivery_distance: f64) -> bool {
        let dist = (self.last_pos - destination_pos).norm();
        if dist <= delivery_distance {
            self.state = LogiRouteState::Delivered;
            true
        } else {
            false
        }
    }

    pub fn execute_transfers(&self, db: &mut Persisted, to_bg: &Option<UnboundedSender<Task>>) -> Result<()> {
        for transfer in &self.transfers {
            transfer.execute(db, to_bg)?;
        }
        Ok(())
    }
}

/// An AI ship transporting supplies from a naval base to a carrier group
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeaLogisticsRoute {
    pub id: LogiRouteId,
    pub group_id: bfprotocols::db::group::GroupId,
    pub origin: ObjectiveId,
    pub destination: ObjectiveId,
    pub cargo_type: ConvoyCargoType,
    pub transfers: Vec<Transfer>,
    pub spawn_time: DateTime<Utc>,
    pub state: LogiRouteState,
    pub side: Side,
    pub last_pos: Vector2,
    pub last_check: DateTime<Utc>,
}

impl SeaLogisticsRoute {
    pub fn check_status(&mut self, lua: MizLua, group_name: &str) -> LogiRouteState {
        use dcso3::group::Group;
        match Group::get_by_name(lua, group_name) {
            Ok(group) => match group.get_units() {
                Ok(units) => {
                    if units.len() == 0 {
                        self.state = LogiRouteState::Destroyed;
                        LogiRouteState::Destroyed
                    } else {
                        if let Ok(unit) = units.get(1) {
                            if let Ok(pos) = unit.get_point() {
                                self.last_pos = Vector2::new(pos.x, pos.z);
                            }
                        }
                        self.state
                    }
                }
                Err(_) => {
                    self.state = LogiRouteState::Destroyed;
                    LogiRouteState::Destroyed
                }
            },
            Err(_) => {
                self.state = LogiRouteState::Destroyed;
                LogiRouteState::Destroyed
            }
        }
    }

    pub fn check_delivery(&mut self, destination_pos: Vector2, delivery_distance: f64) -> bool {
        let dist = (self.last_pos - destination_pos).norm();
        if dist <= delivery_distance {
            self.state = LogiRouteState::Delivered;
            true
        } else {
            false
        }
    }

    pub fn execute_transfers(&self, db: &mut Persisted, to_bg: &Option<UnboundedSender<Task>>) -> Result<()> {
        for transfer in &self.transfers {
            transfer.execute(db, to_bg)?;
        }
        Ok(())
    }
}

struct Needed<'a> {
    oid: &'a ObjectiveId,
    obj: &'a Objective,
    demanded: u32,
    allocated: u32,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Warehouse {
    pub(super) base_equipment: Map<String, Inventory>,
    pub(super) equipment: Map<String, Inventory>,
    pub(super) liquids: MapS<LiquidType, Inventory>,
    pub(super) supplier: Option<ObjectiveId>,
    pub(super) destination: SetS<ObjectiveId>,
    #[serde(default)]
    pub(super) damaged: bool,
}

impl Warehouse {
    pub fn equipment(&self) -> &Map<String, Inventory> {
        &self.equipment
    }

    pub fn liquids(&self) -> &MapS<LiquidType, Inventory> {
        &self.liquids
    }
}

/// Airframe entries sit as plain type-name keys in the same equipment map as
/// weapons/vehicles ("weapons."/"vehicles."/"Fortifications." prefixed), so
/// this is the established way (already used by the supply-transfer
/// exemption logic) to tell them apart within that shared map.
fn is_airframe_item(name: &str) -> bool {
    !name.starts_with("weapons.") && !name.starts_with("vehicles.") && !name.starts_with("Fortifications.")
}

pub(super) fn sync_obj_to_warehouse(obj: &Objective, warehouse: &warehouse::Warehouse) -> Result<()> {
    let perf = unsafe { Perf::get_mut() };
    let perf = Arc::make_mut(&mut perf.inner);
    for (item, inv) in &obj.warehouse.equipment {
        perf.logistics_items.insert((item.clone(), obj.id));
        if item.as_str() == "AJS37" || item.as_str() == "C-130J-30" || item.as_str().starts_with("CH-47F") {
            info!("[WAREHOUSE_SYNC] pushing obj={} owner={:?} {item}=stored:{}",
                  obj.name, obj.owner, inv.stored);
        }
        warehouse
            .set_item(item.clone(), inv.stored)
            .context("setting item")?
    }
    for (name, inv) in &obj.warehouse.liquids {
        warehouse
            .set_liquid_amount(*name, inv.stored)
            .context("setting liquid")?
    }
    Ok(())
}

/// Like sync_obj_to_warehouse but also zeros out items that are in the resource map
/// but not in the objective's warehouse. This is needed for carriers and other objectives
/// that spawn with default DCS warehouse contents that may include items not in the
/// production config.
pub(super) fn sync_obj_to_warehouse_with_zeroing(
    obj: &Objective,
    warehouse: &warehouse::Warehouse,
    resource_map: &warehouse::ResourceMap,
) -> Result<()> {
    let perf = unsafe { Perf::get_mut() };
    let perf = Arc::make_mut(&mut perf.inner);

    // First, zero out all items from the resource map that are NOT in the objective's warehouse
    resource_map.for_each(|name, _| {
        if obj.warehouse.equipment.get(&name).is_none() {
            warehouse.set_item(name, 0).context("zeroing item not in objective warehouse")?;
        }
        Ok(())
    })?;

    // Then set the items that ARE in the objective's warehouse
    for (item, inv) in &obj.warehouse.equipment {
        perf.logistics_items.insert((item.clone(), obj.id));
        warehouse
            .set_item(item.clone(), inv.stored)
            .context("setting item")?
    }
    for (name, inv) in &obj.warehouse.liquids {
        warehouse
            .set_liquid_amount(*name, inv.stored)
            .context("setting liquid")?
    }
    Ok(())
}

fn sync_warehouse_to_obj(obj: &mut Objective, warehouse: &warehouse::Warehouse) -> Result<()> {
    for (name, inv) in obj.warehouse.equipment.iter_mut_cow() {
        inv.stored = warehouse.get_item_count(name.clone())?;
    }
    for (name, inv) in obj.warehouse.liquids.iter_mut_cow() {
        inv.stored = warehouse.get_liquid_amount(*name)?;
    }
    Ok(())
}

fn get_supplier<'lua>(lua: MizLua<'lua>, template: String) -> Result<warehouse::Warehouse<'lua>> {
    Airbase::get_by_name(lua, template.clone())
        .with_context(|| format_compact!("getting airbase {}", template))?
        .get_warehouse()
        .context("getting warehouse")
}

impl Db {
    fn init_resource_map(&mut self, lua: MizLua) -> Result<()> {
        let whcfg = match self.ephemeral.cfg.warehouse.as_ref() {
            None => return Ok(()),
            Some(w) => w,
        };
        if self.ephemeral.production_by_side.is_empty() {
            info!("[WAREHOUSE] Production data empty, initializing from resource map");
            let map =
                warehouse::Warehouse::get_resource_map(lua).context("getting resource map")?;
            let mut warned_neutral = false;
            map.for_each(|name, typ| {
                for side in Side::ALL {
                    let template = match whcfg.supply_source.get(&side) {
                        Some(tmpl) => tmpl,
                        None => {
                            if !warned_neutral && side == dcso3::coalition::Side::Neutral {
                                warn!("[WAREHOUSE] No supply_source configured for Neutral side - skipping");
                                warned_neutral = true;
                            } else if side != dcso3::coalition::Side::Neutral {
                                warn!("[WAREHOUSE] No supply_source configured for side {:?} - warehouses will be empty!", side);
                            }
                            continue;
                        }
                    };
                    let w = get_supplier(lua, template.clone())
                        .with_context(|| format_compact!("getting supplier {template} for side {:?}. Make sure this airbase exists in the mission and has a warehouse configured!", side))?;
                    let production =
                        Arc::make_mut(self.ephemeral.production_by_side.entry(side).or_default());
                    let qty = w
                        .get_item_count(name.clone())
                        .with_context(|| format_compact!("getting {name} from the warehouse"))?;
                    if qty > 0 {
                        production
                            .equipment
                            .insert(name.clone(), Equipment { production: qty });
                        let category = typ.category().context("getting category")?;
                        if category.is_aircraft() {
                            let vehicle = Vehicle::from(name.clone());
                            self.ephemeral
                                .cfg
                                .check_vehicle_has_threat_distance(&vehicle)
                                .with_context(|| format_compact!("checking threat distance for aircraft {}", name))?;
                            self.ephemeral.cfg.check_vehicle_has_life_type(&vehicle)
                                .with_context(|| format_compact!("checking life type for aircraft {}", name))?;
                        }
                    }
                    for name in LiquidType::ALL {
                        let qty = w.get_liquid_amount(name).context("getting liquid amount")?;
                        if qty > 0 {
                            production.liquids.insert(name, qty);
                        }
                    }
                }
                Ok(())
            })
            .context("iterating resource map")?;
            // Backfill explicit zero entries: the loop above only inserts an
            // item when qty > 0, so an item that's deliberately 0 in one
            // side's supply source (e.g. an aircraft type that side isn't
            // meant to have) but nonzero for another side never became a
            // tracked entry for the excluded side at all. That meant nothing
            // ever called set_item(name, 0) to actually zero it out on that
            // side's warehouses -- whatever the built mission file already
            // had for it (from bftools/the base .miz) was silently left in
            // place forever. Explicitly tracking it as production=0 makes
            // the normal init/capture sync paths push a real zero.
            let all_managed: fxhash::FxHashSet<String> = self
                .ephemeral
                .production_by_side
                .values()
                .flat_map(|p| p.equipment.keys().cloned())
                .collect();
            for side in Side::ALL {
                let production =
                    Arc::make_mut(self.ephemeral.production_by_side.entry(side).or_default());
                for name in &all_managed {
                    if !production.equipment.contains_key(name) {
                        production
                            .equipment
                            .insert(name.clone(), Equipment { production: 0 });
                    }
                }
            }
            info!("[WAREHOUSE] Resource map initialized. Sides with production: {:?}",
                  self.ephemeral.production_by_side.keys().collect::<Vec<_>>());
            for (side, production) in &self.ephemeral.production_by_side {
                for probe in ["AJS37", "C-130J-30", "CH-47Fbl1"] {
                    match production.equipment.get(probe) {
                        Some(equip) => info!("[WAREHOUSE_PROBE] {side:?} {probe}: production={}", equip.production),
                        None => info!("[WAREHOUSE_PROBE] {side:?} {probe}: not tracked at all"),
                    }
                }
            }
        } else {
            info!("[WAREHOUSE] Production data already exists, skipping resource map init");
        }
        Ok(())
    }

    pub(super) fn init_farp_warehouse(&mut self, oid: &ObjectiveId) -> Result<()> {
        let whcfg = match self.ephemeral.cfg.warehouse.as_ref() {
            Some(cfg) => cfg,
            None => return Ok(()),
        };
        let obj = objective_mut!(self, oid)?;
        let production = match self.ephemeral.production_by_side.get(&obj.owner) {
            Some(q) => Arc::clone(q),
            None => return Ok(()),
        };
        for (name, equip) in &production.equipment {
            let unlimited = if is_airframe_item(name) { obj.unlimited_aircraft } else { obj.unlimited_supply };
            let inv = Inventory {
                stored: 0,
                capacity: whcfg.capacity_for(&obj.name, unlimited, false, equip.production),
            };
            obj.warehouse.equipment.insert_cow(name.clone(), inv);
        }
        for (name, qty) in &production.liquids {
            let inv = Inventory {
                stored: 0,
                capacity: whcfg.capacity_for(&obj.name, obj.unlimited_supply, false, *qty),
            };
            obj.warehouse.liquids.insert_cow(*name, inv);
        }
        Ok(())
    }

    pub(super) fn init_warehouses(&mut self, lua: MizLua) -> Result<()> {
        self.init_resource_map(lua)
            .context("initializing resource map")?;
        let cfg = &self.ephemeral.cfg;
        info!("[WAREHOUSE] Checking warehouse config: exists = {}", cfg.warehouse.is_some());
        let whcfg = match cfg.warehouse.as_ref() {
            Some(cfg) => {
                info!("[WAREHOUSE] Warehouse config found: hub_max={}, airbase_max={}", cfg.hub_max, cfg.airbase_max);
                cfg
            },
            None => {
                warn!("[WAREHOUSE] No warehouse config found - warehouses will not be initialized!");
                return Ok(());
            }
        };
        info!("[WAREHOUSE] Starting warehouse initialization");
        for side in Side::ALL {
            let production = match self.ephemeral.production_by_side.get(&side) {
                None => {
                    warn!("[WAREHOUSE] No production data for side {:?} - warehouses will be empty for this side!", side);
                    continue;
                }
                Some(q) => Arc::clone(q),
            };
            info!("[WAREHOUSE] Initializing warehouses for side {:?} with {} equipment types and {} liquid types",
                  side, production.equipment.len(), production.liquids.len());
            let mut initialized_count = 0;
            for (name, equip) in &production.equipment {
                for (oid, obj) in self.persisted.objectives.iter_mut_cow() {
                    if obj.owner == side {
                        let is_carrier = self.persisted.carrier_groups.contains(&oid);
                        let hub = self.persisted.logistics_hubs.contains(&oid) || is_carrier;
                        let unlimited = if is_airframe_item(name) { obj.unlimited_aircraft } else { obj.unlimited_supply };
                        let capacity = whcfg.capacity_for(&obj.name, unlimited, hub, equip.production);
                        let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
                        inv.capacity = capacity;
                        inv.stored = capacity;
                        if is_carrier {
                            info!("[WAREHOUSE] Initialized carrier {} with equipment {} (capacity: {}, hub: {})",
                                  obj.name, name, capacity, hub);
                        }
                    }
                }
            }
            for (name, qty) in &production.liquids {
                for (oid, obj) in self.persisted.objectives.iter_mut_cow() {
                    if obj.owner == side {
                        let is_carrier = self.persisted.carrier_groups.contains(&oid);
                        let hub = self.persisted.logistics_hubs.contains(&oid) || is_carrier;
                        let capacity = whcfg.capacity_for(&obj.name, obj.unlimited_supply, hub, *qty);
                        let inv = obj.warehouse.liquids.get_or_default_cow(*name);
                        inv.capacity = capacity;
                        inv.stored = capacity;
                        if is_carrier {
                            initialized_count += 1;
                        }
                    }
                }
            }
            info!("[WAREHOUSE] Initialized {} carrier warehouses for side {:?}", initialized_count, side);
        }
        self.ephemeral.dirty();
        Ok(())
    }

    pub fn reinit_objective_warehouse(&mut self, oid: ObjectiveId) -> Result<()> {
        let whcfg = match self.ephemeral.cfg.warehouse.as_ref() {
            Some(cfg) => cfg,
            None => return Ok(()),
        };

        let obj = objective!(self, oid)?;
        let side = obj.owner;
        // Match init_warehouses: carriers get hub-tier capacity even
        // though they're never in persisted.logistics_hubs, otherwise an
        // admin-triggered reinit demotes a carrier's warehouse to
        // airbase-tier capacity and its numbers stop matching what it had
        // at mission start.
        let is_carrier = self.persisted.carrier_groups.contains(&oid);
        let hub = self.persisted.logistics_hubs.contains(&oid) || is_carrier;

        let production = match self.ephemeral.production_by_side.get(&side) {
            None => {
                debug!("no production data for side {:?}, cannot reinit warehouse for objective {}", side, oid);
                return Ok(());
            }
            Some(q) => Arc::clone(q),
        };

        let obj = objective_mut!(self, oid)?;

        // Initialize equipment inventory
        for (name, equip) in &production.equipment {
            let unlimited = if is_airframe_item(name) { obj.unlimited_aircraft } else { obj.unlimited_supply };
            let capacity = whcfg.capacity_for(&obj.name, unlimited, hub, equip.production);
            let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
            inv.capacity = capacity;
            inv.stored = capacity;
        }

        // Initialize liquids inventory
        for (name, qty) in &production.liquids {
            let capacity = whcfg.capacity_for(&obj.name, obj.unlimited_supply, hub, *qty);
            let inv = obj.warehouse.liquids.get_or_default_cow(*name);
            inv.capacity = capacity;
            inv.stored = capacity;
        }

        info!("[WAREHOUSE] Re-initialized warehouse for objective {} with {:?} coalition aircraft",
              objective!(self, oid)?.name, side);
        self.ephemeral.dirty();
        Ok(())
    }

    pub(super) fn setup_warehouses_after_load(&mut self, lua: MizLua) -> Result<()> {
        self.init_resource_map(lua)
            .context("initializing resource map")?;
        let whcfg = match self.ephemeral.cfg.warehouse.as_ref() {
            Some(cfg) => cfg,
            None => return Ok(()),
        };
        let map = warehouse::Warehouse::get_resource_map(lua).context("getting resource map")?;
        let world = World::singleton(lua).context("getting world")?;
        let mut load_and_sync_airbases = || -> Result<()> {
            world
                .get_airbases()
                .context("getting airbases")?
                .for_each(|airbase| {
                    let airbase = airbase.context("getting airbase")?;
                    let name = airbase.as_object()?.get_name()?;
                    log::info!("setting up airbase {name}");

                    if !airbase.is_exist()? {
                        return Ok(()); // can happen when farps get recycled
                    }
                    let pos3 = airbase.get_point().context("getting airbase position")?;
                    let pos = Vector2::new(pos3.x, pos3.z);
                    airbase
                        .auto_capture(false)
                        .context("setting airbase autocapture")?;
                    let oid = self
                        .persisted
                        .objectives
                        .into_iter()
                        .find(|(_, obj)| obj.zone.contains(pos));
                    let w = airbase
                        .get_warehouse()
                        .context("getting airbase warehouse")?;
                    let (oid, obj) = match oid {
                        Some((oid, obj)) => {
                            airbase
                                .set_coalition(obj.owner)
                                .context("setting airbase owner")?;
                            (*oid, obj)
                        }
                        None if !self.ephemeral.global_pad_templates.contains(&name) => {
                            map.for_each(|name, _| {
                                w.set_item(name, 0).context("zeroing item")?;
                                Ok(())
                            })?;
                            return Ok(());
                        }
                        None => {
                            // Carrier template groups (late-activated BCARRIER/RCARRIER groups)
                            // won't have an objective containing them, which is expected
                            if name.starts_with("BCARRIER") || name.starts_with("RCARRIER") {
                                log::info!("skipping carrier template group {name} (no matching objective zone)");
                            } else {
                                log::info!("airbase {name} has no objective");
                            }
                            return Ok(());
                        }
                    };

                    // For carrier groups, only register the first ship (the main carrier).
                    // The DCS airbase name is the unit name (e.g., "Kurznetsov", "CVN73"),
                    // not the group template name (e.g., "RCARRIER", "BCARRIER").
                    // We identify the main carrier by being the first one we find in the zone.
                    let is_carrier_group = matches!(&obj.kind, ObjectiveKind::CarrierGroup { .. });

                    match self.ephemeral.airbase_by_oid.entry(oid) {
                        Entry::Vacant(e) => {
                            e.insert(airbase.object_id().context("getting airbase object_id")?);

                            // For carrier groups, sync the warehouse with zeroing to remove
                            // items not in the production config
                            if is_carrier_group {
                                log::info!("[CARRIER_WAREHOUSE] Registering carrier warehouse for {} (objective: {})",
                                          name, obj.name);
                                sync_obj_to_warehouse_with_zeroing(obj, &w, &map)
                                    .context("syncing carrier warehouse with zeroing")?;
                            }
                        }
                        Entry::Occupied(_) => {
                            // For carrier groups, skip escort ships (additional airbases in the zone)
                            if is_carrier_group {
                                log::info!("[CARRIER_WAREHOUSE] Skipping escort ship {} in carrier group {} (warehouse already registered)",
                                          name, obj.name);
                                return Ok(());
                            }
                            bail!("multiple airbases inside the trigger zone of {}", obj.name)
                        }
                    }
                    Ok(())
                })
        };
        load_and_sync_airbases().context("loading and syncing airbases")?;
        let mut adjust_warehouses_for_miz_changes = || -> Result<()> {
            for (oid, obj) in self.persisted.objectives.iter_mut_cow() {
                let mut del_eq: SmallVec<[String; 8]> = smallvec![];
                let mut del_l: SmallVec<[LiquidType; 4]> = smallvec![];
                if let Some(prod) = self.ephemeral.production_by_side.get(&obj.owner) {
                    // See capture_warehouse/reinit_objective_warehouse: carriers
                    // need the same hub-tier OR here, otherwise every mission
                    // load/resync re-shrinks a carrier's warehouse capacity down
                    // to airbase-tier.
                    let is_carrier = self.persisted.carrier_groups.contains(oid);
                    let hub = self.persisted.logistics_hubs.contains(oid) || is_carrier;
                    for (name, _) in &obj.warehouse.equipment {
                        if !prod.equipment.contains_key(name) {
                            del_eq.push(name.clone());
                        }
                    }
                    for name in del_eq {
                        obj.warehouse.equipment.remove_cow(&name);
                    }
                    for (liq, _) in &obj.warehouse.liquids {
                        if !prod.liquids.contains_key(liq) {
                            del_l.push(*liq);
                        }
                    }
                    for liq in del_l {
                        obj.warehouse.liquids.remove_cow(&liq);
                    }
                    for (name, eqip) in &prod.equipment {
                        let unlimited = if is_airframe_item(name) { obj.unlimited_aircraft } else { obj.unlimited_supply };
                        let capacity = whcfg.capacity_for(&obj.name, unlimited, hub, eqip.production);
                        let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
                        inv.capacity = capacity;
                    }
                    for (name, prod) in &prod.liquids {
                        let capacity = whcfg.capacity_for(&obj.name, obj.unlimited_supply, hub, *prod);
                        let inv = obj.warehouse.liquids.get_or_default_cow(*name);
                        inv.capacity = capacity;
                    }
                }
            }
            Ok(())
        };
        adjust_warehouses_for_miz_changes().context("adjusting warehouses for miz changes")?;
        let mut missing = vec![];
        for (oid, obj) in &self.persisted.objectives {
            // Only objectives with DCS airbases need warehouse validation
            // CarrierGroups, Logistics hubs, NavalBases, and Factories don't have traditional airbases
            match obj.kind {
                ObjectiveKind::Airbase | ObjectiveKind::Farp { .. } | ObjectiveKind::Fob => {
                    if !self.ephemeral.airbase_by_oid.contains_key(oid) {
                        missing.push(obj.name.clone());
                    }
                }
                ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::Logistics | ObjectiveKind::NavalBase | ObjectiveKind::Factory { .. } | ObjectiveKind::SpecialSamSite { .. } => {
                    // These objective types don't require airbase warehouses
                }
            }
        }
        if !missing.is_empty() {
            bail!("objectives missing a warehouse {:?}", missing)
        }
        self.update_supply_status()
            .context("updating supply status")?;
        self.setup_supply_lines()
            .context("setting up supply lines")?;
        Ok(())
    }

    pub fn admin_tick_now(&mut self) {
        match &mut self.ephemeral.logistics_stage {
            LogiStage::Init
            | LogiStage::SyncFromWarehouses { .. }
            | LogiStage::SyncToWarehouses { .. }
            | LogiStage::ExecuteTransfers { .. }
            | LogiStage::ManageConvoys
            | LogiStage::ManageAirRoutes
            | LogiStage::ManageSeaRoutes => (),
            LogiStage::Complete { last_tick } => {
                *last_tick = DateTime::<Utc>::MIN_UTC;
            }
        }
    }

    pub fn admin_deliver_now(&mut self) {
        self.admin_tick_now();
        self.persisted.logistics_ticks_since_delivery = u32::MAX;
    }

    pub fn logistics_step(
        &mut self,
        lua: MizLua,
        perf: &mut PerfInner,
        ts: DateTime<Utc>,
    ) -> Result<()> {
        if let Some(wcfg) = self.ephemeral.cfg.warehouse.as_ref() {
            let freq = Duration::minutes(wcfg.tick as i64);
            let ticks_per_delivery = wcfg.ticks_per_delivery;
            let start_ts = Utc::now();
            match &mut self.ephemeral.logistics_stage {
                LogiStage::Init => {
                    let objectives = self
                        .persisted
                        .objectives
                        .into_iter()
                        .filter(|(_, obj)| !obj.kind.is_special_sam_site())
                        .map(|(id, _)| *id)
                        .collect();
                    self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives }
                }
                LogiStage::Complete { last_tick } if ts - *last_tick >= freq => {
                    let objectives = self
                        .persisted
                        .objectives
                        .into_iter()
                        .filter(|(_, obj)| !obj.kind.is_special_sam_site())
                        .map(|(id, _)| *id)
                        .collect();
                    self.ephemeral.logistics_stage = LogiStage::SyncFromWarehouses { objectives };
                }
                LogiStage::Complete { last_tick: _ } => (),
                LogiStage::SyncFromWarehouses { objectives } => match objectives.pop() {
                    Some(oid) => {
                        let start_ts = Utc::now();
                        if let Err(e) = self.sync_warehouse_to_objective(lua, oid) {
                            error!("failed to sync objective {oid} from warehouse {:?}", e)
                        }
                        record_perf(&mut perf.logistics_sync_from, start_ts);
                        // Supply critical alert check
                        let threshold = self.ephemeral.cfg.supply_alert_threshold;
                        if threshold > 0 {
                            if let Some(obj) = self.persisted.objectives.get(&oid) {
                                let is_low = obj.warehouse.equipment.into_iter().any(|(_, inv)| {
                                    inv.capacity > 0
                                        && inv
                                            .percent()
                                            .map(|p| p < threshold)
                                            .unwrap_or(false)
                                });
                                let side = obj.owner;
                                let name = obj.name.clone();
                                if is_low {
                                    let newly_warned = !self.ephemeral.supply_warned.contains_key(&oid);
                                    self.ephemeral.supply_warned.entry(oid).or_insert(ts);
                                    if newly_warned {
                                        let pos = obj.zone.pos();
                                        let (ml, msgs) = self.ephemeral.map_layer_and_msgs();
                                        ml.on_supply_critical(oid, pos, side, &name, threshold, msgs);
                                    }
                                } else {
                                    self.ephemeral.supply_warned.remove(&oid);
                                    let (ml, msgs) = self.ephemeral.map_layer_and_msgs();
                                    ml.on_supply_recovered(&oid, msgs);
                                }
                            }
                        }
                    }
                    None => {
                        let sts = Utc::now();
                        let transfers = if self.persisted.logistics_ticks_since_delivery
                            >= ticks_per_delivery
                        {
                            self.persisted.logistics_ticks_since_delivery = 0;
                            let v = match self.deliver_production(lua, ts) {
                                Ok(v) => v,
                                Err(e) => {
                                    error!("failed to deliver production {:?}", e);
                                    vec![]
                                }
                            };
                            record_perf(&mut perf.logistics_deliver, sts);
                            v
                        } else {
                            self.persisted.logistics_ticks_since_delivery += 1;
                            let v = match self.deliver_supplies_from_logistics_hubs(lua, ts) {
                                Ok(v) => v,
                                Err(e) => {
                                    error!("failed to deliver supplies from hubs {:?}", e);
                                    vec![]
                                }
                            };
                            record_perf(&mut perf.logistics_distribute, sts);
                            v
                        };
                        self.ephemeral.logistics_stage = LogiStage::ExecuteTransfers { transfers };
                    }
                },
                LogiStage::ExecuteTransfers { transfers } if transfers.is_empty() => {
                    let st = Utc::now();

                    // ── Auto convoy dispatch after supply-critical delay ───────────
                    let auto_delay_secs = self.ephemeral.cfg.supply_auto_convoy_delay_secs;
                    let convoy_enabled = self.ephemeral.cfg.warehouse
                        .as_ref()
                        .and_then(|w| w.convoy.as_ref())
                        .map(|c| c.enabled)
                        .unwrap_or(false);
                    if auto_delay_secs > 0 && convoy_enabled {
                        let auto_delay = chrono::Duration::seconds(auto_delay_secs as i64);
                        let threshold = self.ephemeral.cfg.supply_alert_threshold as u32;
                        // Collect objectives that have been warned long enough and still need supply
                        let auto_dispatch: Vec<ObjectiveId> = self.ephemeral.supply_warned.iter()
                            .filter(|(_, warned_at)| ts - **warned_at >= auto_delay)
                            .filter_map(|(oid, _)| {
                                self.persisted.objectives.get(oid).and_then(|obj| {
                                    let still_low = obj.warehouse.equipment.into_iter().any(|(_, inv)| {
                                        inv.capacity > 0
                                            && inv.percent().map(|p| (p as u32) < threshold).unwrap_or(false)
                                    });
                                    // Only dispatch if no convoy already heading to this objective
                                    let already_en_route = self.ephemeral.active_convoys.values()
                                        .any(|c| c.destination == *oid);
                                    if still_low && !already_en_route { Some(*oid) } else { None }
                                })
                            })
                            .collect();

                        for dest_oid in auto_dispatch {
                            // Find the nearest logistics hub that serves this objective
                            let hub_oid = self.persisted.logistics_hubs.into_iter()
                                .filter(|lid| {
                                    let logi = self.persisted.objectives.get(*lid);
                                    let dest  = self.persisted.objectives.get(&dest_oid);
                                    match (logi, dest) {
                                        (Some(l), Some(d)) => {
                                            l.owner == d.owner
                                                && l.warehouse.destination.contains(&dest_oid)
                                        }
                                        _ => false,
                                    }
                                })
                                .copied()
                                .next();

                            if let Some(hub) = hub_oid {
                                let dest_name = self.persisted.objectives.get(&dest_oid)
                                    .map(|o| o.name.clone())
                                    .unwrap_or_default();
                                let _side = self.persisted.objectives.get(&dest_oid)
                                    .map(|o| o.owner)
                                    .unwrap_or(dcso3::coalition::Side::Neutral);
                                match self.spawn_supply_convoy(
                                    lua,
                                    hub,
                                    dest_oid,
                                    ConvoyCargoType::Mixed,
                                    vec![],
                                    ts,
                                ) {
                                    Ok(()) => {
                                        info!("AUTO-DISPATCH: supply convoy → {}", dest_name);
                                        self.ephemeral.supply_warned.insert(dest_oid, ts);
                                    }
                                    Err(e) => {
                                        error!("auto convoy dispatch to {} failed: {e:?}", dest_name);
                                    }
                                }
                            }
                        }
                    }

                    self.balance_logistics_hubs()?;

                    // Chain through management stages: convoys → air routes → sea routes → sync
                    if !self.ephemeral.active_convoys.is_empty() {
                        self.ephemeral.logistics_stage = LogiStage::ManageConvoys;
                    } else if !self.ephemeral.active_air_routes.is_empty() {
                        self.ephemeral.logistics_stage = LogiStage::ManageAirRoutes;
                    } else if !self.ephemeral.active_sea_routes.is_empty() {
                        self.ephemeral.logistics_stage = LogiStage::ManageSeaRoutes;
                    } else {
                        let objectives = self
                            .persisted
                            .objectives
                            .into_iter()
                            .map(|(id, _)| *id)
                            .collect();
                        self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives };
                    }
                    record_perf(&mut perf.logistics_transfer, st);
                }
                LogiStage::ExecuteTransfers { transfers } => {
                    let st = Utc::now();
                    while let Some(tr) = transfers.pop() {
                        if let Err(e) = tr.execute(&mut self.persisted, &self.ephemeral.to_bg) {
                            error!("executing transfer {:?} {e:?}", tr)
                        }
                        if Utc::now() - st > Duration::milliseconds(6) {
                            break;
                        }
                    }
                    record_perf(&mut perf.logistics_transfer, st);
                }
                LogiStage::ManageConvoys => {
                    // Check convoy status and handle deliveries/destruction
                    let st = Utc::now();
                    let convoy_cfg = self.ephemeral.cfg.warehouse
                        .as_ref()
                        .and_then(|w| w.convoy.as_ref());

                    if let Some(cfg) = convoy_cfg {
                        let delivery_distance = cfg.delivery_distance;
                        let mut completed_convoys = Vec::new();

                        for convoy_id in self.ephemeral.active_convoys.keys().cloned().collect::<Vec<_>>() {
                            if let Some(convoy) = self.ephemeral.active_convoys.get_mut(&convoy_id) {
                                // Check if enough time has passed since last check
                                if (ts - convoy.last_check).num_seconds() < cfg.check_interval_secs as i64 {
                                    continue;
                                }
                                convoy.last_check = ts;

                                // Get group name for status check
                                let group_name = match group!(self, &convoy.group_id) {
                                    Ok(g) => g.name.clone(),
                                    Err(_) => {
                                        warn!("Convoy {} group not found in database", convoy.id);
                                        convoy.state = ConvoyState::Destroyed;
                                        completed_convoys.push(convoy_id.clone());
                                        continue;
                                    }
                                };

                                // Check convoy status
                                let status = convoy.check_status(lua, &group_name);

                                match status {
                                    ConvoyState::InTransit => {
                                        // Check if convoy reached destination
                                        let dest_obj = match self.persisted.objectives.get(&convoy.destination) {
                                            Some(o) => o,
                                            None => {
                                                warn!("Convoy {} destination {:?} no longer exists", convoy.id, convoy.destination);
                                                convoy.state = ConvoyState::Destroyed;
                                                completed_convoys.push(convoy_id.clone());
                                                continue;
                                            }
                                        };

                                        if convoy.check_delivery(dest_obj.pos(), delivery_distance) {
                                            // Convoy delivered! Execute transfers
                                            info!("Convoy {} delivered to {}", convoy.id, dest_obj.name);
                                            if let Err(e) = convoy.execute_transfers(&mut self.persisted, &self.ephemeral.to_bg) {
                                                error!("Failed to execute convoy transfers: {:?}", e);
                                            }

                                            // Mark convoy as completed (group will eventually be cleaned up)
                                            completed_convoys.push(convoy_id.clone());
                                        }
                                    }
                                    ConvoyState::Destroyed => {
                                        // Convoy destroyed - supplies lost
                                        let origin_obj = self.persisted.objectives.get(&convoy.origin);
                                        let dest_obj = self.persisted.objectives.get(&convoy.destination);

                                        info!(
                                            "Convoy {} destroyed en route from {} to {}",
                                            convoy.id,
                                            origin_obj.map(|o| o.name.as_str()).unwrap_or("Unknown"),
                                            dest_obj.map(|o| o.name.as_str()).unwrap_or("Unknown")
                                        );

                                        completed_convoys.push(convoy_id.clone());
                                    }
                                    _ => {}
                                }
                            }

                            // Stop after processing for too long
                            if Utc::now() - st > Duration::milliseconds(6) {
                                break;
                            }
                        }

                        // Remove completed convoys
                        for convoy_id in completed_convoys {
                            self.ephemeral.active_convoys.remove(&convoy_id);
                        }
                    }

                    // Transition to next stage: convoys → air routes → sea routes → sync
                    if self.ephemeral.active_convoys.is_empty() {
                        if !self.ephemeral.active_air_routes.is_empty() {
                            self.ephemeral.logistics_stage = LogiStage::ManageAirRoutes;
                        } else if !self.ephemeral.active_sea_routes.is_empty() {
                            self.ephemeral.logistics_stage = LogiStage::ManageSeaRoutes;
                        } else {
                            let objectives = self
                                .persisted
                                .objectives
                                .into_iter()
                                .map(|(id, _)| *id)
                                .collect();
                            self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives };
                        }
                    }

                    record_perf(&mut perf.logistics_convoy, st);
                }
                LogiStage::ManageAirRoutes => {
                    let st = Utc::now();
                    let (delivery_distance, check_interval_secs) = match self
                        .ephemeral
                        .cfg
                        .warehouse
                        .as_ref()
                        .and_then(|w| w.air_logistics.as_ref())
                    {
                        Some(cfg) => (cfg.delivery_distance, cfg.check_interval_secs),
                        None => {
                            // Air logistics disabled/unconfigured — clear and move on
                            self.ephemeral.active_air_routes.clear();
                            let objectives = self
                                .persisted
                                .objectives
                                .into_iter()
                                .map(|(id, _)| *id)
                                .collect();
                            self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives };
                            record_perf(&mut perf.logistics_air_routes, st);
                            return Ok(());
                        }
                    };

                    let mut completed = Vec::new();
                    for route_id in self.ephemeral.active_air_routes.keys().cloned().collect::<Vec<_>>() {
                        if let Some(route) = self.ephemeral.active_air_routes.get_mut(&route_id) {
                            if (ts - route.last_check).num_seconds() < check_interval_secs as i64 {
                                continue;
                            }
                            route.last_check = ts;

                            let group_name = match group!(self, &route.group_id) {
                                Ok(g) => g.name.clone(),
                                Err(_) => {
                                    warn!("Air route {} group not found in database", route.id);
                                    route.state = LogiRouteState::Destroyed;
                                    completed.push(route_id.clone());
                                    continue;
                                }
                            };

                            let status = route.check_status(lua, &group_name);
                            match status {
                                LogiRouteState::InTransit => {
                                    let dest_pos = match self.persisted.objectives.get(&route.destination) {
                                        Some(o) => o.pos(),
                                        None => {
                                            warn!("Air route {} destination no longer exists", route.id);
                                            route.state = LogiRouteState::Destroyed;
                                            completed.push(route_id.clone());
                                            continue;
                                        }
                                    };
                                    if route.check_delivery(dest_pos, delivery_distance) {
                                        let dest_name = self.persisted.objectives.get(&route.destination)
                                            .map(|o| o.name.clone()).unwrap_or_default();
                                        info!("Air route {} delivered to {}", route.id, dest_name);
                                        if let Err(e) = route.execute_transfers(&mut self.persisted, &self.ephemeral.to_bg) {
                                            error!("Failed to execute air route transfers: {:?}", e);
                                        }
                                        if let Some(to_bg) = &self.ephemeral.to_bg {
                                            let _ = to_bg.send(Task::Stat(Stat::AirRouteDelivered {
                                                from: route.origin,
                                                to: route.destination,
                                                side: route.side,
                                            }));
                                        }
                                        completed.push(route_id.clone());
                                    }
                                }
                                LogiRouteState::Destroyed => {
                                    info!("Air route {} destroyed en route", route.id);
                                    if let Some(to_bg) = &self.ephemeral.to_bg {
                                        let _ = to_bg.send(Task::Stat(Stat::AirRouteDestroyed {
                                            from: route.origin,
                                            to: route.destination,
                                            side: route.side,
                                        }));
                                    }
                                    completed.push(route_id.clone());
                                }
                                LogiRouteState::Delivered => {}
                            }
                        }

                        if Utc::now() - st > Duration::milliseconds(6) {
                            break;
                        }
                    }

                    for route_id in completed {
                        self.ephemeral.active_air_routes.remove(&route_id);
                    }

                    if self.ephemeral.active_air_routes.is_empty() {
                        if !self.ephemeral.active_sea_routes.is_empty() {
                            self.ephemeral.logistics_stage = LogiStage::ManageSeaRoutes;
                        } else {
                            let objectives = self
                                .persisted
                                .objectives
                                .into_iter()
                                .map(|(id, _)| *id)
                                .collect();
                            self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives };
                        }
                    }

                    record_perf(&mut perf.logistics_air_routes, st);
                }
                LogiStage::ManageSeaRoutes => {
                    let st = Utc::now();
                    let (delivery_distance, check_interval_secs) = match self
                        .ephemeral
                        .cfg
                        .warehouse
                        .as_ref()
                        .and_then(|w| w.sea_logistics.as_ref())
                    {
                        Some(cfg) => (cfg.delivery_distance, cfg.check_interval_secs),
                        None => {
                            self.ephemeral.active_sea_routes.clear();
                            let objectives = self
                                .persisted
                                .objectives
                                .into_iter()
                                .map(|(id, _)| *id)
                                .collect();
                            self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives };
                            record_perf(&mut perf.logistics_sea_routes, st);
                            return Ok(());
                        }
                    };

                    let mut completed = Vec::new();
                    for route_id in self.ephemeral.active_sea_routes.keys().cloned().collect::<Vec<_>>() {
                        if let Some(route) = self.ephemeral.active_sea_routes.get_mut(&route_id) {
                            if (ts - route.last_check).num_seconds() < check_interval_secs as i64 {
                                continue;
                            }
                            route.last_check = ts;

                            let group_name = match group!(self, &route.group_id) {
                                Ok(g) => g.name.clone(),
                                Err(_) => {
                                    warn!("Sea route {} group not found in database", route.id);
                                    route.state = LogiRouteState::Destroyed;
                                    completed.push(route_id.clone());
                                    continue;
                                }
                            };

                            let status = route.check_status(lua, &group_name);
                            match status {
                                LogiRouteState::InTransit => {
                                    let dest_pos = match self.persisted.objectives.get(&route.destination) {
                                        Some(o) => o.pos(),
                                        None => {
                                            warn!("Sea route {} destination no longer exists", route.id);
                                            route.state = LogiRouteState::Destroyed;
                                            completed.push(route_id.clone());
                                            continue;
                                        }
                                    };
                                    if route.check_delivery(dest_pos, delivery_distance) {
                                        let dest_name = self.persisted.objectives.get(&route.destination)
                                            .map(|o| o.name.clone()).unwrap_or_default();
                                        info!("Sea route {} delivered to {}", route.id, dest_name);
                                        if let Err(e) = route.execute_transfers(&mut self.persisted, &self.ephemeral.to_bg) {
                                            error!("Failed to execute sea route transfers: {:?}", e);
                                        }
                                        if let Some(to_bg) = &self.ephemeral.to_bg {
                                            let _ = to_bg.send(Task::Stat(Stat::SeaRouteDelivered {
                                                from: route.origin,
                                                to: route.destination,
                                                side: route.side,
                                            }));
                                        }
                                        completed.push(route_id.clone());
                                    }
                                }
                                LogiRouteState::Destroyed => {
                                    info!("Sea route {} destroyed en route", route.id);
                                    if let Some(to_bg) = &self.ephemeral.to_bg {
                                        let _ = to_bg.send(Task::Stat(Stat::SeaRouteDestroyed {
                                            from: route.origin,
                                            to: route.destination,
                                            side: route.side,
                                        }));
                                    }
                                    completed.push(route_id.clone());
                                }
                                LogiRouteState::Delivered => {}
                            }
                        }

                        if Utc::now() - st > Duration::milliseconds(6) {
                            break;
                        }
                    }

                    for route_id in completed {
                        self.ephemeral.active_sea_routes.remove(&route_id);
                    }

                    if self.ephemeral.active_sea_routes.is_empty() {
                        let objectives = self
                            .persisted
                            .objectives
                            .into_iter()
                            .filter(|(_, obj)| !obj.kind.is_special_sam_site())
                            .map(|(id, _)| *id)
                            .collect();
                        self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives };
                    }

                    record_perf(&mut perf.logistics_sea_routes, st);
                }
                LogiStage::SyncToWarehouses { objectives } => match objectives.pop() {
                    None => self.ephemeral.logistics_stage = LogiStage::Complete { last_tick: ts },
                    Some(oid) => {
                        let start_ts = Utc::now();
                        if let Err(e) = self.sync_objective_to_warehouse(lua, oid) {
                            error!("failed to sync objective {oid} to warehouse {:?}", e)
                        }
                        record_perf(&mut perf.logistics_sync_to, start_ts);
                    }
                },
            }
            record_perf(&mut perf.logistics, start_ts);
        }
        Ok(())
    }

    pub(super) fn capture_warehouse(&mut self, lua: MizLua, oid: ObjectiveId) -> Result<()> {
        let whcfg = match self.ephemeral.cfg.warehouse.as_ref() {
            Some(cfg) => cfg,
            None => return Ok(()),
        };
        let obj = objective_mut!(self, oid)?;
        let other_production = match self.ephemeral.production_by_side.get(&obj.owner.opposite()) {
            Some(q) => Arc::clone(q),
            None => Arc::new(Production::default()),
        };
        let production = match self.ephemeral.production_by_side.get(&obj.owner) {
            Some(q) => Arc::clone(q),
            None => return Ok(()),
        };
        let map = warehouse::Warehouse::get_resource_map(lua).context("getting resource map")?;
        let is_carrier = matches!(obj.kind, ObjectiveKind::CarrierGroup { .. });
        // Carriers aren't ObjectiveKind::Logistics so is_hub() alone says
        // false, but init_warehouses gives them hub-tier capacity at
        // mission start (self.persisted.logistics_hubs.contains(&oid) ||
        // is_carrier) -- without the same OR here, every capture silently
        // downgraded a carrier's warehouse to airbase-tier capacity,
        // diverging from its own mission-start numbers and from land-base
        // hub numbers.
        let hub = obj.kind.is_hub() || is_carrier;
        map.for_each(|name, _| {
            match production.equipment.get(&name) {
                Some(equip) => {
                    let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
                    let unlimited = if is_airframe_item(name.as_str()) { obj.unlimited_aircraft } else { obj.unlimited_supply };
                    let capacity = whcfg.capacity_for(&obj.name, unlimited, hub, equip.production);
                    inv.capacity = capacity;
                    // Also (re)stock, not just resize -- this only ran on
                    // capacity before, so a freshly-captured base never got
                    // its warehouse actually filled with the new owner's
                    // stock (airframes included, since they're plain entries
                    // in this same equipment map) until whatever it already
                    // had happened to reach the new capacity through normal
                    // resupply. New owner should start fully stocked, same
                    // as at mission init.
                    inv.stored = capacity;
                    if name.as_str() == "AJS37" || name.as_str() == "C-130J-30" || name.as_str().starts_with("CH-47F") {
                        info!("[WAREHOUSE_CAPTURE] {:?} obj={} {name}: production={} capacity={capacity}",
                              obj.owner, obj.name, equip.production);
                    }
                }
                None => {
                    if let Some(equip) = other_production.equipment.get(&name) {
                        let inv = obj.warehouse.equipment.get_or_default_cow(name);
                        if is_carrier {
                            // captured carrier: keep the previous owner's aircraft available
                            // with hub capacity so the new owner can operate them
                            inv.capacity = whcfg.capacity(true, equip.production);
                            // stored stays as-is (whatever was on the carrier at capture)
                        } else {
                            inv.stored = 0;
                            inv.capacity = 0;
                        }
                    }
                }
            }
            Ok(())
        })?;
        for name in LiquidType::ALL {
            match production.liquids.get(&name) {
                Some(qty) => {
                    let inv = obj.warehouse.liquids.get_or_default_cow(name);
                    inv.capacity = whcfg.capacity_for(&obj.name, obj.unlimited_supply, hub, *qty);
                }
                None => {
                    if let Some(_) = other_production.liquids.get(&name) {
                        let inv = obj.warehouse.liquids.get_or_default_cow(name);
                        // liquids are side-neutral (fuel/ammo) so always preserve
                        // capacity on carriers; zero out on regular objectives
                        if !is_carrier {
                            inv.stored = 0;
                            inv.capacity = 0;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub(super) fn compute_supplier(&self, obj: &Objective) -> Result<Option<ObjectiveId>> {
        Ok(self
            .persisted
            .logistics_hubs
            .into_iter()
            .fold(Ok::<_, anyhow::Error>(None), |acc, id| {
                let logi = objective!(self, id)?;
                if obj.logistics_detached || logi.owner != obj.owner {
                    acc
                } else {
                    let dist =
                        na::distance_squared(&obj.zone.pos().into(), &logi.zone.pos().into());
                    match acc {
                        Err(e) => Err(e),
                        Ok(None) => Ok(Some((dist, *id))),
                        Ok(Some((pdist, _))) if dist < pdist => Ok(Some((dist, *id))),
                        Ok(Some((dist, id))) => Ok(Some((dist, id))),
                    }
                }
            })?
            .map(|(_, id)| id))
    }

    pub fn setup_supply_lines(&mut self) -> Result<()> {
        let mut suppliers: SmallVec<[(ObjectiveId, Option<ObjectiveId>); 64]> = smallvec![];
        for (oid, obj) in &self.persisted.objectives {
            match obj.kind {
                ObjectiveKind::Logistics | ObjectiveKind::NavalBase | ObjectiveKind::Factory { .. } => (),
                ObjectiveKind::Airbase | ObjectiveKind::Farp { .. } | ObjectiveKind::Fob => {
                    let hub = self.compute_supplier(obj)?;
                    suppliers.push((*oid, hub));
                }
                ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::SpecialSamSite { .. } => (),
            }
        }
        let mut current: FxHashMap<ObjectiveId, SetS<ObjectiveId>> = FxHashMap::default();
        for oid in &self.persisted.logistics_hubs {
            let obj = objective_mut!(self, oid)?;
            current.insert(*oid, mem::take(&mut obj.warehouse.destination));
        }
        for (oid, supplier) in suppliers {
            let obj = objective_mut!(self, oid)?;
            obj.warehouse.supplier = supplier;
            if let Some(id) = supplier {
                objective_mut!(self, id)?
                    .warehouse
                    .destination
                    .insert_cow(oid);
            }
        }

        // Naval Base -> Carrier Group connections
        for nb_id in &self.persisted.naval_bases {
            let nb_obj = objective!(self, nb_id)?;
            let nb_current = nb_obj.warehouse.destination.clone();
            current.insert(*nb_id, nb_current);
        }

        // Collect carrier groups that need connections
        let mut cg_connections: SmallVec<[(ObjectiveId, ObjectiveId); 8]> = smallvec![];
        for (cg_id, cg_obj) in &self.persisted.objectives {
            if let ObjectiveKind::CarrierGroup { parent_naval_base: Some(nb_id), .. } = &cg_obj.kind {
                if cg_obj.owner == objective!(self, nb_id)?.owner {
                    cg_connections.push((*cg_id, *nb_id));
                }
            }
        }

        // Now mutate with collected IDs
        for (cg_id, nb_id) in cg_connections {
            if let Some(nb) = self.persisted.objectives.get_mut_cow(&nb_id) {
                nb.warehouse.destination.insert_cow(cg_id);
            }
            if let Some(cg) = self.persisted.objectives.get_mut_cow(&cg_id) {
                cg.warehouse.supplier = Some(nb_id);
            }
        }

        for (oid, current) in current {
            let obj = objective!(self, oid)?;
            if obj.warehouse.destination != current {
                self.ephemeral.create_objective_markup(&self.persisted, obj)
            }
        }
        Ok(())
    }

    pub fn deliver_production(&mut self, lua: MizLua, now: DateTime<Utc>) -> Result<Vec<Transfer>> {
        if self.ephemeral.cfg.warehouse.is_none() {
            return Ok(vec![]);
        }
        self.setup_supply_lines()
            .context("setting up supply lines")?;
        let mut deliver_produced_supplies = || -> Result<()> {
            for side in Side::ALL {
                let production = match self.ephemeral.production_by_side.get(&side) {
                    Some(e) => e,
                    None => continue,
                };
                for oid in &self.persisted.logistics_hubs {
                    let logi = objective_mut!(self, oid)?;
                    if logi.owner == side {
                        for (name, inv) in logi.warehouse.equipment.iter_mut_cow() {
                            if let Some(eq) = production.equipment.get(name) {
                                *inv += eq.production;
                            }
                        }
                        for (name, inv) in logi.warehouse.liquids.iter_mut_cow() {
                            if let Some(pr) = production.liquids.get(name) {
                                *inv += *pr;
                            }
                        }
                    }
                }
            }
            Ok(())
        };
        deliver_produced_supplies().context("delivering produced supplies")?;
        self.ephemeral.dirty();
        self.deliver_supplies_from_logistics_hubs(lua, now)
            .context("delivering supplies from logistics hubs")
    }

    pub fn sync_vehicle_at_obj(
        &mut self,
        lua: MizLua,
        oid: ObjectiveId,
        typ: Vehicle,
    ) -> Result<()> {
        let obj = objective_mut!(self, oid)?;
        let id = maybe!(self.ephemeral.airbase_by_oid, oid, "airbase")?;
        let wh = Airbase::get_instance(lua, id)
            .context("getting airbase")?
            .get_warehouse()
            .context("getting warehouse")?;
        if let Some(inv) = obj.warehouse.equipment.get_mut_cow(&typ.0) {
            inv.stored = wh.get_item_count(typ.0).context("getting item")?;
            self.ephemeral.dirty();
        }
        Ok(())
    }

    /// Spawn a supply convoy from origin to destination
    fn spawn_supply_convoy(
        &mut self,
        lua: MizLua,
        origin: ObjectiveId,
        destination: ObjectiveId,
        cargo_type: ConvoyCargoType,
        transfers: Vec<Transfer>,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let cfg = match &self.ephemeral.cfg.warehouse {
            Some(w) => w,
            None => return Ok(()),
        };

        let convoy_cfg = match &cfg.convoy {
            Some(c) if c.enabled => c,
            _ => return Ok(()),
        };

        let origin_obj = objective!(self, &origin)?;
        let dest_obj = objective!(self, &destination)?;
        let side = origin_obj.owner;
        let origin_pos = origin_obj.pos();
        let dest_pos = dest_obj.pos();
        let origin_name = origin_obj.name.clone();
        let dest_name = dest_obj.name.clone();

        // Get truck template for this side and clone values we'll need
        let (truck_template, mut speed_kph, trucks_per_convoy) = match convoy_cfg.truck_template.get(&side) {
            Some(t) => (t.clone(), convoy_cfg.speed_kph, convoy_cfg.trucks_per_convoy),
            None => {
                warn!("No truck template configured for side {:?}, skipping convoy spawn", side);
                return Ok(());
            }
        };

        // Apply weather effects to convoy speed if configured
        if let Some(weather_cfg) = self.ephemeral.cfg.weather_effects.as_ref() {
            // Use the most restrictive weather multiplier that's below 1.0
            // (storm < snow < rain). The config author sets which apply.
            let multiplier = weather_cfg.thunderstorm_speed_multiplier
                .min(weather_cfg.snow_speed_multiplier)
                .min(weather_cfg.rain_speed_multiplier);
            if multiplier < 1.0 {
                info!("Applying weather speed multiplier {:.2} to convoy", multiplier);
                speed_kph *= multiplier;
            }
        }

        // Generate unique convoy ID
        let convoy_id = format_compact!(
            "CONVOY_{}_{}_{}",
            side.to_str(),
            self.ephemeral.convoy_counter,
            now.timestamp()
        );
        self.ephemeral.convoy_counter += 1;

        // Calculate heading from origin to destination
        let delta = dest_pos - origin_pos;
        let heading = delta.y.atan2(delta.x);

        // Spawn trucks using existing group spawn infrastructure
        use crate::spawnctx::{SpawnCtx, SpawnLoc};
        use dcso3::group::Group;
        use dcso3::controller::{Task, MissionPoint, PointType, ActionTyp, VehicleFormation, AltType};
        use dcso3::LuaVec2;
        use dcso3::land::Land;
        use dcso3::env::miz::Miz;
        use crate::db::group::DeployKind;
        use enumflags2::BitFlags;

        let spawn_ctx = SpawnCtx::new(lua)?;
        let miz = Miz::singleton(lua)?;
        let idx = miz.index()?;
        let land = Land::singleton(lua)?;

        // Use add_group to spawn the convoy
        let group_id = self.add_group(
            &spawn_ctx,
            &idx,
            side,
            SpawnLoc::AtPos {
                pos: origin_pos,
                offset_direction: Vector2::new(0.0, 0.0),
                group_heading: heading,
            },
            &truck_template,
            DeployKind::Objective { origin },
            BitFlags::empty(),
        )?;

        // Set group to move to destination
        let group = Group::get_by_name(lua, &*self.persisted.groups[&group_id].name)?;
        let controller = group.get_controller()?;
        let origin_alt = land.get_height(LuaVec2(origin_pos))?;
        let dest_alt = land.get_height(LuaVec2(dest_pos))?;

        // Build route using road pathfinding when available
        let speed_mps = speed_kph / 3.6;
        let mut route_points = Vec::new();

        // Start point
        route_points.push(MissionPoint {
            action: Some(ActionTyp::Ground(VehicleFormation::OnRoad)),
            airdrome_id: None,
            helipad: None,
            typ: PointType::TurningPoint,
            link_unit: None,
            pos: LuaVec2(origin_pos),
            alt: origin_alt,
            alt_typ: Some(AltType::BARO),
            time_re_fu_ar: None,
            eta: Some(dcso3::Time(0.)),
            eta_locked: Some(true),
            speed: speed_mps,
            speed_locked: Some(true),
            name: None,
            task: Box::new(Task::ComboTask(vec![])),
        });

        // Try to find road path for intermediate waypoints
        match land.find_path_on_roads(
            dcso3::land::RoadType::Road,
            LuaVec2(origin_pos),
            LuaVec2(dest_pos),
        ) {
            Ok(path) => {
                // Add intermediate road waypoints (skip first/last as they're origin/dest)
                let mut wp_count = 0;
                for wp in path {
                    if let Ok(wp) = wp {
                        let alt = land.get_height(wp).unwrap_or(0.0);
                        route_points.push(MissionPoint {
                            action: Some(ActionTyp::Ground(VehicleFormation::OnRoad)),
                            airdrome_id: None,
                            helipad: None,
                            typ: PointType::TurningPoint,
                            link_unit: None,
                            pos: wp,
                            alt,
                            alt_typ: Some(AltType::BARO),
                            time_re_fu_ar: None,
                            eta: None,
                            eta_locked: None,
                            speed: speed_mps,
                            speed_locked: None,
                            name: None,
                            task: Box::new(Task::ComboTask(vec![])),
                        });
                        wp_count += 1;
                    }
                }
                if wp_count > 0 {
                    info!("Convoy {} using road path with {} waypoints", convoy_id, wp_count);
                }
            }
            Err(e) => {
                debug!("No road path found for convoy {}, using direct route: {}", convoy_id, e);
            }
        }

        // Destination point (always added as final waypoint)
        route_points.push(MissionPoint {
            action: Some(ActionTyp::Ground(VehicleFormation::OnRoad)),
            airdrome_id: None,
            helipad: None,
            typ: PointType::TurningPoint,
            link_unit: None,
            pos: LuaVec2(dest_pos),
            alt: dest_alt,
            alt_typ: Some(AltType::BARO),
            time_re_fu_ar: None,
            eta: None,
            eta_locked: None,
            speed: speed_mps,
            speed_locked: None,
            name: None,
            task: Box::new(Task::ComboTask(vec![])),
        });

        // Create mission with route
        controller.set_task(Task::Mission {
            airborne: Some(false),
            route: route_points,
        })?;

        // Create convoy tracking struct
        let convoy = SupplyConvoy {
            id: convoy_id.clone(),
            group_id: group_id.clone(),
            origin,
            destination,
            cargo_type,
            transfers,
            spawn_time: now,
            state: ConvoyState::InTransit,
            side,
            last_pos: origin_pos,
            last_check: now,
        };

        // Add to tracking
        self.ephemeral.active_convoys.insert(convoy_id.clone(), convoy);
        self.ephemeral.last_convoy_spawn.insert(side, now);

        // Log spawn
        info!(
            "Spawned {} convoy {} from {} to {} with {} trucks",
            cargo_type.as_str(),
            convoy_id,
            origin_name,
            dest_name,
            trucks_per_convoy
        );

        Ok(())
    }

    /// Spawn an AI cargo aircraft to deliver supplies from a logistics hub to a destination
    fn spawn_air_logistics_route(
        &mut self,
        lua: MizLua,
        origin: ObjectiveId,
        destination: ObjectiveId,
        cargo_type: ConvoyCargoType,
        transfers: Vec<Transfer>,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let cfg = match &self.ephemeral.cfg.warehouse {
            Some(w) => w,
            None => return Ok(()),
        };

        let air_cfg = match &cfg.air_logistics {
            Some(c) if c.enabled => c,
            _ => return Ok(()),
        };

        let origin_obj = objective!(self, &origin)?;
        let dest_obj = objective!(self, &destination)?;
        let side = origin_obj.owner;
        let origin_pos = origin_obj.pos();
        let dest_pos = dest_obj.pos();
        let origin_name = origin_obj.name.clone();
        let dest_name = dest_obj.name.clone();

        let (aircraft_template, altitude_m, speed_kph) =
            match air_cfg.aircraft_template.get(&side) {
                Some(t) => (t.clone(), air_cfg.altitude_m, air_cfg.speed_kph),
                None => {
                    warn!(
                        "No aircraft template configured for side {:?}, skipping air route spawn",
                        side
                    );
                    return Ok(());
                }
            };

        let route_id = format_compact!(
            "AIR_{}_{}_{}",
            side.to_str(),
            self.ephemeral.air_route_counter,
            now.timestamp()
        );
        self.ephemeral.air_route_counter += 1;

        let delta = dest_pos - origin_pos;
        let heading = delta.y.atan2(delta.x);
        let speed_mps = speed_kph / 3.6;

        use crate::db::group::DeployKind;
        use crate::spawnctx::{SpawnCtx, SpawnLoc};
        use dcso3::controller::{ActionTyp, AltType, MissionPoint, PointType, Task, TurnMethod};
        use dcso3::env::miz::Miz;
        use dcso3::LuaVec2;
        use enumflags2::BitFlags;

        let spawn_ctx = SpawnCtx::new(lua)?;
        let miz = Miz::singleton(lua)?;
        let idx = miz.index()?;

        let group_id = self.add_group(
            &spawn_ctx,
            &idx,
            side,
            SpawnLoc::InAir {
                pos: origin_pos,
                heading,
                altitude: altitude_m,
                speed: speed_mps,
            },
            &aircraft_template,
            DeployKind::Objective { origin },
            BitFlags::empty(),
        )?;

        use dcso3::group::Group;
        let group = Group::get_by_name(lua, &*self.persisted.groups[&group_id].name)?;
        let controller = group.get_controller()?;

        let route_points = vec![
            MissionPoint {
                action: Some(ActionTyp::Air(TurnMethod::FlyOverPoint)),
                airdrome_id: None,
                helipad: None,
                typ: PointType::TurningPoint,
                link_unit: None,
                pos: LuaVec2(origin_pos),
                alt: altitude_m,
                alt_typ: Some(AltType::BARO),
                time_re_fu_ar: None,
                eta: Some(dcso3::Time(0.)),
                eta_locked: Some(true),
                speed: speed_mps,
                speed_locked: Some(true),
                name: None,
                task: Box::new(Task::ComboTask(vec![])),
            },
            MissionPoint {
                action: Some(ActionTyp::Air(TurnMethod::FlyOverPoint)),
                airdrome_id: None,
                helipad: None,
                typ: PointType::TurningPoint,
                link_unit: None,
                pos: LuaVec2(dest_pos),
                alt: altitude_m,
                alt_typ: Some(AltType::BARO),
                time_re_fu_ar: None,
                eta: None,
                eta_locked: None,
                speed: speed_mps,
                speed_locked: None,
                name: None,
                task: Box::new(Task::ComboTask(vec![])),
            },
        ];

        controller.set_task(Task::Mission {
            airborne: Some(true),
            route: route_points,
        })?;

        let route = AirLogisticsRoute {
            id: route_id.clone(),
            group_id,
            origin,
            destination,
            cargo_type,
            transfers,
            spawn_time: now,
            state: LogiRouteState::InTransit,
            side,
            last_pos: origin_pos,
            last_check: now,
        };

        self.ephemeral.active_air_routes.insert(route_id.clone(), route);
        self.ephemeral.last_air_route_spawn.insert(side, now);

        info!(
            "Spawned {} air logistics route {} from {} to {}",
            cargo_type.as_str(),
            route_id,
            origin_name,
            dest_name
        );

        Ok(())
    }

    /// Spawn an AI ship to deliver supplies from a naval base to a carrier group
    fn spawn_sea_logistics_route(
        &mut self,
        lua: MizLua,
        origin: ObjectiveId,
        destination: ObjectiveId,
        cargo_type: ConvoyCargoType,
        transfers: Vec<Transfer>,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let cfg = match &self.ephemeral.cfg.warehouse {
            Some(w) => w,
            None => return Ok(()),
        };

        let sea_cfg = match &cfg.sea_logistics {
            Some(c) if c.enabled => c,
            _ => return Ok(()),
        };

        let origin_obj = objective!(self, &origin)?;
        let dest_obj = objective!(self, &destination)?;
        let side = origin_obj.owner;
        let origin_pos = origin_obj.pos();
        let dest_pos = dest_obj.pos();
        let origin_name = origin_obj.name.clone();
        let dest_name = dest_obj.name.clone();

        let (ship_template, speed_kph) = match sea_cfg.ship_template.get(&side) {
            Some(t) => (t.clone(), sea_cfg.speed_kph),
            None => {
                warn!(
                    "No ship template configured for side {:?}, skipping sea route spawn",
                    side
                );
                return Ok(());
            }
        };

        let route_id = format_compact!(
            "SEA_{}_{}_{}",
            side.to_str(),
            self.ephemeral.sea_route_counter,
            now.timestamp()
        );
        self.ephemeral.sea_route_counter += 1;

        let delta = dest_pos - origin_pos;
        let heading = delta.y.atan2(delta.x);
        let speed_mps = speed_kph / 3.6;

        use crate::db::group::DeployKind;
        use crate::spawnctx::{SpawnCtx, SpawnLoc};
        use dcso3::controller::{ActionTyp, AltType, MissionPoint, PointType, Task, VehicleFormation};
        use dcso3::env::miz::Miz;
        use dcso3::LuaVec2;
        use enumflags2::BitFlags;

        let spawn_ctx = SpawnCtx::new(lua)?;
        let miz = Miz::singleton(lua)?;
        let idx = miz.index()?;

        let group_id = self.add_group(
            &spawn_ctx,
            &idx,
            side,
            SpawnLoc::AtPos {
                pos: origin_pos,
                offset_direction: Vector2::new(0., 0.),
                group_heading: heading,
            },
            &ship_template,
            DeployKind::Objective { origin },
            BitFlags::empty(),
        )?;

        use dcso3::group::Group;
        let group = Group::get_by_name(lua, &*self.persisted.groups[&group_id].name)?;
        let controller = group.get_controller()?;

        let route_points = vec![
            MissionPoint {
                action: Some(ActionTyp::Ground(VehicleFormation::Vee)),
                airdrome_id: None,
                helipad: None,
                typ: PointType::TurningPoint,
                link_unit: None,
                pos: LuaVec2(origin_pos),
                alt: 0.,
                alt_typ: Some(AltType::BARO),
                time_re_fu_ar: None,
                eta: Some(dcso3::Time(0.)),
                eta_locked: Some(true),
                speed: speed_mps,
                speed_locked: Some(true),
                name: None,
                task: Box::new(Task::ComboTask(vec![])),
            },
            MissionPoint {
                action: Some(ActionTyp::Ground(VehicleFormation::Vee)),
                airdrome_id: None,
                helipad: None,
                typ: PointType::TurningPoint,
                link_unit: None,
                pos: LuaVec2(dest_pos),
                alt: 0.,
                alt_typ: Some(AltType::BARO),
                time_re_fu_ar: None,
                eta: None,
                eta_locked: None,
                speed: speed_mps,
                speed_locked: None,
                name: None,
                task: Box::new(Task::ComboTask(vec![])),
            },
        ];

        controller.set_task(Task::Mission {
            airborne: Some(false),
            route: route_points,
        })?;

        let route = SeaLogisticsRoute {
            id: route_id.clone(),
            group_id,
            origin,
            destination,
            cargo_type,
            transfers,
            spawn_time: now,
            state: LogiRouteState::InTransit,
            side,
            last_pos: origin_pos,
            last_check: now,
        };

        self.ephemeral.active_sea_routes.insert(route_id.clone(), route);
        self.ephemeral.last_sea_route_spawn.insert(side, now);

        info!(
            "Spawned {} sea logistics route {} from {} to {}",
            cargo_type.as_str(),
            route_id,
            origin_name,
            dest_name
        );

        Ok(())
    }

    pub fn deliver_supplies_from_logistics_hubs(&mut self, lua: MizLua, now: DateTime<Utc>) -> Result<Vec<Transfer>> {
        self.update_supply_status()
            .context("updating supply status")?;
        let mut transfers: Vec<Transfer> = vec![];

        // Check which transport modes are enabled
        let convoy_enabled = self.ephemeral.cfg.warehouse
            .as_ref()
            .and_then(|w| w.convoy.as_ref())
            .map(|c| c.enabled)
            .unwrap_or(false);

        let air_enabled = self.ephemeral.cfg.warehouse
            .as_ref()
            .and_then(|w| w.air_logistics.as_ref())
            .map(|a| a.enabled)
            .unwrap_or(false);

        let sea_enabled = self.ephemeral.cfg.warehouse
            .as_ref()
            .and_then(|w| w.sea_logistics.as_ref())
            .map(|s| s.enabled)
            .unwrap_or(false);

        // Collect hub IDs to avoid borrowing issues
        let hub_ids: SmallVec<[ObjectiveId; 16]> = self.persisted.logistics_hubs.into_iter().copied().collect();

        // Collect spawn info to execute after we're done with objective references
        struct RouteSpawnInfo {
            origin: ObjectiveId,
            destination: ObjectiveId,
            cargo_type: ConvoyCargoType,
            transfers: Vec<Transfer>,
        }
        let mut convoys_to_spawn: Vec<RouteSpawnInfo> = Vec::new();
        let mut air_routes_to_spawn: Vec<RouteSpawnInfo> = Vec::new();

        for lid in hub_ids {
            let logi = objective!(self, &lid)?;
            let hub_side = logi.owner;

            // Split destinations into instant transfer, convoy, or air route
            let mut instant_needed: SmallVec<[Needed; 64]> = SmallVec::new();
            let mut convoy_needed: SmallVec<[Needed; 64]> = SmallVec::new();
            let mut air_needed: SmallVec<[Needed; 64]> = SmallVec::new();

            // Check air route throttle for this hub's side
            let air_supply_threshold = self.ephemeral.cfg.warehouse
                .as_ref()
                .and_then(|w| w.air_logistics.as_ref())
                .map(|a| a.supply_threshold)
                .unwrap_or(50);
            let air_max_concurrent = self.ephemeral.cfg.warehouse
                .as_ref()
                .and_then(|w| w.air_logistics.as_ref())
                .map(|a| a.max_concurrent_routes as usize)
                .unwrap_or(6);
            let air_spawn_interval_ticks = self.ephemeral.cfg.warehouse
                .as_ref()
                .and_then(|w| w.air_logistics.as_ref())
                .map(|a| a.spawn_interval_ticks)
                .unwrap_or(3);
            let tick_minutes = self.ephemeral.cfg.warehouse
                .as_ref()
                .map(|w| w.tick)
                .unwrap_or(10);
            let air_active_count = self.ephemeral.active_air_routes.values()
                .filter(|r| r.side == hub_side)
                .count();
            let air_last_spawn = self.ephemeral.last_air_route_spawn.get(&hub_side).copied();
            let air_spawn_interval = Duration::minutes(tick_minutes as i64 * air_spawn_interval_ticks as i64);
            let air_can_spawn = air_enabled
                && air_active_count < air_max_concurrent
                && air_last_spawn.map(|t| now - t >= air_spawn_interval).unwrap_or(true);

            for oid in logi.warehouse.destination.into_iter() {
                if let Some(obj) = self.persisted.objectives.get(oid) {
                    if logi.owner == obj.owner && (obj.supply < 100 || obj.fuel < 100) {
                        let needed = Needed {
                            oid,
                            obj,
                            demanded: 0,
                            allocated: 0,
                        };

                        // Route to appropriate transport mode
                        if convoy_enabled && obj.logistics_detached {
                            convoy_needed.push(needed);
                        } else if air_can_spawn && (obj.supply < air_supply_threshold || obj.fuel < air_supply_threshold) {
                            // Air eligible: below threshold and slot available
                            air_needed.push(needed);
                        } else {
                            // Instant transfer: air disabled, throttled, or above threshold
                            instant_needed.push(needed);
                        }
                    }
                }
            }

            let mut needed = instant_needed;
            macro_rules! schedule_transfers {
                ($typ:expr, $from:ident, $get:ident) => {
                    for (name, inv) in &logi.warehouse.$from {
                        if inv.stored == 0 {
                            continue;
                        }
                        needed.sort_by(|n0, n1| {
                            let i0 = n0.obj.$get(name);
                            let i1 = n1.obj.$get(name);
                            i0.stored.cmp(&i1.stored)
                        });
                        let mut total_demanded = 0;
                        for n in &mut needed {
                            let inv = n.obj.$get(name);
                            let demanded = if inv.stored <= inv.capacity {
                                inv.capacity - inv.stored
                            } else {
                                0
                            };
                            total_demanded += demanded;
                            n.demanded = demanded;
                            n.allocated = 0;
                        }
                        let mut have = inv.stored;
                        let mut total_filled = 0;
                        while have > 0 && total_filled < total_demanded {
                            for n in &mut needed {
                                if have == 0 {
                                    break;
                                }
                                let allocation = max(1, have >> 3);
                                let amount = min(allocation, n.demanded - n.allocated);
                                n.allocated += amount;
                                total_filled += amount;
                                have -= amount;
                            }
                        }
                        for n in &needed {
                            if n.allocated > 0 {
                                transfers.push(Transfer {
                                    source: lid,
                                    target: *n.oid,
                                    amount: n.allocated,
                                    item: $typ(name.clone()),
                                })
                            }
                        }
                    }
                };
            }
            schedule_transfers!(TransferItem::Equipment, equipment, get_equipment);
            schedule_transfers!(TransferItem::Liquid, liquids, get_liquids);

            // Now handle convoy-required destinations
            if !convoy_needed.is_empty() {
                // Group transfers by destination for convoy spawning
                // We'll create separate convoys for fuel and weapons
                let mut convoy_transfers_by_dest: FxHashMap<ObjectiveId, (Vec<Transfer>, Vec<Transfer>)> = FxHashMap::default();

                let mut needed = convoy_needed;
                // Schedule fuel transfers (for convoys)
                for (name, inv) in &logi.warehouse.liquids {
                    if inv.stored == 0 {
                        continue;
                    }
                    needed.sort_by(|n0, n1| {
                        let i0 = n0.obj.get_liquids(name);
                        let i1 = n1.obj.get_liquids(name);
                        i0.stored.cmp(&i1.stored)
                    });
                    let mut total_demanded = 0;
                    for n in &mut needed {
                        let inv = n.obj.get_liquids(name);
                        let demanded = if inv.stored <= inv.capacity {
                            inv.capacity - inv.stored
                        } else {
                            0
                        };
                        total_demanded += demanded;
                        n.demanded = demanded;
                        n.allocated = 0;
                    }
                    let mut have = inv.stored;
                    let mut total_filled = 0;
                    while have > 0 && total_filled < total_demanded {
                        for n in &mut needed {
                            if have == 0 {
                                break;
                            }
                            let allocation = max(1, have >> 3);
                            let amount = min(allocation, n.demanded - n.allocated);
                            n.allocated += amount;
                            total_filled += amount;
                            have -= amount;
                        }
                    }
                    for n in &needed {
                        if n.allocated > 0 {
                            let tr = Transfer {
                                source: lid,
                                target: *n.oid,
                                amount: n.allocated,
                                item: TransferItem::Liquid(name.clone()),
                            };
                            convoy_transfers_by_dest.entry(*n.oid).or_default().1.push(tr);
                        }
                    }
                }

                // Schedule equipment transfers (for convoys)
                for (name, inv) in &logi.warehouse.equipment {
                    if inv.stored == 0 {
                        continue;
                    }
                    needed.sort_by(|n0, n1| {
                        let i0 = n0.obj.get_equipment(name);
                        let i1 = n1.obj.get_equipment(name);
                        i0.stored.cmp(&i1.stored)
                    });
                    let mut total_demanded = 0;
                    for n in &mut needed {
                        let inv = n.obj.get_equipment(name);
                        let demanded = if inv.stored <= inv.capacity {
                            inv.capacity - inv.stored
                        } else {
                            0
                        };
                        total_demanded += demanded;
                        n.demanded = demanded;
                        n.allocated = 0;
                    }
                    let mut have = inv.stored;
                    let mut total_filled = 0;
                    while have > 0 && total_filled < total_demanded {
                        for n in &mut needed {
                            if have == 0 {
                                break;
                            }
                            let allocation = max(1, have >> 3);
                            let amount = min(allocation, n.demanded - n.allocated);
                            n.allocated += amount;
                            total_filled += amount;
                            have -= amount;
                        }
                    }
                    for n in &needed {
                        if n.allocated > 0 {
                            let tr = Transfer {
                                source: lid,
                                target: *n.oid,
                                amount: n.allocated,
                                item: TransferItem::Equipment(name.clone()),
                            };
                            convoy_transfers_by_dest.entry(*n.oid).or_default().0.push(tr);
                        }
                    }
                }

                // Collect convoy spawn info (don't spawn yet to avoid borrowing conflicts)
                for (dest_oid, (equipment_transfers, fuel_transfers)) in convoy_transfers_by_dest {
                    // Add weapons convoy if there are equipment transfers
                    if !equipment_transfers.is_empty() {
                        convoys_to_spawn.push(RouteSpawnInfo {
                            origin: lid,
                            destination: dest_oid,
                            cargo_type: ConvoyCargoType::Weapons,
                            transfers: equipment_transfers,
                        });
                    }

                    // Add fuel convoy if there are fuel transfers
                    if !fuel_transfers.is_empty() {
                        convoys_to_spawn.push(RouteSpawnInfo {
                            origin: lid,
                            destination: dest_oid,
                            cargo_type: ConvoyCargoType::Fuel,
                            transfers: fuel_transfers,
                        });
                    }
                }
            }

            // Schedule air logistics routes for air-eligible destinations
            if !air_needed.is_empty() {
                let mut air_transfers_by_dest: FxHashMap<ObjectiveId, (Vec<Transfer>, Vec<Transfer>)> =
                    FxHashMap::default();

                let mut needed = air_needed;
                for (name, inv) in &logi.warehouse.liquids {
                    if inv.stored == 0 {
                        continue;
                    }
                    needed.sort_by(|n0, n1| {
                        n0.obj.get_liquids(name).stored.cmp(&n1.obj.get_liquids(name).stored)
                    });
                    let mut total_demanded = 0;
                    for n in &mut needed {
                        let inv = n.obj.get_liquids(name);
                        let demanded =
                            if inv.stored <= inv.capacity { inv.capacity - inv.stored } else { 0 };
                        total_demanded += demanded;
                        n.demanded = demanded;
                        n.allocated = 0;
                    }
                    let mut have = inv.stored;
                    let mut total_filled = 0;
                    while have > 0 && total_filled < total_demanded {
                        for n in &mut needed {
                            if have == 0 { break; }
                            let allocation = max(1, have >> 3);
                            let amount = min(allocation, n.demanded - n.allocated);
                            n.allocated += amount;
                            total_filled += amount;
                            have -= amount;
                        }
                    }
                    for n in &needed {
                        if n.allocated > 0 {
                            air_transfers_by_dest.entry(*n.oid).or_default().1.push(Transfer {
                                source: lid,
                                target: *n.oid,
                                amount: n.allocated,
                                item: TransferItem::Liquid(name.clone()),
                            });
                        }
                    }
                }
                for (name, inv) in &logi.warehouse.equipment {
                    if inv.stored == 0 {
                        continue;
                    }
                    needed.sort_by(|n0, n1| {
                        n0.obj.get_equipment(name).stored.cmp(&n1.obj.get_equipment(name).stored)
                    });
                    let mut total_demanded = 0;
                    for n in &mut needed {
                        let inv = n.obj.get_equipment(name);
                        let demanded =
                            if inv.stored <= inv.capacity { inv.capacity - inv.stored } else { 0 };
                        total_demanded += demanded;
                        n.demanded = demanded;
                        n.allocated = 0;
                    }
                    let mut have = inv.stored;
                    let mut total_filled = 0;
                    while have > 0 && total_filled < total_demanded {
                        for n in &mut needed {
                            if have == 0 { break; }
                            let allocation = max(1, have >> 3);
                            let amount = min(allocation, n.demanded - n.allocated);
                            n.allocated += amount;
                            total_filled += amount;
                            have -= amount;
                        }
                    }
                    for n in &needed {
                        if n.allocated > 0 {
                            air_transfers_by_dest.entry(*n.oid).or_default().0.push(Transfer {
                                source: lid,
                                target: *n.oid,
                                amount: n.allocated,
                                item: TransferItem::Equipment(name.clone()),
                            });
                        }
                    }
                }

                for (dest_oid, (equipment_transfers, fuel_transfers)) in air_transfers_by_dest {
                    if !equipment_transfers.is_empty() {
                        air_routes_to_spawn.push(RouteSpawnInfo {
                            origin: lid,
                            destination: dest_oid,
                            cargo_type: ConvoyCargoType::Weapons,
                            transfers: equipment_transfers,
                        });
                    }
                    if !fuel_transfers.is_empty() {
                        air_routes_to_spawn.push(RouteSpawnInfo {
                            origin: lid,
                            destination: dest_oid,
                            cargo_type: ConvoyCargoType::Fuel,
                            transfers: fuel_transfers,
                        });
                    }
                }
            }
        }

        // Spawn all collected convoys (after we're done with objective references)
        for route_info in convoys_to_spawn {
            // Deduct supplies from source immediately (convoy takes them)
            for tr in &route_info.transfers {
                if let Err(e) = tr.execute(&mut self.persisted, &self.ephemeral.to_bg) {
                    error!("Failed to deduct supplies for convoy: {:?}", e);
                }
            }
            if let Err(e) = self.spawn_supply_convoy(
                lua,
                route_info.origin,
                route_info.destination,
                route_info.cargo_type,
                route_info.transfers,
                now,
            ) {
                error!("Failed to spawn {:?} convoy: {:?}", route_info.cargo_type, e);
            }
        }

        // Spawn all collected air routes
        for route_info in air_routes_to_spawn {
            for tr in &route_info.transfers {
                if let Err(e) = tr.execute(&mut self.persisted, &self.ephemeral.to_bg) {
                    error!("Failed to deduct supplies for air route: {:?}", e);
                }
            }
            if let Err(e) = self.spawn_air_logistics_route(
                lua,
                route_info.origin,
                route_info.destination,
                route_info.cargo_type,
                route_info.transfers,
                now,
            ) {
                error!("Failed to spawn {:?} air route: {:?}", route_info.cargo_type, e);
            }
        }

        // Dispatch sea logistics routes: NavalBase → CarrierGroup
        if sea_enabled {
            let sea_supply_threshold = self.ephemeral.cfg.warehouse
                .as_ref()
                .and_then(|w| w.sea_logistics.as_ref())
                .map(|s| s.supply_threshold)
                .unwrap_or(50);
            let sea_max_concurrent = self.ephemeral.cfg.warehouse
                .as_ref()
                .and_then(|w| w.sea_logistics.as_ref())
                .map(|s| s.max_concurrent_routes as usize)
                .unwrap_or(4);
            let sea_spawn_interval_ticks = self.ephemeral.cfg.warehouse
                .as_ref()
                .and_then(|w| w.sea_logistics.as_ref())
                .map(|s| s.spawn_interval_ticks)
                .unwrap_or(3);
            let tick_minutes = self.ephemeral.cfg.warehouse
                .as_ref()
                .map(|w| w.tick)
                .unwrap_or(10);

            // Collect naval base → carrier group candidate pairs
            // First pass: collect (nb_id, side, candidate_dest_ids) without nested borrow
            let naval_hubs: Vec<(ObjectiveId, Side, Vec<ObjectiveId>)> = self
                .persisted
                .objectives
                .into_iter()
                .filter_map(|(nb_id, nb_obj)| {
                    if !matches!(nb_obj.kind, ObjectiveKind::NavalBase) {
                        return None;
                    }
                    let side = nb_obj.owner;
                    if side == Side::Neutral {
                        return None;
                    }
                    let dest_ids: Vec<ObjectiveId> =
                        nb_obj.warehouse.destination.into_iter().copied().collect();
                    Some((*nb_id, side, dest_ids))
                })
                .collect();

            // Second pass: filter destinations to carrier groups below threshold
            let mut naval_pairs: Vec<(ObjectiveId, ObjectiveId, Side)> = Vec::new();
            for (nb_id, side, dest_ids) in &naval_hubs {
                for dest_id in dest_ids {
                    let dest = match self.persisted.objectives.get(dest_id) {
                        Some(o) => o,
                        None => continue,
                    };
                    if !matches!(dest.kind, ObjectiveKind::CarrierGroup { .. }) {
                        continue;
                    }
                    if dest.owner != *side {
                        continue;
                    }
                    if dest.supply >= sea_supply_threshold && dest.fuel >= sea_supply_threshold {
                        continue;
                    }
                    naval_pairs.push((*nb_id, *dest_id, *side));
                }
            }

            let mut sea_routes_to_spawn: Vec<RouteSpawnInfo> = Vec::new();
            for (nb_id, dest_id, side) in naval_pairs {
                let sea_active_count = self.ephemeral.active_sea_routes.values()
                    .filter(|r| r.side == side)
                    .count();
                let sea_last_spawn = self.ephemeral.last_sea_route_spawn.get(&side).copied();
                let sea_spawn_interval = Duration::minutes(
                    tick_minutes as i64 * sea_spawn_interval_ticks as i64,
                );
                let can_spawn = sea_active_count < sea_max_concurrent
                    && sea_last_spawn
                        .map(|t| now - t >= sea_spawn_interval)
                        .unwrap_or(true);
                if !can_spawn {
                    continue;
                }

                // Already have an active route for this pair?
                let already_active = self.ephemeral.active_sea_routes.values()
                    .any(|r| r.origin == nb_id && r.destination == dest_id);
                if already_active {
                    continue;
                }

                let nb_obj = match self.persisted.objectives.get(&nb_id) {
                    Some(o) => o,
                    None => continue,
                };
                let dest_obj = match self.persisted.objectives.get(&dest_id) {
                    Some(o) => o,
                    None => continue,
                };

                // Build transfers
                let mut fuel_transfers: Vec<Transfer> = Vec::new();
                let mut equip_transfers: Vec<Transfer> = Vec::new();
                for (name, inv) in &nb_obj.warehouse.liquids {
                    let dest_inv = dest_obj.get_liquids(name);
                    if inv.stored > 0 && dest_inv.stored < dest_inv.capacity {
                        let amount = min(inv.stored, dest_inv.capacity - dest_inv.stored);
                        fuel_transfers.push(Transfer {
                            source: nb_id,
                            target: dest_id,
                            amount,
                            item: TransferItem::Liquid(name.clone()),
                        });
                    }
                }
                for (name, inv) in &nb_obj.warehouse.equipment {
                    let dest_inv = dest_obj.get_equipment(name);
                    if inv.stored > 0 && dest_inv.stored < dest_inv.capacity {
                        let amount = min(inv.stored, dest_inv.capacity - dest_inv.stored);
                        equip_transfers.push(Transfer {
                            source: nb_id,
                            target: dest_id,
                            amount,
                            item: TransferItem::Equipment(name.clone()),
                        });
                    }
                }

                if !equip_transfers.is_empty() {
                    sea_routes_to_spawn.push(RouteSpawnInfo {
                        origin: nb_id,
                        destination: dest_id,
                        cargo_type: ConvoyCargoType::Weapons,
                        transfers: equip_transfers,
                    });
                }
                if !fuel_transfers.is_empty() {
                    sea_routes_to_spawn.push(RouteSpawnInfo {
                        origin: nb_id,
                        destination: dest_id,
                        cargo_type: ConvoyCargoType::Fuel,
                        transfers: fuel_transfers,
                    });
                }
            }

            for route_info in sea_routes_to_spawn {
                for tr in &route_info.transfers {
                    if let Err(e) = tr.execute(&mut self.persisted, &self.ephemeral.to_bg) {
                        error!("Failed to deduct supplies for sea route: {:?}", e);
                    }
                }
                if let Err(e) = self.spawn_sea_logistics_route(
                    lua,
                    route_info.origin,
                    route_info.destination,
                    route_info.cargo_type,
                    route_info.transfers,
                    now,
                ) {
                    error!("Failed to spawn {:?} sea route: {:?}", route_info.cargo_type, e);
                }
            }
        }

        Ok(transfers)
    }

    pub fn run_factory_production(&mut self, now: DateTime<Utc>) -> Result<()> {
        let cfg = match &self.ephemeral.cfg.factory {
            Some(c) => c,
            None => return Ok(()),
        };

        for (_, obj) in self.persisted.objectives.iter_mut_cow() {
            if let ObjectiveKind::Factory { production_rate, last_production_ts } = &mut obj.kind {
                // Only produce if operational: health > 0, logi > 0, not neutral
                if obj.health > 0 && obj.logi > 0 && obj.owner != Side::Neutral {
                    let should_produce = last_production_ts
                        .map(|ts| now - ts >= Duration::seconds(cfg.production_interval as i64))
                        .unwrap_or(true);

                    if should_produce {
                        // Add to generic equipment inventory
                        if let Some(inv) = obj.warehouse.equipment.get_mut_cow("SUPPLIES") {
                            inv.stored += *production_rate;
                        } else {
                            obj.warehouse.equipment.insert_cow(
                                "SUPPLIES".into(),
                                Inventory {
                                    stored: *production_rate,
                                    capacity: u32::MAX,
                                },
                            );
                        }
                        *last_production_ts = Some(now);
                    }
                }
            }
        }
        Ok(())
    }

    fn balance_logistics_hubs(&mut self) -> Result<()> {
        struct Needed<'a> {
            oid: &'a ObjectiveId,
            obj: &'a Objective,
            had: u32,
            have: u32,
        }
        for side in Side::ALL {
            let mut transfers: Vec<Transfer> = vec![];
            macro_rules! schedule_transfers {
                ($typ:expr, $from:ident, $get:ident) => {{
                    let mut needed: SmallVec<[Needed; 16]> = self
                        .persisted
                        .logistics_hubs
                        .into_iter()
                        .filter_map(|lid| {
                            let obj = &self.persisted.objectives[lid];
                            if obj.owner != side {
                                None
                            } else {
                                Some(Needed {
                                    oid: lid,
                                    obj,
                                    had: 0,
                                    have: 0,
                                })
                            }
                        })
                        .collect();
                    if needed.len() < 2 {
                        continue;
                    }
                    let items = needed[0].obj.warehouse.$from.clone();
                    for (name, _) in &items {
                        let mean = {
                            let sum: u32 = needed
                                .iter_mut()
                                .map(|n| {
                                    n.have = n.obj.$get(name).stored;
                                    n.had = n.have;
                                    n.had
                                })
                                .sum();
                            sum / needed.len() as u32
                        };
                        if mean >> 2 == 0 {
                            continue;
                        }
                        needed.sort_by(|n0, n1| n0.had.cmp(&n1.had));
                        let mut take = needed.len() - 1;
                        for i in 0..needed.len() {
                            if needed[i].have + 1 >= mean {
                                break;
                            }
                            while needed[i].have + 1 < mean {
                                while take > i && needed[take].have <= mean {
                                    take -= 1;
                                }
                                if take == i {
                                    break;
                                }
                                let need = mean - needed[i].have;
                                let available = needed[take].have - mean;
                                let xfer = min(need, available);
                                needed[i].have += xfer;
                                needed[take].have -= xfer;
                                transfers.push(Transfer {
                                    source: *needed[take].oid,
                                    target: *needed[i].oid,
                                    amount: xfer,
                                    item: $typ(name.clone()),
                                });
                            }
                        }
                    }
                }};
            }
            schedule_transfers!(TransferItem::Equipment, equipment, get_equipment);
            schedule_transfers!(TransferItem::Liquid, liquids, get_liquids);
            for tr in transfers.drain(..) {
                tr.execute(&mut self.persisted, &self.ephemeral.to_bg)
                    .with_context(|| format_compact!("executing transfer {:?}", tr))?
            }
            self.ephemeral.dirty();
        }
        self.update_supply_status()?;
        Ok(())
    }

    fn update_supply_status(&mut self) -> Result<()> {
        for (_, obj) in self.persisted.objectives.iter_mut_cow() {
            let current_supply = obj.supply;
            let current_fuel = obj.fuel;
            let mut n = 0;
            let mut sum: u32 = 0;
            for (_, inv) in &obj.warehouse.equipment {
                if let Some(pct) = inv.percent() {
                    sum += pct as u32;
                    n += 1;
                }
            }
            obj.supply = if n == 0 { 0 } else { (sum / n) as u8 };
            n = 0;
            sum = 0;
            for (_, inv) in &obj.warehouse.liquids {
                if let Some(pct) = inv.percent() {
                    sum += pct as u32;
                    n += 1;
                }
            }
            obj.fuel = if n == 0 { 0 } else { (sum / n) as u8 };
            if current_supply != obj.supply || current_fuel != obj.fuel {
                self.ephemeral.stat(Stat::ObjectiveSupply {
                    id: obj.id,
                    supply: obj.supply,
                    fuel: obj.fuel,
                });
            }
        }
        self.ephemeral.dirty();
        Ok(())
    }

    pub fn sync_warehouse_to_objective<'lua>(
        &mut self,
        lua: MizLua<'lua>,
        oid: ObjectiveId,
    ) -> Result<(&mut Objective, warehouse::Warehouse<'lua>)> {
        let obj = objective_mut!(self, oid)?;
        let airbase = self
            .ephemeral
            .airbase_by_oid
            .get(&oid)
            .ok_or_else(|| anyhow!("no logistics for objective {}", obj.name))?;
        let warehouse = Airbase::get_instance(lua, &airbase)
            .context("getting airbase")?
            .get_warehouse()
            .context("getting warehouse")?;
        sync_warehouse_to_obj(obj, &warehouse).context("syncing warehouse to objective")?;
        Ok((obj, warehouse))
    }

    pub fn sync_objective_to_warehouse<'lua>(
        &mut self,
        lua: MizLua<'lua>,
        oid: ObjectiveId,
    ) -> Result<(&mut Objective, warehouse::Warehouse<'lua>)> {
        let obj = objective_mut!(self, oid)?;
        let airbase = self
            .ephemeral
            .airbase_by_oid
            .get(&oid)
            .ok_or_else(|| anyhow!("no logistics for objective {}", obj.name))?;
        let warehouse = Airbase::get_instance(lua, &airbase)
            .context("getting airbase")?
            .get_warehouse()
            .context("getting warehouse")?;
        sync_obj_to_warehouse(obj, &warehouse).context("syncing warehouse to objective")?;
        Ok((obj, warehouse))
    }

    pub fn transfer_supplies(
        &mut self,
        lua: MizLua,
        from: ObjectiveId,
        to: ObjectiveId,
    ) -> Result<()> {
        if from == to {
            bail!("you can't transfer supplies to the same objective")
        }
        let (size, transfer_size_percent) = match self.ephemeral.cfg.warehouse.as_ref() {
            Some(whcfg) => (whcfg.supply_transfer_size as f32 / 100., whcfg.supply_transfer_size),
            None => return Ok(()),
        };
        let side = objective!(self, from)?.owner;
        if side != objective!(self, to)?.owner {
            bail!("can't transfer supply from an enemy objective")
        }
        let mut transfers: SmallVec<[Transfer; 128]> = smallvec![];
        let (_, from_wh) = self
            .sync_warehouse_to_objective(lua, from)
            .context("syncing from objective")?;
        let (_, to_wh) = self
            .sync_warehouse_to_objective(lua, to)
            .context("syncing to objective")?;
        let from_obj = objective!(self, from)?;
        let to_obj = objective!(self, to)?;

        debug!("[SUPPLY_TRANSFER] Starting transfer from {:?} to {:?}, size: {}%", from, to, transfer_size_percent);

        // Transfer all equipment EXCEPT airframes
        // Airframes don't have prefixes like "weapons.", "vehicles." - they're just aircraft type names
        let exempt_airframes = self.ephemeral.cfg.warehouse
            .as_ref()
            .map(|wh| &wh.exempt_airframes)
            .cloned()
            .unwrap_or_default();

        for (name, inv) in &from_obj.warehouse.equipment {
            // Skip airframes - they should never be transferred via supply crates
            // Airframes don't have prefixes like "weapons.", "vehicles." - they're just aircraft type names
            let is_airframe = !name.starts_with("weapons.")
                && !name.starts_with("vehicles.")
                && !name.starts_with("Fortifications.");

            if is_airframe || exempt_airframes.contains(name.as_str()) {
                debug!("[SUPPLY_TRANSFER] Skipping airframe: {} (stored: {})", name, inv.stored);
                continue;
            }

            // Transfer everything else (weapons, vehicles, deployables, etc.)
            if inv.stored > 0 {
                // Calculate how much the destination can accept
                let needed = match to_obj.warehouse.equipment.get(name) {
                    // If destination doesn't have this equipment type, use source capacity as template
                    None => {
                        let amount = max(1, (inv.stored as f32 * size) as u32);
                        debug!("[SUPPLY_TRANSFER] Transferring equipment (new): {} amount: {} (from stored: {}, dest has no capacity - will initialize)",
                            name, amount, inv.stored);
                        transfers.push(Transfer {
                            amount,
                            source: from,
                            target: to,
                            item: TransferItem::Equipment(name.clone()),
                        });
                        continue;
                    }
                    Some(dest_inv) => {
                        // If destination has 0 capacity, initialize it from source
                        if dest_inv.capacity == 0 {
                            let amount = max(1, (inv.stored as f32 * size) as u32);
                            debug!("[SUPPLY_TRANSFER] Transferring equipment (init capacity): {} amount: {} (from stored: {}, dest capacity=0 - will initialize from source capacity={})",
                                name, amount, inv.stored, inv.capacity);
                            transfers.push(Transfer {
                                amount,
                                source: from,
                                target: to,
                                item: TransferItem::Equipment(name.clone()),
                            });
                            continue;
                        }
                        // Normal case: destination has capacity
                        if dest_inv.capacity >= dest_inv.stored {
                            dest_inv.capacity - dest_inv.stored
                        } else {
                            0
                        }
                    }
                };
                let amount = min(needed, max(1, (inv.stored as f32 * size) as u32));
                if amount > 0 {
                    debug!("[SUPPLY_TRANSFER] Transferring equipment: {} amount: {} (from stored: {}, dest needed: {})",
                        name, amount, inv.stored, needed);
                    transfers.push(Transfer {
                        amount,
                        source: from,
                        target: to,
                        item: TransferItem::Equipment(name.clone()),
                    });
                } else {
                    debug!("[SUPPLY_TRANSFER] Skipping {}: destination full or no need (from stored: {}, dest needed: {})",
                        name, inv.stored, needed);
                }
            }
        }

        // Transfer all liquids (fuel)
        for (name, inv) in &from_obj.warehouse.liquids {
            if inv.stored > 0 {
                let needed = match to_obj.warehouse.liquids.get(name) {
                    // If destination doesn't have this liquid type, transfer based on source inventory
                    None => inv.stored,
                    Some(dest_inv) => {
                        if dest_inv.capacity >= dest_inv.stored {
                            dest_inv.capacity - dest_inv.stored
                        } else {
                            0
                        }
                    }
                };
                let amount = min(needed, max(1, (inv.stored as f32 * size) as u32));
                if amount > 0 {
                    debug!("[SUPPLY_TRANSFER] Transferring liquid: {:?} amount: {} (from stored: {}, dest needed: {})",
                        name, amount, inv.stored, needed);
                    transfers.push(Transfer {
                        amount,
                        source: from,
                        target: to,
                        item: TransferItem::Liquid(*name),
                    });
                }
            }
        }

        debug!("[SUPPLY_TRANSFER] Total transfers queued: {}", transfers.len());
        for tr in transfers {
            tr.execute(&mut self.persisted, &self.ephemeral.to_bg)?
        }
        sync_obj_to_warehouse(objective!(self, from)?, &from_wh)?;
        sync_obj_to_warehouse(objective!(self, to)?, &to_wh)?;
        self.update_supply_status()
            .context("updating supply status")?;
        self.ephemeral.dirty();
        Ok(())
    }

    pub fn admin_reduce_inventory(
        &mut self,
        lua: MizLua,
        oid: ObjectiveId,
        amount: u8,
    ) -> Result<()> {
        if amount > 100 {
            bail!("enter a percentage")
        }
        let percent = amount as f32 / 100.;
        let production = match self
            .ephemeral
            .production_by_side
            .get(&objective!(self, oid)?.owner)
        {
            Some(p) => Arc::clone(p),
            None => return Ok(()),
        };
        let (obj, warehouse) = self
            .sync_warehouse_to_objective(lua, oid)
            .with_context(|| format_compact!("syncing warehouses to {oid}"))?;
        for name in production.equipment.keys() {
            if let Some(inv) = obj.warehouse.equipment.get_mut_cow(name) {
                inv.reduce(percent);
            }
        }
        for liq in production.liquids.keys() {
            if let Some(inv) = obj.warehouse.liquids.get_mut_cow(&liq) {
                inv.reduce(percent);
            }
        }
        sync_obj_to_warehouse(obj, &warehouse).context("syncing from warehouse")?;
        self.update_supply_status()
            .context("updating supply status")?;
        self.ephemeral.dirty();
        Ok(())
    }

    pub fn admin_log_inventory(
        &mut self,
        lua: MizLua,
        kind: WarehouseKind,
        oid: ObjectiveId,
    ) -> Result<()> {
        use std::fmt::Write;
        match kind {
            WarehouseKind::DCS => {
                let abid = self
                    .ephemeral
                    .airbase_by_oid
                    .get(&oid)
                    .ok_or_else(|| anyhow!("no airbase for {oid}"))?;
                let wh = Airbase::get_instance(lua, &abid)
                    .context("getting airbase")?
                    .get_warehouse()
                    .context("getting warehouse")?;
                let map =
                    warehouse::Warehouse::get_resource_map(lua).context("getting resource map")?;
                let mut msg = CompactString::new("");
                map.for_each(|name, _| {
                    let qty = wh
                        .get_item_count(name.clone())
                        .with_context(|| format_compact!("getting {name} count from warehouse"))?;
                    if qty > 0 {
                        write!(msg, "{name}, {qty}\n")?
                    }
                    Ok(())
                })?;
                for name in LiquidType::ALL {
                    let qty = wh.get_liquid_amount(name).with_context(|| {
                        format_compact!("getting liquid {:?} from warehouse", name)
                    })?;
                    if qty > 0 {
                        write!(msg, "{:?}, {qty}\n", name)?
                    }
                }
                warn!("{msg}")
            }
            WarehouseKind::Objective => {
                let obj = objective!(self, oid)?;
                let mut msg = CompactString::new("");
                for (name, inv) in &obj.warehouse.equipment {
                    write!(msg, "{name}, {}/{}\n", inv.stored, inv.capacity)?
                }
                for (name, inv) in &obj.warehouse.liquids {
                    write!(msg, "{:?}, {}/{}\n", name, inv.stored, inv.capacity)?
                }
                warn!("{msg}")
            }
        }
        Ok(())
    }
}
