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
    ManageConvoys {
        convoys: Vec<SupplyConvoy>,
    },
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
}

impl ConvoyCargoType {
    pub fn as_str(&self) -> &'static str {
        match self {
            ConvoyCargoType::Fuel => "fuel",
            ConvoyCargoType::Weapons => "weapons",
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

pub(super) fn sync_obj_to_warehouse(obj: &Objective, warehouse: &warehouse::Warehouse) -> Result<()> {
    let perf = unsafe { Perf::get_mut() };
    let perf = Arc::make_mut(&mut perf.inner);
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
            info!("[WAREHOUSE] Resource map initialized. Sides with production: {:?}",
                  self.ephemeral.production_by_side.keys().collect::<Vec<_>>());
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
            let inv = Inventory {
                stored: 0,
                capacity: equip.production * whcfg.airbase_max,
            };
            obj.warehouse.equipment.insert_cow(name.clone(), inv);
        }
        for (name, qty) in &production.liquids {
            let inv = Inventory {
                stored: 0,
                capacity: qty * whcfg.airbase_max,
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
                        let capacity = whcfg.capacity(hub, equip.production);
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
                        let capacity = whcfg.capacity(hub, *qty);
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
        let hub = self.persisted.logistics_hubs.contains(&oid);

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
            let capacity = whcfg.capacity(hub, equip.production);
            let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
            inv.capacity = capacity;
            inv.stored = capacity;
        }

        // Initialize liquids inventory
        for (name, qty) in &production.liquids {
            let capacity = whcfg.capacity(hub, *qty);
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
                    let hub = self.persisted.logistics_hubs.contains(oid);
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
                        let capacity = whcfg.capacity(hub, eqip.production);
                        let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
                        inv.capacity = capacity;
                    }
                    for (name, prod) in &prod.liquids {
                        let capacity = whcfg.capacity(hub, *prod);
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
                ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::Logistics | ObjectiveKind::NavalBase | ObjectiveKind::Factory { .. } => {
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
            | LogiStage::ManageConvoys { .. } => (),
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
                        .map(|(id, _)| *id)
                        .collect();
                    self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives }
                }
                LogiStage::Complete { last_tick } if ts - *last_tick >= freq => {
                    let objectives = self
                        .persisted
                        .objectives
                        .into_iter()
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
                    self.balance_logistics_hubs()?;

                    // Check if there are active convoys to manage
                    if !self.ephemeral.active_convoys.is_empty() {
                        let convoys = self.ephemeral.active_convoys.values().cloned().collect();
                        self.ephemeral.logistics_stage = LogiStage::ManageConvoys { convoys };
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
                LogiStage::ManageConvoys { convoys: _ } => {
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

                    // Transition to next stage
                    if self.ephemeral.active_convoys.is_empty() {
                        let objectives = self
                            .persisted
                            .objectives
                            .into_iter()
                            .map(|(id, _)| *id)
                            .collect();
                        self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives };
                    }

                    record_perf(&mut perf.logistics_convoy, st);
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
        let hub = obj.kind.is_hub();
        map.for_each(|name, _| {
            match production.equipment.get(&name) {
                Some(equip) => {
                    let inv = obj.warehouse.equipment.get_or_default_cow(name);
                    inv.capacity = whcfg.capacity(hub, equip.production);
                }
                None => {
                    if let Some(_) = other_production.equipment.get(&name) {
                        let inv = obj.warehouse.equipment.get_or_default_cow(name);
                        inv.stored = 0;
                        inv.capacity = 0;
                    }
                }
            }
            Ok(())
        })?;
        for name in LiquidType::ALL {
            match production.liquids.get(&name) {
                Some(qty) => {
                    let inv = obj.warehouse.liquids.get_or_default_cow(name);
                    inv.capacity = whcfg.capacity(hub, *qty);
                }
                None => {
                    if let Some(_) = other_production.liquids.get(&name) {
                        let inv = obj.warehouse.liquids.get_or_default_cow(name);
                        inv.stored = 0;
                        inv.capacity = 0;
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
                ObjectiveKind::CarrierGroup { .. } => (),
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
        let (truck_template, speed_kph, trucks_per_convoy) = match convoy_cfg.truck_template.get(&side) {
            Some(t) => (t.clone(), convoy_cfg.speed_kph, convoy_cfg.trucks_per_convoy),
            None => {
                warn!("No truck template configured for side {:?}, skipping convoy spawn", side);
                return Ok(());
            }
        };

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

        // Create mission with route to destination
        controller.set_task(Task::Mission {
            airborne: Some(false),
            route: vec![
                MissionPoint {
                    action: Some(ActionTyp::Ground(VehicleFormation::OffRoad)),
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
                    speed: speed_kph / 3.6, // convert km/h to m/s
                    speed_locked: Some(true),
                    name: None,
                    task: Box::new(Task::ComboTask(vec![])),
                },
                MissionPoint {
                    action: Some(ActionTyp::Ground(VehicleFormation::OffRoad)),
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
                    speed: speed_kph / 3.6, // convert km/h to m/s
                    speed_locked: None,
                    name: None,
                    task: Box::new(Task::ComboTask(vec![])),
                },
            ],
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

    pub fn deliver_supplies_from_logistics_hubs(&mut self, lua: MizLua, now: DateTime<Utc>) -> Result<Vec<Transfer>> {
        self.update_supply_status()
            .context("updating supply status")?;
        let mut transfers: Vec<Transfer> = vec![];

        // Check if convoy system is enabled
        let convoy_enabled = self.ephemeral.cfg.warehouse
            .as_ref()
            .and_then(|w| w.convoy.as_ref())
            .map(|c| c.enabled)
            .unwrap_or(false);

        // Collect hub IDs to avoid borrowing issues
        let hub_ids: SmallVec<[ObjectiveId; 16]> = self.persisted.logistics_hubs.into_iter().copied().collect();

        // Collect convoy spawn info to execute after we're done with objective references
        struct ConvoySpawnInfo {
            origin: ObjectiveId,
            destination: ObjectiveId,
            cargo_type: ConvoyCargoType,
            transfers: Vec<Transfer>,
        }
        let mut convoys_to_spawn: Vec<ConvoySpawnInfo> = Vec::new();

        for lid in hub_ids {
            let logi = objective!(self, &lid)?;

            // Split destinations into instant transfer vs convoy required
            let mut instant_needed: SmallVec<[Needed; 64]> = SmallVec::new();
            let mut convoy_needed: SmallVec<[Needed; 64]> = SmallVec::new();

            for oid in logi.warehouse.destination.into_iter() {
                if let Some(obj) = self.persisted.objectives.get(oid) {
                    if logi.owner == obj.owner && (obj.supply < 100 || obj.fuel < 100) {
                        let needed = Needed {
                            oid,
                            obj,
                            demanded: 0,
                            allocated: 0,
                        };

                        // Check if this destination requires convoy
                        if convoy_enabled && obj.logistics_detached {
                            convoy_needed.push(needed);
                        } else {
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
                        convoys_to_spawn.push(ConvoySpawnInfo {
                            origin: lid,
                            destination: dest_oid,
                            cargo_type: ConvoyCargoType::Weapons,
                            transfers: equipment_transfers,
                        });
                    }

                    // Add fuel convoy if there are fuel transfers
                    if !fuel_transfers.is_empty() {
                        convoys_to_spawn.push(ConvoySpawnInfo {
                            origin: lid,
                            destination: dest_oid,
                            cargo_type: ConvoyCargoType::Fuel,
                            transfers: fuel_transfers,
                        });
                    }
                }
            }
        }

        // Now spawn all collected convoys (after we're done with objective references)
        for convoy_info in convoys_to_spawn {
            // Deduct supplies from source immediately (convoy takes them)
            for tr in &convoy_info.transfers {
                if let Err(e) = tr.execute(&mut self.persisted, &self.ephemeral.to_bg) {
                    error!("Failed to deduct supplies for convoy: {:?}", e);
                }
            }

            // Spawn the convoy
            if let Err(e) = self.spawn_supply_convoy(
                lua,
                convoy_info.origin,
                convoy_info.destination,
                convoy_info.cargo_type,
                convoy_info.transfers,
                now,
            ) {
                error!("Failed to spawn {:?} convoy from {} to {}: {:?}",
                    convoy_info.cargo_type, convoy_info.origin, convoy_info.destination, e);
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
