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
    cfg::{is_model_only_item, ProductionScalingConfig, Vehicle, MATERIEL_ITEM},
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

    /// Put the cargo back where it came from. Used when a load never
    /// actually left (spawn failed) or can never arrive (server restarted
    /// mid-transit, convoy wedged on terrain and timed out). Capped at the
    /// source's capacity via `AddAssign`, so a refund can't overfill a
    /// warehouse that was topped up while the load was away.
    fn refund(&self, db: &mut Persisted, to_bg: &Option<UnboundedSender<Task>>) -> Result<()> {
        let src = db
            .objectives
            .get_mut_cow(&self.source)
            .ok_or_else(|| anyhow!("no such objective {:?}", self.source))?;
        match &self.item {
            TransferItem::Equipment(name) => {
                let inv = src.warehouse.equipment.get_or_default_cow(name.clone());
                *inv += self.amount;
                let stored = inv.stored;
                if let Some(to_bg) = to_bg.as_ref() {
                    let _ = to_bg.send(Task::Stat(Stat::EquipmentInventory {
                        id: src.id,
                        item: name.clone(),
                        amount: stored,
                    }));
                }
            }
            TransferItem::Liquid(name) => {
                let inv = src.warehouse.liquids.get_or_default_cow(*name);
                *inv += self.amount;
                let stored = inv.stored;
                if let Some(to_bg) = to_bg.as_ref() {
                    let _ = to_bg.send(Task::Stat(Stat::LiquidInventory {
                        id: src.id,
                        item: *name,
                        amount: stored,
                    }));
                }
            }
        }
        Ok(())
    }
}

/// Cargo that has been debited from a source objective and handed to an
/// in-flight convoy / air route / sea route, but has not been delivered yet.
///
/// The route structs themselves live in ephemeral state, because their DCS
/// groups die with the mission -- so without this ledger every server
/// restart silently destroyed whatever happened to be on the road at the
/// time (the stock was already taken out of the hub, and the only thing
/// that would ever have put it back was a delivery that can no longer
/// happen). `reconcile_pending_cargo` refunds anything still outstanding
/// at load.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PendingCargo {
    /// Objective the cargo was taken out of.
    pub origin: ObjectiveId,
    /// Objective it was on its way to (for logging).
    pub destination: ObjectiveId,
    /// The transfers that will be executed if it arrives.
    pub transfers: Vec<Transfer>,
    /// When it left. Also drives the in-transit timeout.
    pub departed: DateTime<Utc>,
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

/// Unique identifier for an AI helo mission (troop insertion / resource
/// delivery).
pub type HeloMissionId = CompactString;

/// Current state of an AI helo mission.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum HeloMissionState {
    InTransit,
    Delivered,
    Destroyed,
}

/// What an AI helo mission does once it has landed at its destination.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HeloMissionKind {
    /// Deploy a fresh troop group of the configured type (same spawn path as
    /// the Paratrooper action) once the helo is down.
    TroopInsertion,
    /// Transfer a pre-computed batch of warehouse supply to the destination
    /// once the helo is down.
    ResourceDelivery { transfers: Vec<Transfer> },
}

/// An F10-callable AI helicopter mission: cold-starts from a friendly
/// airbase, flies to and actually lands at the destination, then either
/// deploys troops or hands off supply before despawning. See
/// `Db::call_helo_troop_insertion` / `Db::call_helo_resource_delivery`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HeloMission {
    pub id: HeloMissionId,
    pub group_id: bfprotocols::db::group::GroupId,
    pub kind: HeloMissionKind,
    pub origin: ObjectiveId,
    pub destination: ObjectiveId,
    pub side: Side,
    pub player: dcso3::net::Ucid,
    pub spawn_time: DateTime<Utc>,
    pub state: HeloMissionState,
    pub last_pos: Vector2,
    pub last_check: DateTime<Utc>,
}

impl HeloMission {
    /// Poll the DCS group: track its position, detect destruction, and
    /// detect "landed and delivered" -- on the ground (`in_air() == false`)
    /// AND within `landing_radius` of `destination_pos`. Distance alone
    /// isn't enough; a helo passing low over the point at cruise speed on
    /// its way somewhere else shouldn't count.
    pub fn poll(
        &mut self,
        lua: MizLua,
        group_name: &str,
        destination_pos: Vector2,
        landing_radius: f64,
    ) -> HeloMissionState {
        use dcso3::group::Group;
        match Group::get_by_name(lua, group_name) {
            Ok(group) => match group.get_units() {
                Ok(units) if units.len() > 0 => {
                    if let Ok(unit) = units.get(1) {
                        if let Ok(pos) = unit.get_point() {
                            self.last_pos = Vector2::new(pos.x, pos.z);
                        }
                        // Unknown in-air state is treated as airborne -- never
                        // declare a delivery we aren't sure actually landed.
                        let in_air = unit.in_air().unwrap_or(true);
                        if !in_air {
                            let dist = (self.last_pos - destination_pos).norm();
                            if dist <= landing_radius {
                                self.state = HeloMissionState::Delivered;
                            }
                        }
                    }
                    self.state
                }
                _ => {
                    self.state = HeloMissionState::Destroyed;
                    HeloMissionState::Destroyed
                }
            },
            Err(_) => {
                self.state = HeloMissionState::Destroyed;
                HeloMissionState::Destroyed
            }
        }
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
    !is_model_only_item(name)
        && !name.starts_with("weapons.")
        && !name.starts_with("vehicles.")
        && !name.starts_with("Fortifications.")
}

pub(super) fn sync_obj_to_warehouse(obj: &Objective, warehouse: &warehouse::Warehouse) -> Result<()> {
    let perf = unsafe { Perf::get_mut() };
    let perf = Arc::make_mut(&mut perf.inner);
    for (item, inv) in &obj.warehouse.equipment {
        // Model-only commodities (materiel) have no DCS resource-map entry.
        // Pushing them would at best be ignored and at worst error, and the
        // read-back would zero them.
        if is_model_only_item(item.as_str()) {
            continue;
        }
        perf.logistics_items.insert((item.clone(), obj.id));
        if item.as_str() == "AJS37" || item.as_str() == "C-130J-30" || item.as_str().starts_with("CH-47F") {
            debug!("[WAREHOUSE_SYNC] pushing obj={} owner={:?} {item}=stored:{}",
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
        if is_model_only_item(item.as_str()) {
            continue;
        }
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

/// Like `sync_obj_to_warehouse`, but first zeros every *airframe* entry in the
/// DCS resource map that the objective's model doesn't carry. Land bases spawn
/// with whatever aircraft roster the .miz / DCS defaults gave them, which
/// routinely includes the other coalition's jets; bflib only ever `set_item`s
/// the airframes it tracks, so those foreign jets stay slottable at a captured
/// or mis-templated base forever. Weapons/liquids are left to the normal sync
/// (those are side-neutral and already driven by production). No-op against an
/// unlimited-aircraft warehouse -- DCS ignores `setItem` there.
pub(super) fn sync_obj_to_warehouse_zeroing_foreign_airframes(
    obj: &Objective,
    warehouse: &warehouse::Warehouse,
    resource_map: &warehouse::ResourceMap,
) -> Result<()> {
    resource_map.for_each(|name, _| {
        if is_airframe_item(name.as_str()) && obj.warehouse.equipment.get(&name).is_none() {
            warehouse
                .set_item(name, 0)
                .context("zeroing foreign airframe not in objective warehouse")?;
        }
        Ok(())
    })?;
    sync_obj_to_warehouse(obj, warehouse)
}

fn sync_warehouse_to_obj(obj: &mut Objective, warehouse: &warehouse::Warehouse) -> Result<()> {
    // Read back, but never above the objective's own capacity.
    //
    // Anything DCS manufactures on its own between two logistics ticks --
    // an airbase warehouse with a nonzero OperatingLevel_*, a mission-editor
    // default, a rearm that credited more than it took -- lands in this read
    // and used to be absorbed into the model verbatim. Stock then drifted
    // upward forever with nothing to stop it, which is how a base ends up
    // holding more of something than its warehouse is supposed to be able to
    // hold and never runs short no matter what the supply line is doing.
    // Capacity is the model's ceiling by definition, so enforce it here.
    let clamp = |inv: &mut Inventory, dcs: u32| {
        inv.stored = min(dcs, inv.capacity);
    };
    for (name, inv) in obj.warehouse.equipment.iter_mut_cow() {
        // DCS knows nothing about model-only commodities, so asking it for a
        // count would just wipe them on every logistics tick.
        if is_model_only_item(name.as_str()) {
            continue;
        }
        clamp(inv, warehouse.get_item_count(name.clone())?);
    }
    for (name, inv) in obj.warehouse.liquids.iter_mut_cow() {
        clamp(inv, warehouse.get_liquid_amount(*name)?);
    }
    Ok(())
}

fn get_supplier<'lua>(lua: MizLua<'lua>, template: String) -> Result<warehouse::Warehouse<'lua>> {
    Airbase::get_by_name(lua, template.clone())
        .with_context(|| format_compact!("getting airbase {}", template))?
        .get_warehouse()
        .context("getting warehouse")
}

/// Shortest distance from `p` to the segment `a`-`b`. Used to decide whether
/// an enemy-held objective sits on the ground route between two friendly
/// ones, without having to sample the line.
fn distance_to_segment(p: Vector2, a: Vector2, b: Vector2) -> f64 {
    let ab = b - a;
    let len2 = ab.norm_squared();
    if len2 <= f64::EPSILON {
        return (p - a).norm();
    }
    let t = ((p - a).dot(&ab) / len2).clamp(0., 1.);
    (p - (a + ab * t)).norm()
}

/// Is the ground route between `from` and `to` interdicted for `side`?
///
/// True when any enemy-held objective that still has a functioning garrison
/// sits within its own zone radius plus `margin` of the straight line between
/// them. It is a coarse model of a front line, but it is the difference
/// between a supply network that can be cut and one where a depot behind
/// enemy lines keeps trucking fuel through it because it happens to be the
/// nearest one by ruler.
fn route_interdicted(
    persisted: &super::persisted::Persisted,
    side: Side,
    from: Vector2,
    to: Vector2,
    margin: f64,
) -> bool {
    for (_, obj) in &persisted.objectives {
        if obj.owner == side || obj.owner == Side::Neutral {
            continue;
        }
        // A flattened base doesn't hold ground, and things that aren't on the
        // ground at all can't cut a road.
        if obj.logi == 0
            || matches!(
                obj.kind,
                ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::SpecialSamSite { .. }
            )
        {
            continue;
        }
        let block = obj.zone.radius() + margin;
        if distance_to_segment(obj.zone.pos(), from, to) <= block {
            return true;
        }
    }
    false
}

/// Resolve a carrier deck airbase (named after its ship unit) to the
/// carrier objective it belongs to, via the ship unit -> group ->
/// `objectives_by_group` chain. Returns:
///   - `Ok(oid)` if the deck's group is that objective's LIVE task force
///     (the group registered under the current owner side)
///   - `Err(())` if the deck belongs to a carrier group that is NOT the
///     live task force (a reserve, or the losing side's ships that
///     haven't despawned) -- caller should skip the airbase entirely so a
///     reserve/stale deck can't steal the objective's warehouse slot
///   - `None` if the unit name doesn't resolve to any carrier group
///     (caller falls back to position matching)
fn carrier_deck_live_objective(
    persisted: &super::persisted::Persisted,
    unit_name: &str,
) -> Option<std::result::Result<ObjectiveId, ()>> {
    let gid = persisted.groups.into_iter().find_map(|(gid, g)| {
        let is_carrier = g.name.contains("CARRIER")
            && matches!(g.class, super::objective::ObjGroupClass::Naval);
        if !is_carrier {
            return None;
        }
        let has_unit = g
            .units
            .into_iter()
            .filter_map(|uid| persisted.units.get(uid))
            .any(|u| u.template_name.as_str() == unit_name);
        if has_unit { Some(*gid) } else { None }
    })?;
    let oid = *persisted.objectives_by_group.get(&gid)?;
    let obj = persisted.objectives.get(&oid)?;
    let is_live = obj
        .groups
        .get(&obj.owner)
        .map(|s| s.into_iter().any(|g| *g == gid))
        .unwrap_or(false);
    Some(if is_live { Ok(oid) } else { Err(()) })
}

/// The carrier-group objective whose LIVE task force is closest to `pos`.
/// Both carrier objectives can be owned by the same side (one captured)
/// and their 5km zones overlap once the carriers sail near the same naval
/// base, so attributing a carrier deck airbase / carrier slot by zone
/// containment or "first carrier objective owned by side" mis-assigns
/// then -- match the physical ship instead. Free fn (not a Db method) so
/// callers inside a self-mutating closure can borrow only `persisted`.
pub(super) fn nearest_carrier_objective(
    persisted: &super::persisted::Persisted,
    pos: Vector2,
) -> Option<ObjectiveId> {
    let mut best: Option<(ObjectiveId, f64)> = None;
    for (oid, obj) in &persisted.objectives {
        if !matches!(obj.kind, ObjectiveKind::CarrierGroup { .. }) {
            continue;
        }
        let Some(set) = obj.groups.get(&obj.owner) else {
            continue;
        };
        for gid in set {
            let Some(g) = persisted.groups.get(gid) else {
                continue;
            };
            let mut sum = Vector2::default();
            let mut n = 0u32;
            for uid in &g.units {
                if let Some(u) = persisted.units.get(uid) {
                    sum += u.pos;
                    n += 1;
                }
            }
            if n == 0 {
                continue;
            }
            let c = sum / n as f64;
            let d = na::distance_squared(&c.into(), &pos.into());
            if best.map_or(true, |(_, bd)| d < bd) {
                best = Some((*oid, d));
            }
        }
    }
    best.map(|(o, _)| o)
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
                                // Expected -- Neutral has no production/supply by design.
                                debug!("[WAREHOUSE] No supply_source configured for Neutral side - skipping");
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
                        Some(equip) => debug!("[WAREHOUSE_PROBE] {side:?} {probe}: production={}", equip.production),
                        None => debug!("[WAREHOUSE_PROBE] {side:?} {probe}: not tracked at all"),
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
            let is_airframe = is_airframe_item(name);
            let unlimited = if is_airframe { obj.unlimited_aircraft } else { obj.unlimited_supply };
            let inv = Inventory {
                stored: 0,
                capacity: whcfg.capacity_for_item(
                    &obj.name,
                    is_airframe,
                    unlimited,
                    false,
                    equip.production,
                ),
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
                let is_airframe = is_airframe_item(name);
                for (oid, obj) in self.persisted.objectives.iter_mut_cow() {
                    if obj.owner == side {
                        let is_carrier = self.persisted.carrier_groups.contains(&oid);
                        // A carrier stocks only the aircraft physically in its
                        // deck warehouse (the naval roster bftools set), NOT
                        // the whole side's airframe production list -- otherwise
                        // a Kuznetsov "carries" 700+ types incl. Spitfires and
                        // land-only jets. Weapons/fuel still come from
                        // production (side-neutral). setup_warehouses_after_load
                        // reads the deck inventory into the model.
                        if is_carrier && is_airframe {
                            continue;
                        }
                        let hub = self.persisted.logistics_hubs.contains(&oid) || is_carrier;
                        let unlimited = if is_airframe { obj.unlimited_aircraft } else { obj.unlimited_supply };
                        let capacity = whcfg.capacity_for_item(&obj.name, is_airframe, unlimited, hub, equip.production);
                        let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
                        inv.capacity = capacity;
                        inv.stored = capacity;
                        if is_carrier {
                            initialized_count += 1;
                            debug!("[WAREHOUSE] Initialized carrier {} with equipment {} (capacity: {}, hub: {})",
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
            info!("[WAREHOUSE] Initialized {} carrier warehouse stock entries for side {:?}", initialized_count, side);
        }
        self.sync_materiel_capacity();
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

        // Initialize equipment inventory. A carrier gets weapons/fuel from
        // production but NOT airframes -- its aircraft come from the deck
        // warehouse (naval roster); see init_warehouses.
        for (name, equip) in &production.equipment {
            let is_airframe = is_airframe_item(name);
            if is_carrier && is_airframe {
                continue;
            }
            let unlimited = if is_airframe { obj.unlimited_aircraft } else { obj.unlimited_supply };
            let capacity = whcfg.capacity_for_item(&obj.name, is_airframe, unlimited, hub, equip.production);
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
                    // A carrier deck airbase is named after its ship unit. Both
                    // carrier objectives can be owned by the same side with
                    // overlapping 5km zones (one captured, both near the same
                    // naval base), so zone containment attributes every deck to
                    // whichever carrier objective iterates first -- match the
                    // physical ship instead.
                    let is_carrier_deck =
                        name.starts_with("BCARRIER") || name.starts_with("RCARRIER");
                    let oid: Option<ObjectiveId> = if is_carrier_deck {
                        match carrier_deck_live_objective(&self.persisted, &name) {
                            // deck of a live carrier task force -> its objective
                            Some(Ok(oid)) => Some(oid),
                            // deck of a reserve / stale carrier group -> don't
                            // let it register (or steal) an objective warehouse
                            Some(Err(())) => {
                                log::info!(
                                    "skipping carrier deck {name} (not the live task force)"
                                );
                                return Ok(());
                            }
                            // unrecognised carrier unit -> fall back to position
                            None => nearest_carrier_objective(&self.persisted, pos),
                        }
                    } else {
                        None
                    }
                    .or_else(|| {
                        // Several objective zones can overlap one airfield -- a
                        // hidden SAM site or a command center placed on/next to
                        // the base. `find` would attach the airbase (and its
                        // warehouse, and its `airbase_by_oid` entry) to
                        // whichever iterates first, which then breaks CAP
                        // ground-start (it looks the airbase up by the
                        // Airbase-kind objective id and misses). Prefer an
                        // actual Airbase-kind objective when there's a choice.
                        let mut hit: Option<(&ObjectiveId, bool)> = None;
                        for (oid, obj) in &self.persisted.objectives {
                            if !obj.zone.contains(pos) {
                                continue;
                            }
                            let is_ab = matches!(obj.kind, ObjectiveKind::Airbase);
                            match hit {
                                None => hit = Some((oid, is_ab)),
                                Some((_, false)) if is_ab => hit = Some((oid, is_ab)),
                                _ => {}
                            }
                        }
                        hit.map(|(oid, _)| *oid)
                    });
                    let w = airbase
                        .get_warehouse()
                        .context("getting airbase warehouse")?;
                    let (oid, obj_owner, obj_name, is_carrier_group) = match oid.and_then(|oid| {
                        self.persisted.objectives.get(&oid).map(|o| {
                            (
                                oid,
                                o.owner,
                                o.name.clone(),
                                matches!(o.kind, ObjectiveKind::CarrierGroup { .. }),
                            )
                        })
                    }) {
                        Some(t) => {
                            airbase
                                .set_coalition(t.1)
                                .context("setting airbase owner")?;
                            t
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
                    let _ = obj_owner;

                    match self.ephemeral.airbase_by_oid.entry(oid) {
                        Entry::Vacant(e) => {
                            e.insert(airbase.object_id().context("getting airbase object_id")?);

                            if is_carrier_group {
                                log::info!("[CARRIER_WAREHOUSE] Registering carrier warehouse for {} (objective: {})",
                                          name, obj_name);
                                // Pull in whatever aircraft are physically aboard
                                // this carrier that the model doesn't know about --
                                // a captured carrier keeps the previous owner's
                                // jets, and the mission designer may have loaded
                                // types that aren't in either side's production
                                // list. Without this the zeroing sync below wipes
                                // them and players get "no <type> in stock" for a
                                // jet that's sitting on the deck.
                                let mut aboard: Vec<(dcso3::String, u32)> = vec![];
                                if let Ok(inv) = w.get_inventory(None) {
                                    if let Ok(ac) = inv.aircraft() {
                                        let _ = ac.for_each(|n, c| {
                                            if c > 0 {
                                                aboard.push((n, c));
                                            }
                                            Ok(())
                                        });
                                    }
                                }
                                // The carrier's deck warehouse is the ONLY
                                // source of truth for which aircraft it can
                                // operate (the naval roster). Drop any airframe
                                // in the model that isn't physically aboard --
                                // otherwise the whole side's airframe
                                // production list leaks onto the carrier (a
                                // Kuznetsov "carrying" 700+ types).
                                {
                                    let objm = objective_mut!(self, oid)?;
                                    let aboard_names: std::collections::HashSet<&str> =
                                        aboard.iter().map(|(n, _)| n.as_str()).collect();
                                    let stale: SmallVec<[String; 32]> = objm
                                        .warehouse
                                        .equipment
                                        .into_iter()
                                        .filter(|(n, _)| {
                                            is_airframe_item(n.as_str())
                                                && !aboard_names.contains(n.as_str())
                                        })
                                        .map(|(n, _)| n.clone())
                                        .collect();
                                    for n in stale {
                                        objm.warehouse.equipment.remove_cow(&n);
                                    }
                                    for (n, c) in &aboard {
                                        let cap = whcfg.capacity(true, (*c).max(1));
                                        let inv =
                                            objm.warehouse.equipment.get_or_default_cow(n.clone());
                                        inv.capacity = cap;
                                        if inv.stored < *c {
                                            inv.stored = *c;
                                        }
                                    }
                                }
                                if !aboard.is_empty() {
                                    log::info!("[CARRIER_WAREHOUSE] {} carries {} aircraft type(s) aboard: {:?}",
                                              obj_name, aboard.len(),
                                              aboard.iter().map(|(n, c)| format_compact!("{n}={c}")).collect::<Vec<_>>());
                                }
                                let obj = objective!(self, oid)?;
                                sync_obj_to_warehouse_with_zeroing(obj, &w, &map)
                                    .context("syncing carrier warehouse with zeroing")?;
                            }
                        }
                        Entry::Occupied(_) => {
                            // For carrier groups, skip escort ships (additional airbases in the zone)
                            if is_carrier_group {
                                log::info!("[CARRIER_WAREHOUSE] Skipping escort ship {} in carrier group {} (warehouse already registered)",
                                          name, obj_name);
                                return Ok(());
                            }
                            bail!("multiple airbases inside the trigger zone of {}", obj_name)
                        }
                    }
                    Ok(())
                })
        };
        load_and_sync_airbases().context("loading and syncing airbases")?;
        let salvage_enabled = whcfg.captured_airframes.is_some();
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
                    // A captured carrier keeps the previous owner's airframes so
                    // the new owner can operate them once repairs finish (see
                    // capture_warehouse's carrier branch + the CapturedNotReady
                    // gate in try_occupy_slot_deferred). Don't let this pass
                    // delete those "foreign" entries just because they're not in
                    // the current owner's production -- that left a captured
                    // carrier unable to slot its own retained jets ("Objective
                    // does not have any FA-18C_hornet in stock").
                    let other_prod = self
                        .ephemeral
                        .production_by_side
                        .get(&obj.owner.opposite())
                        .cloned();
                    for (name, inv) in &obj.warehouse.equipment {
                        // Materiel is a campaign commodity, not something the
                        // side "produces" in the resource-map sense, so it is
                        // never in `prod.equipment` -- don't prune it.
                        if is_model_only_item(name.as_str()) {
                            continue;
                        }
                        if !prod.equipment.contains_key(name) {
                            // On a carrier, never drop an airframe entry that
                            // actually has stock (a captured carrier's retained
                            // jets, or types the mission designer loaded aboard
                            // that aren't in either side's production list) or
                            // one that's in the opposite side's production.
                            let keep_carrier = is_carrier
                                && (is_airframe_item(name.as_str()) && inv.stored > 0
                                    || other_prod
                                        .as_ref()
                                        .map(|p| p.equipment.contains_key(name))
                                        .unwrap_or(false));
                            // Same reasoning on land: an airframe this side
                            // doesn't produce but still has stock of is the
                            // salvage from a capture (see capture_warehouse).
                            // Pruning it here would quietly delete the prize
                            // on the next mission load -- the base would show
                            // "does not have any F-16C_50 in stock" for jets
                            // it is visibly parked on.
                            let keep_salvage = !is_carrier
                                && salvage_enabled
                                && is_airframe_item(name.as_str())
                                && inv.stored > 0
                                && other_prod
                                    .as_ref()
                                    .map(|p| p.equipment.contains_key(name))
                                    .unwrap_or(false);
                            if !keep_carrier && !keep_salvage {
                                del_eq.push(name.clone());
                            }
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
                        let is_airframe = is_airframe_item(name);
                        // don't seed the side's full airframe list onto a
                        // carrier -- its aircraft are the deck (naval) roster,
                        // already loaded by load_and_sync_airbases. Weapons/fuel
                        // still get topped up.
                        if is_carrier && is_airframe {
                            continue;
                        }
                        let unlimited = if is_airframe { obj.unlimited_aircraft } else { obj.unlimited_supply };
                        let capacity = whcfg.capacity_for_item(&obj.name, is_airframe, unlimited, hub, eqip.production);
                        let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
                        inv.capacity = capacity;
                    }
                    if is_carrier {
                        // Only refresh capacity on airframes the carrier
                        // ALREADY has (i.e. physically aboard) -- a captured
                        // carrier's retained foreign jets. Never create new
                        // airframe entries from the opposite side's roster.
                        if let Some(other_prod) = &other_prod {
                            let present: SmallVec<[String; 16]> = obj
                                .warehouse
                                .equipment
                                .into_iter()
                                .filter(|(n, _)| {
                                    is_airframe_item(n.as_str())
                                        && !prod.equipment.contains_key(*n)
                                        && other_prod.equipment.contains_key(*n)
                                })
                                .map(|(n, _)| n.clone())
                                .collect();
                            for name in present {
                                let p = other_prod.equipment.get(&name).map(|e| e.production).unwrap_or(1);
                                let cap = whcfg.capacity(true, p);
                                let inv = obj.warehouse.equipment.get_or_default_cow(name);
                                inv.capacity = cap;
                                if inv.stored == 0 {
                                    inv.stored = cap;
                                }
                            }
                        }
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
                ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::Logistics | ObjectiveKind::NavalBase | ObjectiveKind::Factory { .. } | ObjectiveKind::SpecialSamSite { .. } | ObjectiveKind::CommandCenter => {
                    // These objective types don't require airbase warehouses
                }
            }
        }
        if !missing.is_empty() {
            bail!("objectives missing a warehouse {:?}", missing)
        }
        // Anything still sitting in the in-flight ledger belonged to a
        // convoy/aircraft/ship from the previous mission run; those groups
        // are gone, so give the stock back before anything else touches the
        // warehouses.
        self.sync_materiel_capacity();
        self.reconcile_pending_cargo();
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

    /// What `from` could hand to `to` right now: for every equipment item and
    /// liquid the source actually holds, the smaller of the destination's
    /// remaining headroom and `max_frac` of the source's stock. Mirrors what
    /// the scheduled hub distribution does, but for a single point-to-point
    /// load.
    ///
    /// Deliberately ignores the hub reserve: this builds emergency relief
    /// loads for a base that has been sitting supply-critical, and breaking
    /// into the reserve for exactly that is what the reserve is for. Routine
    /// scheduled distribution goes through the `releasable` path instead.
    fn build_transfers(
        &self,
        from: ObjectiveId,
        to: ObjectiveId,
        max_frac: f32,
    ) -> Vec<Transfer> {
        let (Some(src), Some(dst)) = (
            self.persisted.objectives.get(&from),
            self.persisted.objectives.get(&to),
        ) else {
            return vec![];
        };
        let mut out = vec![];
        let share = |stored: u32| -> u32 {
            if max_frac >= 1.0 {
                stored
            } else {
                max(1, (stored as f32 * max_frac) as u32)
            }
        };
        for (name, inv) in &src.warehouse.equipment {
            if inv.stored == 0 {
                continue;
            }
            let d = dst.get_equipment(name);
            let headroom = d.capacity.saturating_sub(d.stored);
            let amount = min(headroom, share(inv.stored));
            if amount > 0 {
                out.push(Transfer {
                    source: from,
                    target: to,
                    amount,
                    item: TransferItem::Equipment(name.clone()),
                });
            }
        }
        for (name, inv) in &src.warehouse.liquids {
            if inv.stored == 0 {
                continue;
            }
            let d = dst.get_liquids(name);
            let headroom = d.capacity.saturating_sub(d.stored);
            let amount = min(headroom, share(inv.stored));
            if amount > 0 {
                out.push(Transfer {
                    source: from,
                    target: to,
                    amount,
                    item: TransferItem::Liquid(*name),
                });
            }
        }
        out
    }

    /// Hand a load to a route: debit the source warehouses and record the
    /// cargo in the persisted ledger under `id`. Call this only once the
    /// transport has actually spawned -- see the dispatch sites, which used
    /// to debit first and silently destroy the stock whenever the spawn
    /// turned out to be a no-op (no truck/aircraft template for the side).
    fn escrow_cargo(
        &mut self,
        id: &str,
        origin: ObjectiveId,
        destination: ObjectiveId,
        transfers: &[Transfer],
        now: DateTime<Utc>,
    ) {
        let (from, to) = (
            self.persisted
                .objectives
                .get(&origin)
                .map(|o| o.name.clone())
                .unwrap_or_default(),
            self.persisted
                .objectives
                .get(&destination)
                .map(|o| o.name.clone())
                .unwrap_or_default(),
        );
        let units: u32 = transfers.iter().map(|t| t.amount).sum();
        for tr in transfers {
            if let Err(e) = tr.execute(&mut self.persisted, &self.ephemeral.to_bg) {
                error!("[LOGI_CARGO] escrowing cargo for {id}: {e:?}");
            }
        }
        info!(
            "[LOGI_CARGO] {id} loaded at {from} for {to}: {} item type(s), {units} unit(s) total \
             (now held in the in-flight ledger, {} load(s) outstanding)",
            transfers.len(),
            self.persisted.pending_cargo.len() + 1
        );
        debug!("[LOGI_CARGO] {id} manifest: {transfers:?}");
        self.persisted.pending_cargo.insert_cow(
            CompactString::from(id),
            PendingCargo {
                origin,
                destination,
                transfers: transfers.to_vec(),
                departed: now,
            },
        );
        self.ephemeral.dirty();
    }

    /// The load arrived (or was destroyed with it): drop the ledger entry so
    /// a later restart doesn't refund cargo that has already been accounted
    /// for one way or the other.
    fn settle_cargo(&mut self, id: &str) {
        let key = CompactString::from(id);
        if let Some(p) = self.persisted.pending_cargo.get(&key).cloned() {
            let units: u32 = p.transfers.iter().map(|t| t.amount).sum();
            info!(
                "[LOGI_CARGO] {id} settled after {} min ({units} unit(s) no longer outstanding)",
                (Utc::now() - p.departed).num_minutes()
            );
            self.persisted.pending_cargo.remove_cow(&key);
            self.ephemeral.dirty();
        } else {
            debug!("[LOGI_CARGO] {id} settled with no ledger entry (pre-ledger load)");
        }
    }

    /// The load can never arrive (spawn failed, transport wedged and timed
    /// out, mission reloaded out from under it): put the stock back in the
    /// origin warehouse and drop the ledger entry.
    fn refund_cargo(&mut self, id: &str) {
        let key = CompactString::from(id);
        let Some(pending) = self.persisted.pending_cargo.get(&key).cloned() else {
            debug!("[LOGI_CARGO] {id} had nothing to refund");
            return;
        };
        for tr in &pending.transfers {
            if let Err(e) = tr.refund(&mut self.persisted, &self.ephemeral.to_bg) {
                error!("[LOGI_CARGO] refunding cargo for {id}: {e:?}");
            }
        }
        let units: u32 = pending.transfers.iter().map(|t| t.amount).sum();
        info!(
            "[LOGI_CARGO] {id} refunded to {}: {units} unit(s) returned after {} min in transit",
            self.persisted
                .objectives
                .get(&pending.origin)
                .map(|o| o.name.clone())
                .unwrap_or_default(),
            (Utc::now() - pending.departed).num_minutes()
        );
        self.persisted.pending_cargo.remove_cow(&key);
        self.ephemeral.dirty();
    }

    /// Called once at mission load. Every ledger entry that survived into a
    /// new mission belongs to a transport that no longer exists, so its
    /// cargo goes back to the origin warehouse. Without this, each restart
    /// permanently deleted whatever was in transit -- a steady, invisible
    /// drain on both coalitions' supply.
    pub(super) fn reconcile_pending_cargo(&mut self) {
        let outstanding: Vec<CompactString> = self
            .persisted
            .pending_cargo
            .into_iter()
            .map(|(id, _)| id.clone())
            .collect();
        if outstanding.is_empty() {
            return;
        }
        info!(
            "[WAREHOUSE] refunding {} in-flight cargo load(s) orphaned by the mission restart",
            outstanding.len()
        );
        for id in outstanding {
            self.refund_cargo(&id);
        }
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
                        .filter(|(id, obj)| {
                            !obj.kind.is_special_sam_site()
                                && self.ephemeral.airbase_by_oid.contains_key(id)
                        })
                        .map(|(id, _)| *id)
                        .collect();
                    self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives }
                }
                LogiStage::Complete { last_tick } if ts - *last_tick >= freq => {
                    let objectives = self
                        .persisted
                        .objectives
                        .into_iter()
                        .filter(|(id, obj)| {
                            !obj.kind.is_special_sam_site()
                                && self.ephemeral.airbase_by_oid.contains_key(id)
                        })
                        .map(|(id, _)| *id)
                        .collect();
                    self.ephemeral.logistics_stage = LogiStage::SyncFromWarehouses { objectives };
                }
                LogiStage::Complete { last_tick: _ } => (),
                LogiStage::SyncFromWarehouses { objectives } => match objectives.pop() {
                    Some(oid) => {
                        // This queue was snapshotted when the stage began and drains
                        // slowly (one objective per tick); by the time a given entry
                        // is reached its airbase registration may legitimately be
                        // gone (owner change, pad respawn, objective destroyed) --
                        // that's an expected race against the slow drain, not a real
                        // failure, so skip quietly instead of erroring every time.
                        if self.ephemeral.airbase_by_oid.contains_key(&oid) {
                            let start_ts = Utc::now();
                            if let Err(e) = self.sync_warehouse_to_objective(lua, oid) {
                                error!("failed to sync objective {oid} from warehouse {:?}", e)
                            }
                            record_perf(&mut perf.logistics_sync_from, start_ts);
                        }
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
                                // Work out what the hub can actually send. This
                                // used to pass an empty transfer list, so the
                                // relief convoy drove the length of the map and
                                // delivered precisely nothing.
                                let load = self.build_transfers(hub, dest_oid, 1.0);
                                if load.is_empty() {
                                    debug!(
                                        "AUTO-DISPATCH: nothing to send to {} (hub empty or destination full)",
                                        dest_name
                                    );
                                    continue;
                                }
                                match self.spawn_supply_convoy(
                                    lua,
                                    hub,
                                    dest_oid,
                                    ConvoyCargoType::Mixed,
                                    load.clone(),
                                    ts,
                                ) {
                                    Ok(Some(id)) => {
                                        self.escrow_cargo(&id, hub, dest_oid, &load, ts);
                                        self.ephemeral.last_dispatch_to.insert(dest_oid, ts);
                                        info!(
                                            "AUTO-DISPATCH: supply convoy → {} ({} item(s))",
                                            dest_name,
                                            load.len()
                                        );
                                        self.ephemeral.supply_warned.insert(dest_oid, ts);
                                    }
                                    Ok(None) => (),
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
                        let max_transit = Duration::minutes(cfg.max_transit_minutes as i64);
                        let mut completed_convoys = Vec::new();
                        let mut timed_out: Vec<ConvoyId> = Vec::new();
                        let mut despawn: Vec<bfprotocols::db::group::GroupId> = Vec::new();

                        for convoy_id in self.ephemeral.active_convoys.keys().cloned().collect::<Vec<_>>() {
                            if let Some(convoy) = self.ephemeral.active_convoys.get_mut(&convoy_id) {
                                let convoy_group_id = convoy.group_id;
                                // A convoy that has been on the road far longer
                                // than the trip could take is wedged on terrain
                                // -- DCS ground pathing does this routinely.
                                // Turn it back rather than leaving it parked
                                // forever holding cargo that can never arrive.
                                if max_transit > Duration::zero()
                                    && ts - convoy.spawn_time > max_transit
                                {
                                    timed_out.push(convoy_id.clone());
                                    despawn.push(convoy_group_id);
                                    completed_convoys.push(convoy_id.clone());
                                    continue;
                                }
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

                                            // Despawn the trucks -- same as air/sea routes.
                                            // Without this they park at the destination
                                            // forever and every well-supplied base ends
                                            // up ringed with dead convoys.
                                            despawn.push(convoy_group_id);
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

                        // Remove completed convoys. A delivered convoy has
                        // already moved its cargo and a destroyed one has lost
                        // it, so both just drop their ledger entry; a timed-out
                        // one gets its load handed back to the hub.
                        for convoy_id in completed_convoys {
                            self.ephemeral.active_convoys.remove(&convoy_id);
                            if timed_out.contains(&convoy_id) {
                                warn!("Convoy {convoy_id} timed out in transit, returning its load");
                                self.refund_cargo(&convoy_id);
                            } else {
                                self.settle_cargo(&convoy_id);
                            }
                        }
                        // Despawn delivered convoy groups (mirrors air/sea routes).
                        for gid in despawn {
                            if let Err(e) = self.delete_group(&gid) {
                                warn!("failed to despawn delivered convoy group {gid}: {e:?}");
                            }
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
                    let (delivery_distance, check_interval_secs, max_transit) = match self
                        .ephemeral
                        .cfg
                        .warehouse
                        .as_ref()
                        .and_then(|w| w.air_logistics.as_ref())
                    {
                        Some(cfg) => (
                            cfg.delivery_distance,
                            cfg.check_interval_secs,
                            Duration::minutes(cfg.max_transit_minutes as i64),
                        ),
                        None => {
                            // Air logistics disabled/unconfigured — clear and move on
                            let orphaned: Vec<LogiRouteId> =
                                self.ephemeral.active_air_routes.keys().cloned().collect();
                            self.ephemeral.active_air_routes.clear();
                            for id in orphaned {
                                self.refund_cargo(&id);
                            }
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
                    let mut timed_out: Vec<LogiRouteId> = Vec::new();
                    let mut despawn: Vec<bfprotocols::db::group::GroupId> = Vec::new();
                    for route_id in self.ephemeral.active_air_routes.keys().cloned().collect::<Vec<_>>() {
                        if let Some(route) = self.ephemeral.active_air_routes.get_mut(&route_id) {
                            let route_group_id: bfprotocols::db::group::GroupId = route.group_id;
                            // Airborne far longer than the leg can possibly
                            // take: the flight is stuck holding or orbiting.
                            // Recall it and give the load back.
                            if max_transit > Duration::zero()
                                && ts - route.spawn_time > max_transit
                            {
                                timed_out.push(route_id.clone());
                                despawn.push(route_group_id);
                                completed.push(route_id.clone());
                                continue;
                            }
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
                                        despawn.push(route_group_id);
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
                        if timed_out.contains(&route_id) {
                            warn!("Air route {route_id} timed out in transit, returning its load");
                            self.refund_cargo(&route_id);
                        } else {
                            self.settle_cargo(&route_id);
                        }
                    }
                    // Despawn the cargo aircraft once it has delivered -- otherwise
                    // it loiters at the destination forever and they pile up.
                    for gid in despawn {
                        if let Err(e) = self.delete_group(&gid) {
                            warn!("failed to despawn delivered air logistics group {gid}: {e:?}");
                        }
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
                    let (delivery_distance, check_interval_secs, max_transit) = match self
                        .ephemeral
                        .cfg
                        .warehouse
                        .as_ref()
                        .and_then(|w| w.sea_logistics.as_ref())
                    {
                        Some(cfg) => (
                            cfg.delivery_distance,
                            cfg.check_interval_secs,
                            Duration::minutes(cfg.max_transit_minutes as i64),
                        ),
                        None => {
                            let orphaned: Vec<LogiRouteId> =
                                self.ephemeral.active_sea_routes.keys().cloned().collect();
                            self.ephemeral.active_sea_routes.clear();
                            for id in orphaned {
                                self.refund_cargo(&id);
                            }
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
                    let mut timed_out: Vec<LogiRouteId> = Vec::new();
                    let mut despawn: Vec<bfprotocols::db::group::GroupId> = Vec::new();
                    for route_id in self.ephemeral.active_sea_routes.keys().cloned().collect::<Vec<_>>() {
                        if let Some(route) = self.ephemeral.active_sea_routes.get_mut(&route_id) {
                            let route_group_id: bfprotocols::db::group::GroupId = route.group_id;
                            if max_transit > Duration::zero()
                                && ts - route.spawn_time > max_transit
                            {
                                timed_out.push(route_id.clone());
                                despawn.push(route_group_id);
                                completed.push(route_id.clone());
                                continue;
                            }
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
                                        despawn.push(route_group_id);
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
                        if timed_out.contains(&route_id) {
                            warn!("Sea route {route_id} timed out in transit, returning its load");
                            self.refund_cargo(&route_id);
                        } else {
                            self.settle_cargo(&route_id);
                        }
                    }
                    for gid in despawn {
                        if let Err(e) = self.delete_group(&gid) {
                            warn!("failed to despawn delivered sea logistics group {gid}: {e:?}");
                        }
                    }

                    if self.ephemeral.active_sea_routes.is_empty() {
                        let objectives = self
                            .persisted
                            .objectives
                            .into_iter()
                            .filter(|(id, obj)| {
                                !obj.kind.is_special_sam_site()
                                    && self.ephemeral.airbase_by_oid.contains_key(id)
                            })
                            .map(|(id, _)| *id)
                            .collect();
                        self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses { objectives };
                    }

                    record_perf(&mut perf.logistics_sea_routes, st);
                }
                LogiStage::SyncToWarehouses { objectives } => match objectives.pop() {
                    None => self.ephemeral.logistics_stage = LogiStage::Complete { last_tick: ts },
                    Some(oid) => {
                        // See the matching comment in SyncFromWarehouses above: this
                        // queue drains slowly and an entry's airbase registration can
                        // legitimately disappear before it's reached.
                        if self.ephemeral.airbase_by_oid.contains_key(&oid) {
                            let start_ts = Utc::now();
                            if let Err(e) = self.sync_objective_to_warehouse(lua, oid) {
                                error!("failed to sync objective {oid} to warehouse {:?}", e)
                            }
                            record_perf(&mut perf.logistics_sync_to, start_ts);
                        }
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
        let salvage = whcfg.captured_airframes.clone();
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
            let is_airframe = is_airframe_item(name.as_str());
            // A carrier's aircraft roster is its deck (naval) warehouse, not
            // the captor's whole airframe production list. On capture the
            // physical deck warehouse is untouched; a reload re-reads it into
            // the model. So here: refresh capacity on airframes the carrier
            // ALREADY has, never add new ones.
            if is_carrier && is_airframe {
                if let Some(inv) = obj.warehouse.equipment.get_mut_cow(&name) {
                    let p = production
                        .equipment
                        .get(&name)
                        .or_else(|| other_production.equipment.get(&name))
                        .map(|e| e.production)
                        .unwrap_or(1);
                    inv.capacity = whcfg.capacity(true, p);
                    if inv.stored == 0 {
                        inv.stored = inv.capacity;
                    }
                }
                return Ok(());
            }
            match production.equipment.get(&name) {
                Some(equip) => {
                    let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
                    let unlimited = if is_airframe { obj.unlimited_aircraft } else { obj.unlimited_supply };
                    let capacity = whcfg.capacity_for_item(&obj.name, is_airframe, unlimited, hub, equip.production);
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
                        if is_carrier {
                            let inv = obj.warehouse.equipment.get_or_default_cow(name);
                            // captured carrier: keep the previous owner's aircraft available
                            // with hub capacity so the new owner can operate them
                            let cap = whcfg.capacity(true, equip.production);
                            inv.capacity = cap;
                            // a retained foreign jet with 0 stock can never be
                            // slotted -- give the captor a usable load (the
                            // CapturedNotReady gate still holds it until
                            // repairs finish).
                            if inv.stored == 0 {
                                inv.stored = cap;
                            }
                        } else {
                            // A land base changes hands with the losing side's
                            // aircraft still parked on it. Salvage a fraction
                            // of what was actually there (this runs after the
                            // owner flip but before anything touches the
                            // model, so `stored` is still the previous owner's
                            // count) and let the captors fly it.
                            //
                            // capacity == the salvaged count on purpose: it is
                            // the model's ceiling, so the read-back clamp in
                            // sync_warehouse_to_obj can't let DCS inflate it,
                            // and nothing refills it -- every resupply path
                            // iterates the *source's* production, which by
                            // definition doesn't contain this type. Captured
                            // jets are a finite prize that drains to nothing.
                            let salvaged = match salvage.as_ref() {
                                Some(sc) if is_airframe && !sc.exclude.contains(name.as_str()) => {
                                    sc.salvaged(obj
                                        .warehouse
                                        .equipment
                                        .get(&name)
                                        .map(|i| i.stored)
                                        .unwrap_or(0))
                                }
                                Some(_) | None => 0,
                            };
                            if salvaged > 0 {
                                let inv = obj.warehouse.equipment.get_or_default_cow(name.clone());
                                inv.stored = salvaged;
                                inv.capacity = salvaged;
                                info!(
                                    "[WAREHOUSE_CAPTURE] {:?} obj={} salvaged {salvaged} {name} from the previous owner",
                                    obj.owner, obj.name
                                );
                            } else if obj.warehouse.equipment.get(&name).is_some() {
                                let inv = obj.warehouse.equipment.get_or_default_cow(name);
                                inv.stored = 0;
                                inv.capacity = 0;
                            }
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

    /// Nearest same-owner logistics hub for `obj`, regardless of whether
    /// `obj` is LOGISTICS_DETACHED -- detached objectives still need a
    /// supplier hub assigned (and added to that hub's destination list) so
    /// they're considered for delivery at all. deliver_supplies_from_logistics_hubs
    /// is what decides convoy vs. instant vs. air transport based on the
    /// detached flag; excluding detached objectives here instead would mean
    /// they never get any supplier and so never receive any delivery, not
    /// even a convoy.
    pub(super) fn compute_supplier(&self, obj: &Objective) -> Result<Option<ObjectiveId>> {
        let (front_line_routing, margin) = match self.ephemeral.cfg.warehouse.as_ref() {
            Some(w) => (w.front_line_routing, w.route_block_margin_m),
            None => (false, 0.),
        };
        // Rank candidates by (route cut, distance): a hub further back that
        // can actually get a convoy through beats a nearer one on the wrong
        // side of the front. Only if every hub is cut off do we fall back to
        // the nearest, and that base then relies on airlift.
        let mut best: Option<(bool, f64, ObjectiveId)> = None;
        for id in &self.persisted.logistics_hubs {
            let logi = objective!(self, id)?;
            if logi.owner != obj.owner {
                continue;
            }
            let dist = na::distance_squared(&obj.zone.pos().into(), &logi.zone.pos().into());
            let cut = front_line_routing
                && route_interdicted(
                    &self.persisted,
                    obj.owner,
                    logi.zone.pos(),
                    obj.zone.pos(),
                    margin,
                );
            let candidate = (cut, dist, *id);
            match best {
                None => best = Some(candidate),
                Some((bcut, bdist, _)) => {
                    if (cut, dist) < (bcut, bdist) {
                        best = Some(candidate);
                    }
                }
            }
        }
        match best {
            Some((true, _, id)) => {
                // Picked anyway, because every hub is behind the front, so
                // this base is airlift-only until the ground situation
                // changes. Debug rather than info: setup_supply_lines re-runs
                // on every capture, so during an offensive this would repeat
                // for every cut-off base every few minutes. The per-tick
                // [LOGI_DISPATCH] CUT OFF line is the one that matters.
                debug!(
                    "[LOGI_ROUTE] {} has no hub with a clear ground route -- nearest is {} and the \
                     road is cut, so it depends on airlift",
                    obj.name,
                    self.persisted
                        .objectives
                        .get(&id)
                        .map(|o| o.name.as_str())
                        .unwrap_or("?")
                );
            }
            Some((false, dist, id)) => debug!(
                "[LOGI_ROUTE] {} <- {} ({:.0} km, route clear)",
                obj.name,
                self.persisted
                    .objectives
                    .get(&id)
                    .map(|o| o.name.as_str())
                    .unwrap_or("?"),
                dist.sqrt() / 1000.
            ),
            None => debug!("[LOGI_ROUTE] {} has no friendly hub at all", obj.name),
        }
        Ok(best.map(|(_, _, id)| id))
    }

    pub fn setup_supply_lines(&mut self) -> Result<()> {
        let mut suppliers: SmallVec<[(ObjectiveId, Option<ObjectiveId>); 64]> = smallvec![];
        for (oid, obj) in &self.persisted.objectives {
            match obj.kind {
                ObjectiveKind::Logistics | ObjectiveKind::Factory { .. } => (),
                // A naval base is a hub for its carrier group but it is also a
                // destination in its own right -- carrier repair and respawn
                // are paid for out of its materiel stock, and nothing was ever
                // routed there, so that stock could only ever be zero.
                ObjectiveKind::NavalBase
                | ObjectiveKind::Airbase
                | ObjectiveKind::Farp { .. }
                | ObjectiveKind::Fob => {
                    let hub = self.compute_supplier(obj)?;
                    suppliers.push((*oid, hub));
                }
                ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::SpecialSamSite { .. } | ObjectiveKind::CommandCenter => (),
            }
        }
        let mut current: FxHashMap<ObjectiveId, SetS<ObjectiveId>> = FxHashMap::default();
        for oid in &self.persisted.logistics_hubs {
            let obj = objective_mut!(self, oid)?;
            current.insert(*oid, mem::take(&mut obj.warehouse.destination));
        }
        for (oid, supplier) in suppliers {
            let obj = objective_mut!(self, oid)?;
            let previous = obj.warehouse.supplier;
            obj.warehouse.supplier = supplier;
            if let Some(id) = supplier {
                objective_mut!(self, id)?
                    .warehouse
                    .destination
                    .insert_cow(oid);
            }
            // A base's supply line moving is a strategic event -- the front
            // shifted, or its depot was taken -- so it belongs in the log at
            // info, not buried in a per-tick dump.
            if previous != supplier {
                let name = |o: Option<ObjectiveId>| {
                    o.and_then(|id| self.persisted.objectives.get(&id))
                        .map(|o| o.name.to_string())
                        .unwrap_or_else(|| String::from("none").to_string())
                };
                info!(
                    "[LOGI_ROUTE] {} re-homed: supplier {} -> {}",
                    objective!(self, oid)?.name,
                    name(previous),
                    name(supplier)
                );
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

    /// A side's territory score: every objective it holds, weighted by what
    /// kind of objective it is and scaled by that objective's logistics
    /// health. A base that has been bombed flat contributes almost nothing
    /// even while the side still nominally owns it.
    fn territory_score(&self, side: Side, cfg: &ProductionScalingConfig) -> f64 {
        let mut score = 0.;
        for (_, obj) in &self.persisted.objectives {
            if obj.owner != side {
                continue;
            }
            let weight = match obj.kind {
                ObjectiveKind::Logistics => cfg.logistics_weight,
                ObjectiveKind::Factory { .. } => cfg.factory_weight,
                ObjectiveKind::CommandCenter => cfg.command_center_weight,
                ObjectiveKind::Airbase
                | ObjectiveKind::Farp { .. }
                | ObjectiveKind::Fob
                | ObjectiveKind::NavalBase => cfg.airbase_weight,
                ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::SpecialSamSite { .. } => 0.,
            };
            if weight > 0. {
                score += weight * (obj.logi as f64 / 100.);
            }
        }
        score
    }

    /// What fraction of nominal production a side is actually managing right
    /// now, as the ratio of its current territory score to the score it
    /// started the campaign with, clamped to the configured floor and
    /// ceiling. Returns 1.0 when scaling is off, which is exactly the old
    /// fixed-constant behaviour.
    fn production_output_frac(&mut self, side: Side) -> f64 {
        let cfg = match self
            .ephemeral
            .cfg
            .warehouse
            .as_ref()
            .and_then(|w| w.production_scaling.as_ref())
        {
            Some(c) if c.enabled => c.clone(),
            _ => return 1.0,
        };
        let score = self.territory_score(side, &cfg);
        // Baseline is whatever the side held the first time this ran, i.e.
        // mission start for a fresh campaign. Persisted so a restart doesn't
        // silently re-baseline a side onto whatever it happens to hold now
        // and hand it back full production after losing half the map.
        let baseline = match self.persisted.production_baseline.get(&side) {
            Some(b) if *b > 0. => *b,
            _ => {
                if score <= 0. {
                    return cfg.floor_percent as f64 / 100.;
                }
                self.persisted.production_baseline.insert_cow(side, score);
                self.ephemeral.dirty();
                score
            }
        };
        let floor = cfg.floor_percent as f64 / 100.;
        let ceiling = (cfg.ceiling_percent as f64 / 100.).max(floor);
        (score / baseline).clamp(floor, ceiling)
    }

    pub fn deliver_production(&mut self, lua: MizLua, now: DateTime<Utc>) -> Result<Vec<Transfer>> {
        if self.ephemeral.cfg.warehouse.is_none() {
            return Ok(vec![]);
        }
        self.setup_supply_lines()
            .context("setting up supply lines")?;
        let materiel_production = self
            .ephemeral
            .cfg
            .warehouse
            .as_ref()
            .and_then(|w| w.materiel.as_ref())
            .filter(|m| m.enabled)
            .map(|m| m.hub_production)
            .unwrap_or(0);
        for side in Side::ALL {
            let production = match self.ephemeral.production_by_side.get(&side) {
                Some(e) => Arc::clone(e),
                None => continue,
            };
            let scale = self.production_output_frac(side);
            if scale < 0.999 || scale > 1.001 {
                info!(
                    "[WAREHOUSE] {side:?} production running at {:.0}% of nominal",
                    scale * 100.
                );
            }
            let hubs: SmallVec<[ObjectiveId; 16]> =
                self.persisted.logistics_hubs.into_iter().copied().collect();
            for oid in hubs {
                let logi = objective_mut!(self, &oid)?;
                if logi.owner != side {
                    continue;
                }
                // A hub that has been bombed off the map doesn't receive a
                // full delivery. Previously a flattened logistics hub kept
                // taking in production at 100% -- there was no point striking
                // one beyond denying the capture.
                let intake = scale * (logi.logi as f64 / 100.);
                if intake <= 0. {
                    continue;
                }
                let portion = |qty: u32| -> u32 { (qty as f64 * intake).round() as u32 };
                let materiel_before = logi
                    .warehouse
                    .equipment
                    .get(MATERIEL_ITEM)
                    .map(|inv| inv.stored)
                    .unwrap_or(0);
                for (name, inv) in logi.warehouse.equipment.iter_mut_cow() {
                    if name.as_str() == MATERIEL_ITEM {
                        *inv += portion(materiel_production);
                    } else if let Some(eq) = production.equipment.get(name) {
                        *inv += portion(eq.production);
                    }
                }
                for (name, inv) in logi.warehouse.liquids.iter_mut_cow() {
                    if let Some(pr) = production.liquids.get(name) {
                        *inv += portion(*pr);
                    }
                }
                let materiel_after = logi
                    .warehouse
                    .equipment
                    .get(MATERIEL_ITEM)
                    .map(|inv| inv.stored)
                    .unwrap_or(0);
                info!(
                    "[WAREHOUSE] delivery to hub {} ({:?}): intake {:.0}% of nominal \
                     (production scale {:.0}%, hub logi {}%), materiel {materiel_before} -> {materiel_after} / {}",
                    logi.name,
                    side,
                    intake * 100.,
                    scale * 100.,
                    logi.logi,
                    logi.warehouse
                        .equipment
                        .get(MATERIEL_ITEM)
                        .map(|inv| inv.capacity)
                        .unwrap_or(0)
                );
            }
        }
        self.ephemeral.dirty();
        self.deliver_supplies_from_logistics_hubs(lua, now)
            .context("delivering supplies from logistics hubs")
    }

    /// Make sure every objective has a materiel stockpile sized to its role.
    /// Cheap and idempotent, so it's safe to call after init, after a load and
    /// after every capture.
    pub(super) fn sync_materiel_capacity(&mut self) {
        let Some(m) = self
            .ephemeral
            .cfg
            .warehouse
            .as_ref()
            .and_then(|w| w.materiel.as_ref())
            .filter(|m| m.enabled)
            .cloned()
        else {
            return;
        };
        let hub_cap = m.hub_production * m.hub_capacity;
        let base_cap = m.hub_production * m.airbase_capacity;
        let (mut sized, mut seeded, mut clamped) = (0u32, 0u32, 0u32);
        for (oid, obj) in self.persisted.objectives.iter_mut_cow() {
            let is_hub = obj.kind.is_hub()
                || matches!(obj.kind, ObjectiveKind::NavalBase | ObjectiveKind::Factory { .. });
            let _ = oid;
            let capacity = if obj.unlimited_supply {
                bfprotocols::cfg::UNLIMITED_CAPACITY
            } else if is_hub {
                hub_cap
            } else {
                base_cap
            };
            let inv = obj
                .warehouse
                .equipment
                .get_or_default_cow(dcso3::String::from(MATERIEL_ITEM));
            let fresh = inv.capacity == 0;
            inv.capacity = capacity;
            // A brand new stockpile starts full, the same way every other
            // warehouse entry does at init; an existing one just gets resized.
            if fresh {
                inv.stored = capacity;
                seeded += 1;
            } else if inv.stored > capacity {
                inv.stored = capacity;
                clamped += 1;
            }
            sized += 1;
        }
        info!(
            "[MATERIEL] sized {sized} stockpile(s) (hub {hub_cap} / base {base_cap} units): \
             {seeded} newly seeded full, {clamped} clamped down to the new capacity"
        );
        self.ephemeral.dirty();
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
    ) -> Result<Option<CompactString>> {
        let cfg = match &self.ephemeral.cfg.warehouse {
            Some(w) => w,
            None => return Ok(None),
        };

        let convoy_cfg = match &cfg.convoy {
            Some(c) if c.enabled => c,
            _ => return Ok(None),
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
                return Ok(None);
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
        use dcso3::controller::{Task, MissionPoint, PointType, ActionTyp, VehicleFormation, AltType};
        use dcso3::LuaVec2;
        use dcso3::land::Land;
        use dcso3::env::miz::Miz;
        use crate::db::group::DeployKind;
        use enumflags2::BitFlags;

        let spawn_ctx = SpawnCtx::new(lua).context("convoy: spawn ctx")?;
        let miz = Miz::singleton(lua).context("convoy: miz singleton")?;
        let idx = miz.index().context("convoy: miz index")?;
        let land = Land::singleton(lua).context("convoy: land singleton")?;

        // Use add_group to spawn the convoy
        let group_id = self
            .add_group(
                &spawn_ctx,
                &idx,
                side,
                SpawnLoc::AtPos {
                    pos: origin_pos,
                    // a real direction so the trucks aren't all stacked on the
                    // origin point (a zero vector left them piled up)
                    offset_direction: {
                        let d = dest_pos - origin_pos;
                        let n = d.norm();
                        if n > 1.0 { d / n } else { Vector2::new(1.0, 0.0) }
                    },
                    group_heading: heading,
                },
                &truck_template,
                DeployKind::Objective { origin },
                BitFlags::empty(),
            )
            .with_context(|| {
                format_compact!(
                    "convoy: add_group template '{truck_template}' side {side:?} {origin_name} -> {dest_name}"
                )
            })?;

        // The group is only queued for spawn at this point -- it does not exist
        // in DCS yet, so we cannot fetch it with Group::get_by_name. Instead we
        // build the road route here and hand it to spawn_group, which bakes the
        // route into the group at actual spawn time (same pattern as
        // add_and_spawn_ai_air).
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
                // DCS's findPathOnRoads returns the raw road polyline -- often
                // thousands of vertices. A route that big chokes the group AI
                // (it just sits at the origin). Decimate to a waypoint roughly
                // every 3 km (and hard-cap the count); "On Road" formation makes
                // DCS follow the actual road between the sparse points anyway.
                const MIN_SPACING_M: f64 = 3000.0;
                const MAX_WAYPOINTS: usize = 60;
                let pts: Vec<LuaVec2> = path.into_iter().filter_map(|wp| wp.ok()).collect();
                let mut last_kept: Option<LuaVec2> = None;
                let mut wp_count = 0;
                for (i, wp) in pts.iter().enumerate() {
                    let far_enough = last_kept
                        .map(|lk| na::distance(&lk.0.into(), &wp.0.into()) >= MIN_SPACING_M)
                        .unwrap_or(true);
                    // always keep the last polyline point so we actually reach
                    // the road exit nearest the destination
                    let is_last = i + 1 == pts.len();
                    if (far_enough || is_last) && wp_count < MAX_WAYPOINTS {
                        let alt = land.get_height(*wp).unwrap_or(0.0);
                        route_points.push(MissionPoint {
                            action: Some(ActionTyp::Ground(VehicleFormation::OnRoad)),
                            airdrome_id: None,
                            helipad: None,
                            typ: PointType::TurningPoint,
                            link_unit: None,
                            pos: *wp,
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
                        last_kept = Some(*wp);
                        wp_count += 1;
                    }
                }
                if wp_count > 0 {
                    info!(
                        "Convoy {} using road path: {} raw pts -> {} waypoints",
                        convoy_id,
                        pts.len(),
                        wp_count
                    );
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

        // Spawn the queued group now, with the road route baked in.
        {
            let perf = unsafe { Perf::get_mut() };
            let perf = Arc::make_mut(&mut perf.inner);
            self.ephemeral
                .spawn_group(
                    perf,
                    &self.persisted,
                    &idx,
                    &spawn_ctx,
                    group!(self, group_id)?,
                    route_points,
                )
                .with_context(|| {
                    format_compact!("convoy: spawn_group '{truck_template}' {origin_name} -> {dest_name}")
                })?;
        }

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

        Ok(Some(convoy_id))
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
    ) -> Result<Option<CompactString>> {
        let cfg = match &self.ephemeral.cfg.warehouse {
            Some(w) => w,
            None => return Ok(None),
        };

        let air_cfg = match &cfg.air_logistics {
            Some(c) if c.enabled => c,
            _ => return Ok(None),
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
                    return Ok(None);
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

        {
            let perf = unsafe { Perf::get_mut() };
            let perf = Arc::make_mut(&mut perf.inner);
            self.ephemeral.spawn_group(
                perf,
                &self.persisted,
                &idx,
                &spawn_ctx,
                group!(self, group_id)?,
                route_points,
            )?;
        }

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

        Ok(Some(route_id))
    }

    /// Nearest objective owned by `side` that can actually launch a
    /// ground-starting flight, for use as a helo mission's launch point.
    /// `exclude` skips the destination itself -- ferrying troops onto a base's
    /// own runway makes no sense.
    ///
    /// `is_airbase()` alone is not enough: it is a static check on the
    /// objective's kind, and an Airbase-kind objective whose live DCS airbase
    /// doesn't resolve gives `spawn_group` nothing to build a parking start
    /// from, so the helo air-starts. Candidates are checked nearest-first
    /// against the same resolver the spawn path uses, so the field we pick is
    /// one that will really put the helo on the ramp.
    /// The closest friendly field a helo mission can launch from. Deliberately
    /// does NOT require a live DCS airbase: most FOBs have no airbase or FARP
    /// pad object at all, and demanding one walked past every nearby field to
    /// launch from one 150km away. A helicopter with no pad to park on starts
    /// from open ground instead (`ephemeral::spawn_group`'s `TakeOffGroundHot`
    /// fallback), so the only thing that matters here is that we own the place
    /// and it's the kind of place helicopters operate out of. Carrier decks are
    /// left out: their deck airbase moves with the ship.
    fn nearest_helo_launch_field(
        &self,
        lua: MizLua,
        side: Side,
        near: Vector2,
        exclude: Option<ObjectiveId>,
    ) -> Option<ObjectiveId> {
        let mut candidates: Vec<(&ObjectiveId, f64)> = self
            .persisted
            .objectives
            .into_iter()
            .filter(|(oid, o)| {
                o.owner() == side
                    && matches!(
                        o.kind,
                        ObjectiveKind::Airbase | ObjectiveKind::Farp { .. } | ObjectiveKind::Fob
                    )
                    && exclude.map(|e| **oid != e).unwrap_or(true)
            })
            .map(|(oid, o)| (oid, na::distance_squared(&o.pos().into(), &near.into())))
            .collect();
        candidates.sort_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let (oid, d2) = candidates.first()?;
        let pad = self
            .ephemeral
            .resolve_airbase(lua, &self.persisted, oid)
            .is_some();
        info!(
            "[HELO_MISSION] launch field {} ({:.0}km from the target, {}) chosen from {} friendly field(s)",
            self.persisted.objectives.get(oid).map(|o| o.name.as_str()).unwrap_or("?"),
            d2.sqrt() / 1000.,
            if pad { "has a DCS pad" } else { "no DCS pad, ground start" },
            candidates.len()
        );
        Some(**oid)
    }

    /// A batch of whatever surplus supply `origin`'s warehouse has on hand,
    /// capped per item, for a resource-delivery helo mission to carry.
    fn build_helo_supply_transfer(
        &self,
        origin: ObjectiveId,
        destination: ObjectiveId,
        per_item_cap: u32,
    ) -> Result<Vec<Transfer>> {
        let origin_obj = objective!(self, &origin)?;
        let mut transfers = Vec::new();
        for (name, inv) in origin_obj.warehouse().equipment() {
            if inv.stored > 0 {
                transfers.push(Transfer {
                    source: origin,
                    target: destination,
                    amount: inv.stored.min(per_item_cap),
                    item: TransferItem::Equipment(name.clone()),
                });
            }
        }
        for (name, inv) in origin_obj.warehouse().liquids() {
            if inv.stored > 0 {
                transfers.push(Transfer {
                    source: origin,
                    target: destination,
                    amount: inv.stored.min(per_item_cap),
                    item: TransferItem::Liquid(*name),
                });
            }
        }
        Ok(transfers)
    }

    /// Spawn the AI helicopter for a helo mission: cold-starts from `origin`
    /// (same `TakeOffParkingHot` mechanism as reactive CAP -- see the
    /// `UnitTag::HotStart` handling in `ephemeral::spawn_group`) and flies to
    /// a real landing at `destination`. Uses `helo_insertion.aircraft_template`
    /// -- deliberately its own field, separate from
    /// `warehouse.air_logistics.aircraft_template` (fixed-wing cargo planes
    /// between hubs; these missions land at arbitrary objectives in the
    /// open, not just airfields, so they need a real helicopter).
    fn spawn_helo_mission(
        &mut self,
        lua: MizLua,
        origin: ObjectiveId,
        destination: ObjectiveId,
        player: dcso3::net::Ucid,
        kind: HeloMissionKind,
        now: DateTime<Utc>,
    ) -> Result<HeloMissionId> {
        let helo_cfg = self
            .ephemeral
            .cfg
            .helo_insertion
            .clone()
            .ok_or_else(|| anyhow!("AI helo missions are not enabled"))?;

        let origin_obj = objective!(self, &origin)?;
        let dest_obj = objective!(self, &destination)?;
        let side = origin_obj.owner();
        let origin_pos = origin_obj.pos();
        let dest_pos = dest_obj.pos();
        let origin_name = origin_obj.name.clone();
        let dest_name = dest_obj.name.clone();

        let (aircraft_template, altitude_m, speed_kph) =
            match helo_cfg.aircraft_template.get(&side) {
                Some(t) => (t.clone(), helo_cfg.altitude_m, helo_cfg.speed_kph),
                None => bail!(
                    "no helo_insertion.aircraft_template configured for side {:?}",
                    side
                ),
            };

        let mission_id: HeloMissionId = format_compact!(
            "HELO_{}_{}_{}",
            side.to_str(),
            self.ephemeral.helo_mission_counter,
            now.timestamp()
        );
        self.ephemeral.helo_mission_counter += 1;

        let delta = dest_pos - origin_pos;
        let heading = delta.y.atan2(delta.x);
        let speed_mps = speed_kph / 3.6;

        use crate::db::group::DeployKind;
        use crate::spawnctx::{SpawnCtx, SpawnLoc};
        use bfprotocols::cfg::UnitTag;
        use dcso3::controller::{ActionTyp, AltType, MissionPoint, PointType, Task, TurnMethod};
        use dcso3::env::miz::Miz;
        use dcso3::LuaVec2;

        let spawn_ctx = SpawnCtx::new(lua)?;
        let miz = Miz::singleton(lua)?;
        let idx = miz.index()?;

        // `DeployKind::Objective { origin }` + the `HotStart` tag is what
        // makes `ephemeral::spawn_group` rewrite waypoint 0 into a real
        // parking start at the resolved origin airbase -- same path reactive
        // CAP uses. WP0 below is a placeholder the rewrite replaces; WP1 is
        // the only waypoint that matters as written, an actual Land at the
        // destination (not a "get within N metres while still flying" cruise
        // point like the auto-dispatch air logistics route uses).
        let group_id = self.add_group(
            &spawn_ctx,
            &idx,
            side,
            SpawnLoc::AtPos {
                pos: origin_pos,
                offset_direction: Vector2::new(1., 0.),
                group_heading: heading,
            },
            &aircraft_template,
            DeployKind::Objective { origin },
            UnitTag::HotStart.into(),
        )?;

        // Terrain elevation at the destination: a Land waypoint's altitude is
        // the ground it lands on, not sea level.
        let dest_alt = dcso3::land::Land::singleton(lua)
            .and_then(|l| l.get_height(LuaVec2(dest_pos)))
            .unwrap_or(0.0);

        let route_points = vec![
            MissionPoint {
                // Overwritten by the HotStart rewrite in `spawn_group` when the
                // origin field resolves; a plain fly-over is the sane fallback
                // if it doesn't, since an action-less waypoint is one DCS may
                // decline to fly at all.
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
                // A Land waypoint DCS will actually fly needs the same shape
                // the mission editor writes: action "Landing", the field
                // elevation as its altitude, and a real transit speed. Left as
                // action-less at zero speed the group spawns on the ramp with
                // a route it won't fly -- engines running, never lifts off.
                action: Some(ActionTyp::Air(TurnMethod::Landing)),
                airdrome_id: None,
                helipad: None,
                typ: PointType::Land,
                link_unit: None,
                pos: LuaVec2(dest_pos),
                alt: dest_alt,
                alt_typ: Some(AltType::BARO),
                // Sit on the ground for up to 10 minutes -- plenty of margin
                // for the mission-poll tick (every ~10s) to see it landed and
                // apply the payoff before DCS would otherwise send it home.
                time_re_fu_ar: Some(600),
                eta: None,
                eta_locked: None,
                speed: speed_mps,
                speed_locked: None,
                name: None,
                task: Box::new(Task::ComboTask(vec![])),
            },
        ];

        {
            let perf = unsafe { Perf::get_mut() };
            let perf = Arc::make_mut(&mut perf.inner);
            self.ephemeral.spawn_group(
                perf,
                &self.persisted,
                &idx,
                &spawn_ctx,
                group!(self, group_id)?,
                route_points,
            )?;
        }

        let mission = HeloMission {
            id: mission_id.clone(),
            group_id,
            kind,
            origin,
            destination,
            side,
            player,
            spawn_time: now,
            state: HeloMissionState::InTransit,
            last_pos: origin_pos,
            last_check: now,
        };
        info!(
            "[HELO_MISSION] {} dispatched from {} to {}",
            mission_id, origin_name, dest_name
        );
        self.ephemeral.active_helo_missions.insert(mission_id.clone(), mission);
        Ok(mission_id)
    }

    /// F10-callable: send an AI helicopter to insert a fresh troop group at
    /// `destination`. Costs the troop's own `cost` plus
    /// `helo_insertion.troop_mission_cost`. Cold-starts from the nearest
    /// friendly airbase and only deploys the troops once actually landed.
    pub fn call_helo_troop_insertion(
        &mut self,
        lua: MizLua,
        side: Side,
        ucid: dcso3::net::Ucid,
        destination: ObjectiveId,
        now: DateTime<Utc>,
    ) -> Result<HeloMissionId> {
        let cfg = self
            .ephemeral
            .cfg
            .helo_insertion
            .clone()
            .ok_or_else(|| anyhow!("AI helo missions are not enabled"))?;
        let dest_obj = objective!(self, &destination)?;
        if dest_obj.owner() == side && !dest_obj.captureable() {
            bail!("{} is already yours and isn't under threat", dest_obj.name);
        }
        let dest_pos = dest_obj.pos();
        let origin = self
            .nearest_helo_launch_field(lua, side, dest_pos, Some(destination))
            .ok_or_else(|| {
                anyhow!("no friendly field able to launch the mission (a launch field has to be an airbase, FARP or FOB with a live DCS pad)")
            })?;
        let origin_obj = objective!(self, &origin)?;
        let range = na::distance(&origin_obj.pos().into(), &dest_pos.into());
        if range > cfg.max_range_m {
            bail!(
                "nearest friendly launch field ({}) is {:.0}km away, past the {:.0}km max range",
                origin_obj.name,
                range / 1000.,
                cfg.max_range_m / 1000.
            );
        }
        let troop_cfg = self
            .ephemeral
            .deployable_idx
            .get(&side)
            .ok_or_else(|| anyhow!("no troops configured for {:?}", side))?
            .squads_by_name
            .get(cfg.troop_name.as_str())
            .ok_or_else(|| {
                anyhow!(
                    "configured helo_insertion troop '{}' not found for {:?}",
                    cfg.troop_name,
                    side
                )
            })?
            .clone();
        let total_cost = troop_cfg.cost as i32 + cfg.troop_mission_cost;
        let available = self.player(&ucid).map(|p| p.points).unwrap_or(0);
        if available < total_cost {
            bail!(
                "not enough points for a helo troop insertion ({total_cost} needed, {available} \
                 available)"
            );
        }
        let mission_id = self.spawn_helo_mission(
            lua,
            origin,
            destination,
            ucid.clone(),
            HeloMissionKind::TroopInsertion,
            now,
        )?;
        self.adjust_points(&ucid, -total_cost, "AI helo troop insertion");
        Ok(mission_id)
    }

    /// F10-callable: send an AI helicopter loaded with surplus supply from
    /// the nearest friendly hub to `destination`. Costs
    /// `helo_insertion.supply_mission_cost`.
    pub fn call_helo_resource_delivery(
        &mut self,
        lua: MizLua,
        side: Side,
        ucid: dcso3::net::Ucid,
        destination: ObjectiveId,
        now: DateTime<Utc>,
    ) -> Result<HeloMissionId> {
        let cfg = self
            .ephemeral
            .cfg
            .helo_insertion
            .clone()
            .ok_or_else(|| anyhow!("AI helo missions are not enabled"))?;
        let dest_obj = objective!(self, &destination)?;
        let dest_pos = dest_obj.pos();
        let origin = self
            .nearest_helo_launch_field(lua, side, dest_pos, Some(destination))
            .ok_or_else(|| {
                anyhow!("no friendly field able to launch the mission (a launch field has to be an airbase, FARP or FOB with a live DCS pad)")
            })?;
        let origin_obj = objective!(self, &origin)?;
        let range = na::distance(&origin_obj.pos().into(), &dest_pos.into());
        if range > cfg.max_range_m {
            bail!(
                "nearest friendly hub ({}) is {:.0}km away, past the {:.0}km max range",
                origin_obj.name,
                range / 1000.,
                cfg.max_range_m / 1000.
            );
        }
        let transfers =
            self.build_helo_supply_transfer(origin, destination, cfg.supply_amount_per_item)?;
        if transfers.is_empty() {
            bail!("{} has no surplus supply on hand to send", origin_obj.name);
        }
        let available = self.player(&ucid).map(|p| p.points).unwrap_or(0);
        if available < cfg.supply_mission_cost {
            bail!(
                "not enough points for a helo supply run ({} needed, {available} available)",
                cfg.supply_mission_cost
            );
        }
        let mission_id = self.spawn_helo_mission(
            lua,
            origin,
            destination,
            ucid.clone(),
            HeloMissionKind::ResourceDelivery { transfers },
            now,
        )?;
        self.adjust_points(&ucid, -cfg.supply_mission_cost, "AI helo resource delivery");
        Ok(mission_id)
    }

    /// Poll all active helo missions: despawn destroyed ones, and on landing
    /// apply the payoff (deploy troops / transfer supply) before despawning.
    /// Called once per slow tick from `lib.rs`, same cadence as convoys.
    pub fn tick_helo_missions(&mut self, lua: MizLua, now: DateTime<Utc>) -> Result<()> {
        let Some(cfg) = self.ephemeral.cfg.helo_insertion.clone() else {
            return Ok(());
        };
        let landing_radius = cfg.landing_radius_m;

        let mut completed: SmallVec<[HeloMissionId; 4]> = smallvec![];
        let mut despawn: SmallVec<[bfprotocols::db::group::GroupId; 4]> = smallvec![];
        #[allow(clippy::type_complexity)]
        let mut to_deploy_troops: SmallVec<
            [(Vector2, dcso3::String, Side, dcso3::net::Ucid, ObjectiveId); 2],
        > = smallvec![];
        let mut to_transfer: SmallVec<[Vec<Transfer>; 2]> = smallvec![];

        for mission_id in self
            .ephemeral
            .active_helo_missions
            .keys()
            .cloned()
            .collect::<Vec<_>>()
        {
            let Some(mission) = self.ephemeral.active_helo_missions.get_mut(&mission_id) else {
                continue;
            };
            if (now - mission.last_check).num_seconds() < 10 {
                continue;
            }
            mission.last_check = now;

            let group_name = match group!(self, &mission.group_id) {
                Ok(g) => g.name.clone(),
                Err(_) => {
                    warn!("[HELO_MISSION] {} group not found in database", mission_id);
                    completed.push(mission_id.clone());
                    continue;
                }
            };
            let dest_pos = match self.persisted.objectives.get(&mission.destination) {
                Some(o) => o.pos(),
                None => {
                    warn!("[HELO_MISSION] {} destination no longer exists", mission_id);
                    completed.push(mission_id.clone());
                    continue;
                }
            };

            match mission.poll(lua, &group_name, dest_pos, landing_radius) {
                HeloMissionState::InTransit => {}
                HeloMissionState::Destroyed => {
                    info!("[HELO_MISSION] {} destroyed en route", mission_id);
                    completed.push(mission_id.clone());
                }
                HeloMissionState::Delivered => {
                    info!("[HELO_MISSION] {} landed and delivered", mission_id);
                    match &mission.kind {
                        HeloMissionKind::TroopInsertion => {
                            to_deploy_troops.push((
                                dest_pos,
                                cfg.troop_name.clone(),
                                mission.side,
                                mission.player,
                                mission.origin,
                            ));
                        }
                        HeloMissionKind::ResourceDelivery { transfers } => {
                            to_transfer.push(transfers.clone());
                        }
                    }
                    despawn.push(mission.group_id);
                    completed.push(mission_id.clone());
                }
            }
        }

        for id in completed {
            self.ephemeral.active_helo_missions.remove(&id);
        }
        for gid in despawn {
            if let Err(e) = self.delete_group(&gid) {
                warn!("failed to despawn delivered helo mission group {gid}: {e:?}");
            }
        }
        if !to_deploy_troops.is_empty() {
            let miz = dcso3::env::miz::Miz::singleton(lua)?;
            let idx = miz.index()?;
            for (pos, troop, side, ucid, origin) in to_deploy_troops {
                if let Err(e) = self.paratroops_to_point(lua, &idx, pos, troop, side, ucid, origin)
                {
                    warn!("helo troop insertion failed to deploy: {e:?}");
                }
            }
        }
        for transfers in to_transfer {
            for t in &transfers {
                if let Err(e) = t.execute(&mut self.persisted, &self.ephemeral.to_bg) {
                    warn!("helo resource delivery transfer failed: {e:?}");
                }
            }
        }
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
    ) -> Result<Option<CompactString>> {
        let cfg = match &self.ephemeral.cfg.warehouse {
            Some(w) => w,
            None => return Ok(None),
        };

        let sea_cfg = match &cfg.sea_logistics {
            Some(c) if c.enabled => c,
            _ => return Ok(None),
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
                return Ok(None);
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

        {
            let perf = unsafe { Perf::get_mut() };
            let perf = Arc::make_mut(&mut perf.inner);
            self.ephemeral.spawn_group(
                perf,
                &self.persisted,
                &idx,
                &spawn_ctx,
                group!(self, group_id)?,
                route_points,
            )?;
        }

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

        Ok(Some(route_id))
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

        // ── Transport budget for this tick ────────────────────────────────
        // `max_concurrent_convoys`, the convoy `spawn_interval_ticks` and the
        // new per-destination cooldown are read here. They were configured
        // but never actually consulted, so dispatch was completely
        // unthrottled: every hub launched a fresh weapons convoy AND a fresh
        // fuel convoy to every destination that was even one round short, on
        // every tick. Air routes get the same treatment, as a budget that
        // decrements as we hand loads out rather than a per-hub snapshot.
        let tick_minutes = self.ephemeral.cfg.warehouse.as_ref().map(|w| w.tick).unwrap_or(10) as i64;
        let (
            convoy_max_concurrent,
            convoy_spawn_interval_ticks,
            convoy_cooldown_ticks,
        ) = match self.ephemeral.cfg.warehouse.as_ref().and_then(|w| w.convoy.as_ref()) {
            Some(c) => (
                c.max_concurrent_convoys as usize,
                c.spawn_interval_ticks as i64,
                c.dispatch_cooldown_ticks as i64,
            ),
            None => (10, 2, 2),
        };
        let (air_supply_threshold, air_max_concurrent, air_spawn_interval_ticks) =
            match self.ephemeral.cfg.warehouse.as_ref().and_then(|w| w.air_logistics.as_ref()) {
                Some(a) => (
                    a.supply_threshold,
                    a.max_concurrent_routes as usize,
                    a.spawn_interval_ticks as i64,
                ),
                None => (50, 6, 3),
            };
        // A hub keeps an operational reserve rather than emptying itself into
        // the first convoy that asks. Without it one lost convoy leaves the
        // whole theatre dry until the next production delivery.
        let reserve_frac = self
            .ephemeral
            .cfg
            .warehouse
            .as_ref()
            .map(|w| w.hub_reserve_percent.min(100) as f32 / 100.)
            .unwrap_or(0.);
        let releasable = move |inv: &Inventory| -> u32 {
            inv.stored
                .saturating_sub((inv.capacity as f32 * reserve_frac) as u32)
        };
        let (front_line_routing, route_margin) = match self.ephemeral.cfg.warehouse.as_ref() {
            Some(w) => (w.front_line_routing, w.route_block_margin_m),
            None => (false, 0.),
        };
        let dispatch_cooldown = Duration::minutes(tick_minutes * convoy_cooldown_ticks);
        let convoy_spawn_interval = Duration::minutes(tick_minutes * convoy_spawn_interval_ticks);
        let air_spawn_interval = Duration::minutes(tick_minutes * air_spawn_interval_ticks);
        let mut convoy_budget: FxHashMap<Side, usize> = FxHashMap::default();
        let mut air_budget: FxHashMap<Side, usize> = FxHashMap::default();
        for side in Side::ALL {
            let convoys_out = self.ephemeral.active_convoys.values().filter(|c| c.side == side).count();
            let side_ready = self
                .ephemeral
                .last_convoy_spawn
                .get(&side)
                .map(|t| now - *t >= convoy_spawn_interval)
                .unwrap_or(true);
            convoy_budget.insert(
                side,
                if convoy_enabled && side_ready {
                    convoy_max_concurrent.saturating_sub(convoys_out)
                } else {
                    0
                },
            );
            let air_out = self.ephemeral.active_air_routes.values().filter(|r| r.side == side).count();
            let air_ready = self
                .ephemeral
                .last_air_route_spawn
                .get(&side)
                .map(|t| now - *t >= air_spawn_interval)
                .unwrap_or(true);
            air_budget.insert(
                side,
                if air_enabled && air_ready {
                    air_max_concurrent.saturating_sub(air_out)
                } else {
                    0
                },
            );
        }

        for lid in hub_ids {
            let logi = objective!(self, &lid)?;
            let hub_side = logi.owner;

            // Split destinations into instant transfer, convoy, or air route
            let mut instant_needed: SmallVec<[Needed; 64]> = SmallVec::new();
            let mut convoy_needed: SmallVec<[Needed; 64]> = SmallVec::new();
            let mut air_needed: SmallVec<[Needed; 64]> = SmallVec::new();

            // Service the most starved destinations first, so a limited number
            // of transports goes where it actually matters instead of to
            // whichever objective happened to iterate first.
            let mut candidates: SmallVec<[(&ObjectiveId, &Objective, u8, bool); 64]> =
                SmallVec::new();
            // Why each destination did or didn't get a run this tick. The
            // dispatch rules are a stack of budgets, cooldowns and route
            // checks, and without this the only observable symptom of any of
            // them firing is a base that quietly never gets resupplied.
            let mut n_dest = 0usize;
            let mut n_full = 0usize;
            let mut n_detached = 0usize;
            let mut n_inbound = 0usize;
            let mut n_cooling = 0usize;
            let mut n_cut_nolift = 0usize;
            let mut n_no_transport = 0usize;
            for oid in logi.warehouse.destination.into_iter() {
                if let Some(obj) = self.persisted.objectives.get(oid) {
                    if logi.owner != obj.owner {
                        continue;
                    }
                    n_dest += 1;
                    if obj.supply >= 100 && obj.fuel >= 100 && obj.aircraft >= 100 {
                        n_full += 1;
                        continue;
                    }
                    // LOGISTICS_DETACHED = the objective is cut off from the
                    // automatic supply chain: no convoy, no air, no instant.
                    // Players resupply it by hand (Base Supply crates, C-130
                    // airdrop).
                    if obj.logistics_detached {
                        n_detached += 1;
                        continue;
                    }
                    // Don't stack a second load on a destination that already
                    // has one inbound, and leave a cooldown between runs so a
                    // base isn't served by a fresh convoy pair every tick.
                    let inbound = self
                        .ephemeral
                        .active_convoys
                        .values()
                        .any(|c| c.destination == *oid)
                        || self
                            .ephemeral
                            .active_air_routes
                            .values()
                            .any(|r| r.destination == *oid);
                    let cooling = self
                        .ephemeral
                        .last_dispatch_to
                        .get(oid)
                        .map(|t| now - *t < dispatch_cooldown)
                        .unwrap_or(false);
                    if inbound || cooling {
                        if inbound {
                            n_inbound += 1;
                        } else {
                            n_cooling += 1;
                        }
                        debug!(
                            "[LOGI_DISPATCH] {} -> {}: skipped ({})",
                            logi.name,
                            obj.name,
                            if inbound { "load already inbound" } else { "in dispatch cooldown" }
                        );
                        continue;
                    }
                    // Can a convoy physically get there? If enemy ground
                    // sits astride the route, the road is closed and the only
                    // way in is by air.
                    let road_cut = front_line_routing
                        && route_interdicted(
                            &self.persisted,
                            obj.owner,
                            logi.zone.pos(),
                            obj.zone.pos(),
                            route_margin,
                        );
                    candidates.push((
                        oid,
                        obj,
                        min(obj.supply, min(obj.fuel, obj.aircraft)),
                        road_cut,
                    ));
                }
            }
            candidates.sort_by_key(|(_, _, worst, _)| *worst);

            for (oid, obj, worst, road_cut) in candidates {
                let needed = Needed {
                    oid,
                    obj,
                    demanded: 0,
                    allocated: 0,
                };
                // An airlift is the priority-relief mode: it goes to the
                // destinations that are genuinely starving, and it works over
                // ground the enemy holds. Everything else goes by road. These
                // used to be mutually exclusive -- with convoys enabled the
                // air branch was unreachable, so the cargo aircraft never
                // flew at all no matter how low a base got.
                let critical = worst < air_supply_threshold;
                let air_left = air_budget.get(&hub_side).copied().unwrap_or(0);
                let convoy_left = convoy_budget.get(&hub_side).copied().unwrap_or(0);
                if air_enabled && (critical || road_cut) && air_left > 0 {
                    air_budget.insert(hub_side, air_left - 1);
                    air_needed.push(needed);
                    debug!(
                        "[LOGI_DISPATCH] {} -> {}: AIR (worst {worst}%, critical={critical}, road_cut={road_cut}, {} slot(s) left)",
                        logi.name,
                        obj.name,
                        air_left - 1
                    );
                } else if convoy_enabled && !road_cut && convoy_left > 0 {
                    convoy_budget.insert(hub_side, convoy_left - 1);
                    convoy_needed.push(needed);
                    debug!(
                        "[LOGI_DISPATCH] {} -> {}: ROAD (worst {worst}%, {} slot(s) left)",
                        logi.name,
                        obj.name,
                        convoy_left - 1
                    );
                } else if road_cut {
                    n_cut_nolift += 1;
                    info!(
                        "[LOGI_DISPATCH] {} -> {}: CUT OFF -- enemy ground astride the route and no airlift slot free (worst {worst}%, air budget {air_left})",
                        logi.name, obj.name
                    );
                } else if !convoy_enabled && !air_enabled {
                    // No transport system at all: fall back to the original
                    // instant warehouse-to-warehouse transfer.
                    instant_needed.push(needed);
                } else {
                    n_no_transport += 1;
                    info!(
                        "[LOGI_DISPATCH] {} -> {}: WAITING -- every transport slot is busy (worst {worst}%, road {convoy_left}, air {air_left})",
                        logi.name, obj.name
                    );
                }
                // Otherwise every transport slot is busy -- the supplies stay
                // at the hub and wait for the next tick, which is the point of
                // having a finite transport fleet.
            }

            // One line per hub per tick: the whole dispatch decision, greppable
            // as [LOGI_DISPATCH]. Turn on debug for the per-destination detail.
            info!(
                "[LOGI_DISPATCH] {} ({:?}): {n_dest} destination(s) -- {} sent by road, {} by air, \
                 {} instant | skipped: {n_full} full, {n_detached} detached, {n_inbound} inbound, \
                 {n_cooling} cooling, {n_cut_nolift} cut off, {n_no_transport} no transport | \
                 budget left road {} air {}",
                logi.name,
                hub_side,
                convoy_needed.len(),
                air_needed.len(),
                instant_needed.len(),
                convoy_budget.get(&hub_side).copied().unwrap_or(0),
                air_budget.get(&hub_side).copied().unwrap_or(0),
            );

            let mut needed = instant_needed;
            macro_rules! schedule_transfers {
                ($typ:expr, $from:ident, $get:ident) => {
                    for (name, inv) in &logi.warehouse.$from {
                        if releasable(inv) == 0 {
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
                        let mut have = releasable(inv);
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
                    if releasable(inv) == 0 {
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
                    let mut have = releasable(inv);
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
                    if releasable(inv) == 0 {
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
                    let mut have = releasable(inv);
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
                    if releasable(inv) == 0 {
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
                    let mut have = releasable(inv);
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
                    if releasable(inv) == 0 {
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
                    let mut have = releasable(inv);
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

        // Spawn the collected convoys and air routes. The stock is debited
        // only once the transport has actually spawned -- doing it the other
        // way round meant a spawn that turned into a no-op (no truck or
        // aircraft template configured for that side) silently destroyed the
        // load. On success it goes into the persisted in-flight ledger so a
        // mission restart refunds it instead of deleting it.
        for route_info in convoys_to_spawn {
            match self.spawn_supply_convoy(
                lua,
                route_info.origin,
                route_info.destination,
                route_info.cargo_type,
                route_info.transfers.clone(),
                now,
            ) {
                Ok(Some(id)) => {
                    self.escrow_cargo(
                        &id,
                        route_info.origin,
                        route_info.destination,
                        &route_info.transfers,
                        now,
                    );
                    self.ephemeral
                        .last_dispatch_to
                        .insert(route_info.destination, now);
                }
                Ok(None) => (),
                Err(e) => error!("Failed to spawn {:?} convoy: {:?}", route_info.cargo_type, e),
            }
        }

        for route_info in air_routes_to_spawn {
            match self.spawn_air_logistics_route(
                lua,
                route_info.origin,
                route_info.destination,
                route_info.cargo_type,
                route_info.transfers.clone(),
                now,
            ) {
                Ok(Some(id)) => {
                    self.escrow_cargo(
                        &id,
                        route_info.origin,
                        route_info.destination,
                        &route_info.transfers,
                        now,
                    );
                    self.ephemeral
                        .last_dispatch_to
                        .insert(route_info.destination, now);
                }
                Ok(None) => (),
                Err(e) => error!("Failed to spawn {:?} air route: {:?}", route_info.cargo_type, e),
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
                match self.spawn_sea_logistics_route(
                    lua,
                    route_info.origin,
                    route_info.destination,
                    route_info.cargo_type,
                    route_info.transfers.clone(),
                    now,
                ) {
                    Ok(Some(id)) => {
                        self.escrow_cargo(
                            &id,
                            route_info.origin,
                            route_info.destination,
                            &route_info.transfers,
                            now,
                        );
                        self.ephemeral
                            .last_dispatch_to
                            .insert(route_info.destination, now);
                    }
                    Ok(None) => (),
                    Err(e) => {
                        error!("Failed to spawn {:?} sea route: {:?}", route_info.cargo_type, e)
                    }
                }
            }
        }

        Ok(transfers)
    }

    pub fn run_factory_production(&mut self, now: DateTime<Utc>) -> Result<()> {
        let interval = match &self.ephemeral.cfg.factory {
            Some(c) => Duration::seconds(c.production_interval as i64),
            None => return Ok(()),
        };
        let materiel_enabled = self
            .ephemeral
            .cfg
            .warehouse
            .as_ref()
            .and_then(|w| w.materiel.as_ref())
            .map(|m| m.enabled)
            .unwrap_or(false);
        // Which factories produced this cycle, and how much. Their output is
        // then trucked to the nearest friendly hub -- a factory used to pile
        // stock into its own warehouse where nothing could ever collect it,
        // so building or bombing one changed nothing at all.
        let mut produced: SmallVec<[(ObjectiveId, Side, u32); 8]> = smallvec![];
        for (oid, obj) in self.persisted.objectives.iter_mut_cow() {
            let ObjectiveKind::Factory { production_rate, last_production_ts } = &mut obj.kind else {
                continue;
            };
            // Only produce if operational: health > 0, logi > 0, not neutral
            if obj.health == 0 || obj.logi == 0 || obj.owner == Side::Neutral {
                continue;
            }
            let should_produce = last_production_ts
                .map(|ts| now - ts >= interval)
                .unwrap_or(true);
            if !should_produce {
                continue;
            }
            // A damaged factory runs at reduced output rather than either
            // full rate or nothing.
            let qty = ((*production_rate as f64) * (obj.logi as f64 / 100.)).round() as u32;
            *last_production_ts = Some(now);
            if qty == 0 {
                continue;
            }
            let inv = obj
                .warehouse
                .equipment
                .get_or_default_cow(dcso3::String::from(MATERIEL_ITEM));
            if inv.capacity == 0 {
                inv.capacity = qty.saturating_mul(4);
            }
            *inv += qty;
            produced.push((*oid, obj.owner, qty));
        }
        if produced.is_empty() {
            return Ok(());
        }
        self.ephemeral.dirty();
        if !materiel_enabled {
            // Without the materiel commodity there is nowhere for factory
            // output to usefully go, so leave it in the factory (the old
            // behaviour) rather than shipping a commodity nothing spends.
            return Ok(());
        }
        for (fid, side, qty) in produced {
            let Some(hub) = self.nearest_hub_for(fid, side) else {
                continue;
            };
            // Only the materiel it just made moves -- a factory is also a
            // normal objective with a full warehouse, and shipping all of it
            // would strip the factory bare every cycle.
            let headroom = self
                .persisted
                .objectives
                .get(&hub)
                .and_then(|h| h.warehouse.equipment.get(MATERIEL_ITEM))
                .map(|inv| inv.capacity.saturating_sub(inv.stored))
                .unwrap_or(0);
            let amount = min(qty, headroom);
            if amount == 0 {
                continue;
            }
            let tr = Transfer {
                source: fid,
                target: hub,
                amount,
                item: TransferItem::Equipment(dcso3::String::from(MATERIEL_ITEM)),
            };
            let (fname, hname) = (
                self.persisted.objectives.get(&fid).map(|o| o.name.clone()).unwrap_or_default(),
                self.persisted.objectives.get(&hub).map(|o| o.name.clone()).unwrap_or_default(),
            );
            if let Err(e) = tr.execute(&mut self.persisted, &self.ephemeral.to_bg) {
                error!("[MATERIEL] moving factory output {fname} -> {hname}: {e:?}");
            } else {
                info!(
                    "[MATERIEL] factory {fname} produced {qty}, shipped {amount} to {hname} \
                     (hub headroom was {headroom})"
                );
            }
        }
        self.ephemeral.dirty();
        Ok(())
    }

    /// Nearest same-side logistics hub to `oid`, used to route factory output
    /// into the supply network.
    fn nearest_hub_for(&self, oid: ObjectiveId, side: Side) -> Option<ObjectiveId> {
        let pos = self.persisted.objectives.get(&oid)?.zone.pos();
        let mut best: Option<(ObjectiveId, f64)> = None;
        for hid in &self.persisted.logistics_hubs {
            let Some(hub) = self.persisted.objectives.get(hid) else {
                continue;
            };
            if hub.owner != side {
                continue;
            }
            let d = na::distance_squared(&pos.into(), &hub.zone.pos().into());
            if best.map_or(true, |(_, bd)| d < bd) {
                best = Some((*hid, d));
            }
        }
        best.map(|(id, _)| id)
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

    /// Recompute every objective's readiness numbers.
    ///
    /// Each item is weighted by how much of it the side produces, which is a
    /// decent proxy for how fast it gets burned through. The old unweighted
    /// mean gave a rare store the same say as a chaff cartridge, so a base
    /// that was out of the only missile anyone actually flies with still read
    /// as comfortably supplied -- and that number is what drives convoy
    /// dispatch and the commander's target selection. Airframes are scored
    /// separately from munitions for the same reason: blended together, an
    /// empty ramp disappeared into a full magazine.
    pub(super) fn update_supply_status(&mut self) -> Result<()> {
        // Materiel has no production-map entry of its own, and it is the
        // commodity repairs and deployments are actually paid for in, so give
        // it a standing weight rather than letting it drop out of the average.
        const MATERIEL_WEIGHT: f64 = 1000.;
        for (_, obj) in self.persisted.objectives.iter_mut_cow() {
            let current_supply = obj.supply;
            let current_fuel = obj.fuel;
            let production = self.ephemeral.production_by_side.get(&obj.owner);
            let mut mun_sum = 0.;
            let mut mun_weight = 0.;
            let mut air_sum = 0.;
            let mut air_weight = 0.;
            for (name, inv) in &obj.warehouse.equipment {
                let Some(pct) = inv.percent() else { continue };
                let weight = if name.as_str() == MATERIEL_ITEM {
                    MATERIEL_WEIGHT
                } else {
                    production
                        .and_then(|p| p.equipment.get(name))
                        .map(|e| e.production as f64)
                        .unwrap_or(0.)
                };
                if weight <= 0. {
                    continue;
                }
                if is_airframe_item(name.as_str()) {
                    air_sum += weight * pct as f64;
                    air_weight += weight;
                } else {
                    mun_sum += weight * pct as f64;
                    mun_weight += weight;
                }
            }
            let mut liq_sum = 0.;
            let mut liq_weight = 0.;
            for (name, inv) in &obj.warehouse.liquids {
                let Some(pct) = inv.percent() else { continue };
                let weight = production
                    .and_then(|p| p.liquids.get(name))
                    .map(|q| *q as f64)
                    .unwrap_or(1.);
                liq_sum += weight * pct as f64;
                liq_weight += weight;
            }
            let mean = |sum: f64, weight: f64| -> u8 {
                if weight <= 0. {
                    0
                } else {
                    (sum / weight).round().clamp(0., 100.) as u8
                }
            };
            obj.supply = mean(mun_sum, mun_weight);
            obj.fuel = mean(liq_sum, liq_weight);
            // A base with no airframe entries at all (a FOB, a logistics hub)
            // isn't short of aircraft -- it just doesn't operate any.
            obj.aircraft = if air_weight <= 0. {
                100
            } else {
                mean(air_sum, air_weight)
            };
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
        match warehouse::Warehouse::get_resource_map(lua) {
            Ok(map) => sync_obj_to_warehouse_zeroing_foreign_airframes(obj, &warehouse, &map)
                .context("syncing objective to warehouse")?,
            Err(e) => {
                // Fall back to the plain sync rather than skipping it entirely.
                warn!("no resource map, foreign airframes not zeroed: {e:?}");
                sync_obj_to_warehouse(obj, &warehouse).context("syncing objective to warehouse")?
            }
        }
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
            // Skip airframes - they should never be transferred via supply crates.
            // Airframes don't have prefixes like "weapons." or "vehicles." --
            // they're just aircraft type names, which is also true of the
            // synthetic materiel item, so use the shared predicate rather than
            // repeating the prefix test and accidentally excluding materiel
            // from the one mechanic players have for moving it by hand.
            let is_airframe = is_airframe_item(name.as_str());

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

    /// Dump the whole logistics picture to the log on demand, so a problem
    /// can be diagnosed without waiting up to a full tick for the periodic
    /// lines to come round -- and without needing every one of them turned up
    /// to debug. Reachable from chat as `-logistics`.
    pub fn admin_log_logistics(&self) -> Result<()> {
        use std::fmt::Write;
        let whcfg = match self.ephemeral.cfg.warehouse.as_ref() {
            Some(w) => w,
            None => {
                warn!("[LOGI_REPORT] no warehouse config -- the logistics system is off entirely");
                return Ok(());
            }
        };
        let mut msg = CompactString::new("logistics report\n");
        let _ = write!(
            msg,
            "tick {} min, delivery every {} tick(s), hub reserve {}%, front-line routing {}\n",
            whcfg.tick,
            whcfg.ticks_per_delivery,
            whcfg.hub_reserve_percent,
            whcfg.front_line_routing
        );
        let materiel = whcfg.materiel.as_ref().filter(|m| m.enabled);
        match materiel {
            Some(m) => {
                let _ = write!(
                    msg,
                    "materiel: {} per delivery, repair {} / deploy {} per use\n",
                    m.hub_production, m.repair_cost, m.deploy_cost
                );
            }
            None => {
                let _ = write!(msg, "materiel: disabled (repair/deploy use the legacy % draw)\n");
            }
        }
        for side in Side::ALL {
            if side == Side::Neutral {
                continue;
            }
            let baseline = self.persisted.production_baseline.get(&side).copied();
            let _ = write!(
                msg,
                "-- {side:?}: convoys {}, air routes {}, sea routes {}, production baseline {}\n",
                self.ephemeral.active_convoys.values().filter(|c| c.side == side).count(),
                self.ephemeral.active_air_routes.values().filter(|r| r.side == side).count(),
                self.ephemeral.active_sea_routes.values().filter(|r| r.side == side).count(),
                baseline.map(|b| format_compact!("{b:.1}")).unwrap_or_else(|| "unset".into()),
            );
        }
        for (oid, obj) in &self.persisted.objectives {
            if !self.persisted.logistics_hubs.contains(oid)
                && !matches!(obj.kind, ObjectiveKind::NavalBase | ObjectiveKind::Factory { .. })
            {
                continue;
            }
            let mat = obj
                .warehouse
                .equipment
                .get(MATERIEL_ITEM)
                .map(|i| format_compact!("{}/{}", i.stored, i.capacity))
                .unwrap_or_else(|| "none".into());
            let _ = write!(
                msg,
                "hub {} ({:?}, {:?}): logi {}%, materiel {mat}, feeds {} destination(s)\n",
                obj.name,
                obj.owner,
                obj.kind,
                obj.logi,
                obj.warehouse.destination.into_iter().count()
            );
        }
        if self.persisted.pending_cargo.len() == 0 {
            let _ = write!(msg, "no cargo in flight\n");
        } else {
            for (id, p) in &self.persisted.pending_cargo {
                let units: u32 = p.transfers.iter().map(|t| t.amount).sum();
                let _ = write!(
                    msg,
                    "in flight {id}: {} -> {}, {units} unit(s), {} min out\n",
                    self.persisted.objectives.get(&p.origin).map(|o| o.name.as_str()).unwrap_or("?"),
                    self.persisted.objectives.get(&p.destination).map(|o| o.name.as_str()).unwrap_or("?"),
                    (Utc::now() - p.departed).num_minutes()
                );
            }
        }
        // Bases that are actually in trouble, which is what anyone running
        // this command is looking for.
        for (_, obj) in &self.persisted.objectives {
            if obj.owner == Side::Neutral || obj.kind.is_special_sam_site() {
                continue;
            }
            let worst = min(obj.supply, min(obj.fuel, obj.aircraft));
            if worst >= 50 {
                continue;
            }
            let _ = write!(
                msg,
                "LOW {}: munitions {}%, fuel {}%, aircraft {}%, supplier {}{}\n",
                obj.name,
                obj.supply,
                obj.fuel,
                obj.aircraft,
                obj.warehouse
                    .supplier
                    .and_then(|id| self.persisted.objectives.get(&id))
                    .map(|o| o.name.as_str())
                    .unwrap_or("NONE"),
                if obj.logistics_detached { " [DETACHED]" } else { "" }
            );
        }
        warn!("[LOGI_REPORT] {msg}");
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
