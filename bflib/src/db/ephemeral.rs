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
    cargo::{Cargo, C130Cargo, GroundVehiclePassengers},

    group::{DeployKind, SpawnedGroup, SpawnedUnit},
    intel::IntelDatabase,
    logistics::LogiStage,
    recon::ReconSession,
    map_layer::MapLayer,
    markup::ObjectiveMarkup,
    objective::Objective,
    persisted::Persisted,
};
use bfprotocols::db::objective::ObjectiveKind;
use crate::{
    bg::Task,
    maybe,
    msgq::MsgQ,
    spawnctx::{Despawn, SpawnCtx, Spawned},
};
use anyhow::{Context, Result, anyhow, bail};
use bfprotocols::{
    cfg::{
        ActionKind, AiPlaneCfg, AwacsCfg, BomberCfg, Cfg, Crate, Deployable, DeployableCfg,
        DeployableKind, DeployableObjective, DroneCfg, Troop, UnitTag, Vehicle, VictoryCondition,
        WarehouseConfig,
    },
    db::{
        group::{GroupId, UnitId},
        objective::ObjectiveId,
    },
    perf::PerfInner,
    stats::Stat,
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{
    LuaVec2, MizLua, Position3, String, Vector2,
    airbase::{Airbase, ClassAirbase},
    centroid2d,
    coalition::Side,
    controller::{MissionPoint, PointType},
    country::Country,
    env::miz::{self, GroupKind, Miz, MizIndex},
    group::GroupCategory,
    net::{SlotId, Ucid},
    object::{ClassObject, DcsObject, DcsOid},
    perf::record_perf,
    static_object::ClassStatic,
    trigger::MarkId,
    unit::{ClassUnit, Unit},
    warehouse::LiquidType,
};
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use indexmap::{IndexMap, IndexSet};
use log::{debug, error, info, warn};
use mlua::prelude::*;
use smallvec::{SmallVec, smallvec};
use std::{
    cmp::max,
    collections::{BTreeMap, VecDeque, hash_map::Entry},
    mem,
    sync::Arc,
};
use tokio::sync::mpsc::UnboundedSender;

#[derive(Debug, Clone)]
pub struct SlotInfo {
    pub unit_name: String,
    pub typ: Vehicle,
    pub objective: ObjectiveId,
    pub ground_start: bool,
    pub miz_gid: miz::GroupId,
    pub side: Side,
}


/// Metadata for a group that was created from inline config rather than a .miz template.
/// Stored in Ephemeral so that spawn_group can build a synthetic Lua group table for DCS.
#[derive(Debug, Clone)]
pub(super) struct SyntheticGroupSpec {
    pub country: Country,
    pub category: GroupCategory,
}

#[derive(Debug, Clone, Default)]
pub(super) struct DeployableIndex {
    pub(super) deployables_by_name: FxHashMap<String, Deployable>,
    pub(super) deployables_by_crates: FxHashMap<String, String>,
    pub(super) deployables_by_repair: FxHashMap<String, String>,
    pub(super) crates_by_name: FxHashMap<String, Crate>,
    pub(super) squads_by_name: FxHashMap<String, Troop>,
    pub(super) pad_templates: FxHashMap<String, FxHashSet<String>>,
}

/// A neutral-coalition static object placed inside an objective zone that should
/// never appear destroyed. Tracked by its live DCS object id; when that object
/// dies, the static is respawned from its original template so it looks immortal.
#[derive(Debug, Clone)]
pub(super) struct ProtectedStatic {
    pub(super) template_name: String,
    pub(super) side: Side,
}

/// A real map-terrain building (DCS "Scenery" category) registered by
/// `scan_objective_scenery` as logistics-relevant for an objective. Tracked so
/// its destruction can be periodically detected (Scenery objects don't reliably
/// fire death events the way units/statics do) and folded into that
/// objective's `logi` rating. This state is intentionally NOT persisted --
/// it's rebuilt by a fresh scan on every mission load, matching the fact that
/// DCS's own terrain destruction doesn't survive a server restart either.
#[derive(Debug, Clone)]
pub(super) struct TrackedScenery {
    pub(super) objective: ObjectiveId,
    pub(super) label: String,
    /// Ground position of the building, kept so its map marker can be recreated
    /// for a new side when the objective is captured or re-spawned.
    pub(super) pos: Vector2,
    /// F10 map marker for this building. Present only while the parent objective
    /// is spawned (culled the same way units are) and the building is standing;
    /// `None` otherwise.
    pub(super) marker: Option<dcso3::trigger::MarkId>,
    /// Which side the current `marker` was drawn for, so a capture while the
    /// objective is still spawned can re-scope it.
    pub(super) marker_side: Option<Side>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct Equipment {
    pub(super) production: u32,
}

#[derive(Debug, Clone, Default)]
pub(super) struct Production {
    pub(super) equipment: FxHashMap<String, Equipment>,
    pub(super) liquids: FxHashMap<LiquidType, u32>,
}

#[derive(Debug)]
pub struct Ephemeral {
    pub(super) dirty: bool,
    pub cfg: Arc<Cfg>,
    pub(super) to_bg: Option<UnboundedSender<Task>>,
    pub(super) players_by_slot: IndexMap<SlotId, Ucid, FxBuildHasher>,
    /// Open sortie state, one entry per pilot per slot session. An entry means
    /// a `Stat::Takeoff` has gone out and the sortie is still open in bfdb; the
    /// value is when the pilot last touched down, or `None` while airborne.
    ///
    /// A sortie spans the whole slot session, not a single takeoff-to-landing
    /// leg: a pilot who lands to rearm and launches again is still flying the
    /// same sortie. Only the first takeoff of a session opens one (see
    /// `Db::takeoff`) and only the deslot that follows a landing closes it (see
    /// `Db::player_deslot`), so land-rearm-launch counts once instead of twice.
    pub(super) open_sorties: FxHashMap<Ucid, Option<DateTime<Utc>>>,
    pub(super) cargo: FxHashMap<SlotId, Cargo>,
    /// C-130 physical cargo tracking: crate_name -> C130Cargo (tracked by name because DCS changes object ID when loading/dropping)
    pub(super) c130_crates: FxHashMap<String, C130Cargo>,
    /// Queue for staggered crate spawning: (spawn_time, crate_data with index
    /// for positioning). anchor_point/anchor_dir/anchor_offset/carrier_link
    /// are resolved once when the whole batch is queued (not re-resolved per
    /// crate per tick) so that a moving carrier deck doesn't drag the grid
    /// reference out from under already-spawned crates in the same batch,
    /// which otherwise makes every later crate collapse back onto the first
    /// grid cell instead of spreading out.
    pub(super) c130_spawn_queue: BTreeMap<
        DateTime<Utc>,
        Vec<(Side, String, ObjectiveId, Ucid, String, Crate, usize, bool, Vector2, Vector2, f64, Option<String>)>,
    >,
    /// Supply convoy tracking: convoy_id -> SupplyConvoy
    pub(super) active_convoys: FxHashMap<super::logistics::ConvoyId, super::logistics::SupplyConvoy>,
    /// Track last convoy spawn time per side to throttle spawning
    pub(super) last_convoy_spawn: FxHashMap<Side, DateTime<Utc>>,
    /// When a supply run was last dispatched to each objective, whatever the
    /// transport mode. Enforces `ConvoyConfig::dispatch_cooldown_ticks` so a
    /// destination isn't served by a fresh pair of convoys on every single
    /// logistics tick.
    pub(super) last_dispatch_to: FxHashMap<ObjectiveId, DateTime<Utc>>,
    /// Counter for generating unique convoy IDs
    pub(super) convoy_counter: u32,
    /// Air logistics route tracking: route_id -> AirLogisticsRoute
    pub(super) active_air_routes: FxHashMap<super::logistics::LogiRouteId, super::logistics::AirLogisticsRoute>,
    /// Track last air route spawn time per side to throttle spawning
    pub(super) last_air_route_spawn: FxHashMap<Side, DateTime<Utc>>,
    pub(crate) pending_achievements: Vec<String>,
    /// Counter for generating unique air route IDs
    pub(super) air_route_counter: u32,
    /// Sea logistics route tracking: route_id -> SeaLogisticsRoute
    pub(super) active_sea_routes: FxHashMap<super::logistics::LogiRouteId, super::logistics::SeaLogisticsRoute>,
    /// Track last sea route spawn time per side to throttle spawning
    pub(super) last_sea_route_spawn: FxHashMap<Side, DateTime<Utc>>,
    /// Counter for generating unique sea route IDs
    pub(super) sea_route_counter: u32,
    /// AI helo mission tracking (F10-callable troop insertion / resource
    /// delivery): mission_id -> HeloMission
    pub(super) active_helo_missions: FxHashMap<super::logistics::HeloMissionId, super::logistics::HeloMission>,
    /// Counter for generating unique helo mission IDs
    pub(super) helo_mission_counter: u32,
    pub(super) deployable_idx: FxHashMap<Side, Arc<DeployableIndex>>,
    pub(super) group_marks: FxHashMap<GroupId, MarkId>,
    objective_markup: FxHashMap<ObjectiveId, ObjectiveMarkup>,
    pub(super) object_id_by_uid: FxHashMap<UnitId, DcsOid<ClassUnit>>,
    pub(super) uid_by_object_id: FxHashMap<DcsOid<ClassUnit>, UnitId>,
    pub(super) object_id_by_slot: FxHashMap<SlotId, DcsOid<ClassUnit>>,
    pub(super) slot_by_object_id: FxHashMap<DcsOid<ClassUnit>, SlotId>,
    pub(super) object_id_by_gid: FxHashMap<GroupId, DcsOid<ClassObject>>,
    pub(super) gid_by_object_id: FxHashMap<DcsOid<ClassObject>, GroupId>,
    pub(super) uid_by_static: FxHashMap<DcsOid<ClassStatic>, UnitId>,
    /// Neutral decoration statics inside objective zones that get respawned in place
    /// whenever they're destroyed, so they behave as if immortal.
    pub(super) protected_statics: FxHashMap<DcsOid<ClassStatic>, ProtectedStatic>,
    /// Real map-terrain buildings registered as logistics-relevant, still standing.
    pub(super) tracked_scenery: FxHashMap<DcsOid<ClassObject>, TrackedScenery>,
    /// Round-robin queue for batched `is_exist()` polling of `tracked_scenery`
    /// (checking all of them every tick would be far too expensive).
    pub(super) scenery_check_queue: VecDeque<DcsOid<ClassObject>>,
    /// How many of an objective's originally-registered scenery buildings have
    /// been confirmed destroyed. Combined with `scenery_total_by_objective` to
    /// scale that objective's `logi` rating down as its infrastructure is lost.
    pub(super) scenery_destroyed_by_objective: FxHashMap<ObjectiveId, u32>,
    /// How many logistics-relevant scenery buildings were registered for an
    /// objective at scan time (the denominator for the ratio above).
    pub(super) scenery_total_by_objective: FxHashMap<ObjectiveId, u32>,
    pub(super) slot_by_miz_gid: FxHashMap<miz::GroupId, SlotId>,
    pub(super) airbase_by_oid: FxHashMap<ObjectiveId, DcsOid<ClassAirbase>>,
    pub(super) slot_info: FxHashMap<SlotId, SlotInfo>,
    used_pad_templates: FxHashSet<String>,
    pub(super) global_pad_templates: FxHashSet<String>,
    force_to_spectators: BTreeMap<DateTime<Utc>, SmallVec<[Ucid; 1]>>,
    pub(super) units_able_to_move: IndexSet<UnitId, FxBuildHasher>,
    pub(super) groups_with_move_missions: FxHashMap<GroupId, Vector2>,
    pub(super) units_potentially_close_to_enemies: FxHashSet<UnitId>,
    /// Recent weapon-launch events: (shooter_pos, attacking_side, timestamp).
    /// Used to keep enemy objectives awake while weapons are inbound.
    pub(crate) recent_shots: Vec<(Vector2, Side, DateTime<Utc>)>,
    /// Objectives that have been targeted by artillery/missile strikes, keyed by
    /// ObjectiveId. Entries expire after ARTY_WAKE_SECS seconds and cause the
    /// objective's units to be (re)spawned so they are present to absorb fire.
    pub(crate) artillery_targeted: FxHashMap<ObjectiveId, DateTime<Utc>>,
    pub(super) production_by_side: FxHashMap<Side, Arc<Production>>,
    pub(super) actions_taken: FxHashMap<Side, FxHashMap<String, u32>>,
    pub(super) delayspawnq: BTreeMap<DateTime<Utc>, SmallVec<[GroupId; 8]>>,
    pub(super) awacs_stn: u32,
    pub(super) logistics_stage: LogiStage,
    spawnq: VecDeque<GroupId>,
    despawnq: VecDeque<(GroupId, Despawn)>,
    /// Groups that should be linked to a carrier unit when spawned.
    /// Maps GroupId to DCS unit ID of the carrier to link to.
    pub(super) carrier_linked_groups: FxHashMap<GroupId, String>,
    sync_warehouse: Vec<(ObjectiveId, Vehicle)>,
    pub(super) msgs: MsgQ,
    pub(super) victory: Option<(DateTime<Utc>, Side)>,
    /// Downed pilots that have already fired their approach flare (reset on restart)
    pub(super) csar_flared: FxHashSet<GroupId>,
    /// Downed pilots currently moving toward a helicopter: gid -> last move order time
    pub(super) csar_moving: FxHashMap<GroupId, DateTime<Utc>>,
    /// Objectives for which the "enemies spotted" threat alert has already been sent this session.
    /// Prevents the alert from repeating each time a threat appears/clears cycle repeats.
    pub(crate) threat_notified: FxHashSet<ObjectiveId>,
    /// Downed pilots for which the all-helicopter-pilots notification has already been sent
    pub(super) csar_notified: FxHashSet<GroupId>,
    /// Per-pilot last renotify broadcast time (bearing/distance reminder to helo pilots)
    pub(super) csar_last_renotify: FxHashMap<GroupId, DateTime<Utc>>,
    /// Per-pilot last smoke request time (for cooldown enforcement)
    pub(crate) csar_smoke_cooldown: FxHashMap<GroupId, DateTime<Utc>>,
    /// Set of objective IDs for which a "supply critical" alert has been broadcast,
    /// mapped to the time the alert first fired. Cleared when supply recovers.
    pub(super) supply_warned: FxHashMap<ObjectiveId, DateTime<Utc>>,
    /// Tracks when enemy troops first entered an objective zone (for capture momentum timer).
    /// Maps ObjectiveId -> (capturing Side, entry DateTime, last_seen DateTime).
    /// last_seen is updated each tick troops are in zone; cleared only after a grace period.
    pub(super) capture_progress: FxHashMap<ObjectiveId, (dcso3::coalition::Side, DateTime<Utc>, DateTime<Utc>)>,
    /// Last time an objective changed hands (captured or fell to Neutral). No
    /// fresh capture timer may start against it until `capture_cooldown_secs`
    /// have passed -- stops a base with capture-capable troops from both sides
    /// parked in the zone from flipping every capture-timer's length forever.
    pub(super) last_owner_change: FxHashMap<ObjectiveId, DateTime<Utc>>,
    /// Throttle for the "captureable but no capturing troops detected" diagnostic
    /// log in check_capture (one line per objective per 15s).
    pub(super) last_capture_debug: FxHashMap<ObjectiveId, DateTime<Utc>>,
    /// Last time treasury income was deposited (Smart Commander).
    pub(crate) last_treasury_income: DateTime<Utc>,
    /// Last time objectives were funded (Smart Commander).
    pub(crate) last_objective_fund: DateTime<Utc>,
    /// Centralised F10 map drawing layer.
    pub(super) map_layer: MapLayer,
    /// Tasks posted in the last couple of minutes, waiting to be picked up
    /// by the next `query-gci` poll so the GCI voice can read them out.
    /// Pruned by `Db::tick_tasks`; this is a broadcast queue, not state --
    /// the board itself lives in `Persisted::tasks`.
    pub(crate) gci_tasks: Vec<(DateTime<Utc>, crate::db::tasks::Task)>,
    /// Registry of groups whose unit definitions came from inline config rather than a .miz template.
    /// Keyed by the group's template_name (which encodes the group's name prefix).
    /// Used by spawn_group to build synthetic Lua tables for DCS when there is no .miz template.
    pub(super) synthetic_templates: FxHashMap<String, SyntheticGroupSpec>,
    /// Mercy timer state: (arm_time, losing_side). Set when a side drops to trigger_count primary objectives.
    pub(crate) last_stand_state: Option<(DateTime<Utc>, Side)>,
    /// Last time an under-attack notification was sent per objective (for cooldown).
    pub(crate) last_under_attack_notif: FxHashMap<ObjectiveId, DateTime<Utc>>,
    /// How many repair crates are stacked on each carrier's in-progress
    /// repair. Divides the repair timer (see check_carrier_repairs). Not
    /// persisted -- a repair spanning a restart just reverts to base speed.
    pub(crate) carrier_repair_crates: FxHashMap<ObjectiveId, u32>,
    /// ELINT/SIGINT persistent intel database (populated when cfg.elint is Some).
    pub(crate) intel_db: IntelDatabase,
    /// Ground vehicle passenger manifests: vehicle UnitId -> passengers.
    pub(crate) ground_vehicle_passengers: FxHashMap<bfprotocols::db::group::UnitId, GroundVehiclePassengers>,
    /// In-progress player "Recon Pass" sessions, keyed by pilot ucid.
    pub(crate) recon_sessions: FxHashMap<Ucid, ReconSession>,
    /// Per-pilot cooldown after a recon pass ends (completed or aborted).
    pub(crate) recon_cooldown: FxHashMap<Ucid, DateTime<Utc>>,
    /// F10 marks drawn from the dashboard's recon markup (bfdb `intel-marks`
    /// RPC), keyed by the source markup id. Not persisted -- redrawn on the
    /// next push after a restart.
    pub(crate) intel_map_marks: FxHashMap<std::string::String, SmallVec<[dcso3::trigger::MarkId; 4]>>,
}

impl Default for Ephemeral {
    fn default() -> Self {
        Self {
            dirty: false,
            cfg: Arc::new(Cfg::default()),
            to_bg: None,
            players_by_slot: IndexMap::default(),
            open_sorties: FxHashMap::default(),
            cargo: FxHashMap::default(),
            c130_crates: FxHashMap::default(),
            c130_spawn_queue: BTreeMap::default(),
            active_convoys: FxHashMap::default(),
            last_convoy_spawn: FxHashMap::default(),
            last_dispatch_to: FxHashMap::default(),
            convoy_counter: 0,
            active_air_routes: FxHashMap::default(),
            last_air_route_spawn: FxHashMap::default(),
            pending_achievements: Vec::new(),
            air_route_counter: 0,
            active_sea_routes: FxHashMap::default(),
            last_sea_route_spawn: FxHashMap::default(),
            sea_route_counter: 0,
            active_helo_missions: FxHashMap::default(),
            helo_mission_counter: 0,
            deployable_idx: FxHashMap::default(),
            group_marks: FxHashMap::default(),
            objective_markup: FxHashMap::default(),
            object_id_by_uid: FxHashMap::default(),
            uid_by_object_id: FxHashMap::default(),
            object_id_by_slot: FxHashMap::default(),
            slot_by_object_id: FxHashMap::default(),
            slot_by_miz_gid: FxHashMap::default(),
            object_id_by_gid: FxHashMap::default(),
            gid_by_object_id: FxHashMap::default(),
            uid_by_static: FxHashMap::default(),
            protected_statics: FxHashMap::default(),
            tracked_scenery: FxHashMap::default(),
            scenery_check_queue: VecDeque::default(),
            scenery_destroyed_by_objective: FxHashMap::default(),
            scenery_total_by_objective: FxHashMap::default(),
            airbase_by_oid: FxHashMap::default(),
            slot_info: FxHashMap::default(),
            used_pad_templates: FxHashSet::default(),
            global_pad_templates: FxHashSet::default(),
            force_to_spectators: BTreeMap::default(),
            units_able_to_move: IndexSet::default(),
            groups_with_move_missions: FxHashMap::default(),
            units_potentially_close_to_enemies: FxHashSet::default(),
            recent_shots: Vec::new(),
            artillery_targeted: FxHashMap::default(),
            production_by_side: FxHashMap::default(),
            actions_taken: FxHashMap::default(),
            delayspawnq: BTreeMap::default(),
            awacs_stn: 0o77777,
            spawnq: VecDeque::default(),
            despawnq: VecDeque::default(),
            carrier_linked_groups: FxHashMap::default(),
            sync_warehouse: Vec::default(),
            msgs: MsgQ::default(),
            logistics_stage: LogiStage::default(),
            victory: None,
            csar_flared: FxHashSet::default(),
            csar_moving: FxHashMap::default(),
            threat_notified: FxHashSet::default(),
            csar_notified: FxHashSet::default(),
            csar_last_renotify: FxHashMap::default(),
            csar_smoke_cooldown: FxHashMap::default(),
            supply_warned: FxHashMap::default(),
            capture_progress: FxHashMap::default(),
            last_owner_change: FxHashMap::default(),
            last_capture_debug: FxHashMap::default(),
            last_treasury_income: DateTime::<Utc>::default(),
            last_objective_fund: DateTime::<Utc>::default(),
            map_layer: MapLayer::default(),
            gci_tasks: Vec::new(),
            synthetic_templates: FxHashMap::default(),
            last_stand_state: None,
            last_under_attack_notif: FxHashMap::default(),
            carrier_repair_crates: FxHashMap::default(),
            intel_db: IntelDatabase::default(),
            ground_vehicle_passengers: FxHashMap::default(),
            recon_sessions: FxHashMap::default(),
            recon_cooldown: FxHashMap::default(),
            intel_map_marks: FxHashMap::default(),
        }
    }
}

/// F10 objective labels are re-issued whenever any displayed field changes, so
/// a raw per-second countdown would rewrite every label on every tick -- a real
/// contributor to F10 draw lag once several bases are repairing at once.
/// Quantising the countdown means a label redraws a handful of times a minute
/// instead of sixty, at no cost to how useful the number is to a player.
const ETA_BUCKET_SECS: i64 = 15;

fn bucket_eta(secs: i64) -> i64 {
    if secs <= 0 {
        return 0;
    }
    ((secs + ETA_BUCKET_SECS - 1) / ETA_BUCKET_SECS) * ETA_BUCKET_SECS
}

fn bucket_pct(pct: u8) -> u8 {
    (pct / 5) * 5
}

impl Ephemeral {
    fn do_bg(&self, task: Task) {
        if let Some(to_bg) = &self.to_bg {
            match to_bg.send(task) {
                Ok(()) => (),
                Err(e) => log::error!("background thread is dead, task dropped: {e}"),
            }
        }
    }

    pub fn stat(&self, stat: Stat) {
        self.do_bg(Task::Stat(stat))
    }

    pub fn get_slot_info(&self, slot: &SlotId) -> Option<&SlotInfo> {
        self.slot_info.get(slot)
    }

    /// The live DCS airbase an objective launches ground-starting flights
    /// from, or `None` if it can't provide one.
    ///
    /// Two steps, and both are needed: the load-time `airbase_by_oid`
    /// registration, then a name match against a live DCS airbase, because
    /// bflib names Airbase-kind objectives after the field and a zone overlap
    /// can leave `airbase_by_oid` keyed to a neighbouring SAM / command-center
    /// objective instead.
    ///
    /// Every caller that needs a parking start goes through here -- the spawn
    /// path that builds the start and the dispatch paths that pick a launch
    /// field -- so a field can't pass the dispatch check and then fail at
    /// spawn time, which just air-starts the flight.
    pub fn resolve_airbase<'lua>(
        &self,
        lua: MizLua<'lua>,
        persisted: &Persisted,
        oid: &ObjectiveId,
    ) -> Option<Airbase<'lua>> {
        self.airbase_by_oid
            .get(oid)
            .and_then(|abid| Airbase::get_instance(lua, abid).ok())
            .filter(|ab| ab.is_exist().unwrap_or(false))
            .or_else(|| {
                persisted.objectives.get(oid).and_then(|o| {
                    Airbase::get_by_name(lua, dcso3::String::from(o.name.as_str()))
                        .ok()
                        .filter(|ab| ab.is_exist().unwrap_or(false))
                })
            })
    }

    pub fn get_airbase_by_oid(&self, oid: &ObjectiveId) -> Option<&DcsOid<ClassAirbase>> {
        self.airbase_by_oid.get(oid)
    }

    /// Logistics-relevant scenery buildings (warehouses, depots, fuel, storage)
    /// still standing at `oid`, sorted by label for stable ordering (used by
    /// JTAC to cycle through them as designatable/callable targets).
    pub fn scenery_at_objective(
        &self,
        oid: ObjectiveId,
    ) -> SmallVec<[(DcsOid<ClassObject>, CompactString); 8]> {
        let mut v: SmallVec<[(DcsOid<ClassObject>, CompactString); 8]> = self
            .tracked_scenery
            .iter()
            .filter(|(_, ts)| ts.objective == oid)
            .map(|(id, ts)| (id.clone(), CompactString::from(ts.label.as_str())))
            .collect();
        v.sort_by(|a, b| a.1.cmp(&b.1));
        v
    }

    pub fn get_slot_info_by_miz_gid(&self, gid: &miz::GroupId) -> Option<(SlotId, &SlotInfo)> {
        self.slot_by_miz_gid
            .get(gid)
            .and_then(|sl| self.slot_info.get(sl).map(|s| (*sl, s)))
    }

    /// Capture progress (0-100) for an objective currently being captured,
    /// or None if it isn't. capture_time_secs == 0 means instant capture --
    /// no meaningful bar to show there.
    fn capture_pct_for(&self, oid: &ObjectiveId) -> Option<u8> {
        let (_, start, _) = self.capture_progress.get(oid)?;
        // Missing campaign_events still means the default 60s momentum, not instant.
        let total = self
            .cfg
            .campaign_events
            .as_ref()
            .map_or(180, |c| c.capture_time_secs);
        if total == 0 {
            return None;
        }
        let elapsed = (Utc::now() - *start).num_seconds().max(0);
        Some(((elapsed as f64 / total as f64) * 100.0).clamp(0.0, 100.0) as u8)
    }

    /// Repair progress (0-100) and estimated seconds remaining for an
    /// objective currently mid-repair, or None if it isn't actively
    /// progressing toward a repair completion.
    ///
    /// Carriers have a single timed repair operation (repair_start_time +
    /// carrier.repair_time). Regular objectives auto-repair to 100% once
    /// `now - last_change_ts` reaches `repair_time / logi_fraction` (see
    /// `maybe_do_repairs`/`repair_objective`) -- last_change_ts only moves
    /// when something actually changes (combat, crate delivery, scenery
    /// loss), so during a quiet stretch this is just as much a clean
    /// countdown as the carrier's is.
    /// Consolidation progress for a base still in its post-capture hold, as
    /// (percent, seconds remaining), bucketed like `repair_pct_for`.
    fn hold_pct_for(&self, obj: &Objective) -> Option<(u8, i64)> {
        obj.capture_hold_pct(self.cfg.capture_consolidation_secs)
            .map(|(pct, secs)| (bucket_pct(pct), bucket_eta(secs)))
    }

    fn repair_pct_for(&self, obj: &Objective) -> Option<(u8, i64)> {
        if let ObjectiveKind::CarrierGroup { repair_start_time: Some(start), .. } = &obj.kind {
            let total = self.cfg.carrier.as_ref().map(|c| c.repair_time).unwrap_or(600) as f64;
            if total <= 0.0 {
                return None;
            }
            let elapsed = (Utc::now() - *start).num_seconds().max(0) as f64;
            let pct = ((elapsed / total) * 100.0).clamp(0.0, 100.0) as u8;
            return Some((
                bucket_pct(pct),
                bucket_eta((total - elapsed).max(0.0) as i64),
            ));
        }
        // Match maybe_do_repairs: a threatened / actively-contested objective
        // does not auto-repair, so don't show a phantom repair ETA. (A merely
        // capturable base does still repair -- burning supply -- so its ETA is
        // real and worth showing.)
        if obj.threatened || self.capture_progress.contains_key(&obj.id) {
            return None;
        }
        if obj.health < 100 && obj.logi > 0 {
            let logi_frac = obj.logi as f64 / 100.;
            let total = self.cfg.repair_time as f64 / logi_frac;
            if !total.is_finite() || total <= 0.0 {
                return None;
            }
            let elapsed = (Utc::now() - obj.last_change_ts).num_seconds().max(0) as f64;
            // A working repair resets last_change_ts each tick it fires, so a
            // real countdown never sits past `total`. If elapsed has run well
            // past it, the repair is blocked (contested, no revivable groups,
            // ...) -- show nothing rather than a frozen "100% (ETA 0s)".
            if elapsed >= total {
                return None;
            }
            let pct = ((elapsed / total) * 100.0).clamp(0.0, 100.0) as u8;
            return Some((
                bucket_pct(pct),
                bucket_eta((total - elapsed).max(0.0) as i64),
            ));
        }
        None
    }

    pub fn create_objective_markup(&mut self, persisted: &Persisted, obj: &Objective) {
        if obj.kind.is_special_sam_site() {
            if let Some(mk) = self.objective_markup.remove(&obj.id) {
                mk.remove(&mut self.msgs);
            }
            return;
        }
        if let Some(mk) = self.objective_markup.remove(&obj.id) {
            mk.remove(&mut self.msgs);
        }
        let capture_pct = self.capture_pct_for(&obj.id);
        let repair_pct = self.repair_pct_for(obj);
        let hold_pct = self.hold_pct_for(obj);
        self.objective_markup.insert(
            obj.id,
            ObjectiveMarkup::new(
                &self.cfg,
                &mut self.msgs,
                obj,
                persisted,
                capture_pct,
                repair_pct,
                hold_pct,
            ),
        );
    }

    pub fn update_objective_markup(
        &mut self,
        persisted: &Persisted,
        obj: &Objective,
        moved: &[ObjectiveId],
    ) {
        // Special SAM sites are position-classified -- they get no F10 label,
        // health bar or rings (see create_objective_markup). The per-tick
        // refresh loop calls straight in here, so without the same guard a
        // Vacant entry would rebuild the full "<name>\nHealth: .." markup every
        // few seconds -- which is the SAM-site label players keep seeing.
        if obj.kind.is_special_sam_site() {
            if let Some(mk) = self.objective_markup.remove(&obj.id) {
                mk.remove(&mut self.msgs);
            }
            return;
        }
        let capture_pct = self.capture_pct_for(&obj.id);
        let repair_pct = self.repair_pct_for(obj);
        let hold_pct = self.hold_pct_for(obj);
        match self.objective_markup.entry(obj.id) {
            Entry::Occupied(mut e) => e.get_mut().update(
                persisted,
                &mut self.msgs,
                obj,
                moved,
                capture_pct,
                repair_pct,
                hold_pct,
            ),
            Entry::Vacant(e) => {
                e.insert(ObjectiveMarkup::new(
                    &self.cfg,
                    &mut self.msgs,
                    obj,
                    persisted,
                    capture_pct,
                    repair_pct,
                    hold_pct,
                ));
            }
        }
    }

    pub fn remove_objective_markup(&mut self, oid: &ObjectiveId) {
        if let Some(mk) = self.objective_markup.remove(oid) {
            mk.remove(&mut self.msgs)
        }
    }

    /// Perform a full diff-based update of the F10 map layer.  Call this from
    /// the slow tick in lib.rs (every ~5 seconds).
    pub fn update_map_layer(&mut self, persisted: &Persisted, now: DateTime<Utc>) {
        let convoys = &self.active_convoys;
        let air = &self.active_air_routes;
        let sea = &self.active_sea_routes;
        let csar_capture_mins = self.cfg.csar.as_ref().map(|c| c.capture_timer).unwrap_or(0);
        self.map_layer.update_all(persisted, convoys, air, sea, csar_capture_mins, now, &mut self.msgs);
    }

    /// Draw a fire-mission overlay on the F10 map.  Replaces the inline
    /// `circle_to_all` call in `db/actions.rs`.
    pub fn on_fire_mission(
        &mut self,
        gun_pos: dcso3::Vector2,
        target_pos: dcso3::Vector2,
        radius_m: f64,
        gun_count: u32,
        side: dcso3::coalition::Side,
        now: DateTime<Utc>,
    ) {
        self.map_layer.on_fire_mission(
            gun_pos,
            target_pos,
            radius_m,
            gun_count,
            side,
            now,
            &mut self.msgs,
        );
    }

    /// Remove all F10 map layer marks (e.g. on mission reset).
    pub fn remove_map_layer(&mut self) {
        self.map_layer.remove_all(&mut self.msgs);
        self.recon_sessions.clear();
    }

    pub fn tick_intel_decay(&mut self, now: DateTime<Utc>) {
        // The intel contact pipeline is shared by the ELINT/SIGINT system and
        // the player Recon Pass -- run decay/marks if either is configured.
        let elint_cfg = match self.cfg.elint.as_ref() {
            Some(c) => c.clone(),
            None if self.cfg.player_recon.is_some() => bfprotocols::cfg::ElintConfig::default(),
            None => return,
        };
        let (updated, removed) = self.intel_db.tick_decay(&elint_cfg, now, 1.0);
        for (rect, label) in removed {
            self.map_layer.remove_intel_contact_marks(rect, label, &mut self.msgs);
        }
        let ids = updated;
        for id in ids {
            if let Some(contact) = self.intel_db.contacts.get_mut(&id) {
                self.map_layer.update_intel_contact_mark(contact, &elint_cfg, &mut self.msgs);
            }
        }
    }

    pub fn on_recon_result(
        &mut self,
        target_pos: dcso3::Vector2,
        scan_radius_m: f64,
        unit_count: usize,
        side: dcso3::coalition::Side,
        now: DateTime<Utc>,
    ) {
        self.map_layer.on_recon_result(target_pos, scan_radius_m, unit_count, side, now, &mut self.msgs);
    }

    pub fn on_objective_threatened(
        &mut self,
        obj_pos: dcso3::Vector2,
        side: dcso3::coalition::Side,
        obj_name: &str,
        now: DateTime<Utc>,
    ) {
        self.map_layer.on_objective_threatened(obj_pos, side, obj_name, now, &mut self.msgs);
    }


    pub fn on_objective_under_attack(
        &mut self,
        obj_pos: dcso3::Vector2,
        side: dcso3::coalition::Side,
        obj_name: &str,
        ttl_secs: i64,
        now: DateTime<Utc>,
    ) {
        self.map_layer.on_objective_under_attack(obj_pos, side, obj_name, ttl_secs, now, &mut self.msgs);
    }

    pub fn push_sync_warehouse(&mut self, oid: ObjectiveId, vehicle: Vehicle) {
        self.sync_warehouse.push((oid, vehicle));
    }

    pub fn warehouses_to_sync(&mut self) -> Vec<(ObjectiveId, Vehicle)> {
        mem::take(&mut self.sync_warehouse)
    }

    pub fn push_despawn(&mut self, gid: GroupId, ds: Despawn) {
        let mut queued_spawn = false;
        self.spawnq.retain(|sp_gid| {
            let qs = &gid == sp_gid;
            queued_spawn |= qs;
            !qs
        });
        let e = (gid, ds);
        if !queued_spawn && !self.despawnq.contains(&e) {
            self.despawnq.push_back(e)
        }
    }

    pub fn push_spawn(&mut self, gid: GroupId) {
        let mut queued_despawn = false;
        self.despawnq.retain(|(ds_gid, _)| {
            let qs = &gid == ds_gid;
            queued_despawn |= qs;
            !qs
        });
        if !queued_despawn && !self.spawnq.contains(&gid) {
            self.spawnq.push_back(gid)
        }
    }

    pub fn spawnq_len(&self) -> usize {
        self.spawnq.len()
    }

    pub fn process_spawn_queue(
        &mut self,
        perf: &mut PerfInner,
        persisted: &Persisted,
        now: DateTime<Utc>,
        idx: &MizIndex,
        spctx: &SpawnCtx,
    ) -> Result<()> {
        let mut delayed: SmallVec<[GroupId; 16]> = smallvec![];
        while let Some((at, gids)) = self.delayspawnq.first_key_value() {
            if now < *at {
                break;
            } else {
                for gid in gids {
                    delayed.push(*gid);
                }
                let at = *at;
                self.delayspawnq.remove(&at);
            }
        }
        for gid in delayed {
            self.push_spawn(gid)
        }
        let dlen = self.despawnq.len();
        let slen = self.spawnq.len();
        if dlen > 0 {
            for _ in 0..max(1, dlen >> 4) {
                if let Some((gid, despawn)) = self.despawnq.pop_front() {
                    // Always clean up gid tracking (delete_group may have already
                    // removed the group from persisted, but object_id_by_gid still
                    // needs cleanup)
                    if let Some(id) = self.object_id_by_gid.remove(&gid) {
                        self.gid_by_object_id.remove(&id);
                    }
                    if let Some(group) = persisted.groups.get(&gid) {
                        for uid in &group.units {
                            self.units_able_to_move.swap_remove(uid);
                            self.units_potentially_close_to_enemies.remove(uid);
                            if let Some(id) = self.object_id_by_uid.remove(uid) {
                                self.uid_by_object_id.remove(&id);
                            }
                        }
                    }
                    spctx.despawn(perf, despawn)?;
                }
            }
        } else if slen > 0 {
            for _ in 0..max(1, slen >> 4) {
                if let Some(gid) = self.spawnq.pop_front() {
                    let group = maybe!(persisted.groups, gid, "group")?;
                    self.spawn_group(perf, persisted, idx, spctx, group, vec![])?;
                }
            }
        }
        Ok(())
    }

    pub fn take_pad_template(&mut self, side: Side, name: &String) -> Option<String> {
        self.deployable_idx.get(&side).and_then(|idx| {
            if let Some(templates) = idx.pad_templates.get(name) {
                for pad in templates {
                    if self.used_pad_templates.insert(pad.clone()) {
                        return Some(pad.clone());
                    }
                }
            }
            None
        })
    }

    pub fn return_pad_template(&mut self, pad: &str) {
        self.used_pad_templates.remove(pad);
    }

    pub fn set_pad_template_used(&mut self, pad: String) {
        self.used_pad_templates.insert(pad);
    }

    pub fn msgs(&mut self) -> &mut MsgQ {
        &mut self.msgs
    }

    pub fn map_layer_and_msgs(&mut self) -> (&mut MapLayer, &mut MsgQ) {
        (&mut self.map_layer, &mut self.msgs)
    }

    #[allow(clippy::type_complexity)]
    pub fn intel_marks_and_msgs(
        &mut self,
    ) -> (
        &mut FxHashMap<std::string::String, SmallVec<[dcso3::trigger::MarkId; 4]>>,
        &mut MsgQ,
    ) {
        (&mut self.intel_map_marks, &mut self.msgs)
    }

    pub fn get_uid_by_object_id(&self, id: &DcsOid<ClassUnit>) -> Option<&UnitId> {
        self.uid_by_object_id.get(id)
    }

    pub fn get_object_id_by_uid(&self, id: &UnitId) -> Option<&DcsOid<ClassUnit>> {
        self.object_id_by_uid.get(id)
    }

    pub fn get_slot_by_object_id(&self, id: &DcsOid<ClassUnit>) -> Option<&SlotId> {
        self.slot_by_object_id.get(id)
    }

    pub fn get_object_id_by_slot(&self, id: &SlotId) -> Option<&DcsOid<ClassUnit>> {
        self.object_id_by_slot.get(id)
    }

    pub fn logistics_stage(&self) -> &LogiStage {
        &self.logistics_stage
    }

    fn index_deployables_for_side(
        &mut self,
        miz: &Miz,
        mizidx: &MizIndex,
        side: Side,
        repair_crate: Crate,
        whcfg: &Option<WarehouseConfig>,
        points: bool,
        deployables: &[Deployable],
    ) -> Result<()> {
        let idx = Arc::make_mut(self.deployable_idx.entry(side).or_default());
        idx.crates_by_name
            .insert(repair_crate.name.clone(), repair_crate);
        if let Some(whcfg) = whcfg.as_ref() {
            // Register fuel transfer crate
            if let Some(fuel_cr) = whcfg.supply_transfer_fuel_crate.get(&side) {
                match idx.crates_by_name.entry(fuel_cr.name.clone()) {
                    Entry::Occupied(_) => bail!("multiple {} crates for side {side}", fuel_cr.name),
                    Entry::Vacant(e) => {
                        e.insert(fuel_cr.clone());
                    }
                }
            }
            // Register weapons transfer crate
            if let Some(weapons_cr) = whcfg.supply_transfer_weapons_crate.get(&side) {
                match idx.crates_by_name.entry(weapons_cr.name.clone()) {
                    Entry::Occupied(_) => bail!("multiple {} crates for side {side}", weapons_cr.name),
                    Entry::Vacant(e) => {
                        e.insert(weapons_cr.clone());
                    }
                }
            }
        }
        for dep in deployables.iter() {
            if let DeployableKind::Group { template } = &dep.kind {
                miz.get_group_by_name(mizidx, GroupKind::Any, side, template)?
                    .ok_or_else(|| anyhow!("missing deployable template {:?} {:?}", side, dep))?;
            }
            if !points && dep.cost > 0 {
                bail!(
                    "the points system is disabled, but {:?} costs points",
                    dep.path
                )
            }
            let name = match dep.path.last() {
                None => bail!("deployable with empty path {:?}", dep),
                Some(name) => name,
            };
            match idx.deployables_by_name.entry(name.clone()) {
                Entry::Occupied(_) => bail!("deployable with duplicate name {name}"),
                Entry::Vacant(e) => e.insert(dep.clone()),
            };
            if let Some(rep) = dep.repair_crate.as_ref() {
                match idx.deployables_by_repair.entry(rep.name.clone()) {
                    Entry::Occupied(_) => {
                        bail!(
                            "multiple deployables use the same repair crate {}",
                            rep.name
                        )
                    }
                    Entry::Vacant(e) => {
                        if idx.deployables_by_crates.contains_key(&rep.name) {
                            bail!(
                                "deployable {} uses repair crate of {}",
                                &idx.deployables_by_crates[&rep.name],
                                name
                            )
                        }
                        e.insert(name.clone())
                    }
                };
            }
            for cr in dep.crates.iter() {
                match idx.deployables_by_crates.entry(cr.name.clone()) {
                    Entry::Occupied(_) => bail!("multiple deployables use crate {}", cr.name),
                    Entry::Vacant(e) => {
                        if idx.deployables_by_repair.contains_key(&cr.name) {
                            bail!(
                                "deployable repair {} uses crate of {}",
                                &idx.deployables_by_repair[&cr.name],
                                name
                            )
                        }
                        e.insert(name.clone())
                    }
                };
            }
            for c in dep.crates.iter().chain(dep.repair_crate.iter()) {
                match idx.crates_by_name.entry(c.name.clone()) {
                    Entry::Occupied(_) => bail!("duplicate crate name {}", c.name),
                    Entry::Vacant(e) => e.insert(c.clone()),
                };
            }
            if let DeployableKind::Objective(DeployableObjective {
                pad_templates,
                defenses_template,
                ammo_template,
                fuel_template,
                barracks_template,
            }) = &dep.kind
            {
                let mut names = FxHashSet::default();
                for name in defenses_template
                    .iter()
                    .chain(ammo_template.iter())
                    .chain(fuel_template.iter())
                    .chain(barracks_template.iter())
                    .chain(pad_templates.iter())
                {
                    miz.get_group_by_name(mizidx, GroupKind::Any, side, name)?
                        .ok_or_else(|| anyhow!("missing farp template {:?} {:?}", side, name))?;
                    if !names.insert(name) {
                        bail!(
                            "deployables with logistics must use unique templates for each part {name} is reused"
                        )
                    }
                }
                for pad in pad_templates {
                    if !idx
                        .pad_templates
                        .entry(name.clone())
                        .or_default()
                        .insert(pad.clone())
                    {
                        bail!("{:?} has a duplicate pad template {pad}", dep)
                    }
                    if !self.global_pad_templates.insert(pad.clone()) {
                        bail!(
                            "pad template names must be globally unique {pad} is used more than once"
                        )
                    }
                    let gifo = miz
                        .get_group_by_name(mizidx, GroupKind::Any, side, pad)?
                        .ok_or_else(|| anyhow!("missing pad template {:?} {:?}", side, pad))?;
                    for unit in gifo.group.units()? {
                        let unit = unit?;
                        let uname = unit.name()?;
                        if &uname != pad {
                            bail!(
                                "pad template groups and units must be named the same thing {uname} != {pad}"
                            )
                        }
                    }
                }
                if dep.limit as usize > pad_templates.len() {
                    bail!(
                        "{:?} does not have enough pad templates {} are required {} are provided",
                        dep,
                        dep.limit,
                        pad_templates.len()
                    )
                }
            }
        }
        Ok(())
    }

    pub fn dirty(&mut self) {
        self.dirty = true
    }

    pub(super) fn take_dirty(&mut self) -> bool {
        let cur = self.dirty;
        self.dirty = false;
        cur
    }

    pub fn slot_instance_unit<'lua>(&self, lua: MizLua<'lua>, slot: &SlotId) -> Result<Unit<'lua>> {
        self.object_id_by_slot
            .get(slot)
            .ok_or_else(|| anyhow!("unit {:?} not currently in the mission", slot))
            .and_then(|id| Unit::get_instance(lua, id))
    }

    pub fn slot_instance_pos(&self, lua: MizLua, slot: &SlotId) -> Result<Position3> {
        self.slot_instance_unit(lua, slot)?.get_position()
    }

    pub fn players_to_force_to_spectators<'a>(
        &'a mut self,
        now: DateTime<Utc>,
    ) -> BTreeMap<DateTime<Utc>, SmallVec<[Ucid; 1]>> {
        let keep = self.force_to_spectators.split_off(&now);
        mem::replace(&mut self.force_to_spectators, keep)
    }

    pub fn cancel_force_to_spectators(&mut self, ucid: &Ucid) {
        info!("canceling force to spectators for {ucid}");
        self.force_to_spectators.retain(|_, ids| {
            ids.retain(|pucid| pucid != ucid);
            !ids.is_empty()
        })
    }

    pub fn force_player_to_spectators(&mut self, ucid: &Ucid) {
        self.force_to_spectators
            .entry(Utc::now())
            .or_default()
            .push(ucid.clone())
    }

    pub fn force_player_to_spectators_at(&mut self, ucid: &Ucid, ts: DateTime<Utc>) {
        self.force_to_spectators
            .entry(ts)
            .or_default()
            .push(ucid.clone())
    }

    pub(super) fn player_deslot(
        &mut self,
        per: &Persisted,
        slot: &SlotId,
        expected_ucid: Option<Ucid>,
    ) -> Option<(UnitId, Ucid)> {
        if let Some(ucid) = self.players_by_slot.swap_remove(slot) {
            if let Some(expected_ucid) = expected_ucid {
                if expected_ucid != ucid {
                    error!("players_by_slot ucid mismatch {expected_ucid} vs {ucid} in slot {slot}")
                }
            }
            info!("deslotting player {ucid}");
            if let Some(player) = per.players.get(&ucid) {
                if !player.changing_slots && !player.jtac_or_spectators {
                    info!("queuing force player {ucid} to spectators");
                    self.force_to_spectators
                        .entry(Utc::now())
                        .or_default()
                        .push(ucid.clone());
                }
            }
            self.cargo.remove(slot);
            if let Some(id) = self.object_id_by_slot.remove(slot) {
                self.slot_by_object_id.remove(&id);
                if let Some(uid) = self.uid_by_object_id.remove(&id) {
                    self.object_id_by_uid.remove(&uid);
                    self.units_able_to_move.swap_remove(&uid);
                    return Some((uid, ucid));
                }
            }
            // Benign: the player left / died before the slot ever finished
            // instancing (ramp death, quick disconnect), or the unit mapping
            // was already torn down by an earlier event. The caller handles
            // None. Not worth an ERROR every time it happens.
            debug!("deslot {slot} {ucid}: no unit id (never instanced or already cleaned)");
        }
        None
    }

    pub(super) fn unit_dead(
        &mut self,
        per: &Persisted,
        id: &DcsOid<ClassUnit>,
    ) -> Option<(UnitId, Option<Ucid>)> {
        let (uid, ucid) = match self.slot_by_object_id.remove(id) {
            Some(slot) => match self.player_deslot(per, &slot, None) {
                Some((uid, ucid)) => (uid, Some(ucid)),
                None => return None,
            },
            None => match self.uid_by_object_id.remove(id) {
                Some(uid) => {
                    self.object_id_by_uid.remove(&uid);
                    (uid, None)
                }
                None => {
                    info!("no uid for object id {:?}", id);
                    return None;
                }
            },
        };
        self.units_potentially_close_to_enemies.remove(&uid);
        self.units_able_to_move.swap_remove(&uid);
        Some((uid, ucid))
    }

    pub fn player_in_slot(&self, slot: &SlotId) -> Option<&Ucid> {
        self.players_by_slot.get(slot)
    }

    pub fn player_in_unit(&self, id: &DcsOid<ClassUnit>) -> Option<&Ucid> {
        self.slot_by_object_id
            .get(id)
            .and_then(|slot| self.players_by_slot.get(slot))
    }

    pub fn panel_to_player<S: Into<String>>(
        &mut self,
        persisted: &Persisted,
        duration: i64,
        ucid: &Ucid,
        msg: S,
    ) {
        if let Some(player) = persisted.players.get(ucid) {
            if let Some(ifo) = player
                .current_slot
                .as_ref()
                .and_then(|(s, _)| self.slot_info.get(s))
            {
                let miz_id = ifo.miz_gid;
                self.msgs().panel_to_group(duration, false, miz_id, msg);
            }
        }
    }

    pub(super) fn set_cfg(
        &mut self,
        miz: &Miz,
        mizidx: &MizIndex,
        cfg: Arc<Cfg>,
        to_bg: UnboundedSender<Task>,
    ) -> Result<()> {
        self.to_bg = Some(to_bg);
        let check_unit_classification = || -> Result<()> {
            let mut not_classified = FxHashSet::default();
            for side in Side::ALL {
                let coa = miz.coalition(side)?;
                for country in coa.countries()? {
                    let country = country?;
                    for group in country
                        .planes()?
                        .into_iter()
                        .chain(country.helicopters()?)
                        .chain(country.vehicles()?)
                        .chain(country.ships()?)
                        .chain(country.statics()?)
                    {
                        let group = group?;
                        for unit in group.units()? {
                            let typ = unit?.typ()?;
                            if !cfg.unit_classification.contains_key(typ.as_str()) {
                                not_classified.insert(typ);
                            }
                        }
                    }
                }
            }
            if not_classified.is_empty() {
                Ok(())
            } else {
                bail!("unit types not classified {:?}", not_classified)
            }
        };
        check_unit_classification()?;
        if let Some(VictoryCondition::MapOwned { fraction }) = cfg.auto_reset.map(|vc| vc.condition)
        {
            if fraction > 1. || fraction < 0. {
                bail!("auto_reset fraction must be between 0 and 1")
            }
        }
        for (side, template) in cfg.crate_template.iter() {
            miz.get_group_by_name(mizidx, GroupKind::Any, *side, template)?
                .ok_or_else(|| anyhow!("missing crate template {:?} {template}", side))?;
        }
        let points = cfg.points.is_some();
        for (side, deployables) in cfg.deployables.iter() {
            let repair_crate = maybe!(cfg.repair_crate, side, "side repair crate")?.clone();
            self.index_deployables_for_side(
                miz,
                mizidx,
                *side,
                repair_crate,
                &cfg.warehouse,
                points,
                deployables,
            )?
        }
        for (side, troops) in cfg.troops.iter() {
            let idx = Arc::make_mut(self.deployable_idx.entry(*side).or_default());
            for troop in troops {
                miz.get_group_by_name(mizidx, GroupKind::Any, *side, &troop.template)?
                    .ok_or_else(|| anyhow!("missing troop template {:?} {:?}", side, troop.name))?;
                if !points && troop.cost > 0 {
                    bail!(
                        "the points system is disabled but {} troops cost points",
                        troop.name
                    )
                }
                match idx.squads_by_name.entry(troop.name.clone()) {
                    Entry::Occupied(_) => bail!("duplicate squad name {}", troop.name),
                    Entry::Vacant(e) => e.insert(troop.clone()),
                };
            }
        }
        for (side, actions) in &cfg.actions {
            for (_, act) in actions {
                if !points && (act.cost > 0 || act.penalty.unwrap_or(0) > 0) {
                    bail!("the points system is disabled but {act:?} costs points")
                }
                match &act.kind {
                    ActionKind::Awacs(AwacsCfg {
                        plane: AiPlaneCfg { template, .. },
                        ..
                    })
                    | ActionKind::Bomber(BomberCfg {
                        plane: AiPlaneCfg { template, .. },
                        ..
                    })
                    | ActionKind::CruiseMissileSpawn(AiPlaneCfg { template, .. })
                    | ActionKind::Tanker(AiPlaneCfg { template, .. })
                    | ActionKind::Drone(DroneCfg {
                        plane: AiPlaneCfg { template, .. },
                        ..
                    })
                    | ActionKind::Fighters(AiPlaneCfg { template, .. })
                    | ActionKind::Attackers(AiPlaneCfg { template, .. })
                    | ActionKind::LogisticsRepair(AiPlaneCfg { template, .. })
                    | ActionKind::LogisticsTransfer(AiPlaneCfg { template, .. }) => {
                        miz.get_group_by_name(mizidx, GroupKind::Any, *side, template.as_str())?
                            .ok_or_else(|| anyhow!("missing template for action {act:?}"))?;
                    }
                    ActionKind::Deployable(DeployableCfg { name, plane }) => {
                        if let Some(AiPlaneCfg { template, .. }) = plane {
                            miz.get_group_by_name(
                                mizidx,
                                GroupKind::Any,
                                *side,
                                template.as_str(),
                            )?
                            .ok_or_else(|| anyhow!("missing template for action {act:?}"))?;
                        }
                        self.deployable_idx
                            .get(side)
                            .and_then(|idx| idx.deployables_by_name.get(name))
                            .ok_or_else(|| anyhow!("missing deployable for action {act:?}"))?;
                    }
                    ActionKind::Paratrooper(DeployableCfg {
                        name,
                        plane: Some(AiPlaneCfg { template, .. }),
                    }) => {
                        miz.get_group_by_name(mizidx, GroupKind::Any, *side, template.as_str())?
                            .ok_or_else(|| anyhow!("missing template for action {act:?}"))?;
                        self.deployable_idx
                            .get(side)
                            .and_then(|idx| idx.squads_by_name.get(name))
                            .ok_or_else(|| anyhow!("missing troop for action {act:?}"))?;
                    }
                    ActionKind::Paratrooper(DeployableCfg { name, plane: None }) => {
                        bail!("patroop mission {name} does not include an ai plane config")
                    }
                    ActionKind::AwacsWaypoint
                    | ActionKind::TankerWaypoint
                    | ActionKind::DroneWaypoint
                    | ActionKind::CruiseMissileWaypoint
                    | ActionKind::FighersWaypoint
                    | ActionKind::AttackersWaypoint
                    | ActionKind::Sead(_)
                    | ActionKind::SeadWaypoint
                    | ActionKind::Move(_)
                    | ActionKind::Rtb
                    | ActionKind::Nuke(_)
                    | ActionKind::CarrierWaypoint
                    | ActionKind::CarrierRepair
                    | ActionKind::CarrierRespawn
                    // Artillery uses existing Armor/Mr/Lr groups; no template validation needed.
                    | ActionKind::Artillery(_)
                    | ActionKind::NavalCruiseMissileStrike(_)
                    // The tasking board draws F10 marks, it spawns nothing,
                    // so there is no template to validate.
                    | ActionKind::AddTask(_)
                    | ActionKind::RemoveTask(_)
                    | ActionKind::Recon(_) => (),
                }
            }
        }
        self.cfg = cfg;
        Ok(())
    }

    pub(super) fn spawn_group<'lua>(
        &mut self,
        perf: &mut PerfInner,
        persisted: &Persisted,
        idx: &MizIndex,
        spctx: &SpawnCtx<'lua>,
        group: &SpawnedGroup,
        mission: Vec<MissionPoint<'lua>>,
    ) -> Result<Option<Spawned<'lua>>> {
        let ts = Utc::now();

        // Check if this is a carrier group defined in config, or falls back to prefix matching
        // For carrier groups, spawn via coalition.addGroup() with positions set to the saved
        // state so the carrier appears directly at its last known location.
        let carrier_cfg = self.cfg.carrier.as_ref()
            .and_then(|c| c.groups.iter().find(|g| g.template.as_str() == group.template_name.as_str()));

        let is_carrier_group = carrier_cfg.is_some()
            || group.template_name.starts_with("BCARRIER")
            || group.template_name.starts_with("RCARRIER")
            || group.template_name.starts_with("NCARRIER");

        if is_carrier_group {
            let display_name = carrier_cfg
                .map(|c| c.display_name.clone())
                .unwrap_or_else(|| group.template_name.clone());
            info!("[CARRIER_SPAWN] Activating carrier group {} (template: {}, display: {})",
                  group.name, group.template_name, display_name);

            // Build map of template_name -> persisted unit data for position lookup
            let by_tname: FxHashMap<&str, &SpawnedUnit> = group
                .units
                .into_iter()
                .filter_map(|uid| {
                    persisted.units.get(uid).and_then(|u| {
                        if u.dead {
                            None
                        } else {
                            Some((u.template_name.as_str(), u))
                        }
                    })
                })
                .collect();

            // Deep-clone the template so we can modify positions without touching the
            // original miz data, then spawn via coalition.addGroup() which reads directly
            // from the Lua table — so the carrier spawns at the saved positions.
            let miz_template = spctx
                .get_template(idx, GroupKind::Any, group.side, group.template_name.as_str())
                .with_context(|| format_compact!("getting carrier miz template {}", group.template_name))?;

            // Must disable lateActivation in the clone so coalition.addGroup() spawns it immediately.
            miz_template.group.set("lateActivation", false)?;

            // Set each unit's position in the cloned template to the saved position.
            // Also update the group centroid and route waypoints to prevent circling.
            {
                let units = miz_template.group.units().context("getting carrier miz units")?;
                let mut centroid_x = 0.0f64;
                let mut centroid_y = 0.0f64;
                let mut count = 0u32;
                for i in 1..=(units.len() as i64) {
                    if let Ok(unit) = units.get(i) {
                        let unit_name = unit.name()?;
                        if let Some(su) = by_tname.get(unit_name.as_str()) {
                            let dist = na::distance(&su.pos.into(), &su.spawn_pos.into());
                            info!("[CARRIER_SPAWN] Setting unit '{}' position: ({:.0}, {:.0}) -> ({:.0}, {:.0}), dist={:.0}m",
                                  unit_name, su.spawn_pos.x, su.spawn_pos.y, su.pos.x, su.pos.y, dist);
                            unit.set_pos(su.pos)?;
                            unit.set_heading(su.heading)?;
                            centroid_x += su.pos.x;
                            centroid_y += su.pos.y;
                            count += 1;
                        }
                    }
                }

                if count > 0 {
                    let cx = centroid_x / count as f64;
                    let cy = centroid_y / count as f64;
                    miz_template.group.set_pos(Vector2::new(cx, cy))?;
                    info!("[CARRIER_SPAWN] Set group position to ({:.0}, {:.0})", cx, cy);

                    // Override all route waypoints to the carrier's saved position so DCS
                    // doesn't send it circling to old ME waypoints before StopRoute takes effect.
                    if let Ok(route) = miz_template.group.raw_get::<_, mlua::Table>("route") {
                        if let Ok(points) = route.raw_get::<_, mlua::Table>("points") {
                            let num_points = points.raw_len();
                            for i in 1..=(num_points as i64) {
                                if let Ok(point) = points.raw_get::<_, mlua::Table>(i) {
                                    point.raw_set("x", cx)?;
                                    point.raw_set("y", cy)?;
                                }
                            }
                            info!("[CARRIER_SPAWN] Set all {} route waypoints to ({:.0}, {:.0})", num_points, cx, cy);
                        }
                    }
                }
            }

            // Spawn via coalition.addGroup() — the carrier appears directly at the saved positions.
            let dcs_group = match spctx.spawn(miz_template)
                .with_context(|| format_compact!("spawning carrier group {}", group.template_name))? {
                Spawned::Group(g) => g,
                other => bail!("[CARRIER_SPAWN] Expected Group from carrier spawn of {}, got {:?}",
                               group.template_name, other),
            };
            let oid = dcs_group.object_id()?.erased();
            self.object_id_by_gid.insert(group.id, oid.clone());
            self.gid_by_object_id.insert(oid, group.id);

            // Manually register each unit's DCS object ID mappings.
            // DCS does NOT fire S_EVENT_BIRTH for coalition.addGroup(), so we must do this here.
            info!("[CARRIER_SPAWN] Registering units from activated group {} ({} persisted units)",
                  group.template_name, group.units.len());
            let mut needs_repositioning = false;
            match dcs_group.get_units() {
                Ok(dcs_units) => {
                    for dcs_unit_res in dcs_units {
                        let dcs_unit = match dcs_unit_res {
                            Ok(u) => u,
                            Err(e) => {
                                error!("[CARRIER_SPAWN] Failed to get DCS unit from sequence: {:?}", e);
                                continue;
                            }
                        };
                        match dcs_unit.get_name() {
                            Ok(dcs_unit_name) => {
                                // Log actual DCS position after activation to verify position fix
                                match dcs_unit.get_ground_position() {
                                    Ok(pos) => info!("[CARRIER_SPAWN] DCS unit '{}' activated at position ({:.0}, {:.0})",
                                                     dcs_unit_name, pos.0.x, pos.0.y),
                                    Err(e) => info!("[CARRIER_SPAWN] Could not read position for '{}': {:?}",
                                                    dcs_unit_name, e),
                                }
                                let uid_match = group.units.into_iter().find_map(|uid| {
                                    persisted.units.get(uid).and_then(|u| {
                                        if !u.dead && u.template_name.as_str() == dcs_unit_name.as_str() {
                                            Some((*uid, u))
                                        } else {
                                            None
                                        }
                                    })
                                });
                                if let Some((uid, unit)) = uid_match {
                                    match dcs_unit.object_id() {
                                        Ok(unit_oid) => {
                                            info!("[CARRIER_SPAWN] Registering carrier unit '{}' (uid={:?}) with DCS object_id {:?}",
                                                  unit.name, uid, unit_oid);
                                            self.uid_by_object_id.insert(unit_oid.clone(), uid);
                                            self.object_id_by_uid.insert(uid, unit_oid.clone());
                                            self.units_potentially_close_to_enemies.insert(uid);
                                            if unit.tags.contains(UnitTag::Driveable) || unit.tags.contains(UnitTag::Boat) {
                                                self.units_able_to_move.insert(uid);
                                                info!("[CARRIER_SPAWN] Added carrier unit '{}' to units_able_to_move",
                                                      unit.name);
                                            }
                                            // Verify unit spawned at the expected position.
                                            // With coalition.addGroup() this should always be correct,
                                            // but setPosition is attempted as a fallback if not.
                                            match dcs_unit.get_ground_position() {
                                                Ok(actual) => {
                                                    let expected = unit.pos;
                                                    let dist = na::distance(
                                                        &na::Point2::new(actual.0.x, actual.0.y),
                                                        &na::Point2::new(expected.x, expected.y),
                                                    );
                                                    info!("[CARRIER_TELEPORT] Unit '{}': actual=({:.0},{:.0}) expected=({:.0},{:.0}) dist={:.0}m",
                                                          unit.name, actual.0.x, actual.0.y, expected.x, expected.y, dist);
                                                    if dist > 100.0 {
                                                        needs_repositioning = true;
                                                        info!("[CARRIER_TELEPORT] Teleporting '{}' {:.0}m to persisted position ({:.0},{:.0})",
                                                              unit.name, dist, expected.x, expected.y);
                                                        match dcs_unit.as_object() {
                                                            Ok(obj) => {
                                                                if let Err(e) = obj.set_position(unit.position) {
                                                                    error!("[CARRIER_TELEPORT] Failed to teleport '{}': {:?}", unit.name, e);
                                                                } else {
                                                                    match dcs_unit.get_ground_position() {
                                                                        Ok(after) => info!("[CARRIER_TELEPORT] '{}' now at ({:.0},{:.0}) after teleport",
                                                                                          unit.name, after.0.x, after.0.y),
                                                                        Err(e) => warn!("[CARRIER_TELEPORT] Could not verify position after teleport for '{}': {:?}",
                                                                                       unit.name, e),
                                                                    }
                                                                }
                                                            }
                                                            Err(e) => error!("[CARRIER_TELEPORT] Failed to get Object for '{}': {:?}", unit.name, e),
                                                        }
                                                    } else {
                                                        info!("[CARRIER_TELEPORT] '{}' already at correct position (dist={:.0}m), no teleport needed",
                                                              unit.name, dist);
                                                    }
                                                }
                                                Err(e) => warn!("[CARRIER_TELEPORT] Could not read position for '{}' to check teleport: {:?}",
                                                               unit.name, e),
                                            }
                                        }
                                        Err(e) => {
                                            error!("[CARRIER_SPAWN] Failed to get object_id for '{}': {:?}",
                                                   dcs_unit_name, e);
                                        }
                                    }
                                } else {
                                    info!("[CARRIER_SPAWN] DCS unit '{}' not matched to any persisted unit",
                                           dcs_unit_name);
                                }
                            }
                            Err(e) => {
                                error!("[CARRIER_SPAWN] Failed to get name for DCS unit: {:?}", e);
                            }
                        }
                    }
                    info!("[CARRIER_SPAWN] Carrier unit registration complete for {} - units_able_to_move has {} entries",
                          group.name, self.units_able_to_move.len());
                }
                Err(e) => {
                    error!("[CARRIER_SPAWN] Failed to get units from activated carrier group {}: {:?}",
                           group.template_name, e);
                }
            }

            // Immediately stop the carrier from following its mission editor route.
            // Without this, the carrier will circle through ME waypoints after activation.
            let controller = dcs_group.get_controller()
                .context("getting carrier controller after activation")?;
            controller.set_command(dcso3::controller::Command::StopRoute(true))
                .context("issuing StopRoute to carrier")?;
            controller.reset_task()
                .context("resetting carrier tasks")?;
            info!("[CARRIER_SPAWN] Issued StopRoute and resetTask for {} to prevent circling", group.template_name);

            // If carrier had a commanded waypoint destination, issue movement command
            // to continue traveling there (starting from the correct activated position).
            let waypoint_pos = persisted.objectives_by_group.get(&group.id)
                .and_then(|oid| {
                    info!("[CARRIER_SPAWN] Group {} is linked to objective {:?}", group.template_name, oid);
                    persisted.objectives.get(oid)
                })
                .and_then(|obj| {
                    match &obj.kind {
                        ObjectiveKind::CarrierGroup { waypoint, .. } => {
                            info!("[CARRIER_SPAWN] Objective is CarrierGroup, waypoint={:?}", waypoint);
                            waypoint.as_ref().map(|wp| *wp)
                        }
                        other => {
                            info!("[CARRIER_SPAWN] Objective is not CarrierGroup: {:?}", mem::discriminant(other));
                            None
                        }
                    }
                });

            if let Some(target) = waypoint_pos {
                let speed = if needs_repositioning {
                    self.cfg.carrier.as_ref()
                        .map(|c| c.spawn_repositioning_speed)
                        .unwrap_or(100.0)
                } else {
                    self.cfg.carrier.as_ref().map(|c| c.movement_speed).unwrap_or(5.0)
                };
                info!("[CARRIER_SPAWN] Commanding carrier {} to continue to waypoint ({:.0}, {:.0}) at speed {:.1}{}",
                      group.template_name, target.x, target.y, speed,
                      if needs_repositioning { " (repositioning speed)" } else { "" });
                // Re-enable route following and set the new destination
                controller.set_command(dcso3::controller::Command::StopRoute(false))
                    .context("re-enabling carrier route")?;
                controller.set_task(dcso3::controller::Task::Mission {
                    route: vec![MissionPoint {
                        action: None,
                        airdrome_id: None,
                        helipad: None,
                        typ: PointType::TurningPoint,
                        time_re_fu_ar: None,
                        link_unit: None,
                        pos: LuaVec2(target),
                        alt: 0.,
                        alt_typ: None,
                        speed,
                        speed_locked: None,
                        eta: None,
                        eta_locked: None,
                        name: Some(dcso3::String::from("restore_waypoint")),
                        task: Box::new(dcso3::controller::Task::ComboTask(vec![])),
                    }],
                    airborne: Some(false),
                }).context("setting carrier waypoint restore task")?;
                info!("[CARRIER_SPAWN] Waypoint task set successfully for {}", group.template_name);
            } else {
                info!("[CARRIER_SPAWN] Carrier {} activated at saved position with StopRoute, no active waypoint",
                      group.template_name);
            }

            // Carrier auto-navaids (TACAN / ICLS / ACLS / Link-4) are lit by
            // the periodic sweep in `maybe_reallocate_navaids`, not here: DCS
            // only registers the carrier deck as an airbase a beat after the
            // group spawns, so this path can't reliably resolve it.

            record_perf(&mut perf.spawn, ts);
            return Ok(Some(Spawned::Group(dcs_group)));
        }

        // Collect alive units and their positions.
        let mut points: SmallVec<[Vector2; 16]> = smallvec![];
        let alive_units: Vec<&SpawnedUnit> = group
            .units
            .into_iter()
            .filter_map(|uid| {
                persisted.units.get(uid).and_then(|u| {
                    points.push(u.pos);
                    if u.dead { None } else { Some(u) }
                })
            })
            .collect();

        // Check whether this is a synthetic (inline-config) group.  If so, build a Lua
        // group table from scratch instead of cloning a .miz template.
        // Filled in by the CAP / hot-start branch below when a real parking
        // start is built. DCS only honours a `TakeOffParking*` waypoint 0 if
        // the units it applies to are actually sitting at the field: the whole
        // group table has to agree (group x/y, unit x/y/alt, unit `parking`),
        // or DCS quietly discards the ground start and spawns the flight
        // airborne over the waypoint instead.
        let mut parking_plan: Vec<dcso3::airbase::ParkingSpot> = vec![];
        // Where to put units we couldn't get a dedicated spot for: the field
        // itself, with any template parking assignment cleared so DCS picks.
        let mut parking_fallback: Option<(Vector2, f64)> = None;
        let mut parking_anchor: Option<Vector2> = None;
        let (template, alive) = if let Some(spec) = self.synthetic_templates.get(group.template_name.as_str()).cloned() {
            if alive_units.is_empty() {
                record_perf(&mut perf.spawn, ts);
                return Ok(None);
            }
            use dcso3::LuaEnv as _;
            let lua_inner = spctx.lua().inner();
            let units_tbl = lua_inner.create_table()?;
            for (i, su) in alive_units.iter().enumerate() {
                let unit_tbl = lua_inner.create_table()?;
                unit_tbl.raw_set("type", su.typ.0.as_str())?;
                unit_tbl.raw_set("name", su.name.as_str())?;
                unit_tbl.raw_set("x", su.pos.x)?;
                unit_tbl.raw_set("y", su.pos.y)?;
                unit_tbl.raw_set("heading", su.heading)?;
                unit_tbl.raw_set("psi", -su.heading)?;
                unit_tbl.raw_set("skill", "High")?;
                unit_tbl.raw_set("playerCanDrive", false)?;
                unit_tbl.raw_set("coldAtStart", false)?;
                units_tbl.raw_set(i + 1, unit_tbl)?;
            }
            let point = centroid2d(points.iter().map(|p| *p));
            let wp_tbl = lua_inner.create_table()?;
            wp_tbl.raw_set("x", point.x)?;
            wp_tbl.raw_set("y", point.y)?;
            wp_tbl.raw_set("alt", 0.0f64)?;
            wp_tbl.raw_set("type", "Turning Point")?;
            wp_tbl.raw_set("action", "Off Road")?;
            wp_tbl.raw_set("speed", 0.0f64)?;
            wp_tbl.raw_set("speed_locked", true)?;
            let combo_params = lua_inner.create_table()?;
            combo_params.raw_set("tasks", lua_inner.create_table()?)?;
            let combo_tbl = lua_inner.create_table()?;
            combo_tbl.raw_set("id", "ComboTask")?;
            combo_tbl.raw_set("params", combo_params)?;
            wp_tbl.raw_set("task", combo_tbl)?;
            let wps_tbl = lua_inner.create_table()?;
            wps_tbl.raw_set(1, wp_tbl)?;
            let route_tbl = lua_inner.create_table()?;
            route_tbl.raw_set("points", wps_tbl)?;
            let group_tbl = lua_inner.create_table()?;
            group_tbl.raw_set("name", group.name.as_str())?;
            group_tbl.raw_set("groupId", 0i64)?;
            group_tbl.raw_set("x", point.x)?;
            group_tbl.raw_set("y", point.y)?;
            group_tbl.raw_set("lateActivation", false)?;
            group_tbl.raw_set("hidden", false)?;
            group_tbl.raw_set("visible", true)?;
            group_tbl.raw_set("uncontrolled", false)?;
            group_tbl.raw_set("task", "Ground Nothing")?;
            group_tbl.raw_set("route", route_tbl)?;
            group_tbl.raw_set("units", units_tbl)?;
            let miz_group = miz::Group::from_lua(LuaValue::Table(group_tbl), lua_inner)
                .map_err(|e| anyhow!("building synthetic group table: {e}"))?;
            use dcso3::env::miz::GroupInfo;
            let template = GroupInfo {
                side: group.side,
                country: spec.country,
                category: match spec.category {
                    GroupCategory::Ground | GroupCategory::Train => dcso3::env::miz::GroupKind::Vehicle,
                    GroupCategory::Ship => dcso3::env::miz::GroupKind::Ship,
                    GroupCategory::Airplane | GroupCategory::Helicopter => dcso3::env::miz::GroupKind::Plane,
                },
                group: miz_group,
            };
            (template, true)
        } else {
            let template = spctx
                .get_template(
                    idx,
                    GroupKind::Any,
                    group.side,
                    group.template_name.as_str(),
                )
                .with_context(|| format_compact!("getting template {}", group.template_name))?;
            template.group.set("lateActivation", false)?;
            template.group.set("hidden", false)?;
            template.group.set("visible", true)?;
            template.group.set_name(group.name.clone())?;
            let group_clone = template.group.clone();
            // A plain vehicle group placed in the ME can come through with no
            // `route` table at all -- give it an empty one rather than blowing
            // up ("expected a table, got Nil"). The points we set below (or the
            // caller's `mission`) then land somewhere valid.
            let route = match group_clone.route() {
                Ok(r) => r,
                Err(_) => {
                    use dcso3::LuaEnv as _;
                    let inner = spctx.lua().inner();
                    let r = inner.create_table()?;
                    r.raw_set("points", inner.create_table()?)?;
                    group_clone.raw_set("route", r)?;
                    group_clone.route().context("getting route")?
                }
            };
            let mut points: Vec<dcso3::controller::MissionPoint> = if mission.len() > 0 {
                mission
            } else {
                route.points().ok().map(|seq| seq.into_iter().filter_map(|p| p.ok()).collect()).unwrap_or_default()
            };

            if group.tags.contains(UnitTag::CAP) || group.tags.contains(UnitTag::HotStart) {
                // Resolve the origin airbase FIRST -- we need its id AND its
                // position to build a valid parking start. A `TakeOffParkingHot`
                // waypoint with no airdromeId (or one whose x/y/alt point
                // nowhere near the field) makes DCS air-start the flight, which
                // is exactly the bug we're fighting. If we can't resolve the
                // base, leave WP0 alone (ugly air start, but survivable) and
                // let `enforce_cap_ground_start` scrap the event.
                let airbase = match &group.origin {
                    DeployKind::Objective { origin } => {
                        self.resolve_airbase(spctx.lua(), persisted, origin)
                    }
                    _ => None,
                };
                let helicopter = template.category == GroupKind::Helicopter;
                // Set when we fall back to starting a helicopter on open
                // ground; the group-table fixups below key off it the same way
                // they key off a parking start.
                let mut ground_start = false;
                let ab_start = airbase.as_ref().and_then(|ab| {
                    let id = ab.get_id().ok()?;
                    let p = ab.get_point().ok()?;
                    // A FARP or a carrier deck is an Airbase too, but DCS
                    // resolves it through `helipadId` + `linkUnit`; feeding its
                    // id to `airdromeId` finds no airdrome, and the flight
                    // spawns wrong. `getDesc().category` is not reliable for a
                    // FARP -- an HL## FOB came back as Airdrome and put a helo
                    // in the ground -- so what the campaign says this objective
                    // is wins, and DCS's answer is only the fallback.
                    let kind = match &group.origin {
                        DeployKind::Objective { origin } => {
                            persisted.objectives.get(origin).map(|o| o.kind.clone())
                        }
                        _ => None,
                    };
                    let cat = match kind {
                        Some(ObjectiveKind::Farp { .. }) | Some(ObjectiveKind::Fob) => {
                            dcso3::airbase::AirbaseCategory::Helipad
                        }
                        Some(ObjectiveKind::CarrierGroup { .. }) => {
                            dcso3::airbase::AirbaseCategory::Ship
                        }
                        _ => ab
                            .get_category()
                            .unwrap_or(dcso3::airbase::AirbaseCategory::Airdrome),
                    };
                    Some((id, p, cat))
                });

                // Build the parking plan. `getParking(true)` is DCS's own
                // free-spot list; a spot is only a legal start if it can take
                // an aircraft at all (`TO_AC`) and its terminal type matches
                // the airframe -- fixed wing want shelters / open medium /
                // open big, helicopters want pads / open ramp, and the runway
                // "spots" are not parking. Nearest-to-runway first so a flight
                // isn't strung across the whole field.
                if let (Some(ab), Some((_, p, _))) = (airbase.as_ref(), ab_start.as_ref()) {
                    let want = alive_units.len().max(1);
                    match ab.get_parking_spots(true) {
                        Ok(spots) => {
                            let total = spots.len();
                            let mut usable: Vec<dcso3::airbase::ParkingSpot> = spots
                                .into_iter()
                                .filter(|s| s.usable_by(helicopter))
                                .collect();
                            usable.sort_by(|a, b| {
                                a.preference(helicopter)
                                    .cmp(&b.preference(helicopter))
                                    .then_with(|| {
                                        a.dist_to_rw
                                            .partial_cmp(&b.dist_to_rw)
                                            .unwrap_or(std::cmp::Ordering::Equal)
                                    })
                            });
                            usable.truncate(want);
                            if usable.len() < want {
                                warn!(
                                    "[GROUND_START] {} wanted {want} parking spots, only {} of {total} free spots are usable by this airframe -- the rest go on the field for DCS to assign",
                                    group.name,
                                    usable.len()
                                );
                            }
                            parking_plan = usable;
                        }
                        Err(e) => warn!(
                            "[GROUND_START] {} could not read parking at its origin field ({e:?}) -- placing on the field and letting DCS assign spots",
                            group.name
                        ),
                    }
                    parking_fallback = Some((Vector2::new(p.0.x, p.0.z), p.0.y));
                    parking_anchor = Some(match parking_plan.first() {
                        Some(spot) => Vector2::new(spot.pos.0.x, spot.pos.0.z),
                        None => Vector2::new(p.0.x, p.0.z),
                    });
                }

                if points.is_empty() {
                    warn!(
                        "[GROUND_START] {} template has no route waypoints -- cannot rewrite a parking start, so this flight will air-start (a CAP is then scrapped by enforce_cap_ground_start)",
                        group.name
                    );
                }
                if let Some(first) = points.first_mut() {
                    match (&ab_start, parking_anchor) {
                        (Some((abid, p, cat)), Some(anchor)) => {
                            // The exact shape DCS itself uses when it spawns a
                            // ground-starting flight at runtime (see
                            // Scripts/GeneratedTasks/modules/one_plane_attack_*.lua):
                            // TakeOffParkingHot + "From Parking Area Hot",
                            // airdromeId, a waypoint sitting on the field at
                            // field elevation BARO, and zero locked speed with
                            // a zero locked ETA. Leave any of those out and DCS
                            // falls back to an air start.
                            first.typ = dcso3::controller::PointType::TakeOffParkingHot;
                            first.action = Some(dcso3::controller::ActionTyp::Air(
                                dcso3::controller::TurnMethod::FromParkingAreaHot,
                            ));
                            // FARP/helipad airbases hand back getPoint() with
                            // y = 0. A parking start at 0m BARO under hundreds
                            // of metres of terrain spawns the flight inside the
                            // ground, which is how the first HL## helo mission
                            // died 15 seconds after dispatch. A carrier deck is
                            // genuinely just above the sea, so leave it alone.
                            let mut elev = p.0.y;
                            if elev <= 0.0
                                && *cat != dcso3::airbase::AirbaseCategory::Ship
                            {
                                if let Ok(h) = dcso3::land::Land::singleton(spctx.lua())
                                    .and_then(|l| l.get_height(LuaVec2(anchor)))
                                {
                                    elev = h;
                                }
                            }
                            first.pos = LuaVec2(anchor);
                            first.alt = elev;
                            first.alt_typ = Some(dcso3::controller::AltType::BARO);
                            match cat {
                                dcso3::airbase::AirbaseCategory::Airdrome => {
                                    first.airdrome_id = Some(*abid);
                                    first.helipad = None;
                                    first.link_unit = None;
                                }
                                dcso3::airbase::AirbaseCategory::Helipad
                                | dcso3::airbase::AirbaseCategory::Ship => {
                                    first.airdrome_id = None;
                                    first.helipad = Some(*abid);
                                    first.link_unit = Some(dcso3::env::miz::UnitId::from(abid.inner()));
                                }
                            }
                            first.speed = 0.0;
                            first.speed_locked = Some(true);
                            first.eta = Some(dcso3::Time(0.));
                            first.eta_locked = Some(true);
                            info!(
                                "[GROUND_START] {} ground-starting from {cat:?} id {:?} at ({:.0},{:.0}) elev {:.0}, {} assigned parking spot(s)",
                                group.name,
                                abid,
                                anchor.x,
                                anchor.y,
                                elev,
                                parking_plan.len()
                            );
                        }
                        _ if helicopter => {
                            // Most FOBs have no DCS airbase or FARP pad at all,
                            // so there is nothing to park at -- but a helicopter
                            // does not need one. "From Ground Area Hot" starts
                            // it on the open ground where the campaign put it,
                            // rotors turning, which is the whole reason a helo
                            // mission can launch from a field a jet could not.
                            let alt = dcso3::land::Land::singleton(spctx.lua())
                                .and_then(|l| l.get_height(first.pos))
                                .unwrap_or(0.0);
                            first.typ = dcso3::controller::PointType::TakeOffGroundHot;
                            first.action = Some(dcso3::controller::ActionTyp::Air(
                                dcso3::controller::TurnMethod::FromGroundAreaHot,
                            ));
                            first.airdrome_id = None;
                            first.helipad = None;
                            first.link_unit = None;
                            first.alt = alt;
                            first.alt_typ = Some(dcso3::controller::AltType::BARO);
                            first.speed = 0.0;
                            first.speed_locked = Some(true);
                            first.eta = Some(dcso3::Time(0.));
                            first.eta_locked = Some(true);
                            ground_start = true;
                            info!(
                                "[GROUND_START] {} has no resolvable airbase for {:?} -- starting from open ground at ({:.0},{:.0}) elev {alt:.0}",
                                group.name, group.origin, first.pos.x, first.pos.y
                            );
                        }
                        _ => warn!(
                            "[GROUND_START] {} has no resolvable airbase for {:?} -- leaving template waypoint 0, so this flight will air-start (a CAP is then scrapped by enforce_cap_ground_start)",
                            group.name, group.origin
                        ),
                    }

                    if group.tags.contains(UnitTag::CAP) {
                        let opts = vec![
                            dcso3::controller::Task::WrappedOption(dcso3::controller::AiOption::Air(dcso3::controller::AirOption::Roe(dcso3::controller::AirRoe::WeaponFree))),
                            dcso3::controller::Task::WrappedOption(dcso3::controller::AiOption::Air(dcso3::controller::AirOption::RadarUsing(dcso3::controller::AirRadarUsing::ForContinuousSearch))),
                            dcso3::controller::Task::WrappedOption(dcso3::controller::AiOption::Air(dcso3::controller::AirOption::ReactionOnThreat(dcso3::controller::AirReactionToThreat::EvadeFire))),
                            dcso3::controller::Task::WrappedOption(dcso3::controller::AiOption::Air(dcso3::controller::AirOption::Silence(false))),
                            *first.task.clone()
                        ];
                        first.task = Box::new(dcso3::controller::Task::ComboTask(opts));
                    }
                }

                if points.is_empty() {
                    // No waypoint 0 to turn into a parking start, so the rest
                    // of the group table must not claim one either -- units
                    // pinned to parking spots under a route that never says
                    // "take off from parking" spawns worse than the plain
                    // template does.
                    parking_plan.clear();
                    parking_fallback = None;
                    parking_anchor = None;
                    ground_start = false;
                }

                if parking_anchor.is_some() || ground_start {
                    // An `uncontrolled` flight parks and never starts its
                    // engines, and a non-zero `start_time` makes DCS hold the
                    // group as a scheduled spawn instead of putting it on the
                    // ramp now.
                    template.group.raw_set("uncontrolled", false)?;
                    template.group.raw_set("start_time", 0)?;
                }
            }

            if parking_anchor.is_none() {
                // Waypoint 0 only round-trips its field binding now that the
                // `airdromeId` key is spelled the way DCS spells it. Everywhere
                // we are NOT deliberately building a parking start, drop the
                // template's own binding the way the old misspelling did:
                // bflib places these groups where the campaign says, and a
                // stale airdromeId would drag the spawn back to whatever field
                // the mission editor parked the template at.
                if let Some(first) = points.first_mut() {
                    first.airdrome_id = None;
                    first.helipad = None;
                }
            }
            if points.len() > 0 {
                route.set_points(points).context("setting points")?;
            }
            if parking_anchor.is_some() {
                // `formation_template` is part of every waypoint DCS writes and
                // it has no MissionPoint field, so stamp it on the raw table.
                // An absent one is not fatal on its own, but a parking start is
                // exactly where DCS is fussiest about a well-formed waypoint.
                if let Ok(pts) = route.raw_get::<_, LuaTable>("points") {
                    if let Ok(wp0) = pts.raw_get::<_, LuaTable>(1) {
                        let _ = wp0.raw_set("formation_template", "");
                    }
                }
            }
            let by_tname: FxHashMap<&str, &SpawnedUnit> = alive_units
                .iter()
                .map(|u| (u.template_name.as_str(), *u))
                .collect();
            let alive = {
                let units = template.group.units().context("getting units")?;
                let mut i = 1;
                while i as usize <= units.len() {
                    let unit = units.get(i)?;
                    match by_tname.get(unit.name()?.as_str()) {
                        None => units.remove(i)?,
                        Some(su) => {
                            if su.tags.contains(UnitTag::AWACS) {
                                let stn = String::from(format_compact!("{:005o}", self.awacs_stn));
                                if let Ok(props) = unit.raw_get::<_, LuaTable>("AddPropAircraft") {
                                    self.awacs_stn -= 1;
                                    props.raw_set("STN_L16", stn)?;
                                }
                            }
                            unit.raw_remove("unitId")?;
                            // A parking start is a property of the whole group
                            // table, not just waypoint 0: DCS wants each unit
                            // physically on its spot, at field elevation BARO,
                            // with `parking` naming the spot. Units left at
                            // their persisted position out over the objective
                            // are what makes DCS throw the ground start away
                            // and spawn the flight in the air.
                            match parking_plan.get(i as usize - 1) {
                                Some(spot) => {
                                    unit.set_pos(Vector2::new(spot.pos.0.x, spot.pos.0.z))?;
                                    unit.set_alt(spot.pos.0.y)?;
                                    unit.set_alt_type("BARO")?;
                                    unit.set_parking(spot.term_index)?;
                                }
                                None => match parking_fallback {
                                    Some((pos, elev)) => {
                                        // Ground start with no spot of our own
                                        // -- sit on the field and let DCS place
                                        // us. Any parking id inherited from the
                                        // template points at another airfield.
                                        unit.clear_parking()?;
                                        unit.set_pos(pos)?;
                                        unit.set_alt(elev)?;
                                        unit.set_alt_type("BARO")?;
                                    }
                                    None => {
                                        unit.set_pos(su.pos)?;
                                        unit.set_alt(su.position.p.y)?;
                                    }
                                },
                            }
                            unit.set_heading(su.heading)?;
                            unit.set_name(su.name.clone())?;
                            i += 1;
                        }
                    }
                }
                units.len() > 0
            };
            (template, alive)
        };
        if !alive {
            record_perf(&mut perf.spawn, ts);
            Ok(None)
        } else {
            // For a parking start the group's own x/y has to be the lead
            // unit's spot. The centroid of the persisted unit positions is
            // wherever the flight was notionally created (out over the
            // objective) and disagrees with waypoint 0, which DCS resolves by
            // air-starting.
            let point = match parking_anchor {
                Some(anchor) => anchor,
                None => centroid2d(points.iter().map(|p| *p)),
            };
            template.group.set_pos(point)?;
            /*
            let radius = points
                .iter()
                .map(|p: &Vector2| na::distance_squared(&(*p).into(), &point.into()))
                .fold(0., |acc, d| if d > acc { d } else { acc });
            let radius = radius.sqrt();
            spctx.remove_junk(point, radius * 1.10).with_context(|| {
                format_compact!("removing junk before spawn of {}", group.template_name)
            })?;
            */
            // Check if this group should be linked to a carrier
            // First, re-establish carrier link for crates that originated from carrier objectives
            // (this link is lost on save/load since carrier_linked_groups is ephemeral)
            if !self.carrier_linked_groups.contains_key(&group.id) {
                if let DeployKind::Crate { origin, .. } = &group.origin {
                    // Check if the origin objective is a carrier group
                    if let Some(obj) = persisted.objectives.get(origin) {
                        if let ObjectiveKind::CarrierGroup { carrier_template, .. } = &obj.kind {
                            if !carrier_template.is_empty() {
                                // carrier_template is the GROUP template name (e.g., "RCARRIER").
                                // Unit template_names are mission editor names (e.g., "Kurznetsov"),
                                // which don't necessarily start with the group name.
                                // Match by GROUP membership instead: find groups matching carrier_template,
                                // then look at their units.
                                'carrier_search: for (_, cg) in &persisted.groups {
                                    if cg.template_name.starts_with(carrier_template.as_str()) && cg.side == group.side {
                                        for uid in cg.units.into_iter() {
                                            if let Some(cu) = persisted.units.get(uid) {
                                                if !cu.dead {
                                                    if Unit::get_by_name(spctx.lua(), &cu.template_name).is_ok() {
                                                        info!("[CARRIER_LINK] Re-establishing link for crate {:?} to carrier '{}'",
                                                              group.id, cu.template_name);
                                                        self.carrier_linked_groups.insert(group.id, cu.template_name.clone());
                                                        break 'carrier_search;
                                                    }
                                                    if Unit::get_by_name(spctx.lua(), &cu.name).is_ok() {
                                                        info!("[CARRIER_LINK] Re-establishing link for crate {:?} to carrier '{}' (bflib name)",
                                                              group.id, cu.name);
                                                        self.carrier_linked_groups.insert(group.id, cu.name.clone());
                                                        break 'carrier_search;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // Crates spawn from one shared template per side (crate_template /
            // c130_cargo_template / helo_cargo_template); per-crate weight
            // override is applied to the cloned template's single unit here,
            // right before spawn. Both the C-130 and helicopters load crates
            // into an internal cargo bay (not a DCS sling-load hook), so the
            // weight override applies uniformly to all crate templates.
            //
            // The `dcs_type` reskin is C-130-only: helo_cargo_template
            // (RCRATE_HELO/BCRATE_HELO) already IS the correct helicopter
            // crate model per side -- reskinning it to a crate's configured
            // dcs_type (meant for the C-130 pallet model) replaced that model
            // with one whose baked-in default mass ignored our weight
            // override, which is why helo crates always showed the same
            // weight regardless of config.
            if let DeployKind::Crate { spec, .. } = &group.origin {
                let is_helo_template = self
                    .cfg
                    .helo_cargo_template
                    .values()
                    .any(|t| t.as_str() == group.template_name.as_str());
                match template.group.units() {
                    Ok(units) => match units.get(1) {
                        Ok(unit) => {
                            if !is_helo_template {
                                if let Some(dcs_type) = spec.dcs_type.clone() {
                                    unit.set_typ(dcs_type)?;
                                }
                            }
                            unit.set_mass(spec.weight)?;
                            info!(
                                "[CRATE_MASS] set mass={}kg on template {} (unit type after override: {:?}) for crate {}",
                                spec.weight,
                                group.template_name,
                                unit.typ(),
                                spec.name
                            );
                        }
                        Err(e) => warn!(
                            "[CRATE_MASS] template {} has no unit at index 1, cannot set mass={}kg: {e:?}",
                            group.template_name, spec.weight
                        ),
                    },
                    Err(e) => warn!(
                        "[CRATE_MASS] template {} has no units() table, cannot set mass={}kg: {e:?}",
                        group.template_name, spec.weight
                    ),
                }
            }
            let carrier_link_id = self.carrier_linked_groups.remove(&group.id);
            let spawned = spctx
                .spawn_with_link(template, carrier_link_id)
                .with_context(|| format_compact!("spawning template {}", group.template_name))?;
            match &spawned {
                Spawned::Static(oid) => {
                    self.object_id_by_gid.insert(group.id, oid.clone());
                    self.gid_by_object_id.insert(oid.clone(), group.id);
                }
                Spawned::Group(g) => {
                    let oid = g.object_id()?.erased();
                    self.object_id_by_gid.insert(group.id, oid.clone());
                    self.gid_by_object_id.insert(oid, group.id);
                    let gci = match &group.origin {
                        DeployKind::Deployed { spec, .. } => spec.gci.as_ref(),
                        _ => None,
                    };
                    if let Some(gci) = gci {
                        if let Some(unit) = g.get_units()?.into_iter().filter_map(|u| u.ok()).next() {
                            let ll = dcso3::coord::Coord::singleton(spctx.lua())?
                                .lo_to_ll(unit.get_point()?)?;
                            let controller = unit.get_controller()
                                .context("getting GCI unit controller")?;
                            controller
                                .set_command(dcso3::controller::Command::ActivateGci {
                                    unit: unit.id()?,
                                    latitude: ll.latitude,
                                    longitude: ll.longitude,
                                    channel: gci.channel,
                                    radius: gci.radius,
                                })
                                .context("activating GCI station")?;
                        }
                    }
                    // Player-deployed air defence comes up hot. DCS spawns a
                    // ground group in AlarmState::Auto, in which a lone
                    // launcher / short-range SAM with no external cueing often
                    // never powers its radar on -- the player unpacks a SAM
                    // and it just sits there dark. Force AlarmState::Red so it
                    // searches and engages immediately; IADN EMCON explicitly
                    // skips player-deployed groups (see ewr.rs) so this isn't
                    // overridden on the next tick. Best effort -- never abort
                    // a spawn.
                    if let DeployKind::Deployed { .. } = &group.origin {
                        if group.tags.contains(UnitTag::SAM) || group.tags.contains(UnitTag::AAA) {
                            if let Ok(con) = g.get_controller() {
                                if let Err(e) = con.set_option(dcso3::controller::AiOption::Ground(
                                    dcso3::controller::GroundOption::AlarmState(
                                        dcso3::controller::AlarmState::Red,
                                    ),
                                )) {
                                    warn!(
                                        "failed to set AlarmState::Red on deployed AD group {}: {e:?}",
                                        group.name
                                    );
                                }
                            }
                        }
                    }
                    // (Re)light this objective's auto-navaid if this group is its
                    // designated beacon host. Best effort -- never abort a spawn.
                    if self.cfg.navaids.enabled {
                        if let DeployKind::Objective { origin } = &group.origin {
                            if let Some(navs) = persisted.navaids.get(origin) {
                                for nav in navs {
                                    if nav.host_gid == Some(group.id) {
                                        if let Err(e) = crate::navaids::activate_on_group(g, nav) {
                                            warn!(
                                                "navaid activation failed for objective {origin:?}: {e:?}"
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            record_perf(&mut perf.spawn, ts);
            Ok(Some(spawned))
        }
    }
}
