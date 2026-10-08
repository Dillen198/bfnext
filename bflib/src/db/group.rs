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
    cargo::C130CargoState,
    ephemeral::{LaunchRequest, QueuedDrive, SlotInfo},
    objective::ObjGroupClass,
    player::SlotAuth,
    Db, SetS,
};
use crate::{
    group, group_health, group_mut,
    spawnctx::{Despawn, SpawnCtx, SpawnLoc},
    unit, unit_mut, Connected,
};
use anyhow::{anyhow, bail, Context, Result};
use bfprotocols::{
    cfg::{Action, ActionKind, Crate, Deployable, LifeType, SpecialSamUnitCfg, Troop, UnitTag, UnitTags, Vehicle},
    db::objective::{ObjectiveId, ObjectiveKind},
    stats::{self, EnId},
};
use bfprotocols::{
    db::group::{GroupId, UnitId},
    stats::Stat,
};
use chrono::prelude::*;
use compact_str::{format_compact, CompactString};
use dcso3::{
    azumith3d, centroid2d, change_heading,
    coalition::{Side, Static},
    coord::Coord,
    country::Country,
    env::miz,
    env::miz::{Group, GroupKind, MizIndex},
    group::GroupCategory,
    land::{Land, RoadType, SurfaceType},
    net::{SlotId, Ucid},
    object::{DcsObject, DcsOid},
    rotate2d_gen,
    static_object::{ClassStatic, StaticObject},
    trigger::MarkId,
    unit::{ClassUnit, Unit},
    LuaVec2, LuaVec3, MizLua, Position3, String, Vector2, Vector3,
};
use enumflags2::BitFlags;
use fxhash::{FxHashMap, FxHashSet};
use log::{error, info, warn};
use serde_derive::{Deserialize, Serialize};
use smallvec::{smallvec, SmallVec};
use std::{cmp::max, collections::VecDeque};

/// How far a deployed group has to travel before its F10 pin is redrawn, in
/// metres. See `mark_group_if_moved`: the pin costs two map commands out of a
/// budget of `max_msgs_per_second` (3 on the live config) shared with every
/// objective label and ring on the map, so redrawing it every second for every
/// vehicle under way is what actually froze the F10 picture.
const GROUP_MARK_MIN_MOVE: f64 = 500.;

/// A friendly field a flight takes off from, and the spot on it where the
/// flight starts (see `Db::launch_spot`).
#[derive(Debug, Clone)]
pub(crate) struct LaunchField {
    pub(crate) oid: ObjectiveId,
    pub(crate) name: String,
    pub(crate) pos: Vector2,
}

/// Can an aircraft of this kind operate out of an objective of this kind?
/// Helicopters fly from airbases, FARPs and FOBs; fixed wing only from
/// airbases, plus the fields the config lists in `extra_fixed_wing_objectives`.
/// Carrier decks are left out for both: the deck airbase moves with the ship.
pub(crate) fn is_launch_kind(kind: &ObjectiveKind, rotary: bool, extra_fixed_wing: bool) -> bool {
    if rotary {
        matches!(kind, ObjectiveKind::Airbase | ObjectiveKind::Farp { .. } | ObjectiveKind::Fob)
    } else {
        matches!(kind, ObjectiveKind::Airbase) || extra_fixed_wing
    }
}

/// `candidates` at least `min_dist` from `near`, nearest first.
pub(crate) fn rank_launch_fields<T: Copy>(
    candidates: impl IntoIterator<Item = (T, Vector2)>,
    near: Vector2,
    min_dist: f64,
) -> Vec<T> {
    let mut v: Vec<(T, f64)> = candidates
        .into_iter()
        .map(|(id, p)| (id, (p - near).norm()))
        .filter(|(_, d)| *d >= min_dist)
        .collect();
    v.sort_by(|a, b| a.1.total_cmp(&b.1));
    v.into_iter().map(|(id, _)| id).collect()
}

/// A planned ground drive: (point, on road) legs, point 0 being where the
/// group starts, and its length.
#[derive(Debug, Clone)]
pub(crate) struct DrivePlan {
    pub(crate) points: Vec<(Vector2, bool)>,
    pub(crate) route_m: f64,
    pub(crate) by_road: bool,
}

/// How far a road may start from the drive's origin, or end from its
/// destination, and still count as serving it. The gap is driven off road.
const ROAD_ACCESS_M: f64 = 3_000.;
/// DCS's road polylines run to thousands of vertices, which chokes the
/// ground AI; "On Road" follows the road between sparse points anyway.
const DRIVE_WAYPOINT_SPACING_M: f64 = 3_000.;
const MAX_DRIVE_WAYPOINTS: usize = 40;

fn drive_length(points: &[(Vector2, bool)]) -> f64 {
    points.windows(2).map(|w| (w[1].0 - w[0].0).norm()).sum()
}

/// The drive from `from` to `to` along `road` (a DCS road polyline between
/// them), or `None` if the road doesn't serve both ends. On road the whole
/// way, then off road for the last stretch: "On Road" stops at the road
/// point nearest the destination, which can be well short of it.
pub(crate) fn plan_road_drive(from: Vector2, to: Vector2, road: &[Vector2]) -> Option<DrivePlan> {
    let (first, last) = (road.first()?, road.last()?);
    if (first - from).norm() > ROAD_ACCESS_M || (last - to).norm() > ROAD_ACCESS_M {
        return None;
    }
    let mut points = vec![(from, true)];
    let mut kept = 0;
    let mut last_kept: Option<Vector2> = None;
    for (i, p) in road.iter().enumerate() {
        let far_enough = last_kept
            .map(|l| (p - l).norm() >= DRIVE_WAYPOINT_SPACING_M)
            .unwrap_or(true);
        if (far_enough || i + 1 == road.len()) && kept < MAX_DRIVE_WAYPOINTS {
            points.push((*p, true));
            last_kept = Some(*p);
            kept += 1;
        }
    }
    points.push((to, false));
    let route_m = drive_length(&points);
    Some(DrivePlan { points, route_m, by_road: true })
}

/// Straight across country from `from` to `to`.
pub(crate) fn plan_cross_country(from: Vector2, to: Vector2) -> DrivePlan {
    let points = vec![(from, false), (to, false)];
    DrivePlan { route_m: drive_length(&points), points, by_road: false }
}

#[derive(Debug, Clone)]
pub enum BirthRes {
    None,
    OccupiedSlot(SlotId),
    DynamicSlotDenied(Ucid, SlotAuth),
}

fn default_cost_fraction() -> f32 {
    1.
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum DeployKind {
    #[serde(rename = "Objective")]
    ObjectiveDeprecated,
    #[serde(rename = "ObjectiveV2")]
    Objective {
        origin: ObjectiveId,
    },
    Deployed {
        player: Ucid,
        #[serde(default)]
        moved_by: Option<(Ucid, u32)>,
        spec: Deployable,
        #[serde(default = "default_cost_fraction")]
        cost_fraction: f32,
        #[serde(default)]
        origin: Option<ObjectiveId>,
        #[serde(default)]
        jtac: Option<bfprotocols::cfg::JtacState>,
    },
    Troop {
        player: Ucid,
        origin: Option<ObjectiveId>,
        #[serde(default)]
        moved_by: Option<(Ucid, u32)>,
        spec: Troop,
        #[serde(default = "default_cost_fraction")]
        cost_fraction: f32,
        #[serde(default)]
        jtac: Option<bfprotocols::cfg::JtacState>,
    },
    DownedPilot {
        ucid: Ucid,
        name: String,
        life_type: LifeType,
    },
    Crate {
        origin: ObjectiveId,
        player: Ucid,
        spec: Crate,
    },
    Action {
        #[serde(skip)]
        marks: FxHashSet<MarkId>,
        loc: SpawnLoc,
        player: Option<Ucid>,
        name: String,
        spec: Action,
        time: DateTime<Utc>,
        destination: Option<Vector2>,
        rtb: Option<Vector2>,
        #[serde(default)]
        origin: Option<ObjectiveId>,
        #[serde(skip)]
        ammo: i32,
        #[serde(default)]
        jtac: Option<bfprotocols::cfg::JtacState>,
        /// Who paid for this group and how many points. `player` is the
        /// responsible party and can change hands; refunds go here. None for
        /// saves that predate it and for groups nobody paid for.
        #[serde(default)]
        paid_by: Option<(Ucid, u32)>,
        /// What a transport action carries -- for Reinforce, the number of
        /// groups on the trailers (the materiel drawn for them is returned
        /// if the convoy has to be recalled).
        #[serde(default)]
        carried: u32,
    },
    /// Infantry that bailed out of a destroyed vehicle
    Dismount {
        from_group: GroupId,
        can_capture: bool,
    },
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SpawnedUnit {
    pub name: String,
    pub id: UnitId,
    pub group: GroupId,
    pub side: Side,
    pub typ: Vehicle,
    pub tags: UnitTags,
    pub template_name: String,
    pub spawn_pos: Vector2,
    pub spawn_heading: f64,
    pub spawn_position: Position3,
    pub pos: Vector2,
    pub heading: f64,
    pub position: Position3,
    pub dead: bool,
    #[serde(skip)]
    pub moved: Option<DateTime<Utc>>,
    #[serde(skip)]
    pub airborne_velocity: Option<Vector3>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnedGroup {
    pub id: GroupId,
    pub name: String,
    pub template_name: String,
    pub side: Side,
    pub kind: Option<GroupCategory>,
    pub class: ObjGroupClass,
    pub origin: DeployKind,
    pub units: SetS<UnitId>,
    pub tags: UnitTags,
}

impl Db {
    pub fn group(&self, id: &GroupId) -> Result<&SpawnedGroup> {
        group!(self, id)
    }

    pub fn group_center(&self, id: &GroupId) -> Result<Vector2> {
        let group = group!(self, id)?;
        Ok(centroid2d(
            group
                .units
                .into_iter()
                .filter_map(|uid| self.persisted.units.get(uid))
                .filter_map(|unit| if unit.dead { None } else { Some(unit.pos) }),
        ))
    }

    pub fn unit(&self, id: &UnitId) -> Result<&SpawnedUnit> {
        unit!(self, id)
    }

    pub fn first_living_unit(&self, gid: &GroupId) -> Result<&DcsOid<ClassUnit>> {
        group!(self, gid)?
            .units
            .into_iter()
            .find_map(|uid| self.ephemeral.get_object_id_by_uid(uid))
            .ok_or_else(|| anyhow!("all units are dead"))
    }

    pub fn instanced_units(
        &self,
    ) -> impl Iterator<Item = (&SpawnedUnit, &DcsOid<ClassUnit>)> {
        self.persisted.units.into_iter().filter_map(|(uid, sp)| {
            self.ephemeral.object_id_by_uid.get(uid).map(|id| (sp, id))
        })
    }

    pub fn deployed(&self) -> impl Iterator<Item = &SpawnedGroup> {
        self.persisted
            .deployed
            .into_iter()
            .chain(self.persisted.troops.into_iter())
            .filter_map(|gid| self.persisted.groups.get(gid))
    }

    pub fn actions(&self) -> impl Iterator<Item = &SpawnedGroup> {
        self.persisted
            .actions
            .into_iter()
            .chain(self.persisted.troops.into_iter())
            .filter_map(|gid| self.persisted.groups.get(gid))
    }

    /// Re-pin a group only if it has actually gone somewhere -- at least
    /// `min_move` metres from where its current pin sits.
    ///
    /// `update_unit_positions` calls this for every group with a unit that
    /// shifted more than a metre since the last sample, which for anything
    /// under way is every single pass. Each re-pin is a delete plus a draw in
    /// the priority-1 queue, which drains ahead of all objective markup, so a
    /// handful of moving convoys was enough to eat the whole per-second budget
    /// and leave the F10 map's labels, rings and supply arrows permanently
    /// stale. A pin that is up to `min_move` metres behind the group is worth
    /// far more than one that is up to date and starves the rest of the map.
    pub(super) fn mark_group_if_moved(&mut self, gid: &GroupId, min_move: f64) -> Result<()> {
        if min_move > 0.
            && let Some((_, at)) = self.ephemeral.group_marks.get(gid)
        {
            let at = *at;
            let group = group!(self, gid)?;
            let center = centroid2d(
                group
                    .units
                    .into_iter()
                    .filter_map(|uid| self.persisted.units.get(uid).map(|u| u.pos)),
            );
            if (center - at).norm() < min_move {
                return Ok(());
            }
        }
        self.mark_group(gid)
    }

    pub(super) fn mark_group(&mut self, gid: &GroupId) -> Result<()> {
        if let Some((id, _)) = self.ephemeral.group_marks.remove(gid) {
            self.ephemeral.msgs.delete_mark(id)
        }
        // Lookups by `get`, never by indexing: a unit or player record that has
        // gone missing must cost this one pin, not panic the tick that asked
        // for it (and with it every other system that tick was running).
        let units = &self.persisted.units;
        let players = &self.persisted.players;
        let pname = |ucid: &Ucid| {
            players
                .get(ucid)
                .map(|p| p.name.clone())
                .unwrap_or_else(|| String::from("unknown"))
        };
        let group = group_mut!(self, gid)?;
        let group_center = centroid2d(
            group
                .units
                .into_iter()
                .filter_map(|uid| units.get(uid).map(|u| u.pos)),
        );
        let id = match &mut group.origin {
            DeployKind::ObjectiveDeprecated => None,
            // Garrison groups (the pre-placed SAM / AAA / armour / logi at every
            // objective) used to get a floating "objective group id N name <miz
            // group name> of class Sr" label on the F10 map for the owning side.
            // That's one debug-grade label per group at every base -- pure
            // clutter (and a contributor to F10 lag), so don't draw it.
            DeployKind::Objective { .. } => None,
            DeployKind::Action { name, spec: _, destination, player, marks, .. } => {
                let pname = player
                    .as_ref()
                    .map(|p| pname(p))
                    .unwrap_or(String::from("Server"));
                let pos_msg = format_compact!("{name} {gid} deployed by {pname}");
                let pos_mark = self.ephemeral.msgs.mark_to_side(
                    group.side,
                    group_center,
                    true,
                    pos_msg,
                );
                match destination {
                    None => Some(pos_mark),
                    Some(dst) => {
                        if !marks.is_empty() {
                            Some(pos_mark)
                        } else {
                            let dst_msg = format_compact!("{name} {gid} destination");
                            marks.insert(
                                self.ephemeral
                                    .msgs
                                    .mark_to_side(group.side, *dst, true, dst_msg),
                            );
                            Some(pos_mark)
                        }
                    }
                }
            }
            DeployKind::Crate { player, spec, .. } => {
                let name = pname(player);
                let msg = format_compact!("{} {gid} deployed by {name}", spec.name);
                Some(self.ephemeral.msgs.mark_to_side(
                    group.side,
                    group_center,
                    true,
                    msg,
                ))
            }
            DeployKind::Deployed {
                spec,
                player,
                moved_by,
                cost_fraction: _,
                origin: _,
                jtac: _,
            } => {
                let name = pname(player);
                let resp = moved_by
                    .as_ref()
                    .map(|(u, _)| {
                        let name = pname(u);
                        format_compact!("\nresponsible party: {name}")
                    })
                    .unwrap_or(CompactString::from(""));
                let msg = format_compact!(
                    "{} {gid} deployed by {name}{resp}",
                    spec.path.last().unwrap()
                );
                Some(self.ephemeral.msgs.mark_to_side(
                    group.side,
                    group_center,
                    true,
                    msg,
                ))
            }
            DeployKind::Troop { player, spec, moved_by, origin: _, cost_fraction: _, .. } => {
                let name = pname(player);
                let resp = moved_by
                    .as_ref()
                    .map(|(u, _)| {
                        let name = pname(u);
                        format_compact!("\nresponsible party: {name}")
                    })
                    .unwrap_or(CompactString::from(""));
                let msg = format_compact!("{} {gid} deployed by {name}{resp}", spec.name);
                Some(self.ephemeral.msgs.mark_to_side(
                    group.side,
                    group_center,
                    true,
                    msg,
                ))
            }
            DeployKind::DownedPilot { name, .. } => {
                let msg = format_compact!("downed pilot: {name}");
                Some(self.ephemeral.msgs.mark_to_side(
                    group.side,
                    group_center,
                    true,
                    msg,
                ))
            }
            DeployKind::Dismount { .. } => None,
        };
        if let Some(id) = id {
            self.ephemeral.group_marks.insert(*gid, (id, group_center));
        }
        Ok(())
    }

    pub fn delete_group(&mut self, gid: &GroupId) -> Result<()> {
        let group = self
            .persisted
            .groups
            .remove_cow(gid)
            .ok_or_else(|| anyhow!("no such group {:?}", gid))?;
        // Read before the units go: whether this deletion is a squad being
        // killed or one being removed with men still alive decides what
        // happens to a post-capture hold it belongs to (scrub_capture_hold).
        let killed = group
            .units
            .into_iter()
            .all(|uid| self.persisted.units.get(uid).map(|u| u.dead).unwrap_or(true));
        self.persisted.groups_by_name.remove_cow(&group.name);
        self.persisted.groups_by_side.get_mut_cow(&group.side).map(|m| m.remove_cow(gid));
        match &group.origin {
            DeployKind::ObjectiveDeprecated | DeployKind::Objective { .. } => (),
            DeployKind::Action { marks, .. } => {
                for id in marks {
                    self.ephemeral.msgs().delete_mark(*id);
                }
                self.persisted.actions.remove_cow(gid);
                self.persisted.jtacs.remove_cow(gid);
                self.persisted.ewrs.remove_cow(gid);
            }
            DeployKind::Crate { player, .. } => {
                self.persisted.crates.remove_cow(gid);
                if let Some(p) = self.persisted.players.get_mut_cow(player) {
                    p.crates.remove_cow(gid);
                }
                // Drop any dynamic-cargo (C-130 / helo) tracking entry for this
                // group too. Without this, unpacking or destroying a tracked
                // crate through any path other than unpack_c130_crate leaves a
                // zombie in c130_crates that update_c130_crates chases every
                // tick ("has no object_id in map, skipping") forever.
                if let Some(c) = self.ephemeral.c130_crates.remove(&group.name) {
                    // ... and take its "Missing: x (need 2, have 1)" map marker
                    // with it. Only unpack_c130_crate used to clear that, so a
                    // set completed the other way -- a helo flying in the last
                    // crate and unpacking the pile by hand -- left the stale
                    // shortfall marker sitting on the F10 map forever, telling
                    // players the delivery still hadn't worked.
                    if let Some(id) = c.missing_marker {
                        self.ephemeral.msgs().delete_mark(id);
                    }
                }
            }
            DeployKind::Deployed { spec, .. } => {
                self.persisted.deployed.remove_cow(gid);
                if spec.jtac.is_some() {
                    self.persisted.jtacs.remove_cow(gid);
                }
                if spec.ewr.is_some() {
                    self.persisted.ewrs.remove_cow(gid);
                }
            }
            DeployKind::Troop { spec, .. } => {
                self.persisted.troops.remove_cow(gid);
                if spec.jtac.is_some() {
                    self.persisted.jtacs.remove_cow(gid);
                }
            }
            DeployKind::DownedPilot { .. } => {
                self.persisted.downed_pilots.remove_cow(gid);
                self.persisted.downed_pilot_spawn_times.remove_cow(gid);
                self.ephemeral.csar_flared.remove(gid);
                self.ephemeral.csar_moving.remove(gid);
                self.ephemeral.csar_notified.remove(gid);
                self.ephemeral.csar_last_renotify.remove(gid);
                self.ephemeral.csar_smoke_cooldown.remove(gid);
            }
            DeployKind::Dismount { .. } => {
                self.persisted.dismounts.remove_cow(gid);
                self.persisted.dismount_spawned.remove_cow(gid);
            }
        }
        if matches!(
            group.origin,
            DeployKind::Troop { .. } | DeployKind::Dismount { .. }
        ) {
            self.scrub_capture_hold(gid, killed);
        }
        if let Some((id, _)) = self.ephemeral.group_marks.remove(gid) {
            self.ephemeral.msgs.delete_mark(id);
        }
        let mut units: SmallVec<[String; 16]> = smallvec![];
        for uid in &group.units {
            self.ephemeral.units_potentially_close_to_enemies.remove(uid);
            self.ephemeral.units_able_to_move.swap_remove(uid);
            if let Some(id) = self.ephemeral.object_id_by_uid.remove(uid) {
                self.ephemeral.uid_by_object_id.remove(&id);
            }
            if let Some(unit) = self.persisted.units.remove_cow(uid) {
                self.persisted.units_by_name.remove_cow(&unit.name);
                units.push(unit.name);
            }
        }
        self.ephemeral.dirty();
        match group.kind {
            None => {
                // it's a static. Prefer using the object_id if we have one (e.g. for
                // C-130 crates whose DCS name may differ from the bflib name after
                // cargo load/drop renames). Fall back to name-based lookup.
                if let Some(oid) = self.ephemeral.object_id_by_gid.get(gid) {
                    self.ephemeral
                        .push_despawn(*gid, Despawn::StaticObject(oid.clone()));
                } else {
                    for unit in &units {
                        self.ephemeral
                            .push_despawn(*gid, Despawn::Static(unit.clone()))
                    }
                }
            }
            Some(_) => {
                // it's a normal group
                if let Some(oid) = self.ephemeral.object_id_by_gid.get(gid) {
                    self.ephemeral.push_despawn(*gid, Despawn::Group(oid.clone()));
                } else {
                    // Object ID not yet tracked (e.g. group spawned moments ago and no DCS
                    // event has fired yet). Fall back to destroying by name so the DCS unit
                    // is actually removed from the world rather than silently left alive.
                    self.ephemeral.push_despawn(*gid, Despawn::GroupByName(group.name.to_string()));
                }
            }
        }
        self.ephemeral.stat(Stat::GroupDeleted { id: *gid });
        Ok(())
    }

    /// Live dismount squads belonging to `side`. The dismount cap is per side:
    /// counted over both, one side's wrecks used up the other's allowance.
    pub fn dismount_count(&self, side: Side) -> usize {
        self.persisted
            .dismounts
            .into_iter()
            .filter(|gid| self.persisted.groups.get(gid).map(|g| g.side == side).unwrap_or(false))
            .count()
    }

    /// Remove dismount squads older than `dismount_ttl_secs`, if that is set.
    ///
    /// Dismounts are a battlefield side effect, nobody's deployable: nothing
    /// ever picks them up or removes them, so on a long campaign they piled up
    /// for good -- still counted toward the per-type cap, still able to
    /// capture. A squad that is holding a freshly captured base is left alone
    /// until the hold resolves; expiring it there would end the hold.
    pub fn expire_dismounts(&mut self, now: DateTime<Utc>) {
        let ttl = match self.ephemeral.cfg.dismount_ttl_secs {
            Some(ttl) if ttl > 0 => chrono::Duration::seconds(ttl as i64),
            _ => return,
        };
        let mut expired: SmallVec<[GroupId; 8]> = smallvec![];
        let mut unstamped: SmallVec<[GroupId; 8]> = smallvec![];
        for gid in &self.persisted.dismounts {
            match self.persisted.dismount_spawned.get(gid) {
                // Squads from a save that predates the timestamp start their
                // clock now rather than all expiring at once.
                None => unstamped.push(*gid),
                Some(ts) if now - *ts >= ttl => expired.push(*gid),
                Some(_) => (),
            }
        }
        for gid in unstamped {
            self.persisted.dismount_spawned.insert_cow(gid, now);
            self.ephemeral.dirty();
        }
        expired.retain(|gid| {
            !self
                .persisted
                .objectives
                .into_iter()
                .any(|(_, o)| o.capture_hold.contains(gid))
        });
        if expired.is_empty() {
            return;
        }
        info!("expiring {} dismount squad(s) past their {}s lifetime", expired.len(), ttl.num_seconds());
        for gid in expired {
            if let Err(e) = self.delete_group(&gid) {
                warn!("could not expire dismount squad {gid}: {e:?}");
            }
        }
    }

    /// add the units to the db, but don't actually spawn them
    pub(super) fn add_group<'lua>(
        &mut self,
        spctx: &'lua SpawnCtx<'lua>,
        idx: &MizIndex,
        side: Side,
        location: SpawnLoc,
        template_name: &str,
        origin: DeployKind,
        extra_tags: BitFlags<UnitTag>,
    ) -> Result<GroupId> {
        fn distance<'a, F: Fn(f64, f64) -> f64>(
            pos: Vector2,
            cmp: F,
            positions: impl IntoIterator<Item = &'a Vector2>,
        ) -> f64 {
            positions
                .into_iter()
                .fold(None, |acc, p| {
                    let d = na::distance_squared(&(*p).into(), &pos.into());
                    let acc = match acc {
                        None => d,
                        Some(d) => d,
                    };
                    Some(cmp(acc, d))
                })
                .map(|d| d.sqrt())
                .unwrap_or(0.)
        }
        #[derive(Debug)]
        struct UnitPosition {
            heading: f64,
            position: Vector2,
            altitude: Option<f64>,
        }
        #[derive(Debug)]
        struct GroupPosition {
            positions: VecDeque<UnitPosition>,
            by_type: FxHashMap<String, VecDeque<UnitPosition>>,
        }
        fn compute_unit_positions(
            spctx: &SpawnCtx,
            idx: &MizIndex,
            location: SpawnLoc,
            template: &Group,
        ) -> Result<GroupPosition> {
            let mut positions = template
                .units()?
                .into_iter()
                .map(|u| {
                    let u = u?;
                    Ok(UnitPosition {
                        heading: u.heading()?,
                        position: u.pos()?,
                        altitude: u.alt().unwrap_or(None),
                    })
                })
                .collect::<Result<VecDeque<_>>>()?;
            match location {
                SpawnLoc::InAir { pos, heading, altitude, speed: _ } => {
                    let group_center = centroid2d(positions.iter().map(|p| p.position));
                    let group_altitude = {
                        let (sum, i) = positions
                            .iter()
                            .filter_map(|p| p.altitude)
                            .fold((0., 0.), |(sum, i), a| (sum + a, i + 1.));
                        sum / i
                    };
                    for p in positions.iter_mut() {
                        p.position = p.position - group_center + pos;
                        p.heading = change_heading(p.heading, heading);
                        if let Some(a) = p.altitude {
                            p.altitude = Some(a - group_altitude + altitude);
                        }
                    }
                    rotate2d_gen(heading, positions.make_contiguous(), |p| {
                        &mut p.position
                    });
                    Ok(GroupPosition { positions, by_type: FxHashMap::default() })
                }
                SpawnLoc::AtPosWithCenter { pos, center } => {
                    for p in positions.iter_mut() {
                        p.position = p.position - center + pos;
                        p.altitude = None;
                    }
                    Ok(GroupPosition { positions, by_type: FxHashMap::default() })
                }
                SpawnLoc::AtTrigger { name, group_heading } => {
                    let group_center = centroid2d(positions.iter().map(|p| p.position));
                    let pos = spctx.get_trigger_zone(idx, name.as_str())?.pos()?;
                    for p in positions.iter_mut() {
                        p.position = p.position - group_center + pos;
                        p.heading = change_heading(p.heading, group_heading);
                        p.altitude = None;
                    }
                    rotate2d_gen(group_heading, positions.make_contiguous(), |p| {
                        &mut p.position
                    });
                    Ok(GroupPosition { positions, by_type: FxHashMap::default() })
                }
                SpawnLoc::AtPos { pos, offset_direction, group_heading } => {
                    let group_center = centroid2d(positions.iter().map(|p| p.position));
                    let radius = distance(
                        group_center,
                        f64::max,
                        positions.iter().map(|p| &p.position),
                    );
                    for p in positions.iter_mut() {
                        p.position =
                            p.position - group_center + pos + radius * offset_direction;
                    }
                    rotate2d_gen(group_heading, positions.make_contiguous(), |p| {
                        &mut p.position
                    });
                    let offset_magnitude = 20.
                        - distance(pos, f64::min, positions.iter().map(|p| &p.position));
                    for p in positions.iter_mut() {
                        p.position = p.position + offset_magnitude * offset_direction;
                        p.heading = change_heading(p.heading, group_heading);
                        p.altitude = None;
                    }
                    Ok(GroupPosition { positions, by_type: FxHashMap::default() })
                }
                SpawnLoc::AtPosExact { pos, group_heading } => {
                    let group_center = centroid2d(positions.iter().map(|p| p.position));
                    for p in positions.iter_mut() {
                        p.position = p.position - group_center + pos;
                        p.heading = change_heading(p.heading, group_heading);
                        p.altitude = None;
                    }
                    rotate2d_gen(group_heading, positions.make_contiguous(), |p| {
                        &mut p.position
                    });
                    Ok(GroupPosition { positions, by_type: FxHashMap::default() })
                }
                SpawnLoc::AtPosWithComponents { pos, group_heading, component_pos } => {
                    let group_center = centroid2d(positions.iter().map(|p| p.position));
                    let center_by_typ: FxHashMap<String, Vector2> = {
                        let mut tbl = FxHashMap::default();
                        for unit in template.units()? {
                            let unit = unit?;
                            let pos = unit.pos()?;
                            let typ = unit.typ()?;
                            if component_pos.contains_key(&**typ) {
                                let (n, v) = tbl
                                    .entry(typ.clone())
                                    .or_insert_with(|| (0, Vector2::new(0., 0.)));
                                *v += pos;
                                *n += 1;
                            }
                        }
                        tbl.into_iter().map(|(k, (n, v))| (k, v / (n as f64))).collect()
                    };
                    let mut by_type: FxHashMap<String, VecDeque<UnitPosition>> =
                        FxHashMap::default();
                    positions.clear();
                    for unit in template.units()? {
                        let unit = unit?;
                        let typ = unit.typ()?;
                        let heading = unit.heading()?;
                        let position = unit.pos()?;
                        let group_center = match center_by_typ.get(&typ) {
                            None => group_center,
                            Some(pos) => *pos,
                        };
                        match component_pos.get(&typ) {
                            None => positions.push_back(UnitPosition {
                                position: position - group_center + pos,
                                heading: change_heading(heading, group_heading),
                                altitude: None,
                            }),
                            Some(pos) => by_type
                                .entry(typ.clone())
                                .or_default()
                                .push_back(UnitPosition {
                                    position: position - group_center + *pos,
                                    heading: change_heading(heading, group_heading),
                                    altitude: None,
                                }),
                        }
                    }
                    rotate2d_gen(group_heading, positions.make_contiguous(), |p| {
                        &mut p.position
                    });
                    for positions in by_type.values_mut() {
                        rotate2d_gen(group_heading, positions.make_contiguous(), |p| {
                            &mut p.position
                        });
                    }
                    Ok(GroupPosition { positions, by_type })
                }
            }
        }
        fn check_water(
            land: &Land,
            positions: &VecDeque<UnitPosition>,
            positions_by_typ: &FxHashMap<String, VecDeque<UnitPosition>>,
        ) -> Result<()> {
            for pos in
                positions.iter().chain(positions_by_typ.values().flat_map(|v| v.iter()))
            {
                match land.get_surface_type(LuaVec2(pos.position))? {
                    SurfaceType::Land | SurfaceType::Road | SurfaceType::Runway => (),
                    SurfaceType::ShallowWater | SurfaceType::Water => {
                        bail!("you can't spawn this unit in water")
                    }
                }
            }
            Ok(())
        }
        fn check_land(
            land: &Land,
            positions: &VecDeque<UnitPosition>,
            positions_by_typ: &FxHashMap<String, VecDeque<UnitPosition>>,
        ) -> Result<()> {
            for pos in
                positions.iter().chain(positions_by_typ.values().flat_map(|v| v.iter()))
            {
                match land.get_surface_type(LuaVec2(pos.position))? {
                    SurfaceType::ShallowWater | SurfaceType::Water => (),
                    SurfaceType::Land | SurfaceType::Road | SurfaceType::Runway => {
                        bail!("you can't spawn this unit on land")
                    }
                }
            }
            Ok(())
        }
        let land = Land::singleton(spctx.lua())?;
        let template_name = String::from(template_name);
        let template =
            spctx.get_template_ref(idx, GroupKind::Any, side, template_name.as_str())?;
        let mut gpos =
            compute_unit_positions(&spctx, idx, location.clone(), &template.group)?;
        let kind = GroupCategory::from_kind(template.category);
        let gid = GroupId::new();
        // naval spawn points need to be pre created in the miz, so they must be
        // spawned with the same name as the pre created group so that they move
        // to their destination.
        let group_name = if extra_tags.contains(UnitTag::NavalSpawnPoint) {
            template_name.clone()
        } else {
            String::from(format_compact!("{}-{}", template_name, gid))
        };
        let mut spawned = SpawnedGroup {
            id: gid,
            name: group_name.clone(),
            template_name: template_name.clone(),
            side,
            kind,
            origin,
            class: if extra_tags.contains(UnitTag::NavalSpawnPoint) {
                ObjGroupClass::Logi
            } else {
                ObjGroupClass::from(template_name.as_str())
            },
            units: SetS::new(),
            tags: UnitTags(BitFlags::empty()),
        };
        for unit in template.group.units()?.into_iter() {
            let unit = unit?;
            let typ = unit.typ()?;
            let tags = *self
                .ephemeral
                .cfg
                .unit_classification
                .get(typ.as_str())
                .ok_or_else(|| anyhow!("unit type not classified {typ}"))?;
            let tags = UnitTags(tags.0 | extra_tags);
            spawned.tags.0.insert(tags.0);
        }
        match &location {
            SpawnLoc::AtPos { .. }
            | SpawnLoc::AtPosExact { .. }
            | SpawnLoc::AtPosWithCenter { .. }
            | SpawnLoc::AtPosWithComponents { .. }
            | SpawnLoc::AtTrigger { .. } => {
                let is_crate_template = [
                    self.ephemeral.cfg.crate_template.get(&side),
                    self.ephemeral.cfg.c130_cargo_template.get(&side),
                    self.ephemeral.cfg.helo_cargo_template.get(&side),
                ]
                .into_iter()
                .flatten()
                .any(|tmpl| tmpl == &template_name);
                if is_crate_template {
                    () // it's ok to spawn crates on ships
                } else if spawned.tags.contains(UnitTag::Boat) {
                    check_land(&land, &gpos.positions, &gpos.by_type)
                        .with_context(|| format_compact!("placing group {group_name}"))?
                } else {
                    check_water(&land, &gpos.positions, &gpos.by_type)
                        .with_context(|| format_compact!("placing group {group_name}"))?
                }
            }
            SpawnLoc::InAir { .. } => (),
        }
        for unit in template.group.units()?.into_iter() {
            let uid = UnitId::new();
            let unit = unit?;
            let typ = unit.typ()?;
            let tags = *self
                .ephemeral
                .cfg
                .unit_classification
                .get(typ.as_str())
                .ok_or_else(|| anyhow!("unit type not classified {typ}"))?;
            let tags = UnitTags(tags.0 | extra_tags);
            let template_name = unit.name()?;
            let unit_name = if extra_tags.contains(UnitTag::NavalSpawnPoint) {
                template_name.clone()
            } else {
                String::from(format_compact!("{}-{}", group_name, uid))
            };
            let pos = match gpos.by_type.get_mut(&typ) {
                None => gpos.positions.pop_front().unwrap(),
                Some(positions) => positions.pop_front().unwrap(),
            };
            let position = {
                let mut p = Position3::default();
                p.p.x = pos.position.x;
                p.p.y = match pos.altitude {
                    None => land.get_height(LuaVec2(pos.position))?,
                    Some(alt) => alt,
                };
                p.p.z = pos.position.y;
                p
            };
            let spawned_unit = SpawnedUnit {
                id: uid,
                group: gid,
                side,
                typ: Vehicle(typ),
                tags,
                name: unit_name.clone(),
                template_name,
                spawn_position: position,
                spawn_pos: pos.position,
                spawn_heading: pos.heading,
                position,
                pos: pos.position,
                heading: pos.heading,
                dead: false,
                moved: None,
                airborne_velocity: None,
            };
            spawned.units.insert_cow(uid);
            self.persisted.units.insert_cow(uid, spawned_unit);
            self.persisted.units_by_name.insert_cow(unit_name, uid);
        }
        match &mut spawned.origin {
            DeployKind::ObjectiveDeprecated | DeployKind::Objective { .. } => (),
            DeployKind::Action { spec, .. } => {
                self.persisted.actions.insert_cow(gid);
                match &spec.kind {
                    ActionKind::Drone(_) => {
                        self.persisted.jtacs.insert_cow(gid);
                    }
                    ActionKind::Awacs(_) => {
                        self.persisted.ewrs.insert_cow(gid);
                    }
                    _ => (),
                }
            }
            DeployKind::Crate { player, .. } => {
                self.persisted.crates.insert_cow(gid);
                match self.persisted.players.get_mut_cow(&*player) {
                    Some(p) => {
                        p.crates.insert_cow(gid);
                    }
                    None => warn!("crate {gid} spawned for unknown player {player:?}"),
                }
            }
            DeployKind::Deployed { spec, .. } => {
                self.persisted.deployed.insert_cow(gid);
                if spec.jtac.is_some() {
                    self.persisted.jtacs.insert_cow(gid);
                }
                if spec.ewr.is_some() {
                    self.persisted.ewrs.insert_cow(gid);
                }
            }
            DeployKind::Troop { spec, .. } => {
                self.persisted.troops.insert_cow(gid);
                if spec.jtac.is_some() {
                    self.persisted.jtacs.insert_cow(gid);
                }
            }
            DeployKind::DownedPilot { .. } => {
                self.persisted.downed_pilots.insert_cow(gid);
            }
            DeployKind::Dismount { .. } => {
                self.persisted.dismounts.insert_cow(gid);
                self.persisted.dismount_spawned.insert_cow(gid, Utc::now());
            }
        }
        self.persisted.groups.insert_cow(gid, spawned);
        self.persisted.groups_by_name.insert_cow(group_name, gid);
        self.persisted.groups_by_side.get_or_default_cow(side).insert_cow(gid);
        self.ephemeral.dirty();
        self.mark_group(&gid)?;
        Ok(gid)
    }

    /// Create a `SpawnedGroup` from inline unit definitions (no .miz template required).
    /// Registers a `SyntheticGroupSpec` in `ephemeral.synthetic_templates` so that
    /// `spawn_group` can build the DCS Lua table at respawn time.
    pub(super) fn add_group_from_units(
        &mut self,
        spctx: &SpawnCtx,
        side: Side,
        country: Country,
        units: &[SpecialSamUnitCfg],
        origin: DeployKind,
    ) -> Result<GroupId> {
        use super::ephemeral::SyntheticGroupSpec;

        let gid = GroupId::new();
        // Use a stable template name derived from the group ID so synthetic_templates can look it up.
        let template_name = String::from(format_compact!("@synthetic:{}", gid));
        let group_name = template_name.clone();

        let mut spawned = SpawnedGroup {
            id: gid,
            name: group_name.clone(),
            template_name: template_name.clone(),
            side,
            kind: Some(GroupCategory::Ground),
            origin,
            class: ObjGroupClass::Lr,
            units: SetS::new(),
            tags: UnitTags(BitFlags::empty()),
        };

        let land = Land::singleton(spctx.lua())?;
        for unit_cfg in units {
            let uid = UnitId::new();
            let tags = *self
                .ephemeral
                .cfg
                .unit_classification
                .get(unit_cfg.typ.as_str())
                .ok_or_else(|| anyhow!("unit type not classified {}", unit_cfg.typ))?;
            let tags = UnitTags(tags.0);
            spawned.tags.0.insert(tags.0);

            let unit_name = String::from(format_compact!("{}-{}", group_name, uid));
            let unit_pos = Vector2::new(unit_cfg.pos.x, unit_cfg.pos.y);
            let height = land.get_height(LuaVec2(unit_pos))?;
            let mut position = Position3::default();
            position.p.x = unit_cfg.pos.x;
            position.p.y = height;
            position.p.z = unit_cfg.pos.y;

            let su = SpawnedUnit {
                id: uid,
                group: gid,
                side,
                typ: Vehicle(String::from(unit_cfg.typ.as_str())),
                tags,
                name: unit_name.clone(),
                template_name: unit_name.clone(),
                spawn_position: position,
                spawn_pos: unit_pos,
                spawn_heading: unit_cfg.heading,
                position,
                pos: unit_pos,
                heading: unit_cfg.heading,
                dead: false,
                moved: None,
                airborne_velocity: None,
            };
            spawned.units.insert_cow(uid);
            self.persisted.units.insert_cow(uid, su);
            self.persisted.units_by_name.insert_cow(unit_name, uid);
        }

        self.ephemeral.synthetic_templates.insert(
            template_name.clone(),
            SyntheticGroupSpec { country, category: GroupCategory::Ground },
        );

        self.persisted.groups.insert_cow(gid, spawned);
        self.persisted.groups_by_name.insert_cow(group_name, gid);
        self.persisted.groups_by_side.get_or_default_cow(side).insert_cow(gid);
        self.ephemeral.dirty();
        self.mark_group(&gid)?;
        Ok(gid)
    }

    pub fn add_and_queue_group<'lua>(
        &mut self,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        location: SpawnLoc,
        template_name: &str,
        origin: DeployKind,
        extra_tags: BitFlags<UnitTag>,
        delay: Option<DateTime<Utc>>,
    ) -> Result<GroupId> {
        let gid = self.add_group(
            &spctx,
            idx,
            side,
            location,
            template_name,
            origin,
            extra_tags,
        )?;
        match delay {
            None => self.ephemeral.push_spawn(gid),
            Some(at) => self.ephemeral.delayspawnq.entry(at).or_default().push(gid),
        }
        Ok(gid)
    }

    /// Spawn a lone radio/GNSS jammer truck (`GPS_Spoofer_Blue`/`_Red`, the
    /// only DCS units with the `Jammer` attribute) at `pos`, straight away.
    /// No .miz template is needed -- the group is built from the unit type,
    /// the same way special SAM sites are. It is session scoped
    /// (`EventSpawn`): a restart drops it rather than respawning a synthetic
    /// template that no longer exists. The jammer starts switched off; turn
    /// it on with `Command::ActivateJammer` on its group controller.
    pub fn spawn_jammer_truck(
        &mut self,
        perf: &mut bfprotocols::perf::PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        pos: Vector2,
        heading: f64,
    ) -> Result<GroupId> {
        let (typ, country) = match side {
            Side::Blue => ("GPS_Spoofer_Blue", Country::CJTF_BLUE),
            Side::Red => ("GPS_Spoofer_Red", Country::CJTF_RED),
            Side::Neutral => bail!("a jammer needs a side"),
        };
        let origin = self
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, o)| o.owner == side)
            .min_by(|(_, a), (_, b)| {
                na::distance_squared(&a.zone.pos().into(), &pos.into())
                    .total_cmp(&na::distance_squared(&b.zone.pos().into(), &pos.into()))
            })
            .map(|(id, _)| *id)
            .ok_or_else(|| anyhow!("{side} holds no objectives"))?;
        let gid = self.add_group_from_units(
            spctx,
            side,
            country,
            &[SpecialSamUnitCfg {
                typ: String::from(typ),
                pos: bfprotocols::cfg::Pos2d { x: pos.x, y: pos.y },
                heading,
            }],
            DeployKind::Objective { origin },
        )?;
        if let Some(group) = self.persisted.groups.get_mut_cow(&gid) {
            group.tags.0.insert(UnitTag::EventSpawn);
        }
        let spawned = group!(self, gid).and_then(|group| {
            self.ephemeral
                .spawn_group(perf, &self.persisted, idx, spctx, group, vec![])
        });
        if let Err(e) = spawned {
            if let Err(de) = self.delete_group(&gid) {
                error!("could not remove unspawned jammer {gid:?}: {de:?}");
            }
            return Err(e);
        }
        Ok(gid)
    }

    /// Spawn an air group from `template` on the ground at friendly field
    /// `origin`, engines running, and fly `mission` instead of the template's
    /// route, right now rather than via the spawn queue. `pos` is the spot on
    /// the field it starts at (see `launch_spot`), and waypoint 0 of `mission`
    /// must sit there too: `spawn_group` turns that waypoint into the takeoff
    /// -- a parking start on the field's ramp, or a lift-off from open ground
    /// for a helicopter (or a fixed-wing airframe with `open_ground`, which
    /// needs no runway). A flight that can do neither is refused, never
    /// air-started over the field.
    ///
    /// The group is tagged `EventSpawn`, so it is session scoped: a restart
    /// drops it instead of respawning it with nothing left to manage it. On a
    /// failed spawn the group is removed from the db again, so the caller
    /// never has to clean up a phantom.
    pub fn spawn_air_flight<'lua>(
        &mut self,
        perf: &mut bfprotocols::perf::PerfInner,
        spctx: &SpawnCtx<'lua>,
        idx: &MizIndex,
        side: Side,
        template: &str,
        origin: ObjectiveId,
        pos: Vector2,
        heading: f64,
        open_ground: bool,
        mission: Vec<dcso3::controller::MissionPoint<'lua>>,
    ) -> Result<GroupId> {
        let gid = self.add_group(
            spctx,
            idx,
            side,
            SpawnLoc::AtPosExact { pos, group_heading: heading },
            template,
            DeployKind::Objective { origin },
            UnitTag::EventSpawn | UnitTag::HotStart,
        )?;
        self.ephemeral.launch_requests.insert(
            gid,
            LaunchRequest { field: origin, cold: false, open_ground },
        );
        let spawned = group!(self, gid).and_then(|group| {
            self.ephemeral
                .spawn_group(perf, &self.persisted, idx, spctx, group, mission)
        });
        if let Err(e) = spawned {
            self.ephemeral.launch_requests.remove(&gid);
            if let Err(de) = self.delete_group(&gid) {
                error!("could not remove unspawned air flight {gid:?}: {de:?}");
            }
            return Err(e);
        }
        Ok(gid)
    }

    /// The spot on friendly field `oid` an aircraft of this kind takes off
    /// from, or `None` if it can't launch from there.
    ///
    /// Fixed wing need a live DCS airbase with at least one free spot their
    /// terminal type can use -- the same check `spawn_group` builds the
    /// parking start from -- and start at that airbase's reference point
    /// (the parking plan moves them onto their spots). Helicopters can always
    /// go: a pad is used if the field has one free, and otherwise they lift
    /// off from open ground, so they start on the clearest patch near the
    /// zone centre rather than inside its garrison.
    /// Airframes too big for a fighter's spot: bombers, tankers, AWACS,
    /// transports and airliners only fit a large open stand (`OPEN_BIG`).
    pub(crate) fn needs_big_spot(typ: &str) -> bool {
        const HEAVY: &[&str] = &[
            "B-1B", "B-52H", "Tu-22M3", "Tu-95MS", "Tu-142", "Tu-160", "E-3A", "E-2C", "A-50", "KJ-2000",
            "KC-135", "KC135MPRS", "KC130", "KC130J", "IL-78M", "IL-76MD", "C-130", "C-130J-30", "C-17A",
            "An-26B", "An-30M", "Yak-40", "A_320", "A_330", "A_380", "B_727", "B_737", "B_747", "B_757",
            "Boeing_C-17A", "Hercules", "P-3C", "S-3B", "S-3B Tanker",
        ];
        HEAVY.contains(&typ)
    }

    pub(crate) fn launch_spot(&self, lua: MizLua, oid: &ObjectiveId, rotary: bool, heavy: bool) -> Option<Vector2> {
        let obj = self.persisted.objectives.get(oid)?;
        if rotary {
            return Some(self.clear_helo_spot(obj.zone.pos()));
        }
        match self.ephemeral.usable_parking(lua, &self.persisted, oid, false, heavy) {
            Some((pos, n)) if n > 0 => Some(pos),
            Some(_) => {
                info!(
                    "[LAUNCH] {} has no free {} parking",
                    obj.name,
                    if heavy { "large (heavy aircraft)" } else { "fixed-wing" }
                );
                None
            }
            None => {
                info!("[LAUNCH] {} has no DCS airbase to park fixed wing at", obj.name);
                None
            }
        }
    }

    /// The nearest friendly field to `near`, at least `min_dist_m` from it,
    /// that an aircraft of this kind can really take off from right now,
    /// nearest first: helicopters from an airbase, FARP or FOB; fixed wing
    /// from an airbase (or an `extra_fixed_wing_objectives` field) with free
    /// parking. A field that fails the parking check is passed over for the
    /// next one out, not used anyway. Bases still in their post-capture hold
    /// are skipped -- the fight for them isn't over.
    pub(crate) fn pick_launch_field(
        &self,
        lua: MizLua,
        side: Side,
        near: Vector2,
        rotary: bool,
        heavy: bool,
        min_dist_m: f64,
    ) -> Option<LaunchField> {
        // Each candidate costs a couple of Lua calls (airbase + parking);
        // past this many the next field out is too far to matter anyway.
        const MAX_FIELDS_TRIED: usize = 8;
        let extra = &self.ephemeral.cfg.extra_fixed_wing_objectives;
        let ranked = rank_launch_fields(
            self.persisted
                .objectives
                .into_iter()
                .filter(|(_, o)| {
                    o.owner == side
                        && !o.captureable()
                        && is_launch_kind(&o.kind, rotary, extra.contains(&o.name))
                })
                .map(|(id, o)| (*id, o.zone.pos())),
            near,
            min_dist_m,
        );
        for oid in ranked.into_iter().take(MAX_FIELDS_TRIED) {
            if let Some(pos) = self.launch_spot(lua, &oid, rotary, heavy) {
                let name = self
                    .persisted
                    .objectives
                    .get(&oid)
                    .map(|o| String::from(o.name.as_str()))
                    .unwrap_or_default();
                return Some(LaunchField { oid, name, pos });
            }
        }
        None
    }

    /// Plan a ground drive from `from` to `to`: along the road network when
    /// DCS has a road that serves both ends, else straight across country if
    /// `cross_country`, else `None`.
    pub(crate) fn plan_drive(
        &self,
        lua: MizLua,
        from: Vector2,
        to: Vector2,
        cross_country: bool,
    ) -> Option<DrivePlan> {
        let land = Land::singleton(lua).ok()?;
        let road: Option<Vec<Vector2>> = land
            .find_path_on_roads(RoadType::Road, LuaVec2(from), LuaVec2(to))
            .ok()
            .map(|seq| seq.into_iter().filter_map(|p| p.ok()).map(|p| p.0).collect());
        road.as_deref()
            .and_then(|r| plan_road_drive(from, to, r))
            .or_else(|| cross_country.then(|| plan_cross_country(from, to)))
    }

    /// Queue a ground group from `template` that starts at objective `origin`
    /// (point 0 of `plan`) and drives `plan` the moment the spawn queue puts
    /// it into DCS. Queued rather than spawned on the spot so it is safe from
    /// event handlers: `coalition.addGroup` fires Birth synchronously.
    pub(crate) fn queue_drive_from_objective(
        &mut self,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        origin: ObjectiveId,
        template: &str,
        plan: &DrivePlan,
        speed_mps: f64,
        tags: BitFlags<UnitTag>,
    ) -> Result<GroupId> {
        self.queue_drive(spctx, idx, side, DeployKind::Objective { origin }, template, plan, speed_mps, tags)
    }

    /// `queue_drive_from_objective` for a group of any origin: a commander's
    /// deployment that has to drive to where it was ordered.
    pub(crate) fn queue_drive(
        &mut self,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        origin: DeployKind,
        template: &str,
        plan: &DrivePlan,
        speed_mps: f64,
        tags: BitFlags<UnitTag>,
    ) -> Result<GroupId> {
        let from = plan
            .points
            .first()
            .map(|(p, _)| *p)
            .ok_or_else(|| anyhow!("empty drive"))?;
        let dir = plan
            .points
            .get(1)
            .map(|(p, _)| *p - from)
            .filter(|d| d.norm() > 1.)
            .map(|d| d.normalize())
            .unwrap_or(Vector2::new(1., 0.));
        let gid = self.add_group(
            spctx,
            idx,
            side,
            SpawnLoc::AtPos {
                pos: from,
                offset_direction: dir,
                group_heading: dir.y.atan2(dir.x),
            },
            template,
            origin,
            tags,
        )?;
        self.ephemeral.queued_drives.insert(
            gid,
            QueuedDrive { points: plan.points.clone(), speed_mps },
        );
        self.ephemeral.push_spawn(gid);
        Ok(gid)
    }

    pub(crate) fn unit_born(
        &mut self,
        lua: MizLua,
        unit: &Unit,
        connected: &Connected,
    ) -> Result<BirthRes> {
        let id = unit.object_id()?;
        let name = unit.get_name()?;
        // First try direct name lookup, then try template_name lookup for activated carrier units
        // Carrier groups use Group.activate() which keeps the original DCS unit names (e.g., "BCARRIER-1")
        // but bflib stores units with names like "{group_name}-{uid}"
        let uid_lookup = self.persisted.units_by_name.get(name.as_str()).copied()
            .or_else(|| {
                // Try finding by template_name for activated carriers
                self.persisted.units.into_iter()
                    .find(|(_, u)| u.template_name == name)
                    .map(|(uid, _)| *uid)
            });
        if let Some(uid) = uid_lookup {
            let unit = unit!(self, uid)?;
            self.ephemeral.uid_by_object_id.insert(id.clone(), uid);
            self.ephemeral.object_id_by_uid.insert(uid, id.clone());
            self.ephemeral.units_potentially_close_to_enemies.insert(uid);
            if unit.tags.contains(UnitTag::Driveable) || unit.tags.contains(UnitTag::Boat) {
                self.ephemeral.units_able_to_move.insert(uid);
            }
            self.ephemeral.stat(Stat::Unit {
                id: EnId::Unit(uid),
                gid: Some(unit.group),
                owner: unit.side,
                typ: stats::Unit { typ: unit.typ.clone(), tags: unit.tags },
                pos: stats::Pos {
                    pos: Coord::singleton(lua)?
                        .lo_to_ll(LuaVec3(Vector3::new(unit.pos.x, 0., unit.pos.y)))?,
                    velocity: unit.airborne_velocity.unwrap_or_default(),
                },
            });
            let gid = unit.group;
            if group_health!(self, gid)?.0 == 1 {
                self.mark_group(&gid)?
            }
            return Ok(BirthRes::None);
        }
        let slot = unit.slot()?;
        let (si, deferred_validate) = match self.ephemeral.slot_info.get(&slot) {
            Some(si) => (si, false),
            None => {
                // it's a dynamic slot
                let typ = Vehicle::from(unit.as_object()?.get_type_name()?);
                let pos = unit.get_ground_position()?;
                let obj =
                    Db::objective_near_point(&self.persisted.objectives, pos.0, |_| true)
                        .map(|(_, _, o)| o)
                        .ok_or_else(|| anyhow!("dynamic slot not near any objective"))?;
                let gid = unit.get_group()?.id()?;
                let gid = miz::GroupId::from(gid.inner());
                self.ephemeral.slot_info.insert(
                    slot,
                    SlotInfo {
                        typ,
                        unit_name: unit.get_name()?,
                        objective: obj.id,
                        ground_start: false,
                        miz_gid: gid,
                        side: obj.owner,
                    },
                );
                self.ephemeral.slot_by_miz_gid.insert(gid, slot);
                (&self.ephemeral.slot_info[&slot], true)
            }
        };
        let name = unit.get_player_name()?;
        let ifo = name.and_then(|name| connected.get_by_name(&name));
        let ucid = match ifo {
            Some(ifo) => ifo.ucid,
            None => {
                error!("slot {slot} born with no player in it");
                unit.clone().destroy()?;
                return Ok(BirthRes::None);
            }
        };
        let side = si.side;
        let typ = si.typ.clone();
        let objective = si.objective;
        let tags = *self
            .ephemeral
            .cfg
            .unit_classification
            .get(&typ)
            .unwrap_or(&UnitTags::default());
        if deferred_validate {
            match self.try_occupy_slot_deferred(Utc::now(), &ucid, slot) {
                SlotAuth::Yes(typ) => {
                    self.ephemeral.stat(Stat::Slot { id: ucid, slot, typ });
                }
                a => {
                    unit.clone().destroy()?;
                    return Ok(BirthRes::DynamicSlotDenied(ucid, a));
                }
            }
        }
        self.ephemeral.stat(Stat::Unit {
            id: EnId::Player(ucid),
            gid: None,
            owner: side,
            typ: stats::Unit { typ, tags },
            pos: stats::Pos {
                pos: Coord::singleton(lua)?.lo_to_ll(unit.get_point()?)?,
                velocity: Vector3::default(),
            },
        });
        self.player_entered_slot(lua, id, unit, slot, objective, ucid)
            .context("entering player into slot")?;
        Ok(BirthRes::OccupiedSlot(slot))
    }

    pub fn static_born(&mut self, st: &StaticObject) -> Result<()> {
        let id = st.object_id()?;
        let name = st.get_name()?;

        // Map static object ID to unit ID
        if let Some(uid) = self.persisted.units_by_name.get(name.as_str()) {
            self.ephemeral.uid_by_static.insert(id.clone(), *uid);
        }

        // Check if this is a C-130 crate
        // When DCS's cargo system loads/drops a crate, DCS destroys the old static and creates a new one
        // We need to match by name pattern since DCS may rename the crate
        let mut matched_crate_key: Option<std::string::String> = None;

        // First try exact match
        if self.ephemeral.c130_crates.contains_key(name.as_str()) {
            matched_crate_key = Some(std::string::String::from(name.as_str()));
            info!("[C130_CARGO] static_born: Found exact match for crate '{}'", name);
        } else if !self.ephemeral.c130_crates.is_empty() {
            // If exact match fails, try prefix match
            // This handles cases where DCS cargo system renames the crate
            for tracked_name in self.ephemeral.c130_crates.keys() {
                // Check if either name is a prefix of the other
                if name.starts_with(tracked_name.as_str()) || tracked_name.starts_with(name.as_str()) {
                    matched_crate_key = Some(std::string::String::from(tracked_name.as_str()));
                    info!("[C130_CARGO] static_born: Found prefix match - static '{}' matches tracked '{}'", name, tracked_name);
                    break;
                }
            }

            // If no match yet, check if names match the template pattern
            // This handles DCS stripping suffixes like "_C130" when renaming
            if matched_crate_key.is_none() {
                for (tracked_name, crate_data) in &self.ephemeral.c130_crates {
                    if let Some(group) = self.persisted.groups.get(&crate_data.group_id) {
                        let template = &group.template_name;

                        // Extract base template name (everything before underscore or hyphen)
                        // E.g., "RCRATE_C130" -> "RCRATE", "BCRATE_C130" -> "BCRATE"
                        let base_template = template.split('_').next().unwrap_or(template.as_str());

                        // Check if both names start with the base template
                        if name.starts_with(base_template) && tracked_name.starts_with(base_template) {
                            // Additional check: extract group ID from both names
                            // Names follow pattern: "RCRATE_C130-2289-8325" or "RCRATE-2289-8325"
                            // Extract the first number after hyphen
                            let name_parts: Vec<&str> = name.split('-').collect();
                            let tracked_parts: Vec<&str> = tracked_name.split('-').collect();

                            if name_parts.len() >= 2 && tracked_parts.len() >= 2 {
                                let name_gid = name_parts[1];
                                let tracked_gid = tracked_parts[1];

                                if name_gid == tracked_gid {
                                    matched_crate_key = Some(std::string::String::from(tracked_name.as_str()));
                                    info!("[C130_CARGO] static_born: Found template match - static '{}' and tracked '{}' both use base template '{}' with gid {}",
                                        name, tracked_name, base_template, name_gid);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        if let Some(crate_key) = matched_crate_key {
            let crate_data = self.ephemeral.c130_crates.get_mut(crate_key.as_str()).unwrap();
            let gid = crate_data.group_id;

            // For static objects, use the static's own object_id directly
            // (not a group's object_id, since statics don't have groups in DCS API)
            if let Ok(obj) = st.as_object() {
                if let Ok(static_oid) = obj.object_id() {
                    // DCS destroys the original static when a player loads it
                    // as cargo (via F8 Ground Crew) and creates a brand new
                    // one, with a new object_id but the same tracked name,
                    // when it's dropped. Gating this on "do we have any
                    // mapping at all" left the tracked mapping pointing at
                    // the now-destroyed original object forever after the
                    // first load/drop cycle, so the dropped crate was never
                    // re-tracked even though it's physically still there.
                    // Compare the actual object_id instead so a drop always
                    // repoints the mapping to the live object.
                    let already_current = self.ephemeral.object_id_by_gid.get(&gid) == Some(&static_oid);
                    if !already_current {
                        if let Some(old_oid) = self.ephemeral.object_id_by_gid.get(&gid) {
                            self.ephemeral.gid_by_object_id.remove(old_oid);
                        }
                        info!("[C130_CARGO] static_born: Updating object_id mapping for crate '{}' (tracked as '{}') group {:?} -> {:?}",
                            name, crate_key, gid, static_oid);
                        self.ephemeral.object_id_by_gid.insert(gid, static_oid.clone());
                        self.ephemeral.gid_by_object_id.insert(static_oid, gid);

                        // Note: We don't transition to Airborne here
                        // The update_c130_crates function will detect when the crate is actually airborne
                        // based on in_air and speed checks
                    } else {
                        info!("[C130_CARGO] static_born: Mapping already up to date for {:?}, skipping", gid);
                    }
                } else {
                    info!("[C130_CARGO] static_born: Failed to get static object_id");
                }
            } else {
                info!("[C130_CARGO] static_born: Failed to convert static to object");
            }
        }

        Ok(())
    }

    pub fn unit_dead(
        &mut self,
        id: &DcsOid<ClassUnit>,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let uid = match self.ephemeral.unit_dead(&self.persisted, id) {
            None => return Ok(()),
            Some((uid, ucid)) => {
                if let Some(ucid) = ucid {
                    self.player_deslot(&ucid);
                    // Physical cargo crates this player was carrying die with the
                    // aircraft -- a crashed delivery must not leave free crates
                    // behind to be recovered or auto-unpacked. Only crates that
                    // actually moved with the aircraft are removed; ones still
                    // sitting where they were spawned stay put for another pilot.
                    let orphaned: Vec<String> = self
                        .ephemeral
                        .c130_crates
                        .iter()
                        .filter(|(_, c)| {
                            c.player == ucid
                                && matches!(
                                    c.state,
                                    C130CargoState::Spawned | C130CargoState::Loaded
                                )
                                && na::distance(&c.last_pos.into(), &c.spawn_pos.into()) > 50.0
                        })
                        .map(|(name, _)| name.clone())
                        .collect();
                    for name in orphaned {
                        if let Some(c) = self.ephemeral.c130_crates.remove(&name) {
                            if let Some(id) = c.missing_marker {
                                self.ephemeral.msgs().delete_mark(id);
                            }
                            if let Err(e) = self.delete_group(&c.group_id) {
                                error!(
                                    "[C130_CARGO] failed to delete orphaned crate {name}: {e:?}"
                                );
                            }
                        }
                    }
                }
                uid
            }
        };
        match self.persisted.units.get_mut_cow(&uid) {
            None => error!("unit_dead: missing unit {:?}", uid),
            Some(unit) => {
                unit.dead = true;
                unit.pos = unit.spawn_pos;
                unit.heading = unit.spawn_heading;
                unit.position = unit.spawn_position;
                self.ephemeral.dirty();
                let gid = unit.group;
                let health = group_health!(self, gid)?.0;
                if let Some(oid) = self.persisted.objectives_by_group.get(&gid).copied() {
                    self.update_objective_status(&oid, now)?;
                    self.ephemeral.units_potentially_close_to_enemies.remove(&uid);
                    if health == 0 {
                        if let Some((id, _)) = self.ephemeral.group_marks.remove(&gid) {
                            self.ephemeral.msgs.delete_mark(id);
                        }
                    }
                }
                if self.is_player_deployed(&gid)
                    || self.persisted.troops.contains(&gid)
                    || self.persisted.crates.contains(&gid)
                    || self.persisted.dismounts.contains(&gid)
                {
                    if health == 0 {
                        match &group!(self, gid)?.origin {
                            DeployKind::Troop {
                                player,
                                moved_by: Some((ucid, p)),
                                ..
                            }
                            | DeployKind::Deployed {
                                player,
                                moved_by: Some((ucid, p)),
                                ..
                            } => {
                                let owner = self
                                    .persisted
                                    .players
                                    .get(player)
                                    .map(|p| p.name.clone())
                                    .unwrap_or_else(|| String::from("unknown"));
                                let ucid = ucid.clone();
                                let p = -(*p as i32);
                                let msg = format_compact!(
                                    "for the death of {gid} which was deployed by {owner} and moved by you"
                                );
                                self.adjust_points(&ucid, p, &msg)
                            }
                            DeployKind::Troop { .. }
                            | DeployKind::Deployed { .. }
                            | DeployKind::Action { .. }
                            | DeployKind::Crate { .. }
                            | DeployKind::Objective { .. }
                            | DeployKind::ObjectiveDeprecated
                            | DeployKind::DownedPilot { .. }
                            | DeployKind::Dismount { .. } => (),
                        }
                        self.delete_group(&gid)?
                    }
                }
                if self.persisted.downed_pilots.contains(&gid) && health == 0 {
                    self.delete_group(&gid)?
                }
                if self.persisted.actions.contains(&gid) {
                    if let DeployKind::Action { player, spec, .. } =
                        &group!(self, gid)?.origin
                    {
                        if self.group_health(&gid)?.0 == 0
                            && matches!(spec.kind, ActionKind::Reinforce(_))
                        {
                            // Says so to both sides and charges its own
                            // penalty (reinforce.rs).
                            self.reinforcements_lost(gid, now);
                        } else if self.group_health(&gid)?.0 == 0 {
                            if let Some((penalty, ucid)) = spec
                                .penalty
                                .and_then(|p| player.as_ref().map(|pl| (p, pl.clone())))
                            {
                                self.adjust_points(
                                    &ucid,
                                    -(penalty as i32),
                                    &format_compact!(
                                        "for the loss of action group {gid}"
                                    ),
                                )
                            }
                            self.delete_group(&gid)?
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn static_dead(
        &mut self,
        id: &DcsOid<ClassStatic>,
        now: DateTime<Utc>,
    ) -> Result<()> {
        if let Some(uid) = self.ephemeral.uid_by_static.remove(id) {
            match self.persisted.units.get_mut_cow(&uid) {
                None => error!("static_dead: missing unit {:?}", uid),
                Some(unit) => {
                    unit.dead = true;
                    let gid = unit.group;
                    self.ephemeral.dirty();
                    if let Some(oid) =
                        self.persisted.objectives_by_group.get(&gid).copied()
                    {
                        self.update_objective_status(&oid, now)?;
                    }
                    if self.is_player_deployed(&gid)
                        || self.persisted.troops.contains(&gid)
                        || self.persisted.crates.contains(&gid)
                    {
                        if self.group_health(&gid)?.0 == 0 {
                            self.delete_group(&gid)?
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// If `id` is a tracked "immortal" decoration static (see `init_protected_statics`),
    /// respawn it from its original template so it looks like it was never destroyed.
    /// Safe to call for any dead static id; no-ops if it isn't a protected one.
    pub fn respawn_protected_static(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        id: &DcsOid<ClassStatic>,
    ) -> Result<()> {
        if let Some(protected) = self.ephemeral.protected_statics.remove(id) {
            let spctx = SpawnCtx::new(lua)?;
            let template = spctx.get_template(
                idx,
                GroupKind::Static,
                protected.side,
                protected.template_name.as_str(),
            )?;
            spctx.spawn(template)?;
            match StaticObject::get_by_name(lua, protected.template_name.as_str()) {
                Ok(Static::Static(obj)) => {
                    let new_id = obj.object_id()?;
                    self.ephemeral.protected_statics.insert(new_id, protected);
                }
                Ok(Static::Airbase(_)) => (),
                Err(e) => warn!(
                    "respawned protected static '{}' but couldn't re-find it: {e:?}",
                    protected.template_name
                ),
            }
        }
        Ok(())
    }

    pub fn group_health(&self, gid: &GroupId) -> Result<(usize, usize)> {
        group_health!(self, gid)
    }

    pub fn artillery_near_point(
        &self,
        side: Side,
        pos: Vector2,
    ) -> SmallVec<[GroupId; 8]> {
        let range2 = (self.ephemeral.cfg.artillery_mission_range as f64).powi(2);
        let artillery = self
            .deployed()
            .filter_map(|group| {
                // Tube/rocket artillery is tagged Artillery; ballistic/cruise TELs
                // (Scud, Iskander, Silkworm, ...) are tagged Launcher. Accept both,
                // but exclude SAM launchers (SA-x) which also carry Launcher.
                let is_arty = group.tags.contains(UnitTag::Artillery)
                    || (group.tags.contains(UnitTag::Launcher)
                        && !group.tags.contains(UnitTag::SAM));
                if is_arty && group.side == side {
                    let center = self.group_center(&group.id).ok()?;
                    if na::distance_squared(&center.into(), &pos.into()) <= range2 {
                        Some(group.id)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect::<SmallVec<[GroupId; 8]>>();
        artillery
    }

    pub fn alcm_near_point(
        &self,
        side: Side,
        lua: MizLua,
        pos: Vector2,
    ) -> SmallVec<[(GroupId, i32); 8]> {
        let range2 = (self.ephemeral.cfg.alcm_mission_range as f64).powi(2);
        let alcm = self
            .actions()
            .filter_map(|group| {
                if group.tags.contains(UnitTag::ALCM) && group.side == side {
                    let center = self.group_center(&group.id).ok()?;
                    if na::distance_squared(
                        &pos.into(),
                        &na::Point2::new(center.x, center.y),
                    ) <= range2
                    {
                        let mut unit: Option<Unit> = None;
                        let mut ammo = 0;
                        if let Some(uid) = group.units.into_iter().next() {
                            if let Some(id) = self.ephemeral.object_id_by_uid.get(&uid) {
                                let instance = match unit.take() {
                                    Some(unit) => unit.change_instance(id),
                                    None => Unit::get_instance(lua, id),
                                };
                                ammo = (|| -> anyhow::Result<i32> {
                                    Ok(instance?.get_ammo()?.first()?.count()? as i32)
                                })()
                                .unwrap_or(0);
                            };
                        }

                        Some((group.id, ammo))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect::<SmallVec<[(GroupId, i32); 8]>>();
        alcm
    }

    pub fn update_unit_positions_incremental(
        &mut self,
        lua: MizLua,
        now: DateTime<Utc>,
        mut last: usize,
    ) -> Result<(usize, Vec<DcsOid<ClassUnit>>)> {
        let total = self.ephemeral.units_able_to_move.len();
        if last < total {
            let mut uids: SmallVec<[UnitId; 64]> = smallvec![];
            let elts = self.ephemeral.units_able_to_move.as_slice();
            // Process 1/16 of units per tick, capped at 32 to bound frame time.
            // Bug fix: compare uids.len() to the CHUNK SIZE, not the absolute
            // stop index — the old `uids.len() < stop` doubled the batch each
            // successive tick (tick 2 processed 2× the intended amount, etc.).
            let chunk = max(1, total >> 4).min(32);
            while last < total && uids.len() < chunk {
                uids.push(elts[last]);
                last += 1;
            }
            Ok((last, self.update_unit_positions(lua, now, &uids)?))
        } else {
            Ok((0, vec![]))
        }
    }

    pub fn update_unit_positions(
        &mut self,
        lua: MizLua,
        now: DateTime<Utc>,
        units: &[UnitId],
    ) -> Result<Vec<DcsOid<ClassUnit>>> {
        let coord = Coord::singleton(lua)?;
        let mut unit: Option<Unit> = None;
        let mut moved: SmallVec<[GroupId; 16]> = smallvec![];
        let mut dead: Vec<DcsOid<ClassUnit>> = vec![];
        for uid in units {
            let id = match self.ephemeral.object_id_by_uid.get(&uid) {
                Some(id) => id,
                None => {
                    warn!("update_unit_positions skipping unknown unit {uid}");
                    continue;
                }
            };
            let instance = match unit.take() {
                Some(unit) => unit.change_instance(id),
                None => Unit::get_instance(lua, id),
            };
            let instance = match instance {
                Ok(i) => i,
                Err(e) => {
                    warn!(
                        "update_unit_positions skipping invalid instance {uid}, {:?}",
                        e
                    );
                    dead.push(id.clone());
                    continue;
                }
            };
            let pos = instance.get_position()?;
            let spunit = unit_mut!(self, uid)?;
            if (spunit.position.p.0 - pos.p.0).magnitude_squared() > 1.0 {
                moved.push(spunit.group);
                spunit.moved = Some(now);
                spunit.position = pos;
                spunit.pos = Vector2::new(pos.p.x, pos.p.z);
                spunit.heading = azumith3d(pos.x.0);
                self.ephemeral.units_potentially_close_to_enemies.insert(*uid);
                let v = if spunit.tags.contains(UnitTag::Aircraft) && instance.in_air()? {
                    let v = instance.get_velocity()?.0;
                    spunit.airborne_velocity = Some(v);
                    Some(v)
                } else {
                    spunit.airborne_velocity = None;
                    None
                };
                self.ephemeral.stat(Stat::Position {
                    id: EnId::Unit(*uid),
                    pos: stats::Pos {
                        pos: coord.lo_to_ll(pos.p)?,
                        velocity: v.unwrap_or_default(),
                    },
                });
            }
            unit = Some(instance);
        }
        // `moved` carries one entry per unit that shifted, so an eight-truck
        // squad used to re-pin itself eight times in a single pass. Collapse it
        // to one pin per group, and only redraw a pin the group has walked
        // away from.
        moved.sort();
        moved.dedup();
        for gid in moved {
            self.ephemeral.dirty();
            self.mark_group_if_moved(&gid, GROUP_MARK_MIN_MOVE)?;
        }
        Ok(dead)
    }

    /// Returns an iterator over all **non-player** aircraft/helicopter units that have been
    /// confirmed alive by DCS (i.e. they appear in `object_id_by_uid`). This includes
    /// AI CAP, AI AWACS, logistics aircraft, etc.
    ///
    /// The EWR system calls this to get the live DCS object IDs it needs to call
    /// `Unit::get_instance()` and check `in_air()` for each AI aircraft, so that
    /// AI units show up in radar reports just like player aircraft do.
    pub fn ai_aircraft_unit_ids(
        &self,
    ) -> impl Iterator<Item = (UnitId, &DcsOid<ClassUnit>, Side)> {
        self.ephemeral.object_id_by_uid.iter().filter_map(|(uid, oid)| {
            let su = self.persisted.units.get(uid)?;
            // Fixed-wing or rotary-wing only.
            if !su.tags.contains(UnitTag::Aircraft) && !su.tags.contains(UnitTag::Helicopter) {
                return None;
            }
            // Exclude units that belong to a player slot (already tracked via instanced_players).
            if self.ephemeral.slot_by_object_id.contains_key(oid) {
                return None;
            }
            Some((*uid, oid, su.side))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn launch_fields_rank_nearest_first_past_the_minimum() {
        let near = Vector2::new(0., 0.);
        let fields = [
            (1, Vector2::new(50_000., 0.)),
            (2, Vector2::new(5_000., 0.)),
            (3, Vector2::new(0., 20_000.)),
        ];
        assert_eq!(rank_launch_fields(fields, near, 10_000.), vec![3, 1]);
        assert_eq!(rank_launch_fields(fields, near, 0.), vec![2, 3, 1]);
    }

    #[test]
    fn helicopters_and_jets_launch_from_their_own_kinds() {
        assert!(is_launch_kind(&ObjectiveKind::Airbase, true, false));
        assert!(is_launch_kind(&ObjectiveKind::Fob, true, false));
        assert!(!is_launch_kind(&ObjectiveKind::Logistics, true, false));
        assert!(!is_launch_kind(&ObjectiveKind::Fob, false, false));
        assert!(is_launch_kind(&ObjectiveKind::Fob, false, true));
        assert!(is_launch_kind(&ObjectiveKind::Airbase, false, false));
    }

    #[test]
    fn road_drive_ends_off_road_at_the_destination() {
        let from = Vector2::new(0., 0.);
        let to = Vector2::new(20_000., 500.);
        let road: Vec<Vector2> = (0..=200).map(|i| Vector2::new(i as f64 * 100., 0.)).collect();
        let plan = plan_road_drive(from, to, &road).expect("road serves both ends");
        assert!(plan.by_road);
        assert_eq!(plan.points.first(), Some(&(from, true)));
        assert_eq!(plan.points.last(), Some(&(to, false)));
        // Thinned to roughly one point every 3 km plus the road exit.
        assert!(plan.points.len() <= 12, "{} points", plan.points.len());
        assert!((plan.route_m - 20_000.).abs() < 1_000.);
    }

    #[test]
    fn a_road_that_misses_either_end_is_no_drive() {
        let road = [Vector2::new(0., 0.), Vector2::new(10_000., 0.)];
        assert!(plan_road_drive(Vector2::new(0., 5_000.), Vector2::new(10_000., 0.), &road).is_none());
        assert!(plan_road_drive(Vector2::new(0., 0.), Vector2::new(10_000., 9_000.), &road).is_none());
        assert!(plan_road_drive(Vector2::new(0., 0.), Vector2::new(1., 1.), &[]).is_none());
    }
}
