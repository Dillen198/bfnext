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

//! Road fallback for AI helo troop insertions.
//!
//! A paid troop insertion used to end in a refund whenever the helo let the
//! player down -- it never started up, was shot down, or never got down at
//! the target. The player wanted troops on the objective, not their points
//! back, so now the same squad is driven in by road instead: one vehicle
//! group from the nearest friendly objective with a road to the target,
//! dismounting the squad (owned by the paying player, through the same
//! `paratroops_to_point` the helo uses) once it is there. Only when that
//! can't be done -- no road, no vehicle template, the vehicle is lost or
//! never arrives -- does the player get the refund.
//!
//! Legs are persisted but the vehicle is not respawned by a mission load, so
//! a restart refunds every leg still on the road
//! (`Db::reconcile_ground_insertions`) rather than leaving a paid squad on a
//! truck that no longer exists.

use super::{
    logistics::{HeloMission, HeloMissionId},
    Db,
};
use crate::{group, objective, spawnctx::{SpawnCtx, SpawnLoc}};
use anyhow::{anyhow, bail, Context, Result};
use bfprotocols::{
    cfg::HeloInsertionCfg,
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::Perf,
};
use chrono::{prelude::*, Duration};
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, net::Ucid, MizLua, String, Vector2};
use log::{info, warn};
use serde_derive::{Deserialize, Serialize};
use smallvec::SmallVec;
use std::sync::Arc;

/// How often a leg is polled; the helo missions use the same cadence.
const POLL_SECS: i64 = 10;
/// Movement smaller than this between polls is the vehicle standing still
/// (DCS ground units jitter a few metres while parked).
const STOPPED_MOVE_M: f64 = 25.;
/// Standing still this long close to the target counts as having arrived:
/// "On Road" vehicles often halt at the road point nearest the objective
/// rather than drive the last stretch.
const STOPPED_ARRIVE_SECS: i64 = 60;
/// Standing still this long anywhere else is a wedged vehicle; waiting out
/// the whole timeout for one just delays the refund.
const STUCK_GIVE_UP_SECS: i64 = 15 * 60;
/// How far off the road either end of the route may be. The vehicle drives
/// that bit cross-country, which is fine for a few km and hopeless for more.
const ROAD_ACCESS_M: f64 = 3000.;
/// Road queries per failed mission. `findPathOnRoads` is not free, and the
/// nearest few candidates are the only ones worth driving from anyway.
const MAX_ROAD_QUERIES: usize = 5;
/// Road polyline decimation, same numbers as the supply convoys: DCS returns
/// thousands of vertices and a route that big leaves the group sitting at
/// its origin.
const ROAD_WAYPOINT_SPACING_M: f64 = 3000.;
const MAX_ROAD_WAYPOINTS: usize = 60;
const MIN_TIMEOUT_SECS: i64 = 20 * 60;

/// A troop insertion whose helo failed, handed over from
/// `Db::tick_helo_missions` to be sent in by road.
#[derive(Debug, Clone)]
pub(super) struct FailedInsertion {
    pub helo_mission: HeloMissionId,
    pub player: Ucid,
    pub cost: i32,
    pub side: Side,
    pub destination: ObjectiveId,
    /// What went wrong with the helo, as the player would have been told it.
    pub why: CompactString,
}

/// Refund queue entry, same shape as `tick_helo_missions` keeps.
type Refund = (Ucid, i32, CompactString);

/// Route a failed helo mission: a troop insertion goes to the road
/// fallback, anything else gets the refund it always got. Called at each
/// place `tick_helo_missions` gives up on a helo.
pub(super) fn refund_or_divert(
    refunds: &mut SmallVec<[Refund; 2]>,
    diverted: &mut SmallVec<[FailedInsertion; 2]>,
    id: &HeloMissionId,
    mission: &HeloMission,
    why: CompactString,
) {
    use super::logistics::HeloMissionKind;
    match mission.kind {
        HeloMissionKind::TroopInsertion => diverted.push(FailedInsertion {
            helo_mission: id.clone(),
            player: mission.player,
            cost: mission.cost,
            side: mission.side,
            destination: mission.destination,
            why,
        }),
        HeloMissionKind::ResourceDelivery { .. } => refunds.push((mission.player, mission.cost, why)),
    }
}

/// A troop insertion on its way in by road. See the module docs.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroundInsertion {
    pub id: CompactString,
    /// The helo mission this replaced, for the log.
    #[serde(default)]
    pub helo_mission: CompactString,
    pub group_id: GroupId,
    pub side: Side,
    pub player: Ucid,
    /// What the player paid for the helo mission; refunded if this fails too.
    pub cost: i32,
    /// The squad, by name, as configured when the helo was called.
    pub troop: String,
    /// Where the vehicle set off from (the squad's origin for pickup rules).
    pub origin: ObjectiveId,
    pub destination: ObjectiveId,
    pub spawn_time: DateTime<Utc>,
    pub deadline: DateTime<Utc>,
    #[serde(default)]
    pub route_m: f64,
    pub last_pos: Vector2,
    pub last_check: DateTime<Utc>,
    /// Where the vehicle was when it last moved meaningfully, and when --
    /// the stopped/stuck detection.
    pub anchor_pos: Vector2,
    pub anchor_time: DateTime<Utc>,
}

/// What one poll of a leg decided.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LegStep {
    Driving,
    Arrived,
    Stuck,
    TimedOut,
}

/// Rank departure candidates nearest-first, dropping any past `max_range_m`
/// of `target`.
fn rank_departures<T>(
    candidates: impl IntoIterator<Item = (T, Vector2)>,
    target: Vector2,
    max_range_m: f64,
) -> Vec<(T, Vector2, f64)> {
    let mut v: Vec<(T, Vector2, f64)> = candidates
        .into_iter()
        .map(|(t, p)| {
            let d = (p - target).norm();
            (t, p, d)
        })
        .filter(|(_, _, d)| *d <= max_range_m)
        .collect();
    v.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap_or(std::cmp::Ordering::Equal));
    v
}

/// Carrier decks move and have no road to anywhere; everything else a side
/// owns can send a vehicle.
fn can_send_vehicle(kind: &ObjectiveKind) -> bool {
    !matches!(kind, ObjectiveKind::CarrierGroup { .. })
}

/// Whether a road polyline actually connects `from` to the target: DCS
/// happily returns a path between the road points nearest each end even
/// when those are 30 km away across a sea or a mountain range.
fn road_reaches(path: &[Vector2], from: Vector2, to: Vector2, to_radius: f64) -> bool {
    match (path.first(), path.last()) {
        (Some(first), Some(last)) => {
            (first - from).norm() <= ROAD_ACCESS_M
                && (last - to).norm() <= to_radius + ROAD_ACCESS_M
        }
        _ => false,
    }
}

/// Thin a road polyline to a waypoint about every `ROAD_WAYPOINT_SPACING_M`
/// (always keeping the last point, the road exit nearest the target), capped
/// at `MAX_ROAD_WAYPOINTS`. "On Road" makes DCS follow the road between them.
fn decimate_road(path: &[Vector2]) -> SmallVec<[Vector2; 64]> {
    let mut out: SmallVec<[Vector2; 64]> = SmallVec::new();
    for (i, p) in path.iter().enumerate() {
        let far_enough = out
            .last()
            .map(|l| (p - l).norm() >= ROAD_WAYPOINT_SPACING_M)
            .unwrap_or(true);
        let is_last = i + 1 == path.len();
        if (far_enough || is_last) && out.len() < MAX_ROAD_WAYPOINTS {
            out.push(*p);
        }
    }
    out
}

/// How long a leg may take: `configured` minutes if set, else twice the
/// planned drive plus 10 minutes (DCS ground AI queues at junctions and
/// bridges), never under 20.
fn leg_timeout_secs(route_m: f64, speed_mps: f64, configured: Option<u32>) -> i64 {
    match configured {
        Some(m) if m > 0 => m as i64 * 60,
        _ => {
            let drive = if speed_mps > 0. { route_m / speed_mps } else { 0. };
            ((drive * 2.) as i64 + 10 * 60).max(MIN_TIMEOUT_SECS)
        }
    }
}

/// Distance from `pos` to the edge of a zone (0 inside). `radius` is the
/// zone's bounding radius, so this errs short for a quad -- fine, since the
/// squad is walked into the zone on dismount regardless.
fn dist_to_zone(pos: Vector2, center: Vector2, radius: f64, inside: bool) -> f64 {
    if inside {
        0.
    } else {
        ((pos - center).norm() - radius).max(0.)
    }
}

/// Move the anchor if the vehicle has gone somewhere since it was set, and
/// return how long it has been standing still.
fn observe_motion(
    anchor: &mut (Vector2, DateTime<Utc>),
    pos: Vector2,
    now: DateTime<Utc>,
) -> i64 {
    if (pos - anchor.0).norm() > STOPPED_MOVE_M {
        *anchor = (pos, now);
    }
    (now - anchor.1).num_seconds()
}

fn judge(to_zone_m: f64, stopped_secs: i64, timed_out: bool, arrive_m: f64) -> LegStep {
    if to_zone_m <= arrive_m || (stopped_secs >= STOPPED_ARRIVE_SECS && to_zone_m <= 2. * arrive_m) {
        LegStep::Arrived
    } else if timed_out {
        LegStep::TimedOut
    } else if stopped_secs >= STUCK_GIVE_UP_SECS {
        LegStep::Stuck
    } else {
        LegStep::Driving
    }
}

/// Where the squad gets out: at the vehicle if that is inside the zone,
/// otherwise the nearest point on the way from the vehicle to the zone
/// centre that is -- troops outside the zone capture nothing, and the helo
/// path makes the same call when it lands outside.
fn dismount_point(vehicle: Vector2, center: Vector2, contains: impl Fn(Vector2) -> bool) -> Vector2 {
    if contains(vehicle) {
        return vehicle;
    }
    const STEPS: u32 = 20;
    (1..=STEPS)
        .map(|i| vehicle + (center - vehicle) * (i as f64 / STEPS as f64))
        .find(|p| contains(*p))
        .unwrap_or(center)
}

/// Poll a vehicle group: its lead vehicle's position, or `None` if it is gone.
fn vehicle_pos(lua: MizLua, group_name: &str) -> Option<Vector2> {
    use dcso3::group::Group;
    let group = Group::get_by_name(lua, group_name).ok()?;
    let units = group.get_units().ok()?;
    if units.len() == 0 {
        return None;
    }
    let pos = units.get(1).ok()?.get_point().ok()?;
    Some(Vector2::new(pos.x, pos.z))
}

impl Db {
    /// Send every failed troop insertion in by road; the ones that can't go
    /// join `refunds`, with why, for `tick_helo_missions` to pay out as usual.
    pub(super) fn divert_to_road(
        &mut self,
        lua: MizLua,
        failed: SmallVec<[FailedInsertion; 2]>,
        refunds: &mut SmallVec<[Refund; 2]>,
        now: DateTime<Utc>,
    ) {
        let Some(cfg) = self.ephemeral.cfg.helo_insertion.clone() else {
            refunds.extend(failed.into_iter().map(|f| (f.player, f.cost, f.why)));
            return;
        };
        for f in failed {
            if !cfg.ground_fallback.enabled {
                refunds.push((f.player, f.cost, f.why));
                continue;
            }
            match self.start_ground_insertion(lua, &f, &cfg, now) {
                Ok(msg) => self.ephemeral.panel_to_player(&self.persisted, 20, &f.player, msg),
                Err(e) => {
                    info!(
                        "[HELO_GROUND] {} could not go by road, refunding: {e:#}",
                        f.helo_mission
                    );
                    refunds.push((
                        f.player,
                        f.cost,
                        format_compact!("{}; the squad can't go by road either: {e}", f.why),
                    ));
                }
            }
        }
    }

    /// Pick the departure, spawn the vehicle with its road route baked in and
    /// record the leg. Returns the message for the player. Errors are worded
    /// for the player too -- they end up in the refund message.
    fn start_ground_insertion(
        &mut self,
        lua: MizLua,
        f: &FailedInsertion,
        cfg: &HeloInsertionCfg,
        now: DateTime<Utc>,
    ) -> Result<CompactString> {
        use crate::db::group::DeployKind;
        use dcso3::{
            controller::{ActionTyp, AltType, MissionPoint, PointType, Task, VehicleFormation},
            env::miz::Miz,
            land::{Land, RoadType},
            LuaVec2,
        };
        use enumflags2::BitFlags;

        let g = &cfg.ground_fallback;
        let side = f.side;
        let dest = objective!(self, &f.destination).map_err(|_| anyhow!("the target is gone"))?;
        let (dest_pos, dest_radius, dest_name) = (dest.pos(), dest.zone.radius(), dest.name.clone());
        let template = g
            .template
            .get(&side)
            .or_else(|| {
                self.ephemeral
                    .cfg
                    .warehouse
                    .as_ref()
                    .and_then(|w| w.convoy.as_ref())
                    .and_then(|c| c.truck_template.get(&side))
            })
            .cloned()
            .ok_or_else(|| anyhow!("no road transport is set up for {side:?}"))?;

        let ranked = rank_departures(
            self.persisted
                .objectives
                .into_iter()
                .filter(|(oid, o)| {
                    o.owner == side && **oid != f.destination && can_send_vehicle(&o.kind)
                })
                .map(|(oid, o)| (*oid, o.pos())),
            dest_pos,
            g.max_range_m,
        );
        if ranked.is_empty() {
            bail!(
                "nothing of ours within {:.0} km of {dest_name}",
                g.max_range_m / 1000.
            );
        }
        let land = Land::singleton(lua).context("land singleton")?;
        let mut found = None;
        for (oid, from, _) in ranked.iter().take(MAX_ROAD_QUERIES) {
            let name = self
                .persisted
                .objectives
                .get(oid)
                .map(|o| o.name.clone())
                .unwrap_or_default();
            match land.find_path_on_roads(RoadType::Road, LuaVec2(*from), LuaVec2(dest_pos)) {
                Ok(seq) => {
                    let pts: Vec<Vector2> = seq.into_iter().filter_map(|p| p.ok()).map(|p| p.0).collect();
                    if road_reaches(&pts, *from, dest_pos, dest_radius) {
                        found = Some((*oid, *from, name, pts));
                        break;
                    }
                    info!(
                        "[HELO_GROUND] {}: no usable road {name} -> {dest_name} ({} road points)",
                        f.helo_mission,
                        pts.len()
                    );
                }
                Err(e) => info!(
                    "[HELO_GROUND] {}: road query {name} -> {dest_name} failed: {e}",
                    f.helo_mission
                ),
            }
        }
        let Some((origin, from, origin_name, road)) = found else {
            bail!(
                "nothing of ours within {:.0} km has a road to {dest_name}",
                g.max_range_m / 1000.
            );
        };

        let speed_mps = g.speed_kph.max(1.) / 3.6;
        let spawn_ctx = SpawnCtx::new(lua).context("spawn ctx")?;
        let miz = Miz::singleton(lua).context("miz singleton")?;
        let idx = miz.index().context("miz index")?;
        let delta = dest_pos - from;
        let heading = delta.y.atan2(delta.x);
        let group_id = self
            .add_group(
                &spawn_ctx,
                &idx,
                side,
                SpawnLoc::AtPos {
                    pos: from,
                    offset_direction: {
                        let n = delta.norm();
                        if n > 1. { delta / n } else { Vector2::new(1., 0.) }
                    },
                    group_heading: heading,
                },
                &template,
                DeployKind::Objective { origin },
                BitFlags::empty(),
            )
            .with_context(|| format_compact!("the vehicle template '{template}' would not spawn"))?;

        let point = |pos: Vector2, formation: VehicleFormation, first: bool| MissionPoint {
            action: Some(ActionTyp::Ground(formation)),
            airdrome_id: None,
            helipad: None,
            typ: PointType::TurningPoint,
            link_unit: None,
            pos: LuaVec2(pos),
            alt: land.get_height(LuaVec2(pos)).unwrap_or(0.),
            alt_typ: Some(AltType::BARO),
            time_re_fu_ar: None,
            eta: first.then_some(dcso3::Time(0.)),
            eta_locked: first.then_some(true),
            speed: speed_mps,
            speed_locked: first.then_some(true),
            name: None,
            task: Box::new(Task::ComboTask(vec![])),
        };
        let mut route = vec![point(from, VehicleFormation::OnRoad, true)];
        route.extend(decimate_road(&road).into_iter().map(|p| point(p, VehicleFormation::OnRoad, false)));
        // Off road for the last stretch: "On Road" stops at the road point
        // nearest the objective, which can be well outside its zone.
        route.push(point(dest_pos, VehicleFormation::OffRoad, false));
        let route_m: f64 = route.windows(2).map(|w| (w[1].pos.0 - w[0].pos.0).norm()).sum();
        let waypoints = route.len();

        let spawned = {
            let perf = unsafe { Perf::get_mut() };
            let perf = Arc::make_mut(&mut perf.inner);
            self.ephemeral.spawn_group(
                perf,
                &self.persisted,
                &idx,
                &spawn_ctx,
                group!(self, group_id)?,
                route,
            )
        };
        if let Err(e) = spawned {
            // Don't leave a record for a vehicle that never appeared.
            let _ = self.delete_group(&group_id);
            return Err(e.context(format_compact!("the vehicle '{template}' would not spawn")));
        }

        let timeout = leg_timeout_secs(route_m, speed_mps, g.timeout_mins);
        let eta_mins = ((route_m / speed_mps) / 60.).ceil().max(1.) as i64;
        let id = format_compact!("GROUND_{}", f.helo_mission);
        let leg = GroundInsertion {
            id: id.clone(),
            helo_mission: f.helo_mission.clone(),
            group_id,
            side,
            player: f.player,
            cost: f.cost,
            troop: cfg.troop_name.clone(),
            origin,
            destination: f.destination,
            spawn_time: now,
            deadline: now + Duration::seconds(timeout),
            route_m,
            last_pos: from,
            last_check: now,
            anchor_pos: from,
            anchor_time: now,
        };
        self.persisted.ground_insertions.insert_cow(id.clone(), leg);
        self.ephemeral.dirty();
        info!(
            "[HELO_GROUND] {id}: {} failed ({}); '{template}' driving {origin_name} -> {dest_name}, \
             {:.1} km by road over {waypoints} waypoints, ETA {eta_mins} min, gives up in {} min",
            f.helo_mission,
            f.why,
            route_m / 1000.,
            timeout / 60
        );
        Ok(format_compact!(
            "{} -- sending the squad to {dest_name} by road from {origin_name} instead, ETA ~{eta_mins} min",
            f.why
        ))
    }

    /// Poll the road legs: dismount the squads that have arrived, refund the
    /// ones that were lost, wedged or ran out of time. Same slow tick as
    /// `tick_helo_missions`.
    pub fn tick_ground_insertions(&mut self, lua: MizLua, now: DateTime<Utc>) -> Result<()> {
        if self.persisted.ground_insertions.len() == 0 {
            return Ok(());
        }
        let (arrive_m, refund_on_loss) = match self.ephemeral.cfg.helo_insertion.as_ref() {
            Some(h) => (h.ground_fallback.arrive_m, h.refund_on_loss),
            None => (default_arrive_m(), true),
        };
        let ids: SmallVec<[CompactString; 4]> =
            self.persisted.ground_insertions.into_iter().map(|(id, _)| id.clone()).collect();
        // (leg, vehicle position) to dismount; (leg, why) to refund.
        let mut arrived: SmallVec<[(GroundInsertion, Vector2); 2]> = SmallVec::new();
        let mut failed: SmallVec<[(GroundInsertion, CompactString); 2]> = SmallVec::new();
        for id in ids {
            let Some(mut leg) = self.persisted.ground_insertions.get(&id).cloned() else {
                continue;
            };
            if (now - leg.last_check).num_seconds() < POLL_SECS {
                continue;
            }
            leg.last_check = now;
            let Some(dest) = self.persisted.objectives.get(&leg.destination) else {
                failed.push((leg, format_compact!("its target no longer exists")));
                continue;
            };
            let dest_name = dest.name.clone();
            let pos = group!(self, leg.group_id)
                .ok()
                .and_then(|g| vehicle_pos(lua, g.name.as_str()));
            let Some(pos) = pos else {
                info!(
                    "[HELO_GROUND] {} destroyed {:.1} km short of {dest_name}",
                    leg.id,
                    (leg.last_pos - dest.pos()).norm() / 1000.
                );
                failed.push((leg, format_compact!("the vehicle carrying the squad was destroyed")));
                continue;
            };
            leg.last_pos = pos;
            let mut anchor = (leg.anchor_pos, leg.anchor_time);
            let stopped = observe_motion(&mut anchor, pos, now);
            (leg.anchor_pos, leg.anchor_time) = anchor;
            let to_zone = dist_to_zone(pos, dest.pos(), dest.zone.radius(), dest.zone.contains(pos));
            match judge(to_zone, stopped, now > leg.deadline, arrive_m) {
                LegStep::Driving => {
                    // Tracking only; not worth a save of its own.
                    self.persisted.ground_insertions.insert_cow(id, leg);
                }
                LegStep::Arrived => {
                    let at = dismount_point(pos, dest.pos(), |p| dest.zone.contains(p));
                    info!(
                        "[HELO_GROUND] {} arrived at {dest_name} ({:.0}m from the zone, stopped {stopped}s), \
                         dismounting {:.0}m from the vehicle",
                        leg.id,
                        to_zone,
                        (at - pos).norm()
                    );
                    arrived.push((leg, at));
                }
                LegStep::TimedOut => {
                    info!(
                        "[HELO_GROUND] {} timed out {:.1} km short of {dest_name}",
                        leg.id,
                        to_zone / 1000.
                    );
                    let mins = (leg.deadline - leg.spawn_time).num_minutes();
                    failed.push((
                        leg,
                        format_compact!("the vehicle didn't reach {dest_name} in {mins} min"),
                    ));
                }
                LegStep::Stuck => {
                    info!(
                        "[HELO_GROUND] {} stuck for {} min, {:.1} km short of {dest_name}",
                        leg.id,
                        stopped / 60,
                        to_zone / 1000.
                    );
                    failed.push((
                        leg,
                        format_compact!("the vehicle got stuck {:.1} km short of {dest_name}", to_zone / 1000.),
                    ));
                }
            }
        }

        if !arrived.is_empty() {
            let miz = dcso3::env::miz::Miz::singleton(lua)?;
            let idx = miz.index()?;
            for (leg, at) in arrived {
                self.end_ground_leg(&leg);
                let dest_name = self.objective_name(leg.destination);
                let origin_name = self.objective_name(leg.origin);
                match self.paratroops_to_point(lua, &idx, at, leg.troop.clone(), leg.side, leg.player, leg.origin) {
                    Ok(()) => {
                        info!("[HELO_GROUND] {} squad dismounted at {dest_name}", leg.id);
                        self.ephemeral.panel_to_player(
                            &self.persisted,
                            20,
                            &leg.player,
                            format_compact!(
                                "your squad dismounted at {dest_name} after the drive from {origin_name} -- keep them alive in the zone to capture it"
                            ),
                        );
                    }
                    Err(e) => {
                        warn!("[HELO_GROUND] {} squad could not be deployed at {dest_name}: {e:?}", leg.id);
                        self.refund_ground_leg(
                            &leg,
                            format_compact!("the squad could not be deployed: {e}"),
                            refund_on_loss,
                        );
                    }
                }
            }
        }
        for (leg, why) in failed {
            self.end_ground_leg(&leg);
            self.refund_ground_leg(&leg, why, refund_on_loss);
        }
        Ok(())
    }

    /// Mission load: the vehicles of any legs still on the road were not
    /// respawned, so hand the points back and clear them out.
    pub(super) fn reconcile_ground_insertions(&mut self) {
        let legs: SmallVec<[GroundInsertion; 4]> =
            self.persisted.ground_insertions.into_iter().map(|(_, l)| l.clone()).collect();
        if legs.is_empty() {
            return;
        }
        let refund = self
            .ephemeral
            .cfg
            .helo_insertion
            .as_ref()
            .map_or(true, |h| h.refund_on_loss);
        for leg in &legs {
            self.end_ground_leg(leg);
            if refund && leg.cost > 0 {
                self.adjust_points(&leg.player, leg.cost, "road troop insertion cut off by a restart");
            }
            info!(
                "[HELO_GROUND] {} cut off by the mission restart{}",
                leg.id,
                if refund && leg.cost > 0 {
                    format_compact!(", refunded {} points", leg.cost)
                } else {
                    CompactString::default()
                }
            );
        }
    }

    /// Drop a leg and its vehicle, if the vehicle is still on the books.
    fn end_ground_leg(&mut self, leg: &GroundInsertion) {
        self.persisted.ground_insertions.remove_cow(&leg.id);
        if self.persisted.groups.get(&leg.group_id).is_some() {
            if let Err(e) = self.delete_group(&leg.group_id) {
                warn!("[HELO_GROUND] {} could not remove its vehicle group: {e:?}", leg.id);
            }
        }
        self.ephemeral.dirty();
    }

    /// Tell the player the road leg failed and why, refunding like the helo
    /// missions do (`refund_on_loss`, nothing to refund on a free mission).
    fn refund_ground_leg(&mut self, leg: &GroundInsertion, why: CompactString, refund_on_loss: bool) {
        let dest_name = self.objective_name(leg.destination);
        let msg = format_compact!("road insertion to {dest_name} failed: {why}");
        if refund_on_loss && leg.cost > 0 {
            info!("[HELO_GROUND] {} failed ({why}), refunding {} points", leg.id, leg.cost);
            self.ephemeral.panel_to_player(
                &self.persisted,
                20,
                &leg.player,
                format_compact!("{msg} -- refunding {} points", leg.cost),
            );
            self.adjust_points(&leg.player, leg.cost, &msg);
        } else {
            info!("[HELO_GROUND] {} failed ({why}), nothing to refund", leg.id);
            self.ephemeral.panel_to_player(&self.persisted, 20, &leg.player, msg);
        }
    }

    fn objective_name(&self, oid: ObjectiveId) -> CompactString {
        self.persisted
            .objectives
            .get(&oid)
            .map(|o| CompactString::from(o.name.as_str()))
            .unwrap_or_else(|| "the objective".into())
    }
}

fn default_arrive_m() -> f64 {
    bfprotocols::cfg::HeloGroundFallbackCfg::default().arrive_m
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(x: f64, y: f64) -> Vector2 {
        Vector2::new(x, y)
    }

    #[test]
    fn departures_nearest_first_within_range() {
        let r = rank_departures([("far", v(50_000., 0.)), ("near", v(0., 10_000.)), ("out", v(70_000., 0.))], v(0., 0.), 60_000.);
        let names: Vec<_> = r.iter().map(|(n, _, _)| *n).collect();
        assert_eq!(names, ["near", "far"]);
        assert_eq!(r[0].2, 10_000.);
    }

    #[test]
    fn carriers_never_send_a_vehicle() {
        assert!(can_send_vehicle(&ObjectiveKind::Fob));
        assert!(can_send_vehicle(&ObjectiveKind::Logistics));
        assert!(!can_send_vehicle(&ObjectiveKind::CarrierGroup {
            carrier_template: "CVN".into(),
            waypoint: None,
            parent_naval_base: None,
            repair_start_time: None,
        }));
    }

    #[test]
    fn a_road_has_to_start_and_end_near_the_places() {
        let (from, to) = (v(0., 0.), v(20_000., 0.));
        assert!(road_reaches(&[v(500., 0.), v(19_000., 0.)], from, to, 1000.));
        // Road ends 10 km short of the target: a different valley.
        assert!(!road_reaches(&[v(500., 0.), v(10_000., 0.)], from, to, 1000.));
        // Nearest road to the departure is on the other side of a range.
        assert!(!road_reaches(&[v(8_000., 0.), v(19_900., 0.)], from, to, 1000.));
        assert!(!road_reaches(&[], from, to, 1000.));
    }

    #[test]
    fn road_is_thinned_but_keeps_its_end() {
        let path: Vec<Vector2> = (0..=100).map(|i| v(i as f64 * 100., 0.)).collect();
        let d = decimate_road(&path);
        assert_eq!(d.first(), Some(&v(0., 0.)));
        assert_eq!(d.last(), Some(&v(10_000., 0.)));
        assert_eq!(d.len(), 5); // 0, 3k, 6k, 9k, 10k
        let long: Vec<Vector2> = (0..=1000).map(|i| v(i as f64 * 3000., 0.)).collect();
        assert_eq!(decimate_road(&long).len(), MAX_ROAD_WAYPOINTS);
    }

    #[test]
    fn timeout_scales_with_the_drive_and_has_a_floor() {
        // 30 km at 50 km/h is 36 min: 72 + 10.
        assert_eq!(leg_timeout_secs(30_000., 50. / 3.6, None), 82 * 60);
        assert_eq!(leg_timeout_secs(2_000., 50. / 3.6, None), MIN_TIMEOUT_SECS);
        assert_eq!(leg_timeout_secs(30_000., 50. / 3.6, Some(45)), 45 * 60);
        assert_eq!(leg_timeout_secs(30_000., 0., None), MIN_TIMEOUT_SECS);
    }

    #[test]
    fn motion_anchor_only_moves_on_real_movement() {
        let t0 = Utc::now();
        let mut a = (v(0., 0.), t0);
        assert_eq!(observe_motion(&mut a, v(10., 0.), t0 + Duration::seconds(30)), 30);
        assert_eq!(observe_motion(&mut a, v(5., 5.), t0 + Duration::seconds(70)), 70);
        assert_eq!(observe_motion(&mut a, v(200., 0.), t0 + Duration::seconds(80)), 0);
        assert_eq!(a.0, v(200., 0.));
    }

    #[test]
    fn arrival_close_or_stopped_near_otherwise_timeout_or_stuck() {
        assert_eq!(judge(300., 0, false, 400.), LegStep::Arrived);
        assert_eq!(judge(700., 30, false, 400.), LegStep::Driving);
        assert_eq!(judge(700., 60, false, 400.), LegStep::Arrived);
        assert_eq!(judge(900., 600, false, 400.), LegStep::Driving);
        assert_eq!(judge(900., STUCK_GIVE_UP_SECS, false, 400.), LegStep::Stuck);
        assert_eq!(judge(5000., 0, true, 400.), LegStep::TimedOut);
        // Arriving beats running out of time on the same poll.
        assert_eq!(judge(100., 0, true, 400.), LegStep::Arrived);
    }

    #[test]
    fn zone_distance() {
        assert_eq!(dist_to_zone(v(5000., 0.), v(0., 0.), 1000., false), 4000.);
        assert_eq!(dist_to_zone(v(500., 0.), v(0., 0.), 1000., true), 0.);
    }

    #[test]
    fn squad_steps_into_the_zone() {
        let center = v(0., 0.);
        let inside = |p: Vector2| (p - center).norm() <= 1000.;
        assert_eq!(dismount_point(v(300., 0.), center, inside), v(300., 0.));
        let at = dismount_point(v(1400., 0.), center, inside);
        assert!(inside(at));
        assert!((at - v(1400., 0.)).norm() < 500., "as close to the vehicle as it can");
        assert_eq!(dismount_point(v(1400., 0.), center, |_| false), center);
    }
}
