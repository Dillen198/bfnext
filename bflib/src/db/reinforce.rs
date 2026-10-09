//! The Reinforce action: a heavy-transport convoy that carries replacement
//! vehicles by road to a damaged friendly objective.
//!
//! - It sets off from the friendly objective nearest the one being
//!   reinforced (within `max_source_m`, not under attack, and able to spare
//!   `materiel_per_group` for each group it carries), on the road network.
//! - It is an ordinary action group: it counts against the action's `limit`,
//!   the enemy can find and kill it, and a restart puts it back on the road
//!   where it was.
//! - On arrival it rebuilds up to `groups` of the objective's destroyed
//!   garrison groups, armour first, in proportion to the transporters that
//!   survived the trip. Tractors are hitched to their trailers when it sets
//!   off (see `logistics::attach_trailers`); a tractor is the transporter,
//!   its trailer is only the load.
//! - A convoy that loses every transporter delivers nothing. One that is
//!   wedged on terrain far past its travel time is recalled, and the payer
//!   gets the points and the source its materiel back.

use super::{
    Db,
    actions::WithObj,
    group::DeployKind,
    logistics::{attach_trailers, road_route},
    objective::ObjGroupClass,
};
use crate::{
    group, objective,
    spawnctx::{SpawnCtx, SpawnLoc, Spawned},
    unit_mut,
};
use anyhow::{Context, Result, anyhow, bail};
use bfprotocols::{
    cfg::{Action, ActionKind, MATERIEL_ITEM, ReinforceCfg, UnitTag, default_trailer_pairs},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::PerfInner,
    stats::Stat,
};
use chrono::{Duration, prelude::*};
use compact_str::format_compact;
use dcso3::{String, Vector2, coalition::Side, env::miz::MizIndex, land::Land, net::Ucid};
use fxhash::{FxHashMap, FxHashSet};
use log::{error, info, warn};
use std::cmp::Reverse;

/// What a reinforcement convoy can put back together, most wanted first.
/// Logistics and services have their own repair; ships don't come by road.
const REBUILD_ORDER: [ObjGroupClass; 7] = [
    ObjGroupClass::Armor,
    ObjGroupClass::Infantry,
    ObjGroupClass::Aaa,
    ObjGroupClass::Sr,
    ObjGroupClass::Mr,
    ObjGroupClass::Lr,
    ObjGroupClass::Other,
];

/// Straight line to road distance, for the travel time estimate.
const ROAD_FACTOR: f64 = 1.4;

fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

enum Outcome {
    Driving,
    Arrived { alive: usize, total: usize },
    Destroyed,
    Stuck,
}

impl Db {
    fn trailer_pairs(&self) -> FxHashMap<String, String> {
        self.ephemeral
            .cfg
            .warehouse
            .as_ref()
            .and_then(|w| w.convoy.as_ref())
            .map(|c| c.trailer_pairs.clone())
            .unwrap_or_else(default_trailer_pairs)
    }

    /// The garrison groups at `oid` that have lost a unit and that a
    /// reinforcement convoy can rebuild, best first: by class in
    /// `REBUILD_ORDER`, then the most damaged.
    pub(crate) fn reinforce_candidates(&self, oid: &ObjectiveId) -> Result<Vec<GroupId>> {
        let obj = objective!(self, oid)?;
        let mut found = vec![];
        if let Some(groups) = obj.groups.get(&obj.owner) {
            for gid in groups {
                let group = group!(self, gid)?;
                let Some(rank) = REBUILD_ORDER.iter().position(|c| *c == group.class) else {
                    continue;
                };
                let dead = group
                    .units
                    .into_iter()
                    .filter(|uid| self.persisted.units.get(*uid).map_or(false, |u| u.dead))
                    .count();
                if dead > 0 {
                    found.push((rank, Reverse(dead), *gid));
                }
            }
        }
        found.sort_by_key(|(rank, dead, _)| (*rank, *dead));
        Ok(found.into_iter().map(|(_, _, gid)| gid).collect())
    }

    /// Put up to `n` of `oid`'s destroyed garrison groups back together.
    /// Returns how many were rebuilt.
    fn rebuild_garrison(&mut self, oid: ObjectiveId, n: usize, now: DateTime<Utc>) -> Result<usize> {
        let gids: Vec<GroupId> = self.reinforce_candidates(&oid)?.into_iter().take(n).collect();
        let spawned = objective!(self, oid)?.spawned;
        for gid in &gids {
            let group = group!(self, gid)?;
            for uid in &group.units {
                unit_mut!(self, uid)?.dead = false;
            }
            if spawned {
                self.ephemeral.push_spawn(*gid);
            }
        }
        if !gids.is_empty() {
            self.update_objective_status(&oid, now)?;
            self.ephemeral.dirty();
        }
        Ok(gids.len())
    }

    /// The reinforcement convoy already headed for `dest`, if there is one.
    fn reinforcements_bound_for(&self, dest: Vector2) -> Option<GroupId> {
        self.persisted.actions.into_iter().copied().find(|gid| {
            self.persisted.groups.get(gid).map_or(false, |g| {
                matches!(
                    &g.origin,
                    DeployKind::Action {
                        spec: Action { kind: ActionKind::Reinforce(_), .. },
                        destination: Some(d),
                        ..
                    } if dist(*d, dest) < 1.
                )
            })
        })
    }

    /// Start a Reinforce action: find the source, load the convoy and send
    /// it off.
    pub(super) fn reinforce(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithObj<ReinforceCfg>,
    ) -> Result<Option<GroupId>> {
        let cfg = args.cfg;
        let target = objective!(self, args.oid)?;
        let tname = target.name.clone();
        let tpos = target.zone.pos();
        if target.owner != side {
            bail!("{tname} is not ours to reinforce")
        }
        if matches!(target.kind, ObjectiveKind::CarrierGroup { .. }) {
            bail!("a carrier group can't be reinforced by road")
        }
        if self.reinforcements_bound_for(tpos).is_some() {
            bail!("reinforcements are already on the road to {tname}")
        }
        let groups = self.reinforce_candidates(&args.oid)?.len().min(cfg.groups as usize);
        if groups == 0 {
            bail!("{tname} has no destroyed ground units to replace")
        }
        let materiel = cfg.materiel_per_group.saturating_mul(groups as u32);
        let materiel_key = String::from(MATERIEL_ITEM);
        let stock = |o: &super::objective::Objective| {
            o.warehouse.equipment.get(&materiel_key).map_or(0, |i| i.stored)
        };
        let source = self
            .persisted
            .objectives
            .into_iter()
            .filter(|(id, o)| {
                **id != args.oid
                    && o.owner == side
                    && !o.threatened
                    && !matches!(o.kind, ObjectiveKind::CarrierGroup { .. })
                    && (materiel == 0 || stock(o) >= materiel)
            })
            .map(|(id, o)| (dist(o.zone.pos(), tpos), *id))
            .filter(|(d, _)| *d <= cfg.max_source_m as f64)
            .min_by(|(a, _), (b, _)| a.total_cmp(b))
            .map(|(_, id)| id)
            .ok_or_else(|| {
                let km = cfg.max_source_m / 1000;
                if materiel > 0 {
                    anyhow!(
                        "no friendly objective within {km} km of {tname} can send reinforcements \
                         (needs {materiel} materiel and not be under attack)"
                    )
                } else {
                    anyhow!(
                        "no friendly objective within {km} km of {tname} can send reinforcements \
                         (it must not be under attack)"
                    )
                }
            })?;
        let src = objective!(self, source)?;
        let (sname, spos) = (src.name.clone(), src.zone.pos());
        if materiel > 0 {
            if let Some(inv) = self
                .persisted
                .objectives
                .get_mut_cow(&source)
                .and_then(|o| o.warehouse.equipment.get_mut_cow(&materiel_key))
            {
                *inv -= materiel;
            }
        }
        let delta = tpos - spos;
        let dir = if delta.norm() > 1. { delta / delta.norm() } else { Vector2::new(1., 0.) };
        let loc = SpawnLoc::AtPos {
            pos: spos,
            offset_direction: dir,
            group_heading: dir.y.atan2(dir.x),
        };
        let origin = DeployKind::Action {
            marks: FxHashSet::default(),
            loc: loc.clone(),
            player: ucid,
            name: name.clone(),
            spec: action,
            time: Utc::now(),
            destination: Some(tpos),
            rtb: Some(spos),
            origin: Some(source),
            ammo: 0,
            jtac: None,
            // Filled in by start_action once the spawn has succeeded.
            paid_by: None,
            carried: groups as u32,
        };
        let spawned = self
            .add_group(spctx, idx, side, loc, &cfg.template, origin, UnitTag::Driveable.into())
            .context("creating the reinforcement convoy")
            .and_then(|gid| {
                match self.drive_reinforcements(perf, spctx, idx, gid, spos, tpos, cfg.speed_kph) {
                    Ok(km) => Ok((gid, km)),
                    Err(e) => {
                        if let Err(de) = self.delete_group(&gid) {
                            error!("could not remove unspawned reinforcement convoy {gid:?}: {de:?}");
                        }
                        Err(e)
                    }
                }
            });
        let (gid, km) = match spawned {
            Ok(r) => r,
            Err(e) => {
                self.return_materiel(source, materiel);
                return Err(e);
            }
        };
        let mins = (km / cfg.speed_kph.max(1.) * 60.).round();
        info!(
            "reinforce: {side:?} {name} {gid} {sname} -> {tname}, {groups} group(s), \
             {km:.0} km, ~{mins:.0} min"
        );
        self.ephemeral.msgs().panel_to_side(
            20,
            false,
            side,
            format_compact!(
                "Reinforcements for {tname} leaving {sname}: {groups} group(s) on transporters, \
                 {km:.0} km by road, about {mins:.0} min. Keep the road open."
            ),
        );
        Ok(Some(gid))
    }

    fn return_materiel(&mut self, source: ObjectiveId, amount: u32) {
        if amount == 0 {
            return;
        }
        if let Some(inv) = self
            .persisted
            .objectives
            .get_mut_cow(&source)
            .and_then(|o| o.warehouse.equipment.get_mut_cow(&String::from(MATERIEL_ITEM)))
        {
            *inv += amount;
            self.ephemeral.dirty();
        }
    }

    /// Spawn (or respawn) the convoy `gid` with a road route from `from` to
    /// `to` and hitch its trailers. Returns the route length in km.
    fn drive_reinforcements(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        gid: GroupId,
        from: Vector2,
        to: Vector2,
        speed_kph: f64,
    ) -> Result<f64> {
        let land = Land::singleton(spctx.lua())?;
        let route = road_route(
            &land,
            from,
            to,
            speed_kph.max(1.) / 3.6,
            &format_compact!("Reinforcements {gid}"),
        )?;
        let km = route.windows(2).map(|w| dist(w[1].pos.0, w[0].pos.0)).sum::<f64>() / 1000.;
        let pairs = self.trailer_pairs();
        let spawned = self
            .ephemeral
            .spawn_group(perf, &self.persisted, idx, spctx, group!(self, gid)?, route.clone())
            .context("spawning the reinforcement convoy")?;
        if let Some(Spawned::Group(g)) = spawned {
            match attach_trailers(&g, route, &pairs) {
                Ok(0) => (),
                Ok(n) => info!("reinforce: {gid} {n} trailer(s) hitched"),
                Err(e) => warn!("reinforce: {gid} could not hitch its trailers: {e:?}"),
            }
        }
        Ok(km)
    }

    /// After a restart: put the convoy back on the road from where it was.
    pub(super) fn respawn_reinforcements(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        gid: GroupId,
    ) -> Result<()> {
        let (dest, speed_kph) = match &group!(self, gid)?.origin {
            DeployKind::Action {
                spec: Action { kind: ActionKind::Reinforce(cfg), .. },
                destination: Some(dest),
                ..
            } => (*dest, cfg.speed_kph),
            _ => bail!("{gid} is not a reinforcement convoy on the road"),
        };
        let from = self.group_center(&gid)?;
        self.drive_reinforcements(perf, spctx, idx, gid, from, dest, speed_kph)?;
        info!("reinforce: {gid} back on the road after a restart");
        Ok(())
    }

    fn reinforce_outcome(&self, gid: GroupId, now: DateTime<Utc>) -> Result<Outcome> {
        let group = group!(self, gid)?;
        let DeployKind::Action {
            spec: Action { kind: ActionKind::Reinforce(cfg), .. },
            destination: Some(dest),
            rtb,
            time,
            ..
        } = &group.origin
        else {
            bail!("{gid} is not a reinforcement convoy on the road")
        };
        let pairs = self.trailer_pairs();
        let trailers: FxHashSet<&String> = pairs.values().collect();
        let units: Vec<_> = group
            .units
            .into_iter()
            .filter_map(|uid| self.persisted.units.get(uid))
            .filter(|u| !trailers.contains(&u.typ.0))
            .collect();
        // The tractors carry the load. A template without any (plain
        // trucks) carries it in every vehicle.
        let tractors: Vec<_> = units.iter().filter(|u| pairs.contains_key(&u.typ.0)).collect();
        let carriers: Vec<_> = if tractors.is_empty() { units.iter().collect() } else { tractors };
        let total = carriers.len();
        let alive = carriers.iter().filter(|u| !u.dead).count();
        if alive == 0 {
            return Ok(Outcome::Destroyed);
        }
        let arrive = cfg.arrive_m as f64;
        if carriers.iter().any(|u| !u.dead && dist(u.pos, *dest) <= arrive) {
            return Ok(Outcome::Arrived { alive, total });
        }
        let leg = rtb.map_or(0., |from| dist(from, *dest)) * ROAD_FACTOR;
        let expected = Duration::seconds((leg / (cfg.speed_kph.max(1.) / 3.6)) as i64);
        if now - *time > expected * 2 + Duration::minutes(30) {
            return Ok(Outcome::Stuck);
        }
        Ok(Outcome::Driving)
    }

    /// Settle every reinforcement convoy that has arrived, been destroyed or
    /// got stuck. Called from `advance_actions` with the Reinforce groups.
    pub(super) fn advance_reinforcements(&mut self, gids: &[GroupId], now: DateTime<Utc>) {
        for gid in gids {
            let outcome = match self.reinforce_outcome(*gid, now) {
                Ok(o) => o,
                Err(e) => {
                    error!("reinforce: {gid}: {e:?}");
                    continue;
                }
            };
            if matches!(outcome, Outcome::Driving) {
                continue;
            }
            if let Err(e) = self.settle_reinforcements(*gid, outcome, now) {
                error!("reinforce: settling {gid}: {e:?}");
            }
            if let Err(e) = self.delete_group(gid) {
                error!("reinforce: removing {gid}: {e:?}");
            }
        }
    }

    /// The whole convoy `gid` has been killed (every vehicle, trailers
    /// included, so the unit death path saw it before `advance_actions`
    /// did): report it and take it off the map.
    pub(super) fn reinforcements_lost(&mut self, gid: GroupId, now: DateTime<Utc>) {
        if let Err(e) = self.settle_reinforcements(gid, Outcome::Destroyed, now) {
            error!("reinforce: settling lost {gid}: {e:?}");
        }
        if let Err(e) = self.delete_group(&gid) {
            error!("reinforce: removing {gid}: {e:?}");
        }
    }

    fn settle_reinforcements(&mut self, gid: GroupId, outcome: Outcome, now: DateTime<Utc>) -> Result<()> {
        let group = group!(self, gid)?;
        let side = group.side;
        let DeployKind::Action {
            spec,
            destination: Some(dest),
            origin,
            player,
            paid_by,
            carried,
            name,
            ..
        } = &group.origin
        else {
            bail!("{gid} is not a reinforcement convoy on the road")
        };
        let ActionKind::Reinforce(cfg) = &spec.kind else {
            bail!("{gid} is not a reinforcement convoy")
        };
        let (dest, source, player, paid_by, carried, name) =
            (*dest, *origin, *player, *paid_by, *carried, name.clone());
        let (materiel_per_group, want, penalty) = (cfg.materiel_per_group, cfg.groups, spec.penalty);
        // The objective the convoy was sent to, whoever holds it now.
        let target = Self::objective_near_point(&self.persisted.objectives, dest, |_| true)
            .filter(|(d, _, _)| *d < 2_000.)
            .map(|(_, _, o)| (o.id, o.name.clone(), o.owner));
        let tname = target.as_ref().map_or_else(|| String::from("the front"), |t| t.1.clone());
        match outcome {
            Outcome::Driving => (),
            Outcome::Destroyed => {
                info!("reinforce: {side:?} {name} {gid} to {tname} destroyed");
                self.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side,
                    format_compact!(
                        "The reinforcement convoy for {tname} was destroyed on the road. Nothing arrives."
                    ),
                );
                self.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side.opposite(),
                    format_compact!("Enemy reinforcement convoy destroyed -- its armour never reaches the front."),
                );
                if let (Some(p), Some(ucid)) = (penalty, player) {
                    self.adjust_points(
                        &ucid,
                        -(p.min(i32::MAX as u32) as i32),
                        &format_compact!("for the loss of reinforcement convoy {gid}"),
                    );
                }
            }
            Outcome::Stuck => {
                warn!("reinforce: {side:?} {name} {gid} to {tname} stuck on the road, recalled");
                if let Some(source) = source {
                    self.return_materiel(source, materiel_per_group.saturating_mul(carried));
                }
                if let Some((ucid, amount)) = paid_by
                    && amount > 0
                {
                    self.adjust_points(
                        &ucid,
                        amount.min(i32::MAX as u32) as i32,
                        &format!("refund for {name} {gid}, stuck on the road"),
                    );
                }
                self.ephemeral.msgs().panel_to_side(
                    15,
                    false,
                    side,
                    format_compact!(
                        "The reinforcement convoy for {tname} is stuck and has been recalled. \
                         Points refunded."
                    ),
                );
            }
            Outcome::Arrived { alive, total } => match target {
                Some((oid, tname, owner)) if owner == side => {
                    // What made it: the load shrinks with every transporter lost.
                    let load = if carried > 0 { carried } else { want } as usize;
                    let n = (load * alive).div_ceil(total.max(1));
                    let rebuilt = self.rebuild_garrison(oid, n, now)?;
                    if let Some(ucid) = player {
                        self.ephemeral.stat(Stat::Repair { id: oid, by: ucid });
                    }
                    let lost = total - alive;
                    info!(
                        "reinforce: {side:?} {name} {gid} reached {tname}, rebuilt {rebuilt} \
                         group(s), {lost}/{total} transporter(s) lost"
                    );
                    let msg = match (rebuilt, lost) {
                        (0, _) => format_compact!(
                            "Reinforcements reached {tname}, but nothing there needed replacing."
                        ),
                        (r, 0) => format_compact!("Reinforcements reached {tname}: {r} group(s) back in the line."),
                        (r, l) => format_compact!(
                            "Reinforcements reached {tname}: {r} group(s) back in the line \
                             ({l} transporter(s) lost on the way)."
                        ),
                    };
                    self.ephemeral.msgs().panel_to_side(15, false, side, msg);
                }
                _ => {
                    info!("reinforce: {side:?} {name} {gid} arrived, but {tname} is no longer ours");
                    self.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        side,
                        format_compact!("{tname} fell before the reinforcements got there."),
                    );
                }
            },
        }
        Ok(())
    }
}
