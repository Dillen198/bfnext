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

//! Ghost units: alive in the db, absent from DCS.
//!
//! The threat check in `cull_or_respawn_objectives` works off the db, not
//! DCS: any living enemy unit parked within ground cull distance of a base
//! marks it threatened, and a threatened base never repairs. A unit that
//! died without its Dead event reaching us, or whose spawn never happened,
//! stays "alive" next to that base for good -- nobody can see it, nobody can
//! kill it, and the base is frozen for the rest of the campaign.
//!
//! `reconcile_ghosts` finds those units on the slow tick (a living unit of a
//! group that should be in the world right now, with no DCS object mapped
//! to it), tries one respawn, and if that doesn't bring it back, retires it:
//! objective units are marked dead so the objective's health says what is
//! really there, player-side groups are deleted. Before either step DCS is
//! asked directly (`Unit.getByName`) whether each unit is there: one that is
//! was only missing its Birth event, so it is re-mapped, not touched. The
//! rest of this module is the admin/owner diagnostics for "why won't this
//! base repair".

use super::{Db, objective::Objective};
use crate::spawnctx::Despawn;
use anyhow::Result;
use bfprotocols::{
    cfg::{MATERIEL_ITEM, UnitTag},
    db::{
        group::{GroupId, UnitId},
        objective::ObjectiveId,
    },
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{
    MizLua, Vector2, azumith2d_to,
    coalition::Side,
    group::GroupCategory,
    object::{DcsObject, DcsOid},
    unit::{ClassUnit, Unit},
};
use enumflags2::BitFlags;
use fxhash::{FxHashMap, FxHashSet};
use log::{info, warn};
use smallvec::SmallVec;

/// How long a group has to stay a ghost before anything is done about it,
/// and again after its respawn attempt before it is retired. Long enough to
/// ride out spawn-queue lag, the Birth event arriving a few ticks late, and a
/// cull/respawn flip-flop at the edge of an objective's wake range.
const GHOST_GRACE_SECS: i64 = 180;
/// Nothing is touched this soon after the mission loads: the whole world is
/// being respawned from the save, and the spawn queue is throttled per frame.
const GHOST_LOAD_GRACE_SECS: i64 = 300;
/// How many ghost groups `ghosts` lists by name.
const GHOST_LIST_MAX: usize = 20;
/// How many threat units `whythreat` lists individually.
const THREAT_LIST_MAX: usize = 15;
/// Fallback threat radius for aircraft types missing from
/// `threatened_distance`, the same number `cull_or_respawn_objectives` uses.
const DEFAULT_THREAT_DIST: f64 = 14400.;

/// Why a group is expected to exist in DCS right now.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GhostClass {
    /// The owner's garrison of a spawned (woken) objective.
    Garrison,
    /// The previous owner's defenses at an objective, which are spawned at
    /// load and never culled.
    LeftBehind,
    Deployed,
    Troop,
    Dismount,
    Action,
}

impl GhostClass {
    pub fn label(self) -> &'static str {
        match self {
            Self::Garrison => "garrison",
            Self::LeftBehind => "left-behind",
            Self::Deployed => "deployed",
            Self::Troop => "troop",
            Self::Dismount => "dismount",
            Self::Action => "action",
        }
    }

    /// Player-side groups are deleted outright once they are given up on;
    /// objective groups have their units marked dead instead, because the
    /// group itself belongs to the objective and repair revives it.
    fn deletes_group(self) -> bool {
        matches!(self, Self::Deployed | Self::Troop | Self::Dismount | Self::Action)
    }
}

/// Where a group sits in the campaign, as far as ghost classification cares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Membership {
    Deployed,
    Troop,
    Dismount,
    Action,
    /// One of an objective's groups (`obj.groups[side]`).
    Objective {
        /// The group's side owns the objective.
        is_owner: bool,
        /// The group's side is the owner's enemy (not Neutral, not the owner).
        is_opposite: bool,
        owner_is_neutral: bool,
        /// The objective is woken (`obj.spawned`).
        spawned: bool,
        /// Carrier task forces and FARPs spawn outside the normal cull path.
        excluded_kind: bool,
        /// In a post-capture hold: garrison spawning is suspended.
        holding: bool,
        /// Services groups are spawned and swapped on their own schedule.
        services: bool,
    },
}

/// Everything `classify` looks at, pulled out so it can be tested without a
/// Lua state or a db.
#[derive(Debug, Clone, Copy)]
pub(super) struct GroupFacts {
    pub kind: Option<GroupCategory>,
    pub tags: BitFlags<UnitTag>,
    pub carrier_template: bool,
    pub membership: Membership,
}

/// Groups carrying any of these are never ghost candidates: aircraft (player
/// slots, AI air, CAP flights) and anything owned by a non-persisted
/// scheduler, whose lifecycle this module can't see.
fn never_ghost_tags() -> BitFlags<UnitTag> {
    UnitTag::Aircraft
        | UnitTag::Helicopter
        | UnitTag::CAP
        | UnitTag::HotStart
        | UnitTag::ColdStart
        | UnitTag::EventSpawn
        | UnitTag::AWACS
        | UnitTag::ALCM
        | UnitTag::NavalSpawnPoint
}

/// Whether a group is expected to be in the DCS world right now, and as what.
/// `None` means it isn't, or it's something this module doesn't know how to
/// judge -- either way it is left alone.
pub(super) fn classify(f: &GroupFacts) -> Option<GhostClass> {
    // Statics (None), aircraft and trains are out; ships only as objective
    // groups (naval base defenses), never as player deployables.
    let ground = match f.kind {
        Some(GroupCategory::Ground) => true,
        Some(GroupCategory::Ship) => false,
        _ => return None,
    };
    if f.carrier_template || f.tags.intersects(never_ghost_tags()) {
        return None;
    }
    match f.membership {
        Membership::Deployed if ground => Some(GhostClass::Deployed),
        Membership::Troop if ground => Some(GhostClass::Troop),
        Membership::Dismount if ground => Some(GhostClass::Dismount),
        Membership::Action if ground => Some(GhostClass::Action),
        Membership::Objective {
            is_owner,
            is_opposite,
            owner_is_neutral,
            spawned,
            excluded_kind,
            holding,
            services,
        } => {
            if excluded_kind || holding || services || owner_is_neutral {
                None
            } else if is_owner {
                // An unwoken objective's garrison is culled on purpose.
                spawned.then_some(GhostClass::Garrison)
            } else if is_opposite {
                Some(GhostClass::LeftBehind)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Per ghost group bookkeeping.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct GhostTrack {
    pub first_seen: DateTime<Utc>,
    /// When the one respawn attempt was queued.
    pub respawned: Option<DateTime<Utc>>,
    /// Retiring it was refused (it would wipe an objective's whole
    /// garrison); wait for an admin instead of retrying every tick.
    pub stuck: bool,
}

impl GhostTrack {
    fn new(now: DateTime<Utc>) -> Self {
        Self { first_seen: now, respawned: None, stuck: false }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum GhostStep {
    Wait,
    Respawn,
    Retire,
}

/// Advance one ghost group's grace-period state machine. `epoch` is when
/// reconciliation first ran after the mission loaded.
pub(super) fn ghost_step(
    track: &mut GhostTrack,
    now: DateTime<Utc>,
    epoch: DateTime<Utc>,
) -> GhostStep {
    let grace = chrono::Duration::seconds(GHOST_GRACE_SECS);
    if track.stuck || now - epoch < chrono::Duration::seconds(GHOST_LOAD_GRACE_SECS) {
        return GhostStep::Wait;
    }
    match track.respawned {
        None if now - track.first_seen >= grace => {
            track.respawned = Some(now);
            GhostStep::Respawn
        }
        Some(at) if now - at >= grace => GhostStep::Retire,
        _ => GhostStep::Wait,
    }
}

#[derive(Debug, Default)]
pub struct GhostTracker {
    epoch: Option<DateTime<Utc>>,
    groups: FxHashMap<GroupId, GhostTrack>,
}

/// A group with at least one ghost unit.
#[derive(Debug, Clone)]
pub(super) struct GhostGroup {
    pub gid: GroupId,
    pub class: GhostClass,
    pub side: Side,
    /// Living units with no DCS object.
    pub ghosts: SmallVec<[UnitId; 8]>,
    /// Every living unit in the group, ghosts included.
    pub alive: usize,
}

/// One enemy unit near an objective, for the threat diagnostics.
#[derive(Debug, Clone, Copy)]
struct ThreatUnit {
    uid: UnitId,
    dist: f64,
    air: bool,
    unarmed: bool,
    /// In `units_potentially_close_to_enemies`, i.e. the threat check sees it.
    counted: bool,
    /// Has an entry in `object_id_by_uid` (the engine thinks it's in DCS).
    mapped: bool,
}

/// Coarse distance for the owner-facing text: enough to go and look, no more.
pub(super) fn distance_band(m: f64) -> &'static str {
    if m < 1000. {
        "under 1 km"
    } else if m < 3000. {
        "1-3 km"
    } else if m < 5000. {
        "3-5 km"
    } else if m < 10000. {
        "5-10 km"
    } else {
        "10+ km"
    }
}

fn fmt_mins(secs: i64) -> CompactString {
    format_compact!("{}m", (secs.max(0) + 59) / 60)
}

impl Db {
    /// Every group that is expected in the DCS world and has a living unit
    /// that isn't. Groups with a spawn or despawn queued are skipped: they
    /// are between states, not ghosts.
    pub(super) fn scan_ghosts(&self, pending: &FxHashSet<GroupId>) -> Vec<GhostGroup> {
        let per = &self.persisted;
        // A squad holding a freshly captured base is judged by the capture
        // hold logic; retiring it here would end the hold as "left".
        let holding: FxHashSet<GroupId> = per
            .objectives
            .into_iter()
            .flat_map(|(_, o)| o.capture_hold.iter().copied())
            .collect();
        let mut out = vec![];
        let mut check = |gid: GroupId, membership: Membership| {
            if pending.contains(&gid) || holding.contains(&gid) {
                return;
            }
            let Some(group) = per.groups.get(&gid) else { return };
            let t = group.template_name.as_str();
            let facts = GroupFacts {
                kind: group.kind,
                tags: group.tags.0,
                carrier_template: t.starts_with("BCARRIER")
                    || t.starts_with("RCARRIER")
                    || t.starts_with("NCARRIER"),
                membership,
            };
            let Some(class) = classify(&facts) else { return };
            let mut ghosts: SmallVec<[UnitId; 8]> = SmallVec::new();
            let mut alive = 0;
            for uid in &group.units {
                let Some(unit) = per.units.get(uid) else { continue };
                if unit.dead {
                    continue;
                }
                alive += 1;
                if unit.tags.0.intersects(UnitTag::Aircraft | UnitTag::Helicopter) {
                    continue;
                }
                if !self.ephemeral.object_id_by_uid.contains_key(uid) {
                    ghosts.push(*uid);
                }
            }
            if !ghosts.is_empty() {
                out.push(GhostGroup { gid, class, side: group.side, ghosts, alive });
            }
        };
        for gid in &per.deployed {
            check(*gid, Membership::Deployed);
        }
        for gid in &per.troops {
            check(*gid, Membership::Troop);
        }
        for gid in &per.dismounts {
            check(*gid, Membership::Dismount);
        }
        for gid in &per.actions {
            check(*gid, Membership::Action);
        }
        for (_, obj) in &per.objectives {
            let excluded_kind = obj.kind.is_carrier_group() || obj.kind.is_farp();
            for (side, groups) in &obj.groups {
                for gid in groups {
                    let services = per
                        .groups
                        .get(gid)
                        .map(|g| g.class.is_services())
                        .unwrap_or(true);
                    check(
                        *gid,
                        Membership::Objective {
                            is_owner: *side == obj.owner,
                            is_opposite: *side != obj.owner
                                && *side != Side::Neutral
                                && *side == obj.owner.opposite(),
                            owner_is_neutral: obj.owner == Side::Neutral,
                            spawned: obj.spawned,
                            excluded_kind,
                            holding: !obj.capture_hold.is_empty(),
                            services,
                        },
                    );
                }
            }
        }
        out
    }

    /// Group name, side, unit type, position and nearest objective, for the
    /// log and the admin listing.
    fn describe_ghost(&self, g: &GhostGroup) -> CompactString {
        let name =
            self.persisted.groups.get(&g.gid).map(|g| g.name.as_str()).unwrap_or("?");
        let unit = g.ghosts.first().and_then(|uid| self.persisted.units.get(uid));
        let (typ, pos) = match unit {
            Some(u) => (u.typ.0.as_str(), u.pos),
            None => ("?", Vector2::default()),
        };
        let near = Db::objective_near_point(&self.persisted.objectives, pos, |_| true)
            .map(|(d, _, o)| format_compact!("{} ({:.1} km)", o.name, d / 1000.))
            .unwrap_or_else(|| CompactString::from("none"));
        format_compact!(
            "{name} ({}, {:?}, {typ}, {}/{} alive units missing) at ({:.0}, {:.0}) near {near}",
            g.class.label(),
            g.side,
            g.ghosts.len(),
            g.alive,
            pos.x,
            pos.y
        )
    }

    /// Living units across an objective's own garrison.
    fn garrison_alive(&self, oid: &ObjectiveId) -> usize {
        let Some(obj) = self.persisted.objectives.get(oid) else { return 0 };
        let Some(groups) = obj.groups.get(&obj.owner) else { return 0 };
        groups
            .into_iter()
            .filter_map(|gid| self.persisted.groups.get(gid))
            .flat_map(|g| g.units.into_iter())
            .filter(|uid| self.persisted.units.get(uid).map(|u| !u.dead).unwrap_or(false))
            .count()
    }

    /// Stop pretending a ghost group exists. Returns false (and does nothing)
    /// when that would wipe out an objective's whole garrison and `force`
    /// isn't set: that drops the base to Neutral, which is too big a call to
    /// make automatically on "DCS never told us about these units".
    fn retire_ghost(
        &mut self,
        g: &GhostGroup,
        now: DateTime<Utc>,
        force: bool,
    ) -> Result<bool> {
        let oid = self.persisted.objectives_by_group.get(&g.gid).copied();
        if g.class == GhostClass::Garrison
            && !force
            && let Some(oid) = oid
            && self.garrison_alive(&oid) <= g.ghosts.len()
        {
            return Ok(false);
        }
        if g.class.deletes_group() && g.ghosts.len() >= g.alive {
            // No refund: nothing the player paid for is being taken away
            // that DCS hadn't already taken.
            self.delete_group(&g.gid)?;
            return Ok(true);
        }
        for uid in &g.ghosts {
            if let Some(unit) = self.persisted.units.get_mut_cow(uid) {
                // Same reset as a real death (see `unit_dead`), so a repair
                // revives it at its post, not wherever the ghost wandered.
                unit.dead = true;
                unit.pos = unit.spawn_pos;
                unit.heading = unit.spawn_heading;
                unit.position = unit.spawn_position;
            }
            self.ephemeral.units_potentially_close_to_enemies.remove(uid);
            self.ephemeral.units_able_to_move.swap_remove(uid);
        }
        self.ephemeral.dirty();
        let alive_left = g.alive.saturating_sub(g.ghosts.len());
        if alive_left == 0 {
            if let Some((id, _)) = self.ephemeral.group_marks.remove(&g.gid) {
                self.ephemeral.msgs.delete_mark(id);
            }
            // Should there be an untracked DCS group under this name after
            // all, it goes too, so a later repair respawn doesn't double up.
            if let Some(group) = self.persisted.groups.get(&g.gid) {
                let name = group.name.to_string();
                self.ephemeral.push_despawn(g.gid, Despawn::GroupByName(name));
            }
        }
        if let Some(oid) = oid {
            self.update_objective_status(&oid, now)?;
        }
        Ok(true)
    }

    /// Ask DCS whether the db unit `uid` is really in the world, by name.
    /// The object-id map only ever learns about a unit from its Birth event,
    /// so a Birth we missed would otherwise read as a ghost; this is the
    /// ground truth the state machine checks before it acts.
    fn dcs_live_unit(&self, lua: MizLua, uid: &UnitId) -> Option<DcsOid<ClassUnit>> {
        let unit = self.persisted.units.get(uid)?;
        // getByName on a name DCS doesn't know returns nil, which the binding
        // reports as an error: that's "missing", not a failure.
        let dcs = Unit::get_by_name(lua, unit.name.as_str()).ok()?;
        if !dcs.is_exist().unwrap_or(false) {
            return None;
        }
        // Below 1 is a wreck by DCS's own rule (see `Unit::get_life`).
        if dcs.get_life().map(|l| l < 1.).unwrap_or(true) {
            return None;
        }
        dcs.object_id().ok()
    }

    /// Record a live DCS object for `uid` exactly as `unit_born` would have.
    fn remap_live_unit(&mut self, uid: UnitId, id: DcsOid<ClassUnit>) {
        self.ephemeral.uid_by_object_id.insert(id.clone(), uid);
        self.ephemeral.object_id_by_uid.insert(uid, id);
        self.ephemeral.units_potentially_close_to_enemies.insert(uid);
        if let Some(unit) = self.persisted.units.get(&uid)
            && (unit.tags.contains(UnitTag::Driveable)
                || unit.tags.contains(UnitTag::Boat))
        {
            self.ephemeral.units_able_to_move.insert(uid);
        }
    }

    /// Check a ghost group's units against DCS before acting on it. Units DCS
    /// has alive are re-mapped and dropped from `g.ghosts`; only the ones DCS
    /// reports missing are left. Returns true if nothing is missing any more.
    fn confirm_ghosts(&mut self, lua: MizLua, g: &mut GhostGroup) -> bool {
        let mut remapped = 0;
        let ghosts = std::mem::take(&mut g.ghosts);
        for uid in ghosts {
            match self.dcs_live_unit(lua, &uid) {
                Some(id) => {
                    self.remap_live_unit(uid, id);
                    remapped += 1;
                }
                None => g.ghosts.push(uid),
            }
        }
        if remapped > 0 {
            let name =
                self.persisted.groups.get(&g.gid).map(|g| g.name.as_str()).unwrap_or("?");
            info!(
                "[GHOST] {name} was live after all; re-mapped {remapped} unit(s), {} still missing",
                g.ghosts.len()
            );
        }
        g.ghosts.is_empty()
    }

    /// The slow-tick ghost pass. O(units in candidate groups); DCS is only
    /// asked about a candidate's units at the two decision points (respawn,
    /// retire), plus the respawn itself through the queue.
    pub fn reconcile_ghosts(&mut self, lua: MizLua, now: DateTime<Utc>) {
        let epoch = *self.ephemeral.ghosts.epoch.get_or_insert(now);
        let pending = self.ephemeral.spawn_pending_gids();
        let found = self.scan_ghosts(&pending);
        let found_ids: FxHashSet<GroupId> = found.iter().map(|g| g.gid).collect();
        // A group that is queued keeps its clock (the respawn attempt itself
        // puts it in the queue); one that came back or went away is forgotten.
        self.ephemeral
            .ghosts
            .groups
            .retain(|gid, _| found_ids.contains(gid) || pending.contains(gid));
        for mut g in found {
            let track = self
                .ephemeral
                .ghosts
                .groups
                .entry(g.gid)
                .or_insert_with(|| GhostTrack::new(now));
            let step = ghost_step(track, now, epoch);
            if step != GhostStep::Wait && self.confirm_ghosts(lua, &mut g) {
                // All there after all: no respawn, no retire, clock reset.
                self.ephemeral.ghosts.groups.remove(&g.gid);
                continue;
            }
            match step {
                GhostStep::Wait => (),
                GhostStep::Respawn => {
                    warn!(
                        "[GHOST] {} -- alive in the db, not in DCS for {}s; queueing a respawn",
                        self.describe_ghost(&g),
                        GHOST_GRACE_SECS
                    );
                    self.ephemeral.push_spawn(g.gid);
                }
                GhostStep::Retire => {
                    let what = self.describe_ghost(&g);
                    match self.retire_ghost(&g, now, false) {
                        Ok(true) => {
                            self.ephemeral.ghosts.groups.remove(&g.gid);
                            warn!(
                                "[GHOST] {what} -- still missing after a respawn; {}",
                                if g.class.deletes_group() && g.ghosts.len() >= g.alive {
                                    "group deleted"
                                } else {
                                    "units marked dead"
                                }
                            );
                        }
                        Ok(false) => {
                            if let Some(t) = self.ephemeral.ghosts.groups.get_mut(&g.gid)
                            {
                                t.stuck = true;
                            }
                            warn!(
                                "[GHOST] {what} -- still missing after a respawn, but it is the \
                                 objective's whole remaining garrison; NOT retiring it \
                                 automatically (that would drop the base to Neutral). \
                                 Use the admin command purge-ghosts to force it"
                            );
                        }
                        Err(e) => warn!("[GHOST] could not retire {what}: {e:?}"),
                    }
                }
            }
        }
    }

    /// `ghosts` admin command: current ghost candidates, per side counts
    /// first, then the first few by name.
    pub fn admin_ghost_report(&self) -> Vec<CompactString> {
        let pending = self.ephemeral.spawn_pending_gids();
        let found = self.scan_ghosts(&pending);
        let (mut blue, mut red, mut other) = (0usize, 0usize, 0usize);
        for g in &found {
            match g.side {
                Side::Blue => blue += g.ghosts.len(),
                Side::Red => red += g.ghosts.len(),
                Side::Neutral => other += g.ghosts.len(),
            }
        }
        let mut lines = vec![format_compact!(
            "{} ghost group(s): blue {blue} units, red {red} units, neutral {other} units",
            found.len()
        )];
        for g in found.iter().take(GHOST_LIST_MAX) {
            let age = self
                .ephemeral
                .ghosts
                .groups
                .get(&g.gid)
                .map(|t| {
                    let mut s = format_compact!(
                        " [ghost {}s",
                        (Utc::now() - t.first_seen).num_seconds()
                    );
                    if t.respawned.is_some() {
                        s.push_str(", respawn tried");
                    }
                    if t.stuck {
                        s.push_str(", held: whole garrison");
                    }
                    s.push(']');
                    s
                })
                .unwrap_or_default();
            lines.push(format_compact!("{}{age}", self.describe_ghost(g)));
        }
        if found.len() > GHOST_LIST_MAX {
            lines.push(format_compact!("... and {} more", found.len() - GHOST_LIST_MAX));
        }
        lines
    }

    /// `purge-ghosts` admin command: retire every current ghost now, skipping
    /// the grace periods and the whole-garrison guard.
    pub fn admin_purge_ghosts(
        &mut self,
        lua: MizLua,
        now: DateTime<Utc>,
    ) -> (usize, usize) {
        let pending = self.ephemeral.spawn_pending_gids();
        let found = self.scan_ghosts(&pending);
        let (mut groups, mut units) = (0, 0);
        for mut g in found {
            // Skipping the grace periods is fine; skipping the DCS check isn't.
            if self.confirm_ghosts(lua, &mut g) {
                self.ephemeral.ghosts.groups.remove(&g.gid);
                continue;
            }
            let what = self.describe_ghost(&g);
            match self.retire_ghost(&g, now, true) {
                Ok(_) => {
                    groups += 1;
                    units += g.ghosts.len();
                    self.ephemeral.ghosts.groups.remove(&g.gid);
                    warn!("[GHOST] admin purge: {what}");
                }
                Err(e) => warn!("[GHOST] admin purge could not retire {what}: {e:?}"),
            }
        }
        (groups, units)
    }

    /// Enemy db units near `obj`, mirroring the reach of the threat check in
    /// `cull_or_respawn_objectives`. With `all` every living enemy unit is
    /// considered (admin view); without it only the set the threat check
    /// actually reads, which keeps this cheap enough for menus.
    fn threat_units(&self, obj: &Objective, all: bool) -> SmallVec<[ThreatUnit; 16]> {
        let cfg = &self.ephemeral.cfg;
        let ground_cull = cfg.ground_vehicle_cull_distance as f64;
        let air_cull = cfg.unit_cull_distance as f64;
        let close = &self.ephemeral.units_potentially_close_to_enemies;
        let pos = obj.zone.pos();
        let mut out: SmallVec<[ThreatUnit; 16]> = SmallVec::new();
        let mut consider = |uid: &UnitId| {
            let Some(unit) = self.persisted.units.get(uid) else { return };
            if unit.dead || unit.side == obj.owner {
                return;
            }
            let air = unit.tags.0.intersects(UnitTag::Aircraft | UnitTag::Helicopter);
            let dist = na::distance(&pos.into(), &unit.pos.into());
            let reach = if air {
                let threat = cfg
                    .threatened_distance
                    .get(unit.typ.as_str())
                    .copied()
                    .map(|d| d as f64)
                    .unwrap_or(DEFAULT_THREAT_DIST);
                threat.min(air_cull)
            } else {
                ground_cull
            };
            if dist <= reach {
                out.push(ThreatUnit {
                    uid: *uid,
                    dist,
                    air,
                    unarmed: unit.tags.0.contains(UnitTag::Unarmed),
                    counted: close.contains(uid),
                    mapped: self.ephemeral.object_id_by_uid.contains_key(uid),
                });
            }
        };
        if all {
            for (uid, _) in &self.persisted.units {
                consider(uid);
            }
        } else {
            for uid in close {
                consider(uid);
            }
        }
        out.sort_by(|a, b| a.dist.total_cmp(&b.dist));
        out
    }

    /// Enemy players close enough to threaten `obj` (line of sight not
    /// checked): (name, type, distance in metres).
    fn threat_players(
        &self,
        obj: &Objective,
    ) -> SmallVec<[(CompactString, CompactString, f64); 4]> {
        let cfg = &self.ephemeral.cfg;
        let pos = obj.zone.pos();
        let mut out = SmallVec::new();
        for ucid in self.ephemeral.players_by_slot.values() {
            let Some(player) = self.persisted.players.get(ucid) else { continue };
            if player.side == obj.owner {
                continue;
            }
            let Some((_, Some(inst))) = player.current_slot.as_ref() else { continue };
            let p = Vector2::new(inst.position.p.x, inst.position.p.z);
            let dist = na::distance(&pos.into(), &p.into());
            let reach = cfg
                .threatened_distance
                .get(inst.typ.as_str())
                .copied()
                .map(|d| d as f64)
                .unwrap_or(DEFAULT_THREAT_DIST);
            if dist <= reach {
                out.push((
                    CompactString::from(player.name.as_str()),
                    CompactString::from(inst.typ.as_str()),
                    dist,
                ));
            }
        }
        out
    }

    /// For the base's own side: why a threatened base is frozen. Count and a
    /// distance band only -- these are units inside the owner's own wake
    /// range, but the exact position is left to their own eyes and sensors.
    /// `None` when the base isn't threatened.
    pub fn owner_threat_note(&self, oid: &ObjectiveId) -> Option<CompactString> {
        let obj = self.persisted.objectives.get(oid)?;
        if !obj.threatened {
            return None;
        }
        let units = self.threat_units(obj, false);
        let ground: SmallVec<[&ThreatUnit; 16]> =
            units.iter().filter(|u| u.counted && !u.air && !u.unarmed).collect();
        if let Some(nearest) = ground.first() {
            return Some(format_compact!(
                "FROZEN -- {} enemy ground unit(s) within {:.0} km, nearest {}; clear them to resume repairs",
                ground.len(),
                (self.ephemeral.cfg.ground_vehicle_cull_distance as f64 / 1000.).ceil(),
                distance_band(nearest.dist)
            ));
        }
        if units.iter().any(|u| u.counted && u.air)
            || !self.threat_players(obj).is_empty()
        {
            return Some(CompactString::from(
                "FROZEN -- enemy aircraft within threat range of the base",
            ));
        }
        // The threat has gone; `threatened` holds for the cooldown after it.
        let cooldown =
            chrono::Duration::seconds(self.ephemeral.cfg.threatened_cooldown as i64);
        let left = (obj.last_threatened_ts + cooldown - Utc::now()).num_seconds();
        Some(format_compact!(
            "FROZEN -- threat has cleared, repairs resume in ~{}",
            fmt_mins(left)
        ))
    }

    /// `whythreat <objective>` admin command: everything that stops the
    /// objective repairing, and every enemy db unit the threat check can see.
    pub fn admin_why_threat(&self, lua: MizLua, oid: &ObjectiveId) -> Vec<CompactString> {
        let Some(obj) = self.persisted.objectives.get(oid) else {
            return vec![CompactString::from("no such objective")];
        };
        let cfg = &self.ephemeral.cfg;
        let now = Utc::now();
        let mut lines = vec![format_compact!(
            "{} ({:?} {}): health {}% logi {}% supply {}% spawned {}",
            obj.name,
            obj.owner,
            obj.kind.name(),
            obj.health,
            obj.logi,
            obj.supply,
            obj.spawned
        )];
        let mut blockers: SmallVec<[CompactString; 6]> = SmallVec::new();
        if obj.owner == Side::Neutral {
            blockers.push("neutral: never self-repairs".into());
        }
        if !obj.capture_hold.is_empty() {
            blockers.push("in post-capture hold".into());
        }
        if obj.threatened {
            let cooldown = chrono::Duration::seconds(cfg.threatened_cooldown as i64);
            blockers.push(format_compact!(
                "THREATENED (last seen {}s ago, clears {}s after the threat leaves)",
                (now - obj.last_threatened_ts).num_seconds(),
                cooldown.num_seconds()
            ));
        }
        if let Some((side, since, _)) = self.ephemeral.capture_progress.get(oid) {
            blockers.push(format_compact!(
                "capture timer running for {side:?} ({}s)",
                (now - *since).num_seconds()
            ));
        }
        if obj.logi == 0 && !obj.kind.is_special_sam_site() {
            blockers.push("logi 0%: repair clock never elapses".into());
        }
        let materiel = cfg
            .warehouse
            .as_ref()
            .and_then(|w| w.materiel.as_ref())
            .filter(|m| m.enabled)
            .map(|m| m.repair_cost);
        match materiel {
            Some(cost) => {
                let have = obj
                    .warehouse
                    .equipment
                    .get(&dcso3::String::from(MATERIEL_ITEM))
                    .map(|inv| inv.stored)
                    .unwrap_or(0);
                if have < cost {
                    blockers.push(format_compact!(
                        "materiel {have} < {cost} per repaired group"
                    ));
                }
            }
            None => {
                if obj.supply < cfg.repair_supply_cost {
                    blockers.push(format_compact!(
                        "supply {}% < {}% repair cost",
                        obj.supply,
                        cfg.repair_supply_cost
                    ));
                }
            }
        }
        if obj.health >= 100 {
            lines.push("repair: at full strength".into());
        } else if blockers.is_empty() {
            let logi = if obj.kind.is_special_sam_site() {
                1.
            } else {
                (obj.logi as f32 / 100.).max(0.01)
            };
            let pulse = (cfg.repair_time as f32 / logi) as i64;
            let left = pulse - (now - obj.last_change_ts).num_seconds();
            lines.push(format_compact!(
                "repair: nothing blocking; next pulse in ~{}",
                fmt_mins(left)
            ));
        } else {
            let why: SmallVec<[&str; 6]> = blockers.iter().map(|b| b.as_str()).collect();
            lines.push(format_compact!("repair blocked: {}", why.join("; ")));
        }
        let units = self.threat_units(obj, true);
        // Liveness from DCS itself, not the object-id map: the map is what a
        // missed Birth or Dead event gets wrong, which is what this is for.
        let live: SmallVec<[bool; 16]> =
            units.iter().map(|u| self.dcs_live_unit(lua, &u.uid).is_some()).collect();
        let counted = units.iter().filter(|u| u.counted && !u.unarmed).count();
        let ghosts = live.iter().filter(|l| !**l).count();
        lines.push(format_compact!(
            "enemy db units in threat reach: {} ({} counted by the threat check, {} with no DCS object)",
            units.len(),
            counted,
            ghosts
        ));
        let pos = obj.zone.pos();
        for (u, live) in units.iter().zip(live.iter().copied()).take(THREAT_LIST_MAX) {
            let Some(unit) = self.persisted.units.get(&u.uid) else { continue };
            let gname = self
                .persisted
                .groups
                .get(&unit.group)
                .map(|g| g.name.as_str())
                .unwrap_or("?");
            let brg = azumith2d_to(pos, unit.pos).to_degrees().round() as i64 % 360;
            lines.push(format_compact!(
                "  {} grp {} {:?} {:.1} km brg {:03} {}{}{}",
                unit.typ.0,
                gname,
                unit.side,
                u.dist / 1000.,
                brg,
                // Where DCS and the map disagree, say so: that's the bug.
                match (live, u.mapped) {
                    (true, true) => "live",
                    (true, false) => "live (unmapped)",
                    (false, true) => "GHOST (still mapped)",
                    (false, false) => "GHOST",
                },
                if u.unarmed { " unarmed" } else { "" },
                if u.counted { "" } else { " (not in close set)" }
            ));
        }
        if units.len() > THREAT_LIST_MAX {
            lines.push(format_compact!(
                "  ... and {} more",
                units.len() - THREAT_LIST_MAX
            ));
        }
        for (name, typ, dist) in self.threat_players(obj) {
            lines.push(format_compact!("  player {name} ({typ}) {:.1} km", dist / 1000.));
        }
        lines
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn obj_facts(is_owner: bool, spawned: bool) -> Membership {
        Membership::Objective {
            is_owner,
            is_opposite: !is_owner,
            owner_is_neutral: false,
            spawned,
            excluded_kind: false,
            holding: false,
            services: false,
        }
    }

    fn facts(kind: Option<GroupCategory>, membership: Membership) -> GroupFacts {
        GroupFacts { kind, tags: BitFlags::empty(), carrier_template: false, membership }
    }

    #[test]
    fn classify_player_side_groups() {
        let g = Some(GroupCategory::Ground);
        assert_eq!(classify(&facts(g, Membership::Deployed)), Some(GhostClass::Deployed));
        assert_eq!(classify(&facts(g, Membership::Troop)), Some(GhostClass::Troop));
        assert_eq!(classify(&facts(g, Membership::Dismount)), Some(GhostClass::Dismount));
        assert_eq!(classify(&facts(g, Membership::Action)), Some(GhostClass::Action));
        // Air actions and statics are never candidates.
        assert_eq!(
            classify(&facts(Some(GroupCategory::Airplane), Membership::Action)),
            None
        );
        assert_eq!(
            classify(&facts(Some(GroupCategory::Helicopter), Membership::Troop)),
            None
        );
        assert_eq!(classify(&facts(None, Membership::Deployed)), None);
        // Ships only count as objective defenses.
        assert_eq!(
            classify(&facts(Some(GroupCategory::Ship), Membership::Deployed)),
            None
        );
    }

    #[test]
    fn classify_objective_groups() {
        let g = Some(GroupCategory::Ground);
        // A culled garrison is supposed to be missing from DCS.
        assert_eq!(classify(&facts(g, obj_facts(true, false))), None);
        assert_eq!(
            classify(&facts(g, obj_facts(true, true))),
            Some(GhostClass::Garrison)
        );
        // Left-behind defenses are spawned whether or not the base is awake.
        assert_eq!(
            classify(&facts(g, obj_facts(false, false))),
            Some(GhostClass::LeftBehind)
        );
        assert_eq!(
            classify(&facts(Some(GroupCategory::Ship), obj_facts(true, true))),
            Some(GhostClass::Garrison)
        );
        // A woken garrison with one disqualifying flag set.
        let with = |which: u8| {
            let mut m = obj_facts(true, true);
            if let Membership::Objective {
                excluded_kind,
                holding,
                services,
                owner_is_neutral,
                is_owner,
                is_opposite,
                ..
            } = &mut m
            {
                match which {
                    0 => *excluded_kind = true,
                    1 => *holding = true,
                    2 => *services = true,
                    3 => *owner_is_neutral = true,
                    _ => {
                        *is_owner = false;
                        *is_opposite = false;
                    }
                }
            }
            classify(&facts(g, m))
        };
        assert_eq!(with(0), None);
        assert_eq!(with(1), None);
        assert_eq!(with(2), None);
        assert_eq!(with(3), None);
        // Neutral-side groups at a Blue/Red base: not something we can judge.
        assert_eq!(with(4), None);
    }

    #[test]
    fn classify_skips_tagged_and_carriers() {
        let g = Some(GroupCategory::Ground);
        for tag in
            [UnitTag::CAP, UnitTag::EventSpawn, UnitTag::Aircraft, UnitTag::HotStart]
        {
            let f = GroupFacts {
                kind: g,
                tags: BitFlags::from(tag),
                carrier_template: false,
                membership: Membership::Deployed,
            };
            assert_eq!(classify(&f), None, "{tag:?}");
        }
        let f = GroupFacts {
            kind: Some(GroupCategory::Ship),
            tags: BitFlags::empty(),
            carrier_template: true,
            membership: obj_facts(true, true),
        };
        assert_eq!(classify(&f), None);
        // Tags that say nothing about lifecycle don't exclude.
        let f = GroupFacts {
            kind: g,
            tags: UnitTag::Armor | UnitTag::Driveable,
            carrier_template: false,
            membership: Membership::Troop,
        };
        assert_eq!(classify(&f), Some(GhostClass::Troop));
    }

    fn t(secs: i64) -> DateTime<Utc> {
        DateTime::<Utc>::from_timestamp(1_800_000_000 + secs, 0).unwrap()
    }

    #[test]
    fn grace_period_state_machine() {
        let epoch = t(0);
        // Seen at load: nothing happens inside the load grace, however old.
        let mut tr = GhostTrack::new(t(10));
        assert_eq!(ghost_step(&mut tr, t(200), epoch), GhostStep::Wait);
        assert_eq!(ghost_step(&mut tr, t(299), epoch), GhostStep::Wait);
        // Load grace over and seen for well past the grace: one respawn.
        assert_eq!(ghost_step(&mut tr, t(300), epoch), GhostStep::Respawn);
        assert_eq!(tr.respawned, Some(t(300)));
        // Then a full grace period for the respawn to land.
        assert_eq!(ghost_step(&mut tr, t(301), epoch), GhostStep::Wait);
        assert_eq!(ghost_step(&mut tr, t(479), epoch), GhostStep::Wait);
        assert_eq!(ghost_step(&mut tr, t(480), epoch), GhostStep::Retire);

        // Seen late: its own grace counts from first sight.
        let mut tr = GhostTrack::new(t(1000));
        assert_eq!(ghost_step(&mut tr, t(1179), epoch), GhostStep::Wait);
        assert_eq!(ghost_step(&mut tr, t(1180), epoch), GhostStep::Respawn);
        // Respawn is only ever tried once.
        assert_eq!(ghost_step(&mut tr, t(1200), epoch), GhostStep::Wait);

        // A refused retire waits for an admin instead of retrying.
        let mut tr = GhostTrack::new(t(1000));
        tr.respawned = Some(t(1180));
        tr.stuck = true;
        assert_eq!(ghost_step(&mut tr, t(5000), epoch), GhostStep::Wait);
    }

    #[test]
    fn distance_bands() {
        assert_eq!(distance_band(10.), "under 1 km");
        assert_eq!(distance_band(1500.), "1-3 km");
        assert_eq!(distance_band(4999.), "3-5 km");
        assert_eq!(distance_band(9000.), "5-10 km");
        assert_eq!(distance_band(25000.), "10+ km");
        assert_eq!(fmt_mins(0).as_str(), "0m");
        assert_eq!(fmt_mins(61).as_str(), "2m");
        assert_eq!(fmt_mins(-30).as_str(), "0m");
    }
}
