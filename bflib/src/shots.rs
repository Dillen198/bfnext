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

//! Lets not bicker and argue about oo killed oo
use crate::db::{Db, group::DeployKind};
use anyhow::Result;
use bfprotocols::shots::{Dead, Shot, Who};
use chrono::{Duration, prelude::*};
use bfprotocols::cfg::UnitTag;
use dcso3::{
    String,
    event::Shot as ShotEvent,
    object::{DcsObject, DcsOid},
    unit::{ClassUnit, Unit, UnitCategory},
};
use fxhash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, Default)]
pub struct ShotDb {
    by_target: FxHashMap<DcsOid<ClassUnit>, Vec<Shot>>,
    dead: FxHashMap<DcsOid<ClassUnit>, DateTime<Utc>>,
    recently_dead: FxHashMap<DcsOid<ClassUnit>, DateTime<Utc>>,
    last_gc: DateTime<Utc>,
}

macro_rules! ok {
    ($r:expr) => {
        match $r {
            Ok(u) => u,
            Err(_) => return Ok(()),
        }
    };
}

macro_rules! some {
    ($o:expr) => {
        match $o {
            Some(u) => u,
            None => return Ok(()),
        }
    };
}

/// Public wrapper around `who` for callers outside the shot pipeline
/// (e.g. the slot-leave-under-threat handler).
pub fn who_for(db: &Db, id: DcsOid<ClassUnit>) -> Option<Who> {
    who(db, id)
}

fn who(db: &Db, id: DcsOid<ClassUnit>) -> Option<Who> {
    match db.ephemeral.get_uid_by_object_id(&id) {
        Some(uid) => db.unit(uid).ok().map(|u| Who::AI {
            side: u.side,
            gid: u.group,
            uid: *uid,
            unit: id,
            ucid: db.group(&u.group).ok().and_then(|g| match &g.origin {
                DeployKind::Action { player, .. } => *player,
                DeployKind::Deployed { player, .. } => Some(*player),
                DeployKind::Troop { player, .. } => Some(*player),
                DeployKind::Crate { .. }
                | DeployKind::Objective { .. }
                | DeployKind::ObjectiveDeprecated
                | DeployKind::Dismount { .. }
                | DeployKind::DownedPilot { .. } => None,
            }),
        }),
        None => db
            .ephemeral
            .get_slot_by_object_id(&id)
            .and_then(|sl| db.ephemeral.player_in_slot(sl).map(|ucid| (sl, ucid)))
            .and_then(|(sl, ucid)| db.player(ucid).map(|p| (sl, ucid, p)))
            .map(|(sl, ucid, p)| Who::Player {
                side: p.side,
                slot: *sl,
                ucid: *ucid,
                unit: id,
            }),
    }
}

impl ShotDb {
    pub fn dead(&mut self, target: DcsOid<ClassUnit>, time: DateTime<Utc>) {
        if let Entry::Vacant(e) = self.dead.entry(target) {
            e.insert(time);
        }
    }

    /// A player bailed out of a slot while airborne and under threat (an enemy
    /// aircraft close by). Credit that enemy with the kill: mark the unit dead
    /// and, only if nothing has already been recorded against it, attach a
    /// synthetic shot so `bring_out_your_dead` produces a `Dead`.
    pub fn abandoned_under_threat(
        &mut self,
        target_oid: DcsOid<ClassUnit>,
        shooter: Who,
        target: Who,
        shooter_typ: Option<String>,
        target_typ: String,
        time: DateTime<Utc>,
    ) {
        if self.recently_dead.contains_key(&target_oid) {
            return;
        }
        let entry = self.by_target.entry(target_oid.clone()).or_default();
        if entry.is_empty() {
            entry.push(Shot {
                weapon_name: Some(String::from("left slot under threat")),
                weapon: None,
                shooter,
                shooter_typ,
                target,
                target_typ,
                time,
                hit: true,
            });
        }
        self.dead.entry(target_oid).or_insert(time);
    }

    pub fn shot(&mut self, db: &Db, now: DateTime<Utc>, e: &ShotEvent) -> Result<()> {
        if db.ephemeral.cfg.weapon_target_exclusions.contains(&e.weapon_name) {
            return Ok(())
        }
        // Calling weapon.get_target() on a weapon that targets a ground point
        // rather than a unit object hard-crashes DCS inside
        // wAmmunitionGuided::Target_ID -- no Lua error, a straight access
        // violation that takes the whole server down. Ground units (artillery,
        // MLRS), ships firing FireAtPoint, and *ballistic weapons that fire a
        // second shot event with the rocket itself as initiator* (Scud /
        // Iskander cluster warheads -- initiator category then comes back as
        // something other than a real aircraft) all hit this.
        //
        // So this is an allow-list, not a deny-list: only proceed when the
        // initiator is unambiguously an Airplane or Helicopter AND we can
        // resolve it to a unit we actually know about. Anything else -- ground,
        // ship, structure, a weapon masquerading as a unit, an errored
        // category lookup -- bails before get_target() is ever called.
        let category = ok!(e.initiator.get_category());
        if category != UnitCategory::Airplane && category != UnitCategory::Helicopter {
            return Ok(());
        }
        let initiator_oid = ok!(e.initiator.object_id());
        let initiator_uid = some!(db.ephemeral.get_uid_by_object_id(&initiator_oid)).clone();
        let initiator_unit = ok!(db.unit(&initiator_uid));
        // Second line of defence for the crash the comment above describes.
        // `get_category()` on a China-Asset-Pack / modded ballistic launcher
        // (Scud_B, CHAP_9K720, ...) has been seen to come back as Airplane,
        // which slips a ground SSM shot past the category gate and straight
        // into the get_target() access violation that takes the whole server
        // down (`weapon_target_exclusions` only matches by display name and
        // missed e.g. "Scud R-17"). Surface-to-surface artillery and SSM/coastal
        // launchers fire at ground points, never at a unit we attribute a kill
        // to -- skip their shots outright. SAM launchers also carry `Launcher`
        // but they DO target aircraft, so keep tracking those.
        let itags = &initiator_unit.tags.0;
        if itags.contains(UnitTag::Artillery)
            || (itags.contains(UnitTag::Launcher) && !itags.contains(UnitTag::SAM))
        {
            return Ok(());
        }
        let target = ok!(some!(e.weapon.get_target()?).as_unit());
        let target_oid = target.object_id()?;
        if self.dead.contains_key(&target_oid) || self.recently_dead.contains_key(&target_oid) {
            return Ok(());
        }
        let shooter = some!(who(db, e.initiator.object_id()?));
        let shooter_typ = e.initiator.get_type_name().ok().map(|s| dcso3::String::from(s.as_str()));
        let target_typ = target.get_type_name()?;
        let target = some!(who(db, target_oid.clone()));
        self.by_target.entry(target_oid).or_default().push(Shot {
            weapon_name: Some(e.weapon_name.clone()),
            weapon: Some(e.weapon.object_id()?),
            shooter,
            shooter_typ,
            target,
            target_typ,
            time: now,
            hit: false,
        });
        Ok(())
    }

    pub fn hit(
        &mut self,
        db: &Db,
        now: DateTime<Utc>,
        dead: bool,
        target: &Unit,
        shooter: &Unit,
        weapon_name: String,
    ) -> Result<()> {
        let target_oid = target.object_id()?;
        if self.dead.contains_key(&target_oid) || self.recently_dead.contains_key(&target_oid) {
            return Ok(());
        }
        let target_typ = target.get_type_name()?;
        let shooter_typ = shooter.get_type_name().ok().map(|s| dcso3::String::from(s.as_str()));
        let shooter = some!(who(db, shooter.object_id()?));
        let target = some!(who(db, target_oid.clone()));
        self.by_target
            .entry(target_oid.clone())
            .or_default()
            .push(Shot {
                weapon_name: Some(weapon_name),
                weapon: None,
                shooter,
                shooter_typ,
                target,
                target_typ,
                time: now,
                hit: true,
            });
        if dead {
            self.dead.insert(target_oid, now);
        }
        Ok(())
    }

    pub fn bring_out_your_dead(&mut self, now: DateTime<Utc>) -> Vec<Dead> {
        let mut dead = Vec::with_capacity(self.dead.len());
        for (target, time) in self.dead.drain() {
            if let Some(shots) = self.by_target.remove(&target) {
                if shots.len() > 0 {
                    let victim = shots[0].target.clone();
                    dead.push(Dead {
                        victim,
                        time,
                        shots,
                    });
                }
            }
            self.recently_dead.insert(target, time);
        }
        const FIVE_MIN: Duration = Duration::minutes(5);
        const THIRTY_MIN: Duration = Duration::minutes(30);
        self.recently_dead.retain(|_, t| now - *t <= FIVE_MIN);
        if now - self.last_gc >= THIRTY_MIN {
            self.last_gc = now;
            self.by_target.retain(|_, shots| {
                shots.retain(|shot| now - shot.time <= THIRTY_MIN);
                !shots.is_empty()
            });
        }
        dead
    }
}
