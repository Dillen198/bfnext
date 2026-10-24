// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Who is connected and what they are flying.
//!
//! The hooks state tells us ucid <-> player id <-> name (onPlayerTryConnect,
//! onPlayerChangeSlot); the mission state tells us which unit a player just
//! got into (BIRTH, `unit:getPlayerName()`). Both run on DCS's one Lua thread
//! and share the engine's context, so the join happens here.
//!
//! There is no slot gating on the range: every slot is open to everyone,
//! nothing costs points and there are no lives.

use crate::{bg, util::V3};
use bfprotocols::{
    cfg::{UnitTags, Vehicle},
    stats::{Stat, Unit as StatUnit},
};
use dcso3::{
    coalition::Side,
    env::miz::{GroupId, UnitId},
    net::{PlayerId, SlotId, Ucid},
};
use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone)]
pub struct Pilot {
    pub ucid: Ucid,
    pub name: String,
}

/// A human in an aircraft (or a Combined Arms vehicle) right now.
#[derive(Debug, Clone)]
pub struct Flying {
    pub ucid: Ucid,
    pub name: String,
    pub unit_name: String,
    pub unit_id: UnitId,
    pub group_name: String,
    pub group_id: GroupId,
    pub typ: String,
    pub side: Side,
    pub is_helo: bool,
    pub is_ground: bool,
    pub in_air: bool,
    pub pos: V3,
    pub vel: V3,
    /// true heading of the nose, degrees
    pub hdg: f64,
    pub alt_agl: f64,
    /// mission time of the last position refresh
    pub updated: f64,
    /// The missile trainer protects this player (default on).
    pub trainer: bool,
    /// What the engine thinks they're doing, for the live page.
    pub activity: Option<String>,
}

#[derive(Debug, Default)]
pub struct Players {
    by_id: FxHashMap<PlayerId, Pilot>,
    by_name: FxHashMap<String, Ucid>,
    /// unit name -> flying
    pub flying: FxHashMap<String, Flying>,
    registered: FxHashSet<Ucid>,
    /// players who turned the missile trainer off, by ucid (survives respawns)
    trainer_off: FxHashSet<Ucid>,
}

impl Players {
    pub fn connected(&mut self, id: PlayerId, ucid: Ucid, name: String, addr: String) {
        bg::send(bg::Task::Stat(Stat::Connect {
            id: ucid,
            addr: addr.as_str().into(),
            name: name.as_str().into(),
        }));
        self.by_name.insert(name.clone(), ucid);
        self.by_id.insert(id, Pilot { ucid, name });
    }

    /// Returns the ucid so the caller can clean up the player's spawns.
    pub fn disconnected(&mut self, id: PlayerId) -> Option<Ucid> {
        let p = self.by_id.remove(&id)?;
        bg::send(bg::Task::Stat(Stat::Disconnect { id: p.ucid }));
        if self.by_name.get(&p.name) == Some(&p.ucid) {
            self.by_name.remove(&p.name);
        }
        Some(p.ucid)
    }

    pub fn pilot_by_id(&self, id: PlayerId) -> Option<&Pilot> {
        self.by_id.get(&id)
    }

    pub fn ucid_by_name(&self, name: &str) -> Option<Ucid> {
        self.by_name.get(name).copied()
    }

    /// A player got into `unit`. Emits the identity stats bfdb uses to tie
    /// range results to the pilot's global profile.
    #[allow(clippy::too_many_arguments)]
    pub fn born(
        &mut self,
        ucid: Ucid,
        name: String,
        unit_name: String,
        unit_id: UnitId,
        group_name: String,
        group_id: GroupId,
        typ: String,
        side: Side,
        is_helo: bool,
        is_ground: bool,
        pos: V3,
        now: f64,
    ) {
        if self.registered.insert(ucid) {
            bg::send(bg::Task::Stat(Stat::Register {
                name: name.as_str().into(),
                id: ucid,
                side,
                initial_points: 0,
            }));
        }
        bg::send(bg::Task::Stat(Stat::Slot {
            id: ucid,
            slot: SlotId::from(unit_id),
            typ: Some(StatUnit {
                typ: Vehicle(typ.as_str().into()),
                tags: UnitTags(Default::default()),
            }),
        }));
        let trainer = !self.trainer_off.contains(&ucid);
        // one unit per player: drop any stale entry for this ucid
        self.flying.retain(|_, f| f.ucid != ucid);
        self.flying.insert(
            unit_name.clone(),
            Flying {
                ucid,
                name,
                unit_name,
                unit_id,
                group_name,
                group_id,
                typ,
                side,
                is_helo,
                is_ground,
                in_air: false,
                pos,
                vel: V3::zeros(),
                hdg: 0.,
                alt_agl: 0.,
                updated: now,
                trainer,
                activity: None,
            },
        );
    }

    /// The player left `unit_name` (death, slot change, disconnect).
    pub fn left(&mut self, unit_name: &str) -> Option<Flying> {
        let f = self.flying.remove(unit_name)?;
        bg::send(bg::Task::Stat(Stat::Deslot { id: f.ucid }));
        Some(f)
    }

    pub fn by_ucid(&self, ucid: &Ucid) -> Option<&Flying> {
        self.flying.values().find(|f| &f.ucid == ucid)
    }

    pub fn by_group(&self, gid: GroupId) -> Option<&Flying> {
        self.flying.values().find(|f| f.group_id == gid)
    }

    pub fn set_trainer(&mut self, ucid: Ucid, on: bool) {
        if on {
            self.trainer_off.remove(&ucid);
        } else {
            self.trainer_off.insert(ucid);
        }
        for f in self.flying.values_mut() {
            if f.ucid == ucid {
                f.trainer = on;
            }
        }
    }

    pub fn takeoff(&self, ucid: Ucid) {
        bg::send(bg::Task::Stat(Stat::Takeoff { id: ucid }));
    }

    pub fn land(&self, ucid: Ucid) {
        bg::send(bg::Task::Stat(Stat::Land { id: ucid }));
    }
}
