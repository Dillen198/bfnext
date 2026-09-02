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
    group::{SpawnedGroup, SpawnedUnit},
    objective::Objective,
    player::Player,
    Map, MapM, MapS, Set, SetM, SetS,
};
use crate::navaids::Navaid;
use bfprotocols::db::{
    group::{GroupId, UnitId},
    objective::ObjectiveId,
};
use chrono::prelude::*;
use dcso3::{coalition::Side, net::Ucid, String};
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Persisted {
    pub groups: Map<GroupId, SpawnedGroup>,
    pub units: Map<UnitId, SpawnedUnit>,
    pub groups_by_name: Map<String, GroupId>,
    pub units_by_name: Map<String, UnitId>,
    pub groups_by_side: MapS<Side, Set<GroupId>>,
    pub deployed: SetM<GroupId>,
    pub farps: SetS<ObjectiveId>,
    pub crates: SetM<GroupId>,
    pub troops: SetM<GroupId>,
    pub jtacs: SetM<GroupId>,
    pub ewrs: SetS<GroupId>,
    #[serde(default)]
    pub actions: SetS<GroupId>,
    pub objectives: MapM<ObjectiveId, Objective>,
    pub objectives_by_name: MapM<String, ObjectiveId>,
    pub objectives_by_group: MapM<GroupId, ObjectiveId>,
    pub players: Map<Ucid, Player>,
    #[serde(default)]
    pub logistics_hubs: SetS<ObjectiveId>,
    #[serde(default)]
    pub naval_bases: SetS<ObjectiveId>,
    #[serde(default)]
    pub carrier_groups: SetS<ObjectiveId>,
    #[serde(default)]
    pub factories: SetS<ObjectiveId>,
    #[serde(default)]
    pub special_sam_sites: SetS<ObjectiveId>,
    #[serde(default)]
    pub command_centers: SetS<ObjectiveId>,
    /// SAM site -> its auto-linked nearest friendly CommandCenter (set at
    /// mission init, same pattern as CarrierGroup.parent_naval_base). A SAM
    /// site missing an entry here, or whose linked command center is dead
    /// or enemy-owned, loses IADN network cueing and falls back to plain
    /// DCS AI.
    #[serde(default)]
    pub sam_command_center_link: MapS<ObjectiveId, ObjectiveId>,
    #[serde(default)]
    pub downed_pilots: SetS<GroupId>,
    #[serde(default)]
    pub dismounts: SetS<GroupId>,
    /// Spawn UTC timestamp for each downed pilot (used for capture timer)
    #[serde(default)]
    pub downed_pilot_spawn_times: MapS<GroupId, DateTime<Utc>>,
    #[serde(default)]
    pub nukes_used: u32,
    #[serde(default)]
    pub logistics_ticks_since_delivery: u32,
    #[serde(default)]
    pub oid: i64,
    #[serde(default)]
    pub gid: i64,
    #[serde(default)]
    pub uid: i64,
    #[serde(default)]
    pub migrated_v0: bool,
    /// Coalition treasury balances (Smart Commander).
    #[serde(default)]
    pub blue_treasury: i64,
    #[serde(default)]
    pub red_treasury: i64,
    /// Auto-generated navaids per objective (see `crate::navaids`). Rebuilt
    /// deterministically whenever the objective set changes, so it's safe if
    /// this loads empty from an older save.
    #[serde(default)]
    pub navaids: MapS<ObjectiveId, Navaid>,
}

impl Persisted {
    pub fn players(&self) -> &Map<Ucid, Player> {
        &self.players
    }

    pub fn treasury(&self, side: Side) -> i64 {
        match side {
            Side::Blue => self.blue_treasury,
            Side::Red => self.red_treasury,
            _ => 0,
        }
    }

    pub fn adjust_treasury(&mut self, side: Side, delta: i64) -> i64 {
        let t = match side {
            Side::Blue => &mut self.blue_treasury,
            Side::Red => &mut self.red_treasury,
            _ => return 0,
        };
        *t = (*t + delta).max(0);
        *t
    }
}
