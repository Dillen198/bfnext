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

extern crate nalgebra as na;
use self::{group::DeployKind, persisted::Persisted};
use crate::{bg::Task, db::ephemeral::Ephemeral, jtac::JtId};
use anyhow::{Result, anyhow, bail};
use bfprotocols::{
    cfg::{
        Action, ActionKind, AwacsCfg, Cfg, Deployable, DeployableEwr, DeployableJtac, DroneCfg,
        Troop,
    },
    db::{
        group::{GroupId, UnitId},
        objective::ObjectiveId,
    },
};
use dcso3::{
    Vector3, centroid3d,
    coalition::Side,
    env::miz::{Miz, MizIndex},
};
use std::{cmp::max, fs::File, path::Path, sync::Arc};
use tokio::sync::mpsc::UnboundedSender;

pub mod actions;
pub mod cargo;
pub mod ephemeral;
pub mod events;
pub mod group;
pub mod logistics;
pub mod markup;
pub mod mizinit;
pub mod objective;
pub mod persisted;
pub mod player;

pub type Map<K, V> = immutable_chunkmap::map::Map<K, V, 256>;
pub type MapM<K, V> = immutable_chunkmap::map::Map<K, V, 64>;
pub type MapS<K, V> = immutable_chunkmap::map::Map<K, V, 16>;

pub type Set<K> = immutable_chunkmap::set::Set<K, 256>;
pub type SetM<K> = immutable_chunkmap::set::Set<K, 64>;
pub type SetS<K> = immutable_chunkmap::set::Set<K, 16>;

pub struct JtDesc {
    pub pos: Vector3,
    pub id: JtId,
    pub side: Side,
    pub spec: DeployableJtac,
    pub air: bool,
}

#[macro_export]
macro_rules! maybe {
    ($t:expr, $id:expr, $name:expr) => {
        $t.get(&$id)
            .ok_or_else(|| anyhow!("no such {} {:?}", $name, $id))
    };
}

#[macro_export]
macro_rules! maybe_mut {
    ($t:expr, $id:expr, $name:expr) => {
        $t.get_mut_cow(&$id)
            .ok_or_else(|| anyhow!("no such {} {:?}", $name, $id))
    };
}

#[macro_export]
macro_rules! unit {
    ($t:expr, $id:expr) => {
        $t.persisted
            .units
            .get(&$id)
            .ok_or_else(|| anyhow!("no such unit {:?}", $id))
    };
}

#[macro_export]
macro_rules! unit_mut {
    ($t:expr, $id:expr) => {
        $t.persisted
            .units
            .get_mut_cow(&$id)
            .ok_or_else(|| anyhow!("no such unit {:?}", $id))
    };
}

#[macro_export]
macro_rules! unit_by_name {
    ($t:expr, $name:expr) => {
        $t.persisted
            .units_by_name
            .get($name)
            .and_then(|id| $t.persisted.units.get(id))
            .ok_or_else(|| anyhow!("no such unit {}", $name))
    };
}

#[macro_export]
macro_rules! group {
    ($t:expr, $id:expr) => {
        $t.persisted
            .groups
            .get(&$id)
            .ok_or_else(|| anyhow!("no such group {:?}", $id))
    };
}

#[macro_export]
macro_rules! group_mut {
    ($t:expr, $id:expr) => {
        $t.persisted
            .groups
            .get_mut_cow(&$id)
            .ok_or_else(|| anyhow!("no such group {:?}", $id))
    };
}

#[macro_export]
macro_rules! group_by_name {
    ($t:expr, $name:expr) => {
        $t.persisted
            .groups_by_name
            .get($name)
            .and_then(|id| $t.persisted.groups.get(id))
            .ok_or_else(|| anyhow!("no such group {}", $name))
    };
}

#[macro_export]
macro_rules! objective {
    ($t:expr, $id:expr) => {
        $t.persisted
            .objectives
            .get(&$id)
            .ok_or_else(|| anyhow!("no such objective {:?}", $id))
    };
}

#[macro_export]
macro_rules! objective_mut {
    ($t:expr, $id:expr) => {
        $t.persisted
            .objectives
            .get_mut_cow(&$id)
            .ok_or_else(|| anyhow!("no such objective {:?}", $id))
    };
}

#[macro_export]
macro_rules! group_health {
    ($t:expr, $gid:expr) => {{
        let group = group!($t, $gid)?;
        let mut alive = 0;
        for uid in &group.units {
            if !unit!($t, uid)?.dead {
                alive += 1;
            }
        }
        Ok::<_, anyhow::Error>((alive, group.units.len()))
    }};
}

#[derive(Debug, Default)]
pub struct Db {
    pub persisted: Persisted,
    pub ephemeral: Ephemeral,
}

impl Db {
    pub fn load(
        miz: &Miz,
        idx: &MizIndex,
        to_bg: UnboundedSender<Task>,
        cfg: Arc<Cfg>,
        path: &Path,
    ) -> Result<Self> {
        let file = File::open(&path)
            .map_err(|e| anyhow!("failed to open save file {:?}, {:?}", path, e))?;
        let file = zstd::stream::Decoder::new(file)?;
        let persisted: Persisted = serde_json::from_reader(file)
            .map_err(|e| anyhow!("failed to decode save file {:?}, {:?}", path, e))?;
        let mut db = Db {
            persisted,
            ephemeral: Ephemeral::default(),
        };
        ObjectiveId::setseq(max(db.persisted.oid, ObjectiveId::seq()));
        GroupId::setseq(max(db.persisted.gid, GroupId::seq()));
        UnitId::setseq(max(db.persisted.uid, UnitId::seq()));
        db.ephemeral.set_cfg(miz, idx, cfg, to_bg)?;
        Ok(db)
    }

    pub fn maybe_snapshot(&mut self) -> Option<Persisted> {
        if self.ephemeral.take_dirty() {
            self.persisted.oid = ObjectiveId::seq();
            self.persisted.gid = GroupId::seq();
            self.persisted.uid = UnitId::seq();
            Some(self.persisted.clone())
        } else {
            None
        }
    }

    /// Get convoy origin/destination if a group ID belongs to an active supply convoy
    pub fn convoy_info_for_group(&self, gid: &GroupId) -> Option<(ObjectiveId, ObjectiveId)> {
        self.ephemeral.active_convoys.values()
            .find(|c| c.group_id == *gid)
            .map(|c| (c.origin, c.destination))
    }

    /// Count active supply convoys for a given side
    pub fn convoy_count_for_side(&self, side: dcso3::coalition::Side) -> usize {
        self.ephemeral.active_convoys.values().filter(|c| c.side == side).count()
    }

    /// Create a new planned mission
    pub fn create_mission(
        &mut self,
        name: compact_str::CompactString,
        mission_type: bfprotocols::db::mission::MissionType,
        creator: dcso3::net::Ucid,
        side: dcso3::coalition::Side,
        briefing: compact_str::CompactString,
    ) -> bfprotocols::db::mission::MissionId {
        use bfprotocols::db::mission::*;
        let id = MissionId::new();
        let mission = PlannedMission {
            id,
            name,
            mission_type,
            creator,
            side,
            created_at: chrono::Utc::now(),
            scheduled_time: None,
            status: MissionStatus::Planning,
            flights: Vec::new(),
            target_pos: None,
            briefing,
            rewarded: false,
        };
        self.ephemeral.planned_missions.push(mission);
        id
    }

    /// Get active missions for a side
    #[allow(dead_code)]
    pub fn missions_for_side(&self, side: dcso3::coalition::Side) -> Vec<&bfprotocols::db::mission::PlannedMission> {
        use bfprotocols::db::mission::MissionStatus;
        self.ephemeral.planned_missions.iter()
            .filter(|m| m.side == side && !matches!(m.status, MissionStatus::Completed | MissionStatus::Cancelled))
            .collect()
    }

    /// Join a player to a mission
    pub fn join_mission(
        &mut self,
        mission_id: bfprotocols::db::mission::MissionId,
        ucid: dcso3::net::Ucid,
    ) -> Result<()> {
        let mission = self.ephemeral.planned_missions.iter_mut()
            .find(|m| m.id == mission_id)
            .ok_or_else(|| anyhow!("mission not found"))?;
        if mission.flights.is_empty() {
            mission.flights.push(bfprotocols::db::mission::FlightPlan {
                callsign: compact_str::format_compact!("Flight 1"),
                aircraft_type: compact_str::format_compact!("Any"),
                num_aircraft: 1,
                role: mission.mission_type,
                waypoints: Vec::new(),
                assigned_players: vec![ucid],
                loadout_suggestion: None,
            });
        } else if let Some(flight) = mission.flights.first_mut() {
            if !flight.assigned_players.contains(&ucid) {
                flight.assigned_players.push(ucid);
            }
        }
        Ok(())
    }

    /// Cancel a mission (only creator can cancel)
    pub fn cancel_mission(
        &mut self,
        mission_id: bfprotocols::db::mission::MissionId,
        ucid: &dcso3::net::Ucid,
    ) -> Result<()> {
        let mission = self.ephemeral.planned_missions.iter_mut()
            .find(|m| m.id == mission_id)
            .ok_or_else(|| anyhow!("mission not found"))?;
        if mission.creator != *ucid {
            bail!("only the mission creator can cancel");
        }
        mission.status = bfprotocols::db::mission::MissionStatus::Cancelled;
        Ok(())
    }

    /// Mark a mission complete (creator only). Sets status to Completed,
    /// sets rewarded=true, and returns (side, mission_type, name, participants).
    pub fn complete_mission(
        &mut self,
        mission_id: bfprotocols::db::mission::MissionId,
        ucid: &dcso3::net::Ucid,
    ) -> Result<(Side, bfprotocols::db::mission::MissionType, compact_str::CompactString, Vec<dcso3::net::Ucid>)> {
        use bfprotocols::db::mission::MissionStatus;
        let mission = self.ephemeral.planned_missions.iter_mut()
            .find(|m| m.id == mission_id)
            .ok_or_else(|| anyhow!("mission not found"))?;
        if mission.creator != *ucid {
            bail!("only the mission creator can complete it");
        }
        if mission.rewarded {
            bail!("mission has already been rewarded");
        }
        mission.status = MissionStatus::Completed;
        mission.rewarded = true;
        let side = mission.side;
        let mt = mission.mission_type;
        let name = mission.name.clone();
        let players: Vec<dcso3::net::Ucid> = mission.flights.iter()
            .flat_map(|f| f.assigned_players.iter().copied())
            .collect();
        Ok((side, mt, name, players))
    }

    pub fn ewrs(&self) -> impl Iterator<Item = (Vector3, Side, &DeployableEwr)> {
        self.persisted.ewrs.into_iter().filter_map(|gid| {
            let group = self.persisted.groups.get(gid)?;
            match &group.origin {
                DeployKind::Crate { .. }
                | DeployKind::Objective { .. }
                | DeployKind::ObjectiveDeprecated
                | DeployKind::Troop { .. }
                | DeployKind::DownedPilot { .. } => None,
                DeployKind::Action {
                    spec:
                        Action {
                            kind: ActionKind::Awacs(AwacsCfg { ewr, .. }),
                            ..
                        },
                    ..
                }
                | DeployKind::Deployed {
                    spec: Deployable { ewr: Some(ewr), .. },
                    ..
                } => {
                    let pos = centroid3d(
                        group
                            .units
                            .into_iter()
                            .map(|u| self.persisted.units[u].position.p.0),
                    );
                    Some((pos, group.side, ewr))
                }
                DeployKind::Action { .. } | DeployKind::Deployed { .. } => None,
            }
        })
    }

    pub fn jtacs<'a>(&'a self) -> impl Iterator<Item = JtDesc> + 'a {
        self.persisted
            .jtacs
            .into_iter()
            .filter_map(|gid| {
                let group = self.persisted.groups.get(gid)?;
                let pos = centroid3d(
                    group
                        .units
                        .into_iter()
                        .filter_map(|u| self.persisted.units.get(u).map(|u| u.position.p.0)),
                );
                match &group.origin {
                    DeployKind::Troop {
                        spec:
                            Troop {
                                jtac: Some(jtac), ..
                            },
                        ..
                    }
                    | DeployKind::Deployed {
                        spec:
                            Deployable {
                                jtac: Some(jtac), ..
                            },
                        ..
                    } => Some(JtDesc {
                        pos,
                        id: JtId::Group(*gid),
                        side: group.side,
                        spec: *jtac,
                        air: false,
                    }),
                    DeployKind::Action {
                        spec:
                            Action {
                                kind: ActionKind::Drone(DroneCfg { jtac, .. }),
                                ..
                            },
                        ..
                    } => Some(JtDesc {
                        pos,
                        id: JtId::Group(*gid),
                        side: group.side,
                        spec: *jtac,
                        air: true,
                    }),
                    DeployKind::Crate { .. }
                    | DeployKind::Action { .. }
                    | DeployKind::Objective { .. }
                    | DeployKind::ObjectiveDeprecated
                    | DeployKind::Troop { .. }
                    | DeployKind::Deployed { .. }
                    | DeployKind::DownedPilot { .. } => None,
                }
            })
            .chain(self.instanced_players().filter_map(|(_, p, inst)| {
                let slot = p.current_slot.as_ref().unwrap().0;
                let pos = inst.position.p.0;
                let id = JtId::Slot(slot);
                match self.ephemeral.cfg.airborne_jtacs.get(&inst.typ) {
                    Some(jt) => Some(JtDesc {
                        pos,
                        id,
                        side: p.side,
                        spec: *jt,
                        air: true,
                    }),
                    None => match self.ephemeral.cargo.get(&slot) {
                        None => None,
                        Some(cargo) => {
                            for it in &cargo.troops {
                                if let Some(jt) = &it.troop.jtac {
                                    return Some(JtDesc {
                                        pos,
                                        id,
                                        side: p.side,
                                        spec: *jt,
                                        air: false,
                                    });
                                }
                            }
                            None
                        }
                    },
                }
            }))
    }
}
