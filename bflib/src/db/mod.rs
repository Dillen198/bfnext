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
use anyhow::{Result, anyhow};
use bfprotocols::{
    cfg::{
        Action, ActionKind, AwacsCfg, Cfg, Deployable, DeployableJtac, DroneCfg, Troop,
    },
    db::{
        group::{GroupId, UnitId},
        objective::ObjectiveId,
    },
};
use dcso3::{
    Position3, Vector3, centroid3d,
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
pub mod intel;
pub mod logistics;
pub mod map_layer;
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
    pub state: Option<bfprotocols::cfg::JtacState>,
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

/// A donor to the EWR radar network.
/// `aspect_half_angle` is the half-angle of the forward detection cone in degrees;
/// `None` means omnidirectional (ground EWRs, AWACS, ships).
#[derive(Debug, Clone, Copy)]
pub struct RadarDonor {
    pub pos: Position3,
    pub side: Side,
    pub range: u32,
    pub aspect_half_angle: Option<u16>,
    pub airborne: bool,
    pub sensor_type: bfprotocols::cfg::SensorType,
    pub pulse_doppler: bool,
    pub look_down_capable: bool,
    /// Owning group, when this donor is a real DCS ground/naval unit (as
    /// opposed to a deployed EWR or an airborne player). Lets IADN EMCON
    /// control resolve a live Controller for this donor when it needs to --
    /// always by name via Group::get_by_name at the point of use, never
    /// cached, so a donor whose objective gets culled and later respawned
    /// is handled correctly with no stale reference.
    pub gid: Option<GroupId>,
    /// Stored for future chaff mechanic — not yet consumed by physics.
    #[allow(dead_code)]
    pub chaff_susceptibility: f32,
    /// Stored for future jamming mechanic — not yet consumed by physics.
    #[allow(dead_code)]
    pub ecm_susceptibility: f32,
    pub scan_interval_secs: u32,
    pub frequency_band: bfprotocols::cfg::RadarBand,
}

impl Db {
    /// Distribute `budget` points across owned objectives on `side`, weighted by
    /// objective kind. Only seeds objectives whose current points == 0 (when
    /// `only_zero` is true) so existing earned points are preserved on load.
    fn seed_objective_points(&mut self, side: Side, budget: i32, only_zero: bool) {
        use bfprotocols::db::objective::ObjectiveKind;
        fn weight(kind: &ObjectiveKind) -> i32 {
            match kind {
                ObjectiveKind::Airbase => 5,
                ObjectiveKind::NavalBase => 4,
                ObjectiveKind::Factory { .. } => 4,
                ObjectiveKind::Logistics => 3,
                ObjectiveKind::Fob => 2,
                ObjectiveKind::CarrierGroup { .. } => 2,
                ObjectiveKind::Farp { .. } => 1,
                ObjectiveKind::SpecialSamSite { .. } => 1,
            }
        }
        let candidates: Vec<_> = self
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, obj)| {
                obj.owner() == side && (!only_zero || obj.points == 0)
            })
            .map(|(id, obj)| (*id, weight(obj.kind())))
            .collect();
        let total_weight: i32 = candidates.iter().map(|(_, w)| w).sum();
        if total_weight == 0 {
            return;
        }
        for (oid, w) in candidates {
            let share = (budget as i64 * w as i64 / total_weight as i64) as i32;
            if share > 0 {
                if let Some(obj) = self.persisted.objectives.get_mut_cow(&oid) {
                    obj.points = share;
                }
            }
        }
    }

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
        for (side, budget) in db.ephemeral.cfg.objective_start_points.clone() {
            if budget > 0 {
                db.seed_objective_points(side, budget, true);
            }
        }
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

    /// True if there is an active supply convoy currently headed to the given objective.
    #[allow(dead_code)]
    pub fn convoy_inbound_to(&self, oid: ObjectiveId) -> bool {
        self.ephemeral
            .active_convoys
            .values()
            .any(|c| c.destination == oid)
    }

    /// Objectives of `side` that enemy troops are actively attempting to capture.
    pub fn objectives_being_captured_by(&self, side: dcso3::coalition::Side) -> Vec<ObjectiveId> {
        self.ephemeral
            .capture_progress
            .iter()
            .filter_map(|(oid, (capturing_side, _, _))| {
                if *capturing_side != side {
                    // capturing_side is the ENEMY — oid is a friendly objective being taken
                    let obj = self.persisted.objectives.get(oid)?;
                    if obj.owner() == side {
                        Some(*oid)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    /// Iterate over every active radar donor:
    /// - deployed ground EWRs (omnidirectional, from the persisted EWR set)
    /// - AI units whose vehicle type is in `cfg.ground_radar_ewrs` (ships, SAM search radars)
    /// - player aircraft whose vehicle type is in `cfg.airborne_ewrs`
    pub fn radar_donors(&self) -> impl Iterator<Item = RadarDonor> + '_ {
        // Deployed EWR groups (AWACS actions, deployed EWR deployables)
        let deployed = self.persisted.ewrs.into_iter().filter_map(|gid| {
            let group = self.persisted.groups.get(gid)?;
            let (ewr, side) = match &group.origin {
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
                } => (ewr, group.side),
                _ => return None,
            };
            let p = centroid3d(
                group
                    .units
                    .into_iter()
                    .map(|u| self.persisted.units[u].position.p.0),
            );
            Some(RadarDonor {
                pos: Position3 { p: dcso3::LuaVec3(p), ..Default::default() },
                side,
                range: ewr.range,
                aspect_half_angle: None,
                airborne: false,
                sensor_type: bfprotocols::cfg::SensorType::GroundEwr,
                pulse_doppler: false,
                look_down_capable: false,
                gid: Some(*gid),
                chaff_susceptibility: 0.3,
                ecm_susceptibility: 0.5,
                scan_interval_secs: 8,
                frequency_band: bfprotocols::cfg::RadarBand::Vhf,
            })
        });
        // AI ground / naval units listed in ground_radar_ewrs
        let ai_ground = self
            .persisted
            .units
            .into_iter()
            .filter(|(_, u)| !u.dead)
            .filter_map(|(_, u)| {
                self.ephemeral
                    .cfg
                    .ground_radar_ewrs
                    .get(&u.typ)
                    .map(|ewr_cfg| {
                        let sensor_type = match ewr_cfg.aspect_half_angle {
                            Some(_) => bfprotocols::cfg::SensorType::SamSearchRadar,
                            None => bfprotocols::cfg::SensorType::NavalRadar,
                        };
                        RadarDonor {
                            pos: u.position,
                            side: u.side,
                            range: ewr_cfg.range,
                            aspect_half_angle: ewr_cfg.aspect_half_angle,
                            airborne: false,
                            sensor_type,
                            pulse_doppler: ewr_cfg.pulse_doppler,
                            look_down_capable: ewr_cfg.look_down_capable,
                            gid: Some(u.group),
                            chaff_susceptibility: ewr_cfg.chaff_susceptibility,
                            ecm_susceptibility: ewr_cfg.ecm_susceptibility,
                            scan_interval_secs: ewr_cfg.scan_interval_secs,
                            frequency_band: ewr_cfg.frequency_band,
                        }
                    })
            });
        // Player aircraft listed in airborne_ewrs
        let airborne = self
            .instanced_players()
            .filter(|(_, _, inst)| inst.in_air)
            .filter_map(|(_, player, inst)| {
                self.ephemeral
                    .cfg
                    .airborne_ewrs
                    .get(&inst.typ)
                    .map(|ewr_cfg| {
                        let sensor_type = match ewr_cfg.aspect_half_angle {
                            Some(_) => bfprotocols::cfg::SensorType::AirborneFighter,
                            None => bfprotocols::cfg::SensorType::Awacs,
                        };
                        RadarDonor {
                            pos: inst.position,
                            side: player.side,
                            range: ewr_cfg.range,
                            aspect_half_angle: ewr_cfg.aspect_half_angle,
                            airborne: true,
                            sensor_type,
                            pulse_doppler: ewr_cfg.pulse_doppler,
                            look_down_capable: ewr_cfg.look_down_capable,
                            gid: None,
                            chaff_susceptibility: ewr_cfg.chaff_susceptibility,
                            ecm_susceptibility: ewr_cfg.ecm_susceptibility,
                            scan_interval_secs: ewr_cfg.scan_interval_secs,
                            frequency_band: ewr_cfg.frequency_band,
                        }
                    })
            });
        deployed.chain(ai_ground).chain(airborne)
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
                        jtac: jtac_state,
                        ..
                    } => Some(JtDesc {
                        pos,
                        id: JtId::Group(*gid),
                        side: group.side,
                        spec: jtac.clone(),
                        state: jtac_state.clone(),
                        air: false,
                    }),
                    DeployKind::Deployed {
                        spec:
                            Deployable {
                                jtac: Some(jtac), ..
                            },
                        jtac: jtac_state,
                        ..
                    } => Some(JtDesc {
                        pos,
                        id: JtId::Group(*gid),
                        side: group.side,
                        spec: jtac.clone(),
                        state: jtac_state.clone(),
                        air: false,
                    }),
                    DeployKind::Action {
                        spec:
                            Action {
                                kind: ActionKind::Drone(DroneCfg { jtac, .. }),
                                ..
                            },
                        jtac: jtac_state,
                        ..
                    } => Some(JtDesc {
                        pos,
                        id: JtId::Group(*gid),
                        side: group.side,
                        spec: jtac.clone(),
                        state: jtac_state.clone(),
                        air: true,
                    }),
                    DeployKind::Crate { .. }
                    | DeployKind::Action { .. }
                    | DeployKind::Objective { .. }
                    | DeployKind::ObjectiveDeprecated
                    | DeployKind::Troop { .. }
                    | DeployKind::Deployed { .. }
                    | DeployKind::DownedPilot { .. }
                    | DeployKind::Dismount { .. } => None,
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
                        spec: jt.clone(),
                        state: None,
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
                                        spec: jt.clone(),
                                        state: it.jtac.clone(),
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
