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
    Db, Map, MapM, MapS, Set,
    group::{DeployKind, SpawnedUnit},
    logistics::{Inventory, LogiStage, Warehouse},
};
use crate::{
    group, group_health, group_mut,
    landcache::LandCache,
    maybe, objective, objective_mut,
    spawnctx::{Despawn, SpawnCtx, SpawnLoc},
    unit, unit_mut,
};
use anyhow::{Context, Result, anyhow};
use bfprotocols::{
    cfg::{Deployable, DeployableObjective, UnitTag, Vehicle, VictoryCondition},
    db::{
        group::{GroupId, UnitId},
        objective::{ObjectiveId, ObjectiveKind},
    },
    stats::Stat,
};
use chrono::{Duration, prelude::*};
use compact_str::format_compact;
use core::f64;
use dcso3::{
    LuaVec2, LuaVec3, MizLua, Quad2, String, Vector2, Vector3,
    airbase::Airbase,
    azumith2d_to, centroid2d,
    coalition::Side,
    coord::Coord,
    cvt_err,
    env::miz::{GroupKind, MizIndex},
    land::Land,
    net::Ucid,
    object::DcsObject,
    warehouse::LiquidType,
};
use enumflags2::BitFlags;
use fxhash::{FxHashMap, FxHashSet};
use log::{debug, error, info, warn};
use mlua::{Value, prelude::*};
use serde_derive::{Deserialize, Serialize};
use smallvec::{SmallVec, smallvec};
use std::{cmp::max, str::FromStr, sync::Arc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ObjGroupClass {
    Logi,
    Aaa,
    Lr,
    Mr,
    Sr,
    Armor,
    Services,
    Naval,
    Infantry,
    Other,
}

impl ObjGroupClass {
    pub fn is_services(&self) -> bool {
        match self {
            Self::Services => true,
            Self::Logi | Self::Aaa | Self::Lr | Self::Mr | Self::Sr | Self::Armor | Self::Naval | Self::Infantry | Self::Other => {
                false
            }
        }
    }

    pub fn is_logi(&self) -> bool {
        match self {
            Self::Logi => true,
            Self::Services
            | Self::Aaa
            | Self::Lr
            | Self::Mr
            | Self::Sr
            | Self::Armor
            | Self::Naval
            | Self::Infantry
            | Self::Other => false,
        }
    }

    pub fn is_infantry(&self) -> bool {
        matches!(self, Self::Infantry)
    }
}

impl From<&str> for ObjGroupClass {
    fn from(s: &str) -> Self {
        if s.starts_with("BLOGI")
            || s.starts_with("RLOGI")
            || s.starts_with("NLOGI")
            || s.starts_with("LOGI")
            || s.starts_with("BDEPFARP")
            || s.starts_with("RDEPFARP")
        {
            ObjGroupClass::Logi
        } else if s.starts_with("BSERVICES")
            || s.starts_with("RSERVICES")
            || s.starts_with("NSERVICES")
            || s.starts_with("SERVICES")
        {
            ObjGroupClass::Services
        } else if s.starts_with("BAAA")
            || s.starts_with("RAAA")
            || s.starts_with("NAAA")
            || s.starts_with("AAA")
        {
            ObjGroupClass::Aaa
        } else if s.starts_with("BLR")
            || s.starts_with("RLR")
            || s.starts_with("NLR")
            || s.starts_with("LR")
        {
            ObjGroupClass::Lr
        } else if s.starts_with("BMR")
            || s.starts_with("RMR")
            || s.starts_with("NMR")
            || s.starts_with("MR")
        {
            ObjGroupClass::Mr
        } else if s.starts_with("BSR")
            || s.starts_with("RSR")
            || s.starts_with("NSR")
            || s.starts_with("SR")
        {
            ObjGroupClass::Sr
        } else if s.starts_with("BARMOR")
            || s.starts_with("RARMOR")
            || s.starts_with("NARMOR")
            || s.starts_with("ARMOR")
        {
            ObjGroupClass::Armor
        } else if s.starts_with("BINF")
            || s.starts_with("RINF")
            || s.starts_with("NINF")
            || s.starts_with("INF")
        {
            ObjGroupClass::Infantry
        } else if s.starts_with("BCARRIER")
            || s.starts_with("RCARRIER")
            || s.starts_with("NCARRIER")
            || s.starts_with("CARRIER")
            || s.starts_with("BESCORT")
            || s.starts_with("RESCORT")
            || s.starts_with("NESCORT")
            || s.starts_with("ESCORT")
            || s.starts_with("BNAVALAA")
            || s.starts_with("RNAVALAA")
            || s.starts_with("NNAVALAA")
            || s.starts_with("NAVALAA")
        {
            ObjGroupClass::Naval
        } else {
            ObjGroupClass::Other
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ObjGroup(String);

impl FromStr for ObjGroup {
    type Err = LuaError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(String::from(s)))
    }
}

impl<'lua> FromLua<'lua> for ObjGroup {
    fn from_lua(value: LuaValue<'lua>, _lua: &'lua Lua) -> LuaResult<Self> {
        match value {
            Value::String(s) => s.to_str()?.parse(),
            _ => Err(cvt_err("ObjGroup")),
        }
    }
}

impl ObjGroup {
    pub(super) fn template(&self, side: Side) -> (Side, String) {
        let s = match self.0.rsplit_once("-") {
            Some((l, _)) => l,
            None => self.0.as_str(),
        };
        if s.starts_with("R") {
            (Side::Red, s.into())
        } else if s.starts_with("B") {
            (Side::Blue, s.into())
        } else if s.starts_with("N") {
            (Side::Neutral, s.into())
        } else {
            let pfx = match side {
                Side::Red => "R",
                Side::Blue => "B",
                Side::Neutral => "N",
            };
            (side, format_compact!("{}{}", pfx, s).into())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Zone {
    Circle { pos: Vector2, radius: f64 },
    Quad { pos: Vector2, points: Quad2 },
}

impl Default for Zone {
    fn default() -> Self {
        Self::Circle {
            pos: Vector2::zeros(),
            radius: 0.,
        }
    }
}

impl Zone {
    pub fn contains(&self, pos: Vector2) -> bool {
        match self {
            Self::Circle {
                pos: center,
                radius,
            } => na::distance_squared(&(*center).into(), &pos.into()) <= radius.powi(2),
            Self::Quad { points, .. } => points.contains(LuaVec2(pos)),
        }
    }

    pub fn pos(&self) -> Vector2 {
        match self {
            Self::Circle { pos, .. } => *pos,
            Self::Quad { pos, .. } => *pos,
        }
    }

    /// returns the radius of the smallest circle that contains the zone
    pub fn radius(&self) -> f64 {
        match self {
            Self::Circle { radius, .. } => *radius,
            Self::Quad { pos, points } => [points.p0, points.p1, points.p2, points.p3]
                .into_iter()
                .fold(0., |max, p| {
                    let d = na::distance_squared(&p.0.into(), &(*pos).into());
                    if d > max { d } else { max }
                })
                .sqrt(),
        }
    }

    /// scale the zone by the specified factor which must be non
    /// negative.
    pub fn scale(&self, factor: f64) -> Self {
        match self {
            Self::Quad { pos, points } => Self::Quad {
                pos: *pos,
                points: points.scale(factor),
            },
            Self::Circle { pos, radius } => {
                let factor = factor.clamp(0., f64::INFINITY);
                Self::Circle {
                    pos: *pos,
                    radius: *radius * factor,
                }
            }
        }
    }

    /// returns true if the specified circle is totally contained by the zone
    #[allow(unused)]
    pub fn contains_circle(&self, center: Vector2, radius: f64) -> bool {
        match self {
            Self::Quad { pos: _, points } => points.contains_circle(center, radius),
            Self::Circle { pos, radius: r } => {
                let d = na::distance(&center.into(), &(*pos).into());
                *r >= radius + d
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Objective {
    pub id: ObjectiveId,
    pub name: String,
    pub owner: Side,
    pub(super) kind: ObjectiveKind,
    pub(super) groups: MapS<Side, Set<GroupId>>,
    pub(super) health: u8,
    pub(super) logi: u8,
    #[serde(default)]
    pub(super) infantry: u8,
    #[serde(default)]
    pub(super) supply: u8,
    #[serde(default)]
    pub(super) fuel: u8,
    pub(super) threatened: bool,
    pub(super) last_threatened_ts: DateTime<Utc>,
    pub(super) last_change_ts: DateTime<Utc>,
    #[serde(default)]
    pub(super) warehouse: Warehouse,
    #[serde(default)]
    pub(super) zone: Zone,
    #[serde(default)]
    pub(super) logistics_detached: bool,
    /// Set via the UNLIMITED_SUPPLY trigger zone property in the mission
    /// editor (same mechanism as LOGISTICS_DETACHED). Never runs low on
    /// weapons/vehicles/fuel, regardless of production/consumption -- only
    /// affects item types this objective's side already has access to.
    /// Does not cover aircraft; see unlimited_aircraft for that.
    #[serde(default)]
    pub(super) unlimited_supply: bool,
    /// Set via the UNLIMITED_AIRCRAFTS trigger zone property. Same idea as
    /// unlimited_supply but scoped to aircraft types only, so a base's
    /// plane/helicopter roster can be made unlimited independently of its
    /// weapons/fuel stock.
    #[serde(default)]
    pub(super) unlimited_aircraft: bool,
    #[serde(default)]
    pub points: i32,
    /// After a capture, the assaulting troop groups that are holding the base
    /// during the consolidation window. While non-empty the garrison does not
    /// spawn and `captureable()` stays true. Cleared on consolidation (troops
    /// survived the timer) or when the base goes Neutral (troops wiped out).
    #[serde(default)]
    pub(super) capture_hold: Vec<GroupId>,
    #[serde(default)]
    pub(super) capture_hold_ts: Option<DateTime<Utc>>,
    /// Commander's intent marker, settable via the fowlengine Discord bot / bfdb
    /// admin API. Display/coordination only -- does not affect AI or logistics.
    #[serde(default)]
    pub(super) priority: bool,
    #[serde(skip)]
    pub(super) spawned: bool,
    #[serde(skip)]
    pub(super) enabled: bool,
    #[serde(skip)]
    pub(super) last_activate: DateTime<Utc>,
    #[serde(skip)]
    pub(super) threat_pos3: Vector3,
}

impl Objective {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn health(&self) -> u8 {
        self.health
    }

    pub fn last_change(&self) -> DateTime<Utc> {
        self.last_change_ts
    }

    pub fn logi(&self) -> u8 {
        self.logi
    }

    pub fn captureable(&self) -> bool {
        // A base still in its post-capture hold (garrison not yet consolidated)
        // is takeable by either side until it consolidates or goes Neutral.
        !self.capture_hold.is_empty() || (self.health <= 20 && self.infantry == 0)
    }

    /// True while the base is held only by the assaulting troops after a
    /// capture and hasn't consolidated its garrison yet.
    pub fn in_capture_hold(&self) -> bool {
        !self.capture_hold.is_empty()
    }

    pub fn owner(&self) -> Side {
        self.owner
    }

    pub fn priority(&self) -> bool {
        self.priority
    }

    pub fn is_farp(&self) -> bool {
        match &self.kind {
            ObjectiveKind::Farp { .. } => true,
            ObjectiveKind::Airbase | ObjectiveKind::Fob | ObjectiveKind::Logistics | ObjectiveKind::NavalBase | ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::Factory { .. } | ObjectiveKind::SpecialSamSite { .. } | ObjectiveKind::CommandCenter => false,
        }
    }

    pub fn is_airbase(&self) -> bool {
        match &self.kind {
            ObjectiveKind::Airbase => true,
            ObjectiveKind::Farp { .. } | ObjectiveKind::Fob | ObjectiveKind::Logistics | ObjectiveKind::NavalBase | ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::Factory { .. } | ObjectiveKind::SpecialSamSite { .. } | ObjectiveKind::CommandCenter => false,
        }
    }

    pub fn get_equipment(&self, name: &str) -> Inventory {
        self.warehouse
            .equipment
            .get(name)
            .map(|i| *i)
            .unwrap_or_default()
    }

    pub fn get_liquids(&self, name: &LiquidType) -> Inventory {
        self.warehouse
            .liquids
            .get(name)
            .map(|i| *i)
            .unwrap_or_default()
    }

    pub fn pos(&self) -> Vector2 {
        self.zone.pos()
    }

    pub fn groups(&self) -> &MapS<Side, Set<GroupId>> {
        &self.groups
    }

    pub fn supply(&self) -> u8 {
        self.supply
    }

    pub fn fuel(&self) -> u8 {
        self.fuel
    }

    pub fn threatened(&self) -> bool {
        self.threatened
    }

    pub fn warehouse(&self) -> &Warehouse {
        &self.warehouse
    }



    pub fn points(&self) -> i32 {
        self.points
    }

    pub fn kind(&self) -> &ObjectiveKind {
        &self.kind
    }
}

impl Db {
    pub fn objective(&self, id: &ObjectiveId) -> Result<&Objective> {
        objective!(self, id)
    }

    /// Set or clear the commander's-intent priority marker on an objective.
    /// Display/coordination only; intentionally does not touch AI targeting,
    /// logistics weighting, or any other gameplay logic.
    pub fn set_objective_priority(&mut self, id: &ObjectiveId, priority: bool) -> Result<()> {
        objective_mut!(self, *id)?.priority = priority;
        Ok(())
    }

    pub fn objectives(&self) -> impl Iterator<Item = (&ObjectiveId, &Objective)> {
        self.persisted.objectives.into_iter()
    }

    /// returns the closest objective that matches the critera to the specified point
    /// (distance, heading from objective to point, objective)
    pub fn objective_near_point<P: Fn(&Objective) -> bool>(
        obj: &MapM<ObjectiveId, Objective>,
        pos: Vector2,
        p: P,
    ) -> Option<(f64, f64, &Objective)> {
        let (dist, obj) =
            obj.into_iter()
                .fold((f64::MAX, None), |(cur_dist, cur_obj), (_, obj)| {
                    if !p(obj) {
                        (cur_dist, cur_obj)
                    } else {
                        let dist = na::distance_squared(&obj.zone.pos().into(), &pos.into());
                        if dist < cur_dist {
                            (dist, Some(obj))
                        } else {
                            (cur_dist, cur_obj)
                        }
                    }
                });
        obj.map(|obj| (dist.sqrt(), azumith2d_to(obj.zone.pos(), pos), obj))
    }

    fn compute_objective_status(&self, obj: &Objective) -> Result<(u8, u8, u8)> {
        let (health, mut logi, infantry) = obj
            .groups
            .get(&obj.owner)
            .map(|groups| -> Result<(u8, u8, u8)> {
                let mut total = 0;
                let mut alive = 0;
                let mut logi_total = 0;
                let mut logi_alive = 0;
                let mut infantry_total = 0;
                let mut infantry_alive = 0;
                let mut has_supply_ship = false;
                let mut supply_ship_alive = false;

                for gid in groups {
                    let group = group!(self, gid)?;
                    let is_logi = group.class.is_logi();
                    let is_infantry = group.class.is_infantry();
                    for uid in &group.units {
                        let unit = unit!(self, uid)?;

                        // Check if this is a supply ship (for carrier groups)
                        if let ObjectiveKind::CarrierGroup { .. } = &obj.kind {
                            if group.name.contains("SUPPLY") || unit.name.contains("SUPPLY") {
                                has_supply_ship = true;
                                if !unit.dead {
                                    supply_ship_alive = true;
                                }
                            }
                        }

                        if !unit.tags.contains(UnitTag::Invincible) {
                            total += 1;
                            if is_logi {
                                logi_total += 1;
                            }
                            if is_infantry {
                                infantry_total += 1;
                            }
                            if !unit.dead {
                                alive += 1;
                                if is_logi {
                                    logi_alive += 1;
                                }
                                if is_infantry {
                                    infantry_alive += 1;
                                }
                            }
                        }
                    }
                }

                let health = ((alive as f32 / total as f32) * 100.).trunc() as u8;
                let mut logi = ((logi_alive as f32 / logi_total as f32) * 100.).trunc() as u8;
                let infantry = ((infantry_alive as f32 / infantry_total as f32) * 100.).trunc() as u8;

                // For carrier groups with supply ships, logi becomes 0 if supply ship is dead
                if let ObjectiveKind::CarrierGroup { .. } = &obj.kind {
                    if has_supply_ship && !supply_ship_alive {
                        logi = 0;
                    }
                }

                Ok((health, logi, infantry))
            })
            .unwrap_or(Ok((0, 0, 0)))?;

        // Logistics-relevant map buildings (warehouses, fuel depots, etc.)
        // destroyed at this objective further degrade its logi rating,
        // independent of the unit-group-based calculation above. See
        // scan_objective_scenery / check_scenery_buildings.
        let destroyed = self
            .ephemeral
            .scenery_destroyed_by_objective
            .get(&obj.id)
            .copied()
            .unwrap_or(0);
        let total = self
            .ephemeral
            .scenery_total_by_objective
            .get(&obj.id)
            .copied()
            .unwrap_or(0);
        if total > 0 && destroyed > 0 {
            let remaining_frac = 1. - (destroyed as f32 / total as f32).min(1.);
            logi = ((logi as f32) * remaining_frac).round() as u8;
        }

        Ok((health, logi, infantry))
    }

    pub(super) fn delete_objective(&mut self, oid: &ObjectiveId) -> Result<()> {
        let obj = self
            .persisted
            .objectives
            .remove_cow(oid)
            .ok_or_else(|| anyhow!("no such objective {oid}"))?;
        self.persisted.objectives_by_name.remove_cow(&obj.name);
        if let Some(lid) = obj.warehouse.supplier {
            let logi = objective_mut!(self, lid)?;
            logi.warehouse.destination.remove_cow(&obj.id);
            self.ephemeral
                .create_objective_markup(&self.persisted, objective!(self, lid)?);
        }
        for (_, groups) in &obj.groups {
            for gid in groups {
                self.delete_group(gid)?;
                self.persisted.objectives_by_group.remove_cow(gid);
            }
        }
        self.ephemeral
            .slot_info
            .retain(|_, si| &si.objective != oid);
        if let ObjectiveKind::Farp {
            spec: _,
            mobile: _,
            pad_template,
        } = obj.kind
        {
            self.ephemeral.return_pad_template(&pad_template);
        }
        self.persisted.farps.remove_cow(oid);
        self.persisted.special_sam_sites.remove_cow(oid);
        self.ephemeral.airbase_by_oid.remove(oid);
        self.ephemeral.remove_objective_markup(oid);
        self.ephemeral.stat(Stat::ObjectiveDestroyed { id: *oid });
        self.ephemeral.dirty();
        Ok(())
    }

    pub fn add_farp(
        &mut self,
        lua: MizLua,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        pos: Vector2,
        spec: &Deployable,
        parts: &DeployableObjective,
    ) -> Result<ObjectiveId> {
        let now = Utc::now();
        let land = Land::singleton(spctx.lua())?;
        let DeployableObjective {
            pad_templates: _,
            defenses_template,
            ammo_template,
            fuel_template,
            barracks_template,
        } = parts;
        let location = {
            let mut points: SmallVec<[Vector2; 16]> = smallvec![];
            let defenses = defenses_template
                .as_ref()
                .map(|t| spctx.get_template_ref(idx, GroupKind::Any, side, t))
                .transpose()?;
            let ammo = ammo_template
                .as_ref()
                .map(|t| spctx.get_template_ref(idx, GroupKind::Any, side, t))
                .transpose()?;
            let fuel = fuel_template
                .as_ref()
                .map(|t| spctx.get_template_ref(idx, GroupKind::Any, side, t))
                .transpose()?;
            let barracks = barracks_template
                .as_ref()
                .map(|t| spctx.get_template_ref(idx, GroupKind::Any, side, t))
                .transpose()?;
            macro_rules! acc_points {
                ($group:expr) => {
                    if let Some(g) = $group.as_ref() {
                        for unit in g.group.units()? {
                            let unit = unit?;
                            points.push(unit.pos()?);
                        }
                    }
                };
            }
            acc_points!(defenses);
            acc_points!(ammo);
            acc_points!(fuel);
            acc_points!(barracks);
            if points.is_empty() {
                SpawnLoc::AtPosWithCenter { pos, center: pos }
            } else {
                let center = centroid2d(points);
                SpawnLoc::AtPosWithCenter { pos, center }
            }
        };
        let dep_name = spec
            .path
            .last()
            .ok_or_else(|| anyhow!("deployable has no name"))?;
        let pad_template = self
            .ephemeral
            .take_pad_template(side, dep_name)
            .ok_or_else(|| anyhow!("not enough farp pads available to build this farp"))?;
        let oid = ObjectiveId::new();
        let mut groups: Set<GroupId> = Set::new();
        // if the pad template is a boat then add it to units/groups so that it
        // will be handled properly when it is born
        let mut mobile = false;
        if let Ok(gifo) = spctx.get_template_ref(idx, GroupKind::Any, side, &pad_template)
            && let Ok(units) = gifo.group.units()
            && let Ok(unit) = units.first()
            && let Ok(typ) = unit.typ()
            && let Some(tags) = self.ephemeral.cfg.unit_classification.get(&Vehicle(typ))
            && tags.contains(UnitTag::Boat)
        {
            log::info!("adding naval spawn point {pad_template}");
            match self.add_group(
                spctx,
                idx,
                side,
                location.clone(),
                &pad_template,
                DeployKind::Objective { origin: oid },
                UnitTag::NavalSpawnPoint.into(),
            ) {
                Ok(gid) => {
                    mobile = true;
                    groups.insert_cow(gid);
                }
                Err(e) => {
                    self.ephemeral.return_pad_template(&pad_template);
                    return Err(e);
                }
            }
        }
        // delay the spawn of the other components so the unpacker can
        // get out of the way
        for name in [
            &defenses_template,
            &ammo_template,
            &fuel_template,
            &barracks_template,
        ] {
            if let Some(name) = name {
                let gid = match self.add_and_queue_group(
                    spctx,
                    idx,
                    side,
                    location.clone(),
                    &name,
                    DeployKind::Objective { origin: oid },
                    BitFlags::empty(),
                    Some(now + Duration::seconds(60)),
                ) {
                    Ok(gid) => gid,
                    Err(e) => {
                        for gid in &groups {
                            let _ = self.delete_group(gid);
                        }
                        return Err(e);
                    }
                };
                groups.insert_cow(gid);
            }
        }
        let name = {
            let get_utm_zone = || -> Result<String> {
                let coord = Coord::singleton(spctx.lua())?;
                let pos = coord.lo_to_ll(LuaVec3(Vector3::new(pos.x, 0., pos.y)))?;
                let mgrs = coord.ll_to_mgrs(pos.latitude, pos.longitude)?;
                Ok(mgrs.utm_zone)
            };
            let utm_zone = get_utm_zone().unwrap_or_else(|_| String::from("UK"));
            let mut n = 0;
            loop {
                let name = String::from(format_compact!("farp {} {n}", utm_zone));
                if self.persisted.objectives_by_name.get(&name).is_none() {
                    break name;
                } else {
                    n += 1
                }
            }
        };
        let threat_pos3 = {
            let alt = land.get_height(LuaVec2(pos)).unwrap_or_else(|_| 0.);
            Vector3::new(pos.x, alt, pos.y)
        };
        let obj = Objective {
            id: oid,
            name: name.clone(),
            groups: MapS::from_iter([(side, groups)]),
            kind: ObjectiveKind::Farp {
                spec: spec.clone(),
                mobile,
                pad_template: pad_template.clone(),
            },
            zone: Zone::Circle { pos, radius: 2000. },
            owner: side,
            health: 100,
            logi: 100,
            infantry: 0,
            supply: 0,
            fuel: 0,
            spawned: true,
            enabled: true,
            threatened: true,
            warehouse: Warehouse::default(),
            logistics_detached: false,
            unlimited_supply: false,
            unlimited_aircraft: false,
            priority: false,
            points: 0,
            capture_hold: vec![],
            capture_hold_ts: None,
            last_threatened_ts: now,
            last_change_ts: now,
            last_activate: DateTime::<Utc>::default(),
            threat_pos3,
        };
        for (_, groups) in &obj.groups {
            for gid in groups {
                self.persisted.objectives_by_group.insert_cow(*gid, oid);
            }
        }
        let pos = obj.zone.pos();
        let llpos = Coord::singleton(lua)?.lo_to_ll(LuaVec3(Vector3::new(pos.x, 0., pos.y)))?;
        self.ephemeral.stat(Stat::Objective {
            name: name.clone(),
            id: obj.id,
            kind: obj.kind.clone(),
            owner: obj.owner,
            pos: llpos,
        });
        self.persisted.objectives.insert_cow(oid, obj);
        self.persisted.objectives_by_name.insert_cow(name, oid);
        self.persisted.farps.insert_cow(oid);
        // move the pad to the new location
        spctx
            .move_farp_pad(idx, side, pad_template.as_str(), pos)
            .context("moving farp pad")?;
        let airbase = Airbase::get_by_name(spctx.lua(), pad_template.clone())
            .with_context(|| format_compact!("getting airbase {pad_template}"))?;
        airbase.set_coalition(side)?;
        let airbase = airbase
            .object_id()
            .with_context(|| format_compact!("getting airbase {pad_template} object id"))?;
        self.ephemeral.airbase_by_oid.insert(oid, airbase);
        self.init_farp_warehouse(&oid)
            .context("initializing farp warehouse")?;
        self.setup_supply_lines().context("setup supply lines")?;
        let now = chrono::Utc::now();
        let trs = self
            .deliver_supplies_from_logistics_hubs(lua, now)
            .context("distributing supplies")?;
        match &mut self.ephemeral.logistics_stage {
            LogiStage::ExecuteTransfers { transfers } => transfers.extend(trs),
            stage @ (LogiStage::Complete { .. }
            | LogiStage::Init
            | LogiStage::SyncFromWarehouses { .. }
            | LogiStage::SyncToWarehouses { .. }
            | LogiStage::ManageConvoys { .. }
            | LogiStage::ManageAirRoutes { .. }
            | LogiStage::ManageSeaRoutes { .. }) => {
                *stage = LogiStage::ExecuteTransfers { transfers: trs };
            }
        }
        self.ephemeral
            .create_objective_markup(&self.persisted, objective!(self, oid)?);
        self.ephemeral.dirty();
        Ok(oid)
    }

    pub(super) fn update_objective_status(
        &mut self,
        oid: &ObjectiveId,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let (kind, health, logi, _prev_logi, name, owner, newly_capturable) = {
            let obj = objective!(self, oid)?;
            let prev_logi = obj.logi;
            let prev_eligible = obj.captureable() || obj.kind.is_special_sam_site();
            let (health, logi, infantry) = self.compute_objective_status(obj)?;
            let obj = objective_mut!(self, oid)?;
            obj.health = health;
            obj.logi = logi;
            obj.infantry = infantry;
            obj.last_change_ts = now;

            // For carrier groups, mark warehouse as damaged if supply ship is destroyed (logi drops to 0)
            if let ObjectiveKind::CarrierGroup { .. } = &obj.kind {
                if prev_logi > 0 && logi == 0 {
                    obj.warehouse.damaged = true;
                    info!("[CARRIER_SUPPLY] {} warehouse disabled - supply ship destroyed", obj.name);
                }
            }

            let new_eligible = obj.captureable() || obj.kind.is_special_sam_site();
            // Don't fire the generic "is now capturable" broadcast while the
            // objective is in its post-capture hold (announced by the capture
            // flow instead), or while its garrison isn't even spawned -- a
            // freshly captured / culled base reads as "capturable" transiently
            // and firing the message right after a capture just confuses.
            let newly_capturable = !prev_eligible
                && new_eligible
                && obj.capture_hold.is_empty()
                && obj.spawned;
            (obj.kind.clone(), health, logi, prev_logi, obj.name.clone(), obj.owner, newly_capturable)
        };
        if newly_capturable {
            self.ephemeral.msgs().panel_to_all(
                15,
                false,
                format_compact!(
                    "{name} ({owner:?}) is now capturable -- get troops into the zone."
                ),
            );
        }
        self.ephemeral.stat(Stat::ObjectiveHealth {
            id: *oid,
            last_change: now,
            health,
            logi,
        });
        if let ObjectiveKind::Farp { .. } = &kind {
            if logi == 0 {
                self.delete_objective(oid)?;
            }
        }
        self.ephemeral.dirty();
        debug!("objective {oid} health: {}, logi: {}", health, logi);
        Ok(())
    }

    pub fn repair_objective(&mut self, oid: ObjectiveId, now: DateTime<Utc>) -> Result<()> {
        let repair_supply_cost = self.ephemeral.cfg.repair_supply_cost;
        let obj = self
            .persisted
            .objectives
            .get(&oid)
            .ok_or_else(|| anyhow!("no such objective {:?}", oid))?;
        if obj.supply < repair_supply_cost {
            return Ok(());
        }
        if let Some(groups) = obj.groups.get(&obj.owner) {
            let mut damaged_by_class: FxHashMap<ObjGroupClass, Vec<(GroupId, usize)>> =
                groups.into_iter().fold(
                    Ok(FxHashMap::default()),
                    |m: Result<FxHashMap<ObjGroupClass, Vec<(GroupId, usize)>>>, id| {
                        let mut m = m?;
                        let group = group!(self, id)?;
                        let mut damaged = 0;
                        for uid in &group.units {
                            damaged += if unit!(self, uid)?.dead { 1 } else { 0 };
                        }
                        if damaged > 0 {
                            m.entry(group.class).or_default().push((*id, damaged));
                            Ok(m)
                        } else {
                            Ok(m)
                        }
                    },
                )?;
            for class in [
                ObjGroupClass::Logi,
                ObjGroupClass::Services,
                ObjGroupClass::Infantry,
                ObjGroupClass::Sr,
                ObjGroupClass::Aaa,
                ObjGroupClass::Mr,
                ObjGroupClass::Lr,
                ObjGroupClass::Armor,
                ObjGroupClass::Other,
            ] {
                if let Some(groups) = damaged_by_class.get_mut(&class) {
                    groups.sort_by_key(|(_, d)| *d); // pick the most damaged group
                    if let Some((gid, _)) = groups.pop() {
                        let group = group!(self, gid)?;
                        for uid in &group.units {
                            unit_mut!(self, uid)?.dead = false;
                        }
                        let spawned = obj.spawned;
                        if spawned || class == ObjGroupClass::Services {
                            self.ephemeral.push_spawn(gid)
                        }
                        let owner = obj.owner;
                        if let Some(production) =
                            self.ephemeral.production_by_side.get(&owner).cloned()
                        {
                            let percent = repair_supply_cost as f32 / 100.;
                            if let Some(obj) = self.persisted.objectives.get_mut_cow(&oid) {
                                for name in production.equipment.keys() {
                                    if let Some(inv) = obj.warehouse.equipment.get_mut_cow(name) {
                                        inv.reduce(percent);
                                    }
                                }
                                for liq in production.liquids.keys() {
                                    if let Some(inv) = obj.warehouse.liquids.get_mut_cow(liq) {
                                        inv.reduce(percent);
                                    }
                                }
                            }
                        }
                        self.update_supply_status()
                            .context("updating supply status after repair")?;
                        self.update_objective_status(&oid, now)?;
                        self.ephemeral.dirty();
                        return Ok(());
                    }
                }
            }
        }
        Ok(())
    }

    /// Draw supply from the objectives that unpacked crates were spawned at --
    /// one share per crate consumed. `origins` is (origin_oid, crates_from_it).
    /// The draw is multiplicative on the warehouse stock (same mechanism as
    /// `repair_objective`), so it tapers with each deploy and never fully
    /// empties a base; several supplying bases each pay only for their crates.
    pub(crate) fn consume_deploy_supply(
        &mut self,
        origins: impl IntoIterator<Item = (ObjectiveId, usize)>,
    ) -> Result<()> {
        let pct = self.ephemeral.cfg.deploy_supply_cost as f32 / 100.;
        if pct <= 0. {
            return Ok(());
        }
        for (oid, count) in origins {
            if count == 0 {
                continue;
            }
            let owner = match self.persisted.objectives.get(&oid) {
                Some(obj) => obj.owner,
                None => continue,
            };
            let production = match self.ephemeral.production_by_side.get(&owner).cloned() {
                Some(p) => p,
                None => continue,
            };
            // compounded fraction removed for `count` crates
            let frac = 1. - (1. - pct).powi(count as i32);
            if let Some(obj) = self.persisted.objectives.get_mut_cow(&oid) {
                for name in production.equipment.keys() {
                    if let Some(inv) = obj.warehouse.equipment.get_mut_cow(name) {
                        inv.reduce(frac);
                    }
                }
                for liq in production.liquids.keys() {
                    if let Some(inv) = obj.warehouse.liquids.get_mut_cow(liq) {
                        inv.reduce(frac);
                    }
                }
            }
        }
        self.update_supply_status()?;
        self.ephemeral.dirty();
        Ok(())
    }

    pub fn cull_or_respawn_objectives(
        &mut self,
        lua: MizLua,
        landcache: &mut LandCache,
        now: DateTime<Utc>,
    ) -> Result<(SmallVec<[ObjectiveId; 4]>, SmallVec<[ObjectiveId; 4]>)> {
        let land = Land::singleton(lua)?;
        let players = self
            .ephemeral
            .players_by_slot
            .values()
            .filter_map(|ucid| {
                let player = &self.persisted.players[ucid];
                let side = player.side;
                player
                    .current_slot
                    .as_ref()
                    .and_then(|(_, inst)| inst.as_ref())
                    .map(|inst| (side, inst.position.p, inst.velocity, inst.typ.clone()))
            })
            .collect::<SmallVec<[_; 64]>>();
        let cfg = Arc::clone(&self.ephemeral.cfg);
        let cull_distance = (cfg.unit_cull_distance as f64).powi(2);
        let ground_cull_distance = (cfg.ground_vehicle_cull_distance as f64).powi(2);
        let lr_cull_distance = (cfg.lr_cull_distance as f64).powi(2);
        let mut is_close_to_enemies: FxHashSet<UnitId> = FxHashSet::default();
        let mut check_close_units = |units: &Map<UnitId, SpawnedUnit>,
                                     close_units: &FxHashSet<UnitId>,
                                     obj: &Objective,
                                     air_cull_dist: f64,
                                     spawn: &mut bool,
                                     threat: &mut bool| {
            for uid in close_units {
                let unit = units
                    .get(uid)
                    .ok_or_else(|| anyhow!("unknown unit {uid}"))?;
                if obj.owner != unit.side {
                    let air = unit.tags.0.contains(UnitTag::Aircraft)
                        || unit.tags.0.contains(UnitTag::Helicopter);
                    let unarmed = unit.tags.0.contains(UnitTag::Unarmed);
                    let cull_dist = if air {
                        air_cull_dist
                    } else {
                        ground_cull_distance
                    };
                    let dist = na::distance_squared(&obj.zone.pos().into(), &unit.pos.into());
                    if dist <= cull_dist {
                        *spawn = true;
                        if unarmed {
                        } else if air {
                            // Fall back to a conservative range for types not in
                            // the config table -- indexing panicked, which killed
                            // threat detection (and culling) for the whole tick.
                            let threat_dist = (cfg
                                .threatened_distance
                                .get(unit.typ.as_str())
                                .copied()
                                .unwrap_or(14400) as f64)
                                .powi(2);
                            if dist <= threat_dist {
                                *threat = true
                            }
                        } else {
                            *threat = true;
                        }
                        is_close_to_enemies.insert(*uid);
                    }
                }
            }
            Ok::<_, anyhow::Error>(())
        };
        let mut check_close_players = |obj: &Objective,
                                       pos3: Vector3,
                                       air_cull_dist: f64,
                                       spawn: &mut bool,
                                       threat: &mut bool| {
            for (side, pos, v, typ) in &players {
                if obj.owner != *side {
                    let threat_dist = (cfg
                        .threatened_distance
                        .get(typ.as_str())
                        .copied()
                        .unwrap_or(14400) as f64)
                        .powi(2);
                    let ppos = Vector2::new(pos.x, pos.z);
                    let (future_ppos30, future_ppos60) = {
                        let pos30 = pos.0 + (v * 30.);
                        let pos60 = pos.0 + (v * 60.);
                        (
                            Vector2::new(pos30.x, pos30.z),
                            Vector2::new(pos60.x, pos60.z),
                        )
                    };
                    let obj_pos = obj.zone.pos();
                    let dist = na::distance_squared(&obj_pos.into(), &ppos.into());
                    let fdist30 = na::distance_squared(&obj_pos.into(), &future_ppos30.into());
                    let fdist60 = na::distance_squared(&obj_pos.into(), &future_ppos60.into());
                    if dist <= air_cull_dist || fdist30 <= air_cull_dist || fdist60 <= air_cull_dist
                    {
                        *spawn = true;
                    }
                    if dist <= threat_dist {
                        if landcache.is_visible(&land, dist.sqrt(), pos3, pos.0)? {
                            *threat = true;
                        }
                    }
                }
            }
            Ok::<_, anyhow::Error>(())
        };
        let mut became_threatened: SmallVec<[ObjectiveId; 4]> = smallvec![];
        let mut became_clear: SmallVec<[ObjectiveId; 4]> = smallvec![];
        // Objectives whose spawn state flipped this tick -- their scenery markers
        // need creating/removing to match (culled exactly like their units).
        let mut scenery_dirty: SmallVec<[ObjectiveId; 8]> = smallvec![];
        let cooldown = Duration::seconds(self.ephemeral.cfg.threatened_cooldown as i64);
        const ARTY_WAKE_SECS: i64 = 300; // keep objective alive 5 min after last artillery targeting
        const DEPLOY_WAKE_DIST_SQ: f64 = 10_000.0 * 10_000.0; // 10 km radius for deployed-unit wake
        // Precompute live deployed-unit positions once (deployed + troops + dismounts).
        // This avoids repeating O(groups × units) hashmap lookups for every objective
        // and gives the inner wake check a tight, cache-friendly vec to scan instead.
        let mut deployed_positions: SmallVec<[(Vector2, Side); 64]> = SmallVec::new();
        for gid in self
            .persisted
            .deployed
            .into_iter()
            .chain(self.persisted.troops.into_iter())
            .chain(self.persisted.dismounts.into_iter())
        {
            let group = match self.persisted.groups.get(gid) {
                Some(g) => g,
                None => continue,
            };
            let side = group.side;
            for uid in &group.units {
                if let Some(unit) = self.persisted.units.get(uid) {
                    if !unit.dead {
                        deployed_positions.push((unit.pos, side));
                    }
                }
            }
        }
        for (oid, obj) in self.persisted.objectives.iter_mut_cow() {
            let mut spawn = false;
            let mut is_threatened = false;
            let pos3 = obj.threat_pos3;
            // Special SAM sites wake from farther away if they contain a
            // long-range component, e.g. an SA-10/S-300 or Patriot battery,
            // so it has its full detection/engagement range against an
            // approaching aircraft instead of only reacting once the
            // aircraft is already within a generic ground-objective wake
            // distance. Per-unit-type distances (special_sam_wake_distance)
            // take priority so e.g. an SA-10 site can wake from farther out
            // than an SA-11 site even though both are LR-tagged; anything
            // not explicitly listed falls back to lr_cull_distance (if
            // LR-tagged) or the generic aircraft cull distance.
            let air_cull_dist = if obj.kind.is_special_sam_site() {
                let mut max_dist = cull_distance;
                for gid in obj.groups.get(&obj.owner).unwrap_or(&Set::new()) {
                    if let Some(group) = self.persisted.groups.get(gid) {
                        for uid in &group.units {
                            if let Some(unit) = self.persisted.units.get(uid) {
                                let unit_dist = match cfg.special_sam_wake_distance.get(&unit.typ)
                                {
                                    Some(d) => (*d as f64).powi(2),
                                    None if unit.tags.0.contains(UnitTag::LR) => lr_cull_distance,
                                    None => cull_distance,
                                };
                                if unit_dist > max_dist {
                                    max_dist = unit_dist;
                                }
                            }
                        }
                    }
                }
                max_dist
            } else {
                cull_distance
            };
            if let Err(e) =
                check_close_players(obj, pos3, air_cull_dist, &mut spawn, &mut is_threatened)
            {
                error!("failed to check for close players {} {e}", obj.id)
            }
            if let Err(e) = check_close_units(
                &self.persisted.units,
                &self.ephemeral.units_potentially_close_to_enemies,
                obj,
                air_cull_dist,
                &mut spawn,
                &mut is_threatened,
            ) {
                error!("failed to check close units {} {e}", obj.id)
            }
            // If enemy artillery has recently targeted this objective, force it to
            // spawn (or stay spawned) so units are present to absorb the incoming fire.
            if let Some(t) = self.ephemeral.artillery_targeted.get(oid) {
                if now - *t <= Duration::seconds(ARTY_WAKE_SECS) {
                    spawn = true;
                }
            }
            // If any enemy-side player-deployed unit (troop, vehicle, or dismount) is
            // within 10 km of this objective, keep it spawned.  Unlike the main
            // check_close_units path, this covers deployed units that have been
            // stationary long enough to age out of units_potentially_close_to_enemies.
            // Uses precomputed deployed_positions to avoid O(groups × units) hashmap
            // lookups per objective — now just a linear scan over a flat vec.
            if !spawn {
                let obj_pos: na::Point2<f64> = obj.zone.pos().into();
                for (pos, side) in &deployed_positions {
                    if *side != obj.owner {
                        let d = na::distance_squared(&obj_pos, &(*pos).into());
                        if d <= DEPLOY_WAKE_DIST_SQ {
                            spawn = true;
                            break;
                        }
                    }
                }
            }
            if spawn {
                obj.last_activate = now;
            }
            if is_threatened {
                if !obj.threatened {
                    became_threatened.push(*oid);
                }
                obj.threatened = true;
                obj.last_threatened_ts = now;
                self.ephemeral.dirty = true;
            } else {
                if now - obj.last_threatened_ts >= cooldown {
                    if obj.threatened {
                        became_clear.push(*oid);
                    }
                    obj.threatened = false;
                    self.ephemeral.dirty = true;
                }
            }
            if !obj.spawned && spawn && obj.capture_hold.is_empty() {
                obj.spawned = true;
                scenery_dirty.push(*oid);
                let is_mobile = obj.kind.is_carrier_group();
                for gid in obj.groups.get(&obj.owner).unwrap_or(&Set::new()) {
                    let group = group!(self, gid)?;
                    let farp = obj.kind.is_farp();
                    if !farp && !group.class.is_services() {
                        // Don't reset positions for mobile objectives like carrier groups -
                        // their units naturally move outside the original zone
                        if !is_mobile {
                            for uid in &group.units {
                                let unit = unit_mut!(self, uid)?;
                                if !obj.zone.contains(unit.pos) {
                                    unit.pos = unit.spawn_pos;
                                    unit.position = unit.spawn_position;
                                }
                            }
                        }
                        self.ephemeral.push_spawn(*gid);
                    }
                }
            } else if obj.spawned
                && !spawn
                && !obj.threatened
                && now - obj.last_activate >= Duration::seconds(cfg.cull_after as i64)
            {
                // Don't cull carrier groups — they are always present
                if obj.kind.is_carrier_group() {
                    continue;
                }
                obj.spawned = false;
                scenery_dirty.push(*oid);
                for gid in obj.groups.get(&obj.owner).unwrap_or(&Set::new()) {
                    let group = group!(self, gid)?;
                    let farp = obj.kind.is_farp();
                    let services = group.class.is_services();
                    if !farp && !services && group_health!(self, gid)?.0 > 0 {
                        match group.kind {
                            Some(_) => {
                                if let Some(oid) = self.ephemeral.object_id_by_gid.get(gid) {
                                    self.ephemeral
                                        .push_despawn(*gid, Despawn::Group(oid.clone()))
                                }
                            }
                            None => {
                                for uid in &group.units {
                                    let unit = unit!(self, uid)?;
                                    self.ephemeral
                                        .push_despawn(*gid, Despawn::Static(unit.name.clone()))
                                }
                            }
                        }
                    }
                }
            } else if spawn != obj.enabled {
                // Don't toggle AI for carrier groups - they need to keep navigating
                // even when no players are nearby
                if obj.kind.is_carrier_group() {
                    obj.enabled = spawn;
                    continue;
                }
                obj.enabled = spawn;
                for gid in obj.groups.get(&obj.owner).unwrap_or(&Set::new()) {
                    let group = group!(self, gid)?;
                    if group.kind.is_none() {
                        // Static groups have no AI controller to toggle
                        continue;
                    }
                    if let Some(oid) = self.ephemeral.object_id_by_gid.get(gid) {
                        // Convert Object oid to Group by getting the Object, converting to Unit, then getting Group
                        let group = match dcso3::object::Object::get_instance(lua, oid) {
                            Ok(obj) => match obj.as_unit() {
                                Ok(unit) => match unit.get_group() {
                                    Ok(group) => group,
                                    Err(e) => {
                                        warn!("could not get group from unit {gid} {e:?}");
                                        continue;
                                    }
                                },
                                Err(e) => {
                                    // Expected for groups whose DCS object turns out to be a
                                    // static (e.g. cargo crates) despite group.kind being set --
                                    // already handled by skipping via `continue`, so this isn't
                                    // something that needs WARN-level attention.
                                    debug!("object is not a unit {gid} {e:?}");
                                    continue;
                                }
                            },
                            Err(e) => {
                                warn!("could not get object {gid} {e:?}");
                                continue;
                            }
                        };
                        group
                            .get_controller()
                            .context("get controller")?
                            .set_on_off(spawn)
                            .context("enable/disable ai")?
                    }
                }
            }
        }
        self.ephemeral
            .units_potentially_close_to_enemies
            .retain(|uid| is_close_to_enemies.contains(uid));
        // Expire stale artillery-targeting entries so the map doesn't grow unbounded.
        self.ephemeral
            .artillery_targeted
            .retain(|_, t| now - *t <= Duration::seconds(ARTY_WAKE_SECS));
        for oid in scenery_dirty {
            self.sync_scenery_markers(oid);
        }
        Ok((became_threatened, became_clear))
    }

    pub fn repair_services(
        &mut self,
        side: Side,
        now: DateTime<Utc>,
        oid: ObjectiveId,
    ) -> Result<()> {
        let obj = objective_mut!(self, oid)?;
        // despawn the previous services
        for side in [Side::Neutral, side.opposite()] {
            if let Some(groups) = obj.groups.get(&side) {
                for gid in groups {
                    if let Some(group) = self.persisted.groups.get(gid) {
                        if group.class.is_services() {
                            if let Some(oid) = self.ephemeral.object_id_by_gid.get(gid) {
                                self.ephemeral
                                    .push_despawn(*gid, Despawn::Group(oid.clone()))
                            }
                        }
                    }
                }
            }
        }
        for gid in maybe!(obj.groups, &side, "side group")? {
            let group = group_mut!(self, gid)?;
            if group.class.is_services() {
                for uid in &group.units {
                    unit_mut!(self, uid)?.dead = false;
                }
                self.ephemeral
                    .delayspawnq
                    .entry(now + Duration::minutes(3))
                    .or_default()
                    .push(*gid);
            }
        }
        self.update_objective_status(&oid, now)
    }

    pub fn repair_one_logi_step(
        &mut self,
        side: Side,
        now: DateTime<Utc>,
        oid: ObjectiveId,
    ) -> Result<()> {
        let obj = objective_mut!(self, oid)?;
        let mut total_logi = 0;
        let mut logi_groups = 0;
        for gid in maybe!(&obj.groups, &side, "side group")? {
            let group = group!(self, gid)?;
            if group.class.is_logi() {
                logi_groups += 1;
                total_logi = max(total_logi, group.units.len());
            }
        }
        let mut to_repair = 1 + (total_logi >> 1);
        let requested = to_repair;
        for gid in maybe!(&obj.groups, &side, "side group")? {
            let group = group_mut!(self, gid)?;
            if group.class.is_logi() {
                for uid in &group.units {
                    let unit = unit_mut!(self, uid)?;
                    if unit.dead && to_repair > 0 {
                        to_repair -= 1;
                        unit.dead = false;
                    }
                }
                if obj.spawned {
                    self.ephemeral.push_spawn(*gid);
                }
            }
        }
        info!(
            "repair_one_logi_step {oid} side {side:?}: {logi_groups} logi group(s), total_logi={total_logi}, revived {}/{requested}",
            requested - to_repair
        );
        self.update_objective_status(&oid, now)
    }

    pub fn maybe_do_repairs(&mut self, now: DateTime<Utc>) -> Result<()> {
        let to_repair = self
            .persisted
            .objectives
            .into_iter()
            .filter_map(|(oid, obj)| {
                // A base under active assault (enemy nearby, or troops already
                // running the capture timer) can't rebuild itself. A capturable
                // but un-pressured base still repairs -- it just burns supply to
                // do it (see repair_objective), so an attacker who leaves gives
                // it the chance to recover from its own stockpile.
                if obj.threatened || self.ephemeral.capture_progress.contains_key(oid) {
                    return None;
                }
                let logi = obj.logi as f32 / 100.;
                let repair_time = self.ephemeral.cfg.repair_time as f32 / logi;
                if repair_time < i64::MAX as f32 {
                    let repair_time = Duration::seconds(repair_time as i64);
                    if obj.health < 100 && (now - obj.last_change_ts) >= repair_time {
                        Some(*oid)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        for oid in to_repair {
            self.repair_objective(oid, now)?
        }
        Ok(())
    }

    pub fn capturable_objectives(&self) -> SmallVec<[ObjectiveId; 1]> {
        let mut cap = smallvec![];
        for (oid, obj) in &self.persisted.objectives {
            if obj.captureable() || obj.kind.is_special_sam_site() {
                cap.push(*oid)
            }
        }
        cap
    }

    pub fn check_victory(&mut self, now: DateTime<Utc>) -> Option<Side> {
        // If victory was already declared (e.g. by last stand expiry), honour it.
        // Use auto_reset delay if configured, otherwise trigger immediately.
        if let Some((vts, side)) = self.ephemeral.victory {
            let delay = self.ephemeral.cfg.auto_reset
                .map(|vc| Duration::seconds(vc.delay as i64))
                .unwrap_or(Duration::zero());
            let elapsed = now - vts;
            if elapsed >= delay {
                return Some(side);
            } else {
                self.ephemeral.msgs().panel_to_all(
                    10,
                    true,
                    format_compact!(
                        "{side} has won. The server will reset in {}s",
                        (delay - elapsed).num_seconds()
                    ),
                );
                return None;
            }
        }
        // Check MapOwned condition if auto_reset is configured.
        if let Some(vc) = self.ephemeral.cfg.auto_reset {
            let VictoryCondition::MapOwned { fraction } = vc.condition;
            let (blue, red, neutral, total) = self.persisted.objectives.into_iter().fold(
                (0., 0., 0., 0.),
                |(blue, red, neutral, total), (_, obj)| match obj.owner {
                    Side::Blue => (blue + 1., red, neutral, total + 1.),
                    Side::Red => (blue, red + 1., neutral, total + 1.),
                    Side::Neutral => (blue, red, neutral + 1., total + 1.),
                },
            );
            if ((blue + neutral) / total) >= fraction {
                self.ephemeral.victory = Some((now, Side::Blue));
            } else if ((red + neutral) / total) >= fraction {
                self.ephemeral.victory = Some((now, Side::Red));
            }
        }
        None
    }

    /// Returns true if `kind` qualifies as a primary objective for mercy timer purposes.
    fn is_primary_objective(kind: &ObjectiveKind) -> bool {
        matches!(kind, ObjectiveKind::Airbase | ObjectiveKind::NavalBase | ObjectiveKind::Farp { .. })
    }

    /// Check last stand timer: if a side is at or below `trigger_count` primary objectives,
    /// arm the timer. If armed and countdown elapsed, trigger a victory for the other side.
    /// Returns the losing side if the campaign should end now.
    pub fn check_last_stand(&mut self, now: DateTime<Utc>) -> Option<Side> {
        let cfg = self.ephemeral.cfg.last_stand.clone()?;
        // If already armed, check for expiry or send countdown message.
        if let Some((arm_time, losing_side)) = self.ephemeral.last_stand_state {
            let elapsed = now - arm_time;
            let countdown = Duration::seconds(cfg.countdown_secs as i64);
            let remaining = countdown - elapsed;
            if remaining <= Duration::zero() {
                self.ephemeral.last_stand_state = None;
                return Some(losing_side);
            }
            let remaining_secs = remaining.num_seconds();
            let winning_side = losing_side.opposite();
            self.ephemeral.msgs().panel_to_all(
                10,
                true,
                format_compact!(
                    "{losing_side:?} is making their last stand! \
                     {winning_side:?} wins in {remaining_secs}s unless {losing_side:?} recaptures."
                ),
            );
            // Re-check: if losing side has recovered objectives, disarm timer.
            let losing_primary = self.persisted.objectives.into_iter()
                .filter(|(_, o)| o.owner == losing_side && Self::is_primary_objective(&o.kind))
                .count();
            if losing_primary > cfg.trigger_count {
                self.ephemeral.last_stand_state = None;
                self.ephemeral.msgs().panel_to_all(
                    10,
                    false,
                    format_compact!("{losing_side:?} has recaptured objectives. Last stand cancelled."),
                );
            }
            return None;
        }
        // Not armed — check if any side should trigger it.
        for side in [Side::Blue, Side::Red] {
            let primary_count = self.persisted.objectives.into_iter()
                .filter(|(_, o)| o.owner == side && Self::is_primary_objective(&o.kind))
                .count();
            if primary_count <= cfg.trigger_count {
                self.ephemeral.last_stand_state = Some((now, side));
                let winning = side.opposite();
                self.ephemeral.msgs().panel_to_all(
                    15,
                    false,
                    format_compact!(
                        "{side:?} is down to {primary_count} primary objective(s) — Last Stand! \
                         {winning:?} wins in {}s if not recaptured.",
                        cfg.countdown_secs
                    ),
                );
                break;
            }
        }
        None
    }

    /// Trigger a victory for `winning_side`, used by the last stand timer expiry.
    pub fn trigger_last_stand_victory(&mut self, now: DateTime<Utc>, winning_side: Side) {
        self.ephemeral.victory = Some((now, winning_side));
        let losing_side = winning_side.opposite();
        self.ephemeral.msgs().panel_to_all(
            20,
            true,
            format_compact!(
                "Last Stand over: {losing_side:?} could not hold their objectives. {winning_side:?} wins!"
            ),
        );
    }

    fn defender_destruction_ratio(&self, obj: &Objective) -> f64 {
        let Some(gids) = obj.groups.get(&obj.owner) else {
            return 1.0;
        };
        let (total, dead) = gids
            .into_iter()
            .filter_map(|gid| self.persisted.groups.get(gid))
            .flat_map(|g| g.units.into_iter())
            .filter_map(|uid| self.persisted.units.get(uid))
            .fold((0usize, 0usize), |(t, d), u| (t + 1, d + u.dead as usize));
        if total == 0 {
            1.0
        } else {
            dead as f64 / total as f64
        }
    }

    pub fn check_capture(
        &mut self,
        lua: MizLua,
        now: DateTime<Utc>,
    ) -> Result<SmallVec<[(Side, ObjectiveId); 1]>> {
        let min_unit_pct = self
            .ephemeral
            .cfg
            .campaign_events
            .as_ref()
            .map(|c| c.capture_min_unit_pct_destroyed)
            .unwrap_or(0.0);
        let mut captured: FxHashMap<ObjectiveId, Vec<(Side, Option<Ucid>, Option<ObjectiveId>, GroupId)>> =
            FxHashMap::default();
        for (oid, obj) in &self.persisted.objectives {
            let unit_threshold_met = min_unit_pct <= 0.0
                || obj.kind.is_special_sam_site()
                || self.defender_destruction_ratio(obj) >= min_unit_pct;
            if (obj.captureable() || obj.kind.is_special_sam_site()) && unit_threshold_met {
                self.ephemeral.capture_blocked_notice.remove(oid);
                for gid in &self.persisted.troops {
                    let group = group!(self, gid)?;
                    match &group.origin {
                        DeployKind::Troop {
                            spec,
                            player,
                            origin,
                            moved_by: _,
                            cost_fraction: _,
                            ..
                        } if spec.can_capture && group.side != obj.owner => {
                            let in_range = group
                                .units
                                .into_iter()
                                .filter_map(|uid| self.persisted.units.get(uid))
                                .any(|u| !u.dead && obj.zone.contains(u.pos));
                            if in_range {
                                captured
                                    .entry(*oid)
                                    .or_default()
                                    .push((group.side, Some(*player), *origin, *gid));
                            }
                        }
                        DeployKind::Crate { .. }
                        | DeployKind::Deployed { .. }
                        | DeployKind::Objective { .. }
                        | DeployKind::ObjectiveDeprecated
                        | DeployKind::Action { .. }
                        | DeployKind::Troop { .. }
                        | DeployKind::DownedPilot { .. }
                        | DeployKind::Dismount { .. } => (),
                    }
                }
                for gid in &self.persisted.dismounts {
                    let group = group!(self, gid)?;
                    if let DeployKind::Dismount { can_capture, .. } = &group.origin {
                        if *can_capture && group.side != obj.owner {
                            let in_range = group
                                .units
                                .into_iter()
                                .filter_map(|uid| self.persisted.units.get(uid))
                                .any(|u| !u.dead && obj.zone.contains(u.pos));
                            if in_range {
                                captured
                                    .entry(*oid)
                                    .or_default()
                                    .push((group.side, None, None, *gid));
                            }
                        }
                    }
                }
            } else {
                // Not yet eligible -- if enemy capture-capable troops are already
                // standing in the zone, tell them why nothing is happening instead
                // of leaving them guessing. Throttled per-objective.
                let mut enemy_in_zone = false;
                for gid in &self.persisted.troops {
                    let group = group!(self, gid)?;
                    if let DeployKind::Troop { spec, .. } = &group.origin {
                        if spec.can_capture
                            && group.side != obj.owner
                            && group
                                .units
                                .into_iter()
                                .filter_map(|uid| self.persisted.units.get(uid))
                                .any(|u| !u.dead && obj.zone.contains(u.pos))
                        {
                            enemy_in_zone = true;
                            break;
                        }
                    }
                }
                if enemy_in_zone {
                    let last = self.ephemeral.capture_blocked_notice.get(oid).copied();
                    let due = last.map(|t| (now - t).num_seconds() >= 30).unwrap_or(true);
                    if due {
                        self.ephemeral.capture_blocked_notice.insert(*oid, now);
                        let reason = if !unit_threshold_met {
                            format_compact!(
                                "not enough defenders destroyed yet ({:.0}% required)",
                                min_unit_pct * 100.0
                            )
                        } else if obj.infantry > 0 {
                            format_compact!("enemy infantry still defending ({}% left)", obj.infantry)
                        } else {
                            format_compact!("health still above 20% ({}%)", obj.health)
                        };
                        self.ephemeral.msgs().panel_to_all(
                            15,
                            false,
                            format_compact!(
                                "{} is not capturable yet: {reason}.",
                                obj.name
                            ),
                        );
                    }
                }
            }
        }
        let mut actually_captured = smallvec![];
        let mut to_mark: SmallVec<[GroupId; 32]> = smallvec![];
        // Keep track of which objectives currently have troops in zone (for cleanup)
        let mut in_zone_objectives: FxHashSet<ObjectiveId> = FxHashSet::default();
        for (oid, gids) in captured {
            let (side, _, _, _) = gids.first().ok_or_else(|| anyhow!("no guid"))?;
            if gids.iter().all(|(s, _, _, _)| side == s) {
                in_zone_objectives.insert(oid);
                let is_sam = self
                    .persisted
                    .objectives
                    .get(&oid)
                    .map(|o| o.kind.is_special_sam_site())
                    .unwrap_or(false);
                let capture_secs = if is_sam {
                    0i64
                } else {
                    // Base momentum (default 180s), divided by the number of
                    // capturing troop groups in the zone -- bring more squads,
                    // take it faster. Floored so it never goes instant.
                    let base_secs = self
                        .ephemeral
                        .cfg
                        .campaign_events
                        .as_ref()
                        .map_or(180, |c| c.capture_time_secs) as i64;
                    let n_groups = gids.len().max(1) as i64;
                    (base_secs / n_groups).max(30)
                };

                if capture_secs > 0 {
                    let is_new = !self.ephemeral.capture_progress.contains_key(&oid);
                    let entry = self.ephemeral.capture_progress
                        .entry(oid)
                        .or_insert((*side, now, now));
                    // Reset timer if a different side takes over
                    if entry.0 != *side {
                        *entry = (*side, now, now);
                    } else {
                        // Refresh last_seen so the grace-period retain keeps this entry alive
                        entry.2 = now;
                    }
                    let elapsed = (now - entry.1).num_seconds();
                    let remaining = capture_secs - elapsed;
                    if is_new {
                        let obj_name = self.persisted.objectives.get(&oid)
                            .map(|o| o.name.clone())
                            .unwrap_or_else(|| "unknown".into());
                        let enemy = side.opposite();
                        self.ephemeral.msgs().panel_to_side(
                            15, false, *side,
                            format_compact!("Capturing {} ({} sec)", obj_name, remaining.max(0)),
                        );
                        self.ephemeral.msgs().panel_to_side(
                            15, false, enemy,
                            format_compact!("{} is being captured! Eliminate enemy troops!", obj_name),
                        );
                    }
                    if elapsed < capture_secs {
                        // Not enough time yet — skip capture this tick
                        continue;
                    }
                    // Timer elapsed — proceed with capture below
                }

                let obj = objective_mut!(self, oid)?;
                let name = obj.name.clone();
                let previous_owner = obj.owner;
                let new_owner = *side;
                obj.spawned = false;
                obj.threatened = true;
                obj.last_threatened_ts = now;
                obj.last_activate = now;
                obj.owner = new_owner;
                self.ephemeral.capture_progress.remove(&oid);
                actually_captured.push((*side, oid));
                self.ephemeral.msgs().panel_to_all(
                    15,
                    true,
                    format_compact!("BASE CAPTURE: {name} has been taken by {new_owner:?}!"),
                );
                for gid in obj.groups.get(&obj.owner).unwrap_or(&Set::new()) {
                    to_mark.push(*gid);
                }
                for gid in obj.groups.get(&obj.owner.opposite()).unwrap_or(&Set::new()) {
                    if let Some(id) = self.ephemeral.group_marks.remove(gid) {
                        self.ephemeral.msgs.delete_mark(id)
                    }
                    for uid in &group!(self, gid)?.units {
                        if !unit!(self, uid)?.dead {
                            self.ephemeral
                                .units_potentially_close_to_enemies
                                .insert(*uid);
                        }
                    }
                }
                let is_sam = objective!(self, oid)?.kind.is_special_sam_site();
                if !is_sam {
                    let abid = self
                        .ephemeral
                        .airbase_by_oid
                        .get(&oid)
                        .ok_or_else(|| anyhow!("no airbase for objective {:?}", oid))?;
                    let airbase =
                        Airbase::get_instance(lua, abid).context("getting captured airbase")?;
                    airbase
                        .set_coalition(*side)
                        .context("setting airbase coalition")?;
                }
                self.repair_one_logi_step(*side, now, oid)
                    .context("repairing captured airbase logi")?;
                self.repair_services(*side, now, oid)
                    .context("repairing captured airbase services")?;
                // Bring the new owner's defensive garrison (armour, infantry,
                // AAA, SAM) back to full. The .miz pre-places BOTH sides'
                // garrisons at every objective with the non-owner's units
                // dead, and nothing else revives them on capture -- so
                // without this a freshly-taken base sits near 0% health,
                // stays permanently re-capturable, and the capturing side
                // can't fix it (friendly troops don't trigger a capture).
                // Logi (gradual, via repair_one_logi_step) and services
                // (delayed, via repair_services) are deliberately left alone.
                {
                    let garrison: SmallVec<[GroupId; 16]> = objective!(self, oid)?
                        .groups
                        .get(side)
                        .into_iter()
                        .flat_map(|s| s.into_iter().copied())
                        .collect();
                    for gid in garrison {
                        let g = group!(self, &gid)?;
                        if g.class.is_logi() || g.class.is_services() {
                            continue;
                        }
                        let uids: SmallVec<[UnitId; 32]> =
                            g.units.into_iter().copied().collect();
                        for uid in uids {
                            unit_mut!(self, &uid)?.dead = false;
                        }
                        if objective!(self, oid)?.spawned {
                            self.ephemeral.push_spawn(gid);
                        }
                    }
                }
                self.capture_warehouse(lua, oid)
                    .context("capturing warehouse")?;
                self.sync_scenery_markers(oid);
                self.setup_supply_lines().context("setup supply lines")?;
                self.deliver_supplies_from_logistics_hubs(lua, now)
                    .context("delivering supplies")?;
                let mut ucids: SmallVec<[Ucid; 1]> = smallvec![];
                let mut hold_gids: Vec<GroupId> = vec![];
                for (_, ucid, troop_origin, gid) in gids {
                    hold_gids.push(gid);
                    if let Some(ucid) = ucid {
                        if previous_owner != new_owner || troop_origin != Some(oid) {
                            if !ucids.contains(&ucid) {
                                ucids.push(ucid);
                            }
                        }
                    }
                }
                // Keep the assault troops holding the base through the
                // consolidation window: garrison stays down and the base stays
                // capturable until they either survive the timer (consolidate)
                // or are wiped out (base goes Neutral). See check_capture_hold.
                let consolidation = self.ephemeral.cfg.capture_consolidation_secs;
                if consolidation > 0 && !hold_gids.is_empty() {
                    let obj = objective_mut!(self, oid)?;
                    obj.capture_hold = hold_gids;
                    obj.capture_hold_ts = Some(now);
                } else {
                    for gid in hold_gids {
                        self.delete_group(&gid)
                            .context("deleting capturing troops")?;
                    }
                }
                self.ephemeral.stat(Stat::Capture {
                    id: oid,
                    side: new_owner,
                    by: ucids.clone(),
                });
                if let Some(points) = self.ephemeral.cfg.points.as_ref() {
                    if !ucids.is_empty() {
                        let ppp = (points.capture as f32 / ucids.len() as f32).ceil() as i32;
                        for ucid in &ucids {
                            self.adjust_points(ucid, ppp, &format!("for capturing {name}"));
                        }
                    }
                }
                let obj = objective!(self, oid)?;
                self.ephemeral.create_objective_markup(&self.persisted, obj);
                self.ephemeral.dirty();
            }
        }
        // Clear capture progress only after a grace period with no troops in zone.
        // This prevents brief position-update gaps from resetting the timer.
        const CAPTURE_GRACE_SECS: i64 = 10;
        self.ephemeral.capture_progress.retain(|oid, entry| {
            in_zone_objectives.contains(oid)
                || (now - entry.2).num_seconds() < CAPTURE_GRACE_SECS
        });
        if actually_captured.len() > 0 {
            self.ephemeral.logistics_stage = LogiStage::SyncToWarehouses {
                objectives: self
                    .persisted
                    .objectives
                    .into_iter()
                    .map(|(oid, _)| *oid)
                    .collect(),
            };
        }
        for gid in to_mark {
            if let Err(e) = self
                .mark_group(&gid)
                .with_context(|| format_compact!("marking gid {gid} after capture"))
            {
                error!("{e:?}")
            }
        }
        Ok(actually_captured)
    }

    /// Resolve post-capture holds. A base captured with
    /// `capture_consolidation_secs > 0` is held only by the assaulting troop
    /// groups until either they survive the timer (consolidate -> garrison may
    /// spawn) or they are all wiped out (base goes Neutral).
    pub fn check_capture_hold(&mut self, now: DateTime<Utc>) -> Result<()> {
        let consolidation =
            Duration::seconds(self.ephemeral.cfg.capture_consolidation_secs as i64);
        let held: SmallVec<[ObjectiveId; 4]> = self
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, o)| !o.capture_hold.is_empty())
            .map(|(oid, _)| *oid)
            .collect();
        for oid in held {
            let (alive, started, name, owner) = {
                let obj = objective!(self, oid)?;
                let started = obj.capture_hold_ts.unwrap_or(now);
                let alive: Vec<GroupId> = obj
                    .capture_hold
                    .iter()
                    .copied()
                    .filter(|gid| {
                        self.group_health(gid).map(|(a, _)| a > 0).unwrap_or(false)
                    })
                    .collect();
                (alive, started, obj.name.clone(), obj.owner)
            };
            if alive.is_empty() {
                let obj = objective_mut!(self, oid)?;
                obj.capture_hold.clear();
                obj.capture_hold_ts = None;
                obj.owner = Side::Neutral;
                obj.spawned = false;
                obj.last_activate = now;
                self.ephemeral.msgs().panel_to_all(
                    15,
                    true,
                    format_compact!(
                        "{name}: the assault force was wiped out -- the base is contested (Neutral)"
                    ),
                );
                let obj = objective!(self, oid)?;
                self.ephemeral.create_objective_markup(&self.persisted, obj);
                self.ephemeral.dirty();
                self.sync_scenery_markers(oid);
            } else if now - started >= consolidation {
                let obj = objective_mut!(self, oid)?;
                obj.capture_hold.clear();
                obj.capture_hold_ts = None;
                obj.spawned = false;
                obj.last_activate = now;
                self.ephemeral.msgs().panel_to_side(
                    10,
                    false,
                    owner,
                    format_compact!("{name} consolidated -- garrison moving in"),
                );
                self.ephemeral.dirty();
            } else {
                let obj = objective_mut!(self, oid)?;
                if obj.capture_hold.len() != alive.len() {
                    obj.capture_hold = alive;
                    self.ephemeral.dirty();
                }
            }
        }
        Ok(())
    }

    pub fn update_objectives_markup(&mut self) -> Result<()> {
        // Collect objectives that need position tracking.
        // For carrier groups, collect the group IDs that belong to the carrier so we can
        // find live units by group membership (more reliable than template_name prefix matching,
        // which breaks if DCS unit names don't follow the "GROUPNAME-N" convention).
        enum PosLookup {
            /// FARP: look up unit by name directly
            ByName(String),
            /// Carrier: find any live unit belonging to these groups
            ByGroup(Set<GroupId>),
        }
        let mut pos_update: SmallVec<[(ObjectiveId, PosLookup); 8]> = smallvec![];
        for (id, obj) in &self.persisted.objectives {
            // Track mobile FARPs
            if let ObjectiveKind::Farp {
                mobile: true,
                pad_template,
                ..
            } = &obj.kind
                && let Zone::Circle { .. } = &obj.zone
            {
                pos_update.push((*id, PosLookup::ByName(pad_template.clone())))
            }
            // Track Carrier Groups
            if let ObjectiveKind::CarrierGroup { .. } = &obj.kind
                && let Zone::Circle { .. } = &obj.zone
            {
                // Clone the carrier's group set so we can look up units by group membership
                let groups = obj.groups.get(&obj.owner).cloned().unwrap_or_default();
                info!("[CARRIER_POS] Carrier objective {:?} '{}' owner={:?}, groups len={}, all_groups={:?}",
                      id, obj.name, obj.owner, groups.len(), obj.groups);
                if groups.len() > 0 {
                    pos_update.push((*id, PosLookup::ByGroup(groups)))
                }
            }
        }
        let mut moved: SmallVec<[ObjectiveId; 8]> = smallvec![];
        for (oid, lookup) in pos_update {
            // Find the unit position
            let unit_pos = match &lookup {
                PosLookup::ByGroup(groups) => {
                    // For carriers: find any live unit belonging to the carrier's groups
                    // Always track the actual DCS position so markers follow the carrier
                    let found = self.persisted.units.into_iter()
                        .find(|(_, unit)| !unit.dead && groups.contains(&unit.group));
                    if found.is_none() {
                        info!("[CARRIER_POS] No live unit found in carrier groups for objective {:?}, groups: {:?}", oid, groups);
                    }
                    found.map(|(_, unit)| unit.pos)
                }
                PosLookup::ByName(template) => {
                    // For FARPs: use direct name lookup (they keep template names)
                    self.persisted.units_by_name.get(template)
                        .and_then(|uid| self.persisted.units.get(uid))
                        .filter(|unit| !unit.dead)
                        .map(|unit| unit.pos)
                }
            };

            if let Some(unit_pos) = unit_pos {
                let obj = objective_mut!(self, oid)?;
                if let Zone::Circle { pos, .. } = &mut obj.zone
                    && pos != &unit_pos
                {
                    info!("[CARRIER_POS] Updating objective {:?} zone pos from ({:.0}, {:.0}) to ({:.0}, {:.0})",
                           oid, pos.x, pos.y, unit_pos.x, unit_pos.y);
                    *pos = unit_pos;
                    moved.push(oid);
                    self.ephemeral.dirty();
                }
            }
        }
        for (_, obj) in &self.persisted.objectives {
            self.ephemeral
                .update_objective_markup(&self.persisted, obj, &moved)
        }
        Ok(())
    }

    /// Resurrect and re-queue every ship group belonging to a carrier
    /// objective's current owner. Shared by the repair crate
    /// (`check_carrier_repairs`), the "Repair Carrier" action and the
    /// "Respawn Carrier" action -- the two actions used to just set
    /// `health = 100` and leave the ships on the seabed.
    pub(super) fn resurrect_carrier_groups(&mut self, oid: ObjectiveId) -> Result<()> {
        let obj = objective!(self, &oid)?;
        let owner = obj.owner;
        let gids: SmallVec<[GroupId; 4]> = obj
            .groups
            .get(&owner)
            .into_iter()
            .flat_map(|s| s.into_iter().copied())
            .collect();
        for gid in gids {
            let uids: SmallVec<[UnitId; 16]> =
                group!(self, gid)?.units.into_iter().copied().collect();
            for uid in uids {
                let unit = unit_mut!(self, uid)?;
                if unit.dead {
                    unit.dead = false;
                }
            }
            self.ephemeral.push_spawn(gid);
        }
        Ok(())
    }

    pub fn check_carrier_repairs(&mut self, now: DateTime<Utc>) -> Result<Vec<(ObjectiveId, String)>> {
        let base_repair_secs = self.ephemeral.cfg.carrier
            .as_ref()
            .map(|c| c.repair_time as i64)
            .unwrap_or(1800);

        let mut completed_repairs: Vec<(ObjectiveId, String)> = Vec::new();

        for (oid, obj) in self.persisted.objectives.iter_mut_cow() {
            if let ObjectiveKind::CarrierGroup { repair_start_time, .. } = &mut obj.kind {
                if let Some(start_time) = repair_start_time {
                    let elapsed = (now - *start_time).num_seconds();
                    // Each delivered repair crate divides the timer; floor at
                    // 60s so a pile of crates can't make it instant.
                    let crates = self.ephemeral.carrier_repair_crates.get(oid).copied().unwrap_or(1);
                    let repair_time_secs =
                        (base_repair_secs / crates.max(1) as i64).max(60);

                    if elapsed >= repair_time_secs {
                        // Repair complete! Resurrect all dead units and respawn groups
                        obj.health = 100;
                        obj.logi = 100;
                        obj.warehouse.damaged = false;
                        *repair_start_time = None;
                        self.ephemeral.carrier_repair_crates.remove(oid);

                        completed_repairs.push((*oid, obj.name.clone().into()));
                        info!("[CARRIER_REPAIR] {} fully repaired - resurrecting units", obj.name);
                        self.ephemeral.dirty();
                    } else {
                        // Log progress at certain intervals
                        let progress_pct = (elapsed as f64 / repair_time_secs as f64 * 100.0) as u8;
                        if elapsed % 300 == 0 && elapsed > 0 {  // Every 5 minutes
                            let remaining = repair_time_secs - elapsed;
                            debug!("[CARRIER_REPAIR] {} repair progress: {}% ({} minutes remaining)",
                                  obj.name, progress_pct, remaining / 60);
                        }
                    }
                }
            }
        }

        // Resurrect and respawn units for completed repairs
        for (oid, _) in &completed_repairs {
            self.resurrect_carrier_groups(*oid)?;
        }

        Ok(completed_repairs)
    }

    /// Check carrier health and auto-initiate repair if near naval base with supplies.
    /// Returns messages about carrier status changes.
    pub fn check_carrier_auto_repair(&mut self, now: DateTime<Utc>) -> Result<Vec<(Side, compact_str::CompactString)>> {
        let repair_cost = self.ephemeral.cfg.carrier
            .as_ref()
            .map(|c| c.repair_cost)
            .unwrap_or(5000);

        let mut messages = Vec::new();

        // Collect carrier states first to avoid borrow issues
        let carriers: Vec<_> = self.persisted.objectives.into_iter()
            .filter_map(|(oid, obj)| {
                if let ObjectiveKind::CarrierGroup { repair_start_time, parent_naval_base, .. } = &obj.kind {
                    if repair_start_time.is_none() && obj.health > 0 && obj.health < 75 {
                        Some((*oid, obj.owner, obj.health, obj.name.clone(), parent_naval_base.clone()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        for (oid, owner, health, name, parent_base) in carriers {
            if let Some(base_oid) = parent_base {
                // Check if naval base has enough supplies
                let base_obj = match self.persisted.objectives.get(&base_oid) {
                    Some(o) => o,
                    None => continue,
                };
                if base_obj.owner != owner {
                    continue;
                }
                let base_supplies = base_obj.warehouse.equipment
                    .get(&dcso3::String::from("SUPPLIES"))
                    .map(|inv| inv.stored)
                    .unwrap_or(0);
                if base_supplies >= repair_cost {
                    // Auto-initiate repair
                    if let Some(obj) = self.persisted.objectives.get_mut_cow(&oid) {
                        if let ObjectiveKind::CarrierGroup { repair_start_time, .. } = &mut obj.kind {
                            *repair_start_time = Some(now);
                            self.ephemeral.carrier_repair_crates.remove(&oid); // auto-repair runs at base speed
                            self.ephemeral.dirty();
                            info!("[CARRIER_AUTO_REPAIR] {} auto-repair initiated (health: {}%)", name, health);
                            messages.push((
                                owner,
                                compact_str::format_compact!(
                                    "NAVAL: {} auto-repair initiated ({}% health). Supplies deducted from naval base.",
                                    name,
                                    health,
                                ),
                            ));
                        }
                    }
                } else if health <= 25 {
                    messages.push((
                        owner,
                        compact_str::format_compact!(
                            "WARNING: {} critically damaged ({}% health)! Insufficient supplies at naval base for auto-repair.",
                            name,
                            health,
                        ),
                    ));
                }
            }
        }

        Ok(messages)
    }

    /// Shared side effects of a carrier group's ownership changing: swaps
    /// ship groups (despawns the old owner's, respawns/resurrects the new
    /// owner's), rebuilds the warehouse for the new owner (keeping any
    /// aircraft the new owner doesn't normally produce, per
    /// capture_warehouse's carrier branch), and reconnects supply lines.
    /// Returns the previous owner. Used both when a carrier is boarded and
    /// captured directly, and when its linked naval base is captured while
    /// the carrier is already disabled (see check_capture's NavalBase arm).
    /// Callers are responsible for troop cleanup, points, stats, and markup
    /// -- those differ (a naval-base-triggered flip has no boarding troops
    /// to award points to).
    fn flip_carrier_group_owner(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        now: DateTime<Utc>,
        oid: ObjectiveId,
        new_owner: Side,
    ) -> Result<Side> {
        let obj = objective_mut!(self, oid)?;
        let old_owner = obj.owner;

        obj.owner = new_owner;
        obj.health = 50; // Carrier starts at 50% after changing hands
        obj.logi = 100;
        obj.last_change_ts = now;
        obj.warehouse.damaged = false;
        if let ObjectiveKind::CarrierGroup { repair_start_time, .. } = &mut obj.kind {
            *repair_start_time = None; // a repair in flight belonged to the old owner
        }
        self.ephemeral.carrier_repair_crates.remove(&oid);

        // --- Despawn old owner's ship groups ---
        // The mission defines separate BCARRIER/RCARRIER group sets for each side.
        // On capture: remove old side's groups from DCS world, then spawn new side's groups.
        if let Some(old_groups) = obj.groups.get(&old_owner).cloned() {
            for gid in &old_groups {
                // Push despawn to remove from DCS world
                if let Some(live_oid) = self.ephemeral.object_id_by_gid.get(gid) {
                    self.ephemeral.push_despawn(*gid, Despawn::Group(live_oid.clone()));
                }
                // Mark all units dead in our DB
                if let Some(group) = self.persisted.groups.get(gid) {
                    for uid in &group.units {
                        if let Some(unit) = self.persisted.units.get_mut_cow(uid) {
                            unit.dead = true;
                        }
                    }
                }
            }
        }

        // --- Spawn new owner's ship groups (BCARRIER → Blue, RCARRIER → Red) ---
        let new_groups = objective!(self, oid)?.groups.get(&new_owner).cloned();
        if let Some(new_groups) = new_groups {
            // Pre-defined co-located group set (mission author placed both
            // sides' task forces at this carrier) -- just resurrect it.
            for gid in &new_groups {
                if let Some(group) = self.persisted.groups.get(gid) {
                    for uid in &group.units {
                        if let Some(unit) = self.persisted.units.get_mut_cow(uid) {
                            unit.dead = false;
                        }
                    }
                }
                self.ephemeral.push_spawn(*gid);
            }
        } else {
            // No pre-defined group set for the new owner -- spawn a fresh task
            // force from that side's own carrier template so a captured carrier
            // becomes a working carrier for the captor instead of a shipless
            // phantom objective with only F10 markers.
            self.ensure_carrier_task_force(lua, idx, oid, new_owner)?;
        }

        // Re-map warehouse capacity to new owner's production (keeps captured aircraft)
        self.capture_warehouse(lua, oid)
            .context("capturing carrier warehouse")?;

        // Recalculate supply lines
        self.setup_supply_lines()
            .context("setup supply lines after carrier ownership change")?;
        self.deliver_supplies_from_logistics_hubs(lua, now)
            .context("delivering supplies after carrier ownership change")?;

        Ok(old_owner)
    }

    /// Ensure carrier objective `oid` has a ship group set for `owner`,
    /// spawning a fresh task force from that side's own carrier template
    /// (RCARRIER / BCARRIER, found from their existing carrier group) if it
    /// doesn't. Positioned at the objective's last known ship centroid (any
    /// side, live or dead units), falling back to the objective zone centre.
    /// Returns true if a task force was spawned. No-op (with a warning) if
    /// that side has no carrier template in the mission.
    pub(super) fn ensure_carrier_task_force(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        oid: ObjectiveId,
        owner: Side,
    ) -> Result<bool> {
        let obj = objective!(self, &oid)?;
        let obj_name = obj.name.clone();
        if obj
            .groups
            .get(&owner)
            .map(|s| s.into_iter().next().is_some())
            .unwrap_or(false)
        {
            return Ok(false); // already has ships for this side
        }
        // Last known position of whatever ships this objective has/had.
        let pos = {
            let mut sum = Vector2::default();
            let mut n = 0u32;
            for (_, groups) in &obj.groups {
                for gid in groups {
                    if let Some(group) = self.persisted.groups.get(gid) {
                        for uid in &group.units {
                            if let Some(u) = self.persisted.units.get(uid) {
                                sum += u.pos;
                                n += 1;
                            }
                        }
                    }
                }
            }
            if n > 0 { sum / n as f64 } else { obj.zone.pos() }
        };
        let template = self.persisted.groups.into_iter().find_map(|(_, g)| {
            let n = g.name.as_str();
            if g.side == owner
                && matches!(g.class, ObjGroupClass::Naval)
                && n.contains("CARRIER")
                && !n.contains("ESCORT")
                && !n.contains("SUPPLY")
            {
                Some(g.template_name.clone())
            } else {
                None
            }
        });
        let Some(template) = template else {
            warn!(
                "[CARRIER_CAPTURE] {} has no ships for {:?} and the mission has no \
                 {:?} carrier template to spawn a replacement -- objective stays shipless",
                obj_name, owner, owner
            );
            return Ok(false);
        };
        let spctx = SpawnCtx::new(lua)?;
        let gid = self.add_and_queue_group(
            &spctx,
            idx,
            owner,
            SpawnLoc::AtPos {
                pos,
                offset_direction: Vector2::default(),
                group_heading: 0.,
            },
            template.as_str(),
            DeployKind::Objective { origin: oid },
            BitFlags::empty(),
            None,
        )?;
        self.persisted.objectives_by_group.insert_cow(gid, oid);
        let obj = objective_mut!(self, oid)?;
        obj.groups.get_or_default_cow(owner).insert_cow(gid);
        if let ObjectiveKind::CarrierGroup { carrier_template, .. } = &mut obj.kind {
            *carrier_template = template.clone();
        }
        // Any old deck-airbase mapping is stale; a mission reload re-registers
        // the new deck (mid-game deck slotting still needs a reload).
        self.ephemeral.airbase_by_oid.remove(&oid);
        info!(
            "[CARRIER_CAPTURE] {} given a fresh {:?} task force from template {}",
            obj_name, owner, template
        );
        Ok(true)
    }

    /// Spawn missing task forces for any carrier objective that is owned by a
    /// side but has no ship group for that side -- e.g. a carrier captured by
    /// an engine build that couldn't spawn the captor's ships, so the save has
    /// a shipless "phantom" carrier objective. Run once on load; self-heals
    /// without a progress reset.
    pub fn reconcile_carrier_task_forces(&mut self, lua: MizLua, idx: &MizIndex) -> Result<()> {
        let carriers: SmallVec<[(ObjectiveId, Side); 4]> = self
            .persisted
            .objectives
            .into_iter()
            .filter_map(|(oid, obj)| {
                let ObjectiveKind::CarrierGroup { .. } = &obj.kind else {
                    return None;
                };
                if obj.owner == Side::Neutral {
                    return None;
                }
                let has_ships = obj
                    .groups
                    .get(&obj.owner)
                    .map(|s| s.into_iter().next().is_some())
                    .unwrap_or(false);
                if has_ships { None } else { Some((*oid, obj.owner)) }
            })
            .collect();
        for (oid, owner) in carriers {
            if let Err(e) = self.ensure_carrier_task_force(lua, idx, oid, owner) {
                error!("reconcile carrier {:?} task force failed: {:?}", oid, e);
            }
        }
        Ok(())
    }

    pub fn check_carrier_group_capture(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        now: DateTime<Utc>,
    ) -> Result<Vec<(ObjectiveId, Side, Side)>> {
        // Capture time reuses campaign_events.capture_time_secs (0 = instant).
        // A missing campaign_events block still means the default 60s, not instant.
        let capture_secs = self
            .ephemeral
            .cfg
            .campaign_events
            .as_ref()
            .map_or(180, |c| c.capture_time_secs as i64);

        // --- Pass 1: collect which carriers have qualifying troops in zone ---
        // Map: carrier ObjectiveId → Vec<(capturing_side, Option<ucid>, group_id)>
        let mut in_zone: FxHashMap<ObjectiveId, Vec<(Side, Option<dcso3::net::Ucid>, GroupId)>> =
            FxHashMap::default();

        for (oid, obj) in &self.persisted.objectives {
            let ObjectiveKind::CarrierGroup { .. } = &obj.kind else {
                continue;
            };
            // Carrier is only capturable when logi == 0 (dead in the water)
            if obj.logi > 0 {
                continue;
            }

            // Scan troops
            for gid in &self.persisted.troops {
                let group = group!(self, gid)?;
                if let DeployKind::Troop { spec, player, .. } = &group.origin {
                    if !spec.can_capture {
                        continue;
                    }
                    let in_range = group
                        .units
                        .into_iter()
                        .filter_map(|uid| self.persisted.units.get(uid))
                        .any(|u| obj.zone.contains(u.pos));
                    if in_range {
                        in_zone
                            .entry(*oid)
                            .or_default()
                            .push((group.side, Some(*player), *gid));
                    }
                }
            }

            // Scan dismounts
            for gid in &self.persisted.dismounts {
                let group = group!(self, gid)?;
                if let DeployKind::Dismount { can_capture, .. } = &group.origin {
                    if !can_capture {
                        continue;
                    }
                    let in_range = group
                        .units
                        .into_iter()
                        .filter_map(|uid| self.persisted.units.get(uid))
                        .any(|u| obj.zone.contains(u.pos));
                    if in_range {
                        in_zone
                            .entry(*oid)
                            .or_default()
                            .push((group.side, None, *gid));
                    }
                }
            }
        }

        // --- Pass 2: apply capture logic for each eligible carrier ---
        let mut actually_captured: Vec<(ObjectiveId, Side, Side)> = Vec::new();
        let mut in_zone_oids: FxHashSet<ObjectiveId> = FxHashSet::default();

        for (oid, groups) in in_zone {
            let (captor_side, _, _) = *groups.first().ok_or_else(|| anyhow!("empty group list"))?;

            // All troops must belong to the same side
            if !groups.iter().all(|(s, _, _)| *s == captor_side) {
                in_zone_oids.insert(oid);
                continue;
            }

            // Must be different from current owner
            let current_owner = objective!(self, oid)?.owner;
            if captor_side == current_owner {
                // Friendly troops — don't capture own carrier
                continue;
            }

            in_zone_oids.insert(oid);

            // --- Momentum timer ---
            if capture_secs > 0 {
                let is_new = !self.ephemeral.capture_progress.contains_key(&oid);
                let entry = self
                    .ephemeral
                    .capture_progress
                    .entry(oid)
                    .or_insert((captor_side, now, now));
                if entry.0 != captor_side {
                    *entry = (captor_side, now, now);
                } else {
                    entry.2 = now;
                }
                let elapsed = (now - entry.1).num_seconds();
                let remaining = capture_secs - elapsed;
                if is_new {
                    let obj_name = self
                        .persisted
                        .objectives
                        .get(&oid)
                        .map(|o| o.name.clone())
                        .unwrap_or_else(|| "carrier".into());
                    let enemy = captor_side.opposite();
                    self.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        captor_side,
                        format_compact!("Boarding {} ({} sec)", obj_name, remaining.max(0)),
                    );
                    self.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        enemy,
                        format_compact!("{} is being boarded! Eliminate enemy troops!", obj_name),
                    );
                }
                if elapsed < capture_secs {
                    continue; // Not enough time yet
                }
                // Timer elapsed — fall through to capture
            }

            // --- Execute capture ---
            self.ephemeral.capture_progress.remove(&oid);

            let old_owner = self
                .flip_carrier_group_owner(lua, idx, now, oid, captor_side)
                .context("flipping carrier ownership on boarding capture")?;
            let obj_name = objective!(self, oid)?.name.clone();

            // Delete capturing troop groups and collect ucids for points
            let mut ucids: SmallVec<[dcso3::net::Ucid; 1]> = smallvec![];
            for (_, ucid, gid) in &groups {
                self.delete_group(gid).context("deleting boarding troops")?;
                if let Some(u) = ucid {
                    if !ucids.contains(u) {
                        ucids.push(*u);
                    }
                }
            }

            // Award capture points
            if let Some(points) = self.ephemeral.cfg.points.as_ref() {
                if !ucids.is_empty() {
                    let ppp = (points.capture as f32 / ucids.len() as f32).ceil() as i32;
                    for ucid in &ucids {
                        self.adjust_points(ucid, ppp, &format!("for capturing {obj_name}"));
                    }
                }
            }

            self.ephemeral.stat(Stat::Capture {
                id: oid,
                side: captor_side,
                by: ucids.clone(),
            });

            // Rebuild objective markup
            let obj = objective!(self, oid)?;
            self.ephemeral.create_objective_markup(&self.persisted, obj);
            self.ephemeral.dirty();

            actually_captured.push((oid, old_owner, captor_side));
            info!(
                "[CARRIER_CAPTURE] {} boarded and captured by {:?} from {:?}; warehouse aircraft transferred intact",
                obj_name, captor_side, old_owner
            );
        }

        // Clear capture_progress for carriers no longer being contested (with grace period)
        self.ephemeral
            .capture_progress
            .retain(|oid, entry| {
                in_zone_oids.contains(oid)
                    || (now - entry.2).num_seconds() < 10
            });

        // --- Pass 3: keep a disabled carrier's ownership in sync with its
        // linked naval base, whichever order the two fall in ---
        // Direct boarding (above) handles "carrier already disabled, then
        // boarded." This handles both remaining orderings: the naval base
        // falls first while the carrier is still active (nothing to do
        // until logi hits 0, then this catches it the same tick), and the
        // naval base falls after the carrier's already disabled (this
        // catches it next tick since it re-checks current state every
        // tick, not just at the moment the naval base capture happens).
        let mut naval_synced: SmallVec<[(ObjectiveId, Side, Side); 4]> = smallvec![];
        for (cg_id, cg_obj) in &self.persisted.objectives {
            let ObjectiveKind::CarrierGroup { parent_naval_base: Some(nb_id), .. } = &cg_obj.kind
            else {
                continue;
            };
            if cg_obj.logi > 0 {
                continue; // still operational -- naval base capture alone doesn't take it
            }
            let Ok(nb_obj) = objective!(self, nb_id) else {
                continue;
            };
            if nb_obj.owner == Side::Neutral || nb_obj.owner == cg_obj.owner {
                continue;
            }
            naval_synced.push((*cg_id, cg_obj.owner, nb_obj.owner));
        }
        for (cg_id, old_owner, new_owner) in naval_synced {
            match self.flip_carrier_group_owner(lua, idx, now, cg_id, new_owner) {
                Ok(_) => {
                    let cg = objective!(self, cg_id)?;
                    let cg_name = cg.name.clone();
                    self.ephemeral.create_objective_markup(&self.persisted, cg);
                    self.ephemeral.stat(Stat::Capture {
                        id: cg_id,
                        side: new_owner,
                        by: smallvec![],
                    });
                    self.ephemeral.dirty();
                    info!(
                        "[CARRIER_CAPTURE] {} auto-captured by {:?} from {:?} \
                         (disabled and its naval base is enemy-owned)",
                        cg_name, new_owner, old_owner
                    );
                    actually_captured.push((cg_id, old_owner, new_owner));
                }
                Err(e) => error!(
                    "failed to sync carrier {:?} to its naval base's owner: {:?}",
                    cg_id, e
                ),
            }
        }

        Ok(actually_captured)
    }
}
