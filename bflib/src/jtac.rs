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

use crate::{
    db::{Db, JtDesc, group::SpawnedUnit, player::InstancedPlayer},
    landcache::LandCache,
};
use anyhow::{Context, Result, anyhow, bail};
use bfprotocols::{
    cfg::{UnitTag, UnitTags, Vehicle, JtacState},
    db::{
        group::{GroupId, UnitId},
        objective::ObjectiveId,
    },
    stats::{DetectionSource, EnId, Stat},
};
use chrono::{Duration, prelude::*};
use compact_str::{CompactString, format_compact};
use dcso3::{
    LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3,
    coalition::Side,
    controller::{
        ActionTyp, AltType, AttackParams, Command, MissionPoint, PointType, Task, TurnMethod,
        VehicleFormation, WeaponExpend,
    },
    cvt_err, err,
    group::Group,
    land::Land,
    net::{SlotId, Ucid},
    object::{ClassObject, DcsObject, DcsOid, Object},
    radians_to_degrees, simple_enum,
    spot::{ClassSpot, Spot},
    trigger::{MarkId, SmokeColor, Trigger},
    unit::{ClassUnit, Unit},
    weapon::Weapon,
};
use enumflags2::BitFlags;
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use indexmap::IndexMap;
use log::{info, warn};
use mlua::{FromLua, IntoLua, Lua, Table, Value, prelude::LuaResult};
use rand::{Rng, thread_rng};
use serde::{Deserialize, Serialize};
use smallvec::{SmallVec, smallvec};
use std::{collections::hash_map::Entry, fmt, str::FromStr};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum JtId {
    Group(GroupId),
    Slot(SlotId),
}

impl FromStr for JtId {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        if let Some(s) = s.strip_prefix("sl") {
            Ok(JtId::Slot(SlotId::Unit(s.parse()?)))
        } else {
            Ok(JtId::Group(s.parse()?))
        }
    }
}

impl<'lua> FromLua<'lua> for JtId {
    fn from_lua(value: Value<'lua>, lua: &'lua Lua) -> LuaResult<Self> {
        let tbl: Table = FromLua::from_lua(value, lua)?;
        match tbl.raw_get::<_, i64>("kind")? {
            0 => Ok(Self::Group(tbl.raw_get("id")?)),
            1 => Ok(Self::Slot(tbl.raw_get("id")?)),
            n => Err(err(&format_compact!("invalid jtid {n}"))),
        }
    }
}

impl<'lua> IntoLua<'lua> for JtId {
    fn into_lua(self, lua: &'lua Lua) -> LuaResult<Value<'lua>> {
        let tbl = lua.create_table()?;
        match self {
            Self::Group(id) => {
                tbl.raw_set("kind", 0)?;
                tbl.raw_set("id", id)?
            }
            Self::Slot(id) => {
                tbl.raw_set("kind", 1)?;
                tbl.raw_set("id", id)?;
            }
        }
        Ok(Value::Table(tbl))
    }
}

impl fmt::Display for JtId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Group(id) => write!(f, "{id}"),
            Self::Slot(id) => write!(f, "sl{id}"),
        }
    }
}

fn ui_jtac_dead(db: &mut Db, side: Side, gid: JtId, name: Option<&CompactString>) {
    let display = match name {
        Some(n) => format_compact!("{n} ({gid})"),
        None => format_compact!("{gid}"),
    };
    db.ephemeral.msgs().panel_to_side(
        10,
        false,
        side,
        format_compact!("JTAC {display} is no longer available"),
    )
}

simple_enum!(AdjustmentDir, u8, [
    Short => 0,
    Long => 1,
    Left => 2,
    Right => 3
]);

#[derive(Debug, Clone)]
pub struct ArtilleryAdjustment {
    adjust: Vector2,
    target: Vector2,
    group: Vec<DcsOid<ClassUnit>>,
    tracked: Option<(Weapon<'static>, Option<Vector3>)>,
}

/// How far (m) a battery is nudged toward its target before firing so that
/// hull-traverse launchers (Grad, MLRS, Smerch, Scud-B, Silkworm, ...) physically
/// slew to face the target instead of relying on turret traverse alone.
const ARTILLERY_AIM_NUDGE_M: f64 = 35.0;
/// If the battery is already within this many radians of the target bearing it
/// fires in place -- avoids walking tube artillery downrange over repeated fire
/// missions once it is lined up. ~17 degrees.
const ARTILLERY_AIM_TOLERANCE_RAD: f64 = 0.30;

/// Current facing (radians, DCS azimuth) of the first alive unit in `gid`.
pub(crate) fn group_facing(db: &Db, gid: &GroupId) -> Option<f64> {
    let g = db.group(gid).ok()?;
    g.units
        .into_iter()
        .find_map(|uid| db.unit(uid).ok().filter(|u| !u.dead).map(|u| u.heading))
}

/// Wrap `fire_task` in a ground `Task::Mission`. If the battery is not already
/// pointed at `target`, the mission first walks it ~`ARTILLERY_AIM_NUDGE_M`
/// toward the target so the AI reorients the hull, then fires; otherwise it
/// fires from `center` without moving. `facing` is the battery's current heading
/// in radians (see `group_facing`).
pub(crate) fn aim_and_fire_route<'lua>(
    center: Vector2,
    target: Vector2,
    facing: Option<f64>,
    fire_task: Task<'lua>,
) -> Task<'lua> {
    let delta = target - center;
    let bearing = dcso3::azumith2d(delta);
    let aligned = facing
        .map(|h| {
            let mut d = (bearing - h).abs() % std::f64::consts::TAU;
            if d > std::f64::consts::PI {
                d = std::f64::consts::TAU - d;
            }
            d <= ARTILLERY_AIM_TOLERANCE_RAD
        })
        .unwrap_or(false);
    let (pos, speed) = if aligned || delta.norm() < 1.0 {
        (center, 0.0)
    } else {
        (center + delta.normalize() * ARTILLERY_AIM_NUDGE_M, 5.5)
    };
    Task::Mission {
        airborne: Some(false),
        route: vec![MissionPoint {
            action: Some(ActionTyp::Ground(VehicleFormation::OffRoad)),
            typ: PointType::TurningPoint,
            airdrome_id: None,
            helipad: None,
            time_re_fu_ar: None,
            link_unit: None,
            pos: LuaVec2(pos),
            alt: 0.,
            alt_typ: Some(AltType::RADIO),
            speed,
            speed_locked: None,
            eta: None,
            eta_locked: None,
            name: None,
            task: Box::new(fire_task),
        }],
    }
}

type LocByCode = FxHashMap<Side, FxHashMap<ObjectiveId, FxHashMap<u16, FxHashSet<JtId>>>>;

#[derive(Debug, Clone, Default)]
pub struct Contact {
    pub pos: Vector3,
    pub typ: Vehicle,
    pub tags: UnitTags,
    pub last_move: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone)]
pub struct JtacTarget {
    pub id: EnId,
    pub pos: Vector3,
    pub typ: Vehicle,
    source: DcsOid<ClassUnit>,
    spot: DcsOid<ClassSpot>,
    ir_pointer: Option<DcsOid<ClassSpot>>,
    mark: Option<MarkId>,
}

impl JtacTarget {
    fn destroy(self, lua: MizLua) -> Result<()> {
        Spot::get_instance(lua, &self.spot)
            .context("getting laser spot")?
            .destroy()
            .context("destroying laser spot")?;
        if let Some(ir_pointer) = self.ir_pointer {
            Spot::get_instance(lua, &ir_pointer)
                .context("getting ir pointer")?
                .destroy()
                .context("destroying ir pointer")?
        }
        if let Some(id) = self.mark {
            Trigger::singleton(lua)?
                .action()?
                .remove_mark(id)
                .context("removing mark")?
        }
        Ok(())
    }
}

/// A designated logistics/scenery building target (from `scan_objective_scenery`).
/// Kept separate from `JtacTarget` since buildings aren't `EnId` contacts --
/// they don't die via unit-kill events, they're polled for destruction by
/// `check_scenery_buildings`.
#[derive(Debug, Clone)]
struct BuildingTarget {
    id: DcsOid<ClassObject>,
    label: CompactString,
    spot: DcsOid<ClassSpot>,
    mark: Option<MarkId>,
}

impl BuildingTarget {
    fn destroy(self, lua: MizLua) -> Result<()> {
        Spot::get_instance(lua, &self.spot)
            .context("getting laser spot")?
            .destroy()
            .context("destroying laser spot")?;
        if let Some(id) = self.mark {
            Trigger::singleton(lua)?
                .action()?
                .remove_mark(id)
                .context("removing mark")?
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct JtacLocation {
    pub pos: Vector2,
    pub oid: ObjectiveId,
    pub bearing: f64,
    pub distance: f64,
}

impl JtacLocation {
    fn new(db: &Db, pos: Vector3) -> Self {
        let pos = Vector2::new(pos.x, pos.z);
        let (distance, bearing, obj) =
            Db::objective_near_point(&db.persisted.objectives, pos, |_| true).unwrap();
        Self {
            pos,
            oid: obj.id,
            bearing,
            distance,
        }
    }
}

pub struct ContactsIter<'a> {
    contacts: Vec<indexmap::map::Iter<'a, EnId, Contact>>,
    i: usize,
}

impl<'a> Iterator for ContactsIter<'a> {
    type Item = (&'a EnId, &'a Contact);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.contacts.len() == 0 {
                break None;
            }
            if self.i < self.contacts.len() {
                match self.contacts[self.i].next() {
                    Some(item) => {
                        self.i += 1;
                        break Some(item);
                    }
                    None => {
                        self.contacts.remove(self.i);
                        self.i += 1;
                    }
                }
            } else {
                self.i = 0;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Jtac {
    gid: JtId,
    name: Option<CompactString>,
    side: Side,
    contacts: IndexMap<EnId, Contact>,
    filter: BitFlags<UnitTag>,
    location: JtacLocation,
    priority: Vec<UnitTags>,
    target: Option<JtacTarget>,
    autoshift: Option<usize>,
    ir_pointer: bool,
    code: u16,
    lase_range_m: f64,
    last_smoke: DateTime<Utc>,
    nearby_artillery: SmallVec<[GroupId; 8]>,
    nearby_alcm: SmallVec<[(GroupId, i32); 8]>,
    menu_dirty: bool,
    air: bool,
    building_target: Option<BuildingTarget>,
    building_idx: usize,
}

impl Jtac {
    pub fn state(&self) -> JtacState {
        JtacState {
            filter: self.filter,
            priority: self.priority.clone(),
            autoshift: self.autoshift,
            ir_pointer: self.ir_pointer,
            code: self.code,
        }
    }

    pub fn apply_state(&mut self, state: JtacState) {
        self.filter = state.filter;
        self.priority = state.priority;
        self.autoshift = state.autoshift;
        self.ir_pointer = state.ir_pointer;
        self.code = state.code;
    }

    fn persist_state(&self, db: &mut Db) {
        use crate::db::group::DeployKind;
        if let JtId::Group(gid) = self.gid {
            if let Some(group) = db.persisted.groups.get_mut_cow(&gid) {
                let state = bfprotocols::cfg::JtacState {
                    filter: self.filter,
                    priority: self.priority.clone(),
                    autoshift: self.autoshift,
                    ir_pointer: self.ir_pointer,
                    code: self.code,
                };
                match &mut group.origin {
                    DeployKind::Deployed { jtac, .. } => *jtac = Some(state),
                    DeployKind::Action { jtac, .. } => *jtac = Some(state),
                    DeployKind::Troop { jtac, .. } => *jtac = Some(state),
                    _ => {}
                }
            }
        }
    }

    fn new(
        db: &Db,
        gid: JtId,
        name: Option<CompactString>,
        side: Side,
        priority: Vec<UnitTags>,
        pos: Vector3,
        air: bool,
        default_laser_code: u16,
        lase_range_m: f64,
    ) -> Self {
        Self {
            gid,
            name,
            side,
            contacts: IndexMap::default(),
            filter: BitFlags::default(),
            priority,
            location: JtacLocation::new(db, pos),
            target: None,
            autoshift: None,
            ir_pointer: false,
            code: default_laser_code,
            lase_range_m,
            last_smoke: DateTime::<Utc>::default(),
            nearby_artillery: smallvec![],
            nearby_alcm: smallvec![],
            menu_dirty: false,
            air,
            building_target: None,
            building_idx: 0,
        }
    }

    pub fn status(&self, db: &Db, loc_by_code: &LocByCode) -> Result<CompactString> {
        use std::fmt::Write;
        fn get_typ(db: &Db, id: &EnId) -> Result<Vehicle> {
            Ok(match id {
                EnId::Unit(uid) => db.unit(uid)?.typ.clone(),
                EnId::Player(ucid) => db
                    .player(ucid)
                    .and_then(|p| p.current_slot.as_ref())
                    .and_then(|(_, i)| i.as_ref())
                    .map(|i| i.typ.clone())
                    .ok_or_else(|| anyhow!("player {ucid} isn't instanced"))?,
            })
        }
        let mut msg = CompactString::new("");
        match &self.name {
            Some(n) => write!(msg, "JTAC {} ({}) status\n", n, self.gid)?,
            None => write!(msg, "JTAC {} status\n", self.gid)?,
        }
        match &self.target {
            None => {
                write!(msg, "no target\n")?;
            }
            Some(target) => {
                let unit_typ = get_typ(db, &target.id)?;
                let mid = match target.mark {
                    None => format_compact!("none"),
                    Some(mid) => format_compact!("{mid}"),
                };
                let conflicts = loc_by_code
                    .get(&self.side)
                    .and_then(|by_side| by_side.get(&self.location.oid))
                    .and_then(|by_code| by_code.get(&self.code))
                    .and_then(|gids| {
                        let len = gids.len();
                        if len <= 1 {
                            None
                        } else {
                            let mut msg = CompactString::new("(code conflicts with [");
                            for (i, gid) in gids.iter().filter(|gid| **gid != self.gid).enumerate()
                            {
                                if i < len - 2 {
                                    write!(msg, "{gid}, ").unwrap()
                                } else {
                                    write!(msg, "{gid}").unwrap()
                                }
                            }
                            write!(msg, "])").unwrap();
                            Some(String::from(msg))
                        }
                    })
                    .unwrap_or(String::from(""));
                write!(
                    msg,
                    "lasing {unit_typ} code {}{} marker {mid}\n",
                    self.code, conflicts
                )?;
            }
        };
        if let Some(bt) = &self.building_target {
            write!(msg, "designating building: {}\n", bt.label)?;
        }
        write!(
            msg,
            "position bearing {} for {:.1}km from {}\n\n",
            radians_to_degrees(self.location.bearing) as u32,
            self.location.distance / 1000.,
            db.objective(&self.location.oid)?.name
        )?;
        if self.contacts.is_empty() {
            write!(msg, "No enemies in sight")?;
        } else {
            let mut counts: IndexMap<Vehicle, usize, FxBuildHasher> = IndexMap::default();
            for id in self.contacts.keys() {
                let typ = get_typ(db, id)?;
                *counts.entry(typ).or_insert(0) += 1;
            }
            write!(msg, "Visual On: ")?;
            for (i, (typ, count)) in counts.into_iter().enumerate() {
                if i == self.contacts.len() - 1 {
                    if count > 1 {
                        write!(msg, "{}x{}", typ, count)?;
                    } else {
                        write!(msg, "{}", typ)?;
                    }
                } else {
                    if count > 1 {
                        write!(msg, "{}x{}, ", typ, count)?;
                    } else {
                        write!(msg, "{}, ", typ)?;
                    }
                }
            }
        }
        write!(
            msg,
            "\n\nautoshift: {}, ir_pointer: {}",
            self.autoshift.is_none(),
            self.ir_pointer
        )?;
        write!(msg, "\nfilter: [")?;
        let len = self.filter.len();
        for (i, tag) in self.filter.iter().enumerate() {
            if i < len - 1 {
                write!(msg, "{:?}, ", tag)?;
            } else {
                write!(msg, "{:?}", tag)?;
            }
        }
        write!(msg, "]\n")?;
        write!(msg, "available artillery: [")?;
        let len = self.nearby_artillery.len();
        for (i, gid) in self.nearby_artillery.iter().enumerate() {
            if i < len - 1 {
                write!(msg, "{gid},")?;
            } else {
                write!(msg, "{gid}")?;
            }
        }
        write!(msg, "]\n")?;
        write!(msg, "available ALCM: [")?;
        let len = self.nearby_alcm.len();
        for (i, (gid, ammo)) in self.nearby_alcm.iter().enumerate() {
            if i < len - 1 {
                write!(msg, "{gid}({ammo}),")?;
            } else {
                write!(msg, "{gid}({ammo})")?;
            }
        }
        write!(msg, "]")?;
        Ok(msg)
    }

    pub fn callsign(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn nine_line(&self, db: &Db) -> Result<CompactString> {
        use std::fmt::Write;
        let target = match &self.target {
            None => bail!("no target — lase a target first"),
            Some(t) => t,
        };
        let target_pos2 = Vector2::new(target.pos.x, target.pos.z);
        let jtac_pos2 = self.location.pos;
        let diff = target_pos2 - jtac_pos2;
        let dist_m = diff.magnitude();
        let bearing_rad = diff.y.atan2(diff.x).rem_euclid(std::f64::consts::TAU);
        let brg_deg = radians_to_degrees(bearing_rad) as u32;
        let recip_deg = (brg_deg + 180) % 360;
        let elev_ft = (target.pos.y * 3.28084) as i32;
        let (tgt_dist_km, tgt_brg, tgt_obj) =
            Db::objective_near_point(&db.persisted.objectives, target_pos2, |_| true)
                .context("no objectives found")?;
        let tgt_obj_name = &tgt_obj.name;
        let tgt_brg_deg = radians_to_degrees(tgt_brg) as u32;
        let jtac_brg_deg = radians_to_degrees(self.location.bearing) as u32;
        let ip_name = db.objective(&self.location.oid)?.name.clone();
        let jtac_name = match &self.name {
            Some(n) => format_compact!("{n}"),
            None => format_compact!("{}", self.gid),
        };
        let mut msg = CompactString::new("");
        write!(msg, "== 9-LINE CAS - {jtac_name} ==\n")?;
        write!(msg, "1. IP: {ip_name} (JTAC {jtac_brg_deg}° / {:.1}km from IP)\n",
            self.location.distance / 1000.0)?;
        write!(msg, "2. HDG: {brg_deg}° to target (egress {recip_deg}°)\n")?;
        write!(msg, "3. DIST: {:.1}km - ELEV: {elev_ft}ft\n", dist_m / 1000.0)?;
        write!(msg, "4. TGT: {}\n", target.typ)?;
        write!(msg, "5. TGT POS: {tgt_brg_deg}° / {:.1}km from {tgt_obj_name}\n",
            tgt_dist_km / 1000.0)?;
        write!(msg, "6. MARK: LASER {}\n", self.code)?;
        write!(msg, "7. FRIENDLIES: {:.0} side\n", self.side)?;
        write!(msg, "8. EGRESS: {recip_deg}°\n")?;
        let contact_count = self.contacts.len();
        write!(msg, "9. REMARKS: {contact_count} contact(s) visible")?;
        if self.ir_pointer {
            write!(msg, ", IR pointer active")?;
        }
        Ok(msg)
    }

    fn add_unit_contact(&mut self, unit: &SpawnedUnit) {
        let ct = self.contacts.entry(EnId::Unit(unit.id)).or_default();
        ct.pos = unit.position.p.0;
        ct.last_move = unit.moved;
        ct.tags = unit.tags;
        ct.typ = unit.typ.clone();
    }

    fn add_player_contact(&mut self, ucid: Ucid, inst: &InstancedPlayer) {
        let ct = self.contacts.entry(EnId::Player(ucid)).or_default();
        ct.pos = inst.position.p.0;
    }

    fn remove_target(&mut self, db: &mut Db, lua: MizLua) -> Result<()> {
        if let Some(target) = self.target.take() {
            target
                .destroy(lua)
                .with_context(|| format_compact!("destroying target for jtac {}", self.gid))?;
        }
        if let JtId::Group(gid) = self.gid {
            let (map_layer, msgs) = db.ephemeral.map_layer_and_msgs();
            map_layer.on_jtac_cleared(&gid, msgs);
        }
        Ok(())
    }

    fn mark_target(&mut self, lua: MizLua) -> Result<()> {
        if let Some(target) = &mut self.target {
            let act = Trigger::singleton(lua)?.action()?;
            if let Some(mid) = target.mark.take() {
                act.remove_mark(mid).context("removing mark")?;
            }
            let new_mid = MarkId::new();
            let ct = &self.contacts[&target.id];
            let msg = format_compact!(
                "JTAC {} target {} marked by code {}",
                self.gid,
                ct.typ,
                self.code
            );
            act.mark_to_coalition(new_mid, msg.into(), LuaVec3(ct.pos), self.side, true, None)
                .context("marking target")?;
            target.mark = Some(new_mid);
        }
        Ok(())
    }

    fn set_target(&mut self, db: &mut Db, lua: MizLua, i: usize) -> Result<bool> {
        let (id, ct) = self
            .contacts
            .get_index(i)
            .ok_or_else(|| anyhow!("no such target"))?;
        let id = *id;
        let pos = ct.pos;
        let prev_arty = self.nearby_artillery.clone();
        let prev_alcm = self.nearby_alcm.clone();
        match &self.target {
            Some(target) if target.id == id => {
                self.nearby_artillery =
                    db.artillery_near_point(self.side, Vector2::new(pos.x, pos.z));
                self.menu_dirty |= prev_arty != self.nearby_artillery;

                self.nearby_alcm = db.alcm_near_point(self.side, lua, Vector2::new(pos.x, pos.z));
                self.menu_dirty |= prev_alcm != self.nearby_alcm;

                Ok(false)
            }
            Some(_) | None => {
                let typ = ct.typ.clone();
                self.remove_target(db, lua)?;
                let jtid = match &self.gid {
                    JtId::Group(gid) => db
                        .first_living_unit(gid)
                        .context("getting jtac beam source")?
                        .clone(),
                    JtId::Slot(sl) => db
                        .ephemeral
                        .get_object_id_by_slot(sl)
                        .ok_or_else(|| anyhow!("no unit for slot {sl}"))?
                        .clone(),
                };
                let jt = match Unit::get_instance(lua, &jtid) {
                    Ok(jt) => jt,
                    Err(_) => {
                        info!("jtac unit died while setting target {:?}", jtid);
                        return Ok(true);
                    }
                };
                let offset = if self.air {
                    Vector3::new(0., -5., 0.)
                } else {
                    Vector3::new(0., 10., 0.)
                };
                let spot = Spot::create_laser(
                    lua,
                    jt.as_object()?,
                    Some(LuaVec3(offset)),
                    LuaVec3(pos),
                    self.code,
                )
                .context("creating laser spot")?
                .object_id()?;
                let ir_pointer = if self.ir_pointer {
                    Some(
                        Spot::create_infra_red(
                            lua,
                            jt.as_object()?,
                            Some(LuaVec3(Vector3::new(0., 5., 0.))),
                            LuaVec3(pos),
                        )
                        .context("creating ir pointer spot")?
                        .object_id()?,
                    )
                } else {
                    None
                };
                self.target = Some(JtacTarget {
                    pos,
                    spot,
                    typ,
                    source: jtid,
                    ir_pointer,
                    mark: None,
                    id,
                });
                self.nearby_artillery =
                    db.artillery_near_point(self.side, Vector2::new(pos.x, pos.z));
                self.nearby_alcm = db.alcm_near_point(self.side, lua, Vector2::new(pos.x, pos.z));
                self.menu_dirty |= prev_arty != self.nearby_artillery;
                self.menu_dirty |= prev_alcm != self.nearby_alcm;
                self.mark_target(lua).context("marking target")?;
                if let JtId::Group(gid) = self.gid {
                    let target_pos2 = Vector2::new(pos.x, pos.z);
                    let text = {
                        let t = self.target.as_ref().unwrap();
                        format_compact!("JTAC {}\nlasing {}\nCode: {}", self.gid, t.typ, self.code)
                    };
                    let (map_layer, msgs) = db.ephemeral.map_layer_and_msgs();
                    map_layer.on_jtac_target(
                        gid,
                        self.location.pos,
                        target_pos2,
                        self.lase_range_m,
                        self.side,
                        text,
                        msgs,
                    );
                }
                Ok(true)
            }
        }
    }

    /// Cycle through the logistics-relevant scenery buildings (the ones pinned
    /// with logi markers on the F10 map, from `scan_objective_scenery`) tracked
    /// at this JTAC's nearest objective, lase the next one, and call it out to
    /// the JTAC's side. Returns the label of the designated building, or `None`
    /// if there are none left standing there.
    pub fn designate_building(&mut self, db: &mut Db, lua: MizLua) -> Result<Option<CompactString>> {
        let candidates = db.ephemeral.scenery_at_objective(self.location.oid);
        if candidates.is_empty() {
            self.remove_building_target(lua)?;
            return Ok(None);
        }
        self.building_idx = (self.building_idx + 1) % candidates.len();
        let (id, label) = candidates[self.building_idx].clone();
        if let Some(bt) = &self.building_target {
            if bt.id == id {
                return Ok(Some(bt.label.clone()));
            }
        }
        let pos = match Object::get_instance(lua, &id) {
            Ok(o) => o.get_point().context("getting building position")?.0,
            Err(_) => {
                // stale entry, e.g. destroyed between scan and now -- retry next cycle
                self.remove_building_target(lua)?;
                return Ok(None);
            }
        };
        self.remove_building_target(lua)?;
        let jtid = match &self.gid {
            JtId::Group(gid) => db
                .first_living_unit(gid)
                .context("getting jtac beam source")?
                .clone(),
            JtId::Slot(sl) => db
                .ephemeral
                .get_object_id_by_slot(sl)
                .ok_or_else(|| anyhow!("no unit for slot {sl}"))?
                .clone(),
        };
        let jt = Unit::get_instance(lua, &jtid).context("getting jtac unit")?;
        let offset = if self.air {
            Vector3::new(0., -5., 0.)
        } else {
            Vector3::new(0., 10., 0.)
        };
        let spot = Spot::create_laser(lua, jt.as_object()?, Some(LuaVec3(offset)), LuaVec3(pos), self.code)
            .context("creating laser spot")?
            .object_id()?;
        let mid = MarkId::new();
        let diff = Vector2::new(pos.x, pos.z) - self.location.pos;
        let brg_deg = radians_to_degrees(diff.y.atan2(diff.x).rem_euclid(std::f64::consts::TAU)) as u32;
        let msg = format_compact!(
            "JTAC {} designating building: {label} (logistics target) {brg_deg}° / {:.1}km, code {}",
            self.gid,
            diff.magnitude() / 1000.,
            self.code
        );
        Trigger::singleton(lua)?
            .action()?
            .mark_to_coalition(mid, msg.clone().into(), LuaVec3(pos), self.side, true, None)
            .context("marking building target")?;
        self.building_target = Some(BuildingTarget {
            id,
            label: label.clone(),
            spot,
            mark: Some(mid),
        });
        db.ephemeral.msgs().panel_to_side(15, false, self.side, msg);
        Ok(Some(label))
    }

    fn remove_building_target(&mut self, lua: MizLua) -> Result<()> {
        if let Some(target) = self.building_target.take() {
            target
                .destroy(lua)
                .with_context(|| format_compact!("destroying building target for jtac {}", self.gid))?;
        }
        Ok(())
    }

    fn set_code(&mut self, lua: MizLua, code_part: u16) -> Result<()> {
        let thousands = code_part / 1000;
        let hundreds = code_part / 100;
        let tens = code_part / 10;
        if thousands > 9 || (thousands > 0 && code_part % 1000 > 0) || (hundreds > 0 && code_part % 100 > 0) || (tens > 0 && code_part % 10 > 0)
        {
            bail!("invalid code part {code_part}, mixed scales")
        }
        if thousands > 0 {
            let rest = self.code % 1000;
            self.code = code_part + rest;
        } else if hundreds > 0 {
            let thou = self.code / 1000;
            let tens_ones = self.code % 100;
            self.code = 1000 * thou + code_part + tens_ones;
        } else if tens > 0 {
            let thou_hund = self.code / 100;
            let ones = self.code % 10;
            self.code = 100 * thou_hund + code_part + ones;
        } else {
            let c = self.code / 10;
            self.code = 10 * c + code_part;
        }
        if let Some(target) = &self.target {
            let spot = Spot::get_instance(lua, &target.spot).context("getting laser spot")?;
            spot.set_code(self.code).context("setting laser code")?;
            self.mark_target(lua).context("marking target")?
        }
        Ok(())
    }

    pub fn shift(&mut self, db: &mut Db, lua: MizLua) -> Result<bool> {
        if self.contacts.is_empty() {
            return Ok(false);
        }
        let i = match (self.autoshift, &self.target) {
            (None, None) => 0,
            (None, Some(target)) => match self.contacts.get_index_of(&target.id) {
                None => 0,
                Some(i) => {
                    if i < self.contacts.len() - 1 {
                        i + 1
                    } else {
                        0
                    }
                }
            },
            (Some(i), _) => {
                if i < self.contacts.len() - 1 {
                    i + 1
                } else {
                    0
                }
            }
        };
        self.autoshift = Some(i);
        self.set_target(db, lua, i).context("setting target")
    }

    fn remove_contact(&mut self, lua: MizLua, db: &mut Db, id: &EnId) -> Result<bool> {
        if let Some(_) = self.contacts.swap_remove(id) {
            if let Some(target) = &self.target {
                if &target.id == id {
                    self.remove_target(db, lua).context("removing target")?;
                    // The shifted target is gone -- fall back to auto so
                    // sort_contacts re-acquires the next one.
                    self.autoshift = None;
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    fn sort_contacts(&mut self, db: &mut Db, lua: MizLua) -> Result<bool> {
        let plist = &self.priority;
        let priority = |tags: UnitTags| {
            plist
                .iter()
                .enumerate()
                .find(|(_, p)| tags.contains(p.0))
                .map(|(i, _)| i)
                .unwrap_or(plist.len())
        };
        self.contacts
            .sort_by(|_, ct0, _, ct1| priority(ct0.tags).cmp(&priority(ct1.tags)));
        // Auto-acquire the top-priority contact when in auto mode, OR any time
        // we have contacts but no target at all (e.g. the manually-shifted
        // target just died/left -- don't sit on "no target" while enemies are
        // still in view).
        if !self.contacts.is_empty() && (self.autoshift.is_none() || self.target.is_none()) {
            let i = match self.autoshift {
                Some(i) if i < self.contacts.len() => i,
                _ => 0,
            };
            return self.set_target(db, lua, i).context("setting target");
        }
        Ok(false)
    }

    pub fn smoke_target(&mut self, lua: MizLua) -> Result<()> {
        if let Some(target) = &self.target {
            if let Some(ct) = self.contacts.get(&target.id) {
                let now = Utc::now();
                if now - self.last_smoke < Duration::seconds(60) {
                    let rdy = (now - self.last_smoke).num_seconds();
                    bail!("smoke will not be ready for another {}s", rdy)
                }
                self.last_smoke = now;
                let mut rng = thread_rng();
                let act = Trigger::singleton(lua)?.action()?;
                let land = Land::singleton(lua)?;
                let pos = Vector2::new(
                    ct.pos.x + rng.gen_range(0. ..10.),
                    ct.pos.z + rng.gen_range(0. ..10.),
                );
                let pos = Vector3::new(pos.x, land.get_height(LuaVec2(pos))?, pos.y);
                let color = match self.side {
                    Side::Blue => SmokeColor::Red,
                    Side::Red => SmokeColor::Blue,
                    Side::Neutral => SmokeColor::Green,
                };
                act.smoke(LuaVec3(pos), color).context("creating smoke")?;
            }
        }
        Ok(())
    }

    fn reset_target(&mut self, db: &mut Db, lua: MizLua) -> Result<()> {
        if let Some(target) = &self.target {
            if let Some(i) = self.contacts.get_index_of(&target.id) {
                self.remove_target(db, lua)?;
                self.set_target(db, lua, i).context("setting jtac target")?;
            }
        }
        Ok(())
    }

    pub fn artillery_mission(
        &mut self,
        db: &Db,
        lua: MizLua,
        adjustment: &mut ArtilleryAdjustment,
        gid: &GroupId,
        n: u8,
    ) -> Result<()> {
        match self.target.as_mut() {
            None => bail!("no target"),
            Some(target) => {
                let name = db.group(gid)?.name.clone();
                let apos = db.group_center(gid)?;
                let pos = Vector2::new(target.pos.x, target.pos.z);
                if let Some(reason) = Jtacs::artillery_range_reason(db, gid, pos) {
                    bail!("{reason}");
                }
                adjustment.target = pos;
                let pos = pos + adjustment.adjust;
                let task = Task::FireAtPoint {
                    point: LuaVec2(pos),
                    radius: None,
                    expend_qty: Some(n as i64),
                    weapon_type: None,
                    altitude: Some(0.),
                    altitude_type: Some(AltType::RADIO),
                };
                let task = aim_and_fire_route(apos, pos, group_facing(db, gid), task);
                let group = Group::get_by_name(lua, &name)
                    .with_context(|| format_compact!("getting group {}", name))?;
                adjustment.tracked = None;
                for unit in group.get_units()? {
                    let unit = unit?;
                    let id = unit.object_id()?;
                    adjustment.group.push(id);
                }
                let con = group.get_controller().context("getting controller")?;
                con.set_task(task)?;
            }
        }
        Ok(())
    }

    pub fn artillery_combo_mission(
        &mut self,
        db: &Db,
        lua: MizLua,
        adjustment: &mut ArtilleryAdjustment,
        gid: &GroupId,
        rounds_per_target: u8,
        num_targets: u8,
    ) -> Result<()> {
        match self.target.as_mut() {
            None => bail!("no target"),
            Some(target) => {
                let aim_target = Vector2::new(target.pos.x, target.pos.z);
                let name = db.group(gid)?.name.clone();
                let apos = db.group_center(gid)?;

                // Get total ammunition from all units in the artillery group
                let mut total_ammo = 0u8;
                for unit_id in db.group(gid)?.units.into_iter() {
                    let first = Unit::get_by_name(lua, &db.unit(unit_id)?.name)?
                        .get_ammo()?
                        .first();
                    let unit_ammo = match first {
                        Ok(ammo_info) => ammo_info.count()? as u8,
                        Err(_e) => 0, // Skip units with no ammo
                    };
                    total_ammo = total_ammo.saturating_add(unit_ammo);
                }
                
                if total_ammo == 0 {
                    bail!("Artillery Abort: {gid} is out of ammunition.");
                }
                
                let total_required = rounds_per_target * num_targets;
                if total_ammo < total_required {
                    bail!("Artillery Abort: {gid} has only {total_ammo} rounds remaining, cannot fire {total_required} rounds ({rounds_per_target} per target for {num_targets} targets).");
                }

                // Create fire tasks for each target
                let mut fire_task_vec: Vec<Task> = vec![];
                let mut allocated_ammo = total_ammo;
                
                for (i, (_, target)) in self.contacts.iter().enumerate() {
                    if i >= num_targets as usize || allocated_ammo < rounds_per_target {
                        break;
                    }
                    
                    let pos = Vector2::new(target.pos.x, target.pos.z);
                    adjustment.target = pos;
                    let pos = pos + adjustment.adjust;
                    
                    let task = Task::FireAtPoint {
                        point: LuaVec2(pos),
                        radius: None,
                        expend_qty: Some(rounds_per_target as i64),
                        weapon_type: None,
                        altitude: Some(0.),
                        altitude_type: Some(AltType::RADIO),
                    };
                    
                    fire_task_vec.push(task);
                    allocated_ammo -= rounds_per_target;
                }
                
                if fire_task_vec.is_empty() {
                    bail!("Artillery Abort: No valid targets found for combo mission.");
                }
                
                let task = aim_and_fire_route(
                    apos,
                    aim_target,
                    group_facing(db, gid),
                    Task::ComboTask(fire_task_vec),
                );

                let group = Group::get_by_name(lua, &name)
                    .with_context(|| format_compact!("getting group {}", name))?;
                adjustment.tracked = None;
                for unit in group.get_units()? {
                    let unit = unit?;
                    let id = unit.object_id()?;
                    adjustment.group.push(id);
                }
                let con = group.get_controller().context("getting controller")?;
                con.set_task(task)?;
            }
        }
        Ok(())
    }

    pub fn alcm_mission(
        &mut self,
        db: &Db,
        lua: MizLua,
        gid: &GroupId,
        mut n: Vec<u8>,
    ) -> Result<()> {
        let per_target = n.pop().unwrap();
        let magazine_expend = n.pop().unwrap();

        match self.target.as_mut() {
            None => bail!("no target"),
            Some(_target) => {
                let name = db.group(gid)?.name.clone();
                let apos = db.group_center(gid)?;
                let expend = match magazine_expend {
                    1 => WeaponExpend::Quarter,
                    2 => WeaponExpend::Half,
                    4 => WeaponExpend::All,
                    _ => bail!("invalid expend {0}", magazine_expend),
                };

                let per = match per_target {
                    1 => WeaponExpend::One,
                    2 => WeaponExpend::Two,
                    4 => WeaponExpend::Four,
                    _ => bail!("invalid per target {0}", per_target),
                };

                let mut allocated_ammo = {
                    let mut ammo = 0;
                    for i in db.group(gid)?.units.into_iter() {
                        let first = Unit::get_by_name(lua, &db.unit(i)?.name)?
                            .get_ammo()?
                            .first();
                        ammo = match first {
                            Ok(ammo) => ammo.count()? as u8,
                            Err(_e) => bail! {"ALCM Abort: {gid} is out of missiles."},
                        };
                        if ammo < per_target {
                            bail!(
                                "ALCM Abort: {gid} has only {ammo} missiles remaining, cannot launch {per_target}."
                            );
                        }
                    }

                    let ammo = match expend {
                        WeaponExpend::Quarter => ammo / 4,
                        WeaponExpend::Half => ammo / 2,
                        WeaponExpend::All => ammo,
                        _ => bail!("nice job"),
                    };
                    if ammo > 0 {
                        ammo
                    } else {
                        bail!("ALCM Abort: not enough missiles to complete a minimum launch.")
                    }
                };

                let mut bombing_task_vec: Vec<MissionPoint> = vec![];
                let mut fire_task_vec: Vec<Task> = vec![];

                info!("allocated ammo: {} {}", allocated_ammo, per_target);

                for (_, target) in &self.contacts {
                    if allocated_ammo >= per_target {
                        allocated_ammo -= per_target;
                    } else {
                        break;
                    }

                    let attack_params = AttackParams {
                        altitude: Some(9000.),
                        attack_qty: Some(1),
                        direction: None,
                        expend: Some(per.clone()),
                        group_attack: Some(false),
                        weapon_type: Some(2097152), // hard coded, change later?
                        attack_qty_limit: None,
                        altitude_enabled: Some(false),
                        direction_enabled: Some(false),
                        point: None,
                        x: Some(target.pos.x),
                        y: Some(target.pos.z),
                    };

                    fire_task_vec.push(Task::Bombing {
                        point: dcso3::LuaVec2(Vector2::new(target.pos.x, target.pos.z)),
                        params: attack_params,
                    });
                }

                fire_task_vec.push(Task::WrappedCommand(Command::SetUnlimitedFuel(true)));

                bombing_task_vec.push(MissionPoint {
                    action: Some(ActionTyp::Air(TurnMethod::FlyOverPoint)),
                    typ: PointType::TurningPoint,
                    airdrome_id: None,
                    helipad: None,
                    time_re_fu_ar: None,
                    link_unit: None,
                    pos: LuaVec2(apos),
                    alt: 9000.,
                    alt_typ: Some(AltType::BARO),
                    speed: 890.,
                    speed_locked: None,
                    eta: None,
                    eta_locked: None,
                    name: None,
                    task: Box::new(Task::ComboTask(fire_task_vec)),
                });

                // bombing_task_vec.push(MissionPoint {
                //     action: Some(ActionTyp::Air(TurnMethod::FlyOverPoint)),
                //     typ: PointType::TurningPoint,
                //     airdrome_id: None,
                //     helipad: None,
                //     time_re_fu_ar: None,
                //     link_unit: None,
                //     pos: LuaVec2(apos), // Same position as first point
                //     alt: 9000.,
                //     alt_typ: Some(AltType::BARO),
                //     speed: 890.,
                //     speed_locked: None,
                //     eta: None,
                //     eta_locked: None,
                //     name: None,
                //     task: Box::new(Task::Orbit {
                //         pattern: OrbitPattern::Circle,
                //         speed: Some(750.0),
                //         altitude: Some(9000.0),
                //         point2: Some(LuaVec2(apos)),
                //         point: Some(LuaVec2(apos)),
                //     }),
                // });

                let task = Task::Mission {
                    airborne: Some(true),
                    route: bombing_task_vec,
                };

                let group = Group::get_by_name(lua, &name)
                    .with_context(|| format_compact!("getting group {}", name))?;
                for unit in group.get_units()? {
                    let unit = unit?;
                    let _id = unit.object_id()?;
                }
                let con = group.get_controller().context("getting controller")?;
                //con.set_task(task.clone())?;
                //con.set_task(task)?;
                con.push_task(task)?;
            }
        }
        Ok(())
    }

    pub fn relay_target(&mut self, db: &Db, lua: MizLua, gid: &GroupId) -> Result<()> {
        match self.target.as_mut() {
            None => bail!("no target"),
            Some(target) => {
                let name = db.group(gid)?.name.clone();
                let shooter = Group::get_by_name(lua, &name)
                    .with_context(|| format_compact!("getting group {}", name))?;
                let target = match &target.id {
                    EnId::Unit(id) => Unit::get_by_name(lua, &db.unit(id)?.name)?,
                    EnId::Player(id) => match db.player(id) {
                        None => bail!("no player"),
                        Some(pl) => match &pl.current_slot {
                            None => bail!("player not slotted"),
                            Some((_, Some(inst))) => Unit::get_by_name(lua, &inst.unit_name)?,
                            Some((_, None)) => bail!("player not instanced"),
                        },
                    },
                };
                let pos = target.get_ground_position()?;
                let task = Task::AttackUnit {
                    unit: target.id()?,
                    params: AttackParams {
                        altitude: None,
                        attack_qty: None,
                        direction: None,
                        expend: None,
                        group_attack: Some(true),
                        weapon_type: None,
                        attack_qty_limit: None,
                        altitude_enabled: None,
                        direction_enabled: None,
                        point: None,
                        x: None,
                        y: None,
                    },
                };
                let task = Task::Mission {
                    airborne: Some(false),
                    route: vec![MissionPoint {
                        action: Some(ActionTyp::Ground(VehicleFormation::OffRoad)),
                        typ: PointType::TurningPoint,
                        airdrome_id: None,
                        helipad: None,
                        time_re_fu_ar: None,
                        link_unit: None,
                        pos,
                        alt: 0.,
                        alt_typ: Some(AltType::RADIO),
                        speed: 0.,
                        speed_locked: None,
                        eta: None,
                        eta_locked: None,
                        name: None,
                        task: Box::new(task),
                    }],
                };
                let con = shooter.get_controller().context("getting controller")?;
                con.set_task(task)?;
            }
        }
        Ok(())
    }

    fn update_target_position(&mut self, lua: MizLua, db: &mut Db) -> Result<()> {
        if let Some(target) = &self.target {
            let (pos, velocity) = match &target.id {
                EnId::Unit(uid) => {
                    let unit = db.unit(uid)?;
                    let v = db
                        .ephemeral
                        .get_object_id_by_uid(uid)
                        .and_then(|oid| Unit::get_instance(lua, oid).ok())
                        .and_then(|unit| unit.get_velocity().ok())
                        .unwrap_or(LuaVec3(Vector3::default()));
                    (unit.position.p.0, v.0)
                }
                EnId::Player(ucid) => {
                    let player = db
                        .player(ucid)
                        .ok_or_else(|| anyhow!("no such player {ucid}"))?;
                    let inst = player
                        .current_slot
                        .as_ref()
                        .and_then(|(_, i)| i.as_ref())
                        .ok_or_else(|| anyhow!("player not instanced {ucid}"))?;
                    (inst.position.p.0, inst.velocity)
                }
            };
            let contact = self.contacts.get_mut(&target.id).unwrap();
            if (contact.pos - pos).magnitude_squared() > 2. {
                contact.pos = pos;
                let typ_clone = contact.typ.clone();
                let spot =
                    Spot::get_instance(lua, &target.spot).context("getting the spot instance")?;
                spot.set_point(LuaVec3(contact.pos + velocity))
                    .context("setting the spot position")?;
                let _ = contact;
                self.mark_target(lua).context("marking moved target")?;
                if let JtId::Group(gid) = self.gid {
                    let new_target_pos2 = Vector2::new(pos.x, pos.z);
                    let text = format_compact!(
                        "JTAC {}\nlasing {}\nCode: {}",
                        self.gid,
                        typ_clone,
                        self.code
                    );
                    let (map_layer, msgs) = db.ephemeral.map_layer_and_msgs();
                    if let Some(marks) = map_layer.jtac_marks.get_mut(&gid) {
                        marks.on_target_move(new_target_pos2, text, msgs);
                    }
                }
            }
        }
        Ok(())
    }

    pub fn toggle_auto_shift(&mut self, db: &mut Db, lua: MizLua) -> Result<()> {
        match self.autoshift {
            None => match self.target.as_ref() {
                None => self.autoshift = Some(0),
                Some(t) => match self.contacts.get_index_of(&t.id) {
                    None => self.autoshift = Some(0),
                    Some(i) => self.autoshift = Some(i),
                },
            },
            Some(_) => {
                self.autoshift = None;
                self.set_target(db, lua, 0)?;
            }
        }
        self.persist_state(db);
        Ok(())
    }

    pub fn toggle_ir_pointer(&mut self, db: &mut Db, lua: MizLua) -> Result<()> {
        self.ir_pointer = !self.ir_pointer;
        self.persist_state(db);
        self.reset_target(db, lua).context("resetting target")?;
        Ok(())
    }

    pub fn clear_filter(&mut self, db: &mut Db, lua: MizLua) -> Result<bool> {
        self.filter = BitFlags::empty();
        self.persist_state(db);
        self.sort_contacts(db, lua)
    }

    pub fn add_filter(&mut self, db: &mut Db, lua: MizLua, tag: BitFlags<UnitTag>) -> Result<bool> {
        self.filter |= tag;
        self.persist_state(db);
        self.sort_contacts(db, lua)
    }

    pub fn filter(&self) -> BitFlags<UnitTag> {
        self.filter
    }

    pub fn gid(&self) -> JtId {
        self.gid
    }

    pub fn side(&self) -> Side {
        self.side
    }

    pub fn location(&self) -> JtacLocation {
        self.location
    }

    pub fn target(&self) -> &Option<JtacTarget> {
        &self.target
    }

    pub fn autoshift(&self) -> bool {
        self.autoshift.is_none()
    }

    pub fn ir_pointer(&self) -> bool {
        self.ir_pointer
    }

    pub fn code(&self) -> u16 {
        self.code
    }

    pub fn nearby_artillery(&self) -> &[GroupId] {
        &self.nearby_artillery
    }

    pub fn nearby_alcm(&self) -> &[(GroupId, i32)] {
        &self.nearby_alcm
    }
}

#[derive(Debug, Clone, Default)]
struct Detected {
    was_detected: bool,
    detected: bool,
}

#[derive(Debug, Clone, Default)]
pub struct Jtacs {
    jtacs: FxHashMap<Side, FxHashMap<JtId, Jtac>>,
    detected: FxHashMap<Side, FxHashMap<EnId, Detected>>,
    artillery_adjustment: FxHashMap<GroupId, ArtilleryAdjustment>,
    code_by_location: LocByCode,
    menu_dirty: FxHashMap<Side, FxHashSet<ObjectiveId>>,
}

impl Jtacs {
    pub fn get(&self, gid: &JtId) -> Result<&Jtac> {
        self.jtacs
            .iter()
            .find_map(|(_, jtx)| jtx.get(gid))
            .ok_or_else(|| anyhow!("no such jtac {gid}"))
    }

    pub fn get_mut(&mut self, gid: &JtId) -> Result<&mut Jtac> {
        self.jtacs
            .iter_mut()
            .find_map(|(_, jtx)| jtx.get_mut(gid))
            .ok_or_else(|| anyhow!("no such jtac"))
    }

    pub fn jtacs(&self) -> impl Iterator<Item = &Jtac> {
        self.jtacs.values().flat_map(|jtx| jtx.values())
    }

    pub fn artillery_range_reason(db: &Db, gid: &GroupId, target_pos: Vector2) -> Option<CompactString> {
        let cfg = &db.ephemeral.cfg;
        let apos = db.group_center(gid).ok()?;
        let dist = na::distance(&apos.into(), &target_pos.into());
        let group = db.group(gid).ok();
        let name = group.map(|g| g.name.to_string()).unwrap_or_default();
        // Prefer the per-unit-type range from cfg.artillery.units (keyed by DCS
        // type, e.g. "Scud_B") -- a ballistic TEL has a huge minimum range the
        // flat artillery_min_range doesn't capture. Fall back to the flat pair.
        let per_unit = group
            .and_then(|g| g.units.into_iter().next())
            .and_then(|uid| db.unit(uid).ok())
            .and_then(|u| {
                cfg.artillery
                    .as_ref()
                    .and_then(|a| a.units.get(u.typ.as_str()))
            });
        let (min, max) = match per_unit {
            Some(r) => (r.min_range_m, r.max_range_m),
            None => (cfg.artillery_min_range as f64, cfg.artillery_mission_range as f64),
        };
        if dist < min {
            Some(format_compact!(
                "{gid} ({name}) too close to target: {dist:.0}m (min {min:.0}m)"
            ))
        } else if dist > max {
            Some(format_compact!(
                "{gid} ({name}) too far from target: {dist:.0}m (max {max:.0}m)"
            ))
        } else {
            None
        }
    }

    pub fn artillery_mission(
        &mut self,
        db: &Db,
        lua: MizLua,
        jtid: &JtId,
        shooter: &GroupId,
        n: u8,
    ) -> Result<()> {
        let jtac = self
            .jtacs
            .iter_mut()
            .find_map(|(_, jtx)| jtx.get_mut(&jtid))
            .ok_or_else(|| anyhow!("no such jtac"))?;
        let adjustment = self
            .artillery_adjustment
            .entry(*shooter)
            .or_insert_with(|| ArtilleryAdjustment {
                adjust: Vector2::zeros(),
                target: Vector2::zeros(),
                group: vec![],
                tracked: None,
            });
        jtac.artillery_mission(db, lua, adjustment, &shooter, n)
    }

    pub fn artillery_fire_all(
        &mut self,
        db: &Db,
        lua: MizLua,
        jtid: &JtId,
        shooter: &GroupId,
    ) -> Result<()> {
        let jtac = self
            .jtacs
            .iter_mut()
            .find_map(|(_, jtx)| jtx.get_mut(&jtid))
            .ok_or_else(|| anyhow!("no such jtac"))?;
        let adjustment = self
            .artillery_adjustment
            .entry(*shooter)
            .or_insert_with(|| ArtilleryAdjustment {
                adjust: Vector2::zeros(),
                target: Vector2::zeros(),
                group: vec![],
                tracked: None,
            });

        // Get total ammunition from all units in the artillery group
        let total_ammo = {
            let mut total = 0u8;
            for unit_id in db.group(shooter)?.units.into_iter() {
                let first = Unit::get_by_name(lua, &db.unit(unit_id)?.name)?
                    .get_ammo()?
                    .first();
                let unit_ammo = match first {
                    Ok(ammo_info) => ammo_info.count()? as u8,
                    Err(_e) => 0, // Skip units with no ammo
                };
                total = total.saturating_add(unit_ammo);
            }
            if total == 0 {
                bail!("Artillery Abort: {shooter} is out of ammunition.");
            }
            total
        };

        jtac.artillery_mission(db, lua, adjustment, &shooter, total_ammo)
    }

    /// Fire every gun in `nearby_artillery` at the current JTAC target simultaneously.
    /// Each gun fires `n` rounds using its individual adjustment. Returns the count
    /// of guns that accepted the order.
    pub fn fire_all_artillery_together(
        &mut self,
        db: &Db,
        lua: MizLua,
        jtid: &JtId,
        n: u8,
    ) -> Result<(usize, SmallVec<[CompactString; 4]>)> {
        let jtac = self
            .jtacs
            .iter_mut()
            .find_map(|(_, jtx)| jtx.get_mut(jtid))
            .ok_or_else(|| anyhow!("no such jtac {jtid}"))?;
        let target_pos = match &jtac.target {
            None => bail!("no JTAC target — designate a target first"),
            Some(t) => Vector2::new(t.pos.x, t.pos.z),
        };
        let arty_gids: SmallVec<[GroupId; 8]> = jtac.nearby_artillery.clone();
        if arty_gids.is_empty() {
            bail!("no nearby artillery groups registered with this JTAC");
        }

        let mut fired = 0usize;
        let mut skipped: SmallVec<[CompactString; 4]> = smallvec![];
        for gid in &arty_gids {
            if let Some(reason) = Jtacs::artillery_range_reason(db, gid, target_pos) {
                skipped.push(reason);
                continue;
            }
            // Per-gun adjustment (zero if none set yet).
            let adjustment = self
                .artillery_adjustment
                .entry(*gid)
                .or_insert_with(|| ArtilleryAdjustment {
                    adjust: Vector2::zeros(),
                    target: Vector2::zeros(),
                    group: vec![],
                    tracked: None,
                });
            let adjusted_pos = target_pos + adjustment.adjust;
            adjustment.target = target_pos;

            let group = match db.group(gid) {
                Ok(g) => g,
                Err(_) => continue,
            };
            let group_name = group.name.clone();
            let apos = match db.group_center(gid) {
                Ok(p) => p,
                Err(_) => continue,
            };
            // n == 0 is the "fire all ammo" sentinel: sum each gun's actual
            // rounds and expend that. A fixed n just uses that count.
            let expend = if n == 0 {
                let mut total = 0i64;
                for uid in group.units.into_iter() {
                    if let Ok(u) = db.unit(uid) {
                        if let Ok(unit) = Unit::get_by_name(lua, &u.name) {
                            if let Ok(ammo) = unit.get_ammo() {
                                if let Ok(info) = ammo.first() {
                                    total += info.count().unwrap_or(0) as i64;
                                }
                            }
                        }
                    }
                }
                if total == 0 {
                    skipped.push(format_compact!("{gid} ({group_name}) is out of ammunition"));
                    continue;
                }
                total
            } else {
                n as i64
            };
            let fire_task = Task::FireAtPoint {
                point: LuaVec2(adjusted_pos),
                radius: None,
                expend_qty: Some(expend),
                weapon_type: None,
                altitude: Some(0.),
                altitude_type: Some(AltType::RADIO),
            };
            let mission =
                aim_and_fire_route(apos, adjusted_pos, group_facing(db, gid), fire_task);
            if let Ok(group) = Group::get_by_name(lua, &group_name) {
                if let Ok(con) = group.get_controller() {
                    if con.set_task(mission).is_ok() {
                        fired += 1;
                    }
                }
            }
        }

        if fired == 0 {
            if !skipped.is_empty() {
                let joined = skipped.iter().map(|s| s.as_str()).collect::<Vec<_>>().join("; ");
                bail!("no guns in range — {joined}");
            }
            bail!("no artillery groups could be reached — are they spawned?");
        }
        Ok((fired, skipped))
    }

    pub fn get_artillery_ammo(
        &self,
        db: &Db,
        lua: MizLua,
        shooter: &GroupId,
    ) -> Result<dcso3::String> {
        let group = db.group(shooter)?;
        let mut total_ammo = 0u8;
        
        for unit_id in group.units.into_iter() {
            let first = Unit::get_by_name(lua, &db.unit(unit_id)?.name)?
                .get_ammo()?
                .first();
            let unit_ammo = match first {
                Ok(ammo_info) => ammo_info.count()? as u8,
                Err(_e) => 0, // Skip units with no ammo
            };
            total_ammo = total_ammo.saturating_add(unit_ammo);
        }
        
        let result = format!("Artillery Group {} has {} rounds total", shooter, total_ammo);
        Ok(result.into())
    }

    pub fn artillery_combo_mission(
        &mut self,
        db: &Db,
        lua: MizLua,
        jtid: &JtId,
        shooter: &GroupId,
        rounds_per_target: u8,
        num_targets: u8,
    ) -> Result<()> {
        let jtac = self
            .jtacs
            .iter_mut()
            .find_map(|(_, jtx)| jtx.get_mut(&jtid))
            .ok_or_else(|| anyhow!("no such jtac"))?;
        let adjustment = self
            .artillery_adjustment
            .entry(*shooter)
            .or_insert_with(|| ArtilleryAdjustment {
                adjust: Vector2::zeros(),
                target: Vector2::zeros(),
                group: vec![],
                tracked: None,
            });

        jtac.artillery_combo_mission(db, lua, adjustment, &shooter, rounds_per_target, num_targets)
    }

    pub fn alcm_mission(
        &mut self,
        db: &Db,
        lua: MizLua,
        jtid: &JtId,
        shooter: &GroupId,
        n: Vec<u8>,
    ) -> Result<()> {
        let jtac = self
            .jtacs
            .iter_mut()
            .find_map(|(_, jtx)| jtx.get_mut(&jtid))
            .ok_or_else(|| anyhow!("no such jtac"))?;

        jtac.alcm_mission(db, lua, &shooter, n)
    }

    /// set part of the laser code, defined by the scale of the passed in number. For example,
    /// passing 600 sets the hundreds part of the code to 6. passing 8 sets the ones part of the code to 8.
    /// other parts of the existing code are left alone.
    pub fn set_code_part(&mut self, db: &mut Db, lua: MizLua, gid: &JtId, code_part: u16) -> Result<()> {
        let jt = self.get_mut(gid)?;
        let prev_code = jt.code;
        let oid = jt.location.oid;
        let side = jt.side;
        jt.set_code(lua, code_part)?;
        let code = jt.code;
        jt.persist_state(db);
        Self::remove_code_by_location(&mut self.code_by_location, side, oid, prev_code, *gid);
        Self::add_code_by_location(&mut self.code_by_location, side, oid, code, *gid);
        Ok(())
    }

    pub fn jtac_targets<'a>(&'a self) -> impl Iterator<Item = EnId> + 'a {
        self.jtacs.values().flat_map(|j| {
            j.values()
                .filter_map(|jt| jt.target.as_ref().map(|target| target.id))
        })
    }

    pub fn contacts_near_point<'a>(
        &'a self,
        side: Side,
        point: Vector2,
        dist: f64,
    ) -> ContactsIter<'a> {
        let dist = dist.powi(2);
        let contacts = self
            .jtacs()
            .filter_map(|jt| {
                if jt.side == side
                    && na::distance_squared(&jt.location.pos.into(), &point.into()) <= dist
                {
                    Some(jt.contacts.iter())
                } else {
                    None
                }
            })
            .collect();
        ContactsIter { i: 0, contacts }
    }

    fn add_code_by_location(t: &mut LocByCode, side: Side, oid: ObjectiveId, code: u16, gid: JtId) {
        t.entry(side)
            .or_default()
            .entry(oid)
            .or_default()
            .entry(code)
            .or_default()
            .insert(gid);
    }

    fn remove_code_by_location(
        t: &mut LocByCode,
        side: Side,
        oid: ObjectiveId,
        code: u16,
        gid: JtId,
    ) {
        match t
            .entry(side)
            .or_default()
            .entry(oid)
            .or_default()
            .entry(code)
        {
            Entry::Vacant(_) => (),
            Entry::Occupied(mut e) => {
                let set = e.get_mut();
                set.remove(&gid);
                if set.is_empty() {
                    e.remove();
                }
            }
        }
    }

    pub fn location_by_code(&self) -> &LocByCode {
        &self.code_by_location
    }

    pub fn unit_dead(&mut self, lua: MizLua, db: &mut Db, id: &DcsOid<ClassUnit>) -> Result<()> {
        let ctid = db
            .ephemeral
            .player_in_unit(id)
            .map(|ucid| EnId::Player(*ucid))
            .or_else(|| {
                db.ephemeral
                    .get_uid_by_object_id(id)
                    .map(|uid| EnId::Unit(*uid))
            });
        let jtid = {
            let sl = db.ephemeral.get_slot_by_object_id(id).map(|sl| *sl);
            match &ctid {
                Some(EnId::Unit(uid)) => db.unit(uid).ok().map(|spu| JtId::Group(spu.group)),
                Some(_) | None => sl.map(|sl| JtId::Slot(sl)),
            }
        };
        if let Some(jtid) = jtid {
            for (side, jtx) in self.jtacs.iter_mut() {
                jtx.retain(|gid, jt| {
                    if &jtid == gid {
                        macro_rules! dead {
                            () => {{
                                if let Err(e) = jt.remove_target(db, lua) {
                                    warn!("0 could not remove jtac target {:?}", e)
                                }
                                ui_jtac_dead(db, *side, jtid, jt.name.as_ref());
                                Self::remove_code_by_location(
                                    &mut self.code_by_location,
                                    jt.side,
                                    jt.location.oid,
                                    jt.code,
                                    jt.gid,
                                );
                                self.menu_dirty
                                    .entry(jt.side)
                                    .or_default()
                                    .insert(jt.location.oid);
                                return false;
                            }};
                        }
                        match jtid {
                            JtId::Slot(_) => dead!(),
                            JtId::Group(gid) => {
                                if db.group_health(&gid).unwrap_or((0, 0)).0 <= 1 {
                                    dead!()
                                }
                            }
                        }
                        if let Some(target) = &jt.target {
                            if &target.source == id {
                                if let Err(e) = jt.reset_target(db, lua) {
                                    warn!("could not reset jtac target {:?}", e)
                                }
                            }
                        }
                    }
                    let dead = match &jt.target {
                        None => false,
                        Some(target) => match ctid {
                            None => false,
                            Some(id) => target.id == id,
                        },
                    };
                    if dead {
                        if let Err(e) = jt.remove_target(db, lua) {
                            warn!("1 could not remove jtac target {:?}", e)
                        }
                        db.ephemeral.msgs().panel_to_side(
                            10,
                            false,
                            jt.side,
                            format_compact!("{} target destroyed", jt.gid),
                        );
                    }
                    true
                })
            }
        }
        Ok(())
    }

    pub fn update_target_positions(
        &mut self,
        lua: MizLua,
        now: DateTime<Utc>,
        db: &mut Db,
    ) -> Result<Vec<DcsOid<ClassUnit>>> {
        let mut units: SmallVec<[UnitId; 16]> = smallvec![];
        let mut players: SmallVec<[Ucid; 16]> = smallvec![];
        for id in self.jtac_targets() {
            match id {
                EnId::Unit(uid) => units.push(uid),
                EnId::Player(ucid) => players.push(ucid),
            }
        }
        let mut dead = db
            .update_unit_positions(lua, now, &units)
            .context("updating the position of jtac targets")?;
        dead.extend(
            db.update_player_positions(lua, now, &players)
                .context("updating the position of player jtac targets")?
                .into_iter(),
        );
        for jtx in self.jtacs.values_mut() {
            for jt in jtx.values_mut() {
                if let Err(e) = jt.update_target_position(lua, db) {
                    warn!("failed to update target position for {} {e:?}", jt.gid)
                }
            }
        }
        Ok(dead)
    }

    fn prepare_detected(&mut self) {
        for detected in self.detected.values_mut() {
            for dt in detected.values_mut() {
                dt.detected = false;
            }
        }
    }

    fn update_jtac(
        &mut self,
        lua: MizLua,
        land: &Land,
        landcache: &mut LandCache,
        db: &mut Db,
        saw_jtacs: &mut SmallVec<[JtId; 32]>,
        saw_units: &mut FxHashSet<EnId>,
        lost_targets: &mut SmallVec<[(Side, JtId, Option<EnId>); 64]>,
        jt: JtDesc,
    ) -> Result<()> {
        let JtDesc {
            mut pos,
            id,
            side,
            spec,
            air,
            state,
        } = jt;
        if !saw_jtacs.contains(&id) {
            saw_jtacs.push(id)
        }
        let detected = self.detected.entry(side.opposite()).or_default();
        let range = (spec.range as f64).powi(2);
        let jtac = self
            .jtacs
            .entry(side)
            .or_default()
            .entry(id)
            .or_insert_with(|| {
                let mut jt = Jtac::new(
                    db,
                    id,
                    spec.name.as_deref().map(CompactString::new),
                    side,
                    db.ephemeral.cfg.jtac_priority.clone(),
                    pos,
                    air,
                    spec.default_laser_code,
                    spec.range as f64,
                );
                if let Some(st) = state.clone() {
                    jt.apply_state(st);
                    let obj_name = db.objective(&jt.location.oid).map(|o| o.name()).unwrap_or("Unknown location");
                    let bearing = (jt.location.bearing * 180.0 / std::f64::consts::PI).round();
                    let name_str = jt.name.as_deref().unwrap_or("");
                    let display = if name_str.is_empty() {
                        format_compact!("{}", jt.gid)
                    } else {
                        format_compact!("{} ({})", name_str, jt.gid)
                    };
                    let msg = format_compact!("JTAC {} is online at {} degrees {:.0} meters from {}", display, bearing, jt.location.distance, obj_name);
                    db.ephemeral.msgs().panel_to_side(10, false, jt.side, msg);
                }
                self.menu_dirty
                    .entry(side)
                    .or_default()
                    .insert(jt.location.oid);
                Self::add_code_by_location(
                    &mut self.code_by_location,
                    jt.side,
                    jt.location.oid,
                    jt.code,
                    jt.gid,
                );
                jt
            });
        let prev_loc = jtac.location;
        jtac.location = JtacLocation::new(db, pos);
        let jtac_moved = (prev_loc.pos - jtac.location.pos).magnitude_squared() > 1.0;
        if jtac_moved {
            if let JtId::Group(gid) = jtac.gid {
                let new_pos2 = jtac.location.pos;
                let (map_layer, msgs) = db.ephemeral.map_layer_and_msgs();
                if let Some(marks) = map_layer.jtac_marks.get_mut(&gid) {
                    marks.on_jtac_move(new_pos2, msgs);
                }
            }
        }
        if prev_loc.oid != jtac.location.oid {
            Self::remove_code_by_location(
                &mut self.code_by_location,
                jtac.side,
                prev_loc.oid,
                jtac.code,
                jtac.gid,
            );
            Self::add_code_by_location(
                &mut self.code_by_location,
                jtac.side,
                jtac.location.oid,
                jtac.code,
                jtac.gid,
            );
            let menu = self.menu_dirty.entry(jtac.side).or_default();
            menu.insert(prev_loc.oid);
            menu.insert(jtac.location.oid);
        }
        if air {
            pos.y -= 5.
        } else {
            pos.y += 10.
        };

        // Collect unit/player snapshots before mutably borrowing db for remove_contact.
        let units_snap: SmallVec<[SpawnedUnit; 32]> = db
            .instanced_units()
            .map(|(u, _)| u.clone())
            .collect();
        let players_snap: SmallVec<[(Ucid, Side, InstancedPlayer); 16]> = db
            .instanced_players()
            .map(|(ucid, pl, inst)| (*ucid, pl.side, inst.clone()))
            .collect();

        let mut to_remove: SmallVec<[EnId; 32]> = smallvec![];
        let now = Utc::now();

        for unit in &units_snap {
            let id = EnId::Unit(unit.id);
            if unit.side == jtac.side {
                continue;
            }
            saw_units.insert(id);
            let detected = detected.entry(id).or_default();
            // Filter is "target any of these types" -- keep a unit if it carries
            // ANY selected tag (intersects), not only if it carries them all
            // (contains), which made multi-tag filters match nothing.
            if !jtac.filter.is_empty() && !unit.tags.intersects(jtac.filter) {
                to_remove.push(id);
                continue;
            }
            if unit.airborne_velocity.is_some() && !unit.tags.contains(UnitTag::Helicopter) {
                to_remove.push(id);
                continue;
            }
            if let Some(ct) = jtac.contacts.get(&id) {
                if !jtac_moved && unit.moved == ct.last_move {
                    detected.detected = true;
                    // Still under observation -- keep its intel confidence
                    // pinned at 1.0 (see IntelSource::Jtac).
                    db.note_jtac_contact(jtac.side, unit, now);
                    continue;
                }
            };
            let dist = na::distance_squared(&pos.into(), &unit.position.p.0.into());
            if dist <= range
                && (spec.nolos
                    || landcache.is_visible(&land, dist.sqrt(), pos, unit.position.p.0)?)
            {
                detected.detected = true;
                jtac.add_unit_contact(unit);
                db.note_jtac_contact(jtac.side, unit, now);
            } else {
                to_remove.push(id);
            }
        }

        for (ucid, player_side, inst) in &players_snap {
            if *player_side == jtac.side {
                continue;
            }
            let id = EnId::Player(*ucid);
            saw_units.insert(id);
            let detected = detected.entry(id).or_default();
            let tags = db.ephemeral.cfg.unit_classification[&inst.typ];
            if !jtac.filter.is_empty() && !tags.intersects(jtac.filter) {
                to_remove.push(id);
                continue;
            }
            if inst.in_air && !tags.contains(UnitTag::Helicopter) {
                continue;
            }
            let dist = na::distance_squared(&pos.into(), &inst.position.p.0.into());
            if dist <= range
                && (spec.nolos
                    || landcache.is_visible(&land, dist.sqrt(), pos, inst.position.p.0)?)
            {
                detected.detected = true;
                jtac.add_player_contact(*ucid, inst)
            } else {
                to_remove.push(id);
            }
        }

        for id in to_remove {
            match jtac.remove_contact(lua, db, &id) {
                Err(e) => warn!("could not remove jtac contact {:?}", e),
                Ok(false) => (),
                Ok(true) => lost_targets.push((jtac.side, jtac.gid, None)),
            }
        }
        Ok(())
    }

    pub fn update_contacts(
        &mut self,
        lua: MizLua,
        landcache: &mut LandCache,
        db: &mut Db,
    ) -> Result<FxHashMap<Side, FxHashSet<ObjectiveId>>> {
        let land = Land::singleton(lua)?;
        self.prepare_detected();
        let mut saw_jtacs: SmallVec<[JtId; 32]> = smallvec![];
        let mut saw_units: FxHashSet<EnId> = FxHashSet::default();
        let mut lost_targets: SmallVec<[(Side, JtId, Option<EnId>); 64]> = smallvec![];
        let jtac_descs: Vec<JtDesc> = db.jtacs().collect();
        for jt in jtac_descs {
            self.update_jtac(
                lua,
                &land,
                landcache,
                db,
                &mut saw_jtacs,
                &mut saw_units,
                &mut lost_targets,
                jt,
            )?
        }
        for (side, jtx) in self.jtacs.iter_mut() {
            jtx.retain(|gid, jt| {
                saw_jtacs.contains(gid) || {
                    if let Err(e) = jt.remove_target(db, lua) {
                        warn!("2 could not remove jtac target {:?}", e)
                    }
                    ui_jtac_dead(db, *side, *gid, jt.name.as_ref());
                    Self::remove_code_by_location(
                        &mut self.code_by_location,
                        jt.side,
                        jt.location.oid,
                        jt.code,
                        jt.gid,
                    );
                    self.menu_dirty
                        .entry(*side)
                        .or_default()
                        .insert(jt.location.oid);
                    false
                }
            })
        }
        for (side, jtx) in self.jtacs.iter_mut() {
            for jtac in jtx.values_mut() {
                for uid in jtac.contacts.keys() {
                    if !saw_units.contains(&uid) {
                        lost_targets.push((*side, jtac.gid, Some(*uid)));
                    }
                }
            }
        }
        for detected in self.detected.values_mut() {
            detected.retain(|id, detected| {
                if !saw_units.contains(id) {
                    false
                } else {
                    if detected.was_detected != detected.detected {
                        detected.was_detected = detected.detected;
                        db.ephemeral.stat(Stat::Detected {
                            id: *id,
                            detected: detected.detected,
                            source: DetectionSource::Jtac,
                        });
                    }
                    true
                }
            })
        }
        for (side, gid, uid) in lost_targets {
            match uid {
                Some(uid) => match self.get_mut(&gid)?.remove_contact(lua, db, &uid) {
                    Ok(_) => (),
                    Err(e) => warn!("3 could not remove jtac target {uid} {:?}", e),
                },
                None => {
                    db.ephemeral.msgs().panel_to_side(
                        10,
                        false,
                        side,
                        format_compact!("JTAC {gid} target lost"),
                    );
                }
            }
        }
        let mut new_contacts: SmallVec<[&Jtac; 32]> = smallvec![];
        for j in self.jtacs.values_mut() {
            for (_, jtac) in j.iter_mut() {
                // Keep the fire-support lists fresh around the JTAC's own
                // position, not just around an acquired target -- otherwise the
                // Artillery / ALCM submenus vanish whenever the JTAC isn't
                // currently lasing something.
                let loc = jtac.location().pos;
                let prev_arty = jtac.nearby_artillery.clone();
                let prev_alcm = jtac.nearby_alcm.clone();
                jtac.nearby_artillery = db.artillery_near_point(jtac.side, loc);
                jtac.nearby_alcm = db.alcm_near_point(jtac.side, lua, loc);
                jtac.menu_dirty |= prev_arty != jtac.nearby_artillery;
                jtac.menu_dirty |= prev_alcm != jtac.nearby_alcm;
                match jtac.sort_contacts(db, lua) {
                    Ok(false) => (),
                    Ok(true) => new_contacts.push(jtac),
                    Err(e) => warn!("could not sort contacts for jtac {}, {:?}", jtac.gid, e),
                }
            }
        }
        let mut msgs: SmallVec<[(Side, CompactString); 32]> = smallvec![];
        for jtac in new_contacts {
            let msg = jtac
                .status(db, &self.code_by_location)
                .with_context(|| format_compact!("generating jtac status for {}", jtac.gid))?;
            msgs.push((jtac.side, msg))
        }
        for (side, msg) in msgs {
            db.ephemeral.msgs().panel_to_side(10, false, side, msg);
        }
        for (side, jtx) in self.jtacs.iter_mut() {
            for jt in jtx.values_mut() {
                if jt.menu_dirty {
                    self.menu_dirty
                        .entry(*side)
                        .or_default()
                        .insert(jt.location.oid);
                    jt.menu_dirty = false;
                }
            }
        }
        Ok(std::mem::take(&mut self.menu_dirty))
    }
}
