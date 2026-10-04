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
    msgq::MsgQ,
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

/// Whether `code` is a laser code DCS weapons can actually be set to: first
/// digit 1, second 1-7, third and fourth 1-8 -- i.e. 1111 through 1788 with
/// no 0s or 9s. A JTAC lasing on anything else can never be matched by a
/// bomb, so reject it up front instead of letting a player dial it in.
/// Also used by the `-jtac <id> code` chat command.
pub fn validate_laser_code(code: u16) -> Result<()> {
    let d = [code / 1000, code / 100 % 10, code / 10 % 10, code % 10];
    if code > 9999
        || d[0] != 1
        || !(1..=7).contains(&d[1])
        || !(1..=8).contains(&d[2])
        || !(1..=8).contains(&d[3])
    {
        bail!(
            "invalid laser code {code}: codes run 1111-1788 -- first digit 1, second 1-7, last two 1-8"
        )
    }
    Ok(())
}

/// Apply a laser-code entry to `current`. A whole four digit code (`1513`)
/// replaces it; a single digit at its scale (`500`, `10`, `3`, or `1000`)
/// replaces just that digit -- that is what the F10 Code submenu sends. The
/// result must be a valid code either way.
fn apply_code_part(current: u16, code_part: u16) -> Result<u16> {
    let code = match code_part {
        p if p > 1000 && p % 1000 != 0 => p,
        p if p >= 1000 && p % 1000 == 0 => p + current % 1000,
        p if p >= 100 && p < 1000 && p % 100 == 0 => current / 1000 * 1000 + p + current % 100,
        p if p >= 10 && p < 100 && p % 10 == 0 => current / 100 * 100 + p + current % 10,
        p if p < 10 => current / 10 * 10 + p,
        p => bail!(
            "invalid laser code entry {p}: give a whole code like 1688, or one digit at its place (600, 80, 8)"
        ),
    };
    validate_laser_code(code)?;
    Ok(code)
}

/// A code for a new JTAC that no other JTAC on its side is using. Every JTAC
/// used to come up on the configured default (1688 on the live servers), so
/// two drones over one fight lased on the same code and a bomb took whichever
/// spot it saw first. The default is still preferred -- the first JTAC keeps
/// it -- and the rest walk the valid codes upward from it.
fn pick_laser_code(preferred: u16, in_use: &FxHashSet<u16>) -> u16 {
    const FALLBACK: u16 = 1688;
    let start = if validate_laser_code(preferred).is_ok() { preferred } else { FALLBACK };
    let valid = (1111u16..=1788).filter(|c| validate_laser_code(*c).is_ok());
    valid
        .clone()
        .filter(|c| *c >= start)
        .chain(valid.filter(|c| *c < start))
        .find(|c| !in_use.contains(c))
        // 448 valid codes; a side with more JTACs than that shares one
        .unwrap_or(start)
}

/// A side-effect-free note for the players following one JTAC. Queued by
/// `Jtacs` (which has no idea who is listening) and delivered by
/// `menu::jtac::flush_jtac_notices`, which knows the requesters and the
/// players who pinned or expanded the JTAC. These used to be panels to the
/// whole coalition -- every target acquired, lost and destroyed by every
/// JTAC, on every screen on the side.
#[derive(Debug, Clone)]
pub struct JtacNotice {
    pub jtid: JtId,
    pub side: Side,
    pub oid: ObjectiveId,
    pub text: CompactString,
}

/// How far (m) a lased target has to drift before a slot JTAC's F10 pin is
/// re-dropped. Pins can't be moved, so following costs a delete plus a
/// create; this matches the map layer's own symbol threshold
/// (`JTAC_SYMBOL_FOLLOW_M`) so the two behave the same.
const JTAC_PIN_FOLLOW_M: f64 = 600.;

/// Convert a true bearing (degrees) to magnetic for a variation (degrees,
/// east positive) and round it to a whole compass heading 1-360.
fn mag_deg(true_rad: f64, magvar_deg: f64) -> u32 {
    let d = crate::atis::true_to_magnetic(radians_to_degrees(true_rad), magvar_deg).round() as u32;
    if d == 0 { 360 } else { d.min(360) }
}

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
    /// Slot JTACs only, see `Jtac::mark_target`.
    mark: Option<MarkId>,
    /// Where `mark` was dropped, for the follow threshold.
    mark_pos: Vector2,
}

impl JtacTarget {
    fn destroy(self, lua: MizLua, msgs: &mut MsgQ) -> Result<()> {
        // The pin first: it goes through the queue and must not be left
        // behind just because the spot below is already gone.
        if let Some(id) = self.mark {
            msgs.delete_mark(id)
        }
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
    fn destroy(self, lua: MizLua, msgs: &mut MsgQ) -> Result<()> {
        // The pin first, as in `JtacTarget::destroy`: it used to come after
        // the spot, so a failure destroying one DCS had already cleaned up
        // (the JTAC died) stranded the pin for good.
        if let Some(id) = self.mark {
            msgs.delete_mark(id)
        }
        Spot::get_instance(lua, &self.spot)
            .context("getting laser spot")?
            .destroy()
            .context("destroying laser spot")?;
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
    /// `None` only when the campaign has no objectives at all.
    fn new(db: &Db, pos: Vector3) -> Option<Self> {
        let pos = Vector2::new(pos.x, pos.z);
        let (distance, bearing, obj) =
            Db::objective_near_point(&db.persisted.objectives, pos, |_| true)?;
        Some(Self {
            pos,
            oid: obj.id,
            bearing,
            distance,
        })
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

/// Furthest a JTAC will lase, whatever its spotting range. A Reaper spots out
/// to 90 km, and with the priority list ranking SAMs first it used to put
/// the spot on an SA-10 fifty miles away while tanks sat under it. Contacts
/// past this are still seen and reported, just never lased. 10 nm.
const JTAC_MAX_LASE_M: f64 = 18_520.;

/// Radius around a player's focus mark that a focused JTAC lases inside
/// (~3 miles, the request was "within 2-3 miles of the marker").
pub const JTAC_FOCUS_RADIUS_M: f64 = 5_000.;

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
    /// Player-set point the JTAC should work around (F10 "Focus on My Mark" /
    /// `-jtac <id> focus`). While set, only contacts within
    /// `JTAC_FOCUS_RADIUS_M` of it are lased, nearest to it first.
    focus: Option<Vector2>,
    /// How many of the (sorted) contacts are lasable: in lase range, and
    /// inside the focus area if there is one. They are always the first
    /// `lasable` entries of `contacts`, see `sort_contacts`.
    lasable: usize,
    /// Magnetic variation (degrees, east positive) as of the last contact
    /// update, so bearings read to pilots can be magnetic without a lua
    /// state to hand. See `crate::atis::magnetic_variation_deg`.
    magvar_deg: f64,
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
        gid: JtId,
        name: Option<CompactString>,
        side: Side,
        priority: Vec<UnitTags>,
        location: JtacLocation,
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
            location,
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
            focus: None,
            lasable: 0,
            magvar_deg: 0.,
        }
    }

    /// "Reaper (123)" or "123", as every JTAC message names it.
    pub fn display_name(&self) -> CompactString {
        match &self.name {
            Some(n) => format_compact!("{n} ({})", self.gid),
            None => format_compact!("{}", self.gid),
        }
    }

    fn notice(&self, text: CompactString) -> JtacNotice {
        JtacNotice {
            jtid: self.gid,
            side: self.side,
            oid: self.location.oid,
            text,
        }
    }

    /// The one-line callout for a newly acquired target.
    fn acquired_line(&self, db: &Db) -> Option<CompactString> {
        let target = self.target.as_ref()?;
        let near = db
            .objective(&self.location.oid)
            .map(|o| o.name.clone())
            .unwrap_or_else(|_| String::from("unknown"));
        Some(format_compact!(
            "JTAC {} [{}] near {near}: lasing {}",
            self.display_name(),
            self.code,
            target.typ
        ))
    }

    /// The side's configured drone-waypoint action and the exact chat line
    /// that moves this JTAC to a mark, for an air JTAC that is a group (a
    /// slot JTAC is a player, who flies themselves closer).
    pub fn move_hint(&self, db: &Db) -> Option<CompactString> {
        let JtId::Group(gid) = self.gid else { return None };
        if !self.air {
            return None;
        }
        let name = db.ephemeral.cfg.actions.get(&self.side)?.iter().find_map(|(n, a)| {
            matches!(a.kind, bfprotocols::cfg::ActionKind::DroneWaypoint).then(|| n.clone())
        })?;
        Some(format_compact!("-action {name} {gid} <mark text>"))
    }

    /// Why a JTAC with enemies in view is lasing nothing, or `None` when it
    /// has a target or sees nothing at all. The status used to say a bare
    /// "no target" under a list of SAMs it could see, and players took the
    /// laser for broken -- it was the lase range (Discord, Sept 29).
    pub fn no_target_reason(&self, db: &Db) -> Option<CompactString> {
        use std::fmt::Write;
        if self.target.is_some() || self.contacts.is_empty() || self.lasable > 0 {
            return None;
        }
        let limit_km = self.lase_limit_m() / 1000.;
        let mut msg = match self.focus_area_contacts() {
            (0, _) if self.focus.is_some() => format_compact!(
                "no target: {} contact(s) in view, but none within {:.0} km of the focus mark (Clear Focus to work the whole area)",
                self.contacts.len(),
                JTAC_FOCUS_RADIUS_M / 1000.
            ),
            (n, Some(d)) if self.focus.is_some() => format_compact!(
                "no target: {n} contact(s) at the focus mark, the nearest {:.1} km from the JTAC -- it lases out to {limit_km:.1} km",
                d / 1000.
            ),
            _ => {
                let jpos = self.location.pos;
                let nearest = self
                    .contacts
                    .values()
                    .map(|ct| (Vector2::new(ct.pos.x, ct.pos.z) - jpos, &ct.typ))
                    .min_by(|a, b| a.0.norm_squared().total_cmp(&b.0.norm_squared()));
                match nearest {
                    None => return None,
                    Some((d, typ)) => format_compact!(
                        "no target: {} contact(s) in view, none in laser range. Nearest is {typ} {:03}°M {:.1} km from the JTAC -- it lases out to {limit_km:.1} km",
                        self.contacts.len(),
                        mag_deg(dcso3::azumith2d(d), self.magvar_deg),
                        d.norm() / 1000.
                    ),
                }
            }
        };
        match self.move_hint(db) {
            Some(hint) => {
                let _ = write!(msg, "\nmove it closer: {hint}");
            }
            None if matches!(self.gid, JtId::Slot(_)) => msg.push_str("\nfly closer to lase"),
            None => (),
        }
        Some(msg)
    }

    pub fn status(&self, db: &Db, loc_by_code: &LocByCode) -> Result<CompactString> {
        use std::fmt::Write;
        // A player contact who just left their aircraft has no type any more;
        // that used to fail the whole status.
        fn get_typ(db: &Db, id: &EnId) -> Vehicle {
            match id {
                EnId::Unit(uid) => db.unit(uid).map(|u| u.typ.clone()).ok(),
                EnId::Player(ucid) => db
                    .player(ucid)
                    .and_then(|p| p.current_slot.as_ref())
                    .and_then(|(_, i)| i.as_ref())
                    .map(|i| i.typ.clone()),
            }
            .unwrap_or_else(|| Vehicle::from("unknown"))
        }
        fn list(msg: &mut CompactString, counts: IndexMap<Vehicle, usize, FxBuildHasher>) {
            // The separator used to be decided against the number of
            // contacts rather than the number of types, so the list always
            // ended in a stray comma.
            for (i, (typ, count)) in counts.into_iter().enumerate() {
                if i > 0 {
                    msg.push_str(", ");
                }
                if count > 1 {
                    let _ = write!(msg, "{typ} x{count}");
                } else {
                    let _ = write!(msg, "{typ}");
                }
            }
        }
        let mut msg = CompactString::new("");
        write!(msg, "JTAC {} [{}] status\n", self.display_name(), self.code)?;
        match &self.target {
            None => match self.no_target_reason(db) {
                Some(why) => write!(msg, "{why}\n")?,
                None => write!(msg, "no target\n")?,
            },
            Some(target) => {
                let unit_typ = get_typ(db, &target.id);
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
                let pos = Vector2::new(target.pos.x, target.pos.z) - self.location.pos;
                write!(
                    msg,
                    "lasing {unit_typ} code {}{} -- {:03}°M {:.1} km from the JTAC\n",
                    self.code,
                    conflicts,
                    mag_deg(dcso3::azumith2d(pos), self.magvar_deg),
                    pos.norm() / 1000.
                )?;
            }
        };
        if let Some(bt) = &self.building_target {
            write!(msg, "designating building: {}\n", bt.label)?;
        }
        write!(
            msg,
            "position {:03}°M {:.1}km from {}, lases out to {:.1} km\n\n",
            mag_deg(self.location.bearing, self.magvar_deg),
            self.location.distance / 1000.,
            db.objective(&self.location.oid)?.name,
            self.lase_limit_m() / 1000.
        )?;
        if self.contacts.is_empty() {
            write!(msg, "No enemies in sight")?;
        } else {
            // `sort_contacts` keeps the lasable ones first. "Visual On" used
            // to list everything together, so a pilot saw SAMs "in sight" and
            // couldn't tell they were twice the laser's range away.
            let mut near: IndexMap<Vehicle, usize, FxBuildHasher> = IndexMap::default();
            let mut far: IndexMap<Vehicle, usize, FxBuildHasher> = IndexMap::default();
            for (i, id) in self.contacts.keys().enumerate() {
                let bucket = if i < self.lasable { &mut near } else { &mut far };
                *bucket.entry(get_typ(db, id)).or_insert(0) += 1;
            }
            if !near.is_empty() {
                write!(msg, "In laser range: ")?;
                list(&mut msg, near);
            }
            if !far.is_empty() {
                if self.lasable > 0 {
                    msg.push('\n');
                }
                let why = if self.focus.is_some() { "out of range / outside focus" } else { "out of laser range" };
                write!(msg, "Seen, {why}: ")?;
                list(&mut msg, far);
            }
        }
        write!(
            msg,
            "\n\nmode: {}, IR pointer: {}{}",
            if self.autoshift.is_none() { "auto" } else { "manual (Shift cycles targets)" },
            if self.ir_pointer { "on" } else { "off" },
            if self.focus.is_some() { ", focused on a mark" } else { "" }
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

    /// The standard 9-line for the current target. Bearings are magnetic.
    /// This used to carry no target coordinates at all and measured lines 2
    /// and 3 from the JTAC rather than from the IP it named in line 1, so a
    /// pilot running in from the IP flew the wrong heading and distance.
    pub fn nine_line(&self, db: &Db, lua: MizLua) -> Result<CompactString> {
        use std::fmt::Write;
        let target = match &self.target {
            None => bail!("no target — lase a target first"),
            Some(t) => t,
        };
        let var = self.magvar_deg;
        let tgt = Vector2::new(target.pos.x, target.pos.z);
        // The IP is the objective the JTAC works from. Only when it can't be
        // found, or sits right on the target, run lines 2/3 from the JTAC.
        let ip = db
            .objective(&self.location.oid)
            .ok()
            .map(|o| (o.name.clone(), o.pos()));
        let (ip_label, from) = match &ip {
            Some((name, p)) if (tgt - *p).norm() >= 500. => (format_compact!("{name}"), *p),
            _ => (
                format_compact!("none -- run in from JTAC {}", self.display_name()),
                self.location.pos,
            ),
        };
        let run = tgt - from;
        let hdg = mag_deg(dcso3::azumith2d(run), var);
        let egress = if hdg > 180 { hdg - 180 } else { hdg + 180 };
        let elev_ft = (target.pos.y * 3.28084).round() as i32;
        let (ll, mgrs) = crate::menu::objectives::fmt_position(lua, tgt)
            .unwrap_or_else(|| (CompactString::new("--"), CompactString::new("--")));
        let (ref_dist, ref_brg, ref_obj) =
            Db::objective_near_point(&db.persisted.objectives, tgt, |_| true)
                .context("no objectives found")?;
        let to_jtac = self.location.pos - tgt;
        let mut msg = CompactString::new("");
        write!(msg, "== 9-LINE CAS - JTAC {} ==\n", self.display_name())?;
        write!(msg, "1. IP: {ip_label}\n")?;
        write!(msg, "2. HDG: {hdg:03}°M IP to target\n")?;
        write!(
            msg,
            "3. DIST: {:.1}km / {:.1}nm\n",
            run.norm() / 1000.,
            run.norm() / 1852.
        )?;
        write!(msg, "4. ELEV: {elev_ft}ft MSL\n")?;
        write!(msg, "5. DESC: {} ({} contact(s) visible)\n", target.typ, self.contacts.len())?;
        write!(msg, "6. LOC: {ll}\n    MGRS {mgrs}\n")?;
        write!(
            msg,
            "    {:03}°M {:.1}km from {}\n",
            mag_deg(ref_brg, var),
            ref_dist / 1000.,
            ref_obj.name
        )?;
        write!(
            msg,
            "7. MARK: LASER {}{}\n",
            self.code,
            if self.ir_pointer { " + IR pointer" } else { "" }
        )?;
        write!(
            msg,
            "8. FRIENDLIES: JTAC {:03}°M {:.1}km from target\n",
            mag_deg(dcso3::azumith2d(to_jtac), var),
            to_jtac.norm() / 1000.
        )?;
        write!(msg, "9. EGRESS: {egress:03}°M")?;
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
        // The map layer's marks first. They used to come after the spot, and
        // any failure destroying it (a spot that went with a dead JTAC) bailed
        // out before them -- so a group JTAC that died lasing something could
        // leave its bearing line, pin and code label on the map with nothing
        // tracking them any more.
        if let JtId::Group(gid) = self.gid {
            let (map_layer, msgs) = db.ephemeral.map_layer_and_msgs();
            map_layer.on_jtac_cleared(&gid, msgs);
        }
        if let Some(target) = self.target.take() {
            target
                .destroy(lua, db.ephemeral.msgs())
                .with_context(|| format_compact!("destroying target for jtac {}", self.gid))?;
        }
        Ok(())
    }

    /// Everything this JTAC has on the map, for when it is gone. Only the
    /// unit target used to be cleared, so a JTAC that died designating a
    /// building left that building's pin up forever.
    fn stand_down(&mut self, db: &mut Db, lua: MizLua) {
        if let Err(e) = self.remove_target(db, lua) {
            warn!("could not remove target of dead jtac {}: {e:?}", self.gid)
        }
        if let Err(e) = self.remove_building_target(db, lua) {
            warn!("could not remove building target of dead jtac {}: {e:?}", self.gid)
        }
    }

    /// Pin the target on the F10 map, for slot JTACs only. Group JTACs get
    /// the map layer's pin, code label and bearing line (`draw_layer`), and
    /// this used to drop a second pin for them on top -- straight through
    /// Trigger, bypassing the message queue, and again every time the target
    /// crept more than a metre and a half, i.e. every second for anything
    /// driving. That churn is what starved the F10 markup queue before. Now
    /// the pin goes through the queue and only follows a target that has
    /// really moved (`JTAC_PIN_FOLLOW_M`), unless `force` (new target, new
    /// code) says the text itself changed.
    fn mark_target(&mut self, db: &mut Db, force: bool) {
        if !matches!(self.gid, JtId::Slot(_)) {
            return;
        }
        let Some(target) = &mut self.target else { return };
        let Some(ct) = self.contacts.get(&target.id) else {
            warn!("jtac {} target {} is not a contact, not marking it", self.gid, target.id);
            return;
        };
        let pos = Vector2::new(ct.pos.x, ct.pos.z);
        if !force && target.mark.is_some() && (pos - target.mark_pos).norm() < JTAC_PIN_FOLLOW_M {
            return;
        }
        let text = format_compact!(
            "JTAC {} target {} marked by code {}",
            self.gid,
            ct.typ,
            self.code
        );
        let msgs = db.ephemeral.msgs();
        if let Some(mid) = target.mark.take() {
            msgs.delete_mark(mid);
        }
        target.mark = Some(msgs.mark_to_side(self.side, pos, true, text));
        target.mark_pos = pos;
    }

    /// (Re)draw the map layer's marks for a group JTAC's current target:
    /// bearing line, info pin and laser-code label.
    fn draw_layer(&self, db: &mut Db) {
        let (JtId::Group(gid), Some(t)) = (self.gid, self.target.as_ref()) else {
            return;
        };
        let text = format_compact!("JTAC {}\nlasing {}\nCode: {}", self.gid, t.typ, self.code);
        let (map_layer, msgs) = db.ephemeral.map_layer_and_msgs();
        map_layer.on_jtac_target(
            gid,
            self.location.pos,
            Vector2::new(t.pos.x, t.pos.z),
            self.lase_range_m,
            self.side,
            text,
            self.code,
            msgs,
        );
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
                    mark_pos: Vector2::new(pos.x, pos.z),
                    id,
                });
                self.nearby_artillery =
                    db.artillery_near_point(self.side, Vector2::new(pos.x, pos.z));
                self.nearby_alcm = db.alcm_near_point(self.side, lua, Vector2::new(pos.x, pos.z));
                self.menu_dirty |= prev_arty != self.nearby_artillery;
                self.menu_dirty |= prev_alcm != self.nearby_alcm;
                self.mark_target(db, true);
                self.draw_layer(db);
                Ok(true)
            }
        }
    }

    /// Cycle through the logistics-relevant scenery buildings (the ones pinned
    /// with logi markers on the F10 map, from `scan_objective_scenery`) tracked
    /// at this JTAC's nearest objective and lase the next one. Returns the
    /// callout for it (the caller decides who hears it), or `None` if there
    /// are none left standing there.
    pub fn designate_building(&mut self, db: &mut Db, lua: MizLua) -> Result<Option<CompactString>> {
        let candidates = db.ephemeral.scenery_at_objective(self.location.oid);
        if candidates.is_empty() {
            self.remove_building_target(db, lua)?;
            return Ok(None);
        }
        self.building_idx = (self.building_idx + 1) % candidates.len();
        let (id, label) = candidates[self.building_idx].clone();
        if let Some(bt) = &self.building_target {
            if bt.id == id {
                return Ok(Some(format_compact!(
                    "JTAC {} still designating building: {} (logistics target), code {}",
                    self.gid,
                    bt.label,
                    self.code
                )));
            }
        }
        let pos = match Object::get_instance(lua, &id) {
            Ok(o) => o.get_point().context("getting building position")?.0,
            Err(_) => {
                // stale entry, e.g. destroyed between scan and now -- retry next cycle
                self.remove_building_target(db, lua)?;
                return Ok(None);
            }
        };
        self.remove_building_target(db, lua)?;
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
        let brg_deg = mag_deg(dcso3::azumith2d(diff), self.magvar_deg);
        let msg = format_compact!(
            "JTAC {} designating building: {label} (logistics target) {brg_deg:03}°M / {:.1}km from the JTAC, code {}",
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
            label,
            spot,
            mark: Some(mid),
        });
        Ok(Some(msg))
    }

    fn remove_building_target(&mut self, db: &mut Db, lua: MizLua) -> Result<()> {
        if let Some(target) = self.building_target.take() {
            target
                .destroy(lua, db.ephemeral.msgs())
                .with_context(|| format_compact!("destroying building target for jtac {}", self.gid))?;
        }
        Ok(())
    }

    /// Change the laser code (see `apply_code_part` for what `code_part` may
    /// be). The pins that print the code are redrawn by the caller, which has
    /// the db.
    fn set_code(&mut self, lua: MizLua, code_part: u16) -> Result<()> {
        self.code = apply_code_part(self.code, code_part)?;
        if let Some(target) = &self.target {
            let spot = Spot::get_instance(lua, &target.spot).context("getting laser spot")?;
            spot.set_code(self.code).context("setting laser code")?;
        }
        Ok(())
    }

    pub fn shift(&mut self, db: &mut Db, lua: MizLua) -> Result<bool> {
        // Only cycles the lasable contacts -- the ones out of range or
        // outside the focus area are sorted after them.
        let n = self.lasable;
        if n == 0 {
            return Ok(false);
        }
        // Step on from where the current target sits NOW. The contacts are
        // re-sorted every update, so the index remembered in `autoshift` from
        // the last shift pointed at whatever had moved into that place since,
        // and Shift skipped or repeated targets.
        let cur = self
            .target
            .as_ref()
            .and_then(|t| self.contacts.get_index_of(&t.id))
            .or(self.autoshift);
        let i = match cur {
            None => 0,
            Some(i) if i + 1 < n => i + 1,
            Some(_) => 0,
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

    fn lase_limit_m(&self) -> f64 {
        self.lase_range_m.min(JTAC_MAX_LASE_M)
    }

    /// Order the contacts lasable first, then by the priority list, then
    /// nearest first (to the focus mark if there is one, else to the JTAC).
    /// The priority list used to be the ONLY key, so a top-priority SAM at
    /// the edge of a drone's 90 km spotting range beat every closer target.
    fn sort_contacts(&mut self, db: &mut Db, lua: MizLua) -> Result<bool> {
        let plist = self.priority.clone();
        let priority = |tags: UnitTags| {
            plist
                .iter()
                .enumerate()
                .find(|(_, p)| tags.contains(p.0))
                .map(|(i, _)| i)
                .unwrap_or(plist.len())
        };
        let jpos = self.location.pos;
        let anchor = self.focus.unwrap_or(jpos);
        let lase2 = self.lase_limit_m().powi(2);
        let focus2 = JTAC_FOCUS_RADIUS_M.powi(2);
        let focused = self.focus.is_some();
        let key = |ct: &Contact| {
            let p = Vector2::new(ct.pos.x, ct.pos.z);
            let from_anchor = (p - anchor).norm_squared();
            let lasable =
                (p - jpos).norm_squared() <= lase2 && (!focused || from_anchor <= focus2);
            (!lasable, priority(ct.tags), from_anchor)
        };
        self.contacts.sort_by(|_, ct0, _, ct1| {
            let (l0, p0, d0) = key(ct0);
            let (l1, p1, d1) = key(ct1);
            l0.cmp(&l1).then(p0.cmp(&p1)).then(d0.total_cmp(&d1))
        });
        self.lasable = self.contacts.values().take_while(|ct| !key(ct).0).count();
        let mut target_idx = self
            .target
            .as_ref()
            .and_then(|t| self.contacts.get_index_of(&t.id));
        // The target drove out of range or out of the focus area: drop it and
        // let auto re-acquire, rather than holding a spot the JTAC can't make.
        if let Some(ti) = target_idx {
            if ti >= self.lasable {
                self.remove_target(db, lua)?;
                self.autoshift = None;
                target_idx = None;
            }
        }
        // Auto-acquire the top contact when in auto mode, OR any time we have
        // lasable contacts but no target at all (e.g. the manually-shifted
        // target just died/left -- don't sit on "no target" while enemies are
        // still in view).
        if self.lasable > 0 && (self.autoshift.is_none() || self.target.is_none()) {
            let i = match (self.autoshift, target_idx) {
                (Some(i), _) if i < self.lasable => i,
                // Stay on the current target while it is still as important
                // as the best one. Distance is now a sort key, so without this
                // two tanks rolling past each other would swap the spot -- and
                // redraw the map marks -- every update.
                (None, Some(ti))
                    if priority(self.contacts[ti].tags) == priority(self.contacts[0].tags) =>
                {
                    ti
                }
                _ => 0,
            };
            return self.set_target(db, lua, i).context("setting target");
        }
        Ok(false)
    }

    /// Point the JTAC at a player's map mark (or clear that with `None`).
    /// Returns how many contacts it can lase there.
    pub fn set_focus(&mut self, db: &mut Db, lua: MizLua, focus: Option<Vector2>) -> Result<usize> {
        self.focus = focus;
        self.autoshift = None;
        // the menu entry reads differently with a focus set
        self.menu_dirty = true;
        self.remove_target(db, lua)?;
        self.sort_contacts(db, lua)?;
        Ok(self.lasable)
    }

    pub fn focus(&self) -> Option<Vector2> {
        self.focus
    }

    /// How far out this JTAC can put a laser spot, metres.
    pub fn lase_limit(&self) -> f64 {
        self.lase_limit_m()
    }

    /// Contacts inside the focus area, and how far the nearest of them is
    /// from the JTAC itself -- so a focus that finds nothing to lase can say
    /// whether that is because the area is empty or out of laser range.
    pub fn focus_area_contacts(&self) -> (usize, Option<f64>) {
        let Some(f) = self.focus else { return (0, None) };
        let jpos = self.location.pos;
        let mut n = 0;
        let mut nearest: Option<f64> = None;
        for ct in self.contacts.values() {
            let p = Vector2::new(ct.pos.x, ct.pos.z);
            if (p - f).norm() <= JTAC_FOCUS_RADIUS_M {
                n += 1;
                let d = (p - jpos).norm();
                nearest = Some(nearest.map_or(d, |m: f64| m.min(d)));
            }
        }
        (n, nearest)
    }

    pub fn smoke_target(&mut self, lua: MizLua) -> Result<()> {
        if let Some(target) = &self.target {
            if let Some(ct) = self.contacts.get(&target.id) {
                let now = Utc::now();
                let cooldown = Duration::seconds(60);
                let since = now - self.last_smoke;
                if since < cooldown {
                    // was the time SINCE the last smoke, which counted up
                    let rdy = (cooldown - since).num_seconds().max(1);
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
                    counter_battery_radius: crate::shoot_and_scoot(&db.ephemeral.cfg),
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
                        counter_battery_radius: crate::shoot_and_scoot(&db.ephemeral.cfg),
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
        let per_target = n.pop().ok_or_else(|| anyhow!("missing ALCM per-target count"))?;
        let magazine_expend = n.pop().ok_or_else(|| anyhow!("missing ALCM expend"))?;

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
            // "the target is a contact" normally holds, but it has been seen to
            // break (fast slot cycling) and this used to unwrap -- a panic in
            // the timed-events loop. Skip the update and let the next contact
            // pass re-acquire instead.
            let Some(contact) = self.contacts.get_mut(&target.id) else {
                warn!(
                    "jtac {} target {} is not among its contacts, skipping position update",
                    self.gid, target.id
                );
                return Ok(());
            };
            if (contact.pos - pos).magnitude_squared() > 2. {
                contact.pos = pos;
                let typ_clone = contact.typ.clone();
                let spot =
                    Spot::get_instance(lua, &target.spot).context("getting the spot instance")?;
                spot.set_point(LuaVec3(contact.pos + velocity))
                    .context("setting the spot position")?;
                // Keep the target's own copy current too: the 9-line and the
                // fire missions read it, and it used to stay where the target
                // was first lased.
                if let Some(t) = &mut self.target {
                    t.pos = pos;
                }
                self.mark_target(db, false);
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
            // Back to auto: let the auto rules pick. This used to lase
            // contact 0 outright -- an error with nothing in view, and with
            // everything out of range, a spot on a SAM 40 km away.
            Some(_) => {
                self.autoshift = None;
                self.sort_contacts(db, lua)?;
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

    /// Everything this JTAC can currently see. Read-only — used to build the
    /// spoken situation update and target description.
    pub fn visible_contacts(&self) -> impl Iterator<Item = &Contact> {
        self.contacts.values()
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

/// Is the unit or player `id` names no longer in the fight: unknown, dead,
/// or a player out of their aircraft?
fn target_gone(db: &Db, id: &EnId) -> bool {
    match id {
        EnId::Unit(uid) => db.unit(uid).map_or(true, |u| u.dead),
        EnId::Player(ucid) => db
            .player(ucid)
            .and_then(|p| p.current_slot.as_ref())
            .and_then(|(_, inst)| inst.as_ref())
            .is_none(),
    }
}

/// A group's "Status" pin: the target it was dropped on, by which JTAC,
/// and where, so `Jtacs::reconcile_marks` can tell when it has gone stale.
#[derive(Debug, Clone, Copy)]
struct StatusMark {
    id: MarkId,
    jtid: JtId,
    target: EnId,
    pos: Vector2,
}

impl StatusMark {
    /// The pin names one target at one spot. It is stale once that JTAC is
    /// gone or lasing something else (`current` is its target now, if any),
    /// or the target has driven off from under it.
    fn stale(&self, current: Option<(EnId, Vector2)>) -> bool {
        match current {
            None => true,
            Some((id, pos)) => id != self.target || (pos - self.pos).norm() >= JTAC_PIN_FOLLOW_M,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Jtacs {
    jtacs: FxHashMap<Side, FxHashMap<JtId, Jtac>>,
    detected: FxHashMap<Side, FxHashMap<EnId, Detected>>,
    artillery_adjustment: FxHashMap<GroupId, ArtilleryAdjustment>,
    code_by_location: LocByCode,
    menu_dirty: FxHashMap<Side, FxHashSet<ObjectiveId>>,
    /// Callouts waiting for `menu::jtac::flush_jtac_notices`.
    notices: Vec<JtacNotice>,
    /// The read-only pin each group last got from "Status", keyed by miz
    /// group. Replaced on the next Status instead of piling up -- every click
    /// used to drop a permanent one -- and removed by `reconcile_marks` once
    /// its target is gone.
    status_marks: FxHashMap<dcso3::env::miz::GroupId, StatusMark>,
    /// Magnetic variation, refreshed every contact update, see `Jtac::magvar_deg`.
    magvar_deg: f64,
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

    /// Drain the queued callouts, see `JtacNotice`.
    pub fn take_notices(&mut self) -> Vec<JtacNotice> {
        std::mem::take(&mut self.notices)
    }

    /// Remember `mark`, dropped at `pos` on `jtid`'s `target`, as `group`'s
    /// status pin, returning the one it replaces.
    pub fn replace_status_mark(
        &mut self,
        group: dcso3::env::miz::GroupId,
        mark: MarkId,
        jtid: JtId,
        target: EnId,
        pos: Vector2,
    ) -> Option<MarkId> {
        let sm = StatusMark { id: mark, jtid, target, pos };
        self.status_marks.insert(group, sm).map(|old| old.id)
    }

    /// Other JTACs on `side` lasing on `code`.
    pub fn code_users(&self, side: Side, code: u16, except: JtId) -> SmallVec<[JtId; 4]> {
        self.jtacs
            .get(&side)
            .map(|jtx| {
                jtx.values()
                    .filter(|j| j.code == code && j.gid != except)
                    .map(|j| j.gid)
                    .collect()
            })
            .unwrap_or_default()
    }

    /// The objective a gun battery belongs to -- the nearest friendly objective
    /// to its center. The artillery menu groups guns by this, and a battery
    /// fire mission targets exactly the guns that answer with the same id.
    pub fn artillery_objective(db: &Db, gid: &GroupId, side: Side) -> Option<ObjectiveId> {
        let pos = db.group_center(gid).ok()?;
        Db::objective_near_point(&db.persisted.objectives, pos, |o| o.owner == side)
            .map(|(_, _, o)| o.id)
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
        let typ = group
            .and_then(|g| g.units.into_iter().next())
            .and_then(|uid| db.unit(uid).ok())
            .map(|u| u.typ.clone());
        let per_unit = typ.as_ref().and_then(|t| {
            cfg.artillery
                .as_ref()
                .and_then(|a| a.units.get(t.as_str()))
        });
        let (min, max) = match per_unit {
            Some(r) => (r.min_range_m, r.max_range_m),
            // Nothing configured for this type -- ask the harvested DCS unit
            // db before falling back to the flat pair, which has no idea a
            // ballistic TEL can't shoot anything inside 50km.
            None => typ
                .as_ref()
                .and_then(|t| {
                    let udb = crate::unitdb::get();
                    let info = udb.get(t.as_str())?;
                    let max = info.threat_range_m?;
                    Some((info.threat_range_min_m.unwrap_or(0.0), max))
                })
                .unwrap_or((cfg.artillery_min_range as f64, cfg.artillery_mission_range as f64)),
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
    /// Fire several guns at the JTAC's target at once. `at` narrows the salvo
    /// to the battery sitting at one objective; `None` fires every gun the
    /// JTAC has in range.
    pub fn fire_all_artillery_together(
        &mut self,
        db: &Db,
        lua: MizLua,
        jtid: &JtId,
        n: u8,
        at: Option<ObjectiveId>,
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
        let side = jtac.side;
        let mut arty_gids: SmallVec<[GroupId; 8]> = jtac.nearby_artillery.clone();
        if let Some(oid) = at {
            arty_gids.retain(|gid| Jtacs::artillery_objective(db, gid, side) == Some(oid));
            if arty_gids.is_empty() {
                bail!("that battery has no guns left");
            }
        }
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
                counter_battery_radius: crate::shoot_and_scoot(&db.ephemeral.cfg),
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
    /// other parts of the existing code are left alone. A whole four digit code
    /// (e.g. 1513) replaces the code outright. The result must be a valid code
    /// (`validate_laser_code`).
    pub fn set_code_part(&mut self, db: &mut Db, lua: MizLua, gid: &JtId, code_part: u16) -> Result<()> {
        let jt = self.get_mut(gid)?;
        let prev_code = jt.code;
        let oid = jt.location.oid;
        let side = jt.side;
        jt.set_code(lua, code_part)?;
        let code = jt.code;
        jt.persist_state(db);
        // Both pins print the code, so they are redrawn with it; and the
        // JTAC's menu label carries it too.
        jt.mark_target(db, true);
        jt.draw_layer(db);
        jt.menu_dirty = true;
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

    /// Safety net for every F10 mark the JTACs own, run on the slow tick
    /// after the contact update.
    ///
    /// Players asked for JTAC drones to wipe and redraw their marks every few
    /// minutes, because stale target pins and labels were piling up around
    /// busy bases. Wiping and redrawing everything is exactly the churn that
    /// starved the F10 markup queue before, so this only looks, and deletes
    /// just the marks whose subject is gone: a target that died (the JTAC
    /// then re-acquires, which draws the next one fresh), a designated
    /// building that was destroyed, map-layer marks no living JTAC target
    /// owns, and Status pins whose target died or moved on. The leaks this
    /// catches are fixed where they start; this is here for the next one.
    pub fn reconcile_marks(&mut self, lua: MizLua, db: &mut Db) {
        let mut live_layers: FxHashSet<GroupId> = FxHashSet::default();
        for jt in self.jtacs.values_mut().flat_map(|jtx| jtx.values_mut()) {
            if let Some(tid) = jt.target.as_ref().map(|t| t.id) {
                if target_gone(db, &tid) {
                    info!("jtac {} target {tid} is gone, clearing its marks", jt.gid);
                    if let Err(e) = jt.remove_contact(lua, db, &tid) {
                        warn!("could not remove gone target {tid} of jtac {}: {e:?}", jt.gid)
                    }
                }
            }
            let building_gone = jt
                .building_target
                .as_ref()
                .map_or(false, |bt| !db.ephemeral.scenery_standing(&bt.id));
            if building_gone {
                if let Err(e) = jt.remove_building_target(db, lua) {
                    warn!("could not remove destroyed building target of jtac {}: {e:?}", jt.gid)
                }
            }
            if let (JtId::Group(gid), Some(_)) = (jt.gid, &jt.target) {
                live_layers.insert(gid);
            }
        }
        let (map_layer, msgs) = db.ephemeral.map_layer_and_msgs();
        let orphans: SmallVec<[GroupId; 8]> = map_layer
            .jtac_marks
            .keys()
            .filter(|gid| !live_layers.contains(gid))
            .copied()
            .collect();
        for gid in orphans {
            info!("removing orphaned jtac map marks of group {gid}");
            map_layer.on_jtac_cleared(&gid, msgs);
        }
        let stale: SmallVec<[dcso3::env::miz::GroupId; 8]> = self
            .status_marks
            .iter()
            .filter(|(_, sm)| {
                let current = self
                    .get(&sm.jtid)
                    .ok()
                    .and_then(|jt| jt.target.as_ref())
                    .map(|t| (t.id, Vector2::new(t.pos.x, t.pos.z)));
                sm.stale(current)
            })
            .map(|(group, _)| *group)
            .collect();
        for group in stale {
            if let Some(sm) = self.status_marks.remove(&group) {
                db.ephemeral.msgs().delete_mark(sm.id);
            }
        }
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
                                jt.stand_down(db, lua);
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
                        let typ = jt.target.as_ref().map(|t| t.typ.clone());
                        if let Err(e) = jt.remove_target(db, lua) {
                            warn!("1 could not remove jtac target {:?}", e)
                        }
                        let text = match typ {
                            Some(typ) => format_compact!(
                                "JTAC {} [{}]: target {typ} destroyed",
                                jt.display_name(),
                                jt.code
                            ),
                            None => format_compact!("JTAC {}: target destroyed", jt.display_name()),
                        };
                        self.notices.push(jt.notice(text));
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
        let Some(location) = JtacLocation::new(db, pos) else {
            warn!("jtac {id} has no objective to locate itself by, skipping it");
            return Ok(());
        };
        let magvar_deg = self.magvar_deg;
        // Codes already taken on this side, for a JTAC being created now.
        let in_use: FxHashSet<u16> = match self.jtacs.get(&side) {
            Some(jtx) if !jtx.contains_key(&id) => jtx.values().map(|j| j.code).collect(),
            Some(_) => FxHashSet::default(),
            None => FxHashSet::default(),
        };
        let detected = self.detected.entry(side.opposite()).or_default();
        let range = (spec.range as f64).powi(2);
        let jtac = self
            .jtacs
            .entry(side)
            .or_default()
            .entry(id)
            .or_insert_with(|| {
                let mut jt = Jtac::new(
                    id,
                    spec.name.as_deref().map(CompactString::new),
                    side,
                    db.ephemeral.cfg.jtac_priority.clone(),
                    location,
                    air,
                    pick_laser_code(spec.default_laser_code, &in_use),
                    spec.range as f64,
                );
                jt.magvar_deg = magvar_deg;
                if let Some(st) = state.clone() {
                    jt.apply_state(st);
                    let obj_name = db.objective(&jt.location.oid).map(|o| o.name()).unwrap_or("Unknown location");
                    let msg = format_compact!(
                        "JTAC {} [{}] is online {:03}°M {:.0} meters from {}",
                        jt.display_name(),
                        jt.code,
                        mag_deg(jt.location.bearing, magvar_deg),
                        jt.location.distance,
                        obj_name
                    );
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
        jtac.location = location;
        jtac.magvar_deg = magvar_deg;
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
            // Marked dead without leaving the instanced set: ghost
            // retirement and the capture sweep flag units dead and only drop
            // their DCS mapping once a despawn drains, and a partly ghosted
            // group never does. The JTAC kept "seeing" those -- teleported
            // back to their spawn points by the dead reset -- and kept their
            // intel pins at full confidence around the base indefinitely.
            // Unseen, they fall out of the contacts like any despawned unit.
            if unit.dead {
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
            // Indexed directly before: an unclassified player airframe
            // panicked the whole contact update.
            let Some(tags) = db.ephemeral.cfg.unit_classification.get(&inst.typ).copied() else {
                warn!("jtac: player aircraft {} is not in unit_classification", inst.typ);
                continue;
            };
            if !jtac.filter.is_empty() && !tags.intersects(jtac.filter) {
                to_remove.push(id);
                continue;
            }
            // Dropped like the airborne-unit case above. A bare `continue`
            // kept the contact -- at the spot the jet took off from -- and a
            // JTAC lasing it kept the spot there for the rest of the sortie.
            if inst.in_air && !tags.contains(UnitTag::Helicopter) {
                to_remove.push(id);
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
        self.magvar_deg = crate::atis::magnetic_variation_deg(lua, &db.ephemeral.cfg);
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
                    jt.stand_down(db, lua);
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
        for (_, gid, uid) in lost_targets {
            // `?` here used to abandon the rest of the update (menus, the
            // acquire callouts) over one JTAC that had just gone away.
            let Ok(jt) = self.get_mut(&gid) else {
                warn!("lost target for jtac {gid}, which no longer exists");
                continue;
            };
            match uid {
                Some(uid) => match jt.remove_contact(lua, db, &uid) {
                    Ok(_) => (),
                    Err(e) => warn!("3 could not remove jtac target {uid} {:?}", e),
                },
                None => {
                    let notice =
                        jt.notice(format_compact!("JTAC {} [{}]: target lost", jt.display_name(), jt.code));
                    self.notices.push(notice);
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
        // One line to the players following the JTAC. This was the full
        // multi-line status panel, to the whole coalition, on every acquire.
        for jtac in new_contacts {
            if let Some(line) = jtac.acquired_line(db) {
                self.notices.push(jtac.notice(line));
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn laser_code_validation() {
        for ok in [1111, 1688, 1788, 1511, 1234] {
            assert!(validate_laser_code(ok).is_ok(), "{ok}");
        }
        for bad in [0, 1000, 1110, 1189, 1800, 1900, 2111, 1698, 11111, 688] {
            assert!(validate_laser_code(bad).is_err(), "{bad}");
        }
    }

    #[test]
    fn code_parts_and_whole_codes() {
        // whole codes, which "-jtac <id> code 1688" used to reject
        assert_eq!(apply_code_part(1111, 1688).unwrap(), 1688);
        assert!(apply_code_part(1688, 1699).is_err());
        // single digits at their scale, as the F10 Code menu sends them
        assert_eq!(apply_code_part(1688, 1000).unwrap(), 1688);
        assert_eq!(apply_code_part(1688, 500).unwrap(), 1588);
        assert_eq!(apply_code_part(1688, 30).unwrap(), 1638);
        assert_eq!(apply_code_part(1688, 2).unwrap(), 1682);
        // digits that make an invalid code
        assert!(apply_code_part(1688, 9).is_err());
        assert!(apply_code_part(1688, 0).is_err());
        assert!(apply_code_part(1688, 800).is_err());
        // mixed scales that aren't a whole code
        assert!(apply_code_part(1688, 150).is_err());
    }

    #[test]
    fn new_jtacs_get_unique_codes() {
        let mut used = FxHashSet::default();
        let a = pick_laser_code(1688, &used);
        assert_eq!(a, 1688);
        used.insert(a);
        let b = pick_laser_code(1688, &used);
        assert_ne!(b, a);
        assert!(validate_laser_code(b).is_ok());
        // wraps round past 1788
        let used: FxHashSet<u16> = (1688u16..=1788).collect();
        let c = pick_laser_code(1688, &used);
        assert!(c < 1688 && validate_laser_code(c).is_ok());
        // an invalid configured default falls back to a valid one
        assert!(validate_laser_code(pick_laser_code(1000, &FxHashSet::default())).is_ok());
    }

    #[test]
    fn status_pin_goes_with_its_target() {
        let tank = EnId::Unit(UnitId::from(1));
        let other = EnId::Unit(UnitId::from(2));
        let here = Vector2::new(1_000., 2_000.);
        let sm = StatusMark { id: MarkId::new(), jtid: JtId::Group(GroupId::from(7)), target: tank, pos: here };
        // Same target, crept a little: keep the pin.
        assert!(!sm.stale(Some((tank, here + Vector2::new(100., 0.)))));
        // JTAC gone or lasing nothing / something else, or the target drove off.
        assert!(sm.stale(None));
        assert!(sm.stale(Some((other, here))));
        assert!(sm.stale(Some((tank, here + Vector2::new(JTAC_PIN_FOLLOW_M, 0.)))));
    }
}
