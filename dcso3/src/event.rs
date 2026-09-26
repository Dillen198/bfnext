/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use std::marker::PhantomData;

use crate::{object::DcsOid, unit::ClassUnit};

use super::{
    as_tbl, as_tbl_ref, lua_err, object::Object, unit::Unit, value_to_json,
    weapon::Weapon, world::MarkPanel, String, Time,
};
use anyhow::Result;
use log::debug;
use mlua::{prelude::*, Value};
use serde_derive::Serialize;

#[derive(Debug, Clone, Serialize)]
pub enum BirthPlace {
    Air,
    Runway,
    Park,
    HeliportHot,
    HeliportCold,
}

#[derive(Debug, Clone, Serialize)]
pub struct Shot<'lua> {
    pub time: Time,
    pub initiator: Unit<'lua>,
    pub weapon: Weapon<'lua>,
    pub weapon_name: Option<String>,
}

impl<'lua> FromLua<'lua> for Shot<'lua> {
    fn from_lua(value: Value<'lua>, _: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("Shot", None, value).map_err(lua_err)?;
        Ok(Self {
            time: tbl.raw_get("time")?,
            initiator: tbl.raw_get("initiator")?,
            weapon: tbl.raw_get("weapon")?,
            weapon_name: opt_weapon_name(&tbl)?,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ShootingEnd<'lua> {
    pub time: Time,
    pub initiator: Unit<'lua>,
    pub weapon_name: Option<String>,
}

impl<'lua> FromLua<'lua> for ShootingEnd<'lua> {
    fn from_lua(value: Value<'lua>, _: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("Shot", None, value).map_err(lua_err)?;
        Ok(Self {
            time: tbl.raw_get("time")?,
            initiator: tbl.raw_get("initiator")?,
            weapon_name: opt_weapon_name(&tbl)?,
        })
    }
}

/// `weapon_name` as DCS actually supplies it.
///
/// DCS leaves the field nil on some hits -- notably cluster submunitions, where
/// the parent weapon is already gone by the time the hit registers. Reading
/// that straight into `String` went through the catch-all arm of dcso3's
/// `FromLua` impl, which stringifies whatever it is given, so the *literal text
/// "nil"* was stored as the weapon name and shown that way in the kill log.
/// `Option<String>` is what the field really is: mlua maps Lua nil to None
/// before `String::from_lua` is ever reached. An empty name is no more useful
/// than a missing one, so it collapses to None too.
fn opt_weapon_name(tbl: &mlua::Table) -> LuaResult<Option<String>> {
    Ok(tbl
        .raw_get::<_, Option<String>>("weapon_name")?
        .filter(|s| !s.as_str().is_empty()))
}

/// DCS sometimes hands an event an `initiator`/`target` that is a bare table
/// with no object metatable -- e.g. a Hit/Kill/Dead/Score for a shell fired by
/// a unit that has since died. Those can't be turned into a usable `Object`
/// (any method call would fail anyway), so degrade them to `None` instead of
/// failing the whole event and spamming the log.
fn opt_object<'lua>(
    tbl: &LuaTable<'lua>,
    key: &str,
    lua: &'lua Lua,
) -> LuaResult<Option<Object<'lua>>> {
    match tbl.raw_get::<_, Value<'lua>>(key)? {
        Value::Table(t) if t.get_metatable().is_some() => {
            Ok(Some(Object::from_lua(Value::Table(t), lua)?))
        }
        _ => Ok(None),
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct WeaponUse<'lua> {
    pub time: Time,
    pub initiator: Option<Object<'lua>>,
    pub target: Option<Object<'lua>>,
    pub weapon_name: Option<String>,
    /// The weapon object itself, when DCS supplies a live one. Hit and Kill
    /// carry it for missiles, bombs and rockets (not for gun rounds), and it
    /// is the only thing that ties a hit back to the Shot that launched it.
    pub weapon: Option<Weapon<'lua>>,
}

impl<'lua> FromLua<'lua> for WeaponUse<'lua> {
    fn from_lua(value: Value<'lua>, lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("WeaponUse", None, value).map_err(lua_err)?;
        Ok(Self {
            time: tbl.raw_get("time")?,
            initiator: opt_object(&tbl, "initiator", lua)?,
            target: opt_object(&tbl, "target", lua)?,
            weapon_name: opt_weapon_name(&tbl)?,
            weapon: opt_weapon(&tbl, lua)?,
        })
    }
}

/// The `weapon` field of an event, if it is a real weapon object. Same
/// reasoning as `opt_object`: a bare table without a metatable is a weapon
/// that no longer exists and cannot be called.
fn opt_weapon<'lua>(tbl: &LuaTable<'lua>, lua: &'lua Lua) -> LuaResult<Option<Weapon<'lua>>> {
    match tbl.raw_get::<_, Value<'lua>>("weapon")? {
        Value::Table(t) if t.get_metatable().is_some() => {
            Ok(Weapon::from_lua(Value::Table(t), lua).ok())
        }
        _ => Ok(None),
    }
}

/// S_EVENT_REFUELING / S_EVENT_REFUELING_STOP. The initiator is kept as an
/// `Option` so a refuelling event for a unit that has just been deleted
/// degrades to `None` instead of failing the whole event.
#[derive(Debug, Clone, Serialize)]
pub struct Refueling<'lua> {
    pub time: Time,
    /// NB: on a dedicated server, when the receiver is a client, DCS reports
    /// the TANKER as the initiator of REFUELING and fires REFUELING_STOP
    /// twice (once for the tanker, once for the receiver). Open ED bug since
    /// 2.8.6; resolve the receiver by proximity when the initiator has the
    /// "Tankers" attribute.
    pub initiator: Option<Object<'lua>>,
}

impl<'lua> FromLua<'lua> for Refueling<'lua> {
    fn from_lua(value: Value<'lua>, lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("Refueling", None, value).map_err(lua_err)?;
        Ok(Self { time: tbl.raw_get("time")?, initiator: opt_object(&tbl, "initiator", lua)? })
    }
}

/// S_EVENT_LANDING_QUALITY_MARK: the Supercarrier LSO's grade for a pass.
///
/// `comment` is the LSO's text, e.g. `"LSO: GRADE:_OK_ : WIRE# 3"` or
/// `"LSO: GRADE:C _SLOX_ _LURX_ 3PTSIW WIRE #1"`. Only the Supercarrier (and
/// the Tarawa, which shares its LSO) produces it, and not for every pass.
#[derive(Debug, Clone, Serialize)]
pub struct LandingQualityMark<'lua> {
    pub time: Time,
    pub initiator: Option<Object<'lua>>,
    /// The carrier the pass was flown to.
    pub place: Option<Object<'lua>>,
    pub comment: Option<String>,
}

impl<'lua> FromLua<'lua> for LandingQualityMark<'lua> {
    fn from_lua(value: Value<'lua>, lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("LandingQualityMark", None, value).map_err(lua_err)?;
        Ok(Self {
            time: tbl.raw_get("time")?,
            initiator: opt_object(&tbl, "initiator", lua)?,
            place: opt_object(&tbl, "place", lua)?,
            comment: tbl
                .raw_get::<_, Option<String>>("comment")?
                .filter(|s| !s.as_str().is_empty()),
        })
    }
}

/// S_EVENT_WEAPON_DROP: fires on jettison, for a narrow set of weapons only.
#[derive(Debug, Clone, Serialize)]
pub struct WeaponDrop<'lua> {
    pub time: Time,
    pub initiator: Option<Object<'lua>>,
    pub weapon_name: Option<String>,
}

impl<'lua> FromLua<'lua> for WeaponDrop<'lua> {
    fn from_lua(value: Value<'lua>, lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("WeaponDrop", None, value).map_err(lua_err)?;
        Ok(Self {
            time: tbl.raw_get("time")?,
            initiator: opt_object(&tbl, "initiator", lua)?,
            weapon_name: opt_weapon_name(&tbl)?,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct LeaveUnit {
    pub initiator: Option<DcsOid<ClassUnit>>,
}

impl<'lua> FromLua<'lua> for LeaveUnit {
    fn from_lua(value: Value<'lua>, _: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("LeaveUnit", None, value).map_err(lua_err)?;
        let tbl: Option<LuaTable> = tbl.raw_get("initiator")?;
        let initiator = tbl
            .map(|tbl| {
                Ok::<_, mlua::Error>(DcsOid {
                    id: tbl.raw_get("id_")?,
                    class: "Unit".into(),
                    t: PhantomData,
                })
            })
            .transpose()?;
        Ok(Self { initiator })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct UnitEvent<'lua> {
    pub time: Time,
    pub initiator: Option<Object<'lua>>,
}

impl<'lua> FromLua<'lua> for UnitEvent<'lua> {
    fn from_lua(value: Value<'lua>, lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("UnitEvent", None, value).map_err(lua_err)?;
        Ok(Self { time: tbl.raw_get("time")?, initiator: opt_object(&tbl, "initiator", lua)? })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct EjectionEvent<'lua> {
    pub time: Time,
    pub initiator: Object<'lua>,
    pub target: Object<'lua>,
}

impl<'lua> FromLua<'lua> for EjectionEvent<'lua> {
    fn from_lua(value: Value<'lua>, _: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("EjectionEvent", None, value).map_err(lua_err)?;
        Ok(Self {
            time: tbl.raw_get("time")?,
            initiator: tbl.raw_get("initiator")?,
            target: tbl.raw_get("target")?,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Birth<'lua> {
    pub time: Time,
    pub initiator: Object<'lua>,
    pub place: Option<Object<'lua>>,
    pub subplace: Option<i64>,
}

impl<'lua> FromLua<'lua> for Birth<'lua> {
    fn from_lua(value: Value<'lua>, _: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("AtPlace", None, value).map_err(lua_err)?;
        Ok(Self {
            time: tbl.raw_get("time")?,
            initiator: tbl.raw_get("initiator")?,
            place: tbl.raw_get("place")?,
            subplace: tbl.raw_get("subPlace")?,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct AtPlace<'lua> {
    pub time: Time,
    pub initiator: Object<'lua>,
    pub place: Option<Object<'lua>>,
    pub subplace: Option<i64>,
}

impl<'lua> FromLua<'lua> for AtPlace<'lua> {
    fn from_lua(value: Value<'lua>, _: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("AtPlace", None, value).map_err(lua_err)?;
        Ok(Self {
            time: tbl.raw_get("time")?,
            initiator: tbl.raw_get("initiator")?,
            place: tbl.raw_get("place")?,
            subplace: tbl.raw_get("subPlace")?,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct WeaponAdd<'lua> {
    pub time: Time,
    pub initiator: Object<'lua>,
    pub weapon_name: Option<String>,
}

impl<'lua> FromLua<'lua> for WeaponAdd<'lua> {
    fn from_lua(value: Value<'lua>, _lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = as_tbl("WeaponAdd", None, value).map_err(lua_err)?;
        Ok(Self {
            time: tbl.raw_get("time")?,
            initiator: tbl.raw_get("initiator")?,
            weapon_name: opt_weapon_name(&tbl)?,
        })
    }
}

/// This is a dcs event
#[derive(Debug, Clone, Serialize)]
pub enum Event<'lua> {
    Invalid,
    Shot(Shot<'lua>),
    Hit(WeaponUse<'lua>),
    Takeoff(AtPlace<'lua>),
    Land(AtPlace<'lua>),
    Crash(UnitEvent<'lua>),
    Ejection(EjectionEvent<'lua>),
    Refueling(Refueling<'lua>),
    Dead(UnitEvent<'lua>),
    PilotDead(UnitEvent<'lua>),
    BaseCaptured,
    MissionStart,
    MissionEnd,
    TookControl,
    RefuelingStop(Refueling<'lua>),
    Birth(Birth<'lua>),
    HumanFailure,
    DetailedFailure,
    EngineStartup(AtPlace<'lua>),
    EngineShutdown(AtPlace<'lua>),
    PlayerEnterUnit(UnitEvent<'lua>),
    PlayerLeaveUnit(LeaveUnit),
    PlayerComment,
    ShootingStart(WeaponUse<'lua>),
    ShootingEnd(ShootingEnd<'lua>),
    MarkAdded(MarkPanel<'lua>),
    MarkChange(MarkPanel<'lua>),
    MarkRemoved(MarkPanel<'lua>),
    Kill(WeaponUse<'lua>),
    Score(UnitEvent<'lua>),
    UnitLost(UnitEvent<'lua>),
    LandingAfterEjection,
    ParatrooperLanding,
    DiscardChairAfterEjection,
    WeaponAdd(WeaponAdd<'lua>),
    TriggerZone,
    LandingQualityMark(LandingQualityMark<'lua>),
    Bda,
    AiAbortMission(UnitEvent<'lua>),
    DayNight,
    FlightTime,
    PlayerSelfKillPilot,
    PlayerCaptureAirfield,
    EmergencyLanding,
    UnitCreateTask,
    UnitDeleteTask,
    SimulationStart,
    WeaponRearm,
    WeaponDrop(WeaponDrop<'lua>),
    UnitTaskComplete,
    UnitTaskStage,
    MacExtraScore,
    MissionRestart,
    MissionWinner,
    /// S_EVENT_RUNWAY_TAKEOFF (2.9.6+): wheels leave a runway, FARP or deck.
    /// On a carrier, a bolter is RunwayTouch followed by RunwayTakeoff.
    RunwayTakeoff(AtPlace<'lua>),
    /// S_EVENT_RUNWAY_TOUCH (2.9.6+): wheels touch a runway, FARP or deck.
    RunwayTouch(AtPlace<'lua>),
    MacLmsRestart,
    SimulationFreeze,
    SimulationUnfreeze,
    HumanAircraftRepairStart,
    HumanAircraftRepairFinish,
    GroupChangeOption,
    Max,
}

fn translate<'a, 'lua: 'a>(
    lua: &'lua Lua,
    id: i64,
    value: Value<'lua>,
) -> Result<Event<'lua>> {
    Ok(match id {
        0 => Event::Invalid,
        1 => Event::Shot(Shot::from_lua(value, lua)?),
        2 => Event::Hit(WeaponUse::from_lua(value, lua)?),
        3 => Event::Takeoff(AtPlace::from_lua(value, lua)?),
        4 => Event::Land(AtPlace::from_lua(value, lua)?),
        5 => Event::Crash(UnitEvent::from_lua(value, lua)?),
        6 => Event::Ejection(EjectionEvent::from_lua(value, lua)?),
        7 => Event::Refueling(Refueling::from_lua(value, lua)?),
        8 => Event::Dead(UnitEvent::from_lua(value, lua)?),
        9 => Event::PilotDead(UnitEvent::from_lua(value, lua)?),
        10 => Event::BaseCaptured,
        11 => Event::MissionStart,
        12 => Event::MissionEnd,
        13 => Event::TookControl,
        14 => Event::RefuelingStop(Refueling::from_lua(value, lua)?),
        15 => Event::Birth(Birth::from_lua(value, lua)?),
        16 => Event::HumanFailure,
        17 => Event::DetailedFailure,
        18 => Event::EngineStartup(AtPlace::from_lua(value, lua)?),
        19 => Event::EngineShutdown(AtPlace::from_lua(value, lua)?),
        20 => Event::PlayerEnterUnit(UnitEvent::from_lua(value, lua)?),
        21 => Event::PlayerLeaveUnit(LeaveUnit::from_lua(value, lua)?),
        22 => Event::PlayerComment,
        23 => Event::ShootingStart(WeaponUse::from_lua(value, lua)?),
        24 => Event::ShootingEnd(ShootingEnd::from_lua(value, lua)?),
        25 => Event::MarkAdded(MarkPanel::from_lua(value, lua)?),
        26 => Event::MarkChange(MarkPanel::from_lua(value, lua)?),
        27 => Event::MarkRemoved(MarkPanel::from_lua(value, lua)?),
        28 => Event::Kill(WeaponUse::from_lua(value, lua)?),
        29 => Event::Score(UnitEvent::from_lua(value, lua)?),
        30 => Event::UnitLost(UnitEvent::from_lua(value, lua)?),
        31 => Event::LandingAfterEjection,
        32 => Event::ParatrooperLanding,
        33 => Event::DiscardChairAfterEjection,
        34 => Event::WeaponAdd(WeaponAdd::from_lua(value, lua)?),
        35 => Event::TriggerZone,
        36 => Event::LandingQualityMark(LandingQualityMark::from_lua(value, lua)?),
        37 => Event::Bda,
        38 => Event::AiAbortMission(UnitEvent::from_lua(value, lua)?),
        39 => Event::DayNight,
        40 => Event::FlightTime,
        41 => Event::PlayerSelfKillPilot,
        42 => Event::PlayerCaptureAirfield,
        43 => Event::EmergencyLanding,
        44 => Event::UnitCreateTask,
        45 => Event::UnitDeleteTask,
        46 => Event::SimulationStart,
        47 => Event::WeaponRearm,
        48 => Event::WeaponDrop(WeaponDrop::from_lua(value, lua)?),
        // Ids 49+ as reported by the live server's own `world.event` table
        // (bflib `log_event_ids`, DCS 2.9.29). An older table had these
        // shifted by one from 51 up, which decoded RUNWAY_TOUCH as a
        // "postponed takeoff" and dropped RUNWAY_TAKEOFF as "mission winner".
        49 => Event::UnitTaskComplete,
        50 => Event::UnitTaskStage,
        51 => Event::MacExtraScore,
        52 => Event::MissionRestart,
        53 => Event::MissionWinner,
        54 => Event::RunwayTakeoff(AtPlace::from_lua(value, lua)?),
        55 => Event::RunwayTouch(AtPlace::from_lua(value, lua)?),
        56 => Event::MacLmsRestart,
        57 => Event::SimulationFreeze,
        58 => Event::SimulationUnfreeze,
        59 => Event::HumanAircraftRepairStart,
        60 => Event::HumanAircraftRepairFinish,
        61 => Event::GroupChangeOption,
        62 => Event::Max,
        // DCS appends new event ids in updates. Rather than erroring every
        // time it adds one we don't know about yet, ignore it; bflib's
        // `log_event_ids` prints the live table at mission start so a new id
        // can be added here from the log.
        n => {
            debug!("ignoring unknown DCS event id {n}: {}", value_to_json(&value));
            Event::Invalid
        }
    })
}

impl<'lua> FromLua<'lua> for Event<'lua> {
    fn from_lua(value: Value<'lua>, lua: &'lua Lua) -> LuaResult<Self> {
        let id = as_tbl_ref("Event", &value).map_err(lua_err)?.raw_get("id")?;
        match translate(lua, id, value.clone()) {
            Ok(ev) => Ok(ev),
            Err(e) => {
                // The world event handler (world.rs) already logs a WARN for
                // this and skips the event -- keep the detailed payload at
                // debug level so a genuine translation bug is still diagnosable
                // without double-logging every dead-object edge case.
                let s = value_to_json(&value);
                debug!("error translating event {id}: {e:?}, value: {s}");
                Err(lua_err(e))
            }
        }
    }
}
