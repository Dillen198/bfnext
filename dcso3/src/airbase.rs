/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use super::{as_tbl, coalition::Side, object::Object, warehouse::Warehouse, LuaVec3, String};
use crate::{
    object::{DcsObject, DcsOid},
    wrapped_table, LuaEnv, MizLua, Sequence, wrapped_prim,
};
use anyhow::{bail, Result};
use mlua::{prelude::*, Value};
use serde_derive::{Serialize, Deserialize};
use std::{marker::PhantomData, ops::Deref};

wrapped_prim!(RunwayId, i64, Hash, Copy);
wrapped_prim!(AirbaseId, i64, Hash, Copy);

wrapped_table!(Runway, None);

impl<'lua> Runway<'lua> {
    pub fn id(&self) -> Result<RunwayId> {
        Ok(self.t.raw_get("Name")?)
    }

    /// The runway's designation as DCS reports it, e.g. "09-27", "13L-31R" or a
    /// bare "22". DCS has stored this as either a string or a number across
    /// versions, so accept both.
    pub fn name(&self) -> Result<String> {
        match self.t.raw_get::<_, Value>("Name")? {
            Value::String(s) => Ok(String::from(s.to_str()?)),
            Value::Integer(i) => Ok(String::from(format!("{:02}", i))),
            Value::Number(n) => Ok(String::from(format!("{:02}", n as i64))),
            _ => bail!("runway has no usable Name field"),
        }
    }

    pub fn course(&self) -> Result<f64> {
        Ok(self.t.raw_get("course")?)
    }

    pub fn position(&self) -> Result<LuaVec3> {
        Ok(self.t.raw_get("position")?)
    }

    pub fn length(&self) -> Result<f64> {
        Ok(self.t.raw_get("length")?)
    }

    pub fn width(&self) -> Result<f64> {
        Ok(self.t.raw_get("width")?)
    }
}

/// `Airbase.Category` -- what kind of base this is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AirbaseCategory {
    Airdrome,
    Helipad,
    Ship,
}

impl<'lua> FromLua<'lua> for AirbaseCategory {
    fn from_lua(value: Value<'lua>, _lua: &'lua Lua) -> LuaResult<Self> {
        match u32::from_lua(value, _lua)? {
            0 => Ok(Self::Airdrome),
            1 => Ok(Self::Helipad),
            2 => Ok(Self::Ship),
            n => Err(crate::lua_err(&anyhow::anyhow!(
                "unknown airbase category {n}"
            ))),
        }
    }
}

wrapped_table!(Parking, None);

/// DCS parking-spot terminal types, as reported in `Term_Type` by
/// `Airbase.getParking`. These are bit-ish category codes, not a dense enum --
/// DCS hands back exactly one of these per spot.
pub mod term_type {
    /// A runway "spot" -- a takeoff position on the runway itself, never a
    /// parking place. Must be excluded from parking starts.
    pub const RUNWAY: i64 = 16;
    /// Helipad. Fixed wing can't use it.
    pub const HELICOPTER_ONLY: i64 = 40;
    /// Hardened aircraft shelter. Fighter-sized only.
    pub const SHELTER: i64 = 68;
    /// Medium open ramp spot. Fighters and helicopters.
    pub const OPEN_MED: i64 = 72;
    /// Large open ramp spot. Anything, including heavies.
    pub const OPEN_BIG: i64 = 104;
}

/// One parking spot as reported by `Airbase.getParking(available)`.
///
/// Field names mirror DCS's: `Term_Index` is what goes in a unit's `parking`
/// field, and `vTerminalPos` is where that unit has to be placed for DCS to
/// accept the parking start.
#[derive(Debug, Clone, Copy)]
pub struct ParkingSpot {
    pub term_index: i64,
    pub term_type: i64,
    pub pos: LuaVec3,
    pub dist_to_rw: f64,
    /// Whether the spot can be used as a takeoff position at all. Spots with
    /// this false exist (maintenance areas, some FARP pads) and DCS will
    /// silently air-start a group assigned to one.
    pub to_ac: bool,
}

impl ParkingSpot {
    /// How much this spot is preferred for the given airframe, lower first.
    ///
    /// Only meaningful for helicopters, which should take a dedicated pad
    /// before they take a fixed-wing ramp spot -- an airfield's pads are
    /// usually clear of the taxi routes the jets need, and parking a helo on
    /// an open-big stand wastes the only spot a heavy can use.
    pub fn preference(&self, helicopter: bool) -> u8 {
        if helicopter && self.term_type == term_type::HELICOPTER_ONLY {
            0
        } else {
            1
        }
    }

    /// Can an aircraft of this kind actually start from this spot?
    ///
    /// Mirrors the terminal-type matching every working DCS spawner does
    /// (MOOSE's `AIRBASE.TerminalType` / `_CheckTerminalType`): fixed wing take
    /// shelters and open medium/big ramp, helicopters take helipads and open
    /// ramp, and nobody parks on the runway.
    pub fn usable_by(&self, helicopter: bool) -> bool {
        if !self.to_ac || self.term_type == term_type::RUNWAY {
            return false;
        }
        if helicopter {
            matches!(
                self.term_type,
                term_type::HELICOPTER_ONLY | term_type::OPEN_MED | term_type::OPEN_BIG
            )
        } else {
            matches!(
                self.term_type,
                term_type::SHELTER | term_type::OPEN_MED | term_type::OPEN_BIG
            )
        }
    }
}

impl<'lua> FromLua<'lua> for ParkingSpot {
    fn from_lua(value: Value<'lua>, _lua: &'lua Lua) -> LuaResult<Self> {
        let tbl: LuaTable = FromLua::from_lua(value, _lua)?;
        Ok(Self {
            term_index: tbl.raw_get("Term_Index")?,
            term_type: tbl.raw_get("Term_Type")?,
            pos: tbl.raw_get("vTerminalPos")?,
            dist_to_rw: tbl.raw_get::<_, Option<f64>>("fDistToRW")?.unwrap_or(0.),
            // Absent on some spots/terrains -- absent means "not usable for
            // takeoff", which is the safe reading.
            to_ac: tbl.raw_get::<_, Option<bool>>("TO_AC")?.unwrap_or(false),
        })
    }
}

wrapped_table!(Airbase, Some("Airbase"));

impl<'lua> Airbase<'lua> {
    pub fn get_by_name(lua: MizLua<'lua>, name: String) -> Result<Self> {
        let globals = lua.inner().globals();
        let airbase: LuaTable = globals.raw_get("Airbase")?;
        Ok(airbase.call_function("getByName", name)?)
    }

    pub fn is_exist(&self) -> Result<bool> {
        Ok(self.t.call_method("isExist", ())?)
    }

    pub fn destroy(&self) -> Result<()> {
        Ok(self.t.call_method("destroy", ())?)
    }

    pub fn get_desc(&self) -> Result<mlua::Table<'lua>> {
        Ok(self.t.call_method("getDesc", ())?)
    }

    /// `Airbase.Category` of this base: airdrome, helipad (FARP) or ship.
    ///
    /// This is not `Object.getCategory` -- that reports every airbase as
    /// `BASE`. It decides which waypoint field a ground start has to use:
    /// airdromes take `airdromeId`, helipads and ships take `helipadId` plus
    /// `linkUnit`, and using the wrong one air-starts the flight.
    pub fn get_category(&self) -> Result<AirbaseCategory> {
        let desc = self.get_desc()?;
        Ok(desc.raw_get("category")?)
    }
    
    pub fn get_point(&self) -> Result<LuaVec3> {
        Ok(self.t.call_method("getPoint", ())?)
    }

    pub fn get_callsign(&self) -> Result<String> {
        Ok(self.t.call_method("getCallsign", ())?)
    }

    pub fn get_unit(&self, i: i64) -> Result<Object<'lua>> {
        Ok(self.t.call_method("getUnit", i)?)
    }

    pub fn get_id(&self) -> Result<AirbaseId> {
        // DCS names this `getID` (inherited from Object), like Unit/Group/
        // StaticObject. `getId` does not exist, so the call errored every time --
        // which is why CAP ground-start could never resolve an airdrome id and
        // every CAP flight air-started.
        Ok(self.t.call_method("getID", ())?)
    }

    pub fn get_parking(&self, available: bool) -> Result<Parking<'lua>> {
        Ok(self.t.call_method("getParking", available)?)
    }

    /// `Airbase.getParking` decoded into typed spots. Pass `available = true`
    /// to get only spots DCS currently considers free.
    ///
    /// The returned list is in DCS's order; callers that care about taxi
    /// distance should sort by `dist_to_rw` themselves.
    pub fn get_parking_spots(&self, available: bool) -> Result<Vec<ParkingSpot>> {
        let tbl: LuaTable = self.t.call_method("getParking", available)?;
        let mut spots = vec![];
        for pair in tbl.pairs::<Value, ParkingSpot>() {
            let (_, spot) = pair?;
            spots.push(spot);
        }
        Ok(spots)
    }

    pub fn get_runways(&self) -> Result<Sequence<'lua, Runway<'lua>>> {
        Ok(self.t.call_method("getRunways", ())?)
    }

    pub fn get_tech_object_pos(&self, obj: String) -> Result<LuaVec3> {
        Ok(self.t.call_method("getTechObjectPos", obj)?)
    }

    pub fn get_radio_silent_mode(&self) -> Result<bool> {
        Ok(self.t.call_method("getRadioSilentMode", ())?)
    }

    pub fn set_radio_silent_mode(&self, on: bool) -> Result<()> {
        Ok(self.t.call_method("setRadioSilentMode", on)?)
    }

    pub fn auto_capture(&self, on: bool) -> Result<()> {
        Ok(self.t.call_method("autoCapture", on)?)
    }

    pub fn auto_capture_is_on(&self) -> Result<bool> {
        Ok(self.t.call_method("autoCaptureIsOn", ())?)
    }

    pub fn set_coalition(&self, coa: Side) -> Result<()> {
        Ok(self.t.call_method("setCoalition", coa)?)
    }

    pub fn get_warehouse(&self) -> Result<Warehouse<'lua>> {
        Ok(self.t.call_method("getWarehouse", ())?)
    }

    pub fn as_object(&self) -> Result<Object<'lua>> {
        Ok(Object::from_lua(Value::Table(self.t.clone()), self.lua)?)
    }
}

#[derive(Debug, Clone)]
pub struct ClassAirbase;

impl<'lua> DcsObject<'lua> for Airbase<'lua> {
    type Class = ClassAirbase;

    fn get_instance(lua: MizLua<'lua>, id: &DcsOid<Self::Class>) -> Result<Self> {
        let t = lua.inner().create_table()?;
        t.set_metatable(Some(lua.inner().globals().raw_get(&**id.class)?));
        t.raw_set("id_", id.id)?;
        let t = Airbase {
            t,
            lua: lua.inner(),
        };
        if !t.is_exist()? {
            bail!("{} is an invalid airbase", id.id)
        }
        Ok(t)
    }

    fn get_instance_dyn<T>(lua: MizLua<'lua>, id: &DcsOid<T>) -> Result<Self> {
        id.check_implements(lua, "Airbase")?;
        let id = DcsOid {
            id: id.id,
            class: id.class.clone(),
            t: PhantomData,
        };
        Self::get_instance(lua, &id)
    }

    fn change_instance(self, id: &DcsOid<Self::Class>) -> Result<Self> {
        self.raw_set("id_", id.id)?;
        if !self.is_exist()? {
            bail!("{} is an invalid airbase", id.id)
        }
        Ok(self)
    }

    fn change_instance_dyn<T>(self, id: &DcsOid<T>) -> Result<Self> {
        id.check_implements(MizLua(self.lua), "Airbase")?;
        self.t.raw_set("id_", id.id)?;
        if !self.is_exist()? {
            bail!("{} is an invalid airbase", id.id)
        }
        Ok(self)
    }
}
