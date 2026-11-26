/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use super::{as_tbl, String};
use crate::{lua_err, wrapped_table, LuaEnv, LuaVec3};
use anyhow::Result;
use mlua::{prelude::*, Value};
use serde_derive::{Deserialize, Serialize};
use std::ops::Deref;

pub use dcso3_core::coord::LLPos;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MGRSPos {
    pub utm_zone: String,
    pub mgrs_digraph: String,
    pub easting: f64,
    pub northing: f64,
}

impl FromLua for MGRSPos {
    fn from_lua(value: Value, _lua: &Lua) -> LuaResult<Self> {
        let tbl = as_tbl("MGRSPos", None, value).map_err(lua_err)?;
        Ok(MGRSPos {
            utm_zone: tbl.raw_get("UTMZone")?,
            mgrs_digraph: tbl.raw_get("MGRSDigraph")?,
            easting: tbl.raw_get("Easting")?,
            northing: tbl.raw_get("Northing")?,
        })
    }
}

impl IntoLua for MGRSPos {
    fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
        let tbl = lua.create_table()?;
        tbl.raw_set("UTMZone", self.utm_zone)?;
        tbl.raw_set("MGRSDigraph", self.mgrs_digraph)?;
        tbl.raw_set("Easting", self.easting)?;
        tbl.raw_set("Northing", self.northing)?;
        Ok(Value::Table(tbl))
    }
}

wrapped_table!(Coord, None);

impl Coord {
    pub fn singleton<'lua, L: LuaEnv<'lua>>(lua: L) -> Result<Self> {
        Ok(lua.inner().globals().raw_get("coord")?)
    }

    pub fn ll_to_lo(&self, pos: LLPos) -> Result<LuaVec3> {
        Ok(self.t.call_function("LLtoLO", (pos.latitude, pos.longitude, pos.altitude))?)
    }

    pub fn lo_to_ll(&self, pos: LuaVec3) -> Result<LLPos> {
        let (latitude, longitude, altitude) = self.t.call_function("LOtoLL", pos)?;
        Ok(LLPos { latitude, longitude, altitude })
    }

    pub fn ll_to_mgrs(&self, latitude: f64, longitude: f64) -> Result<MGRSPos> {
        Ok(self.t.call_function("LLtoMGRS", (latitude, longitude))?)
    }

    pub fn mgrs_to_ll(&self, mgrs: MGRSPos) -> Result<LLPos> {
        let (latitude, longitude, altitude) = self.t.call_function("MGRStoLL", mgrs)?;
        Ok(LLPos { latitude, longitude, altitude })
    }
}
