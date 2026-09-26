/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use super::{as_tbl, LuaVec3};
use crate::{wrapped_table, LuaEnv, MizLua};
use anyhow::Result;
use mlua::{prelude::*, Value};
use serde_derive::Serialize;
use std::ops::Deref;

wrapped_table!(Atmosphere, None);

/// Temperature and pressure at a point, as `atmosphere.getTemperatureAndPressure`
/// returns them.
#[derive(Debug, Clone, Copy, Serialize)]
pub struct TempPressure {
    /// Kelvin
    pub temperature_k: f64,
    /// Pascals
    pub pressure_pa: f64,
}

impl<'lua> Atmosphere<'lua> {
    pub fn singleton(lua: MizLua<'lua>) -> Result<Self> {
        Ok(lua.inner().globals().raw_get("atmosphere")?)
    }

    /// The wind at `p` as a velocity vector in m/s, in the DCS world frame
    /// (x = north, z = east). It is the direction the air moves TOWARD; the
    /// meteorological "wind from" bearing is `atan2(-z, -x)`.
    pub fn get_wind(&self, p: LuaVec3) -> Result<LuaVec3> {
        Ok(self.t.call_function("getWind", p)?)
    }

    /// As `get_wind`, plus the mission's turbulence at that instant.
    pub fn get_wind_with_turbulence(&self, p: LuaVec3) -> Result<LuaVec3> {
        Ok(self.t.call_function("getWindWithTurbulence", p)?)
    }

    pub fn get_temperature_and_pressure(&self, p: LuaVec3) -> Result<TempPressure> {
        let (temperature_k, pressure_pa): (f64, f64) =
            self.t.call_function("getTemperatureAndPressure", p)?;
        Ok(TempPressure { temperature_k, pressure_pa })
    }
}
