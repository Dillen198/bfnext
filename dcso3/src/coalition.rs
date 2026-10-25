/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use super::{
    airbase::Airbase,
    as_tbl,
    country::Country, env,
    group::{Group, GroupCategory},
    static_object::StaticObject,
    unit::Unit,
};
use crate::{record_perf, simple_enum, wrapped_table, LuaEnv, MizLua, Sequence};
use anyhow::{anyhow, Result};
use mlua::{prelude::*, Value};
use serde_derive::Serialize;
use std::ops::Deref;

pub use dcso3_core::coalition::Side;

#[derive(Debug, Clone)]
pub enum Static<'lua> {
    Airbase(Airbase<'lua>),
    Static(StaticObject<'lua>),
}

simple_enum!(Service, u8, [Atc => 0, Awacs => 1, Fac => 3, Tanker => 2]);
wrapped_table!(Coalition, None);

impl<'lua> Coalition<'lua> {
    pub fn singleton(lua: MizLua<'lua>) -> Result<Self> {
        Ok(Self {
            t: lua.inner().globals().raw_get("coalition")?,
            lua: lua.inner(),
        })
    }

    pub fn add_group(
        &self,
        country: Country,
        category: GroupCategory,
        data: env::miz::Group<'lua>,
    ) -> Result<Group<'lua>> {
        Ok(record_perf!(
            add_group,
            self.t
                .call_function("addGroup", (country, category, data))?
        ))
    }

    pub fn add_static_object(
        &self,
        country: Country,
        data: env::miz::Unit<'lua>,
    ) -> Result<Static<'lua>> {
        let tbl: LuaTable = record_perf!(
            add_static_object,
            self.t.call_function("addStaticObject", (country, data))?
        );
        let mt = tbl
            .get_metatable()
            .ok_or_else(|| anyhow!("returned static object has no meta table"))?;
        if mt.raw_get::<_, String>("className_")?.as_str() == "Airbase" {
            Ok(Static::Airbase(Airbase::from_lua(
                Value::Table(tbl),
                self.lua,
            )?))
        } else {
            Ok(Static::Static(StaticObject::from_lua(
                Value::Table(tbl),
                self.lua,
            )?))
        }
    }

    pub fn get_groups(&self, side: Side) -> Result<Sequence<'lua, Group<'lua>>> {
        Ok(self.t.call_function("getGroups", side)?)
    }

    pub fn get_static_objects(&self, side: Side) -> Result<Sequence<'lua, StaticObject<'lua>>> {
        Ok(self.t.call_function("getStaticObjects", side)?)
    }

    pub fn get_airbases(&self, side: Side) -> Result<Sequence<'lua, Airbase<'lua>>> {
        Ok(self.t.call_function("getAirbases", side)?)
    }

    pub fn get_players(&self, side: Side) -> Result<Sequence<'lua, Unit<'lua>>> {
        Ok(self.t.call_function("getPlayers", side)?)
    }

    pub fn get_service_providers(
        &self,
        side: Side,
        service: Service,
    ) -> Result<Sequence<'lua, Unit<'lua>>> {
        Ok(self
            .t
            .call_function("getServiceProviders", (side, service))?)
    }

    pub fn get_country_coalition(&self, country: Country) -> Result<Side> {
        Ok(self.t.call_function("getCountryCoalition", country)?)
    }
}
