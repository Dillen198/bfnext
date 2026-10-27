/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use crate::simple_enum_unknown;
use super::{
    airbase::Airbase,
    as_tbl,
    country::Country, env,
    group::{Group, GroupCategory},
    static_object::StaticObject,
    unit::Unit,
};
use crate::{record_perf, wrapped_table, LuaEnv, MizLua, Sequence};
use anyhow::{anyhow, Result};
use mlua::{prelude::*, Value};
use serde_derive::Serialize;
use std::ops::Deref;

pub use dcso3_core::coalition::Side;

#[derive(Debug, Clone)]
pub enum Static {
    Airbase(Airbase),
    Static(StaticObject),
}

simple_enum_unknown!(Service, u8, [Atc => 0, Awacs => 1, Fac => 3, Tanker => 2]);
wrapped_table!(Coalition, None);

impl Coalition {
    pub fn singleton(lua: MizLua) -> Result<Self> {
        Ok(Self {
            t: lua.inner().globals().raw_get("coalition")?,
            lua: lua.inner().clone(),
        })
    }

    pub fn add_group(
        &self,
        country: Country,
        category: GroupCategory,
        data: env::miz::Group,
    ) -> Result<Group> {
        Ok(record_perf!(
            add_group,
            self.t
                .call_function("addGroup", (country, category, data))?
        ))
    }

    pub fn add_static_object(
        &self,
        country: Country,
        data: env::miz::Unit,
    ) -> Result<Static> {
        let tbl: LuaTable = record_perf!(
            add_static_object,
            self.t.call_function("addStaticObject", (country, data))?
        );
        let mt = tbl
            .metatable()
            .ok_or_else(|| anyhow!("returned static object has no meta table"))?;
        if mt.raw_get::<String>("className_")?.as_str() == "Airbase" {
            Ok(Static::Airbase(Airbase::from_lua(
                Value::Table(tbl),
                &self.lua,
            )?))
        } else {
            Ok(Static::Static(StaticObject::from_lua(
                Value::Table(tbl),
                &self.lua,
            )?))
        }
    }

    pub fn get_groups(&self, side: Side) -> Result<Sequence<Group>> {
        Ok(self.t.call_function("getGroups", side)?)
    }

    pub fn get_static_objects(&self, side: Side) -> Result<Sequence<StaticObject>> {
        Ok(self.t.call_function("getStaticObjects", side)?)
    }

    pub fn get_airbases(&self, side: Side) -> Result<Sequence<Airbase>> {
        Ok(self.t.call_function("getAirbases", side)?)
    }

    pub fn get_players(&self, side: Side) -> Result<Sequence<Unit>> {
        Ok(self.t.call_function("getPlayers", side)?)
    }

    pub fn get_service_providers(
        &self,
        side: Side,
        service: Service,
    ) -> Result<Sequence<Unit>> {
        Ok(self
            .t
            .call_function("getServiceProviders", (side, service))?)
    }

    pub fn get_country_coalition(&self, country: Country) -> Result<Side> {
        Ok(self.t.call_function("getCountryCoalition", country)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn service_ids_round_trip_and_tolerate_new_ones() {
        let lua = Lua::new();
        for (id, s) in [(0, Service::Atc), (1, Service::Awacs), (2, Service::Tanker), (3, Service::Fac)] {
            assert_eq!(Service::from_lua(Value::Integer(id), &lua).unwrap(), s);
            assert!(matches!(s.into_lua(&lua).unwrap(), Value::Integer(i) if i == id));
        }
        let new = Service::from_lua(Value::Integer(9), &lua).unwrap();
        assert_eq!(new, Service::Unknown(9));
        assert!(matches!(new.into_lua(&lua).unwrap(), Value::Integer(9)));
        // ordering follows the DCS id, as the old #[repr] derive did
        assert!(Service::Tanker < Service::Fac);
        // Side is still strict
        assert!(Side::from_lua(Value::Integer(7), &lua).is_err());
    }
}
