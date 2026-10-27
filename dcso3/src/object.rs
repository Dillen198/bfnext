/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use super::{as_tbl, unit::Unit, weapon::Weapon, LuaVec3, Position3, String};
use crate::{
    check_implements, record_perf, simple_enum_unknown, static_object::StaticObject, wrapped_table, LuaEnv,
    MizLua,
};
use anyhow::{anyhow, bail, Result};
use core::fmt;
use mlua::{prelude::*, Value};
use serde_derive::Serialize;
use std::{hash::Hash, marker::PhantomData, ops::Deref};

pub use dcso3_core::object::{ClassObject, DcsOid};

/// `DcsOid` methods that need the Lua state. A trait because `DcsOid` now
/// lives in dcso3-core, which knows nothing about `MizLua`.
pub trait DcsOidExt {
    fn check_implements(&self, lua: MizLua, class: &str) -> Result<()>;
}

impl<T> DcsOidExt for DcsOid<T> {
    fn check_implements(&self, lua: MizLua, class: &str) -> Result<()> {
        let m = lua.inner().globals().raw_get(&**self.class)?;
        if !check_implements(&m, class) {
            bail!("{:?} is does not implement {class}", self)
        }
        Ok(())
    }
}

pub trait DcsObject: Sized + Deref<Target = mlua::Table> {
    type Class: fmt::Debug + Clone;

    fn object_id(&self) -> Result<DcsOid<Self::Class>> {
        let id = self.raw_get("id_")?;
        let m = self
            .metatable()
            .ok_or_else(|| anyhow!("object with no metatable"))?;
        let class = m.raw_get("className_")?;
        Ok(DcsOid {
            id,
            class,
            t: PhantomData,
        })
    }

    fn change_instance(self, id: &DcsOid<Self::Class>) -> Result<Self>;
    fn change_instance_dyn<T>(self, id: &DcsOid<T>) -> Result<Self>;
    fn get_instance(lua: MizLua, id: &DcsOid<Self::Class>) -> Result<Self>;
    fn get_instance_dyn<T>(lua: MizLua, id: &DcsOid<T>) -> Result<Self>;
}

simple_enum_unknown!(ObjectCategory, u8, [
    Void => 0,
    Unit => 1,
    Weapon => 2,
    Static => 3,
    Base => 4,
    Scenery => 5,
    Cargo => 6
]);

wrapped_table!(Object, Some("Object"));

impl Object {
    pub fn destroy(self) -> Result<()> {
        Ok(self.t.call_method("destroy", ())?)
    }

    pub fn get_category(&self) -> Result<ObjectCategory> {
        Ok(self.t.call_method("getCategory", ())?)
    }

    pub fn get_desc(&self) -> Result<mlua::Table> {
        Ok(self.t.call_method("getDesc", ())?)
    }

    pub fn has_attribute(&self, attr: String) -> Result<bool> {
        Ok(self.t.call_method("hasAttribute", attr)?)
    }

    pub fn get_name(&self) -> Result<String> {
        Ok(self.t.call_method("getName", ())?)
    }

    pub fn get_type_name(&self) -> Result<String> {
        Ok(self.t.call_method("getTypeName", ())?)
    }

    pub fn get_point(&self) -> Result<LuaVec3> {
        Ok(record_perf!(get_point, self.t.call_method("getPoint", ())?))
    }

    pub fn get_position(&self) -> Result<Position3> {
        Ok(record_perf!(
            get_position,
            self.t.call_method("getPosition", ())?
        ))
    }

    /// Teleport this object to the given Position3.
    /// Uses DCS Object.setPosition() which immediately moves the object.
    pub fn set_position(&self, pos: Position3) -> Result<()> {
        Ok(self.t.call_method("setPosition", pos)?)
    }

    pub fn get_velocity(&self) -> Result<LuaVec3> {
        Ok(record_perf!(
            get_velocity,
            self.t.call_method("getVelocity", ())?
        ))
    }

    pub fn in_air(&self) -> Result<bool> {
        Ok(self.t.call_method("inAir", ())?)
    }

    /// Coalition of a CoalitionObject (unit, static, weapon, airbase). Errors
    /// for a scenery object, which has none.
    pub fn get_coalition(&self) -> Result<crate::coalition::Side> {
        Ok(self.t.call_method("getCoalition", ())?)
    }

    pub fn is_exist(&self) -> Result<bool> {
        Ok(self.t.call_method("isExist", ())?)
    }

    pub fn as_unit(&self) -> Result<Unit> {
        Ok(Unit::from_lua(Value::Table(self.t.clone()), &self.lua)?)
    }

    pub fn as_weapon(&self) -> Result<Weapon> {
        Ok(Weapon::from_lua(Value::Table(self.t.clone()), &self.lua)?)
    }

    pub fn as_static(&self) -> Result<StaticObject> {
        Ok(StaticObject::from_lua(
            Value::Table(self.t.clone()),
            &self.lua,
        )?)
    }
}

impl DcsObject for Object {
    type Class = ClassObject;

    fn get_instance(lua: MizLua, id: &DcsOid<Self::Class>) -> Result<Self> {
        let t = lua.inner().create_table()?;
        t.set_metatable(Some(lua.inner().globals().raw_get(&**id.class)?));
        t.raw_set("id_", id.id)?;
        let t = Object {
            t,
            lua: lua.inner().clone(),
        };
        if !t.is_exist()? {
            bail!("{} is an invalid object", id.id)
        }
        Ok(t)
    }

    fn get_instance_dyn<T>(lua: MizLua, id: &DcsOid<T>) -> Result<Self> {
        id.check_implements(lua, "Object")?;
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
            bail!("{} is an invalid object", id.id)
        }
        Ok(self)
    }

    fn change_instance_dyn<T>(self, id: &DcsOid<T>) -> Result<Self> {
        id.check_implements(MizLua(&self.lua), "Object")?;
        self.t.raw_set("id_", id.id)?;
        if !self.is_exist()? {
            bail!("{} is an invalid object", id.id)
        }
        Ok(self)
    }
}
