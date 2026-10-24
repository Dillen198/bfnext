/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use super::{as_tbl, object::Object, unit::Unit};
use crate::{cvt_err, object::{DcsObject, DcsOid}, simple_enum, wrapped_table, LuaEnv, LuaVec3, MizLua, Position3};
use anyhow::{bail, Result};
use mlua::{prelude::*, Value};
use serde::Deserialize;
use serde_derive::Serialize;
use std::{marker::PhantomData, ops::Deref};

// the documentation is unfortunately not sufficient for this to be a
// proper bitflags
simple_enum!(WeaponFlag, u64, [
    NoWeapon => 0,
    LGB => 2,
    TvGB => 4,
    SNSGB => 8,
    HEBomb => 16,
    Penetrator => 32,
    NapalmBomb => 64,
    FAEBomb => 128,
    ClusterBomb => 256,
    Dispenser => 512,
    CandleBomb => 1024,
    ParachuteBomb => 2147483648,
    GuidedBomb => 14,
    AnyUnguidedBomb => 2147485680,
    AnyBomb => 2147485694,
    LightRocket => 2048,
    MarkerRocket => 4096,
    CandleRocket => 8192,
    HeavyRocket => 16384,
    AnyRocket => 30720,
    AntiRadarMissile => 32768,
    AntiShipMissile => 65536,
    AntiTankMissile => 131072,
    FireAndForgetASM => 262144,
    LaserASM => 524288,
    TeleASM => 1048576,
    CruiseMissile => 2097152,
    GuidedASM => 1572864,
    TacticalASM => 1835008,
    AnyASM => 4161536,
    SRAAM => 4194304,
    MRAAM => 8388608,
    LRAAM => 16777216,
    IRAAM => 33554432,
    SARAAM => 67108864,
    ARAAM => 134217728,
    AnyAAM => 264241152,
    AnyMissile => 268402688,
    AnyAutonomousMissile => 36012032,
    GunPod => 268435456,
    BuiltInCannon => 536870912,
    Cannons => 805306368,
    AntiRadarMissile2 => 1073741824,
    SmokeShell => 17179869184,
    IlluminationShell => 34359738368,
    MarkerShell => 51539607552,
    SubmunitionDispenserShell => 68719476736,
    GuidedShell => 137438953472,
    ConventionalShell => 206963736576,
    AnyShell => 258503344128,
    Decoys => 8589934592,
    Torpedo => 4294967296,
    AnyAGWeapon => 2956984318,
    AnyAAWeapon => 1069547520,
    UnguidedWeapon => 2952822768,
    GuidedWeapon => 268402702,
    AnyWeapon => 3221225470,
    MarkerWeapon => 13312,
    ArmWeapon => 209379642366
]);

wrapped_table!(Weapon, Some("Weapon"));

impl<'lua> Weapon<'lua> {
    pub fn as_object(&self) -> Result<Object<'lua>> {
        Ok(Object::from_lua(Value::Table(self.t.clone()), self.lua)?)
    }

    pub fn is_exist(&self) -> Result<bool> {
        Ok(self.t.call_method("isExist", ())?)
    }

    pub fn get_name(&self) -> Result<String> {
        Ok(self.t.call_method("getName", ())?)
    }

    pub fn get_type(&self) -> Result<String> {
        Ok(self.t.call_method("getTypeName", ())?)
    }

    pub fn get_launcher(&self) -> Result<Unit<'lua>> {
        Ok(self.t.call_method("getLauncher", ())?)
    }

    pub fn get_target(&self) -> Result<Option<Object<'lua>>> {
        match self.t.call_method("getTarget", ())? {
            Value::Nil => Ok(None),
            v => Ok(Some(Object::from_lua(v, self.lua)?)),
        }
    }

    pub fn get_desc(&self) -> Result<mlua::Table<'lua>> {
        Ok(self.t.call_method("getDesc", ())?)
    }

    /// `getDesc()` with the fields a range scorer needs, typed.
    pub fn get_weapon_desc(&self) -> Result<WeaponDesc> {
        WeaponDesc::from_table(&self.get_desc()?)
    }

    pub fn get_point(&self) -> Result<LuaVec3> {
        Ok(self.t.call_method("getPoint", ())?)
    }

    pub fn get_position(&self) -> Result<Position3> {
        Ok(self.t.call_method("getPosition", ())?)
    }

    pub fn get_velocity(&self) -> Result<LuaVec3> {
        Ok(self.t.call_method("getVelocity", ())?)
    }

    pub fn get_coalition(&self) -> Result<crate::coalition::Side> {
        Ok(self.t.call_method("getCoalition", ())?)
    }

    /// Remove the weapon from the world without an explosion (the missile
    /// trainer's "splash").
    pub fn destroy(&self) -> Result<()> {
        Ok(self.t.call_method("destroy", ())?)
    }
}

simple_enum!(WeaponCategory, u8, [
    Shell => 0,
    Missile => 1,
    Rocket => 2,
    Bomb => 3,
    Torpedo => 4
]);

simple_enum!(GuidanceType, u8, [
    Ins => 1,
    IrHoming => 2,
    MmwRadar => 3,
    ActiveRadar => 4,
    SemiActiveRadar => 5,
    PassiveRadar => 6,
    Tv => 7,
    Laser => 8,
    Telecontrol => 9
]);

simple_enum!(MissileCategory, u8, [
    Aam => 1,
    Sam => 2,
    Bm => 3,
    AntiShip => 4,
    Cruise => 5,
    Other => 6
]);

/// The typed subset of `Weapon:getDesc()`. Fields DCS leaves out for a
/// category (a bomb has no `missileCategory`) are `None`.
#[derive(Debug, Clone, Serialize, Default)]
pub struct WeaponDesc {
    pub type_name: String,
    pub display_name: String,
    pub category: Option<WeaponCategory>,
    pub guidance: Option<GuidanceType>,
    pub missile_category: Option<MissileCategory>,
    /// explosive mass of the warhead, kg
    pub warhead_explosive_kg: Option<f64>,
    /// total warhead mass, kg
    pub warhead_mass_kg: Option<f64>,
    /// max range at max altitude, metres (missiles)
    pub range_max_m: Option<f64>,
    pub range_min_m: Option<f64>,
    pub alt_max_m: Option<f64>,
    /// fuze proximity distance, metres (missiles)
    pub fuse_dist_m: Option<f64>,
}

impl WeaponDesc {
    pub fn from_table(t: &mlua::Table) -> Result<Self> {
        let num = |k: &str| -> Option<f64> { t.raw_get::<_, Option<f64>>(k).ok().flatten() };
        let int = |k: &str| -> Option<i64> { t.raw_get::<_, Option<i64>>(k).ok().flatten() };
        let warhead: Option<mlua::Table> = t.raw_get("warhead").ok().flatten();
        let wnum = |k: &str| -> Option<f64> {
            warhead
                .as_ref()
                .and_then(|w| w.raw_get::<_, Option<f64>>(k).ok().flatten())
        };
        Ok(Self {
            type_name: t.raw_get::<_, Option<std::string::String>>("typeName")?.unwrap_or_default(),
            display_name: t.raw_get::<_, Option<std::string::String>>("displayName")?.unwrap_or_default(),
            category: int("category").and_then(|c| match c {
                0 => Some(WeaponCategory::Shell),
                1 => Some(WeaponCategory::Missile),
                2 => Some(WeaponCategory::Rocket),
                3 => Some(WeaponCategory::Bomb),
                4 => Some(WeaponCategory::Torpedo),
                _ => None,
            }),
            guidance: int("guidance").and_then(|g| match g {
                1 => Some(GuidanceType::Ins),
                2 => Some(GuidanceType::IrHoming),
                3 => Some(GuidanceType::MmwRadar),
                4 => Some(GuidanceType::ActiveRadar),
                5 => Some(GuidanceType::SemiActiveRadar),
                6 => Some(GuidanceType::PassiveRadar),
                7 => Some(GuidanceType::Tv),
                8 => Some(GuidanceType::Laser),
                9 => Some(GuidanceType::Telecontrol),
                _ => None,
            }),
            missile_category: int("missileCategory").and_then(|m| match m {
                1 => Some(MissileCategory::Aam),
                2 => Some(MissileCategory::Sam),
                3 => Some(MissileCategory::Bm),
                4 => Some(MissileCategory::AntiShip),
                5 => Some(MissileCategory::Cruise),
                6 => Some(MissileCategory::Other),
                _ => None,
            }),
            warhead_explosive_kg: wnum("explosiveMass"),
            warhead_mass_kg: wnum("mass"),
            range_max_m: num("rangeMaxAltMax"),
            range_min_m: num("rangeMin"),
            alt_max_m: num("altMax"),
            fuse_dist_m: num("fuseDist"),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ClassWeapon;

impl<'lua> DcsObject<'lua> for Weapon<'lua> {
    type Class = ClassWeapon;

    fn get_instance(lua: MizLua<'lua>, id: &DcsOid<Self::Class>) -> Result<Self> {
        let t = lua.inner().create_table()?;
        t.set_metatable(Some(lua.inner().globals().raw_get(&**id.class)?));
        t.raw_set("id_", id.id)?;
        let t = Weapon {
            t,
            lua: lua.inner(),
        };
        if !t.is_exist()? {
            bail!("{} is an invalid weapon", id.id)
        }
        Ok(t)
    }

    fn get_instance_dyn<T>(lua: MizLua<'lua>, id: &DcsOid<T>) -> Result<Self> {
        id.check_implements(lua, "Weapon")?;
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
            bail!("{} is an invalid weapon", id.id)
        }
        Ok(self)
    }

    fn change_instance_dyn<T>(self, id: &DcsOid<T>) -> Result<Self> {
        id.check_implements(MizLua(self.lua), "Weapon")?;
        self.t.raw_set("id_", id.id)?;
        if !self.is_exist()? {
            bail!("{} is an invalid weapon", id.id)
        }
        Ok(self)
    }
}
