/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use crate::{coalition::Side, env::miz::UnitId, String};
use anyhow::{anyhow, bail, Result};
use compact_str::format_compact;
use core::fmt;
use fixedstr::str48;
use serde_derive::{Deserialize, Serialize};
use std::str::FromStr;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(into = "crate::String")]
#[serde(try_from = "crate::String")]
pub enum SlotId {
    Unit(i64),
    MultiCrew(i64, u8),
    Spectator,
    ArtilleryCommander(Side, u8),
    ForwardObserver(Side, u8),
    Observer(Side, u8),
    Instructor(Side, u8),
}

impl fmt::Display for SlotId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Into::<String>::into(*self))
    }
}

impl TryFrom<String> for SlotId {
    type Error = anyhow::Error;

    fn try_from(s: String) -> Result<Self> {
        match s.parse::<i64>() {
            Ok(i) => {
                if i == 0 {
                    Ok(Self::Spectator)
                } else {
                    Ok(Self::Unit(i))
                }
            }
            Err(_) => Ok(Self::parse_string_slot(s.as_str())?),
        }
    }
}

impl Into<String> for SlotId {
    fn into(self) -> String {
        String::from(match self {
            Self::Unit(i) => format_compact!("{i}"),
            Self::Spectator => format_compact!("0"),
            Self::ArtilleryCommander(s, n) => {
                format_compact!("artillery_commander_{s}_{n}")
            }
            Self::ForwardObserver(s, n) => format_compact!("forward_observer_{s}_{n}"),
            Self::Instructor(s, n) => format_compact!("instructor_{s}_{n}"),
            Self::Observer(s, n) => format_compact!("observer_{s}_{n}"),
            Self::MultiCrew(i, n) => format_compact!("{i}_{n}"),
        })
    }
}

impl From<UnitId> for SlotId {
    fn from(value: UnitId) -> Self {
        Self::Unit(value.inner())
    }
}

impl SlotId {
    pub(crate) fn parse_string_slot(s: &str) -> Result<SlotId> {
        fn side_and_num(s: &str) -> Result<(Side, u8)> {
            match s.split_once("_") {
                None => Err(anyhow!("side number {s}")),
                Some((s, n)) => {
                    let side = match s {
                        "red" => Ok(Side::Red),
                        "blue" => Ok(Side::Blue),
                        "neutrals" => Ok(Side::Neutral),
                        s => Err(anyhow!("slot side {s}")),
                    }?;
                    let n = n.parse::<u8>()?;
                    Ok((side, n))
                }
            }
        }
        if s == "" {
            Ok(Self::Spectator)
        } else if let Ok(i) = s.parse::<i64>() {
            if i == 0 {
                Ok(Self::Spectator)
            } else {
                Ok(Self::Unit(i))
            }
        } else if let Some(s) = s.strip_prefix("artillery_commander_") {
            let (side, n) = side_and_num(s)?;
            Ok(Self::ArtilleryCommander(side, n))
        } else if let Some(s) = s.strip_prefix("observer_") {
            let (side, n) = side_and_num(s)?;
            Ok(Self::Observer(side, n))
        } else if let Some(s) = s.strip_prefix("forward_observer_") {
            let (side, n) = side_and_num(s)?;
            Ok(Self::ForwardObserver(side, n))
        } else if let Some(s) = s.strip_prefix("instructor_") {
            let (side, n) = side_and_num(s)?;
            Ok(Self::Instructor(side, n))
        } else {
            match s.split_once("_") {
                None => Err(anyhow!("invalid string slot {s}")),
                Some((i, n)) => {
                    let i = i.parse::<i64>()?;
                    let n = n.parse::<u8>()?;
                    Ok(Self::MultiCrew(i, n))
                }
            }
        }
    }

    pub fn is_artillery_commander(&self) -> bool {
        match self {
            Self::ArtilleryCommander(_, _) => true,
            _ => false,
        }
    }

    pub fn is_observer(&self) -> bool {
        match self {
            Self::Observer(_, _) => true,
            _ => false,
        }
    }

    pub fn is_forward_observer(&self) -> bool {
        match self {
            Self::ForwardObserver(_, _) => true,
            _ => false,
        }
    }

    pub fn is_instructor(&self) -> bool {
        match self {
            Self::Instructor(_, _) => true,
            _ => false,
        }
    }

    pub fn is_spectator(&self) -> bool {
        match self {
            Self::Spectator => true,
            _ => false,
        }
    }

    pub fn is_multicrew(&self) -> bool {
        match self {
            Self::MultiCrew(_, _) => true,
            _ => false,
        }
    }

    pub fn as_unit_id(&self) -> Option<UnitId> {
        match self {
            Self::Unit(i) => Some(UnitId::from(*i)),
            Self::MultiCrew(i, _) => Some(UnitId::from(*i)),
            Self::ArtilleryCommander(_, _)
            | Self::ForwardObserver(_, _)
            | Self::Instructor(_, _)
            | Self::Observer(_, _)
            | Self::Spectator => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(try_from = "str48", into = "str48")]
pub struct Ucid([u8; 16]);

impl Default for Ucid {
    fn default() -> Self {
        Ucid([0; 16])
    }
}

impl fmt::Display for Ucid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Into::<str48>::into(*self))
    }
}

impl fmt::Debug for Ucid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl TryFrom<str48> for Ucid {
    type Error = anyhow::Error;

    fn try_from(s: str48) -> Result<Self> {
        s.parse()
    }
}

impl FromStr for Ucid {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        // Check the bytes are hex first: len() counts bytes, so a 32 byte
        // string holding a multibyte char would pass the length check and
        // then panic slicing mid-char below (reachable from bfdb HTTP
        // bodies). It also stops from_str_radix accepting a "+f" pair.
        if s.len() != 32 || !s.bytes().all(|b| b.is_ascii_hexdigit()) {
            bail!("expected a 32 character hex string got \"{s}\"")
        }
        let mut a = [0; 16];
        for i in 0..16 {
            let j = i << 1;
            a[i] = u8::from_str_radix(&s[j..j + 2], 16)?;
        }
        Ok(Self(a))
    }
}

impl Into<str48> for Ucid {
    fn into(self) -> str48 {
        use std::fmt::Write;
        let mut s = str48::new();
        for i in 0..16 {
            write!(s, "{:02x}", self.0[i]).unwrap()
        }
        s
    }
}

#[cfg(feature = "lua")]
mod lua_impls {
    use super::{SlotId, Ucid};
    use crate::{
        lua::{err, lua_err},
        String,
    };
    use compact_str::format_compact;
    use fixedstr::str48;
    use mlua::{prelude::*, Value};

    impl FromLua for SlotId {
        fn from_lua(value: Value, _: &Lua) -> LuaResult<Self> {
            match value {
                Value::Integer(i) => {
                    if i == 0 {
                        Ok(Self::Spectator)
                    } else {
                        Ok(Self::Unit(i))
                    }
                }
                Value::Number(i) => {
                    let i = i as i64;
                    if i == 0 {
                        Ok(Self::Spectator)
                    } else {
                        Ok(Self::Unit(i))
                    }
                }
                Value::String(s) => {
                    Self::parse_string_slot(s.to_str().map_err(lua_err)?.trim())
                        .map_err(lua_err)
                }
                v => Err(lua_err(format!("invalid slot type {:?}", v))),
            }
        }
    }

    impl IntoLua for SlotId {
        fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
            match self {
                Self::Unit(i) => {
                    if i < 1 {
                        return Err(lua_err("invalid unit number"));
                    }
                    Ok(Value::Integer(i))
                }
                Self::Spectator => Ok(Value::String(lua.create_string("")?)),
                Self::ArtilleryCommander(s, n) => {
                    if n < 1 {
                        return Err(lua_err("invalid ca slot number"));
                    }
                    String(format_compact!("artillery_commander_{s}_{n}")).into_lua(lua)
                }
                Self::ForwardObserver(s, n) => {
                    if n < 1 {
                        return Err(lua_err("invalid ca slot number"));
                    }
                    String(format_compact!("forward_observer_{s}_{n}")).into_lua(lua)
                }
                Self::Instructor(s, n) => {
                    if n < 1 {
                        return Err(lua_err("invalid ca slot number"));
                    }
                    String(format_compact!("instructor_{s}_{n}")).into_lua(lua)
                }
                Self::Observer(s, n) => {
                    if n < 1 {
                        return Err(lua_err("invalid ca slot number"));
                    }
                    String(format_compact!("observer_{s}_{n}")).into_lua(lua)
                }
                Self::MultiCrew(i, n) => {
                    if n < 1 {
                        return Err(lua_err("invalid multi crew slot number"));
                    }
                    String(format_compact!("{i}_{n}")).into_lua(lua)
                }
            }
        }
    }

    impl FromLua for Ucid {
        fn from_lua(value: Value, _lua: &Lua) -> LuaResult<Self> {
            match value {
                Value::String(s) => s
                    .to_str()?
                    .parse()
                    .map_err(|e| err(&format_compact!("decoding ucid {}", e))),
                _ => Err(err("expected ucid to be a string")),
            }
        }
    }

    impl IntoLua for Ucid {
        fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
            let s: str48 = self.into();
            Ok(Value::String(lua.create_string(s.as_str())?))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slot_ids_round_trip_through_their_string_form() {
        for slot in [
            SlotId::Unit(42),
            SlotId::MultiCrew(42, 2),
            SlotId::Spectator,
            SlotId::ArtilleryCommander(Side::Blue, 1),
            SlotId::ForwardObserver(Side::Red, 3),
            SlotId::Observer(Side::Neutral, 1),
            SlotId::Instructor(Side::Blue, 2),
        ] {
            let s: String = slot.into();
            assert_eq!(SlotId::try_from(s.clone()).unwrap(), slot, "{s}");
        }
        assert_eq!(SlotId::try_from(String::from("")).unwrap(), SlotId::Spectator);
        assert!(SlotId::try_from(String::from("observer_green_1")).is_err());
    }

    #[test]
    fn ucid_parses_only_32_hex_chars() {
        let s = "0123456789abcdef0123456789abcdef";
        let u: Ucid = s.parse().unwrap();
        assert_eq!(u.to_string(), s);
        assert!("0123".parse::<Ucid>().is_err());
        // 32 bytes, but not 32 hex characters
        assert!("é123456789abcdef0123456789abcde".parse::<Ucid>().is_err());
    }
}
