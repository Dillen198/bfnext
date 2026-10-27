/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

//! Lua conversions for the types in this crate (the `lua` feature).

use crate::String;
use compact_str::CompactString;
use mlua::{prelude::*, Value};
use std::fmt::Debug;

pub fn cvt_err(to: &'static str) -> LuaError {
    LuaError::FromLuaConversionError { from: "value", to: to.into(), message: None }
}

pub fn err(msg: &str) -> LuaError {
    LuaError::runtime(msg)
}

pub fn lua_err<E: Debug>(err: E) -> LuaError {
    LuaError::RuntimeError(format!("{:?}", err))
}

impl IntoLua for String {
    fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
        Ok(Value::String(lua.create_string(self.0)?))
    }
}

impl FromLua for String {
    fn from_lua(value: Value, _: &Lua) -> LuaResult<Self> {
        use compact_str::format_compact;
        match value {
            Value::String(s) => Ok(Self(CompactString::from(&*s.to_str()?))),
            Value::Boolean(b) => Ok(Self(format_compact!("{b}"))),
            Value::Integer(n) => Ok(Self(format_compact!("{n}"))),
            Value::Number(n) => Ok(Self(format_compact!("{n}"))),
            // nil and tables used to be stringified ("nil", "table: 0x..."),
            // which leaked into logs, stats and chat as fake names. A value
            // that may legitimately be absent must be read as Option<String>.
            v => Err(LuaError::FromLuaConversionError {
                from: v.type_name(),
                to: "String".into(),
                message: None,
            }),
        }
    }
}
