/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

//! Pins how Lua values convert to Rust and back, for the conversions that
//! have bitten before: nil where a string was expected, numbers sent as
//! strings, and Lua 5.1's single number type meeting Rust's integers. The
//! expectations were written against mlua 0.9; a change in any of them is
//! a behaviour change for the engine and needs a decision, not a quiet
//! update of the test.

use crate::{
    coalition::Side,
    net::{SlotId, Ucid},
    LuaVec3, PathElt, String, Time,
};
use mlua::{prelude::*, Value};
use serde_derive::{Deserialize, Serialize};

fn eval<T: FromLua>(lua: &Lua, src: &str) -> LuaResult<T> {
    T::from_lua(lua.load(src).eval::<Value>()?, lua)
}

#[test]
fn numbers_arrive_as_integer_when_integral() {
    let lua = Lua::new();
    assert!(matches!(eval::<Value>(&lua, "3").unwrap(), Value::Integer(3)));
    assert!(matches!(eval::<Value>(&lua, "3.0").unwrap(), Value::Integer(3)));
    assert!(matches!(eval::<Value>(&lua, "-0").unwrap(), Value::Integer(0)));
    assert!(matches!(eval::<Value>(&lua, "3.5").unwrap(), Value::Number(n) if n == 3.5));
    // outside i64: stays a float
    assert!(matches!(eval::<Value>(&lua, "2^63").unwrap(), Value::Number(_)));
    assert!(
        matches!(eval::<Value>(&lua, "1/0").unwrap(), Value::Number(n) if n.is_infinite())
    );
    assert!(matches!(eval::<Value>(&lua, "'3'").unwrap(), Value::String(_)));
}

#[test]
fn dcso3_string_refuses_nil_and_tables() {
    let lua = Lua::new();
    assert!(eval::<String>(&lua, "nil").is_err());
    assert!(eval::<String>(&lua, "{}").is_err());
    assert_eq!(eval::<Option<String>>(&lua, "nil").unwrap(), None);
    assert_eq!(eval::<Option<String>>(&lua, "'x'").unwrap(), Some(String::from("x")));
    assert_eq!(eval::<String>(&lua, "'abc'").unwrap().as_str(), "abc");
    assert_eq!(eval::<String>(&lua, "''").unwrap().as_str(), "");
    // numbers and booleans are stringified, not refused
    assert_eq!(eval::<String>(&lua, "42").unwrap().as_str(), "42");
    assert_eq!(eval::<String>(&lua, "42.0").unwrap().as_str(), "42");
    assert_eq!(eval::<String>(&lua, "4.5").unwrap().as_str(), "4.5");
    assert_eq!(eval::<String>(&lua, "-7").unwrap().as_str(), "-7");
    assert_eq!(eval::<String>(&lua, "true").unwrap().as_str(), "true");
    // not UTF-8
    assert!(eval::<String>(&lua, r"'\255'").is_err());
}

#[test]
fn std_string_coerces_numbers_and_refuses_nil() {
    let lua = Lua::new();
    assert!(eval::<std::string::String>(&lua, "nil").is_err());
    assert!(eval::<std::string::String>(&lua, "{}").is_err());
    assert!(eval::<std::string::String>(&lua, "true").is_err());
    assert_eq!(eval::<std::string::String>(&lua, "'abc'").unwrap(), "abc");
    assert_eq!(eval::<std::string::String>(&lua, "42").unwrap(), "42");
    assert_eq!(eval::<std::string::String>(&lua, "4.5").unwrap(), "4.5");
    assert_eq!(eval::<Option<std::string::String>>(&lua, "nil").unwrap(), None);
}

#[test]
fn integers_from_numbers_and_numeric_strings() {
    let lua = Lua::new();
    assert_eq!(eval::<i64>(&lua, "42").unwrap(), 42);
    assert_eq!(eval::<i64>(&lua, "42.0").unwrap(), 42);
    assert_eq!(eval::<i64>(&lua, "'42'").unwrap(), 42);
    assert_eq!(eval::<i64>(&lua, "' 42 '").unwrap(), 42);
    assert_eq!(eval::<i64>(&lua, "'0x10'").unwrap(), 16);
    assert!(eval::<i64>(&lua, "'abc'").is_err());
    assert!(eval::<i64>(&lua, "nil").is_err());
    assert!(eval::<i64>(&lua, "true").is_err());
    assert_eq!(eval::<Option<i64>>(&lua, "nil").unwrap(), None);
    // fractional: truncated toward zero, not refused (also from a string)
    assert_eq!(eval::<i64>(&lua, "4.5").unwrap(), 4);
    assert_eq!(eval::<i64>(&lua, "-4.5").unwrap(), -4);
    assert_eq!(eval::<i64>(&lua, "'4.5'").unwrap(), 4);
    // out of range for the target type
    assert!(eval::<u32>(&lua, "-1").is_err());
    assert!(eval::<u8>(&lua, "300").is_err());
    assert_eq!(eval::<u8>(&lua, "255").unwrap(), 255);
    assert!(eval::<i64>(&lua, "2^63").is_err());
    assert!(eval::<i64>(&lua, "0/0").is_err());
}

#[test]
fn floats_from_numbers_and_numeric_strings() {
    let lua = Lua::new();
    assert_eq!(eval::<f64>(&lua, "3").unwrap(), 3.0);
    assert_eq!(eval::<f64>(&lua, "4.5").unwrap(), 4.5);
    assert_eq!(eval::<f64>(&lua, "'4.5'").unwrap(), 4.5);
    assert_eq!(eval::<f64>(&lua, "'1e3'").unwrap(), 1000.0);
    assert!(eval::<f64>(&lua, "'abc'").is_err());
    assert!(eval::<f64>(&lua, "nil").is_err());
    assert!(eval::<f64>(&lua, "true").is_err());
    assert_eq!(eval::<Option<f64>>(&lua, "nil").unwrap(), None);
    assert!(eval::<f64>(&lua, "0/0").unwrap().is_nan());
    assert_eq!(eval::<f32>(&lua, "0.25").unwrap(), 0.25);
    assert_eq!(eval::<Time>(&lua, "5").unwrap(), Time(5.0));
}

#[test]
fn bools_are_lua_truthiness() {
    let lua = Lua::new();
    assert!(!eval::<bool>(&lua, "nil").unwrap());
    assert!(!eval::<bool>(&lua, "false").unwrap());
    assert!(eval::<bool>(&lua, "true").unwrap());
    assert!(eval::<bool>(&lua, "0").unwrap());
    assert!(eval::<bool>(&lua, "'false'").unwrap());
    assert!(eval::<bool>(&lua, "{}").unwrap());
    assert_eq!(eval::<Option<bool>>(&lua, "nil").unwrap(), None);
}

#[test]
fn table_reads_of_missing_and_mistyped_fields() {
    let lua = Lua::new();
    let t: LuaTable = lua.load("{ n = 1, s = '2', f = 2.5, b = false }").eval().unwrap();
    assert_eq!(t.raw_get::<Option<i64>>("missing").unwrap(), None);
    assert!(t.raw_get::<i64>("missing").is_err());
    assert!(t.raw_get::<String>("missing").is_err());
    assert_eq!(t.raw_get::<Option<String>>("missing").unwrap(), None);
    assert_eq!(t.raw_get::<i64>("s").unwrap(), 2);
    assert_eq!(t.raw_get::<String>("n").unwrap().as_str(), "1");
    assert_eq!(t.raw_get::<i64>("f").unwrap(), 2); // 2.5, truncated
    assert_eq!(t.get::<f64>("f").unwrap(), 2.5);
    assert!(!t.get::<bool>("b").unwrap());
    assert!(!t.get::<bool>("missing").unwrap());
    assert!(t.raw_get::<LuaTable>("missing").is_err());
    assert!(t.raw_get::<Option<LuaTable>>("missing").unwrap().is_none());
    // integer and float keys are the same key in Lua
    let a: LuaTable = lua.load("{ 'a', 'b' }").eval().unwrap();
    assert_eq!(a.raw_get::<String>(2).unwrap().as_str(), "b");
    assert_eq!(a.raw_get::<String>(2.0).unwrap().as_str(), "b");
}

#[test]
fn vectors_coerce_numeric_string_components() {
    let lua = Lua::new();
    let v = eval::<LuaVec3>(&lua, "{ x = 1, y = '2', z = 3.5 }").unwrap();
    assert_eq!(v, LuaVec3::new(1., 2., 3.5));
    assert!(eval::<LuaVec3>(&lua, "{ x = 1, y = 2 }").is_err());
    assert!(eval::<LuaVec3>(&lua, "nil").is_err());
}

#[test]
fn engine_ids_from_lua() {
    let lua = Lua::new();
    assert_eq!(eval::<SlotId>(&lua, "12").unwrap(), SlotId::Unit(12));
    assert_eq!(eval::<SlotId>(&lua, "'12'").unwrap(), SlotId::Unit(12));
    assert_eq!(eval::<SlotId>(&lua, "0").unwrap(), SlotId::Spectator);
    assert_eq!(eval::<SlotId>(&lua, "''").unwrap(), SlotId::Spectator);
    assert_eq!(eval::<SlotId>(&lua, "'12_2'").unwrap(), SlotId::MultiCrew(12, 2));
    assert!(eval::<SlotId>(&lua, "nil").is_err());
    assert!(matches!(
        SlotId::Spectator.into_lua(&lua).unwrap(),
        Value::String(s) if s.as_bytes().is_empty()
    ));
    assert!(matches!(SlotId::Unit(7).into_lua(&lua).unwrap(), Value::Integer(7)));
    let ucid = "0123456789abcdef0123456789abcdef";
    assert_eq!(eval::<Ucid>(&lua, &format!("'{ucid}'")).unwrap(), ucid.parse().unwrap());
    assert!(eval::<Ucid>(&lua, "12").is_err());
    assert!(eval::<Ucid>(&lua, "nil").is_err());
}

#[test]
fn enums_from_numbers_and_numeric_strings() {
    let lua = Lua::new();
    assert_eq!(eval::<Side>(&lua, "2").unwrap(), Side::Blue);
    assert_eq!(eval::<Side>(&lua, "2.0").unwrap(), Side::Blue);
    assert_eq!(eval::<Side>(&lua, "'2'").unwrap(), Side::Blue);
    assert!(eval::<Side>(&lua, "7").is_err());
    assert!(eval::<Side>(&lua, "nil").is_err());
    assert!(matches!(Side::Red.into_lua(&lua).unwrap(), Value::Integer(1)));
    assert!(matches!(eval::<PathElt>(&lua, "3").unwrap(), PathElt::Integer(3)));
    assert!(
        matches!(eval::<PathElt>(&lua, "'k'").unwrap(), PathElt::String(s) if s.as_str() == "k")
    );
    assert!(eval::<PathElt>(&lua, "1.5").is_err());
}

#[test]
fn rust_numbers_into_lua() {
    let lua = Lua::new();
    assert!(matches!(3i64.into_lua(&lua).unwrap(), Value::Integer(3)));
    assert!(matches!(3u32.into_lua(&lua).unwrap(), Value::Integer(3)));
    assert!(matches!(3.0f64.into_lua(&lua).unwrap(), Value::Number(n) if n == 3.0));
    assert!(matches!(Time(5.0).into_lua(&lua).unwrap(), Value::Number(n) if n == 5.0));
    assert!(matches!(None::<i64>.into_lua(&lua).unwrap(), Value::Nil));
    // what Lua sees: one number type either way
    lua.globals().set("i", 3i64).unwrap();
    lua.globals().set("f", 3.0f64).unwrap();
    assert!(lua.load("return i == f and type(i) == 'number'").eval::<bool>().unwrap());
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Cfg {
    a: u32,
    b: std::string::String,
    c: Option<std::string::String>,
    f: f64,
    v: Vec<u32>,
}

#[test]
fn serde_round_trip() {
    let lua = Lua::new();
    let v: Value = lua.load("{ a = 1, b = 'x', f = 2, v = {} }").eval().unwrap();
    let cfg: Cfg = lua.from_value(v).unwrap();
    assert_eq!(cfg, Cfg { a: 1, b: "x".into(), c: None, f: 2.0, v: vec![] });
    // a float where an integer is wanted
    let v: Value = lua.load("{ a = 1.5, b = 'x', f = 2, v = {} }").eval().unwrap();
    assert!(lua.from_value::<Cfg>(v).is_err());
    // a number where a string is wanted
    let v: Value = lua.load("{ a = 1, b = 7, f = 2, v = {} }").eval().unwrap();
    assert!(lua.from_value::<Cfg>(v).is_err());
    let back = lua.to_value(&cfg).unwrap();
    let t = back.as_table().unwrap();
    assert!(matches!(t.raw_get::<Value>("a").unwrap(), Value::Integer(1)));
    // stored as a float, read back as an integer: Lua 5.1 has one number type
    assert!(matches!(t.raw_get::<Value>("f").unwrap(), Value::Integer(2)));
    // None is mlua's null (a null light userdata), not nil: the key exists
    assert!(
        matches!(t.raw_get::<Value>("c").unwrap(), Value::LightUserData(p) if p.0.is_null())
    );
    assert_eq!(t.raw_get::<LuaTable>("v").unwrap().raw_len(), 0);
    assert_eq!(lua.from_value::<Cfg>(back).unwrap(), cfg);
}
