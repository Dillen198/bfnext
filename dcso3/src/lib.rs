/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

// A DLL that DCS loads must use the Lua runtime DCS already has.
#[cfg(all(feature = "vendored-lua", not(debug_assertions)))]
compile_error!("dcso3's vendored-lua feature is for tests, not release builds");

extern crate nalgebra as na;
use anyhow::{anyhow, bail, Result};
use fxhash::FxHashMap;
use log::error;
use mlua::{prelude::*, Value};
pub use schemars;
use serde_derive::{Deserialize, Serialize};
use std::{
    backtrace::Backtrace,
    cell::RefCell,
    collections::hash_map::Entry,
    fmt::Debug,
    marker::PhantomData,
    ops::{Add, AddAssign, Deref, DerefMut, Sub},
    panic::{self, AssertUnwindSafe},
};

pub mod airbase;
pub mod atmosphere;
pub mod attribute;
pub mod coalition;
pub mod controller;
#[cfg(test)]
mod conversion_tests;
pub mod coord;
pub mod country;
pub mod dcs;
pub mod env;
pub mod event;
pub mod group;
pub mod hooks;
pub mod land;
pub mod lfs;
pub mod mission_commands;
pub mod net;
pub mod object;
pub mod perf;
pub mod spot;
pub mod static_object;
pub mod timer;
pub mod trigger;
pub mod unit;
pub mod warehouse;
pub mod weapon;
pub mod world;

// The Lua-free types and macros live in dcso3-core so that programs which
// never load into DCS can use them without linking Lua. Re-exported here
// (and in each module, at the type's old path) so nothing downstream moves.
pub use dcso3_core::{
    atomic_id,
    lua::{cvt_err, err, lua_err},
    simple_enum, simple_enum_unknown, string_enum, wrapped_prim, String, Vector2,
    Vector3,
};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Quad2 {
    pub p0: LuaVec2,
    pub p1: LuaVec2,
    pub p2: LuaVec2,
    pub p3: LuaVec2,
}

impl FromLua for Quad2 {
    fn from_lua(value: Value, _lua: &Lua) -> LuaResult<Self> {
        let verts = as_tbl("Quad", None, value).map_err(lua_err)?;
        Ok(Self {
            p0: verts.raw_get(1)?,
            p1: verts.raw_get(2)?,
            p2: verts.raw_get(3)?,
            p3: verts.raw_get(4)?,
        })
    }
}

impl Quad2 {
    pub fn contains(&self, p: LuaVec2) -> bool {
        fn horizontal_ray_intersects_edge(
            p: &LuaVec2,
            v0: &LuaVec2,
            v1: &LuaVec2,
        ) -> bool {
            if (v0.y > p.y) == (v1.y > p.y) {
                // we're casting horizontally so we don't need to consider the case where
                // there couldn't be a horizontal intersection
                false
            } else {
                let int_x = v0.x + (p.y - v0.y) * (v1.x - v0.x) / (v1.y - v0.y);
                int_x > p.x
            }
        }
        if p == self.p0 || p == self.p1 || p == self.p2 || p == self.p3 {
            return true;
        }
        let mut intersections = 0;
        macro_rules! check_edge {
            ($v0:expr, $v1:expr) => {
                if $v0.y != $v1.y && horizontal_ray_intersects_edge(&p, &$v0, &$v1) {
                    // if the ray passes through a vertex only count it if
                    // it passes through the upper vertex (to avoid counting it twice)
                    if $v0.y == p.y || $v1.y == p.y {
                        let to_check = if $v0.y == p.y { $v1 } else { $v0 };
                        if to_check.y < p.y {
                            intersections += 1;
                        }
                    } else {
                        intersections += 1
                    }
                }
            };
        }
        check_edge!(self.p0, self.p1);
        check_edge!(self.p1, self.p2);
        check_edge!(self.p2, self.p3);
        check_edge!(self.p3, self.p0);
        intersections % 2 == 1
    }

    pub fn longest_edge(&self) -> (Vector2, Vector2, f64) {
        [
            (self.p0.0, self.p1.0),
            (self.p1.0, self.p2.0),
            (self.p2.0, self.p3.0),
            (self.p3.0, self.p0.0),
        ]
        .into_iter()
        .fold(
            (Vector2::default(), Vector2::default(), 0.),
            |x @ (_, _, d), (v0, v1)| {
                let d2 = na::distance_squared(&v0.into(), &v1.into());
                if d2 > d {
                    (v0, v1, d2)
                } else {
                    x
                }
            },
        )
    }

    pub fn center(&self) -> Vector2 {
        centroid2d([self.p0.0, self.p1.0, self.p2.0, self.p3.0])
    }

    /// Scale this quad by the specified factor. The factor must be
    /// positive. Numbers less than 1 will make the quad smaller,
    /// numbers greater than 1 will make it bigger. If a negative
    /// number is provided then 0 will be used.
    pub fn scale(&self, factor: f64) -> Self {
        let factor = factor.clamp(0., f64::INFINITY);
        let center = self.center();
        let p0 = (self.p0.0 - center).normalize();
        let p1 = (self.p1.0 - center).normalize();
        let p2 = (self.p2.0 - center).normalize();
        let p3 = (self.p3.0 - center).normalize();
        let pd0 = na::distance(&center.into(), &self.p0.0.into()) * factor;
        let pd1 = na::distance(&center.into(), &self.p1.0.into()) * factor;
        let pd2 = na::distance(&center.into(), &self.p2.0.into()) * factor;
        let pd3 = na::distance(&center.into(), &self.p3.0.into()) * factor;
        Self {
            p0: LuaVec2(center + p0 * pd0),
            p1: LuaVec2(center + p1 * pd1),
            p2: LuaVec2(center + p2 * pd2),
            p3: LuaVec2(center + p3 * pd3),
        }
    }

    /// return true if the specified circle is fully contained within the quad.
    pub fn contains_circle(&self, center: Vector2, radius: f64) -> bool {
        fn distance_to_segment(p: Vector2, a: Vector2, b: Vector2) -> f64 {
            let ap = p - a;
            let ab = b - a;
            let t = (ap.dot(&ab) / ab.dot(&ab)).clamp(0., 1.);
            na::distance(&p.into(), &(a + t * ab).into())
        }
        self.contains(LuaVec2(center))
            && distance_to_segment(center, self.p0.0, self.p1.0) >= radius
            && distance_to_segment(center, self.p1.0, self.p2.0) >= radius
            && distance_to_segment(center, self.p2.0, self.p3.0) >= radius
            && distance_to_segment(center, self.p3.0, self.p0.0) >= radius
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Color {
    r: f32,
    g: f32,
    b: f32,
    a: f32,
}

impl Color {
    pub fn new(r: f32, g: f32, b: f32, a: f32) -> Color {
        Color { r, g, b, a }
    }

    pub fn black(a: f32) -> Color {
        Color { r: 0., g: 0., b: 0., a }
    }

    pub fn red(a: f32) -> Color {
        Color { r: 1., g: 0., b: 0., a }
    }

    pub fn white(a: f32) -> Color {
        Color { r: 1., g: 1., b: 1., a }
    }

    pub fn blue(a: f32) -> Color {
        Color { r: 0., g: 0., b: 1., a }
    }

    pub fn gray(a: f32) -> Color {
        Color { r: 0.25, g: 0.25, b: 0.25, a }
    }

    pub fn green(a: f32) -> Color {
        Color { r: 0., g: 1., b: 0., a }
    }

    pub fn yellow(a: f32) -> Color {
        Color { r: 0.75, g: 1., b: 0., a }
    }

    /// High-contrast stand-in for `red` on F10 map overlays. Pure red sits too
    /// close to the terrain luminance on desert maps (Syria, Sinai, PG) to be
    /// legible; violet separates cleanly from both sand and vegetation, and
    /// leaves red free to mean "bad" in status glyphs.
    pub fn violet(a: f32) -> Color {
        Color { r: 0.72, g: 0.30, b: 1., a }
    }

    /// High-contrast stand-in for `blue` on F10 map overlays. Pure blue is
    /// nearly invisible over water and dark terrain; azure keeps the coalition
    /// reading while staying bright enough to pick out.
    pub fn azure(a: f32) -> Color {
        Color { r: 0.25, g: 0.65, b: 1., a }
    }

    pub fn with_alpha(self, a: f32) -> Color {
        Color { a, ..self }
    }
}

impl FromLua for Color {
    fn from_lua(value: Value, _lua: &Lua) -> LuaResult<Self> {
        let tbl = as_tbl("Color", None, value).map_err(lua_err)?;
        Ok(Self {
            r: tbl.raw_get(1)?,
            g: tbl.raw_get(2)?,
            b: tbl.raw_get(3)?,
            a: tbl.raw_get(4)?,
        })
    }
}

impl IntoLua for Color {
    fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
        let tbl = lua.create_table()?;
        tbl.set(1, self.r)?;
        tbl.set(2, self.g)?;
        tbl.set(3, self.b)?;
        tbl.set(4, self.a)?;
        Ok(Value::Table(tbl))
    }
}

pub fn wrap_f<'lua, L: LuaEnv<'lua>, R: Default, F: FnOnce(L) -> Result<R>>(
    name: &str,
    lua: L,
    f: F,
) -> LuaResult<R> {
    match panic::catch_unwind(AssertUnwindSafe(|| wrap(name, f(lua)))) {
        Ok(r) => r,
        Err(e) => {
            match e.downcast_ref::<anyhow::Error>() {
                Some(e) => error!("{name} panicked {e:?} {}", Backtrace::capture()),
                None => error!("{name} panicked {e:?} {}", Backtrace::capture()),
            }
            Ok(R::default())
        }
    }
}

/// Like `wrap_f`, but for callbacks where the `Default` answer is the
/// dangerous one. The auth hooks (try connect / try change slot) read an
/// empty return as "no objection", so an error or panic in the handler
/// used to let the player through. Here any failure produces `deny()`
/// instead, so the hook fails closed.
pub fn wrap_f_deny<'lua, L, R, D, F>(name: &str, lua: L, deny: D, f: F) -> LuaResult<R>
where
    L: LuaEnv<'lua>,
    D: FnOnce() -> LuaResult<R>,
    F: FnOnce(L) -> Result<R>,
{
    match panic::catch_unwind(AssertUnwindSafe(|| f(lua))) {
        Ok(Ok(r)) => Ok(r),
        Ok(Err(e)) => {
            error!("{name}: {e:?}, denying");
            deny()
        }
        Err(e) => {
            match e.downcast_ref::<anyhow::Error>() {
                Some(e) => {
                    error!("{name} panicked {e:?}, denying {}", Backtrace::capture())
                }
                None => error!("{name} panicked {e:?}, denying {}", Backtrace::capture()),
            }
            deny()
        }
    }
}

pub fn wrap<R: Default>(name: &str, res: Result<R>) -> LuaResult<R> {
    match res {
        Ok(r) => Ok(r),
        Err(e) => {
            error!("{}: {:?}", name, e);
            Ok(R::default())
        }
    }
}

pub trait LuaEnv<'a> {
    fn inner(self) -> &'a Lua;
}

impl<'lua> LuaEnv<'lua> for &'lua Lua {
    fn inner(self) -> &'lua Lua {
        self
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HooksLua<'lua>(&'lua Lua);

impl<'lua> LuaEnv<'lua> for HooksLua<'lua> {
    fn inner(self) -> &'lua Lua {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MizLua<'lua>(pub &'lua Lua);

impl<'lua> LuaEnv<'lua> for MizLua<'lua> {
    fn inner(self) -> &'lua Lua {
        self.0
    }
}

pub fn create_root_module<H, M>(
    lua: &Lua,
    init_hooks: H,
    init_miz: M,
) -> LuaResult<LuaTable>
where
    H: Fn(HooksLua) -> Result<()> + 'static,
    M: Fn(MizLua) -> Result<()> + 'static,
{
    let exports = lua.create_table()?;
    exports.set(
        "initHooks",
        lua.create_function(move |lua, _: ()| {
            wrap_f("init_hooks", HooksLua(lua), &init_hooks)
        })?,
    )?;
    exports.set(
        "initMiz",
        lua.create_function(move |lua, _: ()| {
            wrap_f("init_miz", MizLua(lua), &init_miz)
        })?,
    )?;
    Ok(exports)
}

#[macro_export]
macro_rules! wrapped_table {
    ($name:ident, $class:expr) => {
        #[derive(Clone, Serialize)]
        pub struct $name {
            t: mlua::Table,
            // Owned (a reference count), not a borrow: mlua 0.10 handles
            // carry no lifetime. Do not keep these past the DCS callback
            // that produced them.
            #[allow(dead_code)]
            #[serde(skip)]
            lua: Lua,
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.t.raw_get::<Value>("id_") {
                    Ok(Value::Nil) => {
                        let v = crate::value_to_json(&Value::Table(self.t.clone()));
                        write!(f, "{v}")
                    }
                    Ok(v) => {
                        let class: String = self
                            .t
                            .metatable()
                            .and_then(|mt| mt.raw_get("className_").ok())
                            .unwrap_or(String::from("unknown"));
                        write!(f, "{{ class: {}, id: {:?} }}", class, v)
                    }
                    Err(_) => write!(f, "{:?}", self.t),
                }
            }
        }

        impl Deref for $name {
            type Target = mlua::Table;

            fn deref(&self) -> &Self::Target {
                &self.t
            }
        }

        impl FromLua for $name {
            fn from_lua(value: Value, lua: &Lua) -> LuaResult<Self> {
                Ok(Self {
                    t: as_tbl(stringify!($name), $class, value)
                        .map_err(crate::lua_err)?,
                    lua: lua.clone(),
                })
            }
        }

        impl IntoLua for $name {
            fn into_lua(self, _lua: &Lua) -> LuaResult<Value> {
                Ok(Value::Table(self.t))
            }
        }
    };
}

#[macro_export]
macro_rules! bitflags_enum {
    ($name:ident, $repr:ident, [$($case:ident => $num:literal),+]) => {
        #[bitflags]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        #[allow(non_camel_case_types)]
        #[repr($repr)]
        pub enum $name {
            $($case = $num),+
        }

        impl FromLua for $name {
            fn from_lua(value: Value, lua: &Lua) -> LuaResult<Self> {
                Ok(match $repr::from_lua(value, lua)? {
                    $($num => Self::$case),+,
                    _ => return Err(cvt_err(stringify!($name)))
                })
            }
        }

        impl IntoLua for $name {
            fn into_lua(self, _lua: &Lua) -> LuaResult<Value> {
                Ok(Value::Integer(self as i64))
            }
        }
    };
}

pub fn as_tbl_ref<'a>(to: &'static str, value: &'a Value) -> Result<&'a mlua::Table> {
    value.as_table().ok_or_else(|| anyhow!("can't convert {:?} to {}", value, to))
}

fn check_implements(tbl: &mlua::Table, class: &str) -> bool {
    let mut parent = None;
    loop {
        let tbl = match parent.as_ref() {
            None => tbl,
            Some(tbl) => tbl,
        };
        match tbl.raw_get::<String>("className_") {
            Err(_) => break false,
            Ok(s) if s.as_str() == class => break true,
            Ok(_) => match tbl.raw_get::<mlua::Table>("parentClass_") {
                Err(_) => break false,
                Ok(t) => {
                    parent = Some(t);
                }
            },
        }
    }
}

pub fn as_tbl(
    to: &'static str,
    objtyp: Option<&'static str>,
    value: Value,
) -> Result<mlua::Table> {
    match value {
        Value::Table(tbl) => match objtyp {
            None => Ok(tbl),
            Some(typ) => match tbl.metatable() {
                None => bail!(
                    "to: {to}. not an object, expected object of type {} got {:?}",
                    typ,
                    tbl
                ),
                Some(meta) => {
                    if check_implements(&meta, typ) {
                        Ok(tbl)
                    } else {
                        bail!("to: {to}. expected object of type {}, got {:?}", typ, tbl)
                    }
                }
            },
        },
        _ => bail!("expected a table, got {:?}", value),
    }
}

pub trait DeepClone: IntoLua + FromLua + Clone {
    fn deep_clone(&self, lua: &Lua) -> Result<Self>;
}

impl<T> DeepClone for T
where
    T: IntoLua + FromLua + Clone,
{
    fn deep_clone(&self, lua: &Lua) -> Result<Self> {
        let mut seen = FxHashMap::default();
        let v = deep_clone_value(lua, self.clone().into_lua(lua)?, &mut seen, 0)?;
        Ok(T::from_lua(v, lua)?)
    }
}

/// Nesting deeper than this is treated as runaway input rather than data;
/// real DCS tables (mission, routes, tasks) are a few dozen levels at most.
const DEEP_CLONE_MAX_DEPTH: usize = 200;

/// The recursion behind `DeepClone`. `seen` maps each source table already
/// copied to its copy, so a table reachable twice (or through a cycle) is
/// cloned once and the copy keeps the same shape. Without it a cyclic table
/// recursed until the stack overflowed, which takes DCS down with it.
fn deep_clone_value(
    lua: &Lua,
    v: Value,
    seen: &mut FxHashMap<usize, mlua::Table>,
    depth: usize,
) -> Result<Value> {
    Ok(match v {
        Value::String(s) => Value::String(lua.create_string(s.as_bytes())?),
        Value::Table(t) => {
            let addr = t.to_pointer() as usize;
            if let Some(new) = seen.get(&addr) {
                return Ok(Value::Table(new.clone()));
            }
            if depth >= DEEP_CLONE_MAX_DEPTH {
                bail!("deep_clone: table nesting exceeds {DEEP_CLONE_MAX_DEPTH}")
            }
            let new = lua.create_table()?;
            new.set_metatable(t.metatable());
            seen.insert(addr, new.clone());
            for r in t.pairs::<Value, Value>() {
                let (k, v) = r?;
                let k = deep_clone_value(lua, k, seen, depth + 1)?;
                let v = deep_clone_value(lua, v, seen, depth + 1)?;
                new.set(k, v)?
            }
            Value::Table(new)
        }
        v @ (Value::Boolean(_)
        | Value::Error(_)
        | Value::Function(_)
        | Value::Integer(_)
        | Value::LightUserData(_)
        | Value::Nil
        | Value::Number(_)
        | Value::Thread(_)
        | Value::UserData(_)
        | Value::Other(_)) => v,
    })
}

pub fn is_hooks_env(lua: &Lua) -> bool {
    lua.globals().contains_key("DCS").unwrap_or(false)
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum PathElt {
    Integer(i64),
    String(String),
}

impl From<&str> for PathElt {
    fn from(value: &str) -> Self {
        PathElt::String(String::from(value))
    }
}

impl From<String> for PathElt {
    fn from(value: String) -> Self {
        PathElt::String(value)
    }
}

impl From<std::string::String> for PathElt {
    fn from(value: std::string::String) -> Self {
        PathElt::String(String::from(value))
    }
}

impl From<usize> for PathElt {
    fn from(value: usize) -> Self {
        PathElt::Integer(value as i64)
    }
}

impl From<u64> for PathElt {
    fn from(value: u64) -> Self {
        PathElt::Integer(value as i64)
    }
}

impl From<u32> for PathElt {
    fn from(value: u32) -> Self {
        PathElt::Integer(value as i64)
    }
}

impl From<i64> for PathElt {
    fn from(value: i64) -> Self {
        PathElt::Integer(value)
    }
}

impl From<i32> for PathElt {
    fn from(value: i32) -> Self {
        PathElt::Integer(value as i64)
    }
}

impl From<u8> for PathElt {
    fn from(value: u8) -> Self {
        PathElt::Integer(value as i64)
    }
}

impl IntoLua for &PathElt {
    fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
        Ok(match self {
            PathElt::Integer(i) => Value::Integer(*i),
            PathElt::String(s) => Value::String(lua.create_string(s.as_bytes())?),
        })
    }
}

impl FromLua for PathElt {
    fn from_lua(value: Value, lua: &Lua) -> LuaResult<Self> {
        Ok(match value {
            Value::Integer(n) => PathElt::Integer(n),
            Value::String(_) => PathElt::String(String::from_lua(value, lua)?),
            _ => return Err(cvt_err("String")),
        })
    }
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct Path(Vec<PathElt>);

impl Deref for Path {
    type Target = [PathElt];

    fn deref(&self) -> &Self::Target {
        &self.0[..]
    }
}

impl<'a> IntoIterator for &'a Path {
    type IntoIter = std::slice::Iter<'a, PathElt>;
    type Item = &'a PathElt;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl Path {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push<T: Into<PathElt>>(&mut self, t: T) {
        self.0.push(t.into())
    }

    pub fn pop(&mut self) -> Option<PathElt> {
        self.0.pop()
    }

    pub fn append<T: Into<PathElt>, I: IntoIterator<Item = T>>(&self, elts: I) -> Self {
        let mut new_t = self.clone();
        for elt in elts {
            new_t.push(elt)
        }
        new_t
    }

    pub fn get(&self, i: usize) -> Option<&PathElt> {
        self.0.get(i)
    }
}

#[macro_export]
macro_rules! path {
    ($($v:expr),*) => {{
        let mut path = dcso3::Path::new();
        $(path.push($v));*;
        path
    }}
}

pub trait DcsTableExt {
    fn raw_get_path<T>(&self, path: &Path) -> Result<T>
    where
        T: FromLua;

    fn get_path<T>(&self, path: &Path) -> Result<T>
    where
        T: FromLua;
}

fn table_raw_get_path<T>(tbl: &mlua::Table, path: &[PathElt]) -> Result<T>
where
    T: FromLua,
{
    match path {
        [] => bail!("path not found"),
        [elt] => Ok(tbl.raw_get(elt)?),
        [elt, path @ ..] => {
            let tbl: mlua::Table = tbl.raw_get(elt)?;
            table_raw_get_path(&tbl, path)
        }
    }
}

fn table_get_path<T>(tbl: &mlua::Table, path: &[PathElt]) -> Result<T>
where
    T: FromLua,
{
    match path {
        [] => bail!("path not found"),
        [elt] => Ok(tbl.get(elt)?),
        [elt, path @ ..] => {
            let tbl: mlua::Table = tbl.get(elt)?;
            table_get_path(&tbl, path)
        }
    }
}

impl DcsTableExt for mlua::Table {
    fn raw_get_path<T>(&self, path: &Path) -> Result<T>
    where
        T: FromLua,
    {
        table_raw_get_path(self, &**path)
    }

    fn get_path<T>(&self, path: &Path) -> Result<T>
    where
        T: FromLua,
    {
        table_get_path(self, &**path)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct LuaVec2(pub na::base::Vector2<f64>);

impl Deref for LuaVec2 {
    type Target = na::base::Vector2<f64>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for LuaVec2 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl IntoLua for LuaVec2 {
    fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
        let tbl = lua.create_table()?;
        tbl.set("x", self.0.x)?;
        tbl.set("y", self.0.y)?;
        Ok(Value::Table(tbl))
    }
}

impl FromLua for LuaVec2 {
    fn from_lua(value: Value, _: &Lua) -> LuaResult<Self> {
        let tbl = as_tbl("Vec2", None, value).map_err(lua_err)?;
        Ok(Self(na::base::Vector2::new(tbl.raw_get("x")?, tbl.raw_get("y")?)))
    }
}

impl LuaVec2 {
    pub fn new(x: f64, y: f64) -> Self {
        LuaVec2(na::base::Vector2::new(x, y))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct LuaVec3(pub na::base::Vector3<f64>);

impl Deref for LuaVec3 {
    type Target = na::base::Vector3<f64>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for LuaVec3 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromLua for LuaVec3 {
    fn from_lua(value: Value, _: &Lua) -> LuaResult<Self> {
        let tbl = as_tbl("Vec3", None, value).map_err(lua_err)?;
        Ok(Self(na::base::Vector3::new(
            tbl.raw_get("x")?,
            tbl.raw_get("y")?,
            tbl.raw_get("z")?,
        )))
    }
}

impl IntoLua for LuaVec3 {
    fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
        let tbl = lua.create_table()?;
        tbl.raw_set("x", self.0.x)?;
        tbl.raw_set("y", self.0.y)?;
        tbl.raw_set("z", self.0.z)?;
        Ok(Value::Table(tbl))
    }
}

impl LuaVec3 {
    pub fn new(x: f64, y: f64, z: f64) -> Self {
        Self(na::base::Vector3::new(x, y, z))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default, Serialize, Deserialize)]
pub struct Position3 {
    pub p: LuaVec3,
    pub x: LuaVec3,
    pub y: LuaVec3,
    pub z: LuaVec3,
}

impl FromLua for Position3 {
    fn from_lua(value: Value, _: &Lua) -> LuaResult<Self> {
        let tbl = as_tbl("Position3", None, value).map_err(lua_err)?;
        Ok(Self {
            p: tbl.raw_get("p")?,
            x: tbl.raw_get("x")?,
            y: tbl.raw_get("y")?,
            z: tbl.raw_get("z")?,
        })
    }
}

impl IntoLua for Position3 {
    fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
        let tbl = lua.create_table()?;
        tbl.raw_set("p", self.p)?;
        tbl.raw_set("x", self.x)?;
        tbl.raw_set("y", self.y)?;
        tbl.raw_set("z", self.z)?;
        Ok(Value::Table(tbl))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub struct Box3 {
    pub min: LuaVec3,
    pub max: LuaVec3,
}

impl FromLua for Box3 {
    fn from_lua(value: Value, _: &Lua) -> LuaResult<Self> {
        let tbl = as_tbl("Box3", None, value).map_err(lua_err)?;
        Ok(Self { min: tbl.raw_get("min")?, max: tbl.raw_get("max")? })
    }
}

/// Mission time in seconds (timer.getTime/getAbsTime, event times). f64
/// because that is what Lua holds: an f32 only resolves ~0.06 s once a
/// server has been up ~6 days, and abs time starts at the mission's clock
/// time so it is large from the first frame.
#[derive(Debug, Clone, Copy, Default, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Time(pub f64);

impl IntoLua for Time {
    fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
        self.0.into_lua(lua)
    }
}

impl FromLua for Time {
    fn from_lua(value: Value, lua: &Lua) -> LuaResult<Self> {
        Ok(Self(f64::from_lua(value, lua)?))
    }
}

impl Add<f64> for Time {
    type Output = Self;

    fn add(self, rhs: f64) -> Self::Output {
        Time(self.0 + rhs)
    }
}

impl AddAssign<f64> for Time {
    fn add_assign(&mut self, rhs: f64) {
        self.0 += rhs
    }
}

impl Sub for Time {
    type Output = f64;

    fn sub(self, rhs: Self) -> f64 {
        self.0 - rhs.0
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum VolumeType {
    Segment,
    Box,
    Sphere,
    Pyramid,
}

#[derive(Debug, Clone, Serialize)]
pub struct Sequence<T> {
    t: mlua::Table,
    #[serde(skip)]
    lua: Lua,
    ph: PhantomData<T>,
}

impl<T: FromLua> FromLua for Sequence<T> {
    fn from_lua(value: Value, lua: &Lua) -> LuaResult<Self> {
        match value {
            Value::Table(t) => Ok(Self { t, lua: lua.clone(), ph: PhantomData }),
            Value::Nil => {
                Ok(Self { t: lua.create_table()?, lua: lua.clone(), ph: PhantomData })
            }
            _ => Err(cvt_err("Sequence")),
        }
    }
}

impl<T: IntoLua> IntoLua for Sequence<T> {
    fn into_lua(self, _lua: &Lua) -> LuaResult<Value> {
        Ok(Value::Table(self.t))
    }
}

/// The values of a `Sequence` in order: `t[1]`, `t[2]`, ... up to the
/// first nil, read raw (what mlua's `sequence_values` does, which in 0.10
/// borrows the table and so cannot be returned from `into_iter`).
pub struct SequenceIter<T> {
    t: mlua::Table,
    lua: Lua,
    i: i64,
    ph: PhantomData<T>,
}

impl<T: FromLua> Iterator for SequenceIter<T> {
    type Item = LuaResult<T>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.t.raw_get::<Value>(self.i) {
            Err(e) => Some(Err(e)),
            Ok(Value::Nil) => None,
            Ok(v) => {
                self.i += 1;
                Some(T::from_lua(v, &self.lua))
            }
        }
    }
}

impl<T: FromLua> IntoIterator for Sequence<T> {
    type IntoIter = SequenceIter<T>;
    type Item = LuaResult<T>;

    fn into_iter(self) -> Self::IntoIter {
        SequenceIter { t: self.t, lua: self.lua, i: 1, ph: PhantomData }
    }
}

impl<T: FromLua> Sequence<T> {
    pub fn get(&self, i: i64) -> Result<T> {
        Ok(self.t.raw_get(i)?)
    }
}

impl<T: IntoLua> Sequence<T> {
    pub fn set(&self, i: i64, t: T) -> Result<()> {
        Ok(self.t.raw_set(i, t)?)
    }
}

impl<T: FromLua> Sequence<T> {
    pub fn empty(lua: &Lua) -> Result<Self> {
        Ok(Self { t: lua.create_table()?, lua: lua.clone(), ph: PhantomData })
    }

    pub fn len(&self) -> usize {
        self.t.raw_len()
    }

    pub fn into_inner(self) -> mlua::Table {
        self.t
    }

    pub fn remove(&self, i: i64) -> Result<()> {
        Ok(self.t.raw_remove(i)?)
    }

    pub fn first(&self) -> Result<T> {
        Ok(self.t.raw_get(1)?)
    }

    pub fn for_each<F: FnMut(Result<T>) -> Result<()>>(&self, mut f: F) -> Result<()> {
        Ok(self.t.for_each(|_: Value, v: Value| {
            f(T::from_lua(v, &self.lua).map_err(anyhow::Error::from)).map_err(lua_err)
        })?)
    }
}

impl<T: FromLua + IntoLua> Sequence<T> {
    pub fn push(&self, t: T) -> Result<()> {
        Ok(self.t.push(t)?)
    }

    pub fn pop(&self) -> Result<T> {
        Ok(self.t.pop()?)
    }
}

pub fn value_to_json(v: &Value) -> serde_json::Value {
    thread_local! {
        static CTX: RefCell<FxHashMap<usize, String>> = RefCell::new(FxHashMap::default());
    }
    fn inner(
        ctx: &mut FxHashMap<usize, String>,
        key: Option<&str>,
        v: &Value,
    ) -> serde_json::Value {
        use serde_json::{json, Map, Value as JVal};
        match v {
            Value::Nil => JVal::Null,
            Value::Boolean(b) => json!(b),
            Value::LightUserData(_) => json!("<LightUserData>"),
            Value::Integer(i) => json!(*i),
            Value::Number(i) => json!(*i),
            Value::UserData(_) => json!("<UserData>"),
            Value::String(s) => json!(s),
            Value::Function(_) => json!("<Function>"),
            Value::Thread(_) => json!("<Thread>"),
            Value::Other(_) => json!("<Other>"),
            Value::Error(e) => json!(format!("{e}")),
            Value::Table(tbl) => {
                let address = tbl.to_pointer() as usize;
                match ctx.entry(address) {
                    Entry::Occupied(e) => {
                        json!(format!("<Table(0x{:x} {})>", address, e.get()))
                    }
                    Entry::Vacant(e) => {
                        e.insert(String::from(key.unwrap_or("Root")));
                        let mut map = Map::new();
                        for pair in tbl.clone().pairs::<Value, Value>() {
                            let (k, v) = pair.unwrap();
                            let k = match inner(ctx, None, &k) {
                                JVal::String(s) => s,
                                v => v.to_string(),
                            };
                            let v = inner(ctx, Some(k.as_str()), &v);
                            map.insert(k, v);
                        }
                        JVal::Object(map)
                    }
                }
            }
        }
    }
    CTX.with_borrow_mut(|ctx| {
        let r = inner(ctx, None, v);
        ctx.clear();
        r
    })
}

pub fn centroid2d(points: impl IntoIterator<Item = Vector2>) -> Vector2 {
    let (n, sum) =
        points.into_iter().fold((0, Vector2::new(0., 0.)), |(n, c), p| (n + 1, c + p));
    if n == 0 {
        sum
    } else {
        sum / (n as f64)
    }
}

pub fn centroid3d(points: impl IntoIterator<Item = Vector3>) -> Vector3 {
    let (n, sum) = points
        .into_iter()
        .fold((0, Vector3::new(0., 0., 0.)), |(n, c), p| (n + 1, c + p));
    if n == 0 {
        sum
    } else {
        sum / (n as f64)
    }
}

/// Rotate a collection of points in 2d space around their center
/// point keeping the relative orientations of the points
/// constant. The angle is in radians. General about the underlying
/// point container type
pub fn rotate2d_gen<T, F: Fn(&mut T) -> &mut Vector2>(
    angle: f64,
    points: &mut [T],
    f: F,
) {
    let centroid = centroid2d(points.into_iter().map(|t| *f(t)));
    let sin = angle.sin();
    let cos = angle.cos();
    for t in points {
        let p = f(t);
        *p -= centroid;
        let x = p.x;
        let y = p.y;
        p.x = x * cos - y * sin;
        p.y = x * sin + y * cos;
        *p += centroid
    }
}

/// Rotate a collection of points in 2d space around their center point
/// keeping the relative orientations of the points constant. The angle
/// is in radians.
pub fn rotate2d(angle: f64, points: &mut [Vector2]) {
    rotate2d_gen(angle, points, |x| x)
}

/// return a unit vector pointing in the specified direction. angle is in radians
pub fn pointing_towards2(angle: f64) -> Vector2 {
    let sin = angle.sin();
    let cos = angle.cos();
    // dcs coords are reversed x is north/south y is east/west
    Vector2::new(cos, sin).normalize()
}

pub fn normal2(v: Vector2) -> Vector2 {
    Vector2::new(v.y, -v.x)
}

/// Same as rotate2d, but construct and return a vec containing the rotated points
/// in the same order as the they appear in the input slice.
pub fn rotate2d_vec(angle: f64, points: &[Vector2]) -> Vec<Vector2> {
    let mut points = Vec::from_iter(points.into_iter().map(|p| *p));
    rotate2d(angle, &mut points);
    points
}

pub fn radians_to_degrees(radians: f64) -> f64 {
    radians * (180. / std::f64::consts::PI)
}

pub fn degrees_to_radians(degrees: f64) -> f64 {
    degrees * (std::f64::consts::PI / 180.)
}

/// change the heading (in radians) by the specified amount, adjusting if it rotates through 0/2 pi.
/// e.g. `change_heading(3/2 pi, pi) -> pi / 2 not 5 / 2 pi`
pub fn change_heading(heading: f64, change: f64) -> f64 {
    const PI2: f64 = std::f64::consts::PI * 2.;
    let change = if change.abs() > PI2 { change % PI2 } else { change };
    let res = heading + change;
    if res > PI2 {
        res - PI2
    } else if res < 0. {
        res + PI2
    } else {
        res
    }
}

pub fn azumith2d(v: Vector2) -> f64 {
    let az = v.y.atan2(v.x);
    if az < 0. {
        az + 2. * std::f64::consts::PI
    } else {
        az
    }
}

pub fn azumith2d_to(from: Vector2, to: Vector2) -> f64 {
    azumith2d(to - from)
}

pub fn azumith3d(v: Vector3) -> f64 {
    let az = v.z.atan2(v.x);
    if az < 0. {
        az + 2. * std::f64::consts::PI
    } else {
        az
    }
}

pub fn azumith3d_to(from: Vector3, to: Vector3) -> f64 {
    azumith3d(to - from)
}
