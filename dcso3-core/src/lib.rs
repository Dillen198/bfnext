/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

//! The part of dcso3 that does not need Lua.
//!
//! Plain data types shared by everything that talks about a DCS mission:
//! the ones bfprotocols puts on the wire and bfdb stores (ucids, slots,
//! sides, countries, perf histograms, ids). Each module mirrors the dcso3
//! module the type lives in, and dcso3 re-exports them all at those paths,
//! so `dcso3::net::Ucid` and `dcso3_core::net::Ucid` are the same type.
//!
//! With the `lua` feature (dcso3 always turns it on) the types also get
//! their `FromLua` / `IntoLua` impls. Without it this crate has no Lua
//! dependency at all, which is what lets bfdb build and run without
//! DCS's lua.dll.

use compact_str::CompactString;
pub use schemars;
use serde_derive::{Deserialize, Serialize};
use std::{
    borrow::Borrow,
    ops::{Deref, DerefMut},
};

pub mod coalition;
pub mod controller;
pub mod coord;
pub mod country;
pub mod env;
pub mod net;
pub mod object;
pub mod perf;
pub mod unit;
pub mod warehouse;
pub mod weapon;

#[cfg(feature = "lua")]
pub mod lua;

/// What the exported macros expand to, so a crate invoking them does not
/// need these as its own dependencies.
#[doc(hidden)]
pub mod __private {
    pub use anyhow;
    #[cfg(feature = "lua")]
    pub use mlua;
    pub use paste;
    pub use serde;
    pub use serde_derive;
}

pub type Vector2 = nalgebra::base::Vector2<f64>;
pub type Vector3 = nalgebra::base::Vector3<f64>;

#[derive(
    Debug,
    Clone,
    Default,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
    schemars::JsonSchema,
)]
pub struct String(#[schemars(with = "std::string::String")] CompactString);

impl std::fmt::Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for String {
    type Target = compact_str::CompactString;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for String {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<str> for String {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Borrow<str> for String {
    fn borrow(&self) -> &str {
        self.0.borrow()
    }
}

impl From<&str> for String {
    fn from(value: &str) -> Self {
        Self(CompactString::from(value))
    }
}

impl From<std::string::String> for String {
    fn from(value: std::string::String) -> Self {
        Self(CompactString::from(value))
    }
}

impl From<CompactString> for String {
    fn from(value: CompactString) -> Self {
        Self(value)
    }
}

/// A process-wide id sequence: `new()` hands out the next id, and
/// deserializing an id bumps the sequence past it so loaded and new ids
/// never collide.
#[macro_export]
macro_rules! atomic_id {
    ($name:ident) => {
        $crate::__private::paste::paste! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $name(i64);

            impl From<i64> for $name {
                fn from(x: i64) -> Self {
                    Self(x)
                }
            }

            impl std::str::FromStr for $name {
                type Err = $crate::__private::anyhow::Error;

                fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
                    Ok(Self(s.parse::<i64>()?))
                }
            }

            impl $crate::__private::serde::Serialize for $name {
                fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
                where
                    S: $crate::__private::serde::Serializer,
                {
                    serializer.serialize_i64(self.0)
                }
            }

            pub struct [<$name Visitor>];

            impl<'de> $crate::__private::serde::de::Visitor<'de> for [<$name Visitor>] {
                type Value = $name;

                fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(formatter, "a i64")
                }

                fn visit_i64<E>(self, v: i64) -> std::result::Result<Self::Value, E>
                where
                    E: $crate::__private::serde::de::Error,
                {
                    $name::update_max(v);
                    Ok($name(v))
                }

                fn visit_u64<E>(self, v: u64) -> std::result::Result<Self::Value, E>
                where
                    E: $crate::__private::serde::de::Error,
                {
                    let v = v as i64;
                    $name::update_max(v);
                    Ok($name(v))
                }
            }

            impl<'de> $crate::__private::serde::Deserialize<'de> for $name {
                fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
                where
                    D: $crate::__private::serde::Deserializer<'de>,
                {
                    deserializer.deserialize_i64([<$name Visitor>])
                }
            }

            static [<MAX_ $name:upper _ID>]: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(0);

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.0)
                }
            }

            impl std::default::Default for $name {
                fn default() -> Self {
                    Self::new()
                }
            }

            $crate::__lua_newtype!($name, i64);

            impl $name {
                pub fn new() -> Self {
                    Self([<MAX_ $name:upper _ID>].fetch_add(1, std::sync::atomic::Ordering::Relaxed))
                }

                fn update_max(n: i64) {
                    // saturating: a corrupt/hostile id of i64::MAX must
                    // not wrap the sequence to i64::MIN and start
                    // handing out ids that collide with existing ones
                    [<MAX_ $name:upper _ID>].fetch_max(
                        n.saturating_add(1),
                        std::sync::atomic::Ordering::Relaxed,
                    );
                }

                pub fn inner(&self) -> i64 {
                    self.0
                }

                pub fn setseq(i: i64) {
                    [<MAX_ $name:upper _ID>].store(i, std::sync::atomic::Ordering::Relaxed)
                }

                pub fn seq() -> i64 {
                    [<MAX_ $name:upper _ID>].load(std::sync::atomic::Ordering::Relaxed)
                }

                pub fn zero() -> Self {
                    Self(0)
                }
            }
        }
    }
}

/// A newtype over a primitive, converted to and from Lua as that primitive.
#[macro_export]
macro_rules! wrapped_prim {
    ($name:ident, $type:ty) => {
        $crate::wrapped_prim!($name, $type, );
    };
    ($name:ident, $type:ty, $($extra_derives:ident),*) => {
        #[derive(
            Debug, Clone, PartialEq, Eq, PartialOrd, Ord,
            $crate::__private::serde_derive::Serialize,
            $crate::__private::serde_derive::Deserialize,
            $($extra_derives),*
        )]
        pub struct $name($type);

        impl $name {
            pub fn inner(self) -> $type {
                self.0
            }
        }

        impl From<$type> for $name {
            fn from(t: $type) -> Self {
                Self(t)
            }
        }

        $crate::__lua_newtype!($name, $type);
    };
}

/// An enum over the numeric ids DCS uses. Decoding an id this build does
/// not know is an error; for ids DCS adds to in updates use
/// `simple_enum_unknown!` instead.
#[macro_export]
macro_rules! simple_enum {
    ($name:ident, $repr:ident, [$($case:ident => $num:literal),+]) => {
        #[derive(
            Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash,
            $crate::__private::serde_derive::Serialize,
            $crate::__private::serde_derive::Deserialize,
            schemars::JsonSchema
        )]
        #[allow(non_camel_case_types)]
        #[repr($repr)]
        pub enum $name {
            $($case = $num),+
        }

        $crate::__lua_simple_enum!($name, $repr, [$($case => $num),+]);
    };
}

/// `simple_enum!` plus an `Unknown(n)` catch-all, for ids DCS extends in
/// updates (countries, categories, liquid types). The plain form fails the
/// whole decode on an id it doesn't know, which took down every read that
/// touched e.g. a new country or unit category. `Unknown` must stay the LAST
/// variant: some of these are persisted with positional bincode (bfdb), and
/// appending a variant keeps every existing variant index unchanged.
///
/// A data-carrying variant rules out `#[repr]` discriminants and `as` casts,
/// so the numeric id is `id()`, and ordering is implemented on it to keep
/// the same order the discriminant-based derive gave.
#[macro_export]
macro_rules! simple_enum_unknown {
    ($name:ident, $repr:ident, [$($case:ident => $num:literal),+]) => {
        #[derive(
            Debug, Clone, Copy, PartialEq, Eq, Hash,
            $crate::__private::serde_derive::Serialize,
            $crate::__private::serde_derive::Deserialize,
            schemars::JsonSchema
        )]
        #[allow(non_camel_case_types)]
        pub enum $name {
            $($case),+,
            /// An id this build doesn't know about yet.
            Unknown($repr)
        }

        impl $name {
            /// The numeric id DCS uses for this value.
            pub fn id(self) -> $repr {
                match self {
                    $(Self::$case => $num),+,
                    Self::Unknown(n) => n,
                }
            }

            /// The value for a numeric id, `Unknown` if this build doesn't
            /// know it.
            pub fn from_id(n: $repr) -> Self {
                match n {
                    $($num => Self::$case),+,
                    n => Self::Unknown(n)
                }
            }
        }

        impl PartialOrd for $name {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for $name {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                let key = |v: &Self| (v.id(), matches!(v, Self::Unknown(_)));
                key(self).cmp(&key(other))
            }
        }

        $crate::__lua_simple_enum_unknown!($name, $repr);
    };
}

/// An enum over the strings DCS uses, with `Custom` for any other string.
/// The optional second list adds aliases that decode to an existing case.
#[macro_export]
macro_rules! string_enum {
    ($name:ident, $repr:ident, [$($case:ident => $str:literal),+]) => {
        $crate::string_enum!($name, $repr, [$($case => $str),+], []);
    };
    ($name:ident,
     $repr:ident,
     [$($case:ident => $str:literal),+],
     [$($altcase:ident => $altstr:literal),*]) => {
        #[derive(
            Debug, Clone, PartialEq, Eq, Hash,
            $crate::__private::serde_derive::Serialize,
            $crate::__private::serde_derive::Deserialize,
            schemars::JsonSchema
        )]
        #[allow(non_camel_case_types)]
        #[repr($repr)]
        pub enum $name {
            $($case),+,
            Custom($crate::String)
        }

        $crate::__lua_string_enum!($name, [$($case => $str),+], [$($altcase => $altstr),*]);
    };
}

// The Lua halves of the macros above. Which definition exists is decided by
// this crate's `lua` feature, so a type defined with these macros converts to
// and from Lua exactly when dcso3 (which turns `lua` on) is in the build.

#[cfg(feature = "lua")]
#[doc(hidden)]
#[macro_export]
macro_rules! __lua_newtype {
    ($name:ident, $type:ty) => {
        impl $crate::__private::mlua::FromLua for $name {
            fn from_lua(
                value: $crate::__private::mlua::Value,
                lua: &$crate::__private::mlua::Lua,
            ) -> $crate::__private::mlua::Result<Self> {
                Ok(Self(<$type as $crate::__private::mlua::FromLua>::from_lua(
                    value, lua,
                )?))
            }
        }

        impl $crate::__private::mlua::IntoLua for $name {
            fn into_lua(
                self,
                lua: &$crate::__private::mlua::Lua,
            ) -> $crate::__private::mlua::Result<$crate::__private::mlua::Value> {
                $crate::__private::mlua::IntoLua::into_lua(self.0, lua)
            }
        }
    };
}

#[cfg(feature = "lua")]
#[doc(hidden)]
#[macro_export]
macro_rules! __lua_simple_enum {
    ($name:ident, $repr:ident, [$($case:ident => $num:literal),+]) => {
        impl $crate::__private::mlua::FromLua for $name {
            fn from_lua(
                value: $crate::__private::mlua::Value,
                lua: &$crate::__private::mlua::Lua,
            ) -> $crate::__private::mlua::Result<Self> {
                Ok(match <$repr as $crate::__private::mlua::FromLua>::from_lua(value, lua)? {
                    $($num => Self::$case),+,
                    _ => return Err($crate::lua::cvt_err(stringify!($name)))
                })
            }
        }

        impl $crate::__private::mlua::IntoLua for $name {
            fn into_lua(
                self,
                _lua: &$crate::__private::mlua::Lua,
            ) -> $crate::__private::mlua::Result<$crate::__private::mlua::Value> {
                Ok($crate::__private::mlua::Value::Integer(self as i64))
            }
        }
    };
}

#[cfg(feature = "lua")]
#[doc(hidden)]
#[macro_export]
macro_rules! __lua_simple_enum_unknown {
    ($name:ident, $repr:ident) => {
        impl $crate::__private::mlua::FromLua for $name {
            fn from_lua(
                value: $crate::__private::mlua::Value,
                lua: &$crate::__private::mlua::Lua,
            ) -> $crate::__private::mlua::Result<Self> {
                Ok(Self::from_id(<$repr as $crate::__private::mlua::FromLua>::from_lua(
                    value, lua,
                )?))
            }
        }

        impl $crate::__private::mlua::IntoLua for $name {
            fn into_lua(
                self,
                _lua: &$crate::__private::mlua::Lua,
            ) -> $crate::__private::mlua::Result<$crate::__private::mlua::Value> {
                Ok($crate::__private::mlua::Value::Integer(self.id() as i64))
            }
        }
    };
}

#[cfg(feature = "lua")]
#[doc(hidden)]
#[macro_export]
macro_rules! __lua_string_enum {
    ($name:ident,
     [$($case:ident => $str:literal),+],
     [$($altcase:ident => $altstr:literal),*]) => {
        impl $crate::__private::mlua::FromLua for $name {
            fn from_lua(
                value: $crate::__private::mlua::Value,
                lua: &$crate::__private::mlua::Lua,
            ) -> $crate::__private::mlua::Result<Self> {
                let s = <$crate::String as $crate::__private::mlua::FromLua>::from_lua(value, lua)?;
                Ok(match s.as_str() {
                    $($str => Self::$case,)+
                    $($altstr => Self::$altcase,)*
                    _ => Self::Custom(s)
                })
            }
        }

        impl $crate::__private::mlua::IntoLua for $name {
            fn into_lua(
                self,
                lua: &$crate::__private::mlua::Lua,
            ) -> $crate::__private::mlua::Result<$crate::__private::mlua::Value> {
                Ok($crate::__private::mlua::Value::String(match self {
                    $(Self::$case => lua.create_string($str)?),+,
                    Self::Custom(s) => lua.create_string(s.as_str())?
                }))
            }
        }
    };
}

#[cfg(not(feature = "lua"))]
#[doc(hidden)]
#[macro_export]
macro_rules! __lua_newtype {
    ($($t:tt)*) => {};
}

#[cfg(not(feature = "lua"))]
#[doc(hidden)]
#[macro_export]
macro_rules! __lua_simple_enum {
    ($($t:tt)*) => {};
}

#[cfg(not(feature = "lua"))]
#[doc(hidden)]
#[macro_export]
macro_rules! __lua_simple_enum_unknown {
    ($($t:tt)*) => {};
}

#[cfg(not(feature = "lua"))]
#[doc(hidden)]
#[macro_export]
macro_rules! __lua_string_enum {
    ($($t:tt)*) => {};
}
