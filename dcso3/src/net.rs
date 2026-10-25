/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use super::{as_tbl, coalition::Side, String};
use crate::{
    simple_enum, wrapped_prim, wrapped_table, LuaEnv, Sequence,
};
use anyhow::Result;
use core::fmt;
use mlua::{prelude::*, Value};
use serde_derive::Serialize;
use std::{ops::Deref, str::FromStr};

simple_enum!(PlayerStat, u8, [
    Car => 2,
    Crash => 1,
    Eject => 7,
    ExtraAllyAAA => 17,
    ExtraAllyFighters => 14,
    ExtraAllySAM => 16,
    ExtraAllyTransports => 15,
    ExtraAllyTroops => 18,
    ExtraAllyCoalition => 19,
    ExtraEnemyAAA => 12,
    ExtraEnemyFighters => 9,
    ExtraEnemySAM => 11,
    ExtraEnemyTransports => 10,
    ExtraEnemyTroops => 13,
    Land => 6,
    Num => 20,
    OldScore => 8,
    Ping => 0,
    Plane => 3,
    Score => 5,
    Ship => 4
]);

wrapped_prim!(PlayerId, i64, Copy, Hash);

impl FromStr for PlayerId {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(Self(s.parse::<i64>()?))
    }
}

impl fmt::Display for PlayerId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub use dcso3_core::net::{SlotId, Ucid};

wrapped_table!(PlayerInfo, None);

impl<'lua> PlayerInfo<'lua> {
    pub fn id(&self) -> Result<PlayerId> {
        Ok(self.t.raw_get("id")?)
    }

    pub fn name(&self) -> Result<Option<String>> {
        Ok(self.t.raw_get("name")?)
    }

    pub fn side(&self) -> Result<Side> {
        Ok(self.t.raw_get("side")?)
    }

    pub fn slot(&self) -> Result<SlotId> {
        Ok(self.t.raw_get("slot")?)
    }

    pub fn ping(&self) -> Result<f32> {
        Ok(self.t.raw_get("ping")?)
    }

    pub fn ip(&self) -> Result<Option<String>> {
        Ok(self.t.raw_get("ipaddr")?)
    }

    pub fn ucid(&self) -> Result<Option<Ucid>> {
        Ok(self.t.raw_get("ucid")?)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DcsLuaEnvironment {
    /// aka hooks
    Server,
    Mission,
    Config,
    Export,
}

impl<'lua> IntoLua<'lua> for DcsLuaEnvironment {
    fn into_lua(self, lua: &'lua Lua) -> LuaResult<Value<'lua>> {
        Ok(Value::String(match self {
            Self::Server => lua.create_string("server"),
            Self::Mission => lua.create_string("mission"),
            Self::Config => lua.create_string("config"),
            Self::Export => lua.create_string("export"),
        }?))
    }
}

wrapped_table!(Net, None);

impl<'lua> Net<'lua> {
    pub fn singleton<L: LuaEnv<'lua>>(lua: L) -> Result<Self> {
        Ok(lua.inner().globals().raw_get("net")?)
    }

    pub fn send_chat(&self, message: String, all: bool) -> Result<()> {
        Ok(self.t.call_function("send_chat", (message, all))?)
    }

    pub fn send_chat_to(
        &self,
        message: String,
        player: PlayerId,
        from_id: Option<PlayerId>,
    ) -> Result<()> {
        Ok(self
            .t
            .call_function("send_chat_to", (message, player, from_id))?)
    }

    pub fn get_player_list(&self) -> Result<Sequence<'lua, PlayerId>> {
        Ok(self.t.call_function("get_player_list", ())?)
    }

    pub fn get_my_player_id(&self) -> Result<PlayerId> {
        Ok(self.t.call_function("get_my_player_id", ())?)
    }

    pub fn get_server_id(&self) -> Result<PlayerId> {
        Ok(self.t.call_function("get_server_id", ())?)
    }

    pub fn get_player_info(&self, id: PlayerId) -> Result<PlayerInfo<'_>> {
        Ok(self.t.call_function("get_player_info", id)?)
    }

    pub fn kick(&self, id: PlayerId, message: String) -> Result<()> {
        Ok(self.t.call_function("kick", (id, message))?)
    }

    pub fn get_stat(&self, id: PlayerId, stat: PlayerStat) -> Result<i64> {
        Ok(self.t.call_function("get_stat", (id, stat))?)
    }

    pub fn get_name(&self, id: PlayerId) -> Result<String> {
        Ok(self.t.call_function("get_name", id)?)
    }

    pub fn get_slot(&self, id: PlayerId) -> Result<(Side, SlotId)> {
        Ok(self.t.call_function("get_slot", id)?)
    }

    pub fn force_player_slot(&self, id: PlayerId, side: Side, slot: SlotId) -> Result<()> {
        Ok(self
            .t
            .call_function("force_player_slot", (id, side, slot))?)
    }

    pub fn lua2json<T: IntoLua<'lua>>(&self, v: T) -> Result<String> {
        Ok(self.t.call_function("lua2json", v)?)
    }

    pub fn json2lua<T: FromLua<'lua>>(&self, v: String) -> Result<T> {
        Ok(self.t.call_function("json2lua", v)?)
    }

    /// Returns whatever the chunk returns, which is nil for a statement.
    pub fn dostring_in(
        &self,
        state: DcsLuaEnvironment,
        dostring: String,
    ) -> Result<Option<String>> {
        Ok(self.t.call_function("dostring_in", (state, dostring))?)
    }

    pub fn log(&self, message: String) -> Result<()> {
        Ok(self.t.call_function("log", message)?)
    }
}
