use crate::{Context, admin};
use anyhow::Result;
use dcso3::{MizLua, Vector2, coalition::Side};
use dcso3::LuaEnv;
use bfprotocols::db::group::GroupId;

pub fn register(lua: MizLua) -> Result<()> {
    let globals = lua.inner().globals();
    let api = lua.inner().create_table()?;

    api.set("get_objectives", lua.inner().create_function(|_, ()| {
        let ctx = unsafe { Context::get_mut() };
        let objectives = admin::query_objectives(ctx);
        Ok(serde_json::to_string(&objectives).map_err(mlua::Error::external)?)
    })?)?;

    api.set("get_objective", lua.inner().create_function(|_, name: String| {
        let ctx = unsafe { Context::get_mut() };
        let details = admin::query_objective_details(ctx, &name).map_err(mlua::Error::external)?;
        Ok(serde_json::to_string(&details).map_err(mlua::Error::external)?)
    })?)?;

    api.set("get_players", lua.inner().create_function(|_, ()| {
        let ctx = unsafe { Context::get_mut() };
        let players = admin::query_players(ctx);
        Ok(serde_json::to_string(&players).map_err(mlua::Error::external)?)
    })?)?;

    api.set("get_player", lua.inner().create_function(|_, name: String| {
        let ctx = unsafe { Context::get_mut() };
        let player = admin::query_player_details(ctx, &name).map_err(mlua::Error::external)?;
        Ok(serde_json::to_string(&player).map_err(mlua::Error::external)?)
    })?)?;

    api.set("get_groups", lua.inner().create_function(|_, side: Option<String>| {
        let ctx = unsafe { Context::get_mut() };
        let side_enum = side.and_then(|s| s.parse::<Side>().ok());
        let groups = admin::query_groups(ctx, side_enum);
        Ok(serde_json::to_string(&groups).map_err(mlua::Error::external)?)
    })?)?;

    api.set("get_campaign_state", lua.inner().create_function(|_, ()| {
        let ctx = unsafe { Context::get_mut() };
        let state = admin::query_campaign_state(ctx);
        Ok(serde_json::to_string(&state).map_err(mlua::Error::external)?)
    })?)?;

    // Action APIs
    api.set("spawn_deployable", lua.inner().create_function(|lua, (side_str, name, x, y, heading): (String, String, f64, f64, f64)| {
        let ctx = unsafe { Context::get_mut() };
        let side = side_str.parse::<Side>().map_err(mlua::Error::external)?;
        let pos = Vector2::new(x, y);
        let miz_lua = MizLua(lua);
        let gid = admin::api_spawn_deployable(ctx, miz_lua, side, &name, pos, heading).map_err(mlua::Error::external)?;
        Ok(gid.to_string())
    })?)?;

    api.set("spawn_troop", lua.inner().create_function(|lua, (side_str, name, x, y, heading): (String, String, f64, f64, f64)| {
        let ctx = unsafe { Context::get_mut() };
        let side = side_str.parse::<Side>().map_err(mlua::Error::external)?;
        let pos = Vector2::new(x, y);
        let miz_lua = MizLua(lua);
        let gid = admin::api_spawn_troop(ctx, miz_lua, side, &name, pos, heading).map_err(mlua::Error::external)?;
        Ok(gid.to_string())
    })?)?;

    api.set("move_group", lua.inner().create_function(|lua, (id, x, y): (i64, f64, f64)| {
        let ctx = unsafe { Context::get_mut() };
        let gid = GroupId::from(id);
        let pos = Vector2::new(x, y);
        let miz_lua = MizLua(lua);
        admin::api_move_group(ctx, miz_lua, &gid, pos).map_err(mlua::Error::external)?;
        Ok(true)
    })?)?;

    api.set("add_points", lua.inner().create_function(|_, (player, amount, reason): (String, i32, String)| {
        let ctx = unsafe { Context::get_mut() };
        admin::api_add_points(ctx, &player, amount, &reason).map_err(mlua::Error::external)?;
        Ok(true)
    })?)?;

    globals.set("vector_strike", api)?;
    Ok(())
}

pub(crate) fn dispatch_event(lua: dcso3::MizLua, event_type: &str, message: &str) {
    let globals = lua.inner().globals();
    if let Ok(api) = globals.get::<_, mlua::Table>("vector_strike") {
        if let Ok(func) = api.get::<_, mlua::Function>("on_event") {
            if let Ok(tbl) = lua.inner().create_table() {
                let _ = tbl.set("type", event_type);
                let _ = tbl.set("message", message);
                let _ = func.call::<_, ()>(tbl);
            }
        }
    }
}
