/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use crate::{
    as_tbl, coalition::Side, env::miz::GroupId, wrap_f, wrapped_table, LuaEnv, MizLua, String,
};
use anyhow::Result;
use compact_str::format_compact;
use mlua::{prelude::*, Value};
use serde_derive::Serialize;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ItemPath(Vec<String>);

impl<'lua> IntoLua<'lua> for ItemPath {
    fn into_lua(self, lua: &'lua Lua) -> LuaResult<Value<'lua>> {
        let tbl = lua.create_table()?;
        for s in self.0 {
            tbl.raw_push(s)?
        }
        Ok(Value::Table(tbl))
    }
}

impl<'lua> FromLua<'lua> for ItemPath {
    fn from_lua(value: Value<'lua>, lua: &'lua Lua) -> LuaResult<Self> {
        let tbl = LuaTable::from_lua(value, lua)?;
        let mut res = Vec::new();
        for v in tbl.sequence_values() {
            let v = v?;
            res.push(String::from_lua(v, lua)?);
        }
        Ok(Self(res))
    }
}

macro_rules! item {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(ItemPath);

        impl<'lua> IntoLua<'lua> for $name {
            fn into_lua(self, lua: &'lua Lua) -> LuaResult<Value<'lua>> {
                self.0.into_lua(lua)
            }
        }

        impl<'lua> FromLua<'lua> for $name {
            fn from_lua(value: Value<'lua>, lua: &'lua Lua) -> LuaResult<Self> {
                Ok(Self(ItemPath::from_lua(value, lua)?))
            }
        }

        impl From<Vec<String>> for $name {
            fn from(v: Vec<String>) -> Self {
                Self(ItemPath(v))
            }
        }

        impl Into<Vec<String>> for $name {
            fn into(self) -> Vec<String> {
                (self.0).0
            }
        }
    };
}

item!(SubMenu);
item!(CoalitionSubMenu);
item!(GroupSubMenu);
item!(CommandItem);
item!(CoalitionCommandItem);
item!(GroupCommandItem);

wrapped_table!(MissionCommands, None);

impl<'lua> MissionCommands<'lua> {
    pub fn singleton(lua: MizLua<'lua>) -> Result<Self> {
        Ok(lua.inner().globals().raw_get("missionCommands")?)
    }

    pub fn add_submenu(&self, name: String, parent: Option<SubMenu>) -> Result<SubMenu> {
        Ok(self.call_function("addSubMenu", (name, parent))?)
    }

    pub fn add_command<F, A>(
        &self,
        name: String,
        parent: Option<SubMenu>,
        f: F,
        arg: A,
    ) -> Result<CommandItem>
    where
        F: Fn(MizLua, A) -> Result<()> + 'static,
        A: IntoLua<'lua> + FromLua<'lua>,
    {
        let msg = format_compact!("command {:?},{name}", parent);
        let f = self.lua.create_function(move |lua, arg: A| {
            wrap_f(msg.as_str(), MizLua(lua), |lua| f(lua, arg))
        })?;
        Ok(self.call_function("addCommand", (name, parent, f, arg))?)
    }

    pub fn remove_submenu(&self, menu: SubMenu) -> Result<()> {
        Ok(self.call_function("removeItem", menu)?)
    }

    pub fn remove_command(&self, item: CommandItem) -> Result<()> {
        Ok(self.call_function("removeItem", item)?)
    }

    pub fn add_submenu_for_coalition(
        &self,
        side: Side,
        name: String,
        parent: Option<CoalitionSubMenu>,
    ) -> Result<CoalitionSubMenu> {
        Ok(self.call_function("addSubMenuForCoalition", (side, name, parent))?)
    }

    pub fn add_command_for_coalition<F, A>(
        &self,
        side: Side,
        name: String,
        parent: Option<CoalitionSubMenu>,
        f: F,
        arg: A,
    ) -> Result<CoalitionCommandItem>
    where
        F: Fn(MizLua, A) -> Result<()> + 'static,
        A: IntoLua<'lua> + FromLua<'lua>,
    {
        let msg = format_compact!("coa cmd {:?},{name}", parent);
        let f = self.lua.create_function(move |lua, arg: A| {
            wrap_f(msg.as_str(), MizLua(lua), |lua| f(lua, arg))
        })?;
        Ok(self.call_function("addCommandForCoalition", (side, name, parent, f, arg))?)
    }

    pub fn remove_submenu_for_coalition(&self, side: Side, menu: CoalitionSubMenu) -> Result<()> {
        Ok(self.call_function("removeItemForCoalition", (side, menu))?)
    }

    pub fn remove_command_for_coalition(&self, side: Side, item: CoalitionCommandItem) -> Result<()> {
        Ok(self.call_function("removeItemForCoalition", (side, item))?)
    }

    pub fn add_submenu_for_group(
        &self,
        group: GroupId,
        name: String,
        parent: Option<GroupSubMenu>,
    ) -> Result<GroupSubMenu> {
        let menu: GroupSubMenu =
            self.call_function("addSubMenuForGroup", (group, name.clone(), parent))?;
        let path: Vec<String> = menu.clone().into();
        mirror_record(self.lua.inner(), group, &path, &name, None)?;
        Ok(menu)
    }

    pub fn add_command_for_group<F, A>(
        &self,
        group: GroupId,
        name: String,
        parent: Option<GroupSubMenu>,
        f: F,
        arg: A,
    ) -> Result<GroupCommandItem>
    where
        F: Fn(MizLua, A) -> Result<()> + 'static,
        A: IntoLua<'lua> + FromLua<'lua>,
    {
        let msg = format_compact!("grp cmd {:?}, {name}", parent);
        let f = self.lua.create_function(move |lua, arg: A| {
            wrap_f(msg.as_str(), MizLua(lua), |lua| f(lua, arg))
        })?;
        // The argument is converted once and reused, so the copy handed to DCS
        // and the copy kept in the mirror are the same value -- invoking the
        // item from the mirror is then indistinguishable from clicking it in
        // the F10 menu.
        let arg = arg.into_lua(self.lua.inner())?;
        let item: GroupCommandItem = self.call_function(
            "addCommandForGroup",
            (group, name.clone(), parent, f.clone(), arg.clone()),
        )?;
        let path: Vec<String> = item.clone().into();
        mirror_record(self.lua.inner(), group, &path, &name, Some((f, arg)))?;
        Ok(item)
    }

    pub fn remove_submenu_for_group(&self, group: GroupId, menu: GroupSubMenu) -> Result<()> {
        let path: Vec<String> = menu.clone().into();
        mirror_forget(self.lua.inner(), group, &path)?;
        Ok(self.call_function("removeItemForGroup", (group, menu))?)
    }

    pub fn remove_command_for_group(&self, group: GroupId, item: GroupCommandItem) -> Result<()> {
        let path: Vec<String> = item.clone().into();
        mirror_forget(self.lua.inner(), group, &path)?;
        Ok(self.call_function("removeItemForGroup", (group, item))?)
    }

    pub fn clear_all_menus(&self) -> Result<()> {
        mirror_forget_all(self.lua.inner())?;
        Ok(self.call_function("removeItem", ())?)
    }
}

// ── F10 menu mirror ──────────────────────────────────────────────────
//
// DCS's `missionCommands` is write-only: you can add and remove items, but
// there is no way to ask it what a group's menu currently contains, and the
// Rust closure behind an item disappears into the Lua callback DCS holds. That
// makes the F10 menu impossible to render anywhere else -- an out-of-game UI
// would have to re-implement every menu by hand and then drift out of step
// with it forever.
//
// So every group menu item registered through this module is also recorded in
// a plain Lua side table, `_G.__dcso3_menu_mirror`, together with the exact
// callback function and argument DCS itself was handed. `mirrored_menu` reads
// that tree back, and `invoke_mirrored_menu` calls one entry's stored callback
// with its stored argument -- which is the same call DCS makes when the player
// clicks the item, so a mirrored menu cannot diverge from the real one.
//
// Every menu, present and future, is covered automatically: there is nothing
// to keep in sync because there is only one registration path.
//
// Scope: group menus only. Coalition-wide and global menus are not mirrored
// (nothing needs them yet, and they have no single player to attribute to).

/// Global holding the mirror. A Lua table rather than Rust state because the
/// callbacks themselves are Lua values whose lifetime is the Lua state's.
const MIRROR_GLOBAL: &str = "__dcso3_menu_mirror";

/// ASCII unit separator -- joins a path into one table key. Chosen because it
/// cannot occur in a menu label.
const PATH_SEP: char = '\u{1f}';

fn path_key(path: &[String]) -> std::string::String {
    let mut out = std::string::String::new();
    for (i, seg) in path.iter().enumerate() {
        if i > 0 {
            out.push(PATH_SEP);
        }
        out.push_str(seg.as_str());
    }
    out
}

fn mirror_root(lua: &Lua) -> LuaResult<LuaTable<'_>> {
    let globals = lua.globals();
    match globals.raw_get::<_, Option<LuaTable>>(MIRROR_GLOBAL)? {
        Some(t) => Ok(t),
        None => {
            let t = lua.create_table()?;
            globals.raw_set(MIRROR_GLOBAL, t.clone())?;
            Ok(t)
        }
    }
}

/// `{ order = <counter>, items = { [pathkey] = entry } }` for one group. The
/// counter preserves registration order, which is the order the player sees in
/// the F10 menu and therefore the order a mirrored menu must render in.
fn mirror_group(lua: &Lua, group: GroupId) -> LuaResult<LuaTable<'_>> {
    let root = mirror_root(lua)?;
    match root.raw_get::<_, Option<LuaTable>>(group)? {
        Some(t) => Ok(t),
        None => {
            let t = lua.create_table()?;
            t.raw_set("order", 0i64)?;
            t.raw_set("items", lua.create_table()?)?;
            root.raw_set(group, t.clone())?;
            Ok(t)
        }
    }
}

fn mirror_record<'lua>(
    lua: &'lua Lua,
    group: GroupId,
    path: &[String],
    name: &String,
    handler: Option<(LuaFunction<'lua>, Value<'lua>)>,
) -> LuaResult<()> {
    let g = mirror_group(lua, group)?;
    let order = g.raw_get::<_, i64>("order")? + 1;
    g.raw_set("order", order)?;

    let seq = lua.create_table()?;
    for seg in path {
        seq.raw_push(seg.clone())?;
    }

    let entry = lua.create_table()?;
    entry.raw_set("name", name.clone())?;
    entry.raw_set("path", seq)?;
    entry.raw_set("order", order)?;
    match handler {
        Some((f, arg)) => {
            entry.raw_set("command", true)?;
            entry.raw_set("fn", f)?;
            entry.raw_set("arg", arg)?;
        }
        None => entry.raw_set("command", false)?,
    }

    g.raw_get::<_, LuaTable>("items")?
        .raw_set(path_key(path), entry)?;
    Ok(())
}

/// Drop `path` and everything beneath it. Removing a submenu in DCS removes
/// its whole subtree, so the mirror has to do the same or it keeps offering
/// items that no longer exist.
fn mirror_forget(lua: &Lua, group: GroupId, path: &[String]) -> LuaResult<()> {
    let items = mirror_group(lua, group)?.raw_get::<_, LuaTable>("items")?;
    let key = path_key(path);
    let mut prefix = key.clone();
    prefix.push(PATH_SEP);

    let mut doomed: Vec<std::string::String> = Vec::new();
    for pair in items.clone().pairs::<std::string::String, Value>() {
        let (k, _) = pair?;
        if k == key || k.starts_with(&prefix) {
            doomed.push(k);
        }
    }
    for k in doomed {
        items.raw_remove(k)?;
    }
    Ok(())
}

fn mirror_forget_all(lua: &Lua) -> LuaResult<()> {
    lua.globals().raw_set(MIRROR_GLOBAL, Value::Nil)
}

/// One entry in a group's mirrored F10 menu.
#[derive(Debug, Clone, Serialize)]
pub struct MirroredMenuItem {
    /// Full path from the group's menu root, the way DCS addresses it. Also
    /// this item's identity: pass it back to [`invoke_mirrored_menu`].
    pub path: Vec<std::string::String>,
    /// The label the player sees.
    pub name: std::string::String,
    /// True for a clickable command, false for a submenu.
    pub command: bool,
    /// Registration order, ascending. Matches the order shown in F10.
    pub order: i64,
}

/// Read back the F10 menu currently registered for `group`, in the order the
/// player sees it. Empty when that group has no menu.
pub fn mirrored_menu(lua: MizLua, group: GroupId) -> Result<Vec<MirroredMenuItem>> {
    let lua = lua.inner();
    let root = mirror_root(lua)?;
    let Some(g) = root.raw_get::<_, Option<LuaTable>>(group)? else {
        return Ok(Vec::new());
    };
    let items = g.raw_get::<_, LuaTable>("items")?;

    let mut out = Vec::new();
    for pair in items.pairs::<std::string::String, LuaTable>() {
        let (_, entry) = pair?;
        let mut path = Vec::new();
        for seg in entry.raw_get::<_, LuaTable>("path")?.sequence_values::<String>() {
            path.push(seg?.as_str().to_string());
        }
        out.push(MirroredMenuItem {
            path,
            name: entry.raw_get::<_, String>("name")?.as_str().to_string(),
            command: entry.raw_get::<_, bool>("command")?,
            order: entry.raw_get::<_, i64>("order")?,
        });
    }
    out.sort_by_key(|i| i.order);
    Ok(out)
}

/// Fire the command at `path` in `group`'s menu, exactly as if the player had
/// clicked it. Returns `Ok(false)` if there is no such item or it is a submenu
/// rather than a command.
///
/// The handler runs immediately and synchronously, and it is the same handler
/// DCS holds -- including its `Context::get_mut()` access -- so callers must
/// not be relying on a `&mut Context` borrow surviving across this call.
pub fn invoke_mirrored_menu(lua: MizLua, group: GroupId, path: &[std::string::String]) -> Result<bool> {
    let inner = lua.inner();
    let root = mirror_root(inner)?;
    let Some(g) = root.raw_get::<_, Option<LuaTable>>(group)? else {
        return Ok(false);
    };
    let key: Vec<String> = path.iter().map(|s| String::from(s.as_str())).collect();
    let Some(entry) = g
        .raw_get::<_, LuaTable>("items")?
        .raw_get::<_, Option<LuaTable>>(path_key(&key))?
    else {
        return Ok(false);
    };
    if !entry.raw_get::<_, bool>("command")? {
        return Ok(false);
    }
    let f: LuaFunction = entry.raw_get("fn")?;
    let arg: Value = entry.raw_get("arg")?;
    f.call::<_, ()>(arg)?;
    Ok(true)
}
