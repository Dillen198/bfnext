/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

//! Helpers for reading and rewriting the Lua table entries stored inside a
//! DCS .miz file (itself just a zip archive). Shared between bftools, which
//! builds missions from templates, and bflib, which can rewrite the
//! currently-loaded mission's weather/time in place before a restart.

use anyhow::{bail, Context, Result};
use mlua::{Lua, Table, Value};
use std::{
    fmt::Display,
    fs, io,
    panic::AssertUnwindSafe,
    path::Path,
};
use zip::{read::ZipArchive, write::FileOptions, ZipWriter};

/// Read the Lua table assigned to `entry_name` (e.g. "mission", "options",
/// "warehouses") inside a .miz zip file, using `lua` to evaluate it. `lua`
/// must be a scratch Lua state safe to run an arbitrary DCS-authored script
/// in - the entry's top level `<entry_name> = { ... }` assignment is
/// executed as-is and then read back out of `lua`'s globals.
pub fn read_table_from_miz<'lua>(
    lua: &'lua Lua,
    miz_path: &Path,
    entry_name: &str,
) -> Result<Table<'lua>> {
    let file = fs::File::open(miz_path)
        .with_context(|| format!("opening {miz_path:?}"))?;
    let mut archive =
        ZipArchive::new(file).with_context(|| format!("unzipping {miz_path:?}"))?;
    let mut entry = archive
        .by_name(entry_name)
        .with_context(|| format!("{entry_name} entry not found in {miz_path:?}"))?;
    let mut content = std::string::String::new();
    io::Read::read_to_string(&mut entry, &mut content)
        .with_context(|| format!("reading {entry_name} from {miz_path:?}"))?;
    drop(entry);
    lua.load(&content)
        .exec()
        .with_context(|| format!("loading {entry_name} into lua"))?;
    lua.globals()
        .raw_get(entry_name)
        .with_context(|| format!("extracting {entry_name}"))
}

/// Rewrite a single top level entry inside a .miz zip file in place, leaving
/// every other entry byte for byte unchanged. Writes to a temp file next to
/// `miz_path` and atomically renames it over the original once complete, so
/// a failure partway through never leaves a corrupt mission file behind.
pub fn rewrite_entry_in_miz(miz_path: &Path, entry_name: &str, new_content: &str) -> Result<()> {
    let file = fs::File::open(miz_path)
        .with_context(|| format!("opening {miz_path:?}"))?;
    let mut archive =
        ZipArchive::new(file).with_context(|| format!("unzipping {miz_path:?}"))?;
    let tmp_path = miz_path.with_extension("miz.tmp");
    let tmp_file = fs::File::create(&tmp_path)
        .with_context(|| format!("creating {tmp_path:?}"))?;
    let mut writer = ZipWriter::new(io::BufWriter::new(tmp_file));
    let mut found = false;
    for i in 0..archive.len() {
        let mut entry = archive
            .by_index(i)
            .with_context(|| format!("getting zip entry {i}"))?;
        let name = entry.name().to_string();
        writer
            .start_file(&name, FileOptions::default())
            .with_context(|| format!("starting zip entry {name}"))?;
        if name == entry_name {
            found = true;
            io::Write::write_all(&mut writer, new_content.as_bytes())
                .with_context(|| format!("writing {entry_name}"))?;
        } else {
            io::copy(&mut entry, &mut writer)
                .with_context(|| format!("copying entry {name}"))?;
        }
    }
    writer.finish().context("finishing zip")?;
    if !found {
        let _ = fs::remove_file(&tmp_path);
        bail!("{entry_name} entry not found in {miz_path:?}, mission file left untouched")
    }
    fs::rename(&tmp_path, miz_path)
        .with_context(|| format!("replacing {miz_path:?} with {tmp_path:?}"))?;
    Ok(())
}

struct LuaSerVal {
    value: Value<'static>,
    level: usize,
}

impl LuaSerVal {
    fn indented(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.level {
            write!(f, " ")?;
        }
        Ok(())
    }
}

impl Display for LuaSerVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "\"{}\"", s.to_string_lossy()),
            Value::Table(tbl) => {
                macro_rules! write_elt {
                    ($k:expr, $v:expr) => {
                        let k = LuaSerVal {
                            value: $k,
                            level: self.level + 4,
                        };
                        let v = LuaSerVal {
                            value: $v,
                            level: self.level + 4,
                        };
                        k.indented(f).unwrap();
                        if v.value.is_table() {
                            write!(f, "[{k}] = {v}, -- end of [{k}]\n").unwrap();
                        } else {
                            write!(f, "[{k}] = {v},\n").unwrap();
                        }
                    };
                }
                let mut seq_max: Option<i64> = None;
                write!(f, "\n")?;
                self.indented(f)?;
                write!(f, "{{\n")?;
                if tbl.contains_key(1).unwrap() {
                    for (i, v) in tbl.clone().sequence_values().enumerate() {
                        let i = (i + 1) as i64;
                        let v = v.unwrap();
                        seq_max = Some(i);
                        write_elt!(Value::Integer(i), v);
                    }
                }
                tbl.for_each(|k: Value, v: Value| {
                    if let Some(max) = seq_max {
                        if k.is_integer() && k.as_integer().unwrap() <= max {
                            return Ok(());
                        }
                    }
                    write_elt!(k, v);
                    Ok(())
                })
                .unwrap();
                self.indented(f)?;
                write!(f, "}}")
            }
            Value::Error(_)
            | Value::Function(_)
            | Value::LightUserData(_)
            | Value::Thread(_)
            | Value::UserData(_) => panic!("value type {:?} can't be serialized", self.value),
        }
    }
}

/// Render `key = <value>` as DCS-flavored Lua table source text (the same
/// format DCS itself writes mission/options/warehouses files in).
pub fn serialize_to_lua(key: &str, value: Value<'static>) -> Result<std::string::String> {
    let res = std::panic::catch_unwind(AssertUnwindSafe(move || {
        use std::fmt::Write;
        let mut s = std::string::String::with_capacity(1024 * 1024);
        write!(s, "{key} = {}", LuaSerVal { value, level: 0 })?;
        Ok::<_, anyhow::Error>(s)
    }));
    match res {
        Ok(s) => Ok(s?),
        Err(e) => {
            if let Some(e) = e.downcast_ref::<anyhow::Error>() {
                bail!("{e}");
            }
            if let Some(e) = e.downcast_ref::<&str>() {
                bail!("{e}")
            }
            if let Some(e) = e.downcast_ref::<std::string::String>() {
                bail!("{e}")
            }
            if let Some(e) = e.downcast_ref::<mlua::Error>() {
                bail!("{e}")
            }
            bail!("serialization failed")
        }
    }
}
