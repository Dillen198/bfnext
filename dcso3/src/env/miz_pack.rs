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
use mlua::{ChunkMode, Lua, LuaOptions, StdLib, Table, Value};
use std::{
    fmt::Display,
    fs, io,
    panic::AssertUnwindSafe,
    path::Path,
};
use zip::{read::ZipArchive, write::FileOptions, ZipWriter};

/// A fresh Lua state fit for evaluating .miz entries with
/// [`read_table_from_miz`].
///
/// A .miz entry is a DCS-authored *script*, not inert data: loading it runs
/// whatever it contains. `Lua::new()` hands that script `io`, `os` and
/// `package`, so a doctored mission could write files or spawn processes on
/// the server the moment bflib or bftools opened it. Mission, options,
/// warehouses, dictionary and mapResource files are nothing but one big table
/// constructor and need no library at all, so none is loaded. (Lua 5.1's base
/// library is always present; it has no way to write files or run programs.)
pub fn scratch_lua() -> Result<Lua> {
    Lua::new_with(StdLib::NONE, LuaOptions::default()).context("creating scratch lua state")
}

/// Read the Lua table assigned to `entry_name` (e.g. "mission", "options",
/// "warehouses") inside a .miz zip file, using `lua` to evaluate it. `lua`
/// must be a scratch Lua state safe to run an arbitrary DCS-authored script
/// in (see [`scratch_lua`]) - the entry's top level `<entry_name> = { ... }`
/// assignment is executed as-is and then read back out of `lua`'s globals.
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
    // Bytes, not a String: Lua strings are byte strings, and a mission whose
    // names were typed in a non-UTF-8 codepage (cp1251 is common) is still a
    // perfectly loadable mission. read_to_string refused the whole file.
    let mut content = Vec::new();
    io::Read::read_to_end(&mut entry, &mut content)
        .with_context(|| format!("reading {entry_name} from {miz_path:?}"))?;
    drop(entry);
    // Text only: mlua would otherwise accept a precompiled bytecode chunk,
    // and Lua 5.1 does not verify bytecode -- a crafted one is memory
    // corruption, not a load error.
    lua.load(content)
        .set_mode(ChunkMode::Text)
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
    let tmp_path = miz_path.with_extension("miz.tmp");
    let res = write_rewritten_miz(miz_path, &tmp_path, entry_name, new_content).and_then(|()| {
        fs::rename(&tmp_path, miz_path)
            .with_context(|| format!("replacing {miz_path:?} with {tmp_path:?}"))
    });
    // Every failure path, not just "entry not found": a half-written temp
    // file left next to the mission is at best litter, and at worst picked
    // up by someone as a mission in its own right.
    if res.is_err() {
        let _ = fs::remove_file(&tmp_path);
    }
    res
}

/// Build the rewritten archive at `tmp_path`. Every file handle it opens is
/// closed by the time it returns, so the caller can rename or remove the
/// temp file (Windows refuses both while a handle is open).
fn write_rewritten_miz(
    miz_path: &Path,
    tmp_path: &Path,
    entry_name: &str,
    new_content: &str,
) -> Result<()> {
    let file = fs::File::open(miz_path)
        .with_context(|| format!("opening {miz_path:?}"))?;
    let mut archive =
        ZipArchive::new(file).with_context(|| format!("unzipping {miz_path:?}"))?;
    let tmp_file = fs::File::create(tmp_path)
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
    if !found {
        bail!("{entry_name} entry not found in {miz_path:?}, mission file left untouched")
    }
    let file = writer
        .finish()
        .context("finishing zip")?
        .into_inner()
        .map_err(|e| e.into_error())
        .with_context(|| format!("flushing {tmp_path:?}"))?;
    // The rename is only atomic with respect to the directory entry. Without
    // this the new name can reach the disk before the data does, and a power
    // cut or hard kill right after the restart that follows (exactly when
    // bflib calls this) leaves a zero-filled mission in place of both the old
    // file and the new one.
    file.sync_all()
        .with_context(|| format!("syncing {tmp_path:?}"))?;
    Ok(())
}

/// Write `s` as a Lua string literal, escaping everything that would otherwise
/// end the literal early or start a new one.
///
/// This used to be a bare `write!("\"{}\"")`. A single `"` or `\` anywhere in a
/// mission -- a unit name, a briefing line, a waypoint comment, a livery path --
/// therefore produced a file that is not valid Lua, and the resulting .miz
/// could not be re-opened by anything, bftools included. The damage is also
/// invisible at the point of corruption: the lexer runs on past the stray
/// quote and reports "unfinished string" wherever it eventually gives up, which
/// can be hundreds of thousands of lines away from the real culprit.
///
/// A literal newline in a string is the same class of bug -- Lua does not allow
/// one inside a quoted literal at all.
///
/// `s` is the raw bytes of the Lua string. Valid UTF-8 is written through as
/// text, so Cyrillic briefings stay readable in the file. Bytes that are not
/// valid UTF-8 -- a unit name typed in a cp1251 editor, say -- used to go
/// through `to_string_lossy` and come out as U+FFFD, silently and permanently
/// replacing the original name in every mission bftools or live weather ever
/// rewrote. They are written as `\ddd` decimal escapes instead, which Lua
/// turns back into exactly the original byte.
fn write_lua_string(f: &mut std::fmt::Formatter<'_>, s: &[u8]) -> std::fmt::Result {
    use std::fmt::Write as _;
    f.write_char('"')?;
    for chunk in s.utf8_chunks() {
        for c in chunk.valid().chars() {
            match c {
                '"' => f.write_str("\\\"")?,
                '\\' => f.write_str("\\\\")?,
                '\n' => f.write_str("\\n")?,
                '\r' => f.write_str("\\r")?,
                '\t' => f.write_str("\\t")?,
                // Any other control character, NUL included: a raw NUL
                // truncates the chunk at the C boundary and every lexer
                // downstream of it sees a file that just stops. Always three
                // digits, so a following digit can't be read as part of it.
                c if (c as u32) < 0x20 || c as u32 == 0x7f => write!(f, "\\{:03}", c as u32)?,
                c => f.write_char(c)?,
            }
        }
        for b in chunk.invalid() {
            write!(f, "\\{:03}", b)?;
        }
    }
    f.write_char('"')
}

/// Write `n` as a Lua expression that evaluates back to the same double.
///
/// Rust's `Display` renders the non-finite values as `NaN`, `inf` and `-inf`.
/// Lua reads the first two as (unset) global variables, i.e. nil, so the field
/// vanished from the table on the next load; `-inf` is arithmetic on nil, so
/// the whole file failed to load. `math.huge` is not a usable spelling either
/// -- a mission is evaluated with no libraries loaded (see `scratch_lua`), so
/// `math` is itself nil there. Plain arithmetic needs nothing: Lua 5.1
/// deliberately does not constant-fold a division by zero, so these are
/// evaluated at load time to exactly NaN / +inf / -inf.
fn write_lua_number(f: &mut std::fmt::Formatter<'_>, n: f64) -> std::fmt::Result {
    if n.is_nan() {
        f.write_str("(0/0)")
    } else if n == f64::INFINITY {
        f.write_str("(1/0)")
    } else if n == f64::NEG_INFINITY {
        f.write_str("(-1/0)")
    } else {
        write!(f, "{n}")
    }
}

struct LuaSerVal<'lua> {
    value: Value<'lua>,
    level: usize,
}

impl<'lua> LuaSerVal<'lua> {
    fn indented(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.level {
            write!(f, " ")?;
        }
        Ok(())
    }
}

impl<'lua> Display for LuaSerVal<'lua> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write_lua_number(f, *n),
            Value::String(s) => write_lua_string(f, s.as_bytes()),
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
                        // Only the 1..=max run was already written by the
                        // sequence pass. A key of 0 (or a negative one) is
                        // NOT part of that run -- skipping it here silently
                        // dropped it from the output, which is how a DCS
                        // table indexed from zero, like the F10 view
                        // options' visibleUnitLayersMask, lost its first
                        // entry on every round trip.
                        if let Some(i) = k.as_integer() {
                            if i >= 1 && i <= max {
                                return Ok(());
                            }
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
///
/// Holes in integer-keyed tables are written as they are, with explicit
/// `[k]` keys. The writer cannot tell a sparse array from a table keyed by
/// ids (`warehouses.airports[<airdrome id>]`, a unit layer mask indexed from
/// 0), so renumbering is left to callers that know which tables are arrays --
/// see bftools' `compact_mission_arrays`.
pub fn serialize_to_lua<'lua>(key: &str, value: Value<'lua>) -> Result<std::string::String> {
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

#[cfg(test)]
mod tests {
    use super::*;

    struct Str<'a>(&'a [u8]);
    impl Display for Str<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write_lua_string(f, self.0)
        }
    }

    struct Num(f64);
    impl Display for Num {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write_lua_number(f, self.0)
        }
    }

    #[test]
    fn string_escapes() {
        assert_eq!(Str(b"plain").to_string(), r#""plain""#);
        assert_eq!(Str(b"a\"b\\c").to_string(), r#""a\"b\\c""#);
        assert_eq!(Str(b"l1\nl2\r\t").to_string(), r#""l1\nl2\r\t""#);
        // NUL followed by a digit: the escape must stay three digits wide.
        assert_eq!(Str(b"\x001").to_string(), r#""\0001""#);
        // Valid UTF-8 goes through untouched.
        assert_eq!(Str("Тбилиси".as_bytes()).to_string(), "\"Тбилиси\"");
        // cp1251 "Тб" is not UTF-8 and must survive as the same bytes.
        assert_eq!(Str(b"\xd2\xe1 x").to_string(), r#""\210\225 x""#);
    }

    #[test]
    fn number_emission() {
        assert_eq!(Num(1.5).to_string(), "1.5");
        assert_eq!(Num(-3.0).to_string(), "-3");
        assert_eq!(Num(f64::NAN).to_string(), "(0/0)");
        assert_eq!(Num(f64::INFINITY).to_string(), "(1/0)");
        assert_eq!(Num(f64::NEG_INFINITY).to_string(), "(-1/0)");
    }

    #[test]
    fn round_trip_through_lua() -> Result<()> {
        let lua = scratch_lua()?;
        let t = lua.create_table()?;
        let odd: &[u8] = b"q\"\\\n\x00\xd2\xe1\x7f";
        t.raw_set("s", lua.create_string(odd)?)?;
        t.raw_set("nan", f64::NAN)?;
        t.raw_set("pinf", f64::INFINITY)?;
        t.raw_set("ninf", f64::NEG_INFINITY)?;
        t.raw_set("pi", std::f64::consts::PI)?;
        let src = serialize_to_lua("rt", Value::Table(t))?;
        lua.load(src.as_str()).set_mode(ChunkMode::Text).exec()?;
        let back: Table = lua.globals().raw_get("rt")?;
        assert_eq!(back.raw_get::<_, mlua::String>("s")?.as_bytes(), odd);
        assert!(back.raw_get::<_, f64>("nan")?.is_nan());
        assert_eq!(back.raw_get::<_, f64>("pinf")?, f64::INFINITY);
        assert_eq!(back.raw_get::<_, f64>("ninf")?, f64::NEG_INFINITY);
        assert_eq!(back.raw_get::<_, f64>("pi")?, std::f64::consts::PI);
        Ok(())
    }

    #[test]
    fn scratch_lua_has_no_io() -> Result<()> {
        let lua = scratch_lua()?;
        for lib in ["io", "os", "package", "debug"] {
            assert!(
                matches!(lua.globals().raw_get::<_, Value>(lib)?, Value::Nil),
                "{lib} is loaded"
            );
        }
        Ok(())
    }
}
