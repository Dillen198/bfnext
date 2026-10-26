//! Data Transfer Cartridges baked into the mission. The dashboard's Planner
//! exports native DCS cartridges (`.dtc`, JSON `{name, type, data}`); a
//! folder of them handed to `bftools miz --dtc` is copied into the .miz as
//! `DTC/<name>.dtc` and assigned, pre-loaded, to every player slot of that
//! aircraft type -- static Client slots and dynamic-slot templates alike. A
//! pilot then gets the campaign's presets (radios, countermeasures, ...) the
//! moment they take the slot, with no DTC page to open.
//!
//! The unit side is what the Mission Editor writes for "Pre-load DTC":
//! `DTC = { AutoLoad = true, Cartridges = { { name = <name>, default = true } } }`
//! on the unit, and an empty `DTC = {}` on its group. A cartridge's nav data
//! only loads on the map named in its `data.terrain`, so cartridges for
//! another map are skipped with a warning rather than embedded to do nothing.

use anyhow::{bail, Context, Result};
use log::{info, warn};
use mlua::{Lua, Table, Value};
use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

/// One cartridge from the folder, ready to embed.
#[derive(Debug, Clone)]
pub(crate) struct Cartridge {
    pub(crate) name: std::string::String,
    /// DCS unit type it is for ("F-16C_50", "MiG-29 Fulcrum", ...).
    pub(crate) typ: std::string::String,
    pub(crate) source: PathBuf,
}

/// Names DCS's DTC manager refuses (it checks `[*/?<>|\\:"]`).
fn valid_name(n: &str) -> bool {
    !n.trim().is_empty() && n.trim() == n && !n.chars().any(|c| "*/?<>|\\:\"".contains(c) || c.is_control())
}

/// Read every `.dtc` in `dir`, keeping those that load on `theatre`.
pub(crate) fn load_dir(dir: &Path, theatre: &str) -> Result<Vec<Cartridge>> {
    let mut out = Vec::new();
    let mut entries: Vec<_> = fs::read_dir(dir)
        .with_context(|| format!("reading cartridge folder {dir:?}"))?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|x| x.eq_ignore_ascii_case("dtc")))
        .collect();
    entries.sort();
    for path in entries {
        let text = fs::read_to_string(&path).with_context(|| format!("reading {path:?}"))?;
        let v: serde_json::Value = serde_json::from_str(text.trim_start_matches('\u{feff}'))
            .with_context(|| format!("{path:?} is not a DCS cartridge (JSON)"))?;
        let name = v.get("name").and_then(|x| x.as_str()).unwrap_or_default().to_owned();
        let typ = v.get("type").and_then(|x| x.as_str()).unwrap_or_default().to_owned();
        let inner = v.pointer("/data/type").and_then(|x| x.as_str()).unwrap_or_default();
        if !valid_name(&name) || typ.is_empty() {
            bail!("{path:?}: a cartridge needs a plain \"name\" and a \"type\"")
        }
        if inner != typ {
            bail!("{path:?}: data.type {inner:?} is not the cartridge type {typ:?}; DCS would load defaults")
        }
        // The F-14's cartridge has no terrain: it loads anywhere.
        if let Some(t) = v.pointer("/data/terrain").and_then(|x| x.as_str()) {
            if !t.is_empty() && t != theatre {
                warn!("{path:?}: made for the {t} map, this mission is on {theatre}; skipped");
                continue;
            }
        }
        if out.iter().any(|c: &Cartridge| c.name == name) {
            bail!("{path:?}: two cartridges are named {name:?}; DCS keys them by name")
        }
        out.push(Cartridge { name, typ, source: path });
    }
    Ok(out)
}

/// Put the cartridges on every player slot of their type. Returns how many
/// units got one, per type.
pub(crate) fn assign(lua: &Lua, mission: &Table, carts: &[Cartridge]) -> Result<BTreeMap<std::string::String, usize>> {
    let mut by_type: BTreeMap<&str, Vec<&Cartridge>> = BTreeMap::new();
    for c in carts {
        by_type.entry(c.typ.as_str()).or_default().push(c);
    }
    let mut counts = BTreeMap::new();
    let Ok(coalitions) = mission.raw_get::<_, Table>("coalition") else { return Ok(counts) };
    for coa in coalitions.pairs::<Value, Table>() {
        let (_, coa) = coa?;
        let Ok(countries) = coa.raw_get::<_, Table>("country") else { continue };
        for country in countries.pairs::<Value, Table>() {
            let (_, country) = country?;
            for category in ["plane", "helicopter"] {
                let Ok(cat) = country.raw_get::<_, Table>(category) else { continue };
                let Ok(groups) = cat.raw_get::<_, Table>("group") else { continue };
                for group in groups.pairs::<Value, Table>() {
                    let (_, group) = group?;
                    let dynamic = group.raw_get::<_, Option<bool>>("dynSpawnTemplate")?.unwrap_or(false);
                    let Ok(units) = group.raw_get::<_, Table>("units") else { continue };
                    let mut any = false;
                    for unit in units.pairs::<Value, Table>() {
                        let (_, unit) = unit?;
                        let typ: std::string::String = unit.raw_get::<_, Option<std::string::String>>("type")?.unwrap_or_default();
                        let skill: std::string::String = unit.raw_get::<_, Option<std::string::String>>("skill")?.unwrap_or_default();
                        let player = skill == "Client" || skill == "Player" || dynamic;
                        let Some(list) = by_type.get(typ.as_str()) else { continue };
                        if !player {
                            continue;
                        }
                        // Add to what the mission maker already assigned rather
                        // than replace it: keep their cartridges, their default
                        // and their AutoLoad choice.
                        let dtc = match unit.raw_get::<_, Option<Table>>("DTC")? {
                            Some(t) => t,
                            None => {
                                let t = lua.create_table()?;
                                t.raw_set("AutoLoad", true)?;
                                t
                            }
                        };
                        let cartridges = match dtc.raw_get::<_, Option<Table>>("Cartridges")? {
                            Some(t) => t,
                            None => lua.create_table()?,
                        };
                        let mut names = Vec::new();
                        let mut has_default = false;
                        for e in cartridges.clone().sequence_values::<Table>() {
                            let e = e?;
                            names.push(e.raw_get::<_, Option<std::string::String>>("name")?.unwrap_or_default());
                            has_default |= e.raw_get::<_, Option<bool>>("default")?.unwrap_or(false);
                        }
                        for c in list {
                            if names.iter().any(|n| *n == c.name) {
                                continue;
                            }
                            let e = lua.create_table()?;
                            e.raw_set("name", c.name.as_str())?;
                            e.raw_set("default", !has_default)?;
                            has_default = true;
                            cartridges.raw_set(cartridges.raw_len() + 1, e)?;
                            names.push(c.name.clone());
                        }
                        if dtc.raw_get::<_, Option<bool>>("AutoLoad")?.is_none() {
                            dtc.raw_set("AutoLoad", true)?;
                        }
                        dtc.raw_set("Cartridges", cartridges)?;
                        unit.raw_set("DTC", dtc)?;
                        *counts.entry(typ.clone()).or_insert(0) += 1;
                        any = true;
                    }
                    if any && group.raw_get::<_, Option<Table>>("DTC")?.is_none() {
                        group.raw_set("DTC", lua.create_table()?)?;
                    }
                }
            }
        }
    }
    Ok(counts)
}

/// Load `dir`, assign the cartridges in `mission`, and copy each into the
/// unpacked miz at `root`. Returns the archive entries to add.
pub(crate) fn embed(lua: &Lua, mission: &Table, root: &Path, dir: &Path) -> Result<Vec<(std::string::String, PathBuf)>> {
    let theatre: std::string::String = mission.raw_get::<_, Option<std::string::String>>("theatre")?.unwrap_or_default();
    let carts = load_dir(dir, &theatre)?;
    if carts.is_empty() {
        warn!("--dtc {dir:?}: no cartridge for the {theatre} map");
        return Ok(vec![]);
    }
    let counts = assign(lua, mission, &carts)?;
    let mut entries = Vec::new();
    for c in &carts {
        let n = counts.get(&c.typ).copied().unwrap_or(0);
        if n == 0 {
            warn!("cartridge {:?} ({}): no player slot of that type in the mission; embedded anyway", c.name, c.typ);
        }
        let entry = format!("DTC/{}.dtc", c.name);
        let dest = root.join("DTC").join(format!("{}.dtc", c.name));
        fs::create_dir_all(dest.parent().unwrap()).context("creating the miz DTC folder")?;
        fs::copy(&c.source, &dest).with_context(|| format!("copying {:?} into the miz", c.source))?;
        info!("cartridge {:?} pre-loaded on {n} {} slot(s)", c.name, c.typ);
        entries.push((entry, dest));
    }
    Ok(entries)
}

#[cfg(test)]
mod tests {
    use super::*;

    const MISSION: &str = r#"
        mission = {
          theatre = "Caucasus",
          coalition = {
            blue = { country = {
              [1] = { plane = { group = {
                [1] = { units = {
                  [1] = { type = "F-16C_50", skill = "Client" },
                  [2] = { type = "F-16C_50", skill = "Excellent" },
                } },
                [2] = { dynSpawnTemplate = true, units = { [1] = { type = "F-16C_50", skill = "Excellent" } } },
              } },
              helicopter = { group = { [1] = { units = { [1] = { type = "AH-64D_BLK_II", skill = "Player" } } } } } },
            } },
          },
        }
    "#;

    fn write(dir: &Path, file: &str, body: &str) {
        fs::write(dir.join(file), body).unwrap();
    }

    fn scratch() -> PathBuf {
        let d = std::env::temp_dir().join(format!("bftools-dtc-test-{}-{}", std::process::id(), rand_suffix()));
        fs::create_dir_all(&d).unwrap();
        d
    }

    fn rand_suffix() -> u128 {
        std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos()
    }

    #[test]
    fn embeds_on_player_slots_of_the_type_and_map() {
        let lua = Lua::new();
        lua.load(MISSION).exec().unwrap();
        let mission: Table = lua.globals().get("mission").unwrap();
        let dir = scratch();
        let root = scratch();
        write(&dir, "a.dtc", r#"{"name":"Viper comms","type":"F-16C_50","data":{"type":"F-16C_50","terrain":"Caucasus"}}"#);
        write(&dir, "b.dtc", r#"{"name":"Viper Syria","type":"F-16C_50","data":{"type":"F-16C_50","terrain":"Syria"}}"#);
        write(&dir, "c.dtc", r#"{"name":"Apache","type":"AH-64D_BLK_II","data":{"type":"AH-64D_BLK_II","terrain":"Caucasus"}}"#);
        write(&dir, "notes.txt", "ignored");
        let entries = embed(&lua, &mission, &root, &dir).unwrap();
        let names: Vec<_> = entries.iter().map(|e| e.0.clone()).collect();
        assert_eq!(names, ["DTC/Viper comms.dtc", "DTC/Apache.dtc"]);
        assert!(root.join("DTC").join("Viper comms.dtc").exists());

        let groups: Table = lua.load("return mission.coalition.blue.country[1].plane.group").eval().unwrap();
        let g1: Table = groups.get(1).unwrap();
        let u1: Table = g1.get::<_, Table>("units").unwrap().get(1).unwrap();
        let dtc: Table = u1.get("DTC").unwrap();
        assert!(dtc.get::<_, bool>("AutoLoad").unwrap());
        let first: Table = dtc.get::<_, Table>("Cartridges").unwrap().get(1).unwrap();
        assert_eq!(first.get::<_, std::string::String>("name").unwrap(), "Viper comms");
        assert!(first.get::<_, bool>("default").unwrap());
        // An AI wingman gets nothing; its group still gets the empty group table.
        let u2: Table = g1.get::<_, Table>("units").unwrap().get(2).unwrap();
        assert!(u2.get::<_, Option<Table>>("DTC").unwrap().is_none());
        assert!(g1.get::<_, Option<Table>>("DTC").unwrap().is_some());
        // A dynamic-slot template counts as a player slot.
        let dynu: Table = lua.load("return mission.coalition.blue.country[1].plane.group[2].units[1]").eval().unwrap();
        assert!(dynu.get::<_, Option<Table>>("DTC").unwrap().is_some());
        let heli: Table = lua.load("return mission.coalition.blue.country[1].helicopter.group[1].units[1]").eval().unwrap();
        assert!(heli.get::<_, Option<Table>>("DTC").unwrap().is_some());
        let _ = fs::remove_dir_all(dir);
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn refuses_a_broken_cartridge() {
        let dir = scratch();
        write(&dir, "x.dtc", r#"{"name":"X","type":"F-16C_50","data":{"type":"FA-18C_hornet"}}"#);
        assert!(load_dir(&dir, "Caucasus").is_err());
        write(&dir, "x.dtc", r#"{"name":"a/b","type":"F-16C_50","data":{"type":"F-16C_50"}}"#);
        assert!(load_dir(&dir, "Caucasus").is_err());
        write(&dir, "x.dtc", r#"{"name":"Tomcat","type":"F-14BU","data":{"type":"F-14BU"}}"#);
        assert_eq!(load_dir(&dir, "Syria").unwrap().len(), 1);
        let _ = fs::remove_dir_all(dir);
    }
}

#[cfg(test)]
mod merge_tests {
    use super::*;

    #[test]
    fn keeps_cartridges_the_mission_already_assigns() {
        let lua = Lua::new();
        lua.load(
            r#"mission = { theatre = "Caucasus", coalition = { blue = { country = { [1] = { plane = { group = { [1] = { units = {
                [1] = { type = "F-16C_50", skill = "Client",
                        DTC = { AutoLoad = false, Cartridges = { [1] = { name = "Squadron", default = true } } } },
            } } } } } } } } }"#,
        )
        .exec()
        .unwrap();
        let mission: Table = lua.globals().get("mission").unwrap();
        let carts = vec![
            Cartridge { name: "Campaign".into(), typ: "F-16C_50".into(), source: PathBuf::new() },
            Cartridge { name: "Squadron".into(), typ: "F-16C_50".into(), source: PathBuf::new() },
        ];
        assign(&lua, &mission, &carts).unwrap();
        let dtc: Table = lua.load("return mission.coalition.blue.country[1].plane.group[1].units[1].DTC").eval().unwrap();
        // The mission maker's AutoLoad choice and default stay.
        assert!(!dtc.get::<_, bool>("AutoLoad").unwrap());
        let list: Table = dtc.get("Cartridges").unwrap();
        assert_eq!(list.raw_len(), 2);
        let first: Table = list.get(1).unwrap();
        let second: Table = list.get(2).unwrap();
        assert_eq!(first.get::<_, std::string::String>("name").unwrap(), "Squadron");
        assert!(first.get::<_, bool>("default").unwrap());
        assert_eq!(second.get::<_, std::string::String>("name").unwrap(), "Campaign");
        assert!(!second.get::<_, bool>("default").unwrap());
    }
}
