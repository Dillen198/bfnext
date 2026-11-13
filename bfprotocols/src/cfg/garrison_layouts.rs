//! Garrisons laid out unit by unit (`Cfg::garrison_layouts`).
//!
//! The usual way to garrison an objective is a `G<template>` trigger zone
//! inside it: the engine spawns the template's group at the zone's centre,
//! every unit at its offset in the template and the group facing north. A
//! layout instead gives every unit its own position and heading, the way
//! `special_sam_sites` does, so a tank platoon can stand in line facing the
//! enemy, guns can ring the point they cover and trucks can park behind, each
//! vehicle on ground of its own. Ships work the same way: a group of ships
//! (its units' types classified as boats) spawns as a ship group and must
//! stand in water.
//!
//! Each entry is one group of one objective, with a Red and a Blue version
//! (either may be empty). Both are registered at start, like a G zone's two
//! groups: the side that does not hold the objective starts dead, and comes
//! alive when it captures it. Respawn and repair put every unit back where
//! the layout put it.
//!
//! Layouts and G zones can be mixed; a campaign can garrison some objectives
//! one way and some the other.

use super::*;

/// What a garrison group is, for objective health and the logistics that
/// repair it. Mirrors the class a G zone's template name gives its group
/// (`RAAA` -> AAA, `BARMOR` -> ARMOR, ...).
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, schemars::JsonSchema,
)]
#[serde(rename_all = "UPPERCASE")]
pub enum GarrisonClass {
    Logi,
    Aaa,
    Lr,
    Mr,
    Sr,
    Armor,
    Services,
    Infantry,
    /// Ships (a port's warship and transports): spawned as a ship group,
    /// and they must stand in water.
    Naval,
    Other,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct GarrisonLayoutGroupCfg {
    /// The objective this group defends (its name without the zone prefix,
    /// e.g. "Kutaisi").
    pub objective: String,
    pub class: GarrisonClass,
    /// A label for logs and errors only (the build tools use the G zone the
    /// layout replaced).
    #[serde(default)]
    pub name: Option<String>,
    /// The group as Red holds it: absolute positions and headings.
    #[serde(default)]
    pub red_units: Vec<SpecialSamUnitCfg>,
    /// The group as Blue holds it.
    #[serde(default)]
    pub blue_units: Vec<SpecialSamUnitCfg>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct GarrisonLayoutsCfg {
    /// DCS country the Red groups spawn as.
    #[serde(default = "default_red_country")]
    pub red_country: Country,
    /// DCS country the Blue groups spawn as.
    #[serde(default = "default_blue_country")]
    pub blue_country: Country,
    #[serde(default)]
    pub groups: Vec<GarrisonLayoutGroupCfg>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_a_layout() {
        let cfg: GarrisonLayoutsCfg = serde_json::from_str(
            r#"{
                "groups": [{
                    "objective": "Kutaisi",
                    "class": "AAA",
                    "name": "GAAA-1",
                    "red_units": [{"type": "KS-19", "pos": {"x": 1.0, "y": 2.0}, "heading": 0.5}],
                    "blue_units": []
                }]
            }"#,
        )
        .unwrap();
        assert_eq!(cfg.red_country, Country::CJTF_RED);
        assert_eq!(cfg.blue_country, Country::CJTF_BLUE);
        let g = &cfg.groups[0];
        assert_eq!(g.class, GarrisonClass::Aaa);
        assert_eq!(g.red_units[0].typ.as_str(), "KS-19");
        assert!(g.blue_units.is_empty());
    }

    /// The block in `miz/SAMPLE_CFG.json` reads.
    #[test]
    fn the_sample_block_parses() {
        let raw = include_str!("../../../miz/SAMPLE_CFG.json");
        let v: serde_json::Value =
            serde_json::from_str(raw.trim_start_matches('\u{feff}')).unwrap();
        let c: GarrisonLayoutsCfg =
            serde_json::from_value(v["garrison_layouts"].clone()).unwrap();
        assert_eq!(c.red_country, Country::CJTF_RED);
        assert!(c.groups.is_empty());
    }

    #[test]
    fn class_names_match_the_template_stems() {
        for (s, c) in [
            ("LOGI", GarrisonClass::Logi),
            ("AAA", GarrisonClass::Aaa),
            ("LR", GarrisonClass::Lr),
            ("MR", GarrisonClass::Mr),
            ("SR", GarrisonClass::Sr),
            ("ARMOR", GarrisonClass::Armor),
            ("SERVICES", GarrisonClass::Services),
            ("INFANTRY", GarrisonClass::Infantry),
            ("NAVAL", GarrisonClass::Naval),
            ("OTHER", GarrisonClass::Other),
        ] {
            let parsed: GarrisonClass =
                serde_json::from_str(&format!("\"{s}\"")).unwrap();
            assert_eq!(parsed, c);
        }
    }
}
