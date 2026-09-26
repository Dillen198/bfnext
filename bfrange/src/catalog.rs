// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! What players can spawn, for the range site's spawn page. The same items
//! back the F10 menus, so the website and the cockpit offer the same things.

use crate::{aa, antiship, helo::Helo, ground::Ground};
use bfprotocols::range::{
    cfg::{tanker_type, RangeCfg, TANKER_TYPES},
    CatalogItem, ParamKind, ParamOption, ParamSpec, SpawnCatalog, SpawnCategory,
};

fn choice(key: &str, label: &str, opts: &[(&str, &str)], default: &str) -> ParamSpec {
    ParamSpec {
        key: key.into(),
        label: label.into(),
        kind: ParamKind::Choice {
            options: opts.iter().map(|(v, l)| ParamOption { value: v.to_string(), label: l.to_string() }).collect(),
        },
        default: default.into(),
    }
}

fn choice_owned(key: &str, label: &str, opts: Vec<(String, String)>) -> ParamSpec {
    let default = opts.first().map(|o| o.0.clone()).unwrap_or_default();
    ParamSpec {
        key: key.into(),
        label: label.into(),
        kind: ParamKind::Choice { options: opts.into_iter().map(|(value, label)| ParamOption { value, label }).collect() },
        default,
    }
}

fn number(key: &str, label: &str, min: f64, max: f64, step: f64, unit: &str, default: f64) -> ParamSpec {
    ParamSpec {
        key: key.into(),
        label: label.into(),
        kind: ParamKind::Number { min, max, step, unit: unit.into() },
        default: format!("{default}"),
    }
}

pub const SKILLS: &[(&str, &str)] = &[("Average", "Average"), ("Good", "Good"), ("High", "High"), ("Excellent", "Excellent")];
pub const WEAPONS: &[(&str, &str)] = &[
    ("guns", "Guns only"),
    ("fox2", "Fox 2 (IR missiles)"),
    ("fox1", "Fox 1 (semi-active radar)"),
    ("fox3", "Fox 3 (active radar)"),
];

pub const GROUND_COMPOSITIONS: &[(&str, &str, &[&str])] = &[
    ("armor", "Armour platoon (4x T-72B)", &["T-72B", "T-72B", "T-72B", "T-72B"]),
    ("soft", "Trucks (4x Ural-375)", &["Ural-375", "Ural-375", "Ural-375", "Ural-375"]),
    ("mixed", "Mech. section (2x BMP-2, 2x Ural)", &["BMP-2", "BMP-2", "Ural-375", "Ural-375"]),
    ("shilka", "Convoy with ZSU-23-4", &["ZSU-23-4 Shilka", "Ural-375", "Ural-375", "BTR-80"]),
];

pub const SAMS: &[(&str, &str, &[&str])] = &[
    ("sa8", "SA-8 Osa", &["Osa 9A33 ln"]),
    ("sa15", "SA-15 Tor", &["Tor 9A331"]),
    ("sa6", "SA-6 Kub", &["Kub 1S91 str", "Kub 2P25 ln", "Kub 2P25 ln"]),
    ("sa11", "SA-11 Buk", &["SA-11 Buk SR 9S18M1", "SA-11 Buk CC 9S470M1", "SA-11 Buk LN 9A310M1", "SA-11 Buk LN 9A310M1"]),
    ("sa3", "SA-3 Goa", &["p-19 s-125 sr", "snr s-125 tr", "5p73 s-125 ln", "5p73 s-125 ln"]),
    ("sa2", "SA-2 Guideline", &["p-19 s-125 sr", "SNR_75V", "S_75M_Volhov", "S_75M_Volhov"]),
    ("zsu", "ZSU-23-4 Shilka pair", &["ZSU-23-4 Shilka", "ZSU-23-4 Shilka"]),
];

pub fn build(cfg: &RangeCfg, helo: &Helo, ground: &Ground) -> SpawnCatalog {
    let advs = if cfg.air_to_air.adversaries.is_empty() {
        aa::default_adversaries()
    } else {
        cfg.air_to_air.adversaries.clone()
    };
    let adv_opts: Vec<(String, String)> = advs.iter().map(|a| (a.typ.clone(), a.label.clone())).collect();
    let instructor = |id: &str| cfg.spawn.instructor_only.iter().any(|i| i == id);
    let mut items = vec![
        CatalogItem {
            id: "bfm".into(),
            category: SpawnCategory::AirToAir,
            label: "BFM set-up".into(),
            description: "An AI fighter placed around you for a basic-fighter-manoeuvres set: offensive (you behind it), defensive (it behind you), neutral pass, high perch or head-on.".into(),
            params: vec![
                choice("setup", "Set-up", &[("offensive", "Offensive"), ("defensive", "Defensive"), ("neutral", "Neutral pass"), ("perch", "High perch"), ("head_on", "Head-on")], "offensive"),
                choice_owned("adversary", "Adversary", adv_opts.clone()),
                choice("skill", "Skill", SKILLS, "High"),
                choice("weapons", "Weapons", WEAPONS, "guns"),
                choice("count", "Number", &[("1", "1 v 1"), ("2", "1 v 2")], "1"),
            ],
            instructor_only: instructor("bfm"),
            relative_to_player: true,
        },
        CatalogItem {
            id: "bvr".into(),
            category: SpawnCategory::AirToAir,
            label: "BVR presentation".into(),
            description: "AI fighters presented at range: hot, flanking, beaming, high or low.".into(),
            params: vec![
                choice_owned("adversary", "Adversary", adv_opts.clone()),
                number("range_nm", "Range", 10., 80., 5., "nm", 40.),
                choice("aspect", "Presentation", &[("hot", "Hot"), ("flank", "Flanking"), ("beam", "Beaming"), ("high", "Hot, high"), ("low", "Hot, low")], "hot"),
                choice("skill", "Skill", SKILLS, "High"),
                choice("weapons", "Weapons", &WEAPONS[1..], "fox3"),
                choice("count", "Number", &[("1", "Single"), ("2", "Pair"), ("4", "Four-ship")], "2"),
            ],
            instructor_only: instructor("bvr"),
            relative_to_player: true,
        },
        CatalogItem {
            id: "missile_drill".into(),
            category: SpawnCategory::AirToAir,
            label: "Missile defence drill".into(),
            description: "A shooter at the range you choose fires at you; the missile trainer removes the missile if it would have killed you and grades your defence.".into(),
            params: vec![
                choice_owned("adversary", "Shooter", adv_opts),
                number("range_nm", "Range", 8., 40., 2., "nm", 20.),
                choice("aspect", "Presentation", &[("hot", "Hot"), ("flank", "Flanking"), ("beam", "Beaming")], "hot"),
                choice("weapons", "Missile", &[("fox3", "Fox 3 (active radar)"), ("fox1", "Fox 1 (semi-active radar)"), ("fox2", "Fox 2 (IR)")], "fox3"),
            ],
            instructor_only: instructor("missile_drill"),
            relative_to_player: true,
        },
        CatalogItem {
            id: "tanker".into(),
            category: SpawnCategory::Tanker,
            label: "Tanker on your position".into(),
            description: "A tanker set up ahead of you on a race-track along your heading, with TACAN and a radio frequency.".into(),
            params: vec![
                choice_owned(
                    "type",
                    "Tanker",
                    TANKER_TYPES
                        .iter()
                        .map(|t| {
                            let m = match t.method {
                                bfprotocols::range::RefuelMethod::Boom => "boom",
                                bfprotocols::range::RefuelMethod::Drogue => "basket",
                            };
                            (t.typ.to_string(), format!("{} ({m})", t.typ))
                        })
                        .collect(),
                ),
                number("alt_ft", "Altitude", 8000., 30000., 1000., "ft", 20000.),
                number("leg_nm", "Race-track leg", 10., 40., 5., "nm", 20.),
            ],
            instructor_only: instructor("tanker"),
            relative_to_player: true,
        },
        CatalogItem {
            id: "ground_targets".into(),
            category: SpawnCategory::AirToGround,
            label: "Ground targets ahead".into(),
            description: "A target group placed on land ahead of you, static or driving.".into(),
            params: vec![
                choice(
                    "composition",
                    "Composition",
                    &GROUND_COMPOSITIONS.iter().map(|(k, l, _)| (*k, *l)).collect::<Vec<_>>(),
                    "armor",
                ),
                number("dist_nm", "Distance ahead", 5., 30., 1., "nm", 10.),
                choice("moving", "Moving", &[("no", "Stationary"), ("yes", "Driving")], "no"),
            ],
            instructor_only: instructor("ground_targets"),
            relative_to_player: true,
        },
        CatalogItem {
            id: "ship_target".into(),
            category: SpawnCategory::Naval,
            label: "Ship target".into(),
            description: "An undefended ship placed over water ahead of you.".into(),
            params: vec![
                choice("type", "Ship", antiship::TARGET_SHIPS, antiship::TARGET_SHIPS[0].0),
                choice("count", "Number", &[("1", "1"), ("2", "2"), ("3", "3")], "1"),
                number("dist_nm", "Distance ahead", 10., 80., 5., "nm", 30.),
                choice("moving", "Moving", &[("yes", "Under way"), ("no", "Stopped")], "yes"),
            ],
            instructor_only: instructor("ship_target"),
            relative_to_player: true,
        },
        CatalogItem {
            id: "naval_group".into(),
            category: SpawnCategory::Naval,
            label: "Defended warship".into(),
            description: "A warship with live or holding air defences. The missile trainer protects players from its SAMs.".into(),
            params: vec![
                choice("type", "Ship", antiship::WARSHIPS, antiship::WARSHIPS[0].0),
                choice("count", "Number", &[("1", "1"), ("2", "2")], "1"),
                number("dist_nm", "Distance ahead", 20., 100., 5., "nm", 50.),
                choice("weapons_free", "Air defence", &[("no", "Weapons hold (radar on)"), ("yes", "Weapons free")], "no"),
            ],
            instructor_only: instructor("naval_group"),
            relative_to_player: true,
        },
        CatalogItem {
            id: "sam_site".into(),
            category: SpawnCategory::Ground,
            label: "SAM / AAA site".into(),
            description: "A SAM or AAA site ahead of you for SEAD and threat-reaction training. Weapons hold keeps the radar on without launching; weapons free launches and the missile trainer protects you.".into(),
            params: vec![
                choice("type", "System", &SAMS.iter().map(|(k, l, _)| (*k, *l)).collect::<Vec<_>>(), "sa8"),
                number("dist_nm", "Distance ahead", 10., 40., 1., "nm", 20.),
                choice("weapons_free", "Rules", &[("no", "Weapons hold (radar on)"), ("yes", "Weapons free")], "no"),
            ],
            instructor_only: instructor("sam_site"),
            relative_to_player: true,
        },
    ];
    let sling = helo.sling_courses();
    if !sling.is_empty() {
        items.push(CatalogItem {
            id: "sling_course".into(),
            category: SpawnCategory::Helo,
            label: "Sling-load course".into(),
            description: "Spawns a cargo load at the course pickup; set it down in the drop zone.".into(),
            params: vec![choice_owned("course", "Course", sling)],
            instructor_only: instructor("sling_course"),
            relative_to_player: false,
        });
    }
    let jt = ground.jtac_list();
    if !jt.is_empty() {
        items.push(CatalogItem {
            id: "cas_drill".into(),
            category: SpawnCategory::Jtac,
            label: "CAS drill".into(),
            description: "An AI JTAC passes you a nine-line and lases the target; graded on time, accuracy and danger-close.".into(),
            params: vec![choice_owned("jtac", "JTAC", jt)],
            instructor_only: instructor("cas_drill"),
            relative_to_player: false,
        });
    }
    let _ = tanker_type;
    SpawnCatalog {
        items,
        max_active_per_player: cfg.spawn.max_active_per_player,
        despawn_after_s: cfg.spawn.despawn_after_s,
    }
}
