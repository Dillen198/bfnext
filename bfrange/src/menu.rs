// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! The F10 "Range" menu.
//!
//! DCS caps each menu at 10 entries and has no "menu opened" callback, so
//! nothing can be generated on demand. Instead of one command per
//! combination (set-up x adversary x weapons would be hundreds per group),
//! the air-to-air menus are *selectors*: each pick is remembered per group and
//! "Spawn with this selection" uses it. Everything funnels into
//! `crate::on_menu(lua, group, action)`.

use anyhow::{Context, Result};
use dcso3::{
    env::miz::GroupId,
    mission_commands::{GroupSubMenu, MissionCommands},
    MizLua, String,
};

const PAGE_ITEMS: u32 = 10;

struct Pager {
    group: GroupId,
    cur: GroupSubMenu,
    used: u32,
}

impl Pager {
    fn new(group: GroupId, root: GroupSubMenu) -> Self {
        Self { group, cur: root, used: 0 }
    }

    fn page(&mut self, mc: &MissionCommands) -> Result<GroupSubMenu> {
        if self.used + 1 >= PAGE_ITEMS {
            self.cur = mc
                .add_submenu_for_group(self.group, "More >>".into(), Some(self.cur.clone()))
                .context("adding More >> page")?;
            self.used = 0;
        }
        self.used += 1;
        Ok(self.cur.clone())
    }

    fn cmd(&mut self, mc: &MissionCommands, label: &str, action: std::string::String) -> Result<()> {
        let parent = self.page(mc)?;
        cmd(mc, self.group, parent, label, action)
    }

    fn sub(&mut self, mc: &MissionCommands, label: &str) -> Result<GroupSubMenu> {
        let parent = self.page(mc)?;
        Ok(mc.add_submenu_for_group(self.group, label.into(), Some(parent))?)
    }
}

fn cmd(mc: &MissionCommands, gid: GroupId, parent: GroupSubMenu, label: &str, action: std::string::String) -> Result<()> {
    mc.add_command_for_group(
        gid,
        label.into(),
        Some(parent),
        move |lua: MizLua, a: String| crate::on_menu(lua, gid, a.as_str()),
        String::from(action.as_str()),
    )?;
    Ok(())
}

/// Lists the menu needs, gathered by the caller from the engine state.
pub struct MenuData {
    pub stations: Vec<(std::string::String, std::string::String)>,
    pub adversaries: Vec<(std::string::String, std::string::String)>,
    pub tanker_types: Vec<(std::string::String, std::string::String)>,
    pub sling: Vec<(std::string::String, std::string::String)>,
    pub troops: Vec<(std::string::String, std::string::String)>,
    pub jtacs: Vec<(std::string::String, std::string::String)>,
    pub ships: Vec<(std::string::String, std::string::String)>,
    pub compositions: Vec<(std::string::String, std::string::String)>,
    pub sams: Vec<(std::string::String, std::string::String)>,
    pub is_helo: bool,
}

pub fn build(lua: MizLua, gid: GroupId, d: &MenuData) -> Result<()> {
    let mc = MissionCommands::singleton(lua)?;
    let root = mc.add_submenu_for_group(gid, "Range".into(), None)?;
    let r = |parent: &GroupSubMenu, label: &str, action: &str| cmd(&mc, gid, parent.clone(), label, action.to_string());
    let sub = |parent: &GroupSubMenu, label: &str| -> Result<GroupSubMenu> {
        Ok(mc.add_submenu_for_group(gid, label.into(), Some(parent.clone()))?)
    };

    // 1. status, and what is where
    r(&root, "Range status", "status")?;
    r(&root, "Sectors: what is where", "sectors")?;

    // 2. air-to-ground
    let ag = sub(&root, "Air-to-Ground")?;
    let st = sub(&ag, "Stations")?;
    let mut p = Pager::new(gid, st);
    for (id, name) in &d.stations {
        let s = p.sub(&mc, name)?;
        r(&s, "Info / bearing", &format!("ag:info:{id}"))?;
        r(&s, "Smoke the target", &format!("ag:smoke:{id}"))?;
        r(&s, "Reset targets", &format!("ag:reset:{id}"))?;
    }
    let gt = sub(&ag, "Ground targets 10 nm ahead")?;
    for (k, l) in &d.compositions {
        r(&gt, l, &format!("spawn:ground_targets:composition={k}&dist_nm=10&moving=no"))?;
    }
    let ms = sub(&ag, "SAM site 20 nm ahead (instructor)")?;
    for (k, l) in &d.sams {
        r(&ms, l, &format!("spawn:sam_site:type={k}&dist_nm=20&weapons_free=no"))?;
    }

    // 3. air-to-air
    let aa = sub(&root, "Air-to-Air")?;
    let sel = sub(&aa, "Set-up selection")?;
    let setup = sub(&sel, "Set-up")?;
    for (k, l) in [
        ("offensive", "BFM offensive"),
        ("defensive", "BFM defensive"),
        ("neutral", "BFM neutral pass"),
        ("perch", "BFM high perch"),
        ("head_on", "BFM head-on"),
        ("bvr:hot", "BVR hot"),
        ("bvr:flank", "BVR flanking"),
        ("bvr:beam", "BVR beaming"),
        ("drill", "Missile defence drill"),
    ] {
        r(&setup, l, &format!("sel:setup:{k}"))?;
    }
    let adv = sub(&sel, "Adversary")?;
    let mut p = Pager::new(gid, adv);
    for (k, l) in &d.adversaries {
        p.cmd(&mc, l, format!("sel:adv:{k}"))?;
    }
    let wpn = sub(&sel, "Weapons")?;
    for (k, l) in crate::catalog::WEAPONS {
        r(&wpn, l, &format!("sel:weapons:{k}"))?;
    }
    let skill = sub(&sel, "Skill")?;
    for (k, l) in crate::catalog::SKILLS {
        r(&skill, l, &format!("sel:skill:{k}"))?;
    }
    let rng = sub(&sel, "BVR range")?;
    for n in [10, 20, 30, 40, 60] {
        r(&rng, &format!("{n} nm"), &format!("sel:range:{n}"))?;
    }
    let cnt = sub(&sel, "Number")?;
    for n in [1, 2, 4] {
        r(&cnt, &format!("{n}"), &format!("sel:count:{n}"))?;
    }
    r(&sel, "Show selection", "sel:show")?;
    r(&aa, "FIGHT'S ON (spawn selection)", "aa:go")?;
    let duel = sub(&aa, "Duel (PvP)")?;
    r(&duel, "Challenge nearest player", "duel:challenge")?;
    r(&duel, "Accept challenge", "duel:accept")?;
    r(&duel, "Cancel / knock it off", "duel:cancel")?;
    r(&aa, "Missile trainer ON", "trainer:on")?;
    r(&aa, "Missile trainer OFF (live missiles)", "trainer:off")?;

    // 4. tankers
    let tk = sub(&root, "Tankers")?;
    r(&tk, "Tankers on station", "tk:info")?;
    let tko = sub(&tk, "Tanker on my position")?;
    let mut p = Pager::new(gid, tko);
    for (k, l) in &d.tanker_types {
        p.cmd(&mc, l, format!("spawn:tanker:type={k}&alt_ft=20000&leg_nm=20"))?;
    }

    // 5. carrier
    let cv = sub(&root, "Carrier")?;
    r(&cv, "Carrier status (BRC/FB/WOD, TACAN, ICLS)", "cv:info")?;
    r(&cv, "My last pass", "res:last")?;

    // 6. anti-ship
    let sh = sub(&root, "Anti-ship")?;
    for (k, l) in &d.ships {
        r(&sh, &format!("{l} 30 nm ahead"), &format!("spawn:ship_target:type={k}&count=1&dist_nm=30&moving=yes"))?;
    }

    // 7. helicopter
    let he = sub(&root, "Helicopter")?;
    r(&he, "Landing pads", "helo:pads")?;
    r(&he, "Dynamic cargo: where to deliver", "helo:cargo")?;
    if !d.sling.is_empty() {
        let sl = sub(&he, "Sling-load courses")?;
        let mut p = Pager::new(gid, sl);
        for (k, l) in &d.sling {
            p.cmd(&mc, l, format!("spawn:sling_course:course={k}"))?;
        }
    }
    if !d.troops.is_empty() {
        let tr = sub(&he, "Troops: load at")?;
        let mut p = Pager::new(gid, tr);
        for (k, l) in &d.troops {
            p.cmd(&mc, l, format!("helo:load:{k}"))?;
        }
        r(&he, "Troops: unload here", "helo:unload")?;
    }
    let _ = d.is_helo;

    // 8. CAS
    if !d.jtacs.is_empty() {
        let cas = sub(&root, "CAS / JTAC")?;
        let mut p = Pager::new(gid, cas);
        for (k, l) in &d.jtacs {
            p.cmd(&mc, &format!("Check in with {l}"), format!("spawn:cas_drill:jtac={k}"))?;
        }
    }

    // 9. my spawns
    let my = sub(&root, "My spawns")?;
    r(&my, "List", "my:list")?;
    r(&my, "Despawn all", "my:clear")?;

    // 10. results
    let res = sub(&root, "Results")?;
    r(&res, "My last results", "res:last")?;
    r(&res, "Help", "res:help")?;
    Ok(())
}
