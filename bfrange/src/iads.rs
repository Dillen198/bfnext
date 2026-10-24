// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Integrated air defence networks for SEAD / DEAD training.
//!
//! A threat range where the SAMs only hold fire teaches you what an RWR
//! looks like, not how to kill an air defence system. Here the sites behave
//! like a network (Skynet's model): early-warning radars are always up; a
//! SAM's own radars stay dark until the network sees a target inside its
//! engagement range; a site that sees an anti-radiation missile coming shuts
//! down for a while; destroyed sites are rebuilt later. They are weapons free
//! and the missile trainer is the safety net -- a missile that would have
//! killed you is removed and scored.
//!
//! Every emitter or launcher a player kills is a `SeadResult`: which part of
//! the site, with what, from how far, and whether its radar was up when the
//! weapon left the rail (the difference between SEAD and bombing a parked
//! truck).

use crate::{
    aa::TrainerKill,
    ag,
    players::Players,
    records::{self, Recorder},
    spawn::{self, Spawns},
    util::{self, V3},
    weapons::Shooter,
};
use anyhow::{anyhow, Result};
use bfprotocols::range::{
    cfg::{IadsCfg, RangeCfg, SamSiteCfg},
    LiveIads, LiveSamSite, PilotRef, RangeResult, SeadResult,
};
use dcso3::{
    coalition::Side,
    land::Land,
    object::DcsOid,
    weapon::{ClassWeapon, GuidanceType, Weapon, WeaponDesc},
    LuaVec3, MizLua,
};
use fxhash::{FxHashMap, FxHashSet};
use log::{info, warn};
use serde_json::json;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    Search,
    Track,
    Launcher,
    Command,
    Ewr,
    /// self-contained: search, track and launch on one vehicle
    PointDefence,
    Aaa,
    /// IR SAM, no radar
    Ir,
}

impl Role {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Search => "search radar",
            Self::Track => "track radar",
            Self::Launcher => "launcher",
            Self::Command => "command post",
            Self::Ewr => "EWR",
            Self::PointDefence => "SAM vehicle",
            Self::Aaa => "AAA",
            Self::Ir => "IR SAM",
        }
    }

    fn emitter(&self) -> bool {
        matches!(self, Self::Search | Self::Track | Self::Ewr | Self::PointDefence | Self::Aaa)
    }

    /// Without one of these alive the site cannot shoot.
    fn essential(&self) -> bool {
        matches!(self, Self::Track | Self::PointDefence | Self::Aaa | Self::Ir | Self::Ewr)
    }
}

use Role::*;

/// A SAM / radar system the engine can build.
#[derive(Debug)]
pub struct System {
    pub key: &'static str,
    pub label: &'static str,
    pub units: &'static [(&'static str, Role)],
    /// engagement range, metres (what the network lights the site up for)
    pub range_m: f64,
    /// launchers stand on a ring this far from the radars
    pub ring_m: f64,
}

impl System {
    fn is_ewr(&self) -> bool {
        self.units.iter().all(|(_, r)| *r == Ewr)
    }

    /// Point defence and AAA use their own radar and are not cued.
    fn autonomous(&self) -> bool {
        self.units.iter().all(|(_, r)| matches!(r, PointDefence | Aaa | Ir))
    }
}

pub const SYSTEMS: &[System] = &[
    // ---- Soviet / Russian
    System { key: "sa2", label: "SA-2 Guideline", units: &[("p-19 s-125 sr", Search), ("SNR_75V", Track), ("S_75M_Volhov", Launcher), ("S_75M_Volhov", Launcher), ("S_75M_Volhov", Launcher), ("S_75M_Volhov", Launcher)], range_m: 43_000., ring_m: 130. },
    System { key: "sa3", label: "SA-3 Goa", units: &[("p-19 s-125 sr", Search), ("snr s-125 tr", Track), ("5p73 s-125 ln", Launcher), ("5p73 s-125 ln", Launcher), ("5p73 s-125 ln", Launcher)], range_m: 18_000., ring_m: 90. },
    System { key: "sa5", label: "SA-5 Gammon", units: &[("RLS_19J6", Search), ("RPC_5N62V", Track), ("S-200_Launcher", Launcher), ("S-200_Launcher", Launcher), ("S-200_Launcher", Launcher)], range_m: 150_000., ring_m: 140. },
    System { key: "sa6", label: "SA-6 Kub", units: &[("Kub 1S91 str", Track), ("Kub 2P25 ln", Launcher), ("Kub 2P25 ln", Launcher), ("Kub 2P25 ln", Launcher)], range_m: 25_000., ring_m: 80. },
    System { key: "sa8", label: "SA-8 Osa", units: &[("Osa 9A33 ln", PointDefence), ("Osa 9A33 ln", PointDefence)], range_m: 10_000., ring_m: 150. },
    System { key: "sa10", label: "SA-10 Grumble", units: &[("S-300PS 40B6MD sr", Search), ("S-300PS 64H6E sr", Search), ("S-300PS 40B6M tr", Track), ("S-300PS 54K6 cp", Command), ("S-300PS 5P85C ln", Launcher), ("S-300PS 5P85D ln", Launcher), ("S-300PS 5P85C ln", Launcher), ("S-300PS 5P85D ln", Launcher)], range_m: 75_000., ring_m: 150. },
    System { key: "sa11", label: "SA-11 Buk", units: &[("SA-11 Buk SR 9S18M1", Search), ("SA-11 Buk CC 9S470M1", Command), ("SA-11 Buk LN 9A310M1", PointDefence), ("SA-11 Buk LN 9A310M1", PointDefence), ("SA-11 Buk LN 9A310M1", PointDefence)], range_m: 35_000., ring_m: 110. },
    System { key: "sa13", label: "SA-13 Strela-10", units: &[("Strela-10M3", Ir), ("Strela-10M3", Ir)], range_m: 5_000., ring_m: 120. },
    System { key: "sa15", label: "SA-15 Tor", units: &[("Tor 9A331", PointDefence), ("Tor 9A331", PointDefence)], range_m: 12_000., ring_m: 150. },
    System { key: "sa19", label: "SA-19 Tunguska", units: &[("2S6 Tunguska", PointDefence), ("2S6 Tunguska", PointDefence)], range_m: 8_000., ring_m: 150. },
    System { key: "pantsir", label: "Pantsir-S1", units: &[("CHAP_PantsirS1", PointDefence), ("CHAP_PantsirS1", PointDefence)], range_m: 20_000., ring_m: 150. },
    System { key: "tor_m2", label: "Tor-M2", units: &[("CHAP_TorM2", PointDefence), ("CHAP_TorM2", PointDefence)], range_m: 15_000., ring_m: 150. },
    System { key: "zsu", label: "ZSU-23-4 Shilka", units: &[("ZSU-23-4 Shilka", Aaa), ("ZSU-23-4 Shilka", Aaa)], range_m: 2_500., ring_m: 120. },
    System { key: "ewr_1l13", label: "1L13 early warning radar", units: &[("1L13 EWR", Ewr)], range_m: 150_000., ring_m: 0. },
    System { key: "ewr_55g6", label: "55G6 early warning radar", units: &[("55G6 EWR", Ewr)], range_m: 180_000., ring_m: 0. },
    // ---- NATO / western
    System { key: "hawk", label: "MIM-23 Hawk", units: &[("Hawk sr", Search), ("Hawk tr", Track), ("Hawk pcp", Command), ("Hawk cwar", Search), ("Hawk ln", Launcher), ("Hawk ln", Launcher), ("Hawk ln", Launcher)], range_m: 45_000., ring_m: 100. },
    System { key: "patriot", label: "MIM-104 Patriot", units: &[("Patriot str", Track), ("Patriot ECS", Command), ("Patriot EPP", Command), ("Patriot AMG", Command), ("Patriot cp", Command), ("Patriot ln", Launcher), ("Patriot ln", Launcher), ("Patriot ln", Launcher), ("Patriot ln", Launcher)], range_m: 100_000., ring_m: 150. },
    System { key: "nasams", label: "NASAMS", units: &[("NASAMS_Radar_MPQ64F1", Track), ("NASAMS_Command_Post", Command), ("NASAMS_LN_C", Launcher), ("NASAMS_LN_C", Launcher), ("NASAMS_LN_B", Launcher)], range_m: 25_000., ring_m: 120. },
    System { key: "iris_t", label: "IRIS-T SLM", units: &[("CHAP_IRISTSLM_STR", Track), ("CHAP_IRISTSLM_CP", Command), ("CHAP_IRISTSLM_LN", Launcher), ("CHAP_IRISTSLM_LN", Launcher), ("CHAP_IRISTSLM_LN", Launcher)], range_m: 30_000., ring_m: 120. },
    System { key: "roland", label: "Roland", units: &[("Roland Radar", Search), ("Roland ADS", PointDefence), ("Roland ADS", PointDefence)], range_m: 8_000., ring_m: 120. },
    System { key: "gepard", label: "Gepard", units: &[("Gepard", Aaa), ("Gepard", Aaa)], range_m: 4_000., ring_m: 120. },
    System { key: "avenger", label: "M1097 Avenger", units: &[("M1097 Avenger", Ir), ("M1097 Avenger", Ir)], range_m: 5_000., ring_m: 120. },
    System { key: "rapier", label: "Rapier", units: &[("rapier_fsa_blindfire_radar", Track), ("rapier_fsa_optical_tracker_unit", Search), ("rapier_fsa_launcher", Launcher), ("rapier_fsa_launcher", Launcher)], range_m: 7_000., ring_m: 60. },
    System { key: "hq7", label: "HQ-7", units: &[("HQ-7_STR_SP", Search), ("HQ-7_LN_SP", PointDefence), ("HQ-7_LN_SP", PointDefence)], range_m: 12_000., ring_m: 100. },
    System { key: "ewr_fps117", label: "FPS-117 early warning radar", units: &[("FPS-117", Ewr)], range_m: 180_000., ring_m: 0. },
];

pub fn system(key: &str) -> Option<&'static System> {
    SYSTEMS.iter().find(|s| s.key == key)
}

/// Unit positions for a system: radars in the middle, launchers on a ring,
/// point-defence and AAA vehicles spread apart so one bomb doesn't get both.
pub fn layout(sys: &System, centre: V3, heading: f64) -> Vec<(String, Role, V3)> {
    let launchers = sys.units.iter().filter(|(_, r)| *r == Launcher).count().max(1);
    let spread = sys.units.iter().filter(|(_, r)| matches!(r, PointDefence | Aaa | Ir)).count().max(1);
    let (mut li, mut si, mut ri) = (0, 0, 0);
    sys.units
        .iter()
        .map(|(t, role)| {
            let p = match role {
                Launcher => {
                    // a fan in front of the radars
                    let a = heading - 70. + 140. * li as f64 / (launchers.max(2) - 1) as f64;
                    li += 1;
                    util::offset(centre, a, sys.ring_m, 0.)
                }
                PointDefence | Aaa | Ir if spread > 1 => {
                    let a = heading + 360. * si as f64 / spread as f64;
                    si += 1;
                    util::offset(centre, a, sys.ring_m, 0.)
                }
                _ => {
                    // radars and command posts in a short line behind
                    let p = util::offset(centre, heading, -35. * ri as f64, 30. * (ri % 2) as f64);
                    ri += 1;
                    p
                }
            };
            (t.to_string(), *role, p)
        })
        .collect()
}

#[derive(Debug)]
struct SiteRt {
    cfg: SamSiteCfg,
    sys: &'static System,
    pos: V3,
    group: String,
    /// unit name -> (DCS type, role)
    units: FxHashMap<String, (String, Role)>,
    alive: FxHashSet<String>,
    emitting: bool,
    dark_until: f64,
    blink_next: f64,
    destroyed_at: Option<f64>,
}

impl SiteRt {
    fn effective(&self) -> bool {
        let any_essential = self.units.values().any(|(_, r)| r.essential());
        if any_essential {
            self.alive.iter().any(|u| self.units.get(u).map(|(_, r)| r.essential()).unwrap_or(false))
        } else {
            !self.alive.is_empty()
        }
    }

    fn has_emitter(&self) -> bool {
        self.alive.iter().any(|u| self.units.get(u).map(|(_, r)| r.emitter()).unwrap_or(false))
    }
}

#[derive(Debug)]
struct Net {
    cfg: IadsCfg,
    side: Side,
    sites: Vec<SiteRt>,
    /// only targets inside this circle wake the network
    area: Option<(V3, f64)>,
}

/// An anti-radiation (or any) weapon fired at a site, remembered until it
/// lands so the kill can say whether the radar was up at launch.
#[derive(Debug, Clone)]
struct ShotAt {
    emitting: bool,
    from: V3,
    range_m: f64,
    t: f64,
}

#[derive(Debug, Default, Clone, Copy)]
struct Tally {
    shots_at: u32,
    trainer_deaths: u32,
}

#[derive(Debug, Default)]
pub struct Iads {
    nets: Vec<Net>,
    shots: FxHashMap<DcsOid<ClassWeapon>, ShotAt>,
    /// (ucid, network) -> this sortie's SAM shots and trainer deaths
    tally: FxHashMap<(String, usize), Tally>,
    credited: FxHashSet<String>,
    last: f64,
    rng: u64,
}

fn group_name(site_id: &str) -> String {
    format!("RNG-IADS-{site_id}")
}

/// Turn a group's radars on or off (`Group.enableEmission`, DCS 2.7+).
pub fn set_emission(lua: MizLua, group: &str, on: bool) -> Result<()> {
    let g = dcso3::group::Group::get_by_name(lua, group)?;
    g.enable_emission(on)?;
    Ok(())
}

fn spawn_site(lua: MizLua, net: &IadsCfg, side: Side, site: &mut SiteRt, spawns: &mut Spawns, now: f64) -> Result<()> {
    let country = spawn::country_id(net.country.as_deref().unwrap_or(spawn::default_country(side)))?;
    let placed = layout(site.sys, site.pos, site.cfg.heading_deg);
    let units: Vec<(String, V3, f64)> = placed
        .iter()
        .map(|(t, _, p)| {
            let mut q = util::nearest_land(lua, *p, 200.).unwrap_or(*p);
            q.y = util::ground_height(lua, q);
            (t.clone(), q, site.cfg.heading_deg)
        })
        .collect();
    let g = spawn::surface_group(&site.group, &units, "Excellent", vec![spawn::ground_waypoint(site.pos, 0., false, vec![])]);
    spawn::add_group(lua, country, spawn::GROUND, &g)?;
    site.units.clear();
    site.alive.clear();
    for (i, (t, role, _)) in placed.iter().enumerate() {
        let n = format!("{}-{}", site.group, i + 1);
        site.units.insert(n.clone(), (t.clone(), *role));
        site.alive.insert(n);
    }
    // ROE: 2 open fire / 4 weapon hold; ALARM_STATE (9) red keeps them ready
    let roe = if net.weapons_free { 2 } else { 4 };
    spawns.defer(now + 2., spawn::Pending::GroupOption { group: site.group.clone(), id: 0, value: json!(roe) });
    spawns.defer(now + 2., spawn::Pending::GroupOption { group: site.group.clone(), id: 9, value: json!(2) });
    // everything but EWRs and "always on" networks starts dark; the network
    // brings them up
    site.emitting = site.sys.is_ewr() || net.emcon == "always_on";
    site.destroyed_at = None;
    site.dark_until = 0.;
    Ok(())
}

impl Iads {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg, spawns: &mut Spawns, now: f64) {
        self.rng = 0x9E37_79B9_7F4A_7C15 ^ (now.to_bits());
        for n in &cfg.iads {
            let side = spawn::side_of_str(&n.side);
            let area = n.engage_within.as_ref().and_then(|a| match ag::resolve(lua, &a.loc) {
                Ok(p) => Some((p, a.radius_nm * util::NM)),
                Err(e) => {
                    warn!("iads {}: engage area: {e:?}", n.id);
                    None
                }
            });
            let mut net = Net { cfg: n.clone(), side, sites: vec![], area };
            for s in &n.sites {
                let Some(sys) = system(&s.system) else {
                    warn!("iads {}: site {} has unknown system {:?}", n.id, s.id, s.system);
                    continue;
                };
                let pos = match ag::resolve(lua, &s.loc) {
                    Ok(p) => p,
                    Err(e) => {
                        warn!("iads {}: site {}: {e:?}", n.id, s.id);
                        continue;
                    }
                };
                let mut site = SiteRt {
                    cfg: s.clone(),
                    sys,
                    pos,
                    group: group_name(&s.id),
                    units: FxHashMap::default(),
                    alive: FxHashSet::default(),
                    emitting: true,
                    dark_until: 0.,
                    blink_next: 0.,
                    destroyed_at: None,
                };
                match spawn_site(lua, n, side, &mut site, spawns, now) {
                    Ok(()) => {
                        if !site.emitting {
                            // after the group exists; enableEmission on a
                            // group in its birth frame is ignored
                            let g = site.group.clone();
                            spawns.defer(now + 3., spawn::Pending::Emission { group: g, on: false });
                        }
                        net.sites.push(site)
                    }
                    Err(e) => warn!("iads {}: site {} did not spawn: {e:?}", n.id, s.id),
                }
            }
            info!("iads {} ({}): {} sites, emcon {}", n.id, n.name, net.sites.len(), n.emcon);
            self.nets.push(net);
        }
    }

    fn rand(&mut self) -> f64 {
        // xorshift, only for blink timing
        let mut x = self.rng;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.rng = x;
        (x >> 11) as f64 / (1u64 << 53) as f64
    }

    fn site_of(&self, unit: &str) -> Option<(usize, usize)> {
        for (ni, n) in self.nets.iter().enumerate() {
            for (si, s) in n.sites.iter().enumerate() {
                if spawn::in_group(unit, &s.group) {
                    return Some((ni, si));
                }
            }
        }
        None
    }

    /// Network, emission control and rebuilding; every 2 s.
    pub fn tick(&mut self, lua: MizLua, players: &Players, spawns: &mut Spawns, now: f64) {
        if now - self.last < 2. || self.nets.is_empty() {
            return;
        }
        self.last = now;
        let land = Land::singleton(lua).ok();
        let visible = |a: V3, b: V3| -> bool {
            land.as_ref()
                .and_then(|l| l.is_visible(LuaVec3(V3::new(a.x, a.y + 20., a.z)), LuaVec3(b)).ok())
                .unwrap_or(true)
        };
        for ni in 0..self.nets.len() {
            let side = self.nets[ni].side;
            let area = self.nets[ni].area;
            let targets: Vec<V3> = players
                .flying
                .values()
                .filter(|f| f.side != side && f.in_air && !f.is_ground)
                .filter(|f| area.map(|(c, r)| util::dist2(c, f.pos) <= r).unwrap_or(true))
                .map(|f| f.pos)
                .collect();
            // what the network can see: its live EWRs and emitting search radars
            let eyes: Vec<(V3, f64)> = self.nets[ni]
                .sites
                .iter()
                .filter(|s| s.emitting && s.has_emitter())
                .map(|s| (s.pos, if s.sys.is_ewr() { s.sys.range_m } else { s.sys.range_m * 1.5 }))
                .collect();
            let have_ewr = self.nets[ni].sites.iter().any(|s| s.sys.is_ewr() && s.effective());
            let seen: Vec<V3> = targets
                .iter()
                .copied()
                .filter(|t| eyes.iter().any(|(e, r)| util::dist3(*e, *t) <= *r && visible(*e, *t)))
                .collect();
            let emcon = self.nets[ni].cfg.emcon.clone();
            for si in 0..self.nets[ni].sites.len() {
                let blink_roll = self.rand();
                let net = &mut self.nets[ni];
                let respawn = net.cfg.respawn_s;
                let s = &mut net.sites[si];
                // rebuilding
                if !s.effective() {
                    let at = *s.destroyed_at.get_or_insert(now);
                    if let Some(r) = respawn {
                        if now - at >= r as f64 {
                            spawn::destroy_group(lua, &s.group);
                            let cfg = net.cfg.clone();
                            match spawn_site(lua, &cfg, net.side, s, spawns, now) {
                                Ok(()) => {
                                    info!("iads {}: site {} rebuilt", cfg.id, s.cfg.id);
                                    if !s.emitting {
                                        spawns.defer(now + 3., spawn::Pending::Emission { group: s.group.clone(), on: false });
                                    }
                                }
                                Err(e) => warn!("iads {}: site {} rebuild failed: {e:?}", cfg.id, s.cfg.id),
                            }
                        }
                    }
                    continue;
                }
                let in_range = |from: &[V3], r: f64| from.iter().any(|t| util::dist3(s.pos, *t) <= r);
                let want = if s.sys.is_ewr() || emcon == "always_on" {
                    true
                } else if emcon == "blink" {
                    if now >= s.blink_next {
                        s.blink_next = now + 20. + 40. * blink_roll;
                        in_range(&targets, s.sys.range_m * 1.5) && !s.emitting
                    } else {
                        s.emitting && in_range(&targets, s.sys.range_m * 1.5)
                    }
                } else if s.sys.autonomous() {
                    // point defence watches for itself
                    in_range(&targets, s.sys.range_m * 1.5)
                } else if have_ewr {
                    in_range(&seen, s.sys.range_m * 1.15)
                } else {
                    // the EWRs are dead: every site for itself
                    in_range(&targets, s.sys.range_m * 1.3)
                };
                let want = want && now >= s.dark_until;
                if want != s.emitting {
                    match set_emission(lua, &s.group, want) {
                        Ok(()) => s.emitting = want,
                        Err(e) => warn!("iads site {} emission: {e:?}", s.cfg.id),
                    }
                }
            }
        }
    }

    /// A weapon was fired. Anti-radiation missiles at a site make it go
    /// dark; anything fired at a site is remembered for the SEAD card; SAMs
    /// fired at a player are counted for that player.
    #[allow(clippy::too_many_arguments)]
    pub fn on_shot(
        &mut self,
        lua: MizLua,
        players: &Players,
        weapon: &Weapon,
        desc: &WeaponDesc,
        shooter: &Shooter,
        shooter_pos: V3,
        now: f64,
    ) {
        if self.nets.is_empty() {
            return;
        }
        let target = weapon.get_target().ok().flatten().and_then(|t| t.get_name().ok()).map(|s| s.to_string());
        // a site shooting at a player
        if let Some((ni, _)) = self.site_of(&shooter.unit_name) {
            if let Some(f) = target.as_ref().and_then(|t| players.flying.get(t)) {
                self.tally.entry((f.ucid.to_string(), ni)).or_default().shots_at += 1;
            }
            return;
        }
        if !shooter.is_player() {
            return;
        }
        let arm = desc.guidance == Some(GuidanceType::PassiveRadar);
        // the site aimed at: the weapon's own target, or for an ARM fired
        // without a lock, the emitting site nearest its line of flight
        let mut hit = target.as_ref().and_then(|t| self.site_of(t));
        if hit.is_none() && arm {
            let v = weapon.get_velocity().map(|v| v.0).unwrap_or_else(|_| V3::zeros());
            if v.norm() > 1. {
                let hdg = util::hdg(v);
                hit = self
                    .nets
                    .iter()
                    .enumerate()
                    .flat_map(|(ni, n)| n.sites.iter().enumerate().map(move |(si, s)| (ni, si, s)))
                    .filter(|(_, _, s)| s.emitting && s.effective())
                    .filter(|(_, _, s)| util::dist2(shooter_pos, s.pos) < 180_000.)
                    .filter(|(_, _, s)| util::angdiff(util::bearing(shooter_pos, s.pos), hdg).abs() < 12.)
                    .min_by(|a, b| util::dist2(shooter_pos, a.2.pos).total_cmp(&util::dist2(shooter_pos, b.2.pos)))
                    .map(|(ni, si, _)| (ni, si));
            }
        }
        let Some((ni, si)) = hit else { return };
        let site = &self.nets[ni].sites[si];
        if let Ok(oid) = dcso3::object::DcsObject::object_id(weapon) {
            self.shots.insert(oid, ShotAt { emitting: site.emitting, from: shooter_pos, range_m: util::dist2(shooter_pos, site.pos), t: now });
        }
        if arm {
            if let Some(d) = self.nets[ni].cfg.harm_defence_s {
                let s = &mut self.nets[ni].sites[si];
                // most real sites can't see a HARM coming; give it a moment
                s.dark_until = now + 6. + d as f64;
                if s.emitting && !s.sys.is_ewr() {
                    let g = s.group.clone();
                    if set_emission(lua, &g, false).is_ok() {
                        s.emitting = false;
                    }
                    info!("iads site {} going dark: ARM inbound from {}", s.cfg.id, shooter.name);
                }
            }
        }
    }

    pub fn trainer_kill(&mut self, players: &Players, k: &TrainerKill) {
        if let Some((ni, _)) = self.site_of(&k.shooter_unit) {
            if let Some(f) = players.flying.get(&k.target_unit) {
                self.tally.entry((f.ucid.to_string(), ni)).or_default().trainer_deaths += 1;
            }
        }
    }

    /// A unit died (DEAD / UNIT_LOST): keep the site's alive list honest.
    pub fn unit_dead(&mut self, unit: &str, now: f64) {
        if let Some((ni, si)) = self.site_of(unit) {
            let s = &mut self.nets[ni].sites[si];
            s.alive.remove(unit);
            if !s.effective() && s.destroyed_at.is_none() {
                s.destroyed_at = Some(now);
                info!("iads site {} knocked out", s.cfg.id);
            }
        }
    }

    /// A KILL of a site unit: grade it if a player did it.
    #[allow(clippy::too_many_arguments)]
    pub fn on_kill(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        rec: &mut Recorder,
        players: &Players,
        shooter_unit: Option<&str>,
        target_unit: &str,
        weapon: Option<&Weapon>,
        weapon_name: Option<&str>,
        now: f64,
    ) {
        let Some((ni, si)) = self.site_of(target_unit) else { return };
        self.unit_dead(target_unit, now);
        if !self.credited.insert(target_unit.to_string()) {
            return;
        }
        let Some(f) = shooter_unit.and_then(|u| players.flying.get(u)).cloned() else { return };
        let shot = weapon
            .and_then(|w| dcso3::object::DcsObject::object_id(w).ok())
            .and_then(|oid| self.shots.remove(&oid));
        let (guidance, wname) = match weapon.and_then(|w| w.get_weapon_desc().ok()) {
            Some(d) => (
                crate::weapons::guidance_str(d.guidance).to_string(),
                if d.display_name.is_empty() { d.type_name.clone() } else { d.display_name.clone() },
            ),
            None => ("none".into(), weapon_name.map(|s| s.to_string()).unwrap_or_else(|| "guns".into())),
        };
        let net = &self.nets[ni];
        let site = &net.sites[si];
        let (typ, role) = site.units.get(target_unit).cloned().unwrap_or_else(|| ("unknown".into(), Launcher));
        let emitting = shot.as_ref().map(|s| s.emitting).unwrap_or(site.emitting);
        let tally = self.tally.get(&(f.ucid.to_string(), ni)).copied().unwrap_or_default();
        let destroyed = !site.effective();
        let res = SeadResult {
            network: net.cfg.name.clone(),
            site: site.cfg.name.clone(),
            system: site.sys.label.into(),
            unit_type: typ.clone(),
            role: role.label().into(),
            weapon: wname.clone(),
            guidance,
            launch_range_m: shot.as_ref().map(|s| s.range_m),
            site_was_emitting: emitting,
            site_destroyed: destroyed,
            shots_at_you: tally.shots_at,
            trainer_deaths: tally.trainer_deaths,
            site_pos: util::geo(lua, site.pos),
            launch_pos: shot.as_ref().map(|s| util::geo(lua, s.from)),
        };
        // an emitter killed while it was up is the job done right
        let score = match (role.emitter() || role == Ir, emitting) {
            (true, true) => 5.,
            (true, false) => 4.,
            (false, _) if destroyed => 4.,
            _ => 3.,
        };
        if cfg.in_game_results {
            records::to_group(
                lua,
                f.group_id,
                &format!(
                    "SEAD {}: {} killed the {} {} ({}){}{}",
                    net.cfg.name,
                    wname,
                    site.sys.label,
                    role.label(),
                    if emitting { "radar was up" } else { "radar was dark" },
                    shot.as_ref().map(|s| format!(", launched {:.1} nm", s.range_m / util::NM)).unwrap_or_default(),
                    if destroyed { format!(" - {} is DOWN", site.cfg.name) } else { String::new() },
                ),
                cfg.message_s,
            );
        }
        rec.emit(lua, PilotRef { ucid: Some(f.ucid.to_string()), name: f.name.clone() }, &f.typ, f.side, &f.group_name, Some(score), RangeResult::Sead(res), None);
    }

    /// Forget the sortie tallies for a player who left their aircraft, and
    /// shots that must have landed long ago.
    pub fn player_left(&mut self, ucid: &str, now: f64) {
        self.tally.retain(|(u, _), _| u != ucid);
        self.shots.retain(|_, s| now - s.t < 300.);
    }

    /// Instructor: rebuild every site of a network now.
    pub fn reset(&mut self, lua: MizLua, id: &str, spawns: &mut Spawns, now: f64) -> Result<String> {
        let net = self.nets.iter_mut().find(|n| n.cfg.id == id).ok_or_else(|| anyhow!("no IADS {id}"))?;
        let cfg = net.cfg.clone();
        for s in net.sites.iter_mut() {
            spawn::destroy_group(lua, &s.group);
            spawn_site(lua, &cfg, net.side, s, spawns, now)?;
            if !s.emitting {
                spawns.defer(now + 3., spawn::Pending::Emission { group: s.group.clone(), on: false });
            }
        }
        Ok(format!("{} rebuilt", cfg.name))
    }

    pub fn list(&self) -> Vec<(String, String)> {
        self.nets.iter().map(|n| (n.cfg.id.clone(), n.cfg.name.clone())).collect()
    }

    /// F10: the network's sites from the player, nearest first. Where the
    /// sites are is briefed; whether they are up is for the RWR to say.
    pub fn describe(&self, from: V3, side: Side) -> Vec<String> {
        let mut out = vec![];
        for n in self.nets.iter().filter(|n| n.side != side) {
            out.push(format!(
                "{} ({}, {}):",
                n.cfg.name,
                if n.cfg.weapons_free { "WEAPONS FREE, trainer protects you" } else { "weapons hold" },
                match n.cfg.emcon.as_str() {
                    "iads" => "networked",
                    "blink" => "blinking",
                    _ => "radars always on",
                }
            ));
            let mut sites: Vec<&SiteRt> = n.sites.iter().collect();
            sites.sort_by(|a, b| util::dist2(from, a.pos).total_cmp(&util::dist2(from, b.pos)));
            for s in sites {
                out.push(format!(
                    "  {} [{}] {:03.0}/{:.0}nm, {:.0} nm ring{}",
                    s.cfg.name,
                    s.sys.label,
                    util::bearing(from, s.pos),
                    util::dist2(from, s.pos) / util::NM,
                    s.sys.range_m / util::NM,
                    if s.effective() { "" } else { " - DESTROYED, rebuilding" }
                ));
            }
        }
        if out.is_empty() {
            out.push("No enemy air defence networks on this range".into());
        }
        out
    }

    pub fn live(&self, lua: MizLua) -> Vec<LiveIads> {
        self.nets
            .iter()
            .map(|n| LiveIads {
                id: n.cfg.id.clone(),
                name: n.cfg.name.clone(),
                side: records::side_str(n.side).into(),
                weapons_free: n.cfg.weapons_free,
                sites: n
                    .sites
                    .iter()
                    .map(|s| LiveSamSite {
                        id: s.cfg.id.clone(),
                        name: s.cfg.name.clone(),
                        system: s.sys.label.into(),
                        pos: util::geo(lua, s.pos),
                        emitting: s.emitting && s.effective(),
                        units_alive: s.alive.len() as u32,
                        units_total: s.units.len() as u32,
                        range_m: s.sys.range_m,
                    })
                    .collect(),
            })
            .collect()
    }
}

/// The live-fire unit types the range uses, for a config/unit sanity test.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_system_has_an_essential_unit() {
        for s in SYSTEMS {
            assert!(s.units.iter().any(|(_, r)| r.essential()), "{} has nothing that can shoot or see", s.key);
        }
    }

    /// Every type the IADS, hot zones and EW spawn is a real DCS unit
    /// (data/unitdb-baseline.json is harvested from the live server's DCS).
    #[test]
    fn every_type_is_a_dcs_unit() {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../data/unitdb-baseline.json");
        let Ok(txt) = std::fs::read_to_string(path) else { return };
        let db: serde_json::Value = serde_json::from_str(&txt).unwrap();
        let known = db["by_type"].as_object().unwrap();
        let mut types: Vec<&str> = SYSTEMS.iter().flat_map(|s| s.units.iter().map(|(t, _)| *t)).collect();
        for (_, _, red, blue) in crate::hotzone::HOT_ZONE_COMPOSITIONS {
            types.extend(red.iter().copied());
            types.extend(blue.iter().copied());
        }
        types.extend(["GPS_Spoofer_Red", "GPS_Spoofer_Blue", "Soldier M4", "Infantry AK", "Paratrooper RPG-16", "SA-18 Igla manpad", "Ural-375 ZU-23", "Soldier M249", "Soldier stinger", "M1043 HMMWV Armament"]);
        let missing: Vec<&str> = types.into_iter().filter(|t| !known.contains_key(*t)).collect();
        assert!(missing.is_empty(), "not DCS unit types: {missing:?}");
    }

    #[test]
    fn keys_are_unique() {
        let mut k: Vec<&str> = SYSTEMS.iter().map(|s| s.key).collect();
        k.sort();
        let n = k.len();
        k.dedup();
        assert_eq!(n, k.len());
    }
}
