// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Air-to-air: the missile trainer, BFM/BVR set-ups against AI, and
//! opt-in duels between players.
//!
//! The missile trainer (MOOSE FOX's method) follows every air-to-air
//! missile and SAM, checking its distance to its target more often the
//! closer it gets (5 s beyond 50 km down to every tick inside 1 km). Inside
//! the kill radius it deletes the missile before the fuze can act and tells
//! both sides "you'd be dead". A missile that disappears without ever getting
//! that close was defeated. Guns cannot be protected this way -- DCS offers
//! no hook for a bullet -- so gun damage in BFM is real.

use crate::{
    harvest,
    players::{Flying, Players},
    records::{self, Recorder},
    spawn::{self, AirSpec, Spawn, SpawnKind, Spawns},
    util::{self, V3},
    weapons::Shooter,
};
use anyhow::{anyhow, bail, Result};
use bfprotocols::range::{
    cfg::{AdversaryCfg, MissileTrainerCfg, RangeCfg},
    DefenseSummary, EngagementOutcome, EngagementResult, LaunchGeom, MissileOutcome,
    MissileResult, PilotRef, RangeResult, Track, TrackPt,
};
use chrono::{Duration, Utc};
use dcso3::{
    coalition::Side,
    env::miz::GroupId,
    net::Ucid,
    object::{DcsObject, DcsOid},
    trigger::Trigger,
    unit::Unit,
    weapon::{ClassWeapon, MissileCategory, Weapon, WeaponDesc},
    LuaVec3, MizLua,
};
use fxhash::FxHashMap;
use log::{info, warn};
use serde_json::json;
use std::collections::BTreeMap;

pub const FOX2: &[&str] = &[
    "{FBC29BFE-3D24-4C64-B81D-941239D12249}", // R-73
    "{5CE2FF2A-645A-4197-B48D-8720AC69394F}", // AIM-9X
    "{6CEB49FC-DED8-4DED-B053-E1F033FF72D3}", // AIM-9M
    "{AIM-9P5}",
    "{AIM-9P}",
    "{9BFD8C90-F7AE-4e90-833B-BFD0CED0E536}",
    "{R-60M 2L}",
    "{R-60M 2R}",
    "{R-3S}",
];

/// Fox 1: semi-active radar missiles.
pub const FOX1: &[&str] = &[
    "{E8069896-8435-4B90-95C0-01A03AE6E400}", // R-27ER
    "{9B25D316-0434-4954-868F-D51DB1A38DF0}", // R-27R
    "{8D399DDA-FF81-4F14-904D-099B34FE7918}", // AIM-7M
];

/// Fox 3: active radar missiles. SD-10 and PL-12 are normally found by name
/// (`harvest::loadout_by_class`); these CLSIDs are only the fallback.
pub const FOX3: &[&str] = &[
    "{B4C01D60-A8A3-4237-BD72-CA7655BC0FE9}", // R-77
    "{40EF17B7-F508-45de-8566-6FFECC0C1AB8}", // AIM-120C
    "{C8E06185-7CD6-4C90-959F-044679E90751}", // AIM-120B
    "DIS_SD-10",
    "{PL-12}",
];

/// Adversaries offered when the config lists none.
pub fn default_adversaries() -> Vec<AdversaryCfg> {
    [
        ("MiG-29S", "MiG-29S Fulcrum"),
        ("Su-27", "Su-27 Flanker"),
        ("MiG-21Bis", "MiG-21bis Fishbed"),
        ("F-5E-3", "F-5E Tiger II"),
        ("F-16C_50", "F-16C Viper"),
        ("F-15C", "F-15C Eagle"),
        ("FA-18C_hornet", "F/A-18C Hornet"),
        ("J-11A", "J-11A Flanker"),
        ("JF-17", "JF-17 Thunder"),
    ]
    .iter()
    .map(|(t, l)| AdversaryCfg {
        typ: t.to_string(),
        label: l.to_string(),
        loadouts: BTreeMap::new(),
        side: "red".into(),
        country: "CJTF Red".into(),
        livery: None,
    })
    .collect()
}

pub fn loadout(adv: &AdversaryCfg, weapons: &str) -> BTreeMap<u8, String> {
    if let Some(l) = adv.loadouts.get(weapons) {
        return l.clone();
    }
    // by DCS store name first, the CLSID lists as a fallback
    let pick = |class: &str, list: &[&str], max: usize| {
        let l = harvest::loadout_by_class(&adv.typ, class, max);
        if l.is_empty() { harvest::auto_loadout(&adv.typ, list, max) } else { l }
    };
    let mut l = match weapons {
        "fox1" => pick("fox1", FOX1, 2),
        "fox2" => return pick("fox2", FOX2, 2),
        "fox3" => pick("fox3", FOX3, 2),
        _ => return BTreeMap::new(),
    };
    // radar missiles fly with a pair of heaters, as they would for real
    for (k, v) in pick("fox2", FOX2, 4) {
        if l.len() >= 4 {
            break;
        }
        l.entry(k).or_insert(v);
    }
    l
}

#[derive(Debug, Clone)]
struct Missile {
    oid: DcsOid<ClassWeapon>,
    weapon: String,
    category: &'static str,
    shooter: Shooter,
    t0: f64,
    kill_radius: f64,
    target: Option<String>,
    min_dist: f64,
    next_check: f64,
    last_pos: V3,
    launch: Option<LaunchGeom>,
    // defence sampling
    tgt_hdg0: Option<f64>,
    tgt_alt0: Option<f64>,
    reaction_s: Option<f64>,
    beam_s: f64,
    drag_s: f64,
    hot_s: f64,
    alt_change: f64,
    went_low: bool,
    last_sample: f64,
    missile_pts: Vec<TrackPt>,
    target_pts: Vec<TrackPt>,
    /// the target took a real hit from this missile
    hit: bool,
}

/// Something the engagement tracker needs to hear about.
#[derive(Debug, Clone)]
pub struct TrainerKill {
    pub shooter_unit: String,
    pub target_unit: String,
    pub weapon: String,
}

#[derive(Debug, Clone)]
struct Side_ {
    unit: String,
    ucid: Option<Ucid>,
    name: String,
    typ: String,
    side: Side,
    group: Option<GroupId>,
    callsign: String,
}

#[derive(Debug, Clone)]
struct Engagement {
    setup: String,
    player: Side_,
    /// AI adversary group, or the other player in a duel
    adversary_group: Option<String>,
    adversary_type: String,
    skill: String,
    opponent: Option<Side_>,
    spawn_id: Option<String>,
    started: f64,
    shots: FxHashMap<String, u32>,
    trainer_kills: FxHashMap<String, u32>,
    gun_hits: FxHashMap<String, u32>,
    killed: Option<String>,
    track: BTreeMap<String, Vec<TrackPt>>,
    last_sample: f64,
    notes: Vec<String>,
}

#[derive(Debug, Clone)]
struct Challenge {
    from: Ucid,
    to: Ucid,
    expires: f64,
}

#[derive(Debug, Default)]
pub struct AirToAir {
    missiles: Vec<Missile>,
    engagements: FxHashMap<u64, Engagement>,
    seq: u64,
    challenges: Vec<Challenge>,
}

fn sample(lua: MizLua, t: f64, p: V3, v: V3) -> TrackPt {
    let g = util::geo(lua, p);
    TrackPt { t, lat: g.lat, lon: g.lon, alt_m: p.y, speed_kts: v.norm() * util::MS_TO_KTS }
}

impl AirToAir {
    pub fn active(&self) -> bool {
        !self.missiles.is_empty()
    }

    /// Earliest time any missile needs checking.
    pub fn next_due(&self) -> Option<f64> {
        self.missiles.iter().map(|m| m.next_check).min_by(|a, b| a.total_cmp(b))
    }

    /// An air-to-air missile or SAM was fired.
    #[allow(clippy::too_many_arguments)]
    pub fn on_shot(
        &mut self,
        lua: MizLua,
        cfg: &MissileTrainerCfg,
        players: &Players,
        weapon: &Weapon,
        desc: &WeaponDesc,
        shooter: Shooter,
        shooter_pos: V3,
        now: f64,
    ) {
        if !cfg.enabled {
            return;
        }
        if self.missiles.len() >= cfg.max_tracked {
            self.missiles.remove(0);
        }
        let Ok(oid) = weapon.object_id() else { return };
        let big = desc.warhead_explosive_kg.map(|e| e >= cfg.big_warhead_kg).unwrap_or(false);
        let target = weapon
            .get_target()
            .ok()
            .flatten()
            .and_then(|t| t.get_name().ok())
            .map(|s| s.to_string());
        let category = match desc.missile_category {
            Some(MissileCategory::Aam) => "aam",
            Some(MissileCategory::Sam) => "sam",
            _ => "other",
        };
        let pos = weapon.get_point().map(|p| p.0).unwrap_or(shooter_pos);
        // count the shot for any engagement the shooter is in
        for e in self.engagements.values_mut() {
            if e.player.unit == shooter.unit_name
                || e.opponent.as_ref().map(|o| o.unit == shooter.unit_name).unwrap_or(false)
                || e.adversary_group.as_ref().map(|g| spawn::in_group(&shooter.unit_name, g)).unwrap_or(false)
            {
                *e.shots.entry(shooter.unit_name.clone()).or_default() += 1;
            }
        }
        if cfg.launch_alerts {
            if let Some(tn) = &target {
                if let Some(f) = players.flying.get(tn) {
                    let brg = util::bearing(f.pos, shooter_pos);
                    let rng = util::dist2(f.pos, shooter_pos) / util::NM;
                    records::to_group(
                        lua,
                        f.group_id,
                        &format!(
                            "MISSILE LAUNCH: {} from {:03.0} for {:.1} nm ({}). Notch {:03.0} / {:03.0}",
                            if desc.display_name.is_empty() { &desc.type_name } else { &desc.display_name },
                            brg,
                            rng,
                            shooter.typ,
                            (brg + 90.).rem_euclid(360.),
                            (brg + 270.).rem_euclid(360.)
                        ),
                        8,
                    );
                }
            }
        }
        self.missiles.push(Missile {
            oid,
            weapon: if desc.display_name.is_empty() { desc.type_name.clone() } else { desc.display_name.clone() },
            category,
            shooter,
            t0: now,
            kill_radius: if big { cfg.big_kill_radius_m } else { cfg.kill_radius_m },
            target,
            min_dist: f64::MAX,
            next_check: now,
            last_pos: pos,
            launch: None,
            tgt_hdg0: None,
            tgt_alt0: None,
            reaction_s: None,
            beam_s: 0.,
            drag_s: 0.,
            hot_s: 0.,
            alt_change: 0.,
            went_low: false,
            last_sample: f64::MIN,
            missile_pts: vec![],
            target_pts: vec![],
            hit: false,
        });
    }

    /// A HIT from a missile we track: it got through (trainer off for that
    /// target, or it fuzed between checks).
    pub fn record_hit(&mut self, oid: &DcsOid<ClassWeapon>) {
        if let Some(m) = self.missiles.iter_mut().find(|m| &m.oid == oid) {
            m.hit = true;
        }
    }

    fn protected(cfg: &MissileTrainerCfg, players: &Players, spawned_ai: &dyn Fn(&str) -> bool, unit: &str) -> bool {
        match players.flying.get(unit) {
            Some(f) => f.trainer,
            None => cfg.protect_ai && spawned_ai(unit),
        }
    }

    /// Check the missiles that are due. Returns trainer kills for the
    /// engagement tracker.
    #[allow(clippy::too_many_arguments)]
    pub fn tick(
        &mut self,
        lua: MizLua,
        cfg: &MissileTrainerCfg,
        players: &Players,
        spawns: &Spawns,
        rec: &mut Recorder,
        msg_s: u32,
        now: f64,
    ) -> Vec<TrainerKill> {
        let spawned_ai = |unit: &str| {
            spawns.active.values().any(|s| s.groups.iter().any(|g| spawn::in_group(unit, g)))
        };
        let mut kills = vec![];
        let mut i = 0;
        while i < self.missiles.len() {
            if self.missiles[i].next_check > now {
                i += 1;
                continue;
            }
            let w = Weapon::get_instance(lua, &self.missiles[i].oid);
            let (wpos, wvel) = match &w {
                Ok(w) => match (w.get_point(), w.get_velocity()) {
                    (Ok(p), Ok(v)) => (Some(p.0), v.0),
                    _ => (None, V3::zeros()),
                },
                Err(_) => (None, V3::zeros()),
            };
            let Some(wpos) = wpos else {
                let m = self.missiles.swap_remove(i);
                self.finish(lua, players, rec, m, None, msg_s, now);
                continue;
            };
            let m = &mut self.missiles[i];
            m.last_pos = wpos;
            // re-read the target: seekers switch
            if let Ok(w) = &w {
                if let Ok(Some(t)) = w.get_target() {
                    if let Ok(n) = t.get_name() {
                        m.target = Some(n.to_string());
                    }
                }
            }
            // no target: the nearest protected player within 3 km
            if m.target.is_none() {
                m.target = players
                    .flying
                    .values()
                    .filter(|f| f.trainer && f.side != m.shooter.side)
                    .map(|f| (f.unit_name.clone(), util::dist3(f.pos, wpos)))
                    .filter(|(_, d)| *d < 3000.)
                    .min_by(|a, b| a.1.total_cmp(&b.1))
                    .map(|(n, _)| n);
            }
            let tgt = m.target.as_ref().and_then(|n| Unit::get_by_name(lua, n).ok());
            let Some(tgt) = tgt else {
                m.next_check = now + 0.5;
                i += 1;
                continue;
            };
            let (tpos, tvel) = match (tgt.get_point(), tgt.get_velocity()) {
                (Ok(p), Ok(v)) => (p.0, v.0),
                _ => {
                    m.next_check = now + 0.5;
                    i += 1;
                    continue;
                }
            };
            let d = util::dist3(tpos, wpos);
            m.min_dist = m.min_dist.min(d);
            if m.launch.is_none() {
                let sp = Unit::get_by_name(lua, &m.shooter.unit_name).ok();
                let (spos, svel) = sp
                    .and_then(|u| Some((u.get_point().ok()?.0, u.get_velocity().ok()?.0)))
                    .unwrap_or((wpos, wvel));
                let thdg = util::hdg(tvel);
                let aspect = util::angdiff(thdg, util::bearing(tpos, spos)).abs();
                let los = (tpos - spos).normalize();
                let closure = (svel - tvel).dot(&los) * util::MS_TO_KTS;
                m.launch = Some(LaunchGeom {
                    range_m: util::dist3(spos, tpos),
                    aspect_deg: aspect,
                    shooter_alt_m: spos.y,
                    target_alt_m: tpos.y,
                    shooter_speed_kts: svel.norm() * util::MS_TO_KTS,
                    target_speed_kts: tvel.norm() * util::MS_TO_KTS,
                    closure_kts: closure,
                    shooter_pos: util::geo(lua, spos),
                    target_pos: util::geo(lua, tpos),
                });
                m.tgt_hdg0 = Some(thdg);
                m.tgt_alt0 = Some(tpos.y);
            }
            // defence sampling, 2 Hz
            if now - m.last_sample >= 0.5 {
                let dt = if m.last_sample == f64::MIN { 0. } else { now - m.last_sample };
                m.last_sample = now;
                let thdg = util::hdg(tvel);
                let off = util::angdiff(util::bearing(tpos, wpos), thdg).abs();
                if off < 45. {
                    m.hot_s += dt
                } else if (70. ..=110.).contains(&off) {
                    m.beam_s += dt
                } else if off > 135. {
                    m.drag_s += dt
                }
                if m.reaction_s.is_none() {
                    if let Some(h0) = m.tgt_hdg0 {
                        if util::angdiff(thdg, h0).abs() > 30. {
                            m.reaction_s = Some(now - m.t0);
                        }
                    }
                }
                if let Some(a0) = m.tgt_alt0 {
                    m.alt_change = tpos.y - a0;
                }
                if tpos.y - util::ground_height(lua, tpos) < 1000. {
                    m.went_low = true;
                }
                m.missile_pts.push(sample(lua, now - m.t0, wpos, wvel));
                m.target_pts.push(sample(lua, now - m.t0, tpos, tvel));
            }
            let tname = m.target.clone().unwrap_or_default();
            if d <= m.kill_radius && Self::protected(cfg, players, &spawned_ai, &tname) {
                if let Ok(w) = &w {
                    let _ = w.destroy();
                }
                let m = self.missiles.swap_remove(i);
                kills.push(TrainerKill {
                    shooter_unit: m.shooter.unit_name.clone(),
                    target_unit: tname.clone(),
                    weapon: m.weapon.clone(),
                });
                self.finish(lua, players, rec, m, Some(tname), msg_s, now);
                continue;
            }
            // adaptive re-check interval (FOX's table)
            let km = d / 1000.;
            m.next_check = now
                + if km > 50. {
                    5.
                } else if km > 10. {
                    1.
                } else if km > 5. {
                    0.5
                } else if km > 1. {
                    0.1
                } else {
                    0.02
                };
            i += 1;
        }
        kills
    }

    /// A missile's flight is over. `killed` = the target the trainer killed.
    #[allow(clippy::too_many_arguments)]
    fn finish(
        &mut self,
        lua: MizLua,
        players: &Players,
        rec: &mut Recorder,
        m: Missile,
        killed: Option<String>,
        msg_s: u32,
        _now: f64,
    ) {
        let Some(tname) = killed.clone().or(m.target.clone()) else { return };
        let outcome = if killed.is_some() {
            MissileOutcome::Kill
        } else if m.hit {
            MissileOutcome::Hit
        } else if m.min_dist < f64::MAX {
            MissileOutcome::Defeated
        } else {
            MissileOutcome::Timeout
        };
        let tf = players.flying.get(&tname);
        let target_ref = PilotRef {
            ucid: tf.map(|f| f.ucid.to_string()),
            name: tf.map(|f| f.name.clone()).unwrap_or_else(|| tname.clone()),
        };
        let target_type = tf.map(|f| f.typ.clone()).unwrap_or_else(|| {
            Unit::get_by_name(lua, &tname)
                .and_then(|u| u.get_type_name())
                .map(|s| s.to_string())
                .unwrap_or_default()
        });
        let shooter_ref = PilotRef { ucid: m.shooter.ucid.map(|u| u.to_string()), name: m.shooter.name.clone() };
        let min_d = if m.min_dist == f64::MAX { 0. } else { m.min_dist };
        let base = MissileResult {
            weapon: m.weapon.clone(),
            weapon_category: m.category.into(),
            shooter: shooter_ref.clone(),
            shooter_type: m.shooter.typ.clone(),
            target: target_ref.clone(),
            target_type: target_type.clone(),
            outcome,
            launch: m.launch.clone().unwrap_or_default(),
            min_distance_m: min_d,
            time_of_flight_s: m.last_sample.max(m.t0) - m.t0,
            kill_radius_m: m.kill_radius,
            defense: DefenseSummary {
                reaction_s: m.reaction_s,
                beam_s: m.beam_s,
                drag_s: m.drag_s,
                hot_s: m.hot_s,
                alt_change_m: m.alt_change,
                went_low: m.went_low,
            },
            perspective: String::new(),
        };
        let track = Track::Intercept { missile: m.missile_pts.clone(), target: m.target_pts.clone() };
        // defender's record
        if let Some(f) = tf {
            let mut r = base.clone();
            r.perspective = "target".into();
            let score = match outcome {
                MissileOutcome::Defeated => Some(5.),
                MissileOutcome::Kill | MissileOutcome::Hit => Some(1.),
                MissileOutcome::Timeout => None,
            };
            let text = match outcome {
                MissileOutcome::Kill => format!(
                    "SPLASH! {} from {} ({}) would have killed you - closest {:.0} m",
                    m.weapon, m.shooter.name, m.shooter.typ, min_d
                ),
                MissileOutcome::Defeated => format!(
                    "DEFEATED: {} from {} - closest {:.0} m",
                    m.weapon, m.shooter.name, min_d
                ),
                MissileOutcome::Hit => format!("HIT by {} from {}", m.weapon, m.shooter.name),
                MissileOutcome::Timeout => format!("{} from {} timed out", m.weapon, m.shooter.name),
            };
            records::to_group(lua, f.group_id, &text, msg_s);
            rec.emit(
                lua,
                target_ref.clone(),
                &f.typ,
                f.side,
                &f.group_name,
                score,
                RangeResult::Missile(r),
                Some(track.clone()),
            );
        }
        // shooter's record
        if m.shooter.is_player() {
            let mut r = base;
            r.perspective = "shooter".into();
            let score = match outcome {
                MissileOutcome::Kill | MissileOutcome::Hit => Some(5.),
                MissileOutcome::Defeated => Some(1.),
                MissileOutcome::Timeout => None,
            };
            if let Some(g) = m.shooter.group_id {
                let text = match outcome {
                    MissileOutcome::Kill => format!("SPLASH! Your {} would have killed {}", m.weapon, target_ref.name),
                    MissileOutcome::Defeated => {
                        format!("Your {} was defeated by {} - closest {:.0} m", m.weapon, target_ref.name, min_d)
                    }
                    MissileOutcome::Hit => format!("Your {} hit {}", m.weapon, target_ref.name),
                    MissileOutcome::Timeout => format!("Your {} timed out", m.weapon),
                };
                records::to_group(lua, g, &text, msg_s);
            }
            rec.emit(
                lua,
                shooter_ref,
                &m.shooter.typ,
                m.shooter.side,
                &m.shooter.callsign,
                score,
                RangeResult::Missile(r),
                Some(track),
            );
        }
    }

    // ------------------------------------------------------------ set-ups

    /// Spawn an AI adversary relative to `player`.
    #[allow(clippy::too_many_arguments)]
    pub fn spawn_setup(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        spawns: &mut Spawns,
        player: &Flying,
        kind: &str,
        setup: &str,
        adv_typ: &str,
        skill: &str,
        weapons: &str,
        count: u32,
        range_nm: f64,
        now: f64,
    ) -> Result<String> {
        if !player.in_air {
            bail!("you need to be airborne for an air-to-air set-up")
        }
        let advs = if cfg.air_to_air.adversaries.is_empty() {
            default_adversaries()
        } else {
            cfg.air_to_air.adversaries.clone()
        };
        let adv = advs
            .iter()
            .find(|a| a.typ == adv_typ)
            .cloned()
            .ok_or_else(|| anyhow!("unknown adversary {adv_typ}"))?;
        let p = player.pos;
        let h = util::hdg(player.vel);
        let v = player.vel.norm().max(150.);
        let (fwd, right, dalt, adv_hdg, adv_speed, label) = match (kind, setup) {
            ("bfm", "offensive") => (1000., 0., 0., h, v * 0.9, "BFM offensive"),
            ("bfm", "defensive") => (-1000., 0., 0., h, v * 1.05, "BFM defensive"),
            ("bfm", "neutral") => (3000., 300., 0., h + 180., v, "BFM neutral"),
            ("bfm", "perch") => (1800., 0., -900., h, v * 0.9, "BFM high perch"),
            ("bfm", _) => (9000., 0., 0., h + 180., v, "BFM head-on"),
            (_, "flank") => (range_nm * util::NM, 0., 0., h + 180. + 45., 250., "BVR flanking"),
            (_, "beam") => (range_nm * util::NM, 0., 0., h + 90., 250., "BVR beaming"),
            (_, "high") => (range_nm * util::NM, 0., 3000., h + 180., 250., "BVR hot, high"),
            (_, "low") => (range_nm * util::NM, 0., -3000., h + 180., 250., "BVR hot, low"),
            _ => (range_nm * util::NM, 0., 0., h + 180., 250., "BVR hot"),
        };
        let mut pos = util::offset(p, h, fwd, right);
        pos.y = (p.y + dalt).max(util::ground_height(lua, pos) + 300.);
        let loadout = loadout(&adv, weapons);
        if weapons != "guns" && loadout.is_empty() {
            warn!("no {weapons} loadout could be built for {}", adv.typ)
        }
        let name = spawns.next_name("ADV");
        let side = player.side.opposite();
        let country = spawn::country_id(&adv.country)
            .or_else(|_| spawn::country_id(spawn::default_country(side)))?;
        let adv_hdg = adv_hdg.rem_euclid(360.);
        let far = util::offset(pos, adv_hdg, 60_000., 0.);
        let mut tasks = vec![
            spawn::wrapped_option(1, 0, json!(0)), // ROE weapons free
            spawn::wrapped_option(2, 1, json!(2)), // reaction: evade fire
            spawn::wrapped_option(3, 3, json!(3)), // radar: continuous search
        ];
        if kind == "bfm" {
            tasks.push(spawn::task_entry(
                4,
                json!({ "id": "AttackUnit", "params": { "unitId": player.unit_id.inner(), "groupAttack": true, "expend": "Auto", "attackQtyLimit": false } }),
            ));
        } else {
            tasks.push(spawn::task_entry(
                4,
                json!({ "id": "EngageTargets", "params": { "targetTypes": ["Air"], "priority": 0, "maxDist": 150000 } }),
            ));
        }
        let spec = AirSpec {
            name: name.clone(),
            typ: adv.typ.clone(),
            count: count.clamp(1, 4),
            skill: skill.into(),
            pos,
            alt_m: pos.y,
            speed_ms: adv_speed,
            hdg: adv_hdg,
            pylons: loadout,
            livery: adv.livery.clone(),
            callsign: None,
            freq_mhz: None,
            task: "CAP".into(),
            route: vec![
                spawn::waypoint(pos, pos.y, adv_speed, tasks),
                spawn::waypoint(far, pos.y, adv_speed, vec![]),
            ],
            fuel_kg: harvest::max_fuel_kg(&adv.typ),
            side,
        };
        spawn::add_group(lua, country, spawn::AIRPLANE, &spawn::air_group(&spec))?;
        spawns.defer(now + 1.5, spawn::Pending::GroupCommand { group: name.clone(), cmd: spawn::bool_cmd("SetUnlimitedFuel", true) });
        self.seq += 1;
        let eid = self.seq;
        let spawn_id = name.clone();
        spawns.insert(Spawn {
            id: spawn_id.clone(),
            item: kind.into(),
            label: format!("{label}: {} x{} ({skill}, {weapons})", adv.label, spec.count),
            owner: Some(player.ucid.to_string()),
            owner_name: player.name.clone(),
            groups: vec![name.clone()],
            statics: vec![],
            created: Utc::now(),
            expires: Some(Utc::now() + Duration::seconds(cfg.spawn.despawn_after_s.min(1800) as i64)),
            units: spec.count,
            pos,
            kind: SpawnKind::Adversary { engagement: eid },
        });
        self.engagements.insert(
            eid,
            Engagement {
                setup: format!("{label} ({weapons})"),
                player: side_of(player),
                adversary_group: Some(name),
                adversary_type: adv.typ.clone(),
                skill: skill.into(),
                opponent: None,
                spawn_id: Some(spawn_id.clone()),
                started: now,
                shots: Default::default(),
                trainer_kills: Default::default(),
                gun_hits: Default::default(),
                killed: None,
                track: Default::default(),
                last_sample: f64::MIN,
                notes: vec![],
            },
        );
        records::to_group(
            lua,
            player.group_id,
            &format!(
                "{label}: {} x{} spawned {:03.0} for {:.1} nm. Fight's on!",
                adv.label,
                spec.count,
                util::bearing(p, pos),
                util::dist2(p, pos) / util::NM
            ),
            10,
        );
        info!("engagement {eid} started for {} ({label})", player.name);
        Ok(spawn_id)
    }

    // ------------------------------------------------------------ duels

    pub fn challenge(&mut self, lua: MizLua, players: &Players, from: &Flying, now: f64) -> Result<()> {
        let to = players
            .flying
            .values()
            .filter(|f| f.ucid != from.ucid && f.in_air && !f.is_helo && !f.is_ground)
            .map(|f| (f, util::dist3(f.pos, from.pos)))
            .filter(|(_, d)| *d < 20. * util::NM)
            .min_by(|a, b| a.1.total_cmp(&b.1))
            .map(|(f, _)| f)
            .ok_or_else(|| anyhow!("no airborne player within 20 nm to challenge"))?;
        self.challenges.retain(|c| c.from != from.ucid);
        self.challenges.push(Challenge { from: from.ucid, to: to.ucid, expires: now + 90. });
        records::to_group(
            lua,
            to.group_id,
            &format!(
                "DUEL: {} ({}) challenges you. F10 Range > Air-to-Air > Duel > Accept within 90 s.",
                from.name, from.typ
            ),
            20,
        );
        records::to_group(lua, from.group_id, &format!("Challenge sent to {} ({}).", to.name, to.typ), 10);
        Ok(())
    }

    pub fn accept(&mut self, lua: MizLua, players: &Players, me: &Flying, now: f64) -> Result<()> {
        self.challenges.retain(|c| c.expires > now);
        let c = self
            .challenges
            .iter()
            .position(|c| c.to == me.ucid)
            .ok_or_else(|| anyhow!("no open challenge for you"))?;
        let c = self.challenges.remove(c);
        let other = players
            .by_ucid(&c.from)
            .cloned()
            .ok_or_else(|| anyhow!("the challenger is no longer flying"))?;
        self.seq += 1;
        let eid = self.seq;
        for (a, b) in [(me, &other), (&other, me)] {
            records::to_group(
                lua,
                a.group_id,
                &format!(
                    "DUEL vs {} ({}): FIGHT'S ON. Missile trainer protects both; {} gun hits wins.",
                    b.name, b.typ, 10
                ),
                15,
            );
        }
        self.engagements.insert(
            eid,
            Engagement {
                setup: "Duel".into(),
                player: side_of(me),
                adversary_group: None,
                adversary_type: other.typ.clone(),
                skill: String::new(),
                opponent: Some(side_of(&other)),
                spawn_id: None,
                started: now,
                shots: Default::default(),
                trainer_kills: Default::default(),
                gun_hits: Default::default(),
                killed: None,
                track: Default::default(),
                last_sample: f64::MIN,
                notes: vec![],
            },
        );
        Ok(())
    }

    pub fn cancel_duels(&mut self, ucid: &Ucid) {
        self.challenges.retain(|c| &c.from != ucid && &c.to != ucid);
        for e in self.engagements.values_mut() {
            if e.opponent.is_some()
                && (e.player.ucid.as_ref() == Some(ucid)
                    || e.opponent.as_ref().and_then(|o| o.ucid.as_ref()) == Some(ucid))
            {
                e.killed = Some("__abort__".into());
            }
        }
    }

    /// A gun (or any non-tracked weapon) hit `target` from `shooter`.
    pub fn gun_hit(&mut self, shooter_unit: &str, target_unit: &str) {
        for e in self.engagements.values_mut() {
            let adv = |u: &str| {
                e.adversary_group.as_ref().map(|g| spawn::in_group(u, g)).unwrap_or(false)
                    || e.opponent.as_ref().map(|o| o.unit == u).unwrap_or(false)
            };
            if (e.player.unit == shooter_unit && adv(target_unit)) || (adv(shooter_unit) && e.player.unit == target_unit) {
                *e.gun_hits.entry(shooter_unit.to_string()).or_default() += 1;
            }
        }
    }

    pub fn trainer_kill(&mut self, lua: MizLua, k: &TrainerKill) {
        for e in self.engagements.values_mut() {
            let adv = |u: &str| {
                e.adversary_group.as_ref().map(|g| spawn::in_group(u, g)).unwrap_or(false)
                    || e.opponent.as_ref().map(|o| o.unit == u).unwrap_or(false)
            };
            let involved = (e.player.unit == k.shooter_unit && adv(&k.target_unit))
                || (adv(&k.shooter_unit) && e.player.unit == k.target_unit);
            if involved {
                *e.trainer_kills.entry(k.shooter_unit.clone()).or_default() += 1;
                e.notes.push(format!("{} trainer kill on {} with {}", k.shooter_unit, k.target_unit, k.weapon));
                e.killed.get_or_insert(k.target_unit.clone());
                // an AI "killed" by the trainer blows up, so the pilot sees
                // the splash; a player never does
                if e.adversary_group.as_ref().map(|g| spawn::in_group(&k.target_unit, g)).unwrap_or(false) {
                    if let Ok(u) = Unit::get_by_name(lua, &k.target_unit) {
                        if let Ok(p) = u.get_point() {
                            let _ = Trigger::singleton(lua)
                                .and_then(|t| t.action())
                                .and_then(|a| a.explosion(LuaVec3(p.0), 300.));
                        }
                    }
                }
            }
        }
    }

    /// A unit died for real (DEAD / crash / pilot dead).
    pub fn unit_dead(&mut self, unit: &str) {
        for e in self.engagements.values_mut() {
            let adv = e.adversary_group.as_ref().map(|g| spawn::in_group(unit, g)).unwrap_or(false)
                || e.opponent.as_ref().map(|o| o.unit == unit).unwrap_or(false);
            if (adv || e.player.unit == unit) && e.killed.is_none() {
                e.killed = Some(unit.to_string());
            }
        }
    }

    /// The player left their aircraft (landed, slot change, disconnect).
    pub fn player_left(&mut self, unit: &str) {
        for e in self.engagements.values_mut() {
            if (e.player.unit == unit || e.opponent.as_ref().map(|o| o.unit == unit).unwrap_or(false))
                && e.killed.is_none()
            {
                e.killed = Some("__abort__".into());
            }
        }
    }

    /// Resolve finished engagements; sample tracks.
    pub fn slow_tick(&mut self, lua: MizLua, cfg: &RangeCfg, spawns: &mut Spawns, rec: &mut Recorder, now: f64) {
        let gun_kill = cfg.air_to_air.gun_kill_hits;
        let mut done = vec![];
        for (eid, e) in self.engagements.iter_mut() {
            // tracks at 1 Hz
            if now - e.last_sample >= 1. {
                e.last_sample = now;
                let mut units = vec![(e.player.name.clone(), e.player.unit.clone())];
                if let Some(o) = &e.opponent {
                    units.push((o.name.clone(), o.unit.clone()));
                }
                if let Some(g) = &e.adversary_group {
                    units.push((e.adversary_type.clone(), format!("{g}-1")));
                }
                for (label, u) in units {
                    if let Ok(u) = Unit::get_by_name(lua, &u) {
                        if let (Ok(p), Ok(v)) = (u.get_point(), u.get_velocity()) {
                            e.track.entry(label).or_default().push(sample(lua, now - e.started, p.0, v.0));
                        }
                    }
                }
            }
            let adv_alive = e.adversary_group.as_ref().map(|g| spawn::group_exists(lua, g)).unwrap_or(true);
            let player_guns = e.gun_hits.get(&e.player.unit).copied().unwrap_or(0);
            let opp_guns = e
                .opponent
                .as_ref()
                .map(|o| e.gun_hits.get(&o.unit).copied().unwrap_or(0))
                .unwrap_or_else(|| {
                    e.gun_hits
                        .iter()
                        .filter(|(u, _)| e.adversary_group.as_ref().map(|g| spawn::in_group(u, g)).unwrap_or(false))
                        .map(|(_, n)| *n)
                        .sum()
                });
            let outcome = match &e.killed {
                Some(k) if k == "__abort__" => Some(EngagementOutcome::Abort),
                Some(k) if *k == e.player.unit => Some(EngagementOutcome::Loss),
                Some(_) => Some(EngagementOutcome::Win),
                None if !adv_alive => Some(EngagementOutcome::Win),
                None if e.opponent.is_some() && player_guns >= gun_kill => Some(EngagementOutcome::Win),
                None if e.opponent.is_some() && opp_guns >= gun_kill => Some(EngagementOutcome::Loss),
                None if now - e.started > 900. => Some(EngagementOutcome::Draw),
                None => None,
            };
            if let Some(o) = outcome {
                done.push((*eid, o));
            }
        }
        for (id, outcome) in done {
            let Some(e) = self.engagements.remove(&id) else { continue };
            let duration = now - e.started;
            let emit = |rec: &mut Recorder, me: &Side_, them: Option<&Side_>, o: EngagementOutcome| {
                let res = EngagementResult {
                    setup: e.setup.clone(),
                    adversary: them.map(|t| t.name.clone()).unwrap_or_else(|| e.adversary_type.clone()),
                    adversary_skill: e.skill.clone(),
                    opponent: them.map(|t| PilotRef { ucid: t.ucid.map(|u| u.to_string()), name: t.name.clone() }),
                    outcome: o,
                    duration_s: duration,
                    shots_fired: e.shots.get(&me.unit).copied().unwrap_or(0),
                    trainer_kills: e.trainer_kills.get(&me.unit).copied().unwrap_or(0),
                    gun_hits: e.gun_hits.get(&me.unit).copied().unwrap_or(0),
                    notes: e.notes.clone(),
                };
                let score = match o {
                    EngagementOutcome::Win => Some(5.),
                    EngagementOutcome::Draw => Some(3.),
                    EngagementOutcome::Loss => Some(1.),
                    EngagementOutcome::Abort => None,
                };
                if let Some(g) = me.group {
                    records::to_group(
                        lua,
                        g,
                        &format!(
                            "{} vs {}: {} after {:.0} s",
                            e.setup,
                            res.adversary,
                            o.label(),
                            duration
                        ),
                        15,
                    );
                }
                rec.emit(
                    lua,
                    PilotRef { ucid: me.ucid.map(|u| u.to_string()), name: me.name.clone() },
                    &me.typ,
                    me.side,
                    &me.callsign,
                    score,
                    RangeResult::Engagement(res),
                    Some(Track::Path { paths: e.track.clone() }),
                );
            };
            emit(rec, &e.player, e.opponent.as_ref(), outcome);
            if let Some(o) = &e.opponent {
                let flip = match outcome {
                    EngagementOutcome::Win => EngagementOutcome::Loss,
                    EngagementOutcome::Loss => EngagementOutcome::Win,
                    x => x,
                };
                emit(rec, o, Some(&e.player), flip);
            }
            if let Some(sid) = &e.spawn_id {
                spawns.remove(lua, sid);
            }
        }
    }

    /// The adversary was despawned (by request or expiry): that is an abort,
    /// not a win.
    pub fn abort(&mut self, eid: u64) {
        if let Some(e) = self.engagements.get_mut(&eid) {
            e.killed.get_or_insert("__abort__".into());
            e.spawn_id = None;
        }
    }

    /// Duels in progress, for the live page's arena list.
    pub fn duels(&self) -> Vec<(String, String)> {
        self.engagements
            .values()
            .filter_map(|e| e.opponent.as_ref().map(|o| (e.player.name.clone(), o.name.clone())))
            .collect()
    }
}

fn side_of(f: &Flying) -> Side_ {
    Side_ {
        unit: f.unit_name.clone(),
        ucid: Some(f.ucid),
        name: f.name.clone(),
        typ: f.typ.clone(),
        side: f.side,
        group: Some(f.group_id),
        callsign: f.group_name.clone(),
    }
}
