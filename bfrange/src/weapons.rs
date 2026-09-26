// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Following air-to-ground weapons from release to impact.
//!
//! MOOSE RANGE's method: on SHOT, read the weapon's position every tick; the
//! tick after it stops existing, the impact point is where its last position
//! and velocity ray meets the ground (`land.getIP`). At 20 Hz a bomb falling
//! at 300 m/s moves 15 m between reads, and the ray cast from the last read
//! recovers the rest, so impacts are good to a metre or two.
//!
//! Air-to-air missiles don't come here -- the missile trainer (`aa.rs`) owns
//! them.

use crate::util::{self, V3};
use bfprotocols::range::{TrackPt, WeaponClass};
use dcso3::{
    coalition::Side,
    env::miz::GroupId,
    land::Land,
    net::Ucid,
    object::{DcsObject, DcsOid},
    weapon::{ClassWeapon, GuidanceType, MissileCategory, Weapon, WeaponCategory, WeaponDesc},
    LuaVec3, MizLua,
};
use log::debug;

/// Who fired it.
#[derive(Debug, Clone)]
pub struct Shooter {
    pub ucid: Option<Ucid>,
    pub name: String,
    pub unit_name: String,
    pub group_id: Option<GroupId>,
    pub typ: String,
    pub side: Side,
    pub callsign: String,
}

impl Shooter {
    pub fn is_player(&self) -> bool {
        self.ucid.is_some()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Purpose {
    AirToAir,
    AirToGround,
    AntiShip,
    Ignore,
}

pub fn guidance_str(g: Option<GuidanceType>) -> &'static str {
    match g {
        None => "none",
        Some(GuidanceType::Ins) => "ins",
        Some(GuidanceType::IrHoming) => "ir",
        Some(GuidanceType::MmwRadar) | Some(GuidanceType::ActiveRadar) => "radar",
        Some(GuidanceType::SemiActiveRadar) => "sarh",
        Some(GuidanceType::PassiveRadar) => "arm",
        Some(GuidanceType::Tv) => "tv",
        Some(GuidanceType::Laser) => "laser",
        Some(GuidanceType::Telecontrol) => "command",
    }
}

fn is_cluster(name: &str) -> bool {
    let n = name.to_ascii_uppercase();
    ["CBU", "RBK", "MK-20", "MK_20", "BL755", "BLU-3", "ROCKEYE", "BELOUGA", "KMGU"]
        .iter()
        .any(|k| n.contains(k))
}

pub fn classify(desc: &WeaponDesc) -> (Purpose, WeaponClass) {
    match desc.category {
        Some(WeaponCategory::Missile) => match desc.missile_category {
            Some(MissileCategory::Aam) | Some(MissileCategory::Sam) => {
                (Purpose::AirToAir, WeaponClass::Missile)
            }
            Some(MissileCategory::AntiShip) => (Purpose::AntiShip, WeaponClass::Missile),
            _ => (Purpose::AirToGround, WeaponClass::Missile),
        },
        Some(WeaponCategory::Bomb) => {
            if is_cluster(&desc.type_name) {
                (Purpose::AirToGround, WeaponClass::Cluster)
            } else if desc.guidance.is_some() {
                (Purpose::AirToGround, WeaponClass::Guided)
            } else {
                (Purpose::AirToGround, WeaponClass::Unguided)
            }
        }
        Some(WeaponCategory::Rocket) => (Purpose::AirToGround, WeaponClass::Rocket),
        Some(WeaponCategory::Torpedo) => (Purpose::AntiShip, WeaponClass::Missile),
        Some(WeaponCategory::Shell) | None => (Purpose::Ignore, WeaponClass::Gun),
    }
}

#[derive(Debug, Clone)]
pub struct Tracked {
    pub oid: DcsOid<ClassWeapon>,
    pub weapon: String,
    pub display: String,
    pub class: WeaponClass,
    pub purpose: Purpose,
    pub guidance: &'static str,
    pub shooter: Shooter,
    /// mission time of release
    pub t0: f64,
    pub rel_pos: V3,
    pub rel_vel: V3,
    pub last_pos: V3,
    pub last_vel: V3,
    pub last_t: f64,
    pub pts: Vec<TrackPt>,
    last_pt_t: f64,
    /// name of the object the weapon was guiding on at release, if any
    pub target_name: Option<String>,
    /// objects this weapon hit (HIT events carrying its weapon object)
    pub hits: Vec<String>,
    pub max_range_m: Option<f64>,
}

/// A weapon that has come to the end of its flight.
#[derive(Debug, Clone)]
pub struct Impact {
    pub w: Tracked,
    pub pos: V3,
    pub tof: f64,
}

#[derive(Debug, Default)]
pub struct Tracker {
    pub live: Vec<Tracked>,
}

impl Tracker {
    #[allow(clippy::too_many_arguments)]
    pub fn start(
        &mut self,
        weapon: &Weapon,
        desc: WeaponDesc,
        purpose: Purpose,
        class: WeaponClass,
        shooter: Shooter,
        shooter_pos: V3,
        shooter_vel: V3,
        now: f64,
    ) -> anyhow::Result<()> {
        let oid = weapon.object_id()?;
        let pos = weapon.get_point().map(|p| p.0).unwrap_or(shooter_pos);
        let vel = weapon.get_velocity().map(|v| v.0).unwrap_or(shooter_vel);
        let target_name = weapon
            .get_target()
            .ok()
            .flatten()
            .and_then(|t| t.get_name().ok())
            .map(|s| s.to_string());
        self.live.push(Tracked {
            oid,
            weapon: desc.type_name.clone(),
            display: if desc.display_name.is_empty() {
                desc.type_name.clone()
            } else {
                desc.display_name.clone()
            },
            class,
            purpose,
            guidance: guidance_str(desc.guidance),
            shooter,
            t0: now,
            rel_pos: shooter_pos,
            rel_vel: shooter_vel,
            last_pos: pos,
            last_vel: vel,
            last_t: now,
            pts: vec![],
            last_pt_t: f64::MIN,
            target_name,
            hits: vec![],
            max_range_m: desc.range_max_m,
        });
        Ok(())
    }

    pub fn active(&self) -> bool {
        !self.live.is_empty()
    }

    /// A HIT event carrying one of our weapons.
    pub fn record_hit(&mut self, oid: &DcsOid<ClassWeapon>, target: String) {
        if let Some(w) = self.live.iter_mut().find(|w| &w.oid == oid) {
            if !w.hits.contains(&target) {
                w.hits.push(target)
            }
        }
    }

    /// Advance every weapon; return the ones that have impacted.
    pub fn tick(&mut self, lua: MizLua, now: f64) -> Vec<Impact> {
        let mut done = vec![];
        let mut i = 0;
        while i < self.live.len() {
            let alive = match Weapon::get_instance(lua, &self.live[i].oid) {
                Ok(w) => match (w.get_point(), w.get_velocity()) {
                    (Ok(p), Ok(v)) => {
                        let t = &mut self.live[i];
                        t.last_pos = p.0;
                        t.last_vel = v.0;
                        t.last_t = now;
                        if now - t.last_pt_t >= 0.25 {
                            t.last_pt_t = now;
                            let g = util::geo(lua, p.0);
                            t.pts.push(TrackPt {
                                t: now - t.t0,
                                lat: g.lat,
                                lon: g.lon,
                                alt_m: p.0.y,
                                speed_kts: v.0.norm() * util::MS_TO_KTS,
                            });
                        }
                        true
                    }
                    _ => false,
                },
                Err(_) => false,
            };
            if alive {
                i += 1;
                continue;
            }
            let t = self.live.swap_remove(i);
            let pos = impact_point(lua, t.last_pos, t.last_vel, now - t.last_t);
            debug!("{} from {} impacted after {:.1}s", t.weapon, t.shooter.name, now - t.t0);
            done.push(Impact { tof: now - t.t0, pos, w: t });
        }
        done
    }

    /// Weapons still in the air a long time after release (a stuck object)
    /// are dropped rather than tracked forever.
    pub fn reap(&mut self, now: f64) {
        self.live.retain(|w| now - w.t0 < 600.)
    }
}

/// Where a weapon last seen at `p` moving at `v` met the ground.
pub fn impact_point(lua: MizLua, p: V3, v: V3, since: f64) -> V3 {
    let speed = v.norm();
    if speed > 1. {
        let dir = v / speed;
        let reach = (speed * (since + 0.2)).max(100.);
        if let Ok(land) = Land::singleton(lua) {
            // start a little behind the last read, so a weapon read just
            // below the surface of a slope still intersects it
            let start = p - dir * 5.;
            if let Ok(Some(ip)) = land.get_ip_opt(LuaVec3(start), LuaVec3(dir), reach + 5.) {
                return ip.0;
            }
        }
    }
    let mut q = p;
    q.y = util::ground_height(lua, p);
    q
}
