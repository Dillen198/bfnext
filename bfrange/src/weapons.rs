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
    LuaEnv, LuaVec3, MizLua,
    coalition::Side,
    env::miz::GroupId,
    land::Land,
    net::Ucid,
    object::{DcsObject, DcsOid},
    weapon::{
        ClassWeapon, GuidanceType, MissileCategory, Weapon, WeaponCategory, WeaponDesc,
    },
};
use log::{debug, info, warn};
use mlua::FromLua;
use std::sync::OnceLock;

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

/// DCS's own `Weapon.GuidanceType` numbering, as (number, our label), read
/// once per DCS process by `read_guidance_table`.
static LIVE_GUIDANCE: OnceLock<Vec<(u8, &'static str)>> = OnceLock::new();

fn label_of_dcs_key(k: &str) -> Option<&'static str> {
    Some(match k {
        "INS" => "ins",
        "IR" => "ir",
        "RADAR_ACTIVE" | "RADAR_MMW" | "MMW" => "radar",
        "RADAR_SEMI_ACTIVE" => "sarh",
        "RADAR_PASSIVE" => "arm",
        "TV" => "tv",
        "LASER" => "laser",
        "TELE" => "command",
        _ => return None,
    })
}

/// Log DCS's `Weapon.GuidanceType` table once, and check dcso3's
/// `GuidanceType` numbering against it. dcso3 numbers MmwRadar = 3, which
/// the scripting docs don't list; if DCS really has no MMW entry every
/// value from 3 up decodes one off (an ARM as `Tv`), which broke ARM
/// detection in the IADS trainer. When the two disagree, labels and the ARM
/// test follow DCS's table (see `guidance_str`).
pub fn read_guidance_table(lua: MizLua) {
    if LIVE_GUIDANCE.get().is_some() {
        return;
    }
    let r = (|| -> anyhow::Result<Vec<(std::string::String, i64)>> {
        let w: mlua::Table = lua.inner().globals().raw_get("Weapon")?;
        let g: mlua::Table = w.raw_get("GuidanceType")?;
        let mut v = vec![];
        for kv in g.pairs::<std::string::String, i64>() {
            v.push(kv?);
        }
        v.sort_by_key(|(_, n)| *n);
        Ok(v)
    })();
    let table = match r {
        Ok(t) => t,
        Err(e) => {
            warn!("could not read Weapon.GuidanceType: {e:?}");
            return;
        }
    };
    let listed: Vec<std::string::String> =
        table.iter().map(|(k, n)| format!("{k}={n}")).collect();
    info!("DCS Weapon.GuidanceType: {}", listed.join(", "));
    let mut live = vec![];
    let mut mismatch = vec![];
    for (k, n) in &table {
        let Some(label) = label_of_dcs_key(k) else {
            warn!(
                "Weapon.GuidanceType.{k}={n} is new to the range: shown as dcso3 decodes it"
            );
            continue;
        };
        let Ok(n8) = u8::try_from(*n) else { continue };
        let dcso3 = GuidanceType::from_lua(mlua::Value::Integer(*n), lua.inner()).ok();
        let theirs = dcso3_label(dcso3);
        if theirs != label {
            mismatch.push(format!("{k}={n} decodes as {theirs}"));
        }
        live.push((n8, label));
    }
    if !mismatch.is_empty() {
        warn!(
            "dcso3's GuidanceType numbering disagrees with DCS ({}); guidance labels and ARM detection use DCS's table",
            mismatch.join(", ")
        );
    }
    let _ = LIVE_GUIDANCE.set(live);
}

/// The guidance label recorded for a weapon: DCS's own numbering when it
/// has been read, dcso3's otherwise.
pub fn guidance_str(g: Option<GuidanceType>) -> &'static str {
    if let (Some(g), Some(live)) = (g, LIVE_GUIDANCE.get()) {
        if let Some((_, l)) = live.iter().find(|(n, _)| *n == g as u8) {
            return l;
        }
    }
    dcso3_label(g)
}

/// An anti-radiation (passive radar) missile?
pub fn is_arm(g: Option<GuidanceType>) -> bool {
    guidance_str(g) == "arm"
}

fn dcso3_label(g: Option<GuidanceType>) -> &'static str {
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
            debug!(
                "{} from {} impacted after {:.1}s",
                t.weapon,
                t.shooter.name,
                now - t.t0
            );
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
            if let Ok(Some(ip)) =
                land.get_ip_opt(LuaVec3(start), LuaVec3(dir), reach + 5.)
            {
                return ip.0;
            }
        }
    }
    let mut q = p;
    q.y = util::ground_height(lua, p);
    q
}
