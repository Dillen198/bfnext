// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Combat search and rescue.
//!
//! A helicopter pilot asks for a mission in a CSAR area and a downed pilot
//! appears somewhere in it: on open, not-too-steep ground, never in water.
//! The MAYDAY gives only a rough position (a circle on the F10 map a few km
//! across); the survivor's beacon transmits on an ADF frequency when the
//! mission file carries a beacon sound; and he pops smoke -- or a flare at
//! night -- when he hears a helicopter within 3 km. Pick him up by landing
//! next to him (or a steady low hover) and bring him to any friendly
//! airfield, FARP or ship. A "hot" rescue puts enemy troops in the area who
//! really shoot.

use crate::{
    ag,
    players::{Flying, Players},
    records::{self, Recorder},
    spawn::{self, Spawns},
    util::{self, V3},
};
use anyhow::{anyhow, bail, Result};
use bfprotocols::range::{
    cfg::{CsarAreaCfg, RangeCfg},
    grading, CsarOutcome, CsarResult, LiveCsar, RangeResult,
};
use chrono::{DateTime, Utc};
use dcso3::{
    coalition::Side,
    env::miz::GroupId,
    trigger::{CircleSpec, FlareColor, LineType, MarkId, SideFilter, SmokeColor, Trigger},
    Color, LuaVec3, MizLua,
};
use log::{info, warn};
use serde_json::json;

const PICKUP_M: f64 = 50.;
const PICKUP_HOLD_S: f64 = 8.;
const HEAR_M: f64 = 3000.;
const TIMEOUT_S: f64 = 3600.;

#[derive(Debug)]
struct Mission {
    id: String,
    area: usize,
    side: Side,
    owner: String,
    owner_gid: GroupId,
    survivor: String,
    pos: V3,
    fuzz: V3,
    hostile: Option<String>,
    beacon: Option<String>,
    mark: Option<MarkId>,
    started: f64,
    started_utc: DateTime<Utc>,
    last_smoke: f64,
    hold: Option<(String, f64)>,
    /// (helicopter unit, time, method)
    picked: Option<(String, f64, &'static str)>,
}

#[derive(Debug, Default)]
pub struct Csar {
    areas: Vec<(CsarAreaCfg, V3)>,
    missions: Vec<Mission>,
    seq: u64,
    rng: u64,
    beacon_file: Option<String>,
}

fn for_side(s: &str, side: Side) -> bool {
    match s {
        "blue" => side == Side::Blue,
        "red" => side == Side::Red,
        _ => true,
    }
}

impl Csar {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg) {
        self.rng = 0xD1B5_4A32_D192_ED03;
        self.beacon_file = cfg.csar.beacon_file.clone();
        for a in &cfg.csar.areas {
            match ag::resolve(lua, &a.loc) {
                Ok(p) => self.areas.push((a.clone(), p)),
                Err(e) => warn!("CSAR area {}: {e:?}", a.id),
            }
        }
        info!("CSAR: {} areas", self.areas.len());
    }

    fn rand(&mut self) -> f64 {
        let mut x = self.rng;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.rng = x;
        (x >> 11) as f64 / (1u64 << 53) as f64
    }

    pub fn list_all(&self) -> Vec<(String, String)> {
        self.areas.iter().map(|(a, _)| (a.id.clone(), a.name.clone())).collect()
    }

    /// Somewhere a man could be standing: dry, below 3000 m, not a cliff.
    fn find_spot(&mut self, lua: MizLua, centre: V3, radius: f64) -> Option<V3> {
        for _ in 0..60 {
            let a = self.rand() * 360.;
            let r = radius * self.rand().sqrt() * 0.9;
            let mut p = util::offset(centre, a, r, 0.);
            if util::is_water(lua, p) {
                continue;
            }
            p.y = util::ground_height(lua, p);
            if p.y > 3000. {
                continue;
            }
            let slope_ok = [0., 90., 180., 270.].iter().all(|h| {
                let q = util::offset(p, *h, 25., 0.);
                (util::ground_height(lua, q) - p.y).abs() < 7.
            });
            if slope_ok {
                return Some(p);
            }
        }
        None
    }

    /// F10: start a rescue in an area.
    pub fn request(&mut self, lua: MizLua, f: &Flying, area_id: &str, hostile: bool, spawns: &mut Spawns, now: f64) -> Result<String> {
        if !f.is_helo {
            bail!("CSAR is flown in a helicopter")
        }
        if self.missions.iter().any(|m| m.owner == f.ucid.to_string() && m.picked.is_none()) {
            bail!("you already have a survivor waiting")
        }
        let ai = self.areas.iter().position(|(a, _)| a.id == area_id).ok_or_else(|| anyhow!("no CSAR area {area_id}"))?;
        let (area, centre) = self.areas[ai].clone();
        if !for_side(&area.side, f.side) {
            bail!("{} is laid out for the other side", area.name)
        }
        let pos = self.find_spot(lua, centre, area.radius_m).ok_or_else(|| anyhow!("no open ground found in {}; try again", area.name))?;
        self.seq += 1;
        let id = format!("{}", self.seq);
        // the survivor: friendly, invisible to the enemy, can't be killed
        let survivor = format!("RNG-CSAR-{id}");
        let (man, country) = if f.side == Side::Blue { ("Soldier M4", "CJTF_BLUE") } else { ("Infantry AK", "CJTF_RED") };
        let g = spawn::surface_group(&survivor, &[(man.to_string(), pos, self.rand() * 360.)], "Average", vec![spawn::ground_waypoint(pos, 0., false, vec![])]);
        spawn::add_group(lua, spawn::country_id(country)?, spawn::GROUND, &g)?;
        for c in [spawn::bool_cmd("SetImmortal", true), spawn::bool_cmd("SetInvisible", true)] {
            spawns.defer(now + 1.5, spawn::Pending::GroupCommand { group: survivor.clone(), cmd: c });
        }
        spawns.defer(now + 1.5, spawn::Pending::GroupOption { group: survivor.clone(), id: 0, value: json!(4) });
        // a hot rescue: a squad hunting him a kilometre or so away
        let hostile_group = if hostile {
            let name = format!("RNG-CSARHOT-{id}");
            let hp = util::offset(pos, self.rand() * 360., 900. + 400. * self.rand(), 0.);
            let hp = util::nearest_land(lua, hp, 300.).unwrap_or(hp);
            let kit: &[&str] = if f.side == Side::Blue {
                &["Infantry AK", "Infantry AK", "Paratrooper RPG-16", "SA-18 Igla manpad", "Ural-375 ZU-23"]
            } else {
                &["Soldier M4", "Soldier M4", "Soldier M249", "Soldier stinger", "M1043 HMMWV Armament"]
            };
            let units: Vec<(String, V3, f64)> = kit
                .iter()
                .enumerate()
                .map(|(i, t)| {
                    let mut q = util::offset(hp, i as f64 * 72., if i == 0 { 0. } else { 20. }, 0.);
                    q.y = util::ground_height(lua, q);
                    (t.to_string(), q, util::bearing(q, pos))
                })
                .collect();
            let g = spawn::surface_group(&name, &units, "Average", vec![spawn::ground_waypoint(hp, 0., false, vec![])]);
            spawn::add_group(lua, spawn::country_id(spawn::default_country(f.side.opposite()))?, spawn::GROUND, &g)?;
            spawns.defer(now + 2., spawn::Pending::GroupOption { group: name.clone(), id: 0, value: json!(2) });
            Some(name)
        } else {
            None
        };
        // the beacon
        let beacon = self.beacon_file.as_ref().and_then(|file| {
            let name = format!("CSAR beacon {id}");
            let r = Trigger::singleton(lua).and_then(|t| t.action()).and_then(|a| {
                a.radio_transmission(
                    file.as_str().into(),
                    LuaVec3(V3::new(pos.x, pos.y + 2., pos.z)),
                    dcso3::trigger::Modulation::AM,
                    true,
                    (area.beacon_khz * 1000.) as u64,
                    100,
                    name.as_str().into(),
                )
            });
            match r {
                Ok(()) => Some(name),
                Err(e) => {
                    warn!("CSAR beacon: {e:?}");
                    None
                }
            }
        });
        // the MAYDAY: only a rough position, and a circle on the map
        let fuzz = util::offset(pos, self.rand() * 360., 800. + 1200. * self.rand(), 0.);
        let mark = MarkId::new();
        let _ = Trigger::singleton(lua).and_then(|t| t.action()).and_then(|a| {
            a.circle_to_all(
                if f.side == Side::Blue { SideFilter::Blue } else { SideFilter::Red },
                mark,
                CircleSpec {
                    center: LuaVec3(fuzz),
                    radius: 2500.,
                    color: Color::new(0.5, 1., 0.3, 1.),
                    fill_color: Color::new(0.5, 1., 0.3, 0.12),
                    line_type: LineType::Dashed,
                    read_only: true,
                },
                Some(format!("MAYDAY {id}: survivor within this circle").as_str().into()),
            )
        });
        let g = util::geo(lua, fuzz);
        let msg = format!(
            "MAYDAY MAYDAY: pilot down in {} (CSAR {id}{}). Last known position {:.2}N {:.2}E, {:03.0}/{:.0}nm from you, circled on your F10 map.{} He pops smoke (flare at night) when he hears you within 3 km. Land within {PICKUP_M:.0} m of him, then bring him to any friendly airfield, FARP or ship.",
            area.name,
            if hostile { ", HOT: enemy troops in the area" } else { "" },
            g.lat,
            g.lon,
            util::bearing(f.pos, fuzz),
            util::dist2(f.pos, fuzz) / util::NM,
            if beacon.is_some() { format!(" Beacon {:.0} kHz AM: tune your ADF.", area.beacon_khz) } else { String::new() }
        );
        records::to_group(lua, f.group_id, &msg, 60);
        info!("CSAR {id} for {} in {} at {:.4} {:.4}{}", f.name, area.id, util::geo(lua, pos).lat, util::geo(lua, pos).lon, if hostile { " (hot)" } else { "" });
        self.missions.push(Mission {
            id: id.clone(),
            area: ai,
            side: f.side,
            owner: f.ucid.to_string(),
            owner_gid: f.group_id,
            survivor,
            pos,
            fuzz,
            hostile: hostile_group,
            beacon,
            mark: Some(mark),
            started: now,
            started_utc: Utc::now(),
            last_smoke: f64::MIN,
            hold: None,
            picked: None,
        });
        Ok(format!("CSAR {id} started"))
    }

    fn cleanup(lua: MizLua, m: &mut Mission) {
        spawn::destroy_group(lua, &m.survivor);
        if let Some(h) = m.hostile.take() {
            spawn::destroy_group(lua, &h);
        }
        if let Some(b) = m.beacon.take() {
            let _ = Trigger::singleton(lua).and_then(|t| t.action()).and_then(|a| a.stop_transmission(b.as_str().into()));
        }
        if let Some(k) = m.mark.take() {
            let _ = Trigger::singleton(lua).and_then(|t| t.action()).and_then(|a| a.remove_mark(k));
        }
    }

    /// Smoke, pickups, timeouts; once a second.
    pub fn tick(&mut self, lua: MizLua, cfg: &RangeCfg, players: &Players, rec: &mut Recorder, night: bool, now: f64) {
        let mut ended = vec![];
        for (mi, m) in self.missions.iter_mut().enumerate() {
            if m.picked.is_some() {
                // the helicopter carrying him is gone
                if let Some((u, ..)) = &m.picked {
                    if !players.flying.contains_key(u) {
                        ended.push((mi, None));
                    }
                }
                continue;
            }
            let helos: Vec<&Flying> = players.flying.values().filter(|f| f.is_helo && f.side == m.side).collect();
            // he hears you
            if helos.iter().any(|f| f.in_air && util::dist2(f.pos, m.pos) < HEAR_M) && now - m.last_smoke > 300. {
                m.last_smoke = now;
                let r = Trigger::singleton(lua).and_then(|t| t.action()).and_then(|a| {
                    if night {
                        a.signal_flare(LuaVec3(m.pos), FlareColor::Green, 0)
                    } else {
                        a.smoke(LuaVec3(V3::new(m.pos.x + 15., m.pos.y, m.pos.z)), SmokeColor::Green)
                    }
                });
                if let Err(e) = r {
                    warn!("CSAR smoke: {e:?}")
                }
                for f in helos.iter().filter(|f| util::dist2(f.pos, m.pos) < HEAR_M * 2.) {
                    records::to_group(lua, f.group_id, &format!("CSAR {}: \"I hear you! {} out!\"", m.id, if night { "Flare" } else { "Green smoke" }), 10);
                }
            }
            // pickup: landed next to him, or a steady low hover over him
            let mut picked = None;
            for f in &helos {
                let d = util::dist2(f.pos, m.pos);
                let landed = !f.in_air && d < PICKUP_M;
                let hover = f.in_air && d < 20. && f.alt_agl < 6. && f.vel.norm() < 3.;
                if landed || hover {
                    match &m.hold {
                        Some((u, t)) if u == &f.unit_name => {
                            if now - t >= PICKUP_HOLD_S {
                                picked = Some((f.unit_name.clone(), if landed { "landed" } else { "hover" }, f.group_id));
                            }
                        }
                        _ => m.hold = Some((f.unit_name.clone(), now)),
                    }
                    break;
                } else if m.hold.as_ref().map(|(u, _)| u == &f.unit_name).unwrap_or(false) {
                    m.hold = None;
                }
            }
            if let Some((u, method, gid)) = picked {
                m.picked = Some((u, now, method));
                spawn::destroy_group(lua, &m.survivor);
                if let Some(b) = m.beacon.take() {
                    let _ = Trigger::singleton(lua).and_then(|t| t.action()).and_then(|a| a.stop_transmission(b.as_str().into()));
                }
                records::to_group(lua, gid, &format!("CSAR {}: survivor on board after {:.0} min. Bring him home: land at any friendly airfield, FARP or ship.", m.id, (now - m.started) / 60.), 20);
            } else if now - m.started > TIMEOUT_S {
                ended.push((mi, None));
            }
        }
        ended.sort_by(|a, b| b.0.cmp(&a.0));
        ended.dedup_by_key(|e| e.0);
        for (mi, place) in ended {
            self.finish(lua, cfg, rec, players, mi, place, now);
        }
    }

    /// A helicopter landed at `place` (an airbase, FARP or ship; None in the
    /// open): if it carries a survivor to a friendly place, he is rescued.
    pub fn landed(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, players: &Players, unit: &str, place: Option<(String, Option<Side>)>, now: f64) {
        let Some(mi) = self.missions.iter().position(|m| m.picked.as_ref().map(|(u, ..)| u == unit).unwrap_or(false)) else { return };
        let side = self.missions[mi].side;
        match place {
            Some((name, s)) if s.map(|s| s == side).unwrap_or(true) => self.finish(lua, cfg, rec, players, mi, Some(name), now),
            _ => (),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn finish(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, players: &Players, mi: usize, delivered: Option<String>, now: f64) {
        let mut m = self.missions.remove(mi);
        let hot = m.hostile.is_some();
        Self::cleanup(lua, &mut m);
        let area = self.areas.get(m.area).map(|a| a.0.name.clone()).unwrap_or_default();
        let outcome = match (&m.picked, &delivered) {
            (Some(_), Some(_)) => CsarOutcome::Rescued,
            (Some(_), None) => CsarOutcome::PickedUp,
            _ => CsarOutcome::Failed,
        };
        let total = delivered.as_ref().map(|_| now - m.started);
        let quality = grading::csar_quality(outcome, total);
        // credit whoever carried him; the requester if nobody did
        let who = m.picked.as_ref().and_then(|(u, ..)| players.flying.get(u)).or_else(|| players.flying.values().find(|f| f.ucid.to_string() == m.owner));
        let Some(f) = who else {
            info!("CSAR {} ended ({}) with nobody to credit", m.id, outcome.label());
            return;
        };
        let res = CsarResult {
            area,
            hostile: hot,
            outcome,
            time_to_pickup_s: m.picked.as_ref().map(|(_, t, _)| t - m.started),
            time_total_s: total,
            pickup_method: m.picked.as_ref().map(|(.., meth)| meth.to_string()).unwrap_or_default(),
            delivered_to: delivered.clone(),
            survivor_pos: util::geo(lua, m.pos),
            quality,
        };
        if cfg.in_game_results {
            records::to_group(
                lua,
                f.group_id,
                &format!(
                    "CSAR {} {}: {}{}{} - {}",
                    m.id,
                    res.area,
                    outcome.label(),
                    res.time_to_pickup_s.map(|t| format!(", on board at {:.0} min", t / 60.)).unwrap_or_default(),
                    total.map(|t| format!(", home at {:.0} min ({})", t / 60., delivered.clone().unwrap_or_default())).unwrap_or_default(),
                    quality.label()
                ),
                cfg.message_s,
            );
        }
        let _ = m.owner_gid;
        rec.emit(lua, records::pilot_of(f), &f.typ, f.side, &f.group_name, Some(quality.score()), RangeResult::Csar(res), None);
    }

    /// A helicopter pilot left their aircraft: their waiting survivor is
    /// lost, and one they carried dies with them.
    pub fn player_out(&mut self, lua: MizLua, rec: &mut Recorder, f: &Flying) {
        let ucid = f.ucid.to_string();
        while let Some(mi) = self.missions.iter().position(|m| {
            m.picked.as_ref().map(|(u, ..)| u == &f.unit_name).unwrap_or(false) || (m.picked.is_none() && m.owner == ucid)
        }) {
            let mut m = self.missions.remove(mi);
            let hot = m.hostile.is_some();
            Self::cleanup(lua, &mut m);
            let area = self.areas.get(m.area).map(|a| a.0.name.clone()).unwrap_or_default();
            let outcome = if m.picked.is_some() { CsarOutcome::PickedUp } else { CsarOutcome::Failed };
            let res = CsarResult {
                area,
                hostile: hot,
                outcome,
                time_to_pickup_s: m.picked.as_ref().map(|(_, t, _)| t - m.started),
                time_total_s: None,
                pickup_method: m.picked.as_ref().map(|(.., meth)| meth.to_string()).unwrap_or_default(),
                delivered_to: None,
                survivor_pos: util::geo(lua, m.pos),
                quality: grading::csar_quality(outcome, None),
            };
            rec.emit(lua, records::pilot_of(f), &f.typ, f.side, &f.group_name, Some(res.quality.score()), RangeResult::Csar(res), None);
        }
    }

    pub fn describe(&self, from: V3, side: Side, now: f64) -> Vec<String> {
        let mut out: Vec<String> = self
            .areas
            .iter()
            .filter(|(a, _)| for_side(&a.side, side))
            .map(|(a, p)| format!("{} {:03.0}/{:.0}nm, radius {:.0} km, beacon {:.0} kHz", a.name, util::bearing(from, *p), util::dist2(from, *p) / util::NM, a.radius_m / 1000., a.beacon_khz))
            .collect();
        for m in self.missions.iter().filter(|m| m.side == side) {
            out.push(format!(
                "  CSAR {}: {} {:03.0}/{:.0}nm (rough), {:.0} min{}",
                m.id,
                if m.picked.is_some() { "on board, heading home" } else { "waiting" },
                util::bearing(from, m.fuzz),
                util::dist2(from, m.fuzz) / util::NM,
                (now - m.started) / 60.,
                if m.hostile.is_some() { ", HOT" } else { "" }
            ));
        }
        if out.is_empty() {
            out.push("No CSAR areas for your side".into());
        }
        out
    }

    pub fn live(&self, lua: MizLua, players: &Players) -> Vec<LiveCsar> {
        self.missions
            .iter()
            .map(|m| LiveCsar {
                area: self.areas.get(m.area).map(|a| a.0.name.clone()).unwrap_or_default(),
                pilot: players.flying.values().find(|f| f.ucid.to_string() == m.owner).map(|f| f.name.clone()).unwrap_or_default(),
                area_pos: util::geo(lua, m.fuzz),
                radius_m: 2500.,
                started: m.started_utc,
                picked_up: m.picked.is_some(),
                hostile: m.hostile.is_some(),
            })
            .collect()
    }
}
