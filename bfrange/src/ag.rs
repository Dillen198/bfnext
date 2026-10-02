// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Air-to-ground stations: bomb circles, strafe pits, tactical target arrays,
//! convoys, coordinate and laser targets, SAM/AAA sites.
//!
//! The engine spawns every station's targets from the config at mission
//! start, draws its scoring rings on the F10 map, respawns dead targets on a
//! timer, and scores whatever lands near them.

use crate::{
    records::{self, Recorder},
    spawn::{self, Spawns},
    util::{self, V3},
    weapons::{Impact, Purpose},
};
use anyhow::{anyhow, Result};
use bfprotocols::range::{
    cfg::{Loc, LocKind, ObjCategory, RangeCfg, StationCfg, StrafePitCfg},
    grading, BombResult, LiveStation, PilotRef, RangeResult, Release, StationKind, StrafeQuality,
    StrafeResult, Track, WeaponClass,
};
use dcso3::{
    coalition::Side,
    trigger::{CircleSpec, LineType, SideFilter, TextSpec, Trigger},
    Color, LuaEnv, LuaVec3, MizLua,
};
use fxhash::FxHashMap;
use log::{error, info, warn};
use mlua::{prelude::*, RegistryKey};
use serde_json::json;

#[derive(Debug)]
pub struct Target {
    pub name: String,
    pub typ: String,
    pub category: ObjCategory,
    pub pos: V3,
    pub alive: bool,
}

#[derive(Debug)]
pub struct Station {
    pub cfg: StationCfg,
    pub center: V3,
    pub targets: Vec<Target>,
    /// ground/ship group holding the vehicle targets, if any
    pub group: Option<String>,
    /// players who dropped/fired here in the last few minutes, ucid -> (name, time)
    pub hot: FxHashMap<String, (String, f64)>,
    pub respawn_at: Option<f64>,
    /// AI designator (group name) and its laser spot in the Lua registry
    pub designator: Option<String>,
    pub spot: Option<RegistryKey>,
}

impl Station {
    pub fn alive(&self) -> u32 {
        self.targets.iter().filter(|t| t.alive).count() as u32
    }

    /// Where a scored weapon is measured to: the nearest target of this
    /// station, alive or dead (a dead target is still the thing aimed at).
    pub fn nearest_target(&self, p: V3) -> Option<&Target> {
        self.targets
            .iter()
            .min_by(|a, b| util::dist2(a.pos, p).total_cmp(&util::dist2(b.pos, p)))
    }
}

/// A strafe pass in progress.
#[derive(Debug, Clone)]
pub struct StrafeRun {
    pub station: usize,
    pub ucid: String,
    pub pilot: PilotRef,
    pub unit_type: String,
    pub side: Side,
    pub callsign: String,
    pub group_id: Option<dcso3::env::miz::GroupId>,
    pub gun: String,
    pub ammo_start: u32,
    pub hits: u32,
    pub firing: bool,
    pub fired: bool,
    pub foul: bool,
    pub min_range: f64,
    pub run_in_hdg: f64,
    pub entry_agl: f64,
    pub started: f64,
}

/// Rockets fired as one ripple, scored as one pass (best impact + spread).
#[derive(Debug)]
struct Salvo {
    impacts: Vec<(f64, Impact)>,
    last: f64,
}

#[derive(Debug, Default)]
pub struct AirToGround {
    pub stations: Vec<Station>,
    pub strafe: FxHashMap<String, StrafeRun>,
    salvos: FxHashMap<(String, String), Salvo>,
}

pub fn resolve(lua: MizLua, loc: &Loc) -> Result<V3> {
    match loc.kind() {
        LocKind::Invalid => Err(anyhow!("location needs a zone, or a lat and a lon")),
        LocKind::LatLon(lat, lon) => {
            let mut p = util::from_latlon(lua, lat, lon)?;
            p.y = util::ground_height(lua, p);
            Ok(p)
        }
        LocKind::Zone(zone) => {
            let z = Trigger::singleton(lua)?
                .get_zone(zone.into())
                .map_err(|e| anyhow!("trigger zone {zone:?} is not in the mission: {e}"))?;
            let mut p = z.point.0;
            p.y = util::ground_height(lua, p);
            Ok(p)
        }
    }
}

fn ring_color(kind: StationKind) -> Color {
    match kind {
        StationKind::StrafePit => Color::new(1., 0.6, 0., 0.9),
        StationKind::SamSite => Color::new(1., 0.1, 0.1, 0.9),
        _ => Color::new(1., 1., 0.2, 0.9),
    }
}

impl AirToGround {
    /// Spawn every station's targets and draw the F10 marks.
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg, spawns: &mut Spawns, now: f64) {
        for sc in &cfg.stations {
            match resolve(lua, &sc.loc) {
                Ok(center) => {
                    let mut st = Station {
                        cfg: sc.clone(),
                        center,
                        targets: vec![],
                        group: None,
                        hot: FxHashMap::default(),
                        respawn_at: None,
                        designator: None,
                        spot: None,
                    };
                    if let Err(e) = spawn_station(lua, &mut st, spawns, now) {
                        error!("station {} ({}): {e:?}", sc.id, sc.name);
                    }
                    draw_station(lua, &st);
                    info!(
                        "station {} ({}) at {} with {} targets",
                        sc.id,
                        sc.name,
                        sc.loc.describe(),
                        st.targets.len()
                    );
                    self.stations.push(st);
                }
                Err(e) => error!("station {} ({}) not placed: {e:?}", sc.id, sc.name),
            }
        }
    }

    pub fn station_by_id(&self, id: &str) -> Option<usize> {
        self.stations.iter().position(|s| s.cfg.id == id)
    }

    /// A static/unit died: mark the target dead and schedule its respawn.
    pub fn object_dead(&mut self, name: &str, now: f64) -> Option<usize> {
        for (i, st) in self.stations.iter_mut().enumerate() {
            if let Some(t) = st.targets.iter_mut().find(|t| t.name == name) {
                t.alive = false;
                if st.respawn_at.is_none() {
                    if let Some(s) = st.cfg.respawn_s {
                        st.respawn_at = Some(now + s as f64);
                    }
                }
                return Some(i);
            }
        }
        None
    }

    pub fn is_station_target(&self, name: &str) -> Option<usize> {
        self.stations.iter().position(|st| st.targets.iter().any(|t| t.name == name))
    }

    /// Respawn stations whose timer is up, keep lasers on target, age out
    /// the hot list.
    pub fn slow_tick(&mut self, lua: MizLua, spawns: &mut Spawns, now: f64) {
        for st in self.stations.iter_mut() {
            if let Some(t) = st.respawn_at {
                if now >= t {
                    st.respawn_at = None;
                    if let Err(e) = respawn(lua, st, spawns, now) {
                        warn!("station {} respawn failed: {e:?}", st.cfg.id)
                    }
                }
            }
            st.hot.retain(|_, (_, t)| now - *t < 300.);
            if let Err(e) = update_laser(lua, st, spawns, now) {
                warn!("station {} laser: {e:?}", st.cfg.id)
            }
        }
    }

    pub fn reset(&mut self, lua: MizLua, idx: usize, spawns: &mut Spawns, now: f64) -> Result<()> {
        let st = self.stations.get_mut(idx).ok_or_else(|| anyhow!("no such station"))?;
        respawn(lua, st, spawns, now)
    }

    /// Score a finished air-to-ground weapon against the nearest station.
    /// Returns false when nothing was near enough to score (the caller may
    /// try the anti-ship scorer).
    pub fn score_impact(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        rec: &mut Recorder,
        imp: Impact,
        now: f64,
    ) -> Option<Impact> {
        if imp.w.purpose != Purpose::AirToGround || !imp.w.shooter.is_player() {
            return Some(imp);
        }
        let near = self
            .stations
            .iter()
            .enumerate()
            .filter(|(_, s)| !matches!(s.cfg.kind, StationKind::ShipTarget | StationKind::StrafePit))
            .filter_map(|(i, s)| s.nearest_target(imp.pos).map(|t| (i, util::dist2(t.pos, imp.pos))))
            .filter(|(i, d)| *d <= cfg.scoring.max_score_m.max(self.stations[*i].cfg.radius_m))
            .min_by(|a, b| a.1.total_cmp(&b.1));
        let Some((si, _)) = near else {
            if let Some(g) = imp.w.shooter.group_id {
                records::to_group(
                    lua,
                    g,
                    &format!("RANGE: {} impact not near any range target", imp.w.display),
                    10,
                );
            }
            return None;
        };
        if imp.w.class == WeaponClass::Rocket {
            let key = (imp.w.shooter.unit_name.clone(), imp.w.weapon.clone());
            let s = self.salvos.entry(key).or_insert_with(|| Salvo { impacts: vec![], last: now });
            s.last = now;
            s.impacts.push((now, imp));
            return None;
        }
        self.record_bomb(lua, cfg, rec, si, &imp, None);
        None
    }

    /// Flush rocket ripples once no rocket has landed for 1.5 s.
    pub fn flush_salvos(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, now: f64) {
        let done: Vec<(String, String)> = self
            .salvos
            .iter()
            .filter(|(_, s)| now - s.last > 1.5)
            .map(|(k, _)| k.clone())
            .collect();
        for k in done {
            let Some(s) = self.salvos.remove(&k) else { continue };
            let mut best: Option<(usize, f64, &Impact)> = None;
            for (_, imp) in &s.impacts {
                for (i, st) in self.stations.iter().enumerate() {
                    if let Some(t) = st.nearest_target(imp.pos) {
                        let d = util::dist2(t.pos, imp.pos);
                        if best.map(|b| d < b.1).unwrap_or(true) {
                            best = Some((i, d, imp));
                        }
                    }
                }
            }
            if let Some((si, _, imp)) = best {
                let n = s.impacts.len();
                let imp = imp.clone();
                self.record_bomb(lua, cfg, rec, si, &imp, Some(n));
            }
        }
    }

    fn record_bomb(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        rec: &mut Recorder,
        si: usize,
        imp: &Impact,
        salvo: Option<usize>,
    ) {
        let st = &mut self.stations[si];
        let Some(tgt) = st.nearest_target(imp.pos) else { return };
        let w = &imp.w;
        let tpos = tgt.pos;
        let d = imp.pos - tpos;
        let miss = (d.x * d.x + d.z * d.z).sqrt();
        let radial = util::hdg(d);
        let rel_v = w.rel_vel;
        let attack_hdg = util::hdg(rel_v);
        let rel = util::angdiff(radial, attack_hdg);
        let long = miss * rel.to_radians().cos();
        let cross = miss * rel.to_radians().sin();
        let quality = grading::bomb_quality(&cfg.scoring, w.class, miss);
        let (wind_from, wind_kts, wind_v) = util::wind_at(lua, w.rel_pos);
        let tas = (rel_v - wind_v).norm() * util::MS_TO_KTS;
        let (release_temp_c, _) = util::temp_pressure_at(lua, w.rel_pos);
        let agl = w.rel_pos.y - util::ground_height(lua, w.rel_pos);
        // the air the bomb fell through, as DCS has it, for the calibration
        let atmo = util::atmo_profile(lua, w.rel_pos.x, w.rel_pos.z, imp.pos.y, w.rel_pos.y, 250.);
        let good = grading::good_radius(&cfg.scoring, w.class);
        let release = Release {
            pos: util::geo(lua, w.rel_pos),
            alt_agl_m: agl,
            tas_kts: tas,
            gs_kts: util::gs_kts(rel_v),
            heading_deg: attack_hdg,
            dive_deg: -util::fpa(rel_v),
            slant_range_m: util::dist3(w.rel_pos, tpos),
            ground_range_m: util::dist2(w.rel_pos, tpos),
            wind_from_deg: wind_from,
            wind_kts,
            mach: util::mach(tas, release_temp_c),
            atmo,
        };
        let target_hit = w.hits.iter().any(|h| st.targets.iter().any(|t| &t.name == h));
        let mut display = w.display.clone();
        if let Some(n) = salvo {
            if n > 1 {
                display = format!("{} (best of {n})", w.display);
            }
        }
        let clock = grading::clock(rel);
        let result = BombResult {
            station_id: st.cfg.id.clone(),
            range: st.cfg.name.clone(),
            target: tgt.name.clone(),
            weapon: w.weapon.clone(),
            weapon_display: display.clone(),
            weapon_class: w.class,
            guidance: w.guidance.into(),
            release,
            target_pos: util::geo(lua, tpos),
            impact: util::geo(lua, imp.pos),
            impact_north_m: d.x,
            impact_east_m: d.z,
            miss_m: miss,
            radial_deg: radial,
            clock,
            long_m: long,
            cross_m: cross,
            time_of_flight_s: imp.tof,
            quality,
            target_hit,
            laser_code: st.cfg.laser_code.filter(|_| w.guidance == "laser"),
            rings_m: if st.cfg.rings_m.is_empty() {
                vec![good, good * 2.]
            } else {
                st.cfg.rings_m.clone()
            },
            good_radius_m: good,
        };
        let sh = &w.shooter;
        if let Some(u) = &sh.ucid {
            st.hot.insert(u.to_string(), (sh.name.clone(), imp.w.last_t));
        }
        let msg = format!(
            "RANGE {}: {} {:.0} m @ {} o'clock ({}) - {}\nreleased {:.0} ft AGL, {:.0} KTAS, {:.0} deg dive, {:.1} nm",
            st.cfg.name,
            display,
            miss,
            clock,
            if long >= 0. { format!("{:.0} m long", long.abs()) } else { format!("{:.0} m short", long.abs()) },
            quality.label(),
            agl * util::M_TO_FT,
            tas,
            -util::fpa(rel_v),
            util::dist2(w.rel_pos, tpos) / util::NM,
        );
        if cfg.in_game_results {
            if let Some(g) = sh.group_id {
                records::to_group(lua, g, &msg, cfg.message_s);
            }
        }
        let pilot = PilotRef { ucid: sh.ucid.map(|u| u.to_string()), name: sh.name.clone() };
        rec.emit(
            lua,
            pilot,
            &sh.typ,
            sh.side,
            &sh.callsign,
            Some(quality.score()),
            RangeResult::Bomb(result),
            Some(Track::Weapon { points: w.pts.clone() }),
        );
    }

    // ------------------------------------------------------------- strafe

    /// Is this player set up on a strafe pit right now? Returns the station.
    pub fn in_pit(&self, pos: V3, vel: V3, agl: f64) -> Option<(usize, StrafePitCfg, V3)> {
        for (i, st) in self.stations.iter().enumerate() {
            if st.cfg.kind != StationKind::StrafePit {
                continue;
            }
            let pit = st.cfg.strafe.clone().unwrap_or_default();
            let Some(t) = st.nearest_target(pos) else { continue };
            let d = util::dist2(pos, t.pos);
            if d > pit.box_length_m || agl > pit.max_alt_agl_m {
                continue;
            }
            // pointed at the target, and inside the lateral box
            let brg = util::bearing(pos, t.pos);
            let off = util::angdiff(brg, util::hdg(vel)).abs();
            let lateral = d * off.to_radians().sin();
            if off < 60. && lateral <= pit.box_width_m {
                return Some((i, pit, t.pos));
            }
        }
        None
    }

    pub fn start_strafe(&mut self, unit_name: &str, run: StrafeRun) {
        self.strafe.insert(unit_name.to_string(), run);
    }

    /// A gun hit on something. Counts for any open pass whose pit it belongs to.
    pub fn strafe_hit(&mut self, shooter_unit: &str, target: &str) {
        if let Some(run) = self.strafe.get_mut(shooter_unit) {
            let st = &self.stations[run.station];
            if st.targets.iter().any(|t| t.name == target) {
                run.hits += 1;
            }
        }
    }

    /// Finish a pass: `ammo_now` is the gun's remaining rounds.
    #[allow(clippy::too_many_arguments)]
    pub fn finish_strafe(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        rec: &mut Recorder,
        unit_name: &str,
        ammo_now: u32,
        target_pos: V3,
    ) {
        let Some(run) = self.strafe.remove(unit_name) else { return };
        if !run.fired {
            return;
        }
        let st = &mut self.stations[run.station];
        let pit = st.cfg.strafe.clone().unwrap_or_default();
        let rounds = run.ammo_start.saturating_sub(ammo_now).max(run.hits);
        let acc = if rounds > 0 { (run.hits as f64 / rounds as f64 * 100.).min(100.) } else { 0. };
        let valid = !run.foul && rounds > 0;
        let quality = grading::strafe_quality(&cfg.scoring, acc, valid);
        let invalid_reason = if run.foul {
            Some(format!("fired inside the {:.0} m foul line", pit.foul_line_m))
        } else if rounds == 0 {
            Some("no rounds counted".into())
        } else {
            None
        };
        st.hot.insert(run.ucid.clone(), (run.pilot.name.clone(), 0.));
        let msg = format!(
            "STRAFE {}: {} hits of {} rounds ({:.0}%) - {}{}",
            st.cfg.name,
            run.hits,
            rounds,
            acc,
            quality.label(),
            invalid_reason.as_ref().map(|r| format!(" ({r})")).unwrap_or_default()
        );
        if cfg.in_game_results {
            if let Some(g) = run.group_id {
                records::to_group(lua, g, &msg, cfg.message_s);
            }
        }
        let result = StrafeResult {
            station_id: st.cfg.id.clone(),
            range: st.cfg.name.clone(),
            pit: st.cfg.name.clone(),
            gun: run.gun.clone(),
            rounds_fired: rounds,
            hits: run.hits,
            accuracy_pct: acc,
            quality,
            foul_line_crossed: run.foul,
            invalid_reason,
            run_in_heading_deg: run.run_in_hdg,
            min_range_m: run.min_range,
            entry_alt_agl_m: run.entry_agl,
            target_pos: util::geo(lua, target_pos),
            foul_line_m: pit.foul_line_m,
        };
        let score = if quality == StrafeQuality::Invalid { None } else { quality.score() };
        rec.emit(
            lua,
            run.pilot.clone(),
            &run.unit_type,
            run.side,
            &run.callsign,
            score,
            RangeResult::Strafe(result),
            None,
        );
    }

    pub fn live(&self, lua: MizLua) -> Vec<LiveStation> {
        self.stations
            .iter()
            .map(|st| {
                let elev = util::ground_height(lua, st.center);
                LiveStation {
                    id: st.cfg.id.clone(),
                    name: st.cfg.name.clone(),
                    kind: st.cfg.kind,
                    pos: util::geo(lua, st.center),
                    hot_by: st.hot.values().map(|(n, _)| n.clone()).collect(),
                    targets_alive: st.alive(),
                    targets_total: st.targets.len() as u32,
                    laser_code: st.cfg.laser_code,
                    rings_m: st.cfg.rings_m.clone(),
                    note: st.cfg.note.clone(),
                    elev_m: Some(elev),
                    // what the release calculator plans with: DCS's own air
                    // over the target, surface to 10 km
                    atmo: util::atmo_profile(lua, st.center.x, st.center.z, elev + 10., 10000., 500.),
                }
            })
            .collect()
    }

    /// One-line description of each station for the F10 status page.
    pub fn describe(&self, lua: MizLua, from: Option<V3>) -> Vec<String> {
        self.stations
            .iter()
            .map(|st| {
                let g = util::geo(lua, st.center);
                let rel = from
                    .map(|p| {
                        format!(
                            " {:03.0}/{:.0}nm",
                            util::bearing(p, st.center),
                            util::dist2(p, st.center) / util::NM
                        )
                    })
                    .unwrap_or_default();
                let laser = st.cfg.laser_code.map(|c| format!(" laser {c}")).unwrap_or_default();
                let hot = if st.hot.is_empty() { "cold".to_string() } else { format!("HOT ({})", st.hot.len()) };
                format!(
                    "{} [{}] {}{rel}{laser} {}/{} targets, {hot}  {:.4} {:.4}",
                    st.cfg.name,
                    kind_name(st.cfg.kind),
                    elev(st.center.y),
                    st.alive(),
                    st.targets.len(),
                    g.lat,
                    g.lon
                )
            })
            .collect()
    }
}

fn elev(y: f64) -> String {
    format!("elev {:.0}ft", y * util::M_TO_FT)
}

pub fn kind_name(k: StationKind) -> &'static str {
    match k {
        StationKind::BombCircle => "bomb circle",
        StationKind::StrafePit => "strafe pit",
        StationKind::TacticalArray => "tactical",
        StationKind::Convoy => "convoy",
        StationKind::CoordTarget => "coordinate target",
        StationKind::LaserTarget => "laser target",
        StationKind::ShipTarget => "ship",
        StationKind::GunneryLane => "gunnery lane",
        StationKind::SamSite => "SAM/AAA",
    }
}

fn target_positions(st: &StationCfg, center: V3) -> Vec<(V3, f64)> {
    st.targets
        .iter()
        .map(|t| {
            let mut p = util::offset(center, st.heading_deg, t.x, t.y);
            p.y = center.y;
            (p, (st.heading_deg + t.heading_deg).rem_euclid(360.))
        })
        .collect()
}

fn spawn_station(lua: MizLua, st: &mut Station, spawns: &mut Spawns, now: f64) -> Result<()> {
    let cfg = st.cfg.clone();
    let country = spawn::country_id(&cfg.country)?;
    let side = spawn::side_of_str(&cfg.side);
    let ship_station = cfg.kind == StationKind::ShipTarget;
    let mut pos = target_positions(&cfg, st.center);
    // land targets must be on dry ground: move any that fell in water to the
    // nearest bank, and drop the ones with no land within 600 m
    if !ship_station {
        for (i, (p, _)) in pos.iter_mut().enumerate() {
            if cfg.targets[i].category == ObjCategory::Ship {
                continue;
            }
            match util::nearest_land(lua, *p, 600.) {
                Some(q) => {
                    if util::dist2(q, *p) > 1. {
                        warn!(
                            "station {}: target {} was in water, moved {:.0} m to dry ground",
                            cfg.id,
                            i + 1,
                            util::dist2(q, *p)
                        );
                    }
                    p.y = q.y;
                    p.x = q.x;
                    p.z = q.z;
                }
                None => warn!("station {}: target {} is in water with no land within 600 m", cfg.id, i + 1),
            }
        }
    }
    st.targets.clear();
    let mut vehicles: Vec<(String, V3, f64)> = vec![];
    for (i, (t, (p, h))) in cfg.targets.iter().zip(pos.iter()).enumerate() {
        match t.category {
            ObjCategory::Static => {
                let name = format!("RNG-{}-T{}", cfg.id, i + 1);
                let s = spawn::static_object(&name, &t.typ, *p, *h, None);
                if let Err(e) = spawn::add_static(lua, country, &s) {
                    warn!("station {}: static {} ({}) failed: {e:?}", cfg.id, name, t.typ);
                    continue;
                }
                st.targets.push(Target {
                    name,
                    typ: t.typ.clone(),
                    category: ObjCategory::Static,
                    pos: *p,
                    alive: true,
                });
            }
            ObjCategory::Vehicle | ObjCategory::Ship => vehicles.push((t.typ.clone(), *p, *h)),
        }
    }
    if !vehicles.is_empty() {
        let gname = format!("RNG-{}-G", cfg.id);
        let is_ship = cfg.kind == StationKind::ShipTarget
            || cfg.targets.iter().any(|t| t.category == ObjCategory::Ship);
        let route = station_route(lua, &cfg, st.center, is_ship)?;
        let g = spawn::surface_group(&gname, &vehicles, "Average", route);
        let cat = if is_ship { spawn::SHIP } else { spawn::GROUND };
        spawn::add_group(lua, country, cat, &g)?;
        for (i, (typ, p, _)) in vehicles.iter().enumerate() {
            st.targets.push(Target {
                name: format!("{gname}-{}", i + 1),
                typ: typ.clone(),
                category: if is_ship { ObjCategory::Ship } else { ObjCategory::Vehicle },
                pos: *p,
                alive: true,
            });
        }
        // Targets hold fire unless the station is a live SAM site; SAM sites
        // keep their radar on either way so RWRs light up.
        let (roe, alarm) = if cfg.kind == StationKind::SamSite && cfg.weapons_free {
            (2, 2)
        } else if cfg.kind == StationKind::SamSite {
            (4, 2)
        } else {
            (4, 1)
        };
        // ground ROE: 2 OPEN_FIRE, 4 WEAPON_HOLD; ALARM_STATE (9): 1 GREEN, 2 RED
        spawns.defer(now + 2., spawn::Pending::GroupOption { group: gname.clone(), id: 0, value: json!(roe) });
        spawns.defer(now + 2., spawn::Pending::GroupOption { group: gname.clone(), id: 9, value: json!(alarm) });
        let _ = side;
        st.group = Some(gname);
    }
    Ok(())
}

fn station_route(lua: MizLua, cfg: &StationCfg, center: V3, ship: bool) -> Result<Vec<serde_json::Value>> {
    let Some(r) = &cfg.route else {
        return Ok(vec![if ship {
            spawn::waypoint(center, 0., 0., vec![])
        } else {
            spawn::ground_waypoint(center, 0., false, vec![])
        }]);
    };
    let speed = r.speed_kts / util::MS_TO_KTS;
    let mut pts = vec![];
    let mut all = vec![center];
    for l in &r.points {
        all.push(resolve(lua, l)?);
    }
    let n = all.len();
    for (i, p) in all.iter().enumerate() {
        let mut tasks = vec![];
        if i == n - 1 {
            // loop back to the first point
            tasks.push(spawn::wrapped(
                1,
                json!({ "id": "SwitchWaypoint", "params": { "fromWaypointIndex": n, "goToWaypointIndex": 1 } }),
            ));
        }
        pts.push(if ship {
            spawn::waypoint(*p, 0., speed, tasks)
        } else {
            spawn::ground_waypoint(*p, speed, r.on_road, tasks)
        });
    }
    Ok(pts)
}

fn despawn_station(lua: MizLua, st: &mut Station) {
    for t in &st.targets {
        if t.category == ObjCategory::Static {
            spawn::destroy_static(lua, &t.name);
        }
    }
    if let Some(g) = st.group.take() {
        spawn::destroy_group(lua, &g);
    }
    st.targets.clear();
}

fn respawn(lua: MizLua, st: &mut Station, spawns: &mut Spawns, now: f64) -> Result<()> {
    despawn_station(lua, st);
    spawn_station(lua, st, spawns, now)?;
    info!("station {} respawned", st.cfg.id);
    Ok(())
}

fn draw_station(lua: MizLua, st: &Station) {
    let r = (|| -> Result<()> {
        let act = Trigger::singleton(lua)?.action()?;
        let c = ring_color(st.cfg.kind);
        let rings = if st.cfg.rings_m.is_empty() { vec![25., 50.] } else { st.cfg.rings_m.clone() };
        for (i, radius) in rings.iter().enumerate() {
            act.circle_to_all(
                SideFilter::All,
                dcso3::trigger::MarkId::new(),
                CircleSpec {
                    center: LuaVec3(st.center),
                    radius: *radius,
                    color: c,
                    fill_color: Color::new(0., 0., 0., 0.),
                    line_type: if i == 0 { LineType::Solid } else { LineType::Dashed },
                    read_only: true,
                },
                None,
            )?;
        }
        let mut label = st.center;
        label.x += rings.iter().cloned().fold(50., f64::max) + 60.;
        let laser = st.cfg.laser_code.map(|c| format!(" / LASER {c}")).unwrap_or_default();
        act.text_to_all(
            SideFilter::All,
            dcso3::trigger::MarkId::new(),
            TextSpec {
                pos: LuaVec3(label),
                color: c,
                fill_color: Color::new(0., 0., 0., 0.55),
                font_size: 12,
                read_only: true,
                text: format!("{} ({}){laser}", st.cfg.name, kind_name(st.cfg.kind)).as_str().into(),
            },
        )?;
        Ok(())
    })();
    if let Err(e) = r {
        warn!("could not draw station {}: {e:?}", st.cfg.id)
    }
}

/// Keep an AI designator (an invisible, immortal MQ-9 orbiting overhead)
/// lasing the first live target of a station with a laser code.
fn update_laser(lua: MizLua, st: &mut Station, spawns: &mut Spawns, now: f64) -> Result<()> {
    let Some(code) = st.cfg.laser_code else { return Ok(()) };
    let target = st.targets.iter().find(|t| t.alive).map(|t| t.pos);
    if st.designator.is_none() {
        let name = format!("RNG-{}-LASER", st.cfg.id);
        let alt = st.center.y + 4600.;
        let orbit = json!({
            "id": "Orbit",
            "params": { "pattern": "Circle", "point": { "x": st.center.x, "y": st.center.z }, "speed": 60, "altitude": alt }
        });
        let spec = spawn::AirSpec {
            name: name.clone(),
            typ: "MQ-9 Reaper".into(),
            count: 1,
            skill: "Excellent".into(),
            pos: util::offset(st.center, 0., -3000., 0.),
            alt_m: alt,
            speed_ms: 60.,
            hdg: 90.,
            pylons: Default::default(),
            livery: None,
            callsign: None,
            freq_mhz: None,
            task: "Reconnaissance".into(),
            route: vec![spawn::waypoint(st.center, alt, 60., vec![spawn::task_entry(1, orbit)])],
            fuel_kg: None,
            side: spawn::side_of_str(if st.cfg.side == "red" { "blue" } else { "red" }),
        };
        let country = spawn::country_id(spawn::default_country(spec.side))?;
        spawn::add_group(lua, country, spawn::AIRPLANE, &spawn::air_group(&spec))?;
        for c in [spawn::bool_cmd("SetInvisible", true), spawn::bool_cmd("SetImmortal", true), spawn::bool_cmd("SetUnlimitedFuel", true)] {
            spawns.defer(now + 2., spawn::Pending::GroupCommand { group: name.clone(), cmd: c });
        }
        st.designator = Some(name);
        return Ok(());
    }
    let Some(tp) = target else {
        if let Some(k) = st.spot.take() {
            if let Ok(t) = lua.inner().registry_value::<LuaTable>(&k) {
                let _ = t.call_method::<_, ()>("destroy", ());
            }
            let _ = lua.inner().remove_registry_value(k);
        }
        return Ok(());
    };
    let aim = V3::new(tp.x, tp.y + 1.5, tp.z);
    match &st.spot {
        Some(k) => {
            let t: LuaTable = lua.inner().registry_value(k)?;
            t.call_method::<_, ()>("setPoint", LuaVec3(aim))?;
        }
        None => {
            let d = st.designator.clone().unwrap_or_default();
            let unit = dcso3::unit::Unit::get_by_name(lua, &format!("{d}-1"));
            if let Ok(u) = unit {
                let spot = dcso3::spot::Spot::create_laser(
                    lua,
                    u.as_object()?,
                    Some(LuaVec3(V3::new(0., -1., 0.))),
                    LuaVec3(aim),
                    code,
                )?;
                let t: LuaTable = (*spot).clone();
                st.spot = Some(lua.inner().create_registry_value(t)?);
                info!("station {} lasing on code {code}", st.cfg.id);
            }
        }
    }
    Ok(())
}
