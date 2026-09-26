// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Rotary-wing drills: precision / confined / pinnacle landings on pads,
//! sling-load courses and troop insertions.
//!
//! DCS has no sling-load events, so a course's cargo is polled: it has been
//! picked up once it leaves the ground, delivered once it is back on the
//! ground and has stopped moving.
//!
//! DCS dynamic cargo is graded the same way, with no F10 step at all: at a
//! home field with dynamic cargo on, a pilot asks the ground crew for a load,
//! loads it into the cabin with the DCS cargo loader (or slings it), and
//! sets it down at any marked pad, drop zone or LZ. DCS names those packages
//! `<player>|HH:MM|PKG<n>`, which is how they are recognised and whose
//! delivery they are.

use crate::{
    ag,
    players::{Flying, Players},
    records::{self, Recorder},
    spawn::{self, Spawn, SpawnKind, Spawns},
    util::{self, V3},
};
use anyhow::{anyhow, bail, Result};
use bfprotocols::range::{
    cfg::{LandingPadCfg, RangeCfg, SlingCourseCfg, TroopLzCfg},
    grading, LandingResult, PilotRef, RangeResult, SlingResult, TroopResult,
};
use chrono::{Duration, Utc};
use dcso3::{
    coalition::Static,
    static_object::StaticObject,
    trigger::Trigger,
    MizLua,
};
use fxhash::FxHashMap;
use log::{info, warn};
use std::collections::VecDeque;

#[derive(Debug)]
struct Pad {
    cfg: LandingPadCfg,
    pos: V3,
}

#[derive(Debug)]
struct SlingRun {
    course: SlingCourseCfg,
    cargo: String,
    spawn_id: String,
    ucid: String,
    pilot: PilotRef,
    typ: String,
    side: dcso3::coalition::Side,
    group_id: dcso3::env::miz::GroupId,
    callsign: String,
    dz: V3,
    picked: Option<f64>,
    last_pos: Option<V3>,
    still_since: Option<f64>,
    life0: f64,
    started: f64,
}

#[derive(Debug)]
struct TroopRun {
    lz: TroopLzCfg,
    lz_pos: V3,
    loaded_at: f64,
    load_time: f64,
}

/// A DCS dynamic-cargo package from the ground crew's cargo loader.
#[derive(Debug)]
struct DynCargo {
    owner: String,
    cargo: String,
    mass_kg: f64,
    spawn_pos: V3,
    last_pos: V3,
    /// mission time it first left its spawn spot
    picked: Option<f64>,
    /// seen in the owner's cabin (getCargosOnBoard), not just slung
    inside: bool,
    still_since: Option<f64>,
    life0: f64,
    born: f64,
}

/// Vertical speed history for a helo near a pad, to read the touchdown rate.
#[derive(Debug, Default)]
struct Approach {
    vs: VecDeque<(f64, f64)>,
    hover_s: f64,
    last: f64,
    pad: usize,
}

#[derive(Debug, Default)]
pub struct Helo {
    pads: Vec<Pad>,
    sling: Vec<(SlingCourseCfg, V3, V3)>,
    troops: Vec<(TroopLzCfg, V3, V3)>,
    runs: Vec<SlingRun>,
    troop_runs: FxHashMap<String, TroopRun>,
    troop_started: FxHashMap<String, f64>,
    approaches: FxHashMap<String, Approach>,
    dyn_cargo: FxHashMap<String, DynCargo>,
    last_dyn: f64,
}

impl Helo {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg) {
        let dry = |lua: MizLua, loc| ag::resolve(lua, loc).map(|p| util::nearest_land(lua, p, 300.).unwrap_or(p));
        for p in &cfg.helo.pads {
            match dry(lua, &p.loc) {
                Ok(pos) => {
                    if let Some(m) = &p.marker {
                        let s = spawn::static_object(&format!("RNG-PAD-{}", p.id), m, pos, p.heading_deg.unwrap_or(0.), None);
                        if let Ok(c) = spawn::country_id("CJTF_BLUE") {
                            if let Err(e) = spawn::add_static(lua, c, &s) {
                                warn!("pad {} marker: {e:?}", p.id)
                            }
                        }
                    }
                    draw_pad(lua, pos, &p.name, p.perfect_m);
                    self.pads.push(Pad { cfg: p.clone(), pos })
                }
                Err(e) => warn!("pad {}: {e:?}", p.id),
            }
        }
        for s in &cfg.helo.sling {
            match (dry(lua, &s.pickup), dry(lua, &s.dropzone)) {
                (Ok(a), Ok(b)) => {
                    draw_pad(lua, b, &format!("{} DZ", s.name), s.perfect_m);
                    self.sling.push((s.clone(), a, b))
                }
                (a, b) => warn!("sling course {}: {:?} {:?}", s.id, a.err(), b.err()),
            }
        }
        for t in &cfg.helo.troops {
            match (dry(lua, &t.pickup), dry(lua, &t.lz)) {
                (Ok(a), Ok(b)) => {
                    draw_pad(lua, b, &format!("{} LZ", t.name), t.radius_m);
                    self.troops.push((t.clone(), a, b))
                }
                (a, b) => warn!("troop LZ {}: {:?} {:?}", t.id, a.err(), b.err()),
            }
        }
        info!("helo: {} pads, {} sling courses, {} troop LZs", self.pads.len(), self.sling.len(), self.troops.len());
    }

    pub fn sling_courses(&self) -> Vec<(String, String)> {
        self.sling.iter().map(|(c, ..)| (c.id.clone(), c.name.clone())).collect()
    }

    pub fn troop_lzs(&self) -> Vec<(String, String)> {
        self.troops.iter().map(|(c, ..)| (c.id.clone(), c.name.clone())).collect()
    }

    pub fn pad_list(&self) -> Vec<String> {
        self.pads
            .iter()
            .map(|p| format!("{} ({}, perfect inside {:.0} m)", p.cfg.name, p.cfg.drill, p.cfg.perfect_m))
            .collect()
    }

    pub fn active(&self) -> bool {
        !self.approaches.is_empty() || !self.runs.is_empty() || !self.dyn_cargo.is_empty()
    }

    /// Start a sling-load course: spawn the cargo at the pickup.
    pub fn start_sling(&mut self, lua: MizLua, cfg: &RangeCfg, spawns: &mut Spawns, f: &Flying, course: &str, now: f64) -> Result<String> {
        if !f.is_helo {
            bail!("sling loads are for helicopters")
        }
        let (c, pick, dz) = self
            .sling
            .iter()
            .find(|(c, ..)| c.id == course)
            .cloned()
            .ok_or_else(|| anyhow!("no sling course {course}"))?;
        self.runs.retain(|r| {
            if r.ucid == f.ucid.to_string() {
                spawn::destroy_static(lua, &r.cargo);
                false
            } else {
                true
            }
        });
        let name = spawns.next_name("SLING");
        let mut s = spawn::static_object(&name, &c.cargo_type, pick, 0., Some(c.mass_kg));
        if let Some(shape) = cargo_shape(&c.cargo_type) {
            s["shape_name"] = serde_json::json!(shape);
        }
        let country = spawn::country_id(spawn::default_country(f.side))?;
        spawn::add_static(lua, country, &s)?;
        let life0 = cargo(lua, &name).and_then(|s| s.get_life().ok()).map(|l| l as f64).unwrap_or(1.);
        spawns.insert(Spawn {
            id: name.clone(),
            item: "sling_course".into(),
            label: format!("Sling: {} ({:.0} kg)", c.name, c.mass_kg),
            owner: Some(f.ucid.to_string()),
            owner_name: f.name.clone(),
            groups: vec![],
            statics: vec![name.clone()],
            created: Utc::now(),
            expires: Some(Utc::now() + Duration::seconds(cfg.spawn.despawn_after_s as i64)),
            units: 0,
            pos: pick,
            kind: SpawnKind::Cargo,
        });
        records::to_group(
            lua,
            f.group_id,
            &format!(
                "SLING {}: {:.0} kg {} at {:03.0}/{:.1} nm. Deliver to the DZ {:03.0}/{:.1} nm from it; perfect inside {:.0} m.",
                c.name,
                c.mass_kg,
                c.cargo_type,
                util::bearing(f.pos, pick),
                util::dist2(f.pos, pick) / util::NM,
                util::bearing(pick, dz),
                util::dist2(pick, dz) / util::NM,
                c.perfect_m
            ),
            20,
        );
        self.runs.push(SlingRun {
            course: c,
            cargo: name.clone(),
            spawn_id: name.clone(),
            ucid: f.ucid.to_string(),
            pilot: records::pilot_of(f),
            typ: f.typ.clone(),
            side: f.side,
            group_id: f.group_id,
            callsign: f.group_name.clone(),
            dz,
            picked: None,
            last_pos: None,
            still_since: None,
            life0,
            started: now,
        });
        Ok(name)
    }

    pub fn load_troops(&mut self, lua: MizLua, f: &Flying, lz: &str, now: f64) -> Result<String> {
        let (c, pick, lzp) = self
            .troops
            .iter()
            .find(|(c, ..)| c.id == lz)
            .cloned()
            .ok_or_else(|| anyhow!("no troop LZ {lz}"))?;
        if f.in_air {
            bail!("land at the pickup to load")
        }
        if util::dist2(f.pos, pick) > c.radius_m.max(100.) {
            bail!("you are {:.0} m from the pickup; land within {:.0} m", util::dist2(f.pos, pick), c.radius_m.max(100.))
        }
        let kg = c.troops as f64 * c.kg_per_troop;
        Trigger::singleton(lua)?.action()?.set_unit_internal_cargo(f.unit_name.as_str().into(), kg as i64)?;
        let started = self.troop_started.remove(&f.unit_name).unwrap_or(now);
        self.troop_runs.insert(
            f.unit_name.clone(),
            TroopRun { lz: c.clone(), lz_pos: lzp, loaded_at: now, load_time: now - started },
        );
        Ok(format!(
            "{} troops aboard ({:.0} kg). LZ {} {:03.0}/{:.1} nm.",
            c.troops,
            kg,
            c.name,
            util::bearing(f.pos, lzp),
            util::dist2(f.pos, lzp) / util::NM
        ))
    }

    pub fn unload_troops(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, f: &Flying, now: f64) -> Result<String> {
        let run = self.troop_runs.get(&f.unit_name).ok_or_else(|| anyhow!("no troops aboard"))?;
        if f.in_air {
            bail!("land in the LZ to unload")
        }
        let d = util::dist2(f.pos, run.lz_pos);
        if d > run.lz.radius_m * 3. {
            bail!("you are {:.0} m from the LZ centre", d)
        }
        let run = self.troop_runs.remove(&f.unit_name).unwrap();
        Trigger::singleton(lua)?.action()?.set_unit_internal_cargo(f.unit_name.as_str().into(), 0)?;
        let q = grading::precision_quality(run.lz.radius_m / 4., d);
        let total = now - run.loaded_at + run.load_time;
        let res = TroopResult {
            lz: run.lz.name.clone(),
            troops: run.lz.troops,
            load_time_s: run.load_time,
            total_time_s: total,
            landing_distance_m: d,
            quality: q,
        };
        let msg = format!(
            "TROOPS {}: {} delivered, {:.0} m from the LZ centre ({}), {:.0} s from pickup",
            run.lz.name,
            run.lz.troops,
            d,
            q.label(),
            now - run.loaded_at
        );
        rec.emit(lua, records::pilot_of(f), &f.typ, f.side, &f.group_name, Some(q.score()), RangeResult::Troops(res), None);
        let _ = cfg;
        Ok(msg)
    }

    /// Poll cargo and sample helos near pads.
    pub fn tick(&mut self, lua: MizLua, cfg: &RangeCfg, players: &Players, spawns: &mut Spawns, rec: &mut Recorder, now: f64) {
        self.dyn_cargo_tick(lua, cfg, players, rec, now);
        // approaches to pads (5 Hz)
        for f in players.flying.values().filter(|f| f.is_helo) {
            let near = self
                .pads
                .iter()
                .enumerate()
                .map(|(i, p)| (i, util::dist2(p.pos, f.pos)))
                .filter(|(_, d)| *d < 300.)
                .min_by(|a, b| a.1.total_cmp(&b.1));
            match near {
                Some((i, d)) if f.in_air => {
                    let a = self.approaches.entry(f.unit_name.clone()).or_default();
                    a.pad = i;
                    if now - a.last >= 0.2 {
                        let dt = if a.last == 0. { 0. } else { now - a.last };
                        a.last = now;
                        a.vs.push_back((now, f.vel.y));
                        while a.vs.len() > 15 {
                            a.vs.pop_front();
                        }
                        if d < 10. && f.alt_agl < 5. {
                            a.hover_s += dt;
                        }
                    }
                }
                Some(_) => (),
                None => {
                    self.approaches.remove(&f.unit_name);
                }
            }
        }
        // sling cargo (1 Hz)
        let mut done = vec![];
        for (i, r) in self.runs.iter_mut().enumerate() {
            let Some(s) = cargo(lua, &r.cargo) else {
                done.push((i, None));
                continue;
            };
            let Ok(p) = s.as_object().and_then(|o| o.get_point()).map(|p| p.0) else { continue };
            let agl = p.y - util::ground_height(lua, p);
            let moved = r.last_pos.map(|l| util::dist3(l, p)).unwrap_or(0.);
            r.last_pos = Some(p);
            if r.picked.is_none() && agl > 2. {
                r.picked = Some(now);
                records::to_group(lua, r.group_id, &format!("SLING {}: cargo off the ground", r.course.name), 5);
            }
            if r.picked.is_some() && agl < 1.5 && moved < 0.3 {
                let since = *r.still_since.get_or_insert(now);
                if now - since > 4. {
                    done.push((i, Some(p)));
                }
            } else {
                r.still_since = None;
            }
            if now - r.started > 3600. {
                done.push((i, None));
            }
        }
        done.sort_by(|a, b| b.0.cmp(&a.0));
        for (i, p) in done {
            let r = self.runs.remove(i);
            if let Some(p) = p {
                let d = util::dist2(p, r.dz);
                let life = cargo(lua, &r.cargo).and_then(|s| s.get_life().ok()).map(|l| l as f64).unwrap_or(0.);
                let damage = if r.life0 > 0. { (1. - life / r.life0).clamp(0., 1.) } else { 0. };
                let q = grading::precision_quality(r.course.perfect_m, d);
                let t = now - r.picked.unwrap_or(r.started);
                let res = SlingResult {
                    method: "sling".into(),
                    course: r.course.name.clone(),
                    cargo: r.course.cargo_type.clone(),
                    mass_kg: r.course.mass_kg,
                    time_s: t,
                    distance_m: d,
                    damage,
                    quality: q,
                    dz_pos: util::geo(lua, r.dz),
                    set_down_pos: util::geo(lua, p),
                };
                records::to_group(
                    lua,
                    r.group_id,
                    &format!("SLING {}: set down {:.1} m from the DZ centre in {:.0} s - {}{}", r.course.name, d, t, q.label(),
                        if damage > 0.05 { format!(", cargo {:.0}% damaged", damage * 100.) } else { String::new() }),
                    cfg.message_s,
                );
                rec.emit(lua, r.pilot.clone(), &r.typ, r.side, &r.callsign, Some(q.score()), RangeResult::Sling(res), None);
            }
            spawns.remove(lua, &r.spawn_id);
        }
    }

    /// A helo landed (LAND event): grade it if it was on a pad.
    pub fn landed(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, f: &Flying, now: f64) {
        let Some(a) = self.approaches.remove(&f.unit_name) else { return };
        let Some(pad) = self.pads.get(a.pad) else { return };
        let d = util::dist2(pad.pos, f.pos);
        if d > 60. {
            return;
        }
        // the steepest descent in the last second before the touchdown
        let fpm = a
            .vs
            .iter()
            .filter(|(t, _)| now - t < 1.2)
            .map(|(_, v)| -v * 60. * util::M_TO_FT)
            .fold(0., f64::max);
        let herr = pad.cfg.heading_deg.map(|h| util::angdiff(f.hdg, h).abs());
        let mut q = grading::precision_quality(pad.cfg.perfect_m, d);
        // a hard landing caps the grade
        if fpm > 500. && q > bfprotocols::range::PrecisionQuality::Fair {
            q = bfprotocols::range::PrecisionQuality::Fair;
        }
        let res = LandingResult {
            drill: pad.cfg.drill.clone(),
            pad: pad.cfg.name.clone(),
            distance_m: d,
            touchdown_fpm: fpm,
            heading_error_deg: herr,
            hover_s: a.hover_s,
            quality: q,
            pad_pos: util::geo(lua, pad.pos),
            touchdown_pos: util::geo(lua, f.pos),
        };
        records::to_group(
            lua,
            f.group_id,
            &format!(
                "LANDING {} ({}): {:.1} m from the mark, {:.0} fpm{} - {}",
                pad.cfg.name,
                pad.cfg.drill,
                d,
                fpm,
                herr.map(|h| format!(", heading off {h:.0} deg")).unwrap_or_default(),
                q.label()
            ),
            cfg.message_s,
        );
        rec.emit(lua, records::pilot_of(f), &f.typ, f.side, &f.group_name, Some(q.score()), RangeResult::Landing(res), None);
    }

    /// A static was born; if it is a dynamic-cargo package, start watching it.
    pub fn static_born(&mut self, lua: MizLua, name: &str, now: f64) {
        if !name.contains("|PKG") {
            return;
        }
        let Some(s) = cargo(lua, name) else { return };
        let Ok(p) = s.as_object().and_then(|o| o.get_point()).map(|p| p.0) else { return };
        let owner = name.split('|').next().unwrap_or_default().to_string();
        let mass_kg = s.get_cargo_weight().unwrap_or(0.);
        let cargo_name = s.get_cargo_display_name().map(|s| s.to_string()).unwrap_or_else(|_| "cargo".into());
        let life0 = s.get_life().map(|l| l as f64).unwrap_or(1.);
        info!("dynamic cargo {name}: {cargo_name}, {mass_kg:.0} kg, for {owner}");
        self.dyn_cargo.insert(
            name.to_string(),
            DynCargo {
                owner,
                cargo: cargo_name,
                mass_kg,
                spawn_pos: p,
                last_pos: p,
                picked: None,
                inside: false,
                still_since: None,
                life0,
                born: now,
            },
        );
    }

    /// Every marked place a load can be delivered to: pads, sling drop zones
    /// and troop LZs, with the radius that grades PERFECT.
    fn destinations(&self) -> Vec<(String, V3, f64)> {
        let mut v: Vec<(String, V3, f64)> = self.pads.iter().map(|p| (p.cfg.name.clone(), p.pos, p.cfg.perfect_m.max(5.))).collect();
        v.extend(self.sling.iter().map(|(c, _, dz)| (format!("{} DZ", c.name), *dz, c.perfect_m)));
        v.extend(self.troops.iter().map(|(c, _, lz)| (format!("{} LZ", c.name), *lz, (c.radius_m / 4.).max(5.))));
        v
    }

    /// Poll dynamic-cargo packages (1 Hz) and grade the ones set down at a
    /// destination.
    fn dyn_cargo_tick(&mut self, lua: MizLua, cfg: &RangeCfg, players: &Players, rec: &mut Recorder, now: f64) {
        if self.dyn_cargo.is_empty() || now - self.last_dyn < 1. {
            return;
        }
        self.last_dyn = now;
        let dests = self.destinations();
        let mut done = vec![];
        for (name, c) in self.dyn_cargo.iter_mut() {
            let Some(s) = cargo(lua, name) else {
                done.push(name.clone());
                continue;
            };
            let Ok(p) = s.as_object().and_then(|o| o.get_point()).map(|p| p.0) else { continue };
            let owner = players.flying.values().find(|f| f.name == c.owner);
            if let Some(f) = owner {
                if !c.inside {
                    if let Ok(u) = dcso3::unit::Unit::get_by_name(lua, &f.unit_name) {
                        if let Ok(on) = u.get_cargos_on_board() {
                            c.inside = on.iter().any(|n| n.as_str() == name.as_str());
                        }
                    }
                }
            }
            if c.picked.is_none() && util::dist2(p, c.spawn_pos) > 30. {
                c.picked = Some(now);
            }
            let moved = util::dist3(p, c.last_pos);
            c.last_pos = p;
            let agl = p.y - util::ground_height(lua, p);
            let aboard = owner
                .map(|f| f.in_air && util::dist3(f.pos, p) < 40.)
                .unwrap_or(false);
            if c.picked.is_some() && !aboard && agl < 2. && moved < 0.3 {
                let since = *c.still_since.get_or_insert(now);
                if now - since < 5. {
                    continue;
                }
                let best = dests
                    .iter()
                    .map(|(n, d, perfect)| (n, *d, *perfect, util::dist2(*d, p)))
                    .min_by(|a, b| a.3.total_cmp(&b.3));
                match (best, owner) {
                    (Some((dn, dpos, perfect, dist)), Some(f)) if dist <= 200. => {
                        let life = s.get_life().map(|l| l as f64).unwrap_or(c.life0);
                        let damage = if c.life0 > 0. { (1. - life / c.life0).clamp(0., 1.) } else { 0. };
                        let q = grading::precision_quality(perfect, dist);
                        let t = now - c.picked.unwrap_or(c.born);
                        let method = if c.inside { "internal" } else { "sling" };
                        let res = SlingResult {
                            method: method.into(),
                            course: dn.clone(),
                            cargo: c.cargo.clone(),
                            mass_kg: c.mass_kg,
                            time_s: t,
                            distance_m: dist,
                            damage,
                            quality: q,
                            dz_pos: util::geo(lua, dpos),
                            set_down_pos: util::geo(lua, p),
                        };
                        records::to_group(
                            lua,
                            f.group_id,
                            &format!(
                                "CARGO {} ({}, {:.0} kg, {}) delivered to {}: {:.1} m from the mark, {:.0} s - {}",
                                c.cargo,
                                method,
                                c.mass_kg,
                                if damage > 0.05 { format!("{:.0}% damaged", damage * 100.) } else { "intact".into() },
                                dn,
                                dist,
                                t,
                                q.label()
                            ),
                            cfg.message_s,
                        );
                        rec.emit(lua, records::pilot_of(f), &f.typ, f.side, &f.group_name, Some(q.score()), RangeResult::Sling(res), None);
                        done.push(name.clone());
                    }
                    // set down away from every marked place: not a delivery;
                    // it can still be picked up again
                    _ => {
                        c.picked = None;
                        c.spawn_pos = p;
                        c.still_since = None;
                    }
                }
            } else {
                c.still_since = None;
            }
        }
        for n in done {
            self.dyn_cargo.remove(&n);
        }
        // packages nobody moved for two hours are forgotten
        self.dyn_cargo.retain(|_, c| now - c.born < 7200.);
    }

    pub fn dyn_cargo_help(&self) -> String {
        let mut t = vec![
            "DYNAMIC CARGO: at a home field ask the ground crew (F8) for cargo, load it into the cabin with the cargo loader or sling it, and set it down at any of these to have the delivery graded:".to_string(),
        ];
        t.extend(self.destinations().into_iter().map(|(n, _, perfect)| format!("  {n} (perfect inside {perfect:.0} m)")));
        t.join("\n")
    }

    pub fn player_left(&mut self, unit: &str) {
        self.approaches.remove(unit);
        self.troop_runs.remove(unit);
    }

    pub fn mark_troop_start(&mut self, unit: &str, now: f64) {
        self.troop_started.insert(unit.to_string(), now);
    }
}

fn cargo<'lua>(lua: MizLua<'lua>, name: &str) -> Option<StaticObject<'lua>> {
    match StaticObject::get_by_name(lua, name) {
        Ok(Static::Static(s)) => Some(s),
        _ => None,
    }
}

/// DCS cargo statics need their 3D shape named explicitly when spawned by
/// script.
pub fn cargo_shape(typ: &str) -> Option<&'static str> {
    Some(match typ {
        "ammo_cargo" => "ammo_box_cargo",
        "uh1h_cargo" => "ab-212_cargo",
        "container_cargo" => "bw_container_cargo",
        "fueltank_cargo" => "fueltank_cargo",
        "barrels_cargo" => "barrels_cargo",
        "oiltank_cargo" => "oiltank_cargo",
        "iso_container" => "iso_container_cargo",
        "iso_container_small" => "iso_container_small_cargo",
        "f_bar_cargo" => "f_bar_cargo",
        "m117_cargo" => "m117_cargo",
        "tetrapod_cargo" => "tetrapod_cargo",
        "pipes_small_cargo" => "pipes_small_cargo",
        "pipes_big_cargo" => "pipes_big_cargo",
        _ => return None,
    })
}

fn draw_pad(lua: MizLua, p: V3, name: &str, radius: f64) {
    use dcso3::{trigger::{CircleSpec, LineType, SideFilter, TextSpec}, Color, LuaVec3};
    let r = (|| -> Result<()> {
        let act = Trigger::singleton(lua)?.action()?;
        let c = Color::new(0.3, 1., 0.5, 0.9);
        act.circle_to_all(
            SideFilter::All,
            dcso3::trigger::MarkId::new(),
            CircleSpec { center: LuaVec3(p), radius: radius.max(5.), color: c, fill_color: Color::new(0., 0., 0., 0.), line_type: LineType::Solid, read_only: true },
            None,
        )?;
        let mut l = p;
        l.x += radius.max(5.) + 30.;
        act.text_to_all(
            SideFilter::All,
            dcso3::trigger::MarkId::new(),
            TextSpec { pos: LuaVec3(l), color: c, fill_color: Color::new(0., 0., 0., 0.5), font_size: 11, read_only: true, text: name.into() },
        )?;
        Ok(())
    })();
    if let Err(e) = r {
        warn!("could not draw {name}: {e:?}")
    }
}
