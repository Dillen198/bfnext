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
use anyhow::{Result, anyhow, bail};
use bfprotocols::range::{
    AirdropRelease, AirdropResult, LandingResult, PilotRef, RangeResult, SlingResult,
    TroopResult,
    cfg::{DropZoneCfg, LandingPadCfg, RangeCfg, SlingCourseCfg, TroopLzCfg},
    grading,
};
use chrono::{Duration, Utc};
use dcso3::{
    MizLua, airbase::AirbaseCategory, coalition::Static, static_object::StaticObject,
    trigger::Trigger, world::World,
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
    /// carried by a fixed-wing aircraft: an airdrop or an air-land delivery,
    /// never a helicopter set-down
    fixed_wing: bool,
    /// when, how and where it left the cargo bay in flight
    release: Option<(f64, AirdropRelease, V3)>,
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
    /// airdrop drop zones, resolved
    drop_zones: Vec<(DropZoneCfg, V3)>,
    /// every airfield, for naming an air-land delivery
    fields: Vec<(String, V3)>,
}

impl Helo {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg) {
        let dry = |lua: MizLua, loc| {
            ag::resolve(lua, loc).map(|p| util::nearest_land(lua, p, 300.).unwrap_or(p))
        };
        for p in &cfg.helo.pads {
            match dry(lua, &p.loc) {
                Ok(pos) => {
                    if let Some(m) = &p.marker {
                        let s = spawn::static_object(
                            &format!("RNG-PAD-{}", p.id),
                            m,
                            pos,
                            p.heading_deg.unwrap_or(0.),
                            None,
                        );
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
        for z in &cfg.airdrop.zones {
            match ag::resolve(lua, &z.loc) {
                Ok(pos) => {
                    draw_pad(lua, pos, &format!("{} DZ", z.name), z.perfect_m);
                    self.drop_zones.push((z.clone(), pos))
                }
                Err(e) => warn!("drop zone {}: {e:?}", z.id),
            }
        }
        let fields = (|| -> anyhow::Result<Vec<(String, V3)>> {
            let mut v = vec![];
            for ab in World::singleton(lua)?.get_airbases()? {
                let ab = ab?;
                if ab.get_category().ok() != Some(AirbaseCategory::Airdrome) {
                    continue;
                }
                v.push((
                    ab.as_object()?.get_name()?.to_string(),
                    ab.as_object()?.get_point()?.0,
                ));
            }
            Ok(v)
        })();
        self.fields = fields.unwrap_or_default();
        info!(
            "helo: {} pads, {} sling courses, {} troop LZs; airdrop: {} drop zones",
            self.pads.len(),
            self.sling.len(),
            self.troops.len(),
            self.drop_zones.len()
        );
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
            .map(|p| {
                format!(
                    "{} ({}, perfect inside {:.0} m)",
                    p.cfg.name, p.cfg.drill, p.cfg.perfect_m
                )
            })
            .collect()
    }

    pub fn active(&self) -> bool {
        !self.approaches.is_empty() || !self.runs.is_empty() || !self.dyn_cargo.is_empty()
    }

    /// Start a sling-load course: spawn the cargo at the pickup.
    pub fn start_sling(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        spawns: &mut Spawns,
        f: &Flying,
        course: &str,
        now: f64,
    ) -> Result<String> {
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
        let life0 = cargo(lua, &name)
            .and_then(|s| s.get_life().ok())
            .map(|l| l as f64)
            .unwrap_or(1.);
        spawns.insert(Spawn {
            id: name.clone(),
            item: "sling_course".into(),
            label: format!("Sling: {} ({:.0} kg)", c.name, c.mass_kg),
            owner: Some(f.ucid.to_string()),
            owner_name: f.name.clone(),
            groups: vec![],
            statics: vec![name.clone()],
            created: Utc::now(),
            expires: Some(
                Utc::now() + Duration::seconds(cfg.spawn.despawn_after_s as i64),
            ),
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

    pub fn load_troops(
        &mut self,
        lua: MizLua,
        f: &Flying,
        lz: &str,
        now: f64,
    ) -> Result<String> {
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
            bail!(
                "you are {:.0} m from the pickup; land within {:.0} m",
                util::dist2(f.pos, pick),
                c.radius_m.max(100.)
            )
        }
        let kg = c.troops as f64 * c.kg_per_troop;
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(f.unit_name.as_str().into(), kg as i64)?;
        let started = self.troop_started.remove(&f.unit_name).unwrap_or(now);
        self.troop_runs.insert(
            f.unit_name.clone(),
            TroopRun {
                lz: c.clone(),
                lz_pos: lzp,
                loaded_at: now,
                load_time: now - started,
            },
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

    pub fn unload_troops(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        rec: &mut Recorder,
        f: &Flying,
        now: f64,
    ) -> Result<String> {
        let run = self
            .troop_runs
            .get(&f.unit_name)
            .ok_or_else(|| anyhow!("no troops aboard"))?;
        if f.in_air {
            bail!("land in the LZ to unload")
        }
        let d = util::dist2(f.pos, run.lz_pos);
        if d > run.lz.radius_m * 3. {
            bail!("you are {:.0} m from the LZ centre", d)
        }
        let run = self.troop_runs.remove(&f.unit_name).unwrap();
        Trigger::singleton(lua)?
            .action()?
            .set_unit_internal_cargo(f.unit_name.as_str().into(), 0)?;
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
        rec.emit(
            lua,
            records::pilot_of(f),
            &f.typ,
            f.side,
            &f.group_name,
            Some(q.score()),
            RangeResult::Troops(res),
            None,
        );
        let _ = cfg;
        Ok(msg)
    }

    /// Poll cargo and sample helos near pads.
    pub fn tick(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        players: &Players,
        spawns: &mut Spawns,
        rec: &mut Recorder,
        now: f64,
    ) {
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
            let Ok(p) = s.as_object().and_then(|o| o.get_point()).map(|p| p.0) else {
                continue;
            };
            let agl = p.y - util::ground_height(lua, p);
            let moved = r.last_pos.map(|l| util::dist3(l, p)).unwrap_or(0.);
            r.last_pos = Some(p);
            if r.picked.is_none() && agl > 2. {
                r.picked = Some(now);
                records::to_group(
                    lua,
                    r.group_id,
                    &format!("SLING {}: cargo off the ground", r.course.name),
                    5,
                );
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
                let life = cargo(lua, &r.cargo)
                    .and_then(|s| s.get_life().ok())
                    .map(|l| l as f64)
                    .unwrap_or(0.);
                let damage =
                    if r.life0 > 0. { (1. - life / r.life0).clamp(0., 1.) } else { 0. };
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
                    &format!(
                        "SLING {}: set down {:.1} m from the DZ centre in {:.0} s - {}{}",
                        r.course.name,
                        d,
                        t,
                        q.label(),
                        if damage > 0.05 {
                            format!(", cargo {:.0}% damaged", damage * 100.)
                        } else {
                            String::new()
                        }
                    ),
                    cfg.message_s,
                );
                rec.emit(
                    lua,
                    r.pilot.clone(),
                    &r.typ,
                    r.side,
                    &r.callsign,
                    Some(q.score()),
                    RangeResult::Sling(res),
                    None,
                );
            }
            spawns.remove(lua, &r.spawn_id);
        }
    }

    /// A helo landed (LAND event): grade it if it was on a pad.
    pub fn landed(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        rec: &mut Recorder,
        f: &Flying,
        now: f64,
    ) {
        let Some(a) = self.approaches.remove(&f.unit_name) else { return };
        let Some(pad) = self.pads.get(a.pad) else { return };
        let d = util::dist2(pad.pos, f.pos);
        if d > PAD_REACH_M {
            return;
        }
        let fpm = touchdown_fpm(&a.vs, now);
        let herr = pad.cfg.heading_deg.map(|h| util::angdiff(f.hdg, h).abs());
        let q = pad_landing_quality(pad.cfg.perfect_m, d, fpm);
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
            ship_speed_kts: None,
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
        rec.emit(
            lua,
            records::pilot_of(f),
            &f.typ,
            f.side,
            &f.group_name,
            Some(q.score()),
            RangeResult::Landing(res),
            None,
        );
    }

    /// A static was born; if it is a dynamic-cargo package, start watching it.
    pub fn static_born(&mut self, lua: MizLua, name: &str, now: f64) {
        if !name.contains("|PKG") {
            return;
        }
        let Some(s) = cargo(lua, name) else { return };
        let Ok(p) = s.as_object().and_then(|o| o.get_point()).map(|p| p.0) else {
            return;
        };
        let owner = name.split('|').next().unwrap_or_default().to_string();
        let mass_kg = s.get_cargo_weight().unwrap_or(0.);
        let cargo_name = s
            .get_cargo_display_name()
            .map(|s| s.to_string())
            .unwrap_or_else(|_| "cargo".into());
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
                fixed_wing: false,
                release: None,
                still_since: None,
                life0,
                born: now,
            },
        );
    }

    /// Every marked place a load can be delivered to: pads, sling drop zones
    /// and troop LZs, with the radius that grades PERFECT.
    fn destinations(&self) -> Vec<(String, V3, f64)> {
        let mut v: Vec<(String, V3, f64)> = self
            .pads
            .iter()
            .map(|p| (p.cfg.name.clone(), p.pos, p.cfg.perfect_m.max(5.)))
            .collect();
        v.extend(
            self.sling
                .iter()
                .map(|(c, _, dz)| (format!("{} DZ", c.name), *dz, c.perfect_m)),
        );
        v.extend(self.troops.iter().map(|(c, _, lz)| {
            (format!("{} LZ", c.name), *lz, (c.radius_m / 4.).max(5.))
        }));
        v
    }

    /// Poll dynamic-cargo packages (1 Hz) and grade the ones set down at a
    /// destination.
    fn dyn_cargo_tick(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        players: &Players,
        rec: &mut Recorder,
        now: f64,
    ) {
        if self.dyn_cargo.is_empty() || now - self.last_dyn < 1. {
            return;
        }
        self.last_dyn = now;
        let dests = self.destinations();
        let zones = self.drop_zones.clone();
        let fields = self.fields.clone();
        let mut done = vec![];
        for (name, c) in self.dyn_cargo.iter_mut() {
            let Some(s) = cargo(lua, name) else {
                done.push(name.clone());
                continue;
            };
            let Ok(p) = s.as_object().and_then(|o| o.get_point()).map(|p| p.0) else {
                continue;
            };
            let owner = players.flying.values().find(|f| f.name == c.owner);
            // in the cargo bay right now (None: DCS cannot say)
            let mut on_now = None;
            if let Some(f) = owner {
                if !f.is_helo && !f.is_ground {
                    c.fixed_wing = true;
                }
                if let Ok(u) = dcso3::unit::Unit::get_by_name(lua, &f.unit_name) {
                    if let Ok(on) = u.get_cargos_on_board() {
                        let v = on.iter().any(|n| n.as_str() == name.as_str());
                        on_now = Some(v);
                        c.inside |= v;
                    }
                }
                // out of the bay in flight: an airdrop, released here
                let gone = match on_now {
                    Some(v) => c.inside && !v,
                    None => c.picked.is_some() && util::dist3(f.pos, p) > 60.,
                };
                if c.fixed_wing && c.release.is_none() && f.in_air && gone {
                    let rel = AirdropRelease {
                        pos: util::geo(lua, f.pos),
                        alt_agl_ft: f.alt_agl * util::M_TO_FT,
                        alt_msl_ft: f.pos.y * util::M_TO_FT,
                        gs_kts: f.vel.x.hypot(f.vel.z) * util::MS_TO_KTS,
                        heading_deg: f.hdg.rem_euclid(360.),
                        to_dz_m: None,
                    };
                    info!(
                        "airdrop: {name} left {}'s cargo bay at {:.0} ft AGL, {:.0} kt",
                        f.name, rel.alt_agl_ft, rel.gs_kts
                    );
                    c.release = Some((now, rel, f.pos));
                }
            }
            if c.picked.is_none() && util::dist2(p, c.spawn_pos) > 30. {
                c.picked = Some(now);
            }
            let moved = util::dist3(p, c.last_pos);
            c.last_pos = p;
            let agl = p.y - util::ground_height(lua, p);
            let aboard =
                owner.map(|f| f.in_air && util::dist3(f.pos, p) < 40.).unwrap_or(false);
            if c.picked.is_some() && !aboard && agl < 2. && moved < 0.3 {
                let since = *c.still_since.get_or_insert(now);
                if now - since < 5. {
                    continue;
                }
                if c.fixed_wing {
                    if fixed_wing_delivery(
                        lua, cfg, rec, c, owner, &s, p, &zones, &fields, now,
                    ) {
                        done.push(name.clone());
                    }
                    continue;
                }
                let best = dests
                    .iter()
                    .map(|(n, d, perfect)| (n, *d, *perfect, util::dist2(*d, p)))
                    .min_by(|a, b| a.3.total_cmp(&b.3));
                match (best, owner) {
                    (Some((dn, dpos, perfect, dist)), Some(f)) if dist <= 200. => {
                        let life = s.get_life().map(|l| l as f64).unwrap_or(c.life0);
                        let damage = if c.life0 > 0. {
                            (1. - life / c.life0).clamp(0., 1.)
                        } else {
                            0.
                        };
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
                                if damage > 0.05 {
                                    format!("{:.0}% damaged", damage * 100.)
                                } else {
                                    "intact".into()
                                },
                                dn,
                                dist,
                                t,
                                q.label()
                            ),
                            cfg.message_s,
                        );
                        rec.emit(
                            lua,
                            records::pilot_of(f),
                            &f.typ,
                            f.side,
                            &f.group_name,
                            Some(q.score()),
                            RangeResult::Sling(res),
                            None,
                        );
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

    pub fn drop_zone_list(&self) -> Vec<String> {
        self.drop_zones
            .iter()
            .map(|(z, _)| match z.run_in_deg {
                Some(h) => format!(
                    "{} DZ (run-in {:03.0}, perfect inside {:.0} m)",
                    z.name, h, z.perfect_m
                ),
                None => format!("{} DZ (perfect inside {:.0} m)", z.name, z.perfect_m),
            })
            .collect()
    }

    pub fn dyn_cargo_help(&self) -> String {
        let mut t = vec![
            "DYNAMIC CARGO: at a home field ask the ground crew (F8) for cargo, load it into the cabin with the cargo loader or sling it, and set it down at any of these to have the delivery graded:".to_string(),
        ];
        t.extend(
            self.destinations()
                .into_iter()
                .map(|(n, _, perfect)| format!("  {n} (perfect inside {perfect:.0} m)")),
        );
        if !self.drop_zones.is_empty() {
            t.push("Fixed-wing (C-130J): airdrop the load out of the cargo bay over a drop zone, or air-land it at another field:".into());
            t.extend(self.drop_zone_list().into_iter().map(|z| format!("  {z}")));
        }
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

/// A fixed-wing load has come to rest: grade the airdrop against the nearest
/// drop zone, or record an air-land delivery at another field. True when it
/// is finished with (graded, or nothing more will come of it).
#[allow(clippy::too_many_arguments)]
fn fixed_wing_delivery(
    lua: MizLua,
    cfg: &RangeCfg,
    rec: &mut Recorder,
    c: &mut DynCargo,
    owner: Option<&Flying>,
    s: &StaticObject,
    p: V3,
    zones: &[(DropZoneCfg, V3)],
    fields: &[(String, V3)],
    now: f64,
) -> bool {
    let life = s.get_life().map(|l| l as f64).unwrap_or(c.life0);
    let damage = if c.life0 > 0. { (1. - life / c.life0).clamp(0., 1.) } else { 0. };
    let flight_s = now - c.picked.unwrap_or(c.born);
    if let Some((t_rel, mut rel, rp)) = c.release.clone() {
        let Some(f) = owner else { return true };
        let best = zones
            .iter()
            .map(|(z, zp)| (z, *zp, util::dist2(*zp, p)))
            .min_by(|a, b| a.2.total_cmp(&b.2));
        let Some((z, zp, dist)) = best.filter(|b| b.2 <= cfg.airdrop.reach_m) else {
            let near = best
                .map(|b| format!(", {:.1} nm from {} DZ", b.2 / util::NM, b.0.name))
                .unwrap_or_default();
            records::to_group(
                lua,
                f.group_id,
                &format!(
                    "AIRDROP {}: down off every drop zone{near}; not scored.",
                    c.cargo
                ),
                cfg.message_s,
            );
            return true;
        };
        let brg = util::bearing(zp, p);
        let (long_m, cross_m) = match z.run_in_deg {
            Some(h) => {
                let a = (brg - h).to_radians();
                (Some(dist * a.cos()), Some(dist * a.sin()))
            }
            None => (None, None),
        };
        rel.to_dz_m = Some(util::dist2(rp, zp));
        let q = grading::precision_quality(z.perfect_m, dist);
        let res = AirdropResult {
            method: "airdrop".into(),
            zone: z.name.clone(),
            cargo: c.cargo.clone(),
            mass_kg: c.mass_kg,
            distance_m: dist,
            bearing_deg: brg,
            long_m,
            cross_m,
            quality: q,
            damage,
            release: Some(rel.clone()),
            fall_s: Some(now - t_rel),
            drift_m: Some(util::dist2(rp, p)),
            flight_s,
            dz_pos: util::geo(lua, zp),
            impact_pos: util::geo(lua, p),
        };
        let lr = match (long_m, cross_m) {
            (Some(l), Some(x)) => format!(
                " ({:.0} m {}, {:.0} m {})",
                l.abs(),
                if l >= 0. { "long" } else { "short" },
                x.abs(),
                if x >= 0. { "right" } else { "left" }
            ),
            _ => String::new(),
        };
        records::to_group(
            lua,
            f.group_id,
            &format!(
                "AIRDROP {} ({:.0} kg) on {} DZ: {:.0} m from the point of impact{lr}, released {:.0} ft AGL at {:.0} kt, {:.0} s under canopy - {}",
                c.cargo,
                c.mass_kg,
                z.name,
                dist,
                rel.alt_agl_ft,
                rel.gs_kts,
                now - t_rel,
                q.label()
            ),
            cfg.message_s,
        );
        rec.emit(
            lua,
            records::pilot_of(f),
            &f.typ,
            f.side,
            &f.group_name,
            Some(q.score()),
            RangeResult::Airdrop(res),
            None,
        );
        return true;
    }
    // unloaded on the ground away from home: an air-land delivery
    if c.inside && util::dist2(c.spawn_pos, p) > 5000. {
        let Some(f) = owner.filter(|f| !f.in_air) else { return false };
        let field = fields
            .iter()
            .map(|(n, fp)| (n, util::dist2(*fp, p)))
            .min_by(|a, b| a.1.total_cmp(&b.1));
        let zone = field
            .filter(|x| x.1 < 6000.)
            .map(|x| x.0.clone())
            .unwrap_or_else(|| "a field".into());
        let res = AirdropResult {
            method: "airland".into(),
            zone: zone.clone(),
            cargo: c.cargo.clone(),
            mass_kg: c.mass_kg,
            distance_m: 0.,
            bearing_deg: 0.,
            long_m: None,
            cross_m: None,
            quality: grading::precision_quality(1., 0.),
            damage,
            release: None,
            fall_s: None,
            drift_m: None,
            flight_s,
            dz_pos: util::geo(lua, p),
            impact_pos: util::geo(lua, p),
        };
        records::to_group(
            lua,
            f.group_id,
            &format!(
                "AIR-LAND {} ({:.0} kg) delivered to {zone} after {:.0} min",
                c.cargo,
                c.mass_kg,
                flight_s / 60.
            ),
            cfg.message_s,
        );
        rec.emit(
            lua,
            records::pilot_of(f),
            &f.typ,
            f.side,
            &f.group_name,
            Some(res.quality.score() * (1. - damage).max(0.2)),
            RangeResult::Airdrop(res),
            None,
        );
        return true;
    }
    // set down at home (or not yet anywhere it counts): it can be loaded again
    c.picked = None;
    c.spawn_pos = p;
    c.still_since = None;
    false
}

/// A landing further than this from the mark is not graded.
const PAD_REACH_M: f64 = 60.;

/// The steepest descent in the last second before the touchdown, feet per
/// minute (0 when only climbing). `vs`: (time, vertical speed m/s).
fn touchdown_fpm<'a>(vs: impl IntoIterator<Item = &'a (f64, f64)>, now: f64) -> f64 {
    vs.into_iter()
        .filter(|(t, _)| now - t < 1.2)
        .map(|(_, v)| -v * 60. * util::M_TO_FT)
        .fold(0., f64::max)
}

/// Precision against the pad's perfect radius; a hard landing (over 500
/// fpm) caps it at FAIR.
fn pad_landing_quality(
    perfect_m: f64,
    d: f64,
    fpm: f64,
) -> bfprotocols::range::PrecisionQuality {
    let q = grading::precision_quality(perfect_m, d);
    if fpm > 500. && q > bfprotocols::range::PrecisionQuality::Fair {
        bfprotocols::range::PrecisionQuality::Fair
    } else {
        q
    }
}

fn cargo(lua: MizLua, name: &str) -> Option<StaticObject> {
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
    use dcso3::{
        Color, LuaVec3,
        trigger::{CircleSpec, LineType, SideFilter, TextSpec},
    };
    let r = (|| -> Result<()> {
        let act = Trigger::singleton(lua)?.action()?;
        let c = Color::new(0.3, 1., 0.5, 0.9);
        act.circle_to_all(
            SideFilter::All,
            dcso3::trigger::MarkId::new(),
            CircleSpec {
                center: LuaVec3(p),
                radius: radius.max(5.),
                color: c,
                fill_color: Color::new(0., 0., 0., 0.),
                line_type: LineType::Solid,
                read_only: true,
            },
            None,
        )?;
        let mut l = p;
        l.x += radius.max(5.) + 30.;
        act.text_to_all(
            SideFilter::All,
            dcso3::trigger::MarkId::new(),
            TextSpec {
                pos: LuaVec3(l),
                color: c,
                fill_color: Color::new(0., 0., 0., 0.),
                font_size: 9,
                read_only: true,
                text: name.into(),
            },
        )?;
        Ok(())
    })();
    if let Err(e) = r {
        warn!("could not draw {name}: {e:?}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bfprotocols::range::PrecisionQuality;

    #[test]
    fn touchdown_rate_is_the_last_second_only() {
        // a 1500 fpm descent 3 s out, then 300 fpm at the end
        let vs = [(7., -7.6), (9.0, -1.0), (9.5, -1.52), (10., -0.5)];
        let fpm = touchdown_fpm(&vs, 10.);
        assert!((fpm - 1.52 * 60. * util::M_TO_FT).abs() < 1e-9, "{fpm}");
        // climbing away is not a negative rate
        assert_eq!(touchdown_fpm(&[(9.9, 2.)], 10.), 0.);
        assert_eq!(touchdown_fpm(&[] as &[(f64, f64)], 10.), 0.);
    }

    #[test]
    fn pad_grades() {
        // 2 m perfect radius
        assert_eq!(pad_landing_quality(2., 1.5, 200.), PrecisionQuality::Perfect);
        assert_eq!(pad_landing_quality(2., 7., 200.), PrecisionQuality::Good);
        assert_eq!(pad_landing_quality(2., 30., 200.), PrecisionQuality::Poor);
        // on the mark but hard: capped at FAIR, never raised
        assert_eq!(pad_landing_quality(2., 1., 700.), PrecisionQuality::Fair);
        assert_eq!(pad_landing_quality(2., 30., 700.), PrecisionQuality::Poor);
    }
}
