// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Electronic warfare: GPS / GLONASS jamming and spoofing, radio jamming.
//!
//! DCS 2.9.29 added ground jammers (the ZIL SKP-11 `GPS_Spoofer_Red` /
//! `_Blue`) and the `ActivateJammer` command. A jammer sector teaches what
//! a modern battlefield does to GPS weapons and navigation: players are
//! warned when they fly into a jammer's radius, every bomb released inside it
//! carries "GPS denied" on its result card, and the jammer itself is a target
//! -- kill it and the sky clears until it is rebuilt.

use crate::{
    ag,
    players::Players,
    records,
    spawn::{self, Spawns},
    util::{self, V3},
};
use anyhow::Result;
use bfprotocols::range::{cfg::{JammerCfg, RangeCfg}, LiveJammer};
use dcso3::{coalition::Side, MizLua};
use fxhash::FxHashMap;
use log::{info, warn};
use serde_json::json;

/// Rebuild a destroyed jammer after this long.
const JAMMER_RESPAWN_S: f64 = 900.;

#[derive(Debug)]
struct Jam {
    cfg: JammerCfg,
    side: Side,
    pos: V3,
    spoof_to: Option<V3>,
    group: String,
    alive: bool,
    dead_at: Option<f64>,
}

impl Jam {
    fn radius(&self) -> f64 {
        self.cfg.radius_nm * util::NM
    }

    fn mode(s: &str) -> i64 {
        match s {
            "jam" | "simple" => 1,
            "spoof" | "adaptive" => 2,
            _ => 0,
        }
    }

    fn describe(&self) -> String {
        let mut what = vec![];
        match self.cfg.gps.as_str() {
            "jam" => what.push("GPS jammed"),
            "spoof" => what.push("GPS SPOOFED"),
            _ => (),
        }
        match self.cfg.glonass.as_str() {
            "jam" => what.push("GLONASS jammed"),
            "spoof" => what.push("GLONASS spoofed"),
            _ => (),
        }
        if self.cfg.radio != "off" {
            what.push("radios jammed");
        }
        what.join(", ")
    }
}

#[derive(Debug, Default)]
pub struct Ew {
    jams: Vec<Jam>,
    /// player unit -> the jammer they were last told they are inside
    inside: FxHashMap<String, Option<usize>>,
    last: f64,
}

fn activate(lua: MizLua, j: &Jam, spawns: &mut Spawns, now: f64) -> Result<()> {
    let country = spawn::country_id(j.cfg.country.as_deref().unwrap_or(spawn::default_country(j.side)))?;
    let jammer = if j.side == Side::Blue { "GPS_Spoofer_Blue" } else { "GPS_Spoofer_Red" };
    let support = if j.side == Side::Blue { "M 818" } else { "Ural-375" };
    let units: Vec<(String, V3, f64)> = [(jammer, 0., 0.), (support, 40., 90.), (support, 40., 200.)]
        .iter()
        .map(|(t, r, a)| {
            let p = util::offset(j.pos, *a, *r, 0.);
            let mut q = util::nearest_land(lua, p, 200.).unwrap_or(p);
            q.y = util::ground_height(lua, q);
            (t.to_string(), q, 0.)
        })
        .collect();
    let g = spawn::surface_group(&j.group, &units, "Excellent", vec![spawn::ground_waypoint(j.pos, 0., false, vec![])]);
    spawn::add_group(lua, country, spawn::GROUND, &g)?;
    // the Mission Editor always writes the spoof point when GPS or GLONASS
    // is on (it defaults to 5 km off the group); radio bands are MHz
    let sp = j.spoof_to.unwrap_or(j.pos);
    let mut params = json!({
        "gpsSpoofing": Jam::mode(&j.cfg.gps),
        "glonassSpoofing": Jam::mode(&j.cfg.glonass),
        "radioJamming": Jam::mode(&j.cfg.radio),
        "x": sp.x,
        "y": sp.z,
    });
    if j.cfg.radio != "off" {
        let [a, b] = j.cfg.radio_band_mhz.unwrap_or([225., 400.]);
        params["jammingStart"] = json!(a);
        params["jammingEnd"] = json!(b);
    }
    spawns.defer(now + 3., spawn::Pending::GroupCommand { group: j.group.clone(), cmd: json!({ "id": "ActivateJammer", "params": params }) });
    spawns.defer(now + 2., spawn::Pending::GroupOption { group: j.group.clone(), id: 0, value: json!(4) });
    Ok(())
}

impl Ew {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg, spawns: &mut Spawns, now: f64) {
        for c in &cfg.jammers {
            let pos = match ag::resolve(lua, &c.loc) {
                Ok(p) => p,
                Err(e) => {
                    warn!("jammer {}: {e:?}", c.id);
                    continue;
                }
            };
            let spoof_to = c.spoof_to.as_ref().and_then(|l| ag::resolve(lua, l).ok());
            let mut j = Jam {
                cfg: c.clone(),
                side: spawn::side_of_str(&c.side),
                pos,
                spoof_to,
                group: format!("RNG-JAM-{}", c.id),
                alive: true,
                dead_at: None,
            };
            if let Err(e) = activate(lua, &j, spawns, now) {
                warn!("jammer {}: {e:?}", c.id);
                j.alive = false;
            }
            info!("jammer {} ({}): {} within {:.0} nm", c.id, c.name, j.describe(), c.radius_nm);
            self.jams.push(j);
        }
    }

    /// The enemy jammer (to `side`) whose radius `p` is inside, if any.
    pub fn denied_at(&self, p: V3, side: Side) -> Option<String> {
        self.jams
            .iter()
            .filter(|j| j.alive && j.side != side && util::dist2(p, j.pos) <= j.radius())
            .filter(|j| j.cfg.gps != "off" || j.cfg.glonass != "off")
            .map(|j| j.cfg.name.clone())
            .next()
    }

    pub fn unit_dead(&mut self, lua: MizLua, unit: &str, now: f64) {
        for j in self.jams.iter_mut() {
            // the jammer is unit 1; the trucks don't matter
            if unit == format!("{}-1", j.group) && j.alive {
                j.alive = false;
                j.dead_at = Some(now);
                info!("jammer {} destroyed", j.cfg.id);
                let _ = lua;
            }
        }
    }

    /// Warnings in and out of jammer radii, rebuilding; every 2 s.
    pub fn tick(&mut self, lua: MizLua, players: &Players, spawns: &mut Spawns, now: f64) {
        if now - self.last < 2. || self.jams.is_empty() {
            return;
        }
        self.last = now;
        for j in self.jams.iter_mut() {
            if let Some(t) = j.dead_at {
                if now - t >= JAMMER_RESPAWN_S {
                    spawn::destroy_group(lua, &j.group);
                    match activate(lua, j, spawns, now) {
                        Ok(()) => {
                            j.alive = true;
                            j.dead_at = None;
                            info!("jammer {} rebuilt", j.cfg.id);
                        }
                        Err(e) => {
                            warn!("jammer {} rebuild: {e:?}", j.cfg.id);
                            j.dead_at = Some(now);
                        }
                    }
                }
            }
        }
        self.inside.retain(|u, _| players.flying.contains_key(u));
        for f in players.flying.values().filter(|f| !f.is_ground) {
            let now_in = self
                .jams
                .iter()
                .enumerate()
                .filter(|(_, j)| j.alive && j.side != f.side && util::dist2(f.pos, j.pos) <= j.radius())
                .map(|(i, _)| i)
                .next();
            let before = self.inside.insert(f.unit_name.clone(), now_in);
            let Some(before) = before else { continue };
            if before == now_in {
                continue;
            }
            match now_in {
                Some(i) => {
                    let j = &self.jams[i];
                    records::to_group(
                        lua,
                        f.group_id,
                        &format!(
                            "EW WARNING - {}: {} within {:.0} nm of the jammer ({:03.0}/{:.0}nm). Expect GPS/INS weapons and navigation to degrade: use laser, TV or your own eyes. Kill the jammer to clear it.",
                            j.cfg.name,
                            j.describe(),
                            j.cfg.radius_nm,
                            util::bearing(f.pos, j.pos),
                            util::dist2(f.pos, j.pos) / util::NM
                        ),
                        15,
                    );
                }
                None => records::to_group(lua, f.group_id, "EW: clear of jamming", 8),
            }
        }
    }

    pub fn describe(&self, from: V3, side: Side) -> Vec<String> {
        self.jams
            .iter()
            .filter(|j| j.side != side)
            .map(|j| {
                format!(
                    "{} {:03.0}/{:.0}nm: {} within {:.0} nm{}",
                    j.cfg.name,
                    util::bearing(from, j.pos),
                    util::dist2(from, j.pos) / util::NM,
                    j.describe(),
                    j.cfg.radius_nm,
                    if j.alive { "" } else { " - DESTROYED, rebuilding" }
                )
            })
            .collect()
    }

    pub fn live(&self, lua: MizLua) -> Vec<LiveJammer> {
        self.jams
            .iter()
            .map(|j| LiveJammer {
                id: j.cfg.id.clone(),
                name: j.cfg.name.clone(),
                side: records::side_str(j.side).into(),
                pos: util::geo(lua, j.pos),
                radius_m: j.radius(),
                gps: j.cfg.gps.clone(),
                glonass: j.cfg.glonass.clone(),
                radio: j.cfg.radio.clone(),
                alive: j.alive,
            })
            .collect()
    }
}
