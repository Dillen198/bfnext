// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Ship decks: frigates and destroyers steaming a leg, for helicopter deck
//! landings.
//!
//! A deck landing is graded like a pad landing, but in the ship's own frame:
//! how far off the deck centreline the wheels came down, the sink rate
//! relative to the deck, the heading against the ship's, and how fast the
//! ship was going. The ships are immortal and hold fire.

use crate::{
    ag,
    players::{Flying, Players},
    records::{self, Recorder},
    spawn::{self, Spawns},
    util::{self, V3},
};
use anyhow::Result;
use bfprotocols::range::{
    cfg::{RangeCfg, ShipDeckCfg},
    grading, LandingResult, LiveShipDeck, PrecisionQuality, RangeResult,
};
use dcso3::{coalition::Side, unit::Unit, MizLua};
use fxhash::FxHashMap;
use log::{info, warn};
use serde_json::json;
use std::collections::VecDeque;

#[derive(Debug)]
struct Deck {
    cfg: ShipDeckCfg,
    side: Side,
    group: String,
    unit: String,
    start: V3,
}

#[derive(Debug, Default)]
struct Approach {
    deck: usize,
    /// (time, vertical speed relative to the ship, m/s)
    vs: VecDeque<(f64, f64)>,
    hover_s: f64,
    last: f64,
}

#[derive(Debug, Default)]
pub struct Decks {
    decks: Vec<Deck>,
    appr: FxHashMap<String, Approach>,
}

fn spawn_deck(lua: MizLua, d: &Deck, spawns: &mut Spawns, now: f64) -> Result<()> {
    let country = spawn::country_id(d.cfg.country.as_deref().unwrap_or(spawn::default_country(d.side)))?;
    let speed = d.cfg.speed_kts / util::MS_TO_KTS;
    let end = util::offset(d.start, d.cfg.heading_deg, d.cfg.leg_nm * util::NM, 0.);
    let route = vec![
        spawn::waypoint(d.start, 0., speed, vec![]),
        spawn::waypoint(end, 0., speed, vec![]),
        spawn::waypoint(d.start, 0., speed, vec![spawn::wrapped(1, json!({ "id": "SwitchWaypoint", "params": { "fromWaypointIndex": 3, "goToWaypointIndex": 1 } }))]),
    ];
    let g = spawn::surface_group(&d.group, &[(d.cfg.typ.clone(), d.start, d.cfg.heading_deg)], "Excellent", route);
    spawn::add_group(lua, country, spawn::SHIP, &g)?;
    spawns.defer(now + 2., spawn::Pending::GroupCommand { group: d.group.clone(), cmd: spawn::bool_cmd("SetImmortal", true) });
    spawns.defer(now + 2., spawn::Pending::GroupOption { group: d.group.clone(), id: 0, value: json!(4) });
    if let Some(t) = &d.cfg.tacan {
        // needs the unit id, so it waits for the ship to exist
        spawns.defer(now + 3., spawn::Pending::ShipTacan { group: d.group.clone(), unit: d.unit.clone(), tacan: t.clone() });
    }
    Ok(())
}

impl Decks {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg, spawns: &mut Spawns, now: f64) {
        for c in &cfg.ship_decks {
            let start = match ag::resolve(lua, &c.loc) {
                Ok(mut p) => {
                    p.y = 0.;
                    p
                }
                Err(e) => {
                    warn!("ship deck {}: {e:?}", c.id);
                    continue;
                }
            };
            if !util::is_water(lua, start) {
                warn!("ship deck {}: {} is not over water", c.id, c.loc.describe());
                continue;
            }
            let group = format!("RNG-DECK-{}", c.id);
            let d = Deck { cfg: c.clone(), side: spawn::side_of_str(&c.side), unit: format!("{group}-1"), group, start };
            match spawn_deck(lua, &d, spawns, now) {
                Ok(()) => {
                    info!("ship deck {} ({} {}) under way at {:.0} kts", c.id, c.name, c.typ, c.speed_kts);
                    self.decks.push(d)
                }
                Err(e) => warn!("ship deck {}: {e:?}", c.id),
            }
        }
    }

    pub fn active(&self) -> bool {
        !self.appr.is_empty()
    }

    /// 5 Hz: helicopters close to a deck, their sink rate relative to it.
    pub fn tick(&mut self, lua: MizLua, players: &Players, spawns: &mut Spawns, now: f64) {
        if self.decks.is_empty() {
            return;
        }
        // a ship that somehow went away comes back
        for d in &self.decks {
            if now as i64 % 30 == 0 && !spawn::group_exists(lua, &d.group) {
                if let Err(e) = spawn_deck(lua, d, spawns, now) {
                    warn!("ship deck {} respawn: {e:?}", d.cfg.id)
                }
            }
        }
        let ships: Vec<Option<(V3, V3)>> = self
            .decks
            .iter()
            .map(|d| Unit::get_by_name(lua, &d.unit).ok().and_then(|u| Some((u.get_point().ok()?.0, u.get_velocity().ok()?.0))))
            .collect();
        for f in players.flying.values().filter(|f| f.is_helo) {
            let near = ships
                .iter()
                .enumerate()
                .filter_map(|(i, s)| s.map(|(p, v)| (i, p, v, util::dist2(p, f.pos))))
                .filter(|(.., d)| *d < 400.)
                .min_by(|a, b| a.3.total_cmp(&b.3));
            match near {
                Some((i, _, sv, d)) if f.in_air => {
                    let a = self.appr.entry(f.unit_name.clone()).or_default();
                    a.deck = i;
                    if now - a.last >= 0.2 {
                        let dt = if a.last == 0. { 0. } else { now - a.last };
                        a.last = now;
                        let vy = dcso3::unit::Unit::get_by_name(lua, &f.unit_name)
                            .and_then(|u| u.get_velocity())
                            .map(|v| v.0.y)
                            .unwrap_or(f.vel.y);
                        a.vs.push_back((now, vy - sv.y));
                        while a.vs.len() > 15 {
                            a.vs.pop_front();
                        }
                        if d < 30. && f.alt_agl < 30. {
                            a.hover_s += dt;
                        }
                    }
                }
                Some(_) => (),
                None => {
                    self.appr.remove(&f.unit_name);
                }
            }
        }
    }

    /// LAND on a ship: grade it if it was one of ours.
    pub fn landed(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, f: &Flying, place: &str, now: f64) -> bool {
        let Some(di) = self.decks.iter().position(|d| d.unit == place) else { return false };
        let a = self.appr.remove(&f.unit_name).unwrap_or_default();
        let d = &self.decks[di];
        let Ok(ship) = Unit::get_by_name(lua, &d.unit) else { return true };
        let (Ok(sp), Ok(sv)) = (ship.get_position(), ship.get_velocity()) else { return true };
        let ship_hdg = util::hdg(sp.x.0);
        let hp = dcso3::unit::Unit::get_by_name(lua, &f.unit_name).and_then(|u| u.get_point()).map(|p| p.0).unwrap_or(f.pos);
        // offsets in the ship's frame: along its heading, and to starboard
        let rel = hp - sp.p.0;
        let (s, c) = ship_hdg.to_radians().sin_cos();
        let fwd = rel.x * c + rel.z * s;
        let stbd = -rel.x * s + rel.z * c;
        let fpm = a.vs.iter().filter(|(t, _)| now - t < 1.2).map(|(_, v)| -v * 60. * util::M_TO_FT).fold(0., f64::max);
        let herr = util::angdiff(f.hdg, ship_hdg).abs();
        let mut q = grading::precision_quality(d.cfg.perfect_m, stbd.abs());
        if (fpm > 500. || herr > 30.) && q > PrecisionQuality::Fair {
            q = PrecisionQuality::Fair;
        }
        let ship_kts = sv.0.norm() * util::MS_TO_KTS;
        let res = LandingResult {
            drill: "ship".into(),
            pad: d.cfg.name.clone(),
            distance_m: stbd.abs(),
            touchdown_fpm: fpm,
            heading_error_deg: Some(herr),
            hover_s: a.hover_s,
            quality: q,
            pad_pos: util::geo(lua, sp.p.0),
            touchdown_pos: util::geo(lua, hp),
            ship_speed_kts: Some(ship_kts),
        };
        if cfg.in_game_results {
            records::to_group(
                lua,
                f.group_id,
                &format!(
                    "DECK LANDING {} ({:.0} kts): {:.1} m {} of centreline, {:.0} m {} of the ship's centre, {:.0} fpm, heading off {:.0} deg - {}",
                    d.cfg.name,
                    ship_kts,
                    stbd.abs(),
                    if stbd >= 0. { "starboard" } else { "port" },
                    fwd.abs(),
                    if fwd >= 0. { "forward" } else { "aft" },
                    fpm,
                    herr,
                    q.label()
                ),
                cfg.message_s,
            );
        }
        rec.emit(lua, records::pilot_of(f), &f.typ, f.side, &f.group_name, Some(q.score()), RangeResult::Landing(res), None);
        true
    }

    pub fn player_left(&mut self, unit: &str) {
        self.appr.remove(unit);
    }

    pub fn describe(&self, lua: MizLua, from: V3) -> Vec<String> {
        self.decks
            .iter()
            .filter_map(|d| {
                let u = Unit::get_by_name(lua, &d.unit).ok()?;
                let p = u.get_point().ok()?.0;
                let v = u.get_velocity().ok()?.0;
                Some(format!(
                    "{} ({}, {}) {:03.0}/{:.0}nm, heading {:03.0} at {:.0} kts{}",
                    d.cfg.name,
                    d.cfg.typ,
                    records::side_str(d.side),
                    util::bearing(from, p),
                    util::dist2(from, p) / util::NM,
                    util::hdg(v),
                    v.norm() * util::MS_TO_KTS,
                    d.cfg.tacan.as_ref().map(|t| format!(", TACAN {}", t.describe())).unwrap_or_default()
                ))
            })
            .collect()
    }

    pub fn live(&self, lua: MizLua) -> Vec<LiveShipDeck> {
        self.decks
            .iter()
            .filter_map(|d| {
                let u = Unit::get_by_name(lua, &d.unit).ok()?;
                let p = u.get_point().ok()?.0;
                let v = u.get_velocity().ok()?.0;
                Some(LiveShipDeck {
                    id: d.cfg.id.clone(),
                    name: d.cfg.name.clone(),
                    unit_type: d.cfg.typ.clone(),
                    pos: util::geo(lua, p),
                    heading_deg: util::hdg(v),
                    speed_kts: v.norm() * util::MS_TO_KTS,
                    tacan: d.cfg.tacan.as_ref().map(|t| t.describe()),
                })
            })
            .collect()
    }
}
