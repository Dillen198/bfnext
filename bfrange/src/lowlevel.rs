// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Low-level navigation routes.
//!
//! A route is a string of gates along valleys and plains. Fly through the
//! first gate and the clock starts; every gate after it is timed against the
//! plan (the route's ground speed) and the height above the ground is
//! sampled the whole way. The card says how many gates you made, how much of
//! the route you flew under the ceiling, how low you got, and how close to
//! the planned time you were at the last gate.

use crate::{
    ag,
    players::{Flying, Players},
    records::{self, Recorder},
    util::{self, V3},
};
use bfprotocols::range::{
    cfg::{LowLevelRouteCfg, RangeCfg},
    grading, GateTime, LowLevelResult, PilotRef, RangeResult, Track, TrackPt,
};
use dcso3::{coalition::Side, env::miz::GroupId, MizLua};
use fxhash::FxHashMap;
use log::{info, warn};
use std::collections::BTreeMap;

#[derive(Debug)]
struct Route {
    cfg: LowLevelRouteCfg,
    gates: Vec<(String, V3, f64)>,
    /// planned seconds after gate 1, per gate
    planned: Vec<f64>,
}

#[derive(Debug)]
struct Run {
    route: usize,
    pilot: PilotRef,
    typ: String,
    side: Side,
    callsign: String,
    gid: GroupId,
    started: f64,
    next: usize,
    gate_t: Vec<Option<(f64, f64)>>,
    best_d: f64,
    last: f64,
    last_pos: V3,
    dist_m: f64,
    total_s: f64,
    agl_sum: f64,
    agl_max: f64,
    agl_min: f64,
    below_ceiling_s: f64,
    below_floor_s: f64,
    path: Vec<TrackPt>,
}

#[derive(Debug, Default)]
pub struct LowLevel {
    routes: Vec<Route>,
    runs: FxHashMap<String, Run>,
    /// unit -> when they were last told they're not on a route (debounce)
    last_hint: FxHashMap<String, f64>,
}

fn for_side(side_cfg: &str, side: Side) -> bool {
    match side_cfg {
        "blue" => side == Side::Blue,
        "red" => side == Side::Red,
        _ => true,
    }
}

fn mmss(s: f64) -> String {
    let s = s.max(0.).round() as i64;
    format!("{}:{:02}", s / 60, s % 60)
}

impl LowLevel {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg) {
        for r in &cfg.low_level {
            let mut gates = vec![];
            for g in &r.gates {
                match ag::resolve(lua, &g.loc) {
                    Ok(p) => gates.push((g.name.clone(), p, g.radius_m)),
                    Err(e) => warn!("low-level route {} gate {}: {e:?}", r.id, g.name),
                }
            }
            if gates.len() < 2 {
                warn!("low-level route {}: fewer than two gates placed, not used", r.id);
                continue;
            }
            let v = r.speed_kts / util::MS_TO_KTS;
            let mut t = 0.;
            let mut planned = vec![0.];
            for w in gates.windows(2) {
                t += util::dist2(w[0].1, w[1].1) / v;
                planned.push(t);
            }
            info!("low-level route {} ({}): {} gates, {:.0} nm, planned {}", r.id, r.name, gates.len(), gates.windows(2).map(|w| util::dist2(w[0].1, w[1].1)).sum::<f64>() / util::NM, mmss(t));
            self.routes.push(Route { cfg: r.clone(), gates, planned });
        }
    }

    /// F10 brief: the gates with planned times, from the player.
    pub fn brief(&self, from: V3, side: Side) -> Vec<String> {
        let mut out = vec![];
        for r in self.routes.iter().filter(|r| for_side(&r.cfg.side, side)) {
            out.push(format!(
                "{}: {} gates, ceiling {:.0} ft AGL, floor {:.0} ft, {:.0} kts, planned {}. Gate 1 {:03.0}/{:.0}nm",
                r.cfg.name,
                r.gates.len(),
                r.cfg.max_agl_ft,
                r.cfg.min_agl_ft,
                r.cfg.speed_kts,
                mmss(*r.planned.last().unwrap_or(&0.)),
                util::bearing(from, r.gates[0].1),
                util::dist2(from, r.gates[0].1) / util::NM
            ));
            for (i, (name, p, _)) in r.gates.iter().enumerate().skip(1) {
                let prev = r.gates[i - 1].1;
                out.push(format!("  {i:>2}->{} {}: {:03.0} for {:.1} nm, {}", i + 1, name, util::bearing(prev, *p), util::dist2(prev, *p) / util::NM, mmss(r.planned[i])));
            }
        }
        if out.is_empty() {
            out.push("No low-level routes for your side".into());
        }
        out
    }

    /// Start, sample and finish runs; once a second from the slow tick
    /// (player positions are refreshed there).
    pub fn tick(&mut self, lua: MizLua, cfg: &RangeCfg, players: &Players, rec: &mut Recorder, now: f64) {
        if self.routes.is_empty() {
            return;
        }
        let mut done = vec![];
        for f in players.flying.values().filter(|f| f.in_air && !f.is_ground) {
            match self.runs.get_mut(&f.unit_name) {
                None => self.maybe_start(lua, f, now),
                Some(run) => {
                    let r = &self.routes[run.route];
                    let dt = now - run.last;
                    run.last = now;
                    run.dist_m += util::dist2(run.last_pos, f.pos);
                    run.last_pos = f.pos;
                    let agl_ft = f.alt_agl * util::M_TO_FT;
                    run.total_s += dt;
                    run.agl_sum += agl_ft * dt;
                    run.agl_max = run.agl_max.max(agl_ft);
                    run.agl_min = run.agl_min.min(agl_ft);
                    if agl_ft <= r.cfg.max_agl_ft {
                        run.below_ceiling_s += dt;
                    }
                    if agl_ft < r.cfg.min_agl_ft {
                        run.below_floor_s += dt;
                    }
                    if run.path.last().map(|p| now - run.started - p.t >= 2.).unwrap_or(true) {
                        let g = util::geo(lua, f.pos);
                        run.path.push(TrackPt { t: now - run.started, lat: g.lat, lon: g.lon, alt_m: f.pos.y, speed_kts: f.vel.norm() * util::MS_TO_KTS });
                    }
                    // gates: through the next one, or past it
                    let mut msgs = vec![];
                    while run.next < r.gates.len() {
                        let (name, gp, gr) = &r.gates[run.next];
                        let d = util::dist2(f.pos, *gp);
                        let later = r.gates.get(run.next + 1).map(|(_, p, rr)| util::dist2(f.pos, *p) <= *rr).unwrap_or(false);
                        if d <= *gr {
                            let t = now - run.started;
                            run.gate_t[run.next] = Some((t, agl_ft));
                            let dt_plan = t - r.planned[run.next];
                            msgs.push(format!("GATE {} {}: {:+.0} s, {:.0} ft", run.next + 1, name, dt_plan, agl_ft));
                            run.next += 1;
                            run.best_d = f64::MAX;
                        } else if later || (d > run.best_d + 3000. && run.best_d > *gr) {
                            msgs.push(format!("MISSED GATE {} {}", run.next + 1, name));
                            run.next += 1;
                            run.best_d = f64::MAX;
                        } else {
                            run.best_d = run.best_d.min(d);
                            break;
                        }
                    }
                    if run.next >= r.gates.len() {
                        done.push(f.unit_name.clone());
                    } else if !msgs.is_empty() {
                        let (name, gp, _) = &r.gates[run.next];
                        msgs.push(format!(
                            "next {} {:03.0}/{:.1}nm, due {}",
                            name,
                            util::bearing(f.pos, *gp),
                            util::dist2(f.pos, *gp) / util::NM,
                            mmss(r.planned[run.next])
                        ));
                    }
                    if !msgs.is_empty() && run.next < r.gates.len() {
                        records::to_group(lua, run.gid, &msgs.join(" - "), 8);
                    }
                    // gave up: far too long, or far off the route
                    let limit = r.planned.last().copied().unwrap_or(0.) * 2. + 600.;
                    let off = r.gates.iter().map(|(_, p, _)| util::dist2(f.pos, *p)).fold(f64::MAX, f64::min);
                    if now - run.started > limit || off > 40_000. {
                        done.push(f.unit_name.clone());
                    }
                }
            }
        }
        for u in done {
            self.finish(lua, cfg, rec, &u);
        }
    }

    fn maybe_start(&mut self, lua: MizLua, f: &Flying, now: f64) {
        for (ri, r) in self.routes.iter().enumerate() {
            if !for_side(&r.cfg.side, f.side) {
                continue;
            }
            let (name, gp, gr) = &r.gates[0];
            if util::dist2(f.pos, *gp) > *gr {
                continue;
            }
            let agl_ft = f.alt_agl * util::M_TO_FT;
            if agl_ft > r.cfg.max_agl_ft * 3. {
                if self.last_hint.get(&f.unit_name).map(|t| now - t > 120.).unwrap_or(true) {
                    self.last_hint.insert(f.unit_name.clone(), now);
                    records::to_group(lua, f.group_id, &format!("{}: through gate 1 {name} below {:.0} ft AGL to start the clock", r.cfg.name, r.cfg.max_agl_ft), 8);
                }
                continue;
            }
            let (n1, p1, _) = &r.gates[1];
            records::to_group(
                lua,
                f.group_id,
                &format!(
                    "LOW LEVEL {}: clock running at gate 1 {name}. Ceiling {:.0} ft AGL, {:.0} kts. Next {n1} {:03.0}/{:.1}nm, due {}",
                    r.cfg.name,
                    r.cfg.max_agl_ft,
                    r.cfg.speed_kts,
                    util::bearing(f.pos, *p1),
                    util::dist2(f.pos, *p1) / util::NM,
                    mmss(r.planned[1])
                ),
                12,
            );
            let mut gate_t = vec![None; r.gates.len()];
            gate_t[0] = Some((0., agl_ft));
            self.runs.insert(
                f.unit_name.clone(),
                Run {
                    route: ri,
                    pilot: records::pilot_of(f),
                    typ: f.typ.clone(),
                    side: f.side,
                    callsign: f.group_name.clone(),
                    gid: f.group_id,
                    started: now,
                    next: 1,
                    gate_t,
                    best_d: f64::MAX,
                    last: now,
                    last_pos: f.pos,
                    dist_m: 0.,
                    total_s: 0.,
                    agl_sum: 0.,
                    agl_max: agl_ft,
                    agl_min: agl_ft,
                    below_ceiling_s: 0.,
                    below_floor_s: 0.,
                    path: vec![],
                },
            );
            return;
        }
    }

    /// Finish a run (at the last gate, or given up / aircraft gone).
    pub fn finish(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, unit: &str) {
        let Some(run) = self.runs.remove(unit) else { return };
        let r = &self.routes[run.route];
        let hit = run.gate_t.iter().filter(|g| g.is_some()).count() as u32;
        if hit < 2 {
            return;
        }
        let total = r.gates.len() as u32;
        let last_hit = run.gate_t.iter().enumerate().rev().find_map(|(i, g)| g.map(|(t, _)| (i, t)));
        let (tot_err, time_s) = match last_hit {
            Some((i, t)) => (t - r.planned[i], t),
            None => (0., run.total_s),
        };
        let pct = if run.total_s > 0. { run.below_ceiling_s / run.total_s * 100. } else { 0. };
        let missed = total - hit;
        let quality = grading::low_level_quality(missed, pct, tot_err, r.cfg.tot_tolerance_s, run.below_floor_s);
        let mut calls = vec![];
        if missed > 0 {
            calls.push(format!("{missed} gate(s) missed"));
        }
        if pct < 95. {
            calls.push(format!("{:.0}% of the route above {:.0} ft", 100. - pct, r.cfg.max_agl_ft));
        }
        if run.below_floor_s > 5. {
            calls.push(format!("{:.0} s below the {:.0} ft floor", run.below_floor_s, r.cfg.min_agl_ft));
        }
        if tot_err.abs() > r.cfg.tot_tolerance_s {
            calls.push(format!("{:.0} s {} at the last gate", tot_err.abs(), if tot_err > 0. { "late" } else { "early" }));
        }
        let res = LowLevelResult {
            route: r.cfg.name.clone(),
            gates_hit: hit,
            gates_total: total,
            gates: r
                .gates
                .iter()
                .enumerate()
                .map(|(i, (name, p, _))| GateTime {
                    gate: name.clone(),
                    t: run.gate_t[i].map(|g| g.0),
                    planned_t: r.planned[i],
                    agl_ft: run.gate_t[i].map(|g| g.1),
                    pos: util::geo(lua, *p),
                })
                .collect(),
            time_s,
            planned_s: *r.planned.last().unwrap_or(&0.),
            tot_error_s: tot_err,
            avg_agl_ft: if run.total_s > 0. { run.agl_sum / run.total_s } else { 0. },
            max_agl_ft: run.agl_max,
            min_agl_ft: run.agl_min,
            max_allowed_agl_ft: r.cfg.max_agl_ft,
            min_allowed_agl_ft: r.cfg.min_agl_ft,
            pct_below_ceiling: pct,
            below_floor_s: run.below_floor_s,
            avg_speed_kts: if run.total_s > 0. { run.dist_m / run.total_s * util::MS_TO_KTS } else { 0. },
            quality,
            calls: calls.clone(),
        };
        if cfg.in_game_results {
            records::to_group(
                lua,
                run.gid,
                &format!(
                    "LOW LEVEL {}: {hit}/{total} gates, {:.0}% under {:.0} ft, avg {:.0} ft, TOT {:+.0} s - {}{}",
                    r.cfg.name,
                    pct,
                    r.cfg.max_agl_ft,
                    res.avg_agl_ft,
                    tot_err,
                    quality.label(),
                    if calls.is_empty() { String::new() } else { format!("\n{}", calls.join("; ")) }
                ),
                cfg.message_s,
            );
        }
        let mut paths = BTreeMap::new();
        paths.insert(run.pilot.name.clone(), run.path);
        rec.emit(lua, run.pilot, &run.typ, run.side, &run.callsign, Some(quality.score()), RangeResult::LowLevel(res), Some(Track::Path { paths }));
    }

    pub fn player_out(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, unit: &str) {
        self.finish(lua, cfg, rec, unit);
        self.last_hint.remove(unit);
    }
}
