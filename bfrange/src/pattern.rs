// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Field landings: every runway landing is graded.
//!
//! Aircraft near a field and low are sampled five times a second. On
//! RUNWAY_TOUCH the engine works out which runway and which end from the
//! touchdown point and the aircraft's own track -- never from `getRunways()`
//! course alone, whose sign is not consistent across terrains (see bflib's
//! `took_off_off_runway`) -- then grades the touchdown against the aim point
//! and the centreline, the sink rate in the last second, and the approach
//! at 1 nm and ½ nm against a 3° glideslope. A take-off within 15 s makes it
//! a touch-and-go; the card waits that long before it is written.

use crate::{
    players::{Flying, Players},
    records::{self, Recorder},
    util::{self, V3},
};
use bfprotocols::range::{
    cfg::RangeCfg, grading, FieldLandingOutcome, FieldLandingResult, PilotRef, RangeResult,
};
use dcso3::{airbase::AirbaseCategory, coalition::Side, env::miz::GroupId, world::World, MizLua};
use fxhash::FxHashMap;
use log::{info, warn};
use std::collections::VecDeque;

/// Sample aircraft this close to a field, and this low.
const NEAR_M: f64 = 8. * util::NM;
const LOW_M: f64 = 1200.;

#[derive(Debug, Clone)]
struct Rwy {
    name: String,
    centre: V3,
    length: f64,
    width: f64,
    /// both axis candidates, degrees true (DCS's course and its negation)
    axes: [f64; 2],
}

#[derive(Debug)]
struct Field {
    name: String,
    pos: V3,
    rwys: Vec<Rwy>,
}

#[derive(Debug, Clone, Copy)]
struct Sample {
    t: f64,
    p: V3,
    v: V3,
}

#[derive(Debug)]
struct Pending {
    res: FieldLandingResult,
    pilot: PilotRef,
    typ: String,
    side: Side,
    callsign: String,
    gid: GroupId,
    due: f64,
}

#[derive(Debug, Default)]
pub struct Pattern {
    fields: Vec<Field>,
    track: FxHashMap<String, VecDeque<Sample>>,
    pending: FxHashMap<String, Pending>,
    /// unit -> last touchdown time (a bounce is not a second landing)
    last_touch: FxHashMap<String, f64>,
    last_sample: f64,
}

/// The runway end number closest to a true landing heading, from DCS's
/// "09-27" style name.
fn designator(name: &str, hdg: f64) -> String {
    let parts: Vec<&str> = name.split(['-', '/', ' ']).map(|s| s.trim()).filter(|s| !s.is_empty()).collect();
    let num = |s: &str| s.chars().take_while(|c| c.is_ascii_digit()).collect::<String>().parse::<f64>().ok();
    parts
        .iter()
        .filter_map(|p| num(p).map(|n| (util::angdiff(n * 10., hdg).abs(), p.to_string())))
        .min_by(|a, b| a.0.total_cmp(&b.0))
        .filter(|(d, _)| *d < 40.)
        .map(|(_, p)| p)
        .unwrap_or_else(|| format!("{:02}", ((hdg / 10.).round() as i64 - 1).rem_euclid(36) + 1))
}

impl Pattern {
    pub fn init(&mut self, lua: MizLua, cfg: &RangeCfg) {
        if !cfg.pattern.enabled {
            return;
        }
        let r = (|| -> anyhow::Result<()> {
            for ab in World::singleton(lua)?.get_airbases()? {
                let ab = ab?;
                if ab.get_category().ok() != Some(AirbaseCategory::Airdrome) {
                    continue;
                }
                let name = ab.as_object()?.get_name()?.to_string();
                if !cfg.pattern.fields.is_empty() && !cfg.pattern.fields.iter().any(|f| f.eq_ignore_ascii_case(&name)) {
                    continue;
                }
                let mut rwys = vec![];
                for rw in ab.get_runways()? {
                    let Ok(rw) = rw else { continue };
                    let (Ok(c), Ok(course), Ok(len), Ok(w)) = (rw.position(), rw.course(), rw.length(), rw.width()) else { continue };
                    if len < 300. {
                        continue;
                    }
                    let d = course.to_degrees();
                    rwys.push(Rwy {
                        name: rw.name().map(|s| s.to_string()).unwrap_or_default(),
                        centre: c.0,
                        length: len,
                        width: w,
                        axes: [d.rem_euclid(360.), (-d).rem_euclid(360.)],
                    });
                }
                if !rwys.is_empty() {
                    self.fields.push(Field { name, pos: ab.get_point()?.0, rwys });
                }
            }
            Ok(())
        })();
        if let Err(e) = r {
            warn!("pattern: reading the airfields failed: {e:?}")
        }
        info!("pattern: grading landings at {} airfields", self.fields.len());
    }

    /// 5 Hz sampling of fixed-wing players low near a field.
    pub fn tick(&mut self, lua: MizLua, cfg: &RangeCfg, players: &Players, rec: &mut Recorder, now: f64) {
        // cards whose touch-and-go window has closed
        let due: Vec<String> = self.pending.iter().filter(|(_, p)| now >= p.due).map(|(u, _)| u.clone()).collect();
        for u in due {
            if let Some(p) = self.pending.remove(&u) {
                self.emit(lua, cfg, rec, p);
            }
        }
        if self.fields.is_empty() || now - self.last_sample < 0.2 {
            return;
        }
        self.last_sample = now;
        self.track.retain(|u, _| players.flying.contains_key(u));
        for f in players.flying.values().filter(|f| !f.is_helo && !f.is_ground) {
            let near = self.fields.iter().any(|fl| util::dist2(fl.pos, f.pos) < NEAR_M) && f.alt_agl < LOW_M;
            if !near {
                self.track.remove(&f.unit_name);
                continue;
            }
            let (p, v) = match dcso3::unit::Unit::get_by_name(lua, &f.unit_name).and_then(|u| Ok((u.get_point()?.0, u.get_velocity()?.0))) {
                Ok(x) => x,
                Err(_) => (f.pos, f.vel),
            };
            let q = self.track.entry(f.unit_name.clone()).or_default();
            q.push_back(Sample { t: now, p, v });
            while q.front().map(|s| now - s.t > 90.).unwrap_or(false) {
                q.pop_front();
            }
        }
    }

    pub fn active(&self) -> bool {
        !self.track.is_empty() || !self.pending.is_empty()
    }

    /// RUNWAY_TOUCH at an airfield.
    pub fn touch(&mut self, lua: MizLua, cfg: &RangeCfg, f: &Flying, place: &str, now: f64) {
        if f.is_helo || f.is_ground {
            return;
        }
        let Some(field) = self.fields.iter().find(|fl| fl.name == place) else { return };
        if self.last_touch.insert(f.unit_name.clone(), now).map(|t| now - t < 20.).unwrap_or(false) {
            // a bounce: note it on the card already waiting
            if let Some(p) = self.pending.get_mut(&f.unit_name) {
                if !p.res.calls.iter().any(|c| c == "bounced") {
                    p.res.calls.push("bounced".into());
                }
            }
            return;
        }
        let samples: Vec<Sample> = self.track.get(&f.unit_name).map(|q| q.iter().copied().collect()).unwrap_or_default();
        let (p, v) = match dcso3::unit::Unit::get_by_name(lua, &f.unit_name).and_then(|u| Ok((u.get_point()?.0, u.get_velocity()?.0))) {
            Ok(x) => x,
            Err(_) => (f.pos, f.vel),
        };
        let trk = util::hdg(v);
        // the runway end: the axis direction nearest the track, with the
        // touchdown point inside that runway's rectangle
        let mut best: Option<(f64, &Rwy, f64)> = None;
        for rw in &field.rwys {
            for axis in rw.axes {
                for dir in [axis, (axis + 180.).rem_euclid(360.)] {
                    let off = util::angdiff(dir, trk).abs();
                    let d = p - rw.centre;
                    let (s, c) = dir.to_radians().sin_cos();
                    let along = d.x * c + d.z * s;
                    let across = -d.x * s + d.z * c;
                    if along.abs() <= rw.length / 2. + 400. && across.abs() <= rw.width / 2. + 120. && off < 30. {
                        if best.map(|b| off < b.0).unwrap_or(true) {
                            best = Some((off, rw, dir));
                        }
                    }
                }
            }
        }
        let Some((_, rw, dir)) = best else {
            info!("pattern: {} touched down at {place} but not on a runway we know", f.name);
            return;
        };
        let (s, c) = dir.to_radians().sin_cos();
        let thr = V3::new(rw.centre.x - c * rw.length / 2., rw.centre.y, rw.centre.z - s * rw.length / 2.);
        let rel = |q: V3| {
            let d = q - thr;
            (d.x * c + d.z * s, -d.x * s + d.z * c)
        };
        let (along, across) = rel(p);
        let aim = cfg.pattern.aim_point_m;
        let elev = util::ground_height(lua, p);
        // sink rate: the steepest descent in the last 1.2 s
        let fpm = samples
            .iter()
            .filter(|x| now - x.t < 1.2)
            .map(|x| -x.v.y * 60. * util::M_TO_FT)
            .fold(-v.y * 60. * util::M_TO_FT, f64::max);
        // the approach at 1 nm and ½ nm from the aim point
        let at = |dist: f64| -> Option<(f64, f64, f64)> {
            samples
                .iter()
                .map(|x| {
                    let (a, cr) = rel(x.p);
                    (x, aim - a, cr)
                })
                .filter(|(_, r, _)| *r > 0.)
                .min_by(|a, b| (a.1 - dist).abs().total_cmp(&(b.1 - dist).abs()))
                .filter(|(_, r, _)| (r - dist).abs() < 350.)
                .map(|(x, r, cr)| {
                    let gs = ((x.p.y - elev) / r).atan().to_degrees() - cfg.pattern.glideslope_deg;
                    let lu = (cr / r).atan().to_degrees();
                    (gs, lu, -x.v.y * 60. * util::M_TO_FT)
                })
        };
        let one = at(util::NM);
        let half = at(util::NM / 2.);
        let stable = match (one, half) {
            (Some(a), Some(b)) => [a, b].iter().all(|(gs, lu, vs)| gs.abs() < 1. && lu.abs() < 3. && *vs < 1200.),
            _ => false,
        };
        let undershoot = along < 0.;
        let mut calls = vec![];
        let err = along - aim;
        if err.abs() > 150. {
            calls.push(format!("{:.0} m {}", err.abs(), if err > 0. { "long" } else { "short" }));
        }
        if across.abs() > 5. {
            calls.push(format!("{:.0} m {} of centreline", across.abs(), if across > 0. { "right" } else { "left" }));
        }
        if fpm > 600. {
            calls.push(format!("hard landing, {fpm:.0} fpm"));
        }
        match one {
            Some((gs, lu, _)) => {
                if gs.abs() >= 1. {
                    calls.push(format!("{} at 1 nm ({gs:+.1}°)", if gs > 0. { "high" } else { "low" }));
                }
                if lu.abs() >= 3. {
                    calls.push(format!("lined up {} at 1 nm", if lu > 0. { "right" } else { "left" }));
                }
            }
            None => calls.push("no straight-in final (no 1 nm sample)".into()),
        }
        if undershoot {
            calls.push("touched down short of the threshold".into());
        }
        let quality = grading::field_landing_quality(err, across, fpm, stable, undershoot);
        let res = FieldLandingResult {
            airfield: field.name.clone(),
            runway: designator(&rw.name, dir),
            outcome: if undershoot { FieldLandingOutcome::Undershoot } else { FieldLandingOutcome::FullStop },
            touchdown_from_threshold_m: along,
            aim_error_m: err,
            centreline_m: across,
            touchdown_fpm: fpm,
            touchdown_gs_kts: util::gs_kts(v),
            gs_error_1nm_deg: one.map(|x| x.0),
            gs_error_half_nm_deg: half.map(|x| x.0),
            lineup_1nm_deg: one.map(|x| x.1),
            lineup_half_nm_deg: half.map(|x| x.1),
            stable,
            quality,
            calls,
            touchdown_pos: util::geo(lua, p),
            runway_heading_deg: dir,
            threshold_pos: util::geo(lua, thr),
            aim_point_m: aim,
            runway_length_m: rw.length,
        };
        self.pending.insert(
            f.unit_name.clone(),
            Pending {
                res,
                pilot: records::pilot_of(f),
                typ: f.typ.clone(),
                side: f.side,
                callsign: f.group_name.clone(),
                gid: f.group_id,
                due: now + 15.,
            },
        );
    }

    /// RUNWAY_TAKEOFF / TAKEOFF: a pending landing was a touch-and-go.
    pub fn took_off(&mut self, unit: &str) {
        if let Some(p) = self.pending.get_mut(unit) {
            if p.res.outcome == FieldLandingOutcome::FullStop {
                p.res.outcome = FieldLandingOutcome::TouchAndGo;
            }
            p.due = 0.;
        }
    }

    fn emit(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, p: Pending) {
        let r = &p.res;
        if cfg.in_game_results {
            records::to_group(
                lua,
                p.gid,
                &format!(
                    "LANDING {} rwy {} ({}): {:.0} m past the threshold ({:+.0} m to aim), {:.1} m {}, {:.0} fpm, {:.0} kts{} - {}{}",
                    r.airfield,
                    r.runway,
                    r.outcome.label(),
                    r.touchdown_from_threshold_m,
                    r.aim_error_m,
                    r.centreline_m.abs(),
                    if r.centreline_m >= 0. { "right" } else { "left" },
                    r.touchdown_fpm,
                    r.touchdown_gs_kts,
                    if r.stable { ", stable approach" } else { ", unstable approach" },
                    r.quality.label(),
                    if r.calls.is_empty() { String::new() } else { format!("\n{}", r.calls.join("; ")) }
                ),
                cfg.message_s,
            );
        }
        let score = r.quality.score();
        rec.emit(lua, p.pilot, &p.typ, p.side, &p.callsign, Some(score), RangeResult::FieldLanding(p.res), None);
    }

    pub fn player_out(&mut self, lua: MizLua, cfg: &RangeCfg, rec: &mut Recorder, unit: &str) {
        self.track.remove(unit);
        self.last_touch.remove(unit);
        if let Some(p) = self.pending.remove(unit) {
            self.emit(lua, cfg, rec, p);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::designator;

    #[test]
    fn runway_designators() {
        assert_eq!(designator("09-27", 95.), "09");
        assert_eq!(designator("09-27", 272.), "27");
        assert_eq!(designator("13L-31R", 128.), "13L");
        assert_eq!(designator("", 250.), "25");
    }
}
