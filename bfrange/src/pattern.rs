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
//! a touch-and-go; the in-game grade waits that long.
//!
//! The pattern into the landing is read from the last six minutes of track
//! ([`bfprotocols::range::pattern_legs::pattern`]: initial, break, downwind, base, final).
//! After a full stop the engine keeps following the aircraft through the
//! rollout and the taxi ([`bfprotocols::range::pattern_legs::taxi`]) until it parks, its
//! pilot leaves, it takes off again or ten minutes pass; then the record is
//! written, with the pattern and the taxi as its track.

use crate::{
    players::{Flying, Players},
    records::{self, Recorder},
    util::{self, V3},
};
use bfprotocols::range::{
    FieldLandingOutcome, FieldLandingResult, PilotRef, RangeResult, Track, TrackPt,
    cfg::RangeCfg,
    grading,
    pattern_legs::{self, RelSample, TaxiSample},
};
use dcso3::{
    MizLua, airbase::AirbaseCategory, coalition::Side, env::miz::GroupId, world::World,
};
use fxhash::FxHashMap;
use log::{info, warn};
use std::collections::BTreeMap;
use std::collections::VecDeque;

/// Sample aircraft this close to a field, and this low.
const NEAR_M: f64 = 8. * util::NM;
const LOW_M: f64 = 1200.;
/// How much track to keep: enough for an overhead pattern and a long final.
const KEEP_S: f64 = 360.;
/// Give up following a taxi after this long.
const TAXI_MAX_S: f64 = 600.;
/// Stopped this long, it has parked.
const PARKED_S: f64 = 25.;

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

/// The landing runway, for the runway frame: threshold, direction, size.
#[derive(Debug, Clone, Copy)]
struct Frame {
    thr: V3,
    s: f64,
    c: f64,
    len: f64,
    width: f64,
}

impl Frame {
    /// (along from the threshold, across + right)
    fn rel(&self, q: V3) -> (f64, f64) {
        let d = q - self.thr;
        (d.x * self.c + d.z * self.s, -d.x * self.s + d.z * self.c)
    }
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
    frame: Frame,
    t_touch: f64,
    /// the pattern into the landing, 1 Hz, for the record's track
    pattern_pts: Vec<TrackPt>,
}

/// A full stop being followed through the rollout and the taxi.
#[derive(Debug)]
struct Taxiing {
    p: Pending,
    samples: Vec<TaxiSample>,
    pts: Vec<TrackPt>,
    last: f64,
    still_since: Option<f64>,
}

#[derive(Debug, Default)]
pub struct Pattern {
    fields: Vec<Field>,
    track: FxHashMap<String, VecDeque<Sample>>,
    pending: FxHashMap<String, Pending>,
    taxiing: FxHashMap<String, Taxiing>,
    /// unit -> last touchdown time (a bounce is not a second landing)
    last_touch: FxHashMap<String, f64>,
    last_sample: f64,
}

/// The runway end number closest to a true landing heading, from DCS's
/// "09-27" style name.
pub(crate) fn designator(name: &str, hdg: f64) -> String {
    let parts: Vec<&str> =
        name.split(['-', '/', ' ']).map(|s| s.trim()).filter(|s| !s.is_empty()).collect();
    let num = |s: &str| {
        s.chars()
            .take_while(|c| c.is_ascii_digit())
            .collect::<String>()
            .parse::<f64>()
            .ok()
    };
    parts
        .iter()
        .filter_map(|p| {
            num(p).map(|n| (util::angdiff(n * 10., hdg).abs(), p.to_string()))
        })
        .min_by(|a, b| a.0.total_cmp(&b.0))
        .filter(|(d, _)| *d < 40.)
        .map(|(_, p)| p)
        .unwrap_or_else(|| {
            format!("{:02}", ((hdg / 10.).round() as i64 - 1).rem_euclid(36) + 1)
        })
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
                if !cfg.pattern.fields.is_empty()
                    && !cfg.pattern.fields.iter().any(|f| f.eq_ignore_ascii_case(&name))
                {
                    continue;
                }
                let mut rwys = vec![];
                for rw in ab.get_runways()? {
                    let Ok(rw) = rw else { continue };
                    let (Ok(c), Ok(course), Ok(len), Ok(w)) =
                        (rw.position(), rw.course(), rw.length(), rw.width())
                    else {
                        continue;
                    };
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
    pub fn tick(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        players: &Players,
        rec: &mut Recorder,
        now: f64,
    ) {
        // landings whose touch-and-go window has closed: the grade goes out
        // now; a full stop is then followed through the taxi
        let due: Vec<String> = self
            .pending
            .iter()
            .filter(|(_, p)| now >= p.due)
            .map(|(u, _)| u.clone())
            .collect();
        for u in due {
            if let Some(p) = self.pending.remove(&u) {
                self.message(lua, cfg, &p);
                if p.res.outcome == FieldLandingOutcome::TouchAndGo
                    || !players.flying.contains_key(&u)
                {
                    self.record(lua, rec, p, vec![], None);
                } else {
                    self.taxiing.insert(
                        u,
                        Taxiing {
                            p,
                            samples: vec![],
                            pts: vec![],
                            last: f64::NEG_INFINITY,
                            still_since: None,
                        },
                    );
                }
            }
        }
        self.follow_taxi(lua, players, rec, now);
        if self.fields.is_empty() || now - self.last_sample < 0.2 {
            return;
        }
        self.last_sample = now;
        self.track.retain(|u, _| players.flying.contains_key(u));
        for f in players.flying.values().filter(|f| !f.is_helo && !f.is_ground) {
            let near = self.fields.iter().any(|fl| util::dist2(fl.pos, f.pos) < NEAR_M)
                && f.alt_agl < LOW_M;
            if !near {
                self.track.remove(&f.unit_name);
                continue;
            }
            let (p, v) = match dcso3::unit::Unit::get_by_name(lua, &f.unit_name)
                .and_then(|u| Ok((u.get_point()?.0, u.get_velocity()?.0)))
            {
                Ok(x) => x,
                Err(_) => (f.pos, f.vel),
            };
            let q = self.track.entry(f.unit_name.clone()).or_default();
            q.push_back(Sample { t: now, p, v });
            while q.front().map(|s| now - s.t > KEEP_S).unwrap_or(false) {
                q.pop_front();
            }
        }
    }

    pub fn active(&self) -> bool {
        !self.track.is_empty() || !self.pending.is_empty() || !self.taxiing.is_empty()
    }

    /// One sample a second of every aircraft being followed after a full
    /// stop, and the end of its taxi.
    fn follow_taxi(
        &mut self,
        lua: MizLua,
        players: &Players,
        rec: &mut Recorder,
        now: f64,
    ) {
        let mut done: Vec<(String, &'static str)> = vec![];
        for (u, tx) in self.taxiing.iter_mut() {
            if now - tx.last < 1. {
                continue;
            }
            tx.last = now;
            let Some(f) = players.flying.get(u) else {
                done.push((u.clone(), "left"));
                continue;
            };
            let (p, v) = match dcso3::unit::Unit::get_by_name(lua, u)
                .and_then(|un| Ok((un.get_point()?.0, un.get_velocity()?.0)))
            {
                Ok(x) => x,
                Err(_) => (f.pos, f.vel),
            };
            let (along, across) = tx.p.frame.rel(p);
            let gs = util::gs_kts(v);
            tx.samples.push(TaxiSample {
                t: now,
                along,
                across,
                gs_kts: gs,
                x: p.x,
                z: p.z,
            });
            let g = util::geo(lua, p);
            tx.pts.push(TrackPt {
                t: now - tx.p.t_touch,
                lat: g.lat,
                lon: g.lon,
                alt_m: g.alt_m,
                speed_kts: gs,
            });
            let on_rwy = across.abs() <= tx.p.frame.width / 2. + 4.
                && (-30. ..=tx.p.frame.len + 30.).contains(&along);
            if gs < 1. {
                let since = *tx.still_since.get_or_insert(now);
                if now - since >= PARKED_S {
                    done.push((
                        u.clone(),
                        if on_rwy { "stopped_on_runway" } else { "parked" },
                    ));
                    continue;
                }
            } else {
                tx.still_since = None;
            }
            if now - tx.p.t_touch > TAXI_MAX_S {
                done.push((u.clone(), "timeout"));
            }
        }
        for (u, end) in done {
            self.finish_taxi(lua, rec, &u, end);
        }
    }

    fn finish_taxi(&mut self, lua: MizLua, rec: &mut Recorder, unit: &str, end: &str) {
        let Some(tx) = self.taxiing.remove(unit) else { return };
        let mut p = tx.p;
        // the touchdown itself opens the taxi track
        let mut samples = tx.samples;
        if samples.is_empty() || samples[0].t > p.t_touch + 0.5 {
            let (a, c) = (p.res.touchdown_from_threshold_m, p.res.centreline_m);
            samples.insert(
                0,
                TaxiSample {
                    t: p.t_touch,
                    along: a,
                    across: c,
                    gs_kts: p.res.touchdown_gs_kts,
                    x: f64::NAN,
                    z: f64::NAN,
                },
            );
            if samples.len() > 1 {
                samples[0].x = samples[1].x;
                samples[0].z = samples[1].z;
            }
        }
        let t = pattern_legs::taxi(&samples, p.t_touch, p.frame.len, p.frame.width, end);
        p.res.taxi = Some(t);
        self.record(lua, rec, p, tx.pts, None);
    }

    /// RUNWAY_TOUCH at an airfield.
    pub fn touch(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        f: &Flying,
        place: &str,
        now: f64,
    ) {
        if f.is_helo || f.is_ground {
            return;
        }
        let Some(field) = self.fields.iter().find(|fl| fl.name == place) else { return };
        if self
            .last_touch
            .insert(f.unit_name.clone(), now)
            .map(|t| now - t < 20.)
            .unwrap_or(false)
        {
            // a bounce: note it on the card already waiting
            if let Some(p) = self.pending.get_mut(&f.unit_name)
                && !p.res.calls.iter().any(|c| c == "bounced")
            {
                p.res.calls.push("bounced".into());
            }
            return;
        }
        let samples: Vec<Sample> = self
            .track
            .get(&f.unit_name)
            .map(|q| q.iter().copied().collect())
            .unwrap_or_default();
        let (p, v) = match dcso3::unit::Unit::get_by_name(lua, &f.unit_name)
            .and_then(|u| Ok((u.get_point()?.0, u.get_velocity()?.0)))
        {
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
                    if along.abs() <= rw.length / 2. + 400.
                        && across.abs() <= rw.width / 2. + 120.
                        && off < 30.
                        && best.map(|b| off < b.0).unwrap_or(true)
                    {
                        best = Some((off, rw, dir));
                    }
                }
            }
        }
        let Some((_, rw, dir)) = best else {
            info!(
                "pattern: {} touched down at {place} but not on a runway we know",
                f.name
            );
            return;
        };
        let (s, c) = dir.to_radians().sin_cos();
        let thr = V3::new(
            rw.centre.x - c * rw.length / 2.,
            rw.centre.y,
            rw.centre.z - s * rw.length / 2.,
        );
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
                    let gs = ((x.p.y - elev) / r).atan().to_degrees()
                        - cfg.pattern.glideslope_deg;
                    let lu = (cr / r).atan().to_degrees();
                    (gs, lu, -x.v.y * 60. * util::M_TO_FT)
                })
        };
        let one = at(util::NM);
        let half = at(util::NM / 2.);
        let stable = match (one, half) {
            (Some(a), Some(b)) => [a, b]
                .iter()
                .all(|(gs, lu, vs)| gs.abs() < 1. && lu.abs() < 3. && *vs < 1200.),
            _ => false,
        };
        let undershoot = along < 0.;
        let mut calls = vec![];
        let err = along - aim;
        if err.abs() > 150. {
            calls.push(format!(
                "{:.0} m {}",
                err.abs(),
                if err > 0. { "long" } else { "short" }
            ));
        }
        if across.abs() > 5. {
            calls.push(format!(
                "{:.0} m {} of centreline",
                across.abs(),
                if across > 0. { "right" } else { "left" }
            ));
        }
        if fpm > 600. {
            calls.push(format!("hard landing, {fpm:.0} fpm"));
        }
        match one {
            Some((gs, lu, _)) => {
                if gs.abs() >= 1. {
                    calls.push(format!(
                        "{} at 1 nm ({gs:+.1}°)",
                        if gs > 0. { "high" } else { "low" }
                    ));
                }
                if lu.abs() >= 3. {
                    calls.push(format!(
                        "lined up {} at 1 nm",
                        if lu > 0. { "right" } else { "left" }
                    ));
                }
            }
            None => calls.push("no straight-in final (no 1 nm sample)".into()),
        }
        if undershoot {
            calls.push("touched down short of the threshold".into());
        }
        let quality =
            grading::field_landing_quality(err, across, fpm, stable, undershoot);
        // the pattern, from the track in the runway frame
        let frame = Frame { thr, s, c, len: rw.length, width: rw.width };
        let rel: Vec<RelSample> = samples
            .iter()
            .map(|x| {
                let (a, cr) = frame.rel(x.p);
                RelSample {
                    t: x.t,
                    along: a,
                    across: cr,
                    agl: x.p.y - elev,
                    rel: util::angdiff(util::hdg(x.v), dir),
                    gs_kts: util::gs_kts(x.v),
                }
            })
            .collect();
        let pattern = pattern_legs::pattern(&rel, now);
        let mut pattern_pts = vec![];
        let mut last = f64::NEG_INFINITY;
        for x in &samples {
            if x.t - last >= 1. {
                last = x.t;
                let g = util::geo(lua, x.p);
                pattern_pts.push(TrackPt {
                    t: x.t - now,
                    lat: g.lat,
                    lon: g.lon,
                    alt_m: g.alt_m,
                    speed_kts: util::gs_kts(x.v),
                });
            }
        }
        let res = FieldLandingResult {
            airfield: field.name.clone(),
            runway: designator(&rw.name, dir),
            outcome: if undershoot {
                FieldLandingOutcome::Undershoot
            } else {
                FieldLandingOutcome::FullStop
            },
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
            runway_width_m: rw.width,
            pattern: Some(pattern),
            taxi: None,
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
                frame,
                t_touch: now,
                pattern_pts,
            },
        );
    }

    /// RUNWAY_TAKEOFF / TAKEOFF: a pending landing was a touch-and-go; a taxi
    /// being followed has ended in a take-off.
    pub fn took_off(&mut self, lua: MizLua, rec: &mut Recorder, unit: &str) {
        if let Some(p) = self.pending.get_mut(unit) {
            if p.res.outcome == FieldLandingOutcome::FullStop {
                p.res.outcome = FieldLandingOutcome::TouchAndGo;
            }
            p.due = 0.;
        }
        if self.taxiing.contains_key(unit) {
            self.finish_taxi(lua, rec, unit, "took_off");
        }
    }

    /// The grade, in game, to the pilot's group.
    fn message(&self, lua: MizLua, cfg: &RangeCfg, p: &Pending) {
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
                    if r.calls.is_empty() {
                        String::new()
                    } else {
                        format!("\n{}", r.calls.join("; "))
                    }
                ),
                cfg.message_s,
            );
            if let Some(pt) = &r.pattern
                && !pt.calls.is_empty()
            {
                records::to_group(
                    lua,
                    p.gid,
                    &format!(
                        "PATTERN ({}): {}",
                        pt.kind.replace('_', " "),
                        pt.calls.join("; ")
                    ),
                    cfg.message_s,
                );
            }
        }
    }

    /// The record, with the pattern and the taxi as its track.
    fn record(
        &self,
        lua: MizLua,
        rec: &mut Recorder,
        p: Pending,
        taxi_pts: Vec<TrackPt>,
        _why: Option<&str>,
    ) {
        let score = p.res.quality.score();
        let mut paths = BTreeMap::new();
        if !p.pattern_pts.is_empty() {
            paths.insert("pattern".to_string(), p.pattern_pts.clone());
        }
        if !taxi_pts.is_empty() {
            paths.insert("taxi".to_string(), taxi_pts);
        }
        let track = (!paths.is_empty()).then_some(Track::Path { paths });
        rec.emit(
            lua,
            p.pilot,
            &p.typ,
            p.side,
            &p.callsign,
            Some(score),
            RangeResult::FieldLanding(p.res),
            track,
        );
    }

    pub fn player_out(
        &mut self,
        lua: MizLua,
        cfg: &RangeCfg,
        rec: &mut Recorder,
        unit: &str,
    ) {
        self.track.remove(unit);
        self.last_touch.remove(unit);
        if let Some(p) = self.pending.remove(unit) {
            self.message(lua, cfg, &p);
            self.record(lua, rec, p, vec![], None);
        }
        if let Some(tx) = self.taxiing.get(unit) {
            // stopped clear of the runway before leaving: that was parking
            let parked = tx.still_since.map(|s| tx.last - s >= 5.).unwrap_or(false);
            self.finish_taxi(lua, rec, unit, if parked { "parked" } else { "left" });
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
