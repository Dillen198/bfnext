// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Debrief clips: the few minutes of a range server's Tacview recording
//! around one graded result, cut down to what belongs in that debrief.
//!
//! The range site's 3D debrief plays a clip, and "Open in Tacview" downloads
//! it as a small `.acmi` instead of the whole session (hundreds of MB).
//!
//! The cut reads the recording itself ([`crate::replay::acmi`]) rather than
//! the replay index, because a pilot wants the debrief minutes after the
//! pass, while the server is still recording, and the index only covers
//! recordings that have closed. One pass, front to back:
//!
//! - every object's full state is kept from the start of the file (Tacview
//!   lines carry only what changed, so an object seen first long before the
//!   window still needs its type, name and last transform);
//! - inside the window every kept object is sampled (the pilot's aircraft
//!   and weapons at full rate, other aircraft twice a second, ships and
//!   ground units every few seconds), with a sample at each end;
//! - the read stops at the first frame past the window.
//!
//! Then what goes in: the pilot's aircraft, everything it launched, ships
//! within 20 km (the boat), aircraft that came within 15 km (tankers,
//! adversaries, wingmen) and what they fired that came near, and ground
//! units around the pilot's weapon impacts or under a low pass.
//!
//! Times in a clip are seconds relative to the result (0 = when the engine
//! graded it, negative = before), positions absolute WGS84, altitude metres
//! MSL, attitude degrees as Tacview has it (roll, pitch, yaw = true heading).

use crate::replay::{
    acmi::{self, Line},
    ingest::{classify, Kind},
};
use anyhow::{bail, Context, Result};
use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, Utc};
use fxhash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::{fmt::Write as _, path::Path};

/// Bump when the clip's shape or the selection changes: cached clips with
/// an older version are rebuilt.
pub(crate) const CLIP_VERSION: u32 = 1;

/// What to cut and around whom.
#[derive(Debug, Clone)]
pub(crate) struct ClipReq {
    pub(crate) pilot: String,
    /// DCS type name; breaks a tie when one name flew several aircraft
    pub(crate) unit_type: String,
    /// when the result was graded, wall clock
    pub(crate) ts_ms: i64,
    /// the engine's mission date and time ("2024/8/1", "18:48:38"), used when
    /// the recording has no `RecordingTime`
    pub(crate) mission_date: String,
    pub(crate) mission_time: String,
    /// seconds before / after the result
    pub(crate) lead_s: f64,
    pub(crate) tail_s: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct ClipObj {
    /// Tacview object id, hex
    pub(crate) id: String,
    /// the replay kind code: air, helo, missile, bomb, rocket, carrier, ship, sam, armor...
    pub(crate) kind: String,
    /// self, weapon (launched by self), threat (fired at self), boat, air, ground
    pub(crate) role: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) name: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) pilot: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) group: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) color: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) coalition: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) parent: Option<String>,
    pub(crate) t: Vec<f32>,
    pub(crate) lat: Vec<f64>,
    pub(crate) lon: Vec<f64>,
    pub(crate) alt: Vec<f32>,
    /// empty when the recording has no attitude for it
    #[serde(default)]
    pub(crate) roll: Vec<f32>,
    #[serde(default)]
    pub(crate) pitch: Vec<f32>,
    #[serde(default)]
    pub(crate) yaw: Vec<f32>,
    /// when it left the recording, if inside the window
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) gone: Option<f32>,
    #[serde(default)]
    pub(crate) destroyed: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Clip {
    pub(crate) v: u32,
    /// the recording's ReferenceTime (for the .acmi export)
    #[serde(default)]
    pub(crate) reference_time: Option<String>,
    /// recording seconds of the result (t = 0 in this clip)
    pub(crate) event_s: f64,
    /// the window, relative to the result
    pub(crate) t0: f32,
    pub(crate) t1: f32,
    /// false when the recording ended (or is still being written) before
    /// the window did: not cached, asked again later
    pub(crate) complete: bool,
    #[serde(default)]
    pub(crate) self_id: Option<String>,
    /// the recording's file name
    pub(crate) source: String,
    pub(crate) objects: Vec<ClipObj>,
}

// ─── the pass ──────────────────────────────────────────────────────────────

#[derive(Default)]
struct Obj {
    kind: Option<Kind>,
    /// Type= said this is clutter (bullets, flares...): ignore it
    skip: bool,
    name: Option<String>,
    pilot: Option<String>,
    group: Option<String>,
    color: Option<String>,
    coalition: Option<String>,
    parent: Option<u64>,
    // last transform, absolute degrees / metres
    lon: Option<f64>,
    lat: Option<f64>,
    alt: Option<f64>,
    roll: Option<f64>,
    pitch: Option<f64>,
    yaw: Option<f64>,
    has_att: bool,
    alive: bool,
    destroyed: bool,
    gone: Option<f64>,
    last_sample: f64,
    /// recording seconds, lat, lon, alt, roll, pitch, yaw
    samples: Vec<[f64; 7]>,
}

impl Obj {
    fn interval(&self, is_self: bool) -> f64 {
        if is_self {
            return 0.0;
        }
        match self.kind {
            Some(Kind::Missile | Kind::Bomb | Kind::Rocket | Kind::Torpedo) => 0.0,
            Some(Kind::Air | Kind::Helo) => 0.5,
            Some(Kind::Carrier | Kind::Ship) => 2.0,
            Some(Kind::Static) => f64::INFINITY,
            _ => 5.0,
        }
    }

    fn sample(&mut self, t: f64) {
        let (Some(lat), Some(lon)) = (self.lat, self.lon) else { return };
        if let Some(last) = self.samples.last() {
            if last[0] == t {
                self.samples.pop();
            }
        }
        self.samples.push([
            t,
            lat,
            lon,
            self.alt.unwrap_or(0.0),
            self.roll.unwrap_or(0.0),
            self.pitch.unwrap_or(0.0),
            self.yaw.unwrap_or(0.0),
        ]);
        self.last_sample = t;
    }
}

fn is_weapon(k: Option<Kind>) -> bool {
    matches!(k, Some(Kind::Missile | Kind::Bomb | Kind::Rocket | Kind::Torpedo))
}

fn is_air(k: Option<Kind>) -> bool {
    matches!(k, Some(Kind::Air | Kind::Helo))
}

fn is_ground(k: Option<Kind>) -> bool {
    matches!(k, Some(Kind::Sam | Kind::Armor | Kind::Vehicle | Kind::Infantry | Kind::Static))
}

/// Flat-earth distance in metres: fine over the tens of km a clip spans.
fn dist_m(a: (f64, f64), b: (f64, f64)) -> f64 {
    let lat = ((a.0 + b.0) / 2.0).to_radians();
    let dy = (a.0 - b.0) * 111_320.0;
    let dx = (a.1 - b.1) * 111_320.0 * lat.cos();
    (dx * dx + dy * dy).sqrt()
}

/// Recording seconds of the result: from the wall clock when the recording
/// says when it started (as the replay index does), else from the mission
/// clock against `ReferenceTime`.
fn event_seconds(req: &ClipReq, first_frame: f64, reference: Option<&str>, recording: Option<&str>) -> Option<f64> {
    if let Some(rec) = recording.and_then(|s| DateTime::parse_from_rfc3339(s).ok()) {
        return Some(first_frame + (req.ts_ms - rec.with_timezone(&Utc).timestamp_millis()) as f64 / 1000.0);
    }
    let reference = DateTime::parse_from_rfc3339(reference?).ok()?.with_timezone(&Utc);
    let mut d = req.mission_date.split(['/', '-']).map(|p| p.trim().parse::<u32>().ok());
    let (y, m, dd) = (d.next()??, d.next()??, d.next()??);
    let date = NaiveDate::from_ymd_opt(y as i32, m, dd)?;
    let time = NaiveTime::parse_from_str(req.mission_time.trim(), "%H:%M:%S").ok()?;
    let at = NaiveDateTime::new(date, time).and_utc();
    Some((at - reference).num_milliseconds() as f64 / 1000.0)
}

/// Cut the clip for `req` out of the recording at `path`.
pub(crate) fn build(path: &Path, req: &ClipReq) -> Result<Clip> {
    acmi::open(path, |rd| cut(rd, path, req))
}

fn cut<R: std::io::BufRead>(rd: &mut acmi::Reader<R>, path: &Path, req: &ClipReq) -> Result<Clip> {
    let mut objs: FxHashMap<u64, Obj> = FxHashMap::default();
    let mut dropped: FxHashSet<u64> = FxHashSet::default();
    let (mut ref_lon, mut ref_lat) = (0.0f64, 0.0f64);
    let mut reference_time: Option<String> = None;
    let mut recording_time: Option<String> = None;
    let mut first_frame: Option<f64> = None;
    let mut now = 0.0f64;
    // the window in recording seconds, known at the first frame
    let mut win: Option<(f64, f64, f64)> = None;
    let mut started = false;
    let mut complete = false;
    // who is "self" is decided while reading: the matching aircraft alive
    // nearest the event
    let pilot = req.pilot.trim().to_lowercase();

    while let Some(line) = rd.next_line()? {
        match line {
            Line::Frame(t) => {
                if first_frame.is_none() {
                    first_frame = Some(t);
                    let ev = event_seconds(req, t, reference_time.as_deref(), recording_time.as_deref())
                        .context("cannot place the result in the recording: it has neither RecordingTime nor ReferenceTime")?;
                    win = Some((ev, ev - req.lead_s, ev + req.tail_s));
                }
                if t > now {
                    now = t;
                }
                let (_, w0, w1) = win.unwrap();
                if !started && now >= w0 {
                    started = true;
                    for o in objs.values_mut().filter(|o| o.alive) {
                        o.sample(now);
                    }
                }
                if now > w1 {
                    for o in objs.values_mut().filter(|o| o.alive) {
                        o.sample(w1);
                    }
                    complete = true;
                    break;
                }
            }
            Line::Remove(id) => {
                if let Some(o) = objs.get_mut(&id) {
                    if started {
                        o.sample(now);
                        o.gone = Some(now);
                    }
                    o.alive = false;
                    if !started {
                        // gone before the window: nothing of it is wanted
                        objs.remove(&id);
                    }
                }
                dropped.remove(&id);
            }
            Line::Object(0, props) => {
                for (k, v) in props {
                    match k {
                        "ReferenceLongitude" => ref_lon = v.parse().unwrap_or(0.0),
                        "ReferenceLatitude" => ref_lat = v.parse().unwrap_or(0.0),
                        "ReferenceTime" => reference_time = Some(v.into_owned()),
                        "RecordingTime" => recording_time = Some(v.into_owned()),
                        "Event" if started => {
                            let mut parts = v.split('|');
                            if parts.next() == Some("Destroyed") {
                                if let Some(id) = parts.next().and_then(|s| u64::from_str_radix(s.trim(), 16).ok()) {
                                    if let Some(o) = objs.get_mut(&id) {
                                        o.destroyed = true;
                                    }
                                }
                            }
                        }
                        _ => (),
                    }
                }
            }
            Line::Object(id, props) => {
                if dropped.contains(&id) {
                    continue;
                }
                let o = objs.entry(id).or_insert_with(|| Obj { alive: true, last_sample: f64::NEG_INFINITY, ..Default::default() });
                if !o.alive {
                    // an id reused after a removal: start over
                    *o = Obj { alive: true, last_sample: f64::NEG_INFINITY, ..Default::default() };
                }
                let mut moved = false;
                for (k, v) in props {
                    match k {
                        "T" => {
                            let tf = acmi::transform(&v);
                            if let Some(x) = tf.lon {
                                o.lon = Some(ref_lon + x);
                            }
                            if let Some(x) = tf.lat {
                                o.lat = Some(ref_lat + x);
                            }
                            if let Some(x) = tf.alt {
                                o.alt = Some(x);
                            }
                            if tf.has_att {
                                o.has_att = true;
                                if let Some(x) = tf.roll {
                                    o.roll = Some(x);
                                }
                                if let Some(x) = tf.pitch {
                                    o.pitch = Some(x);
                                }
                                if let Some(x) = tf.yaw {
                                    o.yaw = Some(x);
                                }
                            }
                            moved = true;
                        }
                        "Type" => {
                            o.kind = classify(&v);
                            if o.kind.is_none() {
                                o.skip = true;
                            }
                        }
                        "Name" => o.name = Some(v.into_owned()),
                        "Pilot" => o.pilot = Some(v.into_owned()),
                        "Group" => o.group = Some(v.into_owned()),
                        "Color" => o.color = Some(v.into_owned()),
                        "Coalition" => o.coalition = Some(v.into_owned()),
                        "Parent" => o.parent = u64::from_str_radix(v.trim(), 16).ok(),
                        _ => (),
                    }
                }
                if o.skip {
                    objs.remove(&id);
                    dropped.insert(id);
                    continue;
                }
                if started && moved {
                    let is_self = is_air(o.kind) && o.pilot.as_deref().map_or(false, |p| p.trim().to_lowercase() == pilot);
                    if now - o.last_sample >= o.interval(is_self) {
                        o.sample(now);
                    }
                }
            }
        }
    }

    let Some((ev, w0, w1)) = win else { bail!("the recording has no frames") };
    if !started {
        bail!("the recording ends before this result: it is not in this file");
    }

    // ── who is in the debrief ──
    let candidates: Vec<(&u64, &Obj)> = objs
        .iter()
        .filter(|(_, o)| is_air(o.kind) && !o.samples.is_empty())
        .filter(|(_, o)| o.pilot.as_deref().map_or(false, |p| p.trim().to_lowercase() == pilot))
        .collect();
    let near_event = |o: &Obj| o.samples.iter().map(|s| (s[0] - ev).abs()).fold(f64::INFINITY, f64::min);
    let self_id = candidates
        .iter()
        .min_by(|a, b| {
            let ta = a.1.name.as_deref().map_or(true, |n| !n.eq_ignore_ascii_case(&req.unit_type));
            let tb = b.1.name.as_deref().map_or(true, |n| !n.eq_ignore_ascii_case(&req.unit_type));
            ta.cmp(&tb).then(near_event(a.1).total_cmp(&near_event(b.1)))
        })
        .map(|(id, _)| **id);
    let Some(me) = self_id else {
        bail!("{} is not in the recording around this result", req.pilot);
    };
    let my_track: Vec<(f64, f64, f64)> = objs[&me].samples.iter().map(|s| (s[1], s[2], s[3])).collect();
    let min_to_me = |o: &Obj| {
        let mut best = f64::INFINITY;
        // every few samples is plenty to find a closest approach of km
        for s in o.samples.iter().step_by(2) {
            for m in my_track.iter().step_by(4) {
                best = best.min(dist_m((s[1], s[2]), (m.0, m.1)));
            }
        }
        best
    };
    let mut keep: FxHashMap<u64, &'static str> = FxHashMap::default();
    keep.insert(me, "self");
    // what self launched, then what launched at the aircraft near it
    let mut impacts: Vec<(f64, f64)> = vec![];
    for (id, o) in objs.iter() {
        if is_weapon(o.kind) && o.parent == Some(me) && !o.samples.is_empty() {
            keep.insert(*id, "weapon");
            let s = o.samples.last().unwrap();
            impacts.push((s[1], s[2]));
        }
    }
    let mut air: FxHashSet<u64> = FxHashSet::default();
    for (id, o) in objs.iter() {
        if keep.contains_key(id) || o.samples.is_empty() {
            continue;
        }
        let d = min_to_me(o);
        match o.kind {
            Some(Kind::Carrier | Kind::Ship) if d < 20_000.0 => {
                keep.insert(*id, "boat");
            }
            k if is_air(k) && d < 15_000.0 => {
                keep.insert(*id, "air");
                air.insert(*id);
            }
            k if is_weapon(k) && d < 5_000.0 => {
                keep.insert(*id, "threat");
            }
            _ => (),
        }
    }
    // weapons the nearby aircraft fired (missile defence, BFM shots)
    for (id, o) in objs.iter() {
        if !keep.contains_key(id) && is_weapon(o.kind) && o.parent.map_or(false, |p| air.contains(&p)) && !o.samples.is_empty() {
            keep.insert(*id, "threat");
        }
    }
    // ground units: around the impacts, or under the pilot when low
    let low: Vec<(f64, f64)> = my_track.iter().filter(|m| m.2 < 2_500.0).step_by(4).map(|m| (m.0, m.1)).collect();
    let mut ground: Vec<(f64, u64)> = vec![];
    for (id, o) in objs.iter() {
        if keep.contains_key(id) || !is_ground(o.kind) || o.samples.is_empty() {
            continue;
        }
        let p = (o.samples[0][1], o.samples[0][2]);
        let d_imp = impacts.iter().map(|i| dist_m(p, *i)).fold(f64::INFINITY, f64::min);
        let d_low = low.iter().map(|m| dist_m(p, *m)).fold(f64::INFINITY, f64::min);
        let d = d_imp.min(d_low * 2.0);
        if d_imp < 3_000.0 || d_low < 4_000.0 {
            ground.push((d, *id));
        }
    }
    ground.sort_by(|a, b| a.0.total_cmp(&b.0));
    for (_, id) in ground.into_iter().take(250) {
        keep.insert(id, "ground");
    }

    let hex = |id: u64| format!("{id:x}");
    let r6 = |v: f64| (v * 1e6).round() / 1e6;
    let mut objects: Vec<ClipObj> = keep
        .iter()
        .map(|(id, role)| {
            let o = &objs[id];
            let att = o.has_att;
            ClipObj {
                id: hex(*id),
                kind: o.kind.map(Kind::code).unwrap_or("static").to_string(),
                role: (*role).to_string(),
                name: o.name.clone(),
                pilot: o.pilot.clone(),
                group: o.group.clone(),
                color: o.color.clone(),
                coalition: o.coalition.clone(),
                parent: o.parent.map(hex),
                t: o.samples.iter().map(|s| (s[0] - ev) as f32).collect(),
                lat: o.samples.iter().map(|s| r6(s[1])).collect(),
                lon: o.samples.iter().map(|s| r6(s[2])).collect(),
                alt: o.samples.iter().map(|s| s[3] as f32).collect(),
                roll: if att { o.samples.iter().map(|s| s[4] as f32).collect() } else { vec![] },
                pitch: if att { o.samples.iter().map(|s| s[5] as f32).collect() } else { vec![] },
                yaw: if att { o.samples.iter().map(|s| s[6] as f32).collect() } else { vec![] },
                gone: o.gone.map(|g| (g - ev) as f32),
                destroyed: o.destroyed,
            }
        })
        .collect();
    // self first, then by role, for a stable file
    let rank = |r: &str| ["self", "weapon", "boat", "air", "threat", "ground"].iter().position(|x| *x == r).unwrap_or(9);
    objects.sort_by(|a, b| rank(&a.role).cmp(&rank(&b.role)).then(a.id.cmp(&b.id)));

    Ok(Clip {
        v: CLIP_VERSION,
        reference_time,
        event_s: ev,
        t0: (w0 - ev) as f32,
        t1: (w1 - ev) as f32,
        complete,
        self_id: Some(hex(me)),
        source: path.file_name().and_then(|n| n.to_str()).unwrap_or("").to_string(),
        objects,
    })
}

/// Delete cached clips older than `max_age`; how many went.
pub(crate) fn prune(dir: &Path, max_age: std::time::Duration) -> usize {
    let Ok(rd) = std::fs::read_dir(dir) else { return 0 };
    let now = std::time::SystemTime::now();
    let mut n = 0;
    for e in rd.flatten() {
        let old = e
            .metadata()
            .and_then(|m| m.modified())
            .map(|m| now.duration_since(m).unwrap_or_default() >= max_age)
            .unwrap_or(false);
        if old && e.path().extension().map_or(false, |x| x == "gz") && std::fs::remove_file(e.path()).is_ok() {
            n += 1;
        }
    }
    n
}

// ─── .acmi export ──────────────────────────────────────────────────────────

fn esc(s: &str) -> String {
    s.replace('\\', "\\\\").replace(',', "\\,").replace('\n', "\\\n")
}

/// The clip as a Tacview text recording, times as in the source recording
/// (so Tacview shows the mission clock), positions against a reference
/// point at the pilot's first sample.
pub(crate) fn to_acmi(c: &Clip, title: &str) -> String {
    let (rlat, rlon) = c
        .objects
        .iter()
        .find(|o| o.role == "self")
        .and_then(|o| Some((*o.lat.first()?, *o.lon.first()?)))
        .map(|(a, b)| (a.floor(), b.floor()))
        .unwrap_or((0.0, 0.0));
    let mut out = String::with_capacity(1 << 20);
    out.push_str("FileType=text/acmi/tacview\nFileVersion=2.2\n");
    let _ = writeln!(out, "0,ReferenceTime={}", c.reference_time.as_deref().unwrap_or("2000-01-01T00:00:00Z"));
    let _ = writeln!(out, "0,ReferenceLongitude={rlon}");
    let _ = writeln!(out, "0,ReferenceLatitude={rlat}");
    let _ = writeln!(out, "0,Title={}", esc(title));
    out.push_str("0,DataSource=Vector Strike Range debrief clip\n");

    // every (time, object, sample index), in time order
    let mut ev: Vec<(f32, usize, usize)> = vec![];
    for (oi, o) in c.objects.iter().enumerate() {
        for (si, t) in o.t.iter().enumerate() {
            ev.push((*t, oi, si));
        }
    }
    ev.sort_by(|a, b| a.0.total_cmp(&b.0).then(a.1.cmp(&b.1)));
    let mut seen = vec![false; c.objects.len()];
    let mut frame: Option<f32> = None;
    let mut removed = vec![false; c.objects.len()];
    let flush_removals = |out: &mut String, upto: f32, removed: &mut Vec<bool>| {
        for (oi, o) in c.objects.iter().enumerate() {
            if let Some(g) = o.gone {
                if !removed[oi] && g < upto {
                    if o.destroyed {
                        let _ = writeln!(out, "0,Event=Destroyed|{}|", o.id);
                    }
                    let _ = writeln!(out, "-{}", o.id);
                    removed[oi] = true;
                }
            }
        }
    };
    for (t, oi, si) in ev {
        if frame != Some(t) {
            flush_removals(&mut out, t, &mut removed);
            let _ = writeln!(out, "#{:.2}", c.event_s + t as f64);
            frame = Some(t);
        }
        let o = &c.objects[oi];
        let _ = write!(out, "{},T={:.6}|{:.6}|{:.1}", o.id, o.lon[si] - rlon, o.lat[si] - rlat, o.alt[si]);
        if !o.roll.is_empty() {
            let _ = write!(out, "|{:.1}|{:.1}|{:.1}", o.roll[si], o.pitch[si], o.yaw[si]);
        }
        if !seen[oi] {
            seen[oi] = true;
            let ty = match o.kind.as_str() {
                "air" => "Air+FixedWing",
                "helo" => "Air+Rotorcraft",
                "missile" => "Weapon+Missile",
                "bomb" => "Weapon+Bomb",
                "rocket" => "Weapon+Rocket",
                "torpedo" => "Weapon+Torpedo",
                "carrier" => "Sea+Watercraft+AircraftCarrier",
                "ship" => "Sea+Watercraft",
                "sam" => "Ground+AntiAircraft",
                "armor" => "Ground+Heavy+Armor+Vehicle+Tank",
                "infantry" => "Ground+Light+Human+Infantry",
                "static" => "Ground+Static+Building",
                _ => "Ground+Vehicle",
            };
            let _ = write!(out, ",Type={ty}");
            for (k, v) in [("Name", &o.name), ("Pilot", &o.pilot), ("Group", &o.group), ("Color", &o.color), ("Coalition", &o.coalition)] {
                if let Some(v) = v {
                    let _ = write!(out, ",{k}={}", esc(v));
                }
            }
            if let Some(p) = &o.parent {
                let _ = write!(out, ",Parent={p}");
            }
        }
        out.push('\n');
    }
    flush_removals(&mut out, f32::INFINITY, &mut removed);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn req() -> ClipReq {
        ClipReq {
            pilot: "Casper".into(),
            unit_type: "FA-18C_hornet".into(),
            // RecordingTime 12:00:00 + 100 s
            ts_ms: DateTime::parse_from_rfc3339("2026-10-08T12:01:40Z").unwrap().timestamp_millis(),
            mission_date: "2026/10/8".into(),
            mission_time: "10:01:40".into(),
            lead_s: 30.0,
            tail_s: 10.0,
        }
    }

    fn recording() -> String {
        let mut s = String::from(
            "FileType=text/acmi/tacview\nFileVersion=2.2\n\
             0,ReferenceTime=2026-10-08T10:00:00Z,RecordingTime=2026-10-08T12:00:00Z,\
             ReferenceLongitude=41,ReferenceLatitude=42\n#0\n\
             a1,T=0.10|0.10|500|0|0|90,Type=Air+FixedWing,Name=FA-18C_hornet,Pilot=Casper,Color=Blue\n\
             b2,T=0.12|0.10|20|0|0|90,Type=Sea+Watercraft+AircraftCarrier,Name=CVN_73\n\
             c3,T=0.90|0.90|8000,Type=Air+FixedWing,Name=KC-135,Pilot=Texaco\n\
             d4,T=0.11|0.11|0,Type=Ground+Heavy+Armor+Vehicle+Tank,Name=T-72B\n\
             e5,T=0.10|0.10|0,Type=Projectile+Shell\n",
        );
        for i in 1..=150 {
            let x = 0.10 + i as f64 * 0.0001;
            s.push_str(&format!("#{i}\na1,T={x:.4}|0.10|{}|{}||\n", 500 - i, i % 10));
            if i == 85 {
                s.push_str("f6,T=0.1085|0.10|400,Type=Weapon+Bomb,Name=Mk-82,Parent=a1\n");
            }
            if (86..95).contains(&i) {
                s.push_str(&format!("f6,T=0.11|0.105|{}\n", 400 - (i - 85) * 40));
            }
            if i == 95 {
                s.push_str("0,Event=Destroyed|d4|\n-f6\n");
            }
        }
        s
    }

    fn cut_text(text: &str) -> Clip {
        let dir = std::env::temp_dir().join(format!("bfdb-clip-{}-{}", std::process::id(), text.len()));
        std::fs::create_dir_all(&dir).unwrap();
        let p = dir.join("t.txt.acmi");
        std::fs::write(&p, text).unwrap();
        let c = build(&p, &req()).unwrap();
        let _ = std::fs::remove_dir_all(&dir);
        c
    }

    #[test]
    fn cuts_the_window_around_the_result() {
        let c = cut_text(&recording());
        assert!(c.complete);
        assert_eq!(c.event_s, 100.0);
        assert_eq!(c.self_id.as_deref(), Some("a1"));
        let me = &c.objects[0];
        assert_eq!(me.role, "self");
        assert!(*me.t.first().unwrap() >= -30.0 && *me.t.last().unwrap() <= 10.0);
        // full rate for self: one sample per frame in the window
        assert!(me.t.len() >= 40, "{}", me.t.len());
        // attitude kept, carried over from earlier lines
        assert_eq!(me.yaw[0], 90.0);
        let roles: FxHashMap<&str, &str> = c.objects.iter().map(|o| (o.id.as_str(), o.role.as_str())).collect();
        assert_eq!(roles.get("b2"), Some(&"boat"));
        assert_eq!(roles.get("f6"), Some(&"weapon"));
        assert_eq!(roles.get("d4"), Some(&"ground"));
        // the tanker is ~110 km away, the shell is clutter
        assert!(!roles.contains_key("c3"));
        assert!(!roles.contains_key("e5"));
        let bomb = c.objects.iter().find(|o| o.id == "f6").unwrap();
        assert_eq!(bomb.gone, Some(-5.0));
        assert!(c.objects.iter().find(|o| o.id == "d4").unwrap().destroyed);
    }

    #[test]
    fn an_unfinished_recording_is_not_complete() {
        let text = recording();
        let cut_at = text.find("#105\n").unwrap();
        let c = cut_text(&text[..cut_at]);
        assert!(!c.complete);
    }

    #[test]
    fn the_export_reads_back_as_acmi() {
        let c = cut_text(&recording());
        let a = to_acmi(&c, "Casper, bomb");
        let mut rd = acmi::Reader::new(std::io::BufReader::new(a.as_bytes())).unwrap();
        let (mut frames, mut objs) = (0, 0);
        while let Some(l) = rd.next_line().unwrap() {
            match l {
                Line::Frame(_) => frames += 1,
                Line::Object(id, _) if id != 0 => objs += 1,
                _ => (),
            }
        }
        assert!(frames >= 40);
        assert!(objs > frames);
        assert!(a.contains("Pilot=Casper"));
        assert!(a.contains("-f6\n"));
    }

    #[test]
    fn prune_keeps_young_clips() {
        let dir = std::env::temp_dir().join(format!("bfdb-clip-prune-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("a.json.gz"), b"x").unwrap();
        std::fs::write(dir.join("notes.txt"), b"x").unwrap();
        assert_eq!(prune(&dir, std::time::Duration::from_secs(3600)), 0);
        assert_eq!(prune(&dir, std::time::Duration::ZERO), 1);
        assert!(dir.join("notes.txt").exists());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn falls_back_to_the_mission_clock() {
        let r = req();
        assert_eq!(event_seconds(&r, 0.0, Some("2026-10-08T10:00:00Z"), None), Some(100.0));
    }
}
