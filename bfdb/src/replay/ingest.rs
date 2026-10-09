// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Turn one Tacview recording into replay data the dashboard can stream.
//!
//! A recording is read once, front to back, and written out as:
//!
//! - `meta.json.gz`: the recording's globals, every kept object (type, pilot,
//!   coalition, lifetime, launcher) and the derived events (shots, kills,
//!   takeoffs, landings);
//! - `c<n>.bin.gz`: the tracks of everything alive in the n-th five-minute
//!   window, so a viewer loads only the part of the war it is looking at.
//!
//! Tracks are thinned per kind (aircraft and weapons several times a second,
//! ground units every few seconds, static objects once) and the newest
//! unwritten state is always flushed when an object leaves or a window
//! closes, so a missile's last point is where it actually ended. Shells,
//! bullets, flares, chaff and other clutter are dropped at the door.
//!
//! Chunk layout, little-endian, gzip'd:
//!
//! ```text
//! "BFR1" u32:nobj
//! per object: u32:idx u32:n u8:flags
//!   then columns of n i32 each, delta-coded (first absolute):
//!   t (ms from recording start), lon (1e-7 deg), lat (1e-7 deg), alt (dm MSL)
//!   [flags&1  ATT]  roll, pitch, yaw (centidegrees)
//!   [flags&2  IAS]  cm/s
//!   [flags&4  MACH] x1000
//!   [flags&8  AOA]  centidegrees
//!   [flags&16 AGL]  dm
//! ```
//!
//! Each chunk also repeats an object's last sample from before the window,
//! so any one chunk interpolates on its own.

use super::acmi::{self, Line};
use anyhow::{bail, Context, Result};
use chrono::{DateTime, Utc};
use flate2::{write::GzEncoder, Compression};
use fxhash::FxHashMap;
use serde::Serialize;
use serde_json::{json, Value};
use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
};

/// Length of one track chunk.
pub(crate) const CHUNK_MS: i64 = 300_000;

pub(crate) const F_ATT: u8 = 1;
pub(crate) const F_IAS: u8 = 2;
pub(crate) const F_MACH: u8 = 4;
pub(crate) const F_AOA: u8 = 8;
pub(crate) const F_AGL: u8 = 16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Kind {
    Air,
    Helo,
    Missile,
    Bomb,
    Rocket,
    Torpedo,
    Sam,
    Armor,
    Vehicle,
    Infantry,
    Static,
    Ship,
    Carrier,
}

impl Kind {
    pub(crate) fn code(self) -> &'static str {
        match self {
            Kind::Air => "air",
            Kind::Helo => "helo",
            Kind::Missile => "missile",
            Kind::Bomb => "bomb",
            Kind::Rocket => "rocket",
            Kind::Torpedo => "torpedo",
            Kind::Sam => "sam",
            Kind::Armor => "armor",
            Kind::Vehicle => "vehicle",
            Kind::Infantry => "infantry",
            Kind::Static => "static",
            Kind::Ship => "ship",
            Kind::Carrier => "carrier",
        }
    }

    /// Minimum spacing of stored samples.
    fn interval_ms(self) -> i64 {
        match self {
            Kind::Air | Kind::Helo => 500,
            Kind::Missile | Kind::Bomb | Kind::Rocket | Kind::Torpedo => 250,
            Kind::Ship | Kind::Carrier => 5_000,
            Kind::Sam | Kind::Armor | Kind::Vehicle | Kind::Infantry => 5_000,
            Kind::Static => i64::MAX,
        }
    }

    fn is_weapon(self) -> bool {
        matches!(self, Kind::Missile | Kind::Bomb | Kind::Rocket | Kind::Torpedo)
    }

    fn is_air(self) -> bool {
        matches!(self, Kind::Air | Kind::Helo)
    }
}

/// Map a Tacview `Type=` tag list to what we keep; `None` drops the object.
pub(crate) fn classify(ty: &str) -> Option<Kind> {
    let has = |t: &str| ty.split('+').any(|x| x == t);
    const CLUTTER: [&str; 14] = [
        "Shell", "Bullet", "Projectile", "Grenade", "Flare", "Chaff", "Decoy", "SmokeGrenade",
        "Shrapnel", "Explosion", "Parachutist", "Bullseye", "Waypoint", "Beam",
    ];
    if CLUTTER.iter().any(|c| has(c)) {
        return None;
    }
    if has("Air") {
        Some(if has("Rotorcraft") { Kind::Helo } else { Kind::Air })
    } else if has("Weapon") {
        if has("Missile") {
            Some(Kind::Missile)
        } else if has("Bomb") {
            Some(Kind::Bomb)
        } else if has("Rocket") {
            Some(Kind::Rocket)
        } else if has("Torpedo") {
            Some(Kind::Torpedo)
        } else {
            None
        }
    } else if has("Ground") {
        Some(if has("AntiAircraft") {
            Kind::Sam
        } else if has("Static") || has("Building") || has("Aerodrome") || has("Container") {
            Kind::Static
        } else if has("Tank") || has("Armor") {
            Kind::Armor
        } else if has("Infantry") || has("Human") {
            Kind::Infantry
        } else {
            Kind::Vehicle
        })
    } else if has("Sea") {
        Some(if has("AircraftCarrier") { Kind::Carrier } else { Kind::Ship })
    } else {
        None
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct S {
    t: i64,
    lon: f64,
    lat: f64,
    alt: f64,
    roll: f64,
    pitch: f64,
    yaw: f64,
    ias: f64,
    mach: f64,
    aoa: f64,
    agl: f64,
}

#[derive(Default)]
struct Obj {
    kind: Option<Kind>,
    typed: bool,
    skip: bool,
    name: Option<String>,
    pilot: Option<String>,
    group: Option<String>,
    color: Option<String>,
    callsign: Option<String>,
    country: Option<String>,
    parent_tid: Option<u64>,
    launcher: Option<u32>,
    t0: i64,
    t1: i64,
    cur: S,
    positioned: bool,
    pending: bool,
    emitted: bool,
    last_emit: i64,
    buf: Vec<S>,
    carry: Option<S>,
    flags: u8,
    alive: bool,
    destroyed: Option<(i64, S)>,
    idx: Option<u32>,
    // ── derived from the track (DCS writes no Destroyed/TakenOff/Landed) ──
    /// Last position used for ground speed, and that speed (m/s).
    prev: Option<S>,
    gs: f64,
    /// Airborne (aircraft with AGL): settled state, and a pending change
    /// with when it started.
    air: Option<bool>,
    cand: Option<(bool, i64)>,
    /// Weapons: when the closest-enemy scan last ran, and the closest
    /// approach so far (distance m, sid, time).
    scan_t: Option<i64>,
    best: Option<(f64, u32, i64)>,
    /// Left the recording before it ended, and whether it was flying then.
    removed: bool,
    removed_airborne: bool,
}

impl Obj {
    fn interval(&self) -> i64 {
        self.kind.map(Kind::interval_ms).unwrap_or(500)
    }

    fn emit(&mut self) {
        self.buf.push(self.cur);
        self.last_emit = self.cur.t;
        self.emitted = true;
        self.pending = false;
    }
}

/// Ingest logic version. Recordings processed by an older one are read again
/// (bumped when what we derive from a recording changes, or when a file an
/// older one gave up on can now be read -- 3: cut-off zips).
pub(crate) const INGEST_VERSION: u32 = 3;

/// A weapon whose closest approach to an enemy is within this is a hit.
fn lethal_radius_m(k: Kind) -> f64 {
    match k {
        Kind::Missile => 75.0,
        Kind::Bomb | Kind::Torpedo => 40.0,
        _ => 25.0,
    }
}

fn dist2d_m(a: &S, b: &S) -> f64 {
    let lat = ((a.lat + b.lat) / 2.0).to_radians();
    let dx = (a.lon - b.lon) * 111_320.0 * lat.cos();
    let dy = (a.lat - b.lat) * 110_540.0;
    (dx * dx + dy * dy).sqrt()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EvKind {
    Destroyed,
    TakeOff,
    Landing,
}

struct Ev {
    t: i64,
    kind: EvKind,
    sid: u32,
}

/// One flight found in a recording: an aircraft with a pilot name.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct Flight {
    /// Object index in the recording's meta.
    pub(crate) i: u32,
    pub(crate) pilot: String,
    pub(crate) aircraft: String,
    pub(crate) kind: &'static str,
    pub(crate) color: Option<String>,
    pub(crate) group: Option<String>,
    /// Recording-relative ms.
    pub(crate) t0: i64,
    pub(crate) t1: i64,
    /// Wall-clock unix ms.
    pub(crate) start_ms: i64,
    pub(crate) end_ms: i64,
    pub(crate) shots: u32,
    /// Shots that came within lethal range of an enemy.
    pub(crate) hits: u32,
    pub(crate) kills: u32,
    /// "destroyed", "landed" or "left".
    pub(crate) fate: &'static str,
}

/// What one processed recording is, for the index.
#[derive(Debug, Clone)]
pub(crate) struct Built {
    pub(crate) title: Option<String>,
    pub(crate) start_ms: i64,
    pub(crate) duration_ms: i64,
    pub(crate) objects: usize,
    pub(crate) chunks: u32,
    pub(crate) flights: Vec<Flight>,
}

fn dist_m(a: &S, b: &S) -> f64 {
    let lat = ((a.lat + b.lat) / 2.0).to_radians();
    let dx = (a.lon - b.lon) * 111_320.0 * lat.cos();
    let dy = (a.lat - b.lat) * 110_540.0;
    let dz = a.alt - b.alt;
    (dx * dx + dy * dy + dz * dz).sqrt()
}

struct Builder<'a> {
    out: &'a Path,
    ref_lon: f64,
    ref_lat: f64,
    reference_time: Option<String>,
    recording_time: Option<String>,
    title: Option<String>,
    author: Option<String>,
    data_source: Option<String>,
    first_t: Option<f64>,
    first_frame_s: f64,
    now: i64,
    chunk: i64,
    objs: Vec<Obj>,
    alive: FxHashMap<u64, u32>,
    /// sids that may have something to write in the current chunk.
    active: Vec<u32>,
    next_idx: u32,
    events: Vec<Ev>,
}

impl<'a> Builder<'a> {
    fn frame(&mut self, t: f64) -> Result<()> {
        let first = *self.first_t.get_or_insert_with(|| {
            self.first_frame_s = t;
            t
        });
        let ms = ((t - first) * 1000.0).round() as i64;
        // Time only goes forward; a stray earlier frame is held at "now".
        if ms > self.now {
            self.now = ms;
        }
        while self.now >= (self.chunk + 1) * CHUNK_MS {
            self.flush_chunk()?;
            self.chunk += 1;
        }
        Ok(())
    }

    fn global(&mut self, props: acmi::Props<'_>) {
        for (k, v) in props {
            match k {
                "ReferenceLongitude" => self.ref_lon = v.parse().unwrap_or(0.0),
                "ReferenceLatitude" => self.ref_lat = v.parse().unwrap_or(0.0),
                "ReferenceTime" => self.reference_time = Some(v.into_owned()),
                "RecordingTime" => self.recording_time = Some(v.into_owned()),
                "Title" => self.title = Some(v.into_owned()),
                "Author" => self.author = Some(v.into_owned()),
                "DataSource" => self.data_source = Some(v.into_owned()),
                "Event" => self.event(&v),
                _ => (),
            }
        }
    }

    /// `Type|id|id|...|text`
    fn event(&mut self, v: &str) {
        let mut parts = v.split('|');
        let kind = match parts.next() {
            Some("Destroyed") => EvKind::Destroyed,
            Some("TakenOff") => EvKind::TakeOff,
            Some("Landed") => EvKind::Landing,
            _ => return,
        };
        let Some(sid) = parts
            .next()
            .and_then(|id| u64::from_str_radix(id.trim(), 16).ok())
            .and_then(|tid| self.alive.get(&tid).copied())
        else {
            return;
        };
        let now = self.now;
        let o = &mut self.objs[sid as usize];
        if kind == EvKind::Destroyed {
            if o.destroyed.is_some() {
                return;
            }
            o.destroyed = Some((now, o.cur));
        }
        self.events.push(Ev { t: now, kind, sid });
    }

    fn remove(&mut self, tid: u64) {
        let Some(&sid) = self.alive.get(&tid) else { return };
        // A weapon's last look for its target, while everything that was
        // near it is still in the picture.
        if self.objs[sid as usize].kind.map(Kind::is_weapon).unwrap_or(false) && self.objs[sid as usize].positioned {
            self.scan(sid, true);
        }
        self.alive.remove(&tid);
        let o = &mut self.objs[sid as usize];
        o.alive = false;
        o.removed = true;
        o.t1 = self.now;
        o.removed_airborne = o.kind.map(Kind::is_air).unwrap_or(false)
            && match o.air {
                Some(a) => a,
                // No AGL in the recording: flying if it was moving like it.
                None => o.gs > 50.0,
            };
        if o.pending && o.positioned {
            o.emit();
        }
    }

    /// Track an aircraft's ground speed and whether it is flying; a change
    /// that holds for 3 s is a takeoff or a landing.
    fn air_state(&mut self, sid: u32) {
        let now = self.now;
        let o = &mut self.objs[sid as usize];
        match o.prev {
            Some(p) if now - p.t >= 500 => {
                o.gs = dist2d_m(&p, &o.cur) / ((now - p.t) as f64 / 1000.0);
                o.prev = Some(o.cur);
            }
            Some(_) => (),
            None => o.prev = Some(o.cur),
        }
        if o.flags & F_AGL == 0 {
            return;
        }
        let helo = o.kind == Some(Kind::Helo);
        let (agl, gs) = (o.cur.agl, o.gs);
        let up = if helo { agl > 8.0 } else { agl > 15.0 && gs > 25.0 };
        let down = if helo { agl < 3.5 && gs < 6.0 } else { agl < 4.0 && gs < 40.0 };
        match o.air {
            None => {
                if up {
                    o.air = Some(true);
                } else if down {
                    o.air = Some(false);
                }
            }
            Some(cur) => {
                let wants = if cur { down } else { up };
                match (wants, o.cand) {
                    (false, _) => o.cand = None,
                    (true, None) => o.cand = Some((!cur, now)),
                    (true, Some((to, since))) if now - since >= 3_000 => {
                        o.air = Some(to);
                        o.cand = None;
                        let kind = if to { EvKind::TakeOff } else { EvKind::Landing };
                        self.events.push(Ev { t: since, kind, sid });
                    }
                    (true, Some(_)) => (),
                }
            }
        }
    }

    /// A weapon's closest approach to anything not on its side. Runs every
    /// second while it is far from everything and on every update once
    /// within 3 km, so the closest point is not stepped over.
    fn scan(&mut self, sid: u32, last: bool) {
        let now = self.now;
        let w = &self.objs[sid as usize];
        let close = w.best.map(|b| b.0 < 3_000.0).unwrap_or(false);
        if !last && !close && w.scan_t.map(|t| now - t < 1_000).unwrap_or(false) {
            return;
        }
        let at = w.cur;
        let launcher = w.launcher;
        let side = w.color.clone().or_else(|| launcher.and_then(|l| self.objs[l as usize].color.clone()));
        let mut best: Option<(f64, u32)> = None;
        for &c in self.alive.values() {
            if c == sid || Some(c) == launcher {
                continue;
            }
            let o = &self.objs[c as usize];
            let Some(k) = o.kind else { continue };
            if o.skip || !o.positioned || k.is_weapon() {
                continue;
            }
            if side.is_some() && o.color == side {
                continue;
            }
            let d = dist_m(&o.cur, &at);
            if best.map(|(bd, _)| d < bd).unwrap_or(true) {
                best = Some((d, c));
            }
        }
        let w = &mut self.objs[sid as usize];
        w.scan_t = Some(now);
        if let Some((d, c)) = best {
            if w.best.map(|(bd, _, _)| d < bd).unwrap_or(true) {
                w.best = Some((d, c, now));
            }
        }
    }

    fn object(&mut self, tid: u64, props: acmi::Props<'_>) {
        let sid = match self.alive.get(&tid) {
            Some(sid) => *sid,
            None => {
                let sid = self.objs.len() as u32;
                self.objs.push(Obj { t0: self.now, t1: self.now, alive: true, last_emit: i64::MIN, ..Default::default() });
                self.alive.insert(tid, sid);
                self.active.push(sid);
                sid
            }
        };
        let now = self.now;
        let (ref_lon, ref_lat) = (self.ref_lon, self.ref_lat);
        let o = &mut self.objs[sid as usize];
        if o.skip {
            return;
        }
        let mut tf = None;
        let mut moved = false;
        for (k, v) in props {
            match k {
                "T" => tf = Some(acmi::transform(&v)),
                "Type" => {
                    o.typed = true;
                    o.kind = classify(&v);
                    if o.kind.is_none() {
                        o.skip = true;
                        o.buf = Vec::new();
                        return;
                    }
                }
                "Name" => o.name = Some(v.into_owned()),
                "Pilot" => o.pilot = Some(v.into_owned()),
                "Group" => o.group = Some(v.into_owned()),
                "Color" => o.color = Some(v.into_owned()),
                "CallSign" => o.callsign = Some(v.into_owned()),
                "Country" => o.country = Some(v.into_owned()),
                "Parent" => o.parent_tid = u64::from_str_radix(v.trim(), 16).ok(),
                "IAS" | "Mach" | "AOA" | "AGL" => {
                    let Ok(x) = v.parse::<f64>() else { continue };
                    let (slot, flag) = match k {
                        "IAS" => (&mut o.cur.ias, F_IAS),
                        "Mach" => (&mut o.cur.mach, F_MACH),
                        "AOA" => (&mut o.cur.aoa, F_AOA),
                        _ => (&mut o.cur.agl, F_AGL),
                    };
                    *slot = x;
                    o.flags |= flag;
                    moved = true;
                }
                _ => (),
            }
        }
        if let Some(tf) = tf {
            if let Some(x) = tf.lon {
                o.cur.lon = ref_lon + x;
            }
            if let Some(x) = tf.lat {
                o.cur.lat = ref_lat + x;
            }
            if let Some(x) = tf.alt {
                o.cur.alt = x;
            }
            if tf.has_att {
                o.flags |= F_ATT;
                if let Some(x) = tf.roll {
                    o.cur.roll = x;
                }
                if let Some(x) = tf.pitch {
                    o.cur.pitch = x;
                }
                if let Some(x) = tf.yaw {
                    o.cur.yaw = x;
                }
            }
            moved = true;
        }
        if !moved {
            return;
        }
        let first_fix = !o.positioned && tf.is_some();
        if tf.is_some() {
            o.positioned = true;
        }
        if !o.positioned {
            return;
        }
        o.cur.t = now;
        o.pending = true;
        if !o.emitted || now - o.last_emit >= o.interval() {
            o.emit();
        }
        let kind = o.kind;
        if first_fix && kind.map(Kind::is_weapon).unwrap_or(false) {
            self.find_launcher(sid);
        }
        if tf.is_some() {
            if kind.map(Kind::is_air).unwrap_or(false) {
                self.air_state(sid);
            } else if kind.map(Kind::is_weapon).unwrap_or(false) {
                self.scan(sid, false);
            }
        }
    }

    /// Who fired a weapon: its `Parent` when the recording names one,
    /// otherwise the nearest aircraft (or, failing that, ground unit or ship)
    /// within 1.5 km of where it first appeared.
    fn find_launcher(&mut self, sid: u32) {
        let w = &self.objs[sid as usize];
        if let Some(p) = w.parent_tid.and_then(|tid| self.alive.get(&tid).copied()) {
            self.objs[sid as usize].launcher = Some(p);
            return;
        }
        let at = w.cur;
        let mut best_air: Option<(f64, u32)> = None;
        let mut best_other: Option<(f64, u32)> = None;
        for &c in self.alive.values() {
            if c == sid {
                continue;
            }
            let o = &self.objs[c as usize];
            let Some(k) = o.kind else { continue };
            if o.skip || !o.positioned || k.is_weapon() {
                continue;
            }
            let d = dist_m(&o.cur, &at);
            if d > 1500.0 {
                continue;
            }
            let slot = if k.is_air() { &mut best_air } else { &mut best_other };
            if slot.map(|(bd, _)| d < bd).unwrap_or(true) {
                *slot = Some((d, c));
            }
        }
        self.objs[sid as usize].launcher = best_air.or(best_other).map(|(_, c)| c);
    }

    fn flush_chunk(&mut self) -> Result<()> {
        let mut entries: Vec<(u32, u8, Vec<S>)> = vec![];
        let mut still = Vec::with_capacity(self.active.len());
        for &sid in &self.active {
            let o = &mut self.objs[sid as usize];
            if o.skip {
                continue;
            }
            if !o.typed {
                // Wait for its Type while it lives; forget it if it never had one.
                if o.alive {
                    still.push(sid);
                }
                continue;
            }
            if o.pending && o.positioned {
                o.emit();
            }
            if o.buf.is_empty() && !(o.alive && o.carry.is_some()) {
                if o.alive {
                    still.push(sid);
                }
                continue;
            }
            let idx = *o.idx.get_or_insert_with(|| {
                let i = self.next_idx;
                self.next_idx += 1;
                i
            });
            let mut samples = Vec::with_capacity(o.buf.len() + 1);
            if let Some(c) = o.carry {
                if o.buf.first().map(|f| c.t < f.t).unwrap_or(true) {
                    samples.push(c);
                }
            }
            samples.append(&mut o.buf);
            o.carry = samples.last().copied();
            entries.push((idx, o.flags, samples));
            if o.alive {
                still.push(sid);
            }
        }
        self.active = still;
        write_chunk(&self.out.join(format!("c{}.bin.gz", self.chunk)), &entries)
    }

    fn finish(mut self, path: &Path) -> Result<Built> {
        let end = self.now;
        for o in self.objs.iter_mut().filter(|o| o.alive) {
            o.t1 = end;
        }
        self.flush_chunk()?;
        let chunks = (self.chunk + 1) as u32;

        let start_ms = self
            .recording_time
            .as_deref()
            .and_then(|s| DateTime::parse_from_rfc3339(s).ok())
            .map(|d| d.with_timezone(&Utc).timestamp_millis())
            .or_else(|| {
                let m = fs::metadata(path).ok()?.modified().ok()?;
                let m: DateTime<Utc> = m.into();
                Some(m.timestamp_millis() - end)
            })
            .unwrap_or(0);

        // sid -> meta index, for the kept objects that made it to a chunk.
        let idx_of = |sid: u32| self.objs[sid as usize].idx;

        let mut events: Vec<Value> = vec![];
        let mut shots: FxHashMap<u32, u32> = FxHashMap::default();
        let mut hits: FxHashMap<u32, u32> = FxHashMap::default();
        let mut kills: FxHashMap<u32, u32> = FxHashMap::default();
        let mut landed_last: FxHashMap<u32, bool> = FxHashMap::default();
        // victim sid -> (time of death, killing weapon sid if known)
        let mut dead: FxHashMap<u32, (i64, Option<u32>)> = FxHashMap::default();

        // Shots: who fired, at whom (the enemy it came closest to), how close.
        // A hit whose target leaves the recording soon after is the kill --
        // within 90 s for an aircraft (a wreck falls for a while), 15 s for
        // anything on the ground.
        let mut by_victim: FxHashMap<u32, (i64, u32)> = FxHashMap::default();
        for (wsid, o) in self.objs.iter().enumerate() {
            let (Some(w), Some(k)) = (o.idx, o.kind) else { continue };
            if !k.is_weapon() {
                continue;
            }
            let by = o.launcher.and_then(idx_of);
            if let Some(by) = by {
                *shots.entry(by).or_default() += 1;
            }
            let hit = o.best.filter(|(d, _, _)| *d <= lethal_radius_m(k));
            if let (Some(by), Some(_)) = (by, hit) {
                *hits.entry(by).or_default() += 1;
            }
            let tg = o.best.and_then(|(_, c, _)| idx_of(c));
            events.push(json!({
                "t": o.t0,
                "k": "fired",
                "o": by,
                "w": w,
                "tg": tg,
                "md": o.best.map(|(d, _, _)| d.round() as i64),
                "hit": hit.is_some(),
                "te": o.t1,
            }));
            if let Some((_, vsid, th)) = hit {
                let v = &self.objs[vsid as usize];
                let window = if v.kind.map(Kind::is_air).unwrap_or(false) { 90_000 } else { 15_000 };
                if v.removed && v.t1 >= th - 2_000 && v.t1 <= th + window {
                    // The hit closest before the death takes the credit.
                    let e = by_victim.entry(vsid).or_insert((th, wsid as u32));
                    if th > e.0 && th <= v.t1 + 2_000 {
                        *e = (th, wsid as u32);
                    }
                }
            }
        }
        for (vsid, (_, wsid)) in &by_victim {
            dead.insert(*vsid, (self.objs[*vsid as usize].t1, Some(*wsid)));
        }
        // Recordings that do carry Destroyed events (not DCS today): the
        // weapon that ended nearest the victim around then.
        for ev in self.events.iter().filter(|e| e.kind == EvKind::Destroyed) {
            if dead.contains_key(&ev.sid) {
                continue;
            }
            let (td, at) = self.objs[ev.sid as usize].destroyed.unwrap_or((ev.t, S::default()));
            let w = self
                .objs
                .iter()
                .enumerate()
                .filter(|(_, w)| w.idx.is_some() && w.kind.map(Kind::is_weapon).unwrap_or(false))
                .filter(|(_, w)| w.t1 >= td - 5_000 && w.t1 <= td + 3_000)
                .map(|(i, w)| (dist_m(&w.cur, &at), i as u32))
                .filter(|(d, _)| *d < 300.0)
                .min_by(|a, b| a.0.total_cmp(&b.0))
                .map(|(_, i)| i);
            dead.insert(ev.sid, (td, w));
        }
        for (vsid, (td, wsid)) in &dead {
            let Some(o) = idx_of(*vsid) else { continue };
            let wobj = wsid.map(|w| &self.objs[w as usize]);
            let by = wobj.and_then(|w| w.launcher).and_then(idx_of);
            if let Some(by) = by {
                *kills.entry(by).or_default() += 1;
            }
            match wobj {
                Some(w) => events.push(json!({ "t": td, "k": "kill", "o": o, "by": by, "w": w.idx })),
                None => events.push(json!({ "t": td, "k": "destroyed", "o": o })),
            }
        }
        // An aircraft that vanished in flight with nothing on it: shot down
        // by guns, crashed, collided, or its pilot left mid-air.
        for (sid, o) in self.objs.iter().enumerate() {
            if o.removed_airborne && !dead.contains_key(&(sid as u32)) {
                if let Some(i) = o.idx {
                    events.push(json!({ "t": o.t1, "k": "lost", "o": i }));
                }
            }
        }
        let mut nav: Vec<&Ev> = self.events.iter().filter(|e| e.kind != EvKind::Destroyed).collect();
        nav.sort_by_key(|e| e.t);
        for ev in nav {
            let Some(o) = idx_of(ev.sid) else { continue };
            let landing = ev.kind == EvKind::Landing;
            landed_last.insert(o, landing);
            events.push(json!({ "t": ev.t, "k": if landing { "landing" } else { "takeoff" }, "o": o }));
        }
        events.sort_by_key(|e| e["t"].as_i64().unwrap_or(0));

        // meta index -> time it was destroyed or lost
        let mut deaths: FxHashMap<u32, i64> = FxHashMap::default();
        for (sid, (td, _)) in &dead {
            if let Some(i) = idx_of(*sid) {
                deaths.insert(i, *td);
            }
        }
        for o in self.objs.iter().filter(|o| o.removed_airborne) {
            if let Some(i) = o.idx {
                deaths.entry(i).or_insert(o.t1);
            }
        }
        let mut by_idx: Vec<&Obj> = self.objs.iter().filter(|o| o.idx.is_some()).collect();
        by_idx.sort_by_key(|o| o.idx);
        let objects: Vec<Value> = by_idx
            .iter()
            .map(|o| {
                json!({
                    "k": o.kind.map(Kind::code),
                    "n": o.name,
                    "p": o.pilot,
                    "g": o.group,
                    "c": o.color,
                    "cs": o.callsign,
                    "co": o.country,
                    "t0": o.t0,
                    "t1": o.t1,
                    "par": o.launcher.and_then(idx_of),
                    "f": o.flags,
                    "d": deaths.get(&o.idx.unwrap_or(u32::MAX)).copied(),
                })
            })
            .collect();

        let mut flights = vec![];
        for o in &by_idx {
            let (Some(i), Some(k)) = (o.idx, o.kind) else { continue };
            if !k.is_air() {
                continue;
            }
            let Some(pilot) = o.pilot.as_deref().map(str::trim).filter(|p| !p.is_empty()) else { continue };
            flights.push(Flight {
                i,
                pilot: pilot.to_string(),
                aircraft: o.name.clone().unwrap_or_default(),
                kind: k.code(),
                color: o.color.clone(),
                group: o.group.clone(),
                t0: o.t0,
                t1: o.t1,
                start_ms: start_ms + o.t0,
                end_ms: start_ms + o.t1,
                shots: shots.get(&i).copied().unwrap_or(0),
                hits: hits.get(&i).copied().unwrap_or(0),
                kills: kills.get(&i).copied().unwrap_or(0),
                fate: if deaths.contains_key(&i) {
                    "destroyed"
                } else if landed_last.get(&i).copied().unwrap_or(false) {
                    "landed"
                } else {
                    "left"
                },
            });
        }

        let meta = json!({
            "v": INGEST_VERSION,
            "title": self.title,
            "author": self.author,
            "data_source": self.data_source,
            "reference_time": self.reference_time,
            "recording_time": self.recording_time,
            "first_frame_s": self.first_frame_s,
            "start_ms": start_ms,
            "duration_ms": end,
            "chunk_ms": CHUNK_MS,
            "chunks": chunks,
            "objects": objects,
            "events": events,
            "flights": flights,
        });
        write_gz(&self.out.join("meta.json.gz"), &serde_json::to_vec(&meta)?)?;
        Ok(Built {
            title: self.title,
            start_ms,
            duration_ms: end,
            objects: objects.len(),
            chunks,
            flights,
        })
    }
}

fn write_gz(path: &Path, bytes: &[u8]) -> Result<()> {
    let f = fs::File::create(path).with_context(|| format!("creating {}", path.display()))?;
    let mut gz = GzEncoder::new(std::io::BufWriter::new(f), Compression::new(6));
    gz.write_all(bytes)?;
    gz.finish()?.flush()?;
    Ok(())
}

fn write_chunk(path: &Path, entries: &[(u32, u8, Vec<S>)]) -> Result<()> {
    let mut b: Vec<u8> = Vec::with_capacity(64 + entries.iter().map(|e| 9 + e.2.len() * 44).sum::<usize>());
    b.extend_from_slice(b"BFR1");
    b.extend_from_slice(&(entries.len() as u32).to_le_bytes());
    for (idx, flags, s) in entries {
        b.extend_from_slice(&idx.to_le_bytes());
        b.extend_from_slice(&(s.len() as u32).to_le_bytes());
        b.push(*flags);
        let mut col = |f: &dyn Fn(&S) -> i64| {
            let mut prev = 0i64;
            for x in s {
                let v = f(x);
                b.extend_from_slice(&((v - prev) as i32).to_le_bytes());
                prev = v;
            }
        };
        col(&|x| x.t);
        col(&|x| (x.lon * 1e7).round() as i64);
        col(&|x| (x.lat * 1e7).round() as i64);
        col(&|x| (x.alt * 10.0).round() as i64);
        if flags & F_ATT != 0 {
            col(&|x| (x.roll * 100.0).round() as i64);
            col(&|x| (x.pitch * 100.0).round() as i64);
            col(&|x| (x.yaw * 100.0).round() as i64);
        }
        if flags & F_IAS != 0 {
            col(&|x| (x.ias * 100.0).round() as i64);
        }
        if flags & F_MACH != 0 {
            col(&|x| (x.mach * 1000.0).round() as i64);
        }
        if flags & F_AOA != 0 {
            col(&|x| (x.aoa * 100.0).round() as i64);
        }
        if flags & F_AGL != 0 {
            col(&|x| (x.agl * 10.0).round() as i64);
        }
    }
    write_gz(path, &b)
}

/// Number of i32 columns an entry with these flags carries.
fn ncols(flags: u8) -> usize {
    4 + if flags & F_ATT != 0 { 3 } else { 0 }
        + [F_IAS, F_MACH, F_AOA, F_AGL].iter().filter(|f| flags & **f != 0).count()
}

/// One decoded chunk entry: object index, flags, absolute column values.
type Entry = (u32, u8, Vec<Vec<i64>>);

/// Decode a chunk's (inflated) bytes, keeping only object `want` if given.
fn read_chunk(b: &[u8], want: Option<u32>) -> Result<Vec<Entry>> {
    let rd_u32 = |at: usize| -> Result<u32> {
        Ok(u32::from_le_bytes(b.get(at..at + 4).context("truncated chunk")?.try_into()?))
    };
    if b.get(..4) != Some(b"BFR1".as_slice()) {
        bail!("not a replay chunk");
    }
    let nobj = rd_u32(4)?;
    let mut at = 8;
    let mut out = vec![];
    for _ in 0..nobj {
        let idx = rd_u32(at)?;
        let n = rd_u32(at + 4)? as usize;
        let flags = *b.get(at + 8).context("truncated chunk")?;
        at += 9;
        let cols = ncols(flags);
        let len = cols * n * 4;
        if want.map(|w| w == idx).unwrap_or(true) {
            let body = b.get(at..at + len).context("truncated chunk")?;
            let mut columns = Vec::with_capacity(cols);
            for c in 0..cols {
                let mut acc = 0i64;
                let col = (0..n)
                    .map(|i| {
                        let o = (c * n + i) * 4;
                        acc += i32::from_le_bytes(body[o..o + 4].try_into().unwrap()) as i64;
                        acc
                    })
                    .collect();
                columns.push(col);
            }
            out.push((idx, flags, columns));
        }
        at += len;
    }
    Ok(out)
}

fn write_entries_gz(path: &Path, entries: &[Entry]) -> Result<()> {
    let mut b = Vec::new();
    b.extend_from_slice(b"BFR1");
    b.extend_from_slice(&(entries.len() as u32).to_le_bytes());
    for (idx, flags, cols) in entries {
        let n = cols.first().map(Vec::len).unwrap_or(0);
        b.extend_from_slice(&idx.to_le_bytes());
        b.extend_from_slice(&(n as u32).to_le_bytes());
        b.push(*flags);
        for col in cols {
            let mut prev = 0i64;
            for v in col {
                b.extend_from_slice(&((v - prev) as i32).to_le_bytes());
                prev = *v;
            }
        }
    }
    write_gz(path, &b)
}

/// One object's whole track, stitched from the chunks `first..=last`, in the
/// chunk format (one entry). Written once to `o<idx>.bin.gz` and reused;
/// returns that file's path.
pub(crate) fn object_track(dir: &Path, idx: u32, first: u32, last: u32) -> Result<PathBuf> {
    use flate2::read::GzDecoder;
    use std::io::Read;
    let path = dir.join(format!("o{idx}.bin.gz"));
    if path.exists() {
        return Ok(path);
    }
    let mut merged: Option<Entry> = None;
    for n in first..=last {
        let p = dir.join(format!("c{n}.bin.gz"));
        let Ok(f) = fs::File::open(&p) else { continue };
        let mut raw = vec![];
        GzDecoder::new(f).read_to_end(&mut raw)?;
        for (i, flags, cols) in read_chunk(&raw, Some(idx))? {
            match &mut merged {
                None => merged = Some((i, flags, cols)),
                Some((_, mflags, mcols)) => {
                    if *mflags != flags {
                        // Flags only ever gain bits; keep the old columns'
                        // shape by dropping the new extras (rare: telemetry
                        // that first appears mid-flight).
                        continue;
                    }
                    let last_t = *mcols[0].last().unwrap_or(&i64::MIN);
                    let skip = cols[0].iter().take_while(|t| **t <= last_t).count();
                    for (m, c) in mcols.iter_mut().zip(cols) {
                        m.extend_from_slice(&c[skip..]);
                    }
                }
            }
        }
    }
    let entries: Vec<Entry> = merged.into_iter().collect();
    // Write via a temp name so a concurrent request never reads half a file.
    let tmp = dir.join(format!("o{idx}.bin.gz.{}", std::process::id()));
    write_entries_gz(&tmp, &entries)?;
    fs::rename(&tmp, &path)?;
    Ok(path)
}

/// Process the recording at `path` into `out` (created; must not exist).
pub(crate) fn build(path: &Path, out: &Path) -> Result<Built> {
    if out.exists() {
        bail!("{} already exists", out.display());
    }
    fs::create_dir_all(out)?;
    let mut b = Builder {
        out,
        ref_lon: 0.0,
        ref_lat: 0.0,
        reference_time: None,
        recording_time: None,
        title: None,
        author: None,
        data_source: None,
        first_t: None,
        first_frame_s: 0.0,
        now: 0,
        chunk: 0,
        objs: Vec::with_capacity(16 * 1024),
        alive: FxHashMap::default(),
        active: vec![],
        next_idx: 0,
        events: vec![],
    };
    acmi::open(path, |r| {
        while let Some(line) = r.next_line()? {
            match line {
                Line::Frame(t) => b.frame(t)?,
                Line::Remove(id) => b.remove(id),
                Line::Object(0, props) => b.global(props),
                Line::Object(id, props) => b.object(id, props),
            }
        }
        Ok(())
    })?;
    b.finish(path)
}

/// Where a recording's output goes while it is being built.
pub(crate) fn staging_dir(root: &Path, key: &str) -> PathBuf {
    root.join(format!("_building-{:016x}", fxhash::hash64(key)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use flate2::read::GzDecoder;
    use std::io::Read;

    fn gunzip(p: &Path) -> Vec<u8> {
        let mut v = vec![];
        GzDecoder::new(fs::File::open(p).unwrap()).read_to_end(&mut v).unwrap();
        v
    }

    #[test]
    fn classifies() {
        assert_eq!(classify("Air+FixedWing"), Some(Kind::Air));
        assert_eq!(classify("Air+Rotorcraft"), Some(Kind::Helo));
        assert_eq!(classify("Weapon+Missile"), Some(Kind::Missile));
        assert_eq!(classify("Projectile+Shell"), None);
        assert_eq!(classify("Misc+Decoy+Flare"), None);
        assert_eq!(classify("Ground+AntiAircraft"), Some(Kind::Sam));
        assert_eq!(classify("Ground+Heavy+Armor+Vehicle+Tank"), Some(Kind::Armor));
        assert_eq!(classify("Sea+Watercraft+AircraftCarrier"), Some(Kind::Carrier));
        assert_eq!(classify("Navaid+Static+Bullseye"), None);
    }

    /// What DCS actually writes: no Destroyed/TakenOff/Landed lines at all.
    /// Takeoff and landing come from AGL and ground speed, the kill from a
    /// missile ending on its target followed by the target leaving, a miss
    /// from a missile ending far from everything, and a loss from an
    /// aircraft vanishing in flight with nothing on it.
    #[test]
    fn derives_events_without_event_lines() {
        let dir = std::env::temp_dir().join(format!("bfdb-replay-dcs-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        let src = dir.join("dcs.txt.acmi");
        let mut t = String::from(
            "FileType=text/acmi/tacview\nFileVersion=2.2\n\
             0,ReferenceTime=2026-10-08T10:00:00Z,RecordingTime=2026-10-08T12:00:00Z,\
             ReferenceLongitude=41,ReferenceLatitude=42\n#0\n\
             a1,T=0.1|0.1|100|0|0|90,Type=Air+FixedWing,Name=F-16C_50,Pilot=Viper,Color=Blue,AGL=1\n\
             b2,T=0.3|0.1|5000|0|0|270,Type=Air+FixedWing,Name=MiG-29S,Pilot=Ivan,Color=Red,AGL=4800\n\
             e5,T=0.5|0.3|6000|0|0|270,Type=Air+FixedWing,Name=Su-27,Pilot=Boris,Color=Red,AGL=5800\n",
        );
        // 0.5 s frames. F-16: 20 s parked, then rolls and climbs east.
        let mut lon = 0.1f64;
        let mut agl = 1.0f64;
        for i in 1..=400 {
            let ts = i as f64 * 0.5;
            if ts > 20.0 && ts < 150.0 {
                lon += 0.0015; // ~120 m per frame = 240 m/s
                agl = (agl + 25.0).min(4000.0);
            } else if ts >= 150.0 && ts < 170.0 {
                lon += 0.0002; // slowing down on the runway
                agl = 1.0;
            }
            t.push_str(&format!("#{ts}\na1,T={lon}||{}|0|0|90,AGL={agl}\n", agl + 100.0));
            // Missile 1 at t=60: flies onto the MiG, which is then removed.
            let alt = agl + 100.0;
            if i == 120 {
                t.push_str(&format!("c3,T={lon}|0.1|{alt}|0|0|90,Type=Weapon+Missile,Name=AIM_120C,Color=Blue\n"));
            }
            if i > 120 && i <= 130 {
                let f = (i - 120) as f64 / 10.0;
                t.push_str(&format!("c3,T={}||{}\n", lon + (0.3 - lon) * f, alt + (5000.0 - alt) * f));
            }
            if i == 131 {
                t.push_str("-c3\n");
            }
            if i == 133 {
                t.push_str("-b2\n");
            }
            // Missile 2 at t=70: flies off north and self-destructs far away.
            if i == 140 {
                t.push_str(&format!("d4,T={lon}|0.1|{alt}|0|0|0,Type=Weapon+Missile,Name=AIM_120C,Color=Blue\n"));
            }
            if i > 140 && i <= 150 {
                t.push_str(&format!("d4,T=|{}|\n", 0.1 + (i - 140) as f64 * 0.05));
            }
            if i == 151 {
                t.push_str("-d4\n");
            }
            // The Su-27 flies west, then vanishes at t=100 with nothing near it.
            if i < 200 {
                t.push_str(&format!("e5,T={}||\n", 0.5 - i as f64 * 0.0005));
            }
            if i == 200 {
                t.push_str("-e5\n");
            }
        }
        fs::write(&src, t).unwrap();
        let out = dir.join("out");
        let built = build(&src, &out).unwrap();
        let meta: Value = serde_json::from_slice(&gunzip(&out.join("meta.json.gz"))).unwrap();
        let objs = meta["objects"].as_array().unwrap();
        let idx = |name: &str| objs.iter().position(|o| o["n"] == name).unwrap() as u64;
        let (f16, mig, su) = (idx("F-16C_50"), idx("MiG-29S"), idx("Su-27"));
        let ev = meta["events"].as_array().unwrap();
        let find = |k: &str| ev.iter().filter(|e| e["k"] == k).collect::<Vec<_>>();

        let takeoff = find("takeoff");
        assert_eq!(takeoff.len(), 1, "{ev:?}");
        assert!(takeoff[0]["t"].as_i64().unwrap() >= 20_000 && takeoff[0]["t"].as_i64().unwrap() < 30_000);
        assert_eq!(find("landing").len(), 1, "{ev:?}");

        let fired = find("fired");
        assert_eq!(fired.len(), 2);
        assert!(fired.iter().all(|e| e["o"] == f16));
        let hit = fired.iter().find(|e| e["hit"] == true).expect("one hit");
        assert_eq!(hit["tg"], mig);
        let miss = fired.iter().find(|e| e["hit"] == false).expect("one miss");
        assert!(miss["md"].as_i64().unwrap() > 1_000);

        let kill = find("kill");
        assert_eq!(kill.len(), 1, "{ev:?}");
        assert_eq!((kill[0]["o"].as_u64(), kill[0]["by"].as_u64()), (Some(mig), Some(f16)));
        let lost = find("lost");
        assert_eq!(lost.len(), 1, "{ev:?}");
        assert_eq!(lost[0]["o"].as_u64(), Some(su));

        let viper = built.flights.iter().find(|f| f.pilot == "Viper").unwrap();
        assert_eq!((viper.shots, viper.hits, viper.kills, viper.fate), (2, 1, 1, "landed"));
        assert_eq!(built.flights.iter().find(|f| f.pilot == "Ivan").unwrap().fate, "destroyed");
        assert_eq!(built.flights.iter().find(|f| f.pilot == "Boris").unwrap().fate, "destroyed");
        let _ = fs::remove_dir_all(&dir);
    }

    /// A shooter, a target, a missile that ends on the target, a shell, and
    /// enough time to cross a chunk boundary.
    #[test]
    fn builds_tracks_events_and_flights() {
        let dir = std::env::temp_dir().join(format!("bfdb-replay-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        let src = dir.join("t.txt.acmi");
        let mut text = String::from(
            "FileType=text/acmi/tacview\nFileVersion=2.2\n\
             0,ReferenceTime=2026-10-08T10:00:00Z,RecordingTime=2026-10-08T12:00:00Z,\
             ReferenceLongitude=41,ReferenceLatitude=42,Title=Test\n\
             #0\n\
             a1,T=0.1|0.1|5000|0|0|90,Type=Air+FixedWing,Name=F-16C_50,Pilot=Viper,Color=Blue\n\
             b2,T=0.2|0.1|5000|0|0|270,Type=Air+FixedWing,Name=MiG-29S,Pilot=Ivan,Color=Red\n\
             0,Event=TakenOff|a1|\n",
        );
        // 0.1 s frames for 10 s: the shooter moves, the missile flies.
        for i in 1..=100 {
            let t = i as f64 * 0.1;
            text.push_str(&format!("#{t}\na1,T={}||\n", 0.1 + t * 0.0001));
            if i == 10 {
                text.push_str("c3,T=0.1001|0.1|5000|0|0|90,Type=Weapon+Missile,Name=AIM-120C,Color=Blue\n");
                text.push_str("d4,T=0.1|0.1|5000,Type=Projectile+Shell\n");
            }
            if i > 10 {
                text.push_str(&format!("c3,T={}||\n", 0.1001 + (t - 1.0) * (0.0999 / 9.0)));
            }
        }
        text.push_str("0,Event=Destroyed|b2|\n-c3\n-b2\n#400\na1,T=0.2||\n0,Event=Landed|a1|\n");
        fs::write(&src, text).unwrap();

        let out = dir.join("out");
        let built = build(&src, &out).unwrap();
        assert_eq!(built.chunks, 2, "400 s spans two 300 s chunks");
        assert_eq!(built.duration_ms, 400_000);
        assert_eq!(built.start_ms, DateTime::parse_from_rfc3339("2026-10-08T12:00:00Z").unwrap().timestamp_millis());
        assert_eq!(built.objects, 3, "the shell is dropped");

        let meta: Value = serde_json::from_slice(&gunzip(&out.join("meta.json.gz"))).unwrap();
        let objs = meta["objects"].as_array().unwrap();
        let idx = |name: &str| objs.iter().position(|o| o["n"] == name).unwrap() as u64;
        let (f16, mig, aim) = (idx("F-16C_50"), idx("MiG-29S"), idx("AIM-120C"));
        assert_eq!(objs[aim as usize]["par"], f16, "launcher found by proximity");
        let ev = meta["events"].as_array().unwrap();
        let kill = ev.iter().find(|e| e["k"] == "kill").expect("a kill");
        assert_eq!((kill["o"].as_u64(), kill["by"].as_u64()), (Some(mig), Some(f16)));
        assert!(ev.iter().any(|e| e["k"] == "fired" && e["o"] == f16));
        assert!(ev.iter().any(|e| e["k"] == "takeoff" && e["o"] == f16));

        let viper = built.flights.iter().find(|f| f.pilot == "Viper").unwrap();
        assert_eq!((viper.shots, viper.kills, viper.fate), (1, 1, "landed"));
        let ivan = built.flights.iter().find(|f| f.pilot == "Ivan").unwrap();
        assert_eq!(ivan.fate, "destroyed");

        // Chunk 1 carries the F-16's last pre-window sample so it stands alone.
        let c1 = gunzip(&out.join("c1.bin.gz"));
        assert_eq!(&c1[..4], b"BFR1");
        let nobj = u32::from_le_bytes(c1[4..8].try_into().unwrap());
        assert_eq!(nobj, 1, "only the F-16 is still alive");
        let n = u32::from_le_bytes(c1[12..16].try_into().unwrap());
        assert_eq!(n, 2, "carry + the t=400 s sample");
        let first_t = i32::from_le_bytes(c1[17..21].try_into().unwrap());
        assert!(first_t < 300_000);

        // The stitched track has every F-16 sample exactly once, in order.
        let f16_track = object_track(&out, f16 as u32, 0, 1).unwrap();
        let e = read_chunk(&gunzip(&f16_track), None).unwrap();
        assert_eq!(e.len(), 1);
        let ts = &e[0].2[0];
        assert!(ts.windows(2).all(|w| w[0] < w[1]), "strictly increasing: {ts:?}");
        assert_eq!(*ts.last().unwrap(), 400_000);
        assert_eq!(ts[0], 0);
        let _ = fs::remove_dir_all(&dir);
    }
}
