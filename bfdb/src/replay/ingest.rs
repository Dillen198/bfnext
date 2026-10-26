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
//! Tracks are thinned per kind and the newest unwritten state is always
//! flushed when an object leaves or a window closes, so a missile's last
//! point is where it actually ended. Minimum sample spacing:
//!
//! | kind                                   | spacing        |
//! |----------------------------------------|----------------|
//! | aircraft, helicopters, guided weapons  | 100 ms         |
//! | flares, chaff, other decoys            | 500 ms         |
//! | ground units, ships, shells, parachutists, misc | 1 s   |
//! | statics, explosions, bullseyes         | once           |
//!
//! Everything Tacview shows is kept except `Shrapnel` and `Beam`. Shells,
//! countermeasures, explosions, parachutists, bullseyes and other markers
//! (`Kind::is_effect`) are drawn but never take part in a shot: they are
//! never a weapon's target or launcher.
//!
//! Besides the fixed columns (position, attitude, IAS, Mach, AOA, AGL),
//! every other object property whose value is a number is kept as a named
//! *channel*: `TAS`, `HDG`, `Throttle`, `FuelWeight`, `RadarMode`,
//! `LockedTargetRange`, `Health`, ... The object's last value is carried
//! forward into every sample after it was first set; samples before the
//! first set take that first value. `LockedTarget`, `LockedTarget2` ..
//! `LockedTarget9` are object ids in Tacview and are stored as the target's
//! meta index (resolved when the chunk is written), `-1` for none, unknown,
//! a target never written, or any sample before the first lock.
//!
//! Chunk layout (`BFR2`), little-endian, gzip'd:
//!
//! ```text
//! "BFR2" u16:nchan, per channel: u8:len, utf8 name, f32:scale
//! u32:nobj
//! per object: u32:idx u32:n u8:flags u16:nch, nch x u16:channel (index into the table above)
//!   then columns of n i32 each, delta-coded (first absolute):
//!   t (ms from recording start), lon (1e-7 deg), lat (1e-7 deg), alt (dm MSL)
//!   [flags&1  ATT]  roll, pitch, yaw (centidegrees)
//!   [flags&2  IAS]  cm/s
//!   [flags&4  MACH] x1000
//!   [flags&8  AOA]  centidegrees
//!   [flags&16 AGL]  dm
//!   then one column per listed channel, in the listed order:
//!   round(value / scale), so value = int x scale
//! ```
//!
//! The channel table holds only the channels used in that chunk. Channel
//! integers are clamped to +-(i32::MAX / 2) so every delta fits an i32.
//! Channel scales ([`channel_scale`]; a trailing digit, as in `Throttle2`,
//! is ignored): `LockedTarget*` 1; `*Mode`, `LandingGear`, `Tailhook`,
//! `Afterburner`, `AirBrakes`, `Flaps`, `Throttle*`, `Health` 0.001; `HDG`,
//! `HDM`, `AOS`, `*Azimuth`, `*Elevation`, `*Roll`, `*Pitch`, `*Yaw`,
//! `*Beamwidth` 0.01; `*Range`, `Radius`, `Length`, `Width`, `Height` 0.1;
//! `TAS`, `CAS` 0.01; `*GForce*` 0.001; `FuelWeight*`, `FuelVolume*` 0.1;
//! anything else 0.001.
//!
//! The older `BFR1` layout (ingest v3) is the same without the channel
//! table and the `nch` list; [`read_chunk`] reads both.
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
    /// Shells, bullets, projectiles, grenades.
    Shell,
    Flare,
    Chaff,
    /// Any other decoy.
    Decoy,
    Explosion,
    Parachutist,
    Bullseye,
    /// Any other typed object: smoke, waypoints, ...
    Misc,
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
            Kind::Shell => "shell",
            Kind::Flare => "flare",
            Kind::Chaff => "chaff",
            Kind::Decoy => "decoy",
            Kind::Explosion => "explosion",
            Kind::Parachutist => "parachutist",
            Kind::Bullseye => "bullseye",
            Kind::Misc => "misc",
        }
    }

    /// Minimum spacing of stored samples.
    fn interval_ms(self) -> i64 {
        match self {
            Kind::Air | Kind::Helo => 100,
            Kind::Missile | Kind::Bomb | Kind::Rocket | Kind::Torpedo => 100,
            Kind::Ship | Kind::Carrier => 1_000,
            Kind::Sam | Kind::Armor | Kind::Vehicle | Kind::Infantry => 1_000,
            Kind::Flare | Kind::Chaff | Kind::Decoy => 500,
            Kind::Shell | Kind::Parachutist | Kind::Misc => 1_000,
            Kind::Static | Kind::Explosion | Kind::Bullseye => i64::MAX,
        }
    }

    fn is_weapon(self) -> bool {
        matches!(self, Kind::Missile | Kind::Bomb | Kind::Rocket | Kind::Torpedo)
    }

    fn is_air(self) -> bool {
        matches!(self, Kind::Air | Kind::Helo)
    }

    /// Shown, but never a party to a shot: shells, countermeasures,
    /// explosions, parachutists, bullseyes and other markers.
    pub(crate) fn is_effect(self) -> bool {
        matches!(
            self,
            Kind::Shell
                | Kind::Flare
                | Kind::Chaff
                | Kind::Decoy
                | Kind::Explosion
                | Kind::Parachutist
                | Kind::Bullseye
                | Kind::Misc
        )
    }

    /// What a weapon can be fired at or fired from: aircraft, ground units,
    /// ships and statics.
    fn is_target(self) -> bool {
        !self.is_weapon() && !self.is_effect()
    }
}

/// Map a Tacview `Type=` tag list to what we keep; `None` drops the object
/// (only `Shrapnel` and `Beam`). Tag order matters: DCS types flares
/// `Misc+Decoy+Flare` and shells `Projectile+Shell`.
pub(crate) fn classify(ty: &str) -> Option<Kind> {
    let has = |t: &str| ty.split('+').any(|x| x == t);
    if has("Shrapnel") || has("Beam") {
        return None;
    }
    if has("Flare") {
        return Some(Kind::Flare);
    }
    if has("Chaff") {
        return Some(Kind::Chaff);
    }
    if has("Decoy") {
        return Some(Kind::Decoy);
    }
    if ["Shell", "Bullet", "Projectile", "Grenade"].iter().any(|t| has(t)) {
        return Some(Kind::Shell);
    }
    if has("Explosion") {
        return Some(Kind::Explosion);
    }
    if has("Parachutist") {
        return Some(Kind::Parachutist);
    }
    if has("Bullseye") {
        return Some(Kind::Bullseye);
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
            Some(Kind::Misc)
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
        Some(Kind::Misc)
    }
}

/// Properties that are never channels: identity and text, the transform,
/// object-id references other than `LockedTarget*`, and the four kept as
/// fixed columns.
const NOT_CHANNELS: &[&str] = &[
    "T",
    "Type",
    "Name",
    "Pilot",
    "Group",
    "Color",
    "Coalition",
    "CallSign",
    "Country",
    "Parent",
    "Next",
    "Registration",
    "Squawk",
    "Label",
    "ICAO24",
    "FocusedTarget",
    "Shape",
    "Debug",
    "ShortName",
    "LongName",
    "FullName",
    "IAS",
    "Mach",
    "AOA",
    "AGL",
];

/// `LockedTarget`, `LockedTarget2` .. `LockedTarget9`: an object id, stored
/// as the target's meta index.
pub(crate) fn is_lock_channel(name: &str) -> bool {
    match name.strip_prefix("LockedTarget") {
        Some("") => true,
        Some(d) => d.len() == 1 && matches!(d.as_bytes()[0], b'2'..=b'9'),
        None => false,
    }
}

/// value = stored integer x scale. A trailing digit (`Throttle2`,
/// `EngagementRange2`, `FuelWeight3`) is ignored.
pub(crate) fn channel_scale(name: &str) -> f64 {
    if is_lock_channel(name) {
        return 1.0;
    }
    let n = name.strip_suffix(|c: char| c.is_ascii_digit()).unwrap_or(name);
    let ends = |s: &[&str]| s.iter().any(|x| n.ends_with(x));
    if n.ends_with("Mode")
        || matches!(n, "LandingGear" | "Tailhook" | "Afterburner" | "AirBrakes" | "Flaps" | "Health")
        || n.starts_with("Throttle")
    {
        0.001
    } else if matches!(n, "HDG" | "HDM" | "AOS") || ends(&["Azimuth", "Elevation", "Roll", "Pitch", "Yaw", "Beamwidth"])
    {
        0.01
    } else if n.ends_with("Range") || matches!(n, "Radius" | "Length" | "Width" | "Height") {
        0.1
    } else if matches!(n, "TAS" | "CAS") {
        0.01
    } else if n.contains("GForce") {
        0.001
    } else if n.starts_with("FuelWeight") || n.starts_with("FuelVolume") {
        0.1
    } else {
        0.001
    }
}

/// Largest stored channel integer: any two differ by less than i32::MAX.
const CH_LIM: f64 = (i32::MAX / 2) as f64;

fn quantize(v: f64, scale: f64) -> i64 {
    let q = (v / scale).round();
    if q.is_nan() {
        0
    } else {
        q.clamp(-CH_LIM, CH_LIM) as i64
    }
}

/// A channel known to the recording.
struct ChanDef {
    name: String,
    scale: f64,
    lock: bool,
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
    /// Tacview object id.
    tid: u64,
    kind: Option<Kind>,
    /// Raw `Type=` tags.
    ty: Option<String>,
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
    // ── channels: every other numeric property (see the module docs) ──
    /// Channel ids (into `Builder::chans`), in the order first seen.
    ch_ids: Vec<u32>,
    /// Current value of each; for lock channels the target's sid, or -1.
    ch_cur: Vec<f64>,
    /// What samples before the first set hold.
    ch_first: Vec<f64>,
    /// One column per channel, aligned with `buf`.
    ch_buf: Vec<Vec<f64>>,
    /// Channel values of `carry` (shorter when channels came after it).
    carry_ch: Vec<f64>,
}

impl Obj {
    fn interval(&self) -> i64 {
        self.kind.map(Kind::interval_ms).unwrap_or(500)
    }

    fn emit(&mut self) {
        // Two states at the same moment: the later one stands.
        if let Some(last) = self.buf.last_mut().filter(|l| l.t == self.cur.t) {
            *last = self.cur;
            for (col, v) in self.ch_buf.iter_mut().zip(&self.ch_cur) {
                if let Some(x) = col.last_mut() {
                    *x = *v;
                }
            }
        } else {
            self.buf.push(self.cur);
            for (col, v) in self.ch_buf.iter_mut().zip(&self.ch_cur) {
                col.push(*v);
            }
        }
        self.last_emit = self.cur.t;
        self.emitted = true;
        self.pending = false;
    }

    fn set_channel(&mut self, id: u32, v: f64, lock: bool) {
        match self.ch_ids.iter().position(|c| *c == id) {
            Some(i) => self.ch_cur[i] = v,
            None => {
                let before = if lock { -1.0 } else { v };
                self.ch_ids.push(id);
                self.ch_cur.push(v);
                self.ch_first.push(before);
                self.ch_buf.push(vec![before; self.buf.len()]);
            }
        }
    }
}

/// Ingest logic version. Recordings processed by an older one are read again
/// (bumped when what we derive from a recording changes, or when a file an
/// older one gave up on can now be read -- 3: cut-off zips; 4: effects kept,
/// native sample rates, channels, BFR2 chunks, Tacview events passed through).
pub(crate) const INGEST_VERSION: u32 = 4;

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

/// A Tacview `Event=` line passed through as it is: message, bookmark, hit,
/// left the area.
struct TvEv {
    t: i64,
    k: &'static str,
    /// The first object it names that we know.
    sid: Option<u32>,
    text: Option<String>,
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
    tv_events: Vec<TvEv>,
    /// Every channel seen in the recording, and name -> its id.
    chans: Vec<ChanDef>,
    chan_of: FxHashMap<String, u32>,
}

/// The id of channel `name`, registered on first sight. `None` for a name
/// the format cannot hold (over 255 bytes, or past 65535 channels).
fn chan_id(chans: &mut Vec<ChanDef>, chan_of: &mut FxHashMap<String, u32>, name: &str) -> Option<u32> {
    if let Some(id) = chan_of.get(name) {
        return Some(*id);
    }
    if name.len() > 255 || chans.len() >= u16::MAX as usize {
        return None;
    }
    let id = chans.len() as u32;
    chans.push(ChanDef { name: name.to_string(), scale: channel_scale(name), lock: is_lock_channel(name) });
    chan_of.insert(name.to_string(), id);
    Some(id)
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
            Some(k @ ("Message" | "Bookmark" | "Hit" | "LeftArea")) => {
                self.tv_event(k, v);
                return;
            }
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
        if o.kind.map(Kind::is_effect).unwrap_or(false) {
            return;
        }
        if kind == EvKind::Destroyed {
            if o.destroyed.is_some() {
                return;
            }
            o.destroyed = Some((now, o.cur));
        }
        self.events.push(Ev { t: now, kind, sid });
    }

    /// `Message|ids...|text`, `Bookmark|text`, `Hit|id|...`, `LeftArea|id|`:
    /// the ids are the fields between the first and the last (for Hit and
    /// LeftArea written without the trailing `|`, the second field), the
    /// text is the last field.
    fn tv_event(&mut self, head: &str, v: &str) {
        let parts: Vec<&str> = v.split('|').collect();
        let (k, id_only) = match head {
            "Message" => ("message", false),
            "Bookmark" => ("bookmark", false),
            "Hit" => ("hit", true),
            _ => ("left", true),
        };
        let ids: &[&str] = if parts.len() >= 3 {
            &parts[1..parts.len() - 1]
        } else if id_only && parts.len() == 2 {
            &parts[1..]
        } else {
            &[]
        };
        let sid = ids
            .iter()
            .filter_map(|id| u64::from_str_radix(id.trim(), 16).ok())
            .find_map(|tid| self.alive.get(&tid).copied());
        let text = if parts.len() >= 2 { parts.last().map(|s| s.trim()).filter(|s| !s.is_empty()) } else { None };
        let text = text.filter(|_| !(id_only && parts.len() == 2)).map(str::to_string);
        self.tv_events.push(TvEv { t: self.now, k, sid, text });
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
            if o.skip || !o.positioned || !k.is_target() {
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
                self.objs.push(Obj {
                    tid,
                    t0: self.now,
                    t1: self.now,
                    alive: true,
                    last_emit: i64::MIN,
                    ..Default::default()
                });
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
                        o.ch_buf = Vec::new();
                        return;
                    }
                    o.ty = Some(v.into_owned());
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
                    let (flag, set): (u8, fn(&mut S, f64)) = match k {
                        "IAS" => (F_IAS, |s, x| s.ias = x),
                        "Mach" => (F_MACH, |s, x| s.mach = x),
                        "AOA" => (F_AOA, |s, x| s.aoa = x),
                        _ => (F_AGL, |s, x| s.agl = x),
                    };
                    if o.flags & flag == 0 {
                        // Samples from before it was known hold its first value.
                        for s in o.buf.iter_mut().chain(o.carry.as_mut()) {
                            set(s, x);
                        }
                    }
                    set(&mut o.cur, x);
                    o.flags |= flag;
                    moved = true;
                }
                k if NOT_CHANNELS.contains(&k) => (),
                k => {
                    let lock = is_lock_channel(k);
                    let x = if lock {
                        match u64::from_str_radix(v.trim(), 16) {
                            Ok(0) | Err(_) => -1.0,
                            Ok(t) => self.alive.get(&t).map_or(-1.0, |s| *s as f64),
                        }
                    } else {
                        match v.trim().parse::<f64>() {
                            Ok(x) if x.is_finite() => x,
                            _ => continue,
                        }
                    };
                    let Some(id) = chan_id(&mut self.chans, &mut self.chan_of, k) else {
                        continue;
                    };
                    o.set_channel(id, x, lock);
                    moved = true;
                }
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
                if let Some(x) = tf.roll {
                    o.cur.roll = x;
                }
                if let Some(x) = tf.pitch {
                    o.cur.pitch = x;
                }
                if let Some(x) = tf.yaw {
                    o.cur.yaw = x;
                }
                if o.flags & F_ATT == 0 {
                    let c = o.cur;
                    for s in o.buf.iter_mut().chain(o.carry.as_mut()) {
                        (s.roll, s.pitch, s.yaw) = (c.roll, c.pitch, c.yaw);
                    }
                }
                o.flags |= F_ATT;
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
            if o.skip || !o.positioned || !k.is_target() {
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
        // First who is written (giving each its meta index), then what:
        // a lock channel names its target by index, so every object in
        // the chunk must have one before any is encoded.
        let mut writing: Vec<u32> = vec![];
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
            o.idx.get_or_insert_with(|| {
                let i = self.next_idx;
                self.next_idx += 1;
                i
            });
            writing.push(sid);
            if o.alive {
                still.push(sid);
            }
        }
        self.active = still;
        let mut entries: Vec<Entry> = Vec::with_capacity(writing.len());
        for sid in writing {
            let o = &mut self.objs[sid as usize];
            let carry = o.carry.filter(|c| o.buf.first().map(|f| c.t < f.t).unwrap_or(true));
            let mut samples = Vec::with_capacity(o.buf.len() + 1);
            samples.extend(carry);
            samples.append(&mut o.buf);
            let mut cols: Vec<(u32, Vec<f64>)> = Vec::with_capacity(o.ch_ids.len());
            for (i, buf) in o.ch_buf.iter_mut().enumerate() {
                let mut col = Vec::with_capacity(samples.len());
                if carry.is_some() {
                    col.push(o.carry_ch.get(i).copied().unwrap_or(o.ch_first[i]));
                }
                col.append(buf);
                cols.push((o.ch_ids[i], col));
            }
            o.carry = samples.last().copied();
            o.carry_ch = cols.iter().map(|(_, c)| c.last().copied().unwrap_or(-1.0)).collect();
            let (idx, flags) = (o.idx.unwrap_or_default(), o.flags);
            let objs = &self.objs;
            let ch = cols
                .into_iter()
                .map(|(id, col)| {
                    let d = &self.chans[id as usize];
                    let vals = if d.lock {
                        col.iter()
                            .map(|v| match *v {
                                v if v < 0.0 => -1,
                                v => objs.get(v as usize).and_then(|t| t.idx).map_or(-1, i64::from),
                            })
                            .collect()
                    } else {
                        col.iter().map(|v| quantize(*v, d.scale)).collect()
                    };
                    Chan { name: d.name.clone(), scale: d.scale, vals }
                })
                .collect();
            entries.push(Entry { idx, flags, cols: base_cols(flags, &samples), ch });
        }
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
        for ev in &self.tv_events {
            events.push(json!({ "t": ev.t, "k": ev.k, "o": ev.sid.and_then(idx_of), "text": ev.text }));
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
                    "ty": o.ty,
                    "id": format!("{:x}", o.tid),
                })
            })
            .collect();

        let mut flights = vec![];
        for o in &by_idx {
            let (Some(i), Some(k)) = (o.idx, o.kind) else { continue };
            if !k.is_air() {
                continue;
            }
            let Some(pilot) = o.pilot.as_deref().map(str::trim).filter(|p| !p.is_empty()) else {
                continue;
            };
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
        Ok(Built { title: self.title, start_ms, duration_ms: end, objects: objects.len(), chunks, flights })
    }
}

fn write_gz(path: &Path, bytes: &[u8]) -> Result<()> {
    let f = fs::File::create(path).with_context(|| format!("creating {}", path.display()))?;
    let mut gz = GzEncoder::new(std::io::BufWriter::new(f), Compression::new(6));
    gz.write_all(bytes)?;
    gz.finish()?.flush()?;
    Ok(())
}

/// The fixed columns of `s` with these flags, as absolute integers.
fn base_cols(flags: u8, s: &[S]) -> Vec<Vec<i64>> {
    let col = |f: fn(&S) -> f64, k: f64| s.iter().map(|x| (f(x) * k).round() as i64).collect::<Vec<i64>>();
    let mut v = vec![s.iter().map(|x| x.t).collect(), col(|x| x.lon, 1e7), col(|x| x.lat, 1e7), col(|x| x.alt, 10.0)];
    if flags & F_ATT != 0 {
        v.push(col(|x| x.roll, 100.0));
        v.push(col(|x| x.pitch, 100.0));
        v.push(col(|x| x.yaw, 100.0));
    }
    if flags & F_IAS != 0 {
        v.push(col(|x| x.ias, 100.0));
    }
    if flags & F_MACH != 0 {
        v.push(col(|x| x.mach, 1000.0));
    }
    if flags & F_AOA != 0 {
        v.push(col(|x| x.aoa, 100.0));
    }
    if flags & F_AGL != 0 {
        v.push(col(|x| x.agl, 10.0));
    }
    v
}

fn write_chunk(path: &Path, entries: &[Entry]) -> Result<()> {
    write_gz(path, &encode_entries(entries))
}

/// Number of fixed i32 columns an entry with these flags carries.
fn ncols(flags: u8) -> usize {
    4 + if flags & F_ATT != 0 { 3 } else { 0 }
        + [F_IAS, F_MACH, F_AOA, F_AGL].iter().filter(|f| flags & **f != 0).count()
}

/// One channel column of a chunk entry: absolute integers, value = int x scale.
#[derive(Debug, Clone)]
struct Chan {
    name: String,
    scale: f64,
    vals: Vec<i64>,
}

/// One chunk entry: object index, flags, the fixed columns (absolute
/// integers, in the order the flags give) and the channels.
#[derive(Debug, Clone)]
struct Entry {
    idx: u32,
    flags: u8,
    cols: Vec<Vec<i64>>,
    ch: Vec<Chan>,
}

/// Encode entries as a `BFR2` chunk (not yet gzip'd).
fn encode_entries(entries: &[Entry]) -> Vec<u8> {
    let mut table: Vec<(&str, f64)> = vec![];
    let mut at: FxHashMap<&str, u16> = FxHashMap::default();
    for e in entries {
        for c in &e.ch {
            if !at.contains_key(c.name.as_str()) && table.len() < u16::MAX as usize && c.name.len() <= 255 {
                at.insert(&c.name, table.len() as u16);
                table.push((&c.name, c.scale));
            }
        }
    }
    let body: usize = entries
        .iter()
        .map(|e| 11 + 2 * e.ch.len() + 4 * e.cols.first().map_or(0, Vec::len) * (e.cols.len() + e.ch.len()))
        .sum();
    let mut b: Vec<u8> = Vec::with_capacity(16 + table.len() * 24 + body);
    b.extend_from_slice(b"BFR2");
    b.extend_from_slice(&(table.len() as u16).to_le_bytes());
    for (name, scale) in &table {
        b.push(name.len() as u8);
        b.extend_from_slice(name.as_bytes());
        b.extend_from_slice(&(*scale as f32).to_le_bytes());
    }
    b.extend_from_slice(&(entries.len() as u32).to_le_bytes());
    let col = |b: &mut Vec<u8>, c: &[i64]| {
        let mut prev = 0i64;
        for v in c {
            b.extend_from_slice(&((v - prev) as i32).to_le_bytes());
            prev = *v;
        }
    };
    for e in entries {
        let n = e.cols.first().map_or(0, Vec::len);
        let ch: Vec<&Chan> = e.ch.iter().filter(|c| at.contains_key(c.name.as_str())).collect();
        b.extend_from_slice(&e.idx.to_le_bytes());
        b.extend_from_slice(&(n as u32).to_le_bytes());
        b.push(e.flags);
        b.extend_from_slice(&(ch.len() as u16).to_le_bytes());
        for c in &ch {
            b.extend_from_slice(&at[c.name.as_str()].to_le_bytes());
        }
        for c in &e.cols {
            col(&mut b, c);
        }
        for c in &ch {
            col(&mut b, &c.vals);
        }
    }
    b
}

/// Bounds-checked little-endian reads.
struct Rd<'a> {
    b: &'a [u8],
    at: usize,
}

impl<'a> Rd<'a> {
    fn take(&mut self, n: usize) -> Result<&'a [u8]> {
        let end = self.at.checked_add(n).context("truncated chunk")?;
        let s = self.b.get(self.at..end).context("truncated chunk")?;
        self.at = end;
        Ok(s)
    }
    fn u8(&mut self) -> Result<u8> {
        Ok(self.take(1)?[0])
    }
    fn u16(&mut self) -> Result<u16> {
        Ok(u16::from_le_bytes(self.take(2)?.try_into()?))
    }
    fn u32(&mut self) -> Result<u32> {
        Ok(u32::from_le_bytes(self.take(4)?.try_into()?))
    }
    fn f32(&mut self) -> Result<f32> {
        Ok(f32::from_le_bytes(self.take(4)?.try_into()?))
    }
}

/// Decode a chunk's (inflated) bytes, `BFR2` or the older `BFR1`, keeping
/// only object `want` if given.
fn read_chunk(b: &[u8], want: Option<u32>) -> Result<Vec<Entry>> {
    let mut r = Rd { b, at: 0 };
    let v2 = match r.take(4) {
        Ok(b"BFR2") => true,
        Ok(b"BFR1") => false,
        _ => bail!("not a replay chunk"),
    };
    let mut table: Vec<(String, f64)> = vec![];
    if v2 {
        for _ in 0..r.u16()? {
            let len = r.u8()? as usize;
            let name = String::from_utf8_lossy(r.take(len)?).into_owned();
            table.push((name, r.f32()? as f64));
        }
    }
    let nobj = r.u32()?;
    let mut out = vec![];
    for _ in 0..nobj {
        let idx = r.u32()?;
        let n = r.u32()? as usize;
        let flags = r.u8()?;
        let mut mine = vec![];
        if v2 {
            for _ in 0..r.u16()? {
                let c = r.u16()? as usize;
                if c >= table.len() {
                    bail!("bad channel in replay chunk");
                }
                mine.push(c);
            }
        }
        let base = ncols(flags);
        let len = (base + mine.len()).checked_mul(n).and_then(|x| x.checked_mul(4)).context("truncated chunk")?;
        let body = r.take(len)?;
        if want.map(|w| w == idx).unwrap_or(true) {
            let col = |c: usize| -> Vec<i64> {
                let mut acc = 0i64;
                (0..n)
                    .map(|i| {
                        let o = (c * n + i) * 4;
                        acc += i32::from_le_bytes(body[o..o + 4].try_into().unwrap()) as i64;
                        acc
                    })
                    .collect()
            };
            let cols = (0..base).map(col).collect();
            let ch = mine
                .iter()
                .enumerate()
                .map(|(j, &c)| Chan { name: table[c].0.clone(), scale: table[c].1, vals: col(base + j) })
                .collect();
            out.push(Entry { idx, flags, cols, ch });
        }
    }
    Ok(out)
}

/// The flag each fixed column belongs to (0: always there), in column order.
const SLOT_FLAG: [u8; 11] = [0, 0, 0, 0, F_ATT, F_ATT, F_ATT, F_IAS, F_MACH, F_AOA, F_AGL];

/// One object's track being stitched from chunks: every fixed column it
/// ever had and every channel, all `n` long.
#[derive(Default)]
struct Stitch {
    n: usize,
    slots: [Option<Vec<i64>>; 11],
    ch: Vec<Chan>,
}

impl Stitch {
    /// Append a chunk's part after the samples already held. A column or
    /// channel one side lacks is filled with the nearest known value of it
    /// (`-1` for a lock channel).
    fn add(&mut self, e: Entry) {
        let mut it = e.cols.into_iter();
        let mut part: [Option<Vec<i64>>; 11] = Default::default();
        for (slot, f) in part.iter_mut().zip(SLOT_FLAG) {
            if f == 0 || e.flags & f != 0 {
                *slot = it.next();
            }
        }
        let Some(t) = part[0].as_ref() else { return };
        let last_t = self.slots[0].as_ref().and_then(|c| c.last().copied());
        let skip = last_t.map_or(0, |lt| t.iter().take_while(|x| **x <= lt).count());
        let m = t.len() - skip;
        if m == 0 {
            return;
        }
        let n = self.n;
        for (mine, theirs) in self.slots.iter_mut().zip(part) {
            match (mine.as_mut(), theirs) {
                (Some(a), Some(b)) => a.extend_from_slice(&b[skip..]),
                (Some(a), None) => {
                    let fill = a.last().copied().unwrap_or(0);
                    a.resize(n + m, fill);
                }
                (None, Some(b)) => {
                    let mut c = vec![b[skip]; n];
                    c.extend_from_slice(&b[skip..]);
                    *mine = Some(c);
                }
                (None, None) => (),
            }
        }
        for c in e.ch {
            let lock = is_lock_channel(&c.name);
            match self.ch.iter_mut().find(|x| x.name == c.name) {
                Some(x) => {
                    let rescale = !lock && (x.scale as f32) != (c.scale as f32);
                    let (from, to) = (c.scale, x.scale);
                    x.vals.extend(c.vals[skip..].iter().map(
                        |v| {
                            if rescale {
                                quantize(*v as f64 * from, to)
                            } else {
                                *v
                            }
                        },
                    ));
                }
                None => {
                    let fill = if lock { -1 } else { c.vals[skip] };
                    let mut vals = vec![fill; n];
                    vals.extend_from_slice(&c.vals[skip..]);
                    self.ch.push(Chan { name: c.name, scale: c.scale, vals });
                }
            }
        }
        for x in &mut self.ch {
            if x.vals.len() < n + m {
                let fill = if is_lock_channel(&x.name) { -1 } else { x.vals.last().copied().unwrap_or(0) };
                x.vals.resize(n + m, fill);
            }
        }
        self.n = n + m;
    }

    fn entry(self, idx: u32) -> Option<Entry> {
        if self.n == 0 {
            return None;
        }
        let mut flags = 0;
        for (s, f) in self.slots.iter().zip(SLOT_FLAG) {
            if s.is_some() {
                flags |= f;
            }
        }
        Some(Entry { idx, flags, cols: self.slots.into_iter().flatten().collect(), ch: self.ch })
    }
}

/// One object's whole track, stitched from the chunks `first..=last`, in the
/// chunk format (one entry). Written once to `o<idx>.bin.gz` and reused;
/// returns that file's path. Chunks that disagree on which columns or
/// channels the object has are joined on the union of them
/// ([`Stitch::add`]).
pub(crate) fn object_track(dir: &Path, idx: u32, first: u32, last: u32) -> Result<PathBuf> {
    use flate2::read::GzDecoder;
    use std::io::Read;
    let path = dir.join(format!("o{idx}.bin.gz"));
    if path.exists() {
        return Ok(path);
    }
    let mut merged = Stitch::default();
    for n in first..=last {
        let p = dir.join(format!("c{n}.bin.gz"));
        let Ok(f) = fs::File::open(&p) else { continue };
        let mut raw = vec![];
        GzDecoder::new(f).read_to_end(&mut raw)?;
        for e in read_chunk(&raw, Some(idx))? {
            merged.add(e);
        }
    }
    let entries: Vec<Entry> = merged.entry(idx).into_iter().collect();
    // Write via a temp name so a concurrent request never reads half a file.
    let tmp = dir.join(format!("o{idx}.bin.gz.{}", std::process::id()));
    write_chunk(&tmp, &entries)?;
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
        tv_events: vec![],
        chans: vec![],
        chan_of: FxHashMap::default(),
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
        assert_eq!(classify("Projectile+Shell"), Some(Kind::Shell));
        assert_eq!(classify("Weapon+Bullet"), Some(Kind::Shell));
        assert_eq!(classify("Misc+Decoy+Flare"), Some(Kind::Flare));
        assert_eq!(classify("Misc+Decoy+Chaff"), Some(Kind::Chaff));
        assert_eq!(classify("Misc+Decoy"), Some(Kind::Decoy));
        assert_eq!(classify("Misc+Explosion"), Some(Kind::Explosion));
        assert_eq!(classify("Ground+Light+Human+Air+Parachutist"), Some(Kind::Parachutist));
        assert_eq!(classify("Misc+SmokeGrenade"), Some(Kind::Misc));
        assert_eq!(classify("Navaid+Static+Waypoint"), Some(Kind::Misc));
        assert_eq!(classify("Misc+Shrapnel"), None);
        assert_eq!(classify("Misc+Beam"), None);
        assert_eq!(classify("Ground+AntiAircraft"), Some(Kind::Sam));
        assert_eq!(classify("Ground+Heavy+Armor+Vehicle+Tank"), Some(Kind::Armor));
        assert_eq!(classify("Sea+Watercraft+AircraftCarrier"), Some(Kind::Carrier));
        assert_eq!(classify("Navaid+Static+Bullseye"), Some(Kind::Bullseye));
        for k in [
            Kind::Shell,
            Kind::Flare,
            Kind::Chaff,
            Kind::Decoy,
            Kind::Explosion,
            Kind::Parachutist,
            Kind::Bullseye,
            Kind::Misc,
        ] {
            assert!(k.is_effect() && !k.is_target() && !k.is_weapon() && !k.is_air(), "{k:?}");
        }
    }

    #[test]
    fn channel_scales() {
        assert!(is_lock_channel("LockedTarget") && is_lock_channel("LockedTarget9"));
        assert!(
            !is_lock_channel("LockedTargetMode")
                && !is_lock_channel("LockedTargetRange")
                && !is_lock_channel("LockedTarget1")
        );
        let cases = [
            ("LockedTarget", 1.0),
            ("LockedTarget3", 1.0),
            ("LockedTargetMode", 0.001),
            ("RadarMode", 0.001),
            ("LandingGear", 0.001),
            ("Throttle2", 0.001),
            ("Health", 0.001),
            ("HDG", 0.01),
            ("AOS", 0.01),
            ("RadarAzimuth", 0.01),
            ("LockedTargetElevation", 0.01),
            ("PilotHeadRoll", 0.01),
            ("RadarHorizontalBeamwidth", 0.01),
            ("RadarRange", 0.1),
            ("EngagementRange2", 0.1),
            ("Radius", 0.1),
            ("Height", 0.1),
            ("TAS", 0.01),
            ("CAS", 0.01),
            ("VerticalGForce", 0.001),
            ("FuelWeight", 0.1),
            ("FuelVolume4", 0.1),
            ("Visible", 0.001),
        ];
        for (n, s) in cases {
            assert_eq!(channel_scale(n), s, "{n}");
        }
        assert_eq!(quantize(1e12, 0.001), (i32::MAX / 2) as i64);
        assert_eq!(quantize(-1e12, 0.001), -((i32::MAX / 2) as i64));
    }

    fn chunk(p: &Path) -> Vec<Entry> {
        read_chunk(&gunzip(p), None).unwrap()
    }

    /// Channel values of an entry, as real values.
    fn chan(e: &Entry, name: &str) -> Vec<f64> {
        let c = e.ch.iter().find(|c| c.name == name).unwrap_or_else(|| panic!("no {name} in {:?}", e.ch));
        c.vals.iter().map(|v| *v as f64 * c.scale).collect()
    }

    /// A tiny recording from header lines and `(seconds, lines)` frames.
    fn record(dir: &Path, frames: &[(f64, String)]) -> PathBuf {
        let _ = fs::remove_dir_all(dir);
        fs::create_dir_all(dir).unwrap();
        let mut t = String::from(
            "FileType=text/acmi/tacview\nFileVersion=2.2\n\
             0,ReferenceTime=2026-10-08T10:00:00Z,RecordingTime=2026-10-08T12:00:00Z,\
             ReferenceLongitude=41,ReferenceLatitude=42\n",
        );
        for (ts, lines) in frames {
            t.push_str(&format!("#{ts}\n{lines}"));
        }
        let src = dir.join("r.txt.acmi");
        fs::write(&src, t).unwrap();
        let out = dir.join("out");
        build(&src, &out).unwrap();
        out
    }

    fn meta_of(out: &Path) -> Value {
        serde_json::from_slice(&gunzip(&out.join("meta.json.gz"))).unwrap()
    }

    fn idx_by_id(meta: &Value, id: &str) -> u32 {
        meta["objects"].as_array().unwrap().iter().position(|o| o["id"] == id).unwrap() as u32
    }

    /// Numeric properties come back as channels with their scale; one first
    /// set mid-chunk holds its first value before that; a byte-level read
    /// that follows bfweb's `decodeChunk` step by step agrees.
    #[test]
    fn channels_round_trip() {
        let dir = std::env::temp_dir().join(format!("bfdb-replay-ch-{}", std::process::id()));
        let mut frames = vec![(
            0.0,
            "a1,T=0.1|0.1|5000|0|0|90,Type=Air+FixedWing,Name=F-16C_50,Pilot=Viper,Color=Blue,\
             TAS=250.25,FuelWeight=3000.5,HDG=90.5,Squawk=7700\n"
                .to_string(),
        )];
        for i in 1..=10 {
            let mut l = format!(
                "a1,T={}||,FuelWeight={},TAS={}",
                0.1 + i as f64 * 0.001,
                3000.5 - i as f64 * 1.5,
                250.25 + i as f64
            );
            if i == 5 {
                l.push_str(",Throttle=0.75,RadarRange=40000.3");
            }
            frames.push((i as f64 * 0.5, l + "\n"));
        }
        let out = record(&dir, &frames);
        let raw = gunzip(&out.join("c0.bin.gz"));
        let e = read_chunk(&raw, None).unwrap();
        assert_eq!(e.len(), 1);
        let e = &e[0];
        let n = e.cols[0].len();
        assert_eq!(n, 11, "one sample per frame: {:?}", e.cols[0]);
        let names: Vec<&str> = e.ch.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(names, ["TAS", "FuelWeight", "HDG", "Throttle", "RadarRange"], "Squawk is not a channel");
        let scale = |n: &str| e.ch.iter().find(|c| c.name == n).unwrap().scale;
        assert_eq!(
            (scale("TAS"), scale("FuelWeight"), scale("Throttle")),
            (0.01f32 as f64, 0.1f32 as f64, 0.001f32 as f64)
        );
        let fuel = chan(e, "FuelWeight");
        let tas = chan(e, "TAS");
        for i in 0..n {
            assert!((fuel[i] - (3000.5 - i as f64 * 1.5)).abs() < 0.06, "{fuel:?}");
            assert!((tas[i] - (250.25 + i as f64)).abs() < 0.006, "{tas:?}");
        }
        assert!(chan(e, "HDG").iter().all(|v| (v - 90.5).abs() < 0.006));
        assert!(chan(e, "Throttle").iter().all(|v| (v - 0.75).abs() < 1e-6), "first value before the first set");
        assert!(chan(e, "RadarRange").iter().all(|v| (v - 40000.3).abs() < 0.01));

        // The same bytes, read the way data.ts does.
        let mut at = 0usize;
        let u16_ = |at: &mut usize| {
            let v = u16::from_le_bytes(raw[*at..*at + 2].try_into().unwrap());
            *at += 2;
            v as usize
        };
        let u32_ = |at: &mut usize| {
            let v = u32::from_le_bytes(raw[*at..*at + 4].try_into().unwrap());
            *at += 4;
            v as usize
        };
        assert_eq!(&raw[..4], b"BFR2");
        at += 4;
        let nchan = u16_(&mut at);
        let mut table = vec![];
        for _ in 0..nchan {
            let len = raw[at] as usize;
            let name = std::str::from_utf8(&raw[at + 1..at + 1 + len]).unwrap().to_string();
            let scale = f32::from_le_bytes(raw[at + 1 + len..at + 5 + len].try_into().unwrap());
            table.push((name, scale));
            at += 1 + len + 4;
        }
        assert_eq!(table.len(), 5);
        assert_eq!(u32_(&mut at), 1, "nobj");
        assert_eq!(u32_(&mut at), 0, "idx");
        assert_eq!(u32_(&mut at), n, "n");
        let flags = raw[at];
        at += 1;
        assert_eq!(flags, F_ATT);
        let nch = u16_(&mut at);
        let mine: Vec<usize> = (0..nch).map(|_| u16_(&mut at)).collect();
        // t, lon, lat, alt, roll, pitch, yaw, then the channels
        at += 7 * n * 4;
        let fw = mine.iter().position(|c| table[*c].0 == "FuelWeight").unwrap();
        at += fw * n * 4;
        let first = i32::from_le_bytes(raw[at..at + 4].try_into().unwrap()) as f64 * table[mine[fw]].1 as f64;
        assert!((first - 3000.5).abs() < 0.06, "{first}");
        let _ = fs::remove_dir_all(&dir);
    }

    /// `LockedTarget` holds the target's meta index while locked and -1
    /// before the first lock and after the unlock.
    #[test]
    fn locked_target_resolves_to_meta_index() {
        let dir = std::env::temp_dir().join(format!("bfdb-replay-lock-{}", std::process::id()));
        let mut frames = vec![(
            0.0,
            "a1,T=0.1|0.1|5000|0|0|90,Type=Air+FixedWing,Name=F-16C_50,Pilot=Viper,Color=Blue\n\
             b2,T=0.3|0.1|5000|0|0|270,Type=Air+FixedWing,Name=MiG-29S,Pilot=Ivan,Color=Red\n"
                .to_string(),
        )];
        for i in 1..=10 {
            let mut l = format!("a1,T={}||\n", 0.1 + i as f64 * 0.001);
            if i == 2 {
                l = format!("a1,T={}||,LockedTarget=b2,LockedTargetRange=15000\n", 0.1 + i as f64 * 0.001);
            }
            if i == 6 {
                l = format!("a1,T={}||,LockedTarget=0\n", 0.1 + i as f64 * 0.001);
            }
            frames.push((i as f64 * 0.5, l));
        }
        let out = record(&dir, &frames);
        let meta = meta_of(&out);
        let (f16, mig) = (idx_by_id(&meta, "a1"), idx_by_id(&meta, "b2"));
        assert_eq!(meta["objects"][f16 as usize]["ty"], "Air+FixedWing");
        let c0 = chunk(&out.join("c0.bin.gz"));
        let e = c0.iter().find(|e| e.idx == f16).unwrap();
        let lock = chan(e, "LockedTarget");
        let ts = &e.cols[0];
        assert_eq!(e.ch.iter().find(|c| c.name == "LockedTarget").unwrap().scale, 1.0);
        for (t, v) in ts.iter().zip(&lock) {
            let want = if (1_000..3_000).contains(t) { mig as f64 } else { -1.0 };
            assert_eq!(*v, want, "at {t}: {lock:?} / {ts:?}");
        }
        let range = chan(e, "LockedTargetRange");
        assert!(range.iter().all(|r| (r - 15000.0).abs() < 0.01), "{range:?}");
        let _ = fs::remove_dir_all(&dir);
    }

    /// The whole track joins chunks whose columns and channels differ: what
    /// appears later is filled backwards with its first value (-1 for a
    /// lock), and nothing is dropped.
    #[test]
    fn object_track_stitches_changing_channels() {
        let dir = std::env::temp_dir().join(format!("bfdb-replay-stitch-{}", std::process::id()));
        let mut frames = vec![(
            0.0,
            "a1,T=0.1|0.1|5000|0|0|90,Type=Air+FixedWing,Name=F-16C_50,Pilot=Viper,Color=Blue,TAS=200\n\
             b2,T=0.3|0.1|5000,Type=Ground+Heavy+Armor+Vehicle+Tank,Name=T-72B,Color=Red\n"
                .to_string(),
        )];
        for i in 1..=8 {
            let ts = i as f64 * 50.0;
            let mut l = format!("a1,T={}||,TAS={}", 0.1 + i as f64 * 0.001, 200 + i);
            if ts > 300.0 {
                l.push_str(",FuelWeight=1000,AGL=50,LockedTarget=b2");
            }
            frames.push((ts, l + "\n"));
        }
        let out = record(&dir, &frames);
        let meta = meta_of(&out);
        let (f16, tank) = (idx_by_id(&meta, "a1"), idx_by_id(&meta, "b2"));
        let c0 = chunk(&out.join("c0.bin.gz"));
        let e0 = c0.iter().find(|e| e.idx == f16).unwrap();
        assert_eq!(e0.ch.len(), 1, "chunk 0 has TAS only");
        assert_eq!(e0.flags & F_AGL, 0);
        let path = object_track(&out, f16, 0, 1).unwrap();
        let e = chunk(&path);
        assert_eq!(e.len(), 1);
        let e = &e[0];
        let ts = &e.cols[0];
        assert_eq!(ts, &(0..=8).map(|i| i * 50_000).collect::<Vec<i64>>());
        assert_eq!(e.flags, F_ATT | F_AGL, "union of the chunks' flags");
        assert_eq!(e.cols.len(), ncols(e.flags));
        assert!(e.cols[7].iter().all(|v| *v == 500), "AGL backfilled: {:?}", e.cols[7]);
        let tas = chan(e, "TAS");
        assert!(tas.iter().enumerate().all(|(i, v)| (v - (200 + i) as f64).abs() < 0.006), "{tas:?}");
        assert!(chan(e, "FuelWeight").iter().all(|v| (v - 1000.0).abs() < 0.06));
        let lock = chan(e, "LockedTarget");
        for (t, v) in ts.iter().zip(&lock) {
            assert_eq!(*v, if *t > 300_000 { tank as f64 } else { -1.0 }, "{lock:?}");
        }
        let _ = fs::remove_dir_all(&dir);
    }

    /// Shells, flares and chaff right where a missile is launched and where
    /// it ends are neither its launcher nor its target, and the shot, hit
    /// and kill come out as without them.
    #[test]
    fn effects_are_never_shot_parties() {
        let dir = std::env::temp_dir().join(format!("bfdb-replay-fx-{}", std::process::id()));
        let mut frames = vec![(
            0.0,
            "a1,T=0.1|0.1|5000|0|0|90,Type=Air+FixedWing,Name=F-16C_50,Pilot=Viper,Color=Blue\n\
             b2,T=0.2|0.1|5000|0|0|270,Type=Air+FixedWing,Name=MiG-29S,Pilot=Ivan,Color=Red\n\
             a9,T=0.5|0.1|10,Type=Ground+AntiAircraft,Name=Tor 9A331,Color=Red\n"
                .to_string(),
        )];
        for i in 1..=100 {
            let t = i as f64 * 0.1;
            let mut l = format!("a1,T={}||\n", 0.1 + t * 0.00001);
            if i == 20 {
                // A SAM launch with a shell nearer the missile than the SAM
                // (no aircraft near, so the nearest anything would win).
                l.push_str("b8,T=0.50005|0.1|10,Type=Projectile+Shell,Color=Red\n");
                l.push_str("b9,T=0.50005|0.1|10,Type=Weapon+Missile,Name=9M331,Color=Red\n");
            }
            if i == 10 {
                // At the rail, there before the missile: closer to it than
                // the F-16 is.
                l.push_str("d4,T=0.1001|0.1|5000,Type=Projectile+Shell,Color=Red\n");
                l.push_str("e5,T=0.1001|0.1|5000,Type=Misc+Decoy+Flare,Color=Red\n");
                l.push_str("c3,T=0.1001|0.1|5000|0|0|90,Type=Weapon+Missile,Name=AIM-120C,Color=Blue\n");
            }
            if i > 10 {
                let lon = 0.1001 + (t - 1.0) * (0.0998 / 9.0);
                l.push_str(&format!("c3,T={lon}||\n"));
            }
            if i == 95 {
                // Where the missile ends, nearer to it than the MiG.
                l.push_str("f6,T=0.1999|0.1|5000,Type=Misc+Decoy+Flare,Color=Red\n");
                l.push_str("f7,T=0.1999|0.1|5000,Type=Misc+Decoy+Chaff,Color=Red\n");
                l.push_str("f8,T=0.1999|0.1|5000,Type=Misc+Explosion\n");
            }
            frames.push((t, l));
        }
        frames.push((10.5, "-c3\n".into()));
        frames.push((11.0, "-b2\n".into()));
        let out = record(&dir, &frames);
        let meta = meta_of(&out);
        let (f16, mig, aim) = (idx_by_id(&meta, "a1"), idx_by_id(&meta, "b2"), idx_by_id(&meta, "c3"));
        for id in ["d4", "e5", "f6", "f7", "f8"] {
            let k = &meta["objects"][idx_by_id(&meta, id) as usize]["k"];
            assert!(["shell", "flare", "chaff", "explosion"].contains(&k.as_str().unwrap()), "{id}: {k}");
        }
        assert_eq!(meta["objects"][aim as usize]["par"], f16, "launcher");
        let (sam, sam_msl) = (idx_by_id(&meta, "a9"), idx_by_id(&meta, "b9"));
        assert_eq!(meta["objects"][sam_msl as usize]["par"], sam, "the SAM, not the shell");
        let ev = meta["events"].as_array().unwrap();
        let fired: Vec<&Value> = ev.iter().filter(|e| e["k"] == "fired" && e["w"] == aim).collect();
        assert_eq!(fired.len(), 1);
        assert_eq!((fired[0]["o"].as_u64(), fired[0]["tg"].as_u64()), (Some(f16 as u64), Some(mig as u64)));
        assert_eq!(fired[0]["hit"], true);
        let kill = ev.iter().find(|e| e["k"] == "kill").expect("a kill");
        assert_eq!(
            (kill["o"].as_u64(), kill["by"].as_u64(), kill["w"].as_u64()),
            (Some(mig as u64), Some(f16 as u64), Some(aim as u64))
        );
        let _ = fs::remove_dir_all(&dir);
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
            } else if (150.0..170.0).contains(&ts) {
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
             0,Event=TakenOff|a1|\n\
             0,Event=Message|a1|Fox three\n\
             0,Event=Bookmark|Merge\n\
             0,Event=LeftArea|b2|\n",
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
        assert_eq!(built.objects, 4, "the shell is kept");

        let meta: Value = serde_json::from_slice(&gunzip(&out.join("meta.json.gz"))).unwrap();
        let objs = meta["objects"].as_array().unwrap();
        let idx = |name: &str| objs.iter().position(|o| o["n"] == name).unwrap() as u64;
        let (f16, mig, aim) = (idx("F-16C_50"), idx("MiG-29S"), idx("AIM-120C"));
        let shell = objs.iter().position(|o| o["k"] == "shell").unwrap() as u64;
        assert_eq!(
            (&objs[shell as usize]["ty"], &objs[shell as usize]["id"]),
            (&json!("Projectile+Shell"), &json!("d4"))
        );
        assert_eq!((&objs[f16 as usize]["ty"], &objs[f16 as usize]["id"]), (&json!("Air+FixedWing"), &json!("a1")));
        assert_eq!(objs[aim as usize]["par"], f16, "launcher found by proximity");
        let ev = meta["events"].as_array().unwrap();
        let tv = |k: &str| ev.iter().find(|e| e["k"] == k).unwrap_or_else(|| panic!("no {k} in {ev:?}"));
        assert_eq!((&tv("message")["o"], &tv("message")["text"]), (&json!(f16), &json!("Fox three")));
        assert_eq!((&tv("bookmark")["o"], &tv("bookmark")["text"]), (&Value::Null, &json!("Merge")));
        assert_eq!((&tv("left")["o"], &tv("left")["text"]), (&json!(mig), &Value::Null));
        let kill = ev.iter().find(|e| e["k"] == "kill").expect("a kill");
        assert_eq!((kill["o"].as_u64(), kill["by"].as_u64()), (Some(mig), Some(f16)));
        assert!(ev.iter().any(|e| e["k"] == "fired" && e["o"] == f16));
        assert!(ev.iter().any(|e| e["k"] == "takeoff" && e["o"] == f16));

        let viper = built.flights.iter().find(|f| f.pilot == "Viper").unwrap();
        assert_eq!((viper.shots, viper.kills, viper.fate), (1, 1, "landed"));
        let ivan = built.flights.iter().find(|f| f.pilot == "Ivan").unwrap();
        assert_eq!(ivan.fate, "destroyed");

        // Chunk 1 carries the F-16's last pre-window sample so it stands alone.
        assert_eq!(&gunzip(&out.join("c1.bin.gz"))[..4], b"BFR2");
        let c1 = chunk(&out.join("c1.bin.gz"));
        let mut live: Vec<u64> = c1.iter().map(|e| e.idx as u64).collect();
        live.sort();
        assert_eq!(live, vec![f16, shell], "only the F-16 and the never-removed shell are still alive");
        let e = c1.iter().find(|e| e.idx as u64 == f16).unwrap();
        assert_eq!(e.cols[0].len(), 2, "carry + the t=400 s sample");
        assert!(e.cols[0][0] < 300_000);

        // The stitched track has every F-16 sample exactly once, in order.
        let f16_track = object_track(&out, f16 as u32, 0, 1).unwrap();
        let e = read_chunk(&gunzip(&f16_track), None).unwrap();
        assert_eq!(e.len(), 1);
        let ts = &e[0].cols[0];
        assert!(ts.windows(2).all(|w| w[0] < w[1]), "strictly increasing: {ts:?}");
        assert_eq!(*ts.last().unwrap(), 400_000);
        assert_eq!(ts[0], 0);
        let _ = fs::remove_dir_all(&dir);
    }
}
