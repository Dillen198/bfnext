//! Training range configuration: `<Saved Games>/<sortie>_RANGE` (JSON), read
//! by the `bfrange` engine at mission start.
//!
//! The mission file only has to supply what a script cannot create: client
//! slots, carriers (players spawn on them) and trigger zones marking where
//! things go. Everything else -- targets, strafe pits, tankers, ships, SAM
//! sites, adversaries -- the engine spawns from this file, so a new range
//! layout is a config edit, not a mission rebuild.

use super::{RefuelMethod, StationKind};
use netidx::path::Path as NetidxPath;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::Path};

/// Where something goes: a trigger zone in the .miz (recommended -- placed in
/// the Mission Editor on real terrain), `{"zone": "RANGE-A"}`, or a lat/lon,
/// `{"lat": 42.2, "lon": 40.7}`.
///
/// A struct with optional fields rather than an untagged enum: serde's
/// untagged buffering does not round-trip floats under every serde_json
/// feature set the workspace can end up with, and a config that silently
/// fails to parse its coordinates is worse than a slightly plainer type.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Loc {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub zone: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lat: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lon: Option<f64>,
}

/// A point on the map, degrees.
#[derive(Debug, Clone, Copy, Default, PartialEq, Serialize, Deserialize)]
pub struct LatLon {
    pub lat: f64,
    pub lon: f64,
}

/// What a sector of the theatre is for. The mission draws each kind in its
/// own colour on the F10 map; the engine names it when a player flies in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SectorKind {
    AirToGround,
    Tactical,
    Threat,
    Gunnery,
    Helo,
    AirToAir,
    Bvr,
    Duel,
    Aar,
    Carrier,
    AntiShip,
}

impl SectorKind {
    pub fn label(&self) -> &'static str {
        match self {
            Self::AirToGround => "AIR-TO-GROUND",
            Self::Tactical => "TACTICAL / CAS",
            Self::Threat => "THREAT / SEAD",
            Self::Gunnery => "CA GUNNERY",
            Self::Helo => "HELICOPTER",
            Self::AirToAir => "AIR-TO-AIR",
            Self::Bvr => "BVR",
            Self::Duel => "DUELS",
            Self::Aar => "AIR REFUELLING",
            Self::Carrier => "CARRIER OPS",
            Self::AntiShip => "ANTI-SHIP",
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Serialize, Deserialize)]
pub struct SectorCircle {
    pub lat: f64,
    pub lon: f64,
    pub radius_m: f64,
}

/// A tanker race-track: the leg starts at `lat`/`lon` and runs `leg_m` along
/// `heading_deg`; the sector is everything within `width_m / 2` of that leg.
#[derive(Debug, Clone, Copy, Default, PartialEq, Serialize, Deserialize)]
pub struct SectorTrack {
    pub lat: f64,
    pub lon: f64,
    pub heading_deg: f64,
    pub leg_m: f64,
    pub width_m: f64,
}

/// Exactly one of the three. A struct of options rather than an enum for the
/// same reason as [`Loc`].
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SectorShape {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub polygon: Option<Vec<LatLon>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub circle: Option<SectorCircle>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub track: Option<SectorTrack>,
}

impl SectorShape {
    fn check(&self) -> Result<(), &'static str> {
        let n = self.polygon.is_some() as u8 + self.circle.is_some() as u8 + self.track.is_some() as u8;
        if n != 1 {
            return Err("needs exactly one of polygon, circle or track");
        }
        if let Some(p) = &self.polygon {
            if p.len() < 3 {
                return Err("a polygon needs at least 3 points");
            }
        }
        if let Some(c) = &self.circle {
            if !(c.radius_m > 0.) {
                return Err("a circle needs a positive radius_m");
            }
        }
        if let Some(t) = &self.track {
            if !(t.leg_m > 0.) || !(t.width_m > 0.) {
                return Err("a track needs a positive leg_m and width_m");
            }
        }
        Ok(())
    }
}

/// A part of the theatre with one job: a bombing range, a fight area, a
/// tanker track, the carrier's operating area... The mission draws the same
/// sectors on the F10 map; the engine lists them on F10 > Range > Sectors and
/// tells a player what a sector is for when they fly into it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SectorCfg {
    /// `[a-z0-9-]`, e.g. "r-1".
    pub id: String,
    /// As drawn on the map, e.g. "R-1 SAMGORI".
    pub name: String,
    pub kind: SectorKind,
    /// Who it is laid out for: "blue", "red" or "all".
    #[serde(default = "default_sector_side")]
    pub side: String,
    /// One line of what is in it.
    #[serde(default)]
    pub purpose: String,
    pub shape: SectorShape,
    /// Tell a player what the sector is for when they fly into it.
    #[serde(default = "yes")]
    pub announce: bool,
}

fn default_sector_side() -> String {
    "all".into()
}

/// What a `Loc` resolves to.
pub enum LocKind<'a> {
    Zone(&'a str),
    LatLon(f64, f64),
    Invalid,
}

impl Loc {
    pub fn zone(z: &str) -> Self {
        Self { zone: Some(z.to_string()), lat: None, lon: None }
    }

    pub fn latlon(lat: f64, lon: f64) -> Self {
        Self { zone: None, lat: Some(lat), lon: Some(lon) }
    }

    pub fn kind(&self) -> LocKind<'_> {
        match (&self.zone, self.lat, self.lon) {
            (Some(z), _, _) => LocKind::Zone(z),
            (None, Some(lat), Some(lon)) => LocKind::LatLon(lat, lon),
            _ => LocKind::Invalid,
        }
    }

    pub fn describe(&self) -> String {
        match self.kind() {
            LocKind::Zone(z) => format!("zone {z:?}"),
            LocKind::LatLon(lat, lon) => format!("{lat:.4},{lon:.4}"),
            LocKind::Invalid => "an empty location".into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ObjCategory {
    Static,
    Vehicle,
    Ship,
}

impl Default for ObjCategory {
    fn default() -> Self {
        Self::Static
    }
}

/// One target object at a station.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetCfg {
    /// DCS type, e.g. "Container red 1", "T-72B", "Hangar A".
    pub typ: String,
    #[serde(default)]
    pub category: ObjCategory,
    /// Offset from the station centre, metres, in the station's own frame
    /// (x = along `heading_deg`, y = right of it).
    #[serde(default)]
    pub x: f64,
    #[serde(default)]
    pub y: f64,
    /// Display name; defaults to the type.
    #[serde(default)]
    pub name: Option<String>,
    /// Extra heading relative to the station, degrees.
    #[serde(default)]
    pub heading_deg: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StrafePitCfg {
    /// Box around the target the pass is scored in, along the run-in heading.
    #[serde(default = "default_pit_length")]
    pub box_length_m: f64,
    #[serde(default = "default_pit_width")]
    pub box_width_m: f64,
    /// Firing inside this slant range invalidates the pass.
    #[serde(default = "default_foul_line")]
    pub foul_line_m: f64,
    /// Above this, the aircraft is not considered to be on a strafe run.
    #[serde(default = "default_pit_alt")]
    pub max_alt_agl_m: f64,
}

fn default_pit_length() -> f64 {
    3000.
}
fn default_pit_width() -> f64 {
    300.
}
fn default_foul_line() -> f64 {
    610.
}
fn default_pit_alt() -> f64 {
    914.
}

impl Default for StrafePitCfg {
    fn default() -> Self {
        Self {
            box_length_m: default_pit_length(),
            box_width_m: default_pit_width(),
            foul_line_m: default_foul_line(),
            max_alt_agl_m: default_pit_alt(),
        }
    }
}

/// A moving target: the group drives/sails between these points and loops.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteCfg {
    pub points: Vec<Loc>,
    #[serde(default = "default_route_speed")]
    pub speed_kts: f64,
    /// Drive on roads between points (ground only).
    #[serde(default)]
    pub on_road: bool,
}

fn default_route_speed() -> f64 {
    15.
}

/// One air-to-ground (or ship / SAM / gunnery) station.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StationCfg {
    /// Stable id, used in URLs and results. `[a-z0-9-]`.
    pub id: String,
    pub name: String,
    pub kind: StationKind,
    pub loc: Loc,
    /// Orientation of the target layout, degrees true.
    #[serde(default)]
    pub heading_deg: f64,
    #[serde(default)]
    pub targets: Vec<TargetCfg>,
    /// Scoring rings drawn on the F10 map and on the result card, metres.
    #[serde(default)]
    pub rings_m: Vec<f64>,
    /// Scoring radius a weapon is tracked to this station from.
    #[serde(default = "default_station_radius")]
    pub radius_m: f64,
    #[serde(default)]
    pub strafe: Option<StrafePitCfg>,
    /// An AI laser designator is kept on the station's first target.
    #[serde(default)]
    pub laser_code: Option<u16>,
    /// Respawn destroyed targets after this long. `None` = only on request.
    #[serde(default = "default_respawn")]
    pub respawn_s: Option<u32>,
    /// Moving targets (convoys, ships).
    #[serde(default)]
    pub route: Option<RouteCfg>,
    /// Coalition the targets belong to (so blue AND red players can bomb
    /// them, targets default to neutral-ish red: "red").
    #[serde(default = "default_target_side")]
    pub side: String,
    /// Country for spawned target objects (DCS country name).
    #[serde(default = "default_target_country")]
    pub country: String,
    /// SAM / AAA stations: may the site actually shoot? When false the site
    /// tracks with radar on and weapons held (no launch). When true it fires
    /// and the missile trainer protects players.
    #[serde(default)]
    pub weapons_free: bool,
    /// Free-text shown on the live page ("LGB only", "Cat II laser").
    #[serde(default)]
    pub note: Option<String>,
    /// Only instructors can reset / reconfigure this station.
    #[serde(default)]
    pub locked: bool,
}

fn default_station_radius() -> f64 {
    1000.
}
fn default_respawn() -> Option<u32> {
    Some(300)
}
fn default_target_side() -> String {
    "red".into()
}
fn default_target_country() -> String {
    "CJTF Red".into()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum TacanBand {
    X,
    Y,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TacanCfg {
    pub channel: u8,
    pub band: TacanBand,
    /// Morse identifier, 3 letters.
    pub morse: String,
}

impl TacanCfg {
    pub fn describe(&self) -> String {
        format!("{}{} {}", self.channel, match self.band {
            TacanBand::X => "X",
            TacanBand::Y => "Y",
        }, self.morse)
    }

    /// The frequency DCS wants in ActivateBeacon, in Hz.
    ///
    /// TACAN frequency = (A + channel - B) * 1e6 where for X: A = 962 below
    /// channel 64, 1151 from 64; for Y: A = 1088 below 64, 1025 from 64; and
    /// B = 1 below 64, 64 from 64.
    pub fn frequency_hz(&self) -> f64 {
        let ch = self.channel as f64;
        let (a, b) = match (self.band, self.channel < 64) {
            (TacanBand::X, true) => (962., 1.),
            (TacanBand::X, false) => (1151., 64.),
            (TacanBand::Y, true) => (1088., 1.),
            (TacanBand::Y, false) => (1025., 64.),
        };
        (a + ch - b) * 1e6
    }
}

/// A tanker track. `permanent` tankers are kept on station for the whole
/// mission (respawned if lost); the rest are spawnable on demand.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TankerCfg {
    pub id: String,
    /// Radio callsign name: "Texaco", "Arco", "Shell".
    pub callsign: String,
    #[serde(default = "one")]
    pub callsign_number: u8,
    /// DCS type: "KC-135", "KC135MPRS", "KC130", "S-3B Tanker", "A-6E",
    /// "IL-78M".
    pub typ: String,
    #[serde(default = "default_side_blue")]
    pub side: String,
    #[serde(default = "default_country_blue")]
    pub country: String,
    /// Start of the racetrack leg.
    pub loc: Loc,
    /// Inbound leg heading, degrees true.
    pub heading_deg: f64,
    #[serde(default = "default_leg")]
    pub leg_nm: f64,
    pub alt_ft: f64,
    /// Knots true. Default is the type's usual refuelling speed.
    #[serde(default)]
    pub speed_kts: Option<f64>,
    pub freq_mhz: f64,
    #[serde(default)]
    pub tacan: Option<TacanCfg>,
    #[serde(default = "yes")]
    pub permanent: bool,
    /// Livery id; default is the type's default livery.
    #[serde(default)]
    pub livery: Option<String>,
    /// Pylon loadout override: pylon number -> CLSID. The engine already knows
    /// that an A-6E needs its D-704 buddy store on the centreline.
    #[serde(default)]
    pub pylons: BTreeMap<u8, String>,
}

fn one() -> u8 {
    1
}
fn yes() -> bool {
    true
}
fn default_leg() -> f64 {
    30.
}
fn default_side_blue() -> String {
    "blue".into()
}
fn default_country_blue() -> String {
    "CJTF Blue".into()
}

/// Everything the engine knows about a tanker type.
#[derive(Debug, Clone, Copy)]
pub struct TankerType {
    pub typ: &'static str,
    pub method: RefuelMethod,
    pub default_speed_kts: f64,
    pub carrier_capable: bool,
    /// (pylon, CLSID) the type must carry to refuel anyone.
    pub required_store: Option<(u8, &'static str)>,
}

/// The tanker types DCS ships with. The A-6E refuels from its D-704 buddy
/// store on the centreline pylon (Heatblur's AI A-6E; the separate KA-6D is
/// commented out in DCS's own files).
pub const TANKER_TYPES: &[TankerType] = &[
    TankerType { typ: "KC-135", method: RefuelMethod::Boom, default_speed_kts: 320., carrier_capable: false, required_store: None },
    TankerType { typ: "KC135MPRS", method: RefuelMethod::Drogue, default_speed_kts: 320., carrier_capable: false, required_store: None },
    TankerType { typ: "KC130", method: RefuelMethod::Drogue, default_speed_kts: 240., carrier_capable: false, required_store: None },
    TankerType { typ: "KC130J", method: RefuelMethod::Drogue, default_speed_kts: 240., carrier_capable: false, required_store: None },
    TankerType { typ: "S-3B Tanker", method: RefuelMethod::Drogue, default_speed_kts: 274., carrier_capable: true, required_store: None },
    TankerType { typ: "A-6E", method: RefuelMethod::Drogue, default_speed_kts: 280., carrier_capable: true, required_store: Some((3, "{HB_A6E_D704}")) },
    TankerType { typ: "IL-78M", method: RefuelMethod::Drogue, default_speed_kts: 300., carrier_capable: false, required_store: None },
];

pub fn tanker_type(typ: &str) -> Option<&'static TankerType> {
    TANKER_TYPES.iter().find(|t| t.typ == typ)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CarrierKind {
    /// CVN-71..75 Supercarrier (and Stennis): angled deck, LSO grades.
    Nimitz,
    Forrestal,
    Tarawa,
    Kuznetsov,
}

/// Landing-area geometry the groove tracker grades against.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct DeckGeometry {
    /// Distance from the ship's reference point to the stern, metres (negative
    /// = aft).
    pub stern_m: f64,
    /// Lateral offset of the landing-area centreline at the stern, metres,
    /// + = starboard.
    pub stern_offset_m: f64,
    /// Flight deck height above the waterline, metres.
    pub deck_height_m: f64,
    /// Landing area angle relative to the ship's heading, degrees (negative =
    /// to port, the usual angled deck).
    pub angle_deg: f64,
    /// Arresting wire distances from the stern along the landing area, metres
    /// (1-wire first). Empty for decks without wires.
    pub wires_m: [f64; 4],
    /// Optical glideslope, degrees.
    pub glideslope_deg: f64,
}

impl CarrierKind {
    /// Defaults from MOOSE AIRBOSS's carrier parameters. They are
    /// approximations of the DCS models; override per carrier in the config.
    pub fn deck(&self) -> DeckGeometry {
        match self {
            Self::Nimitz => DeckGeometry {
                stern_m: -164.,
                stern_offset_m: 9.5,
                deck_height_m: 20.1,
                angle_deg: -9.1359,
                wires_m: [46., 58., 70., 81.],
                glideslope_deg: 3.5,
            },
            Self::Forrestal => DeckGeometry {
                stern_m: -135.5,
                stern_offset_m: 7.5,
                deck_height_m: 20.,
                angle_deg: -9.1359,
                wires_m: [44., 55., 67., 78.],
                glideslope_deg: 3.5,
            },
            Self::Tarawa => DeckGeometry {
                stern_m: -125.,
                stern_offset_m: 0.,
                deck_height_m: 20.,
                angle_deg: 0.,
                wires_m: [0.; 4],
                glideslope_deg: 3.0,
            },
            Self::Kuznetsov => DeckGeometry {
                stern_m: -150.,
                stern_offset_m: 6.,
                deck_height_m: 18.,
                angle_deg: -7.,
                wires_m: [45., 57., 69., 81.],
                glideslope_deg: 3.5,
            },
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecoveryWindowCfg {
    /// Minutes after mission start.
    pub start_min: u32,
    pub end_min: u32,
    /// 1, 2 or 3
    #[serde(default = "one")]
    pub case: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CarrierCfg {
    pub id: String,
    /// Name of the carrier UNIT in the .miz (players spawn on it, so it has to
    /// be in the mission file).
    pub unit_name: String,
    /// Display name: "CVN-73 George Washington".
    pub name: String,
    pub kind: CarrierKind,
    #[serde(default)]
    pub deck: Option<DeckGeometry>,
    #[serde(default)]
    pub tacan: Option<TacanCfg>,
    #[serde(default)]
    pub icls_channel: Option<u8>,
    #[serde(default = "yes")]
    pub acls: bool,
    #[serde(default)]
    pub link4_mhz: Option<f64>,
    #[serde(default)]
    pub tower_mhz: Option<f64>,
    /// Wind over the deck the ship steams for, knots.
    #[serde(default = "default_wod")]
    pub wind_over_deck_kts: f64,
    /// Operating box: the ship turns back toward its centre when it drifts
    /// further than `op_radius_nm`.
    pub op_center: Loc,
    #[serde(default = "default_op_radius")]
    pub op_radius_nm: f64,
    /// Empty = recovery always open.
    #[serde(default)]
    pub recovery_windows: Vec<RecoveryWindowCfg>,
    /// "S-3B Tanker" or "A-6E"; spawned from the deck as a recovery tanker.
    #[serde(default)]
    pub recovery_tanker: Option<TankerCfg>,
    /// Keep a plane-guard helicopter on station (DCS type, e.g. "SH-60B").
    #[serde(default)]
    pub plane_guard: Option<String>,
}

fn default_wod() -> f64 {
    27.
}
fn default_op_radius() -> f64 {
    20.
}

/// On-speed AoA band for an aircraft type, degrees.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct AoaBand {
    pub on_speed: f64,
    /// (F) below this / (SLO) above `slow_little`
    pub fast_little: f64,
    pub slow_little: f64,
    /// F / SLO
    pub fast: f64,
    pub slow: f64,
    /// _F_ / _SLO_
    pub fast_lot: f64,
    pub slow_lot: f64,
    /// Tailhook draw argument (25 on most, 1305 on the F-14).
    #[serde(default)]
    pub hook_arg: Option<i64>,
}

/// AIRBOSS on-speed tables. The F-14 is given in degrees converted from its
/// 15-unit on-speed (deg = 0.918 * units - 3.411).
pub fn default_aoa_table() -> BTreeMap<String, AoaBand> {
    let f14 = |u: f64| 0.918 * u - 3.411;
    let mut t = BTreeMap::new();
    let hornet = AoaBand {
        on_speed: 8.1,
        fast_little: 7.4,
        slow_little: 8.8,
        fast: 6.9,
        slow: 9.3,
        fast_lot: 6.3,
        slow_lot: 9.8,
        hook_arg: Some(25),
    };
    t.insert("FA-18C_hornet".into(), hornet);
    t.insert("FA-18E".into(), hornet);
    t.insert("FA-18F".into(), hornet);
    t.insert("EA-18G".into(), hornet);
    let tomcat = AoaBand {
        on_speed: f14(15.),
        fast_little: f14(14.),
        slow_little: f14(16.),
        fast: f14(13.5),
        slow: f14(16.5),
        fast_lot: f14(13.),
        slow_lot: f14(17.),
        hook_arg: Some(1305),
    };
    t.insert("F-14B".into(), tomcat);
    t.insert("F-14A-135-GR".into(), tomcat);
    t
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissileTrainerCfg {
    #[serde(default = "yes")]
    pub enabled: bool,
    /// Missile is destroyed and the target "killed" inside this distance.
    #[serde(default = "default_kill_radius")]
    pub kill_radius_m: f64,
    /// ... or this, for warheads with at least `big_warhead_kg` explosive.
    #[serde(default = "default_big_kill_radius")]
    pub big_kill_radius_m: f64,
    #[serde(default = "default_big_warhead")]
    pub big_warhead_kg: f64,
    /// Tell the target about the launch (bearing, range, notch headings).
    #[serde(default = "yes")]
    pub launch_alerts: bool,
    /// Protect AI too (keeps adversaries alive for the next set-up).
    #[serde(default)]
    pub protect_ai: bool,
    /// Max missiles tracked at once; beyond this the oldest are dropped.
    #[serde(default = "default_max_missiles")]
    pub max_tracked: usize,
}

fn default_kill_radius() -> f64 {
    200.
}
fn default_big_kill_radius() -> f64 {
    500.
}
fn default_big_warhead() -> f64 {
    50.
}
fn default_max_missiles() -> usize {
    64
}

impl Default for MissileTrainerCfg {
    fn default() -> Self {
        Self {
            enabled: true,
            kill_radius_m: default_kill_radius(),
            big_kill_radius_m: default_big_kill_radius(),
            big_warhead_kg: default_big_warhead(),
            launch_alerts: true,
            protect_ai: false,
            max_tracked: default_max_missiles(),
        }
    }
}

/// An adversary type players can ask for.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdversaryCfg {
    /// DCS type, e.g. "MiG-29S", "Su-27", "F-16C_50", "F-5E-3", "MiG-21Bis".
    pub typ: String,
    pub label: String,
    /// Loadouts by weapon class the player can pick: "guns", "fox2", "fox3".
    /// Each is pylon -> CLSID.
    #[serde(default)]
    pub loadouts: BTreeMap<String, BTreeMap<u8, String>>,
    #[serde(default = "default_side_red")]
    pub side: String,
    #[serde(default = "default_country_red")]
    pub country: String,
    #[serde(default)]
    pub livery: Option<String>,
}

fn default_side_red() -> String {
    "red".into()
}
fn default_country_red() -> String {
    "CJTF Red".into()
}

/// A block of airspace where A/A set-ups and duels happen.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArenaCfg {
    pub id: String,
    pub name: String,
    pub loc: Loc,
    #[serde(default = "default_arena_radius")]
    pub radius_nm: f64,
    /// Duels allowed here.
    #[serde(default = "yes")]
    pub duels: bool,
    #[serde(default)]
    pub floor_ft: Option<f64>,
    #[serde(default)]
    pub ceiling_ft: Option<f64>,
}

fn default_arena_radius() -> f64 {
    15.
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AirToAirCfg {
    #[serde(default)]
    pub missile_trainer: MissileTrainerCfg,
    #[serde(default)]
    pub adversaries: Vec<AdversaryCfg>,
    #[serde(default)]
    pub arenas: Vec<ArenaCfg>,
    /// Allow opt-in PvP duels.
    #[serde(default = "yes")]
    pub duels: bool,
    /// Gun hits that end a duel / engagement, when the missile trainer is not
    /// involved.
    #[serde(default = "default_gun_hits")]
    pub gun_kill_hits: u32,
}

fn default_gun_hits() -> u32 {
    10
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LandingPadCfg {
    pub id: String,
    pub name: String,
    pub loc: Loc,
    /// "precision", "confined", "pinnacle", "ship", "fclp"
    #[serde(default = "default_drill")]
    pub drill: String,
    /// Landing heading, degrees true (None = any).
    #[serde(default)]
    pub heading_deg: Option<f64>,
    /// Touchdown within this is PERFECT; the other bands scale from it.
    #[serde(default = "default_pad_radius")]
    pub perfect_m: f64,
    /// Spawn a visual marker (static) on the pad.
    #[serde(default)]
    pub marker: Option<String>,
}

fn default_drill() -> String {
    "precision".into()
}
fn default_pad_radius() -> f64 {
    3.
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SlingCourseCfg {
    pub id: String,
    pub name: String,
    /// Where the cargo spawns.
    pub pickup: Loc,
    /// Where it must be set down.
    pub dropzone: Loc,
    /// DCS cargo static type, e.g. "ammo_cargo", "uh1h_cargo", "container_cargo".
    #[serde(default = "default_cargo_type")]
    pub cargo_type: String,
    #[serde(default = "default_cargo_mass")]
    pub mass_kg: f64,
    #[serde(default = "default_dz_radius")]
    pub perfect_m: f64,
}

fn default_cargo_type() -> String {
    "ammo_cargo".into()
}
fn default_cargo_mass() -> f64 {
    1000.
}
fn default_dz_radius() -> f64 {
    5.
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TroopLzCfg {
    pub id: String,
    pub name: String,
    pub pickup: Loc,
    pub lz: Loc,
    #[serde(default = "default_troops")]
    pub troops: u32,
    #[serde(default = "default_troop_kg")]
    pub kg_per_troop: f64,
    /// Radius the helo must land within to load / unload.
    #[serde(default = "default_lz_radius")]
    pub radius_m: f64,
}

fn default_troops() -> u32 {
    8
}
fn default_troop_kg() -> f64 {
    100.
}
fn default_lz_radius() -> f64 {
    60.
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HeloCfg {
    #[serde(default)]
    pub pads: Vec<LandingPadCfg>,
    #[serde(default)]
    pub sling: Vec<SlingCourseCfg>,
    #[serde(default)]
    pub troops: Vec<TroopLzCfg>,
}

/// An AI JTAC that runs CAS drills: gives a talk-on and a laser on a target
/// at one of the stations.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JtacCfg {
    pub id: String,
    pub callsign: String,
    /// Station whose targets it controls.
    pub station: String,
    pub loc: Loc,
    #[serde(default = "default_laser")]
    pub laser_code: u16,
    #[serde(default)]
    pub freq_mhz: Option<f64>,
    /// DCS type of the JTAC itself ("MQ-9 Reaper" orbits; "Hummer" sits).
    #[serde(default = "default_jtac_type")]
    pub typ: String,
    /// Friendly troops placed this far from the target, for danger-close
    /// training. None = no friendlies.
    #[serde(default)]
    pub friendlies_m: Option<f64>,
}

fn default_laser() -> u16 {
    1688
}
fn default_jtac_type() -> String {
    "MQ-9 Reaper".into()
}

/// Weapon-class grading thresholds.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScoringCfg {
    /// SHACK inside this, metres.
    #[serde(default = "default_shack")]
    pub shack_m: f64,
    /// GOOD radius per weapon class; EXCELLENT is half, INEFFECTIVE double.
    #[serde(default = "default_good_unguided")]
    pub good_unguided_m: f64,
    #[serde(default = "default_good_guided")]
    pub good_guided_m: f64,
    #[serde(default = "default_good_rocket")]
    pub good_rocket_m: f64,
    /// Impacts further than this from every station are not scored.
    #[serde(default = "default_max_score_dist")]
    pub max_score_m: f64,
    /// Strafe accuracy thresholds, percent.
    #[serde(default = "default_strafe_bands")]
    pub strafe_bands: [f64; 4],
    #[serde(default = "default_aoa_table")]
    pub aoa: BTreeMap<String, AoaBand>,
}

fn default_shack() -> f64 {
    1.53
}
fn default_good_unguided() -> f64 {
    25.
}
fn default_good_guided() -> f64 {
    10.
}
fn default_good_rocket() -> f64 {
    30.
}
fn default_max_score_dist() -> f64 {
    1000.
}
fn default_strafe_bands() -> [f64; 4] {
    // DEADEYE, EXCELLENT, GOOD, INEFFECTIVE; below the last is POOR
    [90., 75., 50., 25.]
}

impl Default for ScoringCfg {
    fn default() -> Self {
        Self {
            shack_m: default_shack(),
            good_unguided_m: default_good_unguided(),
            good_guided_m: default_good_guided(),
            good_rocket_m: default_good_rocket(),
            max_score_m: default_max_score_dist(),
            strafe_bands: default_strafe_bands(),
            aoa: default_aoa_table(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnPolicyCfg {
    #[serde(default = "default_max_active")]
    pub max_active_per_player: u32,
    /// Hard cap on engine-spawned AI units across the server (targets from
    /// the config do not count).
    #[serde(default = "default_max_ai")]
    pub max_ai_units: u32,
    #[serde(default = "default_cooldown")]
    pub cooldown_s: u32,
    /// Despawn a player's spawns this long after they were created.
    #[serde(default = "default_despawn")]
    pub despawn_after_s: u32,
    /// ... and when the owner leaves their slot / disconnects.
    #[serde(default = "yes")]
    pub despawn_on_leave: bool,
    /// Catalog item ids only instructors may spawn.
    #[serde(default = "default_instructor_only")]
    pub instructor_only: Vec<String>,
    /// Instructor UCIDs (in addition to dashboard admins, which bfdb flags).
    #[serde(default)]
    pub instructors: Vec<String>,
}

fn default_max_active() -> u32 {
    3
}
fn default_max_ai() -> u32 {
    60
}
fn default_cooldown() -> u32 {
    20
}
fn default_despawn() -> u32 {
    3600
}
fn default_instructor_only() -> Vec<String> {
    vec!["sam_site".into(), "naval_group".into(), "sead_site".into()]
}

impl Default for SpawnPolicyCfg {
    fn default() -> Self {
        Self {
            max_active_per_player: default_max_active(),
            max_ai_units: default_max_ai(),
            cooldown_s: default_cooldown(),
            despawn_after_s: default_despawn(),
            despawn_on_leave: true,
            instructor_only: default_instructor_only(),
            instructors: vec![],
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RangeCfg {
    /// Publish RPCs under this netidx base (bfdb's instance `base`).
    #[serde(default)]
    pub netidx_base: Option<NetidxPath>,
    /// The netidx client config (`client.json`) to publish through. Unset,
    /// netidx looks in its usual places (`NETIDX_CFG`, then the DCS account's
    /// `%APPDATA%\netidx\client.json`). Set it when DCS runs on a different PC
    /// from bfdb and the range has its own resolver there.
    #[serde(default)]
    pub netidx_config: Option<String>,
    /// Which address the RPC publisher listens on, in netidx bind syntax: a
    /// subnet such as `"192.168.1.0/24"` picks this PC's address in it.
    /// Overrides the client config's `default_bind_config`. Unset, that
    /// default applies (`local` when it has none, which only bfdb on this
    /// same PC can reach).
    #[serde(default)]
    pub netidx_bind: Option<String>,
    #[serde(default = "default_range_name")]
    pub name: String,
    /// The theatre divided up by job, as drawn on the F10 map.
    #[serde(default)]
    pub sectors: Vec<SectorCfg>,
    #[serde(default)]
    pub stations: Vec<StationCfg>,
    #[serde(default)]
    pub tankers: Vec<TankerCfg>,
    #[serde(default)]
    pub carriers: Vec<CarrierCfg>,
    #[serde(default)]
    pub air_to_air: AirToAirCfg,
    #[serde(default)]
    pub helo: HeloCfg,
    #[serde(default)]
    pub jtacs: Vec<JtacCfg>,
    #[serde(default)]
    pub scoring: ScoringCfg,
    #[serde(default)]
    pub spawn: SpawnPolicyCfg,
    /// Show results in game as text to the player (and their group).
    #[serde(default = "yes")]
    pub in_game_results: bool,
    /// How long result messages stay on screen, seconds.
    #[serde(default = "default_msg_secs")]
    pub message_s: u32,
    /// Record weapon / groove tracks for the debrief cards.
    #[serde(default = "yes")]
    pub record_tracks: bool,
    /// Welcome text shown on slot-in.
    #[serde(default)]
    pub welcome: Option<String>,
}

fn default_range_name() -> String {
    "Vector Strike Range".into()
}
fn default_msg_secs() -> u32 {
    20
}

impl Default for RangeCfg {
    fn default() -> Self {
        serde_json::from_str("{}").expect("empty range cfg parses")
    }
}

impl RangeCfg {
    pub fn load(path: &Path) -> anyhow::Result<Self> {
        let txt = std::fs::read_to_string(path)?;
        Ok(serde_json::from_str(&txt)?)
    }

    pub fn save(&self, path: &Path) -> anyhow::Result<()> {
        let tmp = path.with_extension("tmp");
        std::fs::write(&tmp, serde_json::to_string_pretty(self)?)?;
        std::fs::rename(tmp, path)?;
        Ok(())
    }

    pub fn tanker_type(&self, t: &TankerCfg) -> Option<&'static TankerType> {
        tanker_type(&t.typ)
    }

    /// Structural checks that don't need DCS: unique ids, sane numbers.
    pub fn validate(&self) -> anyhow::Result<()> {
        use anyhow::bail;
        let mut ids = std::collections::HashSet::new();
        let ok_id = |id: &str| {
            !id.is_empty() && id.chars().all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
        };
        for s in &self.stations {
            if !ok_id(&s.id) || !ids.insert(format!("station:{}", s.id)) {
                bail!("station id {:?} is empty, invalid or duplicated", s.id)
            }
        }
        for t in &self.tankers {
            if !ok_id(&t.id) || !ids.insert(format!("tanker:{}", t.id)) {
                bail!("tanker id {:?} is empty, invalid or duplicated", t.id)
            }
            if tanker_type(&t.typ).is_none() {
                bail!(
                    "tanker {:?}: type {:?} is not a DCS tanker (one of {:?})",
                    t.id,
                    t.typ,
                    TANKER_TYPES.iter().map(|t| t.typ).collect::<Vec<_>>()
                )
            }
        }
        for c in &self.carriers {
            if !ok_id(&c.id) || !ids.insert(format!("carrier:{}", c.id)) {
                bail!("carrier id {:?} is empty, invalid or duplicated", c.id)
            }
            if let Some(t) = &c.recovery_tanker {
                match tanker_type(&t.typ) {
                    Some(tt) if tt.carrier_capable => (),
                    _ => bail!(
                        "carrier {:?}: recovery tanker must be a carrier-capable tanker \
                         (\"S-3B Tanker\" or \"A-6E\"), not {:?}",
                        c.id,
                        t.typ
                    ),
                }
            }
        }
        for a in &self.air_to_air.arenas {
            if !ok_id(&a.id) || !ids.insert(format!("arena:{}", a.id)) {
                bail!("arena id {:?} is empty, invalid or duplicated", a.id)
            }
        }
        for s in &self.sectors {
            if !ok_id(&s.id) || !ids.insert(format!("sector:{}", s.id)) {
                bail!("sector id {:?} is empty, invalid or duplicated", s.id)
            }
            if let Err(e) = s.shape.check() {
                bail!("sector {:?}: {e}", s.id)
            }
            if !matches!(s.side.as_str(), "blue" | "red" | "all") {
                bail!("sector {:?}: side must be blue, red or all, not {:?}", s.id, s.side)
            }
        }
        if let Some(b) = &self.netidx_bind {
            if let Err(e) = b.parse::<netidx::publisher::BindCfg>() {
                bail!("netidx_bind {b:?} is not a netidx bind address: {e}")
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_config_has_defaults() {
        let c = RangeCfg::default();
        assert_eq!(c.scoring.good_unguided_m, 25.);
        assert!(c.scoring.aoa.contains_key("FA-18C_hornet"));
        assert!(c.validate().is_ok());
    }

    #[test]
    fn sectors_are_checked() {
        let mut c = RangeCfg::default();
        let circle = SectorShape { circle: Some(SectorCircle { lat: 42., lon: 40., radius_m: 1000. }), ..Default::default() };
        c.sectors.push(SectorCfg {
            id: "w-1".into(),
            name: "W-1".into(),
            kind: SectorKind::AirToAir,
            side: "all".into(),
            purpose: String::new(),
            shape: circle.clone(),
            announce: true,
        });
        assert!(c.validate().is_ok());
        c.sectors[0].shape.polygon = Some(vec![LatLon { lat: 42., lon: 40. }; 3]);
        assert!(c.validate().is_err(), "two shapes at once");
        c.sectors[0].shape = circle;
        c.sectors[0].side = "green".into();
        assert!(c.validate().is_err());
    }

    #[test]
    fn netidx_bind_is_checked() {
        let mut c = RangeCfg::default();
        c.netidx_bind = Some("192.168.1.0/24".into());
        assert!(c.validate().is_ok());
        c.netidx_bind = Some("192.168.1/24".into());
        assert!(c.validate().is_err());
    }

    #[test]
    fn tacan_frequencies() {
        let t = TacanCfg { channel: 1, band: TacanBand::Y, morse: "TKR".into() };
        assert_eq!(t.frequency_hz(), 1088e6);
        let t = TacanCfg { channel: 74, band: TacanBand::X, morse: "CVN".into() };
        assert_eq!(t.frequency_hz(), (1151. + 74. - 64.) * 1e6);
    }

    #[test]
    fn sample_config_parses_and_validates() {
        let c: RangeCfg =
            serde_json::from_str(include_str!("../../../bfrange/RANGE_CFG.sample.json")).unwrap();
        c.validate().unwrap();
        assert_eq!(c.carriers[0].recovery_tanker.as_ref().unwrap().typ, "A-6E");
        assert!(c.stations.iter().any(|s| s.kind == StationKind::StrafePit));
    }

    #[test]
    fn a6e_is_a_carrier_tanker() {
        let t = tanker_type("A-6E").unwrap();
        assert!(t.carrier_capable);
        assert_eq!(t.method, RefuelMethod::Drogue);
        assert_eq!(t.required_store, Some((3, "{HB_A6E_D704}")));
    }
}
