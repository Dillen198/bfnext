//! Training range wire types, shared by the range engine (`bfrange`), bfdb
//! and the range web app.
//!
//! The engine writes one [`RangeRecord`] per graded event (a bomb, a strafe
//! pass, a carrier pass, an AAR session, a missile-trainer shot ...) as a JSON
//! line to `Logs/range.jsonl`. bfdb ingests those into raw-JSON sled trees
//! (never bincode -- these types are expected to grow) and serves them to the
//! range site and the Discord result feed. Live state ([`RangeLive`]) and the
//! spawn catalogue come over netidx RPCs.
//!
//! Everything here is JSON-only on purpose: add fields with `#[serde(default)]`
//! and old lines keep parsing.

pub mod cfg;
pub mod grading;
pub mod lso;

use chrono::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Wire version of `RangeRecord`; bump on an incompatible change.
pub const RECORD_VERSION: u32 = 1;

/// Who flew it. `ucid` is `None` for an AI shooter/target.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PilotRef {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ucid: Option<String>,
    pub name: String,
}

/// A point on the map.
#[derive(Debug, Clone, Copy, PartialEq, Default, Serialize, Deserialize)]
pub struct GeoPt {
    pub lat: f64,
    pub lon: f64,
    /// metres MSL
    #[serde(default)]
    pub alt_m: f64,
}

/// One graded event.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RangeRecord {
    /// Unique, engine-assigned: `<sortie>-<unix ms>-<seq>`.
    pub id: String,
    #[serde(default = "record_version")]
    pub v: u32,
    pub ts: DateTime<Utc>,
    /// The mission's own clock and date at the event, for the card footer
    /// ("Caucasus: 2024/8/1 (15:48:13)").
    #[serde(default)]
    pub mission_time: String,
    #[serde(default)]
    pub mission_date: String,
    #[serde(default)]
    pub theatre: String,
    pub pilot: PilotRef,
    /// DCS type name of the pilot's aircraft, e.g. "FA-18C_hornet".
    pub unit_type: String,
    /// "blue" / "red" / "neutral"
    #[serde(default)]
    pub side: String,
    /// Group callsign / name in DCS, e.g. "Casper 1-1".
    #[serde(default)]
    pub callsign: String,
    /// Normalised 0..5 score so different disciplines can share a board.
    /// `None` when the event does not count (foul-deck waveoff, aborted run).
    #[serde(default)]
    pub score: Option<f64>,
    pub result: RangeResult,
    /// Sampled geometry for the debrief card. Large; bfdb stores it apart
    /// from the record and strips it from list responses.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub track: Option<Track>,
}

fn record_version() -> u32 {
    RECORD_VERSION
}

impl RangeRecord {
    pub fn kind(&self) -> &'static str {
        self.result.kind()
    }

    /// One-line headline for a feed entry / Discord message.
    pub fn headline(&self) -> String {
        self.result.headline(&self.pilot.name, &self.unit_type)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum RangeResult {
    Bomb(BombResult),
    Strafe(StrafeResult),
    Trap(TrapResult),
    Aar(AarResult),
    Missile(MissileResult),
    Engagement(EngagementResult),
    AntiShip(AntiShipResult),
    Sling(SlingResult),
    Landing(LandingResult),
    Troops(TroopResult),
    Gunnery(GunneryResult),
    Cas(CasResult),
}

impl RangeResult {
    pub fn kind(&self) -> &'static str {
        match self {
            Self::Bomb(_) => "bomb",
            Self::Strafe(_) => "strafe",
            Self::Trap(_) => "trap",
            Self::Aar(_) => "aar",
            Self::Missile(_) => "missile",
            Self::Engagement(_) => "engagement",
            Self::AntiShip(_) => "anti_ship",
            Self::Sling(_) => "sling",
            Self::Landing(_) => "landing",
            Self::Troops(_) => "troops",
            Self::Gunnery(_) => "gunnery",
            Self::Cas(_) => "cas",
        }
    }

    pub fn headline(&self, pilot: &str, typ: &str) -> String {
        match self {
            Self::Bomb(b) => format!(
                "{pilot} ({typ}) {} on {}: {:.0} m @ {} o'clock, {}",
                b.weapon,
                b.target,
                b.miss_m,
                b.clock,
                b.quality.label()
            ),
            Self::Strafe(s) => format!(
                "{pilot} ({typ}) strafe {}: {}/{} hits ({:.0}%), {}",
                s.pit,
                s.hits,
                s.rounds_fired,
                s.accuracy_pct,
                s.quality.label()
            ),
            Self::Trap(t) => {
                let wire = t.wire.map(|w| format!(" #{w} wire")).unwrap_or_default();
                format!(
                    "{pilot} ({typ}) {}: {} => {}{wire}",
                    t.carrier,
                    t.grade,
                    lso::grade_name(&t.grade)
                )
            }
            Self::Aar(a) => format!(
                "{pilot} ({typ}) AAR on {}: {} contact(s), {:.0} lb, grade {}",
                a.tanker, a.contacts, a.fuel_lbs, a.grade
            ),
            Self::Missile(m) => format!(
                "{} {} vs {}: {}",
                m.shooter.name,
                m.weapon,
                m.target.name,
                m.outcome.label()
            ),
            Self::Engagement(e) => format!(
                "{pilot} ({typ}) {} vs {}: {}",
                e.setup,
                e.adversary,
                e.outcome.label()
            ),
            Self::AntiShip(a) => format!(
                "{pilot} ({typ}) {} on {}: {}",
                a.weapon,
                a.ship,
                if a.hit { "HIT" } else { "MISS" }
            ),
            Self::Sling(s) => format!(
                "{pilot} ({typ}) {} {} to {}: {:.1} m off, {}",
                if s.method == "internal" { "cargo" } else { "sling" },
                s.cargo,
                s.course,
                s.distance_m,
                s.quality.label()
            ),
            Self::Landing(l) => format!(
                "{pilot} ({typ}) {} landing at {}: {:.1} m off, {:.0} fpm, {}",
                l.drill,
                l.pad,
                l.distance_m,
                l.touchdown_fpm,
                l.quality.label()
            ),
            Self::Troops(t) => format!(
                "{pilot} ({typ}) troops to {}: {} in {:.0} s",
                t.lz, t.troops, t.total_time_s
            ),
            Self::Gunnery(g) => format!(
                "{pilot} ({typ}) gunnery {}: {}/{} targets in {:.0} s",
                g.lane, g.targets_killed, g.targets_total, g.time_s
            ),
            Self::Cas(c) => format!(
                "{pilot} ({typ}) CAS with {}: {}",
                c.jtac,
                if c.correct_target { "on target" } else { "wrong target" }
            ),
        }
    }
}

// ---------------------------------------------------------------- A/G range

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WeaponClass {
    Unguided,
    Guided,
    Rocket,
    Missile,
    Cluster,
    Gun,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum BombQuality {
    Poor,
    Ineffective,
    Good,
    Excellent,
    Shack,
}

impl BombQuality {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Shack => "SHACK",
            Self::Excellent => "EXCELLENT",
            Self::Good => "GOOD",
            Self::Ineffective => "INEFFECTIVE",
            Self::Poor => "POOR",
        }
    }

    pub fn score(&self) -> f64 {
        match self {
            Self::Shack => 5.,
            Self::Excellent => 4.,
            Self::Good => 3.,
            Self::Ineffective => 2.,
            Self::Poor => 1.,
        }
    }
}

/// DCS's atmosphere at one height above a point, read from the running
/// mission (`atmosphere.getWind`, `atmosphere.getTemperatureAndPressure`).
/// Every wind-dependent number the range reports or calculates comes from
/// these, never from the mission file or an assumed standard day.
#[derive(Debug, Clone, Copy, Default, PartialEq, Serialize, Deserialize)]
pub struct AtmoLayer {
    /// metres MSL
    pub alt_m: f64,
    /// wind, meteorological (the direction it blows FROM), degrees true
    pub wind_from_deg: f64,
    pub wind_kts: f64,
    pub temp_c: f64,
    pub pressure_hpa: f64,
}

/// The aircraft's state at weapon release.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Release {
    pub pos: GeoPt,
    pub alt_agl_m: f64,
    pub tas_kts: f64,
    pub gs_kts: f64,
    /// true heading of the velocity vector (track), degrees
    pub heading_deg: f64,
    /// flight-path dive angle, degrees; positive = descending
    pub dive_deg: f64,
    /// slant and ground range from the aircraft to the target at release
    pub slant_range_m: f64,
    pub ground_range_m: f64,
    /// wind at the release point, meteorological (from)
    pub wind_from_deg: f64,
    pub wind_kts: f64,
    /// from DCS's temperature at the release point
    #[serde(default)]
    pub mach: f64,
    /// DCS's wind, temperature and pressure over the release point from the
    /// impact's ground up to the release altitude, lowest first. What the
    /// bomb actually fell through; empty on records from older engines.
    #[serde(default)]
    pub atmo: Vec<AtmoLayer>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BombResult {
    pub station_id: String,
    /// Range (station) display name, e.g. "Range A - Bomb Circle".
    pub range: String,
    pub target: String,
    /// DCS weapon type name, e.g. "GBU_16".
    pub weapon: String,
    #[serde(default)]
    pub weapon_display: String,
    pub weapon_class: WeaponClass,
    /// "laser", "ins", "tv", "ir", "radar", "none"
    #[serde(default)]
    pub guidance: String,
    pub release: Release,
    pub target_pos: GeoPt,
    pub impact: GeoPt,
    /// impact relative to the target, metres (north, east)
    pub impact_north_m: f64,
    pub impact_east_m: f64,
    /// 2-D miss distance
    pub miss_m: f64,
    /// bearing from target to impact, degrees true (the card's φ)
    pub radial_deg: f64,
    /// 1..12 relative to the attack heading: 12 o'clock is straight ahead of
    /// the attacker (LONG), 6 o'clock is SHORT, 3 is right, 9 is left.
    pub clock: u8,
    /// along-track error, + = long
    pub long_m: f64,
    /// cross-track error, + = right of the run-in line
    pub cross_m: f64,
    pub time_of_flight_s: f64,
    pub quality: BombQuality,
    /// Did it actually hit / kill a target object?
    #[serde(default)]
    pub target_hit: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub laser_code: Option<u16>,
    /// Scoring ring radii for the card, metres.
    #[serde(default)]
    pub rings_m: Vec<f64>,
    /// The GOOD radius this weapon was graded against.
    #[serde(default)]
    pub good_radius_m: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum StrafeQuality {
    Invalid,
    Poor,
    Ineffective,
    Good,
    Excellent,
    Deadeye,
}

impl StrafeQuality {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Deadeye => "DEADEYE",
            Self::Excellent => "EXCELLENT",
            Self::Good => "GOOD",
            Self::Ineffective => "INEFFECTIVE",
            Self::Poor => "POOR",
            Self::Invalid => "INVALID",
        }
    }

    pub fn score(&self) -> Option<f64> {
        Some(match self {
            Self::Deadeye => 5.,
            Self::Excellent => 4.,
            Self::Good => 3.,
            Self::Ineffective => 2.,
            Self::Poor => 1.,
            Self::Invalid => return None,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StrafeResult {
    pub station_id: String,
    pub range: String,
    pub pit: String,
    pub gun: String,
    pub rounds_fired: u32,
    pub hits: u32,
    pub accuracy_pct: f64,
    pub quality: StrafeQuality,
    pub foul_line_crossed: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub invalid_reason: Option<String>,
    /// true heading of the run-in
    pub run_in_heading_deg: f64,
    /// closest slant range to the target while firing
    pub min_range_m: f64,
    pub entry_alt_agl_m: f64,
    #[serde(default)]
    pub target_pos: GeoPt,
    /// foul line distance from the target, metres
    #[serde(default)]
    pub foul_line_m: f64,
}

// ---------------------------------------------------------------- carrier

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PassOutcome {
    Trap,
    Bolter,
    Waveoff,
    OwnWaveoff,
    TouchAndGo,
    Crash,
    Unknown,
}

impl PassOutcome {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Trap => "Trap",
            Self::Bolter => "Bolter",
            Self::Waveoff => "Waveoff",
            Self::OwnWaveoff => "Own waveoff",
            Self::TouchAndGo => "Touch and go",
            Self::Crash => "Crash",
            Self::Unknown => "Unknown",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GradeSource {
    /// DCS's own Supercarrier LSO (LANDING_QUALITY_MARK)
    Dcs,
    /// our groove tracker
    Engine,
}

/// Summary of the pattern before the groove (Case I).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PatternSummary {
    #[serde(default)]
    pub break_alt_ft: Option<f64>,
    #[serde(default)]
    pub abeam_distance_nm: Option<f64>,
    #[serde(default)]
    pub abeam_alt_ft: Option<f64>,
    #[serde(default)]
    pub ninety_alt_ft: Option<f64>,
    /// altitude when crossing the wake / starting the groove (the card's
    /// "Wake Alt")
    #[serde(default)]
    pub wake_alt_ft: Option<f64>,
    /// seconds from the break to touchdown / waveoff
    #[serde(default)]
    pub pattern_time_s: Option<f64>,
    #[serde(default)]
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrapResult {
    pub carrier: String,
    pub carrier_type: String,
    /// 1, 2 or 3
    pub case: u8,
    pub night: bool,
    pub outcome: PassOutcome,
    /// Normalised grade, see `lso::normalize_grade`.
    pub grade: String,
    #[serde(default)]
    pub points: Option<f64>,
    /// The deviation calls as the LSO wrote them, e.g. "AAX FIM (SLO)AR _HAW_".
    #[serde(default)]
    pub lso_comment: String,
    /// Those calls in plain English, one line each.
    #[serde(default)]
    pub lso_description: Vec<String>,
    #[serde(default)]
    pub wire: Option<u8>,
    /// true when `wire` came from DCS, false when estimated from the stop point
    #[serde(default)]
    pub wire_from_dcs: bool,
    #[serde(default)]
    pub groove_time_s: Option<f64>,
    #[serde(default)]
    pub wind_over_deck_kts: Option<f64>,
    /// final bearing of the landing area, degrees true
    #[serde(default)]
    pub final_bearing_deg: Option<f64>,
    /// Where the grade came from. When DCS's LSO graded the pass that wins;
    /// the engine's own grade is kept alongside in `engine_grade`.
    pub source: GradeSource,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub dcs_comment: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub engine_grade: Option<EngineGrade>,
    #[serde(default)]
    pub pattern: PatternSummary,
    /// Was the hook down in the groove (None if the type has no known arg).
    #[serde(default)]
    pub hook_down: Option<bool>,
}

/// The engine's own AIRBOSS-style grade.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EngineGrade {
    pub grade: String,
    pub points: Option<f64>,
    /// e.g. "AAX FIM (SLO)AR _HAW_"
    pub comment: String,
}

/// One sample in the groove / pattern, in the landing-area frame.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct GrooveSample {
    /// seconds since the start of the recorded track
    pub t: f64,
    /// distance aft of the landing point along the final bearing, metres
    /// (positive = behind the ship, i.e. in the groove)
    pub x_m: f64,
    /// lateral offset from the landing-area centreline, metres, + = right
    pub y_m: f64,
    /// height above the deck, feet
    pub alt_ft: f64,
    /// glideslope error, degrees (+ = high)
    pub gse_deg: f64,
    /// lineup error, degrees (+ = right of centreline)
    pub lue_deg: f64,
    #[serde(default)]
    pub aoa_deg: Option<f64>,
    /// ground speed relative to the ship, knots
    pub closure_kts: f64,
    pub vs_fpm: f64,
    #[serde(default)]
    pub lat: f64,
    #[serde(default)]
    pub lon: f64,
}

// ---------------------------------------------------------------- AAR

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RefuelMethod {
    Boom,
    Drogue,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Stability {
    /// standard deviation of the receiver's position in the tanker frame while
    /// connected, metres
    pub fore_aft_sd_m: f64,
    pub lateral_sd_m: f64,
    pub vertical_sd_m: f64,
    /// mean position while connected (fwd, right, up), metres
    pub mean_fwd_m: f64,
    pub mean_right_m: f64,
    pub mean_up_m: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AarResult {
    pub tanker: String,
    pub tanker_type: String,
    pub method: RefuelMethod,
    /// seconds from 1 nm in trail to the first contact
    #[serde(default)]
    pub join_time_s: Option<f64>,
    pub contacts: u32,
    pub disconnects: u32,
    pub time_connected_s: f64,
    pub fuel_kg: f64,
    pub fuel_lbs: f64,
    /// fuel on-load rate while connected, lb/min
    pub onload_rate_lbs_min: f64,
    pub stability: Stability,
    /// closure rate when passing pre-contact, knots (+ = closing)
    #[serde(default)]
    pub precontact_closure_kts: Option<f64>,
    #[serde(default)]
    pub overshoot: bool,
    pub alt_ft: f64,
    pub speed_kts: f64,
    /// "A" .. "F"
    pub grade: String,
    /// What cost points / what went well.
    #[serde(default)]
    pub calls: Vec<String>,
    pub session_s: f64,
}

/// Receiver position in the tanker's body frame.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct RelSample {
    pub t: f64,
    /// + = ahead of the tanker's reference point
    pub fwd_m: f64,
    pub right_m: f64,
    pub up_m: f64,
    pub connected: bool,
    pub fuel_kg: f64,
    /// closure rate, knots
    pub closure_kts: f64,
}

// ---------------------------------------------------------------- A/A

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MissileOutcome {
    /// The trainer destroyed it inside the kill radius: the target would have
    /// died.
    Kill,
    /// It went away without ever getting inside the kill radius.
    Defeated,
    /// It really hit (trainer disabled for this target).
    Hit,
    /// Self-destructed / lost track / time out.
    Timeout,
}

impl MissileOutcome {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Kill => "SPLASH (trainer kill)",
            Self::Defeated => "DEFEATED",
            Self::Hit => "HIT",
            Self::Timeout => "TIMED OUT",
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LaunchGeom {
    pub range_m: f64,
    /// target aspect angle at launch (0 = hot, 180 = cold), degrees
    pub aspect_deg: f64,
    pub shooter_alt_m: f64,
    pub target_alt_m: f64,
    pub shooter_speed_kts: f64,
    pub target_speed_kts: f64,
    /// + = closing
    pub closure_kts: f64,
    pub shooter_pos: GeoPt,
    pub target_pos: GeoPt,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DefenseSummary {
    /// seconds from launch until the target's heading changed > 30 degrees
    #[serde(default)]
    pub reaction_s: Option<f64>,
    /// seconds spent beaming (missile 70..110 degrees off the nose)
    pub beam_s: f64,
    /// seconds spent dragging (missile behind, > 135 degrees)
    pub drag_s: f64,
    /// seconds spent hot (missile < 45 degrees off the nose)
    pub hot_s: f64,
    pub alt_change_m: f64,
    /// did the target descend below 1000 m AGL (notch against the ground)
    #[serde(default)]
    pub went_low: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissileResult {
    pub weapon: String,
    /// "aam" / "sam" / "other"
    pub weapon_category: String,
    pub shooter: PilotRef,
    pub shooter_type: String,
    pub target: PilotRef,
    pub target_type: String,
    pub outcome: MissileOutcome,
    pub launch: LaunchGeom,
    pub min_distance_m: f64,
    pub time_of_flight_s: f64,
    pub kill_radius_m: f64,
    pub defense: DefenseSummary,
    /// Which pilot this record is "for": the shooter (`shooter`) or the
    /// defender (`target`). The engine writes one record per human involved,
    /// keyed by `RangeRecord.pilot`.
    #[serde(default)]
    pub perspective: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EngagementOutcome {
    Win,
    Loss,
    Draw,
    Abort,
}

impl EngagementOutcome {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Win => "WIN",
            Self::Loss => "LOSS",
            Self::Draw => "DRAW",
            Self::Abort => "ABORTED",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EngagementResult {
    /// "BFM offensive", "BVR 30 nm hot", "Duel" ...
    pub setup: String,
    /// Adversary DCS type or the other player's name.
    pub adversary: String,
    #[serde(default)]
    pub adversary_skill: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub opponent: Option<PilotRef>,
    pub outcome: EngagementOutcome,
    pub duration_s: f64,
    pub shots_fired: u32,
    pub trainer_kills: u32,
    pub gun_hits: u32,
    #[serde(default)]
    pub notes: Vec<String>,
}

// ---------------------------------------------------------------- anti-ship

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AntiShipResult {
    pub ship: String,
    pub ship_type: String,
    pub weapon: String,
    pub launch_range_m: f64,
    #[serde(default)]
    pub weapon_max_range_m: Option<f64>,
    pub hit: bool,
    /// fraction of the ship's life removed by this weapon, 0..1
    pub damage: f64,
    pub ship_sunk: bool,
    pub time_of_flight_s: f64,
    #[serde(default)]
    pub intercepted: bool,
    #[serde(default)]
    pub launch_pos: GeoPt,
    #[serde(default)]
    pub ship_pos: GeoPt,
}

// ---------------------------------------------------------------- rotary

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum PrecisionQuality {
    Poor,
    Fair,
    Good,
    Excellent,
    Perfect,
}

impl PrecisionQuality {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Perfect => "PERFECT",
            Self::Excellent => "EXCELLENT",
            Self::Good => "GOOD",
            Self::Fair => "FAIR",
            Self::Poor => "POOR",
        }
    }

    pub fn score(&self) -> f64 {
        match self {
            Self::Perfect => 5.,
            Self::Excellent => 4.,
            Self::Good => 3.,
            Self::Fair => 2.,
            Self::Poor => 1.,
        }
    }
}

/// A helicopter cargo delivery: a sling load, or DCS dynamic cargo carried
/// inside (loaded with the ground crew's cargo loader).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SlingResult {
    /// "sling" or "internal"
    #[serde(default = "sling_method")]
    pub method: String,
    pub course: String,
    pub cargo: String,
    pub mass_kg: f64,
    /// pickup (cargo left the ground) to delivery (cargo set down), seconds
    pub time_s: f64,
    /// distance of the set-down point from the drop zone centre
    pub distance_m: f64,
    /// fraction of the cargo's life lost, 0..1
    pub damage: f64,
    pub quality: PrecisionQuality,
    #[serde(default)]
    pub dz_pos: GeoPt,
    #[serde(default)]
    pub set_down_pos: GeoPt,
}

fn sling_method() -> String {
    "sling".into()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LandingResult {
    /// "precision", "confined", "pinnacle", "ship", "fclp"
    pub drill: String,
    pub pad: String,
    pub distance_m: f64,
    /// vertical speed at touchdown, feet per minute (positive = descending)
    pub touchdown_fpm: f64,
    /// heading error vs the pad's landing heading, degrees
    #[serde(default)]
    pub heading_error_deg: Option<f64>,
    /// seconds hovering within the drill box before touchdown
    #[serde(default)]
    pub hover_s: f64,
    pub quality: PrecisionQuality,
    #[serde(default)]
    pub pad_pos: GeoPt,
    #[serde(default)]
    pub touchdown_pos: GeoPt,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TroopResult {
    pub lz: String,
    pub troops: u32,
    pub load_time_s: f64,
    pub total_time_s: f64,
    pub landing_distance_m: f64,
    pub quality: PrecisionQuality,
}

// ---------------------------------------------------------------- ground / CAS

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GunneryResult {
    pub lane: String,
    pub targets_total: u32,
    pub targets_killed: u32,
    pub shots: u32,
    pub hits: u32,
    pub time_s: f64,
    #[serde(default)]
    pub first_round_hits: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CasResult {
    pub jtac: String,
    pub target: String,
    pub weapon: String,
    /// from the 9-line being passed to the first impact, seconds
    pub time_to_impact_s: f64,
    pub miss_m: f64,
    pub correct_target: bool,
    pub danger_close: bool,
    #[serde(default)]
    pub nearest_friendly_m: Option<f64>,
    #[serde(default)]
    pub laser_code: Option<u16>,
}

// ---------------------------------------------------------------- tracks

/// Sampled geometry behind a result card.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Track {
    /// weapon flight, release to impact
    Weapon { points: Vec<TrackPt> },
    /// carrier groove + pattern
    Groove { samples: Vec<GrooveSample> },
    /// receiver in the tanker frame
    Aar { samples: Vec<RelSample> },
    /// missile and target, launch to end
    Intercept { missile: Vec<TrackPt>, target: Vec<TrackPt> },
    /// generic flight path (engagements, helo drills)
    Path { paths: BTreeMap<String, Vec<TrackPt>> },
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct TrackPt {
    pub t: f64,
    pub lat: f64,
    pub lon: f64,
    pub alt_m: f64,
    #[serde(default)]
    pub speed_kts: f64,
}

// ---------------------------------------------------------------- live

/// Everything the range site's live page shows. `query-range` RPC.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RangeLive {
    pub server_time: DateTime<Utc>,
    #[serde(default)]
    pub theatre: String,
    #[serde(default)]
    pub mission_time: String,
    #[serde(default)]
    pub mission_date: String,
    #[serde(default)]
    pub night: bool,
    pub wind: WindInfo,
    pub players: Vec<LivePlayer>,
    pub stations: Vec<LiveStation>,
    pub tankers: Vec<LiveTanker>,
    pub carriers: Vec<LiveCarrier>,
    pub spawns: Vec<LiveSpawn>,
    #[serde(default)]
    pub arenas: Vec<LiveArena>,
    /// The theatre's sectors, as drawn on the F10 map, for the site's map.
    #[serde(default)]
    pub sectors: Vec<cfg::SectorCfg>,
    /// seconds, engine uptime since mission start
    #[serde(default)]
    pub uptime_s: f64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WindInfo {
    /// DCS's atmosphere over the reference point, lowest first
    #[serde(default)]
    pub layers: Vec<AtmoLayer>,
    pub surface_from_deg: f64,
    pub surface_kts: f64,
    /// wind at ~2000 m / 6600 ft
    pub alt_from_deg: f64,
    pub alt_kts: f64,
    #[serde(default)]
    pub temperature_c: f64,
    #[serde(default)]
    pub qnh_hpa: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LivePlayer {
    pub ucid: String,
    pub name: String,
    pub unit_type: String,
    #[serde(default)]
    pub callsign: String,
    pub side: String,
    pub pos: GeoPt,
    pub heading_deg: f64,
    pub speed_kts: f64,
    pub in_air: bool,
    /// What the engine thinks they are doing: "Range A", "AAR Texaco",
    /// "CV-73 pattern" ...
    #[serde(default)]
    pub activity: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StationKind {
    BombCircle,
    StrafePit,
    TacticalArray,
    Convoy,
    CoordTarget,
    LaserTarget,
    ShipTarget,
    GunneryLane,
    SamSite,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiveStation {
    pub id: String,
    pub name: String,
    pub kind: StationKind,
    pub pos: GeoPt,
    /// players currently working the station
    pub hot_by: Vec<String>,
    pub targets_alive: u32,
    pub targets_total: u32,
    #[serde(default)]
    pub laser_code: Option<u16>,
    #[serde(default)]
    pub rings_m: Vec<f64>,
    #[serde(default)]
    pub note: Option<String>,
    /// ground height at the station's centre, metres MSL, from DCS
    #[serde(default)]
    pub elev_m: Option<f64>,
    /// DCS's wind, temperature and pressure over the station, lowest first
    /// (ground up to 10 km), for the release calculator
    #[serde(default)]
    pub atmo: Vec<AtmoLayer>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TankerState {
    Spawning,
    OnStation,
    Rtb,
    Dead,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiveTanker {
    pub id: String,
    pub callsign: String,
    pub unit_type: String,
    pub method: RefuelMethod,
    pub pos: GeoPt,
    pub heading_deg: f64,
    pub speed_kts: f64,
    pub alt_ft: f64,
    /// e.g. "51Y TEX"
    #[serde(default)]
    pub tacan: Option<String>,
    pub freq_mhz: f64,
    pub state: TankerState,
    /// players currently on the boom/basket or in trail
    #[serde(default)]
    pub receivers: Vec<String>,
    /// who spawned it, if on-demand
    #[serde(default)]
    pub owner: Option<String>,
    /// recovery tanker for this carrier id
    #[serde(default)]
    pub recovery_for: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiveCarrier {
    pub id: String,
    pub name: String,
    pub unit_type: String,
    pub pos: GeoPt,
    /// base recovery course (ship heading), degrees true
    pub brc_deg: f64,
    /// final bearing of the landing area, degrees true
    pub fb_deg: f64,
    pub speed_kts: f64,
    pub wind_over_deck_kts: f64,
    /// relative wind direction over the angled deck, degrees (0 = straight down)
    #[serde(default)]
    pub wind_over_deck_angle_deg: f64,
    pub recovery_open: bool,
    #[serde(default)]
    pub next_window: Option<String>,
    #[serde(default)]
    pub tacan: Option<String>,
    #[serde(default)]
    pub icls: Option<u8>,
    #[serde(default)]
    pub link4_mhz: Option<f64>,
    #[serde(default)]
    pub tower_mhz: Option<f64>,
    #[serde(default)]
    pub recovery_tanker: Option<String>,
    /// players currently in the pattern / groove
    #[serde(default)]
    pub pattern: Vec<String>,
    #[serde(default)]
    pub case: u8,
    /// the true wind at the ship (anemometer height), from DCS
    #[serde(default)]
    pub true_wind_from_deg: f64,
    #[serde(default)]
    pub true_wind_kts: f64,
    /// landing area relative to the bow, degrees (FB - BRC; about -9 for a
    /// Nimitz or Forrestal, 0 for a straight deck)
    #[serde(default)]
    pub deck_angle_deg: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiveSpawn {
    pub id: String,
    pub item: String,
    pub label: String,
    pub owner_name: String,
    #[serde(default)]
    pub owner_ucid: Option<String>,
    pub pos: GeoPt,
    pub created: DateTime<Utc>,
    #[serde(default)]
    pub expires: Option<DateTime<Utc>>,
    #[serde(default)]
    pub units: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiveArena {
    pub id: String,
    pub name: String,
    pub pos: GeoPt,
    pub radius_m: f64,
    pub occupants: Vec<String>,
    /// "open" / "duel: A vs B"
    pub status: String,
}

// ---------------------------------------------------------------- spawning

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SpawnCategory {
    AirToAir,
    AirToGround,
    Tanker,
    Naval,
    Ground,
    Helo,
    Jtac,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ParamKind {
    Choice { options: Vec<ParamOption> },
    Number { min: f64, max: f64, step: f64, unit: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParamOption {
    pub value: String,
    pub label: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParamSpec {
    pub key: String,
    pub label: String,
    pub kind: ParamKind,
    pub default: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CatalogItem {
    pub id: String,
    pub category: SpawnCategory,
    pub label: String,
    pub description: String,
    pub params: Vec<ParamSpec>,
    /// only instructors / admins may spawn it
    #[serde(default)]
    pub instructor_only: bool,
    /// placed relative to the requesting player, who must be airborne
    #[serde(default)]
    pub relative_to_player: bool,
}

/// `query-range-catalog` RPC.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SpawnCatalog {
    pub items: Vec<CatalogItem>,
    pub max_active_per_player: u32,
    pub despawn_after_s: u32,
}

/// `range-spawn` RPC argument (as JSON) -- bfdb fills `ucid` from the
/// caller's session, never from the request body.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnRequest {
    pub ucid: String,
    pub item: String,
    #[serde(default)]
    pub params: BTreeMap<String, String>,
    /// bfdb sets this for dashboard admins; the engine also honours its own
    /// instructor list.
    #[serde(default)]
    pub instructor: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnReply {
    pub ok: bool,
    pub message: String,
    #[serde(default)]
    pub spawn_id: Option<String>,
}

/// Bomb ballistic data harvested from DCS's own weapon database in the hooks
/// state (`query-weapons` RPC), for the release calculator.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WeaponBallistics {
    pub name: String,
    #[serde(default)]
    pub display_name: String,
    pub mass_kg: f64,
    pub caliber_m: f64,
    #[serde(default)]
    pub length_m: f64,
    /// DCS `fm.cx_coeff`; the drag model behind it is undocumented, so the
    /// calculator fits against observed drops rather than trusting it.
    #[serde(default)]
    pub cx_coeff: Vec<f64>,
    /// DCS `targeting_data.char_time`: characteristic fall time, seconds
    #[serde(default)]
    pub char_time_s: Option<f64>,
    /// "unguided" / "guided"
    #[serde(default)]
    pub class: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WeaponDb {
    #[serde(default)]
    pub dcs_version: String,
    pub bombs: Vec<WeaponBallistics>,
}
