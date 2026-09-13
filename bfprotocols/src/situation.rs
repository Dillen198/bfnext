//! Auto-generated situational briefing — "what is going on right now, for my
//! coalition".
//!
//! The engine (`bflib::situation`) fuses the campaign's own state (objective
//! ownership, health, supply, capture timers, treasury, convoys) with the
//! asking coalition's *earned* sensor picture (EWR/AWACS air tracks and the
//! recon/ELINT [`IntelDatabase`]) into one [`SituationReport`] per side, and
//! turns that into a ranked list of [`Task`]s. The same report backs three
//! consumers, so a player reads exactly one story wherever they look:
//!
//! * the slot-entry panel and F10 → Info → Situation pages in game,
//! * `GET /api/situation` → the dashboard BRIEFING page (map + panels),
//! * the kneeboard PDF the dashboard exports.
//!
//! Fog of war: a report is built *for one side* and only ever contains what
//! that side can see. Objective ownership and health are campaign-public (the
//! F10 map shows them to everyone), but threat rings, air tracks, own
//! artillery, hub stocks and tasking are the asking side's alone. `bfdb`
//! resolves the viewer's session cookie to a coalition before it hands one
//! over, so a browser can never pull the other side's picture.

use chrono::prelude::*;
use dcso3::coalition::Side;
use serde::{Deserialize, Serialize};

/// How badly something wants attention. Drives ordering and colour everywhere
/// the report is rendered.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Urgency {
    /// Losing something this sortie if nobody acts.
    Critical,
    /// The thing that decides the next hour.
    High,
    /// Worth doing, not on fire.
    Routine,
}

impl Urgency {
    pub fn label(self) -> &'static str {
        match self {
            Self::Critical => "CRITICAL",
            Self::High => "HIGH",
            Self::Routine => "ROUTINE",
        }
    }
}

/// Which kind of sortie a task is asking for. The client maps this to an icon
/// and the in-game renderer to a short tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TaskKind {
    /// Hold a friendly objective that is being taken or is takeable.
    Defend,
    /// Finish taking an enemy objective that is already eligible.
    Capture,
    /// Knock an enemy objective down far enough to be captured.
    Strike,
    /// Kill the air defence blocking a push.
    Sead,
    /// Kill fielded enemy ground forces, usually with a JTAC on them.
    Cas,
    /// Meet inbound hostile air.
    Intercept,
    /// Move supply / troops / crates.
    Logistics,
    /// Go look — the coalition has no picture of somewhere that matters.
    Recon,
    /// Pick a downed pilot up.
    Csar,
}

impl TaskKind {
    pub fn label(self) -> &'static str {
        match self {
            Self::Defend => "DEFEND",
            Self::Capture => "CAPTURE",
            Self::Strike => "STRIKE",
            Self::Sead => "SEAD",
            Self::Cas => "CAS",
            Self::Intercept => "INTERCEPT",
            Self::Logistics => "LOGISTICS",
            Self::Recon => "RECON",
            Self::Csar => "CSAR",
        }
    }
}

/// One thing the coalition should be doing, with enough context that a pilot
/// can fly it without asking anyone.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    /// Stable within a report: `"<kind>-<objective>"`. Lets the client keep a
    /// row expanded across refreshes.
    pub id: String,
    pub kind: TaskKind,
    pub urgency: Urgency,
    /// Imperative one-liner: `"DEFEND Gudauta"`.
    pub title: String,
    /// Why this is on the list, in plain language — the state that generated
    /// it, not a restatement of the title.
    pub detail: String,
    /// What "done" looks like, when it can be stated concretely.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub success: Option<String>,
    /// Objective this hangs off, when it hangs off one.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub objective: Option<String>,
    pub lat: f64,
    pub lon: f64,
    /// Bearing/range from the asking player's own jet — filled in only for the
    /// in-game render, where there is a player to measure from.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bearing_deg: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub range_nm: Option<f64>,
    /// Airframe/role hints: `["SEAD", "Strike"]`.
    #[serde(default)]
    pub roles: Vec<String>,
}

/// Who is winning, and by how much.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Posture {
    pub friendly_objectives: u32,
    pub enemy_objectives: u32,
    pub neutral_objectives: u32,
    /// Airbases, naval bases and FARPs only — the ones that actually generate
    /// sorties.
    pub friendly_primary: u32,
    pub enemy_primary: u32,
    /// Friendly share of all non-neutral objectives, 0–100.
    pub territory_pct: f64,
    /// Objectives that changed hands in the last hour: (gained, lost).
    pub gained_recent: u32,
    pub lost_recent: u32,
    /// Coalition points available to the commander.
    pub treasury: i64,
    pub players_friendly: u32,
    pub players_enemy: u32,
    /// Set while a last-stand countdown is running.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub last_stand: Option<String>,
    /// How the round is won, when the config states one.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub victory_condition: Option<String>,
}

/// Surface weather at the theatre reference point.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SituationWeather {
    pub wind_from_deg: u32,
    pub wind_kts: f64,
    pub temp_c: f64,
    pub qnh_inhg: f64,
    pub qnh_hpa: f64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cloud_base_m: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub visibility_m: Option<f64>,
    pub precip: bool,
    /// One-line airman's read: `"VFR, 8kt crosswind, 10km vis"`.
    pub summary: String,
}

/// A place where the campaign is actually being decided right now: a
/// friendly objective under threat, or an enemy one that is takeable.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Hotspot {
    pub objective: String,
    pub kind: String,
    pub owner: Side,
    pub lat: f64,
    pub lon: f64,
    pub health: u8,
    pub logi: u8,
    pub supply: u8,
    /// Enemy units are within sight of the objective.
    pub threatened: bool,
    /// A capture timer is running: `(capturing side, seconds held, seconds needed)`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub capture_progress: Option<(Side, i64, i64)>,
    /// Meets the capture preconditions right now.
    pub captureable: bool,
    /// Post-capture consolidation hold.
    pub in_capture_hold: bool,
    /// Plain-language read of what is blocking or driving a capture here —
    /// straight from the engine's capture advisor, so it can't drift from
    /// what the engine enforces.
    pub status: String,
    /// Will this objective heal back out of reach, and what is stopping it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub repair_outlook: Option<String>,
    pub risk: Urgency,
}

/// A known enemy air-defence area, from the asking side's intel only.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThreatArea {
    /// What the intel actually supports — never the exact type unless a JTAC
    /// or recon flight has eyes on it.
    pub label: String,
    pub lat: f64,
    pub lon: f64,
    /// Engagement ring to draw, metres. `None` when the emitter is unidentified.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub radius_m: Option<f64>,
    /// 1-sigma position uncertainty, metres.
    pub uncertainty_m: f32,
    /// 0.0–1.0.
    pub confidence: f32,
    /// `"recon"`, `"jtac"`, `"ewr"`, …
    pub source: String,
    pub age_s: u32,
    pub count: u8,
    /// Nearest friendly or enemy objective, for a human-readable anchor.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub near: Option<String>,
}

/// Coarse read of the air picture the coalition's sensors are holding.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AirPicture {
    pub hostile_tracks: u32,
    pub friendly_airborne: u32,
    /// Hostile tracks the radar net has lost but is still coasting.
    pub stale_tracks: u32,
    /// Where the hostile air is, as a compass sector plus a count:
    /// `"3 hostile groups NE"`. `None` when the scope is clean.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub axis: Option<String>,
    /// Closest hostile track to friendly territory, bearing/range from the
    /// nearest friendly objective.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub nearest: Option<AirThreat>,
    /// True when the coalition has no working radar at all — every call in
    /// the report is then blind to air.
    pub radar_blind: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AirThreat {
    pub lat: f64,
    pub lon: f64,
    pub alt_ft: i32,
    pub heading: u32,
    pub speed_kts: u32,
    /// `"fighter"`, `"helo"`, …
    pub class: String,
    /// Objective the track is closest to.
    pub near: String,
    pub bearing_deg: u32,
    pub range_nm: f64,
}

/// A friendly logistics hub and how much it has left to give.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HubState {
    pub objective: String,
    pub lat: f64,
    pub lon: f64,
    pub supply: u8,
    pub fuel: u8,
    pub health: u8,
    pub logi: u8,
    /// Objectives this hub is currently feeding.
    pub feeding: u32,
    pub threatened: bool,
}

/// A friendly objective that cannot sustain itself.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplyGap {
    pub objective: String,
    pub lat: f64,
    pub lon: f64,
    pub supply: u8,
    pub fuel: u8,
    pub health: u8,
    /// Why it matters: `"repair stalled — needs 10% per pulse"`.
    pub note: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogisticsPosture {
    pub hubs: Vec<HubState>,
    pub gaps: Vec<SupplyGap>,
    pub convoys_active: u32,
    /// What the logistics state machine is doing right now.
    pub stage: String,
}

/// An on-station support asset, with where it is as well as how to call it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupportStation {
    pub label: String,
    /// `"AWACS"`, `"TANKER"`, `"JTAC"`, `"GCI"`.
    pub kind: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub freq_mhz: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tacan: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lat: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lon: Option<f64>,
}

/// One line of the coalition comms card, as flown. Starts as the configured
/// plan entry ([`crate::cfg::CommsChannelCfg`]) and gets `live` / `note`
/// filled in where the engine can match a real on-station asset to it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommsChannel {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub preset: Option<u8>,
    pub label: String,
    pub freq_mhz: f64,
    /// "AM" or "FM".
    pub modulation: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub purpose: Option<String>,
    /// There is something on this channel right now — an airborne AWACS or
    /// tanker, an active JTAC. A planned-but-empty channel is `false`.
    pub live: bool,
    /// Live detail when `live`: TACAN, laser code, the station's real name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
}

/// Something that happened recently and changed the picture.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SituationEvent {
    pub at: DateTime<Utc>,
    pub text: String,
    /// `true` = good for the asking side, `false` = bad, `None` = neutral.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub good: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lat: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lon: Option<f64>,
}

/// Every objective, positioned, for the dashboard briefing map. Ownership and
/// condition are campaign-public (the in-game F10 map shows both sides);
/// `supply`/`fuel` are filled in for friendly objectives only.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MapObjective {
    pub name: String,
    pub kind: String,
    pub owner: Side,
    pub lat: f64,
    pub lon: f64,
    pub health: u8,
    pub logi: u8,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub supply: Option<u8>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fuel: Option<u8>,
    pub threatened: bool,
    pub captureable: bool,
    pub priority: bool,
    /// Generates sorties (airbase / naval base / FARP).
    pub primary: bool,
}

/// The whole picture for one coalition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SituationReport {
    pub side: Side,
    pub generated: DateTime<Utc>,
    /// In-mission clock, `HH:MM` local mission time.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mission_time: Option<String>,
    /// Two or three sentences a pilot can read on the ramp and know what the
    /// round is about. Written by the engine from everything below.
    pub headline: String,
    pub posture: Posture,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub weather: Option<SituationWeather>,
    pub tasking: Vec<Task>,
    pub hotspots: Vec<Hotspot>,
    pub threats: Vec<ThreatArea>,
    pub air: AirPicture,
    pub logistics: LogisticsPosture,
    pub support: Vec<SupportStation>,
    /// The coalition comms card: the configured frequency plan with live
    /// stations matched onto it.
    #[serde(default)]
    pub comms: Vec<CommsChannel>,
    /// Numbered intra-flight channels, `("FLIGHT 1", 305.0)`.
    #[serde(default)]
    pub flight_channels: Vec<(String, f64)>,
    /// Recent campaign events. The engine fills this from its own in-session
    /// log; `bfdb` may extend it from the persisted capture history.
    #[serde(default)]
    pub recent: Vec<SituationEvent>,
    /// Positioned objectives for the briefing map. Empty on the in-game path
    /// (the F10 map already draws them) — populated for the dashboard.
    #[serde(default)]
    pub map: Vec<MapObjective>,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Golden wire contract — the JSON `/api/situation` serves must stay in
    /// sync with the hand-written TS types in `bfweb/src/api.ts`
    /// (`SituationReport` and friends).
    #[test]
    fn situation_json_shape() {
        let r = SituationReport {
            side: Side::Blue,
            generated: DateTime::<Utc>::UNIX_EPOCH,
            mission_time: Some("14:32".into()),
            headline: "Blue holds 11 of 18 objectives.".into(),
            posture: Posture {
                friendly_objectives: 11,
                enemy_objectives: 6,
                neutral_objectives: 1,
                friendly_primary: 4,
                enemy_primary: 3,
                territory_pct: 64.7,
                gained_recent: 1,
                lost_recent: 0,
                treasury: 2400,
                players_friendly: 7,
                players_enemy: 5,
                last_stand: None,
                victory_condition: Some("hold every primary objective".into()),
            },
            weather: None,
            tasking: vec![Task {
                id: "defend-Gudauta".into(),
                kind: TaskKind::Defend,
                urgency: Urgency::Critical,
                title: "DEFEND Gudauta".into(),
                detail: "capture timer running".into(),
                success: Some("kill the troops in the zone".into()),
                objective: Some("Gudauta".into()),
                lat: 43.1,
                lon: 40.6,
                bearing_deg: None,
                range_nm: None,
                roles: vec!["CAS".into()],
            }],
            hotspots: vec![],
            threats: vec![ThreatArea {
                label: "air defence".into(),
                lat: 43.0,
                lon: 40.2,
                radius_m: Some(35_000.0),
                uncertainty_m: 800.0,
                confidence: 0.8,
                source: "recon".into(),
                age_s: 120,
                count: 4,
                near: Some("Sukhumi".into()),
            }],
            air: AirPicture {
                hostile_tracks: 3,
                friendly_airborne: 5,
                stale_tracks: 1,
                axis: Some("3 hostile tracks NE".into()),
                nearest: None,
                radar_blind: false,
            },
            logistics: LogisticsPosture {
                hubs: vec![],
                gaps: vec![],
                convoys_active: 2,
                stage: "ManageConvoys".into(),
            },
            support: vec![],
            comms: vec![CommsChannel {
                preset: Some(1),
                label: "AWACS / GCI -- MAGIC".into(),
                freq_mhz: 251.0,
                modulation: "AM".into(),
                purpose: Some("Primary control".into()),
                live: true,
                note: Some("52Y".into()),
            }],
            flight_channels: vec![("FLIGHT 1".into(), 305.0)],
            recent: vec![],
            map: vec![],
        };
        let v = serde_json::to_value(&r).unwrap();
        assert_eq!(v["side"], "Blue");
        assert_eq!(v["tasking"][0]["kind"], "defend");
        assert_eq!(v["tasking"][0]["urgency"], "critical");
        assert_eq!(v["threats"][0]["radius_m"], 35_000.0);
        assert_eq!(v["comms"][0]["freq_mhz"], 251.0);
        assert_eq!(v["comms"][0]["live"], true);
        assert_eq!(v["flight_channels"][0][0], "FLIGHT 1");
        assert!(
            v["weather"].is_null() || v.get("weather").is_none(),
            "None weather must be omitted"
        );
        assert!(
            v["tasking"][0].get("bearing_deg").is_none(),
            "None bearing must be omitted"
        );
        let back: SituationReport = serde_json::from_value(v).unwrap();
        assert_eq!(back.tasking.len(), 1);
    }

    #[test]
    fn urgency_orders_worst_first() {
        let mut v = vec![Urgency::Routine, Urgency::Critical, Urgency::High];
        v.sort();
        assert_eq!(v, vec![Urgency::Critical, Urgency::High, Urgency::Routine]);
    }
}
