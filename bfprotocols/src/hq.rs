//! The theatre HQ as one coalition sees it (`query-hq`), the strategist's
//! directive (`hq-directive`), and the commands players and human commanders
//! send it (`hq-command`).
//!
//! Like the ground war's picture, the view is fog-of-war by construction:
//! the engine builds it for one side, from what that side can see. bfdb's
//! language-model strategist is handed exactly this and nothing else, so it
//! cannot cheat any more than a human commander reading the dashboard could.

use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// A point on the map: latitude, longitude.
pub type LatLon = [f64; 2];

/// How the side is fighting.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Posture {
    /// Take ground: the main effort is an enemy objective, ground and air
    /// go at it, troops are inserted where it can be captured.
    Offensive,
    /// Hold what is held, take what is cheap.
    Balanced,
    /// Hold: defend the threatened bases, keep the supply flowing, strike
    /// only what is attacking.
    Defensive,
}

impl Posture {
    pub fn label(self) -> &'static str {
        match self {
            Self::Offensive => "OFFENSIVE",
            Self::Balanced => "BALANCED",
            Self::Defensive => "DEFENSIVE",
        }
    }
}

/// A line of effort: what a share of the treasury goes on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Line {
    Air,
    Fires,
    Logistics,
    Troops,
    Ground,
}

impl Line {
    pub const ALL: [Line; 5] = [Line::Air, Line::Fires, Line::Logistics, Line::Troops, Line::Ground];

    pub fn label(self) -> &'static str {
        match self {
            Self::Air => "air",
            Self::Fires => "fires",
            Self::Logistics => "logistics",
            Self::Troops => "troops",
            Self::Ground => "ground",
        }
    }
}

/// One kind of operation the HQ can run.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OpKind {
    Cap,
    Strike,
    Sead,
    Recon,
    Artillery,
    MissileStrike,
    Ambush,
    Convoy,
    HeloSupply,
    HeloTroops,
    Reinforce,
    /// A heavy bomber on a friendly JTAC's target, escorted.
    Bomber,
    /// An airborne early-warning aircraft on station behind the front.
    Awacs,
    /// A tanker on station behind the front.
    Tanker,
    /// Cruise missiles from a friendly carrier group.
    NavalStrike,
    /// A transport flying logistics repair to a friendly base.
    AirRepair,
}

impl OpKind {
    pub const ALL: [OpKind; 16] = [
        OpKind::Cap,
        OpKind::Strike,
        OpKind::Sead,
        OpKind::Recon,
        OpKind::Artillery,
        OpKind::MissileStrike,
        OpKind::Ambush,
        OpKind::Convoy,
        OpKind::HeloSupply,
        OpKind::HeloTroops,
        OpKind::Reinforce,
        OpKind::Bomber,
        OpKind::Awacs,
        OpKind::Tanker,
        OpKind::NavalStrike,
        OpKind::AirRepair,
    ];

    pub fn line(self) -> Line {
        match self {
            Self::Cap | Self::Strike | Self::Sead | Self::Recon | Self::Bomber | Self::Awacs | Self::Tanker => {
                Line::Air
            }
            Self::Artillery | Self::MissileStrike | Self::Ambush | Self::NavalStrike => Line::Fires,
            Self::Convoy | Self::HeloSupply | Self::AirRepair => Line::Logistics,
            Self::HeloTroops => Line::Troops,
            Self::Reinforce => Line::Ground,
        }
    }

    pub fn label(self) -> &'static str {
        match self {
            Self::Cap => "CAP",
            Self::Strike => "strike",
            Self::Sead => "SEAD",
            Self::Recon => "recon",
            Self::Artillery => "artillery",
            Self::MissileStrike => "missile strike",
            Self::Ambush => "convoy ambush",
            Self::Convoy => "supply convoy",
            Self::HeloSupply => "helo supply run",
            Self::HeloTroops => "helo troop insertion",
            Self::Reinforce => "reinforcement convoy",
            Self::Bomber => "bomber strike",
            Self::Awacs => "AWACS",
            Self::Tanker => "tanker",
            Self::NavalStrike => "naval cruise missile strike",
            Self::AirRepair => "air logistics repair",
        }
    }
}

/// What a player can ask the HQ for.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RequestKind {
    /// Air support on an enemy objective.
    Cas,
    /// Fighter cover over a friendly objective.
    Cap,
    /// Suppression of the air defences near an objective.
    Sead,
    /// A look at an objective.
    Recon,
    /// Artillery or missiles on an enemy objective.
    Fires,
    /// Supply for a friendly objective.
    Supply,
    /// Troops inserted at an objective (to capture it, or to hold it).
    Troops,
    /// A tanker on station near a friendly objective.
    Tanker,
    /// AWACS on station near a friendly objective.
    Awacs,
}

impl RequestKind {
    pub const ALL: [RequestKind; 9] = [
        RequestKind::Cas,
        RequestKind::Cap,
        RequestKind::Sead,
        RequestKind::Recon,
        RequestKind::Fires,
        RequestKind::Supply,
        RequestKind::Troops,
        RequestKind::Tanker,
        RequestKind::Awacs,
    ];

    /// The operations that would answer this request.
    pub fn answered_by(self) -> &'static [OpKind] {
        match self {
            Self::Cas => &[OpKind::Strike, OpKind::Bomber],
            Self::Cap => &[OpKind::Cap],
            Self::Sead => &[OpKind::Sead],
            Self::Recon => &[OpKind::Recon],
            Self::Fires => &[OpKind::Artillery, OpKind::MissileStrike, OpKind::NavalStrike],
            Self::Supply => &[OpKind::Convoy, OpKind::HeloSupply, OpKind::AirRepair],
            Self::Troops => &[OpKind::HeloTroops, OpKind::Reinforce],
            Self::Tanker => &[OpKind::Tanker],
            Self::Awacs => &[OpKind::Awacs],
        }
    }

    /// Whether the request is about one of the side's own objectives (else
    /// an enemy one).
    pub fn on_friendly(self) -> bool {
        matches!(self, Self::Cap | Self::Supply | Self::Tanker | Self::Awacs)
    }

    pub fn label(self) -> &'static str {
        match self {
            Self::Cas => "CAS",
            Self::Cap => "CAP",
            Self::Sead => "SEAD",
            Self::Recon => "recon",
            Self::Fires => "fires",
            Self::Supply => "resupply",
            Self::Troops => "troops",
            Self::Tanker => "tanker",
            Self::Awacs => "AWACS",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        Some(match s.trim().to_ascii_lowercase().as_str() {
            "cas" | "strike" | "attack" => Self::Cas,
            "cap" | "cover" | "fighters" => Self::Cap,
            "sead" | "dead" => Self::Sead,
            "recon" | "drone" | "isr" => Self::Recon,
            "fires" | "arty" | "artillery" | "missile" | "missiles" => Self::Fires,
            "supply" | "resupply" | "logistics" | "logi" => Self::Supply,
            "troops" | "infantry" | "insert" => Self::Troops,
            "tanker" | "fuel" | "refuel" | "aar" => Self::Tanker,
            "awacs" | "aew" | "picture" => Self::Awacs,
            _ => return None,
        })
    }
}

/// Strategy from outside the engine: bfdb's language-model strategist, or a
/// human commander. Every field is optional -- whatever is left out the HQ
/// decides itself. Objective ids are the engine's; anything that names an
/// objective on the wrong side for its field is dropped.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Directive {
    #[serde(default)]
    pub posture: Option<Posture>,
    /// The enemy objective to concentrate on.
    #[serde(default)]
    pub main_effort: Option<u64>,
    /// Friendly objectives to hold at all costs, most important first.
    #[serde(default)]
    pub defend: Vec<u64>,
    /// Friendly objectives to resupply first.
    #[serde(default)]
    pub supply_priority: Vec<u64>,
    /// Objectives not to send anything at (a SAM belt to stay out of, a
    /// base not worth the losses).
    #[serde(default)]
    pub avoid: Vec<u64>,
    /// Emphasis on each line of effort, 1.0 = normal. Clamped by the engine.
    #[serde(default)]
    pub weights: BTreeMap<Line, f64>,
    /// The commander's intent, one or two sentences, shown to the side's
    /// players as written.
    #[serde(default)]
    pub intent: Option<String>,
    /// Why -- for the dashboard and the log, not shown in game.
    #[serde(default)]
    pub rationale: Option<String>,
    /// How long this stands, seconds. Clamped by the engine.
    #[serde(default)]
    pub ttl_secs: Option<u32>,
}

/// What the dashboard, chat or a support request asks of the HQ. The caller
/// is identified separately (their ucid, resolved by bfdb from their login);
/// the engine decides what they may do.
///
/// On the wire it is `{"kind": "...", ...fields}`. Deserializing is written
/// out by hand: serde's internally-tagged enums buffer their fields, and with
/// serde_json's `arbitrary_precision` on (a dependency turns it on for the
/// whole workspace) a buffered float comes back as a map, so any directive
/// carrying line weights failed to parse.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum HqCommand {
    /// Take command: posture, main effort and the rest override both the
    /// strategist and the HQ's own judgement until `ttl_secs` or a clear.
    Override {
        #[serde(default)]
        directive: Directive,
        /// Operations the HQ must not start.
        #[serde(default)]
        disabled_ops: Vec<OpKind>,
        /// Stop planning altogether (what is under way carries on).
        #[serde(default)]
        paused: bool,
    },
    ClearOverride,
    /// Call off one of the HQ's operations.
    CancelOp { op: u64 },
    /// Ask for support. `objective` defaults to the one nearest the player.
    Request {
        request: RequestKind,
        #[serde(default)]
        objective: Option<u64>,
    },
    CancelRequest { request_id: u64 },
}

impl<'de> serde::Deserialize<'de> for HqCommand {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        use serde::de::Error;
        #[derive(Deserialize)]
        struct OverrideArgs {
            #[serde(default)]
            directive: Directive,
            #[serde(default)]
            disabled_ops: Vec<OpKind>,
            #[serde(default)]
            paused: bool,
        }
        #[derive(Deserialize)]
        struct CancelOpArgs {
            op: u64,
        }
        #[derive(Deserialize)]
        struct RequestArgs {
            request: RequestKind,
            #[serde(default)]
            objective: Option<u64>,
        }
        #[derive(Deserialize)]
        struct CancelRequestArgs {
            request_id: u64,
        }
        let mut v = serde_json::Value::deserialize(d)?;
        let kind = v
            .get("kind")
            .and_then(|k| k.as_str())
            .ok_or_else(|| D::Error::custom("an HQ command needs a \"kind\""))?
            .to_owned();
        if let Some(o) = v.as_object_mut() {
            o.remove("kind");
        }
        let e = D::Error::custom;
        Ok(match kind.as_str() {
            "override" => {
                let a: OverrideArgs = serde_json::from_value(v).map_err(e)?;
                Self::Override { directive: a.directive, disabled_ops: a.disabled_ops, paused: a.paused }
            }
            "clear_override" => Self::ClearOverride,
            "cancel_op" => {
                let a: CancelOpArgs = serde_json::from_value(v).map_err(e)?;
                Self::CancelOp { op: a.op }
            }
            "request" => {
                let a: RequestArgs = serde_json::from_value(v).map_err(e)?;
                Self::Request { request: a.request, objective: a.objective }
            }
            "cancel_request" => {
                let a: CancelRequestArgs = serde_json::from_value(v).map_err(e)?;
                Self::CancelRequest { request_id: a.request_id }
            }
            k => return Err(D::Error::custom(format!("unknown HQ command \"{k}\""))),
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HqReply {
    pub ok: bool,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjRef {
    pub id: u64,
    pub name: String,
    pub pos: LatLon,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirectiveInfo {
    pub directive: Directive,
    /// RFC 3339.
    pub received: String,
    pub expires: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OverrideInfo {
    pub by: String,
    pub set: String,
    pub expires: String,
    pub directive: Directive,
    pub disabled_ops: Vec<OpKind>,
    pub paused: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpInfo {
    pub id: u64,
    pub kind: OpKind,
    pub line: Line,
    pub target: Option<u64>,
    pub target_name: String,
    pub pos: LatLon,
    pub started: String,
    pub cost: i64,
    /// "active" | "succeeded" | "failed" | "cancelled"
    pub status: String,
    pub detail: String,
    /// The support request it answers, if any.
    pub request: Option<u64>,
    /// The flights flying with it (escorts, SEAD), as "escort" / "sead".
    #[serde(default)]
    pub support: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequestInfo {
    pub id: u64,
    pub kind: RequestKind,
    pub by: String,
    pub target: u64,
    pub target_name: String,
    pub created: String,
    /// "open" | "answered" | "declined" | "expired"
    pub status: String,
    pub answer: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecordInfo {
    pub kind: OpKind,
    pub launched: u32,
    pub succeeded: u32,
    pub failed: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogInfo {
    pub at: String,
    pub text: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjInfo {
    pub id: u64,
    pub name: String,
    /// "airbase" | "fob" | "farp" | "logistics" | "factory" | ...
    pub kind: String,
    /// "own" | "enemy" | "neutral"
    pub owner: String,
    pub pos: LatLon,
    pub health: u8,
    pub logi: u8,
    pub supply: u8,
    pub fuel: u8,
    pub threatened: bool,
    pub being_captured: bool,
    /// No logistics left: it falls to the first troops that reach it.
    pub capturable: bool,
    /// Distance to the nearest objective of the other side, km.
    pub front_km: f64,
    /// Something of ours is on its way to it (a convoy, a helo, a
    /// formation).
    pub inbound: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContactInfo {
    pub pos: LatLon,
    /// "sam" | "armor" | "infantry" | "artillery" | "aircraft" | ...
    pub class: String,
    pub count: u32,
    /// Objective it is nearest, and how far from it, km.
    pub near: String,
    pub near_km: f64,
    pub age_mins: u32,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PictureInfo {
    /// Share of the objectives the side holds, percent.
    pub territory_pct: f64,
    pub own_objectives: u32,
    pub enemy_objectives: u32,
    pub objectives: Vec<ObjInfo>,
    pub humans: u32,
    pub humans_fixed_wing_airborne: u32,
    pub humans_helo_airborne: u32,
    /// Enemy aircraft the side's sensors hold right now.
    pub enemy_air_detected: u32,
    /// Enemy ground the side's intel holds (recon, JTAC, special forces).
    pub enemy_ground: Vec<ContactInfo>,
    pub enemy_sams: Vec<ContactInfo>,
    pub formations: u32,
    pub formations_idle: u32,
    pub enemy_formations_in_contact: u32,
    pub ai_air_up: u32,
    pub logistics_out: u32,
    pub troops_out: u32,
}

/// The HQ of one side, as that side sees it.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HqView {
    /// "Blue" / "Red".
    pub side: String,
    pub enabled: bool,
    pub paused: bool,
    pub posture: Option<Posture>,
    /// Whose strategy is in force: "rules" | "strategist" | "human".
    pub source: String,
    pub main_effort: Option<ObjRef>,
    pub defend: Vec<ObjRef>,
    pub supply_priority: Vec<ObjRef>,
    pub avoid: Vec<ObjRef>,
    pub weights: BTreeMap<Line, f64>,
    /// The commander's intent, as players see it.
    pub intent: String,
    /// Why the HQ's own rules chose what they chose.
    pub reasons: Vec<String>,
    pub directive: Option<DirectiveInfo>,
    #[serde(rename = "override")]
    pub override_: Option<OverrideInfo>,
    pub treasury: i64,
    pub reserve: i64,
    /// How much the HQ is doing given the side's player count, 0..1.
    pub gap_factor: f64,
    /// The cost of each operation the side can run (ones it has no action
    /// or system for are missing).
    pub available: BTreeMap<OpKind, i64>,
    pub ops: Vec<OpInfo>,
    pub requests: Vec<RequestInfo>,
    pub record: Vec<RecordInfo>,
    pub log: Vec<LogInfo>,
    pub picture: PictureInfo,
    /// Seconds until the next planning pass.
    pub next_think_secs: i64,
    /// Whether the viewer the view was built for may override the HQ.
    #[serde(default)]
    pub can_command: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn commands_parse_from_dashboard_json() {
        let c: HqCommand = serde_json::from_str(
            r#"{"kind":"override","directive":{"posture":"defensive","main_effort":12,"weights":{"air":2.0}},"disabled_ops":["missile_strike"]}"#,
        )
        .unwrap();
        match c {
            HqCommand::Override { directive, disabled_ops, paused } => {
                assert_eq!(directive.posture, Some(Posture::Defensive));
                assert_eq!(directive.main_effort, Some(12));
                assert_eq!(directive.weights.get(&Line::Air), Some(&2.0));
                assert_eq!(disabled_ops, vec![OpKind::MissileStrike]);
                assert!(!paused);
            }
            _ => panic!("wrong command"),
        }
        let c: HqCommand = serde_json::from_str(r#"{"kind":"request","request":"cas"}"#).unwrap();
        assert!(matches!(c, HqCommand::Request { request: RequestKind::Cas, objective: None }));
        assert!(serde_json::from_str::<HqCommand>(r#"{"kind":"nuke"}"#).is_err());
        // And back: what the engine would send out round-trips.
        let c = HqCommand::Override {
            directive: Directive { weights: [(Line::Fires, 0.5)].into_iter().collect(), ..Default::default() },
            disabled_ops: vec![],
            paused: true,
        };
        let back: HqCommand = serde_json::from_str(&serde_json::to_string(&c).unwrap()).unwrap();
        assert!(matches!(back, HqCommand::Override { paused: true, .. }));
    }

    #[test]
    fn a_strategist_reply_with_extra_chatter_still_parses() {
        let d: Directive = serde_json::from_str(
            r#"{"posture":"offensive","main_effort":3,"intent":"Take Gori.","confidence":"high"}"#,
        )
        .unwrap();
        assert_eq!(d.main_effort, Some(3));
        assert!(d.defend.is_empty());
    }

    #[test]
    fn every_request_has_an_answer() {
        for r in RequestKind::ALL {
            assert!(!r.answered_by().is_empty());
            assert_eq!(RequestKind::parse(r.label()), Some(r));
        }
    }
}
