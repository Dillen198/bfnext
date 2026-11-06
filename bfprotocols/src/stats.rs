use crate::{
    cfg::{Cfg, LifeType, UnitTags, Vehicle},
    db::{
        group::{GroupId, UnitId},
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::PerfInner,
    shots::Dead,
};
use chrono::prelude::*;
use dcso3::{
    coalition::Side,
    coord::LLPos,
    net::{SlotId, Ucid},
    perf::{HistogramSer, PerfInner as ApiPerfInner},
    warehouse::LiquidType,
    String, Vector3,
};
use enumflags2::bitflags;
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
use std::fmt;

/// the path in the archive of the stats
pub const PATH: &str = "/stats";

pub type MapS<K, V> = immutable_chunkmap::map::Map<K, V, 16>;

#[derive(
    Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize,
)]
pub enum EnId {
    Player(Ucid),
    Unit(UnitId),
}

impl fmt::Display for EnId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Unit {
    pub typ: Vehicle,
    pub tags: UnitTags,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Pos {
    pub pos: LLPos,
    pub velocity: Vector3,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[bitflags]
#[repr(u8)]
pub enum DetectionSource {
    EWR,
    Jtac,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Stat {
    NewRound {
        sortie: String,
    },
    RoundEnd {
        winner: Option<Side>,
    },
    SessionStart {
        stop: Option<DateTime<Utc>>,
        cfg: Box<Cfg>,
    },
    SessionEnd {
        api_perf: ApiPerfInner,
        perf: PerfInner,
        frame: HistogramSer,
    },
    Objective {
        name: String,
        id: ObjectiveId,
        pos: LLPos,
        owner: Side,
        kind: ObjectiveKind,
    },
    Capture {
        id: ObjectiveId,
        by: SmallVec<[Ucid; 1]>,
        side: Side,
    },
    Repair {
        id: ObjectiveId,
        by: Ucid,
    },
    SupplyTransfer {
        from: ObjectiveId,
        to: ObjectiveId,
        by: Ucid,
    },
    EquipmentInventory {
        id: ObjectiveId,
        item: String,
        amount: u32,
    },
    LiquidInventory {
        id: ObjectiveId,
        item: LiquidType,
        amount: u32,
    },
    Action {
        by: Ucid,
        gid: Option<GroupId>,
        action: String,
    },
    DeployTroop {
        by: Ucid,
        troop: String,
        gid: GroupId,
    },
    DeployGroup {
        by: Ucid,
        gid: GroupId,
        deployable: String,
        /// Delivering aircraft's DCS type name, when known (physical C-130/
        /// helo cargo deploys only -- None for instant/admin-triggered ones).
        #[serde(default)]
        aircraft: Option<String>,
        /// "AirDrop" (auto-unpacked on landing) or "ManualUnpack" (player
        /// had to walk up and trigger it), when known.
        #[serde(default)]
        method: Option<String>,
    },
    DeployFarp {
        by: Ucid,
        oid: ObjectiveId,
        deployable: String,
    },
    ObjectiveHealth {
        id: ObjectiveId,
        last_change: DateTime<Utc>,
        health: u8,
        logi: u8,
    },
    ObjectiveSupply {
        id: ObjectiveId,
        supply: u8,
        fuel: u8,
    },
    ObjectiveDestroyed {
        id: ObjectiveId,
    },
    Register {
        name: String,
        id: Ucid,
        side: Side,
        initial_points: i32,
    },
    Sideswitch {
        id: Ucid,
        side: Side,
    },
    Connect {
        id: Ucid,
        addr: String,
        name: String,
    },
    Disconnect {
        id: Ucid,
    },
    Slot {
        id: Ucid,
        slot: SlotId,
        typ: Option<Unit>,
    },
    Deslot {
        id: Ucid,
    },
    Unit {
        id: EnId,
        gid: Option<GroupId>,
        owner: Side,
        typ: Unit,
        pos: Pos,
    },
    GroupDeleted {
        id: GroupId,
    },
    Position {
        id: EnId,
        pos: Pos,
    },
    Detected {
        id: EnId,
        detected: bool,
        source: DetectionSource,
    },
    Takeoff {
        id: Ucid,
    },
    Land {
        id: Ucid,
    },
    Life {
        id: Ucid,
        lives: MapS<LifeType, (DateTime<Utc>, u8)>,
    },
    Kill(Dead),
    Points {
        id: Ucid,
        points: i32,
        reason: String,
    },
    PointsTransfer {
        from: Ucid,
        to: Ucid,
        points: u32,
    },
    PointsTransferToObjective {
        from: Ucid,
        to: ObjectiveId,
        points: u32,
    },
    Bind {
        id: Ucid,
        token: String,
    },
    ConvoyDestroyed {
        from: ObjectiveId,
        to: ObjectiveId,
        side: Side,
        killer: Option<Ucid>,
    },
    AirRouteDelivered {
        from: ObjectiveId,
        to: ObjectiveId,
        side: Side,
    },
    AirRouteDestroyed {
        from: ObjectiveId,
        to: ObjectiveId,
        side: Side,
    },
    SeaRouteDelivered {
        from: ObjectiveId,
        to: ObjectiveId,
        side: Side,
    },
    SeaRouteDestroyed {
        from: ObjectiveId,
        to: ObjectiveId,
        side: Side,
    },
    CampaignEvent {
        event_type: String,
        side: Side,
    },
    PilotXp {
        id: Ucid,
        xp: u32,
        reason: String,
    },
    Weather {
        temp_c: f64,
        wind_speed_kts: f64,
        wind_from_deg: f64,
        cloud_base_m: f64,
        qnh_hpa: f64,
        /// Cloud density 0–10 (0 = clear, 10 = overcast). None if not available.
        #[serde(default)]
        cloud_density: Option<u8>,
        /// Horizontal visibility in metres (capped at 10 000 = unlimited). None if no fog.
        #[serde(default)]
        visibility_m: Option<f64>,
    },
    GciPicture(crate::gci::GciPicture),
    /// One line of a side's combat log (the command map's feed: contacts,
    /// battles, losses, captures, orders, garrison reactions, naval
    /// hunters). bfdb keeps every one for the whole round, so the log
    /// survives restarts and is only cleared by a campaign reset. Appended
    /// last: bfdb's bincode records number variants by position.
    CombatLog {
        side: Side,
        kind: String,
        text: String,
        /// [lat, lon], where it happened.
        #[serde(default)]
        pos: Option<[f64; 2]>,
        #[serde(default)]
        formation: Option<u32>,
    },
    /// Both sides' war will (`Cfg::war_will`), sent when it moves (at most
    /// about once a minute unless a side moves by a point or more) and now
    /// and then while it doesn't. bfdb keeps the latest per round for
    /// `/api/stats`. Appended last: bfdb's bincode records number variants
    /// by position.
    WarWill(WarWillStat),
    /// A story event fired (`Cfg::story`, ENGINE.md 3.4): what happened,
    /// who caused it and what it changed. Sent once per firing; bfdb keeps
    /// the round's events for `/api/stats` and the news. Appended last:
    /// bfdb's bincode records number variants by position.
    StoryEvent(StoryEventStat),
    /// Each side's air order of battle (`Cfg::ai_air_stock`, ENGINE.md
    /// 3.1): its airframes in stock by type and those lost this round. Sent
    /// when it changes, at most about once a minute, and now and then while
    /// it doesn't. bfdb keeps the latest per round for `/api/oob`. Appended
    /// last: bfdb's bincode records number variants by position.
    AirOrderOfBattle(AirOobStat),
}

/// One `Stat::AirOrderOfBattle`.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct AirOobStat {
    pub blue: Vec<AirframeCount>,
    pub red: Vec<AirframeCount>,
}

/// One aircraft type in a side's order of battle.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct AirframeCount {
    /// The DCS type name: "F-14A", "Tu-22M3".
    pub typ: String,
    /// In the warehouses of the bases the side holds, unlimited ones left
    /// out.
    pub stock: u32,
    /// Lost this round: player aircraft, and AI aircraft that came out of
    /// the stock.
    pub lost: u32,
    /// An `unlimited_aircraft` base of the side holds the type: it never
    /// runs out whatever `stock` says.
    pub unlimited: bool,
}

/// One `Stat::StoryEvent`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StoryEventStat {
    /// The event's id in the config: "tuapse_burns".
    pub id: String,
    pub time: DateTime<Utc>,
    pub title: String,
    /// The story as players were told it, placeholders filled in.
    pub text: String,
    /// The pilots (or AI) credited with setting it off.
    pub caused_by: Vec<String>,
    /// The side the event concerns, if any.
    pub side: Option<Side>,
    /// What it changed, one line each: "Red fuel output -40%".
    pub effects: Vec<String>,
}

/// One `Stat::WarWill`.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct WarWillStat {
    pub blue: f64,
    pub red: f64,
    /// Change over the last 3 h (or as much of it as is known, at least
    /// 1 h); None until then.
    pub blue_trend: Option<f64>,
    pub red_trend: Option<f64>,
    /// Below this a side is breaking; held for `hold_hours`, it asks for
    /// terms.
    pub threshold: f64,
    /// The biggest recent causes, newest first.
    pub causes: Vec<WillCause>,
}

/// Something that moved a side's will: "Tuapse refinery destroyed", -5.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WillCause {
    pub side: Side,
    pub text: String,
    pub delta: f64,
    pub time: DateTime<Utc>,
}
