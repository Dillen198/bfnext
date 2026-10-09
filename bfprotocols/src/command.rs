//! The command system's wire types, shared by the engine, bfdb and the
//! dashboard.

use dcso3::{coalition::Side, net::Ucid};
use serde_derive::{Deserialize, Serialize};

/// Who commands each coalition right now, as bfdb works it out (rank or an
/// admin's grant, minus revocations) and pushes to the engine with the
/// `set-commanders` RPC.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Commanders {
    #[serde(default)]
    pub blue: Vec<Ucid>,
    #[serde(default)]
    pub red: Vec<Ucid>,
}

impl Commanders {
    pub fn side_of(&self, ucid: &Ucid) -> Option<Side> {
        if self.blue.contains(ucid) {
            Some(Side::Blue)
        } else if self.red.contains(ucid) {
            Some(Side::Red)
        } else {
            None
        }
    }
}

/// An admin's decision about one pilot's commander access, overriding rank.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CommanderGrant {
    /// A commander whatever their rank.
    Granted,
    /// Never a commander, whatever their rank.
    Revoked,
}

/// One pilot's standing, for the dashboard and the Discord bot.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommanderStatus {
    pub ucid: Ucid,
    pub name: String,
    /// "Blue" | "Red", or None if they have no side on this server.
    pub side: Option<String>,
    pub score: f64,
    /// 1-8.
    pub tier: u8,
    /// The tier that unlocks command, and the score it starts at.
    pub commander_rank: u8,
    pub commander_score: f64,
    /// An admin's override, if any.
    pub grant: Option<CommanderGrant>,
    /// Who set the override, and when (RFC 3339).
    pub grant_by: Option<String>,
    pub grant_at: Option<String>,
    /// A dashboard admin: commands whatever their rank.
    #[serde(default)]
    pub admin: bool,
    /// A commander of `side` right now.
    pub commander: bool,
}

/// Latitude, longitude.
pub type LatLon = [f64; 2];

/// What kind of thing an asset is, which decides the orders it takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AssetKind {
    /// An AI flight: tanker, AWACS, drone, fighters, attackers, SEAD,
    /// bomber, recon, transport.
    Air,
    /// A supply convoy on the road.
    Convoy,
    /// Units a player deployed.
    Deployed,
    /// Troops a player dropped.
    Troops,
    /// A battery that can fire on a point: a garrison's artillery, or
    /// deployed artillery.
    Artillery,
    /// A carrier group.
    Naval,
    /// Any other AI ground group of ours out in the field (a reinforcement
    /// or search party, an ambush).
    Ground,
}

/// What a commander can tell an asset to do.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Verb {
    /// Drive to a point.
    Move,
    /// Fire on a point.
    Fire,
    /// Fly to a point and work there (CAP station, orbit, attack area).
    Station,
    /// Return to base.
    Rtb,
    /// Sail to a point.
    Sail,
}

/// One unit of an asset, where DCS has it now.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssetUnit {
    pub typ: String,
    pub pos: LatLon,
    /// Degrees true.
    pub heading: f64,
    /// Metres above sea level.
    pub alt_m: f64,
    pub speed_kts: f64,
}

/// One of our groups the command map shows and can order.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Asset {
    /// The group's id.
    pub id: i64,
    pub name: String,
    pub kind: AssetKind,
    /// What it is, for a label: "Tanker", "AWACS", "Supply convoy", "SA-11",
    /// "Artillery", "Carrier group", ...
    pub role: String,
    /// The lead unit's type.
    pub typ: String,
    pub pos: LatLon,
    pub heading: f64,
    pub alt_m: f64,
    pub speed_kts: f64,
    /// Units alive, of how many.
    pub alive: u32,
    pub total: u32,
    /// Its units are real in DCS right now (positions are live).
    pub live: bool,
    /// What it is doing, for the panel.
    pub task: Option<String>,
    /// Where it is headed, if anywhere.
    pub dest: Option<LatLon>,
    /// The base it belongs to or is going to, by name.
    pub base: Option<String>,
    /// For artillery: how far it reaches, metres.
    pub range_m: Option<f64>,
    pub orders: Vec<Verb>,
    pub units: Vec<AssetUnit>,
}

/// An operation the theatre HQ can run right now, for a commander to launch.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LaunchOption {
    pub kind: crate::hq::OpKind,
    /// The objective it is about (or nearest to).
    pub objective: i64,
    pub objective_name: String,
    pub pos: LatLon,
    /// Treasury points, escort and SEAD included.
    pub cost: i64,
    /// Why the HQ thinks it worth doing.
    pub why: String,
    /// The treasury can pay for it and the HQ has room for another.
    pub ready: bool,
}

/// One side's own assets, as its commanders see them.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommandPicture {
    pub side: String,
    /// Unix seconds.
    pub time: i64,
    pub treasury: i64,
    pub assets: Vec<Asset>,
    /// Empty when the server has no theatre HQ.
    pub launch: Vec<LaunchOption>,
    /// The server runs a theatre HQ, so `launch` means something.
    pub hq: bool,
    /// Everything this side can order at a place of the commander's
    /// choosing (`CommandOrder::Order`): its configured actions,
    /// deployments by road, and the operations that aren't actions.
    #[serde(default)]
    pub orders: Vec<OrderOption>,
    /// Our supply network: each base and the hub that feeds it.
    #[serde(default)]
    pub supply: Vec<SupplyLine>,
    /// Our air defences and how far they reach.
    #[serde(default)]
    pub defences: Vec<DefenceRing>,
}

/// One leg of our supply network: a hub feeding a base.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplyLine {
    pub from: LatLon,
    pub to: LatLon,
    pub from_name: String,
    pub to_name: String,
    /// The route is cut: an enemy base sits on it, or the hub is wrecked.
    pub cut: bool,
    /// Why it is cut, when it is.
    #[serde(default)]
    pub why: String,
}

/// One of our air-defence sites and its reach.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefenceRing {
    pub name: String,
    pub pos: LatLon,
    /// Engagement range, metres.
    pub range_m: f64,
    /// "sam" or "aaa".
    pub kind: String,
    /// In DCS right now.
    pub live: bool,
}

/// Rules of engagement for ground forces.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Roe {
    /// The doctrine: weapons free, return fire only on a road march.
    Auto,
    /// Engage anything in reach.
    Free,
    /// Shoot only when shot at.
    Return,
    /// Hold fire.
    Hold,
}

/// How hard a ground force drives.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Pace {
    /// Half speed: quieter, keeps together.
    Slow,
    #[default]
    Normal,
    /// Flat out.
    Fast,
}

impl Pace {
    pub fn factor(self) -> f64 {
        match self {
            Pace::Slow => 0.5,
            Pace::Normal => 1.,
            Pace::Fast => 1.35,
        }
    }
}

/// What an order is aimed at, which decides how the map asks for it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OrderTarget {
    /// A point on land.
    Land,
    /// A point anywhere (air operations).
    Point,
    /// A point at sea.
    Sea,
    /// One of our bases.
    OwnBase,
    /// An enemy base we know of.
    EnemyBase,
    /// From one of our bases to another.
    Transfer,
}

/// One entry of the order catalogue.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrderOption {
    /// What `CommandOrder::Order` names: "action:<name>", "deploy:<name>",
    /// "op:ambush", "op:missile", "op:hunt".
    pub key: String,
    pub label: String,
    /// "Air", "Fires", "Ground", "Naval", "Logistics", "Intel".
    pub category: String,
    pub target: OrderTarget,
    /// Treasury points.
    pub cost: i64,
    /// What it does, in a line.
    pub detail: String,
    /// Empty when it can be ordered now; otherwise why not.
    #[serde(default)]
    pub why_not: String,
}

/// An order from the command map. Externally tagged on purpose: an
/// internally tagged enum holding floats doesn't parse under
/// `arbitrary_precision` (see `hq::HqCommand`).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CommandOrder {
    /// A ground group (deployed, troops, other ground) drives to a point.
    Move { group: i64, to: LatLon },
    /// A ground formation drives to a point and holds there, by way of
    /// `via` in order when given.
    MoveFormation {
        formation: u32,
        to: LatLon,
        #[serde(default)]
        via: Vec<LatLon>,
    },
    /// Set how ground forces fight and drive: formations by id, deployed
    /// groups by id. A field left out is left as it is; `Roe::Auto` goes
    /// back to the doctrine.
    Posture {
        #[serde(default)]
        formations: Vec<u32>,
        #[serde(default)]
        groups: Vec<i64>,
        #[serde(default)]
        roe: Option<Roe>,
        #[serde(default)]
        pace: Option<Pace>,
    },
    /// A battery fires on a point.
    Fire { group: i64, at: LatLon },
    /// Every battery of ours in range fires on a point.
    Barrage { at: LatLon },
    /// An AI flight goes to a point and works there.
    Station { group: i64, at: LatLon },
    /// An AI flight goes home.
    Rtb { group: i64 },
    /// A carrier group sails to a point.
    Sail { group: i64, to: LatLon },
    /// A supply convoy to one of our bases.
    Convoy { to: i64 },
    /// A helicopter supply run to one of our bases.
    HeloSupply { to: i64 },
    /// A helicopter troop insertion at an objective.
    HeloTroops { to: i64 },
    /// One of the HQ's operations, paid from the treasury.
    Launch { kind: crate::hq::OpKind, objective: i64 },
    /// Anything from the order catalogue (`CommandPicture::orders`), at the
    /// commander's choice of place: `at` for a point, `objective` for a
    /// base, `objective` -> `to_objective` for a transfer.
    Order {
        key: String,
        #[serde(default)]
        at: Option<LatLon>,
        #[serde(default)]
        objective: Option<i64>,
        #[serde(default)]
        to_objective: Option<i64>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommandReply {
    pub ok: bool,
    pub message: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn orders_parse_with_floats() {
        // The workspace builds serde_json with arbitrary_precision.
        let o: CommandOrder = serde_json::from_str(r#"{"move":{"group":7,"to":[42.1,43.5]}}"#).unwrap();
        assert!(matches!(o, CommandOrder::Move { group: 7, .. }));
        let o: CommandOrder = serde_json::from_str(r#"{"launch":{"kind":"strike","objective":3}}"#).unwrap();
        assert!(matches!(o, CommandOrder::Launch { objective: 3, .. }));
        let o: CommandOrder = serde_json::from_str(r#"{"barrage":{"at":[42.0,44.25]}}"#).unwrap();
        assert!(matches!(o, CommandOrder::Barrage { .. }));
    }
}
