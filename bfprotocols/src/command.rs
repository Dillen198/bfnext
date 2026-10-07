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
}

/// An order from the command map. Externally tagged on purpose: an
/// internally tagged enum holding floats doesn't parse under
/// `arbitrary_precision` (see `hq::HqCommand`).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CommandOrder {
    /// A ground group (deployed, troops, other ground) drives to a point.
    Move { group: i64, to: LatLon },
    /// A ground formation drives to a point and holds there.
    MoveFormation { formation: u32, to: LatLon },
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
