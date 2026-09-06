use crate::admin::{AdminCommand, WarehouseKind};
use anyhow::Result;
use arcstr::ArcStr;
use bfprotocols::db::group::GroupId;
use chrono::prelude::*;
use crossbeam::queue::SegQueue;
use dcso3::{coalition::Side, net::{PlayerId, Ucid}, Vector2};
use futures::{channel::mpsc, stream::StreamExt};
use netidx::{
    chars::Chars,
    path::Path,
    publisher::{Publisher, Value},
};
use netidx_protocols::{
    define_rpc,
    rpc::server::{ArgSpec, Proc, RpcCall},
    rpc_err,
};
use regex::Regex;
use std::{str::FromStr, sync::Arc};
use tokio::{sync::oneshot, task};

pub struct Rpcs {
    _reduce_inventory: Proc,
    _transfer_supply: Proc,
    _logistics_tick_now: Proc,
    _logistics_deliver_now: Proc,
    _repair: Proc,
    _tim: Proc,
    _spawn: Proc,
    _side_switch: Proc,
    _ban: Proc,
    _unban: Proc,
    _kick: Proc,
    _connected: Proc,
    _banned: Proc,
    _search: Proc,
    _log_warehouse: Proc,
    _reset_lives: Proc,
    _add_admin: Proc,
    _remove_admin: Proc,
    _balance: Proc,
    _set_points: Proc,
    _delete: Proc,
    _deslot: Proc,
    _remark: Proc,
    _reset: Proc,
    _shutdown: Proc,
    // Query API
    _query_objectives: Proc,
    _query_objective: Proc,
    _query_players: Proc,
    _query_player: Proc,
    _query_groups: Proc,
    _query_group: Proc,
    _query_units: Proc,
    _query_warehouse: Proc,
    _query_logistics: Proc,
    _query_campaign_state: Proc,
    _query_perf: Proc,
    _query_briefing: Proc,
    // Action API
    _spawn_deployable: Proc,
    _spawn_troop: Proc,
    _move_group: Proc,
    _add_points: Proc,
    _set_objective_priority: Proc,
    // Cockpit UI API (player-scoped, not admin-wide)
    _resolve_player_id: Proc,
    _ewr_toggle: Proc,
    _ewr_report: Proc,
    _ewr_set_units: Proc,
    _ewr_ground_intel: Proc,
    _carp_solve: Proc,
    _carp_solve_latlon: Proc,
    _cargo_spawn_crate: Proc,
    _set_server_info: Proc,
    _intel_marks: Proc,
}

/// Parse the JSON bfdb pushes on `set-server-info` into an `AdminCommand`.
/// Shape: `{"restart_at": "<rfc3339>"|null, "weather": {"temp_c", "wind_speed_kts",
/// "wind_from_deg", "qnh_hpa", "cloud_base_m"}|null}`.
fn parse_server_info(s: &str) -> Result<AdminCommand> {
    let v: serde_json::Value = serde_json::from_str(s)?;
    let restart_at = v
        .get("restart_at")
        .and_then(|r| r.as_str())
        .and_then(|s| DateTime::parse_from_rfc3339(s).ok())
        .map(|d| d.with_timezone(&Utc));
    let weather = v.get("weather").filter(|w| !w.is_null()).map(|w| {
        let f = |k: &str| w.get(k).and_then(|x| x.as_f64()).unwrap_or(0.0);
        crate::BotWeather {
            temp_c: f("temp_c"),
            wind_speed_kts: f("wind_speed_kts"),
            wind_from_deg: f("wind_from_deg"),
            qnh_hpa: f("qnh_hpa"),
            cloud_base_m: f("cloud_base_m"),
            visibility_m: f("visibility_m"),
            cloud_density: f("cloud_density"),
        }
    });
    Ok(AdminCommand::SetServerInfo { restart_at, weather })
}

async fn wait_task(mut ch: mpsc::Receiver<(RpcCall, oneshot::Receiver<Value>)>) {
    while let Some((mut c, ch)) = ch.next().await {
        match ch.await {
            Err(_) => c.reply.send(Value::Error("call failed".into())),
            Ok(v) => c.reply.send(v),
        }
    }
}

impl Rpcs {
    pub async fn new(
        publisher: &Publisher,
        q: &Arc<SegQueue<(AdminCommand, oneshot::Sender<Value>)>>,
        base: &Path,
    ) -> Result<Self> {
        let base = base.append("api");
        let (wait, rx) = mpsc::channel(10);
        task::spawn(wait_task(rx));
        let _q = Arc::clone(&q);
        let reduce_inventory = define_rpc!(
            publisher,
            base.append("reduce-inventory"),
            "Reduce inventory at an airfield",
            |c: RpcCall, airbase: Chars, amount: u8| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::ReduceInventory { airbase: airbase.as_ref().into(), amount };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            airbase: Chars = Value::Null; "The airbase to reduce",
            amount: u8 = Value::Null; "The amount, as a whole number percentage, to reduce"
        )?;
        let _q = Arc::clone(&q);
        let transfer_supply = define_rpc!(
            publisher,
            base.append("transfer-supply"),
            "Transfer supply from one objective to another",
            |c: RpcCall, from: Chars, to: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::TransferSupply { from: from.as_ref().into(), to: to.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            from: Chars = Value::Null; "The airbase to transfer supply from",
            to: Chars = Value::Null; "The airbase to transfer supply to"
        )?;
        let _q = Arc::clone(&q);
        let logistics_tick_now = define_rpc!(
            publisher,
            base.append("logistics-tick-now"),
            "Force a logistics tick to happen on the next timed events",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::LogisticsTickNow;
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(&q);
        let logistics_deliver_now = define_rpc!(
            publisher,
            base.append("logistics-deliver-now"),
            "Force a logistics delivery to happen on the next timed events",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::LogisticsDeliverNow;
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(&q);
        let repair = define_rpc!(
            publisher,
            base.append("repair"),
            "Repair one logistics group",
            |c: RpcCall, airbase: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Repair { airbase: airbase.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            airbase: Chars = Value::Null; "The airbase to repair"
        )?;
        let _q = Arc::clone(&q);
        let tim = define_rpc!(
            publisher,
            base.append("tim"),
            "Cause an explosion on the specified mark",
            |c: RpcCall, key: Chars, size: usize, alt: Option<i64>| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Tim { key: key.as_ref().into(), size, alt: alt.map(|i| i as isize) };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            key: Chars = Value::Null; "The text in the mark you want to blow up",
            size: usize = 3000; "The size of the explosion in kg of TNT",
            alt: Option<i64> = Value::Null; "The altitude of the explosion"
        )?;
        let _q = Arc::clone(&q);
        let spawn = define_rpc!(
            publisher,
            base.append("spawn"),
            "Spawn a group at the specified mark",
            |c: RpcCall, key: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Spawn { key: key.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            key: Chars = Value::Null; "The key of the mark you want to spawn"
        )?;
        let _q = Arc::clone(&q);
        let side_switch = define_rpc!(
            publisher,
            base.append("side-switch"),
            "Side switch a player",
            |mut c: RpcCall, player: Chars, side: Chars| {
                let (tx, rx) = oneshot::channel();
                let side = match Side::from_str(&side) {
                    Ok(side) => side,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    },
                };
                let cmd = AdminCommand::SideSwitch { side, player: player.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The name of the player to switch",
            side: Chars = Value::Null; "The side to switch the player to"
        )?;
        let _q = Arc::clone(&q);
        let ban = define_rpc!(
            publisher,
            base.append("ban"),
            "Ban a player",
            |c: RpcCall, player: Chars, until: Option<DateTime<Utc>>| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Ban { player: player.as_ref().into(), until };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The name of the player to ban",
            until: Option<DateTime<Utc>> = Value::Null; "Optional end time of the ban"
        )?;
        let _q = Arc::clone(&q);
        let unban = define_rpc!(
            publisher,
            base.append("unban"),
            "Unban a player",
            |c: RpcCall, player: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Unban { player: player.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The name of the player to unban"
        )?;
        let _q = Arc::clone(&q);
        let kick = define_rpc!(
            publisher,
            base.append("kick"),
            "Kick a player",
            |c: RpcCall, player: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Kick { player: player.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The name of the player to kick"
        )?;
        let _q = Arc::clone(&q);
        let connected = define_rpc!(
            publisher,
            base.append("connected"),
            "List connected players",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((AdminCommand::Connected, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(&q);
        let banned = define_rpc!(
            publisher,
            base.append("banned"),
            "List banned players",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((AdminCommand::Banned, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(&q);
        let search = define_rpc!(
            publisher,
            base.append("search"),
            "Search players",
            |mut c: RpcCall, expr: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = match Regex::new(&expr) {
                    Ok(expr) => AdminCommand::Search { expr },
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            expr: Chars = Value::Null; "The regular expression to search for"
        )?;
        let _q = Arc::clone(&q);
        let log_warehouse = define_rpc!(
            publisher,
            base.append("log-warehouse"),
            "Log the contents of the specified warehouse",
            |mut c: RpcCall, airbase: Chars, kind: Chars| {
                let (tx, rx) = oneshot::channel();
                let kind = match kind.as_ref() {
                    "Objective" => WarehouseKind::Objective,
                    "DCS" => WarehouseKind::DCS,
                    s => {
                        c.reply.send(Value::Error(format!("invalid objective kind {s}").into()));
                        return None
                    }
                };
                let cmd = AdminCommand::LogWarehouse { kind, airbase: airbase.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            airbase: Chars = Value::Null; "The airbase to log",
            kind: Chars = Value::Null; "The kind of warehouse to log (Objective or DCS)"
        )?;
        let _q = Arc::clone(&q);
        let reset_lives = define_rpc!(
            publisher,
            base.append("reset-lives"),
            "Reset the specified player's lives",
            |c: RpcCall, player: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::ResetLives { player: player.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The player to reset"
        )?;
        let _q = Arc::clone(&q);
        let add_admin = define_rpc!(
            publisher,
            base.append("add-admin"),
            "Add player as an admin",
            |c: RpcCall, player: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::AddAdmin { player: player.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The player to add"
        )?;
        let _q = Arc::clone(&q);
        let remove_admin = define_rpc!(
            publisher,
            base.append("remove-admin"),
            "Remove player as an admin",
            |c: RpcCall, player: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::RemoveAdmin { player: player.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The player to remove"
        )?;
        let _q = Arc::clone(&q);
        let balance = define_rpc!(
            publisher,
            base.append("balance"),
            "Return a player's points balance",
            |c: RpcCall, player: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Balance { player: player.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The player"
        )?;
        let _q = Arc::clone(&q);
        let set_points = define_rpc!(
            publisher,
            base.append("set-points"),
            "Set a player's points balance",
            |c: RpcCall, player: Chars, amount: i32| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::SetPoints { player: player.as_ref().into(), amount };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The player",
            amount: i32 = Value::Null; "The balance"
        )?;
        let _q = Arc::clone(&q);
        let delete = define_rpc!(
            publisher,
            base.append("delete-group"),
            "Delete a group",
            |c: RpcCall, group: i64| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Delete { group: GroupId::from(group) };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            group: i64 = Value::Null; "The id of the group to delete"
        )?;
        let _q = Arc::clone(&q);
        let deslot = define_rpc!(
            publisher,
            base.append("deslot"),
            "Deslot a player",
            |c: RpcCall, player: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Deslot { player: player.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The player to deslot"
        )?;
        let _q = Arc::clone(&q);
        let remark = define_rpc!(
            publisher,
            base.append("remark"),
            "Remark an objective",
            |c: RpcCall, objective: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::Remark { objective: objective.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            objective: Chars = Value::Null; "The objective to remark"
        )?;
        let _q = Arc::clone(&q);
        let reset = define_rpc!(
            publisher,
            base.append("reset"),
            "Reset the campaign",
            |mut c: RpcCall, winner: Option<Chars>| {
                let (tx, rx) = oneshot::channel();
                let cmd = match winner.map(|s| Side::from_str(&s)).transpose() {
                    Ok(winner) => AdminCommand::Reset { winner },
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            winner: Option<Chars> = Value::Null; "The winner, if any"
        )?;
        let _q = Arc::clone(&q);
        let shutdown = define_rpc!(
            publisher,
            base.append("shutdown"),
            "Shutdown the server",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((AdminCommand::Shutdown, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        // ==================== Query API ====================
        let _q = Arc::clone(&q);
        let query_objectives = define_rpc!(
            publisher,
            base.append("query-objectives"),
            "Query all objectives (returns JSON)",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((AdminCommand::QueryObjectives, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(&q);
        let query_objective = define_rpc!(
            publisher,
            base.append("query-objective"),
            "Query a single objective by name (returns JSON)",
            |c: RpcCall, name: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::QueryObjective { name: name.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            name: Chars = Value::Null; "The objective name or partial match"
        )?;
        let _q = Arc::clone(&q);
        let query_players = define_rpc!(
            publisher,
            base.append("query-players"),
            "Query all players (returns JSON)",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((AdminCommand::QueryPlayers, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(&q);
        let query_player = define_rpc!(
            publisher,
            base.append("query-player"),
            "Query a single player by name/ucid (returns JSON)",
            |c: RpcCall, player: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::QueryPlayer { player: player.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The player name, UCID, or player ID"
        )?;
        let _q = Arc::clone(&q);
        let query_groups = define_rpc!(
            publisher,
            base.append("query-groups"),
            "Query all spawned groups, optionally filtered by side (returns JSON)",
            |mut c: RpcCall, side: Option<Chars>| {
                let (tx, rx) = oneshot::channel();
                let side = match side.map(|s| Side::from_str(&s)).transpose() {
                    Ok(side) => side,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                let cmd = AdminCommand::QueryGroups { side };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            side: Option<Chars> = Value::Null; "Optional side filter (Blue, Red, Neutral)"
        )?;
        let _q = Arc::clone(&q);
        let query_group = define_rpc!(
            publisher,
            base.append("query-group"),
            "Query a single group by ID (returns JSON)",
            |c: RpcCall, id: i64| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::QueryGroup { id: GroupId::from(id) };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            id: i64 = Value::Null; "The group ID"
        )?;
        let _q = Arc::clone(&q);
        let query_units = define_rpc!(
            publisher,
            base.append("query-units"),
            "Query all units in a group (returns JSON)",
            |c: RpcCall, group: i64| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::QueryUnits { group: GroupId::from(group) };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            group: i64 = Value::Null; "The group ID"
        )?;
        let _q = Arc::clone(&q);
        let query_warehouse = define_rpc!(
            publisher,
            base.append("query-warehouse"),
            "Query warehouse inventory for an objective (returns JSON)",
            |c: RpcCall, objective: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::QueryWarehouse { objective: objective.as_ref().into() };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            objective: Chars = Value::Null; "The objective name"
        )?;
        let _q = Arc::clone(&q);
        let query_logistics = define_rpc!(
            publisher,
            base.append("query-logistics"),
            "Query logistics state (returns JSON)",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((AdminCommand::QueryLogistics, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(&q);
        let query_campaign_state = define_rpc!(
            publisher,
            base.append("query-campaign-state"),
            "Query overall campaign state summary (returns JSON)",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((AdminCommand::QueryCampaignState, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(&q);
        let query_perf = define_rpc!(
            publisher,
            base.append("query-perf"),
            "Query live engine/API performance stats for the current session (returns JSON)",
            |c: RpcCall, _: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((AdminCommand::QueryPerf, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(&q);
        let query_briefing = define_rpc!(
            publisher,
            base.append("query-briefing"),
            "Query the per-side kneeboard briefing: navaids, radios, artillery, deployables, threats (returns JSON)",
            |mut c: RpcCall, side: Chars| {
                let (tx, rx) = oneshot::channel();
                let side = match Side::from_str(&side.trim().to_lowercase()) {
                    Ok(side) => side,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                _q.push((AdminCommand::QueryBriefing { side }, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            side: Chars = Value::Null; "The side (blue, red, neutral)"
        )?;
        // ==================== Action API ====================
        let _q = Arc::clone(&q);
        let spawn_deployable = define_rpc!(
            publisher,
            base.append("spawn-deployable"),
            "Spawn a deployable unit at a position",
            |mut c: RpcCall, side: Chars, name: Chars, x: f64, z: f64, heading: f64| {
                let (tx, rx) = oneshot::channel();
                let side = match Side::from_str(&side) {
                    Ok(side) => side,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                let cmd = AdminCommand::SpawnDeployable {
                    side,
                    name: name.as_ref().into(),
                    pos: Vector2::new(x, z),
                    heading,
                };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            side: Chars = Value::Null; "The side (Blue or Red)",
            name: Chars = Value::Null; "The deployable name",
            x: f64 = Value::Null; "X position (DCS coordinates)",
            z: f64 = Value::Null; "Z position (DCS coordinates)",
            heading: f64 = 0.0; "Heading in radians"
        )?;
        let _q = Arc::clone(&q);
        let spawn_troop = define_rpc!(
            publisher,
            base.append("spawn-troop"),
            "Spawn a troop unit at a position",
            |mut c: RpcCall, side: Chars, name: Chars, x: f64, z: f64, heading: f64| {
                let (tx, rx) = oneshot::channel();
                let side = match Side::from_str(&side) {
                    Ok(side) => side,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                let cmd = AdminCommand::SpawnTroop {
                    side,
                    name: name.as_ref().into(),
                    pos: Vector2::new(x, z),
                    heading,
                };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            side: Chars = Value::Null; "The side (Blue or Red)",
            name: Chars = Value::Null; "The troop name",
            x: f64 = Value::Null; "X position (DCS coordinates)",
            z: f64 = Value::Null; "Z position (DCS coordinates)",
            heading: f64 = 0.0; "Heading in radians"
        )?;
        let _q = Arc::clone(&q);
        let move_group = define_rpc!(
            publisher,
            base.append("move-group"),
            "Move a group to a new position",
            |c: RpcCall, id: i64, x: f64, z: f64| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::MoveGroup {
                    id: GroupId::from(id),
                    pos: Vector2::new(x, z),
                };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            id: i64 = Value::Null; "The group ID",
            x: f64 = Value::Null; "X position (DCS coordinates)",
            z: f64 = Value::Null; "Z position (DCS coordinates)"
        )?;
        let _q = Arc::clone(&q);
        let add_points = define_rpc!(
            publisher,
            base.append("add-points"),
            "Add or subtract points from a player",
            |c: RpcCall, player: Chars, amount: i32, reason: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::AddPoints {
                    player: player.as_ref().into(),
                    amount,
                    reason: reason.as_ref().into(),
                };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            player: Chars = Value::Null; "The player name, UCID, or player ID",
            amount: i32 = Value::Null; "The points to add (negative to subtract)",
            reason: Chars = Value::Null; "The reason for the points change"
        )?;
        let _q = Arc::clone(&q);
        let set_objective_priority = define_rpc!(
            publisher,
            base.append("set-objective-priority"),
            "Set or clear the commander's-intent priority marker on an objective (display/coordination only)",
            |c: RpcCall, objective: Chars, priority: bool| {
                let (tx, rx) = oneshot::channel();
                let cmd = AdminCommand::SetObjectivePriority {
                    objective: objective.as_ref().into(),
                    priority,
                };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            objective: Chars = Value::Null; "The objective name or partial match",
            priority: bool = false; "Whether the objective should be marked high priority"
        )?;
        // ==================== Cockpit UI API ====================
        let _q = Arc::clone(&q);
        let resolve_player_id = define_rpc!(
            publisher,
            base.append("resolve-player-id"),
            "Resolve a connected player's local DCS player id (net.get_my_player_id()) to their ucid",
            |c: RpcCall, id: i64| {
                let (tx, rx) = oneshot::channel();
                _q.push((AdminCommand::ResolvePlayerId { id: PlayerId::from(id) }, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            id: i64 = Value::Null; "The player's local DCS player id"
        )?;
        let _q = Arc::clone(&q);
        let ewr_toggle = define_rpc!(
            publisher,
            base.append("ewr-toggle"),
            "Toggle EWR reports on/off for the calling player",
            |mut c: RpcCall, ucid: Chars| {
                let (tx, rx) = oneshot::channel();
                let ucid = match Ucid::from_str(&ucid) {
                    Ok(ucid) => ucid,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                _q.push((AdminCommand::EwrToggle { ucid }, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            ucid: Chars = Value::Null; "The calling player's ucid"
        )?;
        let _q = Arc::clone(&q);
        let ewr_report = define_rpc!(
            publisher,
            base.append("ewr-report"),
            "Get a BRAA contact report (bandits or friendlies) for the calling player",
            |mut c: RpcCall, ucid: Chars, friendly: bool| {
                let (tx, rx) = oneshot::channel();
                let ucid = match Ucid::from_str(&ucid) {
                    Ok(ucid) => ucid,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                _q.push((AdminCommand::EwrReport { ucid, friendly }, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            ucid: Chars = Value::Null; "The calling player's ucid",
            friendly: bool = false; "Report friendlies instead of bandits"
        )?;
        let _q = Arc::clone(&q);
        let ewr_set_units = define_rpc!(
            publisher,
            base.append("ewr-set-units"),
            "Set the calling player's EWR report units",
            |mut c: RpcCall, ucid: Chars, imperial: bool| {
                let (tx, rx) = oneshot::channel();
                let ucid = match Ucid::from_str(&ucid) {
                    Ok(ucid) => ucid,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                _q.push((AdminCommand::EwrSetUnits { ucid, imperial }, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            ucid: Chars = Value::Null; "The calling player's ucid",
            imperial: bool = false; "Imperial units if true, otherwise metric"
        )?;
        let _q = Arc::clone(&q);
        let ewr_ground_intel = define_rpc!(
            publisher,
            base.append("ewr-ground-intel"),
            "Get the ground intel picture for the calling player",
            |mut c: RpcCall, ucid: Chars| {
                let (tx, rx) = oneshot::channel();
                let ucid = match Ucid::from_str(&ucid) {
                    Ok(ucid) => ucid,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                _q.push((AdminCommand::EwrGroundIntel { ucid }, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            ucid: Chars = Value::Null; "The calling player's ucid"
        )?;
        let _q = Arc::clone(&q);
        let carp_solve = define_rpc!(
            publisher,
            base.append("carp-solve"),
            "Solve CARP INIT 1/5, 3/5 and 4/5 auto-fillable fields (PI position, elevations, wind, temp) for the PI marked with the given F10 map mark text",
            |mut c: RpcCall, ucid: Chars, mark_key: Chars, drop_altitude_agl_ft: f64| {
                let (tx, rx) = oneshot::channel();
                // ucid is validated (bfdb only calls this for a resolved, trusted
                // player) but the solve itself doesn't need per-player state.
                if let Err(e) = Ucid::from_str(&ucid) {
                    c.reply.send(Value::Error(format!("{e:?}").into()));
                    return None
                }
                _q.push((AdminCommand::CarpSolve {
                    mark_key: mark_key.as_ref().into(),
                    drop_altitude_agl_ft,
                }, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            ucid: Chars = Value::Null; "The calling player's ucid",
            mark_key: Chars = Value::Null; "The text of the F10 map mark placed on the PI",
            drop_altitude_agl_ft: f64 = Value::Null; "Planned drop altitude, feet AGL"
        )?;
        let _q = Arc::clone(&q);
        let carp_solve_latlon = define_rpc!(
            publisher,
            base.append("carp-solve-latlon"),
            "Solve CARP INIT 1/5, 3/5 and 4/5 auto-fillable fields for a PI given directly as lat/long (e.g. a click on the dashboard's map), no F10 mark required",
            |mut c: RpcCall, ucid: Chars, lat: f64, lon: f64, drop_altitude_agl_ft: f64| {
                let (tx, rx) = oneshot::channel();
                if let Err(e) = Ucid::from_str(&ucid) {
                    c.reply.send(Value::Error(format!("{e:?}").into()));
                    return None
                }
                _q.push((AdminCommand::CarpSolveLatLon { lat, lon, drop_altitude_agl_ft }, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            ucid: Chars = Value::Null; "The calling player's ucid",
            lat: f64 = Value::Null; "PI latitude",
            lon: f64 = Value::Null; "PI longitude",
            drop_altitude_agl_ft: f64 = Value::Null; "Planned drop altitude, feet AGL"
        )?;
        let _q = Arc::clone(&q);
        let cargo_spawn_crate = define_rpc!(
            publisher,
            base.append("cargo-spawn-crate"),
            "Queue qty copies of a named crate for the calling player's current slot",
            |mut c: RpcCall, ucid: Chars, crate_name: Chars, qty: i64, c130: bool| {
                let (tx, rx) = oneshot::channel();
                let ucid = match Ucid::from_str(&ucid) {
                    Ok(ucid) => ucid,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("{e:?}").into()));
                        return None
                    }
                };
                if qty < 1 {
                    c.reply.send(Value::Error("qty must be at least 1".into()));
                    return None
                }
                let cmd = AdminCommand::CockpitSpawnCrate {
                    ucid,
                    crate_name: crate_name.as_ref().into(),
                    qty: qty as u32,
                    c130,
                };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            ucid: Chars = Value::Null; "The calling player's ucid",
            crate_name: Chars = Value::Null; "The crate's name, as configured in cfg.deployables",
            qty: i64 = 1; "How many copies to queue",
            c130: bool = true; "Use C-130 cargo rules (true) or helo cargo rules (false)"
        )?;
        let _q = Arc::clone(&q);
        let set_server_info = define_rpc!(
            publisher,
            base.append("set-server-info"),
            "Push DCSServerBot-derived restart time + surface weather into the F10 Info menu",
            |mut c: RpcCall, info: Chars| {
                let (tx, rx) = oneshot::channel();
                let cmd = match parse_server_info(info.as_ref()) {
                    Ok(cmd) => cmd,
                    Err(e) => {
                        c.reply.send(Value::Error(format!("bad server-info json: {e:?}").into()));
                        return None;
                    }
                };
                _q.push((cmd, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            info: Chars = Value::Null; "JSON {restart_at, weather:{temp_c,wind_speed_kts,wind_from_deg,qnh_hpa,cloud_base_m}}"
        )?;
        let _q = Arc::clone(&q);
        let intel_marks = define_rpc!(
            publisher,
            base.append("intel-marks"),
            "Push the dashboard's coalition recon markup onto the F10 map",
            |mut c: RpcCall, data: Chars| {
                let (tx, rx) = oneshot::channel();
                if data.is_empty() {
                    c.reply.send(Value::Error("empty intel-marks payload".into()));
                    return None;
                }
                _q.push((AdminCommand::SetIntelMarks(data.as_ref().into()), tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            data: Chars = Value::Null; "JSON {marks:[{id,side,kind,points,color,by_name}]}"
        )?;
        Ok(Self {
            _reduce_inventory: reduce_inventory,
            _transfer_supply: transfer_supply,
            _logistics_tick_now: logistics_tick_now,
            _logistics_deliver_now: logistics_deliver_now,
            _repair: repair,
            _tim: tim,
            _spawn: spawn,
            _side_switch: side_switch,
            _ban: ban,
            _unban: unban,
            _kick: kick,
            _connected: connected,
            _banned: banned,
            _search: search,
            _log_warehouse: log_warehouse,
            _reset_lives: reset_lives,
            _add_admin: add_admin,
            _remove_admin: remove_admin,
            _balance: balance,
            _set_points: set_points,
            _delete: delete,
            _deslot: deslot,
            _remark: remark,
            _reset: reset,
            _shutdown: shutdown,
            // Query API
            _query_objectives: query_objectives,
            _query_objective: query_objective,
            _query_players: query_players,
            _query_player: query_player,
            _query_groups: query_groups,
            _query_group: query_group,
            _query_units: query_units,
            _query_warehouse: query_warehouse,
            _query_logistics: query_logistics,
            _query_campaign_state: query_campaign_state,
            _query_perf: query_perf,
            _query_briefing: query_briefing,
            // Action API
            _spawn_deployable: spawn_deployable,
            _spawn_troop: spawn_troop,
            _move_group: move_group,
            _add_points: add_points,
            _set_objective_priority: set_objective_priority,
            _resolve_player_id: resolve_player_id,
            _ewr_toggle: ewr_toggle,
            _ewr_report: ewr_report,
            _ewr_set_units: ewr_set_units,
            _ewr_ground_intel: ewr_ground_intel,
            _carp_solve: carp_solve,
            _carp_solve_latlon: carp_solve_latlon,
            _cargo_spawn_crate: cargo_spawn_crate,
            _set_server_info: set_server_info,
            _intel_marks: intel_marks,
        })
    }
}
