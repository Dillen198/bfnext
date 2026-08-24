use anyhow::Result;
use bfprotocols::cfg::UnitTag;
use clap::Parser;
use db::{SessionData, StatsDb, WikiImage, WikiPage};
use futures::{SinkExt, StreamExt};
use netidx::{config::Config, path::Path as NetidxPath, subscriber::SubscriberBuilder};
use regex::Regex;
use rust_embed::RustEmbed;
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::VecDeque,
    net::SocketAddr,
    path::PathBuf,
    sync::{Arc, Mutex},
};
use tokio::{sync::broadcast, task};
use uuid::Uuid;
use warp::{
    http::Method,
    reply::{self, Reply, Response},
    ws::{Message, WebSocket},
    Filter,
};

#[derive(RustEmbed)]
#[folder = "../bfweb/dist/"]
struct Assets;

#[derive(RustEmbed)]
#[folder = "../bfsite/dist/"]
struct SiteAssets;

mod db;
mod db_id;

/// Load stats and serve the Fowl Engine API
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The base path to find and subscribe to the stats (omit for offline mode)
    #[arg(short, long)]
    base: Option<NetidxPath>,
    /// The path to the database
    #[arg(short, long)]
    db: PathBuf,
    /// The certificate to use for TLS
    #[arg(short, long)]
    cert: Option<PathBuf>,
    /// The private key to use for TLS
    #[arg(short, long)]
    key: Option<PathBuf>,
    /// Path to the netidx-archive stats directory (e.g. "E:/Saved Games/DCS/Logs/stats")
    #[arg(long)]
    stats_dir: Option<PathBuf>,
    /// Path to stats JSONL file (e.g. "E:/Saved Games/DCS/Logs/stats.jsonl")
    #[arg(long)]
    stats_jsonl: Option<PathBuf>,
    /// Include only scenarios that match the given regex
    #[arg(long)]
    include: Option<Regex>,
    /// Exclude scenarios that match the given regex
    #[arg(long)]
    exclude: Option<Regex>,
    /// The web address to listen on
    #[arg(long)]
    listen_address: SocketAddr,
    /// Discord OAuth2 client ID
    #[arg(long)]
    discord_client_id: Option<String>,
    /// Discord OAuth2 client secret
    #[arg(long)]
    discord_client_secret: Option<String>,
    /// Discord OAuth2 redirect URI (e.g. http://localhost:8080/api/auth/callback)
    #[arg(long)]
    discord_redirect_uri: Option<String>,
    /// Discord guild ID (for role-based admin check)
    #[arg(long)]
    discord_guild_id: Option<String>,
    /// Discord role ID that grants admin access
    #[arg(long)]
    discord_admin_role_id: Option<String>,
    /// Path to campaign config JSON (e.g. campaign.json).
    /// Served at GET /api/config so the web UI can brand itself.
    #[arg(long)]
    config: Option<PathBuf>,
    /// Optional separate address to serve the public website (bfsite) on.
    /// e.g. 0.0.0.0:8081 — if omitted, the site is still accessible at /site/ on the main port.
    #[arg(long)]
    site_address: Option<SocketAddr>,
    /// Local admin username for password-based login (alternative to Discord OAuth)
    #[arg(long)]
    admin_username: Option<String>,
    /// Local admin password for password-based login
    #[arg(long)]
    admin_password: Option<String>,
    /// SRS server URL to proxy for the dashboard radio panel (e.g. http://localhost:5002)
    #[arg(long)]
    srs_url: Option<String>,
    /// Path to the campaign engine config JSON that bflib loads (e.g. ODFv2_CFG).
    /// Enables the admin config editor at GET/POST /api/admin/cfg. Distinct from
    /// --config, which is just dashboard branding.
    #[arg(long)]
    engine_config: Option<PathBuf>,
    /// Origin(s) allowed to make cross-origin, credentialed API requests
    /// (e.g. https://dashboard.example.com). Repeat for multiple origins.
    /// Pass this when bfweb/bfsite are hosted separately from bfdb instead of
    /// embedded — without it, CORS defaults to same-origin-only behavior and
    /// session cookies use SameSite=Lax. Setting this switches cookies to
    /// SameSite=None; Secure, which requires bfdb to be served over TLS
    /// (--cert/--key), since browsers refuse SameSite=None without Secure.
    #[arg(long = "cors-origin")]
    cors_origins: Vec<String>,
    /// Write logs to this file instead of the console (in addition to the
    /// in-process log history/WebSocket stream used by the dashboard's Engine
    /// Log viewer). Lets the launcher's status console stay quiet and just
    /// show whether bfdb is running.
    #[arg(long = "log-file")]
    log_file: Option<PathBuf>,
    /// Base URL of DCSServerBot's RestAPI plugin, including its configured
    /// `prefix` (e.g. http://127.0.0.1:9876/stats). Discord account linking
    /// (dashboard "My Profile", the in-DCS cockpit UI, etc.) resolves a
    /// player's ucid by querying this bot endpoint's /getuser -- bfdb keeps
    /// no Discord-link database of its own; DCSServerBot's own /linkme +
    /// -linkme <token> flow is the only way to link. Leave unset to disable
    /// linking entirely (those features return "account not linked").
    #[arg(long = "dcsserverbot-url")]
    dcsserverbot_url: Option<String>,
    /// X-API-Key for DCSServerBot's RestAPI plugin (the `api_key` in its
    /// restapi.yaml). Required if --dcsserverbot-url is set.
    #[arg(long = "dcsserverbot-api-key")]
    dcsserverbot_api_key: Option<String>,
}

#[derive(Debug, Clone)]
struct AuthConfig {
    client_id:     String,
    client_secret: String,
    redirect_uri:  String,
    guild_id:      String,
    admin_role_id: String,
}

#[derive(Debug, Clone)]
struct LocalAdminConfig {
    username: String,
    password: String,
}

#[derive(Debug, Clone)]
struct BotLinkConfig {
    base_url: String,
    api_key:  String,
}

#[derive(Deserialize)]
struct BotUserEntry {
    ucid: std::string::String,
}

/// Resolves a Discord user's ucid via DCSServerBot's own player-linking
/// database (RestAPI plugin's POST {prefix}/getuser) -- bfdb has no linking
/// store of its own, so this is the only way a Discord session ever becomes
/// a "linked player". Returns `Ok(None)` both when the bot isn't configured
/// and when the bot has no link for this user (both mean "not linked").
/// Never fails outright -- Discord linking is supplementary information for
/// an already-established session, not something that should be able to
/// break login/session-check itself. Any problem talking to the bot (down,
/// misconfigured, bad response) just logs a warning and resolves to "not
/// linked", the same as if the bot genuinely has no link for this user.
async fn resolve_ucid_via_bot(
    bot_cfg: &Option<BotLinkConfig>,
    discord_id: &str,
) -> Option<dcso3::net::Ucid> {
    let cfg = bot_cfg.as_ref()?;
    let result: anyhow::Result<Option<dcso3::net::Ucid>> = async {
        let http = reqwest::Client::new();
        let users: Vec<BotUserEntry> = http
            .post(format!("{}/getuser", cfg.base_url))
            .header("X-API-Key", &cfg.api_key)
            .form(&[("discord_id", discord_id)])
            .send()
            .await
            .map_err(|e| anyhow::anyhow!("DCSServerBot getuser request failed: {e}"))?
            .json()
            .await
            .map_err(|e| anyhow::anyhow!("DCSServerBot getuser response parse failed: {e}"))?;
        Ok(match users.into_iter().next() {
            Some(u) => Some(u.ucid.parse().map_err(|e| anyhow::anyhow!("bad ucid from bot: {e:?}"))?),
            None => None,
        })
    }.await;
    match result {
        Ok(ucid) => ucid,
        Err(e) => {
            log::warn!("DCSServerBot link lookup failed for discord_id={discord_id}: {e:?}");
            None
        }
    }
}

// Units per DCSServerBot's own RestAPI plugin WeatherInfo model:
// temperature=Celsius, wind_speed=knots, wind_direction=degrees,
// pressure=mmHg, clouds_base=feet. Only clouds_base/pressure need
// converting to match bfdb's existing meters/hPa convention.
#[derive(Deserialize, Clone)]
struct BotWeather {
    temperature:    Option<f64>,
    wind_speed:     Option<f64>,
    wind_direction: Option<f64>,
    pressure:       Option<f64>,
    clouds_base:    Option<f64>,
    clouds_density: Option<u8>,
    visibility:     Option<f64>,
}

#[derive(Deserialize, Clone)]
struct BotServerInfo {
    restart_time: Option<std::string::String>,
    weather:      Option<BotWeather>,
}

/// DCSServerBot's RestAPI plugin serializes datetimes inconsistently across
/// versions -- sometimes RFC3339 with an offset, sometimes a naive
/// "YYYY-MM-DDTHH:MM:SS" with no timezone. Try both; treat a naive one as
/// already UTC (matches how the bot's own scheduler stores it).
fn parse_bot_datetime(s: &str) -> Option<chrono::DateTime<chrono::Utc>> {
    if let Ok(dt) = chrono::DateTime::parse_from_rfc3339(s) {
        return Some(dt.with_timezone(&chrono::Utc));
    }
    chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S")
        .ok()
        .map(|ndt| chrono::DateTime::from_naive_utc_and_offset(ndt, chrono::Utc))
}

/// Fetches DCSServerBot's GET {prefix}/servers -- never fails outright,
/// same never-fails contract as resolve_ucid_via_bot (a dashboard falling
/// back to bflib-derived data, or nothing, beats /api/stats breaking
/// because the bot is down). Backs both the restart countdown and live
/// weather, which live in the same response.
async fn fetch_bot_server_info(bot_cfg: &Option<BotLinkConfig>) -> Option<BotServerInfo> {
    let cfg = bot_cfg.as_ref()?;
    let result: anyhow::Result<Option<BotServerInfo>> = async {
        let http = reqwest::Client::new();
        let servers: Vec<BotServerInfo> = http
            .get(format!("{}/servers", cfg.base_url))
            .header("X-API-Key", &cfg.api_key)
            .send()
            .await
            .map_err(|e| anyhow::anyhow!("DCSServerBot servers request failed: {e}"))?
            .json()
            .await
            .map_err(|e| anyhow::anyhow!("DCSServerBot servers response parse failed: {e}"))?;
        Ok(servers.into_iter().next())
    }.await;
    match result {
        Ok(v) => v,
        Err(e) => {
            log::warn!("DCSServerBot /servers lookup failed: {e:?}");
            None
        }
    }
}

#[derive(Debug)]
struct Error(anyhow::Error);

impl Reply for Error {
    fn into_response(self) -> Response {
        let body = serde_json::json!({ "error": format!("{:?}", self.0) });
        warp::reply::with_status(
            warp::reply::json(&body),
            warp::http::StatusCode::INTERNAL_SERVER_ERROR,
        )
        .into_response()
    }
}

impl From<anyhow::Error> for Error {
    fn from(value: anyhow::Error) -> Self {
        Self(value)
    }
}

// ── Real-time log broadcaster ─────────────────────────────────────────────────

const LOG_HISTORY_CAP: usize = 500;

type LogHistory = Arc<Mutex<VecDeque<String>>>;

#[derive(Debug, Clone, Serialize)]
struct LogLine {
    ts:     String,
    level:  String,
    target: String,
    msg:    String,
}

struct BroadcastLogger {
    inner:   env_logger::Logger,
    tx:      broadcast::Sender<String>,
    history: LogHistory,
}

impl log::Log for BroadcastLogger {
    fn enabled(&self, meta: &log::Metadata) -> bool {
        self.inner.enabled(meta)
    }

    fn log(&self, record: &log::Record) {
        if !self.enabled(record.metadata()) {
            return;
        }
        self.inner.log(record);
        let line = LogLine {
            ts:     chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
            level:  record.level().to_string(),
            target: record.target().to_string(),
            msg:    record.args().to_string(),
        };
        if let Ok(json) = serde_json::to_string(&line) {
            let mut hist = self.history.lock().unwrap();
            if hist.len() >= LOG_HISTORY_CAP {
                hist.pop_front();
            }
            hist.push_back(json.clone());
            let _ = self.tx.send(json);
        }
    }

    fn flush(&self) {
        self.inner.flush();
    }
}

fn json_response(data: String) -> impl warp::Reply {
    reply::with_header(
        reply::with_header(data, "content-type", "application/json"),
        "cache-control",
        "no-store",
    )
}

// ── Campaign config handler ──────────────────────────────────────────

async fn api_config(cfg_json: Arc<String>) -> impl warp::Reply {
    reply::with_header(
        reply::with_header((*cfg_json).clone(), "content-type", "application/json"),
        "cache-control",
        "no-store",
    )
}

// ── API handlers ────────────────────────────────────────────────────

async fn api_rounds(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.all_rounds()?;
        let entries: Vec<_> = rounds
            .iter()
            .map(|(scenario, rid, round)| {
                serde_json::json!({
                    "id": rid.0,
                    "scenario": scenario.to_string(),
                    "start": round.start.to_rfc3339(),
                    "end": round.end.map(|d| d.to_rfc3339()),
                    "active": round.end.is_none(),
                    "winner": round.winner.map(|s| format!("{s:?}")),
                })
            })
            .collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

async fn api_leaderboard(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        // Use all-time totals so pilot stats are never empty
        let pilots = db.pilot_leaderboard(None)?;
        let entries: Vec<_> = pilots
            .iter()
            .map(|(ucid, name, agg)| {
                serde_json::json!({
                    "ucid": ucid.to_string(),
                    "name": name.to_string(),
                    "air_kills": agg.air_kills,
                    "ground_kills": agg.ground_kills,
                    "captures": agg.captures,
                    "repairs": agg.repairs,
                    "supply_transfers": agg.supply_transfers,
                    "troops": agg.troops,
                    "farps": agg.farps,
                    "deploys": agg.deploys,
                    "actions": agg.actions,
                    "deaths": agg.deaths,
                    "hours": agg.hours,
                    "donated_points": agg.donated_points,
                })
            })
            .collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

/// GET /api/pilots — all pilot UCIDs + names (all-time, for name resolution)
async fn api_all_pilots(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let pilots = db.all_pilot_names()?;
        let entries: Vec<_> = pilots.iter().map(|(ucid, name)| {
            serde_json::json!({ "ucid": ucid.to_string(), "name": name.to_string() })
        }).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

/// Call one of bflib's netidx RPC procs and return its reply as an owned
/// String (bail on Value::Error or an unexpected reply shape).
async fn call_engine_rpc_str(
    db: &StatsDb,
    proc_name: &str,
    args: Vec<(&str, netidx::publisher::Value)>,
) -> std::result::Result<std::string::String, Error> {
    use netidx::publisher::Value;
    match db.call_engine_rpc(proc_name, args).await? {
        Value::Error(e) => Err(Error(anyhow::anyhow!("{e}"))),
        Value::String(s) => Ok(s.to_string()),
        other => Err(Error(anyhow::anyhow!("unexpected RPC reply: {other:?}"))),
    }
}

async fn api_objectives(
    db: StatsDb,
    round_id: Option<u64>,
) -> std::result::Result<impl warp::Reply, Error> {
    let (mut entries, is_active) = task::block_in_place(|| -> Result<(Vec<serde_json::Value>, bool)> {
        let rounds = db.latest_rounds()?;
        let active_rid = rounds.iter().find(|(_, _, r)| r.end.is_none()).map(|(_, rid, _)| *rid);
        let rid = match round_id {
            Some(id) => db::RoundId(id),
            None => match active_rid {
                Some(rid) => rid,
                None => match rounds.first() {
                    Some((_, rid, _)) => *rid,
                    None => return Ok((vec![], false)),
                },
            },
        };
        let is_active = active_rid == Some(rid);
        let objs = db.objectives_for_round(rid)?;
        let entries: Vec<_> = objs
            .iter()
            .filter_map(|(oid, obj)| {
                // Anti-cheat: hide carrier group and special SAM site exact positions
                if obj.kind.is_carrier_group() || obj.kind.is_special_sam_site() {
                    return None;
                }
                Some(serde_json::json!({
                    "id": format!("{:?}", oid),
                    "name": obj.name.to_string(),
                    "kind": obj.kind.name(),
                    "owner": format!("{:?}", obj.owner),
                    "lat": obj.pos.latitude,
                    "lon": obj.pos.longitude,
                    "health": obj.health,
                    "logi": obj.logi,
                    "supply": obj.supply,
                    "fuel": obj.fuel,
                    "last_change": obj.last_change.to_rfc3339(),
                    // Overwritten below with a live value for the active round,
                    // when bflib is reachable. Historical rounds keep this default.
                    "priority": false,
                }))
            })
            .collect();
        Ok((entries, is_active))
    })?;

    // The priority flag is live engine state, not something bfdb persists on
    // its own -- refresh it from bflib for the currently active round only.
    // bflib's netidx RPC has no timeout of its own, so if the mission is
    // restarting/unreachable this call would otherwise hang the whole
    // request indefinitely -- bound it so /api/objectives always answers
    // within a few seconds, falling back to the persisted (possibly stale)
    // priority flags on timeout rather than blocking every caller (including
    // the Discord bot's poller, which has its own 10s client timeout).
    if is_active {
        match tokio::time::timeout(
            std::time::Duration::from_secs(3),
            call_engine_rpc_str(&db, "query-objectives", vec![]),
        ).await {
            Ok(Ok(json)) => {
                if let Ok(live) = serde_json::from_str::<Vec<bfprotocols::api::ObjectiveInfo>>(&json) {
                    let priorities: std::collections::HashMap<&str, bool> =
                        live.iter().map(|o| (o.name.as_str(), o.priority)).collect();
                    for entry in entries.iter_mut() {
                        if let Some(name) = entry.get("name").and_then(|n| n.as_str()) {
                            if let Some(&p) = priorities.get(name) {
                                entry["priority"] = serde_json::Value::Bool(p);
                            }
                        }
                    }
                }
            }
            Ok(Err(e)) => log::warn!("api_objectives: query-objectives RPC failed: {}", e.0),
            Err(_) => log::warn!("api_objectives: query-objectives RPC timed out after 3s, engine may be unreachable"),
        }
    }

    let data = serde_json::to_string(&entries).map_err(|e| Error(e.into()))?;
    Ok(json_response(data))
}

async fn api_kills(
    db: StatsDb,
    round_id: Option<u64>,
    limit: Option<usize>,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
        let rid = match round_id {
            Some(id) => db::RoundId(id),
            None => match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
                Some((_, rid, _)) => *rid,
                None => match rounds.first() {
                    Some((_, rid, _)) => *rid,
                    None => return Ok("[]".to_string()),
                },
            },
        };
        let kills = db.recent_kills(rid, limit.unwrap_or(50))?;
        let entries: Vec<_> = kills
            .iter()
            .map(|dead| {
                let victim_name = dead.victim.ucid().map(|u| u.to_string());
                let victim_side = format!("{:?}", dead.victim.side());
                // Same classification as air_kills/ground_kills in the stats
                // aggregator (record_kill) -- lets API consumers (e.g. the
                // Discord kill-streak/achievement poller) filter on air kills
                // specifically instead of guessing from target_type's raw DCS
                // unit-type string.
                let is_air = db.victim_is_air(rid, &dead.victim).unwrap_or(false);
                let killer = dead
                    .shots
                    .iter()
                    .find(|s| s.hit)
                    .map(|s| {
                        serde_json::json!({
                            "ucid": s.shooter.ucid().map(|u| u.to_string()),
                            "side": format!("{:?}", s.shooter.side()),
                            "weapon": s.weapon_name.as_ref().map(|w| w.to_string()),
                            "airframe": s.shooter_typ.as_deref(),
                        })
                    });
                serde_json::json!({
                    "time": dead.time.to_rfc3339(),
                    "victim": {
                        "ucid": victim_name,
                        "side": victim_side,
                    },
                    "killer": killer,
                    "target_type": dead.shots.first().map(|s| s.target_typ.to_string()),
                    "is_air": is_air,
                })
            })
            .collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

async fn api_pilot(
    ucid: String,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let ucid: dcso3::net::Ucid = ucid.parse().map_err(|e| anyhow::anyhow!("{e:?}"))?;
        match db.pilot_detail(&ucid)? {
            None => Ok(serde_json::json!({ "error": "not found" }).to_string()),
            Some((name, agg)) => Ok(serde_json::to_string(&serde_json::json!({
                "ucid": ucid.to_string(),
                "name": name.to_string(),
                "air_kills": agg.air_kills,
                "ground_kills": agg.ground_kills,
                "captures": agg.captures,
                "repairs": agg.repairs,
                "supply_transfers": agg.supply_transfers,
                "troops": agg.troops,
                "farps": agg.farps,
                "deploys": agg.deploys,
                "actions": agg.actions,
                "deaths": agg.deaths,
                "hours": agg.hours,
                "donated_points": agg.donated_points,
            }))?),
        }
    })?;
    Ok(json_response(data))
}

/// GET /api/pilot/:ucid/sorties — all sorties for a pilot
async fn api_pilot_sorties(
    ucid: String,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let ucid: dcso3::net::Ucid = ucid.parse().map_err(|e| anyhow::anyhow!("{e:?}"))?;
        let sorties = db.pilot_sorties(&ucid)?;
        let entries: Vec<_> = sorties.iter().rev().map(|(round_id, _sortie_id, s)| {
            let duration_secs = s.land
                .map(|l| (l - s.takeoff).num_seconds())
                .unwrap_or(0);
            serde_json::json!({
                "round_id": round_id.0,
                "aircraft": s.vehicle.to_string(),
                "takeoff": s.takeoff.to_rfc3339(),
                "land": s.land.map(|l| l.to_rfc3339()),
                "duration_secs": duration_secs,
                "landed": s.land.is_some(),
            })
        }).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

/// GET /api/pilot/:ucid/breakdown — per-round aggregates for a pilot
async fn api_pilot_breakdown(
    ucid: String,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let ucid: dcso3::net::Ucid = ucid.parse().map_err(|e| anyhow::anyhow!("{e:?}"))?;
        let rounds = db.pilot_round_breakdown(&ucid)?;
        let entries: Vec<_> = rounds.iter().map(|(scenario, rid, agg)| serde_json::json!({
            "round_id": rid.0,
            "scenario": scenario,
            "air_kills": agg.air_kills,
            "ground_kills": agg.ground_kills,
            "captures": agg.captures,
            "repairs": agg.repairs,
            "supply_transfers": agg.supply_transfers,
            "troops": agg.troops,
            "farps": agg.farps,
            "deploys": agg.deploys,
            "actions": agg.actions,
            "deaths": agg.deaths,
            "hours": agg.hours,
        })).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

/// GET /api/pilot/:ucid/kills — all kills made by a pilot
async fn api_pilot_kills(
    ucid: String,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let ucid: dcso3::net::Ucid = ucid.parse().map_err(|e| anyhow::anyhow!("{e:?}"))?;
        let kills = db.pilot_kills_for(&ucid)?;
        let entries: Vec<_> = kills.iter().map(|(round_id, dead)| {
            let shot = dead.shots.iter().find(|s| s.hit || dead.shots.len() == 1);
            let weapon = shot.and_then(|s| s.weapon_name.as_ref().map(|w| w.to_string()));
            let airframe = shot.and_then(|s| s.shooter_typ.as_deref().map(|t| t.to_string()));
            let target_type = shot.map(|s| s.target_typ.to_string());
            let victim_ucid = dead.victim.ucid().map(|u| u.to_string());
            let victim_side = format!("{:?}", dead.victim.side());
            serde_json::json!({
                "round_id": round_id.0,
                "time": dead.time.to_rfc3339(),
                "victim_ucid": victim_ucid,
                "victim_side": victim_side,
                "target_type": target_type,
                "weapon": weapon,
                "killer_airframe": airframe,
            })
        }).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

async fn api_stats(
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let mut value = task::block_in_place(|| -> Result<serde_json::Value> {
        let rounds = db.latest_rounds()?;
        let active_round = rounds.iter().find(|(_, _, r)| r.end.is_none());
        let active_rid = active_round.map(|(_, rid, _)| *rid);
        let pilots = db.pilot_leaderboard(active_rid)?;
        let obj_count = if let Some((_, rid, _)) = active_round {
            // Match the anti-cheat filtering in api_objectives (hides carrier
            // groups and special SAM sites) so this count stays consistent
            // with what /api/objectives actually reports.
            db.objectives_for_round(*rid)?
                .iter()
                .filter(|(_, obj)| !obj.kind.is_carrier_group() && !obj.kind.is_special_sam_site())
                .count()
        } else {
            0
        };
        let total_kills: u32 = pilots.iter().map(|(_, _, a)| a.air_kills + a.ground_kills).sum();
        // Fallback only -- bflib's own session stop_time, used when
        // DCSServerBot isn't configured/reachable (see below, api_stats
        // prefers the bot's own scheduler restart time when available,
        // since that's what actually restarts the server on this setup).
        let local_restart_at = active_round
            .and_then(|(_, rid, _)| db.active_session_stop(*rid))
            .map(|t| t.to_rfc3339());
        let weather = db.latest_weather().map(|w| serde_json::json!({
            "temp_c": w.temp_c,
            "wind_speed_kts": w.wind_speed_kts,
            "wind_from_deg": w.wind_from_deg,
            "cloud_base_m": w.cloud_base_m,
            "qnh_hpa": w.qnh_hpa,
            "cloud_density": w.cloud_density,
            "visibility_m": w.visibility_m,
        }));
        let (blue_reg, red_reg, blue_online, red_online) = if let Some((_, rid, _)) = active_round {
            db.pilot_side_counts(*rid).unwrap_or_default()
        } else {
            (0, 0, 0, 0)
        };
        Ok(serde_json::json!({
            "total_pilots": pilots.len(),
            "total_rounds": rounds.len(),
            "active_round": active_round.map(|(s, rid, r)| serde_json::json!({
                "id": rid.0,
                "scenario": s.to_string(),
                "start": r.start.to_rfc3339(),
            })),
            "objective_count": obj_count,
            "total_kills": total_kills,
            "restart_at": local_restart_at,
            "weather": weather,
            "blue_registered": blue_reg,
            "red_registered": red_reg,
            "blue_online": blue_online,
            "red_online": red_online,
        }))
    })?;

    let bot_info = fetch_bot_server_info(&bot_cfg).await;

    // DCSServerBot's Scheduler plugin is what actually restarts this server
    // (bflib's own stop_time isn't in play here) -- prefer its restart_time
    // when reachable, keep the bflib-derived fallback above otherwise. The
    // bot appears to hold onto the last-computed restart_time rather than
    // always keeping a future one queued (observed: it can sit on an
    // already-elapsed moment for a while after a restart), so only trust
    // it if it's actually still ahead of us -- a countdown to the past
    // just clamps to zero and sits there, which reads as broken rather
    // than "no restart currently scheduled".
    if let Some(bot_restart_at) = bot_info.as_ref().and_then(|s| s.restart_time.as_deref()).and_then(parse_bot_datetime) {
        if bot_restart_at > chrono::Utc::now() {
            value["restart_at"] = serde_json::json!(bot_restart_at.to_rfc3339());
        } else {
            value["restart_at"] = serde_json::Value::Null;
        }
    }

    // Fallback only -- this project has a purpose-built weather pipeline
    // (bftools --live-weather syncs real weather into the mission file,
    // bflib reads it via atmosphere.getWind and publishes Stat::Weather),
    // which is the real source of truth once it's working. Only reach for
    // the bot's generic /servers weather reading (same response as
    // restart_time above, no extra request) when bflib hasn't published
    // anything at all, e.g. before that pipeline's fix has been deployed.
    // See BotWeather for the unit conversions this needs (feet->meters,
    // mmHg->hPa).
    if value["weather"].is_null() {
        if let Some(w) = bot_info.as_ref().and_then(|s| s.weather.as_ref()) {
            value["weather"] = serde_json::json!({
                "temp_c": w.temperature,
                "wind_speed_kts": w.wind_speed,
                "wind_from_deg": w.wind_direction,
                "cloud_base_m": w.clouds_base.map(|ft| ft * 0.3048),
                "qnh_hpa": w.pressure.map(|mmhg| mmhg * 1.33322),
                "cloud_density": w.clouds_density,
                "visibility_m": w.visibility,
            });
        }
    }

    Ok(json_response(serde_json::to_string(&value).map_err(anyhow::Error::from)?))
}

async fn api_points(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => return Ok("[]".to_string()),
        };
        let entries = db.pilot_points(rid)?;
        let json: Vec<_> = entries.iter().map(|(name, pts, side)| serde_json::json!({
            "name": name, "points": pts, "side": side,
        })).collect();
        Ok(serde_json::to_string(&json)?)
    })?;
    Ok(json_response(data))
}

async fn api_captures(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => return Ok("[]".to_string()),
        };
        let entries = db.most_captured(rid)?;
        let json: Vec<_> = entries.iter().map(|(name, count)| serde_json::json!({
            "name": name, "count": count,
        })).collect();
        Ok(serde_json::to_string(&json)?)
    })?;
    Ok(json_response(data))
}

/// Recent capture events with pilot attribution -- distinct from
/// /api/captures, which is just a per-objective running count with no
/// timeline or "who did it".
async fn api_capture_events(
    db: StatsDb,
    round_id: Option<u64>,
    limit: Option<usize>,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
        let rid = match round_id {
            Some(id) => db::RoundId(id),
            None => match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
                Some((_, rid, _)) => *rid,
                None => match rounds.first() {
                    Some((_, rid, _)) => *rid,
                    None => return Ok("[]".to_string()),
                },
            },
        };
        let entries = db.recent_captures(rid, limit.unwrap_or(50))?;
        let json: Vec<_> = entries
            .iter()
            .map(|c| {
                serde_json::json!({
                    "time": c.time.to_rfc3339(),
                    "objective": c.objective_name,
                    "side": format!("{:?}", c.side),
                    "by": c.by.iter().map(|u| u.to_string()).collect::<Vec<_>>(),
                })
            })
            .collect();
        Ok(serde_json::to_string(&json)?)
    })?;
    Ok(json_response(data))
}

async fn api_aircraft_usage(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => return Ok("[]".to_string()),
        };
        let entries = db.aircraft_usage(rid)?;
        let json: Vec<_> = entries.iter().map(|(vehicle, count, hours)| serde_json::json!({
            "vehicle": vehicle, "sorties": count, "hours": (hours * 10.0).round() / 10.0,
        })).collect();
        Ok(serde_json::to_string(&json)?)
    })?;
    Ok(json_response(data))
}

async fn api_online(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => return Ok("[]".to_string()),
        };
        let pilots = db.connected_pilots(rid)?;
        let entries: Vec<_> = pilots.iter().map(|(ucid, name, side, aircraft)| {
            serde_json::json!({
                "ucid": ucid,
                "name": name,
                "side": format!("{:?}", side),
                "aircraft": aircraft,
            })
        }).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

async fn api_units(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => match rounds.first() {
                Some((_, rid, _)) => *rid,
                None => return Ok("[]".to_string()),
            },
        };
        let units = db.detected_units_for_round(rid)?;
        let entries: Vec<_> = units
            .iter()
            .filter(|(_, unit, _)| !unit.tags.contains(UnitTag::Boat))
            .map(|(eid, unit, flags)| {
                let vel = &unit.pos.velocity;
                // DCS world axes: x=north, z=east. Bearing is atan2(east, north).
                let heading = (vel.z.atan2(vel.x).to_degrees() + 360.0) % 360.0;
                let speed_mps = (vel.x * vel.x + vel.y * vel.y + vel.z * vel.z).sqrt();
                let speed_kts = speed_mps * 1.94384;
                let tags: Vec<String> = unit.tags.iter().map(|t| format!("{:?}", t)).collect();
                let detected_by: Vec<String> = flags.iter().map(|d| format!("{:?}", d)).collect();
                serde_json::json!({
                    "id": format!("{}", eid),
                    "owner": format!("{:?}", unit.owner),
                    "typ": unit.typ.to_string(),
                    "tags": tags,
                    "lat": unit.pos.pos.latitude,
                    "lon": unit.pos.pos.longitude,
                    "alt": unit.pos.pos.altitude,
                    "heading": heading,
                    "speed": speed_kts,
                    "detected_by": detected_by,
                })
            })
            .collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

// ── Auth handlers ────────────────────────────────────────────────────

#[derive(Deserialize)]
struct LoginQuery {
    /// Where to send the browser after a successful login -- the initiating
    /// frontend's own origin (e.g. "https://dashboard.vectorstrike.org/" or
    /// "https://wiki.vectorstrike.org/"). Only trusted if it exactly matches
    /// one of the configured --cors-origin values; anything else is dropped
    /// silently rather than erroring, since an open redirect here (this
    /// endpoint has no auth gate) would be a phishing vector.
    return_to: Option<std::string::String>,
}

/// GET /api/auth/login  — redirect to Discord OAuth
async fn api_auth_login(
    q: LoginQuery,
    cfg: AuthConfig,
    db: StatsDb,
    allowed_origins: Arc<Vec<std::string::String>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let state = Uuid::new_v4();
    let return_to = q.return_to
        .filter(|rt| allowed_origins.iter().any(|o| rt == &format!("{o}/")));
    task::block_in_place(|| db.store_oauth_state(state, return_to))?;
    let url = format!(
        "https://discord.com/oauth2/authorize?client_id={}&redirect_uri={}&response_type=code&scope=identify+guilds.members.read&state={}",
        cfg.client_id,
        urlencoding::encode(&cfg.redirect_uri),
        state,
    );
    Ok(warp::redirect::temporary(url.parse::<warp::http::Uri>().unwrap()))
}

#[derive(Deserialize)]
struct CallbackQuery { code: String, state: String }

#[derive(Deserialize)]
struct DiscordUser { id: String, username: String, avatar: Option<String> }

#[derive(Deserialize)]
struct GuildMember { roles: Vec<String> }

#[derive(Deserialize)]
struct TokenResponse { access_token: String }

/// GET /api/auth/callback?code=&state=  — exchange code, create session
/// Session cookie attributes. SameSite=None (needed for cross-origin fetch/XHR
/// with credentials) requires Secure, which requires bfdb to be served over
/// TLS — so we only opt into it when --cors-origin was actually configured.
fn session_cookie_attrs(cross_origin: bool) -> &'static str {
    if cross_origin { "SameSite=None; Secure" } else { "SameSite=Lax" }
}

async fn api_auth_callback(
    q: CallbackQuery,
    cfg: AuthConfig,
    db: StatsDb,
    cross_origin: bool,
) -> std::result::Result<impl warp::Reply, Error> {
    // Validate state, recovering which frontend origin to send the browser
    // back to (falls back to bfdb's own "/" for embedded-mode deployments
    // that never passed a return_to at /api/auth/login).
    let state_uuid = q.state.parse::<Uuid>().map_err(|e| anyhow::anyhow!("bad state: {e}"))?;
    let Some(return_to) = task::block_in_place(|| db.take_oauth_state(state_uuid))? else {
        return Err(anyhow::anyhow!("invalid or expired OAuth state").into());
    };
    let redirect_location = return_to.unwrap_or_else(|| "/".to_string());

    let http = reqwest::Client::new();

    // Exchange code for access token
    let token_res: TokenResponse = http
        .post("https://discord.com/api/oauth2/token")
        .form(&[
            ("client_id",     cfg.client_id.as_str()),
            ("client_secret", cfg.client_secret.as_str()),
            ("grant_type",    "authorization_code"),
            ("code",          q.code.as_str()),
            ("redirect_uri",  cfg.redirect_uri.as_str()),
        ])
        .send()
        .await
        .map_err(|e| anyhow::anyhow!("token exchange failed: {e}"))?
        .json()
        .await
        .map_err(|e| anyhow::anyhow!("token parse failed: {e}"))?;

    // Fetch Discord user
    let user: DiscordUser = http
        .get("https://discord.com/api/users/@me")
        .bearer_auth(&token_res.access_token)
        .send()
        .await
        .map_err(|e| anyhow::anyhow!("user fetch failed: {e}"))?
        .json()
        .await
        .map_err(|e| anyhow::anyhow!("user parse failed: {e}"))?;

    // Check guild role for admin
    let is_admin = match http
        .get(format!("https://discord.com/api/users/@me/guilds/{}/member", cfg.guild_id))
        .bearer_auth(&token_res.access_token)
        .send()
        .await
    {
        Ok(r) => r.json::<GuildMember>().await.ok()
            .map(|m| m.roles.contains(&cfg.admin_role_id))
            .unwrap_or(false),
        Err(_) => false,
    };

    let session_id = Uuid::new_v4();
    let session = SessionData {
        discord_id: user.id.clone(),
        username:   user.username.clone(),
        avatar:     user.avatar.clone(),
        is_admin,
        expires:    chrono::Utc::now() + chrono::Duration::days(7),
    };
    task::block_in_place(|| db.create_session(session_id, session))?;

    let cookie = format!(
        "session={}; Path=/; HttpOnly; {}; Max-Age=604800",
        session_id, session_cookie_attrs(cross_origin)
    );
    Ok(warp::http::Response::builder()
        .status(302)
        .header("location", redirect_location)
        .header("set-cookie", cookie)
        .body("")
        .unwrap())
}

/// GET /api/auth/me  — return current user (reads session cookie)
async fn api_auth_me(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    // Return 200 with null user when not logged in — avoids browser console errors
    // since this endpoint is polled on every page load to check auth state
    let Some(id) = session_id else {
        return Ok(json_response(r#"{"user":null}"#.to_string()));
    };
    let session = task::block_in_place(|| db.get_session(id))?;
    let Some(s) = session else {
        return Ok(json_response(r#"{"user":null}"#.to_string()));
    };
    let ucid = resolve_ucid_via_bot(&bot_cfg, &s.discord_id).await
        .map(|u| u.to_string());
    Ok(json_response(serde_json::to_string(&serde_json::json!({
        "user": {
            "discord_id": s.discord_id,
            "username":   s.username,
            "avatar":     s.avatar,
            "is_admin":   s.is_admin,
            "ucid":       ucid,
        }
    })).map_err(anyhow::Error::from)?))
}

/// GET /api/auth/logout  — clear session cookie
async fn api_auth_logout(
    session_id: Option<Uuid>,
    db: StatsDb,
    cross_origin: bool,
) -> std::result::Result<impl warp::Reply, Error> {
    if let Some(id) = session_id {
        task::block_in_place(|| db.delete_session(id))?;
    }
    let cookie = format!("session=; Path=/; HttpOnly; {}; Max-Age=0", session_cookie_attrs(cross_origin));
    Ok(warp::http::Response::builder()
        .status(200)
        .header("set-cookie", cookie)
        .body("")
        .unwrap())
}

#[derive(Deserialize)]
struct LocalLoginBody { username: String, password: String }

/// POST /api/auth/local-login  — username/password admin login (no Discord required)
async fn api_auth_local_login(
    body: LocalLoginBody,
    local_cfg: Option<LocalAdminConfig>,
    db: StatsDb,
    cross_origin: bool,
) -> std::result::Result<impl warp::Reply, Error> {
    let cfg = local_cfg
        .ok_or_else(|| anyhow::anyhow!("Local login is not enabled on this server"))?;
    if body.username != cfg.username || body.password != cfg.password {
        return Err(anyhow::anyhow!("Invalid username or password").into());
    }
    let session_id = Uuid::new_v4();
    let session = SessionData {
        discord_id: format!("local:{}", cfg.username),
        username:   cfg.username.clone(),
        avatar:     None,
        is_admin:   true,
        expires:    chrono::Utc::now() + chrono::Duration::days(7),
    };
    task::block_in_place(|| db.create_session(session_id, session))?;
    let cookie = format!(
        "session={}; Path=/; HttpOnly; {}; Max-Age=604800",
        session_id, session_cookie_attrs(cross_origin)
    );
    Ok(warp::http::Response::builder()
        .status(200)
        .header("set-cookie", cookie)
        .header("content-type", "application/json")
        .body(r#"{"ok":true}"#)
        .unwrap())
}

// ── Admin handlers ───────────────────────────────────────────────────

async fn require_admin(session_id: Option<Uuid>, db: StatsDb) -> std::result::Result<SessionData, Error> {
    let Some(id) = session_id else {
        return Err(anyhow::anyhow!("not logged in").into());
    };
    let session = task::block_in_place(|| db.get_session(id))?
        .ok_or_else(|| anyhow::anyhow!("session expired"))?;
    if !session.is_admin {
        return Err(anyhow::anyhow!("forbidden").into());
    }
    Ok(session)
}

// ── Cockpit UI handlers ──────────────────────────────────────────────
// Identifies the calling player and forwards to a player-scoped bflib RPC
// (see bflib/src/bg/rpcs.rs "Cockpit UI API"). Two ways in:
//  - `?playerid=<id>` from bflib/lua/cockpit.lua, the in-DCS Hooks-script
//    overlay -- <id> is net.get_my_player_id(), meaningful only for the
//    current connection, resolved to a ucid via the live connected-player
//    table (bflib's "resolve-player-id" RPC). No manual step: it's only
//    valid while that player is actually connected.
//  - a browser session cookie linked to a Discord account, for testing the
//    standalone /cockpit page outside DCS.

async fn resolve_by_player_id(id: i64, db: &StatsDb) -> std::result::Result<dcso3::net::Ucid, Error> {
    use netidx::publisher::Value;
    let s = call_engine_rpc_str(db, "resolve-player-id", vec![("id", Value::from(id))]).await?;
    s.parse::<dcso3::net::Ucid>().map_err(|e| anyhow::anyhow!("bad ucid from engine: {e:?}").into())
}

async fn require_linked_player(
    query: &std::collections::HashMap<std::string::String, std::string::String>,
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: &Arc<Option<BotLinkConfig>>,
) -> std::result::Result<dcso3::net::Ucid, Error> {
    if let Some(id) = query.get("playerid").and_then(|s| s.parse::<i64>().ok()) {
        return resolve_by_player_id(id, &db).await;
    }
    let Some(id) = session_id else {
        return Err(anyhow::anyhow!("not logged in").into());
    };
    let session = task::block_in_place(|| db.get_session(id))?
        .ok_or_else(|| anyhow::anyhow!("session expired"))?;
    let ucid = resolve_ucid_via_bot(bot_cfg, &session.discord_id).await
        .ok_or_else(|| anyhow::anyhow!("account not linked -- type -linkme <token> in DCS chat (get the token with /linkme in Discord)"))?;
    Ok(ucid)
}

async fn api_cockpit_ewr_report(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg).await?;
    let friendly = query.get("friendly").map(|s| s == "true").unwrap_or(false);
    use netidx::publisher::Value;
    let report = call_engine_rpc_str(&db, "ewr-report", vec![
        ("ucid", Value::from(ucid.to_string())),
        ("friendly", Value::from(friendly)),
    ]).await?;
    Ok(warp::reply::json(&serde_json::json!({ "report": report })))
}

async fn api_cockpit_ewr_toggle(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg).await?;
    use netidx::publisher::Value;
    let state = call_engine_rpc_str(&db, "ewr-toggle", vec![
        ("ucid", Value::from(ucid.to_string())),
    ]).await?;
    Ok(warp::reply::json(&serde_json::json!({ "state": state })))
}

#[derive(serde::Deserialize)]
struct EwrUnitsBody {
    imperial: bool,
}

async fn api_cockpit_ewr_units(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: EwrUnitsBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg).await?;
    use netidx::publisher::Value;
    let units = call_engine_rpc_str(&db, "ewr-set-units", vec![
        ("ucid", Value::from(ucid.to_string())),
        ("imperial", Value::from(body.imperial)),
    ]).await?;
    Ok(warp::reply::json(&serde_json::json!({ "units": units })))
}

async fn api_cockpit_ewr_intel(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg).await?;
    use netidx::publisher::Value;
    let report = call_engine_rpc_str(&db, "ewr-ground-intel", vec![
        ("ucid", Value::from(ucid.to_string())),
    ]).await?;
    Ok(warp::reply::json(&serde_json::json!({ "report": report })))
}

/// GET /api/cockpit/carp/solve?key=<mark text>&altft=<drop altitude AGL, ft>
/// Solves CARP INIT 1/5, 3/5 and 4/5 auto-fillable fields for the PI marked
/// on the F10 map with the given text -- see bflib/src/carp.rs.
async fn api_cockpit_carp_solve(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg).await?;
    let key = query.get("key").cloned().ok_or_else(|| anyhow::anyhow!("missing key"))?;
    let alt_ft: f64 = query.get("altft")
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| anyhow::anyhow!("missing or invalid altft"))?;
    use netidx::publisher::Value;
    let json = call_engine_rpc_str(&db, "carp-solve", vec![
        ("ucid", Value::from(ucid.to_string())),
        ("mark_key", Value::from(key)),
        ("drop_altitude_agl_ft", Value::from(alt_ft)),
    ]).await?;
    let solution: serde_json::Value = serde_json::from_str(&json)
        .map_err(|e| anyhow::anyhow!("bad carp solution from engine: {e:?}"))?;
    Ok(warp::reply::json(&solution))
}

/// GET /api/cockpit/carp/solve-latlon?lat=<>&lon=<>&altft=<drop altitude AGL, ft>
/// Same as api_cockpit_carp_solve, but for a PI given directly as lat/long
/// (e.g. a click on the dashboard's map) instead of an F10 mark's text.
async fn api_cockpit_carp_solve_latlon(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg).await?;
    let lat: f64 = query.get("lat")
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| anyhow::anyhow!("missing or invalid lat"))?;
    let lon: f64 = query.get("lon")
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| anyhow::anyhow!("missing or invalid lon"))?;
    let alt_ft: f64 = query.get("altft")
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| anyhow::anyhow!("missing or invalid altft"))?;
    use netidx::publisher::Value;
    let json = call_engine_rpc_str(&db, "carp-solve-latlon", vec![
        ("ucid", Value::from(ucid.to_string())),
        ("lat", Value::from(lat)),
        ("lon", Value::from(lon)),
        ("drop_altitude_agl_ft", Value::from(alt_ft)),
    ]).await?;
    let solution: serde_json::Value = serde_json::from_str(&json)
        .map_err(|e| anyhow::anyhow!("bad carp solution from engine: {e:?}"))?;
    Ok(warp::reply::json(&solution))
}

#[derive(serde::Deserialize)]
struct CargoSpawnBody {
    crate_name: std::string::String,
    qty: u32,
    c130: bool,
}

/// POST /api/cockpit/cargo/spawn?playerid=<> -- queue qty copies of a named
/// crate for the calling player's current slot. See bflib's
/// AdminCommand::CockpitSpawnCrate / menu/cargo.rs's spawn_crates_for_ucid,
/// the same logic the F10 "Spawn N Crates" items call.
async fn api_cockpit_cargo_spawn(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: CargoSpawnBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg).await?;
    if body.qty < 1 {
        return Err(anyhow::anyhow!("qty must be at least 1").into());
    }
    use netidx::publisher::Value;
    let msg = call_engine_rpc_str(&db, "cargo-spawn-crate", vec![
        ("ucid", Value::from(ucid.to_string())),
        ("crate_name", Value::from(body.crate_name)),
        ("qty", Value::from(body.qty as i64)),
        ("c130", Value::from(body.c130)),
    ]).await?;
    Ok(warp::reply::json(&serde_json::json!({ "message": msg })))
}

/// GET /api/admin/sessions  — list active sessions
async fn api_admin_sessions(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let sessions = db.list_sessions()?;
        let entries: Vec<_> = sessions.iter().map(|(_, s)| {
            serde_json::json!({
                "discord_id": s.discord_id,
                "username":   s.username,
                "avatar":     s.avatar,
                "is_admin":   s.is_admin,
                "expires":    s.expires.to_rfc3339(),
            })
        }).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}


/// POST /api/admin/reset  — wipe all campaign data, keep auth & Discord links
async fn api_admin_reset(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    task::block_in_place(|| db.reset_campaign_data())?;
    log::info!("ADMIN: campaign data reset by admin");
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

/// GET /api/admin/cfg  — read the current campaign engine config JSON (admin only)
async fn api_admin_cfg_get(
    session_id: Option<Uuid>,
    db: StatsDb,
    path: Arc<Option<PathBuf>>,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let path = path
        .as_ref()
        .clone()
        .ok_or_else(|| anyhow::anyhow!("engine config not configured (missing --engine-config)"))?;
    let data = task::block_in_place(|| -> Result<String> {
        std::fs::read_to_string(&path)
            .map_err(|e| anyhow::anyhow!("reading {:?}: {e}", path))
    })?;
    Ok(json_response(data))
}

/// GET /api/admin/cfg/schema  — JSON Schema generated from the real Cfg type
/// bflib actually parses, so the editor UI can never drift out of sync with
/// the engine (admin only)
async fn api_admin_cfg_schema(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let schema = schemars::schema_for!(bfprotocols::cfg::Cfg);
    Ok(warp::reply::json(&schema))
}

#[derive(Deserialize)]
struct SaveCfgBody {
    cfg: serde_json::Value,
}

/// POST /api/admin/cfg  — validate and save a new campaign engine config
/// (admin only). Validation deserializes the body into the real Cfg type
/// bflib loads, so malformed edits are rejected here instead of silently
/// breaking the server at next restart. The previous file is backed up
/// alongside the new one before being overwritten. Takes effect on the next
/// mission/server restart — bflib only reads Cfg once at startup.
async fn api_admin_cfg_post(
    session_id: Option<Uuid>,
    body: SaveCfgBody,
    db: StatsDb,
    path: Arc<Option<PathBuf>>,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let path = path
        .as_ref()
        .clone()
        .ok_or_else(|| anyhow::anyhow!("engine config not configured (missing --engine-config)"))?;
    let _validated: bfprotocols::cfg::Cfg = serde_json::from_value(body.cfg.clone())
        .map_err(|e| anyhow::anyhow!("config is invalid: {e}"))?;
    let pretty = serde_json::to_string_pretty(&body.cfg)
        .map_err(|e| anyhow::anyhow!("serializing config: {e}"))?;
    task::block_in_place(|| -> Result<()> {
        if path.exists() {
            let backup = path.with_file_name(format!(
                "{}.bak.{}",
                path.file_name().and_then(|n| n.to_str()).unwrap_or("cfg"),
                chrono::Utc::now().format("%Y%m%d%H%M%S")
            ));
            std::fs::copy(&path, &backup)
                .map_err(|e| anyhow::anyhow!("backing up {:?} to {:?}: {e}", path, backup))?;
        }
        std::fs::write(&path, pretty)
            .map_err(|e| anyhow::anyhow!("writing {:?}: {e}", path))?;
        Ok(())
    })?;
    log::info!("ADMIN: engine config saved");
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

// ── bfwiki content API ─────────────────────────────────────────────────────

/// GET /api/wiki/pages  — list all pages (slug/title/section/order only, no
/// content) for building the sidebar. Public -- reading the wiki needs no
/// login, only editing does.
async fn api_wiki_list(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let pages = task::block_in_place(|| db.wiki_list_pages())?;
    let entries: Vec<_> = pages.iter().map(|(slug, p)| {
        serde_json::json!({
            "slug": slug,
            "title": p.title,
            "section": p.section,
            "order": p.order,
        })
    }).collect();
    Ok(warp::reply::json(&entries))
}

/// GET /api/wiki/pages/<slug>  — full content of one page. Public. `<slug>`
/// is itself multi-segment (e.g. "gameplay/objectives"), so this matches on
/// the path tail rather than a single `String` segment.
async fn api_wiki_get(
    slug: warp::path::Tail,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let slug = slug.as_str();
    let page = task::block_in_place(|| db.wiki_get_page(slug))?
        .ok_or_else(|| anyhow::anyhow!("page not found"))?;
    Ok(warp::reply::json(&serde_json::json!({
        "slug": slug,
        "title": page.title,
        "section": page.section,
        "order": page.order,
        "content": page.content,
        "updated_at": page.updated_at,
        "updated_by": page.updated_by,
    })))
}

#[derive(Deserialize)]
struct SaveWikiPageBody {
    title:   std::string::String,
    section: std::string::String,
    order:   i32,
    content: std::string::String,
}

/// POST /api/wiki/pages/<slug>  — create or overwrite a page (admin only).
async fn api_wiki_save(
    slug: warp::path::Tail,
    session_id: Option<Uuid>,
    body: SaveWikiPageBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let session = require_admin(session_id, db.clone()).await?;
    let slug = slug.as_str().to_string();
    let page = WikiPage {
        title:      body.title,
        section:    body.section,
        order:      body.order,
        content:    body.content,
        updated_at: chrono::Utc::now(),
        updated_by: session.discord_id,
    };
    task::block_in_place(|| db.wiki_save_page(&slug, page))?;
    log::info!("ADMIN: wiki page '{}' saved", slug);
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

#[derive(Deserialize)]
struct DeleteWikiPageBody {
    slug: std::string::String,
}

/// POST /api/wiki/delete  — remove a page (admin only). Takes the slug in
/// the body rather than the path so a multi-segment slug can't collide with
/// a literal trailing path segment.
async fn api_wiki_delete(
    session_id: Option<Uuid>,
    body: DeleteWikiPageBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    task::block_in_place(|| db.wiki_delete_page(&body.slug))?;
    log::info!("ADMIN: wiki page '{}' deleted", body.slug);
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

const MAX_WIKI_IMAGE_BYTES: u64 = 8 * 1024 * 1024;

/// POST /api/wiki/images  — upload an image (admin only). Body is the raw
/// image bytes; the `content-type` request header determines how it's
/// served back and is validated to actually be an image type. Capped at
/// `MAX_WIKI_IMAGE_BYTES` via the route's `content_length_limit` filter.
async fn api_wiki_upload_image(
    session_id: Option<Uuid>,
    content_type: std::string::String,
    body: bytes::Bytes,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let session = require_admin(session_id, db.clone()).await?;
    if !content_type.starts_with("image/") {
        return Err(anyhow::anyhow!("only image uploads are allowed (got content-type '{content_type}')").into());
    }
    let id = Uuid::new_v4();
    let image = WikiImage {
        content_type,
        data: body.to_vec(),
        uploaded_at: chrono::Utc::now(),
        uploaded_by: session.discord_id,
    };
    task::block_in_place(|| db.wiki_save_image(id, image))?;
    log::info!("ADMIN: wiki image {id} uploaded");
    Ok(warp::reply::json(&serde_json::json!({
        "id": id.to_string(),
        "url": format!("/api/wiki/images/{id}"),
    })))
}

/// GET /api/wiki/images/<id>  — serve an uploaded image. Public -- images
/// embedded in wiki pages need to load for anonymous readers too. Cached
/// aggressively since an id's content never changes after upload.
async fn api_wiki_get_image(
    id: Uuid,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let image = task::block_in_place(|| db.wiki_get_image(&id))?
        .ok_or_else(|| anyhow::anyhow!("image not found"))?;
    Ok(warp::http::Response::builder()
        .header("content-type", image.content_type)
        .header("cache-control", "public, max-age=31536000, immutable")
        .body(image.data)
        .unwrap())
}

/// GET /api/admin/perf  — last session's DCS engine performance stats (admin only)
/// Snapshot this host's CPU/RAM/disk/GPU usage and available temperature
/// sensors. Two CPU refreshes with a short sleep in between are required
/// because CPU usage in `sysinfo` is a delta measurement -- a single refresh
/// right after process start always reads 0.
fn collect_hardware() -> serde_json::Value {
    use sysinfo::{Components, Disks, System};

    let mut sys = System::new_all();
    sys.refresh_cpu_usage();
    std::thread::sleep(sysinfo::MINIMUM_CPU_UPDATE_INTERVAL);
    sys.refresh_cpu_usage();
    sys.refresh_memory();

    let cpus = sys.cpus();
    let cpu_usage: f32 = if cpus.is_empty() {
        0.
    } else {
        cpus.iter().map(|c| c.cpu_usage()).sum::<f32>() / cpus.len() as f32
    };

    let disks = Disks::new_with_refreshed_list();
    let disk_rows: Vec<serde_json::Value> = disks
        .iter()
        .map(|d| {
            let total = d.total_space();
            let avail = d.available_space();
            let used = total.saturating_sub(avail);
            serde_json::json!({
                "mount": d.mount_point().to_string_lossy(),
                "total_bytes": total,
                "used_bytes": used,
            })
        })
        .collect();

    // Best-effort: not every sensor DCS servers expose is visible to Windows,
    // so this can legitimately come back empty depending on drivers/hardware.
    let components = Components::new_with_refreshed_list();
    let temp_rows: Vec<serde_json::Value> = components
        .iter()
        .filter(|c| !c.temperature().is_nan())
        .map(|c| {
            serde_json::json!({
                "label": c.label(),
                "celsius": c.temperature(),
            })
        })
        .collect();

    let gpu = collect_gpu();

    serde_json::json!({
        "cpu_count": cpus.len(),
        "cpu_usage_pct": cpu_usage,
        "mem_total_bytes": sys.total_memory(),
        "mem_used_bytes": sys.used_memory(),
        "disks": disk_rows,
        "temps": temp_rows,
        "gpu": gpu,
    })
}

/// Best-effort NVIDIA GPU stats via NVML. Returns null if NVML isn't
/// available (no NVIDIA driver, or no GPU) rather than failing the whole
/// hardware snapshot -- this endpoint should degrade gracefully.
fn collect_gpu() -> serde_json::Value {
    use nvml_wrapper::{enum_wrappers::device::TemperatureSensor, Nvml};

    let result: anyhow::Result<serde_json::Value> = (|| {
        let nvml = Nvml::init()?;
        let device = nvml.device_by_index(0)?;
        let name = device.name()?;
        let util = device.utilization_rates()?;
        let mem = device.memory_info()?;
        let temp = device.temperature(TemperatureSensor::Gpu).ok();
        Ok(serde_json::json!({
            "available": true,
            "name": name,
            "usage_pct": util.gpu,
            "mem_usage_pct": util.memory,
            "mem_total_bytes": mem.total,
            "mem_used_bytes": mem.used,
            "celsius": temp,
        }))
    })();

    match result {
        Ok(v) => v,
        Err(e) => {
            log::warn!("GPU stats unavailable: {e:?}");
            serde_json::json!({ "available": false })
        }
    }
}

async fn api_admin_perf(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let hardware = collect_hardware();
        let end = db.latest_session_end()?;
        let json = match end {
            None => serde_json::json!({ "available": false, "hardware": hardware }),
            Some(e) => {
                let ps = e.engine.stat(&e.frame);
                fn row(s: &dcso3::perf::HistStat) -> serde_json::Value {
                    serde_json::json!({
                        "name":  s.name,
                        "unit":  s.unit,
                        "n":     s.n,
                        "mean":  s.mean,
                        "p50":   s.fifty,
                        "p90":   s.ninety,
                        "p99":   s.ninety_nine,
                        "p999":  s.ninety_nine_nine,
                    })
                }
                let engine_rows: Vec<serde_json::Value> = vec![
                    row(&ps.frame), row(&ps.timed_events), row(&ps.slow_timed),
                    row(&ps.dcs_events), row(&ps.dcs_hooks),
                    row(&ps.unit_positions), row(&ps.player_positions),
                    row(&ps.ewr_tracks), row(&ps.ewr_reports),
                    row(&ps.unit_culling), row(&ps.remark_objectives),
                    row(&ps.update_jtac_contacts), row(&ps.do_repairs),
                    row(&ps.spawn_queue), row(&ps.spawn), row(&ps.despawn),
                    row(&ps.advise_captured), row(&ps.advise_capturable),
                    row(&ps.jtac_target_positions), row(&ps.process_messages),
                    row(&ps.snapshot), row(&ps.logistics), row(&ps.logistics_distribute),
                    row(&ps.logistics_deliver), row(&ps.logistics_transfer),
                    row(&ps.logistics_sync_from), row(&ps.logistics_sync_to),
                    row(&ps.logistics_convoy), row(&ps.logistics_air_routes),
                    row(&ps.logistics_sea_routes), row(&ps.frontline),
                ];
                use dcso3::perf::HistStat as HS;
                let a = &e.api;
                let api_rows: Vec<serde_json::Value> = vec![
                    row(&HS::new(&a.get_position, "Unit.getPosition", false)),
                    row(&HS::new(&a.get_point, "Unit.getPoint", false)),
                    row(&HS::new(&a.get_velocity, "Unit.getVelocity", false)),
                    row(&HS::new(&a.in_air, "Unit.inAir", false)),
                    row(&HS::new(&a.get_ammo, "Unit.getAmmo", false)),
                    row(&HS::new(&a.add_group, "Coalition.addGroup", false)),
                    row(&HS::new(&a.add_static_object, "Coalition.addStaticObject", false)),
                    row(&HS::new(&a.unit_is_exist, "Unit.isExist", false)),
                    row(&HS::new(&a.unit_get_by_name, "Unit.getByName", false)),
                    row(&HS::new(&a.unit_get_desc, "Unit.getDesc", false)),
                    row(&HS::new(&a.land_is_visible, "Land.isVisible", false)),
                    row(&HS::new(&a.land_get_height, "Land.getHeight", false)),
                    row(&HS::new(&a.timer_schedule_function, "Timer.scheduleFunction", false)),
                    row(&HS::new(&a.timer_remove_function, "Timer.removeFunction", false)),
                    row(&HS::new(&a.timer_get_time, "Timer.getTime", false)),
                    row(&HS::new(&a.timer_get_abs_time, "Timer.getAbsTime", false)),
                    row(&HS::new(&a.timer_get_time0, "Timer.getTime0", false)),
                ];
                let logistics_items = ps.logistics_items;
                serde_json::json!({
                    "available": true,
                    "time": e.time.to_rfc3339(),
                    "engine": engine_rows,
                    "api": api_rows,
                    "logistics_items": logistics_items,
                    "hardware": hardware,
                })
            }
        };
        Ok(serde_json::to_string(&json)?)
    })?;
    Ok(json_response(data))
}

/// GET /api/admin/banned  — combined ban list (bfdb + last session cfg)
async fn api_admin_banned(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let web_bans = db.list_admin_bans()?;
        let cfg_bans = db.session_bans_from_cfg()?;
        let entries: Vec<_> = web_bans.iter().map(|(ucid, rec)| serde_json::json!({
            "ucid":      ucid.to_string(),
            "name":      rec.name,
            "banned_at": rec.banned_at.to_rfc3339(),
            "until":     rec.until.map(|t| t.to_rfc3339()),
            "reason":    rec.reason,
            "source":    "web",
        })).chain(cfg_bans.iter().filter_map(|(ucid, name, until)| {
            // Don't duplicate entries already in web_bans
            if web_bans.iter().any(|(u, _)| u == ucid) { return None }
            Some(serde_json::json!({
                "ucid":      ucid.to_string(),
                "name":      name,
                "banned_at": serde_json::Value::Null,
                "until":     until.map(|t| t.to_rfc3339()),
                "reason":    "",
                "source":    "engine",
            }))
        })).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

#[derive(serde::Deserialize)]
struct BanBody {
    ucid:   std::string::String,
    name:   std::string::String,
    #[serde(default)]
    reason: std::string::String,
    until:  Option<std::string::String>,   // ISO-8601 or null
}

/// POST /api/admin/ban  — add or update a ban record
async fn api_admin_ban(
    session_id: Option<Uuid>,
    body: BanBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let ucid = body.ucid.parse::<dcso3::net::Ucid>()
        .map_err(|e| Error(anyhow::anyhow!("invalid ucid: {e}")))?;
    let until = body.until.as_deref()
        .filter(|s| !s.is_empty())
        .map(|s| s.parse::<chrono::DateTime<chrono::Utc>>())
        .transpose()
        .map_err(|e| Error(anyhow::anyhow!("invalid until date: {e}")))?;
    let record = crate::db::BanRecord {
        name: body.name.clone(),
        banned_at: chrono::Utc::now(),
        until,
        reason: body.reason.clone(),
    };
    task::block_in_place(|| db.ban_player(ucid, record))?;
    log::info!("ADMIN: banned {} ({})", body.name, body.ucid);
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

#[derive(serde::Deserialize)]
struct UnbanBody2 {
    ucid: std::string::String,
}

/// POST /api/admin/unban  — remove a ban record
async fn api_admin_unban2(
    session_id: Option<Uuid>,
    body: UnbanBody2,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let ucid = body.ucid.parse::<dcso3::net::Ucid>()
        .map_err(|e| Error(anyhow::anyhow!("invalid ucid: {e}")))?;
    let removed = task::block_in_place(|| db.unban_player(&ucid))?;
    log::info!("ADMIN: unbanned {}", body.ucid);
    Ok(warp::reply::json(&serde_json::json!({"ok": true, "was_banned": removed})))
}

#[derive(serde::Deserialize)]
struct SpawnBody {
    airbase: std::string::String,
    #[serde(rename = "type")]
    item_type: std::string::String,
}

/// POST /api/commander/spawn  — spawn logistics from dashboard.
/// Resolves the airbase name to its live DCS position + owning side via
/// bflib's query-objective RPC, then calls its spawn-deployable RPC.
async fn api_commander_spawn(
    session_id: Option<Uuid>,
    body: SpawnBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    log::info!("COMMANDER: spawning {} at {}", body.item_type, body.airbase);

    use netidx::publisher::Value;

    let details_json = call_engine_rpc_str(
        &db, "query-objective", vec![("name", Value::from(body.airbase.clone()))],
    ).await?;
    let details: bfprotocols::api::ObjectiveDetails = serde_json::from_str(&details_json)
        .map_err(|e| Error(anyhow::anyhow!("bad objective details from engine: {e}")))?;
    let (x, z) = details.info.pos;
    let side = details.info.owner;

    let spawn_json = call_engine_rpc_str(&db, "spawn-deployable", vec![
        ("side", Value::from(side.to_str())),
        ("name", Value::from(body.item_type.clone())),
        ("x", Value::from(x)),
        ("z", Value::from(z)),
        ("heading", Value::from(0.0)),
    ]).await?;
    let result: serde_json::Value = serde_json::from_str(&spawn_json)
        .unwrap_or_else(|_| serde_json::json!({"success": true}));
    Ok(warp::reply::json(&result))
}

#[derive(serde::Deserialize)]
struct PriorityBody {
    objective: std::string::String,
    priority: bool,
}

/// POST /api/admin/priority  — mark/unmark an objective as commander's-intent
/// priority (display/coordination only, see bflib's SetObjectivePriority).
async fn api_admin_priority(
    session_id: Option<Uuid>,
    body: PriorityBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    use netidx::publisher::Value;
    call_engine_rpc_str(&db, "set-objective-priority", vec![
        ("objective", Value::from(body.objective.clone())),
        ("priority", Value::from(body.priority)),
    ]).await?;
    log::info!("ADMIN: set priority={} on objective {}", body.priority, body.objective);
    Ok(warp::reply::json(&serde_json::json!({"ok": true, "priority": body.priority})))
}

/// GET /api/admin/perf-history  — per-session perf data for charts (admin only)
async fn api_admin_perf_history(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let history = db.session_perf_history(50)?;
        fn row(s: &dcso3::perf::HistStat) -> serde_json::Value {
            serde_json::json!({ "name": s.name, "mean": s.mean, "p50": s.fifty, "p90": s.ninety, "p99": s.ninety_nine, "p999": s.ninety_nine_nine, "n": s.n, "unit": s.unit })
        }
        let entries: Vec<_> = history.iter().map(|e| {
            let ps = e.engine.stat(&e.frame);
            serde_json::json!({
                "time": e.time.to_rfc3339(),
                "frame":             { "mean": ps.frame.mean,             "p99": ps.frame.ninety_nine },
                "timed_events":      { "mean": ps.timed_events.mean,      "p99": ps.timed_events.ninety_nine },
                "slow_timed":        { "mean": ps.slow_timed.mean,        "p99": ps.slow_timed.ninety_nine },
                "dcs_events":        { "mean": ps.dcs_events.mean,        "p99": ps.dcs_events.ninety_nine },
                "spawn":             { "mean": ps.spawn.mean,             "p99": ps.spawn.ninety_nine },
                "despawn":           { "mean": ps.despawn.mean,           "p99": ps.despawn.ninety_nine },
                "logistics":         { "mean": ps.logistics.mean,         "p99": ps.logistics.ninety_nine },
                "logistics_deliver": { "mean": ps.logistics_deliver.mean, "p99": ps.logistics_deliver.ninety_nine },
                "frontline":         { "mean": ps.frontline.mean,         "p99": ps.frontline.ninety_nine },
                "unit_positions":    { "mean": ps.unit_positions.mean,    "p99": ps.unit_positions.ninety_nine },
                "ewr_tracks":        { "mean": ps.ewr_tracks.mean,        "p99": ps.ewr_tracks.ninety_nine },
                "snapshot":          { "mean": ps.snapshot.mean,          "p99": ps.snapshot.ninety_nine },
            })
        }).collect();
        // Also include per-metric rows for full detail view
        let full: Vec<_> = history.iter().map(|e| {
            let ps = e.engine.stat(&e.frame);
            let engine_rows: Vec<serde_json::Value> = vec![
                row(&ps.frame), row(&ps.timed_events), row(&ps.slow_timed),
                row(&ps.dcs_events), row(&ps.dcs_hooks),
                row(&ps.unit_positions), row(&ps.player_positions),
                row(&ps.spawn_queue), row(&ps.spawn), row(&ps.despawn),
                row(&ps.logistics), row(&ps.logistics_deliver), row(&ps.logistics_distribute),
                row(&ps.logistics_convoy), row(&ps.logistics_air_routes), row(&ps.logistics_sea_routes),
                row(&ps.frontline), row(&ps.snapshot), row(&ps.ewr_tracks), row(&ps.ewr_reports),
                row(&ps.jtac_target_positions), row(&ps.update_jtac_contacts), row(&ps.do_repairs),
            ];
            serde_json::json!({ "time": e.time.to_rfc3339(), "metrics": engine_rows })
        }).collect();
        Ok(serde_json::to_string(&serde_json::json!({ "timeline": entries, "sessions": full }))?)
    })?;
    Ok(json_response(data))
}

/// GET /api/trails  — return recent trail points for the active round
async fn api_trails(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => match rounds.first() {
                Some((_, rid, _)) => *rid,
                None => return Ok("[]".to_string()),
            },
        };
        let points = db.get_trail_points(rid)?;
        let entries: Vec<_> = points.iter().map(|p| serde_json::json!({
            "id":  p.unit_id,
            "lat": p.lat,
            "lon": p.lon,
            "alt": p.alt,
            "hdg": p.hdg,
            "ts":  p.ts,
        })).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

// ── Session cookie extraction helper ────────────────────────────────

fn extract_session_cookie() -> impl Filter<Extract = (Option<Uuid>,), Error = warp::Rejection> + Clone {
    warp::header::optional::<String>("cookie").map(|cookie: Option<String>| {
        cookie.and_then(|c| {
            c.split(';').find_map(|part| {
                let part = part.trim();
                part.strip_prefix("session=").and_then(|v| v.parse::<Uuid>().ok())
            })
        })
    })
}

fn with_auth_cfg(cfg: Option<AuthConfig>) -> impl Filter<Extract = (AuthConfig,), Error = warp::Rejection> + Clone {
    warp::any()
        .map(move || cfg.clone())
        .and_then(|cfg: Option<AuthConfig>| async move {
            cfg.ok_or_else(warp::reject::not_found)
        })
}

fn with_local_admin(cfg: Option<LocalAdminConfig>) -> impl Filter<Extract = (Option<LocalAdminConfig>,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || cfg.clone())
}

// ── Live unit types (from Export.lua UDP feed) ───────────────────────

/// A single unit record from the DCS Export.lua UDP feed.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct LiveUnit {
    id:   String,
    nm:   String,
    typ:  String,
    /// 1=Plane 2=Helo 3=Ground 4=Ship
    cat:  u8,
    /// 1=Red 2=Blue
    coa:  u8,
    lat:  f64,
    lon:  f64,
    alt:  f64,
    hdg:  f64,
    spd:  f64,
    /// Vertical speed m/s (positive = climbing), optional
    #[serde(default, skip_serializing_if = "Option::is_none")]
    vspd: Option<f64>,
    /// Occupying player's name, if this unit is player-flown (absent for AI)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pilot: Option<String>,
}

/// Bullseye reference point for one coalition.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Bullseye {
    /// 1=Red 2=Blue
    side: u8,
    lat:  f64,
    lon:  f64,
}

/// One UDP message from Export.lua.
#[allow(dead_code)]
#[derive(Debug, Deserialize)]
struct ExportMsg {
    /// DCS model time
    t:    f64,
    /// Batch sequence index within this tick
    seq:  u32,
    /// True if this is the last batch for this tick
    last: bool,
    /// Total unit count for this tick (across all batches)
    n:    u32,
    /// Units in this batch
    u:    Vec<LiveUnit>,
    /// Bullseye points (only present on the last batch)
    #[serde(default)]
    bull: Vec<Bullseye>,
}

/// Message broadcast to all WebSocket clients.
#[derive(Debug, Clone, Serialize)]
struct WsUnitsMsg<'a> {
    t:     f64,
    units: &'a [LiveUnit],
    bull:  &'a [Bullseye],
}

type LiveState = Arc<tokio::sync::RwLock<(f64, Vec<LiveUnit>, Vec<Bullseye>)>>;

/// WebSocket handler for `/ws/logs` — streams real-time bfdb log lines (admin only).
async fn ws_logs_handler(
    ws: warp::ws::Ws,
    session_id: Option<Uuid>,
    db: StatsDb,
    tx: broadcast::Sender<String>,
    history: LogHistory,
) -> impl Reply {
    let authed = match session_id {
        Some(id) => task::block_in_place(|| db.get_session(id))
            .ok()
            .flatten()
            .map(|s| s.is_admin)
            .unwrap_or(false),
        None => false,
    };
    if !authed {
        return ws
            .on_upgrade(|sock| async move { drop(sock) })
            .into_response();
    }
    ws.on_upgrade(move |socket| ws_logs(socket, tx.subscribe(), history))
        .into_response()
}

async fn ws_logs(
    ws: WebSocket,
    mut rx: broadcast::Receiver<String>,
    history: LogHistory,
) {
    let (mut sink, mut stream) = ws.split();
    // Collect history without holding the lock across await points
    let snapshot: Vec<String> = history.lock().unwrap().iter().cloned().collect();
    for line in snapshot {
        if sink.send(Message::text(line)).await.is_err() {
            return;
        }
    }
    loop {
        tokio::select! {
            msg = stream.next() => {
                match msg {
                    Some(Ok(m)) if m.is_close() => break,
                    None => break,
                    _ => {}
                }
            }
            msg = rx.recv() => {
                match msg {
                    Ok(json) => { if sink.send(Message::text(json)).await.is_err() { break; } }
                    Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    Err(_) => break,
                }
            }
        }
    }
}

/// WebSocket handler for `/ws/engine-logs` — streams the live bflib engine
/// log (from the running DCS mission, via netidx) rather than bfdb's own
/// process log. No-op stream if bfdb wasn't started with --base (admin only).
async fn ws_engine_logs_handler(
    ws: warp::ws::Ws,
    session_id: Option<Uuid>,
    db: StatsDb,
) -> impl Reply {
    let authed = match session_id {
        Some(id) => task::block_in_place(|| db.get_session(id))
            .ok()
            .flatten()
            .map(|s| s.is_admin)
            .unwrap_or(false),
        None => false,
    };
    if !authed {
        return ws
            .on_upgrade(|sock| async move { drop(sock) })
            .into_response();
    }
    let (rx, history) = db.engine_log_subscribe();
    ws.on_upgrade(move |socket| ws_engine_logs(socket, rx, history))
        .into_response()
}

async fn ws_engine_logs(
    ws: WebSocket,
    mut rx: broadcast::Receiver<String>,
    history: Vec<String>,
) {
    let (mut sink, mut stream) = ws.split();
    for line in history {
        if sink.send(Message::text(line)).await.is_err() {
            return;
        }
    }
    loop {
        tokio::select! {
            msg = stream.next() => {
                match msg {
                    Some(Ok(m)) if m.is_close() => break,
                    None => break,
                    _ => {}
                }
            }
            msg = rx.recv() => {
                match msg {
                    Ok(line) => { if sink.send(Message::text(line)).await.is_err() { break; } }
                    Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    Err(_) => break,
                }
            }
        }
    }
}

/// Background task: listens on UDP 42001, accumulates batches, and
/// broadcasts the full unit list to all WebSocket clients each tick.
/// Also samples unit positions every ~10s into the trail_points DB.
async fn udp_export_listener(state: LiveState, tx: broadcast::Sender<String>, db: StatsDb) {
    let sock = match tokio::net::UdpSocket::bind("0.0.0.0:42001").await {
        Ok(s) => s,
        Err(e) => {
            log::error!("Failed to bind UDP 42001 for DCS export: {e}");
            return;
        }
    };
    log::info!("DCS export listener on UDP 0.0.0.0:42001");

    let mut buf = vec![0u8; 65536];
    // Accumulate batches for one tick before broadcasting
    let mut pending:      Vec<LiveUnit> = Vec::new();
    let mut pending_bull: Vec<Bullseye> = Vec::new();
    let mut pending_t:    f64;

    loop {
        let len = match sock.recv(&mut buf).await {
            Ok(n) => n,
            Err(_) => continue,
        };
        // Strip trailing newline/whitespace
        let slice = buf[..len].iter().rposition(|&b| b > b' ')
            .map(|i| &buf[..=i]).unwrap_or(&buf[..len]);
        let msg: ExportMsg = match serde_json::from_slice(slice) {
            Ok(m) => m,
            Err(_) => continue,
        };

        pending_t = msg.t;
        pending.extend(msg.u);
        if !msg.bull.is_empty() {
            pending_bull = msg.bull;
        }

        if msg.last {
            // Full tick received — update state and broadcast
            let json = {
                let mut w = state.write().await;
                *w = (pending_t, std::mem::take(&mut pending), std::mem::take(&mut pending_bull));
                let broadcast_msg = WsUnitsMsg { t: w.0, units: &w.1, bull: &w.2 };
                serde_json::to_string(&broadcast_msg).unwrap_or_default()
            };
            let _ = tx.send(json);

            // Sample trail points every 10 seconds
            let now_secs = chrono::Utc::now().timestamp();
            if now_secs % 10 == 0 {
                let db2 = db.clone();
                let units_snapshot = {
                    let r = state.read().await;
                    r.1.clone()
                };
                task::spawn_blocking(move || {
                    let rounds = db2.latest_rounds().ok();
                    let rid = rounds.as_deref().and_then(|rs| {
                        rs.iter().find(|(_, _, r)| r.end.is_none()).map(|(_, rid, _)| *rid)
                    });
                    if let Some(rid) = rid {
                        for u in &units_snapshot {
                            let _ = db2.append_trail_point(
                                rid, &u.id, now_secs, u.lat, u.lon, u.alt, u.hdg,
                            );
                        }
                    }
                });
            }
        }
    }
}

/// WebSocket handler for `/ws/units` — streams live unit positions.
async fn ws_units_handler(ws: warp::ws::Ws, state: LiveState, tx: broadcast::Sender<String>) -> impl Reply {
    ws.on_upgrade(move |socket| ws_units(socket, state, tx.subscribe()))
}

async fn ws_units(ws: WebSocket, state: LiveState, mut rx: broadcast::Receiver<String>) {
    let (mut sink, mut stream) = ws.split();

    // Send current snapshot immediately on connect
    {
        let r = state.read().await;
        let msg = WsUnitsMsg { t: r.0, units: &r.1, bull: &r.2 };
        if let Ok(json) = serde_json::to_string(&msg) {
            if sink.send(Message::text(json)).await.is_err() {
                return;
            }
        }
    }

    // Stream updates, quit if client disconnects
    loop {
        tokio::select! {
            update = rx.recv() => {
                match update {
                    Ok(json) => {
                        if sink.send(Message::text(json)).await.is_err() { break; }
                    }
                    Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    Err(_) => break,
                }
            }
            msg = stream.next() => {
                match msg {
                    Some(Ok(m)) if m.is_close() => break,
                    None => break,
                    _ => {}
                }
            }
        }
    }
}

// ── SRS proxy ───────────────────────────────────────────────────────

async fn api_srs(srs_url: Arc<Option<String>>) -> Response {
    let empty = warp::reply::json(&serde_json::json!({"version": null, "clients": []}));
    let url = match srs_url.as_deref() {
        Some(u) => u.to_string(),
        None => return empty.into_response(),
    };
    // reqwest::get() uses a default client with no timeout -- if the local
    // SRS server is down or hanging (not just refusing the connection
    // outright), this would otherwise block the request indefinitely,
    // right through to Cloudflare's own ~100s edge timeout (a 524) instead
    // of falling back quickly like every other failure mode here already does.
    let client = reqwest::Client::builder()
        .timeout(std::time::Duration::from_secs(3))
        .build();
    let Ok(client) = client else { return empty.into_response() };
    match client.get(&url).send().await {
        Ok(resp) => match resp.json::<serde_json::Value>().await {
            Ok(json) => warp::reply::json(&json).into_response(),
            Err(_)   => empty.into_response(),
        },
        Err(_) => empty.into_response(),
    }
}

// ── Static file serving ─────────────────────────────────────────────

fn serve_site_asset(path: &str) -> Response {
    let path = if path.is_empty() { "index.html" } else { path };
    match SiteAssets::get(path) {
        Some(content) => {
            let mime = mime_guess::from_path(path).first_or_octet_stream();
            warp::http::Response::builder()
                .header("content-type", mime.as_ref())
                .body(content.data.into_owned())
                .unwrap()
                .into_response()
        }
        None => {
            // SPA fallback
            let content = SiteAssets::get("index.html").unwrap();
            warp::http::Response::builder()
                .header("content-type", "text/html")
                .body(content.data.into_owned())
                .unwrap()
                .into_response()
        }
    }
}

fn serve_asset(path: &str) -> Response {
    let path = if path.is_empty() { "index.html" } else { path };
    match Assets::get(path) {
        Some(content) => {
            let mime = mime_guess::from_path(path).first_or_octet_stream();
            warp::http::Response::builder()
                .header("content-type", mime.as_ref())
                .body(content.data.into_owned())
                .unwrap()
                .into_response()
        }
        None => {
            // SPA fallback: unknown paths get index.html for client-side routing
            let content = Assets::get("index.html").unwrap();
            warp::http::Response::builder()
                .header("content-type", "text/html")
                .body(content.data.into_owned())
                .unwrap()
                .into_response()
        }
    }
}

// ── Server setup ────────────────────────────────────────────────────

fn with_db(db: StatsDb) -> impl Filter<Extract = (StatsDb,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || db.clone())
}

fn with_bot_link_cfg(
    cfg: Arc<Option<BotLinkConfig>>,
) -> impl Filter<Extract = (Arc<Option<BotLinkConfig>>,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || cfg.clone())
}

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<()> {
    let args = Args::parse();

    // ── Broadcast logger: forwards to env_logger + WebSocket stream ───────
    let (log_tx, _) = broadcast::channel::<String>(512);
    let log_history: LogHistory = Arc::new(Mutex::new(VecDeque::new()));
    {
        let mut builder = env_logger::Builder::from_default_env();
        // Default to Info when RUST_LOG is not set so the log viewer has useful output
        if std::env::var("RUST_LOG").is_err() {
            builder.filter_level(log::LevelFilter::Info);
        }
        if let Some(path) = &args.log_file {
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            let file = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(path)
                .map_err(|e| anyhow::anyhow!("opening --log-file {path:?}: {e}"))?;
            builder.target(env_logger::Target::Pipe(Box::new(file)));
        }
        let env_log = builder.build();
        let max_level = env_log.filter();
        let logger = BroadcastLogger {
            inner:   env_log,
            tx:      log_tx.clone(),
            history: log_history.clone(),
        };
        log::set_boxed_logger(Box::new(logger)).expect("logger already set");
        log::set_max_level(max_level);
    }
    let db = match args.base {
        Some(base) => {
            let subscriber = SubscriberBuilder::new()
                .config(Config::load_default()?)
                .build()?;
            StatsDb::new(subscriber, args.db, base, args.stats_dir, args.include, args.exclude)?
        }
        None => {
            log::info!("Running in offline mode (no --base specified, Netidx disabled)");
            StatsDb::new_offline(args.db, args.stats_dir, args.stats_jsonl)?
        }
    };

    let auth_cfg: Option<AuthConfig> = match (
        args.discord_client_id,
        args.discord_client_secret,
        args.discord_redirect_uri,
        args.discord_guild_id,
        args.discord_admin_role_id,
    ) {
        (Some(id), Some(secret), Some(uri), Some(guild), Some(role)) => {
            log::info!("Discord OAuth enabled (guild={guild}, admin_role={role})");
            Some(AuthConfig {
                client_id:     id,
                client_secret: secret,
                redirect_uri:  uri,
                guild_id:      guild,
                admin_role_id: role,
            })
        }
        _ => {
            log::info!("Discord OAuth disabled (pass --discord-* flags to enable)");
            None
        }
    };

    let local_admin_cfg: Option<LocalAdminConfig> = match (args.admin_username, args.admin_password) {
        (Some(u), Some(p)) => {
            log::info!("Local admin login enabled (username={u})");
            Some(LocalAdminConfig { username: u, password: p })
        }
        _ => {
            log::info!("Local admin login disabled (pass --admin-username and --admin-password to enable)");
            None
        }
    };

    let bot_link_cfg: Arc<Option<BotLinkConfig>> = Arc::new(match (args.dcsserverbot_url, args.dcsserverbot_api_key) {
        (Some(base_url), Some(api_key)) => {
            // Tolerate a trailing slash on --dcsserverbot-url -- otherwise
            // e.g. "http://host:9876/stats/" + "/servers" produces a
            // double-slash path that most web frameworks 404 on, silently
            // breaking the integration rather than erroring loudly.
            let base_url = base_url.trim_end_matches('/').to_string();
            log::info!("Discord account linking via DCSServerBot enabled ({base_url})");
            Some(BotLinkConfig { base_url, api_key })
        }
        _ => {
            log::info!("Discord account linking disabled (pass --dcsserverbot-url and --dcsserverbot-api-key to enable)");
            None
        }
    });

    // ── Load campaign config JSON (served at /api/config) ────────────────
    let (campaign_json, srs_url_from_cfg): (Arc<String>, Option<String>) = match &args.config {
        Some(path) => {
            let raw = std::fs::read_to_string(path)
                .unwrap_or_else(|e| { log::warn!("Could not read --config {path:?}: {e}"); "{}".to_string() });
            match serde_json::from_str::<serde_json::Value>(&raw) {
                Ok(v) => {
                    log::info!("Campaign config loaded from {:?}", path);
                    let srs = v.get("srsUrl").and_then(|u| u.as_str()).filter(|s| !s.is_empty()).map(|s| s.to_string());
                    (Arc::new(raw), srs)
                }
                Err(_) => {
                    log::warn!("--config file is not valid JSON, using empty config");
                    (Arc::new("{}".to_string()), None)
                }
            }
        }
        None => {
            log::info!("No --config file specified; /api/config will return {{}}");
            (Arc::new("{}".to_string()), None)
        }
    };
    // CLI --srs-url takes precedence over campaign.json srsUrl
    let effective_srs_url = args.srs_url.clone().or(srs_url_from_cfg);
    if let Some(ref u) = effective_srs_url {
        log::info!("SRS proxy enabled → {u}");
    }

    let engine_config_path: Arc<Option<PathBuf>> = Arc::new(args.engine_config.clone());
    match &args.engine_config {
        Some(p) => log::info!("Engine config editor enabled → {p:?}"),
        None => log::info!("No --engine-config specified; the admin config editor is disabled"),
    }

    let cross_origin = !args.cors_origins.is_empty();
    let allowed_login_origins: Arc<Vec<std::string::String>> = Arc::new(args.cors_origins.clone());
    let cors = warp::cors()
        .allow_methods(&[Method::GET, Method::POST, Method::OPTIONS])
        .allow_headers(vec!["content-type"])
        .allow_credentials(true);
    let cors = if cross_origin {
        cors.allow_origins(args.cors_origins.iter().map(|s| s.as_str()))
    } else {
        cors.allow_any_origin()
    };
    if cross_origin {
        log::info!("Cross-origin mode enabled for: {:?} (cookies use SameSite=None; Secure — bfdb must be served over TLS)", args.cors_origins);
    }

    let rounds = warp::path!("api" / "rounds")
        .and(with_db(db.clone()))
        .then(api_rounds);

    let leaderboard = warp::path!("api" / "leaderboard")
        .and(with_db(db.clone()))
        .then(api_leaderboard);

    let all_pilots = warp::path!("api" / "pilots")
        .and(with_db(db.clone()))
        .then(api_all_pilots);

    let objectives = warp::path!("api" / "objectives")
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .then(|db, q: std::collections::HashMap<String, String>| {
            let round_id = q.get("round").and_then(|s| s.parse().ok());
            api_objectives(db, round_id)
        });

    let kills = warp::path!("api" / "kills")
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .then(|db, q: std::collections::HashMap<String, String>| {
            let round_id = q.get("round").and_then(|s| s.parse().ok());
            let limit = q.get("limit").and_then(|s| s.parse().ok());
            api_kills(db, round_id, limit)
        });

    let capture_events = warp::path!("api" / "capture-events")
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .then(|db, q: std::collections::HashMap<String, String>| {
            let round_id = q.get("round").and_then(|s| s.parse().ok());
            let limit = q.get("limit").and_then(|s| s.parse().ok());
            api_capture_events(db, round_id, limit)
        });

    let pilot = warp::path!("api" / "pilot" / String)
        .and(with_db(db.clone()))
        .then(api_pilot);

    let pilot_sorties = warp::path!("api" / "pilot" / String / "sorties")
        .and(with_db(db.clone()))
        .then(api_pilot_sorties);

    let pilot_breakdown = warp::path!("api" / "pilot" / String / "breakdown")
        .and(with_db(db.clone()))
        .then(api_pilot_breakdown);

    let pilot_kills_route = warp::path!("api" / "pilot" / String / "kills")
        .and(with_db(db.clone()))
        .then(api_pilot_kills);

    let stats = warp::path!("api" / "stats")
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_stats);

    let units = warp::path!("api" / "units")
        .and(with_db(db.clone()))
        .then(api_units);

    let online = warp::path!("api" / "online")
        .and(with_db(db.clone()))
        .then(api_online);

    let points = warp::path!("api" / "points")
        .and(with_db(db.clone()))
        .then(api_points);

    let captures = warp::path!("api" / "captures")
        .and(with_db(db.clone()))
        .then(api_captures);

    let aircraft_usage = warp::path!("api" / "aircraft-usage")
        .and(with_db(db.clone()))
        .then(api_aircraft_usage);

    // ── Live unit WebSocket ────────────────────────────────────────────
    let live_state: LiveState = Arc::new(tokio::sync::RwLock::new((0.0, Vec::new(), Vec::new())));
    let (live_tx, _) = broadcast::channel::<String>(64);

    // Spawn UDP listener
    tokio::spawn(udp_export_listener(live_state.clone(), live_tx.clone(), db.clone()));

    let ws_state = live_state.clone();
    let ws_tx    = live_tx.clone();
    let ws_units_route = warp::path!("ws" / "units")
        .and(warp::ws())
        .and(warp::any().map(move || ws_state.clone()))
        .and(warp::any().map(move || ws_tx.clone()))
        .then(ws_units_handler);

    // ── Log WebSocket (/ws/logs) — admin only ────────────────────────
    let log_tx_ws  = log_tx.clone();
    let log_hist_ws = log_history.clone();
    let ws_logs_route = warp::path!("ws" / "logs")
        .and(warp::ws())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(warp::any().map(move || log_tx_ws.clone()))
        .and(warp::any().map(move || log_hist_ws.clone()))
        .then(ws_logs_handler);

    // ── Engine log WebSocket (/ws/engine-logs) — live bflib logs, admin only ──
    let ws_engine_logs_route = warp::path!("ws" / "engine-logs")
        .and(warp::ws())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(ws_engine_logs_handler);

    // ── Auth routes ──────────────────────────────────────────────────
    let auth_login = warp::path!("api" / "auth" / "login")
        .and(warp::query::<LoginQuery>())
        .and(with_auth_cfg(auth_cfg.clone()))
        .and(with_db(db.clone()))
        .and(warp::any().map(move || allowed_login_origins.clone()))
        .then(api_auth_login);

    let auth_callback = warp::path!("api" / "auth" / "callback")
        .and(warp::query::<CallbackQuery>())
        .and(with_auth_cfg(auth_cfg.clone()))
        .and(with_db(db.clone()))
        .and(warp::any().map(move || cross_origin))
        .then(api_auth_callback);

    let auth_me = warp::path!("api" / "auth" / "me")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_auth_me);

    let auth_logout = warp::path!("api" / "auth" / "logout")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(warp::any().map(move || cross_origin))
        .then(api_auth_logout);

    let auth_local_login = warp::path!("api" / "auth" / "local-login")
        .and(warp::post())
        .and(warp::body::json::<LocalLoginBody>())
        .and(with_local_admin(local_admin_cfg.clone()))
        .and(with_db(db.clone()))
        .and(warp::any().map(move || cross_origin))
        .then(api_auth_local_login);

    // Tells the frontend whether local admin login is available
    let local_admin_enabled = local_admin_cfg.is_some();
    let auth_local_enabled = warp::path!("api" / "auth" / "local-enabled")
        .map(move || json_response(format!(r#"{{"enabled":{}}}"#, local_admin_enabled)))
        .boxed();

    let admin_sessions = warp::path!("api" / "admin" / "sessions")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_sessions);

    let admin_reset = warp::path!("api" / "admin" / "reset")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_reset);

    let admin_perf = warp::path!("api" / "admin" / "perf")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_perf);

    let admin_perf_history = warp::path!("api" / "admin" / "perf-history")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_perf_history);

    let admin_banned = warp::path!("api" / "admin" / "banned")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_banned);

    let admin_ban_route = warp::path!("api" / "admin" / "ban")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::json::<BanBody>())
        .and(with_db(db.clone()))
        .then(api_admin_ban);

    let admin_unban_route = warp::path!("api" / "admin" / "unban")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::json::<UnbanBody2>())
        .and(with_db(db.clone()))
        .then(api_admin_unban2);

    let commander_spawn_route = warp::path!("api" / "commander" / "spawn")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::json::<SpawnBody>())
        .and(with_db(db.clone()))
        .then(api_commander_spawn);

    let admin_priority_route = warp::path!("api" / "admin" / "priority")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::json::<PriorityBody>())
        .and(with_db(db.clone()))
        .then(api_admin_priority);

    let cockpit_ewr_report_route = warp::path!("api" / "cockpit" / "ewr" / "report")
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_cockpit_ewr_report);

    let cockpit_ewr_intel_route = warp::path!("api" / "cockpit" / "ewr" / "intel")
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_cockpit_ewr_intel);

    let cockpit_ewr_toggle_route = warp::path!("api" / "cockpit" / "ewr" / "toggle")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_cockpit_ewr_toggle);

    let cockpit_ewr_units_route = warp::path!("api" / "cockpit" / "ewr" / "units")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::json::<EwrUnitsBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_cockpit_ewr_units);

    let cockpit_carp_solve_route = warp::path!("api" / "cockpit" / "carp" / "solve")
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_cockpit_carp_solve);

    let cockpit_carp_solve_latlon_route = warp::path!("api" / "cockpit" / "carp" / "solve-latlon")
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_cockpit_carp_solve_latlon);

    let cockpit_cargo_spawn_route = warp::path!("api" / "cockpit" / "cargo" / "spawn")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::json::<CargoSpawnBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_cockpit_cargo_spawn);

    let trails = warp::path!("api" / "trails")
        .and(with_db(db.clone()))
        .then(api_trails);

    let config_route = warp::path!("api" / "config")
        .and(warp::any().map(move || campaign_json.clone()))
        .then(api_config);

    let srs_url_arc: Arc<Option<String>> = Arc::new(effective_srs_url);
    let srs_route = warp::path!("api" / "srs")
        .and(warp::any().map(move || srs_url_arc.clone()))
        .then(api_srs);

    let admin_cfg_get_route = warp::path!("api" / "admin" / "cfg")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(warp::any().map({
            let p = engine_config_path.clone();
            move || p.clone()
        }))
        .then(api_admin_cfg_get);

    let admin_cfg_schema_route = warp::path!("api" / "admin" / "cfg" / "schema")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_cfg_schema);

    let admin_cfg_post_route = warp::path!("api" / "admin" / "cfg")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::json::<SaveCfgBody>())
        .and(with_db(db.clone()))
        .and(warp::any().map(move || engine_config_path.clone()))
        .then(api_admin_cfg_post);

    let wiki_list_route = warp::path!("api" / "wiki" / "pages")
        .and(with_db(db.clone()))
        .then(api_wiki_list);

    // Not the `path!` macro here (it implicitly requires end-of-path) --
    // the slug itself is multi-segment (e.g. "gameplay/objectives"), so
    // these need `path::tail()` to actually capture it, same pattern as
    // `site_files`/`static_files` below.
    let wiki_get_route = warp::path("api")
        .and(warp::path("wiki"))
        .and(warp::path("pages"))
        .and(warp::path::tail())
        .and(with_db(db.clone()))
        .then(api_wiki_get);

    let wiki_save_route = warp::path("api")
        .and(warp::path("wiki"))
        .and(warp::path("pages"))
        .and(warp::path::tail())
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::json::<SaveWikiPageBody>())
        .and(with_db(db.clone()))
        .then(api_wiki_save);

    let wiki_delete_route = warp::path!("api" / "wiki" / "delete")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::json::<DeleteWikiPageBody>())
        .and(with_db(db.clone()))
        .then(api_wiki_delete);

    let wiki_upload_image_route = warp::path!("api" / "wiki" / "images")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::header::<std::string::String>("content-type"))
        .and(warp::body::content_length_limit(MAX_WIKI_IMAGE_BYTES))
        .and(warp::body::bytes())
        .and(with_db(db.clone()))
        .then(api_wiki_upload_image);

    let wiki_get_image_route = warp::path!("api" / "wiki" / "images" / Uuid)
        .and(with_db(db.clone()))
        .then(api_wiki_get_image);

    // /site/* → embedded bfsite SPA
    let site_files = warp::path("site")
        .and(warp::path::tail())
        .map(|tail: warp::path::Tail| serve_site_asset(tail.as_str()));

    // Catch-all → embedded bfweb SPA
    let static_files = warp::get()
        .and(warp::path::tail())
        .map(|tail: warp::path::Tail| serve_asset(tail.as_str()));

    // Box sub-chains to avoid warp filter type overflow
    let api_routes = rounds
        .or(leaderboard)
        .or(objectives)
        .or(kills)
        .or(capture_events)
        .or(pilot_sorties)
        .or(pilot_breakdown)
        .or(pilot_kills_route)
        .or(pilot)
        .or(stats)
        .or(units)
        .or(online)
        .or(points)
        .or(captures)
        .or(aircraft_usage)
        .or(trails)
        .or(all_pilots)
        .or(config_route)
        .or(srs_route)
        .or(cockpit_ewr_report_route)
        .or(cockpit_ewr_intel_route)
        .or(cockpit_carp_solve_route)
        .or(cockpit_carp_solve_latlon_route)
        .or(wiki_list_route)
        .or(wiki_get_route)
        .or(wiki_get_image_route)
        .boxed();

    let auth_routes = auth_login
        .or(auth_callback)
        .or(auth_me)
        .or(auth_logout)
        .or(auth_local_enabled)
        .or(admin_sessions)
        .or(admin_perf)
        .or(admin_perf_history)
        .or(admin_banned)
        .or(admin_cfg_get_route)
        .or(admin_cfg_schema_route)
        .boxed();

    let routes = warp::get()
        .and(
            api_routes
                .or(auth_routes)
                .or(ws_units_route)
                .or(ws_logs_route)
                .or(ws_engine_logs_route)
                .or(site_files)
                .or(static_files),
        )
        .or(auth_local_login)
        .or(admin_reset)
        .or(admin_ban_route)
        .or(admin_unban_route)
        .or(admin_cfg_post_route)
        .or(commander_spawn_route)
        .or(admin_priority_route)
        .or(cockpit_ewr_toggle_route.or(cockpit_ewr_units_route).or(cockpit_cargo_spawn_route).boxed())
        .boxed()
        .or(wiki_save_route.or(wiki_delete_route).or(wiki_upload_image_route).boxed())
        .with(cors);

    log::info!("API server listening on http://{}", args.listen_address);

    // Optional separate site server
    if let Some(site_addr) = args.site_address {
        let site_cors = warp::cors()
            .allow_any_origin()
            .allow_methods(&[Method::GET])
            .build();
        let site_only = warp::get()
            .and(warp::path::tail())
            .map(|tail: warp::path::Tail| serve_site_asset(tail.as_str()))
            .with(site_cors);
        log::info!("Site server listening on http://{}", site_addr);
        tokio::spawn(warp::serve(site_only).run(site_addr));
    }

    match (&args.cert, &args.key) {
        (_, None) | (None, _) => warp::serve(routes).run(args.listen_address).await,
        (Some(cert), Some(key)) => {
            warp::serve(routes)
                .tls()
                .cert_path(cert)
                .key_path(key)
                .run(args.listen_address)
                .await
        }
    }
    Ok(())
}
