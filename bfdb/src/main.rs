use anyhow::Result;
use bfprotocols::cfg::UnitTag;
use clap::Parser;
use db::{SessionData, StatsDb};
use futures::{SinkExt, StreamExt};
use netidx::{config::Config, path::Path as NetidxPath, subscriber::SubscriberBuilder};
use regex::Regex;
use rust_embed::RustEmbed;
use serde_derive::{Deserialize, Serialize};
use std::{net::SocketAddr, path::PathBuf, sync::Arc};
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
}

#[derive(Debug, Clone)]
struct AuthConfig {
    client_id:     String,
    client_secret: String,
    redirect_uri:  String,
    guild_id:      String,
    admin_role_id: String,
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

fn json_response(data: String) -> impl warp::Reply {
    reply::with_header(data, "content-type", "application/json")
}

// ── API handlers ────────────────────────────────────────────────────

async fn api_rounds(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
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
        let pilots = db.pilot_leaderboard()?;
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

async fn api_objectives(
    db: StatsDb,
    round_id: Option<u64>,
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
                }))
            })
            .collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
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
                let killer = dead
                    .shots
                    .iter()
                    .find(|s| s.hit)
                    .map(|s| {
                        serde_json::json!({
                            "ucid": s.shooter.ucid().map(|u| u.to_string()),
                            "side": format!("{:?}", s.shooter.side()),
                            "weapon": s.weapon_name.as_ref().map(|w| w.to_string()),
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

async fn api_stats(db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds()?;
        let pilots = db.pilot_leaderboard()?;
        let active_round = rounds.iter().find(|(_, _, r)| r.end.is_none());
        let obj_count = if let Some((_, rid, _)) = active_round {
            db.objectives_for_round(*rid)?.len()
        } else {
            0
        };
        let total_kills: u32 = pilots.iter().map(|(_, _, a)| a.air_kills + a.ground_kills).sum();
        Ok(serde_json::to_string(&serde_json::json!({
            "total_pilots": pilots.len(),
            "total_rounds": rounds.len(),
            "active_round": active_round.map(|(s, rid, r)| serde_json::json!({
                "id": rid.0,
                "scenario": s.to_string(),
                "start": r.start.to_rfc3339(),
            })),
            "objective_count": obj_count,
            "total_kills": total_kills,
        }))?)
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
                let heading = (vel.x.atan2(vel.z).to_degrees() + 360.0) % 360.0;
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

/// GET /api/auth/login  — redirect to Discord OAuth
async fn api_auth_login(cfg: AuthConfig, db: StatsDb) -> std::result::Result<impl warp::Reply, Error> {
    let state = Uuid::new_v4();
    task::block_in_place(|| db.store_oauth_state(state))?;
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
async fn api_auth_callback(
    q: CallbackQuery,
    cfg: AuthConfig,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    // Validate state
    let state_uuid = q.state.parse::<Uuid>().map_err(|e| anyhow::anyhow!("bad state: {e}"))?;
    let valid = task::block_in_place(|| db.validate_oauth_state(state_uuid))?;
    if !valid {
        return Err(anyhow::anyhow!("invalid or expired OAuth state").into());
    }

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
        "session={}; Path=/; HttpOnly; SameSite=Lax; Max-Age=604800",
        session_id
    );
    Ok(warp::http::Response::builder()
        .status(302)
        .header("location", "/")
        .header("set-cookie", cookie)
        .body("")
        .unwrap())
}

/// GET /api/auth/me  — return current user (reads session cookie)
async fn api_auth_me(
    session_id: Option<Uuid>,
    db: StatsDb,
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
    let ucid = task::block_in_place(|| db.get_ucid_for_discord(&s.discord_id))?
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
) -> std::result::Result<impl warp::Reply, Error> {
    if let Some(id) = session_id {
        task::block_in_place(|| db.delete_session(id))?;
    }
    Ok(warp::http::Response::builder()
        .status(200)
        .header("set-cookie", "session=; Path=/; HttpOnly; Max-Age=0")
        .body("")
        .unwrap())
}

#[derive(Deserialize)]
struct LinkBody { ucid: String }

/// POST /api/auth/link  — link Discord account to DCS UCID
async fn api_auth_link(
    session_id: Option<Uuid>,
    body: LinkBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let Some(id) = session_id else {
        return Err(anyhow::anyhow!("not logged in").into());
    };
    let session = task::block_in_place(|| db.get_session(id))?
        .ok_or_else(|| anyhow::anyhow!("session expired"))?;
    let ucid: dcso3::net::Ucid = body.ucid.parse().map_err(|e| anyhow::anyhow!("{e:?}"))?;
    // Verify pilot exists
    match task::block_in_place(|| db.pilot_detail(&ucid))? {
        None => return Err(anyhow::anyhow!("pilot not found").into()),
        Some(_) => {}
    }
    task::block_in_place(|| db.link_discord_ucid(&session.discord_id, &ucid))?;
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
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

/// GET /api/admin/links  — list all discord↔ucid mappings
async fn api_admin_links(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let links = db.list_links()?;
        let entries: Vec<_> = links.iter().map(|(discord_id, ucid)| {
            serde_json::json!({ "discord_id": discord_id, "ucid": ucid.to_string() })
        }).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
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

#[derive(Deserialize)]
struct UnlinkBody { discord_id: String }

/// POST /api/admin/unlink  — remove a discord↔ucid mapping
async fn api_admin_unlink(
    session_id: Option<Uuid>,
    body: UnlinkBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    task::block_in_place(|| db.unlink_discord(&body.discord_id))?;
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
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

/// Background task: listens on UDP 42001, accumulates batches, and
/// broadcasts the full unit list to all WebSocket clients each tick.
/// Also samples unit positions every ~10s into the trail_points DB.
async fn udp_export_listener(state: LiveState, tx: broadcast::Sender<String>, db: StatsDb) {
    let sock = match tokio::net::UdpSocket::bind("0.0.0.0:42001").await {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to bind UDP 42001 for DCS export: {e}");
            return;
        }
    };
    eprintln!("DCS export listener on UDP 0.0.0.0:42001");

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

// ── Static file serving ─────────────────────────────────────────────

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

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<()> {
    env_logger::init();
    let args = Args::parse();
    let db = match args.base {
        Some(base) => {
            let subscriber = SubscriberBuilder::new()
                .config(Config::load_default()?)
                .build()?;
            StatsDb::new(subscriber, args.db, base, args.stats_dir, args.include, args.exclude)?
        }
        None => {
            eprintln!("Running in offline mode (no --base specified, Netidx disabled)");
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
            eprintln!("Discord OAuth enabled (guild={guild}, admin_role={role})");
            Some(AuthConfig {
                client_id:     id,
                client_secret: secret,
                redirect_uri:  uri,
                guild_id:      guild,
                admin_role_id: role,
            })
        }
        _ => {
            eprintln!("Discord OAuth disabled (pass --discord-* flags to enable)");
            None
        }
    };

    let cors = warp::cors()
        .allow_any_origin()
        .allow_methods(&[Method::GET, Method::POST, Method::OPTIONS])
        .allow_headers(vec!["content-type"])
        .allow_credentials(true);

    let rounds = warp::path!("api" / "rounds")
        .and(with_db(db.clone()))
        .then(api_rounds);

    let leaderboard = warp::path!("api" / "leaderboard")
        .and(with_db(db.clone()))
        .then(api_leaderboard);

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

    let pilot = warp::path!("api" / "pilot" / String)
        .and(with_db(db.clone()))
        .then(api_pilot);

    let stats = warp::path!("api" / "stats")
        .and(with_db(db.clone()))
        .then(api_stats);

    let units = warp::path!("api" / "units")
        .and(with_db(db.clone()))
        .then(api_units);

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

    // ── Auth routes ──────────────────────────────────────────────────
    let auth_login = warp::path!("api" / "auth" / "login")
        .and(with_auth_cfg(auth_cfg.clone()))
        .and(with_db(db.clone()))
        .then(api_auth_login);

    let auth_callback = warp::path!("api" / "auth" / "callback")
        .and(warp::query::<CallbackQuery>())
        .and(with_auth_cfg(auth_cfg.clone()))
        .and(with_db(db.clone()))
        .then(api_auth_callback);

    let auth_me = warp::path!("api" / "auth" / "me")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_auth_me);

    let auth_logout = warp::path!("api" / "auth" / "logout")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_auth_logout);

    let auth_link = warp::path!("api" / "auth" / "link")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::json::<LinkBody>())
        .and(with_db(db.clone()))
        .then(api_auth_link);

    let admin_links = warp::path!("api" / "admin" / "links")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_links);

    let admin_sessions = warp::path!("api" / "admin" / "sessions")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_sessions);

    let admin_unlink = warp::path!("api" / "admin" / "unlink")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::json::<UnlinkBody>())
        .and(with_db(db.clone()))
        .then(api_admin_unlink);

    let trails = warp::path!("api" / "trails")
        .and(with_db(db.clone()))
        .then(api_trails);

    let static_files = warp::get()
        .and(warp::path::tail())
        .map(|tail: warp::path::Tail| serve_asset(tail.as_str()));

    let routes = warp::get()
        .and(
            rounds
                .or(leaderboard)
                .or(objectives)
                .or(kills)
                .or(pilot)
                .or(stats)
                .or(units)
                .or(trails)
                .or(auth_login)
                .or(auth_callback)
                .or(auth_me)
                .or(auth_logout)
                .or(admin_links)
                .or(admin_sessions)
                .or(ws_units_route)
                .or(static_files),
        )
        .or(auth_link)
        .or(admin_unlink)
        .with(cors);

    eprintln!("API server listening on http://{}", args.listen_address);

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
