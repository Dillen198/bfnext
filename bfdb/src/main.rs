use anyhow::Result;
use bfprotocols::cfg::UnitTag;
use clap::Parser;
use db::StatsDb;
use netidx::{config::Config, path::Path as NetidxPath, subscriber::SubscriberBuilder};
use regex::Regex;
use rust_embed::RustEmbed;
use std::{net::SocketAddr, path::PathBuf};
use tokio::task;
use warp::{
    http::Method,
    reply::{self, Reply, Response},
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
                // Anti-cheat: hide carrier group exact positions
                if obj.kind.is_carrier_group() {
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

    let cors = warp::cors()
        .allow_any_origin()
        .allow_methods(&[Method::GET, Method::OPTIONS])
        .allow_headers(vec!["content-type"]);

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
                .or(static_files),
        )
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
