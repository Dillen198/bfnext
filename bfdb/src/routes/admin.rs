//! Admin actions: resets, bot control, the OPS proxy, the config editor,
//! bans and the audit log.

use crate::*;

/// GET /api/admin/audit?limit=&before= -- the admin audit log, newest first.
/// `before` is an RFC 3339 time (the `at` of the last row of the previous page).
pub(crate) async fn api_admin_audit(
    session_id: Option<Uuid>,
    q: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let limit = q.get("limit").and_then(|s| s.parse::<usize>().ok()).unwrap_or(200).clamp(1, 1000);
    let before = match q.get("before") {
        Some(b) => Some(
            chrono::DateTime::parse_from_rfc3339(b)
                .map_err(|_| websec::bad_request("before: not an RFC 3339 time"))?
                .with_timezone(&chrono::Utc),
        ),
        None => None,
    };
    let rows = task::block_in_place(|| db.audit_list(limit, before))?;
    Ok(warp::reply::json(&rows))
}

/// POST /api/admin/reset  — wipe all campaign data, keep auth & Discord links
pub(crate) async fn api_admin_reset(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_admin(session_id, db.clone()).await?;
    // On a range-only box the range is the default, and with one instance a
    // reset takes the whole-database path: every pilot and round gone.
    if inst.cfg.is_range() {
        return Err(Error(websec::bad_request(format!(
            "{} is a training range: it has no campaign to reset",
            inst.id
        ))));
    }
    task::block_in_place(|| db.reset_campaign_data_for(&inst))?;
    audit(&db, &who, "reset campaign", None, Some(&inst.id));
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

/// POST /api/admin/reset-lives-all  — give every player their lives back.
/// Proxies bflib's reset-lives-all RPC; requires a live engine connection.
pub(crate) async fn api_admin_reset_lives_all(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_role(session_id, db.clone(), admin::Role::OPS).await?;
    let reply = call_engine_rpc_str(&db, &inst, "reset-lives-all", vec![]).await?;
    audit(&db, &who, "reset all lives", Some(&reply), Some(&inst.id));
    Ok(warp::reply::json(
        &serde_json::json!({"ok": true, "message": reply}),
    ))
}

#[derive(serde::Deserialize)]
pub(crate) struct SideSwitchBody {
    /// Name (case-insensitive, as the engine's `-admin switch` takes it),
    /// in-game player id, or UCID.
    pub(crate) player: std::string::String,
    pub(crate) side: std::string::String,
}

/// POST /api/admin/side-switch  — force a player onto a side on one server
/// (`?server=` picks the instance). Proxies bflib's side-switch RPC, which
/// also puts a slotted player back to spectators; needs the engine running.
pub(crate) async fn api_admin_side_switch(
    session_id: Option<Uuid>,
    body: SideSwitchBody,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use netidx::publisher::Value;
    let who = require_role(session_id, db.clone(), admin::Role::MODERATION).await?;
    // The engine parses sides as DCS spells them: lowercase.
    let side = match body.side.to_ascii_lowercase().as_str() {
        "blue" => "blue",
        "red" => "red",
        other => {
            return Err(Error(websec::bad_request(format!(
                "side must be Blue or Red, not {other:?}"
            ))))
        }
    };
    let player = body.player.trim();
    if player.is_empty() {
        return Err(Error(websec::bad_request("player is required")));
    }
    let reply = call_engine_rpc_str(
        &db,
        &inst,
        "side-switch",
        vec![("player", Value::from(player.to_string())), ("side", Value::from(side.to_string()))],
    )
    .await?;
    audit(&db, &who, "side switch", Some(&format!("{player} -> {side}: {reply}")), Some(&inst.id));
    Ok(warp::reply::json(&serde_json::json!({"ok": true, "message": reply})))
}

/// POST /api/admin/merge-rounds  — collapse every round id in the stats DB
/// into one, repairing the "one campaign shows as dozens of rounds" damage
/// from the old fork-on-restart bug. `?dry_run=true` (the default) only
/// reports what it would do; `?dry_run=false` actually does it.
pub(crate) async fn api_admin_merge_rounds(
    query: std::collections::HashMap<String, String>,
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_admin(session_id, db.clone()).await?;
    let dry_run = query
        .get("dry_run")
        .map(|v| v != "false" && v != "0")
        .unwrap_or(true);
    let msg = task::block_in_place(|| db.merge_all_rounds(dry_run))?;
    audit(&db, &who, if dry_run { "merge rounds (dry run)" } else { "merge rounds" }, Some(&msg), None);
    Ok(warp::reply::json(&serde_json::json!({ "ok": true, "dry_run": dry_run, "message": msg })))
}

/// POST /api/admin/rebuild-stats  — queue an in-process rebuild: the JSONL
/// reader wipes every stats-derived tree and re-ingests stats.jsonl from the
/// top on its next tick. Corrects counters inflated by the old whole-file
/// re-reads without taking bfdb down. Auth, Discord links, bans, wiki and
/// recon intel are preserved.
pub(crate) async fn api_admin_rebuild_stats(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_admin(session_id, db.clone()).await?;
    task::block_in_place(|| db.request_jsonl_rebuild())?;
    audit(&db, &who, "rebuild stats", None, None);
    Ok(warp::reply::json(&serde_json::json!({
        "ok": true,
        "message": "rebuild queued -- bfdb will wipe derived stats and re-ingest the log over the next few minutes"
    })))
}

/// GET /api/admin/bot/status  — current DCS server name/status via
/// DCSServerBot, so the admin panel can show state before offering actions
/// (admin only)
pub(crate) async fn api_admin_bot_status(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    require_role(session_id, db.clone(), admin::Role::OPS).await?;
    let info = fetch_bot_server_info(&bot_cfg, &inst).await;
    Ok(warp::reply::json(&serde_json::json!({
        "configured": bot_cfg.is_some(),
        "name": info.as_ref().map(|s| s.name.clone()),
        "status": info.and_then(|s| s.status),
    })))
}

/// Shared body for the six DCSServerBot instance/mission control endpoints
/// below -- each just supplies its own admin-role check and RestAPI path.
pub(crate) async fn api_admin_bot_action(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    path: &'static str,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_role(session_id, db.clone(), admin::Role::OPS).await?;
    // Admin-only, and the admin needs to know why the bot said no.
    let body = bot_instance_action(&bot_cfg, &inst, path)
        .await
        .map_err(|e| Error(websec::unavailable(format!("{e:#}"))))?;
    audit(&db, &who, "DCSServerBot", Some(path), Some(&inst.id));
    Ok(warp::reply::json(&body))
}

/// POST /api/admin/bot/start  — DCSServerBot: start the DCS server instance
pub(crate) async fn api_admin_bot_start(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    api_admin_bot_action(session_id, db, bot_cfg, inst, "instance/start").await
}

/// POST /api/admin/bot/stop  — DCSServerBot: stop the DCS server instance
pub(crate) async fn api_admin_bot_stop(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    api_admin_bot_action(session_id, db, bot_cfg, inst, "instance/stop").await
}

/// POST /api/admin/bot/restart  — DCSServerBot: restart the DCS server instance
pub(crate) async fn api_admin_bot_restart(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    api_admin_bot_action(session_id, db, bot_cfg, inst, "instance/restart").await
}

/// POST /api/admin/bot/mission/restart  — DCSServerBot: restart the current mission
pub(crate) async fn api_admin_bot_mission_restart(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    api_admin_bot_action(session_id, db, bot_cfg, inst, "instance/mission/restart").await
}

/// POST /api/admin/bot/mission/pause  — DCSServerBot: pause the current mission
pub(crate) async fn api_admin_bot_mission_pause(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    api_admin_bot_action(session_id, db, bot_cfg, inst, "instance/mission/pause").await
}

/// POST /api/admin/bot/mission/unpause  — DCSServerBot: unpause the current mission
pub(crate) async fn api_admin_bot_mission_unpause(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    api_admin_bot_action(session_id, db, bot_cfg, inst, "instance/mission/unpause").await
}

/// GET|POST /api/admin/ops/<tail>  — the dashboard OPS page (admin only).
///
/// Forwarded to the FowlEngine plugin's OPS API on DCSServerBot's WebService
/// (`{--dcsserverbot-url}/fowlengine/ops/<tail>`, see
/// DCSServerBot/plugins/fowlengine/opsapi.py) with the same X-API-Key the
/// RestAPI calls above use. The bot owns the processes and files the page
/// manages -- DCS, bfdb itself, staging dirs, fowlengine.yaml -- and stays up
/// while bfdb restarts, so it answers, not bfdb. The key never reaches the
/// browser; the dashboard admin login gates it here.
pub(crate) async fn api_admin_ops_proxy(
    method: warp::http::Method,
    tail: warp::path::Tail,
    query: String,
    session_id: Option<Uuid>,
    body: bytes::Bytes,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<warp::reply::Response, Error> {
    use warp::http::StatusCode;
    let who = require_role(session_id, db.clone(), admin::Role::OPS).await?;
    let path = tail.as_str();
    let json_err = |status: StatusCode, msg: String| {
        warp::reply::with_status(warp::reply::json(&serde_json::json!({ "error": msg })), status)
            .into_response()
    };
    if path.is_empty()
        || path.contains("..")
        || !path.chars().all(|c| c.is_ascii_alphanumeric() || matches!(c, '/' | '_' | '-'))
    {
        return Ok(json_err(StatusCode::BAD_REQUEST, format!("bad ops path {path:?}")));
    }
    let Some(cfg) = bot_cfg.as_ref() else {
        return Ok(json_err(
            StatusCode::SERVICE_UNAVAILABLE,
            "DCSServerBot is not configured (--dcsserverbot-url/--dcsserverbot-api-key) -- the OPS \
             page needs the bot"
                .into(),
        ));
    };
    let mut url = format!("{}/fowlengine/ops/{path}", cfg.base_url);
    if !query.is_empty() {
        url.push('?');
        url.push_str(&query);
    }
    // One client for every OPS call: its connection pool to the bot is
    // reused instead of rebuilt (TLS and all) per request.
    static OPS_HTTP: std::sync::LazyLock<std::result::Result<reqwest::Client, std::string::String>> =
        std::sync::LazyLock::new(|| {
            reqwest::Client::builder()
                // an update check downloads the release; give it room
                .timeout(std::time::Duration::from_secs(300))
                .build()
                .map_err(|e| e.to_string())
        });
    let http = OPS_HTTP.as_ref().map_err(|e| anyhow::anyhow!("http client: {e}"))?;
    let req = if method == warp::http::Method::POST {
        http.post(&url)
            .header("content-type", "application/json")
            .body(if body.is_empty() { bytes::Bytes::from_static(b"{}") } else { body })
    } else {
        http.get(&url)
    };
    let resp = match req.header("X-API-Key", &cfg.ops_key).send().await {
        Ok(r) => r,
        Err(e) => {
            return Ok(json_err(
                StatusCode::BAD_GATEWAY,
                format!("the bot's WebService is not answering ({e}) -- is DCSServerBot running?"),
            ))
        }
    };
    let status = resp.status().as_u16();
    let ctype = resp
        .headers()
        .get("content-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("application/json")
        .to_string();
    let data = resp
        .bytes()
        .await
        .map_err(|e| anyhow::anyhow!("reading the bot's answer: {e}"))?;
    if status == 404 && !ctype.starts_with("application/json") {
        return Ok(json_err(
            StatusCode::BAD_GATEWAY,
            "the bot has no OPS API -- update the FowlEngine plugin, and check that \
             --dcsserverbot-url carries the RestAPI prefix"
                .into(),
        ));
    }
    if method == warp::http::Method::POST {
        audit(&db, &who, "OPS", Some(&format!("{path} -> {status}")), None);
    }
    let mut out = warp::reply::Response::new(data.to_vec().into());
    *out.status_mut() = StatusCode::from_u16(status).unwrap_or(StatusCode::BAD_GATEWAY);
    out.headers_mut().insert(
        "content-type",
        warp::http::HeaderValue::from_str(&ctype)
            .unwrap_or(warp::http::HeaderValue::from_static("application/json")),
    );
    out.headers_mut()
        .insert("cache-control", warp::http::HeaderValue::from_static("no-store"));
    Ok(out)
}

/// How many `<cfg>.bak.<time>` copies a config save leaves beside the file.
pub(crate) const CFG_BACKUPS_KEPT: usize = 20;

/// Delete all but the newest `keep` `<cfg>.bak.<YYYYmmddHHMMSS>` copies of
/// `cfg`. Every save made one and nothing removed them.
pub(crate) fn prune_cfg_backups(cfg: &std::path::Path, keep: usize) {
    let (Some(dir), Some(name)) = (cfg.parent(), cfg.file_name().and_then(|n| n.to_str())) else { return };
    let prefix = format!("{name}.bak.");
    let Ok(rd) = std::fs::read_dir(if dir.as_os_str().is_empty() { std::path::Path::new(".") } else { dir }) else {
        return;
    };
    let mut baks: Vec<PathBuf> = rd
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| {
            p.file_name()
                .and_then(|n| n.to_str())
                .and_then(|n| n.strip_prefix(&prefix))
                .map_or(false, |ts| ts.len() == 14 && ts.bytes().all(|b| b.is_ascii_digit()))
        })
        .collect();
    // the timestamp sorts as text
    baks.sort();
    if baks.len() > keep {
        for old in &baks[..baks.len() - keep] {
            if let Err(e) = std::fs::remove_file(old) {
                log::warn!("could not remove old config backup {}: {e}", old.display());
            }
        }
    }
}

/// GET /api/admin/cfg  — read the current campaign engine config JSON (admin only)
pub(crate) async fn api_admin_cfg_get(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let path: Arc<Option<PathBuf>> = Arc::new(inst.cfg.engine_config.clone());
    require_role(session_id, db.clone(), admin::Role::OPS).await?;
    let path = path
        .as_ref()
        .clone()
        .ok_or_else(|| websec::not_found("engine config not configured (missing --engine-config)"))?;
    let data = task::block_in_place(|| -> Result<String> {
        std::fs::read_to_string(&path)
            .map_err(|e| anyhow::anyhow!("reading {:?}: {e}", path))
    })?;
    Ok(json_response(data))
}

/// GET /api/admin/cfg/schema  — JSON Schema generated from the real Cfg type
/// bflib actually parses, so the editor UI can never drift out of sync with
/// the engine (admin only)
pub(crate) async fn api_admin_cfg_schema(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_role(session_id, db.clone(), admin::Role::OPS).await?;
    let schema = schemars::schema_for!(bfprotocols::cfg::Cfg);
    Ok(warp::reply::json(&schema))
}

#[derive(Deserialize)]
pub(crate) struct SaveCfgBody {
    pub(crate) cfg: serde_json::Value,
}

/// POST /api/admin/cfg  — validate and save a new campaign engine config
/// (admin only). Validation deserializes the body into the real Cfg type
/// bflib loads, so malformed edits are rejected here instead of silently
/// breaking the server at next restart. The previous file is backed up
/// alongside the new one before being overwritten. Takes effect on the next
/// mission/server restart — bflib only reads Cfg once at startup.
pub(crate) async fn api_admin_cfg_post(
    session_id: Option<Uuid>,
    body: SaveCfgBody,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let path: Arc<Option<PathBuf>> = Arc::new(inst.cfg.engine_config.clone());
    let who = require_role(session_id, db.clone(), admin::Role::OPS).await?;
    let path = path
        .as_ref()
        .clone()
        .ok_or_else(|| websec::not_found("engine config not configured (missing --engine-config)"))?;
    let _validated: bfprotocols::cfg::Cfg = serde_json::from_value(body.cfg.clone())
        .map_err(|e| websec::bad_request(format!("config is invalid: {e}")))?;
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
            prune_cfg_backups(&path, CFG_BACKUPS_KEPT);
        }
        std::fs::write(&path, pretty)
            .map_err(|e| anyhow::anyhow!("writing {:?}: {e}", path))?;
        Ok(())
    })?;
    audit(&db, &who, "save engine config", path.to_str(), Some(&inst.id));
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

/// GET /api/admin/pilot-sides — every pilot's campaign coalition on this
/// instance, as `[{ucid, name, side}]`.
///
/// This is what makes the Discord coalition roles honest: the bot mirrors the
/// side the *engine* registered (first slot pick, or a `-switch`), so nobody
/// can hand themselves the other faction's briefing channel by picking a role.
/// The engine stays the only thing that decides a side — the bot never writes
/// one back.
///
/// Admin-gated, because the full roster of who is flying for whom is exactly
/// the kind of thing the fog of war is meant to hide from the other side.
pub(crate) async fn api_admin_pilot_sides(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    require_role(session_id, db.clone(), admin::Role::MODERATION).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let mut pilots: Vec<serde_json::Value> = db
            .all_pilot_sides(&inst.id)?
            .into_iter()
            .map(|(ucid, side)| {
                serde_json::json!({
                    "ucid": ucid.to_string(),
                    "name": db.pilot_name(&ucid),
                    // Debug, not Display: the rest of the API emits "Blue"/"Red".
                    "side": format!("{side:?}"),
                })
            })
            .collect();
        pilots.sort_by(|a, b| a["ucid"].as_str().cmp(&b["ucid"].as_str()));
        Ok(serde_json::to_string(&serde_json::json!({
            "instance": inst.id.to_string(),
            "round": db.active_round_id(&inst.id)?.map(|r| r.to_string()),
            "pilots": pilots,
        }))?)
    })?;
    Ok(json_response(data))
}

/// GET /api/admin/banned  — combined ban list (bfdb + last session cfg)
pub(crate) async fn api_admin_banned(
    session_id: Option<Uuid>,
    db: StatsDb,
    q: Q,
) -> std::result::Result<impl warp::Reply, Error> {
    require_role(session_id, db.clone(), admin::Role::MODERATION).await?;
    let data = task::block_in_place(|| -> Result<(String, usize)> {
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
        page_json(entries, &q, ADMIN_LIST_CAP)
    })?;
    Ok(paged_response(data))
}

/// GET /api/admin/engine-errors  — recent ERROR/WARN lines from the live
/// bflib engine log (admin only). Backs the dashboard's error feed -- the
/// same underlying lines the Discord fowlengine plugin's engine log relay
/// posts as alerts, but visible without Discord and independent of whether
/// the bot is currently connected.
pub(crate) async fn api_admin_engine_errors(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    require_role(session_id, db.clone(), admin::Role::OPS).await?;
    let lines = db.engine_error_snapshot(&inst);
    Ok(json_response(serde_json::to_string(&lines).map_err(|e| Error(e.into()))?))
}

#[derive(serde::Deserialize)]
pub(crate) struct BanBody {
    pub(crate) ucid:   std::string::String,
    pub(crate) name:   std::string::String,
    #[serde(default)]
    pub(crate) reason: std::string::String,
    pub(crate) until:  Option<std::string::String>,   // ISO-8601 or null
}

/// POST /api/admin/ban  — add or update a ban record
pub(crate) async fn api_admin_ban(
    session_id: Option<Uuid>,
    body: BanBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_role(session_id, db.clone(), admin::Role::MODERATION).await?;
    let ucid = body.ucid.parse::<dcso3::net::Ucid>()
        .map_err(|e| Error(websec::bad_request(format!("invalid ucid: {e}"))))?;
    let until = body.until.as_deref()
        .filter(|s| !s.is_empty())
        .map(|s| s.parse::<chrono::DateTime<chrono::Utc>>())
        .transpose()
        .map_err(|e| Error(websec::bad_request(format!("invalid until date: {e}"))))?;
    let record = crate::db::BanRecord {
        name: body.name.clone(),
        banned_at: chrono::Utc::now(),
        until,
        reason: body.reason.clone(),
    };
    task::block_in_place(|| db.ban_player(ucid, record))?;
    audit(&db, &who, "ban", Some(&format!("{} ({})", body.name, body.ucid)), None);
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

#[derive(serde::Deserialize)]
pub(crate) struct UnbanBody {
    pub(crate) ucid: std::string::String,
}

/// POST /api/admin/unban  — remove a ban record
pub(crate) async fn api_admin_unban(
    session_id: Option<Uuid>,
    body: UnbanBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_role(session_id, db.clone(), admin::Role::MODERATION).await?;
    let ucid = body.ucid.parse::<dcso3::net::Ucid>()
        .map_err(|e| Error(websec::bad_request(format!("invalid ucid: {e}"))))?;
    let removed = task::block_in_place(|| db.unban_player(&ucid))?;
    audit(&db, &who, "unban", Some(&body.ucid), None);
    Ok(warp::reply::json(&serde_json::json!({"ok": true, "was_banned": removed})))
}
