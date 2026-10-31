//! The in-cockpit plugin's API.

use crate::*;

// ── Cockpit UI handlers ──────────────────────────────────────────────
// Identifies the calling player and forwards to a player-scoped bflib RPC
// (see bflib/src/bg/rpcs.rs "Cockpit UI API"). Two ways in:
//  - `?playerid=<id>&ckey=<key>` from bfcockpit/Scripts/Hooks/bfcockpit.lua,
//    the in-DCS Hooks-script overlay. <id> is net.get_my_player_id(), which is
//    a guessable counter, so it is only honoured together with the key that
//    overlay registered with the engine over chat (see bflib/src/cockpit.rs,
//    "Overlay keys"): bflib's "resolve-player-id" RPC checks both. A bare
//    playerid used to be enough, which let anyone drive anyone's F10 menu.
//  - a browser session cookie linked to a Discord account, for testing the
//    standalone /cockpit page outside DCS -- that player acts only as the ucid
//    their Discord link names, whatever playerid they send.

/// How long a verified (instance, playerid, key) stays trusted without asking
/// the engine again. Short: a key dies with its connection.
pub(crate) const COCKPIT_ID_CACHE: std::time::Duration =
    std::time::Duration::from_secs(30);

pub(crate) type CockpitIdKey = (InstanceId, i64, std::string::String);

pub(crate) static COCKPIT_IDS: std::sync::LazyLock<
    Mutex<
        std::collections::HashMap<CockpitIdKey, (dcso3::net::Ucid, std::time::Instant)>,
    >,
> = std::sync::LazyLock::new(|| Mutex::new(std::collections::HashMap::new()));

/// Wrong cockpit keys per client IP. Twenty in ten minutes is far past any
/// honest reload loop and makes guessing a 40-character key pointless.
pub(crate) static COCKPIT_FAILS: std::sync::LazyLock<
    websec::RateLimiter<std::net::IpAddr>,
> = std::sync::LazyLock::new(|| {
    websec::RateLimiter::new(
        20,
        std::time::Duration::from_secs(600),
        std::time::Duration::from_secs(600),
    )
});

pub(crate) async fn resolve_by_player_id(
    id: i64,
    key: &str,
    db: &StatsDb,
    inst: &InstanceState,
    ip: std::net::IpAddr,
) -> std::result::Result<dcso3::net::Ucid, Error> {
    use netidx::publisher::Value;
    let cache_key: CockpitIdKey = (inst.id.clone(), id, key.to_string());
    {
        let mut g = COCKPIT_IDS.lock().unwrap_or_else(|e| e.into_inner());
        g.retain(|_, (_, at)| at.elapsed() < COCKPIT_ID_CACHE);
        if let Some((ucid, _)) = g.get(&cache_key) {
            return Ok(*ucid);
        }
    }
    if let Some(wait) = COCKPIT_FAILS.blocked(&ip) {
        return Err(websec::too_many(format!(
            "too many rejected cockpit keys from your address -- try again in {}s",
            wait.as_secs().max(1)
        ))
        .into());
    }
    let s = match call_engine_rpc_str(
        db,
        inst,
        "resolve-player-id",
        vec![("id", Value::from(id)), ("key", Value::from(key.to_string()))],
    )
    .await
    {
        Ok(s) => s,
        Err(Error(e)) => {
            // The engine refusing the key (or the id) is a failed guess; the
            // engine being down is not the caller's fault.
            let refused = e.downcast_ref::<websec::ApiError>().is_some_and(|a| {
                a.status == warp::http::StatusCode::UNPROCESSABLE_ENTITY
            });
            if refused {
                COCKPIT_FAILS.hit(&ip);
                return Err(websec::unauthorized(format!(
                    "{e} -- reopen the overlay (and update it from the dashboard if it is older than 1.1.0)"
                ))
                .into());
            }
            return Err(Error(e));
        }
    };
    let ucid = s
        .parse::<dcso3::net::Ucid>()
        .map_err(|e| anyhow::anyhow!("bad ucid from engine: {e:?}"))?;
    COCKPIT_IDS
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .insert(cache_key, (ucid, std::time::Instant::now()));
    Ok(ucid)
}

/// The cockpit routes' query map, with the overlay key taken from the
/// `X-Cockpit-Key` header when it is sent there -- a header stays out of
/// proxy and browser history logs, where `?ckey=` lands. `?ckey=` still
/// works: every overlay released so far sends it.
pub(crate) fn cockpit_query() -> impl Filter<
    Extract = (std::collections::HashMap<std::string::String, std::string::String>,),
    Error = warp::Rejection,
> + Clone {
    warp::query::<std::collections::HashMap<std::string::String, std::string::String>>()
        .and(warp::header::optional::<std::string::String>("x-cockpit-key"))
        .map(
            |mut q: std::collections::HashMap<
                std::string::String,
                std::string::String,
            >,
             key: Option<std::string::String>| {
                if let Some(k) =
                    key.map(|k| k.trim().to_string()).filter(|k| !k.is_empty())
                {
                    q.insert("ckey".into(), k);
                }
                q
            },
        )
}

/// Overlay versions seen on cockpit requests (`?plugin=`): requests and the
/// last time, per version. Shown in `/api/metrics`; a version's first
/// appearance is logged.
pub(crate) static COCKPIT_PLUGINS: std::sync::LazyLock<
    Mutex<
        std::collections::HashMap<
            std::string::String,
            (u64, chrono::DateTime<chrono::Utc>),
        >,
    >,
> = std::sync::LazyLock::new(|| Mutex::new(std::collections::HashMap::new()));

pub(crate) fn note_cockpit_plugin(
    query: &std::collections::HashMap<std::string::String, std::string::String>,
) {
    let v = match query.get("plugin").map(|s| s.trim()) {
        Some(v)
            if !v.is_empty()
                && v.len() <= 32
                && v.chars().all(|c| {
                    c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '_')
                }) =>
        {
            v
        }
        Some(_) => "invalid",
        None => "none",
    };
    let mut g = COCKPIT_PLUGINS.lock().unwrap_or_else(|e| e.into_inner());
    if g.len() >= 64 && !g.contains_key(v) {
        return;
    }
    let e = g.entry(v.to_string()).or_insert_with(|| {
        log::info!("cockpit: first request from overlay version {v}");
        (0, chrono::Utc::now())
    });
    e.0 += 1;
    e.1 = chrono::Utc::now();
}

pub(crate) async fn require_linked_player(
    query: &std::collections::HashMap<std::string::String, std::string::String>,
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: &Arc<Option<BotLinkConfig>>,
    inst: &InstanceState,
    ip: std::net::IpAddr,
) -> std::result::Result<dcso3::net::Ucid, Error> {
    note_cockpit_plugin(query);
    let player_id = query.get("playerid").and_then(|s| s.parse::<i64>().ok());
    let key = query.get("ckey").map(|s| s.trim()).filter(|s| !s.is_empty());
    if let (Some(id), Some(key)) = (player_id, key) {
        return resolve_by_player_id(id, key, &db, inst, ip).await;
    }
    // No key: the only other identity is a Discord-linked browser session,
    // which acts as its own linked ucid.
    let Some(sid) = session_id else {
        return Err(websec::unauthorized(if player_id.is_some() {
            "this cockpit overlay is out of date (no key) -- download the current version from the dashboard"
        } else {
            "not logged in"
        })
        .into());
    };
    let session = task::block_in_place(|| db.get_session(sid))?
        .ok_or_else(|| websec::unauthorized("session expired"))?;
    let ucid = resolve_ucid_via_bot(bot_cfg, &session.discord_id).await
        .ok_or_else(|| websec::forbidden("account not linked -- type -linkme <token> in DCS chat (get the token with /linkme in Discord)"))?;
    Ok(ucid)
}

pub(crate) async fn api_cockpit_ewr_report(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    let friendly = query.get("friendly").map(|s| s == "true").unwrap_or(false);
    use netidx::publisher::Value;
    let report = call_engine_rpc_str(
        &db,
        &inst,
        "ewr-report",
        vec![
            ("ucid", Value::from(ucid.to_string())),
            ("friendly", Value::from(friendly)),
        ],
    )
    .await?;
    Ok(warp::reply::json(&serde_json::json!({ "report": report })))
}

pub(crate) async fn api_cockpit_ewr_toggle(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    use netidx::publisher::Value;
    let state = call_engine_rpc_str(
        &db,
        &inst,
        "ewr-toggle",
        vec![("ucid", Value::from(ucid.to_string()))],
    )
    .await?;
    Ok(warp::reply::json(&serde_json::json!({ "state": state })))
}

#[derive(serde::Deserialize)]
pub(crate) struct EwrUnitsBody {
    pub(crate) imperial: bool,
}

pub(crate) async fn api_cockpit_ewr_units(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: EwrUnitsBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    use netidx::publisher::Value;
    let units = call_engine_rpc_str(
        &db,
        &inst,
        "ewr-set-units",
        vec![
            ("ucid", Value::from(ucid.to_string())),
            ("imperial", Value::from(body.imperial)),
        ],
    )
    .await?;
    Ok(warp::reply::json(&serde_json::json!({ "units": units })))
}

pub(crate) async fn api_cockpit_ewr_intel(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    use netidx::publisher::Value;
    let report = call_engine_rpc_str(
        &db,
        &inst,
        "ewr-ground-intel",
        vec![("ucid", Value::from(ucid.to_string()))],
    )
    .await?;
    Ok(warp::reply::json(&serde_json::json!({ "report": report })))
}

/// GET /api/cockpit/carp/solve?key=<mark text>&altft=<drop altitude AGL, ft>
/// Solves CARP INIT 1/5, 3/5 and 4/5 auto-fillable fields for the PI marked
/// on the F10 map with the given text -- see bflib/src/carp.rs.
pub(crate) async fn api_cockpit_carp_solve(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    let key =
        query.get("key").cloned().ok_or_else(|| websec::bad_request("missing key"))?;
    let alt_ft: f64 = query
        .get("altft")
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| websec::bad_request("missing or invalid altft"))?;
    use netidx::publisher::Value;
    let json = call_engine_rpc_str(
        &db,
        &inst,
        "carp-solve",
        vec![
            ("ucid", Value::from(ucid.to_string())),
            ("mark_key", Value::from(key)),
            ("drop_altitude_agl_ft", Value::from(alt_ft)),
        ],
    )
    .await?;
    let solution: serde_json::Value = serde_json::from_str(&json)
        .map_err(|e| anyhow::anyhow!("bad carp solution from engine: {e:?}"))?;
    Ok(warp::reply::json(&solution))
}

/// GET /api/cockpit/carp/solve-latlon?lat=<>&lon=<>&altft=<drop altitude AGL, ft>
/// Same as api_cockpit_carp_solve, but for a PI given directly as lat/long
/// (e.g. a click on the dashboard's map) instead of an F10 mark's text.
pub(crate) async fn api_cockpit_carp_solve_latlon(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    let lat: f64 = query
        .get("lat")
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| websec::bad_request("missing or invalid lat"))?;
    let lon: f64 = query
        .get("lon")
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| websec::bad_request("missing or invalid lon"))?;
    let alt_ft: f64 = query
        .get("altft")
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| websec::bad_request("missing or invalid altft"))?;
    use netidx::publisher::Value;
    let json = call_engine_rpc_str(
        &db,
        &inst,
        "carp-solve-latlon",
        vec![
            ("ucid", Value::from(ucid.to_string())),
            ("lat", Value::from(lat)),
            ("lon", Value::from(lon)),
            ("drop_altitude_agl_ft", Value::from(alt_ft)),
        ],
    )
    .await?;
    let solution: serde_json::Value = serde_json::from_str(&json)
        .map_err(|e| anyhow::anyhow!("bad carp solution from engine: {e:?}"))?;
    Ok(warp::reply::json(&solution))
}

#[derive(serde::Deserialize)]
pub(crate) struct CargoSpawnBody {
    pub(crate) crate_name: std::string::String,
    pub(crate) qty: u32,
    pub(crate) c130: bool,
}

/// POST /api/cockpit/cargo/spawn?playerid=<> -- queue qty copies of a named
/// crate for the calling player's current slot. See bflib's
/// AdminCommand::CockpitSpawnCrate / menu/cargo.rs's spawn_crates_for_ucid,
/// the same logic the F10 "Spawn N Crates" items call.
pub(crate) async fn api_cockpit_cargo_spawn(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: CargoSpawnBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    if body.qty < 1 {
        return Err(websec::bad_request("qty must be at least 1").into());
    }
    use netidx::publisher::Value;
    let msg = call_engine_rpc_str(
        &db,
        &inst,
        "cargo-spawn-crate",
        vec![
            ("ucid", Value::from(ucid.to_string())),
            ("crate_name", Value::from(body.crate_name)),
            ("qty", Value::from(body.qty as i64)),
            ("c130", Value::from(body.c130)),
        ],
    )
    .await?;
    Ok(warp::reply::json(&serde_json::json!({ "message": msg })))
}

// ── Cockpit overlay plugin distribution ──────────────────────────────
// The overlay is a client-side DCS plugin each player installs (see
// `bfcockpit/`). bfdb ships the copy it was built with, so "which version is
// current" has exactly one answer and the dashboard can hand players the file
// directly instead of pointing them at a repo path.

/// The plugin script this bfdb was built with.
pub(crate) const COCKPIT_PLUGIN_LUA: &str =
    include_str!("../../../bfcockpit/Scripts/Hooks/bfcockpit.lua");

/// Read the version out of the script itself rather than keeping a second
/// copy of it here -- two places to bump is one place to forget.
pub(crate) fn cockpit_plugin_version() -> &'static str {
    COCKPIT_PLUGIN_LUA
        .lines()
        .find_map(|l| {
            let l = l.trim();
            let rest = l.strip_prefix("local BFCOCKPIT_VERSION")?;
            let rest = rest.trim_start().strip_prefix('=')?;
            rest.trim().trim_matches('"').split('"').next()
        })
        .unwrap_or("unknown")
}

/// `--cockpit-plugin-version`: advertise this overlay version as is, with no
/// release check. Set once in `main`.
pub(crate) static COCKPIT_VERSION_OVERRIDE: std::sync::OnceLock<std::string::String> =
    std::sync::OnceLock::new();

/// Whether the installer for the bundled overlay version is published, as
/// last checked, and when.
pub(crate) static COCKPIT_RELEASE: std::sync::LazyLock<
    Mutex<(Option<(std::time::Instant, bool)>, bool)>,
> = std::sync::LazyLock::new(|| Mutex::new((None, false)));

/// Re-check a confirmed release this often, a missing one sooner.
pub(crate) const COCKPIT_RELEASE_RECHECK_OK: std::time::Duration =
    std::time::Duration::from_secs(6 * 3600);
pub(crate) const COCKPIT_RELEASE_RECHECK_MISSING: std::time::Duration =
    std::time::Duration::from_secs(600);

/// The overlay version to advertise. The one bfdb was built with is only
/// offered once its `cockpit-v<version>` release asset is confirmed on
/// GitHub: an overlay told about a version whose installer is not out yet
/// tried to update itself and got a 404. Until then the answer is
/// "unknown", which no overlay or dashboard takes for an update. The check
/// runs in the background; a request never waits on GitHub.
pub(crate) fn advertised_cockpit_version() -> std::string::String {
    if let Some(v) = COCKPIT_VERSION_OVERRIDE.get() {
        return v.clone();
    }
    let built = cockpit_plugin_version();
    let (known, start) = {
        let mut g = COCKPIT_RELEASE.lock().unwrap_or_else(|e| e.into_inner());
        let fresh = g.0.is_some_and(|(at, ok)| {
            at.elapsed()
                < if ok {
                    COCKPIT_RELEASE_RECHECK_OK
                } else {
                    COCKPIT_RELEASE_RECHECK_MISSING
                }
        });
        let start = !fresh && !g.1;
        if start {
            g.1 = true;
        }
        (g.0.map(|(_, ok)| ok), start)
    };
    if start && built != "unknown" {
        let url =
            format!("{COCKPIT_RELEASES}/cockpit-v{built}/{COCKPIT_INSTALLER_ASSET}");
        tokio::spawn(async move {
            let ok = http_client()
                .head(&url)
                .send()
                .await
                .is_ok_and(|r| r.status().is_success());
            let mut g = COCKPIT_RELEASE.lock().unwrap_or_else(|e| e.into_inner());
            if !ok && g.0.is_none_or(|(_, was)| was) {
                log::warn!("cockpit: no published installer at {url} yet -- advertising overlay version \"unknown\" until there is");
            }
            *g = (Some((std::time::Instant::now(), ok)), false);
        });
    }
    match known {
        Some(true) => built.to_string(),
        _ => "unknown".to_string(),
    }
}

/// GET /api/cockpit/plugin/version — what the current overlay version is
/// (see `advertised_cockpit_version`); `built` is the copy this bfdb ships.
///
/// Open on purpose: a player needs this before they are identified, and it
/// reveals nothing.
pub(crate) async fn api_cockpit_plugin_version(
    query: std::collections::HashMap<std::string::String, std::string::String>,
) -> std::result::Result<impl warp::Reply, Error> {
    note_cockpit_plugin(&query);
    Ok(warp::reply::json(&serde_json::json!({
        "version": advertised_cockpit_version(),
        "built": cockpit_plugin_version(),
    })))
}

/// Where the installer for each plugin version is published: a GitHub release
/// tagged `cockpit-v<version>` (outside the `engine-` / `manager-v` prefixes,
/// so neither updater mistakes it for one of theirs).
pub(crate) const COCKPIT_RELEASES: &str =
    "https://github.com/Dillen198/bfnext/releases/download";
pub(crate) const COCKPIT_INSTALLER_ASSET: &str = "VectorStrike-Cockpit-Install.exe";

/// GET /api/cockpit/plugin/installer — the installer for the version this bfdb
/// ships, so the wiki and dashboard have one link that is never out of date.
pub(crate) async fn api_cockpit_plugin_installer(
) -> std::result::Result<impl warp::Reply, Error> {
    // The newest published one; the bundled version's until that is known.
    let advertised = advertised_cockpit_version();
    let version = if advertised == "unknown" {
        cockpit_plugin_version().to_string()
    } else {
        advertised
    };
    let url = format!("{COCKPIT_RELEASES}/cockpit-v{version}/{COCKPIT_INSTALLER_ASSET}");
    Ok(warp::redirect::temporary(url.parse::<warp::http::Uri>().unwrap()))
}

/// GET /api/cockpit/plugin/download — the plugin script itself.
///
/// Since 2.0 the script needs bfcockpit_net.dll beside it, which only the
/// installer ships; a script fetched on its own says so on its panel.
pub(crate) async fn api_cockpit_plugin_download(
) -> std::result::Result<impl warp::Reply, Error> {
    Ok(warp::reply::with_header(
        warp::reply::with_header(
            COCKPIT_PLUGIN_LUA,
            "content-type",
            "text/plain; charset=utf-8",
        ),
        "content-disposition",
        "attachment; filename=\"bfcockpit.lua\"",
    ))
}

/// GET /api/cockpit/context — who the caller is, what they are flying, where.
///
/// The overlay polls this so it can shape itself to the player: a helo gets the
/// helo crate list, a Herc gets CARP, someone sitting in spectators gets told
/// to get in a jet. The engine already knows all of it, so the player is never
/// asked to enter their aircraft, coalition or position.
pub(crate) async fn api_cockpit_context(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    use netidx::publisher::Value;
    let json = call_engine_rpc_str(
        &db,
        &inst,
        "cockpit-context",
        vec![("ucid", Value::from(ucid.to_string()))],
    )
    .await?;
    Ok(json_response(json))
}

/// GET /api/cockpit/menu — the caller's entire F10 menu tree.
///
/// Mirrored out of the live menu rather than re-implemented (see
/// `bflib/src/cockpit.rs`), so every menu the engine has -- Actions, Cargo,
/// Troops, JTAC, Objectives, Info, Recon, and anything added later -- shows up
/// here automatically and cannot drift out of step with the real one.
pub(crate) async fn api_cockpit_menu(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    use netidx::publisher::Value;
    let json = call_engine_rpc_str(
        &db,
        &inst,
        "cockpit-menu",
        vec![("ucid", Value::from(ucid.to_string()))],
    )
    .await?;
    Ok(json_response(json))
}

/// GET /api/cockpit/messages?since=<seq> — the text messages the caller was
/// sent (F10 answers, crate counts, warnings), newer than `since`.
///
/// F10 handlers reply in DCS message boxes, not return values, so this is how
/// the panel shows what a click actually said. `since` is the `next` value
/// from the previous call; 0 (or absent) gets the recent backlog.
pub(crate) async fn api_cockpit_messages(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    let since =
        query.get("since").and_then(|s| s.parse::<i64>().ok()).unwrap_or(0).max(0);
    use netidx::publisher::Value;
    let json = call_engine_rpc_str(
        &db,
        &inst,
        "cockpit-messages",
        vec![("ucid", Value::from(ucid.to_string())), ("since", Value::from(since))],
    )
    .await?;
    Ok(json_response(json))
}

/// GET /api/cockpit/cargo — what the caller's slot can spawn, how many of each
/// crate every deployable needs, what is on board and what is lying nearby.
/// Each crate names the F10 path that spawns it, for /menu/invoke.
pub(crate) async fn api_cockpit_cargo(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    use netidx::publisher::Value;
    let json = call_engine_rpc_str(
        &db,
        &inst,
        "cockpit-cargo",
        vec![("ucid", Value::from(ucid.to_string()))],
    )
    .await?;
    Ok(json_response(json))
}

#[derive(serde::Deserialize)]
pub(crate) struct MenuInvokeBody {
    /// Path segments exactly as `/api/cockpit/menu` reported them.
    pub(crate) path: Vec<std::string::String>,
}

/// POST /api/cockpit/menu/invoke — click one F10 menu item for the caller.
///
/// Fires the same handler DCS fires, so the result is identical to the player
/// having walked the menu themselves.
pub(crate) async fn api_cockpit_menu_invoke(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: MenuInvokeBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = require_linked_player(&query, session_id, db.clone(), &bot_cfg, &inst, ip)
        .await?;
    if body.path.is_empty() {
        return Err(websec::bad_request("path must not be empty").into());
    }
    use netidx::publisher::Value;
    let msg = call_engine_rpc_str(
        &db,
        &inst,
        "cockpit-menu-invoke",
        vec![
            ("ucid", Value::from(ucid.to_string())),
            (
                "path",
                Value::from(
                    serde_json::to_string(&body.path).map_err(anyhow::Error::from)?,
                ),
            ),
        ],
    )
    .await?;
    Ok(warp::reply::json(&serde_json::json!({ "message": msg })))
}
