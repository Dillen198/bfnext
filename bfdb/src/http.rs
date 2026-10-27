//! Shared HTTP plumbing: the error reply, paging, auth gates and the
//! filters every route module builds on.

use crate::*;

#[derive(Debug)]
pub(crate) struct Error(pub(crate) anyhow::Error);

impl Reply for Error {
    fn into_response(self) -> Response {
        // Typed errors (websec::ApiError) keep their status and message;
        // anything else is logged with a reference and answered generically.
        websec::error_response(&self.0)
    }
}

impl From<anyhow::Error> for Error {
    fn from(value: anyhow::Error) -> Self {
        Self(value)
    }
}


/// A route's query string as a map.
pub(crate) type Q = std::collections::HashMap<std::string::String, std::string::String>;

/// Default (and largest) page of each list route: far above what any of them
/// holds today, so a caller that sends no `?limit=` still gets everything --
/// they are a bound, not a page size.
pub(crate) const PILOT_NAMES_CAP: usize = 100_000;
pub(crate) const PILOT_LIST_CAP: usize = 10_000;
pub(crate) const SQUADRON_LIST_CAP: usize = 5_000;
pub(crate) const WIKI_LIST_CAP: usize = 5_000;
pub(crate) const ADMIN_LIST_CAP: usize = 10_000;

/// `?offset=` and `?limit=` (at most `cap`, and `cap` when absent) over a
/// list, plus how long the whole list is.
pub(crate) fn page<T>(mut rows: Vec<T>, q: &Q, cap: usize) -> (Vec<T>, usize) {
    let total = rows.len();
    let num = |k: &str| q.get(k).and_then(|s| s.trim().parse::<usize>().ok());
    let offset = num("offset").unwrap_or(0).min(total);
    let limit = num("limit").unwrap_or(cap).min(cap);
    rows.drain(..offset);
    rows.truncate(limit);
    (rows, total)
}

/// [`page`], serialized.
pub(crate) fn page_json<T: serde::Serialize>(rows: Vec<T>, q: &Q, cap: usize) -> Result<(String, usize)> {
    let (rows, total) = page(rows, q, cap);
    Ok((serde_json::to_string(&rows)?, total))
}

/// A page of a list: the JSON array as before, its full length in
/// `X-Total-Count`.
pub(crate) fn paged_response((data, total): (String, usize)) -> impl warp::Reply {
    reply::with_header(json_response(data), "x-total-count", total.to_string())
}

pub(crate) fn json_response(data: String) -> impl warp::Reply {
    reply::with_header(
        reply::with_header(data, "content-type", "application/json"),
        "cache-control",
        "no-store",
    )
}

/// Short-lived shared answers for the public read routes that walk whole
/// trees (kills, per-round aggregates, points) on every request. Every open
/// dashboard polls these; five seconds of staleness is invisible, and it turns
/// N identical full scans a second into one.
pub(crate) const PUBLIC_CACHE_TTL: std::time::Duration = std::time::Duration::from_secs(5);

/// How old an answer `cached_json` still hands out while it refreshes it in
/// the background.
pub(crate) const PUBLIC_CACHE_MAX_STALE: std::time::Duration = std::time::Duration::from_secs(60);

pub(crate) static ROUTE_CACHE: std::sync::LazyLock<websec::CacheMap<std::string::String, Arc<std::string::String>>> =
    std::sync::LazyLock::new(websec::CacheMap::new);


/// Serve `key` from [`ROUTE_CACHE`], computing it with `f` (a blocking DB
/// read) at most once per TTL however many requests arrive together. Past
/// the TTL the last answer is still served (up to
/// [`PUBLIC_CACHE_MAX_STALE`]) while `f` re-runs on the blocking pool, so a
/// request waits on a full scan only when there is no answer at all yet.
pub(crate) async fn cached_json(
    key: std::string::String,
    f: impl FnOnce() -> Result<std::string::String> + Send + 'static,
) -> std::result::Result<std::string::String, Error> {
    ROUTE_CACHE
        .entry(&key)
        .get_swr(PUBLIC_CACHE_TTL, PUBLIC_CACHE_MAX_STALE, move || f().map(Arc::new))
        .await
        .map(|a| (*a).clone())
        .map_err(Error)
}

/// The linked player behind a browser session, for the squadron routes.
pub(crate) async fn session_player(
    session_id: Option<Uuid>,
    db: &StatsDb,
    bot_cfg: &Option<BotLinkConfig>,
) -> std::result::Result<dcso3::net::Ucid, Error> {
    let sid = session_id.ok_or_else(|| websec::unauthorized("sign in with Discord first"))?;
    let session = task::block_in_place(|| db.get_session(sid))?
        .ok_or_else(|| websec::unauthorized("session expired"))?;
    let ucid = resolve_ucid_via_bot(bot_cfg, &session.discord_id).await.ok_or_else(|| {
        websec::forbidden(
            "account not linked -- type -linkme <token> in DCS chat (get the token with /linkme in Discord)",
        )
    })?;
    Ok(ucid)
}

/// A full admin: the Discord admin role, or the local password login. Kept
/// for what no narrower role covers (campaign resets, sessions, the audit
/// log, shutdown).
pub(crate) async fn require_admin(session_id: Option<Uuid>, db: StatsDb) -> std::result::Result<SessionData, Error> {
    let session = require_session(session_id, &db)?;
    if !session.is_admin {
        return Err(websec::forbidden("forbidden").into());
    }
    Ok(session)
}

/// A full admin, or a session holding `role` (see `admin::Role` and the
/// `--discord-*-role-id` flags). Every existing admin has every role, so for
/// them nothing changed.
pub(crate) async fn require_role(session_id: Option<Uuid>, db: StatsDb, role: u8) -> std::result::Result<SessionData, Error> {
    let session = require_session(session_id, &db)?;
    if !admin::has_role(&session, role) {
        return Err(websec::forbidden("forbidden").into());
    }
    Ok(session)
}

pub(crate) fn require_session(session_id: Option<Uuid>, db: &StatsDb) -> std::result::Result<SessionData, Error> {
    let Some(id) = session_id else {
        return Err(websec::unauthorized("not logged in").into());
    };
    Ok(task::block_in_place(|| db.get_session(id))?.ok_or_else(|| websec::unauthorized("session expired"))?)
}

/// Record an admin action: the `ADMIN:` log line it always had, plus a row
/// in the persistent audit log (`GET /api/admin/audit`).
pub(crate) fn audit(db: &StatsDb, who: &SessionData, action: &str, target: Option<&str>, inst: Option<&str>) {
    log::info!(
        "ADMIN: {} ({}) {action}{}{}",
        who.username,
        who.discord_id,
        target.map(|t| format!(" {t}")).unwrap_or_default(),
        inst.map(|i| format!(" [{i}]")).unwrap_or_default(),
    );
    let entry = db::AuditEntry {
        at: chrono::Utc::now(),
        who: who.discord_id.clone(),
        username: who.username.clone(),
        action: action.to_string(),
        target: target.map(str::to_string),
        instance: inst.map(str::to_string),
    };
    if let Err(e) = task::block_in_place(|| db.audit_put(&entry)) {
        log::warn!("audit log write failed: {e:#}");
    }
}

/// A static token from `?token=` or `Authorization: Bearer`, compared in
/// constant time against each accepted one. The query form stays:
/// WebFetch-style tooling cannot set headers.
pub(crate) fn bearer_or_query_token(
    q: &std::collections::HashMap<std::string::String, std::string::String>,
    auth_header: Option<&str>,
    accepted: &[&str],
) -> bool {
    let provided = q.get("token").cloned().or_else(|| {
        auth_header
            .and_then(|h| h.strip_prefix("Bearer ").or_else(|| h.strip_prefix("bearer ")))
            .map(|s| s.trim().to_string())
    });
    provided.as_deref().map_or(false, |p| {
        // every candidate is compared, so which one matched takes no less time
        accepted.iter().fold(false, |ok, want| websec::ct_eq(p, want) | ok)
    })
}

/// Wrong log/metrics tokens per client IP: 20 in ten minutes locks that
/// address out of both for ten minutes.
pub(crate) static TOKEN_FAILS: std::sync::LazyLock<websec::RateLimiter<std::net::IpAddr>> =
    std::sync::LazyLock::new(|| {
        websec::RateLimiter::new(20, std::time::Duration::from_secs(600), std::time::Duration::from_secs(600))
    });

/// `bearer_or_query_token`, rate limited per client IP.
pub(crate) fn check_static_token(
    q: &std::collections::HashMap<std::string::String, std::string::String>,
    auth_header: Option<&str>,
    accepted: &[&str],
    ip: std::net::IpAddr,
) -> std::result::Result<(), (warp::http::StatusCode, &'static str)> {
    if TOKEN_FAILS.blocked(&ip).is_some() {
        return Err((warp::http::StatusCode::TOO_MANY_REQUESTS, "too many bad tokens -- try again later\n"));
    }
    if bearer_or_query_token(q, auth_header, accepted) {
        return Ok(());
    }
    TOKEN_FAILS.hit(&ip);
    log::warn!("bad or missing log/metrics token from {ip}");
    Err((warp::http::StatusCode::UNAUTHORIZED, "bad or missing token\n"))
}

/// Largest JSON body any route accepts, unless it sets its own limit. Every
/// JSON POST here is a handful of fields; uploads have their own caps.
pub(crate) const JSON_BODY_LIMIT: u64 = 64 * 1024;

pub(crate) fn parse_side(s: &str) -> Option<dcso3::coalition::Side> {
    match s.to_ascii_lowercase().as_str() {
        "blue" => Some(dcso3::coalition::Side::Blue),
        "red" => Some(dcso3::coalition::Side::Red),
        _ => None,
    }
}

/// The authenticated caller of a coalition-locked endpoint (recon intel,
/// per-side briefing).
pub(crate) struct Caller {
    pub(crate) session: SessionData,
    pub(crate) ucid: Option<dcso3::net::Ucid>,
    /// The coalition whose data the caller gets. For a registered player this
    /// is *their* side, full stop. For a dashboard admin with no in-game
    /// registration it's whatever `?side=` asked for (default Blue).
    pub(crate) side: dcso3::coalition::Side,
    /// True only for an admin with no coalition of their own: they picked
    /// `side` via `?side=` and may freely inspect either side. A registered
    /// player -- admin or not -- is locked to their own side.
    pub(crate) god_mode: bool,
}

impl Caller {
    pub(crate) fn ucid(&self) -> Option<&dcso3::net::Ucid> {
        self.ucid.as_ref()
    }

    /// May this caller give orders on `inst` (`command`)? A pilot on a
    /// side who is a commander there, or any dashboard admin flying for one.
    /// An admin looking in with `?side=` has no pilot to order as.
    pub(crate) fn commands(&self, db: &StatsDb, inst: &InstanceState) -> bool {
        if self.god_mode {
            return false;
        }
        let Some(u) = self.ucid() else { return false };
        if self.session.is_admin {
            command::note_admin(*u);
            return true;
        }
        task::block_in_place(|| command::may_command(db, inst, u, self.side)).unwrap_or_else(|e| {
            log::warn!("[{}] commander check for {u}: {e:?}", inst.id);
            false
        })
    }

    /// This caller's commander standing on `inst`, for the dashboard.
    pub(crate) fn command_status(&self, db: &StatsDb, inst: &InstanceState) -> serde_json::Value {
        let Some(u) = self.ucid() else { return serde_json::Value::Null };
        match task::block_in_place(|| command::status_of(db, inst, u)) {
            Ok(s) => serde_json::to_value(s).unwrap_or(serde_json::Value::Null),
            Err(_) => serde_json::Value::Null,
        }
    }
}

/// Resolve the caller for a coalition-locked endpoint. A non-admin with no
/// Blue/Red side this campaign is rejected; a dashboard admin without a
/// registration may target a side via `?side=blue|red` (default Blue).
pub(crate) async fn require_coalition(
    query: &std::collections::HashMap<std::string::String, std::string::String>,
    session_id: Option<Uuid>,
    db: &StatsDb,
    bot_cfg: &Arc<Option<BotLinkConfig>>,
    inst: &InstanceState,
) -> std::result::Result<Caller, Error> {
    let Some(id) = session_id else {
        return Err(websec::unauthorized("not logged in").into());
    };
    let session = task::block_in_place(|| db.get_session(id))?
        .ok_or_else(|| websec::unauthorized("session expired"))?;
    let ucid = resolve_ucid_via_bot(bot_cfg, &session.discord_id).await;
    let own_side = match &ucid {
        Some(u) => task::block_in_place(|| db.pilot_current_side(&inst.id, u))?,
        None => None,
    };
    match own_side {
        Some(side) => Ok(Caller { session, ucid, side, god_mode: false }),
        None if session.is_admin => {
            let side = query
                .get("side")
                .and_then(|s| parse_side(s))
                .unwrap_or(dcso3::coalition::Side::Blue);
            Ok(Caller { session, ucid, side, god_mode: true })
        }
        None => Err(websec::forbidden(
            "you have no coalition this campaign -- register a side on the server first",
        )
        .into()),
    }
}

pub(crate) fn active_round_or_err(db: &StatsDb, inst: &InstanceState) -> std::result::Result<db::RoundId, Error> {
    task::block_in_place(|| db.active_round_id(&inst.id))?
        .ok_or_else(|| websec::not_found("no active round").into())
}

pub(crate) fn side_name(side: dcso3::coalition::Side) -> &'static str {
    match side {
        dcso3::coalition::Side::Red => "Red",
        dcso3::coalition::Side::Blue => "Blue",
        _ => "Neutral",
    }
}

pub(crate) fn extract_session_cookie() -> impl Filter<Extract = (Option<Uuid>,), Error = warp::Rejection> + Clone {
    warp::header::optional::<String>("cookie").map(|cookie: Option<String>| {
        cookie.and_then(|c| {
            c.split(';').find_map(|part| {
                let part = part.trim();
                part.strip_prefix("session=").and_then(|v| v.parse::<Uuid>().ok())
            })
        })
    })
}

pub(crate) fn with_auth_cfg(cfg: Option<AuthConfig>) -> impl Filter<Extract = (AuthConfig,), Error = warp::Rejection> + Clone {
    warp::any()
        .map(move || cfg.clone())
        .and_then(|cfg: Option<AuthConfig>| async move {
            cfg.ok_or_else(warp::reject::not_found)
        })
}

pub(crate) fn with_local_admin(cfg: Option<LocalAdminConfig>) -> impl Filter<Extract = (Option<LocalAdminConfig>,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || cfg.clone())
}

/// Whether this session cookie belongs to a dashboard admin.
pub(crate) fn session_is_admin(db: &StatsDb, session_id: Option<Uuid>) -> bool {
    match session_id {
        Some(id) => task::block_in_place(|| db.get_session(id))
            .ok()
            .flatten()
            .map(|s| s.is_admin)
            .unwrap_or(false),
        None => false,
    }
}

/// Gate an admin-only WebSocket: refuse the upgrade outright (it used to be
/// accepted and dropped, which still cost a socket per attempt) and hold a
/// per-IP slot for the life of the connection.
pub(crate) fn admin_ws_gate(
    db: &StatsDb,
    session_id: Option<Uuid>,
    ip: std::net::IpAddr,
) -> std::result::Result<websec::WsSlot, Response> {
    let ops = session_id
        .and_then(|id| task::block_in_place(|| db.get_session(id)).ok().flatten())
        .map_or(false, |s| admin::has_role(&s, admin::Role::OPS));
    if !ops {
        return Err(websec::ws_refused(warp::http::StatusCode::FORBIDDEN, "admin only"));
    }
    websec::ws_slot(ip).ok_or_else(|| {
        websec::ws_refused(warp::http::StatusCode::TOO_MANY_REQUESTS, "too many open connections")
    })
}

pub(crate) fn with_news_images(
    c: Option<Arc<news_image::ImageCfg>>,
) -> impl Filter<Extract = (Option<Arc<news_image::ImageCfg>>,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || c.clone())
}

pub(crate) fn with_db(db: StatsDb) -> impl Filter<Extract = (StatsDb,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || db.clone())
}

pub(crate) fn with_bot_link_cfg(
    cfg: Arc<Option<BotLinkConfig>>,
) -> impl Filter<Extract = (Arc<Option<BotLinkConfig>>,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || cfg.clone())
}

/// Resolve `?instance=<id>` to that DCS server's runtime state.
///
/// Omitted (or empty) picks the default instance, which is what a
/// single-instance deployment and every pre-existing client sends. An id that
/// is not configured is already answered with a 400 by `bad_instance_guard`,
/// which runs ahead of every route -- so this filter is deliberately
/// **infallible**.
///
/// That matters on the POST routes: several of them mount this after
/// `warp::body::json()`, and a rejection there would move warp on to the next
/// `.or(...)` branch with the request body already consumed. Falling back to
/// the default instance in the (unreachable) unknown-id case keeps that from
/// ever being possible. (The `Rejection` in the signature is only
/// `warp::query`'s own malformed-query-string rejection, which every route
/// here already hits on an earlier query filter, before its body is read.)
pub(crate) fn with_instance(db: StatsDb) -> impl Filter<Extract = (Inst,), Error = warp::Rejection> + Clone {
    warp::query::<std::collections::HashMap<std::string::String, std::string::String>>()
        .and(extract_session_cookie())
        .and(warp::any().map(move || db.clone()))
        .map(
            |q: std::collections::HashMap<std::string::String, std::string::String>, sid: Option<Uuid>, db: StatsDb| {
                // `?server=` lets a caller that only knows the DCSServerBot
                // server name (the Discord plugin) address an instance without
                // having to learn its bfdb id first.
                let by_server = q.get("server").and_then(|name| {
                    db.instances()
                        .by_dcs_server_name(name)
                        .map(|cfg| cfg.id.clone())
                });
                let requested = by_server
                    .as_deref()
                    .or_else(|| q.get("instance").map(|s| s.as_str()))
                    .filter(|s| !s.trim().is_empty());
                match requested {
                    Some(_) => db
                        .resolve_state(requested)
                        .unwrap_or_else(|_| db.state(db.instances().default_id())),
                    None => db.state(&implicit_instance(&db, sid)),
                }
            },
        )
}

/// The instance a request that names none gets: the configured default --
/// unless that is a non-public campaign and the caller is not an admin. It
/// used to be served regardless, so anyone who left `?instance=` off read
/// the war the lock on `?instance=<id>` hides. Such a caller gets the first
/// public campaign instance instead -- the one `GET /api/instances` already
/// tells them is the default -- or, when there is none, the private default,
/// which `bad_instance_guard` then refuses on the per-instance routes.
pub(crate) fn implicit_instance(db: &StatsDb, sid: Option<Uuid>) -> InstanceId {
    let default = db.instances().default_id().clone();
    let Some(cfg) = db.instances().get(&default) else { return default };
    if cfg.public || cfg.is_range() || session_is_admin(db, sid) {
        return default;
    }
    db.instances()
        .all()
        .iter()
        .find(|c| c.public && !c.is_range())
        .map(|c| InstanceId::from(c.id.as_str()))
        .unwrap_or(default)
}

/// The routes that take no `?instance=` (or gate themselves), which the
/// private-default lock in `bad_instance_guard` leaves alone.
pub(crate) fn instance_agnostic(path: &str) -> bool {
    const PREFIXES: &[&str] = &[
        "/api/auth/", "/api/instances", "/api/version", "/api/config", "/api/health",
        "/api/wiki/pages", "/api/wiki/images", "/api/pilots", "/api/pilot/", "/api/squadrons",
        "/api/progression/", "/api/cockpit/", "/api/range/", "/api/replay/", "/api/admin/",
        "/api/metrics", "/api/logs/", "/ws/logs",
    ];
    PREFIXES.iter().any(|p| path.starts_with(p))
}

/// A front-of-chain guard that turns an unroutable `?instance=` / `?server=`
/// into a 400 instead of letting it fall through.
///
/// `with_instance` rejects on an unknown id, but a warp rejection just moves on
/// to the next `.or(...)` branch -- and the chain ends in the SPA catch-all,
/// which happily answers 200 with `index.html`. A stale bookmark would then
/// look like it worked while showing nothing. This filter sits ahead of every
/// other route, matches only `/api/...` requests whose instance cannot be
/// resolved, and answers them directly; anything valid rejects here and
/// continues down the chain as normal.
pub(crate) fn bad_instance_guard(
    db: StatsDb,
) -> impl Filter<Extract = (Response,), Error = warp::Rejection> + Clone {
    warp::path::full()
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(extract_session_cookie())
        .and(warp::any().map(move || db.clone()))
        .and(websec::with_client_ip())
        .and_then(
            |full: warp::path::FullPath,
             q: std::collections::HashMap<std::string::String, std::string::String>,
             session_id: Option<Uuid>,
             db: StatsDb,
             ip: std::net::IpAddr| async move {
                let path = full.as_str();
                if !(path.starts_with("/api/") || path.starts_with("/ws/")) {
                    return Err(warp::reject::reject());
                }
                let by_instance = q.get("instance");
                let named = q.get("server").or(by_instance);
                let Some(name) = named.map(|s| s.trim()).filter(|s| !s.is_empty()) else {
                    // Nothing named: `with_instance` picks the instance. When
                    // that can only be a non-public campaign (no public one
                    // exists), it is locked exactly as if it had been named.
                    // This machine's own callers (the Discord bot) are not.
                    let implicit = implicit_instance(&db, session_id);
                    let locked = db.instances().get(&implicit).map_or(false, |c| !c.public && !c.is_range())
                        && !instance_agnostic(path)
                        && !ip.is_loopback()
                        && !session_is_admin(&db, session_id);
                    if locked {
                        return Ok(websec::error_response(&websec::not_found(
                            "this server's campaign is not public -- log in as an admin",
                        )));
                    }
                    return Err(warp::reject::reject());
                };
                // `?instance=all` is meaningful on the aggregate routes; let
                // those handle it themselves.
                if name == "all" {
                    return Err(warp::reject::reject());
                }
                let cfg = db
                    .instances()
                    .by_dcs_server_name(name)
                    .or_else(|| db.instances().get(name))
                    .cloned();
                if let Some(cfg) = cfg {
                    // A non-public (test/staging) campaign instance used to be
                    // merely left out of the selector: anyone who guessed its
                    // id could read its war. Locked to admins now. The cockpit
                    // overlay keeps its own per-player auth (players on a test
                    // server still need their panel) and training ranges are
                    // not campaigns.
                    let locked = !cfg.public
                        && !cfg.is_range()
                        && !path.starts_with("/api/cockpit/")
                        && !session_is_admin(&db, session_id);
                    if !locked {
                        return Err(warp::reject::reject());
                    }
                    return Ok(websec::error_response(&websec::not_found(format!(
                        "unknown instance {name:?}"
                    ))));
                }
                // `?server=` is a name the *caller* knows itself by, not an id
                // it looked up here: the in-DCS cockpit overlay sends whatever
                // the DCS server calls itself, because a player's machine has
                // no way to learn our short instance ids. A single-instance
                // deployment normally leaves `dcs_server_name` unset, so that
                // name resolves to nothing -- and there is exactly one campaign
                // it could possibly have meant. Let it through as a hint
                // (`with_instance` falls back to the default) instead of
                // 400ing every call the overlay makes. With several instances
                // configured the ambiguity is real, so it still fails loudly
                // rather than showing someone the wrong server's war.
                if by_instance.is_none() && db.instances().all().len() == 1 {
                    return Err(warp::reject::reject());
                }
                let ids: Vec<&str> = db.instances().all().iter().map(|i| i.id.as_str()).collect();
                Ok(warp::reply::with_status(
                    warp::reply::json(&serde_json::json!({
                        "error": format!("unknown instance {name:?}"),
                        "instances": ids,
                    })),
                    warp::http::StatusCode::BAD_REQUEST,
                )
                .into_response())
            },
        )
}

/// Per-instance live-telemetry state owned by the web layer: the `Export.lua`
/// unit feed and the fog-of-war tactical picture. One of these exists per
/// configured instance, so two DCS servers never bleed contacts into each
/// other's scope.
#[derive(Clone)]
pub(crate) struct InstanceLive {
    pub(crate) live: LiveState,
    pub(crate) live_tx: broadcast::Sender<String>,
    pub(crate) tac: TacState,
    /// The ground war per side, for `/ws/groundwar` (`groundfeed`).
    pub(crate) ground: groundfeed::GroundState,
}

pub(crate) type LiveMap = Arc<std::collections::HashMap<InstanceId, InstanceLive>>;

pub(crate) fn live_for(map: &LiveMap, inst: &InstanceState) -> InstanceLive {
    map.get(&inst.id)
        .cloned()
        .expect("every instance gets an InstanceLive at startup")
}

pub(crate) fn with_live(map: LiveMap) -> impl Filter<Extract = (LiveMap,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || map.clone())
}
