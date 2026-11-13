//! `warp::test` coverage for the whole route chain as `build_routes` puts it
//! together: which route answers what, and the auth / CSRF gates in front of
//! them. No DCS server, netidx resolver, bot or socket is involved -- every
//! instance here is offline.
//!
//! Several cases are regressions recorded in comments in `build_routes`; each
//! names the one it pins.

use crate::*;
use std::net::SocketAddr;

/// What `warp::test` hands back.
pub(crate) type TestResp = warp::http::Response<bytes::Bytes>;

pub(crate) struct Harness {
    pub(crate) db: StatsDb,
    pub(crate) routes: warp::filters::BoxedFilter<(Response,)>,
    dir: PathBuf,
}

impl Drop for Harness {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

pub(crate) const DASH: &str = "https://dash.example.org";
pub(crate) const API: &str = "https://api.example.org";

/// One offline campaign instance called "default".
pub(crate) fn harness() -> Harness {
    harness_with(serde_json::json!({ "instances": [{ "id": "default" }] }), |_| ())
}

/// A harness over an `--instances` file's worth of JSON, with a hook to adjust
/// the route context before the chain is built.
pub(crate) fn harness_with(
    instances: serde_json::Value,
    tweak: impl FnOnce(&mut RouteCtx),
) -> Harness {
    // never ask GitHub from a test
    let _ = COCKPIT_VERSION_OVERRIDE.set("9.9.9-test".into());
    let dir = std::env::temp_dir().join(format!("bfdb-route-test-{}", Uuid::new_v4()));
    std::fs::create_dir_all(&dir).unwrap();
    let file: instance::InstancesFile = serde_json::from_value(instances).unwrap();
    let reg = Registry::new(file).unwrap();
    let db =
        StatsDb::new(&std::collections::HashMap::new(), dir.join("db"), reg).unwrap();
    let bot: Arc<Option<BotLinkConfig>> = Arc::new(None);
    let range_ctx = range::RangeCtx::new(
        db.clone(),
        bot.clone(),
        "https://range.example.org",
        API,
        dir.join("clips"),
    )
    .unwrap();
    let replay_ctx = replay::ReplayCtx::new(db.clone(), dir.join("replays"), 30).unwrap();
    let mut live = std::collections::HashMap::new();
    let mut gci = std::collections::HashMap::new();
    for cfg in db.instances().all() {
        let id: InstanceId = Arc::from(cfg.id.as_str());
        let (live_tx, _) = broadcast::channel::<String>(8);
        live.insert(
            id.clone(),
            InstanceLive {
                live: Arc::new(tokio::sync::RwLock::new((0.0, Vec::new(), Vec::new()))),
                live_tx,
                tac: Arc::new(tokio::sync::RwLock::new(TacCache::default())),
                ground: Default::default(),
            },
        );
        let (tx, _) = broadcast::channel::<String>(8);
        gci.insert(id, (tx, Arc::new(Mutex::new(VecDeque::new()))));
    }
    let (log_tx, _) = broadcast::channel::<String>(8);
    let mut rc = RouteCtx {
        db: db.clone(),
        bot_link_cfg: bot,
        auth_cfg: None,
        local_admin_cfg: None,
        live_map: Arc::new(live),
        gci_map: Arc::new(gci),
        log_tx,
        log_history: Arc::new(Mutex::new(VecDeque::new())),
        range_routes: range::api::routes(range_ctx),
        replay_routes: replay::api::routes(replay_ctx),
        campaign_json: Arc::new(r#"{"name":"Test"}"#.to_string()),
        effective_srs_url: None,
        news_images: None,
        shutdown: maint::Shutdown::default(),
        cors_origins: vec![DASH.to_string()],
        public_api_url: API.to_string(),
        listen_port: 8880,
        local_login_from: Vec::new(),
        log_read_token: None,
        metrics_token: None,
        shutdown_token: None,
        get_limits: websec::GetLimits { per_min: 1800, heavy_per_min: 120 },
    };
    tweak(&mut rc);
    let routes = build_routes(rc);
    Harness { db, routes, dir }
}

impl Harness {
    /// A logged-in session; returns its cookie header value.
    pub(crate) fn session(&self, admin: bool) -> String {
        self.session_with(admin, if admin { admin::Role::ALL } else { 0 })
    }

    /// A session holding just `roles` (not a full admin unless `admin`).
    pub(crate) fn session_with(&self, admin: bool, roles: u8) -> String {
        let id = Uuid::new_v4();
        self.db
            .create_session(
                id,
                SessionData {
                    discord_id: format!("discord-{id}"),
                    username: "tester".into(),
                    avatar: None,
                    is_admin: admin,
                    expires: chrono::Utc::now() + chrono::Duration::hours(1),
                    roles,
                },
            )
            .unwrap();
        format!("session={id}")
    }

    pub(crate) async fn get(&self, path: &str, cookie: Option<&str>) -> TestResp {
        let mut r = warp::test::request().method("GET").path(path).remote_addr(peer());
        if let Some(c) = cookie {
            r = r.header("cookie", c);
        }
        r.reply(&self.routes).await
    }

    pub(crate) async fn post_json(
        &self,
        path: &str,
        cookie: Option<&str>,
        origin: Option<&str>,
        body: &str,
    ) -> TestResp {
        let mut r = warp::test::request()
            .method("POST")
            .path(path)
            .remote_addr(peer())
            .header("content-type", "application/json")
            .body(body);
        if let Some(c) = cookie {
            r = r.header("cookie", c);
        }
        if let Some(o) = origin {
            r = r.header("origin", o);
        }
        r.reply(&self.routes).await
    }
}

/// A distinct external client address per call: the per-IP limiters are
/// process-wide statics shared by every test.
pub(crate) fn peer() -> SocketAddr {
    use std::sync::atomic::{AtomicU32, Ordering};
    static N: AtomicU32 = AtomicU32::new(1);
    let n = N.fetch_add(1, Ordering::Relaxed);
    SocketAddr::from(([198, 18, (n >> 8) as u8, n as u8], 40000))
}

pub(crate) fn ctype(r: &TestResp) -> String {
    r.headers()
        .get("content-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("")
        .to_string()
}

pub(crate) fn body_json(r: TestResp) -> serde_json::Value {
    serde_json::from_slice(r.body()).unwrap_or(serde_json::Value::Null)
}

fn is_html(r: &TestResp) -> bool {
    ctype(r).starts_with("text/html")
}

#[tokio::test(flavor = "multi_thread")]
async fn api_gets_are_answered_by_their_route_not_the_spa() {
    // "GETs belong in this group, ahead of the dashboard's catch-all page:
    // added after it, they were never reached (the SPA answered /api/command
    // with index.html, 200)."
    let h = harness();
    for path in [
        "/api/instances",
        "/api/rounds",
        "/api/leaderboard",
        "/api/pilots",
        "/api/kills",
        "/api/capture-events",
        "/api/points",
        "/api/captures",
        "/api/aircraft-usage",
        "/api/online",
        "/api/config",
        "/api/version",
        "/api/squadrons",
        "/api/progression/top",
        "/api/wiki/pages",
        "/api/wiki/facts",
        "/api/command",
        "/api/command/log",
        "/api/command/me",
        "/api/admin/commanders",
        "/api/admin/sessions",
        "/api/admin/banned",
        "/api/admin/cfg",
        "/api/admin/cfg/schema",
        "/api/cockpit/plugin/version",
        "/api/cockpit/context",
        "/api/cockpit/aar",
        "/api/replay/status",
    ] {
        let r = h.get(path, None).await;
        assert!(!is_html(&r), "{path} was answered by the SPA ({})", r.status());
    }
    // The catch-all still serves the dashboard for page routes.
    let r = h.get("/map", None).await;
    assert_eq!(r.status(), 200);
    assert!(is_html(&r));
}

#[tokio::test(flavor = "multi_thread")]
async fn squadron_posts_route_instead_of_405() {
    // "Squadrons and GCI transmit: POSTs as well as GETs, so outside the
    // `warp::get()` group below. Inside it every POST was refused with a 405."
    let h = harness();
    for (path, body) in [
        ("/api/squadrons", r#"{"tag":"ABC","name":"Alpha"}"#),
        ("/api/squadrons/ABC/join", "{}"),
        ("/api/squadrons/leave", "{}"),
        ("/api/squadrons/kick", r#"{"ucid":"x"}"#),
        ("/api/gci/transmit", r#"{"text":"hi"}"#),
    ] {
        let r = h.post_json(path, None, None, body).await;
        assert_eq!(r.status(), 401, "{path}");
        assert!(ctype(&r).starts_with("application/json"), "{path}");
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn squadron_tag_with_symbols_is_found_by_its_encoded_path() {
    // Tags carry symbols ("[.ID]"); the dashboard percent-encodes them and
    // warp hands the segment over still encoded.
    let h = harness();
    let owner: dcso3::net::Ucid = "0123456789abcdef0123456789abcdef".parse().unwrap();
    h.db.squadrons().create(owner, "[.ID]", "Garuda virtual Air Wing").unwrap();
    let r = h.get("/api/squadrons/%5B.ID%5D", None).await;
    assert_eq!(r.status(), 200);
    let v: serde_json::Value = serde_json::from_slice(r.body()).unwrap();
    assert_eq!(v["tag"], "[.ID]");
    let r = h.get("/api/squadrons/%5B.id%5D", None).await;
    assert_eq!(r.status(), 200);
}

#[tokio::test(flavor = "multi_thread")]
async fn ops_proxy_get_without_content_length_reaches_the_handler() {
    // "warp's content_length_limit also *requires* a Content-Length header,
    // which browsers never send on a GET -- so every GET the OPS page made was
    // rejected here and fell through to the SPA catch-all (Sept 28)."
    let h = harness();
    let r = h.get("/api/admin/ops/status", None).await;
    assert_eq!(r.status(), 401);
    let admin = h.session(true);
    let r = h.get("/api/admin/ops/status", Some(&admin)).await;
    // past the admin gate: no bot configured
    assert_eq!(r.status(), 503);
    assert!(!is_html(&r));
    // and a POST still routes
    let r = h.post_json("/api/admin/ops/status", Some(&admin), Some(DASH), "{}").await;
    assert_eq!(r.status(), 503);
}

/// A stand-in for the bot's WebService: answers every request with what it
/// was sent (method, path, query, the headers bfdb adds, the body's length and
/// type), and a file download for GET .../console/mission.
async fn echo_bot() -> String {
    use warp::Filter;
    let echo = warp::method()
        .and(warp::path::full())
        .and(warp::query::raw().or(warp::any().map(String::new)).unify())
        .and(warp::header::headers_cloned())
        .and(warp::body::bytes())
        .map(
            |m: warp::http::Method,
             p: warp::path::FullPath,
             q: String,
             h: warp::http::HeaderMap,
             b: bytes::Bytes| {
                let hv =
                    |n: &str| h.get(n).and_then(|v| v.to_str().ok()).map(str::to_string);
                if m == warp::http::Method::GET && p.as_str().ends_with("/console/stream") {
                    // two events, the second split across lines, and a ping; then it ends
                    let body = format!(
                        "event: fleet\ndata: {{\"roles\":\"{}\"}}\n\n: ping\n\nevent: fleet\ndata: a\ndata: b\n\n",
                        hv("x-fowl-roles").unwrap_or_default()
                    );
                    return warp::http::Response::builder()
                        .header("content-type", "text/event-stream")
                        .body(body.into_bytes())
                        .unwrap();
                }
                if m == warp::http::Method::GET
                    && p.as_str().ends_with("/console/mission")
                {
                    return warp::http::Response::builder()
                        .header("content-type", "application/octet-stream")
                        .header("content-disposition", "attachment; filename=\"Op.miz\"")
                        .body(b"PKmiz".to_vec())
                        .unwrap();
                }
                let doc = serde_json::json!({
                    "method": m.as_str(), "path": p.as_str(), "query": q,
                    "key": hv("x-api-key"), "roles": hv("x-fowl-roles"),
                    "actor": hv("x-fowl-actor"), "subject": hv("x-fowl-subject"),
                    "ctype": hv("content-type"), "len": b.len(),
                });
                warp::http::Response::builder()
                    .header("content-type", "application/json")
                    .body(serde_json::to_vec(&doc).unwrap())
                    .unwrap()
            },
        );
    let listener = tokio::net::TcpListener::bind(SocketAddr::from(([127, 0, 0, 1], 0)))
        .await
        .unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(warp::serve(echo).incoming(listener).run());
    format!("http://{addr}/stats")
}

fn bot_harness(base_url: String) -> Harness {
    harness_with(serde_json::json!({ "instances": [{ "id": "default" }] }), |rc| {
        rc.bot_link_cfg = Arc::new(Some(BotLinkConfig {
            base_url,
            api_key: "rest".into(),
            ops_key: "opskey".into(),
        }));
    })
}

#[tokio::test(flavor = "multi_thread")]
async fn ops_proxy_names_the_caller_and_lets_moderators_into_the_console() {
    let h = bot_harness(echo_bot().await);
    let moderator = h.session_with(false, admin::Role::MODERATION);
    // the OPS page itself stays OPS-only
    let r = h.get("/api/admin/ops/status", Some(&moderator)).await;
    assert_eq!(r.status(), 403);
    // the bot console is open to a moderator; the plugin checks each action
    let r = h
        .post_json(
            "/api/admin/ops/console/act/kick_player",
            Some(&moderator),
            Some(DASH),
            r#"{"params":{"server_name":"vs1","ucid":"u"}}"#,
        )
        .await;
    assert_eq!(r.status(), 200);
    let doc = body_json(r);
    assert_eq!(doc["path"], "/stats/fowlengine/ops/console/act/kick_player");
    assert_eq!(doc["key"], "opskey");
    assert_eq!(doc["roles"], "moderation");
    assert_eq!(doc["actor"], "tester");
    assert!(doc["subject"].as_str().unwrap().starts_with("discord-"));
    assert_eq!(doc["ctype"], "application/json");
    // a full admin is "admin"; a session with no admin role gets nothing
    let admin = h.session(true);
    let r = h.get("/api/admin/ops/console/catalog?x=1", Some(&admin)).await;
    let doc = body_json(r);
    assert_eq!(
        (doc["roles"].as_str(), doc["query"].as_str()),
        (Some("admin"), Some("x=1"))
    );
    let content = h.session_with(false, admin::Role::CONTENT);
    let r = h.get("/api/admin/ops/console/catalog", Some(&content)).await;
    assert_eq!(r.status(), 403);
}

#[tokio::test(flavor = "multi_thread")]
async fn ops_proxy_carries_mission_files_both_ways() {
    let h = bot_harness(echo_bot().await);
    let ops = h.session_with(false, admin::Role::OPS);
    // upload: a body over the 4 MB the rest of ops/ takes, as octet-stream
    let big = vec![7u8; 5 * 1024 * 1024];
    let r = warp::test::request()
        .method("POST")
        .path("/api/admin/ops/console/mission?server=vs1&name=Op.miz")
        .remote_addr(peer())
        .header("cookie", &ops)
        .header("origin", DASH)
        .header("content-type", "application/octet-stream")
        .body(big.clone())
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 200, "{:?}", r.body());
    let doc = body_json(r);
    assert_eq!(doc["len"], big.len());
    assert_eq!(doc["ctype"], "application/octet-stream");
    assert_eq!(doc["query"], "server=vs1&name=Op.miz");
    // anywhere else, octet-stream is not passed on and 5 MB is too big
    let r = warp::test::request()
        .method("POST")
        .path("/api/admin/ops/console/act/kick_player")
        .remote_addr(peer())
        .header("cookie", &ops)
        .header("origin", DASH)
        .header("content-type", "application/octet-stream")
        .body(b"{}")
        .reply(&h.routes)
        .await;
    assert_eq!(body_json(r)["ctype"], "application/json");
    let r = warp::test::request()
        .method("POST")
        .path("/api/admin/ops/config")
        .remote_addr(peer())
        .header("cookie", &ops)
        .header("origin", DASH)
        .header("content-type", "application/json")
        .body(big)
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 413);
    // download: bytes and file name come through
    let r =
        h.get("/api/admin/ops/console/mission?server=vs1&mission=1", Some(&ops)).await;
    assert_eq!(r.status(), 200);
    assert_eq!(r.body().as_ref(), b"PKmiz");
    assert_eq!(
        r.headers().get("content-disposition").unwrap(),
        "attachment; filename=\"Op.miz\""
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn ops_stream_passes_the_bots_events_on() {
    let h = bot_harness(echo_bot().await);
    let r = h.get("/api/admin/ops/console/stream", None).await;
    assert_eq!(r.status(), 401);
    let content = h.session_with(false, admin::Role::CONTENT);
    let r = h.get("/api/admin/ops/console/stream", Some(&content)).await;
    assert_eq!(r.status(), 403);
    let moderator = h.session_with(false, admin::Role::MODERATION);
    let r = h.get("/api/admin/ops/console/stream", Some(&moderator)).await;
    assert_eq!(r.status(), 200);
    assert!(ctype(&r).starts_with("text/event-stream"), "{}", ctype(&r));
    let body = String::from_utf8_lossy(r.body()).to_string();
    assert!(body.contains("event:fleet\ndata:{\"roles\":\"moderation\"}\n\n"), "{body}");
    assert!(body.contains(":ping\n\n"), "{body}");
    assert!(body.contains("event:fleet\ndata:a\ndata:b\n\n"), "{body}");
}

#[test]
fn sse_blocks_become_warp_events() {
    let ev = |s: &str| sse_block(s).to_string();
    assert_eq!(ev("event: fleet\ndata: {}\n\n"), "event:fleet\ndata:{}\n\n");
    assert_eq!(ev(": ping\n\n"), ":ping\n\n");
    assert_eq!(ev("data: x\ndata: y\n"), "data:x\ndata:y\n\n");
}

#[tokio::test(flavor = "multi_thread")]
async fn ops_proxy_carries_music_uploads() {
    let h = bot_harness(echo_bot().await);
    let ops = h.session_with(false, admin::Role::OPS);
    let song = vec![1u8; 5 * 1024 * 1024];
    let r = warp::test::request()
        .method("POST")
        .path("/api/admin/ops/console/music?name=a.mp3&folder=rock")
        .remote_addr(peer())
        .header("cookie", &ops)
        .header("origin", DASH)
        .header("content-type", "application/octet-stream")
        .body(song.clone())
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 200);
    let doc = body_json(r);
    assert_eq!(doc["len"], song.len());
    assert_eq!(doc["ctype"], "application/octet-stream");
    assert_eq!(doc["path"], "/stats/fowlengine/ops/console/music");
    assert!(is_upload_path("console/music") && !is_upload_path("console/act/x"));
}

#[tokio::test(flavor = "multi_thread")]
async fn require_admin_gates() {
    let h = harness();
    let r = h.get("/api/admin/sessions", None).await;
    assert_eq!(r.status(), 401);
    let player = h.session(false);
    let r = h.get("/api/admin/sessions", Some(&player)).await;
    assert_eq!(r.status(), 403);
    let admin = h.session(true);
    let r = h.get("/api/admin/sessions", Some(&admin)).await;
    assert_eq!(r.status(), 200);
    // an unknown or expired session is "not logged in", not an admin
    let r =
        h.get("/api/admin/sessions", Some(&format!("session={}", Uuid::new_v4()))).await;
    assert_eq!(r.status(), 401);
    // POST admin actions use the same gate
    let r = h.post_json("/api/admin/merge-rounds", Some(&player), Some(DASH), "{}").await;
    assert_eq!(r.status(), 403);
}

#[tokio::test(flavor = "multi_thread")]
async fn require_coalition_gates() {
    let h = harness();
    let r = h.get("/api/intel/captures", None).await;
    assert_eq!(r.status(), 401);
    // a player with no side this campaign (no bot link at all here)
    let player = h.session(false);
    let r = h.get("/api/intel/captures", Some(&player)).await;
    assert_eq!(r.status(), 403);
    // an admin with no registration may look in with ?side= -- past the gate,
    // there is simply no round yet
    let admin = h.session(true);
    let r = h.get("/api/intel/captures?side=red", Some(&admin)).await;
    assert_eq!(r.status(), 404);
}

#[tokio::test(flavor = "multi_thread")]
async fn cockpit_auth() {
    let h = harness();
    // nobody at all
    let r = h.get("/api/cockpit/context", None).await;
    assert_eq!(r.status(), 401);
    // an overlay too old to send its key
    let r = h.get("/api/cockpit/context?playerid=3", None).await;
    assert_eq!(r.status(), 401);
    assert!(body_json(r)["error"].as_str().unwrap().contains("out of date"));
    // with a key it goes to the engine to check it -- there is none here
    let r = h.get("/api/cockpit/context?playerid=3&ckey=abcdef", None).await;
    assert_ne!(r.status(), 401);
    assert_ne!(r.status(), 200);
    // the key may come in a header instead of the URL
    let r = warp::test::request()
        .method("GET")
        .path("/api/cockpit/context?playerid=3&plugin=3.4.0")
        .remote_addr(peer())
        .header("x-cockpit-key", "abcdef")
        .reply(&h.routes)
        .await;
    assert_ne!(r.status(), 401);
    assert!(COCKPIT_PLUGINS.lock().unwrap().contains_key("3.4.0"));
    // the advertised version
    let r = h.get("/api/cockpit/plugin/version", None).await;
    assert_eq!(body_json(r)["version"], "9.9.9-test");
    // a browser session with no Discord link is told so
    let player = h.session(false);
    let r = h.get("/api/cockpit/context", Some(&player)).await;
    assert_eq!(r.status(), 403);
}

#[tokio::test(flavor = "multi_thread")]
async fn cockpit_aar_needs_the_key() {
    let h = harness();
    let r = h.get("/api/cockpit/aar", None).await;
    assert_eq!(r.status(), 401);
    let r = h.get("/api/cockpit/aar?playerid=3", None).await;
    assert_eq!(r.status(), 401);
}

#[tokio::test(flavor = "multi_thread")]
async fn unknown_api_route_is_a_404_not_the_dashboard() {
    let h = harness();
    let r = h.get("/api/cockpit/no-such-route", None).await;
    assert_eq!(r.status(), 404);
    let body: serde_json::Value = serde_json::from_slice(r.body()).unwrap();
    assert_eq!(body["error"], "no such API route");
    // a dashboard page address still gets the dashboard
    let r = h.get("/command", None).await;
    assert_eq!(r.status(), 200);
}

#[tokio::test(flavor = "multi_thread")]
async fn cockpit_jtac_needs_the_key() {
    let h = harness();
    let r = h.get("/api/cockpit/jtac", None).await;
    assert_eq!(r.status(), 401);
    let r = h.get("/api/cockpit/jtac?playerid=3", None).await;
    assert_eq!(r.status(), 401);
}

#[test]
fn cockpit_jtac_reply() {
    use bfprotocols::cas::{CasFlight, CasJtac, CasPicture, CasTarget};
    let jtac = CasJtac {
        id: "g42".into(),
        callsign: "Axeman 1".into(),
        lat: 33.5,
        lon: 36.2,
        alt_m: 600,
        airborne: false,
        laser_code: 1688,
        ir_pointer: true,
        location_name: "Rosh Pina".into(),
        location_brg: 270,
        location_rng_m: 4000,
        target: Some(CasTarget {
            typ: "T-72B".into(),
            lat: 33.51,
            lon: 36.21,
            elev_ft: 820,
            brg: 45,
            rng_m: 3100,
            lasing: true,
            group_size: 4,
        }),
        contacts: vec![],
        contact_count: 6,
        nearest_friendly_m: Some(2500),
        nearest_friendly_brg: 180,
        artillery_available: false,
        threats: vec![],
    };
    let flight = |ucid: &str, near: &str| CasFlight {
        ucid: ucid.into(),
        unit_id: Some(7),
        callsign: "Hawg 1".into(),
        player_name: "someone".into(),
        lat: 33.0,
        lon: 36.0,
        alt_m: 3000,
        heading: 90,
        speed_ms: 150,
        nearest_jtac: Some(near.into()),
        jtac_brg: 12,
        jtac_rng_m: 20000,
    };
    let picture = CasPicture {
        jtacs: vec![jtac],
        flights: vec![flight("me", "Axeman 1"), flight("them", "Other")],
        bullseye: Some((33.0, 35.0)),
    };
    let v: serde_json::Value =
        serde_json::from_str(&jtac_reply("blue", Some(picture), "me")).unwrap();
    assert_eq!(v["side"], "blue");
    assert_eq!(v["jtacs"][0]["callsign"], "Axeman 1");
    assert_eq!(v["jtacs"][0]["laser_code"], 1688);
    assert_eq!(v["jtacs"][0]["target"]["lasing"], true);
    assert_eq!(v["me"]["nearest_jtac"], "Axeman 1");
    assert_eq!(v["me"]["jtac_rng_m"], 20000);
    // other pilots and their ucids, and the bullseye, never leave bfdb
    let s = v.to_string();
    assert!(!s.contains("them") && !s.contains("someone") && !s.contains("flights"));
    assert!(v.get("bullseye").is_none());
    // a caller with no side, or not in the picture
    let v: serde_json::Value =
        serde_json::from_str(&jtac_reply("neutral", None, "me")).unwrap();
    assert_eq!(v["jtacs"].as_array().map(|a| a.len()), Some(0));
    assert!(v["me"].is_null());
}

#[test]
fn cockpit_aar_reply() {
    use bfprotocols::aar::{AarCue, AarCues, CueSource, Station};
    use bfprotocols::range::RefuelMethod;
    let cue = AarCue {
        tanker: "Texaco 1-1".into(),
        typ: "KC-135".into(),
        method: RefuelMethod::Boom,
        station: Station::Centre,
        source: CueSource::Tanker,
        ud: Some(-1),
        fa: Some(0),
        lat: None,
        err: None,
        tol: [3.5, 2.5, 3.0],
        rel: [-30., -9., 0.],
        range_m: 31.3,
        closure_kts: 1.,
        connected: false,
        pod: None,
        tx: [1., 0., 0.],
        ty: [0., 1., 0.],
        tz: [0., 0., 1.],
        tvel: [150., 0., 0.],
    };
    let mut cues = AarCues { t: 10., cues: Default::default() };
    cues.cues.insert("abc".into(), cue);
    let m = std::sync::Mutex::new(Some((cues, std::time::Instant::now())));
    let v: serde_json::Value = serde_json::from_str(&aar_reply(&m, "abc")).unwrap();
    assert_eq!(v["cue"]["ud"], -1);
    assert_eq!(v["cue"]["source"], "tanker");
    // someone else's: nothing
    let v: serde_json::Value = serde_json::from_str(&aar_reply(&m, "xyz")).unwrap();
    assert!(v["cue"].is_null());
    // nothing from the engine yet
    let none = std::sync::Mutex::new(None);
    let v: serde_json::Value = serde_json::from_str(&aar_reply(&none, "abc")).unwrap();
    assert!(v["cue"].is_null());
}

#[tokio::test(flavor = "multi_thread")]
async fn csrf_guard_in_front_of_cookie_posts() {
    let h = harness();
    let admin = h.session(true);
    // a hostile page riding the admin's cookie (the CORS layer refuses the
    // Origin too; either way it never reaches the handler)
    let r = h
        .post_json(
            "/api/admin/merge-rounds",
            Some(&admin),
            Some("https://evil.example"),
            "{}",
        )
        .await;
    assert_eq!(r.status(), 403);
    // no Origin but a hostile Referer: only the CSRF guard catches this one
    let r = warp::test::request()
        .method("POST")
        .path("/api/admin/merge-rounds")
        .remote_addr(peer())
        .header("cookie", &admin)
        .header("referer", "https://evil.example/page")
        .header("content-type", "application/json")
        .body("{}")
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 403);
    assert!(body_json(r)["error"].as_str().unwrap().contains("cross-site"));
    // the dashboard itself
    let r = h.post_json("/api/admin/merge-rounds", Some(&admin), Some(DASH), "{}").await;
    assert_eq!(r.status(), 200);
    // the API's own public origin (a page bfdb serves itself)
    let r = warp::test::request()
        .method("POST")
        .path("/api/admin/merge-rounds")
        .remote_addr(peer())
        .header("cookie", &admin)
        .header("origin", API)
        .header("host", "api.example.org")
        .header("content-type", "application/json")
        .body("{}")
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 200);
    // a form post (no preflight) is refused even from an allowed origin
    let r = warp::test::request()
        .method("POST")
        .path("/api/admin/merge-rounds")
        .remote_addr(peer())
        .header("cookie", &admin)
        .header("origin", DASH)
        .header("content-type", "application/x-www-form-urlencoded")
        .body("a=b")
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 403);
    // a bot with no Origin/Referer is not a browser page and is let through
    let r = h.post_json("/api/admin/merge-rounds", Some(&admin), None, "{}").await;
    assert_eq!(r.status(), 200);
    // a WebSocket upgrade from a hostile page is refused before the route
    let r = warp::test::request()
        .method("GET")
        .path("/ws/logs")
        .remote_addr(peer())
        .header("cookie", &admin)
        .header("origin", "https://evil.example")
        .header("connection", "upgrade")
        .header("upgrade", "websocket")
        .header("sec-websocket-version", "13")
        .header("sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ==")
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 403);
}

#[tokio::test(flavor = "multi_thread")]
async fn unknown_instance_is_a_400_on_gets_and_posts() {
    let h = harness_with(
        serde_json::json!({ "instances": [{ "id": "a" }, { "id": "b", "export_port": 9001 }] }),
        |_| (),
    );
    let r = h.get("/api/rounds?instance=nope", None).await;
    assert_eq!(r.status(), 400);
    let admin = h.session(true);
    let r = h
        .post_json("/api/admin/reset?instance=nope", Some(&admin), Some(DASH), "{}")
        .await;
    assert_eq!(r.status(), 400);
    let r = h.get("/api/rounds?instance=b", None).await;
    assert_eq!(r.status(), 200);
}

#[tokio::test(flavor = "multi_thread")]
async fn admin_roles_split_the_admin_api() {
    let h = harness();
    let ops = h.session_with(false, admin::Role::OPS);
    let content = h.session_with(false, admin::Role::CONTENT);
    // the engine config editor is ops: past the gate there is no config here
    assert_eq!(h.get("/api/admin/cfg", Some(&ops)).await.status(), 404);
    assert_eq!(h.get("/api/admin/cfg", Some(&content)).await.status(), 403);
    // the ban list is moderation
    assert_eq!(h.get("/api/admin/banned", Some(&ops)).await.status(), 403);
    // wiki deletes are content
    let r = h
        .post_json("/api/wiki/delete", Some(&ops), Some(DASH), r#"{"slug":"nope"}"#)
        .await;
    assert_eq!(r.status(), 403);
    let r = h
        .post_json("/api/wiki/delete", Some(&content), Some(DASH), r#"{"slug":"nope"}"#)
        .await;
    assert_eq!(r.status(), 200);
    // full-admin only actions stay that way
    assert_eq!(h.get("/api/admin/sessions", Some(&ops)).await.status(), 403);
    assert_eq!(h.get("/api/admin/audit", Some(&content)).await.status(), 403);
}

#[tokio::test(flavor = "multi_thread")]
async fn admin_actions_land_in_the_audit_log() {
    let h = harness();
    let admin = h.session(true);
    let r = h.post_json("/api/admin/merge-rounds", Some(&admin), Some(DASH), "{}").await;
    assert_eq!(r.status(), 200);
    let r = h.get("/api/admin/audit", Some(&admin)).await;
    assert_eq!(r.status(), 200);
    let rows = body_json(r);
    let row = &rows.as_array().unwrap()[0];
    assert_eq!(row["action"], "merge rounds (dry run)");
    assert_eq!(row["username"], "tester");
}

#[tokio::test(flavor = "multi_thread")]
async fn public_gets_are_metered_per_ip() {
    let h =
        harness_with(serde_json::json!({ "instances": [{ "id": "default" }] }), |rc| {
            rc.get_limits = websec::GetLimits { per_min: 60, heavy_per_min: 0 };
        });
    let visitor = peer();
    let get = |addr: SocketAddr| {
        warp::test::request().method("GET").path("/api/version").remote_addr(addr)
    };
    let mut limited = None;
    for i in 0..80 {
        let r = get(visitor).reply(&h.routes).await;
        if r.status() == 429 {
            limited = Some(i);
            assert!(r.headers().contains_key("retry-after"));
            break;
        }
    }
    // the burst is a third of the minute's budget, but at least 60
    assert!(matches!(limited, Some(n) if (60..=62).contains(&n)), "{limited:?}");
    // someone else is unaffected, and so is the bot on this machine
    assert_eq!(get(peer()).reply(&h.routes).await.status(), 200);
    let lo: SocketAddr = "127.0.0.1:5555".parse().unwrap();
    for _ in 0..100 {
        assert_eq!(get(lo).reply(&h.routes).await.status(), 200);
    }
    // a visitor behind Cloudflare that bfdb was not told to trust cannot be
    // told apart from the others: not metered
    let cf =
        get(visitor).header("cf-connecting-ip", "198.51.100.1").reply(&h.routes).await;
    assert_eq!(cf.status(), 200);
}

#[tokio::test(flavor = "multi_thread")]
async fn public_health_keeps_internals_for_admins() {
    let h = harness();
    let r = h.get("/api/health", None).await;
    assert_eq!(r.status(), 200);
    let v = body_json(r);
    assert_eq!(v["engine"]["reachable"], false);
    assert!(v["engine"]["error"].is_string());
    for k in ["db", "build", "ingest"] {
        assert!(v.get(k).is_none(), "public /api/health shows {k}");
    }
    assert_eq!(h.get("/api/admin/health", None).await.status(), 401);
    let ops = h.session_with(false, admin::Role::OPS);
    let v = body_json(h.get("/api/admin/health", Some(&ops)).await);
    assert!(v["db"].is_object() && v["build"].is_object() && v["ingest"].is_object());
}

#[tokio::test(flavor = "multi_thread")]
async fn metrics_and_log_tokens() {
    let h =
        harness_with(serde_json::json!({ "instances": [{ "id": "default" }] }), |rc| {
            rc.log_read_token = Some("logs-secret".into());
            rc.metrics_token = Some("metrics-secret".into());
        });
    let get = |path: &str, addr: SocketAddr| {
        warp::test::request().method("GET").path(path).remote_addr(addr)
    };
    let me = peer();
    assert_eq!(
        get("/api/metrics?token=metrics-secret", me).reply(&h.routes).await.status(),
        200
    );
    assert_eq!(
        get("/api/metrics?token=logs-secret", me).reply(&h.routes).await.status(),
        200
    );
    let r = get("/api/metrics", me)
        .header("authorization", "Bearer metrics-secret")
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 200);
    // the metrics token does not read logs
    assert_eq!(
        get("/api/logs/bfdb?token=metrics-secret", me).reply(&h.routes).await.status(),
        401
    );
    assert_eq!(
        get("/api/logs/bfdb?token=logs-secret", me).reply(&h.routes).await.status(),
        200
    );
    // guessing gets an address locked out of both
    let guesser = peer();
    for _ in 0..21 {
        get("/api/metrics?token=nope", guesser).reply(&h.routes).await;
    }
    assert_eq!(
        get("/api/metrics?token=metrics-secret", guesser).reply(&h.routes).await.status(),
        429
    );
    assert_eq!(
        get("/api/metrics?token=metrics-secret", peer()).reply(&h.routes).await.status(),
        200
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_private_default_is_not_served_to_the_public() {
    // the private test server is the default, the public one is not
    let h = harness_with(
        serde_json::json!({
            "default": "test",
            "instances": [{ "id": "test", "public": false }, { "id": "live", "export_port": 9001 }],
        }),
        |_| (),
    );
    let t0 = chrono::Utc::now();
    for (rid, inst) in [(db::RoundId(1), "test"), (db::RoundId(2), "live")] {
        h.db.insert_test_round(rid, inst, t0);
    }
    let instances_of = |v: serde_json::Value| -> Vec<String> {
        v.as_array()
            .unwrap()
            .iter()
            .map(|r| r["instance"].as_str().unwrap().to_string())
            .collect()
    };
    // no ?instance=: the public gets the public campaign
    assert_eq!(instances_of(body_json(h.get("/api/rounds", None).await)), vec!["live"]);
    // naming the private one is still locked
    assert_eq!(h.get("/api/rounds?instance=test", None).await.status(), 404);
    // an admin gets the configured default either way
    let admin = h.session(true);
    assert_eq!(
        instances_of(body_json(h.get("/api/rounds", Some(&admin)).await)),
        vec!["test"]
    );

    // a box with nothing public: the implicit default is locked too
    let h = harness_with(
        serde_json::json!({ "instances": [{ "id": "test", "public": false }] }),
        |_| (),
    );
    assert_eq!(h.get("/api/rounds", None).await.status(), 404);
    assert_eq!(h.get("/api/version", None).await.status(), 200);
    let admin = h.session(true);
    assert_eq!(h.get("/api/rounds", Some(&admin)).await.status(), 200);
    // the bot on this machine is not locked out
    let r = warp::test::request()
        .method("GET")
        .path("/api/rounds")
        .remote_addr("127.0.0.1:4000".parse().unwrap())
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 200);
}

#[tokio::test(flavor = "multi_thread")]
async fn list_routes_page_with_limit_and_offset() {
    let h = harness();
    // wiki pages are seeded: dozens of them
    let all = h.get("/api/wiki/pages", None).await;
    let total: usize = all.headers()["x-total-count"].to_str().unwrap().parse().unwrap();
    let all = body_json(all);
    assert_eq!(all.as_array().unwrap().len(), total, "no ?limit= is everything");
    assert!(total > 10);
    let page = body_json(h.get("/api/wiki/pages?limit=5&offset=3", None).await);
    let page = page.as_array().unwrap();
    assert_eq!(page.len(), 5);
    assert_eq!(page[0], all[3]);
    assert_eq!(
        body_json(h.get("/api/wiki/pages?offset=100000", None).await),
        serde_json::json!([])
    );
    let r = h.get("/api/pilots?limit=1", None).await;
    assert_eq!(r.headers()["x-total-count"], "0");
}

#[tokio::test(flavor = "multi_thread")]
async fn security_headers_on_api_and_page() {
    let h = harness();
    let r = h.get("/api/version", None).await;
    assert_eq!(r.headers()["x-content-type-options"], "nosniff");
    assert!(r.headers().contains_key("content-security-policy"));
    let r = h.get("/", None).await;
    assert!(r.headers().contains_key("content-security-policy-report-only"));
}

// â”€â”€ HTTP stack (warp 0.4 / hyper 1): CORS, WebSockets, body limits â”€â”€â”€â”€â”€â”€

#[tokio::test(flavor = "multi_thread")]
async fn cors_preflight_and_simple_requests() {
    let h = harness();
    let preflight = |origin: &'static str| {
        warp::test::request()
            .method("OPTIONS")
            .path("/api/intel/upload")
            .remote_addr(peer())
            .header("origin", origin)
            .header("access-control-request-method", "POST")
            .header("access-control-request-headers", "content-type, x-intel-filename")
    };
    let r = preflight(DASH).reply(&h.routes).await;
    assert_eq!(r.status(), 200);
    let hd = r.headers();
    assert_eq!(hd["access-control-allow-origin"], DASH);
    assert_eq!(hd["access-control-allow-credentials"], "true");
    let methods =
        hd["access-control-allow-methods"].to_str().unwrap().to_ascii_uppercase();
    assert!(methods.contains("POST") && methods.contains("GET"), "{methods}");
    let allowed =
        hd["access-control-allow-headers"].to_str().unwrap().to_ascii_lowercase();
    assert!(
        allowed.contains("x-intel-filename") && allowed.contains("content-type"),
        "{allowed}"
    );

    // an origin that is not ours gets no CORS grant
    let r = preflight("https://evil.example").reply(&h.routes).await;
    assert_eq!(r.status(), 403);
    assert!(!r.headers().contains_key("access-control-allow-origin"));

    // a cross-origin GET: granted, and the paging header is readable
    let r = warp::test::request()
        .method("GET")
        .path("/api/pilots")
        .remote_addr(peer())
        .header("origin", DASH)
        .reply(&h.routes)
        .await;
    assert_eq!(r.status(), 200);
    assert_eq!(r.headers()["access-control-allow-origin"], DASH);
    let exposed = r.headers()["access-control-expose-headers"]
        .to_str()
        .unwrap()
        .to_ascii_lowercase();
    assert!(exposed.contains("x-total-count"), "{exposed}");
}

#[tokio::test(flavor = "multi_thread")]
async fn log_websocket_is_admin_only_and_replays_history() {
    let h =
        harness_with(serde_json::json!({ "instances": [{ "id": "default" }] }), |rc| {
            rc.log_history.lock().unwrap().push_back(r#"{"msg":"hello"}"#.to_string());
        });
    let admin = h.session(true);
    let mut ws = warp::test::ws()
        .path("/ws/logs")
        .header("cookie", admin.as_str())
        .handshake(h.routes.clone())
        .await
        .expect("an admin opens the log stream");
    let m = ws.recv().await.unwrap();
    assert_eq!(m.to_str().unwrap(), r#"{"msg":"hello"}"#);

    let player = h.session(false);
    let refused = warp::test::ws()
        .path("/ws/logs")
        .header("cookie", player.as_str())
        .handshake(h.routes.clone())
        .await;
    assert!(refused.is_err(), "a player must not get the log stream");
    let anon = warp::test::ws().path("/ws/logs").handshake(h.routes.clone()).await;
    assert!(anon.is_err());
}

#[tokio::test(flavor = "multi_thread")]
async fn json_bodies_over_the_limit_are_refused() {
    let h = harness();
    let admin = h.session(true);
    let big = format!(r#"{{"ucid":"{}"}}"#, "x".repeat(JSON_BODY_LIMIT as usize + 1));
    let r = h.post_json("/api/admin/ban", Some(&admin), Some(DASH), &big).await;
    assert_eq!(r.status(), 413);
}

#[tokio::test(flavor = "multi_thread")]
async fn cached_public_scans_keep_their_answers() {
    // f44: these now go through the stale-while-revalidate cache on the
    // blocking pool; statuses, bodies and paging must be what they were.
    let h = harness();
    for path in [
        "/api/rounds",
        "/api/captures",
        "/api/capture-events",
        "/api/aircraft-usage",
        "/api/online",
    ] {
        let r = h.get(path, None).await;
        assert_eq!(r.status(), 200, "{path}");
        assert_eq!(body_json(r), serde_json::json!([]), "{path}");
    }
    for path in [
        "/api/pilot/not-a-ucid",
        "/api/pilot/not-a-ucid/kills",
        "/api/pilot/not-a-ucid/sorties",
    ] {
        assert_eq!(h.get(path, None).await.status(), 400, "{path}");
    }
    let ucid = "0123456789abcdef0123456789abcdef";
    let r = h.get(&format!("/api/pilot/{ucid}/sorties?limit=5"), None).await;
    assert_eq!(r.status(), 200);
    assert_eq!(r.headers()["x-total-count"], "0");
    assert_eq!(body_json(r), serde_json::json!([]));
    let admin = h.session(true);
    assert_eq!(h.get("/api/trails", Some(&admin)).await.status(), 200);
    assert_eq!(h.get("/api/trails", None).await.status(), 401);
}

#[tokio::test(flavor = "multi_thread")]
async fn api_stats_carries_the_war_will() {
    // its own instance id: /api/stats answers are cached per instance for
    // the whole test process
    let h = harness_with(serde_json::json!({ "instances": [{ "id": "will" }] }), |_| ());
    // a round, but no war will from the engine: null
    h.db.ingest_test_stats(
        "will",
        vec![bfprotocols::stats::Stat::NewRound { sortie: "campaign".into() }],
    );
    let v = body_json(h.get("/api/stats", None).await);
    assert!(v.get("will").is_some_and(|w| w.is_null()), "{v}");

    let h = harness_with(serde_json::json!({ "instances": [{ "id": "will2" }] }), |_| ());
    let time = chrono::DateTime::parse_from_rfc3339("2026-10-31T12:00:00Z")
        .unwrap()
        .with_timezone(&chrono::Utc);
    h.db.ingest_test_stats(
        "will2",
        vec![
            bfprotocols::stats::Stat::NewRound { sortie: "campaign".into() },
            bfprotocols::stats::Stat::WarWill(bfprotocols::stats::WarWillStat {
                blue: 63.04,
                red: 18.,
                blue_trend: Some(-4.),
                red_trend: None,
                threshold: 20.,
                causes: vec![bfprotocols::stats::WillCause {
                    side: dcso3::coalition::Side::Blue,
                    text: "Carrier group hit".into(),
                    delta: -8.,
                    time,
                }],
            }),
        ],
    );
    let v = body_json(h.get("/api/stats", None).await);
    assert_eq!(
        v["will"],
        serde_json::json!({
            "blue": { "value": 63.0, "trend": -4.0 },
            "red": { "value": 18.0, "trend": null },
            "threshold": 20.0,
            "causes": [{
                "side": "Blue",
                "text": "Carrier group hit",
                "delta": -8.0,
                "time": "2026-10-31T12:00:00+00:00",
            }],
        })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn api_stats_carries_the_story() {
    // a round with no story event: null
    let h = harness_with(serde_json::json!({ "instances": [{ "id": "story" }] }), |_| ());
    h.db.ingest_test_stats(
        "story",
        vec![bfprotocols::stats::Stat::NewRound { sortie: "campaign".into() }],
    );
    let v = body_json(h.get("/api/stats", None).await);
    assert!(v.get("story").is_some_and(|s| s.is_null()), "{v}");

    let h =
        harness_with(serde_json::json!({ "instances": [{ "id": "story2" }] }), |_| ());
    let at = |s: &str| {
        chrono::DateTime::parse_from_rfc3339(s).unwrap().with_timezone(&chrono::Utc)
    };
    let ev = |id: &str, time: &str, side| bfprotocols::stats::StoryEventStat {
        id: id.into(),
        time: at(time),
        title: "Tuapse burns".into(),
        text: "The Tuapse refinery is burning.".into(),
        caused_by: vec!["Shade".into()],
        side,
        effects: vec!["Red fuel output -40%".into(), "Red will -6".into()],
    };
    h.db.ingest_test_stats(
        "story2",
        vec![
            bfprotocols::stats::Stat::NewRound { sortie: "campaign".into() },
            bfprotocols::stats::Stat::StoryEvent(ev(
                "moscow_terms",
                "2026-10-31T14:00:00Z",
                None,
            )),
            bfprotocols::stats::Stat::StoryEvent(ev(
                "tuapse_burns",
                "2026-10-31T12:00:00Z",
                Some(dcso3::coalition::Side::Red),
            )),
        ],
    );
    let v = body_json(h.get("/api/stats", None).await);
    assert_eq!(
        v["story"],
        serde_json::json!([
            {
                "id": "tuapse_burns",
                "time": "2026-10-31T12:00:00+00:00",
                "title": "Tuapse burns",
                "text": "The Tuapse refinery is burning.",
                "caused_by": ["Shade"],
                "side": "Red",
                "effects": ["Red fuel output -40%", "Red will -6"],
            },
            {
                "id": "moscow_terms",
                "time": "2026-10-31T14:00:00+00:00",
                "title": "Tuapse burns",
                "text": "The Tuapse refinery is burning.",
                "caused_by": ["Shade"],
                "side": null,
                "effects": ["Red fuel output -40%", "Red will -6"],
            },
        ])
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn api_oob_shows_losses_to_all_and_stock_to_those_who_may_see_it() {
    use bfprotocols::stats::{AirOobStat, AirframeCount, Stat};
    let h = harness_with(serde_json::json!({ "instances": [{ "id": "oob" }] }), |_| ());
    // a round, but nothing from the engine (ai_air_stock off): empty sides
    h.db.ingest_test_stats("oob", vec![Stat::NewRound { sortie: "campaign".into() }]);
    let v = body_json(h.get("/api/oob", None).await);
    assert_eq!(v["blue"], serde_json::json!([]), "{v}");
    assert_eq!(v["red"], serde_json::json!([]), "{v}");
    assert!(v["round"].is_u64(), "{v}");

    let count = |typ: &str, stock, lost, unlimited| AirframeCount {
        typ: typ.into(),
        stock,
        lost,
        unlimited,
    };
    // each ingest starts afresh, so the round comes along again
    let h = harness_with(serde_json::json!({ "instances": [{ "id": "oob2" }] }), |_| ());
    h.db.ingest_test_stats(
        "oob2",
        vec![
            Stat::NewRound { sortie: "campaign".into() },
            Stat::AirOrderOfBattle(AirOobStat::default()),
            Stat::AirOrderOfBattle(AirOobStat {
                blue: vec![count("F-14A", 12, 3, false), count("F-16C_50", 20, 0, true)],
                red: vec![count("Tu-22M3", 0, 6, false), count("MiG-29S", 14, 2, false)],
            }),
        ],
    );
    // anonymous: losses only
    let v = body_json(h.get("/api/oob", None).await);
    assert_eq!(
        v["red"],
        serde_json::json!([
            { "type": "MiG-29S", "stock": null, "lost": 2, "unlimited": false },
            { "type": "Tu-22M3", "stock": null, "lost": 6, "unlimited": false },
        ])
    );
    assert_eq!(v["blue"][0]["stock"], serde_json::Value::Null);
    // an admin with no side of their own sees both, most aircraft first
    let admin = h.session(true);
    let v = body_json(h.get("/api/oob", Some(&admin)).await);
    assert_eq!(
        v["blue"],
        serde_json::json!([
            { "type": "F-16C_50", "stock": 20, "lost": 0, "unlimited": true },
            { "type": "F-14A", "stock": 12, "lost": 3, "unlimited": false },
        ])
    );
    assert_eq!(
        v["red"][1],
        serde_json::json!({ "type": "Tu-22M3", "stock": 0, "lost": 6, "unlimited": false })
    );
    // a round the engine sent nothing for: empty
    let v = body_json(h.get("/api/oob?round=999999", Some(&admin)).await);
    assert_eq!(
        (v["blue"].clone(), v["red"].clone()),
        (serde_json::json!([]), serde_json::json!([]))
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn weekly_report_has_the_board_the_week_and_the_last_stand() {
    use bfprotocols::{
        db::objective::{ObjectiveId, ObjectiveKind},
        stats::{Stat, StoryEventStat},
    };
    use dcso3::coalition::Side;
    // a round with nothing in it yet
    let h = harness_with(
        serde_json::json!({ "instances": [{
            "id": "weekly", "theme": "redstorm", "brand_name": "Southern Flank 1985",
            "blue_faction": "NATO", "red_faction": "USSR",
        }] }),
        |_| (),
    );
    // (answers are cached for a few seconds per query: each call below asks
    // a different one)
    let v = body_json(h.get("/api/report/weekly?days=2", None).await);
    assert_eq!(v["report"], serde_json::Value::Null, "no round, no report: {v}");
    assert_eq!(v["theme"], "redstorm");
    assert_eq!(v["blue_label"], "NATO");
    assert_eq!(v["days"], 2);

    let pilot: dcso3::net::Ucid = "01234567890abcdef0123456789abcdf".parse().unwrap();
    let pos = dcso3::coord::LLPos { latitude: 41.6, longitude: 41.6, altitude: 0. };
    let obj = |id: u32, name: &str, owner, kind| Stat::Objective {
        name: name.into(),
        id: ObjectiveId::from(id as i64),
        pos,
        owner,
        kind,
    };
    let story = |id: &str, effects: Vec<dcso3::String>, side| {
        Stat::StoryEvent(StoryEventStat {
            id: id.into(),
            time: chrono::Utc::now() - chrono::Duration::hours(1),
            title: format!("{id} title").into(),
            text: "".into(),
            caused_by: vec!["Shade".into()],
            side,
            effects,
        })
    };
    h.db.ingest_test_stats(
        "weekly",
        vec![
            Stat::NewRound { sortie: "campaign".into() },
            Stat::Register {
                name: "Shade".into(),
                id: pilot,
                side: Side::Blue,
                initial_points: 100,
            },
            obj(1, "Batumi", Side::Blue, ObjectiveKind::Airbase),
            obj(2, "Sochi", Side::Red, ObjectiveKind::Airbase),
            obj(3, "SA-10 Gudauta", Side::Red, ObjectiveKind::SpecialSamSite),
            obj(
                4,
                "Moskva group",
                Side::Red,
                ObjectiveKind::CarrierGroup {
                    carrier_template: "".into(),
                    waypoint: None,
                    parent_naval_base: None,
                    repair_start_time: None,
                },
            ),
            Stat::Capture {
                id: ObjectiveId::from(2),
                by: smallvec::smallvec![pilot],
                side: Side::Blue,
            },
            story(
                "collapse",
                vec!["Red orders a nuclear strike".into()],
                Some(Side::Red),
            ),
            story("last_stand_intercepted", vec![], Some(Side::Red)),
        ],
    );
    let v = body_json(h.get("/api/report/weekly", None).await);
    assert_eq!(v["days"], 7);
    let r = &v["report"];
    assert_eq!(v["round_number"], 1, "{v}");
    assert_eq!(v["round_active"], true);
    assert_eq!(r["forces"]["blue"]["airfields"], 2, "Sochi was taken: {r}");
    assert_eq!(r["forces"]["blue"]["captured"], 1);
    assert_eq!(r["forces"]["red"]["sam_sites"], 1);
    assert_eq!(r["forces"]["red"]["fleets"], serde_json::json!(["Moskva group"]));
    assert_eq!(r["forces"]["red"]["objectives"], 1, "the SAM site is not counted");
    assert_eq!(r["capture_count"], 1);
    assert_eq!(r["captures"][0]["objective"], "Sochi");
    assert_eq!(r["captures"][0]["by"], serde_json::json!(["Shade"]));
    assert_eq!(r["story"].as_array().map(|a| a.len()), Some(2));
    assert_eq!(r["last_stand"]["outcome"], "intercepted");
    assert_eq!(r["last_stand"]["this_week"], true);
    assert_eq!(r["pilots"][0]["name"], "Shade");
    assert_eq!(r["pilots"][0]["side"], "Blue");
    assert_eq!(r["pilots"][0]["score"], 30);
    assert_eq!(r["active_pilots"], 1);

    // a window that ends before any of it: an empty week on the same board
    let v = body_json(
        h.get("/api/report/weekly?days=1&until=2020-01-01T00:00:00Z", None).await,
    );
    assert_eq!(v["report"]["capture_count"], 0, "{v}");
    assert_eq!(v["report"]["last_stand"]["this_week"], false);
    assert_eq!(v["report"]["forces"]["blue"]["airfields"], 2);
    assert_eq!(h.get("/api/report/weekly?until=yesterday", None).await.status(), 400);
}
