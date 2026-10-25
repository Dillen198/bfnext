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
pub(crate) fn harness_with(instances: serde_json::Value, tweak: impl FnOnce(&mut RouteCtx)) -> Harness {
    let dir = std::env::temp_dir().join(format!("bfdb-route-test-{}", Uuid::new_v4()));
    std::fs::create_dir_all(&dir).unwrap();
    let file: instance::InstancesFile = serde_json::from_value(instances).unwrap();
    let reg = Registry::new(file).unwrap();
    let db = StatsDb::new(&std::collections::HashMap::new(), dir.join("db"), reg, None, None).unwrap();
    let bot: Arc<Option<BotLinkConfig>> = Arc::new(None);
    let range_ctx = range::RangeCtx::new(db.clone(), bot.clone(), "https://range.example.org", API, dir.join("clips")).unwrap();
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
        shutdown_token: None,
    };
    tweak(&mut rc);
    let routes = build_routes(rc);
    Harness { db, routes, dir }
}

impl Harness {
    /// A logged-in session; returns its cookie header value.
    pub(crate) fn session(&self, admin: bool) -> String {
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

    pub(crate) async fn post_json(&self, path: &str, cookie: Option<&str>, origin: Option<&str>, body: &str) -> TestResp {
        let mut r = warp::test::request()
            .method("POST")
            .path(path)
            .remote_addr(peer())
            .header("content-type", "application/json")
            .body(body.to_string());
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
    r.headers().get("content-type").and_then(|v| v.to_str().ok()).unwrap_or("").to_string()
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
    let r = h.get("/api/admin/sessions", Some(&format!("session={}", Uuid::new_v4()))).await;
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
    // a browser session with no Discord link is told so
    let player = h.session(false);
    let r = h.get("/api/cockpit/context", Some(&player)).await;
    assert_eq!(r.status(), 403);
}

#[tokio::test(flavor = "multi_thread")]
async fn csrf_guard_in_front_of_cookie_posts() {
    let h = harness();
    let admin = h.session(true);
    // a hostile page riding the admin's cookie (the CORS layer refuses the
    // Origin too; either way it never reaches the handler)
    let r = h.post_json("/api/admin/merge-rounds", Some(&admin), Some("https://evil.example"), "{}").await;
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
    let r = h.post_json("/api/admin/reset?instance=nope", Some(&admin), Some(DASH), "{}").await;
    assert_eq!(r.status(), 400);
    let r = h.get("/api/rounds?instance=b", None).await;
    assert_eq!(r.status(), 200);
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
