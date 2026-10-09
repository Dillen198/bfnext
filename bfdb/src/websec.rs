//! Web-layer hardening shared by every route: typed HTTP errors, the
//! cross-site request guard, per-IP rate limiting, security headers, upload
//! sniffing and a small single-flight cache.
//!
//! Kept out of main.rs so the policy lives in one place a reviewer can read
//! top to bottom, instead of being smeared across a 7000-line route file.

use std::{
    collections::HashMap,
    hash::Hash,
    net::{IpAddr, SocketAddr},
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};
use warp::{
    http::{HeaderValue, Method, StatusCode},
    reply::Response,
    Filter,
};

// ── Typed errors ─────────────────────────────────────────────────────────────

/// An error that knows which HTTP status it is and whose message is safe to
/// show the caller. Anything else a handler returns is treated as internal:
/// the caller gets a generic message and a reference, the log gets the detail.
#[derive(Debug)]
pub(crate) struct ApiError {
    pub(crate) status: StatusCode,
    pub(crate) msg: std::string::String,
}

impl std::fmt::Display for ApiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

impl std::error::Error for ApiError {}

fn api(status: StatusCode, msg: impl Into<std::string::String>) -> anyhow::Error {
    anyhow::Error::new(ApiError { status, msg: msg.into() })
}

/// 400: the request itself is wrong.
pub(crate) fn bad_request(msg: impl Into<std::string::String>) -> anyhow::Error {
    api(StatusCode::BAD_REQUEST, msg)
}

/// 401: not logged in / not identified.
pub(crate) fn unauthorized(msg: impl Into<std::string::String>) -> anyhow::Error {
    api(StatusCode::UNAUTHORIZED, msg)
}

/// 403: identified, but not allowed.
pub(crate) fn forbidden(msg: impl Into<std::string::String>) -> anyhow::Error {
    api(StatusCode::FORBIDDEN, msg)
}

/// 404.
pub(crate) fn not_found(msg: impl Into<std::string::String>) -> anyhow::Error {
    api(StatusCode::NOT_FOUND, msg)
}

/// 415.
pub(crate) fn unsupported_media(msg: impl Into<std::string::String>) -> anyhow::Error {
    api(StatusCode::UNSUPPORTED_MEDIA_TYPE, msg)
}

/// 422: the game engine understood the request and refused it ("you must be in
/// a slot"). The engine's own words are meant for the player.
pub(crate) fn engine_refused(msg: impl Into<std::string::String>) -> anyhow::Error {
    api(StatusCode::UNPROCESSABLE_ENTITY, msg)
}

/// 429.
pub(crate) fn too_many(msg: impl Into<std::string::String>) -> anyhow::Error {
    api(StatusCode::TOO_MANY_REQUESTS, msg)
}

/// 503: a dependency (the engine, DCSServerBot) is not answering.
pub(crate) fn unavailable(msg: impl Into<std::string::String>) -> anyhow::Error {
    api(StatusCode::SERVICE_UNAVAILABLE, msg)
}

/// Turn any handler error into a response. A typed [`ApiError`] is shown as
/// is; anything else is logged in full under a short reference and answered
/// with a 500 that carries only the reference -- an anyhow chain names file
/// paths, sled internals and serde positions nobody outside should see.
pub(crate) fn error_response(e: &anyhow::Error) -> Response {
    let (status, body) = match e.downcast_ref::<ApiError>() {
        Some(a) => (a.status, serde_json::json!({ "error": a.msg })),
        None => {
            let reference = &uuid::Uuid::new_v4().simple().to_string()[..8];
            log::warn!("request failed [ref {reference}]: {e:?}");
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                serde_json::json!({
                    "error": format!("internal error (ref {reference})"),
                    "ref": reference,
                }),
            )
        }
    };
    let mut r = warp::reply::with_status(warp::reply::json(&body), status).into_response();
    r.headers_mut().insert("cache-control", HeaderValue::from_static("no-store"));
    r
}

use warp::Reply;

// ── Small helpers ────────────────────────────────────────────────────────────

/// Constant-time equality for secrets. The length is not hidden (it is not
/// the secret), the content is.
pub(crate) fn ct_eq(a: &str, b: &str) -> bool {
    a.len() == b.len() && a.bytes().zip(b.bytes()).fold(0u8, |acc, (x, y)| acc | (x ^ y)) == 0
}

/// An IP or CIDR block, as given on the command line.
#[derive(Debug, Clone)]
pub(crate) struct IpNet {
    addr: IpAddr,
    prefix: u8,
}

impl std::str::FromStr for IpNet {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        let (a, p) = match s.split_once('/') {
            Some((a, p)) => (a, Some(p)),
            None => (s, None),
        };
        let addr: IpAddr = a.trim().parse().map_err(|e| anyhow::anyhow!("bad ip {a:?}: {e}"))?;
        let max = if addr.is_ipv4() { 32 } else { 128 };
        let prefix = match p {
            Some(p) => p.trim().parse::<u8>().map_err(|e| anyhow::anyhow!("bad prefix {p:?}: {e}"))?,
            None => max,
        };
        if prefix > max {
            anyhow::bail!("prefix /{prefix} too long for {addr}");
        }
        Ok(Self { addr, prefix })
    }
}

impl IpNet {
    pub(crate) fn contains(&self, ip: IpAddr) -> bool {
        // An IPv4 client can arrive as an IPv4-mapped IPv6 address on a
        // dual-stack listener.
        let ip = match ip {
            IpAddr::V6(v6) => v6.to_ipv4_mapped().map(IpAddr::V4).unwrap_or(ip),
            v4 => v4,
        };
        match (self.addr, ip) {
            (IpAddr::V4(n), IpAddr::V4(i)) => {
                let mask = if self.prefix == 0 { 0 } else { u32::MAX << (32 - self.prefix as u32) };
                u32::from(n) & mask == u32::from(i) & mask
            }
            (IpAddr::V6(n), IpAddr::V6(i)) => {
                let mask = if self.prefix == 0 { 0 } else { u128::MAX << (128 - self.prefix as u32) };
                u128::from(n) & mask == u128::from(i) & mask
            }
            _ => false,
        }
    }
}

fn is_loopback(ip: IpAddr) -> bool {
    match ip {
        IpAddr::V6(v6) => v6.is_loopback() || v6.to_ipv4_mapped().map_or(false, |v4| v4.is_loopback()),
        IpAddr::V4(v4) => v4.is_loopback(),
    }
}

/// Who is really calling. bfdb sits behind Caddy on the same box, so every
/// proxied request arrives from loopback with the real client in
/// `X-Forwarded-For` -- which Caddy overwrites with the address it saw unless
/// it is itself configured to trust an upstream proxy. The header is only
/// believed when the peer is loopback (i.e. it came through our own proxy);
/// from anyone else it is ignored, since they could write anything there.
/// The rightmost entry is the one our proxy added.
pub(crate) fn client_ip(peer: Option<SocketAddr>, xff: Option<&str>) -> IpAddr {
    let peer_ip = peer.map(|p| p.ip()).unwrap_or(IpAddr::from([127, 0, 0, 1]));
    if is_loopback(peer_ip) {
        if let Some(ip) = xff
            .and_then(|h| h.rsplit(',').next())
            .and_then(|s| s.trim().parse::<IpAddr>().ok())
        {
            return ip;
        }
    }
    peer_ip
}

/// A filter extracting [`client_ip`].
pub(crate) fn with_client_ip(
) -> impl Filter<Extract = (IpAddr,), Error = warp::Rejection> + Clone {
    warp::addr::remote()
        .and(warp::header::optional::<std::string::String>("x-forwarded-for"))
        .map(|peer: Option<SocketAddr>, xff: Option<std::string::String>| client_ip(peer, xff.as_deref()))
}

/// True when the request came straight from this machine, not through the
/// proxy -- the only callers allowed at the shutdown endpoint.
pub(crate) fn with_direct_loopback(
) -> impl Filter<Extract = (bool,), Error = warp::Rejection> + Clone {
    warp::addr::remote()
        .and(warp::header::optional::<std::string::String>("x-forwarded-for"))
        .map(|peer: Option<SocketAddr>, xff: Option<std::string::String>| {
            peer.map_or(false, |p| is_loopback(p.ip())) && xff.is_none()
        })
}

// ── WebSocket limits ─────────────────────────────────────────────────────────

/// Open WebSockets per client IP, across every `/ws/*` route. A dashboard tab
/// holds two or three; a household behind one address a few tabs each.
pub(crate) const MAX_WS_PER_IP: usize = 24;

/// Largest message a client may send us. Clients only ever send close/pong
/// frames, so this is about refusing tungstenite's 64 MiB default buffer.
pub(crate) const WS_MAX_MESSAGE: usize = 64 * 1024;

static WS_OPEN: std::sync::LazyLock<Mutex<HashMap<IpAddr, usize>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// One counted WebSocket; the count drops when this does.
pub(crate) struct WsSlot(IpAddr);

impl Drop for WsSlot {
    fn drop(&mut self) {
        let mut g = WS_OPEN.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(n) = g.get_mut(&self.0) {
            *n = n.saturating_sub(1);
            if *n == 0 {
                g.remove(&self.0);
            }
        }
    }
}

/// Claim a WebSocket slot for `ip`, or `None` when it already has
/// [`MAX_WS_PER_IP`] open.
pub(crate) fn ws_slot(ip: IpAddr) -> Option<WsSlot> {
    let mut g = WS_OPEN.lock().unwrap_or_else(|e| e.into_inner());
    let n = g.entry(ip).or_insert(0);
    if *n >= MAX_WS_PER_IP {
        return None;
    }
    *n += 1;
    Some(WsSlot(ip))
}

/// `warp::ws()` with small message limits, plus the caller's IP.
pub(crate) fn ws_limited(
) -> impl Filter<Extract = (warp::ws::Ws, IpAddr), Error = warp::Rejection> + Clone {
    warp::ws()
        .map(|ws: warp::ws::Ws| ws.max_message_size(WS_MAX_MESSAGE).max_frame_size(WS_MAX_MESSAGE))
        .and(with_client_ip())
}

/// The response for a WebSocket we will not open.
pub(crate) fn ws_refused(status: StatusCode, why: &'static str) -> Response {
    warp::reply::with_status(why, status).into_response()
}

// ── Rate limiting ────────────────────────────────────────────────────────────

/// A fixed-window counter per key (usually a client IP) with a lockout once
/// the budget is spent. Deliberately simple: it only has to make password and
/// key guessing hopeless and stop one client flooding an expensive route.
pub(crate) struct RateLimiter<K> {
    max: u32,
    window: Duration,
    lockout: Duration,
    state: Mutex<HashMap<K, (u32, Instant, Option<Instant>)>>,
}

impl<K: Hash + Eq + Clone> RateLimiter<K> {
    pub(crate) fn new(max: u32, window: Duration, lockout: Duration) -> Self {
        Self { max, window, lockout, state: Mutex::new(HashMap::new()) }
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, HashMap<K, (u32, Instant, Option<Instant>)>> {
        self.state.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// Whether `key` is currently locked out, and for how much longer.
    pub(crate) fn blocked(&self, key: &K) -> Option<Duration> {
        let now = Instant::now();
        let g = self.lock();
        match g.get(key) {
            Some((_, _, Some(until))) if *until > now => Some(*until - now),
            _ => None,
        }
    }

    /// Count one event (a failure, or a request -- whatever this limiter
    /// meters). Returns false if that exhausted the budget.
    pub(crate) fn hit(&self, key: &K) -> bool {
        let now = Instant::now();
        let mut g = self.lock();
        // Bound memory: a flood of distinct keys must not grow this forever.
        if g.len() > 50_000 {
            g.retain(|_, (_, start, until)| {
                now.duration_since(*start) < self.window || until.map_or(false, |u| u > now)
            });
        }
        let e = g.entry(key.clone()).or_insert((0, now, None));
        if let Some(until) = e.2 {
            if until > now {
                return false;
            }
            *e = (0, now, None);
        }
        if now.duration_since(e.1) >= self.window {
            *e = (0, now, None);
        }
        e.0 = e.0.saturating_add(1);
        if e.0 > self.max {
            e.2 = Some(now + self.lockout);
            return false;
        }
        true
    }

    /// Forget `key` (e.g. after a successful login).
    pub(crate) fn clear(&self, key: &K) {
        self.lock().remove(key);
    }
}

// ── Cross-site request guard ─────────────────────────────────────────────────

/// Origins allowed to drive a cookie-authenticated request.
#[derive(Debug, Clone)]
pub(crate) struct OriginPolicy {
    /// `scheme://host[:port]`, lowercased, no trailing slash: the configured
    /// `--cors-origin`s plus `--public-api-url`.
    allowed: Arc<Vec<std::string::String>>,
}

pub(crate) fn origin_of(url: &str) -> Option<std::string::String> {
    let (scheme, rest) = url.split_once("://")?;
    let host = rest.split(['/', '?', '#']).next()?;
    if host.is_empty() {
        return None;
    }
    Some(format!("{}://{}", scheme.to_ascii_lowercase(), host.to_ascii_lowercase()))
}

fn host_of_origin(origin: &str) -> &str {
    origin.split_once("://").map(|(_, h)| h).unwrap_or(origin)
}

impl OriginPolicy {
    pub(crate) fn new<'a>(origins: impl IntoIterator<Item = &'a str>) -> Self {
        let allowed = origins.into_iter().filter_map(origin_of).collect();
        Self { allowed: Arc::new(allowed) }
    }

    /// Whether a browser at `origin` may act on a user's cookie here. The
    /// API's own origin always may -- judged by the `Host` it was addressed
    /// to, since TLS terminates at the proxy and the scheme bfdb sees is not
    /// the one the browser used.
    pub(crate) fn allows(&self, origin: &str, host: Option<&str>) -> bool {
        let Some(o) = origin_of(origin) else { return false };
        if self.allowed.iter().any(|a| *a == o) {
            return true;
        }
        match host {
            Some(h) => host_of_origin(&o).eq_ignore_ascii_case(h.trim()),
            None => false,
        }
    }
}

fn has_session_cookie(cookie: Option<&str>) -> bool {
    cookie.map_or(false, |c| c.split(';').any(|p| p.trim().starts_with("session=")))
}

/// Why a request was refused by [`csrf_verdict`], or `None` to let it through.
///
/// Applies only to requests that carry the session cookie -- that is the
/// ambient credential a hostile page could borrow. Bearer-token and API-key
/// callers, and anonymous ones, have nothing to steal.
///
/// * State-changing methods and WebSocket upgrades must come from an allowed
///   Origin (or, if a browser sent no Origin, an allowed Referer). A request
///   with neither is not from a browser page at all -- DCSServerBot, curl --
///   and is let through: a cross-site attacker cannot strip both.
/// * A cookie-authenticated POST may not use the three "simple" content types
///   a plain HTML form can send without a CORS preflight. Belt and braces for
///   the body-less admin actions, which otherwise read nothing a form could
///   get wrong.
pub(crate) fn csrf_verdict(
    policy: &OriginPolicy,
    method: &Method,
    upgrade: Option<&str>,
    cookie: Option<&str>,
    origin: Option<&str>,
    referer: Option<&str>,
    host: Option<&str>,
    content_type: Option<&str>,
) -> Option<&'static str> {
    if !has_session_cookie(cookie) {
        return None;
    }
    let is_ws = upgrade.map_or(false, |u| u.eq_ignore_ascii_case("websocket"));
    let unsafe_method = matches!(*method, Method::POST | Method::PUT | Method::PATCH | Method::DELETE);
    if !is_ws && !unsafe_method {
        return None;
    }
    let from = origin.filter(|o| !o.is_empty()).or(referer.filter(|r| !r.is_empty()));
    if let Some(from) = from {
        if !policy.allows(from, host) {
            return Some("cross-site request refused (origin not allowed)");
        }
    }
    if unsafe_method {
        if let Some(ct) = content_type {
            let ct = ct.split(';').next().unwrap_or("").trim().to_ascii_lowercase();
            if matches!(
                ct.as_str(),
                "text/plain" | "application/x-www-form-urlencoded" | "multipart/form-data"
            ) {
                return Some("form-encoded requests are not accepted here; send application/json");
            }
        }
    }
    None
}

/// A front-of-chain filter: answers 403 for a request [`csrf_verdict`]
/// refuses, and rejects (i.e. lets the route chain carry on) otherwise.
pub(crate) fn csrf_guard(
    policy: OriginPolicy,
) -> impl Filter<Extract = (Response,), Error = warp::Rejection> + Clone {
    use warp::header::optional as h;
    warp::method()
        .and(h::<std::string::String>("upgrade"))
        .and(h::<std::string::String>("cookie"))
        .and(h::<std::string::String>("origin"))
        .and(h::<std::string::String>("referer"))
        .and(h::<std::string::String>("host"))
        .and(h::<std::string::String>("content-type"))
        .and_then(
            move |method: Method,
                  upgrade: Option<std::string::String>,
                  cookie: Option<std::string::String>,
                  origin: Option<std::string::String>,
                  referer: Option<std::string::String>,
                  host: Option<std::string::String>,
                  ctype: Option<std::string::String>| {
                let verdict = csrf_verdict(
                    &policy,
                    &method,
                    upgrade.as_deref(),
                    cookie.as_deref(),
                    origin.as_deref(),
                    referer.as_deref(),
                    host.as_deref(),
                    ctype.as_deref(),
                );
                async move {
                    match verdict {
                        None => Err(warp::reject::reject()),
                        Some(why) => {
                            log::warn!(
                                "refused {method} (origin={:?} referer={:?}): {why}",
                                origin.as_deref().unwrap_or("-"),
                                referer.as_deref().unwrap_or("-")
                            );
                            Ok(error_response(&forbidden(why)))
                        }
                    }
                }
            },
        )
}

// ── Security headers ─────────────────────────────────────────────────────────

/// The embedded SPA's Content-Security-Policy. Shipped as Report-Only: the
/// dashboard pulls map tiles, glyphs and fonts from several third-party hosts
/// and maplibre needs blob: workers, and a policy that is one host short
/// blanks the TACMAP. Watch the browser console for violations, then promote
/// it to an enforced header.
const SPA_CSP: &str = "default-src 'self'; \
     script-src 'self'; \
     style-src 'self' 'unsafe-inline' https://fonts.googleapis.com; \
     font-src 'self' data: https://fonts.gstatic.com; \
     img-src 'self' data: blob: https:; \
     connect-src 'self' https: wss: ws:; \
     worker-src 'self' blob:; \
     child-src 'self' blob:; \
     object-src 'none'; \
     base-uri 'self'; \
     frame-ancestors 'none'";

/// Add the baseline headers to any response that does not already carry its
/// own. A route that needs something different (uploads served with
/// `sandbox`) sets it itself and is left alone.
pub(crate) fn harden(mut r: Response) -> Response {
    let ctype = r
        .headers()
        .get("content-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("")
        .to_ascii_lowercase();
    let is_html = ctype.starts_with("text/html");
    // Scripts and styles are left alone: a CSP on a script response governs
    // it when it runs as a Web Worker, and 'none' would cut the worker off.
    let is_code = ctype.contains("javascript") || ctype.starts_with("text/css");
    let h = r.headers_mut();
    let mut default = |name: &'static str, value: &'static str| {
        if !h.contains_key(name) {
            h.insert(name, HeaderValue::from_static(value));
        }
    };
    default("x-content-type-options", "nosniff");
    default("x-frame-options", "DENY");
    default("referrer-policy", "strict-origin-when-cross-origin");
    if is_html {
        default("content-security-policy-report-only", SPA_CSP);
    } else if !is_code {
        // Nothing the API returns is meant to be rendered as a page. If a
        // browser is ever talked into doing so anyway, it gets no script, no
        // plugins and no framing.
        default("content-security-policy", "default-src 'none'; frame-ancestors 'none'; sandbox");
    }
    r
}

// ── Upload sniffing ──────────────────────────────────────────────────────────

/// The image type `bytes` actually is, judged by magic number -- never by the
/// uploader's Content-Type, which is how an `image/svg+xml` full of script
/// used to get stored and served straight back. Only raster formats a browser
/// cannot execute are accepted.
pub(crate) fn sniff_image(bytes: &[u8]) -> Option<&'static str> {
    if bytes.starts_with(b"\x89PNG\r\n\x1a\n") {
        Some("image/png")
    } else if bytes.starts_with(&[0xFF, 0xD8, 0xFF]) {
        Some("image/jpeg")
    } else if bytes.len() >= 12 && &bytes[..4] == b"RIFF" && &bytes[8..12] == b"WEBP" {
        Some("image/webp")
    } else {
        None
    }
}

/// Serve stored upload bytes: re-sniffed (rows written before sniffing
/// existed may say anything), locked down so that even a hostile file cannot
/// run as a page.
pub(crate) fn serve_upload(bytes: Vec<u8>, cache_control: &'static str) -> Response {
    let ctype = sniff_image(&bytes).unwrap_or("application/octet-stream");
    let mut r = Response::new(bytes.into());
    let h = r.headers_mut();
    h.insert("content-type", HeaderValue::from_static(ctype));
    h.insert("cache-control", HeaderValue::from_static(cache_control));
    h.insert("x-content-type-options", HeaderValue::from_static("nosniff"));
    h.insert("content-security-policy", HeaderValue::from_static("default-src 'none'; sandbox"));
    if ctype == "application/octet-stream" {
        h.insert("content-disposition", HeaderValue::from_static("attachment"));
    }
    r
}

/// A markup colour: `#rgb`, `#rgba`, `#rrggbb` or `#rrggbbaa`. It ends up in
/// HTML on every coalition member's map, so nothing else is let in.
pub(crate) fn valid_hex_color(s: &str) -> bool {
    let Some(hex) = s.strip_prefix('#') else { return false };
    matches!(hex.len(), 3 | 4 | 6 | 8) && hex.bytes().all(|b| b.is_ascii_hexdigit())
}

// ── Single-flight cache ──────────────────────────────────────────────────────

/// One cached value that many concurrent requests share: while it is being
/// refreshed every other caller waits for that refresh instead of starting
/// its own. Used for public routes that would otherwise fire an engine RPC
/// (run inside the DCS frame) or a full table scan per request.
pub(crate) struct Cached<T> {
    slot: tokio::sync::Mutex<Option<(Instant, T)>>,
}

impl<T: Clone> Cached<T> {
    pub(crate) fn new() -> Self {
        Self { slot: tokio::sync::Mutex::new(None) }
    }

    pub(crate) async fn get_or_refresh<F, Fut, E>(&self, ttl: Duration, f: F) -> Result<T, E>
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = Result<T, E>>,
    {
        let mut g = self.slot.lock().await;
        if let Some((at, v)) = &*g {
            if at.elapsed() < ttl {
                return Ok(v.clone());
            }
        }
        let v = f().await?;
        *g = Some((Instant::now(), v.clone()));
        Ok(v)
    }
}

/// A keyed family of [`Cached`] values (per instance, per round, ...).
pub(crate) struct CacheMap<K, T> {
    map: Mutex<HashMap<K, Arc<Cached<T>>>>,
}

impl<K: Hash + Eq + Clone, T: Clone> CacheMap<K, T> {
    pub(crate) fn new() -> Self {
        Self { map: Mutex::new(HashMap::new()) }
    }

    pub(crate) fn entry(&self, key: &K) -> Arc<Cached<T>> {
        let mut g = self.map.lock().unwrap_or_else(|e| e.into_inner());
        // Keys are instances/rounds -- a handful -- but a caller-controlled
        // key (`?round=`) must not grow this without bound.
        if g.len() > 256 {
            g.clear();
        }
        g.entry(key.clone()).or_insert_with(|| Arc::new(Cached::new())).clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ipnet() {
        let n: IpNet = "10.0.0.0/8".parse().unwrap();
        assert!(n.contains("10.1.2.3".parse().unwrap()));
        assert!(!n.contains("11.1.2.3".parse().unwrap()));
        let one: IpNet = "127.0.0.1".parse().unwrap();
        assert!(one.contains("::ffff:127.0.0.1".parse().unwrap()));
        let any: IpNet = "0.0.0.0/0".parse().unwrap();
        assert!(any.contains("8.8.8.8".parse().unwrap()));
        assert!("1.2.3.4/33".parse::<IpNet>().is_err());
    }

    #[test]
    fn client_ip_trusts_xff_only_from_loopback() {
        let lo: SocketAddr = "127.0.0.1:5000".parse().unwrap();
        let ext: SocketAddr = "203.0.113.9:5000".parse().unwrap();
        assert_eq!(client_ip(Some(lo), Some("1.1.1.1, 198.51.100.7")), "198.51.100.7".parse::<IpAddr>().unwrap());
        assert_eq!(client_ip(Some(ext), Some("1.1.1.1")), "203.0.113.9".parse::<IpAddr>().unwrap());
        assert_eq!(client_ip(Some(lo), None), "127.0.0.1".parse::<IpAddr>().unwrap());
    }

    #[test]
    fn csrf() {
        let p = OriginPolicy::new(["https://dashboard.example.org", "https://api.example.org"]);
        let cookie = Some("session=abc");
        let post = Method::POST;
        let json = Some("application/json");
        // allowed origin
        assert!(csrf_verdict(&p, &post, None, cookie, Some("https://dashboard.example.org"), None, None, json).is_none());
        // hostile origin
        assert!(csrf_verdict(&p, &post, None, cookie, Some("https://evil.example"), None, None, json).is_some());
        // hostile referer, no origin
        assert!(csrf_verdict(&p, &post, None, cookie, None, Some("https://evil.example/x"), None, json).is_some());
        // no origin, no referer: a bot, not a browser
        assert!(csrf_verdict(&p, &post, None, cookie, None, None, None, json).is_none());
        // no cookie: nothing to steal
        assert!(csrf_verdict(&p, &post, None, None, Some("https://evil.example"), None, None, json).is_none());
        // same-origin by Host
        assert!(csrf_verdict(&p, &post, None, cookie, Some("http://localhost:8880"), None, Some("localhost:8880"), json).is_none());
        // form content type
        assert!(csrf_verdict(&p, &post, None, cookie, None, None, None, Some("text/plain;charset=UTF-8")).is_some());
        // websocket upgrade from a hostile page
        assert!(csrf_verdict(&p, &Method::GET, Some("websocket"), cookie, Some("https://evil.example"), None, None, None).is_some());
        // plain GET is not checked
        assert!(csrf_verdict(&p, &Method::GET, None, cookie, Some("https://evil.example"), None, None, None).is_none());
    }

    #[test]
    fn sniff() {
        assert_eq!(sniff_image(b"\x89PNG\r\n\x1a\nrest"), Some("image/png"));
        assert_eq!(sniff_image(&[0xFF, 0xD8, 0xFF, 0xE0]), Some("image/jpeg"));
        assert_eq!(sniff_image(b"RIFF\0\0\0\0WEBPVP8 "), Some("image/webp"));
        assert_eq!(sniff_image(b"<svg xmlns="), None);
    }

    #[test]
    fn colors() {
        assert!(valid_hex_color("#fff"));
        assert!(valid_hex_color("#ff00AA80"));
        assert!(!valid_hex_color("red"));
        assert!(!valid_hex_color("#fff\"><img src=x onerror=alert(1)>"));
        assert!(!valid_hex_color("#12345"));
    }

    #[test]
    fn limiter() {
        let l: RateLimiter<u8> = RateLimiter::new(2, Duration::from_secs(60), Duration::from_secs(60));
        assert!(l.hit(&1));
        assert!(l.hit(&1));
        assert!(!l.hit(&1));
        assert!(l.blocked(&1).is_some());
        assert!(l.blocked(&2).is_none());
        l.clear(&1);
        assert!(l.blocked(&1).is_none());
    }
}
