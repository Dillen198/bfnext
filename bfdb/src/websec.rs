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

/// SHA-256 (FIPS 180-4). Written out here rather than pulled in as a crate:
/// bfdb only needs it to key auth sessions by a digest of the cookie, so a
/// copy of the database (or a backup) holds nothing a browser could present.
pub(crate) fn sha256(data: &[u8]) -> [u8; 32] {
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
    ];
    let mut h: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
    ];
    let mut msg = data.to_vec();
    let bit_len = (data.len() as u64).wrapping_mul(8);
    msg.push(0x80);
    while msg.len() % 64 != 56 {
        msg.push(0);
    }
    msg.extend_from_slice(&bit_len.to_be_bytes());
    for block in msg.chunks_exact(64) {
        let mut w = [0u32; 64];
        for (i, c) in block.chunks_exact(4).enumerate() {
            w[i] = u32::from_be_bytes([c[0], c[1], c[2], c[3]]);
        }
        for i in 16..64 {
            let s0 = w[i - 15].rotate_right(7) ^ w[i - 15].rotate_right(18) ^ (w[i - 15] >> 3);
            let s1 = w[i - 2].rotate_right(17) ^ w[i - 2].rotate_right(19) ^ (w[i - 2] >> 10);
            w[i] = w[i - 16].wrapping_add(s0).wrapping_add(w[i - 7]).wrapping_add(s1);
        }
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut hh] = h;
        for i in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ (!e & g);
            let t1 = hh.wrapping_add(s1).wrapping_add(ch).wrapping_add(K[i]).wrapping_add(w[i]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let t2 = s0.wrapping_add(maj);
            hh = g;
            g = f;
            f = e;
            e = d.wrapping_add(t1);
            d = c;
            c = b;
            b = a;
            a = t1.wrapping_add(t2);
        }
        for (x, y) in h.iter_mut().zip([a, b, c, d, e, f, g, hh]) {
            *x = x.wrapping_add(y);
        }
    }
    let mut out = [0u8; 32];
    for (i, v) in h.iter().enumerate() {
        out[i * 4..i * 4 + 4].copy_from_slice(&v.to_be_bytes());
    }
    out
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

/// Proxies beyond our own (loopback) Caddy whose forwarding headers are
/// believed: `--trusted-proxy` CIDRs, and whether `CF-Connecting-IP` may be
/// read when the request reached Caddy from one of them
/// (`--trust-cf-connecting-ip`). Empty -- the default -- is the behaviour
/// bfdb always had.
#[derive(Debug, Clone, Default)]
pub(crate) struct TrustedProxies {
    nets: Arc<Vec<IpNet>>,
    cloudflare: bool,
}

impl TrustedProxies {
    pub(crate) fn new(nets: Vec<IpNet>, cloudflare: bool) -> Self {
        Self { nets: Arc::new(nets), cloudflare }
    }

    fn trusts(&self, ip: IpAddr) -> bool {
        self.nets.iter().any(|n| n.contains(ip))
    }

    /// Whether `CF-Connecting-IP` can ever be believed here.
    pub(crate) fn trusts_cloudflare(&self) -> bool {
        self.cloudflare && !self.nets.is_empty()
    }
}

static TRUSTED: std::sync::OnceLock<TrustedProxies> = std::sync::OnceLock::new();

/// Set once, in `main`, from the command line.
pub(crate) fn set_trusted_proxies(t: TrustedProxies) {
    let _ = TRUSTED.set(t);
}

pub(crate) fn trusted_proxies() -> &'static TrustedProxies {
    static NONE: std::sync::LazyLock<TrustedProxies> = std::sync::LazyLock::new(TrustedProxies::default);
    TRUSTED.get().unwrap_or(&NONE)
}

/// Who is really calling. bfdb sits behind Caddy on the same box, so every
/// proxied request arrives from loopback with the real client in
/// `X-Forwarded-For` -- which Caddy overwrites with the address it saw unless
/// it is itself configured to trust an upstream proxy. The header is only
/// believed when the peer is loopback (i.e. it came through our own proxy);
/// from anyone else it is ignored, since they could write anything there.
/// The rightmost entry is the one our proxy added.
///
/// With `--trusted-proxy` set (a CDN or load balancer in front of Caddy, and
/// Caddy's own `trusted_proxies` set to match so it keeps their entries),
/// entries from those addresses are skipped right to left, and the first
/// address that is not one of them is the client. With
/// `--trust-cf-connecting-ip` too, a request that reached Caddy from a
/// trusted address takes `CF-Connecting-IP` as the client.
pub(crate) fn client_ip(peer: Option<SocketAddr>, xff: Option<&str>) -> IpAddr {
    client_ip_via(&TrustedProxies::default(), peer, xff, None)
}

pub(crate) fn client_ip_via(
    trusted: &TrustedProxies,
    peer: Option<SocketAddr>,
    xff: Option<&str>,
    cf_connecting_ip: Option<&str>,
) -> IpAddr {
    let peer_ip = peer.map(|p| p.ip()).unwrap_or(IpAddr::from([127, 0, 0, 1]));
    if !is_loopback(peer_ip) && !trusted.trusts(peer_ip) {
        return peer_ip;
    }
    let hops: Vec<IpAddr> = xff
        .map(|h| h.split(',').filter_map(|s| s.trim().parse::<IpAddr>().ok()).collect())
        .unwrap_or_default();
    let Some(&nearest) = hops.last() else { return peer_ip };
    if trusted.trusts(nearest) {
        if trusted.cloudflare {
            if let Some(ip) = cf_connecting_ip.and_then(|s| s.trim().parse::<IpAddr>().ok()) {
                return ip;
            }
        }
        // walk left past every trusted hop; all trusted means the leftmost
        return hops.iter().rev().copied().find(|ip| !trusted.trusts(*ip)).unwrap_or(hops[0]);
    }
    nearest
}

/// A filter extracting [`client_ip_via`] with the configured proxies.
pub(crate) fn with_client_ip(
) -> impl Filter<Extract = (IpAddr,), Error = warp::Rejection> + Clone {
    warp::addr::remote()
        .and(warp::header::optional::<std::string::String>("x-forwarded-for"))
        .and(warp::header::optional::<std::string::String>("cf-connecting-ip"))
        .map(|peer: Option<SocketAddr>, xff: Option<std::string::String>, cf: Option<std::string::String>| {
            client_ip_via(trusted_proxies(), peer, xff.as_deref(), cf.as_deref())
        })
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

/// A token bucket per key: `burst` requests at once, refilled at `per_min` a
/// minute. For metering ordinary traffic, where a fixed window's lockout
/// would be too blunt.
pub(crate) struct TokenBucket<K> {
    per_sec: f64,
    burst: f64,
    state: Mutex<HashMap<K, (f64, Instant)>>,
}

impl<K: Hash + Eq + Clone> TokenBucket<K> {
    pub(crate) fn new(per_min: u32, burst: u32) -> Self {
        Self { per_sec: per_min as f64 / 60.0, burst: burst.max(1) as f64, state: Mutex::new(HashMap::new()) }
    }

    /// Take one token for `key`. `Err(wait)` when there is none: how long
    /// until there is.
    pub(crate) fn take(&self, key: &K) -> std::result::Result<(), Duration> {
        let now = Instant::now();
        let mut g = self.state.lock().unwrap_or_else(|e| e.into_inner());
        if g.len() > 50_000 {
            // a bucket that has refilled is the same as no bucket
            let full = Duration::from_secs_f64(self.burst / self.per_sec.max(1e-9));
            g.retain(|_, (_, at)| now.duration_since(*at) < full);
        }
        let (tokens, at) = g.entry(key.clone()).or_insert((self.burst, now));
        *tokens = (*tokens + now.duration_since(*at).as_secs_f64() * self.per_sec).min(self.burst);
        *at = now;
        if *tokens >= 1.0 {
            *tokens -= 1.0;
            Ok(())
        } else {
            Err(Duration::from_secs_f64(((1.0 - *tokens) / self.per_sec.max(1e-9)).max(1.0)))
        }
    }
}

/// Budgets for anonymous GETs under `/api/` (`--public-get-rate`,
/// `--heavy-get-rate`). 0 turns a budget off.
#[derive(Debug, Clone, Copy)]
pub(crate) struct GetLimits {
    /// Every GET under /api/, per client IP per minute.
    pub(crate) per_min: u32,
    /// The expensive ones on top -- clip cuts, PNG cards, whole Tacview
    /// recordings -- per client IP per minute.
    pub(crate) heavy_per_min: u32,
}

/// A route that costs real CPU or I/O per call: it gets the stricter budget.
pub(crate) fn is_heavy_get(path: &str) -> bool {
    path.ends_with("/card.png")
        || path.ends_with("/clip")
        || path.ends_with("/clip.acmi")
        || path.starts_with("/api/range/tacview/")
        || (path.starts_with("/api/replay/rec/") && path.ends_with("/acmi"))
}

/// A front-of-chain filter: answers 429 when a client has spent its GET
/// budget, and rejects (lets the chain carry on) otherwise.
///
/// Loopback callers -- the Discord bot, bfdb's own tooling -- are never
/// metered. Nor is anything when the client cannot be told apart: a request
/// carrying `CF-Connecting-IP` that bfdb was not told to trust
/// (`--trusted-proxy`) looks like it comes from one Cloudflare edge address
/// for every visitor, and metering that would throttle everyone at once.
pub(crate) fn get_rate_guard(
    limits: GetLimits,
) -> impl Filter<Extract = (Response,), Error = warp::Rejection> + Clone {
    let general: Arc<TokenBucket<IpAddr>> = Arc::new(TokenBucket::new(limits.per_min, (limits.per_min / 3).max(60)));
    let heavy: Arc<TokenBucket<IpAddr>> = Arc::new(TokenBucket::new(limits.heavy_per_min, (limits.heavy_per_min / 3).max(10)));
    warp::get()
        .and(warp::path::full())
        .and(with_client_ip())
        .and(warp::header::optional::<std::string::String>("cf-connecting-ip"))
        .and_then(move |full: warp::path::FullPath, ip: IpAddr, cf: Option<std::string::String>| {
            let (general, heavy) = (general.clone(), heavy.clone());
            async move {
                let path = full.as_str();
                if !path.starts_with("/api/") || is_loopback(ip) {
                    return Err(warp::reject::reject());
                }
                if cf.is_some() && !trusted_proxies().trusts_cloudflare() {
                    return Err(warp::reject::reject());
                }
                let mut verdict = Ok(());
                if limits.per_min > 0 {
                    verdict = general.take(&ip);
                }
                if verdict.is_ok() && limits.heavy_per_min > 0 && is_heavy_get(path) {
                    verdict = heavy.take(&ip);
                }
                match verdict {
                    Ok(()) => Err(warp::reject::reject()),
                    Err(wait) => {
                        log::debug!("rate limited GET {path} from {ip}");
                        let mut r = error_response(&too_many("too many requests -- slow down"));
                        if let Ok(v) = HeaderValue::from_str(&wait.as_secs().max(1).to_string()) {
                            r.headers_mut().insert("retry-after", v);
                        }
                        Ok(r)
                    }
                }
            }
        })
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

/// The tighter policy `--csp-enforce` sends, as an enforced header. Every
/// third-party host the dashboard loads from is named instead of `https:`:
/// Esri basemap tiles, the AWS terrain tiles of the 3D replay, Discord
/// avatars, Google Fonts. `--csp-extra-src` appends to connect-src and
/// img-src for anything a deployment adds.
const SPA_CSP_STRICT: &str = "default-src 'self'; \
     script-src 'self'; \
     style-src 'self' 'unsafe-inline' https://fonts.googleapis.com; \
     font-src 'self' data: https://fonts.gstatic.com; \
     img-src 'self' data: blob: https://server.arcgisonline.com https://s3.amazonaws.com https://cdn.discordapp.com{EXTRA}; \
     connect-src 'self' https://server.arcgisonline.com https://s3.amazonaws.com{EXTRA}; \
     worker-src 'self' blob:; \
     child-src 'self' blob:; \
     object-src 'none'; \
     base-uri 'self'; \
     form-action 'self'; \
     frame-ancestors 'none'";

/// (header name, value) for the dashboard page, set once in `main`.
static SPA_CSP_HEADER: std::sync::OnceLock<(&'static str, HeaderValue)> = std::sync::OnceLock::new();

/// Choose the dashboard's CSP: report-only and broad (the default), or
/// enforced and tight (`--csp-enforce`), with `extra` sources appended.
pub(crate) fn set_spa_csp(enforce: bool, extra: &[std::string::String]) {
    let _ = SPA_CSP_HEADER.set(spa_csp(enforce, extra));
}

fn spa_csp(enforce: bool, extra: &[std::string::String]) -> (&'static str, HeaderValue) {
    if !enforce {
        return ("content-security-policy-report-only", HeaderValue::from_static(SPA_CSP));
    }
    let extra: std::string::String = extra
        .iter()
        // a source is one token: nothing that could end the directive
        .filter(|s| !s.is_empty() && !s.contains([';', ',', ' ', '\'', '"', '\n', '\r']))
        .map(|s| format!(" {s}"))
        .collect();
    let v = SPA_CSP_STRICT.replace("{EXTRA}", &extra);
    let v = HeaderValue::from_str(&v).unwrap_or_else(|_| HeaderValue::from_static(SPA_CSP));
    ("content-security-policy", v)
}

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
        let (name, value) = SPA_CSP_HEADER
            .get()
            .cloned()
            .unwrap_or(("content-security-policy-report-only", HeaderValue::from_static(SPA_CSP)));
        if !h.contains_key(name) {
            h.insert(name, value);
        }
    } else if !is_code {
        // Nothing the API returns is meant to be rendered as a page. If a
        // browser is ever talked into doing so anyway, it gets no script, no
        // plugins and no framing.
        default("content-security-policy", "default-src 'none'; frame-ancestors 'none'; sandbox");
    }
    r
}

// ── Request metrics ──────────────────────────────────────────────────────────

/// Latency buckets, seconds, for `bfdb_http_request_duration_seconds`.
const LATENCY_BUCKETS: [f64; 8] = [0.005, 0.025, 0.1, 0.25, 0.5, 1.0, 2.5, 10.0];

#[derive(Default, Clone)]
struct RouteStats {
    /// requests by status class: 1xx..5xx at index 1..5
    by_class: [u64; 6],
    /// cumulative counts per LATENCY_BUCKETS entry
    buckets: [u64; LATENCY_BUCKETS.len()],
    sum_secs: f64,
    count: u64,
}

static REQUESTS: std::sync::LazyLock<Mutex<HashMap<(std::string::String, &'static str), RouteStats>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// The label a path is counted under: its first two segments
/// (`/api/stats`, `/api/pilot`, `/ws/tacmap`), so ids never become labels.
/// Everything that is not the API is the dashboard.
fn route_label(path: &str) -> std::string::String {
    if !(path.starts_with("/api/") || path.starts_with("/ws/")) {
        return "page".into();
    }
    let mut segs = path.trim_start_matches('/').split('/').take(2);
    let a = segs.next().unwrap_or("");
    let b: std::string::String = segs
        .next()
        .unwrap_or("")
        .chars()
        .take(32)
        .filter(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | '.'))
        .collect();
    format!("/{a}/{b}")
}

/// Count one answered request.
pub(crate) fn record_request(method: &Method, path: &str, status: StatusCode, elapsed: Duration) {
    let method: &'static str = match *method {
        Method::GET => "GET",
        Method::POST => "POST",
        Method::OPTIONS => "OPTIONS",
        _ => "other",
    };
    let mut g = REQUESTS.lock().unwrap_or_else(|e| e.into_inner());
    let label = route_label(path);
    // a flood of made-up first segments must not grow this without bound
    let key = if g.len() >= 2_000 && !g.contains_key(&(label.clone(), method)) { ("/other".into(), method) } else { (label, method) };
    let s = g.entry(key).or_default();
    s.by_class[(status.as_u16() / 100).min(5) as usize] += 1;
    let secs = elapsed.as_secs_f64();
    for (i, b) in LATENCY_BUCKETS.iter().enumerate() {
        if secs <= *b {
            s.buckets[i] += 1;
        }
    }
    s.sum_secs += secs;
    s.count += 1;
}

/// The request counters in Prometheus text form.
pub(crate) fn write_request_metrics(out: &mut std::string::String) {
    use std::fmt::Write as _;
    let g = REQUESTS.lock().unwrap_or_else(|e| e.into_inner());
    let mut keys: Vec<_> = g.keys().cloned().collect();
    keys.sort();
    let _ = writeln!(out, "# TYPE bfdb_http_requests_total counter");
    for k in &keys {
        let s = &g[k];
        for (class, n) in s.by_class.iter().enumerate().skip(1) {
            if *n > 0 {
                let _ = writeln!(
                    out,
                    "bfdb_http_requests_total{{route=\"{}\",method=\"{}\",status=\"{class}xx\"}} {n}",
                    k.0, k.1
                );
            }
        }
    }
    let _ = writeln!(out, "# TYPE bfdb_http_request_duration_seconds histogram");
    for k in &keys {
        let s = &g[k];
        let l = format!("route=\"{}\",method=\"{}\"", k.0, k.1);
        for (i, b) in LATENCY_BUCKETS.iter().enumerate() {
            let _ = writeln!(out, "bfdb_http_request_duration_seconds_bucket{{{l},le=\"{b}\"}} {}", s.buckets[i]);
        }
        let _ = writeln!(out, "bfdb_http_request_duration_seconds_bucket{{{l},le=\"+Inf\"}} {}", s.count);
        let _ = writeln!(out, "bfdb_http_request_duration_seconds_sum{{{l}}} {}", s.sum_secs);
        let _ = writeln!(out, "bfdb_http_request_duration_seconds_count{{{l}}} {}", s.count);
    }
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
    /// A background refresh (`get_swr`) is under way.
    refreshing: std::sync::atomic::AtomicBool,
}

impl<T: Clone + Send + Sync + 'static> Cached<T> {
    /// Stale-while-revalidate: a value younger than `ttl` is served as is;
    /// one younger than `max_stale` is served too, while one background
    /// refresh replaces it. Only with nothing usable does a caller wait --
    /// and then every concurrent caller waits for the same computation.
    ///
    /// `f` is blocking (a sled scan) and runs on the blocking pool, never on
    /// a request's worker thread: a full scan of a big tree used to hold an
    /// executor thread for its whole length on every refresh.
    pub(crate) async fn get_swr<F>(self: Arc<Self>, ttl: Duration, max_stale: Duration, f: F) -> anyhow::Result<T>
    where
        F: FnOnce() -> anyhow::Result<T> + Send + 'static,
    {
        use std::sync::atomic::Ordering;
        let mut g = self.slot.lock().await;
        if let Some((at, v)) = &*g {
            let age = at.elapsed();
            if age < ttl {
                return Ok(v.clone());
            }
            if age < max_stale {
                let v = v.clone();
                if !self.refreshing.swap(true, Ordering::SeqCst) {
                    let me = self.clone();
                    tokio::spawn(async move {
                        match tokio::task::spawn_blocking(f).await {
                            Ok(Ok(new)) => *me.slot.lock().await = Some((Instant::now(), new)),
                            Ok(Err(e)) => log::warn!("background cache refresh failed: {e:#}"),
                            Err(e) => log::warn!("background cache refresh died: {e}"),
                        }
                        me.refreshing.store(false, Ordering::SeqCst);
                    });
                }
                return Ok(v);
            }
        }
        let v = tokio::task::spawn_blocking(f)
            .await
            .map_err(|e| anyhow::anyhow!("cache refresh died: {e}"))??;
        *g = Some((Instant::now(), v.clone()));
        Ok(v)
    }
}

impl<T: Clone> Cached<T> {
    pub(crate) fn new() -> Self {
        Self { slot: tokio::sync::Mutex::new(None), refreshing: std::sync::atomic::AtomicBool::new(false) }
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
    map: Mutex<HashMap<K, (Arc<Cached<T>>, Instant)>>,
}

/// Most keys a [`CacheMap`] holds before it evicts.
const CACHE_MAP_KEYS: usize = 256;

impl<K: Hash + Eq + Clone, T: Clone> CacheMap<K, T> {
    pub(crate) fn new() -> Self {
        Self { map: Mutex::new(HashMap::new()) }
    }

    pub(crate) fn entry(&self, key: &K) -> Arc<Cached<T>> {
        let mut g = self.map.lock().unwrap_or_else(|e| e.into_inner());
        let now = Instant::now();
        if let Some((c, used)) = g.get_mut(key) {
            *used = now;
            return c.clone();
        }
        // Keys are instances/rounds -- a handful -- but a caller-controlled
        // key (`?round=`) must not grow this without bound. Evict the least
        // recently used: clearing the lot, as this once did, threw away every
        // hot dashboard answer whenever someone walked through old rounds.
        if g.len() >= CACHE_MAP_KEYS {
            if let Some(oldest) = g.iter().min_by_key(|(_, (_, used))| *used).map(|(k, _)| k.clone()) {
                g.remove(&oldest);
            }
        }
        let c = Arc::new(Cached::new());
        g.insert(key.clone(), (c.clone(), now));
        c
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.map.lock().unwrap().len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sha256_vectors() {
        let hex = |b: [u8; 32]| b.iter().map(|x| format!("{x:02x}")).collect::<std::string::String>();
        assert_eq!(hex(sha256(b"")), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
        assert_eq!(hex(sha256(b"abc")), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
        assert_eq!(
            hex(sha256(b"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")),
            "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
        );
        // exactly one padding block's worth past a block boundary
        assert_eq!(
            hex(sha256(&[b'a'; 64])),
            "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"
        );
    }

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
    fn client_ip_through_trusted_proxies() {
        let lo: SocketAddr = "127.0.0.1:5000".parse().unwrap();
        let ip = |s: &str| s.parse::<IpAddr>().unwrap();
        let cdn = TrustedProxies::new(vec!["173.245.48.0/20".parse().unwrap()], false);
        // the CDN's own hop is skipped; the visitor is the one it forwarded
        assert_eq!(client_ip_via(&cdn, Some(lo), Some("198.51.100.7, 173.245.48.9"), None), ip("198.51.100.7"));
        // a forged entry left of the visitor's is never reached
        assert_eq!(client_ip_via(&cdn, Some(lo), Some("6.6.6.6, 198.51.100.7, 173.245.48.9"), None), ip("198.51.100.7"));
        // not through the CDN: same as without the flag
        assert_eq!(client_ip_via(&cdn, Some(lo), Some("6.6.6.6, 203.0.113.1"), None), ip("203.0.113.1"));
        let cf = TrustedProxies::new(vec!["173.245.48.0/20".parse().unwrap()], true);
        assert_eq!(client_ip_via(&cf, Some(lo), Some("173.245.48.9"), Some("198.51.100.8")), ip("198.51.100.8"));
        // CF-Connecting-IP from a client that did not come through Cloudflare is ignored
        assert_eq!(client_ip_via(&cf, Some(lo), Some("203.0.113.1"), Some("1.2.3.4")), ip("203.0.113.1"));
        // and a direct, non-proxied peer is always itself
        let ext: SocketAddr = "203.0.113.9:5000".parse().unwrap();
        assert_eq!(client_ip_via(&cf, Some(ext), Some("1.1.1.1"), Some("1.2.3.4")), ip("203.0.113.9"));
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
    fn token_bucket() {
        let b: TokenBucket<u8> = TokenBucket::new(60, 3);
        assert!(b.take(&1).is_ok());
        assert!(b.take(&1).is_ok());
        assert!(b.take(&1).is_ok());
        let wait = b.take(&1).unwrap_err();
        assert!(wait >= Duration::from_secs(1));
        assert!(b.take(&2).is_ok(), "buckets are per key");
    }

    #[test]
    fn cache_map_evicts_least_recently_used() {
        let m: CacheMap<u32, u32> = CacheMap::new();
        let hot = m.entry(&0);
        for k in 1..CACHE_MAP_KEYS as u32 {
            m.entry(&k);
        }
        std::thread::sleep(Duration::from_millis(2));
        m.entry(&0); // touch the hot key
        m.entry(&9999); // over the cap: evicts key 1, the oldest untouched
        assert_eq!(m.len(), CACHE_MAP_KEYS);
        assert!(Arc::ptr_eq(&hot, &m.entry(&0)), "the hot entry survived");
    }

    #[test]
    fn csp_modes() {
        let (n, v) = spa_csp(false, &[]);
        assert_eq!(n, "content-security-policy-report-only");
        assert!(v.to_str().unwrap().contains("connect-src 'self' https: wss: ws:"));
        let (n, v) = spa_csp(true, &["https://tiles.example.org".into(), "evil; script-src *".into()]);
        assert_eq!(n, "content-security-policy");
        let v = v.to_str().unwrap();
        assert!(v.contains("connect-src 'self' https://server.arcgisonline.com https://s3.amazonaws.com https://tiles.example.org;"));
        assert!(!v.contains("evil"));
        assert!(!v.contains("https:;") && !v.contains(" https: "));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn stale_while_revalidate() {
        let c: Arc<Cached<u32>> = Arc::new(Cached::new());
        let ttl = Duration::from_millis(50);
        let stale = Duration::from_secs(60);
        assert_eq!(c.clone().get_swr(ttl, stale, || Ok(1)).await.unwrap(), 1);
        tokio::time::sleep(Duration::from_millis(80)).await;
        // stale: the old value comes back at once, the refresh runs behind it
        assert_eq!(c.clone().get_swr(ttl, stale, || Ok(2)).await.unwrap(), 1);
        for _ in 0..100 {
            tokio::time::sleep(Duration::from_millis(10)).await;
            if c.clone().get_swr(ttl, stale, || Ok(3)).await.unwrap() == 2 {
                return;
            }
        }
        panic!("the background refresh never landed");
    }

    #[test]
    fn request_metrics() {
        assert_eq!(route_label("/api/pilot/0123/sorties"), "/api/pilot");
        assert_eq!(route_label("/ws/tacmap"), "/ws/tacmap");
        assert_eq!(route_label("/map"), "page");
        assert_eq!(route_label("/api/x\"y"), "/api/xy");
        record_request(&Method::GET, "/api/metrics-test", StatusCode::OK, Duration::from_millis(30));
        record_request(&Method::GET, "/api/metrics-test", StatusCode::INTERNAL_SERVER_ERROR, Duration::from_secs(3));
        let mut out = std::string::String::new();
        write_request_metrics(&mut out);
        assert!(out.contains("bfdb_http_requests_total{route=\"/api/metrics-test\",method=\"GET\",status=\"2xx\"} 1"));
        assert!(out.contains("bfdb_http_requests_total{route=\"/api/metrics-test\",method=\"GET\",status=\"5xx\"} 1"));
        assert!(out.contains("bfdb_http_request_duration_seconds_bucket{route=\"/api/metrics-test\",method=\"GET\",le=\"0.1\"} 1"));
        assert!(out.contains("bfdb_http_request_duration_seconds_count{route=\"/api/metrics-test\",method=\"GET\"} 2"));
    }

    #[test]
    fn heavy_routes() {
        assert!(is_heavy_get("/api/range/result/abc/clip"));
        assert!(is_heavy_get("/api/range/result/abc/card.png"));
        assert!(is_heavy_get("/api/range/tacview/abc"));
        assert!(is_heavy_get("/api/replay/rec/x/acmi"));
        assert!(!is_heavy_get("/api/replay/rec/x/chunk/3"));
        assert!(!is_heavy_get("/api/stats"));
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
