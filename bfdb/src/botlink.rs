//! The DCSServerBot link: player lookups, server info and instance actions.

use crate::*;

#[derive(Debug, Clone)]
pub(crate) struct BotLinkConfig {
    pub(crate) base_url: String,
    pub(crate) api_key:  String,
    /// Key for the FowlEngine plugin's OPS API (`/fowlengine/ops/*`);
    /// `--ops-api-key`, else the same as `api_key`.
    pub(crate) ops_key:  String,
}

#[derive(Deserialize)]
pub(crate) struct BotUserEntry {
    pub(crate) ucid: std::string::String,
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
pub(crate) async fn resolve_ucid_via_bot(
    bot_cfg: &Option<BotLinkConfig>,
    discord_id: &str,
) -> Option<dcso3::net::Ucid> {
    let cfg = bot_cfg.as_ref()?;
    // Only a real Discord snowflake can have a DCSServerBot link. A local-login
    // session (`local:admin`, the bot's own account) was sent too, and the
    // bot's getuser crashed on it -- 98 Postgres "invalid input syntax for
    // type bigint" tracebacks in two hours, one per page load.
    if discord_id.is_empty() || !discord_id.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    // Every page load asks (via /api/auth/me) and so does every coalition-
    // gated call; a link changes only when someone runs /linkme. A minute of
    // caching turns dozens of bot round-trips per page into one.
    const UCID_CACHE: std::time::Duration = std::time::Duration::from_secs(60);
    static CACHE: std::sync::LazyLock<
        Mutex<std::collections::HashMap<std::string::String, (Option<dcso3::net::Ucid>, std::time::Instant)>>,
    > = std::sync::LazyLock::new(|| Mutex::new(std::collections::HashMap::new()));
    {
        let mut g = CACHE.lock().unwrap_or_else(|e| e.into_inner());
        g.retain(|_, (_, at)| at.elapsed() < UCID_CACHE);
        if let Some((ucid, _)) = g.get(discord_id) {
            return *ucid;
        }
    }
    let result: anyhow::Result<Option<dcso3::net::Ucid>> = async {
        let http = http_client();
        let resp = http
            .post(format!("{}/getuser", cfg.base_url))
            .header("X-API-Key", &cfg.api_key)
            .form(&[("discord_id", discord_id)])
            .send()
            .await
            .map_err(|e| anyhow::anyhow!("DCSServerBot getuser request failed: {e}"))?;
        // Read the body as text first: when the bot errors it answers with a
        // FastAPI error object or a bare "Internal Server Error", and "error
        // decoding response body" alone said nothing about which (Sept 28:
        // real Discord ids failing, cause unknown from our side).
        let status = resp.status();
        let body = resp
            .text()
            .await
            .map_err(|e| anyhow::anyhow!("DCSServerBot getuser read failed: {e}"))?;
        // The bot answers 404 "Player ... not found" for a Discord account
        // that has never been linked to a DCS one -- that is the ordinary
        // "not linked" answer, not a failure (and was logging a WARN on every
        // dashboard page such a user opened).
        if status == reqwest::StatusCode::NOT_FOUND {
            return Ok(None);
        }
        if !status.is_success() {
            let snippet: std::string::String = body.chars().take(200).collect();
            anyhow::bail!("DCSServerBot getuser -> HTTP {status}: {snippet}");
        }
        let users: Vec<BotUserEntry> = serde_json::from_str(&body).map_err(|e| {
            let snippet: std::string::String = body.chars().take(200).collect();
            anyhow::anyhow!("DCSServerBot getuser response parse failed: {e} (body: {snippet})")
        })?;
        Ok(match users.into_iter().next() {
            Some(u) => Some(u.ucid.parse().map_err(|e| anyhow::anyhow!("bad ucid from bot: {e:?}"))?),
            None => None,
        })
    }.await;
    match result {
        Ok(ucid) => {
            CACHE
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .insert(discord_id.to_string(), (ucid, std::time::Instant::now()));
            ucid
        }
        // Not cached: a bot that is down now may be back in a second.
        Err(e) => {
            log::warn!("DCSServerBot link lookup failed for discord_id={discord_id}: {e:?}");
            None
        }
    }
}

/// The one outbound HTTP client (DCSServerBot, Discord). Shared so requests
/// reuse connections, and with a timeout, because a hung bot used to hang
/// every dashboard request that asked it anything.
pub(crate) fn http_client() -> reqwest::Client {
    static CLIENT: std::sync::LazyLock<reqwest::Client> = std::sync::LazyLock::new(|| {
        reqwest::Client::builder()
            .timeout(std::time::Duration::from_secs(5))
            .connect_timeout(std::time::Duration::from_secs(3))
            .build()
            .expect("building the HTTP client")
    });
    CLIENT.clone()
}

// Units per DCSServerBot's own RestAPI plugin WeatherInfo model:
// temperature=Celsius, wind_speed=knots, wind_direction=degrees,
// pressure=mmHg, clouds_base=feet. Only clouds_base/pressure need
// converting to match bfdb's existing meters/hPa convention.
#[derive(Deserialize, Clone)]
pub(crate) struct BotWeather {
    pub(crate) temperature:    Option<f64>,
    pub(crate) wind_speed:     Option<f64>,
    pub(crate) wind_direction: Option<f64>,
    pub(crate) pressure:       Option<f64>,
    pub(crate) clouds_base:    Option<f64>,
    pub(crate) clouds_density: Option<u8>,
    pub(crate) visibility:     Option<f64>,
}

#[derive(Deserialize, Clone)]
pub(crate) struct BotServerInfo {
    #[serde(default)]
    pub(crate) name:         std::string::String,
    #[serde(default)]
    pub(crate) status:       Option<std::string::String>,
    pub(crate) restart_time: Option<std::string::String>,
    pub(crate) weather:      Option<BotWeather>,
}

/// DCSServerBot's RestAPI plugin serializes datetimes inconsistently across
/// versions -- sometimes RFC3339 with an offset, sometimes a naive
/// "YYYY-MM-DDTHH:MM:SS" with no timezone. Try both; treat a naive one as
/// already UTC (matches how the bot's own scheduler stores it).
pub(crate) fn parse_bot_datetime(s: &str) -> Option<chrono::DateTime<chrono::Utc>> {
    if let Ok(dt) = chrono::DateTime::parse_from_rfc3339(s) {
        return Some(dt.with_timezone(&chrono::Utc));
    }
    chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S")
        .ok()
        .map(|ndt| chrono::DateTime::from_naive_utc_and_offset(ndt, chrono::Utc))
}

/// Which of DCSServerBot's servers belongs to `inst`.
///
/// With one DCS server behind bfdb, "the first one" and "the right one" are
/// the same server and nobody noticed the difference. With two they are not:
/// the bot's `/servers` list is in its own order, so every instance was handed
/// the first entry's weather and restart time. A Syria campaign and a Caucasus
/// campaign, each baking its own METAR station into its own mission, both read
/// the same conditions on the dashboard -- and worse, `/api/stats` pushes that
/// reading back into the engine via `set-server-info`, so one server's in-game
/// ATIS and F10 Info showed the other server's weather. The same bug on
/// `bot_instance_action` means an admin restarting a server restarts whichever
/// one the bot happens to list first.
///
/// `dcs_server_name` on the instance is the match. It already exists for the
/// fowlengine plugin's lookup in the other direction.
pub(crate) fn pick_bot_server(servers: Vec<BotServerInfo>, inst: &Inst) -> anyhow::Result<BotServerInfo> {
    if let Some(want) = inst.cfg.dcs_server_name.as_deref() {
        return servers
            .into_iter()
            .find(|s| s.name == want)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "DCSServerBot has no server named {want:?}, which instance {:?} claims -- \
                     fix `dcs_server_name` in the instances file so it matches the bot exactly",
                    inst.id
                )
            });
    }
    let mut it = servers.into_iter();
    match (it.next(), it.next()) {
        (Some(only), None) => Ok(only),
        // Guessing here is how one server ends up flying another's weather.
        (Some(_), Some(_)) => anyhow::bail!(
            "instance {:?} has no `dcs_server_name` and DCSServerBot fronts more than one \
             server -- set it, or bfdb cannot tell which server's weather, restart time and \
             controls belong to this instance",
            inst.id
        ),
        (None, _) => anyhow::bail!("DCSServerBot has no servers registered"),
    }
}

/// A misconfigured `dcs_server_name` is permanent until somebody edits the
/// file, and `/api/stats` is polled every 30s by every open dashboard tab, so
/// saying so on every request would bury the log. Say it, then say it again
/// occasionally.
pub(crate) fn warn_bot_lookup(inst: &Inst, e: &anyhow::Error) {
    use std::sync::atomic::{AtomicI64, Ordering};
    const QUIET_SECS: i64 = 300;
    static LAST: AtomicI64 = AtomicI64::new(i64::MIN);
    let now = chrono::Utc::now().timestamp();
    let last = LAST.load(Ordering::Relaxed);
    if now - last >= QUIET_SECS {
        LAST.store(now, Ordering::Relaxed);
        log::warn!("[{}] DCSServerBot server lookup failed: {e:#}", inst.id);
    }
}

/// Fetches DCSServerBot's GET {prefix}/servers -- never fails outright,
/// same never-fails contract as resolve_ucid_via_bot (a dashboard falling
/// back to bflib-derived data, or nothing, beats /api/stats breaking
/// because the bot is down). Backs both the restart countdown and live
/// weather, which live in the same response.
pub(crate) async fn fetch_bot_server_info(
    bot_cfg: &Option<BotLinkConfig>,
    inst: &Inst,
) -> Option<BotServerInfo> {
    let cfg = bot_cfg.as_ref()?;
    // /api/stats is public and polled by every tab; the bot's answer (weather,
    // restart time) changes on the scale of minutes.
    static CACHE: std::sync::LazyLock<websec::CacheMap<InstanceId, Option<BotServerInfo>>> =
        std::sync::LazyLock::new(websec::CacheMap::new);
    CACHE
        .entry(&inst.id)
        .get_or_refresh(std::time::Duration::from_secs(15), || async {
            Ok::<_, std::convert::Infallible>(fetch_bot_server_info_uncached(cfg, inst).await)
        })
        .await
        .unwrap_or_else(|e| match e {})
}

pub(crate) async fn fetch_bot_server_info_uncached(cfg: &BotLinkConfig, inst: &Inst) -> Option<BotServerInfo> {
    let result: anyhow::Result<Option<BotServerInfo>> = async {
        let http = http_client();
        let servers: Vec<BotServerInfo> = http
            .get(format!("{}/servers", cfg.base_url))
            .header("X-API-Key", &cfg.api_key)
            .send()
            .await
            .map_err(|e| anyhow::anyhow!("DCSServerBot servers request failed: {e}"))?
            .json()
            .await
            .map_err(|e| anyhow::anyhow!("DCSServerBot servers response parse failed: {e}"))?;
        Ok(Some(pick_bot_server(servers, inst)?))
    }.await;
    match result {
        Ok(v) => v,
        Err(e) => {
            warn_bot_lookup(inst, &e);
            None
        }
    }
}

/// Calls one of DCSServerBot's RestAPI instance/mission control endpoints
/// (POST {base_url}/{path}?server_name=...) against the single server this
/// deployment assumes -- the server name is looked up from GET /servers on
/// every call rather than cached, since it's cheap and avoids acting on a
/// stale name after the bot restarts or the server gets renamed. Unlike
/// resolve_ucid_via_bot/fetch_bot_server_info this does NOT swallow errors:
/// these are admin-triggered actions (start/stop/restart the live DCS
/// server), so the caller needs to know if it actually happened.
pub(crate) async fn bot_instance_action(
    bot_cfg: &Option<BotLinkConfig>,
    inst: &Inst,
    path: &str,
) -> anyhow::Result<serde_json::Value> {
    let cfg = bot_cfg.as_ref().ok_or_else(|| {
        anyhow::anyhow!("DCSServerBot is not configured (--dcsserverbot-url/--dcsserverbot-api-key)")
    })?;
    let http = http_client();
    let servers: Vec<BotServerInfo> = http
        .get(format!("{}/servers", cfg.base_url))
        .header("X-API-Key", &cfg.api_key)
        .send()
        .await
        .map_err(|e| anyhow::anyhow!("DCSServerBot /servers request failed: {e}"))?
        .json()
        .await
        .map_err(|e| anyhow::anyhow!("DCSServerBot /servers response parse failed: {e}"))?;
    // Stopping or restarting the wrong DCS server is not a thing to guess at.
    let name = pick_bot_server(servers, inst)
        .map(|s| s.name)
        .and_then(|n| {
            if n.is_empty() {
                anyhow::bail!("DCSServerBot reported a server with an empty name")
            } else {
                Ok(n)
            }
        })?;
    let resp = http
        .post(format!("{}/{path}", cfg.base_url))
        .header("X-API-Key", &cfg.api_key)
        .query(&[("server_name", name.as_str())])
        // Starting or stopping DCS takes the bot a while to acknowledge.
        .timeout(std::time::Duration::from_secs(30))
        .send()
        .await
        .map_err(|e| anyhow::anyhow!("DCSServerBot {path} request failed: {e}"))?;
    let status = resp.status();
    let body: serde_json::Value = resp
        .json()
        .await
        .map_err(|e| anyhow::anyhow!("DCSServerBot {path} response parse failed: {e}"))?;
    if !status.is_success() {
        anyhow::bail!("DCSServerBot {path} returned {status}: {body}");
    }
    Ok(body)
}
