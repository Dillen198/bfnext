//! The server boxes this app controls: this box's own bot ("local") and any
//! others reached over Tailscale through their OPS API (deploy/manager-remote.md).
//!
//! Keys stay on the Rust side: manager.json keeps them DPAPI-encrypted
//! (config.rs), the UI only ever learns whether one is set, and every OPS
//! call adds the X-API-Key header here.

use crate::bot;
use crate::config::{ManagerConfig, RemoteServer};
use anyhow::{anyhow, bail, Result};
use serde::{Deserialize, Serialize};
use std::net::IpAddr;
use std::time::Duration;

pub const LOCAL_ID: &str = "local";
const OPS_SUFFIX: &str = "/fowlengine/ops";

/// What the UI sees of one server: never the key.
#[derive(Debug, Clone, Serialize)]
pub struct ServerView {
    pub id: String,
    pub name: String,
    pub url: Option<String>,
    pub has_key: bool,
    pub local: bool,
    /// Why this server can't be reached as configured (the local bot's
    /// config is incomplete, say).
    pub problem: Option<String>,
}

/// An add / edit from the UI. `key`: None keeps the saved one, "" clears it.
#[derive(Debug, Clone, Deserialize)]
pub struct ServerInput {
    #[serde(default)]
    pub id: Option<String>,
    pub name: String,
    pub url: String,
    #[serde(default)]
    pub key: Option<String>,
}

/// Where one OPS request goes, and with which key.
pub struct Target {
    pub id: String,
    pub name: String,
    pub base: String,
    pub key: String,
    pub local: bool,
}

/// 100.64.0.0/10 (Tailscale's CGNAT range) or fd7a:115c:a1e0::/48.
pub fn is_tailnet_ip(ip: &IpAddr) -> bool {
    match ip {
        IpAddr::V4(v) => {
            let o = v.octets();
            o[0] == 100 && (o[1] & 0xC0) == 64
        }
        IpAddr::V6(v) => {
            let s = v.segments();
            s[0] == 0xfd7a && s[1] == 0x115c && s[2] == 0xa1e0
        }
    }
}

/// Accept what an admin would paste -- `100.101.2.86:9876`,
/// `http://range-pc.tail1234.ts.net:9876/stats`, the full OPS URL -- and turn
/// it into the OPS base URL. Only Tailscale addresses (100.64.0.0/10,
/// fd7a:115c:a1e0::/48, *.ts.net) or this PC: the routes are plain HTTP and
/// the key goes in a header, so they must only ever travel inside the
/// tailnet's WireGuard tunnel.
pub fn normalize_url(raw: &str) -> Result<String> {
    let raw = raw.trim().trim_end_matches('/');
    if raw.is_empty() {
        bail!("enter the server's OPS address, like http://100.101.2.86:9876/stats");
    }
    let with_scheme = if raw.contains("://") { raw.to_string() } else { format!("http://{raw}") };
    let u = url::Url::parse(&with_scheme).map_err(|e| anyhow!("not a valid address ({e}): {raw}"))?;
    if !matches!(u.scheme(), "http" | "https") {
        bail!("the address must start with http:// (or https://)");
    }
    if !u.username().is_empty() || u.password().is_some() || u.query().is_some() || u.fragment().is_some() {
        bail!("just the address, port and path, please -- no user, query or #");
    }
    let host = match u.host() {
        Some(url::Host::Ipv4(v)) => {
            let ip = IpAddr::V4(v);
            if !(is_tailnet_ip(&ip) || v.is_loopback()) {
                bail!(
                    "{v} is not a Tailscale address (those are 100.64.0.0 - 100.127.255.255) -- a public or LAN \
                     address would send the key in plain HTTP outside the tailnet. Run `tailscale ip -4` on that \
                     box and use what it prints"
                );
            }
            v.to_string()
        }
        Some(url::Host::Ipv6(v)) => {
            let ip = IpAddr::V6(v);
            if !(is_tailnet_ip(&ip) || v.is_loopback()) {
                bail!("{v} is not a Tailscale address");
            }
            format!("[{v}]")
        }
        Some(url::Host::Domain(d)) => {
            let d = d.to_ascii_lowercase();
            if !(d == "localhost" || d.ends_with(".ts.net")) {
                bail!(
                    "{d}: use the server's Tailscale address (100.x.y.z) or its full MagicDNS name \
                     (name.your-tailnet.ts.net) -- the OPS API is only ever reached over Tailscale"
                );
            }
            d
        }
        None => bail!("no host in {raw}"),
    };
    let port = u.port().map(|p| format!(":{p}")).unwrap_or_default();
    let mut path = u.path().trim_end_matches('/').to_string();
    if !path.ends_with(OPS_SUFFIX) {
        path.push_str(OPS_SUFFIX);
    }
    Ok(format!("{}://{host}{port}{path}", u.scheme()))
}

/// A short id from a name, unique among `taken`.
pub fn make_id(name: &str, taken: &[String]) -> String {
    let mut base: String = name
        .to_ascii_lowercase()
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
        .collect();
    base = base.split('-').filter(|s| !s.is_empty()).collect::<Vec<_>>().join("-");
    if base.is_empty() || base == LOCAL_ID {
        base = "server".into();
    }
    base.truncate(32);
    let mut id = base.clone();
    let mut n = 2;
    while taken.iter().any(|t| t == &id) {
        id = format!("{base}-{n}");
        n += 1;
    }
    id
}

fn local_name(cfg: &ManagerConfig) -> String {
    cfg.local_name
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(String::from)
        .unwrap_or_else(|| {
            let h = std::env::var("COMPUTERNAME").unwrap_or_default();
            if h.is_empty() { "This PC".into() } else { h }
        })
}

/// Every server the picker offers: this box's bot first (when it has one),
/// then the remote ones in the order they were added. An existing install
/// needs no migration -- its bot simply is the "local" entry.
pub fn list(cfg: &ManagerConfig) -> Vec<ServerView> {
    let mut out = Vec::new();
    if let Some(d) = cfg.bot_dir().filter(|d| bot::is_bot_dir(d)) {
        let (url, problem) = match bot::ops_target(&d) {
            Ok(t) => (Some(t.base), None),
            Err(e) => (None, Some(format!("{e:#}"))),
        };
        out.push(ServerView { id: LOCAL_ID.into(), name: local_name(cfg), url, has_key: problem.is_none(), local: true, problem });
    }
    for s in &cfg.servers {
        let has_key = s.key.as_deref().map(|k| !k.is_empty()).unwrap_or(false);
        out.push(ServerView {
            id: s.id.clone(),
            name: s.name.clone(),
            url: Some(s.url.clone()),
            has_key,
            local: false,
            problem: (!has_key).then(|| "no OPS key saved for it -- edit it and paste the box's ops_api.api_key".into()),
        });
    }
    out
}

/// Add or change a remote server; returns its id. `key` None keeps the
/// saved key, Some("") drops it.
pub fn upsert(cfg: &mut ManagerConfig, input: ServerInput) -> Result<String> {
    let name = input.name.trim().to_string();
    if name.is_empty() {
        bail!("give the server a name (it shows on every confirm, so make it one you can't mix up)");
    }
    if name.chars().count() > 40 {
        bail!("keep the name under 40 characters");
    }
    let url = normalize_url(&input.url)?;
    let key = input.key.map(|k| k.trim().to_string());
    if let Some(k) = key.as_deref().filter(|k| !k.is_empty()) {
        if k.chars().count() < 16 {
            bail!("that key is short -- use the box's ops_api.api_key (32+ random characters)");
        }
        if k.chars().any(|c| c.is_control() || c == ' ') {
            bail!("the key has spaces or control characters in it -- copy just the value");
        }
    }
    let id = input.id.map(|i| i.trim().to_string()).filter(|i| !i.is_empty());
    if id.as_deref() == Some(LOCAL_ID) {
        // only the name of this box's own entry can be changed here; its
        // address and key come from the bot's own config
        cfg.local_name = Some(name);
        return Ok(LOCAL_ID.into());
    }
    if cfg.servers.iter().any(|s| s.name.eq_ignore_ascii_case(&name) && Some(&s.id) != id.as_ref()) {
        bail!("there already is a server called {name}");
    }
    match id {
        Some(id) => {
            let s = cfg.servers.iter_mut().find(|s| s.id == id).ok_or_else(|| anyhow!("no server {id}"))?;
            s.name = name;
            s.url = url;
            if let Some(k) = key {
                s.key = (!k.is_empty()).then_some(k);
            }
            Ok(id)
        }
        None => {
            let taken: Vec<String> = cfg.servers.iter().map(|s| s.id.clone()).collect();
            let id = make_id(&name, &taken);
            cfg.servers.push(RemoteServer { id: id.clone(), name, url, key: key.filter(|k| !k.is_empty()) });
            Ok(id)
        }
    }
}

pub fn remove(cfg: &mut ManagerConfig, id: &str) -> Result<()> {
    if id == LOCAL_ID {
        bail!("this box's own bot can't be removed here -- it is there while Setup has a DCSServerBot folder");
    }
    let before = cfg.servers.len();
    cfg.servers.retain(|s| s.id != id);
    if cfg.servers.len() == before {
        bail!("no server {id}");
    }
    Ok(())
}

/// Where a request for server `id` goes (None = the local bot, as before).
pub fn target(cfg: &ManagerConfig, id: Option<&str>) -> Result<Target> {
    match id.map(str::trim).filter(|i| !i.is_empty() && *i != LOCAL_ID) {
        None => {
            let dir = cfg.bot_dir().ok_or_else(|| {
                if cfg.servers.is_empty() {
                    anyhow!("no DCSServerBot folder configured -- run Setup")
                } else {
                    anyhow!("this PC has no DCSServerBot -- pick one of your servers at the top")
                }
            })?;
            let t = bot::ops_target(&dir)?;
            Ok(Target { id: LOCAL_ID.into(), name: local_name(cfg), base: t.base, key: t.key, local: true })
        }
        Some(id) => {
            let s = cfg.servers.iter().find(|s| s.id == id).ok_or_else(|| anyhow!("no server {id} -- it was removed?"))?;
            let key = s.key.clone().filter(|k| !k.is_empty()).ok_or_else(|| {
                anyhow!("no OPS key saved for {} -- SERVERS → edit it and paste the box's ops_api.api_key", s.name)
            })?;
            // checked again here: manager.json may have been edited by hand
            let base = normalize_url(&s.url)?;
            Ok(Target { id: s.id.clone(), name: s.name.clone(), base, key, local: false })
        }
    }
}

#[derive(Debug, Serialize)]
pub struct OpsResponse {
    pub status: u16,
    pub content_type: String,
    pub body: String,
}

/// One OPS API call. `path` is relative to the OPS base (status, dcs/stop,
/// logs/follow?which=bot...).
pub fn request(t: &Target, method: &str, path: &str, body: Option<String>) -> Result<OpsResponse> {
    let path = path.trim_start_matches('/');
    if path.contains("..") || path.contains('\\') || path.contains("://") || path.starts_with('/') {
        bail!("bad path");
    }
    let url = format!("{}/{}", t.base, path);
    let b = reqwest::blocking::Client::builder()
        .redirect(reqwest::redirect::Policy::none())
        .connect_timeout(Duration::from_secs(if t.local { 3 } else { 6 }));
    let c = b.timeout(Duration::from_secs(if t.local { 300 } else { 120 })).build()?;
    let req = if method.eq_ignore_ascii_case("POST") {
        c.post(&url).header("content-type", "application/json").body(body.unwrap_or_else(|| "{}".into()))
    } else {
        c.get(&url)
    };
    let resp = req.header("X-API-Key", &t.key).send().map_err(|e| {
        if t.local {
            anyhow!("the bot's WebService is not answering at {} ({e}) -- is DCSServerBot running?", t.base)
        } else {
            anyhow!(
                "{} is not answering at {} ({}) -- is Tailscale up on both PCs, DCSServerBot running there, \
                 and its WebService open to the tailnet (deploy/manager-remote.md)?",
                t.name,
                t.base,
                short_err(&e)
            )
        }
    })?;
    let status = resp.status().as_u16();
    let content_type = resp
        .headers()
        .get("content-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("application/json")
        .to_string();
    Ok(OpsResponse { status, content_type, body: resp.text()? })
}

fn short_err(e: &reqwest::Error) -> String {
    if e.is_timeout() {
        "timed out".into()
    } else if e.is_connect() {
        "can't connect".into()
    } else {
        e.to_string()
    }
}

#[derive(Debug, Serialize, Default)]
pub struct TestResult {
    pub ok: bool,
    pub message: String,
    pub url: String,
    pub hostname: Option<String>,
    pub bot_version: Option<String>,
    pub servers: Vec<String>,
    /// The plugin there answers the remote-control routes (dcs, logs/follow);
    /// false = an older fowlengine plugin: update it there.
    pub plugin_current: bool,
}

/// "Test connection": GET status (key + reachability), then GET dcs (is the
/// plugin new enough for remote control).
pub fn test(t: &Target) -> TestResult {
    let mut out = TestResult { url: t.base.clone(), ..Default::default() };
    let r = match request(t, "GET", "status", None) {
        Ok(r) => r,
        Err(e) => {
            out.message = format!("{e:#}");
            return out;
        }
    };
    match r.status {
        200 => {}
        403 => {
            out.message = format!(
                "{} answered, but refused the key (403) -- paste that box's ops_api.api_key again \
                 (with ops_api.require_own_key: true only its own key works)",
                t.name
            );
            return out;
        }
        404 => {
            out.message = format!(
                "{} answered, but has no Fowl Engine OPS API at this path (404) -- check the prefix \
                 (the box's Manager shows its OPS address at the bottom) and that ops_api is enabled there",
                t.name
            );
            return out;
        }
        s => {
            out.message = format!("{} answered {s}: {}", t.name, r.body.chars().take(300).collect::<String>());
            return out;
        }
    }
    let st: serde_json::Value = serde_json::from_str(&r.body).unwrap_or_default();
    out.hostname = st["host"]["hostname"].as_str().map(String::from);
    out.bot_version = st["bot"]["dcsserverbot_version"].as_str().map(String::from);
    out.servers = st["servers"]
        .as_array()
        .map(|a| a.iter().filter_map(|s| s["name"].as_str().map(String::from)).collect())
        .unwrap_or_default();
    out.plugin_current = matches!(request(t, "GET", "dcs", None), Ok(r) if r.status == 200);
    out.ok = true;
    out.message = format!(
        "connected to {}{} -- {} DCS server(s){}",
        out.hostname.as_deref().unwrap_or("?"),
        out.bot_version.as_deref().map(|v| format!(", DCSServerBot {v}")).unwrap_or_default(),
        out.servers.len(),
        if out.plugin_current { String::new() } else { format!("; update the fowlengine plugin on {} for DCS control and live logs", t.name) }
    );
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn urls_are_tailnet_only_and_normalized() {
        assert_eq!(normalize_url("100.101.2.86:9876").unwrap(), "http://100.101.2.86:9876/fowlengine/ops");
        assert_eq!(
            normalize_url("http://100.101.2.86:9876/stats/").unwrap(),
            "http://100.101.2.86:9876/stats/fowlengine/ops"
        );
        assert_eq!(
            normalize_url(" http://100.127.255.1:9876/stats/fowlengine/ops ").unwrap(),
            "http://100.127.255.1:9876/stats/fowlengine/ops"
        );
        assert_eq!(
            normalize_url("range-pc.tail1234.ts.net:9876").unwrap(),
            "http://range-pc.tail1234.ts.net:9876/fowlengine/ops"
        );
        assert_eq!(
            normalize_url("http://[fd7a:115c:a1e0::1]:9876").unwrap(),
            "http://[fd7a:115c:a1e0::1]:9876/fowlengine/ops"
        );
        assert!(normalize_url("127.0.0.1:9876").is_ok());
        // the internet, a LAN, a bare MagicDNS name, other schemes: refused
        // a public 100.x address is not a tailnet one
        let e = normalize_url("100.17.2.86:9876").unwrap_err().to_string();
        assert!(e.contains("not a Tailscale address") && e.contains("tailscale ip -4"), "{e}");
        for bad in [
            "http://203.0.113.5:9876",
            "192.168.1.20:9876",
            "100.128.0.1:9876",
            "100.63.255.255:9876",
            "range-pc:9876",
            "https://example.com/stats",
            "ftp://100.101.2.86",
            "http://user:pw@100.101.2.86:9876",
            "http://100.101.2.86:9876/stats?x=1",
            "",
        ] {
            assert!(normalize_url(bad).is_err(), "{bad} should be refused");
        }
    }

    #[test]
    fn upsert_keeps_or_clears_the_key_and_ids_are_stable() {
        let mut cfg = ManagerConfig::default();
        let key = "k".repeat(40);
        let id = upsert(&mut cfg, ServerInput { id: None, name: "Range PC".into(), url: "100.101.2.86:9876/stats".into(), key: Some(key.clone()) }).unwrap();
        assert_eq!(id, "range-pc");
        assert_eq!(cfg.servers[0].url, "http://100.101.2.86:9876/stats/fowlengine/ops");
        // same name again: refused; another name gets its own id
        assert!(upsert(&mut cfg, ServerInput { id: None, name: "range pc".into(), url: "100.101.2.87:1".into(), key: None }).is_err());
        let id2 = upsert(&mut cfg, ServerInput { id: None, name: "Campaign".into(), url: "100.101.2.84:9876".into(), key: Some(key.clone()) }).unwrap();
        assert_eq!(id2, "campaign");
        // edit without a key keeps it; "" drops it
        upsert(&mut cfg, ServerInput { id: Some(id.clone()), name: "Range".into(), url: "100.101.2.86:9877".into(), key: None }).unwrap();
        assert_eq!(cfg.servers[0].key.as_deref(), Some(key.as_str()));
        assert_eq!(cfg.servers[0].name, "Range");
        upsert(&mut cfg, ServerInput { id: Some(id.clone()), name: "Range".into(), url: "100.101.2.86:9877".into(), key: Some(String::new()) }).unwrap();
        assert!(cfg.servers[0].key.is_none());
        assert!(target(&cfg, Some(&id)).is_err(), "no key, no request");
        // short keys and internet addresses are refused
        assert!(upsert(&mut cfg, ServerInput { id: None, name: "X".into(), url: "100.1.2.3".into(), key: Some("short".into()) }).is_err());
        assert!(upsert(&mut cfg, ServerInput { id: None, name: "Y".into(), url: "8.8.8.8:9876".into(), key: Some(key.clone()) }).is_err());
        // the views never carry the key
        let views = serde_json::to_string(&list(&cfg)).unwrap();
        assert!(!views.contains(&key), "{views}");
        let t = target(&cfg, Some("campaign")).unwrap();
        assert_eq!((t.key.as_str(), t.local), (key.as_str(), false));
        remove(&mut cfg, &id).unwrap();
        assert!(remove(&mut cfg, LOCAL_ID).is_err());
        assert_eq!(cfg.servers.len(), 1);
        // "local" only renames this box
        upsert(&mut cfg, ServerInput { id: Some(LOCAL_ID.into()), name: "Campaign box".into(), url: "127.0.0.1:1".into(), key: None }).unwrap();
        assert_eq!(cfg.local_name.as_deref(), Some("Campaign box"));
    }

    #[test]
    fn ids() {
        assert_eq!(make_id("VS Campaign #1", &[]), "vs-campaign-1");
        assert_eq!(make_id("local", &[]), "server");
        assert_eq!(make_id("Range", &["range".into()]), "range-2");
        assert_eq!(make_id("!!!", &[]), "server");
    }

    #[test]
    fn bad_paths_never_leave_the_ops_base() {
        let t = Target { id: "x".into(), name: "X".into(), base: "http://127.0.0.1:1/fowlengine/ops".into(), key: "k".into(), local: false };
        for p in ["../admin", "a/../../b", "http://evil/x", "\\\\host\\x"] {
            let e = request(&t, "GET", p, None).err().map(|e| e.to_string()).unwrap_or_default();
            assert_eq!(e, "bad path", "{p}");
        }
    }
}
