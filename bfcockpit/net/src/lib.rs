//! HTTPS for the BFNext cockpit plugin.
//!
//! The plugin's UI is drawn with DCS's own dxgui widgets from a Hooks script,
//! which runs on the sim's GUI thread. That Lua state has LuaSocket but no TLS,
//! and anything blocking there -- a DNS lookup, a slow server -- freezes the
//! sim for every frame it takes. So the script never waits on the network:
//!
//! ```lua
//! local http = require('bfcockpit_net')
//! local id = http.request("GET", url)              -- returns at once
//! -- ...later, from onSimulationFrame:
//! for _, r in ipairs(http.poll()) do               -- never blocks
//!     -- r.id, r.status (0 when no response came back), r.body, r.error
//! end
//! ```
//!
//! `update(version, write_dir)` fetches the installer for a newer plugin
//! version and starts it hidden; it waits for DCS to close before installing
//! (see `update` below). Its result comes back through `poll()` like a request.
//!
//! Requests run on a few background threads; results queue up until the next
//! `poll()`. Nothing here touches Lua outside those two calls, so no Lua state
//! is ever used from another thread. Neither call ever raises a Lua error (see
//! `request`): failures come back as values.
use mlua::prelude::*;
use std::{
    io::Read,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicU64, Ordering},
        mpsc, Arc, Mutex, OnceLock,
    },
    thread,
    time::Duration,
};

/// Enough that a slow menu request cannot hold up a status poll behind it.
const WORKERS: usize = 3;
/// The largest body handed back to Lua. A whole F10 menu is tens of KB.
const MAX_BODY: u64 = 4 * 1024 * 1024;
const DEFAULT_TIMEOUT: Duration = Duration::from_secs(15);

/// Where every plugin version's installer is published (a GitHub release per
/// version, `cockpit-v<version>`). Fixed here rather than taken from the
/// server, so nothing a server says can make a player run anything else.
const RELEASES: &str = "https://github.com/Dillen198/bfnext/releases/download";
const INSTALLER: &str = "VectorStrike-Cockpit-Install.exe";
/// The installer is ~6 MB; anything far bigger is not it.
const MAX_INSTALLER: u64 = 64 * 1024 * 1024;

enum Work {
    Http { method: String, url: String, body: Option<String>, timeout: Duration },
    Update { version: String, write_dir: PathBuf },
}

struct Job {
    id: u64,
    work: Work,
}

struct Done {
    id: u64,
    /// 0 when the request never got a response (DNS, TLS, refused, timeout).
    status: u16,
    body: String,
    error: Option<String>,
}

struct Pool {
    tx: Mutex<mpsc::Sender<Job>>,
    done: Arc<Mutex<Vec<Done>>>,
    next_id: AtomicU64,
}

static POOL: OnceLock<Pool> = OnceLock::new();

/// Started on first use, so loading the module costs nothing until the
/// plugin actually talks to a server.
fn pool() -> &'static Pool {
    POOL.get_or_init(|| {
        let (tx, rx) = mpsc::channel::<Job>();
        let rx = Arc::new(Mutex::new(rx));
        let done = Arc::new(Mutex::new(Vec::new()));
        let agent = ureq::AgentBuilder::new()
            .timeout_connect(Duration::from_secs(5))
            .user_agent(concat!("bfcockpit-net/", env!("CARGO_PKG_VERSION")))
            .build();
        for n in 0..WORKERS {
            let rx = rx.clone();
            let done = done.clone();
            let agent = agent.clone();
            let _ = thread::Builder::new()
                .name(format!("bfcockpit-net-{n}"))
                .spawn(move || loop {
                    // Hold the lock only for the hand-off, not the request.
                    let job = match rx.lock() {
                        Ok(rx) => rx.recv(),
                        Err(_) => return,
                    };
                    let Ok(job) = job else { return };
                    let result = run(&agent, job);
                    if let Ok(mut done) = done.lock() {
                        done.push(result);
                    }
                });
        }
        Pool { tx: Mutex::new(tx), done, next_id: AtomicU64::new(1) }
    })
}

fn read_body(resp: ureq::Response) -> Result<String, String> {
    let mut body = String::new();
    resp.into_reader()
        .take(MAX_BODY)
        .read_to_string(&mut body)
        .map_err(|e| format!("reading response: {e}"))?;
    Ok(body)
}

fn run(agent: &ureq::Agent, job: Job) -> Done {
    match job.work {
        Work::Http { method, url, body, timeout } => http(agent, job.id, &method, &url, body, timeout),
        Work::Update { version, write_dir } => match update_now(agent, &version, &write_dir) {
            Ok(msg) => Done { id: job.id, status: 200, body: msg, error: None },
            Err(e) => Done { id: job.id, status: 0, body: String::new(), error: Some(e) },
        },
    }
}

fn http(agent: &ureq::Agent, id: u64, method: &str, url: &str, body: Option<String>, timeout: Duration) -> Done {
    let req = agent
        .request(method, url)
        .timeout(timeout)
        .set("Accept", "application/json");
    let res = match &body {
        Some(body) => req.set("Content-Type", "application/json").send_string(body),
        None => req.call(),
    };
    // A 4xx/5xx is still an answer: bfdb puts the reason in a JSON body
    // ({"error": "..."}), and the panel shows that rather than "HTTP 401".
    let resp = match res {
        Ok(resp) | Err(ureq::Error::Status(_, resp)) => resp,
        Err(ureq::Error::Transport(t)) => {
            return Done { id, status: 0, body: String::new(), error: Some(t.to_string()) }
        }
    };
    let status = resp.status();
    match read_body(resp) {
        Ok(body) => Done { id, status, body, error: None },
        Err(e) => Done { id, status, body: String::new(), error: Some(e) },
    }
}

/// Download the installer for `version` and start it hidden with
/// `--auto-update --write-dir <profile>`. It installs once DCS has let go of
/// this DLL -- that is, after the player closes DCS -- and then exits.
fn update_now(agent: &ureq::Agent, version: &str, write_dir: &Path) -> Result<String, String> {
    let dir = std::env::temp_dir().join("VectorStrikeCockpit");
    std::fs::create_dir_all(&dir).map_err(|e| format!("creating {}: {e}", dir.display()))?;
    let exe = dir.join(format!("VectorStrike-Cockpit-Install-{version}.exe"));
    let part = dir.join(format!("VectorStrike-Cockpit-Install-{version}.part"));

    let url = format!("{RELEASES}/cockpit-v{version}/{INSTALLER}");
    let resp = agent
        .get(&url)
        .timeout(Duration::from_secs(300))
        .call()
        .map_err(|e| format!("downloading {url}: {e}"))?;
    let mut bytes = Vec::new();
    resp.into_reader()
        .take(MAX_INSTALLER + 1)
        .read_to_end(&mut bytes)
        .map_err(|e| format!("downloading {url}: {e}"))?;
    if bytes.len() as u64 > MAX_INSTALLER || !bytes.starts_with(b"MZ") {
        return Err(format!("{url} is not the installer ({} bytes)", bytes.len()));
    }
    std::fs::write(&part, &bytes).map_err(|e| format!("saving {}: {e}", part.display()))?;
    // An installer for this version may already be waiting from an earlier
    // session. It is running, so it can't be replaced -- and it will do the job.
    let _ = std::fs::remove_file(&exe);
    if let Err(e) = std::fs::rename(&part, &exe) {
        let _ = std::fs::remove_file(&part);
        if exe.is_file() {
            return Ok("already waiting".into());
        }
        return Err(format!("saving {}: {e}", exe.display()));
    }
    spawn_detached(&exe, write_dir).map_err(|e| format!("starting {}: {e}", exe.display()))?;
    Ok("waiting for DCS to close".into())
}

#[cfg(windows)]
fn spawn_detached(exe: &Path, write_dir: &Path) -> std::io::Result<()> {
    use std::os::windows::process::CommandExt;
    use std::process::{Command, Stdio};
    const DETACHED_PROCESS: u32 = 0x0000_0008;
    const CREATE_NEW_PROCESS_GROUP: u32 = 0x0000_0200;
    const CREATE_BREAKAWAY_FROM_JOB: u32 = 0x0100_0000;
    const CREATE_NO_WINDOW: u32 = 0x0800_0000;
    let flags = DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP | CREATE_NO_WINDOW;
    let start = |extra: u32| {
        Command::new(exe)
            .arg("--auto-update")
            .arg("--write-dir")
            .arg(write_dir)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .creation_flags(flags | extra)
            .spawn()
    };
    // Out of DCS's job object if it is in one, so closing DCS doesn't take the
    // installer with it; a job that forbids that still lets it start inside.
    start(CREATE_BREAKAWAY_FROM_JOB).or_else(|_| start(0)).map(|_| ())
}

#[cfg(not(windows))]
fn spawn_detached(_: &Path, _: &Path) -> std::io::Result<()> {
    Err(std::io::Error::new(std::io::ErrorKind::Unsupported, "Windows only"))
}

fn lua_str(v: &LuaValue) -> Option<String> {
    match v {
        LuaValue::String(s) => s.to_str().ok().map(str::to_string),
        _ => None,
    }
}

fn queue(method: &LuaValue, url: &LuaValue, body: &LuaValue, timeout: &LuaValue) -> Result<u64, String> {
    let method = lua_str(method).ok_or("method must be a string")?;
    let url = lua_str(url).ok_or("url must be a string")?;
    if !(url.starts_with("https://") || url.starts_with("http://")) {
        return Err("url must start with https:// or http://".into());
    }
    let body = match body {
        LuaValue::Nil => None,
        v => Some(lua_str(v).ok_or("body must be a string or nil")?),
    };
    let timeout = match timeout {
        LuaValue::Integer(n) => Some(*n as f64),
        LuaValue::Number(n) => Some(*n),
        _ => None,
    }
    .filter(|t| t.is_finite() && *t > 0.)
    .map(|t| Duration::from_secs_f64(t.min(120.)))
    .unwrap_or(DEFAULT_TIMEOUT);
    send(Work::Http { method: method.to_ascii_uppercase(), url, body, timeout })
}

fn send(work: Work) -> Result<u64, String> {
    let pool = pool();
    let id = pool.next_id.fetch_add(1, Ordering::Relaxed);
    let job = Job { id, work };
    pool.tx
        .lock()
        .map_err(|_| "worker queue poisoned")?
        .send(job)
        .map_err(|_| "workers have stopped")?;
    Ok(id)
}

/// `request(method, url [, body [, timeout_secs]]) -> id | nil, err`
///
/// Reports bad arguments as `nil, message` and never raises: raising a Lua
/// error from Rust inside DCS's lua.dll corrupts the heap and kills the
/// process (0xC0000374, reproduced with bin-mt/luae.exe 2026-10-20). For the
/// same reason the arguments are taken as raw values -- a typed signature
/// would make mlua raise on a wrong type before this code ever ran.
fn request<'lua>(
    _: &'lua Lua,
    (method, url, body, timeout): (LuaValue<'lua>, LuaValue<'lua>, LuaValue<'lua>, LuaValue<'lua>),
) -> LuaResult<(Option<u64>, Option<String>)> {
    Ok(match queue(&method, &url, &body, &timeout) {
        Ok(id) => (Some(id), None),
        Err(e) => (None, Some(e)),
    })
}

/// A version is digits and dots only -- it goes into a URL and a file name.
fn valid_version(v: &str) -> bool {
    let parts: Vec<&str> = v.split('.').collect();
    (2..=4).contains(&parts.len())
        && parts.iter().all(|p| !p.is_empty() && p.len() <= 5 && p.bytes().all(|b| b.is_ascii_digit()))
}

/// `update(version, write_dir) -> id | nil, err`
///
/// The result arrives through `poll()`: status 200 with a short note in
/// `body`, or status 0 with `error`. Never raises (see `request`).
fn update<'lua>(
    _: &'lua Lua,
    (version, write_dir): (LuaValue<'lua>, LuaValue<'lua>),
) -> LuaResult<(Option<u64>, Option<String>)> {
    let queued = (|| {
        let version = lua_str(&version).ok_or("version must be a string")?;
        if !valid_version(&version) {
            return Err(format!("not a version: {version:?}"));
        }
        let dir = PathBuf::from(lua_str(&write_dir).ok_or("write_dir must be a string")?);
        if !dir.is_absolute() || !dir.is_dir() {
            return Err(format!("not a folder: {}", dir.display()));
        }
        send(Work::Update { version, write_dir: dir })
    })();
    Ok(match queued {
        Ok(id) => (Some(id), None),
        Err(e) => (None, Some(e)),
    })
}

/// `poll() -> { {id=, status=, body=, error=}, ... }`, oldest first. Never
/// blocks; an empty table when nothing has finished.
fn poll(lua: &Lua, _: ()) -> LuaResult<LuaTable<'_>> {
    let out = lua.create_table()?;
    let Some(pool) = POOL.get() else { return Ok(out) };
    let finished = match pool.done.lock() {
        Ok(mut done) => std::mem::take(&mut *done),
        Err(_) => return Ok(out),
    };
    for (i, d) in finished.into_iter().enumerate() {
        let t = lua.create_table()?;
        t.set("id", d.id)?;
        t.set("status", d.status)?;
        t.set("body", d.body)?;
        if let Some(e) = d.error {
            t.set("error", e)?;
        }
        out.set(i + 1, t)?;
    }
    Ok(out)
}

#[mlua::lua_module]
fn bfcockpit_net(lua: &Lua) -> LuaResult<LuaTable<'_>> {
    let m = lua.create_table()?;
    m.set("request", lua.create_function(request)?)?;
    m.set("poll", lua.create_function(poll)?)?;
    m.set("update", lua.create_function(update)?)?;
    m.set("version", env!("CARGO_PKG_VERSION"))?;
    Ok(m)
}
