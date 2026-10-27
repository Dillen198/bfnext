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
//! -- or http.request(method, url, body, timeout, { ["X-Name"] = "value" })
//! -- ...later, from onSimulationFrame:
//! for _, r in ipairs(http.poll()) do               -- never blocks
//!     -- r.id, r.status (0 when no response came back), r.body, r.error
//! end
//! ```
//!
//! `update(version, write_dir)` fetches the installer for a newer plugin
//! version and starts it hidden; it waits for DCS to close before installing
//! (see `update` below). The installer runs only if its signature verifies
//! against the release key compiled into this DLL (`RELEASE_PUBKEY`). Its
//! result comes back through `poll()` like a request.
//!
//! Also, for the script to adopt (none of it needs the network):
//!
//! ```lua
//! http.random_key([len])   -- `len` (16..128, default 40) letters and digits
//!                          -- from the OS CSPRNG (BCryptGenRandom), for the
//!                          -- `-cockpitkey` handshake instead of math.random
//! http.headers             -- true: `request` takes a 5th argument, a table of
//!                          -- extra request headers (absent in older DLLs,
//!                          -- which ignore that argument)
//! http.version             -- this DLL's crate version ("1.0.0")
//! http.script_version      -- the BFCOCKPIT_VERSION of the Hooks script it
//!                          -- was built with, e.g. "3.3.0": compare it with
//!                          -- the script's own to spot a DLL from another build
//! http.git                 -- the commit it was built from ("unknown" outside git)
//! ```
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
/// The largest body handed back to Lua. A whole F10 menu is tens of KB; a
/// bigger body is refused with an error rather than cut short (half a JSON
/// document only fails later, in the script's decoder, with no hint why).
const MAX_BODY: u64 = 4 * 1024 * 1024;

/// What this build is, in one string: the installer's build script reads it
/// back out of the DLL bytes (build_info.rs), and Lua gets it as `build`.
const BUILD_STAMP: &str = concat!(
    "BFCOCKPIT_NET_BUILD;version=",
    env!("CARGO_PKG_VERSION"),
    ";script=",
    env!("BFCOCKPIT_SCRIPT_VERSION"),
    ";git=",
    env!("BFCOCKPIT_NET_GIT"),
    ";END"
);
const DEFAULT_TIMEOUT: Duration = Duration::from_secs(15);

/// Where every plugin version's installer is published (a GitHub release per
/// version, `cockpit-v<version>`). Fixed here rather than taken from the
/// server, so nothing a server says can make a player run anything else.
const RELEASES: &str = "https://github.com/Dillen198/bfnext/releases/download";
const INSTALLER: &str = "VectorStrike-Cockpit-Install.exe";
/// The installer is ~6 MB; anything far bigger is not it.
const MAX_INSTALLER: u64 = 64 * 1024 * 1024;
/// Its minisign signature, published next to it as `<installer>.minisig`
/// (bfcockpit/release.ps1). A signature file is a few hundred bytes.
const SIG_SUFFIX: &str = ".minisig";
const MAX_SIG: u64 = 4096;

/// The public half of the cockpit release key, `fowl-cockpit.key.pub` as
/// `tauri signer generate` wrote it (or a plain minisign `.pub`). A release
/// installer is run only if it verifies against this, so whoever can publish
/// a GitHub release still can't run anything on a player's PC without the
/// private key. Empty until that key exists: a DLL built so refuses every
/// update, and bfcockpit/release.ps1 refuses to publish one.
const RELEASE_PUBKEY: &str = include_str!("../updater.pub");
const NO_KEY: &str =
    "this bfcockpit_net.dll was built without the release key, so it can't check \
                      an update -- install the new version by hand";

enum Work {
    Http {
        method: String,
        url: String,
        body: Option<String>,
        timeout: Duration,
        headers: Vec<(String, String)>,
    },
    Update {
        version: String,
        write_dir: PathBuf,
    },
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
            let _ = thread::Builder::new().name(format!("bfcockpit-net-{n}")).spawn(
                move || loop {
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
                },
            );
        }
        Pool { tx: Mutex::new(tx), done, next_id: AtomicU64::new(1) }
    })
}

fn read_body(resp: ureq::Response) -> Result<String, String> {
    read_limited(resp.into_reader(), MAX_BODY)
}

fn read_limited(r: impl Read, max: u64) -> Result<String, String> {
    let mut bytes = Vec::new();
    r.take(max + 1)
        .read_to_end(&mut bytes)
        .map_err(|e| format!("reading response: {e}"))?;
    if bytes.len() as u64 > max {
        return Err(format!(
            "response body is larger than {} MB -- not passed on",
            max / (1024 * 1024)
        ));
    }
    String::from_utf8(bytes).map_err(|_| "response body is not UTF-8 text".to_string())
}

fn run(agent: &ureq::Agent, job: Job) -> Done {
    match job.work {
        Work::Http { method, url, body, timeout, headers } => {
            http(agent, job.id, &method, &url, body, timeout, &headers)
        }
        Work::Update { version, write_dir } => {
            match update_now(agent, &version, &write_dir) {
                Ok(msg) => Done { id: job.id, status: 200, body: msg, error: None },
                Err(e) => {
                    Done { id: job.id, status: 0, body: String::new(), error: Some(e) }
                }
            }
        }
    }
}

fn http(
    agent: &ureq::Agent,
    id: u64,
    method: &str,
    url: &str,
    body: Option<String>,
    timeout: Duration,
    headers: &[(String, String)],
) -> Done {
    let mut req =
        agent.request(method, url).timeout(timeout).set("Accept", "application/json");
    for (name, value) in headers {
        req = req.set(name, value);
    }
    let res = match &body {
        Some(body) => req.set("Content-Type", "application/json").send_string(body),
        None => req.call(),
    };
    // A 4xx/5xx is still an answer: bfdb puts the reason in a JSON body
    // ({"error": "..."}), and the panel shows that rather than "HTTP 401".
    let resp = match res {
        Ok(resp) | Err(ureq::Error::Status(_, resp)) => resp,
        Err(ureq::Error::Transport(t)) => {
            return Done {
                id,
                status: 0,
                body: String::new(),
                error: Some(t.to_string()),
            }
        }
    };
    let status = resp.status();
    match read_body(resp) {
        Ok(body) => Done { id, status, body, error: None },
        Err(e) => Done { id, status, body: String::new(), error: Some(e) },
    }
}

/// A minisign key or signature file, as it is ("untrusted comment: ...") or
/// base64-wrapped the way `tauri signer` writes both.
fn minisign_text(s: &str) -> Result<String, String> {
    use base64::Engine;
    let s = s.trim();
    if s.starts_with("untrusted comment:") {
        return Ok(s.to_string());
    }
    let bytes = base64::engine::general_purpose::STANDARD
        .decode(s)
        .map_err(|_| "neither a minisign file nor base64 of one".to_string())?;
    String::from_utf8(bytes).map_err(|_| "not a minisign file".to_string())
}

/// Check `data` against a minisign signature with `pubkey`.
fn verify(data: &[u8], sig: &str, pubkey: &str) -> Result<(), String> {
    if pubkey.trim().is_empty() {
        return Err(NO_KEY.into());
    }
    let pk = minisign_verify::PublicKey::decode(&minisign_text(pubkey)?)
        .map_err(|e| format!("bad release key: {e}"))?;
    let sig = minisign_verify::Signature::decode(&minisign_text(sig)?)
        .map_err(|e| format!("bad signature: {e}"))?;
    pk.verify(data, &sig, false).map_err(|e| format!("signature does not match: {e}"))
}

fn download(agent: &ureq::Agent, url: &str, max: u64) -> Result<Vec<u8>, String> {
    let resp = agent
        .get(url)
        .timeout(Duration::from_secs(300))
        .call()
        .map_err(|e| format!("downloading {url}: {e}"))?;
    let mut bytes = Vec::new();
    resp.into_reader()
        .take(max + 1)
        .read_to_end(&mut bytes)
        .map_err(|e| format!("downloading {url}: {e}"))?;
    if bytes.len() as u64 > max {
        return Err(format!("{url} is too big to be what it should be"));
    }
    Ok(bytes)
}

/// Download the installer for `version`, verify its signature, and start it
/// hidden with `--auto-update --write-dir <profile>`. It installs once DCS
/// has let go of this DLL -- that is, after the player closes DCS -- and then
/// exits.
fn update_now(
    agent: &ureq::Agent,
    version: &str,
    write_dir: &Path,
) -> Result<String, String> {
    // before downloading anything: without the key nothing could be run anyway
    if RELEASE_PUBKEY.trim().is_empty() {
        return Err(NO_KEY.into());
    }
    let dir = std::env::temp_dir().join("VectorStrikeCockpit");
    std::fs::create_dir_all(&dir)
        .map_err(|e| format!("creating {}: {e}", dir.display()))?;
    let exe = dir.join(format!("VectorStrike-Cockpit-Install-{version}.exe"));
    let part = dir.join(format!("VectorStrike-Cockpit-Install-{version}.part"));

    let url = format!("{RELEASES}/cockpit-v{version}/{INSTALLER}");
    let sig_url = format!("{url}{SIG_SUFFIX}");
    let sig = String::from_utf8(download(agent, &sig_url, MAX_SIG)?)
        .map_err(|_| format!("{sig_url} is not a signature"))?;
    let bytes = download(agent, &url, MAX_INSTALLER)?;
    if !bytes.starts_with(b"MZ") {
        return Err(format!("{url} is not the installer ({} bytes)", bytes.len()));
    }
    verify(&bytes, &sig, RELEASE_PUBKEY).map_err(|e| {
        format!("refusing {url}: it isn't signed by the cockpit release key ({e})")
    })?;
    std::fs::write(&part, &bytes)
        .map_err(|e| format!("saving {}: {e}", part.display()))?;
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
    // Once more from disk, immediately before it runs: what runs must be what
    // was verified.
    let on_disk =
        std::fs::read(&exe).map_err(|e| format!("reading {}: {e}", exe.display()))?;
    if let Err(e) = verify(&on_disk, &sig, RELEASE_PUBKEY) {
        let _ = std::fs::remove_file(&exe);
        return Err(format!(
            "{} changed on disk since it was verified -- not running it ({e})",
            exe.display()
        ));
    }
    drop(on_disk);
    spawn_detached(&exe, write_dir)
        .map_err(|e| format!("starting {}: {e}", exe.display()))?;
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
        LuaValue::String(s) => s.to_str().ok().map(|s| s.to_string()),
        _ => None,
    }
}

/// At most this many extra headers on one request.
const MAX_HEADERS: usize = 16;

/// A header name (an HTTP token, and not one this module sets itself) and a
/// value of printable ASCII: nothing that could split the request.
fn valid_header(name: &str, value: &str) -> bool {
    const OURS: &[&str] = &[
        "accept",
        "content-type",
        "content-length",
        "host",
        "user-agent",
        "transfer-encoding",
    ];
    !name.is_empty()
        && name.len() <= 64
        && name.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_')
        && !OURS.contains(&name.to_ascii_lowercase().as_str())
        && value.len() <= 1024
        && value.bytes().all(|b| (0x20..0x7f).contains(&b))
}

/// The optional headers table of `request`: nil, or string keys to string
/// values.
fn headers_of(v: &LuaValue) -> Result<Vec<(String, String)>, String> {
    let t = match v {
        LuaValue::Nil => return Ok(Vec::new()),
        LuaValue::Table(t) => t,
        _ => return Err("headers must be a table or nil".into()),
    };
    let mut out = Vec::new();
    for pair in t.clone().pairs::<LuaValue, LuaValue>() {
        let (k, v) = pair.map_err(|e| format!("reading headers: {e}"))?;
        let (Some(name), Some(value)) = (lua_str(&k), lua_str(&v)) else {
            return Err("headers must map strings to strings".into());
        };
        if !valid_header(&name, &value) {
            return Err(format!("header {name:?} is not allowed"));
        }
        if out.len() == MAX_HEADERS {
            return Err(format!("more than {MAX_HEADERS} headers"));
        }
        out.push((name, value));
    }
    Ok(out)
}

fn queue(
    method: &LuaValue,
    url: &LuaValue,
    body: &LuaValue,
    timeout: &LuaValue,
    headers: &LuaValue,
) -> Result<u64, String> {
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
    let headers = headers_of(headers)?;
    send(Work::Http { method: method.to_ascii_uppercase(), url, body, timeout, headers })
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

/// `request(method, url [, body [, timeout_secs [, headers]]]) -> id | nil, err`
///
/// `headers` is a table of extra request headers, `{ ["X-Name"] = "value" }`.
///
/// Reports bad arguments as `nil, message` and never raises: raising a Lua
/// error from Rust inside DCS's lua.dll corrupts the heap and kills the
/// process (0xC0000374, reproduced with bin-mt/luae.exe 2026-10-20). For the
/// same reason the arguments are taken as raw values -- a typed signature
/// would make mlua raise on a wrong type before this code ever ran.
fn request(
    _: &Lua,
    (method, url, body, timeout, headers): (
        LuaValue,
        LuaValue,
        LuaValue,
        LuaValue,
        LuaValue,
    ),
) -> LuaResult<(Option<u64>, Option<String>)> {
    Ok(match queue(&method, &url, &body, &timeout, &headers) {
        Ok(id) => (Some(id), None),
        Err(e) => (None, Some(e)),
    })
}

/// A version is digits and dots only -- it goes into a URL and a file name.
fn valid_version(v: &str) -> bool {
    let parts: Vec<&str> = v.split('.').collect();
    (2..=4).contains(&parts.len())
        && parts.iter().all(|p| {
            !p.is_empty() && p.len() <= 5 && p.bytes().all(|b| b.is_ascii_digit())
        })
}

/// `update(version, write_dir) -> id | nil, err`
///
/// The result arrives through `poll()`: status 200 with a short note in
/// `body`, or status 0 with `error`. Never raises (see `request`).
fn update(
    _: &Lua,
    (version, write_dir): (LuaValue, LuaValue),
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

/// Fill `buf` from the OS CSPRNG.
#[cfg(windows)]
fn os_random(buf: &mut [u8]) -> Result<(), String> {
    #[link(name = "bcrypt")]
    extern "system" {
        fn BCryptGenRandom(
            alg: *mut core::ffi::c_void,
            buf: *mut u8,
            len: u32,
            flags: u32,
        ) -> i32;
    }
    const BCRYPT_USE_SYSTEM_PREFERRED_RNG: u32 = 0x0000_0002;
    // SAFETY: a null algorithm handle is what SYSTEM_PREFERRED_RNG asks for;
    // buf is valid for len bytes.
    let status = unsafe {
        BCryptGenRandom(
            std::ptr::null_mut(),
            buf.as_mut_ptr(),
            buf.len() as u32,
            BCRYPT_USE_SYSTEM_PREFERRED_RNG,
        )
    };
    if status == 0 {
        Ok(())
    } else {
        Err(format!("BCryptGenRandom failed (NTSTATUS {status:#x})"))
    }
}

#[cfg(not(windows))]
fn os_random(buf: &mut [u8]) -> Result<(), String> {
    use std::fs::File;
    File::open("/dev/urandom")
        .and_then(|mut f| f.read_exact(buf))
        .map_err(|e| format!("/dev/urandom: {e}"))
}

const KEY_CHARS: &[u8] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

/// `len` characters from KEY_CHARS, uniformly: bytes of 248 and up are
/// dropped so `% 62` has no bias.
fn new_key(len: usize) -> Result<String, String> {
    let limit = (256 / KEY_CHARS.len() * KEY_CHARS.len()) as u8; // 248
    let mut out = String::with_capacity(len);
    let mut buf = [0u8; 64];
    while out.len() < len {
        os_random(&mut buf)?;
        for &b in buf.iter().filter(|&&b| b < limit) {
            if out.len() == len {
                break;
            }
            out.push(KEY_CHARS[b as usize % KEY_CHARS.len()] as char);
        }
    }
    Ok(out)
}

/// `random_key([len]) -> key | nil, err`
///
/// A key for the `-cockpitkey` handshake from the OS's CSPRNG, in the form
/// bflib accepts (16..128 ASCII letters and digits); `len` defaults to 40, as
/// the script's own key does. Never raises (see `request`).
fn random_key(_: &Lua, len: LuaValue) -> LuaResult<(Option<String>, Option<String>)> {
    let len = match len {
        LuaValue::Nil => Ok(40),
        LuaValue::Integer(n) if (16..=128).contains(&n) => Ok(n as usize),
        LuaValue::Number(n) if n.fract() == 0. && (16. ..=128.).contains(&n) => {
            Ok(n as usize)
        }
        _ => Err("len must be a whole number from 16 to 128".to_string()),
    };
    Ok(match len.and_then(new_key) {
        Ok(k) => (Some(k), None),
        Err(e) => (None, Some(e)),
    })
}

/// `poll() -> { {id=, status=, body=, error=}, ... }`, oldest first. Never
/// blocks; an empty table when nothing has finished.
fn poll(lua: &Lua, _: ()) -> LuaResult<LuaTable> {
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
fn bfcockpit_net(lua: &Lua) -> LuaResult<LuaTable> {
    let m = lua.create_table()?;
    m.set("request", lua.create_function(request)?)?;
    m.set("poll", lua.create_function(poll)?)?;
    m.set("update", lua.create_function(update)?)?;
    m.set("random_key", lua.create_function(random_key)?)?;
    m.set("headers", true)?;
    m.set("version", env!("CARGO_PKG_VERSION"))?;
    m.set("script_version", env!("BFCOCKPIT_SCRIPT_VERSION"))?;
    m.set("git", env!("BFCOCKPIT_NET_GIT"))?;
    m.set("build", BUILD_STAMP)?;
    Ok(m)
}

#[cfg(test)]
mod tests {
    use super::*;
    use base64::Engine;

    // Fowl Engine Manager's fixture, signed with its release key by `tauri
    // signer`: the same tool and format the cockpit's releases are signed with.
    const DATA: &[u8] = include_bytes!("../../../bfmanager/src-tauri/tests/fixture.bin");
    const SIG: &str = include_str!("../../../bfmanager/src-tauri/tests/fixture.bin.sig");
    const PUB: &str = include_str!("../../../bfmanager/src-tauri/updater.pub");

    fn unwrap_b64(s: &str) -> String {
        String::from_utf8(
            base64::engine::general_purpose::STANDARD.decode(s.trim()).unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn verifies_tauri_and_plain_minisign_forms() {
        verify(DATA, SIG, PUB).expect("base64 .sig + base64 .pub");
        // what release.ps1 publishes: the .sig unwrapped into a .minisig
        verify(DATA, &unwrap_b64(SIG), PUB).expect(".minisig + base64 .pub");
        verify(DATA, &unwrap_b64(SIG), &unwrap_b64(PUB)).expect(".minisig + plain .pub");
    }

    #[test]
    fn rejects_tampering_and_garbage() {
        let mut bad = DATA.to_vec();
        bad[0] ^= 1;
        assert!(verify(&bad, SIG, PUB).is_err(), "tampered file must fail");
        assert!(verify(DATA, "", PUB).is_err(), "no signature");
        assert!(verify(DATA, "not a signature", PUB).is_err());
        assert!(verify(DATA, SIG, "not a key").is_err());
    }

    #[test]
    fn random_keys_are_valid_and_differ() {
        let a = new_key(40).unwrap();
        let b = new_key(40).unwrap();
        assert_eq!(a.len(), 40);
        assert!(a.bytes().all(|c| c.is_ascii_alphanumeric()), "{a}");
        assert_ne!(a, b);
        assert_eq!(new_key(128).unwrap().len(), 128);
        // every character turns up over enough draws (no stuck alphabet)
        let many: std::collections::HashSet<char> =
            new_key(4000).unwrap().chars().collect();
        assert_eq!(many.len(), KEY_CHARS.len());
    }

    #[test]
    fn headers_are_checked() {
        assert!(valid_header("X-Cockpit-Key", "abcDEF123"));
        assert!(
            !valid_header(
                "X-Cockpit-Key",
                "a
Host: evil"
            ),
            "no request splitting"
        );
        assert!(!valid_header("Bad Name", "v"));
        assert!(!valid_header("", "v"));
        assert!(!valid_header("Content-Type", "text/plain"), "set by request itself");
        assert!(!valid_header("x-k", &"v".repeat(1025)));
    }

    #[test]
    fn headers_table_from_lua() {
        let lua = Lua::new();
        assert!(headers_of(&LuaValue::Nil).unwrap().is_empty());
        let t: LuaValue = lua.load(r#"{ ["X-Cockpit-Key"] = "abc" }"#).eval().unwrap();
        assert_eq!(
            headers_of(&t).unwrap(),
            vec![("X-Cockpit-Key".to_string(), "abc".to_string())]
        );
        let bad: LuaValue = lua.load(r#"{ ["X-A"] = 5 }"#).eval().unwrap();
        assert!(headers_of(&bad).is_err());
        let list: LuaValue = lua.load(r#"{ "a", "b" }"#).eval().unwrap();
        assert!(headers_of(&list).is_err(), "integer keys are not header names");
        assert!(headers_of(&LuaValue::Boolean(true)).is_err());
    }

    #[test]
    fn oversized_body_is_an_error_not_a_cut() {
        let big = [b'x'; 11];
        assert_eq!(read_limited(&big[..10], 10).unwrap().len(), 10);
        let e = read_limited(&big[..], 10).unwrap_err();
        assert!(e.contains("larger than"), "{e}");
        assert!(read_limited(&[0xff, 0xfe][..], 10).is_err());
    }

    mod stamp {
        include!("../build_info.rs");

        #[test]
        fn build_stamp_reads_back() {
            let dll =
                [b"junk\0".as_slice(), super::super::BUILD_STAMP.as_bytes(), b"\0more"]
                    .concat();
            let (version, script, git) = read_stamp(&dll).expect("stamp");
            assert_eq!(version, env!("CARGO_PKG_VERSION"));
            assert_eq!(script, env!("BFCOCKPIT_SCRIPT_VERSION"));
            assert_eq!(git, env!("BFCOCKPIT_NET_GIT"));
            assert!(read_stamp(b"no stamp here").is_none());
        }
    }

    #[test]
    fn no_key_compiled_in_refuses_everything() {
        let err = verify(DATA, SIG, "  \n").unwrap_err();
        assert!(err.contains("without the release key"), "{err}");
    }

    /// The key compiled into this build is a real minisign public key (or
    /// empty, before the key exists).
    #[test]
    fn compiled_in_key_parses() {
        if !RELEASE_PUBKEY.trim().is_empty() {
            minisign_verify::PublicKey::decode(&minisign_text(RELEASE_PUBKEY).unwrap())
                .expect("bfcockpit/net/updater.pub is not a minisign public key");
        }
    }

    /// `BFCOCKPIT_VERIFY=<installer.exe> cargo test --release -p bfcockpit-net
    /// verify_release_file -- --ignored` checks a built installer against its
    /// `.minisig` with the key compiled into this build.
    #[test]
    #[ignore]
    fn verify_release_file() {
        let path = std::env::var("BFCOCKPIT_VERIFY")
            .expect("set BFCOCKPIT_VERIFY to an installer path");
        let data = std::fs::read(&path).unwrap();
        let sig = std::fs::read_to_string(format!("{path}{SIG_SUFFIX}")).unwrap();
        verify(&data, &sig, RELEASE_PUBKEY).expect("signature must verify");
        let mut bad = data.clone();
        let n = bad.len() / 2;
        bad[n] ^= 0x55;
        assert!(verify(&bad, &sig, RELEASE_PUBKEY).is_err());
    }
}
