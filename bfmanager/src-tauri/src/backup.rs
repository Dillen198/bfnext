//! Whole-box backup and restore: one zip that carries everything a Vector
//! Strike server needs to come back on a freshly installed Windows.
//!
//! What goes in (each one a "root", a folder copied as it is):
//!
//!   * DCSServerBot's folder -- the bot, its plugins and `config\` including
//!     `config\.secret` (Discord token, database password). Not its Python
//!     venv (run.cmd builds that again), caches or logs.
//!   * every DCS server instance (`Saved Games\DCS.*`): Config, Missions,
//!     Scripts, Mods, the campaign save files bflib writes next to them, the
//!     bfdb database (`<home>\bfdb`) and `Logs\stats*` that bfdb reads. Not
//!     tracks, screenshots, shader caches or DCS's own logs.
//!   * bfdb's home when it is somewhere else, and other folders the bot's
//!     config points at (tools, voices) -- offered, ticked when small.
//!   * this app's own settings (manager.json).
//!   * the bot's PostgreSQL database, as a `pg_dump` custom-format dump.
//!
//! What can't go in -- DCS itself, SRS, Python, PostgreSQL -- is written into
//! the manifest so a restore can say what is missing (and install Python and
//! PostgreSQL with winget).
//!
//! A restore unpacks every root to where it was (or wherever the admin maps
//! it), moving anything already there aside rather than deleting it, then
//! rewrites old paths in the config files when a folder or the Windows user
//! changed, renames the bot's node when the PC's name changed, loads the
//! database and installs + starts the service.
//!
//! Both run as a background job in the GUI process; the UI polls job_status().

use crate::config::{self, ManagerConfig};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::{Duration, Instant};

pub const MANIFEST: &str = "fowl-backup.json";
/// 2: Program and Netidx roots, `only` (a 0.2.16 restore would choke on them).
const FORMAT: u32 = 2;
const ZIP_PREFIX: &str = "FowlEngine-backup-";
/// Extra folders bigger than this start unticked (whisper models, etc.).
const EXTRA_DEFAULT_MAX: u64 = 1024 * 1024 * 1024;
/// Text files bigger than this are never path-rewritten.
const REWRITE_MAX: u64 = 8 * 1024 * 1024;

// ---- what a backup holds ------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Kind {
    Bot,
    Instance,
    Bfdb,
    Extra,
    /// An installed program copied whole, Program Files or not (SRS).
    Program,
    /// netidx: netidx.exe (runs the resolver) and its client config.
    Netidx,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Root {
    /// Folder under `roots/` in the zip; stable for one path.
    pub id: String,
    pub kind: Kind,
    pub label: String,
    /// Where it was on the backed-up PC.
    pub path: String,
    #[serde(default)]
    pub files: u64,
    #[serde(default)]
    pub bytes: u64,
    /// Plan only: ticked by default.
    #[serde(default)]
    pub include: bool,
    #[serde(default)]
    pub note: Option<String>,
    /// Only these files (lowercase names) directly in the folder, nothing
    /// below it; empty = the whole folder. netidx.exe out of .cargo\bin.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub only: Vec<String>,
    /// Backup: files found on disk (`files` is how many made it in).
    #[serde(default)]
    pub expected_files: u64,
    /// Backup: files that couldn't be read, or only partly (capped).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub skipped: Vec<String>,
    /// Backup: files gone between listing and copying -- temp files of a
    /// save being written. Not counted in expected_files.
    #[serde(default, skip_serializing_if = "is_zero")]
    pub vanished: u64,
}

fn is_zero(n: &u64) -> bool {
    *n == 0
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DbInfo {
    pub host: String,
    pub port: u16,
    pub name: String,
    pub user: String,
    /// Entry in the zip, when the dump was taken.
    #[serde(default)]
    pub dump: Option<String>,
    #[serde(default)]
    pub bytes: u64,
    /// `pg_dump --version` of the dumping tool: a restore needs pg_restore at
    /// least this new.
    #[serde(default)]
    pub pg_version: Option<String>,
}

/// Something the server needs that a zip can't carry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub what: String,
    pub path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub format: u32,
    pub created: String,
    pub hostname: String,
    pub manager_version: String,
    /// The Windows user the bot ran as, and their profile folder.
    #[serde(default)]
    pub desktop_user: Option<String>,
    #[serde(default)]
    pub profile: Option<String>,
    #[serde(default)]
    pub bot_dir: Option<String>,
    pub roots: Vec<Root>,
    #[serde(default)]
    pub database: Option<DbInfo>,
    #[serde(default)]
    pub programs: Vec<Program>,
    #[serde(default)]
    pub python: Option<String>,
    #[serde(default)]
    pub service_installed: bool,
    #[serde(default)]
    pub warnings: Vec<String>,
}

// ---- the background job ---------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Default)]
pub struct Job {
    pub id: u64,
    /// backup | restore
    pub kind: String,
    pub running: bool,
    pub phase: String,
    pub done_bytes: u64,
    pub total_bytes: u64,
    pub current: Option<String>,
    pub log: Vec<String>,
    pub warnings: Vec<String>,
    pub error: Option<String>,
    /// The zip written (backup) or a summary (restore).
    pub output: Option<String>,
    /// What the admin still has to do by hand.
    pub next_steps: Vec<String>,
    pub started_at: String,
    pub finished_at: Option<String>,
    /// What the zip was checked to hold (backup and verify).
    pub report: Option<VerifyReport>,
    /// The zip this job wrote or checked.
    pub zip_path: Option<String>,
    /// The whole log, saved when the job ends.
    pub log_file: Option<String>,
}

static JOB: Mutex<Option<Job>> = Mutex::new(None);
static DONE: AtomicU64 = AtomicU64::new(0);
static CANCEL: AtomicBool = AtomicBool::new(false);

fn with_job(f: impl FnOnce(&mut Job)) {
    if let Ok(mut g) = JOB.lock() {
        if let Some(j) = g.as_mut() {
            f(j);
        }
    }
}

fn phase(p: &str) {
    log::info!("{p}");
    with_job(|j| {
        j.phase = p.to_string();
        j.log.push(p.to_string());
    });
}

fn note(line: String) {
    log::info!("{line}");
    with_job(|j| j.log.push(line));
}

fn warn(line: String) {
    log::warn!("{line}");
    with_job(|j| {
        j.log.push(format!("WARNING: {line}"));
        j.warnings.push(line);
    });
}

fn next_step(s: String) {
    with_job(|j| j.next_steps.push(s));
}

fn cancelled() -> Result<()> {
    if CANCEL.load(Ordering::Relaxed) {
        bail!("cancelled");
    }
    Ok(())
}

pub fn job_status() -> Option<Job> {
    let mut j = JOB.lock().ok()?.clone()?;
    j.done_bytes = DONE.load(Ordering::Relaxed);
    if j.log.len() > 400 {
        j.log.drain(..j.log.len() - 400);
    }
    Some(j)
}

pub fn cancel_job() {
    CANCEL.store(true, Ordering::Relaxed);
}

fn spawn(kind: &str, f: impl FnOnce() -> Result<String> + Send + 'static) -> Result<()> {
    let mut g = JOB.lock().map_err(|_| anyhow!("job lock poisoned"))?;
    if g.as_ref().map(|j| j.running).unwrap_or(false) {
        bail!("a {} is already running", g.as_ref().map(|j| j.kind.clone()).unwrap_or_default());
    }
    let id = g.as_ref().map(|j| j.id + 1).unwrap_or(1);
    *g = Some(Job { id, kind: kind.into(), running: true, started_at: now(), ..Default::default() });
    drop(g);
    DONE.store(0, Ordering::Relaxed);
    CANCEL.store(false, Ordering::Relaxed);
    std::thread::spawn(move || {
        let r = f();
        with_job(|j| {
            j.current = None;
            j.finished_at = Some(now());
            match r {
                Ok(out) => {
                    j.phase = "done".into();
                    j.output = Some(out);
                }
                Err(e) => {
                    j.phase = "failed".into();
                    j.error = Some(format!("{e:#}"));
                    log::error!("{} failed: {e:#}", j.kind);
                }
            }
        });
        // the log is on disk before anyone sees the job as finished
        save_log();
        with_job(|j| j.running = false);
    });
    Ok(())
}

/// The whole job as plain text: log, report, warnings, next steps.
fn job_text(j: &Job) -> String {
    let mut t = format!(
        "Fowl Engine Manager {} -- {} started {}, finished {}\nresult: {}\n\n",
        crate::update::current_version(),
        j.kind,
        j.started_at,
        j.finished_at.as_deref().unwrap_or("-"),
        j.error.as_deref().map(|e| format!("FAILED: {e}")).or_else(|| j.output.clone()).unwrap_or_default()
    );
    if let Some(r) = &j.report {
        t.push_str(&r.to_text());
        t.push('\n');
    }
    if !j.warnings.is_empty() {
        t.push_str("WARNINGS\n");
        for w in &j.warnings {
            t.push_str(&format!("  - {w}\n"));
        }
        t.push('\n');
    }
    if !j.next_steps.is_empty() {
        t.push_str("NEXT\n");
        for (i, s) in j.next_steps.iter().enumerate() {
            t.push_str(&format!("  {}. {s}\n", i + 1));
        }
        t.push('\n');
    }
    t.push_str("LOG\n");
    for l in &j.log {
        t.push_str(l);
        t.push('\n');
    }
    t
}

/// Write the finished job's log to logs\<kind>-<time>.log, and a backup's
/// also next to its zip (so the report travels with it).
fn save_log() {
    let Some(j) = JOB.lock().ok().and_then(|g| g.clone()) else { return };
    let text = job_text(&j);
    let stamp = chrono::Local::now().format("%Y%m%d-%H%M%S");
    let _ = config::ensure_dirs();
    let main = config::logs_dir().join(format!("{}-{stamp}.log", j.kind));
    let mut saved = std::fs::write(&main, &text).ok().map(|_| disp(&main));
    if j.kind == "backup" {
        if let Some(zip) = j.zip_path.as_deref().filter(|_| j.error.is_none()) {
            let side = PathBuf::from(zip).with_extension("log");
            if std::fs::write(&side, &text).is_ok() {
                saved = Some(disp(&side));
            }
        }
    }
    with_job(|j| j.log_file = saved);
}

fn now() -> String {
    chrono::Local::now().to_rfc3339()
}

// ---- small helpers ----------------------------------------------------------------------

fn hidden(program: &str) -> std::process::Command {
    #[allow(unused_mut)]
    let mut c = std::process::Command::new(program);
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        c.creation_flags(0x0800_0000); // CREATE_NO_WINDOW
    }
    c
}

fn disp(p: &Path) -> String {
    crate::botcfg::display(p)
}

fn lower(s: &str) -> String {
    s.to_lowercase().replace('/', "\\").trim_end_matches('\\').to_string()
}

/// `p` is `base` or inside it (Windows rules: case-insensitive, either slash).
pub fn is_under(p: &str, base: &str) -> bool {
    let (p, b) = (lower(p), lower(base));
    !b.is_empty() && (p == b || p.starts_with(&format!("{b}\\")))
}

fn short_hash(s: &str) -> String {
    hex::encode(Sha256::digest(lower(s).as_bytes()))[..6].to_string()
}

fn slug(s: &str) -> String {
    let s: String = s
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() || c == '.' || c == '-' || c == '_' { c } else { '_' })
        .collect();
    s.trim_matches('_').chars().take(40).collect()
}

fn root_id(kind: Kind, path: &Path) -> String {
    if kind == Kind::Bot {
        return "bot".into();
    }
    let name = path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_else(|| "root".into());
    let k = match kind {
        Kind::Bot => "bot",
        Kind::Instance => "instance",
        Kind::Bfdb => "bfdb",
        Kind::Extra => "extra",
        Kind::Program => "program",
        Kind::Netidx => "netidx",
    };
    format!("{k}-{}-{}", slug(&name), short_hash(&path.display().to_string()))
}

pub fn fmt_bytes(b: u64) -> String {
    let f = b as f64;
    if f >= 1024.0 * 1024.0 * 1024.0 {
        format!("{:.1} GB", f / (1024.0 * 1024.0 * 1024.0))
    } else if f >= 1024.0 * 1024.0 {
        format!("{:.0} MB", f / (1024.0 * 1024.0))
    } else {
        format!("{:.0} KB", (f / 1024.0).max(1.0))
    }
}

// ---- which files of a root ------------------------------------------------------------

/// DCS's own instance folders that are caches or recordings, never needed.
const INSTANCE_SKIP: [&str; 9] =
    ["tracks", "screenshots", "temp", "movie", "fxo", "metashaders", "metashaders2", "trackdata", "crash"];
const BOT_SKIP: [&str; 4] = ["logs", ".venv", "venv", "node_modules"];

/// `rel` is lowercase, '/'-separated, relative to the root.
pub fn excluded(kind: Kind, rel: &str, is_dir: bool) -> bool {
    let parts: Vec<&str> = rel.split('/').collect();
    let first = parts[0];
    if parts.iter().any(|p| *p == "__pycache__") {
        return true;
    }
    if !is_dir && (rel.ends_with(".pyc") || rel.ends_with(".partial")) {
        return true;
    }
    match kind {
        Kind::Bot => {
            BOT_SKIP.contains(&first) || (!is_dir && parts.len() == 1 && rel.ends_with(".pid"))
        }
        Kind::Instance => {
            if INSTANCE_SKIP.contains(&first) {
                return true;
            }
            // Logs: only what bfdb reads (Logs\stats\, Logs\stats.jsonl*)
            if first == "logs" && parts.len() > 1 {
                return !parts[1].starts_with("stats");
            }
            false
        }
        Kind::Bfdb | Kind::Extra | Kind::Program | Kind::Netidx => false,
    }
}

pub struct FileEntry {
    pub abs: PathBuf,
    /// '/'-separated, relative to the root, original case
    pub rel: String,
    pub size: u64,
}

/// Every file of a root, links not followed (they are reported).
pub fn walk_root(r: &Root, out: &mut Vec<FileEntry>, links: &mut Vec<String>) {
    let root = Path::new(&r.path);
    if r.only.is_empty() {
        return walk(root, r.kind, out, links);
    }
    for name in &r.only {
        let p = root.join(name);
        if let Ok(meta) = std::fs::metadata(&p) {
            if meta.is_file() {
                let rel = p.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_else(|| name.clone());
                out.push(FileEntry { abs: p, rel, size: meta.len() });
            }
        }
    }
}

pub fn walk(root: &Path, kind: Kind, out: &mut Vec<FileEntry>, links: &mut Vec<String>) {
    fn go(dir: &Path, prefix: &str, kind: Kind, out: &mut Vec<FileEntry>, links: &mut Vec<String>) {
        let Ok(rd) = std::fs::read_dir(dir) else { return };
        for e in rd.flatten() {
            let name = e.file_name().to_string_lossy().to_string();
            let rel = if prefix.is_empty() { name.clone() } else { format!("{prefix}/{name}") };
            let Ok(meta) = std::fs::symlink_metadata(e.path()) else { continue };
            if meta.file_type().is_symlink() {
                links.push(disp(&e.path()));
                continue;
            }
            if excluded(kind, &rel.to_lowercase(), meta.is_dir()) {
                continue;
            }
            if meta.is_dir() {
                go(&e.path(), &rel, kind, out, links);
            } else if meta.is_file() {
                out.push(FileEntry { abs: e.path(), rel, size: meta.len() });
            }
        }
    }
    go(root, "", kind, out, links);
}

fn measure(root: &mut Root) -> Vec<String> {
    let mut files = Vec::new();
    let mut links = Vec::new();
    walk_root(root, &mut files, &mut links);
    root.files = files.len() as u64;
    root.bytes = files.iter().map(|f| f.size).sum();
    links
}

// ---- reading the bot's config ---------------------------------------------------------

fn read_yaml(p: &Path) -> Option<serde_yaml::Value> {
    let text = std::fs::read_to_string(p).ok()?;
    serde_yaml::from_str(&text).ok()
}

fn yaml_at<'a>(cfg_dir: &Path, rel: &str, cache: &'a mut Vec<(String, serde_yaml::Value)>) -> Option<&'a serde_yaml::Value> {
    if !cache.iter().any(|(r, _)| r == rel) {
        let mut p = cfg_dir.to_path_buf();
        for part in rel.split('/') {
            p.push(part);
        }
        cache.push((rel.to_string(), read_yaml(&p)?));
    }
    cache.iter().find(|(r, _)| r == rel).map(|(_, v)| v)
}

/// This node's block of nodes.yaml: keyed by the computer name, else the
/// only/first node.
fn node_block(nodes: &serde_yaml::Value, host: &str) -> Option<(String, serde_yaml::Value)> {
    let m = nodes.as_mapping()?;
    let mut first = None;
    for (k, v) in m {
        let Some(k) = k.as_str() else { continue };
        if !v.is_mapping() {
            continue;
        }
        if k.eq_ignore_ascii_case(host) {
            return Some((k.to_string(), v.clone()));
        }
        first.get_or_insert((k.to_string(), v.clone()));
    }
    first
}

fn str_at<'a>(v: &'a serde_yaml::Value, path: &[&str]) -> Option<&'a str> {
    let mut cur = v;
    for p in path {
        cur = cur.get(*p)?;
    }
    cur.as_str().map(str::trim).filter(|s| !s.is_empty())
}

/// Every string in a YAML document that is an absolute Windows path.
fn abs_paths(v: &serde_yaml::Value, out: &mut Vec<String>) {
    match v {
        serde_yaml::Value::String(s) => {
            let s = s.trim();
            let b = s.as_bytes();
            if b.len() > 3 && b[0].is_ascii_alphabetic() && b[1] == b':' && (b[2] == b'\\' || b[2] == b'/') {
                out.push(s.to_string());
            }
        }
        serde_yaml::Value::Sequence(seq) => seq.iter().for_each(|x| abs_paths(x, out)),
        serde_yaml::Value::Mapping(m) => m.values().for_each(|x| abs_paths(x, out)),
        serde_yaml::Value::Tagged(t) => abs_paths(&t.value, out),
        _ => {}
    }
}

fn system_drive() -> String {
    std::env::var("SystemDrive").unwrap_or_else(|_| "C:".into())
}

/// Folders a backup never offers: Windows and installed programs (those are
/// reinstalled, not copied) and this app's own data.
fn program_location(p: &str) -> bool {
    let sd = system_drive();
    let mut bases = vec![
        format!("{sd}\\Windows"),
        format!("{sd}\\Program Files"),
        format!("{sd}\\Program Files (x86)"),
        format!("{sd}\\ProgramData"),
    ];
    for v in ["ProgramFiles", "ProgramFiles(x86)", "ProgramW6432", "windir"] {
        if let Ok(x) = std::env::var(v) {
            bases.push(x);
        }
    }
    bases.iter().any(|b| is_under(p, b))
}

fn profile_of(p: &str) -> Option<String> {
    // X:\Users\<name>\...
    let parts: Vec<&str> = p.split(['\\', '/']).collect();
    (parts.len() >= 3 && parts[1].eq_ignore_ascii_case("users")).then(|| format!("{}\\{}\\{}", parts[0], parts[1], parts[2]))
}

// ---- PostgreSQL -----------------------------------------------------------------------

/// The bot's database, from nodes.yaml's `database.url` (password from
/// `config\.secret\database.pkl` when the URL says SECRET, as the bot does).
pub struct DbConn {
    pub host: String,
    pub port: u16,
    pub name: String,
    pub user: String,
    pub password: Option<String>,
}

pub fn parse_db_url(s: &str) -> Option<DbConn> {
    let u = url::Url::parse(s.trim()).ok()?;
    if !u.scheme().starts_with("postgres") {
        return None;
    }
    let pw = u.password().map(|p| percent_decode(p)).filter(|p| p != "SECRET" && !p.is_empty());
    Some(DbConn {
        host: u.host_str().unwrap_or("127.0.0.1").to_string(),
        port: u.port().unwrap_or(5432),
        name: u.path().trim_start_matches('/').to_string(),
        user: percent_decode(u.username()),
        password: pw,
    })
}

fn percent_decode(s: &str) -> String {
    let b = s.as_bytes();
    let mut out = Vec::with_capacity(b.len());
    let mut i = 0;
    while i < b.len() {
        if b[i] == b'%' && i + 2 < b.len() {
            if let Ok(v) = u8::from_str_radix(&s[i + 1..i + 3], 16) {
                out.push(v);
                i += 3;
                continue;
            }
        }
        out.push(b[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}

/// The string in a Python pickle of one `str` (what DCSServerBot's
/// utils.set_password writes into config\.secret\*.pkl).
pub fn unpickle_str(b: &[u8]) -> Option<String> {
    let mut i = 0;
    while i < b.len() {
        match b[i] {
            0x80 => i += 2,                  // PROTO n
            0x95 => i += 9,                  // FRAME 8-byte length
            0x8c => {
                // SHORT_BINUNICODE
                let n = *b.get(i + 1)? as usize;
                return String::from_utf8(b.get(i + 2..i + 2 + n)?.to_vec()).ok();
            }
            b'X' => {
                let n = u32::from_le_bytes(b.get(i + 1..i + 5)?.try_into().ok()?) as usize;
                return String::from_utf8(b.get(i + 5..i + 5 + n)?.to_vec()).ok();
            }
            0x8d => {
                let n = u64::from_le_bytes(b.get(i + 1..i + 9)?.try_into().ok()?) as usize;
                return String::from_utf8(b.get(i + 9..i + 9 + n)?.to_vec()).ok();
            }
            b'V' => {
                // protocol 0: raw-unicode-escape up to a newline
                let rest = b.get(i + 1..)?;
                let end = rest.iter().position(|c| *c == b'\n')?;
                return String::from_utf8(rest[..end].to_vec()).ok();
            }
            _ => return None,
        }
    }
    None
}

fn bot_db(bot_dir: &Path) -> Option<DbConn> {
    let cfg = crate::botcfg::config_dir(bot_dir);
    let nodes = read_yaml(&cfg.join("nodes.yaml"))?;
    let host = std::env::var("COMPUTERNAME").unwrap_or_default();
    let (_, node) = node_block(&nodes, &host)?;
    let url = str_at(&node, &["database", "url"])
        .map(String::from)
        .or_else(|| read_yaml(&cfg.join("main.yaml")).and_then(|m| str_at(&m, &["database", "url"]).map(String::from)))?;
    let mut c = parse_db_url(&url)?;
    if c.password.is_none() {
        c.password = std::fs::read(cfg.join(".secret").join("database.pkl")).ok().and_then(|b| unpickle_str(&b));
    }
    Some(c)
}

/// PostgreSQL installs on this PC (bin folders), newest first.
pub fn postgres_bins() -> Vec<(u32, PathBuf)> {
    let mut found: Vec<(u32, PathBuf)> = Vec::new();
    let mut push = |base: PathBuf| {
        let bin = base.join("bin");
        if bin.join("pg_restore.exe").is_file() || bin.join("pg_restore").is_file() {
            let major = base
                .file_name()
                .and_then(|n| n.to_string_lossy().split('.').next().and_then(|m| m.parse().ok()))
                .unwrap_or(0);
            if !found.iter().any(|(_, b)| b == &bin) {
                found.push((major, bin));
            }
        }
    };
    // the EDB installer's registry entries
    if let Ok(out) = hidden("reg.exe").args(["query", r"HKLM\SOFTWARE\PostgreSQL\Installations", "/s"]).output() {
        for l in String::from_utf8_lossy(&out.stdout).lines() {
            if let Some(v) = l.trim().strip_prefix("Base Directory").and_then(|r| r.split("REG_SZ").nth(1)) {
                push(PathBuf::from(v.trim()));
            }
        }
    }
    for pf in ["ProgramFiles", "ProgramW6432"] {
        if let Some(base) = std::env::var_os(pf).map(|p| PathBuf::from(p).join("PostgreSQL")) {
            if let Ok(rd) = std::fs::read_dir(base) {
                rd.flatten().for_each(|e| push(e.path()));
            }
        }
    }
    found.sort_by(|a, b| b.0.cmp(&a.0));
    found
}

fn exe(bin: &Path, name: &str) -> PathBuf {
    let p = bin.join(format!("{name}.exe"));
    if p.is_file() {
        p
    } else {
        bin.join(name)
    }
}

fn tool_version(p: &Path) -> Option<String> {
    let out = hidden(&p.display().to_string()).arg("--version").output().ok()?;
    let s = String::from_utf8_lossy(&out.stdout).trim().to_string();
    (!s.is_empty()).then_some(s)
}

/// "pg_dump (PostgreSQL) 16.4" -> 16
fn pg_major(v: &str) -> Option<u32> {
    v.split_whitespace().last()?.split('.').next()?.parse().ok()
}

fn is_local_host(h: &str) -> bool {
    matches!(h.to_ascii_lowercase().as_str(), "127.0.0.1" | "localhost" | "::1" | "[::1]")
        || std::env::var("COMPUTERNAME").map(|c| c.eq_ignore_ascii_case(h)).unwrap_or(false)
}

// ---- Python ---------------------------------------------------------------------------

const MACHINE_ENV: &str = r"HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment";

/// The machine PATH as the registry holds it now (a winget install or a
/// restore may just have changed it): (value type, raw value).
fn machine_path_raw() -> Option<(String, String)> {
    let out = hidden("reg.exe").args(["query", MACHINE_ENV, "/v", "Path"]).output().ok()?;
    let text = String::from_utf8_lossy(&out.stdout).to_string();
    text.lines().find_map(|l| {
        let l = l.trim_start();
        if !l.to_ascii_lowercase().starts_with("path ") {
            return None;
        }
        for t in ["REG_EXPAND_SZ", "REG_SZ"] {
            if let Some(v) = l.split_once(t).map(|x| x.1) {
                return Some((t.to_string(), v.trim().to_string()));
            }
        }
        None
    })
}

/// This process's PATH plus the machine PATH from the registry.
fn path_dirs() -> Vec<String> {
    let mut paths: Vec<String> = std::env::var("PATH").unwrap_or_default().split(';').map(String::from).collect();
    if let Some((_, v)) = machine_path_raw() {
        paths.extend(v.split(';').map(String::from));
    }
    paths.into_iter().map(|p| p.trim().to_string()).filter(|p| !p.is_empty()).collect()
}

/// The folder holding netidx.exe: the bot user's .cargo\bin (where
/// `cargo install netidx-tools` puts it), else anywhere on PATH.
pub fn find_netidx(profile: Option<&Path>) -> Option<PathBuf> {
    let mut cands: Vec<PathBuf> = Vec::new();
    if let Some(p) = profile {
        cands.push(p.join(".cargo").join("bin"));
    }
    cands.extend(path_dirs().into_iter().map(PathBuf::from));
    cands.into_iter().find(|d| d.join("netidx.exe").is_file())
}

/// Folders netidx reads client.json from (netidx's Config::load_default):
/// %NETIDX_CFG%, %APPDATA%\netidx, ~\.config\netidx, C:\netidx -- the ones
/// that exist, for the bot's user.
pub fn netidx_config_dirs(profile: Option<&Path>) -> Vec<PathBuf> {
    let mut c: Vec<PathBuf> = Vec::new();
    if let Some(f) = std::env::var_os("NETIDX_CFG").map(PathBuf::from) {
        if let Some(d) = f.parent() {
            c.push(d.to_path_buf());
        }
    }
    if let Some(p) = profile {
        c.push(p.join("AppData").join("Roaming").join("netidx"));
        c.push(p.join(".config").join("netidx"));
    }
    c.push(PathBuf::from(format!("{}\\netidx", system_drive())));
    let mut out: Vec<PathBuf> = Vec::new();
    for d in c {
        if d.is_dir() && !out.iter().any(|o| lower(&disp(o)) == lower(&disp(&d))) {
            out.push(d);
        }
    }
    out
}

/// Put `dir` on the machine PATH if it isn't there, so procman (in the bot's
/// session) finds netidx.exe. Refuses to write a PATH it couldn't read
/// properly rather than risk clobbering it.
fn ensure_on_machine_path(dir: &Path) -> Result<bool> {
    // the round-trip test must not edit the dev box's real PATH
    if cfg!(test) {
        return Ok(false);
    }
    let d = disp(dir);
    let (ty, cur) = machine_path_raw().ok_or_else(|| anyhow!("couldn't read the machine PATH"))?;
    if !cur.to_ascii_lowercase().contains("system32") {
        bail!("the machine PATH read back looks wrong -- not touching it");
    }
    if cur.split(';').any(|p| lower(p.trim()) == lower(&d)) {
        return Ok(false);
    }
    let new = format!("{};{d}", cur.trim_end_matches(';'));
    let out = hidden("reg.exe").args(["add", MACHINE_ENV, "/v", "Path", "/t", &ty, "/d", &new, "/f"]).output()?;
    if !out.status.success() {
        bail!("{}", String::from_utf8_lossy(&out.stderr).trim());
    }
    Ok(true)
}

/// A real python.exe (not the Store alias) on PATH or in the usual places,
/// with its version.
pub fn find_python() -> Option<(PathBuf, String)> {
    let mut cands: Vec<PathBuf> = Vec::new();
    for p in path_dirs() {
        let p = p.trim();
        if p.is_empty() || p.to_lowercase().contains("windowsapps") {
            continue;
        }
        cands.push(PathBuf::from(p).join("python.exe"));
    }
    for pf in ["ProgramFiles", "LOCALAPPDATA"] {
        let base = match pf {
            "LOCALAPPDATA" => std::env::var_os(pf).map(|p| PathBuf::from(p).join("Programs").join("Python")),
            _ => std::env::var_os(pf).map(PathBuf::from),
        };
        if let Some(Ok(rd)) = base.map(std::fs::read_dir) {
            for e in rd.flatten() {
                if e.file_name().to_string_lossy().to_lowercase().starts_with("python3") {
                    cands.push(e.path().join("python.exe"));
                }
            }
        }
    }
    for c in cands {
        if c.is_file() {
            if let Ok(out) = hidden(&c.display().to_string()).arg("--version").output() {
                let v = format!("{}{}", String::from_utf8_lossy(&out.stdout), String::from_utf8_lossy(&out.stderr))
                    .trim()
                    .to_string();
                if v.starts_with("Python 3") {
                    return Some((c, v));
                }
            }
        }
    }
    None
}

/// "Python 3.12.4" -> at least 3.11 (what DCSServerBot's run.cmd demands)
pub fn python_ok(v: &str) -> bool {
    let mut it = v.trim_start_matches("Python").trim().split('.');
    let (Some(Ok(a)), Some(Ok(b))) = (it.next().map(str::parse::<u32>), it.next().map(str::parse::<u32>)) else {
        return false;
    };
    (a, b) >= (3, 11)
}

// ---- processes -------------------------------------------------------------------------

fn running(names: &[&str]) -> Vec<String> {
    let mut sys = sysinfo::System::new();
    sys.refresh_processes(sysinfo::ProcessesToUpdate::All, true);
    let mut out: Vec<String> = sys
        .processes()
        .values()
        .map(|p| p.name().to_string_lossy().to_string())
        .filter(|n| names.iter().any(|w| n.eq_ignore_ascii_case(w)))
        .collect();
    out.sort();
    out.dedup();
    out
}

const DCS_EXES: [&str; 2] = ["DCS.exe", "DCS_server.exe"];

fn agent_status() -> Option<serde_json::Value> {
    let st: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(config::status_path()).ok()?).ok()?;
    let fresh = st["heartbeat"]
        .as_str()
        .and_then(|h| chrono::DateTime::parse_from_rfc3339(h).ok())
        .map(|t| (chrono::Local::now().fixed_offset() - t).num_seconds() < 30)
        .unwrap_or(false);
    fresh.then_some(st)
}

fn bot_running() -> bool {
    agent_status().and_then(|s| s["bot"]["running"].as_bool()).unwrap_or(false)
}

// ---- drives ----------------------------------------------------------------------------

#[cfg(windows)]
fn wide(s: &str) -> Vec<u16> {
    s.encode_utf16().chain(std::iter::once(0)).collect()
}

/// Free bytes on the drive holding `p`.
pub fn free_space(p: &Path) -> Option<u64> {
    #[cfg(windows)]
    {
        use windows_sys::Win32::Storage::FileSystem::GetDiskFreeSpaceExW;
        let mut probe = p.to_path_buf();
        while !probe.exists() {
            probe = probe.parent()?.to_path_buf();
        }
        let w = wide(&probe.display().to_string());
        let mut free: u64 = 0;
        let ok = unsafe { GetDiskFreeSpaceExW(w.as_ptr(), &mut free, std::ptr::null_mut(), std::ptr::null_mut()) };
        (ok != 0).then_some(free)
    }
    #[cfg(not(windows))]
    {
        let _ = p;
        None
    }
}

/// Fixed and removable drives, the system drive last.
fn data_drives() -> Vec<String> {
    let sd = system_drive().to_uppercase();
    let mut out = Vec::new();
    for d in 'C'..='Z' {
        let root = format!("{d}:\\");
        #[cfg(windows)]
        let usable = {
            use windows_sys::Win32::Storage::FileSystem::GetDriveTypeW;
            let t = unsafe { GetDriveTypeW(wide(&root).as_ptr()) };
            t == 2 || t == 3 // DRIVE_REMOVABLE, DRIVE_FIXED
        };
        #[cfg(not(windows))]
        let usable = Path::new(&root).exists();
        if usable && format!("{d}:") != sd {
            out.push(format!("{d}:"));
        }
    }
    out.push(sd);
    out
}

// ---- backup ----------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct DbPlan {
    pub host: String,
    pub port: u16,
    pub name: String,
    pub user: String,
    pub pg_dump: Option<String>,
    /// Why it can't be dumped from here, if it can't.
    pub problem: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct BackupPlan {
    pub roots: Vec<Root>,
    pub database: Option<DbPlan>,
    pub programs: Vec<Program>,
    pub default_dest: String,
    pub hostname: String,
    pub bot_running: bool,
    pub service_running: bool,
    pub bfdb_running: bool,
    pub dcs_running: bool,
    pub warnings: Vec<String>,
}

pub fn backup_plan() -> Result<BackupPlan> {
    let cfg = ManagerConfig::load();
    let bot_dir = cfg.bot_dir().filter(|d| crate::bot::is_bot_dir(d)).ok_or_else(|| {
        anyhow!("no DCSServerBot folder is set up in this app yet -- Setup step 1 first, so the backup knows what to copy")
    })?;
    let cfg_dir = crate::botcfg::config_dir(&bot_dir);
    let host = std::env::var("COMPUTERNAME").unwrap_or_default();
    let mut warnings = Vec::new();
    let mut cache = Vec::new();
    let mut roots: Vec<Root> = Vec::new();
    let mut programs: Vec<Program> = Vec::new();

    roots.push(Root {
        id: "bot".into(),
        kind: Kind::Bot,
        label: "DCSServerBot (bot, plugins, config + secrets)".into(),
        path: disp(&bot_dir),
        files: 0,
        bytes: 0,
        include: true,
        note: Some("without its Python venv (run.cmd builds it again), caches and logs".into()),
        only: vec![],
        expected_files: 0,
        vanished: 0,
        skipped: vec![],
    });

    // DCS server instances
    let mut homes: Vec<(String, String)> = Vec::new();
    if let Some(nodes) = yaml_at(&cfg_dir, "nodes.yaml", &mut cache).cloned() {
        if let Some((_, node)) = node_block(&nodes, &host) {
            if let Some(inst) = node.get("instances").and_then(|i| i.as_mapping()) {
                for (name, v) in inst {
                    if let (Some(name), Some(home)) = (name.as_str(), str_at(v, &["home"])) {
                        homes.push((name.to_string(), home.to_string()));
                    }
                }
            }
            if let Some(p) = str_at(&node, &["DCS", "installation"]) {
                programs.push(Program { what: "DCS World Server".into(), path: p.into() });
            }
            if let Some(p) = str_at(&node, &["extensions", "SRS", "installation"]) {
                // small, and copying it keeps the exact version + its server
                // settings: restored to the same place, no installer needed
                if Path::new(p).is_dir() {
                    roots.push(Root {
                        id: root_id(Kind::Program, Path::new(p)),
                        kind: Kind::Program,
                        label: "SRS server (DCS-SimpleRadio Standalone, the whole program folder)".into(),
                        path: p.into(),
                        files: 0,
                        bytes: 0,
                        include: true,
                        note: Some("restored to the same folder -- no SRS installer needed".into()),
                        only: vec![],
                        expected_files: 0,
                        vanished: 0,
                        skipped: vec![],
                    });
                } else {
                    programs.push(Program { what: "DCS-SimpleRadio Standalone".into(), path: p.into() });
                }
            }
        }
    }
    // Saved Games\DCS* of the bot's user that nodes.yaml doesn't name
    let profile = cfg
        .desktop_user
        .as_deref()
        .map(crate::desktop::account_name)
        .map(|u| PathBuf::from(format!("{}\\Users\\{u}", system_drive())))
        .filter(|p| p.is_dir())
        .or_else(|| homes.first().and_then(|(_, h)| profile_of(h)).map(PathBuf::from))
        .or_else(|| std::env::var_os("USERPROFILE").map(PathBuf::from));
    if let Some(Ok(rd)) = profile.as_ref().map(|p| std::fs::read_dir(p.join("Saved Games"))) {
        for e in rd.flatten() {
            let n = e.file_name().to_string_lossy().to_string();
            let p = disp(&e.path());
            if n.to_lowercase().starts_with("dcs") && e.path().is_dir() && !homes.iter().any(|(_, h)| is_under(h, &p) && is_under(&p, h)) {
                homes.push((n, p));
            }
        }
    }
    for (name, home) in homes {
        let p = PathBuf::from(&home);
        if !p.is_dir() {
            warnings.push(format!("DCS instance {name}: {home} doesn't exist -- skipped"));
            continue;
        }
        if roots.iter().any(|r| is_under(&home, &r.path) && is_under(&r.path, &home)) {
            continue;
        }
        roots.push(Root {
            id: root_id(Kind::Instance, &p),
            kind: Kind::Instance,
            label: format!("DCS server {name} (config, missions, campaign saves, bfdb data)"),
            path: home,
            files: 0,
            bytes: 0,
            include: true,
            note: Some("without tracks, screenshots, shader caches and DCS logs".into()),
            only: vec![],
            expected_files: 0,
            vanished: 0,
            skipped: vec![],
        });
    }

    // netidx: the tool procman runs the resolver with, and the client config
    // bflib (in DCS) and bfdb find the resolver through
    if let Some(dir) = find_netidx(profile.as_deref()) {
        roots.push(Root {
            id: root_id(Kind::Netidx, &dir),
            kind: Kind::Netidx,
            label: "netidx tools (netidx.exe -- runs the resolver the live map and stats use)".into(),
            path: disp(&dir),
            files: 0,
            bytes: 0,
            include: true,
            note: Some("only netidx.exe from this folder; the restore puts the folder on PATH".into()),
            only: vec!["netidx.exe".into()],
            expected_files: 0,
            vanished: 0,
            skipped: vec![],
        });
    } else {
        warnings.push("netidx.exe was not found on PATH or in .cargo\\bin -- the live map and stats need it (cargo install netidx-tools)".into());
    }
    for dir in netidx_config_dirs(profile.as_deref()) {
        let d = disp(&dir);
        if roots.iter().any(|r| r.only.is_empty() && is_under(&d, &r.path)) {
            continue;
        }
        roots.push(Root {
            id: root_id(Kind::Netidx, &dir),
            kind: Kind::Netidx,
            label: "netidx client config (where bflib and bfdb find the resolver)".into(),
            path: d,
            files: 0,
            bytes: 0,
            include: true,
            note: None,
            only: vec![],
            expected_files: 0,
            vanished: 0,
            skipped: vec![],
        });
    }

    // bfdb's home, when it is not inside an instance
    let fe = yaml_at(&cfg_dir, "plugins/fowlengine.yaml", &mut cache).cloned();
    let mut referenced: Vec<String> = Vec::new();
    if let Some(fe) = &fe {
        abs_paths(fe, &mut referenced);
        // bfdb.home in whichever block holds it
        let mut stack = vec![fe.clone()];
        while let Some(v) = stack.pop() {
            if let Some(m) = v.as_mapping() {
                if let Some(b) = m.get("bfdb") {
                    if let Some(h) = str_at(b, &["home"]) {
                        let covered = roots.iter().any(|r| is_under(h, &r.path));
                        if !covered && Path::new(h).is_dir() {
                            roots.push(Root {
                                id: root_id(Kind::Bfdb, Path::new(h)),
                                kind: Kind::Bfdb,
                                label: "bfdb home (stats database, campaign.json, intel)".into(),
                                path: h.into(),
                                files: 0,
                                bytes: 0,
                                include: true,
                                note: None,
                                only: vec![],
                                expected_files: 0,
                                vanished: 0,
                                skipped: vec![],
                            });
                        }
                    }
                }
                stack.extend(m.values().cloned());
            }
        }
    }
    if let Some(n) = yaml_at(&cfg_dir, "nodes.yaml", &mut cache) {
        abs_paths(n, &mut referenced);
    }
    // the bot's own backup target holds old copies: not worth copying again
    let backup_target = yaml_at(&cfg_dir, "services/backup.yaml", &mut cache)
        .and_then(|b| str_at(b, &["target"]).map(String::from));

    // other folders the config points at
    let mut extras: Vec<String> = Vec::new();
    for p in referenced {
        if roots.iter().any(|r| r.only.is_empty() && is_under(&p, &r.path)) {
            continue;
        }
        let path = PathBuf::from(&p);
        let folder = if path.is_dir() {
            path
        } else if path.is_file() {
            match path.parent() {
                Some(d) => d.to_path_buf(),
                None => continue,
            }
        } else {
            continue;
        };
        let f = disp(&folder);
        if program_location(&f) {
            // installed software: noted, never copied
            if !programs.iter().any(|x| is_under(&p, &x.path)) {
                programs.push(Program { what: "referenced by the bot's config".into(), path: p.clone() });
            }
            continue;
        }
        if folder.parent().is_none() || f.len() <= 3 {
            continue;
        }
        if roots.iter().any(|r| is_under(&f, &r.path))
            || backup_target.as_deref().map(|t| is_under(&f, t)).unwrap_or(false)
            || profile.as_ref().map(|pr| lower(&f) == lower(&disp(pr))).unwrap_or(false)
        {
            continue;
        }
        extras.push(f);
    }
    // keep only the outermost of nested folders
    extras.sort_by_key(|e| e.len());
    let mut kept: Vec<String> = Vec::new();
    for e in extras {
        if !kept.iter().any(|k| is_under(&e, k)) {
            kept.push(e);
        }
    }
    for e in kept {
        roots.push(Root {
            id: root_id(Kind::Extra, Path::new(&e)),
            kind: Kind::Extra,
            label: "Folder the bot's config points at".into(),
            path: e,
            files: 0,
            bytes: 0,
            include: true,
            note: None,
            only: vec![],
            expected_files: 0,
            vanished: 0,
            skipped: vec![],
        });
    }

    for r in roots.iter_mut() {
        for l in measure(r) {
            warnings.push(format!("{l} is a link -- not followed (copy what it points at by hand if it matters)"));
        }
        if r.kind == Kind::Extra && r.bytes > EXTRA_DEFAULT_MAX {
            r.include = false;
            r.note = Some(format!("{} -- unticked because it is big; tick it if it can't be downloaded again", fmt_bytes(r.bytes)));
        }
    }

    let database = bot_db(&bot_dir).map(|c| {
        let bins = postgres_bins();
        let pg_dump = bins.first().map(|(_, b)| exe(b, "pg_dump")).filter(|p| p.is_file());
        let problem = if !is_local_host(&c.host) {
            Some(format!("the database is on another machine ({}) -- back it up there", c.host))
        } else if pg_dump.is_none() {
            Some("PostgreSQL's pg_dump was not found on this PC".into())
        } else if c.password.is_none() {
            Some("the database password was not found (config\\.secret\\database.pkl)".into())
        } else {
            None
        };
        DbPlan { host: c.host, port: c.port, name: c.name, user: c.user, pg_dump: pg_dump.map(|p| disp(&p)), problem }
    });
    if database.is_none() {
        warnings.push("no database URL found in the bot's nodes.yaml / main.yaml -- the bot's database is not backed up".into());
    }

    let drives = data_drives();
    let default_dest = format!("{}\\FowlEngineBackups", drives[0]);
    let service_running = agent_status().is_some();

    Ok(BackupPlan {
        roots,
        database,
        programs,
        default_dest,
        hostname: host,
        bot_running: bot_running(),
        service_running,
        bfdb_running: !running(&["bfdb.exe"]).is_empty(),
        dcs_running: !running(&DCS_EXES).is_empty(),
        warnings,
    })
}

#[derive(Debug, Clone, Deserialize)]
pub struct BackupOptions {
    pub dest_dir: String,
    /// Root ids from the plan to include.
    pub roots: Vec<String>,
    /// More folders to include, typed by the admin.
    #[serde(default)]
    pub extra_paths: Vec<String>,
    #[serde(default)]
    pub database: bool,
    /// Stop DCSServerBot (and so bfdb) while copying -- a consistent copy.
    #[serde(default)]
    pub stop_bot: bool,
    /// Read every file from a Windows shadow copy (VSS): files in use --
    /// bfdb's database, bflib's live stats -- come out whole and all from
    /// one instant, with nothing stopped.
    #[serde(default = "yes")]
    pub shadow_copy: bool,
}

fn yes() -> bool {
    true
}

// ---- shadow copies ---------------------------------------------------------------------

/// A Windows shadow copy (VSS snapshot) of one volume: a frozen view of
/// every file on it as of one instant, the way Windows Backup reads files in
/// use. Crash-consistent -- what a power cut would leave -- which bfdb's
/// database (sled) and bflib's netidx archive are built to recover from;
/// unlike a live copy, a locked range doesn't come out missing. Deleted when
/// dropped.
pub struct Shadow {
    id: String,
    /// `\\?\GLOBALROOT\Device\HarddiskVolumeShadowCopyN`
    device: String,
    /// "C:"
    volume: String,
}

fn powershell(script: &str) -> Result<String> {
    use base64::Engine;
    // errors come back as plain text on stdout: with stderr redirected,
    // powershell writes them as CLIXML
    let script = format!(
        "try {{\n{script}\n}} catch {{ Write-Output ('FOWL-ERROR: ' + $_.Exception.Message); exit 1 }}"
    );
    // -EncodedCommand (UTF-16LE base64): no quoting through cmd lines at all
    let utf16: Vec<u8> = script.encode_utf16().flat_map(|u| u.to_le_bytes()).collect();
    let enc = base64::engine::general_purpose::STANDARD.encode(utf16);
    let out = hidden("powershell.exe")
        .args(["-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass", "-EncodedCommand", &enc])
        .output()
        .context("running powershell.exe")?;
    let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if let Some(e) = stdout.lines().find_map(|l| l.trim().strip_prefix("FOWL-ERROR: ")) {
        bail!("{}", e.trim());
    }
    if !out.status.success() {
        bail!("powershell exited with {:?}", out.status.code());
    }
    Ok(stdout)
}

impl Shadow {
    /// Needs administrator rights (this app has them) and the Volume Shadow
    /// Copy service (on demand on every Windows).
    pub fn create(volume: &str) -> Result<Shadow> {
        let v = volume.trim_end_matches('\\').to_uppercase();
        if v.len() != 2 || !v.ends_with(':') {
            bail!("not a drive: {volume}");
        }
        let script = format!(
            "$ErrorActionPreference = 'Stop'\n\
             $r = (Get-WmiObject -List Win32_ShadowCopy).Create('{v}\\', 'ClientAccessible')\n\
             if ($r.ReturnValue -ne 0) {{ throw ('Win32_ShadowCopy.Create returned ' + $r.ReturnValue) }}\n\
             $s = Get-WmiObject Win32_ShadowCopy | Where-Object {{ $_.ID -eq $r.ShadowID }}\n\
             Write-Output ($s.ID + '|' + $s.DeviceObject)"
        );
        let out = powershell(&script).map_err(|e| {
            let m = format!("{e:#}");
            if m.contains("Initialization failure") || m.to_lowercase().contains("access") {
                anyhow!("{m} -- shadow copies need Fowl Engine Manager running as administrator")
            } else {
                e
            }
        })?;
        let line = out.lines().last().unwrap_or("").trim();
        let (id, device) = line.split_once('|').ok_or_else(|| anyhow!("unexpected answer from VSS: {line:?}"))?;
        if !device.to_lowercase().contains("shadowcopy") {
            bail!("unexpected shadow copy device {device:?}");
        }
        Ok(Shadow { id: id.trim().into(), device: device.trim().trim_end_matches('\\').into(), volume: v })
    }

    /// The same file inside the snapshot, if it is on this volume.
    pub fn map(&self, abs: &Path) -> Option<PathBuf> {
        let s = disp(abs);
        (s.len() > 2 && s[..2].eq_ignore_ascii_case(&self.volume)).then(|| PathBuf::from(format!("{}{}", self.device, &s[2..])))
    }
}

impl Drop for Shadow {
    fn drop(&mut self) {
        let id = self.id.replace('\'', "");
        let script = format!("Get-WmiObject Win32_ShadowCopy | Where-Object {{ $_.ID -eq '{id}' }} | ForEach-Object {{ $_.Delete() }}");
        match powershell(&script) {
            Ok(_) => note(format!("deleted the shadow copy of {}", self.volume)),
            Err(e) => warn(format!("could not delete the shadow copy {} of {} ({e:#}) -- `vssadmin delete shadows /shadow={}` removes it", self.id, self.volume, self.id)),
        }
    }
}

/// Open a file from the shadow copy of its volume, else the live one.
fn open_snapshot(shadows: &[Shadow], abs: &Path) -> std::io::Result<File> {
    if let Some(p) = shadows.iter().find_map(|s| s.map(abs)) {
        match File::open(&p) {
            Ok(f) => return Ok(f),
            // made after the snapshot was taken: read it live
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => return Err(e),
        }
    }
    File::open(abs)
}

pub fn start_backup(opts: BackupOptions) -> Result<()> {
    let dest = PathBuf::from(opts.dest_dir.trim());
    if !dest.is_absolute() {
        bail!("pick a full folder path for the backup, like D:\\FowlEngineBackups");
    }
    spawn("backup", move || run_backup(opts, dest))
}

/// Restarts the bot when the backup is done, however it ends.
struct RestartBot(bool);

impl Drop for RestartBot {
    fn drop(&mut self) {
        if self.0 {
            let _ = std::fs::write(config::commands_dir().join("start-bot"), "backup");
            note("asked the service to start DCSServerBot again".into());
        }
    }
}

struct Counting<R> {
    inner: R,
}

impl<R: Read> Read for Counting<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if CANCEL.load(Ordering::Relaxed) {
            return Err(std::io::Error::other("cancelled"));
        }
        let n = self.inner.read(buf)?;
        DONE.fetch_add(n as u64, Ordering::Relaxed);
        Ok(n)
    }
}

fn stored(name: &str) -> bool {
    let n = name.to_ascii_lowercase();
    [".miz", ".zip", ".7z", ".png", ".jpg", ".jpeg", ".ogg", ".mp3", ".trk", ".dump", ".gz", ".webp"]
        .iter()
        .any(|e| n.ends_with(e))
}

fn file_opts(name: &str, size: u64) -> zip::write::SimpleFileOptions {
    let o = zip::write::SimpleFileOptions::default().large_file(size >= 0xF000_0000);
    if stored(name) {
        o.compression_method(zip::CompressionMethod::Stored)
    } else {
        o.compression_method(zip::CompressionMethod::Deflated).compression_level(Some(3))
    }
}

fn run_backup(opts: BackupOptions, dest: PathBuf) -> Result<String> {
    phase("Looking at what to back up");
    let plan = backup_plan()?;
    let mut roots: Vec<Root> = plan.roots.iter().filter(|r| opts.roots.contains(&r.id)).cloned().collect();
    for e in &opts.extra_paths {
        let p = PathBuf::from(e.trim());
        if e.trim().is_empty() {
            continue;
        }
        if !p.is_dir() {
            bail!("{} is not a folder", e.trim());
        }
        let ps = disp(&p);
        if roots.iter().any(|r| is_under(&ps, &r.path)) {
            continue;
        }
        let mut r = Root {
            id: root_id(Kind::Extra, &p),
            kind: Kind::Extra,
            label: "Folder added by hand".into(),
            path: ps,
            files: 0,
            bytes: 0,
            include: true,
            note: None,
            only: vec![],
            expected_files: 0,
            vanished: 0,
            skipped: vec![],
        };
        measure(&mut r);
        roots.push(r);
    }
    if roots.is_empty() {
        bail!("nothing ticked to back up");
    }
    if let Some(r) = roots.iter().find(|r| is_under(&disp(&dest), &r.path)) {
        bail!("the backup can't be saved inside a folder it copies ({}) -- pick another folder", r.path);
    }
    std::fs::create_dir_all(&dest).with_context(|| format!("creating {}", dest.display()))?;

    let total: u64 = roots.iter().map(|r| r.bytes).sum();
    with_job(|j| j.total_bytes = total);
    if let Some(free) = free_space(&dest) {
        if free < total / 2 {
            bail!(
                "only {} free on the backup drive; the files to copy are {} -- pick another drive",
                fmt_bytes(free),
                fmt_bytes(total)
            );
        }
    }
    if is_under(&disp(&dest), &system_drive()) {
        warn(format!(
            "the backup is going to {} -- the Windows drive. Copy the zip OFF this PC (USB stick, another drive, cloud) before reinstalling Windows.",
            disp(&dest)
        ));
    }
    for w in &plan.warnings {
        warn(w.clone());
    }

    // a consistent copy: bot down -> procman stops bfdb cleanly
    let mut restart = RestartBot(false);
    if opts.stop_bot && plan.bot_running {
        phase("Stopping DCSServerBot (bfdb stops with it)");
        config::ensure_dirs()?;
        std::fs::write(config::commands_dir().join("stop-bot"), "backup")?;
        restart.0 = true;
        let t0 = Instant::now();
        while t0.elapsed() < Duration::from_secs(120) {
            cancelled()?;
            if !bot_running() && running(&["bfdb.exe"]).is_empty() {
                break;
            }
            std::thread::sleep(Duration::from_secs(2));
        }
        if bot_running() {
            warn("DCSServerBot did not stop within 2 minutes -- copying anyway".into());
        }
    } else if opts.stop_bot && !plan.service_running && plan.bot_running {
        warn("the FowlEngine service is not running, so this app can't stop the bot -- copying while it runs".into());
    }
    // one snapshot per drive the copied folders are on
    let mut shadows: Vec<Shadow> = Vec::new();
    if opts.shadow_copy {
        let mut vols: Vec<String> = roots.iter().filter_map(|r| r.path.get(..2).map(|v| v.to_uppercase())).collect();
        vols.sort();
        vols.dedup();
        for v in vols {
            cancelled()?;
            phase(&format!("Taking a shadow copy of {v} (files in use are copied whole, nothing has to stop)"));
            match Shadow::create(&v) {
                Ok(s) => {
                    note(format!("  {} ({})", s.device, s.id));
                    shadows.push(s);
                }
                Err(e) => warn(format!("no shadow copy of {v} ({e:#}) -- files in use there are copied live and may come out partial")),
            }
        }
    }
    let shadowed = |p: &str| shadows.iter().any(|s| s.map(Path::new(p)).is_some());
    let live_roots: Vec<&Root> = roots.iter().filter(|r| !shadowed(&r.path)).collect();
    if !live_roots.is_empty() && !running(&["bfdb.exe"]).is_empty() {
        warn("bfdb.exe is running and its folder is copied live: its database may come out partial. Stop the bot first, or allow the shadow copy.".into());
    }
    if !running(&DCS_EXES).is_empty() {
        note(if shadows.is_empty() {
            "DCS is running: the campaign save in the backup is the last one it wrote, and its live stats files may come out partial.".into()
        } else {
            "DCS is running: the backup holds everything as it was the moment the shadow copy was taken.".into()
        });
    }

    let host = plan.hostname.clone();
    let stamp = chrono::Local::now().format("%Y%m%d-%H%M");
    let final_path = dest.join(format!("{ZIP_PREFIX}{}-{stamp}.zip", slug(&host)));
    let partial = final_path.with_extension("zip.partial");
    let result = (|| -> Result<Manifest> {
        let f = File::create(&partial).with_context(|| format!("creating {}", partial.display()))?;
        let mut z = zip::ZipWriter::new(std::io::BufWriter::with_capacity(1 << 20, f));
        let cfg = ManagerConfig::load();

        // this app's settings
        if let Ok(b) = std::fs::read(config::config_path()) {
            z.start_file("manager/manager.json", file_opts("manager.json", b.len() as u64))?;
            z.write_all(&b)?;
        }

        let mut done_roots = Vec::new();
        for mut r in roots {
            cancelled()?;
            phase(&format!("Copying {} ({})", r.label, r.path));
            let mut files = Vec::new();
            let mut links = Vec::new();
            walk_root(&r, &mut files, &mut links);
            r.expected_files = files.len() as u64;
            r.skipped.clear();
            r.vanished = 0;
            let (mut n, mut bytes) = (0u64, 0u64);
            for fe in files {
                cancelled()?;
                with_job(|j| j.current = Some(disp(&fe.abs)));
                let name = format!("roots/{}/{}", r.id, fe.rel);
                let file = match open_snapshot(&shadows, &fe.abs) {
                    Ok(f) => f,
                    // a save's temp file, renamed away since the listing
                    Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                        note(format!("  gone before it was copied (a temporary file): {}", fe.rel));
                        r.vanished += 1;
                        r.expected_files = r.expected_files.saturating_sub(1);
                        DONE.fetch_add(fe.size, Ordering::Relaxed);
                        continue;
                    }
                    Err(e) => {
                        warn(format!("could not read {}: {e}", disp(&fe.abs)));
                        if r.skipped.len() < 200 {
                            r.skipped.push(format!("{} (not readable: {e})", fe.rel));
                        }
                        DONE.fetch_add(fe.size, Ordering::Relaxed);
                        continue;
                    }
                };
                z.start_file(name.as_str(), file_opts(&name, fe.size))?;
                let copied = std::io::copy(&mut Counting { inner: file }, &mut z);
                match copied {
                    Ok(c) => {
                        n += 1;
                        bytes += c;
                    }
                    Err(e) if e.to_string() == "cancelled" => bail!("cancelled"),
                    // a file locked part-way: the entry holds what was read
                    Err(e) => {
                        warn(format!("{} was only partly read ({e})", disp(&fe.abs)));
                        n += 1;
                        if r.skipped.len() < 200 {
                            r.skipped.push(format!("{} (only partly read: {e})", fe.rel));
                        }
                    }
                }
            }
            r.files = n;
            r.bytes = bytes;
            note(format!("  {} files, {}", n, fmt_bytes(bytes)));
            done_roots.push(r);
        }

        // the bot's database
        let mut database = None;
        let mut db_warnings = Vec::new();
        if opts.database {
            match &plan.database {
                Some(d) if d.problem.is_none() => {
                    phase(&format!("Dumping the bot's database {} (pg_dump)", d.name));
                    match dump_database(&cfg, d, &dest) {
                        Ok((tmp, ver)) => {
                            let entry = format!("database/{}.dump", slug(&d.name));
                            let size = std::fs::metadata(&tmp).map(|m| m.len()).unwrap_or(0);
                            with_job(|j| j.total_bytes += size);
                            z.start_file(entry.as_str(), file_opts(&entry, size))?;
                            std::io::copy(&mut Counting { inner: File::open(&tmp)? }, &mut z)?;
                            let _ = std::fs::remove_file(&tmp);
                            note(format!("  database dump {}", fmt_bytes(size)));
                            database = Some(DbInfo {
                                host: d.host.clone(),
                                port: d.port,
                                name: d.name.clone(),
                                user: d.user.clone(),
                                dump: Some(entry),
                                bytes: size,
                                pg_version: ver,
                            });
                        }
                        Err(e) => db_warnings.push(format!("the bot's database was NOT backed up: {e:#}")),
                    }
                }
                Some(d) => db_warnings.push(format!(
                    "the bot's database was NOT backed up: {}",
                    d.problem.clone().unwrap_or_default()
                )),
                None => {}
            }
        }
        if database.is_none() {
            if let Some(d) = &plan.database {
                database = Some(DbInfo {
                    host: d.host.clone(),
                    port: d.port,
                    name: d.name.clone(),
                    user: d.user.clone(),
                    ..Default::default()
                });
            }
        }
        for w in db_warnings {
            warn(w);
        }

        let profile = cfg
            .desktop_user
            .as_deref()
            .map(crate::desktop::account_name)
            .map(|u| format!("{}\\Users\\{u}", system_drive()))
            .or_else(|| done_roots.iter().find_map(|r| profile_of(&r.path)))
            .or_else(|| std::env::var("USERPROFILE").ok());
        let m = Manifest {
            format: FORMAT,
            created: now(),
            hostname: host.clone(),
            manager_version: crate::update::current_version().to_string(),
            desktop_user: cfg.desktop_user.clone(),
            profile,
            bot_dir: cfg.bot_dir().map(|d| disp(&d)),
            roots: done_roots,
            database,
            programs: plan.programs.clone(),
            python: find_python().map(|(_, v)| v),
            service_installed: plan.service_running,
            warnings: job_status().map(|j| j.warnings).unwrap_or_default(),
        };
        let mj = serde_json::to_vec_pretty(&m)?;
        z.start_file(MANIFEST, file_opts(MANIFEST, mj.len() as u64))?;
        z.write_all(&mj)?;
        let mut w = z.finish()?;
        w.flush()?;
        w.into_inner().map_err(|e| anyhow!("{}", e.error()))?.sync_all()?;
        Ok(m)
    })();
    // every file is read: let the snapshots go
    drop(shadows);
    let m = match result {
        Ok(m) => m,
        Err(e) => {
            let _ = std::fs::remove_file(&partial);
            return Err(e);
        }
    };

    std::fs::rename(&partial, &final_path)?;
    with_job(|j| j.zip_path = Some(disp(&final_path)));
    // the bot can come back while the zip is read through
    drop(restart);
    let report = verify_zip(&final_path).context("checking the finished zip")?;
    let report_ok = report.ok;
    if !report_ok {
        for p in report.problems.iter().chain(report.corrupt.iter().take(20)) {
            warn(format!("check: {p}"));
        }
    }
    with_job(|j| j.report = Some(report));

    let size = std::fs::metadata(&final_path).map(|m| m.len()).unwrap_or(0);
    if !report_ok {
        next_step("The check found problems (see the report above) -- fix them and back up again before wiping anything.".into());
    }
    next_step(format!(
        "Copy {} OFF this PC (USB stick, another drive, cloud) before reinstalling Windows.",
        disp(&final_path)
    ));
    next_step("Keep it private: it holds the bot's Discord token, database password and API keys.".into());
    if m.database.as_ref().and_then(|d| d.dump.as_ref()).is_none() && m.database.is_some() {
        next_step("The bot's database is not in this backup (see the warnings) -- back it up by hand if you need its stats.".into());
    }
    next_step("On the new Windows: install DCS World Server (and SRS), install Fowl Engine Manager, open BACKUP → Restore and pick this zip.".into());
    Ok(format!("{} ({})", disp(&final_path), fmt_bytes(size)))
}

fn dump_database(_cfg: &ManagerConfig, d: &DbPlan, dest: &Path) -> Result<(PathBuf, Option<String>)> {
    let bot = ManagerConfig::load().bot_dir().ok_or_else(|| anyhow!("no bot folder"))?;
    let conn = bot_db(&bot).ok_or_else(|| anyhow!("database settings not found"))?;
    let pg_dump = PathBuf::from(d.pg_dump.clone().ok_or_else(|| anyhow!("pg_dump not found"))?);
    let tmp = dest.join(format!(".{}-db.partial", slug(&d.name)));
    let out = hidden(&pg_dump.display().to_string())
        .args(["-Fc", "--no-owner", "--no-privileges", "-h", &conn.host, "-p", &conn.port.to_string(), "-U", &conn.user, "-d", &conn.name, "-f"])
        .arg(&tmp)
        .env("PGPASSWORD", conn.password.unwrap_or_default())
        .output()
        .with_context(|| format!("running {}", pg_dump.display()))?;
    if !out.status.success() {
        let _ = std::fs::remove_file(&tmp);
        bail!("pg_dump failed: {}", String::from_utf8_lossy(&out.stderr).trim());
    }
    Ok((tmp, tool_version(&pg_dump)))
}

// ---- checking a zip -------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Default)]
pub struct CheckLine {
    pub ok: bool,
    /// A missing extra is a note, not a fault (a test server has no bfdb).
    #[serde(default)]
    pub info: bool,
    pub text: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct Section {
    pub label: String,
    pub kind: String,
    pub path: String,
    /// ok | warn | bad
    pub status: String,
    /// On disk when backed up (0 = not recorded: an older backup).
    pub files_on_disk: u64,
    pub files_in_zip: u64,
    pub bytes_in_zip: u64,
    pub checks: Vec<CheckLine>,
    /// Files that didn't make it in whole.
    pub skipped: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct VerifyReport {
    pub zip: String,
    /// Every entry read back clean and nothing expected is missing.
    pub ok: bool,
    pub created: String,
    pub hostname: String,
    pub manager_version: String,
    pub entries: u64,
    pub bytes: u64,
    pub zip_bytes: u64,
    pub sections: Vec<Section>,
    /// Entries whose data doesn't read back (checksum, truncation).
    pub corrupt: Vec<String>,
    pub problems: Vec<String>,
}

impl VerifyReport {
    pub fn to_text(&self) -> String {
        let mut t = format!(
            "BACKUP CHECK: {}\n  {}\n  made {} on {} by manager {}; {} entries, {} unpacked, {} zip\n",
            if self.ok { "OK -- everything listed is in the zip and reads back clean" } else { "PROBLEMS FOUND" },
            self.zip,
            self.created,
            self.hostname,
            self.manager_version,
            self.entries,
            fmt_bytes(self.bytes),
            fmt_bytes(self.zip_bytes)
        );
        for p in &self.problems {
            t.push_str(&format!("  ! {p}\n"));
        }
        for c in self.corrupt.iter().take(50) {
            t.push_str(&format!("  ! corrupt: {c}\n"));
        }
        for s in &self.sections {
            t.push_str(&format!(
                "\n  [{}] {}\n      {}\n      {} files in zip{}, {}\n",
                s.status.to_uppercase(),
                s.label,
                s.path,
                s.files_in_zip,
                if s.files_on_disk > 0 { format!(" of {} on disk", s.files_on_disk) } else { String::new() },
                fmt_bytes(s.bytes_in_zip)
            ));
            for c in &s.checks {
                t.push_str(&format!("      {} {}\n", if c.ok { "ok " } else if c.info { "-- " } else { "!! " }, c.text));
            }
            for k in &s.skipped {
                t.push_str(&format!("      skipped: {k}\n"));
            }
        }
        t
    }
}

pub fn start_verify(zip: String) -> Result<()> {
    let p = PathBuf::from(zip.trim().trim_matches('"'));
    if !p.is_file() {
        bail!("{} not found", disp(&p));
    }
    spawn("verify", move || {
        with_job(|j| j.zip_path = Some(disp(&p)));
        let r = verify_zip(&p)?;
        let ok = r.ok;
        let n = r.problems.len() + r.corrupt.len();
        with_job(|j| j.report = Some(r));
        Ok(if ok { "the backup is complete and reads back clean".into() } else { format!("{n} problem(s) found -- see the report") })
    })
}

/// Read every entry of a backup back (zip checks each one's CRC as it
/// reads), then compare with what its manifest says went in, and look for
/// the files a restore can't do without.
pub fn verify_zip(zip_path: &Path) -> Result<VerifyReport> {
    phase("Checking the zip: reading every file back");
    let (m, zip_bytes) = read_manifest(zip_path)?;
    let mut a = zip::ZipArchive::new(File::open(zip_path)?)?;
    let total: u64 = (0..a.len()).filter_map(|i| a.by_index_raw(i).ok().map(|e| e.size())).sum();
    DONE.store(0, Ordering::Relaxed);
    with_job(|j| j.total_bytes = total);

    // per entry: read through (CRC), tally by root
    let mut names: Vec<(String, u64)> = Vec::with_capacity(a.len());
    let mut corrupt = Vec::new();
    let mut dump_head: Vec<u8> = Vec::new();
    let dump_entry = m.database.as_ref().and_then(|d| d.dump.clone());
    for i in 0..a.len() {
        cancelled()?;
        let mut e = a.by_index(i)?;
        if e.is_dir() {
            continue;
        }
        let name = e.name().to_string();
        let size = e.size();
        with_job(|j| j.current = Some(name.clone()));
        if dump_entry.as_deref() == Some(name.as_str()) {
            let mut head = [0u8; 5];
            let n = e.read(&mut head).unwrap_or(0);
            dump_head = head[..n].to_vec();
            DONE.fetch_add(n as u64, Ordering::Relaxed);
        }
        let res = std::io::copy(&mut Counting { inner: &mut e }, &mut std::io::sink());
        match res {
            Ok(_) => names.push((name, size)),
            Err(err) if err.to_string() == "cancelled" => bail!("cancelled"),
            Err(err) => corrupt.push(format!("{name} ({err})")),
        }
    }
    let has = |prefix: &str, pred: &dyn Fn(&str) -> bool| {
        names.iter().any(|(n, _)| n.strip_prefix(prefix).map(pred).unwrap_or(false))
    };

    let mut sections = Vec::new();
    let mut problems = Vec::new();
    for r in &m.roots {
        let prefix = format!("roots/{}/", r.id);
        let (count, bytes) = names
            .iter()
            .filter(|(n, _)| n.starts_with(&prefix))
            .fold((0u64, 0u64), |(c, b), (_, s)| (c + 1, b + s));
        let bad_here = corrupt.iter().filter(|c| c.starts_with(&prefix)).count();
        let mut checks = Vec::new();
        let mut later: Option<CheckLine> = None;
        let mut need = |ok: bool, text: &str| checks.push(CheckLine { ok, info: false, text: text.into() });
        let ci = |want: &str| {
            let w = want.to_lowercase();
            move |rel: &str| rel.to_lowercase() == w
        };
        let starts = |want: &str| {
            let w = want.to_lowercase();
            move |rel: &str| rel.to_lowercase().starts_with(&w)
        };
        match r.kind {
            Kind::Bot => {
                need(has(&prefix, &ci("run.py")), "run.py (DCSServerBot itself)");
                need(has(&prefix, &ci("config/main.yaml")), "config/main.yaml");
                need(has(&prefix, &ci("config/nodes.yaml")), "config/nodes.yaml (DCS install, servers, database URL)");
                need(has(&prefix, &starts("config/.secret/")), "config/.secret (Discord token, passwords)");
                need(has(&prefix, &ci("config/plugins/fowlengine.yaml")), "config/plugins/fowlengine.yaml (bfdb, GCI, keys)");
                need(has(&prefix, &starts("plugins/fowlengine/")), "the Fowl Engine plugin");
            }
            Kind::Instance => {
                need(has(&prefix, &ci("config/serversettings.lua")), "Config/serverSettings.lua (name, password, mission list)");
                let miz = names
                    .iter()
                    .filter(|(n, _)| n.starts_with(&prefix) && n.to_lowercase().ends_with(".miz"))
                    .count();
                need(miz > 0, &format!("{miz} mission file(s) (.miz)"));
                let bfdb = has(&prefix, &starts("bfdb/"));
                let dll = has(&prefix, &ci("scripts/bflib.dll"));
                if dll {
                    need(true, "Scripts/bflib.dll (the engine)");
                }
                later = Some(CheckLine {
                    ok: bfdb,
                    info: !bfdb,
                    text: if bfdb { "bfdb/ (stats database)".into() } else { "no bfdb/ here (normal unless bfdb runs from this folder)".into() },
                });
                if has(&prefix, &starts("logs/stats")) {
                    need(true, "Logs/stats (what bfdb reads)");
                }
            }
            Kind::Netidx if !r.only.is_empty() => {
                need(has(&prefix, &ci("netidx.exe")), "netidx.exe");
            }
            Kind::Netidx => {
                need(has(&prefix, &ci("client.json")), "client.json");
            }
            Kind::Program => {
                need(has(&prefix, &|rel: &str| rel.to_lowercase().ends_with(".exe")), "the program's .exe files");
            }
            Kind::Bfdb | Kind::Extra => {}
        }
        checks.extend(later);
        // an older zip has no expected_files: only compare when recorded
        let missing = r.files.saturating_sub(count);
        if missing > 0 {
            checks.push(CheckLine { ok: false, info: false, text: format!("{missing} file(s) the manifest lists are not in the zip") });
        } else {
            checks.push(CheckLine { ok: true, info: false, text: format!("all {} file(s) written are in the zip", r.files) });
        }
        if bad_here > 0 {
            checks.push(CheckLine { ok: false, info: false, text: format!("{bad_here} file(s) don't read back (corrupt)") });
        }
        // in the zip but cut short (a locked range): a restore would get a
        // broken file -- worse than a missing one
        let partial = r.skipped.iter().filter(|k| k.contains("only partly read")).count();
        if partial > 0 {
            checks.push(CheckLine {
                ok: false,
                info: false,
                text: format!("{partial} file(s) only partly copied (in use while copying) -- broken in this backup"),
            });
        }
        if r.vanished > 0 {
            checks.push(CheckLine {
                ok: false,
                info: true,
                text: format!("{} temporary file(s) disappeared while copying (a save being written) -- harmless", r.vanished),
            });
        }
        if r.expected_files > 0 && r.files < r.expected_files {
            checks.push(CheckLine {
                ok: false,
                info: false,
                text: format!("{} of {} file(s) on disk couldn't be read at backup time", r.expected_files - r.files, r.expected_files),
            });
        }
        // bad: something a restore needs is missing or corrupt; warn: some
        // files couldn't be read when the backup was made (locked, gone)
        let unread = !r.skipped.is_empty() || (r.expected_files > 0 && r.files < r.expected_files);
        let key_missing = checks.iter().any(|c| !c.ok && !c.info && !c.text.contains("couldn't be read"));
        let status = if bad_here > 0 || missing > 0 || key_missing {
            "bad"
        } else if unread {
            "warn"
        } else {
            "ok"
        };
        if status == "bad" {
            problems.push(format!("{}: {}", r.label, checks.iter().filter(|c| !c.ok && !c.info).map(|c| c.text.as_str()).collect::<Vec<_>>().join("; ")));
        }
        sections.push(Section {
            label: r.label.clone(),
            kind: format!("{:?}", r.kind).to_lowercase(),
            path: r.path.clone(),
            status: status.into(),
            files_on_disk: r.expected_files,
            files_in_zip: count,
            bytes_in_zip: bytes,
            checks,
            skipped: r.skipped.clone(),
        });
    }

    // the database dump
    if let Some(d) = &m.database {
        let mut checks = Vec::new();
        let (status, count, bytes) = match &d.dump {
            Some(entry) => match names.iter().find(|(n, _)| n == entry) {
                Some((_, size)) => {
                    let magic = dump_head.starts_with(b"PGDMP");
                    checks.push(CheckLine { ok: true, info: false, text: format!("{entry} is in the zip") });
                    checks.push(CheckLine { ok: magic, info: false, text: "a PostgreSQL custom-format dump (PGDMP header)".into() });
                    checks.push(CheckLine {
                        ok: *size == d.bytes,
                        info: false,
                        text: format!("{} -- {} when it was taken", fmt_bytes(*size), fmt_bytes(d.bytes)),
                    });
                    (if magic && *size == d.bytes { "ok" } else { "bad" }, 1, *size)
                }
                None => {
                    checks.push(CheckLine { ok: false, info: false, text: format!("{entry} is listed but not readable in the zip") });
                    ("bad", 0, 0)
                }
            },
            None => {
                checks.push(CheckLine { ok: false, info: false, text: "the database was not dumped when this backup was made".into() });
                ("warn", 0, 0)
            }
        };
        if status == "bad" {
            problems.push("the bot's database dump is broken or missing".into());
        }
        sections.push(Section {
            label: format!("Bot database ({})", d.name),
            kind: "database".into(),
            path: format!("{}:{}/{}", d.host, d.port, d.name),
            status: status.into(),
            files_on_disk: 0,
            files_in_zip: count,
            bytes_in_zip: bytes,
            checks,
            skipped: vec![],
        });
    }
    if !names.iter().any(|(n, _)| n == "manager/manager.json") {
        problems.push("the manager's settings (manager/manager.json) are not in the zip".into());
    }
    for kind in [Kind::Bot, Kind::Instance] {
        if !m.roots.iter().any(|r| r.kind == kind) {
            problems.push(format!("no {} section in this backup", if kind == Kind::Bot { "DCSServerBot" } else { "DCS server" }));
        }
    }
    if !corrupt.is_empty() {
        problems.push(format!("{} file(s) in the zip don't read back -- make the backup again", corrupt.len()));
    }
    let ok = problems.is_empty() && corrupt.is_empty();
    let report = VerifyReport {
        zip: disp(zip_path),
        ok,
        created: m.created.clone(),
        hostname: m.hostname.clone(),
        manager_version: m.manager_version.clone(),
        entries: names.len() as u64 + corrupt.len() as u64,
        bytes: total,
        zip_bytes,
        sections,
        corrupt,
        problems,
    };
    note(String::new());
    for l in report.to_text().lines() {
        note(l.to_string());
    }
    Ok(report)
}

// ---- finding backups -------------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct FoundBackup {
    pub path: String,
    pub bytes: u64,
    pub modified: Option<String>,
}

/// Backup zips in the usual places: every drive's root and FowlEngineBackups
/// folder, and the current user's Desktop / Downloads / Documents.
pub fn find_backups() -> Vec<FoundBackup> {
    let mut dirs: Vec<PathBuf> = Vec::new();
    for d in data_drives() {
        dirs.push(PathBuf::from(format!("{d}\\")));
        dirs.push(PathBuf::from(format!("{d}\\FowlEngineBackups")));
    }
    if let Some(home) = std::env::var_os("USERPROFILE").map(PathBuf::from) {
        for s in ["Desktop", "Downloads", "Documents", "OneDrive\\Desktop", "OneDrive\\Documents"] {
            dirs.push(home.join(s));
        }
    }
    let mut out: Vec<FoundBackup> = Vec::new();
    for d in dirs {
        let Ok(rd) = std::fs::read_dir(&d) else { continue };
        for e in rd.flatten() {
            let n = e.file_name().to_string_lossy().to_string();
            if n.starts_with(ZIP_PREFIX) && n.to_lowercase().ends_with(".zip") {
                let meta = e.metadata().ok();
                let p = disp(&e.path());
                if !out.iter().any(|f| lower(&f.path) == lower(&p)) {
                    out.push(FoundBackup {
                        path: p,
                        bytes: meta.as_ref().map(|m| m.len()).unwrap_or(0),
                        modified: meta
                            .and_then(|m| m.modified().ok())
                            .map(|t| chrono::DateTime::<chrono::Local>::from(t).to_rfc3339()),
                    });
                }
            }
        }
    }
    out.sort_by(|a, b| b.modified.cmp(&a.modified));
    out
}

// ---- restore ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct Mapping {
    pub id: String,
    pub kind: Kind,
    pub label: String,
    pub from: String,
    pub to: String,
    pub files: u64,
    pub bytes: u64,
    /// Something is already at `to` (it is moved aside, not deleted).
    pub exists: bool,
    pub problem: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Check {
    pub what: String,
    pub ok: bool,
    pub detail: String,
    /// "python" | "postgres": this app can install it.
    pub install: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct RestorePreview {
    pub zip: String,
    pub zip_bytes: u64,
    pub manifest: Manifest,
    pub mappings: Vec<Mapping>,
    pub hostname_now: String,
    pub hostname_changed: bool,
    pub profile_now: Option<String>,
    pub desktop_user: Option<String>,
    pub checks: Vec<Check>,
    pub has_database: bool,
    pub postgres_found: Option<String>,
    pub service_installed: bool,
}

fn read_manifest(zip_path: &Path) -> Result<(Manifest, u64)> {
    let f = File::open(zip_path).with_context(|| format!("opening {}", zip_path.display()))?;
    let size = f.metadata().map(|m| m.len()).unwrap_or(0);
    let mut a = zip::ZipArchive::new(f).context("not a zip file (or a damaged one)")?;
    let mut s = String::new();
    a.by_name(MANIFEST)
        .map_err(|_| anyhow!("this zip is not a Fowl Engine backup (no {MANIFEST} inside)"))?
        .read_to_string(&mut s)?;
    let m: Manifest = serde_json::from_str(&s).context("reading the backup's manifest")?;
    if m.format > FORMAT {
        bail!("this backup was made by a newer Fowl Engine Manager ({}) -- update this app first", m.manager_version);
    }
    Ok((m, size))
}

/// Where the old profile goes on this PC: the same folder when that user
/// exists here, else the profile of whoever runs this app.
fn new_profile(old: &str) -> Option<String> {
    if Path::new(old).is_dir() {
        return Some(old.to_string());
    }
    let name = old.rsplit('\\').next()?;
    let same = format!("{}\\Users\\{name}", system_drive());
    if Path::new(&same).is_dir() {
        return Some(same);
    }
    std::env::var("USERPROFILE").ok()
}

/// `p` with the prefix `from` replaced by `to` (case-insensitive), if it has it.
pub fn remap_prefix(p: &str, from: &str, to: &str) -> Option<String> {
    if !is_under(p, from) {
        return None;
    }
    let cut = from.trim_end_matches(['\\', '/']).len();
    Some(format!("{}{}", to.trim_end_matches(['\\', '/']), &p[cut.min(p.len())..]))
}

fn drive_exists(p: &str) -> bool {
    p.len() >= 2 && Path::new(&format!("{}\\", &p[..2])).exists()
}

fn non_empty_dir(p: &Path) -> bool {
    std::fs::read_dir(p).map(|mut r| r.next().is_some()).unwrap_or(false)
}

pub fn inspect(zip: &str) -> Result<RestorePreview> {
    let zip_path = PathBuf::from(zip.trim().trim_matches('"'));
    let (m, zip_bytes) = read_manifest(&zip_path)?;
    let profile_now = m.profile.as_deref().and_then(new_profile);
    let mut mappings = Vec::new();
    for r in &m.roots {
        let to = match (&m.profile, &profile_now) {
            (Some(old), Some(new)) => remap_prefix(&r.path, old, new).unwrap_or_else(|| r.path.clone()),
            _ => r.path.clone(),
        };
        let problem = (!drive_exists(&to)).then(|| format!("drive {} doesn't exist on this PC -- pick another folder", &to[..2.min(to.len())]));
        mappings.push(Mapping {
            id: r.id.clone(),
            kind: r.kind,
            label: r.label.clone(),
            from: r.path.clone(),
            exists: if r.only.is_empty() {
                non_empty_dir(Path::new(&to))
            } else {
                r.only.iter().any(|f| Path::new(&to).join(f).is_file())
            },
            to,
            files: r.files,
            bytes: r.bytes,
            problem,
        });
    }
    let host_now = std::env::var("COMPUTERNAME").unwrap_or_default();
    let desktop_user = match (&m.desktop_user, &profile_now) {
        (_, Some(p)) => p.rsplit('\\').next().map(|n| format!(".\\{n}")),
        (Some(u), None) => Some(u.clone()),
        _ => None,
    };

    let mut checks = Vec::new();
    let py = find_python();
    checks.push(Check {
        what: "Python 3.11+ (runs DCSServerBot)".into(),
        ok: py.as_ref().map(|(_, v)| python_ok(v)).unwrap_or(false),
        detail: match &py {
            Some((p, v)) => format!("{v} at {}", disp(p)),
            None => "not found -- DCSServerBot can't start without it".into(),
        },
        install: Some("python".into()),
    });
    let pg = postgres_bins();
    let has_database = m.database.as_ref().and_then(|d| d.dump.as_ref()).is_some();
    if m.database.is_some() {
        checks.push(Check {
            what: "PostgreSQL (the bot's database)".into(),
            ok: !pg.is_empty(),
            detail: match pg.first() {
                Some((v, b)) => format!("PostgreSQL {v} at {}", disp(b)),
                None => "not installed -- DCSServerBot can't start without it".into(),
            },
            install: Some("postgres".into()),
        });
    }
    let carried = |p: &str| m.roots.iter().any(|r| r.only.is_empty() && is_under(p, &r.path));
    for p in &m.programs {
        if p.what == "referenced by the bot's config" || carried(&p.path) {
            continue;
        }
        let ok = Path::new(&p.path).exists();
        checks.push(Check {
            what: p.what.clone(),
            ok,
            detail: if ok { p.path.clone() } else { format!("not at {} -- install it there (or fix the path in the bot's nodes.yaml)", p.path) },
            install: None,
        });
    }
    let missing: Vec<&Program> = m
        .programs
        .iter()
        .filter(|p| p.what == "referenced by the bot's config" && !Path::new(&p.path).exists() && !carried(&p.path))
        .collect();
    if !missing.is_empty() {
        checks.push(Check {
            what: "Other programs the bot's config uses".into(),
            ok: false,
            detail: missing.iter().map(|p| p.path.clone()).collect::<Vec<_>>().join("\n"),
            install: None,
        });
    }

    let has_netidx = m.roots.iter().any(|r| r.kind == Kind::Netidx && !r.only.is_empty());
    if !has_netidx {
        let found = find_netidx(profile_now.as_deref().map(Path::new));
        checks.push(Check {
            what: "netidx (the resolver behind the live map and stats)".into(),
            ok: found.is_some(),
            detail: match found {
                Some(d) => format!("netidx.exe in {}", disp(&d)),
                None => "not in this backup and not on this PC -- install it: cargo install netidx-tools".into(),
            },
            install: None,
        });
    }

    #[cfg(windows)]
    let service_installed = crate::winsvc::status(config::SERVICE_NAME).installed;
    #[cfg(not(windows))]
    let service_installed = false;

    Ok(RestorePreview {
        zip: disp(&zip_path),
        zip_bytes,
        hostname_changed: !m.hostname.eq_ignore_ascii_case(&host_now),
        hostname_now: host_now,
        profile_now,
        desktop_user,
        mappings,
        checks,
        has_database,
        postgres_found: pg.first().map(|(_, b)| disp(b)),
        service_installed,
        manifest: m,
    })
}

#[derive(Debug, Clone, Deserialize)]
pub struct RestoreTarget {
    pub id: String,
    pub to: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RestoreOptions {
    pub zip: String,
    pub targets: Vec<RestoreTarget>,
    #[serde(default)]
    pub restore_database: bool,
    /// The PostgreSQL superuser's ("postgres") password: for installing
    /// PostgreSQL and for creating the bot's database.
    #[serde(default)]
    pub pg_password: Option<String>,
    #[serde(default)]
    pub install_python: bool,
    #[serde(default)]
    pub install_postgres: bool,
    #[serde(default)]
    pub install_service: bool,
    #[serde(default)]
    pub desktop_user: Option<String>,
}

pub fn start_restore(opts: RestoreOptions) -> Result<()> {
    if (opts.restore_database || opts.install_postgres) && opts.pg_password.as_deref().map(str::is_empty).unwrap_or(true) {
        bail!("type the PostgreSQL 'postgres' password (the one chosen when PostgreSQL was installed, or a new one if this app installs it)");
    }
    spawn("restore", move || run_restore(opts))
}

/// Reject restore targets that would land on a drive root or in Windows.
fn check_target(to: &str) -> Result<PathBuf> {
    let p = PathBuf::from(to.trim());
    if !p.is_absolute() || to.trim().len() <= 3 {
        bail!("{to}: needs a full folder path (not a drive root)");
    }
    let sd = system_drive();
    if is_under(to, &format!("{sd}\\Windows")) {
        bail!("{to}: not into the Windows folder");
    }
    if !drive_exists(to) {
        bail!("{to}: that drive doesn't exist on this PC");
    }
    Ok(p)
}

fn winget(args: &[&str]) -> Result<()> {
    let out = hidden("winget.exe")
        .args(args)
        .args(["--accept-package-agreements", "--accept-source-agreements", "--disable-interactivity"])
        .output()
        .map_err(|e| anyhow!("winget is not available ({e}) -- install it from the Microsoft Store (App Installer), or install the program by hand"))?;
    let text = format!("{}{}", String::from_utf8_lossy(&out.stdout), String::from_utf8_lossy(&out.stderr));
    if !out.status.success() {
        let tail: Vec<&str> = text.lines().filter(|l| !l.trim().is_empty()).rev().take(4).collect();
        bail!("winget failed (exit {:?}): {}", out.status.code(), tail.into_iter().rev().collect::<Vec<_>>().join(" | "));
    }
    Ok(())
}

/// Replace every spelling of `from` (single or doubled backslashes, forward
/// slashes) with the same spelling of `to`, ignoring ASCII case.
pub fn rewrite_text(text: &str, pairs: &[(String, String)]) -> String {
    let mut out = text.to_string();
    for (from, to) in pairs {
        let from = from.trim_end_matches(['\\', '/']);
        let to = to.trim_end_matches(['\\', '/']);
        for (f, t) in [
            (from.replace('/', "\\").replace('\\', "\\\\"), to.replace('/', "\\").replace('\\', "\\\\")),
            (from.replace('/', "\\"), to.replace('/', "\\")),
            (from.replace('\\', "/"), to.replace('\\', "/")),
        ] {
            out = replace_ci(&out, &f, &t);
        }
    }
    out
}

fn replace_ci(hay: &str, needle: &str, with: &str) -> String {
    if needle.is_empty() {
        return hay.to_string();
    }
    let lh = hay.to_ascii_lowercase();
    let ln = needle.to_ascii_lowercase();
    let mut out = String::with_capacity(hay.len());
    let mut last = 0;
    let mut i = 0;
    while let Some(pos) = lh[i..].find(&ln) {
        let at = i + pos;
        // only a whole path prefix: what follows must end the path or go deeper
        let next = hay[at + needle.len()..].chars().next();
        let boundary = matches!(next, None | Some('\\' | '/' | '"' | '\'' | '\r' | '\n' | ' ' | ',' | ']' | '}' | ';'));
        if boundary {
            out.push_str(&hay[last..at]);
            out.push_str(with);
            last = at + needle.len();
        }
        i = at + needle.len();
    }
    out.push_str(&hay[last..]);
    out
}

const TEXT_EXT: [&str; 10] = ["yaml", "yml", "json", "lua", "cfg", "ini", "txt", "toml", "xml", "conf"];

fn rewrite_tree(root: &Path, pairs: &[(String, String)], changed: &mut Vec<String>) {
    let Ok(rd) = std::fs::read_dir(root) else { return };
    for e in rd.flatten() {
        let p = e.path();
        let Ok(meta) = std::fs::symlink_metadata(&p) else { continue };
        if meta.file_type().is_symlink() {
            continue;
        }
        if meta.is_dir() {
            let n = e.file_name().to_string_lossy().to_lowercase();
            // missions and the bfdb database are binary; skip the deep ones
            if n != "bfdb" && n != "missions" && n != "mods" && n != ".git" {
                rewrite_tree(&p, pairs, changed);
            }
            continue;
        }
        let ext = p.extension().map(|x| x.to_string_lossy().to_lowercase()).unwrap_or_default();
        if !TEXT_EXT.contains(&ext.as_str()) || meta.len() > REWRITE_MAX {
            continue;
        }
        let Ok(text) = std::fs::read_to_string(&p) else { continue };
        let new = rewrite_text(&text, pairs);
        if new != text && std::fs::write(&p, new).is_ok() {
            changed.push(disp(&p));
        }
    }
}

/// nodes.yaml is keyed by the PC's name; DCSServerBot (run.cmd) looks itself
/// up by %COMPUTERNAME%.
pub fn rename_node(text: &str, old: &str, new: &str) -> Option<String> {
    let mut hit = false;
    let lines: Vec<String> = text
        .lines()
        .map(|l| {
            let key = l.split(':').next().unwrap_or("");
            if !hit && !l.starts_with([' ', '\t', '#']) && key.trim().trim_matches(['"', '\'']).eq_ignore_ascii_case(old) && l.contains(':') {
                hit = true;
                format!("{new}:{}", &l[key.len() + 1..])
            } else {
                l.to_string()
            }
        })
        .collect();
    hit.then(|| {
        let mut s = lines.join("\n");
        if text.ends_with('\n') {
            s.push('\n');
        }
        s
    })
}

fn run_restore(opts: RestoreOptions) -> Result<String> {
    let zip_path = PathBuf::from(opts.zip.trim().trim_matches('"'));
    phase("Reading the backup");
    let (m, _) = read_manifest(&zip_path)?;

    // where each root goes
    let mut plan: Vec<(Root, PathBuf)> = Vec::new();
    for r in &m.roots {
        let to = opts.targets.iter().find(|t| t.id == r.id).map(|t| t.to.clone()).unwrap_or_else(|| r.path.clone());
        if to.trim().is_empty() {
            note(format!("skipping {} (no target)", r.label));
            continue;
        }
        plan.push((r.clone(), check_target(&to)?));
    }
    // parents before children, so moving a folder aside never moves one just restored
    plan.sort_by_key(|(_, to)| to.as_os_str().len());
    let total: u64 = plan.iter().map(|(r, _)| r.bytes).sum::<u64>() + m.database.as_ref().map(|d| d.bytes).unwrap_or(0);
    with_job(|j| j.total_bytes = total);
    if let Some((_, to)) = plan.first() {
        if let Some(free) = free_space(to) {
            if free < total {
                bail!("only {} free on {}; the backup unpacks to {}", fmt_bytes(free), disp(to), fmt_bytes(total));
            }
        }
    }

    // the service would start the bot half-way through
    #[cfg(windows)]
    {
        let st = crate::winsvc::status(config::SERVICE_NAME);
        if st.installed && st.state.as_deref() == Some("running") {
            phase("Stopping the FowlEngine service");
            crate::winsvc::control(config::SERVICE_NAME, "stop").context("stopping the FowlEngine service")?;
        }
    }
    cancelled()?;

    if opts.install_python {
        if find_python().map(|(_, v)| python_ok(&v)).unwrap_or(false) {
            note("Python is already installed".into());
        } else {
            phase("Installing Python 3.12 (winget)");
            match winget(&[
                "install", "-e", "--id", "Python.Python.3.12", "--scope", "machine", "--silent",
                "--override", "/quiet InstallAllUsers=1 PrependPath=1 Include_test=0",
            ]) {
                Ok(()) => note("  Python installed".into()),
                Err(e) => {
                    warn(format!("Python was not installed: {e:#}"));
                    next_step("Install Python 3.12 from python.org for all users, with \"Add python.exe to PATH\" ticked.".into());
                }
            }
        }
    }
    if opts.install_postgres {
        if !postgres_bins().is_empty() {
            note("PostgreSQL is already installed".into());
        } else {
            let major = m.database.as_ref().and_then(|d| d.pg_version.as_deref()).and_then(pg_major).unwrap_or(17).max(16);
            let port = m.database.as_ref().map(|d| d.port).unwrap_or(5432);
            phase(&format!("Installing PostgreSQL {major} (winget) -- this takes a few minutes"));
            let pw = opts.pg_password.clone().unwrap_or_default();
            let ov = format!("--mode unattended --unattendedmodeui none --superpassword \"{pw}\" --serverport {port}");
            let id = format!("PostgreSQL.PostgreSQL.{major}");
            match winget(&["install", "-e", "--id", &id, "--silent", "--override", &ov]) {
                Ok(()) => note("  PostgreSQL installed".into()),
                Err(e) => {
                    warn(format!("PostgreSQL was not installed: {e:#}"));
                    next_step(format!(
                        "Install PostgreSQL {major} from postgresql.org (port {port}), then run the restore again with only the database ticked."
                    ));
                }
            }
        }
    }

    // unpack
    let f = File::open(&zip_path)?;
    let mut a = zip::ZipArchive::new(f)?;
    let stamp = chrono::Local::now().format("%Y%m%d-%H%M%S");
    let mut restored: Vec<PathBuf> = Vec::new();
    let mut pairs: Vec<(String, String)> = Vec::new();
    for (r, to) in &plan {
        cancelled()?;
        phase(&format!("Restoring {} → {}", r.label, disp(to)));
        let inside_restored = restored.iter().any(|p| is_under(&disp(to), &disp(p)));
        if !r.only.is_empty() {
            // a few files out of a shared folder (.cargo\bin): keep the
            // folder, set aside only the files about to be replaced
            for name in &r.only {
                let cur = to.join(name);
                if cur.is_file() {
                    let aside = PathBuf::from(format!("{}.before-restore-{stamp}", disp(&cur)));
                    std::fs::rename(&cur, &aside).with_context(|| format!("moving {} aside (is it running?)", disp(&cur)))?;
                    note(format!("  the existing {} was moved to {}", name, disp(&aside)));
                }
            }
        } else if !inside_restored && non_empty_dir(to) {
            let aside = PathBuf::from(format!("{}.before-restore-{stamp}", disp(to)));
            std::fs::rename(to, &aside).with_context(|| {
                format!("moving the existing {} aside (is something still using it? close DCS, the bot and any Explorer window in it)", disp(to))
            })?;
            note(format!("  the existing folder was moved to {}", disp(&aside)));
        }
        std::fs::create_dir_all(to)?;
        let prefix = format!("roots/{}/", r.id);
        let mut n = 0u64;
        for i in 0..a.len() {
            cancelled()?;
            let mut e = a.by_index(i)?;
            let Some(rel) = e.name().strip_prefix(&prefix).map(String::from) else { continue };
            if e.is_dir() || rel.is_empty() {
                continue;
            }
            // zip-slip guard: only plain relative parts
            if rel.split('/').any(|p| p.is_empty() || p == ".." || p == "." || p.contains(':')) || e.enclosed_name().is_none() {
                warn(format!("skipped a suspicious entry {}", e.name()));
                continue;
            }
            let dest = rel.split('/').fold(to.clone(), |acc, p| acc.join(p));
            if let Some(parent) = dest.parent() {
                std::fs::create_dir_all(parent)?;
            }
            with_job(|j| j.current = Some(disp(&dest)));
            let mut out = File::create(&dest).with_context(|| format!("writing {}", dest.display()))?;
            std::io::copy(&mut Counting { inner: &mut e }, &mut out).with_context(|| format!("unpacking {}", rel))?;
            n += 1;
        }
        note(format!("  {n} files"));
        if r.kind == Kind::Netidx && r.only.iter().any(|f| f == "netidx.exe") {
            match ensure_on_machine_path(to) {
                Ok(true) => note(format!("  added {} to the machine PATH (procman runs `netidx` from PATH)", disp(to))),
                Ok(false) => note("  already on PATH".into()),
                Err(e) => {
                    warn(format!("could not put {} on PATH: {e:#}", disp(to)));
                    next_step(format!("Add {} to the system PATH so the bot can start the netidx resolver.", disp(to)));
                }
            }
        }
        if r.only.is_empty() {
            restored.push(to.clone());
        }
        let to_s = disp(to);
        if lower(&to_s) != lower(&r.path) {
            pairs.push((r.path.clone(), to_s));
        }
    }

    // paths in config files: moved folders first (longest first), then the profile
    let profile_now = m.profile.as_deref().and_then(|old| {
        // the profile the moved folders landed in, else new_profile()
        plan.iter()
            .find_map(|(r, to)| {
                let old_p = profile_of(&r.path)?;
                (lower(&old_p) == lower(old)).then(|| profile_of(&disp(to)))?
            })
            .or_else(|| new_profile(old))
    });
    pairs.sort_by_key(|(f, _)| std::cmp::Reverse(f.len()));
    if let (Some(old), Some(new)) = (&m.profile, &profile_now) {
        if lower(old) != lower(new) {
            pairs.push((old.clone(), new.clone()));
        }
    }
    if !pairs.is_empty() {
        phase("Updating paths in the config files");
        for (f, t) in &pairs {
            note(format!("  {f} → {t}"));
        }
        let mut changed = Vec::new();
        for p in &restored {
            rewrite_tree(p, &pairs, &mut changed);
        }
        note(format!("  {} file(s) updated", changed.len()));
        for c in changed.iter().take(30) {
            note(format!("    {c}"));
        }
    }

    // the bot folder
    let bot_to = plan.iter().find(|(r, _)| r.kind == Kind::Bot).map(|(_, to)| to.clone());
    let host_now = std::env::var("COMPUTERNAME").unwrap_or_default();
    if let Some(bot) = &bot_to {
        if !m.hostname.eq_ignore_ascii_case(&host_now) && !host_now.is_empty() {
            let nodes = crate::botcfg::config_dir(bot).join("nodes.yaml");
            if let Ok(text) = std::fs::read_to_string(&nodes) {
                match rename_node(&text, &m.hostname, &host_now) {
                    Some(new) => {
                        std::fs::write(&nodes, new)?;
                        note(format!("this PC is called {host_now}, the old one {}: renamed the node in nodes.yaml", m.hostname));
                    }
                    None => warn(format!(
                        "this PC is called {host_now} but nodes.yaml has no '{}:' node to rename -- check nodes.yaml",
                        m.hostname
                    )),
                }
            }
        }
    }

    // this app's settings
    phase("Restoring Fowl Engine Manager's settings");
    let mut cfg: ManagerConfig = match a.by_name("manager/manager.json") {
        Ok(mut e) => {
            let mut s = String::new();
            e.read_to_string(&mut s)?;
            serde_json::from_str(&s).unwrap_or_default()
        }
        Err(_) => ManagerConfig::default(),
    };
    if let Some(bot) = &bot_to {
        cfg.bot_dir = Some(disp(bot));
    }
    if let Some(u) = opts.desktop_user.as_deref().map(str::trim).filter(|u| !u.is_empty()) {
        cfg.desktop_user = Some(u.to_string());
    } else if let Some(p) = &profile_now {
        if let Some(name) = p.rsplit('\\').next() {
            cfg.desktop_user = Some(format!(".\\{name}"));
        }
    }
    if config::config_path().is_file() {
        let _ = std::fs::copy(config::config_path(), config::config_path().with_extension(format!("json.before-restore-{stamp}")));
    }
    cfg.save()?;

    // the bot's database
    let mut db_ok = None;
    if opts.restore_database {
        if let Some(d) = m.database.as_ref().filter(|d| d.dump.is_some()) {
            phase(&format!("Loading the bot's database {}", d.name));
            match restore_database(&mut a, d, bot_to.as_deref(), opts.pg_password.as_deref().unwrap_or("")) {
                Ok(msg) => {
                    note(format!("  {msg}"));
                    db_ok = Some(true);
                }
                Err(e) => {
                    warn(format!("the bot's database was not restored: {e:#}"));
                    db_ok = Some(false);
                    next_step(format!(
                        "Restore the database by hand: the dump is {} inside the zip (pg_restore --no-owner -d {}).",
                        d.dump.clone().unwrap_or_default(),
                        d.name
                    ));
                }
            }
        }
    }

    // the service
    #[cfg(windows)]
    {
        if opts.install_service {
            phase("Installing and starting the FowlEngine service");
            match crate::winsvc::install(None, None) {
                Ok(()) => note("  service installed and started".into()),
                Err(e) => warn(format!("the service was not installed: {e:#} -- Setup step 3")),
            }
        } else if crate::winsvc::status(config::SERVICE_NAME).installed {
            let _ = crate::winsvc::control(config::SERVICE_NAME, "start");
            note("started the FowlEngine service again".into());
        }
    }

    next_step(
        "Setup → step 4: turn automatic sign-in on again for the server's user (Windows never lets anyone read the old password back, so it is not in the backup)."
            .into(),
    );
    for p in &m.programs {
        if p.what != "referenced by the bot's config" && !Path::new(&p.path).exists()
            && !m.roots.iter().any(|r| r.only.is_empty() && is_under(&p.path, &r.path))
        {
            next_step(format!("Install {} at {} (not in the backup).", p.what, p.path));
        }
    }
    if !find_python().map(|(_, v)| python_ok(&v)).unwrap_or(false) {
        next_step("Install Python 3.11+ for all users with \"Add to PATH\" -- the bot can't start without it.".into());
    }
    next_step("Check the router's port forwards / Windows firewall for DCS, SRS and the bot's web ports if this PC's address changed.".into());
    next_step("Watch the OVERVIEW tab: the bot's first start builds its Python venv, which takes a few minutes.".into());

    Ok(format!(
        "{} folder(s) restored{}",
        restored.len(),
        match db_ok {
            Some(true) => ", database loaded",
            Some(false) => ", database NOT loaded (see warnings)",
            None => "",
        }
    ))
}

fn sql_lit(s: &str) -> String {
    format!("'{}'", s.replace('\'', "''"))
}

fn sql_ident(s: &str) -> String {
    format!("\"{}\"", s.replace('"', "\"\""))
}

fn restore_database(a: &mut zip::ZipArchive<File>, d: &DbInfo, bot: Option<&Path>, superpw: &str) -> Result<String> {
    if !is_local_host(&d.host) {
        bail!("the database was on another machine ({}) -- restore it there", d.host);
    }
    let (_, bin) = postgres_bins().into_iter().next().ok_or_else(|| anyhow!("PostgreSQL is not installed on this PC"))?;
    if let (Some(dumped), Some(have)) = (
        d.pg_version.as_deref().and_then(pg_major),
        tool_version(&exe(&bin, "pg_restore")).as_deref().and_then(pg_major),
    ) {
        if have < dumped {
            bail!("the dump is from PostgreSQL {dumped}, this PC has {have} -- install PostgreSQL {dumped} or newer");
        }
    }
    // the bot's password: the restored config\.secret has it (or the URL)
    let role_pw = bot
        .and_then(|b| std::fs::read(crate::botcfg::config_dir(b).join(".secret").join("database.pkl")).ok())
        .and_then(|b| unpickle_str(&b))
        .or_else(|| bot.and_then(bot_db).and_then(|c| c.password))
        .ok_or_else(|| anyhow!("the bot's database password was not found in the restored config\\.secret"))?;

    let port = d.port.to_string();
    let psql = |db: &str, sql: &str| -> Result<String> {
        let out = hidden(&exe(&bin, "psql").display().to_string())
            .args(["-h", &d.host, "-p", &port, "-U", "postgres", "-d", db, "-v", "ON_ERROR_STOP=1", "-tAc", sql])
            .env("PGPASSWORD", superpw)
            .output()?;
        if !out.status.success() {
            bail!("{}", String::from_utf8_lossy(&out.stderr).trim());
        }
        Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
    };
    // a just-installed server may still be starting
    let mut tries = 0;
    loop {
        match psql("postgres", "SELECT 1") {
            Ok(_) => break,
            Err(e) if tries < 15 && !e.to_string().contains("password authentication failed") => {
                tries += 1;
                std::thread::sleep(Duration::from_secs(2));
            }
            Err(e) => bail!("can't sign in to PostgreSQL as 'postgres': {e}"),
        }
    }
    let role = sql_ident(&d.user);
    psql(
        "postgres",
        &format!(
            "DO $$ BEGIN IF EXISTS (SELECT FROM pg_roles WHERE rolname = {}) THEN ALTER ROLE {role} WITH LOGIN PASSWORD {}; \
             ELSE CREATE ROLE {role} WITH LOGIN PASSWORD {}; END IF; END $$;",
            sql_lit(&d.user),
            sql_lit(&role_pw),
            sql_lit(&role_pw)
        ),
    )
    .context("creating the bot's database user")?;
    let exists = psql("postgres", &format!("SELECT 1 FROM pg_database WHERE datname = {}", sql_lit(&d.name)))? == "1";
    if exists {
        let aside = format!("{}_before_restore_{}", d.name, chrono::Local::now().format("%Y%m%d%H%M%S"));
        psql("postgres", &format!("ALTER DATABASE {} RENAME TO {}", sql_ident(&d.name), sql_ident(&aside)))
            .context("moving the existing database aside (stop DCSServerBot first)")?;
        note(format!("  the existing database was renamed {aside}"));
    }
    psql("postgres", &format!("CREATE DATABASE {} OWNER {role}", sql_ident(&d.name))).context("creating the database")?;

    // the dump, out of the zip
    let tmp = config::data_dir().join("restore-db.dump");
    {
        let mut e = a.by_name(d.dump.as_deref().unwrap_or_default())?;
        let mut out = File::create(&tmp)?;
        std::io::copy(&mut Counting { inner: &mut e }, &mut out)?;
    }
    let out = hidden(&exe(&bin, "pg_restore").display().to_string())
        .args(["--no-owner", "--no-privileges", "-h", &d.host, "-p", &port, "-U", &d.user, "-d", &d.name])
        .arg(&tmp)
        .env("PGPASSWORD", &role_pw)
        .output();
    let _ = std::fs::remove_file(&tmp);
    let out = out.context("running pg_restore")?;
    if !out.status.success() {
        let err = String::from_utf8_lossy(&out.stderr);
        let tail: Vec<&str> = err.lines().rev().take(3).collect();
        // pg_restore exits 1 for warnings it ignored; the data is usually in
        warn(format!("pg_restore reported problems: {}", tail.into_iter().rev().collect::<Vec<_>>().join(" | ")));
    }
    Ok(format!("database {} loaded", d.name))
}

/// Explorer, with the zip (or a backup log) selected.
pub fn reveal(path: &str) -> Result<()> {
    let p = PathBuf::from(path.trim());
    let ext = p.extension().map(|e| e.to_string_lossy().to_lowercase()).unwrap_or_default();
    if !p.is_file() || !(ext == "zip" || ext == "log") {
        bail!("not a backup zip or log");
    }
    std::process::Command::new("explorer.exe").arg(format!("/select,{}", disp(&p))).spawn()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn excludes() {
        assert!(excluded(Kind::Instance, "tracks", true));
        assert!(excluded(Kind::Instance, "logs/dcs.log", false));
        assert!(!excluded(Kind::Instance, "logs", true));
        assert!(!excluded(Kind::Instance, "logs/stats", true));
        assert!(!excluded(Kind::Instance, "logs/stats/0001.bin", false));
        assert!(!excluded(Kind::Instance, "logs/stats.jsonl", false));
        assert!(!excluded(Kind::Instance, "bfdb/db", false));
        assert!(!excluded(Kind::Instance, "missions/x.miz", false));
        assert!(!excluded(Kind::Instance, "vs_odfv2", false)); // a campaign save
        assert!(excluded(Kind::Bot, "logs", true));
        assert!(excluded(Kind::Bot, "plugins/x/__pycache__", true));
        assert!(excluded(Kind::Bot, "dcssb_host.pid", false));
        assert!(!excluded(Kind::Bot, "config/.secret/database.pkl", false));
    }

    #[test]
    fn pickle() {
        let p4 = b"\x80\x04\x95\x0b\x00\x00\x00\x00\x00\x00\x00\x8c\x07hunter2\x94.";
        assert_eq!(unpickle_str(p4).as_deref(), Some("hunter2"));
        let p0 = b"Vhunter2\np0\n.";
        assert_eq!(unpickle_str(p0).as_deref(), Some("hunter2"));
        assert_eq!(unpickle_str(b"junk"), None);
    }

    #[test]
    fn db_url() {
        let c = parse_db_url("postgres://dcsserverbot:SECRET@127.0.0.1:5432/dcsserverbot?sslmode=prefer").unwrap();
        assert_eq!((c.user.as_str(), c.port, c.name.as_str(), c.password), ("dcsserverbot", 5432, "dcsserverbot", None));
        let c = parse_db_url("postgres://u:p%40ss@localhost/db").unwrap();
        assert_eq!(c.password.as_deref(), Some("p@ss"));
    }

    #[test]
    fn paths() {
        assert!(is_under(r"C:\Users\A\Saved Games\DCS", r"c:\users\a"));
        assert!(!is_under(r"C:\Users\AB", r"C:\Users\A"));
        assert_eq!(remap_prefix(r"C:\Users\Old\Saved Games\DCS.x", r"C:\Users\old", r"C:\Users\New").as_deref(),
                   Some(r"C:\Users\New\Saved Games\DCS.x"));
        let pairs = vec![(r"C:\Users\ATPAdmin".to_string(), r"C:\Users\Admin".to_string())];
        let t = "home: C:\\Users\\ATPAdmin\\Saved Games\\DCS\nexe: \"C:\\\\Users\\\\atpadmin\\\\Saved Games\\\\x\"\nother: C:\\Users\\ATPAdmin2\\y\nfwd: C:/Users/ATPAdmin/z\n";
        let r = rewrite_text(t, &pairs);
        assert!(r.contains("home: C:\\Users\\Admin\\Saved Games\\DCS"));
        assert!(r.contains("C:\\\\Users\\\\Admin\\\\Saved Games"));
        assert!(r.contains("C:\\Users\\ATPAdmin2\\y"), "a longer name is left alone");
        assert!(r.contains("C:/Users/Admin/z"));
    }

    #[test]
    fn node_rename() {
        let t = "OLD-PC:\n  listen_port: 1\n  DCS:\n    OLD-PC: x\n";
        let r = rename_node(t, "old-pc", "NEW").unwrap();
        assert!(r.starts_with("NEW:\n"));
        assert!(r.contains("    OLD-PC: x"));
        assert!(rename_node("A:\n", "B", "C").is_none());
    }

    /// Back up a fake box, wipe it, restore it for another Windows user and
    /// PC name. Touches the real file system and env, so opt-in:
    /// `FOWL_E2E_DIR=E:\tmp\fowl-e2e cargo test --lib e2e -- --ignored`
    #[test]
    #[ignore]
    fn e2e_roundtrip() {
        let base = PathBuf::from(std::env::var("FOWL_E2E_DIR").expect("set FOWL_E2E_DIR"));
        let _ = std::fs::remove_dir_all(&base);
        let old_user = base.join("Users").join("olduser");
        let new_user = base.join("Users").join("newuser");
        let home = old_user.join("Saved Games").join("DCS.test");
        let bot = base.join("bot");
        let w = |p: PathBuf, s: &str| {
            std::fs::create_dir_all(p.parent().unwrap()).unwrap();
            std::fs::write(p, s).unwrap();
        };
        w(bot.join("run.py"), "");
        w(bot.join("core").join("x.py"), "");
        w(bot.join("plugins").join("p.py"), "");
        w(bot.join("plugins").join("fowlengine").join("commands.py"), "");
        w(bot.join("config").join("main.yaml"), "guild_id: 1");
        w(bot.join("config").join("plugins").join("fowlengine.yaml"), "DEFAULT: {}");
        w(bot.join("logs").join("bot.log"), "skip me");
        w(bot.join("config").join(".secret").join("database.pkl"), "secret");
        let srs = base.join("Program Files").join("SRS");
        w(srs.join("SR-Server.exe"), "srs");
        w(srs.join("server.cfg"), "[Server Settings]");
        w(old_user.join(".cargo").join("bin").join("netidx.exe"), "netidx");
        w(old_user.join(".cargo").join("bin").join("cargo.exe"), "not mine");
        w(old_user.join("AppData").join("Roaming").join("netidx").join("client.json"), "{\"addrs\":[]}");
        w(bot.join("config").join("nodes.yaml"), &format!(
            "OLDPC:\n  extensions:\n    SRS:\n      installation: {}\n  instances:\n    DCS.test:\n      home: {}\n      missions_dir: \"{}\"\n",
            disp(&srs), disp(&home), disp(&home.join("Missions")).replace('\\', "\\\\")));
        w(home.join("Config").join("serverSettings.lua"), &format!("missionList = {{ \"{}\" }}", disp(&home.join("Missions").join("a.miz")).replace('\\', "\\\\")));
        w(home.join("Missions").join("a.miz"), "MIZ");
        w(home.join("vs_save"), "campaign state");
        w(home.join("bfdb").join("db"), "sled");
        w(home.join("Logs").join("stats.jsonl"), "{}");
        w(home.join("Logs").join("dcs.log"), "skip me");
        w(home.join("Tracks").join("t.trk"), "skip me");
        std::fs::create_dir_all(&new_user).unwrap();
        let data = base.join("data");
        std::env::set_var("FOWL_MANAGER_DATA", &data);
        std::env::set_var("USERPROFILE", &old_user);
        let mut cfg = ManagerConfig::default();
        cfg.bot_dir = Some(disp(&bot));
        cfg.save().unwrap();

        let wait = || loop {
            std::thread::sleep(Duration::from_millis(200));
            let j = job_status().unwrap();
            if !j.running {
                return j;
            }
        };
        let plan = backup_plan().unwrap();
        // nodes.yaml is keyed OLDPC; this PC's name differs, so the first node is used
        let kinds: Vec<Kind> = plan.roots.iter().map(|r| r.kind).collect();
        assert_eq!(kinds, vec![Kind::Bot, Kind::Program, Kind::Instance, Kind::Netidx, Kind::Netidx],
                   "{:?}", plan.roots.iter().map(|r| &r.path).collect::<Vec<_>>());
        start_backup(BackupOptions {
            dest_dir: disp(&base.join("out")),
            roots: plan.roots.iter().map(|r| r.id.clone()).collect(),
            extra_paths: vec![],
            database: false,
            stop_bot: false,
            shadow_copy: false,
        })
        .unwrap();
        let j = wait();
        assert!(j.error.is_none(), "{j:?}");
        let zip = std::fs::read_dir(base.join("out"))
            .unwrap()
            .flatten()
            .map(|e| e.path())
            .find(|p| p.extension().map(|x| x == "zip").unwrap_or(false))
            .unwrap();
        let names: Vec<String> = {
            let a = zip::ZipArchive::new(File::open(&zip).unwrap()).unwrap();
            a.file_names().map(String::from).collect()
        };
        assert!(names.iter().any(|n| n.ends_with("/vs_save")));
        assert!(names.iter().any(|n| n.ends_with("/bfdb/db")));
        assert!(names.iter().any(|n| n.ends_with("/.secret/database.pkl")));
        assert!(names.iter().any(|n| n.ends_with("/Logs/stats.jsonl")));
        assert!(!names.iter().any(|n| n.contains("dcs.log") || n.contains("Tracks") || n.contains("bot.log")), "{names:?}");
        assert!(names.iter().any(|n| n.ends_with("/SR-Server.exe")));
        assert!(names.iter().any(|n| n.ends_with("/netidx.exe")));
        assert!(names.iter().any(|n| n.starts_with("roots/netidx-netidx-") && n.ends_with("/client.json")), "{names:?}");
        assert!(!names.iter().any(|n| n.ends_with("/cargo.exe")), "only netidx.exe out of .cargo\\bin");

        // the check that ran at the end of the backup
        let rep = j.report.clone().expect("a backup carries its check");
        assert!(rep.ok, "{}", rep.to_text());
        assert!(rep.sections.iter().all(|s| s.status == "ok"), "{}", rep.to_text());
        assert!(rep.sections.iter().all(|s| s.files_in_zip > 0), "{}", rep.to_text());
        let log = std::fs::read_to_string(zip.with_extension("log")).expect("the log sits next to the zip");
        assert!(log.contains("BACKUP CHECK: OK") && log.contains("LOG"), "{log}");
        assert_eq!(j.log_file.as_deref().map(lower), Some(lower(&disp(&zip.with_extension("log")))));

        // one flipped byte in a stored entry (the .miz) must be caught
        let mut bytes = std::fs::read(&zip).unwrap();
        let at = bytes.windows(3).position(|w| w == b"MIZ").expect("stored miz data");
        bytes[at + 2] = b'X';
        let bad = base.join("corrupt.zip");
        std::fs::write(&bad, bytes).unwrap();
        let rep = verify_zip(&bad).unwrap();
        assert!(!rep.ok && rep.corrupt.iter().any(|c| c.contains("a.miz")), "{}", rep.to_text());
        assert!(rep.sections.iter().any(|s| s.status == "bad"), "{}", rep.to_text());

        // fresh Windows: everything gone, another user
        std::fs::remove_dir_all(&bot).unwrap();
        std::fs::remove_dir_all(&srs).unwrap();
        // the new user already has Rust: their .cargo\bin must survive
        w(new_user.join(".cargo").join("bin").join("rustc.exe"), "theirs");
        std::fs::remove_dir_all(&old_user).unwrap();
        std::fs::remove_dir_all(&data).unwrap();
        std::env::set_var("USERPROFILE", &new_user);
        let prev = inspect(&disp(&zip)).unwrap();
        let new_home = new_user.join("Saved Games").join("DCS.test");
        assert!(prev.mappings.iter().any(|m| lower(&m.to) == lower(&disp(&new_home))), "{:?}", prev.mappings);
        start_restore(RestoreOptions {
            zip: disp(&zip),
            targets: prev.mappings.iter().map(|m| RestoreTarget { id: m.id.clone(), to: m.to.clone() }).collect(),
            restore_database: false,
            pg_password: None,
            install_python: false,
            install_postgres: false,
            install_service: false,
            desktop_user: None,
        })
        .unwrap();
        let j = wait();
        assert!(j.error.is_none(), "{j:?}");
        assert_eq!(std::fs::read_to_string(new_home.join("vs_save")).unwrap(), "campaign state");
        assert!(srs.join("SR-Server.exe").is_file(), "SRS back where it was");
        let cbin = new_user.join(".cargo").join("bin");
        assert!(cbin.join("netidx.exe").is_file() && cbin.join("rustc.exe").is_file(), "netidx added, rustc kept");
        assert!(new_user.join("AppData").join("Roaming").join("netidx").join("client.json").is_file());
        let nodes = std::fs::read_to_string(bot.join("config").join("nodes.yaml")).unwrap();
        assert!(nodes.contains("newuser") && !nodes.contains("olduser"), "{nodes}");
        let lua = std::fs::read_to_string(new_home.join("Config").join("serverSettings.lua")).unwrap();
        assert!(lua.contains("newuser"), "{lua}");
        let cfg = ManagerConfig::load();
        assert_eq!(cfg.desktop_user.as_deref(), Some(".\\newuser"));
        assert_eq!(cfg.bot_dir.as_deref().map(lower), Some(lower(&disp(&bot))));
        let _ = std::fs::remove_dir_all(&base);
    }

    /// What 0.2.18 reported as WARN on the live box: bfdb/db cut short by a
    /// lock. That's a broken backup; a vanished save temp file is not.
    #[test]
    fn partial_read_is_broken() {
        let dir = std::env::temp_dir().join(format!("fowl-partial-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let zp = dir.join("b.zip");
        let root = |skipped: Vec<String>, vanished: u64| Root {
            id: "instance-x".into(),
            kind: Kind::Instance,
            label: "DCS server x".into(),
            path: r"C:\x".into(),
            files: 3,
            bytes: 0,
            include: true,
            note: None,
            only: vec![],
            expected_files: 3,
            skipped,
            vanished,
        };
        let write = |r: Root| {
            let mut z = zip::ZipWriter::new(File::create(&zp).unwrap());
            let o = zip::write::SimpleFileOptions::default();
            for (n, d) in [("roots/instance-x/Config/serverSettings.lua", "x"), ("roots/instance-x/Missions/a.miz", "m"),
                           ("roots/instance-x/bfdb/db", "half"), ("manager/manager.json", "{}")] {
                z.start_file(n, o).unwrap();
                z.write_all(d.as_bytes()).unwrap();
            }
            let m = Manifest {
                format: FORMAT, created: now(), hostname: "H".into(), manager_version: "t".into(), desktop_user: None,
                profile: None, bot_dir: None, roots: vec![r], database: None, programs: vec![], python: None,
                service_installed: false, warnings: vec![],
            };
            z.start_file(MANIFEST, o).unwrap();
            z.write_all(&serde_json::to_vec(&m).unwrap()).unwrap();
            z.finish().unwrap();
        };
        write(root(vec!["bfdb/db (only partly read: locked)".into()], 0));
        let r = verify_zip(&zp).unwrap();
        assert_eq!(r.sections[0].status, "bad", "{}", r.to_text());
        write(root(vec![], 2));
        let r = verify_zip(&zp).unwrap();
        assert_eq!(r.sections[0].status, "ok", "{}", r.to_text());
        assert!(r.to_text().contains("--  2 temporary file(s) disappeared"), "{}", r.to_text());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn shadow_paths() {
        let s = Shadow {
            id: "{x}".into(),
            device: r"\\?\GLOBALROOT\Device\HarddiskVolumeShadowCopy12".into(),
            volume: "C:".into(),
        };
        assert_eq!(
            s.map(Path::new(r"c:\Users\A\Saved Games\DCS.x\bfdb\db")),
            Some(PathBuf::from(r"\\?\GLOBALROOT\Device\HarddiskVolumeShadowCopy12\Users\A\Saved Games\DCS.x\bfdb\db"))
        );
        assert_eq!(s.map(Path::new(r"D:\x")), None);
        std::mem::forget(s); // not a real shadow: don't try to delete it
    }

    /// `cargo test --lib shadow_live -- --ignored --nocapture`: elevated, it
    /// takes a real snapshot of C:, reads a locked file through it and deletes
    /// it; unelevated it must fail with a clear message, not a script error.
    #[test]
    #[ignore]
    fn shadow_live() {
        match Shadow::create("C:") {
            Ok(s) => {
                println!("shadow {} at {}", s.id, s.device);
                let p = s.map(Path::new(r"C:\Windows\System32\config\SYSTEM")).unwrap();
                let mut f = File::open(&p).expect("a file Windows keeps locked opens from the snapshot");
                let mut buf = [0u8; 4];
                f.read_exact(&mut buf).unwrap();
                assert_eq!(&buf, b"regf");
            }
            Err(e) => {
                let m = format!("{e:#}");
                println!("no shadow (expected without admin): {m}");
                assert!(!m.contains("ParserError") && !m.contains("Unexpected token"), "{m}");
            }
        }
    }

    #[test]
    fn python_version() {
        assert!(python_ok("Python 3.12.4"));
        assert!(python_ok("Python 3.11.0"));
        assert!(!python_ok("Python 3.10.9"));
        assert!(!python_ok("junk"));
    }
}
