//! AUTO-FIX tab: runs the auto-fix runner (deploy/autofix/autofix.py `watch`)
//! on this PC and shows what it does.
//!
//! The runner reads every box's issue report and /fe_bug reports through
//! their OPS API, has Claude Code fix them in a worktree of the repository,
//! and publishes the fix by itself when `publish_mode` is "auto" and the fix
//! passes its gates (deploy/autofix/README.md). This module only:
//!
//! * starts / stops it, handing it the boxes as FOWL_AUTOFIX_BOXES (the OPS
//!   keys stay DPAPI-encrypted in manager.json and are never written to disk
//!   in the clear for it)
//! * edits the runner's own settings (%LOCALAPPDATA%\fowl-autofix\config.json)
//! * reads what it leaves there (state.json, events.jsonl, current.json) for
//!   the UI, and runs its one-shot commands (queue, approve, pause, resume)
//!
//! A runner started by hand (`autofix.py watch` in a terminal) is found and
//! shown the same way; there is only ever one.

use crate::config::ManagerConfig;
use crate::servers;
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

pub const MASK: &str = "__SECRET__";
const SCRIPT: &str = "deploy/autofix/autofix.py";

/// The runner settings the UI may change, and how each is checked.
const EDITABLE: &[&str] = &[
    "publish_mode",
    "discord_webhook",
    "max_budget_usd",
    "max_runs_per_day",
    "max_publishes_per_day",
    "auto_max_files",
    "auto_max_changed_lines",
    "interval_minutes",
    "min_count",
    "sources",
    "model",
    "publish_channel",
    "base_branch",
];

/// What manager.json keeps about the runner (`autofix`).
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub struct AutofixSettings {
    /// Start the runner whenever this app starts (and keep the choice).
    #[serde(default)]
    pub enabled: bool,
    /// The bfnext checkout the runner works in (has deploy/autofix/autofix.py).
    #[serde(default)]
    pub repo: Option<String>,
    /// The Python to run it with (blank: `python` on PATH).
    #[serde(default)]
    pub python: Option<String>,
    /// Which servers' ids to watch (None: every one with a key).
    #[serde(default)]
    pub servers: Option<Vec<String>>,
}

pub fn data_dir() -> PathBuf {
    let base = std::env::var_os("LOCALAPPDATA").map(PathBuf::from).unwrap_or_else(std::env::temp_dir);
    base.join("fowl-autofix")
}

fn runner_config_path() -> PathBuf {
    data_dir().join("config.json")
}

fn now_secs() -> f64 {
    SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_secs_f64()).unwrap_or(0.0)
}

fn read_json(p: &Path) -> Option<Value> {
    std::fs::read_to_string(p).ok().and_then(|s| serde_json::from_str(&s).ok())
}

// ---- where things are ---------------------------------------------------------------------

/// The repository to run in: the setting, else the runner config's `repo`.
pub fn repo_dir(s: &AutofixSettings) -> Result<PathBuf> {
    let from_cfg = read_json(&runner_config_path())
        .and_then(|v| v.get("repo").and_then(|r| r.as_str()).map(str::to_string));
    let repo = s
        .repo
        .clone()
        .filter(|r| !r.trim().is_empty())
        .or(from_cfg)
        .ok_or_else(|| anyhow!("set the bfnext repository folder first"))?;
    let p = PathBuf::from(repo.trim());
    if !p.join(SCRIPT).is_file() {
        bail!("{} has no {SCRIPT} -- is it the bfnext checkout (vector branch)?", p.display());
    }
    Ok(p)
}

fn python(s: &AutofixSettings) -> String {
    s.python.clone().filter(|p| !p.trim().is_empty()).unwrap_or_else(|| "python".into())
}

/// Where `claude` is, if it is on PATH: the runner needs Claude Code.
fn find_on_path(exe: &str) -> Option<String> {
    let path = std::env::var_os("PATH")?;
    let exts: Vec<String> = if cfg!(windows) {
        std::env::var("PATHEXT")
            .unwrap_or_else(|_| ".EXE;.CMD;.BAT".into())
            .split(';')
            .map(|e| e.to_lowercase())
            .collect()
    } else {
        vec![String::new()]
    };
    for dir in std::env::split_paths(&path) {
        for ext in exts.iter().map(String::as_str).chain(std::iter::once("")) {
            let p = dir.join(format!("{exe}{ext}"));
            if p.is_file() {
                return Some(p.display().to_string());
            }
        }
    }
    None
}

// ---- the boxes ---------------------------------------------------------------------------

#[derive(Debug, Serialize)]
pub struct BoxView {
    pub id: String,
    pub name: String,
    pub watched: bool,
    pub problem: Option<String>,
}

/// The boxes to hand the runner: each picked server's name, OPS base and key.
/// Servers that can't be reached as configured are left out and reported.
pub fn boxes(cfg: &ManagerConfig) -> (Vec<Value>, Vec<BoxView>) {
    let mut out = Vec::new();
    let mut views = Vec::new();
    for v in servers::list(cfg) {
        let watched = cfg.autofix.servers.as_ref().map(|ids| ids.contains(&v.id)).unwrap_or(true);
        let mut problem = v.problem.clone();
        if watched && problem.is_none() {
            let id = if v.local { None } else { Some(v.id.as_str()) };
            match servers::target(cfg, id) {
                Ok(t) => out.push(json!({"name": t.name, "url": t.base, "key": t.key})),
                Err(e) => problem = Some(format!("{e:#}")),
            }
        }
        views.push(BoxView { id: v.id, name: v.name, watched, problem });
    }
    (out, views)
}

// ---- the runner process ------------------------------------------------------------------

/// `python ... autofix.py watch`: the command line of a runner.
pub fn is_runner_cmd(args: &[String]) -> bool {
    let script = args.iter().position(|a| a.replace('\\', "/").to_lowercase().ends_with("autofix/autofix.py"));
    match script {
        Some(i) => args[i + 1..].iter().any(|a| a == "watch"),
        None => false,
    }
}

/// The pid of the running runner, whoever started it.
pub fn find_runner() -> Option<u32> {
    let mut sys = sysinfo::System::new();
    sys.refresh_processes_specifics(
        sysinfo::ProcessesToUpdate::All,
        true,
        sysinfo::ProcessRefreshKind::nothing().with_cmd(sysinfo::UpdateKind::Always),
    );
    let mut procs: Vec<_> = sys
        .processes()
        .iter()
        .filter(|(_, p)| p.name().to_string_lossy().to_lowercase().starts_with("py"))
        .filter(|(_, p)| {
            let args: Vec<String> = p.cmd().iter().map(|a| a.to_string_lossy().to_string()).collect();
            is_runner_cmd(&args)
        })
        .map(|(pid, _)| pid.as_u32())
        .collect();
    // a venv launcher and its interpreter both match: the launcher is lower
    procs.sort();
    procs.first().copied()
}

fn command(s: &AutofixSettings, repo: &Path, args: &[&str]) -> Command {
    let mut c = Command::new(python(s));
    c.arg(repo.join(SCRIPT)).args(args).current_dir(repo);
    c.env("PYTHONIOENCODING", "utf-8").env("PYTHONUNBUFFERED", "1");
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        // CREATE_NO_WINDOW | CREATE_NEW_PROCESS_GROUP: no console, and it
        // keeps running when this app quits to the tray or exits
        c.creation_flags(0x0800_0000 | 0x0000_0200);
    }
    c
}

fn runner_log() -> Result<std::fs::File> {
    std::fs::create_dir_all(data_dir())?;
    let p = data_dir().join("runner.log");
    if std::fs::metadata(&p).map(|m| m.len() > 2 * 1024 * 1024).unwrap_or(false) {
        let _ = std::fs::rename(&p, data_dir().join("runner.1.log"));
    }
    std::fs::OpenOptions::new().create(true).append(true).open(&p).context("opening runner.log")
}

/// Start the runner unless one is running. Returns its pid.
pub fn start(cfg: &ManagerConfig) -> Result<u32> {
    if let Some(pid) = find_runner() {
        return Ok(pid);
    }
    let repo = repo_dir(&cfg.autofix)?;
    let (list, _) = boxes(cfg);
    if list.is_empty() {
        bail!("no server to watch: add one under SERVERS (with its OPS key), or tick one here");
    }
    let log = runner_log()?;
    let mut c = command(&cfg.autofix, &repo, &["watch"]);
    c.env("FOWL_AUTOFIX_BOXES", serde_json::to_string(&list)?)
        .stdin(Stdio::null())
        .stdout(log.try_clone()?)
        .stderr(log);
    let child = c.spawn().with_context(|| format!("starting {} (is Python installed?)", python(&cfg.autofix)))?;
    Ok(child.id())
}

/// Stop the runner. A run it was in the middle of is abandoned: its branch
/// is not pushed, and the next pass starts over.
pub fn stop() -> Result<bool> {
    let Some(pid) = find_runner() else { return Ok(false) };
    let mut sys = sysinfo::System::new();
    sys.refresh_processes(sysinfo::ProcessesToUpdate::All, true);
    // the runner and everything under it (claude, cargo, a venv's interpreter)
    let mut doomed = vec![sysinfo::Pid::from_u32(pid)];
    let mut i = 0;
    while i < doomed.len() {
        let parent = doomed[i];
        for (cpid, p) in sys.processes() {
            if p.parent() == Some(parent) && !doomed.contains(cpid) {
                doomed.push(*cpid);
            }
        }
        i += 1;
    }
    for p in doomed.iter().rev() {
        if let Some(p) = sys.process(*p) {
            p.kill();
        }
    }
    // what a killed pass leaves behind
    let _ = std::fs::remove_file(data_dir().join("lock"));
    let _ = std::fs::remove_file(data_dir().join("current.json"));
    Ok(true)
}

/// A one-shot runner command: pause, resume, queue <id>. Waits for it.
pub fn quick(s: &AutofixSettings, args: &[&str]) -> Result<String> {
    let repo = repo_dir(s)?;
    let out = command(s, &repo, args).output().with_context(|| format!("running {}", python(s)))?;
    let text = format!("{}{}", String::from_utf8_lossy(&out.stdout), String::from_utf8_lossy(&out.stderr));
    if !out.status.success() {
        bail!("{}", text.trim());
    }
    Ok(text.trim().to_string())
}

/// Publish a held fix: `approve <id>` in the background (it builds and signs
/// a release, which takes a while); its output goes to runner.log.
pub fn approve(cfg: &ManagerConfig, id: &str) -> Result<()> {
    if !id.chars().all(|c| c.is_ascii_alphanumeric() || c == '-') || id.is_empty() {
        bail!("not an auto-fix id: {id}");
    }
    let repo = repo_dir(&cfg.autofix)?;
    let (list, _) = boxes(cfg);
    let log = runner_log()?;
    let mut c = command(&cfg.autofix, &repo, &["approve", id]);
    c.env("FOWL_AUTOFIX_BOXES", serde_json::to_string(&list)?)
        .stdin(Stdio::null())
        .stdout(log.try_clone()?)
        .stderr(log);
    c.spawn().context("starting the publish")?;
    Ok(())
}

// ---- settings ----------------------------------------------------------------------------

/// Apply the UI's changes to the runner's config.json (other keys are kept).
/// discord_webhook == MASK keeps the saved one.
pub fn merge_settings(current: &mut Map<String, Value>, changes: &Map<String, Value>) -> Result<()> {
    for (k, v) in changes {
        if !EDITABLE.contains(&k.as_str()) {
            bail!("{k} can't be changed here");
        }
        match k.as_str() {
            "publish_mode" => {
                if !matches!(v.as_str(), Some("auto") | Some("approve")) {
                    bail!("publish_mode must be auto or approve");
                }
            }
            "discord_webhook" => {
                if v.as_str() == Some(MASK) {
                    continue;
                }
                let s = v.as_str().unwrap_or("");
                if !s.is_empty() && !s.starts_with("https://") {
                    bail!("the Discord webhook must be an https:// URL");
                }
            }
            "publish_channel" => {
                if !matches!(v.as_str(), Some("stable") | Some("beta")) {
                    bail!("publish_channel must be stable or beta");
                }
            }
            "sources" => {
                let ok = v.as_array().map(|a| {
                    a.iter().all(|s| matches!(s.as_str(), Some("engine" | "bot" | "dcs" | "bfdb" | "discord")))
                });
                if ok != Some(true) {
                    bail!("sources: a list of engine, bot, dcs, bfdb, discord");
                }
            }
            "model" | "base_branch" => {
                if v.as_str().map(|s| s.trim().is_empty()).unwrap_or(true) {
                    bail!("{k} can't be blank");
                }
            }
            "max_budget_usd" => {
                let n = v.as_f64().ok_or_else(|| anyhow!("{k} must be a number"))?;
                if !(0.5..=50.0).contains(&n) {
                    bail!("{k} must be between 0.5 and 50");
                }
            }
            _ => {
                let n = v.as_i64().ok_or_else(|| anyhow!("{k} must be a whole number"))?;
                if !(0..=10_000).contains(&n) {
                    bail!("{k} is out of range");
                }
            }
        }
        current.insert(k.clone(), v.clone());
    }
    Ok(())
}

pub fn save_runner_settings(changes: &Map<String, Value>, repo: Option<&Path>) -> Result<()> {
    let p = runner_config_path();
    let mut cur = match read_json(&p) {
        Some(Value::Object(m)) => m,
        Some(_) => bail!("{} is not a JSON object -- fix or delete it", p.display()),
        None => Map::new(),
    };
    merge_settings(&mut cur, changes)?;
    if let Some(r) = repo {
        cur.insert("repo".into(), json!(r.display().to_string()));
    }
    std::fs::create_dir_all(data_dir())?;
    let tmp = p.with_extension("json.tmp");
    std::fs::write(&tmp, serde_json::to_string_pretty(&Value::Object(cur))?)?;
    std::fs::rename(&tmp, &p)?;
    Ok(())
}

// ---- what the UI shows --------------------------------------------------------------------

fn tail_events(n: usize) -> Vec<Value> {
    let text = std::fs::read_to_string(data_dir().join("events.jsonl")).unwrap_or_default();
    let lines: Vec<&str> = text.lines().collect();
    lines[lines.len().saturating_sub(n)..].iter().filter_map(|l| serde_json::from_str(l).ok()).rev().collect()
}

fn count_today(v: Option<&Value>, now: f64) -> usize {
    v.and_then(|a| a.as_array())
        .map(|a| a.iter().filter_map(|t| t.as_f64()).filter(|t| now - t < 86_400.0).count())
        .unwrap_or(0)
}

pub fn state(cfg: &ManagerConfig) -> Value {
    let dir = data_dir();
    let mut settings = match read_json(&runner_config_path()) {
        Some(Value::Object(m)) => m,
        _ => Map::new(),
    };
    settings.remove("boxes"); // keys, when someone filled them in by hand
    if settings.get("discord_webhook").and_then(|w| w.as_str()).map(|w| !w.is_empty()).unwrap_or(false) {
        settings.insert("discord_webhook".into(), json!(MASK));
    }
    let st = read_json(&dir.join("state.json")).unwrap_or_else(|| json!({}));
    let now = now_secs();
    let (_, box_views) = boxes(cfg);
    let repo = repo_dir(&cfg.autofix);
    json!({
        "data_dir": dir.display().to_string(),
        "running": find_runner(),
        "paused": dir.join("paused").exists(),
        "settings": settings,
        "manager": cfg.autofix,
        "repo": repo.as_ref().ok().map(|p| p.display().to_string()),
        "repo_problem": repo.err().map(|e| format!("{e:#}")),
        "claude": find_on_path("claude"),
        "api_key": std::env::var_os("ANTHROPIC_API_KEY").is_some(),
        "boxes": box_views,
        "handled": st.get("handled").cloned().unwrap_or_else(|| json!({})),
        "runs_today": count_today(st.get("runs"), now),
        "publishes_today": count_today(st.get("publishes"), now),
        "last_pass": st.get("last_pass").cloned(),
        "current": read_json(&dir.join("current.json")),
        "queue": read_json(&dir.join("queue.json")).unwrap_or_else(|| json!([])),
        "events": tail_events(150),
    })
}

pub fn read_log(lines: usize) -> Vec<String> {
    let data = std::fs::read(data_dir().join("runner.log")).unwrap_or_default();
    let start = data.len().saturating_sub(1024 * 1024);
    let text = String::from_utf8_lossy(&data[start..]);
    let all: Vec<&str> = text.lines().collect();
    let n = lines.clamp(10, 5_000);
    all[all.len().saturating_sub(n)..].iter().map(|s| s.to_string()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::RemoteServer;

    fn obj(v: Value) -> Map<String, Value> {
        v.as_object().cloned().unwrap()
    }

    #[test]
    fn a_runner_is_known_by_its_command_line() {
        let a = |s: &[&str]| s.iter().map(|x| x.to_string()).collect::<Vec<_>>();
        assert!(is_runner_cmd(&a(&["python", r"E:\bfnext\deploy\autofix\autofix.py", "watch"])));
        assert!(is_runner_cmd(&a(&["py.exe", "deploy/autofix/autofix.py", "--config", "x.json", "watch"])));
        assert!(!is_runner_cmd(&a(&["python", "deploy/autofix/autofix.py", "approve", "abc"])));
        assert!(!is_runner_cmd(&a(&["python", "run.py", "watch"])));
    }

    #[test]
    fn settings_are_checked_and_the_webhook_kept() {
        let mut cur = obj(json!({"repo": "E:/x", "discord_webhook": "https://discord.com/api/webhooks/1/a"}));
        merge_settings(&mut cur, &obj(json!({"publish_mode": "auto", "discord_webhook": MASK, "max_runs_per_day": 5})))
            .unwrap();
        assert_eq!(cur["publish_mode"], "auto");
        assert_eq!(cur["discord_webhook"], "https://discord.com/api/webhooks/1/a");
        assert_eq!(cur["repo"], "E:/x");
        assert!(merge_settings(&mut cur, &obj(json!({"publish_mode": "yolo"}))).is_err());
        assert!(merge_settings(&mut cur, &obj(json!({"repo": "C:/elsewhere"}))).is_err());
        assert!(merge_settings(&mut cur, &obj(json!({"claude_cmd": "evil.exe"}))).is_err());
        assert!(merge_settings(&mut cur, &obj(json!({"discord_webhook": "http://plain"}))).is_err());
        assert!(merge_settings(&mut cur, &obj(json!({"sources": ["engine", "web"]}))).is_err());
        assert!(merge_settings(&mut cur, &obj(json!({"max_budget_usd": 500.0}))).is_err());
        merge_settings(&mut cur, &obj(json!({"sources": ["engine", "discord"], "max_budget_usd": 4.5}))).unwrap();
        assert_eq!(cur["max_budget_usd"], 4.5);
    }

    #[test]
    fn the_runner_gets_each_picked_box_with_its_key() {
        let mut cfg = ManagerConfig { controller_only: true, ..Default::default() };
        cfg.servers = vec![
            RemoteServer {
                id: "campaign".into(),
                name: "Campaign".into(),
                url: "http://100.101.2.84:9876/stats/fowlengine/ops".into(),
                key: Some("k1".into()),
            },
            RemoteServer {
                id: "range".into(),
                name: "Range".into(),
                url: "http://100.101.2.86:9876/stats/fowlengine/ops".into(),
                key: None,
            },
        ];
        let (list, views) = boxes(&cfg);
        assert_eq!(list, vec![json!({"name": "Campaign", "url": "http://100.101.2.84:9876/stats/fowlengine/ops", "key": "k1"})]);
        let range = views.iter().find(|v| v.id == "range").unwrap();
        assert!(range.problem.as_deref().unwrap().contains("no OPS key"));
        cfg.autofix.servers = Some(vec!["range".into()]);
        let (list, views) = boxes(&cfg);
        assert!(list.is_empty());
        assert!(!views.iter().find(|v| v.id == "campaign").unwrap().watched);
    }
}
