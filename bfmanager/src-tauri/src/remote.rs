//! Remote backups: the bridge from this box's OPS API (the fowlengine plugin,
//! running in DCSServerBot as the desktop user) to this app's service, which
//! runs the whole-box backup job (backup.rs) as LocalSystem.
//!
//! Off unless manager.json `remote_backups` is true. Then the service keeps
//!
//!   remote\backup-status.json   rewritten every tick: the job, the backups
//!                               found, the last requests handled
//!   remote\requests\*.json      the plugin drops {id, kind, password?, zip?}
//!
//! and the desktop user the bot runs as gets Modify on remote\ only (the
//! rest of the data folder stays Administrators + SYSTEM, see
//! agent::restrict_data_dir). What a request can do is narrow on purpose:
//! a backup exactly as this box's own plan has it (default folder, default
//! roots, the database, no extra paths, nothing stopped -- a shadow copy
//! reads files in use), a check of a zip the service itself found, or a
//! cancel. A restore overwrites the box, so it is never taken from here.

use crate::backup::{self, BackupOptions, BackupPlan, FoundBackup};
use crate::config::{self, ManagerConfig};
use anyhow::{bail, Result};
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

pub fn remote_dir() -> PathBuf {
    config::data_dir().join("remote")
}
const STATUS_FILE: &str = "backup-status.json";

/// A request older than this (the service was down when it came) is dropped.
const MAX_AGE: Duration = Duration::from_secs(600);
const KEEP_HANDLED: usize = 10;

#[derive(Debug, Deserialize)]
pub struct Request {
    pub id: String,
    pub kind: String,
    #[serde(default)]
    pub password: Option<String>,
    #[serde(default)]
    pub zip: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Handled {
    pub id: String,
    pub kind: String,
    pub at: String,
    pub accepted: bool,
    pub message: String,
}

#[derive(Serialize)]
struct Status<'a> {
    enabled: bool,
    updated: String,
    updated_unix: f64,
    hostname: String,
    manager_version: String,
    default_dest: Option<&'a str>,
    job: Option<backup::Job>,
    backups: &'a [FoundBackup],
    handled: Vec<Handled>,
    problem: Option<&'a str>,
}

/// The backup a remote request gets: this box's own plan, as the BACKUP tab
/// would start it with nothing changed -- except that nothing is stopped
/// (the bot answering is how the controller watches), so files in use come
/// from a shadow copy.
pub fn remote_backup_options(plan: &BackupPlan, password: Option<String>) -> BackupOptions {
    BackupOptions {
        dest_dir: plan.default_dest.clone(),
        password: password.filter(|p| !p.is_empty()),
        roots: plan.roots.iter().filter(|r| r.include).map(|r| r.id.clone()).collect(),
        extra_paths: vec![],
        database: plan.database.as_ref().map(|d| d.problem.is_none() && d.pg_dump.is_some()).unwrap_or(false),
        stop_bot: false,
        shadow_copy: true,
    }
}

/// Only a zip the service found itself may be checked: a request can't
/// name any other file on the box.
pub fn verify_target(zip: &str, found: &[FoundBackup]) -> Result<String> {
    let want = zip.trim().replace('/', "\\").to_lowercase();
    found
        .iter()
        .find(|f| f.path.replace('/', "\\").to_lowercase() == want)
        .map(|f| f.path.clone())
        .ok_or_else(|| anyhow::anyhow!("{zip} is not one of the backups this box lists -- refresh and pick one"))
}

/// When the request was made, from its id (`<unix nanos>-<kind>`).
fn request_age(id: &str) -> Option<Duration> {
    let ns: u128 = id.split('-').next()?.parse().ok()?;
    let now = SystemTime::now().duration_since(UNIX_EPOCH).ok()?.as_nanos();
    Some(Duration::from_nanos(now.saturating_sub(ns).min(u64::MAX as u128) as u64))
}

pub struct Remote {
    /// remote\ (a temp folder in tests)
    dir: PathBuf,
    /// The folder exists and requests are taken.
    on: bool,
    granted_to: Option<String>,
    backups: Arc<Mutex<Vec<FoundBackup>>>,
    scanned: Option<Instant>,
    job_was_running: bool,
    handled: Arc<Mutex<VecDeque<Handled>>>,
    default_dest: Option<String>,
    problem: Option<String>,
}

impl Default for Remote {
    fn default() -> Self {
        Self::new()
    }
}

impl Remote {
    pub fn new() -> Remote {
        Self::in_dir(remote_dir())
    }

    pub fn in_dir(dir: PathBuf) -> Remote {
        Remote {
            dir,
            // so a first tick with the setting off clears a folder left by
            // an earlier run
            on: true,
            granted_to: None,
            backups: Arc::default(),
            scanned: None,
            job_was_running: false,
            handled: Arc::default(),
            default_dest: None,
            problem: None,
        }
    }

    fn requests_dir(&self) -> PathBuf {
        self.dir.join("requests")
    }

    fn status_path(&self) -> PathBuf {
        self.dir.join(STATUS_FILE)
    }

    /// One pass from the service loop. Never blocks on a backup: the plan
    /// and the job run on their own threads.
    pub fn tick(&mut self, cfg: &ManagerConfig) {
        if !cfg.remote_backups {
            if self.on {
                // the plugin then says "not turned on" instead of taking requests
                let _ = std::fs::remove_dir_all(&self.dir);
                self.on = false;
                self.granted_to = None;
            }
            return;
        }
        let req_dir = self.requests_dir();
        if !self.on || !req_dir.is_dir() {
            if let Err(e) = std::fs::create_dir_all(&req_dir) {
                log::warn!("remote backups: can't create {}: {e}", req_dir.display());
                return;
            }
            self.on = true;
            self.granted_to = None;
            log::info!("remote backups: on -- taking requests in {}", req_dir.display());
        }
        self.grant(cfg);
        self.handle_requests();
        let running = backup::job_status().map(|j| j.running).unwrap_or(false);
        let due = self.scanned.map(|t| t.elapsed() >= Duration::from_secs(60)).unwrap_or(true);
        if due || (self.job_was_running && !running) {
            self.scanned = Some(Instant::now());
            let found = backup::find_backups();
            if let Ok(mut b) = self.backups.lock() {
                *b = found;
            }
        }
        self.job_was_running = running;
        self.write_status();
    }

    /// The desktop user the bot runs as may write requests and read the
    /// status -- remote\ only.
    fn grant(&mut self, cfg: &ManagerConfig) {
        let user = cfg
            .desktop_user
            .as_deref()
            .map(crate::desktop::account_name)
            .filter(|u| !u.is_empty())
            .map(String::from);
        let Some(user) = user else {
            self.problem = Some(
                "no desktop user is set in this box's Manager (SETTINGS), so the bot's account can't be given \
                 access to the remote folder"
                    .into(),
            );
            return;
        };
        if self.granted_to.as_deref() == Some(user.as_str()) {
            return;
        }
        #[cfg(windows)]
        {
            use std::os::windows::process::CommandExt;
            let ok = std::process::Command::new("icacls")
                .args([self.dir.display().to_string(), "/grant:r".into(), format!("{user}:(OI)(CI)M")])
                .creation_flags(0x0800_0000)
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .status()
                .map(|s| s.success())
                .unwrap_or(false);
            if !ok {
                self.problem = Some(format!("could not give {user} access to {}", self.dir.display()));
                return;
            }
        }
        self.problem = None;
        self.granted_to = Some(user);
    }

    fn record(&self, id: &str, kind: &str, accepted: bool, message: String) {
        if accepted {
            log::info!("remote backups: {kind} request {id}: {message}");
        } else {
            log::warn!("remote backups: {kind} request {id} refused: {message}");
        }
        record_into(&self.handled, id, kind, accepted, message);
    }

    fn handle_requests(&mut self) {
        let Ok(rd) = std::fs::read_dir(self.requests_dir()) else { return };
        let mut files: Vec<PathBuf> = rd
            .flatten()
            .map(|e| e.path())
            .filter(|p| p.extension().map(|x| x == "json").unwrap_or(false))
            .collect();
        files.sort();
        for path in files.into_iter().take(5) {
            let text = std::fs::read_to_string(&path).unwrap_or_default();
            // gone before it is acted on: a password in it never lingers
            let _ = std::fs::remove_file(&path);
            let req: Request = match serde_json::from_str(&text) {
                Ok(r) => r,
                Err(e) => {
                    let name = path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default();
                    self.record(&name, "?", false, format!("not a valid request: {e}"));
                    continue;
                }
            };
            if request_age(&req.id).map(|a| a > MAX_AGE).unwrap_or(true) {
                self.record(&req.id, &req.kind, false, "too old (the service wasn't running when it came) -- ask again".into());
                continue;
            }
            match self.handle(&req) {
                Ok(msg) => self.record(&req.id, &req.kind, true, msg),
                Err(e) => self.record(&req.id, &req.kind, false, format!("{e:#}")),
            }
        }
    }

    fn handle(&mut self, req: &Request) -> Result<String> {
        let busy = backup::job_status().filter(|j| j.running).map(|j| j.kind);
        match req.kind.as_str() {
            "backup" => {
                if let Some(k) = busy {
                    bail!("a {k} is already running on this box");
                }
                // the plan reads the bot's config and sizes every folder:
                // seconds, so not on the service loop
                let pw = req.password.clone();
                let (id, handled) = (req.id.clone(), self.handled.clone());
                std::thread::spawn(move || {
                    let r = backup::backup_plan().and_then(|plan| {
                        let opts = remote_backup_options(&plan, pw);
                        let dest = opts.dest_dir.clone();
                        backup::start_backup(opts).map(|_| dest)
                    });
                    if let Err(e) = &r {
                        log::warn!("remote backups: backup request {id} failed to start: {e:#}");
                        record_into(&handled, &id, "backup", false, format!("could not start: {e:#}"));
                    }
                });
                Ok("starting a backup of this box (its own plan, into its usual folder)".into())
            }
            "verify" => {
                let found = self.backups.lock().map(|b| b.clone()).unwrap_or_default();
                let zip = verify_target(req.zip.as_deref().unwrap_or(""), &found)?;
                if let Some(k) = busy {
                    bail!("a {k} is already running on this box");
                }
                backup::start_verify(zip.clone(), req.password.clone().filter(|p| !p.is_empty()))?;
                Ok(format!("checking {zip}"))
            }
            "cancel" => {
                if busy.is_none() {
                    bail!("nothing is running");
                }
                backup::cancel_job();
                Ok("cancelling".into())
            }
            "restore" => bail!("a restore is only ever started on the box itself"),
            other => bail!("unknown request {other:?}"),
        }
    }

    fn write_status(&mut self) {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_secs_f64()).unwrap_or(0.0);
        if self.default_dest.is_none() {
            self.default_dest = Some(backup::default_dest());
        }
        let backups = self.backups.lock().map(|b| b.clone()).unwrap_or_default();
        let handled = self.handled.lock().map(|h| h.iter().cloned().collect()).unwrap_or_default();
        let st = Status {
            enabled: true,
            updated: chrono::Local::now().to_rfc3339(),
            updated_unix: now,
            hostname: std::env::var("COMPUTERNAME").unwrap_or_default(),
            manager_version: crate::update::current_version().to_string(),
            default_dest: self.default_dest.as_deref(),
            job: backup::job_status(),
            backups: &backups,
            handled,
            problem: self.problem.as_deref(),
        };
        let Ok(b) = serde_json::to_vec_pretty(&st) else { return };
        let tmp = self.status_path().with_extension("json.tmp");
        if std::fs::write(&tmp, b).is_ok() {
            let _ = std::fs::rename(&tmp, self.status_path());
        }
    }
}

fn record_into(handled: &Mutex<VecDeque<Handled>>, id: &str, kind: &str, accepted: bool, message: String) {
    if let Ok(mut h) = handled.lock() {
        h.push_front(Handled {
            id: id.into(),
            kind: kind.into(),
            at: chrono::Local::now().to_rfc3339(),
            accepted,
            message,
        });
        h.truncate(KEEP_HANDLED);
    }
}

/// What the remote status file says now (for this box's own BACKUP tab, so
/// it can tell a remote job is running).
pub fn read_status() -> Option<serde_json::Value> {
    read_status_in(&remote_dir())
}

fn read_status_in(dir: &std::path::Path) -> Option<serde_json::Value> {
    std::fs::read_to_string(dir.join(STATUS_FILE)).ok().and_then(|s| serde_json::from_str(&s).ok())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backup::{DbPlan, Kind, Root};

    fn root(id: &str, include: bool) -> Root {
        Root {
            id: id.into(),
            kind: Kind::Bot,
            label: id.into(),
            path: format!("C:\\{id}"),
            files: 0,
            bytes: 0,
            include,
            note: None,
            only: vec![],
            expected_files: 0,
            skipped: vec![],
            vanished: 0,
        }
    }

    fn plan(db_problem: Option<&str>) -> BackupPlan {
        BackupPlan {
            roots: vec![root("bot", true), root("instance-a", true), root("extra-big", false)],
            database: Some(DbPlan {
                host: "127.0.0.1".into(),
                port: 5432,
                name: "dcsserverbot".into(),
                user: "u".into(),
                pg_dump: Some("pg_dump.exe".into()),
                problem: db_problem.map(String::from),
            }),
            programs: vec![],
            default_dest: "D:\\FowlEngineBackups".into(),
            hostname: "RANGE-PC".into(),
            bot_running: true,
            service_running: true,
            bfdb_running: true,
            dcs_running: true,
            warnings: vec![],
        }
    }

    #[test]
    fn a_remote_backup_is_the_boxs_own_plan_and_stops_nothing() {
        let o = remote_backup_options(&plan(None), Some("long password".into()));
        assert_eq!(o.dest_dir, "D:\\FowlEngineBackups");
        assert_eq!(o.roots, vec!["bot".to_string(), "instance-a".to_string()]);
        assert!(o.extra_paths.is_empty() && o.database && !o.stop_bot && o.shadow_copy);
        assert_eq!(o.password.as_deref(), Some("long password"));
        let o = remote_backup_options(&plan(Some("no pg_dump")), Some(String::new()));
        assert!(!o.database && o.password.is_none());
    }

    #[test]
    fn verify_only_takes_a_listed_zip() {
        let found = vec![FoundBackup { path: "D:\\FowlEngineBackups\\FowlEngine-backup-A.zip".into(), bytes: 1, modified: None }];
        assert_eq!(
            verify_target("d:/fowlenginebackups/FowlEngine-backup-a.zip", &found).unwrap(),
            "D:\\FowlEngineBackups\\FowlEngine-backup-A.zip"
        );
        assert!(verify_target("C:\\Windows\\System32\\config\\SAM", &found).is_err());
        assert!(verify_target("", &found).is_err());
    }

    #[test]
    fn request_ages() {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        assert!(request_age(&format!("{now}-backup")).unwrap() < Duration::from_secs(5));
        let old = now - 3_600_000_000_000;
        assert!(request_age(&format!("{old}-backup")).unwrap() > MAX_AGE);
        assert!(request_age("garbage").is_none());
    }

    #[test]
    fn requests_are_handled_once_and_restore_is_refused() {
        let dir = std::env::temp_dir().join(format!("fowl-remote-{}", std::process::id())).join("remote");
        let _ = std::fs::remove_dir_all(&dir);
        let mut cfg = ManagerConfig::default();
        let mut r = Remote::in_dir(dir.clone());
        // off: nothing there, a folder from before is cleared
        std::fs::create_dir_all(r.requests_dir()).unwrap();
        r.tick(&cfg);
        assert!(!dir.exists());
        cfg.remote_backups = true;
        cfg.desktop_user = Some(".\\nobody-here".into());
        r.tick(&cfg);
        assert!(r.requests_dir().is_dir() && r.status_path().is_file());
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        let put = |kind: &str, extra: &str| {
            let id = format!("{now}-{kind}");
            let body = format!(r#"{{"id":"{id}","kind":"{kind}"{extra}}}"#);
            std::fs::write(dir.join("requests").join(format!("{id}.json")), body).unwrap();
        };
        put("restore", "");
        put("verify", r#","zip":"C:/Windows/win.ini""#);
        // (no cancel here: the job slot is shared with backup.rs's tests)
        let stale = now - 3_600_000_000_000;
        let body = format!(r#"{{"id":"{stale}-backup","kind":"backup"}}"#);
        std::fs::write(dir.join("requests").join(format!("{stale}-backup.json")), body).unwrap();
        r.tick(&cfg);
        assert_eq!(std::fs::read_dir(r.requests_dir()).unwrap().count(), 0, "every request file is removed");
        let st = read_status_in(&dir).unwrap();
        let handled = st["handled"].as_array().unwrap();
        assert_eq!(handled.len(), 3);
        assert!(handled.iter().all(|h| h["accepted"] == false), "{handled:?}");
        let said = |what: &str| handled.iter().any(|h| h["message"].as_str().unwrap().contains(what));
        assert!(said("only ever started on the box") && said("not one of the backups") && said("too old"));
        assert!(st["enabled"] == true && st["updated_unix"].as_f64().unwrap() > 0.0);
        cfg.remote_backups = false;
        r.tick(&cfg);
        assert!(!dir.exists());
        let _ = std::fs::remove_dir_all(dir.parent().unwrap());
    }
}
