//! The Windows service ("FowlEngine"): this same exe started with `--service`.
//!
//! It keeps DCSServerBot running -- which in turn runs DCS (DCSServerBot's
//! scheduler) and bfdb + the netidx resolver (the FowlEngine plugin's
//! procman) -- from boot:
//!
//!   * starts `run.cmd` in the bot folder, output to logs\bot-console.log --
//!     inside the configured user's desktop session (desktop.rs), because DCS
//!     hangs creating its window in the service's own session 0. After a
//!     reboot Windows signs that user in by itself (automatic sign-in, set up
//!     from Setup), so nobody has to be there.
//!   * restarts it when it exits, backing off 5 s -> 5 min if it keeps dying
//!   * before every start, syncs the Fowl Engine plugin shipped in this app
//!     into the bot folder (backing up what it replaces)
//!   * checks for a newer Fowl Engine Manager and installs it (inside the
//!     update window): the installer stops this service, swaps the files and
//!     starts it again
//!   * obeys commands the GUI drops in commands\ (restart-bot, stop-bot,
//!     start-bot, check-update, install-update)
//!   * writes status.json every few seconds for the GUI
//!
//! Stopping the service stops DCSServerBot only: DCS servers are the bot's
//! children's children and keep running (the bot re-attaches on its next
//! start), and an orphaned bfdb.exe is replaced by procman.
//!
//! `--console` runs the same loop in a terminal, for testing.

use crate::bot;
use crate::config::{self, ManagerConfig};
use crate::desktop;
use crate::update;
use serde::Serialize;
use std::collections::HashSet;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime};

const TICK: Duration = Duration::from_secs(2);
const BOT_LOG_ROTATE_BYTES: u64 = 20 * 1024 * 1024;

#[derive(Debug, Clone, Serialize, Default)]
pub struct BotState {
    pub running: bool,
    pub pid: Option<u32>,
    pub started_at: Option<String>,
    pub uptime_secs: Option<u64>,
    pub starts: u32,
    pub last_exit: Option<String>,
    pub last_exit_at: Option<String>,
    pub paused: bool,
    pub next_start_in_secs: Option<u64>,
    pub problem: Option<String>,
    /// Where it runs: "desktop of ATPAdmin (session 1)", or the service's own
    /// session (no desktop: DCS hangs there).
    pub session: Option<String>,
    /// Running outside a desktop session although desktop mode is on.
    pub no_desktop: bool,
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct AgentStatus {
    pub version: String,
    pub pid: u32,
    pub started_at: String,
    pub heartbeat: String,
    pub bot_dir: Option<String>,
    /// The Windows account the service runs as ("LocalSystem" for SYSTEM).
    pub service_account: String,
    pub bot: BotState,
    pub last_sync: Option<bot::SyncReport>,
    pub update: Option<update::CheckResult>,
    pub update_state: Option<String>,
}

/// The running bot: started by std (in this session) or into a user's
/// desktop session.
enum BotProc {
    Std(Child),
    #[cfg(windows)]
    Desk(desktop::Proc),
}

impl BotProc {
    fn id(&self) -> u32 {
        match self {
            Self::Std(c) => c.id(),
            #[cfg(windows)]
            Self::Desk(p) => p.id(),
        }
    }

    /// Some(exit code) once it has exited.
    fn try_wait(&mut self) -> std::io::Result<Option<Option<i32>>> {
        match self {
            Self::Std(c) => c.try_wait().map(|o| o.map(|s| s.code())),
            #[cfg(windows)]
            Self::Desk(p) => Ok(p.try_wait().map(|c| Some(c as i32))),
        }
    }

    fn kill(&mut self) {
        match self {
            Self::Std(c) => {
                let _ = c.kill();
                let _ = c.wait();
            }
            #[cfg(windows)]
            Self::Desk(p) => {
                p.kill();
                p.wait();
            }
        }
    }
}

pub struct Agent {
    cfg: ManagerConfig,
    cfg_mtime: Option<SystemTime>,
    child: Option<BotProc>,
    /// The account the bot runs as (for diagnoses).
    bot_account: String,
    /// Sessions already locked after an automatic sign-in.
    locked: HashSet<u32>,
    child_started: Option<Instant>,
    status: AgentStatus,
    backoff: Duration,
    next_start: Instant,
    paused: bool,
    last_update_check: Option<Instant>,
}

fn now() -> String {
    chrono::Local::now().to_rfc3339()
}

impl Default for Agent {
    fn default() -> Self {
        Self::new()
    }
}

impl Agent {
    pub fn new() -> Agent {
        let cfg = ManagerConfig::load();
        Agent {
            cfg_mtime: std::fs::metadata(config::config_path()).and_then(|m| m.modified()).ok(),
            cfg,
            child: None,
            bot_account: service_account(),
            locked: HashSet::new(),
            child_started: None,
            status: AgentStatus {
                version: update::current_version().to_string(),
                service_account: service_account(),
                pid: std::process::id(),
                started_at: now(),
                last_sync: bot::last_sync(),
                ..Default::default()
            },
            backoff: Duration::from_secs(5),
            next_start: Instant::now(),
            paused: false,
            // first update check a few minutes after boot, not during it
            last_update_check: Some(Instant::now()),
        }
    }

    fn reload_config(&mut self) {
        let m = std::fs::metadata(config::config_path()).and_then(|m| m.modified()).ok();
        if m != self.cfg_mtime {
            self.cfg_mtime = m;
            self.cfg = ManagerConfig::load();
            log::info!("manager.json changed -- reloaded");
        }
    }

    fn bot_dir(&self) -> Option<PathBuf> {
        self.cfg.bot_dir().filter(|d| bot::is_bot_dir(d))
    }

    fn rotate_bot_log(path: &Path) {
        if std::fs::metadata(path).map(|m| m.len() > BOT_LOG_ROTATE_BYTES).unwrap_or(false) {
            let old = path.with_extension("log.1");
            let _ = std::fs::remove_file(&old);
            let _ = std::fs::rename(path, old);
        }
    }

    fn start_bot(&mut self) {
        let Some(dir) = self.bot_dir() else {
            self.status.bot.problem = Some(match self.cfg.bot_dir() {
                None => "no DCSServerBot folder configured -- open Fowl Engine Manager and run Setup".into(),
                Some(d) => format!("{} is not a DCSServerBot folder", d.display()),
            });
            return;
        };
        // Someone started DCSServerBot by hand (a console window, the old NSSM
        // service): a second copy would fight it over DCS and Discord. Wait
        // for it to go, then take over -- never run two.
        if let Some((pid, name)) = find_external_bot(&dir) {
            let msg = format!("DCSServerBot is already running outside the service ({name}, pid {pid}) -- \
                               close that window (or stop whatever started it); the service takes over within 10 s. \
                               Nothing is started while it runs, so there are never two bots.");
            if self.status.bot.problem.as_deref() != Some(msg.as_str()) {
                log::warn!("{msg}");
            }
            self.status.bot.problem = Some(msg);
            self.next_start = Instant::now() + Duration::from_secs(10);
            return;
        }
        // Where it runs: a signed-in user's desktop, so the DCS servers it
        // starts get real windows. No such session yet (booting, the
        // automatic sign-in still under way, or nobody set it up): wait.
        let session = if cfg!(windows) && self.cfg.desktop_session {
            match desktop::find_session(self.cfg.desktop_user.as_deref()) {
                Some(s) => Some(s),
                None => {
                    let who = self.cfg.desktop_user.as_deref().map(desktop::account_name)
                        .filter(|u| !u.is_empty()).map(String::from);
                    let msg = format!(
                        "Waiting for {} to sign in to Windows. DCS needs a desktop to start in (in the service's own \
                         session it hangs), so the bot starts as soon as {} signed in. Setup -> automatic sign-in makes \
                         Windows do that by itself after every reboot.",
                        who.as_deref().unwrap_or("someone"), if who.is_some() { "they have" } else { "someone has" });
                    if self.status.bot.problem.as_deref() != Some(msg.as_str()) {
                        log::warn!("{msg}");
                    }
                    self.status.bot.problem = Some(msg);
                    self.next_start = Instant::now() + Duration::from_secs(10);
                    return;
                }
            }
        } else {
            None
        };
        if self.cfg.sync_plugin {
            match bot::sync_plugin(&dir) {
                Ok(rep) => {
                    if !rep.changed.is_empty() {
                        log::info!("plugin sync: {} file(s) updated from bundle {} (backup {:?})",
                                   rep.changed.len(), rep.bundle_version, rep.backup);
                    } else if let Some(why) = &rep.skipped_reason {
                        log::info!("plugin sync skipped: {why}");
                    }
                    self.status.last_sync = Some(rep);
                }
                Err(e) => log::error!("plugin sync failed (starting the bot anyway): {e:#}"),
            }
        }
        let log_path = config::logs_dir().join("bot-console.log");
        Self::rotate_bot_log(&log_path);
        let out = File::options().create(true).append(true).open(&log_path);
        let (o, e) = match out {
            Ok(f) => (Stdio::from(f.try_clone().unwrap_or(f)), Stdio::from(
                File::options().create(true).append(true).open(&log_path).expect("bot log"))),
            Err(_) => (Stdio::null(), Stdio::null()),
        };
        let cmdline = self.cfg.bot_command();
        // Name the script by its full path: with NoDefaultCurrentDirectoryInExePath
        // set, cmd.exe won't find "run.cmd" in its own working folder.
        let (first, rest) = cmdline.trim().split_once(' ').unwrap_or((cmdline.trim(), ""));
        let script = dir.join(first.trim_matches('"'));
        let program = if script.is_file() { script.display().to_string() } else { first.trim_matches('"').to_string() };
        #[cfg(windows)]
        if let Some(s) = &session {
            match self.launch_in_session(s, &dir, &program, rest, &log_path) {
                Ok(()) => return,
                Err(e) => {
                    log::error!("could not start DCSServerBot on {}'s desktop: {e:#}", s.user);
                    self.status.bot.problem = Some(format!("{e:#}"));
                    // As LocalSystem the fallback below is no use (it can't
                    // see the user's Python either): try the desktop again.
                    if service_account() == "LocalSystem" {
                        self.schedule_restart();
                        return;
                    }
                }
            }
        }
        #[cfg(not(windows))]
        let _ = &session;
        let mut cmd = Command::new("cmd.exe");
        // DCSServerBot needs a real console: at start it reads and sets the
        // console mode of standard input (utils.quick_edit_mode ->
        // GetConsoleMode), which fails with "The handle is invalid" on a
        // redirected or null stdin. So the bot shares this process's own
        // (hidden) console and gets its input buffer as stdin; its output
        // still goes to the log file. NSSM does the same for the old service.
        let stdin = console_stdin().map(Stdio::from).unwrap_or_else(Stdio::null);
        cmd.current_dir(&dir).stdin(stdin).stdout(o).stderr(e)
            // Output goes to a file, so Python would encode it in the ANSI code
            // page (cp1252) -- and a print() of an emoji then raises
            // UnicodeEncodeError, which is how the radio plugin failed to load.
            // (only stdio: PYTHONUTF8 would also change how the bot reads files)
            .env("PYTHONIOENCODING", "utf-8");
        #[cfg(windows)]
        {
            use std::os::windows::process::CommandExt;
            const CREATE_NEW_PROCESS_GROUP: u32 = 0x0000_0200;
            // no CREATE_NO_WINDOW: that would give it no console at all
            cmd.creation_flags(CREATE_NEW_PROCESS_GROUP);
            // `/s /c ""<script>" args"` is the quoting cmd reliably unwraps.
            cmd.raw_arg(format!("/s /c \"\"{program}\" {rest}\""));
        }
        #[cfg(not(windows))]
        cmd.arg("/c").arg(&cmdline);
        match cmd.spawn() {
            Ok(child) => {
                log::info!("started DCSServerBot: cmd /c {cmdline} in {} (pid {})", dir.display(), child.id());
                self.status.bot.pid = Some(child.id());
                self.status.bot.started_at = Some(now());
                self.status.bot.starts += 1;
                self.bot_account = service_account();
                self.status.bot.no_desktop = self.cfg.desktop_session;
                self.status.bot.session = Some(if self.cfg.desktop_session {
                    "the service's own session -- no desktop, DCS will hang starting".into()
                } else {
                    "the service's own session (desktop mode off)".into()
                });
                // keep the last crash's explanation up until this run proves
                // itself (see watch_bot), so a crash loop doesn't flicker it away
                self.child = Some(BotProc::Std(child));
                self.child_started = Some(Instant::now());
            }
            Err(e) => {
                log::error!("could not start DCSServerBot: {e}");
                self.status.bot.problem = Some(format!("could not start {cmdline}: {e}"));
                self.schedule_restart();
            }
        }
    }

    /// Start the bot as the user signed in to `s`, on their desktop, in its
    /// own (minimized) console: DCSServerBot reads its console mode at start
    /// (GetConsoleMode on stdin), so it gets a real console -- cmd sends what
    /// it prints to bot-console.log for the Logs page and the exit diagnosis.
    #[cfg(windows)]
    fn launch_in_session(&mut self, s: &desktop::Session, dir: &Path, program: &str, rest: &str,
                         log_path: &Path) -> anyhow::Result<()> {
        const SW_SHOWMINNOACTIVE: u16 = 7;
        let sys = std::env::var("SystemRoot").unwrap_or_else(|_| "C:\\Windows".into());
        let line = format!("\"{sys}\\System32\\cmd.exe\" /s /c \"set PYTHONIOENCODING=utf-8&& \"{program}\" {rest} >>\"{}\" 2>&1\"",
                           log_path.display());
        let p = desktop::launch(s.id, &line, dir, "DCSServerBot (Fowl Engine Manager)", SW_SHOWMINNOACTIVE)?;
        let who = if s.domain.is_empty() { s.user.clone() } else { format!("{}\\{}", s.domain, s.user) };
        log::info!("started DCSServerBot on {who}'s desktop (session {}, {}): {program} {rest} in {} (pid {})",
                   s.id, s.state, dir.display(), p.id());
        self.status.bot.pid = Some(p.id());
        self.status.bot.started_at = Some(now());
        self.status.bot.starts += 1;
        self.status.bot.session = Some(format!("desktop of {who} (session {}, {})", s.id, s.state));
        self.status.bot.no_desktop = false;
        self.bot_account = who;
        self.child = Some(BotProc::Desk(p));
        self.child_started = Some(Instant::now());
        // Windows just signed this user in by itself: don't leave an admin
        // desktop open at the console. Everything keeps running when locked.
        let fresh = s.logon_age_secs.map(|a| a < 300).unwrap_or(false);
        if self.cfg.lock_after_autologon && fresh && !self.locked.contains(&s.id) && desktop::autologon_status().enabled {
            self.locked.insert(s.id);
            match desktop::lock_session(s.id) {
                Ok(()) => log::info!("locked session {} after the automatic sign-in", s.id),
                Err(e) => log::warn!("could not lock session {}: {e:#}", s.id),
            }
        }
        Ok(())
    }

    fn schedule_restart(&mut self) {
        self.next_start = Instant::now() + self.backoff;
        self.backoff = (self.backoff * 3).min(Duration::from_secs(300));
    }

    /// Kill the bot -- cmd.exe and its direct children (python), never the
    /// grandchildren (DCS, bfdb, netidx), so the game keeps running.
    fn stop_bot(&mut self, why: &str) {
        let Some(mut child) = self.child.take() else { return };
        let pid = child.id();
        log::info!("stopping DCSServerBot (pid {pid}): {why}");
        // Walk down through the bot's own processes -- cmd.exe, the venv's
        // python.exe launcher, the real interpreter it starts, their conhost --
        // and stop there: DCS, bfdb and netidx (children of the interpreter)
        // are other programs and keep running.
        let mut sys = sysinfo::System::new();
        sys.refresh_processes(sysinfo::ProcessesToUpdate::All, true);
        let is_bot_part = |n: &str| {
            let n = n.to_lowercase();
            n == "cmd.exe" || n == "conhost.exe" || n.starts_with("python") || n.starts_with("py.exe")
        };
        let mut frontier = vec![sysinfo::Pid::from_u32(pid)];
        let mut doomed = Vec::new();
        while let Some(parent) = frontier.pop() {
            for (cpid, p) in sys.processes() {
                if p.parent() == Some(parent) && is_bot_part(&p.name().to_string_lossy()) && !doomed.contains(cpid) {
                    doomed.push(*cpid);
                    frontier.push(*cpid);
                }
            }
        }
        // deepest first, so a launcher doesn't respawn / reap mid-way
        for cpid in doomed.iter().rev() {
            if let Some(p) = sys.process(*cpid) {
                log::info!("  killing {} (pid {cpid})", p.name().to_string_lossy());
                p.kill();
            }
        }
        child.kill();
        self.status.bot.running = false;
        self.status.bot.pid = None;
        self.status.bot.last_exit = Some(format!("stopped: {why}"));
        self.status.bot.last_exit_at = Some(now());
    }

    fn watch_bot(&mut self) {
        let Some(child) = self.child.as_mut() else { return };
        match child.try_wait() {
            Ok(Some(code)) => {
                let ran = self.child_started.map(|t| t.elapsed()).unwrap_or_default();
                let code_s = code.map(|c| c.to_string()).unwrap_or_else(|| "?".into());
                log::warn!("DCSServerBot exited (exit code: {code_s}) after {}s", ran.as_secs());
                // A bot that dies within two minutes didn't crash mid-game --
                // it couldn't start. Say why, from what it printed.
                self.status.bot.problem = if ran < Duration::from_secs(120) {
                    let tail = tail_file(&config::logs_dir().join("bot-console.log"), 40);
                    let why = diagnose_exit(code, ran.as_secs(), &tail, &self.bot_account);
                    log::warn!("  diagnosis: {why}");
                    Some(why)
                } else {
                    None
                };
                self.status.bot.last_exit = Some(format!("exit code: {code_s}"));
                self.status.bot.last_exit_at = Some(now());
                self.child = None;
                if ran > Duration::from_secs(600) {
                    self.backoff = Duration::from_secs(5);
                }
                self.schedule_restart();
            }
            Ok(None) => {
                if self.child_started.map(|t| t.elapsed() > Duration::from_secs(120)).unwrap_or(false) {
                    self.status.bot.problem = None;
                }
                self.watch_for_pause()
            }
            Err(e) => log::warn!("could not poll DCSServerBot: {e}"),
        }
    }

    /// With a real console, run.cmd's `pause` ("Please check the logs and
    /// press any key") after a failed start waits for a key nobody will press.
    /// Spot it -- that prompt is the last thing in the log and the log has gone
    /// quiet -- and treat it as the exit it is.
    fn watch_for_pause(&mut self) {
        let path = config::logs_dir().join("bot-console.log");
        let quiet = std::fs::metadata(&path)
            .and_then(|m| m.modified())
            .ok()
            .and_then(|t| t.elapsed().ok())
            .map(|d| d >= Duration::from_secs(15))
            .unwrap_or(false);
        if !quiet {
            return;
        }
        let tail = tail_file(&path, 6);
        let waiting = tail
            .iter()
            .rev()
            .map(|l| l.trim())
            .find(|l| !l.is_empty())
            .map(|l| l.to_lowercase().contains("press any key"))
            .unwrap_or(false);
        if !waiting {
            return;
        }
        let ran = self.child_started.map(|t| t.elapsed().as_secs()).unwrap_or(0);
        let tail = tail_file(&path, 40);
        let why = diagnose_exit(None, ran, &tail, &self.bot_account);
        self.stop_bot("it stopped and is waiting for a key press");
        log::warn!("  diagnosis: {why}");
        self.status.bot.problem = Some(why);
        self.schedule_restart();
    }

    fn handle_commands(&mut self) {
        let Ok(rd) = std::fs::read_dir(config::commands_dir()) else { return };
        for e in rd.flatten() {
            let name = e.file_name().to_string_lossy().to_string();
            let _ = std::fs::remove_file(e.path());
            log::info!("command from the GUI: {name}");
            match name.as_str() {
                "restart-bot" => {
                    self.stop_bot("restart requested");
                    self.paused = false;
                    self.backoff = Duration::from_secs(5);
                    self.next_start = Instant::now();
                }
                "stop-bot" => {
                    self.stop_bot("stop requested");
                    self.paused = true;
                }
                "start-bot" => {
                    self.paused = false;
                    self.backoff = Duration::from_secs(5);
                    self.next_start = Instant::now();
                }
                "check-update" => self.check_update(false),
                "install-update" => self.check_update(true),
                other => log::warn!("unknown command {other}"),
            }
        }
    }

    fn check_update(&mut self, force_install: bool) {
        self.last_update_check = Some(Instant::now());
        let res = update::check(&self.cfg);
        if let Some(e) = &res.error {
            log::warn!("update check failed: {e}");
        }
        let install = res.update_available
            && (force_install
                || (self.cfg.auto_update
                    && config::in_window(self.cfg.update_window.as_deref(), chrono::Local::now().time())));
        let latest = res.latest.clone();
        self.status.update = Some(res);
        if !install {
            return;
        }
        let Some(rel) = latest else { return };
        self.status.update_state = Some(format!("downloading {}", rel.version));
        self.write_status();
        match update::download(&self.cfg, &rel) {
            Ok(path) => {
                log::warn!("installing Fowl Engine Manager {} -- the installer restarts this service", rel.version);
                self.status.update_state = Some(format!("installing {}", rel.version));
                self.write_status();
                if let Err(e) = update::launch_installer(&path, true) {
                    log::error!("could not start the installer: {e:#}");
                    self.status.update_state = Some(format!("install failed: {e}"));
                }
            }
            Err(e) => {
                log::error!("update {} rejected: {e:#}", rel.version);
                self.status.update_state = Some(format!("update {} rejected: {e}", rel.version));
            }
        }
    }

    fn write_status(&mut self) {
        let s = &mut self.status;
        s.heartbeat = now();
        s.bot_dir = self.cfg.bot_dir.clone();
        s.bot.running = self.child.is_some();
        s.bot.paused = self.paused;
        s.bot.uptime_secs = self.child_started.filter(|_| self.child.is_some()).map(|t| t.elapsed().as_secs());
        s.bot.next_start_in_secs = (self.child.is_none() && !self.paused)
            .then(|| self.next_start.saturating_duration_since(Instant::now()).as_secs());
        if let Ok(b) = serde_json::to_vec_pretty(&self.status) {
            let tmp = config::status_path().with_extension("json.tmp");
            if std::fs::write(&tmp, b).is_ok() {
                let _ = std::fs::rename(&tmp, config::status_path());
            }
        }
    }

    /// One pass of the loop. Returns quickly.
    pub fn tick(&mut self) {
        self.reload_config();
        self.handle_commands();
        self.watch_bot();
        if self.child.is_none() && !self.paused && Instant::now() >= self.next_start {
            self.start_bot();
            if self.child.is_none() && self.status.bot.problem.is_some() && self.bot_dir().is_none() {
                // not configured: check again in a bit, quietly
                self.next_start = Instant::now() + Duration::from_secs(30);
            }
        }
        let due = self
            .last_update_check
            .map(|t| t.elapsed() >= Duration::from_secs_f64(self.cfg.check_hours.max(0.25) * 3600.0))
            .unwrap_or(true);
        // the very first check: 5 minutes after the service came up
        let first_due = self.status.update.is_none()
            && self.last_update_check.map(|t| t.elapsed() >= Duration::from_secs(300)).unwrap_or(true);
        if due || first_due {
            self.check_update(false);
        }
        self.write_status();
    }

    pub fn shutdown(&mut self) {
        self.stop_bot("service stopping");
        self.status.bot.problem = Some("service stopped".into());
        self.write_status();
    }
}

/// The account this process runs as, as an admin would name it.
pub fn service_account() -> String {
    let user = std::env::var("USERNAME").unwrap_or_default();
    if user.is_empty() || user.ends_with('$') {
        "LocalSystem".into()
    } else {
        match std::env::var("USERDOMAIN") {
            Ok(d) if !d.is_empty() => format!("{d}\\{user}"),
            _ => user,
        }
    }
}

/// This process's console input, for the bot's stdin -- allocating a hidden
/// console first if there is none (a service, or the GUI-subsystem exe).
#[cfg(windows)]
fn console_stdin() -> Option<File> {
    use windows_sys::Win32::System::Console::{AllocConsole, GetConsoleWindow};
    use windows_sys::Win32::UI::WindowsAndMessaging::{ShowWindow, SW_HIDE};
    unsafe {
        if GetConsoleWindow().is_null() && AllocConsole() != 0 {
            let w = GetConsoleWindow();
            if !w.is_null() {
                ShowWindow(w, SW_HIDE);
            }
        }
    }
    File::options().read(true).write(true).open("CONIN$").ok()
}

#[cfg(not(windows))]
fn console_stdin() -> Option<File> {
    None
}

/// A DCSServerBot (`python ... run.py`) already running from `bot_dir` that
/// this service didn't start. Only called while the service has no bot of
/// its own, so any match is somebody else's.
fn find_external_bot(bot_dir: &Path) -> Option<(u32, String)> {
    let want = std::fs::canonicalize(bot_dir).unwrap_or_else(|_| bot_dir.to_path_buf());
    let same_dir = |p: &Path| {
        std::fs::canonicalize(p).map(|c| c == want).unwrap_or(false)
    };
    let mut sys = sysinfo::System::new();
    sys.refresh_processes_specifics(
        sysinfo::ProcessesToUpdate::All,
        true,
        sysinfo::ProcessRefreshKind::nothing().with_cmd(sysinfo::UpdateKind::Always)
            .with_cwd(sysinfo::UpdateKind::Always),
    );
    // A venv's python.exe is a launcher with the real interpreter as its
    // child, so both match: report the lowest pid (the launcher) every time.
    let mut procs: Vec<_> = sys.processes().iter().collect();
    procs.sort_by_key(|(pid, _)| pid.as_u32());
    for (pid, p) in procs {
        let name = p.name().to_string_lossy().to_lowercase();
        if !name.starts_with("python") {
            continue;
        }
        let args: Vec<String> = p.cmd().iter().map(|a| a.to_string_lossy().to_string()).collect();
        let Some(script) = args.iter().find(|a| a.to_lowercase().ends_with("run.py")) else { continue };
        let script_path = Path::new(script);
        let hit = if script_path.is_absolute() {
            script_path.parent().map(same_dir).unwrap_or(false)
        } else {
            p.cwd().map(same_dir).unwrap_or(false)
        };
        if hit {
            return Some((pid.as_u32(), p.name().to_string_lossy().to_string()));
        }
    }
    None
}

fn tail_file(path: &Path, n: usize) -> Vec<String> {
    let data = std::fs::read(path).unwrap_or_default();
    let start = data.len().saturating_sub(64 * 1024);
    let text = String::from_utf8_lossy(&data[start..]);
    let lines: Vec<&str> = text.lines().collect();
    lines[lines.len().saturating_sub(n)..].iter().map(|s| s.to_string()).collect()
}

/// Why DCSServerBot stopped right after starting, in words an admin can act
/// on. `tail` is the end of its console output.
pub fn diagnose_exit(code: Option<i32>, ran_secs: u64, tail: &[String], account: &str) -> String {
    let text = tail.join("\n");
    let has = |s: &str| text.to_lowercase().contains(&s.to_lowercase());
    let system = account.eq_ignore_ascii_case("LocalSystem");
    let fix_account = "In Fowl Engine Manager: Setup -> step 3, pick the Windows user that installed DCSServerBot \
                       and runs DCS (the one whose profile has the .dcssb folder) and make sure they are signed in \
                       (step 4 signs them in automatically) -- the bot then runs on their desktop, as them.";
    if has("python.exe was not found") || (code == Some(9009) && !has("is not recognized")) {
        return if system {
            format!("DCSServerBot can't find Python: it ran as LocalSystem, which doesn't see the Python \
                     installed for your user (exit 9009). LocalSystem would also run DCS under the wrong profile. {fix_account}")
        } else {
            format!("DCSServerBot can't find Python for {account} (exit 9009): Python isn't on that account's PATH. \
                     Install Python 3.11+ for all users / add it to PATH, or run the service as the user that installed DCSServerBot.")
        };
    }
    if has("requires Python >= 3.11") {
        return format!("DCSServerBot needs Python 3.11 or newer; the one {account} finds is older.");
    }
    if has("is not recognized as an internal or external command") {
        let line = tail.iter().rev().find(|l| l.contains("is not recognized")).cloned().unwrap_or_default();
        return format!("The start command couldn't be run: {} -- check Settings -> Start command.", line.trim());
    }
    if has("Process already running for node") {
        return "Another DCSServerBot is already running for this node (probably started by hand in a console \
                window). Close it -- the service then starts its own within a few seconds."
            .into();
    }
    if has("Access is denied") || has("PermissionError") {
        return format!("DCSServerBot was denied access to a file or folder as {account}. {}",
                       if system { fix_account } else { "Check that account owns the DCSServerBot and Saved Games folders." });
    }
    // run.cmd's own closing lines ("Unexpected return code: 1", "Please check
    // the logs and press any key") say nothing; the error above them does.
    let boilerplate = |l: &str| {
        let l = l.to_lowercase();
        l.is_empty() || l.contains("press any key") || l.contains("unexpected return code")
    };
    let rc = tail.iter().rev().find_map(|l| l.trim().strip_prefix("Unexpected return code:").map(|c| c.trim().to_string()));
    let error_line = tail.iter().rev().map(|l| l.trim())
        .find(|l| !boilerplate(l) && (l.contains("ERROR") || l.contains("Error") || l.contains("Exception")))
        .or_else(|| tail.iter().rev().map(|l| l.trim()).find(|l| !boilerplate(l)))
        .unwrap_or("(no output)");
    let code = rc.or_else(|| code.map(|c| c.to_string())).unwrap_or_else(|| "?".into());
    let mut msg = format!("DCSServerBot stopped {ran_secs}s after starting (exit {code}). {error_line} \
                           -- full output: Logs -> DCSServerBot console, and DCSServerBot's own logs\\dcssb-<node>.log");
    if system {
        msg.push_str(&format!(" -- note the service runs as LocalSystem. {fix_account}"));
    }
    msg
}

fn init_logging() {
    let _ = config::ensure_dirs();
    let path = config::logs_dir().join("agent.log");
    if std::fs::metadata(&path).map(|m| m.len() > 10 * 1024 * 1024).unwrap_or(false) {
        let _ = std::fs::rename(&path, path.with_extension("log.1"));
    }
    if let Ok(f) = File::options().create(true).append(true).open(&path) {
        let cfg = simplelog::ConfigBuilder::new().set_time_format_rfc3339().build();
        let _ = simplelog::WriteLogger::init(log::LevelFilter::Info, cfg, f);
    }
}

/// Lock down %ProgramData%\FowlEngine: it holds the GitHub token and is what
/// the service acts on, so only admins, SYSTEM and the service's own account.
fn restrict_data_dir() {
    #[cfg(windows)]
    {
        let dir = config::data_dir();
        let user = std::env::var("USERNAME").unwrap_or_default();
        let mut args = vec![
            dir.display().to_string(),
            "/inheritance:r".into(),
            "/grant:r".into(), "*S-1-5-32-544:(OI)(CI)F".into(), // Administrators
            "/grant:r".into(), "*S-1-5-18:(OI)(CI)F".into(),     // SYSTEM
        ];
        if !user.is_empty() && !user.ends_with('$') {
            args.push("/grant:r".into());
            args.push(format!("{user}:(OI)(CI)F"));
        }
        // the desktop user the bot runs as writes bot-console.log
        if let Some(u) = ManagerConfig::load().desktop_user.as_deref().map(desktop::account_name).filter(|u| !u.is_empty()) {
            args.push("/grant:r".into());
            args.push(format!("{u}:(OI)(CI)M"));
        }
        use std::os::windows::process::CommandExt;
        let _ = Command::new("icacls").args(&args).creation_flags(0x0800_0000)
            .stdout(Stdio::null()).stderr(Stdio::null()).status();
    }
}

pub fn run_loop(stop: Arc<AtomicBool>) {
    let mut agent = Agent::new();
    log::info!("Fowl Engine Manager {} service loop starting (pid {})", agent.status.version, std::process::id());
    while !stop.load(Ordering::SeqCst) {
        agent.tick();
        let t = Instant::now();
        while t.elapsed() < TICK && !stop.load(Ordering::SeqCst) {
            std::thread::sleep(Duration::from_millis(200));
        }
    }
    agent.shutdown();
    log::info!("service loop stopped");
}

/// `--console`: the service loop in a terminal (Ctrl-C to stop).
pub fn run_console() {
    init_logging();
    let stop = Arc::new(AtomicBool::new(false));
    eprintln!("Fowl Engine Manager agent (console mode). Logging to {}", config::logs_dir().display());
    run_loop(stop);
}

// ---- Windows service plumbing ---------------------------------------------------

#[cfg(windows)]
mod service {
    use super::*;
    use windows_service::service::{
        ServiceControl, ServiceControlAccept, ServiceExitCode, ServiceState, ServiceStatus, ServiceType,
    };
    use windows_service::service_control_handler::{self, ServiceControlHandlerResult};
    use windows_service::{define_windows_service, service_dispatcher};

    define_windows_service!(ffi_service_main, service_main);

    pub fn run() -> windows_service::Result<()> {
        service_dispatcher::start(config::SERVICE_NAME, ffi_service_main)
    }

    fn service_main(_args: Vec<std::ffi::OsString>) {
        init_logging();
        restrict_data_dir();
        let stop = Arc::new(AtomicBool::new(false));
        let stop2 = stop.clone();
        let handler = move |ctl| match ctl {
            ServiceControl::Stop | ServiceControl::Shutdown | ServiceControl::Preshutdown => {
                stop2.store(true, Ordering::SeqCst);
                ServiceControlHandlerResult::NoError
            }
            ServiceControl::Interrogate => ServiceControlHandlerResult::NoError,
            _ => ServiceControlHandlerResult::NotImplemented,
        };
        let handle = match service_control_handler::register(config::SERVICE_NAME, handler) {
            Ok(h) => h,
            Err(e) => {
                log::error!("could not register the service control handler: {e}");
                return;
            }
        };
        let set = |state, accept, wait: Duration| {
            let _ = handle.set_service_status(ServiceStatus {
                service_type: ServiceType::OWN_PROCESS,
                current_state: state,
                controls_accepted: accept,
                exit_code: ServiceExitCode::Win32(0),
                checkpoint: 0,
                wait_hint: wait,
                process_id: None,
            });
        };
        set(ServiceState::Running, ServiceControlAccept::STOP | ServiceControlAccept::SHUTDOWN, Duration::default());
        run_loop(stop);
        set(ServiceState::Stopped, ServiceControlAccept::empty(), Duration::default());
    }
}

/// `--service`: called by the SCM.
pub fn run_service() {
    #[cfg(windows)]
    if let Err(e) = service::run() {
        init_logging();
        log::error!("not started by the service manager ({e}) -- use --console to run the loop by hand");
        eprintln!("This mode is for the Windows service. To try the loop by hand: --console");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn t(lines: &[&str]) -> Vec<String> {
        lines.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn python_missing_under_localsystem() {
        let tail = t(&["", "***  ERROR  ***", "python.exe was not found in your PATH."]);
        let d = diagnose_exit(Some(9009), 1, &tail, "LocalSystem");
        assert!(d.contains("LocalSystem") && d.contains("step 3"), "{d}");
        let d = diagnose_exit(Some(9009), 1, &[], "LocalSystem");
        assert!(d.contains("can't find Python"), "{d}");
    }

    #[test]
    fn python_missing_for_a_user() {
        let d = diagnose_exit(Some(9009), 1, &t(&["python.exe was not found in your PATH."]), r"BOX\atp");
        assert!(d.contains(r"BOX\atp") && !d.contains("LocalSystem"), "{d}");
    }

    #[test]
    fn bad_start_command_and_fallback() {
        let d = diagnose_exit(Some(9009), 0, &t(&["'run.cmd' is not recognized as an internal or external command,"]), "u");
        assert!(d.contains("Start command"), "{d}");
        let d = diagnose_exit(Some(1), 12, &t(&["Traceback ...", "KeyError: 'token'", ""]), "u");
        assert!(d.contains("KeyError: 'token'") && d.contains("12s"), "{d}");
    }
}

#[cfg(test)]
mod more_tests {
    use super::*;

    fn t(lines: &[&str]) -> Vec<String> {
        lines.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn run_cmd_boilerplate_is_skipped_for_the_real_error() {
        let tail = t(&[
            "2026-09-25 22:28:10.123 ERROR\tPort 9876 is already in use",
            "Unexpected return code: 1",
            "Please check the logs and press any key to continue...",
        ]);
        let d = diagnose_exit(Some(0), 2, &tail, r"DESKTOP\ATPAdmin");
        assert!(d.contains("exit 1") && d.contains("Port 9876"), "{d}");
    }

    #[test]
    fn second_instance() {
        let d = diagnose_exit(Some(0), 1, &t(&["Process already running for node NODE!"]), "u");
        assert!(d.contains("already running"), "{d}");
    }
}
