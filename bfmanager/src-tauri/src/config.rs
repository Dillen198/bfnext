//! Where the manager keeps its state, and its own small config file.
//!
//! Everything lives under %ProgramData%\FowlEngine so the GUI (an admin at the
//! desktop) and the Windows service (the bot's account, no desktop) see the
//! same files:
//!
//!   manager.json      this config
//!   status.json       what the service is doing, rewritten every few seconds
//!   commands\         the GUI drops a file here to ask the service to act
//!   logs\             agent.log (the service), bot-console.log (DCSServerBot's stdout)
//!   updates\          downloaded manager installers
//!   backups\          the bot plugin as it was before each sync

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

pub const SERVICE_NAME: &str = "FowlEngine";
pub const SERVICE_DISPLAY: &str = "Fowl Engine (Vector Strike server)";
pub const SERVICE_DESCRIPTION: &str = "Starts DCSServerBot (and through it DCS, bfdb and netidx) at boot, \
restarts it if it dies, keeps the Fowl Engine bot plugin current and updates Fowl Engine Manager itself.";
/// The NSSM-wrapped service from deploy/windows-service -- this replaces it.
pub const OLD_SERVICE_NAME: &str = "DCSServerBot";

pub fn data_dir() -> PathBuf {
    // FOWL_MANAGER_DATA: a throwaway data folder for testing `--console`
    // without touching the real one.
    if let Some(d) = std::env::var_os("FOWL_MANAGER_DATA").filter(|d| !d.is_empty()) {
        return PathBuf::from(d);
    }
    let base = std::env::var_os("ProgramData")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(r"C:\ProgramData"));
    base.join("FowlEngine")
}

pub fn ensure_dirs() -> Result<()> {
    for d in [data_dir(), logs_dir(), commands_dir(), updates_dir(), backups_dir()] {
        std::fs::create_dir_all(&d).with_context(|| format!("creating {}", d.display()))?;
    }
    Ok(())
}

pub fn logs_dir() -> PathBuf {
    data_dir().join("logs")
}
pub fn commands_dir() -> PathBuf {
    data_dir().join("commands")
}
pub fn updates_dir() -> PathBuf {
    data_dir().join("updates")
}
pub fn backups_dir() -> PathBuf {
    data_dir().join("backups")
}
pub fn status_path() -> PathBuf {
    data_dir().join("status.json")
}
pub fn config_path() -> PathBuf {
    data_dir().join("manager.json")
}

/// The bot plugin + extensions that ship inside this app (installed next to
/// the exe as `bot\`, staged at build time by scripts/stage-bot.mjs).
pub fn bundled_bot_dir() -> PathBuf {
    std::env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(Path::to_path_buf))
        .unwrap_or_default()
        .join("bot")
}

fn default_true() -> bool {
    true
}
fn default_channel() -> String {
    "stable".into()
}
fn default_repo() -> String {
    "Dillen198/bfnext".into()
}
fn default_check_hours() -> f64 {
    6.0
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManagerConfig {
    /// DCSServerBot's folder (the one with run.cmd).
    #[serde(default)]
    pub bot_dir: Option<String>,
    /// What to run in bot_dir. run.cmd creates the venv on first start and
    /// loops on the bot's own restart exit code.
    #[serde(default)]
    pub bot_command: Option<String>,
    /// Keep the bundled Fowl Engine plugin + extensions synced into bot_dir.
    #[serde(default = "default_true")]
    pub sync_plugin: bool,
    /// Install new Fowl Engine Manager releases by itself.
    #[serde(default = "default_true")]
    pub auto_update: bool,
    /// stable | beta (beta also takes GitHub pre-releases).
    #[serde(default = "default_channel")]
    pub channel: String,
    #[serde(default = "default_repo")]
    pub repo: String,
    #[serde(default = "default_check_hours")]
    pub check_hours: f64,
    /// "HH:MM-HH:MM" local time the service may update itself in; blank = any.
    /// An update restarts DCSServerBot (DCS itself keeps running).
    #[serde(default)]
    pub update_window: Option<String>,
    /// Only for a private repo or a heavy rate limit.
    #[serde(default)]
    pub github_token: Option<String>,
    /// Start the bot inside a signed-in user's desktop session rather than
    /// the service's own session 0, where DCS hangs creating its window.
    #[serde(default = "default_true")]
    pub desktop_session: bool,
    /// Whose desktop (".\name"); blank = whoever is signed in at the console.
    #[serde(default)]
    pub desktop_user: Option<String>,
    /// Lock the desktop right after Windows signed that user in by itself.
    #[serde(default = "default_true")]
    pub lock_after_autologon: bool,
}

impl Default for ManagerConfig {
    fn default() -> Self {
        serde_json::from_str("{}").expect("defaults")
    }
}

impl ManagerConfig {
    pub fn load() -> ManagerConfig {
        std::fs::read_to_string(config_path())
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok())
            .unwrap_or_default()
    }

    pub fn save(&self) -> Result<()> {
        ensure_dirs()?;
        let tmp = config_path().with_extension("json.tmp");
        std::fs::write(&tmp, serde_json::to_vec_pretty(self)?)?;
        std::fs::rename(&tmp, config_path())?;
        Ok(())
    }

    pub fn bot_dir(&self) -> Option<PathBuf> {
        self.bot_dir
            .as_deref()
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .map(PathBuf::from)
    }

    pub fn bot_command(&self) -> String {
        self.bot_command
            .clone()
            .filter(|s| !s.trim().is_empty())
            .unwrap_or_else(|| "run.cmd".into())
    }
}

/// True when `now` (local) is inside "HH:MM-HH:MM"; no window = always.
pub fn in_window(window: Option<&str>, now: chrono::NaiveTime) -> bool {
    use chrono::Timelike;
    let Some(w) = window.map(str::trim).filter(|w| !w.is_empty()) else {
        return true;
    };
    let parse = |s: &str| -> Option<u32> {
        let (h, m) = s.trim().split_once(':')?;
        let (h, m): (u32, u32) = (h.parse().ok()?, m.parse().ok()?);
        (h < 24 && m < 60).then_some(h * 60 + m)
    };
    let Some((a, b)) = w.split_once('-') else { return true };
    let (Some(a), Some(b)) = (parse(a), parse(b)) else { return true };
    let cur = now.hour() * 60 + now.minute();
    if a == b {
        true
    } else if a < b {
        a <= cur && cur < b
    } else {
        cur >= a || cur < b
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::NaiveTime;

    #[test]
    fn window() {
        let t = |h, m| NaiveTime::from_hms_opt(h, m, 0).unwrap();
        assert!(in_window(None, t(12, 0)));
        assert!(in_window(Some("03:00-07:00"), t(4, 0)));
        assert!(!in_window(Some("03:00-07:00"), t(8, 0)));
        assert!(in_window(Some("22:00-02:00"), t(23, 0)));
        assert!(in_window(Some("22:00-02:00"), t(1, 0)));
        assert!(!in_window(Some("22:00-02:00"), t(12, 0)));
        assert!(in_window(Some("junk"), t(12, 0)));
    }

    #[test]
    fn defaults() {
        let c = ManagerConfig::default();
        assert!(c.auto_update && c.sync_plugin);
        assert_eq!(c.channel, "stable");
        assert_eq!(c.bot_command(), "run.cmd");
    }
}
