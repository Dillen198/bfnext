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
    /// Start this app (in the tray) when an administrator signs in to
    /// Windows -- a logon task, see autostart.rs. The service runs from boot
    /// either way.
    #[serde(default = "default_true")]
    pub start_with_windows: bool,
    /// This PC only controls other server boxes over Tailscale: no
    /// DCSServerBot here, so Setup doesn't ask for one.
    #[serde(default)]
    pub controller_only: bool,
    /// Other server boxes this app controls through their OPS API
    /// (deploy/manager-remote.md). This box's own bot isn't listed: it is
    /// always there as "local" while bot_dir is set.
    #[serde(default)]
    pub servers: Vec<RemoteServer>,
    /// The name this box's own bot shows under in the server picker
    /// (blank = the PC's name).
    #[serde(default)]
    pub local_name: Option<String>,
    /// Let a controller PC start / list / check whole-box backups here
    /// through this box's OPS API (remote.rs). Restore stays local.
    #[serde(default)]
    pub remote_backups: bool,
}

/// One server box this app controls from afar.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct RemoteServer {
    /// Stable, short ([a-z0-9-]); "local" is this box.
    pub id: String,
    pub name: String,
    /// The OPS API's base, e.g. http://100.101.2.86:9876/stats/fowlengine/ops
    pub url: String,
    /// The box's ops_api.api_key. In memory it is the plain key; on disk it
    /// is DPAPI-encrypted (`key_dpapi`), and it never goes to the UI.
    #[serde(default)]
    pub key: Option<String>,
}

impl Default for ManagerConfig {
    fn default() -> Self {
        serde_json::from_str("{}").expect("defaults")
    }
}

/// manager.json keeps github_token DPAPI-encrypted under this key (base64);
/// in memory, and over IPC to the UI (masked there), it is the plain
/// `github_token` as before.
const TOKEN_PROTECTED: &str = "github_token_dpapi";
/// Each servers[] entry keeps its OPS key DPAPI-encrypted under this key.
const SERVER_KEY_PROTECTED: &str = "key_dpapi";

impl ManagerConfig {
    pub fn load() -> ManagerConfig {
        Self::load_from(&config_path())
    }

    pub fn save(&self) -> Result<()> {
        ensure_dirs()?;
        self.save_to(&config_path())
    }

    fn load_from(path: &Path) -> ManagerConfig {
        let Some(mut v) = std::fs::read_to_string(path)
            .ok()
            .and_then(|s| serde_json::from_str::<serde_json::Value>(&s).ok())
        else {
            return ManagerConfig::default();
        };
        let plain_on_disk = v
            .get("github_token")
            .and_then(|t| t.as_str())
            .map(|t| !t.is_empty())
            .unwrap_or(false);
        let mut plain_server_key = false;
        if let Some(list) = v.get_mut("servers").and_then(|s| s.as_array_mut()) {
            for s in list.iter_mut().filter_map(|s| s.as_object_mut()) {
                plain_server_key |= s.get("key").and_then(|k| k.as_str()).map(|k| !k.is_empty()).unwrap_or(false);
                if let Some(blob) = s.remove(SERVER_KEY_PROTECTED).and_then(|b| b.as_str().map(String::from)) {
                    let key = dpapi::unprotect_b64_for(&blob, dpapi::SERVER_KEY)
                        .map_err(|e| {
                            log::warn!(
                                "manager.json: the OPS key of server {} can't be decrypted on this PC ({e:#}) -- enter it again",
                                s.get("name").and_then(|n| n.as_str()).unwrap_or("?")
                            );
                        })
                        .ok();
                    s.insert("key".into(), key.map(Into::into).unwrap_or(serde_json::Value::Null));
                }
            }
        }
        if let Some(blob) = v.get(TOKEN_PROTECTED).and_then(|b| b.as_str()).map(String::from) {
            let token = dpapi::unprotect_b64(&blob)
                .map_err(|e| {
                    // e.g. manager.json restored onto another PC: the token
                    // has to be entered again there
                    log::warn!(
                        "manager.json: the saved GitHub token can't be decrypted on this PC ({e:#}) -- enter it again"
                    );
                })
                .ok();
            if let Some(o) = v.as_object_mut() {
                o.insert("github_token".into(), token.map(Into::into).unwrap_or(serde_json::Value::Null));
            }
        }
        let cfg: ManagerConfig = serde_json::from_value(v).unwrap_or_default();
        // Written by a version that kept the token in plain text: encrypt it
        // now (best effort -- only an elevated GUI or the service can write).
        if (plain_on_disk || plain_server_key) && dpapi::AVAILABLE {
            if let Err(e) = cfg.save_to(path) {
                log::debug!("manager.json: encrypting the GitHub token deferred: {e:#}");
            }
        }
        cfg
    }

    fn save_to(&self, path: &Path) -> Result<()> {
        let mut v = serde_json::to_value(self)?;
        if let Some(o) = v.as_object_mut() {
            o.remove(TOKEN_PROTECTED);
            let token = self.github_token.as_deref().filter(|t| !t.is_empty());
            if let (Some(t), true) = (token, dpapi::AVAILABLE) {
                match dpapi::protect_b64(t) {
                    Ok(blob) => {
                        o.remove("github_token");
                        o.insert(TOKEN_PROTECTED.into(), blob.into());
                    }
                    // keep it (in plain text, as before) rather than lose it
                    Err(e) => log::warn!(
                        "manager.json: could not encrypt the GitHub token ({e:#}); saved as plain text"
                    ),
                }
            }
            if let Some(list) = o.get_mut("servers").and_then(|s| s.as_array_mut()) {
                for s in list.iter_mut().filter_map(|s| s.as_object_mut()) {
                    s.remove(SERVER_KEY_PROTECTED);
                    let key = s.get("key").and_then(|k| k.as_str()).filter(|k| !k.is_empty()).map(String::from);
                    match key {
                        None => {
                            s.remove("key");
                        }
                        Some(k) if dpapi::AVAILABLE => match dpapi::protect_b64_for(&k, dpapi::SERVER_KEY) {
                            Ok(blob) => {
                                s.remove("key");
                                s.insert(SERVER_KEY_PROTECTED.into(), blob.into());
                            }
                            Err(e) => log::warn!("manager.json: could not encrypt a server's OPS key ({e:#}); saved as plain text"),
                        },
                        Some(_) => {}
                    }
                }
            }
        }
        let tmp = path.with_extension("json.tmp");
        std::fs::write(&tmp, serde_json::to_vec_pretty(&v)?)?;
        std::fs::rename(&tmp, path)?;
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

/// Windows DPAPI, machine scope: the GUI (an elevated admin) and the service
/// (LocalSystem) both have to read the token, so it is bound to this PC
/// rather than to one account. That keeps it out of copies of manager.json --
/// backup zips, a disk image, a support upload -- while the folder's ACL
/// (Administrators + SYSTEM, see agent::restrict_data_dir) still decides who
/// on this PC can read it.
pub mod dpapi {
    use anyhow::Result;
    use base64::Engine as _;

    pub const AVAILABLE: bool = cfg!(windows);
    /// Ties the blobs to this use, so another program's DPAPI blob can't be
    /// swapped in.
    pub const GITHUB_TOKEN: &[u8] = b"FowlEngineManager/manager.json/github_token/v1";
    /// A server's OPS key (servers[].key_dpapi): its own entropy, so a key
    /// blob can't be moved into github_token_dpapi and sent to GitHub.
    pub const SERVER_KEY: &[u8] = b"FowlEngineManager/manager.json/server_key/v1";

    pub fn protect_b64(plain: &str) -> Result<String> {
        protect_b64_for(plain, GITHUB_TOKEN)
    }

    pub fn unprotect_b64(blob: &str) -> Result<String> {
        unprotect_b64_for(blob, GITHUB_TOKEN)
    }

    pub fn protect_b64_for(plain: &str, entropy: &[u8]) -> Result<String> {
        Ok(base64::engine::general_purpose::STANDARD.encode(protect(plain.as_bytes(), entropy)?))
    }

    pub fn unprotect_b64_for(blob: &str, entropy: &[u8]) -> Result<String> {
        let raw = base64::engine::general_purpose::STANDARD.decode(blob.trim())?;
        Ok(String::from_utf8(unprotect(&raw, entropy)?)?)
    }

    #[cfg(windows)]
    fn call(data: &[u8], encrypt: bool, entropy: &[u8]) -> Result<Vec<u8>> {
        use windows_sys::Win32::Foundation::LocalFree;
        use windows_sys::Win32::Security::Cryptography::{
            CryptProtectData, CryptUnprotectData, CRYPTPROTECT_LOCAL_MACHINE,
            CRYPTPROTECT_UI_FORBIDDEN, CRYPT_INTEGER_BLOB,
        };
        let input = CRYPT_INTEGER_BLOB { cbData: data.len() as u32, pbData: data.as_ptr() as *mut u8 };
        let entropy =
            CRYPT_INTEGER_BLOB { cbData: entropy.len() as u32, pbData: entropy.as_ptr() as *mut u8 };
        let mut out = CRYPT_INTEGER_BLOB { cbData: 0, pbData: std::ptr::null_mut() };
        let flags = CRYPTPROTECT_UI_FORBIDDEN | CRYPTPROTECT_LOCAL_MACHINE;
        // SAFETY: every pointer is valid for the duration of the call; `out`
        // is allocated by DPAPI with LocalAlloc and freed below.
        let ok = unsafe {
            if encrypt {
                CryptProtectData(
                    &input,
                    std::ptr::null(),
                    &entropy,
                    std::ptr::null(),
                    std::ptr::null(),
                    flags,
                    &mut out,
                )
            } else {
                CryptUnprotectData(
                    &input,
                    std::ptr::null_mut(),
                    &entropy,
                    std::ptr::null(),
                    std::ptr::null(),
                    flags,
                    &mut out,
                )
            }
        };
        if ok == 0 || out.pbData.is_null() {
            return Err(std::io::Error::last_os_error().into());
        }
        // SAFETY: DPAPI returned cbData bytes at pbData.
        let v = unsafe { std::slice::from_raw_parts(out.pbData, out.cbData as usize) }.to_vec();
        unsafe { LocalFree(out.pbData as _) };
        Ok(v)
    }

    #[cfg(windows)]
    fn protect(data: &[u8], entropy: &[u8]) -> Result<Vec<u8>> {
        call(data, true, entropy)
    }
    #[cfg(windows)]
    fn unprotect(data: &[u8], entropy: &[u8]) -> Result<Vec<u8>> {
        call(data, false, entropy)
    }
    #[cfg(not(windows))]
    fn protect(_: &[u8], _: &[u8]) -> Result<Vec<u8>> {
        anyhow::bail!("DPAPI is Windows-only")
    }
    #[cfg(not(windows))]
    fn unprotect(_: &[u8], _: &[u8]) -> Result<Vec<u8>> {
        anyhow::bail!("DPAPI is Windows-only")
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

    #[cfg(windows)]
    #[test]
    fn dpapi_round_trip() {
        let blob = dpapi::protect_b64("ghp_example").unwrap();
        assert!(!blob.contains("ghp_example"));
        assert_eq!(dpapi::unprotect_b64(&blob).unwrap(), "ghp_example");
        assert!(dpapi::unprotect_b64("bm90IGEgYmxvYg==").is_err());
    }

    /// An old plain-text manager.json loads, and is rewritten encrypted.
    #[cfg(windows)]
    #[test]
    fn token_migrates_to_dpapi() {
        let dir = std::env::temp_dir().join(format!("fowl-cfg-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("manager.json");
        std::fs::write(&path, r#"{"github_token":"ghp_plain","channel":"beta"}"#).unwrap();
        let c = ManagerConfig::load_from(&path);
        assert_eq!(c.github_token.as_deref(), Some("ghp_plain"));
        assert_eq!(c.channel, "beta");
        let disk = std::fs::read_to_string(&path).unwrap();
        assert!(!disk.contains("ghp_plain") && disk.contains(TOKEN_PROTECTED), "{disk}");
        assert_eq!(ManagerConfig::load_from(&path).github_token.as_deref(), Some("ghp_plain"));
        // a blob from another PC (or garbage) is dropped, not fatal
        std::fs::write(&path, r#"{"github_token_dpapi":"AAAA","channel":"beta"}"#).unwrap();
        let c = ManagerConfig::load_from(&path);
        assert!(c.github_token.is_none() && c.channel == "beta");
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// Server OPS keys: encrypted on disk under their own entropy, plain in
    /// memory; a config from before the registry loads with none.
    #[cfg(windows)]
    #[test]
    fn server_keys_are_dpapi_encrypted() {
        let dir = std::env::temp_dir().join(format!("fowl-srv-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("manager.json");
        // an existing install: no servers key at all
        std::fs::write(&path, r#"{"bot_dir":"C:\\DCSServerBot","channel":"stable"}"#).unwrap();
        let mut c = ManagerConfig::load_from(&path);
        assert!(c.servers.is_empty() && !c.controller_only && !c.remote_backups);
        assert_eq!(c.bot_dir.as_deref(), Some("C:\\DCSServerBot"));
        c.servers.push(RemoteServer {
            id: "range".into(),
            name: "Range".into(),
            url: "http://100.101.2.86:9876/fowlengine/ops".into(),
            key: Some("opskey-0123456789abcdef".into()),
        });
        c.servers.push(RemoteServer { id: "nokey".into(), name: "No key".into(), url: "http://100.1.1.1:1/fowlengine/ops".into(), key: None });
        c.save_to(&path).unwrap();
        let disk = std::fs::read_to_string(&path).unwrap();
        assert!(!disk.contains("opskey-0123456789abcdef") && disk.contains(SERVER_KEY_PROTECTED), "{disk}");
        let back = ManagerConfig::load_from(&path);
        assert_eq!(back.servers, c.servers);
        // a server key blob can't be passed off as the GitHub token
        let v: serde_json::Value = serde_json::from_str(&disk).unwrap();
        let blob = v["servers"][0][SERVER_KEY_PROTECTED].as_str().unwrap();
        assert!(dpapi::unprotect_b64(blob).is_err());
        // a key typed into manager.json by hand is encrypted on the next load
        std::fs::write(&path, r#"{"servers":[{"id":"a","name":"A","url":"http://100.1.1.1:1/fowlengine/ops","key":"plain-key-0123456789"}]}"#).unwrap();
        assert_eq!(ManagerConfig::load_from(&path).servers[0].key.as_deref(), Some("plain-key-0123456789"));
        assert!(!std::fs::read_to_string(&path).unwrap().contains("plain-key-0123456789"));
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn defaults() {
        let c = ManagerConfig::default();
        assert!(c.auto_update && c.sync_plugin && c.start_with_windows);
        assert!(!c.controller_only && !c.remote_backups && c.servers.is_empty());
        assert_eq!(c.channel, "stable");
        assert_eq!(c.bot_command(), "run.cmd");
    }
}
