//! Fowl Engine Manager.
//!
//! One exe, three ways to run:
//!   (no args)    the desktop app (Tauri) -- setup wizard, status, updates, OPS
//!   --service    the Windows service the desktop app installs (agent.rs)
//!   --console    the service loop in a terminal, for testing

pub mod agent;
pub mod bot;
pub mod config;
pub mod desktop;
pub mod update;
#[cfg(windows)]
pub mod winsvc;

use config::ManagerConfig;
use serde::Serialize;
use std::path::PathBuf;

type CmdResult<T> = Result<T, String>;

fn err<E: std::fmt::Display>(e: E) -> String {
    format!("{e:#}")
}

const MASK: &str = "__SECRET__";

async fn blocking<T: Send + 'static>(f: impl FnOnce() -> anyhow::Result<T> + Send + 'static) -> CmdResult<T> {
    tauri::async_runtime::spawn_blocking(f)
        .await
        .map_err(err)?
        .map_err(|e| format!("{e:#}"))
}

#[derive(Serialize)]
struct AppState {
    version: String,
    exe: String,
    data_dir: String,
    current_user: String,
    hostname: String,
    elevated: bool,
    config: ManagerConfig,
    service: Option<serde_json::Value>,
    old_service: Option<serde_json::Value>,
    agent: Option<serde_json::Value>,
    agent_fresh: bool,
    bot_dir_valid: bool,
    bundle_version: Option<String>,
    plugin_pending: Vec<String>,
    plugin_link: Option<String>,
    ops_target: Option<String>,
    ops_error: Option<String>,
    /// Windows' automatic sign-in (who, and is a password stored).
    autologon: desktop::Autologon,
    /// Signed-in user sessions (the bot runs in one of them).
    sessions: Vec<desktop::Session>,
}

#[tauri::command]
async fn get_state() -> CmdResult<serde_json::Value> {
    blocking(|| {
        let _ = config::ensure_dirs();
        let mut cfg = ManagerConfig::load();
        if cfg.github_token.as_deref().map(|t| !t.is_empty()).unwrap_or(false) {
            cfg.github_token = Some(MASK.into());
        }
        #[cfg(windows)]
        let (service, old_service, elevated) = (
            Some(serde_json::to_value(winsvc::status(config::SERVICE_NAME))?),
            Some(serde_json::to_value(winsvc::status(config::OLD_SERVICE_NAME))?),
            winsvc::is_elevated(),
        );
        #[cfg(not(windows))]
        let (service, old_service, elevated) = (None, None, false);
        let agent: Option<serde_json::Value> = std::fs::read_to_string(config::status_path())
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok());
        let agent_fresh = agent
            .as_ref()
            .and_then(|a| a["heartbeat"].as_str().map(String::from))
            .and_then(|h| chrono::DateTime::parse_from_rfc3339(&h).ok())
            .map(|t| (chrono::Local::now().fixed_offset() - t).num_seconds() < 30)
            .unwrap_or(false);
        let bot_dir = cfg.bot_dir();
        let bot_dir_valid = bot_dir.as_deref().map(bot::is_bot_dir).unwrap_or(false);
        let bundle = bot::bundle_manifest();
        let (plugin_pending, plugin_link) = match (&bot_dir, &bundle) {
            (Some(d), Some(m)) if bot_dir_valid => (bot::plugin_diff(d, m), bot::linked_plugin(d)),
            _ => (vec![], None),
        };
        let (ops_target, ops_error) = match &bot_dir {
            Some(d) if bot_dir_valid => match bot::ops_target(d) {
                Ok(t) => (Some(t.base), None),
                Err(e) => (None, Some(format!("{e:#}"))),
            },
            _ => (None, None),
        };
        let st = AppState {
            version: update::current_version().to_string(),
            exe: std::env::current_exe().map(|p| p.display().to_string()).unwrap_or_default(),
            data_dir: config::data_dir().display().to_string(),
            current_user: std::env::var("USERNAME").unwrap_or_default(),
            hostname: std::env::var("COMPUTERNAME").unwrap_or_default(),
            elevated,
            config: cfg,
            service,
            old_service,
            agent,
            agent_fresh,
            bot_dir_valid,
            bundle_version: bundle.map(|b| b.version),
            plugin_pending,
            plugin_link,
            ops_target,
            ops_error,
            autologon: desktop::autologon_status(),
            sessions: desktop::sessions(),
        };
        Ok(serde_json::to_value(st)?)
    })
    .await
}

/// Turn on Windows' automatic sign-in for `account` (so the bot's desktop
/// session comes back by itself after a reboot). The password goes to Windows'
/// LSA store, checked first; this app never writes it anywhere.
#[tauri::command]
async fn enable_autologon(account: String, password: String) -> CmdResult<()> {
    blocking(move || desktop::enable_autologon(&account, &password)).await
}

#[tauri::command]
async fn disable_autologon() -> CmdResult<()> {
    blocking(desktop::disable_autologon).await
}

#[tauri::command]
async fn save_config(config: ManagerConfig) -> CmdResult<()> {
    blocking(move || {
        let mut config = config;
        if config.github_token.as_deref() == Some(MASK) {
            config.github_token = ManagerConfig::load().github_token;
        }
        if !["stable", "beta"].contains(&config.channel.as_str()) {
            anyhow::bail!("channel must be stable or beta");
        }
        if let Some(d) = config.bot_dir() {
            if !bot::is_bot_dir(&d) {
                anyhow::bail!("{} is not a DCSServerBot folder (no run.py / core / plugins)", d.display());
            }
        }
        config.save()
    })
    .await
}

#[tauri::command]
async fn detect_bot_dirs() -> CmdResult<Vec<String>> {
    blocking(|| Ok(bot::detect_bot_dirs())).await
}

#[tauri::command]
async fn detect_bot_accounts() -> CmdResult<Vec<bot::BotAccount>> {
    blocking(|| Ok(bot::detect_bot_accounts())).await
}

#[derive(Serialize)]
struct BotDirCheck {
    valid: bool,
    has_venv_hint: bool,
    ops_target: Option<String>,
    ops_error: Option<String>,
    plugin_installed: bool,
}

#[tauri::command]
async fn check_bot_dir(path: String) -> CmdResult<serde_json::Value> {
    blocking(move || {
        let d = PathBuf::from(path.trim());
        let valid = bot::is_bot_dir(&d);
        let (ops_target, ops_error) = if valid {
            match bot::ops_target(&d) {
                Ok(t) => (Some(t.base), None),
                Err(e) => (None, Some(format!("{e:#}"))),
            }
        } else {
            (None, None)
        };
        Ok(serde_json::to_value(BotDirCheck {
            valid,
            has_venv_hint: d.join("run.cmd").is_file(),
            ops_target,
            ops_error,
            plugin_installed: d.join("plugins").join("fowlengine").join("commands.py").is_file(),
        })?)
    })
    .await
}

#[tauri::command]
async fn install_service(account: Option<String>, password: Option<String>) -> CmdResult<()> {
    #[cfg(windows)]
    {
        blocking(move || winsvc::install(account.as_deref(), password.as_deref())).await
    }
    #[cfg(not(windows))]
    {
        let _ = (account, password);
        Err("Windows only".into())
    }
}

#[tauri::command]
async fn service_control(action: String) -> CmdResult<()> {
    #[cfg(windows)]
    {
        blocking(move || winsvc::control(config::SERVICE_NAME, &action)).await
    }
    #[cfg(not(windows))]
    {
        let _ = action;
        Err("Windows only".into())
    }
}

#[tauri::command]
async fn uninstall_service() -> CmdResult<()> {
    #[cfg(windows)]
    {
        blocking(|| winsvc::uninstall(config::SERVICE_NAME)).await
    }
    #[cfg(not(windows))]
    {
        Err("Windows only".into())
    }
}

#[tauri::command]
async fn disable_old_service() -> CmdResult<()> {
    #[cfg(windows)]
    {
        blocking(winsvc::disable_old_service).await
    }
    #[cfg(not(windows))]
    {
        Err("Windows only".into())
    }
}

/// Ask the running service to do something (it polls commands\ every 2 s).
#[tauri::command]
async fn agent_command(name: String) -> CmdResult<()> {
    blocking(move || {
        const OK: [&str; 5] = ["restart-bot", "stop-bot", "start-bot", "check-update", "install-update"];
        if !OK.contains(&name.as_str()) {
            anyhow::bail!("unknown command {name}");
        }
        config::ensure_dirs()?;
        std::fs::write(config::commands_dir().join(&name), chrono::Local::now().to_rfc3339())?;
        Ok(())
    })
    .await
}

/// Sync the bundled plugin now. With the service running the bot is
/// restarted (the service syncs before every start); otherwise copy directly.
#[tauri::command]
async fn sync_plugin_now() -> CmdResult<String> {
    blocking(|| {
        let cfg = ManagerConfig::load();
        let dir = cfg.bot_dir().ok_or_else(|| anyhow::anyhow!("no DCSServerBot folder configured"))?;
        let fresh = std::fs::read_to_string(config::status_path())
            .ok()
            .and_then(|s| serde_json::from_str::<serde_json::Value>(&s).ok())
            .and_then(|a| a["bot"]["running"].as_bool())
            .unwrap_or(false);
        if fresh {
            std::fs::write(config::commands_dir().join("restart-bot"), "sync")?;
            return Ok("the service is restarting DCSServerBot and syncs the plugin on the way up".into());
        }
        let rep = bot::sync_plugin(&dir)?;
        Ok(match (&rep.skipped_reason, rep.changed.len()) {
            (Some(why), _) => format!("skipped: {why}"),
            (None, 0) => "already up to date".into(),
            (None, n) => format!("{n} file(s) updated (backup {})", rep.backup.unwrap_or_default()),
        })
    })
    .await
}

#[tauri::command]
async fn check_update() -> CmdResult<update::CheckResult> {
    blocking(|| Ok(update::check(&ManagerConfig::load()))).await
}

/// Download + verify the newest release and run its installer with a
/// progress bar; it restarts the service and relaunches this app.
#[tauri::command]
async fn install_update(app: tauri::AppHandle) -> CmdResult<String> {
    let path = blocking(|| {
        let cfg = ManagerConfig::load();
        let res = update::check(&cfg);
        if let Some(e) = res.error {
            anyhow::bail!(e);
        }
        let rel = res
            .latest
            .filter(|_| res.update_available)
            .ok_or_else(|| anyhow::anyhow!("already on the newest version"))?;
        update::download(&cfg, &rel)
    })
    .await?;
    update::launch_installer(&path, false).map_err(err)?;
    let h = app.clone();
    std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(800));
        h.exit(0);
    });
    Ok("installing -- the app closes and comes back when it's done".into())
}

#[derive(Serialize)]
struct OpsResponse {
    status: u16,
    content_type: String,
    body: String,
}

/// The FowlEngine plugin's OPS API, straight from this PC (no Discord login:
/// whoever can run this app as admin already owns the box).
#[tauri::command]
async fn ops_request(method: String, path: String, body: Option<String>) -> CmdResult<OpsResponse> {
    blocking(move || {
        let cfg = ManagerConfig::load();
        let dir = cfg.bot_dir().ok_or_else(|| anyhow::anyhow!("no DCSServerBot folder configured -- run Setup"))?;
        let t = bot::ops_target(&dir)?;
        let path = path.trim_start_matches('/');
        if path.contains("..") {
            anyhow::bail!("bad path");
        }
        let url = format!("{}/{}", t.base, path);
        let c = reqwest::blocking::Client::builder().timeout(std::time::Duration::from_secs(300)).build()?;
        let req = if method.eq_ignore_ascii_case("POST") {
            c.post(&url).header("content-type", "application/json").body(body.unwrap_or_else(|| "{}".into()))
        } else {
            c.get(&url)
        };
        let resp = req.header("X-API-Key", t.key).send().map_err(|e| {
            anyhow::anyhow!("the bot's WebService is not answering at {} ({e}) -- is DCSServerBot running?", t.base)
        })?;
        let status = resp.status().as_u16();
        let content_type = resp
            .headers()
            .get("content-type")
            .and_then(|v| v.to_str().ok())
            .unwrap_or("application/json")
            .to_string();
        Ok(OpsResponse { status, content_type, body: resp.text()? })
    })
    .await
}

#[tauri::command]
async fn read_log(name: String, lines: usize) -> CmdResult<Vec<String>> {
    blocking(move || {
        let file = match name.as_str() {
            "agent" => "agent.log",
            "bot" => "bot-console.log",
            other => anyhow::bail!("unknown log {other}"),
        };
        let p = config::logs_dir().join(file);
        let data = std::fs::read(&p).unwrap_or_default();
        let start = data.len().saturating_sub(4 * 1024 * 1024);
        let text = String::from_utf8_lossy(&data[start..]);
        let all: Vec<&str> = text.lines().collect();
        let n = lines.clamp(10, 20_000);
        Ok(all[all.len().saturating_sub(n)..].iter().map(|s| s.to_string()).collect())
    })
    .await
}

#[tauri::command]
async fn open_path(which: String) -> CmdResult<()> {
    let target = match which.as_str() {
        "data" => config::data_dir(),
        "logs" => config::logs_dir(),
        "backups" => config::backups_dir(),
        "bot" => ManagerConfig::load().bot_dir().ok_or("no bot folder configured")?,
        _ => return Err("unknown folder".into()),
    };
    std::process::Command::new("explorer.exe").arg(target).spawn().map_err(err)?;
    Ok(())
}

// ---- the window lives in the tray -------------------------------------------------
//
// Closing the window hides it; the tray icon brings it back and its menu can
// restart the bot or quit. Quitting only closes this window app -- the
// FowlEngine service keeps the server running either way. A second launch
// (Start menu, desktop icon) just shows the running one.

fn show_main(app: &tauri::AppHandle) {
    use tauri::Manager;
    if let Some(w) = app.get_webview_window("main") {
        let _ = w.unminimize();
        let _ = w.show();
        let _ = w.set_focus();
    }
}

/// One line for the tray tooltip, from the service's status file.
fn tray_status() -> String {
    let st: Option<serde_json::Value> = std::fs::read_to_string(config::status_path())
        .ok()
        .and_then(|s| serde_json::from_str(&s).ok());
    let fresh = st
        .as_ref()
        .and_then(|a| a["heartbeat"].as_str())
        .and_then(|h| chrono::DateTime::parse_from_rfc3339(h).ok())
        .map(|t| (chrono::Local::now().fixed_offset() - t).num_seconds() < 30)
        .unwrap_or(false);
    let bot = match st.as_ref().map(|s| &s["bot"]) {
        _ if !fresh => "service not running".to_string(),
        Some(b) if b["running"].as_bool().unwrap_or(false) => "DCSServerBot running".into(),
        Some(b) if b["paused"].as_bool().unwrap_or(false) => "DCSServerBot stopped (paused)".into(),
        _ => "DCSServerBot DOWN".into(),
    };
    format!("Fowl Engine Manager -- {bot}")
}

fn setup_tray(app: &tauri::App) -> tauri::Result<()> {
    use tauri::menu::{Menu, MenuItem, PredefinedMenuItem};
    use tauri::tray::{MouseButton, MouseButtonState, TrayIconBuilder, TrayIconEvent};

    let open = MenuItem::with_id(app, "open", "Open Fowl Engine Manager", true, None::<&str>)?;
    let restart = MenuItem::with_id(app, "restart-bot", "Restart DCSServerBot", true, None::<&str>)?;
    let sep = PredefinedMenuItem::separator(app)?;
    let quit = MenuItem::with_id(app, "quit", "Quit (the server keeps running)", true, None::<&str>)?;
    let menu = Menu::with_items(app, &[&open, &restart, &sep, &quit])?;
    let mut tray = TrayIconBuilder::with_id("main")
        .tooltip(tray_status())
        .menu(&menu)
        .show_menu_on_left_click(false)
        .on_menu_event(|app, e| match e.id.as_ref() {
            "open" => show_main(app),
            "restart-bot" => {
                let _ = config::ensure_dirs();
                let _ = std::fs::write(config::commands_dir().join("restart-bot"), "tray");
            }
            "quit" => app.exit(0),
            _ => {}
        })
        .on_tray_icon_event(|tray, e| {
            if let TrayIconEvent::Click { button: MouseButton::Left, button_state: MouseButtonState::Up, .. } = e {
                show_main(tray.app_handle());
            }
        });
    if let Some(icon) = app.default_window_icon() {
        tray = tray.icon(icon.clone());
    }
    tray.build(app)?;

    // keep the tooltip current
    let handle = app.handle().clone();
    std::thread::spawn(move || loop {
        std::thread::sleep(std::time::Duration::from_secs(10));
        if let Some(t) = handle.tray_by_id("main") {
            let _ = t.set_tooltip(Some(tray_status()));
        }
    });
    Ok(())
}

/// `--migrate-service` (the installer): move a 0.1.x service off the user
/// account it ran as, onto LocalSystem + that user's desktop session.
pub fn migrate_service() {
    #[cfg(windows)]
    match winsvc::migrate_to_desktop_mode() {
        Ok(Some(from)) => println!("FowlEngine service moved from {from} to LocalSystem; the bot runs on {from}'s desktop"),
        Ok(None) => println!("FowlEngine service: nothing to migrate"),
        Err(e) => {
            eprintln!("could not migrate the FowlEngine service: {e:#}");
            std::process::exit(1);
        }
    }
}

pub fn run() {
    let _ = config::ensure_dirs();
    tauri::Builder::default()
        // must be the first plugin: a second launch hands over and exits
        .plugin(tauri_plugin_single_instance::init(|app, _args, _cwd| show_main(app)))
        .setup(|app| {
            setup_tray(app)?;
            Ok(())
        })
        .on_window_event(|window, event| {
            if let tauri::WindowEvent::CloseRequested { api, .. } = event {
                // to the tray, not closed -- Quit is in the tray menu
                api.prevent_close();
                let _ = window.hide();
            }
        })
        .invoke_handler(tauri::generate_handler![
            get_state,
            save_config,
            detect_bot_dirs,
            detect_bot_accounts,
            check_bot_dir,
            install_service,
            service_control,
            uninstall_service,
            disable_old_service,
            enable_autologon,
            disable_autologon,
            agent_command,
            sync_plugin_now,
            check_update,
            install_update,
            ops_request,
            read_log,
            open_path,
        ])
        .run(tauri::generate_context!())
        .expect("error while running Fowl Engine Manager");
}
