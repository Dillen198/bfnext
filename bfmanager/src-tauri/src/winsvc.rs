//! Installing and controlling the FowlEngine Windows service.

use crate::config::{OLD_SERVICE_NAME, SERVICE_DESCRIPTION, SERVICE_DISPLAY, SERVICE_NAME};
use anyhow::{anyhow, bail, Context, Result};
use serde::Serialize;
use std::ffi::OsString;
use std::time::{Duration, Instant};
use windows_service::service::{
    ServiceAccess, ServiceAction, ServiceActionType, ServiceErrorControl, ServiceFailureActions,
    ServiceFailureResetPeriod, ServiceInfo, ServiceStartType, ServiceState, ServiceType,
};
use windows_service::service_manager::{ServiceManager, ServiceManagerAccess};

#[derive(Debug, Clone, Serialize, Default)]
pub struct ServiceStatus {
    pub name: String,
    pub installed: bool,
    pub state: Option<String>,
    pub start_type: Option<String>,
    pub account: Option<String>,
    pub executable: Option<String>,
    /// The service runs a different exe than this one (an old install elsewhere).
    pub foreign_exe: bool,
    pub pid: Option<u32>,
}

fn state_name(s: ServiceState) -> &'static str {
    match s {
        ServiceState::Stopped => "stopped",
        ServiceState::StartPending => "starting",
        ServiceState::StopPending => "stopping",
        ServiceState::Running => "running",
        ServiceState::ContinuePending => "resuming",
        ServiceState::PausePending => "pausing",
        ServiceState::Paused => "paused",
    }
}

pub fn status(name: &str) -> ServiceStatus {
    let mut st = ServiceStatus { name: name.to_string(), ..Default::default() };
    let Ok(mgr) = ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::CONNECT) else {
        return st;
    };
    let Ok(svc) = mgr.open_service(name, ServiceAccess::QUERY_STATUS | ServiceAccess::QUERY_CONFIG) else {
        return st;
    };
    st.installed = true;
    if let Ok(s) = svc.query_status() {
        st.state = Some(state_name(s.current_state).into());
        st.pid = s.process_id;
    }
    if let Ok(c) = svc.query_config() {
        st.start_type = Some(match c.start_type {
            ServiceStartType::AutoStart => "automatic",
            ServiceStartType::OnDemand => "manual",
            ServiceStartType::Disabled => "disabled",
            _ => "system",
        }
        .into());
        st.account = c.account_name.map(|a| a.to_string_lossy().into_owned());
        let exe = c.executable_path.to_string_lossy().into_owned();
        if let Ok(me) = std::env::current_exe() {
            let me = me.to_string_lossy().to_lowercase();
            st.foreign_exe = !exe.to_lowercase().contains(&me);
        }
        st.executable = Some(exe);
    }
    st
}

/// LocalSystem: the only account that can start a program inside a signed-in
/// user's desktop session (WTSQueryUserToken), which is where the bot and DCS
/// have to run -- see desktop.rs.
pub const LOCAL_SYSTEM: &str = "LocalSystem";

fn is_local_system(a: &str) -> bool {
    let a = a.trim().to_lowercase();
    a.is_empty() || a == "localsystem" || a.ends_with("\\localsystem") || a == "nt authority\\system"
}

fn service_info(account: Option<&str>, password: Option<&str>) -> Result<ServiceInfo> {
    let exe = std::env::current_exe().context("locating this exe")?;
    let account = account.map(str::trim).filter(|a| !a.is_empty() && !is_local_system(a));
    Ok(ServiceInfo {
        name: OsString::from(SERVICE_NAME),
        display_name: OsString::from(SERVICE_DISPLAY),
        service_type: ServiceType::OWN_PROCESS,
        start_type: ServiceStartType::AutoStart,
        error_control: ServiceErrorControl::Normal,
        executable_path: exe,
        launch_arguments: vec![OsString::from("--service")],
        dependencies: vec![],
        // spelled out, not None: None means "leave it as it is" when reconfiguring
        account_name: Some(OsString::from(match account {
            Some(a) => normalize_account(a),
            None => LOCAL_SYSTEM.into(),
        })),
        account_password: Some(OsString::from(if account.is_some() { password.unwrap_or("") } else { "" })),
    })
}

/// Create (or reconfigure) the service to run this exe with `--service` as
/// `account` (None = LocalSystem, the normal choice), delayed auto start,
/// restart on failure -- then start it.
pub fn install(account: Option<&str>, password: Option<&str>) -> Result<()> {
    let mgr = ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::CONNECT | ServiceManagerAccess::CREATE_SERVICE)
        .context("opening the service manager (run as administrator)")?;
    let info = service_info(account, password)?;
    if let Some(a) = account.map(str::trim).filter(|a| !a.is_empty() && !is_local_system(a)) {
        grant_logon_as_service(&normalize_account(a))
            .with_context(|| format!("granting 'Log on as a service' to {a}"))?;
    }
    let access = ServiceAccess::QUERY_STATUS | ServiceAccess::START | ServiceAccess::STOP
        | ServiceAccess::CHANGE_CONFIG | ServiceAccess::QUERY_CONFIG;
    let svc = match mgr.open_service(SERVICE_NAME, access) {
        Ok(svc) => {
            stop_and_wait(&svc)?;
            svc.change_config(&info).context("updating the service")?;
            svc
        }
        Err(_) => mgr.create_service(&info, access).context("creating the service")?,
    };
    svc.set_description(SERVICE_DESCRIPTION)?;
    svc.set_delayed_auto_start(true)?;
    svc.update_failure_actions(ServiceFailureActions {
        reset_period: ServiceFailureResetPeriod::After(Duration::from_secs(86_400)),
        reboot_msg: None,
        command: None,
        actions: Some(vec![
            ServiceAction { action_type: ServiceActionType::Restart, delay: Duration::from_secs(15) },
            ServiceAction { action_type: ServiceActionType::Restart, delay: Duration::from_secs(30) },
            ServiceAction { action_type: ServiceActionType::Restart, delay: Duration::from_secs(60) },
        ]),
    })?;
    svc.set_failure_actions_on_non_crash_failures(true)?;
    svc.start::<&str>(&[]).context("starting the service (wrong password?)")?;
    Ok(())
}

/// `--migrate-service`, run by the installer: a service set up by 0.1.x runs
/// as the server's user, so it -- and the DCS it starts -- lives in session 0
/// with no desktop, where DCS hangs. Switch it to LocalSystem and remember
/// that user as the desktop to start the bot in. Leaves the service stopped
/// (the installer starts it). Returns the account it moved from, if any.
pub fn migrate_to_desktop_mode() -> Result<Option<String>> {
    let mgr = ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::CONNECT)?;
    let access = ServiceAccess::QUERY_STATUS | ServiceAccess::STOP | ServiceAccess::CHANGE_CONFIG
        | ServiceAccess::QUERY_CONFIG;
    let Ok(svc) = mgr.open_service(SERVICE_NAME, access) else { return Ok(None) };
    let account = svc.query_config()?.account_name.map(|a| a.to_string_lossy().into_owned()).unwrap_or_default();
    if is_local_system(&account) {
        return Ok(None);
    }
    let mut cfg = crate::config::ManagerConfig::load();
    if cfg.desktop_user.as_deref().map(|u| u.trim().is_empty()).unwrap_or(true) {
        cfg.desktop_user = Some(account.clone());
        cfg.save()?;
    }
    stop_and_wait(&svc)?;
    svc.change_config(&service_info(None, None)?).context("switching the service to LocalSystem")?;
    Ok(Some(account))
}

/// ".\\name" and "name" both mean a local account.
fn normalize_account(a: &str) -> String {
    if a.contains('\\') || a.contains('@') {
        a.to_string()
    } else {
        format!(".\\{a}")
    }
}

fn stop_and_wait(svc: &windows_service::service::Service) -> Result<()> {
    if let Ok(s) = svc.query_status() {
        if s.current_state != ServiceState::Stopped {
            let _ = svc.stop();
            let t = Instant::now();
            while t.elapsed() < Duration::from_secs(60) {
                std::thread::sleep(Duration::from_millis(500));
                if svc.query_status().map(|s| s.current_state == ServiceState::Stopped).unwrap_or(true) {
                    return Ok(());
                }
            }
            bail!("the service did not stop within 60 s");
        }
    }
    Ok(())
}

pub fn control(name: &str, action: &str) -> Result<()> {
    let mgr = ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::CONNECT)?;
    let svc = mgr
        .open_service(name, ServiceAccess::QUERY_STATUS | ServiceAccess::START | ServiceAccess::STOP)
        .with_context(|| format!("opening service {name}"))?;
    match action {
        "start" => svc.start::<&str>(&[]).context("starting the service")?,
        "stop" => stop_and_wait(&svc)?,
        "restart" => {
            stop_and_wait(&svc)?;
            svc.start::<&str>(&[]).context("starting the service")?;
        }
        other => bail!("unknown action {other}"),
    }
    Ok(())
}

pub fn uninstall(name: &str) -> Result<()> {
    let mgr = ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::CONNECT)?;
    let svc = mgr.open_service(name, ServiceAccess::QUERY_STATUS | ServiceAccess::STOP | ServiceAccess::DELETE)?;
    stop_and_wait(&svc)?;
    svc.delete()?;
    Ok(())
}

/// The NSSM "DCSServerBot" service from deploy/windows-service would start a
/// second bot next to ours -- disable it (kept, not deleted, so it can be
/// switched back).
pub fn disable_old_service() -> Result<()> {
    let mgr = ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::CONNECT)?;
    let svc = mgr.open_service(OLD_SERVICE_NAME, ServiceAccess::QUERY_STATUS | ServiceAccess::STOP)?;
    stop_and_wait(&svc)?;
    // sc.exe rather than change_config: rewriting the binary path would
    // re-quote NSSM's and could break it for anyone switching back.
    let out = std::process::Command::new("sc.exe")
        .args(["config", OLD_SERVICE_NAME, "start=", "disabled"])
        .output()
        .context("running sc.exe")?;
    if !out.status.success() {
        bail!("sc.exe config failed: {}", String::from_utf8_lossy(&out.stdout).trim());
    }
    Ok(())
}

// ---- "Log on as a service" -------------------------------------------------------

#[cfg(windows)]
fn grant_logon_as_service(account: &str) -> Result<()> {
    use windows_sys::Win32::Foundation::GetLastError;
    use windows_sys::Win32::Security::Authentication::Identity::{
        LsaAddAccountRights, LsaClose, LsaNtStatusToWinError, LsaOpenPolicy, LSA_OBJECT_ATTRIBUTES,
        LSA_UNICODE_STRING, POLICY_CREATE_ACCOUNT, POLICY_LOOKUP_NAMES,
    };
    use windows_sys::Win32::Security::{LookupAccountNameW, SID_NAME_USE};

    let wide = |s: &str| s.encode_utf16().chain(std::iter::once(0)).collect::<Vec<u16>>();
    // LookupAccountName doesn't understand ".\"
    let lookup = account.strip_prefix(".\\").unwrap_or(account);
    let name = wide(lookup);
    let mut sid = vec![0u8; 256];
    let mut sid_len = sid.len() as u32;
    let mut dom = vec![0u16; 256];
    let mut dom_len = dom.len() as u32;
    let mut use_: SID_NAME_USE = 0;
    let ok = unsafe {
        LookupAccountNameW(std::ptr::null(), name.as_ptr(), sid.as_mut_ptr() as _, &mut sid_len,
                           dom.as_mut_ptr(), &mut dom_len, &mut use_)
    };
    if ok == 0 {
        bail!("no such account {lookup} (error {})", unsafe { GetLastError() });
    }
    let mut attrs: LSA_OBJECT_ATTRIBUTES = unsafe { std::mem::zeroed() };
    attrs.Length = std::mem::size_of::<LSA_OBJECT_ATTRIBUTES>() as u32;
    let mut policy = 0isize;
    let st = unsafe {
        LsaOpenPolicy(std::ptr::null(), &attrs, (POLICY_CREATE_ACCOUNT | POLICY_LOOKUP_NAMES) as u32, &mut policy as *mut _ as _)
    };
    if st != 0 {
        bail!("LsaOpenPolicy failed (error {})", unsafe { LsaNtStatusToWinError(st) });
    }
    let mut right = wide("SeServiceLogonRight");
    right.pop();
    let us = LSA_UNICODE_STRING {
        Length: (right.len() * 2) as u16,
        MaximumLength: (right.len() * 2) as u16,
        Buffer: right.as_mut_ptr(),
    };
    let st = unsafe { LsaAddAccountRights(policy as _, sid.as_mut_ptr() as _, &us, 1) };
    unsafe { LsaClose(policy as _) };
    if st != 0 {
        return Err(anyhow!("LsaAddAccountRights failed (error {})", unsafe { LsaNtStatusToWinError(st) }));
    }
    Ok(())
}

#[cfg(not(windows))]
fn grant_logon_as_service(_account: &str) -> Result<()> {
    Ok(())
}

/// Is this process elevated (a service is; the GUI should be)?
#[cfg(windows)]
pub fn is_elevated() -> bool {
    use windows_sys::Win32::Foundation::{CloseHandle, HANDLE};
    use windows_sys::Win32::Security::{GetTokenInformation, TokenElevation, TOKEN_ELEVATION, TOKEN_QUERY};
    use windows_sys::Win32::System::Threading::{GetCurrentProcess, OpenProcessToken};
    unsafe {
        let mut token: HANDLE = std::ptr::null_mut();
        if OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &mut token) == 0 {
            return false;
        }
        let mut elev: TOKEN_ELEVATION = std::mem::zeroed();
        let mut len = 0u32;
        let ok = GetTokenInformation(token, TokenElevation, &mut elev as *mut _ as _,
                                     std::mem::size_of::<TOKEN_ELEVATION>() as u32, &mut len);
        CloseHandle(token);
        ok != 0 && elev.TokenIsElevated != 0
    }
}

#[cfg(not(windows))]
pub fn is_elevated() -> bool {
    false
}
