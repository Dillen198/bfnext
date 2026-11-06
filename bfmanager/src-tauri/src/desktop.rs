//! Running DCSServerBot inside a user's desktop session, and signing that
//! user in automatically after a reboot.
//!
//! Why: a Windows service lives in session 0, which has no interactive
//! desktop. Whatever the service starts inherits that -- and DCS (2.9+) hangs
//! while it sets up its main window there, even with --norender:
//! DCSServerBot then reports "timeout while launching" and, three minutes
//! later, kills the server as hung. So the service (LocalSystem) launches the
//! bot *into the chosen user's session* instead: the bot runs as that user,
//! with that user's profile and PATH, on their desktop, and every DCS it
//! starts gets a real window.
//!
//! That session has to exist, so for "comes back after a reboot or a blue
//! screen with nobody there", Windows signs the user in by itself: the same
//! Winlogon AutoAdminLogon mechanism Sysinternals Autologon uses, with the
//! password kept as the LSA secret "DefaultPassword" (encrypted, never in the
//! registry, never in this app's files). The desktop can be locked right after
//! the automatic sign-in; programs keep running on a locked desktop.

use anyhow::{bail, Context, Result};
use serde::Serialize;
use std::path::Path;

#[derive(Debug, Clone, Serialize)]
pub struct Session {
    pub id: u32,
    pub user: String,
    pub domain: String,
    /// "active" (someone at the console / RDP) or "disconnected" (RDP closed,
    /// programs still running).
    pub state: String,
    /// Seconds since this session signed in.
    pub logon_age_secs: Option<u64>,
}

/// The account part of ".\\name", "DOMAIN\\name" or "name", for comparing
/// with a session's user name.
pub fn account_name(account: &str) -> &str {
    account.rsplit('\\').next().unwrap_or(account).trim()
}

#[cfg(windows)]
mod imp {
    use super::*;
    use windows_sys::Win32::Foundation::{CloseHandle, GetLastError, HANDLE};
    use windows_sys::Win32::Security::{
        GetTokenInformation, TokenElevationType, TokenElevationTypeLimited, TokenLinkedToken,
        TOKEN_ELEVATION_TYPE, TOKEN_LINKED_TOKEN,
    };
    use windows_sys::Win32::System::Environment::{CreateEnvironmentBlock, DestroyEnvironmentBlock};
    use windows_sys::Win32::System::RemoteDesktop::{
        WTSActive, WTSDisconnected, WTSEnumerateSessionsW, WTSFreeMemory, WTSQuerySessionInformationW,
        WTSQueryUserToken, WTSSessionInfo, WTSINFOW, WTS_CURRENT_SERVER_HANDLE, WTS_SESSION_INFOW,
    };
    use windows_sys::Win32::System::Threading::{
        CreateProcessAsUserW, GetExitCodeProcess, TerminateProcess, WaitForSingleObject,
        CREATE_NEW_CONSOLE, CREATE_NEW_PROCESS_GROUP, CREATE_UNICODE_ENVIRONMENT, PROCESS_INFORMATION,
        STARTF_USESHOWWINDOW, STARTUPINFOW,
    };

    pub(super) fn wide(s: &str) -> Vec<u16> {
        s.encode_utf16().chain(std::iter::once(0)).collect()
    }

    fn from_wide(buf: &[u16]) -> String {
        let end = buf.iter().position(|&c| c == 0).unwrap_or(buf.len());
        String::from_utf16_lossy(&buf[..end])
    }

    /// FILETIME-style 100 ns ticks since 1601 -> seconds since the Unix epoch.
    fn filetime_secs(t: i64) -> Option<u64> {
        const EPOCH_DIFF: i64 = 11_644_473_600;
        (t > 0).then(|| (t / 10_000_000 - EPOCH_DIFF).max(0) as u64)
    }

    pub fn sessions() -> Vec<Session> {
        let mut out = Vec::new();
        unsafe {
            let mut list: *mut WTS_SESSION_INFOW = std::ptr::null_mut();
            let mut count = 0u32;
            if WTSEnumerateSessionsW(WTS_CURRENT_SERVER_HANDLE, 0, 1, &mut list, &mut count) == 0 {
                return out;
            }
            for i in 0..count as usize {
                let s = &*list.add(i);
                if s.State != WTSActive && s.State != WTSDisconnected {
                    continue;
                }
                let mut buf: *mut u16 = std::ptr::null_mut();
                let mut len = 0u32;
                if WTSQuerySessionInformationW(WTS_CURRENT_SERVER_HANDLE, s.SessionId, WTSSessionInfo,
                                               &mut buf, &mut len) == 0 {
                    continue;
                }
                let info = &*(buf as *const WTSINFOW);
                let user = from_wide(&info.UserName);
                let domain = from_wide(&info.Domain);
                let now = filetime_secs(info.CurrentTime);
                let logon = filetime_secs(info.LogonTime);
                WTSFreeMemory(buf as _);
                if user.is_empty() {
                    continue; // the logon screen, or services
                }
                out.push(Session {
                    id: s.SessionId,
                    user,
                    domain,
                    state: if s.State == WTSActive { "active" } else { "disconnected" }.into(),
                    logon_age_secs: match (now, logon) {
                        (Some(n), Some(l)) => Some(n.saturating_sub(l)),
                        _ => None,
                    },
                });
            }
            WTSFreeMemory(list as _);
        }
        out
    }

    /// The user's own token for `session`, elevated if they're an admin (the
    /// service had full rights; so should the bot it starts). Needs the
    /// caller to be LocalSystem.
    fn user_token(session: u32) -> Result<HANDLE> {
        unsafe {
            let mut token: HANDLE = std::ptr::null_mut();
            if WTSQueryUserToken(session, &mut token) == 0 {
                let e = GetLastError();
                if e == 1314 {
                    bail!("the service can't open the user's desktop session (it must run as LocalSystem: \
                           reinstall it from Setup)");
                }
                bail!("could not get the signed-in user's token for session {session} (error {e})");
            }
            let mut et: TOKEN_ELEVATION_TYPE = 0;
            let mut len = 0u32;
            let limited = GetTokenInformation(token, TokenElevationType, &mut et as *mut _ as _,
                                              std::mem::size_of::<TOKEN_ELEVATION_TYPE>() as u32, &mut len) != 0
                && et == TokenElevationTypeLimited;
            if limited {
                let mut linked: TOKEN_LINKED_TOKEN = std::mem::zeroed();
                if GetTokenInformation(token, TokenLinkedToken, &mut linked as *mut _ as _,
                                       std::mem::size_of::<TOKEN_LINKED_TOKEN>() as u32, &mut len) != 0
                    && !linked.LinkedToken.is_null()
                {
                    CloseHandle(token);
                    token = linked.LinkedToken;
                }
            }
            Ok(token)
        }
    }

    /// A process started in a user's session.
    pub struct Proc {
        pid: u32,
        handle: isize,
    }

    impl Proc {
        pub fn id(&self) -> u32 {
            self.pid
        }

        /// Some(exit code) once it has exited.
        pub fn try_wait(&mut self) -> Option<u32> {
            unsafe {
                if WaitForSingleObject(self.handle as HANDLE, 0) != 0 {
                    return None; // WAIT_TIMEOUT: still running
                }
                let mut code = 0u32;
                GetExitCodeProcess(self.handle as HANDLE, &mut code);
                Some(code)
            }
        }

        pub fn kill(&mut self) {
            unsafe {
                TerminateProcess(self.handle as HANDLE, 1);
            }
        }

        pub fn wait(&mut self) {
            unsafe {
                WaitForSingleObject(self.handle as HANDLE, 10_000);
            }
        }
    }

    impl Drop for Proc {
        fn drop(&mut self) {
            unsafe {
                CloseHandle(self.handle as HANDLE);
            }
        }
    }

    /// Start `cmdline` as the user signed in to `session`, on their desktop,
    /// in its own console window (`show`: an SW_* value).
    pub fn launch(session: u32, cmdline: &str, cwd: &Path, title: &str, show: u16) -> Result<Proc> {
        let token = user_token(session)?;
        unsafe {
            let mut env: *mut core::ffi::c_void = std::ptr::null_mut();
            if CreateEnvironmentBlock(&mut env, token, 0) == 0 {
                env = std::ptr::null_mut();
            }
            let mut desktop = wide("winsta0\\default");
            let mut title_w = wide(title);
            let mut si: STARTUPINFOW = std::mem::zeroed();
            si.cb = std::mem::size_of::<STARTUPINFOW>() as u32;
            si.lpDesktop = desktop.as_mut_ptr();
            si.lpTitle = title_w.as_mut_ptr();
            si.dwFlags = STARTF_USESHOWWINDOW;
            si.wShowWindow = show;
            let mut pi: PROCESS_INFORMATION = std::mem::zeroed();
            let mut cmd = wide(cmdline);
            let cwd_w = wide(&cwd.display().to_string());
            let ok = CreateProcessAsUserW(
                token,
                std::ptr::null(),
                cmd.as_mut_ptr(),
                std::ptr::null(),
                std::ptr::null(),
                0,
                CREATE_NEW_CONSOLE | CREATE_NEW_PROCESS_GROUP | CREATE_UNICODE_ENVIRONMENT,
                env,
                cwd_w.as_ptr(),
                &si,
                &mut pi,
            );
            let err = GetLastError();
            if !env.is_null() {
                DestroyEnvironmentBlock(env);
            }
            CloseHandle(token);
            if ok == 0 {
                bail!("could not start the process in session {session} (error {err})");
            }
            CloseHandle(pi.hThread);
            Ok(Proc { pid: pi.dwProcessId, handle: pi.hProcess as isize })
        }
    }
}

#[cfg(windows)]
pub use imp::{launch, sessions, Proc};

#[cfg(not(windows))]
pub fn sessions() -> Vec<Session> {
    vec![]
}

/// The session to run the bot in: `user`'s (active first, then a
/// disconnected RDP one), or with no user set, whoever is at the console.
pub fn find_session(user: Option<&str>) -> Option<Session> {
    let all = sessions();
    let mut hits: Vec<Session> = match user.map(account_name).filter(|u| !u.is_empty()) {
        Some(u) => all.into_iter().filter(|s| s.user.eq_ignore_ascii_case(u)).collect(),
        None => all,
    };
    hits.sort_by_key(|s| (s.state != "active", s.id));
    hits.into_iter().next()
}

/// Lock `session`'s desktop (after an automatic sign-in). Programs keep
/// running on a locked desktop.
#[cfg(windows)]
pub fn lock_session(session: u32) -> Result<()> {
    let sys = std::env::var("SystemRoot").unwrap_or_else(|_| "C:\\Windows".into());
    let p = launch(session, &format!("\"{sys}\\System32\\rundll32.exe\" user32.dll,LockWorkStation"),
                   Path::new(&sys), "lock", 0)?;
    drop(p);
    Ok(())
}

// ---- automatic sign-in -----------------------------------------------------------

const WINLOGON: &str = r"HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon";

#[derive(Debug, Clone, Serialize, Default)]
pub struct Autologon {
    /// AutoAdminLogon = 1 and a password is stored.
    pub enabled: bool,
    /// "DOMAIN\\user" as Winlogon has it.
    pub account: Option<String>,
    pub password_stored: bool,
    /// AutoAdminLogon = 1 but no password anywhere: Windows would stop at the
    /// sign-in screen.
    pub broken: bool,
}

fn reg_get(name: &str) -> Option<String> {
    let out = hidden("reg.exe").args(["query", WINLOGON, "/v", name]).output().ok()?;
    if !out.status.success() {
        return None;
    }
    let text = String::from_utf8_lossy(&out.stdout).to_string();
    text.lines()
        .find(|l| l.trim_start().starts_with(name))
        .and_then(|l| l.split("REG_SZ").nth(1))
        .map(|v| v.trim().to_string())
}

fn reg_set(name: &str, value: &str) -> Result<()> {
    let out = hidden("reg.exe").args(["add", WINLOGON, "/v", name, "/t", "REG_SZ", "/d", value, "/f"])
        .output().context("running reg.exe")?;
    if !out.status.success() {
        bail!("could not set {name}: {}", String::from_utf8_lossy(&out.stderr).trim());
    }
    Ok(())
}

fn reg_delete(name: &str) {
    let _ = hidden("reg.exe").args(["delete", WINLOGON, "/v", name, "/f"]).output();
}

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

pub fn autologon_status() -> Autologon {
    let on = reg_get("AutoAdminLogon").map(|v| v == "1").unwrap_or(false);
    let user = reg_get("DefaultUserName").filter(|u| !u.is_empty());
    let domain = reg_get("DefaultDomainName").filter(|d| !d.is_empty());
    let stored = lsa::has_secret() || reg_get("DefaultPassword").map(|p| !p.is_empty()).unwrap_or(false);
    Autologon {
        enabled: on && stored && user.is_some(),
        account: user.map(|u| match domain {
            Some(d) => format!("{d}\\{u}"),
            None => u,
        }),
        password_stored: stored,
        broken: on && !stored,
    }
}

/// Sign `account` in automatically at boot. The password is checked with
/// Windows first (a wrong one would leave the box at the sign-in screen after
/// the next reboot -- exactly when nobody is there to notice).
pub fn enable_autologon(account: &str, password: &str) -> Result<()> {
    let account = account.trim();
    if account.is_empty() {
        bail!("no account");
    }
    let (domain, user) = match account.rsplit_once('\\') {
        Some((".", u)) | Some(("", u)) => (std::env::var("COMPUTERNAME").unwrap_or_default(), u.to_string()),
        Some((d, u)) => (d.to_string(), u.to_string()),
        None => (std::env::var("COMPUTERNAME").unwrap_or_default(), account.to_string()),
    };
    if password.is_empty() {
        bail!("Windows can't sign in automatically without the account's password");
    }
    lsa::check_password(&user, &domain, password)?;
    lsa::store_secret(Some(password))?;
    reg_delete("DefaultPassword"); // never leave a plain-text copy
    reg_delete("AutoLogonCount"); // a count would switch it off again
    reg_set("DefaultUserName", &user)?;
    reg_set("DefaultDomainName", &domain)?;
    reg_set("AutoAdminLogon", "1")?;
    Ok(())
}

pub fn disable_autologon() -> Result<()> {
    reg_set("AutoAdminLogon", "0")?;
    reg_delete("DefaultPassword");
    lsa::store_secret(None)?;
    Ok(())
}

#[cfg(windows)]
mod lsa {
    use super::imp::wide;
    use anyhow::{bail, Result};
    use windows_sys::Win32::Foundation::{CloseHandle, GetLastError, HANDLE};
    use windows_sys::Win32::Security::Authentication::Identity::{
        LsaClose, LsaFreeMemory, LsaNtStatusToWinError, LsaOpenPolicy, LsaRetrievePrivateData,
        LsaStorePrivateData, LSA_OBJECT_ATTRIBUTES, LSA_UNICODE_STRING, POLICY_CREATE_SECRET,
        POLICY_GET_PRIVATE_INFORMATION,
    };
    use windows_sys::Win32::Security::{LogonUserW, LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT};

    fn us(buf: &mut [u16]) -> LSA_UNICODE_STRING {
        let n = (buf.len() * 2) as u16;
        LSA_UNICODE_STRING { Length: n, MaximumLength: n, Buffer: buf.as_mut_ptr() }
    }

    fn open(access: i32) -> Result<isize> {
        let mut attrs: LSA_OBJECT_ATTRIBUTES = unsafe { std::mem::zeroed() };
        attrs.Length = std::mem::size_of::<LSA_OBJECT_ATTRIBUTES>() as u32;
        let mut policy = 0isize;
        let st = unsafe { LsaOpenPolicy(std::ptr::null(), &attrs, access as u32, &mut policy as *mut _ as _) };
        if st != 0 {
            bail!("LsaOpenPolicy failed (error {}) -- run as administrator", unsafe { LsaNtStatusToWinError(st) });
        }
        Ok(policy)
    }

    fn key() -> Vec<u16> {
        let mut k = wide("DefaultPassword");
        k.pop();
        k
    }

    pub fn store_secret(password: Option<&str>) -> Result<()> {
        let policy = open(POLICY_CREATE_SECRET)?;
        let mut k = key();
        let key_us = us(&mut k);
        let mut pw: Vec<u16> = password.map(|p| p.encode_utf16().collect()).unwrap_or_default();
        let pw_us = us(&mut pw);
        let st = unsafe {
            LsaStorePrivateData(policy as _, &key_us, if password.is_some() { &pw_us } else { std::ptr::null() })
        };
        pw.iter_mut().for_each(|c| *c = 0);
        unsafe { LsaClose(policy as _) };
        // deleting a secret that isn't there is fine
        if st != 0 && password.is_some() {
            bail!("could not store the password (error {})", unsafe { LsaNtStatusToWinError(st) });
        }
        Ok(())
    }

    pub fn has_secret() -> bool {
        let Ok(policy) = open(POLICY_GET_PRIVATE_INFORMATION) else { return false };
        let mut k = key();
        let key_us = us(&mut k);
        let mut data: *mut LSA_UNICODE_STRING = std::ptr::null_mut();
        let st = unsafe { LsaRetrievePrivateData(policy as _, &key_us, &mut data) };
        let found = st == 0 && !data.is_null() && unsafe { (*data).Length } > 0;
        if !data.is_null() {
            unsafe {
                let d = &*data;
                if !d.Buffer.is_null() {
                    std::ptr::write_bytes(d.Buffer, 0, d.Length as usize / 2);
                }
                LsaFreeMemory(data as _);
            }
        }
        unsafe { LsaClose(policy as _) };
        found
    }

    /// Reject a wrong user/password now rather than at the next reboot. Only
    /// "wrong user name or password" fails: other refusals (a logon type
    /// policy, say) don't mean Winlogon can't sign in.
    pub fn check_password(user: &str, domain: &str, password: &str) -> Result<()> {
        let u = wide(user);
        let d = wide(domain);
        let mut p = wide(password);
        let mut token: HANDLE = std::ptr::null_mut();
        let ok = unsafe {
            LogonUserW(u.as_ptr(), d.as_ptr(), p.as_ptr(), LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT, &mut token)
        };
        let err = unsafe { GetLastError() };
        p.iter_mut().for_each(|c| *c = 0);
        if ok != 0 {
            unsafe { CloseHandle(token) };
            return Ok(());
        }
        if err == 1326 {
            bail!("Windows says the user name or password is wrong for {domain}\\{user}");
        }
        Ok(())
    }
}

#[cfg(not(windows))]
mod lsa {
    use anyhow::{bail, Result};
    pub fn store_secret(_p: Option<&str>) -> Result<()> {
        bail!("Windows only")
    }
    pub fn has_secret() -> bool {
        false
    }
    pub fn check_password(_u: &str, _d: &str, _p: &str) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn account_names() {
        assert_eq!(account_name(".\\ATPAdmin"), "ATPAdmin");
        assert_eq!(account_name("DESKTOP-1\\ATPAdmin"), "ATPAdmin");
        assert_eq!(account_name("ATPAdmin"), "ATPAdmin");
    }
}
