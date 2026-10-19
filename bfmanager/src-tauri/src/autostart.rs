//! Start the desktop app when Windows starts (an administrator signs in).
//!
//! The service already runs from boot; this is the window app, so the tray
//! icon and the OPS page are there after a reboot without anyone opening it.
//! It starts with `--tray`: straight into the tray, no window.
//!
//! The app's manifest says requireAdministrator, and Windows never raises a
//! UAC prompt for a Run-key entry at sign-in -- it just doesn't start it. A
//! Task Scheduler task with "run with highest privileges" does start it
//! elevated, with no prompt, so that is what this registers: a logon trigger
//! for any member of Administrators (S-1-5-32-544), run in that user's own
//! session. Automatic sign-in (Setup step 4) signs that user in after every
//! reboot, so the app comes back by itself too.

use anyhow::{bail, Context, Result};
use serde::Serialize;

pub const TASK_NAME: &str = "Fowl Engine Manager";
/// The argument the logon task starts the app with.
pub const TRAY_ARG: &str = "--tray";

#[derive(Debug, Clone, Serialize, Default)]
pub struct AutostartStatus {
    /// The logon task exists.
    pub installed: bool,
    /// ...and starts this exe (an install moved elsewhere leaves a stale one).
    pub current: bool,
}

pub(crate) fn xml_escape(s: &str) -> String {
    s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;").replace('"', "&quot;")
}

/// The task definition. Settings worth spelling out: no time limit (the
/// default kills it after 3 days), runs on battery, one copy only.
pub fn task_xml(exe: &str) -> String {
    format!(
        r#"<?xml version="1.0" encoding="UTF-16"?>
<Task version="1.2" xmlns="http://schemas.microsoft.com/windows/2004/02/mit/task">
  <RegistrationInfo>
    <Author>Fowl Engine Manager</Author>
    <Description>Starts Fowl Engine Manager in the tray when an administrator signs in. The FowlEngine service runs the server either way; turn this off in the manager's Settings.</Description>
  </RegistrationInfo>
  <Triggers>
    <LogonTrigger>
      <Enabled>true</Enabled>
      <Delay>PT10S</Delay>
    </LogonTrigger>
  </Triggers>
  <Principals>
    <Principal id="Author">
      <GroupId>S-1-5-32-544</GroupId>
      <RunLevel>HighestAvailable</RunLevel>
    </Principal>
  </Principals>
  <Settings>
    <MultipleInstancesPolicy>IgnoreNew</MultipleInstancesPolicy>
    <DisallowStartIfOnBatteries>false</DisallowStartIfOnBatteries>
    <StopIfGoingOnBatteries>false</StopIfGoingOnBatteries>
    <AllowHardTerminate>false</AllowHardTerminate>
    <StartWhenAvailable>false</StartWhenAvailable>
    <RunOnlyIfNetworkAvailable>false</RunOnlyIfNetworkAvailable>
    <IdleSettings>
      <StopOnIdleEnd>false</StopOnIdleEnd>
      <RestartOnIdle>false</RestartOnIdle>
    </IdleSettings>
    <AllowStartOnDemand>true</AllowStartOnDemand>
    <Enabled>true</Enabled>
    <Hidden>false</Hidden>
    <RunOnlyIfIdle>false</RunOnlyIfIdle>
    <ExecutionTimeLimit>PT0S</ExecutionTimeLimit>
    <Priority>7</Priority>
  </Settings>
  <Actions Context="Author">
    <Exec>
      <Command>"{exe}"</Command>
      <Arguments>{arg}</Arguments>
    </Exec>
  </Actions>
</Task>
"#,
        exe = xml_escape(exe),
        arg = TRAY_ARG,
    )
}

#[cfg(windows)]
fn schtasks(args: &[&str]) -> Result<std::process::Output> {
    use std::os::windows::process::CommandExt;
    std::process::Command::new("schtasks.exe")
        .args(args)
        .creation_flags(0x0800_0000) // CREATE_NO_WINDOW
        .output()
        .context("running schtasks.exe")
}

fn this_exe() -> Result<String> {
    Ok(std::env::current_exe().context("finding this exe")?.display().to_string())
}

#[cfg(windows)]
pub fn status() -> AutostartStatus {
    let Ok(out) = schtasks(&["/Query", "/TN", TASK_NAME, "/XML"]) else {
        return AutostartStatus::default();
    };
    if !out.status.success() {
        return AutostartStatus::default();
    }
    let xml = String::from_utf8_lossy(&out.stdout).to_lowercase();
    let current = this_exe()
        .map(|exe| xml.contains(&xml_escape(&exe).to_lowercase()))
        .unwrap_or(false);
    AutostartStatus { installed: true, current }
}

#[cfg(windows)]
pub fn enable() -> Result<()> {
    let exe = this_exe()?;
    // schtasks /XML reads UTF-16 LE with a BOM reliably (UTF-8 depends on
    // the Windows version)
    let mut bytes = vec![0xFF, 0xFE];
    for u in task_xml(&exe).encode_utf16() {
        bytes.extend_from_slice(&u.to_le_bytes());
    }
    crate::config::ensure_dirs()?;
    let tmp = crate::config::data_dir().join("autostart-task.xml");
    std::fs::write(&tmp, bytes).with_context(|| format!("writing {}", tmp.display()))?;
    let path = tmp.display().to_string();
    let out = schtasks(&["/Create", "/TN", TASK_NAME, "/XML", &path, "/F"]);
    let _ = std::fs::remove_file(&tmp);
    let out = out?;
    if !out.status.success() {
        bail!(
            "schtasks /Create failed: {}{}",
            String::from_utf8_lossy(&out.stderr).trim(),
            String::from_utf8_lossy(&out.stdout).trim()
        );
    }
    Ok(())
}

#[cfg(windows)]
pub fn disable() -> Result<()> {
    if !status().installed {
        return Ok(());
    }
    let out = schtasks(&["/Delete", "/TN", TASK_NAME, "/F"])?;
    if !out.status.success() {
        bail!("schtasks /Delete failed: {}", String::from_utf8_lossy(&out.stderr).trim());
    }
    Ok(())
}

#[cfg(not(windows))]
pub fn status() -> AutostartStatus {
    AutostartStatus::default()
}
#[cfg(not(windows))]
pub fn enable() -> Result<()> {
    Ok(())
}
#[cfg(not(windows))]
pub fn disable() -> Result<()> {
    Ok(())
}

/// Make the logon task match the setting: there and pointing at this exe
/// when it is on, gone when it is off. Debug builds leave it alone (a
/// `tauri dev` run would point it at target\debug).
pub fn sync(want: bool) -> Result<()> {
    if cfg!(debug_assertions) {
        return Ok(());
    }
    let st = status();
    match (want, st.installed, st.current) {
        (true, true, true) | (false, false, _) => Ok(()),
        (true, _, _) => enable(),
        (false, true, _) => disable(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn xml_has_the_exe_and_the_tray_arg() {
        let x = task_xml(r"C:\Program Files\Fowl & Co\FowlEngineManager.exe");
        assert!(x.contains(r#"<Command>"C:\Program Files\Fowl &amp; Co\FowlEngineManager.exe"</Command>"#));
        assert!(x.contains("<Arguments>--tray</Arguments>"));
        assert!(x.contains("<RunLevel>HighestAvailable</RunLevel>"));
        assert!(x.contains("<ExecutionTimeLimit>PT0S</ExecutionTimeLimit>"));
    }
}
