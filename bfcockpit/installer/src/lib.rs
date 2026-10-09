//! Finding DCS profiles and putting the overlay in them.
//!
//! The overlay is a client-side DCS plugin: one Lua file in a DCS profile's
//! `Scripts\Hooks` folder, which is the engine's supported extension point
//! for code that runs on the player's own machine. Installing it by hand is
//! three steps and two folders that often don't exist yet, and players
//! routinely have several DCS profiles -- stable and Open Beta, a server
//! profile, a `--write-dir` profile for VR settings. Putting the script in one
//! of them and then wondering why it didn't load is the failure this exists to
//! prevent, so it finds every profile and does all of them.
//!
//! The script is embedded here with `include_str!`, so the installer is a
//! single file with nothing to unzip and nothing to keep beside it.
//!
//! Nothing outside a DCS profile is touched: no DCS install files, no registry
//! writes, nothing added to startup. In particular it does NOT touch
//! `DCS World\Scripts\MissionScripting.lua` -- that sanitizes the *mission
//! scripting* state, which is a server-side concern and has no bearing on a
//! Hooks script running on a client.
//!
//! Both front ends (the window and the command line) sit on top of this.
use anyhow::{Context, Result};
use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
    process::Command,
};

/// The plugin itself, baked in.
pub const HOOK_LUA: &str = include_str!("../../Scripts/Hooks/bfcockpit.lua");
pub const HOOK_NAME: &str = "bfcockpit.lua";
pub const DEFAULT_URL: &str = "https://api.vectorstrike.org/cockpit";

/// A DCS profile and what is installed in it.
#[derive(Debug, Clone)]
pub struct Profile {
    pub path: PathBuf,
    /// Version of the overlay already there, if any.
    pub installed: Option<String>,
}

impl Profile {
    pub fn name(&self) -> String {
        self.path
            .file_name()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| self.path.display().to_string())
    }

    /// Short status for a list: what is there versus what we ship.
    pub fn status(&self) -> String {
        match &self.installed {
            None => "not installed".into(),
            Some(v) if v == plugin_version() => "already up to date".into(),
            Some(v) => format!("{v} installed"),
        }
    }
}

/// What a scan turned up.
#[derive(Debug, Clone, Default)]
pub struct Scan {
    pub profiles: Vec<Profile>,
    /// Folders that look like DCS but have no `Config` -- named so that "it
    /// didn't find my profile" has an answer.
    pub near_misses: Vec<PathBuf>,
    /// Where we looked.
    pub roots: Vec<PathBuf>,
}

/// Find every DCS profile on this machine.
pub fn scan(extra_roots: &[PathBuf]) -> Scan {
    let (paths, near_misses) = discover(extra_roots);
    Scan {
        profiles: paths
            .into_iter()
            .map(|p| Profile { installed: installed_version(&p), path: p })
            .collect(),
        near_misses,
        roots: search_roots(extra_roots),
    }
}

/// Treat these specific folders as profiles, without searching.
pub fn profiles_at(paths: &[PathBuf]) -> (Vec<Profile>, Vec<PathBuf>) {
    let mut found = Vec::new();
    let mut missing = Vec::new();
    for p in paths {
        if p.is_dir() {
            found.push(Profile { installed: installed_version(p), path: p.clone() });
        } else {
            missing.push(p.clone());
        }
    }
    (found, missing)
}

/// Version declared by the embedded script. Single source of truth -- bfdb
/// parses the same line out of the same file.
pub fn plugin_version() -> &'static str {
    HOOK_LUA
        .lines()
        .find_map(|l| {
            let rest = l.trim().strip_prefix("local BFCOCKPIT_VERSION")?;
            let rest = rest.trim_start().strip_prefix('=')?;
            rest.trim().trim_start_matches('"').split('"').next()
        })
        .unwrap_or("unknown")
}

pub fn installed_version(profile: &Path) -> Option<String> {
    let text = fs::read_to_string(profile.join("Scripts").join("Hooks").join(HOOK_NAME)).ok()?;
    text.lines().find_map(|l| {
        let rest = l.trim().strip_prefix("local BFCOCKPIT_VERSION")?;
        let rest = rest.trim_start().strip_prefix('=')?;
        Some(rest.trim().trim_start_matches('"').split('"').next()?.to_string())
    })
}

/// Everywhere a DCS profile might live.
///
/// `Saved Games` is a Windows known folder and people really do move it --
/// onto another drive, or into OneDrive, in which case `%USERPROFILE%\Saved
/// Games` is not where it is. The authoritative answer is the shell's own
/// record of it, read here from the registry rather than by taking a
/// dependency on the Win32 shell API for one string.
pub fn search_roots(extra: &[PathBuf]) -> Vec<PathBuf> {
    let mut roots: Vec<PathBuf> = Vec::new();
    let mut push = |p: PathBuf| {
        if p.is_dir() && !roots.contains(&p) {
            roots.push(p);
        }
    };

    if let Some(p) = known_saved_games() {
        push(p);
    }
    if let Ok(profile) = std::env::var("USERPROFILE") {
        push(Path::new(&profile).join("Saved Games"));
        push(Path::new(&profile).join("OneDrive").join("Saved Games"));
        push(Path::new(&profile).join("Documents").join("Saved Games"));
    }
    if let Ok(od) = std::env::var("OneDrive") {
        push(Path::new(&od).join("Saved Games"));
    }
    for e in extra {
        push(e.clone());
    }
    roots
}

/// FOLDERID_SavedGames, via the shell's User Shell Folders record.
fn known_saved_games() -> Option<PathBuf> {
    const FOLDERID_SAVED_GAMES: &str = "{4C5C32FF-BB9D-43B0-B5B4-2D72E54EAAA4}";
    let out = Command::new("reg")
        .args([
            "query",
            r"HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders",
            "/v",
            FOLDERID_SAVED_GAMES,
        ])
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    // One line, three whitespace-separated fields:
    //     {GUID}    REG_SZ           E:\Saved Games
    //     {GUID}    REG_EXPAND_SZ    %USERPROFILE%\Saved Games
    // The value is everything after the type token, and must be taken whole --
    // splitting on whitespace truncates "E:\Saved Games" at the space.
    let text = String::from_utf8_lossy(&out.stdout);
    let line = text.lines().find(|l| l.contains(FOLDERID_SAVED_GAMES))?;
    let type_tok = line.split_whitespace().find(|t| t.starts_with("REG_"))?;
    let after = line.find(type_tok)? + type_tok.len();
    let value = line[after..].trim();
    if value.is_empty() {
        return None;
    }
    Some(PathBuf::from(expand_env(value)))
}

/// Last resort when the registry lookup fails and the usual spots are empty:
/// a relocated `Saved Games` almost always ends up at the root of another
/// drive. Only reached when nothing else turned anything up, so the cost of
/// poking at drive letters is paid once and only when we are otherwise stuck.
fn sweep_drives_for_saved_games() -> Vec<PathBuf> {
    let mut out = Vec::new();
    for letter in b'A'..=b'Z' {
        let p = PathBuf::from(format!("{}:\\Saved Games", letter as char));
        if p.is_dir() {
            out.push(p);
        }
    }
    out
}

/// Expand `%VAR%` the way REG_EXPAND_SZ means it.
fn expand_env(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut rest = s;
    while let Some(start) = rest.find('%') {
        out.push_str(&rest[..start]);
        let after = &rest[start + 1..];
        match after.find('%') {
            Some(end) => {
                let name = &after[..end];
                match std::env::var(name) {
                    Ok(v) => out.push_str(&v),
                    // Not a variable we know -- leave it as written rather
                    // than silently deleting part of the path.
                    Err(_) => {
                        out.push('%');
                        out.push_str(name);
                        out.push('%');
                    }
                }
                rest = &after[end + 1..];
            }
            None => {
                out.push('%');
                out.push_str(after);
                rest = "";
            }
        }
    }
    out.push_str(rest);
    out
}

/// A DCS profile is a `DCS*` folder with a `Config` directory in it. Keying on
/// `Config` rather than on the exact name finds `DCS`, `DCS.openbeta`,
/// `DCS.server`, `DCS.vectorstrike_1` and whatever else someone has made,
/// without mistaking an unrelated Saved Games entry for one.
///
/// Requiring `Config` matters: a Saved Games folder is full of `DCS*` entries
/// that are not profiles at all -- `DCS_F14`, `DCS_F4E`, `DCS.C130J` and
/// friends are per-module data folders written by third-party aircraft, and
/// `DCS MODS` is a mod manager's. Installing a Hooks script into any of them
/// would do nothing at all, silently.
fn discover(extra_roots: &[PathBuf]) -> (Vec<PathBuf>, Vec<PathBuf>) {
    let mut found: BTreeSet<PathBuf> = BTreeSet::new();
    let mut near_misses: BTreeSet<PathBuf> = BTreeSet::new();

    let mut roots = search_roots(extra_roots);
    let scan = |roots: &[PathBuf], found: &mut BTreeSet<PathBuf>, misses: &mut BTreeSet<PathBuf>| {
        for root in roots {
            let Ok(entries) = fs::read_dir(root) else { continue };
            for e in entries.flatten() {
                let path = e.path();
                if !path.is_dir() {
                    continue;
                }
                let name = e.file_name().to_string_lossy().into_owned();
                if !name.starts_with("DCS") {
                    continue;
                }
                if path.join("Config").is_dir() {
                    found.insert(path);
                } else if path.join("Scripts").is_dir() || path.join("Logs").is_dir() {
                    // Looks profile-ish but has no Config -- usually a folder a
                    // mod manager left behind. Worth naming so "it didn't find
                    // my profile" has an answer.
                    misses.insert(path);
                }
            }
        }
    };

    scan(&roots, &mut found, &mut near_misses);

    if found.is_empty() {
        let swept = sweep_drives_for_saved_games();
        if !swept.is_empty() {
            roots.extend(swept);
            scan(&roots, &mut found, &mut near_misses);
        }
    }

    (found.into_iter().collect(), near_misses.into_iter().collect())
}

pub fn install_one(profile: &Path, url: &str) -> Result<Option<String>> {
    let hooks = profile.join("Scripts").join("Hooks");
    fs::create_dir_all(&hooks)
        .with_context(|| format!("creating {}", hooks.display()))?;

    let dest = hooks.join(HOOK_NAME);
    let mut note = String::new();
    if dest.is_file() {
        // Keep one backup, so a bad update is recoverable without a download.
        let backup = dest.with_extension("lua.bak");
        fs::copy(&dest, &backup).with_context(|| format!("backing up {}", dest.display()))?;
        note.push_str(" (previous copy kept as bfcockpit.lua.bak)");
    }
    fs::write(&dest, HOOK_LUA).with_context(|| format!("writing {}", dest.display()))?;

    // Seed settings only when there is no file yet: the script writes its own
    // defaults on first run, but doing it here means a player given a
    // non-default server URL never has to edit anything. An existing file --
    // their keys, opacity, window position -- is never touched.
    let cfg = profile.join("Config").join("BFCockpit.lua");
    if !cfg.exists() && url != DEFAULT_URL {
        if let Some(dir) = cfg.parent() {
            fs::create_dir_all(dir).ok();
        }
        let escaped = url.replace('\\', "\\\\").replace('"', "\\\"");
        let body = format!(
            "-- BFNext cockpit overlay settings.\n\
             -- Written by the installer; safe to hand-edit. Every option is\n\
             -- documented in the header of Scripts/Hooks/bfcockpit.lua.\n\
             -- Window position, size and opacity are updated automatically.\n\n\
             cockpit = {{\n    [\"url\"] = \"{escaped}\",\n}}\n"
        );
        if fs::write(&cfg, body).is_ok() {
            note.push_str(" (settings seeded)");
        }
    }

    Ok(Some(format!("installed{note}")))
}

pub fn uninstall_one(profile: &Path) -> Result<Option<String>> {
    let dest = profile.join("Scripts").join("Hooks").join(HOOK_NAME);
    if !dest.is_file() {
        return Ok(None);
    }
    fs::remove_file(&dest).with_context(|| format!("removing {}", dest.display()))?;
    Ok(Some("removed".into()))
}
