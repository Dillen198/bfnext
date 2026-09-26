//! DCSServerBot: finding it, keeping the Fowl Engine plugin inside it current,
//! and reaching the plugin's OPS API.

use crate::config::{backups_dir, bundled_bot_dir, data_dir};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::io::Write;
use std::path::{Path, PathBuf};

/// A folder is a DCSServerBot install if it has the launcher and the core.
pub fn is_bot_dir(p: &Path) -> bool {
    p.join("run.py").is_file() && p.join("core").is_dir() && p.join("plugins").is_dir()
}

/// Likely DCSServerBot folders on this PC, best guesses first.
pub fn detect_bot_dirs() -> Vec<String> {
    let mut cands: Vec<PathBuf> = Vec::new();
    if let Some(home) = std::env::var_os("USERPROFILE").map(PathBuf::from) {
        cands.push(home.join("DCSServerBot"));
        cands.push(home.join("Documents").join("DCSServerBot"));
    }
    for drive in 'C'..='H' {
        let root = PathBuf::from(format!("{drive}:\\"));
        if !root.exists() {
            continue;
        }
        for sub in ["DCSServerBot", "Github\\DCSServerBot", "GitHub\\DCSServerBot", "Servers\\DCSServerBot",
                    "Program Files\\DCSServerBot", "DCS\\DCSServerBot"] {
            cands.push(root.join(sub));
        }
        // one level of anything called DCSServerBot*
        if let Ok(rd) = std::fs::read_dir(&root) {
            for e in rd.flatten() {
                let name = e.file_name().to_string_lossy().to_lowercase();
                if name.starts_with("dcsserverbot") {
                    cands.push(e.path());
                }
            }
        }
    }
    let mut out = Vec::new();
    for c in cands {
        if is_bot_dir(&c) {
            let s = c.display().to_string();
            if !out.contains(&s) {
                out.push(s);
            }
        }
    }
    out
}

/// A Windows user that looks like the one DCSServerBot was set up as.
#[derive(Debug, Clone, Serialize)]
pub struct BotAccount {
    /// ".\\name" -- what the service install wants
    pub account: String,
    /// DCSServerBot's run.cmd keeps its Python venv in %USERPROFILE%\.dcssb
    pub has_venv: bool,
    /// Saved Games\DCS* folders (the DCS server instances' write dirs)
    pub dcs_instances: Vec<String>,
}

/// Local user profiles with DCSServerBot's venv and/or DCS instance folders,
/// best match first. The service must run as one of these: Python, the venv
/// and the DCS profiles all live under that user.
pub fn detect_bot_accounts() -> Vec<BotAccount> {
    let drive = std::env::var("SystemDrive").unwrap_or_else(|_| "C:".into());
    let users = PathBuf::from(format!("{drive}\\Users"));
    let mut out = Vec::new();
    let Ok(rd) = std::fs::read_dir(&users) else { return out };
    for e in rd.flatten() {
        let name = e.file_name().to_string_lossy().to_string();
        if ["Public", "Default", "Default User", "All Users"].contains(&name.as_str()) || !e.path().is_dir() {
            continue;
        }
        let has_venv = e.path().join(".dcssb").join("Scripts").join("python.exe").is_file();
        let mut dcs_instances = Vec::new();
        if let Ok(sg) = std::fs::read_dir(e.path().join("Saved Games")) {
            for g in sg.flatten() {
                let n = g.file_name().to_string_lossy().to_string();
                if n.to_lowercase().starts_with("dcs") && g.path().is_dir() {
                    dcs_instances.push(n);
                }
            }
        }
        if has_venv || !dcs_instances.is_empty() {
            out.push(BotAccount { account: format!(".\\{name}"), has_venv, dcs_instances });
        }
    }
    out.sort_by_key(|a| (!a.has_venv, std::cmp::Reverse(a.dcs_instances.len())));
    out
}

// ---- plugin sync -----------------------------------------------------------------

/// scripts/stage-bot.mjs writes this into the bundled `bot\` folder.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct BundleManifest {
    pub version: String,
    #[serde(default)]
    pub git: Option<String>,
    /// path relative to the bot folder, '/'-separated -> sha256
    pub files: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SyncReport {
    pub at: String,
    pub bundle_version: String,
    pub changed: Vec<String>,
    pub skipped_reason: Option<String>,
    pub backup: Option<String>,
}

pub fn bundle_manifest() -> Option<BundleManifest> {
    let p = bundled_bot_dir().join("manifest.json");
    std::fs::read_to_string(p).ok().and_then(|s| serde_json::from_str(&s).ok())
}

fn sha256_file(p: &Path) -> Option<String> {
    let bytes = std::fs::read(p).ok()?;
    Some(hex::encode(Sha256::digest(&bytes)))
}

/// Files of the bundle that differ from what's in the bot folder.
pub fn plugin_diff(bot_dir: &Path, m: &BundleManifest) -> Vec<String> {
    m.files
        .iter()
        .filter(|(rel, sha)| sha256_file(&bot_dir.join(rel)).as_deref() != Some(sha.as_str()))
        .map(|(rel, _)| rel.clone())
        .collect()
}

/// A plugin folder that is a link into a development checkout must not be
/// overwritten by an installed build.
pub fn linked_plugin(bot_dir: &Path) -> Option<String> {
    let p = bot_dir.join("plugins").join("fowlengine");
    let meta = std::fs::symlink_metadata(&p).ok()?;
    if meta.file_type().is_symlink() {
        return Some(format!("{} is a link (a development checkout) -- not overwriting it", p.display()));
    }
    let (Ok(real), Ok(base)) = (std::fs::canonicalize(&p), std::fs::canonicalize(bot_dir)) else {
        return None;
    };
    if !real.starts_with(&base) {
        return Some(format!("{} points outside the bot folder -- not overwriting it", p.display()));
    }
    None
}

/// Copy every changed bundled file into the bot folder, after zipping the
/// files it replaces into backups\. Only ever called with the bot stopped.
pub fn sync_plugin(bot_dir: &Path) -> Result<SyncReport> {
    let mut rep = SyncReport { at: chrono::Utc::now().to_rfc3339(), ..Default::default() };
    let Some(m) = bundle_manifest() else {
        rep.skipped_reason = Some("this build carries no bot plugin bundle".into());
        return Ok(rep);
    };
    rep.bundle_version = m.version.clone();
    if !is_bot_dir(bot_dir) {
        bail!("{} is not a DCSServerBot folder", bot_dir.display());
    }
    if let Some(why) = linked_plugin(bot_dir) {
        rep.skipped_reason = Some(why);
        return Ok(rep);
    }
    let changed = plugin_diff(bot_dir, &m);
    if changed.is_empty() {
        return Ok(rep);
    }
    // back up what we are about to replace
    let stamp = chrono::Local::now().format("%Y%m%d-%H%M%S");
    let backup = backups_dir().join(format!("plugin-{stamp}.zip"));
    std::fs::create_dir_all(backups_dir())?;
    {
        let f = std::fs::File::create(&backup)?;
        let mut z = zip::ZipWriter::new(f);
        let opts: zip::write::SimpleFileOptions =
            zip::write::SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);
        for rel in &changed {
            let cur = bot_dir.join(rel);
            if let Ok(bytes) = std::fs::read(&cur) {
                z.start_file(rel.as_str(), opts)?;
                z.write_all(&bytes)?;
            }
        }
        z.finish()?;
    }
    prune_backups(10);
    let src_root = bundled_bot_dir();
    for rel in &changed {
        let src = src_root.join(rel);
        let dst = bot_dir.join(rel);
        if let Some(parent) = dst.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let tmp = dst.with_extension("fowl-new");
        std::fs::copy(&src, &tmp).with_context(|| format!("copying {}", src.display()))?;
        std::fs::rename(&tmp, &dst).with_context(|| format!("replacing {}", dst.display()))?;
    }
    rep.changed = changed;
    rep.backup = Some(backup.display().to_string());
    let _ = std::fs::write(data_dir().join("plugin-sync.json"), serde_json::to_vec_pretty(&rep)?);
    Ok(rep)
}

fn prune_backups(keep: usize) {
    let Ok(rd) = std::fs::read_dir(backups_dir()) else { return };
    let mut zips: Vec<PathBuf> = rd
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.file_name().map(|n| n.to_string_lossy().starts_with("plugin-")).unwrap_or(false))
        .collect();
    zips.sort();
    while zips.len() > keep {
        let _ = std::fs::remove_file(zips.remove(0));
    }
}

pub fn last_sync() -> Option<SyncReport> {
    std::fs::read_to_string(data_dir().join("plugin-sync.json"))
        .ok()
        .and_then(|s| serde_json::from_str(&s).ok())
}

// ---- the plugin's OPS API -------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct OpsTarget {
    pub base: String,
    #[serde(skip)]
    pub key: String,
}

fn yaml_default(path: &Path) -> Option<serde_yaml::Value> {
    let text = std::fs::read_to_string(path).ok()?;
    let doc: serde_yaml::Value = serde_yaml::from_str(&text).ok()?;
    doc.get("DEFAULT").cloned()
}

fn ystr<'a>(v: &'a serde_yaml::Value, path: &[&str]) -> Option<&'a str> {
    let mut cur = v;
    for k in path {
        cur = cur.get(*k)?;
    }
    cur.as_str().filter(|s| !s.trim().is_empty())
}

/// Where the FowlEngine plugin's OPS routes live and the key they want --
/// read from the bot's own config, the same way bfdb finds them.
pub fn ops_target(bot_dir: &Path) -> Result<OpsTarget> {
    let ws = yaml_default(&bot_dir.join("config").join("services").join("webservice.yaml"))
        .ok_or_else(|| anyhow!("DCSServerBot's WebService is not configured (config\\services\\webservice.yaml) \
                                -- the OPS view needs it"))?;
    let port = ws.get("port").and_then(|p| p.as_u64()).unwrap_or(9876);
    let fe = yaml_default(&bot_dir.join("config").join("plugins").join("fowlengine.yaml"))
        .ok_or_else(|| anyhow!("config\\plugins\\fowlengine.yaml not found or not valid YAML"))?;
    let key = ystr(&fe, &["ops_api", "api_key"])
        .or_else(|| ystr(&fe, &["bfdb", "dcsserverbot_api_key"]))
        .ok_or_else(|| anyhow!("fowlengine.yaml has no bfdb.dcsserverbot_api_key (or ops_api.api_key)"))?
        .to_string();
    let prefix = match ystr(&fe, &["ops_api", "prefix"]) {
        Some(p) => p.to_string(),
        None => ystr(&fe, &["bfdb", "dcsserverbot_url"])
            .and_then(|u| u.split_once("://").map(|(_, rest)| rest.to_string()))
            .and_then(|rest| rest.find('/').map(|i| rest[i..].to_string()))
            .unwrap_or_default(),
    };
    let prefix = format!("/{}", prefix.trim_matches('/'));
    let prefix = if prefix == "/" { String::new() } else { prefix };
    Ok(OpsTarget { base: format!("http://127.0.0.1:{port}{prefix}/fowlengine/ops"), key })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ops_target_from_bot_config() {
        let d = std::env::temp_dir().join(format!("fowl-bot-{}", std::process::id()));
        std::fs::create_dir_all(d.join("config/services")).unwrap();
        std::fs::create_dir_all(d.join("config/plugins")).unwrap();
        std::fs::write(d.join("config/services/webservice.yaml"), "DEFAULT:\n  listen: 0.0.0.0\n  port: 9999\n").unwrap();
        std::fs::write(d.join("config/plugins/fowlengine.yaml"),
            "DEFAULT:\n  bfdb:\n    dcsserverbot_url: \"http://127.0.0.1:9999/stats/\"\n    dcsserverbot_api_key: K\n").unwrap();
        let t = ops_target(&d).unwrap();
        assert_eq!(t.base, "http://127.0.0.1:9999/stats/fowlengine/ops");
        assert_eq!(t.key, "K");
        std::fs::write(d.join("config/plugins/fowlengine.yaml"),
            "DEFAULT:\n  ops_api:\n    api_key: Z\n    prefix: \"\"\n  bfdb:\n    dcsserverbot_url: http://h:1\n").unwrap();
        let t = ops_target(&d).unwrap();
        assert_eq!(t.base, "http://127.0.0.1:9999/fowlengine/ops");
        assert_eq!(t.key, "Z");
        let _ = std::fs::remove_dir_all(&d);
    }
}
