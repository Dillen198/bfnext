//! Fowl Engine Manager updating itself.
//!
//! Releases are GitHub releases tagged `manager-v<semver>` on the same repo as
//! the engine (whose `engine-*` releases the bot plugin handles). Each carries
//! the NSIS installer `..._x64-setup.exe` and its `.sig` -- a minisign
//! signature made with the private key only the publisher has
//! (bfmanager/release.ps1). The matching public key is compiled in here, so a
//! tampered or unsigned installer is never run, whoever serves it.
//!
//! Installing = running that installer: `/S /UPDATE` from the service
//! (silent, no uninstall of the old version), `/P /UPDATE /R` from the GUI
//! (progress bar, relaunch). The installer's hooks stop the service, replace
//! the files and start it again (nsis/hooks.nsh).

use crate::config::{updates_dir, ManagerConfig};
use anyhow::{anyhow, bail, Context, Result};
use base64::Engine;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// base64 of the minisign public key file (`tauri signer generate`).
pub const UPDATER_PUBKEY: &str = include_str!("../updater.pub");
pub const TAG_PREFIX: &str = "manager-v";

pub fn current_version() -> semver::Version {
    semver::Version::parse(env!("CARGO_PKG_VERSION")).expect("crate version")
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Release {
    pub tag: String,
    pub version: String,
    pub notes: String,
    pub published: Option<String>,
    pub html_url: Option<String>,
    pub prerelease: bool,
    pub setup_name: String,
    pub setup_url: String,
    pub setup_api_url: String,
    pub sig_url: String,
    pub sig_api_url: String,
    pub size: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CheckResult {
    pub current: String,
    pub latest: Option<Release>,
    pub update_available: bool,
    pub checked_at: String,
    pub error: Option<String>,
}

fn client(cfg: &ManagerConfig) -> Result<reqwest::blocking::Client> {
    let mut headers = reqwest::header::HeaderMap::new();
    headers.insert("User-Agent", "fowl-engine-manager".parse()?);
    headers.insert("Accept", "application/vnd.github+json".parse()?);
    if let Some(tok) = cfg.github_token.as_deref().filter(|t| !t.trim().is_empty()) {
        headers.insert("Authorization", format!("Bearer {}", tok.trim()).parse()?);
    }
    Ok(reqwest::blocking::Client::builder()
        .default_headers(headers)
        .timeout(Duration::from_secs(600))
        .connect_timeout(Duration::from_secs(20))
        .build()?)
}

/// Pick the newest usable manager release from GitHub's release list (newest first).
pub fn pick(releases: &serde_json::Value, channel: &str) -> Option<Release> {
    let mut best: Option<(semver::Version, Release)> = None;
    for r in releases.as_array()? {
        if r["draft"].as_bool().unwrap_or(false) {
            continue;
        }
        let tag = r["tag_name"].as_str().unwrap_or_default();
        let Some(ver) = tag.strip_prefix(TAG_PREFIX).and_then(|v| semver::Version::parse(v).ok()) else {
            continue;
        };
        let pre = r["prerelease"].as_bool().unwrap_or(false);
        if pre && channel != "beta" {
            continue;
        }
        let assets = r["assets"].as_array().cloned().unwrap_or_default();
        let find = |pred: &dyn Fn(&str) -> bool| {
            assets.iter().find(|a| pred(&a["name"].as_str().unwrap_or_default().to_lowercase())).cloned()
        };
        let Some(setup) = find(&|n| n.ends_with("-setup.exe")) else { continue };
        let setup_name = setup["name"].as_str().unwrap_or_default().to_string();
        let Some(sig) = find(&|n| n == format!("{}.sig", setup_name.to_lowercase())) else { continue };
        let rel = Release {
            tag: tag.to_string(),
            version: ver.to_string(),
            notes: r["body"].as_str().unwrap_or_default().chars().take(4000).collect(),
            published: r["published_at"].as_str().map(String::from),
            html_url: r["html_url"].as_str().map(String::from),
            prerelease: pre,
            setup_name,
            setup_url: setup["browser_download_url"].as_str().unwrap_or_default().to_string(),
            setup_api_url: setup["url"].as_str().unwrap_or_default().to_string(),
            sig_url: sig["browser_download_url"].as_str().unwrap_or_default().to_string(),
            sig_api_url: sig["url"].as_str().unwrap_or_default().to_string(),
            size: setup["size"].as_u64().unwrap_or(0),
        };
        if best.as_ref().map(|(v, _)| ver > *v).unwrap_or(true) {
            best = Some((ver, rel));
        }
    }
    best.map(|(_, r)| r)
}

pub fn check(cfg: &ManagerConfig) -> CheckResult {
    let mut res = CheckResult {
        current: current_version().to_string(),
        checked_at: chrono::Utc::now().to_rfc3339(),
        ..Default::default()
    };
    let run = || -> Result<Option<Release>> {
        let c = client(cfg)?;
        let url = format!("https://api.github.com/repos/{}/releases?per_page=40", cfg.repo.trim_matches('/'));
        let resp = c.get(&url).send()?;
        if !resp.status().is_success() {
            bail!("GitHub answered {} for {url}", resp.status());
        }
        let list: serde_json::Value = resp.json()?;
        Ok(pick(&list, &cfg.channel))
    };
    match run() {
        Ok(latest) => {
            res.update_available = latest
                .as_ref()
                .and_then(|r| semver::Version::parse(&r.version).ok())
                .map(|v| v > current_version())
                .unwrap_or(false);
            res.latest = latest;
        }
        Err(e) => res.error = Some(format!("{e:#}")),
    }
    res
}

fn get_bytes(c: &reqwest::blocking::Client, public: &str, api: &str, token: bool) -> Result<Vec<u8>> {
    let req = if token && !api.is_empty() {
        c.get(api).header("Accept", "application/octet-stream")
    } else {
        c.get(public)
    };
    let resp = req.send()?;
    if !resp.status().is_success() {
        bail!("download answered {}", resp.status());
    }
    let mut buf = Vec::new();
    resp.take(512 * 1024 * 1024).read_to_end(&mut buf)?;
    Ok(buf)
}

/// Check `data` against a Tauri/minisign `.sig` (base64 of the signature file)
/// with the compiled-in public key.
pub fn verify_signature(data: &[u8], sig_b64: &str, pubkey_b64: &str) -> Result<()> {
    let b64 = base64::engine::general_purpose::STANDARD;
    let pk_text = String::from_utf8(b64.decode(pubkey_b64.trim()).context("public key is not base64")?)?;
    let sig_text = String::from_utf8(b64.decode(sig_b64.trim()).context("signature is not base64")?)?;
    let pk = minisign_verify::PublicKey::decode(&pk_text).map_err(|e| anyhow!("bad public key: {e}"))?;
    let sig = minisign_verify::Signature::decode(&sig_text).map_err(|e| anyhow!("bad signature: {e}"))?;
    pk.verify(data, &sig, false).map_err(|e| anyhow!("signature does not match: {e}"))
}

/// Download the release's installer, verify its signature, and leave it in
/// %ProgramData%\FowlEngine\updates\. Returns its path.
pub fn download(cfg: &ManagerConfig, rel: &Release) -> Result<PathBuf> {
    let c = client(cfg)?;
    let token = cfg.github_token.as_deref().map(|t| !t.trim().is_empty()).unwrap_or(false);
    let sig = String::from_utf8(get_bytes(&c, &rel.sig_url, &rel.sig_api_url, token)?)?;
    let data = get_bytes(&c, &rel.setup_url, &rel.setup_api_url, token)?;
    if rel.size > 0 && data.len() as u64 != rel.size {
        bail!("downloaded {} bytes, the release says {}", data.len(), rel.size);
    }
    verify_signature(&data, &sig, UPDATER_PUBKEY).context("refusing an installer that isn't signed by the release key")?;
    std::fs::create_dir_all(updates_dir())?;
    let safe: String = rel.setup_name.chars().map(|c| if c.is_ascii_alphanumeric() || "._-".contains(c) { c } else { '_' }).collect();
    let path = updates_dir().join(safe);
    std::fs::write(&path, &data)?;
    log::info!("downloaded + verified {} ({} bytes, sha256 {})", path.display(), data.len(),
               hex::encode(Sha256::digest(&data)));
    prune(&path);
    Ok(path)
}

fn prune(keep: &Path) {
    let Ok(rd) = std::fs::read_dir(updates_dir()) else { return };
    for e in rd.flatten() {
        if e.path() != keep {
            let _ = std::fs::remove_file(e.path());
        }
    }
}

/// Start the installer detached (it outlives this process: its first act is
/// to stop the service we may be running in).
pub fn launch_installer(path: &Path, silent: bool) -> Result<()> {
    let mut cmd = std::process::Command::new(path);
    if silent {
        cmd.args(["/S", "/UPDATE"]);
    } else {
        cmd.args(["/P", "/UPDATE", "/R"]);
    }
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        const DETACHED_PROCESS: u32 = 0x0000_0008;
        const CREATE_NEW_PROCESS_GROUP: u32 = 0x0000_0200;
        const CREATE_BREAKAWAY_FROM_JOB: u32 = 0x0100_0000;
        cmd.creation_flags(DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP | CREATE_BREAKAWAY_FROM_JOB);
    }
    match cmd.spawn() {
        Ok(_) => Ok(()),
        Err(_) if cfg!(windows) => {
            // not in a job that allows breakaway: try again without it
            let mut cmd = std::process::Command::new(path);
            cmd.args(if silent { vec!["/S", "/UPDATE"] } else { vec!["/P", "/UPDATE", "/R"] });
            #[cfg(windows)]
            {
                use std::os::windows::process::CommandExt;
                cmd.creation_flags(0x0000_0008 | 0x0000_0200);
            }
            cmd.spawn().map(|_| ()).context("starting the installer")
        }
        Err(e) => Err(e).context("starting the installer"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn picks_newest_semver_and_honours_channel() {
        let list = serde_json::json!([
            {"tag_name": "engine-2026.09.26-1", "assets": []},
            {"tag_name": "manager-v0.3.0", "prerelease": true, "assets": [
                {"name": "Fowl Engine Manager_0.3.0_x64-setup.exe", "browser_download_url": "u3", "url": "a3", "size": 5},
                {"name": "Fowl Engine Manager_0.3.0_x64-setup.exe.sig", "browser_download_url": "s3", "url": "b3"}]},
            {"tag_name": "manager-v0.2.0", "assets": [
                {"name": "Fowl Engine Manager_0.2.0_x64-setup.exe", "browser_download_url": "u2", "url": "a2", "size": 5},
                {"name": "Fowl Engine Manager_0.2.0_x64-setup.exe.sig", "browser_download_url": "s2", "url": "b2"}]},
            {"tag_name": "manager-v0.9.0", "draft": true, "assets": []},
            {"tag_name": "manager-v0.2.5", "assets": [
                {"name": "Fowl Engine Manager_0.2.5_x64-setup.exe", "browser_download_url": "u25"}]}
        ]);
        let r = pick(&list, "stable").unwrap();
        assert_eq!(r.version, "0.2.0");   // 0.2.5 has no .sig -> never offered
        assert_eq!(r.sig_url, "s2");
        assert_eq!(pick(&list, "beta").unwrap().version, "0.3.0");
    }
}

#[cfg(test)]
mod sig_tests {
    use super::*;

    /// tests/fixture.bin.sig was made with the real release key
    /// (`tauri signer sign`), so this also proves updater.pub matches it.
    #[test]
    fn verifies_real_signature_and_rejects_tampering() {
        let data = include_bytes!("../tests/fixture.bin");
        let sig = include_str!("../tests/fixture.bin.sig");
        verify_signature(data, sig, UPDATER_PUBKEY).expect("genuine file verifies");
        let mut bad = data.to_vec();
        bad[0] ^= 1;
        assert!(verify_signature(&bad, sig, UPDATER_PUBKEY).is_err(), "tampered file must fail");
    }
}

#[cfg(test)]
mod release_check {
    /// `FOWL_VERIFY=<setup.exe> cargo test verify_release_file -- --ignored`
    /// checks a built installer against its .sig with the compiled-in key.
    #[test]
    #[ignore]
    fn verify_release_file() {
        let path = std::env::var("FOWL_VERIFY").expect("set FOWL_VERIFY to an installer path");
        let data = std::fs::read(&path).unwrap();
        let sig = std::fs::read_to_string(format!("{path}.sig")).unwrap();
        super::verify_signature(&data, &sig, super::UPDATER_PUBKEY).expect("signature must verify");
        let mut bad = data.clone();
        let n = bad.len() / 2;
        bad[n] ^= 0x55;
        assert!(super::verify_signature(&bad, &sig, super::UPDATER_PUBKEY).is_err());
    }
}
