//! New box: turn a fresh Windows PC into a Fowl Engine server box, from
//! nothing, in one job. The first profile is the training range (bfrange):
//! a box of its own, with its own DCSServerBot, PostgreSQL, bfdb and netidx
//! resolver, kept apart from the campaign servers.
//!
//! In order (each step skips what is already there, so the job can be run
//! again after fixing whatever stopped it):
//!
//!  1. Python and PostgreSQL (winget, as a restore does);
//!  2. the bot's PostgreSQL role and database;
//!  3. DCSServerBot itself (the pinned release's source zip);
//!  4. the Discord channels (optional, through the bot's own token);
//!  5. the DCS instance folder: serverSettings.lua, the range mission and its
//!     config, the hooks loader, Export.lua;
//!  6. the engine binaries (bfrange.dll, bfdb.exe, netidx.exe) from a signed
//!     engine release on GitHub or a release folder;
//!  7. the bot's config files, written fresh (an existing one is kept);
//!  8. the Fowl Engine plugin, the netidx client config, firewall rules;
//!  9. SRS (release zip), DCS World Server (ED's updater, no GUI installer),
//!     Tacview (its installer, which the admin downloads past the EULA);
//! 10. the FowlEngine service.
//!
//! What it cannot do is left as next steps: the ED account login (a DCS
//! dialog), inviting the bot to Discord, the router, DNS and the main PC's
//! reverse proxy.

use crate::backup::{
    cancelled, exe, hidden, is_cancelled, next_step, note, phase, postgres_bins, set_done,
    sql_ident, sql_lit, warn, winget, with_job, Counting,
};
use crate::config;
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::time::Duration;

/// The DCSServerBot release a new box gets. Its own updater moves it on from
/// there (main.yaml `autoupdate`). The repository moved from Special-K-s to
/// this organisation in 2026; the old URL is a 404.
pub const BOT_REPO: &str = "Special-K-s-Flightsim-Bots/DCSServerBot";
pub const BOT_TAG: &str = "v3.0.4.27";

/// ED's updater on its own: a stable URL, unlike the GUI installer's.
const DCS_UPDATER_URL: &str = "https://cdn.digitalcombatsimulator.com/files/DCS_updater_64bit.zip";
/// What a range server needs: the base game, its map (the Mariana Islands,
/// free), and the Supercarrier (CVN-72 is one).
const DCS_MODULES: &[&str] = &["WORLD", "MARIANAISLANDS_terrain", "SUPERCARRIER"];

const SRS_REPO: &str = "ciribob/DCS-SimpleRadioStandalone";
const TACVIEW_PAGE: &str = "https://www.tacview.net/download/latest/en/";

/// The engine files a range box runs.
const RANGE_ENGINE_FILES: &[&str] = &["bfrange.dll", "bfdb.exe", "netidx.exe"];

const RANGE_MISSION: &str = "VS_Range_Marianas.miz";
const RANGE_CFG: &str = "VSRANGE_RANGE";
const RANGE_NETIDX_BASE: &str = "/local/fowl/range";
const RANGE_EXPORT_PORT: u16 = 42004;
const DISCORD_API: &str = "https://discord.com/api/v10";
/// The engine release key's public half (`tauri signer`'s wrapped .pub, key
/// id 44D2C7BA11CC8E7D), so a new box checks engine releases without anyone
/// pasting it. Its secret half is %USERPROFILE%\.tauriowl-engine.key on
/// the build PC, which publish-release.ps1 signs with.
pub const ENGINE_PUBKEY: &str = include_str!("../engine.pub");

// ---- options ----------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct NewBoxOptions {
    /// "range" (the only profile so far).
    pub profile: String,

    pub install_python: bool,
    pub install_postgres: bool,
    /// The PostgreSQL superuser's password: set by the install, or the one
    /// an existing PostgreSQL already has. Needed to create the bot's role.
    pub pg_password: String,
    pub pg_port: u16,

    pub bot_dir: String,
    pub discord_token: String,
    pub guild_id: String,
    pub owner_id: String,
    /// Discord role names for the bot's Admin / DCS Admin roles.
    pub admin_role: String,
    pub create_channels: bool,
    /// Channel ids typed in by hand (used where create_channels made none).
    pub channels: Channels,

    pub install_dcs: bool,
    pub dcs_dir: String,
    /// The ED account the bot checks versions with (nodes.yaml DCS.user /
    /// password; the bot moves the password into .secret on its first start).
    pub ed_user: String,
    pub ed_password: String,
    pub install_srs: bool,
    pub srs_dir: String,
    /// TacviewSetup.exe the admin downloaded (the EULA is theirs to accept).
    pub tacview_setup: String,

    pub instance: String,
    pub server_name: String,
    pub server_password: String,
    pub max_players: u32,
    pub dcs_port: u16,
    pub srs_port: u16,

    /// github | folder | skip
    pub engine_source: String,
    pub engine_repo: String,
    pub engine_channel: String,
    pub engine_folder: String,
    /// The engine release key's public half (any form `tauri signer` or
    /// minisign writes).
    pub engine_public_key: String,

    /// This bfdb's public origin, e.g. https://range-api.vectorstrike.org
    pub public_api_url: String,
    pub range_site_url: String,
    /// The main PC's LAN address: its reverse proxy reaches this bfdb, so
    /// bfdb listens on the LAN and the firewall lets only that PC in.
    pub main_pc_ip: String,
    /// The bot's own Discord application doubles as the range site's login.
    pub discord_client_secret: String,

    pub firewall: bool,
    /// Rewrite bot config files that are already there (kept aside first).
    pub overwrite_config: bool,
    pub install_service: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(default)]
pub struct Channels {
    pub server_status: String,
    pub chat: String,
    pub range_status: String,
    pub range_results: String,
    pub greenie: String,
    pub admin: String,
    pub audit: String,
}

impl Default for NewBoxOptions {
    fn default() -> Self {
        Self {
            profile: "range".into(),
            install_python: true,
            install_postgres: true,
            pg_password: String::new(),
            pg_port: 5432,
            bot_dir: r"C:\DCSServerBot".into(),
            discord_token: String::new(),
            guild_id: String::new(),
            owner_id: String::new(),
            admin_role: "Staff".into(),
            create_channels: true,
            channels: Channels::default(),
            install_dcs: true,
            dcs_dir: r"C:\Program Files\Eagle Dynamics\DCS World Server".into(),
            ed_user: String::new(),
            ed_password: String::new(),
            install_srs: true,
            srs_dir: r"C:\Program Files\DCS-SimpleRadio-Standalone".into(),
            tacview_setup: String::new(),
            instance: "DCS.range".into(),
            server_name: "[VS] Vector Strike | Training Range".into(),
            server_password: String::new(),
            max_players: 32,
            dcs_port: 10328,
            srs_port: 5005,
            engine_source: "github".into(),
            engine_repo: "Dillen198/bfnext".into(),
            engine_channel: "stable".into(),
            engine_folder: String::new(),
            engine_public_key: ENGINE_PUBKEY.trim().to_string(),
            public_api_url: "https://range-api.vectorstrike.org".into(),
            range_site_url: "https://range.vectorstrike.org".into(),
            main_pc_ip: String::new(),
            discord_client_secret: String::new(),
            firewall: true,
            overwrite_config: false,
            install_service: true,
        }
    }
}

impl NewBoxOptions {
    fn home(&self) -> PathBuf {
        saved_games().join(&self.instance)
    }
}

fn saved_games() -> PathBuf {
    PathBuf::from(std::env::var("USERPROFILE").unwrap_or_else(|_| r"C:\Users\Default".into())).join("Saved Games")
}

fn hostname() -> String {
    std::env::var("COMPUTERNAME").unwrap_or_default()
}

/// The account the bot (and so DCS) will run as: the one using this app.
fn bot_account() -> String {
    let user = std::env::var("USERNAME").unwrap_or_default();
    match std::env::var("USERDOMAIN") {
        Ok(d) if !d.is_empty() => format!("{d}\\{user}"),
        _ => user,
    }
}

// ---- what is already here ---------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct Have {
    pub what: String,
    pub ok: bool,
    pub detail: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct NewBoxState {
    pub defaults: NewBoxOptions,
    pub hostname: String,
    pub account: String,
    pub saved_games: String,
    pub have: Vec<Have>,
    pub winget: bool,
    /// Free space on the drive DCS goes to.
    pub dcs_free_bytes: Option<u64>,
    pub bot_version: String,
    pub engine_files_needed: Vec<String>,
}

pub fn inspect() -> NewBoxState {
    let d = NewBoxOptions::default();
    let mut have = Vec::new();
    let py = crate::backup::find_python();
    have.push(Have {
        what: "Python 3.11+".into(),
        ok: py.as_ref().map(|(_, v)| crate::backup::python_ok(v)).unwrap_or(false),
        detail: py.map(|(p, v)| format!("{v} at {}", p.display())).unwrap_or_else(|| "not found".into()),
    });
    let pg = postgres_bins();
    have.push(Have {
        what: "PostgreSQL".into(),
        ok: !pg.is_empty(),
        detail: pg.first().map(|(v, b)| format!("PostgreSQL {v} at {}", b.display())).unwrap_or_else(|| "not found".into()),
    });
    let bot = PathBuf::from(&d.bot_dir);
    have.push(Have {
        what: "DCSServerBot".into(),
        ok: crate::bot::is_bot_dir(&bot),
        detail: if crate::bot::is_bot_dir(&bot) { bot.display().to_string() } else { format!("not at {}", bot.display()) },
    });
    let dcs = PathBuf::from(&d.dcs_dir).join("bin").join("DCS_server.exe");
    have.push(Have {
        what: "DCS World Server".into(),
        ok: dcs.is_file(),
        detail: if dcs.is_file() { d.dcs_dir.clone() } else { "not installed".into() },
    });
    let srs = PathBuf::from(&d.srs_dir).join("Server").join("SRS-Server.exe");
    have.push(Have {
        what: "SRS".into(),
        ok: srs.is_file(),
        detail: if srs.is_file() { d.srs_dir.clone() } else { "not installed".into() },
    });
    let tv = tacview_dir();
    have.push(Have {
        what: "Tacview".into(),
        ok: tv.is_some(),
        detail: tv.map(|p| p.display().to_string()).unwrap_or_else(|| "not installed".into()),
    });
    let home = d.home();
    have.push(Have {
        what: format!("Saved Games\\{}", d.instance),
        ok: home.join("Config").join("serverSettings.lua").is_file(),
        detail: home.display().to_string(),
    });
    let winget_ok = hidden("winget.exe").arg("--version").output().map(|o| o.status.success()).unwrap_or(false);
    let dcs_drive = PathBuf::from(&d.dcs_dir).ancestors().last().map(Path::to_path_buf).unwrap_or_else(|| "C:\\".into());
    NewBoxState {
        hostname: hostname(),
        account: bot_account(),
        saved_games: saved_games().display().to_string(),
        have,
        winget: winget_ok,
        dcs_free_bytes: crate::backup::free_space(&dcs_drive),
        bot_version: BOT_TAG.into(),
        engine_files_needed: RANGE_ENGINE_FILES.iter().map(|s| s.to_string()).collect(),
        defaults: d,
    }
}

fn tacview_dir() -> Option<PathBuf> {
    let base = std::env::var_os("ProgramFiles(x86)").map(PathBuf::from)?;
    let d = base.join("Tacview");
    d.join("DCS").is_dir().then_some(d)
}

// ---- Discord ----------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Default)]
pub struct DiscordCheck {
    pub bot_name: String,
    pub app_id: String,
    /// Opens the "add to server" page for this bot with admin rights.
    pub invite_url: String,
    pub in_guild: bool,
    pub guild_name: Option<String>,
}

fn http() -> Result<reqwest::blocking::Client> {
    Ok(reqwest::blocking::Client::builder()
        .user_agent(concat!("fowl-engine-manager/", env!("CARGO_PKG_VERSION")))
        .connect_timeout(Duration::from_secs(20))
        .timeout(Duration::from_secs(60 * 60 * 6))
        .build()?)
}

fn discord(c: &reqwest::blocking::Client, token: &str, method: reqwest::Method, path: &str) -> reqwest::blocking::RequestBuilder {
    c.request(method, format!("{DISCORD_API}{path}")).header("Authorization", format!("Bot {}", token.trim()))
}

/// Who the token belongs to, and whether that bot is in the guild yet.
pub fn discord_check(token: &str, guild_id: &str) -> Result<DiscordCheck> {
    let c = http()?;
    let me = discord(&c, token, reqwest::Method::GET, "/users/@me").send()?;
    if me.status() == reqwest::StatusCode::UNAUTHORIZED {
        bail!("Discord refused the token -- copy it again from the Developer Portal (Bot > Reset Token)");
    }
    let me: serde_json::Value = me.error_for_status()?.json()?;
    let app_id = me["id"].as_str().unwrap_or_default().to_string();
    let mut out = DiscordCheck {
        bot_name: me["username"].as_str().unwrap_or_default().to_string(),
        invite_url: format!(
            "https://discord.com/oauth2/authorize?client_id={app_id}&scope=bot+applications.commands&permissions=8"
        ),
        app_id,
        ..Default::default()
    };
    let gid = guild_id.trim();
    if !gid.is_empty() {
        let g = discord(&c, token, reqwest::Method::GET, &format!("/guilds/{gid}")).send()?;
        if g.status().is_success() {
            let g: serde_json::Value = g.json()?;
            out.in_guild = true;
            out.guild_name = g["name"].as_str().map(str::to_string);
        }
    }
    Ok(out)
}

/// A "Training Range" category with the range's channels, reusing any that
/// are already there by name. Fills in `ch` where it was blank.
fn create_channels(token: &str, guild_id: &str, ch: &mut Channels) -> Result<()> {
    let c = http()?;
    let gid = guild_id.trim();
    let existing: Vec<serde_json::Value> =
        discord(&c, token, reqwest::Method::GET, &format!("/guilds/{gid}/channels")).send()?.error_for_status()?.json()?;
    let find = |name: &str, kind: u64, parent: Option<&str>| -> Option<String> {
        existing
            .iter()
            .find(|x| {
                x["name"].as_str().map(|n| n.eq_ignore_ascii_case(name)).unwrap_or(false)
                    && x["type"].as_u64() == Some(kind)
                    && (parent.is_none() || x["parent_id"].as_str() == parent)
            })
            .and_then(|x| x["id"].as_str().map(str::to_string))
    };
    let post = |body: serde_json::Value| -> Result<String> {
        let r = discord(&c, token, reqwest::Method::POST, &format!("/guilds/{gid}/channels")).json(&body).send()?;
        if r.status() == reqwest::StatusCode::FORBIDDEN {
            bail!("the bot may not create channels -- invite it with the link shown (Administrator), or give it Manage Channels");
        }
        let v: serde_json::Value = r.error_for_status()?.json()?;
        v["id"].as_str().map(str::to_string).ok_or_else(|| anyhow!("Discord answered without a channel id"))
    };
    let cat = match find("Training Range", 4, None) {
        Some(id) => id,
        None => post(serde_json::json!({"name": "Training Range", "type": 4}))?,
    };
    let wanted: [(&str, &str, &mut String); 7] = [
        ("range-status", "Live: tankers, carriers, stations, who is flying.", &mut ch.range_status),
        ("range-results", "Every graded pass, drop and trap.", &mut ch.range_results),
        ("greenie-board", "Carrier passes.", &mut ch.greenie),
        ("range-server", "The range server's status.", &mut ch.server_status),
        ("range-chat", "In-game chat.", &mut ch.chat),
        ("range-admin", "Range bot admin and engine updates.", &mut ch.admin),
        ("range-audit", "Range bot audit log.", &mut ch.audit),
    ];
    for (name, topic, slot) in wanted {
        if !slot.trim().is_empty() {
            continue;
        }
        let id = match find(name, 0, Some(&cat)) {
            Some(id) => id,
            None => post(serde_json::json!({"name": name, "type": 0, "parent_id": cat, "topic": topic}))?,
        };
        note(format!("  #{name} = {id}"));
        *slot = id;
    }
    Ok(())
}

// ---- downloads --------------------------------------------------------------------------

/// Stream `url` to `dest` through the job's byte counter. Returns its sha256.
fn download(c: &reqwest::blocking::Client, url: &str, dest: &Path) -> Result<String> {
    set_done(0);
    let resp = c.get(url).send().with_context(|| format!("downloading {url}"))?;
    let resp = resp.error_for_status().with_context(|| format!("downloading {url}"))?;
    let total = resp.content_length().unwrap_or(0);
    with_job(|j| j.total_bytes = total);
    if let Some(parent) = dest.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = dest.with_extension("part");
    let mut out = File::create(&tmp)?;
    let mut h = Sha256::new();
    let mut r = Counting { inner: resp };
    let mut buf = vec![0u8; 1 << 16];
    loop {
        let n = r.read(&mut buf)?;
        if n == 0 {
            break;
        }
        h.update(&buf[..n]);
        out.write_all(&buf[..n])?;
    }
    drop(out);
    std::fs::rename(&tmp, dest)?;
    with_job(|j| j.total_bytes = 0);
    Ok(hex::encode(h.finalize()))
}

fn sha256_of(p: &Path) -> Result<String> {
    let mut f = File::open(p)?;
    let mut h = Sha256::new();
    std::io::copy(&mut f, &mut h)?;
    Ok(hex::encode(h.finalize()))
}

/// Unpack `zip` into `dest`. `strip` drops a single top folder (GitHub's
/// source zips). Falls back to PowerShell's Expand-Archive for compression
/// methods the zip crate here doesn't do (deflate64).
fn unzip(zip: &Path, dest: &Path, strip: bool) -> Result<()> {
    std::fs::create_dir_all(dest)?;
    match unzip_native(zip, dest, strip) {
        Ok(()) => Ok(()),
        Err(e) if !strip => {
            note(format!("  unpacking with Expand-Archive instead ({e:#})"));
            let script = format!(
                "Expand-Archive -LiteralPath '{}' -DestinationPath '{}' -Force",
                zip.display().to_string().replace('\'', "''"),
                dest.display().to_string().replace('\'', "''")
            );
            let out = hidden("powershell.exe").args(["-NoProfile", "-NonInteractive", "-Command", &script]).output()?;
            if !out.status.success() {
                bail!("Expand-Archive failed: {}", String::from_utf8_lossy(&out.stderr).trim());
            }
            Ok(())
        }
        Err(e) => Err(e),
    }
}

fn unzip_native(zip: &Path, dest: &Path, strip: bool) -> Result<()> {
    let mut a = zip::ZipArchive::new(File::open(zip)?)?;
    for i in 0..a.len() {
        cancelled()?;
        let mut e = a.by_index(i)?;
        let Some(rel) = e.enclosed_name() else { continue };
        let rel: PathBuf = if strip { rel.components().skip(1).collect() } else { rel.to_path_buf() };
        if rel.as_os_str().is_empty() {
            continue;
        }
        let out = dest.join(&rel);
        if e.is_dir() {
            std::fs::create_dir_all(&out)?;
            continue;
        }
        if let Some(p) = out.parent() {
            std::fs::create_dir_all(p)?;
        }
        let mut f = File::create(&out).with_context(|| format!("writing {}", out.display()))?;
        std::io::copy(&mut e, &mut f)?;
    }
    Ok(())
}

fn downloads_dir() -> PathBuf {
    config::data_dir().join("newbox")
}

/// The newest GitHub release of `repo` (optionally: only tags starting
/// with `prefix`, prereleases only when `beta`).
fn github_release(c: &reqwest::blocking::Client, repo: &str, prefix: &str, beta: bool) -> Result<serde_json::Value> {
    let rels: Vec<serde_json::Value> = c
        .get(format!("https://api.github.com/repos/{repo}/releases?per_page=40"))
        .header("Accept", "application/vnd.github+json")
        .send()?
        .error_for_status()?
        .json()?;
    rels.into_iter()
        .find(|r| {
            !r["draft"].as_bool().unwrap_or(false)
                && (beta || !r["prerelease"].as_bool().unwrap_or(false))
                && r["tag_name"].as_str().map(|t| t.starts_with(prefix)).unwrap_or(false)
        })
        .ok_or_else(|| anyhow!("{repo} has no release{}", if prefix.is_empty() { String::new() } else { format!(" tagged {prefix}*") }))
}

fn asset_url(rel: &serde_json::Value, name: &str) -> Option<String> {
    rel["assets"].as_array()?.iter().find(|a| a["name"].as_str() == Some(name))?["browser_download_url"]
        .as_str()
        .map(str::to_string)
}

// ---- the job ----------------------------------------------------------------------------

pub fn start(opts: NewBoxOptions) -> Result<()> {
    validate(&opts)?;
    crate::backup::spawn("newbox", move || run(opts))
}

fn digits(s: &str) -> bool {
    !s.trim().is_empty() && s.trim().chars().all(|c| c.is_ascii_digit())
}

fn validate(o: &NewBoxOptions) -> Result<()> {
    if o.profile != "range" {
        bail!("unknown profile {:?} -- only \"range\" so far", o.profile);
    }
    if o.pg_password.is_empty() {
        bail!("the PostgreSQL superuser password is needed (to create the bot's database)");
    }
    if o.pg_password.contains('"') {
        bail!("the PostgreSQL password may not contain a double quote");
    }
    if o.discord_token.trim().is_empty() {
        bail!("the new bot's Discord token is needed");
    }
    if !digits(&o.guild_id) {
        bail!("the Discord server (guild) id is a number -- Discord: Server Settings > Widget, or right-click the server > Copy Server ID");
    }
    if !digits(&o.owner_id) {
        bail!("your Discord user id is a number -- right-click your name > Copy User ID");
    }
    if o.server_name.trim().is_empty() || o.instance.trim().is_empty() {
        bail!("the server needs a name and an instance folder name");
    }
    if o.instance.contains(['\\', '/', ':']) {
        bail!("the instance is a folder name under Saved Games, not a path");
    }
    if !["github", "folder", "skip"].contains(&o.engine_source.as_str()) {
        bail!("engine source is github, folder or skip");
    }
    if o.engine_source == "github" && o.engine_public_key.trim().is_empty() {
        bail!("an engine release from GitHub is checked against the engine release key -- paste its public half");
    }
    if o.engine_source == "folder" && !PathBuf::from(o.engine_folder.trim()).join("manifest.json").is_file() {
        bail!("the engine folder needs the release's manifest.json and files");
    }
    if !o.engine_public_key.trim().is_empty() {
        crate::botcfg::parse_public_key(&o.engine_public_key)?;
    }
    if !o.main_pc_ip.trim().is_empty() && o.main_pc_ip.trim().parse::<std::net::IpAddr>().is_err() {
        bail!("the main PC's address is an IP address, e.g. 192.168.1.10");
    }
    if !o.tacview_setup.trim().is_empty() && !Path::new(o.tacview_setup.trim()).is_file() {
        bail!("no Tacview installer at {}", o.tacview_setup.trim());
    }
    Ok(())
}

fn run(mut o: NewBoxOptions) -> Result<String> {
    let mut done: Vec<String> = Vec::new();
    std::fs::create_dir_all(downloads_dir())?;
    let bot_dir = PathBuf::from(o.bot_dir.trim());
    let home = o.home();

    // 1. Python, PostgreSQL
    prerequisites(&o)?;
    cancelled()?;

    // 2. the bot's database
    let cfg_dir = bot_dir.join("config");
    let db_pw = if cfg_dir.join("nodes.yaml").is_file() && !o.overwrite_config {
        note("The bot's config is already there -- leaving its database as it is".into());
        None
    } else {
        phase("Creating the bot's PostgreSQL database");
        let pw = crate::botcfg::generate_secret()?;
        bot_database(&o, &pw)?;
        done.push("database".into());
        Some(pw)
    };
    cancelled()?;

    // 3. DCSServerBot
    if crate::bot::is_bot_dir(&bot_dir) {
        note(format!("DCSServerBot is already at {}", bot_dir.display()));
    } else {
        phase(&format!("Downloading DCSServerBot {BOT_TAG}"));
        let c = http()?;
        let zip = downloads_dir().join(format!("DCSServerBot-{BOT_TAG}.zip"));
        download(&c, &format!("https://github.com/{BOT_REPO}/archive/refs/tags/{BOT_TAG}.zip"), &zip)?;
        phase(&format!("Unpacking DCSServerBot into {}", bot_dir.display()));
        unzip(&zip, &bot_dir, true)?;
        if !crate::bot::is_bot_dir(&bot_dir) {
            bail!("the DCSServerBot download did not unpack into a bot folder at {}", bot_dir.display());
        }
        done.push(format!("DCSServerBot {BOT_TAG}"));
    }
    cancelled()?;

    // 4. Discord channels
    let check = match discord_check(&o.discord_token, &o.guild_id) {
        Ok(c) => Some(c),
        Err(e) => {
            warn(format!("Discord: {e:#}"));
            None
        }
    };
    if let Some(c) = &check {
        note(format!("Discord bot: {} (application {})", c.bot_name, c.app_id));
        if !c.in_guild {
            next_step(format!("Invite the range bot to the Discord server: {}", c.invite_url));
        }
        if o.create_channels {
            if c.in_guild {
                phase("Creating the Training Range channels in Discord");
                match create_channels(&o.discord_token, &o.guild_id, &mut o.channels) {
                    Ok(()) => done.push("Discord channels".into()),
                    Err(e) => {
                        warn(format!("Discord channels were not created: {e:#}"));
                        next_step("Create the range channels in Discord and put their ids into the bot's config (Bot config tab).".into());
                    }
                }
            } else {
                next_step("Run New box again once the bot is in the Discord server: it creates the range channels then.".into());
            }
        }
    }
    cancelled()?;

    // 5. the DCS instance
    phase(&format!("Setting up Saved Games\\{}", o.instance));
    instance_folder(&o, &home)?;
    done.push(format!("Saved Games\\{}", o.instance));
    cancelled()?;

    // 6. engine binaries
    match o.engine_source.as_str() {
        "skip" => next_step(format!(
            "Put the engine in: bfdb.exe and netidx.exe into {}, bfrange.dll into {} -- or drop them into the range admin channel once the bot runs.",
            home.display(),
            home.join("Scripts").display()
        )),
        src => {
            phase("Fetching the engine (bfrange.dll, bfdb.exe, netidx.exe)");
            match engine_files(&o, src, &home) {
                Ok(tag) => done.push(format!("engine {tag}")),
                Err(e) => {
                    warn(format!("the engine was not installed: {e:#}"));
                    next_step(format!(
                        "Install the engine by hand: bfdb.exe and netidx.exe into {}, bfrange.dll into {}.",
                        home.display(),
                        home.join("Scripts").display()
                    ));
                }
            }
        }
    }
    cancelled()?;

    // 7. the bot's config
    phase("Writing the bot's config");
    let app_id = check.as_ref().map(|c| c.app_id.clone()).unwrap_or_default();
    write_bot_config(&o, &cfg_dir, &home, db_pw.as_deref(), &app_id)?;
    done.push("bot config".into());

    // 8. plugin, netidx client config, firewall
    phase("Installing the Fowl Engine plugin into the bot");
    match crate::bot::sync_plugin(&bot_dir) {
        Ok(r) => note(format!("  plugin {}: {} file(s) copied", r.bundle_version, r.changed.len())),
        Err(e) => warn(format!("the plugin was not installed: {e:#} -- the service syncs it on its first start")),
    }
    netidx_client_config()?;
    if o.firewall {
        phase("Opening the firewall for DCS and SRS");
        if let Err(e) = firewall(&o) {
            warn(format!("firewall rules: {e:#}"));
        }
    }
    let mut cfg = config::ManagerConfig::load();
    cfg.bot_dir = Some(bot_dir.display().to_string());
    cfg.desktop_session = true;
    cfg.desktop_user = Some(bot_account());
    cfg.save()?;
    cancelled()?;

    // 9. SRS, DCS, Tacview -- the big downloads last, so the config is in
    // place even if one of them has to be retried
    if o.install_srs {
        match install_srs(&o) {
            Ok(Some(v)) => done.push(format!("SRS {v}")),
            Ok(None) => {}
            Err(e) => {
                warn(format!("SRS was not installed: {e:#}"));
                next_step(format!("Install SRS server into {} (github.com/{SRS_REPO}/releases).", o.srs_dir));
            }
        }
    }
    cancelled()?;
    if o.install_dcs {
        match install_dcs(&o) {
            Ok(true) => done.push("DCS World Server".into()),
            Ok(false) => {}
            Err(e) if is_cancelled() => return Err(e),
            Err(e) => {
                warn(format!("DCS World Server was not installed: {e:#}"));
                next_step("Install DCS World Server (digitalcombatsimulator.com > Downloads > Dedicated Server) with the Mariana Islands map and the Supercarrier, then run New box again.".into());
            }
        }
    }
    cancelled()?;
    install_tacview(&o, &mut done);

    // 10. the service
    #[cfg(windows)]
    if o.install_service {
        phase("Installing the FowlEngine service");
        match crate::winsvc::install(None, None) {
            Ok(()) => done.push("FowlEngine service".into()),
            Err(e) => warn(format!("the service was not installed: {e:#} -- Setup step 3")),
        }
    }

    // what only a person can do
    let dcs_server = PathBuf::from(&o.dcs_dir).join("bin").join("DCS_server.exe");
    if dcs_server.is_file() {
        next_step(format!(
            "Log the server in to your ED account once: press \"Open DCS login\", sign in with \"save password\" and \"auto login\" ticked, then close DCS. ({} -w {})",
            dcs_server.display(),
            o.instance
        ));
    }
    next_step("Setup step 4: turn on automatic sign-in for this account, so the box comes back by itself after a reboot.".into());
    next_step(format!(
        "Router: forward DCS {} (TCP+UDP) and SRS {} (TCP+UDP) to this PC.",
        o.dcs_port, o.srs_port
    ));
    if !o.public_api_url.trim().is_empty() {
        let host = url::Url::parse(o.public_api_url.trim()).ok().and_then(|u| u.host_str().map(str::to_string)).unwrap_or_default();
        next_step(format!(
            "Main PC: add to its Caddyfile `{host} {{ reverse_proxy <this PC's LAN address>:8880 }}` and reload Caddy; DNS: `{host}` A record -> the public IP (the same as api.vectorstrike.org)."
        ));
    }
    next_step("The bot's first start builds its Python environment: give it a few minutes before the range shows up in Discord.".into());

    Ok(format!("New box set up: {}", done.join(", ")))
}

// ---- 1. prerequisites -------------------------------------------------------------------

fn prerequisites(o: &NewBoxOptions) -> Result<()> {
    if o.install_python {
        if crate::backup::find_python().map(|(_, v)| crate::backup::python_ok(&v)).unwrap_or(false) {
            note("Python is already installed".into());
        } else {
            phase("Installing Python 3.13 (winget)");
            winget(&[
                "install", "-e", "--id", "Python.Python.3.13", "--scope", "machine", "--silent",
                "--override", "/quiet InstallAllUsers=1 PrependPath=1 Include_test=0",
            ])
            .context("installing Python (winget)")?;
            note("  Python installed".into());
        }
    }
    if postgres_bins().is_empty() {
        if !o.install_postgres {
            bail!("PostgreSQL is not installed -- tick \"Install PostgreSQL\" or install it first");
        }
        phase("Installing PostgreSQL 17 (winget) -- this takes a few minutes");
        let ov = format!(
            "--mode unattended --unattendedmodeui none --superpassword \"{}\" --serverport {} --disable-components pgAdmin,stackbuilder",
            o.pg_password, o.pg_port
        );
        winget(&["install", "-e", "--id", "PostgreSQL.PostgreSQL.17", "--silent", "--override", &ov])
            .context("installing PostgreSQL (winget)")?;
        note("  PostgreSQL installed".into());
    } else {
        note("PostgreSQL is already installed".into());
    }
    Ok(())
}

// ---- 2. the bot's database ----------------------------------------------------------------

/// The role and database install.py would make: dcsserverbot / dcsserverbot.
fn bot_database(o: &NewBoxOptions, role_pw: &str) -> Result<()> {
    let (_, bin) = postgres_bins().into_iter().next().ok_or_else(|| anyhow!("PostgreSQL is not installed on this PC"))?;
    let port = o.pg_port.to_string();
    let psql = |sql: &str| -> Result<String> {
        let out = hidden(&exe(&bin, "psql").display().to_string())
            .args(["-h", "127.0.0.1", "-p", &port, "-U", "postgres", "-d", "postgres", "-v", "ON_ERROR_STOP=1", "-tAc", sql])
            .env("PGPASSWORD", &o.pg_password)
            .output()?;
        if !out.status.success() {
            bail!("{}", String::from_utf8_lossy(&out.stderr).trim());
        }
        Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
    };
    let mut tries = 0;
    loop {
        match psql("SELECT 1") {
            Ok(_) => break,
            Err(e) if tries < 15 && !e.to_string().contains("password authentication failed") => {
                tries += 1;
                std::thread::sleep(Duration::from_secs(2));
            }
            Err(e) => bail!("can't sign in to PostgreSQL as 'postgres': {e}"),
        }
    }
    let (user, db) = ("dcsserverbot", "dcsserverbot");
    let role = sql_ident(user);
    psql(&format!(
        "DO $$ BEGIN IF EXISTS (SELECT FROM pg_roles WHERE rolname = {u}) THEN ALTER ROLE {role} WITH LOGIN PASSWORD {p}; \
         ELSE CREATE ROLE {role} WITH LOGIN PASSWORD {p}; END IF; END $$;",
        u = sql_lit(user),
        p = sql_lit(role_pw)
    ))
    .context("creating the bot's database user")?;
    if psql(&format!("SELECT 1 FROM pg_database WHERE datname = {}", sql_lit(db)))? == "1" {
        note("  the database is already there -- kept".into());
        psql(&format!("ALTER DATABASE {} OWNER TO {role}", sql_ident(db)))?;
    } else {
        psql(&format!("CREATE DATABASE {} OWNER {role}", sql_ident(db))).context("creating the database")?;
    }
    note(format!("  database {db} owned by {user}"));
    Ok(())
}

// ---- 5. the DCS instance ------------------------------------------------------------------

fn range_resources() -> PathBuf {
    let bundled = config::bundled_bot_dir().with_file_name("range");
    if bundled.is_dir() {
        return bundled;
    }
    // `tauri dev`: next to the sources
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resources").join("range")
}

fn write_new(p: &Path, text: &str) -> Result<bool> {
    if p.exists() {
        return Ok(false);
    }
    if let Some(d) = p.parent() {
        std::fs::create_dir_all(d)?;
    }
    std::fs::write(p, text).with_context(|| format!("writing {}", p.display()))?;
    Ok(true)
}

fn lua_str(s: &str) -> String {
    format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"").replace('\n', "\\n"))
}

fn instance_folder(o: &NewBoxOptions, home: &Path) -> Result<()> {
    let res = range_resources();
    for d in ["Config", "Missions", "Scripts\\Hooks", "Logs", "Tacview", "_staging"] {
        std::fs::create_dir_all(home.join(d))?;
    }
    let mission = home.join("Missions").join(RANGE_MISSION);
    if !mission.exists() {
        std::fs::copy(res.join(RANGE_MISSION), &mission).context("copying the range mission")?;
        note(format!("  {}", mission.display()));
    }
    // the engine reads <write dir>\<sortie>_RANGE
    let cfg = home.join(RANGE_CFG);
    if !cfg.exists() {
        std::fs::copy(res.join(RANGE_CFG), &cfg).context("copying the range config")?;
        note(format!("  {}", cfg.display()));
    }
    // the hook loader is ours: always the bundled one
    std::fs::copy(res.join("bfrange_hooks.lua"), home.join("Scripts").join("Hooks").join("bfrange_hooks.lua"))?;
    let export = home.join("Scripts").join("Export.lua");
    if export.exists() {
        let have = std::fs::read_to_string(&export).unwrap_or_default();
        if !have.contains("bf_export_port") {
            warn(format!(
                "{} is someone else's -- merge in the repo's scripts/Export.lua by hand (the live map needs it)",
                export.display()
            ));
        }
    } else {
        std::fs::copy(res.join("Export.lua"), &export)?;
    }
    std::fs::write(home.join("Scripts").join("bf_export_port.lua"), format!("return {RANGE_EXPORT_PORT}\n"))?;

    // The bot's SRS extension reads these sections and keys from SRS.cfg and
    // fails every minute without them; an SRS 2.4 install has no server.cfg
    // for it to copy, so the file is written here.
    let srs_cfg = format!(
        "[General Settings]\r\nCLIENT_EXPORT_ENABLED=true\r\nEXTERNAL_AWACS_MODE=false\r\nLOTATC_EXPORT_ENABLED=false\r\n\
         LOTATC_EXPORT_PORT=10712\r\nALLOW_INSTRUCTOR_MODE=false\r\nSPECTATORS_AUDIO_DISABLED=false\r\nRADIO_EFFECT_OVERRIDE=false\r\n\r\n\
         [Server Settings]\r\nSERVER_PORT={port}\r\nSERVER_IP=0.0.0.0\r\nCLIENT_EXPORT_FILE_PATH={export}\r\n\
         HTTP_SERVER_ENABLED=false\r\nHTTP_SERVER_PORT=8090\r\nUPNP_ENABLED=false\r\n\r\n\
         [External AWACS Mode Settings]\r\nEXTERNAL_AWACS_MODE_BLUE_PASSWORD=\r\nEXTERNAL_AWACS_MODE_RED_PASSWORD=\r\n",
        port = o.srs_port,
        export = home.join("Config").join("clients-list.json").display(),
    );
    write_new(&home.join("Config").join("SRS.cfg"), &srs_cfg)?;

    let settings = home.join("Config").join("serverSettings.lua");
    let text = format!(
        r#"cfg =
{{
	["description"] = "Vector Strike training range: graded bombing, strafe, BFM and BVR, tankers, carrier traps and helicopter courses.",
	["require_pure_textures"] = true,
	["advanced"] =
	{{
		["allow_change_tailno"] = true,
		["allow_dynamic_radio"] = true,
		["allow_object_export"] = true,
		["pause_on_load"] = false,
		["coalition_join_cooldown_seconds"] = 0,
		["allow_sensor_export"] = true,
		["event_Takeoff"] = true,
		["pause_without_clients"] = false,
		["client_outbound_limit"] = 0,
		["client_inbound_limit"] = 0,
		["server_can_screenshot"] = false,
		["allow_players_pool"] = true,
		["voice_chat_server"] = false,
		["allow_change_skin"] = true,
		["event_Connect"] = true,
		["event_Ejecting"] = true,
		["event_Kill"] = true,
		["event_Crash"] = true,
		["event_Role"] = true,
		["resume_mode"] = 1,
		["allow_trial_only_clients"] = false,
		["maxPing"] = 0,
		["allow_ownship_export"] = true,
		["enable_coalition_join_cooldown"] = false,
	}}, -- end of ["advanced"]
	["port"] = "{port}",
	["mode"] = 0,
	["bind_address"] = "",
	["isPublic"] = true,
	["missionList"] =
	{{
		[1] = {mission},
	}}, -- end of ["missionList"]
	["listStartIndex"] = 1,
	["listShuffle"] = false,
	["password"] = {password},
	["version"] = 1,
	["name"] = {name},
	["require_pure_scripts"] = false,
	["listLoop"] = false,
	["require_pure_clients"] = false,
	["require_pure_models"] = true,
	["maxPlayers"] = {max},
}} -- end of cfg
"#,
        port = o.dcs_port,
        mission = lua_str(&mission.display().to_string()),
        password = lua_str(&o.server_password),
        name = lua_str(o.server_name.trim()),
        max = o.max_players.max(2),
    );
    if !write_new(&settings, &text)? {
        note(format!("  {} is already there -- kept (its name must stay {:?})", settings.display(), o.server_name.trim()));
    }
    Ok(())
}

// ---- 6. engine binaries -------------------------------------------------------------------

/// The engine release's manifest.json, checked against the release key when
/// one is given. Returns (tag, files: name -> sha256).
fn check_manifest(raw: &[u8], sig: Option<&[u8]>, key: &str) -> Result<(String, std::collections::BTreeMap<String, String>)> {
    if !key.trim().is_empty() {
        let sig = sig.ok_or_else(|| anyhow!("the release has no manifest.json.sig"))?;
        let (line, id) = crate::botcfg::parse_public_key(key)?;
        let pk = minisign_verify::PublicKey::from_base64(&line).map_err(|e| anyhow!("bad engine public key: {e}"))?;
        let sig_text = String::from_utf8_lossy(sig).trim().to_string();
        // `tauri signer` writes the minisign signature base64-wrapped
        let sig_text = if sig_text.starts_with("untrusted comment:") {
            sig_text
        } else {
            use base64::Engine;
            String::from_utf8(base64::engine::general_purpose::STANDARD.decode(&sig_text).context("signature is not base64")?)?
        };
        let s = minisign_verify::Signature::decode(&sig_text).map_err(|e| anyhow!("bad signature: {e}"))?;
        pk.verify(raw, &s, false).map_err(|e| anyhow!("manifest.json is not signed by key {id}: {e}"))?;
        note(format!("  manifest.json signed by {id}"));
    } else {
        warn("no engine release key given: the files are checked against manifest.json only, not its signature".into());
    }
    let doc: serde_json::Value = serde_json::from_slice(raw).context("manifest.json is not JSON")?;
    let tag = doc["tag"].as_str().unwrap_or("?").to_string();
    let mut files = std::collections::BTreeMap::new();
    for (name, meta) in doc["files"].as_object().ok_or_else(|| anyhow!("manifest.json lists no files"))? {
        if let Some(sha) = meta["sha256"].as_str() {
            files.insert(name.to_ascii_lowercase(), sha.to_ascii_lowercase());
        }
    }
    Ok((tag, files))
}

fn engine_target(home: &Path, name: &str) -> PathBuf {
    match name {
        "bfrange.dll" => home.join("Scripts").join(name),
        _ => home.join(name),
    }
}

fn engine_files(o: &NewBoxOptions, source: &str, home: &Path) -> Result<String> {
    let c = http()?;
    let (tag, files, fetch): (String, _, Box<dyn Fn(&str, &Path) -> Result<String>>) = if source == "github" {
        let rel = github_release(&c, o.engine_repo.trim(), "engine-", o.engine_channel == "beta")?;
        let murl = asset_url(&rel, "manifest.json").ok_or_else(|| anyhow!("the release has no manifest.json"))?;
        let raw = c.get(&murl).send()?.error_for_status()?.bytes()?.to_vec();
        let sig = match asset_url(&rel, "manifest.json.sig") {
            Some(u) => Some(c.get(&u).send()?.error_for_status()?.bytes()?.to_vec()),
            None => None,
        };
        let (tag, files) = check_manifest(&raw, sig.as_deref(), &o.engine_public_key)?;
        let rel2 = rel.clone();
        let c2 = c.clone();
        (
            tag,
            files,
            Box::new(move |name: &str, dest: &Path| {
                let u = asset_url(&rel2, name).ok_or_else(|| anyhow!("the release has no {name}"))?;
                download(&c2, &u, dest)
            }),
        )
    } else {
        let dir = PathBuf::from(o.engine_folder.trim());
        let raw = std::fs::read(dir.join("manifest.json"))?;
        let sig = std::fs::read(dir.join("manifest.json.sig")).ok();
        let (tag, files) = check_manifest(&raw, sig.as_deref(), &o.engine_public_key)?;
        (
            tag,
            files,
            Box::new(move |name: &str, dest: &Path| {
                let src = dir.join(name);
                if let Some(p) = dest.parent() {
                    std::fs::create_dir_all(p)?;
                }
                std::fs::copy(&src, dest).with_context(|| format!("copying {}", src.display()))?;
                sha256_of(dest)
            }),
        )
    };
    note(format!("  engine release {tag}"));
    for name in RANGE_ENGINE_FILES {
        cancelled()?;
        let want = files.get(*name).ok_or_else(|| anyhow!("release {tag} has no {name}"))?;
        let dest = engine_target(home, name);
        if dest.is_file() && sha256_of(&dest).ok().as_deref() == Some(want.as_str()) {
            note(format!("  {name}: already this build"));
            continue;
        }
        let tmp = dest.with_extension("newbox");
        let got = fetch(name, &tmp)?;
        if &got != want {
            let _ = std::fs::remove_file(&tmp);
            bail!("{name} does not match the manifest (sha256 {got}, expected {want})");
        }
        if dest.exists() {
            std::fs::rename(&dest, dest.with_extension(format!("before-newbox-{}", chrono::Local::now().format("%Y%m%d%H%M%S"))))?;
        }
        std::fs::rename(&tmp, &dest)?;
        note(format!("  {name} -> {}", dest.display()));
    }
    Ok(tag)
}

// ---- 7. the bot's config ------------------------------------------------------------------

/// A YAML scalar for any string: JSON's double-quoted form is valid YAML.
fn q(s: &str) -> String {
    serde_json::to_string(s).unwrap_or_else(|_| "\"\"".into())
}

/// A channel id, or YAML null when there is none.
fn chan(s: &str) -> String {
    if digits(s) {
        s.trim().to_string()
    } else {
        "null".into()
    }
}

fn put_config(cfg_dir: &Path, rel: &str, text: &str, overwrite: bool) -> Result<()> {
    let p = cfg_dir.join(rel);
    serde_yaml::from_str::<serde_yaml::Value>(text).with_context(|| format!("{rel}: the generated YAML does not parse"))?;
    if p.exists() {
        if !overwrite {
            note(format!("  {rel}: already there -- kept"));
            return Ok(());
        }
        let aside = p.with_extension(format!("yaml.before-newbox-{}", chrono::Local::now().format("%Y%m%d%H%M%S")));
        std::fs::rename(&p, &aside)?;
        note(format!("  {rel}: the old one is {}", aside.display()));
    }
    if let Some(d) = p.parent() {
        std::fs::create_dir_all(d)?;
    }
    std::fs::write(&p, text)?;
    note(format!("  {rel}"));
    Ok(())
}

fn write_bot_config(o: &NewBoxOptions, cfg_dir: &Path, home: &Path, db_pw: Option<&str>, app_id: &str) -> Result<()> {
    let w = o.overwrite_config;
    let stamp = chrono::Local::now().format("%Y-%m-%d %H:%M");
    let head = format!("# Written by Fowl Engine Manager's New box wizard ({stamp}) for a training range box.\n");
    let node = hostname();
    let ch = &o.channels;
    let h = |p: &str| home.join(p).display().to_string();
    let homes = home.display().to_string();

    put_config(
        cfg_dir,
        "main.yaml",
        &format!(
            "{head}guild_id: {gid}\nautoupdate: true\nuse_dashboard: false\nchat_command_prefix: '-'\nlogging:\n  loglevel: INFO\n  logrotate_count: 5\n  logrotate_size: 10485760\n  utc: true\nopt_plugins:\n- restapi\n- commands\n- userstats\n- scheduler\n- fowlengine\n- mission\n",
            gid = o.guild_id.trim()
        ),
        w,
    )?;

    let db_url = match db_pw {
        Some(pw) => format!("postgres://dcsserverbot:{pw}@127.0.0.1:{}/dcsserverbot?sslmode=prefer", o.pg_port),
        None => format!("postgres://dcsserverbot:SECRET@127.0.0.1:{}/dcsserverbot?sslmode=prefer", o.pg_port),
    };
    let mut dcs = format!(
        "  DCS:\n    installation: {}\n    autoupdate: true\n    desanitize: true\n    minimized: true\n",
        q(&o.dcs_dir)
    );
    if !o.ed_user.trim().is_empty() {
        dcs += &format!("    user: {}\n", q(o.ed_user.trim()));
        if !o.ed_password.is_empty() {
            dcs += &format!("    password: {}   # the bot moves it into config\\.secret on its first start\n", q(&o.ed_password));
        }
    }
    let tacview = tacview_dir().is_some() || !o.tacview_setup.trim().is_empty();
    let node_tacview = if tacview { "    Tacview:\n      installation: '%ProgramFiles(x86)%\\Tacview'\n" } else { "" };
    let inst_tacview = if tacview {
        format!(
            "        Tacview:\n          tacviewExportPath: {}\n          tacviewPlaybackDelay: 600\n          tacviewDataCaptureMode: 8\n",
            q(&h("Tacview"))
        )
    } else {
        String::new()
    };
    put_config(
        cfg_dir,
        "nodes.yaml",
        &format!(
            "{head}{node}:\n  listen_port: 10042\n  use_upnp: false\n  database:\n    url: {url}\n{dcs}  extensions:\n    SRS:\n      installation: {srs}\n      autoupdate: true\n{node_tacview}  instances:\n    {inst}:\n      home: {homes}\n      missions_dir: {missions}\n      mission_rewrite: false\n      bot_port: 6666\n      webgui_port: 8088\n      dcs_port: {dcs_port}\n      auto_affinity:\n        quality: 3\n      priority: high\n      extensions:\n        SRS:\n          config: {srs_cfg}\n          port: {srs_port}\n          autostart: true\n          gui_server: true\n        # Swaps a staged bfrange.dll in right before each DCS start.\n        BFBinaries:\n          dll_name: bfrange.dll\n          dll_path: {dll}\n          staging_dir: {staging}\n          keep_backups: 5\n{inst_tacview}        # NO BFWeather: it rebuilds the mission from a campaign template.\n",
            url = q(&db_url),
            srs = q(&o.srs_dir),
            inst = q(&o.instance),
            homes = q(&homes),
            missions = q(&h("Missions")),
            dcs_port = o.dcs_port,
            srs_cfg = q(&h("Config\\SRS.cfg")),
            srs_port = o.srs_port,
            dll = q(&h("Scripts\\bfrange.dll")),
            staging = q(&h("_staging")),
        ),
        w,
    )?;

    put_config(
        cfg_dir,
        "servers.yaml",
        &format!(
            "{head}{name}:\n  channels:\n    status: {status}\n    chat: {chat}\n  server_user: Range Control\n  ping_admin_on_crash: true\n  autoscan: false\n  autoadd: true\n  show_atis: true\n  managed_by:\n  - {role}\n  afk:\n    afk_time: 1800\n    punish: false\n    message: '{{player.name}}, you have been kicked for being AFK for more than {{time}}.'\n    check_on_join: true\n    check_on_spawn: false\n    check_after_landing: false\n",
            name = q(o.server_name.trim()),
            status = if digits(&ch.server_status) { ch.server_status.trim().to_string() } else { "-1".into() },
            chat = if digits(&ch.chat) { ch.chat.trim().to_string() } else { "-1".into() },
            role = q(&o.admin_role),
        ),
        w,
    )?;

    let mut bot_channels = String::new();
    if digits(&ch.audit) || digits(&ch.admin) {
        bot_channels += "channels:\n";
        if digits(&ch.audit) {
            bot_channels += &format!("  audit: {}\n", ch.audit.trim());
        }
        if digits(&ch.admin) {
            bot_channels += &format!("  admin: {}\n", ch.admin.trim());
        }
    }
    put_config(
        cfg_dir,
        "services/bot.yaml",
        &format!(
            "{head}token: {token}   # the bot moves it into config\\.secret on its first start\nowner: {owner}\nautomatch: true\nroles:\n  Admin:\n  - {role}\n  DCS Admin:\n  - {role}\n  DCS:\n  - '@everyone'\n{bot_channels}",
            token = q(o.discord_token.trim()),
            owner = o.owner_id.trim(),
            role = q(&o.admin_role),
        ),
        w,
    )?;

    put_config(
        cfg_dir,
        "services/webservice.yaml",
        &format!("{head}DEFAULT:\n  listen: 127.0.0.1   # bfdb and Fowl Engine Manager call it from this box only\n  port: 9876\n"),
        w,
    )?;

    // keys only this box's bot and bfdb share
    // a kept restapi.yaml keeps its key, which bfdb must then use too
    let kept_rest = (!w)
        .then(|| std::fs::read_to_string(cfg_dir.join("plugins").join("restapi.yaml")).ok())
        .flatten()
        .and_then(|t| serde_yaml::from_str::<serde_yaml::Value>(&t).ok())
        .and_then(|v| v["DEFAULT"]["auth"]["api_key"].as_str().map(str::to_string));
    let rest_key = match kept_rest {
        Some(k) => k,
        None => crate::botcfg::generate_secret()?,
    };
    let ops_key = crate::botcfg::generate_secret()?;
    let admin_pw = crate::botcfg::generate_secret()?;
    put_config(
        cfg_dir,
        "plugins/restapi.yaml",
        &format!("{head}DEFAULT:\n  prefix: stats\n  auth:\n    api_key: {}\n", q(&rest_key)),
        w,
    )?;

    put_config(
        cfg_dir,
        "plugins/scheduler.yaml",
        &format!(
            "{head}{node}:\n  {inst}:\n    schedule:\n      00-24: YYYYYYY\n    # every 8 h of mission time, only when empty; shutdown: true so a\n    # staged bfrange.dll goes in (BFBinaries swaps it as DCS starts)\n    action:\n      method: restart\n      mission_time: 480\n      populated: false\n      shutdown: true\n    startup_delay: 10\n    warn:\n      message: '!!! {{item}} will {{what}} in {{when}} !!!'\n      times:\n        - 600\n        - 300\n        - 60\n        - 10\n",
            inst = q(&o.instance)
        ),
        w,
    )?;

    let public_api = o.public_api_url.trim().trim_end_matches('/').to_string();
    let site = o.range_site_url.trim().trim_end_matches('/').to_string();
    let listen = if o.main_pc_ip.trim().is_empty() { "127.0.0.1:8880" } else { "0.0.0.0:8880" };
    let key_line = if o.engine_public_key.trim().is_empty() {
        String::new()
    } else {
        crate::botcfg::parse_public_key(&o.engine_public_key)?.0
    };
    let oauth = if !o.discord_client_secret.trim().is_empty() && !app_id.is_empty() && !public_api.is_empty() {
        format!(
            "    discord_client_id: {}\n    discord_client_secret: {}\n    discord_redirect_uri: {}\n    discord_guild_id: {}\n",
            q(app_id),
            q(o.discord_client_secret.trim()),
            q(&format!("{public_api}/api/auth/callback")),
            q(o.guild_id.trim())
        )
    } else {
        String::new()
    };
    put_config(
        cfg_dir,
        "plugins/fowlengine.yaml",
        &format!(
            r#"{head}DEFAULT:
  ops_api:
    api_key: {ops_key}
  autoupdate:
    enabled: {au_on}
    source: github
    repo: {repo}
    channel: {channel}
    public_key: {key}
    # a range box: no bflib.dll, no bftools.exe
    files: [bfrange.dll, bfdb.exe, netidx.exe]
    apply: when_idle
    bfdb_apply: when_idle
  brand_name: "Vector Strike"
  api_url: "http://localhost:8880"
  admin_username: "admin"
  admin_password: {admin_pw}
  ops_channel: {ops_channel}
  range_site_url: {site}
  bfdb:
    manage: true
    exe: {exe}
    home: {homes}
    staging_dir: {staging}
    listen_address: "{listen}"
    site_address: "127.0.0.1:8766"
    log_file: {log}
    # the box's own resolver (procman writes its config and netidx's client
    # config the first time); it outlives bot restarts, so bfrange keeps
    # publishing through them
    own_resolver: false
    admin_username: "admin"
    admin_password: {admin_pw}
    public_api_url: {public_api}
    cors_origins:
      - {site}
    dcsserverbot_url: "http://127.0.0.1:9876/stats"
    dcsserverbot_api_key: {rest_key}
{oauth}    default_instance: range
    instances:
      - id: range
        label: "Vector Strike Training Range"
        kind: range
        dcs_server_name: {name}
        # must equal netidx_base in the range's _RANGE config
        netidx_base: "{base}"
        sortie: ""
        stats_jsonl: {stats}
        range_jsonl: {rjsonl}
        tacview_dir: {tv}
        srs_url: {srs_url}
        # must match Scripts\bf_export_port.lua
        export_port: {export}
        public: true
        gci:
          enabled: false
{name}:
  # campaign feeds off: this is a range
  status_channel: null
  alerts_channel: null
  achievements_channel: null
  engine_log_channel: null
  perf_channel: null
  gci_transcript_channel: null
  server_info_channel: null
  blue_briefing_channel: null
  red_briefing_channel: null
  coalition_roles: null
  range_status_channel: {rs}
  range_results_channel: {rr}
  greenie_channel: {gr}
"#,
            ops_key = q(&ops_key),
            au_on = if key_line.is_empty() { "false" } else { "true" },
            repo = q(o.engine_repo.trim()),
            channel = q(&o.engine_channel),
            key = q(&key_line),
            admin_pw = q(&admin_pw),
            ops_channel = chan(&ch.admin),
            site = q(&site),
            exe = q(&h("bfdb.exe")),
            homes = q(&homes),
            staging = q(&h("_staging")),
            log = q(&h("Logs\\bfdb.log")),
            public_api = q(&public_api),
            rest_key = q(&rest_key),
            name = q(o.server_name.trim()),
            base = RANGE_NETIDX_BASE,
            stats = q(&h("Logs\\stats.jsonl")),
            rjsonl = q(&h("Logs\\range.jsonl")),
            tv = q(&h("Tacview")),
            srs_url = q(&h("Config\\clients-list.json")),
            export = RANGE_EXPORT_PORT,
            rs = chan(&ch.range_status),
            rr = chan(&ch.range_results),
            gr = chan(&ch.greenie),
        ),
        w,
    )?;
    if key_line.is_empty() {
        next_step("Engine auto-update is off: paste the engine release public key into fowlengine.yaml autoupdate.public_key (Bot config tab) and set enabled: true.".into());
    }
    Ok(())
}

// ---- 8. netidx, firewall ------------------------------------------------------------------

/// bfrange inside DCS reads netidx's default client config too, and DCS may
/// well start before the bot's procman would write it.
fn netidx_client_config() -> Result<()> {
    let Some(appdata) = std::env::var_os("APPDATA").map(PathBuf::from) else { return Ok(()) };
    let p = appdata.join("netidx").join("client.json");
    let others = [
        std::env::var_os("USERPROFILE").map(|h| PathBuf::from(h).join(".config").join("netidx").join("client.json")),
        Some(PathBuf::from(r"C:\netidx\client.json")),
    ];
    if p.exists() || others.iter().flatten().any(|o| o.exists()) {
        return Ok(());
    }
    let text = r#"{"base": "/", "addrs": [["127.0.0.1:4564", "Anonymous"]], "default_auth": "Anonymous"}"#;
    write_new(&p, text)?;
    note(format!("  netidx client config: {}", p.display()));
    Ok(())
}

fn firewall(o: &NewBoxOptions) -> Result<()> {
    let rule = |name: &str, proto: &str, port: u16, remote: Option<&str>| -> Result<()> {
        // re-runnable: the rule is replaced, never stacked
        let _ = hidden("netsh.exe").args(["advfirewall", "firewall", "delete", "rule", &format!("name={name}")]).output();
        let mut args = vec![
            "advfirewall".to_string(),
            "firewall".into(),
            "add".into(),
            "rule".into(),
            format!("name={name}"),
            "dir=in".into(),
            "action=allow".into(),
            format!("protocol={proto}"),
            format!("localport={port}"),
        ];
        if let Some(r) = remote {
            args.push(format!("remoteip={r}"));
        }
        let out = hidden("netsh.exe").args(&args).output()?;
        if !out.status.success() {
            bail!("{name}: {}", String::from_utf8_lossy(&out.stdout).trim());
        }
        note(format!("  {name}"));
        Ok(())
    };
    rule("FowlEngine Range DCS (TCP)", "TCP", o.dcs_port, None)?;
    rule("FowlEngine Range DCS (UDP)", "UDP", o.dcs_port, None)?;
    rule("FowlEngine Range SRS (TCP)", "TCP", o.srs_port, None)?;
    rule("FowlEngine Range SRS (UDP)", "UDP", o.srs_port, None)?;
    if !o.main_pc_ip.trim().is_empty() {
        rule("FowlEngine Range bfdb (main PC only)", "TCP", 8880, Some(o.main_pc_ip.trim()))?;
    }
    Ok(())
}

/// Let the bot's account change files under `dir` (the bot desanitizes
/// MissionScripting.lua and runs the DCS / SRS updaters).
fn grant_modify(dir: &Path) {
    let acct = bot_account();
    let out = hidden("icacls.exe").arg(dir).args(["/grant", &format!("{acct}:(OI)(CI)M"), "/Q"]).output();
    match out {
        Ok(o) if o.status.success() => note(format!("  {acct} may change {}", dir.display())),
        Ok(o) => warn(format!("icacls {}: {}", dir.display(), String::from_utf8_lossy(&o.stdout).trim())),
        Err(e) => warn(format!("icacls {}: {e}", dir.display())),
    }
}

// ---- 9. SRS, DCS, Tacview -----------------------------------------------------------------

fn install_srs(o: &NewBoxOptions) -> Result<Option<String>> {
    let dir = PathBuf::from(o.srs_dir.trim());
    if dir.join("Server").join("SRS-Server.exe").is_file() {
        note(format!("SRS is already at {}", dir.display()));
        return Ok(None);
    }
    phase("Downloading SRS (about 300 MB)");
    let c = http()?;
    let rel = github_release(&c, SRS_REPO, "", false)?;
    let tag = rel["tag_name"].as_str().unwrap_or("?").to_string();
    let asset = rel["assets"]
        .as_array()
        .and_then(|a| {
            a.iter().find(|x| {
                let n = x["name"].as_str().unwrap_or_default();
                n.starts_with("DCS-SimpleRadioStandalone-") && n.ends_with(".zip")
            })
        })
        .ok_or_else(|| anyhow!("SRS {tag} has no DCS-SimpleRadioStandalone-*.zip"))?;
    let url = asset["browser_download_url"].as_str().unwrap_or_default().to_string();
    let zip = downloads_dir().join(asset["name"].as_str().unwrap_or("srs.zip"));
    download(&c, &url, &zip)?;
    phase(&format!("Unpacking SRS {tag} into {}", dir.display()));
    unzip(&zip, &dir, false)?;
    grant_modify(&dir);
    let redist = dir.join("VC_redist.x64.exe");
    if redist.is_file() {
        let st = hidden(&redist.display().to_string()).args(["/install", "/quiet", "/norestart"]).status()?;
        // 1638: a newer one is already there; 3010: needs a reboot
        if !matches!(st.code(), Some(0 | 1638 | 3010)) {
            warn(format!("the Visual C++ runtime install exited with {:?}", st.code()));
        }
    }
    if !dir.join("Server").join("SRS-Server.exe").is_file() {
        bail!("the SRS zip did not hold Server\\SRS-Server.exe");
    }
    let _ = std::fs::remove_file(&zip);
    Ok(Some(tag))
}

fn dir_size(p: &Path) -> u64 {
    let mut total = 0;
    let mut stack = vec![p.to_path_buf()];
    while let Some(d) = stack.pop() {
        let Ok(rd) = std::fs::read_dir(&d) else { continue };
        for e in rd.flatten() {
            match e.file_type() {
                Ok(t) if t.is_dir() => stack.push(e.path()),
                Ok(_) => total += e.metadata().map(|m| m.len()).unwrap_or(0),
                Err(_) => {}
            }
        }
    }
    total
}

/// Run ED's updater until it exits, showing how much it has put down.
fn run_updater(updater: &Path, args: &[&str], dir: &Path, what: &str) -> Result<()> {
    phase(what);
    // a window of its own: ED's updater shows progress even with --quiet
    let mut child = std::process::Command::new(updater)
        .args(args)
        .current_dir(updater.parent().unwrap_or(dir))
        .spawn()
        .with_context(|| format!("starting {}", updater.display()))?;
    let mut last = std::time::Instant::now() - Duration::from_secs(60);
    loop {
        if let Some(st) = child.try_wait()? {
            note(format!("  the updater exited ({:?})", st.code()));
            break;
        }
        if is_cancelled() {
            let _ = child.kill();
            bail!("cancelled");
        }
        if last.elapsed() >= Duration::from_secs(15) {
            last = std::time::Instant::now();
            let n = dir_size(dir);
            set_done(n);
            with_job(|j| j.current = Some(format!("{} in {}", crate::backup::fmt_bytes(n), dir.display())));
        }
        std::thread::sleep(Duration::from_secs(1));
    }
    with_job(|j| j.current = None);
    Ok(())
}

fn install_dcs(o: &NewBoxOptions) -> Result<bool> {
    let dir = PathBuf::from(o.dcs_dir.trim());
    let bin = dir.join("bin");
    let server = bin.join("DCS_server.exe");
    let cfg_path = dir.join("autoupdate.cfg");
    if server.is_file() {
        // installed: add any module the range needs that isn't there
        let have: Vec<String> = std::fs::read_to_string(&cfg_path)
            .ok()
            .and_then(|t| serde_json::from_str::<serde_json::Value>(&t).ok())
            .and_then(|v| v["modules"].as_array().cloned())
            .unwrap_or_default()
            .iter()
            .filter_map(|m| m.as_str().map(str::to_string))
            .collect();
        let missing: Vec<&str> = DCS_MODULES.iter().copied().filter(|m| !have.iter().any(|h| h == m)).collect();
        if missing.is_empty() {
            note(format!("DCS World Server is already at {} with {}", dir.display(), DCS_MODULES.join(", ")));
            return Ok(false);
        }
        let mut args = vec!["--quiet", "install"];
        args.extend(missing.iter().copied());
        run_updater(&bin.join("DCS_updater.exe"), &args, &dir, &format!("Adding DCS modules: {}", missing.join(", ")))?;
        return Ok(true);
    }
    if let Some(free) = crate::backup::free_space(&dir.ancestors().last().map(Path::to_path_buf).unwrap_or_else(|| dir.clone())) {
        if free < 60 * 1024 * 1024 * 1024 {
            bail!("only {} free for DCS -- it needs about 60 GB with the Mariana Islands and the Supercarrier", crate::backup::fmt_bytes(free));
        }
    }
    phase("Downloading ED's updater");
    std::fs::create_dir_all(&bin)?;
    grant_modify(&dir);
    let c = http()?;
    let zip = downloads_dir().join("DCS_updater_64bit.zip");
    download(&c, DCS_UPDATER_URL, &zip)?;
    unzip(&zip, &bin, false)?;
    let updater = bin.join("DCS_updater.exe");
    if !updater.is_file() {
        bail!("the updater zip did not hold DCS_updater.exe");
    }
    // what the GUI installer would leave for the updater to read
    std::fs::write(dir.join("dcs_variant.txt"), "dcs_server.release")?;
    std::fs::create_dir_all(dir.join("Config"))?;
    std::fs::write(dir.join("Config").join("lang.txt"), "EN")?;
    std::fs::write(
        &cfg_path,
        serde_json::to_string(&serde_json::json!({
            "branch": "dcs_server.release",
            "arch": "x86_64",
            "lang": "EN",
            "modules": DCS_MODULES,
            "launch": null,
        }))?,
    )?;
    // the updater updates itself first; run a copy so it can
    let boot = bin.join("DCS_updater_bootstrap.exe");
    std::fs::copy(&updater, &boot)?;
    let r = run_updater(
        &boot,
        &["--quiet", "update"],
        &dir,
        "Installing DCS World Server with the Mariana Islands and the Supercarrier -- tens of GB, this takes a while",
    );
    let _ = std::fs::remove_file(&boot);
    r?;
    if !server.is_file() {
        bail!("ED's updater finished without bin\\DCS_server.exe -- see its log in {}", dir.display());
    }
    let _ = std::fs::remove_file(&zip);
    Ok(true)
}

fn install_tacview(o: &NewBoxOptions, done: &mut Vec<String>) {
    if tacview_dir().is_some() {
        note("Tacview is already installed".into());
        return;
    }
    let setup = o.tacview_setup.trim();
    if setup.is_empty() {
        next_step(format!(
            "Optional: Tacview recordings for the range site's downloads. Download TacviewSetup.exe from {TACVIEW_PAGE} (accept its licence), install it, and restart the bot."
        ));
        return;
    }
    phase("Installing Tacview");
    match hidden(setup).arg("/quiet").status() {
        Ok(_) if tacview_dir().is_some() => done.push("Tacview".into()),
        Ok(st) => {
            warn(format!("Tacview's installer exited ({:?}) but Tacview is not there", st.code()));
            next_step(format!("Run {setup} by hand."));
        }
        Err(e) => {
            warn(format!("Tacview's installer: {e}"));
            next_step(format!("Run {setup} by hand."));
        }
    }
}

/// The ED login dialog: DCS for this instance, on the desktop, once.
pub fn open_dcs_login(dcs_dir: &str, instance: &str) -> Result<()> {
    let server = PathBuf::from(dcs_dir.trim()).join("bin").join("DCS_server.exe");
    if !server.is_file() {
        bail!("DCS World Server is not installed at {dcs_dir}");
    }
    std::process::Command::new(&server)
        .args(["--server", "--norender", "-w", instance.trim()])
        .current_dir(server.parent().unwrap_or(Path::new(".")))
        .spawn()
        .context("starting DCS_server.exe")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generated_yaml_parses() {
        let dir = std::env::temp_dir().join(format!("fowl-newbox-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let mut o = NewBoxOptions::default();
        o.guild_id = "123".into();
        o.owner_id = "456".into();
        o.discord_token = "tok\"en".into();
        o.server_name = "[VS] Vector Strike | Training Range".into();
        o.channels.range_status = "789".into();
        o.ed_user = "pilot".into();
        o.ed_password = "p'w\"x".into();
        o.discord_client_secret = "s".into();
        o.overwrite_config = true;
        let home = dir.join("DCS.range");
        write_bot_config(&o, &dir.join("config"), &home, Some("dbpw"), "999").unwrap();
        let fe: serde_yaml::Value =
            serde_yaml::from_str(&std::fs::read_to_string(dir.join("config/plugins/fowlengine.yaml")).unwrap()).unwrap();
        let inst = &fe["DEFAULT"]["bfdb"]["instances"][0];
        assert_eq!(inst["kind"].as_str(), Some("range"));
        assert_eq!(inst["dcs_server_name"].as_str(), Some("[VS] Vector Strike | Training Range"));
        assert_eq!(fe["[VS] Vector Strike | Training Range"]["range_status_channel"].as_u64(), Some(789));
        assert!(fe["[VS] Vector Strike | Training Range"]["greenie_channel"].is_null());
        assert_eq!(fe["DEFAULT"]["bfdb"]["discord_client_id"].as_str(), Some("999"));
        let nodes: serde_yaml::Value =
            serde_yaml::from_str(&std::fs::read_to_string(dir.join("config/nodes.yaml")).unwrap()).unwrap();
        let n = &nodes[hostname().as_str()];
        assert_eq!(n["DCS"]["password"].as_str(), Some("p'w\"x"));
        assert_eq!(n["instances"]["DCS.range"]["extensions"]["BFBinaries"]["dll_name"].as_str(), Some("bfrange.dll"));
        assert!(n["database"]["url"].as_str().unwrap().contains(":dbpw@"));
        let bot: serde_yaml::Value =
            serde_yaml::from_str(&std::fs::read_to_string(dir.join("config/services/bot.yaml")).unwrap()).unwrap();
        assert_eq!(bot["token"].as_str(), Some("tok\"en"));
        // a second run keeps what is there
        o.overwrite_config = false;
        o.guild_id = "321".into();
        write_bot_config(&o, &dir.join("config"), &home, None, "").unwrap();
        let main: serde_yaml::Value =
            serde_yaml::from_str(&std::fs::read_to_string(dir.join("config/main.yaml")).unwrap()).unwrap();
        assert_eq!(main["guild_id"].as_u64(), Some(123));
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn server_settings_are_lua_safe() {
        assert_eq!(lua_str(r#"a\b"c"#), r#""a\\b\"c""#);
    }

    /// A manifest signed with `tauri signer` (as publish-release.ps1 does)
    /// verifies against its .pub, and not after a byte changes:
    ///   FOWL_SIGNED_MANIFEST_DIR=<dir with manifest.json, manifest.json.sig, key.pub>
    ///   cargo test --lib newbox::tests::signed_manifest -- --ignored
    #[test]
    #[ignore]
    fn signed_manifest() {
        let dir = PathBuf::from(std::env::var("FOWL_SIGNED_MANIFEST_DIR").expect("FOWL_SIGNED_MANIFEST_DIR"));
        let raw = std::fs::read(dir.join("manifest.json")).unwrap();
        let sig = std::fs::read(dir.join("manifest.json.sig")).unwrap();
        let key = std::fs::read_to_string(dir.join("key.pub")).unwrap();
        let (tag, files) = check_manifest(&raw, Some(&sig), &key).unwrap();
        assert_eq!(tag, "engine-test");
        assert!(files.contains_key("bfdb.exe"));
        let mut bad = raw.clone();
        bad[2] ^= 1;
        assert!(check_manifest(&bad, Some(&sig), &key).is_err());
    }

    /// A release folder (what `publish-release.ps1 -DryRun` leaves) installs
    /// through the wizard's own path, checked against the built-in key:
    ///   FOWL_ENGINE_RELEASE_DIR=<OutDir> cargo test --lib newbox::tests::engine_folder -- --ignored --nocapture
    #[test]
    #[ignore]
    fn engine_folder() {
        let rel = std::env::var("FOWL_ENGINE_RELEASE_DIR").expect("FOWL_ENGINE_RELEASE_DIR");
        let home = std::env::temp_dir().join(format!("fowl-newbox-engine-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&home);
        let mut o = NewBoxOptions::default();
        o.engine_source = "folder".into();
        o.engine_folder = rel;
        let tag = engine_files(&o, "folder", &home).unwrap();
        println!("installed {tag}");
        for name in RANGE_ENGINE_FILES {
            let p = engine_target(&home, name);
            assert!(p.is_file(), "{} missing", p.display());
            println!("  {} {} bytes", p.display(), p.metadata().unwrap().len());
        }
        // a second run finds them in place
        assert_eq!(engine_files(&o, "folder", &home).unwrap(), tag);
        let _ = std::fs::remove_dir_all(&home);
    }

    /// The latest engine release on GitHub installs through the wizard's own
    /// path with the built-in key (about 190 MB):
    ///   cargo test --lib newbox::tests::engine_github -- --ignored --nocapture
    #[test]
    #[ignore]
    fn engine_github() {
        let home = std::env::temp_dir().join(format!("fowl-newbox-gh-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&home);
        let o = NewBoxOptions::default();
        let tag = engine_files(&o, "github", &home).unwrap();
        println!("installed {tag} from {}", o.engine_repo);
        for name in RANGE_ENGINE_FILES {
            assert!(engine_target(&home, name).is_file(), "{name} missing");
        }
        let _ = std::fs::remove_dir_all(&home);
    }

    /// The real downloads: DCSServerBot's pinned source zip unpacks into a
    /// bot folder, SRS's latest release has its zip, ED's updater URL answers.
    ///   cargo test --lib newbox::tests::downloads -- --ignored --nocapture
    #[test]
    #[ignore]
    fn downloads() {
        let dir = std::env::temp_dir().join(format!("fowl-newbox-dl-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let c = http().unwrap();
        let zip = dir.join("bot.zip");
        download(&c, &format!("https://github.com/{BOT_REPO}/archive/refs/tags/{BOT_TAG}.zip"), &zip).unwrap();
        unzip(&zip, &dir.join("DCSServerBot"), true).unwrap();
        assert!(crate::bot::is_bot_dir(&dir.join("DCSServerBot")), "not a bot folder");
        assert!(dir.join("DCSServerBot").join("run.cmd").is_file());
        let rel = github_release(&c, SRS_REPO, "", false).unwrap();
        let names: Vec<String> =
            rel["assets"].as_array().unwrap().iter().filter_map(|a| a["name"].as_str().map(str::to_string)).collect();
        println!("SRS {} assets: {names:?}", rel["tag_name"]);
        assert!(names.iter().any(|n| n.starts_with("DCS-SimpleRadioStandalone-") && n.ends_with(".zip")));
        let head = c.head(DCS_UPDATER_URL).send().unwrap();
        println!("ED updater: {} {:?} bytes", head.status(), head.content_length());
        assert!(head.status().is_success());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn unsigned_manifest_reads_files() {
        let raw = br#"{"tag":"engine-x","files":{"bfdb.exe":{"sha256":"AB"},"bfrange.dll":{"sha256":"cd"}}}"#;
        let (tag, files) = check_manifest(raw, None, "").unwrap();
        assert_eq!(tag, "engine-x");
        assert_eq!(files["bfdb.exe"], "ab");
        assert!(check_manifest(raw, None, "RWQ-not-a-key").is_err());
    }
}
