//! DCSServerBot's own config files (`<bot>\config\**\*.yaml`), edited in place.
//!
//! The dashboard's OPS page changes fowlengine.yaml through the bot's OPS API,
//! which is reachable from the web and so refuses every key that could run a
//! program or redirect an update. This app runs ON the box as its
//! administrator, so it edits the files themselves -- protected keys included
//! -- with the guard rails a hand edit lacks: a conflict check against what
//! was read, a YAML parse before anything is written, a backup of the previous
//! version, and an atomic replace.

use anyhow::{anyhow, bail, Context, Result};
use base64::Engine;
use serde::Serialize;
use serde_yaml::Value;
use sha2::{Digest, Sha256};
use std::io::Write;
use std::path::{Component, Path, PathBuf};

/// Error prefixes the UI keys on (a Tauri command error is just a string).
pub const CONFLICT: &str = "CONFLICT";
pub const MANUAL: &str = "MANUAL";

pub const FOWLENGINE: &str = "plugins/fowlengine.yaml";
pub const WEBSERVICE: &str = "services/webservice.yaml";
const BACKUP_DIR: &str = ".fowl-backups";
const KEEP_BACKUPS: usize = 20;
const MAX_BYTES: u64 = 4 * 1024 * 1024;

// ---- where the files are --------------------------------------------------------

/// `name` inside `dir`, matched case-insensitively (the live box has
/// `config`, a fresh install `Config`; Windows doesn't care, tests on Linux do).
fn child_ci(dir: &Path, name: &str) -> PathBuf {
    if let Ok(rd) = std::fs::read_dir(dir) {
        for e in rd.flatten() {
            if e.file_name().to_string_lossy().eq_ignore_ascii_case(name) {
                return e.path();
            }
        }
    }
    dir.join(name)
}

pub fn config_dir(bot_dir: &Path) -> PathBuf {
    child_ci(bot_dir, "config")
}

/// The configured bot's config folder.
pub fn current_config_dir() -> Result<PathBuf> {
    let bot = crate::config::ManagerConfig::load()
        .bot_dir()
        .ok_or_else(|| anyhow!("no DCSServerBot folder configured -- run Setup"))?;
    Ok(config_dir(&bot))
}

/// `rel` with the on-disk spelling of each part, if it exists -- so the
/// checklist names a file exactly as the file list does.
fn actual_rel(cfg_dir: &Path, rel: &str) -> Option<String> {
    let mut p = cfg_dir.to_path_buf();
    let mut parts = Vec::new();
    for part in rel.split('/') {
        p = child_ci(&p, part);
        parts.push(p.file_name()?.to_string_lossy().to_string());
    }
    p.is_file().then(|| parts.join("/"))
}

fn is_yaml(name: &str) -> bool {
    let n = name.to_ascii_lowercase();
    n.ends_with(".yaml") || n.ends_with(".yml")
}

/// Folders never shown or opened: `.secret` (the bot's Discord/DB tokens),
/// our own `.fowl-backups`, anything hidden, and backup copies.
fn skipped_dir(name: &str) -> bool {
    name.starts_with('.') || name.to_ascii_lowercase().contains("backup")
}

pub fn display(p: &Path) -> String {
    let s = p.display().to_string();
    s.strip_prefix(r"\\?\").map(String::from).unwrap_or(s)
}

/// A config file named by the UI -> its real path, or why not. The name is
/// only ever a relative path under the config folder: no `..`, no drive or
/// stream (`:`), no hidden or backup folder, yaml/yml only, an existing
/// regular file (not a link), and still inside the folder once every link on
/// the way has been resolved.
pub fn resolve(cfg_dir: &Path, rel: &str) -> Result<PathBuf> {
    if rel.is_empty() || rel.len() > 400 || rel.contains('\0') {
        bail!("bad config file name");
    }
    let parts: Vec<&str> = rel.split(['/', '\\']).collect();
    for (i, p) in parts.iter().enumerate() {
        if p.is_empty() || *p == "." || *p == ".." || p.contains(':') {
            bail!("bad config file name {rel:?}");
        }
        if p.eq_ignore_ascii_case(".secret") {
            bail!("{rel}: .secret holds the bot's tokens -- not editable here");
        }
        if p.starts_with('.') || (i + 1 < parts.len() && skipped_dir(p)) {
            bail!("{rel}: hidden and backup folders are not editable here");
        }
    }
    if !is_yaml(parts[parts.len() - 1]) {
        bail!("{rel}: only .yaml / .yml files can be edited here");
    }
    let root = std::fs::canonicalize(cfg_dir)
        .with_context(|| format!("{} not found -- is the DCSServerBot folder right?", display(cfg_dir)))?;
    let mut p = root.clone();
    for part in &parts {
        p.push(part);
    }
    let meta = std::fs::symlink_metadata(&p).map_err(|_| anyhow!("{rel} not found"))?;
    if meta.file_type().is_symlink() {
        bail!("{rel} is a link -- not following it");
    }
    if !meta.is_file() {
        bail!("{rel} is not a file");
    }
    let real = std::fs::canonicalize(&p)?;
    let inside = real
        .strip_prefix(&root)
        .map_err(|_| anyhow!("{rel} resolves outside the bot's config folder"))?;
    if inside
        .components()
        .any(|c| matches!(c, Component::Normal(n) if n.to_string_lossy().starts_with('.')))
    {
        bail!("{rel} resolves into a hidden folder");
    }
    Ok(real)
}

// ---- list / read -----------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct ConfigFile {
    /// '/'-separated, relative to the config folder
    pub rel: String,
    pub size: u64,
    /// UTC, RFC 3339
    pub modified: Option<String>,
    pub label: String,
}

fn label(rel: &str) -> String {
    let l = rel.to_ascii_lowercase();
    let stem = rel.rsplit('/').next().unwrap_or(rel);
    let stem = stem.rsplit_once('.').map(|(s, _)| s).unwrap_or(stem);
    match l.as_str() {
        "plugins/fowlengine.yaml" => "Fowl Engine plugin".into(),
        "services/webservice.yaml" => "WebService (OPS API listener)".into(),
        "main.yaml" => "Bot main settings".into(),
        "nodes.yaml" => "Nodes: DCS installs + instances".into(),
        "servers.yaml" => "DCS servers".into(),
        "services/bot.yaml" => "Discord bot".into(),
        _ if l.starts_with("plugins/") => format!("plugin {stem}"),
        _ if l.starts_with("services/") => format!("service {stem}"),
        _ => stem.to_string(),
    }
}

fn walk(dir: &Path, prefix: &str, depth: usize, out: &mut Vec<ConfigFile>) {
    if depth > 6 || out.len() >= 500 {
        return;
    }
    let Ok(rd) = std::fs::read_dir(dir) else { return };
    for e in rd.flatten() {
        let name = e.file_name().to_string_lossy().to_string();
        let Ok(ft) = e.file_type() else { continue };
        if name.starts_with('.') {
            continue;
        }
        let rel = if prefix.is_empty() { name.clone() } else { format!("{prefix}/{name}") };
        // links (to files or folders) are skipped: resolve() won't open them either
        if ft.is_dir() {
            if !skipped_dir(&name) {
                walk(&e.path(), &rel, depth + 1, out);
            }
        } else if ft.is_file() && is_yaml(&name) {
            let meta = e.metadata().ok();
            out.push(ConfigFile {
                label: label(&rel),
                size: meta.as_ref().map(|m| m.len()).unwrap_or(0),
                modified: meta
                    .and_then(|m| m.modified().ok())
                    .map(|t| chrono::DateTime::<chrono::Utc>::from(t).to_rfc3339()),
                rel,
            });
        }
    }
}

/// Every YAML file of the bot's config, the two the Fowl Engine setup lives
/// in first.
pub fn list(cfg_dir: &Path) -> Result<Vec<ConfigFile>> {
    if !cfg_dir.is_dir() {
        bail!("{} not found -- is the DCSServerBot folder right?", display(cfg_dir));
    }
    let mut out = Vec::new();
    walk(cfg_dir, "", 0, &mut out);
    let rank = |r: &str| {
        if r.eq_ignore_ascii_case(FOWLENGINE) {
            0
        } else if r.eq_ignore_ascii_case(WEBSERVICE) {
            1
        } else {
            2
        }
    };
    out.sort_by_cached_key(|f| (rank(&f.rel), f.rel.to_ascii_lowercase()));
    Ok(out)
}

fn sha_hex(b: &[u8]) -> String {
    hex::encode(Sha256::digest(b))
}

#[derive(Debug, Clone, Serialize)]
pub struct ConfigText {
    pub rel: String,
    pub path: String,
    pub text: String,
    /// of the bytes on disk; hand it back to write() to detect a conflict
    pub sha256: String,
}

fn read_text(p: &Path, rel: &str) -> Result<(Vec<u8>, String)> {
    if std::fs::metadata(p)?.len() > MAX_BYTES {
        bail!("{rel} is over 4 MB -- not a config file this editor handles");
    }
    let bytes = std::fs::read(p).map_err(|e| io_err(e, "reading", p))?;
    let text = String::from_utf8(bytes.clone())
        .map_err(|_| anyhow!("{rel} is not UTF-8 text -- edit it with a text editor instead"))?;
    Ok((bytes, text))
}

pub fn read(cfg_dir: &Path, rel: &str) -> Result<ConfigText> {
    let p = resolve(cfg_dir, rel)?;
    let (bytes, text) = read_text(&p, rel)?;
    Ok(ConfigText { rel: rel.to_string(), path: display(&p), text, sha256: sha_hex(&bytes) })
}

// ---- validate --------------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct Diag {
    /// 1-based
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub message: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct Validation {
    pub ok: bool,
    pub errors: Vec<Diag>,
    pub warnings: Vec<Diag>,
}

/// Parse errors (with where) and a few warnings. Unknown keys are fine: the
/// bot and every plugin read their own, and this editor doesn't know them all.
pub fn validate(rel: &str, text: &str) -> Validation {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    match serde_yaml::from_str::<Value>(text) {
        Err(e) => {
            let loc = e.location();
            let mut message = e.to_string();
            // the location is reported separately
            if let Some(i) = message.find(" at line ") {
                message.truncate(i);
            }
            errors.push(Diag { line: loc.as_ref().map(|l| l.line()), column: loc.map(|l| l.column()), message });
            // libyaml's words for a tab are cryptic; point at them
            for (i, line) in text.lines().enumerate() {
                if line.trim_start_matches(' ').starts_with('\t') {
                    warnings.push(Diag {
                        line: Some(i + 1),
                        column: Some(line.len() - line.trim_start_matches(' ').len() + 1),
                        message: "tab in the indentation -- YAML indents with spaces only".into(),
                    });
                }
            }
        }
        Ok(Value::Null) => warnings.push(Diag { line: None, column: None, message: "the file is empty".into() }),
        Ok(Value::Mapping(m)) => {
            let fowl = rel.eq_ignore_ascii_case(FOWLENGINE) || rel.eq_ignore_ascii_case(WEBSERVICE);
            if fowl && !m.contains_key("DEFAULT") {
                warnings.push(Diag {
                    line: None,
                    column: None,
                    message: "no DEFAULT: section -- the bot reads these settings from DEFAULT".into(),
                });
            }
        }
        Ok(_) => warnings.push(Diag {
            line: None,
            column: None,
            message: "the top level is not `key: value` sections, which is what DCSServerBot expects".into(),
        }),
    }
    Validation { ok: errors.is_empty(), errors, warnings }
}

// ---- write -----------------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct Written {
    pub sha256: String,
    pub backup: Option<String>,
}

fn io_err(e: std::io::Error, what: &str, p: &Path) -> anyhow::Error {
    if e.kind() == std::io::ErrorKind::PermissionDenied {
        anyhow!(
            "permission denied {what} {} -- run Fowl Engine Manager as administrator \
             (right-click -> Run as administrator)",
            display(p)
        )
    } else {
        anyhow!("{what} {}: {e}", display(p))
    }
}

/// Keep the previous version as `.fowl-backups\<rel with / -> __>.<UTC time>`,
/// newest KEEP_BACKUPS per file.
fn backup(root: &Path, real: &Path, bytes: &[u8]) -> Result<PathBuf> {
    let flat = real
        .strip_prefix(root)
        .map(|r| r.components().map(|c| c.as_os_str().to_string_lossy().to_string()).collect::<Vec<_>>().join("__"))
        .unwrap_or_else(|_| real.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default());
    let dir = root.join(BACKUP_DIR);
    std::fs::create_dir_all(&dir).map_err(|e| io_err(e, "creating", &dir))?;
    let stamp = chrono::Utc::now().format("%Y%m%dT%H%M%S%.3fZ").to_string();
    let mut dst = dir.join(format!("{flat}.{stamp}"));
    let mut n = 1;
    while dst.exists() {
        dst = dir.join(format!("{flat}.{stamp}-{n}"));
        n += 1;
    }
    std::fs::write(&dst, bytes).map_err(|e| io_err(e, "writing", &dst))?;

    let prefix = format!("{flat}.");
    let mut mine: Vec<PathBuf> = std::fs::read_dir(&dir)
        .map(|rd| {
            rd.flatten()
                .filter(|e| {
                    let n = e.file_name().to_string_lossy().to_string();
                    n.strip_prefix(&prefix).map(|ts| ts.starts_with(|c: char| c.is_ascii_digit())).unwrap_or(false)
                })
                .map(|e| e.path())
                .collect()
        })
        .unwrap_or_default();
    // the stamps sort as text
    mine.sort();
    while mine.len() > KEEP_BACKUPS {
        let _ = std::fs::remove_file(mine.remove(0));
    }
    Ok(dst)
}

/// Write next to the target, flush to disk, then rename over it: the bot (or
/// a crash) never sees half a file.
fn atomic_write(dest: &Path, bytes: &[u8]) -> Result<()> {
    let dir = dest.parent().ok_or_else(|| anyhow!("no parent folder"))?;
    let name = dest.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default();
    let tmp = dir.join(format!(".{name}.fowl-tmp-{}", std::process::id()));
    let res = (|| -> std::io::Result<()> {
        let mut f = std::fs::File::create(&tmp)?;
        f.write_all(bytes)?;
        f.sync_all()?;
        drop(f);
        std::fs::rename(&tmp, dest)
    })();
    if let Err(e) = res {
        let _ = std::fs::remove_file(&tmp);
        return Err(io_err(e, "writing", dest));
    }
    Ok(())
}

/// Replace a config file with `text`, exactly as given (line endings too),
/// if it is still what was read (`expected_sha`) and parses.
pub fn write(cfg_dir: &Path, rel: &str, text: &str, expected_sha: &str) -> Result<Written> {
    let p = resolve(cfg_dir, rel)?;
    let (cur, _) = read_text(&p, rel)?;
    if !sha_hex(&cur).eq_ignore_ascii_case(expected_sha.trim()) {
        bail!(
            "{CONFLICT}: {rel} was changed on disk since it was opened (by the bot, the OPS page or \
             another editor) -- reload it, or copy your edits first"
        );
    }
    let v = validate(rel, text);
    if let Some(e) = v.errors.first() {
        let at = e.line.map(|l| format!(" (line {l})")).unwrap_or_default();
        bail!("not saved -- {rel} is not valid YAML: {}{at}", e.message);
    }
    if cur == text.as_bytes() {
        return Ok(Written { sha256: sha_hex(&cur), backup: None });
    }
    let root = std::fs::canonicalize(cfg_dir)?;
    let b = backup(&root, &p, &cur)?;
    atomic_write(&p, text.as_bytes())?;
    Ok(Written { sha256: sha_hex(text.as_bytes()), backup: Some(display(&b)) })
}

// ---- targeted, comment-preserving edits ------------------------------------------
//
// Re-serialising the file with serde_yaml would drop every comment -- and
// fowlengine.yaml is mostly comments. So the edit is textual: find the key's
// line by indentation, change that one line (or add one), then prove with a
// real parse that exactly that key changed. Anything the line walker can't be
// sure about is refused and left to the editor.

/// What a targeted edit changes, for the confirmation shown first.
#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Change {
    /// 1-based line of the first changed / inserted line
    pub line: usize,
    pub before: Vec<String>,
    pub after: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
enum Seg {
    Key(String),
    Seq,
}

#[derive(Debug, Clone)]
enum Kind {
    Blank,
    Comment,
    /// part of a block scalar / multi-line value: never structure
    Opaque,
    Seq,
    /// `colon` = byte offset just past the ':' in the raw line
    Key { path: Vec<Seg>, colon: usize },
    Other,
}

#[derive(Debug, Clone)]
struct Line {
    raw: String,
    indent: usize,
    kind: Kind,
}

/// A quoted scalar at the start of `s`: (its text, offset after the closing quote).
fn parse_quoted(s: &str) -> Option<(String, usize)> {
    let q = s.chars().next()?;
    let mut out = String::new();
    let mut it = s.char_indices().skip(1).peekable();
    while let Some((i, c)) = it.next() {
        if q == '\'' && c == '\'' {
            if matches!(it.peek(), Some((_, '\''))) {
                it.next();
                out.push('\'');
                continue;
            }
            return Some((out, i + 1));
        }
        if q == '"' && c == '\\' {
            match it.next() {
                Some((_, 'n')) => out.push('\n'),
                Some((_, 't')) => out.push('\t'),
                Some((_, e)) => out.push(e),
                None => return None,
            }
            continue;
        }
        if q == '"' && c == '"' {
            return Some((out, i + 1));
        }
        out.push(c);
    }
    None
}

/// `key:` at the start of `content` (indent already stripped): the key and
/// the offset just past the colon.
fn parse_key(content: &str) -> Option<(String, usize)> {
    let first = content.chars().next()?;
    if "#[]{},&*!|>%@`?".contains(first) || content.starts_with("- ") || content == "-" {
        return None;
    }
    let colon_ok = |at: usize| matches!(content[at + 1..].chars().next(), None | Some(' ') | Some('\t'));
    if first == '"' || first == '\'' {
        let (key, end) = parse_quoted(content)?;
        let rest = &content[end..];
        let ws = rest.len() - rest.trim_start_matches([' ', '\t']).len();
        let at = end + ws;
        return (content[at..].starts_with(':') && colon_ok(at)).then(|| (key, at + 1));
    }
    let mut prev_space = false;
    for (i, c) in content.char_indices() {
        if c == '#' && prev_space {
            return None;
        }
        if c == ':' && colon_ok(i) {
            let key = content[..i].trim_end();
            return (!key.is_empty()).then(|| (key.to_string(), i + 1));
        }
        prev_space = c == ' ' || c == '\t';
    }
    None
}

/// What follows a key's colon.
enum Val {
    /// nothing, or only a comment (starting at that offset)
    Empty(Option<usize>),
    /// a one-line scalar at [start, end)
    Scalar(usize, usize),
    Complex(&'static str),
}

fn classify(v: &str) -> Val {
    let lead = v.len() - v.trim_start_matches([' ', '\t']).len();
    let t = &v[lead..];
    match t.chars().next() {
        None => Val::Empty(None),
        Some('#') => Val::Empty(Some(lead)),
        Some('|') | Some('>') => Val::Complex("is a block of text"),
        Some('[') | Some('{') => Val::Complex("is written inline (flow style)"),
        Some('&') | Some('*') | Some('!') => Val::Complex("uses a YAML anchor, alias or tag"),
        Some('"') | Some('\'') => match parse_quoted(t) {
            Some((_, end)) => Val::Scalar(lead, lead + end),
            None => Val::Complex("is a quoted value spanning several lines"),
        },
        Some(_) => {
            let b = t.as_bytes();
            let end = (1..b.len()).find(|&i| b[i] == b'#' && (b[i - 1] == b' ' || b[i - 1] == b'\t')).unwrap_or(b.len());
            Val::Scalar(lead, lead + t[..end].trim_end().len())
        }
    }
}

/// The line walker: indentation, kind and (for keys) the full key path.
fn scan(lines: &[&str]) -> Vec<Line> {
    let mut out = Vec::with_capacity(lines.len());
    let mut stack: Vec<(usize, Seg)> = Vec::new();
    // lines indented deeper than this belong to the previous value
    let mut opaque_deeper_than: Option<usize> = None;
    for raw in lines {
        let raw = raw.strip_suffix('\r').unwrap_or(raw);
        let content = raw.trim_start_matches(' ');
        let indent = raw.len() - content.len();
        let blank = content.trim().is_empty();
        if let Some(d) = opaque_deeper_than {
            if blank || indent > d {
                out.push(Line { raw: raw.into(), indent, kind: Kind::Opaque });
                continue;
            }
            opaque_deeper_than = None;
        }
        let kind = if blank {
            Kind::Blank
        } else if content.starts_with('#') {
            Kind::Comment
        } else if content == "-" || content.starts_with("- ") {
            while stack.last().map(|(i, _)| *i >= indent).unwrap_or(false) {
                stack.pop();
            }
            stack.push((indent, Seg::Seq));
            if !matches!(classify(&content[1..]), Val::Empty(_) | Val::Scalar(..)) {
                opaque_deeper_than = Some(indent);
            }
            Kind::Seq
        } else if let Some((key, colon)) = parse_key(content) {
            while stack.last().map(|(i, _)| *i >= indent).unwrap_or(false) {
                stack.pop();
            }
            let mut path: Vec<Seg> = stack.iter().map(|(_, s)| s.clone()).collect();
            path.push(Seg::Key(key.clone()));
            stack.push((indent, Seg::Key(key)));
            if let Val::Complex(_) = classify(&content[colon..]) {
                opaque_deeper_than = Some(indent);
            }
            Kind::Key { path, colon: indent + colon }
        } else {
            Kind::Other
        };
        out.push(Line { raw: raw.into(), indent, kind });
    }
    out
}

/// A string as a YAML scalar that can't be misread (a number, a bool, a
/// comment, ...): double quotes, or single quotes when it has backslashes
/// (Windows paths read better unescaped).
pub fn quote(v: &str) -> String {
    let ctrl = v.chars().any(|c| c.is_control());
    if !ctrl && !v.contains('\\') {
        format!("\"{}\"", v.replace('"', "\\\""))
    } else if !ctrl {
        format!("'{}'", v.replace('\'', "''"))
    } else {
        let mut s = String::from("\"");
        for c in v.chars() {
            match c {
                '"' => s.push_str("\\\""),
                '\\' => s.push_str("\\\\"),
                '\n' => s.push_str("\\n"),
                '\t' => s.push_str("\\t"),
                '\r' => s.push_str("\\r"),
                c if c.is_control() => s.push_str(&format!("\\u{:04X}", c as u32)),
                c => s.push(c),
            }
        }
        s.push('"');
        s
    }
}

fn key_text(k: &str) -> String {
    let plain = !k.is_empty()
        && k.chars().all(|c| c.is_ascii_alphanumeric() || "_-.".contains(c))
        && !k.starts_with('-');
    if plain {
        k.to_string()
    } else {
        quote(k)
    }
}

/// `raw` (a `key: ...` line whose colon ends at `colon`) with its value
/// replaced, keeping any trailing comment.
fn replace_value(raw: &str, colon: usize, value: &str) -> std::result::Result<String, &'static str> {
    let head = &raw[..colon];
    let v = &raw[colon..];
    match classify(v) {
        Val::Empty(None) => Ok(format!("{head} {}", quote(value))),
        Val::Empty(Some(c)) => Ok(format!("{head} {} {}", quote(value), &v[c..])),
        Val::Scalar(_, end) => {
            let tail = &v[end..];
            let tail = if tail.trim().is_empty() { "" } else { tail };
            Ok(format!("{head} {}{tail}", quote(value)))
        }
        Val::Complex(why) => Err(why),
    }
}

fn path_is(p: &[Seg], want: &[String]) -> bool {
    p.len() == want.len() && p.iter().zip(want).all(|(s, w)| matches!(s, Seg::Key(k) if k == w))
}

/// The textual edit: `path` set to the string `value`. Returns the new text
/// and what changed. Not verified here -- plan_set() does that.
fn set_scalar(text: &str, path: &[String], value: &str) -> Result<(String, Change)> {
    let eol = if text.contains("\r\n") { "\r\n" } else { "\n" };
    let raw_lines: Vec<&str> = text.split('\n').collect();
    let lines = scan(&raw_lines);
    let n = path.len();
    let dotted = path.join(".");

    // the longest prefix of the path that exists as a line
    let mut found: Option<(usize, usize)> = None; // (prefix len, line index)
    for k in (1..=n).rev() {
        if let Some(i) = lines.iter().position(|l| matches!(&l.kind, Kind::Key { path: p, .. } if path_is(p, &path[..k]))) {
            found = Some((k, i));
            break;
        }
    }
    let content_after = |i: usize| lines.iter().enumerate().skip(i + 1).find(|(_, l)| !matches!(l.kind, Kind::Blank | Kind::Comment));

    let mut out: Vec<String> = lines.iter().map(|l| l.raw.clone()).collect();

    // 1. the key is there: change its value in place
    if let Some((_, i)) = found.filter(|(k, _)| *k == n) {
        let l = &lines[i];
        let Kind::Key { colon, .. } = l.kind else { unreachable!() };
        if let Val::Empty(_) = classify(&l.raw[colon..]) {
            if let Some((_, next)) = content_after(i) {
                let child = next.indent > l.indent || (matches!(next.kind, Kind::Seq) && next.indent == l.indent);
                if child {
                    bail!("{dotted} is a section (it has entries under it), not a single value");
                }
            }
        }
        let new = replace_value(&l.raw, colon, value).map_err(|why| anyhow!("{dotted} {why}"))?;
        out[i] = new.clone();
        let change = Change { line: i + 1, before: vec![l.raw.clone()], after: vec![new] };
        return Ok((out.join(eol), change));
    }

    // 2. the parent (or the root) is there, the rest isn't
    let (have, parent) = match found {
        Some((k, i)) => (k, Some(i)),
        None => (0, None),
    };
    let parent_indent: Option<usize> = parent.map(|i| lines[i].indent);
    if let Some(pi) = parent {
        let l = &lines[pi];
        let Kind::Key { colon, .. } = l.kind else { unreachable!() };
        if !matches!(classify(&l.raw[colon..]), Val::Empty(_)) {
            bail!("{} holds a value (or an inline {{...}} / [...]), not a section of its own", path[..have].join("."));
        }
    }
    // the parent's block: every line up to the next one indented no deeper
    let start = parent.map(|i| i + 1).unwrap_or(0);
    let end = match parent_indent {
        Some(pi) => lines
            .iter()
            .enumerate()
            .skip(start)
            .find(|(_, l)| !matches!(l.kind, Kind::Blank | Kind::Comment | Kind::Opaque) && l.indent <= pi)
            .map(|(j, _)| j)
            .unwrap_or(lines.len()),
        None => lines.len(),
    };
    // how deep this document indents, from its first nested key
    let step = lines
        .iter()
        .enumerate()
        .find_map(|(j, l)| {
            let Kind::Key { colon, .. } = l.kind else { return None };
            if !matches!(classify(&l.raw[colon..]), Val::Empty(_)) {
                return None;
            }
            content_after(j).map(|(_, c)| c).filter(|c| c.indent > l.indent).map(|c| c.indent - l.indent)
        })
        .unwrap_or(2);
    let first_child = lines[start..end].iter().find(|l| !matches!(l.kind, Kind::Blank | Kind::Comment | Kind::Opaque));
    if let Some(c) = first_child {
        if matches!(c.kind, Kind::Seq) {
            bail!("{} is a list, not a section", if have == 0 { "the file".to_string() } else { path[..have].join(".") });
        }
    }
    let child_indent = match (first_child, parent_indent) {
        (Some(c), _) => c.indent,
        (None, Some(pi)) => pi + step,
        (None, None) => 0,
    };

    // 2a. the leaf is there, commented out (the sample ships `# api_key: ""`):
    //     un-comment it, if it's the only such line in the block
    if have == n - 1 {
        let leaf = &path[n - 1];
        let hits: Vec<(usize, String, usize)> = (start..end)
            .filter(|&j| matches!(lines[j].kind, Kind::Comment) && lines[j].indent == child_indent)
            .filter_map(|j| {
                let rest = &lines[j].raw[child_indent + 1..];
                let rest = rest.strip_prefix(' ').unwrap_or(rest);
                let (key, colon) = parse_key(rest)?;
                (&key == leaf).then(|| (j, format!("{}{rest}", " ".repeat(child_indent)), child_indent + colon))
            })
            .collect();
        if let [(j, uncommented, colon)] = hits.as_slice() {
            if let Ok(new) = replace_value(uncommented, *colon, value) {
                out[*j] = new.clone();
                let change = Change { line: j + 1, before: vec![lines[*j].raw.clone()], after: vec![new] };
                return Ok((out.join(eol), change));
            }
        }
    }

    // 2b. add it (and any missing sections) as the parent's first entry; at
    //     the top level, at the end of the file
    let mut add = Vec::new();
    for (d, key) in path[have..].iter().enumerate() {
        let ind = " ".repeat(child_indent + d * step);
        if have + d + 1 < n {
            add.push(format!("{ind}{}:", key_text(key)));
        } else {
            add.push(format!("{ind}{}: {}", key_text(key), quote(value)));
        }
    }
    let at = match parent {
        Some(pi) => pi + 1,
        None => {
            // before the empty string a trailing newline leaves in the split
            if out.last().map(|l| l.is_empty()).unwrap_or(false) {
                out.len() - 1
            } else {
                out.len()
            }
        }
    };
    out.splice(at..at, add.iter().cloned());
    Ok((out.join(eol), Change { line: at + 1, before: vec![], after: add }))
}

/// Structural equality that ignores key order (a key added as a section's
/// first entry is still the same document).
fn same(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Mapping(x), Value::Mapping(y)) => {
            x.len() == y.len() && x.iter().all(|(k, v)| y.get(k).map(|w| same(v, w)).unwrap_or(false))
        }
        (Value::Sequence(x), Value::Sequence(y)) => x.len() == y.len() && x.iter().zip(y).all(|(v, w)| same(v, w)),
        (Value::Tagged(x), Value::Tagged(y)) => x.tag == y.tag && same(&x.value, &y.value),
        _ => a == b,
    }
}

fn set_path(doc: &mut Value, path: &[String], value: Value) -> Result<()> {
    let mut cur = doc;
    for (i, k) in path.iter().enumerate() {
        if cur.is_null() {
            *cur = Value::Mapping(Default::default());
        }
        let m = cur.as_mapping_mut().ok_or_else(|| anyhow!("{} is not a section", path[..i].join(".")))?;
        if i + 1 == path.len() {
            m.insert(Value::String(k.clone()), value);
            return Ok(());
        }
        cur = m.entry(Value::String(k.clone())).or_insert(Value::Null);
    }
    Ok(())
}

/// The whole targeted edit, proven: the result parses, `path` now holds
/// exactly `value`, and nothing else in the document changed.
pub fn plan_set(text: &str, path: &[String], value: &str) -> Result<(String, Change)> {
    if path.is_empty() || path.iter().any(|k| k.trim().is_empty()) {
        bail!("empty key path");
    }
    let old: Value = serde_yaml::from_str(text)
        .map_err(|e| anyhow!("{MANUAL}: the file isn't valid YAML ({e}) -- fix it in the editor first"))?;
    let (new_text, change) =
        set_scalar(text, path, value).map_err(|e| anyhow!("{MANUAL}: {e:#} -- change it by hand in the editor"))?;
    let new: Value = serde_yaml::from_str(&new_text).map_err(|e| {
        anyhow!("{MANUAL}: the automatic edit wouldn't parse ({e}) -- nothing was written; change it by hand in the editor")
    })?;
    let mut expect = old;
    set_path(&mut expect, path, Value::String(value.to_string()))
        .map_err(|e| anyhow!("{MANUAL}: {e:#} -- change it by hand in the editor"))?;
    if !same(&expect, &new) {
        bail!(
            "{MANUAL}: the automatic edit of {} would have changed more than that key -- nothing was \
             written; change it by hand in the editor",
            path.join(".")
        );
    }
    Ok((new_text, change))
}

// ---- secrets, masking ------------------------------------------------------------

pub fn is_secret_key(name: &str) -> bool {
    let n = name.to_ascii_lowercase();
    ["key", "secret", "password", "token"].iter().any(|w| n.contains(w))
}

pub fn mask(v: &str) -> String {
    let n = v.chars().count();
    match n {
        0 => "(empty)".into(),
        1..=8 => format!("•••• ({n} chars)"),
        _ => format!("{}… ({n} chars)", v.chars().take(4).collect::<String>()),
    }
}

/// A changed line for display, the value of a secret key masked.
fn mask_line(line: &str) -> String {
    let content = line.trim_start_matches(' ');
    let indent = line.len() - content.len();
    let Some((key, colon)) = parse_key(content) else { return line.to_string() };
    if !is_secret_key(&key) {
        return line.to_string();
    }
    let v = &content[colon..];
    match classify(v) {
        Val::Scalar(s, e) => {
            let tok = &v[s..e];
            let inner = parse_quoted(tok).map(|(t, _)| t).unwrap_or_else(|| tok.to_string());
            format!("{}{}{}{}", &line[..indent + colon], &v[..s], mask(&inner), &v[e..])
        }
        _ => line.to_string(),
    }
}

pub fn mask_change(c: &Change) -> Change {
    Change {
        line: c.line,
        before: c.before.iter().map(|l| mask_line(l)).collect(),
        after: c.after.iter().map(|l| mask_line(l)).collect(),
    }
}

/// 32 random bytes, URL-safe base64 -- what the sample's
/// `secrets.token_urlsafe(32)` makes.
pub fn generate_secret() -> Result<String> {
    let mut b = [0u8; 32];
    getrandom::getrandom(&mut b).map_err(|e| anyhow!("no randomness from the OS: {e}"))?;
    Ok(base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(b))
}

#[derive(Debug, Clone, Serialize)]
pub struct PublicKey {
    /// the `RW...` line -- what goes into autoupdate.public_key
    pub key: String,
    /// as minisign prints it
    pub key_id: String,
    pub path: String,
}

/// A minisign public key in any form the plugin accepts (the bare `RW...`
/// line, a minisign .pub, or `tauri signer`'s base64-wrapped .pub) -> the
/// bare line and its key id.
pub fn parse_public_key(text: &str) -> Result<(String, String)> {
    let b64 = base64::engine::general_purpose::STANDARD;
    let mut t = text.trim().to_string();
    if !t.contains('\n') && !t.starts_with("RW") && !t.starts_with("untrusted comment:") {
        if let Some(inner) = b64.decode(&t).ok().and_then(|b| String::from_utf8(b).ok()) {
            if inner.starts_with("untrusted comment:") {
                t = inner.trim().to_string();
            }
        }
    }
    if t.to_ascii_lowercase().contains("secret key") {
        bail!("that is the SECRET key -- pick the .pub file next to it (and keep the secret one off this box)");
    }
    let lines: Vec<&str> = t.lines().map(str::trim).filter(|l| !l.is_empty()).collect();
    let line = match lines.as_slice() {
        [] => bail!("empty public key"),
        [first, .., last] if first.starts_with("untrusted comment:") => *last,
        [first, ..] => *first,
    };
    let raw = b64.decode(line).map_err(|_| anyhow!("not a minisign public key (the key line isn't base64)"))?;
    if raw.len() != 42 || &raw[..2] != b"Ed" {
        bail!("not a minisign Ed25519 public key");
    }
    let mut id = raw[2..10].to_vec();
    id.reverse();
    Ok((line.to_string(), hex::encode_upper(id)))
}

pub fn default_public_key_path() -> Option<PathBuf> {
    std::env::var_os("USERPROFILE").map(|h| PathBuf::from(h).join(".tauri").join("fowl-engine.key.pub"))
}

pub fn read_public_key(path: Option<&str>) -> Result<PublicKey> {
    let p = match path.map(str::trim).filter(|s| !s.is_empty()) {
        Some(s) => PathBuf::from(s.trim_matches('"')),
        None => default_public_key_path().ok_or_else(|| anyhow!("no %USERPROFILE% -- type the .pub file's path"))?,
    };
    let meta = std::fs::metadata(&p).map_err(|_| anyhow!("{} not found", display(&p)))?;
    if meta.len() > 16 * 1024 {
        bail!("{} is too big to be a minisign public key", display(&p));
    }
    let text = std::fs::read_to_string(&p).map_err(|e| io_err(e, "reading", &p))?;
    let (key, key_id) = parse_public_key(&text).with_context(|| display(&p))?;
    Ok(PublicKey { key, key_id, path: display(&p) })
}

/// Fowl Engine Manager's own update key: a different key from the engine's,
/// and pasting one for the other is an easy mistake.
fn manager_key_line() -> Option<String> {
    parse_public_key(crate::update::UPDATER_PUBKEY).ok().map(|(k, _)| k)
}

// ---- the setup / security checklist ----------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct CheckAction {
    /// generate_secret | public_key | set
    pub kind: &'static str,
    pub label: String,
    pub rel: String,
    pub path: Vec<String>,
    /// for `set`
    pub value: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Check {
    pub id: &'static str,
    /// ok | warn | info
    pub level: &'static str,
    pub title: String,
    pub message: String,
    pub fix: Option<String>,
    /// masked when the key is a secret
    pub current: Option<String>,
    pub action: Option<CheckAction>,
}

fn at<'a>(v: &'a Value, path: &[&str]) -> Option<&'a Value> {
    let mut cur = v;
    for k in path {
        cur = cur.get(*k)?;
    }
    Some(cur)
}

/// A scalar as text ("" when unset / null).
fn text_at(v: &Value, path: &[&str]) -> String {
    match at(v, path) {
        Some(Value::String(s)) => s.trim().to_string(),
        Some(Value::Number(n)) => n.to_string(),
        Some(Value::Bool(b)) => b.to_string(),
        _ => String::new(),
    }
}

fn split_host_port(addr: &str) -> (String, Option<String>) {
    let a = addr.trim();
    if let Some(rest) = a.strip_prefix('[') {
        if let Some((h, p)) = rest.split_once(']') {
            return (h.to_string(), p.strip_prefix(':').map(String::from));
        }
    }
    match a.rsplit_once(':') {
        Some((h, p)) if !h.contains(':') && !p.is_empty() && p.chars().all(|c| c.is_ascii_digit()) => {
            (h.to_string(), Some(p.to_string()))
        }
        _ => (a.to_string(), None),
    }
}

fn is_loopback(host: &str) -> bool {
    let h = host.trim().to_ascii_lowercase();
    h == "localhost" || h == "::1" || h.parse::<std::net::Ipv4Addr>().map(|ip| ip.is_loopback()).unwrap_or(false)
}

fn parse_file(cfg_dir: &Path, rel: &str) -> std::result::Result<(String, Value), String> {
    let Some(actual) = actual_rel(cfg_dir, rel) else { return Err(format!("{rel} not found")) };
    let text = resolve(cfg_dir, &actual)
        .and_then(|p| read_text(&p, &actual))
        .map(|(_, t)| t)
        .map_err(|e| format!("{e:#}"))?;
    let doc: Value = serde_yaml::from_str(&text).map_err(|e| format!("{actual} is not valid YAML: {e}"))?;
    Ok((actual, doc))
}

fn path_vec(p: &[&str]) -> Vec<String> {
    p.iter().map(|s| s.to_string()).collect()
}

/// The security / setup items the recent hardening left to the operator,
/// read from fowlengine.yaml and webservice.yaml.
pub fn checks(cfg_dir: &Path) -> Vec<Check> {
    let mut out = Vec::new();
    let ck = |id, level, title: &str, message: String| Check {
        id,
        level,
        title: title.into(),
        message,
        fix: None,
        current: None,
        action: None,
    };

    match parse_file(cfg_dir, FOWLENGINE) {
        Err(e) => out.push(Check {
            fix: Some("Copy fowlengine.sample.yaml from the plugin folder to config/plugins/fowlengine.yaml, or fix it in the editor.".into()),
            ..ck("fowlengine_yaml", "warn", "fowlengine.yaml", e)
        }),
        Ok((rel, doc)) => {
            let d = doc.get("DEFAULT").cloned().unwrap_or(Value::Null);

            // autoupdate.public_key
            let pk = text_at(&d, &["autoupdate", "public_key"]);
            let enabled = matches!(at(&d, &["autoupdate", "enabled"]), Some(Value::Bool(true)));
            let pk_path = path_vec(&["DEFAULT", "autoupdate", "public_key"]);
            let action = Some(CheckAction {
                kind: "public_key",
                label: "Load from .pub & set".into(),
                rel: rel.clone(),
                path: pk_path,
                value: None,
            });
            let fix = Some(
                "The RW... line of %USERPROFILE%\\.tauri\\fowl-engine.key.pub (the ENGINE release key, \
                 not the Manager's)."
                    .to_string(),
            );
            let item = if pk.is_empty() || pk.to_ascii_uppercase().starts_with("REPLACE_WITH") {
                let (level, why) = if enabled {
                    ("warn", "auto-update is on but has no key to check releases against, so it never stages anything")
                } else {
                    ("info", "not needed until auto-update is turned on, but nothing is ever staged without it")
                };
                Check { fix, action, ..ck("autoupdate_public_key", level, "Engine release key", format!("autoupdate.public_key is not set -- {why}.")) }
            } else {
                match parse_public_key(&pk) {
                    Err(e) => Check {
                        fix,
                        action,
                        current: Some(mask(&pk)),
                        ..ck("autoupdate_public_key", "warn", "Engine release key", format!("autoupdate.public_key is malformed: {e:#}"))
                    },
                    Ok((line, id)) if Some(&line) == manager_key_line().as_ref() => Check {
                        fix,
                        action,
                        current: Some(format!("key id {id}")),
                        ..ck("autoupdate_public_key", "warn", "Engine release key",
                             "autoupdate.public_key is Fowl Engine Manager's update key -- engine releases are signed with a different one.".into())
                    },
                    Ok((_, id)) => Check {
                        current: Some(format!("key id {id}")),
                        ..ck("autoupdate_public_key", "ok", "Engine release key", "autoupdate.public_key is set.".into())
                    },
                }
            };
            out.push(item);

            // ops_api.api_key
            let key = text_at(&d, &["ops_api", "api_key"]);
            let rest = text_at(&d, &["bfdb", "dcsserverbot_api_key"]);
            let ops_on = !matches!(at(&d, &["ops_api", "enabled"]), Some(Value::Bool(false)));
            let action = Some(CheckAction {
                kind: "generate_secret",
                label: "Generate & set".into(),
                rel: rel.clone(),
                path: path_vec(&["DEFAULT", "ops_api", "api_key"]),
                value: None,
            });
            let fix = Some("A long random value of its own (32+ characters). bfdb gets it from the plugin automatically.".to_string());
            let title = "OPS API key";
            let (level, msg) = if key.is_empty() {
                (
                    if ops_on { "warn" } else { "info" },
                    "ops_api.api_key is not set -- the OPS API falls back to the RestAPI key \
                     (bfdb.dcsserverbot_api_key), which more things hold."
                        .to_string(),
                )
            } else if key == rest {
                ("warn", "ops_api.api_key is the same as bfdb.dcsserverbot_api_key -- give it its own value.".into())
            } else if key.chars().count() < 32 {
                ("warn", format!("ops_api.api_key is only {} characters -- use 32 or more.", key.chars().count()))
            } else {
                ("ok", "ops_api.api_key is set, long, and separate from the RestAPI key.".into())
            };
            out.push(Check {
                current: (!key.is_empty()).then(|| mask(&key)),
                fix: (level != "ok").then_some(fix).flatten(),
                action: (level != "ok").then_some(action).flatten(),
                ..ck("ops_api_key", level, title, msg)
            });

            // bfdb's listeners (procman defaults them to loopback when unset)
            for (id, key, default, what) in [
                ("bfdb_listen_address", "listen_address", "127.0.0.1:8880", "bfdb's API"),
                ("bfdb_site_address", "site_address", "127.0.0.1:8766", "bfdb's site"),
            ] {
                let v = text_at(&d, &["bfdb", key]);
                let title = format!("bfdb.{key}");
                if v.is_empty() {
                    out.push(ck(id, "ok", &title, format!("not set -- {what} listens on {default} (loopback).")));
                    continue;
                }
                let (host, port) = split_host_port(&v);
                if is_loopback(&host) {
                    out.push(Check { current: Some(v.clone()), ..ck(id, "ok", &title, format!("{what} listens on loopback only.")) });
                } else {
                    let port = port.unwrap_or_else(|| default.rsplit(':').next().unwrap_or_default().to_string());
                    let to = format!("127.0.0.1:{port}");
                    out.push(Check {
                        current: Some(v.clone()),
                        fix: Some(format!(
                            "Bind it to {to} and publish it through the reverse proxy (Caddy) instead of an open port."
                        )),
                        action: Some(CheckAction {
                            kind: "set",
                            label: "Set to 127.0.0.1".into(),
                            rel: rel.clone(),
                            path: path_vec(&["DEFAULT", "bfdb", key]),
                            value: Some(to.clone()),
                        }),
                        ..ck(id, "warn", &title, format!("{what} listens on {v} -- reachable from other machines."))
                    });
                }
            }

            // who may stage engine binaries from Discord
            let role = text_at(&d, &["binary_upload_role"]);
            if !role.is_empty() && !role.eq_ignore_ascii_case("Admin") {
                out.push(Check {
                    current: Some(role.clone()),
                    fix: Some("Leave binary_upload_role unset (the Admin group) unless you mean it.".into()),
                    ..ck(
                        "binary_upload_role",
                        "info",
                        "Binary uploads",
                        format!("members of the \"{role}\" role group can stage engine binaries that run on this box."),
                    )
                });
            }
        }
    }

    // DCSServerBot's WebService -- the OPS API is plain HTTP on it
    match parse_file(cfg_dir, WEBSERVICE) {
        Err(e) => out.push(Check {
            fix: Some("Configure the WebService (config/services/webservice.yaml, DEFAULT: listen: 127.0.0.1, port: 9876).".into()),
            ..ck("webservice_listen", "info", "WebService", format!("{e} -- the OPS API and this app's Server OPS view need it."))
        }),
        Ok((rel, doc)) => {
            let listen = text_at(&doc, &["DEFAULT", "listen"]);
            let action = Some(CheckAction {
                kind: "set",
                label: "Set to 127.0.0.1".into(),
                rel,
                path: path_vec(&["DEFAULT", "listen"]),
                value: Some("127.0.0.1".into()),
            });
            let fix = Some("listen: 127.0.0.1 -- bfdb and this app call it from this box.".to_string());
            if listen.is_empty() {
                out.push(Check {
                    fix,
                    action,
                    ..ck("webservice_listen", "warn", "WebService listen", "listen is not set -- DCSServerBot then listens on every interface, OPS API included.".into())
                });
            } else if is_loopback(&split_host_port(&listen).0) {
                out.push(Check { current: Some(listen), ..ck("webservice_listen", "ok", "WebService listen", "the WebService listens on loopback only.".into()) });
            } else {
                out.push(Check {
                    current: Some(listen.clone()),
                    fix,
                    action,
                    ..ck("webservice_listen", "warn", "WebService listen", format!("the WebService (and the OPS API on it) listens on {listen} -- reachable from other machines."))
                });
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp(name: &str) -> PathBuf {
        let d = std::env::temp_dir().join(format!("fowl-botcfg-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&d);
        std::fs::create_dir_all(d.join("plugins")).unwrap();
        std::fs::create_dir_all(d.join("services")).unwrap();
        d
    }

    fn p(v: &[&str]) -> Vec<String> {
        path_vec(v)
    }

    fn sha_of(d: &Path, rel: &str) -> String {
        read(d, rel).unwrap().sha256
    }

    #[test]
    fn resolve_refuses_escapes() {
        let d = tmp("resolve");
        std::fs::write(d.join("main.yaml"), "a: 1\n").unwrap();
        std::fs::write(d.join("plugins/x.yml"), "a: 1\n").unwrap();
        std::fs::write(d.join("notes.txt"), "a: 1\n").unwrap();
        std::fs::create_dir_all(d.join(".secret")).unwrap();
        std::fs::write(d.join(".secret/t.yaml"), "a: 1\n").unwrap();
        std::fs::write(d.parent().unwrap().join(format!("fowl-outside-{}.yaml", std::process::id())), "a: 1\n").unwrap();
        assert!(resolve(&d, "main.yaml").is_ok());
        assert!(resolve(&d, "plugins/x.yml").is_ok());
        assert!(resolve(&d, "plugins\\x.yml").is_ok());
        for bad in [
            "../x.yaml",
            "plugins/../../x.yaml",
            &format!("../fowl-outside-{}.yaml", std::process::id()),
            ".secret/t.yaml",
            ".SECRET/t.yaml",
            "notes.txt",
            "/etc/x.yaml",
            "C:/x.yaml",
            "main.yaml:stream.yaml",
            "",
            "plugins/",
            "missing.yaml",
            "plugins",
        ] {
            assert!(resolve(&d, bad).is_err(), "{bad} must be refused");
        }
        let _ = std::fs::remove_file(d.parent().unwrap().join(format!("fowl-outside-{}.yaml", std::process::id())));
        let _ = std::fs::remove_dir_all(&d);
    }

    #[test]
    fn resolve_refuses_links() {
        let d = tmp("links");
        let outside = std::env::temp_dir().join(format!("fowl-botcfg-{}-links-out", std::process::id()));
        std::fs::create_dir_all(&outside).unwrap();
        std::fs::write(outside.join("o.yaml"), "a: 1\n").unwrap();
        #[cfg(unix)]
        let made = std::os::unix::fs::symlink(outside.join("o.yaml"), d.join("l.yaml")).is_ok()
            && std::os::unix::fs::symlink(&outside, d.join("ldir")).is_ok();
        // needs Developer Mode or admin on Windows; skip the check without it
        #[cfg(windows)]
        let made = std::os::windows::fs::symlink_file(outside.join("o.yaml"), d.join("l.yaml")).is_ok()
            && std::os::windows::fs::symlink_dir(&outside, d.join("ldir")).is_ok();
        if made {
            assert!(resolve(&d, "l.yaml").is_err(), "file link");
            assert!(resolve(&d, "ldir/o.yaml").is_err(), "folder link escaping the config dir");
            assert!(list(&d).unwrap().iter().all(|f| !f.rel.contains("o.yaml") && f.rel != "l.yaml"));
        }
        let _ = std::fs::remove_dir_all(&d);
        let _ = std::fs::remove_dir_all(&outside);
    }

    #[test]
    fn list_order_and_exclusions() {
        let d = tmp("list");
        for f in ["main.yaml", "nodes.yaml", "plugins/zz.yaml", "plugins/fowlengine.yaml", "services/webservice.yaml", "services/bot.yaml"] {
            std::fs::write(d.join(f), "a: 1\n").unwrap();
        }
        std::fs::create_dir_all(d.join(".secret")).unwrap();
        std::fs::write(d.join(".secret/tok.yaml"), "a: 1\n").unwrap();
        std::fs::create_dir_all(d.join("backup")).unwrap();
        std::fs::write(d.join("backup/main.yaml"), "a: 1\n").unwrap();
        std::fs::write(d.join("readme.txt"), "x").unwrap();
        let rels: Vec<String> = list(&d).unwrap().into_iter().map(|f| f.rel).collect();
        assert_eq!(rels, vec![
            "plugins/fowlengine.yaml", "services/webservice.yaml", "main.yaml", "nodes.yaml", "plugins/zz.yaml",
            "services/bot.yaml",
        ]);
        let _ = std::fs::remove_dir_all(&d);
    }

    #[test]
    fn validate_reports_location() {
        let v = validate("main.yaml", "a: 1\nb:\n  c: [1, 2\n  d: 3\n");
        assert!(!v.ok);
        assert!(v.errors[0].line.unwrap() >= 3, "{:?}", v.errors);
        assert!(!v.errors[0].message.contains(" at line "));
        let v = validate("main.yaml", "a: 1\na: 2\n");
        assert!(!v.ok, "duplicate keys");
        let v = validate(FOWLENGINE, "foo: 1\n");
        assert!(v.ok && !v.warnings.is_empty());
        let v = validate("x.yaml", "a:\n\t- b\n");
        assert!(!v.ok && v.warnings.iter().any(|w| w.line == Some(2)));
        assert!(validate("x.yaml", "unknown_key: {a: 1}\n").ok);
    }

    #[test]
    fn write_conflict_backup_atomic() {
        let d = tmp("write");
        let rel = FOWLENGINE;
        std::fs::write(d.join(rel), "DEFAULT:\r\n  a: 1\r\n").unwrap();
        let sha = sha_of(&d, rel);

        // stale sha
        let e = write(&d, rel, "DEFAULT:\r\n  a: 2\r\n", "00").unwrap_err().to_string();
        assert!(e.starts_with(CONFLICT), "{e}");
        // invalid YAML
        let e = write(&d, rel, "DEFAULT: [\r\n", &sha).unwrap_err().to_string();
        assert!(e.contains("not valid YAML"), "{e}");
        assert_eq!(std::fs::read_to_string(d.join(rel)).unwrap(), "DEFAULT:\r\n  a: 1\r\n");

        // a good write: exact bytes (CRLF kept), a backup, no temp file left
        let w = write(&d, rel, "DEFAULT:\r\n  a: 2\r\n", &sha).unwrap();
        assert_eq!(std::fs::read(d.join(rel)).unwrap(), b"DEFAULT:\r\n  a: 2\r\n");
        assert_eq!(w.sha256, sha_of(&d, rel));
        let b = w.backup.unwrap();
        assert!(b.contains(".fowl-backups") && b.contains("plugins__fowlengine.yaml."), "{b}");
        assert_eq!(std::fs::read_to_string(&b).unwrap(), "DEFAULT:\r\n  a: 1\r\n");
        let leftovers: Vec<_> = std::fs::read_dir(d.join("plugins"))
            .unwrap()
            .flatten()
            .filter(|e| e.file_name().to_string_lossy().contains("fowl-tmp"))
            .collect();
        assert!(leftovers.is_empty());
        // the old sha is now a conflict
        assert!(write(&d, rel, "DEFAULT:\r\n  a: 3\r\n", &sha).unwrap_err().to_string().starts_with(CONFLICT));

        // rotation: newest 20 kept
        for i in 0..25 {
            let sha = sha_of(&d, rel);
            write(&d, rel, &format!("DEFAULT:\n  a: {}\n", 10 + i), &sha).unwrap();
        }
        let backups: Vec<String> = std::fs::read_dir(d.join(BACKUP_DIR))
            .unwrap()
            .flatten()
            .map(|e| e.file_name().to_string_lossy().to_string())
            .filter(|n| n.starts_with("plugins__fowlengine.yaml."))
            .collect();
        assert_eq!(backups.len(), KEEP_BACKUPS);
        let newest = backups.iter().max().unwrap();
        assert_eq!(std::fs::read_to_string(d.join(BACKUP_DIR).join(newest)).unwrap(), "DEFAULT:\n  a: 33\n");
        // backups never show up in the list
        assert!(list(&d).unwrap().iter().all(|f| !f.rel.contains("fowl-backups")));
        let _ = std::fs::remove_dir_all(&d);
    }

    const SAMPLE: &str = "\
# header
DEFAULT:
  brand_name: \"Vector Strike\"
  welcome_briefing: >-
    Read this: then fly.
    api_key: not a key
  # binary_upload_role: Admin
  bfdb:
    manage: true
    listen_address: \"0.0.0.0:8880\"   # public!
    dcsserverbot_api_key: \"\"                     # secret -- X-API-Key
    cors_origins:
      - \"https://a\"
  ops_api:
    enabled: true
    service_name: DCSServerBot     # the service
    # prefix: \"/stats\"             # override
    # api_key: \"\"                  # its own long random value

  # ---------------------------------------------
  # range_site_url: \"https://range\"
  autoupdate:
    enabled: false
    public_key: \"\"
";

    fn set_ok(text: &str, path: &[&str], value: &str) -> (String, Change) {
        let (t, c) = plan_set(text, &p(path), value).unwrap_or_else(|e| panic!("{path:?}: {e:#}"));
        let doc: Value = serde_yaml::from_str(&t).unwrap();
        let got = path.iter().fold(Some(&doc), |v, k| v.and_then(|v| v.get(*k)));
        assert_eq!(got, Some(&Value::String(value.into())));
        (t, c)
    }

    #[test]
    fn set_existing_value_keeps_comment() {
        let (t, c) = set_ok(SAMPLE, &["DEFAULT", "bfdb", "listen_address"], "127.0.0.1:8880");
        assert!(t.contains("    listen_address: \"127.0.0.1:8880\"   # public!\n"), "{t}");
        assert_eq!(c.before, vec!["    listen_address: \"0.0.0.0:8880\"   # public!"]);
        assert_eq!(t.lines().count(), SAMPLE.lines().count());
        // an empty value with a comment
        let (t, _) = set_ok(SAMPLE, &["DEFAULT", "autoupdate", "public_key"], "RWabc");
        assert!(t.contains("    public_key: \"RWabc\"\n"), "{t}");
        // a plain value; a Windows path
        let (t, _) = set_ok(SAMPLE, &["DEFAULT", "ops_api", "service_name"], "C:\\x\\y");
        assert!(t.contains("    service_name: 'C:\\x\\y'     # the service\n"), "{t}");
        // untouched: the block scalar that contains "api_key:"
        assert!(t.contains("    api_key: not a key\n"));
    }

    #[test]
    fn set_uncomments_a_commented_key() {
        let (t, c) = set_ok(SAMPLE, &["DEFAULT", "ops_api", "api_key"], "S3CRET");
        assert!(t.contains("    api_key: \"S3CRET\"                  # its own long random value\n"), "{t}");
        assert!(!t.contains("# api_key"));
        assert_eq!(c.before.len(), 1);
        assert_eq!(t.lines().count(), SAMPLE.lines().count());
        // a DEFAULT-level commented key, below another section's lines
        let (t, _) = set_ok(SAMPLE, &["DEFAULT", "range_site_url"], "https://r");
        assert!(t.contains("\n  range_site_url: \"https://r\"\n"), "{t}");
        // ...but not for a key of ops_api: that comment sits at DEFAULT's indent
        let (t, _) = set_ok(SAMPLE, &["DEFAULT", "ops_api", "range_site_url"], "https://r");
        assert!(t.contains("  # range_site_url: \"https://range\""), "{t}");
        assert!(t.contains("  ops_api:\n    range_site_url: \"https://r\"\n"), "{t}");
        // two commented candidates -> insert a fresh line instead
        let two = "A:\n  # k: 1\n  x: 1\n  # k: 2\n";
        let (t, _) = set_ok(two, &["A", "k"], "v");
        assert_eq!(t, "A:\n  k: \"v\"\n  # k: 1\n  x: 1\n  # k: 2\n");
    }

    #[test]
    fn set_inserts_missing_key_and_parents() {
        let (t, c) = set_ok(SAMPLE, &["DEFAULT", "ops_api", "prefixx"], "/p");
        assert!(t.contains("  ops_api:\n    prefixx: \"/p\"\n    enabled: true\n"), "{t}");
        assert_eq!(c.before.len(), 0);
        let (t, c) = set_ok(SAMPLE, &["DEFAULT", "issues", "github", "token"], "T");
        assert!(t.contains("DEFAULT:\n  issues:\n    github:\n      token: \"T\"\n  brand_name:"), "{t}");
        assert_eq!(c.after.len(), 3);
        assert_eq!(c.line, 3);
        // a missing top-level section goes at the end
        let (t, _) = set_ok("DEFAULT:\n  a: 1\n", &["srv1", "x"], "y");
        assert_eq!(t, "DEFAULT:\n  a: 1\nsrv1:\n  x: \"y\"\n");
        // an empty (null) section
        let (t, _) = set_ok("DEFAULT:\n  ops_api:\n  b: 1\n", &["DEFAULT", "ops_api", "api_key"], "k");
        assert_eq!(t, "DEFAULT:\n  ops_api:\n    api_key: \"k\"\n  b: 1\n");
        // 4-space documents stay 4-space
        let (t, _) = set_ok("DEFAULT:\n    a:\n        b: 1\n", &["DEFAULT", "c", "d"], "e");
        assert_eq!(t, "DEFAULT:\n    c:\n        d: \"e\"\n    a:\n        b: 1\n");
        // no trailing newline stays that way; an empty file
        let (t, _) = set_ok("a: 1", &["b"], "2");
        assert_eq!(t, "a: 1\nb: \"2\"");
        let (t, _) = set_ok("", &["DEFAULT", "listen"], "127.0.0.1");
        assert_eq!(t, "DEFAULT:\n  listen: \"127.0.0.1\"\n");
    }

    #[test]
    fn set_handles_crlf() {
        let crlf = SAMPLE.replace('\n', "\r\n");
        let (t, _) = set_ok(&crlf, &["DEFAULT", "ops_api", "api_key"], "K");
        assert!(!t.replace("\r\n", "").contains('\n'), "a bare LF crept in");
        let (t, _) = set_ok(&t, &["DEFAULT", "x", "y"], "z");
        assert!(t.contains("DEFAULT:\r\n  x:\r\n    y: \"z\"\r\n"));
        assert!(!t.replace("\r\n", "").contains('\n'));
        let (t, _) = set_ok(&t, &["DEFAULT", "bfdb", "listen_address"], "127.0.0.1:1");
        assert!(t.contains("listen_address: \"127.0.0.1:1\"   # public!\r\n"));
    }

    #[test]
    fn set_refuses_what_it_cannot_prove() {
        for (text, path) in [
            ("DEFAULT:\n  ops_api: {enabled: true}\n", vec!["DEFAULT", "ops_api", "api_key"]),
            ("DEFAULT:\n  ops_api: {enabled: true}\n", vec!["DEFAULT", "ops_api"]),
            ("DEFAULT:\n  bfdb:\n    a: 1\n", vec!["DEFAULT", "bfdb"]),
            ("DEFAULT:\n  cors:\n    - a\n", vec!["DEFAULT", "cors"]),
            ("DEFAULT:\n  cors:\n    - a\n", vec!["DEFAULT", "cors", "x"]),
            ("DEFAULT:\n  msg: |\n    hi\n", vec!["DEFAULT", "msg"]),
            ("DEFAULT: 5\n", vec!["DEFAULT", "x"]),
            ("- a\n- b\n", vec!["x"]),
            ("DEFAULT: [\n", vec!["DEFAULT", "x"]),
        ] {
            let e = plan_set(text, &p(&path), "v").unwrap_err().to_string();
            assert!(e.starts_with(MANUAL), "{path:?} on {text:?}: {e}");
        }
    }

    #[test]
    fn set_value_quoting_round_trips() {
        for v in ["plain", "with \"quotes\"", "it's", "C:\\a\\b'c", "# not a comment", "yes", "123", "a: b", "tab\there", ""] {
            set_ok("DEFAULT:\n  k: old\n", &["DEFAULT", "k"], v);
        }
    }

    #[test]
    fn masking() {
        assert_eq!(mask(""), "(empty)");
        assert_eq!(mask("abc"), "•••• (3 chars)");
        assert_eq!(mask("abcdefghijk"), "abcd… (11 chars)");
        assert_eq!(mask_line("    api_key: \"abcdefghijk\"  # c"), "    api_key: abcd… (11 chars)  # c");
        assert_eq!(mask_line("    listen: \"0.0.0.0\""), "    listen: \"0.0.0.0\"");
    }

    #[test]
    fn secrets_and_keys() {
        let s = generate_secret().unwrap();
        assert_eq!(s.len(), 43);
        assert!(s.chars().all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_'));
        assert_ne!(s, generate_secret().unwrap());
        // the Manager's own tauri-wrapped key file parses to its RW line
        let (line, id) = parse_public_key(crate::update::UPDATER_PUBKEY).unwrap();
        assert!(line.starts_with("RW") && line.len() == 56, "{line}");
        assert_eq!(id, "E3B1C9E95405E24F");
        let plain = format!("untrusted comment: minisign public key: {id}\n{line}\n");
        assert_eq!(parse_public_key(&plain).unwrap().0, line);
        assert_eq!(parse_public_key(&line).unwrap().0, line);
        assert!(parse_public_key("RWnope").is_err());
        assert!(parse_public_key("untrusted comment: rsign encrypted secret key\nRWxx\n").is_err());
        let d = tmp("pub");
        std::fs::write(d.join("k.pub"), crate::update::UPDATER_PUBKEY).unwrap();
        let k = read_public_key(Some(d.join("k.pub").to_str().unwrap())).unwrap();
        assert_eq!(k.key, line);
        let _ = std::fs::remove_dir_all(&d);
    }

    fn by_id<'a>(c: &'a [Check], id: &str) -> &'a Check {
        c.iter().find(|c| c.id == id).unwrap_or_else(|| panic!("no {id} in {c:?}"))
    }

    #[test]
    fn checklist() {
        let d = tmp("checks");
        std::fs::write(d.join(FOWLENGINE), SAMPLE).unwrap();
        std::fs::write(d.join(WEBSERVICE), "DEFAULT:\n  listen: 0.0.0.0\n  port: 9876\n").unwrap();
        let c = checks(&d);
        assert_eq!(by_id(&c, "autoupdate_public_key").level, "info");
        let ops = by_id(&c, "ops_api_key");
        assert_eq!(ops.level, "warn");
        assert_eq!(ops.action.as_ref().unwrap().kind, "generate_secret");
        let l = by_id(&c, "bfdb_listen_address");
        assert_eq!(l.level, "warn");
        assert_eq!(l.action.as_ref().unwrap().value.as_deref(), Some("127.0.0.1:8880"));
        assert_eq!(by_id(&c, "bfdb_site_address").level, "ok");
        let ws = by_id(&c, "webservice_listen");
        assert_eq!(ws.level, "warn");
        assert_eq!(ws.action.as_ref().unwrap().path, p(&["DEFAULT", "listen"]));

        // every action applies cleanly to the sample
        let text = std::fs::read_to_string(d.join(FOWLENGINE)).unwrap();
        for a in c.iter().filter_map(|c| c.action.as_ref()).filter(|a| a.rel == FOWLENGINE) {
            plan_set(&text, &a.path, a.value.as_deref().unwrap_or("x")).unwrap();
        }

        // fixed up
        let (key, _) = parse_public_key(crate::update::UPDATER_PUBKEY).unwrap();
        let good = SAMPLE
            .replace("listen_address: \"0.0.0.0:8880\"", "listen_address: \"127.0.0.1:8880\"")
            .replace("# api_key: \"\"", "api_key: \"0123456789abcdef0123456789abcdef-x\"")
            .replace("dcsserverbot_api_key: \"\"", "dcsserverbot_api_key: \"other\"")
            .replace("public_key: \"\"", &format!("public_key: \"{key}\""));
        std::fs::write(d.join(FOWLENGINE), &good).unwrap();
        std::fs::write(d.join(WEBSERVICE), "DEFAULT:\n  listen: 127.0.0.1\n").unwrap();
        let c = checks(&d);
        assert_eq!(by_id(&c, "ops_api_key").level, "ok");
        assert!(by_id(&c, "ops_api_key").current.as_deref().unwrap().starts_with("0123…"));
        assert_eq!(by_id(&c, "bfdb_listen_address").level, "ok");
        assert_eq!(by_id(&c, "webservice_listen").level, "ok");
        // the Manager's key is the wrong key
        assert_eq!(by_id(&c, "autoupdate_public_key").level, "warn");

        // short / shared keys, bad public key, uploads role
        std::fs::write(
            d.join(FOWLENGINE),
            "DEFAULT:\n  binary_upload_role: DCS Admin\n  ops_api:\n    api_key: same\n  bfdb:\n    dcsserverbot_api_key: same\n  autoupdate:\n    enabled: true\n    public_key: RWnope\n",
        )
        .unwrap();
        let c = checks(&d);
        assert!(by_id(&c, "ops_api_key").message.contains("same as"));
        assert_eq!(by_id(&c, "autoupdate_public_key").level, "warn");
        assert_eq!(by_id(&c, "binary_upload_role").level, "info");
        std::fs::write(d.join(FOWLENGINE), "DEFAULT:\n  ops_api:\n    api_key: short\n").unwrap();
        assert!(by_id(&checks(&d), "ops_api_key").message.contains("only 5"));

        // missing files
        std::fs::remove_file(d.join(WEBSERVICE)).unwrap();
        std::fs::remove_file(d.join(FOWLENGINE)).unwrap();
        let c = checks(&d);
        assert_eq!(by_id(&c, "fowlengine_yaml").level, "warn");
        assert_eq!(by_id(&c, "webservice_listen").level, "info");
        let _ = std::fs::remove_dir_all(&d);
    }

    /// The plugin's real sample: every checklist fix applies to it by line.
    #[test]
    fn checklist_fixes_apply_to_the_shipped_sample() {
        let sample = include_str!("../../../DCSServerBot/plugins/fowlengine/fowlengine.sample.yaml");
        let (t, c) = set_ok(sample, &["DEFAULT", "ops_api", "api_key"], "K".repeat(43).as_str());
        assert_eq!(c.before.len(), 1, "the commented-out api_key is reused: {c:?}");
        assert!(c.before[0].trim_start().starts_with("# api_key:"));
        let (t, _) = set_ok(&t, &["DEFAULT", "autoupdate", "public_key"], "RWxyz");
        let (t, _) = set_ok(&t, &["DEFAULT", "bfdb", "listen_address"], "127.0.0.1:8880");
        let (t, _) = set_ok(&t, &["DEFAULT", "bfdb", "site_address"], "127.0.0.1:8766");
        assert_eq!(t.lines().count(), sample.lines().count(), "no line added or lost");
        let crlf = sample.replace('\n', "\r\n");
        set_ok(&crlf, &["DEFAULT", "ops_api", "api_key"], "K");
    }

    #[test]
    fn loopback_parsing() {
        assert_eq!(split_host_port("0.0.0.0:8880"), ("0.0.0.0".into(), Some("8880".into())));
        assert_eq!(split_host_port("[::1]:80"), ("::1".into(), Some("80".into())));
        assert_eq!(split_host_port("127.0.0.1"), ("127.0.0.1".into(), None));
        assert!(is_loopback("127.0.0.1") && is_loopback("localhost") && is_loopback("::1") && is_loopback("127.1.2.3"));
        assert!(!is_loopback("0.0.0.0") && !is_loopback("192.168.1.2") && !is_loopback("::"));
    }
}
