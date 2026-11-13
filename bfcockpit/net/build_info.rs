// Shared by bfcockpit/net/build.rs and bfcockpit/installer/build.rs (both
// `include!` it), so the DLL and the installer that embeds it describe a build
// the same way: the script version it was built for and the git commit.
//
// The DLL carries both in one string, `STAMP_PREFIX ... STAMP_END`, which the
// Lua module also hands out as `build` -- so the installer's build script can
// read it back out of the DLL bytes without loading the DLL.

#[allow(dead_code)]
const STAMP_PREFIX: &str = "BFCOCKPIT_NET_BUILD;";
#[allow(dead_code)]
const STAMP_END: &str = ";END";

/// The `local BFCOCKPIT_VERSION = "x.y.z"` line of the Hooks script.
#[allow(dead_code)]
fn script_version_of(lua: &str) -> Option<String> {
    lua.lines().find_map(|l| {
        let rest = l.trim().strip_prefix("local BFCOCKPIT_VERSION")?;
        let rest = rest.trim_start().strip_prefix('=')?;
        rest.trim().trim_start_matches('"').split('"').next().map(str::to_string)
    })
}

/// `git rev-parse --short=12 HEAD`, plus "-dirty" when anything under
/// bfcockpit/ has uncommitted changes. "unknown" outside a git checkout.
/// Also tells cargo to rerun when HEAD moves.
#[allow(dead_code)]
fn git_rev_of(repo: &std::path::Path) -> String {
    use std::process::Command;
    let git = |args: &[&str]| -> Option<String> {
        let out = Command::new("git").args(args).current_dir(repo).output().ok()?;
        out.status.success().then(|| String::from_utf8_lossy(&out.stdout).trim().to_string())
    };
    for p in ["HEAD", "packed-refs"] {
        if let Some(path) = git(&["rev-parse", "--git-path", p]) {
            println!("cargo:rerun-if-changed={}", repo.join(path).display());
        }
    }
    if let Some(r) = git(&["symbolic-ref", "-q", "HEAD"]) {
        if let Some(path) = git(&["rev-parse", "--git-path", &r]) {
            println!("cargo:rerun-if-changed={}", repo.join(path).display());
        }
    }
    let Some(rev) = git(&["rev-parse", "--short=12", "HEAD"]) else {
        return "unknown".into();
    };
    match git(&["status", "--porcelain", "--", ":/bfcockpit"]) {
        Some(s) if !s.is_empty() => format!("{rev}-dirty"),
        _ => rev,
    }
}

/// (version, script, git) out of a DLL's bytes, when it carries a stamp.
#[allow(dead_code)]
fn read_stamp(dll: &[u8]) -> Option<(String, String, String)> {
    let start = dll.windows(STAMP_PREFIX.len()).position(|w| w == STAMP_PREFIX.as_bytes())?;
    let rest = &dll[start + STAMP_PREFIX.len()..];
    let end = rest.windows(STAMP_END.len()).position(|w| w == STAMP_END.as_bytes())?;
    let text = std::str::from_utf8(&rest[..end.min(512)]).ok()?;
    let mut version = None;
    let mut script = None;
    let mut git = None;
    for kv in text.split(';') {
        match kv.split_once('=') {
            Some(("version", v)) => version = Some(v.to_string()),
            Some(("script", v)) => script = Some(v.to_string()),
            Some(("git", v)) => git = Some(v.to_string()),
            _ => {}
        }
    }
    Some((version?, script?, git?))
}
