//! Embeds bfcockpit_net.dll (the plugin's HTTPS helper, crate bfcockpit/net)
//! so the installer stays one self-contained file.
//!
//! Cargo cannot build a cdylib as a dependency of a binary, so the DLL has to
//! exist before this crate builds:
//!
//!     cargo build --release -p bfcockpit-net
//!     cargo build --release -p bfcockpit-installer
//!
//! Looked for at $BFCOCKPIT_NET_DLL, else <target dir>/release/. When it is
//! not there yet -- a clean `cargo build --release` of the whole workspace can
//! reach this crate first -- the build carries on with nothing embedded and a
//! warning, and that installer refuses to install (see lib.rs, `NET_DLL`)
//! rather than ship a plugin that cannot reach the server. Rebuilding after the
//! DLL exists picks it up.
//!
//! The DLL found is checked against this build (`check_dll`): it carries the
//! Hooks script version and git commit it was built from (build_info.rs).
//! One built for another script version, from another commit or before the
//! stamp existed gives a warning -- an error with BFCOCKPIT_REQUIRE_MATCH=1,
//! which a release build should set -- so a stale target/release DLL can't
//! ride along unnoticed.
use std::{
    env, fs,
    path::{Path, PathBuf},
};

include!("../net/build_info.rs");

/// None when `dll` was built from the same script version and commit as this
/// installer, else what differs.
fn dll_mismatch(dll: &[u8], script: &str, git: &str) -> Option<String> {
    let Some((version, dll_script, dll_git)) = read_stamp(dll) else {
        return Some(
            "it carries no build stamp (built before bfcockpit/net/build.rs existed)"
                .into(),
        );
    };
    let mut why = Vec::new();
    if dll_script != script {
        why.push(format!(
            "it was built for script {dll_script}, this installer ships {script}"
        ));
    }
    // "-dirty" is part of the string, so a clean and a dirty build differ
    if dll_git != git || git == "unknown" {
        why.push(format!("it was built from {dll_git}, this installer from {git}"));
    }
    (!why.is_empty()).then(|| format!("bfcockpit_net {version}: {}", why.join("; ")))
}

fn check_dll(path: &Path, bytes: &[u8]) {
    let here = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let lua = here.join("..").join("Scripts").join("Hooks").join("bfcockpit.lua");
    let script = fs::read_to_string(&lua)
        .ok()
        .and_then(|s| script_version_of(&s))
        .unwrap_or_else(|| "unknown".into());
    let git = git_rev_of(&here);
    println!("cargo:rerun-if-env-changed=BFCOCKPIT_REQUIRE_MATCH");
    let Some(why) = dll_mismatch(bytes, &script, &git) else { return };
    let msg = format!(
        "{} does not match this build -- {why}. Rebuild it first: cargo build --release -p bfcockpit-net",
        path.display()
    );
    if env::var("BFCOCKPIT_REQUIRE_MATCH").map(|v| v == "1").unwrap_or(false) {
        panic!("{msg}");
    }
    println!("cargo:warning={msg}");
}

/// Every file under `dir`, as (path relative to `root` with '/', absolute path).
fn walk(root: &Path, dir: &Path, out: &mut Vec<(String, PathBuf)>) {
    let mut entries: Vec<_> =
        fs::read_dir(dir).expect("reading plugin dir").flatten().collect();
    entries.sort_by_key(|e| e.path());
    for e in entries {
        let p = e.path();
        if p.is_dir() {
            walk(root, &p, out);
        } else {
            let rel = p.strip_prefix(root).unwrap().to_string_lossy().replace('\\', "/");
            out.push((rel, p));
        }
    }
}

/// Write plugin_files.rs: an include_bytes! for every file of the plugin
/// folder, so lib.rs installs whatever is there.
fn embed_plugin() {
    let root = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .join("..")
        .join("Mods")
        .join("Services")
        .join("BFCockpit");
    println!("cargo:rerun-if-changed={}", root.display());
    let mut files = Vec::new();
    walk(&root, &root, &mut files);
    let mut src = String::from("&[\n");
    for (rel, abs) in &files {
        println!("cargo:rerun-if-changed={}", abs.display());
        src.push_str(&format!(
            "    ({:?}, include_bytes!({:?})),\n",
            rel,
            abs.to_string_lossy()
        ));
    }
    src.push_str("]\n");
    let out = PathBuf::from(env::var("OUT_DIR").unwrap()).join("plugin_files.rs");
    fs::write(out, src).expect("writing plugin_files.rs");
}

fn main() {
    embed_plugin();
    let dll = match env::var_os("BFCOCKPIT_NET_DLL") {
        Some(p) => PathBuf::from(p),
        None => {
            let target =
                env::var_os("CARGO_TARGET_DIR").map(PathBuf::from).unwrap_or_else(|| {
                    PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
                        .join("..")
                        .join("..")
                        .join("target")
                });
            target.join("release").join("bfcockpit_net.dll")
        }
    };
    println!("cargo:rerun-if-env-changed=BFCOCKPIT_NET_DLL");
    println!("cargo:rerun-if-changed={}", dll.display());

    let out = PathBuf::from(env::var("OUT_DIR").unwrap()).join("bfcockpit_net.dll");
    match fs::read(&dll) {
        Ok(bytes) => {
            check_dll(&dll, &bytes);
            fs::write(&out, bytes).expect("writing embedded dll")
        }
        Err(e) => {
            println!(
                "cargo:warning=bfcockpit_net.dll not found at {} ({e}); this installer will refuse to install. \
                 Build it first: cargo build --release -p bfcockpit-net",
                dll.display()
            );
            fs::write(&out, []).expect("writing empty placeholder");
        }
    }
}
