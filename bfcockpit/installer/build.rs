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
use std::{env, fs, path::{Path, PathBuf}};

/// Every file under `dir`, as (path relative to `root` with '/', absolute path).
fn walk(root: &Path, dir: &Path, out: &mut Vec<(String, PathBuf)>) {
    let mut entries: Vec<_> = fs::read_dir(dir).expect("reading plugin dir").flatten().collect();
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
        .join("..").join("Mods").join("Services").join("BFCockpit");
    println!("cargo:rerun-if-changed={}", root.display());
    let mut files = Vec::new();
    walk(&root, &root, &mut files);
    let mut src = String::from("&[\n");
    for (rel, abs) in &files {
        println!("cargo:rerun-if-changed={}", abs.display());
        src.push_str(&format!("    ({:?}, include_bytes!({:?})),\n", rel, abs.to_string_lossy()));
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
            let target = env::var_os("CARGO_TARGET_DIR").map(PathBuf::from).unwrap_or_else(|| {
                PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("..").join("..").join("target")
            });
            target.join("release").join("bfcockpit_net.dll")
        }
    };
    println!("cargo:rerun-if-env-changed=BFCOCKPIT_NET_DLL");
    println!("cargo:rerun-if-changed={}", dll.display());

    let out = PathBuf::from(env::var("OUT_DIR").unwrap()).join("bfcockpit_net.dll");
    match fs::read(&dll) {
        Ok(bytes) => fs::write(&out, bytes).expect("writing embedded dll"),
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
