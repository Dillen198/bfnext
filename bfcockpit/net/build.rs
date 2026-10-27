//! Stamps the DLL with the Hooks script version it was built for and the git
//! commit (see build_info.rs), so the plugin and the installer can tell a DLL
//! from another build apart.
use std::{env, fs, path::PathBuf};

include!("build_info.rs");

fn main() {
    let here = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let lua = here.join("..").join("Scripts").join("Hooks").join("bfcockpit.lua");
    println!("cargo:rerun-if-changed={}", lua.display());
    println!("cargo:rerun-if-changed=build_info.rs");
    let script = fs::read_to_string(&lua)
        .ok()
        .and_then(|s| script_version_of(&s))
        .unwrap_or_else(|| "unknown".into());
    let git = git_rev_of(&here);
    println!("cargo:rustc-env=BFCOCKPIT_SCRIPT_VERSION={script}");
    println!("cargo:rustc-env=BFCOCKPIT_NET_GIT={git}");
}
