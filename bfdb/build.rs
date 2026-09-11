// Embeds the git revision and build time so the running binary can report
// exactly which build it is (see `/api/version`). No external crates: shells
// out to git and reads the wall clock.
use std::process::Command;

fn main() {
    let sha = Command::new("git")
        .args(["-C", env!("CARGO_MANIFEST_DIR"), "rev-parse", "--short=12", "HEAD"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "unknown".to_string());

    let dirty = Command::new("git")
        .args(["-C", env!("CARGO_MANIFEST_DIR"), "status", "--porcelain"])
        .output()
        .ok()
        .map(|o| !o.stdout.is_empty())
        .unwrap_or(false);

    let rev = if dirty { format!("{sha}-dirty") } else { sha };

    let epoch = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    println!("cargo:rustc-env=BFNEXT_BUILD_GIT={rev}");
    println!("cargo:rustc-env=BFNEXT_BUILD_EPOCH={epoch}");
    // Re-run (refreshing the sha + build time) on any source change, on a
    // commit/checkout that moves HEAD, and on staging.
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed=Cargo.toml");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=../.git/HEAD");
    println!("cargo:rerun-if-changed=../.git/index");
}
