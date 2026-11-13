fn main() {
    // The release app asks for elevation up front: installing a service,
    // writing %ProgramData% and Program Files all need it. Debug builds skip
    // the manifest so `tauri dev` still starts from a normal terminal.
    let release = std::env::var("PROFILE").map(|p| p == "release").unwrap_or(false);
    let mut windows = tauri_build::WindowsAttributes::new();
    if release {
        windows = windows.app_manifest(include_str!("app.manifest"));
    }
    tauri_build::try_build(tauri_build::Attributes::new().windows_attributes(windows))
        .expect("tauri build script failed");
}
