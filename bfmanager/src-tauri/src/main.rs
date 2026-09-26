// Prevents an extra console window on Windows in release -- DO NOT REMOVE.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "--service") {
        bfmanager_lib::agent::run_service();
    } else if args.iter().any(|a| a == "--console") {
        bfmanager_lib::agent::run_console();
    } else if args.iter().any(|a| a == "--migrate-service") {
        bfmanager_lib::migrate_service();
    } else {
        bfmanager_lib::run();
    }
}
