// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Real-world weather and time for the range (`RangeCfg::live_weather`), the
//! campaign's live weather (the `livewx` crate, shared with bflib).
//!
//! DCS cannot change a running mission's weather or clock, so the mission
//! file is rewritten before it next loads. The campaign does that just before
//! bflib restarts the server itself; the range is restarted from outside (the
//! bot's scheduler, `shutdown: true`, or by hand), so the engine does it at
//! the two moments it does see:
//!
//! * **when DCS starts**, in the hooks state, before the server loads its
//!   mission: the scheduled restart, a crash, a reboot. The mission file and
//!   the settings come from a note written while the last mission ran
//!   (`<Saved Games>\bfrange_live_weather.json`), because the range config is
//!   only read once a mission is running.
//! * **when a mission stops** (`onSimulationStop`): a restart of the mission
//!   inside the running DCS, which never passes DCS start.
//!
//! Both block DCS's main thread while they fetch (at most two requests of
//! 10 s each) and write; a failure is logged and the mission loads as it is.
//! The live picture needs nothing from here: `sky.rs` and the station wind
//! read the loaded mission, so they report the real weather once it is in.

use anyhow::Context;
use bfprotocols::cfg::LiveWeatherConfig;
use log::{info, warn};
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// The mission file DCS loaded, from the hooks state's `onMissionLoadEnd`.
static MISSION: Mutex<Option<PathBuf>> = Mutex::new(None);

const NOTE: &str = "bfrange_live_weather.json";

#[derive(Serialize, Deserialize)]
struct Note {
    miz: PathBuf,
    cfg: LiveWeatherConfig,
}

pub fn set_mission(path: PathBuf) {
    *MISSION.lock() = Some(path);
}

/// Called once the range config is read: remember what to rewrite at the
/// next DCS start, or forget it when the config no longer asks for it.
pub fn note(write_dir: &Path, cfg: Option<&LiveWeatherConfig>) {
    let path = write_dir.join(NOTE);
    let Some(cfg) = cfg else {
        if path.exists() {
            match std::fs::remove_file(&path) {
                Ok(()) => info!("live weather is off: removed {path:?}"),
                Err(e) => warn!("could not remove {path:?}: {e}"),
            }
        }
        return;
    };
    let Some(miz) = MISSION.lock().clone() else {
        warn!(
            "live weather: DCS did not say which mission file is loaded, so it cannot be updated"
        );
        return;
    };
    let res = serde_json::to_string_pretty(&Note { miz: miz.clone(), cfg: cfg.clone() })
        .context("encoding")
        .and_then(|s| {
            let tmp = path.with_extension("tmp");
            std::fs::write(&tmp, s).context("writing")?;
            std::fs::rename(&tmp, &path).context("renaming")
        });
    match res {
        Ok(()) => info!(
            "live weather on: {miz:?} gets the real weather{} before it next loads ({})",
            if cfg.sync_time { " and time" } else { "" },
            cfg.metar_station.as_deref().map_or("open-meteo model", |s| s)
        ),
        Err(e) => warn!("live weather: could not write {path:?}: {e:?}"),
    }
}

/// Rewrite the noted mission file with the real weather now. `when` is for
/// the log.
pub fn apply_noted(write_dir: &Path, when: &str) {
    let path = write_dir.join(NOTE);
    if !path.exists() {
        return;
    }
    let note = match std::fs::read_to_string(&path)
        .context("reading")
        .and_then(|s| serde_json::from_str::<Note>(&s).context("parsing"))
    {
        Ok(n) => n,
        Err(e) => return warn!("live weather: {path:?}: {e:?}"),
    };
    if !note.miz.exists() {
        return warn!(
            "live weather: the mission {:?} is gone, nothing to update",
            note.miz
        );
    }
    info!("live weather ({when}): writing the real weather into {:?}", note.miz);
    match livewx::apply(&note.miz, &note.cfg) {
        Ok(()) => info!(
            "live weather ({when}): {:?} updated, it takes effect when it loads",
            note.miz
        ),
        Err(e) => warn!("live weather ({when}): {:?} left as it was: {e:?}", note.miz),
    }
}
