// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! Turning a graded event into a `RangeRecord` (for bfdb) and a message (for
//! the pilot in the cockpit).

use crate::{bg, players::Flying, util::MissionClock};
use bfprotocols::range::{PilotRef, RangeRecord, RangeResult, Track, RECORD_VERSION};
use chrono::Utc;
use dcso3::{
    coalition::Side,
    env::miz::GroupId,
    timer::Timer,
    trigger::Trigger,
    MizLua,
};
use log::{info, warn};

#[derive(Debug, Default)]
pub struct Recorder {
    pub sortie: String,
    pub clock: MissionClock,
    seq: u64,
    /// the last few records' headlines per ucid, for "Repeat last result"
    pub last: fxhash::FxHashMap<String, Vec<String>>,
    pub record_tracks: bool,
}

pub fn side_str(s: Side) -> &'static str {
    match s {
        Side::Blue => "blue",
        Side::Red => "red",
        Side::Neutral => "neutral",
    }
}

pub fn pilot_of(f: &Flying) -> PilotRef {
    PilotRef { ucid: Some(f.ucid.to_string()), name: f.name.clone() }
}

impl Recorder {
    pub fn new(sortie: String, clock: MissionClock, record_tracks: bool) -> Self {
        Self { sortie, clock, seq: 0, last: Default::default(), record_tracks }
    }

    fn next_id(&mut self) -> String {
        self.seq += 1;
        format!("{}-{}-{}", self.sortie, Utc::now().timestamp_millis(), self.seq)
    }

    /// Emit one record. Returns its id.
    #[allow(clippy::too_many_arguments)]
    pub fn emit(
        &mut self,
        lua: MizLua,
        pilot: PilotRef,
        unit_type: &str,
        side: Side,
        callsign: &str,
        score: Option<f64>,
        result: RangeResult,
        track: Option<Track>,
    ) -> String {
        let abs = Timer::singleton(lua)
            .and_then(|t| t.get_abs_time())
            .map(|t| t.0 as f64)
            .unwrap_or(0.);
        let (mission_date, mission_time) = self.clock.stamp(abs);
        let id = self.next_id();
        let rec = RangeRecord {
            id: id.clone(),
            v: RECORD_VERSION,
            ts: Utc::now(),
            mission_time,
            mission_date,
            theatre: self.clock.theatre.clone(),
            pilot,
            unit_type: unit_type.to_string(),
            side: side_str(side).into(),
            callsign: callsign.to_string(),
            score,
            result,
            track: if self.record_tracks { track } else { None },
        };
        let headline = rec.headline();
        info!("RESULT {id}: {headline}");
        if let Some(u) = &rec.pilot.ucid {
            let v = self.last.entry(u.clone()).or_default();
            v.push(headline);
            if v.len() > 5 {
                v.remove(0);
            }
        }
        bg::send(bg::Task::Record(Box::new(rec)));
        id
    }
}

pub fn to_group(lua: MizLua, gid: GroupId, text: &str, secs: u32) {
    if let Err(e) = Trigger::singleton(lua)
        .and_then(|t| t.action())
        .and_then(|a| a.out_text_for_group(gid, text.into(), secs as i64, false))
    {
        warn!("could not message group {gid:?}: {e:?}")
    }
}
