/*
Copyright 2024 Eric Stokes.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
for more details.
*/

//! Campaign time: the clock every campaign timer is measured on.
//!
//! The engine's own timers run on DCS *model* time (`timer.getTime()`), but
//! the durations the campaign measured -- repairs, production, cooldowns,
//! capture holds, deadlines -- used to be on the wall clock. When the
//! simulation stopped (paused, loading, hung) the wall clock kept going, and
//! on resume everything that came due meanwhile fired in one slow tick.
//!
//! The campaign clock is anchored to a `DateTime<Utc>` when the mission
//! starts and then moves only with model time:
//!
//! ```text
//! campaign_now = anchor_campaign + (model_now - anchor_model)
//! ```
//!
//! so it stops while DCS is paused and runs at half speed at 0.5x. Across a
//! restart the downtime still counts (it always has: lives, round limits and
//! the like rely on it): the save records the campaign time and the wall
//! clock it was taken at (`SavedClock`), and the next mission starts at
//! `saved.campaign + (wall_now - saved.wall)`. A save without one (older than
//! schema 2) starts at the wall clock, which is exactly what its timestamps
//! were measured on.
//!
//! What stays on the wall clock, on purpose: log and perf stamps, stats
//! records (the dashboard and bfdb compare them with real time), and
//! real-world schedules: life resets, ban expiry, the auto shutdown, the
//! round length, sortie start/end for stats.

use chrono::prelude::*;
use serde_derive::{Deserialize, Serialize};

/// The campaign clock as it stood when a save was taken.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SavedClock {
    /// Campaign time at the save.
    pub campaign: DateTime<Utc>,
    /// The wall clock at the same moment.
    pub wall: DateTime<Utc>,
}

impl SavedClock {
    /// Where the campaign clock starts for a mission loading `saved` at wall
    /// time `wall_now`: the downtime counts, a wall clock that went
    /// backwards (a clock correction) doesn't make time run backwards.
    pub fn resume(saved: Option<SavedClock>, wall_now: DateTime<Utc>) -> DateTime<Utc> {
        match saved {
            None => wall_now,
            Some(s) => s.campaign + (wall_now - s.wall).max(chrono::Duration::zero()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Anchor {
    campaign: DateTime<Utc>,
    model: f64,
}

/// See the module docs. Until `start` it reads the wall clock, so code that
/// runs before the mission is up (and tests) behaves as it always did.
#[derive(Debug, Clone, Copy, Default)]
pub struct CampaignClock {
    anchor: Option<Anchor>,
    now: Option<DateTime<Utc>>,
    last_model: f64,
}

impl CampaignClock {
    /// Campaign time `campaign` is model time `model` (seconds).
    pub fn start(&mut self, campaign: DateTime<Utc>, model: f64) {
        self.anchor = Some(Anchor { campaign, model });
        self.now = Some(campaign);
        self.last_model = model;
    }

    pub fn started(&self) -> bool {
        self.anchor.is_some()
    }

    /// Move the clock to model time `model` and return the campaign time.
    /// Never goes backwards; does nothing before `start`.
    pub fn advance(&mut self, model: f64) -> DateTime<Utc> {
        if let Some(a) = self.anchor
            && model.is_finite()
            && model > self.last_model
        {
            self.last_model = model;
            let ms = ((model - a.model) * 1000.).round() as i64;
            self.now = Some(a.campaign + chrono::Duration::milliseconds(ms));
        }
        self.now()
    }

    /// The campaign time as of the last `advance`.
    pub fn now(&self) -> DateTime<Utc> {
        self.now.unwrap_or_else(Utc::now)
    }

    /// What to write into a save taken at wall time `wall`.
    pub fn saved(&self, wall: DateTime<Utc>) -> SavedClock {
        SavedClock { campaign: self.now(), wall }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Duration;

    fn t0() -> DateTime<Utc> {
        Utc.with_ymd_and_hms(2026, 10, 27, 12, 0, 0).unwrap()
    }

    #[test]
    fn follows_model_time() {
        let mut c = CampaignClock::default();
        c.start(t0(), 100.);
        assert_eq!(c.now(), t0());
        assert_eq!(c.advance(101.5), t0() + Duration::milliseconds(1500));
        assert_eq!(c.now(), t0() + Duration::milliseconds(1500));
    }

    #[test]
    fn a_pause_does_not_count() {
        // model time stands still for ten minutes of wall clock: the
        // campaign clock stands still with it
        let mut c = CampaignClock::default();
        c.start(t0(), 0.);
        c.advance(60.);
        let before = c.now();
        c.advance(60.); // the first tick after the pause
        assert_eq!(c.now(), before);
        c.advance(61.);
        assert_eq!(c.now() - before, Duration::seconds(1));
    }

    #[test]
    fn slow_motion_runs_slow() {
        // at 0.5x, ten wall seconds are five model seconds
        let mut c = CampaignClock::default();
        c.start(t0(), 0.);
        for i in 1..=10 {
            c.advance(i as f64 * 0.5);
        }
        assert_eq!(c.now(), t0() + Duration::seconds(5));
    }

    #[test]
    fn never_goes_backwards() {
        let mut c = CampaignClock::default();
        c.start(t0(), 10.);
        c.advance(20.);
        assert_eq!(c.advance(15.), t0() + Duration::seconds(10));
        assert_eq!(c.advance(f64::NAN), t0() + Duration::seconds(10));
    }

    #[test]
    fn wall_clock_until_started() {
        let c = CampaignClock::default();
        let before = Utc::now();
        assert!(c.now() >= before);
        assert!(!c.started());
    }

    #[test]
    fn restart_counts_the_downtime() {
        // saved 2 h into a session that was paused for 30 min: campaign time
        // is 30 min behind the wall clock at the save
        let wall_at_save = t0() + Duration::hours(2);
        let saved = SavedClock {
            campaign: wall_at_save - Duration::minutes(30),
            wall: wall_at_save,
        };
        // back up after 15 minutes
        let wall_now = wall_at_save + Duration::minutes(15);
        assert_eq!(
            SavedClock::resume(Some(saved), wall_now),
            saved.campaign + Duration::minutes(15)
        );
        // an old save has no clock: its timestamps are wall clock
        assert_eq!(SavedClock::resume(None, wall_now), wall_now);
        // the wall clock was set back: no negative downtime
        assert_eq!(
            SavedClock::resume(Some(saved), wall_at_save - Duration::hours(1)),
            saved.campaign
        );
    }

    #[test]
    fn save_round_trip() {
        let mut c = CampaignClock::default();
        c.start(t0(), 0.);
        c.advance(3600.);
        let wall = t0() + Duration::hours(3);
        let s = c.saved(wall);
        assert_eq!(s.campaign, t0() + Duration::hours(1));
        let json = serde_json::to_string(&s).unwrap();
        assert_eq!(serde_json::from_str::<SavedClock>(&json).unwrap(), s);
    }
}
