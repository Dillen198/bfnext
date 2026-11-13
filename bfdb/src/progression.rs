//! Pilot progression: XP and levels, daily and weekly challenges, and tiered
//! achievements.
//!
//! - XP is scored here from the stats bflib already sends (kills, captures,
//!   repairs, supply, troops, deploys, FARPs), plus `Stat::PilotXp` for the
//!   bonuses only the engine knows about (a kill inside a tasked zone).
//! - Farming is capped: past `AI_KILLS_FULL_XP` AI kills in an hour, an AI
//!   kill pays a quarter.
//! - Levels have no cap. Level `L` needs `LEVEL_STEP * L * (L - 1)` XP in total.
//! - Three daily and three weekly challenges, the same for everyone, rotate
//!   through `DAILY` / `WEEKLY` by day and week number. Finishing one pays its
//!   XP once.
//! - Achievements are not stored: their tiers are read off the pilot's
//!   all-time totals (`Pilot::total`), so they are right for the whole
//!   history from the first request.

use anyhow::Result;
use chrono::{DateTime, Utc};
use dcso3::net::Ucid;
use serde::{Deserialize, Serialize};
use yats::Db;
use yats::Tree;

use crate::db::Aggregates;

/// Something a pilot did that is worth XP.
#[derive(Debug, Clone, Copy)]
pub(crate) enum XpEvent {
    PlayerKill,
    AiAirKill,
    AiGroundKill,
    Capture,
    Repair,
    Supply,
    Troops,
    Deploy,
    Farp,
    /// Engine-awarded (`Stat::PilotXp`).
    Bonus(u32),
}

impl XpEvent {
    fn base(self) -> u32 {
        match self {
            XpEvent::PlayerKill => 500,
            XpEvent::AiAirKill => 150,
            XpEvent::AiGroundKill => 60,
            XpEvent::Capture => 1_000,
            XpEvent::Repair => 200,
            XpEvent::Supply => 150,
            XpEvent::Troops => 120,
            XpEvent::Deploy => 100,
            XpEvent::Farp => 300,
            XpEvent::Bonus(n) => n,
        }
    }

    fn counter(self) -> Option<Counter> {
        match self {
            XpEvent::PlayerKill => Some(Counter::PlayerKills),
            XpEvent::AiAirKill => Some(Counter::AirKills),
            XpEvent::AiGroundKill => Some(Counter::GroundKills),
            XpEvent::Capture => Some(Counter::Captures),
            XpEvent::Repair
            | XpEvent::Supply
            | XpEvent::Troops
            | XpEvent::Deploy
            | XpEvent::Farp => Some(Counter::Logistics),
            XpEvent::Bonus(_) => None,
        }
    }
}

/// AI kills an hour that pay full XP.
const AI_KILLS_FULL_XP: u32 = 20;
/// XP for level `L` is `LEVEL_STEP * L * (L - 1)`.
const LEVEL_STEP: u64 = 250;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Counter {
    AirKills,
    GroundKills,
    PlayerKills,
    Captures,
    Logistics,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
struct Counts {
    air_kills: u32,
    ground_kills: u32,
    player_kills: u32,
    captures: u32,
    logistics: u32,
}

impl Counts {
    fn get(&self, c: Counter) -> u32 {
        match c {
            Counter::AirKills => self.air_kills + self.player_kills,
            Counter::GroundKills => self.ground_kills,
            Counter::PlayerKills => self.player_kills,
            Counter::Captures => self.captures,
            Counter::Logistics => self.logistics,
        }
    }

    fn bump(&mut self, c: Counter) {
        let f = match c {
            Counter::AirKills => &mut self.air_kills,
            Counter::GroundKills => &mut self.ground_kills,
            Counter::PlayerKills => &mut self.player_kills,
            Counter::Captures => &mut self.captures,
            Counter::Logistics => &mut self.logistics,
        };
        *f = f.saturating_add(1);
    }
}

/// Whether an event bumping `ev` moves a challenge counting `ch`: an enemy
/// pilot shot down is an aircraft shot down too.
fn feeds(ch: Counter, ev: Counter) -> bool {
    ch == ev || (ch == Counter::AirKills && ev == Counter::PlayerKills)
}

struct Challenge {
    counter: Counter,
    goal: u32,
    label: &'static str,
    xp: u32,
}

const DAILY: &[Challenge] = &[
    Challenge {
        counter: Counter::AirKills,
        goal: 2,
        label: "Shoot down 2 aircraft",
        xp: 500,
    },
    Challenge {
        counter: Counter::GroundKills,
        goal: 5,
        label: "Destroy 5 ground targets",
        xp: 400,
    },
    Challenge {
        counter: Counter::Captures,
        goal: 1,
        label: "Capture an objective",
        xp: 600,
    },
    Challenge {
        counter: Counter::Logistics,
        goal: 3,
        label: "Fly 3 logistics jobs",
        xp: 500,
    },
    Challenge {
        counter: Counter::PlayerKills,
        goal: 1,
        label: "Down an enemy pilot",
        xp: 600,
    },
];

const WEEKLY: &[Challenge] = &[
    Challenge {
        counter: Counter::AirKills,
        goal: 15,
        label: "Shoot down 15 aircraft",
        xp: 2_500,
    },
    Challenge {
        counter: Counter::GroundKills,
        goal: 40,
        label: "Destroy 40 ground targets",
        xp: 2_000,
    },
    Challenge {
        counter: Counter::Captures,
        goal: 5,
        label: "Capture 5 objectives",
        xp: 3_000,
    },
    Challenge {
        counter: Counter::Logistics,
        goal: 20,
        label: "Fly 20 logistics jobs",
        xp: 2_500,
    },
    Challenge {
        counter: Counter::PlayerKills,
        goal: 8,
        label: "Down 8 enemy pilots",
        xp: 3_000,
    },
];

/// How many of each pool are active at once.
const ACTIVE: usize = 3;

/// The pool indices active in period `n`: a window of `ACTIVE` that slides
/// by one each period.
fn active(pool_len: usize, n: i64) -> impl Iterator<Item = usize> {
    let start = n.rem_euclid(pool_len as i64) as usize;
    (0..ACTIVE.min(pool_len)).map(move |i| (start + i) % pool_len)
}

fn day_of(ts: DateTime<Utc>) -> i64 {
    ts.timestamp().div_euclid(86_400)
}

/// Weeks start on Monday (1970-01-01 was a Thursday).
fn week_of(ts: DateTime<Utc>) -> i64 {
    (day_of(ts) + 3).div_euclid(7)
}

/// One pilot's progression record.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct PilotProgress {
    xp: u64,
    /// Hour (since the epoch) the AI-kill count below is for.
    ai_hour: i64,
    ai_kills_hour: u32,
    day: i64,
    day_counts: Counts,
    /// Bit `i` set: daily pool entry `i` is done today.
    day_done: u32,
    week: i64,
    week_counts: Counts,
    week_done: u32,
}

pub(crate) fn level_of(xp: u64) -> u64 {
    // Largest L with LEVEL_STEP * L * (L - 1) <= xp.
    let mut l = (((1. + (1. + 4. * xp as f64 / LEVEL_STEP as f64).sqrt()) / 2.).floor()
        as u64)
        .max(1);
    while LEVEL_STEP * l * (l - 1) > xp {
        l -= 1;
    }
    while LEVEL_STEP * (l + 1) * l <= xp {
        l += 1;
    }
    l
}

fn xp_for_level(l: u64) -> u64 {
    LEVEL_STEP * l * l.saturating_sub(1)
}

#[derive(Clone)]
pub(crate) struct Progression {
    tree: Tree<Ucid, PilotProgress>,
}

impl Progression {
    pub(crate) fn new(db: &Db) -> Result<Self> {
        Ok(Self { tree: Tree::open(db, "pilot_progress")? })
    }

    /// Score `ev` for `ucid` at `ts`. Returns the XP actually awarded,
    /// challenge rewards included.
    pub(crate) fn award(
        &self,
        ucid: Ucid,
        ev: XpEvent,
        ts: DateTime<Utc>,
    ) -> Result<u64> {
        let mut gained = 0u64;
        self.tree.fetch_and_update(&ucid, |p| {
            let mut p = p.unwrap_or_default();
            let mut xp = ev.base() as u64;
            if matches!(ev, XpEvent::AiAirKill | XpEvent::AiGroundKill) {
                let hour = ts.timestamp().div_euclid(3_600);
                if p.ai_hour != hour {
                    p.ai_hour = hour;
                    p.ai_kills_hour = 0;
                }
                p.ai_kills_hour = p.ai_kills_hour.saturating_add(1);
                if p.ai_kills_hour > AI_KILLS_FULL_XP {
                    xp /= 4;
                }
            }
            let (day, week) = (day_of(ts), week_of(ts));
            if p.day != day {
                p.day = day;
                p.day_counts = Counts::default();
                p.day_done = 0;
            }
            if p.week != week {
                p.week = week;
                p.week_counts = Counts::default();
                p.week_done = 0;
            }
            if let Some(c) = ev.counter() {
                p.day_counts.bump(c);
                p.week_counts.bump(c);
                for i in active(DAILY.len(), day) {
                    let ch = &DAILY[i];
                    if p.day_done & (1 << i) == 0
                        && feeds(ch.counter, c)
                        && p.day_counts.get(ch.counter) >= ch.goal
                    {
                        p.day_done |= 1 << i;
                        xp += ch.xp as u64;
                    }
                }
                for i in active(WEEKLY.len(), week) {
                    let ch = &WEEKLY[i];
                    if p.week_done & (1 << i) == 0
                        && feeds(ch.counter, c)
                        && p.week_counts.get(ch.counter) >= ch.goal
                    {
                        p.week_done |= 1 << i;
                        xp += ch.xp as u64;
                    }
                }
            }
            p.xp = p.xp.saturating_add(xp);
            gained = xp;
            Some(p)
        })?;
        Ok(gained)
    }

    /// Everything the pilot page shows, as JSON.
    pub(crate) fn report(
        &self,
        ucid: &Ucid,
        total: &Aggregates,
        now: DateTime<Utc>,
    ) -> Result<serde_json::Value> {
        let p = self.tree.get(ucid)?.unwrap_or_default();
        let level = level_of(p.xp);
        let (day, week) = (day_of(now), week_of(now));
        let (dc, dd) = if p.day == day {
            (p.day_counts, p.day_done)
        } else {
            (Counts::default(), 0)
        };
        let (wc, wd) = if p.week == week {
            (p.week_counts, p.week_done)
        } else {
            (Counts::default(), 0)
        };
        let list = |pool: &[Challenge],
                    n: i64,
                    c: &Counts,
                    done: u32|
         -> Vec<serde_json::Value> {
            active(pool.len(), n)
                .map(|i| {
                    let ch = &pool[i];
                    serde_json::json!({
                        "label": ch.label,
                        "progress": c.get(ch.counter).min(ch.goal),
                        "goal": ch.goal,
                        "xp": ch.xp,
                        "done": done & (1 << i) != 0,
                    })
                })
                .collect()
        };
        Ok(serde_json::json!({
            "xp": p.xp,
            "level": level,
            "level_xp": xp_for_level(level),
            "next_level_xp": xp_for_level(level + 1),
            "daily": list(DAILY, day, &dc, dd),
            "weekly": list(WEEKLY, week, &wc, wd),
            "achievements": achievements(total),
        }))
    }

    /// Forget all XP, for a stats rebuild: the replay awards it again.
    pub(crate) fn clear(&self) -> Result<()> {
        self.tree.clear()
    }

    pub(crate) fn xp_of(&self, ucid: &Ucid) -> Result<u64> {
        Ok(self.tree.get(ucid)?.map(|p| p.xp).unwrap_or(0))
    }

    /// The `n` pilots with the most XP.
    pub(crate) fn top(&self, n: usize) -> Result<Vec<(Ucid, u64, u64)>> {
        let mut v = vec![];
        for r in self.tree.iter() {
            let (u, p) = r?;
            v.push((u, p.xp, level_of(p.xp)));
        }
        v.sort_by_key(|a| std::cmp::Reverse(a.1));
        v.truncate(n);
        Ok(v)
    }
}

/// Ten tiers per achievement.
const TIERS: [u32; 10] = [1, 5, 10, 25, 50, 100, 250, 500, 1_000, 2_500];

fn achievements(t: &Aggregates) -> Vec<serde_json::Value> {
    let rows: [(&str, &str, u32); 8] = [
        ("ace", "Fighter Ace (air kills)", t.air_kills),
        ("mud_mover", "Mud Mover (ground kills)", t.ground_kills),
        ("conqueror", "Conqueror (captures)", t.captures),
        (
            "lifeline",
            "Lifeline (repairs and supply runs)",
            t.repairs + t.supply_transfers,
        ),
        ("air_assault", "Air Assault (troop insertions)", t.troops),
        ("builder", "Builder (FARPs and deployments)", t.farps + t.deploys),
        ("veteran", "Veteran (flight hours)", t.hours as u32),
        ("commander", "Commander (actions called)", t.actions),
    ];
    rows.iter()
        .map(|(key, label, value)| {
            let tier = TIERS.iter().filter(|th| *value >= **th).count();
            serde_json::json!({
                "key": key,
                "label": label,
                "value": value,
                "tier": tier,
                "max_tier": TIERS.len(),
                "next": TIERS.get(tier).copied(),
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levels_are_monotonic_and_match_thresholds() {
        assert_eq!(level_of(0), 1);
        assert_eq!(level_of(499), 1);
        assert_eq!(level_of(500), 2);
        assert_eq!(level_of(1_499), 2);
        assert_eq!(level_of(1_500), 3);
        let mut last = 1;
        for xp in (0..1_000_000).step_by(997) {
            let l = level_of(xp);
            assert!(l >= last);
            assert!(xp_for_level(l) <= xp && xp < xp_for_level(l + 1));
            last = l;
        }
    }

    #[test]
    fn active_window_slides_and_wraps() {
        let a: Vec<_> = active(5, 0).collect();
        assert_eq!(a, vec![0, 1, 2]);
        let b: Vec<_> = active(5, 4).collect();
        assert_eq!(b, vec![4, 0, 1]);
        let c: Vec<_> = active(5, -1).collect();
        assert_eq!(c, vec![4, 0, 1]);
    }
}
