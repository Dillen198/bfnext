//! Daily war news, generated from the campaign's own event stream.
//!
//! The point of this module is NOT to write sentences. It is to decide what
//! was worth reporting. A feed that says "Blue captured Gori" every day is
//! repetitive however many ways it phrases it; what makes something news is
//! the comparison against what came before — that Gori has changed hands three
//! times this week, that Red has not lost an airbase in six days, that a front
//! which had not moved since Tuesday moved forty kilometres overnight.
//!
//! So the work splits in two:
//!
//! 1. **Angles** (this module's `analyse`) — deterministic analysis over the
//!    stored events plus the previous digests. Each angle is a claim with a
//!    subject and a weight. This is the part with the value in it.
//! 2. **Rendering** (`render`) — turning an angle into prose. A weighted
//!    template bank today; the `NewsItem.angle`/`facts` fields are kept
//!    structured so an LLM can be dropped in later as a rendering change
//!    rather than a rewrite.
//!
//! Repetition is suppressed at the angle level, not the wording level: an
//! angle about a given subject is held down for `ANGLE_COOLDOWN_DAYS` once it
//! has run, so the same story does not lead two days running.
//!
//! The news is deliberately NOT fog-of-war scoped. Both coalitions read the
//! same wire report, because that is how war reporting works — the enemy reads
//! the paper too.

use crate::db::{RoundId, StatsDb};
use anyhow::Result;
use chrono::{DateTime, Duration, NaiveDate, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// An angle about a given subject will not run again for this many days.
const ANGLE_COOLDOWN_DAYS: i64 = 3;
/// How far back the "is this unusual?" comparisons look.
const TREND_WINDOW_DAYS: i64 = 7;
/// Digests older than this are dropped when a round is trimmed.
pub const HISTORY_KEEP_DAYS: i64 = 120;

/// One reported story.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NewsItem {
    /// Stable machine name for the kind of claim — `objective_traded`,
    /// `first_airbase_lost`. Kept so the renderer can be swapped and so the
    /// cooldown can recognise a repeat.
    pub angle: String,
    /// What the claim is about: an objective name, a pilot, a side, a front.
    pub subject: String,
    /// 0-100. Orders the bulletin and decides what makes the dashboard panel.
    pub weight: u8,
    /// The rendered sentence(s).
    pub text: String,
}

/// The numbers behind a day, kept alongside the prose so the page can show
/// both and so a future renderer has the raw material.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DigestFacts {
    pub blue_captures: u32,
    pub red_captures: u32,
    pub objectives_traded: u32,
    pub air_kills: u32,
    pub ground_kills: u32,
    pub blue_held: u32,
    pub red_held: u32,
    pub neutral_held: u32,
    /// Objectives that changed hands today, in order.
    pub changed_hands: Vec<String>,
}

/// One day of the war.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NewsDigest {
    /// `YYYY-MM-DD`, UTC. Also the storage key.
    pub day: String,
    pub generated: DateTime<Utc>,
    pub round: u64,
    pub headline: String,
    pub items: Vec<NewsItem>,
    pub facts: DigestFacts,
    /// False while the day is still running and the digest is being refreshed.
    pub final_: bool,
}

// ── deterministic variation ──────────────────────────────────────────────────
//
// The same digest must render identically every time it is regenerated during
// the day, or the page would reword itself under the reader every few minutes.
// So the template choice is a hash of the thing being described, not a random
// number.

fn seed(parts: &[&str]) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for p in parts {
        for b in p.as_bytes() {
            h ^= *b as u64;
            h = h.wrapping_mul(0x0000_0100_0000_01b3);
        }
        h ^= 0x5bf0_3635;
    }
    h
}

fn pick<'a>(options: &[&'a str], parts: &[&str]) -> &'a str {
    if options.is_empty() {
        return "";
    }
    options[(seed(parts) % options.len() as u64) as usize]
}

fn side_name(side: &str) -> &str {
    match side {
        "Blue" => "Blue",
        "Red" => "Red",
        _ => "Neutral",
    }
}

// ── angles ───────────────────────────────────────────────────────────────────

/// A claim the analysis found, before it has been written up.
struct Angle {
    kind: &'static str,
    subject: String,
    weight: u8,
    /// Substitutions for the template bank.
    vars: HashMap<&'static str, String>,
}

impl Angle {
    fn new(kind: &'static str, subject: impl Into<String>, weight: u8) -> Self {
        Self { kind, subject: subject.into(), weight, vars: HashMap::new() }
    }
    fn var(mut self, k: &'static str, v: impl Into<String>) -> Self {
        self.vars.insert(k, v.into());
        self
    }
}

/// Fill `{name}` placeholders from an angle's vars.
fn fill(template: &str, a: &Angle) -> String {
    let mut out = template.to_string();
    out = out.replace("{subject}", &a.subject);
    for (k, v) in &a.vars {
        out = out.replace(&format!("{{{k}}}"), v);
    }
    out
}

/// The template bank. Several phrasings per angle; which one is used is a hash
/// of the day and subject, so it is stable within a day and varies across days
/// and subjects.
fn render(a: &Angle, day: &str) -> String {
    let t: &[&str] = match a.kind {
        "objective_taken" => &[
            "{side} forces took {subject} today.",
            "{subject} fell to {side}.",
            "{side} is reported in control of {subject} as of this evening.",
            "{subject} changed hands, {side} now holding it.",
        ],
        "objective_taken_by" => &[
            "{side} took {subject}, the ground secured by {pilot}.",
            "{subject} fell to {side} after {pilot} put troops into the zone.",
            "{pilot} is credited with the capture of {subject} for {side}.",
        ],
        "objective_traded" => &[
            "{subject} has now changed hands {count} times — neither side has been able to hold it.",
            "That is {count} times {subject} has turned over. It is becoming the sore point of this campaign.",
            "{subject} went back and forth again today, the {count}th time it has changed owner.",
        ],
        "first_loss" => &[
            "It is the first ground {side} has given up since the campaign opened.",
            "{side} had not lost anything until today.",
            "The loss breaks a clean sheet {side} had held since the start.",
        ],
        "streak" => &[
            "{side} took {count} objectives today — the sharpest single-day advance so far.",
            "{count} objectives fell to {side} in one day.",
            "{side} is moving: {count} captures in a single day.",
        ],
        "front_stalled" => &[
            "Nothing changed hands today. The line has now been static for {days} days.",
            "A {days}th day with the map unchanged. Both sides are dug in.",
            "No ground moved. That is {days} days without a capture.",
        ],
        "front_broken" => &[
            "After {days} days of stalemate the line moved again.",
            "The deadlock broke today, the first ground to change hands in {days} days.",
            "{days} quiet days ended this morning.",
        ],
        "air_war" => &[
            "{count} aircraft were lost across both sides.",
            "The air war cost {count} airframes today.",
            "{count} shootdowns were recorded.",
        ],
        "air_war_lopsided" => &[
            "The air fighting went {side}'s way — {won} kills against {lost} losses.",
            "{side} owned the sky today, {won} to {lost}.",
            "{won} against {lost}: a bad day to be flying for the other side.",
        ],
        "pilot_standout" => &[
            "{pilot} was the busiest of them, credited with {count}.",
            "{count} of them belong to {pilot} alone.",
            "{pilot} accounted for {count} single-handed.",
        ],
        "holdings" => &[
            "The map stands at {blue} to Blue, {red} to Red.",
            "Holdings tonight: Blue {blue}, Red {red}.",
            "{blue} objectives fly Blue colours, {red} Red.",
        ],
        "quiet" => &[
            "A quiet day. No ground changed hands and no significant action was reported.",
            "Little to report. The front was still and both sides appear to be resupplying.",
            "Nothing moved today.",
        ],
        _ => &["{subject}"],
    };
    fill(pick(t, &[day, a.kind, &a.subject]), a)
}

fn headline(items: &[NewsItem], facts: &DigestFacts, day: &str) -> String {
    let Some(top) = items.first() else {
        return pick(
            &["A QUIET DAY ON THE FRONT", "NO MOVEMENT REPORTED", "THE LINE HOLDS"],
            &[day],
        )
        .to_string()
    };
    match top.angle.as_str() {
        "objective_traded" => format!("{} CHANGES HANDS AGAIN", top.subject.to_uppercase()),
        "streak" => {
            let side = if facts.blue_captures > facts.red_captures { "BLUE" } else { "RED" };
            format!("{side} ADVANCES ON A BROAD FRONT")
        }
        "front_stalled" => pick(&["STALEMATE HOLDS", "THE LINE DOES NOT MOVE"], &[day]).to_string(),
        "front_broken" => "THE DEADLOCK BREAKS".to_string(),
        "first_loss" => format!("{} LOSES ITS FIRST GROUND", top.subject.to_uppercase()),
        "air_war_lopsided" => "ONE-SIDED DAY IN THE AIR".to_string(),
        _ if !facts.changed_hands.is_empty() => {
            format!("{} FALLS", facts.changed_hands[0].to_uppercase())
        }
        _ => "THE CAMPAIGN CONTINUES".to_string(),
    }
}

// ── analysis ─────────────────────────────────────────────────────────────────

/// Build (or rebuild) the digest for `day`.
///
/// `history` is the already-generated digests for this round, newest first —
/// used both for the trend comparisons and for the repetition cooldown.
pub fn build(
    db: &StatsDb,
    round: RoundId,
    day: NaiveDate,
    history: &[NewsDigest],
) -> Result<NewsDigest> {
    let day_s = day.format("%Y-%m-%d").to_string();
    let start = day.and_hms_opt(0, 0, 0).unwrap().and_utc();
    let end = start + Duration::days(1);

    // --- the day's raw events -------------------------------------------
    let all_caps = db.recent_captures(round, 4000)?;
    let today: Vec<_> = all_caps.iter().filter(|c| c.time >= start && c.time < end).collect();

    let mut facts = DigestFacts::default();
    let mut by_pilot: HashMap<String, u32> = HashMap::new();
    for c in &today {
        match format!("{:?}", c.side).as_str() {
            "Blue" => facts.blue_captures += 1,
            "Red" => facts.red_captures += 1,
            _ => {}
        }
        facts.changed_hands.push(c.objective_name.clone());
        for u in c.by.iter() {
            if let Some(n) = db.pilot_name(u) {
                *by_pilot.entry(n).or_default() += 1;
            }
        }
    }

    for (_, o) in db.objectives_for_round(round)?.iter() {
        match format!("{:?}", o.owner).as_str() {
            "Blue" => facts.blue_held += 1,
            "Red" => facts.red_held += 1,
            _ => facts.neutral_held += 1,
        }
    }

    let kills = db.recent_kills(round, 4000)?;
    // Air losses BY side, so a one-sided day can be reported as one. `victim`
    // is the side that lost the airframe, so the other side scored it.
    let mut air_lost: HashMap<String, u32> = HashMap::new();
    for k in kills.iter().filter(|k| k.time >= start && k.time < end) {
        // A victim with a shooter that was airborne is close enough to an air
        // kill for a newspaper; the exact taxonomy lives in the kill feed.
        let air = k
            .shots
            .iter()
            .any(|s| s.target_typ.contains("plane") || s.target_typ.contains("heli"));
        if air {
            facts.air_kills += 1;
            *air_lost.entry(format!("{:?}", k.victim.side())).or_default() += 1;
        } else {
            facts.ground_kills += 1;
        }
    }

    // How often has each objective turned over, all round?
    let mut turnovers: HashMap<&str, u32> = HashMap::new();
    for c in &all_caps {
        *turnovers.entry(c.objective_name.as_str()).or_default() += 1;
    }
    facts.objectives_traded =
        today.iter().filter(|c| turnovers.get(c.objective_name.as_str()).copied().unwrap_or(0) > 1).count() as u32;

    // --- angles ----------------------------------------------------------
    let mut angles: Vec<Angle> = Vec::new();

    // Repetition is suppressed here, on (angle, subject), not on wording.
    let mut recent: HashSet<(String, String)> = HashSet::new();
    for d in history.iter().take(ANGLE_COOLDOWN_DAYS as usize) {
        for it in &d.items {
            recent.insert((it.angle.clone(), it.subject.clone()));
        }
    }

    let quiet_days = history
        .iter()
        .take_while(|d| d.facts.changed_hands.is_empty())
        .count() as i64;

    if today.is_empty() {
        if quiet_days > 0 {
            angles.push(
                Angle::new("front_stalled", "the front", 70)
                    .var("days", (quiet_days + 1).to_string()),
            );
        } else {
            angles.push(Angle::new("quiet", "the front", 40));
        }
    } else {
        if quiet_days >= 2 {
            angles.push(
                Angle::new("front_broken", "the front", 85).var("days", quiet_days.to_string()),
            );
        }

        // The biggest single story: an objective that keeps turning over.
        if let Some(c) = today
            .iter()
            .max_by_key(|c| turnovers.get(c.objective_name.as_str()).copied().unwrap_or(0))
        {
            let n = turnovers.get(c.objective_name.as_str()).copied().unwrap_or(1);
            if n >= 3 {
                angles.push(
                    Angle::new("objective_traded", c.objective_name.clone(), 90)
                        .var("count", n.to_string()),
                );
            }
        }

        // Each capture, with the pilot when we know them.
        for c in today.iter().take(6) {
            let side = side_name(&format!("{:?}", c.side)).to_string();
            let pilot = c.by.iter().find_map(|u| db.pilot_name(u));
            let a = match pilot {
                Some(p) => Angle::new("objective_taken_by", c.objective_name.clone(), 60)
                    .var("side", side)
                    .var("pilot", p),
                None => Angle::new("objective_taken", c.objective_name.clone(), 55)
                    .var("side", side),
            };
            angles.push(a);
        }

        let (lead, n) = if facts.blue_captures >= facts.red_captures {
            ("Blue", facts.blue_captures)
        } else {
            ("Red", facts.red_captures)
        };
        let best_before = history
            .iter()
            .take(TREND_WINDOW_DAYS as usize)
            .map(|d| d.facts.blue_captures.max(d.facts.red_captures))
            .max()
            .unwrap_or(0);
        if n >= 3 && n > best_before {
            angles.push(
                Angle::new("streak", lead, 80).var("side", lead).var("count", n.to_string()),
            );
        }

        // First ground a side has given up all round.
        let losers: HashSet<String> = today
            .iter()
            .map(|c| {
                if format!("{:?}", c.side) == "Blue" { "Red".to_string() } else { "Blue".to_string() }
            })
            .collect();
        for l in losers {
            let lost_before = all_caps.iter().any(|c| {
                c.time < start
                    && ((format!("{:?}", c.side) == "Blue" && l == "Red")
                        || (format!("{:?}", c.side) == "Red" && l == "Blue"))
            });
            if !lost_before {
                angles.push(Angle::new("first_loss", l.clone(), 88).var("side", l));
            }
        }
    }

    // The air war, whenever there was one.
    let air = facts.air_kills;
    let blue_lost = air_lost.get("Blue").copied().unwrap_or(0);
    let red_lost = air_lost.get("Red").copied().unwrap_or(0);
    if air >= 4 {
        // A lopsided day is the better story; a plain tally is the fallback.
        let (winner, won, lost) = if blue_lost > red_lost {
            ("Red", blue_lost, red_lost)
        } else {
            ("Blue", red_lost, blue_lost)
        };
        if won >= 3 && won >= lost * 3 {
            angles.push(
                Angle::new("air_war_lopsided", "the air war", 72)
                    .var("side", winner)
                    .var("won", won.to_string())
                    .var("lost", lost.to_string()),
            );
        } else {
            angles.push(Angle::new("air_war", "the air war", 50).var("count", air.to_string()));
        }
    }

    if let Some((p, n)) = by_pilot.iter().max_by_key(|(_, n)| **n) {
        if *n >= 2 {
            angles.push(
                Angle::new("pilot_standout", p.clone(), 45)
                    .var("pilot", p.clone())
                    .var("count", n.to_string()),
            );
        }
    }

    angles.push(
        Angle::new("holdings", "the map", 20)
            .var("blue", facts.blue_held.to_string())
            .var("red", facts.red_held.to_string()),
    );

    // --- select, suppress repeats, render --------------------------------
    angles.sort_by(|a, b| b.weight.cmp(&a.weight));
    let mut items = Vec::new();
    let mut used: HashSet<(String, String)> = HashSet::new();
    for a in &angles {
        let key = (a.kind.to_string(), a.subject.clone());
        if used.contains(&key) {
            continue;
        }
        // A story that led recently is held down, not dropped: it still runs
        // if there is nothing else, which is why this is a weight penalty and
        // a cooldown rather than a filter.
        let repeated = recent.contains(&key);
        if repeated && items.len() >= 3 {
            continue;
        }
        used.insert(key);
        items.push(NewsItem {
            angle: a.kind.to_string(),
            subject: a.subject.clone(),
            weight: if repeated { a.weight.saturating_sub(30) } else { a.weight },
            text: render(a, &day_s),
        });
        if items.len() >= 8 {
            break;
        }
    }
    items.sort_by(|a, b| b.weight.cmp(&a.weight));

    Ok(NewsDigest {
        headline: headline(&items, &facts, &day_s),
        day: day_s,
        generated: Utc::now(),
        round: round.0,
        items,
        facts,
        final_: day < Utc::now().date_naive(),
    })
}
