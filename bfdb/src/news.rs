//! Daily war news, generated from the campaign's own event stream.
//!
//! The module splits in two, deliberately:
//!
//! 1. **Analysis** (`build`) — deterministic work over the stored events and
//!    the previous digests, producing *facts* and *angles*. An angle is a claim
//!    with a subject, a weight and its supporting numbers: "Gori has now turned
//!    over four times", "Red's air-defence network lost six systems", "the line
//!    has not moved in five days". This is the part that decides what is worth
//!    reporting, and it is the part with the value in it.
//! 2. **Writing** (`news_llm`, with `render` here as the fallback) — turning
//!    those angles into a dispatch. That is a language job, and it is done by a
//!    language model when one is configured; the template bank below is what
//!    runs when it is not, so the feed never goes dark.
//!
//! Analysis has to be deterministic because it is the record — the same day
//! must produce the same facts every time it is rebuilt. Writing does not, and
//! should not: a war diary that reaches for the same four sentences every
//! Tuesday reads like a form letter, which is the whole objection to a static
//! feed.
//!
//! **What this is modelled on.** Daily reporting of the Russia–Ukraine war,
//! because that is the closest thing to what this feed has to do: file a report
//! every single day of a long war, most of which are days when the line of
//! contact does not move. What those bulletins always have, and a captures-only
//! feed does not:
//!
//! * a categorised loss tally *every* day — armour, artillery, air defence,
//!   aircraft — whether or not any ground changed hands;
//! * cumulative totals since the war opened, which is the attrition story and
//!   the reason a static day is still worth reading;
//! * fighting named by axis ("the Pokrovsk direction") rather than as a list of
//!   unrelated place names;
//! * strikes on the rear — logistics, depots, air defence — as their own thread;
//! * and a real opening-day dispatch. Day one of a war is the *biggest* news
//!   day there is, not the thinnest.
//!
//! Repetition is suppressed at the angle level, not the wording level: an angle
//! about a given subject is held down for `ANGLE_COOLDOWN_DAYS` once it has run,
//! so the same story does not lead two days running. A handful of angles
//! (`RECURRING`) are exempt because they are the standing items every real
//! bulletin carries — the loss tally, the running totals, the holdings.
//!
//! The news is deliberately NOT fog-of-war scoped. Both coalitions read the
//! same wire report, because that is how war reporting works — the enemy reads
//! the paper too.

use crate::db::{RoundId, StatsDb};
use anyhow::Result;
use bfprotocols::{cfg::UnitTag, db::objective::ObjectiveKind, shots::Dead};
use chrono::{DateTime, Duration, NaiveDate, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};

/// An angle about a given subject will not run again for this many days.
const ANGLE_COOLDOWN_DAYS: i64 = 3;
/// How far back the "is this unusual?" comparisons look.
const TREND_WINDOW_DAYS: i64 = 7;
/// Digests older than this are dropped when a round is trimmed.
pub const HISTORY_KEEP_DAYS: i64 = 120;
/// Angles that are standing items — they run every day and are exempt from the
/// repetition cooldown, because a bulletin that skipped the casualty figures on
/// the grounds that it ran them yesterday would not be a bulletin.
const RECURRING: &[&str] = &[
    "losses_tally",
    "losses_cumulative",
    "holdings",
    "air_war",
    "front_stalled",
    "static_front",
    "quiet",
    "opening_day",
    "opening_line",
];
/// How many kills to consider. High enough to be "all of them" for any real
/// round; the query walks the whole tree regardless.
const KILL_SCAN: usize = 100_000;

/// One reported story — the structured claim, not the prose.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NewsItem {
    /// Stable machine name for the kind of claim — `objective_traded`,
    /// `sead`, `attrition_spike`. The writer keys off this, and the cooldown
    /// recognises a repeat by it.
    pub angle: String,
    /// What the claim is about: an objective name, a pilot, a side, a front.
    pub subject: String,
    /// 0-100. Orders the bulletin and decides what makes the dashboard panel.
    pub weight: u8,
    /// The supporting numbers, as `{placeholder} -> value`. This is what the
    /// writer is given; it may not introduce anything that is not in here.
    #[serde(default)]
    pub vars: BTreeMap<String, String>,
    /// The fallback sentence, from the template bank. Shown when no dispatch
    /// was written (`NewsDigest::body` empty).
    pub text: String,
}

/// Losses in one category, split by who lost them.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct LossTally {
    pub blue: u32,
    pub red: u32,
}

impl LossTally {
    fn total(&self) -> u32 {
        self.blue + self.red
    }
    fn side(&self, side: &str) -> u32 {
        if side == "Blue" {
            self.blue
        } else {
            self.red
        }
    }
    fn add(&mut self, side: &str) {
        if side == "Blue" {
            self.blue += 1;
        } else if side == "Red" {
            self.red += 1;
        }
    }
}

/// The numbers behind a day, kept alongside the prose so the page can show both
/// and so the writer has raw material it is not allowed to invent around.
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
    /// Which day of the campaign this is, 1-based.
    #[serde(default)]
    pub campaign_day: u32,
    /// Today's losses by category, labelled the way the kill feed labels them
    /// so the bulletin and the feed never disagree about what "ARMOR" means.
    #[serde(default)]
    pub losses: BTreeMap<String, LossTally>,
    /// The same, accumulated from the opening of the campaign to the end of
    /// this day. This is the attrition story, and it is why a day on which
    /// nothing moved still has something to report.
    #[serde(default)]
    pub losses_total: BTreeMap<String, LossTally>,
    /// Airbases held — the currency that actually decides a campaign.
    #[serde(default)]
    pub blue_airbases: u32,
    #[serde(default)]
    pub red_airbases: u32,
    /// Mean logistics health per side, 0-100 — the state of the rear.
    #[serde(default)]
    pub blue_logi: u8,
    #[serde(default)]
    pub red_logi: u8,
}

/// One day of the war.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NewsDigest {
    /// `YYYY-MM-DD`, UTC. Also the storage key.
    pub day: String,
    pub generated: DateTime<Utc>,
    pub round: u64,
    pub headline: String,
    /// The written dispatch, one string per paragraph. Empty when no writer was
    /// configured or the call failed, in which case the client falls back to
    /// the per-item sentences.
    #[serde(default)]
    pub body: Vec<String>,
    /// Who wrote `body` — a model id, or `templates`. Shown in the byline so a
    /// reader can tell the difference.
    #[serde(default)]
    pub written_by: String,
    pub items: Vec<NewsItem>,
    pub facts: DigestFacts,
    /// Fingerprint of the analysis. The dispatch is only rewritten when this
    /// changes, so a day that is rebuilt every ten minutes does not burn a
    /// model call (or reword itself under the reader) every ten minutes.
    #[serde(default)]
    pub facts_hash: u64,
    /// False while the day is still running and the digest is being refreshed.
    pub final_: bool,
}

impl NewsDigest {
    /// What the writer is asked to turn into prose: the angles in order, each
    /// with its numbers. Deliberately not the raw DB — the model gets the
    /// analysis's conclusions, not the chance to draw its own.
    pub fn brief(&self) -> String {
        let mut s = String::new();
        for it in &self.items {
            s.push_str(&format!("- [{}] subject: {}", it.angle, it.subject));
            for (k, v) in &it.vars {
                s.push_str(&format!("; {k}: {v}"));
            }
            s.push('\n');
        }
        s
    }
}

// ── loss classification ──────────────────────────────────────────────────────
//
// The engine's own unit tags beat guessing from a DCS type string: a
// `Strela-1 9P31` is tagged SAM whatever its name reads like, and a tag survives
// a DCS rename. The type-string fallback is only for players (who carry no unit
// record) and for units destroyed before a snapshot existed.

/// Display order for a tally. Aircraft first, the way these bulletins read.
const CAT_ORDER: &[&str] = &[
    "AIRCRAFT",
    "HELO",
    "NAVAL",
    "AIR DEF",
    "RADAR",
    "ARTY",
    "ARMOR",
    "APC",
    "LOGISTICS",
    "INFANTRY",
    "GROUND",
];

fn loss_category(db: &StatsDb, round: RoundId, k: &Dead) -> &'static str {
    if let Some(tags) = db.victim_tags(round, &k.victim) {
        // Order matters: a Shilka is AAA *and* armour, a 2S19 is artillery
        // *and* armour. The more specific claim wins.
        if tags.contains(UnitTag::Aircraft) {
            return "AIRCRAFT";
        }
        if tags.contains(UnitTag::Helicopter) {
            return "HELO";
        }
        if tags.contains(UnitTag::Boat) {
            return "NAVAL";
        }
        if tags.contains(UnitTag::SAM) || tags.contains(UnitTag::AAA) {
            return "AIR DEF";
        }
        if tags.contains(UnitTag::EWR)
            || tags.contains(UnitTag::SearchRadar)
            || tags.contains(UnitTag::TrackRadar)
        {
            return "RADAR";
        }
        if tags.contains(UnitTag::Artillery) {
            return "ARTY";
        }
        if tags.contains(UnitTag::Armor) {
            return "ARMOR";
        }
        if tags.contains(UnitTag::APC) {
            return "APC";
        }
        if tags.contains(UnitTag::Infantry) {
            return "INFANTRY";
        }
        if tags.contains(UnitTag::Logistics) {
            return "LOGISTICS";
        }
        return "GROUND";
    }
    let typ = k
        .shots
        .iter()
        .map(|s| s.target_typ.as_str())
        .find(|t| !t.is_empty())
        .unwrap_or("");
    type_category(typ)
}

/// Fallback classifier, kept deliberately close to the patterns
/// `bfweb/src/pages/KillFeed.tsx` uses so the two views agree.
fn type_category(t: &str) -> &'static str {
    let t = t.to_lowercase();
    let has = |pats: &[&str]| pats.iter().any(|p| t.contains(p));
    if has(&["mi-", "uh-", "ah-", "ch-", "ka-", "sa 342", "gazelle", "huey", "hind", "helicopter"])
    {
        return "HELO";
    }
    if has(&[
        "f-", "f/a-", "mig-", "su-", "a-10", "av8", "av-8", "tornado", "mirage", "m-2000", "c-130",
        "c-17", "kc-", "e-3", "e-2", "b-1", "b-52", "tu-", "an-", "il-", "jf-17", "ajs37", "l-39",
        "c-101", "mb-339", "yak-",
    ]) {
        return "AIRCRAFT";
    }
    if has(&["ship", "boat", "frigate", "destroyer", "cruiser", "carrier", "cvn", "corvette"]) {
        return "NAVAL";
    }
    if has(&[
        "sa-", "s-300", "s-400", "patriot", "roland", "gepard", "tunguska", "shilka", "zsu",
        "zu-23", "vulcan", "strela", "osa ", "kub ", "buk ", "igla", "stinger", "avenger",
        "chaparral", "linebacker",
    ]) {
        return "AIR DEF";
    }
    if has(&["radar", "ewr", "55g6", "1l13", "p-19", "dog ear", "snr", "str "]) {
        return "RADAR";
    }
    if has(&[
        "2s", "m-109", "sph ", "mlrs", "bm-21", "bm-27", "bm-30", "smerch", "uragan", "grad",
        "howitzer", "mortar",
    ]) {
        return "ARTY";
    }
    if has(&[
        "t-55", "t-72", "t-80", "t-90", "m-60", "m1a", "abrams", "leopard", "challenger",
        "merkava", "leclerc",
    ]) {
        return "ARMOR";
    }
    if has(&["bmp", "btr", "bmd", "mtlb", "brad", "marder", "warrior", "lav-", "m113", "aav"]) {
        return "APC";
    }
    if has(&["infantry", "soldier", "paratrooper", "manpad", "sniper"]) {
        return "INFANTRY";
    }
    if has(&[
        "ural", "kamaz", "zil", "gaz", "hemtt", "m939", "m818", "truck", "tigr", "hummer", "hmmwv",
        "transport", "fuel",
    ]) {
        return "LOGISTICS";
    }
    "GROUND"
}

/// How a category reads in a sentence. Chosen to match the vocabulary these
/// bulletins actually use — "artillery systems", not "artillery pieces".
fn loss_noun(cat: &str, n: u32) -> String {
    let one = n == 1;
    let word = match cat {
        "AIRCRAFT" => "aircraft",
        "HELO" => {
            if one {
                "helicopter"
            } else {
                "helicopters"
            }
        }
        "NAVAL" => {
            if one {
                "vessel"
            } else {
                "vessels"
            }
        }
        "AIR DEF" => {
            if one {
                "air-defence system"
            } else {
                "air-defence systems"
            }
        }
        "RADAR" => {
            if one {
                "radar"
            } else {
                "radars"
            }
        }
        "ARTY" => {
            if one {
                "artillery system"
            } else {
                "artillery systems"
            }
        }
        "ARMOR" => {
            if one {
                "tank"
            } else {
                "tanks"
            }
        }
        "APC" => {
            if one {
                "armoured vehicle"
            } else {
                "armoured vehicles"
            }
        }
        "LOGISTICS" => {
            if one {
                "supply vehicle"
            } else {
                "supply vehicles"
            }
        }
        "INFANTRY" => {
            if one {
                "infantry section"
            } else {
                "infantry sections"
            }
        }
        _ => {
            if one {
                "other vehicle"
            } else {
                "other vehicles"
            }
        }
    };
    format!("{n} {word}")
}

/// "3 aircraft, 11 tanks and 4 supply vehicles", or "no confirmed losses".
fn tally_phrase(t: &BTreeMap<String, LossTally>, side: &str) -> String {
    let mut parts: Vec<String> = Vec::new();
    for cat in CAT_ORDER {
        let n = t.get(*cat).map(|v| v.side(side)).unwrap_or(0);
        if n > 0 {
            parts.push(loss_noun(cat, n));
        }
    }
    match parts.len() {
        0 => "no confirmed losses".to_string(),
        1 => parts.remove(0),
        _ => {
            let last = parts.pop().unwrap();
            format!("{} and {}", parts.join(", "), last)
        }
    }
}

// ── geography ────────────────────────────────────────────────────────────────

/// Great-circle distance in km. Good enough to name an axis.
fn dist_km(a: (f64, f64), b: (f64, f64)) -> f64 {
    let (lat1, lon1) = (a.0.to_radians(), a.1.to_radians());
    let (lat2, lon2) = (b.0.to_radians(), b.1.to_radians());
    let dlat = lat2 - lat1;
    let dlon = lon2 - lon1;
    let h = (dlat / 2.0).sin().powi(2) + lat1.cos() * lat2.cos() * (dlon / 2.0).sin().powi(2);
    6371.0 * 2.0 * h.sqrt().asin()
}

// ── deterministic variation ──────────────────────────────────────────────────
//
// The fallback prose must render identically every time a day is rebuilt, or
// the page would reword itself under the reader every ten minutes. So the
// template choice is a hash of the thing being described, not a random number.

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

fn other_side(side: &str) -> &'static str {
    if side == "Blue" {
        "Red"
    } else {
        "Blue"
    }
}

// ── angles ───────────────────────────────────────────────────────────────────

/// A claim the analysis found, before it has been written up.
struct Angle {
    kind: &'static str,
    subject: String,
    weight: u8,
    /// The supporting numbers. These are the only facts the writer may use.
    vars: BTreeMap<String, String>,
}

impl Angle {
    fn new(kind: &'static str, subject: impl Into<String>, weight: u8) -> Self {
        Self { kind, subject: subject.into(), weight, vars: BTreeMap::new() }
    }
    fn var(mut self, k: &str, v: impl Into<String>) -> Self {
        self.vars.insert(k.to_string(), v.into());
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

/// The fallback template bank, used when no writer is configured. Several
/// phrasings per angle; which one is used is a hash of the day and subject, so
/// it is stable within a day and varies across days and subjects.
fn render(a: &Angle, day: &str) -> String {
    let t: &[&str] = match a.kind {
        "opening_day" => &[
            "The campaign opened today. Blue begins the war holding {blue} objectives, {blue_ab} of them airbases; Red holds {red}, with {red_ab} airbases.",
            "First day of the war. The opening disposition gives Blue {blue} objectives and {blue_ab} airbases against Red's {red} and {red_ab}.",
            "Hostilities began this morning. Blue starts with {blue} objectives ({blue_ab} airbases) and Red with {red} ({red_ab}).",
        ],
        "opening_line" => &[
            "The line of contact runs the length of the theatre; its closest point is between {a} and {b}, {km} km apart.",
            "The two sides face each other across the whole map. The narrowest gap is {km} km, between {a} and {b}.",
            "{a} and {b} sit {km} km apart — the tightest point on the opening line, and the likeliest place for it to break.",
        ],
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
        "axis_activity" => &[
            "The day's fighting concentrated on the {subject} direction, where {count} objectives changed hands.",
            "On the {subject} axis, {count} positions turned over — the sector carried the weight of the day.",
            "Pressure is building on the {subject} direction: {count} objectives lost there today.",
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
            "No change on the line of contact. It has now been static for {days} days.",
            "A {days}th day with the map unchanged. Both sides are dug in.",
            "The line did not move. That is {days} days without a capture.",
        ],
        "static_front" => &[
            "The line of contact is unchanged — which did not make it a quiet day. {count} units were destroyed across the front.",
            "No ground changed hands, but the fighting did not stop: {count} units were lost.",
            "Positionally the map is where it was this morning. {count} units did not survive the day.",
        ],
        "front_broken" => &[
            "After {days} days of stalemate the line moved again.",
            "The deadlock broke today, the first ground to change hands in {days} days.",
            "{days} quiet days ended this morning.",
        ],
        "losses_tally" => &[
            "Losses over the past 24 hours — Blue: {blue}. Red: {red}.",
            "Equipment destroyed in the last day. Blue: {blue}. Red: {red}.",
            "The day's bill: Blue lost {blue}; Red lost {red}.",
            "Confirmed destroyed in the last 24 hours — Blue: {blue}; Red: {red}.",
        ],
        "losses_cumulative" => &[
            "Cumulative losses since the campaign opened, day {days} — Blue: {blue}. Red: {red}.",
            "Running totals after {days} days of war. Blue: {blue}. Red: {red}.",
            "Total confirmed losses to date — Blue: {blue}; Red: {red}.",
        ],
        "attrition_spike" => &[
            "It was the heaviest day of the war so far: {count} units destroyed against a daily average of {avg}.",
            "{count} units were lost today, well above the {avg} a day this campaign has been running at.",
            "Attrition spiked — {count} destroyed today, against {avg} on an average day.",
        ],
        "sead" => &[
            "{side}'s air-defence network took the brunt of it, losing {count} systems.",
            "SEAD work told against {side}: {count} air-defence systems destroyed.",
            "{count} {side} air-defence systems were knocked out — the sky over that sector is opening up.",
        ],
        "logistics_struck" => &[
            "Strikes on the rear are telling. {side}'s logistics network is down to {logi}%, from {was}% yesterday.",
            "{side}'s supply infrastructure lost ground today: logistics health fell from {was}% to {logi}%.",
            "The war on {side}'s rear continues — logistics now at {logi}%, down from {was}%.",
        ],
        "pressure" => &[
            "Fighting continues on the approaches to {subject}, where {side} is holding at {health}% strength.",
            "{subject} is under pressure — {side}'s defences there are down to {health}%.",
            "{side} still holds {subject}, but at {health}% it is not expected to hold it long.",
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
        "top_gun" => &[
            "{pilot} was the day's leading shooter with {count} kills.",
            "{count} of the day's kills belong to {pilot}.",
            "{pilot} finished the day with {count} to their name.",
        ],
        "weapon_of_the_day" => &[
            "The {weapon} did more work than anything else in the inventory today, credited with {count} kills.",
            "{count} of today's kills came from one weapon: the {weapon}.",
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
        .to_string();
    };
    match top.angle.as_str() {
        "opening_day" | "opening_line" => {
            pick(&["THE CAMPAIGN OPENS", "DAY ONE", "THE WAR BEGINS"], &[day]).to_string()
        }
        "objective_traded" => format!("{} CHANGES HANDS AGAIN", top.subject.to_uppercase()),
        "axis_activity" => format!("PRESSURE ON THE {} DIRECTION", top.subject.to_uppercase()),
        "streak" => {
            let side = if facts.blue_captures > facts.red_captures { "BLUE" } else { "RED" };
            format!("{side} ADVANCES ON A BROAD FRONT")
        }
        "front_stalled" => pick(&["STALEMATE HOLDS", "THE LINE DOES NOT MOVE"], &[day]).to_string(),
        "static_front" => pick(
            &["NO CHANGE ON THE LINE OF CONTACT", "STATIC FRONT, HEAVY FIGHTING"],
            &[day],
        )
        .to_string(),
        "front_broken" => "THE DEADLOCK BREAKS".to_string(),
        "first_loss" => format!("{} LOSES ITS FIRST GROUND", top.subject.to_uppercase()),
        "attrition_spike" => "THE HEAVIEST DAY YET".to_string(),
        "sead" => format!("{} AIR DEFENCES TAKE THE BRUNT", top.subject.to_uppercase()),
        "logistics_struck" => format!("{} REAR UNDER SUSTAINED ATTACK", top.subject.to_uppercase()),
        "air_war_lopsided" => "ONE-SIDED DAY IN THE AIR".to_string(),
        "losses_tally" => pick(&["COUNTING THE COST", "THE DAY'S BILL"], &[day]).to_string(),
        "pressure" => format!("FIGHTING ON THE APPROACHES TO {}", top.subject.to_uppercase()),
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
///
/// One honest limitation: holdings, logistics health and garrison strength come
/// from the objectives' *current* state, because that is the only state stored.
/// For today's digest that is right. For a day backfilled after the fact it is
/// the state at the moment of backfill, not the state that evening — which is
/// why the generator writes each day once and then freezes it.
pub fn build(
    db: &StatsDb,
    round: RoundId,
    day: NaiveDate,
    history: &[NewsDigest],
) -> Result<NewsDigest> {
    let day_s = day.format("%Y-%m-%d").to_string();
    let start = day.and_hms_opt(0, 0, 0).unwrap().and_utc();
    let end = start + Duration::days(1);

    let campaign_start = db.round_start(round).map(|t| t.date_naive()).unwrap_or(day);
    let campaign_day = ((day - campaign_start).num_days().max(0) + 1) as u32;
    let opening = campaign_day <= 1 && history.is_empty();

    // --- the day's raw events -------------------------------------------
    let all_caps = db.recent_captures(round, 4000)?;
    let today: Vec<_> = all_caps.iter().filter(|c| c.time >= start && c.time < end).collect();

    let mut facts = DigestFacts { campaign_day, ..Default::default() };
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

    // --- the map ---------------------------------------------------------
    // Positions are kept for the axis naming; health and logi for the rear and
    // pressure angles.
    let objs = db.objectives_for_round(round)?;
    let mut pos_of: HashMap<String, (f64, f64)> = HashMap::new();
    let mut majors: Vec<(String, (f64, f64))> = Vec::new();
    let mut logi_sum: HashMap<String, (u32, u32)> = HashMap::new();
    let mut under_pressure: Vec<(String, String, u8)> = Vec::new();
    for (_, o) in objs.iter() {
        let side = format!("{:?}", o.owner);
        match side.as_str() {
            "Blue" => facts.blue_held += 1,
            "Red" => facts.red_held += 1,
            _ => facts.neutral_held += 1,
        }
        let p = (o.pos.latitude, o.pos.longitude);
        pos_of.insert(o.name.to_string(), p);
        match o.kind {
            ObjectiveKind::Airbase => {
                majors.push((o.name.to_string(), p));
                match side.as_str() {
                    "Blue" => facts.blue_airbases += 1,
                    "Red" => facts.red_airbases += 1,
                    _ => {}
                }
            }
            ObjectiveKind::NavalBase | ObjectiveKind::CarrierGroup { .. } => {
                majors.push((o.name.to_string(), p))
            }
            _ => {}
        }
        if side == "Blue" || side == "Red" {
            let e = logi_sum.entry(side.clone()).or_insert((0, 0));
            e.0 += o.logi as u32;
            e.1 += 1;
            // A position at less than half strength is a story in itself: this
            // is where tomorrow's captures come from.
            if o.health < 45 {
                under_pressure.push((o.name.to_string(), side, o.health));
            }
        }
    }
    let mean_logi = |s: &str| -> u8 {
        logi_sum.get(s).filter(|(_, n)| *n > 0).map(|(t, n)| (t / n) as u8).unwrap_or(0)
    };
    facts.blue_logi = mean_logi("Blue");
    facts.red_logi = mean_logi("Red");

    // --- losses -----------------------------------------------------------
    let kills = db.recent_kills(round, KILL_SCAN)?;
    let mut air_lost: HashMap<String, u32> = HashMap::new();
    let mut by_shooter: HashMap<String, u32> = HashMap::new();
    let mut by_weapon: HashMap<String, u32> = HashMap::new();
    for k in kills.iter() {
        if k.time >= end {
            continue; // a day reports only what had happened by its close
        }
        let side = format!("{:?}", k.victim.side());
        let cat = loss_category(db, round, k);
        facts.losses_total.entry(cat.to_string()).or_default().add(&side);
        if k.time < start {
            continue;
        }
        facts.losses.entry(cat.to_string()).or_default().add(&side);
        if cat == "AIRCRAFT" || cat == "HELO" {
            facts.air_kills += 1;
            *air_lost.entry(side).or_default() += 1;
        } else {
            facts.ground_kills += 1;
        }
        // Credit goes to whoever landed a hit; a shared kill credits both, the
        // same way the feed shows it.
        let mut credited: HashSet<String> = HashSet::new();
        for s in k.shots.iter().filter(|s| s.hit) {
            if let Some(name) = s.shooter.ucid().and_then(|u| db.pilot_name(u)) {
                if credited.insert(name.clone()) {
                    *by_shooter.entry(name).or_default() += 1;
                }
            }
            if let Some(w) =
                s.weapon_name.as_ref().filter(|w| !w.is_empty() && w.as_str() != "nil")
            {
                *by_weapon.entry(w.to_string()).or_default() += 1;
            }
        }
    }
    let day_losses: u32 = facts.losses.values().map(|v| v.total()).sum();

    // How often has each objective turned over, all round?
    let mut turnovers: HashMap<&str, u32> = HashMap::new();
    for c in &all_caps {
        *turnovers.entry(c.objective_name.as_str()).or_default() += 1;
    }
    facts.objectives_traded = today
        .iter()
        .filter(|c| turnovers.get(c.objective_name.as_str()).copied().unwrap_or(0) > 1)
        .count() as u32;

    // --- angles ----------------------------------------------------------
    let mut angles: Vec<Angle> = Vec::new();

    // Repetition is suppressed here, on (angle, subject), not on wording.
    let mut recent: HashSet<(String, String)> = HashSet::new();
    for d in history.iter().take(ANGLE_COOLDOWN_DAYS as usize) {
        for it in &d.items {
            recent.insert((it.angle.clone(), it.subject.clone()));
        }
    }

    let quiet_days =
        history.iter().take_while(|d| d.facts.changed_hands.is_empty()).count() as i64;

    // Day one. A war's opening day is its biggest news day: the disposition,
    // the force ratio and where the two sides are actually touching are all
    // reportable, and none of them need a day of history to work out.
    if opening {
        angles.push(
            Angle::new("opening_day", "the campaign", 100)
                .var("blue", facts.blue_held.to_string())
                .var("red", facts.red_held.to_string())
                .var("blue_ab", facts.blue_airbases.to_string())
                .var("red_ab", facts.red_airbases.to_string()),
        );
        // Where the line is tightest — the first place it is likely to break.
        let mut closest: Option<(f64, String, String)> = None;
        for (_, a) in objs.iter().filter(|(_, o)| format!("{:?}", o.owner) == "Blue") {
            for (_, b) in objs.iter().filter(|(_, o)| format!("{:?}", o.owner) == "Red") {
                let d =
                    dist_km((a.pos.latitude, a.pos.longitude), (b.pos.latitude, b.pos.longitude));
                if closest.as_ref().map(|(best, _, _)| d < *best).unwrap_or(true) {
                    closest = Some((d, a.name.to_string(), b.name.to_string()));
                }
            }
        }
        if let Some((d, a, b)) = closest {
            angles.push(
                Angle::new("opening_line", "the front", 95)
                    .var("a", a)
                    .var("b", b)
                    .var("km", format!("{:.0}", d)),
            );
        }
    }

    if today.is_empty() {
        // A static line is not the same thing as a quiet day, and conflating
        // the two is exactly what made the first cut of this feed thin.
        if day_losses >= 5 {
            angles.push(
                Angle::new("static_front", "the front", 72).var("count", day_losses.to_string()),
            );
        } else if quiet_days > 0 {
            angles.push(
                Angle::new("front_stalled", "the front", 70)
                    .var("days", (quiet_days + 1).to_string()),
            );
        } else if !opening {
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

        // Name the fighting after a direction rather than listing villages. The
        // axis is the nearest airbase or naval base to the day's captures,
        // which is how these sectors get their names in practice.
        let mut axis: HashMap<String, u32> = HashMap::new();
        for c in &today {
            let Some(p) = pos_of.get(c.objective_name.as_str()).copied() else { continue };
            let near = majors
                .iter()
                .map(|(n, mp)| (n, dist_km(p, *mp)))
                .filter(|(_, d)| *d < 160.0)
                .min_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
            if let Some((n, _)) = near {
                *axis.entry(n.clone()).or_default() += 1;
            }
        }
        if let Some((name, n)) = axis.iter().max_by_key(|(_, n)| **n) {
            if *n >= 2 {
                angles.push(
                    Angle::new("axis_activity", name.clone(), 66).var("count", n.to_string()),
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
                None => {
                    Angle::new("objective_taken", c.objective_name.clone(), 55).var("side", side)
                }
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
        let losers: HashSet<String> =
            today.iter().map(|c| other_side(&format!("{:?}", c.side)).to_string()).collect();
        for l in losers {
            let lost_before = all_caps
                .iter()
                .any(|c| c.time < start && other_side(&format!("{:?}", c.side)) == l);
            if !lost_before && !opening {
                angles.push(Angle::new("first_loss", l.clone(), 88).var("side", l));
            }
        }
    }

    // --- the standing items ----------------------------------------------
    // These run every day. They are the spine of a real daily bulletin: what
    // was destroyed, what has been destroyed in total, and who holds what.

    if day_losses > 0 {
        angles.push(
            Angle::new("losses_tally", "the day's losses", 65)
                .var("blue", tally_phrase(&facts.losses, "Blue"))
                .var("red", tally_phrase(&facts.losses, "Red")),
        );

        // Was today unusual? Compare against the campaign's own daily average
        // rather than a fixed threshold, so this stays meaningful whether the
        // server is running four players or forty.
        let prior: Vec<u32> = history
            .iter()
            .take(TREND_WINDOW_DAYS as usize)
            .map(|d| d.facts.losses.values().map(|v| v.total()).sum())
            .collect();
        if prior.len() >= 3 {
            let avg = prior.iter().sum::<u32>() as f64 / prior.len() as f64;
            if avg > 0.0 && day_losses as f64 >= avg * 1.8 && day_losses >= 12 {
                angles.push(
                    Angle::new("attrition_spike", "the fighting", 74)
                        .var("count", day_losses.to_string())
                        .var("avg", format!("{:.0}", avg)),
                );
            }
        }

        // The SEAD thread. Reported for whichever side is losing its network,
        // because that is the side whose sky is about to open up.
        if let Some(ad) = facts.losses.get("AIR DEF") {
            let (side, n) = if ad.blue >= ad.red { ("Blue", ad.blue) } else { ("Red", ad.red) };
            if n >= 3 {
                angles.push(
                    Angle::new("sead", side, 68).var("side", side).var("count", n.to_string()),
                );
            }
        }
    }

    // The rear. Compared against yesterday, so this only fires when the
    // logistics picture actually moved.
    if let Some(prev) = history.first() {
        for (side, now, was) in [
            ("Blue", facts.blue_logi, prev.facts.blue_logi),
            ("Red", facts.red_logi, prev.facts.red_logi),
        ] {
            if was > 0 && was.saturating_sub(now) >= 6 {
                angles.push(
                    Angle::new("logistics_struck", side, 62)
                        .var("side", side)
                        .var("logi", now.to_string())
                        .var("was", was.to_string()),
                );
            }
        }
    }

    // Where tomorrow's captures are coming from.
    under_pressure.sort_by_key(|(_, _, h)| *h);
    for (name, side, health) in under_pressure.iter().take(2) {
        angles.push(
            Angle::new("pressure", name.clone(), 52)
                .var("side", side.clone())
                .var("health", health.to_string()),
        );
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

    if let Some((p, n)) = by_shooter.iter().max_by_key(|(_, n)| **n) {
        if *n >= 3 {
            angles.push(
                Angle::new("top_gun", p.clone(), 44)
                    .var("pilot", p.clone())
                    .var("count", n.to_string()),
            );
        }
    }

    if let Some((w, n)) = by_weapon.iter().max_by_key(|(_, n)| **n) {
        if *n >= 5 {
            angles.push(
                Angle::new("weapon_of_the_day", w.clone(), 35)
                    .var("weapon", w.clone())
                    .var("count", n.to_string()),
            );
        }
    }

    angles.push(
        Angle::new("holdings", "the map", 20)
            .var("blue", facts.blue_held.to_string())
            .var("red", facts.red_held.to_string()),
    );

    // The attrition line last, the way these bulletins close on the totals.
    let total_losses: u32 = facts.losses_total.values().map(|v| v.total()).sum();
    if total_losses > 0 {
        angles.push(
            Angle::new("losses_cumulative", "the war", 18)
                .var("days", campaign_day.to_string())
                .var("blue", tally_phrase(&facts.losses_total, "Blue"))
                .var("red", tally_phrase(&facts.losses_total, "Red")),
        );
    }

    // --- select, suppress repeats, render --------------------------------
    angles.sort_by(|a, b| b.weight.cmp(&a.weight));
    let mut items = Vec::new();
    let mut used: HashSet<(String, String)> = HashSet::new();
    for a in &angles {
        let key = (a.kind.to_string(), a.subject.clone());
        if used.contains(&key) {
            continue;
        }
        // A story that led recently is held down, not dropped: it still runs if
        // there is nothing else, which is why this is a weight penalty and a
        // cooldown rather than a filter. The standing items are exempt — "we
        // already gave the casualty figures yesterday" is not a reason to leave
        // them out today.
        let standing = RECURRING.contains(&a.kind);
        let repeated = !standing && recent.contains(&key);
        if repeated && items.len() >= 3 {
            continue;
        }
        used.insert(key);
        items.push(NewsItem {
            angle: a.kind.to_string(),
            subject: a.subject.clone(),
            weight: if repeated { a.weight.saturating_sub(30) } else { a.weight },
            vars: a.vars.clone(),
            text: render(a, &day_s),
        });
        if items.len() >= 10 {
            break;
        }
    }
    items.sort_by(|a, b| b.weight.cmp(&a.weight));

    let facts_hash = {
        // Only the analysis, not the prose or the timestamp: this is what
        // decides whether the dispatch needs rewriting.
        let mut parts: Vec<String> = Vec::new();
        for it in &items {
            parts.push(format!("{}|{}|{}", it.angle, it.subject, it.weight));
            for (k, v) in &it.vars {
                parts.push(format!("{k}={v}"));
            }
        }
        let refs: Vec<&str> = parts.iter().map(|s| s.as_str()).collect();
        seed(&refs)
    };

    Ok(NewsDigest {
        headline: headline(&items, &facts, &day_s),
        day: day_s,
        generated: Utc::now(),
        round: round.0,
        body: Vec::new(),
        written_by: "templates".to_string(),
        items,
        facts,
        facts_hash,
        final_: day < Utc::now().date_naive(),
    })
}
