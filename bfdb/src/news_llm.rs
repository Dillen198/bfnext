//! The writer for the war diary.
//!
//! `news.rs` decides *what happened and what is worth reporting* — that work is
//! deterministic and stays in Rust, because it is the record. This module does
//! the other half: turning that day's angles into a dispatch someone would
//! actually read. That is a language job, so a language model does it.
//!
//! The division matters. The model is never given the database and never
//! decides what is newsworthy; it is handed the analysis's conclusions with
//! their numbers attached and told to write them up, with an explicit rule that
//! it may not introduce a fact that is not in front of it. So the diary is
//! dynamic in its prose and rigid in its facts, which is the right way round.
//!
//! It is also optional. With no endpoint configured the digest keeps the
//! template sentences `news.rs` produced and the feed still works — it just
//! reads like a form letter, which is the thing this module exists to fix.
//!
//! Any OpenAI-compatible chat-completions endpoint works (OpenAI, OpenRouter,
//! Groq, Google AI Studio, or a local Ollama / llama.cpp / vLLM server), as
//! does the Anthropic messages API, which is detected from the URL.
//!
//! Volume: one call per finished day, plus up to one an hour while the current
//! day is still running and its facts keep moving -- so roughly a dozen or two
//! a day at the top end, not one. Still small enough that a local model on the
//! network is a perfectly sensible way to run this.

use crate::news::{Factions, NewsDigest};
use anyhow::{anyhow, bail, Result};
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    sync::Mutex,
    time::{Duration, Instant},
};

/// Minimum gap between two writer calls, across every instance in the process.
/// Each instance's generator ticks every ten minutes and they share one
/// endpoint and one quota (Groq's free tier is 8,000 tokens a minute; a
/// dispatch is ~2,000-3,000 of them), so this works out to one call per tick
/// for the whole process. Backfill walks forward a day per tick.
const MIN_SPACING: Duration = Duration::from_secs(9 * 60);
/// Ceiling on the global backoff after repeated 429s.
const MAX_RATE_BACKOFF: Duration = Duration::from_secs(60 * 60);

struct Gate {
    /// No call before this.
    next_call: Option<Instant>,
    /// An instance that was turned away and gets the next slot, so one
    /// instance with a long backfill cannot starve the other.
    waiting: Option<(String, Instant)>,
    /// Consecutive 429s, for the exponential part of the backoff.
    rate_limited: u32,
}

static GATE: Mutex<Gate> =
    Mutex::new(Gate { next_call: None, waiting: None, rate_limited: 0 });

/// Ask for the process-wide writer slot. `true` means the caller may make one
/// call now; `false` means leave the day for a later tick.
pub fn acquire(who: &str) -> bool {
    let mut g = GATE.lock().unwrap_or_else(|e| e.into_inner());
    let now = Instant::now();
    if g.next_call.map(|t| now < t).unwrap_or(false) {
        if g.waiting.is_none() {
            g.waiting = Some((who.to_string(), now));
        }
        return false;
    }
    if let Some((w, since)) = &g.waiting {
        if w != who && now.duration_since(*since) < Duration::from_secs(15 * 60) {
            return false;
        }
    }
    g.waiting = None;
    g.next_call = Some(now + MIN_SPACING);
    true
}

/// Record a 429: push the next call out by the server's retry-after or an
/// exponential backoff, whichever is longer. Returns the delay applied.
pub(crate) fn note_rate_limited(retry_after: Option<Duration>) -> Duration {
    let mut g = GATE.lock().unwrap_or_else(|e| e.into_inner());
    g.rate_limited = g.rate_limited.saturating_add(1);
    let exp =
        MIN_SPACING.saturating_mul(1u32 << g.rate_limited.min(6)).min(MAX_RATE_BACKOFF);
    let delay = retry_after.unwrap_or_default().max(exp);
    g.next_call = Some(Instant::now() + delay);
    delay
}

fn note_success() {
    GATE.lock().unwrap_or_else(|e| e.into_inner()).rate_limited = 0;
}

/// A second call inside a slot `acquire` granted -- the other voice of a
/// two-voice campaign, written straight after the first so the two tell the
/// same facts. It pushes the next slot out by one more spacing, so the
/// process still averages one call per `MIN_SPACING` on the shared quota.
pub fn charge_extra_call() {
    let mut g = GATE.lock().unwrap_or_else(|e| e.into_inner());
    let now = Instant::now();
    let from = g.next_call.filter(|t| *t > now).unwrap_or(now);
    g.next_call = Some(from + MIN_SPACING);
}

/// The endpoint said 429. Not the day's fault, so the caller should not count
/// it against the day; the process-wide gate has already been pushed back.
#[derive(Debug)]
pub struct RateLimited {
    pub backoff: Duration,
    pub body: String,
}

impl std::fmt::Display for RateLimited {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "rate limited, all news writing paused for {}s: {}",
            self.backoff.as_secs(),
            self.body
        )
    }
}

impl std::error::Error for RateLimited {}

/// An error body squashed onto one line and cut short -- the providers' bodies
/// end in a newline, which left a blank line in the log after every failure.
fn one_line(s: &str, max: usize) -> String {
    let s = s.split_whitespace().collect::<Vec<_>>().join(" ");
    if s.chars().count() > max {
        let cut: String = s.chars().take(max).collect();
        format!("{cut}...")
    } else {
        s
    }
}

/// Turn a failed HTTP reply into an error, feeding 429s to the gate.
fn http_error(res: reqwest::blocking::Response) -> anyhow::Error {
    let status = res.status();
    let header_wait = res
        .headers()
        .get(reqwest::header::RETRY_AFTER)
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.trim().parse::<f64>().ok());
    let body = one_line(&res.text().unwrap_or_default(), 300);
    if status == reqwest::StatusCode::TOO_MANY_REQUESTS {
        // Groq puts it in the message too: "Please try again in 16.25s."
        let body_wait = body.find("try again in ").and_then(|i| {
            let rest = &body[i + "try again in ".len()..];
            let num: String =
                rest.chars().take_while(|c| c.is_ascii_digit() || *c == '.').collect();
            let secs = num.parse::<f64>().ok()?;
            Some(if rest[num.len()..].starts_with("ms") { secs / 1000. } else { secs })
        });
        let wait = header_wait
            .or(body_wait)
            .filter(|s| s.is_finite() && *s >= 0.)
            .map(|s| Duration::from_secs_f64(s.min(3600.)));
        let backoff = note_rate_limited(wait);
        return anyhow::Error::new(RateLimited { backoff, body });
    }
    anyhow!("language model: {status} {body}")
}

/// Where to send the day's brief, and as whom.
#[derive(Debug, Clone)]
pub struct WriterCfg {
    pub url: String,
    pub key: Option<String>,
    pub model: String,
}

/// Default when only a key is given — the cheap, fast tier is the right one
/// for three paragraphs a day.
const DEFAULT_URL: &str = "https://api.openai.com/v1/chat/completions";
const DEFAULT_MODEL: &str = "gpt-4o-mini";

impl WriterCfg {
    /// Resolve from the CLI flags, falling back to the environment. Returns
    /// `None` when nothing is configured, which is not an error — it just means
    /// the template renderer stands.
    ///
    /// A local endpoint (Ollama, llama.cpp) needs no key, so the URL alone is
    /// enough to turn this on.
    pub fn resolve(
        url: Option<String>,
        key: Option<String>,
        model: Option<String>,
    ) -> Option<Self> {
        let env = |k: &str| std::env::var(k).ok().filter(|v| !v.trim().is_empty());
        let url = url.or_else(|| env("BFDB_NEWS_LLM_URL"));
        let key =
            key.or_else(|| env("BFDB_NEWS_LLM_KEY")).or_else(|| env("OPENAI_API_KEY"));
        let model = model.or_else(|| env("BFDB_NEWS_LLM_MODEL"));
        if url.is_none() && key.is_none() {
            return None;
        }
        Some(Self {
            url: url.unwrap_or_else(|| DEFAULT_URL.to_string()),
            key,
            model: model.unwrap_or_else(|| DEFAULT_MODEL.to_string()),
        })
    }

    fn is_anthropic(&self) -> bool {
        self.url.contains("anthropic")
    }
}

/// What the writer is told it is doing. The constraints are the point: a model
/// asked for "war news" with no leash writes casualty figures, civilian detail
/// and invented commanders, none of which this campaign has.
const SYSTEM: &str = "\
You are the duty editor of a wire service filing one short dispatch a day about \
an ongoing war. The war is a persistent multiplayer DCS World campaign fought \
between two belligerents over a fixed set of objectives — airbases, forward \
operating bases, logistics hubs, naval bases and SAM sites. The brief names the \
two sides; refer to them by those names and their ordinary adjectival forms, and \
never as 'Blue' or 'Red'. The pilots named in the brief are real people flying \
in it. The brief also names the theatre and the countries whose territory the \
war is being fought over; name ground by its country the way a wire report does \
— 'inside Syria', 'on the Jordanian border' — using only the countries listed.

Rules, in order of priority:

1. NEVER INVENT A FACT. Every number, place name, pilot name and claim in your \
dispatch must come from the SITUATION block you are given. If it is not there, \
it did not happen and you do not mention it. In particular: no human casualties, \
no civilians, no politics or diplomacy, no weather, no quotes, no named \
commanders or units beyond those given, no equipment types beyond those given. \
This covers the countries too. A country may be named as the ground something \
happened on, or as a listed member of a coalition. It is never an actor: you do \
not know what any one member state did, only what its side did, and 'Turkish \
armour' is an invented fact unless Turkey is the side's own name.
2. You may draw the inference a desk would draw from the facts you have — that a \
position at 18% strength is unlikely to hold, that a coalition losing its \
air-defence network is losing control of its sky — but write it as assessment, \
not as reported fact.
3. VARY. You are shown the previous dispatches. Do not reuse their opening, \
their structure or their phrasing. If the last one led on the loss figures, lead \
on something else. This is the single most common failure; avoid it deliberately.
4. Register: plain, factual, unexcited wire-service prose. British spelling. \
Short sentences. No purple prose, no war-film language, no exclamation marks, no \
rhetorical questions, no addressing the reader, no 'meanwhile', no closing \
moral. Do not editorialise about who deserves to win.
5. Shape: a headline of at most nine words, upper case, no full stop, reflecting \
the single most important item — followed by two to four paragraphs of 40 to 90 \
words. Lead with what changed. Put the standing figures (holdings, running \
totals) last, briefly.

Reply with JSON only, no code fence, in exactly this form:
{\"headline\": \"...\", \"body\": [\"first paragraph\", \"second paragraph\"]}";

/// The reply contract every voice shares, so the parser and the page never
/// care which one wrote the day.
const REPLY_FORM: &str = "\
Reply with JSON only, no code fence, in exactly this form:
{\"headline\": \"...\", \"body\": [\"first paragraph\", \"second paragraph\"]}";

/// Who is writing the day up. A campaign's theme picks its voices
/// ([`voices_for`]); the facts handed over are the same whatever the voice.
///
/// Carried over the worker link in `Op::NewsWrite` with a serde default, so a
/// request from a bfdb that predates voices reads as [`Voice::Default`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Voice {
    /// The diary's own: a present-day wire service's duty editor (`SYSTEM`).
    #[default]
    Default,
    /// A Western wire service in 1985: the Red Storm campaign's dispatch.
    Western,
    /// A Soviet front communiqué in 1985: the Red Storm campaign's second
    /// voice, spinning the same day's facts.
    Soviet,
}

impl Voice {
    /// The id stored with an [`crate::news::AltDispatch`] and sent over the
    /// worker link.
    pub fn id(self) -> &'static str {
        match self {
            Voice::Default => "default",
            Voice::Western => "western",
            Voice::Soviet => "soviet",
        }
    }

    /// Whether prose in the default voice is an acceptable stand-in -- what
    /// an older worker, which ignores the voice it is asked for, sends back.
    /// A Western wire dispatch is close enough to the default one; a Soviet
    /// communiqué is not, and is better left out than filed in the wrong
    /// voice.
    pub fn default_stands_in(self) -> bool {
        !matches!(self, Voice::Soviet)
    }

    /// The system prompt, with the belligerents' names written in where the
    /// voice depends on who is who. The default voice is `SYSTEM`, unchanged.
    pub fn system(self, f: &Factions) -> Cow<'static, str> {
        match self {
            Voice::Default => Cow::Borrowed(SYSTEM),
            Voice::Western => Cow::Owned(western_1985(f)),
            Voice::Soviet => Cow::Owned(soviet_1985(f)),
        }
    }
}

/// A campaign's voices: the main dispatch's, and the second one's when the
/// theme tells each day from both sides. Unthemed is exactly the diary as it
/// always was.
pub fn voices_for(theme: Option<&str>) -> (Voice, Option<Voice>) {
    if crate::instance::is_redstorm(theme) {
        (Voice::Western, Some(Voice::Soviet))
    } else {
        (Voice::Default, None)
    }
}

/// Which side is the Soviet one: whichever is named for it, else Red (the
/// Red Storm campaign's own arrangement).
fn soviet_side(f: &Factions) -> (&'static str, &'static str) {
    let soviet = |side: &str| {
        let s = format!("{} {}", f.name(side), f.adj(side)).to_lowercase();
        ["soviet", "ussr", "warsaw pact"].iter().any(|w| s.contains(w))
    };
    if soviet("Blue") && !soviet("Red") {
        ("Blue", "Red")
    } else {
        ("Red", "Blue")
    }
}

/// The 1985 ground note both period voices need: the brief names countries
/// by today's borders, which in 1985 were Soviet republics.
const GROUND_1985: &str = "\
The brief names the theatre and the countries whose ground is in play by \
present-day borders. In 1985 Georgia, Armenia, Azerbaijan and the Russian \
Caucasus were all inside the Soviet Union: write 'Soviet Georgia', the region or \
simply the place name, and never treat them as independent states. Name ground \
only from the brief's list, and only as places.";

/// The things a 1985 voice must never reach for.
const NO_ANACHRONISM: &str = "\
THE YEAR IS 1985. Nothing that did not exist then: no drones, no GPS or \
satellite navigation, no mobile phones, no internet, email or social media, no \
present-day terms, organisations or events.";

/// The Red Storm campaign's main dispatch: a Western wire service in 1985.
fn western_1985(f: &Factions) -> String {
    let (s, w) = soviet_side(f);
    let (west, west_adj, sov, sov_adj) = (f.name(w), f.adj(w), f.name(s), f.adj(s));
    format!(
        "\
You are the duty editor of a Western wire service in 1985, filing one short \
dispatch a day on the war between {west} and {sov} on NATO's southern flank. The \
war is a persistent multiplayer DCS World campaign fought over a fixed set of \
objectives — airbases, forward operating bases, logistics hubs, naval bases and \
SAM sites. Call the sides {west} and {sov} (adjectives: {west_adj}, {sov_adj}) — \
'{sov_adj} forces', '{west_adj} aircraft' — and never 'Blue' or 'Red'. The pilots \
named in the brief are real people flying in it. {GROUND_1985}

Rules, in order of priority:

1. NEVER INVENT A FACT. Every number, place name, pilot name and claim in your \
dispatch must come from the SITUATION block you are given. If it is not there, \
it did not happen and you do not mention it. In particular: no human casualties, \
no civilians, no politics or diplomacy, no weather, no quotes, no named \
commanders or units beyond those given, no equipment types beyond those given. \
A country is never an actor; only the two sides act.
2. You may draw the inference a desk would draw from the facts you have — that a \
position at 18% strength is unlikely to hold — but write it as assessment, not \
as reported fact.
3. VARY. You are shown the previous dispatches. Do not reuse their opening, \
their structure or their phrasing. If the last one led on the loss figures, lead \
on something else.
4. {NO_ANACHRONISM} Write as a wire reporter of that year would: 'Soviet forces', \
'NATO aircraft', 'air-defence sites', 'sorties', 'the front'.
5. Register: plain, factual, unexcited wire-service prose of the period. British \
spelling. Short sentences. No purple prose, no exclamation marks, no rhetorical \
questions, no addressing the reader, no closing moral. Do not editorialise about \
who deserves to win.
6. Shape: a headline of at most nine words, upper case, no full stop, reflecting \
the single most important item — followed by two to four paragraphs of 40 to 90 \
words. Lead with what changed. Put the standing figures (holdings, running \
totals) last, briefly.

{REPLY_FORM}"
    )
}

/// The Red Storm campaign's second voice: the Soviet front communiqué. It
/// spins; it may not invent.
fn soviet_1985(f: &Factions) -> String {
    let (s, w) = soviet_side(f);
    let (west, west_adj, sov, sov_adj) = (f.name(w), f.adj(w), f.name(s), f.adj(s));
    format!(
        "\
You are the press office of the {sov_adj} high command in 1985, issuing the \
official daily communiqué on the war against {west} on the southern front, \
rendered in English. The war is a persistent multiplayer DCS World campaign \
fought over a fixed set of objectives — airbases, forward operating bases, \
logistics hubs, naval bases and SAM sites. Your side is {sov} ({sov_adj}); the \
enemy is {west} ({west_adj}), which you call 'the aggressor'. Never write 'Blue' \
or 'Red'. The pilots named in the brief are real people flying in it. \
{GROUND_1985}

Rules, in order of priority:

1. NEVER INVENT AN EVENT. Every capture, loss, place, pilot and number you \
mention must come from the SITUATION block. You choose which facts to stress, \
which to pass over and how to describe them; you may not add an event, a place, \
a figure, a name or a claim that is not there. No casualty figures, no \
civilians, no quotes, no named commanders or units beyond those given, no \
equipment types beyond those given. A country is never an actor.
2. SPIN THE SAME FACTS. Lead with {sov_adj} successes and the aggressor's \
losses. Mention {sov_adj} losses briefly, late and in minimising terms — \
'repositioned', 'withdrew to prepared positions', 'losses were sustained' — or \
leave them out; never state the opposite of a fact in the brief. Ground {sov} \
took is 'liberated'; ground it lost is left 'temporarily'. If the day went badly, \
say less, not something false.
3. VARY. You are shown the previous communiqués. Do not reuse their opening, \
their structure or their phrasing.
4. {NO_ANACHRONISM}
5. Register: the official {sov_adj} communiqué of the period — formal, \
collective and triumphant: 'our airmen', 'the heroic defenders', 'the \
aggressor's aircraft'. No exclamation marks, no rhetorical questions, no \
addressing the reader. A pilot named in the brief may be named; credit {sov_adj} \
pilots warmly and never mock or insult any pilot.
6. Shape: a headline of at most nine words, upper case, no full stop, reflecting \
the item most favourable to {sov} — followed by two to four paragraphs of 40 to \
90 words.

{REPLY_FORM}"
    )
}

/// Build the day's brief — the analysis's conclusions, the standing numbers,
/// and what has already been filed.
fn user_prompt(d: &NewsDigest, history: &[NewsDigest], voice: Voice) -> String {
    let f = &d.facts;
    let mut s = String::new();
    s.push_str(&format!("DAY {} OF THE CAMPAIGN — {}\n\n", f.campaign_day, d.day));

    // Named first, where the model cannot miss them. The analysis already
    // substitutes these into every sentence it hands over, but the standing
    // numbers below are per-side and need them spelled out.
    s.push_str(&format!(
        "THE BELLIGERENTS: {} (adjective: {}) against {} (adjective: {}). \
         Use only these names.\n",
        d.factions.blue, d.factions.blue_adj, d.factions.red, d.factions.red_adj
    ));
    // A coalition's member states, where a side is not simply one country.
    // The writer may say who the coalition is; it may not attribute anything
    // to one member, because nothing here knows which member did what.
    for side in ["Blue", "Red"] {
        let m = d.factions.members(side);
        if !m.is_empty() {
            s.push_str(&format!(
                "{} is a coalition of {}. Name the members only when describing who it \
                 is; every action belongs to the coalition, not to one member.\n",
                d.factions.name(side),
                crate::geo::join_names(m)
            ));
        }
    }
    // Where the war is. This is what lets the dispatch name ground the way a
    // real one does, and the closed list is what stops it reaching for a
    // country that is not on the map.
    if !d.facts.theatre.is_empty() && !d.facts.territory.is_empty() {
        s.push_str(&format!(
            "THE THEATRE: {}. The fighting is on the territory of {} — these are the \
             only countries you may name, and only as places.\n",
            d.facts.theatre,
            crate::geo::join_names(&d.facts.territory)
        ));
    }
    s.push('\n');

    s.push_str("SITUATION — the only facts you have, most important first:\n");
    s.push_str(&d.brief());

    s.push_str("\nSTANDING NUMBERS:\n");
    s.push_str(&format!(
        "- holdings: {} {} objectives ({} airbases), {} {} ({} airbases), {} neutral\n",
        d.factions.blue,
        f.blue_held,
        f.blue_airbases,
        d.factions.red,
        f.red_held,
        f.red_airbases,
        f.neutral_held
    ));
    s.push_str(&format!(
        "- mean logistics health: {} {}%, {} {}%\n",
        d.factions.blue, f.blue_logi, d.factions.red, f.red_logi
    ));
    if !f.held_by_country.is_empty() {
        let by = f
            .held_by_country
            .iter()
            .map(|(c, t)| {
                format!(
                    "{c}: {} {}, {} {}",
                    d.factions.blue, t.blue, d.factions.red, t.red
                )
            })
            .collect::<Vec<_>>()
            .join("; ");
        s.push_str(&format!("- objectives held, by the country they stand in — {by}\n"));
    }
    s.push_str(&format!(
        "- objectives that changed hands today: {}\n",
        if f.changed_hands.is_empty() {
            "none".to_string()
        } else {
            f.changed_hands.join(", ")
        }
    ));
    if !f.fighting_in.is_empty() {
        let w = f
            .fighting_in
            .iter()
            .map(|(c, n)| format!("{n} in {c}"))
            .collect::<Vec<_>>()
            .join(", ");
        s.push_str(&format!("- where those captures were: {w}\n"));
    }
    s.push_str(&format!(
        "- aircraft and helicopters destroyed today: {}; ground and naval units: {}\n",
        f.air_kills, f.ground_kills
    ));

    if history.is_empty() {
        s.push_str(
            "\nThis is the first dispatch of the war. There is no previous coverage to \
             refer back to; report the opening disposition and the day's fighting.\n",
        );
    } else if voice == Voice::Soviet {
        // The second voice varies against its own earlier days, not the main
        // dispatch's. Days filed before it existed have none to show.
        let prev: Vec<_> = history
            .iter()
            .filter_map(|p| p.alt.as_ref().map(|a| (&p.day, a)))
            .take(5)
            .collect();
        if !prev.is_empty() {
            s.push_str(
                "\nPREVIOUSLY ISSUED — do not repeat their wording or structure:\n",
            );
            for (day, a) in prev {
                s.push_str(&format!("[{day}] {}\n", a.headline));
                if let Some(first) = a.body.first() {
                    s.push_str(&format!("    {first}\n"));
                }
            }
        }
    } else {
        s.push_str("\nPREVIOUSLY FILED — do not repeat their wording or structure:\n");
        for p in history.iter().take(5) {
            s.push_str(&format!("[{}] {}\n", p.day, p.headline));
            if let Some(first) = p.body.first() {
                s.push_str(&format!("    {}\n", first));
            } else if let Some(first) = p.items.first() {
                s.push_str(&format!("    {}\n", first.text));
            }
        }
    }
    s
}

/// Pull the JSON object out of a reply that may be fenced or padded.
fn extract_json(raw: &str) -> Result<Written> {
    let start = raw.find('{').ok_or_else(|| anyhow!("no JSON object in reply"))?;
    let end = raw.rfind('}').ok_or_else(|| anyhow!("no JSON object in reply"))?;
    if end <= start {
        bail!("malformed JSON in reply");
    }
    // Only the two fields the contract asks for: anything else a model adds
    // is ignored rather than read into `voice`.
    #[derive(Deserialize)]
    struct Reply {
        headline: String,
        body: Vec<String>,
    }
    let r: Reply = serde_json::from_str(&raw[start..=end])?;
    Ok(Written { headline: r.headline, body: r.body, voice: None })
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Written {
    pub headline: String,
    pub body: Vec<String>,
    /// The voice it was written in. Set by `write_dispatch`, and so present
    /// in a worker's reply from this version on; an older worker's reply has
    /// none, which is how bfdb tells that the voice it asked for was ignored.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub voice: Option<Voice>,
}

#[derive(Deserialize)]
struct OaiChoice {
    message: OaiMessage,
    #[serde(default)]
    finish_reason: Option<String>,
}
#[derive(Deserialize)]
struct OaiMessage {
    // Reasoning models may return null content when the budget ran out.
    #[serde(default)]
    content: Option<String>,
}
#[derive(Deserialize)]
struct OaiReply {
    choices: Vec<OaiChoice>,
}

#[derive(Deserialize)]
struct AnthropicBlock {
    #[serde(default)]
    text: String,
}
#[derive(Deserialize)]
struct AnthropicReply {
    content: Vec<AnthropicBlock>,
}

/// One call to the configured model: `system` and `user` in, the reply text
/// and the provider's finish reason out. Blocking, like everything here. A
/// 429 pushes back the process-wide gate (`acquire`), since every caller --
/// the diary and the HQ strategist -- shares the one endpoint and quota.
pub fn chat(
    cfg: &WriterCfg,
    system: &str,
    user: &str,
    max_tokens: u32,
    temperature: f64,
) -> Result<(String, Option<String>)> {
    let client =
        reqwest::blocking::Client::builder().timeout(Duration::from_secs(300)).build()?;

    let raw = if cfg.is_anthropic() {
        let body = serde_json::json!({
            "model": cfg.model,
            "max_tokens": max_tokens,
            "system": system,
            "messages": [{ "role": "user", "content": user }],
        });
        let mut req =
            client.post(&cfg.url).header("anthropic-version", "2023-06-01").json(&body);
        if let Some(k) = &cfg.key {
            req = req.header("x-api-key", k);
        }
        let res = req.send()?;
        if !res.status().is_success() {
            return Err(http_error(res));
        }
        let parsed: AnthropicReply = res.json()?;
        (parsed.content.into_iter().map(|b| b.text).collect::<Vec<_>>().join(""), None)
    } else {
        let mut body = serde_json::json!({
            "model": cfg.model,
            "temperature": temperature,
            // Room for a reasoning model's thinking as well as the dispatch:
            // at 1200, gpt-oss spent the budget reasoning about long briefs
            // and returned no JSON at all (finish_reason "length").
            "max_tokens": max_tokens * 2,
            "messages": [
                { "role": "system", "content": system },
                { "role": "user", "content": user },
            ],
        });
        // gpt-oss (Groq, OpenRouter, Ollama) reasons before it answers; three
        // paragraphs need little of it. Only sent to that family, as other
        // endpoints reject the parameter on non-reasoning models.
        if cfg.model.contains("gpt-oss") {
            body["reasoning_effort"] = serde_json::json!("low");
        }
        let mut req = client.post(&cfg.url).json(&body);
        if let Some(k) = &cfg.key {
            req = req.bearer_auth(k);
        }
        let res = req.send()?;
        if !res.status().is_success() {
            return Err(http_error(res));
        }
        let parsed: OaiReply = res.json()?;
        let choice = parsed
            .choices
            .into_iter()
            .next()
            .ok_or_else(|| anyhow!("no choices in reply"))?;
        (choice.message.content.unwrap_or_default(), choice.finish_reason)
    };
    note_success();
    Ok(raw)
}

/// Write the dispatch in `voice`. Blocking on purpose — the caller already
/// runs inside `block_in_place`, and this happens a handful of times a day.
pub fn write_dispatch(
    cfg: &WriterCfg,
    voice: Voice,
    digest: &NewsDigest,
    history: &[NewsDigest],
) -> Result<Written> {
    let prompt = user_prompt(digest, history, voice);
    let system = voice.system(&digest.factions);
    // Generous on purpose. This runs on a timer in the background and nothing
    // waits on it, while a local model on a CPU-only box can take minutes to
    // produce a few hundred tokens -- a tight timeout here would fail exactly
    // the setup that is most attractive for this job.
    let (raw, finish) = chat(cfg, &system, &prompt, 1200, 0.9)?;

    let mut w = extract_json(&raw).map_err(|e| {
        anyhow!(
            "{e} (finish_reason={}, {} chars: {:?})",
            finish.as_deref().unwrap_or("?"),
            raw.len(),
            one_line(&raw, 160)
        )
    })?;
    w.body.retain(|p| !p.trim().is_empty());
    if w.body.is_empty() {
        bail!("writer returned an empty dispatch");
    }
    // The headline is displayed in the same slot as the template one, which is
    // upper case; normalise so a model that ignored that does not stand out.
    w.headline = w.headline.trim().trim_end_matches('.').to_uppercase();
    if w.headline.is_empty() {
        bail!("writer returned an empty headline");
    }
    w.voice = Some(voice);
    Ok(w)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_plain_json() {
        let w = extract_json(r#"{"headline":"THE LINE HOLDS","body":["one","two"]}"#)
            .unwrap();
        assert_eq!(w.headline, "THE LINE HOLDS");
        assert_eq!(w.body.len(), 2);
    }

    #[test]
    fn extracts_fenced_json() {
        // Models fence their output whatever the instructions say.
        let raw = "Here you go:\n```json\n{\"headline\":\"GORI FALLS\",\"body\":[\"a\"]}\n```\n";
        let w = extract_json(raw).unwrap();
        assert_eq!(w.headline, "GORI FALLS");
    }

    #[test]
    fn the_theme_picks_the_voices() {
        // Unthemed: one voice, and its prompt is the diary's own, unchanged.
        assert_eq!(voices_for(None), (Voice::Default, None));
        assert_eq!(voices_for(Some("vector")), (Voice::Default, None));
        let f = Factions::new(
            Some("NATO"),
            Some("the Soviet Union"),
            Some("NATO"),
            Some("Soviet"),
        );
        assert!(matches!(Voice::Default.system(&f), Cow::Borrowed(s) if s == SYSTEM));

        // Red Storm: the 1985 Western wire first, the Soviet communiqué second.
        assert_eq!(voices_for(Some("redstorm")), (Voice::Western, Some(Voice::Soviet)));
        let west = Voice::Western.system(&f);
        assert!(west.contains("Western wire service in 1985"), "{west}");
        assert!(west.contains("between NATO and the Soviet Union"), "{west}");
        assert!(west.contains("no drones") && west.contains("GPS"), "{west}");
        let sov = Voice::Soviet.system(&f);
        assert!(sov.contains("communiqué") && sov.contains("'the aggressor'"), "{sov}");
        assert!(
            sov.contains("the war against NATO") && sov.contains("Soviet high command")
        );
        assert!(sov.contains("NEVER INVENT AN EVENT"), "{sov}");
        // Both keep the reply contract the parser reads.
        for p in [&west, &sov] {
            assert!(p.ends_with(REPLY_FORM), "{p}");
        }
        // Who is Soviet follows the names, not the colour.
        let flipped =
            Factions::new(Some("Soviet Union"), Some("NATO"), Some("Soviet"), None);
        assert!(Voice::Soviet.system(&flipped).contains("the war against NATO"));

        // The ids the page and the worker link use.
        assert_eq!(serde_json::to_string(&Voice::Soviet).unwrap(), "\"soviet\"");
        assert_eq!(Voice::Soviet.id(), "soviet");
        assert!(Voice::Western.default_stands_in() && !Voice::Soviet.default_stands_in());
    }

    #[test]
    fn the_second_voice_varies_against_its_own_days() {
        let mut d: NewsDigest = serde_json::from_str(
            r#"{"day":"1985-08-02","generated":"1985-08-02T12:00:00Z","round":1,
            "headline":"KUTAISI FALLS","body":["Soviet forces took Kutaisi."],
            "items":[],"facts":{"blue_captures":0,"red_captures":1,"objectives_traded":0,
            "air_kills":0,"ground_kills":0,"blue_held":1,"red_held":2,"neutral_held":0,
            "changed_hands":["Kutaisi"]},"final_":true}"#,
        )
        .unwrap();
        d.alt = Some(crate::news::AltDispatch {
            voice: "soviet".into(),
            headline: "KUTAISI LIBERATED".into(),
            body: vec!["Our forces liberated Kutaisi.".into()],
            written_by: "m".into(),
        });
        let today = NewsDigest { day: "1985-08-03".into(), alt: None, ..d.clone() };
        let hist = vec![d];
        let sov = user_prompt(&today, &hist, Voice::Soviet);
        assert!(
            sov.contains("KUTAISI LIBERATED") && !sov.contains("KUTAISI FALLS"),
            "{sov}"
        );
        let west = user_prompt(&today, &hist, Voice::Western);
        assert!(west.contains("KUTAISI FALLS") && !west.contains("LIBERATED"), "{west}");
        // The default voice's brief is exactly what it was.
        assert_eq!(user_prompt(&today, &hist, Voice::Default), west);
    }

    #[test]
    fn rejects_prose_without_json() {
        assert!(extract_json("I'm sorry, I can't help with that.").is_err());
    }

    #[test]
    fn nothing_configured_means_no_writer() {
        // Guard against the env leaking a key into the "off" case.
        std::env::remove_var("BFDB_NEWS_LLM_URL");
        std::env::remove_var("BFDB_NEWS_LLM_KEY");
        std::env::remove_var("BFDB_NEWS_LLM_MODEL");
        std::env::remove_var("OPENAI_API_KEY");
        assert!(WriterCfg::resolve(None, None, None).is_none());
    }

    #[test]
    fn url_alone_is_enough_for_a_local_model() {
        let c = WriterCfg::resolve(
            Some("http://127.0.0.1:11434/v1/chat/completions".into()),
            None,
            Some("llama3.1:8b".into()),
        )
        .unwrap();
        assert!(c.key.is_none());
        assert!(!c.is_anthropic());
    }

    #[test]
    fn anthropic_is_detected_from_the_url() {
        let c = WriterCfg::resolve(
            Some("https://api.anthropic.com/v1/messages".into()),
            Some("k".into()),
            None,
        )
        .unwrap();
        assert!(c.is_anthropic());
    }
}
