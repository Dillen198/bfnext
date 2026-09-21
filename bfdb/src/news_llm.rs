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

use crate::news::NewsDigest;
use anyhow::{anyhow, bail, Result};
use serde::Deserialize;
use std::time::Duration;

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
        let key = key.or_else(|| env("BFDB_NEWS_LLM_KEY")).or_else(|| env("OPENAI_API_KEY"));
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

/// Build the day's brief — the analysis's conclusions, the standing numbers,
/// and what has already been filed.
fn user_prompt(d: &NewsDigest, history: &[NewsDigest]) -> String {
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
                format!("{c}: {} {}, {} {}", d.factions.blue, t.blue, d.factions.red, t.red)
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
    Ok(serde_json::from_str(&raw[start..=end])?)
}

#[derive(Debug, Deserialize)]
pub struct Written {
    pub headline: String,
    pub body: Vec<String>,
}

#[derive(Deserialize)]
struct OaiChoice {
    message: OaiMessage,
}
#[derive(Deserialize)]
struct OaiMessage {
    content: String,
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

/// Write the dispatch. Blocking on purpose — the caller already runs inside
/// `block_in_place`, and this happens a handful of times a day.
pub fn write_dispatch(
    cfg: &WriterCfg,
    digest: &NewsDigest,
    history: &[NewsDigest],
) -> Result<Written> {
    let prompt = user_prompt(digest, history);
    // Generous on purpose. This runs on a timer in the background and nothing
    // waits on it, while a local model on a CPU-only box can take minutes to
    // produce a few hundred tokens -- a tight timeout here would fail exactly
    // the setup that is most attractive for this job.
    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(300))
        .build()?;

    let raw = if cfg.is_anthropic() {
        let body = serde_json::json!({
            "model": cfg.model,
            "max_tokens": 1200,
            "system": SYSTEM,
            "messages": [{ "role": "user", "content": prompt }],
        });
        let mut req = client
            .post(&cfg.url)
            .header("anthropic-version", "2023-06-01")
            .json(&body);
        if let Some(k) = &cfg.key {
            req = req.header("x-api-key", k);
        }
        let res = req.send()?;
        if !res.status().is_success() {
            bail!("news writer: {} {}", res.status(), res.text().unwrap_or_default());
        }
        let parsed: AnthropicReply = res.json()?;
        parsed.content.into_iter().map(|b| b.text).collect::<Vec<_>>().join("")
    } else {
        let body = serde_json::json!({
            "model": cfg.model,
            "temperature": 0.9,
            "max_tokens": 1200,
            "messages": [
                { "role": "system", "content": SYSTEM },
                { "role": "user", "content": prompt },
            ],
        });
        let mut req = client.post(&cfg.url).json(&body);
        if let Some(k) = &cfg.key {
            req = req.bearer_auth(k);
        }
        let res = req.send()?;
        if !res.status().is_success() {
            bail!("news writer: {} {}", res.status(), res.text().unwrap_or_default());
        }
        let parsed: OaiReply = res.json()?;
        parsed
            .choices
            .into_iter()
            .next()
            .ok_or_else(|| anyhow!("no choices in reply"))?
            .message
            .content
    };

    let mut w = extract_json(&raw)?;
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
    Ok(w)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_plain_json() {
        let w = extract_json(r#"{"headline":"THE LINE HOLDS","body":["one","two"]}"#).unwrap();
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
