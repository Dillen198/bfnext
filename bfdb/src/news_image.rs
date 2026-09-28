//! A picture for each day of the war diary.
//!
//! `news.rs` decides what happened and `news_llm.rs` writes it up; this module
//! asks an image model for one "wire photo" to run beside the finished
//! dispatch. It is decoration, so everything about it is conservative:
//!
//!  * **Off unless configured.** No provider / account id / url / key, no calls.
//!  * **One image per dispatch, made once.** Only a *filed* day (`final_`) is
//!    illustrated -- the running day's headline still moves. Once stored an
//!    image is never replaced by the generator; only an admin's explicit
//!    regenerate does that.
//!  * **Bounded cost.** A per-instance and a process-wide daily cap on calls,
//!    a short backfill window, and a dispatch whose calls keep failing is given
//!    up on after `MAX_ATTEMPTS` (one WARN, then silence).
//!  * **Nothing private leaves.** Pilots are real people. Their names (and
//!    anything that looks like a callsign or a quote) are stripped from the
//!    story before it becomes a prompt, and the prompt forbids identifiable
//!    people, lettering, flags and gore.
//!
//! Three providers (`--news-image-provider`):
//!
//!  * `cloudflare` -- Workers AI, FLUX.1 schnell. Free: 10,000 neurons a day
//!    at ~40 an image. Needs an account id and a "Workers AI" API token.
//!  * `pollinations` -- pollinations.ai. With an account key (`sk_...`, sent
//!    only as a Bearer header) it uses the current API and calls are spaced
//!    `--news-image-min-interval` apart (3 s); without one, the legacy
//!    anonymous host at one call per 15 s (enforced here), which may
//!    watermark and is no longer documented. Also usable as a keyless
//!    fallback when the provider's call fails.
//!  * `openai` -- any OpenAI-compatible `images/generations` endpoint (paid):
//!    OpenAI (`gpt-image-1`, `dall-e-3`), Together, most gateways. Both reply
//!    forms are handled -- inline `b64_json`, and a `url`, which is
//!    downloaded at once because those links expire within the hour.

use crate::{
    db::{RoundId, StatsDb},
    instance::InstanceCfg,
    news::NewsDigest,
};
use anyhow::{anyhow, bail, Result};
use base64::Engine;
use chrono::{DateTime, Duration as ChronoDuration, Utc};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashSet,
    io::Read,
    sync::Mutex,
    time::{Duration, Instant},
};

/// OpenAI's endpoint, when the `openai` provider is given only a key.
const DEFAULT_URL: &str = "https://api.openai.com/v1/images/generations";
const DEFAULT_MODEL: &str = "gpt-image-1";
/// Cloudflare Workers AI: `{account}` is filled in. FLUX.1 schnell is ~40
/// neurons an image against a free 10,000 a day.
const CF_URL: &str = "https://api.cloudflare.com/client/v4/accounts/{account}/ai/run/{model}";
const CF_MODEL: &str = "@cf/black-forest-labs/flux-1-schnell";
/// Workers AI's FLUX schnell takes 1..=8 diffusion steps.
const CF_DEFAULT_STEPS: u32 = 6;
const CF_MAX_STEPS: u32 = 8;
const CF_PROMPT_MAX: usize = 2048;
/// Pollinations with an account key (`sk_...`): the current API, prompt in
/// the path. The key goes in the `Authorization` header only -- the service
/// also accepts `?key=`, which would put it in every log that records a URL.
const POLLINATIONS_KEYED_URL: &str = "https://gen.pollinations.ai/image/";
/// Pollinations without a key: the legacy anonymous host. No longer in
/// their docs, so it may stop answering; the keyed API is the supported one.
const POLLINATIONS_URL: &str = "https://image.pollinations.ai/prompt/";
/// The anonymous host's model. With a key the model is left to the service's
/// default unless `--news-image-model` names one.
const POLLINATIONS_MODEL: &str = "flux";
const POLLINATIONS_SIZE: &str = "1024x576";
/// Anonymous Pollinations allows one request per 15 s; a second of slack.
/// Nothing configured can make an anonymous call come sooner.
const POLLINATIONS_ANON_SPACING: Duration = Duration::from_secs(16);
/// Default gap between keyed Pollinations calls (`--news-image-min-interval`).
pub const POLLINATIONS_KEYED_SPACING_SECS: u32 = 3;
/// Square is the one size every OpenAI-style model accepts (DALL-E 2/3,
/// gpt-image-1, FLUX via Together). Wider sizes are per-model; pass one with
/// `--news-image-size` if the model supports it.
pub const DEFAULT_SIZE: &str = "1024x1024";
pub const DEFAULT_PER_INSTANCE_DAILY: u32 = 3;
pub const DEFAULT_GLOBAL_DAILY: u32 = 8;

/// A single generation, including the download of a `url` reply.
const CALL_TIMEOUT: Duration = Duration::from_secs(90);
/// Anything bigger is not a news photo. Also comfortably under Discord's
/// attachment limit, which the bot uploads it against.
pub const MAX_IMAGE_BYTES: usize = 8 * 1024 * 1024;
/// Failed calls per dispatch before it is given up on.
const MAX_ATTEMPTS: u32 = 3;
/// Filed days this far back are illustrated when missing. Turning images on
/// mid-campaign should picture the last few days, not spend the daily cap on
/// a season of history.
pub const BACKFILL_DAYS: i64 = 3;
/// With a writer configured, a filed day still carrying template prose is
/// usually about to be rewritten (new headline). Wait this long for that
/// before illustrating the template version anyway.
const WRITER_GRACE: ChronoDuration = ChronoDuration::hours(6);
/// Cap on the story text that goes into a prompt.
const STORY_CHARS: usize = 420;

/// The look, when nobody overrides it. The safety rules in `RULES` are
/// appended whatever the style says.
const DEFAULT_STYLE: &str = "Realistic military photojournalism, a war correspondent's \
photograph: natural light, shallow depth of field, cinematic composition, muted \
colours, 35mm film grain. Era-appropriate vehicles, aircraft and uniforms.";

/// Always appended. The model is told, not trusted, so these are repeated in
/// the plainest words.
const RULES: &str = "The image must contain no text, lettering, captions, numbers, \
signs or watermarks. No identifiable real people, politicians or public figures; \
soldiers are distant or seen from behind. No national flags or real military \
insignia. No gore, blood, bodies or casualties. No brand logos.";

/// Who draws the pictures.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Provider {
    /// Any OpenAI-compatible `images/generations` endpoint (paid).
    OpenAi,
    /// Cloudflare Workers AI (free tier: 10,000 neurons a day).
    Cloudflare,
    /// pollinations.ai (free, no key; the free tier may watermark).
    Pollinations,
}

impl Provider {
    fn parse(s: &str) -> Result<Option<Self>> {
        Ok(match s.trim().to_ascii_lowercase().as_str() {
            "" | "none" | "off" => None,
            "openai" => Some(Self::OpenAi),
            "cloudflare" | "cf" => Some(Self::Cloudflare),
            "pollinations" => Some(Self::Pollinations),
            other => bail!("unknown news image provider {other:?} (openai | cloudflare | pollinations)"),
        })
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::OpenAi => "openai",
            Self::Cloudflare => "cloudflare",
            Self::Pollinations => "pollinations",
        }
    }
}

/// The raw flags / environment, before a provider is settled on.
#[derive(Debug, Default)]
pub struct ImageArgs {
    pub provider: Option<String>,
    pub url: Option<String>,
    pub key: Option<String>,
    pub model: Option<String>,
    pub size: Option<String>,
    pub quality: Option<String>,
    pub style: Option<String>,
    pub cf_account_id: Option<String>,
    pub steps: Option<u32>,
    pub fallback: Option<String>,
    /// Seconds between Pollinations calls with a key (default 3).
    pub min_interval_secs: Option<u32>,
    pub per_instance_daily: u32,
    pub global_daily: u32,
}

#[derive(Debug, Clone)]
pub struct ImageCfg {
    pub provider: Provider,
    /// The full endpoint (OpenAI, Cloudflare) or the prompt-path base
    /// (Pollinations).
    pub url: String,
    /// Bearer token. For Cloudflare the Workers AI API token; for Pollinations
    /// an optional account token (lifts the watermark and the rate limit).
    pub key: Option<String>,
    pub model: String,
    pub size: String,
    /// Passed through when set (`gpt-image-1`: low / medium / high -- the
    /// main cost knob there). OpenAI provider only.
    pub quality: Option<String>,
    /// Replaces `DEFAULT_STYLE` for every instance without its own.
    pub style: Option<String>,
    /// Cloudflare diffusion steps, 1..=8.
    pub steps: u32,
    /// Tried once, keyless, when the provider's own call fails.
    pub fallback: Option<Provider>,
    /// Where that fallback goes -- always Pollinations; a field so the tests
    /// can point it at a local stub.
    pub fallback_url: String,
    /// Least gap between two calls to pollinations.ai, process-wide.
    pub min_interval: Duration,
    pub per_instance_daily: u32,
    pub global_daily: u32,
}

impl ImageCfg {
    /// Settle the provider from the flags, falling back to the environment.
    ///
    /// `Ok(None)` is "off". An explicit provider wins; otherwise a Cloudflare
    /// account id means Cloudflare, an OpenAI url or key means OpenAI, and
    /// nothing at all means no pictures. Pollinations needs no key, so it is
    /// only ever used when named. Deliberately never reads `$OPENAI_API_KEY`
    /// the way the writer does: a paid provider is switched on by name only.
    pub fn resolve(a: ImageArgs) -> Result<Option<Self>> {
        let env = |k: &str| std::env::var(k).ok().filter(|v| !v.trim().is_empty());
        let nonempty = |v: Option<String>| v.map(|s| s.trim().to_string()).filter(|s| !s.is_empty());
        let url = nonempty(a.url).or_else(|| env("BFDB_NEWS_IMAGE_URL"));
        let key = nonempty(a.key).or_else(|| env("BFDB_NEWS_IMAGE_KEY"));
        let model = nonempty(a.model).or_else(|| env("BFDB_NEWS_IMAGE_MODEL"));
        let account = nonempty(a.cf_account_id).or_else(|| env("BFDB_NEWS_IMAGE_CF_ACCOUNT_ID"));
        let provider = match nonempty(a.provider).or_else(|| env("BFDB_NEWS_IMAGE_PROVIDER")) {
            Some(p) => match Provider::parse(&p)? {
                Some(p) => p,
                None => return Ok(None),
            },
            None if account.is_some() => Provider::Cloudflare,
            None if url.is_some() || key.is_some() => Provider::OpenAi,
            None => return Ok(None),
        };
        let fallback = match nonempty(a.fallback) {
            Some(f) => match Provider::parse(&f)? {
                None => None,
                Some(Provider::Pollinations) => Some(Provider::Pollinations),
                Some(p) => bail!("news image fallback can only be pollinations, not {}", p.name()),
            },
            None => None,
        }
        // Falling back to itself is just a second try.
        .filter(|f| *f != provider);
        let (url, model, size) = match provider {
            Provider::OpenAi => {
                if url.is_none() && key.is_none() {
                    bail!("the openai news image provider needs --news-image-url or --news-image-key");
                }
                (
                    url.unwrap_or_else(|| DEFAULT_URL.to_string()),
                    model.unwrap_or_else(|| DEFAULT_MODEL.to_string()),
                    nonempty(a.size).unwrap_or_else(|| DEFAULT_SIZE.to_string()),
                )
            }
            Provider::Cloudflare => {
                if key.is_none() {
                    bail!("the cloudflare news image provider needs an API token (--news-image-key)");
                }
                let model = model.unwrap_or_else(|| CF_MODEL.to_string());
                let url = match (url, &account) {
                    // An explicit endpoint (e.g. through an AI Gateway) as given.
                    (Some(u), _) => u,
                    (None, Some(acct)) => CF_URL.replace("{account}", acct).replace("{model}", &model),
                    (None, None) => bail!(
                        "the cloudflare news image provider needs --news-image-cf-account-id"
                    ),
                };
                // FLUX schnell on Workers AI draws squares only.
                (url, model, "1024x1024".to_string())
            }
            Provider::Pollinations => {
                let keyed = key.is_some();
                let default_url = if keyed { POLLINATIONS_KEYED_URL } else { POLLINATIONS_URL };
                let default_model = if keyed { "" } else { POLLINATIONS_MODEL };
                (
                    url.unwrap_or_else(|| default_url.to_string()),
                    model.unwrap_or_else(|| default_model.to_string()),
                    nonempty(a.size).unwrap_or_else(|| POLLINATIONS_SIZE.to_string()),
                )
            }
        };
        let min_interval = if provider == Provider::Pollinations && key.is_some() {
            Duration::from_secs(a.min_interval_secs.unwrap_or(POLLINATIONS_KEYED_SPACING_SECS) as u64)
        } else {
            POLLINATIONS_ANON_SPACING.max(Duration::from_secs(a.min_interval_secs.unwrap_or(0) as u64))
        };
        Ok(Some(Self {
            provider,
            url,
            key,
            model,
            size,
            quality: nonempty(a.quality),
            style: nonempty(a.style),
            steps: a.steps.unwrap_or(CF_DEFAULT_STEPS).clamp(1, CF_MAX_STEPS),
            fallback,
            fallback_url: POLLINATIONS_URL.to_string(),
            min_interval,
            per_instance_daily: a.per_instance_daily,
            global_daily: a.global_daily,
        }))
    }

    /// The keyless Pollinations config a failed call falls back to. Never
    /// carries this config's key: a Cloudflare token must not be sent to a
    /// different service.
    fn fallback_cfg(&self) -> Option<Self> {
        (self.fallback == Some(Provider::Pollinations)).then(|| Self {
            provider: Provider::Pollinations,
            url: self.fallback_url.clone(),
            key: None,
            model: POLLINATIONS_MODEL.to_string(),
            size: POLLINATIONS_SIZE.to_string(),
            quality: None,
            style: None,
            steps: self.steps,
            fallback: None,
            fallback_url: self.fallback_url.clone(),
            min_interval: POLLINATIONS_ANON_SPACING,
            per_instance_daily: self.per_instance_daily,
            global_daily: self.global_daily,
        })
    }

    /// `provider:model`, as stored with a picture.
    pub fn label(&self) -> String {
        let model = if self.model.is_empty() { "default" } else { &self.model };
        format!("{}:{model}", self.provider.name())
    }

    fn is_together(&self) -> bool {
        self.url.contains("together")
    }

    /// The OpenAI-style JSON body. The common subset (`model`, `prompt`,
    /// `n`, `size`) plus the few per-provider knobs that matter.
    fn request_body(&self, prompt: &str) -> serde_json::Value {
        let mut body = serde_json::json!({
            "model": self.model,
            "prompt": prompt,
            "n": 1,
            "size": self.size,
        });
        // DALL-E defaults to a short-lived URL; ask for the bytes inline.
        // gpt-image-1 always answers b64_json and rejects the parameter.
        if self.model.starts_with("dall-e") {
            body["response_format"] = serde_json::json!("b64_json");
        }
        if let Some(q) = &self.quality {
            body["quality"] = serde_json::json!(q);
        }
        // Together sizes by width/height rather than `size`.
        if self.is_together() {
            if let Some((w, h)) = parse_size(&self.size) {
                body["width"] = serde_json::json!(w);
                body["height"] = serde_json::json!(h);
            }
        }
        body
    }

    /// Workers AI body: prompt (at most 2048 chars), steps and a fresh seed.
    /// No size -- FLUX schnell there is square.
    fn cf_body(&self, prompt: &str, seed: u32) -> serde_json::Value {
        serde_json::json!({
            "prompt": prompt.chars().take(CF_PROMPT_MAX).collect::<String>(),
            "steps": self.steps,
            "seed": seed,
        })
    }

    /// Pollinations request URL: the prompt is the path. Never the key.
    fn pollinations_url(&self, prompt: &str, seed: u32) -> String {
        let (w, h) = parse_size(&self.size).unwrap_or((1024, 576));
        let model = if self.model.is_empty() {
            String::new()
        } else {
            format!("&model={}", urlencoding::encode(&self.model))
        };
        format!(
            "{}/{}?width={w}&height={h}{model}&seed={seed}&nologo=true&private=true&safe=true",
            self.url.trim_end_matches('/'),
            urlencoding::encode(prompt),
        )
    }
}

fn parse_size(s: &str) -> Option<(u32, u32)> {
    let (w, h) = s.split_once(['x', 'X'])?;
    Some((w.trim().parse().ok()?, h.trim().parse().ok()?))
}

fn random_seed() -> u32 {
    (uuid::Uuid::new_v4().as_u128() % (i32::MAX as u128)) as u32
}

// ── the prompt ───────────────────────────────────────────────────────────────

/// Where and when the war is, in words an image model can draw. Per instance:
/// an explicit `news_image_setting` wins; then the scenario's own name (the
/// 2008 Caucasus campaign is RGW2008), then the theatre the objectives sit in.
pub fn campaign_setting(cfg: &InstanceCfg, theatre: &str) -> String {
    if let Some(s) = cfg.news_image_setting.as_deref().map(str::trim).filter(|s| !s.is_empty()) {
        return s.to_string();
    }
    let hay = [
        Some(cfg.id.as_str()),
        cfg.label.as_deref(),
        cfg.dcs_server_name.as_deref(),
        cfg.engine_config.as_deref().and_then(|p| p.file_name()).and_then(|f| f.to_str()),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join(" ")
    .to_lowercase();
    if hay.contains("rgw") || hay.contains("2008") {
        return "the August 2008 Russo-Georgian war in the Caucasus: green mountain \
                valleys and villages of Georgia, late-2000s Soviet-pattern armour, \
                helicopters and jets"
            .to_string();
    }
    match theatre {
        "Syria" => "a present-day war in Syria and the Levant: dry hills, desert \
                    airbases and towns of the eastern Mediterranean, modern military \
                    equipment"
            .to_string(),
        "Caucasus" => "a present-day war in the Caucasus: mountain valleys and the \
                       Black Sea coast of Georgia, modern military equipment"
            .to_string(),
        "Normandy" => "Normandy in 1944: Second World War aircraft, armour and \
                       hedgerow country"
            .to_string(),
        "" => "a present-day war: modern military equipment".to_string(),
        t => format!("a present-day war in the {t} region: modern military equipment"),
    }
}

/// Everyone the digest names who is a private individual -- the pilots.
fn private_names(d: &NewsDigest) -> Vec<String> {
    let mut out: Vec<String> = d
        .items
        .iter()
        .flat_map(|it| {
            let mut v: Vec<String> =
                it.vars.iter().filter(|(k, _)| k.contains("pilot")).map(|(_, v)| v.clone()).collect();
            if it.angle.contains("pilot") {
                v.push(it.subject.clone());
            }
            v
        })
        .map(|s| s.trim().to_string())
        .filter(|s| s.chars().count() >= 2)
        .collect();
    // Longest first, so "Viper 1-1 | Bob" goes before "Bob".
    out.sort_by_key(|s| std::cmp::Reverse(s.len()));
    out.dedup();
    out
}

/// Case-insensitive replace of every `needle` in `hay`.
fn replace_ci(hay: &str, needle: &str, with: &str) -> String {
    let lower = hay.to_lowercase();
    let n = needle.to_lowercase();
    // Lower-casing can change byte lengths outside ASCII; fall back to an
    // exact-case replace rather than cut a string at a bad index.
    if lower.len() != hay.len() || n.len() != needle.len() || n.is_empty() {
        return hay.replace(needle, with);
    }
    let mut out = String::with_capacity(hay.len());
    let mut i = 0;
    while let Some(off) = lower[i..].find(&n) {
        out.push_str(&hay[i..i + off]);
        out.push_str(with);
        i += off + n.len();
    }
    out.push_str(&hay[i..]);
    out
}

/// Strip what a prompt must not carry: the named pilots, anything quoted or
/// bracketed (callsigns, squadron tags, quoted remarks), and control
/// characters; then collapse whitespace.
pub fn sanitise(text: &str, names: &[String]) -> String {
    let mut s = text.to_string();
    for n in names {
        s = replace_ci(&s, n, "a pilot");
    }
    let mut out = String::with_capacity(s.len());
    let mut close: Option<char> = None;
    for c in s.chars() {
        if let Some(end) = close {
            if c == end {
                close = None;
            }
            continue;
        }
        close = match c {
            '"' => Some('"'),
            '\u{201C}' => Some('\u{201D}'),
            '[' => Some(']'),
            '(' => Some(')'),
            '{' => Some('}'),
            '<' => Some('>'),
            _ => None,
        };
        if close.is_none() && !c.is_control() {
            out.push(c);
        }
    }
    out.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn clip(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        return s.to_string();
    }
    let cut: String = s.chars().take(max).collect();
    // Back to the last word boundary.
    match cut.rfind(' ') {
        Some(i) if i > max / 2 => format!("{}...", &cut[..i]),
        _ => format!("{cut}..."),
    }
}

/// The prompt for one dispatch: the story (sanitised), the setting, the look,
/// and the rules.
pub fn build_prompt(d: &NewsDigest, setting: &str, style: Option<&str>) -> String {
    let names = private_names(d);
    let headline = sanitise(&d.headline, &names);
    let first = d
        .body
        .first()
        .cloned()
        .or_else(|| d.items.first().map(|i| i.text.clone()))
        .unwrap_or_default();
    let story = clip(&sanitise(&first, &names), STORY_CHARS);
    // The headline is upper case wire style; a model reads that as text to
    // render. Sentence case it.
    let headline = {
        let lower = headline.to_lowercase();
        let mut c = lower.chars();
        match c.next() {
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
            None => String::new(),
        }
    };
    let style = style.map(str::trim).filter(|s| !s.is_empty()).unwrap_or(DEFAULT_STYLE);
    let mut p = format!("A news photograph illustrating this war report. Story: {headline}.");
    if !story.is_empty() {
        p.push(' ');
        p.push_str(&story);
    }
    p.push_str(&format!(" Setting: {setting}. Style: {style} {RULES}"));
    p
}

// ── the call ─────────────────────────────────────────────────────────────────

/// What an images endpoint handed back.
#[derive(Debug, PartialEq)]
enum Returned {
    Bytes(Vec<u8>),
    Url(String),
}

/// Read `data[0]` of an OpenAI-style reply: inline base64 (`b64_json`, also
/// a `data:` URL some gateways put in `url`) or a link to download.
fn parse_reply(v: &serde_json::Value) -> Result<Returned> {
    let first = v
        .get("data")
        .and_then(|d| d.as_array())
        .and_then(|a| a.first())
        .ok_or_else(|| anyhow!("no data[] in reply"))?;
    if let Some(b) = first.get("b64_json").and_then(|b| b.as_str()).filter(|b| !b.is_empty()) {
        return Ok(Returned::Bytes(decode_b64(b)?));
    }
    if let Some(u) = first.get("url").and_then(|u| u.as_str()).filter(|u| !u.is_empty()) {
        if let Some(rest) = u.strip_prefix("data:") {
            let (_, b64) = rest.split_once(',').ok_or_else(|| anyhow!("malformed data: URL"))?;
            return Ok(Returned::Bytes(decode_b64(b64)?));
        }
        if !(u.starts_with("https://") || u.starts_with("http://")) {
            bail!("reply url is not http(s)");
        }
        return Ok(Returned::Url(u.to_string()));
    }
    bail!("reply has neither b64_json nor url")
}

fn decode_b64(b64: &str) -> Result<Vec<u8>> {
    Ok(base64::engine::general_purpose::STANDARD.decode(b64.trim())?)
}

/// The `errors[]` of a Cloudflare API envelope as one line, if it says the
/// call failed (`success: false` or any error listed).
fn cf_error(v: &serde_json::Value) -> Option<String> {
    let errors: Vec<String> = v
        .get("errors")
        .and_then(|e| e.as_array())
        .map(|a| {
            a.iter()
                .map(|e| match (e.get("code"), e.get("message").and_then(|m| m.as_str())) {
                    (Some(c), Some(m)) => format!("{c}: {m}"),
                    (_, Some(m)) => m.to_string(),
                    _ => e.to_string(),
                })
                .collect()
        })
        .unwrap_or_default();
    let failed = v.get("success").and_then(|s| s.as_bool()) == Some(false);
    if errors.is_empty() && !failed {
        return None;
    }
    Some(if errors.is_empty() { "success: false".to_string() } else { errors.join("; ") })
}

/// The image in a Workers AI reply: the REST envelope's `result.image`, or a
/// bare `image` (the model's own output shape), base64 either way.
fn parse_cf_reply(v: &serde_json::Value) -> Result<Vec<u8>> {
    if let Some(e) = cf_error(v) {
        bail!("cloudflare: {e}");
    }
    let b64 = v
        .pointer("/result/image")
        .or_else(|| v.get("image"))
        .and_then(|i| i.as_str())
        .filter(|s| !s.is_empty())
        .ok_or_else(|| anyhow!("cloudflare: no result.image in reply"))?;
    decode_b64(b64)
}

/// Out of free neurons, or throttled: the quota's fault, not the dispatch's.
fn cf_quota(status: u16, text: &str) -> bool {
    let t = text.to_ascii_lowercase();
    status == 429
        || t.contains("neuron")
        || t.contains("daily free allocation")
        || t.contains("rate limit")
        || t.contains("capacity temporarily exceeded")
}

/// PNG, JPEG or WebP and not too big -- else an error naming why.
pub fn validate(bytes: &[u8]) -> Result<&'static str> {
    if bytes.is_empty() {
        bail!("empty image");
    }
    if bytes.len() > MAX_IMAGE_BYTES {
        bail!("image is {} bytes, over the {MAX_IMAGE_BYTES} cap", bytes.len());
    }
    crate::websec::sniff_image(bytes).ok_or_else(|| anyhow!("reply is not a PNG, JPEG or WebP"))
}

/// Why a call did not produce an image.
#[derive(Debug)]
pub struct GenError {
    pub msg: String,
    /// The endpoint answered 2xx, so the call was probably billed even though
    /// the image was unusable. Counts against the daily caps.
    pub billed: bool,
    /// A 429 / quota refusal: not this dispatch's fault, not an attempt.
    pub rate_limited: bool,
}

impl std::fmt::Display for GenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

fn one_line(s: &str, max: usize) -> String {
    let s = s.split_whitespace().collect::<Vec<_>>().join(" ");
    if s.chars().count() > max {
        format!("{}...", s.chars().take(max).collect::<String>())
    } else {
        s
    }
}

/// Keeps calls to one host at least `min` apart. `reserve` books the next
/// slot and says how long to wait for it.
#[derive(Debug, Default)]
struct Spacing {
    next: Option<Instant>,
}

impl Spacing {
    fn reserve(&mut self, now: Instant, min: Duration) -> Duration {
        let start = self.next.map(|n| n.max(now)).unwrap_or(now);
        self.next = Some(start + min);
        start - now
    }
}

/// Pollinations' rate limit, process-wide.
static POLLINATIONS_GATE: Mutex<Spacing> = Mutex::new(Spacing { next: None });

fn fail(msg: String, billed: bool) -> GenError {
    GenError { msg, billed, rate_limited: false }
}

/// A transport error without its URL: the URL carries the whole prompt
/// (Pollinations) and adds nothing to a log line.
fn send_failed(e: reqwest::Error) -> GenError {
    fail(format!("request failed: {}", e.without_url()), false)
}

/// `msg` with every occurrence of the secret masked. Applied to every error
/// a call returns, since a provider may echo the key back in its body.
fn redact(msg: &str, key: Option<&str>) -> String {
    match key.map(str::trim).filter(|k| k.len() >= 4) {
        Some(k) => msg.replace(k, "***"),
        None => msg.to_string(),
    }
}

/// Make one image with `cfg`'s provider. Blocking: the callers are already
/// off the async runtime.
fn generate(cfg: &ImageCfg, prompt: &str) -> std::result::Result<Vec<u8>, GenError> {
    generate_raw(cfg, prompt).map_err(|mut e| {
        e.msg = redact(&e.msg, cfg.key.as_deref());
        e
    })
}

fn generate_raw(cfg: &ImageCfg, prompt: &str) -> std::result::Result<Vec<u8>, GenError> {
    let client = reqwest::blocking::Client::builder()
        .timeout(CALL_TIMEOUT)
        .build()
        .map_err(|e| fail(e.without_url().to_string(), false))?;
    let bytes = match cfg.provider {
        Provider::OpenAi => generate_openai(&client, cfg, prompt)?,
        Provider::Cloudflare => generate_cf(&client, cfg, prompt)?,
        Provider::Pollinations => generate_pollinations(&client, cfg, prompt)?,
    };
    validate(&bytes).map_err(|e| fail(e.to_string(), true))?;
    Ok(bytes)
}

fn generate_openai(
    client: &reqwest::blocking::Client,
    cfg: &ImageCfg,
    prompt: &str,
) -> std::result::Result<Vec<u8>, GenError> {
    let mut req = client.post(&cfg.url).json(&cfg.request_body(prompt));
    if let Some(k) = &cfg.key {
        req = req.bearer_auth(k);
    }
    let res = req.send().map_err(send_failed)?;
    let status = res.status();
    if !status.is_success() {
        let body = one_line(&res.text().unwrap_or_default(), 300);
        return Err(GenError {
            msg: format!("{status} {body}"),
            billed: false,
            rate_limited: status == reqwest::StatusCode::TOO_MANY_REQUESTS,
        });
    }
    let v: serde_json::Value =
        res.json().map_err(|e| fail(format!("unreadable reply: {}", e.without_url()), true))?;
    match parse_reply(&v).map_err(|e| fail(e.to_string(), true))? {
        Returned::Bytes(b) => Ok(b),
        Returned::Url(u) => download(client, &u).map_err(|e| fail(e.to_string(), true)),
    }
}

fn generate_cf(
    client: &reqwest::blocking::Client,
    cfg: &ImageCfg,
    prompt: &str,
) -> std::result::Result<Vec<u8>, GenError> {
    let mut req = client.post(&cfg.url).json(&cfg.cf_body(prompt, random_seed()));
    if let Some(k) = &cfg.key {
        req = req.bearer_auth(k);
    }
    let res = req.send().map_err(send_failed)?;
    let status = res.status();
    let text = res
        .text()
        .map_err(|e| fail(format!("unreadable reply: {}", e.without_url()), status.is_success()))?;
    // Errors come back as the same JSON envelope, with the reason in errors[].
    let v: Option<serde_json::Value> = serde_json::from_str(&text).ok();
    if !status.is_success() {
        let why = v.as_ref().and_then(cf_error).unwrap_or_else(|| one_line(&text, 300));
        return Err(GenError {
            rate_limited: cf_quota(status.as_u16(), &why),
            msg: format!("cloudflare {status} {why}"),
            billed: false,
        });
    }
    let v = v.ok_or_else(|| fail(format!("cloudflare: not JSON: {}", one_line(&text, 160)), true))?;
    parse_cf_reply(&v).map_err(|e| {
        let msg = e.to_string();
        GenError { rate_limited: cf_quota(200, &msg), msg, billed: true }
    })
}

fn generate_pollinations(
    client: &reqwest::blocking::Client,
    cfg: &ImageCfg,
    prompt: &str,
) -> std::result::Result<Vec<u8>, GenError> {
    // Only the real service is rate limited; a local stub / self-hosted
    // instance is not.
    if cfg.url.contains("pollinations.ai") {
        let wait = POLLINATIONS_GATE
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .reserve(Instant::now(), cfg.min_interval);
        if !wait.is_zero() {
            std::thread::sleep(wait);
        }
    }
    let mut req = client.get(cfg.pollinations_url(prompt, random_seed()));
    if let Some(k) = &cfg.key {
        req = req.bearer_auth(k);
    }
    let res = req.send().map_err(send_failed)?;
    let status = res.status();
    if !status.is_success() {
        let body = one_line(&res.text().unwrap_or_default(), 300);
        return Err(GenError {
            msg: format!("pollinations {status} {body}"),
            billed: false,
            // 429 throttled, 402 the account's pollen budget is spent: both
            // the quota's fault, not the dispatch's.
            rate_limited: matches!(status.as_u16(), 402 | 429),
        });
    }
    read_capped(res).map_err(|e| fail(format!("pollinations: {e}"), true))
}

/// Fetch a returned image URL at once (they expire), never more than the cap.
fn download(client: &reqwest::blocking::Client, url: &str) -> Result<Vec<u8>> {
    let res = client.get(url).send().map_err(|e| e.without_url())?;
    if !res.status().is_success() {
        bail!("image download: {}", res.status());
    }
    read_capped(res)
}

fn read_capped(res: reqwest::blocking::Response) -> Result<Vec<u8>> {
    if res.content_length().map(|n| n as usize > MAX_IMAGE_BYTES).unwrap_or(false) {
        bail!("image is over the {MAX_IMAGE_BYTES} byte cap");
    }
    let mut buf = Vec::new();
    res.take(MAX_IMAGE_BYTES as u64 + 1).read_to_end(&mut buf)?;
    Ok(buf)
}

/// The provider's call, then -- if it failed and a fallback is configured --
/// one keyless try at Pollinations. Returns the bytes and who drew them. A
/// double failure reports the provider's error (and its rate-limit flag),
/// with the fallback's appended.
fn generate_with_fallback(
    cfg: &ImageCfg,
    prompt: &str,
) -> std::result::Result<(Vec<u8>, String), GenError> {
    match generate(cfg, prompt) {
        Ok(b) => Ok((b, cfg.label())),
        Err(e) => {
            let Some(fb) = cfg.fallback_cfg() else { return Err(e) };
            log::info!("news image: {} failed ({e}); trying {}", cfg.provider.name(), fb.label());
            match generate(&fb, prompt) {
                Ok(b) => Ok((b, fb.label())),
                Err(fe) => Err(GenError { msg: format!("{}; fallback: {}", e.msg, fe.msg), ..e }),
            }
        }
    }
}

// ── bookkeeping ──────────────────────────────────────────────────────────────

/// Per-dispatch image state, stored as JSON (see `StatsDb::news_image_meta`)
/// so it can grow fields without breaking rows an older build wrote.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ImageMeta {
    /// Bytes are stored for this dispatch.
    #[serde(default)]
    pub has_image: bool,
    /// Bumped on every stored image; the dashboard URL carries it so a
    /// regenerated picture is not served from cache.
    #[serde(default)]
    pub version: u32,
    #[serde(default)]
    pub created: Option<DateTime<Utc>>,
    #[serde(default)]
    pub model: String,
    /// The headline the picture was made for.
    #[serde(default)]
    pub headline: String,
    /// The prompt sent, for an admin wondering why the picture looks as it does.
    #[serde(default)]
    pub prompt: String,
    /// Consecutive failed calls since the last success (or forced retry).
    #[serde(default)]
    pub attempts: u32,
    #[serde(default)]
    pub next_try: Option<DateTime<Utc>>,
    /// `MAX_ATTEMPTS` failures: the generator leaves this dispatch alone.
    #[serde(default)]
    pub gave_up: bool,
    #[serde(default)]
    pub last_error: Option<String>,
}

/// Calls in flight, so the generator tick and an admin regenerate never pay
/// for the same dispatch twice at once.
static IN_FLIGHT: Mutex<Option<HashSet<(String, u64, String)>>> = Mutex::new(None);

struct FlightGuard((String, u64, String));

impl FlightGuard {
    fn take(inst: &str, rid: RoundId, day: &str) -> Option<Self> {
        let key = (inst.to_string(), rid.0, day.to_string());
        let mut g = IN_FLIGHT.lock().unwrap_or_else(|e| e.into_inner());
        g.get_or_insert_with(HashSet::new).insert(key.clone()).then_some(Self(key))
    }
}

impl Drop for FlightGuard {
    fn drop(&mut self) {
        let mut g = IN_FLIGHT.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(s) = g.as_mut() {
            s.remove(&self.0);
        }
    }
}

/// Scope key of the process-wide counter in `news_image_count`.
const GLOBAL_SCOPE: &str = "*";

/// Whether today's caps leave room for one more call for `inst`.
fn under_caps(db: &StatsDb, cfg: &ImageCfg, inst: &str, today: &str) -> Result<Option<String>> {
    let mine = db.news_image_count(inst, today)?;
    if mine >= cfg.per_instance_daily {
        return Ok(Some(format!(
            "daily cap for this instance reached ({mine}/{})",
            cfg.per_instance_daily
        )));
    }
    let all = db.news_image_count(GLOBAL_SCOPE, today)?;
    if all >= cfg.global_daily {
        return Ok(Some(format!("global daily cap reached ({all}/{})", cfg.global_daily)));
    }
    Ok(None)
}

/// Why a call for `inst` would be refused by today's caps, if it would --
/// so the admin route can say so up front instead of queueing a no-op.
pub fn capped(db: &StatsDb, cfg: &ImageCfg, inst: &str) -> Result<Option<String>> {
    under_caps(db, cfg, inst, &Utc::now().format("%Y-%m-%d").to_string())
}

fn count_call(db: &StatsDb, inst: &str, today: &str) -> Result<()> {
    db.news_image_count_bump(inst, today)?;
    db.news_image_count_bump(GLOBAL_SCOPE, today)?;
    Ok(())
}

/// Is this day ready to be illustrated? Filed, and -- with a writer on --
/// either written or long enough past filing that it is not going to be.
pub fn eligible(d: &NewsDigest, writer_on: bool, now: DateTime<Utc>) -> bool {
    d.final_ && (!writer_on || !d.body.is_empty() || now.signed_duration_since(d.generated) >= WRITER_GRACE)
}

/// What happened to one attempt, for the caller's log line / reply.
#[derive(Debug, PartialEq)]
pub enum Outcome {
    Stored { version: u32 },
    Busy,
    Capped(String),
    Failed(String),
}

/// Generate and store the picture for one dispatch. `forced` (the admin
/// route) ignores a stored image, a give-up and the retry backoff -- not the
/// caps. On failure the old image, if any, stays.
pub fn illustrate(
    db: &StatsDb,
    inst: &InstanceCfg,
    rid: RoundId,
    d: &NewsDigest,
    cfg: &ImageCfg,
    forced: bool,
) -> Result<Outcome> {
    let Some(_guard) = FlightGuard::take(&inst.id, rid, &d.day) else {
        return Ok(Outcome::Busy);
    };
    let now = Utc::now();
    let today = now.format("%Y-%m-%d").to_string();
    if let Some(why) = under_caps(db, cfg, &inst.id, &today)? {
        return Ok(Outcome::Capped(why));
    }
    let mut meta = db.news_image_meta(&inst.id, rid, &d.day)?.unwrap_or_default();
    if forced {
        meta.attempts = 0;
        meta.gave_up = false;
        meta.next_try = None;
    }
    let style = inst.news_image_style.as_deref().or(cfg.style.as_deref());
    let prompt = build_prompt(d, &campaign_setting(inst, &d.facts.theatre), style);
    match generate_with_fallback(cfg, &prompt) {
        Ok((bytes, drawn_by)) => {
            count_call(db, &inst.id, &today)?;
            meta.has_image = true;
            meta.version = meta.version.saturating_add(1);
            meta.created = Some(now);
            meta.model = drawn_by;
            meta.headline = d.headline.clone();
            meta.prompt = prompt;
            meta.attempts = 0;
            meta.next_try = None;
            meta.gave_up = false;
            meta.last_error = None;
            db.news_image_put(&inst.id, rid, &d.day, &bytes, &meta)?;
            Ok(Outcome::Stored { version: meta.version })
        }
        Err(e) => {
            if e.billed {
                count_call(db, &inst.id, &today)?;
            }
            if e.rate_limited {
                // The quota's fault, not the dispatch's: try again in an hour
                // without spending an attempt.
                meta.next_try = Some(now + ChronoDuration::hours(1));
            } else {
                meta.attempts = meta.attempts.saturating_add(1);
                meta.next_try =
                    Some(now + ChronoDuration::minutes(30 * (1i64 << (meta.attempts - 1).min(4))));
                if meta.attempts >= MAX_ATTEMPTS && !meta.gave_up {
                    meta.gave_up = true;
                    log::warn!(
                        "[{}] news image: giving up on {} after {} failed attempts: {e}",
                        inst.id,
                        d.day,
                        meta.attempts
                    );
                }
            }
            meta.last_error = Some(e.msg.clone());
            db.news_image_meta_put(&inst.id, rid, &d.day, &meta)?;
            Ok(Outcome::Failed(e.msg))
        }
    }
}

/// The generator's pass: illustrate at most one filed day per tick, newest
/// first, within the backfill window. Returns whether a call was made.
pub fn tick(
    db: &StatsDb,
    inst: &InstanceCfg,
    rid: RoundId,
    cfg: &ImageCfg,
    writer_on: bool,
) -> Result<bool> {
    let now = Utc::now();
    let oldest = (now - ChronoDuration::days(BACKFILL_DAYS)).format("%Y-%m-%d").to_string();
    for d in db.news_history(rid, (BACKFILL_DAYS + 2) as usize)? {
        if d.day < oldest || !eligible(&d, writer_on, now) {
            continue;
        }
        let meta = db.news_image_meta(&inst.id, rid, &d.day)?.unwrap_or_default();
        if meta.has_image || meta.gave_up || meta.next_try.map(|t| now < t).unwrap_or(false) {
            continue;
        }
        return match illustrate(db, inst, rid, &d, cfg, false)? {
            Outcome::Stored { .. } => {
                log::info!("[{}] news image: stored for {}", inst.id, d.day);
                Ok(true)
            }
            Outcome::Busy => Ok(false),
            Outcome::Capped(why) => {
                log::debug!("[{}] news image for {} deferred: {why}", inst.id, d.day);
                Ok(false)
            }
            Outcome::Failed(e) => {
                if !db.news_image_meta(&inst.id, rid, &d.day)?.map(|m| m.gave_up).unwrap_or(false) {
                    log::info!("[{}] news image for {} failed (will retry): {e}", inst.id, d.day);
                }
                Ok(true)
            }
        };
    }
    Ok(false)
}

/// The `/api/news` fields for one day: the image URL (versioned, so a
/// regenerated picture is not served from cache) and whether one is still
/// expected -- which is what the Discord feed waits on.
pub fn api_fields(
    db: &StatsDb,
    inst: &str,
    rid: RoundId,
    d: &NewsDigest,
    enabled: bool,
) -> Result<(Option<String>, bool)> {
    let meta = db.news_image_meta(inst, rid, &d.day)?;
    let url = meta.as_ref().filter(|m| m.has_image).map(|m| {
        format!(
            "/api/news/image/{}?instance={}&v={}",
            d.day,
            urlencoding::encode(inst),
            m.version
        )
    });
    let oldest = (Utc::now() - ChronoDuration::days(BACKFILL_DAYS)).format("%Y-%m-%d").to_string();
    let pending = enabled
        && url.is_none()
        && d.final_
        && d.day >= oldest
        && !meta.map(|m| m.gave_up).unwrap_or(false);
    Ok((url, pending))
}

/// A dispatch id as the routes take it: `YYYY-MM-DD`.
pub fn valid_day(s: &str) -> bool {
    s.len() == 10
        && s.bytes().enumerate().all(|(i, b)| if i == 4 || i == 7 { b == b'-' } else { b.is_ascii_digit() })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::news::{DigestFacts, NewsItem};
    use std::collections::BTreeMap;

    fn inst(json: serde_json::Value) -> InstanceCfg {
        serde_json::from_value(json).unwrap()
    }

    fn digest(headline: &str, body: &[&str], pilot: Option<&str>) -> NewsDigest {
        let mut items = vec![];
        if let Some(p) = pilot {
            let mut vars = BTreeMap::new();
            vars.insert("pilot".to_string(), p.to_string());
            items.push(NewsItem {
                angle: "pilot_standout".into(),
                subject: p.into(),
                weight: 45,
                vars,
                text: format!("{p} downed four aircraft."),
            });
        }
        NewsDigest {
            day: "2026-09-27".into(),
            generated: Utc::now(),
            round: 1,
            headline: headline.into(),
            body: body.iter().map(|s| s.to_string()).collect(),
            written_by: "m".into(),
            items,
            facts: DigestFacts { theatre: "Caucasus".into(), ..Default::default() },
            factions: Default::default(),
            facts_hash: 0,
            final_: true,
        }
    }

    #[test]
    fn pilot_names_and_callsigns_never_reach_the_prompt() {
        let d = digest(
            "VIPER 1-1 | BOB DOWNS FOUR OVER GORI",
            &["Viper 1-1 | Bob, flying as [JTF] \"Hammer\", shot down four Russian jets near Gori (callsign Hammer 2)."],
            Some("Viper 1-1 | Bob"),
        );
        let p = build_prompt(&d, "somewhere", None);
        let lp = p.to_lowercase();
        assert!(!lp.contains("bob"), "{p}");
        assert!(!lp.contains("viper"), "{p}");
        assert!(!lp.contains("jtf") && !lp.contains("hammer"), "{p}");
        assert!(p.contains("Gori"));
        // The rules are always there, and the headline is not shouted.
        assert!(p.contains("no text"));
        assert!(!p.contains("DOWNS FOUR"));
    }

    #[test]
    fn story_is_clipped() {
        let long = "word ".repeat(400);
        let d = digest("THE LINE HOLDS", &[&long], None);
        let p = build_prompt(&d, "x", None);
        assert!(p.len() < STORY_CHARS + 1200, "{}", p.len());
    }

    #[test]
    fn style_override_replaces_the_look_not_the_rules() {
        let d = digest("GORI FALLS", &["Gori fell."], None);
        let p = build_prompt(&d, "x", Some("Oil painting."));
        assert!(p.contains("Oil painting.") && !p.contains("35mm"));
        assert!(p.contains("No gore"));
    }

    #[test]
    fn setting_comes_from_the_scenario() {
        let rgw = inst(serde_json::json!({"id": "vs2", "engine_config": "C:\\x\\RGW2008_CFG"}));
        assert!(campaign_setting(&rgw, "Caucasus").contains("2008"));
        let odf = inst(serde_json::json!({"id": "vs1", "engine_config": "C:\\x\\ODFv2_CFG"}));
        assert!(campaign_setting(&odf, "Syria").contains("Syria"));
        let plain = inst(serde_json::json!({"id": "vs3"}));
        assert!(campaign_setting(&plain, "").contains("present-day"));
        let over = inst(serde_json::json!({"id": "vs1", "news_image_setting": "Falklands 1982"}));
        assert_eq!(campaign_setting(&over, "Syria"), "Falklands 1982");
    }

    #[test]
    fn parses_b64_url_and_data_url_replies() {
        let png = b"\x89PNG\r\n\x1a\nrest".to_vec();
        let b64 = base64::engine::general_purpose::STANDARD.encode(&png);
        let r = parse_reply(&serde_json::json!({"data": [{"b64_json": b64}]})).unwrap();
        assert_eq!(r, Returned::Bytes(png.clone()));
        let r = parse_reply(&serde_json::json!({"data": [{"url": "https://cdn.example/x.png"}]})).unwrap();
        assert_eq!(r, Returned::Url("https://cdn.example/x.png".into()));
        let r = parse_reply(&serde_json::json!({"data": [{"url": format!("data:image/png;base64,{b64}")}]}))
            .unwrap();
        assert_eq!(r, Returned::Bytes(png));
        assert!(parse_reply(&serde_json::json!({"data": []})).is_err());
        assert!(parse_reply(&serde_json::json!({"data": [{"url": "file:///etc/passwd"}]})).is_err());
        assert!(parse_reply(&serde_json::json!({"error": {"message": "nope"}})).is_err());
    }

    #[test]
    fn only_real_images_under_the_cap_are_kept() {
        assert_eq!(validate(b"\x89PNG\r\n\x1a\nxxxx").unwrap(), "image/png");
        assert_eq!(validate(&[0xFF, 0xD8, 0xFF, 0xE0]).unwrap(), "image/jpeg");
        assert!(validate(b"<svg onload=alert(1)>").is_err());
        assert!(validate(b"").is_err());
        let mut big = b"\x89PNG\r\n\x1a\n".to_vec();
        big.resize(MAX_IMAGE_BYTES + 1, 0);
        assert!(validate(&big).is_err());
    }

    #[test]
    fn request_body_follows_the_provider() {
        let mut c = cfg(ImageArgs {
            url: Some("https://api.openai.com/v1/images/generations".into()),
            key: Some("k".into()),
            ..Default::default()
        });
        let b = c.request_body("p");
        assert_eq!(b["model"], "gpt-image-1");
        assert_eq!(b["size"], DEFAULT_SIZE);
        assert!(b.get("response_format").is_none() && b.get("quality").is_none());
        c.model = "dall-e-3".into();
        assert_eq!(c.request_body("p")["response_format"], "b64_json");
        c.url = "https://api.together.xyz/v1/images/generations".into();
        c.size = "1024x768".into();
        let b = c.request_body("p");
        assert_eq!((b["width"].as_u64(), b["height"].as_u64()), (Some(1024), Some(768)));
    }

    #[test]
    fn off_unless_named() {
        std::env::remove_var("BFDB_NEWS_IMAGE_URL");
        std::env::remove_var("BFDB_NEWS_IMAGE_KEY");
        std::env::remove_var("BFDB_NEWS_IMAGE_MODEL");
        // (Not setting $OPENAI_API_KEY here to prove it is ignored: tests run
        // in parallel and news_llm's asserts on that variable.)
        assert!(ImageCfg::resolve(ImageArgs::default()).unwrap().is_none());
        let blank = ImageArgs { key: Some(" ".into()), ..Default::default() };
        assert!(ImageCfg::resolve(blank).unwrap().is_none());
        let off = ImageArgs { provider: Some("off".into()), key: Some("k".into()), ..Default::default() };
        assert!(ImageCfg::resolve(off).unwrap().is_none());
    }

    fn cfg(a: ImageArgs) -> ImageCfg {
        ImageCfg::resolve(ImageArgs { per_instance_daily: 10, global_daily: 10, ..a }).unwrap().unwrap()
    }

    #[test]
    fn provider_is_inferred_or_named() {
        // An account id means Cloudflare; the token rides in the key.
        let c = cfg(ImageArgs {
            cf_account_id: Some("acc123".into()),
            key: Some("tok".into()),
            ..Default::default()
        });
        assert_eq!(c.provider, Provider::Cloudflare);
        assert_eq!(
            c.url,
            "https://api.cloudflare.com/client/v4/accounts/acc123/ai/run/@cf/black-forest-labs/flux-1-schnell"
        );
        assert_eq!((c.steps, c.label().as_str()), (6, "cloudflare:@cf/black-forest-labs/flux-1-schnell"));
        // A url or key alone is OpenAI, as before.
        assert_eq!(cfg(ImageArgs { key: Some("k".into()), ..Default::default() }).provider, Provider::OpenAi);
        // Pollinations only when named -- it needs nothing, so it is never guessed.
        let p = cfg(ImageArgs { provider: Some("pollinations".into()), ..Default::default() });
        assert_eq!((p.provider, p.key.as_deref(), p.size.as_str()), (Provider::Pollinations, None, "1024x576"));
        // Named but incomplete, or unknown: a clear error, not a silent guess.
        for bad in [
            ImageArgs { provider: Some("cloudflare".into()), key: Some("t".into()), ..Default::default() },
            ImageArgs { provider: Some("cloudflare".into()), cf_account_id: Some("a".into()), ..Default::default() },
            ImageArgs { provider: Some("openai".into()), ..Default::default() },
            ImageArgs { provider: Some("midjourney".into()), ..Default::default() },
            ImageArgs { key: Some("k".into()), fallback: Some("openai".into()), ..Default::default() },
        ] {
            assert!(ImageCfg::resolve(bad).is_err());
        }
        // Steps are clamped to what FLUX schnell takes; model overrides the path.
        let c = cfg(ImageArgs {
            cf_account_id: Some("a".into()),
            key: Some("t".into()),
            model: Some("@cf/other/model".into()),
            steps: Some(40),
            ..Default::default()
        });
        assert!(c.url.ends_with("/accounts/a/ai/run/@cf/other/model") && c.steps == 8);
    }

    #[test]
    fn fallback_is_keyless_pollinations_only() {
        let c = cfg(ImageArgs {
            cf_account_id: Some("a".into()),
            key: Some("cf-secret".into()),
            fallback: Some("pollinations".into()),
            ..Default::default()
        });
        let fb = c.fallback_cfg().unwrap();
        assert_eq!(fb.provider, Provider::Pollinations);
        assert!(fb.key.is_none(), "the Cloudflare token must never go to Pollinations");
        assert!(fb.fallback_cfg().is_none());
        // Default off; and a provider never falls back to itself.
        let plain = ImageArgs { cf_account_id: Some("a".into()), key: Some("t".into()), ..Default::default() };
        assert!(cfg(plain).fallback_cfg().is_none());
        let p = ImageArgs {
            provider: Some("pollinations".into()),
            fallback: Some("pollinations".into()),
            ..Default::default()
        };
        assert!(cfg(p).fallback.is_none());
    }

    #[test]
    fn cloudflare_request_and_replies() {
        let c = cfg(ImageArgs { cf_account_id: Some("a".into()), key: Some("t".into()), ..Default::default() });
        let b = c.cf_body(&"x".repeat(5000), 42);
        assert_eq!(b["prompt"].as_str().unwrap().len(), CF_PROMPT_MAX);
        assert_eq!((b["steps"].as_u64(), b["seed"].as_u64()), (Some(6), Some(42)));
        assert!(b.get("width").is_none() && b.get("height").is_none());

        let jpg = vec![0xFF, 0xD8, 0xFF, 0xE0, 1, 2, 3];
        let b64 = base64::engine::general_purpose::STANDARD.encode(&jpg);
        let env = serde_json::json!({"result": {"image": b64}, "success": true, "errors": [], "messages": []});
        assert_eq!(parse_cf_reply(&env).unwrap(), jpg);
        assert_eq!(parse_cf_reply(&serde_json::json!({"image": b64})).unwrap(), jpg);
        let bad = serde_json::json!({"result": null, "success": false,
            "errors": [{"code": 3036, "message": "Account limited: daily free allocation of 10,000 neurons used"}]});
        let e = parse_cf_reply(&bad).unwrap_err().to_string();
        assert!(e.contains("3036") && e.contains("neurons"), "{e}");
        assert!(cf_quota(200, &e) && cf_quota(429, "") && !cf_quota(400, "bad prompt"));
        assert!(parse_cf_reply(&serde_json::json!({"success": false})).is_err());
        assert!(parse_cf_reply(&serde_json::json!({"success": true, "result": {}})).is_err());
    }

    #[test]
    fn pollinations_url_is_built_from_the_prompt() {
        // Anonymous: the legacy host, model flux, 16 s apart whatever is asked.
        let c = cfg(ImageArgs {
            provider: Some("pollinations".into()),
            min_interval_secs: Some(1),
            ..Default::default()
        });
        assert_eq!(c.min_interval, Duration::from_secs(16));
        let u = c.pollinations_url("tanks at dawn, no text & no flags?", 7);
        assert!(u.starts_with("https://image.pollinations.ai/prompt/tanks%20at%20dawn%2C%20no%20text%20%26%20no%20flags%3F?"), "{u}");
        for q in ["width=1024", "height=576", "model=flux", "seed=7", "nologo=true", "private=true", "safe=true"] {
            assert!(u.contains(q), "{u} lacks {q}");
        }
        // With a key: the current API, the service's own default model, 3 s
        // apart by default -- and the key is nowhere in the URL.
        let k = cfg(ImageArgs {
            provider: Some("pollinations".into()),
            key: Some("sk_test_not_real".into()),
            ..Default::default()
        });
        assert_eq!((k.min_interval, k.label().as_str()), (Duration::from_secs(3), "pollinations:default"));
        let u = k.pollinations_url("p", 1);
        assert!(u.starts_with("https://gen.pollinations.ai/image/p?width=1024&height=576&seed=1"), "{u}");
        assert!(!u.contains("model=") && !u.contains("sk_") && !u.contains("key="), "{u}");
        let k = cfg(ImageArgs {
            provider: Some("pollinations".into()),
            key: Some("sk_test_not_real".into()),
            model: Some("black-forest-labs/flux.1-schnell".into()),
            min_interval_secs: Some(10),
            ..Default::default()
        });
        assert_eq!(k.min_interval, Duration::from_secs(10));
        assert!(k.pollinations_url("p", 1).contains("&model=black-forest-labs%2Fflux.1-schnell&"));
    }

    #[test]
    fn keys_never_reach_an_error_message() {
        assert_eq!(redact("bad key sk_abc123 given", Some("sk_abc123")), "bad key *** given");
        assert_eq!(redact("nothing", None), "nothing");
        // A provider that echoes the key back in its error body.
        let (url, h) = stub(1, "401 Unauthorized", "application/json",
            br#"{"error":"invalid key sk_test_echoed_back"}"#.to_vec());
        let c = cfg(ImageArgs {
            provider: Some("pollinations".into()),
            url: Some(format!("{url}/image/")),
            key: Some("sk_test_echoed_back".into()),
            ..Default::default()
        });
        let e = generate(&c, "p").unwrap_err();
        assert!(!e.msg.contains("sk_test_echoed_back") && e.msg.contains("401"), "{}", e.msg);
        assert!(!e.rate_limited, "a bad key is not a quota");
        h.join().unwrap();
        // A spent pollen budget is.
        let (url, h) = stub(1, "402 Payment Required", "application/json", b"{}".to_vec());
        let c = cfg(ImageArgs {
            provider: Some("pollinations".into()),
            url: Some(format!("{url}/image/")),
            key: Some("sk_x".into()),
            ..Default::default()
        });
        assert!(generate(&c, "p").unwrap_err().rate_limited);
        h.join().unwrap();
        // Transport errors do not carry the (prompt-bearing) URL either.
        let c = cfg(ImageArgs {
            provider: Some("pollinations".into()),
            url: Some("http://127.0.0.1:9/image/".into()),
            ..Default::default()
        });
        let e = generate(&c, "secret-ish prompt").unwrap_err();
        assert!(!e.msg.contains("prompt") && !e.msg.contains("127.0.0.1"), "{}", e.msg);
    }

    #[test]
    fn pollinations_calls_are_spaced() {
        let min = Duration::from_secs(16);
        let t0 = Instant::now();
        let mut g = Spacing::default();
        assert_eq!(g.reserve(t0, min), Duration::ZERO);
        // Five seconds later the next slot is eleven seconds away...
        assert_eq!(g.reserve(t0 + Duration::from_secs(5), min), Duration::from_secs(11));
        // ...and the one after queues behind it.
        assert_eq!(g.reserve(t0 + Duration::from_secs(5), min), Duration::from_secs(27));
        // Long after, no wait.
        assert_eq!(g.reserve(t0 + Duration::from_secs(100), min), Duration::ZERO);
    }

    /// A one-shot local HTTP server answering `n` requests with a fixed reply,
    /// and handing back each request's head+body -- the providers' stand-in.
    /// Nothing leaves the machine.
    fn stub(n: usize, status: &'static str, ctype: &'static str, body: Vec<u8>)
        -> (String, std::thread::JoinHandle<Vec<String>>) {
        use std::io::Write;
        let l = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let url = format!("http://{}", l.local_addr().unwrap());
        let h = std::thread::spawn(move || {
            let mut seen = vec![];
            for _ in 0..n {
                let (mut s, _) = l.accept().unwrap();
                s.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
                let mut buf = vec![0u8; 64 * 1024];
                let mut got = Vec::new();
                // Head, then as much body as Content-Length says.
                loop {
                    let k = s.read(&mut buf).unwrap_or(0);
                    got.extend_from_slice(&buf[..k]);
                    let text = String::from_utf8_lossy(&got).to_string();
                    if let Some(i) = text.find("

") {
                        let len = text[..i]
                            .lines()
                            .find_map(|l| l.to_ascii_lowercase().strip_prefix("content-length:").map(|v| v.trim().parse::<usize>().unwrap_or(0)))
                            .unwrap_or(0);
                        if got.len() >= i + 4 + len || k == 0 {
                            break;
                        }
                    } else if k == 0 {
                        break;
                    }
                }
                seen.push(String::from_utf8_lossy(&got).to_string());
                let head = format!(
                    "HTTP/1.1 {status}
Content-Type: {ctype}
Content-Length: {}
Connection: close

",
                    body.len()
                );
                let _ = s.write_all(head.as_bytes());
                let _ = s.write_all(&body);
            }
            seen
        });
        (url, h)
    }

    #[test]
    fn cloudflare_end_to_end_against_a_stub() {
        let jpg = vec![0xFF, 0xD8, 0xFF, 0xE0, 9, 9];
        let b64 = base64::engine::general_purpose::STANDARD.encode(&jpg);
        let reply = serde_json::json!({"result": {"image": b64}, "success": true, "errors": []});
        let (url, h) = stub(1, "200 OK", "application/json", reply.to_string().into_bytes());
        let c = cfg(ImageArgs {
            provider: Some("cloudflare".into()),
            url: Some(format!("{url}/client/v4/accounts/a/ai/run/@cf/black-forest-labs/flux-1-schnell")),
            key: Some("cf-token".into()),
            ..Default::default()
        });
        assert_eq!(generate(&c, "a prompt").unwrap(), jpg);
        let req = h.join().unwrap().remove(0);
        assert!(req.starts_with("POST /client/v4/accounts/a/ai/run/@cf/black-forest-labs/flux-1-schnell"));
        assert!(req.to_ascii_lowercase().contains("authorization: bearer cf-token"));
        assert!(req.contains("\"prompt\":\"a prompt\"") && req.contains("\"steps\":6"));
    }

    #[test]
    fn cloudflare_quota_is_not_an_attempt_and_pollinations_takes_over() {
        let quota = serde_json::json!({"success": false, "result": null,
            "errors": [{"code": 3036, "message": "you have used up your daily free allocation of 10,000 neurons"}]});
        let (cf_url, cf_h) = stub(2, "429 Too Many Requests", "application/json", quota.to_string().into_bytes());
        let png = b"\x89PNG\r\n\x1a\nfake-png-body".to_vec();
        let (pl_url, pl_h) = stub(1, "200 OK", "image/png", png.clone());
        let mut c = cfg(ImageArgs {
            provider: Some("cloudflare".into()),
            url: Some(format!("{cf_url}/run")),
            key: Some("cf-token".into()),
            ..Default::default()
        });
        // No fallback: a quota error, flagged as such.
        let e = generate_with_fallback(&c, "p").unwrap_err();
        assert!(e.rate_limited && e.msg.contains("neurons"), "{}", e.msg);
        // With the fallback: Pollinations draws it, keyless.
        c.fallback = Some(Provider::Pollinations);
        c.fallback_url = format!("{pl_url}/prompt/");
        let (bytes, by) = generate_with_fallback(&c, "tanks at dawn").unwrap();
        assert_eq!((bytes, by.as_str()), (png, "pollinations:flux"));
        cf_h.join().unwrap();
        let req = pl_h.join().unwrap().remove(0);
        assert!(req.starts_with("GET /prompt/tanks%20at%20dawn?width=1024&height=576&model=flux"), "{req}");
        assert!(!req.to_ascii_lowercase().contains("authorization"), "{req}");
    }

    #[test]
    fn pollinations_raw_bytes_are_validated() {
        let (url, h) = stub(1, "200 OK", "text/html", b"<html>busy</html>".to_vec());
        let c = cfg(ImageArgs {
            provider: Some("pollinations".into()),
            url: Some(format!("{url}/prompt/")),
            key: Some("pl-token".into()),
            ..Default::default()
        });
        let e = generate(&c, "p").unwrap_err();
        assert!(e.billed && !e.rate_limited && e.msg.contains("not a PNG"), "{}", e.msg);
        // Its own account token, when it is the provider, is sent.
        assert!(h.join().unwrap()[0].to_ascii_lowercase().contains("authorization: bearer pl-token"));
    }

    #[test]
    fn eligibility_waits_for_the_filed_written_day() {
        let now = Utc::now();
        let mut d = digest("X", &[], None);
        d.final_ = false;
        assert!(!eligible(&d, false, now));
        d.final_ = true;
        assert!(eligible(&d, false, now));
        // Writer on, template prose: wait for the rewrite, but not forever.
        d.generated = now;
        assert!(!eligible(&d, true, now));
        d.generated = now - ChronoDuration::hours(7);
        assert!(eligible(&d, true, now));
        d.body = vec!["written".into()];
        d.generated = now;
        assert!(eligible(&d, true, now));
    }

    #[test]
    fn day_ids_are_checked() {
        assert!(valid_day("2026-09-27"));
        assert!(!valid_day("2026-9-27"));
        assert!(!valid_day("../../etc/x"));
        assert!(!valid_day("2026-09-27x"));
    }

    fn temp_db() -> StatsDb {
        let dir = std::env::temp_dir().join(format!("bfdb-newsimg-{}", uuid::Uuid::new_v4()));
        let reg = crate::instance::Registry::single(inst(serde_json::json!({"id": "vs1"})));
        StatsDb::new(&std::collections::HashMap::new(), dir, reg, None, None).unwrap()
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn caps_are_per_instance_and_global() {
        let db = temp_db();
        let cfg = ImageCfg::resolve(ImageArgs {
            url: Some("http://127.0.0.1:9/x".into()),
            per_instance_daily: 2,
            global_daily: 3,
            ..Default::default()
        })
        .unwrap()
        .unwrap();
        let day = "2026-09-28";
        assert!(under_caps(&db, &cfg, "vs1", day).unwrap().is_none());
        count_call(&db, "vs1", day).unwrap();
        count_call(&db, "vs1", day).unwrap();
        assert!(under_caps(&db, &cfg, "vs1", day).unwrap().unwrap().contains("this instance"));
        // vs2 has its own allowance but shares the global one (2 of 3 used).
        assert!(under_caps(&db, &cfg, "vs2", day).unwrap().is_none());
        count_call(&db, "vs2", day).unwrap();
        assert!(under_caps(&db, &cfg, "vs2", day).unwrap().unwrap().contains("global"));
        // A new day starts fresh.
        assert!(under_caps(&db, &cfg, "vs1", "2026-09-29").unwrap().is_none());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn stored_images_round_trip_and_feed_the_api_fields() {
        let db = temp_db();
        let rid = RoundId(7);
        let mut d = digest("X", &["y"], None);
        d.day = Utc::now().format("%Y-%m-%d").to_string();
        let (url, pending) = api_fields(&db, "vs1", rid, &d, true).unwrap();
        assert!(url.is_none() && pending);
        let (_, pending) = api_fields(&db, "vs1", rid, &d, false).unwrap();
        assert!(!pending);
        let meta = ImageMeta { has_image: true, version: 2, ..Default::default() };
        db.news_image_put("vs1", rid, &d.day, b"\x89PNG\r\n\x1a\n", &meta).unwrap();
        let (url, pending) = api_fields(&db, "vs1", rid, &d, true).unwrap();
        assert_eq!(url.unwrap(), format!("/api/news/image/{}?instance=vs1&v=2", d.day));
        assert!(!pending);
        assert_eq!(db.news_image_get("vs1", rid, &d.day).unwrap().unwrap(), b"\x89PNG\r\n\x1a\n");
        assert!(db.news_image_get("vs2", rid, &d.day).unwrap().is_none());
    }

    /// A failing endpoint is tried `MAX_ATTEMPTS` times at most, and never
    /// again by the generator after that. Uses a closed local port -- nothing
    /// leaves the machine.
    #[tokio::test(flavor = "multi_thread")]
    async fn failures_back_off_then_give_up() {
        let db = temp_db();
        let cfg = cfg(ImageArgs { url: Some("http://127.0.0.1:9/v1/images".into()), ..Default::default() });
        let ic = inst(serde_json::json!({"id": "vs1"}));
        let rid = RoundId(3);
        let d = digest("X", &["y"], None);
        // The real callers run this off the runtime (block_in_place /
        // spawn_blocking); the blocking HTTP client insists on the same.
        let run = |forced| tokio::task::block_in_place(|| illustrate(&db, &ic, rid, &d, &cfg, forced).unwrap());
        for n in 1..=MAX_ATTEMPTS {
            assert!(matches!(run(false), Outcome::Failed(_)));
            let m = db.news_image_meta("vs1", rid, &d.day).unwrap().unwrap();
            assert_eq!(m.attempts, n);
            assert!(m.next_try.is_some());
            assert_eq!(m.gave_up, n == MAX_ATTEMPTS);
        }
        // A connection failure is not billed, so it did not eat the caps.
        assert_eq!(db.news_image_count("vs1", &Utc::now().format("%Y-%m-%d").to_string()).unwrap(), 0);
        // A forced retry clears the give-up (and fails again here).
        run(true);
        let m = db.news_image_meta("vs1", rid, &d.day).unwrap().unwrap();
        assert_eq!((m.attempts, m.gave_up), (1, false));
    }
}
