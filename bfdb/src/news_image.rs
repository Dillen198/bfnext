//! A picture for each day of the war diary.
//!
//! `news.rs` decides what happened and `news_llm.rs` writes it up; this module
//! asks an image model for one "wire photo" to run beside the finished
//! dispatch. It is decoration, so everything about it is conservative:
//!
//!  * **Off unless configured.** No `--news-image-url` / key, no calls.
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
//! Any OpenAI-compatible `images/generations` endpoint works: OpenAI
//! (`gpt-image-1`, `dall-e-3`), Together (`black-forest-labs/FLUX.1-schnell`),
//! and most self-hosted gateways. Both reply forms are handled -- inline
//! `b64_json`, and a `url`, which is downloaded at once because those links
//! expire within the hour.

use crate::{
    db::{RoundId, StatsDb},
    instance::InstanceCfg,
    news::NewsDigest,
};
use anyhow::{anyhow, bail, Result};
use base64::Engine;
use chrono::{DateTime, Duration as ChronoDuration, Utc};
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, io::Read, sync::Mutex, time::Duration};

/// Default endpoint when only a key is given.
const DEFAULT_URL: &str = "https://api.openai.com/v1/images/generations";
const DEFAULT_MODEL: &str = "gpt-image-1";
/// Square is the one size every provider and model accepts (DALL-E 2/3,
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

#[derive(Debug, Clone)]
pub struct ImageCfg {
    pub url: String,
    pub key: Option<String>,
    pub model: String,
    pub size: String,
    /// Passed through when set (`gpt-image-1`: low / medium / high -- the
    /// main cost knob there). Omitted otherwise, as other models reject it.
    pub quality: Option<String>,
    /// Replaces `DEFAULT_STYLE` for every instance without its own.
    pub style: Option<String>,
    pub per_instance_daily: u32,
    pub global_daily: u32,
}

impl ImageCfg {
    /// Resolve from the CLI flags, falling back to the environment. `None`
    /// (feature off) unless a URL or a key is configured. Deliberately does NOT
    /// fall back to `$OPENAI_API_KEY` the way the writer does: images cost real
    /// money per call, so they are turned on by name only.
    #[allow(clippy::too_many_arguments)]
    pub fn resolve(
        url: Option<String>,
        key: Option<String>,
        model: Option<String>,
        size: Option<String>,
        quality: Option<String>,
        style: Option<String>,
        per_instance_daily: u32,
        global_daily: u32,
    ) -> Option<Self> {
        let env = |k: &str| std::env::var(k).ok().filter(|v| !v.trim().is_empty());
        let nonempty = |v: Option<String>| v.map(|s| s.trim().to_string()).filter(|s| !s.is_empty());
        let url = nonempty(url).or_else(|| env("BFDB_NEWS_IMAGE_URL"));
        let key = nonempty(key).or_else(|| env("BFDB_NEWS_IMAGE_KEY"));
        if url.is_none() && key.is_none() {
            return None;
        }
        Some(Self {
            url: url.unwrap_or_else(|| DEFAULT_URL.to_string()),
            key,
            model: nonempty(model)
                .or_else(|| env("BFDB_NEWS_IMAGE_MODEL"))
                .unwrap_or_else(|| DEFAULT_MODEL.to_string()),
            size: nonempty(size).unwrap_or_else(|| DEFAULT_SIZE.to_string()),
            quality: nonempty(quality),
            style: nonempty(style),
            per_instance_daily,
            global_daily,
        })
    }

    fn is_together(&self) -> bool {
        self.url.contains("together")
    }

    /// The JSON body for one image. The common subset (`model`, `prompt`,
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
}

fn parse_size(s: &str) -> Option<(u32, u32)> {
    let (w, h) = s.split_once(['x', 'X'])?;
    Some((w.trim().parse().ok()?, h.trim().parse().ok()?))
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

/// Read `data[0]` of an images reply: inline base64 (`b64_json`, also a
/// `data:` URL some gateways put in `url`) or a link to download.
fn parse_reply(v: &serde_json::Value) -> Result<Returned> {
    let first = v
        .get("data")
        .and_then(|d| d.as_array())
        .and_then(|a| a.first())
        .ok_or_else(|| anyhow!("no data[] in reply"))?;
    let decode = |b64: &str| -> Result<Vec<u8>> {
        Ok(base64::engine::general_purpose::STANDARD.decode(b64.trim())?)
    };
    if let Some(b) = first.get("b64_json").and_then(|b| b.as_str()).filter(|b| !b.is_empty()) {
        return Ok(Returned::Bytes(decode(b)?));
    }
    if let Some(u) = first.get("url").and_then(|u| u.as_str()).filter(|u| !u.is_empty()) {
        if let Some(rest) = u.strip_prefix("data:") {
            let (_, b64) = rest.split_once(',').ok_or_else(|| anyhow!("malformed data: URL"))?;
            return Ok(Returned::Bytes(decode(b64)?));
        }
        if !(u.starts_with("https://") || u.starts_with("http://")) {
            bail!("reply url is not http(s)");
        }
        return Ok(Returned::Url(u.to_string()));
    }
    bail!("reply has neither b64_json nor url")
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
    /// A 429: the quota, not this dispatch. Not counted as an attempt.
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

/// Make one image. Blocking: the callers are already off the async runtime.
fn generate(cfg: &ImageCfg, prompt: &str) -> std::result::Result<Vec<u8>, GenError> {
    let fail = |msg: String, billed: bool| GenError { msg, billed, rate_limited: false };
    let client = reqwest::blocking::Client::builder()
        .timeout(CALL_TIMEOUT)
        .build()
        .map_err(|e| fail(e.to_string(), false))?;
    let mut req = client.post(&cfg.url).json(&cfg.request_body(prompt));
    if let Some(k) = &cfg.key {
        req = req.bearer_auth(k);
    }
    let res = req.send().map_err(|e| fail(format!("request failed: {e}"), false))?;
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
        res.json().map_err(|e| fail(format!("unreadable reply: {e}"), true))?;
    let bytes = match parse_reply(&v).map_err(|e| fail(e.to_string(), true))? {
        Returned::Bytes(b) => b,
        Returned::Url(u) => download(&client, &u).map_err(|e| fail(e.to_string(), true))?,
    };
    validate(&bytes).map_err(|e| fail(e.to_string(), true))?;
    Ok(bytes)
}

/// Fetch a returned image URL at once (they expire), never more than the cap.
fn download(client: &reqwest::blocking::Client, url: &str) -> Result<Vec<u8>> {
    let res = client.get(url).send()?;
    if !res.status().is_success() {
        bail!("image download: {}", res.status());
    }
    if res.content_length().map(|n| n as usize > MAX_IMAGE_BYTES).unwrap_or(false) {
        bail!("image download is over the {MAX_IMAGE_BYTES} byte cap");
    }
    let mut buf = Vec::new();
    res.take(MAX_IMAGE_BYTES as u64 + 1).read_to_end(&mut buf)?;
    Ok(buf)
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
    match generate(cfg, &prompt) {
        Ok(bytes) => {
            count_call(db, &inst.id, &today)?;
            meta.has_image = true;
            meta.version = meta.version.saturating_add(1);
            meta.created = Some(now);
            meta.model = cfg.model.clone();
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
        let mut c = ImageCfg::resolve(
            Some("https://api.openai.com/v1/images/generations".into()),
            Some("k".into()),
            None,
            None,
            None,
            None,
            3,
            8,
        )
        .unwrap();
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
        assert!(ImageCfg::resolve(None, None, None, None, None, None, 3, 8).is_none());
        assert!(ImageCfg::resolve(None, Some(" ".into()), None, None, None, None, 3, 8).is_none());
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
        let cfg = ImageCfg::resolve(Some("http://127.0.0.1:9/x".into()), None, None, None, None, None, 2, 3)
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
        let cfg = ImageCfg::resolve(Some("http://127.0.0.1:9/v1/images".into()), None, None, None, None, None, 10, 10)
            .unwrap();
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
