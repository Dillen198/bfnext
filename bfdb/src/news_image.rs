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
    news::{Factions, NewsDigest},
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
/// Cap on the story text that goes into a prompt. Short on purpose: the
/// planned scene is the subject, and a paragraph that mentions tanks,
/// helicopters and a town gets all three drawn into every frame.
const STORY_CHARS: usize = 240;
/// Cap on a whole prompt. Under Workers AI's 2048 (which truncates from the
/// end, where the rules are) and a comfortable Pollinations URL once encoded.
const PROMPT_MAX: usize = 1900;
/// Caps on operator-written setting / style text.
const SETTING_CHARS: usize = 300;
const STYLE_CHARS: usize = 400;

/// How the frame is composed, whatever the style. The old single framing --
/// a soldier's back in the foreground watching parked vehicles -- came from
/// "seen from behind" and "cinematic composition"; it is named here so the
/// model steers away from it.
const COMPOSITION: &str = "One clear subject, framed the way a news photographer on the \
scene would frame it; any people are small figures far away. No soldier in the \
foreground, no over-the-shoulder view, no vehicles parked in a row, no posed line-up.";

/// Always appended. The model is told, not trusted, so these are repeated in
/// the plainest words.
const RULES: &str = "The image must contain no text, lettering, captions, numbers, \
signs or watermarks. No identifiable real people, politicians or public figures; \
no faces toward the camera. No national flags, insignia close-ups or real military \
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
    /// Replaces the default photographic look for every instance without
    /// its own.
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

// ── the setting ──────────────────────────────────────────────────────────────

/// Which period's equipment is correct in a picture.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Era {
    /// The Russo-Georgian war, August 2008.
    Georgia2008,
    /// Any present-day war.
    Modern,
    /// 1944.
    Ww2,
    /// An operator-written setting: no period kit is assumed, only what the
    /// story itself names.
    Custom,
}

/// What the ground looks like, by the kind of place a scene needs. Each list
/// is a handful of real-looking cues for one theatre; the planner picks one.
#[derive(Debug)]
pub struct Land {
    towns: &'static [&'static str],
    roads: &'static [&'static str],
    open: &'static [&'static str],
    airfields: &'static [&'static str],
    coast: &'static [&'static str],
    weather: &'static [&'static str],
}

const LAND_GEORGIA: Land = Land {
    towns: &[
        "a Georgian country town of stone houses with rusted tin roofs and walnut trees",
        "a Soviet-era town of five-storey concrete apartment blocks",
        "a hillside village with an old stone church tower",
        "the edge of a small Georgian town, low houses behind vine-covered fences",
    ],
    roads: &[
        "a two-lane road lined with poplar trees through farmland",
        "a winding mountain road above a river gorge",
        "a straight highway across a green valley floor",
        "a dirt track between maize fields",
    ],
    open: &[
        "green farmland and maize fields in a wide valley",
        "wooded foothills below the Greater Caucasus",
        "a grassy ridge above a river valley",
        "orchards and hayfields at the foot of the mountains",
    ],
    airfields: &[
        "a Soviet-built military airfield with concrete aircraft shelters",
        "a long concrete runway in a green valley with mountains behind",
    ],
    coast: &["the Black Sea coast with green hills behind", "a Black Sea harbour with cranes"],
    weather: &[
        "hazy summer air",
        "towering afternoon thunderclouds over the mountains",
        "morning mist lying in the valley",
        "clear mountain air",
        "smoke drifting from grass fires",
        "wet asphalt after a summer rainstorm",
    ],
};

const LAND_LEVANT: Land = Land {
    towns: &[
        "a town of flat-roofed concrete houses and a minaret",
        "a dusty market town of half-built breeze-block houses",
        "the outskirts of a hillside town above olive groves",
        "a stone village on a terraced hillside",
    ],
    roads: &[
        "a straight desert highway with power lines alongside",
        "a narrow road through olive groves and stone walls",
        "a dusty road across a flat plain",
        "a mountain road through dry rocky hills",
    ],
    open: &[
        "dry rocky hills dotted with olive trees",
        "a flat, sun-baked plain of dry fields",
        "black basalt plateau country",
        "terraced hillsides with mountains in haze behind",
    ],
    airfields: &[
        "a desert airbase with hardened aircraft shelters and sand-coloured taxiways",
        "a long runway on a dusty plain",
    ],
    coast: &["the eastern Mediterranean coast with a port town behind", "a rocky Mediterranean shoreline"],
    weather: &[
        "heat shimmer",
        "a dust haze turning the sky pale",
        "a hard clear sky",
        "high thin cloud",
        "drifting smoke",
        "a winter overcast with low grey cloud",
    ],
};

const LAND_DESERT: Land = Land {
    towns: &[
        "a low town of mud-brick and concrete houses",
        "a desert town of flat-roofed houses and palm trees",
        "a roadside settlement of cinder-block buildings",
    ],
    roads: &[
        "a straight desert highway",
        "a dusty track across open desert",
        "a road through a rocky wadi",
    ],
    open: &["open desert with low rocky ridges", "a gravel plain under bare mountains", "sand dunes and scrub"],
    airfields: &[
        "a desert airbase with hardened aircraft shelters",
        "a long runway on a sun-baked plain",
    ],
    coast: &["a flat desert coastline with a port behind", "a hazy shoreline with oil terminals"],
    weather: &["heat shimmer", "a dust haze", "a hard clear sky", "a sandstorm building on the horizon", "drifting smoke"],
};

const LAND_NORMANDY: Land = Land {
    towns: &[
        "a Norman stone village with a church steeple",
        "a small market town of grey stone houses",
    ],
    roads: &["a sunken lane between high hedgerows", "a straight road lined with plane trees"],
    open: &["bocage country of small fields and hedgerows", "apple orchards and pasture"],
    airfields: &["a temporary airstrip of steel matting laid in a field"],
    coast: &["a wide beach with bluffs behind", "a small Channel harbour"],
    weather: &["low grey cloud", "summer haze", "drizzle", "broken cloud and bright sun", "drifting smoke"],
};

const LAND_TEMPERATE: Land = Land {
    towns: &["a small town of low houses", "the edge of a small town", "a village of pitched-roof houses"],
    roads: &["a two-lane country road", "a straight road across open country", "a forest road"],
    open: &["open farmland", "rolling wooded hills", "a wide river valley"],
    airfields: &["a military airfield with hardened aircraft shelters"],
    coast: &["a rocky coastline", "a grey harbour"],
    weather: &["overcast", "haze", "a clear sky", "drifting smoke", "light rain"],
};

/// Where and when the war is, for the pictures.
#[derive(Debug, Clone)]
pub struct Setting {
    /// Place, period and landscape in words an image model can draw. No
    /// equipment: a list of hardware here ends up in every frame.
    pub text: String,
    pub era: Era,
    pub land: &'static Land,
}

fn land_for(theatre: &str, rgw: bool) -> &'static Land {
    if rgw {
        return &LAND_GEORGIA;
    }
    match theatre {
        "Caucasus" => &LAND_GEORGIA,
        "Syria" => &LAND_LEVANT,
        "Persian Gulf" | "Iraq" | "Sinai" | "Afghanistan" | "Nevada" => &LAND_DESERT,
        "Normandy" => &LAND_NORMANDY,
        _ => &LAND_TEMPERATE,
    }
}

/// Where and when the war is, in words an image model can draw. Per instance:
/// an explicit `news_image_setting` wins (and then no period equipment is
/// assumed); then the scenario's own name (the 2008 Caucasus campaign is
/// RGW2008), then the theatre the objectives sit in.
pub fn campaign_setting(cfg: &InstanceCfg, theatre: &str) -> Setting {
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
    let rgw = hay.contains("rgw") || hay.contains("2008");
    let land = land_for(theatre, rgw);
    if let Some(s) = cfg.news_image_setting.as_deref().map(str::trim).filter(|s| !s.is_empty()) {
        return Setting { text: clip(s, SETTING_CHARS), era: Era::Custom, land };
    }
    let (text, era) = if rgw {
        (
            "Georgia in August 2008, the Russo-Georgian war: green valleys, maize fields \
             and stone villages below the Caucasus mountains"
                .to_string(),
            Era::Georgia2008,
        )
    } else {
        match theatre {
            "Syria" => (
                "a present-day war in Syria and the Levant: dry hills, olive groves, desert \
                 airbases and flat-roofed towns of the eastern Mediterranean"
                    .to_string(),
                Era::Modern,
            ),
            "Caucasus" => (
                "a present-day war in the Caucasus: mountain valleys and the Black Sea coast \
                 of Georgia"
                    .to_string(),
                Era::Modern,
            ),
            "Normandy" => ("Normandy in 1944: hedgerow country and stone villages".to_string(), Era::Ww2),
            "" => ("a present-day war".to_string(), Era::Modern),
            t => (format!("a present-day war in the {t} region"), Era::Modern),
        }
    };
    Setting { text, era, land }
}

// ── the equipment ────────────────────────────────────────────────────────────

/// A job a vehicle does in a picture.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Role {
    Tank,
    Ifv,
    Truck,
    /// Strike / attack jet.
    Jet,
    Fighter,
    /// Transport helicopter.
    Thelo,
    /// Attack helicopter.
    Ahelo,
    Sam,
    Arty,
    Ship,
}

impl Role {
    fn parse(s: &str) -> Option<Self> {
        Some(match s {
            "tank" => Self::Tank,
            "ifv" => Self::Ifv,
            "truck" => Self::Truck,
            "jet" => Self::Jet,
            "fighter" => Self::Fighter,
            "thelo" => Self::Thelo,
            "ahelo" => Self::Ahelo,
            "sam" => Self::Sam,
            "arty" => Self::Arty,
            "ship" => Self::Ship,
            _ => return None,
        })
    }
}

/// One side's period-correct kit, by role. Every entry ends in a countable
/// noun, so a plural is the entry plus "s".
struct Kit {
    tank: &'static [&'static str],
    ifv: &'static [&'static str],
    truck: &'static [&'static str],
    jet: &'static [&'static str],
    fighter: &'static [&'static str],
    thelo: &'static [&'static str],
    ahelo: &'static [&'static str],
    sam: &'static [&'static str],
    arty: &'static [&'static str],
    ship: &'static [&'static str],
}

impl Kit {
    fn get(&self, r: Role) -> &'static [&'static str] {
        match r {
            Role::Tank => self.tank,
            Role::Ifv => self.ifv,
            Role::Truck => self.truck,
            Role::Jet => self.jet,
            Role::Fighter => self.fighter,
            Role::Thelo => self.thelo,
            Role::Ahelo => self.ahelo,
            Role::Sam => self.sam,
            Role::Arty => self.arty,
            Role::Ship => self.ship,
        }
    }
}

const KIT_GEO_RU: Kit = Kit {
    tank: &["T-72B main battle tank", "T-62M tank"],
    ifv: &["BMP-2 infantry fighting vehicle", "BTR-80 armoured personnel carrier", "BMD-2 airborne fighting vehicle"],
    truck: &["Ural-4320 army truck", "KamAZ army truck"],
    jet: &["Su-25 attack jet", "Su-24 strike bomber"],
    fighter: &["Su-27 fighter", "MiG-29 fighter"],
    thelo: &["Mi-8 transport helicopter"],
    ahelo: &["Mi-24 helicopter gunship"],
    sam: &["Buk (SA-11) missile launcher", "Tor (SA-15) missile vehicle", "Osa (SA-8) missile vehicle"],
    arty: &["BM-21 Grad rocket launcher", "2S3 Akatsiya self-propelled howitzer", "2S1 Gvozdika self-propelled howitzer"],
    ship: &["Black Sea Fleet missile boat", "Russian navy patrol ship"],
};

const KIT_GEO_GE: Kit = Kit {
    tank: &["T-72 main battle tank"],
    ifv: &["BMP-2 infantry fighting vehicle", "BMP-1 infantry fighting vehicle", "BTR-80 armoured personnel carrier"],
    truck: &["KrAZ army truck", "Ural-4320 army truck"],
    jet: &["Su-25 attack jet"],
    fighter: &["Su-25 attack jet"],
    thelo: &["Mi-8 transport helicopter", "UH-1H Huey helicopter"],
    ahelo: &["Mi-24 helicopter gunship"],
    sam: &["Buk (SA-11) missile launcher", "Osa (SA-8) missile vehicle"],
    arty: &["BM-21 Grad rocket launcher", "DANA self-propelled howitzer", "2S7 Pion self-propelled gun"],
    ship: &["coastguard patrol boat"],
};

const KIT_WEST: Kit = Kit {
    tank: &["M1A2 Abrams tank", "Leopard 2A4 tank"],
    ifv: &["M2 Bradley fighting vehicle", "Stryker armoured vehicle", "M113 armoured personnel carrier"],
    truck: &["HEMTT army truck", "M939 army truck"],
    jet: &["A-10C attack jet", "F-16C fighter-bomber", "F/A-18C Hornet", "F-15E Strike Eagle"],
    fighter: &["F-15C Eagle", "F-16C fighter"],
    thelo: &["UH-60 Black Hawk helicopter", "CH-47 Chinook helicopter"],
    ahelo: &["AH-64D Apache helicopter", "AH-1W Cobra helicopter"],
    sam: &["Patriot missile launcher", "NASAMS missile launcher", "Hawk missile launcher"],
    arty: &["M109 self-propelled howitzer", "M142 HIMARS rocket launcher", "M270 MLRS rocket launcher"],
    ship: &["Arleigh Burke-class destroyer", "Oliver Hazard Perry-class frigate"],
};

const KIT_EAST: Kit = Kit {
    tank: &["T-72B3 tank", "T-90A tank", "T-55 tank"],
    ifv: &["BMP-2 infantry fighting vehicle", "BTR-82A armoured personnel carrier", "BMP-3 infantry fighting vehicle"],
    truck: &["KamAZ army truck", "Ural-4320 army truck"],
    jet: &["Su-25 attack jet", "Su-24 strike bomber", "Su-34 strike fighter"],
    fighter: &["MiG-29 fighter", "Su-27 fighter", "Su-30 fighter"],
    thelo: &["Mi-8 transport helicopter"],
    ahelo: &["Mi-24 helicopter gunship", "Ka-52 attack helicopter", "Mi-28 attack helicopter"],
    sam: &["Pantsir-S1 air-defence vehicle", "S-300 missile launcher", "Buk-M2 missile launcher", "Kub (SA-6) missile launcher"],
    arty: &["BM-21 Grad rocket launcher", "2S19 Msta self-propelled howitzer", "D-30 towed howitzer"],
    ship: &["missile corvette", "Russian navy frigate"],
};

const KIT_WW2_ALLIED: Kit = Kit {
    tank: &["M4 Sherman tank", "Cromwell tank"],
    ifv: &["M3 half-track", "Universal Carrier"],
    truck: &["GMC CCKW army truck", "Bedford army truck"],
    jet: &["P-47 Thunderbolt fighter-bomber", "Typhoon fighter-bomber"],
    fighter: &["Spitfire fighter", "P-51 Mustang fighter"],
    thelo: &["C-47 Dakota transport plane"],
    ahelo: &["P-47 Thunderbolt fighter-bomber"],
    sam: &["Bofors 40mm anti-aircraft gun"],
    arty: &["M7 Priest self-propelled howitzer", "25-pounder field gun"],
    ship: &["destroyer", "landing craft"],
};

const KIT_WW2_AXIS: Kit = Kit {
    tank: &["Panzer IV tank", "Panther tank", "Tiger tank"],
    ifv: &["Sd.Kfz. 251 half-track"],
    truck: &["Opel Blitz army truck"],
    jet: &["Fw 190 fighter-bomber"],
    fighter: &["Bf 109 fighter", "Fw 190 fighter"],
    thelo: &["Ju 52 transport plane"],
    ahelo: &["Fw 190 fighter-bomber"],
    sam: &["88mm Flak gun", "Flak 38 anti-aircraft gun"],
    arty: &["Nebelwerfer rocket launcher", "Wespe self-propelled howitzer"],
    ship: &["E-boat"],
};

const KIT_GENERIC: Kit = Kit {
    tank: &["main battle tank"],
    ifv: &["armoured personnel carrier", "infantry fighting vehicle"],
    truck: &["military truck"],
    jet: &["ground-attack jet"],
    fighter: &["fighter jet"],
    thelo: &["transport helicopter"],
    ahelo: &["attack helicopter"],
    sam: &["surface-to-air missile launcher"],
    arty: &["self-propelled howitzer", "multiple rocket launcher"],
    ship: &["warship"],
};

/// The kit one side fields. The faction's own name wins where it says who
/// the side is; otherwise Blue and Red take the scenario's usual roles.
fn kit_for(era: Era, side: &str, f: &Factions) -> &'static Kit {
    let mut who = format!("{} {}", f.name(side), f.adj(side)).to_lowercase();
    let members = if side == "Blue" { &f.blue_members } else { &f.red_members };
    for m in members {
        who.push(' ');
        who.push_str(&m.to_lowercase());
    }
    let any = |ws: &[&str]| ws.iter().any(|w| who.contains(w));
    let blue = side == "Blue";
    match era {
        Era::Georgia2008 => {
            if any(&["russia"]) {
                &KIT_GEO_RU
            } else if any(&["georgia"]) || blue {
                &KIT_GEO_GE
            } else {
                &KIT_GEO_RU
            }
        }
        Era::Modern => {
            if any(&["russia", "syria", "iran", "soviet"]) {
                &KIT_EAST
            } else if any(&["nato", "coalition", "united states", "america", "israel", "turkey", "jordan"]) || blue {
                &KIT_WEST
            } else {
                &KIT_EAST
            }
        }
        Era::Ww2 => {
            if any(&["german", "axis"]) || !blue {
                &KIT_WW2_AXIS
            } else {
                &KIT_WW2_ALLIED
            }
        }
        Era::Custom => &KIT_GENERIC,
    }
}

/// DCS type names (and the ways a dispatch writes them) to the real-world
/// name an image model knows. More specific patterns first: the first match
/// in a span wins.
const TYPE_NAMES: &[(&[&str], &str, Role)] = &[
    (&["t-72b"], "T-72B main battle tank", Role::Tank),
    (&["t-72"], "T-72 main battle tank", Role::Tank),
    (&["t-80"], "T-80 tank", Role::Tank),
    (&["t-90"], "T-90 tank", Role::Tank),
    (&["t-62"], "T-62 tank", Role::Tank),
    (&["t-55"], "T-55 tank", Role::Tank),
    (&["m1a2", "m-1 abrams", "abrams"], "M1A2 Abrams tank", Role::Tank),
    (&["leopard"], "Leopard 2 tank", Role::Tank),
    (&["merkava"], "Merkava tank", Role::Tank),
    (&["bmp-1"], "BMP-1 infantry fighting vehicle", Role::Ifv),
    (&["bmp-2"], "BMP-2 infantry fighting vehicle", Role::Ifv),
    (&["bmp-3"], "BMP-3 infantry fighting vehicle", Role::Ifv),
    (&["bmd"], "BMD airborne fighting vehicle", Role::Ifv),
    (&["btr-80", "btr-82"], "BTR-80 armoured personnel carrier", Role::Ifv),
    (&["mtlb", "mt-lb"], "MT-LB armoured tractor", Role::Ifv),
    (&["bradley", "m-2 bradley"], "M2 Bradley fighting vehicle", Role::Ifv),
    (&["m-113", "m113"], "M113 armoured personnel carrier", Role::Ifv),
    (&["stryker", "m1126"], "Stryker armoured vehicle", Role::Ifv),
    (&["lav-25"], "LAV-25 armoured vehicle", Role::Ifv),
    (&["ural-4320", "ural"], "Ural-4320 army truck", Role::Truck),
    (&["kamaz"], "KamAZ army truck", Role::Truck),
    (&["hemtt"], "HEMTT army truck", Role::Truck),
    (&["m939"], "M939 army truck", Role::Truck),
    (&["su-25"], "Su-25 attack jet", Role::Jet),
    (&["su-24"], "Su-24 strike bomber", Role::Jet),
    (&["su-34"], "Su-34 strike fighter", Role::Jet),
    (&["su-17", "su-22"], "Su-22 fighter-bomber", Role::Jet),
    (&["a-10"], "A-10 attack jet", Role::Jet),
    (&["f-15e"], "F-15E Strike Eagle", Role::Jet),
    (&["fa-18", "f/a-18", "hornet"], "F/A-18C Hornet", Role::Jet),
    (&["av8bna", "av-8b", "harrier"], "AV-8B Harrier jump jet", Role::Jet),
    (&["tornado"], "Tornado strike jet", Role::Jet),
    (&["tu-22"], "Tu-22M3 bomber", Role::Jet),
    (&["jf-17"], "JF-17 fighter", Role::Fighter),
    (&["su-27", "su-33"], "Su-27 fighter", Role::Fighter),
    (&["su-30"], "Su-30 fighter", Role::Fighter),
    (&["mig-21"], "MiG-21 fighter", Role::Fighter),
    (&["mig-23"], "MiG-23 fighter", Role::Fighter),
    (&["mig-29"], "MiG-29 fighter", Role::Fighter),
    (&["mig-31"], "MiG-31 interceptor", Role::Fighter),
    (&["f-16"], "F-16C fighter", Role::Fighter),
    (&["f-15"], "F-15C Eagle", Role::Fighter),
    (&["f-14"], "F-14 Tomcat", Role::Fighter),
    (&["f-4"], "F-4 Phantom", Role::Fighter),
    (&["m-2000", "mirage"], "Mirage 2000 fighter", Role::Fighter),
    (&["mi-8", "mi-17"], "Mi-8 transport helicopter", Role::Thelo),
    (&["uh-60", "black hawk"], "UH-60 Black Hawk helicopter", Role::Thelo),
    (&["uh-1", "huey"], "UH-1 Huey helicopter", Role::Thelo),
    (&["ch-47", "chinook"], "CH-47 Chinook helicopter", Role::Thelo),
    (&["sa342", "gazelle"], "Gazelle light helicopter", Role::Thelo),
    (&["mi-24", "hind"], "Mi-24 helicopter gunship", Role::Ahelo),
    (&["ka-50"], "Ka-50 attack helicopter", Role::Ahelo),
    (&["ka-52"], "Ka-52 attack helicopter", Role::Ahelo),
    (&["mi-28"], "Mi-28 attack helicopter", Role::Ahelo),
    (&["ah-64", "apache"], "AH-64 Apache helicopter", Role::Ahelo),
    (&["oh-58", "kiowa"], "OH-58 Kiowa helicopter", Role::Ahelo),
    (&["sa-11", "buk"], "Buk (SA-11) missile launcher", Role::Sam),
    (&["sa-10", "s-300"], "S-300 missile launcher", Role::Sam),
    (&["sa-15", "tor"], "Tor (SA-15) missile vehicle", Role::Sam),
    (&["sa-8", "osa"], "Osa (SA-8) missile vehicle", Role::Sam),
    (&["sa-6", "kub"], "Kub (SA-6) missile launcher", Role::Sam),
    (&["sa-19", "tunguska"], "Tunguska air-defence vehicle", Role::Sam),
    (&["sa-22", "pantsir"], "Pantsir-S1 air-defence vehicle", Role::Sam),
    (&["sa-2", "s-75"], "S-75 (SA-2) missile launcher", Role::Sam),
    (&["sa-3", "s-125"], "S-125 (SA-3) missile launcher", Role::Sam),
    (&["patriot"], "Patriot missile launcher", Role::Sam),
    (&["nasams"], "NASAMS missile launcher", Role::Sam),
    (&["shilka", "zsu-23"], "ZSU-23-4 Shilka anti-aircraft vehicle", Role::Sam),
    (&["zu-23"], "ZU-23 anti-aircraft gun", Role::Sam),
    (&["gepard"], "Gepard anti-aircraft vehicle", Role::Sam),
    (&["bm-21", "grad"], "BM-21 Grad rocket launcher", Role::Arty),
    (&["bm-27", "uragan"], "BM-27 Uragan rocket launcher", Role::Arty),
    (&["bm-30", "smerch"], "BM-30 Smerch rocket launcher", Role::Arty),
    (&["2s19", "msta"], "2S19 Msta self-propelled howitzer", Role::Arty),
    (&["2s3"], "2S3 Akatsiya self-propelled howitzer", Role::Arty),
    (&["2s1"], "2S1 Gvozdika self-propelled howitzer", Role::Arty),
    (&["m-109", "m109"], "M109 self-propelled howitzer", Role::Arty),
    (&["mlrs", "m270"], "M270 MLRS rocket launcher", Role::Arty),
    (&["himars"], "M142 HIMARS rocket launcher", Role::Arty),
    (&["scud"], "Scud missile launcher", Role::Arty),
    (&["arleigh", "burke"], "Arleigh Burke-class destroyer", Role::Ship),
    (&["ticonderoga"], "Ticonderoga-class cruiser", Role::Ship),
    (&["perry"], "Oliver Hazard Perry-class frigate", Role::Ship),
    (&["moskva"], "Slava-class cruiser", Role::Ship),
    (&["molniya"], "Molniya missile corvette", Role::Ship),
];

/// Does `hay` (lower case) mention `pat` as a word? The start must be a word
/// boundary; so must the end, unless the pattern ends in a digit -- "su-25"
/// matches "su-25t", but "tor" does not match "tornado".
fn mentions(hay: &str, pat: &str) -> bool {
    let mut from = 0;
    while let Some(off) = hay[from..].find(pat) {
        let i = from + off;
        let j = i + pat.len();
        let before_ok = hay[..i].chars().next_back().map_or(true, |c| !c.is_alphanumeric());
        let after = hay[j..].chars().next();
        let after_ok = match after {
            None => true,
            Some(c) if !c.is_alphanumeric() => true,
            Some(c) => pat.ends_with(|p: char| p.is_ascii_digit()) && !c.is_ascii_digit(),
        };
        if before_ok && after_ok {
            return true;
        }
        from = i + pat.len().max(1);
        while !hay.is_char_boundary(from) {
            from += 1;
        }
    }
    false
}

/// The real equipment a (sanitised) story names, by role, first mention per
/// role.
fn named_kit(text: &str) -> Vec<(Role, &'static str)> {
    let t = text.to_lowercase();
    let mut out: Vec<(Role, &'static str)> = Vec::new();
    for (pats, name, role) in TYPE_NAMES {
        if out.iter().any(|(r, _)| r == role) {
            continue;
        }
        if pats.iter().any(|p| mentions(&t, p)) {
            out.push((*role, name));
        }
    }
    out
}

// ── the scene ────────────────────────────────────────────────────────────────

/// What a dispatch's picture is of, from the day's dominant story.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SceneKind {
    Capture,
    CaptureAirfield,
    Contested,
    Advance,
    Opening,
    Stalemate,
    AirCombat,
    Helicopters,
    AirDefence,
    Convoy,
    Armour,
    Artillery,
    Naval,
    Carrier,
    Resupply,
}

/// Where the camera can sensibly be for a subject.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Frame {
    Ground,
    Air,
    Sea,
}

/// One way of picturing a kind of story. `{a:role}` is the acting side's
/// kit, `{v:role}` the side on the receiving end, `+` makes it plural;
/// `{town}`, `{road}`, `{open}`, `{airfield}`, `{coast}` come from the theatre.
struct Variant(&'static str, Frame);

use Frame::{Air as FA, Ground as FG, Sea as FS};

fn variants(k: SceneKind) -> &'static [Variant] {
    match k {
        SceneKind::Capture => &[
            Variant("{a:ifv+} moving in file down the main street of {town}, shattered windows and a collapsed roof, infantry spread out along the walls", FG),
            Variant("a {a:tank} rolling past a burnt-out checkpoint at the entrance to {town}, black smoke rising from a building behind it", FG),
            Variant("infantry dismounting from a {a:ifv} in the rubble-strewn square of {town}, small figures crouched in cover", FG),
            Variant("a knocked-out {v:ifv} burning at the roadside as a {a:tank} pushes through drifting smoke into {town}", FG),
        ],
        SceneKind::CaptureAirfield => &[
            Variant("{a:ifv+} crossing a cratered runway at {airfield}, a wrecked hangar smouldering in the distance", FG),
            Variant("a {a:thelo} setting down beside a damaged control tower at {airfield}, rotor wash kicking up dust and grass", FG),
            Variant("a {a:tank} taking up position at the end of the runway at {airfield}, smoke from a burning fuel store behind", FG),
            Variant("a burnt-out {v:jet} in a shattered aircraft shelter at {airfield}, a {a:ifv} passing on the taxiway", FG),
        ],
        SceneKind::Contested => &[
            Variant("smoke columns rising over the rooftops of {town} after days of fighting, a knocked-out {v:ifv} on the approach road", FG),
            Variant("artillery shells bursting on the outskirts of {town}, seen across {open}", FG),
            Variant("a {a:tank} firing from a tree line toward {town}, muzzle flash and a burst of dust", FG),
            Variant("a pockmarked, half-collapsed street in {town}, a burnt car and a wrecked {v:ifv}, smoke hanging in the air", FG),
        ],
        SceneKind::Advance => &[
            Variant("a long armoured column of {a:tank+} and {a:ifv+} advancing along {road}, well spaced, dust trailing behind", FG),
            Variant("{a:ifv+} fording a shallow river at speed, spray thrown up, {open} beyond", FG),
            Variant("a {a:tank} cresting a ridge above {open}, its tracks throwing up dirt", FG),
            Variant("{a:ifv+} racing past an abandoned roadblock on {road}, a destroyed {v:tank} pushed into the ditch", FG),
        ],
        SceneKind::Opening => &[
            Variant("the first night of the war: tracer fire and the flashes of explosions on the horizon over {town}", FG),
            Variant("a {a:jet} taking off in full afterburner, heat haze over the runway at {airfield}", FG),
            Variant("a column of {a:tank+} crossing {open} at first light, headlights still on", FG),
            Variant("a {a:arty} firing the first salvo of the war, smoke and flame, {open} beyond", FG),
        ],
        SceneKind::Stalemate => &[
            Variant("an empty sandbagged trench line on a ridge, {open} stretching away to a distant plume of smoke", FG),
            Variant("a camouflaged {a:tank} dug into a hull-down position under netting at the edge of {open}", FG),
            Variant("a deserted road into {town} blocked by concrete barriers and a burnt-out car, artillery smoke far off", FG),
            Variant("a lone {a:arty} firing from a camouflaged position, the smoke of the shot hanging in the air", FG),
            Variant("a shell-cratered no man's land across {open}, wrecked vehicles scattered between the lines", FG),
        ],
        SceneKind::AirCombat => &[
            Variant("a {a:jet} banking hard at low level over {open}, vapour streaming off its wings", FA),
            Variant("white contrails twisting high above {open}, a flare trail and a distant smoke puff where an aircraft was hit", FA),
            Variant("a {a:jet} releasing a fan of flares as it pulls up over {open}", FA),
            Variant("two {a:fighter+} climbing steeply in loose formation, afterburners glowing", FA),
            Variant("the burning wreckage of a {v:jet} scattered across {open}, a black smoke column rising", FG),
        ],
        SceneKind::Helicopters => &[
            Variant("a {a:ahelo} flying nap-of-the-earth along a river valley, rotor blur", FA),
            Variant("a pair of {a:thelo+} crossing a ridgeline low over {open}, haze behind", FA),
            Variant("the wreck of a downed {v:thelo} in {open}, its tail boom broken off, smoke curling up", FG),
            Variant("a {a:ahelo} firing rockets toward a tree line, smoke trails streaking ahead", FA),
        ],
        SceneKind::AirDefence => &[
            Variant("a burnt-out {v:sam} in a scorched field, its launch rails empty, black smoke drifting", FG),
            Variant("a {v:sam} firing a missile, a bright exhaust trail climbing into the sky", FG),
            Variant("a radar site on a hilltop torn apart by an air strike, fires still burning", FG),
            Variant("a camouflaged {v:sam} under netting at the edge of {open}, an explosion rising behind a nearby hill", FG),
        ],
        SceneKind::Convoy => &[
            Variant("a line of burnt-out {v:truck+} along {road}, cabs blackened, smoke still rising", FG),
            Variant("a supply convoy of {v:truck+} caught in an air strike on {road}, the flash of an explosion and flying debris", FG),
            Variant("a wrecked fuel tanker on its side beside {road}, a tall column of black smoke, other trucks scattered", FG),
            Variant("the aftermath of an ambush on {road}: a burnt {v:truck} slewed across the lane, a damaged {v:ifv} beyond", FG),
        ],
        SceneKind::Armour => &[
            Variant("a knocked-out {v:tank} with its turret blown off beside {road}, still smouldering", FG),
            Variant("a burnt {v:ifv} in a roadside ditch at the edge of {town}, hatches open", FG),
            Variant("a smoke column where a {v:tank} was hit in {open}, seen from far off", FG),
            Variant("a {a:tank} firing, the muzzle blast kicking up dust, a burning {v:ifv} in the distance", FG),
        ],
        SceneKind::Artillery => &[
            Variant("a {a:arty} firing, the muzzle flash lighting the ground around it", FG),
            Variant("a ripple of rockets leaving a {a:arty}, smoke trails arcing into the sky", FG),
            Variant("craters and smoke drifting across {open} after an artillery barrage", FG),
            Variant("shell bursts walking across {open} toward {town}, seen from far off", FG),
        ],
        SceneKind::Naval => &[
            Variant("a {v:ship} burning at sea, a thick smoke column over a grey swell", FS),
            Variant("a {a:ship} under way at speed off {coast}, a big bow wave", FS),
            Variant("a missile leaving the deck of a {a:ship} in a burst of smoke and flame", FS),
        ],
        SceneKind::Carrier => &[
            Variant("a jet launching off the catapult of an aircraft carrier, steam trailing across the deck, deck crew small in the frame", FS),
            Variant("an aircraft carrier under way on a calm sea, a jet on final approach behind it", FS),
            Variant("a jet catching the arrestor wire on a carrier deck, smoke off its tyres", FS),
        ],
        SceneKind::Resupply => &[
            Variant("a {a:thelo} unloading crates at a dusty landing zone, rotor wash raising a brownout cloud", FG),
            Variant("{a:truck+} moving up {road} with supplies, well spaced, dust behind them", FG),
            Variant("ammunition crates being unloaded from a {a:truck} at a camouflaged depot at the edge of {open}, the crew small and far off", FG),
        ],
    }
}

/// Camera position and framing, with the lens that goes with it.
const SHOTS_GROUND: &[(&str, &str)] = &[
    ("photographed from a hillside far away, telephoto compression and heat haze", "400mm telephoto lens"),
    ("wide view from a helicopter door, looking down at an angle", "wide-angle lens"),
    ("ground-level view from beside the road, low angle", "35mm lens"),
    ("long-lens view across a valley, the subject small in the frame", "300mm telephoto lens"),
    ("wide establishing shot under a big sky, the action small in the frame", "24mm lens"),
    ("handheld frame taken on the move, slight motion blur", "35mm lens"),
    ("tight telephoto framing, background compressed and soft", "200mm lens"),
    ("high vantage point from a rooftop", "50mm lens"),
];
const SHOTS_AIR: &[(&str, &str)] = &[
    ("photographed from the ground with a long telephoto lens, heat shimmer", "600mm telephoto lens"),
    ("air-to-air view from a chase aircraft", "70-200mm lens"),
    ("wide view with the aircraft small against a huge sky", "24mm lens"),
    ("panning shot, the background streaked with motion blur", "300mm lens"),
    ("seen from a ridge, looking down on the aircraft as it passes below", "200mm lens"),
];
const SHOTS_SEA: &[(&str, &str)] = &[
    ("seen from far off across the water, telephoto compression", "400mm telephoto lens"),
    ("aerial view from a helicopter", "wide-angle lens"),
    ("low angle from a small boat, spray in the air", "35mm lens"),
];

/// Time of day. The last two are dark; they take the night weather.
const LIGHT: &[&str] = &[
    "at first light, a low sun raking across the ground",
    "in harsh midday sun",
    "late in the afternoon, long shadows",
    "at dusk under a deep orange sky",
    "under flat grey overcast light",
    "at night, lit only by fires and flares",
    "in the blue hour before dawn",
];
const NIGHT_WEATHER: &[&str] = &["smoke drifting through the firelight", "a clear dark sky", "low cloud lit from below"];

/// How a dispatch will be pictured: subject, camera, light.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScenePlan {
    pub kind: SceneKind,
    /// What is in the frame, with the equipment named.
    pub subject: String,
    /// Where the camera is and how the frame is composed.
    pub shot: &'static str,
    /// The lens, for the default photographic look.
    pub lens: &'static str,
    pub light: &'static str,
    pub weather: &'static str,
}

/// A deterministic stream of choices. Seeded from the dispatch itself, so a
/// retry of the same day pictures the same scene, while another server or
/// another day lands somewhere else.
struct Dice(u64);

impl Dice {
    fn new(parts: &[&str]) -> Self {
        let mut h: u64 = 0xcbf2_9ce4_8422_2325;
        for p in parts {
            for b in p.as_bytes() {
                h ^= *b as u64;
                h = h.wrapping_mul(0x0000_0100_0000_01b3);
            }
            h ^= 0xff;
            h = h.wrapping_mul(0x0000_0100_0000_01b3);
        }
        Self(h)
    }

    /// splitmix64.
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }

    fn pick<'a, T>(&mut self, xs: &'a [T]) -> &'a T {
        &xs[(self.next() % xs.len() as u64) as usize]
    }
}

/// Headline words to the scene they call for. The writer composes its own
/// headline, so this is the best single read of what the dispatch is about;
/// specific phrases come before the general words they contain.
const HEADLINE_CUES: &[(&str, SceneKind)] = &[
    ("aircraft carrier", SceneKind::Carrier),
    ("carrier group", SceneKind::Carrier),
    ("carrier deck", SceneKind::Carrier),
    ("changes hands again", SceneKind::Contested),
    ("deadlock breaks", SceneKind::Advance),
    ("breakthrough", SceneKind::Advance),
    ("air defence", SceneKind::AirDefence),
    ("air defense", SceneKind::AirDefence),
    ("air-defence", SceneKind::AirDefence),
    ("sam", SceneKind::AirDefence),
    ("sams", SceneKind::AirDefence),
    ("radar", SceneKind::AirDefence),
    ("missile site", SceneKind::AirDefence),
    ("convoy", SceneKind::Convoy),
    ("supply line", SceneKind::Convoy),
    ("supply lines", SceneKind::Convoy),
    ("logistics", SceneKind::Convoy),
    ("rear", SceneKind::Convoy),
    ("depot", SceneKind::Convoy),
    ("ambush", SceneKind::Convoy),
    ("trucks", SceneKind::Convoy),
    ("resupply", SceneKind::Resupply),
    ("airlift", SceneKind::Resupply),
    ("reinforcements", SceneKind::Resupply),
    ("helicopter", SceneKind::Helicopters),
    ("helicopters", SceneKind::Helicopters),
    ("helos", SceneKind::Helicopters),
    ("gunships", SceneKind::Helicopters),
    ("warship", SceneKind::Naval),
    ("navy", SceneKind::Naval),
    ("naval", SceneKind::Naval),
    ("fleet", SceneKind::Naval),
    ("at sea", SceneKind::Naval),
    ("harbour", SceneKind::Naval),
    ("war begins", SceneKind::Opening),
    ("campaign opens", SceneKind::Opening),
    ("day one", SceneKind::Opening),
    ("opening", SceneKind::Opening),
    ("stalemate", SceneKind::Stalemate),
    ("deadlock", SceneKind::Stalemate),
    ("line holds", SceneKind::Stalemate),
    ("does not move", SceneKind::Stalemate),
    ("static", SceneKind::Stalemate),
    ("no change", SceneKind::Stalemate),
    ("no movement", SceneKind::Stalemate),
    ("quiet", SceneKind::Stalemate),
    ("dug in", SceneKind::Stalemate),
    ("in the air", SceneKind::AirCombat),
    ("air war", SceneKind::AirCombat),
    ("air battle", SceneKind::AirCombat),
    ("skies", SceneKind::AirCombat),
    ("sky", SceneKind::AirCombat),
    ("dogfight", SceneKind::AirCombat),
    ("jets", SceneKind::AirCombat),
    ("shot down", SceneKind::AirCombat),
    ("aircraft", SceneKind::AirCombat),
    ("pilots", SceneKind::AirCombat),
    ("airfield", SceneKind::CaptureAirfield),
    ("airbase", SceneKind::CaptureAirfield),
    ("air base", SceneKind::CaptureAirfield),
    ("airport", SceneKind::CaptureAirfield),
    ("advance", SceneKind::Advance),
    ("advances", SceneKind::Advance),
    ("offensive", SceneKind::Advance),
    ("moves into", SceneKind::Advance),
    ("push", SceneKind::Advance),
    ("pushes", SceneKind::Advance),
    ("sweep", SceneKind::Advance),
    ("breaks", SceneKind::Advance),
    ("falls", SceneKind::Capture),
    ("fall of", SceneKind::Capture),
    ("taken", SceneKind::Capture),
    ("takes", SceneKind::Capture),
    ("captures", SceneKind::Capture),
    ("captured", SceneKind::Capture),
    ("seize", SceneKind::Capture),
    ("seizes", SceneKind::Capture),
    ("retake", SceneKind::Capture),
    ("retakes", SceneKind::Capture),
    ("changes hands", SceneKind::Capture),
    ("first ground", SceneKind::Capture),
    ("artillery", SceneKind::Artillery),
    ("barrage", SceneKind::Artillery),
    ("shelling", SceneKind::Artillery),
    ("guns", SceneKind::Artillery),
    ("rockets", SceneKind::Artillery),
    ("tank", SceneKind::Armour),
    ("tanks", SceneKind::Armour),
    ("armour", SceneKind::Armour),
    ("armor", SceneKind::Armour),
    ("pressure", SceneKind::Contested),
    ("approaches", SceneKind::Contested),
    ("battle for", SceneKind::Contested),
    ("direction", SceneKind::Contested),
    ("fighting", SceneKind::Contested),
    ("siege", SceneKind::Contested),
];

/// The scene an angle calls for; `None` for the tallies and standing items,
/// which are pictured by what the losses were.
fn angle_scene(angle: &str) -> Option<SceneKind> {
    Some(match angle {
        "opening_day" | "opening_line" => SceneKind::Opening,
        "objective_taken" | "objective_taken_by" | "first_loss" => SceneKind::Capture,
        "objective_traded" | "pressure" | "axis_activity" => SceneKind::Contested,
        "streak" | "front_broken" | "country_sector" => SceneKind::Advance,
        "front_stalled" | "static_front" | "quiet" => SceneKind::Stalemate,
        "sead" => SceneKind::AirDefence,
        "logistics_struck" => SceneKind::Convoy,
        "air_war" | "air_war_lopsided" | "pilot_standout" | "top_gun" | "weapon_of_the_day" => {
            SceneKind::AirCombat
        }
        _ => return None,
    })
}

/// Angles whose `side` is the one on the receiving end.
fn side_is_victim(angle: &str) -> bool {
    matches!(angle, "first_loss" | "sead" | "logistics_struck" | "pressure")
}

/// Loss category (as the tally labels it) to the scene of its aftermath.
fn loss_scene(cat: &str) -> SceneKind {
    match cat {
        "AIRCRAFT" => SceneKind::AirCombat,
        "HELO" => SceneKind::Helicopters,
        "NAVAL" => SceneKind::Naval,
        "AIR DEF" | "RADAR" => SceneKind::AirDefence,
        "ARTY" => SceneKind::Artillery,
        "ARMOR" | "APC" => SceneKind::Armour,
        "LOGISTICS" => SceneKind::Convoy,
        _ => SceneKind::Contested,
    }
}

/// The loss category that dominated the day, and the side that took most of
/// it.
fn dominant_loss(d: &NewsDigest) -> Option<(&str, &'static str)> {
    d.facts
        .losses
        .iter()
        .filter(|(_, t)| t.blue + t.red > 0)
        .max_by_key(|(_, t)| t.blue + t.red)
        .map(|(c, t)| (c.as_str(), if t.blue >= t.red { "Blue" } else { "Red" }))
}

fn other(side: &str) -> &'static str {
    if side == "Blue" {
        "Red"
    } else {
        "Blue"
    }
}

/// "Blue" / "Red" for a side as a dispatch names it.
fn side_key(d: &NewsDigest, name: &str) -> Option<&'static str> {
    let n = name.trim();
    let f = &d.factions;
    if n.eq_ignore_ascii_case(&f.blue) || n.eq_ignore_ascii_case(&f.blue_adj) || n.eq_ignore_ascii_case("Blue") {
        Some("Blue")
    } else if n.eq_ignore_ascii_case(&f.red) || n.eq_ignore_ascii_case(&f.red_adj) || n.eq_ignore_ascii_case("Red") {
        Some("Red")
    } else {
        None
    }
}

/// Does `hay` (lower case) contain `cue` as whole words?
fn has_words(hay: &str, cue: &str) -> bool {
    let mut from = 0;
    while let Some(off) = hay[from..].find(cue) {
        let i = from + off;
        let j = i + cue.len();
        let before = hay[..i].chars().next_back().map_or(true, |c| !c.is_alphanumeric());
        let after = hay[j..].chars().next().map_or(true, |c| !c.is_alphanumeric());
        if before && after {
            return true;
        }
        from = j;
        while !hay.is_char_boundary(from) {
            from += 1;
        }
    }
    false
}

fn looks_like_airfield(s: &str) -> bool {
    let s = s.to_lowercase();
    ["airfield", "airbase", "air base", "airport", "aerodrome", "heliport"].iter().any(|w| s.contains(w))
        || has_words(&s, "ab")
}

/// A subject that is a place worth naming in the picture: short, no digits
/// (those are grid squares and FOB numbers, not towns).
fn nameable_place(s: &str) -> Option<String> {
    let s = s.trim();
    (s.chars().count() >= 3
        && s.chars().count() <= 32
        && !s.chars().any(|c| c.is_ascii_digit())
        && !s.starts_with("the "))
        .then(|| s.to_string())
}

/// "a" before a word said with a vowel sound becomes "an". Equipment names
/// start with letters said as letters ("an M1A2", "an F-16C", "a BMP-2").
fn fix_articles(s: &str) -> String {
    fn wants_an(w: &str) -> bool {
        let mut cs = w.chars();
        let Some(c0) = cs.next() else { return false };
        let c1 = cs.next();
        if ["HEMTT", "NASAMS"].iter().any(|x| w.starts_with(x)) {
            return false;
        }
        let spelled = c0.is_ascii_uppercase()
            && c1.map_or(true, |c| c == '-' || c.is_ascii_digit() || c.is_ascii_uppercase());
        if spelled {
            return "AEFHILMNORSX".contains(c0);
        }
        let l = c0.to_ascii_lowercase();
        "aeio".contains(l) || (l == 'u' && w.to_lowercase().starts_with("un"))
    }
    let words: Vec<&str> = s.split(' ').collect();
    let mut out = String::with_capacity(s.len() + 8);
    for (i, w) in words.iter().enumerate() {
        if i > 0 {
            out.push(' ');
        }
        let next = words.get(i + 1).copied().unwrap_or("");
        match *w {
            "a" if wants_an(next) => out.push_str("an"),
            "A" if wants_an(next) => out.push_str("An"),
            _ => out.push_str(w),
        }
    }
    out
}

/// Fill a variant's placeholders.
#[allow(clippy::too_many_arguments)]
fn fill_scene(
    tpl: &str,
    dice: &mut Dice,
    setting: &Setting,
    actor: &Kit,
    victim: &Kit,
    named: &[(Role, &'static str)],
    named_victim: bool,
    place: Option<&str>,
) -> String {
    let land = setting.land;
    let mut used: Vec<Role> = Vec::new();
    let mut out = String::with_capacity(tpl.len() + 64);
    let mut rest = tpl;
    while let Some(open) = rest.find('{') {
        out.push_str(&rest[..open]);
        let Some(close) = rest[open..].find('}') else {
            out.push_str(&rest[open..]);
            rest = "";
            break;
        };
        let key = &rest[open + 1..open + close];
        rest = &rest[open + close + 1..];
        let with_place = |desc: &str| match place {
            Some(p) => format!("{p}, {desc}"),
            None => desc.to_string(),
        };
        let text = match key {
            "town" => with_place(*dice.pick(land.towns)),
            "airfield" => with_place(*dice.pick(land.airfields)),
            "road" => dice.pick(land.roads).to_string(),
            "open" => dice.pick(land.open).to_string(),
            "coast" => dice.pick(land.coast).to_string(),
            k => {
                let (who, role) = k.split_once(':').unwrap_or(("a", k));
                let plural = role.ends_with('+');
                let role_name = role.trim_end_matches('+');
                match Role::parse(role_name) {
                    Some(r) => {
                        // The story's own equipment first, once, on the side
                        // the story is about; then the side's period kit.
                        let name = match named.iter().find(|(nr, _)| *nr == r) {
                            Some((_, n)) if !used.contains(&r) && (who == "v") == named_victim => {
                                used.push(r);
                                *n
                            }
                            _ => *dice.pick((if who == "v" { victim } else { actor }).get(r)),
                        };
                        if plural {
                            format!("{name}s")
                        } else {
                            name.to_string()
                        }
                    }
                    None => String::new(),
                }
            }
        };
        out.push_str(&text);
    }
    out.push_str(rest);
    fix_articles(&out)
}

/// Decide what a dispatch's picture shows. Deterministic in (instance, day,
/// headline): the same dispatch always plans the same scene; another server
/// or another day, a different one.
pub fn plan_scene(d: &NewsDigest, instance: &str, setting: &Setting) -> ScenePlan {
    let names = private_names(d);
    let headline = sanitise(&d.headline, &names).to_lowercase();
    // Place names are not cues: "SAM RIDGE FALLS" is a capture, not an
    // air-defence story.
    let headline_cues = d
        .items
        .iter()
        .filter(|it| {
            matches!(
                it.angle.as_str(),
                "objective_taken"
                    | "objective_taken_by"
                    | "objective_traded"
                    | "pressure"
                    | "axis_activity"
                    | "country_sector"
            )
        })
        .fold(headline.clone(), |h, it| {
            if it.subject.trim().chars().count() >= 3 {
                replace_ci(&h, it.subject.trim(), " ")
            } else {
                h
            }
        });
    let mut dice = Dice::new(&[instance, &d.day, &d.headline]);

    // The kind: the headline if it says, else the lead angle, else the day's
    // losses.
    let lead = d.items.iter().find(|it| angle_scene(&it.angle).is_some());
    let top_is_tally = d.items.first().map_or(true, |it| angle_scene(&it.angle).is_none());
    let from_headline = HEADLINE_CUES.iter().find(|(cue, _)| has_words(&headline_cues, cue)).map(|(_, k)| *k);
    let loss = dominant_loss(d);
    let mut kind = match (from_headline, lead) {
        (Some(k), _) => k,
        (None, Some(it)) if !top_is_tally => angle_scene(&it.angle).unwrap(),
        (None, _) => match loss {
            Some((cat, _)) => loss_scene(cat),
            None => lead.and_then(|it| angle_scene(&it.angle)).unwrap_or(SceneKind::Stalemate),
        },
    };
    // Which item the picture is about, for its place and its sides.
    let about = d
        .items
        .iter()
        .find(|it| angle_scene(&it.angle).map_or(false, |k| k == kind || (kind == SceneKind::CaptureAirfield && k == SceneKind::Capture)))
        .or(lead);
    let place = about
        .filter(|it| {
            matches!(
                it.angle.as_str(),
                "objective_taken" | "objective_taken_by" | "objective_traded" | "pressure" | "axis_activity"
            )
        })
        .and_then(|it| nameable_place(&sanitise(&it.subject, &names)));
    if kind == SceneKind::Capture && place.as_deref().map_or(false, looks_like_airfield) {
        kind = SceneKind::CaptureAirfield;
    }
    // A carrier is a present-day story; anywhere else it is a sea story.
    if kind == SceneKind::Carrier && !matches!(setting.era, Era::Modern | Era::Custom) {
        kind = SceneKind::Naval;
    }

    // Who is acting and who is on the receiving end.
    let from_item = about.and_then(|it| {
        let s = side_key(d, it.vars.get("side")?)?;
        Some(if side_is_victim(&it.angle) { other(s) } else { s })
    });
    let actor = match (from_item, loss) {
        (Some(s), _) => s,
        (None, Some((_, loser))) => other(loser),
        (None, None) => {
            if dice.next() % 2 == 0 {
                "Blue"
            } else {
                "Red"
            }
        }
    };
    let actor_kit = kit_for(setting.era, actor, &d.factions);
    let victim_kit = kit_for(setting.era, other(actor), &d.factions);

    // The story's own equipment, from everything it says (names stripped).
    let mut said = sanitise(&d.headline, &names);
    for p in &d.body {
        said.push(' ');
        said.push_str(&sanitise(p, &names));
    }
    for it in &d.items {
        said.push(' ');
        said.push_str(&sanitise(&it.text, &names));
    }
    let named = named_kit(&said);

    let variant = dice.pick(variants(kind));
    // In a story about losses the hardware it names is what was lost; in any
    // other, what did the work.
    let named_victim = matches!(
        kind,
        SceneKind::AirDefence | SceneKind::Convoy | SceneKind::Armour | SceneKind::Naval
    );
    let subject = fill_scene(
        variant.0,
        &mut dice,
        setting,
        actor_kit,
        victim_kit,
        &named,
        named_victim,
        place.as_deref(),
    );
    let (shot, lens) = *dice.pick(match variant.1 {
        Frame::Ground => SHOTS_GROUND,
        Frame::Air => SHOTS_AIR,
        Frame::Sea => SHOTS_SEA,
    });
    // A subject that names its own time of day keeps it.
    let li = (dice.next() % LIGHT.len() as u64) as usize;
    let light = if variant.0.contains("night") {
        LIGHT[5]
    } else if variant.0.contains("first light") {
        LIGHT[0]
    } else {
        LIGHT[li]
    };
    let weather = if light.contains("night") {
        *dice.pick(NIGHT_WEATHER)
    } else {
        *dice.pick(setting.land.weather)
    };
    ScenePlan { kind, subject, shot, lens, light, weather }
}

// ── the prompt ───────────────────────────────────────────────────────────────

/// Everyone the digest names who is a private individual -- the pilots.
fn private_names(d: &NewsDigest) -> Vec<String> {
    let mut out: Vec<String> = d
        .items
        .iter()
        .flat_map(|it| {
            let mut v: Vec<String> =
                it.vars.iter().filter(|(k, _)| k.contains("pilot")).map(|(_, v)| v.clone()).collect();
            if it.angle.contains("pilot") || it.angle == "top_gun" {
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

fn capitalise(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

/// What makes equipment period-correct, said once.
fn era_clause(era: Era) -> &'static str {
    match era {
        Era::Georgia2008 => {
            " All military equipment is authentic August 2008 Russian and Georgian hardware, \
             exactly as it really looked."
        }
        Era::Modern => " All military equipment is real, in-service hardware, exactly as it really looks.",
        Era::Ww2 => " All equipment is authentic 1944 hardware.",
        Era::Custom => "",
    }
}

/// The prompt for one dispatch: a planned scene (subject, camera, light),
/// the setting, a line of the story (sanitised), the look, what to avoid and
/// the rules.
pub fn build_prompt(d: &NewsDigest, instance: &str, setting: &Setting, style: Option<&str>) -> String {
    let plan = plan_scene(d, instance, setting);
    let names = private_names(d);
    // The headline is upper case wire style; a model reads that as text to
    // render. Sentence case it.
    let mut headline = sanitise(&d.headline, &names).to_lowercase();
    // ... but keep the place and side names proper nouns.
    let proper = d
        .items
        .iter()
        .filter(|it| !it.angle.contains("pilot") && it.angle != "top_gun" && !it.subject.starts_with("the "))
        .map(|it| it.subject.as_str())
        .chain([d.factions.blue.as_str(), d.factions.red.as_str(), d.factions.blue_adj.as_str(), d.factions.red_adj.as_str()]);
    for p in proper {
        let p = p.trim();
        if p.chars().count() >= 3 && p.chars().next().map_or(false, char::is_uppercase) {
            headline = replace_ci(&headline, p, p);
        }
    }
    let headline = capitalise(&headline);
    let first = d
        .body
        .first()
        .cloned()
        .or_else(|| d.items.first().map(|i| i.text.clone()))
        .unwrap_or_default();
    let style = style.map(str::trim).filter(|s| !s.is_empty()).map(|s| clip(s, STYLE_CHARS));
    let look = match &style {
        Some(s) => format!("Style: {s}"),
        None => format!(
            "Style: unstaged news wire photograph, photojournalism, shot on a {}, natural light, \
             film grain, muted colours, real-world scale and detail. Avoid: 3D render, CGI, video game \
             graphics, illustration, painting, concept art, toy-like or miniature-looking vehicles, \
             oversaturated colour, invented or fantasy vehicles.",
            plan.lens
        ),
    };
    let head = format!(
        "A news photograph: {}. {}, {}, {}. Setting: {}.{} Report: {headline}.",
        plan.subject,
        capitalise(plan.shot),
        plan.light,
        plan.weather,
        setting.text,
        era_clause(setting.era),
    );
    let tail = format!(" {look} Composition: {COMPOSITION} {RULES}");
    // The story gets whatever room is left, so the rules at the end are never
    // what a length cap cuts.
    let room = PROMPT_MAX.saturating_sub(head.chars().count() + tail.chars().count() + 1);
    let story = clip(&sanitise(&first, &names), STORY_CHARS.min(room.saturating_sub(3)));
    let mut p = head;
    if !story.is_empty() && room > 40 {
        p.push(' ');
        p.push_str(&story);
    }
    p.push_str(&tail);
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
    let prompt = build_prompt(d, &inst.id, &campaign_setting(inst, &d.facts.theatre), style);
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

    fn item(angle: &str, subject: &str, weight: u8, vars: &[(&str, &str)]) -> NewsItem {
        NewsItem {
            angle: angle.into(),
            subject: subject.into(),
            weight,
            vars: vars.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
            text: format!("{subject}: {angle}."),
        }
    }

    fn story(day: &str, headline: &str, items: Vec<NewsItem>, theatre: &str, factions: Factions) -> NewsDigest {
        NewsDigest {
            day: day.into(),
            generated: Utc::now(),
            round: 1,
            headline: headline.into(),
            body: vec![],
            written_by: "templates".into(),
            items,
            facts: DigestFacts { theatre: theatre.into(), ..Default::default() },
            factions,
            facts_hash: 0,
            final_: true,
        }
    }

    fn rgw() -> Setting {
        campaign_setting(&inst(serde_json::json!({"id": "vs2", "engine_config": "C:\\x\\RGW2008_CFG"})), "Caucasus")
    }

    fn syria() -> Setting {
        campaign_setting(&inst(serde_json::json!({"id": "vs1", "engine_config": "C:\\x\\ODFv2_CFG"})), "Syria")
    }

    fn georgia_russia() -> Factions {
        Factions::new(Some("Georgia"), Some("Russia"), Some("Georgian"), Some("Russian"))
    }

    fn coalition_syria() -> Factions {
        Factions::new(Some("the Coalition"), Some("Syria"), Some("coalition"), Some("Syrian"))
    }

    fn capture(day: &str, town: &str, side: &str, f: Factions, theatre: &str) -> NewsDigest {
        story(
            day,
            &format!("{} FALLS", town.to_uppercase()),
            vec![item("objective_taken", town, 55, &[("side", side), ("side_adj", side)])],
            theatre,
            f,
        )
    }

    /// The part of a prompt that describes what to draw -- everything before
    /// the composition guidance, which names what to avoid.
    fn positive(p: &str) -> &str {
        p.split(" Composition:").next().unwrap()
    }

    #[test]
    fn pilot_names_and_callsigns_never_reach_the_prompt() {
        let d = digest(
            "VIPER 1-1 | BOB DOWNS FOUR OVER GORI",
            &["Viper 1-1 | Bob, flying as [JTF] \"Hammer\", shot down four Russian jets near Gori (callsign Hammer 2)."],
            Some("Viper 1-1 | Bob"),
        );
        let p = build_prompt(&d, "vs2", &rgw(), None);
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
    fn a_named_capture_keeps_the_place_and_loses_the_pilot() {
        let mut d = story(
            "2026-09-20",
            "GORI FALLS",
            vec![item(
                "objective_taken_by",
                "Gori",
                60,
                &[("side", "Russia"), ("side_adj", "Russian"), ("pilot", "Wolf 2 | Alice")],
            )],
            "Caucasus",
            georgia_russia(),
        );
        d.body = vec!["Wolf 2 | Alice led the Russian push into Gori.".into()];
        for inst in ["vs1", "vs2", "vs3"] {
            let plan = plan_scene(&d, inst, &rgw());
            assert!(matches!(plan.kind, SceneKind::Capture), "{plan:?}");
            let p = build_prompt(&d, inst, &rgw(), None);
            assert!(!p.to_lowercase().contains("alice") && !p.contains("Wolf"), "{p}");
            assert!(plan.subject.contains("Gori"), "{plan:?}");
        }
    }

    #[test]
    fn story_is_clipped() {
        let long = "word ".repeat(400);
        let d = digest("THE LINE HOLDS", &[&long], None);
        let p = build_prompt(&d, "x", &rgw(), None);
        assert!(p.chars().count() <= PROMPT_MAX, "{}", p.len());
        // Even with the longest operator text, the rules survive the cap.
        let setting = Setting { text: "x ".repeat(400), ..syria() };
        let setting = Setting { text: clip(&setting.text, SETTING_CHARS), ..setting };
        let style = "y ".repeat(600);
        let p = build_prompt(&d, "x", &setting, Some(&style));
        assert!(p.chars().count() <= PROMPT_MAX, "{}", p.chars().count());
        assert!(p.ends_with(RULES), "{p}");
        // And the whole thing is a sane Pollinations URL.
        let c = cfg(ImageArgs { provider: Some("pollinations".into()), ..Default::default() });
        assert!(c.pollinations_url(&p, 1).len() < 8000);
    }

    #[test]
    fn style_override_replaces_the_look_not_the_rules() {
        let d = digest("GORI FALLS", &["Gori fell."], None);
        let p = build_prompt(&d, "x", &rgw(), Some("Oil painting."));
        assert!(p.contains("Oil painting.") && !p.contains("35mm") && !p.contains("CGI"));
        assert!(p.contains("No gore"));
        assert!(p.contains(COMPOSITION));
        // The default look is a wire photo and says what it is not.
        let p = build_prompt(&d, "x", &rgw(), None);
        for w in ["photojournalism", "film grain", "muted colours", "natural light", "3D render", "CGI", "video game", "painting", "toy-like", "oversaturated"] {
            assert!(p.contains(w), "{w} missing: {p}");
        }
    }

    #[test]
    fn setting_comes_from_the_scenario() {
        let r = rgw();
        assert!(r.text.contains("2008") && r.era == Era::Georgia2008);
        let s = syria();
        assert!(s.text.contains("Syria") && s.era == Era::Modern);
        let plain = inst(serde_json::json!({"id": "vs3"}));
        assert!(campaign_setting(&plain, "").text.contains("present-day"));
        let over = inst(serde_json::json!({"id": "vs1", "news_image_setting": "Falklands 1982"}));
        let o = campaign_setting(&over, "Syria");
        assert_eq!((o.text.as_str(), o.era), ("Falklands 1982", Era::Custom));
        // No equipment list in the setting: that is what put tanks and two
        // helicopters in every picture.
        for t in [&r.text, &s.text] {
            assert!(!t.contains("helicopter") && !t.contains("armour") && !t.contains("jets"), "{t}");
        }
    }

    #[test]
    fn the_same_dispatch_plans_the_same_scene() {
        let d = capture("2026-09-21", "Gori", "Russia", georgia_russia(), "Caucasus");
        assert_eq!(plan_scene(&d, "vs2", &rgw()), plan_scene(&d, "vs2", &rgw()));
        assert_eq!(build_prompt(&d, "vs2", &rgw(), None), build_prompt(&d, "vs2", &rgw(), None));
    }

    #[test]
    fn servers_days_and_headlines_get_different_scenes() {
        let days: Vec<String> = (1..=12).map(|n| format!("2026-09-{n:02}")).collect();
        let mut all = HashSet::new();
        let mut differ_across_servers = 0;
        for day in &days {
            let a = plan_scene(&capture(day, "Gori", "Russia", georgia_russia(), "Caucasus"), "vs1", &rgw());
            let b = plan_scene(&capture(day, "Gori", "Russia", georgia_russia(), "Caucasus"), "vs2", &rgw());
            if a != b {
                differ_across_servers += 1;
            }
            all.insert(format!("{} | {} | {}", a.subject, a.shot, a.light));
            all.insert(format!("{} | {} | {}", b.subject, b.shot, b.light));
        }
        assert!(differ_across_servers >= 10, "{differ_across_servers}");
        assert!(all.len() >= 18, "only {} distinct scenes of 24", all.len());
        // Same server and day, different story.
        let d1 = capture("2026-09-21", "Gori", "Russia", georgia_russia(), "Caucasus");
        let d2 = capture("2026-09-21", "Tskhinvali", "Russia", georgia_russia(), "Caucasus");
        assert_ne!(plan_scene(&d1, "vs2", &rgw()), plan_scene(&d2, "vs2", &rgw()));
        // And every subject variant of a kind is reachable.
        let subjects: HashSet<String> = (0..60)
            .map(|n| {
                let d = capture(&format!("2026-{:02}-{:02}", 1 + n / 28, 1 + n % 28), "Gori", "Russia", georgia_russia(), "Caucasus");
                let p = plan_scene(&d, "vs2", &rgw());
                p.subject.split(',').next().unwrap().split(' ').take(3).collect::<Vec<_>>().join(" ")
            })
            .collect();
        assert!(subjects.len() >= 3, "{subjects:?}");
    }

    #[test]
    fn the_old_framing_is_gone() {
        let headlines = [
            "GORI FALLS",
            "STALEMATE HOLDS",
            "ONE-SIDED DAY IN THE AIR",
            "RUSSIA REAR UNDER SUSTAINED ATTACK",
            "RUSSIA AIR DEFENCES TAKE THE BRUNT",
            "THE CAMPAIGN OPENS",
            "COUNTING THE COST",
            "HELICOPTERS RESUPPLY THE FRONT",
            "SHELLING ON THE APPROACHES",
        ];
        for (i, h) in headlines.iter().enumerate() {
            for inst in ["vs1", "vs2"] {
                for (setting, f, t) in [(rgw(), georgia_russia(), "Caucasus"), (syria(), coalition_syria(), "Syria")] {
                    let d = story(&format!("2026-08-{:02}", i + 1), h, vec![], t, f);
                    let p = build_prompt(&d, inst, &setting, None);
                    let pos = positive(&p).to_lowercase();
                    for bad in ["shoulder", "from behind", "foreground", "cinematic", "in a row", "line-up", "parked"] {
                        assert!(!pos.contains(bad), "{bad}: {p}");
                    }
                    assert!(!p.contains("seen from behind"), "{p}");
                    // ... and the model is told to steer clear of it.
                    assert!(p.contains("no over-the-shoulder view"), "{p}");
                }
            }
        }
    }

    #[test]
    fn a_capture_story_pictures_a_capture_with_the_captors_kit() {
        for day in ["2026-09-01", "2026-09-02", "2026-09-03", "2026-09-04", "2026-09-05"] {
            let d = capture(day, "Gori", "Russia", georgia_russia(), "Caucasus");
            let plan = plan_scene(&d, "vs2", &rgw());
            assert_eq!(plan.kind, SceneKind::Capture);
            assert!(plan.subject.contains("Gori"), "{plan:?}");
            let russian = KIT_GEO_RU.tank.iter().chain(KIT_GEO_RU.ifv).any(|k| plan.subject.contains(k));
            assert!(russian, "{plan:?}");
            let p = build_prompt(&d, "vs2", &rgw(), None);
            assert!(p.contains("2008") && !p.contains("present-day"), "{p}");
            assert!(p.contains("Report: Gori falls."), "{p}");
        }
        // An airfield is taken on the runway, not in a town square.
        let d = capture("2026-09-06", "Senaki Airbase", "Russia", georgia_russia(), "Caucasus");
        let plan = plan_scene(&d, "vs2", &rgw());
        assert_eq!(plan.kind, SceneKind::CaptureAirfield);
        assert!(plan.subject.contains("Senaki Airbase"), "{plan:?}");
        // Syria: the Coalition takes a town with western kit, on Levant ground.
        let d = capture("2026-09-06", "Palmyra", "the Coalition", coalition_syria(), "Syria");
        let plan = plan_scene(&d, "vs1", &syria());
        let west = KIT_WEST.tank.iter().chain(KIT_WEST.ifv).any(|k| plan.subject.contains(k));
        assert!(west && plan.subject.contains("Palmyra"), "{plan:?}");
    }

    #[test]
    fn the_dominant_event_picks_the_subject() {
        let f = georgia_russia;
        let convoy = story(
            "2026-09-10",
            "RUSSIA REAR UNDER SUSTAINED ATTACK",
            vec![item("logistics_struck", "Russia", 62, &[("side", "Russia")])],
            "Caucasus",
            f(),
        );
        let plan = plan_scene(&convoy, "vs2", &rgw());
        assert_eq!(plan.kind, SceneKind::Convoy);
        assert!(plan.subject.contains("burn") || plan.subject.contains("wreck") || plan.subject.contains("strike"), "{plan:?}");
        let air = story("2026-09-10", "ONE-SIDED DAY IN THE AIR", vec![], "Caucasus", f());
        assert_eq!(plan_scene(&air, "vs2", &rgw()).kind, SceneKind::AirCombat);
        let sead = story(
            "2026-09-10",
            "RUSSIA AIR DEFENCES TAKE THE BRUNT",
            vec![item("sead", "Russia", 68, &[("side", "Russia")])],
            "Caucasus",
            f(),
        );
        assert_eq!(plan_scene(&sead, "vs2", &rgw()).kind, SceneKind::AirDefence);
        // A tally headline is pictured by what was lost.
        let mut tally = story(
            "2026-09-10",
            "COUNTING THE COST",
            vec![item("losses_tally", "the day's losses", 65, &[])],
            "Caucasus",
            f(),
        );
        tally.facts.losses.insert("LOGISTICS".into(), crate::news::LossTally { blue: 1, red: 9 });
        tally.facts.losses.insert("ARMOR".into(), crate::news::LossTally { blue: 2, red: 1 });
        let plan = plan_scene(&tally, "vs2", &rgw());
        assert_eq!(plan.kind, SceneKind::Convoy);
        // Red lost the trucks, so any trucks pictured are Russian.
        if plan.subject.contains("army truck") {
            assert!(plan.subject.contains("Ural") || plan.subject.contains("KamAZ"), "{plan:?}");
        }
        // No carriers in 2008.
        let cv = story("2026-09-10", "AIRCRAFT CARRIER GROUP MOVES UP", vec![], "Caucasus", f());
        assert_eq!(plan_scene(&cv, "vs2", &rgw()).kind, SceneKind::Naval);
        assert_eq!(plan_scene(&cv, "vs1", &syria()).kind, SceneKind::Carrier);
    }

    #[test]
    fn dcs_type_names_become_real_names() {
        let named = named_kit(
            "Two Su-25T and an F-16C_50 were lost; a Mi-8MTV2 and T-72B pushed on. \
             A tornado of fire swept the sector behind the lines.",
        );
        let names: Vec<&str> = named.iter().map(|(_, n)| *n).collect();
        for want in ["Su-25 attack jet", "F-16C fighter", "Mi-8 transport helicopter", "T-72B main battle tank"] {
            assert!(names.contains(&want), "{want} missing from {names:?}");
        }
        assert!(!names.iter().any(|n| n.starts_with("Tor (") || n.contains("Mi-24")), "{names:?}");
        // A story's own aircraft appear in the picture, by their real name.
        let mut seen = false;
        for n in 1..=20 {
            let mut d = story(&format!("2026-07-{n:02}"), "ONE-SIDED DAY IN THE AIR", vec![], "Caucasus", georgia_russia());
            d.body = vec!["Russian Su-25T strike aircraft ranged over the valley.".into()];
            let plan = plan_scene(&d, "vs2", &rgw());
            assert!(!plan.subject.contains("Su-25T"), "{plan:?}");
            seen |= plan.subject.contains("Su-25 attack jet");
        }
        assert!(seen);
    }

    #[test]
    fn articles_follow_the_sound() {
        assert_eq!(fix_articles("a M1A2 Abrams tank and a F-16C"), "an M1A2 Abrams tank and an F-16C");
        assert_eq!(fix_articles("a BMP-2 and a HEMTT truck"), "a BMP-2 and a HEMTT truck");
        assert_eq!(fix_articles("a S-300 near a Osa and a Ural-4320"), "an S-300 near an Osa and a Ural-4320");
        assert_eq!(fix_articles("a infantry section"), "an infantry section");
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
