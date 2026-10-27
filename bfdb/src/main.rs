// The warp route chain is very deep; the default limit is not enough once a
// `.recover` layer sits on top of it.
#![recursion_limit = "512"]

use anyhow::Result;
use bfprotocols::cfg::UnitTag;
use clap::Parser;
use db::{IntelCapture, IntelMarkup, SessionData, SessionEnd, StatsDb, WikiImage, WikiPage};
use futures::{SinkExt, StreamExt};
use netidx::{config::Config, path::Path as NetidxPath, subscriber::SubscriberBuilder};

use rust_embed::RustEmbed;
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::VecDeque,
    net::SocketAddr,
    path::PathBuf,
    sync::{Arc, Mutex},
};
use tokio::{sync::broadcast, task};
use uuid::Uuid;
use warp::{
    http::Method,
    reply::{self, Reply, Response},
    ws::{Message, WebSocket},
    Filter,
};

/// Build identity, embedded at compile time by `build.rs`.
pub const BUILD_GIT: &str = env!("BFNEXT_BUILD_GIT");
pub const BUILD_EPOCH: &str = env!("BFNEXT_BUILD_EPOCH");
pub const BUILD_VERSION: &str = env!("CARGO_PKG_VERSION");

/// `{ version, git, built }` for this binary. `built` is an RFC3339 UTC string.
fn build_info_json() -> serde_json::Value {
    let built = BUILD_EPOCH
        .parse::<i64>()
        .ok()
        .and_then(|s| chrono::DateTime::from_timestamp(s, 0))
        .map(|dt| dt.to_rfc3339_opts(chrono::SecondsFormat::Secs, true))
        .unwrap_or_else(|| "unknown".to_string());
    serde_json::json!({ "version": BUILD_VERSION, "git": BUILD_GIT, "built": built })
}

#[derive(RustEmbed)]
#[folder = "../bfweb/dist/"]
struct Assets;

#[derive(RustEmbed)]
#[folder = "../bfsite/dist/"]
struct SiteAssets;

mod geo;
mod news;
mod news_llm;
mod hq_strategist;
mod command;
mod groundfeed;
mod news_image;
mod db;
mod progression;
mod squadrons;
mod flightplans;
mod pilot_card;
mod db_id;
mod atc;
mod milatc;
mod gci;
mod voice;
mod intel;
mod instance;
mod range;
mod replay;
mod websec;
mod maint;
mod admin;
mod botlink;
mod engine;
mod http;
mod logs;
mod routes;
mod tasks;
#[cfg(test)]
mod route_tests;

// The handlers live in their own modules; main.rs keeps startup, the
// command line and the route assembly in `build_routes`. Everything is
// glob-imported here so the modules see one another as they did when this
// was one file.
use botlink::*;
use engine::*;
use http::*;
use logs::*;
use routes::{admin::*, auth::*, campaign::*, cockpit::*, command::*, intel::*, live::*, ops::*, pilots::*, static_files::*, wiki::*};
use tasks::*;

#[cfg(test)]
mod cfg_backup_tests {
    #[test]
    fn keeps_only_the_newest_config_backups() {
        let dir = std::env::temp_dir().join(format!("bfdb-cfgbak-{}", uuid::Uuid::new_v4()));
        std::fs::create_dir_all(&dir).unwrap();
        let cfg = dir.join("ODF_CFG");
        std::fs::write(&cfg, "{}").unwrap();
        for i in 0..5 {
            std::fs::write(dir.join(format!("ODF_CFG.bak.2026010100000{i}")), "").unwrap();
        }
        std::fs::write(dir.join("ODF_CFG.bak.notes"), "").unwrap();
        super::prune_cfg_backups(&cfg, 2);
        let mut left: Vec<String> = std::fs::read_dir(&dir)
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        left.sort();
        assert_eq!(left, ["ODF_CFG", "ODF_CFG.bak.20260101000003", "ODF_CFG.bak.20260101000004", "ODF_CFG.bak.notes"]);
        let _ = std::fs::remove_dir_all(&dir);
    }
}
mod snapshot;

use crate::db::InstanceState;
use crate::instance::{InstanceCfg, InstanceId, Registry, DEFAULT_INSTANCE};

/// One DCS server instance's runtime state, as handlers see it.
type Inst = Arc<InstanceState>;

/// Load stats and serve the Fowl Engine API
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The base path to find and subscribe to the stats (omit for offline mode)
    #[arg(short, long)]
    base: Option<NetidxPath>,
    /// Override the mission sortie name used for the LIVE engine subscriptions
    /// (RPC calls + engine log). bflib publishes under `<base>/<sortie>/...`
    /// where `<sortie>` is the DCS mission's Sortie field (the same name as the
    /// `<sortie>_CFG` file). Normally bfdb learns this from the stats stream,
    /// but a persisted campaign that was renamed keeps emitting the old name —
    /// set this to the real one (e.g. `ODFv2`) to point RPCs at the right path.
    /// Check with: `netidx resolver list <base>`.
    #[arg(long)]
    sortie: Option<String>,
    /// Path to a JSON file describing every DCS server instance this bfdb
    /// fronts (see `deploy/multi-instance.md` and `instances.sample.json`).
    /// Each entry carries its own netidx base, stats.jsonl/archive, engine
    /// CFG, UDP export port, SRS URL and GCI config, and every API route
    /// accepts `?instance=<id>` to pick one.
    ///
    /// Mutually exclusive with the single-server flags below (`--base`,
    /// `--stats-jsonl`, `--stats-dir`, `--sortie`, `--engine-config`,
    /// `--srs-url`, `--gci-config`, `--export-port`) -- pass either this or
    /// those, not both. Omit it and those flags synthesize one instance called
    /// "default", which is exactly the pre-multi-instance behaviour.
    #[arg(long)]
    instances: Option<PathBuf>,
    /// UDP port the DCS `Export.lua` live-unit feed arrives on (single-server
    /// mode only -- with `--instances`, set `export_port` per instance).
    #[arg(long, default_value_t = crate::instance::DEFAULT_EXPORT_PORT)]
    export_port: u16,
    /// The path to the database
    #[arg(short, long)]
    db: PathBuf,
    /// The certificate to use for TLS
    #[arg(short, long)]
    cert: Option<PathBuf>,
    /// The private key to use for TLS
    #[arg(short, long)]
    key: Option<PathBuf>,
    /// Path to the netidx-archive stats directory (e.g. "E:/Saved Games/DCS/Logs/stats")
    #[arg(long)]
    stats_dir: Option<PathBuf>,
    /// Path to stats JSONL file (e.g. "E:/Saved Games/DCS/Logs/stats.jsonl")
    #[arg(long)]
    stats_jsonl: Option<PathBuf>,
    /// No longer does anything (it never did: the filter was not applied
    /// anywhere). Still accepted so an old command line starts.
    #[arg(long, hide = true)]
    include: Option<String>,
    /// No longer does anything; see --include.
    #[arg(long, hide = true)]
    exclude: Option<String>,
    /// Store uploaded recon-intel (TARPS) photos as files in this directory
    /// instead of as blobs in the stats DB. bfdb creates it if missing and
    /// clears it on a campaign reset. Recommended for any real deployment --
    /// TARPS PNGs are large and a busy round can hold hundreds.
    #[arg(long)]
    intel_dir: Option<PathBuf>,
    /// The web address to listen on. Defaulted so the one-off maintenance
    /// modes (`--clear-sessions`, `--rebuild-stats`, `--merge-rounds`) don't
    /// need it -- the normal server run always gets it from procman anyway.
    /// Loopback by default: bfdb sits behind a reverse proxy, and exposing
    /// it on every interface (admin API included) has to be asked for.
    #[arg(long, default_value = "127.0.0.1:8880")]
    listen_address: SocketAddr,
    /// Discord OAuth2 client ID
    #[arg(long)]
    discord_client_id: Option<String>,
    /// Discord OAuth2 client secret
    #[arg(long)]
    discord_client_secret: Option<String>,
    /// Discord OAuth2 redirect URI (e.g. http://localhost:8080/api/auth/callback)
    #[arg(long)]
    discord_redirect_uri: Option<String>,
    /// Discord guild ID (for role-based admin check)
    #[arg(long)]
    discord_guild_id: Option<String>,
    /// Discord role ID that grants admin access
    #[arg(long)]
    discord_admin_role_id: Option<String>,
    /// Optional Discord role ID whose members get only the `ops` admin role:
    /// the OPS page, DCSServerBot start/stop, the engine config editor, perf
    /// and logs. The admin role above always has every role.
    #[arg(long)]
    discord_ops_role_id: Option<String>,
    /// Optional Discord role ID for the `content` admin role: editing the
    /// wiki and redrawing war-diary pictures.
    #[arg(long)]
    discord_content_role_id: Option<String>,
    /// Optional Discord role ID for the `moderation` admin role: bans, side
    /// switches, commander grants, recon intel purges.
    #[arg(long)]
    discord_moderation_role_id: Option<String>,
    /// Path to campaign config JSON (e.g. campaign.json).
    /// Served at GET /api/config so the web UI can brand itself.
    #[arg(long)]
    config: Option<PathBuf>,
    /// Optional separate address to serve the public website (bfsite) on.
    /// e.g. 0.0.0.0:8081 — if omitted, the site is still accessible at /site/ on the main port.
    #[arg(long)]
    site_address: Option<SocketAddr>,
    /// Local admin username for password-based login (alternative to Discord OAuth)
    #[arg(long)]
    admin_username: Option<String>,
    /// Local admin password for password-based login
    #[arg(long)]
    admin_password: Option<String>,
    /// SRS client list for the dashboard radio panel: either an http(s) URL
    /// to proxy (SRS's own HTTP_SERVER_ENABLED feature, which needs admin
    /// elevation to work reliably) or a local path to the JSON file SRS
    /// writes when CLIENT_EXPORT_ENABLED=True (CLIENT_EXPORT_FILE_PATH in
    /// SRS.cfg) -- the latter is simpler and needs no extra SRS config.
    #[arg(long)]
    srs_url: Option<String>,
    /// Path to the campaign engine config JSON that bflib loads (e.g. ODFv2_CFG).
    /// Enables the admin config editor at GET/POST /api/admin/cfg. Distinct from
    /// --config, which is just dashboard branding.
    #[arg(long)]
    engine_config: Option<PathBuf>,
    /// Path to the live GCI config JSON (see gci.sample.json in the repo root).
    /// Enables the proactive AWACS-style SRS callout system. Takes precedence
    /// over a `gci` key in --config. Requires --base (a live engine to query)
    /// and DCS-SR-ExternalAudio.exe (ships with SRS).
    #[arg(long = "gci-config")]
    gci_config: Option<PathBuf>,
    /// What the war diary calls the two sides, for a single-server bfdb. The
    /// `--instances` file has per-instance `blue_faction`/`red_faction` keys
    /// that do the same job; these are the equivalent for a bfdb started with
    /// the plain flags, which otherwise had no way to say it at all and left
    /// the diary reporting "Blue" and "Red".
    ///
    /// Left unset, the diary names each side after the country whose ground it
    /// started the campaign holding, worked out from the objectives' own
    /// positions -- so this is only needed to override that.
    #[arg(long = "blue-faction")]
    blue_faction: Option<String>,
    #[arg(long = "red-faction")]
    red_faction: Option<String>,
    /// The adjectival forms ("Syrian", "Georgian"). Default to the names.
    #[arg(long = "blue-adjective")]
    blue_adjective: Option<String>,
    #[arg(long = "red-adjective")]
    red_adjective: Option<String>,
    /// Origin(s) allowed to make cross-origin, credentialed API requests
    /// (e.g. https://dashboard.example.com). Repeat for multiple origins.
    /// Pass this when bfweb/bfsite are hosted separately from bfdb instead of
    /// embedded — without it, CORS defaults to same-origin-only behavior and
    /// session cookies use SameSite=Lax. Setting this switches cookies to
    /// SameSite=None; Secure, which requires bfdb to be served over TLS
    /// (--cert/--key), since browsers refuse SameSite=None without Secure.
    #[arg(long = "cors-origin")]
    cors_origins: Vec<String>,
    /// Write logs to this file instead of the console (in addition to the
    /// in-process log history/WebSocket stream used by the dashboard's Engine
    /// Log viewer). Lets the launcher's status console stay quiet and just
    /// show whether bfdb is running.
    #[arg(long = "log-file")]
    log_file: Option<PathBuf>,
    /// Base URL of DCSServerBot's RestAPI plugin, including its configured
    /// `prefix` (e.g. http://127.0.0.1:9876/stats). Discord account linking
    /// (dashboard "My Profile", the in-DCS cockpit UI, etc.) resolves a
    /// player's ucid by querying this bot endpoint's /getuser -- bfdb keeps
    /// no Discord-link database of its own; DCSServerBot's own /linkme +
    /// -linkme <token> flow is the only way to link. Leave unset to disable
    /// linking entirely (those features return "account not linked").
    #[arg(long = "dcsserverbot-url")]
    dcsserverbot_url: Option<String>,
    /// X-API-Key for DCSServerBot's RestAPI plugin (the `api_key` in its
    /// restapi.yaml). Required if --dcsserverbot-url is set.
    #[arg(long = "dcsserverbot-api-key")]
    dcsserverbot_api_key: Option<String>,
    /// Bearer token that unlocks the read-only plain-text log endpoints
    /// `GET /api/logs/engine` and `GET /api/logs/bfdb` (pass as
    /// `?token=<TOKEN>` or `Authorization: Bearer <TOKEN>`). These return the
    /// same in-memory backlog the dashboard's Engine Log viewer streams, but as
    /// plain text over a single GET so tooling (or a remote helper) can pull
    /// logs without a browser session. Leave unset to disable both endpoints
    /// entirely. Use a long random value and only expose it over TLS.
    #[arg(long = "log-read-token")]
    log_read_token: Option<String>,
    /// A token for `GET /api/metrics` alone, so a metrics scraper does not
    /// need the one that reads the logs. The log-read token keeps working
    /// there too. Also --metrics-token-file or $BFDB_METRICS_TOKEN.
    #[arg(long = "metrics-token")]
    metrics_token: Option<String>,
    /// File holding the --metrics-token (first line).
    #[arg(long = "metrics-token-file")]
    metrics_token_file: Option<PathBuf>,
    /// One-off maintenance: clear the `session` tree (per-round Cfg
    /// snapshot + perf history) and exit immediately without starting the
    /// server. Use this to recover from old Session records that predate a
    /// bincode-incompatible Cfg/Deployable change, which surface as
    /// "string is not valid utf8" errors from /api/admin/perf and
    /// /api/admin/banned. Round/kill/objective/pilot data is untouched.
    #[arg(long = "clear-sessions")]
    clear_sessions: bool,
    /// One-off maintenance: wipe every tree derived from replaying the stats
    /// archive (rounds, sessions, pilot stats, kills, sorties, deploys,
    /// objectives, trails) and rewind the replay cursor, then exit. The next
    /// normal startup re-ingests the whole archive from the beginning with the
    /// current idempotency guards in place -- use this to repair phantom
    /// duplicate sorties/kills/deploys (and the inflated counters they left
    /// behind) from stats that were redelivered before the guards existed.
    /// Auth sessions, Discord links, the ban list, wiki content and recon
    /// intel photos are all preserved. Requires the full archive under
    /// --stats-dir to still be present.
    #[arg(long = "rebuild-stats")]
    rebuild_stats: bool,
    /// One-off maintenance: collapse every round recorded under the given
    /// sortie into a single round -- re-keying all round-scoped data (pilot
    /// stats, kills, sorties, deploys, captures, objectives, trails, recon
    /// intel) onto the earliest one -- then exit. Repairs a campaign that an
    /// older bug split into dozens of near-identical "rounds" in the
    /// dashboard: bfdb used to close the live round on every restart whenever
    /// the sortie was named the same as the last path segment of --base (the
    /// documented default netidx_base is "/local/fowl/campaign" + sortie
    /// "campaign"), and the next SessionStart forked a fresh round. Summable
    /// counters are summed; the newest fork wins any other key collision.
    /// The replay cursor, auth sessions, Discord links, the ban list, wiki
    /// content and the stats archive are untouched. Pass the exact sortie
    /// name shown in GET /api/rounds (usually "campaign").
    #[arg(long = "merge-rounds", value_name = "SORTIE")]
    merge_rounds: Option<String>,
    /// Chat-completions endpoint that writes the daily war diary (see
    /// `news_llm.rs`). Any OpenAI-compatible URL works -- OpenAI, OpenRouter,
    /// Groq, or a local Ollama/llama.cpp/vLLM server -- as does the Anthropic
    /// messages API, which is detected from the URL. Defaults to OpenAI's
    /// endpoint when only a key is given. Falls back to $BFDB_NEWS_LLM_URL.
    ///
    /// Leave this and --news-llm-key unset and the diary still runs, writing
    /// from its template bank instead: same facts, flatter prose.
    #[arg(long = "news-llm-url")]
    news_llm_url: Option<String>,
    /// API key for --news-llm-url. Omit for a local endpoint that needs none.
    /// Falls back to $BFDB_NEWS_LLM_KEY, then $OPENAI_API_KEY.
    #[arg(long = "news-llm-key")]
    news_llm_key: Option<String>,
    /// File holding the --news-llm-key (first line), to keep it off the
    /// command line.
    #[arg(long = "news-llm-key-file")]
    news_llm_key_file: Option<PathBuf>,
    /// Model id for --news-llm-url (default gpt-4o-mini). The volume is a
    /// dozen or two short calls a day at the top end, so the cheap tier is the
    /// right one. Falls back to $BFDB_NEWS_LLM_MODEL.
    #[arg(long = "news-llm-model")]
    news_llm_model: Option<String>,
    /// Model endpoint for the HQ strategist (`hq_strategist.rs`), which sets
    /// each coalition's posture, main effort and priorities for the engine's
    /// theatre HQ. Same formats as --news-llm-url. Unset: the strategist uses
    /// the war diary's model; neither set: no strategist, and the HQ runs on
    /// its own rules.
    #[arg(long = "hq-llm-url")]
    hq_llm_url: Option<String>,
    /// API key for --hq-llm-url. Also --hq-llm-key-file or $BFDB_HQ_LLM_KEY.
    #[arg(long = "hq-llm-key")]
    hq_llm_key: Option<String>,
    /// File holding the --hq-llm-key (first line).
    #[arg(long = "hq-llm-key-file")]
    hq_llm_key_file: Option<PathBuf>,
    #[arg(long = "hq-llm-model")]
    hq_llm_model: Option<String>,
    /// Minutes between strategist reviews, per instance (two model calls each,
    /// one per side). Default 15. 0 turns the strategist off.
    #[arg(long = "hq-strategist-minutes", default_value_t = 15)]
    hq_strategist_minutes: u64,
    /// Who draws one picture per filed war-diary dispatch (see
    /// `news_image.rs`): `pollinations` (with an account key via
    /// --news-image-key; anonymous without one, rate limited and may
    /// watermark), `cloudflare` (Workers AI FLUX schnell, free tier) or `openai` (any
    /// OpenAI-compatible images endpoint, paid). Unset: cloudflare when
    /// --news-image-cf-account-id is set, openai when --news-image-url or
    /// --news-image-key is, else no pictures. Also $BFDB_NEWS_IMAGE_PROVIDER.
    #[arg(long = "news-image-provider")]
    news_image_provider: Option<String>,
    /// Endpoint override. openai: the images/generations URL (default
    /// OpenAI's; Together is https://api.together.xyz/v1/images/generations).
    /// cloudflare: a full run URL (e.g. through an AI Gateway). pollinations:
    /// the prompt base. Falls back to $BFDB_NEWS_IMAGE_URL.
    #[arg(long = "news-image-url")]
    news_image_url: Option<String>,
    /// The provider's credential: the Pollinations account key (`sk_...`),
    /// the Cloudflare API token (Workers AI permission), or the OpenAI key.
    /// Only ever sent as an Authorization header, and masked in logs. Also
    /// --news-image-key-file or $BFDB_NEWS_IMAGE_KEY. Never taken from
    /// $OPENAI_API_KEY, and never sent to the fallback.
    #[arg(long = "news-image-key")]
    news_image_key: Option<String>,
    /// Cloudflare account id (dashboard -> Workers AI -> "Use REST API").
    /// Also $BFDB_NEWS_IMAGE_CF_ACCOUNT_ID.
    #[arg(long = "news-image-cf-account-id")]
    news_image_cf_account_id: Option<String>,
    /// Cloudflare diffusion steps, 1-8 (default 6). More is sharper and costs
    /// more neurons.
    #[arg(long = "news-image-steps")]
    news_image_steps: Option<u32>,
    /// `pollinations`: when the provider's call fails (a Cloudflare quota
    /// included), try Pollinations once, keyless. Default off.
    #[arg(long = "news-image-fallback")]
    news_image_fallback: Option<String>,
    /// Seconds between pollinations.ai calls when --news-image-key is set
    /// (default 3). Anonymous calls stay at least 16 s apart regardless.
    #[arg(long = "news-image-min-interval")]
    news_image_min_interval: Option<u32>,
    /// File holding the --news-image-key (first line).
    #[arg(long = "news-image-key-file")]
    news_image_key_file: Option<PathBuf>,
    /// Model id: openai default gpt-image-1 (e.g. dall-e-3, or
    /// black-forest-labs/FLUX.1-schnell on Together); cloudflare default
    /// @cf/black-forest-labs/flux-1-schnell; pollinations default flux. Also
    /// $BFDB_NEWS_IMAGE_MODEL.
    #[arg(long = "news-image-model")]
    news_image_model: Option<String>,
    /// WIDTHxHEIGHT. openai default 1024x1024, the one size every model
    /// accepts (gpt-image-1 also 1536x1024, dall-e-3 1792x1024); pollinations
    /// default 1024x576. Cloudflare's FLUX schnell is always square.
    #[arg(long = "news-image-size")]
    news_image_size: Option<String>,
    /// openai only: passed through as `quality` when set (gpt-image-1: low /
    /// medium / high, its main price knob).
    #[arg(long = "news-image-quality")]
    news_image_quality: Option<String>,
    /// The look of the pictures, replacing the default (realistic war
    /// photojournalism). An instance's `news_image_style` overrides this. The
    /// safety rules -- no text, no real people, no flags, no gore -- are always
    /// appended.
    #[arg(long = "news-image-style")]
    news_image_style: Option<String>,
    /// Most image calls one instance may make per UTC day (admin regenerates
    /// included).
    #[arg(long = "news-image-daily-per-instance", default_value_t = news_image::DEFAULT_PER_INSTANCE_DAILY)]
    news_image_daily_per_instance: u32,
    /// Most image calls the whole process may make per UTC day.
    #[arg(long = "news-image-daily-global", default_value_t = news_image::DEFAULT_GLOBAL_DAILY)]
    news_image_daily_global: u32,
    /// Public base URL of the training range site (`range/` app). Used for
    /// the result links in the Discord embeds `/api/range/result/<id>/discord`
    /// hands the bot. Only matters when an instance has `kind: "range"`.
    #[arg(long = "range-site-url", default_value = "https://range.vectorstrike.org")]
    range_site_url: String,
    /// Public base URL this API is reachable at, for absolute links that leave
    /// the browser -- the result-card PNGs embedded in Discord messages.
    #[arg(long = "public-api-url", default_value = "https://api.vectorstrike.org")]
    public_api_url: String,
    /// Days to keep a training-range result's debrief track (the sampled
    /// geometry behind its card). The result itself is kept forever; after
    /// this its card is drawn without the track.
    #[arg(long = "range-track-days", default_value_t = 180)]
    range_track_days: u32,
    /// Where flight replays (processed Tacview recordings, see
    /// `src/replay`) are kept. Default: `<db>.replays` next to the database.
    /// Each instance's `tacview_dir` is the source.
    #[arg(long = "replay-dir")]
    replay_dir: Option<PathBuf>,
    /// Days to keep a flight replay before it is deleted.
    #[arg(long = "replay-days", default_value_t = 30)]
    replay_days: u32,
    /// Addresses allowed to use `POST /api/auth/local-login` (the password
    /// login DCSServerBot uses), as IPs or CIDR blocks; repeat for several.
    /// Default: loopback only -- the bot talks to bfdb on this machine
    /// (`api_url: http://localhost:8880`). A request that came through the
    /// reverse proxy counts as its real client, not as loopback. Before this
    /// flag existed any address could try the password; pass
    /// `--local-login-from 0.0.0.0/0 --local-login-from ::/0` to get that back.
    #[arg(long = "local-login-from", value_name = "IP[/PREFIX]")]
    local_login_from: Vec<websec::IpNet>,
    /// Send the dashboard page's Content-Security-Policy as an enforced
    /// header with tight connect-src/img-src lists, instead of the default
    /// report-only policy that allows any https: host. Check the browser
    /// console on the TACMAP, the replay and the 3D debrief before turning
    /// it on: a host missing from the list blanks that view.
    #[arg(long = "csp-enforce")]
    csp_enforce: bool,
    /// With --csp-enforce: one more source (e.g. https://tiles.example.org)
    /// for connect-src and img-src. Repeat for several.
    #[arg(long = "csp-extra-src", value_name = "SOURCE")]
    csp_extra_src: Vec<String>,
    /// GETs under /api/ one client IP may make per minute (a token bucket
    /// with a third of that as its burst). Generous: a dashboard tab polls a
    /// few dozen a minute. Loopback callers (the bot) are never metered.
    /// 0 turns it off.
    #[arg(long = "public-get-rate", default_value_t = 1800)]
    public_get_rate: u32,
    /// The stricter budget, per client IP per minute, for the GETs that cost
    /// real work: range clip cuts, PNG cards, whole Tacview recordings.
    /// 0 turns it off.
    #[arg(long = "heavy-get-rate", default_value_t = 120)]
    heavy_get_rate: u32,
    /// The cockpit overlay version /api/cockpit/plugin/version advertises,
    /// as is. Default: the version bfdb was built with, but only once its
    /// `cockpit-v<version>` installer is confirmed published on GitHub
    /// ("unknown" until then, which no overlay takes for an update).
    #[arg(long = "cockpit-plugin-version", value_name = "VERSION")]
    cockpit_plugin_version: Option<String>,
    /// A proxy in front of Caddy (a CDN, a load balancer) whose
    /// X-Forwarded-For entries are believed, as an IP or CIDR block; repeat
    /// for several. Without it, the client is the address Caddy saw -- which,
    /// behind a CDN, is the CDN's edge, so every visitor shares one rate-limit
    /// budget. Caddy must be told the same ranges (`trusted_proxies`, see
    /// deploy/backend/Caddyfile.vectorstrike) or it overwrites the header.
    #[arg(long = "trusted-proxy", value_name = "IP[/PREFIX]")]
    trusted_proxies: Vec<websec::IpNet>,
    /// With --trusted-proxy listing Cloudflare's ranges: take the client from
    /// `CF-Connecting-IP` when the request reached Caddy from one of them.
    /// Ignored for any request that did not.
    #[arg(long = "trust-cf-connecting-ip")]
    trust_cf_connecting_ip: bool,
    /// Read the local admin password from this file (first line) instead of
    /// the command line, where every process on the box can read it. Also
    /// read from $BFDB_ADMIN_PASSWORD when neither is given.
    #[arg(long = "admin-password-file")]
    admin_password_file: Option<PathBuf>,
    /// X-API-Key for the FowlEngine plugin's OPS API on the bot (the OPS page
    /// proxy and the bot-side log sources). Falls back to
    /// --dcsserverbot-api-key when unset. Also $BFDB_OPS_API_KEY.
    #[arg(long = "ops-api-key")]
    ops_api_key: Option<String>,
    /// File holding the DCSServerBot RestAPI key. Also $BFDB_DCSSERVERBOT_API_KEY.
    #[arg(long = "dcsserverbot-api-key-file")]
    dcsserverbot_api_key_file: Option<PathBuf>,
    /// File holding the Discord OAuth client secret. Also $BFDB_DISCORD_CLIENT_SECRET.
    #[arg(long = "discord-client-secret-file")]
    discord_client_secret_file: Option<PathBuf>,
    /// File holding the log-read token. Also $BFDB_LOG_READ_TOKEN.
    #[arg(long = "log-read-token-file")]
    log_read_token_file: Option<PathBuf>,
    /// Bearer token accepted by `POST /api/admin/shutdown` (which only ever
    /// answers direct loopback connections). An admin session cookie works
    /// too. Also $BFDB_SHUTDOWN_TOKEN.
    #[arg(long = "shutdown-token")]
    shutdown_token: Option<String>,
    /// Where database backups go. Default: `<db>.backups` next to the DB.
    #[arg(long = "backup-dir")]
    backup_dir: Option<PathBuf>,
    /// Hours between database backups; 0 turns them off.
    #[arg(long = "backup-interval-hours", default_value_t = 24)]
    backup_interval_hours: u64,
    /// Also back up what lives beside the database rather than in it: the
    /// recon photos (`<db>.intel`), flight replays (`<db>.replays`), range
    /// clip cache (`<db>.range-clips`), every instance's engine config file,
    /// and the --instances / --config files. Unchanged files are hard-linked
    /// to the previous backup's copy, so seven backups of a recording cost
    /// one. Off by default (replays can be large); see bfdb/BACKUPS.md.
    #[arg(long = "backup-files")]
    backup_files: bool,
    /// How many backups to keep; older ones are deleted.
    #[arg(long = "backup-keep", default_value_t = 7)]
    backup_keep: usize,
    /// Move the current database aside (kept as `<db>.damaged-<time>`) and
    /// restore the newest backup before starting. For a database sled can no
    /// longer open; never done automatically.
    #[arg(long = "restore-latest-backup")]
    restore_latest_backup: bool,
    /// Address the Export.lua UDP listener binds (every instance's port).
    /// Default loopback: the feed is unauthenticated god's-eye data and DCS
    /// normally runs on this machine. Set 0.0.0.0 (plus --export-secret) for a
    /// DCS server on another PC.
    #[arg(long = "export-bind", default_value = "127.0.0.1")]
    export_bind: std::net::IpAddr,
    /// Shared secret Export.lua must send (`BF_SECRET` in Export.lua) for its
    /// packets to be accepted. Unset accepts any packet, as before. Also
    /// $BFDB_EXPORT_SECRET.
    #[arg(long = "export-secret")]
    export_secret: Option<String>,
    /// The public snapshot the front ends fall back on (`--snapshot-*`).
    #[command(flatten)]
    snapshot: snapshot::SnapshotArgs,
}

/// A secret from, in order: its command-line flag, its `--...-file` flag, or
/// its environment variable. Keeping secrets off the command line matters on
/// Windows, where any local process can read another's arguments.
fn secret_from(flag: Option<String>, file: Option<&std::path::Path>, env: &str) -> Result<Option<String>> {
    if let Some(v) = flag.filter(|v| !v.is_empty()) {
        return Ok(Some(v));
    }
    if let Some(p) = file {
        let raw = std::fs::read_to_string(p)
            .map_err(|e| anyhow::anyhow!("reading secret file {}: {e}", p.display()))?;
        let v = raw.lines().next().unwrap_or("").trim().to_string();
        if v.is_empty() {
            anyhow::bail!("secret file {} is empty", p.display());
        }
        return Ok(Some(v));
    }
    Ok(std::env::var(env).ok().map(|v| v.trim().to_string()).filter(|v| !v.is_empty()))
}

/// `<db><suffix>`, the convention for the folders kept beside the database.
fn beside_db(db: &std::path::Path, suffix: &str) -> PathBuf {
    let mut p = db.as_os_str().to_os_string();
    p.push(suffix);
    PathBuf::from(p)
}

/// What `--backup-files` copies, named as it appears under a backup's `files/`.
fn backup_file_list(args: &Args, db: &StatsDb, intel_dir: &std::path::Path) -> Vec<(String, PathBuf)> {
    let mut v = vec![
        ("intel".to_string(), intel_dir.to_path_buf()),
        ("replays".to_string(), args.replay_dir.clone().unwrap_or_else(|| beside_db(&args.db, ".replays"))),
        ("range-clips".to_string(), beside_db(&args.db, ".range-clips")),
    ];
    for cfg in db.instances().all() {
        if let Some(p) = &cfg.engine_config {
            v.push((format!("engine-cfg/{}", cfg.id), p.clone()));
        }
    }
    if let Some(p) = &args.instances {
        v.push(("instances.json".into(), p.clone()));
    }
    if let Some(p) = &args.config {
        v.push(("campaign-config.json".into(), p.clone()));
    }
    v
}

/// Build the instance registry from the CLI: either `--instances <file>`, or
/// the legacy single-server flags synthesized into one instance called
/// `DEFAULT_INSTANCE` (which is also the id every pre-existing round in the DB
/// is treated as belonging to, so an upgrade is a no-op).
fn registry_from_args(args: &Args) -> Result<Registry> {
    let legacy_used = args.base.is_some()
        || args.stats_jsonl.is_some()
        || args.stats_dir.is_some()
        || args.sortie.is_some()
        || args.engine_config.is_some()
        || args.gci_config.is_some();
    match &args.instances {
        Some(path) => {
            if legacy_used {
                anyhow::bail!(
                    "--instances cannot be combined with the single-server flags \
                     (--base/--sortie/--stats-jsonl/--stats-dir/--engine-config/--gci-config); \
                     move those settings into the instances file"
                );
            }
            let reg = Registry::load(path)?;
            log::info!(
                "multi-instance mode: {} instance(s) from {}",
                reg.all().len(),
                path.display()
            );
            Ok(reg)
        }
        None => Ok(Registry::single(InstanceCfg {
            id: DEFAULT_INSTANCE.to_string(),
            label: None,
            base: args.base.clone(),
            netidx_config: None,
            sortie: args.sortie.clone(),
            stats_jsonl: args.stats_jsonl.clone(),
            stats_dir: args.stats_dir.clone(),
            export_port: Some(args.export_port),
            engine_config: args.engine_config.clone(),
            srs_url: args.srs_url.clone(),
            gci_config: args.gci_config.clone(),
            dcs_server_name: None,
            public: true,
            // Single-server mode gets the same per-campaign faction naming the
            // instances file has, off the command line. Unset, the diary names
            // the sides after the ground they hold.
            blue_faction: args.blue_faction.clone(),
            red_faction: args.red_faction.clone(),
            blue_adjective: args.blue_adjective.clone(),
            red_adjective: args.red_adjective.clone(),
            // Single-server mode takes the look from --news-image-style and
            // works the setting out from the scenario.
            news_image_setting: None,
            news_image_style: None,
            // A training range needs the instances file (`kind: "range"`).
            kind: Default::default(),
            range_jsonl: None,
            tacview_dir: None,
            replay_delay_secs: 0,
        })),
    }
}

/// Open the DB with no ingestion and no netidx, for the one-off maintenance
/// modes (`--clear-sessions`, `--rebuild-stats`, `--merge-rounds`). They only
/// touch trees, never a live engine, so a bare default instance is enough --
/// but it must be the *real* default id so the per-instance cursors it rewinds
/// are the ones the next normal start will read.
fn open_db_offline(args: &Args, db_path: PathBuf) -> Result<StatsDb> {
    let mut reg = registry_from_args(args)?;
    if args.instances.is_none() {
        reg = Registry::single(InstanceCfg {
            id: DEFAULT_INSTANCE.to_string(),
            label: None,
            base: None,
            netidx_config: None,
            sortie: None,
            stats_jsonl: None,
            stats_dir: None,
            export_port: None,
            engine_config: None,
            srs_url: None,
            gci_config: None,
            dcs_server_name: None,
            public: true,
            // The offline maintenance modes never write a dispatch, so the
            // faction names do not matter here.
            blue_faction: None,
            red_faction: None,
            blue_adjective: None,
            red_adjective: None,
            news_image_setting: None,
            news_image_style: None,
            kind: Default::default(),
            range_jsonl: None,
            tacview_dir: None,
            replay_delay_secs: 0,
        });
    }
    StatsDb::new(&std::collections::HashMap::new(), db_path, reg)
}

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<()> {
    let mut args = Args::parse();
    let backup_dir = args
        .backup_dir
        .clone()
        .unwrap_or_else(|| maint::default_backup_dir(&args.db));

    // Before anything opens the database -- the maintenance modes included.
    if args.restore_latest_backup {
        match maint::restore_latest(&args.db, &backup_dir) {
            Ok(from) => eprintln!("restored {} from backup {}", args.db.display(), from.display()),
            Err(e) => {
                eprintln!("--restore-latest-backup failed: {e:#}");
                std::process::exit(1);
            }
        }
    }

    if args.trust_cf_connecting_ip && args.trusted_proxies.is_empty() {
        eprintln!("--trust-cf-connecting-ip needs --trusted-proxy (Cloudflare's address ranges); ignoring it");
    }
    websec::set_trusted_proxies(websec::TrustedProxies::new(
        args.trusted_proxies.clone(),
        args.trust_cf_connecting_ip,
    ));

    websec::set_spa_csp(args.csp_enforce, &args.csp_extra_src);
    if let Some(v) = args.cockpit_plugin_version.clone().filter(|v| !v.trim().is_empty()) {
        let _ = COCKPIT_VERSION_OVERRIDE.set(v.trim().to_string());
    }

    // Secrets may come from a file or the environment instead of argv. On
    // the command line they still work, with a warning: on Windows any local
    // process can read another's arguments.
    let secrets_on_argv: Vec<&'static str> = [
        ("--admin-password", args.admin_password.is_some()),
        ("--dcsserverbot-api-key", args.dcsserverbot_api_key.is_some()),
        ("--discord-client-secret", args.discord_client_secret.is_some()),
        ("--log-read-token", args.log_read_token.is_some()),
        ("--metrics-token", args.metrics_token.is_some()),
        ("--shutdown-token", args.shutdown_token.is_some()),
        ("--ops-api-key", args.ops_api_key.is_some()),
        ("--export-secret", args.export_secret.is_some()),
        ("--news-image-key", args.news_image_key.is_some()),
        ("--news-llm-key", args.news_llm_key.is_some()),
        ("--hq-llm-key", args.hq_llm_key.is_some()),
    ]
    .into_iter()
    .filter(|(_, on)| *on)
    .map(|(n, _)| n)
    .collect();
    args.news_llm_key = secret_from(
        args.news_llm_key.take(),
        args.news_llm_key_file.as_deref(),
        "BFDB_NEWS_LLM_KEY",
    )?;
    args.hq_llm_key = secret_from(args.hq_llm_key.take(), args.hq_llm_key_file.as_deref(), "BFDB_HQ_LLM_KEY")?;
    args.metrics_token = secret_from(
        args.metrics_token.take(),
        args.metrics_token_file.as_deref(),
        "BFDB_METRICS_TOKEN",
    )?;
    args.admin_password = secret_from(
        args.admin_password.take(),
        args.admin_password_file.as_deref(),
        "BFDB_ADMIN_PASSWORD",
    )?;
    args.dcsserverbot_api_key = secret_from(
        args.dcsserverbot_api_key.take(),
        args.dcsserverbot_api_key_file.as_deref(),
        "BFDB_DCSSERVERBOT_API_KEY",
    )?;
    args.discord_client_secret = secret_from(
        args.discord_client_secret.take(),
        args.discord_client_secret_file.as_deref(),
        "BFDB_DISCORD_CLIENT_SECRET",
    )?;
    args.log_read_token = secret_from(
        args.log_read_token.take(),
        args.log_read_token_file.as_deref(),
        "BFDB_LOG_READ_TOKEN",
    )?;
    args.shutdown_token = secret_from(args.shutdown_token.take(), None, "BFDB_SHUTDOWN_TOKEN")?;
    args.ops_api_key = secret_from(args.ops_api_key.take(), None, "BFDB_OPS_API_KEY")?;
    args.export_secret = secret_from(args.export_secret.take(), None, "BFDB_EXPORT_SECRET")?;
    args.news_image_key = secret_from(
        args.news_image_key.take(),
        args.news_image_key_file.as_deref(),
        "BFDB_NEWS_IMAGE_KEY",
    )?;

    if args.clear_sessions {
        let db = open_db_offline(&args, args.db.clone())?;
        db.clear_stale_sessions()?;
        println!("cleared the session tree -- perf history and cfg-derived ban entries are gone, round/kill/objective/pilot data is untouched");
        return Ok(());
    }

    if args.rebuild_stats {
        if args.instances.is_none() && args.stats_dir.is_none() && args.stats_jsonl.is_none() {
            eprintln!("--rebuild-stats needs --stats-dir (or --stats-jsonl), or --instances, present so the next start can re-ingest the archive");
            std::process::exit(1);
        }
        let db = open_db_offline(&args, args.db.clone())?;
        db.rebuild_stats_from_archive()?;
        println!(
            "rebuilt: wiped every archive-derived tree and rewound the replay cursor. \
             Restart bfdb normally to re-ingest -- auth sessions, Discord links, bans, \
             wiki content and recon intel are preserved."
        );
        return Ok(());
    }

    if let Some(sortie) = args.merge_rounds.as_deref() {
        let db = open_db_offline(&args, args.db.clone())?;
        let msg = db.merge_rounds(sortie)?;
        println!("{msg}");
        return Ok(());
    }

    // ── Broadcast logger: forwards to env_logger + WebSocket stream ───────
    let (log_tx, _) = broadcast::channel::<String>(512);
    let log_history: LogHistory = Arc::new(Mutex::new(VecDeque::new()));
    {
        let mut builder = env_logger::Builder::from_default_env();
        // Default to Info when RUST_LOG is not set so the log viewer has useful output
        if std::env::var("RUST_LOG").is_err() {
            builder.filter_level(log::LevelFilter::Info);
            // netidx's subscriber logs a WARN on every resubscription attempt for
            // a path that isn't published yet, and retries roughly once a second
            // forever -- e.g. an engine RPC the mission hasn't registered. That's
            // tens of thousands of identical lines over a campaign; keep only the
            // errors from that module.
            builder.filter_module("netidx::subscriber", log::LevelFilter::Error);
        }
        if let Some(path) = &args.log_file {
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            // Rotate the previous run's log aside on startup (same convention as
            // bflib): foo.log -> foo<timestamp>.log, then start a fresh file, so
            // the active log doesn't grow unbounded across restarts.
            rotate_log(path);
            let file = std::fs::OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)
                .map_err(|e| anyhow::anyhow!("opening --log-file {path:?}: {e}"))?;
            builder.target(env_logger::Target::Pipe(Box::new(file)));
        }
        let env_log = builder.build();
        let max_level = env_log.filter();
        let logger = BroadcastLogger {
            inner:   env_log,
            tx:      log_tx.clone(),
            history: log_history.clone(),
        };
        log::set_boxed_logger(Box::new(logger)).expect("logger already set");
        log::set_max_level(max_level);
    }

    log::info!(
        "bfdb v{} git:{} built:{}",
        BUILD_VERSION,
        BUILD_GIT,
        build_info_json()["built"].as_str().unwrap_or("unknown")
    );
    if args.include.is_some() || args.exclude.is_some() {
        log::warn!("--include/--exclude do nothing and will be removed; drop them from the command line");
    }
    for flag in &secrets_on_argv {
        log::warn!(
            "{flag} was given on the command line, where other local processes can read it -- \
             use {flag}-file or its BFDB_* environment variable instead"
        );
    }
    let registry = registry_from_args(&args)?;
    // True when *any* instance has a live engine to talk to.
    let has_engine = registry.all().iter().any(|i| i.base.is_some());
    let db = {
        // One subscriber per netidx client config: normally just the default
        // one, plus one for each instance that reaches its engine through a
        // resolver of its own (a DCS server on another PC).
        use anyhow::Context as _;
        let mut subscribers = std::collections::HashMap::new();
        for cfg in registry.all().iter().filter(|i| i.base.is_some()) {
            if subscribers.contains_key(&cfg.netidx_config) {
                continue;
            }
            let ncfg = match &cfg.netidx_config {
                None => Config::load_default()?,
                Some(p) => Config::load(p).with_context(|| {
                    format!("instance {:?}: loading netidx config {}", cfg.id, p.display())
                })?,
            };
            if let Some(p) = &cfg.netidx_config {
                log::info!("instance {:?}: netidx via {}", cfg.id, p.display());
            }
            subscribers.insert(cfg.netidx_config.clone(), SubscriberBuilder::new().config(ncfg).build()?);
        }
        if !has_engine {
            log::info!("Running in offline mode (no instance has a netidx base, Netidx disabled)");
        }
        // sled reports some on-disk damage by panicking inside its recovery
        // (an assertion over its segment accounting) rather than returning
        // an error: the process died with code 101 and none of the advice
        // below was ever printed (Oct 8). Catch it so it is.
        let opened = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            StatsDb::new(&subscribers, args.db.clone(), registry)
        }));
        let opened = match opened {
            Ok(r) => r,
            Err(p) => {
                let what = p
                    .downcast_ref::<&str>()
                    .map(|s| s.to_string())
                    .or_else(|| p.downcast_ref::<std::string::String>().cloned())
                    .unwrap_or_else(|| "a panic".into());
                let first = what.lines().next().unwrap_or("").to_string();
                Err(anyhow::anyhow!("the database files are damaged (sled panicked while opening them: {first})"))
            }
        };
        match opened {
            Ok(db) => db,
            Err(e) => {
                // Say what to do about it, not just what sled said.
                maint::explain_open_failure(&args.db, &backup_dir, &e);
                return Err(e);
            }
        }
    };
    // Recon photos go to disk by default now (`<db>.intel`): hundreds of
    // multi-megabyte PNGs per round do not belong in sled, where they bloat
    // every compaction and every backup. Photos already stored in the DB stay
    // there and remain readable.
    let intel_dir = args.intel_dir.clone().unwrap_or_else(|| {
        let mut s = args.db.as_os_str().to_os_string();
        s.push(".intel");
        PathBuf::from(s)
    });
    db.set_intel_dir(Some(intel_dir.clone()))?;
    log::info!("recon intel photos stored on disk at {}", intel_dir.display());

    // ── Durability: signals, backups, housekeeping ───────────────────────
    let _ = DB_PATH.set(args.db.clone());
    let shutdown = maint::Shutdown::default();
    maint::spawn_signal_handlers(shutdown.clone());
    if args.backup_interval_hours > 0 {
        maint::spawn_backups(
            db.clone(),
            maint::BackupCfg {
                dir: backup_dir.clone(),
                every: std::time::Duration::from_secs(args.backup_interval_hours.saturating_mul(3600)),
                keep: args.backup_keep,
                files: if args.backup_files { backup_file_list(&args, &db, &intel_dir) } else { Vec::new() },
            },
        );
    } else {
        log::warn!("database backups are OFF (--backup-interval-hours 0)");
    }
    maint::spawn_sweeps(db.clone());

    // Push the coalition recon markup onto the in-game F10 map every ~15s.
    // Fire-and-forget: the engine may be unreachable, and a failed push just
    // retries next tick. Only runs when there's a live engine (--base).
    if has_engine {
        for cfg in db.instances().all() {
        // Recon markup is a campaign thing; a training range has none.
        if cfg.base.is_none() || cfg.is_range() {
            continue;
        }
        let intel_db = db.clone();
        let intel_inst = db.state(&Arc::from(cfg.id.as_str()));
        tokio::spawn(async move {
            let mut tick = tokio::time::interval(std::time::Duration::from_secs(15));
            loop {
                tick.tick().await;
                let payload = match task::block_in_place(|| intel_db.intel_marks_payload(&intel_inst.id)) {
                    Ok(Some(p)) => p,
                    Ok(None) => continue,
                    Err(e) => {
                        log::warn!("intel-marks: building payload failed: {e:?}");
                        continue;
                    }
                };
                let _ = tokio::time::timeout(
                    std::time::Duration::from_secs(3),
                    call_engine_rpc_str(
                        &intel_db,
                        &intel_inst,
                        "intel-marks",
                        vec![("data", netidx::publisher::Value::from(payload))],
                    ),
                )
                .await;
            }
        });
        }
    }

    let auth_cfg: Option<AuthConfig> = match (
        args.discord_client_id,
        args.discord_client_secret,
        args.discord_redirect_uri,
        args.discord_guild_id,
        args.discord_admin_role_id,
    ) {
        (Some(id), Some(secret), Some(uri), Some(guild), Some(role)) => {
            log::info!("Discord OAuth enabled (guild={guild}, admin_role={role})");
            Some(AuthConfig {
                client_id:     id,
                client_secret: secret,
                redirect_uri:  uri,
                guild_id:      guild,
                admin_role_id: role,
                roles:         admin::DiscordRoles {
                    ops: args.discord_ops_role_id.clone(),
                    content: args.discord_content_role_id.clone(),
                    moderation: args.discord_moderation_role_id.clone(),
                },
            })
        }
        _ => {
            log::info!("Discord OAuth disabled (pass --discord-* flags to enable)");
            None
        }
    };

    let local_admin_cfg: Option<LocalAdminConfig> = match (args.admin_username, args.admin_password) {
        (Some(u), Some(p)) => {
            log::info!("Local admin login enabled (username={u})");
            Some(LocalAdminConfig { username: u, password: p })
        }
        _ => {
            log::info!("Local admin login disabled (pass --admin-username and --admin-password to enable)");
            None
        }
    };

    let bot_link_cfg: Arc<Option<BotLinkConfig>> = Arc::new(match (args.dcsserverbot_url, args.dcsserverbot_api_key) {
        (Some(base_url), Some(api_key)) => {
            // Tolerate a trailing slash on --dcsserverbot-url -- otherwise
            // e.g. "http://host:9876/stats/" + "/servers" produces a
            // double-slash path that most web frameworks 404 on, silently
            // breaking the integration rather than erroring loudly.
            let base_url = base_url.trim_end_matches('/').to_string();
            log::info!("Discord account linking via DCSServerBot enabled ({base_url})");
            let ops_key = args.ops_api_key.clone().unwrap_or_else(|| api_key.clone());
            Some(BotLinkConfig { base_url, api_key, ops_key })
        }
        _ => {
            log::info!("Discord account linking disabled (pass --dcsserverbot-url and --dcsserverbot-api-key to enable)");
            None
        }
    });

    // ── Training range (kind: "range" instances) ─────────────────────────
    // Ingests each range instance's range.jsonl and serves /api/range/*. The
    // routes exist either way (they answer 400 when no range is configured).
    let range_clip_dir = {
        let mut p = args.db.clone().into_os_string();
        p.push(".range-clips");
        PathBuf::from(p)
    };
    let range_ctx = range::RangeCtx::new(
        db.clone(),
        bot_link_cfg.clone(),
        &args.range_site_url,
        &args.public_api_url,
        range_clip_dir,
    )?;
    range::spawn_tasks(&range_ctx, args.range_track_days);
    let range_routes = range::api::routes(range_ctx.clone());

    // ── Flight replay (each instance's Tacview recordings) ───────────────
    let replay_dir = args.replay_dir.clone().unwrap_or_else(|| {
        let mut p = args.db.clone().into_os_string();
        p.push(".replays");
        PathBuf::from(p)
    });
    let replay_ctx = replay::ReplayCtx::new(db.clone(), replay_dir, args.replay_days.max(1))?;
    replay::spawn_tasks(&replay_ctx);
    let _ = range_ctx.replay.set(replay_ctx.clone());
    let replay_routes = replay::api::routes(replay_ctx);

    // ── Load campaign config JSON (served at /api/config) ────────────────
    let (campaign_json, srs_url_from_cfg, gci_cfg): (Arc<String>, Option<String>, Option<gci::GciConfig>) = match &args.config {
        Some(path) => {
            match std::fs::read_to_string(path) {
                Ok(raw) => match serde_json::from_str::<serde_json::Value>(&raw) {
                    Ok(v) => {
                        log::info!("Campaign config loaded from {:?}", path);
                        let srs = v.get("srsUrl").and_then(|u| u.as_str()).filter(|s| !s.is_empty()).map(|s| s.to_string());
                        let gci = gci::from_campaign_json(&v);
                        // Served publicly, so only the branding keys.
                        (Arc::new(public_campaign_config(&raw)), srs, gci)
                    }
                    Err(_) => {
                        log::warn!("--config file is not valid JSON, using empty config");
                        (Arc::new("{}".to_string()), None, None)
                    }
                },
                Err(e) => {
                    // Not an error worth failing startup over -- bfsystem.ps1
                    // always passes --config unconditionally, so a missing
                    // campaign.json (never created, or a fresh setup) just
                    // means "use default dashboard branding", same as
                    // --config being omitted entirely below.
                    log::warn!("Could not read --config {path:?}: {e} -- using default campaign branding");
                    (Arc::new("{}".to_string()), None, None)
                }
            }
        }
        None => {
            log::info!("No --config file specified; /api/config will return {{}}");
            (Arc::new("{}".to_string()), None, None)
        }
    };
    // CLI --srs-url takes precedence over campaign.json srsUrl
    let effective_srs_url = args.srs_url.clone().or(srs_url_from_cfg);
    if let Some(ref u) = effective_srs_url {
        let kind = if u.starts_with("http://") || u.starts_with("https://") { "proxying" } else { "reading" };
        log::info!("SRS client list enabled, {kind} {u}");
    }

    // A dedicated --gci-config file wins over a `gci` key in --config, same
    // precedence rule as --srs-url vs campaign.json srsUrl above.
    let gci_cfg: Option<gci::GciConfig> = args
        .gci_config
        .as_ref()
        .and_then(|p| gci::from_file(p))
        .or(gci_cfg);

    for cfg in db.instances().all() {
        match &cfg.engine_config {
            Some(p) => log::info!("[{}] engine config editor enabled → {p:?}", cfg.id),
            None => log::info!(
                "[{}] no engine config set; the admin config editor is disabled for this instance",
                cfg.id
            ),
        }
    }

    // ── Live telemetry, per DCS server instance ────────────────────────
    // Each instance gets its own Export.lua UDP listener (on its own port),
    // its own live-unit broadcast, and its own fog-of-war tactical cache. The
    // websocket routes resolve `?instance=` and pick the matching one, so two
    // servers never bleed contacts into each other's scope.
    // The war diary's writer, shared by every instance's generator. None when
    // no endpoint is configured, in which case the diary renders from its own
    // template bank -- see `news_llm.rs`.
    let news_writer = news_llm::WriterCfg::resolve(
        args.news_llm_url.clone(),
        args.news_llm_key.clone(),
        args.news_llm_model.clone(),
    );
    match &news_writer {
        Some(w) => log::info!("news: dispatches written by {} at {}", w.model, w.url),
        None => log::info!(
            "news: no --news-llm-url/--news-llm-key -- the war diary will use its template bank"
        ),
    }
    // The HQ strategist's model: its own flags, else the diary's. See
    // `hq_strategist.rs`.
    let hq_writer = if args.hq_strategist_minutes == 0 {
        None
    } else if args.hq_llm_url.is_some() || args.hq_llm_key.is_some() {
        news_llm::WriterCfg::resolve(args.hq_llm_url.clone(), args.hq_llm_key.clone(), args.hq_llm_model.clone())
    } else {
        news_writer.clone().map(|mut w| {
            if let Some(m) = args.hq_llm_model.clone() {
                w.model = m;
            }
            w
        })
    };
    match &hq_writer {
        Some(w) => log::info!(
            "hq strategist: {} at {}, every {} min",
            w.model,
            w.url,
            args.hq_strategist_minutes
        ),
        None => log::info!("hq strategist: off -- the engine's HQ runs on its own rules"),
    }
    // Pictures for the diary. Off unless named -- see `news_image.rs`.
    // A misconfigured provider turns pictures off with an error rather than
    // stopping bfdb: they are decoration, the rest of the server is not.
    let news_images = match news_image::ImageCfg::resolve(news_image::ImageArgs {
        provider: args.news_image_provider.clone(),
        url: args.news_image_url.clone(),
        key: args.news_image_key.clone(),
        model: args.news_image_model.clone(),
        size: args.news_image_size.clone(),
        quality: args.news_image_quality.clone(),
        style: args.news_image_style.clone(),
        cf_account_id: args.news_image_cf_account_id.clone(),
        steps: args.news_image_steps,
        fallback: args.news_image_fallback.clone(),
        min_interval_secs: args.news_image_min_interval,
        per_instance_daily: args.news_image_daily_per_instance,
        global_daily: args.news_image_daily_global,
    }) {
        Ok(c) => c.map(Arc::new),
        Err(e) => {
            log::error!("news: dispatch pictures are OFF -- {e}");
            None
        }
    };
    match &news_images {
        Some(c) => log::info!(
            "news: dispatch pictures by {} ({}{}, at most {}/instance and {} in all per day)",
            c.label(),
            c.size,
            c.fallback.map(|f| format!(", falling back to {}", f.name())).unwrap_or_default(),
            c.per_instance_daily,
            c.global_daily
        ),
        None => log::info!("news: no --news-image-provider -- dispatches run without pictures"),
    }
    // Public snapshot for the front ends to fall back on while this box is
    // down. Off unless --snapshot-endpoint or --snapshot-dir (snapshot.rs).
    snapshot::spawn(&args.snapshot, db.clone(), bot_link_cfg.clone(), news_images.clone());

    let export_listen = ExportListenCfg {
        bind: args.export_bind,
        secret: args.export_secret.clone().map(Arc::new),
    };
    let live_map: LiveMap = {
        let mut m = std::collections::HashMap::new();
        for cfg in db.instances().all() {
            let id: InstanceId = Arc::from(cfg.id.as_str());
            let inst = db.state(&id);
            let live: LiveState = Arc::new(tokio::sync::RwLock::new((0.0, Vec::new(), Vec::new())));
            let (live_tx, _) = broadcast::channel::<String>(64);
            let tac: TacState = Arc::new(tokio::sync::RwLock::new(TacCache::default()));
            let ground: groundfeed::GroundState = Default::default();

            match cfg.export_port {
                Some(port) => {
                    tokio::spawn(udp_export_listener(
                        live.clone(),
                        live_tx.clone(),
                        db.clone(),
                        inst.clone(),
                        port,
                        export_listen.clone(),
                    ));
                }
                None => log::info!(
                    "[{}] no export_port configured -- the live unit feed (/ws/units) is off for this instance",
                    cfg.id
                ),
            }
            // Campaign-only: a training range (bfrange) publishes none of
            // query-tacmap / query-unitdb, and has no war to write a diary
            // about. Its live state is polled on demand by `range::api`.
            if cfg.base.is_some() && !cfg.is_range() {
                tokio::spawn(tacmap_poller(db.clone(), inst.clone(), tac.clone()));
                tokio::spawn(groundfeed::poller(db.clone(), inst.clone(), ground.clone()));
                tokio::spawn(command::pusher(db.clone(), inst.clone()));
                tokio::spawn(unitdb_refresher(db.clone(), inst.clone()));
                tokio::spawn(news_generator(
                    db.clone(),
                    inst.clone(),
                    news_writer.clone(),
                    news_images.clone(),
                ));
                if let Some(w) = hq_writer.clone() {
                    tokio::spawn(hq_strategist::run(
                        db.clone(),
                        inst.clone(),
                        w,
                        std::time::Duration::from_secs(args.hq_strategist_minutes.max(1) * 60),
                    ));
                }
            }
            m.insert(id, InstanceLive { live, live_tx, tac, ground });
        }
        Arc::new(m)
    };

    // ── Live GCI: proactive AWACS-style SRS callouts, per instance ────────
    // A broadcast stream of every GCI call, for the dashboard transcript panel
    // (/ws/gci) and the last-N snapshot (/api/gci/transcript). Each instance
    // runs its own GCI (its own SRS server, frequencies and callsigns) and so
    // gets its own transcript channel.
    let mut gci_map: std::collections::HashMap<InstanceId, (broadcast::Sender<String>, LogHistory)> =
        std::collections::HashMap::new();
    for cfg in db.instances().all() {
        let id: InstanceId = Arc::from(cfg.id.as_str());
        let inst = db.state(&id);
        let (tx, _) = broadcast::channel::<String>(128);
        let hist: LogHistory = Arc::new(Mutex::new(VecDeque::new()));
        // A dedicated per-instance gci_config wins; otherwise the single-server
        // --gci-config / campaign.json `gci` block applies to the default
        // instance only (there is only one of it).
        let cfg_for_inst = cfg
            .gci_config
            .as_ref()
            .and_then(|p| gci::from_file(p))
            .or_else(|| {
                // never a range's: it has no GCI picture to serve
                if id == *db.instances().default_id() && !cfg.is_range() {
                    gci_cfg.clone()
                } else {
                    None
                }
            });
        match (cfg.base.is_some(), cfg_for_inst) {
            // A range has no GCI picture or campaign towers; its voice
            // service is the military ATC.
            (true, Some(c)) if cfg.is_range() => {
                match c.range_atc.clone() {
                    Some(r) if r.enabled => {
                        log::info!("[{}] range ATC enabled", cfg.id);
                        tokio::spawn(milatc::run(db.clone(), inst, c.clone(), r));
                    }
                    _ => log::info!("[{}] range instance has a gci_config but no rangeAtc block", cfg.id),
                }
            }
            (true, Some(c)) => {
                log::info!("[{}] live GCI enabled", cfg.id);
                // ATC rides the same config file and the same voice bus, on
                // its own SRS clients so a busy tower can never delay a threat
                // call on the GCI net.
                if let Some(atc_cfg) = c.atc.clone() {
                    let atc_transcript: atc::AtcTranscript = Default::default();
                    tokio::spawn(atc::run(
                        db.clone(),
                        inst.clone(),
                        c.clone(),
                        atc_cfg,
                        atc_transcript,
                    ));
                }
                tokio::spawn(gci::run(db.clone(), inst, c, tx.clone(), hist.clone()));
            }
            (false, Some(_)) => {
                log::warn!(
                    "[{}] GCI configured but disabled: this instance has no netidx base (no live engine to query)",
                    cfg.id
                );
            }
            _ => {}
        }
        gci_map.insert(id, (tx, hist));
    }
    let gci_map = Arc::new(gci_map);

    let routes = build_routes(RouteCtx {
        db: db.clone(),
        bot_link_cfg: bot_link_cfg.clone(),
        auth_cfg,
        local_admin_cfg,
        live_map,
        gci_map,
        log_tx,
        log_history,
        range_routes,
        replay_routes,
        campaign_json,
        effective_srs_url,
        news_images,
        shutdown: shutdown.clone(),
        cors_origins: args.cors_origins.clone(),
        public_api_url: args.public_api_url.clone(),
        listen_port: args.listen_address.port(),
        local_login_from: args.local_login_from.clone(),
        log_read_token: args.log_read_token.clone(),
        metrics_token: args.metrics_token.clone(),
        shutdown_token: args.shutdown_token.clone(),
        get_limits: websec::GetLimits { per_min: args.public_get_rate, heavy_per_min: args.heavy_get_rate },
    });

    log::info!("API server listening on http://{}", args.listen_address);

    // Optional separate site server
    if let Some(site_addr) = args.site_address {
        let site_cors = warp::cors()
            .allow_any_origin()
            .allow_methods(&[Method::GET])
            .build();
        let site_only = warp::get()
            .and(warp::path::tail())
            .map(|tail: warp::path::Tail| serve_site_asset(tail.as_str()))
            .with(site_cors);
        log::info!("Site server listening on http://{}", site_addr);
        tokio::spawn(warp::serve(site_only).run(site_addr));
    }

    // Serve until a shutdown is requested, then flush and exit. Not warp's
    // own graceful shutdown: it waits for every open connection to finish,
    // and dashboard WebSockets never do, so it would wait until the
    // supervisor gave up and hard-killed us -- the very thing this avoids.
    let server = async {
        match (&args.cert, &args.key) {
            (_, None) | (None, _) => warp::serve(routes).run(args.listen_address).await,
            (Some(cert), Some(key)) => {
                warp::serve(routes)
                    .tls()
                    .cert_path(cert)
                    .key_path(key)
                    .run(args.listen_address)
                    .await
            }
        }
    };
    tokio::select! {
        _ = server => log::warn!("web server stopped"),
        _ = shutdown.wait() => (),
    }
    let flush_db = db.clone();
    let _ = task::spawn_blocking(move || maint::flush_db(&flush_db)).await;
    log::info!("bfdb exiting");
    log::logger().flush();
    std::process::exit(0)
}

/// Everything the HTTP layer needs, gathered by `main` (which also starts the
/// background tasks that feed the live maps). Split out so the whole route
/// chain can be built -- and exercised with `warp::test` -- without a DCS
/// server, a netidx resolver or a socket.
struct RouteCtx {
    db: StatsDb,
    bot_link_cfg: Arc<Option<BotLinkConfig>>,
    auth_cfg: Option<AuthConfig>,
    local_admin_cfg: Option<LocalAdminConfig>,
    live_map: LiveMap,
    gci_map: GciMap,
    log_tx: broadcast::Sender<String>,
    log_history: LogHistory,
    range_routes: warp::filters::BoxedFilter<(Response,)>,
    replay_routes: warp::filters::BoxedFilter<(Response,)>,
    campaign_json: Arc<String>,
    effective_srs_url: Option<String>,
    news_images: Option<Arc<news_image::ImageCfg>>,
    shutdown: maint::Shutdown,
    cors_origins: Vec<String>,
    public_api_url: String,
    listen_port: u16,
    local_login_from: Vec<websec::IpNet>,
    log_read_token: Option<String>,
    metrics_token: Option<String>,
    shutdown_token: Option<String>,
    get_limits: websec::GetLimits,
}

/// Per-instance GCI transcript channel and backlog.
type GciMap = Arc<std::collections::HashMap<InstanceId, (broadcast::Sender<String>, LogHistory)>>;

/// The whole route chain, in the order it is matched.
fn build_routes(rc: RouteCtx) -> warp::filters::BoxedFilter<(Response,)> {
    let RouteCtx {
        db,
        bot_link_cfg,
        auth_cfg,
        local_admin_cfg,
        live_map,
        gci_map,
        log_tx,
        log_history,
        range_routes,
        replay_routes,
        campaign_json,
        effective_srs_url,
        news_images,
        shutdown,
        cors_origins,
        public_api_url,
        listen_port,
        local_login_from,
        log_read_token,
        metrics_token,
        shutdown_token,
        get_limits,
    } = rc;

    let cross_origin = !cors_origins.is_empty();
    let allowed_login_origins: Arc<Vec<std::string::String>> = Arc::new(cors_origins.clone());
    let cors = warp::cors()
        .allow_methods(&[Method::GET, Method::POST, Method::OPTIONS])
        // x-intel-filename: sent by the recon-intel photo upload, which is a
        // cross-origin POST and so triggers a CORS preflight.
        // x-cockpit-key: the cockpit overlay key, sent as a header by
        // overlays that no longer put it in the URL.
        .allow_headers(vec!["content-type", "x-intel-filename", "x-cockpit-key"])
        .expose_headers(vec!["x-total-count"])
        .allow_credentials(true);
    let cors = if cross_origin {
        // Our own public origin too. A page bfdb serves itself
        // (api.vectorstrike.org/map) sends no Origin on a same-origin GET, so
        // its fetches got through -- but a browser always sends one on a
        // WebSocket handshake and on a POST, and every one of those was 403'd
        // ("origin not allowed"): TACMAP, logs, login, admin actions all dead
        // on that copy of the dashboard. The CSRF policy already trusts it.
        let own = websec::origin_of(&public_api_url);
        cors.allow_origins(
            cors_origins
                .iter()
                .map(|s| s.as_str())
                .chain(own.as_deref())
                .collect::<Vec<_>>(),
        )
    } else {
        // No --cors-origin: only pages bfdb serves itself, at its public
        // address or opened locally on the listen port. allow_any_origin with
        // credentials let any site a signed-in user visited read the API as
        // them.
        let port = listen_port;
        let local = [format!("http://localhost:{port}"), format!("http://127.0.0.1:{port}")];
        let own = websec::origin_of(&public_api_url);
        cors.allow_origins(local.iter().map(|s| s.as_str()).chain(own.as_deref()).collect::<Vec<_>>())
    };
    if cross_origin {
        log::info!("Cross-origin mode enabled for: {:?} (cookies use SameSite=None; Secure — bfdb must be served over TLS)", cors_origins);
    }

    // The DCS server instances this bfdb fronts -- backs the dashboard's
    // instance selector. Must come before the routes that take ?instance=.
    let instances_route = warp::path!("api" / "instances")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .then(api_instances);

    let rounds = warp::path!("api" / "rounds")
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_instance(db.clone()))
        .then(api_rounds);

    let round_summary = warp::path!("api" / "rounds" / u64 / "summary")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_round_summary);

    let round_report = warp::path!("api" / "rounds" / u64 / "report")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_round_report);

    let leaderboard = warp::path!("api" / "leaderboard")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .then(api_leaderboard);

    let all_pilots = warp::path!("api" / "pilots")
        .and(with_db(db.clone()))
        .and(warp::query::<Q>())
        .then(api_all_pilots);

    let objectives = warp::path!("api" / "objectives")
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .then(|db, q: std::collections::HashMap<String, String>, inst| {
            let round_id = q.get("round").and_then(|s| s.parse().ok());
            api_objectives(db, round_id, inst)
        });

    let frontline = warp::path!("api" / "frontline")
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .then(|db, q: std::collections::HashMap<String, String>, inst| {
            let round_id = q.get("round").and_then(|s| s.parse().ok());
            api_frontline(db, round_id, inst)
        });

    let briefing = warp::path!("api" / "briefing")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .then(api_briefing);

    let warehouse = warp::path!("api" / "warehouse")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .then(api_warehouse);

    let situation = warp::path!("api" / "situation")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .then(api_situation);

    let groundwar = warp::path!("api" / "groundwar")
        .and(warp::get())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .then(api_groundwar);

    let hq = warp::path!("api" / "hq")
        .and(warp::get())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .then(api_hq);

    let kills = warp::path!("api" / "kills")
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .then(|db, q: std::collections::HashMap<String, String>, inst| {
            let round_id = q.get("round").and_then(|s| s.parse().ok());
            let limit = q.get("limit").and_then(|s| s.parse().ok());
            api_kills(db, round_id, limit, inst)
        });

    let capture_events = warp::path!("api" / "capture-events")
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .then(|db, q: std::collections::HashMap<String, String>, inst| {
            let round_id = q.get("round").and_then(|s| s.parse().ok());
            let limit = q.get("limit").and_then(|s| s.parse().ok());
            let offset = q.get("offset").and_then(|s| s.parse().ok()).unwrap_or(0);
            api_capture_events(db, round_id, limit, offset, inst)
        });

    let pilot = warp::path!("api" / "pilot" / String)
        .and(with_db(db.clone()))
        .then(api_pilot);

    let pilot_sorties = warp::path!("api" / "pilot" / String / "sorties")
        .and(with_db(db.clone()))
        .and(warp::query::<Q>())
        .then(api_pilot_sorties);

    let pilot_sortie = warp::path!("api" / "pilot" / String / "sortie" / u64)
        .and(with_db(db.clone()))
        .then(api_pilot_sortie);

    let pilot_breakdown = warp::path!("api" / "pilot" / String / "breakdown")
        .and(with_db(db.clone()))
        .then(api_pilot_breakdown);

    let squadron_list = warp::path!("api" / "squadrons")
        .and(warp::get())
        .and(with_db(db.clone()))
        .and(warp::query::<Q>())
        .then(api_squadrons);
    let squadron_one = warp::path!("api" / "squadrons" / String)
        .and(warp::get())
        .and(with_db(db.clone()))
        .then(api_squadron);
    let squadron_create = warp::path!("api" / "squadrons")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<SquadronCreateBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_squadron_create);
    let squadron_join = warp::path!("api" / "squadrons" / String / "join")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_squadron_join);
    let squadron_leave = warp::path!("api" / "squadrons" / "leave")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_squadron_leave);
    let squadron_kick = warp::path!("api" / "squadrons" / "kick")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<SquadronKickBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .then(api_squadron_kick);
    let pilot_card = warp::path!("api" / "pilot" / String / "card.png")
        .and(with_db(db.clone()))
        .then(api_pilot_card);
    let gci_transmit = warp::path!("api" / "gci" / "transmit")
        .and(warp::post())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<GciTransmitBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_gci_transmit);
    let squadron_routes = gci_transmit
        .or(squadron_leave)
        .or(squadron_kick)
        .or(squadron_list)
        .or(squadron_create)
        .or(squadron_join)
        .or(squadron_one)
        .or(pilot_card)
        .boxed();

    let pilot_progression = warp::path!("api" / "pilot" / String / "progression")
        .and(with_db(db.clone()))
        .then(api_pilot_progression);

    let xp_leaderboard = warp::path!("api" / "progression" / "top")
        .and(with_db(db.clone()))
        .then(api_xp_leaderboard);

    let pilot_kills_route = warp::path!("api" / "pilot" / String / "kills")
        .and(with_db(db.clone()))
        .and(warp::query::<Q>())
        .then(api_pilot_kills);

    let pilot_deploys_route = warp::path!("api" / "pilot" / String / "deploys")
        .and(with_db(db.clone()))
        .and(warp::query::<Q>())
        .then(api_pilot_deploys);

    // Liveness: an engine probe (cached 15s) and one read of the round
    // tree. Process supervisors should poll this, not /api/stats (which runs
    // several Sled queries and can be slow while the stats archive is
    // still replaying at startup).
    let health = warp::path!("api" / "health")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_health);

    let admin_health = warp::path!("api" / "admin" / "health")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_health);

    // This bfdb binary's own build identity (git rev + build time), compiled
    // in by build.rs. Public -- it's just build metadata.
    let version = warp::path!("api" / "version")
        .map(|| warp::reply::json(&build_info_json()));

    let unitdb = warp::path!("api" / "unitdb")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_unitdb);


    let unitdb_stale = warp::path!("api" / "unitdb" / "stale-overrides")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_unitdb_stale);


    let stats = warp::path!("api" / "stats")
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_stats);

    let units = warp::path!("api" / "units")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_units);

    let online = warp::path!("api" / "online")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_online);

    let points = warp::path!("api" / "points")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_points);

    let captures = warp::path!("api" / "captures")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_captures);

    let aircraft_usage = warp::path!("api" / "aircraft-usage")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_aircraft_usage);

    let ws_units_route = warp::path!("ws" / "units")
        .and(websec::ws_limited())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .and(with_live(live_map.clone()))
        .then(|ws, ip, sid, db, inst: Inst, map: LiveMap| async move {
            let l = live_for(&map, &inst);
            ws_units_handler(ws, ip, sid, db, l.live, l.live_tx).await
        });

    let ws_groundwar_route = warp::path!("ws" / "groundwar")
        .and(websec::ws_limited())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(with_live(live_map.clone()))
        .then(|ws, ip, sid, q, db, bot, inst: Inst, map: LiveMap| async move {
            let l = live_for(&map, &inst);
            groundfeed::handler(ws, ip, sid, q, db, bot, inst, l.ground).await
        });

    let ws_tacmap_route = warp::path!("ws" / "tacmap")
        .and(websec::ws_limited())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(with_live(live_map.clone()))
        .then(
            |ws, ip, sid, q, db, bot, inst: Inst, map: LiveMap| async move {
                let l = live_for(&map, &inst);
                ws_tacmap_handler(ws, ip, sid, q, db, bot, inst, l.tac, l.live).await
            },
        );

    // ── Log WebSocket (/ws/logs) — admin only ────────────────────────
    let log_tx_ws  = log_tx.clone();
    let log_hist_ws = log_history.clone();
    let ws_logs_route = warp::path!("ws" / "logs")
        .and(websec::ws_limited())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(warp::any().map(move || log_tx_ws.clone()))
        .and(warp::any().map(move || log_hist_ws.clone()))
        .then(ws_logs_handler);

    // ── GCI transcript (/ws/gci live, /api/gci/transcript snapshot) — admin ──
    let gci_map_ws = gci_map.clone();
    let ws_gci_route = warp::path!("ws" / "gci")
        .and(websec::ws_limited())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .and(warp::any().map(move || gci_map_ws.clone()))
        .then(|ws, ip, sid, db, inst: Inst, map: Arc<std::collections::HashMap<InstanceId, (broadcast::Sender<String>, LogHistory)>>| async move {
            let (tx, hist) = map.get(&inst.id).expect("gci channel per instance").clone();
            ws_logs_handler(ws, ip, sid, db, tx, hist).await
        });
    let gci_map_api = gci_map.clone();
    let gci_transcript_route = warp::path!("api" / "gci" / "transcript")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .and(warp::any().map(move || gci_map_api.clone()))
        .then(|sid: Option<Uuid>, db: StatsDb, inst: Inst, map: Arc<std::collections::HashMap<InstanceId, (broadcast::Sender<String>, LogHistory)>>| async move {
            let hist = map.get(&inst.id).expect("gci channel per instance").1.clone();
            let authed = match sid {
                Some(id) => task::block_in_place(|| db.get_session(id))
                    .ok()
                    .flatten()
                    .map(|s| s.is_admin)
                    .unwrap_or(false),
                None => false,
            };
            if !authed {
                return warp::reply::with_status(
                    warp::reply::json(&serde_json::json!({"error": "admin only"})),
                    warp::http::StatusCode::FORBIDDEN,
                )
                .into_response();
            }
            let lines: Vec<serde_json::Value> = hist
                .lock()
                .unwrap()
                .iter()
                .filter_map(|s| serde_json::from_str(s).ok())
                .collect();
            warp::reply::json(&lines).into_response()
        });

    // ── Engine log WebSocket (/ws/engine-logs) — live bflib logs, admin only ──
    let ws_engine_logs_route = warp::path!("ws" / "engine-logs")
        .and(websec::ws_limited())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(ws_engine_logs_handler);

    // ── Auth routes ──────────────────────────────────────────────────
    let auth_login = warp::path!("api" / "auth" / "login")
        .and(warp::query::<LoginQuery>())
        .and(with_auth_cfg(auth_cfg.clone()))
        .and(with_db(db.clone()))
        .and(warp::any().map(move || allowed_login_origins.clone()))
        .and(websec::with_client_ip())
        .then(api_auth_login);

    let auth_callback = warp::path!("api" / "auth" / "callback")
        .and(warp::query::<CallbackQuery>())
        .and(with_auth_cfg(auth_cfg.clone()))
        .and(with_db(db.clone()))
        .and(warp::any().map(move || cross_origin))
        .then(api_auth_callback);

    let auth_me = warp::path!("api" / "auth" / "me")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_auth_me);

    // POST only: a GET logout can be fired by any <img> on any page.
    let auth_logout = warp::path!("api" / "auth" / "logout")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(warp::any().map(move || cross_origin))
        .then(api_auth_logout);

    let local_login_from = Arc::new(local_login_from.clone());
    if local_admin_cfg.is_some() {
        if local_login_from.is_empty() {
            log::info!("local admin login accepted from loopback only (see --local-login-from)");
        } else {
            log::info!("local admin login accepted from {:?}", local_login_from);
        }
    }
    let auth_local_login = warp::path!("api" / "auth" / "local-login")
        .and(warp::post())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<LocalLoginBody>())
        .and(with_local_admin(local_admin_cfg.clone()))
        .and(with_db(db.clone()))
        .and(warp::any().map(move || cross_origin))
        .and(websec::with_client_ip())
        .and(warp::any().map(move || local_login_from.clone()))
        .then(api_auth_local_login);


    let admin_audit = warp::path!("api" / "admin" / "audit")
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .then(api_admin_audit);

    let admin_sessions = warp::path!("api" / "admin" / "sessions")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(warp::query::<Q>())
        .then(api_admin_sessions);

    let admin_reset = warp::path!("api" / "admin" / "reset")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_reset);

    let admin_reset_lives_all = warp::path!("api" / "admin" / "reset-lives-all")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_reset_lives_all);

    let admin_side_switch = warp::path!("api" / "admin" / "side-switch")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<SideSwitchBody>())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_side_switch);

    let admin_merge_rounds = warp::path!("api" / "admin" / "merge-rounds")
        .and(warp::post())
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_merge_rounds);

    let admin_rebuild_stats = warp::path!("api" / "admin" / "rebuild-stats")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_rebuild_stats);

    let admin_bot_status = warp::path!("api" / "admin" / "bot" / "status")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_bot_status);

    let admin_bot_start = warp::path!("api" / "admin" / "bot" / "start")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_bot_start);

    let admin_bot_stop = warp::path!("api" / "admin" / "bot" / "stop")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_bot_stop);

    let admin_bot_restart = warp::path!("api" / "admin" / "bot" / "restart")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_bot_restart);

    let admin_bot_mission_restart = warp::path!("api" / "admin" / "bot" / "mission" / "restart")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_bot_mission_restart);

    let admin_bot_mission_pause = warp::path!("api" / "admin" / "bot" / "mission" / "pause")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_bot_mission_pause);

    let admin_bot_mission_unpause = warp::path!("api" / "admin" / "bot" / "mission" / "unpause")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_bot_mission_unpause);

    // GET and POST, outside the warp::get() group below so the SPA catch-all
    // can't answer it and a POST still routes.
    let admin_ops_proxy = warp::path("api")
        .and(warp::path("admin"))
        .and(warp::path("ops"))
        .and(warp::method())
        .and(warp::path::tail())
        .and(
            warp::query::raw()
                .or(warp::any().map(String::new))
                .unify(),
        )
        .and(extract_session_cookie())
        // A GET has no body to read. warp's content_length_limit also
        // *requires* a Content-Length header, which browsers never send on a
        // GET -- so every GET the OPS page made was rejected here and fell
        // through to the SPA catch-all, and the page got index.html back
        // ("Unexpected token '<'", Sept 28). Only a POST is size-checked.
        .and(
            warp::get()
                .map(bytes::Bytes::new)
                .or(warp::body::content_length_limit(4 * 1024 * 1024).and(warp::body::bytes()))
                .unify(),
        )
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and_then(|method: warp::http::Method, tail, query, session, body, db, bot| async move {
            if method != warp::http::Method::GET && method != warp::http::Method::POST {
                return Err(warp::reject::not_found());
            }
            Ok::<_, warp::Rejection>(api_admin_ops_proxy(method, tail, query, session, body, db, bot).await)
        })
        .boxed();

    let admin_perf = warp::path!("api" / "admin" / "perf")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_perf);

    let admin_perf_history = warp::path!("api" / "admin" / "perf-history")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_perf_history);

    let admin_pilot_sides = warp::path!("api" / "admin" / "pilot-sides")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_pilot_sides);

    let admin_banned = warp::path!("api" / "admin" / "banned")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(warp::query::<Q>())
        .then(api_admin_banned);

    let admin_engine_errors = warp::path!("api" / "admin" / "engine-errors")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_engine_errors);

    // ── Plain-text log tail (/api/logs/:source) — static-token auth ──────────
    let logs_token = Arc::new(log_read_token.clone());
    let logs_hist_api = log_history.clone();
    let logs_route = warp::path!("api" / "logs" / String)
        .and(warp::get())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::header::optional::<std::string::String>("authorization"))
        .and(warp::any().map(move || logs_token.clone()))
        .and(with_db(db.clone()))
        .and(warp::any().map(move || logs_hist_api.clone()))
        .and(with_instance(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(websec::with_client_ip())
        .then(api_logs);

    let admin_ban_route = warp::path!("api" / "admin" / "ban")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<BanBody>())
        .and(with_db(db.clone()))
        .then(api_admin_ban);

    let admin_unban_route = warp::path!("api" / "admin" / "unban")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<UnbanBody>())
        .and(with_db(db.clone()))
        .then(api_admin_unban);

    let commander_spawn_route = warp::path!("api" / "commander" / "spawn")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<SpawnBody>())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_commander_spawn);

    let admin_priority_route = warp::path!("api" / "admin" / "priority")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<PriorityBody>())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_priority);

    let cockpit_ewr_report_route = warp::path!("api" / "cockpit" / "ewr" / "report")
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_ewr_report);

    let cockpit_ewr_intel_route = warp::path!("api" / "cockpit" / "ewr" / "intel")
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_ewr_intel);

    let cockpit_ewr_toggle_route = warp::path!("api" / "cockpit" / "ewr" / "toggle")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_ewr_toggle);

    let cockpit_ewr_units_route = warp::path!("api" / "cockpit" / "ewr" / "units")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<EwrUnitsBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_ewr_units);

    let cockpit_carp_solve_route = warp::path!("api" / "cockpit" / "carp" / "solve")
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_carp_solve);

    let cockpit_carp_solve_latlon_route = warp::path!("api" / "cockpit" / "carp" / "solve-latlon")
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_carp_solve_latlon);

    let cockpit_cargo_spawn_route = warp::path!("api" / "cockpit" / "cargo" / "spawn")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<CargoSpawnBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_cargo_spawn);

    let cockpit_plugin_version_route = warp::path!("api" / "cockpit" / "plugin" / "version")
        .and(warp::get())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .then(api_cockpit_plugin_version);

    let cockpit_plugin_download_route = warp::path!("api" / "cockpit" / "plugin" / "download")
        .and(warp::get())
        .then(api_cockpit_plugin_download);

    let cockpit_plugin_installer_route = warp::path!("api" / "cockpit" / "plugin" / "installer")
        .and(warp::get())
        .then(api_cockpit_plugin_installer);

    let cockpit_context_route = warp::path!("api" / "cockpit" / "context")
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_context);

    let cockpit_menu_route = warp::path!("api" / "cockpit" / "menu")
        .and(warp::get())
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_menu);

    let cockpit_messages_route = warp::path!("api" / "cockpit" / "messages")
        .and(warp::get())
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_messages);

    let cockpit_cargo_route = warp::path!("api" / "cockpit" / "cargo")
        .and(warp::get())
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_cargo);

    let cockpit_menu_invoke_route = warp::path!("api" / "cockpit" / "menu" / "invoke")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(cockpit_query())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<MenuInvokeBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .and(websec::with_client_ip())
        .then(api_cockpit_menu_invoke);

    let trails = warp::path!("api" / "trails")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_trails);

    let config_route = warp::path!("api" / "config")
        .and(warp::any().map(move || campaign_json.clone()))
        .then(api_config);

    // Each DCS instance runs its own SRS server, so the client list is
    // resolved per instance -- falling back to the single-server --srs-url /
    // campaign.json `srsUrl` for the default instance.
    let srs_fallback: Arc<Option<String>> = Arc::new(effective_srs_url);
    let srs_route = warp::path!("api" / "srs")
        .and(with_instance(db.clone()))
        .and(warp::any().map(move || srs_fallback.clone()))
        .then(|inst: Inst, fallback: Arc<Option<String>>| async move {
            let url: Arc<Option<String>> = match &inst.cfg.srs_url {
                Some(u) => Arc::new(Some(u.clone())),
                None => fallback,
            };
            api_srs(url).await
        });

    let admin_cfg_get_route = warp::path!("api" / "admin" / "cfg")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_cfg_get);

    let admin_cfg_schema_route = warp::path!("api" / "admin" / "cfg" / "schema")
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .then(api_admin_cfg_schema);

    let admin_cfg_post_route = warp::path!("api" / "admin" / "cfg")
        .and(warp::post())
        .and(extract_session_cookie())
        // A whole campaign engine config -- far bigger than any other body.
        .and(warp::body::content_length_limit(8 * 1024 * 1024))
        .and(warp::body::json::<SaveCfgBody>())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_cfg_post);

    let wiki_list_route = warp::path!("api" / "wiki" / "pages")
        .and(with_db(db.clone()))
        .and(warp::query::<Q>())
        .then(api_wiki_list);

    // Not the `path!` macro here (it implicitly requires end-of-path) --
    // the slug itself is multi-segment (e.g. "gameplay/objectives"), so
    // these need `path::tail()` to actually capture it, same pattern as
    // `site_files`/`static_files` below.
    let wiki_get_route = warp::path("api")
        .and(warp::path("wiki"))
        .and(warp::path("pages"))
        .and(warp::path::tail())
        .and(with_db(db.clone()))
        .then(api_wiki_get);

    let wiki_save_route = warp::path("api")
        .and(warp::path("wiki"))
        .and(warp::path("pages"))
        .and(warp::path::tail())
        .and(warp::post())
        .and(extract_session_cookie())
        // A full wiki page of Markdown.
        .and(warp::body::content_length_limit(2 * 1024 * 1024))
        .and(warp::body::json::<SaveWikiPageBody>())
        .and(with_db(db.clone()))
        .then(api_wiki_save);

    let wiki_delete_route = warp::path!("api" / "wiki" / "delete")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<DeleteWikiPageBody>())
        .and(with_db(db.clone()))
        .then(api_wiki_delete);

    let wiki_upload_image_route = warp::path!("api" / "wiki" / "images")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::header::<std::string::String>("content-type"))
        .and(warp::body::content_length_limit(MAX_WIKI_IMAGE_BYTES))
        .and(warp::body::bytes())
        .and(with_db(db.clone()))
        .then(api_wiki_upload_image);

    // Instance-scoped: the numbers a page quotes come from whichever DCS
    // server the reader picked in the wiki's instance selector.
    let news_route = warp::path!("api" / "news")
        .and(with_db(db.clone()))
        .and(warp::query::<std::collections::HashMap<String, String>>())
        .and(with_instance(db.clone()))
        .and(with_news_images(news_images.clone()))
        .then(|db, q: std::collections::HashMap<String, String>, inst, images| {
            let limit = q.get("limit").and_then(|s| s.parse().ok());
            api_news(db, limit, inst, images)
        });

    let news_image_route = warp::path!("api" / "news" / "image" / String)
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_news_image);

    let admin_news_regenerate_image = warp::path!("api" / "admin" / "news" / "regenerate-image")
        .and(warp::post())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .and(with_news_images(news_images.clone()))
        .then(api_admin_news_regenerate_image);

    let wiki_facts_route = warp::path!("api" / "wiki" / "facts")
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_wiki_facts);

    let wiki_get_image_route = warp::path!("api" / "wiki" / "images" / Uuid)
        .and(with_db(db.clone()))
        .then(api_wiki_get_image);

    // ── Recon intel (TARPS) ──────────────────────────────────────────────
    let intel_list_route = warp::path!("api" / "intel" / "captures")
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_intel_list);

    let intel_get_image_route = warp::path!("api" / "intel" / "images" / Uuid)
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_intel_get_image);

    let intel_upload_route = warp::path!("api" / "intel" / "upload")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::header::optional::<std::string::String>("x-intel-filename"))
        .and(warp::header::<std::string::String>("content-type"))
        .and(warp::body::content_length_limit(MAX_INTEL_IMAGE_BYTES))
        .and(warp::body::bytes())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_intel_upload);

    let intel_adjust_route = warp::path!("api" / "intel" / "adjust")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<IntelAdjustBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_intel_adjust);

    let intel_delete_route = warp::path!("api" / "intel" / "delete")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<IntelDeleteBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_intel_delete);

    let intel_purge_route = warp::path!("api" / "intel" / "purge")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_intel_purge);

    let intel_markup_list_route = warp::path!("api" / "intel" / "markup")
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_intel_markup_list);

    let intel_markup_add_route = warp::path!("api" / "intel" / "markup")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::content_length_limit(512 * 1024))
        .and(warp::body::json::<IntelMarkupBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_intel_markup_add);

    let groundwar_command_route = warp::path!("api" / "groundwar" / "command")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<bfprotocols::groundwar::GroundCommand>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_groundwar_command);

    let command_picture_route = warp::path!("api" / "command")
        .and(warp::get())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_command);

    let command_order_route = warp::path!("api" / "command" / "order")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<bfprotocols::command::CommandOrder>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_command_order);

    let command_log_route = warp::path!("api" / "command" / "log")
        .and(warp::get())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_command_log);

    let command_me_route = warp::path!("api" / "command" / "me")
        .and(warp::get())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_command_me);

    let admin_commanders_route = warp::path!("api" / "admin" / "commanders")
        .and(warp::get())
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_commanders);

    let admin_commander_grant_route = warp::path!("api" / "admin" / "commanders")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<CommanderGrantBody>())
        .and(with_db(db.clone()))
        .and(with_instance(db.clone()))
        .then(api_admin_commander_grant);

    let hq_command_route = warp::path!("api" / "hq" / "command")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<bfprotocols::hq::HqCommand>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_hq_command);

    let plans_list_route = warp::path!("api" / "plans")
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_plans_list);

    let plans_save_route = warp::path!("api" / "plans")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        // A full plan: 99 waypoints plus up to 48 KB of avionics settings.
        .and(warp::body::content_length_limit(128 * 1024))
        .and(warp::body::json::<PlanSaveBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_plans_save);

    let plans_delete_route = warp::path!("api" / "plans" / "delete")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<IntelDeleteBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_plans_delete);

    let intel_markup_delete_route = warp::path!("api" / "intel" / "markup" / "delete")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<IntelDeleteBody>())
        .and(with_db(db.clone()))
        .and(with_bot_link_cfg(bot_link_cfg.clone()))
        .and(with_instance(db.clone()))
        .then(api_intel_markup_delete);

    // /site/* → embedded bfsite SPA
    let site_files = warp::path("site")
        .and(warp::path::tail())
        .map(|tail: warp::path::Tail| serve_site_asset(tail.as_str()));

    // Catch-all → embedded bfweb SPA
    let static_files = warp::get()
        .and(warp::path::tail())
        .map(|tail: warp::path::Tail| serve_asset(tail.as_str()));

    // Box sub-chains to avoid warp filter type overflow
    let api_routes = instances_route
        .or(rounds)
        .or(round_summary)
        .or(round_report)
        .or(health)
        .or(version)

        .or(unitdb_stale)

        .or(unitdb)
        .or(leaderboard)
        .or(objectives)
        .or(frontline)
        .or(briefing)
        .or(situation)
        .or(groundwar)
        .or(hq)
        .or(warehouse)
        .or(kills)
        .or(capture_events)
        .or(pilot_sorties)
        .or(pilot_sortie)
        .or(pilot_breakdown)
        .or(pilot_progression)
        .or(xp_leaderboard)
        .or(pilot_kills_route)
        .or(pilot_deploys_route)
        .or(pilot)
        .or(stats)
        .or(units)
        .or(online)
        .or(points)
        .or(captures)
        .or(aircraft_usage)
        .or(trails)
        .or(all_pilots)
        .or(config_route)
        .or(srs_route)
        .or(cockpit_plugin_version_route)
        .or(cockpit_plugin_download_route)
        .or(cockpit_plugin_installer_route)
        .or(cockpit_context_route)
        .or(cockpit_menu_route)
        .or(cockpit_messages_route)
        .or(cockpit_cargo_route)
        .or(cockpit_ewr_report_route)
        .or(cockpit_ewr_intel_route)
        .or(cockpit_carp_solve_route)
        .or(cockpit_carp_solve_latlon_route)
        .or(wiki_list_route)
        .or(wiki_get_route)
        .or(wiki_get_image_route)
        .or(news_route)
        .or(news_image_route)
        .or(wiki_facts_route)
        .or(intel_list_route)
        .or(intel_get_image_route)
        .or(intel_markup_list_route)
        .or(plans_list_route)
        .boxed();

    let auth_routes = auth_login
        .or(auth_callback)
        .or(auth_me)

        .or(admin_sessions)
        .or(admin_audit)
        .or(admin_health)
        .or(admin_perf)
        .or(admin_perf_history)
        .or(admin_pilot_sides)
        .or(admin_banned)
        .or(admin_engine_errors)
        .or(logs_route)
        .or(admin_bot_status)
        .or(admin_cfg_get_route)
        .or(admin_cfg_schema_route)
        // GETs belong in this group, ahead of the dashboard's catch-all
        // page: added after it, they were never reached (the SPA answered
        // /api/command with index.html, 200).
        .or(command_me_route)
        .or(command_log_route)
        .or(command_picture_route)
        .or(admin_commanders_route)
        .boxed();

    // ── Maintenance routes: shutdown, session revocation, metrics ──────────
    let shutdown_token = Arc::new(shutdown_token.clone());
    let shutdown_route = warp::path!("api" / "admin" / "shutdown")
        .and(warp::post())
        .and(websec::with_direct_loopback())
        .and(warp::header::optional::<std::string::String>("authorization"))
        .and(extract_session_cookie())
        .and(with_db(db.clone()))
        .and(warp::any().map(move || shutdown_token.clone()))
        .and(warp::any().map({
            let sd = shutdown.clone();
            move || sd.clone()
        }))
        .then(api_admin_shutdown);

    let admin_revoke_route = warp::path!("api" / "admin" / "sessions" / "revoke")
        .and(warp::post())
        .and(extract_session_cookie())
        .and(warp::body::content_length_limit(JSON_BODY_LIMIT))
        .and(warp::body::json::<RevokeBody>())
        .and(with_db(db.clone()))
        .then(api_admin_revoke_sessions);

    let metrics_tokens = Arc::new((metrics_token.clone(), log_read_token.clone()));
    let metrics_route = warp::path!("api" / "metrics")
        .and(warp::get())
        .and(warp::query::<std::collections::HashMap<std::string::String, std::string::String>>())
        .and(warp::header::optional::<std::string::String>("authorization"))
        .and(warp::any().map(move || metrics_tokens.clone()))
        .and(with_db(db.clone()))
        .and(websec::with_client_ip())
        .then(api_metrics);

    let origin_policy = websec::OriginPolicy::new(
        cors_origins
            .iter()
            .map(|s| s.as_str())
            .chain(std::iter::once(public_api_url.as_str())),
    );

    // The dashboard's own files (/assets/*.js, *.css, fonts, and the site's
    // under /site/), answered ahead of the CORS filter. A browser loads module
    // scripts and crossorigin stylesheets in CORS mode, so it sends an Origin
    // even when the page came from this very server -- and the CORS filter
    // below refused every Origin not in --cors-origin, the server's own
    // included. A page opened at any other address (http://localhost:<port>,
    // which is how the Discord bot captures /snapshot) came up blank: its
    // HTML loaded, every script 403'd. Only files that really exist match
    // here; everything else, the SPA fallback and the whole API, still goes
    // through CORS exactly as before.
    let embedded_files = warp::get()
        .and(warp::path::tail())
        .and_then(|tail: warp::path::Tail| async move {
            let path = tail.as_str();
            if let Some(site) = path.strip_prefix("site/") {
                if !site.is_empty() && SiteAssets::get(site).is_some() {
                    return Ok::<Response, warp::Rejection>(serve_site_asset(site));
                }
            } else if !path.is_empty() && path != "index.html" && Assets::get(path).is_some() {
                return Ok(serve_asset(path));
            }
            Err(warp::reject::not_found())
        })
        .map(|r: Response| websec::harden(r));

    let cors_routes = websec::csrf_guard(origin_policy)
        // First of all: a cookie-authenticated POST or WebSocket from a page
        // we do not trust is refused before any route sees it.
        // Then a client past its GET budget (`--public-get-rate`).
        .or(websec::get_rate_guard(get_limits))
        .or(shutdown_route)
        .or(admin_revoke_route)
        .or(metrics_route)
        .or(bad_instance_guard(db.clone()))
        // Ahead of everything, and outside the `warp::get()` group, so a POST
        // with an unroutable ?instance= gets the same 400 a GET does instead
        // of silently falling back to the default server.
        // The training range API: one pre-boxed filter, ahead of the SPA
        // catch-all in the GET group below (which would otherwise answer
        // /api/range/... with index.html).
        .or(range_routes)
        // Flight replay: also boxed, also ahead of the SPA catch-all.
        .or(replay_routes)
        // Squadrons and GCI transmit: POSTs as well as GETs, so outside the
        // `warp::get()` group below. Inside it every POST was refused with a
        // 405 -- founding, joining, leaving or kicking from a squadron, and
        // transmitting to GCI from the dashboard all failed (a cross-origin
        // 405 carries no CORS header, so the browser reported a CORS block).
        // Each route checks its own method.
        .or(squadron_routes)
        .or(admin_ops_proxy)
        .or(warp::get()
        .and(
            api_routes
                .or(auth_routes)
                .or(ws_units_route)
                .or(ws_tacmap_route)
                .or(ws_groundwar_route)
                .or(ws_logs_route)
                .or(ws_gci_route)
                .or(gci_transcript_route)
                .or(ws_engine_logs_route)
                .or(site_files)
                .or(static_files),
        ))
        .or(auth_local_login)
        .or(auth_logout)
        .or(admin_reset)
        .or(admin_merge_rounds)
        .or(admin_rebuild_stats)
        .or(admin_news_regenerate_image)
        .or(admin_reset_lives_all)
        .or(admin_side_switch)
        .or(admin_ban_route)
        .or(admin_unban_route)
        .or(admin_cfg_post_route)
        .or(commander_spawn_route)
        .or(admin_priority_route)
        .or(cockpit_ewr_toggle_route.or(cockpit_ewr_units_route).or(cockpit_cargo_spawn_route).or(cockpit_menu_invoke_route).boxed())
        .boxed()
        .or(wiki_save_route.or(wiki_delete_route).or(wiki_upload_image_route).boxed())
        .or(intel_upload_route
            .or(intel_adjust_route)
            .or(intel_delete_route)
            .or(intel_purge_route)
            .or(intel_markup_add_route)
            .or(intel_markup_delete_route)
            .or(plans_delete_route)
            .or(plans_save_route)
            .or(groundwar_command_route)
            .or(hq_command_route)
            .or(command_order_route)
            .or(admin_commander_grant_route)
            .boxed())
        .or(admin_bot_start
            .or(admin_bot_stop)
            .or(admin_bot_restart)
            .or(admin_bot_mission_restart)
            .or(admin_bot_mission_pause)
            .or(admin_bot_mission_unpause)
            .boxed())
        .map(|r| websec::harden(warp::Reply::into_response(r)))
        .with(cors);
    embedded_files
        .or(cors_routes)
        .map(|r| warp::Reply::into_response(r))
        .with(warp::log::custom(|info| {
            websec::record_request(info.method(), info.path(), info.status(), info.elapsed())
        }))
        .map(|r| warp::Reply::into_response(r))
        .boxed()
}
