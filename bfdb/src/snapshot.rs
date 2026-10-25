//! Public snapshot: the last known campaign state, kept somewhere that stays
//! up when this box does not.
//!
//! bfdb runs on the game server box, and the four Vercel front ends (the
//! dashboard, the public site, the wiki and the range site) read everything
//! from it. When the box is down they used to have nothing to show. With this
//! on, bfdb renders its *public* read routes to JSON every `--snapshot-interval`
//! seconds (and straight away when a round starts or ends) and uploads the
//! files that changed to an S3-compatible bucket (Cloudflare R2 in production,
//! see `deploy/snapshot-fallback.md`). The front ends load the same resource
//! from there when the API does not answer (`shared/web/snapshot.ts`).
//!
//! The JSON is produced by calling the route handlers themselves, so a
//! snapshot file is byte-for-byte what the live route answers.
//!
//! Only public data. Nothing behind `require_coalition` / `require_admin`,
//! no non-public instance, nothing from a session: [`allowed_key`] is the
//! list, checked again before every upload, and `published_set_is_allow_listed`
//! below pins it.
//!
//! Layout (keys under `--snapshot-prefix`), mirrored by `snapshotKey` in
//! `shared/web/snapshot.ts`:
//!
//! ```text
//! manifest.json                      generated_at, default instance, every file's hash
//! g/instances.json                   /api/instances
//! g/instances~kind-range.json        /api/instances?kind=range
//! g/wiki/pages.json                  /api/wiki/pages
//! g/wiki/pages/<slug>.json           /api/wiki/pages/<slug>
//! i/<id>/online.json                 /api/online?instance=<id>   (and objectives,
//! i/<id>/rounds/<round>/summary.json /api/rounds/<round>/summary  frontline, rounds,
//! i/<id>/wiki/facts.json             /api/wiki/facts              leaderboard, stats, news)
//! i/<id>/state.json                  this server's last known state (not an API route)
//! ```
//!
//! Upload signing is AWS Signature V4, written out here (about sixty lines)
//! on top of `sha2` and `hmac`, which the build already had through its TLS
//! stack. The S3 SDKs pull in hundreds of crates for the one request this
//! needs, a signed PUT (plus GET and DELETE, same signature).

use crate::db::StatsDb;
use crate::instance::InstanceId;
use crate::{BotLinkConfig, Inst};
use anyhow::{anyhow, bail, Context, Result};
use chrono::{DateTime, Utc};
use futures::StreamExt;
use hmac::{Hmac, Mac};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use warp::Reply;

/// `--snapshot-*`: where the public snapshot goes. Everything is off unless
/// `--snapshot-endpoint` (S3/R2) or `--snapshot-dir` (a local folder) is set.
///
/// Every flag can also come from the environment (`--snapshot-endpoint` from
/// `$BFDB_SNAPSHOT_ENDPOINT`, and so on; a flag wins), so a bfdb that
/// DCSServerBot launches, and which inherits its environment, can be switched
/// on without a change to the bot.
#[derive(clap::Args, Debug, Clone, Default)]
pub(crate) struct SnapshotArgs {
    /// S3-compatible endpoint to publish the public snapshot to, e.g.
    /// `https://<account id>.r2.cloudflarestorage.com`. Unset: no snapshot.
    /// See deploy/snapshot-fallback.md. Also $BFDB_SNAPSHOT_ENDPOINT.
    #[arg(long = "snapshot-endpoint")]
    pub(crate) endpoint: Option<String>,
    /// Bucket name (path-style addressing: `<endpoint>/<bucket>/<key>`).
    /// Also $BFDB_SNAPSHOT_BUCKET.
    #[arg(long = "snapshot-bucket")]
    pub(crate) bucket: Option<String>,
    /// Signing region. R2 wants `auto` (the default); AWS wants the bucket's
    /// region. Also $BFDB_SNAPSHOT_REGION.
    #[arg(long = "snapshot-region")]
    pub(crate) region: Option<String>,
    /// Key prefix inside the bucket, e.g. `prod/`. Default: the bucket root.
    /// Also $BFDB_SNAPSHOT_PREFIX.
    #[arg(long = "snapshot-prefix")]
    pub(crate) prefix: Option<String>,
    /// File holding the access key id. Also $BFDB_SNAPSHOT_ACCESS_KEY_ID.
    #[arg(long = "snapshot-access-key-id-file")]
    pub(crate) access_key_id_file: Option<PathBuf>,
    /// File holding the secret access key. Also $BFDB_SNAPSHOT_SECRET_ACCESS_KEY.
    #[arg(long = "snapshot-secret-access-key-file")]
    pub(crate) secret_access_key_file: Option<PathBuf>,
    /// Where the front ends read the snapshot from (the bucket's public
    /// address, e.g. `https://snapshot.vectorstrike.org`). Optional: when set,
    /// bfdb checks after its first upload that the manifest is readable there.
    /// Also $BFDB_SNAPSHOT_PUBLIC_URL.
    #[arg(long = "snapshot-public-url")]
    pub(crate) public_url: Option<String>,
    /// Write the snapshot to this folder instead of a bucket (development, or
    /// a static host of your own). Files are written uncompressed. Also
    /// $BFDB_SNAPSHOT_DIR.
    #[arg(long = "snapshot-dir")]
    pub(crate) dir: Option<PathBuf>,
    /// Seconds between snapshots (default 60, at least 15). A round starting
    /// or ending publishes at once. Also $BFDB_SNAPSHOT_INTERVAL.
    #[arg(long = "snapshot-interval")]
    pub(crate) interval_secs: Option<u64>,
}

impl SnapshotArgs {
    /// These flags, with each one that is unset taken from its
    /// `BFDB_SNAPSHOT_*` environment variable.
    pub(crate) fn with_env(&self) -> SnapshotArgs {
        let env = |name: &str| std::env::var(name).ok().map(|v| v.trim().to_string()).filter(|v| !v.is_empty());
        let mut a = self.clone();
        a.endpoint = a.endpoint.or_else(|| env("BFDB_SNAPSHOT_ENDPOINT"));
        a.bucket = a.bucket.or_else(|| env("BFDB_SNAPSHOT_BUCKET"));
        a.region = a.region.or_else(|| env("BFDB_SNAPSHOT_REGION"));
        a.prefix = a.prefix.or_else(|| env("BFDB_SNAPSHOT_PREFIX"));
        a.public_url = a.public_url.or_else(|| env("BFDB_SNAPSHOT_PUBLIC_URL"));
        a.dir = a.dir.or_else(|| env("BFDB_SNAPSHOT_DIR").map(PathBuf::from));
        a.interval_secs = a.interval_secs.or_else(|| env("BFDB_SNAPSHOT_INTERVAL").and_then(|v| v.parse().ok()));
        a
    }
}

/// Start the publisher if it is configured. A bad configuration is logged and
/// leaves it off rather than stopping bfdb: the snapshot is a fallback, the
/// live API is not.
pub(crate) fn spawn(
    args: &SnapshotArgs,
    db: StatsDb,
    bot: Arc<Option<BotLinkConfig>>,
    news_images: Option<Arc<crate::news_image::ImageCfg>>,
) {
    let args = &args.with_env();
    let sink = match Sink::from_args(args) {
        Ok(Some(s)) => s,
        Ok(None) => {
            log::info!("snapshot: no --snapshot-endpoint or --snapshot-dir -- the public snapshot is off");
            return;
        }
        Err(e) => {
            log::error!("snapshot: the public snapshot is OFF -- {e:#}");
            return;
        }
    };
    let interval = Duration::from_secs(args.interval_secs.unwrap_or(60).max(15));
    log::info!(
        "snapshot: publishing the public snapshot to {} every {}s",
        sink.describe(),
        interval.as_secs()
    );
    let public_url = args.public_url.clone().map(|u| u.trim_end_matches('/').to_string());
    let src = Source { db, bot, news_images };
    tokio::spawn(run(Publisher::new(sink), src, interval, public_url));
}

/// What the renderer needs: the handlers' own inputs.
#[derive(Clone)]
pub(crate) struct Source {
    pub(crate) db: StatsDb,
    pub(crate) bot: Arc<Option<BotLinkConfig>>,
    pub(crate) news_images: Option<Arc<crate::news_image::ImageCfg>>,
}

async fn run(mut p: Publisher, src: Source, interval: Duration, public_url: Option<String>) {
    // How often to look for a round starting or ending. Reading the round
    // list is a few tree lookups; the publish it may trigger is the work.
    const WATCH: Duration = Duration::from_secs(5);
    let mut tick = tokio::time::interval(interval);
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    let mut watch = tokio::time::interval(WATCH);
    watch.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
    let mut rounds = round_signature(&src.db);
    let mut checked_public = public_url.is_none();
    loop {
        tokio::select! {
            _ = tick.tick() => {}
            _ = watch.tick() => {
                let now = round_signature(&src.db);
                if now == rounds {
                    continue;
                }
                log::info!("snapshot: a round started or ended -- publishing now");
                rounds = now;
            }
        }
        match p.publish(&src).await {
            Ok(r) => {
                if r.failed > 0 || r.uploaded > 0 || r.deleted > 0 {
                    log::debug!(
                        "snapshot: {} uploaded, {} unchanged, {} removed, {} failed",
                        r.uploaded, r.unchanged, r.deleted, r.failed
                    );
                }
                if !checked_public {
                    if let Some(url) = &public_url {
                        checked_public = true;
                        check_public(url).await;
                    }
                }
            }
            Err(e) => p.warn(format!("snapshot: publish failed -- {e:#}")),
        }
    }
}

/// Warn once if the manifest is not readable at `--snapshot-public-url`: the
/// bucket has no public access, the custom domain is not connected, or CORS
/// blocks it. The front ends would silently get nothing.
async fn check_public(url: &str) {
    let res = crate::http_client()
        .get(format!("{url}/manifest.json"))
        .header("origin", "https://dashboard.vectorstrike.org")
        .send()
        .await;
    match res {
        Ok(r) if r.status().is_success() => {
            if r.headers().get("access-control-allow-origin").is_none() {
                log::warn!(
                    "snapshot: {url}/manifest.json has no CORS header for the dashboard's origin -- \
                     browsers will refuse it (see deploy/snapshot-fallback.md, CORS)"
                );
            } else {
                log::info!("snapshot: readable at {url}");
            }
        }
        Ok(r) => log::warn!("snapshot: {url}/manifest.json answered {} -- is public access on?", r.status()),
        Err(e) => log::warn!("snapshot: could not read {url}/manifest.json -- {e}"),
    }
}

/// Changes when any public instance's active round, or latest finished round,
/// changes: a round started or ended.
fn round_signature(db: &StatsDb) -> Vec<(InstanceId, Option<u64>, Option<u64>)> {
    public_campaigns(db)
        .into_iter()
        .map(|inst| {
            let rounds = tokio::task::block_in_place(|| db.latest_rounds_for(&inst.id)).unwrap_or_default();
            let active = rounds.iter().find(|(_, _, r)| r.end.is_none()).map(|(_, rid, _)| rid.0);
            let ended = rounds
                .iter()
                .filter(|(_, _, r)| r.end.is_some())
                .max_by_key(|(_, _, r)| r.end)
                .map(|(_, rid, _)| rid.0);
            (inst.id.clone(), active, ended)
        })
        .collect()
}

/// The campaign instances whose data is public. A test/staging server
/// (`public: false`) is never published; training ranges have no campaign.
fn public_campaigns(db: &StatsDb) -> Vec<Inst> {
    db.instances()
        .all()
        .iter()
        .filter(|cfg| cfg.public && !cfg.is_range() && safe_id(&cfg.id))
        .map(|cfg| db.state(&InstanceId::from(cfg.id.as_str())))
        .collect()
}

/// Public training ranges: only their `state.json` is published.
fn public_ranges(db: &StatsDb) -> Vec<Inst> {
    db.instances()
        .all()
        .iter()
        .filter(|cfg| cfg.public && cfg.is_range() && safe_id(&cfg.id))
        .map(|cfg| db.state(&InstanceId::from(cfg.id.as_str())))
        .collect()
}

/// An instance id that can sit in a key and a URL as it is.
fn safe_id(id: &str) -> bool {
    !id.is_empty()
        && id != "all"
        && id.len() <= 64
        && id.chars().all(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_'))
}

// ── The allow-list ─────────────────────────────────────────────────────────

/// Whether `key` (relative to the prefix) is something the snapshot may
/// publish. Every upload is checked against this, on top of the renderer only
/// producing these: a new file has to be added here on purpose.
pub(crate) fn allowed_key(key: &str) -> bool {
    static RE: std::sync::LazyLock<regex::Regex> = std::sync::LazyLock::new(|| {
        regex::Regex::new(concat!(
            r"^(?:",
            r"manifest\.json",
            r"|g/instances(?:~kind-range)?\.json",
            r"|g/wiki/pages\.json",
            r"|g/wiki/pages/[A-Za-z0-9._-]+(?:/[A-Za-z0-9._-]+)*\.json",
            r"|i/[A-Za-z0-9_-]+/(?:online|objectives|frontline|rounds|leaderboard|stats|news|state)\.json",
            r"|i/[A-Za-z0-9_-]+/wiki/facts\.json",
            r"|i/[A-Za-z0-9_-]+/rounds/[0-9]+/summary\.json",
            r")$",
        ))
        .expect("snapshot allow-list regex")
    });
    // A `..` segment would let a wiki slug climb out of its folder on a disk sink.
    RE.is_match(key) && !key.split('/').any(|s| s == "." || s == "..")
}

// ── Rendering ──────────────────────────────────────────────────────────────

/// One file of the snapshot. `body: None` means it could not be rendered this
/// time (an engine RPC timed out, say): the copy already published stays.
pub(crate) struct Planned {
    pub(crate) key: String,
    pub(crate) body: Option<Vec<u8>>,
}

/// The body of a handler's reply, if it answered 200.
async fn body_of<R: Reply>(r: std::result::Result<R, crate::Error>) -> Option<Vec<u8>> {
    let resp = match r {
        Ok(r) => r.into_response(),
        Err(e) => {
            log::debug!("snapshot: render failed: {:#}", e.0);
            return None;
        }
    };
    if !resp.status().is_success() {
        log::debug!("snapshot: render answered {}", resp.status());
        return None;
    }
    warp::hyper::body::to_bytes(resp.into_body()).await.ok().map(|b| b.to_vec())
}

fn query(pairs: &[(&str, &str)]) -> HashMap<String, String> {
    pairs.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect()
}

/// A `warp::path::Tail` for a wiki slug, which only a filter can make.
async fn wiki_tail(slug: &str) -> Option<warp::path::Tail> {
    use warp::Filter;
    let f = warp::path("w").and(warp::path::tail());
    warp::test::request().path(&format!("/w/{slug}")).filter(&f).await.ok()
}

/// Render every file of the snapshot, as the live routes would answer an
/// anonymous request.
pub(crate) async fn render(src: &Source, last_seen: &mut HashMap<InstanceId, DateTime<Utc>>) -> Vec<Planned> {
    let db = &src.db;
    let mut out = vec![];
    let mut put = |key: String, body: Option<Vec<u8>>| out.push(Planned { key, body });

    // No session: exactly what a signed-out visitor gets, which leaves out
    // every non-public instance.
    put("g/instances.json".into(), body_of(crate::api_instances(None, db.clone(), query(&[])).await).await);
    put(
        "g/instances~kind-range.json".into(),
        body_of(crate::api_instances(None, db.clone(), query(&[("kind", "range")])).await).await,
    );

    // The wiki is not instance-scoped.
    put("g/wiki/pages.json".into(), body_of(crate::api_wiki_list(db.clone()).await).await);
    let pages = tokio::task::block_in_place(|| db.wiki_list_pages()).unwrap_or_default();
    for (slug, _) in pages {
        if !crate::valid_wiki_slug(&slug) {
            continue;
        }
        let Some(tail) = wiki_tail(&slug).await else { continue };
        put(format!("g/wiki/pages/{slug}.json"), body_of(crate::api_wiki_get(tail, db.clone()).await).await);
    }

    for inst in public_campaigns(db) {
        let id = inst.id.to_string();
        let k = |name: &str| format!("i/{id}/{name}.json");
        put(k("online"), body_of(crate::api_online(db.clone(), inst.clone()).await).await);
        put(k("objectives"), body_of(crate::api_objectives(db.clone(), None, inst.clone()).await).await);
        put(k("frontline"), body_of(crate::api_frontline(db.clone(), None, inst.clone()).await).await);
        put(k("rounds"), body_of(crate::api_rounds(db.clone(), query(&[]), inst.clone()).await).await);
        put(k("leaderboard"), body_of(crate::api_leaderboard(db.clone(), inst.clone(), query(&[])).await).await);
        let stats = body_of(crate::api_stats(db.clone(), src.bot.clone(), inst.clone()).await).await;
        let players = stats.as_deref().and_then(players_from_stats);
        put(k("stats"), stats);
        put(
            k("news"),
            body_of(crate::api_news(db.clone(), None, inst.clone(), src.news_images.clone()).await).await,
        );
        put(k("wiki/facts"), body_of(crate::api_wiki_facts(db.clone(), inst.clone()).await).await);
        for rid in summary_rounds(db, &inst) {
            put(
                format!("i/{id}/rounds/{rid}/summary.json"),
                body_of(crate::api_round_summary(rid, db.clone(), inst.clone()).await).await,
            );
        }
        put(k("state"), Some(state_json(db, &inst, players, last_seen).await));
    }
    for inst in public_ranges(db) {
        let key = format!("i/{}/state.json", inst.id);
        put(key, Some(state_json(db, &inst, None, last_seen).await));
    }
    out
}

/// The rounds worth a summary file: the one running and the last one finished
/// (the round-over rundown the site and dashboard show).
fn summary_rounds(db: &StatsDb, inst: &Inst) -> Vec<u64> {
    let rounds = tokio::task::block_in_place(|| db.latest_rounds_for(&inst.id)).unwrap_or_default();
    let mut ids = vec![];
    if let Some((_, rid, _)) = rounds.iter().find(|(_, _, r)| r.end.is_none()) {
        ids.push(rid.0);
    }
    if let Some((_, rid, _)) = rounds.iter().filter(|(_, _, r)| r.end.is_some()).max_by_key(|(_, _, r)| r.end) {
        ids.push(rid.0);
    }
    ids
}

fn players_from_stats(body: &[u8]) -> Option<serde_json::Value> {
    let v: serde_json::Value = serde_json::from_slice(body).ok()?;
    let blue = v.get("blue_online")?.as_u64()?;
    let red = v.get("red_online")?.as_u64()?;
    Some(serde_json::json!({ "blue": blue, "red": red, "total": blue + red }))
}

/// `i/<id>/state.json`: what the public site's status page says about a
/// server while bfdb itself is unreachable. `last_seen` is the last time this
/// bfdb saw the DCS engine answer; the snapshot's own age (the manifest's
/// `generated_at`) says when bfdb itself was last up.
async fn state_json(
    db: &StatsDb,
    inst: &Inst,
    players: Option<serde_json::Value>,
    last_seen: &mut HashMap<InstanceId, DateTime<Utc>>,
) -> Vec<u8> {
    // The same probe /api/health runs (and caches), so this adds no RPC
    // traffic while a dashboard is open.
    let online = match body_of(crate::api_health(db.clone(), inst.clone()).await).await {
        Some(b) => serde_json::from_slice::<serde_json::Value>(&b)
            .ok()
            .and_then(|v| v.pointer("/engine/reachable").and_then(|r| r.as_bool()))
            .unwrap_or(false),
        None => false,
    };
    if online {
        // Whole minutes, so an idle server's file does not change every cycle.
        let t = Utc::now().timestamp();
        if let Some(now) = DateTime::from_timestamp(t - t % 60, 0) {
            last_seen.insert(inst.id.clone(), now);
        }
    }
    let rounds = tokio::task::block_in_place(|| db.latest_rounds_for(&inst.id)).unwrap_or_default();
    let active = rounds.iter().find(|(_, _, r)| r.end.is_none()).map(|(scenario, rid, r)| {
        serde_json::json!({
            "id": rid.0,
            "number": db.round_number(*rid),
            "scenario": scenario.to_string(),
            "start": r.start.to_rfc3339(),
        })
    });
    let dcs_version = tokio::task::block_in_place(|| db.unit_db_latest(&inst.id))
        .ok()
        .flatten()
        .map(|(v, _)| v);
    let mission_time = inst
        .last_stat
        .lock()
        .ok()
        .and_then(|g| g.map(|(t, _)| t.to_rfc3339()));
    let v = serde_json::json!({
        "instance": inst.id.to_string(),
        "label": inst.cfg.label(),
        "kind": inst.cfg.kind,
        "online": online,
        "last_seen": last_seen.get(&inst.id).map(|t| t.to_rfc3339()),
        "mission": inst.live_sortie_public(),
        "active_round": active,
        // The engine's clock on the newest stat bfdb ingested.
        "mission_time": if online { mission_time } else { None },
        "players": if online { players } else { None },
        "dcs_version": dcs_version,
        "bfdb": crate::build_info_json(),
    });
    serde_json::to_vec(&v).unwrap_or_default()
}

// ── Publishing ─────────────────────────────────────────────────────────────

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct PublishReport {
    pub(crate) uploaded: usize,
    pub(crate) unchanged: usize,
    pub(crate) deleted: usize,
    pub(crate) failed: usize,
}

pub(crate) struct Publisher {
    sink: Sink,
    /// What is in the bucket now: key -> (sha256 of the uncompressed body, bytes).
    published: BTreeMap<String, (String, usize)>,
    last_seen: HashMap<InstanceId, DateTime<Utc>>,
    seeded: bool,
    /// The last warning, so an outage logs once rather than every minute.
    last_warning: Option<String>,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct ManifestFile {
    sha256: String,
    bytes: usize,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct Manifest {
    version: u32,
    generated_at: DateTime<Utc>,
    /// The instance a request naming none is answered for.
    default_instance: String,
    /// The campaign instances that have files under `i/`.
    instances: Vec<String>,
    files: BTreeMap<String, ManifestFile>,
}

fn sha256_hex(b: &[u8]) -> String {
    hex(&Sha256::digest(b))
}

fn hex(b: &[u8]) -> String {
    b.iter().map(|x| format!("{x:02x}")).collect()
}

impl Publisher {
    pub(crate) fn new(sink: Sink) -> Self {
        Self { sink, published: BTreeMap::new(), last_seen: HashMap::new(), seeded: false, last_warning: None }
    }

    fn warn(&mut self, msg: String) {
        if self.last_warning.as_deref() != Some(msg.as_str()) {
            log::warn!("{msg}");
            self.last_warning = Some(msg);
        }
    }

    /// After a restart, pick up what is already in the bucket, so unchanged
    /// files are not uploaded again and `last_seen` survives.
    async fn seed(&mut self, db: &StatsDb) {
        self.seeded = true;
        if let Ok(Some(raw)) = self.sink.get("manifest.json").await {
            if let Ok(m) = serde_json::from_slice::<Manifest>(&raw) {
                self.published = m.files.into_iter().map(|(k, f)| (k, (f.sha256, f.bytes))).collect();
            }
        }
        for inst in public_campaigns(db).into_iter().chain(public_ranges(db)) {
            let key = format!("i/{}/state.json", inst.id);
            let Ok(Some(raw)) = self.sink.get(&key).await else { continue };
            let seen = serde_json::from_slice::<serde_json::Value>(&raw)
                .ok()
                .and_then(|v| v.get("last_seen")?.as_str().map(str::to_string))
                .and_then(|s| DateTime::parse_from_rfc3339(&s).ok())
                .map(|t| t.with_timezone(&Utc));
            if let Some(t) = seen {
                self.last_seen.insert(inst.id.clone(), t);
            }
        }
    }

    /// One snapshot: render, upload what changed, drop what is gone, then the
    /// manifest (always: its `generated_at` is how old the front ends say the
    /// data is).
    pub(crate) async fn publish(&mut self, src: &Source) -> Result<PublishReport> {
        if !self.seeded {
            self.seed(&src.db).await;
        }
        let planned = render(src, &mut self.last_seen).await;
        let mut report = PublishReport::default();
        let mut next: BTreeMap<String, (String, usize)> = BTreeMap::new();
        let mut uploads = vec![];
        for p in planned {
            if !allowed_key(&p.key) {
                // A renderer bug, not a runtime condition: never publish it.
                log::error!("snapshot: refusing to publish {:?}, which is not on the allow-list", p.key);
                continue;
            }
            match p.body {
                Some(body) => {
                    let hash = sha256_hex(&body);
                    let same = self.published.get(&p.key).is_some_and(|(h, _)| *h == hash);
                    next.insert(p.key.clone(), (hash, body.len()));
                    if same {
                        report.unchanged += 1;
                    } else {
                        uploads.push((p.key, body));
                    }
                }
                // Could not render it now: keep what was published.
                None => {
                    if let Some(prev) = self.published.get(&p.key) {
                        next.insert(p.key.clone(), prev.clone());
                    }
                }
            }
        }

        let sink = &self.sink;
        let results: Vec<(String, Result<()>)> = futures::stream::iter(uploads)
            .map(|(key, body)| async move {
                let r = sink.put(&key, body).await;
                (key, r)
            })
            .buffer_unordered(4)
            .collect()
            .await;
        let mut first_err = None;
        for (key, r) in results {
            match r {
                Ok(()) => report.uploaded += 1,
                Err(e) => {
                    report.failed += 1;
                    first_err.get_or_insert_with(|| format!("{key}: {e:#}"));
                    // Not up there: the manifest must not claim it, and the
                    // next cycle tries again.
                    match self.published.get(&key) {
                        Some(prev) => next.insert(key, prev.clone()),
                        None => next.remove(&key),
                    };
                }
            }
        }

        // Gone from the plan entirely: a deleted wiki page, a round summary
        // that rotated out. (A file that merely failed to render was kept above.)
        let gone: Vec<String> = self
            .published
            .keys()
            .filter(|k| *k != "manifest.json" && !next.contains_key(*k))
            .cloned()
            .collect();
        for key in gone {
            match self.sink.delete(&key).await {
                Ok(()) => report.deleted += 1,
                Err(e) => {
                    report.failed += 1;
                    first_err.get_or_insert_with(|| format!("delete {key}: {e:#}"));
                }
            }
        }

        let manifest = Manifest {
            version: 1,
            generated_at: Utc::now(),
            default_instance: src.db.instances().default_id().to_string(),
            instances: public_campaigns(&src.db).iter().map(|i| i.id.to_string()).collect(),
            files: next
                .iter()
                .map(|(k, (h, n))| (k.clone(), ManifestFile { sha256: h.clone(), bytes: *n }))
                .collect(),
        };
        let body = serde_json::to_vec(&manifest)?;
        self.sink.put("manifest.json", body).await.context("uploading manifest.json")?;
        self.published = next;
        match first_err {
            Some(e) => self.warn(format!("snapshot: {} file(s) failed, first: {e}", report.failed)),
            None => {
                if self.last_warning.take().is_some() {
                    log::info!("snapshot: publishing again");
                }
            }
        }
        Ok(report)
    }
}

// ── Sinks ──────────────────────────────────────────────────────────────────

/// Where files go: an S3-compatible bucket, a folder, or (tests) memory.
pub(crate) enum Sink {
    S3(S3Sink),
    Dir(PathBuf),
    #[cfg(test)]
    Memory(Arc<std::sync::Mutex<BTreeMap<String, Vec<u8>>>>),
}

pub(crate) struct S3Sink {
    http: reqwest::Client,
    /// `<endpoint>/<bucket>`, no trailing slash.
    base: reqwest::Url,
    prefix: String,
    signer: SigV4,
}

/// Bodies at least this big are stored gzip'd (`Content-Encoding: gzip`,
/// which browsers undo themselves).
const GZIP_MIN: usize = 1024;
/// Short: a snapshot is replaced every minute while bfdb is up.
const CACHE_CONTROL: &str = "public, max-age=30";
const MANIFEST_CACHE_CONTROL: &str = "public, max-age=10";

impl Sink {
    fn from_args(a: &SnapshotArgs) -> Result<Option<Sink>> {
        let prefix = a.prefix.as_deref().unwrap_or("").trim_matches('/');
        let prefix = if prefix.is_empty() { String::new() } else { format!("{prefix}/") };
        match (&a.endpoint, &a.dir) {
            (Some(_), Some(_)) => bail!("--snapshot-endpoint and --snapshot-dir are exclusive; pick one"),
            (None, None) => Ok(None),
            (None, Some(dir)) => {
                std::fs::create_dir_all(dir).with_context(|| format!("creating {}", dir.display()))?;
                Ok(Some(Sink::Dir(dir.join(prefix))))
            }
            (Some(endpoint), None) => {
                let bucket = a.bucket.as_deref().filter(|b| !b.is_empty()).ok_or_else(|| anyhow!("--snapshot-bucket is required"))?;
                let key_id = crate::secret_from(None, a.access_key_id_file.as_deref(), "BFDB_SNAPSHOT_ACCESS_KEY_ID")?
                    .ok_or_else(|| anyhow!("no access key id (--snapshot-access-key-id-file or $BFDB_SNAPSHOT_ACCESS_KEY_ID)"))?;
                let secret = crate::secret_from(None, a.secret_access_key_file.as_deref(), "BFDB_SNAPSHOT_SECRET_ACCESS_KEY")?
                    .ok_or_else(|| anyhow!("no secret access key (--snapshot-secret-access-key-file or $BFDB_SNAPSHOT_SECRET_ACCESS_KEY)"))?;
                Ok(Some(Sink::S3(S3Sink::new(endpoint, bucket, a.region.as_deref().unwrap_or("auto"), &prefix, key_id, secret)?)))
            }
        }
    }

    fn describe(&self) -> String {
        match self {
            Sink::S3(s) => format!("{}/{}", s.base, s.prefix),
            Sink::Dir(d) => d.display().to_string(),
            #[cfg(test)]
            Sink::Memory(_) => "memory".into(),
        }
    }

    async fn put(&self, key: &str, body: Vec<u8>) -> Result<()> {
        match self {
            Sink::S3(s) => s.put(key, body).await,
            Sink::Dir(d) => {
                let path = d.join(key);
                let tmp = path.with_extension("json.tmp");
                tokio::task::block_in_place(|| -> Result<()> {
                    if let Some(parent) = path.parent() {
                        std::fs::create_dir_all(parent)?;
                    }
                    // Write then rename, so a reader never sees half a file.
                    std::fs::write(&tmp, &body)?;
                    std::fs::rename(&tmp, &path)?;
                    Ok(())
                })
            }
            #[cfg(test)]
            Sink::Memory(m) => {
                m.lock().unwrap().insert(key.to_string(), body);
                Ok(())
            }
        }
    }

    async fn get(&self, key: &str) -> Result<Option<Vec<u8>>> {
        match self {
            Sink::S3(s) => s.get(key).await,
            Sink::Dir(d) => match std::fs::read(d.join(key)) {
                Ok(b) => Ok(Some(b)),
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
                Err(e) => Err(e.into()),
            },
            #[cfg(test)]
            Sink::Memory(m) => Ok(m.lock().unwrap().get(key).cloned()),
        }
    }

    async fn delete(&self, key: &str) -> Result<()> {
        match self {
            Sink::S3(s) => s.delete(key).await,
            Sink::Dir(d) => match std::fs::remove_file(d.join(key)) {
                Err(e) if e.kind() != std::io::ErrorKind::NotFound => Err(e.into()),
                _ => Ok(()),
            },
            #[cfg(test)]
            Sink::Memory(m) => {
                m.lock().unwrap().remove(key);
                Ok(())
            }
        }
    }
}

fn gzip(body: &[u8]) -> Result<Vec<u8>> {
    use std::io::Write;
    let mut enc = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    enc.write_all(body)?;
    Ok(enc.finish()?)
}

fn gunzip_if_needed(body: Vec<u8>) -> Result<Vec<u8>> {
    if body.starts_with(&[0x1f, 0x8b]) {
        use std::io::Read;
        let mut out = vec![];
        flate2::read::GzDecoder::new(&body[..]).read_to_end(&mut out)?;
        Ok(out)
    } else {
        Ok(body)
    }
}

/// RFC 3986 percent-encoding of a key, keeping `/` (S3 does not want the
/// path encoded twice, and our keys rarely need it once).
fn uri_encode_path(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for b in s.bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' | b'/' => out.push(b as char),
            _ => out.push_str(&format!("%{b:02X}")),
        }
    }
    out
}

impl S3Sink {
    pub(crate) fn new(endpoint: &str, bucket: &str, region: &str, prefix: &str, key_id: String, secret: String) -> Result<Self> {
        let base = reqwest::Url::parse(&format!("{}/{}", endpoint.trim_end_matches('/'), bucket))
            .with_context(|| format!("--snapshot-endpoint {endpoint:?} is not a URL"))?;
        if base.scheme() != "https" && !matches!(base.host_str(), Some("localhost" | "127.0.0.1")) {
            bail!("--snapshot-endpoint must be https");
        }
        let http = reqwest::Client::builder()
            .timeout(Duration::from_secs(30))
            .connect_timeout(Duration::from_secs(10))
            .build()?;
        Ok(Self {
            http,
            base,
            prefix: prefix.to_string(),
            signer: SigV4 { key_id, secret, region: region.to_string(), service: "s3".into() },
        })
    }

    fn url(&self, key: &str) -> (reqwest::Url, String, String) {
        let path = format!("{}/{}{}", self.base.path().trim_end_matches('/'), uri_encode_path(&self.prefix), uri_encode_path(key));
        let mut url = self.base.clone();
        url.set_path(&path);
        let host = match url.port() {
            Some(p) => format!("{}:{p}", url.host_str().unwrap_or_default()),
            None => url.host_str().unwrap_or_default().to_string(),
        };
        (url, host, path)
    }

    /// A signed request. `headers` are extra signed headers (lowercase names).
    fn request(&self, method: reqwest::Method, key: &str, headers: Vec<(String, String)>, payload_hash: &str) -> reqwest::RequestBuilder {
        let (url, host, path) = self.url(key);
        let now = Utc::now();
        let mut signed = headers;
        signed.push(("host".into(), host));
        signed.push(("x-amz-content-sha256".into(), payload_hash.to_string()));
        signed.push(("x-amz-date".into(), now.format("%Y%m%dT%H%M%SZ").to_string()));
        let auth = self.signer.authorization(method.as_str(), &path, "", &signed, payload_hash, now);
        let mut rb = self.http.request(method, url).header("authorization", auth);
        for (k, v) in signed {
            // reqwest sets Host itself from the URL, to the same value.
            if k != "host" {
                rb = rb.header(k, v);
            }
        }
        rb
    }

    async fn put(&self, key: &str, body: Vec<u8>) -> Result<()> {
        let mut headers = vec![
            ("cache-control".into(), if key == "manifest.json" { MANIFEST_CACHE_CONTROL } else { CACHE_CONTROL }.into()),
            ("content-type".into(), "application/json; charset=utf-8".into()),
        ];
        let body = if body.len() >= GZIP_MIN {
            headers.push(("content-encoding".into(), "gzip".into()));
            gzip(&body)?
        } else {
            body
        };
        let hash = sha256_hex(&body);
        let res = self.request(reqwest::Method::PUT, key, headers, &hash).body(body).send().await?;
        if !res.status().is_success() {
            let status = res.status();
            let text: String = res.text().await.unwrap_or_default().chars().take(300).collect();
            bail!("PUT {key}: {status} {text}");
        }
        Ok(())
    }

    async fn get(&self, key: &str) -> Result<Option<Vec<u8>>> {
        let res = self.request(reqwest::Method::GET, key, vec![], EMPTY_SHA256).send().await?;
        match res.status() {
            s if s.is_success() => Ok(Some(gunzip_if_needed(res.bytes().await?.to_vec())?)),
            reqwest::StatusCode::NOT_FOUND => Ok(None),
            s => bail!("GET {key}: {s}"),
        }
    }

    async fn delete(&self, key: &str) -> Result<()> {
        let res = self.request(reqwest::Method::DELETE, key, vec![], EMPTY_SHA256).send().await?;
        if !res.status().is_success() && res.status() != reqwest::StatusCode::NOT_FOUND {
            bail!("DELETE {key}: {}", res.status());
        }
        Ok(())
    }
}

const EMPTY_SHA256: &str = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";

/// AWS Signature Version 4, header form, unsigned query.
/// <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html>
pub(crate) struct SigV4 {
    key_id: String,
    secret: String,
    region: String,
    service: String,
}

type HmacSha256 = Hmac<Sha256>;

fn hmac(key: &[u8], msg: &[u8]) -> Vec<u8> {
    let mut m = HmacSha256::new_from_slice(key).expect("HMAC takes any key length");
    m.update(msg);
    m.finalize().into_bytes().to_vec()
}

impl SigV4 {
    /// The `Authorization` header for a request. `path` is already
    /// URI-encoded; `headers` are every signed header, lowercase names.
    pub(crate) fn authorization(
        &self,
        method: &str,
        path: &str,
        query: &str,
        headers: &[(String, String)],
        payload_hash: &str,
        now: DateTime<Utc>,
    ) -> String {
        let mut hs: Vec<(String, String)> =
            headers.iter().map(|(k, v)| (k.to_ascii_lowercase(), v.trim().to_string())).collect();
        hs.sort();
        let canonical_headers: String = hs.iter().map(|(k, v)| format!("{k}:{v}\n")).collect();
        let signed_headers = hs.iter().map(|(k, _)| k.as_str()).collect::<Vec<_>>().join(";");
        let canonical = format!("{method}\n{path}\n{query}\n{canonical_headers}\n{signed_headers}\n{payload_hash}");
        let amz_date = now.format("%Y%m%dT%H%M%SZ").to_string();
        let day = now.format("%Y%m%d").to_string();
        let scope = format!("{day}/{}/{}/aws4_request", self.region, self.service);
        let to_sign = format!("AWS4-HMAC-SHA256\n{amz_date}\n{scope}\n{}", sha256_hex(canonical.as_bytes()));
        let k = hmac(format!("AWS4{}", self.secret).as_bytes(), day.as_bytes());
        let k = hmac(&k, self.region.as_bytes());
        let k = hmac(&k, self.service.as_bytes());
        let k = hmac(&k, b"aws4_request");
        let sig = hex(&hmac(&k, to_sign.as_bytes()));
        format!(
            "AWS4-HMAC-SHA256 Credential={}/{scope}, SignedHeaders={signed_headers}, Signature={sig}",
            self.key_id
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::WikiPage;
    use crate::instance::{InstancesFile, Registry};

    /// AWS's own worked example ("GET Object" in the SigV4 header-auth docs).
    #[test]
    fn sigv4_matches_the_aws_example() {
        let s = SigV4 {
            key_id: "AKIAIOSFODNN7EXAMPLE".into(),
            secret: "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY".into(),
            region: "us-east-1".into(),
            service: "s3".into(),
        };
        let now = DateTime::parse_from_rfc3339("2013-05-24T00:00:00Z").unwrap().with_timezone(&Utc);
        let headers = vec![
            ("host".to_string(), "examplebucket.s3.amazonaws.com".to_string()),
            ("range".to_string(), "bytes=0-9".to_string()),
            ("x-amz-content-sha256".to_string(), EMPTY_SHA256.to_string()),
            ("x-amz-date".to_string(), "20130524T000000Z".to_string()),
        ];
        assert_eq!(
            s.authorization("GET", "/test.txt", "", &headers, EMPTY_SHA256, now),
            "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, \
             SignedHeaders=host;range;x-amz-content-sha256;x-amz-date, \
             Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"
        );
        assert_eq!(sha256_hex(b""), EMPTY_SHA256);
    }

    #[test]
    fn allow_list_takes_public_files_only() {
        for ok in [
            "manifest.json",
            "g/instances.json",
            "g/instances~kind-range.json",
            "g/wiki/pages.json",
            "g/wiki/pages/gameplay/objectives.json",
            "i/vs1/online.json",
            "i/vs1/stats.json",
            "i/vs1/state.json",
            "i/vs1/wiki/facts.json",
            "i/vs1/rounds/108000656/summary.json",
        ] {
            assert!(allowed_key(ok), "{ok} should be allowed");
        }
        for bad in [
            "i/vs1/briefing.json",
            "i/vs1/warehouse.json",
            "i/vs1/situation.json",
            "i/vs1/groundwar.json",
            "i/vs1/hq.json",
            "i/vs1/intel/captures.json",
            "i/vs1/command.json",
            "i/vs1/cockpit/context.json",
            "i/vs1/trails.json",
            "i/vs1/units.json",
            "i/vs1/health.json",
            "i/vs1/kills.json",
            "g/logs/bfdb.json",
            "g/admin/sessions.json",
            "g/instances~all-1.json",
            "i/vs1/rounds/1/report.json",
            "g/wiki/pages/../../secret.json",
            "i/../online.json",
            "/manifest.json",
        ] {
            assert!(!allowed_key(bad), "{bad} must not be allowed");
        }
    }

    fn temp_db(instances: serde_json::Value) -> (StatsDb, PathBuf) {
        let dir = std::env::temp_dir().join(format!("bfdb-snapshot-{}", uuid::Uuid::new_v4()));
        let file: InstancesFile = serde_json::from_value(instances).unwrap();
        let db = StatsDb::new(&HashMap::new(), dir.join("db"), Registry::new(file).unwrap(), None, None).unwrap();
        (db, dir)
    }

    fn source(db: &StatsDb) -> Source {
        Source { db: db.clone(), bot: Arc::new(None), news_images: None }
    }

    fn page(title: &str, content: &str) -> WikiPage {
        WikiPage {
            title: title.into(),
            section: "Guide".into(),
            order: 1,
            content: content.into(),
            updated_at: Utc::now(),
            updated_by: "test".into(),
        }
    }

    fn three_instances() -> serde_json::Value {
        serde_json::json!({
            "default": "vs1",
            "instances": [
                { "id": "vs1", "export_port": 15001 },
                { "id": "staging", "public": false, "export_port": 15002 },
                { "id": "range1", "kind": "range", "export_port": 15003 },
            ]
        })
    }

    /// The published set, from a real render of a database with a public
    /// campaign, a private (staging) campaign and a range, is exactly the
    /// allow-listed public files -- and nothing of the private instance.
    #[tokio::test(flavor = "multi_thread")]
    async fn published_set_is_allow_listed() {
        let (db, dir) = temp_db(three_instances());
        db.wiki_save_page("gameplay/objectives", page("Objectives", "Take them.")).unwrap();
        db.wiki_save_page("start", page("Start", "Hello.")).unwrap();
        let mem = Arc::new(std::sync::Mutex::new(BTreeMap::new()));
        let mut p = Publisher::new(Sink::Memory(mem.clone()));
        let r = p.publish(&source(&db)).await.unwrap();
        assert_eq!(r.failed, 0);

        let keys: Vec<String> = mem.lock().unwrap().keys().cloned().collect();
        for k in &keys {
            assert!(allowed_key(k), "published {k}, which is not allow-listed");
            assert!(!k.contains("staging"), "published the private instance: {k}");
        }
        // Every wiki page (bfdb seeds the player docs, plus the two above)...
        let wiki: Vec<String> = keys.iter().filter(|k| k.starts_with("g/wiki/pages/")).cloned().collect();
        let mut pages: Vec<String> =
            db.wiki_list_pages().unwrap().into_iter().map(|(slug, _)| format!("g/wiki/pages/{slug}.json")).collect();
        pages.sort();
        assert_eq!(wiki, pages);
        assert!(wiki.contains(&"g/wiki/pages/gameplay/objectives.json".to_string()));
        assert!(wiki.contains(&"g/wiki/pages/start.json".to_string()));
        // ...and exactly these besides.
        let rest: Vec<&String> = keys.iter().filter(|k| !k.starts_with("g/wiki/pages/")).collect();
        let expected = [
            "g/instances.json",
            "g/instances~kind-range.json",
            "g/wiki/pages.json",
            "i/range1/state.json",
            "i/vs1/frontline.json",
            "i/vs1/leaderboard.json",
            "i/vs1/news.json",
            "i/vs1/objectives.json",
            "i/vs1/online.json",
            "i/vs1/rounds.json",
            "i/vs1/state.json",
            "i/vs1/stats.json",
            "i/vs1/wiki/facts.json",
            "manifest.json",
        ];
        assert_eq!(rest, expected);

        // The instance list is the signed-out one: no staging server in it.
        let list: serde_json::Value = serde_json::from_slice(&mem.lock().unwrap()["g/instances.json"]).unwrap();
        let ids: Vec<&str> = list["instances"].as_array().unwrap().iter().map(|i| i["id"].as_str().unwrap()).collect();
        assert_eq!(ids, ["vs1"]);

        // Same shape as the live route: render it the way warp would.
        let live = body_of(crate::api_wiki_list(db.clone()).await).await.unwrap();
        assert_eq!(mem.lock().unwrap()["g/wiki/pages.json"], live);

        let m: serde_json::Value = serde_json::from_slice(&mem.lock().unwrap()["manifest.json"]).unwrap();
        assert_eq!(m["default_instance"], "vs1");
        assert_eq!(m["instances"], serde_json::json!(["vs1"]));
        assert_eq!(m["files"].as_object().unwrap().len(), keys.len() - 1);
        let _ = std::fs::remove_dir_all(dir);
    }

    /// Only changed files go up again; a deleted wiki page is removed; a
    /// restarted publisher picks up the manifest instead of re-uploading.
    #[tokio::test(flavor = "multi_thread")]
    async fn uploads_changes_only() {
        let (db, dir) = temp_db(three_instances());
        db.wiki_save_page("start", page("Start", "Hello.")).unwrap();
        db.wiki_save_page("old", page("Old", "Bye.")).unwrap();
        let mem = Arc::new(std::sync::Mutex::new(BTreeMap::new()));
        let src = source(&db);
        let mut p = Publisher::new(Sink::Memory(mem.clone()));
        let first = p.publish(&src).await.unwrap();
        assert!(first.uploaded > 5);

        let again = p.publish(&src).await.unwrap();
        assert_eq!(again.uploaded, 0, "nothing changed, nothing uploaded");

        db.wiki_save_page("start", page("Start", "Hello again.")).unwrap();
        db.wiki_delete_page("old").unwrap();
        let changed = p.publish(&src).await.unwrap();
        // The page itself and the page list (its titles are unchanged, but
        // one entry is gone).
        assert_eq!(changed.uploaded, 2);
        assert_eq!(changed.deleted, 1);
        assert!(!mem.lock().unwrap().contains_key("g/wiki/pages/old.json"));

        let mut restarted = Publisher::new(Sink::Memory(mem.clone()));
        let r = restarted.publish(&src).await.unwrap();
        assert_eq!(r.uploaded, 0, "a restart re-uses the manifest's hashes");
        let _ = std::fs::remove_dir_all(dir);
    }

    /// The whole S3 path against a mock bucket: signed PUTs, gzip for the
    /// big files, the cache header, GET for seeding, DELETE.
    #[tokio::test(flavor = "multi_thread")]
    async fn publishes_to_a_mock_s3() {
        use warp::Filter;
        type Store = Arc<std::sync::Mutex<BTreeMap<String, (Vec<u8>, warp::http::HeaderMap)>>>;
        let store: Store = Default::default();
        let s = store.clone();
        let put = warp::put()
            .and(warp::path::full())
            .and(warp::header::headers_cloned())
            .and(warp::body::bytes())
            .map(move |p: warp::path::FullPath, h: warp::http::HeaderMap, b: bytes::Bytes| {
                let auth = h.get("authorization").and_then(|v| v.to_str().ok()).unwrap_or("");
                let ok = auth.starts_with("AWS4-HMAC-SHA256 Credential=AKIDTEST/")
                    && auth.contains("/auto/s3/aws4_request, SignedHeaders=")
                    && h.get("x-amz-content-sha256").and_then(|v| v.to_str().ok()) == Some(&sha256_hex(&b));
                if !ok {
                    return warp::http::StatusCode::FORBIDDEN;
                }
                s.lock().unwrap().insert(p.as_str().to_string(), (b.to_vec(), h));
                warp::http::StatusCode::OK
            });
        let s = store.clone();
        let get = warp::get().and(warp::path::full()).map(move |p: warp::path::FullPath| {
            match s.lock().unwrap().get(p.as_str()) {
                Some((b, _)) => warp::http::Response::builder().status(200).body(b.clone()).unwrap(),
                None => warp::http::Response::builder().status(404).body(vec![]).unwrap(),
            }
        });
        let s = store.clone();
        let del = warp::delete().and(warp::path::full()).map(move |p: warp::path::FullPath| {
            s.lock().unwrap().remove(p.as_str());
            warp::http::StatusCode::NO_CONTENT
        });
        let (addr, server) = warp::serve(put.or(get).or(del)).bind_ephemeral(([127, 0, 0, 1], 0));
        tokio::spawn(server);

        let (db, dir) = temp_db(three_instances());
        let long = "x".repeat(4000);
        db.wiki_save_page("start", page("Start", &long)).unwrap();
        let endpoint = format!("http://127.0.0.1:{}", addr.port());
        let sink = S3Sink::new(&endpoint, "snap", "auto", "prod/", "AKIDTEST".into(), "secret".into()).unwrap();
        let mut p = Publisher::new(Sink::S3(sink));
        let r = p.publish(&source(&db)).await.unwrap();
        assert_eq!(r.failed, 0, "every PUT was signed and accepted");

        {
            let st = store.lock().unwrap();
            let (page, h) = &st["/snap/prod/g/wiki/pages/start.json"];
            assert_eq!(h.get("content-encoding").unwrap(), "gzip");
            assert_eq!(h.get("cache-control").unwrap(), CACHE_CONTROL);
            let v: serde_json::Value = serde_json::from_slice(&gunzip_if_needed(page.clone()).unwrap()).unwrap();
            assert_eq!(v["content"], long);
            let (small, h) = &st["/snap/prod/i/vs1/online.json"];
            assert!(h.get("content-encoding").is_none());
            assert_eq!(small, b"[]");
            assert!(st.contains_key("/snap/prod/manifest.json"));
        }

        // A fresh publisher seeds itself from the bucket (signed GET).
        let sink = S3Sink::new(&endpoint, "snap", "auto", "prod/", "AKIDTEST".into(), "secret".into()).unwrap();
        let mut p2 = Publisher::new(Sink::S3(sink));
        db.wiki_delete_page("start").unwrap();
        let r = p2.publish(&source(&db)).await.unwrap();
        assert_eq!(r.uploaded, 1, "only the page list changed");
        assert_eq!(r.deleted, 1);
        assert!(!store.lock().unwrap().contains_key("/snap/prod/g/wiki/pages/start.json"));
        let _ = std::fs::remove_dir_all(dir);
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn dir_sink_writes_plain_files() {
        let (db, dir) = temp_db(three_instances());
        db.wiki_save_page("a/b", page("B", "content")).unwrap();
        let out = dir.join("snap");
        let args = SnapshotArgs { dir: Some(out.clone()), ..Default::default() };
        let mut p = Publisher::new(Sink::from_args(&args).unwrap().unwrap());
        p.publish(&source(&db)).await.unwrap();
        let page: serde_json::Value = serde_json::from_slice(&std::fs::read(out.join("g/wiki/pages/a/b.json")).unwrap()).unwrap();
        assert_eq!(page["slug"], "a/b");
        assert!(out.join("manifest.json").exists());
        let _ = std::fs::remove_dir_all(dir);
    }

    #[test]
    fn flags_win_over_the_environment() {
        // Names no other test reads, so setting them cannot race one.
        std::env::set_var("BFDB_SNAPSHOT_BUCKET", "from-env");
        std::env::set_var("BFDB_SNAPSHOT_INTERVAL", "120");
        let a = SnapshotArgs { bucket: Some("from-flag".into()), ..Default::default() }.with_env();
        assert_eq!(a.bucket.as_deref(), Some("from-flag"));
        assert_eq!(a.interval_secs, Some(120));
        std::env::remove_var("BFDB_SNAPSHOT_BUCKET");
        std::env::remove_var("BFDB_SNAPSHOT_INTERVAL");
    }

    #[test]
    fn misconfiguration_is_reported() {
        assert!(Sink::from_args(&SnapshotArgs::default()).unwrap().is_none());
        let a = SnapshotArgs { endpoint: Some("https://x.r2.cloudflarestorage.com".into()), ..Default::default() };
        assert!(Sink::from_args(&a).is_err(), "no bucket");
        let a = SnapshotArgs {
            endpoint: Some("http://example.com".into()),
            bucket: Some("b".into()),
            ..Default::default()
        };
        // No credentials in the test environment, or plain http: either way, an error.
        assert!(Sink::from_args(&a).is_err());
    }
}
