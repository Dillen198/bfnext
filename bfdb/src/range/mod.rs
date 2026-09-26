// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Training range support: the bfdb side of a `kind: "range"` instance.
//!
//! A range instance runs the `bfrange` engine instead of bflib. It still
//! writes the ordinary identity stats (connect, slot, takeoff, land ...) to
//! `stats.jsonl`, which the normal ingestion handles, but its real product is
//! `range.jsonl`: one [`RangeRecord`] per graded event -- a bomb, a strafe
//! pass, a carrier pass, an AAR session, a missile-trainer shot.
//!
//! This module
//!   * tails that file into raw-JSON sled trees ([`store`], [`ingest`]),
//!   * serves the range site and the Discord result feed under `/api/range/`
//!     ([`api`]),
//!   * renders result cards as SVG and PNG ([`cards`]) and Discord embeds
//!     ([`discord`]),
//!   * derives per-pilot insights, qualifications and boards ([`insights`],
//!     [`quals`], [`boards`]), and
//!   * fits a drag model to recorded unguided drops ([`ballistics`]).
//!
//! Live state (`query-range`), the spawn catalogue and the weapon database
//! come from the engine over the same netidx RPC path bflib uses, through
//! [`RangeCtx::rpc_cached`] so fifty viewers cost one RPC.
//!
//! [`RangeRecord`]: bfprotocols::range::RangeRecord

pub(crate) mod api;
pub(crate) mod ballistics;
pub(crate) mod boards;
pub(crate) mod cards;
pub(crate) mod discord;
pub(crate) mod ingest;
pub(crate) mod insights;
pub(crate) mod quals;
pub(crate) mod store;

use crate::{
    db::StatsDb,
    instance::{InstanceCfg, InstanceId},
    BotLinkConfig,
};
use anyhow::{anyhow, Result};
use log::{info, warn};
use std::{
    collections::HashMap,
    sync::{Arc, Mutex as StdMutex},
    time::{Duration, Instant},
};
use store::RangeStore;
use uuid::Uuid;

/// Which range instance(s) a request is about.
#[derive(Debug, Clone)]
pub(crate) enum Scope {
    One(Arc<InstanceCfg>),
    /// `?instance=all`: every range instance, for the read-only views.
    All(Vec<Arc<InstanceCfg>>),
}

impl Scope {
    /// Instance ids to read records from.
    pub(crate) fn ids(&self) -> Vec<String> {
        match self {
            Scope::One(c) => vec![c.id.clone()],
            Scope::All(v) => v.iter().map(|c| c.id.clone()).collect(),
        }
    }

    /// The one instance to talk to for anything live (RPCs). `all` means the
    /// first range instance.
    pub(crate) fn primary(&self) -> &Arc<InstanceCfg> {
        match self {
            Scope::One(c) => c,
            Scope::All(v) => &v[0],
        }
    }

    /// A stable cache-key fragment.
    pub(crate) fn key(&self) -> String {
        self.ids().join(",")
    }
}

/// One cached engine RPC reply.
#[derive(Default)]
struct RpcSlot {
    /// When the last attempt (successful or not) finished.
    fetched: Option<Instant>,
    /// The last good reply and when it arrived.
    ok: Option<(Instant, serde_json::Value)>,
    /// Why the last attempt failed, if it did.
    err: Option<String>,
}

/// A small least-recently-used byte cache for rendered PNG cards. Results
/// never change once written, so an entry is only ever evicted for space.
struct Lru {
    cap: usize,
    tick: u64,
    map: HashMap<String, (u64, Arc<Vec<u8>>)>,
}

impl Lru {
    fn new(cap: usize) -> Self {
        Self { cap, tick: 0, map: HashMap::new() }
    }

    fn get(&mut self, k: &str) -> Option<Arc<Vec<u8>>> {
        self.tick += 1;
        let t = self.tick;
        self.map.get_mut(k).map(|(at, v)| {
            *at = t;
            v.clone()
        })
    }

    fn put(&mut self, k: String, v: Arc<Vec<u8>>) {
        self.tick += 1;
        self.map.insert(k, (self.tick, v));
        while self.map.len() > self.cap {
            let oldest = self.map.iter().min_by_key(|(_, (t, _))| *t).map(|(k, _)| k.clone());
            match oldest {
                Some(k) => {
                    self.map.remove(&k);
                }
                None => break,
            }
        }
    }
}

/// Everything the range routes and background tasks share.
pub(crate) struct RangeCtx {
    pub(crate) db: StatsDb,
    pub(crate) store: RangeStore,
    pub(crate) bot: Arc<Option<BotLinkConfig>>,
    /// `--range-site-url`, no trailing slash: where a result's page lives.
    pub(crate) site_url: String,
    /// `--public-api-url`, no trailing slash: the absolute base for card
    /// images handed to Discord.
    pub(crate) api_url: String,
    rpc: StdMutex<HashMap<(String, &'static str), Arc<tokio::sync::Mutex<RpcSlot>>>>,
    pngs: StdMutex<Lru>,
    /// Last spawn/despawn per ucid, for the 1-per-3s rate limit.
    spawn_rl: StdMutex<HashMap<String, Instant>>,
    /// Discord session -> linked ucid, so `/api/range/me` polling does not
    /// hit DCSServerBot on every page load.
    ucids: StdMutex<HashMap<Uuid, (Instant, Option<String>)>>,
    /// Short-lived cache for the whole-window aggregates (boards, greenie,
    /// impacts, calibration), keyed by route + query.
    agg: StdMutex<HashMap<String, (Instant, serde_json::Value)>>,
}

/// How long a Discord -> ucid link lookup is trusted. "Not linked" is
/// re-asked sooner, so a player who has just run `-linkme` is not kept
/// waiting.
const UCID_TTL: Duration = Duration::from_secs(120);
const UNLINKED_TTL: Duration = Duration::from_secs(15);
/// Rendered-card cache size.
const PNG_CACHE: usize = 200;

impl RangeCtx {
    pub(crate) fn new(
        db: StatsDb,
        bot: Arc<Option<BotLinkConfig>>,
        site_url: &str,
        api_url: &str,
    ) -> Result<Arc<Self>> {
        let store = RangeStore::open(db.sled())?;
        Ok(Arc::new(Self {
            db,
            store,
            bot,
            site_url: site_url.trim_end_matches('/').to_string(),
            api_url: api_url.trim_end_matches('/').to_string(),
            rpc: StdMutex::new(HashMap::new()),
            pngs: StdMutex::new(Lru::new(PNG_CACHE)),
            spawn_rl: StdMutex::new(HashMap::new()),
            ucids: StdMutex::new(HashMap::new()),
            agg: StdMutex::new(HashMap::new()),
        }))
    }

    /// Every configured range instance, in configured order.
    pub(crate) fn range_instances(&self) -> Vec<Arc<InstanceCfg>> {
        self.db.instances().all().iter().filter(|c| c.is_range()).cloned().collect()
    }

    /// Resolve `?instance=` (or `?server=`) for a range route. Absent picks
    /// the first range instance; `all` means every range instance; an id that
    /// exists but is not a range is an error rather than a silent fallback.
    pub(crate) fn scope(
        &self,
        q: &HashMap<String, String>,
    ) -> std::result::Result<Scope, String> {
        let ranges = self.range_instances();
        if ranges.is_empty() {
            return Err("no training range instance is configured (kind: \"range\")".into());
        }
        let by_server = q.get("server").and_then(|n| {
            self.db.instances().by_dcs_server_name(n).map(|c| c.id.clone())
        });
        let requested = by_server
            .or_else(|| q.get("instance").cloned())
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty());
        match requested.as_deref() {
            None => Ok(Scope::One(ranges[0].clone())),
            Some("all") => Ok(Scope::All(ranges)),
            Some(id) => match ranges.iter().find(|c| c.id == id) {
                Some(c) => Ok(Scope::One(c.clone())),
                None => Err(format!("instance {id:?} is not a training range")),
            },
        }
    }

    // ── engine RPCs ────────────────────────────────────────────────────

    /// Call one of the range engine's RPCs and parse its JSON reply.
    pub(crate) async fn rpc_json(
        &self,
        inst: &InstanceCfg,
        proc_name: &str,
        args: Vec<(&str, netidx::publisher::Value)>,
        timeout: Duration,
    ) -> std::result::Result<serde_json::Value, String> {
        let st = self.db.state(&InstanceId::from(inst.id.as_str()));
        match tokio::time::timeout(
            timeout,
            crate::call_engine_rpc_str(&self.db, &st, proc_name, args),
        )
        .await
        {
            Err(_) => Err(format!(
                "range engine did not answer {proc_name} within {}s",
                timeout.as_secs()
            )),
            Ok(Err(e)) => Err(format!("{:#}", e.0)),
            Ok(Ok(s)) => serde_json::from_str(&s)
                .map_err(|e| format!("{proc_name} returned invalid JSON: {e}")),
        }
    }

    /// An argument-less RPC served from a per-instance cache: at most one call
    /// per `ttl` no matter how many requests arrive (the rest wait on the same
    /// slot and share the answer). When a refresh fails, the last good reply
    /// is served for up to `stale_ok` before the failure is reported.
    pub(crate) async fn rpc_cached(
        &self,
        inst: &InstanceCfg,
        proc_name: &'static str,
        ttl: Duration,
        timeout: Duration,
        stale_ok: Duration,
    ) -> std::result::Result<serde_json::Value, String> {
        let slot = {
            let mut m = self.rpc.lock().unwrap();
            m.entry((inst.id.clone(), proc_name)).or_default().clone()
        };
        let mut g = slot.lock().await;
        let fresh = g.fetched.map_or(false, |t| t.elapsed() < ttl);
        if !fresh {
            match self.rpc_json(inst, proc_name, vec![], timeout).await {
                Ok(v) => {
                    g.ok = Some((Instant::now(), v));
                    g.err = None;
                }
                Err(e) => g.err = Some(e),
            }
            g.fetched = Some(Instant::now());
        }
        match (&g.err, &g.ok) {
            (None, Some((_, v))) => Ok(v.clone()),
            (Some(_), Some((at, v))) if at.elapsed() < stale_ok => Ok(v.clone()),
            (Some(e), _) => Err(e.clone()),
            (None, None) => Err("no reply from the range engine yet".into()),
        }
    }

    // ── identity ───────────────────────────────────────────────────────

    /// The ucid linked to a dashboard session's Discord account, via
    /// DCSServerBot, cached for a couple of minutes.
    pub(crate) async fn session_ucid(&self, sid: Uuid, discord_id: &str) -> Option<String> {
        if let Some((at, u)) = self.ucids.lock().unwrap().get(&sid) {
            let ttl = if u.is_some() { UCID_TTL } else { UNLINKED_TTL };
            if at.elapsed() < ttl {
                return u.clone();
            }
        }
        let u = crate::resolve_ucid_via_bot(&self.bot, discord_id).await.map(|u| u.to_string());
        let mut m = self.ucids.lock().unwrap();
        m.retain(|_, (at, _)| at.elapsed() < UCID_TTL);
        m.insert(sid, (Instant::now(), u.clone()));
        u
    }

    /// Rate limit for the spawn/despawn routes: one call per `per` per ucid.
    /// Returns the seconds left to wait when refused.
    pub(crate) fn rate_limit(&self, ucid: &str, per: Duration) -> std::result::Result<(), f64> {
        let mut m = self.spawn_rl.lock().unwrap();
        let now = Instant::now();
        if let Some(t) = m.get(ucid) {
            let e = now.duration_since(*t);
            if e < per {
                return Err((per - e).as_secs_f64());
            }
        }
        m.retain(|_, t| now.duration_since(*t) < Duration::from_secs(60));
        m.insert(ucid.to_string(), now);
        Ok(())
    }

    // ── caches ─────────────────────────────────────────────────────────

    pub(crate) fn png_get(&self, id: &str) -> Option<Arc<Vec<u8>>> {
        self.pngs.lock().unwrap().get(id)
    }

    pub(crate) fn png_put(&self, id: &str, png: Arc<Vec<u8>>) {
        self.pngs.lock().unwrap().put(id.to_string(), png)
    }

    pub(crate) fn agg_get(&self, key: &str, ttl: Duration) -> Option<serde_json::Value> {
        let m = self.agg.lock().unwrap();
        m.get(key).filter(|(at, _)| at.elapsed() < ttl).map(|(_, v)| v.clone())
    }

    pub(crate) fn agg_put(&self, key: String, v: serde_json::Value) {
        let mut m = self.agg.lock().unwrap();
        // Unbounded query strings must not grow this forever.
        if m.len() > 512 {
            m.retain(|_, (at, _)| at.elapsed() < Duration::from_secs(300));
            if m.len() > 512 {
                m.clear();
            }
        }
        m.insert(key, (Instant::now(), v));
    }

    /// Relative URL of a result's rendered card.
    pub(crate) fn card_url(id: &str, ext: &str) -> String {
        format!("/api/range/result/{}/card.{ext}", urlencoding::encode(id))
    }
}

/// Start the per-range-instance background work: the `range.jsonl` tailer
/// and the daily track-retention sweep. A no-op when no instance is a range.
pub(crate) fn spawn_tasks(ctx: &Arc<RangeCtx>, track_days: u32) {
    let ranges = ctx.range_instances();
    if ranges.is_empty() {
        return;
    }
    for cfg in &ranges {
        match cfg.range_jsonl_path() {
            Some(path) => {
                info!("[{}] training range: ingesting results from {}", cfg.id, path.display());
                tokio::spawn(ingest::ingest_loop(ctx.clone(), cfg.id.clone(), path));
            }
            None => warn!(
                "[{}] is a training range but has neither `range_jsonl` nor `stats_jsonl` -- \
                 its results will not be ingested",
                cfg.id
            ),
        }
    }
    let c = ctx.clone();
    tokio::spawn(async move {
        let mut tick = tokio::time::interval(Duration::from_secs(24 * 3600));
        loop {
            tick.tick().await;
            let cutoff = chrono::Utc::now() - chrono::Duration::days(track_days as i64);
            for cfg in c.range_instances() {
                let r = tokio::task::block_in_place(|| {
                    c.store.prune_tracks(&cfg.id, cutoff.timestamp_millis().max(0) as u64)
                });
                match r {
                    Ok(0) => (),
                    Ok(n) => info!(
                        "[{}] range: pruned {n} debrief track(s) older than {track_days} days",
                        cfg.id
                    ),
                    Err(e) => warn!("[{}] range: track pruning failed: {e:?}", cfg.id),
                }
            }
        }
    });
    // The system font scan takes a noticeable moment on Windows; do it now
    // rather than on the first card request.
    tokio::task::spawn_blocking(|| {
        let _ = cards::fontdb();
    });
}

/// Parse an RFC3339 timestamp from a query parameter.
pub(crate) fn parse_ts(s: &str) -> Result<chrono::DateTime<chrono::Utc>> {
    chrono::DateTime::parse_from_rfc3339(s)
        .map(|d| d.with_timezone(&chrono::Utc))
        .map_err(|e| anyhow!("bad timestamp {s:?}: {e}"))
}
