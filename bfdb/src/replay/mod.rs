// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Flight replay: every sortie, replayable in the browser from the servers'
//! own Tacview recordings, so nobody needs Tacview installed.
//!
//! Each instance's `tacview_dir` is scanned once a minute. A recording that
//! has stopped growing is read once ([`ingest`]) into compact per-window track
//! files under `--replay-dir`, and its flights are indexed by pilot name. The
//! dashboard then streams just the windows it is showing ([`api`]).
//!
//! Index trees (raw sled, JSON values -- not yats, so the schema can grow;
//! see the note in range/store.rs):
//!
//! | tree               | key                                     | value          |
//! |--------------------|-----------------------------------------|----------------|
//! | `replay_files`     | `<instance>\0<file name>`               | scan state     |
//! | `replay_recs`      | `<instance>\0<start_ms BE><rec id>`     | summary        |
//! | `replay_rec_by_id` | `<rec id>`                              | `replay_recs` key |
//! | `replay_flights`   | `<lowercase pilot>\0<start_ms BE><rec id>\0<idx BE>` | flight |
//!
//! None of this derives from stats, so a stats rebuild or campaign reset
//! leaves it alone; recordings age out after `--replay-days`.

pub(crate) mod acmi;
pub(crate) mod api;
pub(crate) mod ingest;

use crate::{db::StatsDb, instance::InstanceCfg};
use anyhow::{anyhow, Context, Result};
use log::{info, warn};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use sled::Tree;
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

/// A recording must have been left alone this long before it is read: a
/// file Tacview is still writing is never touched.
const SETTLE: Duration = Duration::from_secs(120);
/// Give up on a recording that failed this many times.
const MAX_TRIES: u32 = 3;

pub(crate) struct ReplayCtx {
    pub(crate) db: StatsDb,
    pub(crate) dir: PathBuf,
    pub(crate) days: u32,
    files: Tree,
    recs: Tree,
    rec_by_id: Tree,
    flights: Tree,
}

/// Scan state of one recording file.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct FileState {
    size: u64,
    mtime_ms: u64,
    #[serde(default)]
    rec: Option<String>,
    #[serde(default)]
    error: Option<String>,
    #[serde(default)]
    tries: u32,
}

/// One processed recording, as listed.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct RecSummary {
    pub(crate) id: String,
    pub(crate) instance: String,
    pub(crate) file: String,
    pub(crate) title: Option<String>,
    pub(crate) start_ms: i64,
    pub(crate) duration_ms: i64,
    pub(crate) objects: usize,
    pub(crate) chunks: u32,
    pub(crate) flights: usize,
    /// Distinct pilot names with a flight in it.
    pub(crate) pilots: usize,
}

/// One indexed flight.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct FlightRow {
    pub(crate) rec: String,
    pub(crate) instance: String,
    pub(crate) i: u32,
    pub(crate) pilot: String,
    /// The ucid when exactly one known pilot has this name.
    pub(crate) ucid: Option<String>,
    pub(crate) aircraft: String,
    pub(crate) kind: String,
    pub(crate) color: Option<String>,
    pub(crate) group: Option<String>,
    pub(crate) t0: i64,
    pub(crate) t1: i64,
    pub(crate) start_ms: i64,
    pub(crate) end_ms: i64,
    pub(crate) shots: u32,
    pub(crate) kills: u32,
    pub(crate) fate: String,
}

fn ms(t: SystemTime) -> u64 {
    t.duration_since(UNIX_EPOCH).map(|d| d.as_millis() as u64).unwrap_or(0)
}

fn key2(a: &str, b: &[u8]) -> Vec<u8> {
    let mut k = Vec::with_capacity(a.len() + 1 + b.len());
    k.extend_from_slice(a.as_bytes());
    k.push(0);
    k.extend_from_slice(b);
    k
}

fn safe_id(s: &str) -> String {
    s.chars().map(|c| if c.is_ascii_alphanumeric() || c == '-' || c == '_' { c } else { '_' }).collect()
}

impl ReplayCtx {
    pub(crate) fn new(db: StatsDb, dir: PathBuf, days: u32) -> Result<Arc<Self>> {
        let s = db.sled();
        Ok(Arc::new(Self {
            files: s.open_tree("replay_files")?,
            recs: s.open_tree("replay_recs")?,
            rec_by_id: s.open_tree("replay_rec_by_id")?,
            flights: s.open_tree("replay_flights")?,
            db,
            dir,
            days,
        }))
    }

    /// The directory holding a processed recording's files, if it exists.
    pub(crate) fn rec_dir(&self, id: &str) -> Option<PathBuf> {
        if id.is_empty() || safe_id(id) != id {
            return None;
        }
        let p = self.dir.join(id);
        p.is_dir().then_some(p)
    }

    pub(crate) fn summary(&self, id: &str) -> Result<Option<RecSummary>> {
        let Some(k) = self.rec_by_id.get(id.as_bytes())? else { return Ok(None) };
        let Some(v) = self.recs.get(k)? else { return Ok(None) };
        Ok(Some(serde_json::from_slice(&v)?))
    }

    /// Recordings of one instance, newest first, started before `before_ms`.
    pub(crate) fn recordings(&self, instance: &str, before_ms: Option<i64>, limit: usize) -> Result<Vec<RecSummary>> {
        let lo = key2(instance, &[]);
        let hi = key2(instance, &before_ms.map(|b| (b.max(0) as u64).to_be_bytes()).unwrap_or([0xff; 8]));
        let mut out = vec![];
        for r in self.recs.range(lo..hi).rev() {
            let (_, v) = r?;
            out.push(serde_json::from_slice(&v)?);
            if out.len() >= limit {
                break;
            }
        }
        Ok(out)
    }

    /// Every flight flown under any of `names`, newest first.
    pub(crate) fn flights_for(&self, names: &[String], limit: usize) -> Result<Vec<FlightRow>> {
        let mut out: Vec<FlightRow> = vec![];
        let mut seen = std::collections::HashSet::new();
        for n in names {
            let n = n.trim().to_lowercase();
            if n.is_empty() || !seen.insert(n.clone()) {
                continue;
            }
            for r in self.flights.scan_prefix(key2(&n, &[])).rev() {
                let (_, v) = r?;
                out.push(serde_json::from_slice(&v)?);
                if out.len() >= limit * 2 {
                    break;
                }
            }
        }
        out.sort_by(|a, b| b.start_ms.cmp(&a.start_ms));
        out.truncate(limit);
        Ok(out)
    }

    /// How many recordings each instance has, and what the scanner thinks of
    /// its files.
    pub(crate) fn status(&self, cfg: &InstanceCfg) -> Result<Value> {
        let recs = self.recs.scan_prefix(key2(&cfg.id, &[])).count();
        let mut pending = 0usize;
        let mut failed = vec![];
        for r in self.files.scan_prefix(key2(&cfg.id, &[])) {
            let (k, v) = r?;
            let st: FileState = serde_json::from_slice(&v).unwrap_or_default();
            if st.rec.is_some() {
                continue;
            }
            match &st.error {
                Some(e) if st.tries >= MAX_TRIES => {
                    let name = String::from_utf8_lossy(&k[cfg.id.len() + 1..]).to_string();
                    failed.push(json!({ "file": name, "error": e }));
                }
                _ => pending += 1,
            }
        }
        Ok(json!({
            "instance": cfg.id,
            "tacview": cfg.tacview_dir.is_some(),
            "recordings": recs,
            "pending": pending,
            "failed": failed,
        }))
    }

    /// Look at an instance's Tacview folder and process every recording
    /// that is new, settled and recent enough. Blocking; one file at a time.
    fn scan(&self, cfg: &InstanceCfg) -> Result<()> {
        let Some(dir) = &cfg.tacview_dir else { return Ok(()) };
        let now = SystemTime::now();
        let oldest = now - Duration::from_secs(self.days as u64 * 86_400);
        let mut todo = vec![];
        for e in fs::read_dir(dir).with_context(|| format!("reading {}", dir.display()))?.flatten() {
            let p = e.path();
            let Some(name) = p.file_name().and_then(|n| n.to_str()).map(str::to_string) else { continue };
            if !name.to_ascii_lowercase().ends_with(".acmi") {
                continue;
            }
            let Ok(md) = e.metadata() else { continue };
            let Ok(mtime) = md.modified() else { continue };
            if !md.is_file() || md.len() == 0 || mtime < oldest {
                continue;
            }
            if now.duration_since(mtime).unwrap_or_default() < SETTLE {
                continue;
            }
            let key = key2(&cfg.id, name.as_bytes());
            let st: Option<FileState> = self.files.get(&key)?.and_then(|v| serde_json::from_slice(&v).ok());
            let fresh = FileState { size: md.len(), mtime_ms: ms(mtime), ..Default::default() };
            match st {
                Some(st) if st.size == fresh.size && st.mtime_ms == fresh.mtime_ms => {
                    if st.rec.is_some() || st.tries >= MAX_TRIES {
                        continue;
                    }
                    todo.push((mtime, p, name, key, st));
                }
                // New, or changed since we last looked: start over.
                _ => todo.push((mtime, p, name, key, fresh)),
            }
        }
        todo.sort_by_key(|t| t.0);
        for (_, path, name, key, mut st) in todo {
            let t = std::time::Instant::now();
            match self.process(cfg, &path, &name) {
                Ok(sum) => {
                    info!(
                        "[{}] replay: {} -> {} ({} objects, {} flights, {:.1}s)",
                        cfg.id,
                        name,
                        sum.id,
                        sum.objects,
                        sum.flights,
                        t.elapsed().as_secs_f32()
                    );
                    st.rec = Some(sum.id);
                    st.error = None;
                }
                Err(e) => {
                    st.tries += 1;
                    warn!("[{}] replay: {} failed (try {}/{}): {e:#}", cfg.id, name, st.tries, MAX_TRIES);
                    st.error = Some(format!("{e:#}"));
                }
            }
            self.files.insert(key, serde_json::to_vec(&st)?)?;
        }
        Ok(())
    }

    fn process(&self, cfg: &InstanceCfg, path: &Path, name: &str) -> Result<RecSummary> {
        fs::create_dir_all(&self.dir)?;
        let stage = ingest::staging_dir(&self.dir, &format!("{}\0{}", cfg.id, name));
        let _ = fs::remove_dir_all(&stage);
        let built = match ingest::build(path, &stage) {
            Ok(b) => b,
            Err(e) => {
                let _ = fs::remove_dir_all(&stage);
                return Err(e);
            }
        };
        let stamp = chrono::DateTime::from_timestamp_millis(built.start_ms)
            .map(|d| d.format("%Y%m%d-%H%M%S").to_string())
            .unwrap_or_else(|| "unknown".into());
        let id = safe_id(&format!("{}-{}-{:08x}", cfg.id, stamp, fxhash::hash32(name)));
        let dest = self.dir.join(&id);
        // A re-processed file replaces its old output and index rows.
        if self.rec_by_id.contains_key(id.as_bytes())? {
            self.forget(&id)?;
        }
        let _ = fs::remove_dir_all(&dest);
        fs::rename(&stage, &dest).with_context(|| format!("moving into {}", dest.display()))?;

        let mut pilots = std::collections::HashSet::new();
        for f in &built.flights {
            pilots.insert(f.pilot.to_lowercase());
            let ucids = self.db.ucids_by_name(&f.pilot);
            let row = FlightRow {
                rec: id.clone(),
                instance: cfg.id.clone(),
                i: f.i,
                pilot: f.pilot.clone(),
                ucid: (ucids.len() == 1).then(|| ucids[0].to_string()),
                aircraft: f.aircraft.clone(),
                kind: f.kind.to_string(),
                color: f.color.clone(),
                group: f.group.clone(),
                t0: f.t0,
                t1: f.t1,
                start_ms: f.start_ms,
                end_ms: f.end_ms,
                shots: f.shots,
                kills: f.kills,
                fate: f.fate.to_string(),
            };
            self.flights.insert(flight_key(&row), serde_json::to_vec(&row)?)?;
        }
        let sum = RecSummary {
            id: id.clone(),
            instance: cfg.id.clone(),
            file: name.to_string(),
            title: built.title,
            start_ms: built.start_ms,
            duration_ms: built.duration_ms,
            objects: built.objects,
            chunks: built.chunks,
            flights: built.flights.len(),
            pilots: pilots.len(),
        };
        let rk = rec_key(&cfg.id, built.start_ms, &id);
        self.recs.insert(&rk, serde_json::to_vec(&sum)?)?;
        self.rec_by_id.insert(id.as_bytes(), rk)?;
        Ok(sum)
    }

    /// Drop a recording's index rows and files.
    fn forget(&self, id: &str) -> Result<()> {
        if let Some(k) = self.rec_by_id.remove(id.as_bytes())? {
            self.recs.remove(k)?;
        }
        let mut dead = vec![];
        for r in self.flights.iter() {
            let (k, v) = r?;
            if serde_json::from_slice::<FlightRow>(&v).map(|f| f.rec == id).unwrap_or(false) {
                dead.push(k);
            }
        }
        for k in dead {
            self.flights.remove(k)?;
        }
        let _ = fs::remove_dir_all(self.dir.join(id));
        Ok(())
    }

    /// Delete every recording that started more than `days` ago.
    fn prune(&self) -> Result<usize> {
        let cutoff = chrono::Utc::now().timestamp_millis() - self.days as i64 * 86_400_000;
        let mut old = vec![];
        for r in self.recs.iter() {
            let (_, v) = r?;
            let s: RecSummary = serde_json::from_slice(&v)?;
            if s.start_ms < cutoff {
                old.push(s.id);
            }
        }
        for id in &old {
            self.forget(id)?;
        }
        Ok(old.len())
    }
}

fn rec_key(instance: &str, start_ms: i64, id: &str) -> Vec<u8> {
    let mut k = key2(instance, &(start_ms.max(0) as u64).to_be_bytes());
    k.extend_from_slice(id.as_bytes());
    k
}

fn flight_key(f: &FlightRow) -> Vec<u8> {
    let mut k = key2(&f.pilot.trim().to_lowercase(), &(f.start_ms.max(0) as u64).to_be_bytes());
    k.extend_from_slice(f.rec.as_bytes());
    k.push(0);
    k.extend_from_slice(&f.i.to_be_bytes());
    k
}

/// Start the scanner (every minute, every instance with a `tacview_dir`)
/// and the daily retention sweep.
pub(crate) fn spawn_tasks(ctx: &Arc<ReplayCtx>) {
    let with_dir: Vec<Arc<InstanceCfg>> =
        ctx.db.instances().all().iter().filter(|c| c.tacview_dir.is_some()).cloned().collect();
    if with_dir.is_empty() {
        info!("replay: no instance has a tacview_dir; flight replay is off");
        return;
    }
    // Two servers writing into one folder would each claim every recording.
    let mut seen: std::collections::HashMap<String, &str> = std::collections::HashMap::new();
    for c in &with_dir {
        let Some(d) = &c.tacview_dir else { continue };
        let key = fs::canonicalize(d).unwrap_or_else(|_| d.clone()).to_string_lossy().to_lowercase();
        if let Some(other) = seen.insert(key, &c.id) {
            warn!(
                "replay: instances {other} and {} share the Tacview folder {} -- every recording \
                 there will be listed under both. Give each server its own tacviewExportPath.",
                c.id,
                d.display()
            );
        }
    }
    for c in &with_dir {
        info!(
            "[{}] replay: reading Tacview recordings from {} into {}",
            c.id,
            c.tacview_dir.as_ref().map(|p| p.display().to_string()).unwrap_or_default(),
            ctx.dir.display()
        );
    }
    let c = ctx.clone();
    tokio::spawn(async move {
        // Let startup (stats replay etc.) settle first.
        tokio::time::sleep(Duration::from_secs(30)).await;
        let mut tick = tokio::time::interval(Duration::from_secs(60));
        // Not `now - 24h`: an Instant cannot go back past boot, and on a
        // machine up for less than a day that subtraction panics.
        let mut last_prune: Option<std::time::Instant> = None;
        loop {
            tick.tick().await;
            let c2 = c.clone();
            let cfgs = with_dir.clone();
            let prune = last_prune.map(|p| p.elapsed() >= Duration::from_secs(86_400)).unwrap_or(true);
            if prune {
                last_prune = Some(std::time::Instant::now());
            }
            let r = tokio::task::spawn_blocking(move || -> Result<()> {
                if prune {
                    match c2.prune() {
                        Ok(0) => (),
                        Ok(n) => info!("replay: pruned {n} recording(s) older than {} days", c2.days),
                        Err(e) => warn!("replay: pruning failed: {e:#}"),
                    }
                }
                for cfg in &cfgs {
                    if let Err(e) = c2.scan(cfg) {
                        warn!("[{}] replay: scan failed: {e:#}", cfg.id);
                    }
                }
                Ok(())
            })
            .await
            .map_err(|e| anyhow!("{e}"));
            if let Err(e) = r {
                warn!("replay: scanner task died: {e:#}");
            }
        }
    });
}
