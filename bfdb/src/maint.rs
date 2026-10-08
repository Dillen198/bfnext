//! Keeping the stats database alive: graceful shutdown, periodic backups and
//! restoring one, and the housekeeping sweeps that stop the auth and trail
//! trees growing forever.
//!
//! ## Shutdown interface (for whatever supervises bfdb)
//!
//! bfdb flushes sled and exits 0 on any of:
//!
//! * **Ctrl+C / SIGINT**, and on Unix **SIGTERM**.
//! * **Windows console control events**: `CTRL_BREAK_EVENT`, `CTRL_C_EVENT`,
//!   `CTRL_CLOSE_EVENT`, `CTRL_SHUTDOWN_EVENT`. A supervisor should start bfdb
//!   with `CREATE_NEW_PROCESS_GROUP` and send
//!   `GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, <bfdb pid>)` -- that reaches
//!   only bfdb's own group.
//! * **`POST /api/admin/shutdown`**, accepted only from a direct loopback
//!   connection (no `X-Forwarded-For`, i.e. not through the proxy) and only
//!   with an admin session cookie or `Authorization: Bearer <token>` matching
//!   `--shutdown-token` / `$BFDB_SHUTDOWN_TOKEN`. Answers
//!   `202 {"ok":true,"message":...}` and exits ~0.5 s later.
//!
//! In every case: the listener stops being served, `sled::Db::flush()` runs
//! (so every write acknowledged so far is on disk), and the process exits.
//! Give it ~10 s before a hard kill; a flush of a busy DB can take a few
//! seconds. bfdb holds no other state that needs closing -- rounds and
//! sorties are the engine's, and stay open across a bfdb restart by design.

use crate::db::StatsDb;
use anyhow::{Context as _, Result};
use std::{
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::Duration,
};
use tokio::task;

// ── Graceful shutdown ────────────────────────────────────────────────────────

/// Raised once, by a signal or the shutdown endpoint; `main` waits on it.
#[derive(Clone, Default)]
pub(crate) struct Shutdown {
    notify: Arc<tokio::sync::Notify>,
    raised: Arc<AtomicBool>,
}

impl Shutdown {
    pub(crate) fn trigger(&self, why: &str) {
        if !self.raised.swap(true, Ordering::SeqCst) {
            log::warn!("shutdown requested ({why})");
        }
        self.notify.notify_waiters();
        // A waiter that has not started waiting yet still sees it.
        self.notify.notify_one();
    }

    pub(crate) async fn wait(&self) {
        if self.raised.load(Ordering::SeqCst) {
            return;
        }
        self.notify.notified().await
    }
}

/// Wire process signals to `sd`.
pub(crate) fn spawn_signal_handlers(sd: Shutdown) {
    {
        let sd = sd.clone();
        tokio::spawn(async move {
            if tokio::signal::ctrl_c().await.is_ok() {
                sd.trigger("ctrl-c");
            }
        });
    }
    #[cfg(windows)]
    {
        use tokio::signal::windows;
        macro_rules! on {
            ($ctor:expr, $name:literal) => {
                match $ctor {
                    Ok(mut s) => {
                        let sd = sd.clone();
                        tokio::spawn(async move {
                            if s.recv().await.is_some() {
                                sd.trigger($name);
                            }
                        });
                    }
                    Err(e) => log::warn!("cannot listen for {}: {e}", $name),
                }
            };
        }
        on!(windows::ctrl_break(), "ctrl-break");
        on!(windows::ctrl_close(), "console close");
        on!(windows::ctrl_shutdown(), "system shutdown");
    }
    #[cfg(unix)]
    {
        use tokio::signal::unix::{signal, SignalKind};
        if let Ok(mut s) = signal(SignalKind::terminate()) {
            tokio::spawn(async move {
                if s.recv().await.is_some() {
                    sd.trigger("SIGTERM");
                }
            });
        }
    }
}

/// Flush everything sled has acknowledged to disk. Called on the way out.
pub(crate) fn flush_db(db: &StatsDb) {
    let started = std::time::Instant::now();
    match db.sled().flush() {
        Ok(n) => log::info!("database flushed ({n} bytes) in {}ms", started.elapsed().as_millis()),
        Err(e) => log::error!("database flush on shutdown FAILED: {e}"),
    }
}

// ── Backups ──────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub(crate) struct BackupCfg {
    pub(crate) dir: PathBuf,
    pub(crate) every: Duration,
    pub(crate) keep: usize,
}

/// Where backups of `db_path` live unless `--backup-dir` says otherwise.
pub(crate) fn default_backup_dir(db_path: &Path) -> PathBuf {
    let mut s = db_path.as_os_str().to_os_string();
    s.push(".backups");
    PathBuf::from(s)
}

/// Snapshot the live database into `<dir>/<UTC timestamp>` as a complete sled
/// database of its own.
///
/// sled keeps writing while this runs, so its files cannot simply be copied --
/// a copy taken mid-write is exactly the torn page that already cost this
/// server a cold `--rebuild-stats`. `export`/`import` walks every tree through
/// sled itself and writes a fresh, consistent DB, which can be opened (or
/// restored) as-is.
pub(crate) fn backup_now(db: &sled::Db, dir: &Path, keep: usize) -> Result<PathBuf> {
    std::fs::create_dir_all(dir).with_context(|| format!("creating {}", dir.display()))?;
    db.flush().context("flushing before backup")?;
    let stamp = chrono::Utc::now().format("%Y%m%dT%H%M%SZ").to_string();
    let target = dir.join(&stamp);
    let partial = dir.join(format!("{stamp}.partial"));
    let _ = std::fs::remove_dir_all(&partial);
    {
        let out = sled::open(&partial).with_context(|| format!("creating {}", partial.display()))?;
        // sled::import panics on IO trouble rather than returning it; keep a
        // failed backup from taking the whole process down with it.
        let export = db.export();
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| out.import(export)))
            .map_err(|_| anyhow::anyhow!("sled import panicked while writing the backup"))?;
        out.flush().context("flushing the backup")?;
    }
    std::fs::rename(&partial, &target)
        .with_context(|| format!("renaming {} -> {}", partial.display(), target.display()))?;
    prune_backups(dir, keep);
    Ok(target)
}

/// Completed backups in `dir`, oldest first. Names are UTC timestamps, so
/// lexical order is time order.
pub(crate) fn list_backups(dir: &Path) -> Vec<PathBuf> {
    let mut v: Vec<PathBuf> = std::fs::read_dir(dir)
        .map(|rd| {
            rd.filter_map(|e| e.ok())
                .map(|e| e.path())
                .filter(|p| {
                    p.is_dir()
                        && p.file_name()
                            .and_then(|n| n.to_str())
                            .map_or(false, |n| n.ends_with('Z') && n.len() == 16)
                })
                .collect()
        })
        .unwrap_or_default();
    v.sort();
    v
}

fn prune_backups(dir: &Path, keep: usize) {
    let all = list_backups(dir);
    if all.len() <= keep.max(1) {
        return;
    }
    for old in &all[..all.len() - keep.max(1)] {
        match std::fs::remove_dir_all(old) {
            Ok(()) => log::info!("backup rotated out: {}", old.display()),
            Err(e) => log::warn!("could not remove old backup {}: {e}", old.display()),
        }
    }
}

/// Take a backup every `cfg.every`, the first one an interval after start (a
/// restart loop must not turn into a backup loop).
pub(crate) fn spawn_backups(db: StatsDb, cfg: BackupCfg) {
    tokio::spawn(async move {
        log::info!(
            "database backups: every {}h into {} (keeping {})",
            cfg.every.as_secs() / 3600,
            cfg.dir.display(),
            cfg.keep
        );
        let mut tick = tokio::time::interval_at(tokio::time::Instant::now() + cfg.every, cfg.every);
        tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        loop {
            tick.tick().await;
            let sled = db.sled().clone();
            let c = cfg.clone();
            let started = std::time::Instant::now();
            match task::spawn_blocking(move || backup_now(&sled, &c.dir, c.keep)).await {
                Ok(Ok(p)) => log::info!(
                    "database backup written to {} in {}s",
                    p.display(),
                    started.elapsed().as_secs()
                ),
                Ok(Err(e)) => log::error!("database backup FAILED: {e:#}"),
                Err(e) => log::error!("database backup task died: {e}"),
            }
        }
    });
}

/// `--restore-latest-backup`: move the current DB aside (never deleted) and
/// put the newest backup in its place. Runs before the DB is opened.
pub(crate) fn restore_latest(db_path: &Path, dir: &Path) -> Result<PathBuf> {
    let latest = list_backups(dir)
        .pop()
        .ok_or_else(|| anyhow::anyhow!("no backups found in {}", dir.display()))?;
    if db_path.exists() {
        let mut aside = db_path.as_os_str().to_os_string();
        aside.push(format!(".damaged-{}", chrono::Utc::now().format("%Y%m%dT%H%M%SZ")));
        let aside = PathBuf::from(aside);
        std::fs::rename(db_path, &aside)
            .with_context(|| format!("moving {} aside to {}", db_path.display(), aside.display()))?;
        log::warn!("moved the current database aside to {}", aside.display());
    }
    copy_dir(&latest, db_path)?;
    log::warn!("restored {} from backup {}", db_path.display(), latest.display());
    Ok(latest)
}

fn copy_dir(from: &Path, to: &Path) -> Result<()> {
    std::fs::create_dir_all(to).with_context(|| format!("creating {}", to.display()))?;
    for e in std::fs::read_dir(from).with_context(|| format!("reading {}", from.display()))? {
        let e = e?;
        let dst = to.join(e.file_name());
        if e.file_type()?.is_dir() {
            copy_dir(&e.path(), &dst)?;
        } else {
            std::fs::copy(e.path(), &dst)
                .with_context(|| format!("copying {} -> {}", e.path().display(), dst.display()))?;
        }
    }
    Ok(())
}

/// Explain a failed open instead of just propagating sled's error.
pub(crate) fn explain_open_failure(db_path: &Path, dir: &Path, e: &anyhow::Error) {
    let backups = list_backups(dir);
    log::error!("could not open the stats database at {}: {e:#}", db_path.display());
    eprintln!("could not open the stats database at {}: {e:#}", db_path.display());
    let hint = match backups.last() {
        Some(b) => format!(
            "The newest backup is {}. Restart bfdb with --restore-latest-backup to move the \
             damaged database aside (it is kept, not deleted) and restore that backup. \
             Anything recorded since the backup was taken is re-read from stats.jsonl.",
            b.display()
        ),
        None => format!(
            "No backups exist in {} to restore from. Try --rebuild-stats to re-ingest the \
             stats archive into a fresh database.",
            dir.display()
        ),
    };
    log::error!("{hint}");
    eprintln!("{hint}");
    // The bot also copies the database aside before every bfdb.exe swap
    // (`<home>/_backups/db-*`); with redeploys more often than the daily
    // backup interval, that may be the newest copy there is.
    if let Some(home) = db_path.parent() {
        let newest = std::fs::read_dir(home.join("_backups"))
            .map(|rd| {
                let mut v: Vec<PathBuf> = rd
                    .filter_map(|e| e.ok())
                    .map(|e| e.path())
                    .filter(|p| p.is_dir() && p.file_name().map_or(false, |n| n.to_string_lossy().starts_with("db-")))
                    .collect();
                v.sort();
                v.pop()
            })
            .ok()
            .flatten();
        if let Some(s) = newest {
            let more = format!(
                "There is also a copy taken before the last bfdb.exe swap: {}. With DCSServerBot, \
                 /feops db_restore restores the newest copy of either kind.",
                s.display()
            );
            log::error!("{more}");
            eprintln!("{more}");
        }
    }
}

// ── Housekeeping ─────────────────────────────────────────────────────────────

/// Trail points older than this are pruned; the API only ever serves the last
/// 30 minutes.
pub(crate) const TRAIL_KEEP_SECS: i64 = 2 * 3600;

/// Periodic sweeps: expired auth sessions and OAuth states (previously only
/// removed if the same id was ever looked up again, so an abandoned login left
/// a row forever), and trail points past `TRAIL_KEEP_SECS`.
pub(crate) fn spawn_sweeps(db: StatsDb) {
    tokio::spawn(async move {
        let mut tick = tokio::time::interval(Duration::from_secs(15 * 60));
        tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        loop {
            tick.tick().await;
            let d = db.clone();
            let res = task::spawn_blocking(move || -> Result<(usize, usize, usize)> {
                let (s, o) = d.sweep_auth()?;
                let cutoff = chrono::Utc::now().timestamp() - TRAIL_KEEP_SECS;
                let t = d.prune_trail_points(cutoff)?;
                Ok((s, o, t))
            })
            .await;
            match res {
                Ok(Ok((0, 0, 0))) => (),
                Ok(Ok((s, o, t))) => log::info!(
                    "housekeeping: removed {s} expired session(s), {o} stale login state(s), \
                     {t} old trail point(s)"
                ),
                Ok(Err(e)) => log::warn!("housekeeping sweep failed: {e:#}"),
                Err(e) => log::warn!("housekeeping sweep task died: {e}"),
            }
        }
    });
}

/// Free space on the volume holding `path`, if it can be determined. Cached
/// for a minute: it is read by the public health check.
pub(crate) fn disk_free(path: &Path) -> Option<u64> {
    static CACHE: std::sync::Mutex<Option<(std::time::Instant, Option<u64>)>> = std::sync::Mutex::new(None);
    let mut g = CACHE.lock().unwrap_or_else(|e| e.into_inner());
    if let Some((at, v)) = *g {
        if at.elapsed() < Duration::from_secs(60) {
            return v;
        }
    }
    let v = disk_free_uncached(path);
    *g = Some((std::time::Instant::now(), v));
    v
}

fn disk_free_uncached(path: &Path) -> Option<u64> {
    use sysinfo::Disks;
    let abs = std::fs::canonicalize(path).ok()?;
    let abs = abs.to_string_lossy().trim_start_matches(r"\\?\").to_ascii_lowercase();
    let disks = Disks::new_with_refreshed_list();
    disks
        .iter()
        .filter(|d| abs.starts_with(&d.mount_point().to_string_lossy().to_ascii_lowercase()))
        .max_by_key(|d| d.mount_point().as_os_str().len())
        .map(|d| d.available_space())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backup_roundtrip_and_rotation() {
        let base = std::env::temp_dir().join(format!("bfdb-backup-test-{}", uuid::Uuid::new_v4()));
        let db_path = base.join("db");
        let dir = base.join("backups");
        {
            let db = sled::open(&db_path).unwrap();
            db.open_tree("t").unwrap().insert(b"k", b"v").unwrap();
            let b = backup_now(&db, &dir, 2).unwrap();
            let restored = sled::open(&b).unwrap();
            assert_eq!(restored.open_tree("t").unwrap().get(b"k").unwrap().as_deref(), Some(&b"v"[..]));
        }
        // rotation keeps only the newest `keep`
        for n in 0..3 {
            let fake = dir.join(format!("2000010{n}T000000Z"));
            std::fs::create_dir_all(&fake).unwrap();
        }
        prune_backups(&dir, 2);
        assert_eq!(list_backups(&dir).len(), 2);
        let _ = std::fs::remove_dir_all(&base);
    }
}
