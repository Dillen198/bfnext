//! Keeping the stats database alive: graceful shutdown, periodic backups and
//! restoring one, and the housekeeping sweeps that stop the auth and trail
//! trees growing forever.
//!
//! ## Shutdown interface (for whatever supervises bfdb)
//!
//! bfdb flushes the database (sled or redb, see `crate::storage`) and exits
//! 0 on any of:
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
//! In every case: the listener stops being served, the database is flushed
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

/// Flush everything the database has acknowledged to disk. Called on the way
/// out.
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
    /// Files and folders kept beside the database that a backup copies too
    /// (`--backup-files`): (name inside the backup's `files/`, path).
    pub(crate) files: Vec<(String, PathBuf)>,
}

/// Where backups of `db_path` live unless `--backup-dir` says otherwise.
pub(crate) fn default_backup_dir(db_path: &Path) -> PathBuf {
    let mut s = db_path.as_os_str().to_os_string();
    s.push(".backups");
    PathBuf::from(s)
}

/// The file a redb backup holds the database in, inside `<dir>/<timestamp>/`.
pub(crate) const REDB_BACKUP_FILE: &str = "store.redb";

/// Which engine a backup in `dir` was taken from: a redb backup holds
/// `REDB_BACKUP_FILE`, a sled one is a sled directory.
pub(crate) fn backup_engine(backup: &Path) -> crate::storage::Backend {
    if backup.join(REDB_BACKUP_FILE).is_file() {
        crate::storage::Backend::Redb
    } else {
        crate::storage::Backend::Sled
    }
}

/// Snapshot the live database into `<dir>/<UTC timestamp>`: for sled, as a
/// complete sled database of its own; for redb, as `<timestamp>/store.redb`.
///
/// **redb** has real snapshots: every tree is copied from one read
/// transaction, so the backup is the whole database at one instant, while
/// writes go on.
///
/// **sled** keeps writing while this runs, so its files cannot simply be copied --
/// a copy taken mid-write is exactly the torn page that already cost this
/// server a cold `--rebuild-stats`. `export`/`import` walks every tree through
/// sled itself and writes a fresh, consistent DB, which can be opened (or
/// restored) as-is.
///
/// sled has no snapshot, so the trees are exported one after another while
/// writes go on: each tree is consistent, the set is not one instant. The
/// ingest cursors go first, so a restored database re-reads the last few
/// seconds of stats.jsonl rather than skipping them (kills, sorties and
/// deploys are idempotent on re-read; a capture or supply counter in that
/// window can count twice).
///
/// `files` (see [`BackupCfg::files`]) are copied into `<backup>/files/`. A
/// file unchanged since the previous backup (same size and modification
/// time) is hard-linked to that backup's copy instead -- recordings and
/// photos never change, so keeping seven backups of them costs one copy --
/// falling back to a copy where links are not possible (another volume, a
/// network share).
pub(crate) fn backup_now(db: &yats::Db, dir: &Path, keep: usize, files: &[(String, PathBuf)]) -> Result<PathBuf> {
    std::fs::create_dir_all(dir).with_context(|| format!("creating {}", dir.display()))?;
    let previous = list_backups(dir).pop();
    db.flush().context("flushing before backup")?;
    let stamp = chrono::Utc::now().format("%Y%m%dT%H%M%SZ").to_string();
    let target = dir.join(&stamp);
    let partial = dir.join(format!("{stamp}.partial"));
    let _ = std::fs::remove_dir_all(&partial);
    if db.engine() == yats::Engine::Redb {
        // Live credentials stay out of it (see BACKUP_EXCLUDED_TREES).
        db.snapshot_redb(&partial.join(REDB_BACKUP_FILE), crate::db::BACKUP_EXCLUDED_TREES)
            .context("writing the redb snapshot")?;
    } else {
        let db = db.as_sled().expect("a sled database");
        let out = sled::open(&partial).with_context(|| format!("creating {}", partial.display()))?;
        // sled::import panics on IO trouble rather than returning it; keep a
        // failed backup from taking the whole process down with it.
        // Live credentials stay out of it (see BACKUP_EXCLUDED_TREES).
        let mut export: Vec<_> = db
            .export()
            .into_iter()
            .filter(|(_, name, _)| {
                !crate::db::BACKUP_EXCLUDED_TREES.iter().any(|t| t.as_bytes() == &name[..])
            })
            .collect();
        // cursors first (stable: everything else keeps sled's order)
        export.sort_by_key(|(_, name, _)| !name.windows(6).any(|w| w == b"cursor"));
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| out.import(export)))
            .map_err(|_| anyhow::anyhow!("sled import panicked while writing the backup"))?;
        out.flush().context("flushing the backup")?;
    }
    for (name, src) in files {
        if !src.exists() {
            continue;
        }
        let dst = partial.join("files").join(name);
        let prev = previous.as_ref().map(|p| p.join("files").join(name));
        copy_linked(src, &dst, prev.as_deref())
            .with_context(|| format!("backing up {} into {}", src.display(), dst.display()))?;
    }
    // On Windows a file in the backup can still be open for a moment after
    // sled lets go of it (its own threads winding down, or a virus scanner
    // looking at the new files), and renaming the folder then fails with
    // "Access is denied". Give it a few seconds before calling it a failure.
    let mut tries = 0;
    loop {
        match std::fs::rename(&partial, &target) {
            Ok(()) => break,
            Err(e) if tries < 40 && e.kind() == std::io::ErrorKind::PermissionDenied => {
                tries += 1;
                std::thread::sleep(Duration::from_millis(100));
            }
            Err(e) => {
                return Err(e).with_context(|| {
                    format!("renaming {} -> {}", partial.display(), target.display())
                })
            }
        }
    }
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

/// Take a backup every `cfg.every`. The first is due `cfg.every` after the
/// newest backup on disk -- not after this start: a server restarted more
/// often than the interval (daily deploys) used to never take one at all
/// (vs1 had none when its database was damaged, Oct 8). Never sooner than
/// FIRST_BACKUP_DELAY after start, so a restart loop isn't a backup loop.
const FIRST_BACKUP_DELAY: Duration = Duration::from_secs(15 * 60);

fn first_backup_in(cfg: &BackupCfg) -> Duration {
    let age = list_backups(&cfg.dir)
        .last()
        .and_then(|p| std::fs::metadata(p).and_then(|m| m.modified()).ok())
        .and_then(|t| t.elapsed().ok());
    match age {
        Some(age) => cfg.every.saturating_sub(age).max(FIRST_BACKUP_DELAY),
        None => FIRST_BACKUP_DELAY,
    }
}

pub(crate) fn spawn_backups(db: StatsDb, cfg: BackupCfg) {
    tokio::spawn(async move {
        log::info!(
            "database backups: every {}h into {} (keeping {})",
            cfg.every.as_secs() / 3600,
            cfg.dir.display(),
            cfg.keep
        );
        let first = first_backup_in(&cfg);
        log::info!("database backups: the next one in {} min", first.as_secs() / 60);
        let mut tick = tokio::time::interval_at(tokio::time::Instant::now() + first, cfg.every);
        tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        loop {
            tick.tick().await;
            let sled = db.sled().clone();
            let c = cfg.clone();
            let started = std::time::Instant::now();
            match task::spawn_blocking(move || backup_now(&sled, &c.dir, c.keep, &c.files)).await {
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
/// put the newest backup in its place. Runs before the DB is opened, and only
/// considers backups of the engine bfdb is about to run on (`--storage`): a
/// sled backup taken before a cutover is not a redb database.
pub(crate) fn restore_latest(db_path: &Path, dir: &Path) -> Result<PathBuf> {
    restore_latest_on(db_path, dir, crate::storage::selected())
}

pub(crate) fn restore_latest_on(db_path: &Path, dir: &Path, engine: crate::storage::Backend) -> Result<PathBuf> {
    let all = list_backups(dir);
    let latest = all.iter().rev().find(|b| backup_engine(b) == engine).cloned().ok_or_else(|| {
        let other = all.iter().filter(|b| backup_engine(b) != engine).count();
        if other > 0 {
            anyhow::anyhow!(
                "no {engine} backups in {} ({other} from the other engine: start with the --storage                  they were taken on to restore one)",
                dir.display()
            )
        } else {
            anyhow::anyhow!("no backups found in {}", dir.display())
        }
    })?;
    if engine == crate::storage::Backend::Redb {
        let live = crate::storage::location(db_path, engine);
        if live.exists() {
            let mut aside = live.as_os_str().to_os_string();
            aside.push(format!(".damaged-{}", chrono::Utc::now().format("%Y%m%dT%H%M%SZ")));
            let aside = PathBuf::from(aside);
            std::fs::rename(&live, &aside)
                .with_context(|| format!("moving {} aside to {}", live.display(), aside.display()))?;
            log::warn!("moved the current database aside to {}", aside.display());
        }
        let from = latest.join(REDB_BACKUP_FILE);
        std::fs::copy(&from, &live).with_context(|| format!("copying {} -> {}", from.display(), live.display()))?;
        log::warn!("restored {} from backup {}", live.display(), latest.display());
        return Ok(latest);
    }
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

/// Copy `src` (a file or a folder) to `dst`, hard-linking each file that is
/// unchanged against its copy under `prev`.
fn copy_linked(src: &Path, dst: &Path, prev: Option<&Path>) -> Result<()> {
    let md = std::fs::metadata(src)?;
    if md.is_dir() {
        std::fs::create_dir_all(dst)?;
        for e in std::fs::read_dir(src)? {
            let e = e?;
            let name = e.file_name();
            copy_linked(&e.path(), &dst.join(&name), prev.map(|p| p.join(&name)).as_deref())?;
        }
        return Ok(());
    }
    if let Some(parent) = dst.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let unchanged = prev.and_then(|p| std::fs::metadata(p).ok().map(|m| (p, m))).filter(|(_, pm)| {
        pm.is_file() && pm.len() == md.len() && pm.modified().ok() == md.modified().ok()
    });
    if let Some((p, _)) = unchanged {
        if std::fs::hard_link(p, dst).is_ok() {
            return Ok(());
        }
    }
    std::fs::copy(src, dst)?;
    // keep the source's time, so the next backup can tell it is unchanged
    if let Ok(t) = md.modified() {
        if let Ok(f) = std::fs::OpenOptions::new().write(true).open(dst) {
            let _ = f.set_modified(t);
        }
    }
    Ok(())
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
                d.audit_prune()?;
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
    fn backup_copies_files_and_links_unchanged_ones() {
        let base = std::env::temp_dir().join(format!("bfdb-backup-files-{}", uuid::Uuid::new_v4()));
        let dir = base.join("backups");
        let photos = base.join("db.intel");
        std::fs::create_dir_all(photos.join("sub")).unwrap();
        std::fs::write(photos.join("a.png"), b"aaaa").unwrap();
        std::fs::write(photos.join("sub").join("b.png"), b"bb").unwrap();
        let cfg = base.join("ODF_CFG");
        std::fs::write(&cfg, b"{}").unwrap();
        let files = vec![("intel".to_string(), photos.clone()), ("engine-cfg/default".to_string(), cfg)];
        let db = yats::Db::open_sled(base.join("db")).unwrap();
        let first = backup_now(&db, &dir, 5, &files).unwrap();
        assert_eq!(std::fs::read(first.join("files/intel/sub/b.png")).unwrap(), b"bb");
        assert_eq!(std::fs::read(first.join("files/engine-cfg/default")).unwrap(), b"{}");
        // a second backup in a later second
        std::thread::sleep(Duration::from_millis(1100));
        std::fs::write(photos.join("c.png"), b"c").unwrap();
        let second = backup_now(&db, &dir, 5, &files).unwrap();
        assert_ne!(first, second);
        assert_eq!(std::fs::read(second.join("files/intel/a.png")).unwrap(), b"aaaa");
        assert_eq!(std::fs::read(second.join("files/intel/c.png")).unwrap(), b"c");
        // a missing source is skipped, not an error
        std::thread::sleep(Duration::from_millis(1100));
        backup_now(&db, &dir, 5, &[("gone".into(), base.join("nope"))]).unwrap();
        drop(db);
        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn backup_roundtrip_and_rotation() {
        let base = std::env::temp_dir().join(format!("bfdb-backup-test-{}", uuid::Uuid::new_v4()));
        let db_path = base.join("db");
        let dir = base.join("backups");
        {
            let db = yats::Db::open_sled(&db_path).unwrap();
            db.open_tree("t").unwrap().insert(b"k", b"v").unwrap();
            db.open_tree("auth_sessions_h").unwrap().insert(b"k", b"secret").unwrap();
            let b = backup_now(&db, &dir, 2, &[]).unwrap();
            assert_eq!(backup_engine(&b), crate::storage::Backend::Sled);
            let restored = sled::open(&b).unwrap();
            assert_eq!(restored.open_tree("t").unwrap().get(b"k").unwrap().as_deref(), Some(&b"v"[..]));
            // live logins are not copied into backups
            assert!(!restored.tree_names().iter().any(|n| &n[..] == b"auth_sessions_h"));
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

    #[test]
    fn redb_backup_is_a_snapshot_and_restores() {
        use crate::storage::{location, Backend};
        let base = std::env::temp_dir().join(format!("bfdb-backup-redb-{}", uuid::Uuid::new_v4()));
        let db_path = base.join("db");
        let dir = base.join("backups");
        let photos = base.join("db.intel");
        std::fs::create_dir_all(&photos).unwrap();
        std::fs::write(photos.join("a.png"), b"aaaa").unwrap();
        let b = {
            let db = yats::Db::open_redb(location(&db_path, Backend::Redb)).unwrap();
            db.open_tree("t").unwrap().insert(b"k", b"v").unwrap();
            db.open_tree("auth_sessions_h").unwrap().insert(b"k", b"secret").unwrap();
            db.ensure_next_id(500).unwrap();
            let b = backup_now(&db, &dir, 2, &[("intel".into(), photos.clone())]).unwrap();
            // written after the backup: must not be in it
            db.open_tree("t").unwrap().insert(b"later", b"x").unwrap();
            b
        };
        assert_eq!(backup_engine(&b), Backend::Redb);
        assert_eq!(std::fs::read(b.join("files/intel/a.png")).unwrap(), b"aaaa");
        {
            let snap = yats::Db::open_redb(b.join(REDB_BACKUP_FILE)).unwrap();
            assert_eq!(snap.open_tree("t").unwrap().get(b"k").unwrap().as_deref(), Some(&b"v"[..]));
            assert!(snap.open_tree("t").unwrap().get(b"later").unwrap().is_none());
            assert!(!snap.tree_names().unwrap().iter().any(|n| n == b"auth_sessions_h"));
            assert!(snap.generate_id().unwrap() >= 500);
        }
        // a sled restore ignores redb backups; a redb restore takes it and
        // keeps the damaged file aside
        assert!(restore_latest_on(&db_path, &dir, Backend::Sled).is_err());
        assert_eq!(restore_latest_on(&db_path, &dir, Backend::Redb).unwrap(), b);
        let restored = yats::Db::open_redb(location(&db_path, Backend::Redb)).unwrap();
        assert!(restored.open_tree("t").unwrap().get(b"later").unwrap().is_none());
        drop(restored);
        assert!(std::fs::read_dir(&base)
            .unwrap()
            .any(|e| e.unwrap().file_name().to_string_lossy().starts_with("db.redb.damaged-")));
        let _ = std::fs::remove_dir_all(&base);
    }
}
