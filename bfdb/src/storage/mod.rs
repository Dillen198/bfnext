//! Which storage engine the stats database runs on, and moving it between
//! them.
//!
//! bfdb's data has lived in sled 0.34 since the start; sled is unmaintained
//! and its values are positional bincode, so a field added to a stored type
//! breaks every old row. redb is the replacement (`yats::backend`, and
//! `docs/plans/storage-migration.md` for why). Both run behind the same
//! `yats::Db`, so nothing above `StatsDb` knows which one it is on.
//!
//! **sled stays the default.** redb is used only when an operator says so
//! with `--storage redb` (or `$BFDB_STORAGE=redb`), after converting the
//! database with `--migrate-storage sled redb`. The runbook is
//! `docs/storage-cutover.md`.
//!
//! Where each engine keeps the database, for `--db <path>`:
//!
//! | engine | location |
//! | --- | --- |
//! | sled | the directory `<path>` (as always) |
//! | redb | the file `<path>.redb`, beside it |
//!
//! so `<path>.backups`, `<path>.intel` and everything else derived from
//! `--db` stay where they are, and a converted sled directory is left exactly
//! as it was: rolling back is starting on sled again.

use crate::db::StatsDb;
use anyhow::{bail, Context, Result};
use std::{
    fmt,
    path::{Path, PathBuf},
    sync::OnceLock,
    time::Duration,
};
use yats::{
    convert::{self, CopyStats, VerifyReport},
    Db, ReencodeStats,
};

#[cfg(test)]
mod tests;

/// A storage engine bfdb can run on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, clap::ValueEnum)]
pub(crate) enum Backend {
    /// sled 0.34, in the directory `--db`. The default.
    #[default]
    Sled,
    /// redb, in the file `<--db>.redb`.
    Redb,
}

impl fmt::Display for Backend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Backend::Sled => "sled",
            Backend::Redb => "redb",
        })
    }
}

impl std::str::FromStr for Backend {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "sled" => Ok(Backend::Sled),
            "redb" => Ok(Backend::Redb),
            other => bail!("unknown storage engine {other:?} (sled or redb)"),
        }
    }
}

/// The storage flags (`--storage`, `--migrate-storage ...`).
#[derive(clap::Args, Debug, Clone, Default)]
pub(crate) struct StorageArgs {
    /// Storage engine for the stats database: `sled` (the default) or
    /// `redb`. redb keeps the database in `<db>.redb` and needs it converted
    /// first (`--migrate-storage sled redb`); see docs/storage-cutover.md.
    /// Also $BFDB_STORAGE.
    #[arg(long = "storage", value_enum, value_name = "ENGINE")]
    pub(crate) storage: Option<Backend>,
    /// Convert the database from one engine to the other, verify the copy,
    /// and exit; bfdb must be stopped. Reads a copy of the source, so the
    /// source is never modified. `sled redb` writes `<db>.redb`; `redb sled`
    /// (rolling back) writes `<db>.from-redb`, which then replaces `<db>` by
    /// hand. Run again on a finished target to just re-verify it.
    #[arg(long = "migrate-storage", num_args = 2, value_names = ["FROM", "TO"], value_enum)]
    pub(crate) migrate_storage: Option<Vec<Backend>>,
    /// Where `--migrate-storage` writes, instead of the default above.
    #[arg(long = "migrate-into", value_name = "PATH")]
    pub(crate) migrate_into: Option<PathBuf>,
    /// With `--migrate-storage`: do the whole conversion and verification
    /// into a scratch copy, report, and delete it. Nothing is left behind.
    #[arg(long = "migrate-dry-run")]
    pub(crate) migrate_dry_run: bool,
    /// With `--migrate-storage`: copy rows byte for byte and leave them in
    /// the source's value encoding, rather than rewriting each into the
    /// target's (MessagePack on redb, bincode on sled). Every row is still
    /// decoded once as a check.
    #[arg(long = "migrate-keep-encoding")]
    pub(crate) migrate_keep_encoding: bool,
}

static SELECTED: OnceLock<Backend> = OnceLock::new();

/// Record the engine for this process, from the flag or `$BFDB_STORAGE`.
/// Called once from `main` before anything opens the database.
pub(crate) fn select(args: &StorageArgs) -> Result<Backend> {
    let b = match args.storage {
        Some(b) => b,
        None => match std::env::var("BFDB_STORAGE").ok().filter(|v| !v.trim().is_empty()) {
            Some(v) => v.parse().context("$BFDB_STORAGE")?,
            None => Backend::Sled,
        },
    };
    let _ = SELECTED.set(b);
    Ok(b)
}

/// The engine this process runs on: sled unless `select` said otherwise.
pub(crate) fn selected() -> Backend {
    SELECTED.get().copied().unwrap_or_default()
}

/// Where `backend` keeps the database for `--db db_path`.
pub(crate) fn location(db_path: &Path, backend: Backend) -> PathBuf {
    match backend {
        Backend::Sled => db_path.to_path_buf(),
        Backend::Redb => with_suffix(db_path, ".redb"),
    }
}

fn with_suffix(p: &Path, suffix: &str) -> PathBuf {
    let mut s = p.as_os_str().to_os_string();
    s.push(suffix);
    PathBuf::from(s)
}

fn dir_has_files(p: &Path) -> bool {
    std::fs::read_dir(p).map(|mut rd| rd.next().is_some()).unwrap_or(false)
}

fn open_engine(path: &Path, backend: Backend) -> Result<Db> {
    match backend {
        Backend::Sled => Db::open_sled(path),
        Backend::Redb => Db::open_redb(path),
    }
}

/// Open the database for `--db db_path` on `backend`, refusing the mistakes
/// that would quietly lose data: redb next to a sled database that was never
/// converted (bfdb would start empty).
pub(crate) fn open(db_path: &Path, backend: Backend) -> Result<Db> {
    let redb = location(db_path, Backend::Redb);
    match backend {
        Backend::Sled => {
            if redb.exists() {
                log::warn!(
                    "storage: running on sled, but {} exists -- this database was converted to \
                     redb. Writes made on redb since are not in the sled database; to keep \
                     them, stop bfdb and convert back (docs/storage-cutover.md, rolling back).",
                    redb.display()
                );
            }
            log::info!("storage: sled, {}", db_path.display());
            Db::open_sled(db_path)
        }
        Backend::Redb => {
            if !redb.exists() && dir_has_files(db_path) {
                bail!(
                    "--storage redb, but {} does not exist while the sled database {} does. \
                     Convert it first: stop bfdb and run `bfdb --db {} --migrate-storage sled redb` \
                     (docs/storage-cutover.md). bfdb will not start an empty redb database beside \
                     a sled one.",
                    redb.display(),
                    db_path.display(),
                    db_path.display()
                );
            }
            log::info!("storage: redb, {}", redb.display());
            Db::open_redb(&redb)
        }
    }
}

/// Fail if a running bfdb (or anything else) has the database at `src` open.
/// sled locks `<dir>/db`, redb its file; an exclusive lock attempt from here
/// fails while either is held. Nothing is written.
pub(crate) fn ensure_not_in_use(src: &Path, backend: Backend) -> Result<()> {
    let lock_file = match backend {
        Backend::Sled => src.join("db"),
        Backend::Redb => src.to_path_buf(),
    };
    let f = match std::fs::File::open(&lock_file) {
        Ok(f) => f,
        // no lock file: nothing can hold it (an empty or foreign directory
        // fails later, when it is opened)
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) => {
            return Err(e).with_context(|| {
                format!("{} cannot be read -- is bfdb still running?", lock_file.display())
            })
        }
    };
    match f.try_lock() {
        Ok(()) => {
            let _ = f.unlock();
            Ok(())
        }
        Err(std::fs::TryLockError::WouldBlock) => bail!(
            "the database at {} is open in another process -- stop bfdb first",
            src.display()
        ),
        Err(std::fs::TryLockError::Error(e)) => {
            Err(e).with_context(|| format!("checking whether {} is in use", lock_file.display()))
        }
    }
}

fn copy_path(src: &Path, dst: &Path) -> Result<u64> {
    let md = std::fs::metadata(src).with_context(|| format!("reading {}", src.display()))?;
    if md.is_file() {
        if let Some(parent) = dst.parent().filter(|p| !p.as_os_str().is_empty()) {
            std::fs::create_dir_all(parent)?;
        }
        return std::fs::copy(src, dst).with_context(|| format!("copying {} -> {}", src.display(), dst.display()));
    }
    std::fs::create_dir_all(dst).with_context(|| format!("creating {}", dst.display()))?;
    let mut n = 0;
    for e in std::fs::read_dir(src)? {
        let e = e?;
        n += copy_path(&e.path(), &dst.join(e.file_name()))?;
    }
    Ok(n)
}

fn size_of(p: &Path) -> u64 {
    match std::fs::metadata(p) {
        Ok(m) if m.is_file() => m.len(),
        Ok(_) => std::fs::read_dir(p)
            .map(|rd| rd.filter_map(|e| e.ok()).map(|e| size_of(&e.path())).sum())
            .unwrap_or(0),
        Err(_) => 0,
    }
}

fn remove_path(p: &Path) -> Result<()> {
    let r = if p.is_dir() { std::fs::remove_dir_all(p) } else { std::fs::remove_file(p) };
    match r {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e).with_context(|| format!("removing {}", p.display())),
    }
}

/// Rename, retrying for a few seconds: on Windows a file can stay open for
/// a moment after its database is dropped (its flush thread winding down, a
/// virus scanner).
fn rename_patiently(from: &Path, to: &Path) -> Result<()> {
    let mut tries = 0;
    loop {
        match std::fs::rename(from, to) {
            Ok(()) => return Ok(()),
            Err(e) if tries < 50 && e.kind() == std::io::ErrorKind::PermissionDenied => {
                tries += 1;
                std::thread::sleep(Duration::from_millis(100));
            }
            Err(e) => return Err(e).with_context(|| format!("renaming {} -> {}", from.display(), to.display())),
        }
    }
}

fn remove_patiently(p: &Path) -> Result<()> {
    let mut tries = 0;
    loop {
        match remove_path(p) {
            Ok(()) => return Ok(()),
            Err(_) if tries < 50 => {
                tries += 1;
                std::thread::sleep(Duration::from_millis(100));
            }
            Err(e) => return Err(e),
        }
    }
}

/// Compact a redb file, retrying for a few seconds while the handle just
/// dropped lets go of it (its flush thread can hold it a moment longer).
fn compact_patiently(path: &Path) -> Result<(u64, u64)> {
    let mut tries = 0;
    loop {
        match yats::backend::compact_redb_file(path) {
            Ok(r) => return Ok(r),
            Err(_) if tries < 50 => {
                tries += 1;
                std::thread::sleep(Duration::from_millis(100));
            }
            Err(e) => return Err(e),
        }
    }
}

/// A `--migrate-storage` run.
#[derive(Debug, Clone)]
pub(crate) struct MigrateOpts {
    pub(crate) db_path: PathBuf,
    pub(crate) from: Backend,
    pub(crate) to: Backend,
    pub(crate) into: Option<PathBuf>,
    pub(crate) dry_run: bool,
    pub(crate) keep_encoding: bool,
}

/// What a `--migrate-storage` run did.
#[derive(Debug, Clone)]
pub(crate) struct MigrateReport {
    pub(crate) source: PathBuf,
    pub(crate) target: PathBuf,
    /// The target was already there: it was only verified.
    pub(crate) verified_existing: bool,
    pub(crate) copied: CopyStats,
    /// The byte-for-byte check right after copying (skipped for an existing
    /// target).
    pub(crate) exact: Option<VerifyReport>,
    /// The typed pass: per tree, rows re-encoded (or, keeping the encoding,
    /// just decoded).
    pub(crate) typed: Vec<(String, ReencodeStats)>,
    /// The final check, allowing re-encoded values.
    pub(crate) last: VerifyReport,
    pub(crate) source_bytes: u64,
    pub(crate) target_bytes: u64,
}

/// Every `SAMPLE_EVERY`-th pair is also fetched from the target by key.
const SAMPLE_EVERY: u64 = 97;

/// The default target of a conversion to `to`.
pub(crate) fn default_target(db_path: &Path, to: Backend) -> PathBuf {
    match to {
        Backend::Redb => location(db_path, Backend::Redb),
        // never the live sled directory itself: that is the database being
        // rolled back to, and it is not touched
        Backend::Sled => with_suffix(db_path, ".from-redb"),
    }
}

/// Convert the database. See `StorageArgs::migrate_storage`.
///
/// 1. Refuse if the source is in use, or the target is the source.
/// 2. Copy the source's files to `<target>.staging-source` and work from
///    that copy, so the source is only ever read as files.
/// 3. If the target exists, verify it against the source and stop
///    (idempotent: a finished conversion re-verifies, it is not redone).
/// 4. Otherwise convert into `<target>.partial` (`<target>.dry-run`):
///    copy every tree, verify byte for byte, run the typed pass (rewrite
///    every row in the target's encoding, or only decode it), verify again,
///    and only then rename it into place (or, dry run, delete it). A failed
///    or interrupted run leaves no target; running it again starts over.
pub(crate) fn migrate(o: &MigrateOpts, mut say: impl FnMut(&str)) -> Result<MigrateReport> {
    if o.from == o.to {
        bail!("--migrate-storage {} {}: nothing to convert", o.from, o.to)
    }
    let src = location(&o.db_path, o.from);
    if !src.exists() {
        bail!("no {} database at {}", o.from, src.display())
    }
    let target = o.into.clone().unwrap_or_else(|| default_target(&o.db_path, o.to));
    if target == src || (o.to == Backend::Sled && target == o.db_path) {
        bail!("refusing to convert into {} (that is a live database location)", target.display())
    }
    ensure_not_in_use(&src, o.from)?;
    let source_bytes = size_of(&src);
    let work_dir = target.parent().filter(|p| !p.as_os_str().is_empty()).unwrap_or(Path::new("."));
    std::fs::create_dir_all(work_dir).with_context(|| format!("creating {}", work_dir.display()))?;
    // the staging copy plus a target of about the same size
    if let Some(free) = crate::maint::disk_free(work_dir) {
        let need = source_bytes.saturating_mul(2) + (64 << 20);
        if free < need {
            bail!(
                "{} has {} MB free; converting a {} MB database needs about {} MB (a staging \
                 copy of the source plus the new database)",
                work_dir.display(),
                free >> 20,
                source_bytes >> 20,
                need >> 20
            )
        }
    }

    let staging = with_suffix(&target, ".staging-source");
    remove_patiently(&staging)?;
    say(&format!("copying {} ({} MB) to {}", src.display(), source_bytes >> 20, staging.display()));
    copy_path(&src, &staging)?;
    let result = migrate_from_staging(o, &src, &staging, &target, source_bytes, &mut say);
    if let Err(e) = remove_patiently(&staging) {
        say(&format!("could not remove the staging copy {}: {e:#}", staging.display()));
    }
    result
}

fn migrate_from_staging(
    o: &MigrateOpts,
    src: &Path,
    staging: &Path,
    target: &Path,
    source_bytes: u64,
    say: &mut impl FnMut(&str),
) -> Result<MigrateReport> {
    let source = open_engine(staging, o.from)?;
    if target.exists() && !o.dry_run {
        say(&format!("{} already exists: verifying it against {}", target.display(), src.display()));
        let dst = open_engine(target, o.to)?;
        let last = convert::verify(&source, &dst, true, SAMPLE_EVERY)?;
        let typed = StatsDb::reencode_store(&dst, true)?;
        let target_bytes = size_of(target);
        return Ok(MigrateReport {
            source: src.to_path_buf(),
            target: target.to_path_buf(),
            verified_existing: true,
            copied: CopyStats::default(),
            exact: None,
            typed,
            last,
            source_bytes,
            target_bytes,
        });
    }
    let work = with_suffix(target, if o.dry_run { ".dry-run" } else { ".partial" });
    remove_patiently(&work)?;
    let result = (|| -> Result<MigrateReport> {
        let dst = open_engine(&work, o.to)?;
        say(&format!("converting {} -> {} into {}", o.from, o.to, work.display()));
        let copied = convert::copy_all(&source, &dst, |tree, n| {
            if n > 0 {
                say(&format!("  {tree}: {n}"))
            }
        })?;
        let exact = convert::verify(&source, &dst, false, SAMPLE_EVERY)?;
        if !exact.ok() {
            bail!("the copy does not match its source: {:?}", exact.mismatches)
        }
        say(&format!("copied and verified {} pairs in {} trees", copied.pairs, copied.trees.len()));
        let rewrite = !o.keep_encoding;
        say(if rewrite { "re-encoding rows with bfdb's types" } else { "decoding every row with bfdb's types" });
        let typed = StatsDb::reencode_store(&dst, !rewrite)?;
        let last = convert::verify(&source, &dst, true, SAMPLE_EVERY)?;
        if !last.ok() {
            bail!("after re-encoding, the copy does not match its source: {:?}", last.mismatches)
        }
        dst.flush()?;
        drop(dst);
        if o.to == Backend::Redb {
            // the copy and then the rewrite of every row leave about a
            // database's worth of free pages in the file
            let (before, after) = compact_patiently(&work)?;
            say(&format!("compacted {}: {} MB -> {} MB", work.display(), before >> 20, after >> 20));
        }
        Ok(MigrateReport {
            source: src.to_path_buf(),
            target: target.to_path_buf(),
            verified_existing: false,
            copied,
            exact: Some(exact),
            typed,
            last,
            source_bytes,
            target_bytes: size_of(&work),
        })
    })();
    drop(source);
    match result {
        Ok(rep) if o.dry_run => {
            remove_patiently(&work)?;
            Ok(rep)
        }
        Ok(rep) => {
            rename_patiently(&work, target)?;
            Ok(rep)
        }
        Err(e) => {
            let _ = remove_patiently(&work);
            Err(e)
        }
    }
}

/// `--migrate-storage FROM TO`: run it and print the report. Returns the
/// process exit code.
pub(crate) fn migrate_cli(args: &StorageArgs, db_path: &Path) -> i32 {
    let Some([from, to]) = args.migrate_storage.as_deref().map(|v| [v[0], v[1]]) else { return 2 };
    let o = MigrateOpts {
        db_path: db_path.to_path_buf(),
        from,
        to,
        into: args.migrate_into.clone(),
        dry_run: args.migrate_dry_run,
        keep_encoding: args.migrate_keep_encoding,
    };
    match migrate(&o, |s| println!("{s}")) {
        Ok(rep) => {
            print!("{}", render(&o, &rep));
            if rep.last.ok() {
                0
            } else {
                1
            }
        }
        Err(e) => {
            eprintln!("--migrate-storage failed: {e:#}");
            eprintln!("Nothing was changed in {}.", location(db_path, from).display());
            1
        }
    }
}

/// The report, as printed.
pub(crate) fn render(o: &MigrateOpts, rep: &MigrateReport) -> String {
    use std::fmt::Write as _;
    let mut s = String::new();
    let _ = writeln!(s);
    let _ = writeln!(s, "{:<28} {:>10} {:>10} {:>10} {:>11}", "tree", "rows", "rewritten", "undecoded", "unverified");
    let counts: std::collections::HashMap<&str, u64> =
        rep.copied.trees.iter().map(|(n, c)| (n.as_str(), *c)).collect();
    let mut total = ReencodeStats::default();
    for (name, st) in &rep.typed {
        total += *st;
        let _ = writeln!(
            s,
            "{:<28} {:>10} {:>10} {:>10} {:>11}",
            name,
            counts.get(name.as_str()).copied().unwrap_or(st.rows),
            st.reencoded,
            st.undecodable,
            st.unverified
        );
    }
    let raw: u64 = rep
        .copied
        .trees
        .iter()
        .filter(|(n, _)| !rep.typed.iter().any(|(t, _)| t == n))
        .map(|(_, c)| c)
        .sum();
    let _ = writeln!(s);
    if !rep.verified_existing {
        let _ = writeln!(
            s,
            "copied: {} pairs in {} trees ({} of them in raw-JSON or legacy trees, copied as bytes)",
            rep.copied.pairs,
            rep.copied.trees.len(),
            raw
        );
    }
    if let Some(exact) = &rep.exact {
        let _ = writeln!(
            s,
            "byte-for-byte check after copying: {} pairs, {} point lookups, {} mismatches",
            exact.pairs, exact.lookups, exact.mismatch_count
        );
    }
    let _ = writeln!(
        s,
        "typed pass: {} rows, {} {}, {} already {}, {} undecodable, {} unverified (both left as they were)",
        total.rows,
        total.reencoded,
        if o.keep_encoding || rep.verified_existing { "could be rewritten" } else { "rewritten" },
        total.current,
        if o.to == Backend::Redb { "MessagePack" } else { "bincode" },
        total.undecodable,
        total.unverified
    );
    let _ = writeln!(
        s,
        "verify: {} trees, {} pairs, {} point lookups, {} re-encoded, {} mismatches",
        rep.last.trees, rep.last.pairs, rep.last.lookups, rep.last.reencoded, rep.last.mismatch_count
    );
    for m in &rep.last.mismatches {
        let _ = writeln!(s, "  mismatch: {m:?}");
    }
    let _ = writeln!(s, "size: source {} MB, target {} MB", rep.source_bytes >> 20, rep.target_bytes >> 20);
    let _ = writeln!(s);
    if !rep.last.ok() {
        let _ = writeln!(s, "FAILED: the target does not match the source. Do not start bfdb on it.");
    } else if o.dry_run {
        let _ = writeln!(s, "dry run OK: the conversion works. Nothing was kept; run without --migrate-dry-run to do it.");
    } else if rep.verified_existing {
        let _ = writeln!(s, "OK: {} matches {}.", rep.target.display(), rep.source.display());
    } else {
        let _ = writeln!(s, "OK: wrote {}. {} was not modified.", rep.target.display(), rep.source.display());
        match o.to {
            Backend::Redb => {
                let _ = writeln!(s, "Start bfdb with --storage redb (or $BFDB_STORAGE=redb) to run on it.");
            }
            Backend::Sled => {
                let _ = writeln!(
                    s,
                    "To run on it: move {} aside and rename {} to {}, then start bfdb without --storage redb.",
                    o.db_path.display(),
                    rep.target.display(),
                    o.db_path.display()
                );
            }
        }
    }
    if total.undecodable > 0 {
        let _ = writeln!(
            s,
            "Undecodable rows were copied as they are; they were already unreadable on {} \
             (rows from older builds, e.g. pre-session_v2 sessions).",
            o.from
        );
    }
    s
}
