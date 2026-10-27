/*
Copyright 2024 Eric Stokes.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
for more details.
*/

mod logpub;
mod perf;
mod rpcs;
mod statspub;

use crate::{admin::AdminCommand, db::persisted::Persisted};
use anyhow::{Context, Result, anyhow, bail};
use bfprotocols::{
    cfg::Cfg,
    perf::{Perf, PerfStat},
    stats::Stat,
};
use bytes::{BufMut, Bytes, BytesMut};
use chrono::prelude::*;
use compact_str::format_compact;
use crossbeam::queue::SegQueue;
use dcso3::perf::{Perf as ApiPerf, PerfStat as ApiPerfStat};
use fowlbg::jsonl::JsonlSink;
use fxhash::FxHashMap;
use log::{debug, error, info};
use logpub::LogPublisher;
use netidx::{
    chars::Chars,
    config::Config,
    path::Path as NetIdxPath,
    publisher::{Publisher, Value},
};
use parking_lot::{Condvar, Mutex};
use perf::PubPerf;
use rpcs::Rpcs;
use serde::Serialize;
use statspub::Statspub;
use std::{
    cell::RefCell,
    fs, io,
    path::{Path, PathBuf},
    sync::Arc,
};
use tokio::{
    fs::File,
    io::AsyncWriteExt,
    sync::{mpsc::UnboundedSender, oneshot},
    task,
};

// The thread, the logger, the fallback log and the supervisor are fowlbg's
// (shared with bfrange); bflib turns all of its options on (see `init`).
pub(crate) use fowlbg::{panic_msg, panic_payload_msg};

/// Last-resort log sink that doesn't depend on the background thread:
/// stderr plus `Logs/bfnext-fallback.txt`, appended from whatever thread is
/// logging. Every log line, every stat and every save goes through that one
/// thread, so when it dies this is the only record of why.
pub(super) use fowlbg::fallback_log;

/// Seconds since the background loop last finished a task, `None` before the
/// first one. The main thread pings it every minute (`Task::Ping`), so a value
/// that stops moving means the loop is wedged even though its channel is still
/// open.
pub(super) fn heartbeat_age(now: DateTime<Utc>) -> Option<i64> {
    fowlbg::heartbeat_age(now.timestamp())
}

/// The most log lines waiting for the background thread. Logs share its
/// queue with the saves, so a stalled log sink (a hung netidx publisher, a
/// slow disk) used to grow that queue without limit. Past this, lines are
/// dropped and counted; the next line that gets through says how many.
const MAX_QUEUED_LOG_LINES: usize = 20_000;

fn encode<T: Serialize>(db: &T) -> Result<BytesMut> {
    thread_local! {
        static BUF: RefCell<BytesMut> = RefCell::new(BytesMut::new());
    }
    BUF.with(|buf| {
        let mut buf = buf.borrow_mut();
        serde_json::to_writer((&mut *buf).writer(), db)?;
        Ok(buf.split())
    })
}

fn save_name(path: &Path) -> Result<&str> {
    path.file_name()
        .and_then(|n| n.to_str())
        .ok_or_else(|| anyhow!("save file with no name"))
}

/// The temp file a save is written to before it replaces the live one.
pub(crate) fn save_tmp_path(path: &Path) -> PathBuf {
    let mut tmp = PathBuf::from(path);
    tmp.set_extension("tmp");
    tmp
}

/// Written by an admin campaign reset (`Task::ResetState`) next to the save
/// file it removes, and removed again by the first save of the new round.
///
/// Startup treats a missing save file as "recover from the newest backup", not
/// "start a new round" -- see `backup_saves`. This marker is what tells a
/// deliberate reset apart from a save file that went missing by accident.
pub(crate) fn reset_marker_path(path: &Path) -> PathBuf {
    let mut p = PathBuf::from(path);
    let mut name = path.file_name().unwrap_or_default().to_os_string();
    name.push(".reset");
    p.set_file_name(name);
    p
}

/// Pilots who are past rookie mode (`db::player::ROOKIE_SORTIES`), kept next
/// to the save. The save is deleted at the end of a round and the sortie
/// counts with it; this file is not, so a veteran stays one in the next round.
pub(crate) fn veterans_path(path: &Path) -> PathBuf {
    let mut p = PathBuf::from(path);
    let mut name = path.file_name().unwrap_or_default().to_os_string();
    name.push(".veterans");
    p.set_file_name(name);
    p
}

/// Every file a missing live save could be recovered from: the timestamped
/// backups `save` leaves behind and the temp file of an interrupted save.
/// Empty if the campaign was deliberately reset (see `reset_marker_path`).
pub(crate) fn backup_saves(path: &Path) -> Vec<PathBuf> {
    if reset_marker_path(path).exists() {
        return vec![];
    }
    let mut found = vec![];
    let tmp = save_tmp_path(path);
    if tmp.is_file() {
        found.push(tmp);
    }
    let (Ok(name), Some(dir)) = (save_name(path), path.parent()) else {
        return found;
    };
    let Ok(rd) = fs::read_dir(dir) else { return found };
    for file in rd.flatten() {
        let fname = file.file_name();
        let Some(fname) = fname.to_str() else { continue };
        if let Some(ts) = fname.strip_prefix(name) {
            if !ts.is_empty() && ts.parse::<i64>().is_ok() && file.path().is_file() {
                found.push(file.path());
            }
        }
    }
    found
}

/// Keep the save that is about to be replaced as a timestamped backup.
///
/// This used to *rename* the live save out of the way first and move the new
/// one in afterwards, so there was a window -- the directory scan and the
/// deletes of the old rotation ran inside it -- with no live save file at
/// all. A hard kill there, or the final rename failing (an antivirus holding
/// the file is enough), and the next start found no save and began a brand
/// new round. Now the backup is a hard link (a copy where links aren't
/// possible) and the live file is only ever replaced in one atomic rename.
fn backup_state(path: &Path, now: DateTime<Utc>) -> Result<()> {
    if !path.exists() {
        return Ok(());
    }
    let mut backup = PathBuf::from(path);
    backup.set_file_name(
        format_compact!("{}{}", save_name(path)?, now.timestamp()).as_str(),
    );
    if backup.exists() {
        // two saves in the same second, one backup of them is plenty
        return Ok(());
    }
    if fs::hard_link(path, &backup).is_err() {
        fs::copy(path, &backup)?;
    }
    Ok(())
}

/// Thin out the timestamped backups: one per minute for the last ten minutes,
/// one per ten minutes for the last hour, one per hour for the last day, and so
/// on out to one per month.
fn prune_backups(path: &Path, now: DateTime<Utc>) -> Result<()> {
    {
        let name = save_name(path)?;
        let dir = path.parent().ok_or_else(|| anyhow!("path has no parent dir"))?;
        let mut by_age: FxHashMap<i64, Vec<(i64, PathBuf)>> = FxHashMap::default();
        for file in fs::read_dir(dir)? {
            let file = file?;
            let fname = file.file_name();
            let fname = match fname.to_str() {
                Some(s) => s,
                None => continue,
            };
            let now = now.timestamp();
            let onemin = 60;
            let tenmin = 600;
            let hour = 3600;
            let day = 86400;
            let week = day * 7;
            let month = week * 4;
            if file.file_type()?.is_file() {
                if let Some(ts) = fname.strip_prefix(name) {
                    if let Ok(ts) = ts.parse::<i64>() {
                        let age = now - ts;
                        let file = PathBuf::from(file.path());
                        if age > month {
                            by_age
                                .entry((age / month) * month)
                                .or_default()
                                .push((ts, file));
                        } else if age > week {
                            by_age
                                .entry((age / week) * week)
                                .or_default()
                                .push((ts, file));
                        } else if age > day {
                            by_age.entry((age / day) * day).or_default().push((ts, file));
                        } else if age > hour {
                            by_age
                                .entry((age / hour) * hour)
                                .or_default()
                                .push((ts, file));
                        } else if age > tenmin {
                            by_age
                                .entry((age / tenmin) * tenmin)
                                .or_default()
                                .push((ts, file));
                        } else if age > onemin {
                            by_age
                                .entry((age / onemin) * onemin)
                                .or_default()
                                .push((ts, file));
                        }
                    }
                }
            }
        }
        for (_, mut paths) in by_age {
            paths.sort_by_key(|(ts, _)| *ts);
            paths.reverse();
            while paths.len() > 1 {
                if let Some(path) = paths.pop() {
                    fs::remove_file(path.1)?;
                }
            }
        }
    }
    Ok(())
}

async fn save(path: PathBuf, encoded: Bytes) -> Result<()> {
    task::spawn_blocking(move || {
        use std::fs::File;
        let tmp = save_tmp_path(&path);
        let file = File::options().write(true).truncate(true).create(true).open(&tmp)?;
        // Level 3 (zstd's default): a large campaign compressed at 9 took
        // seconds of CPU per save for a few percent of size.
        let mut file = zstd::stream::Encoder::new(file, 3)?;
        io::copy(&mut &*encoded, &mut file)?;
        // finish and sync by hand. auto_finish throws away the error from
        // the final flush, which silently truncates the zstd frame, and
        // without the sync the rename below can reach the disk before the
        // data does, which after a hard kill leaves a save file of exactly
        // the right size full of zeros and a mission that won't start.
        let file = file.finish()?;
        file.sync_all()?;
        drop(file);
        let now = Utc::now();
        if let Err(e) = backup_state(&path, now) {
            error!("failed to back up the previous save file {e:?}")
        }
        // Replaces the live save in one step: rename(2) on unix and a
        // replace-existing rename on Windows (std documents both). If this
        // fails the old live save is still there, untouched, and the next
        // save tries again.
        fs::rename(&tmp, &path)?;
        // the first save of a new round retires the reset marker
        let marker = reset_marker_path(&path);
        if marker.exists() {
            if let Err(e) = fs::remove_file(&marker) {
                error!("failed to remove the campaign reset marker {marker:?} {e:?}")
            }
        }
        if let Err(e) = prune_backups(&path, now) {
            error!("failed to prune backup save files {e:?}")
        }
        Ok(())
    })
    .await?
}

/// Writes saves off the background loop, newest first.
///
/// One thread used to encode and write every save in line with the logs,
/// the stats and the netidx RPCs, so a large save held all of them up --
/// and saves queued behind a slow one were each written in turn, though
/// only the newest matters. Now `Task::SaveState` just parks the snapshot
/// here and a worker task writes it; a newer snapshot that arrives before
/// the worker gets to the last one replaces it.
#[derive(Default)]
pub(super) struct Saver {
    /// The newest snapshot not yet being written.
    pending: Mutex<Option<(PathBuf, Persisted)>>,
    wake: tokio::sync::Notify,
    /// Held while a snapshot is encoded and written. `flush` and `cancel`
    /// take it to wait for that.
    writing: tokio::sync::Mutex<()>,
}

impl Saver {
    fn submit(&self, path: PathBuf, db: Persisted) {
        if self.pending.lock().replace((path, db)).is_some() {
            debug!("a newer save replaced one that was still waiting to be written");
        }
        self.wake.notify_one();
    }

    /// Write whatever is pending, while holding `writing`.
    async fn write_pending(&self) {
        let Some((path, db)) = self.pending.lock().take() else { return };
        let encoded =
            match task::spawn_blocking(move || encode(&db).map(|b| b.freeze())).await {
                Ok(Ok(encoded)) => encoded,
                Ok(Err(e)) => {
                    error!("failed to encode save state {e:?}");
                    return;
                }
                Err(e) => {
                    error!("encoding the save state panicked {e:?}");
                    return;
                }
            };
        if let Err(e) = save(path.clone(), encoded).await {
            error!("failed to save state to {path:?}, {e:?}")
        }
    }

    /// The worker: write each snapshot as it arrives.
    async fn run(self: Arc<Self>) {
        loop {
            self.wake.notified().await;
            let _w = self.writing.lock().await;
            self.write_pending().await;
        }
    }

    /// Wait until every snapshot submitted so far is on disk.
    async fn flush(&self) {
        let _w = self.writing.lock().await;
        self.write_pending().await;
    }

    /// Drop any snapshot not yet written and wait for one being written to
    /// finish, then run `f` before another can start. A campaign reset uses
    /// this: a save landing after it would bring the old round back.
    async fn cancel_then<R>(&self, f: impl FnOnce() -> R) -> R {
        let _w = self.writing.lock().await;
        if self.pending.lock().take().is_some() {
            info!("campaign reset: dropped a save that was still waiting to be written");
        }
        f()
    }
}

/// `Task::ResetState`: drop the live save so the next start is a new round,
/// leaving the marker that says the missing save is deliberate.
fn reset_state(path: &Path) -> Result<()> {
    fs::write(reset_marker_path(path), b"campaign reset\n")
        .context("writing the reset marker")?;
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e.into()),
    }
}

/// Move last session's log aside as `<name><UTC timestamp>.<ext>`.
fn rotate_log(path: &Path) {
    if let Err(e) = fowlbg::rotate::rotate(path) {
        let to = fowlbg::rotate::rotated_path(path, Utc::now());
        println!("could not rotate log file {:?} to {:?} {:?}", path, to, e)
    }
}

/// Environment variable that turns on rotation of `Logs/stats.jsonl`: its
/// size limit in MiB. Unset or 0 (the default) keeps today's behaviour, one
/// file that only grows.
///
/// When set, a `stats.jsonl` at least this big when the engine starts (each
/// mission load) is renamed to `stats-<UTC timestamp>.jsonl` beside it and a
/// new one is started. bfdb's reader follows the rename: it finishes the
/// rotated file, then reads the new one, and a stats rebuild re-reads the
/// rotated files oldest first -- so keep them for as long as you want
/// rebuilds to cover. Needs a bfdb with rotation support (jsonl_cursor_v3);
/// an older bfdb treats the new file as a replacement and can miss the last
/// few seconds written before the rename.
pub(super) const STATS_JSONL_ROTATE_ENV: &str = "FOWL_STATS_JSONL_ROTATE_MB";

/// The configured rotation limit in bytes, if rotation is on.
fn stats_jsonl_rotate_limit() -> Option<u64> {
    let mb = std::env::var(STATS_JSONL_ROTATE_ENV).ok()?.trim().parse::<u64>().ok()?;
    (mb > 0).then(|| mb.saturating_mul(1024 * 1024))
}

/// Where a rotated `stats.jsonl` goes: `stats-<timestamp>.jsonl` in the same
/// directory. Fixed width, so name order is age order (bfdb relies on it).
fn rotated_stats_path(path: &Path, now: DateTime<Utc>) -> PathBuf {
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("stats");
    let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("jsonl");
    path.with_file_name(format!("{stem}-{}.{ext}", now.format("%Y%m%dT%H%M%S%3fZ")))
}

/// Rotate `stats.jsonl` if rotation is on and it has reached the limit.
fn maybe_rotate_stats_jsonl(path: &Path) {
    let Some(limit) = stats_jsonl_rotate_limit() else { return };
    let len = match fs::metadata(path) {
        Ok(md) => md.len(),
        Err(_) => return,
    };
    if len < limit {
        return;
    }
    let to = rotated_stats_path(path, Utc::now());
    match fs::rename(path, &to) {
        Ok(()) => info!("rotated {path:?} ({len} bytes) to {to:?}"),
        Err(e) => {
            error!("could not rotate {path:?} to {to:?}, still appending to it: {e:?}")
        }
    }
}

#[derive(Debug)]
pub(super) enum Task {
    SaveState(PathBuf, Persisted),
    ResetState(PathBuf),
    /// Write the veterans list (`veterans_path`).
    SaveVeterans(PathBuf, Vec<dcso3::net::Ucid>),
    CfgLoaded {
        sortie: dcso3::String,
        cfg: Arc<Cfg>,
        admin_channel: Arc<SegQueue<(AdminCommand, oneshot::Sender<Value>)>>,
        /// True only when this mission load started a genuinely new round
        /// (no saved state to resume). Threaded through to the netidx stats
        /// publisher so it doesn't announce a spurious NewRound on every
        /// technical restart (crash recovery, bot-triggered restart) that
        /// resumes existing saved state.
        fresh: bool,
    },
    SaveConfig(PathBuf, Arc<Cfg>),
    /// rewrite the on-disk mission file's weather (and optionally date/time)
    /// with live real-world conditions. Only takes effect the next time the
    /// mission loads, so this must be enqueued before Task::Shutdown.
    RewriteMissionWeather {
        miz_path: PathBuf,
        cfg: bfprotocols::cfg::LiveWeatherConfig,
    },
    WriteLog(Bytes),
    LogPerf {
        players: usize,
        perf: Perf,
        api_perf: ApiPerf,
    },
    Shutdown(Arc<(Mutex<bool>, Condvar)>),
    Stat(Stat),
    /// Does nothing but move the heartbeat (see `BG_HEARTBEAT`).
    Ping,
}

impl Task {
    fn kind(&self) -> &'static str {
        match self {
            Task::SaveState(..) => "SaveState",
            Task::ResetState(..) => "ResetState",
            Task::SaveVeterans(..) => "SaveVeterans",
            Task::CfgLoaded { .. } => "CfgLoaded",
            Task::SaveConfig(..) => "SaveConfig",
            Task::RewriteMissionWeather { .. } => "RewriteMissionWeather",
            Task::WriteLog(..) => "WriteLog",
            Task::LogPerf { .. } => "LogPerf",
            Task::Shutdown(..) => "Shutdown",
            Task::Stat(..) => "Stat",
            Task::Ping => "Ping",
        }
    }
}

impl fowlbg::Task for Task {
    fn log_line(line: Bytes) -> Self {
        Task::WriteLog(line)
    }

    fn into_log_line(self) -> Option<Bytes> {
        match self {
            Task::WriteLog(line) => Some(line),
            _ => None,
        }
    }

    fn is_log_line(&self) -> bool {
        matches!(self, Task::WriteLog(_))
    }

    fn kind(&self) -> &'static str {
        Task::kind(self)
    }

    fn is_final(&self) -> bool {
        matches!(self, Task::Shutdown(_))
    }
}

/// The last `Task::CfgLoaded`, kept so a restarted background loop can bring
/// netidx (stats publisher, log publisher, engine RPCs) back up by itself --
/// the mission only sends it once, at load.
#[derive(Clone)]
struct CfgReplay {
    sortie: dcso3::String,
    cfg: Arc<Cfg>,
    admin_channel: Arc<SegQueue<(AdminCommand, oneshot::Sender<Value>)>>,
}

enum Logs {
    Netidx {
        publisher: Publisher,
        perf: PubPerf,
        stats: Statspub,
        log: LogPublisher,
        stats_jsonl: Option<JsonlSink>,
    },
    Files {
        log_path: PathBuf,
        log_file: Option<File>,
        stats_path: PathBuf,
        stats_jsonl: Option<JsonlSink>,
    },
}

impl Logs {
    async fn open_files(&mut self) -> Result<()> {
        match self {
            Self::Netidx { .. } => Ok(()),
            Self::Files { log_path, log_file, stats_path: _, .. } => {
                *log_file = Some(
                    File::options().create(true).append(true).open(&log_path).await?,
                );
                Ok(())
            }
        }
    }

    /// Can't fail: if the log file won't open, lines go to the fallback sink
    /// (see `write_log`). This used to `expect()` in the background thread,
    /// which took saves and stats down with the log file.
    async fn new(write_dir: &Path) -> Self {
        let stats_path = write_dir.join("Logs").join("stats");
        let log_path = write_dir.join("Logs").join("bfnext.txt");
        let jsonl_path = write_dir.join("Logs").join("stats.jsonl");
        rotate_log(&log_path);
        maybe_rotate_stats_jsonl(&jsonl_path);
        let stats_jsonl = match fowlbg::jsonl::open_append(&jsonl_path) {
            Ok(f) => {
                info!("stats JSONL file opened at {jsonl_path:?}");
                Some(JsonlSink::new(jsonl_path, Some(f)))
            }
            Err(e) => {
                error!("could not open stats JSONL file at {jsonl_path:?}: {e:?}");
                None
            }
        };
        let mut t = Self::Files { log_file: None, log_path, stats_path, stats_jsonl };
        if let Err(e) = t.open_files().await {
            fallback_log(
                format!(
                    "bflib: could not open the log file, using the fallback log: {e:?}\n"
                )
                .as_bytes(),
            )
        }
        t
    }

    async fn write_log(&mut self, buf: Chars) -> Result<()> {
        match self {
            Self::Netidx { log, .. } => {
                if let Err(e) = log.append(buf.clone()) {
                    fallback_log(buf.as_bytes());
                    bail!("log publisher is gone {e:?}")
                }
                Ok(())
            }
            Self::Files { log_file: Some(log_file), .. } => {
                if let Err(e) = log_file.write_all_buf(&mut buf.as_bytes()).await {
                    fallback_log(buf.as_bytes());
                    bail!("log file write failed {e:?}")
                }
                Ok(())
            }
            Self::Files { .. } => {
                fallback_log(buf.as_bytes());
                Ok(())
            }
        }
    }

    fn write_stat(&mut self, stat: &Stat) -> Result<()> {
        // Write to JSONL file (available in both modes for bfdb to read)
        let jsonl = match self {
            Self::Files { stats_jsonl, .. } => stats_jsonl,
            Self::Netidx { stats_jsonl, .. } => stats_jsonl,
        };
        if let Some(f) = jsonl {
            if let Err(e) = f.append_stamped("stat", stat) {
                error!("failed to write stat to JSONL: {e:?}");
            }
        }
        // Also write to netidx archive if in Netidx mode
        match self {
            Self::Files { .. } => Ok(()),
            Self::Netidx { stats, .. } => stats.append(Utc::now(), stat),
        }
    }

    async fn log_perf(
        &self,
        players: usize,
        perf_stat: &PerfStat,
        api_perf_stat: &ApiPerfStat,
    ) {
        perf_stat.log();
        api_perf_stat.log();
        match self {
            Self::Files { .. } => (),
            Self::Netidx { publisher, perf, .. } => {
                let mut batch = publisher.start_batch();
                perf.update(&mut batch, players, perf_stat, api_perf_stat);
                batch.commit(None).await
            }
        }
    }

    async fn switch_to_netidx(
        &mut self,
        publisher: Publisher,
        cfg: &Config,
        base: NetIdxPath,
        sortie: dcso3::String,
        fresh: bool,
    ) -> Result<()> {
        match self {
            Self::Netidx { .. } => Ok(()),
            Self::Files { log_path, log_file, stats_path, stats_jsonl } => {
                drop(log_file.take());
                let taken_jsonl = stats_jsonl.take();
                let go = || async {
                    let perf = PubPerf::new(
                        &publisher,
                        &base,
                        0,
                        &PerfStat::default(),
                        &ApiPerfStat::default(),
                    )
                    .context("starting pubperf")?;
                    let stats = Statspub::new(
                        publisher.clone(),
                        &cfg,
                        stats_path.clone(),
                        base.append("stats"),
                        sortie.clone(),
                        fresh,
                    )
                    .await
                    .context("starting stats pub")?;
                    let log = LogPublisher::new(
                        publisher.clone(),
                        log_path,
                        base.append("log"),
                    )
                    .context("starting log pub")?;
                    Ok::<_, anyhow::Error>((perf, stats, log))
                };
                match go().await {
                    Ok((perf, stats, log)) => {
                        *self = Self::Netidx {
                            publisher: publisher.clone(),
                            perf,
                            stats,
                            log,
                            stats_jsonl: taken_jsonl,
                        };
                        Ok(())
                    }
                    Err(e) => {
                        *stats_jsonl = taken_jsonl;
                        if let Err(e) = self.open_files().await {
                            error!(
                                "netidx init failed and reopening files also failed {e:?}"
                            )
                        }
                        return Err(e);
                    }
                }
            }
        }
    }

    fn flush_stats(&mut self) -> Result<()> {
        match self {
            Self::Files { .. } => Ok(()),
            Self::Netidx { stats, .. } => task::block_in_place(|| stats.flush()),
        }
    }

    async fn shutdown(&mut self) {
        match self {
            Self::Files { .. } => (),
            Self::Netidx { publisher, log, stats, .. } => {
                let _ = log.close().await;
                let _ = task::block_in_place(|| stats.flush());
                publisher.clone().shutdown().await
            }
        }
    }
}

struct Background {
    write_dir: PathBuf,
    saver: Arc<Saver>,
    /// The save worker for this run of the loop (see `Saver::run`).
    worker: task::JoinHandle<()>,
    logs: Logs,
    rpcs: Option<Rpcs>,
    // The netidx publisher's lifetime has to outlive the CfgLoaded arm and
    // every fallible thing in it. `Rpcs`/`Proc` don't keep it alive on their
    // own, and in the happy path only the stats Recorder (inside `logs`) does
    // -- so if `switch_to_netidx` fails (e.g. a corrupt archive segment:
    // "compressing archive: Src size is incorrect") the publisher used to drop
    // at the end of the arm, taking every engine query RPC with it and leaving
    // the dashboard dark. Holding a clone here keeps the RPCs up regardless.
    publisher: Option<Publisher>,
    /// Outlives a restart of the loop (see `CfgReplay`).
    last_cfg: Arc<Mutex<Option<CfgReplay>>>,
    /// Set by `Task::Shutdown`, signalled in `stop` -- after the shutdown
    /// task, even if it panicked: the mission thread waits on it before it
    /// exits DCS.
    shutdown: Option<Arc<(Mutex<bool>, Condvar)>>,
}

impl Background {
    /// One run of the loop: a fresh start, or a restart after a panic (the
    /// supervisor's), when `last_cfg` and any pending save carry over.
    async fn start(
        write_dir: PathBuf,
        saver: Arc<Saver>,
        last_cfg: Arc<Mutex<Option<CfgReplay>>>,
    ) -> Self {
        let replay = last_cfg.lock().clone();
        let worker = task::spawn(Arc::clone(&saver).run());
        // a snapshot left pending by a loop that died gets written now
        saver.wake.notify_one();
        let logs = Logs::new(&write_dir).await;
        let mut bg = Background {
            write_dir,
            saver,
            worker,
            logs,
            rpcs: None,
            publisher: None,
            last_cfg,
            shutdown: None,
        };
        if let Some(replay) = replay {
            // restarted after a panic: bring netidx back up for the mission that
            // is still running. Never a new round -- the round didn't change.
            error!(
                "background loop restarted, restoring netidx for sortie {:?}",
                replay.sortie
            );
            bg.cfg_loaded(replay, false).await;
        }
        bg
    }

    async fn cfg_loaded(&mut self, replay: CfgReplay, fresh: bool) {
        let CfgReplay { sortie, cfg, admin_channel } = replay.clone();
        *self.last_cfg.lock() = Some(replay);
        // A second CfgLoaded is a new mission in the same DCS process
        // (MissionEnd -> load). The old code kept the first mission's stats
        // publisher and sortie base, because switch_to_netidx is a no-op in
        // netidx mode, while swapping in new RPCs -- so stats went out under the
        // previous sortie. Tear the old netidx state down first.
        if let Logs::Netidx { .. } = &self.logs {
            info!(
                "netidx: new mission loaded, replacing the previous sortie's publishers"
            );
            self.logs.shutdown().await;
            self.rpcs = None;
            self.publisher = None;
            self.logs = Logs::new(&self.write_dir).await;
        }
        if let Some(base) = cfg.netidx_base.as_ref() {
            let base = base.append(&sortie);
            info!(
                "netidx: publishing under {base} (netidx_base={} + sortie={sortie:?}); \
                 bfdb must use --base {} and see this exact sortie",
                cfg.netidx_base.as_ref().unwrap(),
                cfg.netidx_base.as_ref().unwrap(),
            );
            let tls = cfg.netidx_tls.as_ref();
            let cfg = match fowlbg::netidx::load_config(None, tls) {
                Ok(c) => c,
                Err(e) => {
                    error!("failed to load netidx config {e:?}");
                    return;
                }
            };
            if let Some(tls) = tls {
                info!(
                    "netidx: TLS to resolver {} as {}",
                    tls.resolver_name, tls.certificate
                );
            }
            // one try, on the config's default bind
            let publisher = match fowlbg::netidx::start_publisher(&cfg, None, &[]).await {
                Ok(p) => p,
                Err(e) => {
                    error!("failed to init netidx publisher {e:?}");
                    return;
                }
            };
            info!("netidx: publisher bound to {:?}", publisher.addr());
            self.publisher = Some(publisher.clone());
            self.rpcs = match Rpcs::new(&publisher, &admin_channel, &base).await {
                Ok(r) => Some(r),
                Err(e) => {
                    error!("failed to init rpcs {e:?}");
                    None
                }
            };
            if let Err(e) = self
                .logs
                .switch_to_netidx(
                    publisher.clone(),
                    &cfg,
                    base.clone(),
                    sortie.clone(),
                    fresh,
                )
                .await
            {
                // The netidx stats archive picks up a torn segment
                // whenever bfdb (or the mission) is hard-killed --
                // "compressing archive: Src size is incorrect" -- and a
                // cold reopen then chokes on it every restart until
                // someone renames Logs/stats by hand. Quarantine it and
                // retry once so the dashboard heals on its own. The
                // publisher and RPCs are already held open above, so a
                // second failure just means file-mode stats (bfdb reads
                // the JSONL regardless).
                let es = format!("{e:?}");
                let corrupt = es.contains("Src size is incorrect")
                    || es.contains("compressing archive")
                    || es.contains("corrupt");
                let stats_dir = self.write_dir.join("Logs").join("stats");
                if corrupt && stats_dir.exists() {
                    let aside = self.write_dir.join("Logs").join(
                        format_compact!("stats.corrupt-{}", Utc::now().timestamp())
                            .as_str(),
                    );
                    match fs::rename(&stats_dir, &aside) {
                        Ok(()) => {
                            error!(
                                "netidx stats archive corrupt ({e:?}); quarantined to \
                                 {aside:?}, retrying"
                            );
                            if let Err(e2) = self
                                .logs
                                .switch_to_netidx(
                                    publisher.clone(),
                                    &cfg,
                                    base.clone(),
                                    sortie.clone(),
                                    fresh,
                                )
                                .await
                            {
                                error!(
                                    "failed to initialize netidx logs after quarantine \
                                     {e2:?}"
                                )
                            }
                        }
                        Err(re) => error!(
                            "failed to quarantine corrupt stats archive {aside:?}: {re:?} \
                             (original error {e:?})"
                        ),
                    }
                } else {
                    error!("failed to initialize netidx logs {e:?}")
                }
            }
        }
        match &self.logs {
            Logs::Files { .. } => log::info!("log is in file mode"),
            Logs::Netidx { .. } => log::info!("log is in netidx mode"),
        }
    }
}

/// Each task runs inside fowlbg's panic boundary (`Options::supervise`).
/// This one thread does the saves, the log, the stats and every netidx RPC,
/// and a single panic in any of them used to end the loop for good: the
/// mission carried on with nothing saved and nothing logged, and no way to
/// tell from the outside.
impl fowlbg::Handler for Background {
    type Task = Task;

    async fn handle(&mut self, msg: Task) {
        match msg {
            Task::CfgLoaded { sortie, cfg, admin_channel, fresh } => {
                let replay = CfgReplay { sortie, cfg, admin_channel };
                self.cfg_loaded(replay, fresh).await
            }
            Task::SaveState(path, db) => {
                // written by the save worker (see `Saver`)
                self.saver.submit(path, db);
                if let Err(e) = self.logs.flush_stats() {
                    error!("failed to flush stats {e:?}")
                }
            }
            Task::ResetState(path) => {
                match self.saver.cancel_then(|| reset_state(&path)).await {
                    Ok(()) => (),
                    Err(e) => error!("failed to reset state {path:?}, {e:?}"),
                }
            }
            Task::SaveVeterans(path, vets) => {
                let res = serde_json::to_vec(&vets)
                    .map_err(anyhow::Error::from)
                    .and_then(|buf| {
                        let mut tmp = path.clone();
                        tmp.set_extension("veterans.tmp");
                        fs::write(&tmp, buf)?;
                        fs::rename(&tmp, &path)?;
                        Ok(())
                    });
                if let Err(e) = res {
                    error!("failed to save the veterans list to {path:?}, {e:?}")
                }
            }
            Task::SaveConfig(path, cfg) => match cfg.save(&path) {
                Ok(()) => (),
                Err(e) => error!("failed to save config {e:?}"),
            },
            Task::RewriteMissionWeather { miz_path, cfg } => {
                match task::spawn_blocking(move || livewx::apply(&miz_path, &cfg)).await {
                    Ok(Ok(())) => log::info!("applied live weather/time to mission file"),
                    Ok(Err(e)) => {
                        error!("failed to apply live weather to mission file {e:?}")
                    }
                    Err(e) => error!("live weather task panicked {e:?}"),
                }
            }
            Task::WriteLog(buf) => match Chars::from_bytes(buf) {
                Err(e) => eprintln!("invalid unicode log {e:?}"),
                Ok(buf) => {
                    if let Err(e) = self.logs.write_log(buf).await {
                        eprintln!("could not write log line {e:?}")
                    }
                }
            },
            Task::LogPerf { players, perf, api_perf } => {
                self.logs.log_perf(players, &perf.stat(), &api_perf.stat()).await;
            }
            Task::Shutdown(a) => {
                self.shutdown = Some(a);
                // the final save has to be on disk before the mission exits
                self.saver.flush().await;
                println!("starting netidx shutdown");
                self.logs.shutdown().await;
                println!("netidx shutdown complete");
            }
            Task::Stat(st) => {
                if let Err(e) = self.logs.write_stat(&st) {
                    error!("could not write stat {st:?} {e:?}")
                }
            }
            Task::Ping => (),
        }
    }

    /// The loop has ended: the channel closed, or `Task::Shutdown` (the final
    /// task) was handled.
    async fn stop(&mut self) {
        if let Some(a) = self.shutdown.take() {
            let &(ref lock, ref cvar) = &*a;
            let mut synced = lock.lock();
            *synced = true;
            cvar.notify_all();
            println!("condvar signaled, exiting background loop");
            *self.last_cfg.lock() = None;
        }
        self.worker.abort();
    }
}

pub(super) fn init(write_dir: PathBuf) -> UnboundedSender<Task> {
    let opts = fowlbg::Options {
        fallback_log: Some(write_dir.join("Logs").join("bfnext-fallback.txt")),
        // netidx-archive joins a "cluster" even when this server is its only
        // member, and its subscriber then retries the peer path forever, a
        // WARN per attempt (~310 in a 20 minute log). bfdb filters the same
        // module (env_logger `filter_module("netidx::subscriber", Error)`).
        quiet_netidx: true,
        max_queued_log_lines: Some(MAX_QUEUED_LOG_LINES),
        // a panicked task is reported and the loop goes on; a dead loop is
        // started again on a fresh runtime instead of leaving the mission with
        // no saves, no log and no stats for the rest of the session
        supervise: true,
        ..fowlbg::Options::new("bflib")
    };
    // outlive a restart of the loop, with any save still pending
    let last_cfg: Arc<Mutex<Option<CfgReplay>>> = Arc::default();
    let saver = Arc::new(Saver::default());
    fowlbg::init(opts, move |_restart| {
        Background::start(write_dir.clone(), Arc::clone(&saver), Arc::clone(&last_cfg))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use tokio::runtime::Builder;

    fn scratch(name: &str) -> PathBuf {
        let dir = env::temp_dir()
            .join(format!("bflib-save-test-{name}-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn treasury_on_disk(path: &Path) -> i64 {
        let raw = zstd::stream::decode_all(&fs::read(path).unwrap()[..]).unwrap();
        serde_json::from_slice::<Persisted>(&raw).unwrap().blue_treasury as i64
    }

    fn snapshot(treasury: i64) -> Persisted {
        let mut p = Persisted::default();
        p.blue_treasury = treasury as _;
        p
    }

    #[test]
    fn saver_writes_newest_and_reset_cancels_pending() {
        let dir = scratch("saver");
        let live = dir.join("campaign");
        let rt = Builder::new_multi_thread().enable_all().build().unwrap();
        rt.block_on(async {
            let saver = Arc::new(Saver::default());
            // two snapshots before anything is written: only the newer one is
            saver.submit(live.clone(), snapshot(1));
            saver.submit(live.clone(), snapshot(2));
            saver.flush().await;
            assert_eq!(treasury_on_disk(&live), 2);
            // the worker writes on its own
            let worker = task::spawn(Arc::clone(&saver).run());
            saver.submit(live.clone(), snapshot(3));
            for _ in 0..200 {
                if live.exists() && treasury_on_disk(&live) == 3 {
                    break;
                }
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
            assert_eq!(treasury_on_disk(&live), 3);
            // a reset drops a snapshot still waiting; nothing brings it back
            worker.abort();
            saver.submit(live.clone(), snapshot(4));
            saver.cancel_then(|| reset_state(&live)).await.unwrap();
            saver.flush().await;
            assert!(!live.exists());
        });
        let _ = fs::remove_dir_all(&dir);
    }

    /// The mission thread waits on the shutdown condvar before it exits DCS:
    /// it must be signalled once the final task is done, and a supervisor
    /// restart after that must not bring the old mission's netidx back.
    #[test]
    fn shutdown_signals_and_clears_the_replay() {
        use fowlbg::{Handler, Task as _};
        let dir = scratch("shutdown");
        fs::create_dir_all(dir.join("Logs")).unwrap();
        let rt = Builder::new_multi_thread().enable_all().build().unwrap();
        rt.block_on(async {
            let last_cfg: Arc<Mutex<Option<CfgReplay>>> = Arc::default();
            let mut bg =
                Background::start(dir.clone(), Arc::default(), Arc::clone(&last_cfg))
                    .await;
            *last_cfg.lock() = Some(CfgReplay {
                sortie: "s".into(),
                cfg: Arc::new(Cfg::default()),
                admin_channel: Arc::default(),
            });
            let sig = Arc::new((Mutex::new(false), Condvar::new()));
            let task = Task::Shutdown(Arc::clone(&sig));
            assert!(task.is_final());
            assert_eq!(fowlbg::Task::kind(&task), "Shutdown");
            bg.handle(task).await;
            assert!(!*sig.0.lock(), "signalled by stop, after the task");
            bg.stop().await;
            assert!(*sig.0.lock());
            assert!(last_cfg.lock().is_none());
        });
        drop(rt);
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn rotated_stats_names_sort_by_age() {
        let live = Path::new("Logs").join("stats.jsonl");
        let t0 = Utc.with_ymd_and_hms(2026, 10, 25, 9, 59, 59).unwrap();
        let t1 = t0 + chrono::Duration::milliseconds(1);
        let t2 = t0 + chrono::Duration::days(40);
        let (a, b, c) = (
            rotated_stats_path(&live, t0),
            rotated_stats_path(&live, t1),
            rotated_stats_path(&live, t2),
        );
        // bfdb looks for `stats-*.jsonl` next to the live file and reads them
        // in name order
        assert_eq!(a, Path::new("Logs").join("stats-20261025T095959000Z.jsonl"));
        assert!(a < b && b < c);
    }

    #[test]
    fn save_backup_keeps_live_file() {
        let dir = scratch("backup");
        let live = dir.join("campaign");
        fs::write(&live, b"old").unwrap();
        backup_state(&live, Utc::now()).unwrap();
        // the live save is still there, and a backup of it exists
        assert_eq!(fs::read(&live).unwrap(), b"old");
        let backups = backup_saves(&live);
        assert_eq!(backups.len(), 1);
        assert_eq!(fs::read(&backups[0]).unwrap(), b"old");
        // replacing the live file must not change the backup (hard link)
        let tmp = save_tmp_path(&live);
        fs::write(&tmp, b"new").unwrap();
        fs::rename(&tmp, &live).unwrap();
        assert_eq!(fs::read(&live).unwrap(), b"new");
        assert_eq!(fs::read(&backups[0]).unwrap(), b"old");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn save_reset_marker_blocks_recovery() {
        let dir = scratch("reset");
        let live = dir.join("campaign");
        fs::write(&live, b"old").unwrap();
        backup_state(&live, Utc::now()).unwrap();
        fs::write(save_tmp_path(&live), b"tmp").unwrap();
        assert_eq!(backup_saves(&live).len(), 2);
        reset_state(&live).unwrap();
        assert!(!live.exists());
        assert!(backup_saves(&live).is_empty());
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn save_prune_thins_old_backups() {
        let dir = scratch("prune");
        let live = dir.join("campaign");
        let now = Utc::now();
        // three backups in the same two-hour-old bucket, one recent
        for age in [7200 + 10, 7200 + 20, 7200 + 30, 30] {
            fs::write(dir.join(format!("campaign{}", now.timestamp() - age)), b"x")
                .unwrap();
        }
        prune_backups(&live, now).unwrap();
        let left = backup_saves(&live);
        assert_eq!(left.len(), 2, "{left:?}");
        let _ = fs::remove_dir_all(&dir);
    }
}
