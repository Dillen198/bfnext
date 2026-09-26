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

mod live_weather;
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
use compact_str::{CompactString, format_compact};
use crossbeam::queue::SegQueue;
use dcso3::perf::{Perf as ApiPerf, PerfStat as ApiPerfStat};
use futures::FutureExt;
use fxhash::FxHashMap;
use log::{error, info, Level, Log, Metadata, Record};
use logpub::LogPublisher;
use netidx::{
    chars::Chars,
    config::Config,
    path::Path as NetIdxPath,
    publisher::{Publisher, PublisherBuilder, Value},
};
use once_cell::sync::OnceCell;
use parking_lot::{Condvar, Mutex};
use perf::PubPerf;
use rpcs::Rpcs;
use serde::Serialize;
use simplelog::{LevelFilter, WriteLogger};
use statspub::Statspub;
use std::{
    cell::RefCell,
    env,
    ffi::OsStr,
    fs, io,
    panic::{AssertUnwindSafe, catch_unwind},
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicI64, Ordering},
    },
    thread,
};
use tokio::{
    fs::File,
    io::AsyncWriteExt,
    runtime::Builder,
    sync::{
        mpsc::{self, UnboundedReceiver, UnboundedSender},
        oneshot,
    },
    task,
};

thread_local! {
    static LOGBUF: RefCell<BytesMut> = RefCell::new(BytesMut::new());
}

/// Where log lines go when the background thread can't take them.
static FALLBACK_LOG: OnceCell<PathBuf> = OnceCell::new();

/// Unix seconds of the last task the background loop finished. The main
/// thread pings it every minute (`Task::Ping`), so a value that stops moving
/// means the loop is wedged even though its channel is still open.
static BG_HEARTBEAT: AtomicI64 = AtomicI64::new(0);

/// Last-resort log sink that doesn't depend on the background thread.
///
/// Every log line, every stat and every save goes through that one thread,
/// so when it died the logger kept handing lines to a closed channel and the
/// only record of *why* it died -- the panic hook's message -- went with them.
/// This appends straight to `Logs/bfnext-fallback.txt` (and stderr) from
/// whatever thread is logging. It opens the file per call, which is slow, but
/// it only runs when the normal path is already broken.
pub(super) fn fallback_log(buf: &[u8]) {
    use std::io::Write;
    let _ = io::stderr().write_all(buf);
    if let Some(path) = FALLBACK_LOG.get() {
        if let Ok(mut f) = fs::OpenOptions::new().create(true).append(true).open(path) {
            let _ = f.write_all(buf);
        }
    }
}

/// Seconds since the background loop last finished a task, `None` before the
/// first one.
pub(super) fn heartbeat_age(now: DateTime<Utc>) -> Option<i64> {
    match BG_HEARTBEAT.load(Ordering::Relaxed) {
        0 => None,
        ts => Some(now.timestamp().saturating_sub(ts)),
    }
}

struct LogHandle(UnboundedSender<Task>);

impl io::Write for LogHandle {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        LOGBUF.with_borrow_mut(|lbuf| {
            lbuf.extend_from_slice(buf);
            if lbuf.len() > 0 && lbuf[lbuf.len() - 1] == 0xA {
                let line = lbuf.split().freeze();
                if let Err(e) = self.0.send(Task::WriteLog(line)) {
                    // The background thread is gone. Don't return an error --
                    // the logger would just drop the line -- write it where it
                    // can still be read.
                    if let Task::WriteLog(line) = e.0 {
                        fallback_log(&line)
                    }
                }
            }
            Ok(buf.len())
        })
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

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
    backup.set_file_name(format_compact!("{}{}", save_name(path)?, now.timestamp()).as_str());
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
        let dir = path
            .parent()
            .ok_or_else(|| anyhow!("path has no parent dir"))?;
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
                            by_age
                                .entry((age / day) * day)
                                .or_default()
                                .push((ts, file));
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
        let file = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(&tmp)?;
        let mut file = zstd::stream::Encoder::new(file, 9)?;
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

fn rotate_log(path: &Path) {
    if path.exists() {
        let ext = path
            .extension()
            .unwrap_or(&OsStr::new("ext"))
            .to_str()
            .unwrap_or("inv");
        let mut rotate_path = path.to_path_buf();
        rotate_path.set_extension("");
        let name = rotate_path
            .file_name()
            .unwrap_or(&OsStr::new("nameless"))
            .to_str()
            .unwrap_or("invalid");
        let ts = Utc::now()
            .to_rfc3339_opts(SecondsFormat::Secs, true)
            .chars()
            .filter(|c| c != &'-' && c != &':')
            .collect::<CompactString>();
        rotate_path.set_file_name(format_compact!("{name}{ts}.{ext}"));
        if let Err(e) = fs::rename(&path, &rotate_path) {
            println!(
                "could not rotate log file {:?} to {:?} {:?}",
                path, rotate_path, e
            )
        }
    }
}

#[derive(Debug)]
pub(super) enum Task {
    SaveState(PathBuf, Persisted),
    ResetState(PathBuf),
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
        stats_jsonl: Option<std::fs::File>,
    },
    Files {
        log_path: PathBuf,
        log_file: Option<File>,
        stats_path: PathBuf,
        stats_jsonl: Option<std::fs::File>,
    },
}

impl Logs {
    async fn open_files(&mut self) -> Result<()> {
        match self {
            Self::Netidx { .. } => Ok(()),
            Self::Files {
                log_path,
                log_file,
                stats_path: _, ..
            } => {
                *log_file = Some(
                    File::options()
                        .create(true)
                        .write(true)
                        .open(&log_path)
                        .await?,
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
        let stats_jsonl = match std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&jsonl_path)
        {
            Ok(f) => {
                info!("stats JSONL file opened at {jsonl_path:?}");
                Some(f)
            }
            Err(e) => {
                error!("could not open stats JSONL file at {jsonl_path:?}: {e:?}");
                None
            }
        };
        let mut t = Self::Files {
            log_file: None,
            log_path,
            stats_path,
            stats_jsonl,
        };
        if let Err(e) = t.open_files().await {
            fallback_log(format!("bflib: could not open the log file, using the fallback log: {e:?}\n").as_bytes())
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
            Self::Files {
                log_file: Some(log_file),
                ..
            } => {
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
            use std::io::Write;
            let ts = Utc::now();
            // One write per record: `writeln!` on an unbuffered File issues
            // several, so bfdb could read a record half-written.
            let line = format!("{}\n", serde_json::json!({"ts": ts.to_rfc3339(), "stat": stat}));
            if let Err(e) = f.write_all(line.as_bytes()) {
                error!("failed to write stat to JSONL: {e:?}");
            }
        }
        // Also write to netidx archive if in Netidx mode
        match self {
            Self::Files { .. } => Ok(()),
            Self::Netidx { stats, .. } => stats.append(Utc::now(), stat),
        }
    }

    async fn log_perf(&self, players: usize, perf_stat: &PerfStat, api_perf_stat: &ApiPerfStat) {
        perf_stat.log();
        api_perf_stat.log();
        match self {
            Self::Files { .. } => (),
            Self::Netidx {
                publisher, perf, ..
            } => {
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
            Self::Files {
                log_path,
                log_file,
                stats_path,
                stats_jsonl,
            } => {
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
                    let log = LogPublisher::new(publisher.clone(), log_path, base.append("log"))
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
                            error!("netidx init failed and reopening files also failed {e:?}")
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
            Self::Netidx {
                publisher,
                log,
                stats,
                ..
            } => {
                let _ = log.close().await;
                let _ = task::block_in_place(|| stats.flush());
                publisher.clone().shutdown().await
            }
        }
    }
}

struct Background<'a> {
    write_dir: &'a Path,
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
    last_cfg: &'a mut Option<CfgReplay>,
}

impl<'a> Background<'a> {
    async fn cfg_loaded(&mut self, replay: CfgReplay, fresh: bool) {
        let CfgReplay {
            sortie,
            cfg,
            admin_channel,
        } = replay.clone();
        *self.last_cfg = Some(replay);
        // A second CfgLoaded is a new mission in the same DCS process
        // (MissionEnd -> load). The old code kept the first mission's stats
        // publisher and sortie base, because switch_to_netidx is a no-op in
        // netidx mode, while swapping in new RPCs -- so stats went out under the
        // previous sortie. Tear the old netidx state down first.
        if let Logs::Netidx { .. } = &self.logs {
            info!("netidx: new mission loaded, replacing the previous sortie's publishers");
            self.logs.shutdown().await;
            self.rpcs = None;
            self.publisher = None;
            self.logs = Logs::new(self.write_dir).await;
        }
        if let Some(base) = cfg.netidx_base.as_ref() {
            let base = base.append(&sortie);
            info!(
                "netidx: publishing under {base} (netidx_base={} + sortie={sortie:?}); \
                 bfdb must use --base {} and see this exact sortie",
                cfg.netidx_base.as_ref().unwrap(),
                cfg.netidx_base.as_ref().unwrap(),
            );
            let cfg = match Config::load_default() {
                Ok(c) => c,
                Err(e) => {
                    error!("failed to load netidx config {e:?}");
                    return;
                }
            };
            let publisher = match PublisherBuilder::new(cfg.clone()).build().await {
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
                .switch_to_netidx(publisher.clone(), &cfg, base.clone(), sortie.clone(), fresh)
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
                        format_compact!("stats.corrupt-{}", Utc::now().timestamp()).as_str(),
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

    async fn handle(&mut self, msg: Task) {
        match msg {
            Task::CfgLoaded {
                sortie,
                cfg,
                admin_channel,
                fresh,
            } => {
                let replay = CfgReplay {
                    sortie,
                    cfg,
                    admin_channel,
                };
                self.cfg_loaded(replay, fresh).await
            }
            Task::SaveState(path, db) => {
                let encoded = match encode(&db) {
                    Ok(encoded) => encoded.freeze(),
                    Err(e) => {
                        error!("failed to encode save state {e:?}");
                        return;
                    }
                };
                drop(db); // don't hold the db reference any longer than necessary
                if let Err(e) = save(path.clone(), encoded).await {
                    error!("failed to save state to {path:?}, {e:?}")
                }
                if let Err(e) = self.logs.flush_stats() {
                    error!("failed to flush stats {e:?}")
                }
            }
            Task::ResetState(path) => match reset_state(&path) {
                Ok(()) => (),
                Err(e) => error!("failed to reset state {path:?}, {e:?}"),
            },
            Task::SaveConfig(path, cfg) => match cfg.save(&path) {
                Ok(()) => (),
                Err(e) => error!("failed to save config {e:?}"),
            },
            Task::RewriteMissionWeather { miz_path, cfg } => {
                let req = live_weather::LiveWeatherRequest { miz_path, cfg };
                match task::spawn_blocking(move || live_weather::apply(&req)).await {
                    Ok(Ok(())) => log::info!("applied live weather/time to mission file"),
                    Ok(Err(e)) => error!("failed to apply live weather to mission file {e:?}"),
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
            Task::LogPerf {
                players,
                perf,
                api_perf,
            } => {
                self.logs
                    .log_perf(players, &perf.stat(), &api_perf.stat())
                    .await;
            }
            Task::Shutdown(_) => {
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
}

/// Runs until the channel closes or a `Task::Shutdown` is processed.
///
/// Each task is handled inside its own panic boundary. This one thread does
/// the saves, the log, the stats and every netidx RPC, and a single panic in
/// any of them used to end the loop for good: the mission carried on with
/// nothing saved and nothing logged, and no way to tell from the outside.
async fn background_loop(
    write_dir: &Path,
    rx: &mut UnboundedReceiver<Task>,
    last_cfg: &mut Option<CfgReplay>,
) {
    let replay = last_cfg.clone();
    let mut bg = Background {
        write_dir,
        logs: Logs::new(write_dir).await,
        rpcs: None,
        publisher: None,
        last_cfg,
    };
    if let Some(replay) = replay {
        // restarted after a panic: bring netidx back up for the mission that
        // is still running. Never a new round -- the round didn't change.
        error!("background loop restarted, restoring netidx for sortie {:?}", replay.sortie);
        bg.cfg_loaded(replay, false).await;
    }
    while let Some(msg) = rx.recv().await {
        let shutdown = match &msg {
            Task::Shutdown(a) => Some(Arc::clone(a)),
            _ => None,
        };
        let kind = msg.kind();
        if let Err(e) = AssertUnwindSafe(bg.handle(msg)).catch_unwind().await {
            let m = e
                .downcast_ref::<&'static str>()
                .copied()
                .or_else(|| e.downcast_ref::<std::string::String>().map(|s| s.as_str()))
                .unwrap_or("<panic payload was not a string>");
            fallback_log(format!("bflib: background task {kind} panicked: {m}\n").as_bytes());
            error!("background task {kind} panicked: {m}")
        }
        BG_HEARTBEAT.store(Utc::now().timestamp(), Ordering::Relaxed);
        if let Some(a) = shutdown {
            // signalled even if the shutdown itself panicked -- the mission
            // thread is blocked on this condvar
            let &(ref lock, ref cvar) = &*a;
            let mut synced = lock.lock();
            *synced = true;
            cvar.notify_all();
            println!("condvar signaled, exiting background loop");
            *bg.last_cfg = None;
            break;
        }
    }
}

static TXCOM: OnceCell<mpsc::UnboundedSender<Task>> = OnceCell::new();

fn setup_logger(tx: UnboundedSender<Task>) {
    // Default to Info -- Debug emits a few tens of thousands of lines an hour
    // on a populated server (per-crate C-130 cargo polling, per-tick carrier
    // position dumps, unknown-event chatter), which is real disk/IO load over
    // a long campaign. Set RUST_LOG=debug to get it back.
    let level = match env::var("RUST_LOG").ok().map(|s| s.to_ascii_lowercase()) {
        None => LevelFilter::Info,
        Some(s) if &s == "trace" => LevelFilter::Trace,
        Some(s) if &s == "debug" => LevelFilter::Debug,
        Some(s) if &s == "info" => LevelFilter::Info,
        Some(s) if &s == "error" => LevelFilter::Error,
        Some(s) if &s == "warn" => LevelFilter::Warn,
        Some(s) if &s == "off" => LevelFilter::Off,
        Some(_) => LevelFilter::Info,
    };
    let logger = WriteLogger::new(level, simplelog::Config::default(), LogHandle(tx));
    log::set_boxed_logger(Box::new(QuietNetidx(*logger))).expect("could not init logger");
    log::set_max_level(level);
}

/// Wraps the real logger to drop netidx's subscriber chatter below Error.
///
/// netidx-archive joins a "cluster" even when this server is its only member,
/// and the subscriber then retries the peer path forever, logging a WARN per
/// attempt: a 20 minute log carried ~310 copies of `resubscription error
/// /local/fowl/campaign/<x>/stats/cluster/publish/<uuid>: no such value`, which
/// is every warning that actually mattered buried under one benign retry loop.
/// bfdb already filters the same module the same way (env_logger
/// `filter_module("netidx::subscriber", Error)` in its main). simplelog has no
/// per-module level, and `ConfigBuilder::add_filter_ignore_str` would throw
/// away genuine netidx errors along with the noise, so filter here instead.
struct QuietNetidx<L>(L);

impl<L: Log> Log for QuietNetidx<L> {
    fn enabled(&self, m: &Metadata) -> bool {
        if m.level() > Level::Error && m.target().starts_with("netidx::subscriber") {
            return false;
        }
        // Routine at INFO, every minute, forever: the resolver write
        // connection's heartbeat connect/drop (5 lines a minute) and the stats
        // archive's per-minute "rotating log file". Warnings still get through.
        if m.level() > Level::Warn
            && (m.target().starts_with("netidx::resolver_client")
                || m.target().starts_with("netidx::channel")
                || m.target().starts_with("netidx_archive::logfile_collection"))
        {
            return false;
        }
        self.0.enabled(m)
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            self.0.log(record)
        }
    }

    fn flush(&self) {
        self.0.flush()
    }
}

pub(super) fn init(write_dir: PathBuf) -> UnboundedSender<Task> {
    match TXCOM.get() {
        Some(tx) => tx.clone(),
        None => {
            let (tx, mut rx) = mpsc::unbounded_channel();
            TXCOM.set(tx.clone()).expect("txcom is already set");
            let _ = FALLBACK_LOG.set(write_dir.join("Logs").join("bfnext-fallback.txt"));
            setup_logger(tx.clone());
            thread::spawn(move || {
                // Supervisor. `background_loop` already survives a panic in
                // any one task; this catches whatever gets past that (the
                // runtime itself, the loop's own setup) and starts it again on
                // a fresh runtime instead of leaving the mission with no saves,
                // no log and no stats for the rest of the session.
                let mut last_cfg: Option<CfgReplay> = None;
                let mut restarts: u32 = 0;
                loop {
                    let rt = match Builder::new_multi_thread().enable_all().build() {
                        Ok(rt) => rt,
                        Err(e) => {
                            fallback_log(
                                format!("bflib: could not start the async runtime {e:?}\n")
                                    .as_bytes(),
                            );
                            thread::sleep(std::time::Duration::from_secs(5));
                            continue;
                        }
                    };
                    let res = catch_unwind(AssertUnwindSafe(|| {
                        rt.block_on(background_loop(&write_dir, &mut rx, &mut last_cfg))
                    }));
                    match res {
                        Ok(()) => break,
                        Err(e) => {
                            restarts = restarts.saturating_add(1);
                            let m = e
                                .downcast_ref::<&'static str>()
                                .copied()
                                .or_else(|| {
                                    e.downcast_ref::<std::string::String>().map(|s| s.as_str())
                                })
                                .unwrap_or("<panic payload was not a string>");
                            fallback_log(
                                format!(
                                    "bflib: background loop died ({m}), restart #{restarts}\n"
                                )
                                .as_bytes(),
                            );
                            drop(rt);
                            thread::sleep(std::time::Duration::from_secs(1));
                        }
                    }
                }
                println!("background thread exiting")
            });
            tx
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scratch(name: &str) -> PathBuf {
        let dir = env::temp_dir().join(format!("bflib-save-test-{name}-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
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
            fs::write(dir.join(format!("campaign{}", now.timestamp() - age)), b"x").unwrap();
        }
        prune_backups(&live, now).unwrap();
        let left = backup_saves(&live);
        assert_eq!(left.len(), 2, "{left:?}");
        let _ = fs::remove_dir_all(&dir);
    }
}
