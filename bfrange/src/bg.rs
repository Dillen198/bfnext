// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! The background thread: logging, the two JSONL feeds bfdb tails, and the
//! netidx RPCs bfdb calls.
//!
//! DCS's Lua runs on one thread and must never block, so everything that
//! touches a file or the network happens here, fed by an unbounded channel.
//! RPC calls come the other way through a lock-free queue the engine drains
//! on its fast tick.

use anyhow::Result;
use bfprotocols::{range::RangeRecord, stats::Stat};
use chrono::prelude::*;
use crossbeam::queue::SegQueue;
use futures::{channel::mpsc as fmpsc, StreamExt};
use log::{error, info, LevelFilter};
use netidx::{
    chars::Chars,
    config::Config,
    path::Path as NetidxPath,
    publisher::{BindCfg, Publisher, PublisherBuilder, Value},
};
use arcstr::ArcStr;
use netidx_protocols::{
    define_rpc,
    rpc::server::{ArgSpec, Proc, RpcCall},
    rpc_err,
};
use once_cell::sync::OnceCell;
use simplelog::WriteLogger;
use std::{
    fs::{self, File, OpenOptions},
    io::{self, Write},
    path::{Path, PathBuf},
    sync::Arc,
    thread,
};
use tokio::{
    runtime::Builder,
    sync::{
        mpsc::{self, UnboundedReceiver, UnboundedSender},
        oneshot,
    },
    task,
};

/// A request from bfdb, answered by the engine on the DCS thread.
#[derive(Debug)]
pub enum Cmd {
    QueryRange,
    QueryCatalog,
    QueryWeapons,
    Spawn(bfprotocols::range::SpawnRequest),
    Despawn { ucid: String, spawn_id: String, instructor: bool },
    ResetStation { ucid: String, station: String, instructor: bool },
}

pub type CmdQueue = Arc<SegQueue<(Cmd, oneshot::Sender<Value>)>>;

/// Where and how the RPCs are published, from the range config.
#[derive(Debug, Clone, Default)]
pub struct NetCfg {
    pub base: Option<NetidxPath>,
    /// A `client.json` path; `None` is netidx's default search.
    pub config: Option<String>,
    /// A netidx bind spec overriding the client config's default.
    pub bind: Option<String>,
}

#[derive(Debug)]
pub enum Task {
    WriteLog(Vec<u8>),
    Stat(Stat),
    Record(Box<RangeRecord>),
    Start { sortie: String, net: NetCfg, q: CmdQueue },
}

thread_local! {
    /// Partial log line; the logger writes a record in pieces and only a
    /// complete line is sent to the background thread.
    static LOGBUF: std::cell::RefCell<Vec<u8>> = std::cell::RefCell::new(Vec::new());
}

struct LogHandle(UnboundedSender<Task>);

impl io::Write for LogHandle {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        LOGBUF.with_borrow_mut(|b| {
            b.extend_from_slice(buf);
            if b.last() == Some(&b'\n') {
                let line = std::mem::take(b);
                self.0
                    .send(Task::WriteLog(line))
                    .map_err(|_| io::Error::new(io::ErrorKind::Other, "background thread gone"))?;
            }
            Ok(buf.len())
        })
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn open_append(p: &Path) -> Option<File> {
    match OpenOptions::new().create(true).append(true).open(p) {
        Ok(f) => Some(f),
        Err(e) => {
            eprintln!("bfrange: could not open {p:?}: {e:?}");
            None
        }
    }
}

fn rotate(p: &Path) {
    if p.exists() {
        let ts = Utc::now().format("%Y%m%dT%H%M%SZ");
        let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("bfrange");
        let ext = p.extension().and_then(|s| s.to_str()).unwrap_or("txt");
        let to = p.with_file_name(format!("{stem}{ts}.{ext}"));
        let _ = fs::rename(p, to);
    }
}

struct Sinks {
    log: Option<File>,
    stats: Option<File>,
    range: Option<File>,
}

impl Sinks {
    fn new(write_dir: &Path) -> Self {
        let logs = write_dir.join("Logs");
        let _ = fs::create_dir_all(&logs);
        let log_path = logs.join("bfrange.txt");
        rotate(&log_path);
        Self {
            log: open_append(&log_path),
            stats: open_append(&logs.join("stats.jsonl")),
            range: open_append(&logs.join("range.jsonl")),
        }
    }

    fn stat(&mut self, st: &Stat) {
        if let Some(f) = &mut self.stats {
            let line = serde_json::json!({"ts": Utc::now().to_rfc3339(), "stat": st});
            if let Err(e) = writeln!(f, "{line}") {
                error!("could not write stat: {e:?}")
            }
        }
    }

    fn record(&mut self, r: &RangeRecord) {
        if let Some(f) = &mut self.range {
            match serde_json::to_string(r) {
                Ok(s) => {
                    if let Err(e) = writeln!(f, "{s}") {
                        error!("could not write range record: {e:?}")
                    }
                    let _ = f.flush();
                }
                Err(e) => error!("could not encode range record {}: {e:?}", r.id),
            }
        }
    }
}

async fn wait_task(mut ch: fmpsc::Receiver<(RpcCall, oneshot::Receiver<Value>)>) {
    while let Some((mut c, rx)) = ch.next().await {
        match rx.await {
            Err(_) => c.reply.send(Value::Error("the engine dropped the call".into())),
            Ok(v) => c.reply.send(v),
        }
    }
}

/// The RPC procs. Held for their lifetime only.
#[allow(dead_code)]
struct Rpcs {
    query_range: Proc,
    query_catalog: Proc,
    query_weapons: Proc,
    spawn: Proc,
    despawn: Proc,
    reset_station: Proc,
}

impl Rpcs {
    fn new(publisher: &Publisher, q: &CmdQueue, base: &NetidxPath) -> Result<Self> {
        let base = base.append("api");
        let (wait, rx) = fmpsc::channel(10);
        task::spawn(wait_task(rx));
        let _q = Arc::clone(q);
        let query_range = define_rpc!(
            publisher,
            base.append("query-range"),
            "Live range picture: players, stations, tankers, carriers, spawns (returns JSON RangeLive)",
            |c: RpcCall, _a: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((Cmd::QueryRange, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(q);
        let query_catalog = define_rpc!(
            publisher,
            base.append("query-range-catalog"),
            "What players can spawn on the range (returns JSON SpawnCatalog)",
            |c: RpcCall, _a: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((Cmd::QueryCatalog, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(q);
        let query_weapons = define_rpc!(
            publisher,
            base.append("query-weapons"),
            "Bomb ballistic data from the running DCS install (returns JSON WeaponDb)",
            |c: RpcCall, _a: Value| {
                let (tx, rx) = oneshot::channel();
                _q.push((Cmd::QueryWeapons, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            arg: Value = Value::Null; ""
        )?;
        let _q = Arc::clone(q);
        let spawn = define_rpc!(
            publisher,
            base.append("range-spawn"),
            "Spawn a catalog item for a player (arg: JSON SpawnRequest; returns JSON SpawnReply)",
            |mut c: RpcCall, req: Chars| {
                let (tx, rx) = oneshot::channel();
                match serde_json::from_str::<bfprotocols::range::SpawnRequest>(&req) {
                    Ok(req) => {
                        _q.push((Cmd::Spawn(req), tx));
                        Some((c, rx))
                    }
                    Err(e) => {
                        c.reply.send(Value::Error(format!("bad spawn request: {e}").into()));
                        None
                    }
                }
            },
            Some(wait.clone()),
            req: Chars = Value::Null; "JSON SpawnRequest"
        )?;
        let _q = Arc::clone(q);
        let despawn = define_rpc!(
            publisher,
            base.append("range-despawn"),
            "Remove one of a player's spawns, or all of them with spawn_id = \"all\"",
            |c: RpcCall, ucid: Chars, spawn_id: Chars, instructor: bool| {
                let (tx, rx) = oneshot::channel();
                _q.push((
                    Cmd::Despawn {
                        ucid: ucid.to_string(),
                        spawn_id: spawn_id.to_string(),
                        instructor,
                    },
                    tx,
                ));
                Some((c, rx))
            },
            Some(wait.clone()),
            ucid: Chars = Value::Null; "The calling player's ucid",
            spawn_id: Chars = Value::Null; "The spawn id, or \"all\"",
            instructor: bool = false; "The caller is an instructor/admin"
        )?;
        let _q = Arc::clone(q);
        let reset_station = define_rpc!(
            publisher,
            base.append("range-reset-station"),
            "Respawn every target at a station",
            |c: RpcCall, ucid: Chars, station: Chars, instructor: bool| {
                let (tx, rx) = oneshot::channel();
                _q.push((
                    Cmd::ResetStation {
                        ucid: ucid.to_string(),
                        station: station.to_string(),
                        instructor,
                    },
                    tx,
                ));
                Some((c, rx))
            },
            Some(wait.clone()),
            ucid: Chars = Value::Null; "The calling player's ucid (may be empty for admins)",
            station: Chars = Value::Null; "The station id",
            instructor: bool = false; "The caller is an instructor/admin"
        )?;
        Ok(Self { query_range, query_catalog, query_weapons, spawn, despawn, reset_station })
    }
}

async fn background_loop(write_dir: PathBuf, mut rx: UnboundedReceiver<Task>) {
    let mut sinks = Sinks::new(&write_dir);
    let mut _rpcs: Option<Rpcs> = None;
    let mut _publisher: Option<Publisher> = None;
    while let Some(t) = rx.recv().await {
        match t {
            Task::WriteLog(line) => {
                if let Some(f) = &mut sinks.log {
                    let _ = f.write_all(&line);
                }
            }
            Task::Stat(st) => sinks.stat(&st),
            Task::Record(r) => sinks.record(&r),
            Task::Start { sortie, net, q } => {
                let Some(base) = net.base else {
                    info!("no netidx_base in the range config: RPCs disabled, JSONL only");
                    continue;
                };
                let base = base.append(&sortie);
                // a mission restart starts again: retire the old RPCs first so
                // the resolver never lists two publishers for one path
                _rpcs = None;
                _publisher = None;
                let cfg = match &net.config {
                    Some(path) => Config::load(path),
                    None => Config::load_default(),
                };
                let cfg = match cfg {
                    Ok(c) => c,
                    Err(e) => {
                        let from = net.config.as_deref().unwrap_or("the default location");
                        error!("failed to load the netidx config from {from}: {e:?}");
                        continue;
                    }
                };
                let bind = match net.bind.as_deref().map(str::parse::<BindCfg>).transpose() {
                    Ok(b) => b,
                    Err(e) => {
                        error!("netidx_bind in the range config is not a bind address: {e:?}");
                        continue;
                    }
                };
                let publisher = match PublisherBuilder::new(cfg).bind_cfg(bind).build().await {
                    Ok(p) => p,
                    Err(e) => {
                        error!("failed to start the netidx publisher: {e:?}");
                        continue;
                    }
                };
                info!("netidx: publishing on {}", publisher.addr());
                info!("netidx: range RPCs under {base}/api (bfdb's instance base must be the parent)");
                match Rpcs::new(&publisher, &q, &base) {
                    Ok(r) => _rpcs = Some(r),
                    Err(e) => error!("failed to publish range RPCs: {e:?}"),
                }
                _publisher = Some(publisher);
            }
        }
    }
}

static TX: OnceCell<UnboundedSender<Task>> = OnceCell::new();

pub fn init(write_dir: PathBuf) -> UnboundedSender<Task> {
    TX.get_or_init(|| {
        let (tx, rx) = mpsc::unbounded_channel();
        let level = match std::env::var("RUST_LOG").ok().map(|s| s.to_ascii_lowercase()).as_deref() {
            Some("trace") => LevelFilter::Trace,
            Some("debug") => LevelFilter::Debug,
            Some("warn") => LevelFilter::Warn,
            Some("error") => LevelFilter::Error,
            _ => LevelFilter::Info,
        };
        let logger = WriteLogger::new(level, simplelog::Config::default(), LogHandle(tx.clone()));
        let _ = log::set_boxed_logger(logger).map(|()| log::set_max_level(level));
        thread::spawn(move || {
            let rt = Builder::new_multi_thread()
                .worker_threads(2)
                .enable_all()
                .build()
                .expect("could not start the async runtime");
            rt.block_on(background_loop(write_dir, rx));
        });
        tx
    })
    .clone()
}

pub fn send(t: Task) {
    if let Some(tx) = TX.get() {
        let _ = tx.send(t);
    }
}
