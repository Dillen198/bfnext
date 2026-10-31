// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! The background thread: logging, the two JSONL feeds bfdb tails, and the
//! netidx RPCs bfdb calls.
//!
//! DCS's Lua runs on one thread and must never block, so everything that
//! touches a file or the network happens here, fed by an unbounded channel.
//! RPC calls come the other way through a lock-free queue the engine drains
//! on its fast tick. The thread, the logger, the feeds and the publisher
//! setup are fowlbg's; what is here is the range's own tasks and RPCs.

use anyhow::Result;
use arcstr::ArcStr;
use bfprotocols::{range::RangeRecord, stats::Stat};
use bytes::Bytes;
use crossbeam::queue::SegQueue;
use fowlbg::{
    jsonl::{JsonlSink, open_append},
    netidx::{BIND_BACKOFF, Tls, load_config, start_publisher},
};
use futures::{StreamExt, channel::mpsc as fmpsc};
use log::{error, info};
use netidx::{
    chars::Chars,
    path::Path as NetidxPath,
    publisher::{BindCfg, Publisher, Value},
};
use netidx_protocols::{
    define_rpc,
    rpc::server::{ArgSpec, Proc, RpcCall},
    rpc_err,
};
use std::{
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    sync::Arc,
};
use tokio::{
    sync::{mpsc::UnboundedSender, oneshot},
    task,
};

/// A request from bfdb, answered by the engine on the DCS thread.
#[derive(Debug)]
pub enum Cmd {
    QueryRange,
    QueryAtc,
    /// A controller's command: "start" / "hold" / "resume" an AI flight (target =
    /// group), or "text": show `text` to the player in unit `target`.
    Atc {
        action: String,
        target: String,
        text: String,
    },
    QueryCatalog,
    QueryWeapons,
    Spawn(bfprotocols::range::SpawnRequest),
    Despawn {
        ucid: String,
        spawn_id: String,
        instructor: bool,
    },
    ResetStation {
        ucid: String,
        station: String,
        instructor: bool,
    },
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
    /// Mutual TLS to the resolver and bfdb (`netidx_tls`); `None` is the
    /// client config as written, normally Anonymous.
    pub tls: Option<Tls>,
}

#[derive(Debug)]
pub enum Task {
    WriteLog(Bytes),
    Stat(Stat),
    Record(Box<RangeRecord>),
    Start {
        sortie: String,
        net: NetCfg,
        q: CmdQueue,
    },
    /// Rotate the JSONL feeds that are bigger than these many bytes.
    RotateJsonl {
        range_max: Option<u64>,
        stats_max: Option<u64>,
    },
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
}

fn open(p: &Path) -> Option<File> {
    match open_append(p) {
        Ok(f) => Some(f),
        Err(e) => {
            eprintln!("bfrange: could not open {p:?}: {e:?}");
            None
        }
    }
}

fn rotate_jsonl(sink: &mut JsonlSink, max: u64) {
    if let Err(e) = sink.rotate_if_over(max) {
        eprintln!("bfrange: could not open {:?}: {e:?}", sink.path());
    }
}

struct Sinks {
    log: Option<File>,
    stats: JsonlSink,
    range: JsonlSink,
}

impl Sinks {
    fn new(write_dir: &Path) -> Self {
        let logs = write_dir.join("Logs");
        let _ = fs::create_dir_all(&logs);
        let log_path = logs.join("bfrange.txt");
        let _ = fowlbg::rotate::rotate(&log_path);
        let stats_path = logs.join("stats.jsonl");
        let range_path = logs.join("range.jsonl");
        Self {
            log: open(&log_path),
            stats: JsonlSink::new(stats_path.clone(), open(&stats_path)),
            range: JsonlSink::new(range_path.clone(), open(&range_path)),
        }
    }

    fn rotate_jsonl(&mut self, range_max: Option<u64>, stats_max: Option<u64>) {
        if let Some(max) = range_max {
            rotate_jsonl(&mut self.range, max);
        }
        if let Some(max) = stats_max {
            rotate_jsonl(&mut self.stats, max);
        }
    }

    fn stat(&mut self, st: &Stat) {
        if let Err(e) = self.stats.append_stamped("stat", st) {
            error!("could not write stat: {e:?}")
        }
    }

    fn record(&mut self, r: &RangeRecord) {
        if !self.range.is_open() {
            return;
        }
        match serde_json::to_vec(r) {
            Ok(s) => {
                if let Err(e) = self.range.append_line(s) {
                    error!("could not write range record: {e:?}")
                }
                let _ = self.range.flush();
            }
            Err(e) => error!("could not encode range record {}: {e:?}", r.id),
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
    query_atc: Proc,
    atc_cmd: Proc,
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
        let query_atc = define_rpc!(
            publisher,
            base.append("query-atc"),
            "Radar picture for the spoken ATC: airfields, runways, weather and player traffic (returns JSON AtcPicture)",
            |c: RpcCall, side: Chars| {
                let _ = side;
                let (tx, rx) = oneshot::channel();
                _q.push((Cmd::QueryAtc, tx));
                Some((c, rx))
            },
            Some(wait.clone()),
            side: Chars = Value::Null; "Accepted for compatibility with the campaign's query-atc; the range returns every side"
        )?;
        let _q = Arc::clone(q);
        let atc_cmd = define_rpc!(
            publisher,
            base.append("range-atc"),
            "The spoken ATC's commands: start/hold/resume an AI flight, or show a player a transmission's text",
            |c: RpcCall, action: Chars, target: Chars, text: Chars| {
                let (tx, rx) = oneshot::channel();
                _q.push((
                    Cmd::Atc { action: action.to_string(), target: target.to_string(), text: text.to_string() },
                    tx,
                ));
                Some((c, rx))
            },
            Some(wait.clone()),
            action: Chars = Value::Null; "start | hold | resume | text",
            target: Chars = Value::Null; "AI group name, or a player's unit id for text",
            text: Chars = Value::from(""); "The text to show (action text)"
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
        Ok(Self {
            query_range,
            query_atc,
            atc_cmd,
            query_catalog,
            query_weapons,
            spawn,
            despawn,
            reset_station,
        })
    }
}

struct Background {
    sinks: Sinks,
    rpcs: Option<Rpcs>,
    publisher: Option<Publisher>,
}

impl Background {
    async fn start(&mut self, sortie: String, net: NetCfg, q: CmdQueue) {
        let Some(base) = net.base else {
            info!("no netidx_base in the range config: RPCs disabled, JSONL only");
            return;
        };
        let base = base.append(&sortie);
        // a mission restart starts again: retire the old RPCs first so
        // the resolver never lists two publishers for one path
        self.rpcs = None;
        self.publisher = None;
        let cfg =
            match load_config(net.config.as_deref().map(Path::new), net.tls.as_ref()) {
                Ok(c) => c,
                Err(e) => {
                    let from = net.config.as_deref().unwrap_or("the default location");
                    error!("failed to load the netidx config from {from}: {e:?}");
                    return;
                }
            };
        if let Some(tls) = &net.tls {
            info!("netidx: TLS to resolver {} as {}", tls.resolver_name, tls.certificate);
        }
        let bind = match net.bind.as_deref().map(str::parse::<BindCfg>).transpose() {
            Ok(b) => b,
            Err(e) => {
                error!("netidx_bind in the range config is not a bind address: {e:?}");
                return;
            }
        };
        let publisher = match start_publisher(&cfg, bind, BIND_BACKOFF).await {
            Ok(p) => p,
            Err(e) => {
                error!("failed to start the netidx publisher: {e:?}");
                return;
            }
        };
        info!("netidx: publishing on {}", publisher.addr());
        info!(
            "netidx: range RPCs under {base}/api (bfdb's instance base must be the parent)"
        );
        match Rpcs::new(&publisher, &q, &base) {
            Ok(r) => self.rpcs = Some(r),
            Err(e) => error!("failed to publish range RPCs: {e:?}"),
        }
        self.publisher = Some(publisher);
    }
}

impl fowlbg::Handler for Background {
    type Task = Task;

    async fn handle(&mut self, t: Task) {
        match t {
            Task::WriteLog(line) => {
                if let Some(f) = &mut self.sinks.log {
                    let _ = f.write_all(&line);
                }
            }
            Task::Stat(st) => self.sinks.stat(&st),
            Task::Record(r) => self.sinks.record(&r),
            Task::RotateJsonl { range_max, stats_max } => {
                self.sinks.rotate_jsonl(range_max, stats_max)
            }
            Task::Start { sortie, net, q } => self.start(sortie, net, q).await,
        }
    }
}

pub fn init(write_dir: PathBuf) -> UnboundedSender<Task> {
    // Unsupervised, no fallback log and no netidx filter: the range as it
    // was before fowlbg. Each is one option away.
    let opts =
        fowlbg::Options { worker_threads: Some(2), ..fowlbg::Options::new("bfrange") };
    fowlbg::init(opts, move |_restart| {
        let write_dir = write_dir.clone();
        async move {
            Background { sinks: Sinks::new(&write_dir), rpcs: None, publisher: None }
        }
    })
}

pub fn send(t: Task) {
    if let Some(tx) = fowlbg::sender::<Task>() {
        let _ = tx.send(t);
    }
}
