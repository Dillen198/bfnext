// Copyright 2026 Dillen Weerasinghe. MIT licensed: see fowlbg/LICENSE.
//! The background thread.
//!
//! DCS runs a mission's Lua on one thread that must never block, so a DLL
//! hands anything touching a file or the network to this thread over an
//! unbounded channel. The DLL owns its task type and what each task does
//! ([`Handler`]); this owns the thread, the tokio runtime on it, the logger
//! that feeds it, and (opt-in) keeping it alive through panics.

use crate::log::{LogHandle, Quiet, QUEUED, fallback_log, level_from_env, panic_msg, set_fallback};
use bytes::Bytes;
use futures::FutureExt;
use log::error;
use once_cell::sync::OnceCell;
use simplelog::WriteLogger;
use std::{
    any::Any,
    future::Future,
    marker::PhantomData,
    panic::{AssertUnwindSafe, catch_unwind, resume_unwind},
    path::PathBuf,
    sync::atomic::{AtomicI64, Ordering},
    thread,
    time::{Duration, SystemTime, UNIX_EPOCH},
};
use tokio::{
    runtime::Builder,
    sync::mpsc::{self, UnboundedReceiver, UnboundedSender},
};

/// A DLL's background task type. One variant carries a log line.
pub trait Task: Send + 'static {
    /// Wrap one complete log line (newline included).
    fn log_line(line: Bytes) -> Self;
    /// The line back out of a task made by [`Task::log_line`], `None` for
    /// any other task.
    fn into_log_line(self) -> Option<Bytes>;
    /// Whether this is a log line.
    fn is_log_line(&self) -> bool;
    /// A short name for the task in panic reports.
    fn kind(&self) -> &'static str {
        "task"
    }
    /// The last task: the loop ends after it (panicked or not) and is not
    /// restarted.
    fn is_final(&self) -> bool {
        false
    }
}

/// What the background loop does with each task, made fresh by the factory
/// passed to [`init`] each time the loop (re)starts.
pub trait Handler {
    type Task: Task;
    /// One task. With [`Options::supervise`] a panic in here is caught and
    /// reported, and the loop carries on with the next task.
    fn handle(&mut self, task: Self::Task) -> impl Future<Output = ()>;
    /// After the last task: the channel closed or a final task was handled.
    fn stop(&mut self) -> impl Future<Output = ()> {
        async {}
    }
}

/// How [`init`] sets the thread up.
#[derive(Debug, Clone)]
pub struct Options {
    /// The DLL's name, at the front of messages that go to the fallback log.
    pub name: &'static str,
    /// Where lines go when the background thread can't take them: the
    /// logger's lines once the thread is gone, and the supervisor's reports.
    /// `None`: those lines are dropped (and the logger is told the write
    /// failed).
    pub fallback_log: Option<PathBuf>,
    /// Put the netidx noise filter ([`crate::log::netidx_noise`]) in front
    /// of the logger.
    pub quiet_netidx: bool,
    /// Drop log lines, counting them, while this many are still queued; the
    /// next line that gets through says how many went. `None`: no cap.
    pub max_queued_log_lines: Option<usize>,
    /// Worker threads for the runtime; `None` is tokio's default (one per
    /// core).
    pub worker_threads: Option<usize>,
    /// Catch a panic in a task (the loop goes on), and restart the loop on a
    /// fresh runtime if something else panics. Off, a panic ends the thread.
    pub supervise: bool,
}

impl Options {
    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            fallback_log: None,
            quiet_netidx: false,
            max_queued_log_lines: None,
            worker_threads: None,
            supervise: false,
        }
    }
}

/// Unix seconds when the loop last finished a task; 0 before the first.
static HEARTBEAT: AtomicI64 = AtomicI64::new(0);

fn unix_now() -> i64 {
    SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_secs() as i64).unwrap_or(0)
}

/// Seconds between `now` (Unix seconds) and the last task the loop
/// finished; `None` before the first. A value that keeps growing while tasks
/// are being sent means the loop is stuck.
pub fn heartbeat_age(now: i64) -> Option<i64> {
    match HEARTBEAT.load(Ordering::Relaxed) {
        0 => None,
        ts => Some(now.saturating_sub(ts)),
    }
}

static TX: OnceCell<Box<dyn Any + Send + Sync>> = OnceCell::new();

/// The sender [`init`] returned, if it has run with this task type.
pub fn sender<T: Task>() -> Option<&'static UnboundedSender<T>> {
    TX.get().and_then(|tx| tx.downcast_ref())
}

/// Start the background thread and install the logger, once per process.
///
/// `make` runs on the background thread, inside its runtime, and returns
/// the handler; it runs again (`restart` counting up from 0) each time the
/// supervisor restarts the loop. A second call returns the first call's
/// sender and does nothing else: `log` takes one logger per process, and
/// DCS keeps the DLL loaded across missions.
///
/// Panics if a second call passes a different task type.
pub fn init<H, F, Fut>(opts: Options, make: F) -> UnboundedSender<H::Task>
where
    H: Handler + 'static,
    F: FnMut(u32) -> Fut + Send + 'static,
    Fut: Future<Output = H>,
{
    let tx = TX.get_or_init(|| {
        let (tx, rx) = mpsc::unbounded_channel::<H::Task>();
        if let Some(p) = &opts.fallback_log {
            set_fallback(p.clone());
        }
        install_logger(&opts, tx.clone());
        thread::spawn(move || run_thread(opts, make, rx));
        Box::new(tx)
    });
    tx.downcast_ref::<UnboundedSender<H::Task>>()
        .expect("fowlbg::init called again with a different task type")
        .clone()
}

fn install_logger<T: Task>(opts: &Options, tx: UnboundedSender<T>) {
    let level = level_from_env();
    let handle = LogHandle {
        tx,
        name: opts.name,
        max_queued: opts.max_queued_log_lines,
        fallback: opts.fallback_log.is_some(),
        _t: PhantomData,
    };
    let logger = WriteLogger::new(level, simplelog::Config::default(), handle);
    let installed = if opts.quiet_netidx {
        log::set_boxed_logger(Box::new(Quiet(*logger)))
    } else {
        log::set_boxed_logger(logger)
    };
    // something else in the process already has the logger: leave it be
    if installed.is_ok() {
        log::set_max_level(level);
    }
}

fn run_thread<H, F, Fut>(opts: Options, mut make: F, mut rx: UnboundedReceiver<H::Task>)
where
    H: Handler,
    F: FnMut(u32) -> Fut,
    Fut: Future<Output = H>,
{
    let mut restarts: u32 = 0;
    loop {
        let mut b = Builder::new_multi_thread();
        if let Some(n) = opts.worker_threads {
            b.worker_threads(n);
        }
        let rt = match b.enable_all().build() {
            Ok(rt) => rt,
            Err(e) if opts.supervise => {
                fallback_log(format!("{}: could not start the async runtime {e:?}\n", opts.name).as_bytes());
                thread::sleep(Duration::from_secs(5));
                continue;
            }
            Err(e) => panic!("could not start the async runtime: {e:?}"),
        };
        let res = catch_unwind(AssertUnwindSafe(|| {
            rt.block_on(run_loop(&opts, &mut make, restarts, &mut rx))
        }));
        match res {
            Ok(()) => break,
            Err(e) if opts.supervise => {
                restarts = restarts.saturating_add(1);
                let m = panic_msg(&e);
                fallback_log(
                    format!("{}: background loop died ({m}), restart #{restarts}\n", opts.name).as_bytes(),
                );
                drop(rt);
                thread::sleep(Duration::from_secs(1));
            }
            Err(e) => resume_unwind(e),
        }
    }
    if opts.supervise {
        println!("background thread exiting")
    }
}

async fn run_loop<H, F, Fut>(
    opts: &Options,
    make: &mut F,
    restart: u32,
    rx: &mut UnboundedReceiver<H::Task>,
) where
    H: Handler,
    F: FnMut(u32) -> Fut,
    Fut: Future<Output = H>,
{
    let mut h = make(restart).await;
    while let Some(task) = rx.recv().await {
        let last = task.is_final();
        let kind = task.kind();
        if task.is_log_line() {
            QUEUED.fetch_sub(1, Ordering::Relaxed);
        }
        if opts.supervise {
            if let Err(e) = AssertUnwindSafe(h.handle(task)).catch_unwind().await {
                let m = panic_msg(&e);
                fallback_log(format!("{}: background task {kind} panicked: {m}\n", opts.name).as_bytes());
                error!("background task {kind} panicked: {m}")
            }
        } else {
            h.handle(task).await
        }
        HEARTBEAT.store(unix_now(), Ordering::Relaxed);
        if last {
            break;
        }
    }
    h.stop().await
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Arc, Mutex};

    #[derive(Debug)]
    enum T {
        Log(Bytes),
        Do(&'static str),
        Boom,
        Last,
    }

    impl Task for T {
        fn log_line(line: Bytes) -> Self {
            T::Log(line)
        }
        fn into_log_line(self) -> Option<Bytes> {
            match self {
                T::Log(l) => Some(l),
                _ => None,
            }
        }
        fn is_log_line(&self) -> bool {
            matches!(self, T::Log(_))
        }
        fn kind(&self) -> &'static str {
            match self {
                T::Boom => "Boom",
                _ => "other",
            }
        }
        fn is_final(&self) -> bool {
            matches!(self, T::Last)
        }
    }

    type Seen = Arc<Mutex<Vec<String>>>;

    struct H(u32, Seen);

    impl Handler for H {
        type Task = T;
        async fn handle(&mut self, t: T) {
            match t {
                T::Log(l) => self.1.lock().unwrap().push(String::from_utf8_lossy(&l).into()),
                T::Do(s) => self.1.lock().unwrap().push(format!("{}:{s}", self.0)),
                T::Boom => panic!("boom"),
                T::Last => self.1.lock().unwrap().push("last".into()),
            }
        }
        async fn stop(&mut self) {
            self.1.lock().unwrap().push("stop".into())
        }
    }

    fn run(opts: Options, tasks: Vec<T>, panic_in_make_once: bool) -> Vec<String> {
        let seen: Seen = Arc::default();
        let (tx, rx) = mpsc::unbounded_channel();
        for t in tasks {
            tx.send(t).unwrap();
        }
        drop(tx);
        let s = seen.clone();
        let mut made = 0;
        let make = move |restart: u32| {
            made += 1;
            let s = s.clone();
            let boom = panic_in_make_once && made == 1;
            async move {
                if boom {
                    panic!("setup failed");
                }
                s.lock().unwrap().push(format!("start {restart}"));
                H(restart, s)
            }
        };
        let r = catch_unwind(AssertUnwindSafe(|| run_thread(opts, make, rx)));
        let mut out = seen.lock().unwrap().clone();
        if r.is_err() {
            out.push("thread panicked".into());
        }
        out
    }

    fn supervised() -> Options {
        Options { supervise: true, worker_threads: Some(1), ..Options::new("test") }
    }

    #[test]
    fn supervised_task_panic_is_contained() {
        let out = run(supervised(), vec![T::Do("a"), T::Boom, T::Do("b")], false);
        assert_eq!(out, ["start 0", "0:a", "0:b", "stop"]);
    }

    #[test]
    fn final_task_ends_the_loop() {
        let out = run(supervised(), vec![T::Do("a"), T::Last, T::Do("never")], false);
        assert_eq!(out, ["start 0", "0:a", "last", "stop"]);
    }

    #[test]
    fn supervisor_restarts_a_dead_loop() {
        let out = run(supervised(), vec![T::Do("a")], true);
        assert_eq!(out, ["start 1", "1:a", "stop"]);
    }

    #[test]
    fn unsupervised_panic_ends_the_thread() {
        let opts = Options { worker_threads: Some(1), ..Options::new("test") };
        let out = run(opts, vec![T::Do("a"), T::Boom, T::Do("b")], false);
        assert_eq!(out, ["start 0", "0:a", "thread panicked"]);
    }

    #[test]
    fn log_lines_reach_the_handler() {
        let out = run(supervised(), vec![T::log_line(Bytes::from_static(b"hello\n"))], false);
        assert_eq!(out, ["start 0", "hello\n", "stop"]);
    }
}
