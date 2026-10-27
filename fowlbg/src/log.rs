// Copyright 2026 Dillen Weerasinghe. MIT licensed: see fowlbg/LICENSE.
//! The process logger: `log` records become lines handed to the background
//! thread, which writes them wherever the DLL wants.

use crate::runtime::Task;
use bytes::{Bytes, BytesMut};
use log::{Level, LevelFilter, Log, Metadata, Record};
use once_cell::sync::OnceCell;
use std::{
    cell::RefCell,
    fs, io,
    io::Write,
    marker::PhantomData,
    path::PathBuf,
    sync::atomic::{AtomicUsize, Ordering},
};
use tokio::sync::mpsc::UnboundedSender;

/// The level `RUST_LOG` asks for: one of trace, debug, info, warn, error or
/// off, any case. Anything else, or unset, is info.
pub fn level_from_env() -> LevelFilter {
    level_from(std::env::var("RUST_LOG").ok().as_deref())
}

fn level_from(v: Option<&str>) -> LevelFilter {
    match v.map(str::to_ascii_lowercase).as_deref() {
        Some("trace") => LevelFilter::Trace,
        Some("debug") => LevelFilter::Debug,
        Some("warn") => LevelFilter::Warn,
        Some("error") => LevelFilter::Error,
        Some("off") => LevelFilter::Off,
        _ => LevelFilter::Info,
    }
}

/// Whether the netidx noise filter drops a record with this metadata.
///
/// netidx logs a lot that is routine for an engine: the subscriber retrying
/// a path that is not there (netidx-archive's single-member cluster does
/// this forever, a WARN each time), and at INFO the resolver connection's
/// heartbeat reconnects and the archive's per-minute file rotation. This
/// keeps the subscriber's errors and everyone else's warnings.
pub fn netidx_noise(m: &Metadata) -> bool {
    let t = m.target();
    let level = m.level();
    (level > Level::Error && t.starts_with("netidx::subscriber"))
        || (level > Level::Warn
            && ["netidx::resolver_client", "netidx::channel", "netidx_archive::logfile_collection"]
                .iter()
                .any(|p| t.starts_with(p)))
}

/// A logger with the netidx noise filter in front of it.
pub struct Quiet<L>(pub L);

impl<L: Log> Log for Quiet<L> {
    fn enabled(&self, m: &Metadata) -> bool {
        !netidx_noise(m) && self.0.enabled(m)
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

static FALLBACK: OnceCell<PathBuf> = OnceCell::new();

/// Set the fallback log file (first call wins).
pub(crate) fn set_fallback(path: PathBuf) {
    let _ = FALLBACK.set(path);
}

/// Write `buf` to stderr and append it to the fallback log file, from
/// whatever thread is calling. For when the background thread cannot take
/// it: the thread is gone, or this is the message saying why. Opens the file
/// on every call, which is fine for a path that only runs when the normal
/// one is broken. Does nothing to a file if none was configured.
pub fn fallback_log(buf: &[u8]) {
    let _ = io::stderr().write_all(buf);
    if let Some(path) = FALLBACK.get() {
        if let Ok(mut f) = fs::OpenOptions::new().create(true).append(true).open(path) {
            let _ = f.write_all(buf);
        }
    }
}

/// The text of a panic payload: `panic!` with a literal carries a
/// `&'static str`, with formatting a `String`.
pub fn panic_payload_msg(p: &(dyn std::any::Any + Send)) -> &str {
    if let Some(s) = p.downcast_ref::<&'static str>() {
        s
    } else if let Some(s) = p.downcast_ref::<String>() {
        s.as_str()
    } else {
        "<panic payload was not a string>"
    }
}

/// [`panic_payload_msg`] for what `catch_unwind` returns.
pub fn panic_msg(e: &Box<dyn std::any::Any + Send>) -> &str {
    // deref twice: a `&Box<dyn Any>` would itself coerce to `&dyn Any`, and
    // then every downcast would be looking at the Box
    panic_payload_msg(e.as_ref())
}

/// Log lines sent to the background thread and not yet taken off the queue.
pub(crate) static QUEUED: AtomicUsize = AtomicUsize::new(0);
/// Lines dropped because the queue was at its cap, not yet reported.
static DROPPED: AtomicUsize = AtomicUsize::new(0);

thread_local! {
    /// The line being written on this thread. A record arrives in several
    /// writes; only a complete line goes to the background thread.
    static LINE: RefCell<BytesMut> = RefCell::new(BytesMut::new());
}

pub(crate) struct LogHandle<T> {
    pub(crate) tx: UnboundedSender<T>,
    pub(crate) name: &'static str,
    pub(crate) max_queued: Option<usize>,
    pub(crate) fallback: bool,
    pub(crate) _t: PhantomData<fn(T)>,
}

impl<T: Task> LogHandle<T> {
    fn send_line(&self, mut line: Bytes) -> io::Result<()> {
        if let Some(max) = self.max_queued {
            if QUEUED.load(Ordering::Relaxed) >= max {
                DROPPED.fetch_add(1, Ordering::Relaxed);
                return Ok(());
            }
            let dropped = DROPPED.swap(0, Ordering::Relaxed);
            if dropped > 0 {
                let mut b = BytesMut::from(
                    format!("WARN [{}] log queue full: {dropped} log line(s) dropped\n", self.name).as_bytes(),
                );
                b.extend_from_slice(&line);
                line = b.freeze();
            }
        }
        QUEUED.fetch_add(1, Ordering::Relaxed);
        match self.tx.send(T::log_line(line)) {
            Ok(()) => Ok(()),
            Err(e) => {
                QUEUED.fetch_sub(1, Ordering::Relaxed);
                if !self.fallback {
                    return Err(io::Error::other("background thread gone"));
                }
                // An error here would only make the logger drop the line;
                // put it where it can still be read.
                if let Some(line) = e.0.into_log_line() {
                    fallback_log(&line)
                }
                Ok(())
            }
        }
    }
}

impl<T: Task> io::Write for LogHandle<T> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let line = LINE.with_borrow_mut(|l| {
            l.extend_from_slice(buf);
            (l.last() == Some(&b'\n')).then(|| l.split().freeze())
        });
        if let Some(line) = line {
            self.send_line(line)?;
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levels() {
        assert_eq!(level_from(None), LevelFilter::Info);
        assert_eq!(level_from(Some("DEBUG")), LevelFilter::Debug);
        assert_eq!(level_from(Some("trace")), LevelFilter::Trace);
        assert_eq!(level_from(Some("Warn")), LevelFilter::Warn);
        assert_eq!(level_from(Some("error")), LevelFilter::Error);
        assert_eq!(level_from(Some("off")), LevelFilter::Off);
        assert_eq!(level_from(Some("info")), LevelFilter::Info);
        assert_eq!(level_from(Some("bflib=debug")), LevelFilter::Info);
    }

    fn meta(level: Level, target: &str) -> Metadata<'_> {
        Metadata::builder().level(level).target(target).build()
    }

    #[test]
    fn noise_filter() {
        assert!(netidx_noise(&meta(Level::Warn, "netidx::subscriber::connection")));
        assert!(!netidx_noise(&meta(Level::Error, "netidx::subscriber")));
        assert!(netidx_noise(&meta(Level::Info, "netidx::resolver_client::write_client")));
        assert!(!netidx_noise(&meta(Level::Warn, "netidx::resolver_client")));
        assert!(netidx_noise(&meta(Level::Info, "netidx::channel")));
        assert!(netidx_noise(&meta(Level::Debug, "netidx_archive::logfile_collection")));
        assert!(!netidx_noise(&meta(Level::Info, "netidx_archive::recorder")));
        assert!(!netidx_noise(&meta(Level::Debug, "bflib::db")));
    }

    #[test]
    fn panic_messages() {
        let e = std::panic::catch_unwind(|| panic!("plain")).unwrap_err();
        assert_eq!(panic_msg(&e), "plain");
        let n = 3;
        let e = std::panic::catch_unwind(move || panic!("formatted {n}")).unwrap_err();
        assert_eq!(panic_msg(&e), "formatted 3");
        let e = std::panic::catch_unwind(|| std::panic::panic_any(7u8)).unwrap_err();
        assert_eq!(panic_msg(&e), "<panic payload was not a string>");
    }
}
