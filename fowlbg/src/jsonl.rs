// Copyright 2026 Dillen Weerasinghe. MIT licensed: see fowlbg/LICENSE.
//! An append-only JSONL feed that bfdb tails.
//!
//! Each record goes out in a single `write_all` of the whole line: bfdb reads
//! while the engine writes, and `writeln!` on an unbuffered file is several
//! writes, so a reader could catch a record half written.

use crate::rotate;
use chrono::Utc;
use log::{error, info};
use serde::Serialize;
use std::{
    fs::{self, File, OpenOptions},
    io::{self, Write},
    path::{Path, PathBuf},
};

/// Open `p` for appending, creating it.
pub fn open_append(p: &Path) -> io::Result<File> {
    OpenOptions::new().create(true).append(true).open(p)
}

/// One feed. A feed that would not open is `None` and every write to it is
/// skipped; the caller says why once, when opening it.
pub struct JsonlSink {
    path: PathBuf,
    file: Option<File>,
}

impl JsonlSink {
    pub fn new(path: PathBuf, file: Option<File>) -> Self {
        Self { path, file }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn is_open(&self) -> bool {
        self.file.is_some()
    }

    /// Append one line (the newline is added). Ok if the feed is closed.
    pub fn append_line(&mut self, mut line: Vec<u8>) -> io::Result<()> {
        let Some(f) = &mut self.file else { return Ok(()) };
        line.push(b'\n');
        f.write_all(&line)
    }

    /// Append `v` as one JSON line.
    pub fn append_json<T: Serialize>(&mut self, v: &T) -> io::Result<()> {
        if self.file.is_none() {
            return Ok(());
        }
        self.append_line(serde_json::to_vec(v)?)
    }

    /// Append `{"ts": <now, RFC 3339>, <key>: v}`, the stats feed's framing.
    pub fn append_stamped<T: Serialize>(&mut self, key: &str, v: &T) -> io::Result<()> {
        if self.file.is_none() {
            return Ok(());
        }
        let mut rec = serde_json::Map::new();
        rec.insert("ts".into(), Utc::now().to_rfc3339().into());
        rec.insert(key.into(), serde_json::to_value(v)?);
        self.append_line(serde_json::to_vec(&serde_json::Value::Object(rec))?)
    }

    pub fn flush(&mut self) -> io::Result<()> {
        match &mut self.file {
            Some(f) => f.flush(),
            None => Ok(()),
        }
    }

    /// Rotate the feed (see [`rotate::rotate`]) once it is over `max` bytes.
    /// The handle is closed first, since Windows won't rename a file this
    /// process holds open, and the feed is reopened either way. `Err` is the
    /// reopen failing: the feed is closed until the next rotation.
    pub fn rotate_if_over(&mut self, max: u64) -> io::Result<()> {
        let len = match fs::metadata(&self.path) {
            Ok(m) => m.len(),
            Err(_) => return Ok(()),
        };
        if len <= max {
            return Ok(());
        }
        self.file = None;
        match rotate::rotate(&self.path) {
            Ok(Some(to)) => info!("rotated {:?} ({len} bytes) to {to:?}", self.path),
            Ok(None) => (),
            Err(e) => error!("could not rotate {:?}: {e:?}", self.path),
        }
        self.file = Some(open_append(&self.path)?);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir()
            .join(format!("fowlbg-jsonl-{name}-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn framing() {
        let dir = scratch("framing");
        let p = dir.join("stats.jsonl");
        let mut s = JsonlSink::new(p.clone(), Some(open_append(&p).unwrap()));
        s.append_stamped("stat", &serde_json::json!({"Connect": {"id": "u"}})).unwrap();
        s.append_json(&serde_json::json!({"id": 1})).unwrap();
        let txt = fs::read_to_string(&p).unwrap();
        let lines: Vec<&str> = txt.lines().collect();
        assert_eq!(lines.len(), 2, "{txt}");
        assert!(txt.ends_with('\n'));
        // the same object serde_json::json!({"ts": .., "stat": ..}) makes
        let v: serde_json::Value = serde_json::from_str(lines[0]).unwrap();
        let expect = serde_json::json!({"ts": v["ts"], "stat": {"Connect": {"id": "u"}}});
        assert_eq!(lines[0], serde_json::to_string(&expect).unwrap());
        assert_eq!(lines[1], r#"{"id":1}"#);
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn closed_feed_skips() {
        let mut s = JsonlSink::new(PathBuf::from("Z:/nowhere/x.jsonl"), None);
        assert!(!s.is_open());
        s.append_json(&1).unwrap();
        s.append_stamped("stat", &1).unwrap();
    }

    #[test]
    fn rotates_once_over_the_limit() {
        let dir = scratch("rotate");
        let p = dir.join("range.jsonl");
        let mut s = JsonlSink::new(p.clone(), Some(open_append(&p).unwrap()));
        s.append_line(b"0123456789".to_vec()).unwrap();
        s.rotate_if_over(11).unwrap();
        assert_eq!(fs::read_dir(&dir).unwrap().count(), 1, "11 bytes is not over 11");
        s.rotate_if_over(10).unwrap();
        let names: Vec<_> =
            fs::read_dir(&dir).unwrap().map(|e| e.unwrap().file_name()).collect();
        assert_eq!(names.len(), 2, "{names:?}");
        assert_eq!(fs::metadata(&p).unwrap().len(), 0, "a fresh feed");
        s.append_line(b"x".to_vec()).unwrap();
        assert_eq!(fs::read(&p).unwrap(), b"x\n");
        let _ = fs::remove_dir_all(&dir);
    }
}
