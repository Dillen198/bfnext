// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Tail a range instance's `range.jsonl` into the store.
//!
//! Same robustness rules as the campaign `jsonl_loop` in db.rs, plus two it
//! learned the hard way:
//!   * a missing file is reported (once, then every 15 minutes), not fatal;
//!   * a file shorter than the cursor is a new file -- re-read from the top;
//!   * a file whose first bytes changed is ALSO a new file, even when it has
//!     already grown past the old cursor between two polls (the length test
//!     alone misses that and silently skips the start of the new file);
//!   * only complete lines are consumed: the engine may be mid-append, so a
//!     trailing fragment waits for its newline. One that stays unterminated
//!     and unchanged for 30 s is taken as-is if it parses (an engine that died
//!     after writing a whole record but before the newline);
//!   * unparsable lines are counted and reported once per pass with the first
//!     reason, and the cursor moves past them.
//!
//! Records are deduplicated by id in the store, so re-reading a file (after a
//! rotation, a restored copy or a lost cursor) never double-counts.

use super::{store::Insert, RangeCtx};
use log::{error, info, warn};
use std::{
    io::{Read, Seek, SeekFrom},
    path::{Path, PathBuf},
    sync::Arc,
    time::{Duration, Instant},
};

const POLL: Duration = Duration::from_secs(2);
/// Bytes read per pass; a backlog is drained in several passes back to back.
const CHUNK: usize = 8 * 1024 * 1024;
/// Bytes of the file's head that identify it.
const HEAD: usize = 512;
const MISSING_WARN_EVERY: Duration = Duration::from_secs(900);
/// How long an unterminated tail must sit unchanged before it is parsed.
const STALE_TAIL: Duration = Duration::from_secs(30);

/// FNV-1a, 64 bit: a stable fingerprint of the file's head (std's hasher is
/// not guaranteed stable across Rust releases, and this one is persisted).
fn fnv1a(b: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf29ce484222325;
    for x in b {
        h ^= *x as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

/// What one pass over the file found.
enum Pass {
    Missing(String),
    Idle,
    /// Consumed bytes up to `pos`; `full` = the chunk limit was hit, so more
    /// is waiting.
    Read { pos: u64, full: bool, lines: Vec<Vec<u8>> },
    /// Not the file the cursor belongs to any more.
    Replaced(&'static str),
    /// Only an unterminated fragment past the cursor.
    Partial { len: u64 },
}

fn read_pass(path: &Path, pos: u64, known_head: Option<u64>) -> std::io::Result<(Pass, Option<u64>)> {
    let mut f = match std::fs::File::open(path) {
        Ok(f) => f,
        Err(e) => return Ok((Pass::Missing(e.to_string()), None)),
    };
    let len = f.metadata()?.len();
    // Fingerprint once the head is complete; an append-only file never
    // changes those bytes again.
    let head = if len >= HEAD as u64 {
        let mut b = vec![0u8; HEAD];
        f.read_exact(&mut b)?;
        Some(fnv1a(&b))
    } else {
        None
    };
    if len < pos {
        return Ok((Pass::Replaced("shrank below the cursor"), head));
    }
    if let (Some(k), Some(h)) = (known_head, head) {
        if k != h && pos > 0 {
            return Ok((Pass::Replaced("its first bytes changed"), head));
        }
    }
    if len == pos {
        return Ok((Pass::Idle, head));
    }
    f.seek(SeekFrom::Start(pos))?;
    let want = ((len - pos) as usize).min(CHUNK);
    let mut buf = vec![0u8; want];
    f.read_exact(&mut buf)?;
    let Some(last_nl) = buf.iter().rposition(|b| *b == b'\n') else {
        if want == CHUNK {
            // A single "line" longer than the chunk: not something the engine
            // writes. Skip it rather than stall behind it forever.
            return Ok((
                Pass::Read { pos: pos + want as u64, full: true, lines: vec![buf] },
                head,
            ));
        }
        return Ok((Pass::Partial { len }, head));
    };
    let lines = buf[..last_nl]
        .split(|b| *b == b'\n')
        .map(|l| l.to_vec())
        .collect::<Vec<_>>();
    let consumed = last_nl as u64 + 1;
    Ok((Pass::Read { pos: pos + consumed, full: want == CHUNK, lines }, head))
}

/// Read the unterminated tail from `pos` to EOF.
fn read_tail(path: &Path, pos: u64) -> std::io::Result<Vec<u8>> {
    let mut f = std::fs::File::open(path)?;
    f.seek(SeekFrom::Start(pos))?;
    let mut b = Vec::new();
    f.take(CHUNK as u64).read_to_end(&mut b)?;
    Ok(b)
}

#[derive(Default)]
struct Tally {
    inserted: u64,
    duplicate: u64,
    unparsable: u64,
    first_unparsable: Option<String>,
    rejected: u64,
    first_rejected: Option<String>,
    /// Stored, but not decodable as this build's `RangeRecord`.
    foreign: u64,
    first_foreign: Option<String>,
}

fn ingest_lines(ctx: &RangeCtx, inst: &str, lines: Vec<Vec<u8>>, t: &mut Tally) {
    for l in lines {
        let s = String::from_utf8_lossy(&l);
        let s = s.trim();
        if s.is_empty() {
            continue;
        }
        let v: serde_json::Value = match serde_json::from_str(s) {
            Ok(v) => v,
            Err(e) => {
                t.unparsable += 1;
                if t.first_unparsable.is_none() {
                    let preview: String = s.chars().take(160).collect();
                    t.first_unparsable = Some(format!("{e}, raw: {preview}"));
                }
                continue;
            }
        };
        let typed_err = serde_json::from_value::<bfprotocols::range::RangeRecord>(v.clone()).err();
        match ctx.store.insert(inst, v) {
            Ok(Insert::Inserted) => {
                t.inserted += 1;
                if let Some(e) = typed_err {
                    t.foreign += 1;
                    if t.first_foreign.is_none() {
                        t.first_foreign = Some(e.to_string());
                    }
                }
            }
            Ok(Insert::Duplicate) => t.duplicate += 1,
            Err(e) => {
                t.rejected += 1;
                if t.first_rejected.is_none() {
                    t.first_rejected = Some(format!("{e:#}"));
                }
            }
        }
    }
}

pub(crate) async fn ingest_loop(ctx: Arc<RangeCtx>, inst: String, path: PathBuf) {
    let head_key = format!("head\0{inst}");
    let mut pos = match ctx.store.cursor(&inst) {
        Ok(p) => p,
        Err(e) => {
            error!("[{inst}] range: cannot read the range.jsonl cursor ({e:?}); starting at 0");
            0
        }
    };
    let mut known_head: Option<u64> = ctx
        .store
        .meta_get(&head_key)
        .ok()
        .flatten()
        .filter(|v| v.len() == 8)
        .map(|v| u64::from_be_bytes(v[..8].try_into().unwrap()));
    info!("[{inst}] range: reading {} from offset {pos}", path.display());
    let mut last_missing_warn: Option<Instant> = None;
    // (file length, first seen) of an unterminated tail
    let mut partial: Option<(u64, Instant)> = None;
    let mut tick = tokio::time::interval(POLL);
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    loop {
        tick.tick().await;
        // Drain a backlog chunk by chunk without waiting between them.
        loop {
            let res = tokio::task::block_in_place(|| read_pass(&path, pos, known_head));
            let (pass, head) = match res {
                Ok(r) => r,
                Err(e) => {
                    error!("[{inst}] range: reading {} failed: {e}", path.display());
                    break;
                }
            };
            if let Some(h) = head {
                if known_head != Some(h) {
                    known_head = Some(h);
                    let _ = ctx.store.meta_set(&head_key, &h.to_be_bytes()[..]);
                }
            }
            match pass {
                Pass::Missing(e) => {
                    if last_missing_warn.map_or(true, |t| t.elapsed() >= MISSING_WARN_EVERY) {
                        last_missing_warn = Some(Instant::now());
                        warn!(
                            "[{inst}] range: cannot read {} ({e}) -- no range results are being \
                             ingested. The range engine writes Logs/range.jsonl in its own Saved \
                             Games folder; check `range_jsonl` / `stats_jsonl` in the instances \
                             file.",
                            path.display()
                        );
                    }
                    break;
                }
                Pass::Idle => {
                    partial = None;
                    break;
                }
                Pass::Replaced(why) => {
                    warn!(
                        "[{inst}] range: {} {why} -- it is a new file; re-reading it from the \
                         start (records already stored are skipped by id)",
                        path.display()
                    );
                    pos = 0;
                    partial = None;
                    if let Err(e) = ctx.store.set_cursor(&inst, 0) {
                        error!("[{inst}] range: failed to reset the cursor: {e:?}");
                    }
                    continue;
                }
                Pass::Partial { len } => {
                    match partial {
                        Some((l, since)) if l == len => {
                            if since.elapsed() >= STALE_TAIL {
                                let tail = tokio::task::block_in_place(|| read_tail(&path, pos));
                                if let Ok(tail) = tail {
                                    let ok = serde_json::from_slice::<serde_json::Value>(&tail).is_ok();
                                    if ok {
                                        let mut t = Tally::default();
                                        tokio::task::block_in_place(|| {
                                            ingest_lines(&ctx, &inst, vec![tail.clone()], &mut t)
                                        });
                                        pos += tail.len() as u64;
                                        let _ = ctx.store.set_cursor(&inst, pos);
                                        warn!(
                                            "[{inst}] range: took an unterminated last record \
                                             ({} bytes) after {}s without a newline",
                                            tail.len(),
                                            STALE_TAIL.as_secs()
                                        );
                                    }
                                }
                                partial = None;
                            }
                        }
                        _ => partial = Some((len, Instant::now())),
                    }
                    break;
                }
                Pass::Read { pos: new_pos, full, lines } => {
                    partial = None;
                    last_missing_warn = None;
                    let mut t = Tally::default();
                    tokio::task::block_in_place(|| ingest_lines(&ctx, &inst, lines, &mut t));
                    if let Err(e) = ctx.store.set_cursor(&inst, new_pos) {
                        error!("[{inst}] range: failed to persist the cursor: {e:?}");
                    }
                    if t.inserted > 0 || t.duplicate > 0 {
                        info!(
                            "[{inst}] range: {} new result(s), {} already stored (pos {pos} -> \
                             {new_pos})",
                            t.inserted, t.duplicate
                        );
                    }
                    if let Some(first) = t.first_unparsable {
                        error!(
                            "[{inst}] range: skipped {} unparsable line(s) in range.jsonl (torn \
                             write?); first: {first}",
                            t.unparsable
                        );
                    }
                    if let Some(first) = t.first_rejected {
                        error!(
                            "[{inst}] range: skipped {} line(s) that are not range records; \
                             first: {first}",
                            t.rejected
                        );
                    }
                    if let Some(first) = t.first_foreign {
                        warn!(
                            "[{inst}] range: stored {} record(s) this bfdb build cannot decode \
                             (a newer engine?) -- they are kept raw; first: {first}",
                            t.foreign
                        );
                    }
                    pos = new_pos;
                    if !full {
                        break;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn complete_lines_only_and_rotation() {
        let dir = std::env::temp_dir().join(format!("bfdb-range-ingest-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let p = dir.join("range.jsonl");
        let mut f = std::fs::File::create(&p).unwrap();
        write!(f, "{{\"a\":1}}\n{{\"b\":2}}\n{{\"c\":").unwrap();
        f.flush().unwrap();
        let (pass, _) = read_pass(&p, 0, None).unwrap();
        let Pass::Read { pos, lines, full } = pass else { panic!("expected a read") };
        assert!(!full);
        assert_eq!(lines.len(), 2);
        assert_eq!(pos, 16);
        let (pass, _) = read_pass(&p, pos, None).unwrap();
        assert!(matches!(pass, Pass::Partial { .. }));
        write!(f, "3}}\n").unwrap();
        f.flush().unwrap();
        let (pass, _) = read_pass(&p, pos, None).unwrap();
        let Pass::Read { pos: p2, lines, .. } = pass else { panic!("expected a read") };
        assert_eq!(lines, vec![b"{\"c\":3}".to_vec()]);
        let (pass, _) = read_pass(&p, p2, None).unwrap();
        assert!(matches!(pass, Pass::Idle));
        drop(f);
        // truncated
        std::fs::write(&p, "{\"z\":1}\n").unwrap();
        let (pass, _) = read_pass(&p, p2, None).unwrap();
        assert!(matches!(pass, Pass::Replaced(_)));
        // head changed although longer than the cursor
        let a = format!("{}\n", "a".repeat(600));
        std::fs::write(&p, &a).unwrap();
        let (_, h1) = read_pass(&p, 0, None).unwrap();
        let b = format!("{}\n{}\n", "b".repeat(600), "b".repeat(600));
        std::fs::write(&p, &b).unwrap();
        let (pass, _) = read_pass(&p, a.len() as u64, h1).unwrap();
        assert!(matches!(pass, Pass::Replaced(_)));
        let (pass, _) = read_pass(&dir.join("nope.jsonl"), 0, None).unwrap();
        assert!(matches!(pass, Pass::Missing(_)));
        let _ = std::fs::remove_dir_all(&dir);
    }
}
