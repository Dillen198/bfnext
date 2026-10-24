// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Range result storage: raw JSON in plain sled trees.
//!
//! Deliberately not yats: yats encodes values with positional bincode, so a
//! new field on `RangeRecord` would make every row an older build wrote
//! undecodable (see the note on `news` in db.rs). These records are the
//! newest, fastest-moving schema in the project. They are also stored as the
//! engine wrote them rather than re-serialised from our own struct, so a field
//! a newer engine adds survives an older bfdb and reaches the range site the
//! moment the site knows about it.
//!
//! Trees (all keys are raw bytes; `ts` is the record's unix-ms timestamp as a
//! big-endian u64 so keys sort chronologically):
//!
//! | tree             | key                          | value                          |
//! |------------------|------------------------------|--------------------------------|
//! | `range_records`  | `<instance>\0<ts><id>`       | record JSON, `track` removed   |
//! | `range_by_id`    | `<id>`                       | the `range_records` key        |
//! | `range_by_pilot` | `<ucid>\0<ts><id>`           | `<instance>`                   |
//! | `range_tracks`   | `<id>`                       | zstd-compressed `Track` JSON   |
//! | `range_cursor`   | `<instance>`                 | byte offset into range.jsonl   |
//! | `range_pilots`   | `<instance>\0<ucid>`         | `{"name","last_ts"}` JSON      |
//! | `range_meta`     | free-form                    | head fingerprints, prune marks,|
//! |                  |                              | the last weapon database       |
//!
//! None of these derive from `stats.jsonl`, so the stats rebuild and the
//! per-instance campaign reset leave them alone.

use anyhow::{anyhow, bail, Result};
use bfprotocols::range::RangeRecord;
use chrono::{DateTime, Utc};
use serde::Deserialize;
use serde_json::Value;
use sled::{Db, Tree};

pub(crate) struct RangeStore {
    records: Tree,
    by_id: Tree,
    by_pilot: Tree,
    tracks: Tree,
    cursor: Tree,
    pilots: Tree,
    meta: Tree,
}

/// One stored record.
#[derive(Debug, Clone)]
pub(crate) struct Stored {
    pub(crate) instance: String,
    pub(crate) ts_ms: u64,
    pub(crate) id: String,
    /// The record JSON as the engine wrote it, minus `track`.
    pub(crate) raw: Value,
}

impl Stored {
    /// The typed record, or `None` when this build does not understand it
    /// (a result kind or required field added by a newer engine).
    pub(crate) fn decode(&self) -> Option<RangeRecord> {
        RangeRecord::deserialize(&self.raw).ok()
    }

    pub(crate) fn kind(&self) -> &str {
        self.raw.pointer("/result/kind").and_then(|v| v.as_str()).unwrap_or("")
    }

    pub(crate) fn ucid(&self) -> Option<&str> {
        self.raw.pointer("/pilot/ucid").and_then(|v| v.as_str())
    }

    pub(crate) fn pilot_name(&self) -> &str {
        self.raw.pointer("/pilot/name").and_then(|v| v.as_str()).unwrap_or("")
    }

    pub(crate) fn unit_type(&self) -> &str {
        self.raw.get("unit_type").and_then(|v| v.as_str()).unwrap_or("")
    }

    pub(crate) fn station(&self) -> Option<&str> {
        self.raw.pointer("/result/station_id").and_then(|v| v.as_str())
    }

    pub(crate) fn ts(&self) -> DateTime<Utc> {
        DateTime::from_timestamp_millis(self.ts_ms as i64).unwrap_or_default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Insert {
    Inserted,
    Duplicate,
}

fn primary_key(instance: &str, ts_ms: u64, id: &str) -> Vec<u8> {
    let mut k = Vec::with_capacity(instance.len() + 9 + id.len());
    k.extend_from_slice(instance.as_bytes());
    k.push(0);
    k.extend_from_slice(&ts_ms.to_be_bytes());
    k.extend_from_slice(id.as_bytes());
    k
}

/// `<prefix>\0` and the exclusive end of that prefix range.
fn prefix_bounds(prefix: &str) -> (Vec<u8>, Vec<u8>) {
    let mut lo = prefix.as_bytes().to_vec();
    lo.push(0);
    let mut hi = prefix.as_bytes().to_vec();
    hi.push(1);
    (lo, hi)
}

/// Split `<prefix>\0<ts><id>` into (prefix, ts, id).
fn split_key(k: &[u8]) -> Option<(&str, u64, &str)> {
    let z = k.iter().position(|b| *b == 0)?;
    let rest = &k[z + 1..];
    if rest.len() < 8 {
        return None;
    }
    let ts = u64::from_be_bytes(rest[..8].try_into().ok()?);
    Some((std::str::from_utf8(&k[..z]).ok()?, ts, std::str::from_utf8(&rest[8..]).ok()?))
}

impl RangeStore {
    pub(crate) fn open(db: &Db) -> Result<Self> {
        Ok(Self {
            records: db.open_tree("range_records")?,
            by_id: db.open_tree("range_by_id")?,
            by_pilot: db.open_tree("range_by_pilot")?,
            tracks: db.open_tree("range_tracks")?,
            cursor: db.open_tree("range_cursor")?,
            pilots: db.open_tree("range_pilots")?,
            meta: db.open_tree("range_meta")?,
        })
    }

    // ── ingestion ──────────────────────────────────────────────────────

    /// Store one engine line. `line` is the full record object including
    /// `track`. Idempotent: a record id already present is a `Duplicate` and
    /// nothing is written. The `range_by_id` entry is written last, so a
    /// crash part-way leaves no id marker and the re-read simply redoes it
    /// (every other key is deterministic).
    pub(crate) fn insert(&self, instance: &str, mut line: Value) -> Result<Insert> {
        let obj = line.as_object_mut().ok_or_else(|| anyhow!("not a JSON object"))?;
        let id = match obj.get("id").and_then(|v| v.as_str()) {
            Some(s) if !s.is_empty() => s.to_string(),
            _ => bail!("record has no id"),
        };
        let ts: DateTime<Utc> = obj
            .get("ts")
            .cloned()
            .ok_or_else(|| anyhow!("record {id} has no ts"))
            .and_then(|v| serde_json::from_value(v).map_err(|e| anyhow!("record {id}: bad ts: {e}")))?;
        if self.by_id.contains_key(id.as_bytes())? {
            return Ok(Insert::Duplicate);
        }
        let ts_ms = ts.timestamp_millis().max(0) as u64;
        let track = obj.remove("track").filter(|t| !t.is_null());
        let pk = primary_key(instance, ts_ms, &id);
        self.records.insert(&pk, serde_json::to_vec(&line)?)?;
        if let Some(t) = track {
            let js = serde_json::to_vec(&t)?;
            self.tracks.insert(id.as_bytes(), zstd::encode_all(&js[..], 3)?)?;
        }
        let ucid = line.pointer("/pilot/ucid").and_then(|v| v.as_str()).filter(|s| !s.is_empty());
        if let Some(ucid) = ucid {
            self.by_pilot.insert(primary_key(ucid, ts_ms, &id), instance.as_bytes())?;
            let name = line.pointer("/pilot/name").and_then(|v| v.as_str()).unwrap_or("");
            let mut nk = instance.as_bytes().to_vec();
            nk.push(0);
            nk.extend_from_slice(ucid.as_bytes());
            let newer = match self.pilots.get(&nk)? {
                Some(v) => serde_json::from_slice::<Value>(&v)
                    .ok()
                    .and_then(|v| v.get("last_ts").and_then(|t| t.as_u64()))
                    .map_or(true, |last| ts_ms >= last),
                None => true,
            };
            if newer && !name.is_empty() {
                let v = serde_json::json!({ "name": name, "last_ts": ts_ms });
                self.pilots.insert(nk, serde_json::to_vec(&v)?)?;
            }
        }
        self.by_id.insert(id.as_bytes(), pk)?;
        Ok(Insert::Inserted)
    }

    pub(crate) fn cursor(&self, instance: &str) -> Result<u64> {
        Ok(match self.cursor.get(instance.as_bytes())? {
            Some(v) if v.len() == 8 => u64::from_be_bytes(v.as_ref().try_into()?),
            _ => 0,
        })
    }

    pub(crate) fn set_cursor(&self, instance: &str, pos: u64) -> Result<()> {
        self.cursor.insert(instance.as_bytes(), &pos.to_be_bytes()[..])?;
        Ok(())
    }

    pub(crate) fn meta_get(&self, key: &str) -> Result<Option<Vec<u8>>> {
        Ok(self.meta.get(key.as_bytes())?.map(|v| v.to_vec()))
    }

    pub(crate) fn meta_set(&self, key: &str, v: &[u8]) -> Result<()> {
        self.meta.insert(key.as_bytes(), v)?;
        Ok(())
    }

    /// Drop the debrief tracks of records older than `before_ms`. The records
    /// themselves stay. Resumes from where the previous sweep stopped.
    pub(crate) fn prune_tracks(&self, instance: &str, before_ms: u64) -> Result<usize> {
        let mark_key = format!("pruned\0{instance}");
        let from = match self.meta.get(mark_key.as_bytes())? {
            Some(v) if v.len() == 8 => u64::from_be_bytes(v.as_ref().try_into()?),
            _ => 0,
        };
        if before_ms <= from {
            return Ok(0);
        }
        let lo = primary_key(instance, from, "");
        let hi = primary_key(instance, before_ms, "");
        let mut n = 0;
        for r in self.records.range(lo..hi) {
            let (k, _) = r?;
            if let Some((_, _, id)) = split_key(&k) {
                if self.tracks.remove(id.as_bytes())?.is_some() {
                    n += 1;
                }
            }
        }
        self.meta.insert(mark_key.as_bytes(), &before_ms.to_be_bytes()[..])?;
        Ok(n)
    }

    // ── reads ──────────────────────────────────────────────────────────

    fn decode_row(k: &[u8], v: &[u8]) -> Option<Stored> {
        let (instance, ts_ms, id) = split_key(k)?;
        let raw = serde_json::from_slice(v).ok()?;
        Some(Stored { instance: instance.to_string(), ts_ms, id: id.to_string(), raw })
    }

    /// One record by id (without its track).
    pub(crate) fn get(&self, id: &str) -> Result<Option<Stored>> {
        let Some(pk) = self.by_id.get(id.as_bytes())? else {
            return Ok(None);
        };
        Ok(self.records.get(&pk)?.and_then(|v| Self::decode_row(&pk, &v)))
    }

    /// Where a record sits in time, for `before_id` paging: (unix ms, id).
    pub(crate) fn position(&self, id: &str) -> Result<Option<(u64, String)>> {
        Ok(self
            .by_id
            .get(id.as_bytes())?
            .and_then(|pk| split_key(&pk).map(|(_, ts, id)| (ts, id.to_string()))))
    }

    pub(crate) fn has_track(&self, id: &str) -> bool {
        self.tracks.contains_key(id.as_bytes()).unwrap_or(false)
    }

    /// A record's debrief track, decompressed.
    pub(crate) fn track(&self, id: &str) -> Result<Option<Value>> {
        let Some(z) = self.tracks.get(id.as_bytes())? else {
            return Ok(None);
        };
        let js = zstd::decode_all(&z[..])?;
        Ok(Some(serde_json::from_slice(&js)?))
    }

    /// The full record, `track` included, as the engine wrote it.
    pub(crate) fn full(&self, id: &str) -> Result<Option<Stored>> {
        let Some(mut s) = self.get(id)? else {
            return Ok(None);
        };
        if let (Some(t), Some(o)) = (self.track(id)?, s.raw.as_object_mut()) {
            o.insert("track".into(), t);
        }
        Ok(Some(s))
    }

    /// Newest-first walk over the records of `instances` at or after
    /// `since_ms` and strictly before `before` (all when `None`), merged by
    /// time across instances. `before` is (unix ms, record id): with an empty
    /// id it excludes everything at that millisecond; with the id of the last
    /// record a page ended on, it resumes right after that record, so records
    /// sharing a timestamp (a missile shot's shooter and target records) are
    /// never skipped at a page boundary. `f` returns false to stop.
    pub(crate) fn walk_newest(
        &self,
        instances: &[String],
        since_ms: u64,
        before: Option<(u64, &str)>,
        mut f: impl FnMut(Stored) -> bool,
    ) -> Result<()> {
        let mut iters: Vec<_> = instances
            .iter()
            .map(|inst| {
                let lo = primary_key(inst, since_ms, "");
                let hi = match before {
                    Some((b, id)) => primary_key(inst, b, id),
                    None => prefix_bounds(inst).1,
                };
                self.records.range(lo..hi).rev().peekable()
            })
            .collect();
        loop {
            // Pick the iterator whose head is newest.
            let mut best: Option<(usize, u64)> = None;
            let mut failed: Option<usize> = None;
            for (i, it) in iters.iter_mut().enumerate() {
                match it.peek() {
                    Some(Ok((k, _))) => {
                        let ts = split_key(k).map(|(_, t, _)| t).unwrap_or(0);
                        if best.map_or(true, |(_, bt)| ts > bt) {
                            best = Some((i, ts));
                        }
                    }
                    Some(Err(_)) => {
                        failed = Some(i);
                        break;
                    }
                    None => (),
                }
            }
            if let Some(i) = failed {
                if let Some(Err(e)) = iters[i].next() {
                    return Err(e.into());
                }
            }
            let Some((i, _)) = best else {
                return Ok(());
            };
            let (k, v) = iters[i].next().unwrap()?;
            if let Some(s) = Self::decode_row(&k, &v) {
                if !f(s) {
                    return Ok(());
                }
            }
        }
    }

    /// Every record of `instances` at or after `since_ms`, oldest first.
    pub(crate) fn window(&self, instances: &[String], since_ms: u64) -> Result<Vec<Stored>> {
        let mut out = Vec::new();
        self.walk_newest(instances, since_ms, None, |s| {
            out.push(s);
            true
        })?;
        out.reverse();
        Ok(out)
    }

    /// Newest-first walk over one pilot's records within `instances`.
    pub(crate) fn walk_pilot(
        &self,
        ucid: &str,
        instances: &[String],
        mut f: impl FnMut(Stored) -> bool,
    ) -> Result<()> {
        let (lo, hi) = prefix_bounds(ucid);
        for r in self.by_pilot.range(lo..hi).rev() {
            let (k, inst) = r?;
            let inst = std::str::from_utf8(&inst).unwrap_or("");
            if !instances.iter().any(|i| i == inst) {
                continue;
            }
            let Some((_, ts, id)) = split_key(&k) else {
                continue;
            };
            let pk = primary_key(inst, ts, id);
            if let Some(v) = self.records.get(&pk)? {
                if let Some(s) = Self::decode_row(&pk, &v) {
                    if !f(s) {
                        break;
                    }
                }
            }
        }
        Ok(())
    }

    /// How many records a pilot has within `instances`.
    pub(crate) fn pilot_count(&self, ucid: &str, instances: &[String]) -> Result<usize> {
        let (lo, hi) = prefix_bounds(ucid);
        let mut n = 0;
        for r in self.by_pilot.range(lo..hi) {
            let (_, inst) = r?;
            if instances.iter().any(|i| i.as_bytes() == inst.as_ref()) {
                n += 1;
            }
        }
        Ok(n)
    }

    /// Every pilot seen on `instances`: (ucid, latest name, last unix ms).
    /// With several instances the newest name wins.
    pub(crate) fn pilots(&self, instances: &[String]) -> Result<Vec<(String, String, u64)>> {
        let mut by: std::collections::HashMap<String, (String, u64)> = Default::default();
        for inst in instances {
            let (lo, hi) = prefix_bounds(inst);
            for r in self.pilots.range(lo..hi) {
                let (k, v) = r?;
                let Some(z) = k.iter().position(|b| *b == 0) else {
                    continue;
                };
                let ucid = String::from_utf8_lossy(&k[z + 1..]).to_string();
                let Ok(v) = serde_json::from_slice::<Value>(&v) else {
                    continue;
                };
                let name = v.get("name").and_then(|n| n.as_str()).unwrap_or("").to_string();
                let last = v.get("last_ts").and_then(|t| t.as_u64()).unwrap_or(0);
                match by.get(&ucid) {
                    Some((_, l)) if *l >= last => (),
                    _ => {
                        by.insert(ucid, (name, last));
                    }
                }
            }
        }
        Ok(by.into_iter().map(|(u, (n, l))| (u, n, l)).collect())
    }

    /// The latest name recorded for a pilot, if any.
    pub(crate) fn pilot_name(&self, ucid: &str, instances: &[String]) -> Option<String> {
        let mut best: Option<(String, u64)> = None;
        for inst in instances {
            let mut k = inst.as_bytes().to_vec();
            k.push(0);
            k.extend_from_slice(ucid.as_bytes());
            if let Ok(Some(v)) = self.pilots.get(k) {
                if let Ok(v) = serde_json::from_slice::<Value>(&v) {
                    let name = v.get("name").and_then(|n| n.as_str()).unwrap_or("").to_string();
                    let last = v.get("last_ts").and_then(|t| t.as_u64()).unwrap_or(0);
                    if best.as_ref().map_or(true, |(_, l)| last > *l) {
                        best = Some((name, last));
                    }
                }
            }
        }
        best.map(|(n, _)| n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp_store() -> (RangeStore, Db) {
        let db = sled::Config::new().temporary(true).open().unwrap();
        (RangeStore::open(&db).unwrap(), db)
    }

    fn rec(id: &str, ts: &str, ucid: &str) -> Value {
        serde_json::json!({
            "id": id, "ts": ts, "pilot": {"ucid": ucid, "name": format!("pilot-{ucid}")},
            "unit_type": "FA-18C_hornet",
            "result": {"kind": "future_kind", "x": 1},
            "track": {"kind": "path", "paths": {}}
        })
    }

    #[test]
    fn insert_dedupe_and_walk() {
        let (s, _db) = tmp_store();
        let insts = vec!["r1".to_string()];
        assert_eq!(s.insert("r1", rec("a", "2026-09-01T10:00:00Z", "u1")).unwrap(), Insert::Inserted);
        assert_eq!(s.insert("r1", rec("a", "2026-09-01T10:00:00Z", "u1")).unwrap(), Insert::Duplicate);
        s.insert("r1", rec("b", "2026-09-02T10:00:00Z", "u1")).unwrap();
        s.insert("r1", rec("c", "2026-09-03T10:00:00Z", "u2")).unwrap();
        s.insert("r2", rec("d", "2026-09-04T10:00:00Z", "u2")).unwrap();
        let mut ids = vec![];
        s.walk_newest(&insts, 0, None, |r| {
            ids.push(r.id.clone());
            true
        })
        .unwrap();
        assert_eq!(ids, vec!["c", "b", "a"]);
        let both = vec!["r1".to_string(), "r2".to_string()];
        let w = s.window(&both, 0).unwrap();
        assert_eq!(w.iter().map(|r| r.id.as_str()).collect::<Vec<_>>(), vec!["a", "b", "c", "d"]);
        // stored raw, unknown kind kept, track split off
        let a = s.get("a").unwrap().unwrap();
        assert!(a.raw.get("track").is_none());
        assert_eq!(a.kind(), "future_kind");
        assert!(s.has_track("a"));
        assert!(s.full("a").unwrap().unwrap().raw.get("track").is_some());
        assert_eq!(s.pilot_count("u1", &insts).unwrap(), 2);
        assert_eq!(s.pilot_count("u2", &insts).unwrap(), 1);
        let mut p = vec![];
        s.walk_pilot("u2", &both, |r| {
            p.push(r.id.clone());
            true
        })
        .unwrap();
        assert_eq!(p, vec!["d", "c"]);
        assert_eq!(s.pilot_name("u2", &both).as_deref(), Some("pilot-u2"));
        // before bound is exclusive
        let before = DateTime::parse_from_rfc3339("2026-09-02T10:00:00Z").unwrap().timestamp_millis();
        let mut ids = vec![];
        s.walk_newest(&insts, 0, Some((before as u64, "")), |r| {
            ids.push(r.id.clone());
            true
        })
        .unwrap();
        assert_eq!(ids, vec!["a"]);
        // pruning drops tracks, keeps records
        let cut = DateTime::parse_from_rfc3339("2026-09-02T12:00:00Z").unwrap().timestamp_millis();
        assert_eq!(s.prune_tracks("r1", cut as u64).unwrap(), 2);
        assert!(!s.has_track("a"));
        assert!(s.get("a").unwrap().is_some());
        assert!(s.has_track("c"));
        assert_eq!(s.prune_tracks("r1", cut as u64).unwrap(), 0);
        // cursor round trip
        s.set_cursor("r1", 1234).unwrap();
        assert_eq!(s.cursor("r1").unwrap(), 1234);
        assert_eq!(s.cursor("r9").unwrap(), 0);
    }

    #[test]
    fn pages_through_shared_timestamps() {
        let (s, _db) = tmp_store();
        let insts = vec!["r1".to_string()];
        for id in ["m-a", "m-b", "m-c"] {
            s.insert("r1", rec(id, "2026-09-05T10:00:00Z", "u1")).unwrap();
        }
        s.insert("r1", rec("older", "2026-09-04T10:00:00Z", "u1")).unwrap();
        let page = |before: Option<(u64, &str)>| {
            let mut ids = vec![];
            s.walk_newest(&insts, 0, before, |r| {
                ids.push(r.id.clone());
                ids.len() < 2
            })
            .unwrap();
            ids
        };
        let p1 = page(None);
        assert_eq!(p1, vec!["m-c", "m-b"]);
        let (ts, id) = s.position(p1.last().unwrap()).unwrap().unwrap();
        assert_eq!(page(Some((ts, id.as_str()))), vec!["m-a", "older"]);
    }

    #[test]
    fn rejects_malformed() {
        let (s, _db) = tmp_store();
        assert!(s.insert("r1", serde_json::json!([1, 2])).is_err());
        assert!(s.insert("r1", serde_json::json!({"ts": "2026-09-01T10:00:00Z"})).is_err());
        assert!(s.insert("r1", serde_json::json!({"id": "x", "ts": "yesterday"})).is_err());
    }
}
