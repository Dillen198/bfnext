use anyhow::{Context, Result, anyhow};
use arcstr::ArcStr;
use bfprotocols::stats::{PATH, Stat};
use chrono::prelude::*;
use dcso3::String as LuaString;
use netidx::{
    chars::Chars,
    config::Config,
    path::Path,
    publisher::{Publisher, Value},
    resolver_client::GlobSet,
    subscriber::Event,
};
use netidx_archive::{
    config::{ConfigBuilder, PublishConfigBuilder, RecordConfigBuilder},
    logfile::{BATCH_POOL, BatchItem, Id},
    logfile_collection::ArchiveCollectionWriter,
    recorder::Recorder,
};
use std::path::PathBuf;
use tokio::task;

use super::encode;

/// Rotate the segment the last run left behind out of the way, compressed if
/// it can be. Compressing trains a zstd dictionary on the segment's records,
/// and zstd refuses to train on only a handful ("Src size is incorrect"), which
/// a segment of a few large records over 4 KiB is. That used to fail the whole
/// stats publisher, and `cfg_loaded` took the message for a torn archive and
/// set the entire stats directory aside -- on most restarts. A failed
/// compression has already closed the writer and leaves the file where it was,
/// so the plain rotation the 60 second timer uses still works.
async fn rotate_leftover(
    log: &mut ArchiveCollectionWriter,
    now: DateTime<Utc>,
) -> Result<()> {
    if let Err(e) = log.rotate_and_compress(now, None).await {
        log::warn!(
            "stats: could not compress the leftover segment ({e:#}), rotating it as is"
        );
        task::block_in_place(|| log.rotate(now))
            .context("rotating the leftover segment")?;
    }
    Ok(())
}

pub(super) struct Statspub {
    id: Id,
    log: ArchiveCollectionWriter,
    _recorder: Recorder,
    last_rotate: DateTime<Utc>,
    sortie: LuaString,
}

impl Statspub {
    pub(super) async fn new(
        publisher: Publisher,
        cfg: &Config,
        write_dir: PathBuf,
        base: Path,
        sortie: LuaString,
        fresh: bool,
    ) -> Result<Self> {
        let shard = ArcStr::from("0");
        let config = ConfigBuilder::default()
            .archive_directory(write_dir)
            .publish(
                PublishConfigBuilder::default()
                    .base(base)
                    .bind_from_cfg(cfg)
                    .build()
                    .context("publish config builder")?,
            )
            .record([(
                shard.clone(),
                RecordConfigBuilder::default()
                    .spec(GlobSet::new(true, vec![])?)
                    .build()
                    .context("record config builder")?,
            )])
            .netidx_config(cfg.clone())
            .build()?;
        let recorder = Recorder::start_with(config, Some(publisher), None)
            .await
            .context("starting recorder tasks")?;
        let shard_id = *recorder
            .shards
            .by_name
            .get(&shard)
            .ok_or_else(|| anyhow!("no shard id for shard {shard}"))?;
        let mut log = recorder
            .shards
            .writers
            .lock()
            .remove(&shard_id)
            .ok_or_else(|| anyhow!("no writer for shard"))?;
        if log.len()? > 4096 {
            let now = Utc::now();
            rotate_leftover(&mut log, now).await?;
            let reader =
                log.current_reader().context("get current reader after rotate")?;
            recorder
                .shards
                .notify_rotated(shard_id, now, reader)
                .context("notify rotate")?
        }
        let id = match log.id_for_path(&Path::from(PATH)) {
            Some(id) => id,
            None => {
                log.add_paths([&Path::from(PATH)])?;
                task::block_in_place(|| log.flush_pathindex())?;
                log.id_for_path(&Path::from(PATH))
                    .ok_or_else(|| anyhow!("no id after adding id"))?
            }
        };
        let now = Utc::now();
        let mut t = Self { id, log, _recorder: recorder, last_rotate: now, sortie };
        // Only announce a genuinely new round here -- bflib already decided
        // this in lib.rs (no saved state to resume) and callers pass that
        // through as `fresh`. Writing this unconditionally made bfdb close
        // and reopen the round on every technical restart (crash recovery,
        // bot-triggered restart) that resumes existing saved state, not
        // just real campaign resets. When resuming, bfdb keeps whatever
        // round context it already has for this sortie.
        if fresh {
            t.append(now, &Stat::NewRound { sortie: t.sortie.clone() })?;
        }
        Ok(t)
    }

    /// This will not block
    pub(super) fn append(&mut self, ts: DateTime<Utc>, stat: &Stat) -> Result<()> {
        task::block_in_place(|| {
            let mut batch = BATCH_POOL.take();
            let buf = Chars::from_bytes(encode(&stat)?.freeze())?;
            batch.push(BatchItem(self.id, Event::Update(Value::String(buf))));
            self.log.add_batch(false, ts, &batch)?;
            // Rotate every 60 seconds so bfdb can read completed historical files.
            // bfdb already has round context from the genuine NewRound seen at
            // round start and doesn't need it re-announced on every rotation --
            // doing so would end and reopen the round every 60 seconds.
            if (ts - self.last_rotate).num_seconds() >= 60 {
                self.last_rotate = ts;
                self.log.flush_current()?;
                self.log.rotate(ts)?;
            }
            Ok(())
        })
    }

    /// Flush the log
    pub(super) fn flush(&mut self) -> Result<()> {
        self.log.flush_current()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn scratch_dir(name: &str) -> PathBuf {
        let d = std::env::temp_dir()
            .join(format!("bflib-statspub-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&d);
        std::fs::create_dir_all(&d).unwrap();
        d
    }

    fn writer(dir: &std::path::Path) -> ArchiveCollectionWriter {
        // the recorder's shape, as `Statspub::new` builds it
        let record = RecordConfigBuilder::default()
            .spec(GlobSet::new(true, vec![]).unwrap())
            .build()
            .unwrap();
        let config = ConfigBuilder::default()
            .archive_directory(dir.to_path_buf())
            .record([(ArcStr::from("0"), record)])
            .build()
            .unwrap();
        ArchiveCollectionWriter::open(Arc::new(config), ArcStr::from("0")).unwrap()
    }

    /// Two records of 3 KiB: over the 4 KiB threshold, far too few to train
    /// a dictionary on.
    fn add_big_records(log: &mut ArchiveCollectionWriter, ts: DateTime<Utc>) -> Id {
        let path = Path::from(PATH);
        log.add_paths([&path]).unwrap();
        let id = log.id_for_path(&path).unwrap();
        for i in 0..2 {
            let mut batch = BATCH_POOL.take();
            let s = format!("{i}{}", "x".repeat(3072));
            batch.push(BatchItem(id, Event::Update(Value::String(Chars::from(s)))));
            log.add_batch(false, ts, &batch).unwrap();
        }
        log.flush_all().unwrap();
        id
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn a_short_leftover_segment_still_rotates() {
        let dir = scratch_dir("short");
        let mut log = writer(&dir);
        let id = add_big_records(&mut log, Utc::now());
        assert!(log.len().unwrap() > 4096);
        // what zstd does with it, so the test keeps meaning something
        let mut probe = writer(&scratch_dir("probe"));
        add_big_records(&mut probe, Utc::now());
        let e = probe.rotate_and_compress(Utc::now(), None).await.unwrap_err();
        assert!(format!("{e:#}").contains("Src size is incorrect"), "{e:#}");

        rotate_leftover(&mut log, Utc::now())
            .await
            .expect("falls back to a plain rotation");
        // a fresh current that takes records again, and the old one kept
        assert!(log.len().unwrap() < 4096);
        let mut batch = BATCH_POOL.take();
        batch.push(BatchItem(id, Event::Update(Value::String(Chars::from("after")))));
        log.add_batch(false, Utc::now(), &batch).unwrap();
        let rotated = std::fs::read_dir(&dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .filter(|e| {
                let n = e.file_name();
                let n = n.to_string_lossy();
                n != "current" && n != "pathmap" && !n.starts_with('.')
            })
            .count();
        assert_eq!(rotated, 1, "the leftover segment is kept, uncompressed");
        drop(log);
        let _ = std::fs::remove_dir_all(&dir);
    }
}
