//! A portable dump of a whole sled database, for moving off sled.
//!
//! Every tree and every key/value pair, as the raw bytes yats stored (keys
//! keep their big-endian, order-preserving encoding; values stay bincode),
//! in a simple length-prefixed file that needs neither sled nor the value
//! types to read. `read_dump` hands the records to a callback, which is all a
//! loader for another store (redb, SQLite) needs. See
//! `docs/plans/storage-migration.md`.
//!
//! The dump walks trees one after another and is not a snapshot across them:
//! take it from a stopped database or a backup copy (bfdb's maintenance
//! backups are consistent copies).
//!
//! ```text
//! file   := MAGIC record* END count
//! record := TREE u32 len name | KV u32 len key u32 len value
//! ```
//! (`TREE` = 1, `KV` = 2, `END` = 0, integers big endian, `count` the u64
//! number of KV records, so a truncated file is detected.)

use anyhow::{bail, Context, Result};
use std::io::{Read, Write};

pub const MAGIC: &[u8; 8] = b"YATSDMP1";
const END: u8 = 0;
const TREE: u8 = 1;
const KV: u8 = 2;

/// What a dump held.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DumpStats {
    pub trees: u64,
    pub pairs: u64,
}

fn put_bytes(w: &mut impl Write, b: &[u8]) -> Result<()> {
    let len = u32::try_from(b.len()).context("record longer than 4 GiB")?;
    w.write_all(&len.to_be_bytes())?;
    w.write_all(b)?;
    Ok(())
}

/// Write every tree of `db` to `w`.
pub fn write_dump(db: &sled::Db, mut w: impl Write) -> Result<DumpStats> {
    let mut stats = DumpStats::default();
    w.write_all(MAGIC)?;
    for name in db.tree_names() {
        let tree = db.open_tree(&name)?;
        w.write_all(&[TREE])?;
        put_bytes(&mut w, &name)?;
        stats.trees += 1;
        for kv in tree.iter() {
            let (k, v) = kv?;
            w.write_all(&[KV])?;
            put_bytes(&mut w, &k)?;
            put_bytes(&mut w, &v)?;
            stats.pairs += 1;
        }
    }
    w.write_all(&[END])?;
    w.write_all(&stats.pairs.to_be_bytes())?;
    w.flush()?;
    Ok(stats)
}

fn get_bytes(r: &mut impl Read) -> Result<Vec<u8>> {
    let mut len = [0; 4];
    r.read_exact(&mut len)?;
    let mut b = vec![0; u32::from_be_bytes(len) as usize];
    r.read_exact(&mut b)?;
    Ok(b)
}

/// Read a dump, calling `pair(tree, key, value)` for every record in order.
pub fn read_dump(
    mut r: impl Read,
    mut pair: impl FnMut(&[u8], Vec<u8>, Vec<u8>) -> Result<()>,
) -> Result<DumpStats> {
    let mut magic = [0; 8];
    r.read_exact(&mut magic).context("reading dump header")?;
    if &magic != MAGIC {
        bail!("not a yats dump")
    }
    let mut stats = DumpStats::default();
    let mut tree: Option<Vec<u8>> = None;
    loop {
        let mut tag = [0; 1];
        r.read_exact(&mut tag).context("dump is truncated")?;
        match tag[0] {
            TREE => {
                tree = Some(get_bytes(&mut r)?);
                stats.trees += 1;
            }
            KV => {
                let Some(t) = &tree else { bail!("key/value record before any tree") };
                let k = get_bytes(&mut r)?;
                let v = get_bytes(&mut r)?;
                pair(t, k, v)?;
                stats.pairs += 1;
            }
            END => {
                let mut n = [0; 8];
                r.read_exact(&mut n).context("dump is truncated")?;
                if u64::from_be_bytes(n) != stats.pairs {
                    bail!("dump says {} pairs, read {}", u64::from_be_bytes(n), stats.pairs)
                }
                return Ok(stats);
            }
            t => bail!("unknown dump record {t}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Tree;

    #[test]
    fn dump_round_trips_every_tree() {
        let db = sled::Config::new().temporary(true).open().unwrap();
        let a: Tree<(u32, String), Vec<u8>> = Tree::open(&db, "a").unwrap();
        a.insert(&(1, "x".into()), &vec![1, 2]).unwrap();
        a.insert(&(2, "y".into()), &vec![]).unwrap();
        let b: Tree<u64, u64> = Tree::open(&db, "b").unwrap();
        b.insert(&7, &8).unwrap();
        let mut file = vec![];
        let wrote = write_dump(&db, &mut file).unwrap();
        assert_eq!(wrote.pairs, 3);
        // load it into a fresh sled, as a migration would into another store
        let db2 = sled::Config::new().temporary(true).open().unwrap();
        let read = read_dump(&file[..], |t, k, v| {
            db2.open_tree(t)?.insert(k, v)?;
            Ok(())
        })
        .unwrap();
        assert_eq!(read, wrote);
        let a2: Tree<(u32, String), Vec<u8>> = Tree::open(&db2, "a").unwrap();
        let rows: Vec<_> = a2.iter().map(|r| r.unwrap()).collect();
        assert_eq!(rows, vec![((1, "x".into()), vec![1, 2]), ((2, "y".into()), vec![])]);
        assert_eq!(Tree::<u64, u64>::open(&db2, "b").unwrap().get(&7).unwrap(), Some(8));
        // a cut-off file is an error, not a short read
        assert!(read_dump(&file[..file.len() - 3], |_, _, _| Ok(())).is_err());
    }
}
