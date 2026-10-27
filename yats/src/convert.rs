//! Copying a whole database from one engine to another, and checking the
//! copy. The transport for moving bfdb off sled (and back, to roll back); see
//! `docs/plans/storage-migration.md`.
//!
//! [`copy_all`] copies every tree, pair for pair, as raw bytes: keys keep
//! their order-preserving encoding and values are not touched, so a copy is
//! exact whatever the value types are. [`verify`] then walks both databases
//! side by side. Neither ever writes to the source.
//!
//! Like [`export`](crate::export), a copy walks trees one after another and
//! is not a snapshot across them: copy from a stopped database or a copy of
//! one.

use crate::{codec, Db};
use anyhow::{bail, Result};

/// What a copy (or a snapshot) moved.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CopyStats {
    /// Every tree with its pair count, in copy order.
    pub trees: Vec<(String, u64)>,
    pub pairs: u64,
}

/// Pairs gathered for one write: about `MAX_BYTES` or `MAX_PAIRS`.
#[derive(Default)]
pub(crate) struct Chunk {
    pairs: Vec<(Vec<u8>, Vec<u8>)>,
    bytes: usize,
}

impl Chunk {
    const MAX_PAIRS: usize = 10_000;
    const MAX_BYTES: usize = 32 << 20;

    pub(crate) fn push(&mut self, k: &[u8], v: &[u8]) {
        self.bytes += k.len() + v.len();
        self.pairs.push((k.to_vec(), v.to_vec()));
    }

    pub(crate) fn is_full(&self) -> bool {
        self.pairs.len() >= Self::MAX_PAIRS || self.bytes >= Self::MAX_BYTES
    }

    pub(crate) fn take(&mut self) -> Vec<(Vec<u8>, Vec<u8>)> {
        self.bytes = 0;
        std::mem::take(&mut self.pairs)
    }
}

fn tree_name(n: &[u8]) -> String {
    String::from_utf8_lossy(n).into_owned()
}

/// Copy every tree of `from` into `to` (which should be empty), then make
/// sure `to` hands out no id `from` might already have. `progress` is called
/// after each tree. Returns what was copied.
pub fn copy_all(from: &Db, to: &Db, mut progress: impl FnMut(&str, u64)) -> Result<CopyStats> {
    let mut stats = CopyStats::default();
    for name in from.tree_names()? {
        let src = from.open_tree(&name)?;
        let dst = to.open_tree(&name)?;
        let mut n = 0u64;
        let mut chunk = Chunk::default();
        for r in src.iter() {
            let (k, v) = r?;
            chunk.push(&k, &v);
            n += 1;
            if chunk.is_full() {
                dst.insert_many(chunk.take())?;
            }
        }
        dst.insert_many(chunk.take())?;
        let name = tree_name(&name);
        progress(&name, n);
        stats.trees.push((name, n));
        stats.pairs += n;
    }
    to.ensure_next_id(from.next_id_for_copy()?)?;
    to.flush()?;
    Ok(stats)
}

/// One way a copy differs from its source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mismatch {
    /// A tree of the source is not in the copy.
    MissingTree(String),
    /// The copy has a tree the source does not.
    ExtraTree(String),
    /// Different numbers of pairs.
    Count { tree: String, source: u64, copy: u64 },
    /// The first key at which the trees differ (missing, extra or
    /// reordered); the comparison of that tree stops there.
    Key { tree: String, key: Vec<u8> },
    /// Same key, different value.
    Value { tree: String, key: Vec<u8> },
    /// A point lookup in the copy did not find what iteration did.
    Lookup { tree: String, key: Vec<u8> },
}

/// What [`verify`] found.
#[derive(Debug, Clone, Default)]
pub struct VerifyReport {
    pub trees: u64,
    pub pairs: u64,
    /// Values that are MessagePack in the copy and bincode in the source
    /// (re-encoded after the copy). Only counted with `allow_reencoded`.
    pub reencoded: u64,
    /// Point lookups done, besides the full walk.
    pub lookups: u64,
    /// The first few differences (at most 50).
    pub mismatches: Vec<Mismatch>,
    /// All differences, including those past the first 50.
    pub mismatch_count: u64,
}

impl VerifyReport {
    pub fn ok(&self) -> bool {
        self.mismatch_count == 0
    }

    fn push(&mut self, m: Mismatch) {
        self.mismatch_count += 1;
        if self.mismatches.len() < 50 {
            self.mismatches.push(m);
        }
    }
}

/// Compare `copy` with `source`: the same trees, the same pair count in each,
/// the same keys in the same order, and the same values -- byte for byte, or,
/// with `allow_reencoded`, a bincode value in the source may be MessagePack
/// in the copy (the typed check of those is `Tree::reencode`'s). Every
/// `sample_every`-th pair is also fetched from the copy by key, which checks
/// its index as well as its iteration order.
pub fn verify(source: &Db, copy: &Db, allow_reencoded: bool, sample_every: u64) -> Result<VerifyReport> {
    if sample_every == 0 {
        bail!("sample_every must be at least 1")
    }
    let mut rep = VerifyReport::default();
    let src_names = source.tree_names()?;
    let copy_names = copy.tree_names()?;
    for n in &copy_names {
        if !src_names.contains(n) {
            rep.push(Mismatch::ExtraTree(tree_name(n)));
        }
    }
    for name in &src_names {
        let tree = tree_name(name);
        rep.trees += 1;
        if !copy_names.contains(name) {
            rep.push(Mismatch::MissingTree(tree));
            continue;
        }
        let (a, b) = (source.open_tree(name)?, copy.open_tree(name)?);
        let (mut ia, mut ib) = (a.iter(), b.iter());
        let (mut na, mut nb) = (0u64, 0u64);
        let mut diverged = false;
        loop {
            let (x, y) = (ia.next().transpose()?, ib.next().transpose()?);
            if x.is_some() {
                na += 1;
            }
            if y.is_some() {
                nb += 1;
            }
            match (x, y) {
                (None, None) => break,
                (Some((ka, va)), Some((kb, vb))) if !diverged => {
                    rep.pairs += 1;
                    if ka != kb {
                        rep.push(Mismatch::Key { tree: tree.clone(), key: ka.to_vec() });
                        diverged = true;
                        continue;
                    }
                    if va != vb {
                        if allow_reencoded && !codec::is_msgpack(&va) && codec::is_msgpack(&vb) {
                            rep.reencoded += 1;
                        } else {
                            rep.push(Mismatch::Value { tree: tree.clone(), key: ka.to_vec() });
                        }
                    }
                    if na % sample_every == 0 {
                        rep.lookups += 1;
                        if b.get(&ka)?.as_deref() != Some(&vb[..]) {
                            rep.push(Mismatch::Lookup { tree: tree.clone(), key: ka.to_vec() });
                        }
                    }
                }
                (Some((k, _)), None) | (None, Some((k, _))) if !diverged => {
                    rep.push(Mismatch::Key { tree: tree.clone(), key: k.to_vec() });
                    diverged = true;
                }
                _ => (),
            }
        }
        if na != nb {
            rep.push(Mismatch::Count { tree, source: na, copy: nb });
        }
    }
    Ok(rep)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Engine, Tree};

    fn fill(db: &Db) {
        let a: Tree<(u32, String), Vec<u8>> = Tree::open(db, "a").unwrap();
        for i in 0..2500u32 {
            a.insert(&(i % 7, format!("k{i}")), &vec![i as u8; (i % 13) as usize]).unwrap();
        }
        let b: Tree<u64, (f64, String)> = Tree::open(db, "b").unwrap();
        b.insert(&7, &(1.5, "x".into())).unwrap();
        Tree::<u8, u8>::open(db, "empty").unwrap();
        for _ in 0..10 {
            db.generate_id().unwrap();
        }
    }

    #[test]
    fn copies_both_ways_and_verifies() {
        for (from_e, to_e) in [(Engine::Sled, Engine::Redb), (Engine::Redb, Engine::Sled)] {
            let from = Db::temporary(from_e).unwrap();
            fill(&from);
            let to = Db::temporary(to_e).unwrap();
            let stats = copy_all(&from, &to, |_, _| ()).unwrap();
            assert_eq!(stats.pairs, 2501, "{from_e} -> {to_e}");
            let rep = verify(&from, &to, false, 7).unwrap();
            assert!(rep.ok(), "{from_e} -> {to_e}: {:?}", rep.mismatches);
            assert!(rep.lookups > 300);
            // the copy reads back typed, in the same order
            let a: Tree<(u32, String), Vec<u8>> = Tree::open(&to, "a").unwrap();
            let src: Tree<(u32, String), Vec<u8>> = Tree::open(&from, "a").unwrap();
            let x: Vec<_> = a.iter().map(|r| r.unwrap()).collect();
            let y: Vec<_> = src.iter().map(|r| r.unwrap()).collect();
            assert_eq!(x, y);
            // ids carry on past the source's
            let last = from.next_id_for_copy().unwrap();
            assert!(to.generate_id().unwrap() >= last.min(10));
            assert!(to.generate_id().unwrap() >= 10);
        }
    }

    #[test]
    fn verify_finds_differences() {
        let from = Db::temporary_sled().unwrap();
        fill(&from);
        let to = Db::temporary_redb().unwrap();
        copy_all(&from, &to, |_, _| ()).unwrap();
        let b = to.open_tree("b").unwrap();
        b.insert(crate::serialize(&7u64).unwrap(), &b"changed"[..]).unwrap();
        b.insert(crate::serialize(&8u64).unwrap(), &b"extra"[..]).unwrap();
        to.open_tree("zzz").unwrap();
        let rep = verify(&from, &to, false, 1).unwrap();
        assert!(!rep.ok());
        assert!(rep.mismatches.contains(&Mismatch::ExtraTree("zzz".into())));
        assert!(rep.mismatches.iter().any(|m| matches!(m, Mismatch::Value { tree, .. } if tree == "b")));
        assert!(rep.mismatches.iter().any(|m| matches!(m, Mismatch::Count { tree, source: 1, copy: 2 } if tree == "b")));
    }

    #[test]
    fn reencoded_values_verify_only_when_allowed() {
        let from = Db::temporary_sled().unwrap();
        fill(&from);
        let to = Db::temporary_redb().unwrap();
        copy_all(&from, &to, |_, _| ()).unwrap();
        let st = Tree::<u64, (f64, String)>::open(&to, "b").unwrap().reencode(false).unwrap();
        assert_eq!((st.rows, st.reencoded), (1, 1));
        assert!(!verify(&from, &to, false, 1).unwrap().ok());
        let rep = verify(&from, &to, true, 1).unwrap();
        assert!(rep.ok(), "{:?}", rep.mismatches);
        assert_eq!(rep.reencoded, 1);
    }
}
