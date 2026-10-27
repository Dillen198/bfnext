//! An opt-in format version for tree values.
//!
//! yats stores values as positional bincode: fields in declaration order,
//! no names, no lengths for structs. Adding, removing or reordering a field
//! of a stored struct therefore makes every existing row undecodable (bfdb
//! had to move sessions to a new tree, `session_v2`, for exactly this).
//!
//! [`Envelope`] puts a small header in front of the value:
//!
//! ```text
//! MAGIC (6 bytes) | version (u16, big endian) | bincode(T)
//! ```
//!
//! and decodes three kinds of row:
//! - its own rows at `T::VERSION`, directly;
//! - its own rows at an older version, through [`Versioned::upgrade`];
//! - legacy rows with no header at all (written before the tree used
//!   `Envelope`), through `Versioned::upgrade(0, ..)`.
//!
//! So a tree can switch from `Tree<K, T>` to `Tree<K, Envelope<T>>` without
//! migrating anything: old rows keep decoding, and rows written from then on
//! carry their version. Nothing changes for a tree that does not opt in.
//!
//! `Envelope` must be the tree's value type itself (`Tree<K, Envelope<T>>`),
//! not nested inside another value, and it is only decodable by yats: its
//! `Deserialize` reads the whole row yats is decoding, which only yats
//! provides.

use crate::decode_plain;
use anyhow::{bail, Result};
use serde::{de, ser::SerializeTuple, Deserialize, Deserializer, Serialize, Serializer};
use std::{
    cell::Cell,
    ops::{Deref, DerefMut},
};

/// Starts every enveloped row. Chosen so no legacy row is likely to begin
/// with it: as a big-endian length or count it would be ~1.8e19, as an i64
/// about -7e16, as an f64 about -1.7e308.
pub const MAGIC: [u8; 6] = [0xFE, 0xFF, b'y', b'a', b't', b's'];

/// A value type with a format version, for use in an [`Envelope`].
pub trait Versioned: Serialize + de::DeserializeOwned {
    /// The version `Envelope` writes. Bump it whenever the encoding of the
    /// type changes, and teach `upgrade` the previous one.
    const VERSION: u16;

    /// Decode a row stored at an older `version`. `bytes` is that version's
    /// payload. Version 0 is a legacy row written before the tree used
    /// `Envelope`: plain bincode of the type as it was then. For a type whose
    /// legacy layout is its version 1 layout, that is `decode_plain(bytes)`.
    fn upgrade(version: u16, bytes: &[u8]) -> Result<Self>;
}

/// A tree value carrying its format version. See the module docs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Envelope<T>(pub T);

impl<T> Envelope<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> From<T> for Envelope<T> {
    fn from(t: T) -> Self {
        Self(t)
    }
}

impl<T> Deref for Envelope<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> DerefMut for Envelope<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T: Versioned> Serialize for Envelope<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        // bincode writes a tuple as its elements back to back, and a [u8; N]
        // as N bytes, so this is exactly MAGIC | version | payload.
        let mut t = serializer.serialize_tuple(3)?;
        t.serialize_element(&MAGIC)?;
        t.serialize_element(&T::VERSION)?;
        t.serialize_element(&self.0)?;
        t.end()
    }
}

impl<'de, T: Versioned> Deserialize<'de> for Envelope<T> {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        // In a MessagePack row (see `codec`) the envelope is an ordinary
        // tuple, and the payload is self-describing: an older version's
        // fields decode by name, so `upgrade` is not needed.
        if SELF_DESCRIBING.with(|s| s.get()) {
            let (magic, version, t): ([u8; 6], u16, T) = Deserialize::deserialize(d)?;
            if magic != MAGIC {
                return Err(de::Error::custom("not a yats::Envelope"));
            }
            if version > T::VERSION {
                return Err(de::Error::custom(format!(
                    "row is version {version}, newer than this build understands ({})",
                    T::VERSION
                )));
            }
            return Ok(Envelope(t));
        }
        // The deserializer cannot back up after reading a header that turns
        // out not to be one, so the row is decoded from its raw bytes instead.
        let row = ROW.with(|r| r.take()).ok_or_else(|| {
            de::Error::custom(
                "yats::Envelope can only be decoded by yats, as a tree's value type",
            )
        })?;
        // SAFETY: `ROW` is only ever set by `with_row`, to a slice that
        // `with_row`'s caller keeps borrowed for the whole call, and is
        // cleared before that call returns. We are inside it (it is what is
        // running this Deserialize impl), so the slice is still alive, and
        // `take` above means no other Envelope can reach it.
        let bytes = unsafe { std::slice::from_raw_parts(row.0, row.1) };
        decode::<T>(bytes).map(Envelope).map_err(de::Error::custom)
    }
}

/// Decode an enveloped (or legacy) row of `T`.
fn decode<T: Versioned>(bytes: &[u8]) -> Result<T> {
    match bytes.strip_prefix(&MAGIC[..]) {
        Some(rest) if rest.len() >= 2 => {
            let version = u16::from_be_bytes([rest[0], rest[1]]);
            let payload = &rest[2..];
            if version == T::VERSION {
                decode_plain(payload)
            } else if version < T::VERSION {
                T::upgrade(version, payload)
            } else {
                bail!(
                    "row is version {version}, newer than this build understands ({})",
                    T::VERSION
                )
            }
        }
        _ => T::upgrade(0, bytes),
    }
}

thread_local! {
    /// The row yats is decoding right now, for `Envelope`'s Deserialize.
    static ROW: Cell<Option<(*const u8, usize)>> = const { Cell::new(None) };
}

thread_local! {
    /// A MessagePack row is being decoded right now.
    static SELF_DESCRIBING: Cell<bool> = const { Cell::new(false) };
}

/// Run `f` (a MessagePack decode) with `Envelope` decoding as a plain tuple.
pub(crate) fn self_describing<R>(f: impl FnOnce() -> R) -> R {
    struct Restore(bool, Option<(*const u8, usize)>);
    impl Drop for Restore {
        fn drop(&mut self) {
            SELF_DESCRIBING.with(|s| s.set(self.0));
            ROW.with(|r| r.set(self.1));
        }
    }
    let _restore = Restore(
        SELF_DESCRIBING.with(|s| s.replace(true)),
        ROW.with(|r| r.replace(None)),
    );
    f()
}

/// Run `f` (a decode of `bytes`) with `bytes` available to `Envelope`.
pub(crate) fn with_row<R>(bytes: &[u8], f: impl FnOnce() -> R) -> R {
    struct Restore(Option<(*const u8, usize)>);
    impl Drop for Restore {
        fn drop(&mut self) {
            ROW.with(|r| r.set(self.0));
        }
    }
    let _restore = Restore(ROW.with(|r| r.replace(Some((bytes.as_ptr(), bytes.len())))));
    // a bincode row inside a MessagePack decode (it cannot nest, but be sure)
    let was = SELF_DESCRIBING.with(|s| s.replace(false));
    let r = f();
    SELF_DESCRIBING.with(|s| s.set(was));
    r
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{serialize, Tree};
    use serde_derive::{Deserialize, Serialize};

    /// A stored row the way bfdb's look: positional, no field names.
    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    struct GroupV1 {
        owner: u8,
        units: Vec<i64>,
    }

    /// The same row after a field was added -- the change that breaks a
    /// plain tree.
    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    struct Group {
        owner: u8,
        units: Vec<i64>,
        kind: Option<String>,
    }

    impl Versioned for GroupV1 {
        const VERSION: u16 = 1;
        fn upgrade(version: u16, bytes: &[u8]) -> Result<Self> {
            match version {
                0 => decode_plain(bytes),
                v => bail!("no version {v}"),
            }
        }
    }

    impl Versioned for Group {
        const VERSION: u16 = 2;
        fn upgrade(version: u16, bytes: &[u8]) -> Result<Self> {
            match version {
                // legacy and v1 rows have the v1 layout
                0 | 1 => {
                    let GroupV1 { owner, units } = decode_plain(bytes)?;
                    Ok(Group { owner, units, kind: None })
                }
                v => bail!("no version {v}"),
            }
        }
    }

    fn db() -> sled::Db {
        sled::Config::new().temporary(true).open().unwrap()
    }

    #[test]
    fn header_layout() {
        let v = GroupV1 { owner: 2, units: vec![7] };
        let bytes = serialize(&Envelope(v.clone())).unwrap();
        assert_eq!(&bytes[..6], &MAGIC);
        assert_eq!(&bytes[6..8], &[0, 1]);
        assert_eq!(&bytes[8..], &serialize(&v).unwrap()[..]);
    }

    #[test]
    fn a_tree_can_switch_to_envelope_and_then_change_its_type() {
        let db = db();
        let a = GroupV1 { owner: 1, units: vec![10, 11] };
        // 1. a plain tree, as every bfdb tree is today
        Tree::<u32, GroupV1>::open(&db, "groups").unwrap().insert(&1, &a).unwrap();
        // 2. the same tree opened with an envelope: the legacy row still
        //    decodes, and new rows carry version 1
        let t1 = Tree::<u32, Envelope<GroupV1>>::open(&db, "groups").unwrap();
        assert_eq!(t1.get(&1).unwrap().unwrap().0, a);
        let b = GroupV1 { owner: 2, units: vec![] };
        t1.insert(&2, &Envelope(b.clone())).unwrap();
        // 3. the type gains a field: legacy and v1 rows upgrade, v2 rows
        //    are read directly
        let t2 = Tree::<u32, Envelope<Group>>::open(&db, "groups").unwrap();
        let c = Group { owner: 3, units: vec![1], kind: Some("deployed".into()) };
        t2.insert(&3, &Envelope(c.clone())).unwrap();
        let rows: Vec<(u32, Group)> =
            t2.iter().map(|r| r.map(|(k, v)| (k, v.0)).unwrap()).collect();
        assert_eq!(
            rows,
            vec![
                (1, Group { owner: 1, units: vec![10, 11], kind: None }),
                (2, Group { owner: 2, units: vec![], kind: None }),
                (3, c),
            ]
        );
        // without the envelope, the same change makes the old rows unreadable
        let plain = Tree::<u32, Group>::open(&db, "plain").unwrap();
        plain.inner.insert(serialize(&1u32).unwrap(), serialize(&a).unwrap()).unwrap();
        assert!(plain.get(&1).is_err());
        // a build that predates a row's version refuses it, it does not guess
        assert!(t1.get(&3).is_err());
    }

    #[test]
    fn transactions_see_enveloped_rows() {
        let db = db();
        let t = Tree::<u32, Envelope<Group>>::open(&db, "t").unwrap();
        let g = Group { owner: 1, units: vec![], kind: None };
        t.insert(&1, &Envelope(g.clone())).unwrap();
        let got: Option<Envelope<Group>> = t
            .transaction(|tx| tx.get(&1))
            .map_err(|e| anyhow::anyhow!("{e:?}"))
            .unwrap();
        assert_eq!(got.map(|e| e.0), Some(g));
    }

    #[test]
    fn enveloped_rows_on_redb_are_messagepack() {
        let db = crate::Db::temporary_redb().unwrap();
        let t1 = Tree::<u32, Envelope<GroupV1>>::open(&db, "groups").unwrap();
        t1.insert(&1, &Envelope(GroupV1 { owner: 1, units: vec![5] })).unwrap();
        // the payload is self-describing, so the v2 type reads a v1 row by
        // field names
        let t2 = Tree::<u32, Envelope<Group>>::open(&db, "groups").unwrap();
        assert_eq!(t2.get(&1).unwrap().unwrap().0, Group { owner: 1, units: vec![5], kind: None });
        let c = Group { owner: 3, units: vec![], kind: Some("x".into()) };
        t2.insert(&2, &Envelope(c.clone())).unwrap();
        assert_eq!(t2.get(&2).unwrap().unwrap().0, c);
        // a build that predates the row's version still refuses it
        assert!(t1.get(&2).is_err());
    }

    #[test]
    fn envelope_is_only_decodable_by_yats() {
        use bincode::Options;
        let row = serialize(&Envelope(GroupV1 { owner: 1, units: vec![] })).unwrap();
        let opts = bincode::DefaultOptions::new().with_big_endian().with_fixint_encoding();
        // a plain bincode decode, without yats providing the row
        assert!(opts.deserialize::<Envelope<GroupV1>>(&row).is_err());
        // and yats itself is fine with it
        assert_eq!(crate::deserialize::<Envelope<GroupV1>>(&row).unwrap().0.owner, 1);
    }
}
