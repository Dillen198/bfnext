//! How values are encoded.
//!
//! Keys are always positional bincode (big endian, fixed-width integers):
//! their byte order is their logical order, which `scan_prefix` and `range`
//! rely on. Values are one of:
//!
//! - [`Codec::Bincode`]: the same positional bincode. No field names and no
//!   lengths for structs, so adding, removing or reordering a field of a
//!   stored struct makes every existing row undecodable. Every sled-backed
//!   tree writes this, as yats always has.
//! - [`Codec::MsgPack`]: `MAGIC` followed by MessagePack with structs as maps
//!   of field name to value. Self-describing: a field added to a type decodes
//!   from older rows as long as it is an `Option` or `#[serde(default)]`,
//!   removed fields are skipped, and field order does not matter. A
//!   redb-backed tree writes this.
//!
//! Decoding does not need to know which: a row starting with `MAGIC` is
//! MessagePack, anything else is bincode (and `Envelope` handling applies).
//! So a tree can hold both, which is exactly what a converted database does
//! until every row has been rewritten (see `Tree::reencode`).
//!
//! `MAGIC` is chosen like `Envelope`'s, so no bincode row is likely to start
//! with it: as a big-endian length or count it would be ~1.8e19, as an enum
//! tag 4278189933. A value type that is a bare byte array (`[u8; N]`, written
//! as N raw bytes) could collide with probability 2^-48 per row; bfdb stores
//! none.

use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Starts every MessagePack row.
pub const MAGIC: [u8; 6] = [0xFE, 0xFF, b'y', b'm', b'p', b'1'];

/// A value encoding. See the module docs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Codec {
    Bincode,
    MsgPack,
}

/// Encode a value as `codec`.
pub fn encode_value<T: Serialize>(v: &T, codec: Codec) -> Result<Vec<u8>> {
    match codec {
        Codec::Bincode => crate::serialize(v),
        Codec::MsgPack => {
            let mut out = MAGIC.to_vec();
            let mut ser = rmp_serde::Serializer::new(&mut out)
                .with_struct_map()
                // `Vec<u8>` and friends as one `bin`, not an array of
                // integers (images are stored this way).
                .with_bytes(rmp_serde::config::BytesMode::ForceIterables);
            v.serialize(&mut ser)?;
            Ok(out)
        }
    }
}

/// True if `row` was written as MessagePack.
pub fn is_msgpack(row: &[u8]) -> bool {
    row.starts_with(&MAGIC)
}

/// Decode a MessagePack payload (the row after `MAGIC`).
pub(crate) fn decode_msgpack<'a, T: Deserialize<'a>>(payload: &'a [u8]) -> Result<T> {
    crate::envelope::self_describing(|| Ok(rmp_serde::from_slice(payload)?))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_derive::{Deserialize, Serialize};

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    struct V1 {
        a: u32,
        name: String,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    enum Kind {
        Plain,
        Deployed { by: String },
    }

    /// V1 after the changes that break a bincode tree.
    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    struct V2 {
        name: String,
        a: u32,
        added: Option<Kind>,
        #[serde(default)]
        count: u64,
    }

    #[test]
    fn msgpack_rows_survive_field_changes() {
        let row = encode_value(&V1 { a: 7, name: "x".into() }, Codec::MsgPack).unwrap();
        assert!(is_msgpack(&row));
        let v2: V2 = crate::deserialize(&row).unwrap();
        assert_eq!(v2, V2 { name: "x".into(), a: 7, added: None, count: 0 });
        // and back: the extra fields are ignored by the old type
        let row = encode_value(
            &V2 { added: Some(Kind::Deployed { by: "me".into() }), ..v2 },
            Codec::MsgPack,
        )
        .unwrap();
        let v1: V1 = crate::deserialize(&row).unwrap();
        assert_eq!(v1, V1 { a: 7, name: "x".into() });
        // the same change on bincode rows fails, which is the point
        let row = encode_value(&V1 { a: 7, name: "x".into() }, Codec::Bincode).unwrap();
        assert!(crate::deserialize::<V2>(&row).is_err());
    }

    #[test]
    fn bytes_are_bin_and_floats_exact() {
        let img: Vec<u8> = (0..=255).collect();
        let row = encode_value(&img, Codec::MsgPack).unwrap();
        // MAGIC, bin16 marker, 2-byte length, the bytes
        assert_eq!(row.len(), 6 + 3 + 256);
        assert_eq!(crate::deserialize::<Vec<u8>>(&row).unwrap(), img);
        let f = (f64::NAN, -0.0f64, f32::INFINITY, 1e-300f64);
        let row = encode_value(&f, Codec::MsgPack).unwrap();
        let back: (f64, f64, f32, f64) = crate::deserialize(&row).unwrap();
        assert!(back.0.is_nan() && back.1.to_bits() == (-0.0f64).to_bits());
        assert_eq!((back.2, back.3), (f.2, f.3));
        // maps with non-string keys, which JSON cannot hold
        let m: std::collections::BTreeMap<(u32, u8), Vec<u64>> =
            [((1, 2), vec![3, 400, 5_000_000_000])].into();
        let row = encode_value(&m, Codec::MsgPack).unwrap();
        assert_eq!(
            crate::deserialize::<std::collections::BTreeMap<(u32, u8), Vec<u64>>>(&row)
                .unwrap(),
            m
        );
    }
}
