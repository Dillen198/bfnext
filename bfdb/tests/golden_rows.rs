//! Golden bytes for the shared types bfdb stores in sled.
//!
//! bfdb keeps its rows as positional bincode through yats (no field names,
//! no struct lengths), so a stored type's encoding *is* its on-disk format:
//! reordering a field, adding one, or changing how a type serializes makes
//! every existing row of every tree holding it unreadable. Most of those
//! types come from dcso3 (dcso3-core) and bfprotocols, where nothing in the
//! crate itself says the layout is frozen. These tests do.
//!
//! bfdb is a binary crate, so its own row structs (Pilot, Sortie, Group, ...)
//! are not reachable from here; their parts that come from other crates are.
//!
//! If one of these fails, the change it caught breaks existing databases.
//! Revert it, or give the type a new tree or a `yats::Envelope` with an
//! upgrade path. Do not just update the expected bytes.
//!
//! On redb, values are MessagePack with field names instead
//! (`yats::codec`). That format tolerates added and removed fields, but not
//! renamed ones, renamed enum variants, or a type serializing differently;
//! the `msgpack_*` tests pin it the same way, and every value goes through
//! a real tree on both engines.

use bfprotocols::{
    cfg::Vehicle,
    db::{
        group::{GroupId, UnitId},
        objective::ObjectiveId,
    },
    shots::{Dead, Shot, Who},
    stats::DetectionSource,
};
use chrono::{TimeZone, Utc};
use dcso3::{
    coalition::Side,
    coord::LLPos,
    net::{SlotId, Ucid},
    object::DcsOid,
    perf::HistogramSer,
    warehouse::LiquidType,
};
use enumflags2::BitFlags;
use std::marker::PhantomData;

fn hex(b: &[u8]) -> String {
    b.iter().map(|b| format!("{b:02x}")).collect()
}

/// `value` encodes to exactly `expected`, and decodes back from it.
fn golden<T>(value: &T, expected: &str)
where
    T: serde::Serialize + serde::de::DeserializeOwned + std::fmt::Debug,
{
    let bytes = yats::serialize(value).unwrap();
    assert_eq!(hex(&bytes), expected, "encoding of {value:?} changed");
    let back: T = yats::deserialize(&bytes).unwrap();
    assert_eq!(hex(&yats::serialize(&back).unwrap()), expected, "{value:?} does not round trip");
}

const UCID: &str = "0123456789abcdef0123456789abcdef";

fn ucid() -> Ucid {
    UCID.parse().unwrap()
}

#[test]
fn side() {
    golden(&Side::Neutral, "00000000");
    golden(&Side::Red, "00000001");
    golden(&Side::Blue, "00000002");
}

#[test]
fn ucid_is_its_hex_string() {
    golden(&ucid(), &format!("{:016x}{}", UCID.len(), hex(UCID.as_bytes())));
}

#[test]
fn slot_id_is_its_string_form() {
    let s = |s: &str| format!("{:016x}{}", s.len(), hex(s.as_bytes()));
    golden(&SlotId::Unit(42), &s("42"));
    golden(&SlotId::Spectator, &s("0"));
    golden(&SlotId::MultiCrew(42, 2), &s("42_2"));
    golden(&SlotId::ArtilleryCommander(Side::Blue, 1), &s("artillery_commander_blue_1"));
}

#[test]
fn llpos() {
    golden(
        &LLPos { latitude: 1.5, longitude: -2.25, altitude: 100. },
        "3ff8000000000000c0020000000000004059000000000000",
    );
}

#[test]
fn liquid_type_keeps_its_variant_indices() {
    // also a key: (RoundId, ObjectiveId, LiquidType) in `liquids`
    golden(&LiquidType::JetFuel, "00000000");
    golden(&LiquidType::Avgas, "00000001");
    golden(&LiquidType::MW50, "00000002");
    golden(&LiquidType::Diesel, "00000003");
    // Unknown is appended last, so the four above never moved
    golden(&LiquidType::Unknown(9), "0000000409");
}

#[test]
fn strings_and_vehicles() {
    golden(&dcso3::String::from("F-16C"), "0000000000000005462d313643");
    golden(&Vehicle("F-16C_50".into()), "0000000000000008462d3136435f3530");
}

#[test]
fn ids_are_big_endian_i64() {
    golden(&GroupId::from(7), "0000000000000007");
    golden(&UnitId::from(-1), "ffffffffffffffff");
    golden(&ObjectiveId::from(258), "0000000000000102");
}

#[test]
fn detection_flags_are_one_byte() {
    golden(&BitFlags::<DetectionSource, u8>::empty(), "00");
    golden(&BitFlags::<DetectionSource, u8>::all(), "03");
}

fn unit(id: u64) -> DcsOid<dcso3::unit::ClassUnit> {
    DcsOid { id, class: "Unit".into(), t: PhantomData }
}

#[test]
fn dead_row() {
    // the value type of bfdb's `kills` tree
    let t = Utc.timestamp_opt(1_700_000_000, 0).unwrap();
    let ai = Who::AI {
        unit: unit(5),
        side: Side::Red,
        gid: GroupId::from(2),
        uid: UnitId::from(3),
        ucid: None,
    };
    let player = Who::Player {
        unit: unit(6),
        side: Side::Blue,
        ucid: ucid(),
        slot: SlotId::Unit(77),
    };
    let dead = Dead {
        victim: ai.clone(),
        time: t,
        shots: vec![Shot {
            weapon_name: Some("AIM-120C".into()),
            weapon: None,
            shooter: player,
            shooter_typ: Some("F-16C_50".into()),
            target: ai,
            target_typ: "MiG-29S".into(),
            time: t,
            hit: true,
        }],
    };
    golden(&dead, DEAD);
}

/// `dead_row`'s value as stored. Long, but every byte is the format.
const DEAD: &str = concat!(
    // victim: Who::AI { unit: DcsOid { id: 5, class: "Unit" }, side: Red,
    //   gid: 2, uid: 3, ucid: None }
    "00000000", "0000000000000005", "0000000000000004", "556e6974",
    "00000001", "0000000000000002", "0000000000000003", "00",
    // time: DateTime<Utc> as its RFC 3339 string
    "0000000000000014", "323032332d31312d31345432323a31333a32305a",
    // shots: 1 element
    "0000000000000001",
    //   weapon_name: Some("AIM-120C")
    "01", "0000000000000008", "41494d2d31323043",
    //   weapon: None
    "00",
    //   shooter: Who::Player { unit: 6, "Unit", side: Blue, ucid, slot: 77 }
    "00000001", "0000000000000006", "0000000000000004", "556e6974", "00000002",
    "0000000000000020", "3031323334353637383961626364656630313233343536373839616263646566",
    "0000000000000002", "3737",
    //   shooter_typ: Some("F-16C_50")
    "01", "0000000000000008", "462d3136435f3530",
    //   target: the victim again
    "00000000", "0000000000000005", "0000000000000004", "556e6974",
    "00000001", "0000000000000002", "0000000000000003", "00",
    //   target_typ: "MiG-29S"
    "0000000000000007", "4d69472d323953",
    //   time, hit
    "0000000000000014", "323032332d31312d31345432323a31333a32305a", "01",
);

/// A histogram as stored inside the perf rows (session records): a base64
/// string of hdrhistogram's V2 deflate format. Compressed bytes depend on
/// the deflate implementation, so this pins the decode of a stored value
/// rather than the exact encode.
#[test]
fn stored_histogram_still_decodes() {
    let mut h = HistogramSer::default();
    for v in [1_000u64, 2_000, 2_000, 1_000_000] {
        *h += v;
    }
    let bytes = yats::serialize(&h).unwrap();
    let back: HistogramSer = yats::deserialize(&bytes).unwrap();
    assert_eq!(back.len(), 4);
    assert_eq!(back.max(), h.max());
    let stored: HistogramSer = yats::deserialize(&hex_decode(STORED_HISTOGRAM)).unwrap();
    assert_eq!(stored.len(), 4);
    assert_eq!(stored.min(), 1_000);
    assert!(stored.max() >= 1_000_000 && stored.max() < 1_001_000);
}

fn hex_decode(s: &str) -> Vec<u8> {
    (0..s.len()).step_by(2).map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap()).collect()
}

/// yats bytes of the histogram in `stored_histogram_still_decodes` as written
/// in October 2026: a u64 length, then the base64 text
/// "HISTFAAAAD14nAEyAM3/HISTEwAAAAoAAAAAAAAAAwAAAAAAAAABAAAAADuaygA/8AAAAAAAAM8PAs0PBJ+PAQKI6gcU".
const STORED_HISTOGRAM: &str = concat!(
    "000000000000005c",
    "4849535446414141414431346e414579414d332f48495354457741414141",
    "6f41414141414141414141774141414141414141414241414141414475",
    "617967412f3841414141414141414d385041733050424a2b5041514b49",
    "36676355",
);

// -- MessagePack (redb) ----------------------------------------------------

/// `value` encodes as MessagePack to exactly `expected` (after yats' magic
/// header), decodes back from it, and survives a tree on both engines.
fn golden_mp<T>(value: &T, expected: &str)
where
    T: serde::Serialize + serde::de::DeserializeOwned + std::fmt::Debug + 'static,
{
    let bytes = yats::codec::encode_value(value, yats::Codec::MsgPack).unwrap();
    assert_eq!(&bytes[..6], &yats::codec::MAGIC);
    assert_eq!(hex(&bytes[6..]), expected, "MessagePack encoding of {value:?} changed");
    let back: T = yats::deserialize(&bytes).unwrap();
    assert_eq!(hex(&yats::codec::encode_value(&back, yats::Codec::MsgPack).unwrap()[6..]), expected);
    for engine in [yats::Engine::Sled, yats::Engine::Redb] {
        let db = yats::Db::temporary(engine).unwrap();
        let tree = yats::Tree::<u32, T>::open(&db, "golden").unwrap();
        tree.insert(&1, value).unwrap();
        let got = tree.get(&1).unwrap().unwrap();
        assert_eq!(
            hex(&yats::serialize(&got).unwrap()),
            hex(&yats::serialize(value).unwrap()),
            "{value:?} through a {engine} tree"
        );
    }
}

#[test]
fn msgpack_side() {
    golden_mp(&Side::Neutral, "a74e65757472616c");
    golden_mp(&Side::Blue, "a4426c7565");
}

#[test]
fn msgpack_ucid_and_slots() {
    golden_mp(&ucid(), "d9203031323334353637383961626364656630313233343536373839616263646566");
    golden_mp(&SlotId::Unit(42), "a23432");
    golden_mp(&SlotId::ArtilleryCommander(Side::Blue, 1), "ba617274696c6c6572795f636f6d6d616e6465725f626c75655f31");
}

#[test]
fn msgpack_llpos_and_liquids() {
    golden_mp(&LLPos { latitude: 1.5, longitude: -2.25, altitude: 100. }, "83a86c61746974756465cb3ff8000000000000a96c6f6e676974756465cbc002000000000000a8616c746974756465cb4059000000000000");
    golden_mp(&LiquidType::Diesel, "a644696573656c");
    golden_mp(&LiquidType::Unknown(9), "81a7556e6b6e6f776e09");
}

#[test]
fn msgpack_ids_vehicles_flags() {
    golden_mp(&GroupId::from(7), "07");
    golden_mp(&ObjectiveId::from(258), "cd0102");
    golden_mp(&Vehicle("F-16C_50".into()), "a8462d3136435f3530");
    golden_mp(&BitFlags::<DetectionSource, u8>::all(), "03");
}

#[test]
fn msgpack_dead_row() {
    let t = Utc.timestamp_opt(1_700_000_000, 0).unwrap();
    let ai = Who::AI { unit: unit(5), side: Side::Red, gid: GroupId::from(2), uid: UnitId::from(3), ucid: None };
    let player = Who::Player { unit: unit(6), side: Side::Blue, ucid: ucid(), slot: SlotId::Unit(77) };
    let dead = Dead {
        victim: ai.clone(),
        time: t,
        shots: vec![Shot {
            weapon_name: Some("AIM-120C".into()),
            weapon: None,
            shooter: player,
            shooter_typ: Some("F-16C_50".into()),
            target: ai,
            target_typ: "MiG-29S".into(),
            time: t,
            hit: true,
        }],
    };
    golden_mp(&dead, MP_DEAD);
}

const MP_DEAD: &str = concat!(
    "83a676696374696d81a2414985a4756e697482a2696405a5636c617373a4556e",
    "6974a473696465a3526564a367696402a375696403a475636964c0a474696d65",
    "b4323032332d31312d31345432323a31333a32305aa573686f74739188ab7765",
    "61706f6e5f6e616d65a841494d2d31323043a6776561706f6ec0a773686f6f74",
    "657281a6506c6179657284a4756e697482a2696406a5636c617373a4556e6974",
    "a473696465a4426c7565a475636964d920303132333435363738396162636465",
    "6630313233343536373839616263646566a4736c6f74a23737ab73686f6f7465",
    "725f747970a8462d3136435f3530a674617267657481a2414985a4756e697482",
    "a2696405a5636c617373a4556e6974a473696465a3526564a367696402a37569",
    "6403a475636964c0aa7461726765745f747970a74d69472d323953a474696d65",
    "b4323032332d31312d31345432323a31333a32305aa3686974c3",
);
