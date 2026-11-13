//! Storage-engine tests that need `StatsDb`'s insides: the same operations
//! on sled and redb, and converting a database built here from one to the
//! other. A child module of `db` (see the `#[path]` there), so it can drive
//! `add_stat` and write legacy rows the way older builds did.

use super::*;
use crate::storage::{self, Backend, MigrateOpts};
use bfprotocols::{cfg::UnitTags, shots::Shot, stats::Unit as StatUnit};
use dcso3::object::DcsOid;
use std::{collections::BTreeMap, marker::PhantomData};

fn scratch(name: &str) -> PathBuf {
    let dir =
        std::env::temp_dir().join(format!("bfdb-storage-{name}-{}", Uuid::new_v4()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// `StatsDb` on `backend` with the startup migrations, but none of the
/// background tasks `new` spawns: those hold the database open for the rest
/// of the test, and these tests reopen it.
fn open_quiet(path: &Path, backend: Backend) -> StatsDb {
    let cfg: InstanceCfg = serde_json::from_value(
        serde_json::json!({ "id": crate::instance::DEFAULT_INSTANCE }),
    )
    .unwrap();
    let db = storage::open(path, backend).unwrap();
    let t = StatsDb::open_trees(&HashMap::new(), db, Registry::single(cfg)).unwrap();
    t.check_schema_version().unwrap();
    t.migrate_legacy_cursors().unwrap();
    t.migrate_legacy_sessions().unwrap();
    t.migrate_session_keys().unwrap();
    t
}

fn ucid(n: u8) -> Ucid {
    format!("{n:02x}{}", "0123456789abcdef0123456789abcd").parse().unwrap()
}

fn t(secs: i64) -> DateTime<Utc> {
    Utc.timestamp_opt(1_760_000_000 + secs, 0).unwrap()
}

fn oid(id: u64) -> DcsOid<dcso3::unit::ClassUnit> {
    DcsOid { id, class: "Unit".into(), t: PhantomData }
}

fn dead(at: DateTime<Utc>, shooter: Ucid) -> Dead {
    let ai = Who::AI {
        unit: oid(5),
        side: Side::Red,
        gid: GroupId::from(2),
        uid: bfprotocols::db::group::UnitId::from(3),
        ucid: None,
    };
    let player = Who::Player {
        unit: oid(6),
        side: Side::Blue,
        ucid: shooter,
        slot: SlotId::Unit(42),
    };
    Dead {
        victim: ai.clone(),
        time: at,
        shots: vec![Shot {
            weapon_name: Some("AIM-120C".into()),
            weapon: None,
            shooter: player,
            shooter_typ: Some("F-16C_50".into()),
            target: ai,
            target_typ: "MiG-29S".into(),
            time: at,
            hit: true,
        }],
    }
}

/// A short round through the stats pipeline, plus the stores the API
/// writes directly. Deterministic: fixed times, ids from a fresh database.
fn drive(db: &StatsDb) {
    let inst = db.state(&InstanceId::from(crate::instance::DEFAULT_INSTANCE));
    let mut ctx = StatCtx::default();
    let (u1, u2) = (ucid(1), ucid(2));
    let obj = ObjectiveId::from(1);
    let stats = vec![
        Stat::NewRound { sortie: "campaign".into() },
        Stat::Register {
            name: "Alpha".into(),
            id: u1,
            side: Side::Blue,
            initial_points: 100,
        },
        Stat::Register {
            name: "Bravo".into(),
            id: u2,
            side: Side::Red,
            initial_points: 50,
        },
        Stat::Connect { id: u1, addr: "192.0.2.1".into(), name: "Alpha".into() },
        Stat::Objective {
            name: "Batumi".into(),
            id: obj,
            pos: LLPos { latitude: 41.6, longitude: 41.6, altitude: 10. },
            owner: Side::Red,
            kind: ObjectiveKind::Airbase,
        },
        Stat::Slot {
            id: u1,
            slot: SlotId::Unit(42),
            typ: Some(StatUnit {
                typ: Vehicle::from("F-16C_50"),
                tags: UnitTags(BitFlags::empty()),
            }),
        },
        Stat::Takeoff { id: u1 },
        Stat::Kill(dead(t(8), u1)),
        Stat::Capture { id: obj, by: smallvec::smallvec![u1], side: Side::Blue },
        Stat::EquipmentInventory { id: obj, item: "AIM-120C".into(), amount: 12 },
        Stat::LiquidInventory { id: obj, item: LiquidType::JetFuel, amount: 1000 },
        Stat::Points { id: u1, points: 25, reason: "kill".into() },
        Stat::PilotXp { id: u1, xp: 40, reason: "kill".into() },
        Stat::Land { id: u1 },
        Stat::Deslot { id: u1 },
        Stat::Disconnect { id: u1 },
        Stat::RoundEnd { winner: Some(Side::Blue) },
    ];
    for (i, st) in stats.into_iter().enumerate() {
        db.add_stat(&inst, &mut ctx, t(i as i64), st).unwrap();
    }
    db.create_session(
        Uuid::from_u128(1),
        SessionData {
            discord_id: "1234".into(),
            username: "alpha".into(),
            avatar: None,
            is_admin: true,
            expires: Utc::now() + chrono::Duration::days(30),
            roles: 3,
        },
    )
    .unwrap();
    db.ban_player(
        u2,
        BanRecord {
            name: "Bravo".into(),
            banned_at: t(100),
            until: None,
            reason: "test".into(),
        },
    )
    .unwrap();
    db.audit_put(&AuditEntry {
        at: t(101),
        who: "local:admin".into(),
        username: "admin".into(),
        action: "ban".into(),
        target: Some(u2.to_string()),
        instance: None,
    })
    .unwrap();
    db.wiki_save_page(
        "storage/test",
        WikiPage {
            title: "Storage".into(),
            section: "Ops".into(),
            order: 1,
            content: "redb".into(),
            updated_at: t(102),
            updated_by: "admin".into(),
        },
    )
    .unwrap();
    let rid = db.all_rounds().unwrap()[0].1;
    db.append_trail_point(rid, "unit-1", 1_760_000_050, 41.0, 42.0, 1000.0, 90.0)
        .unwrap();
    db.news_image_data
        .insert(&("default".into(), rid, "2026-10-27".into()), &(0..=255u8).collect())
        .unwrap();
}

/// What the read side of the API says about `drive`'s data, as text.
fn observe(db: &StatsDb) -> Vec<std::string::String> {
    let (u1, u2) = (ucid(1), ucid(2));
    let rounds = db.all_rounds().unwrap();
    let rid = rounds[0].1;
    let numbers: BTreeMap<u64, u32> =
        db.round_numbers().unwrap().into_iter().map(|(k, v)| (k.0, v)).collect();
    vec![
        format!("{rounds:?}"),
        format!("{numbers:?}"),
        format!("{:?}", db.pilot_leaderboard(None).unwrap()),
        format!("{:?}", db.pilot_leaderboard(Some(rid)).unwrap()),
        format!("{:?}", db.pilot_sorties(&u1).unwrap()),
        format!("{:?}", db.pilot_kills_for(&u1).unwrap()),
        format!("{:?}", db.pilot_detail(&u1).unwrap()),
        format!("{:?}", db.pilot_detail(&u2).unwrap()),
        format!("{:?}", db.recent_captures(rid, 10).unwrap()),
        format!("{:?}", db.recent_kills(rid, 10).unwrap()),
        format!("{:?}", db.objectives_for_round(rid).unwrap()),
        format!("{:?}", db.aircraft_usage(rid).unwrap()),
        format!("{:?}", db.list_admin_bans().unwrap()),
        format!(
            "{:?}",
            db.list_sessions()
                .unwrap()
                .into_iter()
                .map(|s| s.username)
                .collect::<Vec<_>>()
        ),
        format!("{:?}", db.get_session(Uuid::from_u128(1)).unwrap().map(|s| s.username)),
        format!("{:?}", db.audit_list(10, None).unwrap()),
        format!("{:?}", db.wiki_get_page("storage/test").unwrap()),
        format!("{:?}", db.get_trail_points(rid).unwrap()),
        format!("{:?}", db.xp_leaderboard(10).unwrap()),
        format!("{}", db.pilot_progression(&u1).unwrap()),
        format!("{:?}", db.news_image_get("default", rid, "2026-10-27").unwrap()),
    ]
}

/// Every key of every tree, per tree. `admin_audit` keys end in a random
/// u32 (two entries in the same microsecond); that part is left out.
fn all_keys(db: &Db) -> BTreeMap<std::string::String, Vec<Vec<u8>>> {
    db.tree_names()
        .unwrap()
        .into_iter()
        .map(|n| {
            let random_tail = if &n[..] == b"admin_audit" { 4 } else { 0 };
            let keys = db
                .open_tree(&n)
                .unwrap()
                .iter()
                .map(|r| {
                    let k = r.unwrap().0;
                    k[..k.len() - random_tail].to_vec()
                })
                .collect();
            (std::string::String::from_utf8_lossy(&n).into_owned(), keys)
        })
        .filter(|(_, k): &(_, Vec<Vec<u8>>)| !k.is_empty())
        .collect()
}

#[tokio::test(flavor = "multi_thread")]
async fn same_operations_same_results_on_both_engines() {
    let dir = scratch("parity");
    let mut seen = vec![];
    for backend in [Backend::Sled, Backend::Redb] {
        let db = open_quiet(&dir.join(format!("db-{backend}")), backend);
        assert_eq!(db.sled().engine().to_string(), backend.to_string());
        drive(&db);
        seen.push((observe(&db), all_keys(db.sled())));
    }
    let (sled, redb) = (&seen[0], &seen[1]);
    for (a, b) in sled.0.iter().zip(&redb.0) {
        assert_eq!(a, b);
    }
    assert_eq!(sled.1, redb.1, "the same keys in the same trees");
    assert!(sled.1.len() > 15, "drive touched {} trees", sled.1.len());
    let _ = std::fs::remove_dir_all(&dir);
}

/// A capture by AI troops carries the all-zero "no player" ucid. It is
/// credited to nobody: dropped at ingest, and from records saved before that.
#[tokio::test(flavor = "multi_thread")]
async fn ai_captures_credit_no_pilot() {
    let dir = scratch("ai-capture");
    let db = open_quiet(&dir.join("db"), Backend::Sled);
    let inst = db.state(&InstanceId::from(crate::instance::DEFAULT_INSTANCE));
    let mut ctx = StatCtx::default();
    let (u1, nobody) = (ucid(1), Ucid::default());
    let obj = ObjectiveId::from(1);
    let stats = vec![
        Stat::NewRound { sortie: "campaign".into() },
        Stat::Register {
            name: "Alpha".into(),
            id: u1,
            side: Side::Blue,
            initial_points: 100,
        },
        Stat::Objective {
            name: "HS42".into(),
            id: obj,
            pos: LLPos { latitude: 41.6, longitude: 41.6, altitude: 10. },
            owner: Side::Red,
            kind: ObjectiveKind::Airbase,
        },
        Stat::Capture { id: obj, by: smallvec::smallvec![nobody, u1], side: Side::Blue },
    ];
    for (i, st) in stats.into_iter().enumerate() {
        db.add_stat(&inst, &mut ctx, t(i as i64), st).unwrap();
    }
    let rid = db.all_rounds().unwrap()[0].1;
    // as an older build stored it
    let cid = CaptureId::new(&db.db).unwrap();
    db.captures
        .insert(
            &(rid, cid),
            &CaptureRecord {
                time: t(10),
                objective_name: "HS42".into(),
                side: Side::Red,
                by: smallvec::smallvec![nobody],
            },
        )
        .unwrap();
    let by: Vec<_> =
        db.recent_captures(rid, 10).unwrap().into_iter().map(|c| c.by.to_vec()).collect();
    assert_eq!(by, vec![vec![], vec![u1]]);
    let _ = std::fs::remove_dir_all(&dir);
}

/// Every row `drive` leaves, of every stored type, survives a change of
/// codec exactly, both ways: bincode rows (sled) re-encoded as MessagePack,
/// and MessagePack rows (redb) back as bincode.
#[tokio::test(flavor = "multi_thread")]
async fn every_stored_type_round_trips_on_both_engines() {
    let dir = scratch("roundtrip");
    for (from, to) in [(Backend::Sled, Backend::Redb), (Backend::Redb, Backend::Sled)] {
        let path = dir.join(format!("db-{from}"));
        let db = open_quiet(&path, from);
        drive(&db);
        let other = match to {
            Backend::Sled => Db::temporary_sled().unwrap(),
            Backend::Redb => Db::temporary_redb().unwrap(),
        };
        yats::convert::copy_all(db.sled(), &other, |_, _| ()).unwrap();
        let stats = StatsDb::reencode_store(&other, false).unwrap();
        let mut total = yats::ReencodeStats::default();
        for (tree, st) in &stats {
            assert_eq!(
                (st.undecodable, st.unverified),
                (0, 0),
                "{from} -> {to}: {tree}: {st:?}"
            );
            total += *st;
        }
        assert!(total.reencoded > 20, "{from} -> {to}: {total:?}");
        assert!(yats::convert::verify(db.sled(), &other, true, 1).unwrap().ok());
        // and the re-encoded copy reads back as the original does
        let reread =
            StatsDb::open_trees(&HashMap::new(), other, db.instances().clone()).unwrap();
        assert_eq!(observe(&reread), observe(&db), "{from} -> {to}");
    }
    let _ = std::fs::remove_dir_all(&dir);
}

/// sha-ish fingerprint of every file under `p`: names, sizes and contents.
fn fingerprint(p: &Path) -> Vec<(PathBuf, Vec<u8>)> {
    let mut out = vec![];
    for e in std::fs::read_dir(p).unwrap() {
        let e = e.unwrap();
        if e.file_type().unwrap().is_dir() {
            out.extend(fingerprint(&e.path()));
        } else {
            out.push((e.path(), std::fs::read(e.path()).unwrap()));
        }
    }
    out.sort();
    out
}

#[tokio::test(flavor = "multi_thread")]
async fn converts_a_sled_database_with_legacy_rows_and_rolls_back() {
    let dir = scratch("convert");
    let db_path = dir.join("db");
    let before = {
        let db = open_quiet(&db_path, Backend::Sled);
        drive(&db);
        observe(&db)
    };
    // Rows as older builds left them, written after the last start so no
    // startup migration has touched them.
    let legacy_id = Uuid::from_u128(77);
    {
        let sled = sled::open(&db_path).unwrap();
        // a pre-hash login, keyed by the raw cookie id
        let old: Tree<Uuid, LegacySessionData> =
            Tree::open(&sled, "auth_sessions").unwrap();
        old.insert(
            &legacy_id,
            &LegacySessionData {
                discord_id: "77".into(),
                username: "legacy".into(),
                avatar: None,
                is_admin: false,
                expires: Utc::now() + chrono::Duration::days(1),
            },
        )
        .unwrap();
        // a pre-session_v2 session row whose engine config no build can read
        let sess = sled.open_tree("session").unwrap();
        sess.insert(yats::serialize(&(RoundId(0), t(1))).unwrap(), vec![1u8, 2, 3])
            .unwrap();
        // a session_v2 row (engine config as JSON)
        let v2: Tree<(RoundId, DateTime<Utc>), SessionRec> =
            Tree::open(&sled, "session_v2").unwrap();
        v2.insert(
            &(RoundId(0), t(2)),
            &SessionRec { stop_time: Some(t(3)), end: None, cfg_json: "{}".into() },
        )
        .unwrap();
        // raw JSON trees (training range, replays) are copied as bytes
        sled.open_tree("range_records")
            .unwrap()
            .insert(b"inst\0k", &br#"{"id":"k"}"#[..])
            .unwrap();
        sled.flush().unwrap();
    }
    let source_files = fingerprint(&db_path);

    // a dry run leaves nothing behind
    let mut o = MigrateOpts {
        db_path: db_path.clone(),
        from: Backend::Sled,
        to: Backend::Redb,
        into: None,
        dry_run: true,
        keep_encoding: false,
    };
    let dry = storage::migrate(&o, |_| ()).unwrap();
    assert!(dry.last.ok());
    let target = storage::location(&db_path, Backend::Redb);
    assert!(!target.exists());
    let leftovers: Vec<_> =
        std::fs::read_dir(&dir).unwrap().map(|e| e.unwrap().file_name()).collect();
    assert_eq!(
        leftovers,
        vec![std::ffi::OsString::from("db")],
        "dry run left {leftovers:?}"
    );

    // the real thing
    o.dry_run = false;
    let rep = storage::migrate(&o, |_| ()).unwrap();
    println!("{}", storage::render(&o, &rep));
    assert!(rep.last.ok() && rep.exact.as_ref().unwrap().ok());
    assert!(target.is_file());
    let typed: BTreeMap<_, _> = rep.typed.iter().cloned().collect();
    assert_eq!(
        typed["session"].undecodable, 1,
        "the unreadable legacy session is kept, and reported"
    );
    assert_eq!(typed["auth_sessions"].reencoded, 1);
    assert_eq!(typed["session_v2"].reencoded, 1);
    assert_eq!(fingerprint(&db_path), source_files, "the sled database was not modified");

    // running it again only verifies
    let again = storage::migrate(&o, |_| ()).unwrap();
    assert!(again.verified_existing && again.last.ok());

    // bfdb on redb sees what it saw on sled, and the legacy login still works
    {
        let db = open_quiet(&db_path, Backend::Redb);
        let (mut after, mut before) = (observe(&db), before.clone());
        // the one difference: the legacy login, rekeyed at this start
        assert_eq!(after.remove(13), r#"["legacy", "alpha"]"#);
        before.remove(13);
        assert_eq!(after, before);
        assert_eq!(
            db.get_session(legacy_id).unwrap().map(|s| s.username).as_deref(),
            Some("legacy")
        );
        let raw = db.sled().open_tree("pilots").unwrap().first().unwrap().unwrap().1;
        assert!(yats::codec::is_msgpack(&raw), "rows were re-encoded");
        assert!(db
            .sled()
            .open_tree("range_records")
            .unwrap()
            .get(b"inst\0k")
            .unwrap()
            .is_some());
        let kid = KillId::new(db.sled()).unwrap();
        // a write after the cutover, which a rollback has to carry back
        db.ban_player(
            ucid(9),
            BanRecord {
                name: "Late".into(),
                banned_at: t(500),
                until: None,
                reason: "x".into(),
            },
        )
        .unwrap();
        assert!(kid.0 > 0);
    }

    // roll back: redb -> a new sled directory, never the original one
    let back = MigrateOpts { from: Backend::Redb, to: Backend::Sled, ..o.clone() };
    let rep = storage::migrate(&back, |_| ()).unwrap();
    assert!(rep.last.ok());
    assert_eq!(rep.target, dir.join("db.from-redb"));
    assert_eq!(fingerprint(&db_path), source_files);
    {
        let sled = Db::open_sled(&rep.target).unwrap();
        let raw = sled.open_tree("pilots").unwrap().first().unwrap().unwrap().1;
        assert!(!yats::codec::is_msgpack(&raw), "rolled back rows are bincode again");
        let bans: Tree<Ucid, BanRecord> = Tree::open(&sled, "admin_bans").unwrap();
        assert!(
            bans.get(&ucid(9)).unwrap().is_some(),
            "the post-cutover write came back"
        );
        // no id handed out on redb is handed out again
        let next = sled.generate_id().unwrap();
        let redb = Db::open_redb(&target).unwrap();
        assert!(next >= redb.next_id_for_copy().unwrap());
    }
    let _ = std::fs::remove_dir_all(&dir);
}

#[tokio::test(flavor = "multi_thread")]
async fn refuses_unsafe_conversions() {
    let dir = scratch("refuse");
    let db_path = dir.join("db");
    {
        let _db = open_quiet(&db_path, Backend::Sled);
        let o = MigrateOpts {
            db_path: db_path.clone(),
            from: Backend::Sled,
            to: Backend::Redb,
            into: None,
            dry_run: false,
            keep_encoding: false,
        };
        // bfdb (here, this process) still has it open
        let e = storage::migrate(&o, |_| ()).unwrap_err();
        assert!(format!("{e:#}").contains("open in another process"), "{e:#}");
        assert!(!storage::location(&db_path, Backend::Redb).exists());
    }
    let o = MigrateOpts {
        db_path: db_path.clone(),
        from: Backend::Redb,
        to: Backend::Sled,
        into: Some(db_path.clone()),
        dry_run: false,
        keep_encoding: false,
    };
    assert!(storage::migrate(&o, |_| ()).is_err(), "no redb database to convert");
    std::fs::write(storage::location(&db_path, Backend::Redb), b"").unwrap();
    let e = storage::migrate(&o, |_| ()).unwrap_err();
    assert!(format!("{e:#}").contains("live database"), "{e:#}");
    // --storage redb next to an unconverted sled database is refused
    std::fs::remove_file(storage::location(&db_path, Backend::Redb)).unwrap();
    let e = StatsDb::new_offline_on(&db_path, Backend::Redb).err().unwrap();
    assert!(format!("{e:#}").contains("Convert it first"), "{e:#}");
    let _ = std::fs::remove_dir_all(&dir);
}
