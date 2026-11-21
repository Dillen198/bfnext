use super::*;

#[test]
fn engines_parse_and_default_to_sled() {
    assert_eq!("redb".parse::<Backend>().unwrap(), Backend::Redb);
    assert_eq!(" Sled ".parse::<Backend>().unwrap(), Backend::Sled);
    assert!("sqlite".parse::<Backend>().is_err());
    assert_eq!(Backend::default(), Backend::Sled);
    // nothing selected yet in the test binary: sled
    assert_eq!(selected(), Backend::Sled);
}

#[test]
fn locations_keep_everything_beside_the_db() {
    let db = Path::new("C:/fe/db");
    assert_eq!(location(db, Backend::Sled), PathBuf::from("C:/fe/db"));
    assert_eq!(location(db, Backend::Redb), PathBuf::from("C:/fe/db.redb"));
    assert_eq!(default_target(db, Backend::Redb), PathBuf::from("C:/fe/db.redb"));
    // a rollback never writes into the live sled directory
    assert_eq!(default_target(db, Backend::Sled), PathBuf::from("C:/fe/db.from-redb"));
}

#[test]
fn in_use_detection() {
    let dir = std::env::temp_dir().join(format!("bfdb-inuse-{}", uuid::Uuid::new_v4()));
    let sled_path = dir.join("db");
    {
        let _db = Db::open_sled(&sled_path).unwrap();
        assert!(ensure_not_in_use(&sled_path, Backend::Sled).is_err());
    }
    // sled's threads let go of the lock shortly after the last handle drops
    let mut free = false;
    for _ in 0..50 {
        if ensure_not_in_use(&sled_path, Backend::Sled).is_ok() {
            free = true;
            break;
        }
        std::thread::sleep(Duration::from_millis(100));
    }
    assert!(free);
    let redb_path = dir.join("db.redb");
    {
        let _db = Db::open_redb(&redb_path).unwrap();
        assert!(ensure_not_in_use(&redb_path, Backend::Redb).is_err());
    }
    assert!(ensure_not_in_use(&redb_path, Backend::Redb).is_ok());
    // nothing there at all: nothing holds it
    assert!(ensure_not_in_use(&dir.join("nope"), Backend::Sled).is_ok());
    let _ = std::fs::remove_dir_all(&dir);
}
