//! The storage engines under yats: sled 0.34 (the original) and redb.
//!
//! yats' typed [`Tree`](crate::Tree) sits on [`RawTree`], an ordered
//! byte-key/byte-value tree, and opens it from a [`Db`]. Both engines give the
//! same model: one keyspace per tree, keys compared as raw bytes (so yats'
//! big-endian tuple keys keep their logical order), point reads and writes,
//! ordered double-ended iteration, and an id generator. See
//! `docs/plans/storage-migration.md` for why redb and how a database moves.
//!
//! What differs, and how this module hides it:
//!
//! - **Commits.** sled buffers writes and flushes them every 500 ms; redb
//!   commits a transaction per write. Every write here is a redb commit with
//!   `Durability::None` (written, not fsynced), and a background thread makes
//!   them durable every 500 ms, as sled did. [`Db::flush`] does it at once.
//!   A crash loses at most the last half second either way.
//! - **Read-modify-write.** sled's `update_and_fetch` is a compare-and-swap
//!   loop that may call its closure more than once; [`RawTree::update`] is
//!   the same loop over redb, with the closure run outside the write lock so
//!   it may itself use the database.
//! - **Ids.** redb has no id generator; a `__yats_meta` table holds the next
//!   one. The converter seeds it above sled's, so no id is ever reissued.
//! - **Values.** A redb-backed tree writes values in a self-describing
//!   encoding ([`Codec::MsgPack`]); see `codec.rs`. Keys stay bincode on both.

use crate::codec::{self, Codec};
use anyhow::{anyhow, bail, Context, Result};
use redb::{ReadableDatabase, ReadableTable, ReadableTableMetadata, TableDefinition, TableHandle};
pub use sled::IVec;
use std::{
    collections::HashMap,
    fmt,
    ops::{Bound, RangeBounds},
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc, Mutex, Weak,
    },
    time::Duration,
};

/// Which engine a [`Db`] runs on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Engine {
    Sled,
    Redb,
}

impl fmt::Display for Engine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Engine::Sled => "sled",
            Engine::Redb => "redb",
        })
    }
}

/// redb keeps the next id here, under `NEXT_ID`. Not a tree: never listed by
/// [`Db::tree_names`], never copied as one.
const META_TABLE: &str = "__yats_meta";
const NEXT_ID: &[u8] = b"next_id";
/// How often redb's non-durable commits are made durable (sled's
/// `flush_every_ms` default).
const FLUSH_EVERY: Duration = Duration::from_millis(500);

type Def<'a> = TableDefinition<'a, &'static [u8], &'static [u8]>;

/// Re-encodes one tree's values into the database's codec, with the tree's
/// own types. Registered by `Tree::open`; see [`Db::reencode_all`].
pub(crate) type Reencoder =
    Arc<dyn Fn(&RawTree, bool) -> Result<crate::ReencodeStats> + Send + Sync>;

/// A database on either engine. Cheap to clone; clones share the database.
#[derive(Clone)]
pub struct Db {
    inner: DbInner,
    reencoders: Arc<Mutex<HashMap<String, Reencoder>>>,
}

#[derive(Clone)]
enum DbInner {
    Sled(sled::Db),
    Redb(Arc<RedbStore>),
}

impl fmt::Debug for Db {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            DbInner::Sled(_) => write!(f, "Db(sled)"),
            DbInner::Redb(s) => write!(f, "Db(redb {})", s.path.display()),
        }
    }
}

impl From<sled::Db> for Db {
    fn from(db: sled::Db) -> Self {
        Self { inner: DbInner::Sled(db), reencoders: Default::default() }
    }
}

impl Db {
    /// Open (or create) a sled database in the directory `path`.
    pub fn open_sled(path: impl AsRef<Path>) -> Result<Self> {
        Ok(sled::open(path.as_ref())
            .with_context(|| format!("opening sled database {}", path.as_ref().display()))?
            .into())
    }

    /// Open (or create) a redb database in the file `path`.
    pub fn open_redb(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        if let Some(parent) = path.parent().filter(|p| !p.as_os_str().is_empty()) {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("creating {}", parent.display()))?;
        }
        let db = redb::Database::create(path)
            .with_context(|| format!("opening redb database {}", path.display()))?;
        Ok(Self::from_redb(db, path.to_path_buf(), false))
    }

    /// A throwaway sled database, removed when dropped.
    pub fn temporary_sled() -> Result<Self> {
        Ok(sled::Config::new().temporary(true).open()?.into())
    }

    /// A throwaway redb database in the system temp folder, removed when the
    /// last handle to it is dropped.
    pub fn temporary_redb() -> Result<Self> {
        static N: AtomicU64 = AtomicU64::new(0);
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let path = std::env::temp_dir().join(format!(
            "yats-{}-{}-{nanos}.redb",
            std::process::id(),
            N.fetch_add(1, Ordering::Relaxed)
        ));
        let db = redb::Database::create(&path)?;
        Ok(Self::from_redb(db, path, true))
    }

    /// A throwaway database on `engine`.
    pub fn temporary(engine: Engine) -> Result<Self> {
        match engine {
            Engine::Sled => Self::temporary_sled(),
            Engine::Redb => Self::temporary_redb(),
        }
    }

    fn from_redb(db: redb::Database, path: PathBuf, temporary: bool) -> Self {
        let store = Arc::new(RedbStore {
            db: Some(db),
            path,
            temporary,
            dirty: AtomicBool::new(false),
        });
        let weak = Arc::downgrade(&store);
        let _ = std::thread::Builder::new()
            .name("yats-redb-flush".into())
            .spawn(move || flush_loop(weak));
        Self { inner: DbInner::Redb(store), reencoders: Default::default() }
    }

    pub fn engine(&self) -> Engine {
        match &self.inner {
            DbInner::Sled(_) => Engine::Sled,
            DbInner::Redb(_) => Engine::Redb,
        }
    }

    /// How a tree of this database encodes values it writes.
    pub fn codec(&self) -> Codec {
        match &self.inner {
            DbInner::Sled(_) => Codec::Bincode,
            DbInner::Redb(_) => Codec::MsgPack,
        }
    }

    /// The sled database underneath, for sled-only features (transactions,
    /// subscriptions, export/import).
    pub fn as_sled(&self) -> Option<&sled::Db> {
        match &self.inner {
            DbInner::Sled(db) => Some(db),
            DbInner::Redb(_) => None,
        }
    }

    /// The redb file, for a redb database.
    pub fn redb_path(&self) -> Option<&Path> {
        match &self.inner {
            DbInner::Sled(_) => None,
            DbInner::Redb(s) => Some(&s.path),
        }
    }

    /// Open (creating if needed) the raw tree `name`.
    pub fn open_tree(&self, name: impl AsRef<[u8]>) -> Result<RawTree> {
        let name = name.as_ref();
        match &self.inner {
            DbInner::Sled(db) => Ok(RawTree(RawInner::Sled(db.open_tree(name)?))),
            DbInner::Redb(store) => {
                let name = std::str::from_utf8(name)
                    .map_err(|_| anyhow!("redb tree names must be UTF-8"))?;
                if name == META_TABLE {
                    bail!("{META_TABLE} is reserved")
                }
                let t = RedbTree { store: store.clone(), name: Arc::from(name) };
                // Create the table now, so readers always find it.
                t.write(|_| Ok(()))?;
                Ok(RawTree(RawInner::Redb(t)))
            }
        }
    }

    /// Every tree in the database, by name.
    pub fn tree_names(&self) -> Result<Vec<Vec<u8>>> {
        match &self.inner {
            // sled always has a default tree; it only counts if it holds
            // something (bfdb never uses it)
            DbInner::Sled(db) => Ok(db
                .tree_names()
                .into_iter()
                .filter(|n| &n[..] != b"__sled__default" || !db.is_empty())
                .map(|n| n.to_vec())
                .collect()),
            DbInner::Redb(store) => {
                let tx = store.db().begin_read()?;
                let names = tx
                    .list_tables()?
                    .map(|t| t.name().as_bytes().to_vec())
                    .filter(|n| n != META_TABLE.as_bytes())
                    .collect();
                Ok(names)
            }
        }
    }

    /// Make every write so far durable. Returns bytes flushed (sled) or 0.
    pub fn flush(&self) -> Result<usize> {
        match &self.inner {
            DbInner::Sled(db) => Ok(db.flush()?),
            DbInner::Redb(store) => {
                store.durable_commit()?;
                Ok(0)
            }
        }
    }

    /// Bytes the database takes on disk.
    pub fn size_on_disk(&self) -> Result<u64> {
        match &self.inner {
            DbInner::Sled(db) => Ok(db.size_on_disk()?),
            DbInner::Redb(store) => Ok(std::fs::metadata(&store.path)?.len()),
        }
    }

    /// A new id, unique and increasing for the life of the database (also
    /// across restarts; sled may skip ahead after one, redb does not).
    pub fn generate_id(&self) -> Result<u64> {
        match &self.inner {
            DbInner::Sled(db) => Ok(db.generate_id()?),
            DbInner::Redb(store) => store.with_meta(|t| {
                let id = read_u64(t.get(NEXT_ID)?.as_ref().map(|g| g.value()))?;
                t.insert(NEXT_ID, &(id + 1).to_be_bytes()[..])?;
                Ok(id)
            }),
        }
    }

    /// The next id [`generate_id`](Self::generate_id) would hand out, for a
    /// conversion. On sled this *takes* one (sled cannot peek), so call it on
    /// a copy, never on a database that is going to stay in service.
    pub fn next_id_for_copy(&self) -> Result<u64> {
        match &self.inner {
            DbInner::Sled(db) => Ok(db.generate_id()?),
            DbInner::Redb(store) => {
                let tx = store.db().begin_read()?;
                match tx.open_table(meta_def()) {
                    Ok(t) => read_u64(t.get(NEXT_ID)?.as_ref().map(|g| g.value())),
                    Err(redb::TableError::TableDoesNotExist(_)) => Ok(0),
                    Err(e) => Err(e.into()),
                }
            }
        }
    }

    /// Make sure no id below `next` is handed out from now on.
    pub fn ensure_next_id(&self, next: u64) -> Result<()> {
        match &self.inner {
            DbInner::Sled(db) => {
                // sled has no setter; take ids until past it. Its ids grow by
                // about a million per restart, so this is millions of atomic
                // increments, not more -- but refuse something absurd.
                let mut id = db.generate_id()?;
                if next.saturating_sub(id) > 50_000_000_000 {
                    bail!("cannot advance sled's id generator from {id} to {next}")
                }
                while id + 1 < next {
                    id = db.generate_id()?;
                }
                Ok(())
            }
            DbInner::Redb(store) => store.with_meta(|t| {
                let cur = read_u64(t.get(NEXT_ID)?.as_ref().map(|g| g.value()))?;
                if cur < next {
                    t.insert(NEXT_ID, &next.to_be_bytes()[..])?;
                }
                Ok(())
            }),
        }
    }

    pub(crate) fn register_reencoder(&self, name: &str, r: Reencoder) {
        self.reencoders.lock().unwrap_or_else(|e| e.into_inner()).insert(name.to_string(), r);
    }

    /// Re-encode every tree opened through this handle with `Tree::open`
    /// (and so with known types) into the database's codec -- for a redb
    /// database, from legacy bincode to MessagePack. Rows already in it are
    /// skipped; rows that do not decode as the tree's type, or that would not
    /// survive the change exactly, are left as they are and counted. With
    /// `check_only`, nothing is written: every row is just decoded.
    pub fn reencode_all(&self, check_only: bool) -> Result<Vec<(String, crate::ReencodeStats)>> {
        let mut all: Vec<(String, Reencoder)> = self
            .reencoders
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        all.sort_by(|a, b| a.0.cmp(&b.0));
        let mut out = vec![];
        for (name, r) in all {
            let tree = self.open_tree(&name)?;
            let stats = r(&tree, check_only).with_context(|| format!("re-encoding tree {name}"))?;
            out.push((name, stats));
        }
        self.flush()?;
        Ok(out)
    }

    /// Copy every tree but `exclude` into a new redb file at `to`, from one
    /// read transaction: a point-in-time snapshot of the whole database while
    /// writes go on. redb only.
    pub fn snapshot_redb(&self, to: &Path, exclude: &[&str]) -> Result<crate::convert::CopyStats> {
        if self.engine() != Engine::Redb {
            bail!("snapshot_redb on a {} database", self.engine())
        }
        let DbInner::Redb(store) = &self.inner else { unreachable!() };
        let out = Db::open_redb(to)?;
        let mut stats = crate::convert::CopyStats::default();
        let tx = store.db().begin_read()?;
        for handle in tx.list_tables()? {
            let name = handle.name().to_string();
            if name == META_TABLE || exclude.contains(&name.as_str()) {
                continue;
            }
            let src = tx.open_table(Def::new(&name))?;
            let dst = out.open_tree(&name)?;
            let mut n = 0u64;
            let mut batch = crate::convert::Chunk::default();
            for r in src.range::<&[u8]>(..)? {
                let (k, v) = r?;
                batch.push(k.value(), v.value());
                n += 1;
                if batch.is_full() {
                    dst.insert_many(batch.take())?;
                }
            }
            dst.insert_many(batch.take())?;
            stats.trees.push((name, n));
            stats.pairs += n;
        }
        if let Ok(meta) = tx.open_table(meta_def()) {
            let next = read_u64(meta.get(NEXT_ID)?.as_ref().map(|g| g.value()))?;
            out.ensure_next_id(next)?;
        }
        drop(tx);
        out.flush()?;
        Ok(stats)
    }
}

/// Compact the redb file at `path`, giving back the space a bulk load or a
/// rewrite of every row leaves free inside it. Nothing may have the file
/// open. Returns the file's size before and after.
pub fn compact_redb_file(path: &Path) -> Result<(u64, u64)> {
    let before = std::fs::metadata(path)?.len();
    {
        let mut db = redb::Database::create(path)
            .with_context(|| format!("opening {} to compact it", path.display()))?;
        // each pass moves what it can; stop when one finds nothing to do
        for _ in 0..8 {
            if !db.compact()? {
                break;
            }
        }
    }
    Ok((before, std::fs::metadata(path)?.len()))
}

fn meta_def() -> Def<'static> {
    Def::new(META_TABLE)
}

fn read_u64(v: Option<&[u8]>) -> Result<u64> {
    match v {
        None => Ok(0),
        Some(b) => Ok(u64::from_be_bytes(
            b.try_into().map_err(|_| anyhow!("corrupt {META_TABLE} entry"))?,
        )),
    }
}

/// Anything a typed tree can be opened from: a yats [`Db`] or, as before, a
/// bare `sled::Db`.
pub trait OpenTree {
    #[doc(hidden)]
    fn open_typed<K: crate::KV + 'static, V: crate::KV + 'static>(&self, name: &str)
        -> Result<RawTree>;
}

impl OpenTree for sled::Db {
    fn open_typed<K: crate::KV + 'static, V: crate::KV + 'static>(
        &self,
        name: &str,
    ) -> Result<RawTree> {
        Ok(RawTree(RawInner::Sled(self.open_tree(name)?)))
    }
}

impl OpenTree for Db {
    fn open_typed<K: crate::KV + 'static, V: crate::KV + 'static>(
        &self,
        name: &str,
    ) -> Result<RawTree> {
        let raw = self.open_tree(name)?;
        self.register_reencoder(
            name,
            Arc::new(|raw: &RawTree, check_only: bool| {
                crate::Tree::<K, V>::from_raw(raw.clone()).reencode(check_only)
            }),
        );
        Ok(raw)
    }
}

// ── redb store ───────────────────────────────────────────────────────────────

struct RedbStore {
    /// Always `Some`; an `Option` only so `Drop` can close the file before
    /// removing a temporary one.
    db: Option<redb::Database>,
    path: PathBuf,
    temporary: bool,
    /// A non-durable commit happened since the last durable one.
    dirty: AtomicBool,
}

impl RedbStore {
    fn db(&self) -> &redb::Database {
        self.db.as_ref().expect("redb database is open")
    }

    fn begin_write(&self) -> Result<redb::WriteTransaction> {
        let mut tx = self.db().begin_write()?;
        tx.set_durability(redb::Durability::None)?;
        Ok(tx)
    }

    fn commit(&self, tx: redb::WriteTransaction) -> Result<()> {
        tx.commit()?;
        self.dirty.store(true, Ordering::Release);
        Ok(())
    }

    fn durable_commit(&self) -> Result<()> {
        self.dirty.store(false, Ordering::Release);
        // an empty commit at the default (Immediate) durability persists
        // every non-durable commit before it
        self.db().begin_write()?.commit()?;
        Ok(())
    }

    fn with_meta<R>(&self, f: impl FnOnce(&mut redb::Table<&'static [u8], &'static [u8]>) -> Result<R>) -> Result<R> {
        let tx = self.begin_write()?;
        let r = {
            let mut t = tx.open_table(meta_def())?;
            f(&mut t)?
        };
        self.commit(tx)?;
        Ok(r)
    }
}

impl Drop for RedbStore {
    fn drop(&mut self) {
        if self.dirty.load(Ordering::Acquire) {
            let _ = self.durable_commit();
        }
        drop(self.db.take());
        if self.temporary {
            let _ = std::fs::remove_file(&self.path);
        }
    }
}

fn flush_loop(store: Weak<RedbStore>) {
    loop {
        std::thread::sleep(FLUSH_EVERY);
        let Some(s) = store.upgrade() else { return };
        if s.dirty.load(Ordering::Acquire) {
            if let Err(e) = s.durable_commit() {
                // stays dirty; the next pass or Db::flush tries again
                s.dirty.store(true, Ordering::Release);
                eprintln!("yats: making redb commits durable failed: {e}");
            }
        }
    }
}

#[derive(Clone)]
struct RedbTree {
    store: Arc<RedbStore>,
    name: Arc<str>,
}

impl RedbTree {
    fn def(&self) -> Def<'_> {
        Def::new(&self.name)
    }

    fn write<R>(&self, f: impl FnOnce(&mut redb::Table<&'static [u8], &'static [u8]>) -> Result<R>) -> Result<R> {
        let tx = self.store.begin_write()?;
        let r = {
            let mut t = tx.open_table(self.def())?;
            f(&mut t)?
        };
        self.store.commit(tx)?;
        Ok(r)
    }

    /// The table in a fresh read transaction, or `None` if it does not exist.
    fn read(&self) -> Result<Option<redb::ReadOnlyTable<&'static [u8], &'static [u8]>>> {
        let tx = self.store.db().begin_read()?;
        match tx.open_table(self.def()) {
            Ok(t) => Ok(Some(t)),
            Err(redb::TableError::TableDoesNotExist(_)) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    fn range(&self, lo: Bound<&[u8]>, hi: Bound<&[u8]>) -> Result<RawIter> {
        if bounds_empty(lo, hi) {
            return Ok(RawIter(IterInner::Empty));
        }
        Ok(match self.read()? {
            None => RawIter(IterInner::Empty),
            Some(t) => RawIter(IterInner::Redb(t.range::<&[u8]>((lo, hi))?)),
        })
    }
}

/// True for bounds no key can fall between. BTree-style ranges panic on
/// these; sled just yields nothing, and so do we.
fn bounds_empty(lo: Bound<&[u8]>, hi: Bound<&[u8]>) -> bool {
    match (lo, hi) {
        (Bound::Included(a), Bound::Included(b)) => a > b,
        (Bound::Included(a), Bound::Excluded(b))
        | (Bound::Excluded(a), Bound::Included(b))
        | (Bound::Excluded(a), Bound::Excluded(b)) => a >= b,
        _ => false,
    }
}

/// The first key after every key starting with `prefix`, if there is one.
fn prefix_end(prefix: &[u8]) -> Bound<Vec<u8>> {
    let mut end = prefix.to_vec();
    while let Some(last) = end.pop() {
        if last < 0xff {
            end.push(last + 1);
            return Bound::Excluded(end);
        }
    }
    Bound::Unbounded
}

// ── raw trees ────────────────────────────────────────────────────────────────

/// One ordered keyspace of raw bytes, on either engine. The method names and
/// shapes are sled's (`IVec` values, `AsRef<[u8]>` keys, `Into<IVec>` values),
/// so code written against `sled::Tree` moves over by changing its import.
#[derive(Clone)]
pub struct RawTree(RawInner);

#[derive(Clone)]
enum RawInner {
    Sled(sled::Tree),
    Redb(RedbTree),
}

impl fmt::Debug for RawTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            RawInner::Sled(t) => write!(f, "RawTree(sled {:?})", String::from_utf8_lossy(&t.name())),
            RawInner::Redb(t) => write!(f, "RawTree(redb {:?})", t.name),
        }
    }
}

/// What [`RawTree::apply`] does to one key.
pub enum Op {
    Insert(Vec<u8>, Vec<u8>),
    Remove(Vec<u8>),
}

impl RawTree {
    pub fn engine(&self) -> Engine {
        match &self.0 {
            RawInner::Sled(_) => Engine::Sled,
            RawInner::Redb(_) => Engine::Redb,
        }
    }

    /// How values written to this tree are encoded.
    pub fn codec(&self) -> Codec {
        match &self.0 {
            RawInner::Sled(_) => Codec::Bincode,
            RawInner::Redb(_) => Codec::MsgPack,
        }
    }

    pub fn as_sled(&self) -> Option<&sled::Tree> {
        match &self.0 {
            RawInner::Sled(t) => Some(t),
            RawInner::Redb(_) => None,
        }
    }

    pub fn name(&self) -> IVec {
        match &self.0 {
            RawInner::Sled(t) => t.name(),
            RawInner::Redb(t) => IVec::from(t.name.as_bytes()),
        }
    }

    pub fn get<K: AsRef<[u8]>>(&self, key: K) -> Result<Option<IVec>> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.get(key)?),
            RawInner::Redb(t) => match t.read()? {
                None => Ok(None),
                Some(table) => Ok(table.get(key.as_ref())?.map(|g| IVec::from(g.value()))),
            },
        }
    }

    pub fn contains_key<K: AsRef<[u8]>>(&self, key: K) -> Result<bool> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.contains_key(key)?),
            RawInner::Redb(_) => Ok(self.get(key)?.is_some()),
        }
    }

    /// Set `key` to `value`, returning the value it had.
    pub fn insert<K: AsRef<[u8]>, V: Into<IVec>>(&self, key: K, value: V) -> Result<Option<IVec>> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.insert(key, value)?),
            RawInner::Redb(t) => {
                let value: IVec = value.into();
                t.write(|table| {
                    Ok(table.insert(key.as_ref(), value.as_ref())?.map(|g| IVec::from(g.value())))
                })
            }
        }
    }

    /// Remove `key`, returning the value it had.
    pub fn remove<K: AsRef<[u8]>>(&self, key: K) -> Result<Option<IVec>> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.remove(key)?),
            RawInner::Redb(t) => {
                t.write(|table| Ok(table.remove(key.as_ref())?.map(|g| IVec::from(g.value()))))
            }
        }
    }

    /// Apply every op at once (one sled batch, one redb transaction).
    pub fn apply(&self, ops: Vec<Op>) -> Result<()> {
        if ops.is_empty() {
            return Ok(());
        }
        match &self.0 {
            RawInner::Sled(t) => {
                let mut b = sled::Batch::default();
                for op in ops {
                    match op {
                        Op::Insert(k, v) => b.insert(k, v),
                        Op::Remove(k) => b.remove(k),
                    }
                }
                Ok(t.apply_batch(b)?)
            }
            RawInner::Redb(t) => t.write(|table| {
                for op in &ops {
                    match op {
                        Op::Insert(k, v) => {
                            table.insert(&k[..], &v[..])?;
                        }
                        Op::Remove(k) => {
                            table.remove(&k[..])?;
                        }
                    }
                }
                Ok(())
            }),
        }
    }

    /// Insert many pairs at once.
    pub fn insert_many(&self, pairs: Vec<(Vec<u8>, Vec<u8>)>) -> Result<()> {
        self.apply(pairs.into_iter().map(|(k, v)| Op::Insert(k, v)).collect())
    }

    /// Replace each `(key, old, new)` whose value is still exactly `old`, in
    /// one transaction. Returns how many were replaced.
    pub fn replace_unchanged(&self, rows: Vec<(Vec<u8>, Vec<u8>, Vec<u8>)>) -> Result<usize> {
        if rows.is_empty() {
            return Ok(0);
        }
        match &self.0 {
            RawInner::Sled(t) => {
                let mut n = 0;
                for (k, old, new) in rows {
                    if t.compare_and_swap(k, Some(old), Some(new))?.is_ok() {
                        n += 1;
                    }
                }
                Ok(n)
            }
            RawInner::Redb(t) => t.write(|table| {
                let mut n = 0;
                for (k, old, new) in &rows {
                    let same = table.get(&k[..])?.is_some_and(|g| g.value() == &old[..]);
                    if same {
                        table.insert(&k[..], &new[..])?;
                        n += 1;
                    }
                }
                Ok(n)
            }),
        }
    }

    /// Compare and swap: set `key` to `new` (or remove it, for `None`) only
    /// if its value is `old` (or absent, for `None`). On a mismatch, returns
    /// the current value.
    pub fn compare_and_swap<K: AsRef<[u8]>>(
        &self,
        key: K,
        old: Option<&[u8]>,
        new: Option<Vec<u8>>,
    ) -> Result<std::result::Result<(), Option<IVec>>> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.compare_and_swap(key, old, new)?.map_err(|e| e.current)),
            RawInner::Redb(t) => t.write(|table| {
                let cur = table.get(key.as_ref())?.map(|g| IVec::from(g.value()));
                if cur.as_deref() != old {
                    return Ok(Err(cur));
                }
                match &new {
                    Some(v) => {
                        table.insert(key.as_ref(), &v[..])?;
                    }
                    None => {
                        table.remove(key.as_ref())?;
                    }
                }
                Ok(Ok(()))
            }),
        }
    }

    /// Atomic read-modify-write: `f` gets the current value and returns the
    /// new one (`None` removes the key). Like sled's, `f` may be called more
    /// than once if another writer gets in first, so it must not have side
    /// effects beyond computing the value. Returns (old, new).
    pub fn update<K: AsRef<[u8]>>(
        &self,
        key: K,
        mut f: impl FnMut(Option<&[u8]>) -> Option<Vec<u8>>,
    ) -> Result<(Option<IVec>, Option<IVec>)> {
        let key = key.as_ref();
        match &self.0 {
            RawInner::Sled(t) => {
                let mut old = None;
                let new = t.update_and_fetch(key, |cur| {
                    old = cur.map(IVec::from);
                    f(cur)
                })?;
                Ok((old, new))
            }
            RawInner::Redb(_) => loop {
                let cur = self.get(key)?;
                let new = f(cur.as_deref());
                if new.as_deref() == cur.as_deref() {
                    return Ok((cur.clone(), cur));
                }
                if self.compare_and_swap(key, cur.as_deref(), new.clone())?.is_ok() {
                    return Ok((cur, new.map(IVec::from)));
                }
            },
        }
    }

    pub fn iter(&self) -> RawIter {
        match &self.0 {
            RawInner::Sled(t) => RawIter(IterInner::Sled(t.iter())),
            RawInner::Redb(t) => t
                .range(Bound::Unbounded, Bound::Unbounded)
                .unwrap_or_else(|e| RawIter(IterInner::Failed(Some(e)))),
        }
    }

    pub fn range<K: AsRef<[u8]>, R: RangeBounds<K>>(&self, range: R) -> RawIter {
        match &self.0 {
            RawInner::Sled(t) => RawIter(IterInner::Sled(t.range(range))),
            RawInner::Redb(t) => {
                fn b<K: AsRef<[u8]>>(b: Bound<&K>) -> Bound<&[u8]> {
                    b.map(|k| k.as_ref())
                }
                t.range(b(range.start_bound()), b(range.end_bound()))
                    .unwrap_or_else(|e| RawIter(IterInner::Failed(Some(e))))
            }
        }
    }

    pub fn scan_prefix<P: AsRef<[u8]>>(&self, prefix: P) -> RawIter {
        match &self.0 {
            RawInner::Sled(t) => RawIter(IterInner::Sled(t.scan_prefix(prefix))),
            RawInner::Redb(t) => {
                let end = prefix_end(prefix.as_ref());
                t.range(Bound::Included(prefix.as_ref()), end.as_ref().map(|e| &e[..]))
                    .unwrap_or_else(|e| RawIter(IterInner::Failed(Some(e))))
            }
        }
    }

    pub fn first(&self) -> Result<Option<(IVec, IVec)>> {
        self.iter().next().transpose()
    }

    pub fn last(&self) -> Result<Option<(IVec, IVec)>> {
        self.iter().next_back().transpose()
    }

    /// The pair with the greatest key below `key`.
    pub fn get_lt<K: AsRef<[u8]>>(&self, key: K) -> Result<Option<(IVec, IVec)>> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.get_lt(key)?),
            RawInner::Redb(_) => self.range(..key.as_ref()).next_back().transpose(),
        }
    }

    /// The pair with the smallest key above `key`.
    pub fn get_gt<K: AsRef<[u8]>>(&self, key: K) -> Result<Option<(IVec, IVec)>> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.get_gt(key)?),
            RawInner::Redb(_) => self
                .range::<&[u8], _>((Bound::Excluded(key.as_ref()), Bound::Unbounded))
                .next()
                .transpose(),
        }
    }

    pub fn pop_min(&self) -> Result<Option<(IVec, IVec)>> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.pop_min()?),
            RawInner::Redb(t) => t.write(|table| {
                Ok(table.pop_first()?.map(|(k, v)| (IVec::from(k.value()), IVec::from(v.value()))))
            }),
        }
    }

    pub fn pop_max(&self) -> Result<Option<(IVec, IVec)>> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.pop_max()?),
            RawInner::Redb(t) => t.write(|table| {
                Ok(table.pop_last()?.map(|(k, v)| (IVec::from(k.value()), IVec::from(v.value()))))
            }),
        }
    }

    pub fn len(&self) -> usize {
        match &self.0 {
            RawInner::Sled(t) => t.len(),
            RawInner::Redb(t) => t
                .read()
                .ok()
                .flatten()
                .and_then(|table| table.len().ok())
                .unwrap_or(0) as usize,
        }
    }

    pub fn is_empty(&self) -> bool {
        match &self.0 {
            RawInner::Sled(t) => t.is_empty(),
            RawInner::Redb(_) => self.first().ok().flatten().is_none(),
        }
    }

    /// Remove every pair.
    pub fn clear(&self) -> Result<()> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.clear()?),
            RawInner::Redb(t) => t.write(|table| Ok(table.retain(|_, _| false)?)),
        }
    }

    /// Make writes durable (the whole database's, on redb as on sled).
    pub fn flush(&self) -> Result<usize> {
        match &self.0 {
            RawInner::Sled(t) => Ok(t.flush()?),
            RawInner::Redb(t) => {
                t.store.durable_commit()?;
                Ok(0)
            }
        }
    }

    /// Value bytes in this tree's codec.
    pub(crate) fn encode_value<V: serde::Serialize>(&self, v: &V) -> Result<Vec<u8>> {
        codec::encode_value(v, self.codec())
    }
}

/// An ordered, double-ended iterator over raw pairs. On redb it reads one
/// snapshot (a read transaction, held until the iterator is dropped).
pub struct RawIter(IterInner);

enum IterInner {
    Sled(sled::Iter),
    Redb(redb::Range<'static, &'static [u8], &'static [u8]>),
    Empty,
    /// Opening the range failed: yield the error once.
    Failed(Option<anyhow::Error>),
}

impl RawIter {
    pub fn from_sled(iter: sled::Iter) -> Self {
        RawIter(IterInner::Sled(iter))
    }
}

type Pair = (IVec, IVec);

fn redb_pair(
    r: std::result::Result<
        (redb::AccessGuard<'static, &'static [u8]>, redb::AccessGuard<'static, &'static [u8]>),
        redb::StorageError,
    >,
) -> Result<Pair> {
    let (k, v) = r?;
    Ok((IVec::from(k.value()), IVec::from(v.value())))
}

impl Iterator for RawIter {
    type Item = Result<Pair>;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            IterInner::Sled(i) => i.next().map(|r| r.map_err(anyhow::Error::from)),
            IterInner::Redb(i) => i.next().map(redb_pair),
            IterInner::Empty => None,
            IterInner::Failed(e) => e.take().map(Err),
        }
    }
}

impl DoubleEndedIterator for RawIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            IterInner::Sled(i) => i.next_back().map(|r| r.map_err(anyhow::Error::from)),
            IterInner::Redb(i) => i.next_back().map(redb_pair),
            IterInner::Empty => None,
            IterInner::Failed(e) => e.take().map(Err),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn both(f: impl Fn(Db)) {
        for e in [Engine::Sled, Engine::Redb] {
            f(Db::temporary(e).unwrap());
        }
    }

    fn keys(i: RawIter) -> Vec<Vec<u8>> {
        i.map(|r| r.unwrap().0.to_vec()).collect()
    }

    #[test]
    fn ordered_iteration_matches() {
        both(|db| {
            let t = db.open_tree("t").unwrap();
            for k in [&b"b"[..], b"a", b"ab", b"\xff", b"\xff\xff", b"\x00", b"abc"] {
                t.insert(k, k).unwrap();
            }
            assert_eq!(
                keys(t.iter()),
                vec![b"\x00".to_vec(), b"a".to_vec(), b"ab".to_vec(), b"abc".to_vec(), b"b".to_vec(), b"\xff".to_vec(), b"\xff\xff".to_vec()],
                "{db:?}"
            );
            assert_eq!(keys(t.scan_prefix(b"ab")), vec![b"ab".to_vec(), b"abc".to_vec()]);
            assert_eq!(keys(t.scan_prefix(b"\xff")), vec![b"\xff".to_vec(), b"\xff\xff".to_vec()]);
            assert_eq!(keys(t.range(&b"a"[..]..&b"b"[..])), vec![b"a".to_vec(), b"ab".to_vec(), b"abc".to_vec()]);
            assert_eq!(t.range(&b"a"[..]..&b"b"[..]).rev().count(), 3);
            // an inverted range is empty, not a panic
            assert!(keys(t.range(&b"b"[..]..&b"a"[..])).is_empty());
            assert_eq!(t.get_lt(b"ab").unwrap().unwrap().0.as_ref(), b"a");
            assert_eq!(t.get_gt(b"ab").unwrap().unwrap().0.as_ref(), b"abc");
            assert_eq!(t.len(), 7);
            assert_eq!(t.pop_min().unwrap().unwrap().0.as_ref(), b"\x00");
            assert_eq!(t.pop_max().unwrap().unwrap().0.as_ref(), b"\xff\xff");
            t.clear().unwrap();
            assert!(t.is_empty());
        })
    }

    #[test]
    fn update_and_cas() {
        both(|db| {
            let t = db.open_tree("t").unwrap();
            let (old, new) = t.update(b"k", |v| Some(v.map_or(vec![1], |v| vec![v[0] + 1]))).unwrap();
            assert_eq!((old, new.as_deref()), (None, Some(&[1u8][..])));
            t.update(b"k", |v| Some(vec![v.unwrap()[0] + 1])).unwrap();
            assert_eq!(t.get(b"k").unwrap().as_deref(), Some(&[2u8][..]));
            // the closure can use the database: no write lock is held around it
            t.update(b"k", |v| {
                let _ = t.get(b"other");
                v.map(|v| v.to_vec())
            })
            .unwrap();
            assert!(t.compare_and_swap(b"k", Some(&[9]), None).unwrap().is_err());
            assert!(t.compare_and_swap(b"k", Some(&[2]), None).unwrap().is_ok());
            assert!(t.get(b"k").unwrap().is_none());
        })
    }

    #[test]
    fn ids_increase() {
        both(|db| {
            let a = db.generate_id().unwrap();
            let b = db.generate_id().unwrap();
            assert!(b > a);
            db.ensure_next_id(1000).unwrap();
            assert!(db.generate_id().unwrap() >= 1000);
        })
    }

    /// Rough per-write cost on each engine, for the cutover's throughput
    /// risk: `cargo test -p yats --release -- --ignored --nocapture writes_per_second`.
    #[test]
    #[ignore]
    fn writes_per_second() {
        use crate::Tree;
        for e in [Engine::Sled, Engine::Redb] {
            let db = Db::temporary(e).unwrap();
            let t: Tree<(u64, u64), (String, f64, Vec<u32>)> = Tree::open(&db, "t").unwrap();
            let n = 20_000u64;
            let start = std::time::Instant::now();
            for i in 0..n {
                t.insert(&(i % 97, i), &(format!("pilot {i}"), i as f64, vec![1, 2, 3])).unwrap();
            }
            let ins = start.elapsed();
            let start = std::time::Instant::now();
            for i in 0..n {
                t.update_and_fetch(&(i % 97, i), |v| v.map(|(s, f, l)| (s, f + 1.0, l))).unwrap();
            }
            let upd = start.elapsed();
            let start = std::time::Instant::now();
            let scanned = t.iter().count();
            let scan = start.elapsed();
            db.flush().unwrap();
            println!(
                "{e}: {:.0} inserts/s, {:.0} updates/s, scan of {scanned} rows in {} ms",
                n as f64 / ins.as_secs_f64(),
                n as f64 / upd.as_secs_f64(),
                scan.as_millis()
            );
        }
    }

    #[test]
    fn redb_reopens_with_data_and_ids() {
        let path = std::env::temp_dir().join(format!("yats-reopen-{}.redb", std::process::id()));
        let _ = std::fs::remove_file(&path);
        {
            let db = Db::open_redb(&path).unwrap();
            db.open_tree("t").unwrap().insert(b"k", &b"v"[..]).unwrap();
            db.ensure_next_id(77).unwrap();
            // no explicit flush: dropping the last handle makes it durable
        }
        {
            let db = Db::open_redb(&path).unwrap();
            assert_eq!(db.open_tree("t").unwrap().get(b"k").unwrap().as_deref(), Some(&b"v"[..]));
            assert_eq!(db.generate_id().unwrap(), 77);
            assert_eq!(db.tree_names().unwrap(), vec![b"t".to_vec()]);
        }
        let _ = std::fs::remove_file(&path);
    }
}
