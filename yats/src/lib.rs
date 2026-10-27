//! This is based on the typed_sled crate with some fundamental changes
//! - serialization errors do not cause panics
//! - various features I didn't need removed
//! - big endian number encoding so that ordering in sled works properly
//! - tuple based prefix iteration restores the functionality of scan_prefix
//! - use anyhow for errors, because I like anyhow, and it's
//!   neccessary to unify sled errors with serialization errors to
//!   maintain an ergonomic interface

pub mod backend;
pub mod codec;
pub mod convert;
pub mod envelope;
pub mod export;
pub mod transaction;

use anyhow::Result;
pub use backend::{Db, Engine, IVec, OpenTree, RawIter, RawTree};
use bincode::Options;
pub use codec::Codec;
use serde::Serialize;
pub use sled::{open, Config};
use sled::transaction::{ConflictableTransactionResult, TransactionError, TransactionResult};
use std::{
    fmt,
    iter::{DoubleEndedIterator, Iterator},
    marker::PhantomData,
    ops::{Bound, RangeBounds},
};
pub use envelope::{Envelope, Versioned};
use transaction::TransactionalTree;

/// A flash-sympathetic persistent lock-free B+ tree.
///
/// A `Tree` represents a single logical keyspace / namespace / bucket.
///
/// # Example
/// ```
/// fn main() -> anyhow::Result<()> {
///     // Creating a temporary sled database.
///     // If you want to persist the data use yats::open instead.
///     let db = yats::Config::new().temporary(true).open()?;
///
///     // The id is used by sled to identify which Tree in the database (db) to open.
///     let tree = yats::Tree::<String, u32>::open(&db, "unique_id")?;
///
///     tree.insert(&"some_key".to_owned(), &10)?;
///
///     assert_eq!(tree.get(&"some_key".to_owned())?, Some(10));
///     Ok(())
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Tree<K, V> {
    inner: RawTree,
    _key: PhantomData<K>,
    _value: PhantomData<V>,
}

/// Trait alias for bounds required on keys and values.
/// For now only types that implement DeserializeOwned
/// are supported.
pub trait KV: serde::de::DeserializeOwned + Serialize {}

impl<T: serde::de::DeserializeOwned + Serialize> KV for T {}

/// marker trait for types that serialize to a valid prefix of another
/// type for the purposes of sled. Used by subscribe, scan_prefix,
/// etc.
///
/// You can implement this yourself for structs and can rely on
/// serde visiting struct fields in the order they are lexicially
/// defined.
pub trait Prefix<T>: KV {}

impl<T: KV> Prefix<T> for T {}

impl<A: KV, B: KV> Prefix<A> for (A, B) {}
impl<A: KV, B: KV, C: KV> Prefix<A> for (A, B, C) {}
impl<A: KV, B: KV, C: KV> Prefix<(A, B)> for (A, B, C) {}
impl<A: KV, B: KV, C: KV, D: KV> Prefix<A> for (A, B, C, D) {}
impl<A: KV, B: KV, C: KV, D: KV> Prefix<(A, B)> for (A, B, C, D) {}
impl<A: KV, B: KV, C: KV, D: KV> Prefix<(A, B, C)> for (A, B, C, D) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV> Prefix<A> for (A, B, C, D, E) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV> Prefix<(A, B)> for (A, B, C, D, E) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV> Prefix<(A, B, C)> for (A, B, C, D, E) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV> Prefix<(A, B, C, D)> for (A, B, C, D, E) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV> Prefix<A> for (A, B, C, D, E, F) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV> Prefix<(A, B)> for (A, B, C, D, E, F) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV> Prefix<(A, B, C)> for (A, B, C, D, E, F) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV> Prefix<(A, B, C, D)> for (A, B, C, D, E, F) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV> Prefix<(A, B, C, D, E)> for (A, B, C, D, E, F) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV> Prefix<A> for (A, B, C, D, E, F, G) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV> Prefix<(A, B)> for (A, B, C, D, E, F, G) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV> Prefix<(A, B, C)> for (A, B, C, D, E, F, G) {}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV> Prefix<(A, B, C, D)>
    for (A, B, C, D, E, F, G)
{
}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV> Prefix<(A, B, C, D, E)>
    for (A, B, C, D, E, F, G)
{
}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV> Prefix<(A, B, C, D, E, F)>
    for (A, B, C, D, E, F, G)
{
}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV, H: KV> Prefix<A>
    for (A, B, C, D, E, F, G, H)
{
}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV, H: KV> Prefix<(A, B)>
    for (A, B, C, D, E, F, G, H)
{
}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV, H: KV> Prefix<(A, B, C)>
    for (A, B, C, D, E, F, G, H)
{
}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV, H: KV> Prefix<(A, B, C, D)>
    for (A, B, C, D, E, F, G, H)
{
}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV, H: KV> Prefix<(A, B, C, D, E)>
    for (A, B, C, D, E, F, G, H)
{
}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV, H: KV> Prefix<(A, B, C, D, E, F)>
    for (A, B, C, D, E, F, G, H)
{
}
impl<A: KV, B: KV, C: KV, D: KV, E: KV, F: KV, G: KV, H: KV> Prefix<(A, B, C, D, E, F, G)>
    for (A, B, C, D, E, F, G, H)
{
}

/// Compare and swap error.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompareAndSwapError<V> {
    /// The current value which caused your CAS to fail.
    pub current: Option<V>,
    /// Returned value that was proposed unsuccessfully.
    pub proposed: Option<V>,
}

impl<V> fmt::Display for CompareAndSwapError<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Compare and swap conflict")
    }
}

// implemented like this in the sled source
impl<V: std::fmt::Debug> std::error::Error for CompareAndSwapError<V> {}

/// What [`Tree::reencode`] did.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ReencodeStats {
    /// Rows looked at.
    pub rows: u64,
    /// Rows already in the tree's codec.
    pub current: u64,
    /// Rows rewritten (or, checking only, that would be).
    pub reencoded: u64,
    /// Rows that do not decode as the tree's value type: left as they are.
    pub undecodable: u64,
    /// Rows that decode but would not come back exactly after the change:
    /// left as they are.
    pub unverified: u64,
}

impl std::ops::AddAssign for ReencodeStats {
    fn add_assign(&mut self, o: Self) {
        self.rows += o.rows;
        self.current += o.current;
        self.reencoded += o.reencoded;
        self.undecodable += o.undecodable;
        self.unverified += o.unverified;
    }
}

fn unsupported_on_redb(what: &str) -> anyhow::Error {
    anyhow::anyhow!("{what} is only supported on sled-backed trees")
}

// These Trait bounds should probably be specified on the functions themselves, but too lazy.
impl<K, V> Tree<K, V> {
    /// Initialize a typed tree. The id identifies the tree to be opened from
    /// the db: a [`yats::Db`](Db) on either engine, or a bare `sled::Db`.
    /// # Example
    ///
    /// ```
    /// fn main() -> anyhow::Result<()> {
    ///     let db = yats::Config::new().temporary(true).open()?;
    ///     let tree = yats::Tree::<String, u32>::open(&db, "unique_id")?;
    ///     tree.insert(&"some_key".to_owned(), &10)?;
    ///     assert_eq!(tree.get(&"some_key".to_owned())?, Some(10));
    ///     Ok(())
    /// }
    /// ```
    pub fn open<D: OpenTree + ?Sized, T: AsRef<str>>(db: &D, id: T) -> Result<Self>
    where
        K: KV + 'static,
        V: KV + 'static,
    {
        Ok(Self::from_raw(db.open_typed::<K, V>(id.as_ref())?))
    }

    /// A typed view of a raw tree.
    pub fn from_raw(inner: RawTree) -> Self {
        Self { inner, _key: PhantomData, _value: PhantomData }
    }

    /// The raw tree underneath.
    pub fn raw(&self) -> &RawTree {
        &self.inner
    }

    fn sled_tree(&self, what: &str) -> Result<&sled::Tree> {
        self.inner.as_sled().ok_or_else(|| unsupported_on_redb(what))
    }

    fn enc(&self, v: &V) -> Result<Vec<u8>>
    where
        V: KV,
    {
        self.inner.encode_value(v)
    }

    /// Insert a key to a new value, returning the last value if it was set.
    pub fn insert(&self, key: &K, value: &V) -> Result<Option<V>>
    where
        K: KV,
        V: KV,
    {
        Ok(self
            .inner
            .insert(serialize(key)?, self.enc(value)?)?
            .map(|old| deserialize(&old))
            .transpose()?)
    }

    /// Perform a multi-key serializable transaction. sled-backed trees only;
    /// on redb this fails with a storage error.
    pub fn transaction<F, A, E>(&self, f: F) -> TransactionResult<A, E>
    where
        F: Fn(&TransactionalTree<K, V>) -> ConflictableTransactionResult<A, E>,
    {
        match self.inner.as_sled() {
            Some(t) => t.transaction(|sled_transactional_tree| {
                f(&TransactionalTree::new(sled_transactional_tree))
            }),
            None => Err(TransactionError::Storage(sled::Error::Unsupported(
                "transactions on a redb-backed tree".into(),
            ))),
        }
    }

    /// Create a new batched update that can be atomically applied.
    ///
    /// It is possible to apply a Batch in a transaction as well,
    /// which is the way you can apply a Batch to multiple Trees
    /// atomically.
    pub fn apply_batch(&self, batch: Batch<K, V>) -> Result<()> {
        self.inner.apply(batch.into_ops(self.inner.codec())?)
    }

    /// Retrieve a value from the Tree if it exists.
    pub fn get(&self, key: &K) -> Result<Option<V>>
    where
        K: KV,
        V: KV,
    {
        Ok(self
            .inner
            .get(serialize(key)?)?
            .map(|v| deserialize(&v))
            .transpose()?)
    }

    /// Retrieve a value from the Tree if it exists. The key must be in serialized form.
    pub fn get_from_raw<B: AsRef<[u8]>>(&self, key_bytes: B) -> Result<Option<V>>
    where
        K: KV,
        V: KV,
    {
        Ok(self
            .inner
            .get(key_bytes.as_ref())?
            .map(|v| deserialize(&v))
            .transpose()?)
    }

    /// Deserialize a key and retrieve it's value from the Tree if it exists.
    /// The deserialization is only done if a value was retrieved successfully.
    pub fn get_kv_from_raw<B: AsRef<[u8]>>(&self, key_bytes: B) -> Result<Option<(K, V)>>
    where
        K: KV,
        V: KV,
    {
        Ok(self
            .inner
            .get(key_bytes.as_ref())?
            .map(|v| Ok::<_, anyhow::Error>((decode_key(key_bytes.as_ref())?, deserialize(&v)?)))
            .transpose()?)
    }

    /// Delete a value, returning the old value if it existed.
    pub fn remove(&self, key: &K) -> Result<Option<V>>
    where
        K: KV,
        V: KV,
    {
        Ok(self
            .inner
            .remove(serialize(key)?)?
            .map(|v| deserialize(&v))
            .transpose()?)
    }

    /// Delete a key without decoding its old value. `remove` hands the old
    /// value back, so it fails on a row written by an older build in a layout
    /// this one can no longer read -- exactly the rows that most need deleting.
    pub fn delete(&self, key: &K) -> Result<bool>
    where
        K: KV,
    {
        Ok(self.inner.remove(serialize(key)?)?.is_some())
    }

    /// Compare and swap. Capable of unique creation, conditional
    /// modification, or deletion. If old is None, this will only set
    /// the value if it doesn't exist yet. If new is None, will delete
    /// the value if old is correct. If both old and new are Some,
    /// will modify the value if old is correct.
    ///
    /// It returns Ok(Ok(())) if operation finishes successfully.
    ///
    /// If it fails it returns: - Ok(Err(CompareAndSwapError(current,
    /// proposed))) if operation failed to setup a new
    /// value. CompareAndSwapError contains current and proposed
    /// values. - Err(Error::Unsupported) if the database is opened in
    /// read-only mode.
    ///
    /// `old` is compared as encoded bytes, so on a tree holding rows in
    /// both codecs (a converted database) a legacy row never matches.
    pub fn compare_and_swap(
        &self,
        key: &K,
        old: Option<&V>,
        new: Option<&V>,
    ) -> Result<std::result::Result<(), CompareAndSwapError<V>>>
    where
        K: KV,
        V: KV,
    {
        let old_b = old.map(|old| self.enc(old)).transpose()?;
        let new_b = new.map(|new| self.enc(new)).transpose()?;
        match self.inner.compare_and_swap(serialize(key)?, old_b.as_deref(), new_b.clone())? {
            Ok(()) => Ok(Ok(())),
            Err(current) => Ok(Err(CompareAndSwapError {
                current: current.as_ref().map(|b| deserialize(b)).transpose()?,
                proposed: new_b.as_ref().map(|b| deserialize(b)).transpose()?,
            })),
        }
    }

    /// The typed side of `update_and_fetch`/`fetch_and_update`: run `f` on
    /// the decoded value. A value that fails to decode or encode leaves the
    /// stored one unchanged and makes the call an error.
    fn update_typed<F>(&self, key: &K, mut f: F) -> Result<(Option<V>, Option<V>)>
    where
        K: KV,
        V: KV,
        F: FnMut(Option<V>) -> Option<V>,
    {
        let mut failed = None;
        let (old, new) = self.inner.update(serialize(key)?, |opt_value| {
            failed = None;
            let v_ser = match opt_value.map(|v| deserialize(v)).transpose() {
                Ok(v_ser) => v_ser,
                Err(e) => {
                    failed = Some(e);
                    return opt_value.map(Vec::from);
                }
            };
            match f(v_ser).map(|r| self.enc(&r)).transpose() {
                Ok(r_ser) => r_ser,
                Err(e) => {
                    failed = Some(e);
                    opt_value.map(Vec::from)
                }
            }
        })?;
        if let Some(e) = failed {
            return Err(e);
        }
        Ok((
            old.map(|v| deserialize(&v)).transpose()?,
            new.map(|v| deserialize(&v)).transpose()?,
        ))
    }

    /// Fetch the value, apply a function to it and return the result.
    /// If serialization or deserialization of the value passed to or
    /// received from F fails the value in the database will not be
    /// changed and an error will be returned.
    pub fn update_and_fetch<F>(&self, key: &K, f: F) -> Result<Option<V>>
    where
        K: KV,
        V: KV,
        F: FnMut(Option<V>) -> Option<V>,
    {
        Ok(self.update_typed(key, f)?.1)
    }

    /// Fetch the value, apply a function to it and return the previous value.
    pub fn fetch_and_update<F>(&self, key: &K, f: F) -> Result<Option<V>>
    where
        K: KV,
        V: KV,
        F: FnMut(Option<V>) -> Option<V>,
    {
        Ok(self.update_typed(key, f)?.0)
    }

    /// Subscribe to `Event`s that happen to keys that have
    /// the specified prefix. Events for particular keys are
    /// guaranteed to be witnessed in the same order by all
    /// threads, but threads may witness different interleavings
    /// of `Event`s across different keys. If subscribers don't
    /// keep up with new writes, they will cause new writes
    /// to block. There is a buffer of 1024 items per
    /// `Subscriber`. This can be used to build reactive
    /// and replicated systems. sled-backed trees only.
    pub fn watch_prefix<P>(&self, prefix: &P) -> Result<Subscriber<K, V>>
    where
        P: KV,
        K: Prefix<P>,
    {
        Ok(Subscriber::from_sled(
            self.sled_tree("watch_prefix")?.watch_prefix(serialize(prefix)?),
        ))
    }

    /// Subscribe to  all`Event`s. Events for particular keys are
    /// guaranteed to be witnessed in the same order by all
    /// threads, but threads may witness different interleavings
    /// of `Event`s across different keys. If subscribers don't
    /// keep up with new writes, they will cause new writes
    /// to block. There is a buffer of 1024 items per
    /// `Subscriber`. This can be used to build reactive
    /// and replicated systems. sled-backed trees only: panics on redb.
    pub fn watch_all(&self) -> Subscriber<K, V>
    where
        K: KV,
    {
        Subscriber::from_sled(
            self.inner
                .as_sled()
                .expect("watch_all is only supported on sled-backed trees")
                .watch_prefix(vec![]),
        )
    }

    /// Synchronously flushes all dirty IO buffers and calls
    /// fsync. If this succeeds, it is guaranteed that all
    /// previous writes will be recovered if the system
    /// crashes. Returns the number of bytes flushed during
    /// this call (0 on redb).
    ///
    /// Flushing can take quite a lot of time, and you should
    /// measure the performance impact of using it on
    /// realistic sustained workloads running on realistic
    /// hardware.
    pub fn flush(&self) -> Result<usize> {
        self.inner.flush()
    }

    /// Asynchronously flushes all dirty IO buffers
    /// and calls fsync. If this succeeds, it is
    /// guaranteed that all previous writes will
    /// be recovered if the system crashes. Returns
    /// the number of bytes flushed during this call.
    ///
    /// Flushing can take quite a lot of time, and you
    /// should measure the performance impact of
    /// using it on realistic sustained workloads
    /// running on realistic hardware.
    pub async fn flush_async(&self) -> Result<usize> {
        match self.inner.as_sled() {
            Some(t) => Ok(t.flush_async().await?),
            None => self.inner.flush(),
        }
    }

    /// Returns `true` if the `Tree` contains a value for
    /// the specified key.
    pub fn contains_key(&self, key: &K) -> Result<bool>
    where
        K: KV,
    {
        self.inner.contains_key(serialize(key)?)
    }

    /// Retrieve the key and value before the provided key,
    /// if one exists.
    pub fn get_lt(&self, key: &K) -> Result<Option<(K, V)>>
    where
        K: KV,
        V: KV,
    {
        self.inner.get_lt(serialize(key)?)?.map(|(k, v)| decode_pair(&k, &v)).transpose()
    }

    /// Retrieve the next key and value from the `Tree` after the
    /// provided key.
    pub fn get_gt(&self, key: &K) -> Result<Option<(K, V)>>
    where
        K: KV,
        V: KV,
    {
        self.inner.get_gt(serialize(key)?)?.map(|(k, v)| decode_pair(&k, &v)).transpose()
    }

    /// Merge state directly into a given key's value using the
    /// configured merge operator. This allows state to be written
    /// into a value directly, without any read-modify-write steps.
    /// Merge operators can be used to implement arbitrary data
    /// structures.
    ///
    /// Calling `merge` will return an `Unsupported` error if it
    /// is called without first setting a merge operator function.
    ///
    /// Merge operators are shared by all instances of a particular
    /// `Tree`. Different merge operators may be set on different
    /// `Tree`s. sled-backed trees only.
    pub fn merge(&self, key: &K, value: &V) -> Result<Option<V>>
    where
        K: KV,
        V: KV,
    {
        Ok(self
            .sled_tree("merge")?
            .merge(serialize(key)?, serialize(value)?)?
            .map(|old_v| deserialize(&old_v))
            .transpose()?)
    }

    /// Sets a merge operator for use with the `merge` function.
    ///
    /// Merge state directly into a given key's value using the
    /// configured merge operator. This allows state to be written
    /// into a value directly, without any read-modify-write steps.
    /// Merge operators can be used to implement arbitrary data
    /// structures.
    ///
    /// Calling `merge` returns an `Unsupported` error if no merge
    /// operator has been configured.
    ///
    /// sled gives a merge operator no way to report an error, so a key or
    /// value that fails to (de)serialize leaves the stored value exactly as it
    /// was instead of panicking inside sled -- this crate's whole point is that
    /// a bad record is an error, never a crash.
    ///
    /// sled-backed trees only; on redb this does nothing and `merge` fails.
    pub fn set_merge_operator(&self, merge_operator: impl MergeOperator<K, V> + 'static)
    where
        K: KV,
        V: KV,
    {
        let Some(tree) = self.inner.as_sled() else { return };
        tree.set_merge_operator(move |key: &[u8], old_v: Option<&[u8]>, value: &[u8]| {
            let unchanged = || old_v.map(|v| v.to_vec());
            let Ok(key_des) = decode_key(key) else { return unchanged() };
            let old_v_des = match old_v.map(|v| deserialize(v)).transpose() {
                Ok(v) => v,
                Err(_) => return unchanged(),
            };
            let Ok(value_des) = deserialize(value) else { return unchanged() };
            match merge_operator(key_des, old_v_des, value_des) {
                None => None,
                Some(v) => match serialize(&v) {
                    Ok(bytes) => Some(bytes),
                    Err(_) => unchanged(),
                },
            }
        });
    }

    /// Create a double-ended iterator over the tuples of keys and
    /// values in this tree.
    pub fn iter(&self) -> Iter<K, V> {
        Iter::from_raw(self.inner.iter())
    }

    /// Create a double-ended iterator over tuples of keys and values,
    /// where the keys fall within the specified range.
    pub fn range<R: RangeBounds<K>>(&self, range: R) -> Result<Iter<K, V>>
    where
        K: KV + std::fmt::Debug,
    {
        // Map each bound onto its serialized form one-for-one. This used to be
        // a hand-written match that turned an `Excluded` lower bound into an
        // `Included` one (sled's `a..b` syntax has no excluded-start form), so
        // `range((Excluded(k), ..))` silently yielded `k` itself. A
        // `(Bound, Bound)` pair expresses every combination directly.
        fn ser<K: KV>(b: Bound<&K>) -> Result<Bound<Vec<u8>>> {
            Ok(match b {
                Bound::Included(k) => Bound::Included(serialize(k)?),
                Bound::Excluded(k) => Bound::Excluded(serialize(k)?),
                Bound::Unbounded => Bound::Unbounded,
            })
        }
        let bounds = (ser(range.start_bound())?, ser(range.end_bound())?);
        Ok(Iter::from_raw(self.inner.range::<Vec<u8>, _>(bounds)))
    }

    /// Create an iterator over tuples of keys and values,
    /// where the all the keys starts with the given prefix.
    pub fn scan_prefix<P>(&self, prefix: &P) -> Result<Iter<K, V>>
    where
        P: KV,
        K: Prefix<P>,
    {
        Ok(Iter::from_raw(self.inner.scan_prefix(serialize(prefix)?)))
    }

    /// Returns the first key and value in the `Tree`, or
    /// `None` if the `Tree` is empty.
    pub fn first(&self) -> Result<Option<(K, V)>>
    where
        K: KV,
        V: KV,
    {
        self.inner.first()?.map(|(k, v)| decode_pair(&k, &v)).transpose()
    }

    /// Returns the last key and value in the `Tree`, or
    /// `None` if the `Tree` is empty.
    pub fn last(&self) -> Result<Option<(K, V)>>
    where
        K: KV,
        V: KV,
    {
        self.inner.last()?.map(|(k, v)| decode_pair(&k, &v)).transpose()
    }

    /// Atomically removes the maximum item in the `Tree` instance.
    pub fn pop_max(&self) -> Result<Option<(K, V)>>
    where
        K: KV,
        V: KV,
    {
        self.inner.pop_max()?.map(|(k, v)| decode_pair(&k, &v)).transpose()
    }

    /// Atomically removes the minimum item in the `Tree` instance.
    pub fn pop_min(&self) -> Result<Option<(K, V)>>
    where
        K: KV,
        V: KV,
    {
        self.inner.pop_min()?.map(|(k, v)| decode_pair(&k, &v)).transpose()
    }

    /// Returns the number of elements in this tree.
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns `true` if the `Tree` contains no elements.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clears the `Tree`, removing all values.
    ///
    /// Note that this is not atomic on sled.
    pub fn clear(&self) -> Result<()> {
        self.inner.clear()
    }

    /// Returns the name of the tree.
    pub fn name(&self) -> IVec {
        self.inner.name()
    }

    /// Returns the CRC32 of all keys and values
    /// in this Tree.
    ///
    /// This is O(N) and locks the underlying tree
    /// for the duration of the entire scan. sled-backed trees only.
    pub fn checksum(&self) -> Result<u32> {
        Ok(self.sled_tree("checksum")?.checksum()?)
    }

    /// Rewrite every row not yet in this tree's codec (on redb, legacy
    /// bincode rows) in it, decoding each with `V`. A row is only rewritten
    /// if it comes back exactly: decoded from the new bytes and encoded as
    /// bincode again, it must give the original bytes (or, for types holding
    /// hash maps, whose order is not stable, the same bytes in some order).
    /// Rows that fail either way are left as they are and counted. With
    /// `check_only`, nothing is written.
    ///
    /// Safe to run on a live tree: each rewrite only lands if the row has
    /// not changed since it was read.
    pub fn reencode(&self, check_only: bool) -> Result<ReencodeStats>
    where
        K: KV,
        V: KV,
    {
        const CHUNK: usize = 2048;
        let codec = self.inner.codec();
        let mut st = ReencodeStats::default();
        let mut pending: Vec<(Vec<u8>, Vec<u8>, Vec<u8>)> = vec![];
        for r in self.inner.iter() {
            let (k, v) = r?;
            st.rows += 1;
            if codec::is_msgpack(&v) == (codec == Codec::MsgPack) {
                st.current += 1;
                continue;
            }
            let Ok(val) = deserialize::<V>(&v) else {
                st.undecodable += 1;
                continue;
            };
            let new = codec::encode_value(&val, codec)?;
            if !reencode_is_exact::<V>(&v, &new) {
                st.unverified += 1;
                continue;
            }
            st.reencoded += 1;
            if !check_only {
                pending.push((k.to_vec(), v.to_vec(), new));
                if pending.len() >= CHUNK {
                    self.inner.replace_unchanged(std::mem::take(&mut pending))?;
                }
            }
        }
        self.inner.replace_unchanged(pending)?;
        Ok(st)
    }
}

/// `new` decodes to a value whose bincode is `old`'s (see `Tree::reencode`).
fn reencode_is_exact<V: KV>(old: &[u8], new: &[u8]) -> bool {
    let reference = if codec::is_msgpack(old) {
        // going back to bincode: compare in bincode
        match deserialize::<V>(old).and_then(|v| serialize(&v)) {
            Ok(b) => b,
            Err(_) => return false,
        }
    } else {
        old.to_vec()
    };
    let Ok(back) = deserialize::<V>(new).and_then(|v| serialize(&v)) else { return false };
    if back == reference {
        return true;
    }
    // A HashMap/HashSet re-serializes in a different order each time it is
    // rebuilt; its entries are the same bytes, permuted. Anything lost or
    // changed shows up as different bytes.
    let (mut a, mut b) = (back, reference);
    a.sort_unstable();
    b.sort_unstable();
    a == b
}

fn decode_pair<K: KV, V: KV>(k: &[u8], v: &[u8]) -> Result<(K, V)> {
    Ok((decode_key(k)?, deserialize(v)?))
}

/// # Examples
///
/// ```
/// # fn main() -> anyhow::Result<()> {
/// fn concatenate_merge(
///   _key: String,               // the key being merged
///   old_value: Option<Vec<f32>>,  // the previous value, if one existed
///   merged_bytes: Vec<f32>        // the new bytes being merged in
/// ) -> Option<Vec<f32>> {       // set the new value, return None to delete
///   let mut ret = old_value
///     .map(|ov| ov.to_vec())
///     .unwrap_or_else(|| vec![]);
///
///   ret.extend_from_slice(&merged_bytes);
///
///   Some(ret)
/// }
///
/// let db = yats::Config::new()
///   .temporary(true).open()?;
///
/// let tree = yats::Tree::<String, Vec<f32>>::open(&db, "unique_id")?;
/// tree.set_merge_operator(concatenate_merge);
///
/// let k = String::from("some_key");
///
/// tree.insert(&k, &vec![0.0])?;
/// tree.merge(&k, &vec![1.0])?;
/// tree.merge(&k, &vec![2.0])?;
/// assert_eq!(tree.get(&k)?, Some(vec![0.0, 1.0, 2.0]));
///
/// // Replace previously merged data. The merge function will not be called.
/// tree.insert(&k, &vec![3.0])?;
/// assert_eq!(tree.get(&k)?, Some(vec![3.0]));
///
/// // Merges on non-present values will cause the merge function to be called
/// // with `old_value == None`. If the merge function returns something (which it
/// // does, in this case) a new value will be inserted.
/// tree.remove(&k)?;
/// tree.merge(&k, &vec![4.0])?;
/// assert_eq!(tree.get(&k)?, Some(vec![4.0]));
/// # Ok(()) }
/// ```
pub trait MergeOperator<K, V>: Fn(K, Option<V>, V) -> Option<V> {}

impl<K, V, F> MergeOperator<K, V> for F where F: Fn(K, Option<V>, V) -> Option<V> {}

pub struct Iter<K, V> {
    inner: RawIter,
    _key: PhantomData<fn() -> K>,
    _value: PhantomData<fn() -> V>,
}

impl<K: KV, V: KV> Iterator for Iter<K, V> {
    type Item = Result<(K, V)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|r| r.and_then(|(k, v)| decode_pair(&k, &v)))
    }

    fn last(mut self) -> Option<Self::Item> {
        self.inner.next_back().map(|r| r.and_then(|(k, v)| decode_pair(&k, &v)))
    }
}

impl<K: KV, V: KV> DoubleEndedIterator for Iter<K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back().map(|r| r.and_then(|(k, v)| decode_pair(&k, &v)))
    }
}

impl<K, V> Iter<K, V> {
    pub fn from_sled(iter: sled::Iter) -> Self {
        Self::from_raw(RawIter::from_sled(iter))
    }

    pub fn from_raw(iter: RawIter) -> Self {
        Iter {
            inner: iter,
            _key: PhantomData,
            _value: PhantomData,
        }
    }

    /// Iterate over the keys alone, never decoding a value. Useful when the
    /// key is all a caller needs to pick a record (e.g. the newest by a
    /// timestamp in the key) and old values may no longer decode.
    pub fn keys_only(self) -> impl DoubleEndedIterator<Item = Result<K>> + Send + Sync
    where
        K: KV + Send + Sync,
    {
        let inner = self.inner;
        KeysOnly::<K> { inner, _key: PhantomData }
    }

    pub fn keys(self) -> impl DoubleEndedIterator<Item = Result<K>> + Send + Sync
    where
        K: KV + Send + Sync,
        V: KV + Send + Sync,
    {
        self.map(|r| r.map(|(k, _v)| k))
    }

    /// Iterate over the values of this Tree
    pub fn values(self) -> impl DoubleEndedIterator<Item = Result<V>> + Send + Sync
    where
        K: KV + Send + Sync,
        V: KV + Send + Sync,
    {
        self.map(|r| r.map(|(_k, v)| v))
    }
}

struct KeysOnly<K> {
    inner: RawIter,
    _key: PhantomData<fn() -> K>,
}

impl<K: KV> Iterator for KeysOnly<K> {
    type Item = Result<K>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|r| r.and_then(|(k, _)| decode_key(&k)))
    }
}

impl<K: KV> DoubleEndedIterator for KeysOnly<K> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back().map(|r| r.and_then(|(k, _)| decode_key(&k)))
    }
}

/// Writes collected to apply at once with [`Tree::apply_batch`]. Values are
/// encoded in both codecs up front; the tree's is used when it is applied.
#[derive(Clone, Debug)]
pub struct Batch<K, V> {
    ops: Vec<BatchOp>,
    _key: PhantomData<K>,
    _value: PhantomData<V>,
}

#[derive(Clone, Debug)]
enum BatchOp {
    /// key, bincode value, MessagePack value
    Insert(Vec<u8>, Vec<u8>, Vec<u8>),
    Remove(Vec<u8>),
}

impl<K, V> Batch<K, V> {
    pub fn insert(&mut self, key: &K, value: &V) -> Result<()>
    where
        K: KV,
        V: KV,
    {
        self.ops.push(BatchOp::Insert(
            serialize(key)?,
            serialize(value)?,
            codec::encode_value(value, Codec::MsgPack)?,
        ));
        Ok(())
    }

    pub fn remove(&mut self, key: &K) -> Result<()>
    where
        K: KV,
    {
        self.ops.push(BatchOp::Remove(serialize(key)?));
        Ok(())
    }

    fn into_ops(self, codec: Codec) -> Result<Vec<backend::Op>> {
        Ok(self
            .ops
            .into_iter()
            .map(|op| match op {
                BatchOp::Insert(k, b, m) => {
                    backend::Op::Insert(k, if codec == Codec::MsgPack { m } else { b })
                }
                BatchOp::Remove(k) => backend::Op::Remove(k),
            })
            .collect())
    }

    /// As a sled batch (sled trees always take bincode).
    pub(crate) fn to_sled(&self) -> sled::Batch {
        let mut b = sled::Batch::default();
        for op in &self.ops {
            match op {
                BatchOp::Insert(k, v, _) => b.insert(k.clone(), v.clone()),
                BatchOp::Remove(k) => b.remove(k.clone()),
            }
        }
        b
    }
}

// Implementing Default manually to not require K and V to implement Default.
impl<K, V> Default for Batch<K, V> {
    fn default() -> Self {
        Self {
            ops: Vec::new(),
            _key: PhantomData,
            _value: PhantomData,
        }
    }
}

use pin_project::pin_project;
#[pin_project]
pub struct Subscriber<K, V> {
    #[pin]
    inner: sled::Subscriber,
    _key: PhantomData<fn() -> K>,
    _value: PhantomData<fn() -> V>,
}

impl<K, V> Subscriber<K, V> {
    pub fn next_timeout(
        &mut self,
        timeout: std::time::Duration,
    ) -> std::result::Result<Result<Event<K, V>>, std::sync::mpsc::RecvTimeoutError>
    where
        K: KV,
        V: KV,
    {
        self.inner
            .next_timeout(timeout)
            .map(|e| Event::from_sled(&e))
    }

    pub fn from_sled(subscriber: sled::Subscriber) -> Self {
        Self {
            inner: subscriber,
            _key: PhantomData,
            _value: PhantomData,
        }
    }
}

use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

impl<K: KV + Unpin, V: KV + Unpin> Future for Subscriber<K, V> {
    type Output = Option<Result<Event<K, V>>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.project()
            .inner
            .poll(cx)
            .map(|opt| opt.map(|e| Event::from_sled(&e)))
    }
}

impl<K: KV, V: KV> Iterator for Subscriber<K, V> {
    type Item = Result<Event<K, V>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|e| Event::from_sled(&e))
    }
}

pub enum Event<K, V> {
    Insert { key: K, value: V },
    Remove { key: K },
}

impl<K, V> Event<K, V> {
    pub fn key(&self) -> &K
    where
        K: KV,
    {
        match self {
            Self::Insert { key, .. } | Self::Remove { key } => key,
        }
    }

    pub fn from_sled(event: &sled::Event) -> Result<Self>
    where
        K: KV,
        V: KV,
    {
        match event {
            sled::Event::Insert { key, value } => Ok(Self::Insert {
                key: deserialize(key)?,
                value: deserialize(value)?,
            }),
            sled::Event::Remove { key } => Ok(Self::Remove {
                key: deserialize(key)?,
            }),
        }
    }
}

/// The function which is used to deserialize all values: MessagePack rows
/// (see [`codec`]) and bincode rows alike.
pub fn deserialize<'a, T>(bytes: &'a [u8]) -> Result<T>
where
    T: serde::de::Deserialize<'a>,
{
    if let Some(payload) = bytes.strip_prefix(&codec::MAGIC[..]) {
        return codec::decode_msgpack(payload);
    }
    // The row is made available to `Envelope`, which needs to look at it
    // whole; for any other type this is two thread-local writes.
    envelope::with_row(bytes, || decode_plain(bytes))
}

/// Keys are always plain bincode: no codec detection, no `Envelope`.
fn decode_key<'a, T>(bytes: &'a [u8]) -> Result<T>
where
    T: serde::de::Deserialize<'a>,
{
    decode_plain(bytes)
}

/// Plain bincode in yats' encoding, without `Envelope` handling: for
/// `Versioned::upgrade` impls decoding an old layout.
pub fn decode_plain<'a, T>(bytes: &'a [u8]) -> Result<T>
where
    T: serde::de::Deserialize<'a>,
{
    Ok(bincode::DefaultOptions::new()
        .with_big_endian()
        .with_fixint_encoding()
        .allow_trailing_bytes()
        .deserialize(bytes)?)
}

/// The function which is used to serialize all keys, and values on a
/// bincode (sled) tree. See [`codec::encode_value`] for the other codec.
pub fn serialize<T>(value: &T) -> Result<Vec<u8>>
where
    T: serde::Serialize,
{
    Ok(bincode::DefaultOptions::new()
        .with_big_endian()
        .with_fixint_encoding()
        .allow_trailing_bytes()
        .serialize(value)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_range() {
        let config = sled::Config::new().temporary(true);
        let db = config.open().unwrap();

        let tree: Tree<u32, u32> = Tree::open(&db, "test_tree").unwrap();

        tree.insert(&1, &2).unwrap();
        tree.insert(&3, &4).unwrap();
        tree.insert(&6, &2).unwrap();
        tree.insert(&10, &2).unwrap();
        tree.insert(&15, &2).unwrap();
        tree.flush().unwrap();

        let expect_results = [(6, 2), (10, 2)];

        let got: Vec<(u32, u32)> = tree.range(6..11).unwrap().map(|r| r.unwrap()).collect();
        assert_eq!(got, expect_results);
    }

    #[test]
    fn test_range_excluded_lower_bound() {
        let db = sled::Config::new().temporary(true).open().unwrap();
        let tree: Tree<u32, u32> = Tree::open(&db, "test_tree").unwrap();
        for k in [1u32, 3, 6, 10] {
            tree.insert(&k, &k).unwrap();
        }
        let got: Vec<u32> = tree
            .range((Bound::Excluded(3u32), Bound::Unbounded))
            .unwrap()
            .map(|r| r.unwrap().0)
            .collect();
        assert_eq!(got, vec![6, 10]);
        let got: Vec<u32> = tree
            .range((Bound::Excluded(1u32), Bound::Included(6u32)))
            .unwrap()
            .map(|r| r.unwrap().0)
            .collect();
        assert_eq!(got, vec![3, 6]);
    }

    #[test]
    fn test_keys_only_skips_undecodable_values() {
        let db = sled::Config::new().temporary(true).open().unwrap();
        let tree: Tree<u32, u64> = Tree::open(&db, "test_tree").unwrap();
        tree.insert(&1, &10).unwrap();
        // a value too short to be a u64 -- what a schema change leaves behind
        tree.inner.insert(serialize(&2u32).unwrap(), vec![1u8]).unwrap();
        assert!(tree.iter().any(|r| r.is_err()));
        let keys: Vec<u32> = tree.iter().keys_only().map(|r| r.unwrap()).collect();
        assert_eq!(keys, vec![1, 2]);
    }

    #[test]
    fn test_merge_operator_bad_value_is_a_noop() {
        let db = sled::Config::new().temporary(true).open().unwrap();
        let tree: Tree<u32, u64> = Tree::open(&db, "test_tree").unwrap();
        tree.set_merge_operator(|_k, old: Option<u64>, v: u64| Some(old.unwrap_or(0) + v));
        tree.insert(&1, &5).unwrap();
        tree.merge(&1, &2).unwrap();
        assert_eq!(tree.get(&1).unwrap(), Some(7));
        // an undecodable merge operand must not panic, and must not change the value
        tree.inner.as_sled().unwrap().merge(serialize(&1u32).unwrap(), vec![9u8]).unwrap();
        assert_eq!(tree.get(&1).unwrap(), Some(7));
    }

    /// The typed API behaves the same on both engines, including on a tree
    /// holding legacy bincode rows beside MessagePack ones (a converted DB).
    #[test]
    fn typed_api_on_both_engines() {
        for engine in [Engine::Sled, Engine::Redb] {
            let db = Db::temporary(engine).unwrap();
            let tree: Tree<(u32, String), Vec<u64>> = Tree::open(&db, "t").unwrap();
            // a legacy row, as a converter copies it
            tree.inner
                .insert(serialize(&(5u32, "e".to_string())).unwrap(), serialize(&vec![5u64]).unwrap())
                .unwrap();
            for (i, s) in [(1u32, "a"), (3, "c"), (3, "b"), (9, "z")] {
                tree.insert(&(i, s.to_string()), &vec![i as u64]).unwrap();
            }
            let keys: Vec<(u32, String)> = tree.iter().keys().map(|r| r.unwrap()).collect();
            let want: Vec<(u32, String)> =
                [(1, "a"), (3, "b"), (3, "c"), (5, "e"), (9, "z")].map(|(i, s)| (i, s.to_string())).into();
            assert_eq!(keys, want, "{engine}");
            assert_eq!(tree.scan_prefix(&3u32).unwrap().count(), 2);
            assert_eq!(tree.range((3u32, String::new())..(9u32, String::new())).unwrap().count(), 3);
            assert_eq!(tree.iter().rev().next().unwrap().unwrap().0 .0, 9);
            assert_eq!(tree.first().unwrap().unwrap().1, vec![1]);
            assert_eq!(tree.get(&(5, "e".into())).unwrap(), Some(vec![5]));
            let new = tree.update_and_fetch(&(5, "e".into()), |v| v.map(|mut v| { v.push(6); v })).unwrap();
            assert_eq!(new, Some(vec![5, 6]));
            let old = tree.fetch_and_update(&(1, "a".into()), |_| None).unwrap();
            assert_eq!(old, Some(vec![1]));
            assert!(!tree.contains_key(&(1, "a".into())).unwrap());
            assert_eq!(tree.len(), 4);
            let mut b = Batch::default();
            b.insert(&(7, "q".into()), &vec![7]).unwrap();
            b.remove(&(9, "z".into())).unwrap();
            tree.apply_batch(b).unwrap();
            assert_eq!(tree.last().unwrap().unwrap().0, (7, "q".into()));
            // values written now are in the engine's codec
            let raw = tree.inner.get(serialize(&(7u32, "q".to_string())).unwrap()).unwrap().unwrap();
            assert_eq!(codec::is_msgpack(&raw), engine == Engine::Redb);
            let st = tree.reencode(false).unwrap();
            assert_eq!(st.undecodable + st.unverified, 0);
            assert_eq!(tree.iter().count(), 4);
            tree.clear().unwrap();
            assert!(tree.is_empty());
        }
    }

    #[test]
    fn test_cas() {
        let config = sled::Config::new().temporary(true);
        let db = config.open().unwrap();

        let tree: Tree<u32, u32> = Tree::open(&db, "test_tree").unwrap();

        let current = 2;
        tree.insert(&1, &current).unwrap();
        let expected = 3;
        let proposed = 4;
        let res = tree
            .compare_and_swap(&1, Some(&expected), Some(&proposed))
            .expect("db failure");

        assert_eq!(
            res,
            Err(CompareAndSwapError {
                current: Some(current),
                proposed: Some(proposed),
            }),
        );
    }
}
