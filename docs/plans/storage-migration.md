# Plan: moving bfdb off sled 0.34 and bincode 1

Status: **phases 1 to 3 and 6 built, cutover is opt-in** (audit finding f55,
branch `storage-migration-5b03fc`). bfdb still runs on sled unless an
operator converts the database and starts it with `--storage redb`. The
operator runbook is [`docs/storage-cutover.md`](../storage-cutover.md).
Phases 4 (bot/Manager), 5 (rehearsal on live data) and 7 (default and
removal) are what is left.

## Why

bfdb keeps all its state (pilots, sorties, rounds, kills, sessions, news,
wiki, auth) in sled 0.34 through yats, with values as bincode 1. Both are
unmaintained: sled's 0.34 line has had no release since 2021 and its
rewrite never shipped; bincode 1 is frozen. Nothing is broken today, but
there is no upstream for a corruption bug, and bfdb has already been bitten
by torn sled pages (the maintenance backups exist because of one).

And bincode is positional: adding a field to a stored struct made every
existing row of its tree undecodable. That is why `session_v2` exists, and
why `news`, the range and the replays store JSON in raw trees.

## What bfdb actually uses

From `bfdb/src`: about 50 typed trees (`Tree<K, V>`, most in `db.rs`), 12 raw
JSON trees (`range/store.rs`, `replay/mod.rs`, `flightplans.rs`), and of the
yats/sled API only

- `get`, `insert`, `remove`, `delete`, `contains_key`;
- ordered iteration: `scan_prefix`, `range`, `iter`, `first`/`last`,
  `keys_only`, all double-ended;
- `update_and_fetch` / `fetch_and_update`: atomic read-modify-write;
- `clear`, `len`, `flush`, `size_on_disk`, and `generate_id` for ids
  (`db_id.rs`);
- `export` / `import` for consistent backups (`maint.rs`).

Not used: watch/subscribe, merge operators, compare-and-swap, transactions.
Keys are tuples encoded as big-endian fixint bincode, so their byte order is
their logical order; that is what `scan_prefix` and `range` rely on, and the
replacement sorts keys as raw bytes too.

## Choice: redb

| | redb | SQLite (rusqlite, bundled) |
| --- | --- | --- |
| model | ordered key/value tables, ACID, MVCC readers | SQL tables |
| fit with yats | one redb table per tree, raw byte keys: sled's model | one `(tree, k BLOB, v BLOB)` table or a table per tree, `ORDER BY k` on every scan |
| range scans | native, double-ended, lazy | `WHERE k >= ? AND k < ? ORDER BY k`; reverse scans need their own queries |
| snapshots | a read transaction is a point-in-time view of every table | `VACUUM INTO` / backup API |
| build | pure Rust, no deps (redb 4.4) | bundled C, a C toolchain on the build box |
| inspecting a live DB | needs a tool (the converter or a dump) | any SQLite browser |
| maintained | yes | yes |

redb, because it is the same shape as sled: one ordered byte-keyed table per
tree, so yats keeps its API and nothing above `StatsDb` changes. Its read
transactions are real snapshots, which gives bfdb its first consistent
backups across trees. SQLite's advantage, ad-hoc inspection, is not worth
rewriting every scan as SQL; the converter can produce a dump, and if the
operator story ever needs SQL, the same converter makes a second migration
as cheap as this one.

## Value encoding: MessagePack with field names

Values written to a redb-backed tree are `MAGIC | MessagePack`, with structs
as maps of field name to value (`yats/src/codec.rs`). Self-describing: a
field added to a stored type decodes from older rows as long as it is an
`Option` or `#[serde(default)]`, removed fields are skipped, field order does
not matter. That is the fix for the `session_v2` problem, for every tree at
once, without a per-type upgrade function. Keys stay bincode on both engines
(their byte order is their sort order).

Why not the alternatives the task listed:
- `yats::Envelope` alone: versioned, but each type needs an `upgrade` for
  each old layout, written at the moment it changes, forever. Envelope still
  works on redb (its payload is MessagePack there, and decodes by field
  name).
- serde_json: no NaN/infinity (stats have them), no non-string map keys,
  and larger.
- postcard with a version tag: positional again, so it needs the same
  per-type upgrades as Envelope.

MessagePack via `rmp-serde` handles every type bfdb stores (NaN, bytes as
`bin`, tuple keys in maps, internally tagged enums); the round-trip test
below proves it per tree.

Decoding never needs to know the encoding: a row that starts with `MAGIC` is
MessagePack, anything else is bincode. So a converted database can hold
both, and sled databases are untouched: **sled-backed trees keep writing
bincode**, exactly as before.

## What was built

### yats (`yats/src`)

- `backend.rs`: `Db` and `RawTree`, one API over both engines. sled's method
  names and shapes (`IVec` values, `AsRef<[u8]>` keys), so the raw-JSON
  modules moved over by changing an import.
  - Every redb write is a commit with `Durability::None`; a background
    thread makes them durable every 500 ms (sled's `flush_every_ms`), and
    `flush()` does it at once. A crash loses at most the last half second,
    as with sled.
  - `update_and_fetch` is a compare-and-swap loop, like sled's: the closure
    runs outside the write lock (so it may use the database) and may run
    more than once.
  - Ids: a `__yats_meta` table holds the next one. A conversion seeds it
    above the source's, so no id is reissued, in either direction.
  - `snapshot_redb`: every table copied from one read transaction.
- `codec.rs`: the value codecs above.
- `Tree<K, V>` sits on `RawTree`; its API is unchanged. `Tree::open` takes a
  `yats::Db` or a bare `sled::Db`. sled-only features (transactions,
  subscriptions, merge operators, checksums) fail cleanly on redb; bfdb uses
  none of them.
- `Tree::reencode`: rewrites each row not in the tree's codec into it, only
  if it comes back exactly (decoded from the new bytes and encoded as
  bincode again, it must equal the original, or for hash maps the same bytes
  permuted). Rows that do not decode, or would not come back, stay as they
  are and are counted. `Tree::open` on a `yats::Db` registers each tree's
  re-encoder, so `Db::reencode_all` can re-encode every tree with its own
  types.
- `convert.rs`: `copy_all` (every tree, raw bytes, then ids) and `verify`
  (same trees, counts, keys in order, values byte for byte or re-encoded,
  plus point lookups of every 97th key).

### bfdb

- `storage/mod.rs`: `--storage sled|redb` (or `$BFDB_STORAGE`), **default
  sled**. redb keeps the database in `<db>.redb`, so `<db>.backups`,
  `<db>.intel` and the rest stay where they are and the sled directory is
  left alone. bfdb refuses `--storage redb` when `<db>.redb` is missing but
  a sled database exists (it would start empty), and warns when it runs on
  sled with a `<db>.redb` beside it.
- `--migrate-storage FROM TO` (`storage::migrate`): refuses a database in
  use (lock check), copies the source's files to a staging copy and reads
  only that (the source is never opened by an engine), converts into
  `<target>.partial`, verifies byte for byte, re-encodes every typed tree
  with bfdb's own types, verifies again, then renames into place.
  Idempotent: on an existing target it only re-verifies. `--migrate-dry-run`
  does everything into a scratch copy and deletes it. `--migrate-into`,
  `--migrate-keep-encoding` (copy bytes, decode-check only). Rolling back is
  the same command the other way (`redb sled`), into `<db>.from-redb`, never
  the live sled directory; it re-encodes rows back to bincode so an older
  bfdb can read them too.
- `StatsDb::new` keeps its signature; `new_on` takes the engine, and
  `open_trees` (open every tree, nothing else) is what the converter's
  typed pass uses. `db.sled()` now returns a `yats::Db`.
- `maint.rs`: backups on redb are point-in-time snapshots
  (`<backup>/store.redb`, credentials excluded as before), `--backup-files`
  works unchanged, `--restore-latest-backup` restores only backups of the
  engine bfdb runs on.
- `main.rs`: three lines of dispatch and a flattened `StorageArgs`.
  `range/store.rs`, `replay/mod.rs`, `flightplans.rs`, `progression.rs`,
  `squadrons.rs`: their `sled` import became `yats`.

### Tests

- yats: the backend contract on both engines (order, prefix and inverted
  ranges, CAS, update, ids, reopen), the codecs (field changes, NaN, bytes,
  tuple map keys), Envelope on redb, copy and verify both ways, a mixed
  legacy/MessagePack tree.
- bfdb `storage/db_tests.rs`:
  - parity: the same stats round (17 stats through `add_stat`) plus the
    session, ban, audit, wiki, trail and image stores, on sled and on redb;
    21 read APIs and every key of every tree must match.
  - every stored type round-trips: that data copied to the other engine
    and re-encoded, both directions, with zero undecodable or unverified
    rows, and the read APIs unchanged.
  - conversion of a sled database built in the test with legacy rows (a
    pre-hash `auth_sessions` login, an unreadable pre-`session_v2` session,
    a `session_v2` row, raw range JSON): dry run leaves nothing, the real
    run verifies, the sled files are byte-identical afterwards, bfdb on
    redb answers as it did on sled and the legacy login still works; a
    second run only verifies; rolling back carries a post-cutover write back
    as bincode, with ids past redb's.
  - refusals: a database in use, converting into a live location, redb
    next to an unconverted sled database.
- `maint.rs`: redb backup is a snapshot, restores, and sled restore ignores
  it.
- `bfdb/tests/golden_rows.rs`: golden MessagePack bytes for the shared
  types beside the bincode ones.

## Phases

1. ~~A storage seam in yats.~~ Done (`backend.rs`).
2. ~~A redb backend.~~ Done; not behind a cargo feature (redb is small and
   dependency-free, and the converter needs both engines in one binary).
3. ~~Migration command.~~ Done (`--migrate-storage`). It does not take a
   maintenance backup itself: the source is never modified, and the runbook
   has the operator copy it first anyway.
4. **Cutover, opt-in.** Built in bfdb (`--storage redb`). Left to do outside
   bfdb: the DCSServerBot plugin's pre-swap snapshot and `/feops db_restore`
   (`procman.py`, which copy `<home>/bfdb`) and the Manager's backup and
   status code must learn `<home>/bfdb.redb`. Until then the runbook says
   what to do by hand.
5. **Rollback rehearsal** on a copy of the live DB: sled -> redb -> sled,
   then start bfdb on the result and compare the API. The tests do this on
   a built database; it has not been done on live data yet.
6. ~~Values off bincode 1.~~ Done for redb: MessagePack on write, legacy
   rows re-encoded by the converter. sled keeps bincode.
7. **Default and removal.** New installs default to redb once one live
   server has run a full round on it; sled support (and the sled half of
   `RawTree`) is removed a release after every known server has migrated.
   `RawTree` still hands out `sled::IVec`; that becomes a yats type then.

## Risks

- **Write throughput.** sled absorbs many small writes; redb pays a commit
  per write (non-durable, so no fsync, but still a page write). Measured
  with `cargo test -p yats --release -- --ignored writes_per_second` (20k
  rows, typed tree, dev PC): sled 317k inserts/s and 557k updates/s, redb
  120k and 108k. A live night is a few hundred writes a second at most, so
  this only shows in `--rebuild-stats`; still, time a rebuild of a busy
  round's archive on redb before the first cutover, and batch ingest writes
  per stats line if it is too slow.
- **File size after converting.** The copy and then the rewrite of every
  row leave free pages; the converter compacts the file at the end (a
  181 MB sled test database became a 288 MB redb file before compaction).
- **Long-running readers.** redb readers hold their snapshot's pages until
  dropped. Iterators are lazy and short-lived in bfdb, but a slow consumer
  of a big scan holds pages (and grows the file) for as long as it runs.
- **Disk size.** redb files grow and are compacted only on demand; compare
  after converting, and watch growth for the first week.
- **One writer at a time.** redb serializes writers. sled let independent
  trees write in parallel. bfdb's writers are a handful of tasks; watch
  request latency on busy nights.
- **Mixed encodings.** A converted tree can hold a legacy bincode row the
  re-encode left alone (undecodable, so already unreadable). Adding a field
  to that tree's type does not make those rows any worse.
- **Key order.** Guarded by the parity and backend tests (bytewise on both).
