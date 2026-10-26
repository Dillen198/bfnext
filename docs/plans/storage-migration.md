# Plan: moving bfdb off sled 0.34 and bincode 1

Status: plan (audit finding f55). The groundwork in "Done already" is on the
`audit-foundations` branch; the rest is not started.

## Why

bfdb keeps all its state (pilots, sorties, rounds, kills, sessions, news,
wiki, auth) in sled 0.34 through yats, with values as bincode 1. Both are
unmaintained: sled's 0.34 line has had no release since 2021 and its
rewrite never shipped; bincode 1 is frozen. Nothing is broken today, but
there is no upstream for a corruption bug, and bfdb has already been bitten
by torn sled pages (the maintenance backups exist because of one).

## What bfdb actually uses

From `bfdb/src` on this branch: about 45 typed trees (`Tree<K, V>` in
`db.rs`), and of the yats/sled API only

- `get`, `insert`, `remove`, `contains_key`;
- ordered iteration: `scan_prefix` (21 call sites), `range` (8), `iter`,
  `first`/`last`;
- `update_and_fetch` / `fetch_and_update` (23): atomic read-modify-write;
- `flush` (10), and `sled::Db::generate_id` for ids (`db_id.rs`);
- `export` / `import` for consistent backups (`maint.rs`).

Not used: watch/subscribe, merge operators, compare-and-swap, multi-tree
transactions. Keys are tuples encoded as big-endian fixint bincode, so their
byte order is their logical order; that is what `scan_prefix` and `range`
rely on, and any replacement must sort keys as raw bytes.

## Target: redb

| | redb | SQLite (rusqlite) |
| --- | --- | --- |
| model | ordered key/value tables, ACID, MVCC readers | SQL tables |
| fit with yats | one redb table per tree, raw byte keys: same model | one `(tree, k BLOB, v BLOB)` table or a table per tree |
| build | pure Rust | bundled C (fine on Windows, more to build) |
| inspecting a live DB | needs a tool | any SQLite browser |
| maintained | yes | yes |

redb, because it is the same shape as sled: yats can keep its API and bfdb
barely changes. SQLite's advantage (ad-hoc inspection) can be had from a
dump (below) without making it the engine. If the operator story later
needs SQL, the dump format makes a second migration as cheap as the first.

bincode 1 stays as the value encoding for now. It is unmaintained but its
format is fixed, which is what a storage format wants; changing engine and
encoding at once doubles the risk. Moving values off it is phase 6.

## Done already

- `bfdb/tests/golden_rows.rs` pins the stored encoding of the shared types
  (Side, Ucid, SlotId, LiquidType, the ids, a whole `Dead` row, ...). Any
  engine or codec change has to keep it green.
- `yats::Envelope`: versioned values that also decode legacy rows, so a tree
  can change its value type without a new tree (`session_v2` was that
  workaround).
- `yats::export::{write_dump, read_dump}`: every tree and pair of a sled DB
  as raw bytes in a length-prefixed file with a record count, readable
  without sled or bfdb's types. This is the migration's transport.

## Phases

1. **A storage seam in yats.** Put sled behind a small internal trait
   (open tree, get, insert, remove, ordered range from a byte prefix or
   bounds, read-modify-write, flush, next id). `Tree<K, V>` keeps its public
   API; bfdb does not change. The existing yats tests run against it.
2. **A redb backend** behind a yats cargo feature, one redb table per tree,
   `&[u8]` keys and values (yats keeps encoding). Notes:
   - `update_and_fetch` becomes a write transaction: read, apply the closure,
     write, commit. redb has one writer at a time, so the closure must stay
     cheap (it already is: they are counter bumps and small struct edits).
   - `generate_id`: a `meta` table holding the next id, bumped in the same
     kind of transaction. At migration it is seeded with a fresh
     `sled::Db::generate_id()` so no issued id repeats.
   - `flush` maps to commit durability; batch the high-rate writers (stats
     ingest) into one transaction per batch rather than one per row.
   Run the yats suite and `cargo test -p bfdb` on both backends in CI.
3. **Migration command**: `bfdb migrate-store --to redb --from <sled dir>
   --into <redb file>`. It refuses a running DB (lock file), takes a
   maintenance backup first, then `write_dump` from sled and `read_dump`
   into redb. It verifies before declaring success: per-tree pair counts,
   byte-for-byte comparison of every pair read back, and a typed pass that
   decodes every row of every bfdb tree with bfdb's own types (an
   undecodable row is reported, not dropped).
4. **Cutover, opt-in.** A bfdb setting `store = "sled" | "redb"`, default
   `sled`. An operator migrates, sets `redb`, starts bfdb. The sled
   directory is left exactly as it was. The DCSServerBot plugin and Fowl
   Engine Manager learn the new path for backups and status (check
   `procman.py` and the Manager's backup code for the sled directory name
   before this phase).
5. **Rollback.** Set `store = "sled"` again. Writes made since the cutover
   live only in redb, so the same command runs the other way
   (`--to sled`): the dump format is engine-agnostic. Rehearse the full
   round trip (sled -> redb -> sled, byte-compare) on a copy of the live DB
   before the first real cutover.
6. **Values off bincode 1**, tree by tree, after redb has run a while:
   wrap a tree's value in `Envelope` (legacy rows keep decoding), then
   change the codec behind `yats::serialize` for enveloped rows only
   (bincode 2 with the same big-endian fixint layout is the low-risk
   choice; golden_rows must stay green for legacy rows).
7. **Default and removal.** New installs default to redb once one live
   server has run a full round on it; sled support is removed a release
   after every known server has migrated.

## Risks

- **Key order.** redb compares `&[u8]` keys bytewise, as sled does; a test
  that iterates a tuple-keyed tree on both backends and compares the order
  guards it.
- **Write throughput.** sled absorbs many small writes; redb pays per commit.
  Measure the stats ingest on a replay of a busy round before cutover.
- **Long-running readers.** redb readers see a snapshot and hold old pages;
  bfdb's long scans (stats rebuilds) must not keep a read transaction open
  for minutes.
- **Disk size.** Compare after migration; compaction behaviour differs.

## Effort

Phases 1 to 3: about a week. Cutover and rollback rehearsal: a day each, on
a copy of the live DB. Phase 6 is separate, per tree.
