# bfdb operations notes

Stats DB, web API and dashboard host for Fowl Engine. This file covers the
parts of running it that are not obvious from `bfdb --help`.

## Shutting down cleanly

bfdb flushes the sled database and exits 0 on Ctrl+C, SIGTERM, any Windows
console control event (`CTRL_BREAK_EVENT` is the one a supervisor should send,
to a bfdb started with `CREATE_NEW_PROCESS_GROUP`), or
`POST /api/admin/shutdown`. That endpoint only answers a direct loopback
connection (no `X-Forwarded-For`) and needs an admin session cookie (e.g. from
`POST /api/auth/local-login`) or `Authorization: Bearer <--shutdown-token>`.
It answers `202` and exits about half a second later. Allow ~10 s before a hard
kill. See the top of `src/maint.rs` for the exact contract.

## Backups

Every `--backup-interval-hours` (default 24; 0 turns it off) bfdb writes a
complete, consistent copy of the database to `<db>.backups/<UTC time>/`
(`--backup-dir` to move it) and keeps the newest `--backup-keep` (default 7).
Each backup is itself an openable sled database -- it is written through sled's
export/import, not by copying files that are being written to.

**Restore:** stop bfdb, then start it once with `--restore-latest-backup`. It
moves the current database aside to `<db>.damaged-<time>` (nothing is deleted)
and copies the newest backup into place. To restore an older one, stop bfdb and
copy that backup directory over `<db>` by hand. The stats.jsonl read cursor is
stored in the database, so a restored backup resumes reading from where it was
when the backup was taken and replays everything since -- as long as
stats.jsonl still holds it. Auth sessions, wiki edits and recon uploads made
after the backup are lost.

If sled cannot open the database at startup, bfdb says so and names the newest
backup; it never restores on its own.

## Schema version

yats stores values as positional bincode, so changing the field layout of any
persisted type makes existing rows undecodable. Values are **not** wrapped in a
version tag (adding one would itself be the breaking change). Instead the `meta`
tree holds `schema_version` (`SCHEMA_VERSION` in `src/db.rs`, currently 1),
written on first start.

To change a persisted layout: bump `SCHEMA_VERSION`, and in
`StatsDb::check_schema_version` add a migration that runs when the stored
version is lower (read old rows with the old type, write them with the new
one), then record the new version. bfdb refuses to open a database whose stored
version is newer than it understands.

Known key-order quirks (no migration, just be aware): `DateTime<Utc>` in a key
is an RFC3339 string with a length prefix, so key order is not time order (the
`session` tree is sorted in code); negative `i64` key parts sort after positive
ones under bincode's big-endian encoding.

## Secrets

Each secret flag also reads from a file flag or an environment variable, so
nothing sensitive has to be on the command line (visible to every local
process on Windows). The flag wins if both are set.

| flag | file flag | environment |
|---|---|---|
| `--admin-password` | `--admin-password-file` | `BFDB_ADMIN_PASSWORD` |
| `--dcsserverbot-api-key` | `--dcsserverbot-api-key-file` | `BFDB_DCSSERVERBOT_API_KEY` |
| `--ops-api-key` (falls back to the bot key) | | `BFDB_OPS_API_KEY` |
| `--discord-client-secret` | `--discord-client-secret-file` | `BFDB_DISCORD_CLIENT_SECRET` |
| `--log-read-token` | `--log-read-token-file` | `BFDB_LOG_READ_TOKEN` |
| `--shutdown-token` | | `BFDB_SHUTDOWN_TOKEN` |
| `--export-secret` | | `BFDB_EXPORT_SECRET` |

## Network exposure

* `POST /api/auth/local-login` is accepted from loopback only unless
  `--local-login-from <ip/cidr>` says otherwise, and locks an address out for
  15 minutes after 5 failures.
* Cookie-authenticated POSTs and WebSocket upgrades must come from an allowed
  Origin (`--cors-origin`s, `--public-api-url`, or the API's own host). Clients
  that send neither Origin nor Referer (the bot, scripts) are unaffected.
* The Export.lua UDP listener binds `--export-bind` (default `127.0.0.1`). For
  a DCS server on another PC, bind the LAN address and set `--export-secret`
  plus `Scripts/bf_export_secret.lua` on the DCS side.
* `/api/metrics` (Prometheus text) uses the log-read token.
