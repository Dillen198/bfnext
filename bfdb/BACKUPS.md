# bfdb backups and restore

## What is backed up

Every `--backup-interval-hours` (default 24; `0` turns backups off) bfdb
writes a complete copy of its sled database to
`<backup dir>/<UTC timestamp>/` and keeps the newest `--backup-keep`
(default 7).

- **Where:** `--backup-dir <path>`, default `<db>.backups` next to the
  database. Put it on another disk or a network share
  (`--backup-dir \\nas\bfdb-backups`) so a dead disk does not take the
  backups with it.
- **Left out on purpose:** login sessions and pending Discord logins
  (`auth_sessions`, `auth_sessions_h`, `auth_states`). After a restore
  everybody logs in again.
- **Not point-in-time across trees.** sled has no snapshot: the trees are
  exported one after another while bfdb keeps writing. Each tree is
  consistent; together they may differ by a few seconds. The ingest cursors
  are exported first, so a restored database re-reads those seconds of
  `stats.jsonl` rather than missing them. Kills, sorties and deploys are
  idempotent on a re-read; a capture or supply counter from that window can
  count twice.

### `--backup-files` (opt-in)

Also copies what lives *beside* the database, into the backup's `files/`
folder:

| In the backup | From |
|---|---|
| `files/intel/` | recon photos, `--intel-dir` (default `<db>.intel`) |
| `files/replays/` | flight replays, `--replay-dir` (default `<db>.replays`) |
| `files/range-clips/` | range debrief clip cache, `<db>.range-clips` |
| `files/engine-cfg/<instance>` | each instance's engine config (`engine_config` / `--engine-config`) |
| `files/instances.json` | the `--instances` file |
| `files/campaign-config.json` | the `--config` file |

A file that has not changed since the previous backup (same size and
modification time) is hard-linked to that backup's copy, so a week of
backups of a recording costs one copy. Where hard links are not possible
(a different volume, most network shares) it is copied each time: size the
backup disk for `--backup-keep` × the replay folder in that case, or lower
`--replay-days`.

## Restoring

### The database

1. Stop bfdb (from the dashboard OPS page, or stop whatever supervises it).
2. Start it once with `--restore-latest-backup` (plus the usual flags). It
   moves the current database aside to `<db>.damaged-<time>` -- never
   deleted -- copies the newest backup in its place, and carries on starting.
   To restore an older backup instead, move the newer timestamp folders out
   of the backup dir first, or copy the one you want over `<db>` by hand
   with bfdb stopped.
3. Remove `--restore-latest-backup` again so the next start does not repeat
   it.

Anything recorded after the backup was taken is re-read from `stats.jsonl`
on start, as far back as the file still goes.

### The files

Not restored automatically: they are copied back by hand, with bfdb
stopped, from the backup's `files/` folder:

- `files/intel/*` → the intel dir, `files/replays/*` → the replay dir
  (both are only ever added to, so copying over the live folder is safe);
- `files/range-clips/` can be skipped -- clips are re-cut on demand;
- `files/engine-cfg/<instance>` → that instance's engine config path. Check
  it is the version you want: the dashboard's config editor also leaves
  `<cfg>.bak.<time>` copies next to the live file.
