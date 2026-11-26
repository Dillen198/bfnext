# Moving a live bfdb from sled to redb

Operator runbook for the campaign box and the range box. Why and how it
works: [`docs/plans/storage-migration.md`](plans/storage-migration.md).

**Nothing changes until you do this.** bfdb runs on sled by default, and a
bfdb that knows about redb behaves exactly as before until it is started
with `--storage redb` (or `BFDB_STORAGE=redb`). The conversion never writes
to the sled database, so going back is always possible.

## What you need

- A bfdb.exe built from this change or later (`bfdb --help` lists
  `--storage` and `--migrate-storage`). It must already be the build the
  box runs, past the bot's probation: a bot rollback to an older bfdb.exe
  after the cutover would start on the old sled data (see Risks).
- Free disk on the database's drive: about twice the size of the sled
  folder (a staging copy of it plus the new file). The converter checks.
- A quiet window: bfdb is down for the whole conversion. Expect roughly a
  minute per GB of sled data, plus the copy.

## Where things are

The bot starts bfdb as `bfdb.exe --db <home>\bfdb`, where `<home>` is the
instance home in `fowlengine.yaml` (`bfdb.home`). Below, `DB` means that
path.

| | sled (now) | redb (after) |
| --- | --- | --- |
| database | folder `DB` | file `DB.redb` |
| bfdb's own backups | `DB.backups\<UTC>\` (a sled folder) | `DB.backups\<UTC>\store.redb` |
| recon photos, replays, configs | unchanged | unchanged |

## Cutover

1. **Stop bfdb.** Stop DCSServerBot (procman stops bfdb with it), or use the
   Manager. Check nothing is left: `Get-Process bfdb -ErrorAction SilentlyContinue`
   returns nothing.

2. **Back up the sled folder** yourself, whatever else exists:

   ```powershell
   $DB = "C:\path\to\home\bfdb"     # the instance's bfdb folder
   Copy-Item $DB "$DB.pre-redb-$(Get-Date -Format yyyyMMddTHHmmss)" -Recurse
   ```

3. **Dry run.** Converts into a scratch copy, verifies, prints the report,
   deletes the scratch copy:

   ```powershell
   & "<bfdb.exe>" --db $DB --migrate-storage sled redb --migrate-dry-run
   ```

   Read the report. Every tree should show `undecoded 0` except trees that
   were already unreadable on sled: `session` (records from before
   `session_v2`) commonly has some. `unverified` should be 0 everywhere; a
   non-zero count means rows that are kept as they are (still readable),
   and is worth reporting. The last line must say `dry run OK`.

4. **Convert:**

   ```powershell
   & "<bfdb.exe>" --db $DB --migrate-storage sled redb
   ```

   It ends with `OK: wrote DB.redb. DB was not modified.` and exit code 0.
   Anything else: stop here, nothing was changed; keep the output.

5. **Switch bfdb to redb.** For the bot, set the environment variable for
   the account the bot runs as, then restart the bot so it inherits it:

   ```powershell
   [Environment]::SetEnvironmentVariable("BFDB_STORAGE", "redb", "Machine")
   ```

   (A bfdb started by hand takes `--storage redb` instead.)

6. **Start and check.** Start the bot. In the bfdb log, near the top:
   `storage: redb, ...\bfdb.redb`. Then check the dashboard: the current
   round, the leaderboard, a pilot page, the wiki, and that you are still
   logged in (sessions are converted too). `/api/health` should be green
   and the stats lag should come down as bfdb catches up on stats.jsonl.

7. **First backup.** bfdb's backup is due at its usual interval; to have
   one now, leave the default `--backup-interval-hours` and check
   `DB.backups\` gains a folder with `store.redb` in it within a day. Until
   then your step-2 copy plus `DB` itself are the fallback.

Leave the sled folder `DB` where it is for at least a full round. bfdb on
redb never touches it.

## Rolling back

**Fast, losing what was written on redb:** stop bfdb, remove the
`BFDB_STORAGE` variable (or the `--storage redb` flag), start. bfdb runs on
the sled folder exactly as it was at the cutover, and warns in its log that
`DB.redb` exists. Stats since the cutover are re-read from stats.jsonl
(bfdb resumes from the sled database's cursor, through rotated files that
are still there); everything else written since the cutover is lost:
logins, bans, wiki edits, news, intel, squadron and progression changes.

**Keeping what was written on redb:**

1. Stop bfdb.
2. Convert back. This writes a new sled folder `DB.from-redb` and never
   touches `DB`:

   ```powershell
   & "<bfdb.exe>" --db $DB --migrate-storage redb sled
   ```

   Rows are re-encoded to bincode, so even a bfdb.exe from before this
   change can read the result.
3. Swap the folders: `Rename-Item $DB "$DB.redb-era-sled"` then
   `Rename-Item "$DB.from-redb" $DB`.
4. Remove `BFDB_STORAGE`, start bfdb, check as in step 6 above.
5. Move `DB.redb` aside (do not delete it yet), so bfdb stops warning.

## Restoring a backup on redb

`--restore-latest-backup` works as before and picks the newest backup taken
on the engine bfdb is started with: on redb it moves `DB.redb` aside to
`DB.redb.damaged-<UTC>` and copies the newest `DB.backups\<UTC>\store.redb`
into place. Old sled backups in the same folder are ignored on redb (and
redb backups on sled).

To restore by hand: stop bfdb, move `DB.redb` aside, copy a backup's
`store.redb` to `DB.redb`, start.

## Re-checking a conversion

Running the same `--migrate-storage sled redb` again with `DB.redb` in
place does not redo anything: it verifies `DB.redb` against `DB` and
reports. Only meaningful before bfdb has written to `DB.redb`.

## Risks to watch

- **The bot and Manager do not know `DB.redb` yet.**
  - The bot's copy of the database before a bfdb.exe swap
    (`_backups\db-*`) copies the sled folder, which is stale after the
    cutover. bfdb's own backups (`DB.backups`) do cover redb.
  - `/feops db_restore` restores into the sled folder, which bfdb on redb
    does not read. Restore by hand (above) instead.
  - The Manager backs up the whole instance home, so `DB.redb` is in its
    backups; its status view still describes the sled folder.
  - A bot rollback of bfdb.exe to a build from before this change ignores
    `BFDB_STORAGE` and starts on the stale sled data: new stats land in the
    sled folder and the redb file falls behind. If that happens, stop bfdb,
    put the redb-aware bfdb.exe back, and start it on redb; what the old
    build wrote to sled in between is not in redb.
- **Write speed.** redb commits each write. A normal night is far below its
  limits, but a `--rebuild-stats` re-ingest is many writes in a row; expect
  it to be slower than on sled.
- **File growth.** Watch `DB.redb`'s size for the first week. It grows with
  data like the sled folder did, but is not compacted automatically.
- **Disk space during conversion**: the staging copy is deleted at the end,
  also after a failure.

## The range box

Same steps. The range box runs its own bot and bfdb, so its `DB` is that
instance's `bfdb` folder; the training-range results live in the same
database (`range_*` trees), and are converted with everything else. Do the
campaign box first and let it run a full round before doing the range box.
