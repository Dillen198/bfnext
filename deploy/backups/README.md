# Off-box backups of the bot

Two things on a server box can't be rebuilt from this repo or a release:

* **DCSServerBot's PostgreSQL database**: player stats, links between Discord
  and DCS, punishments, and every plugin's tables.
* **DCSServerBot's `config\` folder**: `nodes.yaml`, `main.yaml`, the plugin
  yamls, and `config\.secret\` (Discord token, database password). These
  are live secrets.

bfdb's own database (the campaign) is covered separately: bfdb snapshots it
and `/feops db_restore` restores it. For the whole box, Fowl Engine Manager's
**BACKUP** page writes one zip on demand (bot folder, DCS instances, bfdb,
`pg_dump`). Neither of those runs on a schedule, and neither keeps a copy
off the box. [`backup-bot.ps1`](backup-bot.ps1) covers that gap. It is
**opt-in**: nothing is scheduled until you install it.

## What it writes

Every run writes one file, `FowlBot-<PC>-<yyyyMMdd-HHmmss>.fowlbak`, to the
destination you give it. It is a zip encrypted with your passphrase:
AES-256-CBC, with keys from PBKDF2-SHA256 at 600k iterations and an
HMAC-SHA256 over the whole file. The full format is in the script's
header. Inside:

| Entry | What |
|---|---|
| `db\<database>.dump` | `pg_dump -Fc --no-owner --no-privileges` of the bot's database |
| `config\...` | the bot's `config\` folder, `.secret\` included |
| `backup.txt` | when, from where, which `pg_dump`, sizes |

The script reads the database settings the way the bot does. The URL comes
from `config\nodes.yaml` (`database.url`). The password comes from
`config\.secret\database.pkl` when the URL says `SECRET`. A `PGPASSWORD`
or `PGPASSFILE` already in the environment takes precedence.

The unencrypted copy is staged in a new folder under the task account's
TEMP and is deleted at the end of every run, whether it worked or not.
Files older than `-KeepDays` (default 30) are pruned from the destination.

## Set it up

1. **Choose the destination.** It should not be on this box: a NAS share
   (`\\nas\fowl-backups`), a folder that a sync client copies elsewhere,
   or a removable drive.
2. **Make a passphrase file** that only Administrators and SYSTEM can read:

   ```powershell
   $f = 'C:\ProgramData\FowlEngine\backup-passphrase.txt'
   -join ((48..57 + 65..90 + 97..122) | Get-Random -Count 40 | ForEach-Object { [char]$_ }) | Set-Content $f
   icacls $f /inheritance:r /grant:r 'SYSTEM:R' 'Administrators:R'
   ```

   **Store the passphrase off the box as well**, in your password manager.
   If you lose it, nobody can open the backups.
3. **Run it once by hand** from an elevated PowerShell and check that a
   `.fowlbak` appears:

   ```powershell
   .\deploy\backups\backup-bot.ps1 -BotDir C:\DCSServerBot -Destination \\nas\fowl-backups `
       -PassphraseFile C:\ProgramData\FowlEngine\backup-passphrase.txt
   ```

4. **Schedule it.** Run the same command with `-Install` (and `-At 04:30`
   if you want a different time). This registers the Task Scheduler task
   "FowlEngine bot backup".
   The task runs as SYSTEM, which reaches a share as the PC's computer
   account (`DOMAIN\PC$`). If your share won't allow that, add
   `-TaskUser <account>` and Task Scheduler stores that account's
   password. `-Uninstall` removes the task.
5. **Test a restore** (below) once, on another PC if you can. An untested
   backup is not a backup.

`pg_dump` is found under `Program Files\PostgreSQL\<ver>\bin` (newest
version first) or on PATH. Pass `-PgDump <path>` to choose one. Its major
version must be at least the server's.

## Restore

On the PC you are restoring to (with this repo, or just `backup-bot.ps1`):

1. **Decrypt** the backup. The HMAC is checked first, so a wrong passphrase
   or a damaged file is refused before anything is written.

   ```powershell
   .\backup-bot.ps1 -Decrypt E:\FowlBot-VS1-20261025-043000.fowlbak -PassphraseFile .\passphrase.txt
   Expand-Archive E:\FowlBot-VS1-20261025-043000.zip E:\restore
   ```

2. **Database.** Install the same or a newer PostgreSQL major version.
   Create the role and an empty database, then load the dump. Give the
   role the old password, which is in the backup's
   `config\.secret\database.pkl` (a pickled string). The restored config
   then works unchanged.

   ```powershell
   $pg = 'C:\Program Files\PostgreSQL\16\bin'
   & "$pg\psql.exe" -U postgres -c "CREATE ROLE dcsserverbot LOGIN PASSWORD '<the old password>'"
   & "$pg\psql.exe" -U postgres -c "CREATE DATABASE dcsserverbot OWNER dcsserverbot"
   & "$pg\pg_restore.exe" -U dcsserverbot -d dcsserverbot --no-owner --no-privileges E:\restore\db\dcsserverbot.dump
   ```

3. **Config.** Stop the bot (Fowl Engine Manager → Stop, or stop the
   service). Move the existing `config\` aside and copy `E:\restore\config`
   into the bot folder. If the PC's name changed, rename the node block in
   `nodes.yaml` to match it. If the database host changed, update
   `database.url` there.
4. **Start the bot** and check the log for `FowlEngine` and database errors.

To bring back a whole box, restore Fowl Engine Manager's BACKUP zip first,
then the newest `.fowlbak` over it if it is more recent.

## Why it isn't part of the Manager's backup

The Manager's BACKUP is an interactive, whole-box job: it walks the DCS
instances, maps paths on restore and installs the service. A nightly,
unattended, encrypted copy of two things is a different job. The Manager
would need a scheduler, a stored passphrase and an off-box target to do it,
which is more moving parts than one Task Scheduler entry. The two can
coexist. The Manager can now also password-protect its own zip (see
`bfmanager/README.md`).
