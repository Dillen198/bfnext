# Hands-off operation: auto-start, auto-update, rollback, log analysis

> **Easiest route: [Fowl Engine Manager](../bfmanager/README.md).** It is a Windows app with an
> installer that sets up the boot-time service, supervises DCSServerBot, keeps the bot
> plugin current and updates itself. It replaces the NSSM service in step 1 of section 1 below.
> Everything else on this page (engine auto-update, probation, rollback, the OPS page,
> the log analyzer) works the same with or without it.

The server box looks after itself:

```
 Windows boots (reboot, blue screen, power cut, Windows Update)
   └─ service "DCSServerBot" (NSSM, delayed auto-start, no login needed)
        ├─ DCS servers            started + crash-restarted by DCSServerBot
        ├─ bfdb.exe + netidx      started + health-checked by the FowlEngine plugin (procman.py)
        ├─ auto-updater           new engine release → verify → stage → apply → probation → rollback
        ├─ log analyzer           every log → distinct issues + a persistent archive
        └─ OPS API                what the dashboard's OPS page talks to (through bfdb)
```

| Piece | Code |
|---|---|
| Windows service, preflight check | `deploy/windows-service/install-service.ps1`, `check-autostart.ps1` |
| Releases | `deploy/publish-release.ps1`, `.github/workflows/release.yml` |
| Auto-updater + DLL probation | `DCSServerBot/plugins/fowlengine/autoupdate.py` |
| bfdb probation, DB snapshots, rollback | `DCSServerBot/plugins/fowlengine/procman.py` |
| DLL swap at DCS start → probation | `DCSServerBot/extensions/bfbinaries/extension.py` |
| Log analyzer + archive | `DCSServerBot/plugins/fowlengine/loganalyzer.py` |
| OPS API | `DCSServerBot/plugins/fowlengine/opsapi.py` |
| bfdb side | `/api/admin/ops/*` proxy and extra `/api/logs/<source>` sources in `bfdb/src/main.rs` |
| Dashboard | `bfweb/src/pages/OpsPage.tsx` (nav: **OPS**, admins only) |
| Tests | `DCSServerBot/tests/test_fowlengine_ops.py` |

## 1. One-time setup on the server

1. **Service.** From an elevated PowerShell on the server:
   `deploy\windows-service\install-service.ps1`, then `check-autostart.ps1`.
   See [windows-service/README.md](windows-service/README.md). Then do a real
   test: `Restart-Computer`, don't log in, and check that the dashboard comes back.
2. **BIOS/UEFI:** set *Restore on AC power loss* to *Power On*, so a power cut
   also ends with the box booting. Windows can't check this setting.
3. **Bot WebService.** The OPS page reaches the plugin through DCSServerBot's
   WebService (the one the RestAPI plugin uses). bfdb already calls it with
   `bfdb.dcsserverbot_url` + `bfdb.dcsserverbot_api_key`, and the plugin
   requires that same key. No key means no OPS page: the page stays disabled
   rather than run exposed.
4. **fowlengine.yaml:** add the `autoupdate:`, `issues:` and `ops_api:` blocks
   (see `fowlengine.sample.yaml`). Start with `autoupdate.enabled: false`,
   open the OPS page, press **Check now**, and look at what it would stage.
   Then switch it on.
5. **Claude's read access (optional but recommended):** start bfdb with
   `--log-read-token <long random string>` (the `bfdb:` block gets a
   `log_read_token:`, or add it to the args) and give Claude the public bfdb
   URL + token. See section 5.

## 2. Publishing a release

A release is **one commit**, built in a clean `git worktree`:

```powershell
git push                                   # the release tag points at a pushed commit
.\deploy\publish-release.ps1               # stable, to GitHub Releases
.\deploy\publish-release.ps1 -Channel beta -Notes "new CAP logic"
.\deploy\publish-release.ps1 -Folder \\SERVER\fowl-releases   # no GitHub: a shared folder
.\deploy\publish-release.ps1 -DryRun       # build + manifest only
.\deploy\publish-release.ps1 -BotPlugin    # also ship the bot plugin (fowlengine-bot.zip)
```

It builds `bflib.dll`, `bfrange.dll` (when it is a workspace member at that
commit), `bfdb.exe` (dashboard + site embedded) and `bftools.exe`, then writes
`manifest.json` (a sha256 for every file) and publishes the tag `engine-<date>-<sha>`.
The GitHub Action **Engine release** runs the same script on a Windows runner.
It is manual-dispatch only, and **beta** is the safer channel for it: `Cargo.lock`
is gitignored, so CI resolves dependencies fresh.

## 3. What the server does with it

1. **Check** every `check_minutes`. Drafts, other tags, pre-releases (unless
   on the `beta` channel), and anything marked bad are all skipped. A server
   never "updates" to an older release.
2. **Fetch + verify** into `<staging>\_downloads\<tag>\`. A sha256 mismatch
   aborts the whole release.
3. **Stage** the same way a Discord drag-and-drop does: `<name>.pending` plus a sidecar with
   `source: autoupdate`. `bflib.dll` goes to each campaign server's own BFBinaries staging
   dir, `bfdb.exe` / `bftools.exe` to the global one. A **manual upload waiting
   there wins** and is never overwritten.
4. **Apply** (`apply:` for DLLs, `bfdb_apply:` for bfdb):
   - `next_restart`: the next scheduled DCS restart (same as a manual upload).
   - `when_idle`: once that server has had no players for `idle_minutes`,
     inside `apply_window` if set. The DLL needs a full DCS stop/start; a
     mission restart keeps the old DLL loaded.
   - `immediately`: right now, even with players on.

   Only auto-staged files are applied early; a manual upload keeps its
   "next restart" meaning. `bftools.exe` is swapped as soon as it isn't in use.
5. **Probation**: every swapped-in engine, manual uploads included.
   - **DLL:** passes once bflib's load sidecar (`Logs\bfnext-bflib-build.json`)
     is newer than the swap and the server has run for `probation_minutes`.
     **Rolled back** if DCS goes down unexpectedly during probation, or if the
     mission never loads it within `load_timeout_minutes` of running time.
     (bfrange writes no sidecar yet, so a range DLL is judged on "stayed up".)
   - **bfdb:** passes after `probation_minutes` of answering `/api/health`.
     **Rolled back** if it exits, or never answers within `bfdb_unhealthy_minutes`.
6. **Rollback:**
   - **DLL:** the pre-swap backup is staged back and DCS is restarted onto it.
   - **bfdb:** the previous exe *and* the DB snapshot taken just before the
     swap are restored; the failed ones are kept as `bfdb.exe.failed-*` /
     `bfdb.failed-*`. Nothing written since the swap is lost: the JSONL cursor
     lives in the DB, so the restored DB re-reads the stats log, and the
     duplicate guards stop double counting.
   - The release is **marked bad**, pulled from every staging dir it was still
     waiting in, and never offered again. Clear the mark with **Allow again**
     on the OPS page or `/feops update_unmark`.

Everything is announced in `ops_channel`. Discord: `/feops update_status`,
`update_check`, `update_pause`, `update_rollback`, `update_unmark`, `issues`.

## 4. The OPS page (dashboard → OPS)

- **Summary:** box uptime, the auto-start service, bfdb health, auto-update state, open issues.
- **Server box & auto-start:** service state, start type, account; restart after BSOD;
  pending Windows Update reboot; disk; and recent BSOD / power-loss events from the event log.
  Each problem comes with the fix.
- **Processes:** bfdb (pid, uptime, relaunches, last exit code, netidx), every DCS server;
  **Restart bfdb**, **Restart bot** (DCS keeps running).
- **Engine builds:** per binary, the *running* build vs the file on disk vs what's staged vs
  the latest release; **Apply now**, **Discard**, **Roll back**.
- **Automatic updates:** on/off, pause, channel, apply policies, idle minutes, window,
  **Check now**, the latest release and its notes, probation, bad releases.
- **History & backups:** every find / stage / apply / probation / rollback; DB snapshots.
- **Issues:** see below.
- **Bot config:** `fowlengine.yaml` in an editor. Secrets show as `__SECRET__` and are kept
  unless you type a new value. The file is validated and backed up
  (`config\backup\fowlengine\`) before the plugin reloads it; bfdb optionally restarts.
- **Logs:** live tails (bot, service, bfdb, bfdb boot, netidx), plus the **log archive**.

## 5. Log analyzer, archive, and handing issues to Claude

Every `scan_seconds` the bot reads what was appended to each log: the engine's
`bfnext.txt` (or `bfrange.txt`), DCS's `dcs.log` (only script errors and crashes,
none of the asset chatter), bfdb's log and the bot's own log. It also watches for
new crash dumps. Multi-line entries (Rust panics, Python tracebacks) are kept together.
Every WARN/ERROR is **fingerprinted**: timestamps, numbers, ids, IPs, UCIDs and
quoted names are normalised away, so one bug is one issue with a count, first/last
seen, the builds it was seen on, and three scrubbed samples with context.
A fixed issue that comes back is flagged **REGRESSED**.

**Nothing is lost to a restart.** Everything read is appended to
`<bfdb.home>\_logarchive\<source>\<UTC date>.log`, gzipped after the day ends
and kept for `archive_days` (default 90; 0 = forever). A log rotated between two
scans is finished from its renamed file first, so the last lines before a crash survive.
(DCS overwrites `dcs.log` at every start and bfdb's in-memory tails reset with it; the archive doesn't.)

**The loop with Claude:**

1. The analyzer finds an issue. New errors are posted in `ops_channel`.
2. Give Claude the report, in any of these ways:
   - OPS page → **Copy report for Claude** (or **Download .md**), paste it in.
   - `/feops issues` in Discord (report attached).
   - Let Claude fetch it: `GET <bfdb>/api/logs/issues?token=<log-read-token>`.
     The same token also serves `/api/logs/archive` (index),
     `/api/logs/archive?source=engine_<server>&date=YYYY-MM-DD&grep=...`, and
     `/api/logs/bot`.
   - Optional: `issues.github` files a GitHub issue per new recurring error.
     Use a **private** repo, because samples can contain player names.
3. Claude fixes it on a branch, you review and merge, and run
   `publish-release.ps1` (or the **Engine release** Action).
4. The servers pick up the release, apply it when empty, and watch it through
   probation. Mark the issue **fixed** on the OPS page; if it comes back, it shows as REGRESSED.

Scrubbing happens before anything leaves the box: configured secrets, IPv4
addresses, UCIDs, Discord webhook URLs and bearer tokens.

## 6. When something goes wrong

- **The OPS page says the bot has no OPS API:** update the plugin, and check
  that `bfdb.dcsserverbot_url` includes the RestAPI `prefix`.
- **The plugin itself is broken after a `bot_plugin` update:** the previous plugin is in
  `<DCSServerBot>\_fowl_backups\plugin-<ts>.zip`. Unzip it over the bot folder and restart the service.
- **Manual bfdb rollback:** stop the service, copy `bfdb.exe.backup-<ts>` over
  `bfdb.exe` and `_backups\db-<ts>-<tag>` over `bfdb`, then start the service.
- **Manual DLL rollback:** `/feops update_rollback which:dll server:<name> confirm:True`,
  or copy `Scripts\bflib.dll.backup-<ts>` over `bflib.dll` while DCS is down.
