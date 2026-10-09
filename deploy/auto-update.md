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
| Campaign packs (publisher side) | `deploy/campaigns.sample.json` → `publish-release.ps1 -Campaigns` |
| Auto-updater + DLL probation + campaign packs | `DCSServerBot/plugins/fowlengine/autoupdate.py` |
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
   WebService (the one the RestAPI plugin uses), over plain HTTP. Bind it to
   loopback -- `config\services\webservice.yaml`: `DEFAULT: listen: 127.0.0.1` --
   since bfdb and Fowl Engine Manager both call it from this box. The OPS
   routes want their own key, `ops_api.api_key`; without one they fall back to
   `bfdb.dcsserverbot_api_key` and the bot log says so on every start. (bfdb
   still sends `bfdb.dcsserverbot_api_key` on its OPS calls, so a different
   `ops_api.api_key` locks the dashboard OPS page out until bfdb has its own
   `--ops-api-key`; Fowl Engine Manager is unaffected.) No key at all means
   no OPS page: it stays disabled rather than run exposed. The page's config
   editor can't change keys that name a program, a path, a URL or an update
   source; edit those in `fowlengine.yaml` on the box.
4. **fowlengine.yaml:** add the `autoupdate:`, `issues:` and `ops_api:` blocks
   (see `fowlengine.sample.yaml`), including `autoupdate.public_key` (see
   "Signing releases" below). Start with `autoupdate.enabled: false`,
   open the OPS page, press **Check now**, and look at what it would stage.
   Then switch it on. The exact keys for the whole hands-off setup (engine,
   bot plugin and campaign packs) are in "Server checklist" below.
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
.\deploy\publish-release.ps1 -Campaigns deploy\campaigns.json   # + each server's cfg + mission
.\deploy\publish-release.ps1 -Files @() -SkipBuild -Campaigns deploy\campaigns.json   # campaign packs only
```

It builds `bflib.dll`, `bfrange.dll` (when it is a workspace member at that
commit), `bfdb.exe` (dashboard + site embedded) and `bftools.exe`, then writes
`manifest.json` (a sha256 for every file), signs it into `manifest.json.sig`
and publishes the tag `engine-<date>-<sha>`.

### Signing releases

Servers stage **nothing** that isn't signed. `manifest.json` lists every
file's sha256, so its minisign signature covers the whole release; the bot
checks it against `autoupdate.public_key` before it downloads anything else.
The key is a `tauri signer` (minisign) key, the same kind Fowl Engine Manager
uses, but a **separate** one -- a leaked manager key must not be able to ship
an engine, and the other way round.

One-time, on the PC you publish from (user action):

```powershell
cd bfmanager; npm ci --ignore-scripts
npx tauri signer generate -w "$env:USERPROFILE\.tauri\fowl-engine.key"   # give it a password
```

- Keep `fowl-engine.key` (and a backup of it) private. Put its password in
  `$env:FOWL_ENGINE_SIGNING_PASSWORD` before running `publish-release.ps1`
  (it is handed to the signer in the environment, never on a command line).
- Paste the contents of `fowl-engine.key.pub` (or just its `RW...` line) into
  every server's `fowlengine.yaml` as `autoupdate.public_key`, and restart the
  bot. The OPS page shows the pinned key's id under the update settings.
- `publish-release.ps1 -SigningKey <path>` for a key somewhere else. Without a
  key it refuses to publish (a `-DryRun` just warns).
- For the **Engine release** GitHub Action, the key and password become
  secrets on a protected environment (see the notes on `release.yml` in the
  ops security review).

Rotating the key: generate a new one, publish with it, and update
`public_key` on every server at the same time -- a server only trusts the key
it has pinned.
The GitHub Action **Engine release** runs the same script on a Windows runner.
It is manual-dispatch only, and **beta** is the safer channel for it: `Cargo.lock`
is gitignored, so CI resolves dependencies fresh.

### Campaign packs

A release can also carry each server's **campaign**: its engine config
(`<sortie>_CFG`, e.g. `ODFv2_CFG`, `RGW2008_CFG`) and mission file(s). Keep a
`campaigns.json` on the publishing PC (start from `deploy/campaigns.sample.json`):

```json
{
  "vs1": { "cfg": "C:\\...\\ODFv2_CFG",   "cfg_name": "ODFv2_CFG",   "miz": ["C:\\...\\vs-odf-1.0.0.miz"] },
  "vs2": { "cfg": "C:\\...\\RGW2008_CFG", "cfg_name": "RGW2008_CFG", "miz": ["C:\\...\\rgw2008_1.0.0.miz"] }
}
```

- **The key** is how a server recognises its pack: the server's `id` in
  `bfdb.instances` (fowlengine.yaml) -- short, stable, and already the id bfdb
  tags rounds with. On a box without `bfdb.instances`, use the DCSServerBot
  instance name instead (`DCS.vectorstrike_1`, the Saved Games folder name).
  Case doesn't matter. The OPS page's Campaign packs card lists the keys each
  server answers to.
- `cfg_name` is the file name on the server (default: the cfg file's own
  name). It must end in `_CFG`: bflib loads `<write dir>\<sortie>_CFG`.
- `miz`: each file replaces the file **of the same name** on that server: the
  mission-list entry of that name, else a BFWeather template of that name
  (`base`/`weapon`/`options`/`warehouse`), else `<Missions>\<name>`. DCSServerBot's
  own `.dcssb\<name>.orig` / `.dcssb\<name>` copies are replaced too, so the
  next start can't bring the old mission back. A new name (a version bump) is
  added to the Missions folder; switching the server to it is still yours to
  do (`/mission`).
- Every cfg is parsed as JSON before packing (UTF-8, no BOM); one that doesn't
  parse fails the publish.

Each entry becomes `campaign-<key>.zip` -- the cfg and the missions, flat, no
folders -- and the manifest entry lists every file in it with its sha256
(`key`, `cfg_name`, `contents: {name: {sha256, size, role: cfg|miz}}`), so it is
covered by the manifest signature like everything else. `-DryRun`,
`-SignOnly`, `-PublishOnly` and `-Folder` work as for any release.

## 3. What the server does with it

1. **Check** every `check_minutes`. Drafts, other tags, pre-releases (unless
   on the `beta` channel), and anything marked bad are all skipped. A server
   never "updates" to an older release.
2. **Fetch + verify** into `<staging>\_downloads\<tag>\`. No `public_key`, no
   `manifest.json.sig` or a signature that doesn't verify, and nothing is
   fetched; a sha256 mismatch aborts the whole release. A server on an agent
   node downloads its DLL itself, and the bot reads it back and checks its
   sha256 against the signed manifest before it becomes the `.pending` file.
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
     "Unexpectedly" = straight from running to down; a scheduled restart or an
     admin's shutdown passes through *Shutting down* (the bot samples every
     second) and never counts as a crash. A forced shutdown that skips it does.
     (bfrange writes no sidecar yet, so a range DLL is judged on "stayed up".)
   - **bfdb:** passes after `probation_minutes` of answering `/api/health`.
     **Rolled back** if it exits, or never answers within `bfdb_unhealthy_minutes`.
6. **Rollback:**
   - **DLL:** the pre-swap backup is staged back and DCS is restarted onto it.
     The build it replaces is kept as `<dll>.failed-<ts>`, never as a
     `.backup-*`, so a second rollback can't bring the bad build back.
   - **bfdb:** the previous exe *and* the DB snapshot taken just before the
     swap are restored; the failed ones are kept as `bfdb.exe.failed-*` /
     `bfdb.failed-*`. Nothing written since the swap is lost: the JSONL cursor
     lives in the DB, so the restored DB re-reads the stats log, and the
     duplicate guards stop double counting.
   - The release is **marked bad**, pulled from every staging dir it was still
     waiting in, and never offered again. Clear the mark with **Allow again**
     on the OPS page or `/feops update_unmark`.

Everything is announced in `ops_channel`. Discord: `/feops update_status`,
`update_check`, `update_pause`, `update_rollback`, `update_unmark`,
`campaign_apply`, `campaign_keep`, `issues`.

### Campaign packs on the server (`autoupdate.campaigns: true`)

Off by default. With it on, a pack whose key matches one of **this box's**
servers (see "Campaign packs" above; agent-node servers never get one) goes:

```
 staged ──(that DCS server's next start)──> probation ──(mission up, probation_minutes)──> applied
   │  ▲                                          └──(crash / engine refused it / never came up)──> failed
   ▼  │ Apply                                                               (backup restored, tag never retried)
  held ──Keep server's──> dismissed  (or staged for the unedited missions only)
```

- **Staged:** downloaded with the release (sha256 checked against the signed
  manifest), unpacked into `<staging>\_campaigns\<key>\<tag>\`, the cfg parsed
  as JSON. A pack that doesn't verify or parse is refused and not retried.
- **Applied at the next DCS start**, from BFBinaries' `prepare()` -- the same
  moment and extension that swap a staged DLL, so the instance needs BFBinaries
  in nodes.yaml. The `apply:` policy counts for packs too (`when_idle` restarts
  an empty server for a staged pack, `next_restart` waits). First the files it
  replaces are copied to `<instance home>\_fowl_campaign_backups\<ts>-<tag>\`
  (the newest `campaign_backups_keep` are kept), then each file is written to a
  temp name and renamed into place.
- **Held -- "server copy was edited since the last update":** the bot remembers
  the sha256 of every cfg/mission it wrote (and, the first time, of what it
  found: every `*_CFG` in the server's write dir is noted as soon as the
  feature is on). If the file on the server no longer matches -- the dashboard
  CONFIG page, a text editor, a mission uploaded through Discord -- the pack is
  **not written**; it waits, and ops_channel says so. Answer it on the OPS page
  (**Campaign packs**: **Apply** / **Keep server's**), or in Discord:
  - `/feops campaign_apply server:<name> confirm:True` (or `POST campaign/apply {server}`):
    write the pack over the server's copy at its next start; the edited copy is in the backup.
  - `/feops campaign_keep server:<name>` (or `POST campaign/keep {server}`): keep the
    server's cfg (and any mission edited there). The pack's other missions still
    go in at the next start; the pack is dismissed for that server. The next
    release asks again while the server's cfg still differs from ours.
  A pack whose cfg has a different `netidx_base` than the running one is held
  too -- that is almost always another server's pack under the wrong key.
- **Probation:** passes once bflib logs that the mission is up ("starting timed
  events" in `Logs\bfnext.txt`) and the server has run `probation_minutes`.
  **Rolled back** if DCS goes down unexpectedly, bflib logs `THE MISSION CANNOT
  START` (a cfg it refuses), the mission isn't up after `load_timeout_minutes`
  of running, or DCS sits in LOADING that long. The backup is restored at the
  next start (the server is restarted for it), the tag goes on the server's
  "never again" list, and ops_channel says why. If the DLL of the same release
  fails on that server at the same time, one restart restores both.
- A release rolled back for its DLL anywhere cancels its still-waiting packs too.

### Server checklist

What to set on the server box for the whole hands-off setup. In
`<DCSServerBot>\config\plugins\fowlengine.yaml`, `DEFAULT:`:

```yaml
  autoupdate:
    enabled: true
    # the contents of %USERPROFILE%\.tauri\fowl-engine.key.pub on the publishing
    # PC (tauri writes it as one base64 line), or just the decoded "RW..." line
    public_key: "dW50cnVzdGVkIGNvbW1lbnQ6IG1pbmlzaWduIHB1YmxpYyBrZXk6..."
    source: github                # or folder + folder: "\\\\PC\\fowl-releases"
    repo: "Dillen198/bfnext"
    bot_plugin: true              # releases built with -BotPlugin update the plugin itself
    campaigns: true               # releases built with -Campaigns update cfg + missions
  bfdb:
    instances:                    # the `id`s are the campaign keys in campaigns.json
      - id: vs1
        dcs_server_name: "[VS] Vector Strike - ..."
        # ...
      - id: vs2
        dcs_server_name: "[VS] Vector Strike #2 ..."
        # ...
```

1. Restart the bot (the OPS page's **Restart bot**, or the service). `bot_plugin`,
   `campaigns`, `public_key` and the source are read from the YAML only -- the
   OPS page can't change them.
2. Every campaign instance has **BFBinaries** in nodes.yaml (it already does if
   engine DLLs auto-update).
3. OPS page → **Campaign packs** shows `on`, and under each server the keys it
   answers to (`vs1 / dcs.vectorstrike_1`). They must match the keys in the
   publisher's `campaigns.json`.
4. **Check now** after publishing. A pack for a server whose cfg was edited on
   the box since the feature was switched on shows up **held** -- decide there.

Fowl Engine Manager: keep **Settings → keep the plugin synced** on. The
plugin carries a build stamp (`plugins\fowlengine\.fowl-plugin.json`: commit,
commit time, and `engine-release` or `manager-bundle`); the Manager's pre-start
sync skips a bundle older than the installed plugin (Overview says so), and a
`bot_plugin` release is not unpacked over a newer plugin either. So the plugin
only ever moves forward, whichever of the two brought it.

## 4. The OPS page (dashboard → OPS)

- **Summary:** box uptime, the auto-start service, bfdb health, auto-update state, open issues.
- **Server box & auto-start:** service state, start type, account; restart after BSOD;
  pending Windows Update reboot; disk; and recent BSOD / power-loss events from the event log.
  Each problem comes with the fix.
- **Processes:** bfdb (pid, uptime, relaunches, last exit code, netidx), every DCS server;
  **Restart bfdb**, **Restart bot** (DCS keeps running).
- **Engine builds:** per binary, the *running* build vs the file on disk vs what's staged vs
  the latest release; **Apply now**, **Discard**, **Roll back**.
- **Campaign packs:** per server, the installed pack, what's staged / held (and
  why) / on probation, the last result; **Apply** and **Keep server's** for a held
  pack. Fowl Engine Manager shows the same card under **Server OPS**.
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
- **The plugin itself is broken after a `bot_plugin` update** (`autoupdate.bot_plugin`
  is set in `fowlengine.yaml` only; the OPS page can't switch it on): the previous plugin is in
  `<DCSServerBot>\_fowl_backups\plugin-<ts>.zip`. Unzip it over the bot folder and restart the service.
- **Manual bfdb rollback:** stop the service, copy `bfdb.exe.backup-<ts>` over
  `bfdb.exe` and `_backups\db-<ts>-<tag>` over `bfdb`, then start the service.
- **Manual DLL rollback:** `/feops update_rollback which:dll server:<name> confirm:True`,
  or copy `Scripts\bflib.dll.backup-<ts>` over `bflib.dll` while DCS is down.
- **Manual campaign rollback:** with DCS down, copy the files back from
  `<instance home>\_fowl_campaign_backups\<ts>-<tag>\` -- `backup.json` there lists
  where each one came from (an entry with no backup file was added by the pack:
  delete it).
- **A campaign pack stays held and nobody edited anything:** the engine itself
  rewrites its cfg when it migrates deprecated keys, which counts as an edit.
  Check the file, then **Apply** or **Keep server's**.
