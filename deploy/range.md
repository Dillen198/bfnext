# Training range server

The training range is a separate DCS server running `bfrange.dll` (the range
engine) instead of `bflib.dll` (the campaign engine). To bfdb and
DCSServerBot it is one more bfdb instance, of `kind: range`.

It can run on the same PC as the campaign servers (everything below as
written) or on **another PC on the same network**: then follow
[Range on a second PC](#range-on-a-second-pc) first, which changes only the
networking and where a few files live.

## Setup, start to finish

Every step, in order. The sections further down have the detail.

| # | Do | Where the detail is |
|---|---|---|
| 1 | **Build the three binaries/apps** on the dev box: `. .\setup-build.ps1`, then `cargo build --release --package=bfrange` (`target\release\bfrange.dll`) and `cargo build --release --package=bfdb` (the new bfdb with the range module); `cd bfrange-web; npm install; npm run build` for the site. | `bfrange/README.md`, `bfrange-web/README.md` |
| 2 | **Build the mission.** `python bfrange/mission/build_range_miz.py` writes `VS_Range_Caucasus.miz` and its config `VSRANGE_RANGE` to `E:\Saved Games\DCS\Missions\Vector\range\` (already done once; re-run after editing `bfrange/mission/layout.py`). Sortie is `VSRANGE`. The mission loads bfrange through its init script, so there is no trigger to add. | "The range mission" below |
| 3 | **Check it in the Mission Editor.** Open the `.miz`: the F10 map shows the 27 coloured sectors. Look at the target zones inside the ground sectors (`R1-*`, `R2-*`, `R11-*`...) and the helicopter zones (`H1-*`, `H2-*`, `H11-*`, `H12-*`): they should sit on open, flat ground, and each `H*-PINNACLE` on a hill top. To move one, edit `bfrange/mission/layout.py` and re-run the builder (zones, config and drawings move together); a zone dragged in the ME is overwritten by the next build. The engine moves any single target that lands in water to the bank. | "The range mission" |
| 4 | **Create the server's Saved Games folder** `DCS.range` (copy a campaign instance's `Config\`), set `serverSettings.lua` name/port/password and put `VS_Range_Caucasus.miz` in its mission list. | DCSServerBot step 1 |
| 5 | **Copy the engine files** into `DCS.range`: `Scripts\bfrange.dll`, `Scripts\Hooks\bfrange_hooks.lua` (and **not** bflib's hooks), `Scripts\Export.lua` + `Scripts\bf_export_port.lua` (`return 42004`), and the config as `DCS.range\VSRANGE_RANGE`. | DCSServerBot steps 2, 3, 5 |
| 6 | **Tacview** on the instance, export path `DCS.range\Tacview`. | DCSServerBot step 6 |
| 7 | **Bot YAML**: paste the four blocks from `DCSServerBot/config/SAMPLE-range-server.md` (nodes, servers, scheduler, fowlengine), with your Discord channel ids for `range_status_channel`, `range_results_channel`, `greenie_channel`. No BFWeather on this instance. | DCSServerBot step 7 |
| 8 | **CORS**: add `https://range.vectorstrike.org` to `bfdb.cors_origins`. | DCSServerBot step 8 |
| 9 | **Deploy bfdb**: drop the new `bfdb.exe` in the admin channel (staged, swapped on the next bfdb restart), then `/feops bfdb_restart`. Check the log for `[range] range: reading ...range.jsonl from offset N`. | bfdb section |
| 10 | **Deploy the site**: a new Vercel project from `bfrange-web/` (build `npm run build`, output `dist`, env `VITE_API_BASE=https://api.vectorstrike.org`), domain `range.vectorstrike.org`, DNS `CNAME range → cname.vercel-dns.com`. | DCSServerBot step 9, `bfrange-web/README.md` |
| 11 | **Start the server** from the bot and run the checks: `dcs.log` shows `loading bfrange.dll`; `Logs\bfrange.txt` shows `range engine running` and one line per station/tanker/carrier; F10 has **Range**; drop one bomb and see it in `#range-results` and on the site within ~15 s. | DCSServerBot step 10, "First flight" |

### The range mission

`bfrange/mission/build_range_miz.py` derives the mission from the RGW2008
Caucasus campaign file (for its theatre, weather, options, dictionary and
airfield warehouses), strips every campaign group, zone, trigger and drawing,
and adds:

* **CVN-72** (Supercarrier, LSO on) at 41.70 N 41.00 E and **LHA-1** (Tarawa)
  beside it, both immortal and weapons-hold. The engine steers them into the
  wind and lights TACAN 72X / 71X, ICLS 11 / 12, ACLS and Link-4 336.0.
* **Deck slots** (dynamic spawn can't use a Supercarrier deck): F/A-18C x4,
  F-14B x2, F-14A x2 on CVN-72; AV-8B x2 and UH-1H x2 on LHA-1.
* **Dynamic spawn + hot start + dynamic cargo, unlimited aircraft, weapons and
  fuel** at the home fields: blue Batumi, Kobuleti, Senaki, Kutaisi,
  Tbilisi-Lochini, Soganlug, Vaziani; red Mozdok, Beslan, Nalchik, Mineralnye
  Vody, Maykop, Krymsk. Every other field is neutral. Any airframe, any
  weapon, hot or cold.
* **Combined Arms**: 2 tactical commanders, 2 JTAC/operators and 2 observers
  per side, 1 Game Master.
* **27 sectors over the whole map**, one job each, drawn on the F10 map
  (`bfrange/mission/layout.py`; the table is in `bfrange/README.md`). Blue's
  ground ranges are east and south of Tbilisi (R-1 to R-3, G-1), red's on the
  steppe north of Mozdok (R-11 to R-13, G-11) and on the Kuban for Maykop and
  Krymsk (R-14); helicopter areas at Vaziani, Kutaisi, Mozdok and Nalchik;
  fight areas, BVR, duels, anti-ship, the carrier and most tanker tracks over
  the Black Sea. Each side's sectors are on its own drawing layer.
* **55 trigger zones** inside them, one per thing the config places, named
  `<SECTOR>-<WHAT>` (`R1-BOMB`, `R12-CONVOY-END`, `H2-PINNACLE`).
* a clear June morning, sortie `VSRANGE`, and a training briefing.

Everything else -- targets, strafe pits, tankers (incl. the A-6E recovery
tanker), ships, the SA-8, JTAC drones -- the engine spawns from
`VSRANGE_RANGE` at mission start, so moving a range later is a config or zone
edit, not a rebuild.

Air-start slots next to the tanker tracks or BFM boxes are not in the
generated mission (scripts can't air-start players); add plain air-start
client groups in the ME if you want them.

### First flight

A 20-minute smoke test that exercises every part of the chain:

1. Slot in at Vaziani (dynamic spawn), open **F10 > Range > Range status**:
   stations, tankers and both carriers listed with bearing/range. Then
   **F10 > Range > Sectors: what is where**: blue's sectors and the shared
   ones, nearest first; the F10 map shows the same coloured areas.
2. Fly east from Vaziani into **R-1 SAMGORI**: an `Entering R-1 SAMGORI
   (AIR-TO-GROUND)` message says what it is for. Drop one bomb on the bomb
   circle: an in-cockpit `RANGE R-1 Samgori - Bomb Circle: ... o'clock`
   message; the card appears in
   `#range-results` and on range.vectorstrike.org.
3. **Air-to-Air > Set-up selection**: MiG-29S, Fox 2, BFM offensive, then
   **FIGHT'S ON**; take a missile shot at you and check the missile trainer
   says `SPLASH` or `DEFEATED`.
4. Join **Texaco 2** (sector AR-4, KC-135, TACAN 54Y, 254.0, over Kartli west
   of Tbilisi) and plug:
   `contact` / `disconnect` calls and an AAR grade after you leave.
5. From the CVN-72 deck: one pass. The trap card shows DCS's LSO grade (if it
   graded) or ours, the decoded comment and the trap sheet.
6. In a helicopter at Vaziani: ask the ground crew (F8) for cargo, load it
   with the cargo loader, fly it to the **H-1 Vaziani - Precision Pad** or a
   course DZ/LZ and
   unload it: a `CARGO ... delivered to ...` message and a "Cargo delivery
   (internal)" card. (**F10 > Range > Helicopter > Dynamic cargo** lists
   every destination.)
7. On the site: **Spawn** a tanker at your position, then despawn it.

---

## Range on a second PC

The range DCS server runs on its own PC; bfdb, the bot's Discord side, the
database and the campaign servers stay on the main PC. Two scripts in
`deploy/range-pc/` do the networking; the campaign servers are not touched
(they keep their own resolver on the main PC, nothing of theirs restarts).

```
 RANGE PC  (e.g. 192.168.1.60)                 MAIN PC  (e.g. 192.168.1.10)
 ┌───────────────────────────────┐             ┌────────────────────────────────┐
 │ DCS.range + bfrange.dll       │── RPCs ────►│ bfdb  (range instance talks to │
 │ netidx resolver :4564 (range) │◄─ queries ──│        the range PC's resolver)│
 │ Logs\ Tacview\  (read shares) │◄─ reads ────│                                │
 │ Export.lua  ──────────────────┼─ UDP 42004 ►│                                │
 │ DCSServerBot agent node ──────┼─ TCP 5432 ─►│ PostgreSQL + DCSServerBot      │
 │ SRS :5005                     │             │ campaign DCS servers (as now)  │
 └───────────────────────────────┘             └────────────────────────────────┘
        ▲ players: DCS 10328, SRS 5005
```

Why the range gets its own resolver: netidx will not mix a loopback
resolver with network publishers, so moving the campaign's shared resolver
onto the network would mean re-pointing every campaign process. A second
resolver on the range PC leaves the campaign exactly as it is; bfdb simply
reaches the range instance through it (`netidx_resolver` below).

### Checklist

| # | Where | Do |
|---|---|---|
| 1 | router | Give both PCs **fixed LAN addresses** (DHCP reservations). Everything below names them. |
| 2 | range PC | Install DCS World Server, SRS and Tacview. Copy `netidx.exe` from the main PC (`(Get-Command netidx).Source` there) to `C:\fowl-range\netidx.exe`. |
| 3 | range PC | Create `DCS.range` exactly as in "DCSServerBot" steps 1-6 below (Saved Games folder, hooks, `Export.lua`, the `_RANGE` config, Tacview), on this PC. |
| 4 | range PC | Elevated PowerShell: `.\setup-range-pc.ps1 -MainPcIp 192.168.1.10`. It asks for a password for the new `fowlread` account. It starts the range's resolver (a scheduled task, back after every reboot), opens the firewall (resolver/RPCs/shares for the main PC only, DCS and SRS for everyone), shares `Logs` and `Tacview` read-only, points `Export.lua` at the main PC and sets `netidx_config` in `VSRANGE_RANGE`. Re-run it any time; it replaces what it made. |
| 5 | main PC | Elevated PowerShell, logged in as the bot's account (ATPAdmin): `.\setup-main-pc.ps1 -RangePcIp 192.168.1.60`. It checks the range PC answers, opens UDP 42004 and PostgreSQL to the range PC only, saves the `fowlread` login for bfdb (type the same password) and prints the YAML to paste. Add `-ConfigurePostgres` to let it make the two PostgreSQL edits; it never restarts PostgreSQL itself. |
| 6 | main PC | If step 5 changed PostgreSQL: `Restart-Service postgresql-x64-*` at a quiet moment (the bot reconnects; DCS keeps running). |
| 7 | range PC | Install DCSServerBot as an **agent node** of the same cluster: run its installer, point it at the main PC's database (`postgres://...@192.168.1.10:5432/...`), and put `DCS.range` (with `BFBinaries` and `Tacview`, see `SAMPLE-range-server.md` section 1) in **this PC's** section of its `nodes.yaml`, plus `cluster: no_master: true` so it never takes over the Discord side. `BFBinaries` needs an explicit `staging_dir` here. |
| 8 | main PC | `fowlengine.yaml`, the `range` instance: paste the lines from step 5 (`netidx_resolver`, the three `\\192.168.1.60\...` paths, `export_port`). Leave out `srs_url` (or point SRS's client export at `DCS.range\Logs` and use the share). Then `/feops bfdb_restart`. |
| 9 | router | Players reach the range PC directly: forward DCS **10328** (TCP+UDP) and SRS **5005** (TCP+UDP) to it, if the PCs sit behind a home/office router. The campaign's forwards stay on the main PC. |
| 10 | Discord | Start the range server from the bot and run the usual checks (DCSServerBot step 10). bfdb's log shows `instance "range": netidx via ...netidx.range.json`; the bot log has no `not reachable from this PC` line. |

What the bot does across the two PCs:

* Engine updates work as before: drop `bfrange.dll` in the admin channel. The
  range PC downloads it into its own `staging_dir`, and its BFBinaries swaps
  it in on the next DCS start. `/feops stage_status`, `stage_cancel` and
  `stage_apply` ask the range PC over the bot's node link.
* bfdb reads the range's `stats.jsonl`, `range.jsonl` and Tacview files over
  the read-only shares. Both are polled, so a share that drops (range PC
  rebooting) just pauses the feed; it picks up where it left off.
* Nothing on the main PC restarts when the range PC goes down. The range site
  shows "No live data" until it is back.

### If something is off

| Symptom | Look at |
|---|---|
| Range site "No live data", results still arrive | bfdb log for the range's netidx line; on the range PC `C:\fowl-range\resolver.log` and the task **FowlRange netidx resolver**; `Logs\bfrange.txt` should say `netidx: publishing on 192.168.1.60:<port>`. A `loopback address and the resolver is not` error means `netidx_config` is missing from `VSRANGE_RANGE`. |
| Nothing arrives at all | procman's `not reachable from this PC` error: the share login. `cmdkey /list` as the bot's account; `Test-Path \\192.168.1.60\FowlRangeLogs`. |
| Live positions missing on the range map | `Scripts\bf_export_host.lua` on the range PC must hold the main PC's address, and `Export.lua` must be the current one from `scripts/` (older copies ignore that file). |
| RPCs time out but the resolver answers | A **Block** rule for DCS in the range PC's firewall (a dismissed "allow access" prompt leaves one); `setup-range-pc.ps1` warns about it. Delete it in `wf.msc`. |
| The range server is missing in Discord | The agent node's database connection: PostgreSQL `listen_addresses` / `pg_hba.conf` (step 5-6) and TCP 5432. |

---

## DCSServerBot

Step by step, for a range server next to the existing campaign servers on the
same node. (On a second PC: these steps happen on that PC, after
[Range on a second PC](#range-on-a-second-pc).) Names and ports used throughout (change them together):

| | |
|---|---|
| DCSServerBot instance | `DCS.range` |
| Saved Games folder | `C:\Users\ATPAdmin\Saved Games\DCS.range` |
| Server name (DCS `name`, `servers.yaml`, `dcs_server_name`) | `[VS] Vector Strike \| Training Range` |
| DCS / bot / WebGUI ports | 10328 / 6668 / 8092 |
| SRS port | 5005 |
| Export port (`BF_PORT`, bfdb `export_port`) | 42004 |
| bfdb instance id / `netidx_base` | `range` / `/local/fowl/range` |

The paste-ready YAML for all four bot config files is in
`DCSServerBot/config/SAMPLE-range-server.md`.

### 1. The Saved Games folder

Create `C:\Users\ATPAdmin\Saved Games\DCS.range`. The quickest start is a copy
of a campaign instance's `Config\` folder (serverSettings.lua, SRS.cfg,
options.lua), then:

* `Config\serverSettings.lua`: `name` = the server name above (exactly), `port`
  = 10328, its own password and mission list (the range mission).
* `Config\SRS.cfg`: `SERVER_PORT=5005`.
* Do **not** copy the campaign's `Scripts\` folder or its `<sortie>_CFG` file.

When it is done it looks like this:

```
DCS.range\
  Config\serverSettings.lua        name, port 10328, missionList
  Config\SRS.cfg                   SERVER_PORT=5005
  Missions\<range mission>.miz     with the bfrange mission-start trigger (step 4)
  Scripts\bfrange.dll              the engine
  Scripts\Hooks\bfrange_hooks.lua  the engine's hook loader (step 2)
  Scripts\Export.lua               live positions to bfdb (step 3)
  Scripts\bf_export_port.lua       return 42004
  <sortie>_RANGE                   the range config (step 5)
  Logs\stats.jsonl, Logs\range.jsonl   written by bfrange
  Tacview\                         recordings (step 6)
  _staging\                        staged bfrange.dll updates (BFBinaries)
```

### 2. `Scripts\Hooks`

Exactly these, and nothing campaign-related:

* `bfrange_hooks.lua` from `bfrange/lua/` in this repo. It loads
  `Scripts\bfrange.dll` into the hooks state, and first swaps in
  `Scripts\_bfrange.dll` if one is waiting there (the manual update path).
* DCSServerBot's own `DCSServerBotGameGui.lua` (plus `Scripts\net\DCSServerBot\`).
  The bot installs and refreshes these itself on every server start; nothing
  to copy.
* Whatever the SRS and Tacview extensions install for themselves.

**Not** bflib's hook loader (`bflib/lua/hooks.lua`, under whatever name the
campaign servers have it in their `Scripts\Hooks`). It gates every slot change
through the campaign database; on a range mission it knows nothing about, it
refuses every slot and nobody can fly. If the folder was copied from a
campaign instance, delete it.

### 3. `Scripts\Export.lua` and the export port

Copy `scripts/Export.lua` from this repo to `DCS.range\Scripts\Export.lua`
(merge it if an `Export.lua` is already there). Then create
`DCS.range\Scripts\bf_export_port.lua` containing one line:

```lua
return 42004
```

It must equal the instance's `export_port` in `fowlengine.yaml`. It is read
in preference to `BF_PORT`, so the same `Export.lua` works on every instance.

### 4. MissionScripting.lua and the mission trigger

The range mission loads the engine from a trigger script, which needs
`require`, `package` and `lfs` -- the same desanitisation bflib needs.
`MissionScripting.lua` belongs to the DCS **installation**
(`C:\Program Files\Eagle Dynamics\DCS World Server\Scripts\MissionScripting.lua`),
so it is shared by every instance on the node, and this node already has
`DCS: desanitize: true` in `nodes.yaml`: DCSServerBot comments out
`sanitizeModule('io')`, `sanitizeModule('lfs')`, `_G['require'] = nil` and
`_G['package'] = nil` on every start, and re-does it after a DCS update. Check
those four lines are commented out if the range misbehaves after an update.

The generated `VS_Range_Caucasus.miz` already loads the engine through its
mission init script (`l10n/DEFAULT/bfrange_mizinit.lua`); nothing to add. For a
mission you build yourself, add a trigger: **MISSION START**, no condition,
action **DO SCRIPT** with the three lines from `bfrange/lua/bfrange_mizinit.lua`:

```lua
package.cpath = package.cpath .. ";" .. lfs.writedir() .. "\\Scripts\\?.dll"
local bfrange = require("bfrange")
bfrange.initMiz()
```

### 5. The `_RANGE` config file

`DCS.range\<sortie>_RANGE` (JSON, no extension), where `<sortie>` is the range
mission's sortie name -- the same convention as the campaign's
`<sortie>_CFG`. For the generated mission that is `DCS.range\VSRANGE_RANGE`,
written by the mission builder next to the `.miz` (the same file is
`bfrange/RANGE_CFG.sample.json` in the repo). The one key the bot integration
depends on:

```json
{
  "netidx_base": "/local/fowl/range",
  "name": "Vector Strike Range"
}
```

`netidx_base` must equal the bfdb instance's `netidx_base`. Without it the
engine still writes `Logs\range.jsonl` (so results reach bfdb and the Discord
feed), but publishes no RPCs: `/range status`, the range status embed and the
site's live map stay on "No live data". Everything else in the file (stations,
tankers, carriers, adversaries) is the range layout, see
`bfprotocols/src/range/cfg.rs`.

### 6. Tacview

On the node, the Tacview extension needs `installation:` pointing at Tacview
(node-level `extensions:`). On the instance:

* `tacviewExportPath: C:\Users\ATPAdmin\Saved Games\DCS.range\Tacview` -- and
  the bfdb instance's `tacview_dir` must be the **same** folder, or the range
  site's "download Tacview" links find nothing.
* Real-time/remote-control ports unique on the box (42684 / 42685 in the
  sample; Tacview's defaults are 42674 / 42675).
* `tacviewPlaybackDelay: 600` (the extension warns about performance
  otherwise).
* `target:` optional: a Discord channel (`'<id:...>'`) or a directory each
  finished recording is also sent to.
* Recordings pile up: prune them with the Cleanup service
  (`config/services/cleanup.yaml`), not by hand.

### 7. The bot's YAML

Paste the blocks from `DCSServerBot/config/SAMPLE-range-server.md`:

* **`nodes.yaml`**: the `DCS.range` instance with unique `dcs_port` /
  `bot_port` / `webgui_port`, SRS on 5005, `BFBinaries` with
  `dll_name: bfrange.dll` + its own `staging_dir`, and `Tacview`.
  **No `BFWeather`**: it rebuilds the mission from the *campaign* base
  template before every load, which would replace the range mission with the
  campaign. `BFViewLock` isn't needed either (nothing to scout on a range).
* **`servers.yaml`**: the server's own `status` / `chat` channels.
* **`plugins/scheduler.yaml`**: restart every 8 h of mission time
  (`mission_time: 480`), only when empty (`populated: false`), with
  `shutdown: true` -- a staged `bfrange.dll` is only swapped in when DCS
  itself starts, never on a mission-only restart.
* **`plugins/fowlengine.yaml`**:
  * the bfdb instance under `bfdb.instances`: `id: range`, `kind: range`,
    `dcs_server_name`, `netidx_base: /local/fowl/range`, `export_port: 42004`,
    `stats_jsonl` / `range_jsonl` / `tacview_dir` under the range's Saved
    Games, `public: true`, `gci: {enabled: false}`. (A range instance never
    inherits `gci.enabled` from the shared block anyway; if `range_jsonl` is
    left out procman uses `Logs\range.jsonl` next to `stats_jsonl` and logs
    it.)
  * a per-server section keyed by the server name, with every campaign
    channel `null` and `range_status_channel`, `range_results_channel`,
    optionally `greenie_channel` and `range_results_kinds`.

What the plugin does with a `kind: range` server:

* skips everything campaign-only: Campaign Status, objective/capture alerts,
  kill-streak achievements, coalition briefings, coalition-role sync, rank
  sync, the GCI transcript; `/fe_objective`, `/fe_terminal` and
  `/fe_briefing` answer that it is a range server;
* posts one **range status** embed to `range_status_channel`, edited every
  2 min (tankers on station, carriers with BRC/FB, wind over deck and
  recovery window, stations hot/cold, who is flying) from bfdb
  `/api/range/live`;
* posts every graded result to `range_results_channel` (and traps also to
  `greenie_channel`) as bfdb's embed with the debrief card attached, polling
  `/api/range/feed` every 15 s; at most 10 posts per poll, any catch-up
  beyond that collapses into one summary line. It remembers the last result
  it posted across bot restarts, and a brand-new setup starts from "now"
  rather than replaying history;
* `/range status`, `/range me` (link to the player's page on the range site,
  via their `/linkme` Discord link) and `/range greenie` (top 10, last 30
  days).

Updating the engine: drop `bfrange.dll` into the bot's admin channel. It is
staged only into the range instance's `_staging` (a `bflib.dll` only into the
campaign instances'), and BFBinaries swaps it in on the next DCS start.
`/feops stage_status` shows it; `/feops stage_apply` with the server shut down
applies it immediately. By hand: put the new DLL at
`DCS.range\Scripts\_bfrange.dll` and the hook installs it on the next load.

### 8. CORS

The range site calls bfdb from the browser, so its origin has to be allowed.
Add it to `cors_origins` under `bfdb:` in `fowlengine.yaml` (procman passes
each entry to bfdb as `--cors-origin`):

```yaml
    cors_origins:
      # ... the existing vectorstrike.org origins ...
      - "https://range.vectorstrike.org"
```

then `/feops bfdb_restart`. A missing origin shows up as CORS errors in the
browser console on the range site while `curl` against the API works fine.

### 9. DNS and the site

The range site is its own Vercel project. In the project: **Settings →
Domains → Add** `range.vectorstrike.org`. At the DNS provider for
`vectorstrike.org`, add the record Vercel shows for it -- for a subdomain a
`CNAME` `range` → `cname.vercel-dns.com`. Point the site's API base at the
same public bfdb origin the dashboard uses, and keep the CORS entry above in
step with the domain.

### 10. Checks

1. Bot log on start: the range server registers under the exact server name;
   procman renders `<bfdb.home>\instances.json` with the `range` entry having
   `"kind": "range"`, `range_jsonl`, `tacview_dir` and `"gci_config": null`.
2. `dcs.log` on the range server: `loading bfrange.dll`, then the engine's own
   start-up lines. A `module 'bfrange' not found` means the DLL is not at
   `Scripts\bfrange.dll` (or `package.cpath` does not point at `Scripts\`).
3. Fly one bomb pass: a line appears in `Logs\range.jsonl`, bfdb ingests it,
   and within ~15 s the result is posted to `range_results_channel` with its
   card.
4. The range status embed shows tankers/carriers once bfrange publishes on
   `/local/fowl/range` (`netidx resolver list /local/fowl/range`).
5. Nothing campaign-side changed: the campaign servers' status embeds, alerts
   and briefings carry on as before.

---

## bfdb

The range is one more entry in bfdb's `--instances` file. See
[`instances.range.sample.json`](../instances.range.sample.json) for a
commented copy with two campaign servers and the range.

### Instance fields

| Field | Default | Meaning |
|---|---|---|
| `kind` | `"campaign"` | `"range"` for a bfrange server. |
| `range_jsonl` | `range.jsonl` next to `stats_jsonl` | The engine's graded results, one JSON `RangeRecord` per line. |
| `tacview_dir` | unset | The folder Tacview writes this server's `.acmi` files to (the same folder as `tacviewExportPath`). Unset turns off `/api/range/tacview/<id>`. |
| `netidx_config` | unset (bfdb's default `client.json`) | A netidx client config for reaching this instance's engine, when it publishes through its own resolver on another PC. procman writes it from `netidx_resolver: <ip>:<port>` in `fowlengine.yaml` (`<bfdb.home>\netidx.<id>.json`). Instances without it share one subscriber, as before. |

Everything else (`base`, `stats_jsonl`, `export_port`, `dcs_server_name`,
`public` ...) means what it means for a campaign server. The range's
`stats.jsonl` (connect, slot, takeoff, land) is ingested as usual, so the
range's rounds and sorties exist in the DB.

What `kind: "range"` changes in bfdb:

* **Never counted toward campaign totals.** Flying the range does not touch
  the leaderboard, a pilot's lifetime profile or `/api/stats`
  (`InstanceCfg::counts_toward_totals` = `public && kind == campaign`).
* **Not in the campaign dashboard's server selector.** `GET /api/instances`
  leaves range instances out; `?kind=range` lists only them, `?all=1` every
  kind. Every row now carries `"kind"`.
* **No campaign background loops**: no TACMAP poller, war diary, intel-mark
  push or unit-database refresher for it. GCI/ATC stay config-driven (set no
  `gci_config` for the range).
* A per-instance campaign reset and `--rebuild-stats` / `rebuild_stats` leave
  range results alone: they live in their own trees and come from
  `range.jsonl`, not `stats.jsonl`.

### Flags

| Flag | Default | |
|---|---|---|
| `--range-site-url` | `https://range.vectorstrike.org` | Result links in the Discord embeds. |
| `--public-api-url` | `https://api.vectorstrike.org` | Absolute card-image URLs in the Discord embeds. |
| `--range-track-days` | `180` | How long a result's debrief track (the sampled geometry behind its card) is kept. Results themselves are kept forever; an older card is drawn without its track. |
| `--cors-origin https://range.vectorstrike.org` | | Required for the range site (see step 8 above). Login from the range site uses the dashboard's `/api/auth/login?return_to=` flow; `return_to` must be exactly the origin plus `/` (`https://range.vectorstrike.org/`), anything else is dropped. |

### Storage and ingestion

`range.jsonl` is tailed every 2 s with a persisted byte cursor. Only complete
lines are consumed (the engine may be mid-write); a file that shrank, or whose
first 512 bytes changed, is a new file and is re-read from the top. Records
are deduplicated by `id`, so a re-read never double-counts. Unparsable lines
are counted and logged once per pass. A record this bfdb build cannot decode
(a result kind from a newer engine) is still stored, raw, and shows up in the
feed with a generic headline.

Sled trees, all raw JSON (not bincode, so the record type can grow):
`range_records`, `range_by_id`, `range_by_pilot`, `range_tracks`
(zstd-compressed), `range_cursor`, `range_pilots`, `range_meta`.

### API

All under `/api/range/`, all take an optional `?instance=<id>` (or
`?server=<DCS server name>`). Absent means the first range instance; `all`
means every range instance (read-only views); a campaign instance or no range
configured at all is a 400. Public unless noted. `Summary` below is the record
as the engine wrote it, without `track`, plus `headline`, `has_track`,
`card_png`, `card_svg` (relative URLs) and `instance`.

| Route | Returns |
|---|---|
| `GET instances` | `[{id, label, dcs_server_name}]` of the range instances |
| `GET live` | `{live: RangeLive \| null, reason: string \| null}`. `query-range`, cached 2 s per instance (fifty viewers cost one RPC); the last good answer is served for 15 s if a refresh fails |
| `GET feed?limit=&before=<rfc3339>&before_id=&kind=` | `{items: [Summary]}` newest first, `limit` default 50, max 200. Page with `before_id=<last id>`: exact even when records share a timestamp (a missile shot writes one per pilot involved) |
| `GET results?pilot=<ucid>&kind=&unit_type=&station=&days=&limit=&offset=` | `{items: [Summary], total}` |
| `GET result/<id>` | the full record including `track` (plus the Summary fields) |
| `GET result/<id>/card.svg`, `card.png` | the rendered result card; PNGs cached (200) |
| `GET result/<id>/discord` | `{title, description, color, fields: [{name, value, inline}], footer, image, url, timestamp}` for the bot to post; `image` is the absolute card PNG, `url` the result's page on the range site |
| `GET me` | `{logged_in, ucid, name, discord_name, admin}` (ucid via the DCSServerBot link) |
| `GET pilot/<ucid>` | `{ucid, name, per_kind: {<kind>: {count, avg_score, best_score, last_ts}}, trend: {<kind>: [{week, avg_score, count}]}, insights: [Insight], quals: [Qual], airframes: [{unit_type, count}], recent: [Summary x20]}` |
| `GET pilots?q=` | `[{ucid, name, count}]`, name search, max 50 |
| `GET greenie?days=30&carrier=&unit_type=` | `{rows: [{ucid, name, avg_points, count, passes: [{id, ts, grade, points, wire, case, night, outcome, unit_type}]}]}`, best average first, 30 passes per row, newest first |
| `GET leaderboards?days=30` | `{bombing: [{ucid, name, count, cep_m, avg_score}], strafe: [{..., avg_accuracy, avg_score}], lso: [{..., avg_points, traps}], aar: [{..., avg_score}], duels: [{..., wins, losses, elo}], missile_defense: [{..., defeated, killed}]}`; minimum samples 5 bombs, 3 valid strafe passes, 3 graded passes, 2 AAR sessions |
| `GET stations/<station_id>/impacts?days=&pilot=` | `{target: GeoPt \| null, rings_m, impacts: [{id, north_m, east_m, miss_m, weapon, weapon_class, quality, ucid, name, ts}], cep_m}` |
| `GET catalog` | `SpawnCatalog` (`query-range-catalog`, cached 60 s) |
| `POST spawn` `{item, params}` | `SpawnReply`. Needs a logged-in, bot-linked player (401 / 403 `{ok:false, message}`); the ucid comes from the session, `instructor` from the dashboard admin role. One request per 3 s per player (429) |
| `POST despawn` `{spawn_id \| "all"}` | `{ok, message}`, same rules |
| `POST admin/reset-station` `{station}` | `{ok, message}`, dashboard admins only |
| `GET weapons` | `{db: WeaponDb \| null, calibration: [{weapon, samples, drag_scale, residual_m, cd_ref, mass_kg, caliber_m}]}`. The last weapon database is kept, so this answers while the range server is down. `calibration` fits one drag multiplier per unguided bomb with 8+ recorded drops in the last 180 days; the model (which the site's release calculator reimplements) is documented in `bfdb/src/range/ballistics.rs` |
| `GET tacview/<id>` | the `.acmi` recording that contains the result, as a download; 404 without `tacview_dir` |

`Insight` is `{id, kind, severity: "info" | "warn" | "good", title, detail,
evidence: [record ids]}`; `Qual` is `{id, name, description, earned,
progress (0..1), detail}` (`bfdb/src/range/insights.rs`, `quals.rs`).

---

© 2026 Dillen Weerasinghe. All rights reserved. Proprietary — see the repository NOTICE file.
