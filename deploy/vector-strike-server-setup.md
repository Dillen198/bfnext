# Vector Strike server — full setup (bot-managed, no RDP)

End state: one Windows service (`DCSServerBot`) that on boot brings up the bot,
DCS, the netidx resolver and `bfdb.exe`, renders `gci.json` from YAML, and
self-heals crashes. New engine builds and GCI/config changes are shipped from
Discord.

```
Windows boots
 └─ Service "DCSServerBot" (runs as ATPAdmin, auto-restart)
      ├─ DCSServerBot ── Discord
      ├─ Scheduler ───── DCS.exe (as KillerDog198)
      └─ fowlengine plugin (bfdb.manage: true)
           ├─ netidx resolver   (child)
           └─ bfdb.exe          (child; renders gci.json first)
```

Paths below match this server. Adjust if yours differ:
`E:\Github\bfnext-vector` (repo), `E:\Github\DCSServerBot` (bot),
`C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1` (DCS instance = "home").

---

## 0. Prerequisites (once)

- DCSServerBot already installed & working at `E:\Github\DCSServerBot`
  (Postgres reachable, `run.cmd` starts it, Discord bot online).
- `netidx-tools` on PATH for the `ATPAdmin` account
  (`cargo install netidx-tools`) and `%APPDATA%\netidx\client.json` set up for
  **both** `ATPAdmin` and `KillerDog198` — see `user-guide/src/server-setup`.
- DCS-SimpleRadio-Standalone installed (for GCI) — note the path to `opus.dll`.
- [NSSM](https://nssm.cc/) installed (`choco install nssm`).
- The `ATPAdmin` account has the **Log on as a service** right
  (`secpol.msc` → Local Policies → User Rights Assignment).

---

## 1. Build the binaries

From the repo, in an **elevated** PowerShell (the build needs the DCS Lua env):

```powershell
cd E:\Github\bfnext-vector
. .\setup-build.ps1
cargo build --release --package=bflib
cargo build --release --package=bfdb
cargo build --release --package=bftools    # only if you use the BFWeather extension
```

Copy the outputs into place:

```powershell
Copy-Item target\release\bfdb.exe  "C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\bfdb.exe" -Force
Copy-Item target\release\bflib.dll "C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\Scripts\bflib.dll" -Force
# opus.dll next to bfdb.exe (or set gci.opus_dll_path instead):
Copy-Item "C:\Program Files\DCS-SimpleRadio-Standalone\opus.dll" "C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\opus.dll" -Force
```

After the first run you never touch these by hand again — new builds go through
Discord (step 7).

---

## 2. Put the plugin + extension into the bot

The bot loads code from its own tree, so copy (or symlink) the two folders:

```powershell
# plugin
robocopy E:\Github\bfnext-vector\plugins\fowlengine E:\Github\DCSServerBot\plugins\fowlengine /MIR
# restart-cycle binary-swap extension
robocopy E:\Github\bfnext-vector\extensions\bfbinaries E:\Github\DCSServerBot\extensions\bfbinaries /MIR
```

(Symlinks are nicer if you rebuild often:
`New-Item -ItemType Junction -Path E:\Github\DCSServerBot\plugins\fowlengine -Target E:\Github\bfnext-vector\plugins\fowlengine`)

`fowlengine` is already in `Server Bot\config\main.yaml` → `opt_plugins`.
Extensions need no main.yaml entry — they're switched on per-instance in step 4.

Install the plugin's Python deps into the bot's venv if not already there:
`aiohttp` (already used by the bot). Nothing new is required.

---

## 3. Discord channels

Create the channels you want and copy their IDs (Developer Mode → right-click →
Copy ID). You need at minimum:

| Channel | Purpose | YAML key |
|---|---|---|
| #vs-admin | drag-drop binary uploads, `/dcs` commands, crash pings | `servers.yaml` → `channels: admin:` |
| #vs-status | live campaign embed | `status_channel` |
| #vs-alerts | captures / ready-to-capture | `alerts_channel` |
| #vs-ops | "bfdb relaunched / staged binary applied" | `ops_channel` |
| #vs-server-info | consolidated connect/status/GCI/deploy embed | `server_info_channel` |
| #vs-gci | GCI transcript relay | `gci_transcript_channel` |
| #vs-engine-log | raw bflib engine log | `engine_log_channel` |
| #vs-perf | hardware / frame-time embed | `perf_channel` |
| #vs-achievements | killstreaks | `achievements_channel` |
| #vs-welcome | join briefing | `welcome_channel` |

Add the admin channel to `Server Bot\config\servers.yaml` (the drag-drop upload
needs it):

```yaml
'[VS] Vector Strike - Vector Strike | Dynamic Campaign PVP Modern':
  channels:
    status: 1451170139859976192
    chat: 1451170192456548455
    admin: <YOUR #vs-admin CHANNEL ID>
```

---

## 4. Configure `nodes.yaml` (the BFBinaries extension)

`Server Bot\config\nodes.yaml`, under the instance's `extensions:` — **already
added** by this change, just confirm the paths:

```yaml
      extensions:
        SRS: { ... }
        BFBinaries:
          bflib_dll_path: 'C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\Scripts\bflib.dll'
          staging_dir:    'C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\_staging'
          keep_backups: 5
```

`bflib_dll_path` must be the path your mission's `require` actually resolves —
if in doubt, check the DCS log for where it loads `bflib` from.

---

## 5. Configure `fowlengine.yaml`

`Server Bot\config\plugins\fowlengine.yaml`. The `bfdb:` and `gci:` blocks are
**already added** with this server's values — you only need to fill the blanks:

```yaml
DEFAULT:
  # ... existing channels ...
  ops_channel: <#vs-ops ID>
  server_info_channel: <#vs-server-info ID>     # uncomment + set
  gci_transcript_channel: <#vs-gci ID>          # uncomment + set

  bfdb:
    manage: true
    # paths are pre-filled for this server; verify they exist
    discord_client_id: "<Discord app client id>"        # for dashboard OAuth
    discord_client_secret: "<Discord app client secret>"
    discord_guild_id: "1450815563524866144"
    discord_admin_role_id: "<role id that grants dashboard admin>"
    dcsserverbot_api_key: "<X-API-Key from restapi.yaml>"
    # admin_password already set (matches the top-level one)

  gci:
    enabled: true
    opus_dll_path: "C:\\Program Files\\DCS-SimpleRadio-Standalone\\opus.dll"
    blue_eam_password: "<SRS EXTERNAL_AWACS_MODE_BLUE_PASSWORD>"   # if your SRS needs EAM
    red_eam_password:  "<SRS EXTERNAL_AWACS_MODE_RED_PASSWORD>"
    # for speech recognition, also set:
    # whisper_exe: "C:\\tools\\whisper\\whisper-cli.exe"
    # whisper_model: "C:\\tools\\whisper\\ggml-base.en.bin"
```

`bfdb.manage: true` means the bot now owns bfdb + the resolver. **Stop using
`bfsystem.ps1`** — running both gives you two bfdb processes on the same port.

GCI briefing shown to players on slot entry is still set in the **mission**
config (`ODFv2_CFG` / bfweb Config Editor → SAM Sites & Air Defense →
`gci_briefing`), not here.

---

## 6. First run (interactive, to verify before installing the service)

```powershell
cd E:\Github\DCSServerBot
.\run.cmd
```

Watch the log for:
- `FowlEngine/procman: rendered ...\gci.json from YAML`
- `FowlEngine/procman: started netidx resolver`
- `FowlEngine/procman: launching bfdb.exe ...`
- no tracebacks from `fowlengine` / `bfbinaries`

Then check:

```powershell
Invoke-WebRequest http://localhost:8880/api/health          # -> 200
Test-NetConnection 127.0.0.1 -Port 4564 -InformationLevel Quiet   # -> True
Get-Content "C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\gci.json"
```

In Discord: `/fe_gci` shows the frequencies; the #vs-server-info embed appears
within ~2 min; start the DCS server and confirm the #vs-status embed populates.

Stop with Ctrl-C. If bfdb didn't stop, `taskkill /IM bfdb.exe /F`.

---

## 7. Install as a Windows service

```powershell
cd E:\Github\bfnext-vector\deploy\windows-service
# edit the param defaults at the top if your paths/account differ, then:
powershell -ExecutionPolicy Bypass -File .\install-service.ps1
```

It runs NSSM (auto-start, restart-on-exit, log to `service.log`) + `sc failure`
recovery, and starts the service. Full command list and uninstall steps are in
`deploy/windows-service/README.md`.

**Reboot test:** `Restart-Computer`, wait, then with **nobody logged in**:

```powershell
Get-Service DCSServerBot                                     # Running
Invoke-WebRequest http://localhost:8880/api/health           # 200
Test-NetConnection 127.0.0.1 -Port 4564 -InformationLevel Quiet   # True
```

Crash test: `taskkill /IM python.exe /F` → NSSM restarts the bot in ~15 s.
`taskkill /IM bfdb.exe /F` → the plugin relaunches bfdb within ~1–2 min and
posts to #vs-ops.

---

## 8. Day-to-day operations (all from Discord, no RDP)

**Ship a new engine build**
1. Build `bflib.dll` / `bfdb.exe` locally (step 1), or have CI produce them.
2. Drag the file into **#vs-admin**. The bot stages it and replies with the
   next-rotation time.
3. It swaps in automatically on the next scheduled DCS restart (backup kept as
   `bflib.dll.backup-<timestamp>`, newest 5 retained).
   - `/feops stage_status` — what's staged + when it applies
   - `/feops stage_apply <server> bflib.dll` — do it now (server must be shut
     down for bflib; bfdb swaps live)
   - `/feops stage_cancel all` — discard

**Change GCI settings** (frequency, callsigns, voices, EAM passwords)
1. Edit the `gci:` block. Easiest without RDP: download `fowlengine.yaml` via
   the bot's `/download`, edit, drag it back into an admin channel (the bot
   rewrites `config/` and restarts).
2. `/feops bfdb_restart` — re-renders `gci.json` and restarts bfdb.
3. `/feops gci_show` — confirm the effective config (secrets masked).

**Change any bfdb setting** (ports, CORS, netidx base, engine config path): same
as GCI — edit the `bfdb:` block, upload, `/feops bfdb_restart`.

**Check server state**: the #vs-server-info embed (connect string, status, GCI
freqs, engine build sha, pending updates, next rotation), or `/fe_gci`.

**GCI transcript**: every AWACS call is mirrored to #vs-gci, 🔵/🔴 by side.

---

## 9. Troubleshooting

| Symptom | Check |
|---|---|
| `bfdb.exe not found` in log | `bfdb.exe` at `bfdb.exe:` path; rebuild + copy |
| GCI silent | `gci.enabled: true`; `opus.dll` present; `--base` set (`netidx_base`); SRS running; bfdb log for `gci:` lines |
| `/feops` commands missing | plugin loaded? (`opt_plugins`), `/reload fowlengine`, wait for Discord command sync |
| drag-drop upload does nothing | you're in the **admin** channel + have the `DCS Admin` role; `servers.yaml` has `channels: admin:` |
| two bfdb processes | you're still running `bfsystem.ps1` — stop it, `bfdb.manage` owns it now |
| resolver not starting | `netidx` on PATH for the service account; `netidx-resolver.json` path in `bfdb.netidx_resolver_config` |
| service won't start | `service.log` in the bot dir; account has "Log on as a service"; `run.cmd` works interactively first |
| staged bflib.dll didn't swap | it applies on the **next DCS restart**; `BFBinaries` in `nodes.yaml` with the right `bflib_dll_path`; check the bot log at startup for `BFBinaries: engine updated` |

`bfsystem.ps1` still works as a manual fallback if the bot is down — just never
run it while `bfdb.manage: true` and the service is up.
