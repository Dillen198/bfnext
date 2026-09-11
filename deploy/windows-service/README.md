# Running DCSServerBot as a Windows service (no login, auto-restart)

Goal: after a Windows reboot or a crash, the whole Vector Strike stack comes
back **without anyone logging in or RDPing**:

```
Windows boots
   └─ Service "DCSServerBot" auto-starts (as the ATPAdmin account)
        ├─ DCSServerBot connects to Discord
        ├─ Scheduler launches DCS.exe (as KillerDog198, per nodes.yaml DCS.user)
        └─ fowlengine plugin (bfdb.manage: true) launches:
             ├─ netidx resolver  (child process)
             └─ bfdb.exe         (child process, renders gci.json from YAML first)
```

DCS crash -> the bot's Scheduler/processmanager relaunches it.
bfdb crash/hang -> the fowlengine plugin's `supervise_bfdb` loop relaunches it.
Bot crash -> the service manager (NSSM) restarts the service.

DCSServerBot 3.x has **no bundled service installer**, so we wrap `run.cmd`
with [NSSM](https://nssm.cc/).

## One-time install

1. Install NSSM (`choco install nssm`, or unzip it and put `nssm.exe` on PATH).

2. Give the service account the **Log on as a service** right:
   `secpol.msc` -> Local Policies -> User Rights Assignment -> *Log on as a
   service* -> add `ATPAdmin`. (The stack must run as the account that owns
   `C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1` and
   `%APPDATA%\netidx\client.json`.)

3. Make sure the bot has been through its first-run setup once interactively
   (`run.cmd` from a normal shell) so `Server Bot\config\` is fully populated
   and no console prompt is hit on a clean start.

4. Run `install-service.ps1` **from an elevated PowerShell** (edit the paths
   at the top first). It runs the NSSM commands below and starts the service.

```powershell
nssm install  DCSServerBot "E:\Github\DCSServerBot\run.cmd"
nssm set DCSServerBot AppDirectory  "E:\Github\DCSServerBot"
nssm set DCSServerBot ObjectName    ".\ATPAdmin" "<password>"
nssm set DCSServerBot Start         SERVICE_AUTO_START
nssm set DCSServerBot AppExit       Default Restart
nssm set DCSServerBot AppRestartDelay 15000
nssm set DCSServerBot AppStdout     "E:\Github\DCSServerBot\service.log"
nssm set DCSServerBot AppStderr     "E:\Github\DCSServerBot\service.log"
nssm set DCSServerBot AppRotateFiles 1
nssm set DCSServerBot AppRotateBytes 10485760
sc.exe failure DCSServerBot reset= 86400 actions= restart/15000/restart/30000/restart/60000
nssm start DCSServerBot
```

## Verify

```powershell
Get-Service DCSServerBot                                   # Running
Invoke-WebRequest http://localhost:8880/api/health         # 200
Test-NetConnection 127.0.0.1 -Port 4564 -InformationLevel Quiet   # True (netidx)
```

Then `Restart-Computer` and confirm all three still pass with **no interactive
login**. Kill `python.exe` (the bot) and confirm NSSM brings it back within
~15 s; `taskkill /IM bfdb.exe /F` and confirm the plugin relaunches bfdb.

## Uninstall

```powershell
nssm stop DCSServerBot
nssm remove DCSServerBot confirm
```

## Notes

- `bfsystem.ps1` is now a **manual fallback only** -- with `bfdb.manage: true`
  in `fowlengine.yaml` the bot owns bfdb + the resolver. Don't run both.
- The repo's `plugins/fowlengine/` and `extensions/bfbinaries/` must be present
  in the DCSServerBot install (copied or symlinked), same as the fowlengine
  plugin already is.
