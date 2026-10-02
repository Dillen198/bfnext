# Running DCSServerBot as a Windows service (no login, auto-restart)

> **Superseded by [Fowl Engine Manager](../../bfmanager/README.md)**, an installer and app
> that creates its own boot-time service (`FowlEngine`) and disables this NSSM one when
> it is set up. These scripts still work if you'd rather not use the app. Don't run both.

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

2. Make sure the bot has been through its first-run setup once interactively
   (`run.cmd` from a normal shell) so `Server Bot\config\` is fully populated
   and no console prompt is hit on a clean start. Stop that interactive bot
   afterwards -- two bots would fight over DCS.

3. Run `install-service.ps1` **from an elevated PowerShell** (edit the paths
   at the top first, or pass `-BotDir` / `-Account`). It:
   - installs the service with **delayed** auto-start (after the network is up),
   - runs it as the account that owns `Saved Games\DCS.*` and
     `%APPDATA%\netidx\client.json`, and grants it *Log on as a service*,
   - restarts it on exit (NSSM) and on failure (Windows service recovery),
   - sets `AppKillProcessTree 0`: **a bot restart leaves DCS running** (the bot
     re-attaches; procman replaces an orphaned bfdb.exe),
   - gives the bot 20 s to shut down cleanly, and rotates `service.log`.

4. Run `check-autostart.ps1` (read-only). It checks the service, restart after
   a blue screen (Windows' *Automatically restart*), pending Windows Update
   reboots / active hours, recent unexpected shutdowns, and that bfdb and the
   netidx resolver answer. Fix anything it flags.

5. BIOS/UEFI: *Restore on AC power loss* -> *Power On*, so a power cut ends
   with the box booting too (not checkable from Windows).

## Verify

```powershell
.\check-autostart.ps1
```

Then `Restart-Computer` and run it again (over SSH / from another PC) with
**no interactive login**. Kill `python.exe` (the bot) and confirm NSSM brings
it back within ~15 s *and DCS stays up*; `taskkill /IM bfdb.exe /F` and
confirm the plugin relaunches bfdb. The dashboard's OPS page shows all of this
live (service state, boot time, crash history) -- see ../auto-update.md.

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
