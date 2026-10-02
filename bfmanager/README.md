# Fowl Engine Manager

A Windows application that installs, runs and updates a Vector Strike server box.

- **Installer:** `Fowl Engine Manager_<version>_x64-setup.exe` installs into Program Files
  and adds a Start-menu entry and uninstaller.
- **Setup wizard** (first launch): find DCSServerBot, install the Fowl Engine bot plugin
  into it, then create the **FowlEngine Windows service**.
- **The service** is the same exe, run with `--service`:
  - starts DCSServerBot at boot (after a reboot, a blue screen or a power cut) **on the
    desktop of the server's Windows user**, as that user; DCSServerBot then runs DCS, and the
    Fowl Engine plugin runs bfdb and netidx. The service runs as LocalSystem only so that it
    can start programs in that user's session: a service's own session 0 has no desktop, and
    DCS (2.9+) hangs creating its window there ("timeout while launching", then killed as
    hung). Windows' automatic sign-in (Setup step 4) brings that session back by itself;
    the password is kept as the LSA secret `DefaultPassword`, as Sysinternals Autologon does;
  - restarts the bot if it dies, backing off 5 s → 5 min;
  - before every bot start, syncs the plugin bundled with the app into the bot;
  - checks GitHub for a newer manager and installs it (within an optional time window).
    The install restarts the bot, not DCS.
- **The app:** overview (service, bot, plugin, updates), **Server OPS** (the dashboard's
  OPS page, unchanged, talking to the local bot), settings, and logs.
- **Engine binaries** (`bflib.dll`, `bfdb.exe`, `bftools.exe`) are still updated by the
  Fowl Engine plugin's auto-updater, with probation and rollback (see
  [../deploy/auto-update.md](../deploy/auto-update.md)). The manager takes care of the box
  and the bot around it.

```
Windows boot
 └─ service FowlEngine  (FowlEngineManager.exe --service, delayed auto-start, restart on failure)
     ├─ syncs bot\ (this app's plugin bundle) → DCSServerBot\plugins\fowlengine, extensions\bf*
     ├─ cmd /c run.cmd in DCSServerBot  →  DCS servers, bfdb, netidx
     ├─ self-update: GitHub manager-v* → verify signature → setup.exe /S /UPDATE
     └─ %ProgramData%\FowlEngine\  status.json · commands\ · logs\ · backups\ · manager.json
```

## Layout

| Path | What |
|---|---|
| `src-tauri/src/agent.rs` | the service: bot supervisor, plugin sync, self-update, status file |
| `src-tauri/src/winsvc.rs` | install/control the service, "Log on as a service", elevation |
| `src-tauri/src/update.rs` | GitHub releases, minisign verification, running the installer |
| `src-tauri/src/bot.rs` | finding DCSServerBot, plugin sync with backups, the bot's OPS API address |
| `src-tauri/src/lib.rs` | the desktop app's commands |
| `src-tauri/nsis/hooks.nsh` | installer: stop the service before copying files, start it after; keep it on `/UPDATE` |
| `../bfweb/src/manager/` | the UI. It is built from bfweb, so it shares React, the design and the OPS page |
| `scripts/stage-bot.mjs` | copies the bot plugin and extensions into the bundle (never `fowlengine.yaml`) |
| `release.ps1` | build, sign and publish `manager-v<version>` |

## Develop

```powershell
cd bfmanager
npm install
npx tauri dev          # starts bfweb's manager dev server (:5190) and the app
cargo test --manifest-path src-tauri/Cargo.toml
src-tauri\target\debug\FowlEngineManager.exe --console   # run the service loop in a terminal
```

`tauri dev` runs unelevated, because debug builds leave out the admin manifest.
Service install and control need an elevated terminal.

## Release

1. Bump `version` in `src-tauri/Cargo.toml`. Installed copies only move to a *newer* version.
2. `.\bfmanager\release.ps1` (or `-Channel beta`, or `-DryRun`).

It builds the UI and the bot bundle, compiles, builds the NSIS installer, signs it
with `%USERPROFILE%\.tauri\fowl-manager.key`, and publishes the installer and its
`.sig` as GitHub release `manager-v<version>`.

**The signing key.** Every installed copy carries `src-tauri/updater.pub` and runs only
installers signed by the matching private key. If the key is lost, no installed copy
accepts an update again until it is reinstalled by hand. Back up
`%USERPROFILE%\.tauri\fowl-manager.key`. For the GitHub Action, store it as the
`TAURI_SIGNING_PRIVATE_KEY` secret.

## Install on the server

1. Run `Fowl Engine Manager_<version>_x64-setup.exe`.
2. The app opens on **Setup**:
   - **DCSServerBot folder.** Its own first-run setup (Discord, servers) must be done already.
   - **Install plugin.**
   - **Service.** Pick the account that owns `Saved Games\DCS.*` and `%USERPROFILE%\.dcssb`
     (normally an admin): the bot runs on its desktop. The wizard disables the older NSSM
     `DCSServerBot` service so two bots don't run.
   - **Automatic sign-in** for that account, and optionally lock the screen right after it.
3. Restart the PC without signing in and check that DCS and the dashboard come back.
   In the BIOS, set *Restore on AC power loss* to Power On.
