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
| `src-tauri/src/backup.rs` | whole-box backup to one zip, and restore onto a fresh Windows |
| `src-tauri/src/lib.rs` | the desktop app's commands |
| `src-tauri/nsis/hooks.nsh` | installer: stop the service before copying files, start it after; keep it on `/UPDATE` |
| `../bfweb/src/manager/` | the UI. It is built from bfweb, so it shares React, the design and the OPS page |
| `scripts/stage-bot.mjs` | copies the bot plugin and extensions into the bundle (never `fowlengine.yaml`) |
| `release.ps1` | build, sign and publish `manager-v<version>` |

## Backup and restore (moving PC, reinstalling Windows)

The **BACKUP** tab writes one `FowlEngine-backup-<pc>-<date>.zip`:

- the DCSServerBot folder with `config\` and `config\.secret` (the venv, caches and logs stay out);
- every `Saved Games\DCS.*` instance: Config, Missions, Scripts, Mods, the campaign saves, the bfdb
  database (`bfdb\`) and `Logs\stats*` (no tracks, screenshots, shader caches or DCS logs);
- bfdb's home if it is somewhere else, and other folders the bot's config points at (offered;
  folders over 1 GB start unticked);
- this app's `manager.json`;
- the SRS server program folder (from `nodes.yaml` `extensions.SRS.installation`), restored to the
  same place, so no SRS installer is needed;
- netidx: `netidx.exe` only, out of `.cargo\bin` or wherever it is on PATH (the restore puts that
  folder on the machine PATH), and the client config (`%APPDATA%\netidx`, `~\.config\netidx`,
  `C:\netidx`). `netidx-resolver.json` is already in the DCS server folder;
- the bot's PostgreSQL database (`pg_dump -Fc`, password from `config\.secret\database.pkl`).

By default it reads every file from a **Windows shadow copy** (VSS, via WMI
`Win32_ShadowCopy.Create`; needs the app elevated, which it always is), one per drive. Files that
bfdb and DCS hold open (`bfdb\db`, `Logs\stats\0\current`) come out whole and all from the same
instant, with nothing stopped. The snapshots are deleted when the copy is done. Without one
(it failed, or was unticked), a locked file comes out partial, and the check marks that section
BROKEN. Stopping DCSServerBot while copying is still offered, but off by default. The zip holds the Discord token and passwords: keep it private.

**Backup password (optional).** `BackupOptions.password` (8 or more characters) encrypts every
entry with AES-256, in the WinZip AES format that 7-Zip also opens. `fowl-backup.json` stays
readable, so a restore can show what is inside before it asks. Its `encrypted: true` tells the
restore and **Check a backup** to ask for the password (`RestoreOptions.password`, and the
`password` argument of `verify_backup`). A wrong password is refused with one clear error.
Keep the password off the PC, because without it the zip can't be restored. *The BACKUP page in
bfweb (`src/manager/Backup.tsx`) doesn't have the field yet. Until it does, backups are written
unencrypted, as before.*

For a nightly, encrypted, off-box copy of just the bot's database and config, see
[`deploy/backups/`](../deploy/backups/README.md).

**Restore** (same tab, or the button on Setup for a fresh box) shows what is missing on this PC
(Python, PostgreSQL, DCS, SRS). It can install Python and PostgreSQL with winget, lets the admin
move each folder somewhere else, and then:

- unpacks the zip. A folder that is already there is renamed `*.before-restore-<time>`, never deleted;
- rewrites old paths in the text config files when a folder or the Windows user changed;
- renames the node in `nodes.yaml` when the PC's name changed;
- loads the database (creates the bot's role and database with the `postgres` password);
- restores `manager.json` and installs and starts the service.

**Checking a backup.** Every backup ends by reading the whole zip back, which checks each file's
CRC. It then compares each section with what was on disk and looks for the files a restore needs
(`nodes.yaml`, `.secret`, `serverSettings.lua`, missions, `bfdb\`, `netidx.exe`, the SRS program, a
`PGDMP` database dump). The report shows OK / CHECK (some files couldn't be read at backup time) /
BROKEN per section. A file that doesn't read back (CRC mismatch) is written again: the zip is
rebuilt with every good entry raw-copied and the bad ones re-read from disk, then checked again,
up to 3 times. That fixes a one-off bit flip. A file that keeps failing means the PC itself is
damaging data (RAM or disk). **Check a backup** runs the same check on any zip, e.g. the copy on the USB
stick. The whole log is saved as `<zip>.log` next to the zip and as `logs\<backup|verify|restore>-<time>.log`.

It can't restore automatic sign-in, because Windows never hands the old password back. Turn it
on again in Setup step 4. The same goes for the GitHub token in Settings when the PC changed:
`manager.json` keeps it DPAPI-encrypted to the PC it was saved on (below), so on a new PC the
restore drops it, logs a warning, and you type it in again.

**The GitHub token in `manager.json`** (optional, for a private repo or rate limits) is stored as
`github_token_dpapi`: Windows DPAPI, machine scope, plus app-specific entropy. The GUI (an
elevated admin) and the service (LocalSystem) can both read it, but a copy of the file (in a
backup zip, a disk image or a support upload) can't be decrypted on any other PC. On this PC the
folder ACL (Administrators + SYSTEM, `agent::restrict_data_dir`) still decides who can read it.
A token that an older version saved in plain text is encrypted the first time the GUI or the
service loads the file. If you downgrade to a version older than this one, it won't see the token.

`cargo test --lib e2e -- --ignored` (with `FOWL_E2E_DIR` set to a scratch folder) runs a full
backup, wipe and restore round trip on a fake box.

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
`TAURI_SIGNING_PRIVATE_KEY` secret. The password, offline backup and rotation steps for
this key and the other release keys are in
[`deploy/signing-keys.md`](../deploy/signing-keys.md).

### Authenticode (code signing): not set up yet

The minisign/Tauri signature above protects **updates**. The first install is a different matter:
`Fowl Engine Manager_<version>_x64-setup.exe` and `FowlEngineManager.exe` carry no Authenticode
signature. SmartScreen warns about an unknown publisher, and nothing proves who built the
installer that was downloaded. Fixing that needs a code-signing certificate, which costs money
and requires identity checks, so it hasn't been done. When there is one:

1. **Get a certificate.** One of:
   - **Azure Trusted Signing** (Microsoft, pay-monthly, cloud HSM). It is the cheapest option and
     needs no hardware token. Identity validation takes days. Signing goes through
     `signtool` + the Trusted Signing dlib, or the `azure/trusted-signing-action` in CI.
   - **An OV or EV certificate** from a CA (DigiCert, Sectigo, SSL.com, ...). Since 2023 these
     keys must live in a hardware token or a cloud HSM (e.g. SSL.com eSigner), so plan CI around
     that HSM. EV no longer skips SmartScreen's reputation building: both types earn it over time.
2. **Sign in the build.** Tauri signs the exe and the NSIS installer when
   `bundle.windows.signCommand` is set in `src-tauri/tauri.conf.json`. Point it at a script that
   calls `signtool sign /fd sha256 /tr http://timestamp.digicert.com /td sha256 ... %1`, or at
   the Trusted Signing equivalent. Always timestamp, or signatures die when the certificate
   expires.
3. **CI.** In `.github/workflows/manager-release.yml`, the signing credentials go on the
   `manager-release` environment (required reviewers), like the Tauri key. Authenticode signing
   has to happen *before* the Tauri updater signature is made, because the `.sig` covers the
   bytes of the signed installer. So sign inside the build via `signCommand`, not after it.
   `release.ps1 -NoSign` then has to leave Authenticode on.
4. **Check** with `Get-AuthenticodeSignature '.\Fowl Engine Manager_*_x64-setup.exe'`
   (Status `Valid`) and in the file's Properties → Digital Signatures.

The same applies to `bfcockpit-install.exe` and the engine binaries if they are ever offered as
downloads.

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
