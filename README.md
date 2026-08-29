# Fowl Engine (BFNEXT)

A dynamic campaign system for DCS World: persistent multiplayer warfare with
territory control, logistics, JTAC/EWR, SAM sites, CSAR, and a live web
dashboard — built in Rust.

- **Discord**: https://discord.gg/wAsBEfse
- **Presentation**: https://docs.google.com/presentation/d/1EAOe0iK-1s6i0UV5ObxSD86gGBj1Ixz6FOotQn5XPdc/edit#slide=id.g2b6a346170f_1_35
- **Test server**: The Coop - Operation Fowl Intent

## What's in this repo

| Crate/app | What it is |
|---|---|
| `dcso3` | Rust bindings to DCS's Lua scripting API |
| `bflib` | The campaign engine itself — compiled to `bflib.dll`, loaded into DCS via `require()` |
| `bfprotocols` | Shared config/data types used across the other crates |
| `bfdb` | Stats database + web server: serves the JSON/WebSocket API and (by default) the dashboard/site below |
| `bftools` | CLI: builds the final `.miz` mission file from templates, and generates `special_sam_sites` config from a mission editor template |
| `bfweb` | The admin/stats dashboard (React) |
| `bfsite` | The public-facing campaign website (React) |
| `bfwiki` | The player/gameplay wiki (React) — public to read, admin login to edit; content lives in `bfdb`, seeded from `bfdb/seed_wiki/*.md` |
| `user-guide` | Static mdBook version of the gameplay docs |
| `yats` | Small typed wrapper around the Sled embedded database |

## Prerequisites

- Rust (stable toolchain)
- Node.js + npm, for `bfweb`/`bfsite`
- A local DCS World install — `bflib` (and transitively `bfdb`, since it
  depends on the same `dcso3`/`bfprotocols` crates) link against DCS's own
  Lua 5.1 runtime, so a real `lua.dll`/`lua.lib` from your DCS install is
  required just to compile.

## Building

**1. Set up the Lua linking environment** (every new shell, before any `cargo build`):

```bash
# Linux/macOS
source setup-build.sh
```
```powershell
# Windows
. .\setup-build.ps1
```
This sets `LUA_LIB` (path to DCS's `lua.dll`/`lua.lib`), `LUA_LINK=dylib`,
and `LUA_LIB_NAME=lua`. When DCS updates, copy the new `lua.dll` from DCS's
`bin-mt` folder and regenerate `lua.lib` with `dll2lib.bat` (Windows SDK
required).

**2. Build the engine DLL** (the primary target — what actually runs in DCS):

```bash
cargo build --release --package=bflib
```
Output: `target/release/bflib.dll`. Copy it into your DCS mission folder to
test in-game.

**3. Build everything else:**

```bash
cargo build --release          # bfdb, bftools, dcso3, bfprotocols, yats
cd bfweb  && npm ci && npm run build   # dashboard
cd bfsite && npm ci && npm run build   # public site
cd bfwiki && npm ci && npm run build   # player wiki (deployed separately, e.g. Vercel)
```

`bfdb` embeds the built `bfweb`/`bfsite` directly (via `rust-embed`), so
build the frontends *before* `bfdb` if you want an up-to-date embedded copy.
`bfwiki` is hosted separately and talks to `bfdb`'s API; on first run `bfdb`
seeds the wiki database from `bfdb/seed_wiki/*.md`, after which pages are
edited live through the site by anyone with admin access. Edit the seed
files for the *initial* content of a fresh database.

## Running the campaign config + dashboard (bfdb)

`bfdb` serves stats, the live map, and admin tools, all as one process:

```bash
bfdb.exe \
  --db "path/to/bfdb-data" \
  --listen-address 0.0.0.0:8765 \
  --site-address 0.0.0.0:8766 \
  --config "path/to/campaign.json" \
  --admin-username admin --admin-password changeme
```

See `campaign.sample.json` for dashboard branding options (name, colors,
Discord link, map center — copy it, edit it, pass it via `--config`).

Full flag reference: `bfdb.exe --help`. Notable ones:

- `--engine-config <path>` — points at the **same JSON file `bflib` loads**
  (e.g. `ODFv2_CFG`). Enables the in-dashboard **Config Editor**
  (`/admin/config`): a form generated live from the engine's actual config
  schema, so every field bflib accepts is editable without hand-writing
  JSON. Saving validates against that schema before writing to disk and
  keeps a timestamped backup of the previous file. Changes apply on the
  next mission/server restart (bflib only reads its config once at
  startup).
- `--base <netidx-path>` — connects to netidx for live stats and the
  in-dashboard **Engine Log** panel (bflib's own log stream, separate from
  bfdb's process log — both are visible under Admin).
- `--stats-jsonl` / `--stats-dir` — where sortie/kill stats come from.
- `--discord-client-id` / `--discord-client-secret` / `--discord-redirect-uri`
  / `--discord-guild-id` / `--discord-admin-role-id` — Discord OAuth login.
  Omit any of these to fall back to `--admin-username`/`--admin-password`
  local login only.
- `--srs-url` — proxies an SRS server for the dashboard's radio panel.
- `--cors-origin <url>` (repeatable) — only needed if you're hosting
  `bfweb`/`bfsite` separately from `bfdb` instead of using the embedded
  copies. See **[deploy/README.md](deploy/README.md)** for that whole setup
  (Docker/nginx for the frontends, Caddy for TLS in front of `bfdb`).

`bfsystem.ps1` is a ready-made PowerShell launcher wiring these flags
together for a Windows game server — copy and edit the variables at the top.

## Generating the final mission (bftools)

```bash
bftools.exe miz \
  --output final.miz --base base.miz \
  --weapon weapons.miz --options options.miz \
  [--warehouse warehouse.miz]
```
Merges a base mission with per-slot loadouts/Link-16 assignments (from the
weapon template) and warehouse/dynamic-spawn config, producing the mission
you actually run.

```bash
bftools.exe special-sam \
  --template "path/to/special-sam-template.miz" \
  --output special_sam_sites.json \
  [--merge-into path/to/ODFv2_CFG]
```
Reads a dedicated mission-editor template and generates `special_sam_sites`
config entries — hidden, map-fixed SAM sites capturable by ground troops.
Every vehicle/static group whose name is `<Location> - <Label>` (no
Red/Blue prefix — the group's coalition placement is what determines its
starting owner) becomes one site; the opposite coalition's mirror is
synthesized automatically so the site can flip on capture. `--merge-into`
writes the result straight into an existing campaign config's
`special_sam_sites` array instead of just producing a standalone file.

## Testing

Limited automated coverage; most testing is done live in DCS. See
`TESTING.md` for the manual QA checklist, and:
```bash
cargo run --bin test --package=bfdb
```

## Hosting the dashboard/site separately from bfdb

By default everything above runs as one process on the game server
machine. If you want `bfweb`/`bfsite` hosted elsewhere (a cloud VPS, static
host, etc.) while `bfdb` stays with the game server, see
**[deploy/README.md](deploy/README.md)**.

## Licenses

`dcso3` is MIT. `bflib` is AGPL v3.
