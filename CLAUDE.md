# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Fowl Engine** (BFNEXT) is a dynamic campaign system for DCS World, built in Rust. The project enables persistent, multiplayer warfare campaigns with territory control, logistics, objectives, and various combat systems.

- **Discord**: https://discord.gg/wAsBEfse
- **Presentation**: https://docs.google.com/presentation/d/1EAOe0iK-1s6i0UV5ObxSD86gGBj1Ixz6FOotQn5XPdc/edit#slide=id.g2b6a346170f_1_35
- **Test Server**: The Coop - Operation Fowl Intent

## Build Setup

### Prerequisites

Before building, you must configure environment variables to link against DCS's Lua runtime:

**Linux/macOS**:
```bash
source setup-build.sh
```

**Windows (PowerShell)**:
```powershell
. .\setup-build.ps1
```

This sets:
- `LUA_LIB`: Path to lua.dll/lua.lib
- `LUA_LINK`: "dylib" (dynamic linking)
- `LUA_LIB_NAME`: "lua"

**Important**: The project must link to the same Lua version as DCS (currently Lua 5.1). Dynamic linking ensures compatibility. When DCS updates, copy the new `lua.dll` from DCS's `bin-mt` folder and regenerate `lua.lib` using `dll2lib.bat` (Windows SDK required).

### Building

**Primary build target** (DCS mission script DLL):
```bash
cargo build --release --package=bflib
```

Output: `target/release/bflib.dll` (or `.so` on Linux)

**Build all workspace members**:
```bash
cargo build --release
```

**Fast iteration builds**: The commented-out profile in root `Cargo.toml` provides faster builds for development, though mlua requires release mode for proper linking.

**Build bftools** (mission file generator):
```bash
cd bftools
cargo build --release
```

**Build user guide** (MDBook):
```bash
cd user-guide
mdbook build
mdbook serve  # View at http://localhost:3000
```

### Testing

CI (`.github/workflows/build.yml`) runs these jobs on every push and PR:

| Job | Runner | What |
|---|---|---|
| `check` | Windows | `cargo check` + `cargo test --workspace --locked` (about 450 Rust tests), `cargo check` of bftools |
| `rust-lint` | Windows | `cargo fmt --all --check` and `cargo clippy --workspace --all-targets`, **report-only** (`continue-on-error`) until the tree is clean |
| `manager` | Windows | `cargo test` for `bfmanager/src-tauri` (own Cargo.lock), after staging a placeholder `bfmanager/dist` and the bundled resources |
| `frontend` | Ubuntu | bfweb, bfsite, bfwiki, bfrange-web: `npm ci --ignore-scripts`, then `typecheck`/`lint`/`test`/`build` (each `--if-present`), Node 22 |
| `bot` | Windows | `compileall` over `DCSServerBot/`, then pytest in `DCSServerBot/tests` |

Locally:

```bash
cargo check --workspace --locked
cargo test --workspace --locked
cargo check --locked --manifest-path bftools/Cargo.toml
cargo test --locked --manifest-path bfmanager/src-tauri/Cargo.toml   # see notes
npm --prefix bfweb test        # vitest
npm --prefix bfrange-web test  # vitest
pip install -r DCSServerBot/tests/requirements.txt && python -m pytest -q   # in DCSServerBot/tests
```

Notes:
- The test binaries link `lua.dll` dynamically, so the repo root must be on
  `PATH` (CI adds `$GITHUB_WORKSPACE`; locally, source `setup-build.ps1`).
- bfdb embeds `bfweb/dist` and `bfsite/dist` with RustEmbed and will not
  compile if they are missing. Build the frontends first, or create empty
  directories (CI stubs both).
- bfmanager's build needs `bfmanager/dist` (any `index.html` will do for
  tests) and the bundles from `node bfmanager/scripts/stage-bot.mjs` and
  `stage-range.mjs`. `tauri build` makes them all for real.
- The bot tests' requirements leave out discord.py on purpose: the icon
  tests check the plain-unicode fallback. With discord.py installed,
  `test_fowlengine_cog_wiring.py` also imports the real cog.
- No test needs DCS, a netidx resolver or the network.

Gameplay is still verified in DCS: `TESTING.md` is the in-game checklist.

## Workspace Architecture

The root `Cargo.toml` workspace has 9 members: `dcso3`, `livewx`, `bflib`,
`bfrange`, `bfdb`, `bfprotocols`, `yats`, `bfcockpit/installer` and
`bfcockpit/net`. `bftools` is excluded and builds on its own (own
`Cargo.lock`, vendored Lua).

### dcso3 - DCS Lua API Bindings
- **Purpose**: Minimal, safe Rust binding to DCS's Lua scripting API
- **Type**: Library (published crate, docs at https://docs.rs/dcso3)
- **License**: MIT
- **Location**: `dcso3/src/`
- **Modules**:
  - `env/`: Mission environment and miz file handling
  - `event.rs`: DCS event system
  - `hooks.rs`: DCS hook system for mission scripts
  - `net.rs`: Multiplayer networking APIs
  - `controller.rs`, `group.rs`, `unit.rs`: Unit/group control
  - `coalition.rs`, `country.rs`: Faction management
  - `trigger.rs`, `timer.rs`: Mission scripting utilities
  - `world.rs`, `land.rs`: World queries
  - `mission_commands.rs`: F10 radio menu system

### livewx - Live Weather
- **Purpose**: Real-world weather and time for a .miz: fetches METAR (CheckWX)
  and open-meteo and rewrites the mission before it next loads
- **Type**: Library, shared by bflib and bfrange
- **License**: MIT

### bflib - Campaign Mission Script
- **Purpose**: Main campaign logic, loaded as a DLL into the DCS Lua environment
- **Type**: cdylib
- **License**: AGPL v3
- **Location**: `bflib/src/`
- **Key modules**:
  - `lib.rs`: Entry point, event handlers, main game loop
  - `db/`: Campaign state (players, objectives, groups, logistics, cargo, actions)
  - `menu/`: F10 radio menus (actions, JTAC, cargo, troops, EWR, ...)
  - `bg/`: Background tasks on tokio (netidx, log and stats publishing, RPCs)
  - `groundwar/`, `hq/`, `commander.rs`, `modern_war/`: AI ground war and commander
  - `jtac.rs`, `ewr.rs`, `atis.rs`, `navaids.rs`, `cockpit.rs`: player-facing systems
  - `chatcmd.rs`, `admin.rs`, `command/`: chat and admin commands
  - `spawnctx.rs`, `shots.rs`, `msgq.rs`, `unitdb.rs`: spawning, kill tracking,
    message queue, unit data

### bfrange - Training Range Mission Script
- **Purpose**: Vector Strike training range engine. Scores bombing, strafing,
  carrier passes, AAR, missile defence, anti-ship, helicopter and
  combined-arms drills
- **Type**: cdylib (`bfrange.dll`)
- **License**: `bfrange/LICENSE` (all rights reserved)
- **Location**: `bfrange/src/`; the range mission is built by
  `bfrange/mission/build_range_miz.py` into `bfrange/mission/dist/`

### bfdb - Database & Web Server
- **Purpose**: Stats database, persistence and the HTTP/websocket backend for
  every web app (warp). Embeds the `bfweb` and `bfsite` builds
- **Type**: Binary `bfdb` (server). The old `test` and `migrate` helper
  binaries were removed
- **Location**: `bfdb/src/`
- **Key files**: `main.rs` (routes), `db.rs` (Sled database), `db_id.rs`,
  `range/` (training range API), `news*.rs`, `gci.rs`, `milatc/`, `voice/`, `replay/`

### bfprotocols - Shared Protocol Types
- **Purpose**: Data structures shared by bflib, bfrange and bfdb
- **Type**: Library
- **Location**: `bfprotocols/src/`
- **Contents**: `cfg/` (campaign config), `db/` (objective/group schemas),
  `range/`, `stats.rs`, `shots.rs`, `perf.rs`, `situation.rs`, `api.rs`, ...

### yats - Yet Another Typed Sled
- **Purpose**: Type-safe wrapper around the Sled embedded database
- **Type**: Library
- **Location**: `yats/src/`
- **Features**: Big-endian number encoding for ordering, tuple-based prefix
  iteration, errors via anyhow (no panics on serialization errors)

### bfcockpit/installer and bfcockpit/net - Cockpit Plugin
- **Purpose**: Client-side DCS plugin: an in-game panel for the F10 menu,
  player status and the EWR picture. The Lua lives in `bfcockpit/Scripts`
  and `bfcockpit/Mods`
- `installer`: Win32 GUI and CLI installer (`bfcockpit-install`,
  `bfcockpit-install-cli`)
- `net`: cdylib giving the plugin HTTPS from DCS's GUI Lua state

### Excluded: bftools - Mission File Generator
- **Purpose**: CLI that builds the final DCS mission (.miz) from templates
- **Type**: Binary, not in the workspace
- **Location**: `bftools/`
- **Usage**:
  ```bash
  bftools.exe miz --output final.miz --base base.miz --weapon weapons.miz --options options.miz --warehouse warehouse.miz
  ```

## Apps

| Path | What it is |
|------|------------|
| `bfweb/` | Campaign dashboard (React + Vite). Also builds the Manager UI (`build:manager`) |
| `bfsite/` | Public Vector Strike website (React + Vite), embedded in bfdb |
| `bfwiki/` | Player wiki (React + Vite); content lives in bfdb, seeded from `bfdb/seed_wiki/` |
| `bfrange-web/` | Training range dashboard (React + Vite) |
| `bfmanager/` | Fowl Engine Manager: Tauri installer, Windows service and auto-updater for a server box |
| `DCSServerBot/plugins/fowlengine/` | DCSServerBot plugin: Discord status, alerts, ops commands, binary staging and auto-update. The cog is `commands.py` plus `cog_*.py` mixins (layout in its README). Related extensions are in `DCSServerBot/extensions/`; optional pip deps in `DCSServerBot/requirements-plugins.txt` |
| `user-guide/` | Player guide (mdBook) |
| `deploy/` | Deployment docs (server setup, auto-update, multi-instance, range, `backups/`, `monitoring.md`, `signing-keys.md`), Caddy config and `publish-release.ps1` |

## Data Flow

1. **DCS Mission → bflib**: DCS loads `bflib.dll` via Lua, fires events
2. **bflib → dcso3**: Campaign logic calls DCS API through dcso3 bindings
3. **bflib ↔ bfdb**: Netidx pub/sub for stats, state synchronization
4. **bfdb → Web**: Warp web server serves campaign UI and stats
5. **All crates → bfprotocols**: Shared types for serialization/communication

## Key Technologies

- **mlua**: Lua-Rust FFI (module mode for DLL loading)
- **Netidx**: Publish/subscribe system for distributed state
- **Sled**: Embedded database (wrapped by yats)
- **Tokio**: Async runtime for background tasks
- **Warp**: Web server framework
- **nalgebra**: Vector/matrix math for 3D operations
- **Serde**: Serialization (bincode, JSON)

## Development Workflow

1. **Source environment**: Run `setup-build.sh` or `setup-build.ps1`
2. **Make changes** to relevant crate
3. **Build**: `cargo build --release --package=<crate>`
4. **For bflib changes**: Copy `target/release/bflib.dll` to DCS mission folder, test in-game
5. **For dcso3 changes**: May affect all dependent crates
6. **For bftools changes**: Rebuild standalone in `bftools/` directory

## Important Notes

- **Lua linking**: Always use release builds for bflib due to mlua's linking requirements
- **DCS compatibility**: When DCS updates, verify Lua version compatibility
- **Licenses**: dcso3 and livewx are MIT, bflib is AGPL v3, bfrange has its own `bfrange/LICENSE`
- **Netidx**: Campaign uses netidx for RPC and stats - requires netidx infrastructure
- **Tests**: Unit tests cover the Rust crates (see Testing); gameplay is still verified in DCS
