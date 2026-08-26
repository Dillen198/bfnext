# Vector Strike DCSServerBot Plugin

The Vector Strike plugin for DCSServerBot bridges your DCS Vector Strike campaign to Discord. It features a continuously updating live campaign status, objective capture alerts, killstreak achievements, and interactive Discord slash commands for player stats and commander logistics.

## Features

- **Live Campaign Status Embed:** A single, continuously updating Discord embed showing live points, objective counts, and player counts per faction, plus which objectives are ready to capture, commander-priority targets, and the next rotation time as a localized Discord timestamp.
- **Live Engine Log Relay:** Tails bfdb's `/ws/engine-logs` websocket (the raw `bflib` engine log) into a Discord channel — one message is continuously edited with a rolling tail, and `[ERROR]`/`[WARN]` lines are additionally posted as standalone alerts so they don't get missed.
- **Capture/Neutral/Ready-to-Capture Alerts:** Polls bfdb's public `/api/objectives` every ~20s and diffs owner/health against the previous poll to detect captures, objectives going neutral, and objectives dropping to capturable health.
- **Per-Faction Alert Threads:** `alerts_channel` only needs to be set once — the plugin auto-creates a "Blue Ops" and "Red Ops" thread under it and routes alerts by relevance: a defending faction gets "ready to capture, defend it!" while the opposing faction gets "opportunity!" for the same event; captures post to both. Set `use_faction_threads: false` to go back to one shared channel.
- **Killstreak Achievements:** Polls bfdb's public `/api/kills` every ~20s to track each pilot's consecutive kills (reset on death) and announces streaks of 5 (Ace), 10 (Unstoppable), and 15 (God of War).
- **Mission-Briefing Welcome Message:** Posts an embed to `welcome_channel` when someone joins the Discord server, pulling the active scenario, round duration, and current front (objective counts per faction) from bfdb — same data as the live status embed — plus a customizable briefing blurb and dashboard link.
- **Server Performance Embed:** Posts and edits a live CPU/RAM/GPU/disk/temp + DCS frame-time embed every 5 minutes, pulled from bfdb's admin-only `/api/admin/perf`.
- **Dual-Login Dashboard:** Supports both standard Discord OAuth web-login and securely generated HMAC bot-tokens to seamlessly bridge the `bfweb` dashboard.
- **Interactive Commander Terminal:** A slick UI terminal allowing commanders to drop crates and infantry squads at airbases directly from Discord.
- **bfdb Crash Supervision:** Optionally health-checks bfdb.exe every ~30s and relaunches it via `bfsystem.ps1` if it stops responding, so a bfdb crash doesn't need someone to RDP in and restart it by hand. See `bfdb_supervisor` in Configuration.
- **bflib.dll Upload:** Admin-only `/vs fe_upload_bflib` command lets you ship a new engine build straight from Discord -- server must already be shut down, then it backs up the current DLL, overwrites it with your upload, and restarts the server.
- **Engine Error Feed:** bfdb keeps a rolling buffer of ERROR/WARN lines from the live engine log and exposes it at the admin-only `/api/admin/engine-errors` endpoint, shown as a persistent panel on the `bfweb` admin page -- so recent errors are visible even if nobody had the dashboard or Discord open when they happened, alongside the existing Discord relay (Live Engine Log Relay, above).

## Installation

1. Copy the `vectorstrike` folder into your `DCSServerBot\plugins` directory.
2. Ensure you have the `vectorstrike.yaml` configuration file set up (see Configuration below).
3. Restart your DCSServerBot instance so it loads the plugin.

## Configuration

In your DCSServerBot `config/plugins/` folder, create a file named `vectorstrike.yaml` and populate it with your channel IDs and secrets:

```yaml
DEFAULT:
  # The Discord channel where the live status embed will be continuously updated
  status_channel: 123456789012345678
  
  # The Discord channel for killstreaks (Ace, God of War)
  achievements_channel: 123456789012345678
  
  # The Discord channel for objective captures and team alerts
  alerts_channel: 123456789012345678

  # URL to your existing bfweb instance
  dashboard_url: "https://bfweb.your-domain.com"
  
  # Secret key to sign one-time auto-login tokens
  dashboard_secret: "YOUR_SUPER_SECRET_KEY"

  # The base URL to your bfdb REST API
  api_url: "http://localhost:8765"

  # (Optional) Discord channel for the live engine log relay. Omit to disable.
  engine_log_channel: 123456789012345678

  # (Optional) Whether alerts_channel gets auto-created "Blue Ops"/"Red Ops"
  # threads for faction-relevant routing. Defaults to true. Set to false to
  # post every alert straight to alerts_channel instead.
  use_faction_threads: true

  # (Optional) Discord channel for the mission-briefing welcome message,
  # posted whenever someone joins the Discord server. Omit to disable.
  welcome_channel: 123456789012345678

  # (Optional) Discord channel for the server performance/hardware embed
  # (CPU/RAM/GPU/disk/temps + DCS frame-time), updated every 5 minutes.
  # Omit to disable. bfdb must be running on the machine it's reporting on.
  perf_channel: 123456789012345678

  # Required if engine_log_channel or perf_channel is set -- must match
  # bfdb's own --admin-username/--admin-password startup flags.
  admin_username: "admin"
  admin_password: "YOUR_BFDB_ADMIN_PASSWORD"
```

## Slash Commands

### Player Commands
- `/vs dashboard` - Retrieves your secure Vector Strike web dashboard login link.
- `/vs join [Red|Blue|Neutral]` - Pre-selects your faction before slotting into a DCS aircraft.
- `/vs stats [@user]` - Shows pilot kills, highest streak, and points available.
- `/vs online` - Shows who's currently in a slot, grouped by faction.
- `/vs leaderboard [top]` - Shows the top pilots by kills, with captures and K/D.
- `/vs objective [name]` - Shows detailed status (owner, health, priority) for one objective; matches by substring.

### Commander & Admin Commands
- `/vs terminal` - Deploys the interactive Commander Terminal (UI buttons and dropdowns) allowing you to deploy logistics from Discord.
- `/vs spawn_deployable [type] [airbase]` - Direct command to spawn cargo/infantry at a specific airbase.
- `/vs priority [objective]` - Marks an objective as a high priority target for your team.
- `/vs ban [ucid] [name] [reason] [until]` - Bans a pilot from the campaign (requires admin_username/admin_password).
- `/vs unban [ucid]` - Removes a ban.
- `/vs fe_upload_bflib [server] [file] [restart]` - Uploads a `.dll` attachment and overwrites `bflib_dll_path` with it (after a timestamped backup of the current file), then restarts the server unless `restart:False` is passed. The server must already be shut down -- DCS.exe holds a file lock on bflib.dll while running, so there's no way to replace it live; shut the server down first (e.g. via DCSServerBot's own server stop/shutdown command), then run this. Requires `bflib_dll_path` to be set.

## Architecture & Integration
This plugin talks to bfdb, not directly to the DCS process. Three integration paths:

- **Read-only data** (`/status`, `fe_objectives`, `fe_stats`, capture/achievement polling, the engine log relay's history replay): plain HTTP/WebSocket calls to bfdb, no DCS-side setup needed beyond running bfdb itself.
- **Commander actions** (`fe_priority`, `fe_spawn_deployable`, `fe_terminal`): bfdb calls into bflib's live netidx RPC server (`bflib/src/bg/rpcs.rs`) -- e.g. `spawn-deployable`, `set-objective-priority` -- which requires `admin_username`/`admin_password` to authenticate against bfdb, and requires bfdb to have been started with `--base` pointing at the mission's netidx path so it can reach bflib's RPCs.
- **Server/process control** (`fe_upload_bflib`, and the `bfdb_supervisor` background task): neither goes through bfdb at all. `fe_upload_bflib` only accepts servers that are already `SHUTDOWN`/`STOPPED` (DCS.exe holds a file lock on bflib.dll while running), overwrites it directly, and calls `server.startup()` on DCSServerBot's own `Server` object to bring the server back up. The bfdb supervisor shells out to `bfsystem_script` (`bfsystem.ps1`) in its own console when bfdb's `/api/stats` stops responding. Both run with whatever OS privileges the bot process itself has, on the machine it's running on -- `fe_upload_bflib` is gated to `DCS Admin`.

`lua/callbacks.lua` and `lua/commands.lua` are legacy from an earlier design (a direct Lua-hooks bridge) and are not used by any of the above -- they're unwired stubs kept only in case a lower-latency native bridge is built later.
