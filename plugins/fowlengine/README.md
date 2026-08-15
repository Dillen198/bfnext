# Vector Strike DCSServerBot Plugin

The Vector Strike plugin for DCSServerBot bridges your DCS Vector Strike campaign to Discord. It features a continuously updating live campaign status, objective capture alerts, killstreak achievements, and interactive Discord slash commands for player stats and commander logistics.

## Features

- **Live Campaign Status Embed:** A single, continuously updating Discord embed showing live points, objective counts, and player counts per faction.
- **Live Engine Log Relay:** Tails bfdb's `/ws/engine-logs` websocket (the raw `bflib` engine log) into a Discord channel — one message is continuously edited with a rolling tail, and `[ERROR]`/`[WARN]` lines are additionally posted as standalone alerts so they don't get missed.
- **Capture/Neutral/Ready-to-Capture Alerts:** Polls bfdb's public `/api/objectives` every ~20s and diffs owner/health against the previous poll to detect captures, objectives going neutral, and objectives dropping to capturable health.
- **Killstreak Achievements:** Polls bfdb's public `/api/kills` every ~20s to track each pilot's consecutive kills (reset on death) and announces streaks of 5 (Ace), 10 (Unstoppable), and 15 (God of War).
- **Dual-Login Dashboard:** Supports both standard Discord OAuth web-login and securely generated HMAC bot-tokens to seamlessly bridge the `bfweb` dashboard.
- **Interactive Commander Terminal:** A slick UI terminal allowing commanders to drop crates and infantry squads at airbases directly from Discord.

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

  # Required if engine_log_channel is set -- must match bfdb's own
  # --admin-username/--admin-password startup flags.
  admin_username: "admin"
  admin_password: "YOUR_BFDB_ADMIN_PASSWORD"
```

## Slash Commands

### Player Commands
- `/vs dashboard` - Retrieves your secure Vector Strike web dashboard login link.
- `/vs join [Red|Blue|Neutral]` - Pre-selects your faction before slotting into a DCS aircraft.
- `/vs stats [@user]` - Shows pilot kills, highest streak, and points available.

### Commander & Admin Commands
- `/vs terminal` - Deploys the interactive Commander Terminal (UI buttons and dropdowns) allowing you to deploy logistics from Discord.
- `/vs spawn_deployable [type] [airbase]` - Direct command to spawn cargo/infantry at a specific airbase.
- `/vs priority [objective]` - Marks an objective as a high priority target for your team.

## Architecture & Integration
This plugin talks to bfdb, not directly to the DCS process. Two integration paths:

- **Read-only data** (`/status`, `fe_objectives`, `fe_stats`, capture/achievement polling, the engine log relay's history replay): plain HTTP/WebSocket calls to bfdb, no DCS-side setup needed beyond running bfdb itself.
- **Commander actions** (`fe_priority`, `fe_spawn_deployable`, `fe_terminal`): bfdb calls into bflib's live netidx RPC server (`bflib/src/bg/rpcs.rs`) -- e.g. `spawn-deployable`, `set-objective-priority` -- which requires `admin_username`/`admin_password` to authenticate against bfdb, and requires bfdb to have been started with `--base` pointing at the mission's netidx path so it can reach bflib's RPCs.

`lua/callbacks.lua` and `lua/commands.lua` are legacy from an earlier design (a direct Lua-hooks bridge) and are not used by any of the above -- they're unwired stubs kept only in case a lower-latency native bridge is built later.
