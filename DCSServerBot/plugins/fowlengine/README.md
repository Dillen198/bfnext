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
- **Interactive Commander Terminal:** A UI terminal (`/fe_terminal`) to drop crates/infantry at airbases **and** set objective priority, directly from Discord.
- **bfdb + netidx Process Ownership:** With `bfdb.manage: true` the plugin runs `bfdb.exe` and the netidx resolver as child processes of the bot: renders `gci.json` from YAML, builds bfdb's arg list, health-checks it, and relaunches on crash/hang. Replaces `bfsystem.ps1`. Combined with running the bot as a Windows service (`deploy/windows-service/`), a reboot brings the whole stack back with no RDP.
- **GCI from YAML:** the `gci:` block in `fowlengine.yaml` is rendered to `<bfdb.home>\gci.json` on every bfdb start (bfdb reads it only at startup). `/feops gci_show` prints the effective config with secrets masked; `/feops bfdb_restart` reloads it.
- **GCI transcript relay:** set `gci_transcript_channel` and the bot tails bfdb's `/ws/gci`, posting every AWACS call to that channel prefixed 🔵/🔴 (replaces the raw `discord_webhook_url`).
- **Consolidated server-info embed:** set `server_info_channel` for one auto-updating embed with the connect address/port/password, DCS + mission status, next rotation, GCI frequencies and deploy status. `/fe_gci` gives any player the current GCI freqs on demand.
- **Staged engine updates:** drag `bflib.dll` or `bfdb.exe` into the bot's admin channel -> staged, then swapped in (after a timestamped backup) on the next scheduled DCS restart. `bflib.dll` via the `BFBinaries` extension, `bfdb.exe` via this plugin. `/feops stage_status | stage_cancel | stage_apply` to control it. No manual server shutdown needed to stage.
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

  # Ops notices (bfdb relaunched, staged binary applied). Falls back to
  # alerts_channel if unset.
  ops_channel: 123456789012345678
```

See `fowlengine.sample.yaml` for the full `bfdb:` (process management) and
`gci:` (Live GCI) blocks. The `BFBinaries` extension (staged `bflib.dll`
swap) is configured in `nodes.yaml`, not here.

## Slash Commands

### Player
- `/fe_dashboard` - your secure web-dashboard login link (Discord OAuth + 1-hour HMAC auto-login).
- `/fe_objective <name>` - owner / health / priority for one objective (substring match).
- `/fe_gci` - current GCI (AWACS) frequencies, callsigns and usage.

Live stats, the leaderboard, who's online and the full objective list all
live on the web dashboard now -- `/fe_dashboard` points there.

### Commander & Admin (`DCS Admin`)
- `/fe_terminal` - interactive Commander Terminal: drop cargo/infantry at an airbase, and set/clear objective priority.
- `/fe_ban <ucid> <name> [reason] [until]` / `/fe_unban <ucid>` - campaign ban management.
- `/feops bfdb_restart` - restart bfdb (re-renders `gci.json`, picks up a staged `bfdb.exe`).
- `/feops gci_show` - print the effective `gci.json` (secrets masked).
- `/feops stage_status | stage_cancel <which> | stage_apply <server> <which>` - manage staged engine binaries.
- **Upload:** drop `bflib.dll` / `bfdb.exe` into the admin channel (DCS Admin only) to stage it.

## Architecture & Integration

- **Read-only data** (status/welcome embeds, capture & achievement polling, engine-log relay): plain HTTP/WebSocket to bfdb.
- **Commander actions** (`/fe_terminal`): bfdb -> bflib netidx RPCs (`bflib/src/bg/rpcs.rs`); needs `admin_username`/`admin_password` and bfdb started with `--base`.
- **Process ownership** (`bfdb.manage`, `procman.py`): the bot runs `bfdb.exe` + the netidx resolver as children, health-checks bfdb, renders `gci.json` from YAML.
- **Restart-cycle binary swap** (`extensions/bfbinaries`): `prepare()` swaps a staged `bflib.dll` while DCS is down; a staged `bfdb.exe` is applied by procman on its next restart. Both back up the previous binary and never block a restart.

- **Multiple DCS servers on one machine:** one bfdb fronts them all. Add a
  `bfdb.instances:` list to `fowlengine.yaml` and procman renders bfdb's
  `instances.json` (plus one `gci.<id>.json` per server) and starts it with
  `--instances`. Every request the plugin makes carries
  `?server=<DCS server name>`, which bfdb maps to an instance via that
  instance's `dcs_server_name` -- so each server's status embed, alerts,
  achievements, engine-log relay, GCI transcript, perf embed and commander
  terminal are about that server only. Channel ids can be split per server
  using DCSServerBot's normal per-server config sections. See
  [`deploy/multi-instance.md`](../../deploy/multi-instance.md).

`lua/callbacks.lua` / `lua/commands.lua` are unwired legacy stubs.
