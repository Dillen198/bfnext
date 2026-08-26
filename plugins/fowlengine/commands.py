import discord
from core import Plugin, utils, Server, Status, command
from discord import app_commands
from discord.ext import tasks, commands
from services.bot import DCSServerBot
from datetime import datetime, timezone
import json
import os
import re
import asyncio
import shutil
import subprocess
import time
from collections import deque

# NOTE: this plugin previously subclassed Plugin[FowlEngineEventListener] and
# registered .listener.FowlEngineEventListener for the vs_event/registerDCSServer
# bridge. That bridge has no producer yet (lua/callbacks.lua never actually calls
# sendBotTable), so it's untested and was the one piece structurally different
# from this bot's other working plugins (radio, smartmod), which both use plain
# Plugin with no eventlistener. Dropped for now to isolate a plugin-load failure;
# listener.py is still there to wire back in once the Lua side sends real events.

# ── Live engine log relay ───────────────────────────────────────────────────
# Tails bfdb's /ws/engine-logs websocket (the raw bflib engine log, distinct
# from the curated vs_event alerts above) into a Discord channel: a single
# message gets edited with a rolling tail, and ERROR/WARN lines additionally
# get posted as standalone alerts so they don't scroll off unnoticed.
ENGINE_LOG_TAIL_LINES = 40
ENGINE_LOG_FLUSH_SECS = 3.0
ENGINE_LOG_RECONNECT_SECS = 10
ENGINE_LOG_ALERT_DEDUPE = 500
ENGINE_LOG_LEVEL_RE = re.compile(r"\[(ERROR|WARN|WARNING)\]", re.IGNORECASE)

# ── Live capture/achievement alerts ─────────────────────────────────────────
# vs_event (listener.py) has no producer on the Lua side, so this polls bfdb's
# public /api/objectives and /api/kills instead: no bflib/Lua changes needed,
# and both endpoints are unauthenticated (no admin_username/password required).
CAMPAIGN_POLL_SECS = 20
CAMPAIGN_RECONNECT_SECS = 15
ACHIEVEMENT_THRESHOLDS = [(15, "God of War"), (10, "Unstoppable"), (5, "Ace")]

# ── bfdb process supervision ────────────────────────────────────────────────
# bfdb.exe isn't managed by DCSServerBot's own Server object (that only
# controls the DCS process) -- it's a plain sidecar process on the same
# machine, so this plugin health-checks it independently and relaunches it
# via bfsystem.ps1 when it stops responding, the same way an admin would by
# hand today.
BFDB_HEALTH_CHECK_SECS = 30
BFDB_DEFAULT_HEALTH_FAILURES = 3   # consecutive failed checks before relaunching
BFDB_DEFAULT_RESTART_COOLDOWN = 120  # min seconds between relaunch attempts

# ── bfdb admin auth ──────────────────────────────────────────────────────────
# bfdb's admin-gated endpoints (e.g. /api/commander/spawn, /ws/engine-logs) only
# ever check the "session" cookie set by POST /api/auth/local-login -- they do
# not accept an Authorization header. Module-level (not on FowlEngine) so
# CommanderTerminalView, which isn't a Plugin, can use it too.


async def bfdb_login(http, api_url: str, username: str, password: str) -> str:
    """POST /api/auth/local-login and return the resulting session cookie value."""
    async with http.post(
        f"{api_url}/api/auth/local-login",
        json={"username": username, "password": password},
        timeout=8,
    ) as resp:
        if resp.status != 200:
            raise RuntimeError(f"bfdb local-login failed ({resp.status}): {await resp.text()}")
        set_cookie = resp.headers.get("set-cookie", "")
        m = re.search(r"session=([0-9a-fA-F-]+)", set_cookie)
        if not m:
            raise RuntimeError("bfdb local-login succeeded but returned no session cookie")
        return m.group(1)


async def bfdb_admin_post(api_url: str, username: str, password: str, path: str, json_body: dict):
    """POST to an admin-gated bfdb endpoint via the local-login session cookie flow.
    Returns (status, parsed_json_or_None)."""
    import aiohttp
    async with aiohttp.ClientSession() as http:
        session_cookie = await bfdb_login(http, api_url, username, password)
        async with http.post(
            f"{api_url}{path}", json=json_body, headers={"Cookie": f"session={session_cookie}"}, timeout=10
        ) as resp:
            status = resp.status
            try:
                data = await resp.json()
            except Exception:
                data = None
            return status, data


async def bfdb_admin_get(api_url: str, username: str, password: str, path: str):
    """GET an admin-gated bfdb endpoint via the local-login session cookie flow.
    Returns (status, parsed_json_or_None)."""
    import aiohttp
    async with aiohttp.ClientSession() as http:
        session_cookie = await bfdb_login(http, api_url, username, password)
        async with http.get(
            f"{api_url}{path}", headers={"Cookie": f"session={session_cookie}"}, timeout=10
        ) as resp:
            status = resp.status
            try:
                data = await resp.json()
            except Exception:
                data = None
            return status, data

class CommanderTerminalView(discord.ui.View):
    def __init__(self, api_url: str, admin_username: str, admin_password: str, airbases: list, dynamic_types: list):
        super().__init__(timeout=None)
        self.api_url = api_url
        self.admin_username = admin_username
        self.admin_password = admin_password
        self.selected_airbase = None
        self.selected_type = None

        options = []
        for ab in airbases[:25]:
            options.append(discord.SelectOption(label=ab['name'], description=f"Owner: {ab['owner']}"))
        if not options:
            options.append(discord.SelectOption(label="No Airbases Found", value="none"))
            
        self.airbase_select = discord.ui.Select(placeholder="Step 1: Select Airbase/FARP...", options=options, custom_id="ab_select")
        self.airbase_select.callback = self.ab_callback
        
        type_options = []
        for t_label, t_desc in dynamic_types[:25]:
            type_options.append(discord.SelectOption(label=t_label, description=t_desc))
            
        if not type_options:
            type_options.append(discord.SelectOption(label="No Deployables found in CFG", value="none"))
            
        self.type_select = discord.ui.Select(placeholder="Step 2: Select Deployable...", options=type_options, custom_id="type_select")
        self.type_select.callback = self.type_callback

        self.add_item(self.airbase_select)
        self.add_item(self.type_select)

    async def ab_callback(self, interaction: discord.Interaction):
        self.selected_airbase = self.airbase_select.values[0]
        await interaction.response.send_message(f"Base selected: {self.selected_airbase}", ephemeral=True)

    async def type_callback(self, interaction: discord.Interaction):
        self.selected_type = self.type_select.values[0]
        await interaction.response.send_message(f"Deployable selected: {self.selected_type}", ephemeral=True)

    @discord.ui.button(label="EXECUTE SPAWN", style=discord.ButtonStyle.success, row=2)
    async def spawn_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        if not self.selected_airbase or self.selected_airbase == "none":
            await interaction.response.send_message("Please select a valid Airbase first.", ephemeral=True)
            return
        if not self.selected_type:
            await interaction.response.send_message("Please select a Deployable first.", ephemeral=True)
            return
            
        await interaction.response.defer(ephemeral=True)
        if not self.admin_username or not self.admin_password:
            await interaction.followup.send(
                "❌ admin_username/admin_password are not configured for this server "
                "(required to authenticate against bfdb's commander API)."
            )
            return
        try:
            status, _data = await bfdb_admin_post(
                self.api_url, self.admin_username, self.admin_password,
                "/api/commander/spawn", {"airbase": self.selected_airbase, "type": self.selected_type},
            )
        except Exception as ex:
            await interaction.followup.send(f"❌ Failed to spawn: {ex}")
            return
        if status == 200:
            await interaction.followup.send(f"✅ Successfully ordered {self.selected_type} at {self.selected_airbase}.")
        else:
            await interaction.followup.send(f"❌ Failed to spawn: HTTP {status}")

class FowlEngine(Plugin):
    """
    Fowl Engine plugin for DCSServerBot.
    Provides live status and objective events.
    """

    def __init__(self, bot: DCSServerBot):
        super().__init__(bot)
        self.status_msg_id = None
        self.perf_msg_id = None
        self.tail_msg_ids = {}  # server name -> engine log tail message id
        self.faction_thread_ids = {}  # server name -> {"Blue": thread_id, "Red": thread_id}
        self.state_file = os.path.join(bot.node.config_dir, 'fowlengine_state.json')
        if os.path.exists(self.state_file):
            try:
                with open(self.state_file, 'r') as f:
                    state = json.load(f)
                    self.status_msg_id = state.get('status_msg_id')
                    self.perf_msg_id = state.get('perf_msg_id')
                    self.tail_msg_ids = state.get('tail_msg_ids', {})
                    self.faction_thread_ids = state.get('faction_thread_ids', {})
            except Exception as ex:
                self.log.error(f"Failed to load Fowl Engine state: {ex}")
        # Per-server live state for the engine log relay (not persisted -- rebuilt on connect).
        self._log_relay_tasks = {}   # server name -> asyncio.Task
        self._log_tail_buffers = {}  # server name -> deque[str]
        self._log_seen_alerts = {}   # server name -> deque[str]
        self._log_seen_alert_set = {}  # server name -> set[str]
        # Per-server live state for capture/achievement polling (not persisted).
        self._campaign_poll_tasks = {}  # server name -> asyncio.Task
        self._obj_state = {}            # server name -> {obj_name: {"owner", "health"}}
        self._kill_cursor = {}          # server name -> last-processed kill ISO timestamp
        self._kill_streaks = {}         # server name -> {ucid: consecutive kill count}
        self._kill_announced = {}       # server name -> {ucid: highest threshold already announced}
        self._capture_cursor = {}       # server name -> last-processed capture-event ISO timestamp
        self._active_round = {}         # server name -> last-seen active round id
        # Per-server bfdb health-check state (not persisted -- rebuilt on connect).
        self._bfdb_fail_count = {}      # server name -> consecutive failed health checks
        self._bfdb_last_restart = {}    # server name -> monotonic time.time() of last relaunch attempt

    async def cog_load(self) -> None:
        await super().cog_load()
        utils.safe_start(self.update_status)
        utils.safe_start(self.sync_ranks)
        utils.safe_start(self.supervise_engine_logs)
        utils.safe_start(self.supervise_campaign_events)
        utils.safe_start(self.update_perf_status)
        utils.safe_start(self.supervise_bfdb)

    async def cog_unload(self):
        await utils.safe_cancel(self.update_status)
        await utils.safe_cancel(self.sync_ranks)
        await utils.safe_cancel(self.supervise_engine_logs)
        await utils.safe_cancel(self.supervise_campaign_events)
        await utils.safe_cancel(self.update_perf_status)
        await utils.safe_cancel(self.supervise_bfdb)
        for task in self._log_relay_tasks.values():
            task.cancel()
        for task in self._campaign_poll_tasks.values():
            task.cancel()
        await super().cog_unload()

    def save_state(self):
        try:
            with open(self.state_file, 'w') as f:
                json.dump({
                    'status_msg_id': self.status_msg_id,
                    'perf_msg_id': self.perf_msg_id,
                    'tail_msg_ids': self.tail_msg_ids,
                    'faction_thread_ids': self.faction_thread_ids,
                }, f)
        except Exception as ex:
            self.log.error(f"Failed to save Fowl Engine state: {ex}")

    @tasks.loop(minutes=1.0)
    async def update_status(self):
        # We need to find the active server running Fowl Engine. 
        # For simplicity, we assume the first active server or a specific one.
        for server in self.bot.servers.values():
            if server.status not in [Status.RUNNING, Status.PAUSED]:
                continue
                
            config = self.get_config(server)
            if not config or 'status_channel' not in config:
                continue
                
            try:
                import aiohttp
                config = self.get_config(server) or {}
                api_url = config.get("api_url", "http://localhost:8765")
                async with aiohttp.ClientSession() as session:
                    async with session.get(f"{api_url}/api/stats") as resp:
                        if resp.status != 200:
                            continue
                        stats = await resp.json()
                        
                    async with session.get(f"{api_url}/api/objectives") as resp:
                        if resp.status == 200:
                            objs = await resp.json()
                        else:
                            objs = []

                    async with session.get(f"{api_url}/api/leaderboard") as resp:
                        leaderboard = await resp.json() if resp.status == 200 else []

                    async with session.get(f"{api_url}/api/captures") as resp:
                        captures = await resp.json() if resp.status == 200 else []

                blue_objs = len([o for o in objs if o.get('owner') == 'Blue'])
                red_objs = len([o for o in objs if o.get('owner') == 'Red'])
                neutral_objs = len([o for o in objs if o.get('owner') == 'Neutral'])
                total_objs = len(objs)

                # Dynamic accent color reflects who's currently winning the territorial fight
                if blue_objs > red_objs:
                    embed_color = discord.Color.blue()
                elif red_objs > blue_objs:
                    embed_color = discord.Color.red()
                else:
                    embed_color = discord.Color.gold()

                brand_name = config.get('brand_name', 'Fowl Engine')
                embed = discord.Embed(title=f"⚔️ {brand_name} Campaign Status", color=embed_color)

                active_round = stats.get('active_round')
                if active_round:
                    desc = f"**Active Scenario:** {active_round.get('scenario', 'Unknown')}"
                    start_raw = active_round.get('start')
                    if start_raw:
                        try:
                            started = self._parse_iso(start_raw)
                            elapsed = datetime.now(timezone.utc) - started
                            days, rem = divmod(int(elapsed.total_seconds()), 86400)
                            hours, rem = divmod(rem, 3600)
                            minutes, _ = divmod(rem, 60)
                            elapsed_str = f"{days}d {hours}h {minutes}m" if days else f"{hours}h {minutes}m"
                            desc += f"\n**Round Duration:** {elapsed_str}"
                        except ValueError:
                            pass
                    embed.description = desc
                else:
                    embed.description = "**Server Offline or No Active Round**"

                blue_online = stats.get('blue_online', 0)
                red_online = stats.get('red_online', 0)
                embed.add_field(
                    name="🟦 Blue Faction",
                    value=f"Pilots Online: **{blue_online}**\nObjectives: **{blue_objs}**",
                    inline=True,
                )
                embed.add_field(
                    name="🟥 Red Faction",
                    value=f"Pilots Online: **{red_online}**\nObjectives: **{red_objs}**",
                    inline=True,
                )
                embed.add_field(
                    name="⬜ Neutral",
                    value=f"Objectives: **{neutral_objs}**",
                    inline=True,
                )

                # Text-based territory control bar, mirroring the web dashboard
                if total_objs > 0:
                    bar_len = 20
                    blue_blocks = round((blue_objs / total_objs) * bar_len)
                    red_blocks = round((red_objs / total_objs) * bar_len)
                    neutral_blocks = max(0, bar_len - blue_blocks - red_blocks)
                    bar = "🟦" * blue_blocks + "⬜" * neutral_blocks + "🟥" * red_blocks
                    blue_pct = round((blue_objs / total_objs) * 100)
                    red_pct = round((red_objs / total_objs) * 100)
                    embed.add_field(
                        name="📊 Territory Control",
                        value=f"{bar}\n{blue_pct}% Blue · {100 - blue_pct - red_pct}% Neutral · {red_pct}% Red",
                        inline=False,
                    )

                embed.add_field(
                    name="📈 Campaign Stats",
                    value=(
                        f"Total Kills: **{stats.get('total_kills', 0)}**\n"
                        f"Pilots Registered: **{stats.get('total_pilots', 0)}** "
                        f"({stats.get('blue_registered', 0)} blue · {stats.get('red_registered', 0)} red)\n"
                        f"Total Objectives: **{total_objs}**"
                    ),
                    inline=True,
                )

                if leaderboard:
                    top = max(leaderboard, key=lambda p: p.get('air_kills', 0) + p.get('ground_kills', 0))
                    kills = top.get('air_kills', 0) + top.get('ground_kills', 0)
                    deaths = top.get('deaths', 0)
                    kd = f"{kills / deaths:.2f}" if deaths > 0 else ("∞" if kills > 0 else "0.00")
                    embed.add_field(
                        name="🏆 Top Ace",
                        value=f"**{top.get('name', 'Unknown')}**\n{kills} kills · {kd} K/D",
                        inline=True,
                    )

                if captures:
                    top_obj = max(captures, key=lambda c: c.get('count', 0))
                    embed.add_field(
                        name="🎯 Most Contested",
                        value=f"**{top_obj.get('name', 'Unknown')}**\nCaptured {top_obj.get('count', 0)}x this round",
                        inline=True,
                    )

                ready_objs = [o for o in objs if o.get('health', 100) <= 20 and o.get('owner') in ('Blue', 'Red')]
                if ready_objs:
                    names = ", ".join(f"{o['name']} ({o['owner']})" for o in ready_objs[:5])
                    if len(ready_objs) > 5:
                        names += f" +{len(ready_objs) - 5} more"
                    embed.add_field(name="⏳ Ready to Capture", value=names, inline=False)

                priority_objs = [o.get('name') for o in objs if o.get('priority')]
                if priority_objs:
                    embed.add_field(name="⭐ Commander Priority", value=", ".join(priority_objs[:5]), inline=False)

                footer_parts = []
                restart_at = stats.get('restart_at')
                if restart_at:
                    try:
                        restart_ts = int(self._parse_iso(restart_at).timestamp())
                        embed.add_field(name="🔄 Next Rotation", value=f"<t:{restart_ts}:R> (<t:{restart_ts}:f>)", inline=False)
                    except ValueError:
                        footer_parts.append(f"Rotation scheduled for {restart_at}")
                embed.timestamp = discord.utils.utcnow()
                footer_parts.append("Updated")
                embed.set_footer(text=" | ".join(footer_parts))
                channel_id = int(config['status_channel'])
                channel = self.bot.get_channel(channel_id)
                if not channel:
                    self.log.error(f"FowlEngine: status_channel {channel_id} not found or bot lacks access.")
                    continue
                    
                if self.status_msg_id:
                    try:
                        msg = await channel.fetch_message(self.status_msg_id)
                        await msg.edit(embed=embed)
                        continue
                    except discord.NotFound:
                        self.status_msg_id = None
                    except discord.Forbidden:
                        self.log.error(f"FowlEngine: Bot lacks permissions to read/edit in channel {channel_id}")
                        self.status_msg_id = None
                        
                msg = await channel.send(embed=embed)
                self.status_msg_id = msg.id
                self.save_state()
                
            except Exception as ex:
                import traceback
                self.log.error(f"Error updating Fowl Engine status: {ex}\n{traceback.format_exc()}")

    @update_status.before_loop
    async def before_update_status(self):
        await self.bot.wait_until_ready()

    @staticmethod
    def _parse_iso(raw: str):
        """Parses bfdb's ISO-8601 timestamps, which may carry nanosecond
        fractional seconds (9 digits) -- datetime.fromisoformat only accepts
        up to 6 (microseconds), so truncate before parsing."""
        raw = raw.replace('Z', '+00:00')
        m = re.match(r'^(.*?\.\d{6})\d*(\+.*)?$', raw)
        if m:
            raw = m.group(1) + (m.group(2) or '')
        return datetime.fromisoformat(raw)

    @staticmethod
    def _fmt_bytes(n: int) -> str:
        gb = n / (1024 ** 3)
        return f"{gb:.1f} GB"

    @staticmethod
    def _bar(pct: float, width: int = 10) -> str:
        pct = max(0.0, min(100.0, pct))
        filled = round((pct / 100.0) * width)
        return "█" * filled + "░" * (width - filled)

    def _build_perf_embed(self, brand_name: str, data: dict) -> discord.Embed:
        hw = data.get('hardware') or {}

        cpu_pct = hw.get('cpu_usage_pct', 0.0)
        mem_total = hw.get('mem_total_bytes', 0)
        mem_used = hw.get('mem_used_bytes', 0)
        mem_pct = (mem_used / mem_total * 100) if mem_total else 0.0
        gpu = hw.get('gpu') or {}

        worst_pct = max(cpu_pct, mem_pct, gpu.get('usage_pct', 0) if gpu.get('available') else 0)
        for d in hw.get('disks', []):
            if d.get('total_bytes'):
                worst_pct = max(worst_pct, d['used_bytes'] / d['total_bytes'] * 100)
        color = discord.Color.red() if worst_pct >= 90 else (
            discord.Color.orange() if worst_pct >= 75 else discord.Color.green()
        )

        embed = discord.Embed(title=f"🖥️ {brand_name} Server Performance", color=color)

        embed.add_field(
            name="⚙️ CPU",
            value=f"{self._bar(cpu_pct)} {cpu_pct:.0f}%\n{hw.get('cpu_count', '?')} logical cores",
            inline=True,
        )
        embed.add_field(
            name="🧠 RAM",
            value=f"{self._bar(mem_pct)} {mem_pct:.0f}%\n{self._fmt_bytes(mem_used)} / {self._fmt_bytes(mem_total)}",
            inline=True,
        )
        if gpu.get('available'):
            gpu_pct = gpu.get('usage_pct', 0)
            gpu_mem_total = gpu.get('mem_total_bytes', 0)
            gpu_mem_used = gpu.get('mem_used_bytes', 0)
            gpu_temp = gpu.get('celsius')
            temp_str = f" · 🌡️ {gpu_temp}°C" if gpu_temp is not None else ""
            embed.add_field(
                name=f"🎮 GPU ({gpu.get('name', 'Unknown')})",
                value=(
                    f"{self._bar(gpu_pct)} {gpu_pct:.0f}%{temp_str}\n"
                    f"VRAM: {self._fmt_bytes(gpu_mem_used)} / {self._fmt_bytes(gpu_mem_total)}"
                ),
                inline=True,
            )

        disks = hw.get('disks', [])
        if disks:
            lines = []
            for d in disks[:5]:
                total = d.get('total_bytes', 0)
                used = d.get('used_bytes', 0)
                pct = (used / total * 100) if total else 0.0
                lines.append(f"`{d.get('mount', '?')}` {self._fmt_bytes(used)} / {self._fmt_bytes(total)} ({pct:.0f}%)")
            embed.add_field(name="💾 Disk", value="\n".join(lines), inline=False)

        temps = hw.get('temps', [])
        cpu_temps = [t for t in temps if 'cpu' in t.get('label', '').lower() or 'package' in t.get('label', '').lower()]
        if cpu_temps:
            embed.add_field(
                name="🌡️ CPU Temp",
                value="\n".join(f"{t['label']}: {t['celsius']:.0f}°C" for t in cpu_temps[:3]),
                inline=True,
            )

        if data.get('available'):
            engine_by_name = {row['name']: row for row in data.get('engine', [])}
            frame = engine_by_name.get('frame')
            if frame:
                mean_us = frame.get('mean', 0)
                flag = "🔴 " if mean_us >= 25000 else ("🟡 " if mean_us >= 20000 else "")
                embed.add_field(
                    name=f"{flag}📊 Mission Frame Time",
                    value=(
                        f"mean **{mean_us:.0f}{frame.get('unit', 'us')}** · "
                        f"p50 {frame.get('p50', 0):.0f} · p90 {frame.get('p90', 0):.0f} · "
                        f"p99 {frame.get('p99', 0):.0f}"
                    ),
                    inline=False,
                )
            watch = ['unit_culling', 'logistics', 'spawn', 'process_messages', 'dcs_events']
            watch_lines = []
            for name in watch:
                row = engine_by_name.get(name)
                if row:
                    watch_lines.append(f"`{name}`: {row.get('mean', 0):.0f}{row.get('unit', 'us')}")
            if watch_lines:
                embed.add_field(name="🔍 Script Breakdown (mean)", value="\n".join(watch_lines), inline=False)
        else:
            embed.add_field(name="📊 Mission Status", value="No active DCS session reporting yet.", inline=False)

        embed.set_footer(text=f"Updated {datetime.now(timezone.utc).strftime('%H:%M:%S UTC')} — thresholds are approximate, use as a starting point")
        return embed

    @tasks.loop(minutes=5.0)
    async def update_perf_status(self):
        for server in self.bot.servers.values():
            if server.status not in [Status.RUNNING, Status.PAUSED]:
                continue

            config = self.get_config(server) or {}
            if not config.get('perf_channel'):
                continue

            api_url = config.get("api_url", "http://localhost:8765")
            username = config.get("admin_username")
            password = config.get("admin_password")
            if not username or not password:
                self.log.error(
                    "FowlEngine: perf_channel is set but admin_username/admin_password "
                    "are missing (must match bfdb's --admin-username/--admin-password)"
                )
                continue

            try:
                status, data = await bfdb_admin_get(api_url, username, password, "/api/admin/perf")
                if status != 200 or data is None:
                    self.log.error(f"FowlEngine: /api/admin/perf returned {status}")
                    continue

                brand_name = config.get('brand_name', 'Fowl Engine')
                embed = self._build_perf_embed(brand_name, data)

                channel_id = int(config['perf_channel'])
                channel = self.bot.get_channel(channel_id)
                if not channel:
                    self.log.error(f"FowlEngine: perf_channel {channel_id} not found or bot lacks access.")
                    continue

                if self.perf_msg_id:
                    try:
                        msg = await channel.fetch_message(self.perf_msg_id)
                        await msg.edit(embed=embed)
                        continue
                    except discord.NotFound:
                        self.perf_msg_id = None
                    except discord.Forbidden:
                        self.log.error(f"FowlEngine: Bot lacks permissions to read/edit in channel {channel_id}")
                        self.perf_msg_id = None

                msg = await channel.send(embed=embed)
                self.perf_msg_id = msg.id
                self.save_state()

            except Exception as ex:
                import traceback
                self.log.error(f"Error updating Fowl Engine perf status: {ex}\n{traceback.format_exc()}")

    @update_perf_status.before_loop
    async def before_update_perf_status(self):
        await self.bot.wait_until_ready()

    # ── bfdb process supervision ─────────────────────────────────────────────

    @tasks.loop(seconds=BFDB_HEALTH_CHECK_SECS)
    async def supervise_bfdb(self):
        """Health-checks bfdb.exe (independent of DCS server status -- bfdb
        should be up serving the dashboard even between missions) and
        relaunches it via bfsystem_script when it stops answering."""
        for server in self.bot.servers.values():
            config = self.get_config(server) or {}
            if not config.get('bfdb_supervisor'):
                continue
            script = config.get('bfsystem_script')
            if not script:
                self.log.error(f"FowlEngine: bfdb_supervisor is enabled for {server.name} but bfsystem_script is not set")
                continue

            api_url = config.get("api_url", "http://localhost:8765")
            healthy = await self._bfdb_health_check(api_url)

            if healthy:
                self._bfdb_fail_count[server.name] = 0
                continue

            fail_count = self._bfdb_fail_count.get(server.name, 0) + 1
            self._bfdb_fail_count[server.name] = fail_count
            threshold = int(config.get('bfdb_health_failures', BFDB_DEFAULT_HEALTH_FAILURES))
            if fail_count < threshold:
                self.log.warning(f"FowlEngine: bfdb health check failed for {server.name} ({fail_count}/{threshold})")
                continue

            cooldown = int(config.get('bfdb_restart_cooldown', BFDB_DEFAULT_RESTART_COOLDOWN))
            last_restart = self._bfdb_last_restart.get(server.name, 0)
            if time.time() - last_restart < cooldown:
                continue  # already tried recently -- give it time to come up before trying again

            self._bfdb_fail_count[server.name] = 0
            self._bfdb_last_restart[server.name] = time.time()
            self.log.error(f"FowlEngine: bfdb unreachable at {api_url} after {fail_count} checks -- relaunching via {script}")
            ok, err = self._launch_bfsystem(script)
            await self._notify_ops(
                config,
                f"⚠️ **bfdb was unreachable at `{api_url}`** after {fail_count} failed health checks.\n"
                + (f"Relaunched via `{script}`." if ok else f"❌ Failed to relaunch: {err}")
            )

    @supervise_bfdb.before_loop
    async def before_supervise_bfdb(self):
        await self.bot.wait_until_ready()

    @staticmethod
    async def _bfdb_health_check(api_url: str) -> bool:
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/stats", timeout=8) as resp:
                    return resp.status == 200
        except Exception:
            return False

    @staticmethod
    def _launch_bfsystem(script: str) -> tuple:
        """Launches bfsystem.ps1 in its own console, independent of this bot
        process. bfsystem.ps1 isn't a plain one-shot launcher -- it starts
        bfdb as a background job and then runs its own interactive status
        loop (Console.ReadKey() to catch 'Q' for shutdown), so it needs a
        real console attached; CREATE_NEW_CONSOLE gives it one without tying
        its lifetime to the bot's own console/process."""
        try:
            subprocess.Popen(
                ["powershell.exe", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", script],
                creationflags=subprocess.CREATE_NEW_CONSOLE,
                close_fds=True,
            )
            return True, None
        except Exception as ex:
            return False, str(ex)

    async def _notify_ops(self, config: dict, message: str):
        """Best-effort notice to ops_channel (falling back to alerts_channel)
        about bfdb supervision events. Silent if neither is configured."""
        channel_id = config.get('ops_channel') or config.get('alerts_channel')
        if not channel_id:
            return
        channel = self.bot.get_channel(int(channel_id))
        if not channel:
            return
        try:
            await channel.send(message)
        except discord.HTTPException as ex:
            self.log.error(f"FowlEngine: failed to send ops notice: {ex}")

    @tasks.loop(minutes=5.0)
    async def sync_ranks(self):
        for server in self.bot.servers.values():
            if server.status not in [Status.RUNNING, Status.PAUSED]:
                continue

            config = self.get_config(server)
            if not config or 'rank_thresholds' not in config:
                continue

            rank_thresholds = config['rank_thresholds']
            if not rank_thresholds:
                continue

            # Sort thresholds descending by points
            sorted_ranks = sorted(rank_thresholds.items(), key=lambda x: x[1], reverse=True)

            api_url = config.get("api_url", "http://localhost:8765")
            try:
                import aiohttp
                async with aiohttp.ClientSession() as session:
                    async with session.get(f"{api_url}/api/pilots") as resp:
                        if resp.status != 200:
                            continue
                        pilots = await resp.json()

                for p in pilots:
                    ucid = p.get('ucid')
                    if not ucid:
                        continue

                    total_air = p.get('air_kills', 0)
                    total_gnd = p.get('ground_kills', 0)
                    total_caps = p.get('captures', 0)

                    points = (total_air * 10) + (total_gnd * 2) + (total_caps * 50)

                    target_role_name = None
                    for r_name, r_thresh in sorted_ranks:
                        if points >= r_thresh:
                            target_role_name = r_name
                            break

                    if not target_role_name:
                        continue

                    member = await self.bot.get_member_by_ucid(ucid)
                    if not member:
                        continue

                    # Find role and assign
                    guild = member.guild
                    target_role = discord.utils.get(guild.roles, name=target_role_name)
                    if not target_role:
                        continue

                    if target_role not in member.roles:
                        # Optionally remove other rank roles
                        roles_to_remove = [r for r in member.roles if r.name in rank_thresholds and r.name != target_role_name]
                        if roles_to_remove:
                            await member.remove_roles(*roles_to_remove, reason="Fowl Engine auto-demotion")
                        await member.add_roles(target_role, reason="Fowl Engine auto-promotion")
                        self.log.info(f"FowlEngine: Promoted {member.display_name} to {target_role_name}")

            except Exception as ex:
                self.log.error(f"FowlEngine rank sync error: {ex}")

    @sync_ranks.before_loop
    async def before_sync_ranks(self):
        await self.bot.wait_until_ready()

    # ── Welcome / mission briefing ───────────────────────────────────────────

    @commands.Cog.listener()
    async def on_member_join(self, member: discord.Member):
        """Posts a mission-briefing embed to welcome_channel when a new member
        joins the guild. Config is guild-wide (not per-DCS-server), same as
        fe_dashboard, since a Discord join isn't tied to a specific server."""
        config = self.get_config() or {}
        channel_id = config.get('welcome_channel')
        if not channel_id:
            return
        channel = self.bot.get_channel(int(channel_id))
        if not channel:
            self.log.error(f"FowlEngine: welcome_channel {channel_id} not found or bot lacks access")
            return
        if getattr(channel, 'guild', None) and channel.guild.id != member.guild.id:
            return

        api_url = config.get("api_url", "http://localhost:8765")
        brand_name = config.get('brand_name', 'Fowl Engine')
        stats, objs = {}, []
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/stats", timeout=10) as resp:
                    if resp.status == 200:
                        stats = await resp.json()
                async with session.get(f"{api_url}/api/objectives", timeout=10) as resp:
                    if resp.status == 200:
                        objs = await resp.json()
        except Exception as ex:
            self.log.error(f"FowlEngine: failed to fetch briefing data for welcome message: {type(ex).__name__}: {ex or '(no message)'}")

        embed = discord.Embed(
            title=f"⚔️ Welcome to {brand_name}",
            color=discord.Color.gold(),
        )

        active_round = stats.get('active_round')
        if active_round:
            desc = f"**Active Scenario:** {active_round.get('scenario', 'Unknown')}"
            start_raw = active_round.get('start')
            if start_raw:
                try:
                    started = datetime.fromisoformat(start_raw.replace('Z', '+00:00'))
                    elapsed = datetime.now(timezone.utc) - started
                    days, rem = divmod(int(elapsed.total_seconds()), 86400)
                    hours, rem = divmod(rem, 3600)
                    minutes, _ = divmod(rem, 60)
                    elapsed_str = f"{days}d {hours}h {minutes}m" if days else f"{hours}h {minutes}m"
                    desc += f"\n**Round Duration:** {elapsed_str}"
                except ValueError:
                    pass
            embed.description = desc
        else:
            embed.description = "**No active round right now — check back soon.**"

        blue_objs = len([o for o in objs if o.get('owner') == 'Blue'])
        red_objs = len([o for o in objs if o.get('owner') == 'Red'])
        neutral_objs = len([o for o in objs if o.get('owner') not in ('Blue', 'Red')])
        if objs:
            embed.add_field(
                name="📊 Current Front",
                value=f"🟦 Blue: **{blue_objs}** · 🟥 Red: **{red_objs}** · ⬜ Neutral: **{neutral_objs}**",
                inline=False,
            )
            top_priority = next((o.get('name') for o in objs if o.get('priority')), None)
            if top_priority:
                embed.add_field(name="⭐ Commander Priority", value=top_priority, inline=False)

        briefing = config.get(
            'welcome_briefing',
            "Read the rules, pick your faction, and check the dashboard for your pilot profile and the live map.\n\n"
            "Use `/vs dashboard` for your secure web login, `/vs objectives` for the full objective list, "
            "and `/vs stats` to track your kills and captures.",
        )
        embed.add_field(name="📋 Briefing", value=briefing, inline=False)

        dashboard_url = config.get("dashboard_url")
        if dashboard_url:
            embed.add_field(name="🔗 Dashboard", value=f"[Open Dashboard]({dashboard_url})", inline=False)

        embed.set_thumbnail(url=member.display_avatar.url)
        embed.set_footer(text=f"Pilot #{member.guild.member_count}")

        welcome_message = config.get('welcome_message', "Welcome aboard, {mention}!")
        try:
            await channel.send(content=welcome_message.format(mention=member.mention, brand_name=brand_name), embed=embed)
        except discord.HTTPException as ex:
            self.log.error(f"FowlEngine: failed to send welcome message: {ex}")

    # ── Engine log relay ─────────────────────────────────────────────────────

    @tasks.loop(seconds=15.0)
    async def supervise_engine_logs(self):
        """Starts/stops one relay task per active server that has engine_log_channel configured."""
        active = set()
        for server in self.bot.servers.values():
            if server.status not in [Status.RUNNING, Status.PAUSED]:
                continue
            config = self.get_config(server) or {}
            if not config.get('engine_log_channel'):
                continue
            active.add(server.name)
            existing = self._log_relay_tasks.get(server.name)
            if existing is None or existing.done():
                self._log_relay_tasks[server.name] = self.bot.loop.create_task(
                    self._engine_log_relay(server)
                )
        for name in list(self._log_relay_tasks.keys()):
            if name not in active:
                self._log_relay_tasks.pop(name).cancel()

    @supervise_engine_logs.before_loop
    async def before_supervise_engine_logs(self):
        await self.bot.wait_until_ready()

    # ── Live capture/achievement alerts ─────────────────────────────────────

    @tasks.loop(seconds=15.0)
    async def supervise_campaign_events(self):
        """Starts/stops one poll task per active server that has alerts_channel
        and/or achievements_channel configured."""
        active = set()
        for server in self.bot.servers.values():
            if server.status not in [Status.RUNNING, Status.PAUSED]:
                continue
            config = self.get_config(server) or {}
            if not config.get('alerts_channel') and not config.get('achievements_channel'):
                continue
            active.add(server.name)
            existing = self._campaign_poll_tasks.get(server.name)
            if existing is None or existing.done():
                self._campaign_poll_tasks[server.name] = self.bot.loop.create_task(
                    self._campaign_event_poll(server)
                )
        for name in list(self._campaign_poll_tasks.keys()):
            if name not in active:
                self._campaign_poll_tasks.pop(name).cancel()

    @supervise_campaign_events.before_loop
    async def before_supervise_campaign_events(self):
        await self.bot.wait_until_ready()

    async def _get_faction_channels(self, main_channel, server_name: str, config: dict):
        """Resolves (creating if needed) a per-faction thread pair under
        main_channel so Blue and Red can each get alerts relevant to them
        without cluttering a shared channel or requiring separate channel IDs
        in config. Set use_faction_threads: false to disable and route
        everything to main_channel instead."""
        if not config.get('use_faction_threads', True) or not isinstance(main_channel, discord.TextChannel):
            return {"Blue": main_channel, "Red": main_channel, "Neutral": main_channel}

        brand_name = config.get('brand_name', 'Fowl Engine')
        ids = self.faction_thread_ids.setdefault(server_name, {})
        result = {}
        changed = False
        for side in ("Blue", "Red"):
            thread = None
            tid = ids.get(side)
            if tid:
                thread = main_channel.guild.get_thread(tid)
                if thread is None:
                    try:
                        thread = await self.bot.fetch_channel(tid)
                    except (discord.NotFound, discord.Forbidden):
                        thread = None
            if thread is None:
                try:
                    thread = await main_channel.create_thread(
                        name=f"{brand_name} — {side} Ops",
                        type=discord.ChannelType.public_thread,
                        auto_archive_duration=10080,
                    )
                    ids[side] = thread.id
                    changed = True
                except Exception as ex:
                    self.log.error(f"FowlEngine: failed to create {side} alerts thread for {server_name}: {ex}")
                    thread = main_channel
            result[side] = thread
        result["Neutral"] = main_channel
        if changed:
            self.save_state()
        return result

    async def _check_round_transition(self, session, api_url, server_name):
        """Detects when bfdb's active round id changes -- an admin `reset` or a
        scheduled rotation both end the current round and start a fresh one --
        and clears the per-round diff baselines. Without this, the pollers keep
        comparing new-round data against whatever owner/health/cursor was last
        seen in the round that just ended, so a reset can silently suppress the
        alerts for the objectives/kills that carried over unchanged."""
        async with session.get(f"{api_url}/api/stats", timeout=10) as resp:
            if resp.status != 200:
                return
            stats = await resp.json()
        active_round = stats.get('active_round')
        if not active_round:
            return
        rid = active_round.get('id')
        if rid is None:
            return
        prev_rid = self._active_round.get(server_name)
        self._active_round[server_name] = rid
        if prev_rid is not None and prev_rid != rid:
            self.log.info(
                f"FowlEngine: {server_name} started round {rid} (was {prev_rid}) -- "
                "resetting alert/achievement baselines"
            )
            # .clear() rather than reassignment: _campaign_event_poll holds a
            # long-lived reference to the obj_state dict across loop iterations,
            # so replacing self._obj_state[name] with a new dict wouldn't be
            # visible through that reference.
            self._obj_state.setdefault(server_name, {}).clear()
            self._capture_cursor.pop(server_name, None)
            self._kill_cursor.pop(server_name, None)
            self._kill_streaks.setdefault(server_name, {}).clear()
            self._kill_announced.setdefault(server_name, {}).clear()

    async def _campaign_event_poll(self, server: Server):
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        alerts_channel_id = config.get('alerts_channel')
        achievements_channel_id = config.get('achievements_channel')
        messages = config.get('messages', {})
        obj_state = self._obj_state.setdefault(server.name, {})

        import aiohttp
        consecutive_failures = 0
        while True:
            any_failed = False
            try:
                async with aiohttp.ClientSession() as session:
                    any_failed |= await self._poll_step(
                        server.name, api_url, "/api/stats (round check)",
                        self._check_round_transition(session, api_url, server.name),
                    )
                    if alerts_channel_id:
                        main_channel = self.bot.get_channel(int(alerts_channel_id))
                        if not main_channel:
                            self.log.error(f"FowlEngine: alerts_channel {alerts_channel_id} not found or bot lacks access")
                        else:
                            faction_channels = await self._get_faction_channels(main_channel, server.name, config)
                            any_failed |= await self._poll_step(
                                server.name, api_url, "/api/objectives",
                                self._poll_objective_changes(session, api_url, faction_channels, messages, obj_state),
                            )
                            any_failed |= await self._poll_step(
                                server.name, api_url, "/api/capture-events",
                                self._poll_capture_events(session, api_url, server.name, faction_channels, messages),
                            )
                    if achievements_channel_id:
                        any_failed |= await self._poll_step(
                            server.name, api_url, "/api/kills",
                            self._poll_kill_streaks(session, api_url, server.name, int(achievements_channel_id), messages),
                        )
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                # str(asyncio.TimeoutError()) (and several aiohttp timeout
                # subclasses) is "" by default -- include the exception type
                # so a bare-colon log line doesn't hide what actually failed.
                self.log.error(
                    f"FowlEngine: campaign event poll error for {server.name} ({api_url}): "
                    f"{type(ex).__name__}: {ex or '(no message)'}"
                )
                any_failed = True

            if any_failed:
                consecutive_failures += 1
                if consecutive_failures in (1, 5) or consecutive_failures % 30 == 0:
                    self.log.error(
                        f"FowlEngine: campaign polling for {server.name} has failed "
                        f"{consecutive_failures} time(s) in a row -- is bfdb reachable at {api_url}?"
                    )
                await asyncio.sleep(CAMPAIGN_RECONNECT_SECS)
            else:
                consecutive_failures = 0
                await asyncio.sleep(CAMPAIGN_POLL_SECS)

    async def _poll_step(self, server_name: str, api_url: str, endpoint: str, coro) -> bool:
        """Runs one poll sub-step in isolation so a hung/broken endpoint
        doesn't also block the others in the same cycle. Returns True if it failed."""
        try:
            await coro
            return False
        except Exception as ex:
            self.log.error(
                f"FowlEngine: {endpoint} poll failed for {server_name} ({api_url}): "
                f"{type(ex).__name__}: {ex or '(no message)'}"
            )
            return True

    async def _poll_objective_changes(self, session, api_url, faction_channels, messages, obj_state):
        async with session.get(f"{api_url}/api/objectives", timeout=10) as resp:
            if resp.status != 200:
                self.log.error(f"FowlEngine: /api/objectives returned {resp.status} while polling for alerts")
                return
            objs = await resp.json()

        # Don't alert on the very first snapshot -- there's no prior state to
        # diff against, and every objective would look like a fresh capture.
        first_poll = not obj_state

        for o in objs:
            name = o.get('name')
            owner = o.get('owner')
            health = o.get('health', 0)
            prev = obj_state.get(name)
            obj_state[name] = {"owner": owner, "health": health}
            if first_poll or prev is None:
                continue

            if owner != prev.get("owner"):
                if owner == "Neutral":
                    fmt = messages.get('neutral', "🏳️ **[NEUTRAL]** {message}")
                    await faction_channels["Neutral"].send(fmt.format(message=f"{name} has gone neutral."))
                # Non-neutral ownership changes are announced by
                # _poll_capture_events instead, which has pilot attribution
                # this owner-diff can't provide.
            elif prev.get("health", 0) > 20 and health <= 20:
                # The owner needs to know to defend; the opposing faction
                # needs to know there's an opportunity -- different framing,
                # each posted only to the thread it's relevant to.
                if owner in ("Blue", "Red"):
                    defend_fmt = messages.get('ready_to_capture', "⏳ **[READY TO CAPTURE]** {message}")
                    await faction_channels[owner].send(defend_fmt.format(message=f"{name} is ready to be captured -- defend it!"))
                    attacker = "Red" if owner == "Blue" else "Blue"
                    attack_fmt = messages.get('capture_opportunity', "🎯 **[OPPORTUNITY]** {message}")
                    await faction_channels[attacker].send(attack_fmt.format(message=f"{name} ({owner}) is weak and ready to be captured!"))
                else:
                    fmt = messages.get('ready_to_capture', "⏳ **[READY TO CAPTURE]** {message}")
                    await faction_channels["Neutral"].send(fmt.format(message=f"{name} ({owner}) is ready to be captured."))

    async def _poll_capture_events(self, session, api_url, server_name, faction_channels, messages):
        async with session.get(f"{api_url}/api/capture-events", params={"limit": 50}, timeout=10) as resp:
            if resp.status != 200:
                self.log.error(f"FowlEngine: /api/capture-events returned {resp.status} while polling for alerts")
                return
            events = await resp.json()
        if not events:
            return

        cursor = self._capture_cursor.get(server_name)
        new_events = [e for e in events if not cursor or e.get('time', '') > cursor]
        if not new_events:
            return
        new_events.sort(key=lambda e: e.get('time', ''))
        # First time we've ever polled this server: just establish the cursor.
        # Don't replay potentially-huge capture history as fresh alerts.
        first_poll = cursor is None
        self._capture_cursor[server_name] = new_events[-1].get('time', cursor)
        if first_poll:
            return

        names = None  # lazily fetched only if we actually need to announce something
        for e in new_events:
            obj_name = e.get('objective', 'Unknown')
            owner = e.get('side', 'Unknown')
            by = e.get('by') or []
            if by:
                if names is None:
                    names = await self._fetch_pilot_names(session, api_url)
                pilot_names = ", ".join(names.get(u, u[:8]) for u in by)
                message = f"{obj_name} was captured by {owner} ({pilot_names})."
            else:
                message = f"{obj_name} was captured by {owner}."
            fmt = messages.get('capture', "🏆 **[CAPTURED]** {message}")
            # Both factions care about a capture -- the winner as a win, the
            # loser as something to retake -- so it goes to both threads
            # rather than only the capturing side's.
            for channel in {faction_channels["Blue"], faction_channels["Red"]}:
                await channel.send(fmt.format(message=message))

    async def _fetch_pilot_names(self, session, api_url):
        try:
            async with session.get(f"{api_url}/api/pilots", timeout=10) as resp:
                if resp.status != 200:
                    return {}
                pilots = await resp.json()
                return {p.get('ucid'): p.get('name', p.get('ucid')) for p in pilots}
        except Exception:
            return {}

    async def _poll_kill_streaks(self, session, api_url, server_name, channel_id, messages):
        async with session.get(f"{api_url}/api/kills", params={"limit": 100}, timeout=10) as resp:
            if resp.status != 200:
                self.log.error(f"FowlEngine: /api/kills returned {resp.status} while polling for achievements")
                return
            kills = await resp.json()
        if not kills:
            return

        cursor = self._kill_cursor.get(server_name)
        new_kills = [k for k in kills if not cursor or k.get('time', '') > cursor]
        if not new_kills:
            return
        new_kills.sort(key=lambda k: k.get('time', ''))
        # First time we've ever polled this server: just establish the cursor.
        # Don't replay potentially-huge kill history as fresh streaks/achievements.
        first_poll = cursor is None
        self._kill_cursor[server_name] = new_kills[-1].get('time', cursor)
        if first_poll:
            return

        channel = self.bot.get_channel(channel_id)
        if not channel:
            self.log.error(f"FowlEngine: achievements_channel {channel_id} not found or bot lacks access")
            return
        streaks = self._kill_streaks.setdefault(server_name, {})
        announced = self._kill_announced.setdefault(server_name, {})
        names = None  # lazily fetched only if we actually need to announce something

        for k in new_kills:
            victim_ucid = (k.get('victim') or {}).get('ucid')
            killer = k.get('killer') or {}
            killer_ucid = killer.get('ucid')
            is_air = k.get('is_air', False)

            if victim_ucid:
                streaks[victim_ucid] = 0
                announced[victim_ucid] = 0

            if not killer_ucid or killer_ucid == victim_ucid:
                continue
            # Ace/Unstoppable/God of War are air-to-air killstreak achievements --
            # a ground kill (tank, truck, infantry, ...) shouldn't count toward or
            # reset it, so it's simply skipped rather than counted or breaking the
            # streak.
            if not is_air:
                continue
            streaks[killer_ucid] = streaks.get(killer_ucid, 0) + 1
            count = streaks[killer_ucid]
            for threshold, label in ACHIEVEMENT_THRESHOLDS:
                if count >= threshold and announced.get(killer_ucid, 0) < threshold:
                    announced[killer_ucid] = threshold
                    if names is None:
                        names = await self._fetch_pilot_names(session, api_url)
                    pname = names.get(killer_ucid, killer_ucid[:8])
                    fmt = messages.get('achievement', "🎖️ **[ACHIEVEMENT]** {message}")
                    await channel.send(fmt.format(message=f"{pname} achieved {label} ({count} air kills without dying)!"))
                    break  # only announce the highest newly-crossed threshold per kill

    async def _engine_log_relay(self, server: Server):
        """Long-lived task: logs into bfdb as admin, tails /ws/engine-logs, and
        relays it into engine_log_channel until the server stops or is unconfigured."""
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        username = config.get("admin_username")
        password = config.get("admin_password")
        channel_id = int(config['engine_log_channel'])
        channel = self.bot.get_channel(channel_id)
        if not channel:
            self.log.error(f"FowlEngine: engine_log_channel {channel_id} not found for server {server.name}")
            return
        if not username or not password:
            self.log.error(
                f"FowlEngine: engine_log_channel is set for {server.name} but admin_username/admin_password "
                "are missing (must match bfdb's --admin-username/--admin-password)"
            )
            return

        self._log_tail_buffers.setdefault(server.name, deque(maxlen=ENGINE_LOG_TAIL_LINES))
        self._log_seen_alerts.setdefault(server.name, deque(maxlen=ENGINE_LOG_ALERT_DEDUPE))
        self._log_seen_alert_set.setdefault(server.name, set())

        import aiohttp
        ws_url = api_url.replace("https://", "wss://", 1).replace("http://", "ws://", 1) + "/ws/engine-logs"

        while True:
            try:
                async with aiohttp.ClientSession() as http:
                    session_cookie = await bfdb_login(http, api_url, username, password)
                    async with http.ws_connect(
                        ws_url, headers={"Cookie": f"session={session_cookie}"}, timeout=10
                    ) as ws:
                        self.log.info(f"FowlEngine: engine log relay connected for {server.name}")
                        await self._pump_engine_log(ws, channel, server.name)
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                # Same blank-message pitfall as _campaign_event_poll: bare
                # timeouts stringify to "", so include the exception type.
                self.log.error(
                    f"FowlEngine: engine log relay error for {server.name}: "
                    f"{type(ex).__name__}: {ex or '(no message)'}"
                )
            await asyncio.sleep(ENGINE_LOG_RECONNECT_SECS)

    async def _pump_engine_log(self, ws, channel: discord.abc.Messageable, server_name: str):
        import aiohttp
        tail = self._log_tail_buffers[server_name]
        seen = self._log_seen_alerts[server_name]
        seen_set = self._log_seen_alert_set[server_name]
        pending_alerts = []
        last_flush = self.bot.loop.time()

        while True:
            timeout = max(0.1, ENGINE_LOG_FLUSH_SECS - (self.bot.loop.time() - last_flush))
            try:
                msg = await asyncio.wait_for(ws.receive(), timeout=timeout)
            except asyncio.TimeoutError:
                msg = None

            if msg is not None:
                if msg.type == aiohttp.WSMsgType.TEXT:
                    line = msg.data
                    tail.append(line)
                    if ENGINE_LOG_LEVEL_RE.search(line) and line not in seen_set:
                        if len(seen) == seen.maxlen:
                            seen_set.discard(seen[0])
                        seen.append(line)
                        seen_set.add(line)
                        pending_alerts.append(line)
                elif msg.type in (aiohttp.WSMsgType.CLOSED, aiohttp.WSMsgType.ERROR, aiohttp.WSMsgType.CLOSE):
                    if pending_alerts:
                        await self._send_engine_log_alerts(channel, pending_alerts)
                    raise RuntimeError("engine log websocket closed")

            now = self.bot.loop.time()
            if now - last_flush >= ENGINE_LOG_FLUSH_SECS:
                if tail:
                    await self._flush_engine_log_tail(channel, server_name, tail)
                if pending_alerts:
                    await self._send_engine_log_alerts(channel, pending_alerts)
                    pending_alerts = []
                last_flush = now

    async def _flush_engine_log_tail(self, channel: discord.abc.Messageable, server_name: str, tail: deque):
        body = "\n".join(tail)
        if len(body) > 3800:
            body = body[-3800:]
        embed = discord.Embed(
            title=f"📟 Live Engine Log — {server_name}",
            description=f"```{body}```",
            color=discord.Color.dark_gray(),
        )
        embed.timestamp = discord.utils.utcnow()
        msg_id = self.tail_msg_ids.get(server_name)
        try:
            if msg_id:
                msg = await channel.fetch_message(msg_id)
                await msg.edit(embed=embed)
                return
        except discord.NotFound:
            pass
        except discord.HTTPException as ex:
            self.log.error(f"FowlEngine: failed to edit engine log tail for {server_name}: {ex}")
            return
        msg = await channel.send(embed=embed)
        self.tail_msg_ids[server_name] = msg.id
        self.save_state()

    async def _send_engine_log_alerts(self, channel: discord.abc.Messageable, lines: list):
        body = "\n".join(lines)
        for i in range(0, len(body), 1900):
            chunk = body[i:i + 1900]
            embed = discord.Embed(
                title="⚠️ Engine Warning/Error",
                description=f"```{chunk}```",
                color=discord.Color.red(),
            )
            embed.timestamp = discord.utils.utcnow()
            try:
                await channel.send(embed=embed)
            except discord.HTTPException as ex:
                self.log.error(f"FowlEngine: failed to send engine log alert: {ex}")
        
    @command(description='Force an immediate update of the status embed.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_force_status(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        await self.update_status()
        await interaction.followup.send("Forced status update loop to run. Check the bot logs or the status channel!")

    @command(description='List Fowl Engine objectives.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS')
    async def fe_objectives(self, interaction: discord.Interaction,
                     server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])]):
        await interaction.response.defer(thinking=True)
        try:
            import aiohttp
            config = self.get_config(server) or {}
            api_url = config.get("api_url", "http://localhost:8765")
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/objectives") as resp:
                    if resp.status != 200:
                        await interaction.followup.send("Failed to retrieve objectives from dashboard API.")
                        return
                    objs = await resp.json()
            
            if not objs:
                await interaction.followup.send("No objectives found.")
                return

            embed = discord.Embed(title="Fowl Engine Objectives", color=discord.Color.green())

            def fmt_obj(o):
                return f"⭐ {o['name']}" if o.get('priority') else o['name']

            blue_objs = [fmt_obj(o) for o in objs if o.get('owner') == 'Blue']
            red_objs = [fmt_obj(o) for o in objs if o.get('owner') == 'Red']
            neutral_objs = [fmt_obj(o) for o in objs if o.get('owner') not in ['Blue', 'Red']]

            embed.add_field(name=f"Blue ({len(blue_objs)})", value=", ".join(blue_objs) if blue_objs else "None", inline=False)
            embed.add_field(name=f"Red ({len(red_objs)})", value=", ".join(red_objs) if red_objs else "None", inline=False)
            embed.add_field(name=f"Neutral ({len(neutral_objs)})", value=", ".join(neutral_objs) if neutral_objs else "None", inline=False)
            
            await interaction.followup.send(embed=embed)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @command(description='Show detailed status for one objective.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS')
    async def fe_objective(self, interaction: discord.Interaction,
                           server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                           name: str):
        await interaction.response.defer()
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/objectives", timeout=10) as resp:
                    if resp.status != 200:
                        await interaction.followup.send("Failed to retrieve objectives from dashboard API.")
                        return
                    objs = await resp.json()
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")
            return

        needle = name.strip().lower()
        matches = [o for o in objs if needle in o.get('name', '').lower()]
        if not matches:
            await interaction.followup.send(f"No objective matching `{name}` found.")
            return
        if len(matches) > 1:
            exact = [o for o in matches if o.get('name', '').lower() == needle]
            if len(exact) == 1:
                matches = exact
            else:
                names = ", ".join(o.get('name', '?') for o in matches[:10])
                await interaction.followup.send(f"Multiple objectives match `{name}`: {names}. Be more specific.")
                return

        o = matches[0]
        owner = o.get('owner', 'Unknown')
        health = o.get('health', 0)
        color = {"Blue": discord.Color.blue(), "Red": discord.Color.red()}.get(owner, discord.Color.light_grey())
        embed = discord.Embed(title=f"🎯 {o.get('name', 'Unknown')}", color=color)
        embed.add_field(name="Owner", value=owner, inline=True)
        embed.add_field(name="Health", value=f"{health}%", inline=True)
        if o.get('kind'):
            embed.add_field(name="Type", value=o['kind'], inline=True)
        if o.get('priority'):
            embed.add_field(name="Priority", value="⭐ Yes", inline=True)
        if health <= 20:
            embed.add_field(name="Status", value="⏳ Ready to capture!", inline=False)
        await interaction.followup.send(embed=embed)

    @command(description='Get the link to the Fowl Engine web dashboard.')
    @app_commands.guild_only()
    async def fe_dashboard(self, interaction: discord.Interaction):
        config = self.get_config() or {}
        dashboard_url = config.get("dashboard_url", "https://bfweb.your-domain.com")
        dashboard_secret = config.get("dashboard_secret", None)
        
        embed = discord.Embed(title="Fowl Engine Dashboard", color=discord.Color.blue())
        embed.description = "Access your pilot profile, live map, and stats."
        
        embed.add_field(name="Standard Login", value=f"[Login with Discord]({dashboard_url}/login)", inline=False)
        
        if dashboard_secret:
            import hmac, hashlib, base64, time, json
            payload = {"id": str(interaction.user.id), "exp": int(time.time()) + 3600}
            payload_b64 = base64.urlsafe_b64encode(json.dumps(payload).encode()).decode().rstrip('=')
            signature = hmac.new(dashboard_secret.encode(), payload_b64.encode(), hashlib.sha256).digest()
            sig_b64 = base64.urlsafe_b64encode(signature).decode().rstrip('=')
            token = f"{payload_b64}.{sig_b64}"
            auto_login_url = f"{dashboard_url}/auth?token={token}"
            embed.add_field(name="Auto-Login (Expires in 1 hr)", value=f"[Click here for secure auto-login]({auto_login_url})\n*Do not share this link!*", inline=False)
            
        await interaction.response.send_message(embed=embed, ephemeral=True)

    @command(description='Join a faction (Red, Blue, Neutral).')
    @app_commands.guild_only()
    async def fe_join(self, interaction: discord.Interaction, 
                      server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])]):
        await interaction.response.defer(ephemeral=True)
        await interaction.followup.send("⚠️ Faction selection in the Fowl Engine is now securely handled via the web dashboard.\n\nPlease use the `/fe_dashboard` command to securely login and select your faction there!", ephemeral=True)

    @command(description='Show your Fowl Engine statistics.')
    @app_commands.guild_only()
    async def fe_stats(self, interaction: discord.Interaction,
                       server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                       user: discord.Member = None):
        target = user or interaction.user
        await interaction.response.defer()
        
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        
        # Get the UCID for the discord member
        ucid = self.bot.get_ucid_by_member(target)
        if not ucid:
            await interaction.followup.send(f"{target.display_name} has not linked their Discord account to a DCS UCID.")
            return
            
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/pilot/{ucid}/breakdown") as resp:
                    if resp.status != 200:
                        await interaction.followup.send(f"Failed to fetch stats for {target.display_name}. Ensure the backend is running.")
                        return
                    rounds = await resp.json()
                    
            if not rounds:
                await interaction.followup.send(f"No stats found for {target.display_name}.")
                return
                
            # Aggregate stats across all rounds
            total_air = sum(r.get('air_kills', 0) for r in rounds)
            total_gnd = sum(r.get('ground_kills', 0) for r in rounds)
            total_caps = sum(r.get('captures', 0) for r in rounds)
            total_deaths = sum(r.get('deaths', 0) for r in rounds)
            total_hours = sum(r.get('hours', 0.0) for r in rounds)
            
            # Calculate synthetic points
            points = (total_air * 10) + (total_gnd * 2) + (total_caps * 50)
            
            brand_name = config.get('brand_name', 'Fowl Engine')
            embed = discord.Embed(title=f"{brand_name} Stats for {target.display_name}", color=discord.Color.gold())
            embed.add_field(name="Air Kills", value=str(total_air), inline=True)
            embed.add_field(name="Ground Kills", value=str(total_gnd), inline=True)
            embed.add_field(name="Captures", value=str(total_caps), inline=True)
            embed.add_field(name="Deaths", value=str(total_deaths), inline=True)
            embed.add_field(name="Flight Hours", value=f"{total_hours:.1f}h", inline=True)
            embed.add_field(name="Total Points", value=str(points), inline=True)
            
            await interaction.followup.send(embed=embed)
        except Exception as ex:
            self.log.error(f"Error in fe_stats: {ex}")
            await interaction.followup.send("An error occurred while fetching stats.")

    @command(description='Show who is currently online, by faction.')
    @app_commands.guild_only()
    async def fe_online(self, interaction: discord.Interaction,
                        server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])]):
        await interaction.response.defer()
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/online", timeout=10) as resp:
                    if resp.status != 200:
                        await interaction.followup.send("Failed to retrieve online pilots from dashboard API.")
                        return
                    pilots = await resp.json()
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")
            return

        if not pilots:
            await interaction.followup.send("No pilots are currently online.")
            return

        brand_name = config.get('brand_name', 'Fowl Engine')
        embed = discord.Embed(title=f"🟢 {brand_name} — Online Pilots", color=discord.Color.green())

        def fmt(p):
            aircraft = p.get('aircraft')
            return f"{p.get('name', 'Unknown')} ({aircraft})" if aircraft else p.get('name', 'Unknown')

        for side in ("Blue", "Red", "Neutral"):
            side_pilots = [fmt(p) for p in pilots if p.get('side') == side]
            if side_pilots:
                embed.add_field(name=f"{side} ({len(side_pilots)})", value="\n".join(side_pilots), inline=True)

        await interaction.followup.send(embed=embed)

    @command(description='Show the campaign leaderboard.')
    @app_commands.guild_only()
    async def fe_leaderboard(self, interaction: discord.Interaction,
                             server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                             top: app_commands.Range[int, 1, 25] = 10):
        await interaction.response.defer()
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/leaderboard", timeout=10) as resp:
                    if resp.status != 200:
                        await interaction.followup.send("Failed to retrieve the leaderboard from dashboard API.")
                        return
                    pilots = await resp.json()
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")
            return

        if not pilots:
            await interaction.followup.send("No pilot stats recorded yet.")
            return

        def total_kills(p):
            return p.get('air_kills', 0) + p.get('ground_kills', 0)

        ranked = sorted(pilots, key=total_kills, reverse=True)[:top]
        brand_name = config.get('brand_name', 'Fowl Engine')
        embed = discord.Embed(title=f"🏆 {brand_name} Leaderboard", color=discord.Color.gold())
        medals = {0: "🥇", 1: "🥈", 2: "🥉"}
        lines = []
        for i, p in enumerate(ranked):
            deaths = p.get('deaths', 0)
            kills = total_kills(p)
            kd = f"{kills / deaths:.2f}" if deaths > 0 else ("∞" if kills > 0 else "0.00")
            rank = medals.get(i, f"{i + 1}.")
            lines.append(
                f"{rank} **{p.get('name', 'Unknown')}** — {kills} kills "
                f"({p.get('air_kills', 0)} air / {p.get('ground_kills', 0)} gnd) · "
                f"{p.get('captures', 0)} captures · {kd} K/D"
            )
        embed.description = "\n".join(lines)
        await interaction.followup.send(embed=embed)

    @command(description='Ban a pilot from the campaign (by UCID).')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_ban(self, interaction: discord.Interaction,
                     server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                     ucid: str, name: str, reason: str = "", until: str = None):
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                "❌ admin_username/admin_password must be set in fowlengine.yaml (must match bfdb's "
                "--admin-username/--admin-password) to use admin actions."
            )
            return
        try:
            status, data = await bfdb_admin_post(
                api_url, username, password,
                "/api/admin/ban", {"ucid": ucid, "name": name, "reason": reason, "until": until},
            )
            if status == 200:
                await interaction.followup.send(f"🔨 Banned **{name}** (`{ucid}`)" + (f" until {until}" if until else " indefinitely") + (f": {reason}" if reason else "."))
            else:
                await interaction.followup.send(f"❌ Failed to ban: HTTP {status}")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @command(description='Unban a pilot from the campaign (by UCID).')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_unban(self, interaction: discord.Interaction,
                       server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                       ucid: str):
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                "❌ admin_username/admin_password must be set in fowlengine.yaml (must match bfdb's "
                "--admin-username/--admin-password) to use admin actions."
            )
            return
        try:
            status, data = await bfdb_admin_post(
                api_url, username, password, "/api/admin/unban", {"ucid": ucid},
            )
            if status == 200:
                was_banned = bool(data and data.get("was_banned"))
                await interaction.followup.send(f"✅ Unbanned `{ucid}`." if was_banned else f"ℹ️ `{ucid}` was not banned.")
            else:
                await interaction.followup.send(f"❌ Failed to unban: HTTP {status}")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @command(description='Spawn a deployable at an airbase.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_spawn_deployable(self, interaction: discord.Interaction,
                                  server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                                  deployable_type: str, airbase: str):
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                "❌ admin_username/admin_password must be set in fowlengine.yaml (must match bfdb's "
                "--admin-username/--admin-password) to use commander actions."
            )
            return
        try:
            status, _data = await bfdb_admin_post(
                api_url, username, password,
                "/api/commander/spawn", {"airbase": airbase, "type": deployable_type},
            )
            if status == 200:
                await interaction.followup.send(f"✅ Successfully ordered {deployable_type} at {airbase}.")
            else:
                await interaction.followup.send(f"❌ Failed to spawn: HTTP {status}")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @command(description='Mark (or unmark) an objective as commander priority.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_priority(self, interaction: discord.Interaction,
                          server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                          objective: str, priority: bool = True):
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                "❌ admin_username/admin_password must be set in fowlengine.yaml (must match bfdb's "
                "--admin-username/--admin-password) to use commander actions."
            )
            return
        try:
            status, _data = await bfdb_admin_post(
                api_url, username, password,
                "/api/admin/priority", {"objective": objective, "priority": priority},
            )
            if status == 200:
                verb = "marked" if priority else "unmarked"
                await interaction.followup.send(f"⭐ **{objective}** {verb} as priority.")
            else:
                await interaction.followup.send(f"❌ Failed to set priority: HTTP {status}")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @command(description='Open the interactive Commander Terminal.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_terminal(self, interaction: discord.Interaction,
                          server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])]):
        await interaction.response.defer(ephemeral=True)
        try:
            import aiohttp
            config = self.get_config(server) or {}
            api_url = config.get("api_url", "http://localhost:8765")
            admin_username = config.get("admin_username")
            admin_password = config.get("admin_password")

            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/objectives") as resp:
                    if resp.status != 200:
                        await interaction.followup.send("Failed to retrieve airbases from dashboard API.")
                        return
                    objs = await resp.json()
                    
                cfg = None
                cfg_path = config.get("cfg_path")
                if cfg_path:
                    try:
                        import json
                        with open(cfg_path, 'r', encoding='utf-8') as f:
                            cfg = json.load(f)
                    except Exception as e:
                        self.log.error(f"Failed to read CFG from {cfg_path}: {e}")
                        
                if not cfg:
                    async with session.get(f"{api_url}/api/config") as resp:
                        if resp.status != 200:
                            await interaction.followup.send("Failed to retrieve CFG from local file or dashboard API.")
                            return
                        cfg = await resp.json()
            
            # Filter for Airbases and FARPs
            airbases = [o for o in objs if o.get('kind') in ['Airbase', 'Farp']]
            
            dynamic_types = []
            if 'ground_vehicle_cargo' in cfg:
                for k in cfg['ground_vehicle_cargo'].keys():
                    dynamic_types.append((k, "Cargo Vehicle"))
            
            if 'deployables' in cfg:
                for side in ['Red', 'Blue']:
                    if side in cfg['deployables']:
                        for d in cfg['deployables'][side]:
                            if 'path' in d and len(d['path']) > 0:
                                name = d['path'][-1]
                                if (name, side) not in dynamic_types:
                                    dynamic_types.append((name, f"{side} Deployable"))
            
            # Fallback
            if not dynamic_types:
                dynamic_types.append(("No Deployables found", "CFG empty"))
                
            view = CommanderTerminalView(api_url, admin_username, admin_password, airbases, dynamic_types)
            brand_name = config.get('brand_name', 'Fowl Engine')
            embed = discord.Embed(title=f"{brand_name} Commander Terminal", color=discord.Color.dark_red())
            embed.description = "Use the controls below to order logistics."
            await interaction.followup.send(embed=embed, view=view)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    # ── bflib.dll upload ─────────────────────────────────────────────────────
    # One step, not stage-then-deploy: DCS.exe only releases bflib.dll's file
    # lock once it's actually shut down, so there's no way to swap the file in
    # while the server is still up anyway -- the ServerTransformer status
    # filter below only offers servers that are already SHUTDOWN/STOPPED (shut
    # it down first, e.g. via DCSServerBot's own server stop/shutdown command).
    # Overwrites bflib_dll_path directly after a timestamped backup, then
    # restarts the server unless restart:False is passed.
    @command(description='Upload a new bflib.dll and restart the server with it (server must already be shut down).')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_upload_bflib(self, interaction: discord.Interaction,
                               server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.SHUTDOWN, Status.STOPPED])],
                               file: discord.Attachment, restart: bool = True):
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        live_path = config.get('bflib_dll_path')
        if not live_path:
            await interaction.followup.send("❌ `bflib_dll_path` is not set in fowlengine.yaml.")
            return
        if server.status not in (Status.SHUTDOWN, Status.STOPPED):
            await interaction.followup.send(
                f"❌ **{server.name}** is currently `{server.status.name}` -- shut it down first. "
                f"DCS.exe has to release bflib.dll before it can be replaced."
            )
            return
        if not file.filename.lower().endswith('.dll'):
            await interaction.followup.send(f"❌ `{file.filename}` doesn't look like a .dll -- refusing.")
            return
        if file.size > 200 * 1024 * 1024:
            await interaction.followup.send(f"❌ `{file.filename}` is {file.size / 1024 / 1024:.1f} MB -- too large to be a real bflib.dll, refusing.")
            return

        try:
            if os.path.exists(live_path):
                backup_path = f"{live_path}.backup-{datetime.now(timezone.utc).strftime('%Y%m%d-%H%M%S')}"
                shutil.copy2(live_path, backup_path)
            await file.save(live_path)
        except Exception as ex:
            await interaction.followup.send(f"❌ Failed to write `{live_path}`: {ex}")
            return

        if not restart:
            await interaction.followup.send(
                f"✅ `{live_path}` overwritten with `{file.filename}` ({file.size / 1024:.0f} KB). "
                f"Start **{server.name}** when you're ready."
            )
            return

        await interaction.followup.send(f"✅ `{live_path}` overwritten with `{file.filename}`. Starting **{server.name}**...")
        try:
            await server.startup()
        except Exception as ex:
            await interaction.followup.send(f"❌ Server failed to come back up: {ex}\nStart it manually and check its logs.")
            return
        await interaction.followup.send(f"✅ **{server.name}** is back up with the new bflib.dll.")

async def setup(bot: DCSServerBot):
    await bot.add_cog(FowlEngine(bot))
