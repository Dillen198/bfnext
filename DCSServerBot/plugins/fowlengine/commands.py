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
import subprocess
from collections import deque
from typing import Optional

from .procman import Procman, BFDB_HEALTH_CHECK_SECS, sha256_of
from .upload import handle_bfbinary_upload

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

# ── Objective alert thresholds ──────────────────────────────────────────────
# An objective is "ready to be captured" at or below WEAK_HEALTH, and only
# stops being so once it has recovered to WEAK_CLEAR_HEALTH. The gap between
# the two is deliberate hysteresis: a base sitting right on the threshold gets
# nudged either side of it by every repair tick, and without the gap that
# re-announced "ready to be captured" indefinitely.
WEAK_HEALTH = 20
WEAK_CLEAR_HEALTH = 35

# How many consecutive polls must agree before an ownership change is
# announced. One poll is not enough: a single stale or mid-capture read
# otherwise produces a phantom "has gone neutral", and with the value
# alternating between polls, an endless stream of them.
OWNER_CONFIRM_POLLS = 2
ACHIEVEMENT_THRESHOLDS = [(15, "God of War"), (10, "Unstoppable"), (5, "Ace")]

# ── bfdb process supervision ────────────────────────────────────────────────
# bfdb.exe + the netidx resolver are owned by procman.py as child processes of
# the bot (this replaces the old bfsystem.ps1 launcher). The supervise_bfdb
# task below just ticks procman's health check on a loop; all the relaunch /
# cooldown / staged-binary logic lives in Procman. Config is the `bfdb:` and
# `gci:` blocks in fowlengine.yaml -- see fowlengine.sample.yaml.

# ── bfdb admin auth ──────────────────────────────────────────────────────────
# bfdb's admin-gated endpoints (e.g. /api/commander/spawn, /ws/engine-logs) only
# ever check the "session" cookie set by POST /api/auth/local-login -- they do
# not accept an Authorization header. Module-level (not on FowlEngine) so
# CommanderTerminalView, which isn't a Plugin, can use it too.


def srv_params(server_name: str | None, **extra) -> dict:
    """Query params that pin a bfdb request to one DCS server.

    A single bfdb can front several DCS servers (see deploy/multi-instance.md).
    Every instance-scoped route accepts `?server=<DCSServerBot server name>`,
    which bfdb maps to an instance via that instance's `dcs_server_name` in its
    --instances file -- so the bot never has to know bfdb's own instance ids.
    Omitting it (single-server bfdb, or a server not listed) lets bfdb answer
    from its default instance, which is the old behaviour exactly.
    """
    p = {k: v for k, v in extra.items() if v is not None}
    if server_name:
        p["server"] = server_name
    return p


def srv_path(path: str, server_name: str | None) -> str:
    """`srv_params` for the places that build a URL string rather than pass
    params -- the admin GET/POST helpers, whose callers pass a full path."""
    if not server_name:
        return path
    from urllib.parse import quote
    sep = "&" if "?" in path else "?"
    return f"{path}{sep}server={quote(server_name)}"


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
    def __init__(self, api_url: str, admin_username: str, admin_password: str, airbases: list,
                 dynamic_types: list, objectives: list | None = None, server_name: str | None = None):
        super().__init__(timeout=None)
        # Which DCS server this terminal drives. Passed through to bfdb as
        # ?server= so a shared bfdb spawns on the right one.
        self.server_name = server_name
        self.api_url = api_url
        self.admin_username = admin_username
        self.admin_password = admin_password
        self.selected_airbase = None
        self.selected_type = None
        self.selected_objective = None

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

        # Priority section (replaces the old standalone /fe_priority command).
        obj_options = []
        for o in (objectives or []):
            if o.get('owner') in ('Blue', 'Red'):
                mark = "⭐ " if o.get('priority') else ""
                obj_options.append(discord.SelectOption(
                    label=o['name'], description=f"{mark}{o.get('owner')} · {o.get('health', 0)}%"))
        obj_options = obj_options[:25]
        if obj_options:
            self.objective_select = discord.ui.Select(
                placeholder="Priority: select an objective...", options=obj_options,
                custom_id="obj_select", row=2)
            self.objective_select.callback = self.obj_callback
            self.add_item(self.objective_select)
        else:
            self.objective_select = None

    async def ab_callback(self, interaction: discord.Interaction):
        self.selected_airbase = self.airbase_select.values[0]
        await interaction.response.send_message(f"Base selected: {self.selected_airbase}", ephemeral=True)

    async def type_callback(self, interaction: discord.Interaction):
        self.selected_type = self.type_select.values[0]
        await interaction.response.send_message(f"Deployable selected: {self.selected_type}", ephemeral=True)

    async def obj_callback(self, interaction: discord.Interaction):
        self.selected_objective = self.objective_select.values[0]
        await interaction.response.send_message(f"Objective selected: {self.selected_objective}", ephemeral=True)

    async def _set_priority(self, interaction: discord.Interaction, priority: bool):
        if not self.selected_objective:
            await interaction.response.send_message("Select an objective first.", ephemeral=True)
            return
        await interaction.response.defer(ephemeral=True)
        if not self.admin_username or not self.admin_password:
            await interaction.followup.send("❌ admin_username/admin_password are not configured.")
            return
        try:
            status, _data = await bfdb_admin_post(
                self.api_url, self.admin_username, self.admin_password,
                srv_path("/api/admin/priority", self.server_name),
                {"objective": self.selected_objective, "priority": priority},
            )
        except Exception as ex:
            await interaction.followup.send(f"❌ Failed: {ex}")
            return
        if status == 200:
            verb = "marked" if priority else "unmarked"
            await interaction.followup.send(f"⭐ **{self.selected_objective}** {verb} as priority.")
        else:
            await interaction.followup.send(f"❌ Failed to set priority: HTTP {status}")

    @discord.ui.button(label="SET PRIORITY", style=discord.ButtonStyle.primary, row=3)
    async def priority_on_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self._set_priority(interaction, True)

    @discord.ui.button(label="CLEAR PRIORITY", style=discord.ButtonStyle.secondary, row=3)
    async def priority_off_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self._set_priority(interaction, False)

    @discord.ui.button(label="EXECUTE SPAWN", style=discord.ButtonStyle.success, row=4)
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
                srv_path("/api/commander/spawn", self.server_name),
                {"airbase": self.selected_airbase, "type": self.selected_type},
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
        # All of these are keyed by DCS server name. Every embed the plugin
        # keeps up to date is per server -- each instance has its own channels
        # (see the per-server sections in fowlengine.yaml) -- so a single
        # shared message id would have each server's tick trying to edit the
        # other's message, and the last writer winning.
        self.status_msg_ids = {}   # server name -> campaign status embed
        self.perf_msg_ids = {}     # server name -> performance embed
        # server name -> consolidated server-info embed message id. Per server
        # (like tail_msg_ids), otherwise two DCS servers fight over one message
        # and each overwrites the other's every 2 minutes.
        self.info_msg_ids = {}
        self.tail_msg_ids = {}  # server name -> engine log tail message id
        self.faction_thread_ids = {}  # server name -> {"Blue": thread_id, "Red": thread_id}
        self.state_file = os.path.join(bot.node.config_dir, 'fowlengine_state.json')
        if os.path.exists(self.state_file):
            try:
                with open(self.state_file, 'r') as f:
                    state = json.load(f)
                    # Migrate the pre-multi-server single ids. They belonged
                    # to whichever server was configured at the time, so park
                    # them under a legacy key and let the first tick re-home
                    # them to the server that actually owns that channel.
                    self.status_msg_ids = state.get('status_msg_ids') or {}
                    if state.get('status_msg_id') and not self.status_msg_ids:
                        self.status_msg_ids = {'__legacy__': state['status_msg_id']}
                    self.perf_msg_ids = state.get('perf_msg_ids') or {}
                    if state.get('perf_msg_id') and not self.perf_msg_ids:
                        self.perf_msg_ids = {'__legacy__': state['perf_msg_id']}
                    # Migrate the pre-multi-server single id: it belonged to
                    # whichever server was configured then, so keep it under
                    # the legacy key and let the first tick re-home it.
                    self.info_msg_ids = state.get('info_msg_ids') or {}
                    legacy_info = state.get('info_msg_id')
                    if legacy_info and not self.info_msg_ids:
                        self.info_msg_ids = {'__legacy__': legacy_info}
                    self.tail_msg_ids = state.get('tail_msg_ids', {})
                    self.faction_thread_ids = state.get('faction_thread_ids', {})
            except Exception as ex:
                self.log.error(f"Failed to load Fowl Engine state: {ex}")
        self._gci_relay_tasks = {}  # server name -> asyncio.Task (GCI transcript relay)
        # Per-server live state for the engine log relay (not persisted -- rebuilt on connect).
        self._log_relay_tasks = {}   # server name -> asyncio.Task
        self._log_tail_buffers = {}  # server name -> deque[str]
        self._log_seen_alerts = {}   # server name -> deque[str]
        self._log_seen_alert_set = {}  # server name -> set[str]
        # Per-server live state for capture/achievement polling (not persisted).
        self._campaign_poll_tasks = {}  # server name -> asyncio.Task
        # server name -> {obj_name: {owner, health, announced_owner, weak,
        # pending, pending_count}} -- the alert poller's diff baseline. See
        # _poll_objective_changes for what each field latches.
        self._obj_state = {}
        self._kill_cursor = {}          # server name -> last-processed kill ISO timestamp
        self._kill_streaks = {}         # server name -> {ucid: consecutive kill count}
        self._kill_announced = {}       # server name -> {ucid: highest threshold already announced}
        self._capture_cursor = {}       # server name -> last-processed capture-event ISO timestamp
        self._active_round = {}         # server name -> last-seen active round id
        # bfdb.exe + netidx resolver process manager (replaces bfsystem.ps1).
        # Constructed in cog_load once the config is available.
        self.procman: Procman | None = None
        self._bfdb_admin_password: str | None = None

    async def cog_load(self) -> None:
        await super().cog_load()
        # Build the process manager from the (guild-wide) config and, if
        # bfdb.manage is on, start bfdb + the netidx resolver as bot children.
        cfg = self.get_config() or {}
        self._bfdb_admin_password = (cfg.get("bfdb") or {}).get("admin_password") \
            or cfg.get("admin_password", "")
        self.procman = Procman(self.log, cfg, self.notify_ops)
        if self.procman.enabled:
            try:
                await self.procman.start(self._bfdb_admin_password)
            except Exception as ex:
                self.log.exception(f"FowlEngine: procman failed to start bfdb: {ex}")

        utils.safe_start(self.update_status)
        utils.safe_start(self.sync_ranks)
        utils.safe_start(self.supervise_engine_logs)
        utils.safe_start(self.supervise_campaign_events)
        utils.safe_start(self.update_perf_status)
        utils.safe_start(self.update_server_info)
        utils.safe_start(self.supervise_bfdb)
        utils.safe_start(self.supervise_gci_transcript)
        self._warn_shared_channels()

    # Channels that must not be shared between DCS servers: each carries a
    # continuously-edited embed or a per-server event stream, so two servers
    # pointed at one of them either interleave their alerts or fight over the
    # same message. `welcome_channel` and `ops_channel` are deliberately absent
    # -- those are guild-wide and shared on purpose.
    PER_SERVER_CHANNEL_KEYS = (
        'status_channel', 'alerts_channel', 'achievements_channel',
        'engine_log_channel', 'perf_channel', 'gci_transcript_channel',
        'server_info_channel',
    )

    def _warn_shared_channels(self) -> None:
        """Log a warning for every channel two or more servers both post to.

        With several DCS instances behind one bfdb, a channel left in DEFAULT
        is inherited by every server -- so both post their campaign status to
        the same channel and each overwrites the other's embed, which reads as
        the bot duplicating or flip-flopping. Give each server its own channel
        ids in its own per-server section (see fowlengine.sample.yaml), or set
        the key to null for the servers that shouldn't post.
        """
        try:
            seen: dict = {}
            for server in self.bot.servers.values():
                cfg = self.get_config(server) or {}
                for key in self.PER_SERVER_CHANNEL_KEYS:
                    cid = cfg.get(key)
                    if not cid:
                        continue
                    seen.setdefault((key, int(cid)), []).append(server.name)
            for (key, cid), names in seen.items():
                if len(names) > 1:
                    self.log.warning(
                        f"FowlEngine: {key} {cid} is shared by {len(names)} servers "
                        f"({', '.join(names)}) -- they will interleave or overwrite each "
                        f"other there. Give each server its own {key} in its per-server "
                        f"section of fowlengine.yaml, or set it to null for the ones that "
                        f"shouldn't post."
                    )
        except Exception as ex:
            self.log.debug(f"FowlEngine: shared-channel check skipped: {ex}")

    async def cog_unload(self):
        await utils.safe_cancel(self.update_status)
        await utils.safe_cancel(self.sync_ranks)
        await utils.safe_cancel(self.supervise_engine_logs)
        await utils.safe_cancel(self.supervise_campaign_events)
        await utils.safe_cancel(self.update_perf_status)
        await utils.safe_cancel(self.update_server_info)
        await utils.safe_cancel(self.supervise_bfdb)
        await utils.safe_cancel(self.supervise_gci_transcript)
        for task in self._log_relay_tasks.values():
            task.cancel()
        for task in self._campaign_poll_tasks.values():
            task.cancel()
        for task in self._gci_relay_tasks.values():
            task.cancel()
        if self.procman and self.procman.enabled:
            try:
                await self.procman.stop()
            except Exception as ex:
                self.log.error(f"FowlEngine: procman shutdown error: {ex}")
        await super().cog_unload()

    # ── Discord message hook: engine-binary drag-and-drop upload ─────────────

    @commands.Cog.listener()
    async def on_message(self, message: discord.Message):
        try:
            await handle_bfbinary_upload(self, message)
        except Exception as ex:
            self.log.exception(f"FowlEngine: engine-binary upload handler error: {ex}")

    # ── ops-channel notifier (shared with procman + the BFBinaries extension) ─

    async def notify_ops(self, message: str):
        """Best-effort notice to ops_channel / alerts_channel for supervision
        and deploy events. Silent if neither channel is configured."""
        cfg = self.get_config() or {}
        for server in self.bot.servers.values():
            sc = self.get_config(server)
            if sc:
                cfg = sc
                break
        await self._notify_ops(cfg, message)

    def save_state(self):
        try:
            with open(self.state_file, 'w') as f:
                json.dump({
                    'status_msg_ids': self.status_msg_ids,
                    'perf_msg_ids': self.perf_msg_ids,
                    'info_msg_ids': self.info_msg_ids,
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
                dash = (config.get("dashboard_url") or "").rstrip("/")
                async with aiohttp.ClientSession() as session:
                    sp = srv_params(server.name)
                    async with session.get(f"{api_url}/api/stats", params=sp) as resp:
                        if resp.status != 200:
                            continue
                        stats = await resp.json()
                    async with session.get(f"{api_url}/api/objectives", params=sp) as resp:
                        objs = await resp.json() if resp.status == 200 else []

                blue_objs = len([o for o in objs if o.get('owner') == 'Blue'])
                red_objs = len([o for o in objs if o.get('owner') == 'Red'])
                neutral_objs = len([o for o in objs if o.get('owner') == 'Neutral'])

                # Accent still tracks who's ahead on territory -- the one place a
                # dynamic colour actually says something.
                if blue_objs > red_objs:
                    embed_color = discord.Color.blue()
                elif red_objs > blue_objs:
                    embed_color = discord.Color.red()
                else:
                    embed_color = discord.Color.gold()

                embed = self._vs_embed("Campaign Status", color=embed_color,
                                       url=f"{dash}/map" if dash else None)

                active_round = stats.get('active_round')
                if active_round:
                    desc = f"**Scenario:** {active_round.get('scenario', 'Unknown')}"
                    start_raw = active_round.get('start')
                    if start_raw:
                        try:
                            started = self._parse_iso(start_raw)
                            elapsed = datetime.now(timezone.utc) - started
                            days, rem = divmod(int(elapsed.total_seconds()), 86400)
                            hours, rem = divmod(rem, 3600)
                            minutes, _ = divmod(rem, 60)
                            desc += "\n**Round:** " + (f"{days}d {hours}h {minutes}m" if days else f"{hours}h {minutes}m")
                        except ValueError:
                            pass
                    embed.description = desc
                else:
                    embed.description = "**No active round.**"

                embed.add_field(
                    name="🟦 Blue",
                    value=f"{stats.get('blue_online', 0)} online · {blue_objs} obj",
                    inline=True,
                )
                embed.add_field(
                    name="🟥 Red",
                    value=f"{stats.get('red_online', 0)} online · {red_objs} obj",
                    inline=True,
                )
                embed.add_field(name="⬜ Neutral", value=f"{neutral_objs} obj", inline=True)

                ready_objs = [o for o in objs if o.get('health', 100) <= 20 and o.get('owner') in ('Blue', 'Red')]
                if ready_objs:
                    names = ", ".join(f"{o['name']} ({o['owner']})" for o in ready_objs[:5])
                    if len(ready_objs) > 5:
                        names += f" +{len(ready_objs) - 5}"
                    embed.add_field(name="⏳ Ready to capture", value=names, inline=False)

                priority_objs = [o.get('name') for o in objs if o.get('priority')]
                if priority_objs:
                    embed.add_field(name="⭐ Priority", value=", ".join(priority_objs[:5]), inline=False)

                restart_at = stats.get('restart_at')
                if restart_at:
                    try:
                        restart_ts = int(self._parse_iso(restart_at).timestamp())
                        embed.add_field(name="🔄 Next rotation", value=f"<t:{restart_ts}:R>", inline=False)
                    except ValueError:
                        pass
                if dash:
                    embed.add_field(name="​", value=f"**[Open the live map ›]({dash}/map)**", inline=False)
                channel_id = int(config['status_channel'])
                channel = self.bot.get_channel(channel_id)
                if not channel:
                    self.log.error(f"FowlEngine: status_channel {channel_id} not found or bot lacks access.")
                    continue
                    
                msg_id = self.status_msg_ids.get(server.name) or self.status_msg_ids.pop('__legacy__', None)
                if msg_id:
                    try:
                        msg = await channel.fetch_message(msg_id)
                        await msg.edit(embed=embed)
                        self.status_msg_ids[server.name] = msg_id
                        continue
                    except discord.NotFound:
                        self.status_msg_ids.pop(server.name, None)
                    except discord.Forbidden:
                        self.log.error(f"FowlEngine: Bot lacks permissions to read/edit in channel {channel_id}")
                        self.status_msg_ids.pop(server.name, None)

                msg = await channel.send(embed=embed)
                self.status_msg_ids[server.name] = msg.id
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

        embed = self._vs_embed("Server Performance", color=color)

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

        embed.add_field(name="🧩 Deploy status", value=self._deploy_status_line(), inline=False)
        return embed

    def _ext_cfg(self, server, name: str) -> dict:
        """Merged config for the named extension on this server (node- and
        instance-level `extensions.<name>`, instance winning)."""
        merged: dict = {}
        for holder in (getattr(server, "node", None), getattr(server, "instance", None)):
            loc = getattr(holder, "locals", None)
            if isinstance(loc, dict):
                ext = (loc.get("extensions") or {}).get(name)
                if isinstance(ext, dict):
                    merged.update(ext)
        return merged

    def _bfbinaries_cfg(self, server) -> dict:
        """The BFBinaries *extension* config -- per fowlengine.sample.yaml it,
        not this plugin, owns `bflib_dll_path` / `staging_dir`."""
        return self._ext_cfg(server, "BFBinaries")

    def _engine_builds(self, server) -> dict:
        """{'bfdb': {...}, 'bflib': {...}, 'bftools': {...}} where each value is
        {version, git, built} for the *running/loaded* engine binary, or an
        {'error': ...}. bfdb answers `/api/version`; bflib drops a
        `Logs/bfnext-bflib-build.json` sidecar on load; bftools has a `version`
        subcommand. Results (bftools especially) are cached by file mtime."""
        import time as _t
        out: dict = {}

        cfg = self.get_config(server) or {}
        api_url = cfg.get("api_url", "http://localhost:8880")
        try:
            import urllib.request
            with urllib.request.urlopen(f"{api_url}/api/version", timeout=4) as r:
                out["bfdb"] = json.loads(r.read().decode())
        except Exception as ex:  # noqa: BLE001
            out["bfdb"] = {"error": f"{ex}"}

        home = os.path.expandvars((cfg.get("bfdb") or {}).get("home", ""))
        sidecar = os.path.join(home, "Logs", "bfnext-bflib-build.json") if home else ""
        if sidecar and os.path.exists(sidecar):
            try:
                with open(sidecar, encoding="utf-8") as fh:
                    out["bflib"] = json.load(fh)
            except Exception as ex:  # noqa: BLE001
                out["bflib"] = {"error": f"{ex}"}
        else:
            out["bflib"] = {"error": "no sidecar yet (mission not loaded since this build?)"}

        bftools = os.path.expandvars(self._ext_cfg(server, "BFWeather").get("bftools", ""))
        if bftools and os.path.exists(bftools):
            mtime = os.path.getmtime(bftools)
            cache = getattr(self, "_bftools_ver_cache", None)
            if cache and cache[0] == mtime:
                out["bftools"] = cache[1]
            else:
                try:
                    p = subprocess.run([bftools, "version"], capture_output=True,
                                       text=True, timeout=15)
                    out["bftools"] = json.loads((p.stdout or "").strip().splitlines()[-1])
                except Exception as ex:  # noqa: BLE001
                    out["bftools"] = {"error": f"{ex}"}
                self._bftools_ver_cache = (mtime, out["bftools"])
        else:
            out["bftools"] = {"error": "bftools path not configured (BFWeather extension)"}
        _ = _t  # (kept import tidy for future use)
        return out

    @staticmethod
    def _fmt_build(b: dict) -> str:
        if not b or "error" in (b or {}):
            return f"⚠️ {(b or {}).get('error', 'unknown')}"
        return f"`{b.get('git', '?')}` · {b.get('built', '?')}"

    def _bflib_dll_path(self, server) -> str:
        """Resolve the live bflib.dll path: plugin config first (legacy
        `bfbinaries:` block or bare key), then the BFBinaries extension."""
        cfg = self.get_config(server) or {}
        return os.path.expandvars(
            (cfg.get("bfbinaries") or {}).get("bflib_dll_path")
            or cfg.get("bflib_dll_path")
            or self._bfbinaries_cfg(server).get("bflib_dll_path", ""))

    def _deploy_status_line(self, server=None) -> str:
        """Engine build summary for an embed field: git rev + build time of the
        RUNNING bfdb / bflib / bftools, plus anything staged for next restart."""
        srv = server or next(iter(self.bot.servers.values()), None)
        builds = self._engine_builds(srv) if srv else {}
        lines = [f"**{n}** {self._fmt_build(builds.get(n) or {})}"
                 for n in ("bfdb", "bflib", "bftools")]
        if self.procman and self.procman.enabled:
            staged = [n for n in ("bflib.dll", "bfdb.exe") if self.procman.pending_info(n)]
            if staged:
                lines.append("⏳ staged: " + ", ".join(f"`{s}`" for s in staged)
                             + " — applies next restart")
        return "\n".join(lines) if lines else "no engine build info"

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
                status, data = await bfdb_admin_get(
                    api_url, username, password, srv_path("/api/admin/perf", server.name))
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

                msg_id = self.perf_msg_ids.get(server.name) or self.perf_msg_ids.pop('__legacy__', None)
                if msg_id:
                    try:
                        msg = await channel.fetch_message(msg_id)
                        await msg.edit(embed=embed)
                        self.perf_msg_ids[server.name] = msg_id
                        continue
                    except discord.NotFound:
                        self.perf_msg_ids.pop(server.name, None)
                    except discord.Forbidden:
                        self.log.error(f"FowlEngine: Bot lacks permissions to read/edit in channel {channel_id}")
                        self.perf_msg_ids.pop(server.name, None)

                msg = await channel.send(embed=embed)
                self.perf_msg_ids[server.name] = msg.id
                self.save_state()

            except Exception as ex:
                import traceback
                self.log.error(f"Error updating Fowl Engine perf status: {ex}\n{traceback.format_exc()}")

    @update_perf_status.before_loop
    async def before_update_perf_status(self):
        await self.bot.wait_until_ready()

    # ── GCI: frequency briefing + transcript relay ──────────────────────────

    def _gci_cfg(self, server=None) -> dict:
        """The effective `gci:` block for one DCS server: its own instance's
        overrides merged over the shared top-level block. With a single server
        (no `bfdb.instances:`) this is just the top-level block, as before."""
        cfg = self.get_config() or {}
        base = dict(cfg.get('gci') or {})
        if server is None:
            return base
        for inst in ((cfg.get('bfdb') or {}).get('instances') or []):
            if inst.get('dcs_server_name') == server.name:
                base.update(inst.get('gci') or {})
                break
        return base

    def _gci_freq_lines(self, server=None) -> list[str]:
        """Human-readable GCI frequency briefing from the `gci:` YAML block."""
        g = self._gci_cfg(server)
        if not g.get('enabled'):
            return ["GCI is currently **off**."]
        mod = g.get('modulation', 'AM')
        blue_cs = g.get('blue_controller_callsign', 'Magic')
        red_cs = g.get('red_controller_callsign', 'Overlord')
        lines = [
            f"🔵 **Blue** — `{g.get('blue_freq_mhz', 251.0):.3f} {mod}`  ({blue_cs})",
            f"🔴 **Red** — `{g.get('red_freq_mhz', 252.0):.3f} {mod}`  ({red_cs})",
        ]
        if g.get('whisper_exe'):
            lines.append("Key up and ask by callsign (e.g. *\"Magic, bogey dope\"*) — BOGEY DOPE / "
                         "PICTURE / DECLARE / SNAPLOCK / ALPHA CHECK.")
        else:
            lines.append("Broadcast only — no check-in needed. Toggle with `-gci on|off` in chat or F10 → EWR → GCI Voice.")
        return lines

    @command(description='Show the current GCI (AWACS) frequencies and how to use them.')
    @app_commands.guild_only()
    async def fe_gci(self, interaction: discord.Interaction,
                     server: Optional[app_commands.Transform[Server, utils.ServerTransformer()]] = None):
        # `server` is optional: with one DCS server there is nothing to choose,
        # and with several the frequencies are usually per-server.
        title = f"GCI / AWACS - {server.name}" if server else "GCI / AWACS"
        embed = self._vs_embed(title, color=discord.Color.teal())
        embed.description = "\n".join(self._gci_freq_lines(server))
        await interaction.response.send_message(embed=embed, ephemeral=True)

    @tasks.loop(seconds=15.0)
    async def supervise_gci_transcript(self):
        """One relay task per active server that has gci_transcript_channel set,
        mirroring supervise_engine_logs."""
        active = set()
        for server in self.bot.servers.values():
            if server.status not in [Status.RUNNING, Status.PAUSED]:
                continue
            config = self.get_config(server) or {}
            if not config.get('gci_transcript_channel'):
                continue
            active.add(server.name)
            existing = self._gci_relay_tasks.get(server.name)
            if existing is None or existing.done():
                self._gci_relay_tasks[server.name] = self.bot.loop.create_task(self._gci_transcript_relay(server))
        for name in list(self._gci_relay_tasks.keys()):
            if name not in active:
                self._gci_relay_tasks.pop(name).cancel()

    @supervise_gci_transcript.before_loop
    async def before_supervise_gci_transcript(self):
        await self.bot.wait_until_ready()

    async def _gci_transcript_relay(self, server: Server):
        """Tails bfdb's /ws/gci (admin-gated) and posts each call to
        gci_transcript_channel. Each line is JSON {time, side, text}."""
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        username, password = config.get("admin_username"), config.get("admin_password")
        channel = self.bot.get_channel(int(config['gci_transcript_channel']))
        if not channel:
            self.log.error(f"FowlEngine: gci_transcript_channel not found for {server.name}")
            return
        if not username or not password:
            self.log.error("FowlEngine: gci_transcript_channel needs admin_username/admin_password (/ws/gci is admin-only)")
            return

        import aiohttp
        # ?server= picks this DCS server's GCI transcript out of a shared bfdb.
        ws_url = api_url.replace("https://", "wss://", 1).replace("http://", "ws://", 1) + srv_path(
            "/ws/gci", server.name
        )
        while True:
            try:
                async with aiohttp.ClientSession() as http:
                    cookie = await bfdb_login(http, api_url, username, password)
                    async with http.ws_connect(ws_url, headers={"Cookie": f"session={cookie}"}, timeout=10) as ws:
                        self.log.info(f"FowlEngine: GCI transcript relay connected for {server.name}")
                        async for msg in ws:
                            if msg.type != aiohttp.WSMsgType.TEXT:
                                break
                            try:
                                call = json.loads(msg.data)
                                side = call.get("side", "blue")
                                text = call.get("text", "").strip()
                            except (ValueError, AttributeError):
                                side, text = "blue", str(msg.data)
                            if not text:
                                continue
                            emoji = "🔴" if side == "red" else "🔵"
                            try:
                                await channel.send(f"{emoji} {text}")
                            except discord.HTTPException as ex:
                                self.log.error(f"FowlEngine: GCI transcript post failed: {ex}")
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                self.log.error(f"FowlEngine: GCI transcript relay error for {server.name}: "
                               f"{type(ex).__name__}: {ex or '(no message)'}")
            await asyncio.sleep(ENGINE_LOG_RECONNECT_SECS)

    # ── consolidated server-info embed ─────────────────────────────────────

    def _server_connect_info(self, server) -> str:
        try:
            settings = getattr(server, 'settings', {}) or {}
        except Exception:
            settings = {}
        ip = (getattr(server.node, 'public_ip', None)
              or (server.node.locals.get('public_ip') if hasattr(server.node, 'locals') else None)
              or "?")
        port = settings.get('port') or getattr(getattr(server, 'instance', None), 'dcs_port', None) or "?"
        pw = settings.get('password')
        line = f"`{ip}:{port}`"
        line += f" · password `{pw}`" if pw else " · no password"
        return line

    def _build_server_info_embed(self, server) -> discord.Embed:
        up = server.status in (Status.RUNNING, Status.PAUSED)
        embed = self._vs_embed("Server Info", color=discord.Color.green() if up else discord.Color.dark_grey())
        embed.add_field(name="🔌 Connect", value=self._server_connect_info(server), inline=False)
        mission = getattr(getattr(server, 'current_mission', None), 'name', None)
        embed.add_field(name="🎮 DCS", value=f"{server.status.value}" + (f" · {mission}" if mission else ""), inline=True)
        rt = getattr(server, 'restart_time', None)
        if rt:
            try:
                embed.add_field(name="🔄 Next rotation", value=f"<t:{int(rt.timestamp())}:R>", inline=True)
            except (AttributeError, TypeError, ValueError):
                pass
        embed.add_field(name="📡 GCI / AWACS", value="\n".join(self._gci_freq_lines(server)), inline=False)
        embed.add_field(name="🧩 Engine builds", value=self._deploy_status_line(server), inline=False)
        dash = (self.get_config() or {}).get('dashboard_url')
        if dash:
            embed.add_field(name="🔗 Links",
                            value=f"[Dashboard]({dash}) · [Live map]({dash.rstrip('/')}/map)", inline=False)
        return embed

    @tasks.loop(minutes=2.0)
    async def update_server_info(self):
        for server in self.bot.servers.values():
            config = self.get_config(server) or {}
            channel_id = config.get('server_info_channel')
            if not channel_id:
                continue
            channel = self.bot.get_channel(int(channel_id))
            if not channel:
                self.log.error(f"FowlEngine: server_info_channel {channel_id} not found")
                continue
            try:
                embed = self._build_server_info_embed(server)
            except Exception as ex:
                self.log.error(f"FowlEngine: failed to build server-info embed: {ex}")
                continue
            msg_id = self.info_msg_ids.get(server.name) or self.info_msg_ids.pop('__legacy__', None)
            if msg_id:
                try:
                    msg = await channel.fetch_message(msg_id)
                    await msg.edit(embed=embed)
                    self.info_msg_ids[server.name] = msg_id
                    continue
                except discord.NotFound:
                    self.info_msg_ids.pop(server.name, None)
                except discord.Forbidden:
                    self.log.error(f"FowlEngine: cannot edit server_info_channel {channel_id}")
                    self.info_msg_ids.pop(server.name, None)
            msg = await channel.send(embed=embed)
            self.info_msg_ids[server.name] = msg.id
            self.save_state()

    @update_server_info.before_loop
    async def before_update_server_info(self):
        await self.bot.wait_until_ready()

    # ── bfdb process supervision ─────────────────────────────────────────────

    @tasks.loop(seconds=BFDB_HEALTH_CHECK_SECS)
    async def supervise_bfdb(self):
        """Ticks procman's health check. procman owns bfdb.exe + the netidx
        resolver as child processes and handles relaunch / cooldown / staged
        binary swaps itself -- this just drives it on a loop."""
        if not self.procman or not self.procman.enabled:
            return
        try:
            await self.procman.supervise_tick(self._bfdb_admin_password)
        except Exception as ex:
            self.log.exception(f"FowlEngine: procman supervise tick error: {ex}")

    @supervise_bfdb.before_loop
    async def before_supervise_bfdb(self):
        await self.bot.wait_until_ready()

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
        # A Discord join isn't tied to a DCS server, so with several of them
        # behind one bfdb the briefing needs to name which one it's about.
        # Unset -> bfdb's default instance, i.e. the old single-server result.
        sp = srv_params(config.get('welcome_server'))
        stats, objs = {}, []
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/stats", params=sp, timeout=10) as resp:
                    if resp.status == 200:
                        stats = await resp.json()
                async with session.get(f"{api_url}/api/objectives", params=sp, timeout=10) as resp:
                    if resp.status == 200:
                        objs = await resp.json()
        except Exception as ex:
            self.log.error(f"FowlEngine: failed to fetch briefing data for welcome message: {type(ex).__name__}: {ex or '(no message)'}")

        embed = self._vs_embed(f"Welcome to {brand_name}", color=discord.Color.gold())

        active_round = stats.get('active_round')
        if active_round:
            embed.description = f"**Now flying:** {active_round.get('scenario', 'Unknown')}"
        else:
            embed.description = "**No active round right now — check back soon.**"

        blue_objs = len([o for o in objs if o.get('owner') == 'Blue'])
        red_objs = len([o for o in objs if o.get('owner') == 'Red'])
        neutral_objs = len([o for o in objs if o.get('owner') not in ('Blue', 'Red')])
        if objs:
            embed.add_field(
                name="Front",
                value=f"🟦 {blue_objs} · ⬜ {neutral_objs} · 🟥 {red_objs}",
                inline=False,
            )

        briefing = config.get(
            'welcome_briefing',
            "Read the rules, pick your faction on the dashboard, and check the live map. "
            "`/fe_dashboard` for your secure login · `/fe_gci` for AWACS frequencies.",
        )
        embed.add_field(name="📋 Briefing", value=briefing, inline=False)

        if self._gci_cfg().get('enabled'):
            embed.add_field(name="📡 GCI", value="\n".join(self._gci_freq_lines()[:2]), inline=False)

        dashboard_url = config.get("dashboard_url")
        if dashboard_url:
            embed.add_field(name="🔗 Dashboard", value=f"**[Open Dashboard ›]({dashboard_url})**", inline=False)

        embed.set_thumbnail(url=member.display_avatar.url)

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
        # Neutral has its own thread too: "X has gone neutral" is high-volume
        # background noise on a contested map and was drowning the main alerts
        # channel that captures and ready-to-capture warnings also land in.
        for side in ("Blue", "Red", "Neutral"):
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
                        name=f"{brand_name} — {side} Ops" if side != "Neutral"
                             else f"{brand_name} — Neutral / Contested",
                        type=discord.ChannelType.public_thread,
                        auto_archive_duration=10080,
                    )
                    ids[side] = thread.id
                    changed = True
                except Exception as ex:
                    self.log.error(f"FowlEngine: failed to create {side} alerts thread for {server_name}: {ex}")
                    thread = main_channel
            result[side] = thread
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
        async with session.get(f"{api_url}/api/stats", params=srv_params(server_name), timeout=10) as resp:
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
                                self._poll_objective_changes(session, api_url, server.name, faction_channels, messages, obj_state),
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

    async def _poll_objective_changes(self, session, api_url, server_name, faction_channels, messages, obj_state):
        async with session.get(f"{api_url}/api/objectives", params=srv_params(server_name), timeout=10) as resp:
            if resp.status != 200:
                self.log.error(f"FowlEngine: /api/objectives returned {resp.status} while polling for alerts")
                return
            objs = await resp.json()
            live_hdr = resp.headers.get('x-fowl-live')

        # bfdb serves /api/objectives from its persisted snapshot and overlays
        # the RUNNING engine's owner/health on top -- but only when its RPC to
        # bflib succeeds. When that call times out (a loaded mission, a restart)
        # the response silently falls back to stale persisted values.
        #
        # Diffing that against the previous poll is what produced the endless
        # "X has gone neutral" / "ready to be captured" streams: successive
        # polls alternated between live and stale values, and every alternation
        # looked like a genuine owner flip or health-threshold crossing. A
        # degraded response tells us nothing about change, so skip the diff
        # entirely -- and do NOT update the baseline, so the next good poll
        # still compares against the last known-good state.
        degraded = live_hdr == '0' or (
            live_hdr is None and objs and not any(o.get('live') for o in objs)
        )
        if degraded:
            self.log.debug(
                f"FowlEngine: /api/objectives for {server_name} came back without live engine "
                f"data -- skipping this alert diff rather than reporting stale values as changes"
            )
            return

        # Don't alert on the very first snapshot -- there's no prior state to
        # diff against, and every objective would look like a fresh capture.
        first_poll = not obj_state

        for o in objs:
            name = o.get('name')
            owner = o.get('owner')
            health = o.get('health', 0)
            prev = obj_state.get(name)
            if first_poll or prev is None:
                obj_state[name] = {
                    "owner": owner, "health": health,
                    "announced_owner": owner, "weak": health <= WEAK_HEALTH,
                    "pending": None, "pending_count": 0,
                }
                continue

            state = prev
            state["health"] = health

            # ── owner change: require agreement on consecutive polls ───────
            # A single poll disagreeing with the last announced owner is not
            # enough to announce; it has to hold. This is belt-and-braces on
            # top of the degraded-response check above -- any other source of
            # a one-poll blip (a mid-capture read, a partial engine update)
            # is absorbed the same way.
            if owner != state.get("announced_owner"):
                if state.get("pending") == owner:
                    state["pending_count"] = state.get("pending_count", 0) + 1
                else:
                    state["pending"] = owner
                    state["pending_count"] = 1
                if state["pending_count"] >= OWNER_CONFIRM_POLLS:
                    state["announced_owner"] = owner
                    state["pending"] = None
                    state["pending_count"] = 0
                    if owner == "Neutral":
                        fmt = messages.get('neutral', "🏳️ **[NEUTRAL]** {message}")
                        await faction_channels["Neutral"].send(fmt.format(message=f"{name} has gone neutral."))
                    # Non-neutral ownership changes are announced by
                    # _poll_capture_events instead, which has pilot attribution
                    # this owner-diff can't provide.
            else:
                state["pending"] = None
                state["pending_count"] = 0

            state["owner"] = owner

            # ── health threshold, with hysteresis ─────────────────────────
            # `weak` latches when health drops to the threshold and only clears
            # once it has recovered well past it (WEAK_CLEAR_HEALTH). Without
            # the gap, an objective sitting right on the boundary re-announces
            # every time a repair tick nudges it one point either way.
            was_weak = state.get("weak", False)
            if not was_weak and health <= WEAK_HEALTH:
                state["weak"] = True
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
            elif was_weak and health >= WEAK_CLEAR_HEALTH:
                state["weak"] = False

    async def _poll_capture_events(self, session, api_url, server_name, faction_channels, messages):
        async with session.get(f"{api_url}/api/capture-events", params=srv_params(server_name, limit=50), timeout=10) as resp:
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
        async with session.get(f"{api_url}/api/kills", params=srv_params(server_name, limit=100), timeout=10) as resp:
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
        # ?server= picks this DCS server's engine log out of a shared bfdb.
        ws_url = api_url.replace("https://", "wss://", 1).replace("http://", "ws://", 1) + srv_path(
            "/ws/engine-logs", server.name
        )

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
        # keep it narrow enough not to force horizontal scroll on mobile
        body = "\n".join(line[:110] for line in body.splitlines())
        if len(body) > 3800:
            body = body[-3800:]
        embed = self._vs_embed(f"Live Engine Log — {server_name}", color=discord.Color.dark_gray())
        embed.description = f"```{body}```"
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
                async with session.get(f"{api_url}/api/objectives", params=srv_params(server.name), timeout=10) as resp:
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
        embed = self._vs_embed(f"🎯 {o.get('name', 'Unknown')}", color=color)
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
        
        embed = self._vs_embed("Dashboard", color=discord.Color.blue(), url=dashboard_url)
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
                async with session.get(f"{api_url}/api/objectives", params=srv_params(server.name)) as resp:
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
                    async with session.get(f"{api_url}/api/config", params=srv_params(server.name)) as resp:
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
                
            view = CommanderTerminalView(api_url, admin_username, admin_password, airbases,
                                         dynamic_types, objectives=objs, server_name=server.name)
            embed = self._vs_embed(f"{config.get('brand_name', 'Fowl Engine')} Commander Terminal",
                                   color=discord.Color.dark_red())
            embed.description = f"Order logistics (top) or set objective priority (bottom).\nServer: **{server.name}**"
            await interaction.followup.send(embed=embed, view=view)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    # ── shared embed builder ────────────────────────────────────────────────

    def _vs_embed(self, title: str, *, color: discord.Color | None = None,
                  url: str | None = None) -> discord.Embed:
        """Every FowlEngine embed goes through here for consistent Vector
        Strike branding: brand author line + logo, timestamped footer. The
        logo is pulled from the dashboard origin so it survives message edits
        (no attachment needed)."""
        cfg = self.get_config() or {}
        brand = cfg.get('brand_name', 'Fowl Engine')
        dash = (cfg.get('dashboard_url') or '').rstrip('/')
        icon = f"{dash}/vs-vectorstrike_hd-white.png" if dash else None
        embed = discord.Embed(title=title, color=color or discord.Color.from_str('#c8102e'))
        if url:
            embed.url = url
        embed.set_author(name=brand, icon_url=icon, url=dash or None)
        embed.set_footer(text=brand, icon_url=icon)
        embed.timestamp = discord.utils.utcnow()
        return embed

    # ── engine ops: staged binaries, bfdb control, GCI ──────────────────────

    feops = app_commands.Group(name="feops", description="Fowl Engine server operations (admin)")

    async def _procman_or_warn(self, interaction: discord.Interaction):
        if not self.procman or not self.procman.enabled:
            await interaction.followup.send(
                "❌ `bfdb.manage` is not enabled in fowlengine.yaml -- the bot isn't managing "
                "bfdb/netidx, so there's nothing to control here.")
            return None
        return self.procman

    @feops.command(name="bfdb_restart", description="Restart bfdb (re-renders gci.json, picks up a staged bfdb.exe).")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_bfdb_restart(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        cfg = self.get_config() or {}
        pm.reload_config(cfg)
        self._bfdb_admin_password = (cfg.get("bfdb") or {}).get("admin_password") \
            or cfg.get("admin_password", "") or self._bfdb_admin_password
        await pm.restart(self._bfdb_admin_password)
        ok = await pm.health_ok()
        await interaction.followup.send(
            "✅ bfdb restarted and answering." if ok else
            "⚠️ bfdb restarted but not answering yet -- give it a minute, then check `/feops` again.")

    @feops.command(name="gci_show", description="Show the effective gci.json that will be written for bfdb (secrets masked).")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_gci_show(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        if pm.multi_instance:
            # One gci.<id>.json per DCS server -- show which ones are on,
            # not a single merged blob that belongs to none of them.
            lines = []
            for inst in pm.instances_cfg:
                merged = dict(pm.gci_cfg)
                merged.update(inst.get("gci") or {})
                iid = inst.get("id", "?")
                if not merged.get("enabled"):
                    lines.append(f"**{iid}** - GCI off")
                    continue
                lines.append(
                    f"**{iid}** - blue `{merged.get('blue_freq_mhz', 251.0)} "
                    f"{merged.get('modulation', 'AM')}` ({merged.get('blue_controller_callsign', 'Magic')}) / "
                    f"red `{merged.get('red_freq_mhz', 252.0)}` "
                    f"({merged.get('red_controller_callsign', 'Overlord')}) "
                    f"via SRS {merged.get('srs_host', '127.0.0.1')}:{merged.get('srs_port', 5002)}"
                )
            await interaction.followup.send("\n".join(lines) or "no instances configured")
            return
        if not pm.gci_enabled():
            await interaction.followup.send("GCI is disabled (`gci.enabled: false`) -- no gci.json is written.")
            return
        body = json.dumps(pm.gci_effective(mask=True), indent=2)
        await interaction.followup.send(f"```json\n{body[:1900]}\n```")

    @feops.command(name="stage_status", description="Show any staged bflib.dll / bfdb.exe waiting to be applied.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_stage_status(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        lines = []
        for name in ("bflib.dll", "bfdb.exe"):
            info = pm.pending_info(name)
            if not info:
                continue
            lines.append(
                f"• `{name}` — {info['size'] / 1024 / 1024:.1f} MB · "
                f"`sha256:{(info.get('sha256') or '')[:12]}` · by {info.get('uploader', '?')} "
                f"at {info.get('utc', '?')}" + (f"\n   notes: {info['notes']}" if info.get('notes') else ""))
        if not lines:
            await interaction.followup.send("Nothing staged. Drop `bflib.dll` or `bfdb.exe` into the admin channel to stage one.")
            return
        next_at = ""
        for server in self.bot.servers.values():
            rt = getattr(server, "restart_time", None)
            if rt:
                next_at = f"\n\nNext scheduled restart: <t:{int(rt.timestamp())}:R>"
                break
        await interaction.followup.send("**Staged engine binaries:**\n" + "\n".join(lines) + next_at)

    @feops.command(name="stage_cancel", description="Discard a staged binary.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    @app_commands.choices(which=[
        app_commands.Choice(name="bflib.dll", value="bflib.dll"),
        app_commands.Choice(name="bfdb.exe", value="bfdb.exe"),
        app_commands.Choice(name="all", value="all"),
    ])
    async def feops_stage_cancel(self, interaction: discord.Interaction, which: app_commands.Choice[str]):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        targets = ["bflib.dll", "bfdb.exe"] if which.value == "all" else [which.value]
        removed = [t for t in targets if pm.cancel_pending(t)]
        await interaction.followup.send(
            f"🗑️ Discarded: {', '.join(f'`{t}`' for t in removed)}" if removed else "Nothing to discard.")

    @feops.command(name="stage_apply", description="Apply a staged binary now instead of waiting for the next restart.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    @app_commands.choices(which=[
        app_commands.Choice(name="bflib.dll", value="bflib.dll"),
        app_commands.Choice(name="bfdb.exe", value="bfdb.exe"),
        app_commands.Choice(name="all", value="all"),
    ])
    async def feops_stage_apply(self, interaction: discord.Interaction,
                                server: app_commands.Transform[Server, utils.ServerTransformer()],
                                which: app_commands.Choice[str]):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        want = ["bflib.dll", "bfdb.exe"] if which.value == "all" else [which.value]
        msgs = []

        if "bfdb.exe" in want and pm.pending_info("bfdb.exe"):
            await pm.restart(self._bfdb_admin_password)  # start() applies the staged exe
            msgs.append("bfdb.exe: swapped and bfdb restarted." if await pm.health_ok()
                        else "bfdb.exe: swapped, bfdb restarting (not answering yet).")

        if "bflib.dll" in want and pm.pending_info("bflib.dll"):
            live = self._bflib_dll_path(server)
            if not live:
                msgs.append("bflib.dll: skipped -- no bflib_dll_path configured (set it on the BFBinaries extension in nodes.yaml).")
            elif server.status not in (Status.SHUTDOWN, Status.STOPPED):
                msgs.append(f"bflib.dll: **{server.name}** is `{server.status.name}` -- shut it down first, "
                            f"then it applies automatically on startup (or run this again).")
            else:
                note = pm.apply_staged("bflib.dll", live)
                msgs.append(f"bflib.dll: {note}" if note else "bflib.dll: nothing staged.")
                try:
                    await server.startup()
                    msgs.append(f"{server.name} started.")
                except Exception as ex:
                    msgs.append(f"⚠️ {server.name} failed to start: {ex}")

        await interaction.followup.send("\n".join(msgs) if msgs else "Nothing staged to apply.")

    @feops.command(name="versions", description="Show the git rev + build time of the running engine binaries.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_versions(self, interaction: discord.Interaction,
                             server: app_commands.Transform[Server, utils.ServerTransformer()]):
        await interaction.response.defer(ephemeral=True)
        builds = await asyncio.get_running_loop().run_in_executor(
            None, lambda: self._engine_builds(server))
        cfg = self.get_config(server) or {}
        exe = os.path.expandvars((cfg.get("bfdb") or {}).get("exe", ""))
        dll = self._bflib_dll_path(server)
        bftools = os.path.expandvars(self._ext_cfg(server, "BFWeather").get("bftools", ""))
        disk = {"bfdb": exe, "bflib": dll, "bftools": bftools}

        lines = []
        for name in ("bfdb", "bflib", "bftools"):
            b = builds.get(name) or {}
            path = disk.get(name) or ""
            if "error" in b:
                lines.append(f"**{name}** — ⚠️ {b['error']}")
            else:
                lines.append(f"**{name}** `v{b.get('version','?')}` `{b.get('git','?')}` "
                             f"built {b.get('built','?')}")
            if path and os.path.exists(path):
                mt = datetime.fromtimestamp(os.path.getmtime(path), tz=timezone.utc)
                lines.append(f"   ↳ file `{sha256_of(path)[:10]}` · modified {mt:%Y-%m-%d %H:%M}Z")
            elif path:
                lines.append(f"   ↳ file missing: `{path}`")

        if self.procman and self.procman.enabled:
            staged = [n for n in ("bflib.dll", "bfdb.exe") if self.procman.pending_info(n)]
            if staged:
                lines.append("\n⏳ staged (applies next restart): " + ", ".join(f"`{s}`" for s in staged))

        embed = self._vs_embed("Engine Builds", color=discord.Color.blurple())
        embed.description = "\n".join(lines)
        embed.set_footer(text="'built' = compiled-in timestamp of the running binary · "
                              "'file' = what's on disk right now")
        await interaction.followup.send(embed=embed)

    @feops.command(name="rebuild_stats",
                   description="Wipe & re-ingest all stats from the log to undo duplicated/inflated numbers.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_rebuild_stats(self, interaction: discord.Interaction,
                                  server: app_commands.Transform[Server, utils.ServerTransformer()],
                                  confirm: bool = False):
        """Stops bfdb, runs `bfdb --rebuild-stats` (wipes every stats-derived
        tree + rewinds the replay cursors), restarts it so it re-ingests the
        stats log cleanly. Fixes inflated career totals / kill counts left by
        the old whole-file re-reads. Auth, Discord links, bans, wiki and recon
        intel are preserved."""
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                "❌ admin_username/admin_password must be set in fowlengine.yaml to use admin actions.")
            return
        if not confirm:
            await interaction.followup.send(
                "⚠️ This wipes all pilot stats / kills / sorties / objectives and rebuilds them "
                "from the stats log — takes a few minutes. bfdb stays up (in-process rebuild). "
                "Auth, Discord links, bans, wiki and recon intel are kept.\n"
                "Re-run with **confirm: True** to proceed.")
            return
        try:
            status, data = await bfdb_admin_post(
                api_url, username, password, "/api/admin/rebuild-stats", {})
            if status != 200:
                await interaction.followup.send(f"❌ rebuild-stats failed: HTTP {status} {data}")
                return
            msg = (data or {}).get("message", str(data))
            await interaction.followup.send(
                f"✅ {msg}\nWatch the engine-log / perf embed. When it settles, run `/feops merge_rounds`.")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @feops.command(name="fresh_db",
                   description="LAST RESORT: move a corrupt bfdb DB aside and rebuild it from the stats log.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_fresh_db(self, interaction: discord.Interaction,
                             server: app_commands.Transform[Server, utils.ServerTransformer()],
                             confirm: bool = False):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        if not confirm:
            await interaction.followup.send(
                "⚠️ **Only if the sled DB is corrupt** (a rebuild that can't get past a 'Read corrupted "
                "data at file offset' error). Moves the whole `bfdb` folder aside and rebuilds from "
                "`stats.jsonl`.\n"
                "**Rebuilt:** pilot stats, Discord links, a single clean round.\n"
                "**Lost:** dashboard login sessions (auto re-login), dashboard-added bans, in-dashboard "
                "wiki edits (reverts to seeded), per-round recon photos.\n"
                "The old folder is kept. Re-run with **confirm: True**.")
            return
        try:
            msg = await pm.fresh_db(self._bfdb_admin_password)
            await interaction.followup.send(msg)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @feops.command(name="merge_rounds",
                   description="Collapse a campaign that shows as dozens of duplicated 'rounds' back into one.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_merge_rounds(self, interaction: discord.Interaction,
                                 server: app_commands.Transform[Server, utils.ServerTransformer()],
                                 confirm: bool = False):
        """Dry-run by default. Pass confirm:True to actually re-key the data.
        Repairs the old fork-on-restart bug where one continuous campaign
        fragmented into many round ids (duplicated sorties/kills/deploys)."""
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                "❌ admin_username/admin_password must be set in fowlengine.yaml to use admin actions.")
            return
        dry = "false" if confirm else "true"
        try:
            status, data = await bfdb_admin_post(
                api_url, username, password,
                srv_path(f"/api/admin/merge-rounds?dry_run={dry}", server.name), {})
            if status != 200:
                await interaction.followup.send(f"❌ merge-rounds failed: HTTP {status} {data}")
                return
            msg = (data or {}).get("message", str(data))
            if confirm:
                await interaction.followup.send(f"✅ {msg}")
            else:
                await interaction.followup.send(
                    f"🔍 {msg}\n\nRe-run with **confirm: True** to apply.")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")


async def setup(bot: DCSServerBot):
    await bot.add_cog(FowlEngine(bot))
