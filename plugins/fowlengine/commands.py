import discord
from core import Plugin, utils, Server, Status, command
from discord import app_commands
from discord.ext import tasks
from services.bot import DCSServerBot
import json
import os
import re
import asyncio
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
        self.tail_msg_ids = {}  # server name -> engine log tail message id
        self.state_file = os.path.join(bot.node.config_dir, 'fowlengine_state.json')
        if os.path.exists(self.state_file):
            try:
                with open(self.state_file, 'r') as f:
                    state = json.load(f)
                    self.status_msg_id = state.get('status_msg_id')
                    self.tail_msg_ids = state.get('tail_msg_ids', {})
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

    async def cog_load(self) -> None:
        await super().cog_load()
        utils.safe_start(self.update_status)
        utils.safe_start(self.sync_ranks)
        utils.safe_start(self.supervise_engine_logs)
        utils.safe_start(self.supervise_campaign_events)

    async def cog_unload(self):
        await utils.safe_cancel(self.update_status)
        await utils.safe_cancel(self.sync_ranks)
        await utils.safe_cancel(self.supervise_engine_logs)
        await utils.safe_cancel(self.supervise_campaign_events)
        for task in self._log_relay_tasks.values():
            task.cancel()
        for task in self._campaign_poll_tasks.values():
            task.cancel()
        await super().cog_unload()

    def save_state(self):
        try:
            with open(self.state_file, 'w') as f:
                json.dump({'status_msg_id': self.status_msg_id, 'tail_msg_ids': self.tail_msg_ids}, f)
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
                        
                blue_objs = len([o for o in objs if o.get('owner') == 'Blue'])
                red_objs = len([o for o in objs if o.get('owner') == 'Red'])
                
                brand_name = config.get('brand_name', 'Fowl Engine')
                embed = discord.Embed(title=f"{brand_name} Campaign Status", color=discord.Color.blue())
                
                if 'active_round' in stats and stats['active_round']:
                    embed.description = f"**Active Scenario:** {stats['active_round'].get('scenario', 'Unknown')}"
                else:
                    embed.description = "**Server Offline or No Active Round**"
                    
                embed.add_field(name="🟦 Blue Faction", value=f"Pilots: {stats.get('blue_online', 0)}\nObjectives: {blue_objs}", inline=True)
                embed.add_field(name="🟥 Red Faction", value=f"Pilots: {stats.get('red_online', 0)}\nObjectives: {red_objs}", inline=True)
                
                weather = stats.get('weather')
                if weather:
                    temp = weather.get('temp_c', 0)
                    wind = weather.get('wind_speed_kts', 0)
                    clouds = weather.get('cloud_density', 0)
                    embed.add_field(name="⛅ Live Weather", value=f"🌡️ {temp:.1f}°C | 💨 {wind:.1f}kts | ☁️ Density {clouds}/10", inline=False)
                    
                restart_at = stats.get('restart_at')
                if restart_at:
                    embed.set_footer(text=f"Rotation scheduled for {restart_at}")    
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

                    member = self.bot.get_member_by_ucid(ucid)
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

    async def _campaign_event_poll(self, server: Server):
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        alerts_channel_id = config.get('alerts_channel')
        achievements_channel_id = config.get('achievements_channel')
        messages = config.get('messages', {})
        obj_state = self._obj_state.setdefault(server.name, {})

        import aiohttp
        while True:
            try:
                async with aiohttp.ClientSession() as session:
                    if alerts_channel_id:
                        await self._poll_objective_changes(
                            session, api_url, int(alerts_channel_id), messages, obj_state
                        )
                    if achievements_channel_id:
                        await self._poll_kill_streaks(
                            session, api_url, server.name, int(achievements_channel_id), messages
                        )
                await asyncio.sleep(CAMPAIGN_POLL_SECS)
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                self.log.error(f"FowlEngine: campaign event poll error for {server.name}: {ex}")
                await asyncio.sleep(CAMPAIGN_RECONNECT_SECS)

    async def _poll_objective_changes(self, session, api_url, channel_id, messages, obj_state):
        async with session.get(f"{api_url}/api/objectives", timeout=10) as resp:
            if resp.status != 200:
                return
            objs = await resp.json()
        channel = self.bot.get_channel(channel_id)
        if not channel:
            return

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
                    await channel.send(fmt.format(message=f"{name} has gone neutral."))
                else:
                    fmt = messages.get('capture', "🏆 **[CAPTURED]** {message}")
                    await channel.send(fmt.format(message=f"{name} was captured by {owner}."))
            elif prev.get("health", 0) > 20 and health <= 20:
                fmt = messages.get('ready_to_capture', "⏳ **[READY TO CAPTURE]** {message}")
                await channel.send(fmt.format(message=f"{name} ({owner}) is ready to be captured."))

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
            return
        streaks = self._kill_streaks.setdefault(server_name, {})
        announced = self._kill_announced.setdefault(server_name, {})
        names = None  # lazily fetched only if we actually need to announce something

        for k in new_kills:
            victim_ucid = (k.get('victim') or {}).get('ucid')
            killer = k.get('killer') or {}
            killer_ucid = killer.get('ucid')

            if victim_ucid:
                streaks[victim_ucid] = 0
                announced[victim_ucid] = 0

            if not killer_ucid or killer_ucid == victim_ucid:
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
                    await channel.send(fmt.format(message=f"{pname} achieved {label} ({count} kills without dying)!"))
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
                self.log.error(f"FowlEngine: engine log relay error for {server.name}: {ex}")
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

async def setup(bot: DCSServerBot):
    await bot.add_cog(FowlEngine(bot))
