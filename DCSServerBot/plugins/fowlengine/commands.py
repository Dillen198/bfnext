import discord
from core import Plugin, utils, Server, Status, command
from discord import app_commands
from discord.ext import tasks, commands
from services.bot import DCSServerBot
from datetime import datetime, timezone
import json
import os
import re
import time
import asyncio
import subprocess
from copy import deepcopy
from typing import Optional

from .procman import Procman, BFDB_HEALTH_CHECK_SECS, effective_instance_gci
from .autoupdate import Updater
from .opsapi import OpsApi
from .heartbeat import Heartbeat
from .loganalyzer import LogAnalyzer
from .upload import handle_bfbinary_upload, engine_binaries, BFTOOLS, bftools_path_of
from .icons import IconSet, icon, render as render_icons
from . import rangefeed
from . import mapsnap
from . import serverinfo
from . import branding
from .common import (ENGINE_LOG_RECONNECT_SECS, NO_PINGS, SHUTDOWN_REQUEST_FILE, STATUS_MAP_FILE, bfdb_admin_get,
                     bfdb_login, srv_params, srv_path)
from .views import StagedApplyView
from .cog_events import EventsMixin
from .cog_coalition import CoalitionMixin
from .cog_player import PlayerCommandsMixin
from .cog_ops import OpsMixin
from .cog_range import RangeMixin
from .cog_news import NewsMixin

# The cog is one class, FowlEngine, assembled from mixins -- one module per
# area, each moved out of this file unchanged:
#   cog_events.py      EventsMixin
#   cog_coalition.py   CoalitionMixin
#   cog_player.py      PlayerCommandsMixin
#   cog_ops.py         OpsMixin
#   cog_range.py       RangeMixin
#   cog_news.py        NewsMixin
#   common.py          constants and bfdb helpers
#   views.py           the Discord views (Commander Terminal, staged apply)
# DCSServerBot loads this module (plugins.fowlengine.commands) and its setup();
# discord.py finds the mixins' commands, groups, loops and listeners through
# the MRO. Like the other plugin modules, the parts take effect on a bot
# restart, not a plugin reload.


class FowlEngine(EventsMixin, CoalitionMixin, PlayerCommandsMixin,
                 OpsMixin, RangeMixin, NewsMixin, Plugin):
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
        # server name -> {"Blue": message id, "Red": message id}: the one live
        # briefing embed each coalition channel holds. Persisted, so a bot
        # restart keeps editing the same two messages instead of leaving a
        # graveyard of stale briefings behind it.
        self.briefing_msg_ids = {}
        self.faction_thread_ids = {}  # server name -> {"Blue": thread_id, "Red": thread_id}
        # Training range servers (kind: range). Both persisted: the status
        # embed is edited in place across bot restarts, and the results feed
        # resumes from the last result it posted instead of replaying history
        # (or silently skipping what arrived while the bot was down).
        self.range_status_msg_ids = {}   # server name -> range status embed
        self.range_feed_cursors = {}     # server name -> {"id", "ts", "recent"}
        # War diary feed: server name -> {"posted": [ids], "seen": {id: ts}}.
        # Persisted, so a restart never posts a dispatch twice.
        self.news_feed_state = {}
        self.rotation_index = {}        # server name -> index into its rotation.theatres
        self.round_result_state = {}    # server name -> id of the last round announced as won
        # Staged DLL / bftools.exe an admin asked to "apply when empty":
        # server name -> {"by", "utc", "attempts", "why"}. Persisted (fsync'd),
        # so a bot restart or a crash of the whole box doesn't drop the ask.
        self.stage_apply_when_empty = {}
        self._stage_empty_since = {}    # server name -> ts it was first seen empty
        self._stage_restarting = {}     # server name -> ts of the restart we started
        self._stage_missed_seen = set()  # (server name, DCS start ts) already checked
        self._stage_parts_swept = False
        # Status-embed map snapshot per server: {"sig", "at", "png",
        # "failed_at"}. Memory only -- after a restart the first tick captures
        # afresh (and re-uploads), which is also what re-binds the image.
        self._mapsnap = {}
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
                    self.briefing_msg_ids = state.get('briefing_msg_ids', {})
                    self.faction_thread_ids = state.get('faction_thread_ids', {})
                    self.range_status_msg_ids = state.get('range_status_msg_ids') or {}
                    self.range_feed_cursors = state.get('range_feed_cursors') or {}
                    self.news_feed_state = state.get('news_feed_state') or {}
                    self.rotation_index = state.get('rotation_index') or {}
                    self.round_result_state = state.get('round_result_state') or {}
                    self.stage_apply_when_empty = state.get('stage_apply_when_empty') or {}
            except Exception as ex:
                self.log.error(f"Failed to load Fowl Engine state: {ex}")
        # rendered instances.json, cached by (path, mtime) -- see _instances_list
        self._instances_cache = None
        # api_url -> (fetched epoch, /api/instances rows): bfdb's own `theme`
        # and `brand_name` per instance, for the embed branding (_branding)
        self._bfdb_instance_rows = {}
        self._range_fail_counts = {}     # server name -> consecutive feed/status failures
        self._range_warned = set()       # (server name, reason) already logged once
        self._bfdb_down = {}             # api_url -> when it was first found unreachable
        self._briefing_down = {}         # (server name, side) -> why /api/situation is 503
        self._news_fail_counts = {}      # server name -> consecutive news feed failures
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
        self._round_number = {}         # server name -> that round's number on its server
        # bfdb.exe + netidx resolver process manager (replaces bfsystem.ps1).
        # Constructed in cog_load once the config is available.
        self.procman: Procman | None = None
        self._bfdb_admin_password: str | None = None
        # Automatic engine updates + the probation/rollback watch on every
        # swapped-in engine (autoupdate.py), and the HTTP API the dashboard's
        # OPS page reaches through bfdb (opsapi.py). Both built in cog_load.
        self.updater: Updater | None = None
        self.opsapi: OpsApi | None = None
        # Log analyzer: log files -> fingerprinted issues (loganalyzer.py).
        self.issues: LogAnalyzer | None = None
        # Optional dead man's switch to an outside monitor (heartbeat.py).
        self.heartbeat = Heartbeat(self, self.log)
        # Vector Strike custom emoji. Empty until refresh() runs and until an
        # admin has actually installed them -- every icon has a unicode
        # stand-in, so embeds render correctly either way.
        self.icons = IconSet(bot, self.log)
        # for status_watch: a shutdown request older than this is stale
        self._loaded_at = time.time()

    # ── per-server config sections ──────────────────────────────────────────

    def get_base_config(self, server: Server, *args, **kwargs):
        """(default, specific) config for one DCS server.

        Stock DCSServerBot (3.0.x core/plugin.py) finds a plugin's per-server
        section by INSTANCE name -- `DCS.vectorstrike_1:`, optionally nested
        under the node name. This plugin's YAML has always keyed them by the
        DCSServerBot SERVER name instead (the display name used in
        servers.yaml, which is also what `dcs_server_name` holds), and on a
        bot that only looks up instance names those sections are silently
        ignored: every server then falls back to DEFAULT.

        Accept both. An instance-keyed section wins -- stock behaviour,
        unchanged -- and only when there is none is a section keyed by the
        server name (top level, or under the node name) used.
        """
        base = super().get_base_config(server, *args, **kwargs)
        try:
            default, specific = base
        except (TypeError, ValueError):
            return base  # a bot version with a different shape: leave it alone
        if specific:
            return default, specific
        name = getattr(server, "name", None)
        locals_ = self.locals or {}
        node_name = getattr(getattr(server, "node", None), "name", None)
        for holder in (locals_.get(node_name) if node_name else None, locals_):
            if isinstance(holder, dict) and name and isinstance(holder.get(name), dict):
                return default, deepcopy(holder[name])
        return default, specific

    # ── instance kind: campaign (bflib) vs training range (bfrange) ────────

    def _instances_list(self) -> list:
        """The bfdb instance entries: the RENDERED <bfdb.home>/instances.json
        (what bfdb is actually running with), else the YAML `bfdb.instances`."""
        cfg = self.get_config() or {}
        bcfg = cfg.get('bfdb') or {}
        home = os.path.expandvars(bcfg.get('home') or '')
        path = os.path.join(home, 'instances.json') if home else None
        if path and os.path.exists(path):
            try:
                mtime = os.path.getmtime(path)
                cache = self._instances_cache
                if cache and cache[0] == path and cache[1] == mtime:
                    return cache[2]
                with open(path, 'r', encoding='utf-8') as fh:
                    doc = json.load(fh)
                entries = doc.get('instances') if isinstance(doc, dict) else doc
                if isinstance(entries, list):
                    self._instances_cache = (path, mtime, entries)
                    return entries
            except (OSError, ValueError) as ex:
                self.log.debug(f"FowlEngine: could not read {path}: {ex}")
        return list(bcfg.get('instances') or [])

    def _instance_entry(self, server) -> dict | None:
        return rangefeed.find_instance(self._instances_list(), getattr(server, 'name', None))

    def _instance_kind(self, server) -> str:
        """"range" for a server whose bfdb instance is `kind: range`, else
        "campaign" (including every server not listed in bfdb.instances)."""
        return rangefeed.normalize_kind((self._instance_entry(server) or {}).get('kind'))

    def _instance_id(self, server) -> str | None:
        return (self._instance_entry(server) or {}).get('id') or None

    def _is_range(self, server) -> bool:
        try:
            return self._instance_kind(server) == 'range'
        except Exception as ex:  # a config read must never take a loop down
            self.log.debug(f"FowlEngine: instance kind lookup failed: {ex}")
            return False

    def _range_servers(self) -> list:
        return [s for s in self.bot.servers.values() if self._is_range(s)]

    def _range_params(self, server, **extra) -> dict:
        """Query params pinning a request to this server's instance: its bfdb
        instance id when known, and the server name either way."""
        p = srv_params(getattr(server, 'name', None), **extra)
        iid = self._instance_id(server)
        if iid:
            p['instance'] = iid
        return p

    def _range_site(self, config: dict | None = None) -> str:
        cfg = config if config is not None else (self.get_config() or {})
        return (cfg.get('range_site_url') or rangefeed.RANGE_SITE_URL).rstrip('/')

    def _warn_once(self, server_name: str, reason: str, message: str) -> None:
        key = (server_name, reason)
        if key not in self._range_warned:
            self._range_warned.add(key)
            self.log.warning(message)

    def _bfdb_failed(self, api_url: str, where: str, ex: BaseException) -> bool:
        """Whether `ex` just means bfdb is down or restarting. If so it is
        logged once per outage (here) and the caller logs nothing; otherwise
        it returns False and the caller logs the error as usual.

        A bfdb outage used to log an ERROR from every poller and relay, with
        tracebacks, every few seconds -- dozens of lines, and a dozen separate
        issues in the report, for one restart."""
        import aiohttp
        restarting = bool(self.procman and self.procman.busy)
        outage = restarting or isinstance(
            ex, (aiohttp.ClientConnectionError, asyncio.TimeoutError, ConnectionError)
        ) or str(ex) == "engine log websocket closed"
        if not outage:
            return False
        if api_url not in self._bfdb_down:
            self._bfdb_down[api_url] = time.time()
            why = "being restarted" if restarting else f"{type(ex).__name__}: {ex or '(no message)'}"
            self.log.warning(
                f"FowlEngine: bfdb at {api_url} is unreachable ({why}); "
                f"polling and relays retry quietly until it is back"
            )
        else:
            self.log.debug(f"FowlEngine: {where}: bfdb at {api_url} still down ({type(ex).__name__})")
        return True

    def _bfdb_ok(self, api_url: str) -> None:
        since = self._bfdb_down.pop(api_url, None)
        if since is not None:
            self.log.info(f"FowlEngine: bfdb at {api_url} is reachable again after {time.time() - since:.0f}s")

    async def cog_load(self) -> None:
        await super().cog_load()
        # Build the process manager from the (guild-wide) config and, if
        # bfdb.manage is on, start bfdb + the netidx resolver as bot children.
        cfg = self.get_config() or {}
        self._bfdb_admin_password = (cfg.get("bfdb") or {}).get("admin_password") \
            or cfg.get("admin_password", "")
        self.procman = Procman(self.log, cfg, self.notify_ops)
        # The updater exists even with autoupdate off: it is also what watches
        # a hand-uploaded engine through probation and rolls it back.
        self.updater = Updater(self, self.log,
                               os.path.join(self.bot.node.config_dir, 'fowlengine_update.json'))
        self.procman.on_swapped = self.updater.on_bfdb_swapped
        self.procman.on_rollback = self.updater.on_bfdb_rollback
        self.sync_update_tuning()
        self.issues = LogAnalyzer(self, self.log,
                                  os.path.join(self.bot.node.config_dir, 'fowlengine_issues.json'))
        if self.procman.enabled:
            try:
                await self.procman.start(self._bfdb_admin_password)
            except Exception as ex:
                self.log.exception(f"FowlEngine: procman failed to start bfdb: {ex}")
        self.opsapi = OpsApi(self)
        asyncio.create_task(self.opsapi.register())

        utils.safe_start(self.update_status)
        utils.safe_start(self.sync_ranks)
        utils.safe_start(self.supervise_engine_logs)
        utils.safe_start(self.supervise_campaign_events)
        utils.safe_start(self.update_perf_status)
        utils.safe_start(self.update_server_info)
        utils.safe_start(self.supervise_bfdb)
        utils.safe_start(self.supervise_gci_transcript)
        utils.safe_start(self.update_briefings)
        utils.safe_start(self.sync_coalition_roles)
        utils.safe_start(self.update_range_status)
        utils.safe_start(self.poll_range_results)
        utils.safe_start(self.post_news)
        utils.safe_start(self.autoupdate_loop)
        utils.safe_start(self.status_watch)
        utils.safe_start(self.stage_apply_watch)
        utils.safe_start(self.heartbeat_loop)
        self._warn_shared_channels()

    # Channels that must not be shared between DCS servers: each carries a
    # continuously-edited embed or a per-server event stream, so two servers
    # pointed at one of them either interleave their alerts or fight over the
    # same message. `welcome_channel` and `ops_channel` are deliberately absent
    # -- those are guild-wide and shared on purpose.
    PER_SERVER_CHANNEL_KEYS = (
        'status_channel', 'alerts_channel', 'achievements_channel',
        'engine_log_channel', 'perf_channel', 'gci_transcript_channel',
        'server_info_channel', 'blue_briefing_channel', 'red_briefing_channel',
        'range_status_channel', 'range_results_channel', 'greenie_channel',
        'news_channel',
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
                # Two DIFFERENT feeds pointed at one channel on the SAME server
                # is the easier mistake to make and the harder one to spot: the
                # two embeds interleave and each edit lands on whichever message
                # that loop last created, so the channel looks like it is
                # flickering between two unrelated panels.
                own: dict = {}
                for key in self.PER_SERVER_CHANNEL_KEYS:
                    cid = cfg.get(key)
                    if not cid:
                        continue
                    try:
                        cid = int(cid)
                    except (TypeError, ValueError):
                        self.log.warning(f"FowlEngine: {server.name}: {key} is not a "
                                         f"channel id: {cfg.get(key)!r}")
                        continue
                    own.setdefault(cid, []).append(key)
                    seen.setdefault((key, cid), []).append(server.name)
                for cid, keys in own.items():
                    if len(keys) > 1:
                        self.log.warning(
                            f"FowlEngine: {server.name} points {', '.join(keys)} at the same "
                            f"channel ({cid}). Those are separate feeds -- they will interleave "
                            f"and overwrite each other there. Give each one its own channel.")
            # Coalition roles are worse than a shared channel when two servers
            # share them: a pilot who is Blue on one and Red on the other has
            # the two syncs fighting, and the role flips every few minutes --
            # handing them the other faction's briefing half the time.
            role_seen: dict = {}
            for server in self.bot.servers.values():
                cr = (self.get_config(server) or {}).get('coalition_roles') or {}
                if not cr.get('manage'):
                    continue
                for side in ('blue', 'red'):
                    if cr.get(side):
                        role_seen.setdefault((side, str(cr[side])), []).append(server.name)
            for (side, spec), names in role_seen.items():
                if len(names) > 1:
                    self.log.warning(
                        f"FowlEngine: coalition role {spec!r} ({side}) is managed by "
                        f"{len(names)} servers ({', '.join(names)}). A pilot registered to "
                        f"opposite sides on two servers will have the role flip between "
                        f"syncs. Give each server its own coalition_roles pair.")
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
        await utils.safe_cancel(self.update_briefings)
        await utils.safe_cancel(self.sync_coalition_roles)
        await utils.safe_cancel(self.update_range_status)
        await utils.safe_cancel(self.poll_range_results)
        await utils.safe_cancel(self.post_news)
        await utils.safe_cancel(self.autoupdate_loop)
        await utils.safe_cancel(self.status_watch)
        await utils.safe_cancel(self.stage_apply_watch)
        await utils.safe_cancel(self.heartbeat_loop)
        if self.opsapi:
            try:
                self.opsapi.unregister()
            except Exception as ex:
                self.log.debug(f"FowlEngine: OPS API unregister: {ex}")
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

    # ── config reload (OPS page edits) + auto-update wiring ─────────────────

    def reload_plugin_config(self) -> None:
        """Re-read fowlengine.yaml after an edit (the dashboard OPS page) and
        push it into everything that caches it. bfdb itself only reads its
        flags / gci.json at start -- the caller restarts it if asked."""
        self.locals = self.read_locals()
        self._config.clear()
        self._instances_cache = None
        cfg = self.get_config() or {}
        self._bfdb_admin_password = (cfg.get("bfdb") or {}).get("admin_password")             or cfg.get("admin_password", "") or self._bfdb_admin_password
        if self.procman:
            self.procman.reload_config(cfg)
        if self.updater:
            self.updater.reload_config()
        if self.issues:
            self.issues.reload_config()
        self.sync_update_tuning()

    def sync_update_tuning(self) -> None:
        """procman runs bfdb's probation; its knobs live in `autoupdate:`."""
        if not (self.procman and self.updater):
            return
        c = self.updater.cfg
        self.procman.probation_minutes = c.probation_minutes
        self.procman.unhealthy_minutes = c.bfdb_unhealthy_minutes
        self.procman.db_snapshots_keep = c.db_snapshots_keep

    @tasks.loop(seconds=30.0)
    async def autoupdate_loop(self):
        """Release checks, apply policy, the engine probation watch, and the
        log analyzer's scan (which paces itself by issues.scan_seconds)."""
        if self.updater:
            await self.updater.tick()
        if self.issues:
            await self.issues.tick()

    @autoupdate_loop.before_loop
    async def before_autoupdate_loop(self):
        await self.bot.wait_until_ready()

    @tasks.loop(seconds=5.0)
    async def heartbeat_loop(self):
        """Ping heartbeat.url when one is set (it paces itself by
        heartbeat.interval_seconds)."""
        await self.heartbeat.tick()

    @heartbeat_loop.before_loop
    async def before_heartbeat_loop(self):
        await self.bot.wait_until_ready()

    @tasks.loop(seconds=1.0)
    async def status_watch(self):
        """Once a second, two cheap checks: the auto-updater's view of every
        server's status (so a scheduled restart's brief SHUTTING_DOWN isn't
        missed and mistaken for a crash), and Fowl Engine Manager's request
        for a clean shutdown."""
        if self.updater:
            try:
                self.updater.sample_status()
            except Exception as ex:
                self.log.debug(f"FowlEngine: status sample failed: {ex}")
        try:
            st = os.stat(SHUTDOWN_REQUEST_FILE)
        except OSError:
            return
        # Only a request made while this bot was running counts: one left
        # behind by a crash must not shut the next start straight down.
        if st.st_mtime < self._loaded_at - 5:
            try:
                os.remove(SHUTDOWN_REQUEST_FILE)
            except OSError:
                pass
            return
        try:
            os.remove(SHUTDOWN_REQUEST_FILE)
        except OSError:
            pass
        self.log.warning("FowlEngine: Fowl Engine Manager asked for a clean shutdown -- shutting the bot down")
        await self.bot.node.shutdown()

    @status_watch.before_loop
    async def before_status_watch(self):
        await self.bot.wait_until_ready()

    # ── staged DLL / bftools.exe: apply when empty, and catch-up after a crash ─

    def staged_apply_view(self, server_names: list, roles):
        """The buttons under a staged-binary reply (upload.py)."""
        return StagedApplyView(self, server_names, roles)

    def _stage_dir_of(self, server) -> str:
        b = self._engine_binaries(server)
        return b["staging_dir"] or (self.procman.staging_dir if self.procman else "")

    def _swap_names_of(self, server) -> list:
        """What BFBinaries swaps in at this server's DCS start: its engine DLL,
        and bftools.exe when it has a BFWeather `bftools` path."""
        names = [self._engine_binaries(server)["dll_name"]]
        if bftools_path_of(self, server):
            names.append(BFTOOLS)
        return names

    async def _pending_swaps(self, server, manual_only: bool = False) -> list:
        """[(name, pending path)] staged for this server's next DCS start.
        `manual_only` leaves out what the auto-updater staged (it applies its
        own on its own policy)."""
        sdir = self._stage_dir_of(server)
        if not sdir:
            return []
        out = []
        for name in self._swap_names_of(server):
            path = os.path.join(sdir, f"{name}.pending")
            if self._on_remote_node(server):
                try:
                    _, files = await server.node.list_directory(sdir, pattern=f"{name}.pending")
                except Exception:  # noqa: BLE001 - node down / dir missing
                    continue
                if files:
                    out.append((name, path))
                continue
            if not os.path.exists(path):
                continue
            if manual_only:
                try:
                    with open(path + ".json", encoding="utf-8") as fh:
                        if (json.load(fh) or {}).get("source") == "autoupdate":
                            continue
                except (OSError, ValueError):
                    pass
            out.append((name, path))
        return out

    def _stage_empty_minutes(self) -> float:
        cfg = (self.get_config() or {}).get("stage_apply") or {}
        try:
            return max(0.0, float(cfg.get("empty_minutes", 3)))
        except (TypeError, ValueError):
            return 3.0

    def schedule_apply_when_empty(self, server_names: list, by: str, why: str = "") -> str:
        """Remember that these servers should restart DCS to swap in what is
        staged as soon as nobody is on them. Kept in the plugin state."""
        now = datetime.now(timezone.utc).isoformat()
        for name in server_names:
            self.stage_apply_when_empty[name] = {"by": by, "utc": now, "attempts": 0, "why": why}
            self._stage_empty_since.pop(name, None)
        self.save_state()
        mins = self._stage_empty_minutes()
        return (f"{icon('pending')} **{', '.join(server_names)}**: DCS restarts to apply the staged build once "
                f"it has been empty for {mins:g} min (a server that is down takes it on its next start).")

    async def restart_to_apply(self, server_names: list, by: str) -> str:
        """Restart DCS now on each named server (BFBinaries swaps on the way up)."""
        out = []
        for name in server_names:
            server = self.bot.servers.get(name)
            if server is None:
                out.append(f"{name}: not known to the bot.")
                continue
            if server.status not in (Status.RUNNING, Status.PAUSED):
                out.append(f"{name}: is `{server.status.name}` -- it swaps the build in on its next start.")
                continue
            self.stage_apply_when_empty.pop(name, None)
            self.save_state()
            asyncio.create_task(self._restart_for_stage(server, f"applying a staged build (asked by {by})"))
            out.append(f"{icon('restart')} {name}: restarting DCS now.")
        return "\n".join(out) or "Nothing to restart."

    async def _restart_for_stage(self, server, why: str) -> None:
        self._stage_restarting[server.name] = time.time()
        try:
            if self.updater:
                # records it as an orderly restart, so it is never taken for a crash
                await self.updater.restart_dcs(server, why)
            else:
                await self.notify_ops(f"{icon('restart')} **{server.name}**: restarting DCS -- {why}.")
                await server.shutdown()
                await server.startup()
        except Exception as ex:  # noqa: BLE001
            self.log.error(f"FowlEngine: restart of {server.name} to apply a staged build failed: {ex}")
        finally:
            self._stage_restarting.pop(server.name, None)

    def _sweep_part_files(self) -> None:
        """A crash mid-download leaves `<name>.pending.part` behind; it never
        swaps in (only a complete .pending does) but it would sit there."""
        dirs = set()
        if self.procman and self.procman.staging_dir:
            dirs.add(self.procman.staging_dir)
        for server in self.bot.servers.values():
            if self._on_remote_node(server):
                continue
            try:
                d = self._stage_dir_of(server)
            except Exception:  # noqa: BLE001
                continue
            if d:
                dirs.add(d)
        for d in dirs:
            try:
                names = os.listdir(d)
            except OSError:
                continue
            for f in names:
                if f.endswith(".pending.part"):
                    try:
                        os.remove(os.path.join(d, f))
                        self.log.info(f"FowlEngine: removed an interrupted upload {os.path.join(d, f)}")
                    except OSError:
                        pass

    async def _check_missed_stage(self, server) -> None:
        """DCS is up, but on a start that should have swapped a staged build in
        and didn't -- the box came back from a crash and DCS was started before
        the swap could run, or by something other than the bot. The file is
        older than the DCS process, so that start missed it: queue it to apply
        when the server is empty instead of leaving it for a restart nobody
        knows is needed."""
        if server.name in self.stage_apply_when_empty or self._on_remote_node(server):
            return
        proc = getattr(server, "process", None)
        try:
            started = float(proc.create_time()) if proc else None
        except Exception:  # noqa: BLE001 - process gone / no access
            started = None
        if not started:
            return
        key = (server.name, int(started))
        if key in self._stage_missed_seen:
            return
        self._stage_missed_seen.add(key)
        if not self._engine_binaries(server).get("has_extension"):
            return  # nothing would swap it at a restart either
        missed = []
        for name, path in await self._pending_swaps(server, manual_only=True):
            try:
                if os.path.getmtime(path) < started - 5:
                    missed.append(name)
            except OSError:
                continue
        if not missed:
            return
        self.schedule_apply_when_empty([server.name], "crash recovery", why="missed at the last DCS start")
        await self.notify_ops(
            f"{icon('warning')} **{server.name}**: {', '.join(f'`{n}`' for n in missed)} was staged before this "
            f"DCS start but did not get swapped in (the box or the bot went down in between?). It applies "
            f"automatically once the server has been empty for {self._stage_empty_minutes():g} min -- "
            f"`/feops stage_cancel` to drop it.")

    @tasks.loop(seconds=30.0)
    async def stage_apply_watch(self):
        """Runs "apply when empty" for staged DLLs / bftools.exe, and spots a
        staged build a DCS start missed (see _check_missed_stage)."""
        if not self._stage_parts_swept:
            self._stage_parts_swept = True
            try:
                self._sweep_part_files()
            except Exception as ex:  # noqa: BLE001
                self.log.debug(f"FowlEngine: .part sweep failed: {ex}")
        for server in list(self.bot.servers.values()):
            if server.status in (Status.RUNNING, Status.PAUSED):
                try:
                    await self._check_missed_stage(server)
                except Exception as ex:  # noqa: BLE001 - one server never stops the rest
                    self.log.debug(f"FowlEngine: {server.name}: missed-stage check failed: {ex}")
        now = time.time()
        wait = self._stage_empty_minutes() * 60
        for name, ent in list(self.stage_apply_when_empty.items()):
            server = self.bot.servers.get(name)
            if server is None:
                continue  # not registered (yet) -- keep the ask
            try:
                pending = await self._pending_swaps(server)
            except Exception as ex:  # noqa: BLE001
                self.log.debug(f"FowlEngine: {name}: staged lookup failed: {ex}")
                continue
            if not pending:
                # swapped in (by our restart, a scheduled one, or a manual
                # start) or discarded -- either way, done
                self.stage_apply_when_empty.pop(name, None)
                self._stage_empty_since.pop(name, None)
                self.save_state()
                if ent.get("attempts"):
                    await self.notify_ops(f"{icon('good')} **{name}**: staged build applied.")
                continue
            if name in self._stage_restarting:
                continue
            if server.status not in (Status.RUNNING, Status.PAUSED):
                self._stage_empty_since.pop(name, None)
                continue  # down: BFBinaries swaps it in on its next start
            populated = False
            try:
                populated = bool(server.is_populated())
            except Exception:  # noqa: BLE001
                pass
            if populated:
                self._stage_empty_since.pop(name, None)
                continue
            since = self._stage_empty_since.setdefault(name, now)
            if now - since < wait:
                continue
            if int(ent.get("attempts") or 0) >= 2:
                self.stage_apply_when_empty.pop(name, None)
                self.save_state()
                await self.notify_ops(
                    f"{icon('warning')} **{name}**: {', '.join(f'`{n}`' for n, _ in pending)} is still staged after "
                    f"two restarts -- giving up on applying it when empty. Check the BFBinaries lines in the "
                    f"DCSServerBot log, then `/feops stage_apply` or `/feops stage_cancel`.")
                continue
            ent["attempts"] = int(ent.get("attempts") or 0) + 1
            self.save_state()
            self._stage_empty_since.pop(name, None)
            what = ", ".join(n for n, _ in pending)
            asyncio.create_task(self._restart_for_stage(
                server, f"server empty, applying staged {what} (asked by {ent.get('by', '?')})"))

    @stage_apply_watch.before_loop
    async def before_stage_apply_watch(self):
        await self.bot.wait_until_ready()

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
        # Written to a temp file and swapped in: the bot can be killed at any
        # moment (a service stop, a reboot), and a half-written state file
        # loses every message id -- the next start then posts a new copy of
        # every embed.
        try:
            tmp = self.state_file + '.tmp'
            with open(tmp, 'w') as f:
                json.dump({
                    'status_msg_ids': self.status_msg_ids,
                    'perf_msg_ids': self.perf_msg_ids,
                    'info_msg_ids': self.info_msg_ids,
                    'tail_msg_ids': self.tail_msg_ids,
                    'briefing_msg_ids': self.briefing_msg_ids,
                    'faction_thread_ids': self.faction_thread_ids,
                    'range_status_msg_ids': self.range_status_msg_ids,
                    'range_feed_cursors': self.range_feed_cursors,
                    'news_feed_state': self.news_feed_state,
                    'rotation_index': self.rotation_index,
                    'round_result_state': self.round_result_state,
                    'stage_apply_when_empty': self.stage_apply_when_empty,
                }, f)
                f.flush()
                os.fsync(f.fileno())
            os.replace(tmp, self.state_file)
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
            if self._is_range(server):
                # "Campaign Status" has nothing to say about a training range
                # (no rounds, no objectives); it has range_status_channel.
                continue

            try:
                import aiohttp
                config = self.get_config(server) or {}
                api_url = config.get("api_url", "http://localhost:8880")
                dash = (config.get("dashboard_url") or "").rstrip("/")
                async with aiohttp.ClientSession() as session:
                    sp = srv_params(server.name)
                    async with session.get(f"{api_url}/api/stats", params=sp, timeout=10) as resp:
                        if resp.status != 200:
                            continue
                        stats = await resp.json()
                    async with session.get(f"{api_url}/api/objectives", params=sp, timeout=10) as resp:
                        objs = await resp.json() if resp.status == 200 else []
                    captures = []
                    try:
                        async with session.get(f"{api_url}/api/capture-events",
                                               params=srv_params(server.name, limit=3)) as resp:
                            captures = await resp.json() if resp.status == 200 else []
                    except (aiohttp.ClientError, ValueError):
                        pass

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
                                       url=f"{dash}/map" if dash else None, server=server)

                active_round = stats.get('active_round')
                if active_round:
                    # The instance label ("Modern - Syria") reads better than
                    # the round's scenario id, which on some servers is just
                    # "campaign"; the id stays when it says something extra.
                    label = ((self._instance_entry(server) or {}).get('label') or "").strip()
                    scenario = str(active_round.get('scenario') or "").strip()
                    if label:
                        desc = f"**Campaign:** {label}"
                        if scenario and scenario.lower() not in ('campaign', label.lower()):
                            desc += f" · {scenario}"
                    else:
                        desc = f"**Scenario:** {scenario or 'Unknown'}"
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
                    name=f"{icon('side_blue')} Blue",
                    value=f"{stats.get('blue_online', 0)} online · {blue_objs} obj",
                    inline=True,
                )
                embed.add_field(
                    name=f"{icon('side_red')} Red",
                    value=f"{stats.get('red_online', 0)} online · {red_objs} obj",
                    inline=True,
                )
                embed.add_field(name=f"{icon('unowned')} Neutral", value=f"{neutral_objs} obj", inline=True)

                # The engine's own flags when bfdb reached it this poll; the
                # old health guess only when it didn't (persisted snapshot).
                live = any(o.get('live') for o in objs)
                sided = [o for o in objs if o.get('owner') in ('Blue', 'Red')]
                if live:
                    ready_objs = [o for o in sided if o.get('captureable')]
                    attacked = [o for o in sided if o.get('threatened') and not o.get('captureable')]
                else:
                    ready_objs = [o for o in sided if o.get('health', 100) <= 20]
                    attacked = []
                if attacked:
                    embed.add_field(name=f"{icon('threat')} Under attack",
                                    value=self._obj_list(attacked), inline=False)
                if ready_objs:
                    embed.add_field(name=f"{icon('pending')} Ready to capture",
                                    value=self._obj_list(ready_objs), inline=False)

                recent = self._recent_captures(captures)
                if recent:
                    embed.add_field(name=f"{icon('captured')} Latest captures", value=recent, inline=False)

                wx = self._weather_line(stats.get('weather'))
                if wx:
                    embed.add_field(name=f"{icon('weather')} Conditions", value=wx, inline=False)

                priority_objs = [o.get('name') for o in objs if o.get('priority')]
                if priority_objs:
                    embed.add_field(name=f"{icon('priority')} Priority", value=", ".join(priority_objs[:5]), inline=False)

                restart_at = stats.get('restart_at')
                if restart_at:
                    try:
                        restart_ts = int(self._parse_iso(restart_at).timestamp())
                        embed.add_field(name=f"{icon('rotation')} Next rotation", value=f"<t:{restart_ts}:R>", inline=False)
                    except ValueError:
                        pass
                if dash:
                    embed.add_field(name="​", value=f"**[Open the live map ›]({dash}/map)**", inline=False)
                channel_id = int(config['status_channel'])
                channel = self.bot.get_channel(channel_id)
                if not channel:
                    self.log.error(f"FowlEngine: status_channel {channel_id} not found or bot lacks access.")
                    continue
                    
                png, upload = await self._status_map(server, config, api_url, objs)
                if png:
                    embed.set_image(url=f"attachment://{STATUS_MAP_FILE}")
                delivered = await self._upsert_embed(self.status_msg_ids, server.name, channel, embed,
                                                     "status embed", png=png, png_name=STATUS_MAP_FILE,
                                                     refresh_png=upload, carries_image=True)
                if png and upload and delivered:
                    self._mapsnap[server.name]["uploaded"] = True
                
            except Exception as ex:
                if self._bfdb_failed(api_url, f"status embed for {server.name}", ex):
                    continue
                import traceback
                self.log.error(f"Error updating Fowl Engine status: {ex}\n{traceback.format_exc()}")

    @update_status.before_loop
    async def before_update_status(self):
        await self.bot.wait_until_ready()

    @staticmethod
    def _obj_list(objs: list, cap: int = 5) -> str:
        names = ", ".join(f"{o['name']} ({o['owner']})" for o in objs[:cap])
        if len(objs) > cap:
            names += f" +{len(objs) - cap}"
        return names

    def _recent_captures(self, captures) -> str:
        """The last few ownership changes, newest first, with Discord relative
        timestamps: "Hama -> Red by Shade <t:..:R>"."""
        lines = []
        for c in (captures if isinstance(captures, list) else [])[:3]:
            name, side = c.get('objective'), c.get('side')
            if not name or side not in ('Blue', 'Red', 'Neutral'):
                continue
            when = ""
            try:
                when = f" <t:{int(self._parse_iso(c['time']).timestamp())}:R>"
            except (KeyError, TypeError, ValueError):
                pass
            side_icon = icon({'Blue': 'side_blue', 'Red': 'side_red'}.get(side, 'unowned'))
            # An all-zero UCID is bfdb's "no pilot" (AI troops), not a name.
            by = [str(b) for b in (c.get('by') or []) if b and not re.fullmatch(r"0+", str(b))][:2]
            who = f" by {discord.utils.escape_markdown(', '.join(by))}" if by else ""
            lines.append(f"{side_icon} **{name}** \u2192 {side}{who}{when}")
        return "\n".join(lines)

    @staticmethod
    def _weather_line(w) -> str:
        """One line of the mission weather from /api/stats: wind, temperature,
        cloud base, visibility -- whichever of them bfdb has."""
        if not isinstance(w, dict):
            return ""
        parts = []
        if w.get('wind_speed_kts') is not None:
            spd = round(w['wind_speed_kts'])
            if spd < 1:
                parts.append("Wind calm")
            elif w.get('wind_from_deg') is not None:
                parts.append(f"Wind {round(w['wind_from_deg']) % 360:03d}\u00b0 / {spd} kt")
            else:
                parts.append(f"Wind {spd} kt")
        if w.get('temp_c') is not None:
            parts.append(f"{round(w['temp_c'])}\u00b0C")
        density = w.get('cloud_density')
        if w.get('cloud_base_m') is not None and (density or 0) > 0:
            parts.append(f"Clouds {round(w['cloud_base_m'] * 3.28084 / 100) * 100:,} ft")
        elif density is not None and not density:
            parts.append("Clear")
        if w.get('visibility_m') is not None and w['visibility_m'] < 9000:
            parts.append(f"Vis {w['visibility_m'] / 1000:.1f} km")
        return " \u00b7 ".join(parts)

    async def _status_map(self, server, config: dict, api_url: str, objs: list) -> tuple:
        """(png, upload) for the status embed. `png` is the latest map snapshot
        (None when disabled or never captured); `upload` says the message
        doesn't carry it yet -- captured this tick, or an earlier upload
        failed -- so it must be (re)sent. update_status sets "uploaded" once
        Discord has taken it.

        Captured only when the picture would change -- owner, health band or
        contested flags (mapsnap.map_signature) -- at most every
        status_map_min_gap_secs, plus a refresh every status_map_minutes, so
        the one-minute text tick doesn't re-upload an identical image."""
        if not config.get('status_map', True) or not objs:
            return None, False
        browser = mapsnap.find_browser(config.get('status_map_browser'))
        if not browser:
            self._warn_once(server.name, "mapsnap-browser",
                            "FowlEngine: no Edge/Chrome found for the status-embed map snapshot -- "
                            "set status_map_browser, or status_map: false to silence this")
            return None, False
        st = self._mapsnap.setdefault(server.name, {"sig": None, "at": 0.0, "png": None, "failed_at": 0.0,
                                                    "uploaded": False})
        pending = st["png"] is not None and not st["uploaded"]
        now = time.monotonic()
        sig = mapsnap.map_signature(objs)
        min_gap = float(config.get('status_map_min_gap_secs', 120))
        refresh = float(config.get('status_map_minutes', 10)) * 60
        due = (st["png"] is None
               or (sig != st["sig"] and now - st["at"] >= min_gap)
               or now - st["at"] >= refresh)
        if not due or (st["failed_at"] and now - st["failed_at"] < min_gap):
            return st["png"], pending
        base = (config.get('status_map_url') or api_url).rstrip('/')
        profile = os.path.join(self.bot.node.config_dir, 'fowlengine_mapsnap',
                               re.sub(r'[^A-Za-z0-9_.-]+', '_', server.name))
        try:
            png = await mapsnap.capture(browser, mapsnap.snapshot_url(base, server.name), profile,
                                        size=mapsnap.parse_size(config.get('status_map_size')), log=self.log)
        except asyncio.CancelledError:
            raise
        except Exception as ex:
            st["failed_at"] = now
            self.log.warning(f"FowlEngine: status map snapshot for {server.name} failed: "
                             f"{type(ex).__name__}: {ex or '(no message)'}")
            return st["png"], pending
        if not png:
            st["failed_at"] = now
            return st["png"], pending
        st.update(sig=sig, at=now, png=png, failed_at=0.0, uploaded=False)
        if not st.get("logged_first"):
            st["logged_first"] = True
            self.log.info(f"FowlEngine: status map snapshot for {server.name} captured "
                          f"({len(png) // 1024} KB, {browser})")
        return png, True

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

    def _build_perf_embed(self, brand_name: str, data: dict, server=None) -> discord.Embed:
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

        embed = self._vs_embed("Server Performance", color=color, server=server)

        embed.add_field(
            name=f"{icon('cpu')} CPU",
            value=f"{self._bar(cpu_pct)} {cpu_pct:.0f}%\n{hw.get('cpu_count', '?')} logical cores",
            inline=True,
        )
        embed.add_field(
            name=f"{icon('memory')} RAM",
            value=f"{self._bar(mem_pct)} {mem_pct:.0f}%\n{self._fmt_bytes(mem_used)} / {self._fmt_bytes(mem_total)}",
            inline=True,
        )
        if gpu.get('available'):
            gpu_pct = gpu.get('usage_pct', 0)
            gpu_mem_total = gpu.get('mem_total_bytes', 0)
            gpu_mem_used = gpu.get('mem_used_bytes', 0)
            gpu_temp = gpu.get('celsius')
            temp_str = f" · {icon('temp')} {gpu_temp}°C" if gpu_temp is not None else ""
            embed.add_field(
                name=f"{icon('gpu')} GPU ({gpu.get('name', 'Unknown')})",
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
            embed.add_field(name=f"{icon('disk')} Disk", value="\n".join(lines), inline=False)

        temps = hw.get('temps', [])
        cpu_temps = [t for t in temps if 'cpu' in t.get('label', '').lower() or 'package' in t.get('label', '').lower()]
        if cpu_temps:
            embed.add_field(
                name=f"{icon('temp')} CPU Temp",
                value="\n".join(f"{t['label']}: {t['celsius']:.0f}°C" for t in cpu_temps[:3]),
                inline=True,
            )

        if data.get('available'):
            engine_by_name = {row['name']: row for row in data.get('engine', [])}
            frame = engine_by_name.get('frame')
            if frame:
                mean_us = frame.get('mean', 0)
                flag = f"{icon('critical')} " if mean_us >= 25000 else (f"{icon('caution')} " if mean_us >= 20000 else "")
                embed.add_field(
                    name=f"{flag}{icon('perf')} Mission Frame Time",
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
                embed.add_field(name=f"{icon('inspect')} Script Breakdown (mean)", value="\n".join(watch_lines), inline=False)
        else:
            embed.add_field(name=f"{icon('perf')} Mission Status", value="No active DCS session reporting yet.", inline=False)

        embed.add_field(name=f"{icon('build')} Deploy status", value=self._deploy_status_line(), inline=False)
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
            return f"{icon('warning')} {(b or {}).get('error', 'unknown')}"
        return f"`{b.get('git', '?')}` · {b.get('built', '?')}"

    def _engine_binaries(self, server) -> dict:
        """{dll_name, dll_path, staging_dir, ...} for this server's engine DLL
        (bflib.dll, or bfrange.dll on a range server) -- the same resolution
        upload.py stages by and the BFBinaries extension swaps by."""
        return engine_binaries(self, server)

    def _bflib_dll_path(self, server) -> str:
        """Resolve the live engine DLL path (bfrange.dll on a range server):
        the BFBinaries extension, then the legacy plugin keys, then
        <instance home>/Scripts/<dll>."""
        return self._engine_binaries(server).get("dll_path", "")

    def _staged_for(self, server) -> list[str]:
        """What is waiting to be swapped in for this server: its own engine
        DLL (in its own staging dir) and the shared bfdb.exe."""
        if not self.procman:
            return []
        out = []
        try:
            # a server on another PC stages on that PC; /feops stage_status asks it
            b = self._engine_binaries(server) if server and not self._on_remote_node(server) else None
            if b and self.procman.pending_info(b["dll_name"], b["staging_dir"] or None):
                out.append(b["dll_name"])
            if b and bftools_path_of(self, server) and self.procman.pending_info(BFTOOLS, b["staging_dir"] or None):
                out.append(BFTOOLS)
        except Exception as ex:  # noqa: BLE001
            self.log.debug(f"FowlEngine: staged-DLL lookup failed: {ex}")
        if self.procman.pending_info("bfdb.exe"):
            out.append("bfdb.exe")
        return out

    def _deploy_status_line(self, server=None) -> str:
        """Engine build summary for an embed field: git rev + build time of the
        RUNNING bfdb / bflib / bftools, plus anything staged for next restart."""
        srv = server or next(iter(self.bot.servers.values()), None)
        builds = self._engine_builds(srv) if srv else {}
        lines = [f"**{n}** {self._fmt_build(builds.get(n) or {})}"
                 for n in ("bfdb", "bflib", "bftools")]
        if self.procman and self.procman.enabled:
            staged = self._staged_for(srv)
            if staged:
                lines.append(f"{icon('pending')} staged: " + ", ".join(f"`{s}`" for s in staged)
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

            api_url = config.get("api_url", "http://localhost:8880")
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

                brand_name = self._branding(server, config)['brand_name']
                embed = self._build_perf_embed(brand_name, data, server=server)

                channel_id = int(config['perf_channel'])
                channel = self.bot.get_channel(channel_id)
                if not channel:
                    self.log.error(f"FowlEngine: perf_channel {channel_id} not found or bot lacks access.")
                    continue

                await self._upsert_embed(self.perf_msg_ids, server.name, channel, embed, "performance embed")

            except Exception as ex:
                if self._bfdb_failed(api_url, f"perf embed for {server.name}", ex):
                    continue
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
                # Same merge procman renders gci.<id>.json with -- including
                # "a kind: range instance is off unless it says otherwise".
                return effective_instance_gci(base, inst)
        return base

    def _gci_freq_lines(self, server=None) -> list[str]:
        """Human-readable GCI frequency briefing from the `gci:` YAML block."""
        g = self._gci_cfg(server)
        if not g.get('enabled'):
            return ["GCI is currently **off**."]
        mod = str(g.get('modulation') or 'AM').upper()
        blue_cs = g.get('blue_controller_callsign', 'Magic')
        red_cs = g.get('red_controller_callsign', 'Overlord')

        def freqs(multi: str, single: str, default: float) -> str:
            # What bfdb actually transmits on (voice::radios_from): a
            # `*_freqs` list replaces the single `*_freq_mhz` outright, and an
            # entry without a modulation is FM below 108 MHz, AM above. This
            # used to print only the single value -- live, Red's 252.0 while
            # the controller was really on 124.0 / 228.0 / 40.0.
            out = []
            for f in g.get(multi) or []:
                try:
                    mhz = float(f.get('mhz'))
                except (AttributeError, TypeError, ValueError):
                    continue
                m = str(f.get('modulation') or ('FM' if mhz < 108.0 else 'AM')).upper()
                out.append(f"`{mhz:.3f} {m}`")
            if out:
                return " · ".join(out)
            try:
                return f"`{float(g.get(single, default)):.3f} {mod}`"
            except (TypeError, ValueError):
                return f"`{default:.3f} {mod}`"

        lines = [
            f"{icon('blue')} **Blue** ({blue_cs}) — {freqs('blue_freqs', 'blue_freq_mhz', 251.0)}",
            f"{icon('red')} **Red** ({red_cs}) — {freqs('red_freqs', 'red_freq_mhz', 252.0)}",
        ]
        if g.get('whisper_exe'):
            lines.append(f"Key up and call it: *\"{blue_cs}, bogey dope\"* — also picture, declare, "
                         "snaplock, alpha check.")
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
        embed = self._vs_embed(title, color=discord.Color.teal(), server=server)
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
            if self._is_range(server):
                continue  # no campaign picture, no controller to transcribe
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
        api_url = config.get("api_url", "http://localhost:8880")
        username, password = config.get("admin_username"), config.get("admin_password")
        # bfdb can post the transcript straight to a Discord webhook, and this
        # relay posts the same calls through the bot. Both on means every call
        # appears twice, which reads as the bot duplicating itself.
        gci_cfg = config.get("gci") or {}
        dupes = [k for k in ("discord_webhook_url", "blue_discord_webhook_url",
                             "red_discord_webhook_url") if (gci_cfg.get(k) or "").strip()]
        if dupes:
            self.log.warning(
                f"FowlEngine: {server.name} has gci_transcript_channel AND gci.{dupes[0]} set -- "
                f"every GCI call will be posted twice. Clear the webhook(s) "
                f"({', '.join(dupes)}) to keep the bot relay as the only transcript."
            )
        cid = config['gci_transcript_channel']
        channel = self.bot.get_channel(int(cid))
        if not channel:
            # The supervisor relaunches this every 15 s: say it once, not forever.
            self._warn_once(server.name, f"gci_channel:{cid}",
                            f"FowlEngine: gci_transcript_channel {cid} for {server.name} doesn't exist or the bot "
                            f"can't see it (wrong id, or no View Channel permission) -- fix the id in fowlengine.yaml "
                            f"or set it to null. No GCI transcript is posted for this server until then.")
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
                        self._bfdb_ok(api_url)
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
                            mark = icon("red" if side == "red" else "blue")
                            try:
                                await channel.send(f"{mark} {text}", allowed_mentions=NO_PINGS)
                            except discord.HTTPException as ex:
                                self.log.error(f"FowlEngine: GCI transcript post failed: {ex}")
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                if not self._bfdb_failed(api_url, f"GCI transcript relay for {server.name}", ex):
                    self.log.error(f"FowlEngine: GCI transcript relay error for {server.name}: "
                                   f"{type(ex).__name__}: {ex or '(no message)'}")
            await asyncio.sleep(ENGINE_LOG_RECONNECT_SECS)

    # ── GCI & links embed (complements DCSServerBot's server-status embed) ──

    async def _fetch_wiki_facts(self, server, config: dict) -> dict:
        """The instance's public cfg facts (bfdb `GET /api/wiki/facts`, no
        auth): the static comms plan and the campaign rules. {} when bfdb
        is unreachable or has no engine_config for this instance."""
        import aiohttp
        api_url = config.get("api_url", "http://localhost:8880")
        try:
            async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=10)) as http:
                async with http.get(f"{api_url}/api/wiki/facts", params=srv_params(server.name)) as resp:
                    if resp.status != 200:
                        return {}
                    body = await resp.json(content_type=None)
        except asyncio.CancelledError:
            raise
        except Exception as ex:
            self.log.debug(f"FowlEngine: wiki facts for {server.name} unavailable: {ex}")
            return {}
        facts = body.get("facts") if isinstance(body, dict) else None
        return facts if isinstance(facts, dict) else {}

    def _build_server_info_embed(self, server, facts: dict | None = None) -> discord.Embed:
        """Only what DCSServerBot's own server-status embed lacks: the GCI
        frequencies, each side's planned comms channels, the campaign rules
        and the links. Connect address, slots, mission time, restart,
        weather, SRS and Tacview are all in the bot's embed -- repeating them
        here just made two embeds to keep in sync. Engine builds are ops
        detail: the perf embed and /feops versions have them.

        Comms and rules come from the public cfg facts and are STATIC: the
        planned channels (the wiki publishes the same tables), never the live
        overlay -- which AWACS/tanker is really up, TACANs, laser codes --
        that the per-side briefing channels carry."""
        up = server.status in (Status.RUNNING, Status.PAUSED)
        config = self.get_config(server) or {}
        facts = facts or {}
        wiki = (config.get('wiki_url') or "").strip()
        title = "Comms, Rules & Links"
        if sum(1 for s in self.bot.servers.values() if (self.get_config(s) or {}).get('server_info_channel')) > 1:
            title += f" — {server.name}"
        embed = self._vs_embed(title, color=discord.Color.green() if up else discord.Color.dark_grey(),
                               server=server)
        embed.add_field(name=f"{icon('air')} GCI / AWACS", value="\n".join(self._gci_freq_lines(server)), inline=False)
        plan = facts.get('comms_plan')
        hint = "full plan in the wiki" if wiki else ""
        for side, key in (("Blue", "side_blue"), ("Red", "side_red")):
            block = serverinfo.comms_plan_block(plan, side, hint)
            if block:
                embed.add_field(name=f"{icon(key)} {side} comms plan", value=block, inline=False)
        rules = serverinfo.rules_lines(facts)
        if rules:
            embed.add_field(name=f"{icon('rules')} Rules", value="\n".join(rules)[:serverinfo.FIELD_LIMIT],
                            inline=False)
        links = []
        dash = (config.get('dashboard_url') or "").rstrip('/')
        if dash:
            # /map is the fog-of-war tacmap: a visitor who isn't signed in
            # gets a "not signed in" card, so say so up front.
            links += [f"[Dashboard]({dash})", f"[Tactical map]({dash}/map) (sign in)"]
        if wiki:
            links.append(f"[Wiki]({wiki})")
        if links:
            embed.add_field(name=f"{icon('url')} Links", value=" · ".join(links), inline=False)
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
                facts = await self._fetch_wiki_facts(server, config)
                embed = self._build_server_info_embed(server, facts)
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                self.log.error(f"FowlEngine: failed to build server-info embed: {ex}")
                continue
            try:
                await self._upsert_embed(self.info_msg_ids, server.name, channel, embed, "server-info embed")
            except Exception as ex:
                self.log.error(f"FowlEngine: failed to post the server-info embed: {ex}")

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
            await channel.send(message, allowed_mentions=NO_PINGS)
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
            if self._is_range(server):
                continue  # ranks are earned in the campaign, not on the range

            rank_thresholds = config['rank_thresholds']
            if not rank_thresholds:
                continue

            # Sort thresholds descending by points
            sorted_ranks = sorted(rank_thresholds.items(), key=lambda x: x[1], reverse=True)

            api_url = config.get("api_url", "http://localhost:8880")
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

        api_url = config.get("api_url", "http://localhost:8880")
        # A Discord join isn't tied to a DCS server, so with several of them
        # behind one bfdb the briefing needs to name which one it's about.
        # Unset -> bfdb's default instance, i.e. the old single-server result.
        # The welcome then carries that server's branding too.
        welcome_server = config.get('welcome_server') or None
        brand_name = self._branding(welcome_server)['brand_name']
        sp = srv_params(welcome_server)
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

        embed = self._vs_embed(f"Welcome to {brand_name}", color=discord.Color.gold(), server=welcome_server)

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
                value=f"{icon('side_blue')} {blue_objs} · {icon('unowned')} {neutral_objs} · {icon('side_red')} {red_objs}",
                inline=False,
            )

        briefing = config.get(
            'welcome_briefing',
            "Read the rules, pick your faction on the dashboard, and check the live map. "
            "`/fe_dashboard` for your secure login · `/fe_gci` for AWACS frequencies.",
        )
        embed.add_field(name=f"{icon('briefing')} Briefing", value=briefing, inline=False)

        if self._gci_cfg().get('enabled'):
            embed.add_field(name=f"{icon('air')} GCI", value="\n".join(self._gci_freq_lines()[:2]), inline=False)

        dashboard_url = config.get("dashboard_url")
        if dashboard_url:
            embed.add_field(name=f"{icon('url')} Dashboard", value=f"**[Open Dashboard ›]({dashboard_url})**", inline=False)

        embed.set_thumbnail(url=member.display_avatar.url)

        welcome_message = config.get('welcome_message', "Welcome aboard, {mention}!")
        try:
            await channel.send(content=render_icons(welcome_message).format(mention=member.mention, brand_name=brand_name),
                               embed=embed)
        except discord.HTTPException as ex:
            self.log.error(f"FowlEngine: failed to send welcome message: {ex}")

    # ── shared embed builder ────────────────────────────────────────────────

    # ── per-instance branding ───────────────────────────────────────────────

    def _bfdb_instance_row(self, server_name: str | None, iid: str | None, api_url: str | None) -> dict:
        """This server's row of bfdb's /api/instances, from the last fetch
        (_refresh_bfdb_instances), or {}."""
        cached = self._bfdb_instance_rows.get((api_url or '').rstrip('/'))
        rows = cached[1] if cached else []
        for key, val in (('id', iid), ('dcs_server_name', server_name)):
            if val:
                for r in rows:
                    if isinstance(r, dict) and r.get(key) == val:
                        return r
        return {}

    async def _refresh_bfdb_instances(self, http, api_url: str, data=None) -> None:
        """Keep the /api/instances rows behind _branding fresh (every
        branding.REFRESH_SECS at most). `data` is a reply the caller already has.
        Best effort: on failure the last rows (or none) stay."""
        key = (api_url or '').rstrip('/')
        if data is None:
            cached = self._bfdb_instance_rows.get(key)
            if cached and time.time() - cached[0] < branding.REFRESH_SECS:
                return
            try:
                async with http.get(f"{key}/api/instances", timeout=10) as resp:
                    if resp.status != 200:
                        return
                    data = await resp.json(content_type=None)
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                self.log.debug(f"FowlEngine: /api/instances for branding: {type(ex).__name__}: {ex}")
                return
        rows = data.get('instances') if isinstance(data, dict) else data
        if isinstance(rows, list):
            self._bfdb_instance_rows[key] = (time.time(), rows)

    def _branding(self, server=None, config: dict | None = None) -> dict:
        """branding.resolve for one DCS server (a Server or its name), or the
        global branding with none. The instance is its instances.json /
        bfdb.instances entry with bfdb's /api/instances row over it."""
        name = server if isinstance(server, str) else getattr(server, 'name', None)
        if isinstance(server, str):
            server = self.bot.servers.get(server)
        if config is None:
            config = (self.get_config(server) if server is not None else self.get_config()) or {}
        if not name:
            return branding.resolve(config)
        try:
            local = rangefeed.find_instance(self._instances_list(), name) or {}
            row = self._bfdb_instance_row(name, local.get('id'), config.get('api_url', 'http://localhost:8880'))
            inst = {**local, **{k: v for k, v in row.items() if v is not None}}
        except Exception as ex:  # branding must never stop a post
            self.log.debug(f"FowlEngine: branding lookup for {name} failed: {ex}")
            inst = {}
        return branding.resolve(config, inst, name)

    # ── shared embed builder ────────────────────────────────────────────────

    def _vs_embed(self, title: str, *, color: discord.Color | None = None,
                  url: str | None = None, server=None) -> discord.Embed:
        """Every FowlEngine embed goes through here for consistent branding:
        brand author line + logo, timestamped footer. With `server` (a Server
        or its name) the brand is that server's instance's (_branding: the
        Red Storm campaign posts as itself), without it the global one. The
        logo is pulled from the dashboard origin so it survives message edits
        (no attachment needed)."""
        srv = self.bot.servers.get(server) if isinstance(server, str) else server
        cfg = (self.get_config(srv) if srv is not None else self.get_config()) or {}
        b = self._branding(server, cfg)
        dash = (cfg.get('dashboard_url') or '').rstrip('/')
        embed = discord.Embed(title=title, color=color or discord.Color(b['colour_int']))
        if url:
            embed.url = url
        embed.set_author(name=b['brand_name'], icon_url=b['logo_url'], url=dash or None)
        embed.set_footer(text=b['brand_name'], icon_url=b['logo_url'])
        embed.timestamp = discord.utils.utcnow()
        return embed


async def setup(bot: DCSServerBot):
    await bot.add_cog(FowlEngine(bot))
