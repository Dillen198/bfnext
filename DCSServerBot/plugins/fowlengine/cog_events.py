"""EventsMixin: part of the FowlEngine cog (commands.py).

Live engine-log relay, capture / neutral / ready-to-capture and kill-streak
alerts, round results and the theatre rotation.

Moved out of commands.py unchanged. A mixin: it holds no state of its own and
relies on what FowlEngine.__init__ sets up; discord.py collects its commands,
groups, loops and listeners through the MRO."""
import discord
from core import utils, Server, Status, command
from discord import app_commands
from discord.ext import tasks
import time
import asyncio
from collections import deque
from typing import Optional
from .icons import icon, render as render_icons
from . import roundresult
from .common import (ACHIEVEMENT_THRESHOLDS, CAMPAIGN_POLL_SECS, CAMPAIGN_RECONNECT_SECS, ENGINE_LOG_ALERT_DEDUPE,
                     ENGINE_LOG_FLUSH_SECS, ENGINE_LOG_LEVEL_RE, ENGINE_LOG_RECONNECT_SECS, ENGINE_LOG_TAIL_LINES,
                     NO_PINGS, OWNER_CONFIRM_POLLS, WEAK_CLEAR_HEALTH, WEAK_HEALTH, bfdb_login, srv_params, srv_path)


class EventsMixin:
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
            if self._is_range(server):
                # objective alerts and kill streaks are campaign events; the
                # range's kills are training shots. Not in `active`, so a
                # poller left over from before the kind changed is cancelled.
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
        # What players call it: the id is a database key (0, then 108000656).
        self._round_number[server_name] = active_round.get('number')
        prev_rid = self._active_round.get(server_name)
        self._active_round[server_name] = rid
        # A new round means the last one just ended -- and a bot that has just
        # started may have missed the change -- so look for a result to post.
        if prev_rid != rid:
            try:
                await self._announce_round_result(session, api_url, server_name)
            except Exception as ex:
                self.log.warning(f"FowlEngine: round result for {server_name} failed: "
                                 f"{type(ex).__name__}: {ex or '(no message)'}")
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
            await self._announce_rotation(server_name, rid)

    async def _announce_round_result(self, session, api_url, server_name):
        """Post the rundown of the server's last won round, once per round.

        Goes to `round_channel` when set (one announcements channel can serve
        every server), and to `news_channel` and `alerts_channel` -- each
        once, wherever two of them are the same channel. See roundresult.py."""
        server = self.bot.servers.get(server_name)
        config = (self.get_config(server) or {}) if server else {}
        iid = self._instance_id(server) if server else None
        async with session.get(f"{api_url}/api/instances", timeout=10) as resp:
            if resp.status != 200:
                return
            instances = await resp.json(content_type=None)
        row = roundresult.find_instance(instances, iid, server_name)
        lr = (row or {}).get("last_round")
        if not lr or lr.get("id") is None:
            return
        iid = iid or (row or {}).get("id")
        announced = self.round_result_state.get(server_name)
        post = roundresult.should_announce(lr, announced, time.time())
        if not post and announced is not None:
            return
        # Recorded BEFORE posting, like the news feed: a Discord hiccup halfway
        # must never make the next round change post the same result again.
        self.round_result_state[server_name] = lr["id"]
        self.save_state()
        if not post:
            return
        params = {"instance": iid} if iid else srv_params(server_name)

        async def get(path):
            try:
                async with session.get(f"{api_url}{path}", params=params, timeout=10) as r:
                    return await r.json(content_type=None) if r.status == 200 else None
            except Exception:
                return None

        summary = await get(f"/api/rounds/{lr['id']}/summary")
        report = await get(f"/api/rounds/{lr['id']}/report")
        label = (row or {}).get("label") or server_name
        embed = self._embed_from_dict(roundresult.build_embed(
            lr, summary, report, config.get("dashboard_url"), iid, label))
        seen = set()
        for key in ("round_channel", "news_channel", "alerts_channel"):
            channel = self._channel_for(config, key, server_name)
            if channel is None or channel.id in seen:
                continue
            seen.add(channel.id)
            try:
                await channel.send(embed=embed, allowed_mentions=NO_PINGS)
            except discord.HTTPException as ex:
                self.log.error(f"FowlEngine: posting round {lr['id']} result for {server_name} "
                               f"to {channel.id} failed: {ex}")
        self.log.info(f"FowlEngine: announced round {lr.get('number') or lr['id']} "
                      f"({lr.get('winner')} won) for {server_name}")

    # ── theatre rotation ─────────────────────────────────────────────────
    # `rotation: {theatres: [...], channel: <id>}` per server. The bot keeps
    # the schedule, posts it at every new round and runs the vote; the switch
    # itself (the BFWeather base mission and the campaign config) stays an
    # admin's job, recorded with /fe_rotation_advance.

    def _rotation(self, server_name: str):
        server = self.bot.servers.get(server_name)
        cfg = (self.get_config(server) or {}) if server else {}
        rot = cfg.get('rotation') or {}
        theatres = [str(t) for t in (rot.get('theatres') or []) if str(t).strip()]
        return rot, theatres

    def _rotation_lines(self, server_name: str) -> list[str]:
        _, theatres = self._rotation(server_name)
        if not theatres:
            return []
        cur = self.rotation_index.get(server_name, 0) % len(theatres)
        lines = []
        for i in range(len(theatres)):
            t = theatres[(cur + i) % len(theatres)]
            lines.append(("**Now:** " if i == 0 else ("**Next:** " if i == 1 else f"{i + 1}. ")) + t)
        return lines

    async def _announce_rotation(self, server_name: str, rid):
        rot, theatres = self._rotation(server_name)
        if not theatres or not rot.get('channel'):
            return
        channel = self.bot.get_channel(int(rot['channel']))
        if not channel:
            return
        embed = self._vs_embed(f"Round {self._round_number.get(server_name) or rid} has started",
                               color=discord.Color.gold())
        embed.description = "\n".join(self._rotation_lines(server_name)) + \
            "\n\nVote on what comes next with `/fe_rotation_vote`."
        try:
            await channel.send(embed=embed)
        except Exception as ex:
            self.log.warning(f"FowlEngine: rotation announcement failed for {server_name}: {ex}")

    @command(description='Show the theatre rotation: what is flying now and what comes next.')
    @app_commands.guild_only()
    async def fe_rotation(self, interaction: discord.Interaction,
                          server: app_commands.Transform[Server, utils.ServerTransformer()]):
        lines = self._rotation_lines(server.name)
        if not lines:
            await interaction.response.send_message(
                f"No rotation is configured for {server.name}.", ephemeral=True)
            return
        embed = self._vs_embed(f"Theatre rotation -- {server.name}", color=discord.Color.gold())
        embed.description = "\n".join(lines)
        await interaction.response.send_message(embed=embed)

    @command(description='Open a poll for the next theatre in the rotation.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_rotation_vote(self, interaction: discord.Interaction,
                               server: app_commands.Transform[Server, utils.ServerTransformer()],
                               hours: Optional[int] = 24):
        _, theatres = self._rotation(server.name)
        if len(theatres) < 2:
            await interaction.response.send_message("The rotation needs at least two theatres to vote on.",
                                                    ephemeral=True)
            return
        cur = self.rotation_index.get(server.name, 0) % len(theatres)
        options = [t for i, t in enumerate(theatres) if i != cur][:10]
        if not hasattr(discord, 'Poll'):
            await interaction.response.send_message(
                "This bot's discord.py has no polls. Options: " + ", ".join(options), ephemeral=True)
            return
        from datetime import timedelta
        poll = discord.Poll(question=f"Next theatre on {server.name}?"[:300],
                            duration=timedelta(hours=max(1, min(int(hours or 24), 168))))
        for o in options:
            poll.add_answer(text=o[:55])
        await interaction.response.send_message(poll=poll)

    @command(description='Record that the server has moved on to the next theatre in the rotation.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_rotation_advance(self, interaction: discord.Interaction,
                                  server: app_commands.Transform[Server, utils.ServerTransformer()]):
        _, theatres = self._rotation(server.name)
        if not theatres:
            await interaction.response.send_message(f"No rotation is configured for {server.name}.",
                                                    ephemeral=True)
            return
        self.rotation_index[server.name] = (self.rotation_index.get(server.name, 0) + 1) % len(theatres)
        self.save_state()
        await interaction.response.send_message("\n".join(self._rotation_lines(server.name)))

    async def _campaign_event_poll(self, server: Server):
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
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
                if not self._bfdb_failed(api_url, f"campaign poll for {server.name}", ex):
                    self.log.error(
                        f"FowlEngine: campaign event poll error for {server.name} ({api_url}): "
                        f"{type(ex).__name__}: {ex or '(no message)'}"
                    )
                any_failed = True

            if any_failed:
                consecutive_failures += 1
                if api_url not in self._bfdb_down and (
                        consecutive_failures in (1, 5) or consecutive_failures % 30 == 0):
                    self.log.error(
                        f"FowlEngine: campaign polling for {server.name} has failed "
                        f"{consecutive_failures} time(s) in a row -- is bfdb reachable at {api_url}?"
                    )
                await asyncio.sleep(CAMPAIGN_RECONNECT_SECS)
            else:
                consecutive_failures = 0
                self._bfdb_ok(api_url)
                await asyncio.sleep(CAMPAIGN_POLL_SECS)

    async def _poll_step(self, server_name: str, api_url: str, endpoint: str, coro) -> bool:
        """Runs one poll sub-step in isolation so a hung/broken endpoint
        doesn't also block the others in the same cycle. Returns True if it failed."""
        if api_url in self._bfdb_down and endpoint != "/api/stats (round check)":
            # bfdb is down: the first step of the cycle probes it, the rest
            # wait for the next cycle rather than each failing on their own.
            coro.close()
            return True
        try:
            await coro
            self._bfdb_ok(api_url)
            return False
        except Exception as ex:
            if not self._bfdb_failed(api_url, f"{endpoint} poll for {server_name}", ex):
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
                        fmt = render_icons(messages.get('neutral', "{icon:neutralised} **[NEUTRAL]** {message}"))
                        await faction_channels["Neutral"].send(fmt.format(message=f"{name} has gone neutral."),
                                                                  allowed_mentions=NO_PINGS)
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
                    defend_fmt = render_icons(messages.get('ready_to_capture', "{icon:pending} **[READY TO CAPTURE]** {message}"))
                    await faction_channels[owner].send(defend_fmt.format(message=f"{name} is ready to be captured -- defend it!"),
                                                      allowed_mentions=NO_PINGS)
                    attacker = "Red" if owner == "Blue" else "Blue"
                    attack_fmt = render_icons(messages.get('capture_opportunity', "{icon:target} **[OPPORTUNITY]** {message}"))
                    await faction_channels[attacker].send(attack_fmt.format(message=f"{name} ({owner}) is weak and ready to be captured!"),
                                                         allowed_mentions=NO_PINGS)
                else:
                    fmt = render_icons(messages.get('ready_to_capture', "{icon:pending} **[READY TO CAPTURE]** {message}"))
                    await faction_channels["Neutral"].send(fmt.format(message=f"{name} ({owner}) is ready to be captured."),
                                                          allowed_mentions=NO_PINGS)
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
            fmt = render_icons(messages.get('capture', "{icon:captured} **[CAPTURED]** {message}"))
            # Both factions care about a capture -- the winner as a win, the
            # loser as something to retake -- so it goes to both threads
            # rather than only the capturing side's.
            for channel in {faction_channels["Blue"], faction_channels["Red"]}:
                await channel.send(fmt.format(message=message), allowed_mentions=NO_PINGS)

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
                    fmt = render_icons(messages.get('achievement', "{icon:achievement} **[ACHIEVEMENT]** {message}"))
                    await channel.send(fmt.format(message=f"{pname} achieved {label} ({count} air kills without dying)!"),
                                       allowed_mentions=NO_PINGS)
                    break  # only announce the highest newly-crossed threshold per kill

    async def _engine_log_relay(self, server: Server):
        """Long-lived task: logs into bfdb as admin, tails /ws/engine-logs, and
        relays it into engine_log_channel until the server stops or is unconfigured."""
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username")
        password = config.get("admin_password")
        channel_id = int(config['engine_log_channel'])
        channel = self.bot.get_channel(channel_id)
        if not channel:
            self._warn_once(server.name, f"engine_log_channel:{channel_id}",
                            f"FowlEngine: engine_log_channel {channel_id} for {server.name} doesn't exist or the bot "
                            f"can't see it (wrong id, or no View Channel permission) -- fix the id in fowlengine.yaml "
                            f"or set it to null.")
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
                        self._bfdb_ok(api_url)
                        await self._pump_engine_log(ws, channel, server.name)
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                # Same blank-message pitfall as _campaign_event_poll: bare
                # timeouts stringify to "", so include the exception type.
                if not self._bfdb_failed(api_url, f"engine log relay for {server.name}", ex):
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
        await self._upsert_embed(self.tail_msg_ids, server_name, channel, embed, "engine log tail")

    async def _send_engine_log_alerts(self, channel: discord.abc.Messageable, lines: list):
        body = "\n".join(lines)
        for i in range(0, len(body), 1900):
            chunk = body[i:i + 1900]
            embed = discord.Embed(
                title=f"{icon('warning')} Engine Warning/Error",
                description=f"```{chunk}```",
                color=discord.Color.red(),
            )
            embed.timestamp = discord.utils.utcnow()
            try:
                await channel.send(embed=embed)
            except discord.HTTPException as ex:
                self.log.error(f"FowlEngine: failed to send engine log alert: {ex}")
