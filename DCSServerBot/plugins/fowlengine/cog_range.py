"""RangeMixin: part of the FowlEngine cog (commands.py).

Training range (bfrange) servers: the live status embed, the graded-results
feed and /range.

Moved out of commands.py unchanged. A mixin: it holds no state of its own and
relies on what FowlEngine.__init__ sets up; discord.py collects its commands,
groups, loops and listeners through the MRO."""
import discord
from core import utils, Server, Status
from discord import app_commands
from discord.ext import tasks
import asyncio
import io
from typing import Optional
from urllib.parse import quote
from .icons import icon
from . import rangefeed
from .common import (NO_PINGS, RANGE_CARD_MAX_BYTES, RANGE_HTTP_TIMEOUT, RANGE_RESULTS_POLL_SECS,
                     RANGE_STATUS_MINUTES, srv_params)


class RangeMixin:
    # ── Training range (bfrange): live status embed + graded-results feed ──
    #
    # Per-server config (the range server's own section of fowlengine.yaml):
    #   range_status_channel   one embed, edited every RANGE_STATUS_MINUTES
    #   range_results_channel  every graded result, with its debrief card
    #   range_results_kinds    optional filter, e.g. [trap, bomb, strafe]
    #   greenie_channel        optional: carrier passes only
    #   range_site_url         optional, default https://range.vectorstrike.org
    # All of it is ignored (with one warning) on a server whose bfdb instance
    # is not `kind: range`.

    def _channel_for(self, config: dict, key: str, server_name: str):
        cid = config.get(key)
        if not cid:
            return None
        try:
            cid = int(cid)
        except (TypeError, ValueError):
            self._warn_once(server_name, f"bad:{key}",
                            f"FowlEngine: {server_name}: {key} is not a channel id: {cid!r}")
            return None
        channel = self.bot.get_channel(cid)
        if channel is None:
            self._warn_once(server_name, f"missing:{key}:{cid}",
                            f"FowlEngine: {server_name}: {key} {cid} not found or the bot lacks access")
        return channel

    def _range_misconfigured(self, server, config: dict, keys) -> bool:
        """True when range feeds are configured on a server that is not a
        `kind: range` instance (warned about once, then skipped)."""
        if self._is_range(server):
            return False
        set_keys = [k for k in keys if config.get(k)]
        if set_keys:
            self._warn_once(server.name, "range-feeds-on-campaign",
                            f"FowlEngine: {server.name} sets {', '.join(set_keys)} but is not a "
                            f"`kind: range` instance in bfdb.instances -- ignored")
        return True

    def _range_fail(self, server_name: str, what: str, ex: Exception) -> None:
        key = (server_name, what)
        n = self._range_fail_counts.get(key, 0) + 1
        self._range_fail_counts[key] = n
        # a range server that is down for an hour must not write 240 errors
        if n in (1, 5) or n % 40 == 0:
            self.log.error(f"FowlEngine: range {what} for {server_name} failed ({n}x in a row): "
                           f"{type(ex).__name__}: {ex or '(no message)'}")

    def _range_ok(self, server_name: str, what: str) -> None:
        if self._range_fail_counts.pop((server_name, what), 0) >= 5:
            self.log.info(f"FowlEngine: range {what} for {server_name} recovered")

    def _embed_from_dict(self, d: dict, base: discord.Embed | None = None) -> discord.Embed:
        """A rangefeed embed dict (already clipped to Discord's limits) onto a
        discord.Embed -- a fresh one, or `base` (e.g. a branded _vs_embed)."""
        embed = base if base is not None else discord.Embed()
        if d.get("title"):
            embed.title = d["title"]
        if d.get("description"):
            embed.description = d["description"]
        if d.get("url"):
            embed.url = d["url"]
        if d.get("color") is not None:
            embed.colour = discord.Colour(d["color"])
        for f in d.get("fields") or []:
            embed.add_field(name=f["name"], value=f["value"], inline=bool(f.get("inline")))
        if d.get("footer"):
            embed.set_footer(text=d["footer"])
        if d.get("thumbnail"):
            embed.set_thumbnail(url=d["thumbnail"])
        if d.get("image"):
            embed.set_image(url=d["image"])
        return embed

    async def _range_get_json(self, http, url: str, params: dict | None = None):
        async with http.get(url, params=params) as resp:
            if resp.status != 200:
                raise RuntimeError(f"/api/{url.split('/api/', 1)[-1]} -> HTTP {resp.status}")
            return await resp.json(content_type=None)

    async def _fetch_range_live(self, http, api_url: str, server):
        data = await self._range_get_json(http, f"{api_url}/api/range/live", self._range_params(server))
        return data.get("live") if isinstance(data, dict) else None

    async def _build_range_status_embed(self, server, config: dict) -> discord.Embed:
        import aiohttp
        running = server.status in (Status.RUNNING, Status.PAUSED)
        live, err = None, None
        if running:
            api_url = config.get("api_url", "http://localhost:8880").rstrip("/")
            try:
                async with aiohttp.ClientSession(
                        timeout=aiohttp.ClientTimeout(total=RANGE_HTTP_TIMEOUT)) as http:
                    live = await self._fetch_range_live(http, api_url, server)
                self._range_ok(server.name, "live status")
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                err = "bfdb did not answer."
                self._range_fail(server.name, "live status", ex)
        d = rangefeed.build_status(
            live, self._range_site(config), running=running, error=err,
            label=server.name if len(self._range_servers()) > 1 else "")
        color = discord.Colour(d["color"]) if d.get("color") is not None else None
        base = self._vs_embed(d["title"], color=color, url=d.get("url"), server=server)
        return self._embed_from_dict({k: v for k, v in d.items() if k != "title"}, base=base)

    async def _upsert_embed(self, ids: dict, key: str, channel, embed: discord.Embed,
                            what: str = "embed", png: bytes | None = None,
                            png_name: str = "image.png", refresh_png: bool = False,
                            carries_image: bool = False) -> bool:
        """Edit the one persisted message for `key` in place, or post it (and
        persist its id) when there is none yet or it was deleted.

        Edits go straight to the message by id (a partial message) instead of
        fetching it first: fetching needs "Read Message History" on the
        channel, and without it every tick used to fail, forget the id and
        post a fresh embed -- a new message every few minutes. Only "that
        message is gone" (404) posts a replacement; any other failure is
        logged and retried next tick, never answered with another post.

        `png` is an image the embed shows as attachment://`png_name`. It is
        uploaded when the message is posted, and on an edit only when
        `refresh_png` says it changed -- otherwise the edit leaves the
        message's attachment alone. `carries_image` callers get the old
        attachment dropped on a tick with no `png`, so it doesn't hang under
        the embed as a loose file.

        Returns whether the message now carries `png` when an upload was asked
        for (True otherwise, when the edit/post went through). The caller keeps
        re-sending an image until that is True -- an upload that failed once
        used to leave the embed pointing at an attachment that was never there,
        so the picture silently stayed missing. Without "Attach Files" in the
        channel the image is dropped and the text is still updated."""
        if key not in ids and '__legacy__' in ids:
            ids[key] = ids.pop('__legacy__')  # pre-multi-server id: re-home it
        msg_id = ids.get(key)
        uploading = png is not None and refresh_png

        def attach_forbidden():
            self._warn_once(str(channel.id), "attach-forbidden",
                            f"FowlEngine: can't attach the {what}'s picture in channel {channel.id} -- give the "
                            f"bot Attach Files there (posting it without the picture until then)")
            embed.set_image(url=None)

        async def edit(**kw):
            if hasattr(channel, 'get_partial_message'):
                await channel.get_partial_message(int(msg_id)).edit(**kw)
            else:
                await (await channel.fetch_message(int(msg_id))).edit(**kw)

        if msg_id:
            try:
                kw = {"embed": embed}
                if uploading:
                    kw["attachments"] = [discord.File(io.BytesIO(png), filename=png_name)]
                elif png is None and carries_image:
                    kw["attachments"] = []
                try:
                    await edit(**kw)
                    return True
                except discord.Forbidden:
                    if not uploading:
                        raise
                    attach_forbidden()
                    await edit(embed=embed, attachments=[])
                    return False
            except discord.NotFound:
                self.log.info(f"FowlEngine: {what} message {msg_id} in channel {channel.id} is gone -- posting a new one")
                ids.pop(key, None)
            except discord.Forbidden:
                self._warn_once(str(channel.id), "edit-forbidden",
                                f"FowlEngine: can't edit the {what} in channel {channel.id} -- give the bot "
                                f"View Channel, Send Messages and Embed Links there")
                return False
            except discord.HTTPException as ex:
                self.log.warning(f"FowlEngine: editing the {what} in channel {channel.id} failed ({ex}); retrying next tick")
                return False
        delivered = True
        if png is not None:
            try:
                msg = await channel.send(embed=embed, file=discord.File(io.BytesIO(png), filename=png_name))
            except discord.Forbidden:
                attach_forbidden()
                msg = await channel.send(embed=embed)
                delivered = False
        else:
            msg = await channel.send(embed=embed)
        ids[key] = msg.id
        self.save_state()
        return delivered

    @tasks.loop(minutes=RANGE_STATUS_MINUTES)
    async def update_range_status(self):
        for server in list(self.bot.servers.values()):
            try:
                config = self.get_config(server) or {}
                if not config.get("range_status_channel"):
                    continue
                if self._range_misconfigured(server, config, ("range_status_channel",)):
                    continue
                channel = self._channel_for(config, "range_status_channel", server.name)
                if channel is None:
                    continue
                embed = await self._build_range_status_embed(server, config)
                await self._upsert_embed(self.range_status_msg_ids, server.name, channel, embed)
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                # never let one server's failure stop the loop for the others
                self.log.error(f"FowlEngine: range status for {server.name}: "
                               f"{type(ex).__name__}: {ex or '(no message)'}")

    @update_range_status.before_loop
    async def before_update_range_status(self):
        await self.bot.wait_until_ready()

    @tasks.loop(seconds=RANGE_RESULTS_POLL_SECS)
    async def poll_range_results(self):
        keys = ("range_results_channel", "greenie_channel")
        targets = []
        for server in list(self.bot.servers.values()):
            try:
                config = self.get_config(server) or {}
                if not any(config.get(k) for k in keys):
                    continue
                if self._range_misconfigured(server, config, keys):
                    continue
                targets.append((server, config))
            except Exception as ex:
                self.log.error(f"FowlEngine: range feed config for {server.name}: {ex}")
        if not targets:
            return
        import aiohttp
        async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=RANGE_HTTP_TIMEOUT)) as http:
            for server, config in targets:
                try:
                    await self._poll_range_results_for(http, server, config)
                    self._range_ok(server.name, "results feed")
                except asyncio.CancelledError:
                    raise
                except Exception as ex:
                    self._range_fail(server.name, "results feed", ex)

    @poll_range_results.before_loop
    async def before_poll_range_results(self):
        await self.bot.wait_until_ready()

    async def _poll_range_results_for(self, http, server, config: dict) -> None:
        api_url = config.get("api_url", "http://localhost:8880").rstrip("/")
        data = await self._range_get_json(
            http, f"{api_url}/api/range/feed",
            self._range_params(server, limit=rangefeed.FEED_LIMIT))
        items = rangefeed.feed_items(data)
        cursor = self.range_feed_cursors.get(server.name)
        if cursor is None:
            # The first poll this bot has ever made for this server: start
            # from "now" instead of replaying the whole history as new.
            self.range_feed_cursors[server.name] = rangefeed.next_cursor(items, None)
            self.save_state()
            self.log.info(f"FowlEngine: range results feed for {server.name} starts after "
                          f"{self.range_feed_cursors[server.name].get('id') or '(empty feed)'}")
            return
        new, overflow = rangefeed.select_new(items, cursor, limit=rangefeed.FEED_LIMIT)
        new_cursor = rangefeed.next_cursor(items, cursor)
        if new_cursor != cursor:
            # Advanced BEFORE posting: a Discord hiccup halfway through a
            # batch must never make the next poll post the same results again.
            self.range_feed_cursors[server.name] = new_cursor
            self.save_state()
        if not new:
            return

        site = self._range_site(config)
        results_ch = self._channel_for(config, "range_results_channel", server.name)
        greenie_ch = self._channel_for(config, "greenie_channel", server.name)
        kinds = config.get("range_results_kinds") or None
        cap = rangefeed.MAX_POSTS_PER_POLL
        plan = []  # (channel, items to post oldest first, catch-up summary line)
        if results_ch is not None:
            want = rangefeed.parse_kinds(kinds)
            if want and greenie_ch is not None and greenie_ch.id == results_ch.id:
                want = want + ["trap"]  # one channel for both: traps still belong there
            chosen = rangefeed.filter_kinds(new, want)
            skipped, post = rangefeed.split_cap(chosen, cap)
            plan.append((results_ch, post,
                         rangefeed.summary_line(skipped, overflow and bool(chosen), site)))
        if greenie_ch is not None and (results_ch is None or greenie_ch.id != results_ch.id):
            traps = rangefeed.filter_kinds(new, ["trap"])
            if traps:
                skipped, post = rangefeed.split_cap(traps, cap)
                plan.append((greenie_ch, post,
                             rangefeed.summary_line(skipped, False, site, noun="carrier pass")))

        rendered: dict = {}  # result id -> (embed dict, card png) -- fetched once per poll
        for channel, post, summary in plan:
            if summary:
                try:
                    await channel.send(summary, allowed_mentions=NO_PINGS)
                except discord.HTTPException as ex:
                    self.log.error(f"FowlEngine: range catch-up line to {channel.id} failed: {ex}")
            for item in post:
                rid = rangefeed.item_id(item)
                if rid not in rendered:
                    rendered[rid] = await self._render_range_result(http, api_url, server, item, site)
                emb, png = rendered[rid]
                try:
                    await self._send_range_result(channel, rid, emb, png)
                except discord.HTTPException as ex:
                    self.log.error(f"FowlEngine: posting range result {rid} to {channel.id} failed: {ex}")

    async def _render_range_result(self, http, api_url: str, server, item: dict, site: str):
        """(embed dict, card PNG bytes or None) for one feed item: bfdb's own
        Discord embed for the result, and the card downloaded here so Discord
        never has to reach bfdb for the image."""
        rid = rangefeed.item_id(item)
        iid = self._instance_id(server)
        params = {"instance": iid} if iid else srv_params(server.name)
        emb = None
        try:
            data = await self._range_get_json(
                http, f"{api_url}/api/range/result/{quote(rid, safe='')}/discord", params)
            emb = rangefeed.normalize_embed(data)
        except asyncio.CancelledError:
            raise
        except Exception as ex:
            self.log.warning(f"FowlEngine: range result {rid}: embed route failed "
                             f"({type(ex).__name__}: {ex or '(no message)'}) -- posting the headline")
        if not emb or not (emb.get("title") or emb.get("description")):
            emb = rangefeed.fallback_embed(item, site)
        png = None
        url = rangefeed.card_url(api_url, item, emb)
        if url:
            try:
                async with http.get(url) as resp:
                    ctype = (resp.headers.get("Content-Type") or "").lower()
                    too_big = (resp.content_length or 0) > RANGE_CARD_MAX_BYTES
                    if resp.status == 200 and not too_big and (not ctype or "image" in ctype):
                        body = await resp.read()
                        if 0 < len(body) <= RANGE_CARD_MAX_BYTES:
                            png = body
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                self.log.debug(f"FowlEngine: range card {url} not downloaded: {ex}")
        return emb, png

    @staticmethod
    def _link_view(links) -> Optional[discord.ui.View]:
        """bfdb's links (debrief, 3D replay, web Tacview, Tacview clip) as a row
        of link buttons. Link buttons have no callback, so the view needs no
        timeout and nothing to listen for after a restart."""
        if not links:
            return None
        view = discord.ui.View(timeout=None)
        for l in links:
            view.add_item(discord.ui.Button(style=discord.ButtonStyle.link, label=l["label"], url=l["url"]))
        return view

    async def _send_range_result(self, channel, rid: str, emb: dict, png: bytes | None) -> None:
        embed = self._embed_from_dict(emb)
        kw = {"embed": embed}
        view = self._link_view(emb.get("links"))
        if view is not None:
            kw["view"] = view
        if png:
            fname = rangefeed.attachment_name(rid)
            embed.set_image(url=f"attachment://{fname}")
            await channel.send(file=discord.File(io.BytesIO(png), filename=fname), **kw)
        else:
            # falls back to the absolute image URL bfdb put in the embed, if any
            await channel.send(**kw)

    # ── /range ──────────────────────────────────────────────────────────────

    range_cmds = app_commands.Group(name="range", description="Vector Strike training range")

    def _pick_range_server(self, server):
        if server is not None:
            if self._is_range(server):
                return server, None
            return None, f"**{server.name}** is not a training range server."
        servers = self._range_servers()
        if not servers:
            return None, ("No training range server is configured (no `kind: range` instance "
                          "in `bfdb.instances`).")
        return servers[0], None

    @range_cmds.command(name="status", description="Live range status: tankers, carriers, stations, who is flying.")
    @app_commands.guild_only()
    async def range_status(self, interaction: discord.Interaction,
                           server: Optional[app_commands.Transform[Server, utils.ServerTransformer()]] = None):
        await interaction.response.defer(ephemeral=True)
        srv, why = self._pick_range_server(server)
        if srv is None:
            await interaction.followup.send(f"{icon('bad')} {why}")
            return
        try:
            embed = await self._build_range_status_embed(srv, self.get_config(srv) or {})
        except Exception as ex:
            await interaction.followup.send(f"Error: {type(ex).__name__}: {ex}")
            return
        await interaction.followup.send(embed=embed)

    @range_cmds.command(name="me", description="Link to your training range pilot page.")
    @app_commands.guild_only()
    async def range_me(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        srv = next(iter(self._range_servers()), None)
        site = self._range_site(self.get_config(srv) if srv else None)
        ucid = await self._ucid_for_member(interaction.user, [])
        if not ucid:
            linkme = "`/linkme`"
            try:
                cmd = await utils.get_command(self.bot, name="linkme")
                if cmd is not None and getattr(cmd, "mention", None):
                    linkme = cmd.mention
            except Exception:  # noqa: BLE001 - only decorates the hint
                pass
            await interaction.followup.send(
                f"I can't find a DCS account linked to your Discord user. Link it with {linkme} "
                f"(you get a code to type into the in-game chat), then try again. Meanwhile, "
                f"every result is on <{site}>.")
            return
        url = rangefeed.pilot_url(site, ucid)
        embed = self._vs_embed("Your range record", color=discord.Color.teal(), url=url)
        embed.description = (f"**[Open your pilot page ›]({url})**\n"
                             f"Every graded bomb, strafe pass, trap and AAR session you have "
                             f"flown, with the debrief cards.")
        await interaction.followup.send(embed=embed)

    @range_cmds.command(name="greenie", description="The carrier greenie board: top 10 LSO averages.")
    @app_commands.guild_only()
    @app_commands.describe(days="Look-back window in days (default 30)")
    async def range_greenie(self, interaction: discord.Interaction,
                            days: app_commands.Range[int, 1, 365] = 30):
        await interaction.response.defer()
        srv = next(iter(self._range_servers()), None)
        config = (self.get_config(srv) if srv else self.get_config()) or {}
        api_url = config.get("api_url", "http://localhost:8880").rstrip("/")
        params = self._range_params(srv, days=days) if srv else {"days": days}
        import aiohttp
        try:
            async with aiohttp.ClientSession(
                    timeout=aiohttp.ClientTimeout(total=RANGE_HTTP_TIMEOUT)) as http:
                data = await self._range_get_json(http, f"{api_url}/api/range/greenie", params)
        except Exception as ex:
            await interaction.followup.send(f"{icon('bad')} bfdb did not answer: {type(ex).__name__}: {ex or ''}")
            return
        site = self._range_site(config)
        lines = rangefeed.greenie_lines(data, site, limit=10)
        embed = self._vs_embed(f"Greenie Board — last {days} days", color=discord.Color.green(), url=site)
        embed.description = ("\n".join(lines) if lines
                             else "No graded carrier passes in that window yet.")[:4096]
        await interaction.followup.send(embed=embed)
