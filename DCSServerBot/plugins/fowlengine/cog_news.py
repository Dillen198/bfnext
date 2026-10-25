"""NewsMixin: part of the FowlEngine cog (commands.py).

The war diary feed: each newly filed daily dispatch posted once to
news_channel (the logic is in newsfeed.py).

Moved out of commands.py unchanged. A mixin: it holds no state of its own and
relies on what FowlEngine.__init__ sets up; discord.py collects its commands,
groups, loops and listeners through the MRO."""
import discord
from discord.ext import tasks
import time
import asyncio
import io
from . import newsfeed
from .common import NEWS_HTTP_TIMEOUT, NO_PINGS, srv_params


class NewsMixin:
    # ── War diary -> news_channel ───────────────────────────────────────────
    # Per server, in its own section of fowlengine.yaml:
    #   news_channel   every newly filed daily dispatch, once, picture attached
    # Unset = off. What gets posted and when lives in newsfeed.py; this is the
    # HTTP and the send. Skipped for training range servers (no war, no diary).

    @tasks.loop(seconds=newsfeed.NEWS_POLL_SECS)
    async def post_news(self):
        targets = []
        for server in list(self.bot.servers.values()):
            try:
                config = self.get_config(server) or {}
                if config.get("news_channel") and not self._is_range(server):
                    targets.append((server, config))
            except Exception as ex:
                self.log.error(f"FowlEngine: news feed config for {server.name}: {ex}")
        if not targets:
            return
        import aiohttp
        async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=NEWS_HTTP_TIMEOUT)) as http:
            for server, config in targets:
                try:
                    await self._post_news_for(http, server, config)
                    if self._news_fail_counts.pop(server.name, 0) >= 5:
                        self.log.info(f"FowlEngine: news feed for {server.name} recovered")
                except asyncio.CancelledError:
                    raise
                except Exception as ex:
                    n = self._news_fail_counts.get(server.name, 0) + 1
                    self._news_fail_counts[server.name] = n
                    # bfdb down for an hour must not write 30 errors
                    if n in (1, 5) or n % 30 == 0:
                        self.log.warning(f"FowlEngine: news feed for {server.name} failed ({n}x in a row): "
                                         f"{type(ex).__name__}: {ex or '(no message)'}")

    @post_news.before_loop
    async def before_post_news(self):
        await self.bot.wait_until_ready()

    async def _post_news_for(self, http, server, config: dict) -> None:
        channel = self._channel_for(config, "news_channel", server.name)
        if channel is None:
            return
        api_url = config.get("api_url", "http://localhost:8880").rstrip("/")
        iid = self._instance_id(server)
        params = {"instance": iid} if iid else srv_params(server.name)
        async with http.get(f"{api_url}/api/news",
                            params={**params, "limit": newsfeed.FETCH_LIMIT}) as resp:
            if resp.status != 200:
                raise RuntimeError(f"/api/news -> HTTP {resp.status}")
            data = await resp.json(content_type=None)
        prev = self.news_feed_state.get(server.name)
        to_post, state = newsfeed.plan(data, prev, time.time())
        if state != prev:
            self.news_feed_state[server.name] = state
            self.save_state()
        for day in to_post:
            # Recorded BEFORE posting: a Discord hiccup halfway must never make
            # the next poll post the same dispatch again.
            state = newsfeed.mark_posted(state, day)
            self.news_feed_state[server.name] = state
            self.save_state()
            png, ctype = await self._fetch_news_image(http, api_url, day)
            embed = self._embed_from_dict(newsfeed.build_embed(day, config.get("dashboard_url"), iid))
            try:
                if png:
                    fname = newsfeed.attachment_name(day, ctype)
                    embed.set_image(url=f"attachment://{fname}")
                    await channel.send(embed=embed, file=discord.File(io.BytesIO(png), filename=fname),
                                       allowed_mentions=NO_PINGS)
                else:
                    await channel.send(embed=embed, allowed_mentions=NO_PINGS)
            except discord.HTTPException as ex:
                self.log.error(f"FowlEngine: posting dispatch {day.get('day')} for {server.name} "
                               f"to {channel.id} failed: {ex}")

    async def _fetch_news_image(self, http, api_url: str, day: dict):
        """(bytes, content type) of a dispatch's picture from bfdb, or
        (None, None). Uploaded as an attachment, so Discord never has to reach
        bfdb (or its public URL) for it."""
        path = newsfeed.image_path(day)
        if not path:
            return None, None
        try:
            async with http.get(f"{api_url}{path}") as resp:
                ctype = (resp.headers.get("Content-Type") or "").lower()
                too_big = (resp.content_length or 0) > newsfeed.IMAGE_MAX_BYTES
                if resp.status == 200 and not too_big and ctype.startswith("image/"):
                    body = await resp.read()
                    if 0 < len(body) <= newsfeed.IMAGE_MAX_BYTES:
                        return body, ctype
        except asyncio.CancelledError:
            raise
        except Exception as ex:
            self.log.debug(f"FowlEngine: dispatch picture {path} not downloaded: {ex}")
        return None, None
