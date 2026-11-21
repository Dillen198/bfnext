"""ReportMixin: part of the FowlEngine cog (commands.py).

The weekly campaign report poster -> Discord (the logic is in
weeklyreport.py). Two ways in:

* `post_weekly_report`, called by the OPS API (`POST <ops>/report/post`)
  when an admin presses Post to Discord on the dashboard's REPORT page: the
  poster and the text arrive ready-made.
* `weekly_report_loop`, the optional schedule: per server, `weekly_report:`
  with `enabled: true` in its section of fowlengine.yaml. The bot captures the
  dashboard's `/report/print` page (mapsnap.capture_page) and posts it.

Both post to the server's `report_channel` (else its `news_channel`). A
mixin: it holds no state of its own and relies on what FowlEngine.__init__
sets up (`weekly_report_state`, persisted by save_state)."""
import asyncio
import io
import os
import re
from datetime import datetime, timezone

import discord
from discord.ext import tasks

from . import mapsnap, weeklyreport
from .common import NO_PINGS


class ReportMixin:
    # ── which server, which channel ─────────────────────────────────────────

    def _report_server(self, instance: str | None, server_name: str | None):
        """The DCS server an instance id (or a server name) belongs to, or None."""
        servers = list(self.bot.servers.values())
        if instance:
            for s in servers:
                if self._instance_id(s) == instance:
                    return s
        if server_name:
            return self.bot.servers.get(server_name)
        return None

    def _report_channel(self, server, config: dict):
        key = weeklyreport.channel_key(config)
        if not key:
            return None
        return self._channel_for(config, key, getattr(server, "name", "DEFAULT"))

    async def _send_report(self, channel, png: bytes, text: str, filename: str) -> str:
        msg = await channel.send(content=text, file=discord.File(io.BytesIO(png), filename=filename),
                                 allowed_mentions=NO_PINGS)
        return getattr(msg, "jump_url", None) or ""

    # ── from the dashboard ──────────────────────────────────────────────────

    async def post_weekly_report(self, *, instance: str | None, server_name: str | None,
                                 png: bytes, text: str, filename: str, who: str | None = None) -> dict:
        """Post a poster an admin made on the dashboard. {"ok", "channel",
        "url"} or {"ok": False, "error"}."""
        server = self._report_server(instance, server_name)
        config = (self.get_config(server) if server is not None else self.get_config()) or {}
        channel = self._report_channel(server, config)
        if channel is None:
            where = server.name if server is not None else "DEFAULT"
            return {"ok": False, "error": f"no report_channel (or news_channel) set for {where} in fowlengine.yaml, "
                                          f"or the bot cannot see it"}
        try:
            url = await self._send_report(channel, png, text, filename)
        except discord.HTTPException as ex:
            return {"ok": False, "error": f"Discord refused the post: {ex}"}
        self.log.info(f"FowlEngine: weekly report for {instance or server_name} posted to #{channel} "
                      f"by {who or 'the dashboard'}")
        return {"ok": True, "channel": str(getattr(channel, "name", channel)), "url": url}

    def weekly_report_status(self, instance: str | None, server_name: str | None) -> dict:
        server = self._report_server(instance, server_name)
        config = (self.get_config(server) if server is not None else self.get_config()) or {}
        sched = weeklyreport.parse_schedule(config.get("weekly_report"))
        last = self.weekly_report_state.get(server.name) if server is not None else None
        channel = self._report_channel(server, config) if weeklyreport.channel_key(config) else None
        return {
            "server": getattr(server, "name", None),
            **weeklyreport.status(sched, last, str(getattr(channel, "name", "")) or None,
                                  datetime.now(timezone.utc)),
        }

    # ── the schedule ────────────────────────────────────────────────────────

    @tasks.loop(seconds=weeklyreport.POLL_SECS)
    async def weekly_report_loop(self):
        now = datetime.now(timezone.utc)
        for server in list(self.bot.servers.values()):
            try:
                config = self.get_config(server) or {}
                if self._is_range(server):
                    continue
                key = weeklyreport.due(now, weeklyreport.parse_schedule(config.get("weekly_report")),
                                       self.weekly_report_state.get(server.name))
                if key:
                    await self._scheduled_report(server, config, key)
            except asyncio.CancelledError:
                raise
            except Exception as ex:
                self.log.warning(f"FowlEngine: weekly report for {server.name} failed: "
                                 f"{type(ex).__name__}: {ex or '(no message)'}")

    @weekly_report_loop.before_loop
    async def before_weekly_report_loop(self):
        await self.bot.wait_until_ready()

    async def _scheduled_report(self, server, config: dict, key: str) -> None:
        channel = self._report_channel(server, config)
        if channel is None:
            self._warn_once(server.name, "report-channel",
                            f"FowlEngine: {server.name}: weekly_report is on but there is no report_channel "
                            f"(or news_channel) -- not posting")
            return
        browser = mapsnap.find_browser(config.get("status_map_browser"))
        if not browser:
            self._warn_once(server.name, "report-browser",
                            "FowlEngine: weekly report: no Edge/Chrome found to draw the poster -- "
                            "set status_map_browser")
            return
        # Recorded first: a failure halfway must not post the week twice; a
        # week that cannot be drawn is skipped, with a warning.
        self.weekly_report_state[server.name] = key
        self.save_state()
        sched = weeklyreport.parse_schedule(config.get("weekly_report")) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        base = (config.get("status_map_url") or api_url).rstrip("/")
        url = weeklyreport.print_url(base, server.name, self._instance_id(server), sched.get("days", 7))
        profile = os.path.join(self.bot.node.config_dir, "fowlengine_mapsnap",
                               "report-" + re.sub(r"[^A-Za-z0-9_.-]+", "_", server.name))
        png, text = await mapsnap.capture_page(
            browser, url, profile, path="/report/print", selector="#weekly-poster",
            extract="document.getElementById('weekly-post-text')?.textContent || null", log=self.log)
        if not png:
            return
        brand = self._branding(server, config)
        dash = (brand.get("dashboard_url") or config.get("dashboard_url") or "").rstrip("/") or None
        text = (text or weeklyreport.fallback_text(brand.get("brand_name") or server.name, dash))[:weeklyreport.MAX_TEXT]
        name = weeklyreport.safe_filename(f"weekly-report-{self._instance_id(server) or server.name}-{key[:10]}", "png")
        try:
            await self._send_report(channel, png, text, name)
            self.log.info(f"FowlEngine: weekly report for {server.name} posted ({len(png) // 1024} KB)")
        except discord.HTTPException as ex:
            self.log.error(f"FowlEngine: posting the weekly report for {server.name} failed: {ex}")
