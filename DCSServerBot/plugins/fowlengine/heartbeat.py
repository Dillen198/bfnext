"""
Dead man's switch for the bot: ping an outside URL (healthchecks.io, Uptime
Kuma's push monitor, Better Stack heartbeats, ...) every `interval_seconds`.
If the bot, its event loop or the whole box dies, the pings stop and that
service raises the alarm -- the one failure the bot can't report itself.

Off unless `heartbeat.url` is set in fowlengine.yaml:

    heartbeat:
      url: https://hc-ping.com/<uuid>   # the check's ping URL
      interval_seconds: 60              # keep well under the check's period
      fail_when_bfdb_down: true         # ping <url>/fail while procman's bfdb is down
      timeout_seconds: 10

`<url>/fail` is the healthchecks.io convention for "I'm alive but something is
wrong"; with a service that has no such path, set fail_when_bfdb_down: false.
The body of every ping is one short status line (no secrets, no player data)
that the service shows next to the ping.

Pure logic here; commands.py's heartbeat_loop calls tick().
"""
from __future__ import annotations

import time
from typing import Optional
from urllib.parse import urlparse

DEFAULT_INTERVAL = 60
MIN_INTERVAL = 15
DEFAULT_TIMEOUT = 10


def settings(cfg: Optional[dict]) -> Optional[dict]:
    """The heartbeat section, normalised, or None when it is off."""
    hb = (cfg or {}).get("heartbeat") or {}
    url = str(hb.get("url") or "").strip()
    if not url or hb.get("enabled") is False:
        return None
    if urlparse(url).scheme not in ("http", "https"):
        return {"error": f"heartbeat.url must be http(s), not {url!r}"}
    try:
        interval = max(MIN_INTERVAL, int(hb.get("interval_seconds") or DEFAULT_INTERVAL))
    except (TypeError, ValueError):
        interval = DEFAULT_INTERVAL
    try:
        timeout = max(1, int(hb.get("timeout_seconds") or DEFAULT_TIMEOUT))
    except (TypeError, ValueError):
        timeout = DEFAULT_TIMEOUT
    return {"url": url.rstrip("/"), "interval": interval, "timeout": timeout,
            "fail_when_bfdb_down": hb.get("fail_when_bfdb_down", True) is not False}


def ping_target(s: dict, bfdb_managed: bool, bfdb_running: bool) -> tuple[str, str]:
    """(url to hit, status line for the body)."""
    if not bfdb_managed:
        return s["url"], "bot up"
    if bfdb_running:
        return s["url"], "bot up, bfdb up"
    if s["fail_when_bfdb_down"]:
        return s["url"] + "/fail", "bot up, bfdb DOWN"
    return s["url"], "bot up, bfdb down"


class Heartbeat:
    def __init__(self, cog, log):
        self.cog = cog
        self.log = log
        self._next = 0.0
        self._failing = False
        self._warned_bad = None

    async def tick(self, now: Optional[float] = None) -> Optional[str]:
        """Ping when due. Returns the URL pinged (for tests), else None."""
        now = time.monotonic() if now is None else now
        s = settings(self.cog.get_config())
        if s is None:
            return None
        if "error" in s:
            if self._warned_bad != s["error"]:
                self._warned_bad = s["error"]
                self.log.warning(f"FowlEngine/heartbeat: {s['error']} -- not pinging")
            return None
        if now < self._next:
            return None
        self._next = now + s["interval"]
        pm = getattr(self.cog, "procman", None)
        managed = bool(pm and pm.enabled)
        running = bool(managed and pm.is_running())
        url, line = ping_target(s, managed, running)
        try:
            await self._post(url, line, s["timeout"])
            if self._failing:
                self.log.info("FowlEngine/heartbeat: pings are getting through again")
            self._failing = False
        except Exception as ex:  # noqa: BLE001 -- a monitoring blip must never hurt the bot
            if not self._failing:
                self.log.warning(f"FowlEngine/heartbeat: ping failed ({type(ex).__name__}: {ex}); "
                                 "retrying every interval, logging once")
            self._failing = True
        return url

    async def _post(self, url: str, body: str, timeout: int) -> None:
        import aiohttp
        async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=timeout)) as http:
            async with http.post(url, data=body.encode("utf-8"),
                                 headers={"Content-Type": "text/plain; charset=utf-8"}) as r:
                if r.status >= 400:
                    raise RuntimeError(f"HTTP {r.status}")
