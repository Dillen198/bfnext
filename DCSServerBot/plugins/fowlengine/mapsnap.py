# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See DCSServerBot/plugins/fowlengine/LICENSE and the repository NOTICE file.
"""
Campaign map snapshot for the Discord status embed.

The picture is the dashboard's own public map: bfweb's chromeless `/snapshot`
route (the SITREP TacMap full-frame, objectives + front line only -- never the
fog-of-war tacmap feed), captured by a headless Chromium. On Windows that is
Microsoft Edge, which every server already has, so there is nothing to
install; Chrome works too, or `snapshot_browser` names any Chromium.

The browser is driven over the DevTools protocol (aiohttp's websocket client,
already a dependency) rather than `--screenshot`: the page raises
`data-snapshot-ready` once its data is in and every basemap tile has loaded,
and the capture waits for exactly that instead of guessing a delay.

The browser runs with its own throwaway-ish profile directory and no cookies,
so it sees the dashboard exactly as an anonymous visitor does.
"""
from __future__ import annotations

import asyncio
import base64
import hashlib
import json
import os
import shutil
import socket
import subprocess
import time
from typing import Iterable, Optional
from urllib.parse import quote

__all__ = [
    "find_browser", "snapshot_url", "map_signature", "capture",
    "DEFAULT_SIZE", "parse_size",
]

DEFAULT_SIZE = (1200, 800)
_BROWSER_CANDIDATES = (
    r"%ProgramFiles(x86)%\Microsoft\Edge\Application\msedge.exe",
    r"%ProgramFiles%\Microsoft\Edge\Application\msedge.exe",
    r"%ProgramFiles%\Google\Chrome\Application\chrome.exe",
    r"%ProgramFiles(x86)%\Google\Chrome\Application\chrome.exe",
    r"%LocalAppData%\Google\Chrome\Application\chrome.exe",
)


def find_browser(configured: Optional[str] = None) -> Optional[str]:
    """The Chromium to capture with: `configured` when it exists, else Edge,
    else Chrome, else whatever is on PATH."""
    if configured:
        p = os.path.expandvars(configured)
        return p if os.path.isfile(p) else None
    for c in _BROWSER_CANDIDATES:
        p = os.path.expandvars(c)
        if "%" not in p and os.path.isfile(p):
            return p
    for name in ("msedge", "chrome", "chromium", "google-chrome", "chromium-browser"):
        p = shutil.which(name)
        if p:
            return p
    return None


def parse_size(raw) -> tuple[int, int]:
    """"1200x800" (or [1200, 800]) -> (1200, 800), clamped to sane bounds."""
    try:
        if isinstance(raw, str):
            w, h = (int(v) for v in raw.lower().split("x", 1))
        else:
            w, h = int(raw[0]), int(raw[1])
    except (TypeError, ValueError, IndexError):
        return DEFAULT_SIZE
    return max(400, min(w, 2400)), max(300, min(h, 1600))


def snapshot_url(base: str, server_name: Optional[str]) -> str:
    """bfweb's snapshot page for one instance, forced to the dark theme (the
    fresh headless profile otherwise reports a light OS theme)."""
    url = f"{base.rstrip('/')}/snapshot?theme=dark"
    if server_name:
        url += f"&server={quote(server_name)}"
    return url


def map_signature(objectives: Iterable[dict]) -> str:
    """What the picture shows, hashed: owner, a coarse health band and the
    contested flags of every plotted objective. A new signature means the
    map changed and is worth re-capturing; health drift inside a band isn't."""
    rows = []
    for o in objectives:
        if not o.get("lat") and not o.get("lon"):
            continue  # not plotted (carrier groups)
        rows.append((
            str(o.get("id")), o.get("owner"),
            int((o.get("health") or 0) // 25) if (o.get("health") or 0) > 0 else -1,
            bool(o.get("threatened")), bool(o.get("captureable")),
        ))
    rows.sort()
    return hashlib.sha1(json.dumps(rows).encode()).hexdigest()


class _Cdp:
    """Minimal DevTools-protocol client over one websocket."""

    def __init__(self, ws):
        self.ws = ws
        self.next_id = 0

    async def call(self, method: str, params: Optional[dict] = None, timeout: float = 15.0) -> dict:
        self.next_id += 1
        mid = self.next_id
        await self.ws.send_str(json.dumps({"id": mid, "method": method, "params": params or {}}))
        deadline = time.monotonic() + timeout
        while True:
            left = deadline - time.monotonic()
            if left <= 0:
                raise TimeoutError(f"CDP {method} timed out")
            msg = await self.ws.receive(timeout=left)
            if msg.type.name != "TEXT":
                raise ConnectionError(f"CDP socket closed during {method}")
            data = json.loads(msg.data)
            if data.get("id") != mid:
                continue  # an event, or a reply to something else
            if "error" in data:
                raise RuntimeError(f"CDP {method}: {data['error'].get('message')}")
            return data.get("result") or {}


def _free_port() -> int:
    """A loopback port nothing is listening on right now."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def _tail(path: str, limit: int = 700) -> str:
    """The end of a browser's stderr log, whitespace-collapsed, for a warning."""
    try:
        with open(path, "rb") as f:
            f.seek(0, os.SEEK_END)
            f.seek(max(0, f.tell() - limit * 4))
            text = f.read().decode("utf-8", "replace")
    except OSError:
        return "(no stderr log)"
    text = " ".join(text.split())
    return (text[-limit:] if text else "(stderr empty)")


async def _wait_devtools(port: int, profile: str, timeout: float) -> int:
    """The browser's DevTools port once it answers.

    We pick the port ourselves and poll it, rather than rely only on the
    DevToolsActivePort file Chromium writes into the profile -- on the live
    server (bot started elevated by the Fowl Engine service) that file never
    appeared. The file is still read as a second source in case the browser
    chose another port. The launched process exiting is not fatal on its own:
    elevated Edge can hand over to a relaunched process."""
    import aiohttp
    marker = os.path.join(profile, "DevToolsActivePort")
    deadline = time.monotonic() + timeout
    async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=2)) as http:
        while time.monotonic() < deadline:
            candidates = [port]
            try:
                with open(marker, encoding="utf-8") as f:
                    first = f.readline().strip()
                if first.isdigit() and int(first) != port:
                    candidates.append(int(first))
            except OSError:
                pass
            for p in candidates:
                try:
                    async with http.get(f"http://127.0.0.1:{p}/json/version") as resp:
                        if resp.status == 200:
                            return p
                except Exception:
                    pass
            await asyncio.sleep(0.3)
    raise TimeoutError(f"browser never opened its DevTools port ({port})")


def _launch(browser: str, args: list, stderr_path: str):
    """Start the browser with plain Popen -- not asyncio.create_subprocess_exec:
    DCSServerBot runs on the Windows *selector* event loop (psycopg needs it),
    which has no subprocess support (a bare NotImplementedError). procman
    spawns bfdb the same way. stderr goes to a file for the failure report."""
    err = open(stderr_path, "wb")
    try:
        proc = subprocess.Popen([browser, *args], stdin=subprocess.DEVNULL,
                                stdout=subprocess.DEVNULL, stderr=err)
    except Exception:
        err.close()
        raise
    return proc, err


async def _stop(proc, err) -> None:
    if proc.poll() is None:
        proc.kill()
        try:
            await asyncio.get_running_loop().run_in_executor(None, proc.wait, 10)
        except subprocess.TimeoutExpired:
            pass
    try:
        err.close()
    except OSError:
        pass


# Flags every launch shares. --do-not-de-elevate: Edge started elevated
# otherwise relaunches itself de-elevated; other Chromium builds ignore it.
_COMMON = ["--headless=new", "--disable-gpu", "--hide-scrollbars", "--mute-audio",
           "--no-first-run", "--no-default-browser-check", "--disable-extensions",
           "--disable-background-networking", "--disable-sync", "--do-not-de-elevate",
           "--enable-logging=stderr", "--v=0"]

# A real snapshot is ~150-350 KB; the dark page with nothing drawn is ~5 KB.
_BLANK_BYTES = 20_000


async def _screenshot_mode(browser: str, url: str, base: str, size, why: str) -> bytes:
    """Fallback without DevTools: the browser's one-shot --screenshot mode,
    which writes the PNG itself once the page settles (--virtual-time-budget
    covers the data fetch and the basemap tiles). Can't check the page's
    ready flag, so a near-empty image is refused instead of posted."""
    profile = _fresh_profile(base)
    out = os.path.join(profile, "shot.png")
    stderr_path = os.path.join(profile, "browser-stderr.log")
    w, h = size
    proc, err = _launch(browser, [*_COMMON, f"--user-data-dir={profile}", f"--window-size={w},{h}",
                                  "--virtual-time-budget=25000", f"--screenshot={out}", url], stderr_path)
    try:
        deadline = time.monotonic() + 60
        last = -1
        while time.monotonic() < deadline:
            await asyncio.sleep(1.0)
            try:
                n = os.path.getsize(out)
            except OSError:
                continue
            if n > 0 and n == last:  # written and no longer growing
                with open(out, "rb") as f:
                    png = f.read()
                if len(png) < _BLANK_BYTES:
                    raise RuntimeError(f"--screenshot came out blank ({len(png)} bytes)")
                return png
            last = n
        raise TimeoutError(f"{why}; --screenshot fallback wrote nothing in 60s either "
                           f"(exit={proc.poll()}, stderr: {_tail(stderr_path)})")
    finally:
        await _stop(proc, err)
        shutil.rmtree(profile, ignore_errors=True)


def _fresh_profile(base: str) -> str:
    """A new, empty profile directory under `base` for one capture, after
    clearing earlier ones. A reused profile that a leftover browser still
    holds makes the next launch hand over to that browser and exit, and no
    new DevTools port is ever written."""
    os.makedirs(base, exist_ok=True)
    for name in os.listdir(base):
        if name.startswith("run-"):
            shutil.rmtree(os.path.join(base, name), ignore_errors=True)
    path = os.path.join(base, f"run-{os.getpid()}-{int(time.time() * 1000)}")
    os.makedirs(path, exist_ok=True)
    return path


async def capture(browser: str, url: str, profile: str, size=DEFAULT_SIZE,
                  timeout: float = 45.0, log=None) -> Optional[bytes]:
    """PNG of `url` at `size`, or None when the page never became a snapshot
    (an old bfweb without the /snapshot route falls through to the SITREP
    page -- that is not posted). Raises on browser/protocol failures."""
    import aiohttp

    base = profile
    profile = _fresh_profile(base)
    stderr_path = os.path.join(profile, "browser-stderr.log")
    w, h = size
    want = _free_port()
    proc, err = _launch(browser, [*_COMMON, f"--remote-debugging-port={want}", "--remote-allow-origins=*",
                                  f"--user-data-dir={profile}", f"--window-size={w},{h}", "about:blank"],
                        stderr_path)
    started = time.monotonic()
    port = None
    try:
        try:
            port = await _wait_devtools(want, profile, 20.0)
        except TimeoutError as ex:
            why = f"{ex} (exit={proc.poll()}, stderr: {_tail(stderr_path)})"
            await _stop(proc, err)
            if log:
                log.warning(f"FowlEngine: map snapshot: {why} -- trying the one-shot --screenshot mode")
            return await _screenshot_mode(browser, url, base, size, why)
        async with aiohttp.ClientSession() as http:
            async with http.get(f"http://127.0.0.1:{port}/json/list") as resp:
                targets = await resp.json(content_type=None)
            page = next((t for t in targets if t.get("type") == "page"), None)
            if not page:
                raise RuntimeError("browser has no page target")
            async with http.ws_connect(page["webSocketDebuggerUrl"], max_msg_size=0) as ws:
                cdp = _Cdp(ws)
                await cdp.call("Emulation.setDeviceMetricsOverride",
                               {"width": w, "height": h, "deviceScaleFactor": 1, "mobile": False})
                await cdp.call("Page.enable")
                await cdp.call("Page.navigate", {"url": url})
                probe = ("JSON.stringify({p: location.pathname,"
                         " r: document.documentElement.getAttribute('data-snapshot-ready'),"
                         " m: !!document.querySelector('.leaflet-container')})")
                state = {}
                while time.monotonic() - started < timeout:
                    await asyncio.sleep(0.5)
                    try:
                        res = await cdp.call("Runtime.evaluate", {"expression": probe, "returnByValue": True})
                        state = json.loads(res.get("result", {}).get("value") or "{}")
                    except (RuntimeError, ValueError):
                        continue  # mid-navigation
                    if state.get("r") == "1":
                        break
                if state.get("p") != "/snapshot":
                    if log:
                        log.warning(f"FowlEngine: map snapshot page redirected to {state.get('p')!r} -- "
                                    f"the deployed bfweb predates /snapshot (rebuild bfdb)")
                    return None
                if state.get("r") != "1" and not state.get("m"):
                    # Not a slow basemap: the page never drew a map at all,
                    # so a capture would be a blank rectangle. Usually its
                    # scripts were refused (a bfdb before the CORS fix answers
                    # 403 to pages opened at an address outside --cors-origin)
                    # or its data calls failed.
                    if log:
                        log.warning(f"FowlEngine: map snapshot page at {url} never rendered a map -- not posting "
                                    f"a blank picture (bfdb unreachable, or its scripts blocked: open the URL "
                                    f"in a browser and check the console)")
                    return None
                if state.get("r") != "1" and log:
                    log.info("FowlEngine: map snapshot not ready in time (basemap slow?) -- capturing as-is")
                await asyncio.sleep(0.4)  # let the last tiles paint
                shot = await cdp.call("Page.captureScreenshot", {"format": "png"}, timeout=20.0)
                return base64.b64decode(shot["data"])
    finally:
        if port is not None:
            await _close_browser(port)
        await _stop(proc, err)
        shutil.rmtree(profile, ignore_errors=True)  # a straggler's lock -> the next run clears it


async def _close_browser(port: int) -> None:
    """Browser.close over the browser-level DevTools socket. Reaches the
    browser even when it is a relaunched process we hold no handle to."""
    import aiohttp
    try:
        async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=10)) as http:
            async with http.get(f"http://127.0.0.1:{port}/json/version") as resp:
                ver = await resp.json(content_type=None)
            async with http.ws_connect(ver["webSocketDebuggerUrl"]) as ws:
                await ws.send_str(json.dumps({"id": 1, "method": "Browser.close"}))
                try:
                    await ws.receive(timeout=3)
                except (asyncio.TimeoutError, TimeoutError):
                    pass
    except Exception:
        pass  # already gone, or unreachable; the process kill below still runs
