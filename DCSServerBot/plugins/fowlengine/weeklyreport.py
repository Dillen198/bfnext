# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See DCSServerBot/plugins/fowlengine/LICENSE and the repository NOTICE file.
"""
The weekly campaign report poster -> Discord. Pure logic; the sends live in
cog_report.py and the OPS API routes in opsapi.py.

The poster is drawn by the dashboard (bfweb `/report/print`, bfrsr-web too)
from bfdb's `/api/report/weekly`. Two ways it reaches Discord:

* An admin makes it on the dashboard's REPORT page (`/admin/report`), edits
  the dispatch and the post text, and presses Post to Discord. The PNG and the
  text come through bfdb's OPS proxy to `POST <ops>/report/post`.
* The optional weekly schedule (off by default; per server, `weekly_report:`
  in its section of fowlengine.yaml). The bot opens `/report/print` in the
  same headless Edge/Chrome as the status map (mapsnap.py), which draws the
  poster with the automatic dispatch and hands back the post text, and posts
  both to the server's `report_channel`.

    report_channel: 123456789012345678   # else news_channel
    weekly_report:
      enabled: false        # the schedule; posting from the dashboard works either way
      weekday: sun          # mon..sun, or 0 (Monday) .. 6
      hour: 18              # UTC
      days: 7               # how far back the report looks
"""
from __future__ import annotations

import base64
import re
from datetime import datetime, timedelta, timezone
from typing import Optional
from urllib.parse import quote

__all__ = [
    "WEEKDAYS", "MAX_TEXT", "MAX_IMAGE_BYTES", "CATCH_UP", "POLL_SECS",
    "parse_schedule", "last_slot", "due", "print_url", "channel_key",
    "check_upload", "safe_filename", "fallback_text", "status",
]

WEEKDAYS = ("mon", "tue", "wed", "thu", "fri", "sat", "sun")
# Discord's limit on a message's text.
MAX_TEXT = 2000
# Under every guild's upload limit (10 MB without boosts).
MAX_IMAGE_BYTES = 8 * 1024 * 1024
# A slot the bot missed (it was down) is still posted within this long of
# it; after that the week is skipped rather than posted a day late.
CATCH_UP = timedelta(hours=6)
POLL_SECS = 300

_PNG = b"\x89PNG\r\n\x1a\n"
_JPEG = b"\xff\xd8\xff"


def _weekday(raw) -> Optional[int]:
    if isinstance(raw, bool):
        return None
    if isinstance(raw, int):
        return raw if 0 <= raw <= 6 else None
    if isinstance(raw, str):
        s = raw.strip().lower()[:3]
        if s in WEEKDAYS:
            return WEEKDAYS.index(s)
        if s.isdigit():
            return _weekday(int(s))
    return None


def parse_schedule(cfg: Optional[dict]) -> Optional[dict]:
    """A server's `weekly_report:` as {"weekday": 0-6 (Monday 0), "hour":
    0-23 UTC, "days": 1-31}, or None when it is off, missing or unreadable.
    Off unless `enabled: true` -- the schedule posts without anyone looking."""
    if not isinstance(cfg, dict) or cfg.get("enabled") is not True:
        return None
    wd = _weekday(cfg.get("weekday", "sun"))
    try:
        hour = int(cfg.get("hour", 18))
        days = int(cfg.get("days", 7))
    except (TypeError, ValueError):
        return None
    if wd is None or not 0 <= hour <= 23:
        return None
    return {"weekday": wd, "hour": hour, "days": max(1, min(days, 31))}


def last_slot(now: datetime, sched: dict) -> datetime:
    """The latest scheduled time at or before `now` (UTC)."""
    now = now.astimezone(timezone.utc)
    slot = now.replace(hour=sched["hour"], minute=0, second=0, microsecond=0)
    slot -= timedelta(days=(now.weekday() - sched["weekday"]) % 7)
    if slot > now:
        slot -= timedelta(days=7)
    return slot


def due(now: datetime, sched: Optional[dict], last_posted: Optional[str]) -> Optional[str]:
    """The slot to post now, as an ISO time (the state to keep once it is
    posted), or None: nothing scheduled, already posted, or missed by more
    than CATCH_UP."""
    if not sched:
        return None
    slot = last_slot(now, sched)
    key = slot.isoformat()
    if key == last_posted or now.astimezone(timezone.utc) - slot > CATCH_UP:
        return None
    return key


def print_url(base: str, server_name: Optional[str], instance: Optional[str] = None, days: int = 7) -> str:
    """The dashboard's chromeless poster page for one server. bfdb serves the
    dashboard itself, so `base` is usually its api_url (`status_map_url`)."""
    url = f"{base.rstrip('/')}/report/print?days={int(days)}"
    if instance:
        url += f"&instance={quote(instance)}"
    elif server_name:
        url += f"&server={quote(server_name)}"
    return url


def channel_key(config: dict) -> Optional[str]:
    """Which channel setting a server's reports go to: report_channel, else
    its news_channel; None with neither."""
    for k in ("report_channel", "news_channel"):
        if config.get(k):
            return k
    return None


def safe_filename(raw: Optional[str], ext: str) -> str:
    stem = re.sub(r"[^A-Za-z0-9_-]+", "-", (raw or "").rsplit(".", 1)[0]).strip("-")[:80]
    return f"{stem or 'weekly-report'}.{ext}"


def check_upload(doc: dict) -> tuple[bytes, str, str]:
    """(image bytes, post text, file name) from the dashboard's post, or
    ValueError saying what is wrong with it."""
    if not isinstance(doc, dict):
        raise ValueError("expected a JSON object")
    text = doc.get("text")
    if not isinstance(text, str) or not text.strip():
        raise ValueError("the post has no text")
    if len(text) > MAX_TEXT:
        raise ValueError(f"the post text is {len(text)} characters; Discord takes {MAX_TEXT}")
    raw = doc.get("image")
    if not isinstance(raw, str) or not raw:
        raise ValueError("the post has no poster image")
    if raw.startswith("data:"):
        raw = raw.split(",", 1)[-1]
    try:
        img = base64.b64decode(raw, validate=True)
    except (ValueError, TypeError) as ex:
        raise ValueError(f"the poster is not base64: {ex}") from None
    if len(img) > MAX_IMAGE_BYTES:
        raise ValueError(f"the poster is {len(img) // 1024} KB; the limit is {MAX_IMAGE_BYTES // 1024} KB")
    if img.startswith(_PNG):
        ext = "png"
    elif img.startswith(_JPEG):
        ext = "jpg"
    else:
        raise ValueError("the poster is not a PNG or JPEG")
    return img, text.strip(), safe_filename(doc.get("filename"), ext)


def fallback_text(brand: str, url: Optional[str]) -> str:
    """What a scheduled post says when the page gave no text of its own."""
    lines = [f"**{brand}: the week at the front**"]
    if url:
        lines.append(url)
    return "\n".join(lines)


def status(sched: Optional[dict], last_posted: Optional[str], channel: Optional[str], now: datetime) -> dict:
    """The REPORT page's view of a server's schedule."""
    nxt = None
    if sched:
        slot = last_slot(now, sched)
        nxt = (slot if slot.isoformat() != last_posted and now - slot <= CATCH_UP
               else slot + timedelta(days=7)).isoformat()
    return {
        "scheduled": bool(sched),
        "weekday": WEEKDAYS[sched["weekday"]] if sched else None,
        "hour": sched["hour"] if sched else None,
        "days": sched["days"] if sched else None,
        "next": nxt,
        "last_posted": last_posted,
        "channel": channel,
    }
