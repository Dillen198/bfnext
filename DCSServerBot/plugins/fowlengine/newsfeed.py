# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See DCSServerBot/plugins/fowlengine/LICENSE and the repository NOTICE file.
"""
War diary -> Discord, pure logic only (no discord / aiohttp imports, like
rangefeed.py). commands.py does the HTTP and the sends.

bfdb files one dispatch per campaign day (bfdb/src/news.rs); once a day is
FILED (`final_`) it never changes, and when bfdb has an image endpoint set it
draws one picture for it a few minutes later (bfdb/src/news_image.rs). This
posts each newly filed dispatch to the server's `news_channel` once:

  * Only filed days. The running day's headline moves all day.
  * Deduplicated by `<round>:<day>`, persisted in the plugin state file, so a
    bot restart never reposts.
  * First enable posts the newest filed dispatch only -- no backfill.
  * If the picture is still being drawn (`image_pending`), the post waits up to
    IMAGE_WAIT_SECS for it, then goes out without one. It is never edited in
    afterwards: a late picture still shows on the dashboard, and one simple
    post beats an edit path that can fail halfway.

bfdb route consumed (public, instance-scoped):
  GET /api/news?limit=N   {"days": [NewsDay + image, image_pending], "images": bool}
"""
from __future__ import annotations

from urllib.parse import quote

__all__ = [
    "NEWS_POLL_SECS", "FETCH_LIMIT", "IMAGE_WAIT_SECS", "POSTED_KEEP", "IMAGE_MAX_BYTES",
    "dispatch_id", "filed_days", "plan", "mark_posted", "build_embed", "image_path",
    "attachment_name", "diary_url",
]

# How often the feed looks for a newly filed dispatch. A day is filed once, a
# little after UTC midnight, so there is no hurry.
NEWS_POLL_SECS = 120
# Dispatches asked for per poll: enough to catch up after a day or two down.
FETCH_LIMIT = 5
# How long a filed dispatch waits for its picture before posting without.
IMAGE_WAIT_SECS = 600
# Posted ids remembered per server.
POSTED_KEEP = 60
# bfdb caps stored pictures at 8 MiB; anything bigger is not one of them.
IMAGE_MAX_BYTES = 8 * 1024 * 1024

# Discord embed limits.
_TITLE_MAX = 256
_DESC_MAX = 4096
_FOOTER_MAX = 2048
# The dispatch's own text is cut here, leaving room for the "Read more" link.
_STORY_MAX = 3600
_COLOR = 0x8A6D3B  # newsprint / khaki


def dispatch_id(day: dict) -> str | None:
    """`<round>:<YYYY-MM-DD>` -- a day key alone repeats when a new round
    starts on the same date."""
    d = day.get("day") if isinstance(day, dict) else None
    if not d:
        return None
    return f"{day.get('round', 0)}:{d}"


def filed_days(data) -> list[dict]:
    """The filed (final) dispatches in an /api/news reply, oldest first."""
    days = data.get("days") if isinstance(data, dict) else None
    out = [d for d in (days or []) if isinstance(d, dict) and d.get("final_") and dispatch_id(d)]
    out.sort(key=lambda d: (d.get("round", 0), d.get("day", "")))
    return out


def plan(data, state: dict | None, now: float, image_wait: float = IMAGE_WAIT_SECS):
    """What to post this poll.

    Returns (to_post, new_state). `to_post` is oldest first. `state` is this
    server's persisted `{"posted": [ids], "seen": {id: first seen epoch}}` or
    None the first time the feed runs for a server.

    Stops at the first dispatch still waiting for its picture, so posts stay
    in date order."""
    filed = filed_days(data)
    if state is None:
        # First enable: everything but the newest counts as already posted.
        state = {"posted": [dispatch_id(d) for d in filed[:-1]], "seen": {}}
    posted = list(state.get("posted") or [])
    seen = dict(state.get("seen") or {})
    done = set(posted)
    to_post = []
    for d in filed:
        did = dispatch_id(d)
        if did in done:
            continue
        first = seen.setdefault(did, now)
        if not d.get("image") and d.get("image_pending") and now - first < image_wait:
            break
        to_post.append(d)
    # Only ids still in play keep a first-seen time.
    live = {dispatch_id(d) for d in filed} - done
    seen = {k: v for k, v in seen.items() if k in live}
    return to_post, {"posted": posted[-POSTED_KEEP:], "seen": seen}


def mark_posted(state: dict, day: dict) -> dict:
    """`state` with this dispatch recorded as posted (and no longer waited on)."""
    did = dispatch_id(day)
    posted = [p for p in (state.get("posted") or []) if p != did] + [did]
    seen = {k: v for k, v in (state.get("seen") or {}).items() if k != did}
    return {"posted": posted[-POSTED_KEEP:], "seen": seen}


def _clip(s: str, n: int) -> str:
    s = s or ""
    return s if len(s) <= n else s[: max(0, n - 1)].rstrip() + "…"


def diary_url(dashboard_url: str | None, instance_id: str | None) -> str | None:
    """The dashboard's War Diary page for this server, or None without a
    dashboard_url."""
    if not dashboard_url:
        return None
    url = dashboard_url.rstrip("/") + "/news"
    if instance_id:
        url += f"?instance={quote(str(instance_id), safe='')}"
    return url


def build_embed(day: dict, dashboard_url: str | None = None,
                instance_id: str | None = None) -> dict:
    """An embed dict (clipped to Discord's limits) for one dispatch: the
    headline, the dispatch text (or the analysis's sentences when bfdb had no
    writer), a link to the full diary, and the day in the footer."""
    body = [p for p in (day.get("body") or []) if isinstance(p, str) and p.strip()]
    if not body:
        body = [it.get("text", "") for it in (day.get("items") or [])[:5]
                if isinstance(it, dict) and it.get("text")]
    story = _clip("\n\n".join(body), _STORY_MAX)
    link = diary_url(dashboard_url, instance_id)
    desc = story + (f"\n\n[Read more in the War Diary]({link})" if link else "")
    facts = day.get("facts") or {}
    bits = []
    if facts.get("campaign_day"):
        bits.append(f"Day {facts['campaign_day']}")
    bits.append(day.get("day", ""))
    if facts.get("theatre"):
        bits.append(facts["theatre"])
    out = {
        "title": _clip(day.get("headline") or "War Diary", _TITLE_MAX),
        "description": _clip(desc, _DESC_MAX),
        "color": _COLOR,
        "footer": _clip(" · ".join(b for b in bits if b), _FOOTER_MAX),
    }
    if link:
        out["url"] = link
    return out


def image_path(day: dict) -> str | None:
    """The bfdb path of the dispatch's picture, or None. Only bfdb's own
    image route is ever followed -- nothing else in the reply is fetched."""
    p = day.get("image")
    if isinstance(p, str) and p.startswith("/api/news/image/"):
        return p
    return None


def attachment_name(day: dict, content_type: str | None = None) -> str:
    ext = {"image/jpeg": "jpg", "image/webp": "webp"}.get((content_type or "").split(";")[0].strip(), "png")
    safe = "".join(c for c in str(day.get("day", "dispatch")) if c.isalnum() or c == "-") or "dispatch"
    return f"dispatch-{safe}.{ext}"
