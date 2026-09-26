# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See DCSServerBot/plugins/fowlengine/LICENSE and the repository NOTICE file.
"""
Training range (bfrange) helpers for the FowlEngine cog -- pure logic only.

Nothing in here imports discord, aiohttp or the bot: every function takes and
returns plain dicts/lists/strings, so it can be exercised without a running
bot. commands.py does the I/O (bfdb HTTP, Discord sends) and turns the embed
dicts built here into discord.Embed objects.

bfdb routes this consumes (all instance-scoped, `?instance=<id>` or
`?server=<DCS server name>`):
  GET /api/range/feed?limit=N&kind=       {"items": [Summary]}, newest first
  GET /api/range/result/{id}/discord      ready-made embed JSON
  GET /api/range/live                     {"live": RangeLive | null}
  GET /api/range/greenie?days=N           the carrier greenie board
Wire types: bfprotocols/src/range/mod.rs.
"""
from __future__ import annotations

import re
from datetime import datetime, timezone
from typing import Iterable, Optional
from urllib.parse import quote, urlparse

__all__ = [
    "RANGE_SITE_URL", "FEED_LIMIT", "MAX_POSTS_PER_POLL", "RECENT_IDS_KEEP",
    "normalize_kind", "find_instance", "instance_kind", "parse_ts", "item_id",
    "item_kind", "feed_items", "select_new", "next_cursor", "parse_kinds", "filter_kinds",
    "split_cap", "summary_line", "normalize_embed", "fallback_embed", "card_url",
    "attachment_name", "build_status", "greenie_rows", "greenie_lines", "pilot_url",
]

RANGE_SITE_URL = "https://range.vectorstrike.org"
# How many results one feed poll asks bfdb for, and how many of them may be
# posted individually per poll -- the rest collapse into one summary line.
FEED_LIMIT = 20
MAX_POSTS_PER_POLL = 10
# Ids already seen, kept with the cursor so a reordered or re-served page can
# never post the same result twice.
RECENT_IDS_KEEP = 50

# Discord embed limits.
_TITLE_MAX = 256
_DESC_MAX = 4096
_FIELDS_MAX = 25
_FIELD_NAME_MAX = 256
_FIELD_VALUE_MAX = 1024
_FOOTER_MAX = 2048
_TOTAL_MAX = 6000
_ZWSP = "​"

_MIN_TS = datetime.min.replace(tzinfo=timezone.utc)


# ---- instance kind ----------------------------------------------------------

def normalize_kind(val) -> str:
    """"range" or "campaign" (the default for anything else, incl. unset)."""
    return "range" if str(val or "").strip().lower() == "range" else "campaign"


def find_instance(instances, server_name: Optional[str]) -> Optional[dict]:
    """The instance entry whose `dcs_server_name` is this DCS server.

    `instances` is either the rendered instances.json payload
    ({"instances": [...]}) or the YAML `bfdb.instances` list."""
    if not server_name:
        return None
    if isinstance(instances, dict):
        instances = instances.get("instances")
    for inst in instances or []:
        if isinstance(inst, dict) and inst.get("dcs_server_name") == server_name:
            return inst
    return None


def instance_kind(instances, server_name: Optional[str]) -> str:
    return normalize_kind((find_instance(instances, server_name) or {}).get("kind"))


# ---- result feed ------------------------------------------------------------

def parse_ts(raw) -> Optional[datetime]:
    """bfdb/chrono RFC 3339 -> aware datetime. Tolerates 'Z', 0-9 fractional
    digits (fromisoformat only takes up to 6) and a missing offset (UTC)."""
    if not raw or not isinstance(raw, str):
        return None
    s = raw.strip()
    if s[-1:] in ("Z", "z"):
        s = s[:-1] + "+00:00"
    m = re.match(r"^(\d{4}-\d{2}-\d{2})[T ](\d{2}:\d{2}:\d{2})(?:\.(\d+))?(?:([+-]\d{2}):?(\d{2}))?$", s)
    if not m:
        return None
    # exactly 6 fractional digits (py3.10's fromisoformat takes only 3 or 6)
    frac = "." + (m.group(3) or "")[:6].ljust(6, "0")
    offset = f"{m.group(4)}:{m.group(5)}" if m.group(4) else "+00:00"
    try:
        dt = datetime.fromisoformat(f"{m.group(1)}T{m.group(2)}{frac}{offset}")
    except ValueError:
        return None
    return dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)


def item_id(item) -> Optional[str]:
    if not isinstance(item, dict):
        return None
    rid = item.get("id")
    return str(rid) if rid not in (None, "") else None


def item_kind(item) -> str:
    """A summary's result kind: top-level `kind`, else `result.kind`."""
    if not isinstance(item, dict):
        return ""
    k = item.get("kind")
    if not k and isinstance(item.get("result"), dict):
        k = item["result"].get("kind")
    return str(k or "").strip().lower().replace("-", "_")


def feed_items(payload) -> list:
    """The item list out of a /api/range/feed answer, newest first."""
    if isinstance(payload, dict):
        payload = payload.get("items")
    items = [i for i in (payload or []) if isinstance(i, dict) and item_id(i)]
    # The route promises newest-first; re-sort defensively when every item
    # carries a parseable timestamp (stable, so ties keep bfdb's order).
    stamps = [parse_ts(i.get("ts")) for i in items]
    if items and all(stamps):
        order = sorted(range(len(items)), key=lambda n: stamps[n], reverse=True)
        items = [items[n] for n in order]
    return items


def select_new(items: list, cursor: Optional[dict], limit: int = FEED_LIMIT) -> tuple[list, bool]:
    """Which feed items have not been posted yet.

    items   one feed page, newest first (feed_items()).
    cursor  {"id", "ts", "recent"} from next_cursor(), or None for a server
            that has never been polled -- which posts nothing: a fresh bot
            must not replay the whole history as new results.

    Returns (new items OLDEST first, overflow). `overflow` means the cursor was
    not on this page and even its oldest item was new, so more results may
    have arrived than one page holds."""
    if cursor is None:
        return [], False
    cid = cursor.get("id")
    cts = parse_ts(cursor.get("ts"))
    recent = set(cursor.get("recent") or [])
    new: list = []
    found = False
    for it in items:
        rid = item_id(it)
        if cid is not None and rid == cid:
            found = True
            break
        new.append(it)
    if not found and cid is not None and cts is not None:
        # The cursor fell off the page (a burst bigger than a page, or bfdb's
        # feed was rebuilt): keep only what is strictly newer than it.
        new = [it for it in new if (parse_ts(it.get("ts")) or _MIN_TS) > cts]
    seen: set = set()
    fresh = []
    for it in new:
        rid = item_id(it)
        if rid in recent or rid in seen:
            continue
        seen.add(rid)
        fresh.append(it)
    overflow = bool(
        not found and items and len(items) >= limit and fresh
        and item_id(fresh[-1]) == item_id(items[-1])
    )
    fresh.reverse()
    return fresh, overflow


def next_cursor(items: list, cursor: Optional[dict]) -> dict:
    """The cursor after processing `items` (newest first): the newest item,
    plus every id on the page folded into `recent`. An empty first page still
    yields a cursor, so the very first result after it is posted."""
    cursor = dict(cursor or {})
    recent = [item_id(i) for i in items if item_id(i)]
    for rid in cursor.get("recent") or []:
        if rid not in recent:
            recent.append(rid)
    out = {
        "id": cursor.get("id"),
        "ts": cursor.get("ts"),
        "recent": recent[:RECENT_IDS_KEEP],
    }
    if items:
        newest = items[0]
        new_ts = parse_ts(newest.get("ts"))
        old_ts = parse_ts(out["ts"])
        # Only move forward -- unless the old position is gone from bfdb
        # altogether (no timestamp to compare), then trust the page.
        if old_ts is None or new_ts is None or new_ts >= old_ts or out["id"] is None:
            out["id"] = item_id(newest)
            out["ts"] = newest.get("ts")
    return out


def parse_kinds(kinds) -> Optional[list]:
    """`range_results_kinds` from YAML -> a clean list, or None for "all".
    Takes a list ([trap, bomb]) or a string ("trap, bomb")."""
    if not kinds:
        return None
    if isinstance(kinds, str):
        kinds = re.split(r"[,\s]+", kinds)
    out = [str(k).strip().lower().replace("-", "_") for k in kinds if str(k).strip()]
    return out or None


def filter_kinds(items: list, kinds) -> list:
    """Keep items whose kind is in `kinds` (case-insensitive, '-' == '_').
    None/empty keeps everything."""
    want = set(parse_kinds(kinds) or [])
    if not want:
        return list(items)
    return [i for i in items if item_kind(i) in want]


def split_cap(items: list, cap: int = MAX_POSTS_PER_POLL) -> tuple[list, list]:
    """(older items to summarise, newest `cap` items to post), both oldest
    first."""
    if cap <= 0:
        return list(items), []
    if len(items) <= cap:
        return [], list(items)
    return list(items[:-cap]), list(items[-cap:])


def _kind_label(kind: str) -> str:
    return {"aar": "AAR", "cas": "CAS", "anti_ship": "anti-ship"}.get(kind, kind.replace("_", " ") or "other")


def summary_line(skipped: list, overflow: bool, site_url: str = RANGE_SITE_URL,
                 noun: str = "range result") -> Optional[str]:
    """One line standing in for everything not posted individually."""
    if not skipped and not overflow:
        return None
    counts: dict = {}
    for it in skipped:
        k = _kind_label(item_kind(it))
        counts[k] = counts.get(k, 0) + 1
    breakdown = " · ".join(f"{n} {k}" for k, n in sorted(counts.items(), key=lambda kv: (-kv[1], kv[0])))
    site = (site_url or RANGE_SITE_URL).rstrip("/")
    plural = noun + ("es" if noun.endswith(("s", "sh", "ch", "x")) else "s")
    if skipped:
        n = len(skipped)
        head = (f"⏩ {n}{'+' if overflow else ''} earlier {noun if n == 1 and not overflow else plural} "
                f"not posted one by one (catching up)")
        return f"{head}: {breakdown} — all of them at <{site}>"
    return f"⏩ More {plural} arrived than one poll holds — the older ones are at <{site}>"


# ---- embeds -------------------------------------------------------------------

def _s(val) -> str:
    return "" if val is None else str(val)


def _color(val) -> Optional[int]:
    if val is None or isinstance(val, bool):
        return None
    if isinstance(val, (int, float)):
        return int(val) & 0xFFFFFF
    s = str(val).strip().lower()
    for prefix in ("#", "0x"):
        if s.startswith(prefix):
            s = s[len(prefix):]
    try:
        return int(s, 16) & 0xFFFFFF if re.fullmatch(r"[0-9a-f]{1,6}", s) else None
    except ValueError:
        return None


def _url(val) -> Optional[str]:
    if isinstance(val, dict):
        val = val.get("url")
    s = _s(val).strip()
    return s if s.lower().startswith(("http://", "https://")) else None


def _clip(s: str, n: int) -> str:
    return s if len(s) <= n else s[: max(0, n - 1)] + "…"


def normalize_embed(data) -> dict:
    """bfdb's /discord embed JSON -> a dict that is safe to hand to Discord:
    keys title/description/url/color/fields/footer/image/thumbnail, every
    string clipped to Discord's limits and the 6000-char total respected
    (description trimmed first, then trailing fields dropped)."""
    d = data if isinstance(data, dict) else {}
    if isinstance(d.get("embed"), dict):
        d = d["embed"]
    elif isinstance(d.get("embeds"), list) and d["embeds"] and isinstance(d["embeds"][0], dict):
        d = d["embeds"][0]
    footer = d.get("footer")
    if isinstance(footer, dict):
        footer = footer.get("text")
    out = {
        "title": _clip(_s(d.get("title")).strip(), _TITLE_MAX),
        "description": _clip(_s(d.get("description")).strip(), _DESC_MAX),
        "url": _url(d.get("url")),
        "color": _color(d.get("color", d.get("colour"))),
        "fields": [],
        "footer": _clip(_s(footer).strip(), _FOOTER_MAX),
        "image": _url(d.get("image")),
        "thumbnail": _url(d.get("thumbnail")),
    }
    for f in (d.get("fields") or [])[:_FIELDS_MAX]:
        if not isinstance(f, dict):
            continue
        out["fields"].append({
            "name": _clip(_s(f.get("name")).strip(), _FIELD_NAME_MAX) or _ZWSP,
            "value": _clip(_s(f.get("value")).strip(), _FIELD_VALUE_MAX) or _ZWSP,
            "inline": bool(f.get("inline", False)),
        })

    def total() -> int:
        return (len(out["title"]) + len(out["description"]) + len(out["footer"])
                + sum(len(f["name"]) + len(f["value"]) for f in out["fields"]))

    over = total() - _TOTAL_MAX
    if over > 0 and out["description"]:
        keep = max(0, len(out["description"]) - over)
        out["description"] = _clip(out["description"], keep) if keep else ""
    while total() > _TOTAL_MAX and out["fields"]:
        out["fields"].pop()
    return out


def fallback_embed(item: dict, site_url: str = RANGE_SITE_URL) -> dict:
    """What gets posted when bfdb's /discord route fails: the feed headline."""
    kind = item_kind(item)
    site = (site_url or RANGE_SITE_URL).rstrip("/")
    return normalize_embed({
        "title": _kind_label(kind).upper() if kind else "Range result",
        "description": _s(item.get("headline")) or "New training range result.",
        "url": site,
        "footer": _host(site),
    })


def card_url(api_url: str, summary: dict, embed: Optional[dict] = None) -> Optional[str]:
    """Where to download the result card PNG from: the summary's relative
    `card_png` against the bot's own bfdb URL (no public round trip), else the
    embed's absolute image URL."""
    rel = _s((summary or {}).get("card_png") or (summary or {}).get("card_url")).strip()
    if rel:
        if rel.lower().startswith(("http://", "https://")):
            return rel
        return f"{(api_url or '').rstrip('/')}/{rel.lstrip('/')}"
    return (embed or {}).get("image")


def attachment_name(result_id: Optional[str]) -> str:
    """A Discord-safe attachment filename for a result card."""
    safe = re.sub(r"[^A-Za-z0-9_-]", "_", _s(result_id))[:80] or "result"
    return f"range-{safe}.png"


# ---- live status embed -----------------------------------------------------

def _deg(v) -> str:
    try:
        return f"{int(round(float(v))) % 360:03d}"
    except (TypeError, ValueError):
        return "---"


def _num(v, default=None):
    try:
        return float(v)
    except (TypeError, ValueError):
        return default


def _alt(ft) -> str:
    ft = _num(ft)
    if ft is None:
        return "?"
    if ft >= 18000:
        return f"FL{int(round(ft / 100.0)):03d}"
    return f"{ft:,.0f} ft"


def _duration(secs) -> str:
    secs = int(_num(secs, 0) or 0)
    d, rem = divmod(secs, 86400)
    h, rem = divmod(rem, 3600)
    m = rem // 60
    if d:
        return f"{d}d {h}h"
    return f"{h}h {m:02d}m" if h else f"{m}m"


def _lines_value(lines: list, limit: int = _FIELD_VALUE_MAX, more_noun: str = "more") -> str:
    """Join lines into a field value, cutting at whole lines with a '… +N more'
    (room for that tail is always kept while more lines follow)."""
    out: list = []
    used = 0
    for n, line in enumerate(lines):
        after = len(lines) - n - 1
        reserve = len(f"\n… +{after} {more_noun}") if after else 0
        need = len(line) + (1 if out else 0)
        if used + need + reserve > limit:
            if not out:  # a single over-long first line: clip it rather than lose it
                out.append(_clip(line, max(1, limit - reserve)))
                used = len(out[0])
                continue
            out.append(f"… +{len(lines) - n} {more_noun}")
            break
        out.append(line)
        used += need
    return _clip("\n".join(out), limit) or _ZWSP


def _host(url: str) -> str:
    try:
        return urlparse(url).netloc or url
    except ValueError:
        return url


def build_status(live: Optional[dict], site_url: str = RANGE_SITE_URL, *,
                 running: bool = True, error: Optional[str] = None,
                 label: str = "") -> dict:
    """The live range status embed as a dict (see normalize_embed's keys).

    live     RangeLive (bfprotocols) or None
    running  whether the DCS server itself is up
    label    server name, only when more than one server is configured"""
    site = (site_url or RANGE_SITE_URL).rstrip("/")
    title = "Training Range" + (f" — {label}" if label else "")
    link = {"name": "🔗 Range", "value": f"[{_host(site)}]({site}) · live map, results, greenie board",
            "inline": False}
    if not running or not isinstance(live, dict):
        if not running:
            desc = "🔴 **Server offline.** The range comes back with the next restart."
        elif error:
            desc = f"🟠 **No live data** — {error}"
        else:
            desc = "🟠 **No live data** — the range engine isn't reporting (mission loading or restarting)."
        return normalize_embed({"title": title, "url": site, "description": desc,
                                "color": 0x95A5A6, "fields": [link]})

    wind = live.get("wind") or {}
    desc = []
    head = []
    if live.get("theatre"):
        head.append(f"**{live['theatre']}**")
    if live.get("mission_time"):
        head.append(_s(live["mission_time"]))
    head.append("🌙 night" if live.get("night") else "☀️ day")
    desc.append(" · ".join(head))
    if wind:
        w = (f"💨 {_deg(wind.get('surface_from_deg'))}°/{_num(wind.get('surface_kts'), 0):.0f} kt surface"
             f" · {_deg(wind.get('alt_from_deg'))}°/{_num(wind.get('alt_kts'), 0):.0f} kt aloft")
        if _num(wind.get("qnh_hpa")):
            w += f" · QNH {_num(wind.get('qnh_hpa')):.0f}"
        if wind.get("temperature_c") is not None and _num(wind.get("temperature_c")) is not None:
            w += f" · {_num(wind.get('temperature_c')):.0f}°C"
        desc.append(w)
    if _num(live.get("uptime_s")):
        desc.append(f"⏱️ up {_duration(live.get('uptime_s'))}")

    fields = []

    # Tankers
    tankers = [t for t in (live.get("tankers") or []) if isinstance(t, dict)]
    on_station = [t for t in tankers if _s(t.get("state")).lower() == "on_station"]
    lines = []
    for t in on_station:
        parts = [f"**{_s(t.get('callsign')) or '?'}** {_s(t.get('unit_type'))}".strip()]
        if t.get("method"):
            parts.append(_s(t["method"]))
        if t.get("tacan"):
            parts.append(f"TACAN {t['tacan']}")
        if _num(t.get("freq_mhz")):
            parts.append(f"{_num(t.get('freq_mhz')):.3f} MHz")
        parts.append(_alt(t.get("alt_ft")))
        if t.get("receivers"):
            parts.append(f"{len(t['receivers'])} in contact")
        if t.get("recovery_for"):
            parts.append(f"recovery tanker ({t['recovery_for']})")
        lines.append(" · ".join(parts))
    others = len(tankers) - len(on_station)
    if others:
        lines.append(f"*{others} more spawning / RTB*")
    fields.append({"name": f"⛽ Tankers on station — {len(on_station)}",
                   "value": _lines_value(lines) if lines else "None on station.", "inline": False})

    # Carriers
    lines = []
    for c in (live.get("carriers") or []):
        if not isinstance(c, dict):
            continue
        wod = f"WOD {_num(c.get('wind_over_deck_kts'), 0):.0f} kt"
        ang = _num(c.get("wind_over_deck_angle_deg"))
        if ang:
            wod += f" ({ang:+.0f}°)"
        first = (f"**{_s(c.get('name')) or '?'}** · BRC {_deg(c.get('brc_deg'))} / FB {_deg(c.get('fb_deg'))}"
                 f" · {wod}")
        if c.get("recovery_open"):
            case = c.get("case")
            second = [f"🟢 recovery OPEN" + (f" (Case {case})" if case else "")]
        else:
            nxt = c.get("next_window")
            second = ["🔴 deck closed" + (f", next {nxt}" if nxt else "")]
        if c.get("tacan"):
            second.append(f"TACAN {c['tacan']}")
        if c.get("icls") is not None:
            second.append(f"ICLS {c['icls']}")
        if _num(c.get("tower_mhz")):
            second.append(f"Tower {_num(c.get('tower_mhz')):.3f}")
        if c.get("pattern"):
            second.append(f"{len(c['pattern'])} in the pattern")
        lines.append(first + "\n" + " · ".join(second))
    if lines:
        fields.append({"name": f"⚓ Carriers — {len(lines)}", "value": _lines_value(lines), "inline": False})

    # Stations
    stations = [s for s in (live.get("stations") or []) if isinstance(s, dict)]
    hot = [s for s in stations if s.get("hot_by")]
    cold = [s for s in stations if not s.get("hot_by")]
    lines = []
    for s in hot:
        kind = _s(s.get("kind")).replace("_", " ")
        lines.append(f"🔴 **{_s(s.get('name')) or s.get('id')}** ({kind}) — {', '.join(map(str, s['hot_by']))}")
    if cold:
        names = " · ".join(_s(s.get("name")) or _s(s.get("id")) for s in cold)
        lines.append("⚪ " + _clip(names, 700))
    if stations:
        fields.append({"name": f"🎯 Stations — {len(hot)} hot · {len(cold)} cold",
                       "value": _lines_value(lines, more_noun="more hot"), "inline": False})

    # Players
    players = [p for p in (live.get("players") or []) if isinstance(p, dict)]
    lines = []
    for p in players:
        line = f"**{_s(p.get('name')) or '?'}** {_s(p.get('unit_type'))}".strip()
        if p.get("activity"):
            line += f" · {p['activity']}"
        elif p.get("in_air") is False:
            line += " · on deck"
        lines.append(line)
    fields.append({"name": f"👥 Players online — {len(players)}",
                   "value": _lines_value(lines, more_noun="more") if lines else "Nobody flying right now.",
                   "inline": False})
    fields.append(link)

    return normalize_embed({
        "title": title,
        "url": site,
        "description": "\n".join(desc),
        "color": 0x2ECC71 if players else 0x1ABC9C,
        "fields": fields,
    })


# ---- greenie board ---------------------------------------------------------

def pilot_url(site_url: str, ucid: str) -> str:
    return f"{(site_url or RANGE_SITE_URL).rstrip('/')}/pilot/{quote(_s(ucid), safe='')}"


def _first(row: dict, keys: Iterable[str]):
    for k in keys:
        if row.get(k) is not None:
            return row.get(k)
    return None


def greenie_rows(data) -> list[dict]:
    """Normalise a /api/range/greenie answer into
    [{"name", "ucid", "avg", "passes", "grades"}], in bfdb's order.

    Tolerant of the exact shape: a bare list, or a dict holding the list under
    pilots / board / items / rows; a row's pilot either flat (name/ucid) or as
    a PilotRef under `pilot`."""
    rows = data
    if isinstance(data, dict):
        rows = _first(data, ("pilots", "board", "items", "rows", "greenie")) or []
    out = []
    for r in rows or []:
        if not isinstance(r, dict):
            continue
        pilot = r.get("pilot")
        name = r.get("name")
        ucid = r.get("ucid")
        if isinstance(pilot, dict):
            name = name or pilot.get("name")
            ucid = ucid or pilot.get("ucid")
        elif isinstance(pilot, str):
            name = name or pilot
        avg = _num(_first(r, ("avg", "average", "avg_points", "points_avg", "gpa", "points", "score")))
        passes = _first(r, ("passes", "count", "traps", "n", "num_passes"))
        try:
            passes = int(passes) if passes is not None else None
        except (TypeError, ValueError):
            passes = None
        grades = _first(r, ("grades", "last", "recent", "last_grades")) or []
        if isinstance(grades, str):
            grades = grades.split()
        grades = [(g.get("grade") if isinstance(g, dict) else g) for g in grades]
        grades = [_s(g) for g in grades if g]
        out.append({"name": _s(name) or "?", "ucid": _s(ucid) or None, "avg": avg,
                    "passes": passes, "grades": grades})
    return out


def greenie_lines(data, site_url: str = RANGE_SITE_URL, limit: int = 10) -> list[str]:
    medals = {1: "🥇", 2: "🥈", 3: "🥉"}
    lines = []
    for n, r in enumerate(greenie_rows(data)[:limit], 1):
        who = f"[{r['name']}]({pilot_url(site_url, r['ucid'])})" if r["ucid"] else r["name"]
        parts = [f"{medals.get(n, f'`{n:>2}.`')} **{who}**"]
        if r["avg"] is not None:
            parts.append(f"{r['avg']:.2f}")
        if r["passes"] is not None:
            parts.append(f"{r['passes']} pass{'es' if r['passes'] != 1 else ''}")
        if r["grades"]:
            parts.append("last " + " ".join(f"`{g}`" for g in r["grades"][-5:]))
        lines.append(" · ".join(parts))
    return lines
