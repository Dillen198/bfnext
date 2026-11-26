# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See DCSServerBot/plugins/fowlengine/LICENSE and the repository NOTICE file.
"""
Round over -> Discord, pure logic only (no discord / aiohttp imports, like
newsfeed.py). commands.py does the HTTP and the sends.

A round used to end without a word on Discord: bfdb recorded the winner on
the round and nothing read it. When a server's round changes (or the bot
starts), commands.py reads the instance's `last_round` and, if it is a won
round not yet announced, posts one rundown of it:

  * Who won, when, and after how long; the front line it ended on.
  * The kill exchange and captures by side, from the round report.
  * The totals, the MVP, the top five, the best pilot in each line of work
    and the most fought-over objectives, from the round summary.

Announced once per round, by id, persisted in the plugin state file -- a bot
restart never reposts. With no state yet (first enable) only a round that
ended in the last FRESH_SECS is announced: no backfill of old history.

bfdb routes consumed (public, instance-scoped):
  GET /api/instances              {"instances": [{id, dcs_server_name, last_round, ...}]}
  GET /api/rounds/<id>/summary    totals, top pilots, leaders
  GET /api/rounds/<id>/report     buckets (kills, captures, held), contested
"""
from __future__ import annotations

from datetime import datetime
from urllib.parse import quote

__all__ = ["FRESH_SECS", "find_instance", "should_announce", "build_embed", "rundown_url"]

# A won round older than this is history, not news, the first time the bot
# looks: it is recorded as seen without a post.
FRESH_SECS = 24 * 3600

_TITLE_MAX = 256
_DESC_MAX = 4096
_FIELD_MAX = 1024
_FOOTER_MAX = 2048
_BLUE = 0x3B82F6
_RED = 0xEF4444


def _clip(s: str, n: int) -> str:
    return s if len(s) <= n else s[: n - 1] + "…"


def _ts(iso: str | None) -> float | None:
    if not iso:
        return None
    try:
        return datetime.fromisoformat(str(iso).replace("Z", "+00:00")).timestamp()
    except ValueError:
        return None


def find_instance(instances: dict | None, instance_id: str | None, server_name: str) -> dict | None:
    """This server's row of /api/instances: by instance id when the bot knows
    it, else by the DCS server name bfdb was told."""
    rows = (instances or {}).get("instances") or []
    if instance_id:
        for r in rows:
            if r.get("id") == instance_id:
                return r
    for r in rows:
        if r.get("dcs_server_name") == server_name:
            return r
    return rows[0] if len(rows) == 1 else None


def should_announce(last_round: dict | None, announced: int | None, now: float) -> bool:
    """Whether `last_round` is a won round to post now. `announced` is the id
    of the last round posted for this server (None: the bot never has)."""
    if not last_round or last_round.get("winner") not in ("Blue", "Red"):
        return False
    if announced is not None:
        return last_round.get("id") != announced
    end = _ts(last_round.get("end"))
    return end is not None and now - end <= FRESH_SECS


def rundown_url(dashboard_url: str | None, instance_id: str | None, round_id) -> str | None:
    """The round's report on the dashboard's Rounds page, or None."""
    if not dashboard_url:
        return None
    url = f"{dashboard_url.rstrip('/')}/rounds?round={quote(str(round_id), safe='')}"
    if instance_id:
        url += f"&instance={quote(str(instance_id), safe='')}"
    return url


def _lasted(start: str | None, end: str | None) -> str | None:
    a, b = _ts(start), _ts(end)
    if a is None or b is None:
        return None
    hours = max(0.0, (b - a) / 3600)
    return f"{hours:.1f} hours" if hours < 48 else f"{hours / 24:.1f} days"


def _side_name(lr: dict, side: str) -> str:
    """The war diary's name for a side, with the colour when the name is not
    already the colour ("Coalition (Blue)")."""
    name = lr.get("winner_name") if side == lr.get("winner") else lr.get("loser_name")
    if not name or name == side:
        return side
    return f"{name} ({side})"


def _sum(buckets: list, key: str) -> int:
    return sum(int(b.get(key) or 0) for b in buckets if isinstance(b, dict))


def build_embed(lr: dict, summary: dict | None, report: dict | None,
                dashboard_url: str | None = None, instance_id: str | None = None,
                server_label: str | None = None) -> dict:
    """An embed dict (clipped to Discord's limits) for one finished round.
    `summary` and `report` may be None: the post still says who won."""
    winner = lr.get("winner")
    loser = "Red" if winner == "Blue" else "Blue"
    n = lr.get("number") or lr.get("id")
    won = _side_name(lr, winner)
    lost = _side_name(lr, loser)
    link = rundown_url(dashboard_url, instance_id, lr.get("id"))

    lines = [f"**{won}** has won Round {n}" + (f" on {server_label}" if server_label else "") + "."]
    lasted = _lasted(lr.get("start"), lr.get("end"))
    end = _ts(lr.get("end"))
    when = f"<t:{int(end)}:f>" if end is not None else None
    if lasted and when:
        lines.append(f"The round lasted {lasted} and ended {when}.")
    if lr.get("scenario"):
        lines.append(f"Campaign: {lr['scenario']}")
    if link:
        lines.append(f"\n[Full rundown on the dashboard]({link})")

    fields: list[dict] = []
    buckets = (report or {}).get("buckets") or []
    if buckets:
        last = buckets[-1]
        total = (report or {}).get("objectives") or (
            int(last.get("blue_held") or 0) + int(last.get("red_held") or 0) + int(last.get("neutral_held") or 0))
        held = (f"{winner_side_label(winner, 'Blue', lr)} **{last.get('blue_held', 0)}** · "
                f"{winner_side_label(winner, 'Red', lr)} **{last.get('red_held', 0)}**")
        if last.get("neutral_held"):
            held += f" · Neutral **{last['neutral_held']}**"
        fields.append({"name": "Final front line", "value": f"{held}\nof {total} objectives", "inline": False})
        b_air, b_gnd = _sum(buckets, "blue_air"), _sum(buckets, "blue_ground")
        r_air, r_gnd = _sum(buckets, "red_air"), _sum(buckets, "red_ground")
        fields.append({"name": "Kills by Blue", "value": f"{b_air} air · {b_gnd} ground", "inline": True})
        fields.append({"name": "Kills by Red", "value": f"{r_air} air · {r_gnd} ground", "inline": True})
        fields.append({"name": "Captures",
                       "value": f"Blue {_sum(buckets, 'blue_caps')} · Red {_sum(buckets, 'red_caps')}",
                       "inline": True})

    if summary:
        hours = float(summary.get("hours") or 0)
        fields.append({
            "name": "The round in numbers",
            "value": (f"{summary.get('pilots', 0)} pilots · {hours:,.0f} flight hours\n"
                      f"{summary.get('air_kills', 0)} air kills · {summary.get('ground_kills', 0)} ground kills\n"
                      f"{summary.get('captures', 0)} captures · {summary.get('supply_runs', 0)} supply runs"),
            "inline": False,
        })
        top = [p for p in (summary.get("top") or []) if isinstance(p, dict)]
        if top:
            mvp = top[0]
            fields.append({
                "name": "MVP",
                "value": (f"**{mvp.get('name') or 'Unknown'}**: {mvp.get('air_kills', 0)} A/A, "
                          f"{mvp.get('ground_kills', 0)} A/G, {mvp.get('captures', 0)} captures, "
                          f"{mvp.get('logistics', 0)} logistics, {float(mvp.get('hours') or 0):.1f} h"),
                "inline": False,
            })
            rows = [f"{i}. {p.get('name') or 'Unknown'}: {p.get('air_kills', 0)} A/A · "
                    f"{p.get('ground_kills', 0)} A/G · {p.get('captures', 0)} cap"
                    for i, p in enumerate(top[:5], 1)]
            fields.append({"name": "Top pilots", "value": "\n".join(rows), "inline": False})
        leaders = summary.get("leaders") or {}
        best = []
        for key, title, unit in (("air", "Top ace", "air kills"), ("ground", "Top striker", "ground kills"),
                                 ("captures", "Top capturer", "captures"),
                                 ("logistics", "Top logistician", "logistics jobs")):
            l = leaders.get(key)
            if isinstance(l, dict) and l.get("name"):
                best.append(f"{title}: **{l['name']}** ({l.get('value', 0)} {unit})")
        if best:
            fields.append({"name": "Best in each line of work", "value": "\n".join(best), "inline": False})

    contested = [c for c in ((report or {}).get("contested") or []) if isinstance(c, dict)][:3]
    if contested:
        fields.append({
            "name": "Most fought over",
            "value": "\n".join(f"{c.get('name')}: changed hands {c.get('captures', 0)}x" for c in contested),
            "inline": False,
        })

    for f in fields:
        f["value"] = _clip(f["value"], _FIELD_MAX)
    out = {
        "title": _clip(f"Round {n} is over: {won} wins", _TITLE_MAX),
        "description": _clip("\n".join(lines), _DESC_MAX),
        "color": _BLUE if winner == "Blue" else _RED,
        "fields": fields[:25],
        "footer": _clip(f"{lost} lost the round · a new round is under way", _FOOTER_MAX),
    }
    if link:
        out["url"] = link
    return out


def winner_side_label(winner: str | None, side: str, lr: dict) -> str:
    """A side's name for the front-line line: the war diary's name where it
    has one, else the colour."""
    name = lr.get("winner_name") if side == winner else lr.get("loser_name")
    return name if name else side
