# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See DCSServerBot/plugins/fowlengine/LICENSE and the repository NOTICE file.
"""
Formatting for the public "Comms, Rules & Links" embed -- pure logic only.

The input is bfdb's public `GET /api/wiki/facts?server=<name>` (`facts`: the
allow-listed top-level keys of that instance's engine cfg, the same numbers
the wiki's {{cfg:...}} placeholders show). Only STATIC configuration is used
here: the planned comms channels and the campaign rules. The live overlay
(which AWACS/tanker is really up, TACANs, laser codes) is coalition intel and
stays in the per-side briefing channels.

Nothing here needs discord or aiohttp; commands.py does the I/O.
"""
from __future__ import annotations

from typing import Optional

__all__ = ["comms_plan_block", "flight_line", "rules_lines", "FIELD_LIMIT"]

FIELD_LIMIT = 1024  # Discord embed field value
_LIFE_ORDER = ("Standard", "Intercept", "Attack", "Logistics", "Recon")
# airframe_cost uses a huge sentinel for "not flyable here"; it isn't a price.
_COST_SENTINEL = 99999


def _label(ch: dict) -> str:
    lab = str(ch.get("label") or "").replace(" -- ", " · ").strip()
    return lab if len(lab) <= 26 else lab[:25] + "…"


def comms_plan_block(plan: Optional[dict], side: str, more_hint: str = "") -> str:
    """One side's planned channels as a monospace table, preset order:

        01 251.000     AWACS / GCI · MAGIC
        20  30.000 FM  Ground / logistics net

    AM is the default and left out; anything else is shown. Cut to fit an
    embed field, with `more_hint` (e.g. "full plan in the wiki") after the
    count of what was cut. Empty when the plan has no channels for `side`."""
    chans = (plan or {}).get(side.lower()) or []
    rows = []
    for ch in chans:
        try:
            mhz = float(ch.get("freq_mhz"))
        except (TypeError, ValueError):
            continue
        preset = ch.get("preset")
        pre = f"{int(preset):02d}" if isinstance(preset, (int, float)) else "--"
        mod = str(ch.get("modulation") or "AM").upper()
        rows.append(f"{pre} {mhz:7.3f} {'' if mod == 'AM' else mod:2} {_label(ch)}")
    if not rows:
        return ""
    flights = flight_line(plan, side)
    budget = FIELD_LIMIT - len("```\n```\n") - len(flights) - 60
    shown, used = [], 0
    for r in rows:
        if used + len(r) + 1 > budget:
            break
        shown.append(r)
        used += len(r) + 1
    out = "```\n" + "\n".join(shown) + "\n```\n"
    cut = len(rows) - len(shown)
    if cut:
        out += f"+{cut} more" + (f" — {more_hint}" if more_hint else "") + "\n"
    if flights:
        out += flights
    return out


def flight_line(plan: Optional[dict], side: str) -> str:
    """"Flights 1-8: 305.000-312.000 (1 MHz apart)" from the plan's flight
    base/step/count, as the engine's CommsPlanCfg::flight_channels does."""
    if not plan:
        return ""
    try:
        base = float(plan.get(f"{side.lower()}_flight_base_mhz"))
        step = float(plan.get("flight_step_mhz", 1.0))
        count = int(plan.get("flight_count", 8))
    except (TypeError, ValueError):
        return ""
    if count <= 0:
        return ""
    last = base + step * (count - 1)
    step_txt = f"{step:g} MHz apart"
    return f"Flights 1–{count}: `{base:.3f}`–`{last:.3f}` ({step_txt})"


def _dur(secs) -> str:
    try:
        s = int(secs)
    except (TypeError, ValueError):
        return ""
    if s <= 0:
        return ""
    if s % 3600 == 0:
        return f"{s // 3600} h"
    return f"{round(s / 60)} min"


def _lives_line(facts: dict) -> str:
    if not facts.get("limited_lives"):
        return "**Lives:** unlimited"
    lives = facts.get("default_lives") or {}
    if not isinstance(lives, dict) or not lives:
        return "**Lives:** limited"
    order = [k for k in _LIFE_ORDER if k in lives] + sorted(k for k in lives if k not in _LIFE_ORDER)
    resets = {_dur(v[1]) for v in lives.values() if isinstance(v, (list, tuple)) and len(v) == 2}
    parts = []
    for k in order:
        v = lives[k]
        if not (isinstance(v, (list, tuple)) and len(v) == 2):
            continue
        n = v[0]
        parts.append(f"{k.lower()} {n}" if len(resets) == 1 else f"{k.lower()} {n}/{_dur(v[1])}")
    line = "**Lives:** " + " · ".join(parts)
    if len(resets) == 1:
        (r,) = resets
        if r:
            line += f" — reset every {r}"
    return line


def _sides_line(facts: dict) -> str:
    sw = facts.get("side_switches")
    if facts.get("lock_sides"):
        if sw is None:
            return "**Sides:** locked to the side you pick first"
        if sw == 0:
            return "**Sides:** locked for the round — pick carefully"
        return (f"**Sides:** locked to the side you pick first · "
                f"{sw} switch{'es' if sw != 1 else ''} per round (`-switch`)")
    if sw is None:
        return "**Sides:** switch freely (`-switch`)"
    return f"**Sides:** up to {sw} switch{'es' if sw != 1 else ''} per round (`-switch`)"


def _points_lines(facts: dict) -> list[str]:
    p = facts.get("points")
    if not isinstance(p, dict):
        return []
    earn = []
    for key, what in (("new_player_join", "start"), ("air_kill", "air kill"),
                      ("ground_kill", "ground kill"), ("capture", "capture"),
                      ("logistics_repair", "repair"), ("logistics_transfer", "supply run")):
        v = p.get(key)
        if isinstance(v, (int, float)) and v:
            earn.append(f"{what} {'' if key == 'new_player_join' else '+'}{int(v):,}")
    lines = []
    if earn:
        lines.append("**Points:** " + " · ".join(earn))
    gain = p.get("periodic_point_gain")
    if isinstance(gain, (list, tuple)) and len(gain) == 2 and gain[0] and gain[1]:
        lines.append(f"**Income:** {int(gain[0]):+,} every {_dur(gain[1])}")
    costs = [v for v in (p.get("airframe_cost") or {}).values()
             if isinstance(v, (int, float)) and 0 < v < _COST_SENTINEL]
    if costs:
        lo, hi = int(min(costs)), int(max(costs))
        rng = f"{lo:,}" if lo == hi else f"{lo:,}–{hi:,}"
        spend = f"**Spend:** aircraft cost {rng} pts to fly"
        if p.get("weapon_cost"):
            spend += ", weapons extra"
        if p.get("strict"):
            spend += " — you can't take off with more than your balance"
        lines.append(spend)
    if p.get("provisional"):
        lines.append("Kill points only bank when you land at a friendly base.")
    return lines


def rules_lines(facts: Optional[dict]) -> list[str]:
    """The campaign rules a new pilot needs, from the instance's cfg facts:
    lives, side lock/switches, what earns and costs points."""
    if not isinstance(facts, dict) or not facts:
        return []
    lines = [_lives_line(facts), _sides_line(facts)]
    lines += _points_lines(facts)
    return [l for l in lines if l]
