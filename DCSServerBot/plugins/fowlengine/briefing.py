"""Rendering for the per-coalition briefing channels.

The content is *not* composed here: bfdb's `GET /api/situation` hands back a
whole `bfprotocols::situation::SituationReport` -- the same object that backs
the in-game F10 Info pages, the dashboard BRIEFING page and the kneeboard PDF
-- and this module only turns one into a Discord embed. That is deliberate: a
pilot reads one story wherever they look, and nothing that decides *what* a
side knows lives in the bot.

Fog of war is enforced upstream. A report is built for one side and only ever
contains what that side has earned: threat rings come from its own recon/ELINT,
the air picture from its own radar net. The bot never merges the two sides'
reports, and each rendered embed goes to exactly one coalition's channel.

Every glyph goes through the `icons` callable (an `icons.IconSet`), which
resolves to the Vector Strike custom emoji when they are installed and to a
unicode stand-in when they are not -- so this renders correctly on a fresh
deployment that has never run `/feops icons_install`.
"""
import discord

# `urgency` and `kind` arrive as bfprotocols' own lowercase spellings, which
# are also the icon keys -- so a new TaskKind added engine-side needs a PNG in
# assets/icons and a FALLBACK entry, and nothing here.
URGENCY_KEYS = ("critical", "high", "routine")
TASK_KEYS = ("defend", "capture", "strike", "sead", "cas",
             "intercept", "logistics", "recon", "csar")

# Discord's own caps. A field value over 1024 characters, or an embed over
# 6000, is rejected outright -- so every list below is fitted, not hoped about.
FIELD_LIMIT = 1024
EMBED_LIMIT = 5800  # leave headroom for the author/footer/title chrome


def side_color(side: str) -> discord.Color:
    return discord.Color.red() if side == "Red" else discord.Color.blue()


def _fit(lines: list, limit: int = FIELD_LIMIT) -> str:
    """Join as many lines as fit, then say how many were dropped.

    Silently truncating a tasking list is worse than useless -- a pilot has no
    way to tell a short list from a clipped one -- so the overflow is always
    named.
    """
    out, used = [], 0
    for i, line in enumerate(lines):
        remaining = len(lines) - i
        # Reserve room for the "+N more" line we'd have to add if we stop here.
        tail = len(f"\n_+{remaining} more_")
        if used + len(line) + 1 + tail > limit:
            out.append(f"_+{remaining} more_")
            break
        out.append(line)
        used += len(line) + 1
    return "\n".join(out) if out else "_none_"


def _add(embed: discord.Embed, name: str, lines: list, inline: bool = False) -> None:
    """Add a field only when there is something in it -- an empty section is
    noise on a message that is edited every few minutes."""
    if not lines:
        return
    if len(embed) >= EMBED_LIMIT:
        return
    embed.add_field(name=name, value=_fit(lines), inline=inline)


def _pct(v) -> str:
    try:
        return f"{float(v):.0f}%"
    except (TypeError, ValueError):
        return "?"


def _posture_lines(p: dict, side: str, icons) -> list:
    enemy = "Red" if side == "Blue" else "Blue"
    lines = [
        f"**Territory:** {_pct(p.get('territory_pct'))} "
        f"({p.get('friendly_objectives', 0)} vs {p.get('enemy_objectives', 0)} obj, "
        f"{p.get('neutral_objectives', 0)} neutral)",
        f"**Airfields/FARPs:** {p.get('friendly_primary', 0)} ours · "
        f"{p.get('enemy_primary', 0)} {enemy.lower()}",
        f"**Treasury:** {p.get('treasury', 0)} pts",
        f"**Online:** {p.get('players_friendly', 0)} friendly · "
        f"{p.get('players_enemy', 0)} {enemy.lower()}",
    ]
    gained, lost = p.get("gained_recent", 0), p.get("lost_recent", 0)
    if gained or lost:
        lines.append(f"**Last hour:** +{gained} / -{lost} objectives")
    if p.get("last_stand"):
        lines.append(f"{icons('critical')} **LAST STAND:** {p['last_stand']}")
    if p.get("victory_condition"):
        lines.append(f"**Victory:** {p['victory_condition']}")
    return lines


def _task_lines(tasking: list, icons, limit: int = 8) -> list:
    lines = []
    for t in tasking[:limit]:
        u = icons(t.get("urgency", ""))
        k = icons(t.get("kind", ""))
        line = f"{u} {k} **{t.get('title', '?')}** — {t.get('detail', '')}".rstrip(" —")
        if t.get("success"):
            line += f"\n   ↳ _{t['success']}_"
        lines.append(line)
    if len(tasking) > limit:
        lines.append(f"_+{len(tasking) - limit} more on the dashboard_")
    return lines


def _hotspot_lines(hotspots: list, icons, limit: int = 5) -> list:
    lines = []
    for h in hotspots[:limit]:
        risk = icons(h.get("risk", ""))
        flags = []
        if h.get("captureable"):
            flags.append("CAPTUREABLE")
        if h.get("in_capture_hold"):
            flags.append("HOLD")
        if h.get("threatened"):
            flags.append("THREATENED")
        tag = f" `{' '.join(flags)}`" if flags else ""
        lines.append(
            f"{risk} **{h.get('objective', '?')}** ({h.get('owner', '?')}){tag}\n"
            f"   hp {h.get('health', '?')}% · logi {h.get('logi', '?')}% · "
            f"sup {h.get('supply', '?')}% — {h.get('status', '')}"
        )
    return lines


def _threat_lines(threats: list, icons, limit: int = 6) -> list:
    """Known enemy air defence, from this side's own intel only.

    Every line carries its source and age on purpose: an hour-old recon fix is
    a very different thing to fly against than a live emitter, and the report
    deliberately refuses to name a type the side has not identified.
    """
    lines = []
    mark = icons("threat")
    for t in threats[:limit]:
        bits = []
        if t.get("radius_m"):
            bits.append(f"{t['radius_m'] / 1852:.0f}nm ring")
        conf = t.get("confidence")
        if conf is not None:
            bits.append(f"{float(conf) * 100:.0f}% conf")
        age = t.get("age_s")
        if age is not None:
            bits.append(f"{int(age) // 60}m old" if age >= 60 else "fresh")
        if t.get("source"):
            bits.append(str(t["source"]))
        near = f" near **{t['near']}**" if t.get("near") else ""
        count = f" ×{t['count']}" if t.get("count", 1) > 1 else ""
        lines.append(f"{mark} {t.get('label', 'air defence')}{count}{near} — {' · '.join(bits)}")
    if len(threats) > limit:
        lines.append(f"_+{len(threats) - limit} more_")
    return lines


def _air_lines(air: dict, icons) -> list:
    if air.get("radar_blind"):
        return [f"{icons('critical')} **RADAR BLIND** — no working EWR. "
                f"Nothing below is trustworthy."]
    lines = [
        f"**Hostile tracks:** {air.get('hostile_tracks', 0)}"
        + (f" (+{air['stale_tracks']} coasting)" if air.get("stale_tracks") else ""),
        f"**Friendly airborne:** {air.get('friendly_airborne', 0)}",
    ]
    if air.get("axis"):
        lines.append(f"**Axis:** {air['axis']}")
    n = air.get("nearest")
    if n:
        lines.append(
            f"**Nearest:** {n.get('class', 'contact')} {n.get('bearing_deg', 0):03.0f}°/"
            f"{n.get('range_nm', 0):.0f}nm off {n.get('near', '?')} at "
            f"{n.get('alt_ft', 0):,}ft, {n.get('speed_kts', 0)}kt"
        )
    return lines


def _logistics_lines(logi: dict, icons, limit: int = 4) -> list:
    lines = [f"**Stage:** {logi.get('stage', '?')} · **Convoys:** {logi.get('convoys_active', 0)}"]
    for h in (logi.get("hubs") or [])[:limit]:
        warn = f" {icons('high')}" if h.get("threatened") else ""
        lines.append(
            f"{icons('supply')} **{h.get('objective', '?')}**{warn} — "
            f"sup {h.get('supply', '?')}% · fuel {h.get('fuel', '?')}% · "
            f"feeding {h.get('feeding', 0)}"
        )
    for g in (logi.get("gaps") or [])[:limit]:
        lines.append(f"{icons('bad')} **{g.get('objective', '?')}** — {g.get('note', '')}")
    return lines


def _comms_lines(comms: list, flight_channels: list, icons, limit: int = 8) -> list:
    lines = []
    for c in comms[:limit]:
        preset = f"`{c['preset']:>2}` " if c.get("preset") is not None else ""
        live = icons("live") if c.get("live") else icons("offline")
        note = f" — {c['note']}" if c.get("note") else ""
        lines.append(
            f"{live} {preset}**{c.get('label', '?')}** "
            f"{c.get('freq_mhz', 0):.3f} {c.get('modulation', '')}{note}"
        )
    if flight_channels:
        flights = " · ".join(f"{n} {f:.3f}" for n, f in flight_channels[:6])
        lines.append(f"{icons('intercept')} {flights}")
    return lines


def _recent_lines(recent: list, icons, limit: int = 6) -> list:
    lines = []
    for e in recent[:limit]:
        good = e.get("good")
        mark = icons("good") if good is True else (
            icons("bad") if good is False else icons("neutral"))
        lines.append(f"{mark} {e.get('text', '')}")
    return lines


def build_briefing_embed(report: dict, *, icons, embed_factory,
                         dashboard_url: str = "", instance_label: str = "") -> discord.Embed:
    """One coalition's whole situational picture as a single embed.

    `embed_factory(title, color=..., url=...)` is the plugin's branded builder
    and `icons` its IconSet, both passed in rather than imported so this module
    stays free of plugin state and is trivially testable with stubs.
    """
    side = report.get("side", "Blue")
    where = f" — {instance_label}" if instance_label else ""
    dash = (dashboard_url or "").rstrip("/")
    mark = icons("red" if side == "Red" else "blue")
    embed = embed_factory(
        f"{side.upper()} BRIEFING{where}",
        color=side_color(side),
        url=f"{dash}/briefing" if dash else None,
    )

    desc = report.get("headline", "")
    if report.get("mission_time"):
        desc = f"{icons('recent')} **Mission time {report['mission_time']}**\n{desc}"
    embed.description = f"{mark} {desc}"[:4000]

    _add(embed, f"{icons('posture')} Posture",
         _posture_lines(report.get("posture") or {}, side, icons))

    w = report.get("weather")
    if w:
        wx = [
            f"**{w.get('summary', '')}**",
            f"Wind {w.get('wind_from_deg', 0):03.0f}°/{w.get('wind_kts', 0):.0f}kt · "
            f"{w.get('temp_c', 0):.0f}°C · QNH {w.get('qnh_inhg', 0):.2f}\" "
            f"({w.get('qnh_hpa', 0):.0f} hPa)",
        ]
        if w.get("cloud_base_m"):
            wx.append(f"Cloud base {w['cloud_base_m'] * 3.28084:,.0f}ft")
        _add(embed, f"{icons('weather')} Weather", wx, inline=True)

    _add(embed, f"{icons('air')} Air picture", _air_lines(report.get("air") or {}, icons),
         inline=True)
    _add(embed, f"{icons('tasking')} Tasking", _task_lines(report.get("tasking") or [], icons))
    _add(embed, f"{icons('hotspot')} Hotspots", _hotspot_lines(report.get("hotspots") or [], icons))
    _add(embed, f"{icons('threat')} Known air defence",
         _threat_lines(report.get("threats") or [], icons))
    _add(embed, f"{icons('logistics')} Logistics",
         _logistics_lines(report.get("logistics") or {}, icons))
    _add(embed, f"{icons('comms')} Comms card",
         _comms_lines(report.get("comms") or [], report.get("flight_channels") or [], icons))
    _add(embed, f"{icons('recent')} Recent", _recent_lines(report.get("recent") or [], icons))

    if dash:
        embed.add_field(
            name="​",
            value=f"{icons('link')} **[Full briefing & map ›]({dash}/briefing)** · "
                  f"in game: **F10 → Info → Situation**",
            inline=False,
        )
    return embed
