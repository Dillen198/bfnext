# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See DCSServerBot/plugins/fowlengine/LICENSE and the repository NOTICE file.
"""
Per-instance branding for the Discord embeds, pure logic only (no discord /
aiohttp imports, like newsfeed.py). commands.py feeds it the config and the
instance, and builds the embeds.

One bfdb can run several campaigns, and they need not share a look: the
"Red Storm" instance (1985 Cold War) posts under its own name, emblem,
colour and newspaper, while Vector Strike posts stay exactly as they were.

Each field is taken from the first of these that sets it:

  1. `branding.<instance id or DCS server name>` in fowlengine.yaml
  2. bfdb's own `brand_name` for the instance (`/api/instances`)
  3. the theme's defaults, when bfdb reports `theme` (THEMES below)
  4. today's global values: `brand_name` from the config, the Vector Strike
     logo, colour and "War Diary"

An instance with no override and no theme therefore resolves to exactly the
values the embeds have always used.
"""
from __future__ import annotations

import re

__all__ = ["DEFAULT_LOGO", "REFRESH_SECS", "DEFAULT_COLOUR", "DEFAULT_DIARY", "THEMES", "FIELDS", "resolve",
           "override_for", "logo_url", "colour_int"]

DEFAULT_BRAND = "Fowl Engine"
DEFAULT_LOGO = "/vs-vectorstrike_hd-white.png"
DEFAULT_COLOUR = "#c8102e"
DEFAULT_DIARY = "War Diary"
# How often the cog re-reads bfdb's /api/instances for `theme` / `brand_name`.
REFRESH_SECS = 600

# Defaults per bfdb `theme`. Discord does not render SVG in an embed icon or
# thumbnail, so the logos here are PNGs the dashboard serves.
THEMES = {
    "redstorm": {
        "brand_name": "Southern Flank 1985",
        "logo": "/redstorm/emblem.png",
        "colour": "#b3261e",
        "diary_title": "The Allied Dispatch",
    },
}

# What an override block may set. `color` is accepted for `colour`.
FIELDS = ("brand_name", "logo", "colour", "diary_title", "diary_link_text")

_HEX = re.compile(r"^#?([0-9a-fA-F]{6})$")
_SVG = re.compile(r"\.svg(?=$|[?#])", re.IGNORECASE)


def _s(v) -> str | None:
    """A non-empty stripped string, or None."""
    if isinstance(v, str) and v.strip():
        return v.strip()
    return None


def _hex(v) -> str | None:
    """'#rrggbb' from '#rrggbb' / 'rrggbb' / an int, or None."""
    if isinstance(v, bool):
        return None
    if isinstance(v, int) and 0 <= v <= 0xFFFFFF:
        return f"#{v:06x}"
    m = _HEX.match(v.strip()) if isinstance(v, str) else None
    return f"#{m.group(1).lower()}" if m else None


def override_for(config: dict | None, instance_id: str | None, server_name: str | None) -> dict:
    """The `branding:` block for this instance: keyed by bfdb instance id, or
    by the DCS server name. {} when there is none."""
    blocks = (config or {}).get("branding")
    if not isinstance(blocks, dict):
        return {}
    for key in (instance_id, server_name):
        if key and isinstance(blocks.get(key), dict):
            o = dict(blocks[key])
            if "colour" not in o and "color" in o:
                o["colour"] = o["color"]
            return o
    return {}


def logo_url(logo: str | None, dashboard_url: str | None) -> str | None:
    """An absolute logo URL: a full http(s) URL as given, a path on the
    dashboard origin otherwise (None without a dashboard_url). An SVG becomes
    the PNG beside it -- Discord shows no SVG icons."""
    logo = _s(logo)
    if not logo:
        return None
    logo = _SVG.sub(".png", logo)
    if re.match(r"^https?://", logo, re.IGNORECASE):
        return logo
    dash = (dashboard_url or "").strip().rstrip("/")
    if not dash:
        return None
    return f"{dash}/{logo.lstrip('/')}"


def colour_int(colour: str) -> int:
    return int(colour.lstrip("#"), 16)


def resolve(config: dict | None, instance: dict | None = None, server_name: str | None = None) -> dict:
    """The branding for one instance.

    `config` is the plugin config (the server's own section merged over
    DEFAULT, or DEFAULT alone); `instance` is the instance's entry --
    instances.json / `bfdb.instances`, with bfdb's `/api/instances` row over
    it -- or None for an embed tied to no server.

    Returns {brand_name, logo_url, colour, colour_int, diary_title,
    diary_link_text, theme}."""
    config = config or {}
    instance = instance if isinstance(instance, dict) else {}
    iid = _s(instance.get("id"))
    server_name = server_name or _s(instance.get("dcs_server_name"))
    theme = _s(instance.get("theme"))
    theme = theme.lower() if theme else None
    over = override_for(config, iid, server_name)
    themed = THEMES.get(theme or "", {})

    def pick(field, *extra):
        for v in (over.get(field), *extra, themed.get(field)):
            if _s(v):
                return _s(v)
        return None

    brand = pick("brand_name", instance.get("brand_name")) or _s(config.get("brand_name")) or DEFAULT_BRAND
    colour = next((c for c in (_hex(over.get("colour")), _hex(themed.get("colour"))) if c), DEFAULT_COLOUR)
    diary = pick("diary_title") or DEFAULT_DIARY
    link_text = pick("diary_link_text") or f"Read more in the {diary}"
    # "the The Allied Dispatch" -- a title that brings its own article keeps it
    if diary.lower().startswith("the ") and not _s(over.get("diary_link_text")):
        link_text = f"Read more in {diary}"
    return {
        "brand_name": brand,
        "logo_url": logo_url(pick("logo") or DEFAULT_LOGO, config.get("dashboard_url")),
        "colour": colour,
        "colour_int": colour_int(colour),
        "diary_title": diary,
        "diary_link_text": link_text,
        "theme": theme,
    }
