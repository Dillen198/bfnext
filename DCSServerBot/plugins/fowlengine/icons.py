"""Custom Discord icons for everything the Fowl Engine bot posts.

The briefing, alert, status, ops and range embeds -- and the plain replies to
admin commands -- are rendered with the Vector Strike icon set
(`assets/icons/`, drawn by `assets/icons/render_icons.py`) rather than stock
unicode emoji, so the bot's output looks like the dashboard and the in-game
overlay instead of like a group chat.

They are uploaded as **application** emoji, not guild emoji: they work in every
guild the app is installed to, they don't consume the guild's own 50-emoji
budget, and they survive the bot being removed and re-added to a server. On an
older discord.py without the application-emoji API this falls back to uploading
into the guild.

Nothing here is required for the bot to work. Every icon has a unicode
stand-in, and a message rendered before the icons are installed reads exactly
as it did before this module existed -- so a fresh deployment is never broken,
just plainer, until an admin runs `/feops icons_install`.

How the rest of the plugin reaches it:

* `icon("key")` -- module-level, for code with no handle on the cog (autoupdate,
  procman, opsapi, upload, rangefeed, the terminal view). It resolves through
  whichever IconSet the cog created last, and to the unicode stand-in before
  there is one.
* `icon_emoji("key")` -- the same icon as a `discord.PartialEmoji`, for the
  `emoji=` of a button or select option (their text can't hold emoji markup).
* `render(text)` -- expands `{icon:key}` placeholders, for config-supplied
  templates (`messages:`, `welcome_message`). Plain emoji in them still work.
* `plain(text)` -- the reverse: custom-emoji markup back to unicode, for text
  that also lands somewhere that isn't Discord (the dashboard OPS page,
  persisted update history).

Adding an icon: draw it in render_icons.py and add it to its ICONS table, run
`python render_icons.py`, add the key to FALLBACK below with the unicode it
replaces, then `/feops icons_install` uploads just the new one.
"""
import asyncio
import os
import re

try:
    import discord
except ImportError:  # the offline tests import this without discord.py
    discord = None

ICON_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "assets", "icons")

# Icon key -> unicode stand-in, used until the custom emoji are installed (and
# permanently, if the operator never installs them). A key either has its own
# PNG (`assets/icons/vs_<key>.png`) or is listed in ALIASES and borrows one.
FALLBACK = {
    # briefing: urgency, task kinds, section headers
    "critical": "🔴", "high": "🟠", "routine": "🟢",
    "defend": "🛡️", "capture": "🚩", "strike": "💥", "sead": "📡", "cas": "🎯",
    "intercept": "✈️", "logistics": "📦", "recon": "🔍", "csar": "🚁",
    "posture": "📊", "weather": "🌦️", "air": "📡", "tasking": "🎯",
    "hotspot": "🔥", "threat": "☢️", "supply": "🚚", "comms": "📻", "recent": "📰",
    # coalitions and status markers
    "blue": "🔵", "red": "🔴", "unowned": "⬜",
    "live": "🟢", "offline": "⚫", "down": "🔴",
    "good": "✅", "bad": "❌", "neutral": "▫️", "link": "➡️",
    # notices and admin replies
    "warning": "⚠️", "blocked": "⛔", "info": "ℹ️", "alert": "🚨",
    "pending": "⏳", "paused": "⏸️", "restart": "🔁", "rollback": "⏪", "forward": "⏩",
    "update": "📦", "build": "🧩", "probation": "🧪", "new": "🆕", "priority": "⭐",
    "campaign": "🗂️", "settings": "⚙️",
    # hardware (perf embed, server info)
    "cpu": "⚙️", "memory": "🧠", "disk": "💾", "temp": "🌡️", "perf": "📊",
    "server": "🎮", "connect": "🔌",
    # objectives, results, people
    "captured": "🏆", "neutralised": "🏳️",
    "gold": "🥇", "silver": "🥈", "bronze": "🥉", "players": "👥",
    # range feed
    "day": "☀️", "night": "🌙", "wind": "💨", "fuel": "⛽", "carrier": "⚓",
    # aliases (see ALIASES): a borrowed glyph, the unicode they always showed
    "target": "🎯", "inspect": "🔍", "caution": "🟡", "stale": "🟠", "hot": "🔴",
    "cold": "⚪", "side_blue": "🟦", "side_red": "🟥", "rotation": "🔄", "gpu": "🎮",
    "briefing": "📋", "ban": "🔨", "discard": "🗑️", "resume": "▶️", "uptime": "⏱️",
    "url": "🔗", "upload": "📥", "achievement": "🎖️", "all_clear": "🎉",
    "welcome": "🪖",
}

# Key -> the key whose glyph it borrows. An alias is a meaning with no picture
# of its own worth drawing: it keeps its own unicode stand-in (so nothing
# changes before the install) but renders as an existing custom emoji after
# it, which keeps the uploaded set small and coherent.
ALIASES = {
    "target": "cas", "inspect": "recon", "caution": "high", "stale": "warning",
    "hot": "down", "cold": "offline", "side_blue": "blue", "side_red": "red",
    "rotation": "restart", "gpu": "cpu", "briefing": "tasking", "ban": "blocked",
    "discard": "bad", "resume": "live", "uptime": "recent", "url": "link",
    "upload": "update", "achievement": "gold", "all_clear": "good", "welcome": "players",
}

# Discord's cap on application emoji. Checked before an install so a full app
# fails with one clear line per icon instead of a wall of HTTP errors.
APP_EMOJI_LIMIT = 2000
# Pause between uploads. discord.py already honours the rate-limit headers;
# this just keeps a 60-icon install from arriving as one burst.
UPLOAD_DELAY = 0.5

_PLACEHOLDER = re.compile(r"\{icon:([A-Za-z0-9_]+)\}")
_MARKUP = re.compile(r"<a?:vs_([A-Za-z0-9_]+):\d+>")


# Emoji name in Discord for an icon key. Prefixed so they can't collide with a
# guild's own emoji, and stable -- renaming one orphans what is already
# uploaded and silently drops that icon back to its unicode stand-in.
def emoji_name(key: str) -> str:
    return f"vs_{ALIASES.get(key, key)}"


def glyph_keys() -> list:
    """The keys that own a PNG and an uploaded emoji (FALLBACK minus aliases)."""
    return [k for k in FALLBACK if k not in ALIASES]


# The IconSet the cog created last. Module-level so code with no handle on the
# cog renders the same icons the cog's own embeds do.
_active = None


def icon(key: str) -> str:
    """Markup for one icon via the active IconSet, else its unicode stand-in."""
    if _active is not None:
        return _active(key)
    return FALLBACK.get(key, "")


def icon_emoji(key: str):
    """`icon(key)` as a PartialEmoji, for a button's / select option's emoji."""
    return _as_partial(icon(key))


def render(text):
    """Expand `{icon:key}` placeholders. Everything else passes through, so a
    template can still go on to `.format()` -- emoji markup holds no braces."""
    if not isinstance(text, str) or "{icon:" not in text:
        return text
    return _PLACEHOLDER.sub(lambda m: icon(m.group(1)), text)


def plain(text):
    """Custom-emoji markup -> unicode stand-in, for text leaving Discord."""
    if not isinstance(text, str) or "<" not in text:
        return text
    return _MARKUP.sub(lambda m: FALLBACK.get(m.group(1), ""), text)


def _as_partial(markup: str):
    if not markup:
        return None
    if discord is None:
        return markup
    return discord.PartialEmoji.from_str(markup)


class IconSet:
    """Resolves icon keys to whatever this bot can actually render.

    Call `await refresh()` once the bot is ready (and again after an install);
    after that `icons("cas")` is a plain dict lookup, cheap enough to call from
    inside an embed builder. Creating one makes it the module's active set,
    which is what `icon()` and friends resolve through.
    """

    def __init__(self, bot, log):
        global _active
        self.bot = bot
        self.log = log
        self._resolved: dict = {}
        _active = self

    def __call__(self, key: str) -> str:
        """The markup for one icon: a custom emoji if installed, else unicode.

        Unknown keys return an empty string rather than raising -- a missing
        icon must never be able to take down a status embed.
        """
        return self._resolved.get(ALIASES.get(key, key)) or FALLBACK.get(key, "")

    def emoji(self, key: str):
        """The icon as a PartialEmoji (None for an unknown key)."""
        return _as_partial(self(key))

    @property
    def installed(self) -> int:
        """How many of the set's glyphs resolved to real custom emoji."""
        return sum(1 for k in glyph_keys() if k in self._resolved)

    async def refresh(self) -> None:
        """Re-read what is installed, application emoji first, then guilds."""
        global _active
        found = {}
        for name, emoji in (await self._list_available()).items():
            if not name.startswith("vs_"):
                continue
            found[name[3:]] = str(emoji)
        self._resolved = found
        _active = self
        if found:
            self.log.debug(f"FowlEngine: {self.installed}/{len(glyph_keys())} custom icons resolved")

    async def _list_available(self) -> dict:
        """name -> Emoji, from the application first and the guilds after.

        Application emoji win: if an operator once installed the set into a
        guild and later installed it properly, we want the portable copy.
        """
        out = {}
        for guild in getattr(self.bot, "guilds", []):
            for e in guild.emojis:
                out.setdefault(e.name, e)
        fetch = getattr(self.bot, "fetch_application_emojis", None)
        if fetch:
            try:
                for e in await fetch():
                    out[e.name] = e
            except Exception as ex:
                self.log.debug(f"FowlEngine: application emoji unavailable: {ex}")
        return out

    async def _app_emoji_count(self):
        fetch = getattr(self.bot, "fetch_application_emojis", None)
        if not fetch:
            return None
        try:
            return len(await fetch())
        except Exception:
            return None

    async def install(self, guild=None, delay: float = UPLOAD_DELAY) -> tuple:
        """Upload the icons that are missing. Returns (added, skipped, errors).

        Idempotent: an icon already present under its `vs_` name (in the
        application or a guild) is skipped, so re-running after adding one
        icon uploads exactly that one. Uploads go one at a time.

        Outward-facing and rate-limited, so this is only ever driven by an
        explicit admin command -- never on startup.
        """
        available = await self._list_available()
        create_app = getattr(self.bot, "create_application_emoji", None)
        http_error = discord.HTTPException if discord is not None else ()
        added, skipped, errors, missing = [], [], [], []
        for key in glyph_keys():
            name = emoji_name(key)
            (skipped if name in available else missing).append(name)
        if create_app and missing:
            count = await self._app_emoji_count()
            room = max(APP_EMOJI_LIMIT - count, 0) if count is not None else len(missing)
            for name in missing[room:]:
                errors.append(f"{name}: the application is at its {APP_EMOJI_LIMIT}-emoji limit")
            missing = missing[:room]
        for name in missing:
            path = os.path.join(ICON_DIR, f"{name}.png")
            if not os.path.exists(path):
                errors.append(f"{name}: no PNG in assets/icons (run render_icons.py)")
                continue
            if added and delay:
                await asyncio.sleep(delay)
            try:
                with open(path, "rb") as f:
                    data = f.read()
                if create_app:
                    await create_app(name=name, image=data)
                elif guild:
                    await guild.create_custom_emoji(
                        name=name, image=data,
                        reason="Fowl Engine icon set")
                else:
                    errors.append(f"{name}: no application-emoji API and no guild given")
                    continue
                added.append(name)
            except http_error as ex:
                # The usual one is the guild's 50-emoji cap; say so plainly
                # rather than making the operator read a raw Discord error.
                errors.append(f"{name}: {ex.text or ex}")
            except Exception as ex:
                errors.append(f"{name}: {ex}")
        await self.refresh()
        return added, skipped, errors

    async def uninstall(self) -> tuple:
        """Delete every installed `vs_*` emoji. Returns (removed, errors)."""
        removed, errors = [], []
        for name, emoji in (await self._list_available()).items():
            if not name.startswith("vs_"):
                continue
            try:
                await emoji.delete()
                removed.append(name)
            except Exception as ex:
                errors.append(f"{name}: {ex}")
        await self.refresh()
        return removed, errors
