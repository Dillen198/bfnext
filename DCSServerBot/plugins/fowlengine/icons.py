"""Custom Discord icons for the Fowl Engine embeds.

The briefing, alert and status embeds are rendered with the Vector Strike icon
set (`assets/icons/`, drawn by `assets/icons/render_icons.py`) rather than
stock unicode emoji, so the bot's output looks like the dashboard and the
in-game overlay instead of like a group chat.

They are uploaded as **application** emoji, not guild emoji: they work in every
guild the app is installed to, they don't consume the guild's own 50-emoji
budget, and they survive the bot being removed and re-added to a server. On an
older discord.py without the application-emoji API this falls back to uploading
into the guild.

Nothing here is required for the bot to work. Every icon has a unicode
stand-in, and an embed rendered before the icons are installed reads exactly
as it did before this module existed -- so a fresh deployment is never broken,
just plainer, until an admin runs `/feops icons_install`.
"""
import os

import discord

ICON_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "assets", "icons")

# Icon key -> unicode stand-in, used until the custom emoji are installed (and
# permanently, if the operator never installs them). The keys match the PNG
# basenames in assets/icons minus the `vs_` prefix.
FALLBACK = {
    "critical": "🔴", "high": "🟠", "routine": "🟢",
    "defend": "🛡️", "capture": "🚩", "strike": "💥", "sead": "📡", "cas": "🎯",
    "intercept": "✈️", "logistics": "📦", "recon": "🔍", "csar": "🚁",
    "posture": "📊", "weather": "🌦️", "air": "📡", "tasking": "🎯",
    "hotspot": "🔥", "threat": "☢️", "supply": "🚚", "comms": "📻", "recent": "📰",
    "blue": "🔵", "red": "🔴",
    "live": "🟢", "offline": "⚫", "good": "✅", "bad": "❌", "neutral": "▫️",
    "link": "➡️",
}

# Emoji name in Discord for an icon key. Prefixed so they can't collide with a
# guild's own emoji, and stable -- renaming one orphans what is already
# uploaded and silently drops that icon back to its unicode stand-in.
def emoji_name(key: str) -> str:
    return f"vs_{key}"


class IconSet:
    """Resolves icon keys to whatever this bot can actually render.

    Call `await refresh()` once the bot is ready (and again after an install);
    after that `icons("cas")` is a plain dict lookup, cheap enough to call from
    inside an embed builder.
    """

    def __init__(self, bot, log):
        self.bot = bot
        self.log = log
        self._resolved: dict = {}

    def __call__(self, key: str) -> str:
        """The markup for one icon: a custom emoji if installed, else unicode.

        Unknown keys return an empty string rather than raising -- a missing
        icon must never be able to take down a status embed.
        """
        return self._resolved.get(key) or FALLBACK.get(key, "")

    @property
    def installed(self) -> int:
        """How many of the set resolved to real custom emoji."""
        return len(self._resolved)

    async def refresh(self) -> None:
        """Re-read what is installed, application emoji first, then guilds."""
        found = {}
        for name, emoji in (await self._list_available()).items():
            if not name.startswith("vs_"):
                continue
            found[name[3:]] = str(emoji)
        self._resolved = found
        if found:
            self.log.debug(f"FowlEngine: {len(found)}/{len(FALLBACK)} custom icons resolved")

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

    async def install(self, guild: discord.Guild | None = None) -> tuple:
        """Upload any missing icons. Returns (added, skipped, errors).

        Outward-facing and rate-limited, so this is only ever driven by an
        explicit admin command -- never on startup.
        """
        available = await self._list_available()
        create_app = getattr(self.bot, "create_application_emoji", None)
        added, skipped, errors = [], [], []
        for key in FALLBACK:
            name = emoji_name(key)
            if name in available:
                skipped.append(name)
                continue
            path = os.path.join(ICON_DIR, f"{name}.png")
            if not os.path.exists(path):
                errors.append(f"{name}: no PNG in assets/icons (run render_icons.py)")
                continue
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
            except discord.HTTPException as ex:
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
