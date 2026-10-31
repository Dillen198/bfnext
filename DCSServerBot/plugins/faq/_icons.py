"""The Vector Strike icon set, borrowed from the fowlengine plugin.

The same file sits in every plugin that posts to Discord next to fowlengine
(about, announcements, faq, radio, rules, smartmod, tickets); the tests keep
the copies identical, so change them together.

How it finds the icons: DCSServerBot loads a plugin as `plugins.<name>`, so
fowlengine's `from .icons import ...` puts its icon module in sys.modules as
`plugins.fowlengine.icons`, and the IconSet the FowlEngine cog creates (and
refreshes once the bot is ready, and after `/feops icons_install`) is that
module's active set. Resolving through that exact module object means these
plugins render whatever custom emoji fowlengine has installed.

It is looked up in sys.modules on every call rather than imported:

* plugin load order doesn't matter -- a plugin loaded before fowlengine picks
  the icons up as soon as fowlengine is there;
* nothing here ever imports (and so executes) a fowlengine plugin the operator
  hasn't enabled;
* without fowlengine every key falls back to the unicode stand-in below, the
  same one fowlengine itself shows before `/feops icons_install` -- so these
  plugins never fail because fowlengine is missing, only look plainer.

API, mirroring plugins/fowlengine/icons.py:

* `icon(key)`       -- markup for message text, embed titles, field names/values
* `icon_emoji(key)` -- for the `emoji=` of a button or select option (a custom
                       emoji can't live in a label)
* `render(text)`    -- expands `{icon:key}` in config-supplied text
* `emoji_from(v)`   -- a config `emoji:` value (`{icon:key}` or plain unicode)
                       for an `emoji=` slot
* `unicode(key)`    -- always the unicode stand-in, for text that is not
                       rendered by Discord's emoji markup (bot presence, the
                       .txt ticket transcript)
* `plain(text)`     -- `render()` with unicode stand-ins, for select option
                       labels / descriptions (no custom emoji there either)
* `await wait_ready()` -- bounded wait for fowlengine's icon set to be read
                       from Discord, before posting something once at startup
"""
import re
import sys

SHARED_MODULE = "plugins.fowlengine.icons"

# Unicode stand-ins for every key these plugins use. They match fowlengine's
# FALLBACK table (a test checks), and are only consulted when fowlengine isn't
# loaded or is an older build that doesn't know a key yet.
FALLBACK = {
    # status and replies
    "good": "✅", "bad": "❌", "warning": "⚠️", "alert": "🚨", "pending": "⏳",
    "live": "🟢", "down": "🔴", "connect": "🔌", "discard": "🗑️",
    "verify": "🛡️", "removed": "🛑", "help": "❓",
    # tickets
    "ticket": "🎫", "lock": "🔒", "mail": "📩", "attachment": "📎",
    # rules, faq, about
    "rules": "📜", "globe": "🌐", "book": "📖", "dashboard": "📊",
    "rocket": "🚀", "wings": "🦅", "capture": "🚩", "logistics": "📦",
    "tasking": "🎯", "csar": "🚁", "comms": "📻", "intercept": "✈️", "keyboard": "⌨️",
    # radio
    "play": "▶️", "play_pause": "⏯️", "paused": "⏸️", "skip": "⏭️", "stop": "⏹️",
    "prev": "◀️", "next": "▶️", "queue": "📜", "music": "🎵", "radio": "📻",
    "broadcast": "📡", "filter": "🎛️", "loop": "🔁", "shuffle": "🔀", "volume": "🔊",
    "save": "💾", "load": "📂", "leave": "👋",
}

_PLACEHOLDER = re.compile(r"\{icon:([A-Za-z0-9_]+)\}")


def shared():
    """fowlengine's icon module when that plugin is loaded, else None."""
    return sys.modules.get(SHARED_MODULE)


def icon(key: str) -> str:
    """Markup for one icon: fowlengine's custom emoji if installed, else unicode.

    Never raises: an unknown key is an empty string, and a broken fowlengine
    module is treated as an absent one.
    """
    mod = shared()
    if mod is not None:
        try:
            found = mod.icon(key)
            if found:
                return found
        except Exception:
            pass
    return FALLBACK.get(key, "")


def icon_emoji(key: str):
    """`icon(key)` for a button's / select option's `emoji=` (None if unknown).

    A PartialEmoji through fowlengine; otherwise the unicode string, which
    discord.py accepts in the same place.
    """
    mod = shared()
    if mod is not None:
        try:
            found = mod.icon_emoji(key)
            if found:
                return found
        except Exception:
            pass
    return FALLBACK.get(key) or None


def unicode(key: str) -> str:
    """The plain unicode stand-in, whatever is installed."""
    mod = shared()
    table = getattr(mod, "FALLBACK", None) if mod is not None else None
    return (table or {}).get(key) or FALLBACK.get(key, "")


def render(text):
    """Expand `{icon:key}` placeholders; anything else passes through untouched
    (plain emoji in an operator's config keep working, non-strings too)."""
    if not isinstance(text, str) or "{icon:" not in text:
        return text
    return _PLACEHOLDER.sub(lambda m: icon(m.group(1)), text)


def plain(text):
    """Expand `{icon:key}` to the unicode stand-in, for text Discord shows
    without emoji markup (select option labels and descriptions)."""
    if not isinstance(text, str) or "{icon:" not in text:
        return text
    return _PLACEHOLDER.sub(lambda m: unicode(m.group(1)), text)


async def wait_ready(timeout: float = 30.0, poll: float = 1.0) -> bool:
    """Wait, at most `timeout` seconds, for fowlengine's icon set to have read
    what is installed (it does that once the bot is ready). For text posted
    once at startup and then left alone -- the rules message -- which would
    otherwise lose the race and keep unicode until the next post. False on
    timeout (fowlengine absent, or an older build without `ready()`); the
    caller carries on with whatever icon() gives."""
    import asyncio
    loop = asyncio.get_running_loop()
    deadline = loop.time() + timeout
    while True:
        mod = shared()
        try:
            if mod is not None and mod.ready():
                return True
        except Exception:
            pass
        if loop.time() >= deadline:
            return False
        await asyncio.sleep(poll)


def emoji_from(value):
    """A config-supplied `emoji:` (`{icon:key}`, plain unicode, or nothing) as
    something a button / select option `emoji=` takes, or None."""
    if not isinstance(value, str) or not value.strip():
        return None
    value = value.strip()
    m = _PLACEHOLDER.fullmatch(value)
    if m:
        return icon_emoji(m.group(1))
    return render(value) or None
