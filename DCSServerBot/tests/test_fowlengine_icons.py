"""
Offline tests for the Vector Strike icon set (icons.py + assets/icons) and
for its use across the plugin: every key the code asks for exists and has a
glyph, the unicode fallback is what renders before an install, the install
only uploads what is missing, and no raw emoji is left in Discord output.
The same checks cover the community plugins (about, announcements, faq, radio,
rules, smartmod, tickets), which borrow the set through their _icons.py.

    python -m pytest DCSServerBot/tests -q
"""
import ast
import asyncio
import importlib.util
import io
import logging
import re
import sys
import tokenize
import unicodedata
from pathlib import Path

import pytest


BOT = Path(__file__).resolve().parent.parent          # <repo>/DCSServerBot
PLUGIN = BOT / "plugins" / "fowlengine"
ICON_DIR = PLUGIN / "assets" / "icons"
EXTENSIONS = BOT / "extensions"
# The plugins that post to Discord alongside fowlengine and borrow its icons
# through their own copy of _icons.py.
COMMUNITY = ("about", "announcements", "faq", "radio", "rules", "smartmod", "tickets")
SHARED_NAME = "plugins.fowlengine.icons"


def load_icons():
    """A fresh copy of icons.py (so one test's active IconSet can't leak)."""
    spec = importlib.util.spec_from_file_location("fe_icons", PLUGIN / "icons.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


ic = load_icons()
log = logging.getLogger("test-icons")

# Emoji that must not appear in Discord-bound text. Plain typography (→ ← ↳ •
# ▲ ▼ ·) is deliberately outside these ranges.
EMOJI = re.compile("[\U0001F000-\U0001FAFF\u2600-\u27BF\u2B00-\u2BFF\u23E9-\u23FA\u2139\u25B6\u25AB]")

# (file, text) pairs that may keep a raw emoji, and why.
RAW_EMOJI_ALLOWED = {
    # render_report() is the markdown issue report for the OPS page and for
    # Claude (/api/logs/issues) -- never posted to Discord.
    ("loganalyzer.py", "No open issues. 🎉"),
}


def _py_sources():
    files = sorted(PLUGIN.glob("*.py")) + sorted(EXTENSIONS.glob("*/extension.py"))
    return [(f, f.read_text(encoding="utf-8")) for f in files]


def _keys_used_in_code() -> dict:
    """icon key -> first place it is used, from every icon lookup in the code:
    icon("k"), icons("k"), self.icons("k"), icon_emoji("k"), self._icon("k"),
    {icon:k} templates -- plus briefing's engine-supplied urgency/kind keys."""
    used = {}
    call = re.compile(r"\b(?:icon|icons|icon_emoji|_icon)\(([^()]*(?:\([^()]*\)[^()]*)*)\)")
    for path, src in _py_sources():
        if path.name == "icons.py":
            continue
        for m in call.finditer(src):
            args = m.group(1)
            if ".get(" in args or not args.strip():
                continue  # data-driven (briefing's urgency/kind) or a definition
            for k in re.findall(r"""["']([a-z0-9_]+)["']""", args):
                used.setdefault(k, f"{path.name}: {m.group(0)}")
        for k in re.findall(r"\{icon:([A-Za-z0-9_]+)\}", src):
            used.setdefault(k, f"{path.name}: {{icon:{k}}}")
    sample = (PLUGIN / "fowlengine.sample.yaml").read_text(encoding="utf-8")
    for k in re.findall(r"\{icon:([a-z0-9_]+)\}", sample):
        used.setdefault(k, f"fowlengine.sample.yaml: {{icon:{k}}}")
    briefing = (PLUGIN / "briefing.py").read_text(encoding="utf-8")
    for tup in re.findall(r"(?:URGENCY_KEYS|TASK_KEYS) = \(([^)]*)\)", briefing):
        for k in re.findall(r'"([a-z0-9_]+)"', tup):
            used.setdefault(k, "briefing.py: URGENCY_KEYS/TASK_KEYS")
    return used


# ---- the table and the PNGs agree ---------------------------------------------------


def test_code_finds_a_meaningful_number_of_keys():
    # guards the scanner itself: if the regex broke, the next test would pass vacuously
    assert len(_keys_used_in_code()) > 60


def test_every_key_used_in_code_has_a_fallback_and_a_glyph():
    missing, no_png = [], []
    for key, where in sorted(_keys_used_in_code().items()):
        if key not in ic.FALLBACK:
            missing.append(where)
            continue
        if not (ICON_DIR / f"{ic.emoji_name(key)}.png").exists():
            no_png.append(where)
    assert not missing, "icon keys with no FALLBACK entry:\n" + "\n".join(missing)
    assert not no_png, "icon keys with no PNG:\n" + "\n".join(no_png)


def test_every_png_is_a_glyph_key_and_every_glyph_key_has_a_png():
    pngs = {p.stem[3:] for p in ICON_DIR.glob("vs_*.png")}
    glyphs = set(ic.glyph_keys())
    assert pngs - glyphs == set(), "PNGs with no FALLBACK entry (or listed as an alias)"
    assert glyphs - pngs == set(), "FALLBACK keys with no PNG (run render_icons.py)"


def test_render_script_draws_exactly_the_glyph_keys():
    src = (ICON_DIR / "render_icons.py").read_text(encoding="utf-8")
    table = src[src.index("ICONS = {"):]
    drawn = re.findall(r'"vs_([a-z0-9_]+)": icon_', table)
    assert len(drawn) == len(set(drawn))
    assert set(drawn) == set(ic.glyph_keys())


def test_aliases_point_at_glyphs_and_keep_their_own_fallback():
    for alias, target in ic.ALIASES.items():
        assert alias in ic.FALLBACK, alias
        assert target in ic.glyph_keys(), f"{alias} -> {target} is not a glyph"
        assert not (ICON_DIR / f"vs_{alias}.png").exists(), f"{alias} is an alias but has its own PNG"


def test_pngs_are_discord_emoji_sized():
    from struct import unpack
    for p in ICON_DIR.glob("vs_*.png"):
        data = p.read_bytes()
        assert len(data) < 256 * 1024, p.name
        assert data[:8] == b"\x89PNG\r\n\x1a\n", p.name
        w, h = unpack(">II", data[16:24])
        assert w == h == 128, p.name


def test_emoji_names_are_valid_discord_names():
    for key in ic.glyph_keys():
        name = ic.emoji_name(key)
        assert re.fullmatch(r"[A-Za-z0-9_]{2,32}", name), name


# ---- rendering before and after the install ----------------------------------------


def test_fallback_renders_the_unicode_stand_in():
    mod = load_icons()                      # nothing active yet
    assert mod.icon("warning") == "⚠️"
    assert mod.icon("target") == "🎯"       # an alias keeps its own unicode
    assert mod.icon("no_such_icon") == ""
    icons = mod.IconSet(bot=object(), log=log)
    assert mod.icon("captured") == icons("captured") == "🏆"
    assert icons("side_blue") == "🟦"
    assert mod.icon_emoji("priority") == "⭐"   # no discord.py here: the raw unicode
    assert mod.icon_emoji("no_such_icon") is None


def test_installed_icons_resolve_to_markup_and_aliases_share_them():
    mod = load_icons()
    icons = mod.IconSet(bot=object(), log=log)
    icons._resolved = {"cas": "<:vs_cas:111>", "warning": "<:vs_warning:222>"}
    assert mod.icon("cas") == "<:vs_cas:111>"
    assert mod.icon("target") == "<:vs_cas:111>"
    assert mod.icon("stale") == "<:vs_warning:222>"
    assert mod.icon("good") == "✅"           # not installed: still the fallback
    assert icons.installed == 2


def test_templates_expand_icon_placeholders_and_still_format():
    mod = load_icons()
    fmt = mod.render("{icon:captured} **[CAPTURED]** {message}")
    assert fmt == "🏆 **[CAPTURED]** {message}"
    assert fmt.format(message="Kobuleti fell") == "🏆 **[CAPTURED]** Kobuleti fell"
    # an operator's plain-emoji template is untouched
    assert mod.render("🏆 {message}") == "🏆 {message}"
    icons = mod.IconSet(bot=object(), log=log)
    icons._resolved = {"captured": "<:vs_captured:9>"}
    assert mod.render("{icon:captured} {message}").format(message="x") == "<:vs_captured:9> x"
    assert mod.render("{icon:nope}|") == "|"


def test_plain_turns_markup_back_into_unicode():
    assert ic.plain("<:vs_warning:123> bfdb down") == "⚠️ bfdb down"
    assert ic.plain("no markup <here>") == "no markup <here>"
    assert ic.plain(None) is None


# ---- install ------------------------------------------------------------------------


class FakeEmoji:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return f"<:{self.name}:{abs(hash(self.name)) % 10**18}>"


class FakeBot:
    def __init__(self, have=(), extra=0):
        self.guilds = []
        self.app = [FakeEmoji(n) for n in have] + [FakeEmoji(f"other_{i}") for i in range(extra)]
        self.created = []

    async def fetch_application_emojis(self):
        return list(self.app)

    async def create_application_emoji(self, *, name, image):
        assert image[:4] == b"\x89PNG"
        self.created.append(name)
        self.app.append(FakeEmoji(name))


def test_install_uploads_only_the_missing_icons_and_is_idempotent():
    mod = load_icons()
    have = ["vs_critical", "vs_cas", "vs_link"]
    bot = FakeBot(have)
    icons = mod.IconSet(bot, log)
    added, skipped, errors = asyncio.run(icons.install(delay=0))
    want = [mod.emoji_name(k) for k in mod.glyph_keys() if mod.emoji_name(k) not in have]
    assert added == bot.created == want
    assert sorted(skipped) == sorted(have)
    assert errors == []
    assert not any(name[3:] in mod.ALIASES for name in bot.created)   # aliases never upload
    assert icons.installed == len(mod.glyph_keys())
    assert mod.icon("target").startswith("<:vs_cas:")                  # refreshed afterwards
    # a second run finds everything and uploads nothing
    added, skipped, errors = asyncio.run(icons.install(delay=0))
    assert (added, errors) == ([], [])
    assert len(skipped) == len(mod.glyph_keys())


def test_install_stops_at_the_application_emoji_limit():
    mod = load_icons()
    bot = FakeBot(extra=mod.APP_EMOJI_LIMIT - 3)
    icons = mod.IconSet(bot, log)
    added, skipped, errors = asyncio.run(icons.install(delay=0))
    assert len(added) == 3
    assert len(errors) == len(mod.glyph_keys()) - 3
    assert all("limit" in e for e in errors)


def test_install_falls_back_to_the_guild_without_the_application_api():
    mod = load_icons()

    class Guild:
        emojis = [FakeEmoji("vs_good")]
        made = []

        async def create_custom_emoji(self, *, name, image, reason):
            self.made.append(name)

    class OldBot:
        guilds = [Guild()]

    icons = mod.IconSet(OldBot(), log)
    g = OldBot.guilds[0]
    added, skipped, errors = asyncio.run(icons.install(g, delay=0))
    assert skipped == ["vs_good"]
    assert added == g.made and "vs_good" not in added
    assert len(added) == len(mod.glyph_keys()) - 1


def test_install_reports_a_failed_upload_and_carries_on():
    mod = load_icons()

    class Flaky(FakeBot):
        async def create_application_emoji(self, *, name, image):
            if name == "vs_bad":
                raise RuntimeError("boom")
            await super().create_application_emoji(name=name, image=image)

    bot = Flaky()
    added, skipped, errors = asyncio.run(mod.IconSet(bot, log).install(delay=0))
    assert errors == ["vs_bad: boom"]
    assert len(added) == len(mod.glyph_keys()) - 1


# ---- no raw emoji left in Discord output --------------------------------------------


_NAMED_ESCAPE = re.compile(r"\\N\{([^}]+)\}")


def _decoded(text: str) -> str:
    """A literal's value where it can be evaluated, so an emoji spelled as an
    escape (`"\\N{OPEN BOOK}"`, `"\\U0001f4d6"`) is caught like a literal one."""
    try:
        value = ast.literal_eval(text)
        if isinstance(value, str):
            return value
    except Exception:
        pass  # an f-string (or a piece of one)

    def lookup(m):
        try:
            return unicodedata.lookup(m.group(1))
        except KeyError:
            return m.group(0)
    return _NAMED_ESCAPE.sub(lookup, text)


def _string_tokens(src: str):
    """(line, text) of every string literal / f-string text part."""
    for tok in tokenize.generate_tokens(io.StringIO(src).readline):
        if tokenize.tok_name[tok.type] in ("STRING", "FSTRING_MIDDLE"):
            yield tok.start[0], _decoded(tok.string)


def test_no_raw_emoji_in_plugin_strings():
    offenders = []
    for path, src in _py_sources():
        if path.name == "icons.py":
            continue  # the FALLBACK table is where the unicode lives
        for line, text in _string_tokens(src):
            if not EMOJI.search(text):
                continue
            if any(path.name == f and snippet in text for f, snippet in RAW_EMOJI_ALLOWED):
                continue
            offenders.append(f"{path.name}:{line}: {text.strip()[:80]}")
    assert not offenders, ("raw emoji in plugin strings -- use icon('<key>') "
                           "(and add the key to icons.FALLBACK):\n" + "\n".join(offenders))


def test_message_template_defaults_use_icon_placeholders():
    for name in ("commands.py", "listener.py"):
        src = (PLUGIN / name).read_text(encoding="utf-8")
        defaults = re.findall(r"messages\.get\('\w+', \"([^\"]*)\"\)", src)
        assert defaults, name
        for d in defaults:
            assert d.startswith("{icon:"), f"{name}: {d}"


# ---- the community plugins ------------------------------------------------------------


def _community_dir(name):
    return BOT / "plugins" / name


def load_shim(name="radio"):
    """A fresh copy of one plugin's _icons.py, outside any package."""
    spec = importlib.util.spec_from_file_location(f"shim_{name}", _community_dir(name) / "_icons.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _yaml(path):
    yaml = pytest.importorskip("ruamel.yaml")
    return yaml.YAML(typ="safe").load(path.read_text(encoding="utf-8"))["DEFAULT"]


def _community_sources():
    out = []
    for name in COMMUNITY:
        for f in sorted(_community_dir(name).glob("*.py")):
            if f.name != "_icons.py":
                out.append((f, f.read_text(encoding="utf-8")))
    return out


def _community_keys() -> dict:
    """icon key -> first use, across the plugins' code and shipped yaml."""
    used = {}
    call = re.compile(r"\b(?:icon|icon_emoji|unicode)\(\s*[\"']([a-z0-9_]+)[\"']\s*\)")
    ternary = re.compile(r"\bicon_emoji\(\s*[\"']([a-z0-9_]+)[\"'] if .*? else [\"']([a-z0-9_]+)[\"']\s*\)")
    for path, src in _community_sources():
        where = f"{path.parent.name}/{path.name}"
        for m in call.finditer(src):
            used.setdefault(m.group(1), f"{where}: {m.group(0)}")
        for m in ternary.finditer(src):
            for k in m.groups():
                used.setdefault(k, f"{where}: {m.group(0)}")
        for k in re.findall(r"\{icon:([A-Za-z0-9_]+)\}", src):
            used.setdefault(k, f"{where}: {{icon:{k}}}")
    for name in COMMUNITY:
        for y in _community_dir(name).glob("*.yaml"):
            for k in re.findall(r"\{icon:([A-Za-z0-9_]+)\}", y.read_text(encoding="utf-8")):
                used.setdefault(k, f"{name}/{y.name}: {{icon:{k}}}")
    return used


@pytest.fixture
def no_fowlengine(monkeypatch):
    monkeypatch.delitem(sys.modules, SHARED_NAME, raising=False)


@pytest.fixture
def fowlengine_loaded(monkeypatch):
    """fowlengine's icons.py registered under the name DCSServerBot loads it as,
    with a set that has a few icons 'installed'."""
    mod = load_icons()
    monkeypatch.setitem(sys.modules, SHARED_NAME, mod)
    icons = mod.IconSet(bot=object(), log=log)
    icons._resolved = {"lock": "<:vs_lock:11>", "comms": "<:vs_comms:12>",
                       "rules": "<:vs_rules:13>", "rocket": "<:vs_rocket:14>"}
    return mod


def test_every_community_plugin_has_the_same_shim():
    first = (_community_dir(COMMUNITY[0]) / "_icons.py").read_text(encoding="utf-8")
    for name in COMMUNITY[1:]:
        path = _community_dir(name) / "_icons.py"
        assert path.exists(), f"{name} has no _icons.py"
        assert path.read_text(encoding="utf-8") == first, f"{name}/_icons.py differs from {COMMUNITY[0]}'s"


def test_shim_points_at_the_module_fowlengine_loads():
    # DCSServerBot loads plugins as `plugins.<name>`; fowlengine's commands.py
    # imports its icons relatively, so this is the module holding the IconSet.
    assert load_shim().SHARED_MODULE == SHARED_NAME
    assert "from .icons import IconSet" in (PLUGIN / "commands.py").read_text(encoding="utf-8")


def test_shim_fallback_matches_fowlengine():
    shim = load_shim()
    for key, uni in shim.FALLBACK.items():
        assert key in ic.FALLBACK, f"{key} is missing from fowlengine's FALLBACK"
        assert ic.FALLBACK[key] == uni, f"{key}: shim {uni!r} vs fowlengine {ic.FALLBACK[key]!r}"


def test_community_scanner_finds_keys():
    assert len(_community_keys()) > 40


def test_every_key_the_community_plugins_use_is_known_and_drawn():
    shim = load_shim()
    missing, no_png = [], []
    for key, where in sorted(_community_keys().items()):
        if key not in shim.FALLBACK or key not in ic.FALLBACK:
            missing.append(where)
        elif not (ICON_DIR / f"{ic.emoji_name(key)}.png").exists():
            no_png.append(where)
    assert not missing, "keys missing from _icons.FALLBACK / icons.FALLBACK:\n" + "\n".join(missing)
    assert not no_png, "keys with no PNG:\n" + "\n".join(no_png)


def test_no_raw_emoji_in_community_plugin_strings():
    offenders = []
    for path, src in _community_sources():
        for line, text in _string_tokens(src):
            if EMOJI.search(text):
                offenders.append(f"{path.parent.name}/{path.name}:{line}: {text.strip()[:80]}")
    assert not offenders, ("raw emoji in plugin strings -- use icon('<key>') from the plugin's "
                           "_icons.py (and add the key to both FALLBACK tables):\n" + "\n".join(offenders))


def test_no_raw_emoji_in_shipped_community_yaml():
    offenders = []
    for name in COMMUNITY:
        for y in sorted(_community_dir(name).glob("*.yaml")):
            for n, line in enumerate(y.read_text(encoding="utf-8").splitlines(), 1):
                if EMOJI.search(line):
                    offenders.append(f"{name}/{y.name}:{n}: {line.strip()[:80]}")
    assert not offenders, "use {icon:<key>} in the shipped yaml:\n" + "\n".join(offenders)


def test_the_raw_emoji_guard_sees_escaped_emoji():
    src = 'x = "\\N{OPEN BOOK}"\ny = f"{a} \\N{LARGE RED CIRCLE}"\nz = "\\U0001f4d6"\n'
    assert sum(1 for _, t in _string_tokens(src) if EMOJI.search(t)) == 3


# -- the shim without fowlengine (not installed, or not loaded) ---------------------------


def test_shim_falls_back_to_unicode_without_fowlengine(no_fowlengine):
    shim = load_shim()
    assert shim.shared() is None
    assert shim.icon("lock") == "🔒"
    assert shim.icon("radio") == "📻"
    assert shim.icon("no_such_icon") == ""
    assert shim.icon_emoji("ticket") == "🎫"          # a string discord.py takes as emoji=
    assert shim.icon_emoji("no_such_icon") is None
    assert shim.unicode("music") == "🎵"
    assert shim.render("{icon:rules} **RULES** {icon:nope}|") == "📜 **RULES** |"
    assert shim.plain("{icon:rocket} Start") == "🚀 Start"


def test_shim_survives_a_broken_fowlengine(monkeypatch):
    class Broken:
        FALLBACK = None

        def icon(self, key):
            raise RuntimeError("half-loaded")

        icon_emoji = icon

    monkeypatch.setitem(sys.modules, SHARED_NAME, Broken())
    shim = load_shim()
    assert shim.icon("lock") == "🔒"
    assert shim.icon_emoji("lock") == "🔒"
    assert shim.unicode("lock") == "🔒"


def test_shim_covers_keys_an_older_fowlengine_does_not_know(monkeypatch):
    old = load_icons()
    del old.FALLBACK["ticket"]                    # a build from before this key existed
    monkeypatch.setitem(sys.modules, SHARED_NAME, old)
    shim = load_shim()
    assert shim.icon("ticket") == "🎫"
    assert shim.icon_emoji("ticket") == "🎫"


# -- the shim with fowlengine loaded -----------------------------------------------------


def test_shim_resolves_through_fowlengines_active_icon_set(fowlengine_loaded):
    shim = load_shim("tickets")
    assert shim.shared() is fowlengine_loaded
    assert shim.icon("lock") == "<:vs_lock:11>"
    assert shim.icon("radio") == "<:vs_comms:12>"          # alias -> borrowed glyph
    assert shim.icon("ticket") == "🎫"                     # not installed: fowlengine's stand-in
    assert shim.icon_emoji("lock") == "<:vs_lock:11>"       # (a PartialEmoji with discord.py)
    assert shim.unicode("lock") == "🔒"                     # presence / transcript text
    assert shim.render("{icon:radio} Upcoming") == "<:vs_comms:12> Upcoming"
    assert shim.plain("{icon:lock} Close") == "🔒 Close"


def test_shim_picks_up_fowlengine_loaded_after_it(no_fowlengine, monkeypatch):
    shim = load_shim("radio")                      # plugin imported first...
    assert shim.icon("lock") == "🔒"
    mod = load_icons()                             # ...fowlengine loads and refreshes later
    monkeypatch.setitem(sys.modules, SHARED_NAME, mod)
    mod.IconSet(bot=object(), log=log)._resolved = {"lock": "<:vs_lock:5>"}
    assert shim.icon("lock") == "<:vs_lock:5>"


def test_ready_flips_once_the_set_has_been_read():
    mod = load_icons()
    assert not mod.ready()                             # no IconSet yet
    icons = mod.IconSet(FakeBot(), log)
    assert not mod.ready()                             # created, not refreshed
    asyncio.run(icons.refresh())
    assert mod.ready()


def test_wait_ready_returns_once_fowlengine_has_refreshed(monkeypatch):
    mod = load_icons()
    monkeypatch.setitem(sys.modules, SHARED_NAME, mod)
    icons = mod.IconSet(FakeBot(have=["vs_rules"]), log)
    shim = load_shim("rules")

    async def scenario():
        waiter = asyncio.create_task(shim.wait_ready(timeout=5, poll=0.01))
        await asyncio.sleep(0.05)
        assert not waiter.done()                       # still waiting on the refresh
        await icons.refresh()
        return await waiter

    assert asyncio.run(scenario()) is True
    assert shim.icon("rules").startswith("<:vs_rules:")


def test_wait_ready_gives_up_without_fowlengine(no_fowlengine):
    assert asyncio.run(load_shim("rules").wait_ready(timeout=0.05, poll=0.01)) is False


# -- yaml placeholders -----------------------------------------------------------------


def test_emoji_from_takes_placeholders_unicode_and_nothing(no_fowlengine):
    shim = load_shim("faq")
    assert shim.emoji_from("{icon:rocket}") == "🚀"
    assert shim.emoji_from(" 🚀 ") == "🚀"
    assert shim.emoji_from("") is None
    assert shim.emoji_from(None) is None
    assert shim.emoji_from("{icon:no_such_icon}") is None


def test_faq_topic_emoji_are_icon_placeholders(no_fowlengine):
    cfg = _yaml(_community_dir("faq") / "faq.yaml")
    shim = load_shim("faq")
    topics = {k: v for k, v in cfg.items() if isinstance(v, dict) and "title" in v and k != "panel"}
    assert len(topics) >= 9
    for key, topic in topics.items():
        emoji = topic.get("emoji", "")
        assert re.fullmatch(r"\{icon:[a-z0-9_]+\}", emoji), f"{key}: {emoji!r}"
        assert shim.emoji_from(emoji) == shim.unicode(emoji[6:-1]), key
    assert shim.emoji_from(cfg["getting_started"]["emoji"]) == "🚀"
    assert shim.emoji_from(cfg["chat_commands"]["emoji"]) == "⌨️"


def test_faq_topic_emoji_render_as_custom_emoji_once_installed(fowlengine_loaded):
    cfg = _yaml(_community_dir("faq") / "faq.yaml")
    shim = load_shim("faq")
    assert shim.emoji_from(cfg["getting_started"]["emoji"]) == "<:vs_rocket:14>"
    assert shim.render(cfg["comms_and_navaids"]["emoji"]) == "<:vs_comms:12>"


def test_rules_title_placeholder_renders(no_fowlengine):
    cfg = _yaml(_community_dir("rules") / "rules.yaml")
    shim = load_shim("rules")
    assert cfg["title"].startswith("{icon:rules}")
    assert shim.render(cfg["title"]) == "📜 **VECTOR STRIKE — RULES**"
    # an operator's live rules.yaml with plain unicode keeps working
    assert shim.render("📜 **VECTOR STRIKE — RULES**") == "📜 **VECTOR STRIKE — RULES**"
    assert "{icon:rules}" in (_community_dir("rules") / "commands.py").read_text(encoding="utf-8")


def test_rules_title_renders_as_custom_emoji_once_installed(fowlengine_loaded):
    cfg = _yaml(_community_dir("rules") / "rules.yaml")
    assert load_shim("rules").render(cfg["title"]) == "<:vs_rules:13> **VECTOR STRIKE — RULES**"


def test_about_yaml_loads_and_documents_the_placeholder(no_fowlengine):
    path = _community_dir("about") / "about.yaml"
    cfg = _yaml(path)
    shim = load_shim("about")
    assert cfg["about_us"]["title"]
    assert '#     emoji: "{icon:globe}"' in path.read_text(encoding="utf-8")
    assert shim.emoji_from("{icon:globe}") == "🌐"
    # the built-in link buttons use placeholders too
    src = (_community_dir("about") / "commands.py").read_text(encoding="utf-8")
    for key in ("globe", "book", "dashboard"):
        assert f'"emoji": "{{icon:{key}}}"' in src
