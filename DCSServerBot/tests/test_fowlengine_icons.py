"""
Offline tests for the Vector Strike icon set (icons.py + assets/icons) and
for its use across the plugin: every key the code asks for exists and has a
glyph, the unicode fallback is what renders before an install, the install
only uploads what is missing, and no raw emoji is left in Discord output.

    python -m pytest DCSServerBot/tests -q
"""
import asyncio
import importlib.util
import io
import logging
import re
import tokenize
from pathlib import Path


BOT = Path(__file__).resolve().parent.parent          # <repo>/DCSServerBot
PLUGIN = BOT / "plugins" / "fowlengine"
ICON_DIR = PLUGIN / "assets" / "icons"
EXTENSIONS = BOT / "extensions"


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


def _string_tokens(src: str):
    """(line, text) of every string literal / f-string text part."""
    for tok in tokenize.generate_tokens(io.StringIO(src).readline):
        if tokenize.tok_name[tok.type] in ("STRING", "FSTRING_MIDDLE"):
            yield tok.start[0], tok.string


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
