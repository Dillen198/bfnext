"""
Offline tests for the per-instance Discord branding (branding.py, the news
embed's use of it in newsfeed.py, and the cog's lookup in commands.py). No
bot, no Discord, no bfdb: the config, instances and /api/instances replies
are fixtures.

    python -m pytest DCSServerBot/tests -q
"""
import ast
import asyncio
import importlib.util
import logging
import types
from pathlib import Path

BOT = Path(__file__).resolve().parent.parent          # <repo>/DCSServerBot
PLUGIN = BOT / "plugins" / "fowlengine"


def _load(name):
    spec = importlib.util.spec_from_file_location(f"fe_{name}", PLUGIN / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


br = _load("branding")
nf = _load("newsfeed")

DASH = "https://dash.example/"
CONFIG = {"brand_name": "Vector Strike", "dashboard_url": DASH}
RED_STORM = {"id": "rs1", "dcs_server_name": "[RS] Red Storm", "theme": "redstorm"}

# What every embed carried before branding was per instance.
TODAY = {
    "brand_name": "Vector Strike",
    "logo_url": "https://dash.example/vs-vectorstrike_hd-white.png",
    "colour": "#c8102e",
    "colour_int": 0xC8102E,
    "diary_title": "War Diary",
    "diary_link_text": "Read more in the War Diary",
    "theme": None,
}


# ---- branding.resolve: override > bfdb brand_name > theme > global ---------------------


def test_no_theme_and_no_override_is_todays_branding():
    for inst in (None, {}, {"id": "vs1", "dcs_server_name": "[VS] Server 1"}, {"id": "vs1", "theme": None}):
        assert br.resolve(CONFIG, inst, "[VS] Server 1") == TODAY


def test_brand_name_falls_back_like_before():
    assert br.resolve({})["brand_name"] == "Fowl Engine"
    assert br.resolve({})["logo_url"] is None              # no dashboard_url, no logo (as before)


def test_theme_defaults_apply_when_bfdb_reports_the_theme():
    b = br.resolve(CONFIG, RED_STORM)
    assert b["brand_name"] == "Southern Flank 1985"
    assert b["logo_url"] == "https://dash.example/redstorm/emblem.png"
    assert b["colour"] == "#b3261e" and b["colour_int"] == 0xB3261E
    assert b["diary_title"] == "The Allied Dispatch"
    assert b["diary_link_text"] == "Read more in The Allied Dispatch"   # no "the The"
    assert b["theme"] == "redstorm"
    assert br.resolve(CONFIG, {**RED_STORM, "theme": "RedStorm "})["theme"] == "redstorm"


def test_an_unknown_theme_reads_as_unset():
    assert br.resolve(CONFIG, {"id": "x", "theme": "space-opera"}) == {**TODAY, "theme": "space-opera"}


def test_override_beats_theme_field_by_field():
    cfg = {**CONFIG, "branding": {"rs1": {"brand_name": "Operation Northern Wind", "color": "#123456"}}}
    b = br.resolve(cfg, RED_STORM)
    assert b["brand_name"] == "Operation Northern Wind"
    assert b["colour"] == "#123456"                                    # `color` works too
    # the rest still come from the theme
    assert b["diary_title"] == "The Allied Dispatch"
    assert b["logo_url"] == "https://dash.example/redstorm/emblem.png"


def test_override_beats_global_without_a_theme_and_is_keyed_by_id_or_server_name():
    block = {"brand_name": "Desert Fury", "logo": "https://cdn.example/df.png", "colour": "0a0b0c",
             "diary_title": "Desert Times", "diary_link_text": "More from the front"}
    for key in ("vs2", "[VS] Server 2"):
        cfg = {**CONFIG, "branding": {key: block}}
        b = br.resolve(cfg, {"id": "vs2", "dcs_server_name": "[VS] Server 2"})
        assert b == {"brand_name": "Desert Fury", "logo_url": "https://cdn.example/df.png",
                     "colour": "#0a0b0c", "colour_int": 0x0A0B0C, "diary_title": "Desert Times",
                     "diary_link_text": "More from the front", "theme": None}
    # another instance's block is not this one's
    cfg = {**CONFIG, "branding": {"vs2": block}}
    assert br.resolve(cfg, {"id": "vs1"}) == TODAY


def test_bfdbs_brand_name_beats_the_theme_but_not_the_override():
    inst = {**RED_STORM, "brand_name": "Southern Flank"}
    assert br.resolve(CONFIG, inst)["brand_name"] == "Southern Flank"
    cfg = {**CONFIG, "branding": {"rs1": {"brand_name": "Mine"}}}
    assert br.resolve(cfg, inst)["brand_name"] == "Mine"
    # and the global name is only the last resort
    assert br.resolve(CONFIG, {"id": "vs1", "brand_name": "From bfdb"})["brand_name"] == "From bfdb"


def test_garbage_overrides_fall_through():
    cfg = {**CONFIG, "branding": {"rs1": {"brand_name": "  ", "colour": "red", "logo": 7}}}
    b = br.resolve(cfg, RED_STORM)
    assert b["brand_name"] == "Southern Flank 1985" and b["colour"] == "#b3261e"
    assert br.resolve({**CONFIG, "branding": ["not", "a", "map"]}, None) == TODAY
    assert br.resolve({**CONFIG, "branding": {"rs1": "nope"}}, RED_STORM)["brand_name"] == "Southern Flank 1985"


def test_svg_logos_are_posted_as_the_png_beside_them():
    assert br.logo_url("/redstorm/emblem-light.svg", DASH) == "https://dash.example/redstorm/emblem-light.png"
    assert br.logo_url("https://cdn.example/a.SVG?v=2", None) == "https://cdn.example/a.png?v=2"
    assert br.logo_url("redstorm/emblem.png", "https://dash.example") == "https://dash.example/redstorm/emblem.png"
    assert br.logo_url("/x.png", "") is None and br.logo_url("", DASH) is None


# ---- newsfeed.build_embed with branding -------------------------------------------------


def _day(**kw):
    d = {"day": "2026-09-27", "round": 5, "final_": True, "headline": "Kars falls",
         "body": ["Wire copy."], "facts": {"campaign_day": 3, "theatre": "Caucasus"}}
    d.update(kw)
    return d


def test_news_embed_unchanged_for_an_instance_with_no_theme():
    for day in (_day(), _day(headline=None)):
        plain = nf.build_embed(day, DASH, "vs1")
        assert nf.build_embed(day, DASH, "vs1", br.resolve(CONFIG, {"id": "vs1"})) == plain
    assert plain["title"] == "War Diary"
    assert plain["description"] == "Wire copy.\n\n[Read more in the War Diary](https://dash.example/news?instance=vs1)"
    assert "fields" not in plain


def test_news_embed_names_the_instances_paper():
    b = br.resolve(CONFIG, RED_STORM)
    e = nf.build_embed(_day(headline=None), DASH, "rs1", b)
    assert e["title"] == "The Allied Dispatch"
    assert e["description"].endswith("[Read more in The Allied Dispatch](https://dash.example/news?instance=rs1)")
    assert nf.build_embed(_day(), DASH, "rs1", b)["title"] == "Kars falls"     # a headline still wins


def test_news_embed_carries_the_alt_headline_as_one_short_field():
    alt = {"voice": "soviet", "headline": "Guards Tank Army Repels Aggressors at Kars",
           "body": ["A long communique. " * 50], "written_by": "m"}
    e = nf.build_embed(_day(alt=alt), DASH, "rs1", br.resolve(CONFIG, RED_STORM))
    assert e["fields"] == [{"name": "Front Bulletin", "value": alt["headline"], "inline": False}]
    assert "A long communique" not in e["description"]
    # clipped, and nothing for an alt without a headline
    long = nf.build_embed(_day(alt={**alt, "headline": "H" * 2000}), DASH, "rs1")
    assert len(long["fields"][0]["value"]) <= 256
    for bad in (None, {}, {"headline": "  "}, {"headline": 3}, "text"):
        assert "fields" not in nf.build_embed(_day(alt=bad), DASH, "rs1")


# ---- the cog's lookup: instances.json entry + bfdb's /api/instances row ------------------


def _branding_cog(local_instances, *, config=None):
    """commands.py's _branding, _bfdb_instance_row and _refresh_bfdb_instances
    on a stand-in cog (the module itself needs discord and DCSServerBot)."""
    src = (PLUGIN / "commands.py").read_text(encoding="utf-8")
    names = ("_branding", "_bfdb_instance_row", "_refresh_bfdb_instances")
    fns = [n for n in ast.walk(ast.parse(src))
           if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef)) and n.name in names]
    assert len(fns) == 3
    clock = types.SimpleNamespace(now=1000.0)

    def find_instance(insts, name):
        return next((i for i in insts if i.get("dcs_server_name") == name), None)

    ns = {"branding": br, "asyncio": asyncio, "time": types.SimpleNamespace(time=lambda: clock.now),
          "rangefeed": types.SimpleNamespace(find_instance=find_instance)}
    exec(compile(ast.Module(body=fns, type_ignores=[]), "commands.py", "exec"), ns)
    cfg = config or {**CONFIG, "api_url": "http://127.0.0.1:8880"}
    servers = {"[RS] Red Storm": types.SimpleNamespace(name="[RS] Red Storm"),
               "[VS] Server 1": types.SimpleNamespace(name="[VS] Server 1")}
    Cog = type("Cog", (), {n: ns[n] for n in names})
    cog = Cog()
    cog.log = logging.getLogger("test-branding")
    cog.bot = types.SimpleNamespace(servers=servers)
    cog._bfdb_instance_rows = {}
    cog._instances_list = lambda: local_instances
    cog.get_config = lambda server=None: cfg
    cog.clock = clock
    return cog


class _Resp:
    def __init__(self, status, data):
        self.status, self._data = status, data

    async def __aenter__(self):
        return self

    async def __aexit__(self, *a):
        return False

    async def json(self, content_type=None):
        return self._data


class _Http:
    def __init__(self, data, status=200):
        self.data, self.status, self.calls = data, status, []

    def get(self, url, timeout=None):
        self.calls.append(url)
        return _Resp(self.status, self.data)


LOCAL = [{"id": "vs1", "dcs_server_name": "[VS] Server 1"}, {"id": "rs1", "dcs_server_name": "[RS] Red Storm"}]


def test_cog_branding_without_bfdbs_rows_is_todays():
    cog = _branding_cog(LOCAL)
    assert cog._branding("[RS] Red Storm") == TODAY
    assert cog._branding("[VS] Server 1") == TODAY
    assert cog._branding() == TODAY
    assert cog._branding("not a server") == TODAY


def test_cog_branding_takes_the_theme_bfdb_reports():
    cog = _branding_cog(LOCAL)
    reply = {"instances": [{"id": "vs1", "dcs_server_name": "[VS] Server 1", "theme": None},
                           {"id": "rs1", "dcs_server_name": "[RS] Red Storm", "theme": "redstorm",
                            "brand_name": None}]}
    http = _Http(reply)
    asyncio.run(cog._refresh_bfdb_instances(http, "http://127.0.0.1:8880/"))
    assert http.calls == ["http://127.0.0.1:8880/api/instances"]
    rs = cog._branding(cog.bot.servers["[RS] Red Storm"])
    assert rs["brand_name"] == "Southern Flank 1985" and rs["diary_title"] == "The Allied Dispatch"
    assert cog._branding("[VS] Server 1") == TODAY
    # cached: no second request inside the refresh window, one after it
    asyncio.run(cog._refresh_bfdb_instances(http, "http://127.0.0.1:8880"))
    assert len(http.calls) == 1
    cog.clock.now += br.REFRESH_SECS
    asyncio.run(cog._refresh_bfdb_instances(http, "http://127.0.0.1:8880"))
    assert len(http.calls) == 2


def test_cog_branding_reads_the_theme_from_instances_json_too():
    local = [{"id": "rs1", "dcs_server_name": "[RS] Red Storm", "theme": "redstorm"}]
    assert _branding_cog(local)._branding("[RS] Red Storm")["brand_name"] == "Southern Flank 1985"


def test_cog_keeps_the_last_rows_when_bfdb_does_not_answer():
    cog = _branding_cog(LOCAL)
    asyncio.run(cog._refresh_bfdb_instances(None, "http://127.0.0.1:8880",
                                            data={"instances": [{"id": "rs1", "theme": "redstorm"}]}))
    cog.clock.now += br.REFRESH_SECS
    asyncio.run(cog._refresh_bfdb_instances(_Http(None, status=502), "http://127.0.0.1:8880"))

    class _Boom:
        def get(self, url, timeout=None):
            raise ConnectionError("down")
    asyncio.run(cog._refresh_bfdb_instances(_Boom(), "http://127.0.0.1:8880"))
    assert cog._branding("[RS] Red Storm")["theme"] == "redstorm"
