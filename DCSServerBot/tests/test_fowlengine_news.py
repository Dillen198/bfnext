"""
Offline tests for the war diary -> Discord feed (newsfeed.py, and the posting
path in commands.py). No bot, no Discord, no bfdb, no image API: bfdb's
/api/news replies are fixtures and the channel / HTTP session are fakes.

    python -m pytest DCSServerBot/tests -q
"""
import ast
import asyncio
import importlib.util
import io
import logging
import types
from pathlib import Path

import pytest

BOT = Path(__file__).resolve().parent.parent          # <repo>/DCSServerBot
PLUGIN = BOT / "plugins" / "fowlengine"

_spec = importlib.util.spec_from_file_location("fe_newsfeed", PLUGIN / "newsfeed.py")
nf = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(nf)

log = logging.getLogger("test-news")

PNG = b"\x89PNG\r\n\x1a\n" + b"\0" * 64


def day(d, *, final=True, image=None, pending=False, rnd=5, headline=None, body=None):
    return {
        "day": d, "round": rnd, "final_": final,
        "headline": headline or f"HEADLINE {d}",
        "body": body if body is not None else [f"First paragraph for {d}.", "Second."],
        "items": [{"angle": "x", "subject": "y", "weight": 50, "text": f"Template for {d}."}],
        "facts": {"campaign_day": 3, "theatre": "Caucasus"},
        "image": image, "image_pending": pending,
    }


def news(*days):
    # bfdb answers newest first
    return {"days": sorted(days, key=lambda d: d["day"], reverse=True), "images": True}


# ---- newsfeed.plan: what gets posted, once -------------------------------------------


def test_first_enable_posts_only_the_latest_filed_dispatch():
    data = news(day("2026-09-25"), day("2026-09-26"), day("2026-09-27"), day("2026-09-28", final=False))
    to_post, st = nf.plan(data, None, 1000.0)
    assert [d["day"] for d in to_post] == ["2026-09-27"]          # no backfill, never the running day
    assert set(st["posted"]) == {"5:2026-09-25", "5:2026-09-26"}


def test_empty_diary_on_first_enable_then_first_filed_day_is_posted():
    to_post, st = nf.plan(news(day("2026-09-28", final=False)), None, 0.0)
    assert to_post == [] and st["posted"] == []
    to_post, st = nf.plan(news(day("2026-09-28")), st, 10.0)
    assert [d["day"] for d in to_post] == ["2026-09-28"]


def test_posted_dispatches_are_never_posted_again():
    data = news(day("2026-09-26"), day("2026-09-27"))
    to_post, st = nf.plan(data, None, 0.0)
    for d in to_post:
        st = nf.mark_posted(st, d)
    # every later poll, and a restart (state round-tripped through JSON)
    import json
    st = json.loads(json.dumps(st))
    for t in (60.0, 3600.0, 86400.0):
        again, st = nf.plan(data, st, t)
        assert again == []
    # the next filed day is new
    to_post, st = nf.plan(news(day("2026-09-27"), day("2026-09-28")), st, 90000.0)
    assert [d["day"] for d in to_post] == ["2026-09-28"]


def test_same_date_in_a_new_round_is_a_new_dispatch():
    st = {"posted": ["5:2026-09-28"], "seen": {}}
    to_post, _ = nf.plan(news(day("2026-09-28", rnd=6)), st, 0.0)
    assert [nf.dispatch_id(d) for d in to_post] == ["6:2026-09-28"]


def test_waits_for_a_pending_picture_then_posts_without():
    st = {"posted": [], "seen": {}}
    pending = news(day("2026-09-28", pending=True))
    to_post, st = nf.plan(pending, st, 1000.0)
    assert to_post == [] and st["seen"] == {"5:2026-09-28": 1000.0}
    to_post, st = nf.plan(pending, st, 1000.0 + nf.IMAGE_WAIT_SECS - 1)
    assert to_post == []
    # the picture landed in time
    to_post, _ = nf.plan(news(day("2026-09-28", image="/api/news/image/2026-09-28?instance=vs1&v=1")),
                         st, 1100.0)
    assert len(to_post) == 1
    # or it did not: bounded wait, then out it goes without one
    to_post, _ = nf.plan(pending, st, 1000.0 + nf.IMAGE_WAIT_SECS)
    assert len(to_post) == 1 and not to_post[0]["image"]


def test_no_wait_when_pictures_are_off_or_given_up():
    st = {"posted": [], "seen": {}}
    to_post, _ = nf.plan(news(day("2026-09-28", pending=False)), st, 0.0)
    assert len(to_post) == 1


def test_a_waiting_dispatch_holds_back_later_ones_to_keep_date_order():
    st = {"posted": [], "seen": {}}
    data = news(day("2026-09-27", pending=True), day("2026-09-28"))
    to_post, st = nf.plan(data, st, 0.0)
    assert to_post == []
    to_post, _ = nf.plan(data, st, nf.IMAGE_WAIT_SECS + 1)
    assert [d["day"] for d in to_post] == ["2026-09-27", "2026-09-28"]


def test_garbage_replies_post_nothing():
    for bad in (None, [], {"days": None}, {"days": [None, 3, {"final_": True}]}):
        to_post, st = nf.plan(bad, {"posted": [], "seen": {}}, 0.0)
        assert to_post == []


def test_posted_list_is_bounded():
    st = {"posted": [f"1:{i}" for i in range(500)], "seen": {}}
    _, st = nf.plan(news(), st, 0.0)
    assert len(st["posted"]) == nf.POSTED_KEEP


# ---- newsfeed.build_embed ----------------------------------------------------------


def test_embed_fits_discord_and_links_the_diary():
    d = day("2026-09-27", headline="H" * 400, body=["x" * 3000, "y" * 3000])
    e = nf.build_embed(d, "https://dash.example/", "vs2")
    assert len(e["title"]) <= 256 and len(e["description"]) <= 4096
    assert e["url"] == "https://dash.example/news?instance=vs2"
    assert e["description"].endswith("(https://dash.example/news?instance=vs2)")
    assert e["footer"] == "Day 3 · 2026-09-27 · Caucasus"


def test_embed_falls_back_to_template_sentences_and_works_without_dashboard():
    e = nf.build_embed(day("2026-09-27", body=[]), None, None)
    assert "Template for 2026-09-27." in e["description"] and "url" not in e


def test_only_bfdbs_own_image_route_is_followed():
    assert nf.image_path(day("d", image="/api/news/image/2026-09-27?instance=vs1&v=2"))
    for bad in ("https://evil.example/x.png", "//evil/x", "/api/admin/x", None, 3):
        assert nf.image_path(day("d", image=bad)) is None
    assert nf.attachment_name({"day": "2026-09-27"}, "image/jpeg") == "dispatch-2026-09-27.jpg"
    assert nf.attachment_name({"day": "../../x"}, None) == "dispatch-x.png"


# ---- commands.py _post_news_for, against fakes -----------------------------------


def _posting_cog(channel, *, iid="vs1"):
    """commands.py's _post_news_for + _fetch_news_image on a stand-in cog. The
    module itself needs discord and DCSServerBot, so the two methods are
    lifted out of its source."""
    src = (PLUGIN / "commands.py").read_text(encoding="utf-8")
    fns = [n for n in ast.walk(ast.parse(src))
           if isinstance(n, ast.AsyncFunctionDef) and n.name in ("_post_news_for", "_fetch_news_image")]
    assert len(fns) == 2

    class Embed:
        def __init__(self):
            self.image = None
            self.d = {}

        def set_image(self, url):
            self.image = url

    class File:
        def __init__(self, fp, filename):
            self.data, self.filename = fp.read(), filename

    fake_discord = types.SimpleNamespace(File=File, HTTPException=type("HTTPException", (Exception,), {}))
    clock = types.SimpleNamespace(now=10_000.0)
    ns = {"discord": fake_discord, "newsfeed": nf, "NO_PINGS": "NO_PINGS", "io": io, "asyncio": asyncio,
          "time": types.SimpleNamespace(time=lambda: clock.now),
          "srv_params": lambda name, **kw: {"server": name, **kw}}
    exec(compile(ast.Module(body=fns, type_ignores=[]), "commands.py", "exec"), ns)

    saves = []

    def embed_from_dict(d):
        e = Embed()
        e.d = d
        return e

    Cog = type("Cog", (), {"_post_news_for": ns["_post_news_for"], "_fetch_news_image": ns["_fetch_news_image"]})
    cog = Cog()
    cog.log = log
    cog.news_feed_state = {}
    cog.save_state = lambda: saves.append(dict(cog.news_feed_state))
    cog._channel_for = lambda config, key, name: channel if config.get(key) else None
    cog._instance_id = lambda server: iid
    cog._embed_from_dict = embed_from_dict
    cog.saves, cog.clock = saves, clock
    return cog


class _Resp:
    def __init__(self, status, *, json=None, body=b"", ctype="application/json"):
        self.status, self._json, self._body = status, json, body
        self.headers = {"Content-Type": ctype}
        self.content_length = len(body) if body else None

    async def __aenter__(self):
        return self

    async def __aexit__(self, *a):
        return False

    async def json(self, content_type=None):
        return self._json

    async def read(self):
        return self._body


class _Http:
    """GET /api/news -> the current fixture; image paths -> PNG bytes."""

    def __init__(self):
        self.data = news()
        self.calls = []
        self.image_status = 200

    def get(self, url, params=None):
        self.calls.append((url, params))
        if url.endswith("/api/news"):
            return _Resp(200, json=self.data)
        if "/api/news/image/" in url:
            return _Resp(self.image_status, body=PNG, ctype="image/png")
        return _Resp(404)


class _Channel:
    def __init__(self, fail=None):
        self.id = 77
        self.sent = []
        self.fail = fail

    async def send(self, embed=None, file=None, allowed_mentions=None):
        if self.fail:
            raise self.fail
        self.sent.append((embed, file, allowed_mentions))


SERVER = types.SimpleNamespace(name="[VS] Server 1")
CONFIG = {"news_channel": 77, "api_url": "http://127.0.0.1:8880/", "dashboard_url": "https://dash.example"}


def test_posts_new_dispatch_once_with_the_picture_attached():
    ch, http = _Channel(), _Http()
    cog = _posting_cog(ch)
    img = "/api/news/image/2026-09-27?instance=vs1&v=1"
    http.data = news(day("2026-09-26"), day("2026-09-27", image=img))
    asyncio.run(cog._post_news_for(http, SERVER, CONFIG))
    assert len(ch.sent) == 1
    embed, file, mentions = ch.sent[0]
    assert mentions == "NO_PINGS"
    assert embed.d["title"] == "HEADLINE 2026-09-27"
    assert file.filename == "dispatch-2026-09-27.png" and file.data == PNG
    assert embed.image == "attachment://dispatch-2026-09-27.png"
    # the list was asked for this server's instance, the picture from bfdb itself
    assert http.calls[0] == ("http://127.0.0.1:8880/api/news", {"instance": "vs1", "limit": nf.FETCH_LIMIT})
    assert http.calls[1][0] == "http://127.0.0.1:8880" + img
    # restart: state survives in the state file, nothing is posted twice
    state = cog.news_feed_state
    cog2 = _posting_cog(ch)
    cog2.news_feed_state = state
    for _ in range(3):
        asyncio.run(cog2._post_news_for(http, SERVER, CONFIG))
    assert len(ch.sent) == 1


def test_recorded_before_sending_so_a_discord_error_never_reposts():
    ch = _Channel()
    cog = _posting_cog(ch)
    http = _Http()
    http.data = news(day("2026-09-27"))
    # first poll: the send blows up with discord's HTTPException
    ch.fail = cog._post_news_for.__globals__["discord"].HTTPException("boom")
    asyncio.run(cog._post_news_for(http, SERVER, CONFIG))
    assert ch.sent == [] and "5:2026-09-27" in cog.news_feed_state[SERVER.name]["posted"]
    assert cog.saves  # persisted before the attempt
    ch.fail = None
    asyncio.run(cog._post_news_for(http, SERVER, CONFIG))
    assert ch.sent == []


def test_waits_for_the_picture_then_posts_without_it():
    ch, http = _Channel(), _Http()
    cog = _posting_cog(ch)
    cog.news_feed_state = {SERVER.name: {"posted": [], "seen": {}}}
    http.data = news(day("2026-09-27", pending=True))
    asyncio.run(cog._post_news_for(http, SERVER, CONFIG))
    assert ch.sent == []
    cog.clock.now += nf.IMAGE_WAIT_SECS + 1
    asyncio.run(cog._post_news_for(http, SERVER, CONFIG))
    assert len(ch.sent) == 1 and ch.sent[0][1] is None and ch.sent[0][0].image is None


def test_a_picture_that_will_not_download_does_not_block_the_post():
    ch, http = _Channel(), _Http()
    http.image_status = 404
    cog = _posting_cog(ch)
    cog.news_feed_state = {SERVER.name: {"posted": [], "seen": {}}}
    http.data = news(day("2026-09-27", image="/api/news/image/2026-09-27?instance=vs1&v=1"))
    asyncio.run(cog._post_news_for(http, SERVER, CONFIG))
    assert len(ch.sent) == 1 and ch.sent[0][1] is None


def test_bfdb_error_raises_for_the_loop_to_count():
    class _Down(_Http):
        def get(self, url, params=None):
            return _Resp(502)
    cog = _posting_cog(_Channel())
    with pytest.raises(RuntimeError):
        asyncio.run(cog._post_news_for(_Down(), SERVER, CONFIG))
    assert cog.news_feed_state == {}


def test_no_channel_means_no_request():
    http = _Http()
    cog = _posting_cog(None)
    asyncio.run(cog._post_news_for(http, SERVER, CONFIG))
    assert http.calls == []
