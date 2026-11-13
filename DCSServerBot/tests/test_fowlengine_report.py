"""
Offline tests for the weekly report poster -> Discord: the schedule and the
upload checks (weeklyreport.py) and the cog's posting (cog_report.py, lifted
out of its source onto a stand-in, since the module needs discord.py). The
OPS API route is in test_fowlengine_ops.py.

    python -m pytest DCSServerBot/tests -q
"""
import ast
import asyncio
import base64
import importlib.util
import io
import logging
import os
import re
import types
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest

BOT = Path(__file__).resolve().parent.parent          # <repo>/DCSServerBot
PLUGIN = BOT / "plugins" / "fowlengine"

_spec = importlib.util.spec_from_file_location("fe_weeklyreport", PLUGIN / "weeklyreport.py")
wr = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(wr)

log = logging.getLogger("test")
PNG = b"\x89PNG\r\n\x1a\n" + b"\0" * 32
# Sunday 9 November 2026, 18:00 UTC
SUN = datetime(2026, 11, 8, 18, 0, tzinfo=timezone.utc)


# ---- weeklyreport.py ------------------------------------------------------------

def test_the_schedule_is_off_unless_enabled():
    assert wr.parse_schedule(None) is None
    assert wr.parse_schedule({}) is None
    assert wr.parse_schedule({"weekday": "sun", "hour": 18}) is None
    assert wr.parse_schedule({"enabled": "yes"}) is None, "only a real true turns it on"
    assert wr.parse_schedule({"enabled": True}) == {"weekday": 6, "hour": 18, "days": 7}
    assert wr.parse_schedule({"enabled": True, "weekday": "Friday", "hour": 20, "days": 99}) == \
        {"weekday": 4, "hour": 20, "days": 31}
    assert wr.parse_schedule({"enabled": True, "weekday": 0}) == {"weekday": 0, "hour": 18, "days": 7}
    assert wr.parse_schedule({"enabled": True, "weekday": "someday"}) is None
    assert wr.parse_schedule({"enabled": True, "hour": 24}) is None
    assert wr.parse_schedule({"enabled": True, "hour": "six"}) is None


def test_due_posts_each_slot_once_and_skips_a_stale_one():
    sched = wr.parse_schedule({"enabled": True, "weekday": "sun", "hour": 18})
    assert wr.last_slot(SUN + timedelta(minutes=5), sched) == SUN
    assert wr.last_slot(SUN - timedelta(minutes=5), sched) == SUN - timedelta(days=7)
    key = wr.due(SUN + timedelta(minutes=5), sched, None)
    assert key == SUN.isoformat()
    assert wr.due(SUN + timedelta(minutes=10), sched, key) is None, "already posted"
    assert wr.due(SUN + timedelta(hours=5), sched, "older") == key, "caught up within six hours"
    assert wr.due(SUN + timedelta(hours=7), sched, "older") is None, "too late: skip the week"
    assert wr.due(SUN + timedelta(days=7, minutes=1), sched, key) == (SUN + timedelta(days=7)).isoformat()
    assert wr.due(SUN, None, None) is None


def test_status_names_the_next_post():
    sched = wr.parse_schedule({"enabled": True, "weekday": "sun", "hour": 18})
    st = wr.status(sched, SUN.isoformat(), "war-reports", SUN + timedelta(hours=1))
    assert st["next"] == (SUN + timedelta(days=7)).isoformat()
    assert st["weekday"] == "sun" and st["hour"] == 18 and st["channel"] == "war-reports"
    assert wr.status(sched, None, None, SUN + timedelta(hours=1))["next"] == SUN.isoformat()
    off = wr.status(None, None, "news", SUN)
    assert off["scheduled"] is False and off["next"] is None


def test_print_url_and_channel():
    assert wr.print_url("http://127.0.0.1:8880/", "[VS] Two", None, 7) == \
        "http://127.0.0.1:8880/report/print?days=7&server=%5BVS%5D%20Two"
    assert wr.print_url("https://dash", "x", "redstorm", 3) == "https://dash/report/print?days=3&instance=redstorm"
    assert wr.channel_key({"news_channel": 1}) == "news_channel"
    assert wr.channel_key({"news_channel": 1, "report_channel": 2}) == "report_channel"
    assert wr.channel_key({}) is None


def test_check_upload():
    b64 = base64.b64encode(PNG).decode()
    img, text, name = wr.check_upload({"text": " hi ", "image": f"data:image/png;base64,{b64}",
                                       "filename": "../../weekly report.png"})
    assert img == PNG and text == "hi" and name == "weekly-report.png"
    jpg = base64.b64encode(b"\xff\xd8\xff\xe0rest").decode()
    assert wr.check_upload({"text": "x", "image": jpg})[2] == "weekly-report.jpg"
    for bad, why in [
        ({"text": "x"}, "no poster"),
        ({"text": "", "image": b64}, "no text"),
        ({"text": "x" * 2001, "image": b64}, "2001 characters"),
        ({"text": "x", "image": "%%%"}, "not base64"),
        ({"text": "x", "image": base64.b64encode(b"GIF89a").decode()}, "not a PNG"),
        ({"text": "x", "image": base64.b64encode(PNG + b"\0" * wr.MAX_IMAGE_BYTES).decode()}, "the limit"),
    ]:
        with pytest.raises(ValueError, match=why):
            wr.check_upload(bad)


# ---- cog_report.py, against fakes ---------------------------------------------


class _Loop:
    """tasks.loop's decorator, enough for the class body to define."""
    def __init__(self, fn):
        self.fn = fn

    def before_loop(self, f):
        return f


class _Channel:
    def __init__(self, name="war-reports", fail=False):
        self.name, self.fail, self.sent = name, fail, []

    async def send(self, content=None, file=None, allowed_mentions=None):
        if self.fail:
            raise _HTTPException("nope")
        self.sent.append((content, file.filename, file.data))
        return types.SimpleNamespace(jump_url="https://discord.com/channels/1/2/3")


class _HTTPException(Exception):
    pass


class _File:
    def __init__(self, fp, filename):
        self.data, self.filename = fp.read(), filename


def _cog(servers, configs, channel, capture=None):
    """ReportMixin on a stand-in: the class is lifted out of cog_report.py."""
    src = (PLUGIN / "cog_report.py").read_text(encoding="utf-8")
    cls = next(n for n in ast.parse(src).body if isinstance(n, ast.ClassDef) and n.name == "ReportMixin")
    calls = []

    async def capture_page(browser, url, profile, **kw):
        calls.append((url, kw))
        return capture or (None, None)

    ns = {"discord": types.SimpleNamespace(File=_File, HTTPException=_HTTPException),
          "tasks": types.SimpleNamespace(loop=lambda **kw: _Loop),
          "mapsnap": types.SimpleNamespace(find_browser=lambda c=None: "msedge.exe", capture_page=capture_page),
          "weeklyreport": wr, "NO_PINGS": "NO_PINGS", "asyncio": asyncio, "io": io, "os": os, "re": re,
          "datetime": datetime, "timezone": timezone}
    exec(compile(ast.Module(body=[cls], type_ignores=[]), "cog_report.py", "exec"), ns)
    cog = ns["ReportMixin"]()
    cog.log = log
    cog.bot = types.SimpleNamespace(servers={s.name: s for s in servers},
                                    node=types.SimpleNamespace(config_dir="cfg"))
    cog.get_config = lambda server=None: configs.get(getattr(server, "name", None), configs.get(None, {}))
    cog._instance_id = lambda s: {"vs1": "vs1", "rsr": "redstorm"}.get(s.name)
    cog._is_range = lambda s: False
    cog._channel_for = lambda config, key, name: channel if config.get(key) else None
    cog._branding = lambda server, config: {"brand_name": "Southern Flank 1985", "dashboard_url": "https://rs.example"}
    cog.warned = []
    cog._warn_once = lambda name, why, msg: cog.warned.append(why)
    cog.weekly_report_state = {}
    cog.saves = []
    cog.save_state = lambda: cog.saves.append(dict(cog.weekly_report_state))
    cog.captures = calls
    return cog


SERVERS = [types.SimpleNamespace(name="vs1"), types.SimpleNamespace(name="rsr")]


def test_post_goes_to_the_instances_server_channel():
    ch = _Channel()
    cog = _cog(SERVERS, {"rsr": {"report_channel": 5}, "vs1": {}}, ch)
    res = asyncio.run(cog.post_weekly_report(instance="redstorm", server_name=None, png=PNG,
                                             text="Week 3", filename="w.png", who="Dill"))
    assert res == {"ok": True, "channel": "war-reports", "url": "https://discord.com/channels/1/2/3"}
    assert ch.sent == [("Week 3", "w.png", PNG)]
    # vs1 has neither report_channel nor news_channel
    res = asyncio.run(cog.post_weekly_report(instance="vs1", server_name=None, png=PNG, text="x", filename="w.png"))
    assert res["ok"] is False and "vs1" in res["error"]
    # Discord says no
    cog = _cog(SERVERS, {"rsr": {"news_channel": 5}}, _Channel(fail=True))
    res = asyncio.run(cog.post_weekly_report(instance="redstorm", server_name=None, png=PNG, text="x", filename="w.png"))
    assert res["ok"] is False and "refused" in res["error"]


def test_the_schedule_posts_once_with_the_pages_text():
    ch = _Channel()
    cfg = {"rsr": {"report_channel": 5, "api_url": "http://127.0.0.1:8880",
                   "weekly_report": {"enabled": True, "weekday": "sun", "hour": 18, "days": 7}},
           "vs1": {"news_channel": 6}}
    cog = _cog(SERVERS, cfg, ch, capture=(PNG, "**Week 3 on the Southern Flank**"))
    key = SUN.isoformat()
    asyncio.run(cog._scheduled_report(SERVERS[1], cfg["rsr"], key))
    assert cog.saves == [{"rsr": key}], "the slot is recorded before the post"
    url, kw = cog.captures[0]
    assert url == "http://127.0.0.1:8880/report/print?days=7&instance=redstorm"
    assert kw["path"] == "/report/print" and kw["selector"] == "#weekly-poster"
    assert ch.sent == [("**Week 3 on the Southern Flank**", "weekly-report-redstorm-2026-11-08.png", PNG)]
    # vs1 has no schedule: the loop leaves it alone
    status = cog.weekly_report_status("vs1", None)
    assert status["scheduled"] is False and status["server"] == "vs1"
    assert cog.weekly_report_status("redstorm", None)["last_posted"] == key


def test_the_schedule_falls_back_to_a_plain_line_and_skips_a_failed_capture():
    ch = _Channel()
    cfg = {"rsr": {"report_channel": 5, "weekly_report": {"enabled": True}}}
    cog = _cog(SERVERS, cfg, ch, capture=(PNG, None))
    asyncio.run(cog._scheduled_report(SERVERS[1], cfg["rsr"], "k1"))
    assert ch.sent[0][0] == "**Southern Flank 1985: the week at the front**\nhttps://rs.example"
    cog = _cog(SERVERS, cfg, ch, capture=(None, None))
    asyncio.run(cog._scheduled_report(SERVERS[1], cfg["rsr"], "k2"))
    assert len(ch.sent) == 1 and cog.weekly_report_state == {"rsr": "k2"}, "not drawn: skipped, not retried"
    # no channel: warned, nothing recorded
    cog = _cog(SERVERS, {"rsr": {"weekly_report": {"enabled": True}}}, ch)
    asyncio.run(cog._scheduled_report(SERVERS[1], {"weekly_report": {"enabled": True}}, "k3"))
    assert cog.warned == ["report-channel"] and cog.weekly_report_state == {}
