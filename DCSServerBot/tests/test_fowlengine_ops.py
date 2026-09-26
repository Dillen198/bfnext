"""
Offline tests for the FowlEngine auto-update / ops / log-analyzer modules.
Runs the real plugin files (copied into a throwaway package) against fakes of
DCSServerBot's `core`, its servers and the cog -- no bot, Discord or DCS needed.

    python -m venv .venv
    .venv/Scripts/pip install pytest fastapi httpx ruamel.yaml aiohttp psutil
    cd DCSServerBot/tests
    ../../.venv/Scripts/python -m pytest -q
"""
import asyncio
import enum
import gzip
import hashlib
import json
import logging
import os
import shutil
import sys
import tempfile
import time
import types
from pathlib import Path

import pytest

BOT = Path(__file__).resolve().parent.parent          # <repo>/DCSServerBot
PLUGIN = BOT / "plugins" / "fowlengine"
EXT = BOT / "extensions" / "bfbinaries" / "extension.py"
HERE = Path(tempfile.mkdtemp(prefix="fowl-tests-"))   # the throwaway package lives here
PKG = HERE / "fe"

# ---- fake `core` ---------------------------------------------------------------


class Status(enum.Enum):
    UNREGISTERED = 'Unregistered'
    SHUTDOWN = 'Shutdown'
    LOADING = 'Loading'
    RUNNING = 'Running'
    PAUSED = 'Paused'
    STOPPED = 'Stopped'
    SHUTTING_DOWN = 'Shutting down'


class UploadStatus(enum.Enum):
    OK = 1


core = types.ModuleType("core")
core.Status = Status
core.UploadStatus = UploadStatus
sys.modules["core"] = core

# copy the plugin modules into a plain package (the real __init__ imports discord)
if PKG.exists():
    shutil.rmtree(PKG)
PKG.mkdir()
(PKG / "__init__.py").write_text("")
for name in ("autoupdate.py", "opsapi.py", "loganalyzer.py", "procman.py", "upload.py", "rangefeed.py"):
    shutil.copy(PLUGIN / name, PKG / name)
sys.path.insert(0, str(HERE))

from fe import autoupdate as au  # noqa: E402
from fe import loganalyzer as la  # noqa: E402
from fe import opsapi as oa  # noqa: E402
from fe import procman as pmmod  # noqa: E402

log = logging.getLogger("test")


def sha(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()


# ---- fakes ------------------------------------------------------------------


class FakeInstance:
    def __init__(self, home):
        self.home = str(home)
        self.locals = {}


class FakeNode:
    name = "NODE"
    is_remote = False
    locals = {}

    def __init__(self, config_dir):
        self.config_dir = str(config_dir)
        self.restarted = False

    async def restart(self):
        self.restarted = True


class FakeServer:
    def __init__(self, name, home, node, dll_path, staging, players=0):
        self.name = name
        self.instance = FakeInstance(home)
        self.instance.locals = {"extensions": {"BFBinaries": {"dll_path": str(dll_path),
                                                              "staging_dir": str(staging)}}}
        self.node = node
        self.status = Status.RUNNING
        self._players = players
        self.restart_time = None
        self.current_mission = None
        self.calls = []

    def get_active_players(self):
        return [object()] * self._players

    def is_populated(self):
        return self.status in (Status.RUNNING, Status.PAUSED) and self._players > 0

    async def shutdown(self):
        self.calls.append("shutdown")
        self.status = Status.SHUTDOWN

    async def startup(self):
        self.calls.append("startup")
        # BFBinaries' prepare() would run here; emulate its swap
        ext_swap(self)
        self.status = Status.RUNNING


def ext_swap(server):
    """What extensions/bfbinaries does on startup: pending -> live, backup."""
    b = server.instance.locals["extensions"]["BFBinaries"]
    pend = os.path.join(b["staging_dir"], "bflib.dll.pending")
    if not os.path.exists(pend):
        return
    side = {}
    if os.path.exists(pend + ".json"):
        side = json.load(open(pend + ".json"))
    live = b["dll_path"]
    backup = f"{live}.backup-{time.time_ns()}"
    shutil.copy2(live, backup)
    os.replace(pend, live)
    if os.path.exists(pend + ".json"):
        os.remove(pend + ".json")
    server.last_swap = (backup, side)


class FakeBot:
    def __init__(self, servers):
        self.servers = {s.name: s for s in servers}


class FakeCog:
    def __init__(self, tmp, config, servers, node):
        self.log = log
        self.bot = FakeBot(servers)
        self.bot.node = node
        self.node = node
        self._config = config
        self.locals = {"DEFAULT": config}
        self.notices = []
        self.procman = None
        self._bfdb_admin_password = "pw"

    def get_config(self, server=None):
        return self._config

    def _ext_cfg(self, server, name):
        return (server.instance.locals.get("extensions") or {}).get(name) or {}

    def _instance_kind(self, server):
        return "campaign"

    def _is_range(self, server):
        return False

    async def notify_ops(self, msg):
        self.notices.append(msg)

    def reload_plugin_config(self):
        pass

    def sync_update_tuning(self):
        pass


@pytest.fixture
def world(tmp_path):
    """Two campaign servers + a bfdb home + a folder release source."""
    node = FakeNode(tmp_path / "config")
    os.makedirs(node.config_dir, exist_ok=True)
    home = tmp_path / "bfdb_home"
    home.mkdir()
    (home / "bfdb").mkdir()
    (home / "bfdb" / "db").write_bytes(b"old-db")
    exe = home / "bfdb.exe"
    exe.write_bytes(b"bfdb-v1")
    servers = []
    for n in ("vs1", "vs2"):
        shome = tmp_path / n
        (shome / "Scripts").mkdir(parents=True)
        (shome / "Logs").mkdir()
        dll = shome / "Scripts" / "bflib.dll"
        dll.write_bytes(b"bflib-v1")
        servers.append(FakeServer(n, shome, node, dll, shome / "_staging"))
    rel = tmp_path / "releases"
    cfg = {
        "bfdb": {"manage": True, "exe": str(exe), "home": str(home),
                 "staging_dir": str(home / "_staging"),
                 "dcsserverbot_url": "http://127.0.0.1:9876/stats", "dcsserverbot_api_key": "KEY123"},
        "autoupdate": {"enabled": True, "source": "folder", "folder": str(rel),
                       "files": ["bflib.dll", "bfdb.exe"], "apply": "when_idle", "idle_minutes": 0},
        "issues": {"scan_seconds": 15},
    }
    cog = FakeCog(tmp_path, cfg, servers, node)
    cog.procman = pmmod.Procman(log, cfg, cog.notify_ops)
    return types.SimpleNamespace(tmp=tmp_path, cog=cog, servers=servers, home=home, exe=exe, rel=rel, cfg=cfg)


def publish(rel: Path, tag: str, files: dict, built: str, channel="stable"):
    d = rel / tag
    d.mkdir(parents=True)
    man = {"schema": 1, "tag": tag, "git": tag[-6:], "built": built, "channel": channel,
           "files": {}}
    for name, data in files.items():
        (d / name).write_bytes(data)
        man["files"][name] = {"sha256": sha(data), "size": len(data), "git": tag[-6:]}
    (d / "manifest.json").write_text(json.dumps(man))
    return man


# ---- pure helpers ---------------------------------------------------------------------


def test_parse_manifest_rejects_bad():
    with pytest.raises(ValueError):
        au.parse_manifest({"tag": "x", "files": {"bflib.dll": {"sha256": "nothex"}}})
    with pytest.raises(ValueError):
        au.parse_manifest({"files": {}})
    m = au.parse_manifest({"tag": "t", "files": {"bflib.dll": {"sha256": "a" * 64}, "weird.bin": {}}})
    assert list(m["files"]) == ["bflib.dll"]


def test_pick_github_release():
    rels = [
        {"tag_name": "engine-3", "draft": True, "assets": [{"name": "manifest.json"}]},
        {"tag_name": "engine-2", "prerelease": True, "assets": [{"name": "manifest.json"}]},
        {"tag_name": "other-9", "assets": [{"name": "manifest.json"}]},
        {"tag_name": "engine-1", "assets": [{"name": "manifest.json"}]},
    ]
    assert au.pick_github_release(rels, "engine-", "stable")["tag"] == "engine-1"
    assert au.pick_github_release(rels, "engine-", "beta")["tag"] == "engine-2"
    # newest usable is bad -> nothing (never "update" to something older)
    assert au.pick_github_release(rels, "engine-", "beta", {"engine-2"}) is None


def test_in_window():
    from datetime import datetime
    assert au.in_window(None)
    assert au.in_window("03:00-07:00", datetime(2026, 1, 1, 4, 0))
    assert not au.in_window("03:00-07:00", datetime(2026, 1, 1, 8, 0))
    assert au.in_window("22:00-02:00", datetime(2026, 1, 1, 23, 30))
    assert au.in_window("22:00-02:00", datetime(2026, 1, 1, 1, 0))
    assert not au.in_window("22:00-02:00", datetime(2026, 1, 1, 12, 0))
    assert au.in_window("garbage")


def test_file_needs_update():
    assert au.file_needs_update("a", "a", None) == "current"
    assert au.file_needs_update("a", "b", "a") == "staged"
    assert au.file_needs_update("a", "b", "c") == "stage"


# ---- updater end to end (folder source) ----------------------------------------------


def test_check_stages_and_respects_manual_upload(world):
    publish(world.rel, "engine-2026.09.26-aaaaaa", {"bflib.dll": b"bflib-v2", "bfdb.exe": b"bfdb-v2"},
            "2026-09-26T10:00:00Z")
    # an admin already dropped a bflib.dll for vs2 by hand
    s2 = world.servers[1]
    st2 = s2.instance.locals["extensions"]["BFBinaries"]["staging_dir"]
    os.makedirs(st2)
    Path(st2, "bflib.dll.pending").write_bytes(b"manual-build")

    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    world.cog.updater = upd
    res = asyncio.run(upd.check(reason="test"))
    assert res["ok"], res
    s1 = world.servers[0]
    st1 = s1.instance.locals["extensions"]["BFBinaries"]["staging_dir"]
    side = json.load(open(Path(st1, "bflib.dll.pending.json")))
    assert side["source"] == "autoupdate" and side["tag"] == "engine-2026.09.26-aaaaaa"
    assert Path(st1, "bflib.dll.pending").read_bytes() == b"bflib-v2"
    # the manual upload is untouched
    assert Path(st2, "bflib.dll.pending").read_bytes() == b"manual-build"
    # bfdb staged globally
    assert Path(world.cfg["bfdb"]["staging_dir"], "bfdb.exe.pending").read_bytes() == b"bfdb-v2"
    assert any("Engine update" in n for n in world.cog.notices)

    # a second check stages nothing new
    res2 = asyncio.run(upd.check(reason="again"))
    assert res2["staged"] == []


def test_sha_mismatch_refused(world):
    man = publish(world.rel, "engine-bad-000001", {"bflib.dll": b"bflib-v2"}, "2026-09-26T10:00:00Z")
    (world.rel / "engine-bad-000001" / "bflib.dll").write_bytes(b"tampered")
    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    res = asyncio.run(upd.check())
    assert not res["ok"] and "sha256 mismatch" in res["error"]
    st1 = world.servers[0].instance.locals["extensions"]["BFBinaries"]["staging_dir"]
    assert not Path(st1, "bflib.dll.pending").exists()
    assert man


def test_when_idle_applies_dll_then_probation_passes(world):
    publish(world.rel, "engine-r2-bbbbbb", {"bflib.dll": b"bflib-v2"}, "2026-09-26T10:00:00Z")
    world.cfg["autoupdate"]["files"] = ["bflib.dll"]
    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    world.cog.updater = upd
    s1, s2 = world.servers
    s2._players = 3  # vs2 is busy
    asyncio.run(upd.check())
    upd._track_idle()
    asyncio.run(upd._apply_phase())
    assert s1.calls == ["shutdown", "startup"]           # empty -> restarted onto the new DLL
    assert s2.calls == []                                 # populated -> left alone
    assert Path(s1.instance.locals["extensions"]["BFBinaries"]["dll_path"]).read_bytes() == b"bflib-v2"

    # the extension hands the swap to the updater
    backup, side = s1.last_swap
    upd.begin_dll_probation(s1, "bflib.dll", s1.instance.locals["extensions"]["BFBinaries"]["dll_path"],
                            backup, side)
    # bflib writes its build sidecar with the expected git
    Path(s1.instance.home, "Logs", "bfnext-bflib-build.json").write_text(
        json.dumps({"name": "bflib", "git": "bbbbbb"}))
    upd._last_status = {s1.name: "RUNNING", s2.name: "RUNNING"}
    asyncio.run(upd._probation_phase(30))
    p = upd.state["probation"][s1.name]
    assert p["loaded_at"]
    p["loaded_at"] -= 11 * 60
    asyncio.run(upd._probation_phase(30))
    assert s1.name not in upd.state["probation"]
    assert any("passed probation" in n for n in world.cog.notices)


def test_crash_during_probation_rolls_back_and_marks_bad(world):
    publish(world.rel, "engine-r3-cccccc", {"bflib.dll": b"bflib-v3"}, "2026-09-26T10:00:00Z")
    world.cfg["autoupdate"]["files"] = ["bflib.dll"]
    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    world.cog.updater = upd
    s1, s2 = world.servers
    s2._players = 2
    asyncio.run(upd.check())
    upd._track_idle()
    asyncio.run(upd._apply_phase())
    backup, side = s1.last_swap
    live = s1.instance.locals["extensions"]["BFBinaries"]["dll_path"]
    upd.begin_dll_probation(s1, "bflib.dll", live, backup, side)
    # DCS dies without an orderly shutdown
    upd._last_status = {s1.name: "RUNNING"}
    s1.status = Status.SHUTDOWN
    s1.calls.clear()
    asyncio.run(upd._probation_phase(30))
    assert "engine-r3-cccccc" in upd.bad
    assert s1.calls == ["startup"]                       # started back up...
    assert Path(live).read_bytes() == b"bflib-v1"        # ...onto the old engine
    # vs2 never gets the bad release
    st2 = s2.instance.locals["extensions"]["BFBinaries"]["staging_dir"]
    assert not Path(st2, "bflib.dll.pending").exists()
    assert any("rolling" in n for n in world.cog.notices)
    # and a new check doesn't offer it again
    res = asyncio.run(upd.check())
    assert res["latest"] is None


def test_never_loaded_rolls_back(world):
    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    world.cog.updater = upd
    s1 = world.servers[0]
    live = s1.instance.locals["extensions"]["BFBinaries"]["dll_path"]
    backup = live + ".backup-1"
    shutil.copy2(live, backup)
    Path(live).write_bytes(b"manual-broken")
    upd.begin_dll_probation(s1, "bflib.dll", live, backup, {"uploader": "someone"})
    upd._last_status = {s1.name: "RUNNING"}
    world.cfg["autoupdate"]["load_timeout_minutes"] = 2
    upd.reload_config()
    for _ in range(5):
        asyncio.run(upd._probation_phase(30))
    assert Path(live).read_bytes() == b"bflib-v1"
    assert s1.calls == ["shutdown", "startup"]


def test_overrides(world):
    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    upd.set_overrides({"apply": "next_restart", "apply_window": "03:00-07:00"})
    assert upd.cfg.apply == "next_restart"
    with pytest.raises(ValueError):
        upd.set_overrides({"apply": "whenever"})
    with pytest.raises(ValueError):
        upd.set_overrides({"repo": "evil/repo"})
    with pytest.raises(ValueError):
        upd.set_overrides({"apply_window": "3pm"})
    upd2 = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    assert upd2.cfg.apply == "next_restart"  # persisted


# ---- procman: bfdb snapshot, probation, rollback ---------------------------------------


def test_bfdb_swap_snapshots_db_and_rolls_back(world):
    pm = world.cog.procman
    staging = pm.staging_dir
    os.makedirs(staging, exist_ok=True)
    Path(staging, "bfdb.exe.pending").write_bytes(b"bfdb-v2")
    Path(staging, "bfdb.exe.pending.json").write_text(json.dumps({"source": "autoupdate", "tag": "engine-x"}))
    swapped = []
    rolled = []
    pm.on_swapped = swapped.append
    pm.on_rollback = lambda tag, why: rolled.append((tag, why))
    note = pm.apply_staged("bfdb.exe", pm.exe)
    assert "DB snapshot" in note and "engine-x" in note
    assert world.exe.read_bytes() == b"bfdb-v2"
    assert pm.probation and pm.probation["db_snapshot"]
    assert swapped and swapped[0]["tag"] == "engine-x"
    # probation survives a bot restart
    pm2 = pmmod.Procman(log, world.cfg, world.cog.notify_ops)
    assert pm2.probation["tag"] == "engine-x"

    # the new build writes to the DB, then dies
    (world.home / "bfdb" / "db").write_bytes(b"new-format-db")

    async def fake_start(pw):
        pm.started = True

    async def fake_term(proc, label):
        return None
    pm.start = fake_start
    pm._terminate = fake_term
    pm._kill_orphan_bfdb = lambda: None
    msg = asyncio.run(pm.rollback_bfdb("pw", "test"))
    assert "rolled back" in msg
    assert world.exe.read_bytes() == b"bfdb-v1"
    assert (world.home / "bfdb" / "db").read_bytes() == b"old-db"
    assert rolled == [("engine-x", "test")]
    assert pm.probation is None
    assert any(n.startswith("bfdb.failed-") for n in os.listdir(world.home))


def test_bfdb_probation_exit_triggers_rollback(world):
    pm = world.cog.procman
    pm.probation = {"tag": "engine-y", "swapped_at": time.time(), "healthy_at": None, "exits": 0,
                    "backup": None, "db_snapshot": None}
    called = []

    async def fake_rb(pw, why):
        called.append(why)
        pm.probation = None
        return "rolled"
    pm.rollback_bfdb = fake_rb
    pm.is_running = lambda: False
    assert asyncio.run(pm._probation_tick(False, "pw"))
    assert called and "exited" in called[0]


# ---- log analyzer -------------------------------------------------------------------


ENGINE_LOG = """13:05:25 [INFO] initializing db
13:05:26 [WARN] (20) bflib::db: objective "Kutaisi" has no logistics hub (id 12345)
13:05:27 [WARN] (20) bflib::db: objective "Batumi" has no logistics hub (id 999)
13:05:28 [ERROR] (20) bflib: panicked at bflib/src/lib.rs:4422:9: index out of bounds: the len is 3 but the index is 7
stack backtrace:
   0: std::panicking
13:05:29 [INFO] player 0123456789abcdef0123456789abcdef joined from 192.168.1.20
"""

BOT_LOG = """2026-09-26 13:04:38.123 INFO\tstarting
2026-09-26 13:04:39.001 ERROR\tFowlEngine: tick failed
Traceback (most recent call last):
  File "plugins/fowlengine/commands.py", line 10, in tick
    x = y[3]
IndexError: list index out of range
2026-09-26 13:04:40.001 INFO\tok
"""

DCS_LOG = """2026-09-26 13:04:38.123 ERROR   ASSET (Main): texture missing foo.dds
2026-09-26 13:04:39.123 ERROR   SCRIPTING (Main): [string "bflib"]:12: attempt to index a nil value
2026-09-26 13:04:40.123 ALERT   EDCORE (Main): # -------------- 20260926-130440 --------------
"""


def test_parse_and_fingerprint():
    ents = la.parse_entries("engine:vs1", ENGINE_LOG.splitlines(), "WARN")
    assert [e.level for e in ents] == ["WARN", "WARN", "PANIC"]
    assert la.fingerprint(ents[0]) == la.fingerprint(ents[1])  # same bug, different ids/names? no --
    assert "stack backtrace:" in ents[2].full
    bot = la.parse_entries("bot", BOT_LOG.splitlines())
    assert len(bot) == 1 and "IndexError" in bot[0].full
    assert "IndexError" in la.signature(bot[0])
    dcs = la.parse_entries("dcs:vs1", DCS_LOG.splitlines())
    assert [e.level for e in dcs] == ["ERROR", "CRASH"]   # the ASSET chatter is not an issue


def test_scrub():
    s = la.scrub("pw hunter22 from 10.0.0.5 ucid 0123456789abcdef0123456789abcdef "
                 "https://discord.com/api/webhooks/1/abc token=XYZXYZXYZ", ["hunter22"])
    assert "hunter22" not in s and "10.0.0.5" not in s and "0123456789abcdef" not in s
    assert "webhooks/1" not in s and "XYZXYZ" not in s


def test_analyzer_scan_rotation_archive_and_report(world):
    s1 = world.servers[0]
    logf = Path(s1.instance.home, "Logs", "bfnext.txt")
    logf.write_text(ENGINE_LOG)
    an = la.LogAnalyzer(world.cog, log, str(world.tmp / "config" / "issues.json"))
    world.cog.issues = an
    r = asyncio.run(an.scan())
    issues = an.listing()
    assert len(issues) == 2  # the two WARNs are one issue; the panic another
    warn = next(i for i in issues if i["level"] == "WARN")
    assert warn["count"] == 2
    # first sight = backlog -> recorded but not announced
    assert not any("New" in n for n in world.cog.notices)

    # new lines + rotation between scans: nothing may be lost
    with open(logf, "a") as fh:
        fh.write("13:06:00 [ERROR] (20) bflib: last words before the crash\n")
    rotated = logf.with_name("bfnext20260926T130600Z.txt")
    os.rename(logf, rotated)
    logf.write_text("13:07:00 [ERROR] (20) bflib: fresh start error\n")
    r = asyncio.run(an.scan())
    sigs = [i["signature"] for i in an.listing()]
    assert any("last words" in s for s in sigs), sigs
    assert any("fresh start" in s for s in sigs), sigs
    assert any("New" in n for n in world.cog.notices)  # live errors are announced

    # archive holds everything, including INFO lines and the scrub-free raw text
    idx = an.archive_index()
    eng = next(x for x in idx if x["source"].startswith("engine"))
    day = eng["days"][0]["date"]
    lines = an.archive_read(eng["source"], day, 1000)
    assert any("initializing db" in l for l in lines)
    assert any("last words" in l for l in lines)
    assert any("fresh start" in l for l in lines)
    assert an.archive_read(eng["source"], day, 1000, grep="fresh") == ["13:07:00 [ERROR] (20) bflib: fresh start error"]

    # the report is scrubbed
    rep = an.report()
    assert "# Fowl Engine issue report" in rep
    assert "0123456789abcdef0123456789abcdef" not in rep

    # regressions
    fid = warn["id"]
    an.set_status(fid, "fixed")
    with open(logf, "a") as fh:
        fh.write('13:08:00 [WARN] (20) bflib::db: objective "Poti" has no logistics hub (id 7)\n')
    asyncio.run(an.scan())
    assert an.state["issues"][fid]["status"] == "regressed"

    # a state reload (bot restart) keeps issues and cursors
    an2 = la.LogAnalyzer(world.cog, log, str(world.tmp / "config" / "issues.json"))
    assert fid in an2.state["issues"]
    r = asyncio.run(an2.scan())
    assert r["read"].get(f"engine:{s1.name}") == 0


def test_archive_maintenance_gzips_and_prunes(world):
    an = la.LogAnalyzer(world.cog, log, str(world.tmp / "config" / "issues.json"))
    root = Path(an.archive_root())
    d = root / "bot"
    d.mkdir(parents=True)
    (d / "2020-01-01.log").write_text("ancient\n")
    (d / "2099-01-01.log").write_text("future\n")   # not today, not expired -> compressed
    an._archive_maintenance()
    assert not (d / "2020-01-01.log").exists()
    assert (d / "2099-01-01.log.gz").exists()
    assert gzip.open(d / "2099-01-01.log.gz", "rt").read() == "future\n"
    assert an.archive_read("bot", "2099-01-01") == ["future"]


# ---- ops API over HTTP --------------------------------------------------------------


def test_ops_api_http(world):
    from fastapi import FastAPI
    from fastapi.testclient import TestClient

    cfgfile = Path(world.cog.node.config_dir, "plugins", "fowlengine.yaml")
    cfgfile.parent.mkdir(parents=True, exist_ok=True)
    cfgfile.write_text(
        "# top comment\nDEFAULT:\n  admin_password: \"s3cretpw\"\n  bfdb:\n    manage: true\n"
        "    exe: x.exe\n    home: h\n    dcsserverbot_api_key: KEY123  # keep me\n"
        "  gci:\n    blue_eam_password: ''\n")
    world.cog.locals = {"DEFAULT": {**world.cfg, "admin_password": "s3cretpw"}}  # what the bot loaded
    world.cog.updater = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    world.cog.issues = la.LogAnalyzer(world.cog, log, str(world.tmp / "config" / "issues.json"))
    api = oa.OpsApi(world.cog)
    app = FastAPI()
    api.mount(app)
    c = TestClient(app)
    base = "/stats/fowlengine/ops"
    assert c.get(f"{base}/status").status_code == 403
    assert c.get(f"{base}/status", headers={"X-API-Key": "nope"}).status_code == 403
    H = {"X-API-Key": "KEY123"}
    st = c.get(f"{base}/status", headers=H)
    assert st.status_code == 200, st.text
    body = st.json()
    assert {s["name"] for s in body["servers"]} == {"vs1", "vs2"}
    assert body["bfdb"]["managed"] is True
    assert body["updates"]["config"]["source"] == "folder"

    # config: masked on the way out, unmasked on the way in, comments kept
    g = c.get(f"{base}/config", headers=H).json()
    assert "s3cretpw" not in g["yaml"] and "KEY123" not in g["yaml"] and g["masked"] == 2
    edited = g["yaml"].replace("manage: true", "manage: true\n    health_failures: 5")
    p = c.post(f"{base}/config", headers=H, json={"yaml": edited, "base_mtime": g["mtime"]})
    assert p.status_code == 200, p.text
    saved = cfgfile.read_text()
    assert "s3cretpw" in saved and "KEY123" in saved and "health_failures: 5" in saved
    assert "# top comment" in saved and "# keep me" in saved
    # stale base -> 409
    p = c.post(f"{base}/config", headers=H, json={"yaml": edited, "base_mtime": 1.0})
    assert p.status_code == 409
    # broken structure -> 400
    p = c.post(f"{base}/config", headers=H, json={"yaml": "DEFAULT: [1, 2]\n"})
    assert p.status_code == 400
    assert os.listdir(api.config_backup_dir)

    # settings
    r = c.post(f"{base}/update/settings", headers=H, json={"changes": {"apply": "next_restart"}})
    assert r.status_code == 200 and r.json()["config"]["apply"] == "next_restart"
    r = c.post(f"{base}/update/settings", headers=H, json={"changes": {"apply": "bogus"}})
    assert r.status_code == 400

    # issues + report + archive
    Path(world.servers[0].instance.home, "Logs", "bfnext.txt").write_text(ENGINE_LOG)
    assert c.post(f"{base}/issues/scan", headers=H).status_code == 200
    iss = c.get(f"{base}/issues", headers=H).json()
    assert iss["summary"]["open"] == 2
    rep = c.get(f"{base}/issues/report", headers=H)
    assert rep.status_code == 200 and rep.text.startswith("# Fowl Engine issue report")
    idx = c.get(f"{base}/archive", headers=H).json()
    src = next(s for s in idx["sources"] if s["source"].startswith("engine"))
    txt = c.get(f"{base}/archive/read", headers=H,
                params={"source": src["source"], "date": src["days"][0]["date"], "format": "text"})
    assert "initializing db" in txt.text
    fid = iss["issues"][0]["id"]
    assert c.post(f"{base}/issues/status", headers=H, json={"id": fid, "status": "ignored"}).status_code == 200
    assert c.get(f"{base}/issues", headers=H).json()["summary"]["open"] == 1
    # bulk: mark the rest fixed, reopen both, then delete them by id
    rest = [i["id"] for i in iss["issues"][1:]]
    r = c.post(f"{base}/issues/status", headers=H, json={"ids": rest + ["nope"], "status": "fixed"})
    assert r.status_code == 200 and "1 issue" in r.json()["message"]
    assert c.get(f"{base}/issues", headers=H).json()["summary"]["open"] == 0
    all_ids = [fid] + rest
    assert c.post(f"{base}/issues/status", headers=H, json={"ids": all_ids, "status": "open"}).status_code == 200
    assert c.get(f"{base}/issues", headers=H).json()["summary"]["open"] == 2
    assert c.post(f"{base}/issues/status", headers=H, json={"ids": ["nope"], "status": "fixed"}).status_code == 404
    assert c.post(f"{base}/issues/status", headers=H, json={"ids": all_ids, "status": "bogus"}).status_code == 400
    r = c.post(f"{base}/issues/clear", headers=H, json={"ids": [fid]})
    assert r.status_code == 200 and "deleted 1" in r.json()["message"]
    assert len(c.get(f"{base}/issues", headers=H, params={"closed": 1}).json()["issues"]) == 1
    r = c.post(f"{base}/issues/clear", headers=H, json={"which": "all"})
    assert "deleted 1" in r.json()["message"]
    assert c.get(f"{base}/issues", headers=H, params={"closed": 1}).json()["issues"] == []

    # logs tail, with a secret redacted
    Path(world.home, "procman-bfdb-boot.log").write_text("boot with s3cretpw\n")
    lg = c.get(f"{base}/logs", headers=H, params={"which": "bfdb_boot"}).json()
    assert lg["lines"] == ["boot with ***"]

    api.unregister()
    assert not [r for r in app.router.routes if str(getattr(r, "path", "")).startswith(base)]


# ---- the BFBinaries extension hands its swap to probation ---------------------------


def test_bfbinaries_extension_starts_probation(world, monkeypatch):
    import importlib.util

    class Extension:
        def __init__(self, server, config):
            self.server, self.config = server, config
            self.name = "BFBinaries"
            self.log = log
            self.version = "1"

    core.Extension = Extension
    core.Server = object
    te = types.ModuleType("typing_extensions")
    te.override = lambda f: f
    monkeypatch.setitem(sys.modules, "typing_extensions", te)
    spec = importlib.util.spec_from_file_location("bfb_ext", EXT)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)

    s1 = world.servers[0]
    b = s1.instance.locals["extensions"]["BFBinaries"]
    os.makedirs(b["staging_dir"], exist_ok=True)
    Path(b["staging_dir"], "bflib.dll.pending").write_bytes(b"bflib-v9")
    Path(b["staging_dir"], "bflib.dll.pending.json").write_text(
        json.dumps({"source": "autoupdate", "tag": "engine-9", "git": "g9"}))
    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    world.cog.updater = upd
    ext = mod.BFBinaries(s1, dict(b))
    monkeypatch.setattr(ext, "_fowlengine_cog", lambda: world.cog)
    ok = asyncio.run(ext.prepare())
    assert ok is True
    assert Path(b["dll_path"]).read_bytes() == b"bflib-v9"
    p = upd.state["probation"][s1.name]
    assert p["tag"] == "engine-9" and p["git"] == "g9" and os.path.exists(p["backup"])
    assert any("release engine-9" in n for n in world.cog.notices)

    # a rollback swap does NOT start another probation
    upd.state["probation"].clear()
    Path(b["staging_dir"], "bflib.dll.pending").write_bytes(b"bflib-v1")
    Path(b["staging_dir"], "bflib.dll.pending.json").write_text(json.dumps({"rollback": True, "source": "rollback"}))
    asyncio.run(ext.prepare())
    assert not upd.state["probation"]
    assert any("ROLLED BACK" in n for n in world.cog.notices)


# ---- the GitHub source, against a local fake of the GitHub API ---------------------


def test_github_source(world):
    from aiohttp import web

    dll = b"bflib-from-github"
    man = {"schema": 1, "tag": "engine-gh-1", "git": "abc", "built": "2026-09-26T00:00:00Z",
           "files": {"bflib.dll": {"sha256": sha(dll), "size": len(dll)}}}

    async def main():
        app = web.Application()
        base = {}

        async def releases(req):
            u = base["url"]
            return web.json_response([
                {"tag_name": "engine-gh-2", "draft": True, "assets": []},
                {"tag_name": "engine-gh-1", "prerelease": False, "html_url": "x", "body": "notes",
                 "assets": [{"name": "manifest.json", "browser_download_url": f"{u}/dl/manifest.json"},
                            {"name": "bflib.dll", "browser_download_url": f"{u}/dl/bflib.dll"}]}])

        async def dl(req):
            name = req.match_info["name"]
            return web.Response(body=json.dumps(man).encode() if name == "manifest.json" else dll)

        app.router.add_get("/repos/me/bf/releases", releases)
        app.router.add_get("/dl/{name}", dl)
        runner = web.AppRunner(app)
        await runner.setup()
        site = web.TCPSite(runner, "127.0.0.1", 0)
        await site.start()
        port = site._server.sockets[0].getsockname()[1]
        base["url"] = f"http://127.0.0.1:{port}"
        au.GITHUB_API = base["url"]
        world.cfg["autoupdate"].update({"source": "github", "repo": "me/bf", "files": ["bflib.dll"]})
        upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
        res = await upd.check()
        await runner.cleanup()
        return upd, res

    upd, res = asyncio.run(main())
    assert res["ok"] and res["latest"] == "engine-gh-1", res
    st = world.servers[0].instance.locals["extensions"]["BFBinaries"]["staging_dir"]
    assert Path(st, "bflib.dll.pending").read_bytes() == dll
    assert upd.state["latest"]["notes"] == "notes"


def test_fresh_box_applies_staged_bfdb(world):
    """No bfdb.exe yet (a new install): start() must apply the staged one
    instead of giving up."""
    pm = world.cog.procman
    os.remove(world.exe)
    os.makedirs(pm.staging_dir, exist_ok=True)
    Path(pm.staging_dir, "bfdb.exe.pending").write_bytes(b"bfdb-first")
    Path(pm.staging_dir, "bfdb.exe.pending.json").write_text(json.dumps({"source": "autoupdate", "tag": "engine-1"}))
    pm._start_resolver = lambda: asyncio.sleep(0)
    pm._kill_orphan_bfdb = lambda: None
    asyncio.run(pm.start("pw"))          # launching the fake exe fails; that's fine here
    assert world.exe.read_bytes() == b"bfdb-first"
    assert pm.probation and pm.probation["tag"] == "engine-1" and pm.probation["backup"] is None


def test_dcs_texture_lines_are_not_crashes():
    """DCS logs 'No suitable driver found to mount ...crash...texture' on every
    start; only the dump header / exception mean DCS actually crashed."""
    benign = [
        "2026-09-25 20:58:29.878 ERROR   EDCORE (Main): No suitable driver found to mount "
        "mods/terrains/syria/models/crash/crash.texture.min.zip",
        "2026-09-25 20:55:57.590 ERROR   EDCORE (Main): No suitable driver found to mount "
        "mods/terrains/caucasus/models/crashmodels/crashmodels.texture",
    ]
    assert la.parse_entries("dcs:vs1", benign) == []
    real = [
        "2026-09-26 10:22:33.000 ALERT   EDCORE (Main): # -------------- 20260926-102233 --------------",
        "2026-09-26 10:22:33.001 ALERT   EDCORE (Main): # C0000005 ACCESS_VIOLATION at 00007ff6 00:00000000",
    ]
    assert [e.level for e in la.parse_entries("dcs:vs1", real)] == ["CRASH", "CRASH"]
