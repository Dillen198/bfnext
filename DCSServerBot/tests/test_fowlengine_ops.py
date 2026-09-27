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
for name in ("autoupdate.py", "opsapi.py", "loganalyzer.py", "procman.py", "upload.py", "rangefeed.py",
             "minisign.py"):
    shutil.copy(PLUGIN / name, PKG / name)
sys.path.insert(0, str(HERE))

from fe import autoupdate as au  # noqa: E402
from fe import loganalyzer as la  # noqa: E402
from fe import minisign as ms  # noqa: E402
from fe import opsapi as oa  # noqa: E402
from fe import procman as pmmod  # noqa: E402

log = logging.getLogger("test")


def sha(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()


# ---- a throwaway minisign release key (pure-Python Ed25519 signing) --------------


def _compress(pt) -> bytes:
    zinv = pow(pt[2], ms._P - 2, ms._P)
    x, y = pt[0] * zinv % ms._P, pt[1] * zinv % ms._P
    return (y | ((x & 1) << 255)).to_bytes(32, "little")


def _ed_sign(seed: bytes, msg: bytes) -> tuple[bytes, bytes]:
    """(public key, signature) -- RFC 8032 signing, test-only."""
    h = hashlib.sha512(seed).digest()
    a = int.from_bytes(h[:32], "little")
    a &= (1 << 254) - 8
    a |= 1 << 254
    pub = _compress(ms._mul(a, ms._G))
    r = int.from_bytes(hashlib.sha512(h[32:] + msg).digest(), "little") % ms._L
    big_r = _compress(ms._mul(r, ms._G))
    k = int.from_bytes(hashlib.sha512(big_r + pub + msg).digest(), "little") % ms._L
    return pub, big_r + ((r + k * a) % ms._L).to_bytes(32, "little")


def make_key(seed: bytes = b"\x07" * 32, key_id: bytes = b"TESTKEY1"):
    import base64
    pub, _ = _ed_sign(seed, b"")
    text = ("untrusted comment: minisign public key: TEST\n"
            + base64.b64encode(b"Ed" + key_id + pub).decode() + "\n")

    def sign(data: bytes, tauri_wrap: bool = True, prehash: bool = True) -> str:
        msg = hashlib.blake2b(data, digest_size=64).digest() if prehash else data
        _, sig = _ed_sign(seed, msg)
        comment = "timestamp:0\tfile:manifest.json"
        _, gsig = _ed_sign(seed, sig + comment.encode())
        body = ("untrusted comment: signature from tauri secret key\n"
                + base64.b64encode((b"ED" if prehash else b"Ed") + key_id + sig).decode() + "\n"
                + f"trusted comment: {comment}\n" + base64.b64encode(gsig).decode() + "\n")
        return base64.b64encode(body.encode()).decode() if tauri_wrap else body
    return text, sign


TEST_PUB, release_sign = make_key()


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
        "autoupdate": {"enabled": True, "source": "folder", "folder": str(rel), "public_key": TEST_PUB,
                       "files": ["bflib.dll", "bfdb.exe"], "apply": "when_idle", "idle_minutes": 0},
        "issues": {"scan_seconds": 15},
    }
    cog = FakeCog(tmp_path, cfg, servers, node)
    cog.procman = pmmod.Procman(log, cfg, cog.notify_ops)
    return types.SimpleNamespace(tmp=tmp_path, cog=cog, servers=servers, home=home, exe=exe, rel=rel, cfg=cfg)


def publish(rel: Path, tag: str, files: dict, built: str, channel="stable", sign=release_sign):
    d = rel / tag
    d.mkdir(parents=True)
    man = {"schema": 1, "tag": tag, "git": tag[-6:], "built": built, "channel": channel,
           "files": {}}
    for name, data in files.items():
        (d / name).write_bytes(data)
        man["files"][name] = {"sha256": sha(data), "size": len(data), "git": tag[-6:]}
    raw = json.dumps(man).encode()
    (d / "manifest.json").write_bytes(raw)
    if sign:
        (d / "manifest.json.sig").write_text(sign(raw))
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
    pm._start_unlocked = fake_start
    pm._terminate = fake_term
    pm._kill_orphan_bfdb = lambda: None
    pm._orphan_bfdb_procs = lambda: []
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

    class _Exited:
        returncode = 3

        def poll(self):
            return 3
    pm._bfdb = _Exited()
    assert asyncio.run(pm._probation_tick(False, "pw"))
    assert called and "exited" in called[0]


def test_bfdb_probation_ignores_our_own_stop(world):
    # procman's own stop leaves _bfdb None; that is a restart in progress, not
    # the new build crashing -- it used to roll the live box back.
    pm = world.cog.procman
    pm.probation = {"tag": "engine-y", "swapped_at": time.time(), "healthy_at": None, "exits": 0,
                    "backup": None, "db_snapshot": None}
    called = []

    async def fake_rb(pw, why):
        called.append(why)
        return "rolled"
    pm.rollback_bfdb = fake_rb
    pm._bfdb = None
    assert not asyncio.run(pm._probation_tick(False, "pw"))
    assert not called and pm.probation is not None


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
                            {"name": "manifest.json.sig", "browser_download_url": f"{u}/dl/manifest.json.sig"},
                            {"name": "bflib.dll", "browser_download_url": f"{u}/dl/bflib.dll"}]}])

        raw = json.dumps(man).encode()

        async def dl(req):
            name = req.match_info["name"]
            body = {"manifest.json": raw, "manifest.json.sig": release_sign(raw).encode()}.get(name, dll)
            return web.Response(body=body)

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
    pm._orphan_bfdb_procs = lambda: []
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


# ---- release signing ----------------------------------------------------------------


MANAGER = BOT.parent / "bfmanager" / "src-tauri"


@pytest.mark.parametrize("pure", [True, False])
def test_minisign_verifies_a_real_tauri_signature(pure):
    """bfmanager's fixture was signed with the real key by `tauri signer sign`
    -- the same tool publish-release.ps1 signs engine manifests with."""
    if not pure:
        pytest.importorskip("cryptography")
    data = (MANAGER / "tests" / "fixture.bin").read_bytes()
    sig = (MANAGER / "tests" / "fixture.bin.sig").read_text()
    pub = (MANAGER / "updater.pub").read_text()
    assert "fixture.bin" in ms.verify(data, sig, pub, pure=pure)
    with pytest.raises(ms.SignatureError):
        ms.verify(data + b"x", sig, pub, pure=pure)
    with pytest.raises(ms.SignatureError):
        ms.verify(data, sig, TEST_PUB, pure=pure)   # wrong key


def test_minisign_formats_and_trusted_comment():
    raw = b'{"tag": "x"}'
    for wrap in (True, False):
        for prehash in (True, False):
            assert ms.verify(raw, release_sign(raw, wrap, prehash), TEST_PUB, pure=True)
    # the bare RW... line works as the key too
    bare = TEST_PUB.strip().splitlines()[-1]
    assert ms.verify(raw, release_sign(raw), bare)
    # a swapped trusted comment is caught by the global signature
    import base64
    body = base64.b64decode(release_sign(raw)).decode().replace("file:manifest.json", "file:evil")
    with pytest.raises(ms.SignatureError):
        ms.verify(raw, body, TEST_PUB, pure=True)


def test_unsigned_or_forged_release_is_refused(world):
    st1 = world.servers[0].instance.locals["extensions"]["BFBinaries"]["staging_dir"]
    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    # no signature at all
    publish(world.rel, "engine-u-000001", {"bflib.dll": b"evil"}, "2026-09-26T10:00:00Z", sign=None)
    res = asyncio.run(upd.check())
    assert not res["ok"] and "unsigned" in res["error"], res
    assert not Path(st1, "bflib.dll.pending").exists()
    # signed by somebody else's key
    shutil.rmtree(world.rel)
    _, other = make_key(seed=b"\x09" * 32)
    publish(world.rel, "engine-u-000002", {"bflib.dll": b"evil"}, "2026-09-26T11:00:00Z", sign=other)
    res = asyncio.run(upd.check())
    assert not res["ok"] and "signature check failed" in res["error"], res
    # a genuine signature over a manifest edited afterwards
    shutil.rmtree(world.rel)
    publish(world.rel, "engine-u-000003", {"bflib.dll": b"good"}, "2026-09-26T12:00:00Z")
    mpath = world.rel / "engine-u-000003" / "manifest.json"
    doc = json.loads(mpath.read_text())
    doc["files"]["bflib.dll"]["sha256"] = sha(b"evil")
    mpath.write_text(json.dumps(doc))
    (world.rel / "engine-u-000003" / "bflib.dll").write_bytes(b"evil")
    res = asyncio.run(upd.check())
    assert not res["ok"] and "signature check failed" in res["error"], res
    assert not Path(st1, "bflib.dll.pending").exists()
    # and no pinned key: nothing is even looked at
    world.cfg["autoupdate"]["public_key"] = ""
    upd.reload_config()
    res = asyncio.run(upd.check())
    assert not res["ok"] and "public_key is not set" in res["error"]


def test_bot_plugin_is_not_a_runtime_key(world):
    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    with pytest.raises(ValueError):
        upd.set_overrides({"bot_plugin": True})
    # a stale override from an older version is ignored
    assert au.UpdateConfig.from_dicts({}, {"bot_plugin": True}).bot_plugin is False


def test_remote_node_staging_verifies_the_download(world):
    class RemoteNode:
        name = "RANGE"
        is_remote = True

        def __init__(self, payload):
            self.files, self.payload = {}, payload

        async def create_directory(self, path):
            pass

        async def write_file(self, path, url, overwrite=False):
            self.files[path] = self.payload
            return UploadStatus.OK

        async def read_file(self, path):
            return self.files[path]

        async def remove_file(self, path):
            self.files.pop(path, None)

        async def rename_file(self, old, new, force=False):
            self.files[new] = self.files.pop(old)

    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    s = world.servers[0]
    good = b"engine"
    latest = {"tag": "engine-r-1", "urls": {"bflib.dll": "http://x/bflib.dll"}}
    meta = {"sha256": sha(good)}
    for payload, ok in ((b"tampered", False), (good, True)):
        node = RemoteNode(payload)
        s.node = node
        t = au.DllTarget(server=s, name=s.name, dll_name="bflib.dll", dll_path="x", staging_dir="C:/st",
                         remote=True, home=None)
        upd.state.pop("remote_staged", None)
        assert asyncio.run(upd._stage_remote(t, "bflib.dll", meta, latest, [s.name])) is ok
        pend = os.path.join("C:/st", "bflib.dll.pending")
        assert (pend in node.files) is ok
        assert not any(k.endswith(".part") for k in node.files)


# ---- probation vs a scheduled restart ----------------------------------------------


def test_scheduled_restart_is_not_a_crash(world):
    upd = au.Updater(world.cog, log, str(world.tmp / "config" / "upd.json"))
    s1 = world.servers[0]
    upd.state["probation"] = {s1.name: {"server": s1.name, "dll": "bflib.dll", "live": "x", "backup": None,
                                        "tag": "engine-ok", "swapped_at": time.time(), "sidecar": None,
                                        "running_secs": 0.0, "loaded_at": None, "crashes": 0}}
    rolled = []

    async def fake_rollback(server, why):
        rolled.append(why)
        return "rolled"
    upd.rollback_dll = fake_rollback
    # RUNNING -> SHUTTING_DOWN for a second or two -> SHUTDOWN, all between two 30 s ticks
    for st in (Status.RUNNING, Status.SHUTTING_DOWN, Status.SHUTDOWN):
        s1.status = st
        upd.sample_status()
    upd._last_status = {s1.name: "RUNNING"}
    asyncio.run(upd._probation_phase(30))
    assert rolled == []
    # a real crash: straight from RUNNING to SHUTDOWN
    upd._orderly.clear()
    for st in (Status.RUNNING, Status.SHUTDOWN):
        s1.status = st
        upd.sample_status()
    asyncio.run(upd._probation_phase(30))
    assert rolled and "crashed" in rolled[0]


# ---- rollback never restores the bad build -------------------------------------------


def test_newest_backup_skips_failed_and_identical(tmp_path):
    live = tmp_path / "bflib.dll"
    live.write_bytes(b"bad")
    (tmp_path / "bflib.dll.backup-20260101-000000").write_bytes(b"good")
    (tmp_path / "bflib.dll.backup-20260102-000000").write_bytes(b"bad")     # == live
    (tmp_path / "bflib.dll.failed-20260103-000000").write_bytes(b"worse")
    assert au.Updater._newest_backup(str(live)).endswith("backup-20260101-000000")


# ---- ops API: protected keys, secrets next to a changed URL --------------------------


def test_protected_changes():
    old = {"DEFAULT": {"bfdb": {"exe": "a.exe", "health_failures": 3, "netidx_resolver_cmd": ["netidx"],
                                "instances": [{"id": "vs1", "stats_jsonl": "a"}]},
                       "autoupdate": {"source": "github", "channel": "stable"},
                       "status_channel": 1}}
    import copy
    new = copy.deepcopy(old)
    new["DEFAULT"]["bfdb"]["health_failures"] = 5
    new["DEFAULT"]["status_channel"] = 2
    new["DEFAULT"]["autoupdate"]["channel"] = "beta"
    assert oa.protected_changes(new, old) == []
    new["DEFAULT"]["bfdb"]["netidx_resolver_cmd"] = ["powershell", "-c", "calc"]
    new["DEFAULT"]["autoupdate"]["folder"] = "\\\\evil\\share"
    new["DEFAULT"]["bfdb"]["instances"][0]["stats_jsonl"] = "b"
    new["DEFAULT"]["bfdb"]["instances"].append({"id": "vs2", "engine_config": "c"})
    new["DEFAULT"]["gci"] = {"piper_exe": "x.exe"}
    got = set(oa.protected_changes(new, old))
    assert got == {"DEFAULT.bfdb.netidx_resolver_cmd", "DEFAULT.autoupdate.folder",
                   "DEFAULT.bfdb.instances[0].stats_jsonl", "DEFAULT.bfdb.instances[1].engine_config",
                   "DEFAULT.gci.piper_exe"}, got
    # removing one counts too
    del new["DEFAULT"]["bfdb"]["exe"]
    assert "DEFAULT.bfdb.exe" in oa.protected_changes(new, old)


def test_masked_secret_not_carried_to_a_new_url():
    old = {"bfdb": {"news_llm_url": "https://api.openai.com/v1", "news_llm_key": "sk-real"}}
    new = {"bfdb": {"news_llm_url": "https://api.openai.com/v1", "news_llm_key": oa.SECRET_MASK}}
    oa.unmask_secrets(new, old)
    assert new["bfdb"]["news_llm_key"] == "sk-real"
    new = {"bfdb": {"news_llm_url": "https://evil.example", "news_llm_key": oa.SECRET_MASK}}
    with pytest.raises(ValueError):
        oa.unmask_secrets(new, old)


def test_config_post_refuses_protected_edit(world):
    from fastapi import FastAPI
    from fastapi.testclient import TestClient

    cfgfile = Path(world.cog.node.config_dir, "plugins", "fowlengine.yaml")
    cfgfile.parent.mkdir(parents=True, exist_ok=True)
    cfgfile.write_text("DEFAULT:\n  ops_api:\n    api_key: OPSKEY\n  bfdb:\n    manage: true\n    exe: x.exe\n"
                       "    home: h\n    dcsserverbot_api_key: RESTKEY\n")
    world.cfg["ops_api"] = {"api_key": "OPSKEY"}
    api = oa.OpsApi(world.cog)
    app = FastAPI()
    api.mount(app)
    c = TestClient(app)
    base = "/stats/fowlengine/ops"
    # the RestAPI key is no longer enough once ops_api.api_key is set
    assert c.get(f"{base}/config", headers={"X-API-Key": "RESTKEY"}).status_code == 403
    H = {"X-API-Key": "OPSKEY"}
    g = c.get(f"{base}/config", headers=H).json()
    evil = g["yaml"].replace("exe: x.exe", "exe: x.exe\n    netidx_resolver_cmd: [powershell, -c, calc]")
    r = c.post(f"{base}/config", headers=H, json={"yaml": evil, "base_mtime": g["mtime"]})
    assert r.status_code == 403 and "netidx_resolver_cmd" in r.json()["error"]
    assert "powershell" not in cfgfile.read_text()


# ---- procman: redaction, restart folding, loopback defaults ---------------------------


def test_redact_hides_every_secret_flag(world):
    pm = world.cog.procman
    line = pm._redact(["--db", "x", "--admin-password", "p1", "--news-llm-key", "sk-1", "--discord-client-secret",
                       "s", "--log-read-token", "t", "--some-api-key=k2", "--listen-address", "127.0.0.1:8880"])
    words = line.split()
    for secret in ("p1", "sk-1", "s", "t", "--some-api-key=k2"):
        assert secret not in words, line
    assert "--some-api-key=***" in words and "127.0.0.1:8880" in words
    # the LLM key goes through the environment, not the command line
    world.cfg["bfdb"]["news_llm_key"] = "sk-env"
    pm.reload_config(world.cfg)
    assert "sk-env" not in " ".join(pm._build_args("pw", False))
    assert pm._bfdb_env()["BFDB_NEWS_LLM_KEY"] == "sk-env"
    assert pm.api_url == "http://127.0.0.1:8880"
    assert "127.0.0.1:8880" in pm._build_args("pw", False)


def test_concurrent_restarts_are_serialised_and_folded(world):
    pm = world.cog.procman
    events = []

    async def fake_stop(**kw):
        events.append("stop")
        await asyncio.sleep(0.01)

    async def fake_start(pw):
        events.append("start")
        await asyncio.sleep(0.01)
    pm._stop_unlocked = fake_stop
    pm._start_unlocked = fake_start

    async def main():
        await asyncio.gather(*(pm.restart("pw") for _ in range(5)))
    asyncio.run(main())
    # never interleaved, and the 3 requests that queued behind a running one became one
    assert events == ["stop", "start", "stop", "start"], events


def test_archive_source_cannot_escape():
    for bad in ("..", ".", "...", "../..", "..\\x"):
        assert ".." != la.LogAnalyzer._safe(bad) and la.LogAnalyzer._safe(bad) not in (".", "..")
    assert la.LogAnalyzer._safe("engine_vs1") == "engine_vs1"


# ---- coalition roles: servers sharing one role pair are synced as one -------------


def _apply_roles_fn():
    import ast
    src = (PLUGIN / "commands.py").read_text(encoding="utf-8")
    fn = next(n for n in ast.walk(ast.parse(src))
              if isinstance(n, ast.AsyncFunctionDef) and n.name == "_apply_coalition_roles")
    fake = types.SimpleNamespace(Forbidden=type("Forbidden", (Exception,), {}))
    ns = {"discord": fake, "REVOKE_MIN_PER_TICK": 10, "REVOKE_MAX_FRACTION": 0.25}
    exec(compile(ast.Module(body=[fn], type_ignores=[]), "commands.py", "exec"), ns)
    return ns["_apply_coalition_roles"]


class _Role:
    def __init__(self, rid):
        self.id, self.name, self.members = rid, f"role{rid}", []


class _Member:
    def __init__(self, mid, *roles):
        self.id, self.roles = mid, []
        for r in roles:
            self._add(r)

    def _add(self, r):
        self.roles.append(r)
        r.members.append(self)

    async def add_roles(self, r, reason=None):
        self._add(r)

    async def remove_roles(self, r, reason=None):
        self.roles.remove(r)
        r.members.remove(self)


def test_shared_role_pair_keeps_pilots_backed_by_either_server():
    apply = _apply_roles_fn()
    blue, red = _Role(1), _Role(2)
    modern_only = _Member(10, blue)      # Blue on Modern, never flew 2008
    both = _Member(11, blue)             # Blue on Modern, Red on 2008
    stray = _Member(12, red)             # registered nowhere
    members = {"u10": modern_only, "u11": both}

    class _Bot:
        async def get_member_by_ucid(self, ucid):
            return members.get(ucid)
    cog = types.SimpleNamespace(bot=_Bot(), log=log)
    g = {"roles": {"Blue": blue, "Red": red}, "cr": {}, "servers": ["vs1", "vs2"],
         "complete": True,
         # merged: vs1 says u10/u11 Blue, vs2 says u11 Red
         "sides": {"u10": {"Blue"}, "u11": {"Blue", "Red"}}}
    asyncio.run(apply(cog, g))
    assert blue in modern_only.roles           # not stripped by the 2008 roster
    assert blue in both.roles and red in both.roles
    assert red not in stray.roles              # unbacked role still revoked


def test_incomplete_roster_never_revokes():
    apply = _apply_roles_fn()
    blue, red = _Role(1), _Role(2)
    holder = _Member(10, blue)

    class _Bot:
        async def get_member_by_ucid(self, ucid):
            return None
    cog = types.SimpleNamespace(bot=_Bot(), log=log)
    g = {"roles": {"Blue": blue, "Red": red}, "cr": {}, "servers": ["vs1", "vs2"],
         "complete": False, "sides": {"someone": {"Red"}}}
    asyncio.run(apply(cog, g))
    assert blue in holder.roles


# ---- persistent Discord embeds are edited in place, never re-posted per tick -------

def _upsert_embed_fn():
    """commands.py's _upsert_embed on its own (the module itself needs discord
    and DCSServerBot), with stand-in discord exceptions."""
    import ast
    src = (PLUGIN / "commands.py").read_text(encoding="utf-8")
    fn = next(n for n in ast.walk(ast.parse(src))
              if isinstance(n, ast.AsyncFunctionDef) and n.name == "_upsert_embed")
    fn.decorator_list = []
    fake = types.SimpleNamespace(
        HTTPException=type("HTTPException", (Exception,), {}),
        Embed=object,
    )
    fake.NotFound = type("NotFound", (fake.HTTPException,), {})
    fake.Forbidden = type("Forbidden", (fake.HTTPException,), {})
    ns = {"discord": fake}
    exec(compile(ast.Module(body=[fn], type_ignores=[]), "commands.py", "exec"), ns)
    return ns["_upsert_embed"], fake


class _Chan:
    def __init__(self, edit_error=None):
        self.id = 42
        self.edit_error = edit_error
        self.sent, self.edited = [], []

    def get_partial_message(self, mid):
        chan = self

        class _P:
            async def edit(self, embed):
                if chan.edit_error:
                    raise chan.edit_error
                chan.edited.append((mid, embed))
        return _P()

    async def fetch_message(self, mid):  # must never be needed
        raise AssertionError("fetch_message needs Read Message History")

    async def send(self, embed):
        self.sent.append(embed)
        return types.SimpleNamespace(id=1000 + len(self.sent))


def _cog():
    warned = []
    return types.SimpleNamespace(log=log, save_state=lambda: None,
                                 _warn_once=lambda *a: warned.append(a), warned=warned)


def test_embed_is_edited_in_place_without_read_history():
    upsert, fake = _upsert_embed_fn()
    cog, ids = _cog(), {}
    ch = _Chan()
    asyncio.run(upsert(cog, ids, "vs1", ch, "e1"))          # first tick posts
    asyncio.run(upsert(cog, ids, "vs1", ch, "e2"))          # later ticks edit
    asyncio.run(upsert(cog, ids, "vs1", ch, "e3"))
    assert ch.sent == ["e1"] and [e for _, e in ch.edited] == ["e2", "e3"] and ids == {"vs1": 1001}


def test_permission_or_api_errors_never_post_a_new_embed():
    upsert, fake = _upsert_embed_fn()
    for err in (fake.Forbidden(), fake.HTTPException()):
        cog, ids, ch = _cog(), {"vs1": 7}, _Chan(edit_error=err)
        for _ in range(3):
            asyncio.run(upsert(cog, ids, "vs1", ch, "e"))
        assert ch.sent == [] and ids == {"vs1": 7}


def test_deleted_embed_is_replaced_once_and_legacy_id_rehomed():
    upsert, fake = _upsert_embed_fn()
    cog, ch = _cog(), _Chan(edit_error=fake.NotFound())
    ids = {"vs1": 7}
    asyncio.run(upsert(cog, ids, "vs1", ch, "e"))
    assert ch.sent == ["e"] and ids == {"vs1": 1001}
    ids, ch = {"__legacy__": 9}, _Chan()
    asyncio.run(upsert(cog, ids, "vs2", ch, "e"))
    assert ids == {"vs2": 9} and ch.sent == [] and ch.edited == [(9, "e")]
