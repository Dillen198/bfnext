"""
Offline tests for the rest of the bot console: the bot's config files
(botconfig.py and the console/config routes), the Access tab's role overrides,
music upload, player screenshots, a server's overview and the live stream.
"""
import asyncio
import json
import os
import types

import pytest

from test_fowlengine_ops import BASE, Status, world  # noqa: F401 -- `world` is a fixture
from test_fowlengine_console import FakeLayer, FakeResult, KEY, _console_client
from fe import botconfig as bc
from fe import console as co

ADMIN = {**KEY, "X-Fowl-Roles": "admin", "X-Fowl-Actor": "Fowl", "X-Fowl-Subject": "42"}


# ---- the bot's config files ---------------------------------------------------------


@pytest.fixture
def cfgdir(tmp_path):
    d = tmp_path / "config"
    (d / "plugins").mkdir(parents=True)
    (d / "services").mkdir()
    (d / "main.yaml").write_text("# the bot\nguild_id: 123\ntoken: SECRETTOKEN  # keep\nautoupdate: true\n")
    (d / "nodes.yaml").write_text("NODE:\n  DCS:\n    installation: C:/DCS\n  instances:\n    vs1:\n      bot_port: 6666\n")
    (d / "presets.yaml").write_text("Sunny:\n  clouds: Preset1\n  temperature: 20\nNight:\n  start_time: '22:00'\n")
    (d / "plugins" / "scheduler.yaml").write_text("DEFAULT:\n  warn:\n    times: [300, 60]\n")
    (d / "plugins" / "fowlengine.yaml").write_text("DEFAULT: {}\n")
    (d / "services" / "bot.yaml").write_text("DEFAULT: {}\n")
    (d / "services" / "music.yaml.disabled").write_text("DEFAULT: {}\n")
    return d


def test_botconfig_lists_what_it_may_edit(cfgdir):
    cfg = bc.BotConfig(str(cfgdir), bot_dir=str(cfgdir.parent))
    files = {f["path"]: f for f in cfg.files()}
    assert set(files) == {"main.yaml", "nodes.yaml", "presets.yaml", "plugins/scheduler.yaml",
                          "services/bot.yaml", "services/music.yaml.disabled"}
    assert "plugins/fowlengine.yaml" not in files                    # the OPS page's
    assert files["plugins/scheduler.yaml"]["kind"] == "plugin" and files["plugins/scheduler.yaml"]["applies"] == "reload"
    assert files["presets.yaml"]["applies"] == "launch" and files["main.yaml"]["applies"] == "restart"
    assert files["services/music.yaml.disabled"]["disabled"] is True
    for bad in ("../secrets.yaml", "plugins/fowlengine.yaml", "C:/Windows/win.ini", ""):
        with pytest.raises(bc.ConfigError) as e:
            cfg.read(bad)
        assert e.value.status == 404


def test_botconfig_masks_and_saves(cfgdir):
    cfg = bc.BotConfig(str(cfgdir), bot_dir=str(cfgdir.parent))
    doc = cfg.read("main.yaml")
    assert "SECRETTOKEN" not in doc["yaml"] and doc["doc"]["token"] == bc.SECRET_MASK and doc["masked"] == 1
    assert "# the bot" in doc["yaml"]                                # comments survive
    # a stale revision is refused
    with pytest.raises(bc.ConfigError) as e:
        cfg.save("main.yaml", {"revision": "nope", "yaml": doc["yaml"]})
    assert e.value.status == 409
    # the masked secret goes back as it was
    out = cfg.save("main.yaml", {"revision": doc["revision"], "yaml": doc["yaml"].replace("autoupdate: true", "autoupdate: false")})
    assert out["changed"] and out["backup"].startswith(".fowlengine-backups/main.yaml.")
    text = (cfgdir / "main.yaml").read_text()
    assert "token: SECRETTOKEN" in text and "autoupdate: false" in text and "# keep" in text
    assert (cfgdir / out["backup"]).read_text().count("autoupdate: true") == 1
    # unchanged = no write
    again = cfg.read("main.yaml")
    assert cfg.save("main.yaml", {"revision": again["revision"], "yaml": again["yaml"]})["changed"] is False
    # bad YAML
    with pytest.raises(bc.ConfigError) as e:
        cfg.save("main.yaml", {"revision": again["revision"], "yaml": "a: [1,"})
    assert e.value.status == 400


def test_botconfig_refuses_programs_paths_and_hosts(cfgdir):
    cfg = bc.BotConfig(str(cfgdir), bot_dir=str(cfgdir.parent))
    doc = cfg.read("nodes.yaml")
    moved = doc["yaml"].replace("C:/DCS", "C:/Evil")
    with pytest.raises(bc.ConfigError) as e:
        cfg.save("nodes.yaml", {"revision": doc["revision"], "yaml": moved})
    assert e.value.status == 403 and "NODE.DCS.installation" in e.value.message
    added = doc["yaml"] + "  extensions:\n    SRS:\n      cmd: calc.exe\n"
    with pytest.raises(bc.ConfigError) as e:
        cfg.save("nodes.yaml", {"revision": doc["revision"], "yaml": added})
    assert e.value.status == 403 and "cmd" in e.value.message
    # the rest of the file is fine
    ok = cfg.save("nodes.yaml", {"revision": doc["revision"], "yaml": doc["yaml"].replace("6666", "6667")})
    assert ok["changed"]
    assert bc.is_bot_protected_key("installation") and bc.is_bot_protected_key("missions_dir")
    assert not bc.is_bot_protected_key("bot_port")


def test_botconfig_sets_and_drops_top_level_keys(cfgdir):
    cfg = bc.BotConfig(str(cfgdir), bot_dir=str(cfgdir.parent))
    doc = cfg.read("presets.yaml")
    out = cfg.save("presets.yaml", {"revision": doc["revision"],
                                    "set": {"Dusk": {"start_time": "19:00", "temperature": 12}},
                                    "delete": ["Night"]})
    assert out["changed"]
    after = cfg.read("presets.yaml")["doc"]
    assert after == {"Sunny": {"clouds": "Preset1", "temperature": 20}, "Dusk": {"start_time": "19:00", "temperature": 12}}
    with pytest.raises(bc.ConfigError):
        cfg.save("presets.yaml", {"revision": cfg.read("presets.yaml")["revision"], "set": [1]})


def test_botconfig_switches_services(cfgdir):
    cfg = bc.BotConfig(str(cfgdir), bot_dir=str(cfgdir.parent))
    assert cfg.set_service_enabled("music", True)["changed"]
    assert (cfgdir / "services" / "music.yaml").exists()
    assert cfg.set_service_enabled("music", True)["changed"] is False
    assert cfg.set_service_enabled("music", False)["changed"]
    assert (cfgdir / "services" / "music.yaml.disabled").exists()
    with pytest.raises(bc.ConfigError):
        cfg.set_service_enabled("../x", True)


def test_console_config_routes_are_for_full_admins(world, cfgdir):  # noqa: F811
    world.cog.node.config_dir = str(cfgdir)
    api, layer, c = _console_client(world)
    api.console._config = bc.BotConfig(str(cfgdir), bot_dir=str(cfgdir.parent))
    reloaded = []
    world.cog.bot.reload_plugin = lambda name: reloaded.append(name)
    audits = []
    world.cog.bot.audit = lambda event: audits.append(event)
    for h in ({**KEY, "X-Fowl-Roles": "ops,moderation"}, {**KEY, "X-Fowl-Roles": "ops"}):
        assert c.get(f"{BASE}/console/config/files", headers=h).status_code == 403
    files = c.get(f"{BASE}/console/config/files", headers=ADMIN).json()["files"]
    assert {f["path"] for f in files} >= {"main.yaml", "plugins/scheduler.yaml"}
    doc = c.get(f"{BASE}/console/config/file?path=plugins/scheduler.yaml", headers=ADMIN).json()
    r = c.post(f"{BASE}/console/config/validate", headers=ADMIN,
               json={"path": "plugins/scheduler.yaml", "revision": doc["revision"], "yaml": doc["yaml"]})
    assert r.status_code == 200 and r.json() == {"ok": True, "errors": []}
    r = c.post(f"{BASE}/console/config/file", headers=ADMIN,
               json={"path": "plugins/scheduler.yaml", "revision": doc["revision"],
                     "yaml": doc["yaml"].replace("300", "600")})
    assert r.status_code == 200, r.text
    assert r.json()["changed"] and r.json()["reloaded"] is True and reloaded == ["scheduler"]
    assert audits == ["[web:fowlengine/42] changed config plugins/scheduler.yaml"]
    r = c.post(f"{BASE}/console/config/file", headers=ADMIN, json={"path": "../x", "revision": "", "yaml": ""})
    assert r.status_code == 404
    r = c.post(f"{BASE}/console/config/service", headers=ADMIN, json={"name": "music", "enabled": True})
    assert r.status_code == 200 and r.json()["changed"]


# ---- the Access tab ----------------------------------------------------------------


def test_console_access_overrides(world, tmp_path):  # noqa: F811
    world.cog.node.config_dir = str(tmp_path)
    api, layer, c = _console_client(world)
    MOD = {**KEY, "X-Fowl-Roles": "moderation"}
    params = {"params": {"server_name": "vs1", "ucid": "u"}}
    assert c.post(f"{BASE}/console/act/kick_player", headers=MOD, json=params).status_code == 200
    # only a full admin changes it; locked actions stay admin
    assert c.post(f"{BASE}/console/access", headers={**KEY, "X-Fowl-Roles": "ops"},
                  json={"action": "kick_player", "role": "ops"}).status_code == 403
    assert c.post(f"{BASE}/console/access", headers=ADMIN, json={"action": "restart_node", "role": "ops"}).status_code == 403
    assert c.post(f"{BASE}/console/access", headers=ADMIN, json={"action": "kick_player", "role": "root"}).status_code == 400
    r = c.post(f"{BASE}/console/access", headers=ADMIN, json={"action": "kick_player", "role": "ops"})
    assert r.status_code == 200 and r.json()["role"] == "ops"
    assert json.loads((tmp_path / co.ACCESS_FILE).read_text()) == {"kick_player": "ops"}
    # now a moderator may not, and the catalog says so
    assert c.post(f"{BASE}/console/act/kick_player", headers=MOD, json=params).status_code == 403
    cat = {a["name"]: a for a in c.get(f"{BASE}/console/catalog", headers=MOD).json()["actions"]}
    assert cat["kick_player"]["role"] == "ops" and cat["kick_player"]["allowed"] is False
    acc = c.get(f"{BASE}/console/access", headers=MOD).json()
    row = next(a for a in acc["actions"] if a["name"] == "kick_player")
    assert row == {"name": "kick_player", "kind": "write", "default": "moderation", "role": "ops",
                   "locked": False, "plugin": "mission"} and acc["editable"] is False
    # back to the default
    c.post(f"{BASE}/console/access", headers=ADMIN, json={"action": "kick_player", "role": None})
    assert json.loads((tmp_path / co.ACCESS_FILE).read_text()) == {}
    # a hand-edited file can't unlock a locked action
    (tmp_path / co.ACCESS_FILE).write_text(json.dumps({"restart_node": "moderation", "nope": "ops", "kick_player": "x"}))
    assert api.console.overrides() == {}


# ---- music, screenshots, the server overview ----------------------------------------


def test_console_music_upload(world):  # noqa: F811
    api, layer, c = _console_client(world)

    async def upload_music_track(ctx, *, folder="", name="", node_name="", source=b""):
        return FakeResult(True, f"uploaded {name} to {folder or 'the library'} ({len(source)} bytes)")
    layer.registry["upload_music_track"] = upload_music_track
    r = c.post(f"{BASE}/console/music?name=song.mp3&folder=rock", headers={**KEY, "X-Fowl-Roles": "ops"}, content=b"ID3xx")
    assert r.status_code == 200 and r.json()["message"] == "uploaded song.mp3 to rock (5 bytes)"
    assert c.post(f"{BASE}/console/music?name=s.mp3", headers={**KEY, "X-Fowl-Roles": "moderation"},
                  content=b"x").status_code == 403


def test_console_screenshot(world):  # noqa: F811
    api, layer, c = _console_client(world)
    asked = []

    async def read_file(url):
        asked.append(url)
        return b"\x89PNG..."
    world.servers[0].instance.webgui_port = 8088
    world.cog.node.read_file = read_file
    H = {**KEY, "X-Fowl-Roles": "moderation"}
    try:
        r = c.get(f"{BASE}/console/screenshot?server=vs1&key=/screenshots/abc_1.png", headers=H)
        assert r.status_code == 200 and r.content == b"\x89PNG..." and r.headers["content-type"] == "image/png"
        assert asked == ["http://127.0.0.1:8088/screenshots/abc_1.png"]
        for key in ("/etc/passwd", "/screenshots/../x.png", "http://evil/x.png", "/screenshots/a.txt"):
            assert c.get(f"{BASE}/console/screenshot", params={"server": "vs1", "key": key}, headers=H).status_code == 400
        assert c.get(f"{BASE}/console/screenshot?server=nope&key=/screenshots/a.png", headers=H).status_code == 404
        assert c.get(f"{BASE}/console/screenshot?server=vs1&key=/screenshots/a.png",
                     headers={**KEY, "X-Fowl-Roles": "content"}).status_code == 403
    finally:
        del world.cog.node.read_file


def test_console_server_overview(world):  # noqa: F811
    api, layer, c = _console_client(world)
    sv = world.servers[0]
    sv.current_mission = types.SimpleNamespace(weather={"season": {"temperature": 21}}, clouds={"preset": "Preset3"})

    async def send_to_dcs_sync(msg, timeout=10):
        assert msg == {"command": "getFog"}
        return {"visibility": 8000, "thickness": 100}

    async def render_extensions():
        return [{"name": "SRS", "value": "127.0.0.1:5002"}, "junk"]
    sv.send_to_dcs_sync = send_to_dcs_sync
    sv.render_extensions = render_extensions
    world.cog.bot.cogs = {"Scheduler": types.SimpleNamespace(
        get_config=lambda s: {"schedule": {"00-24": "YYYYYYY"}, "restart": {"method": "restart"}})}
    r = c.get(f"{BASE}/console/server?name=vs1", headers={**KEY, "X-Fowl-Roles": "ops"})
    assert r.status_code == 200, r.text
    doc = r.json()
    assert doc["visibility"] == 8000 and doc["clouds"] == {"preset": "Preset3"}
    assert doc["extensions"] == [{"name": "SRS", "value": "127.0.0.1:5002"}]
    assert doc["schedule"] == {"00-24": "YYYYYYY"} and doc["restart"] == {"method": "restart"}
    # a stopped server: no live fog asked for
    sv.status = Status.SHUTDOWN
    assert c.get(f"{BASE}/console/server?name=vs1", headers=ADMIN).json()["visibility"] is None
    assert c.get(f"{BASE}/console/server?name=x", headers=ADMIN).status_code == 404


# ---- the live stream ----------------------------------------------------------------


def test_console_stream_sends_changes_and_pings(world):  # noqa: F811
    api, layer, c = _console_client(world)

    class Req:
        n = 0

        async def is_disconnected(self):
            Req.n += 1
            return Req.n > 4

    async def collect():
        out = []
        async for chunk in api.console.stream_events(Req(), tick=0, ping=0):
            out.append(chunk)
            if len(out) == 1:
                world.servers[0].status = Status.PAUSED        # a change: a second fleet event
        return out

    chunks = asyncio.run(collect())
    fleets = [ch for ch in chunks if ch.startswith("event: fleet\n")]
    assert len(fleets) == 2 and ": ping\n\n" in chunks
    doc = json.loads(fleets[1].split("data: ", 1)[1])
    assert next(s for s in doc["servers"] if s["name"] == "vs1")["status"] == "PAUSED"
    assert api.console._streams == 0                            # the count comes back down
    # roles, and the cap
    assert c.get(f"{BASE}/console/stream", headers={**KEY, "X-Fowl-Roles": "content"}).status_code == 403
    api.console._streams = co.STREAM_MAX
    assert c.get(f"{BASE}/console/stream", headers=KEY).status_code == 503
    api.console._streams = 0
