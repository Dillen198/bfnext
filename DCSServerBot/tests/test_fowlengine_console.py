"""
Offline tests for the bot console (plugins/fowlengine/console.py): the
dashboard's door to DCSServerBot's own actions. A fake of core/actions.py
stands in for the bot; the OPS fixtures (test_fowlengine_ops.py) supply the
servers, cog and FastAPI app.
"""
import json
import types
from dataclasses import dataclass, field
from datetime import datetime

from test_fowlengine_ops import BASE, Status, _ops_client, world  # noqa: F401 -- `world` is a fixture
from fe import console as co


@dataclass
class FakeResult:
    success: bool
    message: str = ""
    data: dict = field(default_factory=dict)


@dataclass
class FakePlayerResult(FakeResult):
    player_name: str = ""
    ucid: str = ""


@dataclass
class FakeDownload(FakeResult):
    filename: str = ""
    content: bytes = b""


class FakeActionContext:
    @classmethod
    def from_web(cls, identity, node, bus):
        ctx = cls()
        ctx.identity = identity
        ctx.bus = bus
        ctx.audit_actor = f"[web:{identity.backend}/{identity.subject}]"
        return ctx


class FakeLayer:
    """core.actions as ConsoleApi uses it: a registry, call/read, from_web."""
    ActionContext = FakeActionContext

    def __init__(self):
        self.calls = []

        async def kick_player(ctx, server_name: str, ucid: str, reason: str = ""):
            return FakePlayerResult(True, f"kicked {ucid}",
                                    {"audit": "recorded", "when": datetime(2026, 11, 5, 12, 0)},
                                    player_name="Bob", ucid=ucid)

        async def list_bans(ctx):
            return FakeResult(True, "", {"bans": [{"ucid": "u1", "reason": "tk"}]})

        async def restart_server(ctx, server_name: str):
            return FakeResult(True, "restarting")

        async def download_mission(ctx, server_name: str, mission):
            return FakeDownload(True, "", filename="Op Fowl.miz", content=b"PK\x03\x04miz")

        async def upload_mission(ctx, server_name: str, name: str, source, *, force=False, autostart=False,
                                 load=False):
            return FakeResult(True, f"uploaded {name} ({len(source)} bytes)", {"load": load})

        async def restart_node(ctx, node_name: str):
            return FakeResult(True, "restarting node")

        self.registry = {f.__name__: f for f in (kick_player, list_bans, restart_server, download_mission,
                                                 upload_mission, restart_node)}

    def action_registry(self):
        return dict(self.registry)

    def action_available(self, name):
        return name in self.registry

    async def call_action(self, name, ctx, /, **params):
        self.calls.append(("call", name, ctx, params))
        return await self.registry[name](ctx, **params)

    async def read_action(self, name, ctx, /, **params):
        self.calls.append(("read", name, ctx, params))
        return await self.registry[name](ctx, **params)


def _console_client(world):  # noqa: F811
    api, c = _ops_client(world)
    layer = FakeLayer()
    api.console._layer = layer
    api.console._discovered = True
    return api, layer, c


KEY = {"X-API-Key": "KEY123"}


def test_console_roles():
    assert co.caller_roles(None) == {"admin"}
    assert co.caller_roles("") == frozenset()
    assert co.caller_roles("ops, Moderation,bogus") == {"ops", "moderation"}
    assert co.may_use(frozenset({"admin"}), "admin")
    assert not co.may_use(frozenset({"ops", "moderation"}), "admin")
    assert co.may_use(frozenset({"moderation"}), "moderation")
    assert not co.may_use(frozenset({"moderation"}), "ops")
    assert not co.may_use(frozenset({"content"}), "moderation")
    assert co.bot_roles(frozenset({"admin"})) == {"Admin"}
    assert co.bot_roles(frozenset({"ops"})) == {"DCS Admin"}


def test_console_every_action_has_a_known_role_and_kind():
    for name, (kind, role) in co.ACTIONS.items():
        assert kind in ("read", "write"), name
        assert role in ("ops", "moderation", "admin"), name
        # reads are the ones upstream names as reads
        assert (kind == "read") == name.startswith(("list_", "get_", "read_", "search_")), name


def test_console_jsonable():
    @dataclass
    class D:
        a: int
        b: bytes

    out = co.jsonable({"t": datetime(2026, 1, 2, 3, 4), "s": {1}, "e": Status.RUNNING, "d": D(1, b"xy"),
                       "f": float("nan")})
    assert out == {"t": "2026-01-02T03:04:00", "s": [1], "e": "RUNNING", "d": {"a": 1, "b": {"bytes": 2}},
                   "f": None}
    json.dumps(out)


def test_console_catalog(world):  # noqa: F811
    api, layer, c = _console_client(world)
    r = c.get(f"{BASE}/console/catalog", headers={**KEY, "X-Fowl-Roles": "moderation"})
    assert r.status_code == 200, r.text
    doc = r.json()
    assert doc["available"] is True and doc["roles"] == ["moderation"]
    by = {a["name"]: a for a in doc["actions"]}
    assert set(by) == set(co.ACTIONS)
    kick = by["kick_player"]
    assert kick["available"] and kick["allowed"] and kick["kind"] == "write" and kick["plugin"] == "mission"
    assert [p["name"] for p in kick["params"]] == ["server_name", "ucid", "reason"]
    assert [p["required"] for p in kick["params"]] == [True, True, False]
    assert by["restart_server"]["allowed"] is False                  # needs ops
    assert by["restart_node"]["allowed"] is False                    # needs admin
    assert by["install_mod"]["available"] is False                   # not in this registry
    assert by["install_mod"]["plugin"] == "modmanager"
    assert by["change_mod_version"]["plugin"] == "modmanager"
    assert by["punish_player"]["plugin"] == "punishment"
    assert by["control_radio"]["plugin"] == "music"
    assert by["get_load_statistics"]["plugin"] == "monitoring"
    assert c.get(f"{BASE}/console/catalog").status_code == 403       # no key


def test_console_without_action_layer(world):  # noqa: F811
    api, c = _ops_client(world)
    r = c.get(f"{BASE}/console/catalog", headers=KEY)
    assert r.status_code == 200
    doc = r.json()
    assert doc["available"] is False and "3.0.5" in doc["reason"] and doc["actions"] == []
    r = c.post(f"{BASE}/console/act/kick_player", headers=KEY,
               json={"params": {"server_name": "vs1", "ucid": "u"}})
    assert r.status_code == 501


def test_console_act_and_read(world):  # noqa: F811
    api, layer, c = _console_client(world)
    H = {**KEY, "X-Fowl-Roles": "moderation", "X-Fowl-Actor": "Fowl%20%5BAdmin%5D%20%C3%91and%C3%BA", "X-Fowl-Subject": "1234"}
    r = c.post(f"{BASE}/console/act/kick_player", headers=H,
               json={"params": {"server_name": "vs1", "ucid": "abc", "reason": "afk"}})
    assert r.status_code == 200, r.text
    assert r.json() == {"ok": True, "message": "kicked abc",
                        "data": {"player_name": "Bob", "ucid": "abc", "audit": "recorded",
                                 "when": "2026-11-05T12:00:00"}}
    kind, name, ctx, params = layer.calls[-1]
    assert (kind, name, params) == ("call", "kick_player",
                                    {"server_name": "vs1", "ucid": "abc", "reason": "afk"})
    assert ctx.identity.roles == {"DCS Admin"} and ctx.identity.scope.unrestricted
    # brackets can't forge a second [web:...] actor in the bot's audit
    assert ctx.identity.shown_name == "Fowl _Admin_ Ñandú" and ctx.identity.subject == "1234"
    assert ctx.audit_actor == "[web:fowlengine/1234]"
    # reads go through read_action
    r = c.post(f"{BASE}/console/read/list_bans", headers=H, json={})
    assert r.status_code == 200 and r.json()["data"]["bans"][0]["ucid"] == "u1"
    assert layer.calls[-1][0] == "read"
    # the wrong door for the kind
    assert c.post(f"{BASE}/console/read/kick_player", headers=H, json={}).status_code == 405
    assert c.post(f"{BASE}/console/act/list_bans", headers=H, json={}).status_code == 405
    # roles
    assert c.post(f"{BASE}/console/act/restart_server", headers=H,
                  json={"params": {"server_name": "vs1"}}).status_code == 403
    assert c.post(f"{BASE}/console/act/restart_node", headers={**H, "X-Fowl-Roles": "ops,moderation"},
                  json={"params": {"node_name": "NODE"}}).status_code == 403
    assert c.post(f"{BASE}/console/act/restart_node", headers={**H, "X-Fowl-Roles": "admin"},
                  json={"params": {"node_name": "NODE"}}).status_code == 200
    # no role header = the key holder itself (the Manager)
    assert c.post(f"{BASE}/console/act/restart_node", headers=KEY,
                  json={"params": {"node_name": "NODE"}}).status_code == 200
    # names and parameters
    assert c.post(f"{BASE}/console/act/os_system", headers=H, json={}).status_code == 404
    assert c.post(f"{BASE}/console/act/Kick-Player", headers=H, json={}).status_code == 404
    assert c.post(f"{BASE}/console/act/install_mod", headers={**H, "X-Fowl-Roles": "ops"},
                  json={"params": {}}).status_code == 501
    r = c.post(f"{BASE}/console/act/kick_player", headers=H, json={"params": {"server_name": "vs1"}})
    assert r.status_code == 400 and "ucid" in r.json()["error"]
    r = c.post(f"{BASE}/console/act/kick_player", headers=H,
               json={"params": {"server_name": "vs1", "ucid": "a", "evil": 1}})
    assert r.status_code == 400 and "evil" in r.json()["error"]
    assert c.post(f"{BASE}/console/act/kick_player", headers=H, json={"params": [1]}).status_code == 400


def test_console_power_actions_wait_for_ops(world):  # noqa: F811
    api, layer, c = _console_client(world)
    H = {**KEY, "X-Fowl-Roles": "ops"}
    api._dcs_actions["vs1"] = {"action": "restart", "state": "running"}
    r = c.post(f"{BASE}/console/act/restart_server", headers=H, json={"params": {"server_name": "vs1"}})
    assert r.status_code == 409 and "already under way" in r.json()["error"]
    r = c.post(f"{BASE}/console/act/restart_server", headers=H, json={"params": {"server_name": "vs2"}})
    assert r.status_code == 200 and r.json()["ok"] is True


def test_console_mission_files(world):  # noqa: F811
    api, layer, c = _console_client(world)
    H = {**KEY, "X-Fowl-Roles": "ops"}
    r = c.get(f"{BASE}/console/mission?server=vs1&mission=2", headers=H)
    assert r.status_code == 200 and r.content == b"PK\x03\x04miz"
    assert 'filename="Op Fowl.miz"' in r.headers["content-disposition"]
    assert layer.calls[-1][3] == {"server_name": "vs1", "mission": 2}
    assert c.get(f"{BASE}/console/mission?server=vs1&mission=2",
                 headers={**H, "X-Fowl-Roles": "moderation"}).status_code == 403
    r = c.post(f"{BASE}/console/mission?server=vs1&name=New.miz&load=true", headers=H, content=b"12345")
    assert r.status_code == 200 and r.json()["message"] == "uploaded New.miz (5 bytes)"
    assert layer.calls[-1][3]["load"] is True and layer.calls[-1][3]["source"] == b"12345"
    assert c.post(f"{BASE}/console/mission?server=vs1&name=x.miz", headers=H, content=b"").status_code == 400
    # loading it straight away is a power action
    api._dcs_actions["vs1"] = {"action": "restart", "state": "running"}
    assert c.post(f"{BASE}/console/mission?server=vs1&name=y.miz&load=true", headers=H,
                  content=b"1").status_code == 409


def test_console_fleet(world):  # noqa: F811
    api, layer, c = _console_client(world)

    class P:
        name, ucid, id, side, slot, active, banned = "Bob", "u1", 2, Status.RUNNING, 3, True, False
        unit_type = "F-16C_50"
        ipaddr = "10.0.0.9"

        @property
        def member(self):
            raise RuntimeError("no discord")

    world.servers[0].get_active_players = lambda: [P()]
    world.servers[0].current_mission = types.SimpleNamespace(name="Op", map="Syria", filename="x.miz",
                                                             date="2026-01-01", start_time=0,
                                                             mission_time=125.5, real_time=10)
    world.cog.node.all_nodes = {"NODE": world.cog.node, "AGENT": None}
    world.cog.node.instances = {"DCS.vs1": world.servers[0].instance}
    try:
        r = c.get(f"{BASE}/console/fleet", headers=KEY)
    finally:
        del world.cog.node.all_nodes, world.cog.node.instances
    assert r.status_code == 200, r.text
    doc = r.json()
    vs1 = next(s for s in doc["servers"] if s["name"] == "vs1")
    assert vs1["status"] == "RUNNING" and vs1["node"] == "NODE"
    assert vs1["mission"]["map"] == "Syria" and vs1["mission"]["mission_time"] == 125.5
    p = vs1["players"][0]
    assert p["name"] == "Bob" and p["discord"] is None and p["unit_type"] == "F-16C_50"
    assert "10.0.0.9" not in r.text                                  # player IPs never leave the bot
    nodes = {n["name"]: n for n in doc["nodes"]}
    assert nodes["AGENT"] == {"name": "AGENT", "online": False}
    assert nodes["NODE"]["online"] and nodes["NODE"]["instances"][0]["name"] == "DCS.vs1"


def test_console_audit_is_for_full_admins(world):  # noqa: F811
    api, layer, c = _console_client(world)
    r = c.get(f"{BASE}/console/audit", headers={**KEY, "X-Fowl-Roles": "ops,moderation"})
    assert r.status_code == 403
    r = c.get(f"{BASE}/console/audit", headers={**KEY, "X-Fowl-Roles": "admin"})
    assert r.status_code == 503                                      # the fake cog has no pool
