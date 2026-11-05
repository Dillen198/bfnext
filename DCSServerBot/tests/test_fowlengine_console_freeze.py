"""
The bot froze on 2026-11-05 the first time the SERVER BOT page read the ban
list: the bot's servers are a ThreadSafeDict whose iteration yields
(name, server) pairs, list_bans took those for names and returned Server
objects, and jsonable() then walked server -> players -> server ... on the
event loop. These pin both halves of the fix.
"""
import json
import threading
import time
from dataclasses import dataclass, field

from test_fowlengine_ops import BASE, world  # noqa: F401 -- `world` is a fixture
from test_fowlengine_console import FakeResult, KEY, _console_client
from fe import console as co


class ThreadSafeDict(dict):
    """core/utils/helper.py's, as it is: __iter__ yields (key, value) pairs
    under a lock that is held until the iteration ends."""

    def __init__(self, *args, **kwargs):
        self.lock = threading.Lock()
        super().__init__(*args, **kwargs)

    def __iter__(self):
        with self.lock:
            for key in dict.keys(self):
                yield key, dict.__getitem__(self, key)

    def items(self):
        with self.lock:
            return list(super().items())

    def values(self):
        with self.lock:
            return list(super().values())


@dataclass
class Srv:
    name: str
    players: dict = field(default_factory=dict)


@dataclass
class Plr:
    name: str
    server: "Srv" = None


def _graph(players=14):
    s = Srv("vs1")
    for i in range(players):
        s.players[f"p{i}"] = Plr(f"p{i}", s)
    return s


def test_jsonable_is_bounded_on_a_cyclic_graph():
    srv = _graph()
    t = time.monotonic()
    out = co.jsonable({"servers": [("vs1", srv)], "deep": srv})
    assert time.monotonic() - t < 1.0
    json.dumps(out)
    # the cycle is cut where it closes, not walked again
    assert out["deep"]["players"]["p0"]["server"].startswith("Srv(")


def test_jsonable_names_live_bot_objects():
    Live = type("Server", (), {"__module__": "core.data.impl.serverimpl", "name": "vs1"})
    assert co.jsonable({"server": Live()}) == {"server": "vs1"}
    Bus = type("ServiceBus", (), {"__module__": "services.servicebus.service"})
    assert co.jsonable([Bus()]) == ["ServiceBus"]


def test_jsonable_budget():
    big = list(range(co.JSON_MAX_NODES + 10))
    out = co.jsonable(big)
    assert out[-1] == "..." and out[100] == 100


def test_actions_get_server_names_not_threadsafedict_pairs(world):  # noqa: F811
    api, layer, c = _console_client(world)
    world.cog.bot.servers = ThreadSafeDict({s.name: _graph() for s in world.servers})

    async def list_bans(ctx):
        # what DCSServerBot's own list_bans does for an unscoped caller
        return FakeResult(True, "", {"bans": [], "servers": tuple(name for name in ctx.bus.servers)})
    layer.registry["list_bans"] = list_bans
    import types as _t
    from test_fowlengine_ops import FakeBot  # noqa: F401
    api.console._bus = lambda: _t.SimpleNamespace(servers=world.cog.bot.servers)
    t = time.monotonic()
    r = c.post(f"{BASE}/console/read/list_bans", headers={**KEY, "X-Fowl-Roles": "moderation"}, json={})
    assert time.monotonic() - t < 2.0
    assert r.status_code == 200, r.text
    assert sorted(r.json()["data"]["servers"]) == ["vs1", "vs2"]
    # nothing left holding the dict's lock
    assert world.cog.bot.servers.lock.acquire(timeout=1)
    world.cog.bot.servers.lock.release()


def test_cluster_view():
    servers = ThreadSafeDict({"vs1": object(), "vs2": object()})
    node = _t = type("N", (), {"all_nodes": {"NODE": None}})()
    cl = co._Cluster(type("B", (), {"servers": servers})(), node)
    assert list(cl.servers) == ["vs1", "vs2"] and type(cl.servers) is dict
    assert cl.nodes == {"NODE": None}
    assert co._Cluster(object(), object()).servers == {}
