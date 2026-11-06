"""
Offline tests for netfail.py: a connection error the bfdb pollers catch is
only "bfdb is down" when it was bfdb the request failed to reach. On 6 Nov a
DNS outage logged "bfdb at http://localhost:8880 is unreachable" for what was
discord.com failing to resolve.

    python -m pytest DCSServerBot/tests -q
"""
import importlib.util
from pathlib import Path

import aiohttp
import pytest

PLUGIN = Path(__file__).resolve().parent.parent / "plugins" / "fowlengine"


@pytest.fixture(scope="module")
def netfail():
    spec = importlib.util.spec_from_file_location("fe_netfail", PLUGIN / "netfail.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def connector_error(host: str, port: int = 443) -> aiohttp.ClientConnectorError:
    """aiohttp's own error, the way the connector raises it."""
    from aiohttp.client_reqrep import ConnectionKey
    key = ConnectionKey(host, port, True, True, None, None, None)
    return aiohttp.ClientConnectorDNSError(key, OSError(11001, "getaddrinfo failed"))


def test_discord_failing_is_not_bfdb(netfail):
    ex = connector_error("discord.com")
    assert netfail.failed_host(ex) == "discord.com"
    assert netfail.elsewhere("http://localhost:8880", ex) == "discord.com"


def test_bfdb_failing_is_bfdb(netfail):
    assert netfail.elsewhere("http://localhost:8880", connector_error("localhost", 8880)) is None
    # localhost and the loopback addresses are the same box
    assert netfail.elsewhere("http://localhost:8880", connector_error("127.0.0.1", 8880)) is None
    assert netfail.elsewhere("http://127.0.0.1:8880", connector_error("::1", 8880)) is None
    assert netfail.elsewhere("https://api.vectorstrike.org", connector_error("API.vectorstrike.org")) is None


def test_errors_without_a_host_stay_bfdb(netfail):
    # a timeout or a closed websocket names no host: those keep meaning bfdb
    assert netfail.elsewhere("http://localhost:8880", TimeoutError()) is None
    assert netfail.elsewhere("http://localhost:8880", ConnectionResetError()) is None
