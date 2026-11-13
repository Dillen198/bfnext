"""Which host a failed request was actually trying to reach.

The pollers and relays talk to bfdb and then post to Discord in one go, so a
connection error they catch is not always bfdb's. When the box lost DNS on
6 Nov, every loop reported "bfdb at http://localhost:8880 is unreachable
(ClientConnectorDNSError: Cannot connect to host discord.com:443 ...)": bfdb
was fine, discord.com would not resolve. aiohttp's connection errors carry
the host they tried (`ClientConnectorError.host`), so the cog can tell the
two apart. No aiohttp import here: the check is by attribute, so the tests
need neither aiohttp nor discord.py.
"""
from urllib.parse import urlsplit


def failed_host(ex: BaseException) -> str | None:
    """The host `ex` failed to connect to, when it says (aiohttp's
    ClientConnectorError and its DNS/SSL subclasses do); None otherwise."""
    host = getattr(ex, "host", None)
    return host.lower() if isinstance(host, str) and host else None


def _same_host(a: str, b: str) -> bool:
    local = {"localhost", "127.0.0.1", "::1"}
    a, b = a.strip("[]").lower(), b.strip("[]").lower()
    return a == b or (a in local and b in local)


def elsewhere(api_url: str, ex: BaseException) -> str | None:
    """The other host `ex` is about, when it failed to reach one that is not
    bfdb's (Discord, say); None when it was bfdb or can't be told."""
    host = failed_host(ex)
    if host is None:
        return None
    bfdb = urlsplit(api_url).hostname
    if bfdb is None or _same_host(host, bfdb):
        return None
    return host
