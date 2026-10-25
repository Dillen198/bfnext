"""Constants and bfdb helpers shared by the FowlEngine cog and its parts
(commands.py and the cog_*.py mixins).

Split out of commands.py unchanged; see that module for how the cog is put
together."""
import discord
import re
import time

# The status embed's map snapshot, attached as attachment://<this>.
STATUS_MAP_FILE = "campaign-map.png"

# NOTE: this plugin once subclassed Plugin[FowlEngineEventListener] and
# registered listener.py's FowlEngineEventListener for a vs_event /
# registerDCSServer bridge. That bridge never had a producer (lua/callbacks.lua
# never calls sendBotTable), so the listener was unregistered and then removed
# as dead code; `git log --diff-filter=D -- DCSServerBot/plugins/fowlengine/listener.py`
# finds it, should the Lua side ever send real events. Everything it would have
# posted is polled from bfdb instead (below).

# ── Live engine log relay ───────────────────────────────────────────────────
# Tails bfdb's /ws/engine-logs websocket (the raw bflib engine log, distinct
# from the curated vs_event alerts above) into a Discord channel: a single
# message gets edited with a rolling tail, and ERROR/WARN lines additionally
# get posted as standalone alerts so they don't scroll off unnoticed.
ENGINE_LOG_TAIL_LINES = 40
ENGINE_LOG_FLUSH_SECS = 3.0
ENGINE_LOG_RECONNECT_SECS = 10
ENGINE_LOG_ALERT_DEDUPE = 500
ENGINE_LOG_LEVEL_RE = re.compile(r"\[(ERROR|WARN|WARNING)\]", re.IGNORECASE)

# ── Live capture/achievement alerts ─────────────────────────────────────────
# vs_event (the removed listener.py) had no producer on the Lua side, so this polls bfdb's
# public /api/objectives and /api/kills instead: no bflib/Lua changes needed,
# and both endpoints are unauthenticated (no admin_username/password required).
CAMPAIGN_POLL_SECS = 20
CAMPAIGN_RECONNECT_SECS = 15

# ── Objective alert thresholds ──────────────────────────────────────────────
# An objective is "ready to be captured" at or below WEAK_HEALTH, and only
# stops being so once it has recovered to WEAK_CLEAR_HEALTH. The gap between
# the two is deliberate hysteresis: a base sitting right on the threshold gets
# nudged either side of it by every repair tick, and without the gap that
# re-announced "ready to be captured" indefinitely.
WEAK_HEALTH = 20
WEAK_CLEAR_HEALTH = 35

# How many consecutive polls must agree before an ownership change is
# announced. One poll is not enough: a single stale or mid-capture read
# otherwise produces a phantom "has gone neutral", and with the value
# alternating between polls, an endless stream of them.
OWNER_CONFIRM_POLLS = 2
ACHIEVEMENT_THRESHOLDS = [(15, "God of War"), (10, "Unstoppable"), (5, "Ace")]

# Every post whose text carries something a player or an outside system chose
# (pilot and objective names, GCI transcripts, release notes, exception text)
# goes out with mentions disabled: DCSServerBot sets no client-wide default,
# so a pilot named "@everyone" would otherwise ping the whole guild.
NO_PINGS = discord.AllowedMentions.none()

# Fowl Engine Manager drops this file into DCSServerBot's folder (the bot's
# working directory) to ask for a clean shutdown before it would otherwise
# kill the process -- so cogs unload, bfdb is stopped gracefully and state is
# saved. Checked every second by status_watch.
SHUTDOWN_REQUEST_FILE = "fowl-shutdown.request"

# ── Per-coalition briefing channels ──────────────────────────────
# Two channels per DCS server, one per coalition, each holding a single embed
# that is edited in place from bfdb's GET /api/situation. Read access is gated
# by the coalition role below, and that role is a mirror of the side the ENGINE
# registered -- so the only way into Red's channel is to actually be flying Red.
BRIEFING_UPDATE_MINUTES = 3.0

# How long to wait on /api/situation. The engine walks every objective, the
# intel db and the radar net to build one, and bfdb already allows itself 8s
# for that RPC -- so anything shorter here just times the whole thing out on a
# busy server.
BRIEFING_HTTP_TIMEOUT = 20

# How often the Discord coalition roles are reconciled against the engine's own
# registrations. Slow on purpose: it is a mirror, not a gate on joining, and a
# player who just switched sides can wait a few minutes for their channel.
COALITION_SYNC_MINUTES = 5.0

# Safety rail on the revocation half of that sync. Revoking is the destructive
# direction and has no undo, so one pass may only take the role off whichever
# is larger: this many people, or this fraction of the current role holders.
# Anything beyond that is treated as bad input from bfdb rather than obeyed.
REVOKE_MIN_PER_TICK = 10
REVOKE_MAX_FRACTION = 0.25

# ── Training range (bfrange) servers ─────────────────────────────────────────
# A DCS server whose bfdb instance is `kind: range` runs bfrange.dll instead of
# bflib.dll: no objectives, no coalitions, no GCI. Every campaign-only loop and
# command below skips it, and it gets its own two feeds instead -- a live
# status embed (tankers, carriers, stations, players) and a graded-results
# feed with the debrief card attached. Pure logic lives in rangefeed.py.
RANGE_STATUS_MINUTES = 2.0
RANGE_RESULTS_POLL_SECS = 15.0
RANGE_HTTP_TIMEOUT = 15
# Result cards are a few hundred KB; anything near Discord's attachment limit
# is not a card, so post the image by URL instead.
RANGE_CARD_MAX_BYTES = 8 * 1024 * 1024
# ── War diary (newsfeed.py) ─────────────────────────────────────────────────
# Each newly filed daily dispatch goes to the server's `news_channel` once,
# with bfdb's picture attached when there is one. How long to wait on bfdb for
# the list and for the picture download.
NEWS_HTTP_TIMEOUT = 30

RANGE_ONLY_MSG = ("**{name}** is a training range server -- this command is for campaign "
                  "servers. Try `/range status` instead.")

# ── bfdb process supervision ────────────────────────────────────────────────
# bfdb.exe + the netidx resolver are owned by procman.py as child processes of
# the bot (this replaces the old bfsystem.ps1 launcher). The supervise_bfdb
# task below just ticks procman's health check on a loop; all the relaunch /
# cooldown / staged-binary logic lives in Procman. Config is the `bfdb:` and
# `gci:` blocks in fowlengine.yaml -- see fowlengine.sample.yaml.

# ── bfdb admin auth ──────────────────────────────────────────────────────────
# bfdb's admin-gated endpoints (e.g. /api/commander/spawn, /ws/engine-logs) only
# ever check the "session" cookie set by POST /api/auth/local-login -- they do
# not accept an Authorization header. Module-level (not on FowlEngine) so
# CommanderTerminalView, which isn't a Plugin, can use it too.


def srv_params(server_name: str | None, **extra) -> dict:
    """Query params that pin a bfdb request to one DCS server.

    A single bfdb can front several DCS servers (see deploy/multi-instance.md).
    Every instance-scoped route accepts `?server=<DCSServerBot server name>`,
    which bfdb maps to an instance via that instance's `dcs_server_name` in its
    --instances file -- so the bot never has to know bfdb's own instance ids.
    Omitting it (single-server bfdb, or a server not listed) lets bfdb answer
    from its default instance, which is the old behaviour exactly.
    """
    p = {k: v for k, v in extra.items() if v is not None}
    if server_name:
        p["server"] = server_name
    return p


def srv_path(path: str, server_name: str | None) -> str:
    """`srv_params` for the places that build a URL string rather than pass
    params -- the admin GET/POST helpers, whose callers pass a full path."""
    if not server_name:
        return path
    from urllib.parse import quote
    sep = "&" if "?" in path else "?"
    return f"{path}{sep}server={quote(server_name)}"


async def bfdb_login(http, api_url: str, username: str, password: str) -> str:
    """POST /api/auth/local-login and return the resulting session cookie value."""
    async with http.post(
        f"{api_url}/api/auth/local-login",
        json={"username": username, "password": password},
        timeout=8,
    ) as resp:
        if resp.status != 200:
            raise RuntimeError(f"bfdb local-login failed ({resp.status}): {await resp.text()}")
        set_cookie = resp.headers.get("set-cookie", "")
        m = re.search(r"session=([0-9a-fA-F-]+)", set_cookie)
        if not m:
            raise RuntimeError("bfdb local-login succeeded but returned no session cookie")
        return m.group(1)


async def bfdb_admin_post(api_url: str, username: str, password: str, path: str, json_body: dict):
    """POST to an admin-gated bfdb endpoint via the local-login session cookie flow.
    Returns (status, parsed_json_or_None)."""
    import aiohttp
    async with aiohttp.ClientSession() as http:
        session_cookie = await bfdb_login(http, api_url, username, password)
        async with http.post(
            f"{api_url}{path}", json=json_body, headers={"Cookie": f"session={session_cookie}"}, timeout=10
        ) as resp:
            status = resp.status
            try:
                data = await resp.json()
            except Exception:
                data = None
            return status, data


async def bfdb_admin_get(api_url: str, username: str, password: str, path: str):
    """GET an admin-gated bfdb endpoint via the local-login session cookie flow.
    Returns (status, parsed_json_or_None)."""
    import aiohttp
    async with aiohttp.ClientSession() as http:
        session_cookie = await bfdb_login(http, api_url, username, password)
        async with http.get(
            f"{api_url}{path}", headers={"Cookie": f"session={session_cookie}"}, timeout=10
        ) as resp:
            status = resp.status
            try:
                data = await resp.json()
            except Exception:
                data = None
            return status, data

# ── cached admin session ────────────────────────────────────────────────────
# bfdb only prunes an auth session when that session id is next looked up, so a
# login the bot makes and never reuses sits in the sled tree indefinitely. The
# one-shot helpers above are fine for a slash command; a loop that logs in on
# every tick would leave thousands of dead rows a day. These reuse one cookie
# (bfdb issues them for 7 days) and re-login only when it is actually rejected.
_ADMIN_SESSION_CACHE: dict = {}
_ADMIN_SESSION_TTL = 6 * 3600


async def bfdb_session(http, api_url: str, username: str, password: str,
                       force: bool = False) -> str:
    key = (api_url, username)
    now = time.monotonic()
    if not force:
        hit = _ADMIN_SESSION_CACHE.get(key)
        if hit and now - hit[1] < _ADMIN_SESSION_TTL:
            return hit[0]
    cookie = await bfdb_login(http, api_url, username, password)
    _ADMIN_SESSION_CACHE[key] = (cookie, now)
    return cookie


class BriefingUnavailable(RuntimeError):
    """/api/situation answered 503: bfdb is up but the engine isn't answering."""


async def bfdb_get_cached(api_url: str, username: str, password: str, path: str,
                          timeout: int = 10):
    """GET an admin-gated endpoint on the cached session, re-logging in once if
    the cookie has been rejected. Returns (status, parsed_json_or_None)."""
    import aiohttp
    async with aiohttp.ClientSession() as http:
        for attempt in (0, 1):
            cookie = await bfdb_session(http, api_url, username, password,
                                        force=attempt == 1)
            async with http.get(f"{api_url}{path}",
                                headers={"Cookie": f"session={cookie}"},
                                timeout=timeout) as resp:
                if resp.status in (401, 403) and attempt == 0:
                    continue
                try:
                    data = await resp.json()
                except Exception:
                    data = None
                return resp.status, data


def _fit_rows(rows: list, limit: int, sep: str) -> str:
    """Join rows until `limit` characters, never cutting one in half -- a
    custom emoji sliced mid-markup renders as garbage text."""
    out, used = [], 0
    for i, row in enumerate(rows):
        if used + len(row) + len(sep) > limit:
            out.append(f"… +{len(rows) - i} more")
            break
        out.append(row)
        used += len(row) + len(sep)
    return sep.join(out)


# The rank ladder's titles by tier (NATO service), as the dashboard and the
# engine name them.
RANK_TITLES = ["2nd Lieutenant", "1st Lieutenant", "Captain", "Major",
               "Lieutenant Colonel", "Colonel", "Brigadier General", "Major General"]
