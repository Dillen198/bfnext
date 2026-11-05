"""
The bot console: what the dashboard's SERVER BOT pages talk to (through bfdb).

DCSServerBot 3.0.5 grew an admin web console, and with it an action layer
(core/actions.py): every server, mission, player, ban, node, mod, music and
statistics operation is one `@action` function that Discord, MCP and its own
console all call. We don't run that console. The dashboard has the same pages,
and they reach the same actions through here, so a kick from the dashboard is
the bot's own kick: the same checks, the same audit entry, the same refusal
when another action on that server is still running.

Routes sit on the OPS router (opsapi.py), so they share its X-API-Key and its
prefix:

  GET  console/catalog            the actions this bot offers, which ones the caller may use,
                                  and each one's parameters
  POST console/read/<action>      {params: {...}}  -- a read action (list_*, get_*, ...)
  POST console/act/<action>       {params: {...}}  -- a write action
  GET  console/fleet              nodes, servers and who is on each, one call
  GET  console/audit?limit=&q=    the bot's own audit trail
  GET  console/mission?server=&mission=
                                  a mission file's bytes (download_mission)
  POST console/mission?server=&name=&force=&load=&autostart=
                                  a .miz as the raw request body (upload_mission)

Who may do what: bfdb has already checked the dashboard login. It names the
caller (X-Fowl-Actor, X-Fowl-Subject) and the roles it holds (X-Fowl-Roles:
admin | ops,content,moderation). Each action below needs one of those; a
request without X-Fowl-Roles comes from something that holds the key itself
(Fowl Engine Manager), which is the box owner. The bot sees a dashboard admin
as its "Admin" role and anyone else as "DCS Admin", both unscoped: the
dashboard has no per-server managers.
"""
from __future__ import annotations

import asyncio
import dataclasses
import enum
import hashlib
import inspect
import json
import os
import re
from datetime import date, datetime, timedelta, timezone
from decimal import Decimal
from typing import Any, Callable, Optional

from .icons import plain

try:  # FastAPI resolves handler annotations from this module's globals
    from fastapi import Request
except Exception:  # noqa: BLE001 - imported outside the bot (unit tests)
    Request = Any  # type: ignore[misc,assignment]

__all__ = ["ConsoleApi", "ACTIONS", "jsonable", "caller_roles", "may_use", "UPLOAD_MAX_BYTES"]

# The largest .miz the dashboard may upload. bfdb lets this path through with
# the same cap.
UPLOAD_MAX_BYTES = 256 * 1024 * 1024

# name -> (kind, role). kind: "read" goes through read_action (no in-flight
# guard, no audit), "write" through call_action. role: what the dashboard
# login must hold -- "ops" (server operations), "moderation" (players and
# bans) or "admin" (whole nodes, and what adds or removes servers).
ACTIONS: dict[str, tuple[str, str]] = {
    # -- servers (plugins/mission) --
    "start_server": ("write", "ops"),
    "stop_server": ("write", "ops"),
    "restart_server": ("write", "ops"),
    "startup_server": ("write", "ops"),
    "shutdown_server": ("write", "ops"),
    "message_server": ("write", "ops"),
    # -- missions --
    "pause_mission": ("write", "ops"),
    "unpause_mission": ("write", "ops"),
    "restart_mission": ("write", "ops"),
    "rotate_mission": ("write", "ops"),
    "load_mission": ("write", "ops"),
    "set_active_mission": ("write", "ops"),
    "add_mission": ("write", "ops"),
    "delete_mission": ("write", "ops"),
    "delete_missions": ("write", "ops"),
    "reorder_mission": ("write", "ops"),
    "list_missions": ("read", "ops"),
    "get_mission_info": ("read", "ops"),
    "get_mission_list": ("read", "ops"),
    "get_addable_missions": ("read", "ops"),
    "read_mission_weather": ("read", "ops"),
    # -- players --
    "message_player": ("write", "moderation"),
    "kick_player": ("write", "moderation"),
    "ban_player": ("write", "moderation"),
    "ban_ucid": ("write", "moderation"),
    "unban": ("write", "moderation"),
    "edit_ban": ("write", "moderation"),
    "mute_player": ("write", "moderation"),
    "unmute_player": ("write", "moderation"),
    "watch_player": ("write", "moderation"),
    "unwatch_player": ("write", "moderation"),
    "edit_watch": ("write", "moderation"),
    "screenshot_player": ("write", "moderation"),
    "punish_player": ("write", "moderation"),
    "forgive_player": ("write", "moderation"),
    "list_bans": ("read", "moderation"),
    "list_player_directory": ("read", "moderation"),
    "get_player_detail": ("read", "moderation"),
    "get_player_statistics": ("read", "moderation"),
    "get_player_infractions": ("read", "moderation"),
    "search_players": ("read", "moderation"),
    # -- scheduler: maintenance and the server's DCS settings --
    "set_maintenance": ("write", "ops"),
    "clear_maintenance": ("write", "ops"),
    "get_server_config": ("read", "ops"),
    "set_server_config": ("write", "ops"),
    "get_server_coalitions": ("read", "ops"),
    "set_coalition_password": ("write", "ops"),
    "set_server_channels": ("write", "admin"),
    # -- nodes (plugins/admin) --
    "restart_node": ("write", "admin"),
    "shutdown_node": ("write", "admin"),
    "upgrade_node": ("write", "admin"),
    "update_dcs": ("write", "admin"),
    "take_node_offline": ("write", "admin"),
    "bring_node_online": ("write", "admin"),
    "add_server": ("write", "admin"),
    "delete_server": ("write", "admin"),
    # -- mods (plugins/modmanager) --
    "list_installed_mods": ("read", "ops"),
    "list_available_mods": ("read", "ops"),
    "list_repo_versions": ("read", "ops"),
    "list_mod_versions": ("read", "ops"),
    "install_mod": ("write", "ops"),
    "uninstall_mod": ("write", "ops"),
    "change_mod_version": ("write", "ops"),
    "download_mod": ("write", "admin"),
    # -- statistics (plugins/monitoring) --
    "get_server_statistics": ("read", "ops"),
    "get_node_statistics": ("read", "ops"),
    "get_load_statistics": ("read", "ops"),
    # -- music (plugins/music) --
    "list_music_playlists": ("read", "ops"),
    "get_music_playlist": ("read", "ops"),
    "read_music_library": ("read", "ops"),
    "get_music_radios": ("read", "ops"),
    "add_playlist_track": ("write", "ops"),
    "remove_playlist_track": ("write", "ops"),
    "reorder_playlist": ("write", "ops"),
    "rename_playlist": ("write", "ops"),
    "delete_playlist": ("write", "ops"),
    "set_radio_playlist": ("write", "ops"),
    "control_radio": ("write", "ops"),
    "set_radio_config": ("write", "ops"),
}

# The upstream plugin each action lives in, for the catalog (and the pages'
# "this needs the X plugin" hint).
_PLUGIN_OF = {
    **{n: "admin" for n in ("restart_node", "shutdown_node", "upgrade_node", "update_dcs",
                            "take_node_offline", "bring_node_online", "add_server", "delete_server")},
    **{n: "scheduler" for n in ("set_maintenance", "clear_maintenance", "get_server_config",
                                "set_server_config", "get_server_coalitions", "set_coalition_password",
                                "set_server_channels")},
    **{n: "punishment" for n in ("punish_player", "forgive_player")},
    **{n: "monitoring" for n in ("get_server_statistics", "get_node_statistics", "get_load_statistics")},
    **{n: "modmanager" for n in ("list_installed_mods", "list_available_mods", "list_repo_versions",
                                 "list_mod_versions", "install_mod", "uninstall_mod", "change_mod_version",
                                 "download_mod")},
    **{n: "music" for n in ("list_music_playlists", "get_music_playlist", "read_music_library",
                            "get_music_radios", "add_playlist_track", "remove_playlist_track",
                            "reorder_playlist", "rename_playlist", "delete_playlist", "set_radio_playlist",
                            "control_radio", "set_radio_config")},
}
for _n in ACTIONS:
    _PLUGIN_OF.setdefault(_n, "mission")

# Actions that start, stop or reload DCS: refused while the auto-updater is
# staging a release or OPS is already restarting that server (opsapi's
# _dcs_blocked), the same rule the OPS page's own DCS buttons follow.
POWER_ACTIONS = frozenset({
    "start_server", "stop_server", "restart_server", "startup_server", "shutdown_server",
    "restart_mission", "rotate_mission", "load_mission",
})

# Actions whose role the Access tab can't lower: whole nodes, adding and
# removing servers, downloading code onto the box, the bot's Discord channels.
# The bot's own console keeps these Admin-only too.
LOCKED_ADMIN = frozenset({
    "restart_node", "shutdown_node", "upgrade_node", "update_dcs", "take_node_offline", "bring_node_online",
    "add_server", "delete_server", "download_mod", "set_server_channels",
})
ACCESS_FILE = "fowlengine-console-access.json"
STREAM_MAX = 16                 # concurrent live streams
STREAM_TICK = 2.0               # seconds between looks at the fleet
STREAM_PING = 15.0              # a comment line at least this often, so proxies keep it open
SCREENSHOT_KEY = re.compile(r"^/screenshots/[A-Za-z0-9 ._()\-]{1,120}\.png$")

_ROLE_NAMES = ("admin", "ops", "content", "moderation")
_ACTION_NAME = re.compile(r"^[a-z][a-z0-9_]{0,63}$")
AUDIT_LIMIT_MAX = 1000


def caller_roles(header: Optional[str]) -> frozenset[str]:
    """The dashboard roles bfdb named in X-Fowl-Roles. No header at all is
    the key holder itself (Fowl Engine Manager): every role. An empty one is
    a login with none, which bfdb would not have let through, so: none."""
    if header is None:
        return frozenset({"admin"})
    return frozenset(r for r in (p.strip().lower() for p in header.split(",")) if r in _ROLE_NAMES)


def may_use(roles: frozenset[str], role: str) -> bool:
    """Whether a caller holding `roles` may use an action that needs `role`."""
    if "admin" in roles:
        return True
    if role == "admin":
        return False
    return role in roles


def jsonable(value: Any, depth: int = 0) -> Any:
    """`value` as something json.dumps takes: an action's result carries
    dataclasses, datetimes, enums, Decimals and (in one place) bytes."""
    if depth > 40:
        return str(value)
    if value is None or isinstance(value, (bool, int, str)):
        return value
    if isinstance(value, float):
        return value if value == value and value not in (float("inf"), float("-inf")) else None
    if isinstance(value, Decimal):
        return float(value)
    if isinstance(value, (datetime, date)):
        return value.isoformat()
    if isinstance(value, timedelta):
        return value.total_seconds()
    if isinstance(value, enum.Enum):
        return value.name
    if isinstance(value, (bytes, bytearray, memoryview)):
        return {"bytes": len(value)}
    if dataclasses.is_dataclass(value) and not isinstance(value, type):
        return {f.name: jsonable(getattr(value, f.name, None), depth + 1) for f in dataclasses.fields(value)}
    if isinstance(value, dict):
        return {str(k): jsonable(v, depth + 1) for k, v in value.items()}
    if isinstance(value, (list, tuple, set, frozenset)):
        return [jsonable(v, depth + 1) for v in value]
    return str(value)


def result_doc(result: Any) -> dict:
    """An ActionResult as the dashboard reads it: ok, message, and the rest
    under data -- the result's own `data` dict plus any typed fields its
    subclass adds (server_name, missions, applied, ...). Bytes never travel
    here; the mission download has its own route."""
    doc: dict = {"ok": bool(getattr(result, "success", False)),
                 "message": plain(str(getattr(result, "message", "") or ""))}
    data: dict = {}
    if dataclasses.is_dataclass(result) and not isinstance(result, type):
        for f in dataclasses.fields(result):
            if f.name in ("success", "message", "data"):
                continue
            data[f.name] = jsonable(getattr(result, f.name, None))
    extra = getattr(result, "data", None)
    if isinstance(extra, dict):
        data.update(jsonable(extra))
    doc["data"] = data
    return doc


class _Unscoped:
    """The scope a dashboard caller carries: every server. core/actions.py
    reads `unrestricted`, `resolved` and `allows(managed_by)` off it."""
    unrestricted = True
    resolved = True

    @staticmethod
    def allows(_managed_by: Any) -> bool:
        return True

    @staticmethod
    def server_managed_by(_server: Any) -> bool:
        return True


@dataclasses.dataclass
class Identity:
    """Duck-typed for ActionContext.from_web: roles, scope, shown_name, and
    backend/subject for the audit's `[web:fowlengine/<subject>]` actor."""
    roles: frozenset
    shown_name: str
    subject: str
    backend: str = "fowlengine"
    scope: Any = dataclasses.field(default_factory=_Unscoped)


def bot_roles(roles: frozenset[str]) -> frozenset[str]:
    """The bot's own role names for a dashboard caller."""
    return frozenset({"Admin"}) if "admin" in roles else frozenset({"DCS Admin"})


def _header(request: Any, name: str) -> Optional[str]:
    try:
        v = request.headers.get(name)
    except Exception:  # noqa: BLE001
        return None
    return None if v is None else str(v)


def _clean_name(value: Optional[str], fallback: str) -> str:
    """A caller name from bfdb (percent-encoded: header values are ASCII),
    made safe for the bot's `[web:fowlengine/<subject>]` audit actor."""
    from urllib.parse import unquote
    text = re.sub(r"[\x00-\x1f\x7f\[\]]", "_", unquote(str(value or ""))).strip()
    return text[:80] or fallback


class _Layer:
    """The pieces of core/actions.py this module calls, looked up once.
    Tests hand ConsoleApi a fake with the same five attributes."""

    def __init__(self, mod: Any):
        self.ActionContext = mod.ActionContext
        self.call_action: Callable = mod.call_action
        self.read_action: Callable = mod.read_action
        self.action_available: Callable = mod.action_available
        self.action_registry: Callable = mod.action_registry


class ConsoleApi:
    def __init__(self, cog, ops=None, layer: Any = None):
        self.cog = cog
        self.ops = ops                      # the OpsApi this hangs off (for _dcs_blocked)
        self.log = cog.log
        self._layer = layer
        self._layer_error: Optional[str] = None
        self._discovered = layer is not None
        self._streams = 0
        self._config = None

    # ---- who may use what (the Access tab) -----------------------------------

    def _access_path(self) -> str:
        node = getattr(self.cog.bot, "node", None)
        return os.path.join(getattr(node, "config_dir", None) or "config", ACCESS_FILE)

    def overrides(self) -> dict[str, str]:
        """{action: role} the Access tab set, ignoring anything it may not set."""
        try:
            with open(self._access_path(), encoding="utf-8") as fh:
                doc = json.load(fh)
        except (OSError, ValueError):
            return {}
        if not isinstance(doc, dict):
            return {}
        return {k: v for k, v in doc.items()
                if k in ACTIONS and k not in LOCKED_ADMIN and v in ("ops", "moderation", "admin")}

    def role_of(self, name: str, overrides: Optional[dict] = None) -> str:
        o = self.overrides() if overrides is None else overrides
        return o.get(name) or ACTIONS[name][1]

    # ---- the action layer -------------------------------------------------

    def layer(self) -> Optional[Any]:
        """core.actions, or None (with the reason in _layer_error) on a bot
        older than 3.0.5."""
        if self._layer is not None:
            if not self._discovered:
                self._discover()
            return self._layer
        try:
            import importlib
            mod = importlib.import_module("core.actions")
            self._layer = _Layer(mod)
        except Exception as ex:  # noqa: BLE001
            self._layer_error = (f"this DCSServerBot has no action layer ({ex}) -- the bot console "
                                 "needs DCSServerBot 3.0.5 or later")
            return None
        self._discover()
        return self._layer

    def _discover(self) -> None:
        """The bot's own plugin load registers every action. Run discovery once
        more ourselves, the way upstream's console does, in case this bot loaded
        its plugins without it. Never fatal."""
        self._discovered = True
        try:
            import importlib
            mod = importlib.import_module("core.actions")
            pm = importlib.import_module("core.plugin_manager")
            names = list(pm.PluginManager(self.cog.bot.node).plugins)
            mod.discover_actions(names)
        except Exception as ex:  # noqa: BLE001
            self.log.debug(f"FowlEngine/console: action discovery skipped ({ex})")

    def _bus(self) -> Any:
        try:
            from core import ServiceRegistry
            from services.servicebus import ServiceBus
            bus = ServiceRegistry.get(ServiceBus)
            if bus is not None:
                return bus
        except Exception:  # noqa: BLE001
            pass
        return self.cog.bot.bus if hasattr(self.cog.bot, "bus") else self.cog.bot

    def _ctx(self, layer, request) -> Any:
        roles = caller_roles(_header(request, "X-Fowl-Roles"))
        actor = _clean_name(_header(request, "X-Fowl-Actor"), "dashboard")
        subject = _clean_name(_header(request, "X-Fowl-Subject"), actor)
        ident = Identity(roles=bot_roles(roles), shown_name=actor, subject=subject)
        return layer.ActionContext.from_web(ident, self.cog.bot.node, self._bus())

    # ---- routes ---------------------------------------------------------------

    def add_routes(self, r) -> None:
        r.add_api_route("/console/catalog", self.catalog, methods=["GET"])
        r.add_api_route("/console/read/{name}", self.read, methods=["POST"])
        r.add_api_route("/console/act/{name}", self.act, methods=["POST"])
        r.add_api_route("/console/fleet", self.fleet, methods=["GET"])
        r.add_api_route("/console/audit", self.audit, methods=["GET"])
        r.add_api_route("/console/mission", self.mission_download, methods=["GET"])
        r.add_api_route("/console/mission", self.mission_upload, methods=["POST"])
        r.add_api_route("/console/music", self.music_upload, methods=["POST"])
        r.add_api_route("/console/screenshot", self.screenshot, methods=["GET"])
        r.add_api_route("/console/server", self.server_overview, methods=["GET"])
        r.add_api_route("/console/stream", self.stream, methods=["GET"])
        r.add_api_route("/console/access", self.access_get, methods=["GET"])
        r.add_api_route("/console/access", self.access_set, methods=["POST"])
        r.add_api_route("/console/config/files", self.config_files, methods=["GET"])
        r.add_api_route("/console/config/file", self.config_read, methods=["GET"])
        r.add_api_route("/console/config/file", self.config_write, methods=["POST"])
        r.add_api_route("/console/config/validate", self.config_validate, methods=["POST"])
        r.add_api_route("/console/config/service", self.config_service, methods=["POST"])

    @staticmethod
    def _err(status: int, message: str):
        from fastapi.responses import JSONResponse
        return JSONResponse(status_code=status, content={"ok": False, "error": message})

    @staticmethod
    def _params_of(fn: Callable) -> list[dict]:
        out = []
        try:
            sig = inspect.signature(fn)
        except (TypeError, ValueError):
            return out
        for i, (name, p) in enumerate(sig.parameters.items()):
            if i == 0 or p.kind in (p.VAR_POSITIONAL, p.VAR_KEYWORD):
                continue                # ctx, and *args/**kwargs
            entry: dict = {"name": name, "required": p.default is p.empty}
            if p.default is not p.empty:
                entry["default"] = jsonable(p.default)
            if p.annotation is not p.empty:
                entry["type"] = p.annotation if isinstance(p.annotation, str) else \
                    getattr(p.annotation, "__name__", str(p.annotation))
            out.append(entry)
        return out

    async def catalog(self, request: Request):
        layer = self.layer()
        roles = caller_roles(_header(request, "X-Fowl-Roles"))
        try:
            import importlib
            bot_version = getattr(importlib.import_module("version"), "__version__", None)
        except Exception:  # noqa: BLE001
            bot_version = None
        if layer is None:
            return {"available": False, "reason": self._layer_error, "bot_version": bot_version,
                    "roles": sorted(roles), "actions": []}
        try:
            registry = layer.action_registry()
        except Exception:  # noqa: BLE001
            registry = {}
        actions = []
        over = self.overrides()
        for name, (kind, _default) in ACTIONS.items():
            role = self.role_of(name, over)
            fn = registry.get(name)
            actions.append({
                "name": name, "kind": kind, "role": role, "plugin": _PLUGIN_OF.get(name, "mission"),
                "available": fn is not None, "allowed": may_use(roles, role),
                "params": self._params_of(fn) if fn is not None else [],
            })
        return {"available": True, "bot_version": bot_version, "roles": sorted(roles), "actions": actions}

    async def _run(self, request, name: str, kind: str):
        if not _ACTION_NAME.match(name or "") or name not in ACTIONS:
            return self._err(404, f"no such action {name!r}")
        want_kind = ACTIONS[name][0]
        role = self.role_of(name)
        if want_kind != kind:
            other = "act" if want_kind == "write" else "read"
            return self._err(405, f"{name} is a {want_kind} action -- POST console/{other}/{name}")
        roles = caller_roles(_header(request, "X-Fowl-Roles"))
        if not may_use(roles, role):
            return self._err(403, f"{name} needs the {role} role")
        layer = self.layer()
        if layer is None:
            return self._err(501, self._layer_error or "no action layer")
        try:
            body = await request.json()
        except Exception:  # noqa: BLE001
            body = {}
        params = body.get("params", {}) if isinstance(body, dict) else {}
        if not isinstance(params, dict):
            return self._err(400, "params must be an object")
        try:
            fn = layer.action_registry().get(name)
        except Exception:  # noqa: BLE001
            fn = None
        if fn is None:
            return self._err(501, f"{name} is not available on this bot -- its "
                                  f"{_PLUGIN_OF.get(name, 'mission')} plugin is not loaded")
        known = {p["name"] for p in self._params_of(fn)}
        unknown = sorted(set(params) - known)
        if unknown:
            return self._err(400, f"{name} takes no parameter(s) {', '.join(unknown)}")
        missing = [p["name"] for p in self._params_of(fn) if p["required"] and p["name"] not in params]
        if missing:
            return self._err(400, f"{name} needs {', '.join(missing)}")
        if name in POWER_ACTIONS and self.ops is not None:
            server = self.cog.bot.servers.get(str(params.get("server_name", "")))
            if server is not None:
                blocked = self.ops._dcs_blocked(server)
                if blocked:
                    return self._err(409, blocked)
        ctx = self._ctx(layer, request)
        try:
            if kind == "read":
                result = await layer.read_action(name, ctx, **params)
            else:
                result = await layer.call_action(name, ctx, **params)
        except Exception as ex:  # noqa: BLE001 -- call_action types its own; this is belt and braces
            self.log.exception(f"FowlEngine/console: {name} raised")
            return self._err(500, f"{name} failed: {ex}")
        if kind == "write":
            self.log.info(f"FowlEngine/console: {name} by {getattr(ctx, 'audit_actor', '?')} -> "
                          f"{'ok' if getattr(result, 'success', False) else 'refused'}")
        return result_doc(result)

    async def read(self, name: str, request: Request):
        return await self._run(request, name, "read")

    async def act(self, name: str, request: Request):
        return await self._run(request, name, "write")

    # ---- the fleet: what the bot console's dashboard, servers and nodes pages show ----

    @staticmethod
    def _safe(fn: Callable, default: Any = None) -> Any:
        try:
            return fn()
        except Exception:  # noqa: BLE001
            return default

    def _player_row(self, p: Any) -> dict:
        s = self._safe
        side = s(lambda: p.side)
        member = s(lambda: p.member)
        return {
            "name": s(lambda: p.name),
            "ucid": s(lambda: p.ucid),
            "id": s(lambda: p.id),
            "side": getattr(side, "name", side if side is None else str(side)),
            "slot": s(lambda: p.slot),
            "unit_type": s(lambda: p.unit_type),
            "unit_callsign": s(lambda: p.unit_callsign),
            "unit_name": s(lambda: p.unit_name),
            "group_name": s(lambda: p.group_name),
            "active": bool(s(lambda: p.active, False)),
            "verified": bool(s(lambda: p.verified, False)),
            "watchlist": bool(s(lambda: p.watchlist, False)),
            "banned": bool(s(lambda: p.banned, False)),
            "muted": bool(s(lambda: p.muted, False)),
            "vip": bool(s(lambda: p.vip, False)),
            "discord": getattr(member, "display_name", None) if member else None,
            "sub_slot": s(lambda: p.sub_slot),
        }

    def _server_row(self, server: Any) -> dict:
        s = self._safe
        mission = s(lambda: server.current_mission)
        status = s(lambda: server.status)
        node = s(lambda: server.node)
        players = s(lambda: server.get_active_players(), []) or []
        settings = s(lambda: server.settings, {}) or {}
        locals_ = s(lambda: server.locals, {}) or {}
        restart = s(lambda: server.restart_time)
        row = {
            "name": s(lambda: server.name),
            "display_name": s(lambda: server.display_name),
            "status": getattr(status, "name", None if status is None else str(status)),
            "node": getattr(node, "name", None),
            "instance": s(lambda: server.instance.name),
            "maintenance": bool(s(lambda: server.maintenance, False)),
            "is_remote": bool(s(lambda: server.is_remote, False)),
            "managed_by": jsonable(locals_.get("managed_by")) if isinstance(locals_, dict) else None,
            "max_players": s(lambda: int(settings.get("maxPlayers"))) if isinstance(settings, dict) else None,
            "password": bool(isinstance(settings, dict) and settings.get("password")),
            "port": s(lambda: server.instance.dcs_port),
            "restart_time": restart.isoformat() if isinstance(restart, datetime) else None,
            "mission": None,
            "players": [self._player_row(p) for p in players],
        }
        if mission is not None:
            def num(attr: str) -> Optional[float]:
                v = s(lambda: getattr(mission, attr))
                return v if isinstance(v, (int, float)) and not isinstance(v, bool) else None
            row["mission"] = {
                "name": s(lambda: mission.name),
                "map": s(lambda: mission.map),
                "filename": s(lambda: mission.filename),
                "date": jsonable(s(lambda: mission.date)),
                "start_time": num("start_time"),
                "mission_time": num("mission_time"),
                "real_time": num("real_time"),
            }
        return row

    def _node_row(self, name: str, node: Any) -> dict:
        s = self._safe
        if node is None:
            return {"name": name, "online": False}
        raw = s(lambda: node.instances, None) or {}
        instances = list(raw.values()) if isinstance(raw, dict) else list(raw)
        return {
            "name": name,
            "online": True,
            "master": bool(s(lambda: node.master, False)),
            "dcs_version": s(lambda: node.dcs_version),
            "dcs_branch": s(lambda: node.dcs_branch),
            "bot_version": s(lambda: node.bot_version),
            "is_remote": bool(s(lambda: node.is_remote, False)),
            "instances": [{
                "name": s(lambda i=i: i.name),
                "server": s(lambda i=i: i.server.name),
                "dcs_port": s(lambda i=i: i.dcs_port),
                "webgui_port": s(lambda i=i: i.webgui_port),
            } for i in instances],
        }

    async def fleet(self, request: Request):
        bot = self.cog.bot
        servers = [self._server_row(sv) for sv in list(getattr(bot, "servers", {}).values())]
        nodes: list[dict] = []
        own = getattr(bot, "node", None)
        all_nodes = self._safe(lambda: dict(own.all_nodes), None) if own is not None else None
        if all_nodes:
            nodes = [self._node_row(n, v) for n, v in all_nodes.items()]
        elif own is not None:
            nodes = [self._node_row(getattr(own, "name", "node"), own)]
        return {"generated": datetime.now(timezone.utc).isoformat(), "nodes": nodes, "servers": servers}

    # ---- the bot's audit trail --------------------------------------------------

    async def audit(self, request: Request, limit: int = 200, q: str = "", before: Optional[str] = None):
        if not may_use(caller_roles(_header(request, "X-Fowl-Roles")), "admin"):
            return self._err(403, "the audit trail is for full admins")
        limit = max(1, min(int(limit or 200), AUDIT_LIMIT_MAX))
        pool = getattr(self.cog, "apool", None) or getattr(getattr(self.cog, "bot", None), "apool", None)
        if pool is None:
            return self._err(503, "the bot has no database pool")
        where, args = [], []
        if q:
            where.append("(event ILIKE %s OR coalesce(server_name, '') ILIKE %s OR "
                         "coalesce(ucid, '') ILIKE %s)")
            needle = "%" + q.replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_") + "%"
            args += [needle, needle, needle]
        if before:
            where.append("time < %s")
            args.append(before)
        sql = ("SELECT * FROM audit" + (" WHERE " + " AND ".join(where) if where else "")
               + " ORDER BY time DESC LIMIT %s")
        args.append(limit)
        try:
            async with pool.connection() as conn:
                cursor = await conn.execute(sql, args)
                cols = [c.name for c in cursor.description]
                rows = [dict(zip(cols, r)) for r in await cursor.fetchall()]
        except Exception as ex:  # noqa: BLE001
            return self._err(500, f"reading the audit table: {ex}")
        return {"rows": [jsonable(r) for r in rows], "limit": limit}

    # ---- mission files ----------------------------------------------------------------

    async def mission_download(self, request: Request, server: str = "", mission: str = ""):
        roles = caller_roles(_header(request, "X-Fowl-Roles"))
        if not may_use(roles, "ops"):
            return self._err(403, "downloading a mission needs the ops role")
        layer = self.layer()
        if layer is None:
            return self._err(501, self._layer_error or "no action layer")
        if not server or not mission:
            return self._err(400, "server and mission are required")
        target: Any = int(mission) if mission.isdigit() else mission
        result = await layer.read_action("download_mission", self._ctx(layer, request),
                                         server_name=server, mission=target)
        if not getattr(result, "success", False):
            return self._err(404, plain(str(getattr(result, "message", "") or "no such mission")))
        from fastapi.responses import Response
        filename = re.sub(r'[^A-Za-z0-9 ._()\-]', "_", str(getattr(result, "filename", "") or "mission.miz"))
        return Response(content=bytes(getattr(result, "content", b"") or b""),
                        media_type="application/octet-stream",
                        headers={"Content-Disposition": f'attachment; filename="{filename}"'})

    async def mission_upload(self, request: Request, server: str = "", name: str = "",
                             force: bool = False, load: bool = False, autostart: bool = False):
        roles = caller_roles(_header(request, "X-Fowl-Roles"))
        if not may_use(roles, "ops"):
            return self._err(403, "uploading a mission needs the ops role")
        layer = self.layer()
        if layer is None:
            return self._err(501, self._layer_error or "no action layer")
        if not server or not name:
            return self._err(400, "server and name are required")
        data = await request.body()
        if not data:
            return self._err(400, "the request has no file in it")
        if len(data) > UPLOAD_MAX_BYTES:
            return self._err(413, f"a mission may be at most {UPLOAD_MAX_BYTES // (1024 * 1024)} MB")
        if load and self.ops is not None:
            sv = self.cog.bot.servers.get(server)
            blocked = self.ops._dcs_blocked(sv) if sv is not None else None
            if blocked:
                return self._err(409, blocked)
        result = await layer.call_action("upload_mission", self._ctx(layer, request), server_name=server,
                                         name=name, source=data, force=bool(force), autostart=bool(autostart),
                                         load=bool(load))
        return result_doc(result)


    # ---- music track upload ---------------------------------------------------------

    async def music_upload(self, request: Request, name: str = "", folder: str = "", node: str = ""):
        role = self.role_of("add_playlist_track")
        if not may_use(caller_roles(_header(request, "X-Fowl-Roles")), role):
            return self._err(403, f"uploading music needs the {role} role")
        layer = self.layer()
        if layer is None:
            return self._err(501, self._layer_error or "no action layer")
        if not name:
            return self._err(400, "name is required")
        data = await request.body()
        if not data:
            return self._err(400, "the request has no file in it")
        result = await layer.call_action("upload_music_track", self._ctx(layer, request), folder=folder,
                                         name=name, node_name=node, source=data)
        return result_doc(result)

    # ---- a player screenshot's image -----------------------------------------------------

    async def screenshot(self, request: Request, server: str = "", key: str = ""):
        """The PNG screenshot_player took, read from that server's own web GUI
        (as the bot's console and Discord command read it)."""
        role = self.role_of("screenshot_player")
        if not may_use(caller_roles(_header(request, "X-Fowl-Roles")), role):
            return self._err(403, f"screenshots need the {role} role")
        sv = self.cog.bot.servers.get(server)
        if sv is None:
            return self._err(404, f"no server {server!r}")
        if not SCREENSHOT_KEY.match(key or ""):
            return self._err(400, "not a screenshot key")
        port = self._safe(lambda: int(sv.instance.webgui_port), 0)
        if not port:
            return self._err(404, f"{server} has no web GUI port")
        try:
            data = await sv.node.read_file(f"http://127.0.0.1:{port}{key}")
        except Exception as ex:  # noqa: BLE001
            return self._err(404, f"the screenshot could not be read: {ex}")
        if not data:
            return self._err(404, "the screenshot is gone")
        from fastapi.responses import Response
        return Response(content=bytes(data), media_type="image/png", headers={"Cache-Control": "no-store"})

    # ---- one server's overview: weather, extensions, schedule ---------------------------

    async def server_overview(self, request: Request, name: str = ""):
        roles = caller_roles(_header(request, "X-Fowl-Roles"))
        if not (may_use(roles, "ops") or may_use(roles, "moderation")):
            return self._err(403, "needs the ops or moderation role")
        sv = self.cog.bot.servers.get(name)
        if sv is None:
            return self._err(404, f"no server {name!r}")
        out: dict = {"name": name, "weather": None, "clouds": None, "visibility": None, "fog": None,
                     "extensions": [], "schedule": None, "restart": None}
        mission = self._safe(lambda: sv.current_mission)
        if mission is not None:
            out["weather"] = jsonable(self._safe(lambda: mission.weather))
            out["clouds"] = jsonable(self._safe(lambda: mission.clouds))
        status = getattr(self._safe(lambda: sv.status), "name", "")
        if status in ("RUNNING", "PAUSED"):
            try:
                fog = await asyncio.wait_for(sv.send_to_dcs_sync({"command": "getFog"}), 5)
                if isinstance(fog, dict):
                    out["visibility"] = jsonable(fog.get("visibility"))
                    out["fog"] = jsonable(fog)
            except Exception:  # noqa: BLE001 -- no live fog is a state, not an error
                pass
        try:
            exts = await asyncio.wait_for(sv.render_extensions(), 5)
            out["extensions"] = jsonable([e for e in (exts or []) if isinstance(e, dict)])
        except Exception:  # noqa: BLE001
            pass
        cogs = self._safe(lambda: self.cog.bot.cogs, {}) or {}
        sched = cogs.get("Scheduler") if isinstance(cogs, dict) else None
        if sched is not None:
            cfg = self._safe(lambda: sched.get_config(sv), {}) or {}
            if isinstance(cfg, dict):
                out["schedule"] = jsonable(cfg.get("schedule"))
                out["restart"] = jsonable(cfg.get("restart"))
        return out

    # ---- the live stream -----------------------------------------------------------------

    def _fleet_doc(self) -> dict:
        bot = self.cog.bot
        servers = [self._server_row(sv) for sv in list(getattr(bot, "servers", {}).values())]
        own = getattr(bot, "node", None)
        all_nodes = self._safe(lambda: dict(own.all_nodes), None) if own is not None else None
        if all_nodes:
            nodes = [self._node_row(n, v) for n, v in all_nodes.items()]
        elif own is not None:
            nodes = [self._node_row(getattr(own, "name", "node"), own)]
        else:
            nodes = []
        return {"nodes": nodes, "servers": servers}

    def stream_events(self, request, tick: float = STREAM_TICK, ping: float = STREAM_PING):
        """The event stream's body: `fleet` (the console/fleet document)
        whenever it changes, looked at every `tick` seconds, and a comment line
        every `ping` seconds so nothing in between closes it."""

        async def events():
            self._streams += 1
            last, quiet = None, 0.0
            try:
                while True:
                    if await self._disconnected(request):
                        return
                    doc = self._fleet_doc()
                    digest = hashlib.sha256(json.dumps(doc, sort_keys=True, default=str).encode()).hexdigest()
                    if digest != last:
                        last, quiet = digest, 0.0
                        doc["generated"] = datetime.now(timezone.utc).isoformat()
                        yield f"event: fleet\ndata: {json.dumps(doc, default=str)}\n\n"
                    elif quiet >= ping:
                        quiet = 0.0
                        yield ": ping\n\n"
                    await asyncio.sleep(tick)
                    quiet += tick
            finally:
                self._streams -= 1

        return events()

    async def stream(self, request: Request):
        roles = caller_roles(_header(request, "X-Fowl-Roles"))
        if not (may_use(roles, "ops") or may_use(roles, "moderation")):
            return self._err(403, "needs the ops or moderation role")
        if self._streams >= STREAM_MAX:
            return self._err(503, f"{STREAM_MAX} live streams are open already -- the page polls instead")
        from fastapi.responses import StreamingResponse
        return StreamingResponse(self.stream_events(request), media_type="text/event-stream",
                                 headers={"Cache-Control": "no-store", "X-Accel-Buffering": "no"})

    @staticmethod
    async def _disconnected(request) -> bool:
        try:
            return bool(await request.is_disconnected())
        except Exception:  # noqa: BLE001
            return False

    # ---- the Access tab -------------------------------------------------------------------

    async def access_get(self, request: Request):
        over = self.overrides()
        return {"actions": [{"name": n, "kind": k, "default": r, "role": self.role_of(n, over),
                             "locked": n in LOCKED_ADMIN, "plugin": _PLUGIN_OF.get(n, "mission")}
                            for n, (k, r) in ACTIONS.items()],
                "editable": may_use(caller_roles(_header(request, "X-Fowl-Roles")), "admin")}

    async def access_set(self, request: Request):
        """{action, role} -- role null puts the action back to its default."""
        if not may_use(caller_roles(_header(request, "X-Fowl-Roles")), "admin"):
            return self._err(403, "only a full admin changes who may do what")
        body = await self._json(request)
        name = str(body.get("action") or "")
        role = body.get("role")
        if name not in ACTIONS:
            return self._err(404, f"no such action {name!r}")
        if name in LOCKED_ADMIN:
            return self._err(403, f"{name} stays with full admins")
        if role is not None and role not in ("ops", "moderation", "admin"):
            return self._err(400, "role must be ops, moderation, admin or null")
        over = self.overrides()
        if role is None or role == ACTIONS[name][1]:
            over.pop(name, None)
        else:
            over[name] = role
        path = self._access_path()
        os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
        tmp = path + ".tmp"
        with open(tmp, "w", encoding="utf-8") as fh:
            json.dump(over, fh, indent=1, sort_keys=True)
        os.replace(tmp, path)
        now = self.role_of(name, over)
        who = _clean_name(_header(request, "X-Fowl-Actor"), "dashboard")
        self.log.info(f"FowlEngine/console: {name} now needs {now} (by {who})")
        await self._audit(f"[web:fowlengine/{_clean_name(_header(request, 'X-Fowl-Subject'), who)}] "
                          f"set {name} to the {now} role")
        return {"ok": True, "action": name, "role": now}

    # ---- the bot's config files (Settings and Presets) ------------------------------------

    def _bot_config(self):
        if self._config is None:
            from .botconfig import BotConfig
            node = getattr(self.cog.bot, "node", None)
            self._config = BotConfig(getattr(node, "config_dir", None) or "config")
        return self._config

    def _config_gate(self, request):
        if not may_use(caller_roles(_header(request, "X-Fowl-Roles")), "admin"):
            return self._err(403, "the bot's config files are for full admins")
        return None

    def _config_err(self, ex):
        from .botconfig import ConfigError
        if isinstance(ex, ConfigError):
            return self._err(ex.status, ex.message)
        self.log.exception("FowlEngine/console: config")
        return self._err(500, str(ex))

    async def config_files(self, request: Request):
        refused = self._config_gate(request)
        if refused is not None:
            return refused
        try:
            return {"files": self._bot_config().files()}
        except Exception as ex:  # noqa: BLE001
            return self._config_err(ex)

    async def config_read(self, request: Request, path: str = ""):
        refused = self._config_gate(request)
        if refused is not None:
            return refused
        try:
            return await asyncio.get_running_loop().run_in_executor(None, self._bot_config().read, path)
        except Exception as ex:  # noqa: BLE001
            return self._config_err(ex)

    async def config_validate(self, request: Request):
        refused = self._config_gate(request)
        if refused is not None:
            return refused
        body = await self._json(request)
        try:
            errors = await asyncio.get_running_loop().run_in_executor(
                None, self._bot_config().validate, str(body.get("path") or ""), body)
        except Exception as ex:  # noqa: BLE001
            return self._config_err(ex)
        return {"ok": not errors, "errors": errors}

    async def config_write(self, request: Request):
        refused = self._config_gate(request)
        if refused is not None:
            return refused
        body = await self._json(request)
        rel = str(body.get("path") or "")
        try:
            out = await asyncio.get_running_loop().run_in_executor(None, self._bot_config().save, rel, body)
        except Exception as ex:  # noqa: BLE001
            return self._config_err(ex)
        who = _clean_name(_header(request, "X-Fowl-Actor"), "dashboard")
        if out.get("changed"):
            self.log.info(f"FowlEngine/console: {rel} saved by {who} (backup {out.get('backup')})")
            await self._audit(f"[web:fowlengine/{_clean_name(_header(request, 'X-Fowl-Subject'), who)}] "
                              f"changed config {rel}")
            if out.get("applies") == "reload":
                out["reloaded"] = await self._reload_plugin(rel.split("/")[-1].split(".")[0])
        return out

    async def config_service(self, request: Request):
        refused = self._config_gate(request)
        if refused is not None:
            return refused
        body = await self._json(request)
        try:
            out = self._bot_config().set_service_enabled(str(body.get("name") or ""), bool(body.get("enabled")))
        except Exception as ex:  # noqa: BLE001
            return self._config_err(ex)
        if out.get("changed"):
            who = _clean_name(_header(request, "X-Fowl-Subject"), "dashboard")
            await self._audit(f"[web:fowlengine/{who}] "
                              f"{'enabled' if body.get('enabled') else 'disabled'} service {body.get('name')}")
        return out

    @staticmethod
    async def _json(request) -> dict:
        try:
            body = await request.json()
            return body if isinstance(body, dict) else {}
        except Exception:  # noqa: BLE001
            return {}

    async def _reload_plugin(self, name: str) -> Optional[bool]:
        """Reload the plugin whose YAML was saved, as the bot's console does.
        None = this bot can't (the change applies at its next restart)."""
        reload = getattr(self.cog.bot, "reload_plugin", None)
        if not callable(reload):
            return None
        try:
            res = reload(name)
            if asyncio.iscoroutine(res):
                res = await res
            return res is not False
        except Exception as ex:  # noqa: BLE001
            self.log.warning(f"FowlEngine/console: reloading plugin {name} failed: {ex}")
            return False

    async def _audit(self, event: str) -> None:
        """A line in the bot's audit trail, beside the actions' own."""
        audit = getattr(self.cog.bot, "audit", None)
        if not callable(audit):
            return
        try:
            res = audit(event)
            if asyncio.iscoroutine(res):
                await res
        except Exception:  # noqa: BLE001 -- an audit that can't be written must not undo the save
            self.log.debug("FowlEngine/console: audit entry not written", exc_info=True)
