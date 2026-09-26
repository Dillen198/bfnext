"""
The Ops API: what the dashboard's OPS page talks to (through bfdb).

Routes are added to DCSServerBot's own WebService (the FastAPI app the RestAPI
plugin also lives on) under

    <restapi prefix>/fowlengine/ops/...

and every one needs the same X-API-Key bfdb already sends to the RestAPI
(`bfdb.dcsserverbot_api_key` in fowlengine.yaml). bfdb exposes them to a
logged-in dashboard admin as /api/admin/ops/..., so the browser never sees the
key and nothing here is reachable without both a Discord admin login and the
bot's key.

Why here and not in bfdb: the bot owns the processes (DCS, bfdb, the netidx
resolver) and the files (staging dirs, fowlengine.yaml), and it keeps running
while bfdb restarts -- which is exactly when an admin wants to watch.

  GET  status                    everything the OPS page shows, one call
  POST bfdb/restart              restart bfdb (answers first, then restarts)
  POST bot/restart               restart the bot process (the service / run.cmd
                                 brings it back; DCS keeps running)
  POST update/check              look for a new release now (and stage it)
  POST update/settings           {changes: {...}} | {reset: true}
  POST update/apply              {target: bfdb|dll|bftools, server?}
  POST update/rollback           {target: bfdb|dll, server?}
  POST update/unmark             {tag}  -- let a rolled-back release be offered again
  POST stage/cancel              {name, server?}
  GET  config                    fowlengine.yaml with secrets masked
  POST config                    {yaml, base_mtime, restart_bfdb?}
  GET  logs?which=...&lines=N    tail of bot | service | bfdb | bfdb_boot | netidx
  GET  issues?closed=0|1         the log analyzer's issue list
  POST issues/scan               scan the logs now
  POST issues/status             {id | ids: [...], status: open|acknowledged|ignored|fixed, note?}
  POST issues/clear              {which: closed|all} | {ids: [...]}  -- delete
  GET  issues/report?format=md   the Markdown report (what Claude reads)
  GET  archive                   the persistent log archive: sources x days
  GET  archive/read?source=&date=&lines=&grep=&format=json|text
"""
from __future__ import annotations

import asyncio
import io
import json
import os
import platform
import shutil
import socket
import subprocess
import sys
import time
from datetime import datetime, timezone
from typing import Any, Optional
from urllib.parse import urlparse

try:  # FastAPI resolves handler annotations from this module's globals
    from fastapi import Request
except Exception:  # noqa: BLE001 - imported outside the bot (unit tests)
    Request = Any  # type: ignore[misc,assignment]

__all__ = ["OpsApi", "mask_secrets", "unmask_secrets", "SECRET_MASK", "is_secret_key",
           "tail_lines", "validate_plugin_yaml"]

SECRET_MASK = "__SECRET__"
_SECRET_HINTS = ("password", "secret", "api_key", "apikey", "token", "webhook", "llm_key")
CONFIG_BACKUPS_KEEP = 20
LOG_FILES = ("bot", "service", "bfdb", "bfdb_boot", "netidx")


# ---- secrets in fowlengine.yaml ------------------------------------------------

def is_secret_key(key: Any) -> bool:
    k = str(key).lower()
    return any(h in k for h in _SECRET_HINTS)


def mask_secrets(node: Any) -> int:
    """Replace every non-empty string under a secret-looking key with the mask,
    in place (works on ruamel CommentedMap/Seq as on plain dicts/lists).
    Returns how many were masked."""
    n = 0
    if isinstance(node, dict):
        for k in list(node.keys()):
            v = node[k]
            if is_secret_key(k) and isinstance(v, str) and v.strip():
                node[k] = SECRET_MASK
                n += 1
            else:
                n += mask_secrets(v)
    elif isinstance(node, list):
        for item in node:
            n += mask_secrets(item)
    return n


def unmask_secrets(new: Any, old: Any, path: str = "") -> None:
    """Put the real values back wherever the edited document still carries the
    mask. Raises ValueError if a masked value has no counterpart in the old
    document (the key was renamed/moved -- type the secret in again)."""
    if isinstance(new, dict):
        for k in list(new.keys()):
            v = new[k]
            here = f"{path}.{k}" if path else str(k)
            old_v = old.get(k) if isinstance(old, dict) else None
            if v == SECRET_MASK:
                if not isinstance(old_v, str) or old_v == SECRET_MASK:
                    raise ValueError(f"{here} still says {SECRET_MASK} but there is no saved value "
                                     f"for it -- type the real value in")
                new[k] = old_v
            else:
                unmask_secrets(v, old_v, here)
    elif isinstance(new, list):
        for i, item in enumerate(new):
            old_i = old[i] if isinstance(old, list) and i < len(old) else None
            unmask_secrets(item, old_i, f"{path}[{i}]")


def validate_plugin_yaml(doc: Any) -> list[str]:
    """Structural checks the plugin actually depends on. Returns problems."""
    from .autoupdate import APPLY_POLICIES

    problems = []
    if not isinstance(doc, dict):
        return ["the file must be a YAML mapping (key: value) at the top level"]
    default = doc.get("DEFAULT")
    if default is None:
        problems.append("there is no DEFAULT: section -- every server-wide setting lives there")
        return problems
    if not isinstance(default, dict):
        return ["DEFAULT: must be a mapping"]
    bfdb = default.get("bfdb")
    if bfdb is not None and not isinstance(bfdb, dict):
        problems.append("DEFAULT.bfdb must be a mapping")
    elif isinstance(bfdb, dict):
        if bfdb.get("manage") and not bfdb.get("exe"):
            problems.append("DEFAULT.bfdb.manage is true but bfdb.exe is not set")
        if bfdb.get("manage") and not bfdb.get("home"):
            problems.append("DEFAULT.bfdb.manage is true but bfdb.home is not set")
        inst = bfdb.get("instances")
        if inst is not None:
            if not isinstance(inst, list):
                problems.append("DEFAULT.bfdb.instances must be a list")
            else:
                ids = [i.get("id") for i in inst if isinstance(i, dict)]
                if any(not i for i in ids):
                    problems.append("every bfdb.instances entry needs an id")
                dupes = {i for i in ids if i and ids.count(i) > 1}
                if dupes:
                    problems.append(f"duplicate bfdb.instances ids: {', '.join(sorted(dupes))}")
    au = default.get("autoupdate")
    if au is not None:
        if not isinstance(au, dict):
            problems.append("DEFAULT.autoupdate must be a mapping")
        else:
            for key in ("apply", "bfdb_apply"):
                if au.get(key) and au[key] not in APPLY_POLICIES:
                    problems.append(f"autoupdate.{key} must be one of {', '.join(APPLY_POLICIES)}")
            if au.get("source") and au["source"] not in ("github", "folder"):
                problems.append("autoupdate.source must be github or folder")
            if au.get("source") == "folder" and not au.get("folder"):
                problems.append("autoupdate.source is folder but autoupdate.folder is empty")
    for key in ("gci",):
        if default.get(key) is not None and not isinstance(default.get(key), dict):
            problems.append(f"DEFAULT.{key} must be a mapping")
    return problems


# ---- log tails ------------------------------------------------------------------

def tail_lines(path: str, lines: int = 200, max_bytes: int = 2 * 1024 * 1024) -> list[str]:
    """The last `lines` lines of a (possibly huge, possibly being written) file."""
    lines = max(1, min(int(lines), 5000))
    try:
        size = os.path.getsize(path)
        with open(path, "rb") as fh:
            start = max(0, size - max_bytes)
            fh.seek(start)
            data = fh.read()
    except OSError:
        return []
    text = data.decode("utf-8", "replace")
    out = text.splitlines()
    if start > 0 and out:
        out = out[1:]  # first line is probably cut in half
    return out[-lines:]


# ---- host facts (Windows first, harmless elsewhere) ----------------------------

def _run(cmd: list[str], timeout: float = 8.0) -> str:
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout,
                           creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
        return (p.stdout or "") + (p.stderr or "")
    except Exception:  # noqa: BLE001
        return ""


def _service_info(name: str) -> dict:
    if os.name != "nt":
        return {"name": name, "installed": False, "note": "not Windows"}
    q = _run(["sc", "query", name])
    if "1060" in q or not q.strip():
        return {"name": name, "installed": False}
    info: dict = {"name": name, "installed": True}
    for line in q.splitlines():
        if "STATE" in line:
            info["state"] = line.split()[-1]
    qc = _run(["sc", "qc", name])
    for line in qc.splitlines():
        line = line.strip()
        if line.startswith("START_TYPE"):
            info["start_type"] = " ".join(line.split()[3:]) or line
        elif line.startswith("SERVICE_START_NAME"):
            info["account"] = line.split(":", 1)[-1].strip()
    fl = _run(["sc", "qfailure", name])
    info["restart_on_failure"] = "RESTART" in fl.upper()
    return info


def _running_as_service() -> bool:
    try:
        import psutil
        p = psutil.Process()
        for _ in range(6):
            p = p.parent()
            if p is None:
                return False
            n = (p.name() or "").lower()
            if n in ("fowlenginemanager.exe", "nssm.exe", "services.exe", "winsw.exe", "srvany.exe"):
                return True
    except Exception:  # noqa: BLE001
        pass
    return False


def _reg_value(path: str, name: str):
    if os.name != "nt":
        return None
    try:
        import winreg
        with winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, path) as k:
            return winreg.QueryValueEx(k, name)[0]
    except OSError:
        return None


def _reg_key_exists(path: str) -> bool:
    if os.name != "nt":
        return False
    try:
        import winreg
        with winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, path):
            return True
    except OSError:
        return False


def _unexpected_shutdowns(limit: int = 5) -> list[dict]:
    """Kernel-Power 41 (lost power / hard reset / bugcheck) and EventLog 6008
    (previous shutdown was unexpected), newest first -- what a BSOD or a power
    cut leaves behind. Read-only query of the System log."""
    if os.name != "nt":
        return []
    out = _run(["wevtutil", "qe", "System", "/q:*[System[(EventID=41 or EventID=6008 or EventID=1001)]]",
                f"/c:{limit * 3}", "/rd:true", "/f:text"], timeout=15)
    events, cur = [], {}
    for line in out.splitlines():
        s = line.strip()
        if s.startswith("Event["):
            if cur:
                events.append(cur)
            cur = {}
        elif s.startswith("Date:"):
            cur["date"] = s.split(":", 1)[1].strip()
        elif s.startswith("Event ID:"):
            cur["id"] = s.split(":", 1)[1].strip()
        elif s.startswith("Source:"):
            cur["source"] = s.split(":", 1)[1].strip()
    if cur:
        events.append(cur)
    keep = []
    for e in events:
        eid = e.get("id")
        if eid == "1001" and "WER" not in (e.get("source") or "") and "BugCheck" not in (e.get("source") or ""):
            continue  # 1001 is shared by other sources; only the bugcheck one matters
        e["kind"] = {"41": "hard reset / power loss", "6008": "unexpected shutdown",
                     "1001": "bugcheck (BSOD)"}.get(eid, eid)
        keep.append(e)
    return keep[:limit]


def host_info(service_name: str, home: Optional[str]) -> dict:
    info: dict = {
        "hostname": socket.gethostname(),
        "os": f"{platform.system()} {platform.release()} ({platform.version()})",
        "python": sys.version.split()[0],
    }
    try:
        import psutil
        boot = psutil.boot_time()
        info["boot_time"] = datetime.fromtimestamp(boot, timezone.utc).isoformat()
        info["uptime_secs"] = int(time.time() - boot)
        info["cpu_percent"] = psutil.cpu_percent(interval=None)
        vm = psutil.virtual_memory()
        info["memory"] = {"total": vm.total, "available": vm.available, "percent": vm.percent}
    except Exception:  # noqa: BLE001
        pass
    if home:
        try:
            du = shutil.disk_usage(home)
            info["disk"] = {"path": os.path.splitdrive(home)[0] or home, "total": du.total, "free": du.free}
        except OSError:
            pass
    info["service"] = _service_info(service_name)
    info["service"]["this_process_is_service"] = _running_as_service()
    ar = _reg_value(r"SYSTEM\CurrentControlSet\Control\CrashControl", "AutoReboot")
    info["auto_reboot_on_bsod"] = None if ar is None else bool(ar)
    info["reboot_pending"] = (
        _reg_key_exists(r"SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update\RebootRequired")
        or _reg_key_exists(r"SOFTWARE\Microsoft\Windows\CurrentVersion\Component Based Servicing\RebootPending"))
    info["unexpected_shutdowns"] = _unexpected_shutdowns()
    return info


# ---- the API ----------------------------------------------------------------------

class OpsApi:
    def __init__(self, cog):
        self.cog = cog
        self.log = cog.log
        self.app = None
        self.router = None
        self.prefix = ""
        self._host_cache: Optional[tuple[float, dict]] = None
        self._started = time.time()

    # ---- config ---------------------------------------------------------

    def _cfg(self) -> dict:
        return self.cog.get_config() or {}

    def _api_key(self) -> Optional[str]:
        cfg = self._cfg()
        ops = cfg.get("ops_api") or {}
        key = ops.get("api_key") or (cfg.get("bfdb") or {}).get("dcsserverbot_api_key")
        return str(key) if key else None

    def _prefix(self) -> str:
        cfg = self._cfg()
        ops = cfg.get("ops_api") or {}
        base = ops.get("prefix")
        if base is None:
            url = (cfg.get("bfdb") or {}).get("dcsserverbot_url") or ""
            base = urlparse(url).path if url else ""
        base = "/" + str(base or "").strip("/")
        return (base.rstrip("/") + "/fowlengine/ops").replace("//", "/")

    @property
    def service_name(self) -> str:
        """The Windows service that keeps this bot up: `ops_api.service_name`
        if set, else Fowl Engine Manager's "FowlEngine" when it's installed,
        else the NSSM "DCSServerBot" from deploy/windows-service."""
        configured = (self._cfg().get("ops_api") or {}).get("service_name")
        if configured:
            return str(configured)
        if os.name == "nt":
            q = _run(["sc", "query", "FowlEngine"])
            if q.strip() and "1060" not in q:
                return "FowlEngine"
        return "DCSServerBot"

    # ---- registration -------------------------------------------------------

    async def register(self) -> None:
        if (self._cfg().get("ops_api") or {}).get("enabled") is False:
            self.log.info("FowlEngine/ops: ops_api.enabled is false -- no OPS routes")
            return
        key = self._api_key()
        if not key:
            self.log.warning("FowlEngine/ops: no API key (bfdb.dcsserverbot_api_key or ops_api.api_key) -- "
                             "the dashboard OPS page is disabled rather than exposed without one")
            return
        try:
            from core import ServiceRegistry
            from services.webservice import WebService
        except Exception as ex:  # noqa: BLE001
            self.log.warning(f"FowlEngine/ops: WebService unavailable ({ex}) -- OPS page disabled")
            return
        ws = None
        for _ in range(20):
            ws = ServiceRegistry.get(WebService)
            if ws and ws.is_running() and ws.app:
                break
            await asyncio.sleep(1)
        if not ws or not ws.app:
            self.log.warning("FowlEngine/ops: DCSServerBot's WebService is not running (configure "
                             "services/webservice.yaml) -- OPS page disabled")
            return
        self.mount(ws.app)

    def mount(self, app) -> None:
        """Add the OPS routes to a FastAPI app (the WebService's, or a test app)."""
        from fastapi import APIRouter, Depends, HTTPException
        from fastapi.security import APIKeyHeader

        header = APIKeyHeader(name="X-API-Key", auto_error=False)

        def check_key(got: Optional[str] = Depends(header)):
            import hmac
            want = self._api_key() or ""
            if not got or not want or not hmac.compare_digest(str(got), want):
                raise HTTPException(status_code=403, detail="Invalid API Key")

        self.prefix = self._prefix()
        r = APIRouter(prefix=self.prefix, dependencies=[Depends(check_key)])
        r.add_api_route("/status", self.status, methods=["GET"])
        r.add_api_route("/bfdb/restart", self.bfdb_restart, methods=["POST"])
        r.add_api_route("/bot/restart", self.bot_restart, methods=["POST"])
        r.add_api_route("/update/check", self.update_check, methods=["POST"])
        r.add_api_route("/update/settings", self.update_settings, methods=["POST"])
        r.add_api_route("/update/apply", self.update_apply, methods=["POST"])
        r.add_api_route("/update/rollback", self.update_rollback, methods=["POST"])
        r.add_api_route("/update/unmark", self.update_unmark, methods=["POST"])
        r.add_api_route("/stage/cancel", self.stage_cancel, methods=["POST"])
        r.add_api_route("/config", self.config_get, methods=["GET"])
        r.add_api_route("/config", self.config_post, methods=["POST"])
        r.add_api_route("/logs", self.logs, methods=["GET"])
        r.add_api_route("/issues", self.issues_list, methods=["GET"])
        r.add_api_route("/issues/scan", self.issues_scan, methods=["POST"])
        r.add_api_route("/issues/status", self.issues_status, methods=["POST"])
        r.add_api_route("/issues/clear", self.issues_clear, methods=["POST"])
        r.add_api_route("/issues/report", self.issues_report, methods=["GET"])
        r.add_api_route("/archive", self.archive_index, methods=["GET"])
        r.add_api_route("/archive/read", self.archive_read, methods=["GET"])
        self.app = app
        self.router = r
        self.app.include_router(r)
        self.log.info(f"FowlEngine/ops: OPS API on the WebService at {self.prefix}/*")

    def unregister(self) -> None:
        if self.app and self.router:
            for route in list(self.router.routes):
                try:
                    self.app.routes.remove(route)
                except ValueError:
                    pass
            # FastAPI copies the router's routes into the app with the prefix
            # applied, so also drop anything still under our prefix
            self.app.router.routes = [rt for rt in self.app.router.routes
                                      if not str(getattr(rt, "path", "")).startswith(self.prefix + "/")]
        self.app = None
        self.router = None

    # ---- helpers -------------------------------------------------------------

    @staticmethod
    def _ok(message: str, **extra) -> dict:
        return {"ok": True, "message": message, **extra}

    @staticmethod
    def _err(status: int, message: str):
        from fastapi.responses import JSONResponse
        return JSONResponse(status_code=status, content={"ok": False, "error": message})

    @staticmethod
    async def _body(request) -> dict:
        try:
            doc = await request.json()
            return doc if isinstance(doc, dict) else {}
        except Exception:  # noqa: BLE001
            return {}

    def _server(self, name: Optional[str]):
        if not name:
            return None
        return self.cog.bot.servers.get(name)

    async def _host(self) -> dict:
        now = time.time()
        if self._host_cache and now - self._host_cache[0] < 60:
            return self._host_cache[1]
        pm = self.cog.procman
        home = pm.home if pm else None
        data = await asyncio.get_running_loop().run_in_executor(
            None, host_info, self.service_name, home)
        self._host_cache = (now, data)
        return data

    async def _bfdb_version(self) -> Optional[dict]:
        import aiohttp
        pm = self.cog.procman
        url = pm.api_url if pm else (self._cfg().get("api_url") or "http://127.0.0.1:8880")
        try:
            async with aiohttp.ClientSession() as http:
                async with http.get(f"{url}/api/version", timeout=4) as r:
                    if r.status == 200:
                        return await r.json(content_type=None)
        except Exception:  # noqa: BLE001
            return None
        return None

    def _server_row(self, server) -> dict:
        from .procman import sha256_cached
        from .upload import engine_binaries, is_remote_node

        row: dict = {
            "name": server.name,
            "status": getattr(server.status, "name", str(server.status)),
            "node": getattr(getattr(server, "node", None), "name", None),
            "remote": is_remote_node(getattr(server, "node", None)),
            "kind": self.cog._instance_kind(server) if hasattr(self.cog, "_instance_kind") else "campaign",
        }
        try:
            row["players"] = len(server.get_active_players())
        except Exception:  # noqa: BLE001
            row["players"] = None
        rt = getattr(server, "restart_time", None)
        row["restart_time"] = rt.isoformat() if rt else None
        try:
            mission = getattr(server, "current_mission", None)
            row["mission"] = getattr(mission, "name", None)
            row["map"] = getattr(mission, "map", None)
        except Exception:  # noqa: BLE001
            pass
        try:
            b = engine_binaries(self.cog, server)
        except Exception as ex:  # noqa: BLE001
            row["error"] = str(ex)
            return row
        row["dll_name"] = b["dll_name"]
        row["dll_path"] = b["dll_path"]
        row["staging_dir"] = b["staging_dir"]
        row["has_bfbinaries"] = b["has_extension"]
        if not row["remote"]:
            row["dll_sha256"] = sha256_cached(b["dll_path"]) if b["dll_path"] else None
            try:
                row["dll_mtime"] = os.path.getmtime(b["dll_path"]) if b["dll_path"] else None
            except OSError:
                row["dll_mtime"] = None
            pm = self.cog.procman
            if pm:
                row["pending"] = pm.pending_info(b["dll_name"], b["staging_dir"] or None)
            home = getattr(getattr(server, "instance", None), "home", None)
            sidecar = os.path.join(home, "Logs", "bfnext-bflib-build.json") if home else None
            if b["dll_name"] == "bflib.dll" and sidecar and os.path.exists(sidecar):
                try:
                    with open(sidecar, encoding="utf-8") as fh:
                        row["loaded_build"] = json.load(fh)
                    row["loaded_build"]["seen_at"] = os.path.getmtime(sidecar)
                except (OSError, ValueError):
                    pass
            d = os.path.dirname(b["dll_path"]) if b["dll_path"] else ""
            base = os.path.basename(b["dll_path"]) if b["dll_path"] else ""
            try:
                row["backups"] = sorted((f for f in os.listdir(d) if f.startswith(f"{base}.backup-")),
                                        reverse=True)[:5] if d else []
            except OSError:
                row["backups"] = []
        return row

    # ---- handlers ------------------------------------------------------------

    async def status(self):
        pm = self.cog.procman
        upd = getattr(self.cog, "updater", None)
        servers = []
        for s in list(self.cog.bot.servers.values()):
            try:
                servers.append(await asyncio.get_running_loop().run_in_executor(None, self._server_row, s))
            except Exception as ex:  # noqa: BLE001
                servers.append({"name": s.name, "error": str(ex)})
        bftools = None
        if upd is not None:
            from .procman import sha256_cached
            p = upd.bftools_path()
            if p:
                bftools = {"path": p, "sha256": sha256_cached(p) if os.path.exists(p) else None,
                           "pending": pm.pending_info("bftools.exe") if pm else None}
        try:
            import importlib
            bot_version = getattr(importlib.import_module("version"), "__version__", None)
        except Exception:  # noqa: BLE001
            bot_version = None
        return {
            "generated": datetime.now(timezone.utc).isoformat(),
            "host": await self._host(),
            "bot": {
                "pid": os.getpid(),
                "plugin_started_at": datetime.fromtimestamp(self._started, timezone.utc).isoformat(),
                "dcsserverbot_version": bot_version,
                "node": getattr(self.cog.bot.node, "name", None),
                "cwd": os.getcwd(),
            },
            "bfdb": (await pm.process_info()) if pm else {"managed": False},
            "bfdb_build": await self._bfdb_version(),
            "servers": servers,
            "bftools": bftools,
            "updates": upd.status() if upd else None,
            "issues": (await asyncio.get_running_loop().run_in_executor(None, self._an().summary))
            if self._an() else None,
            "backups": pm.list_backups() if pm else None,
        }

    async def bfdb_restart(self):
        pm = self.cog.procman
        if not pm or not pm.enabled:
            return self._err(409, "bfdb.manage is off -- the bot isn't running bfdb")

        async def later():
            await asyncio.sleep(1.5)  # let this answer get back through bfdb first
            cfg = self.cog.get_config() or {}
            pm.reload_config(cfg)
            await pm.restart(self.cog._bfdb_admin_password)
            await self.cog.notify_ops("🔁 bfdb restarted from the dashboard OPS page.")

        asyncio.create_task(later())
        return self._ok("bfdb is restarting -- the dashboard will reconnect in a few seconds")

    async def bot_restart(self):
        async def later():
            await asyncio.sleep(1.5)
            await self.cog.notify_ops("🔁 Bot restarting (requested from the dashboard OPS page). "
                                      "DCS keeps running; bfdb restarts with the bot.")
            await self.cog.bot.node.restart()

        asyncio.create_task(later())
        return self._ok("the bot is restarting -- this page comes back when it has")

    async def update_check(self):
        upd = getattr(self.cog, "updater", None)
        if upd is None:
            return self._err(409, "the auto-updater is not loaded")
        res = await upd.check(reason="manual (dashboard)")
        return {"ok": bool(res.get("ok")), "message": res.get("message") or res.get("error"), "result": res}

    async def update_settings(self, request: Request):
        upd = getattr(self.cog, "updater", None)
        if upd is None:
            return self._err(409, "the auto-updater is not loaded")
        body = await self._body(request)
        try:
            if body.get("reset"):
                upd.clear_overrides()
                cfg = upd.cfg.public()
            else:
                cfg = upd.set_overrides(body.get("changes") or {})
        except ValueError as ex:
            return self._err(400, str(ex))
        self.cog.sync_update_tuning()
        return self._ok("saved", config=cfg)

    async def update_apply(self, request: Request):
        from core import Status

        upd = getattr(self.cog, "updater", None)
        pm = self.cog.procman
        body = await self._body(request)
        target = body.get("target")
        if target == "bfdb":
            if not pm or not pm.enabled:
                return self._err(409, "bfdb.manage is off")
            if not pm.pending_info("bfdb.exe"):
                return self._err(409, "no bfdb.exe is staged")

            async def later():
                await asyncio.sleep(1.5)
                await pm.restart(self.cog._bfdb_admin_password)
                await self.cog.notify_ops("🔁 Staged bfdb.exe applied from the dashboard OPS page.")

            asyncio.create_task(later())
            return self._ok("bfdb is restarting onto the staged build")
        if target == "bftools":
            if upd is None:
                return self._err(409, "the auto-updater is not loaded")
            await upd._apply_bftools()
            return self._ok("bftools.exe swap attempted -- see the history")
        if target == "dll":
            server = self._server(body.get("server"))
            if server is None:
                return self._err(404, f"no server named {body.get('server')!r}")
            row = self._server_row(server)
            if not row.get("pending") and not row.get("remote"):
                return self._err(409, f"nothing is staged for {server.name}")
            if server.status in (Status.RUNNING, Status.PAUSED):
                if upd is None:
                    return self._err(409, "the auto-updater is not loaded")
                asyncio.create_task(upd.restart_dcs(server, "staged engine applied from the dashboard OPS page"))
                return self._ok(f"{server.name}: DCS is restarting onto the staged {row.get('dll_name')}")
            if server.status in (Status.SHUTDOWN, Status.STOPPED, Status.UNREGISTERED):
                asyncio.create_task(server.startup())
                return self._ok(f"{server.name}: starting DCS -- the staged engine swaps in on the way up")
            return self._err(409, f"{server.name} is {server.status.name} -- try again in a moment")
        return self._err(400, "target must be bfdb, dll or bftools")

    async def update_rollback(self, request: Request):
        upd = getattr(self.cog, "updater", None)
        pm = self.cog.procman
        body = await self._body(request)
        target = body.get("target")
        why = "rolled back by hand from the dashboard OPS page"
        if target == "bfdb":
            if not pm or not pm.enabled:
                return self._err(409, "bfdb.manage is off")

            async def later():
                await asyncio.sleep(1.5)
                await self.cog.notify_ops(await pm.rollback_bfdb(self.cog._bfdb_admin_password, why))

            asyncio.create_task(later())
            return self._ok("bfdb is rolling back to the previous build")
        if target == "dll":
            server = self._server(body.get("server"))
            if server is None:
                return self._err(404, f"no server named {body.get('server')!r}")
            if upd is None:
                return self._err(409, "the auto-updater is not loaded")
            asyncio.create_task(upd.rollback_dll(server, why))
            return self._ok(f"{server.name}: rolling the engine back to its newest backup")
        return self._err(400, "target must be bfdb or dll")

    async def update_unmark(self, request: Request):
        upd = getattr(self.cog, "updater", None)
        body = await self._body(request)
        tag = str(body.get("tag") or "")
        if upd is None or not tag:
            return self._err(400, "need a tag")
        if not upd.unmark_bad(tag):
            return self._err(404, f"{tag} is not marked bad")
        return self._ok(f"{tag} may be offered again on the next check")

    async def stage_cancel(self, request: Request):
        pm = self.cog.procman
        body = await self._body(request)
        name = str(body.get("name") or "").lower()
        if not pm:
            return self._err(409, "procman is not loaded")
        if name in ("bfdb.exe", "bftools.exe"):
            return (self._ok(f"discarded staged {name}") if pm.cancel_pending(name)
                    else self._err(404, f"nothing staged for {name}"))
        server = self._server(body.get("server"))
        if server is None:
            return self._err(404, "need the server whose engine DLL to discard")
        from .upload import engine_binaries
        b = engine_binaries(self.cog, server)
        if pm.cancel_pending(b["dll_name"], b["staging_dir"] or None):
            return self._ok(f"discarded the staged {b['dll_name']} for {server.name}")
        return self._err(404, f"nothing staged for {server.name}")

    # ---- fowlengine.yaml ---------------------------------------------------------

    @property
    def config_path(self) -> str:
        return os.path.join(self.cog.node.config_dir, "plugins", "fowlengine.yaml")

    @property
    def config_backup_dir(self) -> str:
        return os.path.join(self.cog.node.config_dir, "backup", "fowlengine")

    @staticmethod
    def _yaml():
        from ruamel.yaml import YAML
        y = YAML()  # round-trip: keeps comments, order and quoting
        y.preserve_quotes = True
        y.width = 4096
        return y

    async def config_get(self):
        path = self.config_path
        try:
            with open(path, encoding="utf-8") as fh:
                raw = fh.read()
            st = os.stat(path)
        except OSError as ex:
            return self._err(404, f"cannot read {path}: {ex}")
        y = self._yaml()
        try:
            doc = y.load(raw)
            masked = mask_secrets(doc)
            buf = io.StringIO()
            y.dump(doc, buf)
            text = buf.getvalue()
        except Exception as ex:  # noqa: BLE001
            return self._err(500, f"{path} does not parse as YAML right now: {ex}")
        try:
            backups = sorted(os.listdir(self.config_backup_dir), reverse=True)[:CONFIG_BACKUPS_KEEP]
        except OSError:
            backups = []
        return {"path": path, "yaml": text, "mtime": st.st_mtime, "masked": masked,
                "mask": SECRET_MASK, "backups": backups}

    async def config_post(self, request: Request):
        body = await self._body(request)
        text = body.get("yaml")
        if not isinstance(text, str) or not text.strip():
            return self._err(400, "empty document")
        path = self.config_path
        try:
            st = os.stat(path)
            with open(path, encoding="utf-8") as fh:
                old_raw = fh.read()
        except OSError as ex:
            return self._err(404, f"cannot read {path}: {ex}")
        base = body.get("base_mtime")
        if base is not None and abs(float(base) - st.st_mtime) > 0.001:
            return self._err(409, "fowlengine.yaml changed on disk since you opened it -- reload and "
                                  "re-apply your edit")
        y = self._yaml()
        try:
            new_doc = y.load(text)
        except Exception as ex:  # noqa: BLE001
            return self._err(400, f"not valid YAML: {ex}")
        try:
            old_doc = y.load(old_raw)
        except Exception:  # noqa: BLE001
            old_doc = {}
        try:
            unmask_secrets(new_doc, old_doc)
        except ValueError as ex:
            return self._err(400, str(ex))
        problems = validate_plugin_yaml(new_doc)
        if problems:
            return self._err(400, "; ".join(problems))
        buf = io.StringIO()
        y.dump(new_doc, buf)
        out = buf.getvalue()
        try:
            os.makedirs(self.config_backup_dir, exist_ok=True)
            stamp = datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S")
            with open(os.path.join(self.config_backup_dir, f"fowlengine.yaml.{stamp}"), "w",
                      encoding="utf-8") as fh:
                fh.write(old_raw)
            olds = sorted(os.listdir(self.config_backup_dir), reverse=True)
            for stale in olds[CONFIG_BACKUPS_KEEP:]:
                try:
                    os.remove(os.path.join(self.config_backup_dir, stale))
                except OSError:
                    pass
            tmp = path + ".tmp"
            with open(tmp, "w", encoding="utf-8") as fh:
                fh.write(out)
            os.replace(tmp, path)
        except OSError as ex:
            return self._err(500, f"could not write {path}: {ex}")
        try:
            self.cog.reload_plugin_config()
        except Exception as ex:  # noqa: BLE001
            return self._err(500, f"saved, but the plugin could not reload it: {ex} -- the backup is in "
                                  f"{self.config_backup_dir}")
        msg = "saved and reloaded"
        if body.get("restart_bfdb"):
            pm = self.cog.procman
            if pm and pm.enabled:
                async def later():
                    await asyncio.sleep(1.5)
                    await pm.restart(self.cog._bfdb_admin_password)
                asyncio.create_task(later())
                msg += "; bfdb is restarting to pick up bfdb:/gci: changes"
        await self.cog.notify_ops("⚙️ fowlengine.yaml was edited from the dashboard OPS page "
                                  f"(backup kept in `{self.config_backup_dir}`).")
        return self._ok(msg, mtime=os.path.getmtime(path))

    # ---- logs ---------------------------------------------------------------------

    def _log_path(self, which: str) -> Optional[str]:
        pm = self.cog.procman
        home = pm.home if pm else ""
        if which == "bot":
            node = getattr(self.cog.bot.node, "name", "")
            return os.path.abspath(os.path.join("logs", f"dcssb-{node}.log"))
        if which == "service":
            return os.path.abspath("service.log")
        if which == "bfdb":
            lf = (self._cfg().get("bfdb") or {}).get("log_file")
            return os.path.expandvars(lf) if lf else (os.path.join(home, "Logs", "bfdb.log") if home else None)
        if which == "bfdb_boot":
            return os.path.join(home, "procman-bfdb-boot.log") if home else None
        if which == "netidx":
            return os.path.join(home, "procman-netidx.log") if home else None
        return None

    async def logs(self, which: str = "bot", lines: int = 300):
        if which not in LOG_FILES:
            return self._err(400, f"which must be one of {', '.join(LOG_FILES)}")
        path = self._log_path(which)
        if not path or not os.path.exists(path):
            return {"which": which, "path": path, "lines": [], "missing": True}
        out = await asyncio.get_running_loop().run_in_executor(None, tail_lines, path, lines)
        secrets = [s for s in self._secret_values() if len(s) >= 6]
        if secrets:
            clean = []
            for line in out:
                for s in secrets:
                    if s in line:
                        line = line.replace(s, "***")
                clean.append(line)
            out = clean
        return {"which": which, "path": path, "lines": out, "size": os.path.getsize(path)}

    # ---- log analyzer ---------------------------------------------------------------

    def _an(self):
        return getattr(self.cog, "issues", None)

    async def issues_list(self, closed: int = 0):
        an = self._an()
        if an is None:
            return self._err(409, "the log analyzer is not loaded")
        return {"summary": an.summary(), "issues": an.listing(include_closed=bool(closed))}

    async def issues_scan(self):
        an = self._an()
        if an is None:
            return self._err(409, "the log analyzer is not loaded")
        res = await an.scan()
        return self._ok(f"scanned: {res['entries']} warning/error entries, {res['new']} new issue(s)", result=res)

    async def issues_status(self, request: Request):
        an = self._an()
        if an is None:
            return self._err(409, "the log analyzer is not loaded")
        body = await self._body(request)
        ids = body.get("ids") if isinstance(body.get("ids"), list) else [body.get("id")]
        ids = [str(i) for i in ids if i]
        if not ids:
            return self._err(400, "need id or ids")
        try:
            n = an.set_status_many(ids, str(body.get("status") or ""), body.get("note"))
        except ValueError as ex:
            return self._err(400, str(ex))
        if n == 0:
            return self._err(404, "no such issue")
        return self._ok(f"{n} issue(s) marked {body.get('status')}")

    async def issues_clear(self, request: Request):
        an = self._an()
        if an is None:
            return self._err(409, "the log analyzer is not loaded")
        body = await self._body(request)
        if isinstance(body.get("ids"), list):
            return self._ok(f"deleted {an.forget([str(i) for i in body['ids'] if i])} issue(s)")
        which = "all" if body.get("which") == "all" else "closed"
        return self._ok(f"deleted {an.clear(which)} issue(s)")

    async def issues_report(self, closed: int = 0, min_level: str = "WARN", limit: int = 40):
        from fastapi.responses import PlainTextResponse
        an = self._an()
        if an is None:
            return self._err(409, "the log analyzer is not loaded")
        text = an.report(include_closed=bool(closed), min_level=min_level, limit=max(1, min(int(limit), 200)))
        return PlainTextResponse(text, media_type="text/markdown; charset=utf-8")

    async def archive_index(self):
        an = self._an()
        if an is None:
            return self._err(409, "the log analyzer is not loaded")
        idx = await asyncio.get_running_loop().run_in_executor(None, an.archive_index)
        return {"root": an.archive_root(), "keep_days": an.cfg.archive_days, "sources": idx}

    async def archive_read(self, source: str, date: str, lines: int = 500, grep: str = "",
                           format: str = "json"):
        from fastapi.responses import PlainTextResponse
        an = self._an()
        if an is None:
            return self._err(409, "the log analyzer is not loaded")
        out = await asyncio.get_running_loop().run_in_executor(
            None, an.archive_read, source, date, lines, grep or None)
        secrets = [s for s in self._secret_values() if len(s) >= 6]
        if secrets:
            out = [self._redact(line, secrets) for line in out]
        if format == "text":
            return PlainTextResponse("\n".join(out) + "\n")
        return {"source": source, "date": date, "lines": out}

    @staticmethod
    def _redact(line: str, secrets: list[str]) -> str:
        for s in secrets:
            if s in line:
                line = line.replace(s, "***")
        return line

    def _secret_values(self) -> list[str]:
        vals: list[str] = []

        def walk(node):
            if isinstance(node, dict):
                for k, v in node.items():
                    if is_secret_key(k) and isinstance(v, str) and v.strip():
                        vals.append(v)
                    else:
                        walk(v)
            elif isinstance(node, list):
                for i in node:
                    walk(i)
        walk(self.cog.locals or {})
        return vals
