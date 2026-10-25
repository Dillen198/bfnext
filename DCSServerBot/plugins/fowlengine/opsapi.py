"""
The Ops API: what the dashboard's OPS page talks to (through bfdb).

Routes are added to DCSServerBot's own WebService (the FastAPI app the RestAPI
plugin also lives on) under

    <restapi prefix>/fowlengine/ops/...

and every one needs an X-API-Key: `ops_api.api_key` in fowlengine.yaml -- its
own key, because these routes restart processes and rewrite the bot's config,
which the RestAPI key (`bfdb.dcsserverbot_api_key`, read-mostly stats) should
not be able to do. Without it the old shared key is still accepted, with a
loud deprecation warning. bfdb exposes the routes to a logged-in dashboard
admin as /api/admin/ops/..., so the browser never sees the key. Keep the
WebService on 127.0.0.1 (services/webservice.yaml `listen:`): bfdb and Fowl
Engine Manager both call it from this box, and the routes are plain HTTP.

POST config can't change anything that names a program, a path, a URL or an
update source (see protected_changes): those are edited on the box itself, so
a dashboard login can't be turned into running a command or leaking a secret.

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
  POST campaign/apply            {server}  -- write a held campaign pack over the server's copy
                                 at its next DCS start (backup kept)
  POST campaign/keep             {server}  -- keep the server's cfg (+ missions edited there);
                                 the pack's other missions still go in, the pack is dismissed
  POST stage/cancel              {name, server?}
  GET  config                    fowlengine.yaml with secrets masked
  POST config                    {yaml, base_mtime, restart_bfdb?}
  GET  logs?which=...&lines=N    tail of one log (an id from logs/list)
  GET  logs/list                 every log this box has: bot | service | bfdb | bfdb_boot | netidx,
                                 engine:<server> (bfnext.txt / bfrange.txt), dcs:<server> (dcs.log),
                                 file:<name> (issues.extra_files)
  GET  logs/follow?which=&offset=&ident=
                                 the lines added since `offset` (a byte offset an earlier call
                                 returned) + the new offset; no offset = the tail. A rotated or
                                 truncated log starts again (see follow_file)
  GET  dcs                       each DCS instance: state, players, mission, what may be done now
  POST dcs/start|stop|restart|mission_restart  {server}
                                 answers at once, the action runs on; GET dcs shows how it went.
                                 Refused (409) while the auto-updater checks/stages a release or
                                 the server is already being restarted
  GET  backup                    the box's Fowl Engine Manager: remote backup job + backups found
  POST backup/start              {password?}  -- a backup the way the box's own plan has it
  POST backup/verify             {zip, password?}  -- one of the zips GET backup lists
  POST backup/cancel
                                 Handed to the Manager's service through a folder it watches
                                 (<manager data>\\remote\\requests), only when the box's Manager
                                 has "remote backups" on. Restore is never remote: it overwrites
                                 the box and needs someone at it.
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

from .icons import icon, plain

try:  # FastAPI resolves handler annotations from this module's globals
    from fastapi import Request
except Exception:  # noqa: BLE001 - imported outside the bot (unit tests)
    Request = Any  # type: ignore[misc,assignment]

__all__ = ["OpsApi", "mask_secrets", "unmask_secrets", "SECRET_MASK", "is_secret_key",
           "tail_lines", "follow_file", "file_ident", "validate_plugin_yaml", "protected_changes",
           "is_protected_key"]

SECRET_MASK = "__SECRET__"
_SECRET_HINTS = ("password", "secret", "api_key", "apikey", "token", "webhook", "llm_key", "image_key", "tts_key")
CONFIG_BACKUPS_KEEP = 20
LOG_FILES = ("bot", "service", "bfdb", "bfdb_boot", "netidx")
_LOG_LABELS = {"bot": "DCSServerBot", "service": "Bot service wrapper", "bfdb": "bfdb",
               "bfdb_boot": "bfdb start-up output", "netidx": "netidx resolver"}
FOLLOW_MAX_BYTES = 256 * 1024      # one logs/follow answer reads at most this much
FOLLOW_TAIL_LINES = 300            # a first call (or a restart) shows this many lines
DCS_ACTIONS = ("start", "stop", "restart", "mission_restart")
# How old the Manager service's remote status may be before the box counts
# as "service not running" (it rewrites it every tick, ~2 s).
REMOTE_STATUS_STALE_SECS = 60


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


def _is_endpoint_key(key: Any) -> bool:
    k = str(key).lower()
    return ("url" in k or "uri" in k or k.endswith("host")
            or k.endswith("_provider") or k.endswith("_account_id"))


def unmask_secrets(new: Any, old: Any, path: str = "") -> None:
    """Put the real values back wherever the edited document still carries the
    mask. Raises ValueError if a masked value has no counterpart in the old
    document (the key was renamed/moved -- type the secret in again), or if a
    URL/host next to it changed: a saved secret only ever goes back to the
    endpoint it was saved for, so pointing news_llm_url somewhere else can't
    quietly send it the old news_llm_key."""
    if isinstance(new, dict):
        moved = [str(k) for k in new.keys() if _is_endpoint_key(k)
                 and (not isinstance(old, dict) or new.get(k) != old.get(k))]
        for k in list(new.keys()):
            v = new[k]
            here = f"{path}.{k}" if path else str(k)
            old_v = old.get(k) if isinstance(old, dict) else None
            if v == SECRET_MASK:
                if not isinstance(old_v, str) or old_v == SECRET_MASK:
                    raise ValueError(f"{here} still says {SECRET_MASK} but there is no saved value "
                                     f"for it -- type the real value in")
                if moved:
                    raise ValueError(f"{', '.join(moved)} changed next to {here}, so its saved value is not "
                                     f"carried over -- type {here} in again")
                new[k] = old_v
            else:
                unmask_secrets(v, old_v, here)
    elif isinstance(new, list):
        for i, item in enumerate(new):
            old_i = old[i] if isinstance(old, list) and i < len(old) else None
            unmask_secrets(item, old_i, f"{path}[{i}]")


# Keys an HTTP config edit may not add, change or remove, wherever they sit
# (DEFAULT, a per-server section, a bfdb.instances entry): anything that names
# a program to run, a file or folder, a network endpoint, or where engine
# updates come from and how they are trusted. With one of those, a dashboard
# admin login would be a way to run a command as the bot (netidx_resolver_cmd,
# bfdb.exe), to feed it a build (autoupdate.source/folder/repo/public_key,
# bot_plugin, campaigns) or to send a saved secret elsewhere (*_url). They are edited on
# the box. Everything else -- tuning, channels, messages, toggles, secrets
# themselves -- stays editable.
_PROTECTED_EXACT = {
    "exe", "cmd", "command", "home", "folder", "source", "repo", "url", "uri", "public_key",
    "bot_plugin", "campaigns", "token", "files", "tag_prefix", "config", "prefix", "bftools", "path",
    "listen", "shutdown_path",
    # Which service the saved news_image_key is sent to: switching the
    # provider (or the fallback) would hand a Cloudflare token to someone else.
    "news_image_provider", "news_image_fallback", "news_image_cf_account_id",
}
_PROTECTED_SUFFIXES = (
    "_exe", "_cmd", "_command", "_path", "_dir", "_url", "_uri", "_config", "_jsonl", "_file",
    "_folder", "_address", "_origins", "_model", "_host",
)


def is_protected_key(key: Any) -> bool:
    k = str(key).lower()
    return k in _PROTECTED_EXACT or k.endswith(_PROTECTED_SUFFIXES)


def protected_changes(new: Any, old: Any, path: str = "", under: bool = False) -> list[str]:
    """Paths of protected keys (is_protected_key) whose value differs between
    two documents -- added, removed or changed, at any depth. `under` = an
    ancestor key is protected, so everything below it counts (a list like
    cors_origins, a mapping like netidx_resolver_cmd's)."""
    out: list[str] = []
    if isinstance(new, dict) or isinstance(old, dict):
        n = new if isinstance(new, dict) else {}
        o = old if isinstance(old, dict) else {}
        if not isinstance(new, dict) or not isinstance(old, dict):
            if under and new != old:
                return [path or "(document)"]
        for k in list(n.keys()) + [k for k in o.keys() if k not in n]:
            here = f"{path}.{k}" if path else str(k)
            prot = under or is_protected_key(k)
            out += protected_changes(n.get(k), o.get(k), here, prot)
        return out
    if isinstance(new, list) and isinstance(old, list) and not under:
        for i in range(max(len(new), len(old))):
            out += protected_changes(new[i] if i < len(new) else None,
                                     old[i] if i < len(old) else None, f"{path}[{i}]", False)
        return out
    if under and new != old:
        return [path]
    if not under and isinstance(new, list) != isinstance(old, list):
        # a list replaced wholesale by a scalar (or back): look inside the list side
        return protected_changes(new if isinstance(new, list) else [], old if isinstance(old, list) else [],
                                 path, False)
    return out


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


def file_ident(st) -> str:
    """Which file this is, beyond its name. NTFS keeps a file's index and
    creation time across a rename, and a new file (DCS writing a fresh
    dcs.log, bflib renaming bfnext.txt aside at start) gets new ones. Off
    Windows st_ctime changes on every write, so only device + inode count."""
    ident = f"{getattr(st, 'st_dev', 0)}:{getattr(st, 'st_ino', 0)}"
    if os.name == "nt":
        born = getattr(st, "st_birthtime", None) or getattr(st, "st_ctime", 0)
        ident += f":{int(born)}"
    return ident


def _read_span(path: str, start: int, end: int) -> bytes:
    with open(path, "rb") as fh:
        fh.seek(start)
        return fh.read(max(0, end - start))


def _tail_span(path: str, size: int, lines: int, max_bytes: int) -> list[str]:
    start = max(0, size - max_bytes)
    out = _read_span(path, start, size).decode("utf-8", "replace").splitlines()
    if start > 0 and out:
        out = out[1:]  # first line is probably cut in half
    return out[-lines:] if lines > 0 else []


def follow_file(path: str, offset: Optional[int] = None, ident: Optional[str] = None,
                max_bytes: int = FOLLOW_MAX_BYTES, tail: int = FOLLOW_TAIL_LINES) -> dict:
    """One step of following a log that is being written.

    `offset` is what the previous answer returned (None = first call), and
    `ident` the file_ident it saw. The answer: {lines, offset, ident, size,
    reset, reason, more, skipped}. Lines are complete ones only -- a
    half-written last line waits for the next call -- unless a single line
    is longer than max_bytes, which then comes in pieces.

    - first call (no offset): the last `tail` lines, offset = the size
    - the file was replaced (ident differs: rotated, or DCS started a new
      dcs.log): reset, the new file from its start (its tail if it is big)
    - offset > size (truncated in place): reset, the tail
    - more than 4 x max_bytes behind: jump to the tail, `skipped` = bytes left out
    - otherwise: the next complete lines, at most max_bytes; `more` = there
      is more to read right now
    """
    base = {"lines": [], "offset": 0, "ident": None, "size": 0, "reset": False, "reason": None,
            "more": False, "skipped": 0}
    try:
        st = os.stat(path)
    except OSError:
        return {**base, "missing": True, "reset": bool(offset), "reason": "missing" if offset else None}
    size, cur = st.st_size, file_ident(st)
    base.update(ident=cur, size=size)
    try:
        if offset is None or offset < 0:
            return {**base, "lines": _tail_span(path, size, tail, max_bytes), "offset": size, "reason": "start"}
        if ident and ident != cur:
            if size <= max_bytes:
                lines = _read_span(path, 0, size).decode("utf-8", "replace").splitlines()
            else:
                lines = _tail_span(path, size, tail, max_bytes)
            return {**base, "lines": lines, "offset": size, "reset": True, "reason": "rotated"}
        if offset > size:
            return {**base, "lines": _tail_span(path, size, tail, max_bytes), "offset": size, "reset": True,
                    "reason": "truncated"}
        if size - offset > 4 * max_bytes:
            return {**base, "lines": _tail_span(path, size, tail, max_bytes), "offset": size,
                    "reason": "skipped", "skipped": size - offset - min(size, max_bytes)}
        data = _read_span(path, offset, min(size, offset + max_bytes))
    except OSError:
        return {**base, "offset": offset or 0, "reason": "unreadable"}
    cut = data.rfind(b"\n")
    if cut >= 0:
        data = data[:cut + 1]
    elif len(data) < max_bytes:
        data = b""  # a line still being written
    new_offset = offset + len(data)
    # capped by max_bytes (not just a half-written last line left): ask again now
    capped = offset + max_bytes < size
    return {**base, "lines": data.decode("utf-8", "replace").splitlines(), "offset": new_offset,
            "more": capped and new_offset < size}


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
        # server name -> the last DCS action asked for over this API
        # ({action, state: running|done|failed, at, finished, error})
        self._dcs_actions: dict[str, dict] = {}

    # ---- config ---------------------------------------------------------

    def _cfg(self) -> dict:
        return self.cog.get_config() or {}

    def _own_key(self) -> Optional[str]:
        key = (self._cfg().get("ops_api") or {}).get("api_key")
        return str(key) if key else None

    def _require_own_key(self) -> bool:
        return (self._cfg().get("ops_api") or {}).get("require_own_key") is True

    def _api_key(self) -> Optional[str]:
        """The key the OPS routes accept: ops_api.api_key, else -- unless
        ops_api.require_own_key is true -- the RestAPI key. register() warns
        about the fallback on every start."""
        own = self._own_key()
        if own:
            return own
        if self._require_own_key():
            return None
        key = (self._cfg().get("bfdb") or {}).get("dcsserverbot_api_key")
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
        if self._require_own_key() and not self._own_key():
            self.log.error("FowlEngine/ops: ops_api.require_own_key is true but ops_api.api_key is not "
                           "set -- the dashboard OPS page is disabled until it is")
            return
        key = self._api_key()
        if not key:
            self.log.warning("FowlEngine/ops: no API key (bfdb.dcsserverbot_api_key or ops_api.api_key) -- "
                             "the dashboard OPS page is disabled rather than exposed without one")
            return
        if not self._own_key():
            # Said on every start, not once: this is the one setting that lets
            # anything holding the RestAPI key restart processes and rewrite
            # fowlengine.yaml.
            self.log.warning(
                "FowlEngine/ops: ********************************************************************")
            self.log.warning(
                "FowlEngine/ops: DEPRECATED -- the OPS API is accepting the RestAPI key "
                "(bfdb.dcsserverbot_api_key) because ops_api.api_key is not set. Anything holding "
                "the RestAPI key can then restart processes and rewrite fowlengine.yaml. Set "
                "ops_api.api_key to its own long random value, and ops_api.require_own_key: true "
                "to refuse the fallback (see fowlengine.sample.yaml).")
            self.log.warning(
                "FowlEngine/ops: ********************************************************************")
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
        r.add_api_route("/campaign/apply", self.campaign_apply, methods=["POST"])
        r.add_api_route("/campaign/keep", self.campaign_keep, methods=["POST"])
        r.add_api_route("/stage/cancel", self.stage_cancel, methods=["POST"])
        r.add_api_route("/config", self.config_get, methods=["GET"])
        r.add_api_route("/config", self.config_post, methods=["POST"])
        r.add_api_route("/logs", self.logs, methods=["GET"])
        r.add_api_route("/logs/list", self.logs_list, methods=["GET"])
        r.add_api_route("/logs/follow", self.logs_follow, methods=["GET"])
        r.add_api_route("/dcs", self.dcs_list, methods=["GET"])
        for action in DCS_ACTIONS:
            r.add_api_route(f"/dcs/{action}", self._dcs_handler(action), methods=["POST"])
        r.add_api_route("/backup", self.backup_get, methods=["GET"])
        r.add_api_route("/backup/start", self.backup_start, methods=["POST"])
        r.add_api_route("/backup/verify", self.backup_verify, methods=["POST"])
        r.add_api_route("/backup/cancel", self.backup_cancel, methods=["POST"])
        r.add_api_route("/backup/restore", self.backup_restore, methods=["POST"])
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
        # The same text often went to ops_channel too; the dashboard gets it
        # with any custom-emoji markup turned back into unicode.
        return {"ok": True, "message": plain(message), **extra}

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
                # hashing is off the event loop: a first hash of a big file
                # would otherwise stall every Discord and web handler
                bftools = await asyncio.get_running_loop().run_in_executor(None, lambda: {
                    "path": p, "sha256": sha256_cached(p) if os.path.exists(p) else None,
                    "pending": pm.pending_info("bftools.exe") if pm else None})
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
            await self.cog.notify_ops(f"{icon('restart')} bfdb restarted from the dashboard OPS page.")

        asyncio.create_task(later())
        return self._ok("bfdb is restarting -- the dashboard will reconnect in a few seconds")

    async def bot_restart(self):
        async def later():
            await asyncio.sleep(1.5)
            await self.cog.notify_ops(f"{icon('restart')} Bot restarting (requested from the dashboard OPS page). "
                                      "DCS keeps running; bfdb restarts with the bot.")
            await self.cog.bot.node.restart()

        asyncio.create_task(later())
        return self._ok("the bot is restarting -- this page comes back when it has")

    async def update_check(self):
        upd = getattr(self.cog, "updater", None)
        if upd is None:
            return self._err(409, "the auto-updater is not loaded")
        res = await upd.check(reason="manual (dashboard)")
        return {"ok": bool(res.get("ok")), "message": plain(res.get("message") or res.get("error")), "result": res}

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
                await self.cog.notify_ops(f"{icon('restart')} Staged bfdb.exe applied from the dashboard OPS page.")

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
            row = await asyncio.get_running_loop().run_in_executor(None, self._server_row, server)
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

    async def _campaign_decide(self, request, decision: str):
        upd = getattr(self.cog, "updater", None)
        if upd is None:
            return self._err(409, "the auto-updater is not loaded")
        body = await self._body(request)
        name = str(body.get("server") or "")
        if not name:
            return self._err(400, "need the server whose campaign pack to decide on")
        try:
            # keep hashes the server's mission files: off the event loop
            msg = await asyncio.get_running_loop().run_in_executor(
                None, upd.campaign_decide, name, decision, "the OPS page")
        except LookupError as ex:
            return self._err(404, str(ex))
        except ValueError as ex:
            return self._err(400, str(ex))
        await self.cog.notify_ops(f"{icon('campaign')} **{name}**: {msg}")
        return self._ok(msg)

    async def campaign_apply(self, request: Request):
        return await self._campaign_decide(request, "apply")

    async def campaign_keep(self, request: Request):
        return await self._campaign_decide(request, "keep")

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
        locked = protected_changes(new_doc, old_doc)
        if locked:
            shown = ", ".join(locked[:8]) + (f" (+{len(locked) - 8} more)" if len(locked) > 8 else "")
            return self._err(403, f"these settings name a program, a path, a URL or an update source and can "
                                  f"only be changed in fowlengine.yaml on the server itself: {shown}")
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
        await self.cog.notify_ops(f"{icon('settings')} fowlengine.yaml was edited from the dashboard OPS page "
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
        entry = next((e for e in self._log_catalog() if e["id"] == which), None)
        return entry["path"] if entry else None

    def _log_catalog(self) -> list[dict]:
        """Every log the OPS API will read on this box: {id, label, group,
        server, path}. Ids are the only way in -- no path ever comes from the
        caller. Per DCS instance on this PC: the engine log (bfnext.txt, or
        bfrange.txt on a range) and dcs.log."""
        out: list[dict] = []
        for which in LOG_FILES:
            path = self._log_path(which)
            if path:
                out.append({"id": which, "label": _LOG_LABELS[which], "group": "bot", "server": None, "path": path})
        try:
            from .upload import is_remote_node
        except Exception:  # noqa: BLE001
            is_remote_node = lambda n: False  # noqa: E731
        for s in list(self.cog.bot.servers.values()):
            if is_remote_node(getattr(s, "node", None)):
                continue  # its files are on another PC
            home = getattr(getattr(s, "instance", None), "home", None)
            if not home:
                continue
            is_range = False
            try:
                is_range = bool(self.cog._is_range(s))
            except Exception:  # noqa: BLE001
                pass
            engine = "bfrange.txt" if is_range else "bfnext.txt"
            out.append({"id": f"engine:{s.name}", "label": f"{s.name}: engine ({engine})", "group": "engine",
                        "server": s.name, "path": os.path.join(home, "Logs", engine)})
            out.append({"id": f"dcs:{s.name}", "label": f"{s.name}: DCS (dcs.log)", "group": "dcs",
                        "server": s.name, "path": os.path.join(home, "Logs", "dcs.log")})
        an = self._an()
        for x in (getattr(getattr(an, "cfg", None), "extra_files", None) or []):
            try:
                name = str(x.get("name") or os.path.basename(x["path"]))
                out.append({"id": f"file:{name}", "label": name, "group": "extra", "server": None,
                            "path": os.path.expandvars(x["path"])})
            except (KeyError, TypeError, AttributeError):
                continue
        return out

    def _redact_lines(self, lines: list[str]) -> list[str]:
        secrets = [s for s in self._secret_values() if len(s) >= 6]
        return [self._redact(line, secrets) for line in lines] if secrets else lines

    async def logs(self, which: str = "bot", lines: int = 300):
        path = self._log_path(which)
        if path is None and which not in LOG_FILES:
            return self._err(400, f"no log {which!r} here -- see logs/list")
        if not path or not os.path.exists(path):
            return {"which": which, "path": path, "lines": [], "missing": True}
        out = await asyncio.get_running_loop().run_in_executor(None, tail_lines, path, lines)
        return {"which": which, "path": path, "lines": self._redact_lines(out), "size": os.path.getsize(path)}

    async def logs_list(self):
        def stat(e: dict) -> dict:
            try:
                st = os.stat(e["path"])
                return {**e, "exists": True, "size": st.st_size, "modified": st.st_mtime}
            except OSError:
                return {**e, "exists": False, "size": 0, "modified": None}
        cat = self._log_catalog()
        rows = await asyncio.get_running_loop().run_in_executor(None, lambda: [stat(e) for e in cat])
        return {"logs": rows}

    async def logs_follow(self, which: str = "bot", offset: Optional[int] = None, ident: Optional[str] = None):
        path = self._log_path(which)
        if path is None:
            return self._err(400 if which not in LOG_FILES else 404, f"no log {which!r} here -- see logs/list")
        res = await asyncio.get_running_loop().run_in_executor(None, follow_file, path, offset, ident)
        res["lines"] = self._redact_lines(res["lines"])
        return {"which": which, "path": path, **res}

    # ---- DCS server control ----------------------------------------------------------

    @staticmethod
    def _dcs_can(status_name: str) -> dict:
        """Which DCS actions make sense in this DCSServerBot state.
        SHUTDOWN = DCS not running; STOPPED = DCS up, no mission running."""
        up = status_name in ("RUNNING", "PAUSED", "STOPPED")
        down = status_name in ("SHUTDOWN", "UNREGISTERED")
        return {"start": down or status_name == "STOPPED",
                "stop": up or status_name in ("LOADING", "UNREGISTERED"),
                "restart": up or down, "mission_restart": up}

    def _dcs_blocked(self, server) -> Optional[str]:
        """Why no DCS action may start on `server` right now, or None."""
        upd = getattr(self.cog, "updater", None)
        lock = getattr(upd, "_lock", None) if upd is not None else None
        if lock is not None and lock.locked():
            what = getattr(upd, "_busy", None) or "checking for / staging a release"
            return f"the auto-updater is busy ({what}) -- try again when it is done"
        if server.name in (getattr(self.cog, "_stage_restarting", None) or {}):
            return f"{server.name} is already restarting to apply a staged build"
        act = self._dcs_actions.get(server.name)
        if act and act.get("state") == "running":
            return f"a {act['action'].replace('_', ' ')} of {server.name} is already under way"
        return None

    def _dcs_row(self, server) -> dict:
        st = getattr(server.status, "name", str(server.status))
        row: dict = {"name": server.name, "status": st,
                     "node": getattr(getattr(server, "node", None), "name", None)}
        try:
            row["players"] = len(server.get_active_players())
        except Exception:  # noqa: BLE001
            row["players"] = None
        mission = getattr(server, "current_mission", None)
        row["mission"] = getattr(mission, "name", None)
        row["map"] = getattr(mission, "map", None)
        try:
            row["kind"] = "range" if self.cog._is_range(server) else "campaign"
        except Exception:  # noqa: BLE001
            row["kind"] = None
        row["can"] = self._dcs_can(st)
        row["blocked"] = self._dcs_blocked(server)
        row["last_action"] = self._dcs_actions.get(server.name)
        return row

    async def dcs_list(self):
        return {"servers": [self._dcs_row(s) for s in list(self.cog.bot.servers.values())],
                "actions": list(DCS_ACTIONS)}

    def _dcs_handler(self, action: str):
        async def handler(request: Request):
            return await self.dcs_action(action, await self._body(request))
        handler.__name__ = f"dcs_{action}"
        return handler

    async def dcs_action(self, action: str, body: dict):
        if action not in DCS_ACTIONS:
            return self._err(400, f"action must be one of {', '.join(DCS_ACTIONS)}")
        name = str(body.get("server") or "")
        server = self._server(name)
        if server is None:
            known = ", ".join(sorted(self.cog.bot.servers)) or "none"
            return self._err(404, f"no DCS server named {name!r} on this bot (it has: {known})")
        why = self._dcs_blocked(server)
        if why:
            return self._err(409, why)
        st = getattr(server.status, "name", str(server.status))
        if not self._dcs_can(st)[action]:
            hint = {"start": "it is already running" if st in ("RUNNING", "PAUSED") else None,
                    "stop": "DCS is not running" if st == "SHUTDOWN" else None,
                    "mission_restart": "DCS is not running -- start it instead"}.get(action)
            return self._err(409, f"{server.name} is {st}: can't {action.replace('_', ' ')} now"
                                  + (f" ({hint})" if hint else " -- try again in a moment"))
        if action == "mission_restart" and not callable(getattr(server, "restart", None)):
            return self._err(501, "this DCSServerBot version has no mission restart (Server.restart)")
        upd = getattr(self.cog, "updater", None)
        rec = {"action": action, "state": "running", "at": datetime.now(timezone.utc).isoformat(),
               "finished": None, "error": None, "from_status": st}
        self._dcs_actions[server.name] = rec
        if action in ("stop", "restart") and upd is not None and hasattr(upd, "_orderly"):
            # asked for, so the crash watch (engine probation) doesn't count it
            upd._orderly[server.name] = time.time()

        async def run():
            what = action.replace("_", " ")
            try:
                if action == "start":
                    if st == "STOPPED" and callable(getattr(server, "start", None)):
                        await server.start()
                    else:
                        await server.startup()
                elif action == "stop":
                    await server.shutdown()
                elif action == "restart":
                    if st in ("SHUTDOWN", "UNREGISTERED"):
                        await server.startup()
                    elif upd is not None and hasattr(upd, "restart_dcs"):
                        await upd.restart_dcs(server, "restart asked for over the OPS API")
                    else:
                        await server.shutdown()
                        await server.startup()
                else:
                    await server.restart()
                rec.update(state="done")
                await self.cog.notify_ops(f"{icon('restart')} **{server.name}**: DCS {what} (asked for over "
                                          f"the OPS API) -- done.")
            except Exception as ex:  # noqa: BLE001
                rec.update(state="failed", error=str(ex) or type(ex).__name__)
                self.log.error(f"FowlEngine/ops: DCS {what} of {server.name} failed: {ex}")
                await self.cog.notify_ops(f"{icon('warning')} **{server.name}**: DCS {what} failed: {ex}")
            finally:
                rec["finished"] = datetime.now(timezone.utc).isoformat()

        asyncio.create_task(run())
        players = None
        try:
            players = len(server.get_active_players())
        except Exception:  # noqa: BLE001
            pass
        msg = {"start": "starting DCS", "stop": "shutting DCS down", "restart": "restarting DCS",
               "mission_restart": "restarting the mission"}[action]
        return self._ok(f"{server.name}: {msg}" + (f" ({players} player(s) on)" if players else ""),
                        server=server.name, action=rec)

    # ---- backups, through the box's Fowl Engine Manager ---------------------------------

    def _manager_dir(self) -> str:
        configured = (self._cfg().get("ops_api") or {}).get("manager_dir")
        if configured:
            return os.path.expandvars(str(configured))
        env = os.environ.get("FOWL_MANAGER_DATA")
        if env:
            return env
        return os.path.join(os.environ.get("ProgramData") or r"C:\ProgramData", "FowlEngine")

    def _remote_dir(self) -> str:
        return os.path.join(self._manager_dir(), "remote")

    def _remote_status(self) -> tuple[Optional[dict], Optional[str]]:
        """(the Manager service's remote status, why not)."""
        path = os.path.join(self._remote_dir(), "backup-status.json")
        try:
            with open(path, encoding="utf-8") as fh:
                doc = json.load(fh)
        except FileNotFoundError:
            return None, ("remote backups are not turned on in this box's Fowl Engine Manager "
                          "(on the box: Manager -> SETTINGS -> Remote backups; needs a Manager newer than 0.3.2)")
        except PermissionError:
            return None, ("the bot can't read the Manager's remote folder -- on the box, check SETTINGS -> "
                          "desktop user, then turn Remote backups off and on again (that grants the bot's user)")
        except (OSError, ValueError) as ex:
            return None, f"can't read the Manager's remote status: {ex}"
        if not isinstance(doc, dict):
            return None, "the Manager's remote status is not a JSON object"
        age = time.time() - float(doc.get("updated_unix") or 0)
        doc["age_secs"] = int(age)
        doc["stale"] = age > REMOTE_STATUS_STALE_SECS
        return doc, None

    def _remote_request(self, kind: str, **fields) -> tuple[Optional[str], Optional[object]]:
        """Drop a request where the Manager service picks it up: (id, error response)."""
        doc, why = self._remote_status()
        if why:
            return None, self._err(409, why)
        if doc.get("stale"):
            return None, self._err(409, f"the Fowl Engine Manager service on this box hasn't reported for "
                                        f"{doc['age_secs'] // 60} min -- is it running?")
        if not doc.get("enabled"):
            return None, self._err(409, "remote backups are turned off in this box's Fowl Engine Manager")
        req_id = f"{time.time_ns()}-{kind}"
        d = os.path.join(self._remote_dir(), "requests")
        try:
            os.makedirs(d, exist_ok=True)
            tmp = os.path.join(d, req_id + ".tmp")
            with open(tmp, "w", encoding="utf-8") as fh:
                json.dump({"id": req_id, "kind": kind, **fields}, fh)
            os.replace(tmp, os.path.join(d, req_id + ".json"))
        except OSError as ex:
            return None, self._err(500, f"could not hand the request to the Manager: {ex}")
        return req_id, None

    async def backup_get(self):
        doc, why = self._remote_status()
        if why:
            return self._err(409, why)
        return {"ok": True, **doc}

    async def backup_start(self, request: Request):
        body = await self._body(request)
        pw = body.get("password")
        if pw is not None and not isinstance(pw, str):
            return self._err(400, "password must be a string")
        if pw and len(pw) < 8:
            return self._err(400, "the backup password must be at least 8 characters")
        req_id, err = self._remote_request("backup", password=pw or None)
        if err:
            return err
        await self.cog.notify_ops(f"{icon('settings')} A whole-box backup was asked for over the OPS API.")
        return self._ok("backup asked for -- the Manager service starts it within a few seconds", request=req_id)

    async def backup_verify(self, request: Request):
        body = await self._body(request)
        zip_path = body.get("zip")
        if not isinstance(zip_path, str) or not zip_path.strip():
            return self._err(400, "need zip: one of the backups GET backup lists")
        pw = body.get("password")
        req_id, err = self._remote_request("verify", zip=zip_path.strip(),
                                           password=pw if isinstance(pw, str) and pw else None)
        if err:
            return err
        return self._ok("check asked for -- the Manager service reads the zip back", request=req_id)

    async def backup_cancel(self):
        req_id, err = self._remote_request("cancel")
        if err:
            return err
        return self._ok("cancel asked for", request=req_id)

    async def backup_restore(self):
        return self._err(403, "a restore overwrites the box and is only done there: open Fowl Engine Manager on "
                              "the box itself -> BACKUP -> Restore")

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
        out = self._redact_lines(out)
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
