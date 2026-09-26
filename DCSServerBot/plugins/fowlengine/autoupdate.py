"""
Automatic engine updates for the Fowl Engine stack, with a safety net.

What it does, on a loop driven by the FowlEngine cog:

  1. CHECK   a release source for a newer engine build: GitHub Releases on the
             repo (tags `engine-...`, published by deploy/publish-release.ps1),
             or a plain folder of releases on this PC / a LAN share.
  2. FETCH   the release's files into <staging>/_downloads/<tag>/ and verify
             every one against the sha256 in its manifest.json. A file that
             doesn't match is never staged.
  3. STAGE   each file the same way a Discord drag-and-drop upload does
             (upload.py): `<name>.pending` + a `.pending.json` sidecar in the
             right staging dir -- every campaign server's own dir for
             bflib.dll, every range server's for bfrange.dll, the global one
             for bfdb.exe and bftools.exe. Nothing new swaps binaries: the
             BFBinaries extension (DLLs, at DCS start) and procman (bfdb.exe,
             at bfdb start) still do, so a manual upload and an automatic one
             take exactly the same road.
  4. APPLY   per the `apply:` policy -- at the next scheduled restart (the
             stock behaviour), as soon as the server has been empty for a
             while, or immediately.
  5. WATCH   every new engine through a probation period and ROLL IT BACK by
             itself if it crashes DCS, never gets loaded by the mission, or
             (bfdb) dies / won't come up. A rolled-back release is marked bad
             and never staged again; the ops channel is told what happened.

Only files this module staged (sidecar `source: autoupdate`) are ever applied
early. A binary an admin dropped into Discord keeps its old meaning -- it
lands at the next restart -- whatever the policy says. Probation and rollback
cover both, since a hand-uploaded build can be just as bad.

Release manifest (manifest.json, one per release -- see publish-release.ps1):

  {
    "schema": 1,
    "tag": "engine-2026.09.26-1412",
    "git": "a1b2c3d4e5f6",            # commit the release was built from
    "built": "2026-09-26T14:12:00Z",
    "channel": "stable",               # stable | beta
    "notes": "free text",
    "files": {
      "bflib.dll": {"sha256": "...", "size": 123, "git": "a1b2c3d4e5f6"},
      "bfdb.exe":  {"sha256": "...", "size": 456, "git": "a1b2c3d4e5f6"},
      ...
    }
  }

Everything here is best-effort and never raises into the cog's loop: an
unreachable GitHub, a half-downloaded file or a server that won't start is a
logged, reported, retried condition -- never a dead bot.
"""
from __future__ import annotations

import asyncio
import hashlib
import json
import os
import shutil
import time
import zipfile
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any, Awaitable, Callable, Optional

__all__ = [
    "Updater", "UpdateConfig", "ENGINE_FILES", "parse_manifest", "pick_github_release",
    "pick_folder_release", "file_needs_update", "in_window", "sha256_file",
    "AUTOUPDATE_SOURCE", "ROLLBACK_SOURCE",
]

# Sidecar `source:` values. upload.py writes none (a human uploaded it).
AUTOUPDATE_SOURCE = "autoupdate"
ROLLBACK_SOURCE = "rollback"

# Every file a release may carry, and what it is.
ENGINE_FILES = ("bflib.dll", "bfrange.dll", "bfdb.exe", "bftools.exe")
DLL_FILES = ("bflib.dll", "bfrange.dll")
BOT_PLUGIN_ZIP = "fowlengine-bot.zip"

APPLY_POLICIES = ("next_restart", "when_idle", "immediately")

# The sidecar bflib writes on load (bflib/src/lib.rs report_build). bfrange has
# none yet, so a range engine is judged on "DCS stayed up" alone.
BUILD_SIDECARS = {"bflib.dll": "bfnext-bflib-build.json"}

HISTORY_KEEP = 150
DOWNLOADS_KEEP = 3
GITHUB_API = "https://api.github.com"


def _utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def _now_tag() -> str:
    return datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S")


def sha256_file(path: str) -> Optional[str]:
    try:
        h = hashlib.sha256()
        with open(path, "rb") as fh:
            for chunk in iter(lambda: fh.read(1 << 20), b""):
                h.update(chunk)
        return h.hexdigest()
    except OSError:
        return None


def _clean(val) -> Optional[str]:
    if val is None:
        return None
    s = str(val).strip()
    if not s or s.upper().startswith("REPLACE_WITH"):
        return None
    return os.path.expandvars(s)


# ---- config ----------------------------------------------------------------

@dataclass
class UpdateConfig:
    enabled: bool = False
    source: str = "github"               # github | folder
    repo: str = "Dillen198/bfnext"
    token: Optional[str] = None
    folder: Optional[str] = None
    tag_prefix: str = "engine-"
    channel: str = "stable"              # stable | beta
    check_minutes: float = 15.0
    files: tuple = ENGINE_FILES
    apply: str = "when_idle"
    bfdb_apply: str = "when_idle"
    idle_minutes: float = 10.0
    apply_window: Optional[str] = None   # "HH:MM-HH:MM", local time
    probation_minutes: float = 10.0
    load_timeout_minutes: float = 15.0
    bfdb_unhealthy_minutes: float = 15.0
    rollback_on_crash: bool = True
    db_snapshots_keep: int = 3
    download_dir: Optional[str] = None
    bftools_path: Optional[str] = None
    bot_plugin: bool = False
    paused: bool = False

    # Keys the Ops page may change at runtime (persisted as overrides in the
    # updater's state file, so no YAML is rewritten for a toggle).
    RUNTIME_KEYS = ("enabled", "channel", "apply", "bfdb_apply", "idle_minutes",
                    "apply_window", "check_minutes", "paused", "rollback_on_crash",
                    "probation_minutes", "bot_plugin")

    @classmethod
    def from_dicts(cls, yaml_block: Optional[dict], overrides: Optional[dict] = None) -> "UpdateConfig":
        raw = dict(yaml_block or {})
        raw.update({k: v for k, v in (overrides or {}).items() if k in cls.RUNTIME_KEYS})
        c = cls()
        c.enabled = bool(raw.get("enabled", c.enabled))
        c.source = str(raw.get("source") or c.source).strip().lower()
        if c.source not in ("github", "folder"):
            c.source = "github"
        c.repo = str(raw.get("repo") or c.repo).strip().strip("/")
        c.token = _clean(raw.get("token"))
        c.folder = _clean(raw.get("folder"))
        c.tag_prefix = str(raw.get("tag_prefix", c.tag_prefix) or "")
        ch = str(raw.get("channel") or c.channel).strip().lower()
        c.channel = ch if ch in ("stable", "beta") else "stable"
        c.check_minutes = max(1.0, float(raw.get("check_minutes", c.check_minutes) or c.check_minutes))
        files = raw.get("files")
        if isinstance(files, (list, tuple)) and files:
            c.files = tuple(str(f).lower() for f in files if str(f).lower() in ENGINE_FILES)
        pol = str(raw.get("apply") or c.apply).strip().lower()
        c.apply = pol if pol in APPLY_POLICIES else "next_restart"
        bpol = str(raw.get("bfdb_apply") or c.apply).strip().lower()
        c.bfdb_apply = bpol if bpol in APPLY_POLICIES else c.apply
        c.idle_minutes = max(0.0, float(raw.get("idle_minutes", c.idle_minutes) or 0))
        c.apply_window = _clean(raw.get("apply_window"))
        c.probation_minutes = max(1.0, float(raw.get("probation_minutes", c.probation_minutes) or c.probation_minutes))
        c.load_timeout_minutes = max(2.0, float(raw.get("load_timeout_minutes", c.load_timeout_minutes)
                                                or c.load_timeout_minutes))
        c.bfdb_unhealthy_minutes = max(2.0, float(raw.get("bfdb_unhealthy_minutes", c.bfdb_unhealthy_minutes)
                                                  or c.bfdb_unhealthy_minutes))
        c.rollback_on_crash = bool(raw.get("rollback_on_crash", c.rollback_on_crash))
        c.db_snapshots_keep = max(1, int(raw.get("db_snapshots_keep", c.db_snapshots_keep) or 1))
        c.download_dir = _clean(raw.get("download_dir"))
        c.bftools_path = _clean(raw.get("bftools_path"))
        c.bot_plugin = bool(raw.get("bot_plugin", c.bot_plugin))
        c.paused = bool(raw.get("paused", c.paused))
        return c

    def public(self) -> dict:
        """What the Ops page shows (no token)."""
        return {
            "enabled": self.enabled, "source": self.source,
            "repo": self.repo if self.source == "github" else None,
            "folder": self.folder if self.source == "folder" else None,
            "has_token": bool(self.token), "tag_prefix": self.tag_prefix,
            "channel": self.channel, "check_minutes": self.check_minutes,
            "files": list(self.files), "apply": self.apply, "bfdb_apply": self.bfdb_apply,
            "idle_minutes": self.idle_minutes, "apply_window": self.apply_window,
            "probation_minutes": self.probation_minutes,
            "load_timeout_minutes": self.load_timeout_minutes,
            "rollback_on_crash": self.rollback_on_crash, "bot_plugin": self.bot_plugin,
            "paused": self.paused,
        }


# ---- pure helpers (unit-tested without a bot) -------------------------------

def parse_manifest(doc: Any) -> dict:
    """Validate a manifest.json document. Raises ValueError with the reason."""
    if not isinstance(doc, dict):
        raise ValueError("manifest is not a JSON object")
    tag = str(doc.get("tag") or "").strip()
    if not tag:
        raise ValueError("manifest has no tag")
    files = doc.get("files")
    if not isinstance(files, dict) or not files:
        raise ValueError("manifest lists no files")
    clean_files = {}
    for name, meta in files.items():
        lname = str(name).lower()
        if lname not in ENGINE_FILES and lname != BOT_PLUGIN_ZIP:
            continue  # a newer publisher may ship things this bot doesn't know
        if not isinstance(meta, dict):
            raise ValueError(f"manifest entry for {name} is not an object")
        sha = str(meta.get("sha256") or "").lower()
        if len(sha) != 64 or any(c not in "0123456789abcdef" for c in sha):
            raise ValueError(f"manifest entry for {name} has no valid sha256")
        clean_files[lname] = {
            "sha256": sha,
            "size": int(meta.get("size") or 0),
            "git": (str(meta.get("git")) if meta.get("git") else None),
            "built": (str(meta.get("built")) if meta.get("built") else None),
        }
    if not clean_files:
        raise ValueError("manifest lists no engine files this bot knows")
    ch = str(doc.get("channel") or "stable").lower()
    return {
        "schema": int(doc.get("schema") or 1),
        "tag": tag,
        "git": (str(doc.get("git")) if doc.get("git") else None),
        "built": (str(doc.get("built")) if doc.get("built") else None),
        "channel": ch if ch in ("stable", "beta") else "stable",
        "notes": str(doc.get("notes") or "")[:4000],
        "files": clean_files,
    }


def pick_github_release(releases: list, tag_prefix: str, channel: str,
                        bad: Optional[set] = None) -> Optional[dict]:
    """The newest usable release from GET /repos/{repo}/releases (which GitHub
    returns newest first). Drafts are never used; pre-releases only on the
    beta channel; tags outside `tag_prefix` belong to something else; a
    release we rolled back is skipped so an older good one is not offered
    again either -- nothing older than a bad release is ever picked."""
    bad = bad or set()
    for rel in releases or []:
        if not isinstance(rel, dict) or rel.get("draft"):
            continue
        tag = str(rel.get("tag_name") or "")
        if tag_prefix and not tag.startswith(tag_prefix):
            continue
        if rel.get("prerelease") and channel != "beta":
            continue
        if tag in bad:
            return None
        assets = {str(a.get("name") or "").lower(): a for a in rel.get("assets") or []
                  if isinstance(a, dict)}
        if "manifest.json" not in assets:
            continue
        return {"tag": tag, "assets": assets, "prerelease": bool(rel.get("prerelease")),
                "published": rel.get("published_at"), "html_url": rel.get("html_url"),
                "body": rel.get("body") or ""}
    return None


def pick_folder_release(folder: str, channel: str, bad: Optional[set] = None) -> Optional[dict]:
    """The newest release in a folder source: either the folder itself holds
    manifest.json (+ files), or it holds one sub-folder per release. Newest =
    latest manifest `built`, ties broken by tag."""
    bad = bad or set()
    candidates = []
    roots = []
    if os.path.isfile(os.path.join(folder, "manifest.json")):
        roots.append(folder)
    else:
        try:
            for entry in os.scandir(folder):
                if entry.is_dir() and os.path.isfile(os.path.join(entry.path, "manifest.json")):
                    roots.append(entry.path)
        except OSError:
            return None
    for root in roots:
        try:
            with open(os.path.join(root, "manifest.json"), encoding="utf-8") as fh:
                m = parse_manifest(json.load(fh))
        except (OSError, ValueError):
            continue
        if m["channel"] == "beta" and channel != "beta":
            continue
        candidates.append((m.get("built") or "", m["tag"], root, m))
    if not candidates:
        return None
    candidates.sort(reverse=True)
    _, tag, root, m = candidates[0]
    if tag in bad:
        return None
    return {"tag": tag, "root": root, "manifest": m}


def file_needs_update(want_sha: str, live_sha: Optional[str], pending_sha: Optional[str]) -> str:
    """'current' | 'staged' | 'stage' for one file."""
    if live_sha and live_sha.lower() == want_sha.lower():
        return "current"
    if pending_sha and pending_sha.lower() == want_sha.lower():
        return "staged"
    return "stage"


def in_window(window: Optional[str], now: Optional[datetime] = None) -> bool:
    """True if `now` (local time) falls in "HH:MM-HH:MM" (may wrap midnight).
    No window = always."""
    if not window:
        return True
    try:
        a, b = [p.strip() for p in window.split("-", 1)]
        ah, am = [int(x) for x in a.split(":")]
        bh, bm = [int(x) for x in b.split(":")]
    except (ValueError, AttributeError):
        return True  # a malformed window must not block updates forever
    now = now or datetime.now()
    cur = now.hour * 60 + now.minute
    start, end = ah * 60 + am, bh * 60 + bm
    if start == end:
        return True
    if start < end:
        return start <= cur < end
    return cur >= start or cur < end


def _inside_git_worktree(path: str) -> bool:
    p = os.path.abspath(path)
    for _ in range(12):
        if os.path.exists(os.path.join(p, ".git")):
            return True
        parent = os.path.dirname(p)
        if parent == p:
            break
        p = parent
    return False


# ---- the updater -------------------------------------------------------------

@dataclass
class DllTarget:
    """One DCS server's engine DLL, as the cog resolves it."""
    server: Any                  # DCSServerBot Server
    name: str                    # DCSServerBot server name
    dll_name: str
    dll_path: str
    staging_dir: str
    remote: bool
    home: Optional[str]


class Updater:
    """Owned by the FowlEngine cog. `cog` supplies the bot-facing bits
    (server list, procman, notify); everything else lives here."""

    def __init__(self, cog, log, state_path: str):
        self.cog = cog
        self.log = log
        self.state_path = state_path
        self.state: dict = self._load_state()
        self.cfg = UpdateConfig.from_dicts(self._yaml_block(), self.state.get("overrides"))
        self._lock = asyncio.Lock()
        self._busy: Optional[str] = None         # what a manual action is doing right now
        self._last_status: dict = {}              # server name -> Status name, for crash detection
        self._orderly: dict = {}                  # server name -> ts of an orderly/requested shutdown
        self._idle_since: dict = {}               # server name -> ts the server was last seen empty
        self._last_tick = time.monotonic()

    # ---- config / state -------------------------------------------------

    def _yaml_block(self) -> dict:
        try:
            return (self.cog.get_config() or {}).get("autoupdate") or {}
        except Exception:  # noqa: BLE001
            return {}

    def reload_config(self) -> None:
        self.cfg = UpdateConfig.from_dicts(self._yaml_block(), self.state.get("overrides"))

    def set_overrides(self, changes: dict) -> dict:
        ov = dict(self.state.get("overrides") or {})
        for k, v in (changes or {}).items():
            if k not in UpdateConfig.RUNTIME_KEYS:
                raise ValueError(f"{k} cannot be changed from the Ops page")
            if k in ("apply", "bfdb_apply") and v not in APPLY_POLICIES:
                raise ValueError(f"{k} must be one of {', '.join(APPLY_POLICIES)}")
            if k == "channel" and v not in ("stable", "beta"):
                raise ValueError("channel must be stable or beta")
            if k == "apply_window" and v:
                a, _, b = str(v).partition("-")
                for part in (a, b):
                    hh, _, mm = part.strip().partition(":")
                    if not (hh.isdigit() and mm.isdigit() and int(hh) < 24 and int(mm) < 60):
                        raise ValueError("apply_window must look like 03:00-07:00")
            ov[k] = v
        self.state["overrides"] = ov
        self.reload_config()
        self._save_state()
        self._history("settings", ", ".join(f"{k}={v}" for k, v in (changes or {}).items()))
        return self.cfg.public()

    def clear_overrides(self) -> None:
        self.state["overrides"] = {}
        self.reload_config()
        self._save_state()

    def _load_state(self) -> dict:
        try:
            with open(self.state_path, encoding="utf-8") as fh:
                doc = json.load(fh)
            if isinstance(doc, dict):
                return doc
        except (OSError, ValueError):
            pass
        return {}

    def _save_state(self) -> None:
        try:
            tmp = self.state_path + ".tmp"
            with open(tmp, "w", encoding="utf-8") as fh:
                json.dump(self.state, fh, indent=2, default=str)
            os.replace(tmp, self.state_path)
        except OSError as ex:
            self.log.error(f"FowlEngine/autoupdate: could not save {self.state_path}: {ex}")

    def _history(self, event: str, detail: str = "", **extra) -> None:
        h = list(self.state.get("history") or [])
        h.append({"ts": _utc_now(), "event": event, "detail": detail, **extra})
        self.state["history"] = h[-HISTORY_KEEP:]
        self._save_state()

    @property
    def bad(self) -> set:
        return set(self.state.get("bad") or [])

    def mark_bad(self, tag: Optional[str], why: str) -> None:
        if not tag:
            return
        bad = list(self.state.get("bad") or [])
        if tag not in bad:
            bad.append(tag)
        self.state["bad"] = bad[-50:]
        self._history("marked_bad", why, tag=tag)

    def unmark_bad(self, tag: str) -> bool:
        bad = list(self.state.get("bad") or [])
        if tag not in bad:
            return False
        bad.remove(tag)
        self.state["bad"] = bad
        self._history("unmarked_bad", "", tag=tag)
        return True

    async def _notify(self, msg: str) -> None:
        try:
            await self.cog.notify_ops(msg)
        except Exception as ex:  # noqa: BLE001
            self.log.error(f"FowlEngine/autoupdate: notify failed: {ex}")

    # ---- where things live ------------------------------------------------

    @property
    def procman(self):
        return getattr(self.cog, "procman", None)

    def global_staging(self) -> str:
        pm = self.procman
        if pm is not None:
            try:
                return pm.staging_dir
            except Exception:  # noqa: BLE001
                pass
        from .upload import global_staging_dir
        return global_staging_dir(self.cog.get_config() or {})

    def download_root(self) -> str:
        return self.cfg.download_dir or os.path.join(self.global_staging(), "_downloads")

    def dll_targets(self) -> list[DllTarget]:
        from .upload import engine_binaries, is_remote_node
        out = []
        for server in list(self.cog.bot.servers.values()):
            try:
                b = engine_binaries(self.cog, server)
            except Exception as ex:  # noqa: BLE001
                self.log.debug(f"FowlEngine/autoupdate: {server.name}: binaries lookup failed: {ex}")
                continue
            out.append(DllTarget(
                server=server, name=server.name, dll_name=b["dll_name"], dll_path=b["dll_path"],
                staging_dir=b["staging_dir"] or self.global_staging(),
                remote=is_remote_node(getattr(server, "node", None)),
                home=getattr(getattr(server, "instance", None), "home", None),
            ))
        return out

    def bftools_path(self) -> Optional[str]:
        if self.cfg.bftools_path:
            return self.cfg.bftools_path
        for server in list(self.cog.bot.servers.values()):
            try:
                p = _clean(self.cog._ext_cfg(server, "BFWeather").get("bftools"))
            except Exception:  # noqa: BLE001
                p = None
            if p:
                return p
        return None

    @staticmethod
    def _pending(staging: str, name: str) -> str:
        return os.path.join(staging, f"{name}.pending")

    @staticmethod
    def _read_sidecar(pending: str) -> dict:
        try:
            with open(pending + ".json", encoding="utf-8") as fh:
                doc = json.load(fh)
            return doc if isinstance(doc, dict) else {}
        except (OSError, ValueError):
            return {}

    # ---- 1. check --------------------------------------------------------

    async def _http_json(self, http, url: str) -> Any:
        headers = {"Accept": "application/vnd.github+json", "User-Agent": "fowlengine-autoupdate"}
        if self.cfg.token:
            headers["Authorization"] = f"Bearer {self.cfg.token}"
        async with http.get(url, headers=headers, timeout=30) as r:
            if r.status == 403 and r.headers.get("X-RateLimit-Remaining") == "0":
                raise RuntimeError("GitHub API rate limit hit -- set autoupdate.token or raise check_minutes")
            if r.status != 200:
                raise RuntimeError(f"GET {url} -> HTTP {r.status}")
            return await r.json(content_type=None)

    async def _download(self, http, asset: dict, dest: str, want_sha: str) -> None:
        """Stream one GitHub asset to dest, verifying its sha256."""
        headers = {"User-Agent": "fowlengine-autoupdate"}
        if self.cfg.token:
            # private repo: the API asset URL with an octet-stream Accept
            url = asset.get("url")
            headers["Authorization"] = f"Bearer {self.cfg.token}"
            headers["Accept"] = "application/octet-stream"
        else:
            url = asset.get("browser_download_url")
        if not url:
            raise RuntimeError(f"asset {asset.get('name')} has no download URL")
        part = dest + ".part"
        h = hashlib.sha256()
        async with http.get(url, headers=headers, timeout=None) as r:
            if r.status != 200:
                raise RuntimeError(f"download {asset.get('name')} -> HTTP {r.status}")
            with open(part, "wb") as fh:
                async for chunk in r.content.iter_chunked(1 << 20):
                    fh.write(chunk)
                    h.update(chunk)
        if h.hexdigest() != want_sha:
            try:
                os.remove(part)
            except OSError:
                pass
            raise RuntimeError(f"{asset.get('name')}: sha256 mismatch (got {h.hexdigest()[:12]}, "
                               f"manifest says {want_sha[:12]}) -- refusing it")
        os.replace(part, dest)

    async def check(self, *, stage: bool = True, reason: str = "scheduled") -> dict:
        """Find the newest release, fetch + verify it, stage what differs.
        Returns a summary dict (also stored as state['last_check'])."""
        async with self._lock:
            self._busy = "checking for updates"
            try:
                summary = await self._check_locked(stage=stage)
                summary["reason"] = reason
            except Exception as ex:  # noqa: BLE001
                self.log.warning(f"FowlEngine/autoupdate: check failed: {ex}")
                summary = {"ok": False, "error": str(ex), "reason": reason}
            finally:
                self._busy = None
            summary["at"] = _utc_now()
            self.state["last_check"] = summary
            self._save_state()
            return summary

    async def _check_locked(self, *, stage: bool) -> dict:
        import aiohttp

        cfg = self.cfg
        root = self.download_root()
        os.makedirs(root, exist_ok=True)
        if cfg.source == "folder":
            if not cfg.folder:
                raise RuntimeError("autoupdate.source is folder but autoupdate.folder is not set")
            rel = await asyncio.get_running_loop().run_in_executor(
                None, lambda: pick_folder_release(cfg.folder, cfg.channel, self.bad))
            if not rel:
                return {"ok": True, "latest": None, "message": f"no usable release in {cfg.folder}"}
            manifest = rel["manifest"]
            dest_dir = os.path.join(root, manifest["tag"])
            os.makedirs(dest_dir, exist_ok=True)
            urls = {}
            for name, meta in manifest["files"].items():
                if not self._want_file(name):
                    continue
                dest = os.path.join(dest_dir, name)
                if sha256_file(dest) == meta["sha256"]:
                    continue
                src = os.path.join(rel["root"], name)
                if not os.path.isfile(src):
                    raise RuntimeError(f"release {manifest['tag']} lists {name} but the file is missing")
                await asyncio.get_running_loop().run_in_executor(None, shutil.copy2, src, dest + ".part")
                got = sha256_file(dest + ".part")
                if got != meta["sha256"]:
                    os.remove(dest + ".part")
                    raise RuntimeError(f"{name} in {rel['root']}: sha256 mismatch -- refusing it")
                os.replace(dest + ".part", dest)
        else:
            async with aiohttp.ClientSession() as http:
                releases = await self._http_json(
                    http, f"{GITHUB_API}/repos/{cfg.repo}/releases?per_page=30")
                rel = pick_github_release(releases, cfg.tag_prefix, cfg.channel, self.bad)
                if not rel:
                    return {"ok": True, "latest": None,
                            "message": f"no usable `{cfg.tag_prefix}*` release on {cfg.repo} "
                                       f"({cfg.channel} channel)"}
                massets = rel["assets"]
                headers = {"User-Agent": "fowlengine-autoupdate"}
                murl = massets["manifest.json"].get("browser_download_url")
                if cfg.token:
                    murl = massets["manifest.json"].get("url")
                    headers.update({"Authorization": f"Bearer {cfg.token}",
                                    "Accept": "application/octet-stream"})
                async with http.get(murl, headers=headers, timeout=30) as r:
                    if r.status != 200:
                        raise RuntimeError(f"manifest.json download -> HTTP {r.status}")
                    manifest = parse_manifest(json.loads(await r.read()))
                if manifest["tag"] != rel["tag"]:
                    raise RuntimeError(f"release {rel['tag']} carries a manifest for {manifest['tag']}")
                dest_dir = os.path.join(root, manifest["tag"])
                os.makedirs(dest_dir, exist_ok=True)
                urls = {}
                for name, meta in manifest["files"].items():
                    if not self._want_file(name):
                        continue
                    asset = massets.get(name)
                    if not asset:
                        raise RuntimeError(f"release {manifest['tag']} lists {name} but has no such asset")
                    urls[name] = asset.get("browser_download_url")
                    dest = os.path.join(dest_dir, name)
                    if sha256_file(dest) == meta["sha256"]:
                        continue
                    self.log.info(f"FowlEngine/autoupdate: downloading {name} from {manifest['tag']}")
                    await self._download(http, asset, dest, meta["sha256"])
                manifest["html_url"] = rel.get("html_url")
                manifest["notes"] = manifest["notes"] or rel.get("body", "")[:4000]

        with open(os.path.join(dest_dir, "manifest.json"), "w", encoding="utf-8") as fh:
            json.dump(manifest, fh, indent=2)
        prev = (self.state.get("latest") or {}).get("tag")
        self.state["latest"] = {**manifest, "dir": dest_dir, "urls": urls}
        if prev != manifest["tag"]:
            self._history("found", f"release {manifest['tag']} ({manifest.get('git') or '?'})",
                          tag=manifest["tag"])
        self._prune_downloads(root, keep_tag=manifest["tag"])
        staged = await self._stage_latest() if stage else []
        return {"ok": True, "latest": manifest["tag"], "staged": staged,
                "message": (f"staged {', '.join(staged)}" if staged else "everything is up to date")}

    def _want_file(self, name: str) -> bool:
        if name == BOT_PLUGIN_ZIP:
            return self.cfg.bot_plugin
        return name in self.cfg.files

    def _prune_downloads(self, root: str, keep_tag: str) -> None:
        try:
            dirs = sorted((e for e in os.scandir(root) if e.is_dir()),
                          key=lambda e: e.stat().st_mtime, reverse=True)
        except OSError:
            return
        kept = 0
        for e in dirs:
            if e.name == keep_tag:
                continue
            kept += 1
            if kept >= DOWNLOADS_KEEP:
                shutil.rmtree(e.path, ignore_errors=True)

    # ---- 2. stage -------------------------------------------------------

    def _sidecar_for(self, name: str, meta: dict, manifest: dict, targets: list[str]) -> dict:
        return {
            "uploader": "auto-update",
            "source": AUTOUPDATE_SOURCE,
            "tag": manifest["tag"],
            "git": meta.get("git") or manifest.get("git"),
            "built": meta.get("built") or manifest.get("built"),
            "utc": datetime.now(timezone.utc).isoformat(),
            "size": meta.get("size"),
            "sha256": meta["sha256"],
            "notes": f"release {manifest['tag']}",
            "targets": targets,
        }

    def _stage_local(self, src: str, staging: str, name: str, sidecar: dict) -> None:
        os.makedirs(staging, exist_ok=True)
        pending = self._pending(staging, name)
        tmp = pending + ".tmp"
        shutil.copy2(src, tmp)
        os.replace(tmp, pending)
        with open(pending + ".json", "w", encoding="utf-8") as fh:
            json.dump(sidecar, fh, indent=2)

    async def _stage_latest(self) -> list[str]:
        """Stage every file of the latest release that differs from what's
        live and isn't already pending. Returns human-readable lines."""
        latest = self.state.get("latest") or {}
        if not latest or latest.get("tag") in self.bad:
            return []
        files = latest.get("files") or {}
        d = latest.get("dir") or ""
        out: list[str] = []
        loop = asyncio.get_running_loop()

        # engine DLLs -> each server's own staging dir
        for dll in DLL_FILES:
            meta = files.get(dll)
            if not meta or not self._want_file(dll):
                continue
            src = os.path.join(d, dll)
            groups: dict = {}
            for t in self.dll_targets():
                if t.dll_name != dll:
                    continue
                key = (getattr(getattr(t.server, "node", None), "name", None),
                       os.path.normcase(os.path.normpath(t.staging_dir)))
                groups.setdefault(key, []).append(t)
            for (_node, _sdir), tgts in groups.items():
                t0 = tgts[0]
                names = [t.name for t in tgts]
                if t0.remote:
                    done = await self._stage_remote(t0, dll, meta, latest, names)
                    if done:
                        out.append(f"{dll} → {', '.join(names)} (node {t0.server.node.name})")
                    continue
                live_sha = await loop.run_in_executor(None, sha256_file, t0.dll_path) if t0.dll_path else None
                pend = self._pending(t0.staging_dir, dll)
                pend_sha = self._read_sidecar(pend).get("sha256") if os.path.exists(pend) else None
                if pend_sha is None and os.path.exists(pend):
                    pend_sha = await loop.run_in_executor(None, sha256_file, pend)
                verdict = file_needs_update(meta["sha256"], live_sha, pend_sha)
                if verdict != "stage":
                    continue
                if os.path.exists(pend) and self._read_sidecar(pend).get("source") != AUTOUPDATE_SOURCE:
                    # an admin's manual upload is waiting there -- theirs wins
                    self.log.info(f"FowlEngine/autoupdate: {dll} for {', '.join(names)}: a manual upload "
                                  f"is staged, not overwriting it")
                    continue
                await loop.run_in_executor(
                    None, self._stage_local, src, t0.staging_dir, dll,
                    self._sidecar_for(dll, meta, latest, names))
                out.append(f"{dll} → {', '.join(names)}")

        # bfdb.exe + bftools.exe -> the global staging dir
        for name, live in (("bfdb.exe", getattr(self.procman, "exe", None) if self.procman else None),
                           ("bftools.exe", self.bftools_path())):
            meta = files.get(name)
            if not meta or not self._want_file(name) or not live:
                continue
            staging = self.global_staging()
            if not staging:
                continue
            live_sha = await loop.run_in_executor(None, sha256_file, live)
            pend = self._pending(staging, name)
            side = self._read_sidecar(pend) if os.path.exists(pend) else {}
            pend_sha = side.get("sha256") if os.path.exists(pend) else None
            if file_needs_update(meta["sha256"], live_sha, pend_sha) != "stage":
                continue
            if os.path.exists(pend) and side.get("source") != AUTOUPDATE_SOURCE:
                continue
            await loop.run_in_executor(None, self._stage_local, os.path.join(d, name), staging, name,
                                       self._sidecar_for(name, meta, latest, [name[:-4]]))
            out.append(name)

        if BOT_PLUGIN_ZIP in files and self.cfg.bot_plugin:
            note = await loop.run_in_executor(None, self._apply_bot_plugin, latest)
            if note:
                out.append(note)

        if out:
            self._history("staged", "; ".join(out), tag=latest.get("tag"))
            await self._notify(
                f"📦 **Engine update {latest.get('tag')}** (`{latest.get('git') or '?'}`) staged: "
                + "; ".join(out) + f"\nApply policy: `{self.cfg.apply}` (bfdb: `{self.cfg.bfdb_apply}`).")
        return out

    async def _stage_remote(self, t: DllTarget, dll: str, meta: dict, latest: dict, names: list) -> bool:
        """A server on an agent node: its node downloads the file itself
        (node.write_file takes a URL), so this only works from GitHub."""
        key = f"{t.name}|{dll}"
        remote_done = self.state.setdefault("remote_staged", {})
        if remote_done.get(key) == latest.get("tag"):
            return False
        url = (latest.get("urls") or {}).get(dll)
        if not url:
            self.log.info(f"FowlEngine/autoupdate: {t.name} is on node {t.server.node.name} -- only a "
                          f"GitHub source can stage there; skipped")
            return False
        from core import UploadStatus
        try:
            try:
                await t.server.node.create_directory(t.staging_dir)
            except Exception:  # noqa: BLE001
                pass
            rc = await t.server.node.write_file(self._pending(t.staging_dir, dll), url, overwrite=True)
            if rc != UploadStatus.OK:
                raise RuntimeError(getattr(rc, "name", rc))
        except Exception as ex:  # noqa: BLE001
            self.log.warning(f"FowlEngine/autoupdate: staging {dll} on node {t.server.node.name} failed: {ex}")
            return False
        remote_done[key] = latest.get("tag")
        self._save_state()
        return True

    # ---- bot plugin self-update (opt-in) -------------------------------------

    def _apply_bot_plugin(self, latest: dict) -> Optional[str]:
        """Unpack fowlengine-bot.zip over this plugin + its extensions. Takes
        effect on the next bot restart. Refuses when the plugin is a symlink or
        lives in a git checkout -- that is a developer's tree, not an install."""
        tag = latest.get("tag")
        if self.state.get("bot_plugin_tag") == tag:
            return None
        plugin_dir = os.path.dirname(os.path.abspath(__file__))
        bot_root = os.path.dirname(os.path.dirname(plugin_dir))
        if os.path.realpath(plugin_dir) != os.path.abspath(plugin_dir) or _inside_git_worktree(plugin_dir):
            self.log.warning("FowlEngine/autoupdate: bot_plugin is on but the plugin lives in a git "
                             "checkout / symlink -- not overwriting a working tree")
            self.state["bot_plugin_tag"] = tag
            return "bot plugin: skipped (plugin is a git checkout / symlink)"
        zpath = os.path.join(latest.get("dir") or "", BOT_PLUGIN_ZIP)
        try:
            with zipfile.ZipFile(zpath) as z:
                names = z.namelist()
                if "plugins/fowlengine/commands.py" not in names:
                    raise RuntimeError("zip has no plugins/fowlengine/commands.py")
                for n in names:
                    if n.startswith("/") or ".." in n.replace("\\", "/").split("/"):
                        raise RuntimeError(f"unsafe path in zip: {n}")
                    if not (n.startswith("plugins/fowlengine/") or n.startswith("extensions/bf")):
                        raise RuntimeError(f"zip touches {n}, outside the fowlengine plugin/extensions")
                backup = os.path.join(bot_root, "_fowl_backups", f"plugin-{_now_tag()}.zip")
                os.makedirs(os.path.dirname(backup), exist_ok=True)
                with zipfile.ZipFile(backup, "w", zipfile.ZIP_DEFLATED) as bz:
                    for top in {n.split("/")[0] + "/" + n.split("/")[1] for n in names if n.count("/") >= 2}:
                        src_dir = os.path.join(bot_root, *top.split("/"))
                        for dirpath, _dirs, fnames in os.walk(src_dir):
                            if "__pycache__" in dirpath:
                                continue
                            for f in fnames:
                                full = os.path.join(dirpath, f)
                                bz.write(full, os.path.relpath(full, bot_root))
                z.extractall(bot_root)
        except Exception as ex:  # noqa: BLE001
            self.log.error(f"FowlEngine/autoupdate: bot plugin update failed: {ex}")
            return f"bot plugin: FAILED ({ex})"
        self.state["bot_plugin_tag"] = tag
        self._history("bot_plugin", f"unpacked {BOT_PLUGIN_ZIP}; active after the bot restarts", tag=tag)
        return "bot plugin (active after the next bot restart)"

    # ---- 3. apply ---------------------------------------------------------

    def _server_populated(self, server) -> bool:
        try:
            return bool(server.is_populated())
        except Exception:  # noqa: BLE001
            return False

    def _idle_long_enough(self, name: str) -> bool:
        since = self._idle_since.get(name)
        return since is not None and time.time() - since >= self.cfg.idle_minutes * 60

    def _auto_pending(self, staging: str, name: str) -> Optional[dict]:
        p = self._pending(staging, name)
        if not os.path.exists(p):
            return None
        side = self._read_sidecar(p)
        return side if side.get("source") == AUTOUPDATE_SOURCE else None

    def pending_dll_servers(self) -> list[DllTarget]:
        """Local servers with an auto-staged engine DLL waiting."""
        return [t for t in self.dll_targets()
                if not t.remote and self._auto_pending(t.staging_dir, t.dll_name)]

    async def _apply_phase(self) -> None:
        from core import Status

        cfg = self.cfg
        # bftools.exe: nothing holds it open between mission generations
        await self._apply_bftools()

        # bfdb.exe
        pm = self.procman
        if pm is not None and pm.enabled and self._auto_pending(pm.staging_dir, "bfdb.exe"):
            servers = list(self.cog.bot.servers.values())
            go = cfg.bfdb_apply == "immediately" or (
                cfg.bfdb_apply == "when_idle" and in_window(cfg.apply_window)
                and all(self._idle_long_enough(s.name) or s.status not in (Status.RUNNING, Status.PAUSED)
                        for s in servers))
            if go:
                await self._notify("🔁 Applying staged **bfdb.exe** (auto-update) -- dashboard and GCI "
                                   "blip for a few seconds.")
                await pm.restart(self.cog._bfdb_admin_password)

        # engine DLLs: needs that DCS server down
        for t in self.pending_dll_servers():
            s = t.server
            if s.status not in (Status.RUNNING, Status.PAUSED):
                continue  # swaps in by itself at its next start
            if cfg.apply == "next_restart":
                continue
            if cfg.apply == "when_idle":
                if not (in_window(cfg.apply_window) and self._idle_long_enough(t.name)):
                    continue
            await self.restart_dcs(s, f"applying staged {t.dll_name} (auto-update, {cfg.apply})")

    async def restart_dcs(self, server, why: str) -> None:
        """Full DCS stop/start (a mission restart keeps the old DLL loaded).
        BFBinaries' prepare() swaps the staged DLL in on the way up."""
        self._orderly[server.name] = time.time()
        await self._notify(f"🔁 **{server.name}**: restarting DCS -- {why}.")
        self._history("dcs_restart", why, server=server.name)
        try:
            await server.shutdown()
            await server.startup()
        except Exception as ex:  # noqa: BLE001
            self.log.error(f"FowlEngine/autoupdate: restart of {server.name} failed: {ex}")
            await self._notify(f"⚠️ **{server.name}**: DCS restart failed ({ex}).")

    async def _apply_bftools(self) -> None:
        staging = self.global_staging()
        side = self._auto_pending(staging, "bftools.exe") if staging else None
        live = self.bftools_path()
        if not side or not live:
            return
        pending = self._pending(staging, "bftools.exe")
        backup = f"{live}.backup-{_now_tag()}"
        try:
            if os.path.exists(live):
                shutil.copy2(live, backup)
            os.replace(pending, live)
        except OSError as ex:
            # in use by a mission generation right now -- next tick
            self.log.info(f"FowlEngine/autoupdate: bftools.exe busy, retrying later ({ex})")
            return
        try:
            os.remove(pending + ".json")
        except OSError:
            pass
        self._prune_backups(live)
        self._history("applied", f"bftools.exe {side.get('tag')}", tag=side.get("tag"))
        await self._notify(f"🧩 bftools.exe updated to {side.get('tag')} (backup `{os.path.basename(backup)}`).")

    @staticmethod
    def _prune_backups(live: str, keep: int = 5) -> None:
        d, base = os.path.dirname(live), os.path.basename(live)
        try:
            backups = sorted((f for f in os.listdir(d) if f.startswith(f"{base}.backup-")), reverse=True)
        except OSError:
            return
        for stale in backups[keep:]:
            try:
                os.remove(os.path.join(d, stale))
            except OSError:
                pass

    # ---- 4. probation (engine DLLs; bfdb's lives in procman) ---------------

    def begin_dll_probation(self, server, dll_name: str, live: str, backup: Optional[str],
                            sidecar: dict) -> None:
        """Called by the BFBinaries extension right after it swapped a DLL in."""
        if sidecar.get("rollback") or sidecar.get("source") == ROLLBACK_SOURCE:
            return  # a rollback is the known-good build; nothing to watch
        home = getattr(getattr(server, "instance", None), "home", None)
        probation = self.state.setdefault("probation", {})
        probation[server.name] = {
            "server": server.name,
            "dll": dll_name,
            "live": live,
            "backup": backup,
            "tag": sidecar.get("tag"),
            "git": sidecar.get("git"),
            "source": sidecar.get("source") or "manual",
            "swapped_at": time.time(),
            "sidecar": os.path.join(home, "Logs", BUILD_SIDECARS[dll_name])
            if home and dll_name in BUILD_SIDECARS else None,
            "running_secs": 0.0,
            "loaded_at": None,
            "crashes": 0,
        }
        self._orderly.pop(server.name, None)
        self._history("probation_start", f"{dll_name} on {server.name}", tag=sidecar.get("tag"),
                      server=server.name)
        # also remember what's installed, for the Ops page
        inst = self.state.setdefault("installed", {})
        inst[f"{dll_name}@{server.name}"] = {"tag": sidecar.get("tag"), "git": sidecar.get("git"),
                                             "sha256": sidecar.get("sha256"), "at": _utc_now(),
                                             "source": sidecar.get("source") or "manual"}
        self._save_state()

    def _dll_loaded(self, p: dict) -> bool:
        side = p.get("sidecar")
        if not side:
            # no build sidecar for this engine: stable-running is the only signal
            return p["running_secs"] >= 120
        # The sidecar is rewritten every time bflib initialises a mission, and
        # the new DLL is the only one on disk -- so a sidecar newer than the
        # swap means the new engine loaded. Its git label is only compared
        # for the record: a hand-built release can carry a label that isn't
        # the one baked into the binary, and that must not read as "never
        # loaded" and trigger a rollback.
        try:
            if os.path.getmtime(side) < p["swapped_at"] - 5:
                return False
        except OSError:
            return False
        if p.get("git") and "git_seen" not in p:
            try:
                with open(side, encoding="utf-8") as fh:
                    got = str(json.load(fh).get("git") or "")
            except (OSError, ValueError):
                got = ""
            p["git_seen"] = got
            if got and got.split("-")[0] != str(p["git"]).split("-")[0]:
                self.log.warning(f"FowlEngine/autoupdate: {p['server']} loaded {p['dll']} reporting git "
                                 f"{got}, the release said {p['git']}")
        return True

    async def _probation_phase(self, dt: float) -> None:
        from core import Status

        probation = self.state.get("probation") or {}
        if not probation:
            return
        servers = {s.name: s for s in self.cog.bot.servers.values()}
        changed = False
        for name, p in list(probation.items()):
            s = servers.get(name)
            if s is None:
                continue
            st = s.status
            prev = self._last_status.get(name)
            if st == Status.SHUTTING_DOWN:
                self._orderly[name] = time.time()
            if st in (Status.RUNNING, Status.PAUSED):
                p["running_secs"] = p.get("running_secs", 0.0) + dt
                changed = True
            went_down = (prev in ("RUNNING", "PAUSED", "LOADING")
                         and st in (Status.SHUTDOWN, Status.UNREGISTERED))
            orderly = time.time() - self._orderly.get(name, 0) < 300
            if went_down and not orderly:
                p["crashes"] = p.get("crashes", 0) + 1
                changed = True
                self.log.warning(f"FowlEngine/autoupdate: {name} went down unexpectedly during "
                                 f"{p['dll']} probation ({p['crashes']})")
            if not p.get("loaded_at") and self._dll_loaded(p):
                p["loaded_at"] = time.time()
                changed = True
                self._history("probation_loaded", f"{p['dll']} loaded on {name}", tag=p.get("tag"),
                              server=name)
            if p.get("crashes", 0) >= 1 and self.cfg.rollback_on_crash:
                await self.rollback_dll(s, f"DCS crashed {p['crashes']}x after the swap")
                continue
            if not p.get("loaded_at") and p.get("running_secs", 0) > self.cfg.load_timeout_minutes * 60:
                await self.rollback_dll(s, f"the mission never loaded the new {p['dll']} "
                                           f"({self.cfg.load_timeout_minutes:.0f} min running)")
                continue
            if (p.get("loaded_at") and st in (Status.RUNNING, Status.PAUSED)
                    and time.time() - p["loaded_at"] >= self.cfg.probation_minutes * 60):
                probation.pop(name, None)
                changed = True
                self._history("probation_passed", f"{p['dll']} on {name}", tag=p.get("tag"), server=name)
                await self._notify(f"✅ **{name}**: `{p['dll']}` {p.get('tag') or '(manual upload)'} "
                                   f"passed probation.")
        if changed:
            self._save_state()

    async def rollback_dll(self, server, why: str) -> str:
        """Stage the pre-swap backup as a rollback and restart DCS onto it."""
        from core import Status

        probation = self.state.setdefault("probation", {})
        p = probation.pop(server.name, None)
        from .upload import engine_binaries
        b = engine_binaries(self.cog, server)
        dll = (p or {}).get("dll") or b["dll_name"]
        backup = (p or {}).get("backup") or self._newest_backup(b["dll_path"])
        if not backup or not os.path.exists(backup):
            self._save_state()
            msg = f"⛔ **{server.name}**: wanted to roll back `{dll}` ({why}) but there is no backup to restore."
            await self._notify(msg)
            return msg
        staging = b["staging_dir"] or self.global_staging()
        side = {"uploader": "auto-rollback", "source": ROLLBACK_SOURCE, "rollback": True,
                "utc": datetime.now(timezone.utc).isoformat(), "sha256": sha256_file(backup),
                "notes": f"rollback: {why}", "targets": [server.name]}
        try:
            self._stage_local(backup, staging, dll, side)
        except OSError as ex:
            msg = f"⛔ **{server.name}**: rollback of `{dll}` failed to stage ({ex})."
            await self._notify(msg)
            return msg
        tag = (p or {}).get("tag")
        if tag:
            self.mark_bad(tag, f"{dll} on {server.name}: {why}")
            self._cancel_tag_everywhere(tag)
        self._history("rollback", f"{dll} on {server.name}: {why}", tag=tag, server=server.name)
        await self._notify(f"⏪ **{server.name}**: rolling `{dll}` back to "
                           f"`{os.path.basename(backup)}` -- {why}."
                           + (f" Release {tag} is marked bad and won't be staged again." if tag else ""))
        if server.status in (Status.RUNNING, Status.PAUSED):
            await self.restart_dcs(server, f"rollback of {dll}")
        elif server.status in (Status.SHUTDOWN, Status.UNREGISTERED):
            self._orderly[server.name] = time.time()
            try:
                await server.startup()
            except Exception as ex:  # noqa: BLE001
                self.log.warning(f"FowlEngine/autoupdate: startup after rollback failed: {ex}")
        return f"rolled back {dll} on {server.name}"

    @staticmethod
    def _newest_backup(live: str) -> Optional[str]:
        if not live:
            return None
        d, base = os.path.dirname(live), os.path.basename(live)
        try:
            backups = sorted((f for f in os.listdir(d) if f.startswith(f"{base}.backup-")), reverse=True)
        except OSError:
            return None
        return os.path.join(d, backups[0]) if backups else None

    def _cancel_tag_everywhere(self, tag: str) -> None:
        """A release just failed somewhere: pull it from every staging dir it
        is still waiting in, so the other servers never get it."""
        dirs = {t.staging_dir for t in self.dll_targets() if not t.remote}
        dirs.add(self.global_staging())
        for d in dirs:
            for name in ENGINE_FILES:
                p = self._pending(d, name)
                if os.path.exists(p) and self._read_sidecar(p).get("tag") == tag:
                    for f in (p, p + ".json"):
                        try:
                            os.remove(f)
                        except OSError:
                            pass

    def on_bfdb_rollback(self, tag: Optional[str], why: str) -> None:
        """procman rolled bfdb back by itself."""
        if tag:
            self.mark_bad(tag, f"bfdb.exe: {why}")
            self._cancel_tag_everywhere(tag)
        self._history("rollback", f"bfdb.exe: {why}", tag=tag)

    def on_bfdb_swapped(self, sidecar: dict) -> None:
        inst = self.state.setdefault("installed", {})
        inst["bfdb.exe"] = {"tag": sidecar.get("tag"), "git": sidecar.get("git"),
                            "sha256": sidecar.get("sha256"), "at": _utc_now(),
                            "source": sidecar.get("source") or "manual"}
        self._history("applied", "bfdb.exe", tag=sidecar.get("tag"))

    # ---- the loop -----------------------------------------------------------

    def _track_idle(self) -> None:
        from core import Status

        now = time.time()
        for s in list(self.cog.bot.servers.values()):
            if s.status in (Status.RUNNING, Status.PAUSED) and not self._server_populated(s):
                self._idle_since.setdefault(s.name, now)
            else:
                self._idle_since.pop(s.name, None)

    async def tick(self) -> None:
        """Every minute from the cog. Never raises."""
        now = time.monotonic()
        dt = min(max(now - self._last_tick, 0.0), 300.0)
        self._last_tick = now
        try:
            self._track_idle()
            await self._probation_phase(dt)
            if self.cfg.enabled and not self.cfg.paused and not self._lock.locked():
                last = self.state.get("last_check") or {}
                last_at = last.get("at")
                due = True
                if last_at:
                    try:
                        age = (datetime.now(timezone.utc) - datetime.strptime(
                            last_at, "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=timezone.utc)).total_seconds()
                        due = age >= self.cfg.check_minutes * 60
                    except ValueError:
                        due = True
                if due:
                    await self.check(reason="scheduled")
                await self._apply_phase()
        except Exception as ex:  # noqa: BLE001
            self.log.exception(f"FowlEngine/autoupdate: tick failed: {ex}")
        finally:
            for s in list(self.cog.bot.servers.values()):
                self._last_status[s.name] = getattr(s.status, "name", str(s.status))

    # ---- status for the Ops page / Discord ------------------------------------

    def status(self) -> dict:
        latest = dict(self.state.get("latest") or {})
        latest.pop("urls", None)
        latest.pop("dir", None)
        probation = []
        for name, p in (self.state.get("probation") or {}).items():
            probation.append({
                "server": name, "dll": p.get("dll"), "tag": p.get("tag"), "source": p.get("source"),
                "loaded": bool(p.get("loaded_at")), "running_secs": int(p.get("running_secs") or 0),
                "crashes": p.get("crashes", 0),
                "passes_in_secs": (max(0, int(self.cfg.probation_minutes * 60 - (time.time() - p["loaded_at"])))
                                   if p.get("loaded_at") else None),
            })
        pm = self.procman
        if pm is not None and getattr(pm, "probation", None):
            bp = pm.probation
            probation.append({"server": None, "dll": "bfdb.exe", "tag": bp.get("tag"),
                              "source": bp.get("source"), "loaded": bool(bp.get("healthy_at")),
                              "running_secs": int(time.time() - bp.get("swapped_at", time.time())),
                              "crashes": bp.get("exits", 0), "passes_in_secs": None})
        return {
            "config": self.cfg.public(),
            "busy": self._busy,
            "last_check": self.state.get("last_check"),
            "latest": latest or None,
            "installed": self.state.get("installed") or {},
            "bad": sorted(self.bad),
            "probation": probation,
            "idle_since": {k: datetime.fromtimestamp(v, timezone.utc).isoformat()
                           for k, v in self._idle_since.items()},
            "history": list(reversed((self.state.get("history") or [])[-60:])),
        }
