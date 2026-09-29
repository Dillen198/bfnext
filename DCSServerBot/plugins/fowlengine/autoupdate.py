"""
Automatic engine updates for the Fowl Engine stack, with a safety net.

What it does, on a loop driven by the FowlEngine cog:

  1. CHECK   a release source for a newer engine build: GitHub Releases on the
             repo (tags `engine-...`, published by deploy/publish-release.ps1),
             or a plain folder of releases on this PC / a LAN share.
  2. FETCH   the release's manifest.json and its minisign signature
             (manifest.json.sig), verify the signature against the public key
             pinned in fowlengine.yaml (`autoupdate.public_key`) -- no key, no
             signature or a bad one and NOTHING is fetched or staged -- then
             the release's files into <staging>/_downloads/<tag>/, each checked
             against the sha256 in that (now authenticated) manifest. A file
             that doesn't match is never staged.
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

Release manifest (manifest.json, one per release -- see publish-release.ps1,
which also writes manifest.json.sig; deploy/auto-update.md has the key setup):

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
      "campaign-vs2.zip": {"sha256": "...", "size": 789, "key": "vs2",
                           "cfg_name": "RGW2008_CFG",
                           "contents": {"RGW2008_CFG": {"sha256": "...", "role": "cfg"},
                                        "rgw2008_1.0.0.miz": {"sha256": "...", "role": "miz"}}},
      ...
    }
  }

CAMPAIGN PACKS (`campaign-<key>.zip`, opt-in with `autoupdate.campaigns`): one
DCS server's campaign config + mission files, flat in the zip. <key> is that
server's bfdb instance `id` (bfdb.instances) or, on a box without instances,
its DCSServerBot instance name (`DCS.vectorstrike_1`). A pack is staged into
<staging>/_campaigns/<key>/<tag>/ and written at that server's next DCS start,
from BFBinaries' prepare() like an engine DLL, after a timestamped backup:
the cfg to <write dir>\\<cfg_name> (where bflib reads <sortie>_CFG), each .miz
over the file of that name in the mission list / Missions folder. It is never
written over a file somebody edited on the server since our last write -- the
pack is HELD until an admin says apply (overwrite) or keep (the OPS page,
/feops campaign_apply | campaign_keep). After it is written the server is on
probation like a new DLL: a crash, a mission that never gets going, or the
engine refusing the cfg restores the backup and that tag is never retried.

  staged ──(DCS start)──> probation ──> applied
    │ ▲                        └──(crash / refused / never loaded)──> failed
    ▼ │ apply
   held ──keep──> dismissed (or staged, missions only)

The plugin stamp (plugins/fowlengine/.fowl-plugin.json) says which build of
this plugin is installed and where it came from (an engine release or Fowl
Engine Manager's bundle). Both installers refuse to replace a NEWER plugin
with an older one, so the Manager's pre-start sync and a `bot_plugin` release
can't undo each other.

Everything here is best-effort and never raises into the cog's loop: an
unreachable GitHub, a half-downloaded file or a server that won't start is a
logged, reported, retried condition -- never a dead bot.
"""
from __future__ import annotations

import asyncio
import hashlib
import json
import os
import re
import shutil
import time
import zipfile
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any, Awaitable, Callable, Optional

from .icons import icon, plain

__all__ = [
    "Updater", "UpdateConfig", "ENGINE_FILES", "parse_manifest", "pick_github_release",
    "pick_folder_release", "file_needs_update", "in_window", "sha256_file",
    "AUTOUPDATE_SOURCE", "ROLLBACK_SOURCE", "campaign_key_of", "campaign_conflicts",
    "plugin_stamp_older", "PLUGIN_STAMP",
]

# Sidecar `source:` values. upload.py writes none (a human uploaded it).
AUTOUPDATE_SOURCE = "autoupdate"
ROLLBACK_SOURCE = "rollback"

# Every file a release may carry, and what it is.
ENGINE_FILES = ("bflib.dll", "bfrange.dll", "bfdb.exe", "bftools.exe")
DLL_FILES = ("bflib.dll", "bfrange.dll")
BOT_PLUGIN_ZIP = "fowlengine-bot.zip"
MANIFEST = "manifest.json"
MANIFEST_SIG = "manifest.json.sig"

# Campaign packs: campaign-<key>.zip. The key becomes a file name on both
# ends, hence the narrow alphabet (the long DCSServerBot server names, with
# their brackets and pipes, can't be one -- see campaign_keys()).
CAMPAIGN_PREFIX = "campaign-"
CAMPAIGN_KEY_RE = re.compile(r"^[a-z0-9][a-z0-9_.-]{0,63}$")
CAMPAIGN_BACKUPS = "_fowl_campaign_backups"
# What bflib logs once a mission, cfg included, is fully up -- and what it
# logs when it refuses one (bflib/src/lib.rs delayed_init_miz / on_mission_load_end).
ENGINE_LOG_UP = "starting timed events"
ENGINE_LOG_REFUSED = "THE MISSION CANNOT START"

# Which build of this plugin is installed, and who put it there. Written by
# _apply_bot_plugin and by Fowl Engine Manager's sync (bfmanager bot.rs).
PLUGIN_STAMP = ".fowl-plugin.json"

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


def _write_json_atomic(path: str, doc: Any) -> None:
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, indent=2)
    os.replace(tmp, path)


def _copy_atomic(src: str, dst: str) -> None:
    """dst becomes src in one rename: DCS or bflib reading it mid-copy sees
    the old file or the new one, never half of each."""
    os.makedirs(os.path.dirname(dst) or ".", exist_ok=True)
    tmp = dst + ".fowl-new"
    shutil.copy2(src, tmp)
    os.replace(tmp, dst)


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
    # Stage + apply campaign-<key>.zip packs (cfg + mission files) for the
    # servers on this box. YAML only, like bot_plugin: it lets a release
    # rewrite a live campaign.
    campaigns: bool = False
    campaign_backups_keep: int = 5
    paused: bool = False
    # The minisign public key every release manifest must be signed with
    # ("RW..." or a `tauri signer` .pub file's contents). Unset = nothing is
    # ever staged.
    public_key: Optional[str] = None

    # Keys the Ops page may change at runtime (persisted as overrides in the
    # updater's state file, so no YAML is rewritten for a toggle). Deliberately
    # NOT bot_plugin: it lets a release unpack Python into the running bot,
    # which is a decision for whoever edits fowlengine.yaml on the box.
    RUNTIME_KEYS = ("enabled", "channel", "apply", "bfdb_apply", "idle_minutes",
                    "apply_window", "check_minutes", "paused", "rollback_on_crash",
                    "probation_minutes")

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
        c.campaigns = bool(raw.get("campaigns", c.campaigns))
        c.campaign_backups_keep = max(1, int(raw.get("campaign_backups_keep", c.campaign_backups_keep) or 1))
        c.paused = bool(raw.get("paused", c.paused))
        c.public_key = _clean(raw.get("public_key"))
        return c

    def key_id(self) -> Optional[str]:
        """The pinned key's id as minisign prints it, or None."""
        if not self.public_key:
            return None
        from .minisign import SignatureError, parse_public_key
        try:
            return parse_public_key(self.public_key)[0][::-1].hex().upper()
        except SignatureError:
            return "INVALID"

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
            "campaigns": self.campaigns, "campaign_backups_keep": self.campaign_backups_keep,
            "paused": self.paused, "signing_key_id": self.key_id(),
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
        key = campaign_key_of(lname)
        if lname not in ENGINE_FILES and lname != BOT_PLUGIN_ZIP and key is None:
            continue  # a newer publisher may ship things this bot doesn't know
        if not isinstance(meta, dict):
            raise ValueError(f"manifest entry for {name} is not an object")
        sha = str(meta.get("sha256") or "").lower()
        if not _is_sha(sha):
            raise ValueError(f"manifest entry for {name} has no valid sha256")
        clean_files[lname] = {
            "sha256": sha,
            "size": int(meta.get("size") or 0),
            "git": (str(meta.get("git")) if meta.get("git") else None),
            "built": (str(meta.get("built")) if meta.get("built") else None),
        }
        if key is not None:
            clean_files[lname]["campaign"] = _parse_campaign_meta(name, key, meta)
    if not clean_files:
        raise ValueError("manifest lists no engine files this bot knows")
    ch = str(doc.get("channel") or "stable").lower()
    return {
        "schema": int(doc.get("schema") or 1),
        "tag": tag,
        "git": (str(doc.get("git")) if doc.get("git") else None),
        "built": (str(doc.get("built")) if doc.get("built") else None),
        # when the release's commit was made -- what plugin stamps are ordered by
        "commit_time": (str(doc.get("commit_time")) if doc.get("commit_time") else None),
        "channel": ch if ch in ("stable", "beta") else "stable",
        "notes": str(doc.get("notes") or "")[:4000],
        "files": clean_files,
    }


def _is_sha(s: str) -> bool:
    return len(s) == 64 and all(c in "0123456789abcdef" for c in s)


def campaign_key_of(name: str) -> Optional[str]:
    """'campaign-vs2.zip' -> 'vs2'; None for anything that isn't a pack."""
    n = str(name).lower()
    if not (n.startswith(CAMPAIGN_PREFIX) and n.endswith(".zip")):
        return None
    key = n[len(CAMPAIGN_PREFIX):-4]
    return key if CAMPAIGN_KEY_RE.match(key) else None


def _flat_name(n: str) -> bool:
    return bool(n) and n not in (".", "..") and not any(ch in n for ch in '/\\:*?"<>|')


def _parse_campaign_meta(name: str, key: str, meta: dict) -> dict:
    """The per-file listing of one pack. It is inside the signed manifest, so
    once the zip's own sha256 matched these are authenticated too."""
    contents = meta.get("contents")
    if not isinstance(contents, dict) or not contents:
        raise ValueError(f"manifest entry for {name} lists no contents")
    cfg_name = str(meta.get("cfg_name") or "")
    files = {}
    for fname, fm in contents.items():
        fname = str(fname)
        if not _flat_name(fname) or not isinstance(fm, dict):
            raise ValueError(f"{name}: bad content entry {fname!r}")
        sha = str(fm.get("sha256") or "").lower()
        if not _is_sha(sha):
            raise ValueError(f"{name}: {fname} has no valid sha256")
        role = str(fm.get("role") or ("cfg" if fname == cfg_name else "miz")).lower()
        if role == "cfg" and fname != cfg_name:
            raise ValueError(f"{name}: {fname} is marked cfg but cfg_name is {cfg_name!r}")
        if role == "miz" and not fname.lower().endswith(".miz"):
            raise ValueError(f"{name}: {fname} is not a .miz")
        if role not in ("cfg", "miz"):
            raise ValueError(f"{name}: {fname} has unknown role {role!r}")
        files[fname] = {"sha256": sha, "role": role, "size": int(fm.get("size") or 0)}
    # bflib loads <write dir>\<sortie>_CFG; anything else would be written
    # where the engine never looks
    if cfg_name not in files or not cfg_name.endswith("_CFG"):
        raise ValueError(f"{name}: cfg_name {cfg_name!r} must be one of its files and end in _CFG")
    return {"key": key, "cfg_name": cfg_name, "contents": files}


def campaign_conflicts(want: dict, on_disk: dict, baseline: dict) -> list[str]:
    """Files a pack must NOT overwrite without asking: present on the server,
    different from the pack, and different from what we last wrote there (or
    first saw there). `want`/`on_disk`/`baseline` map file name -> sha256
    (on_disk None = no such file)."""
    out = []
    for name, sha in want.items():
        cur = on_disk.get(name)
        if cur is None or cur == sha:
            continue
        base = baseline.get(name)
        if base is not None and cur != base:
            out.append(name)
    return out


def _stamp_time(stamp: Optional[dict]) -> Optional[datetime]:
    """When the stamped plugin's code was committed (falling back to when it
    was built): the order two plugin builds are compared in."""
    for k in ("commit_time", "built"):
        v = (stamp or {}).get(k)
        if not v:
            continue
        try:
            t = datetime.fromisoformat(str(v).replace("Z", "+00:00"))
        except ValueError:
            continue
        return t if t.tzinfo else t.replace(tzinfo=timezone.utc)
    return None


# The other plugins in DCSServerBot/plugins/ this project ships next to
# fowlengine (they borrow its icon set). A release's fowlengine-bot.zip carries
# their code, never their *.yaml -- that is the operator's settings. Keep in
# step with COMMUNITY_PLUGINS in bfmanager/scripts/stage-bot.mjs.
COMMUNITY_PLUGINS = ("about", "announcements", "faq", "radio", "rules", "smartmod", "tickets")


def bot_zip_member_allowed(name: str) -> bool:
    """Whether fowlengine-bot.zip may write `name`: the fowlengine plugin, the
    bf* extensions, and the community plugins' code (not their yaml)."""
    if name.startswith("plugins/fowlengine/") or name.startswith("extensions/bf"):
        return True
    parts = name.split("/")
    if len(parts) >= 3 and parts[0] == "plugins" and parts[1] in COMMUNITY_PLUGINS:
        return not name.lower().endswith((".yaml", ".yml"))
    return False


def plugin_stamp_older(incoming: Optional[dict], installed: Optional[dict]) -> Optional[str]:
    """Why `incoming` must not replace `installed`, or None to go ahead. An
    unstamped install counts as older than anything (the pre-stamp builds);
    the same commit is never "older", dirty or not."""
    if not installed:
        return None
    base = lambda s: str((s or {}).get("git") or "").split("-")[0]  # noqa: E731
    if base(incoming) and base(incoming) == base(installed):
        return None
    t_in, t_have = _stamp_time(incoming), _stamp_time(installed)
    if t_in is None or t_have is None or t_in >= t_have:
        return None
    return (f"the installed plugin ({installed.get('source') or '?'} "
            f"{installed.get('git') or '?'}, {installed.get('commit_time') or installed.get('built')}) "
            f"is newer than this one ({(incoming or {}).get('git') or '?'}, "
            f"{(incoming or {}).get('commit_time') or (incoming or {}).get('built')})")


def read_plugin_stamp(path: str) -> Optional[dict]:
    try:
        with open(path, encoding="utf-8") as fh:
            doc = json.load(fh)
        return doc if isinstance(doc, dict) else None
    except (OSError, ValueError):
        return None


def verify_manifest(raw: bytes, sig_text: Optional[str], public_key: Optional[str]) -> dict:
    """Authenticate a release's manifest.json bytes against its minisign
    signature, then parse it. Raises ValueError (with the reason) for a
    missing key, a missing signature or one that doesn't verify -- nothing
    unsigned is ever staged."""
    from .minisign import SignatureError, verify

    if not public_key:
        raise ValueError("autoupdate.public_key is not set in fowlengine.yaml -- refusing to stage "
                         "unsigned engine releases (see deploy/auto-update.md, 'Signing releases')")
    if not sig_text or not sig_text.strip():
        raise ValueError(f"the release has no {MANIFEST_SIG} -- refusing an unsigned release")
    try:
        verify(raw, sig_text, public_key)
    except SignatureError as ex:
        raise ValueError(f"{MANIFEST} signature check failed: {ex} -- refusing the release") from None
    try:
        doc = json.loads(raw)
    except ValueError as ex:
        raise ValueError(f"{MANIFEST} is not JSON: {ex}") from None
    return parse_manifest(doc)


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
        # sample_status(): per-second status, and unexpected downs it counted
        # for servers on probation (consumed by _probation_phase)
        self._sampling = False
        self._fast_status: dict = {}
        self._unexpected_down: dict = {}
        self._unexpected_down_campaign: dict = {}
        # servers restart_dcs() bounced during this probation pass, so a DLL
        # rollback and a campaign rollback on one server share one restart
        self._restarted: set = set()

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
        # History is persisted and shown on the dashboard: never store emoji
        # markup in it (its ids die with an icon reinstall; the web can't draw it).
        h.append({"ts": _utc_now(), "event": event, "detail": plain(detail), **extra})
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
        if not cfg.public_key:
            # Say so before touching the network: without a pinned key there
            # is nothing a release could be checked against.
            raise RuntimeError("autoupdate.public_key is not set in fowlengine.yaml -- refusing to stage "
                               "unsigned engine releases (see deploy/auto-update.md, 'Signing releases')")
        root = self.download_root()
        os.makedirs(root, exist_ok=True)
        if cfg.source == "folder":
            if not cfg.folder:
                raise RuntimeError("autoupdate.source is folder but autoupdate.folder is not set")
            rel = await asyncio.get_running_loop().run_in_executor(
                None, lambda: pick_folder_release(cfg.folder, cfg.channel, self.bad))
            if not rel:
                return {"ok": True, "latest": None, "message": f"no usable release in {cfg.folder}"}

            def read_signed():
                with open(os.path.join(rel["root"], MANIFEST), "rb") as fh:
                    raw = fh.read()
                try:
                    with open(os.path.join(rel["root"], MANIFEST_SIG), encoding="utf-8") as fh:
                        sig = fh.read()
                except OSError:
                    sig = None
                return raw, sig
            raw, sig = await asyncio.get_running_loop().run_in_executor(None, read_signed)
            try:
                manifest = verify_manifest(raw, sig, cfg.public_key)
            except ValueError as ex:
                raise RuntimeError(f"release in {rel['root']}: {ex}") from None
            if manifest["tag"] != rel["tag"]:
                raise RuntimeError(f"{rel['root']} changed while it was being read -- retrying next check")
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
                if cfg.token:
                    headers.update({"Authorization": f"Bearer {cfg.token}",
                                    "Accept": "application/octet-stream"})

                async def asset_bytes(name: str) -> Optional[bytes]:
                    asset = massets.get(name)
                    if not asset:
                        return None
                    url = asset.get("url") if cfg.token else asset.get("browser_download_url")
                    async with http.get(url, headers=headers, timeout=30) as r:
                        if r.status != 200:
                            raise RuntimeError(f"{name} download -> HTTP {r.status}")
                        return await r.read()

                raw = await asset_bytes(MANIFEST)
                sig = await asset_bytes(MANIFEST_SIG)
                try:
                    manifest = verify_manifest(raw or b"", sig.decode("utf-8", "replace") if sig else None,
                                               cfg.public_key)
                except ValueError as ex:
                    raise RuntimeError(f"release {rel['tag']}: {ex}") from None
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
        staged = [plain(x) for x in staged]   # kept in state + shown on the OPS page
        return {"ok": True, "latest": manifest["tag"], "staged": staged,
                "message": (f"staged {', '.join(staged)}" if staged else "everything is up to date")}

    def _want_file(self, name: str) -> bool:
        if name == BOT_PLUGIN_ZIP:
            return self.cfg.bot_plugin
        key = campaign_key_of(name)
        if key is not None:
            # only this box's own servers' packs are ever downloaded
            return self.cfg.campaigns and key in self.campaign_key_map()
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

        if self.cfg.campaigns:
            out += await loop.run_in_executor(None, self._stage_campaigns, latest)

        if out:
            self._history("staged", "; ".join(out), tag=latest.get("tag"))
            await self._notify(
                f"{icon('update')} **Engine update {latest.get('tag')}** (`{latest.get('git') or '?'}`) staged: "
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
        node = t.server.node
        pending = self._pending(t.staging_dir, dll)
        part = pending + ".part"
        try:
            try:
                await node.create_directory(t.staging_dir)
            except Exception:  # noqa: BLE001
                pass
            rc = await node.write_file(part, url, overwrite=True)
            if rc != UploadStatus.OK:
                raise RuntimeError(getattr(rc, "name", rc))
            # The node fetched the URL itself, so what it wrote was never
            # checked here: read it back and hold it to the (signed)
            # manifest's sha256 before it may become the .pending file.
            data = await node.read_file(part)
            if not isinstance(data, (bytes, bytearray)):
                raise RuntimeError(f"could not read the download back ({data})")
            got = hashlib.sha256(data).hexdigest()
            if got != meta["sha256"]:
                raise RuntimeError(f"sha256 mismatch (got {got[:12]}, manifest says {meta['sha256'][:12]}) "
                                   f"-- refusing it")
            try:
                await node.remove_file(pending)
            except Exception:  # noqa: BLE001 - nothing staged there yet
                pass
            await node.rename_file(part, pending)
        except Exception as ex:  # noqa: BLE001
            self.log.warning(f"FowlEngine/autoupdate: staging {dll} on node {node.name} failed: {ex}")
            try:
                await node.remove_file(part)
            except Exception:  # noqa: BLE001
                pass
            return False
        remote_done[key] = latest.get("tag")
        self._save_state()
        return True

    # ---- bot plugin self-update (opt-in) -------------------------------------

    def _apply_bot_plugin(self, latest: dict, plugin_dir: Optional[str] = None) -> Optional[str]:
        """Unpack fowlengine-bot.zip over this plugin + its extensions. Takes
        effect on the next bot restart. Refuses when the plugin is a symlink or
        lives in a git checkout -- that is a developer's tree, not an install
        -- and when the plugin installed now is newer than the zip's (Fowl
        Engine Manager may have synced a later bundle; see PLUGIN_STAMP)."""
        tag = latest.get("tag")
        if self.state.get("bot_plugin_tag") == tag:
            return None
        plugin_dir = plugin_dir or os.path.dirname(os.path.abspath(__file__))
        bot_root = os.path.dirname(os.path.dirname(plugin_dir))
        if os.path.realpath(plugin_dir) != os.path.abspath(plugin_dir) or _inside_git_worktree(plugin_dir):
            self.log.warning("FowlEngine/autoupdate: bot_plugin is on but the plugin lives in a git "
                             "checkout / symlink -- not overwriting a working tree")
            self.state["bot_plugin_tag"] = tag
            return "bot plugin: skipped (plugin is a git checkout / symlink)"
        zpath = os.path.join(latest.get("dir") or "", BOT_PLUGIN_ZIP)
        stamp_member = f"plugins/fowlengine/{PLUGIN_STAMP}"
        try:
            with zipfile.ZipFile(zpath) as z:
                names = z.namelist()
                if "plugins/fowlengine/commands.py" not in names:
                    raise RuntimeError("zip has no plugins/fowlengine/commands.py")
                for n in names:
                    if n.startswith("/") or ".." in n.replace("\\", "/").split("/"):
                        raise RuntimeError(f"unsafe path in zip: {n}")
                    if not bot_zip_member_allowed(n):
                        raise RuntimeError(f"zip touches {n}, outside the fowlengine plugin/extensions")
                # A zip from before stamps: judge it by the release it came in.
                incoming = None
                if stamp_member in names:
                    try:
                        incoming = json.loads(z.read(stamp_member))
                    except ValueError:
                        incoming = None
                if not isinstance(incoming, dict):
                    incoming = {"git": latest.get("git"), "commit_time": latest.get("commit_time"),
                                "built": latest.get("built")}
                incoming = {**incoming, "schema": 1, "source": "engine-release", "tag": tag}
                older = plugin_stamp_older(incoming, read_plugin_stamp(os.path.join(plugin_dir, PLUGIN_STAMP)))
                if older:
                    self.log.info(f"FowlEngine/autoupdate: bot plugin from {tag} not applied: {older}")
                    self.state["bot_plugin_tag"] = tag
                    self._history("bot_plugin", f"skipped: {older}", tag=tag)
                    return f"bot plugin: skipped ({older})"
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
            # after the unpack, so the stamp always says what is really there
            _write_json_atomic(os.path.join(plugin_dir, PLUGIN_STAMP), incoming)
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
                await self._notify(f"{icon('restart')} Applying staged **bfdb.exe** (auto-update) -- dashboard and GCI "
                                   "blip for a few seconds.")
                await pm.restart_if_pending(self.cog._bfdb_admin_password)

        # engine DLLs and campaign packs: need that DCS server down. Both land
        # at the same start, so one restart covers a server with both.
        due: dict = {}
        for t in self.pending_dll_servers():
            due.setdefault(t.name, (t.server, []))[1].append(t.dll_name)
        if cfg.campaigns:
            for s, c in self._campaign_servers():
                if ((c.get("pack") or {}).get("status") == "staged"
                        and not self._is_remote(s) and self._has_bfbinaries(s)):
                    due.setdefault(s.name, (s, []))[1].append(f"campaign pack {c['pack'].get('tag')}")
        for name, (s, what) in due.items():
            if s.status not in (Status.RUNNING, Status.PAUSED):
                continue  # swaps in by itself at its next start
            if cfg.apply == "next_restart":
                continue
            if cfg.apply == "when_idle":
                if not (in_window(cfg.apply_window) and self._idle_long_enough(name)):
                    continue
            await self.restart_dcs(s, f"applying staged {', '.join(what)} (auto-update, {cfg.apply})")

    async def restart_dcs(self, server, why: str) -> None:
        """Full DCS stop/start (a mission restart keeps the old DLL loaded).
        BFBinaries' prepare() swaps the staged DLL in on the way up."""
        self._orderly[server.name] = time.time()
        self._restarted.add(server.name)
        await self._notify(f"{icon('restart')} **{server.name}**: restarting DCS -- {why}.")
        self._history("dcs_restart", why, server=server.name)
        try:
            await server.shutdown()
            await server.startup()
        except Exception as ex:  # noqa: BLE001
            self.log.error(f"FowlEngine/autoupdate: restart of {server.name} failed: {ex}")
            await self._notify(f"{icon('warning')} **{server.name}**: DCS restart failed ({ex}).")

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
        await self._notify(f"{icon('build')} bftools.exe updated to {side.get('tag')} (backup `{os.path.basename(backup)}`).")

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
        self._unexpected_down.pop(server.name, None)
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

    def sample_status(self) -> None:
        """Every second, from the cog. A scheduled restart or an admin's
        shutdown passes through SHUTTING_DOWN (DCSServerBot's do_shutdown)
        or STOPPED, often for only a few seconds -- the 30 s tick used to miss
        it and roll back a perfectly good engine as "crashed". A crash goes
        straight from RUNNING to SHUTDOWN/UNREGISTERED. Cheap: attribute
        reads only."""
        now = time.time()
        self._sampling = True
        on_probation = self.state.get("probation") or {}
        campaign_probation = {n for n, c in (self.state.get("campaigns") or {}).items() if c.get("probation")}
        for s in list(self.cog.bot.servers.values()):
            st = getattr(s.status, "name", str(s.status))
            prev = self._fast_status.get(s.name)
            self._fast_status[s.name] = st
            if st in ("SHUTTING_DOWN", "STOPPED"):
                self._orderly[s.name] = now
            if (prev in ("RUNNING", "PAUSED", "LOADING") and st in ("SHUTDOWN", "UNREGISTERED")
                    and now - self._orderly.get(s.name, 0) >= 300):
                if s.name in on_probation:
                    self._unexpected_down[s.name] = self._unexpected_down.get(s.name, 0) + 1
                if s.name in campaign_probation:
                    self._unexpected_down_campaign[s.name] = self._unexpected_down_campaign.get(s.name, 0) + 1

    async def _probation_phase(self, dt: float) -> None:
        # Campaign packs first: a failed one only stages its restore, and if
        # the DLL on the same server fails too its rollback restart picks the
        # restore up -- one restart, not two.
        self._restarted = set()
        bounce = await self._campaign_probation_phase(dt)
        await self._dll_probation_phase(dt)
        for name, (server, why) in bounce.items():
            if name not in self._restarted:
                await self._bounce(server, why)

    async def _bounce(self, server, why: str) -> None:
        """Get DCS through a fresh start (BFBinaries' prepare()) whatever
        state it is in: restart a running one, start a crashed one."""
        from core import Status

        if server.status in (Status.RUNNING, Status.PAUSED):
            await self.restart_dcs(server, why)
        elif server.status in (Status.SHUTDOWN, Status.UNREGISTERED):
            self._orderly[server.name] = time.time()
            self._restarted.add(server.name)
            try:
                await server.startup()
            except Exception as ex:  # noqa: BLE001
                self.log.warning(f"FowlEngine/autoupdate: startup of {server.name} failed: {ex}")

    async def _dll_probation_phase(self, dt: float) -> None:
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
            if self._sampling:
                # sample_status() saw every transition, a second apart, and
                # counted only the ones that skipped the orderly SHUTTING_DOWN
                crashed = self._unexpected_down.pop(name, 0) > 0
            else:
                # no status watch (tests, or it hasn't started): judge from
                # this tick's coarse before/after
                went_down = (prev in ("RUNNING", "PAUSED", "LOADING")
                             and st in (Status.SHUTDOWN, Status.UNREGISTERED))
                crashed = went_down and time.time() - self._orderly.get(name, 0) >= 300
            if crashed:
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
                await self._notify(f"{icon('good')} **{name}**: `{p['dll']}` {p.get('tag') or '(manual upload)'} "
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
            msg = f"{icon('blocked')} **{server.name}**: wanted to roll back `{dll}` ({why}) but there is no backup to restore."
            await self._notify(msg)
            return msg
        staging = b["staging_dir"] or self.global_staging()
        side = {"uploader": "auto-rollback", "source": ROLLBACK_SOURCE, "rollback": True,
                "utc": datetime.now(timezone.utc).isoformat(), "sha256": sha256_file(backup),
                "notes": f"rollback: {why}", "targets": [server.name]}
        try:
            self._stage_local(backup, staging, dll, side)
        except OSError as ex:
            msg = f"{icon('blocked')} **{server.name}**: rollback of `{dll}` failed to stage ({ex})."
            await self._notify(msg)
            return msg
        tag = (p or {}).get("tag")
        if tag:
            self.mark_bad(tag, f"{dll} on {server.name}: {why}")
            self._cancel_tag_everywhere(tag)
        self._history("rollback", f"{dll} on {server.name}: {why}", tag=tag, server=server.name)
        await self._notify(f"{icon('rollback')} **{server.name}**: rolling `{dll}` back to "
                           f"`{os.path.basename(backup)}` -- {why}."
                           + (f" Release {tag} is marked bad and won't be staged again." if tag else ""))
        if server.status in (Status.RUNNING, Status.PAUSED):
            await self.restart_dcs(server, f"rollback of {dll}")
        elif server.status in (Status.SHUTDOWN, Status.UNREGISTERED):
            self._orderly[server.name] = time.time()
            self._restarted.add(server.name)
            try:
                await server.startup()
            except Exception as ex:  # noqa: BLE001
                self.log.warning(f"FowlEngine/autoupdate: startup after rollback failed: {ex}")
        return f"rolled back {dll} on {server.name}"

    @staticmethod
    def _newest_backup(live: str) -> Optional[str]:
        """The newest `.backup-*` that is not the build running now. A build
        that was rolled away from is kept as `.failed-*` by BFBinaries, never
        as a backup, so it can't be "restored" by a second rollback; and a
        backup identical to the live DLL would be a rollback that changes
        nothing."""
        if not live:
            return None
        d, base = os.path.dirname(live), os.path.basename(live)
        try:
            backups = sorted((f for f in os.listdir(d) if f.startswith(f"{base}.backup-")), reverse=True)
        except OSError:
            return None
        live_sha = sha256_file(live)
        for f in backups:
            path = os.path.join(d, f)
            if live_sha is None or sha256_file(path) != live_sha:
                return path
        return None

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
        # Its campaign packs too: a pack may need the engine it shipped with.
        # "cancelled", not failed -- if the release is allowed again, so are they.
        for name, c in (self.state.get("campaigns") or {}).items():
            pack = c.get("pack") or {}
            if pack.get("tag") == tag and pack.get("status") in ("staged", "held"):
                pack["status"] = "cancelled"
                pack["reason"] = "the release was rolled back elsewhere and marked bad"
                self._history("campaign_cancelled", f"{name}: {pack['reason']}", tag=tag, server=name)
        self._save_state()

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

    # ---- campaign packs (autoupdate.campaigns) ---------------------------------

    @staticmethod
    def _is_remote(server) -> bool:
        from .upload import is_remote_node
        return is_remote_node(getattr(server, "node", None))

    @staticmethod
    def _home(server) -> Optional[str]:
        return getattr(getattr(server, "instance", None), "home", None)

    def campaign_keys(self, server) -> list[str]:
        """The pack keys that mean this server, lower-case like pack names:
        its bfdb instance id (bfdb.instances[].id -- what campaigns.json
        should use: short, stable, "never change it"), then its DCSServerBot
        instance name (`dcs.vectorstrike_1`) for a box without instances."""
        keys: list[str] = []
        try:
            iid = self.cog._instance_id(server) if hasattr(self.cog, "_instance_id") else None
        except Exception:  # noqa: BLE001
            iid = None
        inst = getattr(getattr(server, "instance", None), "name", None)
        for k in (iid, inst):
            k = str(k or "").strip().lower()
            if k and CAMPAIGN_KEY_RE.match(k) and k not in keys:
                keys.append(k)
        return keys

    def campaign_key_map(self) -> dict:
        """{pack key: server} for the servers on THIS PC. A server on an agent
        node never gets a pack: its files live on another machine."""
        out: dict = {}
        for s in list(self.cog.bot.servers.values()):
            if self._is_remote(s):
                continue
            for k in self.campaign_keys(s):
                out.setdefault(k, s)
        return out

    def _has_bfbinaries(self, server) -> bool:
        """BFBinaries' prepare() is what writes a pack; without it a staged
        pack would sit there -- and `when_idle` restart the server for it
        again and again."""
        try:
            return bool(self.cog._ext_cfg(server, "BFBinaries"))
        except Exception:  # noqa: BLE001
            return False

    def _camp(self, name: str) -> dict:
        return self.state.setdefault("campaigns", {}).setdefault(name, {"server": name})

    def _campaign_servers(self):
        servers = {s.name: s for s in self.cog.bot.servers.values()}
        for name, c in list((self.state.get("campaigns") or {}).items()):
            if name in servers:
                yield servers[name], c

    def _cfg_target(self, server, cfg_name: str) -> Optional[str]:
        """<write dir>\\<cfg_name> -- bflib reads <sortie>_CFG from DCS's
        write dir (bfprotocols Cfg::load), nowhere else."""
        home = self._home(server)
        return os.path.join(home, cfg_name) if home else None

    def _miz_paths(self, server, name: str) -> dict:
        """Where one mission file of a pack goes. `primary` is the mission
        itself: the mission-list entry of that name, else a BFWeather template
        of that name, else <Missions>\\<name>. DCSServerBot keeps a pristine
        `.dcssb\\<name>.orig` beside a mission it modifies and copies it back
        over the mission at every start (and may run a `.dcssb\\<name>` copy):
        both are rewritten too, or that start would bring the old mission
        straight back. `orig`/`copy` are None when absent."""
        lname = name.lower()
        primary = None
        try:
            mlist = list((getattr(server, "settings", None) or {}).get("missionList") or [])
        except Exception:  # noqa: BLE001
            mlist = []
        for m in mlist:
            m = os.path.normpath(str(m))
            if os.path.basename(m).lower() != lname:
                continue
            d = os.path.dirname(m)
            if os.path.basename(d).lower() == ".dcssb":
                d = os.path.dirname(d)
            primary = os.path.join(d, os.path.basename(m))
            break
        if primary is None:
            try:
                bfw = self.cog._ext_cfg(server, "BFWeather") or {}
            except Exception:  # noqa: BLE001
                bfw = {}
            for k in ("base", "weapon", "options", "warehouse"):
                p = _clean(bfw.get(k))
                if p and os.path.basename(p).lower() == lname:
                    primary = os.path.normpath(p)
                    break
        if primary is None:
            home = self._home(server)
            mdir = getattr(getattr(server, "instance", None), "missions_dir", None) \
                or (os.path.join(home, "Missions") if home else None)
            if not mdir:
                return {"primary": None, "orig": None, "copy": None}
            primary = os.path.join(mdir, name)
        side = os.path.join(os.path.dirname(primary), ".dcssb", os.path.basename(primary))
        return {"primary": primary,
                "orig": side + ".orig" if os.path.exists(side + ".orig") else None,
                "copy": side if os.path.exists(side) else None}

    def _target_paths(self, server, files: dict) -> dict:
        """name -> {authored: the copy that says what the server runs, write:
        every path it is written to, new: no such mission yet}. For a mission
        DCSServerBot modifies, the authored copy is its .orig: the mission
        itself is rewritten (weather, time) at every start."""
        out = {}
        for n, f in files.items():
            if f.get("role") == "cfg":
                p = self._cfg_target(server, n)
                out[n] = {"authored": p, "write": [p] if p else [], "new": not (p and os.path.exists(p))}
                continue
            mp = self._miz_paths(server, n)
            if not mp["primary"]:
                out[n] = {"authored": None, "write": [], "new": True}
                continue
            out[n] = {"authored": mp["orig"] or mp["primary"],
                      "write": [p for p in (mp["primary"], mp["orig"], mp["copy"]) if p],
                      "new": not os.path.exists(mp["primary"])}
        return out

    @staticmethod
    def _authored_shas(targets: dict) -> dict:
        return {n: (sha256_file(t["authored"]) if t["authored"] and os.path.exists(t["authored"]) else None)
                for n, t in targets.items()}

    def _observe_campaign_baselines(self) -> None:
        """First sight of each server's *_CFG: remember its sha, so an edit
        made on the server from now on (the dashboard CONFIG page, a text
        editor) holds the next pack instead of being overwritten. An edit
        made before the bot ever saw the file can't be told from the file."""
        changed = False
        for s in list(self.cog.bot.servers.values()):
            home = self._home(s)
            if not home or self._is_remote(s):
                continue
            try:
                names = [f for f in os.listdir(home) if f.endswith("_CFG")
                         and os.path.isfile(os.path.join(home, f))]
            except OSError:
                continue
            for f in names:
                base = self._camp(s.name).setdefault("baseline", {})
                if f in base:
                    continue
                sha = sha256_file(os.path.join(home, f))
                if sha:
                    base[f] = sha
                    changed = True
        if changed:
            self._save_state()

    @staticmethod
    def _parse_cfg(data: bytes, name: str) -> dict:
        """A campaign cfg as bflib reads it (serde_json): UTF-8 JSON, no BOM
        (Python refuses one too), a JSON object."""
        try:
            doc = json.loads(data.decode("utf-8"))
        except (UnicodeDecodeError, ValueError) as ex:
            raise ValueError(f"{name} is not valid JSON ({ex}) -- the engine would refuse it") from None
        if not isinstance(doc, dict):
            raise ValueError(f"{name} is not a JSON object")
        return doc

    @classmethod
    def _netidx_mismatch(cls, current: Optional[str], new: str) -> Optional[str]:
        """A pack whose netidx_base differs from the running cfg's is most
        likely ANOTHER server's (a wrong key in campaigns.json), and would
        cut this engine off from bfdb. Asked about, never just applied."""
        try:
            with open(current or "", "rb") as fh:
                a = cls._parse_cfg(fh.read(), "current").get("netidx_base")
            with open(new, "rb") as fh:
                b = cls._parse_cfg(fh.read(), "new").get("netidx_base")
        except (OSError, ValueError):
            return None
        if a and b and a != b:
            return (f"its netidx_base {b!r} is not this server's {a!r} -- another server's pack? "
                    f"(if intended, bfdb.instances needs the new one too)")
        return None

    def _stage_campaigns(self, latest: dict) -> list[str]:
        """Stage every pack in the latest release that is for a server on this
        box and differs from what it runs. Returns lines for ops_channel."""
        out: list[str] = []
        tag = latest.get("tag")
        keymap = self.campaign_key_map()
        for fname, meta in (latest.get("files") or {}).items():
            camp = meta.get("campaign")
            if not camp:
                continue
            server = keymap.get(camp["key"])
            if server is None:
                continue
            c = self._camp(server.name)
            c["key"] = camp["key"]
            pack = c.get("pack") or {}
            if (tag in (c.get("failed") or []) or tag in (c.get("dismissed") or [])
                    or c.get("rejected") == tag
                    or (pack.get("tag") == tag and pack.get("status") != "cancelled")
                    # one change at a time: the next check stages it
                    or c.get("probation") or c.get("restore")
                    or (c.get("installed") or {}).get("sha256") == meta["sha256"]):
                continue
            try:
                note = self._stage_campaign(server, c, fname, meta, latest)
            except Exception as ex:  # noqa: BLE001
                # a pack that doesn't verify or parse won't next time either
                c["last_error"] = f"{tag}: {ex}"
                c["rejected"] = tag
                self.log.warning(f"FowlEngine/autoupdate: campaign pack {fname} for {server.name}: {ex}")
                self._history("campaign_rejected", f"{server.name}: {ex}", tag=tag, server=server.name)
                out.append(f"{icon('warning')} campaign pack for **{server.name}** refused: {ex}")
                continue
            if note:
                out.append(note)
        self._save_state()
        return out

    def _stage_campaign(self, server, c: dict, fname: str, meta: dict, latest: dict) -> Optional[str]:
        camp = meta["campaign"]
        tag = latest["tag"]
        if not self._has_bfbinaries(server):
            raise ValueError("this instance has no BFBinaries extension in nodes.yaml -- nothing would write "
                             "the pack at its start")
        want = {n: f["sha256"] for n, f in camp["contents"].items()}
        dest = os.path.join(self.global_staging(), "_campaigns", camp["key"], tag)
        shutil.rmtree(dest, ignore_errors=True)
        os.makedirs(dest, exist_ok=True)
        with zipfile.ZipFile(os.path.join(latest.get("dir") or "", fname)) as z:
            names = [n for n in z.namelist() if not n.endswith("/")]
            if sorted(names) != sorted(want):
                raise ValueError(f"{fname} holds {', '.join(sorted(names))} but the manifest lists "
                                 f"{', '.join(sorted(want))}")
            for n in names:
                data = z.read(n)
                if hashlib.sha256(data).hexdigest() != want[n]:
                    raise ValueError(f"{fname}: {n} does not match its sha256 in the signed manifest")
                if camp["contents"][n]["role"] == "cfg":
                    self._parse_cfg(data, n)
                with open(os.path.join(dest, n), "wb") as fh:
                    fh.write(data)
        targets = self._target_paths(server, camp["contents"])
        nowhere = [n for n, t in targets.items() if not t["write"]]
        if nowhere:
            raise ValueError(f"nowhere to write {', '.join(nowhere)} (no instance home / Missions folder)")
        on_disk = self._authored_shas(targets)
        base = c.setdefault("baseline", {})
        old = c.get("pack") or {}
        if old.get("dir") and os.path.normcase(old["dir"]) != os.path.normcase(dest):
            shutil.rmtree(old["dir"], ignore_errors=True)
        if all(on_disk.get(n) == s for n, s in want.items()):
            shutil.rmtree(dest, ignore_errors=True)
            c["pack"] = None
            c["installed"] = {"tag": tag, "sha256": meta["sha256"], "at": _utc_now(), "files": want,
                              "cfg_name": camp["cfg_name"]}
            base.update(want)
            self._history("campaign_current", f"{server.name} already runs these files", tag=tag,
                          server=server.name)
            return None
        held = campaign_conflicts(want, on_disk, base)
        for n, s in on_disk.items():
            if s is not None:
                base.setdefault(n, s)   # first observation
        reasons = []
        if held:
            reasons.append("server copy was edited since the last update: " + ", ".join(held))
        mismatch = self._netidx_mismatch(targets[camp["cfg_name"]]["authored"],
                                         os.path.join(dest, camp["cfg_name"]))
        if mismatch:
            reasons.append(mismatch)
            if camp["cfg_name"] not in held:
                held.append(camp["cfg_name"])
        status = "held" if reasons else "staged"
        c["pack"] = {
            "tag": tag, "git": meta.get("git") or latest.get("git"),
            "built": meta.get("built") or latest.get("built"), "sha256": meta["sha256"],
            "name": fname, "cfg_name": camp["cfg_name"], "files": camp["contents"], "dir": dest,
            "status": status, "reason": "; ".join(reasons) or None, "held": held,
            "decision": None, "apply_only": None, "staged_at": _utc_now(),
        }
        c.pop("last_error", None)
        self._history(f"campaign_{status}", f"{server.name}: {c['pack']['reason'] or ', '.join(want)}",
                      tag=tag, server=server.name)
        if status == "held":
            return (f"{icon('paused')} campaign pack for **{server.name}** HELD -- {c['pack']['reason']}. Decide on the OPS "
                    f"page (Campaign packs) or with `/feops campaign_apply` / `/feops campaign_keep`")
        return f"campaign pack → {server.name} ({', '.join(want)}, at its next DCS start)"

    def campaign_prepare(self, server) -> Optional[str]:
        """BFBinaries' prepare(), right before DCS starts (DCS is down, the
        mission not yet loaded): restore a failed pack's backup, or write a
        staged pack. Returns a line for ops_channel. Never raises -- a start
        is never blocked by this."""
        try:
            return self._campaign_prepare(server)
        except Exception as ex:  # noqa: BLE001
            self.log.exception(f"FowlEngine/autoupdate: campaign pack for {server.name}: {ex}")
            return f"{icon('warning')} campaign pack: {ex} -- starting with the files as they are."

    def _campaign_prepare(self, server) -> Optional[str]:
        c = (self.state.get("campaigns") or {}).get(server.name)
        if not c:
            return None
        if c.get("restore"):
            return self._restore_campaign(server, c)
        pack = c.get("pack")
        if not pack or pack.get("status") != "staged" or not self.cfg.campaigns or self._is_remote(server):
            return None
        tag = pack["tag"]
        names = list(pack.get("apply_only") or pack["files"])
        files = {n: pack["files"][n] for n in names}
        want = {n: f["sha256"] for n, f in files.items()}
        targets = self._target_paths(server, files)
        on_disk = self._authored_shas(targets)
        base = c.setdefault("baseline", {})
        if pack.get("decision") != "overwrite":
            # somebody may have edited it between staging and this start
            held = campaign_conflicts(want, on_disk, base)
            if held:
                pack.update(status="held", held=held,
                            reason="server copy was edited since the last update: " + ", ".join(held))
                self._history("campaign_held", f"{server.name}: {pack['reason']}", tag=tag, server=server.name)
                self._save_state()
                return f"{icon('paused')} campaign pack {tag} HELD at start -- {pack['reason']}."
        todo = [n for n in names if on_disk.get(n) != want[n]]
        for n in todo:
            if sha256_file(os.path.join(pack["dir"], n)) != want[n]:
                c["pack"] = None   # the next check stages it afresh
                c["last_error"] = f"{tag}: the staged copy of {n} is missing or changed"
                self._save_state()
                return f"{icon('warning')} campaign pack {tag}: its staged {n} is missing or changed -- dropped, the next check stages it again."
        prev_installed = c.get("installed")
        installed_files = dict((prev_installed or {}).get("files") or {})
        installed_files.update(want)
        c["installed"] = {"tag": tag, "sha256": pack["sha256"], "at": _utc_now(), "files": installed_files,
                          "cfg_name": pack["cfg_name"], "partial": bool(pack.get("apply_only"))}
        if pack.get("decision") == "keep":
            c["dismissed"] = (list(c.get("dismissed") or []) + [tag])[-20:]
        for n in names:
            base[n] = want[n]
        if not todo:
            c["pack"] = None
            self._history("campaign_current", f"{server.name} already runs these files", tag=tag, server=server.name)
            self._save_state()
            return None

        home = self._home(server)
        backup_dir = os.path.join(home, CAMPAIGN_BACKUPS, f"{_now_tag()}-{tag}")
        writes = [(os.path.join(pack["dir"], n), dst) for n in todo for dst in targets[n]["write"]]
        entries = []
        try:
            os.makedirs(backup_dir, exist_ok=True)
            for i, (_src, dst) in enumerate(writes):
                bname = None
                if os.path.exists(dst):
                    bname = f"{i:02d}-{os.path.basename(dst)}"
                    shutil.copy2(dst, os.path.join(backup_dir, bname))
                entries.append({"path": dst, "backup": bname})
            _write_json_atomic(os.path.join(backup_dir, "backup.json"),
                               {"tag": tag, "server": server.name, "at": _utc_now(), "entries": entries,
                                "files": {n: {"role": files[n]["role"]} for n in todo}})
        except OSError as ex:
            c["installed"] = prev_installed
            c["last_error"] = f"{tag}: backup failed ({ex})"
            self._save_state()
            return f"{icon('warning')} campaign pack {tag} NOT written: could not back up the current files ({ex})."
        try:
            for src, dst in writes:
                _copy_atomic(src, dst)
        except OSError as ex:
            problems = self._restore_entries(backup_dir, entries)
            c["installed"] = prev_installed
            c["last_error"] = f"{tag}: write failed ({ex})"
            self._save_state()
            return (f"{icon('warning')} campaign pack {tag} NOT written ({ex}); the previous files are back"
                    + (f" except: {'; '.join(problems)}" if problems else "") + ". Retried at the next start.")

        log_path, off, head = self._engine_log_mark(server)
        c["probation"] = {"tag": tag, "backup": backup_dir, "swapped_at": time.time(), "running_secs": 0.0,
                          "loaded_at": None, "crashes": 0, "prev_installed": prev_installed, "files": todo,
                          "log": log_path, "log_offset": off, "log_head": head}
        pack["status"] = "probation"
        c.pop("last_error", None)
        # the restart that brought us here was orderly; a crash from here on is not
        self._orderly.pop(server.name, None)
        self._unexpected_down_campaign.pop(server.name, None)
        self._prune_campaign_backups(home)
        self._history("campaign_applied", f"{server.name}: {', '.join(todo)}", tag=tag, server=server.name)
        self._save_state()
        new = [n for n in todo if targets[n]["new"] and files[n]["role"] == "miz"]
        note = f"campaign pack {tag} written: {', '.join(todo)} (backup `{os.path.basename(backup_dir)}`)."
        if new:
            note += (f" New mission file(s) {', '.join(new)}: this start still runs the mission in the list "
                     f"-- add/select it with /mission if that's the plan.")
        return note

    @staticmethod
    def _restore_entries(bdir: str, entries: list) -> list[str]:
        problems = []
        for e in reversed(entries):
            try:
                if e.get("backup"):
                    _copy_atomic(os.path.join(bdir, e["backup"]), e["path"])
                elif os.path.exists(e["path"]):
                    os.remove(e["path"])   # the pack added it
            except OSError as ex:
                problems.append(f"{e.get('path')}: {ex}")
        return problems

    def _restore_campaign(self, server, c: dict) -> str:
        bdir = c.get("restore")
        c["restore"] = None
        try:
            with open(os.path.join(bdir, "backup.json"), encoding="utf-8") as fh:
                doc = json.load(fh)
        except (OSError, TypeError, ValueError) as ex:
            self._save_state()
            return (f"{icon('blocked')} campaign restore for {server.name} impossible: backup unreadable ({ex}) -- restore by "
                    f"hand from `{bdir}`.")
        problems = self._restore_entries(bdir, doc.get("entries") or [])
        # what is on disk again is what we last wrote there
        base = c.setdefault("baseline", {})
        files = doc.get("files") or {}
        for n, sha in self._authored_shas(self._target_paths(server, files)).items():
            if sha:
                base[n] = sha
            else:
                base.pop(n, None)
        self._history("campaign_restored", f"{server.name}: {', '.join(files)}"
                      + (f" -- problems: {'; '.join(problems)}" if problems else ""),
                      tag=doc.get("tag"), server=server.name)
        self._save_state()
        if problems:
            return f"{icon('blocked')} campaign files restored from `{os.path.basename(bdir)}` with problems: {'; '.join(problems)}"
        return f"campaign files restored from `{os.path.basename(bdir)}` (pack {doc.get('tag')} rolled back)."

    def _list_campaign_backups(self, home: Optional[str]) -> list[str]:
        if not home:
            return []
        try:
            return sorted((e.name for e in os.scandir(os.path.join(home, CAMPAIGN_BACKUPS)) if e.is_dir()),
                          reverse=True)
        except OSError:
            return []

    def _prune_campaign_backups(self, home: str) -> None:
        for stale in self._list_campaign_backups(home)[self.cfg.campaign_backups_keep:]:
            shutil.rmtree(os.path.join(home, CAMPAIGN_BACKUPS, stale), ignore_errors=True)

    def _engine_log(self, server) -> Optional[str]:
        home = self._home(server)
        try:
            is_range = bool(self.cog._is_range(server)) if hasattr(self.cog, "_is_range") else False
        except Exception:  # noqa: BLE001
            is_range = False
        # bfrange logs neither marker: a range is judged on "stayed up" alone
        return os.path.join(home, "Logs", "bfnext.txt") if home and not is_range else None

    @staticmethod
    def _log_head(path: str) -> Optional[str]:
        try:
            with open(path, "rb") as fh:
                return hashlib.sha256(fh.read(256)).hexdigest()
        except OSError:
            return None

    def _engine_log_mark(self, server) -> tuple:
        p = self._engine_log(server)
        if not p:
            return None, 0, None
        try:
            size = os.path.getsize(p)
        except OSError:
            size = 0
        return p, size, self._log_head(p)

    def _scan_engine_log(self, p: dict) -> Optional[str]:
        """What bflib logged since the pack was written: "up", "refused: ..."
        or None (nothing decisive yet)."""
        path = p.get("log")
        if not path:
            return None
        try:
            size = os.path.getsize(path)
        except OSError:
            return None
        off = int(p.get("log_offset") or 0)
        head = self._log_head(path)
        if size < off or head != p.get("log_head"):
            off = 0   # bflib renames the old log aside at every start (rotate_log)
        try:
            with open(path, "rb") as fh:
                fh.seek(off)
                data = fh.read(8 << 20)
        except OSError:
            return None
        cut = data.rfind(b"\n")
        data = data[:cut + 1] if cut >= 0 else b""   # a line still being written waits
        p["log_offset"] = off + len(data)
        p["log_head"] = head
        text = data.decode("utf-8", "replace")
        for line in text.splitlines():
            if ENGINE_LOG_REFUSED in line:
                return "refused: " + line.strip()[:300]
        return "up" if ENGINE_LOG_UP in text else None

    async def _campaign_probation_phase(self, dt: float) -> dict:
        """Judge every freshly written pack. Returns {server name: (server,
        why)} for the ones to restart onto their restored files."""
        from core import Status

        bounce: dict = {}
        servers = {s.name: s for s in self.cog.bot.servers.values()}
        changed = False
        for name, c in list((self.state.get("campaigns") or {}).items()):
            p = c.get("probation")
            s = servers.get(name)
            if not p or s is None:
                continue
            st = s.status
            prev = self._last_status.get(name)
            if st in (Status.RUNNING, Status.PAUSED):
                p["running_secs"] = p.get("running_secs", 0.0) + dt
                changed = True
            if self._sampling:
                crashed = self._unexpected_down_campaign.pop(name, 0) > 0
            else:
                went_down = (prev in ("RUNNING", "PAUSED", "LOADING")
                             and st in (Status.SHUTDOWN, Status.UNREGISTERED))
                crashed = went_down and time.time() - self._orderly.get(name, 0) >= 300
            if crashed:
                p["crashes"] = p.get("crashes", 0) + 1
                changed = True
            why = None
            if not p.get("loaded_at"):
                seen = await asyncio.get_running_loop().run_in_executor(None, self._scan_engine_log, p)
                changed = True
                if seen == "up" or (not p.get("log") and p.get("running_secs", 0) >= 120):
                    p["loaded_at"] = time.time()
                    self._history("campaign_loaded", f"{name}: the mission came up", tag=p.get("tag"), server=name)
                elif seen:
                    why = f"the engine refused the mission ({seen[len('refused: '):]})"
            timeout = self.cfg.load_timeout_minutes * 60
            if why is None and p.get("crashes", 0) >= 1 and self.cfg.rollback_on_crash:
                why = f"DCS crashed {p['crashes']}x after the campaign files changed"
            if why is None and not p.get("loaded_at") and p.get("running_secs", 0) > timeout:
                why = f"the mission never came up ({self.cfg.load_timeout_minutes:.0f} min running)"
            if why is None and st == Status.LOADING and time.time() - p.get("swapped_at", time.time()) > timeout:
                why = f"DCS never got past LOADING ({self.cfg.load_timeout_minutes:.0f} min)"
            if why:
                await self._notify(self._fail_campaign(s, c, why))
                bounce[name] = (s, f"restoring the previous campaign files ({why})")
                changed = True
                continue
            if (p.get("loaded_at") and st in (Status.RUNNING, Status.PAUSED)
                    and time.time() - p["loaded_at"] >= self.cfg.probation_minutes * 60):
                c["probation"] = None
                c["last_applied"] = {"tag": p.get("tag"), "at": _utc_now(), "result": "passed",
                                     "backup": p.get("backup"), "files": p.get("files")}
                pack = c.get("pack") or {}
                if pack.get("tag") == p.get("tag"):
                    shutil.rmtree(pack.get("dir") or "", ignore_errors=True)
                    c["pack"] = None
                changed = True
                self._history("campaign_passed", f"{name}", tag=p.get("tag"), server=name)
                await self._notify(f"{icon('good')} **{name}**: campaign pack {p.get('tag')} passed probation.")
        if changed:
            self._save_state()
        return bounce

    def _fail_campaign(self, server, c: dict, why: str) -> str:
        """Stage the pre-write backup for the next start and never offer this
        tag to this server again."""
        p = c.get("probation") or {}
        tag = p.get("tag")
        c["probation"] = None
        c["failed"] = ([t for t in (c.get("failed") or []) if t != tag] + [tag])[-20:] if tag else c.get("failed")
        c["restore"] = p.get("backup")
        c["installed"] = p.get("prev_installed")
        pack = c.get("pack") or {}
        if pack.get("tag") == tag:
            pack.update(status="failed", reason=why)
        c["last_applied"] = {"tag": tag, "at": _utc_now(), "result": "failed", "reason": why,
                             "backup": p.get("backup"), "files": p.get("files")}
        self._history("campaign_rollback", f"{server.name}: {why}", tag=tag, server=server.name)
        return (f"{icon('rollback')} **{server.name}**: campaign pack {tag} failed -- {why}. The previous cfg + mission are "
                f"restored at the restart, and {tag} won't be applied here again.")

    def campaign_decide(self, server_name: str, decision: str, who: str = "") -> str:
        """An admin's answer to a held (or staged) pack. `apply`: write it
        over the server's copy at the next start (backup kept). `keep`: keep
        the server's cfg (and any mission edited there); the pack's other
        missions still go in, and the pack is dismissed for this server."""
        c = (self.state.get("campaigns") or {}).get(server_name)
        pack = (c or {}).get("pack") or {}
        if pack.get("status") not in ("held", "staged"):
            raise LookupError(f"{server_name} has no held or staged campaign pack")
        tag = pack["tag"]
        by = f" (by {who})" if who else ""
        if decision == "apply":
            pack.update(status="staged", decision="overwrite", reason=None, held=[], apply_only=None)
            self._history("campaign_decision", f"{server_name}: apply over the server's copy{by}", tag=tag,
                          server=server_name)
            self._save_state()
            when = {"next_restart": "its next DCS restart",
                    "when_idle": "its next DCS start (restarted once empty)",
                    "immediately": "a DCS restart within the minute"}.get(self.cfg.apply, "its next DCS start")
            return f"{tag} will overwrite {server_name}'s campaign files at {when}; a backup is kept."
        if decision != "keep":
            raise ValueError("decision must be apply or keep")
        server = {s.name: s for s in self.cog.bot.servers.values()}.get(server_name)
        if server is None:
            raise LookupError(f"no server named {server_name!r}")
        miz = {n: f for n, f in pack["files"].items() if f["role"] == "miz"}
        want = {n: f["sha256"] for n, f in miz.items()}
        on_disk = self._authored_shas(self._target_paths(server, miz))
        edited = set(campaign_conflicts(want, on_disk, c.get("baseline") or {}))
        only = [n for n in miz if n not in edited and on_disk.get(n) != want[n]]
        if only:
            pack.update(status="staged", decision="keep", apply_only=only, held=[],
                        reason=f"keeping the server's {pack['cfg_name']}; only {', '.join(only)} will be written")
            msg = f"kept {server_name}'s {pack['cfg_name']}; {', '.join(only)} from {tag} go in at its next DCS start."
        else:
            c["dismissed"] = (list(c.get("dismissed") or []) + [tag])[-20:]
            shutil.rmtree(pack.get("dir") or "", ignore_errors=True)
            c["pack"] = None
            msg = f"kept {server_name}'s campaign files; {tag} is dismissed there."
        self._history("campaign_decision", f"{server_name}: keep the server's copy{by}", tag=tag, server=server_name)
        self._save_state()
        return msg

    def campaign_status(self) -> dict:
        """The OPS page's Campaign packs section: one row per DCS server."""
        camps = self.state.get("campaigns") or {}
        rows = []
        for s in list(self.cog.bot.servers.values()):
            c = camps.get(s.name) or {}
            pack = c.get("pack")
            p = c.get("probation")
            remote = self._is_remote(s)
            rows.append({
                "server": s.name,
                "keys": self.campaign_keys(s),
                "key": c.get("key"),
                "remote": remote,
                "installed": c.get("installed"),
                "pack": {**{k: pack.get(k) for k in ("tag", "git", "built", "status", "reason", "held", "decision",
                                                      "apply_only", "staged_at", "cfg_name")},
                         "files": sorted(pack.get("files") or {})} if pack else None,
                "probation": {
                    "tag": p.get("tag"), "running_secs": int(p.get("running_secs") or 0),
                    "loaded": bool(p.get("loaded_at")), "crashes": p.get("crashes", 0),
                    "passes_in_secs": (max(0, int(self.cfg.probation_minutes * 60 - (time.time() - p["loaded_at"])))
                                       if p.get("loaded_at") else None),
                } if p else None,
                "last_applied": c.get("last_applied"),
                "failed": list(c.get("failed") or [])[-10:],
                "dismissed": list(c.get("dismissed") or [])[-10:],
                "restore_pending": bool(c.get("restore")),
                "last_error": c.get("last_error"),
                "backups": [] if remote else self._list_campaign_backups(self._home(s))[:10],
            })
        return {"enabled": self.cfg.campaigns, "servers": rows}

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
            if self.cfg.campaigns:
                await asyncio.get_running_loop().run_in_executor(None, self._observe_campaign_baselines)
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
            "campaigns": self.campaign_status(),
            "history": list(reversed((self.state.get("history") or [])[-60:])),
        }
