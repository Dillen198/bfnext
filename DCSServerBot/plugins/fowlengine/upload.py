"""
Discord drag-and-drop upload of new engine binaries (bflib.dll / bfrange.dll /
bfdb.exe).

An admin drops a binary into the bot's admin channel; this stages it as
`<staging_dir>/<name>.pending` (+ a `.pending.json` sidecar with
uploader/time/size/sha/targets). The actual swap happens later,
backup-and-replace, on the next DCS restart -- see extensions/bfbinaries for
the engine DLLs and plugins/fowlengine/procman.py for bfdb.exe.

Where each file goes:

  bfdb.exe     once, into the global `bfdb.staging_dir` -- one bfdb serves
               every DCS server on the box.
  bflib.dll    into the staging dir of every DCS instance that runs the
  bfrange.dll  campaign engine (bflib) / the training range (bfrange), i.e.
               whose BFBinaries `dll_name` (or `kind:` in bfdb.instances) says
               so. Each instance's own BFBinaries `staging_dir` from nodes.yaml,
               falling back to the global `bfdb.staging_dir`.

Which instances: dropped into a server's own channel -> that server only (and
only if it runs that engine); dropped into the bot's common admin channel ->
every server that runs that engine. Each one swaps it in on its own next
restart, so a DLL can never land on the other engine's server.

Built on DCSServerBot's native upload primitives (core.utils.discord.
NodeUploadHandler.is_valid + node.write_file), same as the mission/modmanager
plugins, so it respects the DCS Admin role gate and is audited.
"""
from __future__ import annotations

import json
import os
from datetime import datetime, timezone
from typing import Callable, Iterable, Optional

__all__ = [
    "BFBINARY_PATTERNS", "ENGINE_DLLS", "KIND_DLL", "handle_bfbinary_upload",
    "global_staging_dir", "resolve_engine_binaries", "engine_binaries",
    "pick_targets", "group_by_staging_dir", "is_remote_node",
]

# exact-match filenames we accept (matched case-insensitively by is_valid)
BFBINARY_PATTERNS = [r"^bflib\.dll$", r"^bfrange\.dll$", r"^bfdb\.exe$"]
ENGINE_DLLS = ("bflib.dll", "bfrange.dll")
ACCEPTED = ENGINE_DLLS + ("bfdb.exe",)
# instance kind (bfdb.instances `kind:`) -> the engine DLL it loads
KIND_DLL = {"campaign": "bflib.dll", "range": "bfrange.dll"}

# generous sanity ceilings -- a real engine binary is tens of MB
_MAX_SIZE = 300 * 1024 * 1024


# ---- pure config resolution (no discord / bot imports) ---------------------

def _clean(val) -> Optional[str]:
    """A configured path/name, %ENV%-expanded; None for unset, blank or an
    un-edited `REPLACE_WITH_...` placeholder from an old sample file."""
    if val is None:
        return None
    s = str(val).strip()
    if not s or s.upper().startswith("REPLACE_WITH"):
        return None
    return os.path.expandvars(s)


def global_staging_dir(config: Optional[dict]) -> str:
    """The plugin-wide staging dir: bfdb.staging_dir, else <bfdb.home>/_staging.
    Same rule as Procman.staging_dir."""
    b = (config or {}).get("bfdb") or {}
    raw = _clean(b.get("staging_dir"))
    if raw:
        return raw
    home = _clean(b.get("home"))
    return os.path.join(home, "_staging") if home else ""


def resolve_engine_binaries(ext_cfg: Optional[dict], plugin_cfg: Optional[dict], kind: str,
                            instance_home: Optional[str], global_staging: str) -> dict:
    """Which engine DLL one DCS instance runs, where it lives and where its
    updates are staged. Mirrors extensions/bfbinaries so both ends agree.

    ext_cfg      the instance's BFBinaries block from nodes.yaml
    plugin_cfg   the instance's merged fowlengine config (legacy `bfbinaries:`)
    kind         "campaign" | "range" (bfdb.instances `kind:`)
    """
    ext_cfg = ext_cfg or {}
    plugin_cfg = plugin_cfg or {}
    legacy = plugin_cfg.get("bfbinaries") or {}
    name = (_clean(ext_cfg.get("dll_name")) or _clean(legacy.get("dll_name"))
            or KIND_DLL.get(kind, "bflib.dll"))
    name = os.path.basename(name).lower()
    path = (_clean(ext_cfg.get("dll_path")) or _clean(ext_cfg.get("bflib_dll_path"))
            or _clean(legacy.get("dll_path")))
    if not path and name == "bflib.dll":
        # The top-level `bflib_dll_path` is inherited from DEFAULT by every
        # server, so it can only ever describe a campaign engine.
        path = _clean(legacy.get("bflib_dll_path")) or _clean(plugin_cfg.get("bflib_dll_path"))
    if not path and instance_home:
        path = os.path.join(instance_home, "Scripts", name)
    staging = _clean(ext_cfg.get("staging_dir"))
    source = "BFBinaries"
    if not staging:
        staging = _clean(legacy.get("staging_dir"))
        source = "fowlengine bfbinaries"
    if not staging:
        staging = global_staging or ""
        source = "global bfdb.staging_dir"
    return {
        "dll_name": name,
        "dll_path": path or "",
        "staging_dir": staging,
        "staging_source": source,
        "has_extension": bool(ext_cfg),
        "kind": kind,
    }


def engine_binaries(cog, server) -> dict:
    """resolve_engine_binaries() for a live DCSServerBot server via the cog."""
    ext = cog._ext_cfg(server, "BFBinaries") if hasattr(cog, "_ext_cfg") else {}
    pcfg = cog.get_config(server) or {}
    kind = "campaign"
    kind_of = getattr(cog, "_instance_kind", None)
    if kind_of:
        try:
            kind = kind_of(server)
        except Exception:  # noqa: BLE001
            kind = "campaign"
    home = getattr(getattr(server, "instance", None), "home", None)
    return resolve_engine_binaries(ext, pcfg, kind, home,
                                   global_staging_dir(cog.get_config() or {}))


def pick_targets(dll_name: str, servers: Iterable, binaries_of: Callable[[object], dict],
                 scoped=None) -> tuple[list, Optional[str]]:
    """The (server, binaries) pairs a dropped engine DLL is staged for, or
    ([], why-not). `scoped` is the server whose own channel it was dropped in."""
    dll_name = dll_name.lower()
    if scoped is not None:
        b = binaries_of(scoped)
        if b["dll_name"] == dll_name:
            return [(scoped, b)], None
        return [], (f"**{scoped.name}** runs `{b['dll_name']}`, not `{dll_name}` -- drop it in "
                    f"the common admin channel to stage it for every server that runs it.")
    out = []
    for s in servers:
        b = binaries_of(s)
        if b["dll_name"] == dll_name:
            out.append((s, b))
    if not out:
        return [], (f"no server here runs `{dll_name}` (check `dll_name` on the BFBinaries "
                    f"extension in nodes.yaml, or `kind:` in bfdb.instances).")
    return out, None


def group_by_staging_dir(targets: list) -> dict:
    """{(node name, staging dir): [(server, binaries), ...]}, in target order --
    two servers that share a staging dir get ONE pending file between them."""
    groups: dict = {}
    for server, b in targets:
        node = getattr(getattr(server, "node", None), "name", None)
        key = (node, os.path.normcase(os.path.normpath(b["staging_dir"])) if b["staging_dir"] else "")
        groups.setdefault(key, []).append((server, b))
    return groups


# ---- the Discord handler ---------------------------------------------------

def _int(val) -> Optional[int]:
    try:
        return int(val)
    except (TypeError, ValueError):
        return None


def is_remote_node(node) -> bool:
    """True for a DCSServerBot agent node on another PC: its files are only
    reachable through the node API, never with local os/open calls."""
    return bool(getattr(node, "is_remote", False))


async def _stage_one(node, staging_dir: str, name: str, att, notes: str, author,
                     targets: list[str]) -> tuple[Optional[dict], Optional[str]]:
    """Write one `<name>.pending` + sidecar. Returns (sidecar, None) or (None, error).

    On an agent node on another PC the node downloads the attachment itself
    (node.write_file); no sidecar is written there, so /feops stage_status
    shows the file without the uploader details."""
    from core import UploadStatus
    from .procman import sha256_of

    remote = is_remote_node(node)
    if remote:
        try:
            await node.create_directory(staging_dir)
        except Exception:  # noqa: BLE001 - it may exist already; write_file says if not
            pass
    else:
        try:
            os.makedirs(staging_dir, exist_ok=True)
        except OSError as ex:
            return None, f"cannot create `{staging_dir}`: {ex}"
    pending = os.path.join(staging_dir, f"{name}.pending")
    rc = await node.write_file(pending, att.url, overwrite=True)
    if rc != UploadStatus.OK:
        where = f" on node {getattr(node, 'name', '?')}" if remote else ""
        return None, f"write failed{where}: {getattr(rc, 'name', rc)}"
    if remote:
        import hashlib
        try:
            digest = hashlib.sha256(await att.read()).hexdigest()
        except Exception:  # noqa: BLE001
            digest = None
        return {
            "uploader": str(author),
            "uploader_id": getattr(author, "id", None),
            "utc": datetime.now(timezone.utc).isoformat(),
            "size": att.size,
            "sha256": digest,
            "notes": notes,
            "targets": targets,
            "node": getattr(node, "name", None),
        }, None
    sidecar = {
        "uploader": str(author),
        "uploader_id": getattr(author, "id", None),
        "utc": datetime.now(timezone.utc).isoformat(),
        "size": os.path.getsize(pending) if os.path.exists(pending) else att.size,
        "sha256": sha256_of(pending),
        "notes": notes,
        "targets": targets,
    }
    try:
        with open(pending + ".json", "w", encoding="utf-8") as fh:
            json.dump(sidecar, fh, indent=2)
    except OSError:
        pass
    return sidecar, None


async def handle_bfbinary_upload(cog, message) -> bool:
    """Returns True if the message was a (valid or rejected) engine-binary
    upload attempt that this handler consumed, False if it was unrelated."""
    from core.utils.discord import ServerUploadHandler

    bot = cog.bot
    roles = bot.roles["DCS Admin"]
    if not ServerUploadHandler.is_valid(message, patterns=BFBINARY_PATTERNS, roles=roles):
        return False

    # Only act in a channel that is ours: a server's own channel scopes the
    # upload to that server, the common admin channel means "every server
    # that runs this engine". Anywhere else it is ignored, as before.
    scoped = bot.get_server(message, admin_only=True)
    if isinstance(scoped, list):  # some bot versions hand back a 1-element list
        scoped = scoped[0] if scoped else None
    common_admin = _int(((getattr(bot, "locals", None) or {}).get("channels") or {}).get("admin"))
    if scoped is None and message.channel.id != common_admin:
        return True

    notes = (message.content or "").strip()
    servers = list(bot.servers.values())
    procman = getattr(cog, "procman", None)
    local_node = getattr(cog, "node", None)
    lines: list[str] = []
    warnings: list[str] = []
    staged_any = False

    for att in message.attachments:
        name = (att.filename or "").lower()
        if name not in ACCEPTED:
            continue
        if att.size > _MAX_SIZE:
            await message.channel.send(
                f"❌ `{name}` is {att.size / 1024 / 1024:.0f} MB -- refusing, that's not a real engine binary."
            )
            continue

        if name == "bfdb.exe":
            staging = procman.staging_dir if procman else global_staging_dir(cog.get_config() or {})
            if not staging:
                await message.channel.send(
                    "❌ `bfdb.staging_dir` (or `bfdb.home`) is not configured in fowlengine.yaml.")
                continue
            node = local_node or (scoped.node if scoped else (servers[0].node if servers else None))
            sidecar, err = await _stage_one(node, staging, name, att, notes, message.author, ["bfdb"])
            if err:
                await message.channel.send(f"❌ Failed to stage `{name}`: {err}")
                continue
            staged_any = True
            lines.append(f"• `{name}` — {sidecar['size'] / 1024 / 1024:.1f} MB, "
                         f"`sha256:{(sidecar['sha256'] or '')[:12]}` → bfdb (applies on its next restart)")
            await bot.audit(f'staged engine binary "{name}"', server=scoped, user=message.author)
            continue

        targets, why = pick_targets(name, servers, lambda s: engine_binaries(cog, s), scoped)
        if not targets:
            await message.channel.send(f"❌ Not staging `{name}`: {why}")
            continue
        for (_node_name, staging), group in group_by_staging_dir(targets).items():
            names = [s.name for s, _ in group]
            if not group[0][1]["staging_dir"]:
                await message.channel.send(
                    f"❌ No staging dir for {', '.join(names)}: set `staging_dir` on its BFBinaries "
                    f"extension in nodes.yaml (or `bfdb.staging_dir` in fowlengine.yaml).")
                continue
            staging_dir = group[0][1]["staging_dir"]
            node = getattr(group[0][0], "node", None) or local_node
            sidecar, err = await _stage_one(node, staging_dir, name, att, notes, message.author, names)
            if err:
                await message.channel.send(f"❌ Failed to stage `{name}` for {', '.join(names)}: {err}")
                continue
            staged_any = True
            when = _next_restart_hint(group[0][0])
            on_node = f" on node `{sidecar['node']}`" if sidecar.get("node") else ""
            lines.append(f"• `{name}` — {sidecar['size'] / 1024 / 1024:.1f} MB, "
                         f"`sha256:{(sidecar['sha256'] or '')[:12]}` → **{', '.join(names)}**{when}"
                         f"\n   `{staging_dir}`{on_node}")
            if len(group) > 1:
                warnings.append(
                    f"⚠️ {', '.join(names)} share one staging dir, so whichever restarts first "
                    f"takes `{name}` and the other keeps its old engine. Give each its own "
                    f"BFBinaries `staging_dir` in nodes.yaml.")
            for s, b in group:
                if not b["has_extension"]:
                    warnings.append(
                        f"⚠️ **{s.name}** has no BFBinaries extension, so nothing swaps `{name}` in "
                        f"on restart -- use `/feops stage_apply` once it is shut down.")
            for s, _ in group:
                await bot.audit(f'staged engine binary "{name}"', server=s, user=message.author)

    if not staged_any:
        return True

    out = ["✅ Staged -- each server swaps it in on its own next DCS restart:"]
    out += lines
    out += warnings
    out.append("Use `/feops stage_apply` to swap it in now, or `/feops stage_cancel` to discard.")
    await message.channel.send("\n".join(out)[:1990])
    return True


def _next_restart_hint(server) -> str:
    restart_at = getattr(server, "restart_time", None)
    if not restart_at:
        return ""
    try:
        return f" (<t:{int(restart_at.timestamp())}:R>)"
    except (AttributeError, TypeError, ValueError):
        return ""
