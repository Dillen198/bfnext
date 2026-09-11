"""
Discord drag-and-drop upload of new engine binaries (bflib.dll / bfdb.exe).

An admin drops `bflib.dll` or `bfdb.exe` into the bot's admin channel; this
stages it as `<staging_dir>/<name>.pending` (+ a `.pending.json` sidecar with
uploader/time/size/sha). The actual swap happens later, backup-and-replace, on
the next scheduled DCS restart -- see extensions/bfbinaries for bflib.dll and
plugins/fowlengine/procman.py for bfdb.exe.

Built on DCSServerBot's native upload primitives (core.utils.discord.
NodeUploadHandler.is_valid + node.write_file), same as the mission/modmanager
plugins, so it respects the admin channel + DCS Admin role gate and is audited.
"""
from __future__ import annotations

import json
import os
from datetime import datetime, timezone

import discord

from core import UploadStatus
from core.utils.discord import ServerUploadHandler

__all__ = ["BFBINARY_PATTERNS", "handle_bfbinary_upload"]

# exact-match filenames we accept
BFBINARY_PATTERNS = [r"^bflib\.dll$", r"^bfdb\.exe$"]

# generous sanity ceilings -- a real bflib.dll/bfdb.exe is tens of MB
_MAX_SIZE = 300 * 1024 * 1024


async def handle_bfbinary_upload(cog, message: discord.Message) -> bool:
    """Returns True if the message was a (valid or rejected) engine-binary
    upload attempt that this handler consumed, False if it was unrelated."""
    roles = cog.bot.roles["DCS Admin"]
    if not ServerUploadHandler.is_valid(message, patterns=BFBINARY_PATTERNS, roles=roles):
        return False

    server = await ServerUploadHandler.get_server(message)
    if not server:
        return True  # it was addressed to us, just no server resolved

    config = cog.get_config(server) or {}
    staging_dir = os.path.expandvars(
        (config.get("bfdb") or {}).get("staging_dir")
        or os.path.join((config.get("bfdb") or {}).get("home", ""), "_staging")
    )
    if not staging_dir:
        await message.channel.send(
            "❌ `bfdb.staging_dir` (or `bfdb.home`) is not configured in fowlengine.yaml."
        )
        return True
    os.makedirs(staging_dir, exist_ok=True)

    notes = (message.content or "").strip()
    staged = []
    for att in message.attachments:
        name = att.filename
        if name not in ("bflib.dll", "bfdb.exe"):
            continue
        if att.size > _MAX_SIZE:
            await message.channel.send(
                f"❌ `{name}` is {att.size / 1024 / 1024:.0f} MB -- refusing, that's not a real engine binary."
            )
            continue

        pending = os.path.join(staging_dir, f"{name}.pending")
        rc = await server.node.write_file(pending, att.url, overwrite=True)
        if rc != UploadStatus.OK:
            await message.channel.send(f"❌ Failed to stage `{name}`: {rc.name}")
            continue

        from .procman import sha256_of

        sidecar = {
            "uploader": str(message.author),
            "uploader_id": message.author.id,
            "utc": datetime.now(timezone.utc).isoformat(),
            "size": os.path.getsize(pending),
            "sha256": sha256_of(pending),
            "notes": notes,
        }
        try:
            with open(pending + ".json", "w", encoding="utf-8") as fh:
                json.dump(sidecar, fh, indent=2)
        except OSError:
            pass

        staged.append((name, sidecar))
        await cog.bot.audit(f'staged engine binary "{name}"', server=server, user=message.author)

    if not staged:
        return True

    when = _next_restart_hint(server)
    lines = ["✅ Staged, will be applied on the next scheduled DCS restart" + when + ":"]
    for name, sc in staged:
        lines.append(f"• `{name}` — {sc['size'] / 1024 / 1024:.1f} MB, `sha256:{(sc['sha256'] or '')[:12]}`")
    lines.append("Use `/feops stage apply` to swap it in now, or `/feops stage cancel` to discard.")
    await message.channel.send("\n".join(lines))
    return True


def _next_restart_hint(server) -> str:
    restart_at = getattr(server, "restart_time", None)
    if not restart_at:
        return ""
    try:
        return f" (<t:{int(restart_at.timestamp())}:R>)"
    except (AttributeError, TypeError, ValueError):
        return ""
