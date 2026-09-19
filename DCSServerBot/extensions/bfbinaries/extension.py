"""
BFBinaries -- applies a staged engine build on the next DCS (re)start.

An admin stages a new `bflib.dll` (drag it into the bot's admin channel, see
plugins/fowlengine/upload.py). This extension's `prepare()` hook runs right
before every DCS start, while DCS.exe is guaranteed to be down and its file
lock on `bflib.dll` released -- the one race-free moment to swap the engine.
It timestamps a backup, replaces the live DLL, and never blocks a restart: a
bad swap is rolled back and the server still comes up (same stance as
extensions/bfweather).

A staged `bfdb.exe` is handled by plugins/fowlengine/procman.py on its own
bfdb restarts; if one is pending when DCS restarts, this extension nudges the
FowlEngine plugin to cycle bfdb so the new build lands promptly.

nodes.yaml:
  MyNode:
    instances:
      MyInstance:
        extensions:
          BFBinaries:
            bflib_dll_path: 'C:\\Users\\ATPAdmin\\Saved Games\\DCS.vectorstrike_1\\Scripts\\bflib.dll'
            staging_dir:    'C:\\Users\\ATPAdmin\\Saved Games\\DCS.vectorstrike_1\\_staging'
            keep_backups: 5
"""
from __future__ import annotations

import os
import shutil
from datetime import datetime, timezone

from core import Extension, Server
from typing_extensions import override

__all__ = ["BFBinaries"]


def _now_tag() -> str:
    return datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S")


class BFBinaries(Extension):
    def __init__(self, server: Server, config: dict):
        super().__init__(server, config)

    # ---- config ----------------------------------------------------------

    @property
    def _dll_path(self) -> str:
        return os.path.expandvars(self.config.get("bflib_dll_path", ""))

    @property
    def _staging_dir(self) -> str:
        return os.path.expandvars(self.config.get("staging_dir", ""))

    @property
    def _keep(self) -> int:
        return int(self.config.get("keep_backups", 5))

    def _pending(self, name: str) -> str:
        return os.path.join(self._staging_dir, f"{name}.pending")

    @override
    def is_available(self) -> bool:
        if not self._dll_path:
            self.log.error(f"  => {self.name}: 'bflib_dll_path' is not set in nodes.yaml.")
            return False
        if not self._staging_dir:
            self.log.error(f"  => {self.name}: 'staging_dir' is not set in nodes.yaml.")
            return False
        return True

    # ---- the swap ------------------------------------------------------

    def _prune_backups(self) -> None:
        d = os.path.dirname(self._dll_path)
        base = os.path.basename(self._dll_path)
        try:
            backups = sorted(
                (f for f in os.listdir(d) if f.startswith(f"{base}.backup-")), reverse=True
            )
        except OSError:
            return
        for stale in backups[self._keep:]:
            try:
                os.remove(os.path.join(d, stale))
            except OSError:
                pass

    def _swap_bflib(self) -> str | None:
        pending = self._pending("bflib.dll")
        if not os.path.exists(pending):
            return None
        live = self._dll_path
        backup = f"{live}.backup-{_now_tag()}"
        try:
            if os.path.exists(live):
                shutil.copy2(live, backup)
            os.makedirs(os.path.dirname(live), exist_ok=True)
            os.replace(pending, live)
        except OSError as ex:
            self.log.error(f"{self.name}: staged bflib.dll swap failed ({ex}) -- restoring")
            if os.path.exists(backup) and not os.path.exists(live):
                try:
                    shutil.copy2(backup, live)
                except OSError:
                    pass
            return f"⚠️ staged `bflib.dll` swap FAILED ({ex}); kept the previous engine."
        for side in (pending + ".json",):
            if os.path.exists(side):
                try:
                    os.remove(side)
                except OSError:
                    pass
        self._prune_backups()
        note = f"engine updated: swapped in staged `bflib.dll` (backup `{os.path.basename(backup)}`)."
        self.log.warning(f"{self.name}: {note}")
        return note

    def _nudge_bfdb_if_pending(self) -> None:
        if not os.path.exists(self._pending("bfdb.exe")):
            return
        try:
            from services.bot import BotService
            from core import ServiceRegistry

            bot = ServiceRegistry.get(BotService).bot
            cog = bot.cogs.get("FowlEngine")
            procman = getattr(cog, "procman", None)
            pw = getattr(cog, "_bfdb_admin_password", None)
            if procman and procman.enabled and pw:
                self.loop.create_task(procman.restart(pw))
                self.log.info(f"{self.name}: staged bfdb.exe pending -- asked FowlEngine to cycle bfdb")
        except Exception as ex:  # noqa: BLE001
            self.log.warning(f"{self.name}: could not nudge bfdb for a staged update: {ex}")

    async def _announce(self, note: str) -> None:
        try:
            from services.bot import BotService
            from core import ServiceRegistry

            bot = ServiceRegistry.get(BotService).bot
            cog = bot.cogs.get("FowlEngine")
            if cog and hasattr(cog, "notify_ops"):
                await cog.notify_ops(f"🧩 {self.server.name}: {note}")
        except Exception as ex:  # noqa: BLE001
            self.log.debug(f"{self.name}: ops announce skipped: {ex}")

    @override
    async def prepare(self) -> bool:
        if not self.is_available():
            return True  # never block a restart
        note = self._swap_bflib()
        self._nudge_bfdb_if_pending()
        if note:
            await self._announce(note)
        return True

    @override
    async def render(self, param: dict | None = None) -> dict:
        pending = [
            n for n in ("bflib.dll", "bfdb.exe") if os.path.exists(self._pending(n))
        ]
        return {
            "name": self.name,
            "version": self.version,
            "value": ("staged: " + ", ".join(pending)) if pending else "up to date",
        }
