"""
BFBinaries -- applies a staged engine build on the next DCS (re)start.

An admin stages a new engine DLL (drag it into the bot's admin channel, see
plugins/fowlengine/upload.py). This extension's `prepare()` hook runs right
before every DCS start, while DCS.exe is guaranteed to be down and its file
lock on the DLL released -- the one race-free moment to swap the engine.
It timestamps a backup, replaces the live DLL, and never blocks a restart: a
bad swap is rolled back and the server still comes up (same stance as
extensions/bfweather).

Which DLL is set by `dll_name`: `bflib.dll` (the campaign engine, the default)
or `bfrange.dll` (the training range). Each instance only ever swaps its own
engine -- a `bflib.dll.pending` sitting in a range instance's staging dir is
left alone (and logged), and vice versa.

After a swap the new engine goes on probation with the FowlEngine plugin's
auto-updater (plugins/fowlengine/autoupdate.py): if DCS crashes on it, or the
mission never loads it, the backup taken here is staged back and DCS is
restarted onto it. That covers a hand-uploaded DLL as much as an automatic one.

A staged `bfdb.exe` is handled by plugins/fowlengine/procman.py on its own
bfdb restarts; if one is pending when DCS restarts, this extension nudges the
FowlEngine plugin to cycle bfdb so the new build lands promptly.

nodes.yaml:
  MyNode:
    instances:
      MyInstance:                 # a campaign server
        extensions:
          BFBinaries:
            bflib_dll_path: 'C:\\Users\\ATPAdmin\\Saved Games\\DCS.vectorstrike_1\\Scripts\\bflib.dll'
            staging_dir:    'C:\\Users\\ATPAdmin\\Saved Games\\DCS.vectorstrike_1\\_staging'
            keep_backups: 5
      MyRange:                    # the training range
        extensions:
          BFBinaries:
            dll_name: bfrange.dll
            dll_path:    'C:\\Users\\ATPAdmin\\Saved Games\\DCS.range\\Scripts\\bfrange.dll'
            staging_dir: 'C:\\Users\\ATPAdmin\\Saved Games\\DCS.range\\_staging'

Keys:
  dll_name        engine DLL this instance loads. Unset -> bfrange.dll if the
                  FowlEngine plugin lists this server as `kind: range` in
                  bfdb.instances, else bflib.dll. Set it explicitly anyway.
  dll_path        live DLL path; `bflib_dll_path` is the older spelling and is
                  still read. Unset -> <instance home>\\Scripts\\<dll_name>
  staging_dir     where upload.py stages this instance's DLL. Give EVERY
                  instance its own. Unset -> the FowlEngine plugin's global
                  bfdb.staging_dir (the old shared behaviour)
  keep_backups    timestamped backups kept next to the live DLL (default 5)
"""
from __future__ import annotations

import json
import os
import shutil
from datetime import datetime, timezone

from core import Extension, Server
from typing_extensions import override

__all__ = ["BFBinaries"]

DEFAULT_DLL_NAME = "bflib.dll"
# Every engine DLL name upload.py knows how to stage. Used to spot a pending
# file for the wrong engine in this instance's staging dir.
ENGINE_DLL_NAMES = ("bflib.dll", "bfrange.dll")


def _now_tag() -> str:
    return datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S")


class BFBinaries(Extension):
    def __init__(self, server: Server, config: dict):
        super().__init__(server, config)
        self._last_swap: dict | None = None

    # ---- config ----------------------------------------------------------

    @property
    def _dll_name(self) -> str:
        """Explicit `dll_name` > the engine the FowlEngine plugin says this
        instance runs (`kind:` in bfdb.instances) > bflib.dll. upload.py
        resolves it the same way, so both ends agree on the file name."""
        name = str(self.config.get("dll_name") or "").strip()
        if not name:
            cog = self._fowlengine_cog()
            kind_of = getattr(cog, "_instance_kind", None)
            try:
                if kind_of and kind_of(self.server) == "range":
                    name = "bfrange.dll"
            except Exception:  # noqa: BLE001
                pass
        return os.path.basename(name or DEFAULT_DLL_NAME) or DEFAULT_DLL_NAME

    @property
    def _dll_path(self) -> str:
        raw = self.config.get("dll_path") or self.config.get("bflib_dll_path")
        if raw:
            return os.path.expandvars(raw)
        home = getattr(getattr(self.server, "instance", None), "home", None)
        if home:
            return os.path.join(home, "Scripts", self._dll_name)
        return ""

    def _fowlengine_cog(self):
        try:
            from services.bot import BotService
            from core import ServiceRegistry

            return ServiceRegistry.get(BotService).bot.cogs.get("FowlEngine")
        except Exception:  # noqa: BLE001 - bot not up yet / plugin not loaded
            return None

    @property
    def _staging_dir(self) -> str:
        raw = self.config.get("staging_dir")
        if raw:
            return os.path.expandvars(raw)
        # Same fallback upload.py uses when it stages for an instance with no
        # staging_dir of its own: the plugin's global bfdb.staging_dir.
        procman = getattr(self._fowlengine_cog(), "procman", None)
        try:
            return procman.staging_dir if procman else ""
        except Exception:  # noqa: BLE001
            return ""

    @property
    def _keep(self) -> int:
        return int(self.config.get("keep_backups", 5))

    def _pending(self, name: str) -> str:
        return os.path.join(self._staging_dir, f"{name}.pending")

    @override
    def is_available(self) -> bool:
        if not self._dll_path:
            self.log.error(f"  => {self.name}: 'dll_path' (or 'bflib_dll_path') is not set in "
                           f"nodes.yaml and the instance home is unknown.")
            return False
        if not self._staging_dir:
            self.log.error(f"  => {self.name}: 'staging_dir' is not set in nodes.yaml.")
            return False
        # A drag-and-drop upload writes straight into this folder, and on an
        # agent node (a DCS server on another PC) nothing else creates it.
        try:
            os.makedirs(self._staging_dir, exist_ok=True)
        except OSError as ex:
            self.log.warning(f"  => {self.name}: cannot create staging_dir {self._staging_dir}: {ex}")
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

    def _warn_foreign_pending(self) -> None:
        """A pending DLL for the OTHER engine in this instance's staging dir is
        never swapped in here -- say so, instead of leaving it to rot silently
        (it usually means two instances share one staging_dir)."""
        for other in ENGINE_DLL_NAMES:
            if other == self._dll_name:
                continue
            if os.path.exists(self._pending(other)):
                self.log.warning(
                    f"{self.name}: {self._pending(other)} is staged here, but this instance "
                    f"runs {self._dll_name} -- ignored. Give every instance its own "
                    f"BFBinaries staging_dir in nodes.yaml.")

    def _swap_engine(self) -> str | None:
        name = self._dll_name
        pending = self._pending(name)
        if not os.path.exists(pending):
            return None
        # Who staged it (an admin's upload, the auto-updater, a rollback) and
        # which release -- read before the swap deletes the sidecar, so the
        # FowlEngine plugin can put the new engine on probation.
        sidecar: dict = {}
        try:
            with open(pending + ".json", "r", encoding="utf-8") as fh:
                sidecar = json.load(fh) or {}
        except (OSError, ValueError):
            pass
        self._last_swap = None
        live = self._dll_path
        backup = f"{live}.backup-{_now_tag()}"
        try:
            if os.path.exists(live):
                shutil.copy2(live, backup)
            os.makedirs(os.path.dirname(live), exist_ok=True)
            os.replace(pending, live)
        except OSError as ex:
            self.log.error(f"{self.name}: staged {name} swap failed ({ex}) -- restoring")
            if os.path.exists(backup) and not os.path.exists(live):
                try:
                    shutil.copy2(backup, live)
                except OSError:
                    pass
            return f"⚠️ staged `{name}` swap FAILED ({ex}); kept the previous engine."
        for side in (pending + ".json",):
            if os.path.exists(side):
                try:
                    os.remove(side)
                except OSError:
                    pass
        self._prune_backups()
        if sidecar.get("rollback"):
            note = f"engine ROLLED BACK: restored `{name}` from a backup (backup of the bad one `{os.path.basename(backup)}`)."
        else:
            note = f"engine updated: swapped in staged `{name}` (backup `{os.path.basename(backup)}`)."
            if sidecar.get("tag"):
                note = note[:-1] + f" -- release {sidecar['tag']}."
        self.log.warning(f"{self.name}: {note}")
        self._last_swap = {"dll": name, "live": live,
                           "backup": backup if os.path.exists(backup) else None,
                           "sidecar": sidecar}
        return note

    def _begin_probation(self) -> None:
        """Hand the swap to the FowlEngine auto-updater, which watches the new
        engine load and rolls it back if DCS crashes on it. Only possible on
        the master node, where the plugin lives; an agent node just swaps."""
        swap = getattr(self, "_last_swap", None)
        if not swap:
            return
        updater = getattr(self._fowlengine_cog(), "updater", None)
        if updater is None:
            return
        try:
            updater.begin_dll_probation(self.server, swap["dll"], swap["live"], swap["backup"],
                                        swap["sidecar"])
        except Exception as ex:  # noqa: BLE001
            self.log.warning(f"{self.name}: could not start the engine probation: {ex}")

    # kept for anything that still calls the old name
    _swap_bflib = _swap_engine

    def _bfdb_pending(self) -> bool:
        """bfdb.exe is staged in the plugin's global staging dir (one bfdb per
        box), which is not necessarily this instance's."""
        procman = getattr(self._fowlengine_cog(), "procman", None)
        if procman is not None:
            try:
                return bool(procman.pending_info("bfdb.exe"))
            except Exception:  # noqa: BLE001
                pass
        return os.path.exists(self._pending("bfdb.exe"))

    def _nudge_bfdb_if_pending(self) -> None:
        if not self._bfdb_pending():
            return
        try:
            cog = self._fowlengine_cog()
            procman = getattr(cog, "procman", None)
            pw = getattr(cog, "_bfdb_admin_password", None)
            if procman and procman.enabled and pw:
                self.loop.create_task(procman.restart(pw))
                self.log.info(f"{self.name}: staged bfdb.exe pending -- asked FowlEngine to cycle bfdb")
        except Exception as ex:  # noqa: BLE001
            self.log.warning(f"{self.name}: could not nudge bfdb for a staged update: {ex}")

    async def _announce(self, note: str) -> None:
        try:
            cog = self._fowlengine_cog()
            if cog and hasattr(cog, "notify_ops"):
                await cog.notify_ops(f"🧩 {self.server.name}: {note}")
        except Exception as ex:  # noqa: BLE001
            self.log.debug(f"{self.name}: ops announce skipped: {ex}")

    @override
    async def prepare(self) -> bool:
        if not self.is_available():
            return True  # never block a restart
        self._warn_foreign_pending()
        note = self._swap_engine()
        if note:
            self._begin_probation()
        self._nudge_bfdb_if_pending()
        if note:
            await self._announce(note)
        return True

    @override
    async def render(self, param: dict | None = None) -> dict:
        pending = [self._dll_name] if os.path.exists(self._pending(self._dll_name)) else []
        if self._bfdb_pending():
            pending.append("bfdb.exe")
        return {
            "name": self.name,
            "version": self.version,
            "value": ("staged: " + ", ".join(pending)) if pending else f"{self._dll_name} up to date",
        }
