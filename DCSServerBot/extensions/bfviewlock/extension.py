"""
BFViewLock -- enforces the camera/view restrictions DCS only honours from the
SERVER's own options, so players cannot scout with a free camera.

Why this exists as an extension rather than as part of the mission build:
DCS splits view restrictions across two places.

  * The F10 map view IS mission-enforceable. `bftools miz` forces DCS's
    extended per-role view options into every generated mission, so
    spectators and observers get `optview_onlymap` (terrain and markup, no
    units) while the people in a cockpit keep the mission's own setting.
    Nothing here needs to repeat that.

  * F5 "nearest aircraft", F11 free camera and spectator external views are
    NOT. In DCS's own options database `f5_nearest_ac` and `f11_free_camera`
    are `misc` entries and `spectatorExternalViews` is a difficulty entry
    without `setEnforceable()` -- none of them can be put in a mission's
    `forcedOptions`. They are read from the server's `Config/options.lua`,
    which is what this writes.

It runs in `prepare()`, which DCSServerBot calls right before each DCS start
-- DCS is down, so the file it would otherwise rewrite on exit is ours to
change. Every restart re-applies it, so a manual edit (or DCS writing its own
state back out) cannot quietly undo the lock.

Nothing here can block a server start: a failure is logged and the server
comes up anyway, the same stance as extensions/bfbinaries and bfweather.

nodes.yaml:
  MyNode:
    instances:
      MyInstance:
        extensions:
          BFViewLock:
            # every key is optional; these are the defaults
            f5_nearest_ac: false          # F5 jump to the nearest aircraft
            f11_free_camera: false        # F11 free camera
            spectator_external_views: false
            # not touched unless you set them:
            # external_views: false       # ALL external views for players too
            # f10_awacs: true
            # miscellaneous: {...}        # any other options.lua misc key
            # difficulty: {...}           # any other options.lua difficulty key
"""
from __future__ import annotations

from core import Extension, Server
from typing_extensions import override

__all__ = ["BFViewLock"]

# key in this extension's config -> (options.lua section, options.lua key)
_KEYS = {
    "f5_nearest_ac": ("miscellaneous", "f5_nearest_ac"),
    "f11_free_camera": ("miscellaneous", "f11_free_camera"),
    "f10_awacs": ("miscellaneous", "f10_awacs"),
    "spectator_external_views": ("difficulty", "spectatorExternalViews"),
    "external_views": ("difficulty", "externalViews"),
}

# Applied when the config does not mention them. These are the three that let
# a player see what they have not earned: F5 walks the nearest aircraft, F11
# flies the camera anywhere, and spectator external views do both with no
# aircraft of your own at risk.
_DEFAULTS = {
    "f5_nearest_ac": False,
    "f11_free_camera": False,
    "spectator_external_views": False,
}


class BFViewLock(Extension):
    def __init__(self, server: Server, config: dict):
        super().__init__(server, config)
        self.last_applied: dict[str, bool] = {}

    def _wanted(self) -> dict[str, bool]:
        """The value every managed key should have, config over defaults."""
        wanted = dict(_DEFAULTS)
        for name in _KEYS:
            if name in self.config:
                value = self.config[name]
                if value is None:
                    # explicit null means "leave whatever the server has"
                    wanted.pop(name, None)
                else:
                    wanted[name] = bool(value)
        return wanted

    def _apply(self) -> dict[str, bool]:
        """Write the managed keys into the server's options.lua.

        Returns what was actually changed. Sections are written back whole:
        `Server.options` only persists to disk on assignment to a top level
        key, so mutating the nested dict in place would be a silent no-op.
        """
        changed: dict[str, bool] = {}
        by_section: dict[str, dict[str, bool]] = {}
        for name, value in self._wanted().items():
            section, key = _KEYS[name]
            by_section.setdefault(section, {})[key] = value
        # Free-form passthrough for anything else in options.lua.
        for section in ("miscellaneous", "difficulty"):
            extra = self.config.get(section) or {}
            if isinstance(extra, dict):
                by_section.setdefault(section, {}).update(extra)
        for section, values in by_section.items():
            current = dict(self.server.options.get(section) or {})
            updates = {k: v for k, v in values.items() if current.get(k) != v}
            if not updates:
                continue
            current.update(updates)
            self.server.options[section] = current
            changed.update({f"{section}.{k}": v for k, v in updates.items()})
        return changed

    @override
    async def prepare(self) -> bool:
        try:
            changed = self._apply()
            self.last_applied = self._wanted()
            if changed:
                detail = ", ".join(f"{k}={str(v).lower()}" for k, v in sorted(changed.items()))
                self.log.info(f"  => {self.name}: server view options set ({detail})")
            else:
                self.log.debug(f"{self.name}: server view options already as configured")
        except Exception as ex:  # noqa: BLE001
            # A view that stays open is a gameplay problem; a server that will
            # not start is an outage. Never trade one for the other.
            self.log.error(f"  => {self.name}: could not apply server view options: {ex}")
        return True

    @override
    async def render(self, param: dict | None = None) -> dict:
        wanted = self.last_applied or self._wanted()
        labels = {
            "f5_nearest_ac": "F5",
            "f11_free_camera": "F11",
            "spectator_external_views": "spectator ext",
            "external_views": "external",
            "f10_awacs": "F10 AWACS",
        }
        if wanted:
            value = " · ".join(
                f"{labels.get(k, k)} {'on' if v else 'off'}" for k, v in sorted(wanted.items())
            )
        else:
            value = "not enforcing anything"
        return {"name": self.name, "version": self.version, "value": value}
