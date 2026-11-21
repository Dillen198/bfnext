"""
The bot's own config files, for the dashboard's SERVER BOT > Settings and
Presets tabs: main.yaml, nodes.yaml, servers.yaml, the presets files, and
every plugin's and service's YAML.

Each save is checked the way the bot checks it -- against the same schema
files (core.utils.configwrite on DCSServerBot 3.0.5+, the bot's own validator),
written atomically with a .bak -- and refused if the file changed since the
page read it. The dashboard rules for fowlengine.yaml hold here too:

  * secrets go out masked and come back as they were (opsapi.mask_secrets /
    unmask_secrets), and never to a changed endpoint;
  * a key that names a program, a path, a folder, an install or a network
    endpoint can't be added, changed or removed from here (protected_changes):
    a dashboard login must not be a way to run a command as the bot. Those are
    edited on the box.

fowlengine.yaml itself is left to the OPS page's editor.
"""
from __future__ import annotations

import copy
import glob
import hashlib
import io
import os
import shutil
import time
from typing import Any, Optional

from .opsapi import SECRET_MASK, is_protected_key, mask_secrets, protected_changes, unmask_secrets

__all__ = ["BotConfig", "is_bot_protected_key", "revision_of", "BACKUPS_KEEP"]

BACKUPS_KEEP = 20

# On top of opsapi's protected keys (exe, cmd, home, *_path, *_dir, *_url, ...):
# what in the bot's own files names a program, an install or a host.
_BOT_PROTECTED = {
    "installation", "executable", "python", "autoexec", "script", "scripts", "host", "proxy",
    "proxy_auth", "dsn", "database", "pool",
}

# Files the editor offers, relative to the bot's config dir. Order is the
# listing's order.
_PATTERNS = (
    "main.yaml", "nodes.yaml", "servers.yaml", "presets.yaml", "presets*.yaml", "presets/*.yaml",
    "plugins/*.yaml", "services/*.yaml", "services/*.yaml.disabled",
)
_EXCLUDED = {"plugins/fowlengine.yaml"}      # the OPS page edits it, with its own rules


def is_bot_protected_key(key: Any) -> bool:
    return is_protected_key(key) or str(key).lower() in _BOT_PROTECTED


def revision_of(data: Optional[bytes]) -> str:
    return hashlib.sha256(data or b"").hexdigest()


def _plain(node: Any) -> Any:
    """A ruamel round-trip document as plain JSON data."""
    if isinstance(node, dict):
        return {str(k): _plain(v) for k, v in node.items()}
    if isinstance(node, (list, tuple)):
        return [_plain(v) for v in node]
    if node is None or isinstance(node, (bool, int, float, str)):
        return node
    return str(node)


def _yaml():
    from ruamel.yaml import YAML
    y = YAML()
    y.preserve_quotes = True
    y.width = 4096
    return y


def _dump(doc: Any) -> str:
    buf = io.StringIO()
    _yaml().dump(doc, buf)
    return buf.getvalue()


class ConfigError(Exception):
    """A refusal with the HTTP status it answers with."""

    def __init__(self, status: int, message: str):
        super().__init__(message)
        self.status = status
        self.message = message


class BotConfig:
    def __init__(self, config_dir: str, bot_dir: Optional[str] = None):
        self.config_dir = os.path.abspath(config_dir)
        self.bot_dir = os.path.abspath(bot_dir or os.getcwd())

    # ---- which files, and how each is checked and applied ----------------------

    def files(self) -> list[dict]:
        seen: list[str] = []
        for pat in _PATTERNS:
            for full in sorted(glob.glob(os.path.join(self.config_dir, pat))):
                rel = os.path.relpath(full, self.config_dir).replace(os.sep, "/")
                if rel in seen or rel in _EXCLUDED or not os.path.isfile(full):
                    continue
                seen.append(rel)
        out = []
        for rel in seen:
            full = os.path.join(self.config_dir, rel)
            st = os.stat(full)
            out.append({"path": rel, "kind": self.kind(rel), "size": st.st_size, "mtime": st.st_mtime,
                        "disabled": rel.endswith(".disabled"), "schema": bool(self.schemas(rel)),
                        "applies": self.applies(rel)})
        return out

    def resolve(self, rel: str) -> str:
        """`rel` as a full path -- only ever one of the files `files()` lists,
        so nothing outside them can be read or written."""
        rel = str(rel or "").replace("\\", "/").strip("/")
        if rel not in {f["path"] for f in self.files()}:
            raise ConfigError(404, f"no such config file {rel!r}")
        return os.path.join(self.config_dir, rel)

    @staticmethod
    def kind(rel: str) -> str:
        name = rel.split("/")[-1]
        if rel.startswith("plugins/"):
            return "plugin"
        if rel.startswith("services/"):
            return "service"
        if name.startswith("presets") or rel.startswith("presets/"):
            return "presets"
        return name.split(".")[0]                 # main / nodes / servers

    @staticmethod
    def applies(rel: str) -> str:
        """When a saved change takes effect: "reload" (the plugin is reloaded
        right after the save), "launch" (a preset: at the next mission launch)
        or "restart" (the bot reads it at start-up)."""
        kind = BotConfig.kind(rel)
        return {"plugin": "reload", "presets": "launch"}.get(kind, "restart")

    def schemas(self, rel: str) -> list[str]:
        """The schema files the bot validates this file against."""
        kind = self.kind(rel)
        stem = rel.split("/")[-1].split(".")[0]
        if kind == "plugin":
            primary = os.path.join(self.bot_dir, "plugins", stem, "schemas", f"{stem}_schema.yaml")
        elif kind == "service":
            primary = os.path.join(self.bot_dir, "services", stem, "schemas", f"{stem}_schema.yaml")
        elif kind == "presets":
            primary = os.path.join(self.bot_dir, "schemas", "presets_schema.yaml")
        else:
            primary = os.path.join(self.bot_dir, "schemas", f"{kind}_schema.yaml")
        if not os.path.isfile(primary):
            return []
        try:  # the console's own resolver (3.0.5+): siblings, commands_schema, extensions
            from services.webservice.configgen import schema_files_for
            return list(schema_files_for(primary))
        except Exception:  # noqa: BLE001
            return [primary]

    # ---- read ---------------------------------------------------------------------

    def read(self, rel: str) -> dict:
        full = self.resolve(rel)
        with open(full, "rb") as fh:
            data = fh.read()
        try:
            doc = _yaml().load(data.decode("utf-8")) if data.strip() else {}
        except Exception as ex:  # noqa: BLE001
            # a broken file still opens, as text, so it can be fixed
            return {"path": rel, "kind": self.kind(rel), "revision": revision_of(data),
                    "yaml": data.decode("utf-8", "replace"), "doc": None, "masked": 0,
                    "parse_error": str(ex), "schema": bool(self.schemas(rel)), "applies": self.applies(rel)}
        doc = doc if doc is not None else {}
        masked = mask_secrets(doc)
        return {"path": rel, "kind": self.kind(rel), "revision": revision_of(data),
                "yaml": _dump(doc) if masked else data.decode("utf-8"), "doc": _plain(doc),
                "masked": masked, "secret_mask": SECRET_MASK, "schema": bool(self.schemas(rel)),
                "applies": self.applies(rel)}

    # ---- check and write -------------------------------------------------------------

    def _candidate(self, rel: str, body: dict) -> tuple[str, Any, bytes, Any]:
        """(full path, the document to write, current bytes, current document)."""
        full = self.resolve(rel)
        with open(full, "rb") as fh:
            current = fh.read()
        want = body.get("revision")
        if not want or want != revision_of(current):
            raise ConfigError(409, f"{rel} changed on disk since the page read it -- reload it and "
                                   f"make the change again")
        try:
            old = _yaml().load(current.decode("utf-8")) if current.strip() else None
        except Exception:  # noqa: BLE001
            old = None
        old = old if old is not None else {}
        if "yaml" in body:
            text = body.get("yaml")
            if not isinstance(text, str):
                raise ConfigError(400, "yaml must be text")
            try:
                new = _yaml().load(text) if text.strip() else {}
            except Exception as ex:  # noqa: BLE001
                raise ConfigError(400, f"that is not valid YAML: {ex}")
            new = new if new is not None else {}
        else:
            # top-level keys replaced or removed, the rest of the file (and its
            # comments) left as it is: the Presets tab saves one preset this way
            sets = body.get("set") or {}
            drops = body.get("delete") or []
            if not isinstance(sets, dict) or not isinstance(drops, list):
                raise ConfigError(400, "set must be an object and delete a list")
            if not isinstance(old, dict):
                raise ConfigError(400, f"{rel} is not a mapping at the top; edit it as YAML")
            new = copy.deepcopy(old)
            for k, v in sets.items():
                new[str(k)] = v
            for k in drops:
                new.pop(str(k), None)
        try:
            unmask_secrets(new, old)
        except ValueError as ex:
            raise ConfigError(400, str(ex))
        prot = protected_changes(_plain(new), _plain(old), is_protected=is_bot_protected_key)
        if prot:
            raise ConfigError(403, "these name a program, a path, an install or a network endpoint, and are "
                                   "edited on the box, not from the dashboard: " + ", ".join(prot[:12])
                              + (" ..." if len(prot) > 12 else ""))
        return full, new, current, old

    def validate(self, rel: str, body: dict) -> list[str]:
        """What the bot's schema says about a candidate; [] = fine. Writes nothing."""
        full, new, _cur, _old = self._candidate(rel, body)
        return self._schema_errors(full, rel, new)

    def _schema_errors(self, full: str, rel: str, doc: Any) -> list[str]:
        schemas = self.schemas(rel)
        if not schemas:
            return []
        try:
            from core.utils.configwrite import ConfigWriteError, validate_against_schema
        except Exception:  # noqa: BLE001 -- an older bot: it checks the file when it next loads it
            return []
        try:
            validate_against_schema(_dump(doc), schemas, path=full)
        except ConfigWriteError as ex:
            where = getattr(ex, "error_path", None)
            return [f"{where}: {ex}" if where else str(ex)]
        except Exception as ex:  # noqa: BLE001
            return [str(ex)]
        return []

    def save(self, rel: str, body: dict) -> dict:
        full, new, current, _old = self._candidate(rel, body)
        errors = self._schema_errors(full, rel, new)
        if errors:
            raise ConfigError(400, "the bot's schema refuses it: " + "; ".join(errors))
        text = _dump(new)
        if text.encode("utf-8") == current:
            return {"ok": True, "changed": False, "revision": revision_of(current), "applies": self.applies(rel)}
        backup = self._backup(full, current)
        tmp = f"{full}.tmp-{os.getpid()}"
        with open(tmp, "w", encoding="utf-8", newline="") as fh:
            fh.write(text)
        os.replace(tmp, full)
        with open(full, "rb") as fh:
            rev = revision_of(fh.read())
        return {"ok": True, "changed": True, "revision": rev, "backup": backup, "applies": self.applies(rel)}

    def _backup(self, full: str, data: bytes) -> str:
        d = os.path.join(os.path.dirname(full), ".fowlengine-backups")
        os.makedirs(d, exist_ok=True)
        name = f"{os.path.basename(full)}.{time.strftime('%Y%m%d-%H%M%S')}"
        path = os.path.join(d, name)
        with open(path, "wb") as fh:
            fh.write(data)
        mine = sorted(f for f in os.listdir(d) if f.startswith(os.path.basename(full) + "."))
        for old in mine[:-BACKUPS_KEEP]:
            try:
                os.remove(os.path.join(d, old))
            except OSError:
                pass
        return os.path.relpath(path, self.config_dir).replace(os.sep, "/")

    def set_service_enabled(self, name: str, enabled: bool) -> dict:
        """Switch a service on or off the way the bot reads it: its file is
        services/<name>.yaml when on, .yaml.disabled when off."""
        if not name.replace("_", "").replace("-", "").isalnum():
            raise ConfigError(400, f"bad service name {name!r}")
        on = os.path.join(self.config_dir, "services", f"{name}.yaml")
        off = on + ".disabled"
        src, dst = (off, on) if enabled else (on, off)
        if not os.path.isfile(src):
            if os.path.isfile(dst):
                return {"ok": True, "changed": False}
            raise ConfigError(404, f"no config file for the {name} service")
        if os.path.exists(dst):
            raise ConfigError(409, f"both {os.path.basename(on)} and {os.path.basename(off)} exist -- "
                                   f"remove one on the box")
        shutil.move(src, dst)
        return {"ok": True, "changed": True, "applies": "restart"}
