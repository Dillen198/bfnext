"""
Process manager for the Fowl Engine sidecar stack (bfdb.exe + the netidx
resolver), owned by the FowlEngine DCSServerBot plugin.

This replaces bfsystem.ps1: instead of an admin RDPing in and running an
interactive PowerShell launcher, the bot starts bfdb + the resolver as child
processes, renders gci.json from the plugin's own YAML, health-checks bfdb and
relaunches it on crash/hang, and swaps in a staged bfdb.exe on the next
(re)start.

Everything is driven from the `bfdb:` and `gci:` blocks in fowlengine.yaml --
see fowlengine.sample.yaml for the full key list.
"""
from __future__ import annotations

import asyncio
import json
import os
import shutil
import socket
import subprocess
import time
from datetime import datetime, timezone
from typing import Awaitable, Callable, Optional

import aiohttp

__all__ = ["Procman", "BFDB_HEALTH_CHECK_SECS"]

# How often the supervising task calls health_ok().
BFDB_HEALTH_CHECK_SECS = 30

NETIDX_RESOLVER_PORT = 4564

# gci.yaml (snake_case) -> gci.json (camelCase, matches bfdb/src/gci.rs GciConfig).
# `enabled` is consumed by procman itself and never written to the file.
_GCI_KEY_MAP = {
    "srs_host": "srsHost",
    "srs_port": "srsPort",
    "opus_dll_path": "opusDllPath",
    "blue_freq_mhz": "blueFreqMhz",
    "red_freq_mhz": "redFreqMhz",
    # Multi-band form: a list of {mhz, modulation} the controller transmits on
    # simultaneously (UHF + VHF + FM). Wins over blue_freq_mhz/red_freq_mhz.
    "blue_freqs": "blueFreqs",
    "red_freqs": "redFreqs",
    "modulation": "modulation",
    "blue_controller_callsign": "blueControllerCallsign",
    "red_controller_callsign": "redControllerCallsign",
    "blue_eam_password": "blueEamPassword",
    "red_eam_password": "redEamPassword",
    "blue_bullseye_name": "blueBullseyeName",
    "red_bullseye_name": "redBullseyeName",
    "piper_exe": "piperExe",
    "piper_model": "piperModel",
    "blue_piper_model": "bluePiperModel",
    "red_piper_model": "redPiperModel",
    "tts_voice": "ttsVoice",
    "blue_tts_voice": "blueTtsVoice",
    "red_tts_voice": "redTtsVoice",
    "whisper_exe": "whisperExe",
    "whisper_model": "whisperModel",
    "whisper_server_url": "whisperServerUrl",
    "discord_webhook_url": "discordWebhookUrl",
    "blue_discord_webhook_url": "blueDiscordWebhookUrl",
    "red_discord_webhook_url": "redDiscordWebhookUrl",
    "discord_tag_instance": "discordTagInstance",
    "units": "units",
    "reference": "reference",
    "sam_threat_calls": "samThreatCalls",
    "splash_calls": "splashCalls",
    "chute_calls": "chuteCalls",
    "tumbleweed_calls": "tumbleweedCalls",
    "support_calls": "supportCalls",
    "support_interval_secs": "supportIntervalSecs",
    "threat_range_nm": "threatRangeNm",
    "per_player_cooldown_secs": "perPlayerCooldownSecs",
    "picture_interval_secs": "pictureIntervalSecs",
    "poll_secs": "pollSecs",
    "inter_call_gap_secs": "interCallGapSecs",
}
# The `atc:` sub-block (spoken tower + ATIS). Rendered as a nested object under
# gci.json's "atc" key -- see bfdb/src/atc.rs AtcConfig.
_ATC_KEY_MAP = {
    "enabled": "enabled",
    "tower_base_mhz": "towerBaseMhz",
    "tower_step_mhz": "towerStepMhz",
    "tower_modulation": "towerModulation",
    "atis_base_mhz": "atisBaseMhz",
    "atis_step_mhz": "atisStepMhz",
    "atis_modulation": "atisModulation",
    "atis_interval_secs": "atisIntervalSecs",
    "poll_secs": "pollSecs",
    "pattern_altitude_ft": "patternAltitudeFt",
    # Per-field frequency overrides and ATIS exclusions. `fields` is keyed by
    # the campaign's field name and its inner keys (tower/atis/mhz/modulation/
    # label) are already single words, so it passes through untouched.
    "fields": "fields",
    "atis_exclude": "atisExclude",
}

# Which rendered keys are secrets (masked by gci_effective(mask=True)).
_GCI_SECRET_KEYS = {
    "blueEamPassword",
    "redEamPassword",
    "discordWebhookUrl",
    "blueDiscordWebhookUrl",
    "redDiscordWebhookUrl",
}

# gci.json keys whose empty-string value should still be omitted so bfdb's serde
# defaults apply (everything except explicit booleans / numbers).
_GCI_OMIT_IF_EMPTY = True


def _map_gci(src: dict, mask: bool = False) -> dict:
    """snake_case YAML -> camelCase gci.json.

    Flat keys come from _GCI_KEY_MAP; `atc:` is mapped through _ATC_KEY_MAP into
    a nested object. Empty strings are dropped so bfdb's serde defaults apply
    rather than a blank overriding them.
    """
    out: dict = {}
    for snake, camel in _GCI_KEY_MAP.items():
        if snake not in src:
            continue
        val = src[snake]
        if _GCI_OMIT_IF_EMPTY and isinstance(val, str) and val == "":
            continue
        if mask and camel in _GCI_SECRET_KEYS and val:
            val = "***"
        out[camel] = val
    atc = src.get("atc")
    if isinstance(atc, dict):
        mapped = {}
        for snake, camel in _ATC_KEY_MAP.items():
            if snake not in atc:
                continue
            val = atc[snake]
            if _GCI_OMIT_IF_EMPTY and isinstance(val, str) and val == "":
                continue
            mapped[camel] = val
        if mapped:
            out["atc"] = mapped
    return out


def _expand(val) -> Optional[str]:
    """Expand %ENV% in a YAML path value; empty/missing -> None."""
    if val in (None, ""):
        return None
    return os.path.expandvars(val)


def _now_tag() -> str:
    return datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S")


def sha256_of(path: str) -> Optional[str]:
    import hashlib

    try:
        h = hashlib.sha256()
        with open(path, "rb") as fh:
            for chunk in iter(lambda: fh.read(1 << 20), b""):
                h.update(chunk)
        return h.hexdigest()
    except OSError:
        return None


def _port_listening(host: str, port: int, timeout: float = 1.0) -> bool:
    try:
        with socket.create_connection((host, port), timeout=timeout):
            return True
    except OSError:
        return False


class Procman:
    """Owns the bfdb.exe + netidx resolver child processes for one node.

    Constructed once by the FowlEngine cog. `config` is the plugin's merged
    config dict (self.get_config()); `notify` is an async callback used for
    ops-channel notices (crash relaunch, staged-binary swap).
    """

    def __init__(self, log, config: dict, notify: Callable[[str], Awaitable[None]]):
        self.log = log
        self._notify = notify
        self.cfg = (config or {}).get("bfdb", {}) or {}
        self.gci_cfg = (config or {}).get("gci", {}) or {}
        # `bfdb.instances:` -- one entry per DCS server this bfdb fronts. Empty
        # (the default) keeps the old single-server shape, where the flat
        # netidx_base/stats_jsonl/... keys above describe the only server.
        self.instances_cfg = list(self.cfg.get("instances") or [])

        self._bfdb: Optional[subprocess.Popen] = None
        self._resolver: Optional[subprocess.Popen] = None
        self._lock = asyncio.Lock()

        # health-supervision state (mirrors the old commands.py fields)
        self._fail_count = 0
        self._first_fail: Optional[float] = None
        self._last_restart = 0.0
        self._last_swap_note: Optional[str] = None

    def reload_config(self, config: dict) -> None:
        """Pick up edited YAML (e.g. after a config upload) without recreating
        the manager. Takes effect on the next start()/restart()."""
        self.cfg = (config or {}).get("bfdb", {}) or {}
        self.gci_cfg = (config or {}).get("gci", {}) or {}
        self.instances_cfg = list(self.cfg.get("instances") or [])

    # ---- config helpers ---------------------------------------------------

    @property
    def enabled(self) -> bool:
        return bool(self.cfg.get("manage"))

    @property
    def exe(self) -> str:
        return os.path.expandvars(self.cfg.get("exe", ""))

    @property
    def home(self) -> str:
        return os.path.expandvars(self.cfg.get("home", ""))

    @property
    def staging_dir(self) -> str:
        return os.path.expandvars(
            self.cfg.get("staging_dir") or os.path.join(self.home, "_staging")
        )

    @property
    def gci_json_path(self) -> str:
        return os.path.join(self.home, "gci.json")

    @property
    def multi_instance(self) -> bool:
        """True when `bfdb.instances:` lists the DCS servers explicitly."""
        return bool(self.instances_cfg)

    @property
    def instances_json_path(self) -> str:
        return os.path.join(self.home, "instances.json")

    def _instance_gci_path(self, inst_id: str) -> str:
        return os.path.join(self.home, f"gci.{inst_id}.json")

    @property
    def api_url(self) -> str:
        # health checks always go to the loopback listener, not a public origin
        addr = self.cfg.get("listen_address", "0.0.0.0:8880")
        port = addr.rsplit(":", 1)[-1]
        return f"http://127.0.0.1:{port}"

    def _resolved(self, key: str) -> Optional[str]:
        val = self.cfg.get(key)
        if val in (None, ""):
            return None
        return os.path.expandvars(val)

    # ---- gci.json rendering --------------------------------------------------

    def gci_enabled(self) -> bool:
        return bool(self.gci_cfg.get("enabled"))

    def gci_effective(self, mask: bool = False) -> dict:
        """The dict that will be written to gci.json (camelCase)."""
        return _map_gci(self.gci_cfg, mask=mask)

    def _render_gci(self) -> bool:
        """Write (or remove) home/gci.json. Returns True if the file exists after."""
        path = self.gci_json_path
        if not self.gci_enabled():
            if os.path.exists(path):
                try:
                    os.remove(path)
                    self.log.info(f"FowlEngine/procman: GCI disabled -- removed {path}")
                except OSError as ex:
                    self.log.error(f"FowlEngine/procman: could not remove {path}: {ex}")
            return False
        payload = self.gci_effective(mask=False)
        payload["_generated"] = (
            f"rendered by FowlEngine/procman from fowlengine.yaml at {_now_tag()} UTC "
            f"-- do not edit by hand, edit the plugin YAML instead"
        )
        try:
            os.makedirs(self.home, exist_ok=True)
            tmp = path + ".tmp"
            with open(tmp, "w", encoding="utf-8") as fh:
                json.dump(payload, fh, indent=2)
            os.replace(tmp, path)
            self.log.info(f"FowlEngine/procman: rendered {path} from YAML")
            return True
        except OSError as ex:
            self.log.error(f"FowlEngine/procman: failed to write {path}: {ex}")
            return os.path.exists(path)

    def _render_gci_for(self, inst: dict) -> Optional[str]:
        """Render one instance's gci.json from its own `gci:` block, merged over
        the top-level one so shared settings (piper/whisper paths, units,
        brevity toggles) are written once and only the per-server bits --
        SRS port, frequencies, callsigns, EAM passwords -- are repeated.

        Returns the file path if GCI is on for this instance, else None (and
        removes any file left from a previous run with it enabled)."""
        inst_id = inst.get("id")
        path = self._instance_gci_path(inst_id)
        merged = dict(self.gci_cfg)
        inst_gci = inst.get("gci") or {}
        merged.update(inst_gci)
        # `atc:` is a nested block -- a per-instance one that only overrides the
        # frequencies must not wipe out the shared ATIS settings, so merge it
        # key by key instead of letting update() replace the whole dict.
        if isinstance(self.gci_cfg.get("atc"), dict) and isinstance(inst_gci.get("atc"), dict):
            merged_atc = dict(self.gci_cfg["atc"])
            merged_atc.update(inst_gci["atc"])
            merged["atc"] = merged_atc
        # An instance-level `enabled:` wins; otherwise inherit the global one.
        if not merged.get("enabled"):
            if os.path.exists(path):
                try:
                    os.remove(path)
                    self.log.info(f"FowlEngine/procman: GCI off for {inst_id} -- removed {path}")
                except OSError as ex:
                    self.log.error(f"FowlEngine/procman: could not remove {path}: {ex}")
            return None
        # bfdb's GciConfig has no defaults for the frequencies: a file without
        # them is rejected outright and GCI silently stays off for that server.
        # Catch it here, where we can say which instance is misconfigured.
        for single, multi in (("blue_freq_mhz", "blue_freqs"), ("red_freq_mhz", "red_freqs")):
            if merged.get(single) is None and not merged.get(multi):
                self.log.error(
                    f"FowlEngine/procman: instance {inst_id} has gci.enabled but neither "
                    f"`{single}` nor `{multi}` (set one in the top-level `gci:` block or this "
                    f"instance's override) -- bfdb will refuse the file and run this server "
                    f"without GCI"
                )
        payload = _map_gci(merged)
        payload["_generated"] = (
            f"rendered by FowlEngine/procman from fowlengine.yaml (instance {inst_id}) "
            f"at {_now_tag()} UTC -- do not edit by hand, edit the plugin YAML instead"
        )
        try:
            os.makedirs(self.home, exist_ok=True)
            tmp = path + ".tmp"
            with open(tmp, "w", encoding="utf-8") as fh:
                json.dump(payload, fh, indent=2)
            os.replace(tmp, path)
            self.log.info(f"FowlEngine/procman: rendered {path} from YAML")
            return path
        except OSError as ex:
            self.log.error(f"FowlEngine/procman: failed to write {path}: {ex}")
            return path if os.path.exists(path) else None

    def _render_instances(self) -> Optional[str]:
        """Write home/instances.json from `bfdb.instances:` and return its path,
        or None in single-server mode (where the flat flags are used instead).

        Validation that would otherwise only surface as a bfdb startup failure
        -- duplicate ids, shared netidx bases or shared UDP export ports -- is
        left to bfdb, which refuses to start and says exactly which key clashed.
        Rendering here only maps YAML keys onto the file's schema."""
        if not self.multi_instance:
            # Clean up a stale file so a downgrade to single-server mode can't
            # silently keep using the old instance list.
            if os.path.exists(self.instances_json_path):
                try:
                    os.remove(self.instances_json_path)
                except OSError:
                    pass
            return None
        entries = []
        for inst in self.instances_cfg:
            inst_id = inst.get("id")
            if not inst_id:
                self.log.error("FowlEngine/procman: an entry in bfdb.instances has no `id` -- skipped")
                continue
            gci_path = self._render_gci_for(inst)
            entry = {
                "id": inst_id,
                "label": inst.get("label") or inst_id,
                "base": inst.get("netidx_base") or None,
                "sortie": inst.get("sortie") or None,
                "stats_jsonl": _expand(inst.get("stats_jsonl")),
                "stats_dir": _expand(inst.get("stats_dir")),
                "engine_config": _expand(inst.get("engine_config")),
                "export_port": inst.get("export_port"),
                "srs_url": inst.get("srs_url") or None,
                "gci_config": gci_path,
                # The DCSServerBot server name, so the plugin can address this
                # instance as ?server=<name> without knowing bfdb's own ids.
                "dcs_server_name": inst.get("dcs_server_name") or None,
                # public: false -> a test/staging server. Hidden from the
                # dashboard's server selector for everyone but an admin, and
                # its rounds are left out of the all-time pilot totals so
                # testing can't inflate the public leaderboard.
                "public": bool(inst.get("public", True)),
            }
            entries.append(entry)
        payload = {
            "_generated": (
                f"rendered by FowlEngine/procman from fowlengine.yaml at {_now_tag()} UTC "
                f"-- do not edit by hand, edit the plugin YAML instead"
            ),
            "default": self.cfg.get("default_instance") or (entries[0]["id"] if entries else None),
            "instances": entries,
        }
        try:
            os.makedirs(self.home, exist_ok=True)
            tmp = self.instances_json_path + ".tmp"
            with open(tmp, "w", encoding="utf-8") as fh:
                json.dump(payload, fh, indent=2)
            os.replace(tmp, self.instances_json_path)
            self.log.info(
                f"FowlEngine/procman: rendered {self.instances_json_path} "
                f"({len(entries)} instance(s))"
            )
            return self.instances_json_path
        except OSError as ex:
            self.log.error(f"FowlEngine/procman: failed to write {self.instances_json_path}: {ex}")
            return self.instances_json_path if os.path.exists(self.instances_json_path) else None

    # ---- staged binary swap ------------------------------------------------

    def _pending_path(self, name: str) -> str:
        return os.path.join(self.staging_dir, f"{name}.pending")

    def pending_info(self, name: str) -> Optional[dict]:
        p = self._pending_path(name)
        if not os.path.exists(p):
            return None
        info = {"path": p, "size": os.path.getsize(p), "sha256": sha256_of(p)}
        side = p + ".json"
        if os.path.exists(side):
            try:
                with open(side, "r", encoding="utf-8") as fh:
                    info.update(json.load(fh))
            except (OSError, ValueError):
                pass
        return info

    def cancel_pending(self, name: str) -> bool:
        removed = False
        for p in (self._pending_path(name), self._pending_path(name) + ".json"):
            if os.path.exists(p):
                try:
                    os.remove(p)
                    removed = True
                except OSError as ex:
                    self.log.error(f"FowlEngine/procman: could not remove {p}: {ex}")
        return removed

    def _prune_backups(self, live_path: str, keep: int = 5) -> None:
        d = os.path.dirname(live_path)
        base = os.path.basename(live_path)
        try:
            backups = sorted(
                (f for f in os.listdir(d) if f.startswith(f"{base}.backup-")),
                reverse=True,
            )
        except OSError:
            return
        for stale in backups[keep:]:
            try:
                os.remove(os.path.join(d, stale))
            except OSError:
                pass

    def apply_staged(self, name: str, live_path: str, keep: int = 5) -> Optional[str]:
        """Swap staging/<name>.pending over live_path after a timestamped backup.
        Returns a human-readable note if a swap happened, else None. Never raises
        -- on failure the live file is left untouched (or restored)."""
        pending = self._pending_path(name)
        if not os.path.exists(pending):
            return None
        live_path = os.path.expandvars(live_path)
        backup = f"{live_path}.backup-{_now_tag()}"
        try:
            if os.path.exists(live_path):
                shutil.copy2(live_path, backup)
            os.makedirs(os.path.dirname(live_path), exist_ok=True)
            os.replace(pending, live_path)
        except OSError as ex:
            self.log.error(f"FowlEngine/procman: staged swap of {name} failed: {ex}")
            if os.path.exists(backup) and not os.path.exists(live_path):
                try:
                    shutil.copy2(backup, live_path)
                except OSError:
                    pass
            return f"⚠️ staged {name} swap FAILED ({ex}) -- kept the previous binary"
        self.cancel_pending(name)  # clears the sidecar
        self._prune_backups(live_path, keep)
        note = f"swapped in staged `{name}` (backup: `{os.path.basename(backup)}`)"
        self.log.warning(f"FowlEngine/procman: {note}")
        self._last_swap_note = note
        return note

    # ---- bfdb arg list ---------------------------------------------------

    def _build_args(self, admin_password: str, gci_file_exists: bool,
                    instances_file: Optional[str] = None) -> list[str]:
        c = self.cfg
        args: list[str] = [
            "--db", os.path.join(self.home, "bfdb"),
            "--listen-address", c.get("listen_address", "0.0.0.0:8880"),
            "--site-address", c.get("site_address", "0.0.0.0:8766"),
            "--admin-username", c.get("admin_username", "admin"),
            "--admin-password", admin_password,
        ]
        # Flags that describe the whole bfdb process, whatever it fronts.
        pairs = {
            "--config": self._resolved("config"),
            "--intel-dir": self._resolved("intel_dir"),
            "--log-file": self._resolved("log_file"),
        }
        if instances_file:
            # Multi-instance: every per-server path/port/base lives in the file,
            # and bfdb refuses to start if the single-server flags are also
            # passed -- so they are deliberately not added here.
            pairs["--instances"] = instances_file
        else:
            pairs.update({
                "--stats-jsonl": self._resolved("stats_jsonl"),
                "--stats-dir": self._resolved("stats_dir"),
                "--engine-config": self._resolved("engine_config"),
                "--srs-url": (c.get("srs_url") or None),
                "--base": (c.get("netidx_base") or None),
                "--sortie": (c.get("sortie") or None),
            })
            if c.get("export_port"):
                pairs["--export-port"] = str(c["export_port"])
        for flag, val in pairs.items():
            if val:
                args += [flag, val]
        if gci_file_exists and not instances_file:
            args += ["--gci-config", self.gci_json_path]
        for origin in c.get("cors_origins", []) or []:
            args += ["--cors-origin", origin]
        dbot_url = c.get("dcsserverbot_url")
        dbot_key = c.get("dcsserverbot_api_key")
        if dbot_url and dbot_key:
            args += ["--dcsserverbot-url", dbot_url, "--dcsserverbot-api-key", dbot_key]
        cid = c.get("discord_client_id")
        csecret = c.get("discord_client_secret")
        if cid and csecret:
            args += [
                "--discord-client-id", cid,
                "--discord-client-secret", csecret,
                "--discord-redirect-uri", c.get("discord_redirect_uri", ""),
                "--discord-guild-id", str(c.get("discord_guild_id", "")),
                "--discord-admin-role-id", str(c.get("discord_admin_role_id", "")),
            ]
        return args

    # ---- lifecycle -----------------------------------------------------

    def is_running(self) -> bool:
        return self._bfdb is not None and self._bfdb.poll() is None

    def _redact(self, args: list[str]) -> str:
        secret_flags = {
            "--admin-password", "--discord-client-secret", "--dcsserverbot-api-key",
        }
        out, redact_next = [], False
        for a in args:
            out.append("***" if redact_next else a)
            redact_next = a in secret_flags
        return " ".join(out)

    async def _start_resolver(self) -> None:
        # One resolver serves every instance -- they are kept apart by their
        # distinct netidx bases, not by separate resolvers.
        if self.multi_instance:
            if not any(i.get("netidx_base") for i in self.instances_cfg):
                return
        elif not self.cfg.get("netidx_base"):
            return
        if _port_listening("127.0.0.1", NETIDX_RESOLVER_PORT):
            self.log.info(
                "FowlEngine/procman: netidx resolver already listening on "
                f"127.0.0.1:{NETIDX_RESOLVER_PORT} -- leaving it alone"
            )
            return
        # don't stack a second resolver on top of one that's still coming up
        if self._resolver is not None and self._resolver.poll() is None:
            self.log.info("FowlEngine/procman: netidx resolver child already spawned -- waiting on it")
        else:
            rcfg = self._resolved("netidx_resolver_config")
            override = self.cfg.get("netidx_resolver_cmd")
            if override:
                # exact command wins -- use it to pin whatever your installed
                # netidx version actually wants (run it by hand first to find out)
                cmd = list(override)
            else:
                cmd = ["netidx", "resolver-server", "-f"]
                if rcfg:
                    cmd += ["-c", rcfg]
            log_path = os.path.join(self.home, "procman-netidx.log") if self.home else None
            try:
                out = open(log_path, "ab", buffering=0) if log_path else subprocess.DEVNULL
                self._resolver = subprocess.Popen(cmd, cwd=self.home or None, stdout=out, stderr=out)
                self.log.info(f"FowlEngine/procman: spawned netidx resolver ({' '.join(cmd)}) -> {log_path}")
            except OSError as ex:
                self.log.error(
                    f"FowlEngine/procman: could not start netidx resolver ({ex}) -- "
                    "is netidx-tools on PATH for the bot's account? live engine features will be degraded"
                )
                return
        # give it a moment to bind before bfdb tries to subscribe
        for _ in range(20):
            if _port_listening("127.0.0.1", NETIDX_RESOLVER_PORT):
                self.log.info(f"FowlEngine/procman: netidx resolver is listening on 127.0.0.1:{NETIDX_RESOLVER_PORT}")
                return
            if self._resolver is not None and self._resolver.poll() is not None:
                tail = ""
                try:
                    with open(os.path.join(self.home, "procman-netidx.log"), "rb") as fh:
                        tail = fh.read()[-800:].decode("utf-8", "replace")
                except OSError:
                    pass
                self.log.error(
                    f"FowlEngine/procman: netidx resolver exited immediately (code "
                    f"{self._resolver.returncode}). Engine RPCs (GCI, live map, engine log) "
                    f"will all time out until it runs. Last output:\n{tail}"
                )
                self._resolver = None
                return
            await asyncio.sleep(0.25)
        self.log.error(
            f"FowlEngine/procman: netidx resolver still not listening on 127.0.0.1:{NETIDX_RESOLVER_PORT} "
            "after 5s -- check procman-netidx.log and %APPDATA%\\netidx\\client.json for the bot's account"
        )

    def _kill_orphan_bfdb(self) -> None:
        """Best-effort: kill any bfdb.exe we're not tracking (orphans from a
        bot process that was force-killed). No-op on non-Windows / if none."""
        our_pid = self._bfdb.pid if (self._bfdb and self._bfdb.poll() is None) else None
        try:
            import psutil
            for p in psutil.process_iter(["name", "pid"]):
                if (p.info.get("name") or "").lower() in ("bfdb.exe", "bfdb") and p.pid != our_pid:
                    self.log.warning(f"FowlEngine/procman: killing orphan bfdb.exe (pid {p.pid})")
                    p.kill()
            return
        except Exception:
            pass
        if os.name == "nt" and our_pid is None:
            try:
                subprocess.run(["taskkill", "/F", "/IM", "bfdb.exe"],
                               capture_output=True, timeout=10)
            except Exception:
                pass

    async def start(self, admin_password: str) -> None:
        """(Re)start the sidecar stack: apply staged bfdb.exe, render gci.json,
        start the resolver if needed, then start bfdb."""
        async with self._lock:
            if not self.enabled:
                self.log.debug("FowlEngine/procman: bfdb.manage is false -- not starting anything")
                return
            if not self.exe or not os.path.exists(self.exe):
                self.log.error(f"FowlEngine/procman: bfdb exe not found at {self.exe!r}")
                return
            os.makedirs(self.staging_dir, exist_ok=True)

            note = self.apply_staged("bfdb.exe", self.exe)
            if note:
                await self._safe_notify(f"🧩 bfdb: {note}")

            # Multi-instance renders instances.json (and one gci.<id>.json per
            # instance); single-server keeps the flat gci.json.
            instances_file = self._render_instances()
            gci_ok = False if instances_file else self._render_gci()
            await self._start_resolver()

            # kill any orphan bfdb.exe (e.g. left by a previous bot process that
            # was killed without cleanup) so the new one can bind its port
            self._kill_orphan_bfdb()

            args = self._build_args(admin_password, gci_ok, instances_file)
            self.log.info(f"FowlEngine/procman: launching {os.path.basename(self.exe)} {self._redact(args)}")
            # capture anything bfdb prints before its own --log-file logger is up
            # (missing-DLL loader errors, panics, "address in use", ...)
            boot_log = os.path.join(self.home, "procman-bfdb-boot.log") if self.home else None
            try:
                out = open(boot_log, "wb", buffering=0) if boot_log else subprocess.DEVNULL
                self._bfdb = subprocess.Popen([self.exe, *args], cwd=self.home or None,
                                              stdout=out, stderr=out)
            except OSError as ex:
                self.log.error(f"FowlEngine/procman: failed to launch bfdb: {ex}")
                self._bfdb = None
                return
            self._fail_count = 0
            self._first_fail = None
            self._last_restart = time.time()

            # give it a beat; if it died instantly, surface why now
            await asyncio.sleep(2.0)
            if self._bfdb.poll() is not None:
                tail = ""
                try:
                    with open(boot_log, "rb") as fh:
                        tail = fh.read()[-1200:].decode("utf-8", "replace")
                except OSError:
                    pass
                self.log.error(
                    f"FowlEngine/procman: bfdb.exe exited immediately (code {self._bfdb.returncode}). "
                    f"Common causes: missing lua.dll/opus.dll next to bfdb.exe, port 8880 already in use, "
                    f"bad --config path. Boot output:\n{tail}\n"
                    f"Also check {os.path.join(self.home, 'Logs', 'bfdb.log')}"
                )

    async def stop(self, *, stop_resolver: Optional[bool] = None) -> None:
        async with self._lock:
            await self._terminate(self._bfdb, "bfdb")
            self._bfdb = None
            if stop_resolver is None:
                stop_resolver = bool(self.cfg.get("own_resolver"))
            if stop_resolver:
                await self._terminate(self._resolver, "netidx resolver")
                self._resolver = None

    async def restart(self, admin_password: str) -> None:
        await self.stop()
        await self.start(admin_password)

    async def rebuild_stats(self, admin_password: str) -> str:
        """Stop bfdb, run a one-shot `bfdb.exe --rebuild-stats` (wipes every
        stats-archive-derived tree and rewinds both replay cursors), then start
        bfdb again so it re-ingests the stats log from the top -- this time
        with the idempotency guards and the name-blind round sweep in place, so
        the rebuilt numbers aren't inflated by the old whole-file re-reads.
        Returns a status line for the caller to relay."""
        db_path = os.path.join(self.home, "bfdb")
        jsonl = self._resolved("stats_jsonl")
        stats_dir = self._resolved("stats_dir")
        if not jsonl and not stats_dir:
            return "❌ neither `bfdb.stats_jsonl` nor `bfdb.stats_dir` is configured -- nothing to rebuild from."
        args = [self.exe, "--db", db_path, "--rebuild-stats"]
        if jsonl:
            args += ["--stats-jsonl", jsonl]
        if stats_dir:
            args += ["--stats-dir", stats_dir]
        async with self._lock:
            await self._terminate(self._bfdb, "bfdb")
            self._bfdb = None
        self._kill_orphan_bfdb()
        await asyncio.sleep(3.0)  # let sled release its file lock / settle

        # Snapshot the DB before we touch it, so a bad rebuild is fully
        # reversible: stop bfdb, restore this folder over `bfdb`, start bfdb.
        backup_note = ""
        if os.path.isdir(db_path):
            backup = f"{db_path}.rebuild-bak-{_now_tag()}"
            try:
                await asyncio.get_running_loop().run_in_executor(
                    None, lambda: shutil.copytree(db_path, backup))
                backup_note = f" (DB backed up to `{os.path.basename(backup)}`)"
                self.log.warning(f"FowlEngine/procman: DB snapshot -> {backup}")
            except Exception as ex:  # noqa: BLE001
                await self.start(admin_password)
                return f"❌ could not snapshot the DB before rebuild ({ex}); aborted, bfdb restarted unchanged."

        self.log.warning(f"FowlEngine/procman: running one-shot {self._redact(args)}")
        try:
            proc = await asyncio.get_running_loop().run_in_executor(
                None,
                lambda: subprocess.run(args, cwd=self.home or None,
                                       capture_output=True, text=True, timeout=120),
            )
        except Exception as ex:  # noqa: BLE001
            await self.start(admin_password)
            return f"❌ rebuild one-shot failed to run ({ex}); bfdb restarted with existing data."
        out = ((proc.stdout or "") + (proc.stderr or "")).strip()
        await self.start(admin_password)
        if proc.returncode != 0:
            return (f"❌ `--rebuild-stats` exited {proc.returncode}: {out[:400]}\n"
                    f"bfdb restarted with existing data{backup_note}.")
        return (f"✅ {out[:400] or 'wiped derived stats + rewound cursors'}{backup_note}\n"
                f"bfdb restarted -- it's re-ingesting the stats log now (watch the engine-log / perf embed). "
                f"Run `/feops merge_rounds` afterwards to fold in any residual rounds. "
                f"If the rebuilt numbers look wrong, restore the backup folder over `bfdb` and restart.")

    async def fresh_db(self, admin_password: str) -> str:
        """Last resort when the sled DB itself is corrupt (a torn page a normal
        rebuild can't get past): move the whole `bfdb` folder aside and start
        bfdb on an empty one. It then re-ingests `stats.jsonl` from the top --
        which reconstructs pilot stats, Discord links (from `Bind` stats) and a
        single clean round. LOST: dashboard login sessions (re-login is
        automatic), dashboard-added bans, any in-dashboard wiki edits (reverts
        to the seeded content), and per-round recon photos."""
        db_path = os.path.join(self.home, "bfdb")
        if not os.path.isdir(db_path):
            return f"❌ no bfdb DB folder at `{db_path}` -- nothing to move."
        async with self._lock:
            await self._terminate(self._bfdb, "bfdb")
            self._bfdb = None
        self._kill_orphan_bfdb()
        await asyncio.sleep(3.0)
        archived = f"{db_path}.corrupt-{_now_tag()}"
        try:
            os.rename(db_path, archived)
        except OSError as ex:
            await self.start(admin_password)
            return f"❌ could not move the DB folder ({ex}); bfdb restarted on the old one."
        self.log.warning(f"FowlEngine/procman: moved DB {db_path} -> {archived}, starting fresh")
        await self.start(admin_password)
        return (f"✅ old DB moved to `{os.path.basename(archived)}`, bfdb started on a fresh one.\n"
                f"It's re-ingesting `stats.jsonl` from the top now — pilot stats, Discord links and one "
                f"clean round rebuild over the next several minutes (watch the engine-log embed).\n"
                f"Re-add any dashboard bans; wiki is back to seeded content. "
                f"The old folder is kept in case you need to pull anything from it.")

    async def _terminate(self, proc: Optional[subprocess.Popen], label: str) -> None:
        if proc is None or proc.poll() is not None:
            return
        self.log.info(f"FowlEngine/procman: stopping {label} (pid {proc.pid})")
        proc.terminate()
        for _ in range(20):
            if proc.poll() is not None:
                return
            await asyncio.sleep(0.25)
        self.log.warning(f"FowlEngine/procman: {label} did not exit -- killing")
        proc.kill()

    # ---- health supervision -------------------------------------------

    async def health_ok(self) -> bool:
        url = self.api_url
        try:
            async with aiohttp.ClientSession() as s:
                try:
                    async with s.get(f"{url}/api/health", timeout=10) as r:
                        if r.status == 200:
                            return True
                        if r.status != 404:
                            return False
                except aiohttp.ClientResponseError:
                    pass
                async with s.get(f"{url}/api/stats", timeout=15) as r:
                    return r.status == 200
        except Exception:
            return False

    async def supervise_tick(self, admin_password: str) -> None:
        """Call every BFDB_HEALTH_CHECK_SECS from the cog's @tasks.loop."""
        if not self.enabled:
            return
        if await self.health_ok():
            self._fail_count = 0
            self._first_fail = None
            return

        self._fail_count += 1
        threshold = int(self.cfg.get("health_failures", 3))
        if self._fail_count < threshold:
            self.log.warning(
                f"FowlEngine/procman: bfdb health check failed ({self._fail_count}/{threshold})"
            )
            return

        self._first_fail = self._first_fail or time.time()
        unresponsive_for = time.time() - self._first_fail
        hung_after = int(self.cfg.get("hung_relaunch_secs", 600))
        if self.is_running() and unresponsive_for < hung_after:
            self.log.warning(
                f"FowlEngine/procman: bfdb unresponsive {unresponsive_for:.0f}s but process is "
                f"alive -- assuming startup/busy, relaunching after {hung_after}s"
            )
            return

        cooldown = int(self.cfg.get("restart_cooldown", 120))
        if time.time() - self._last_restart < cooldown:
            return

        why = "hung" if self.is_running() else "not running"
        self.log.error(f"FowlEngine/procman: bfdb {why} -- relaunching")
        self._fail_count = 0
        self._first_fail = None
        await self.restart(admin_password)
        await self._safe_notify(
            f"⚠️ **bfdb was {why}** at `{self.api_url}` -- the bot relaunched it."
        )

    async def _safe_notify(self, msg: str) -> None:
        try:
            await self._notify(msg)
        except Exception as ex:  # noqa: BLE001 - notification must never break supervision
            self.log.error(f"FowlEngine/procman: notify failed: {ex}")
