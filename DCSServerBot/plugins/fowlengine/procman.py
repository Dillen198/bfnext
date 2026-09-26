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

__all__ = [
    "Procman", "BFDB_HEALTH_CHECK_SECS", "INSTANCE_KINDS", "instance_kind",
    "effective_instance_gci", "default_range_jsonl", "sha256_of", "sha256_cached",
]

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


# What a `bfdb.instances:` entry runs. "campaign" is bflib (the default, and
# every instance that predates the training range); "range" is bfrange.
INSTANCE_KINDS = ("campaign", "range")


def instance_kind(inst: Optional[dict]) -> str:
    """The normalised `kind` of one instance entry (YAML or rendered JSON).
    Missing/blank -> "campaign". An unknown value also reads as "campaign" --
    _render_instances() is what reports it."""
    raw = str((inst or {}).get("kind") or "").strip().lower()
    return raw if raw in INSTANCE_KINDS else "campaign"


def effective_instance_gci(global_gci: Optional[dict], inst: Optional[dict]) -> dict:
    """One instance's effective `gci:` block: its own overrides merged over the
    shared top-level one.

    `atc:` is merged key by key, so a per-instance block that only overrides
    the frequencies keeps the shared ATIS settings.

    A `kind: range` instance never inherits `enabled: true` from the shared
    block: the range has no campaign picture for a controller to call, and a
    stray AWACS keyed up on the shared frequencies would talk over the
    campaign's. GCI is on for a range instance only if that instance's own
    `gci:` says `enabled: true`."""
    global_gci = dict(global_gci or {})
    inst = inst or {}
    inst_gci = inst.get("gci") or {}
    merged = dict(global_gci)
    merged.update(inst_gci)
    if isinstance(global_gci.get("atc"), dict) and isinstance(inst_gci.get("atc"), dict):
        merged_atc = dict(global_gci["atc"])
        merged_atc.update(inst_gci["atc"])
        merged["atc"] = merged_atc
    if instance_kind(inst) == "range" and "enabled" not in inst_gci:
        merged["enabled"] = False
    return merged


def default_range_jsonl(stats_jsonl: Optional[str]) -> Optional[str]:
    """bfrange writes Logs/range.jsonl next to Logs/stats.jsonl."""
    if not stats_jsonl:
        return None
    return os.path.join(os.path.dirname(stats_jsonl), "range.jsonl")


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


_SHA_CACHE: dict = {}


def sha256_cached(path: str) -> Optional[str]:
    """sha256_of, remembered per (path, size, mtime) -- the Ops page asks for
    the hash of 50 MB binaries every few seconds."""
    try:
        st = os.stat(path)
    except OSError:
        return None
    key = (os.path.normcase(path), st.st_size, st.st_mtime)
    hit = _SHA_CACHE.get(key)
    if hit:
        return hit
    val = sha256_of(path)
    if val:
        if len(_SHA_CACHE) > 64:
            _SHA_CACHE.clear()
        _SHA_CACHE[key] = val
    return val


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

        # For the Ops page: when bfdb last came up, how often it has been
        # relaunched, and how the last one ended.
        self.started_at: Optional[float] = None
        self.relaunches = 0
        self.last_exit_code: Optional[int] = None

        # A freshly swapped bfdb.exe is on probation until it has answered
        # /api/health for `probation_minutes`. If it dies, or never comes up,
        # the previous exe and the pre-swap DB snapshot are put back. Persisted
        # so a bot restart mid-probation keeps watching.
        self.probation: Optional[dict] = None
        # Hooks the cog wires to the auto-updater: on_swapped(sidecar) and
        # on_rollback(tag, why). Both optional.
        self.on_swapped: Optional[Callable[[dict], None]] = None
        self.on_rollback: Optional[Callable[[Optional[str], str], None]] = None
        # probation / snapshot tuning (the cog copies these from autoupdate:)
        self.probation_minutes = 10.0
        self.unhealthy_minutes = 15.0
        self.db_snapshots_keep = 3
        self._load_probation()

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
        # Instance overrides merged over the shared block (`atc:` key by key).
        # An instance-level `enabled:` wins; otherwise inherit the global one
        # -- except on a `kind: range` instance, which is off unless its own
        # `gci:` turns it on (see effective_instance_gci).
        merged = effective_instance_gci(self.gci_cfg, inst)
        if (instance_kind(inst) == "range" and self.gci_cfg.get("enabled")
                and "enabled" not in (inst.get("gci") or {})):
            self.log.info(
                f"FowlEngine/procman: instance {inst_id} is kind: range -- not inheriting "
                f"gci.enabled from the shared block (set `gci: {{enabled: true}}` on the "
                f"instance to run a controller there)"
            )
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

    def _validate_instance_kind(self, inst: dict) -> tuple[str, Optional[str]]:
        """(kind, range_jsonl) for one `bfdb.instances:` entry, logging what an
        operator would otherwise only find out from bfdb or an empty feed."""
        inst_id = inst.get("id")
        raw_kind = str(inst.get("kind") or "").strip().lower()
        if raw_kind and raw_kind not in INSTANCE_KINDS:
            self.log.error(
                f"FowlEngine/procman: instance {inst_id} has kind: {inst.get('kind')!r} -- "
                f"expected one of {', '.join(INSTANCE_KINDS)}; treating it as a campaign"
            )
        kind = instance_kind(inst)
        range_jsonl = _expand(inst.get("range_jsonl"))
        stats_jsonl = _expand(inst.get("stats_jsonl"))
        if kind == "range" and not range_jsonl:
            derived = default_range_jsonl(stats_jsonl)
            if derived:
                self.log.warning(
                    f"FowlEngine/procman: range instance {inst_id} has no `range_jsonl` -- "
                    f"using {derived} (next to its stats_jsonl, where bfrange writes it). "
                    f"Set range_jsonl explicitly to silence this."
                )
                range_jsonl = derived
            else:
                self.log.error(
                    f"FowlEngine/procman: range instance {inst_id} has neither `range_jsonl` "
                    f"nor `stats_jsonl` -- bfdb has nothing to ingest graded results from"
                )
        elif kind == "campaign" and range_jsonl:
            self.log.warning(
                f"FowlEngine/procman: instance {inst_id} sets range_jsonl but is not "
                f"kind: range -- passed through, but a campaign engine never writes one"
            )
        return kind, range_jsonl

    def _netidx_config_for(self, inst: dict) -> Optional[str]:
        """The netidx client config bfdb uses for one instance, or None for the
        shared default. `netidx_config:` names an existing file; `netidx_resolver:
        <ip>:<port>` has one rendered as <home>/netidx.<id>.json."""
        explicit = _expand(inst.get("netidx_config"))
        if explicit:
            return explicit
        resolver = str(inst.get("netidx_resolver") or "").strip()
        if not resolver:
            return None
        inst_id = inst.get("id")
        host, _, port = resolver.rpartition(":")
        if not host or not port.isdigit():
            self.log.error(
                f"FowlEngine/procman: instance {inst_id} has netidx_resolver: {resolver!r} -- "
                f"expected <LAN ip>:<port>, e.g. 192.168.1.60:4564; using the shared resolver"
            )
            return None
        if host in ("127.0.0.1", "localhost", "0.0.0.0"):
            self.log.error(
                f"FowlEngine/procman: instance {inst_id} has netidx_resolver: {resolver!r} -- "
                f"it must be the other PC's LAN address; using the shared resolver"
            )
            return None
        path = os.path.join(self.home, f"netidx.{inst_id}.json")
        client = {"base": "/", "addrs": [[resolver, "Anonymous"]], "default_auth": "Anonymous"}
        try:
            os.makedirs(self.home, exist_ok=True)
            tmp = path + ".tmp"
            with open(tmp, "w", encoding="utf-8") as fh:
                json.dump(client, fh, indent=2)
            os.replace(tmp, path)
        except OSError as ex:
            self.log.error(f"FowlEngine/procman: failed to write {path}: {ex}")
            return path if os.path.exists(path) else None
        return path

    def _warn_unreachable_paths(self, inst_id, entry: dict) -> None:
        """A network path bfdb cannot open is only ever an empty feed on the
        site; say so where an operator looks."""
        for key in ("stats_jsonl", "range_jsonl", "tacview_dir"):
            p = entry.get(key)
            if not p or not p.startswith(("\\\\", "//")):
                continue
            target = p if key == "tacview_dir" else os.path.dirname(p)
            if not os.path.isdir(target):
                self.log.error(
                    f"FowlEngine/procman: instance {inst_id}: {key} {p} is not reachable from "
                    f"this PC as the bot's account -- check the share on the DCS PC and the "
                    f"account's saved credentials (cmdkey). See deploy/range.md."
                )

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
            kind, range_jsonl = self._validate_instance_kind(inst)
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
                # Who the two sides are, for the war diary. Per instance
                # because the belligerents belong to the campaign, not to the
                # bfdb process. Unset -> bfdb names each side after the
                # country it started the campaign holding.
                "blue_faction": inst.get("blue_faction") or None,
                "red_faction": inst.get("red_faction") or None,
                "blue_adjective": inst.get("blue_adjective") or None,
                "red_adjective": inst.get("red_adjective") or None,
                # "campaign" (bflib) or "range" (bfrange). A range instance's
                # graded results come from range_jsonl (bfrange's
                # Logs/range.jsonl); its stats_jsonl still carries the
                # identity events. tacview_dir is where that server's Tacview
                # recordings land, for the debrief links on the range site.
                "kind": kind,
                "range_jsonl": range_jsonl,
                "tacview_dir": _expand(inst.get("tacview_dir")),
                # A DCS server on another PC with its own netidx resolver:
                # bfdb reaches that engine through this client config instead
                # of the shared one. See deploy/range.md, "Range on a second PC".
                "netidx_config": self._netidx_config_for(inst),
            }
            self._warn_unreachable_paths(inst_id, entry)
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

    # `staging_dir=` below: bfdb.exe is staged once, in the global
    # bfdb.staging_dir (one bfdb per box). An engine DLL is staged per DCS
    # instance, in that instance's own BFBinaries staging_dir -- callers pass
    # it; None keeps the global directory (the old behaviour).

    def _pending_path(self, name: str, staging_dir: Optional[str] = None) -> str:
        return os.path.join(staging_dir or self.staging_dir, f"{name}.pending")

    def pending_info(self, name: str, staging_dir: Optional[str] = None) -> Optional[dict]:
        p = self._pending_path(name, staging_dir)
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

    def cancel_pending(self, name: str, staging_dir: Optional[str] = None) -> bool:
        removed = False
        pending = self._pending_path(name, staging_dir)
        for p in (pending, pending + ".json"):
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

    def apply_staged(self, name: str, live_path: str, keep: int = 5,
                     staging_dir: Optional[str] = None) -> Optional[str]:
        """Swap staging/<name>.pending over live_path after a timestamped backup.
        Returns a human-readable note if a swap happened, else None. Never raises
        -- on failure the live file is left untouched (or restored).

        For bfdb.exe (only ever called with bfdb stopped) the database is
        snapshotted first and the new exe goes on probation -- see
        supervise_tick()."""
        pending = self._pending_path(name, staging_dir)
        if not os.path.exists(pending):
            return None
        sidecar = {}
        try:
            with open(pending + ".json", "r", encoding="utf-8") as fh:
                sidecar = json.load(fh) or {}
        except (OSError, ValueError):
            pass
        live_path = os.path.expandvars(live_path)
        backup = f"{live_path}.backup-{_now_tag()}"
        is_bfdb = name == "bfdb.exe" and os.path.normcase(live_path) == os.path.normcase(self.exe)
        is_rollback = bool(sidecar.get("rollback"))
        snapshot = None
        if is_bfdb and not is_rollback:
            snapshot = self._snapshot_db(sidecar.get("tag"))
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
        self.cancel_pending(name, staging_dir)  # clears the sidecar
        self._prune_backups(live_path, keep)
        note = f"swapped in staged `{name}` (backup: `{os.path.basename(backup)}`)"
        if sidecar.get("tag"):
            note += f" -- release {sidecar['tag']}"
        if snapshot:
            note += f", DB snapshot `{os.path.basename(snapshot)}`"
        self.log.warning(f"FowlEngine/procman: {note}")
        self._last_swap_note = note
        if is_bfdb and not is_rollback:
            self.probation = {
                "tag": sidecar.get("tag"),
                "git": sidecar.get("git"),
                "source": sidecar.get("source") or "manual",
                "backup": backup if os.path.exists(backup) else None,
                "db_snapshot": snapshot,
                "swapped_at": time.time(),
                "healthy_at": None,
                "exits": 0,
            }
            self._save_probation()
            if self.on_swapped:
                try:
                    self.on_swapped({**sidecar, "sha256": sha256_of(live_path)})
                except Exception as ex:  # noqa: BLE001
                    self.log.debug(f"FowlEngine/procman: on_swapped hook failed: {ex}")
        return note

    # ---- DB snapshots + bfdb probation ------------------------------------

    @property
    def db_path(self) -> str:
        return os.path.join(self.home, "bfdb")

    @property
    def backups_dir(self) -> str:
        return os.path.join(self.home, "_backups")

    def _snapshot_db(self, tag: Optional[str]) -> Optional[str]:
        """Copy the (stopped) sled DB aside before a new bfdb.exe touches it.
        bfdb persists bfprotocols types with positional bincode, so a build
        whose structs moved can leave a DB the old exe cannot read; this copy
        is what makes a bfdb rollback a real rollback. Keeps the newest few."""
        if not os.path.isdir(self.db_path):
            return None
        safe_tag = "".join(c if c.isalnum() or c in "-._" else "_" for c in (tag or "manual"))
        dest = os.path.join(self.backups_dir, f"db-{_now_tag()}-{safe_tag}")
        try:
            os.makedirs(self.backups_dir, exist_ok=True)
            shutil.copytree(self.db_path, dest)
        except Exception as ex:  # noqa: BLE001
            self.log.error(f"FowlEngine/procman: DB snapshot before the bfdb.exe swap failed ({ex}) -- "
                           f"swapping anyway, but a rollback will only restore the exe")
            shutil.rmtree(dest, ignore_errors=True)
            return None
        self.log.warning(f"FowlEngine/procman: DB snapshot -> {dest}")
        try:
            snaps = sorted(e.path for e in os.scandir(self.backups_dir)
                           if e.is_dir() and e.name.startswith("db-"))
            for stale in snaps[:-self.db_snapshots_keep]:
                shutil.rmtree(stale, ignore_errors=True)
        except OSError:
            pass
        return dest

    def list_backups(self) -> dict:
        """For the Ops page: exe backups next to bfdb.exe and the DB snapshots."""
        out: dict = {"bfdb_exe": [], "db_snapshots": []}
        exe_dir, exe_base = os.path.dirname(self.exe), os.path.basename(self.exe)
        try:
            for f in sorted(os.listdir(exe_dir), reverse=True):
                if f.startswith(f"{exe_base}.backup-"):
                    p = os.path.join(exe_dir, f)
                    out["bfdb_exe"].append({"name": f, "size": os.path.getsize(p),
                                            "mtime": os.path.getmtime(p)})
        except OSError:
            pass
        try:
            for e in sorted(os.scandir(self.backups_dir), key=lambda e: e.name, reverse=True):
                if e.is_dir() and e.name.startswith("db-"):
                    out["db_snapshots"].append({"name": e.name, "mtime": e.stat().st_mtime})
        except OSError:
            pass
        return out

    @property
    def _probation_file(self) -> str:
        return os.path.join(self.staging_dir, "bfdb-probation.json")

    def _load_probation(self) -> None:
        try:
            with open(self._probation_file, "r", encoding="utf-8") as fh:
                doc = json.load(fh)
            self.probation = doc if isinstance(doc, dict) and doc else None
        except (OSError, ValueError):
            self.probation = None

    def _save_probation(self) -> None:
        try:
            os.makedirs(self.staging_dir, exist_ok=True)
            if self.probation:
                with open(self._probation_file, "w", encoding="utf-8") as fh:
                    json.dump(self.probation, fh, indent=2)
            elif os.path.exists(self._probation_file):
                os.remove(self._probation_file)
        except OSError as ex:
            self.log.debug(f"FowlEngine/procman: probation state not saved: {ex}")

    async def rollback_bfdb(self, admin_password: str, why: str) -> str:
        """Put the previous bfdb.exe back -- and, when this swap took one, the
        DB snapshot from right before it (the broken build's DB is kept as
        bfdb.failed-<ts> for a post-mortem). Stats written since the swap are
        not lost: the JSONL cursor lives in the DB too, so the restored DB
        re-ingests them from the stats log, and the idempotency guards keep
        that from double-counting."""
        p = self.probation or {}
        backup = p.get("backup")
        if not backup:
            exe_dir, base = os.path.dirname(self.exe), os.path.basename(self.exe)
            try:
                cands = sorted((f for f in os.listdir(exe_dir) if f.startswith(f"{base}.backup-")),
                               reverse=True)
            except OSError:
                cands = []
            backup = os.path.join(exe_dir, cands[0]) if cands else None
        if not backup or not os.path.exists(backup):
            self.probation = None
            self._save_probation()
            return f"⛔ bfdb rollback wanted ({why}) but there is no bfdb.exe backup to restore."
        async with self._lock:
            await self._terminate(self._bfdb, "bfdb")
            self._bfdb = None
        self._kill_orphan_bfdb()
        await asyncio.sleep(3.0)
        tag = _now_tag()
        notes = []
        try:
            failed_exe = f"{self.exe}.failed-{tag}"
            if os.path.exists(self.exe):
                os.replace(self.exe, failed_exe)
            shutil.copy2(backup, self.exe)
            notes.append(f"exe ← `{os.path.basename(backup)}`")
        except OSError as ex:
            await self.start(admin_password)
            return f"⛔ bfdb rollback failed while restoring the exe ({ex}); restarted what was there."
        snap = p.get("db_snapshot")
        if snap and os.path.isdir(snap):
            try:
                if os.path.isdir(self.db_path):
                    os.rename(self.db_path, f"{self.db_path}.failed-{tag}")
                await asyncio.get_running_loop().run_in_executor(
                    None, lambda: shutil.copytree(snap, self.db_path))
                notes.append(f"DB ← `{os.path.basename(snap)}`")
            except Exception as ex:  # noqa: BLE001
                notes.append(f"DB restore FAILED ({ex}) -- running the old exe on the current DB")
        rel = p.get("tag")
        self.probation = None
        self._save_probation()
        # Before start(): the hook pulls this release out of every staging
        # dir, so start() can't swap the same bad build straight back in.
        if self.on_rollback:
            try:
                self.on_rollback(rel, why)
            except Exception as ex:  # noqa: BLE001
                self.log.debug(f"FowlEngine/procman: on_rollback hook failed: {ex}")
        await self.start(admin_password)
        msg = (f"⏪ **bfdb rolled back** -- {why}. " + "; ".join(notes)
               + (f". Release {rel} is marked bad." if rel else "."))
        self.log.error(f"FowlEngine/procman: {msg}")
        return msg

    async def _probation_tick(self, healthy: bool, admin_password: str) -> bool:
        """Returns True if it handled the tick (rolled back), so the normal
        relaunch logic doesn't also fire."""
        p = self.probation
        if not p:
            return False
        now = time.time()
        if healthy:
            if not p.get("healthy_at"):
                p["healthy_at"] = now
                self._save_probation()
            elif now - p["healthy_at"] >= self.probation_minutes * 60:
                self.probation = None
                self._save_probation()
                await self._safe_notify(f"✅ bfdb {p.get('tag') or '(manual upload)'} passed probation.")
            return False
        if not self.is_running():
            # died on the new build: that's the clearest signal there is
            p["exits"] = p.get("exits", 0) + 1
            self._save_probation()
            why = f"the new bfdb.exe exited (code {self.last_exit_code})"
            await self._safe_notify(await self.rollback_bfdb(admin_password, why))
            return True
        if not p.get("healthy_at") and now - p["swapped_at"] >= self.unhealthy_minutes * 60:
            why = f"the new bfdb.exe never answered /api/health in {self.unhealthy_minutes:.0f} min"
            await self._safe_notify(await self.rollback_bfdb(admin_password, why))
            return True
        return False

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
            # The war diary's writer. Omit all three and bfdb still files a
            # dispatch every day, from its own template bank.
            "--news-llm-url": (c.get("news_llm_url") or None),
            "--news-llm-key": (c.get("news_llm_key") or None),
            "--news-llm-model": (c.get("news_llm_model") or None),
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

    async def process_info(self) -> dict:
        """bfdb + resolver state for the Ops page."""
        running = self.is_running()
        return {
            "managed": self.enabled,
            "exe": self.exe,
            "home": self.home,
            "running": running,
            "pid": self._bfdb.pid if running else None,
            "healthy": await self.health_ok() if self.enabled else None,
            "started_at": self.started_at,
            "uptime_secs": int(time.time() - self.started_at) if (running and self.started_at) else None,
            "relaunches": self.relaunches,
            "last_exit_code": self.last_exit_code,
            "failing_checks": self._fail_count,
            "resolver_listening": _port_listening("127.0.0.1", NETIDX_RESOLVER_PORT, 0.5),
            "resolver_owned": self._resolver is not None and self._resolver.poll() is None,
            "last_swap": self._last_swap_note,
            "probation": self.probation,
            "exe_sha256": sha256_cached(self.exe) if self.exe else None,
            "pending": self.pending_info("bfdb.exe"),
        }

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
            os.makedirs(self.staging_dir, exist_ok=True)
            # A fresh box has no bfdb.exe yet: the first one arrives staged
            # (auto-update or a Discord upload), so apply before giving up.
            note = self.apply_staged("bfdb.exe", self.exe) if self.exe else None
            if not self.exe or not os.path.exists(self.exe):
                self.log.error(f"FowlEngine/procman: bfdb exe not found at {self.exe!r} and none is staged "
                               f"-- enable autoupdate or drop a bfdb.exe into the admin channel")
                return
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
            self.started_at = time.time()

            # give it a beat; if it died instantly, surface why now
            await asyncio.sleep(2.0)
            if self._bfdb.poll() is not None:
                self.last_exit_code = self._bfdb.returncode
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
        if self._bfdb is not None and self._bfdb.poll() is not None:
            self.last_exit_code = self._bfdb.returncode
        healthy = await self.health_ok()
        if await self._probation_tick(healthy, admin_password):
            return
        if healthy:
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
        self.relaunches += 1
        await self.restart(admin_password)
        await self._safe_notify(
            f"⚠️ **bfdb was {why}** at `{self.api_url}` -- the bot relaunched it."
        )

    async def _safe_notify(self, msg: str) -> None:
        try:
            await self._notify(msg)
        except Exception as ex:  # noqa: BLE001 - notification must never break supervision
            self.log.error(f"FowlEngine/procman: notify failed: {ex}")
