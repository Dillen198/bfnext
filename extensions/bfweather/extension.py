import asyncio
import os
import re
import shutil
import subprocess
import tempfile

from core import Extension, MizFile, Server, utils
from typing_extensions import override

__all__ = [
    "BFWeather",
    "BFWeatherException",
]

ANSI_ESCAPE_RE = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')

REQUIRED_PATH_KEYS = ('bftools', 'base', 'weapon', 'options')


class BFWeatherException(Exception):
    pass


class BFWeather(Extension):
    """
    Regenerates the mission from the bfnext-vector `bftools miz` build
    pipeline (base + weapon + options + warehouse templates) with live
    real-world weather baked in, every time DCSServerBot is about to load
    it -- so a scheduled restart always comes back with fresh conditions
    instead of whatever was authored in the template.

    This intentionally does NOT touch the mission bflib itself is running
    off of while the server is still up (unlike bflib's own live_weather
    config, which patches the already-running mission's zip in place right
    before shutdown). This extension only ever writes to a fresh temp file
    and hands DCSServerBot a new filename to load; if bftools fails for any
    reason, the existing mission is loaded unchanged and the failure is
    logged -- a bad weather fetch or template edit can never corrupt the
    live mission slot or block a restart.

    Campaign save progress is unaffected: bflib keys its saved campaign
    state off the mission's "sortie" field, which bftools carries through
    unchanged from the --base template. As long as --base stays the same
    template (same sortie, same objective zones), every rebuild resumes the
    same campaign save regardless of how many times the mission itself gets
    regenerated.

    nodes.yaml:
      MyNode:
        instances:
          MyInstance:
            extensions:
              BFWeather:
                bftools: 'E:\\Github\\bfnext-vector\\target\\release\\bftools.exe'
                base: 'E:\\Saved Games\\DCS\\Missions\\Vector\\v2\\base-odf2.1d6.miz'
                weapon: 'E:\\Saved Games\\DCS\\Missions\\Vector\\v2\\weapons.miz'
                options: 'E:\\Saved Games\\DCS\\Missions\\Vector\\v2\\options.miz'
                warehouse: 'E:\\Saved Games\\DCS\\Missions\\Vector\\v2\\warehouse.miz'  # optional
                lat: 33.4114
                lon: 36.5156
                live_time: false                     # optional, default false
                options_overrides: 'E:\\...\\overrides.json'  # optional
                timeout: 120                          # optional, seconds, default 120
    """

    def __init__(self, server: Server, config: dict):
        super().__init__(server, config)

    @override
    def is_available(self) -> bool:
        for key in REQUIRED_PATH_KEYS:
            path = self.config.get(key)
            if not path:
                self.log.error(f"  => {self.name}: missing '{key}' in your nodes.yaml.")
                return False
            if not os.path.exists(os.path.expandvars(path)):
                self.log.error(f"  => {self.name}: {key} path {path!r} not found.")
                return False
        warehouse = self.config.get('warehouse')
        if warehouse and not os.path.exists(os.path.expandvars(warehouse)):
            self.log.error(f"  => {self.name}: warehouse path {warehouse!r} not found.")
            return False
        if self.config.get('lat') is None or self.config.get('lon') is None:
            self.log.error(f"  => {self.name}: 'lat' and 'lon' are required in your nodes.yaml.")
            return False
        return True

    def _build_command(self, output: str) -> list[str]:
        cfg = self.config
        cmd = [
            os.path.expandvars(cfg['bftools']), 'miz',
            '--base', os.path.expandvars(cfg['base']),
            '--weapon', os.path.expandvars(cfg['weapon']),
            '--options', os.path.expandvars(cfg['options']),
            '--output', output,
            '--live-weather',
            '--live-weather-lat', str(cfg['lat']),
            '--live-weather-lon', str(cfg['lon']),
        ]
        if cfg.get('warehouse'):
            cmd += ['--warehouse', os.path.expandvars(cfg['warehouse'])]
        if cfg.get('live_time'):
            cmd.append('--live-time')
        if cfg.get('options_overrides'):
            cmd += ['--options-overrides', os.path.expandvars(cfg['options_overrides'])]
        if cfg.get('blue_production_template'):
            cmd += ['--blue-production-template', cfg['blue_production_template']]
        if cfg.get('red_production_template'):
            cmd += ['--red-production-template', cfg['red_production_template']]
        return cmd

    def _run_bftools(self, output: str):
        cmd = self._build_command(output)
        timeout = self.config.get('timeout', 120)
        self.log.debug(f"{self.name}: running {' '.join(cmd)}")
        # bftools uses plain env_logger::init(), which only shows error-level
        # logs unless RUST_LOG is set -- without this, its own info! lines
        # (including the applied temperature/QNH/wind values) are silently
        # dropped before we ever get a chance to see them.
        env = dict(os.environ, RUST_LOG=self.config.get('rust_log', 'info'))
        process = subprocess.Popen(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env
        )
        try:
            stdout, stderr = process.communicate(timeout=timeout)
        except subprocess.TimeoutExpired:
            process.kill()
            process.communicate()
            raise BFWeatherException(f"bftools timed out after {timeout}s")
        # env_logger writes to stderr; log it regardless of outcome so the
        # actual applied weather values show up in the DCSServerBot log.
        text = ANSI_ESCAPE_RE.sub('', (stderr or b'').decode('utf-8', errors='replace')).strip()
        if text:
            for line in text.splitlines():
                self.log.info(f"{self.name}: {line}")
        if process.returncode != 0:
            out_text = (stdout or b'').decode('utf-8', errors='replace')
            raise BFWeatherException(
                f"bftools exited with {process.returncode}: {ANSI_ESCAPE_RE.sub('', out_text) or text}"
            )
        if not os.path.exists(output) or os.path.getsize(output) == 0:
            raise BFWeatherException(f"bftools reported success but {output} is missing or empty")

    @override
    async def beforeMissionLoad(self, filename: str) -> tuple[str, bool]:
        if not self.is_available():
            return filename, False
        tmpfd, tmpname = tempfile.mkstemp(suffix='.miz')
        os.close(tmpfd)
        os.remove(tmpname)  # bftools must create the file itself
        try:
            await asyncio.to_thread(self._run_bftools, tmpname)
            # proof step: make sure the rebuilt mission actually parses
            # before it ever touches the live mission slot
            await asyncio.to_thread(MizFile, tmpname)
            new_filename = utils.create_writable_mission(filename)
            await asyncio.to_thread(shutil.copy2, tmpname, new_filename)
            self.log.info(f"{self.name}: applied live weather/time to {new_filename}.")
            return new_filename, True
        except Exception as ex:
            self.log.error(
                f"{self.name}: failed to regenerate mission with live weather, "
                f"loading the existing mission unchanged: {ex}"
            )
            return filename, False
        finally:
            if os.path.exists(tmpname):
                os.remove(tmpname)

    @override
    async def render(self, param: dict | None = None) -> dict:
        return {
            "name": self.name,
            "version": self.version,
            "value": f"({self.config.get('lat')}, {self.config.get('lon')})",
        }
