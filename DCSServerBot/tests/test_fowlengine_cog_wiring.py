"""
The FowlEngine cog is one class assembled from mixins (commands.py plus the
cog_*.py parts). These tests keep that wiring honest.

* Static, always run: every cog_*.py mixin is a base of FowlEngine, no member
  is defined twice across the parts, and every loop cog_load starts exists
  and is cancelled again in cog_unload.
* Import, run when discord.py is installed (skipped in CI, whose venv leaves
  it out on purpose): the real module under a stub of DCSServerBot's `core`,
  checked for the slash commands, groups, loops and listeners the bot
  registers.
"""
import ast
import re
import shutil
import sys
import tempfile
import types
from pathlib import Path

import pytest

PLUGIN = Path(__file__).resolve().parent.parent / "plugins" / "fowlengine"
PARTS = sorted(PLUGIN.glob("cog_*.py"))


def _classes():
    """{module file name: (class name, [member names], bases)} for commands.py's
    FowlEngine and each part's mixin."""
    out = {}
    for path in [PLUGIN / "commands.py", *PARTS]:
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for n in tree.body:
            if isinstance(n, ast.ClassDef) and (n.name == "FowlEngine" or n.name.endswith("Mixin")):
                members = []
                for m in n.body:
                    if isinstance(m, (ast.FunctionDef, ast.AsyncFunctionDef)):
                        members.append(m.name)
                    elif isinstance(m, ast.Assign):
                        members += [t.id for t in m.targets if isinstance(t, ast.Name)]
                out[path.name] = (n.name, members, [ast.unparse(b) for b in n.bases])
    return out


def test_every_part_is_a_base_and_nothing_is_defined_twice():
    classes = _classes()
    name, members, bases = classes["commands.py"]
    assert name == "FowlEngine" and bases[-1] == "Plugin", bases
    assert PARTS, "no cog_*.py parts found"
    seen = {m: "commands.py" for m in members}
    for path in PARTS:
        klass, part_members, _ = classes[path.name]
        assert klass in bases, f"{path.name}: {klass} is not a base of FowlEngine"
        for m in part_members:
            assert m not in seen, f"{m} is defined in both {seen[m]} and {path.name}"
            seen[m] = path.name
    assert len(bases) == len(PARTS) + 1, bases


def test_loops_started_in_cog_load_exist_and_are_cancelled():
    src = (PLUGIN / "commands.py").read_text(encoding="utf-8")
    load = src[src.index("async def cog_load"):src.index("async def cog_unload")]
    unload = src[src.index("async def cog_unload"):]
    unload = unload[:unload.index("\n    def ")]
    started = set(re.findall(r"utils\.safe_start\(self\.(\w+)\)", load))
    cancelled = set(re.findall(r"utils\.safe_cancel\(self\.(\w+)\)", unload))
    assert started and started == cancelled, (started ^ cancelled)
    every = {m for _, members, _ in _classes().values() for m in members}
    assert started <= every, started - every


# ---- the real module, under a stub `core` ---------------------------------------

EXPECTED_COMMANDS = {
    "fe_ban", "fe_briefing", "fe_card", "fe_commander", "fe_commanders", "fe_dashboard", "fe_gci",
    "fe_objective", "fe_rotation", "fe_rotation_advance", "fe_rotation_vote", "fe_side_switch",
    "fe_terminal", "fe_unban",
}
EXPECTED_GROUPS = {
    "feops": {"bfdb_restart", "briefing_lock", "campaign_apply", "campaign_keep", "db_restore", "fresh_db",
              "gci_show", "icons_install", "icons_status", "icons_uninstall", "issues", "merge_rounds",
              "rebuild_stats", "stage_apply", "stage_cancel", "stage_status", "update_check", "update_pause",
              "update_rollback", "update_status", "update_unmark", "versions"},
    "range": {"greenie", "me", "status"},
}
EXPECTED_LOOPS = {
    "autoupdate_loop", "heartbeat_loop", "poll_range_results", "post_news", "stage_apply_watch",
    "status_watch", "supervise_bfdb", "supervise_campaign_events", "supervise_engine_logs",
    "supervise_gci_transcript", "sync_coalition_roles", "sync_ranks", "update_briefings",
    "update_perf_status", "update_range_status", "update_server_info", "update_status",
}
EXPECTED_LISTENERS = {"on_member_join", "on_message"}


def _stub_core(app_commands, dcommands):
    class AnyMeta(type):
        def __getattr__(cls, n):
            if n.startswith("__"):
                raise AttributeError(n)
            return cls()

    class Any(metaclass=AnyMeta):
        def __init__(self, *a, **k):
            pass

        def __call__(self, *a, **k):
            return Any()

        def __getattr__(self, n):
            return Any()

        def __class_getitem__(cls, item):
            return cls

    class Mod(types.ModuleType):
        def __getattr__(self, n):
            if n.startswith("__"):
                raise AttributeError(n)
            return Any

    class PluginMeta(type(dcommands.Cog)):
        pass

    class Plugin(dcommands.Cog, metaclass=PluginMeta):
        def __class_getitem__(cls, item):
            return cls

    class Transformer(app_commands.Transformer):
        def __init__(self, *a, **k):
            super().__init__()

        async def transform(self, interaction, value):
            return value

    class Utils(types.ModuleType):
        def __getattr__(self, n):
            if n.startswith("__"):
                raise AttributeError(n)
            return Transformer if n.endswith("Transformer") else (lambda *a, **k: (lambda f: f))

    class Server:
        pass

    core = Mod("core")
    core.Plugin = Plugin
    core.Server = Server
    core.command = lambda **kw: app_commands.command(
        **{k: v for k, v in kw.items() if k in ("name", "description")})
    core.utils = Utils("core.utils")
    for n in ("Status", "EventListener", "event", "ServiceRegistry", "UploadStatus"):
        setattr(core, n, Any)
    mods = {"core": core}
    for m in ("services", "services.bot", "services.webservice", "services.servicebus"):
        mods[m] = Mod(m)
    mods["services.bot"].DCSServerBot = Any
    return mods


def test_the_cog_registers_every_command_loop_and_listener(monkeypatch):
    discord = pytest.importorskip("discord")
    from discord import app_commands
    from discord.ext import commands as dcommands, tasks

    root = Path(tempfile.mkdtemp(prefix="fe-wiring-"))
    try:
        (root / "plugins").mkdir()
        (root / "plugins" / "__init__.py").write_text("")
        shutil.copytree(PLUGIN, root / "plugins" / "fowlengine",
                        ignore=shutil.ignore_patterns("__pycache__", "assets"))
        for name, mod in _stub_core(app_commands, dcommands).items():
            monkeypatch.setitem(sys.modules, name, mod)
        for name in [m for m in sys.modules if m == "plugins" or m.startswith("plugins.")]:
            monkeypatch.delitem(sys.modules, name)
        monkeypatch.syspath_prepend(str(root))
        fe = __import__("plugins.fowlengine.commands", fromlist=["FowlEngine"]).FowlEngine

        top = {c.name: c for c in fe.__cog_app_commands__}
        assert set(top) == EXPECTED_COMMANDS | set(EXPECTED_GROUPS), set(top) ^ (EXPECTED_COMMANDS | set(EXPECTED_GROUPS))
        for group, children in EXPECTED_GROUPS.items():
            assert isinstance(top[group], app_commands.Group)
            assert {c.name for c in top[group].commands} == children, group
        loops = {n for n in dir(fe) if isinstance(getattr(fe, n, None), tasks.Loop)}
        assert loops == EXPECTED_LOOPS, loops ^ EXPECTED_LOOPS
        assert {name for name, _ in fe.__cog_listeners__} == EXPECTED_LISTENERS
    finally:
        for name in [m for m in sys.modules if m == "plugins" or m.startswith("plugins.")]:
            sys.modules.pop(name, None)
        shutil.rmtree(root, ignore_errors=True)
        del discord
