"""
Offline tests for deploy/autofix/autofix.py: which issues go to the agent,
what the agent may run, and a whole pass with a stand-in for Claude Code
against a throwaway git repo (no API key, network or DCS needed).

    python -m pytest DCSServerBot/tests -q
"""
import importlib.util
import json
import subprocess
import sys
import textwrap
from pathlib import Path

import pytest

AUTOFIX = Path(__file__).resolve().parents[2] / "deploy" / "autofix" / "autofix.py"


@pytest.fixture()
def af(tmp_path, monkeypatch):
    spec = importlib.util.spec_from_file_location("autofix_under_test", AUTOFIX)
    mod = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, spec.name, mod)   # dataclasses look the module up
    spec.loader.exec_module(mod)
    monkeypatch.setattr(mod, "DATA", tmp_path / "data")
    return mod


REPORT = textwrap.dedent("""\
    # Fowl Engine issue report
    Node `Server4`

    ### `485ae3130c5c` ERROR in engine:[VS] Vector Strike | Dynamic Campaign PVP Modern
    - seen **692×**, first 2026-11-06T13:25:56Z, last 2026-11-06T13:28:56Z
    - status: new
    - signature: `could not send message (SetMarkupColor) runtime error`

    Sample 1:
    ```
    13:27:53 [ERROR] could not send message (SetMarkupColor) runtime error
    ```

    ### `9464563de8d6` CRASH in dcs:[VS] Vector Strike | Dynamic Campaign PVP Modern
    - seen **5×**, first 2026-11-06T13:33:57Z, last 2026-11-06T14:53:56Z
    - status: new
    - signature: `DCS crashed -- new crash files dcsNN.*`

    ### `aee7e54b041c` ERROR in bot
    - seen **3×**, first 2026-11-06T10:22:26Z, last 2026-11-06T10:22:26Z
    - status: new
    - signature: `Cannot connect to host discord.com:N ssl:default [getaddrinfo failed]`

    ### `d699d240ffdf` ERROR in engine:[VS] Vector Strike | RSR, engine:[VS] Vector Strike | Dynamic Campaign PVP Modern
    - seen **10×**, first 2026-11-03T07:32:39Z, last 2026-11-06T07:01:30Z
    - status: acknowledged
    - signature: `netidx stats archive corrupt (starting stats pub`
    """)


def test_the_report_parses_into_issues(af):
    issues = af.parse_report(REPORT)
    assert [i.id for i in issues] == ["485ae3130c5c", "9464563de8d6", "aee7e54b041c", "d699d240ffdf"]
    first = issues[0]
    assert (first.level, first.count, first.status) == ("ERROR", 692, "new")
    assert first.sources == ["engine:[VS] Vector Strike | Dynamic Campaign PVP Modern"]
    assert "SetMarkupColor" in first.signature and "Sample 1" in first.text
    assert "9464563de8d6" not in first.text
    assert len(issues[3].sources) == 2


def test_only_our_new_errors_go_to_the_agent(af):
    cfg = af.Config()
    why = {i.id: af.worth_fixing(i, cfg, {}) for i in af.parse_report(REPORT)}
    assert why["485ae3130c5c"] is None
    assert "dcs" in why["9464563de8d6"]                  # DCS's own crash
    assert "skip pattern" in why["aee7e54b041c"]         # a DNS outage
    assert "acknowledged" in why["d699d240ffdf"]
    issue = af.parse_report(REPORT)[0]
    assert "already handled" in af.worth_fixing(issue, cfg, {issue.id: {"status": "new", "outcome": "no_fix"}})
    issue.status = "regressed"                           # back after a fix: once more
    assert af.worth_fixing(issue, cfg, {issue.id: {"status": "new", "outcome": "fixed"}}) is None
    issue.count = 1
    assert "under" in af.worth_fixing(issue, cfg, {})


def test_the_agent_can_only_edit_and_test(af):
    cmd = af.claude_command(af.Config())
    flag = lambda name: cmd[cmd.index(name) + 1]
    assert flag("--permission-mode") == "dontAsk"
    assert flag("--tools") == "Read,Edit,Write,Glob,Grep,Bash"     # no web tools
    allowed = cmd[cmd.index("--allowedTools") + 1:]
    assert all(a.startswith(("Read", "Edit", "Write", "Glob", "Grep", "Bash(cargo ", "Bash(python -m pytest",
                             "Bash(npm --prefix ", "Bash(git diff", "Bash(git status", "Bash(git log"))
               for a in allowed), allowed
    assert not any("push" in a or "commit" in a or "Bash(*" in a for a in allowed)
    assert "--strict-mcp-config" in cmd and flag("--model") == "claude-opus-5-5"


def test_fixes_may_not_touch_how_releases_are_made(af):
    assert af.protected_hits(["bflib/src/lib.rs", "deploy/publish-release.ps1", ".github/workflows/release.yml",
                              "Cargo.lock", "bfmanager/src-tauri/engine.pub"]) == [
        "deploy/publish-release.ps1", ".github/workflows/release.yml", "Cargo.lock", "bfmanager/src-tauri/engine.pub"]


def test_checks_follow_what_changed(af):
    cmds = af.checks_for(["dcso3/src/unit.rs", "DCSServerBot/plugins/fowlengine/cog_player.py", "bfweb/src/api.ts"])
    test = next(c for c in cmds if c[:2] == ["cargo", "test"])
    assert test[test.index("-p") + 1:] [::2] == ["dcso3", "bflib", "bfrange"]
    assert any(c[1:3] == ["-m", "pytest"] for c in cmds)
    assert ["npm", "--prefix", "bfweb", "test"] in cmds
    assert af.checks_for(["README.md"]) == []


def test_the_verdict_is_read_from_the_last_line(af):
    wrapper = {"result": 'Looked at it.\n{"outcome": "fixed", "title": "t", "summary": "s", "tests": "ok"}',
               "total_cost_usd": 1.25, "num_turns": 9}
    v = af.agent_verdict(json.dumps(wrapper))
    assert (v["outcome"], v["title"], v["cost_usd"], v["turns"]) == ("fixed", "t", 1.25, 9)
    assert af.agent_verdict("not json")["outcome"] == "error"
    assert af.agent_verdict(json.dumps({"result": "no json here"}))["outcome"] == "error"


def _git(*args, cwd):
    subprocess.run(["git", *args], cwd=cwd, check=True, capture_output=True)


def _fake_claude(tmp_path, edit: str, verdict: dict) -> str:
    """A stand-in for `claude -p`: edits one file, answers like --output-format json."""
    script = tmp_path / "fake_claude.py"
    script.write_text(textwrap.dedent(f"""\
        import json, sys, pathlib
        args = sys.argv[1:]
        assert args[args.index("--permission-mode") + 1] == "dontAsk"
        issue = sys.stdin.read()
        assert "SetMarkupColor" in issue
        p = pathlib.Path({edit!r})
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text("fixed\\n")
        print(json.dumps({{"result": "done\\n" + json.dumps({verdict!r}), "total_cost_usd": 0.5, "num_turns": 3}}))
        """), encoding="utf-8")
    if sys.platform == "win32":
        shim = tmp_path / "fake_claude.cmd"
        shim.write_text(f'@"{sys.executable}" "{script}" %*\r\n', encoding="utf-8")
    else:
        shim = tmp_path / "fake_claude"
        shim.write_text(f'#!/bin/sh\nexec "{sys.executable}" "{script}" "$@"\n', encoding="utf-8")
        shim.chmod(0o755)
    return str(shim)


def _repo(tmp_path):
    remote = tmp_path / "remote.git"
    _git("init", "--bare", "-q", "-b", "vector", str(remote), cwd=tmp_path)
    repo = tmp_path / "repo"
    _git("clone", "-q", str(remote), str(repo), cwd=tmp_path)
    _git("config", "user.email", "t@example.com", cwd=repo)
    _git("config", "user.name", "t", cwd=repo)
    _git("checkout", "-q", "-b", "vector", cwd=repo)
    (repo / "notes.md").write_text("broken\n")
    _git("add", ".", cwd=repo)
    _git("commit", "-q", "-m", "init", cwd=repo)
    _git("push", "-q", "origin", "vector", cwd=repo)
    return repo, remote


def _cfg(af, tmp_path, repo, claude):
    cfg = af.Config()
    cfg.repo, cfg.claude_cmd, cfg.worktrees = str(repo), claude, str(tmp_path / "wt")
    return cfg


def test_a_fix_becomes_a_pushed_branch(af, tmp_path, monkeypatch):
    repo, remote = _repo(tmp_path)
    claude = _fake_claude(tmp_path, "notes.md", {"outcome": "fixed", "title": "Fix the notes", "summary": "s", "tests": "none"})
    cfg = _cfg(af, tmp_path, repo, claude)
    monkeypatch.setenv("GIT_AUTHOR_NAME", "t"); monkeypatch.setenv("GIT_AUTHOR_EMAIL", "t@example.com")
    monkeypatch.setenv("GIT_COMMITTER_NAME", "t"); monkeypatch.setenv("GIT_COMMITTER_EMAIL", "t@example.com")
    af.check(cfg, report=REPORT)
    state = json.loads((af.DATA / "state.json").read_text())
    assert state["handled"]["485ae3130c5c"]["outcome"] == "fixed"
    branches = subprocess.run(["git", "branch", "--list"], cwd=remote, capture_output=True, text=True).stdout
    assert "autofix/485ae3130c5c" in branches
    msg = subprocess.run(["git", "log", "-1", "--format=%B", "autofix/485ae3130c5c"], cwd=remote,
                         capture_output=True, text=True).stdout
    assert msg.startswith("autofix: Fix the notes")
    # a second pass leaves it alone
    af.check(cfg, report=REPORT)
    assert len(json.loads((af.DATA / "state.json").read_text())["runs"]) == 1


def test_a_fix_touching_release_files_is_refused(af, tmp_path, monkeypatch):
    repo, remote = _repo(tmp_path)
    claude = _fake_claude(tmp_path, "deploy/publish-release.ps1", {"outcome": "fixed", "title": "t", "summary": "s", "tests": "t"})
    monkeypatch.setenv("GIT_AUTHOR_NAME", "t"); monkeypatch.setenv("GIT_AUTHOR_EMAIL", "t@example.com")
    monkeypatch.setenv("GIT_COMMITTER_NAME", "t"); monkeypatch.setenv("GIT_COMMITTER_EMAIL", "t@example.com")
    af.check(_cfg(af, tmp_path, repo, claude), report=REPORT)
    state = json.loads((af.DATA / "state.json").read_text())
    assert state["handled"]["485ae3130c5c"]["outcome"] == "rejected"
    branches = subprocess.run(["git", "branch", "--list"], cwd=remote, capture_output=True, text=True).stdout
    assert "autofix/" not in branches


def test_the_daily_limit_holds(af):
    import time
    now = time.time()
    assert af.runs_today({"runs": [now - 10, now - 100, now - 90000]}, now) == 2


def test_approving_merges_the_fix_into_vector(af, tmp_path, monkeypatch):
    repo, remote = _repo(tmp_path)
    claude = _fake_claude(tmp_path, "notes.md", {"outcome": "fixed", "title": "Fix the notes", "summary": "s", "tests": "none"})
    for k in ("AUTHOR", "COMMITTER"):
        monkeypatch.setenv(f"GIT_{k}_NAME", "t")
        monkeypatch.setenv(f"GIT_{k}_EMAIL", "t@example.com")
    cfg = _cfg(af, tmp_path, repo, claude)
    af.check(cfg, report=REPORT)
    assert af.approve(cfg, "485ae3130c5c", publish=False) == 0
    shown = subprocess.run(["git", "show", "vector:notes.md"], cwd=remote, capture_output=True, text=True).stdout
    assert shown == "fixed\n"
