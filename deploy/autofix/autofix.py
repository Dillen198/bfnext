"""Fowl Engine auto-fix: hand new engine errors to Claude, get back a tested
fix branch, and publish it only when a person approves.

    python deploy/autofix/autofix.py check            # one pass over the issue report
    python deploy/autofix/autofix.py watch            # a pass every `interval_minutes`
    python deploy/autofix/autofix.py approve <id>     # merge autofix/<id> into vector and publish
    python deploy/autofix/autofix.py status           # what it has done so far

What a pass does, per new or regressed error in the bot's issue report:

  1. fetch the report from bfdb (GET /api/logs/issues?token=...), already
     scrubbed of IPs, UCIDs and secrets by the bot's log analyzer
  2. make a git worktree of origin/vector on branch autofix/<issue id>
  3. run Claude Code headless (`claude -p`) in it with agent-rules.md, the
     issue section on stdin, and only file tools plus an allow list of test
     commands: no web access, no push, every other command refused
     (`--permission-mode dontAsk`)
  4. check the result itself: no edits to protected paths, the checks for
     what changed pass when run again here
  5. commit, push the branch, and post a summary to the ops Discord webhook

Nothing reaches a server until `approve`, which merges the branch into
vector, pushes, and runs deploy/publish-release.ps1 for that commit.

Config: %LOCALAPPDATA%\\fowl-autofix\\config.json (see autofix.sample.json).
The Claude API key comes from ANTHROPIC_API_KEY. Stdlib only. README.md here
has the setup.
"""
from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import re
import shutil
import subprocess
import sys
import time
import urllib.parse
import urllib.request
from dataclasses import dataclass, field
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
RULES = HERE / "agent-rules.md"
DATA = Path(os.environ.get("LOCALAPPDATA", str(Path.home()))) / "fowl-autofix"

# What the agent may run without being asked. Everything else is refused
# (dontAsk). File reads and edits stay inside the worktree.
ALLOWED_TOOLS = [
    "Read", "Edit", "Write", "Glob", "Grep",
    "Bash(cargo fmt *)", "Bash(cargo check *)", "Bash(cargo clippy *)", "Bash(cargo test *)",
    "Bash(python -m pytest *)",
    "Bash(npm --prefix bfweb run typecheck)", "Bash(npm --prefix bfweb test)",
    "Bash(npm --prefix bfrsr-web run typecheck)", "Bash(npm --prefix bfrsr-web test)",
    "Bash(git diff *)", "Bash(git status *)", "Bash(git log *)",
]
TOOLS = "Read,Edit,Write,Glob,Grep,Bash"

# Paths a fix may not touch: what a release is built and signed with, and
# what decides which code runs where.
PROTECTED = (
    "deploy/", ".github/", ".claude/", "Cargo.lock", "bftools/Cargo.lock",
    "package-lock.json", "bfmanager/src-tauri/updater.pub", "bfmanager/src-tauri/engine.pub",
    "lua.lib", "lua.dll", "lua.sha256", "setup-build.ps1", "setup-build.sh",
)

# Crates whose tests to run when a path under them changed.
CRATES = {
    "dcso3": ["dcso3", "bflib", "bfrange"], "dcso3-core": ["dcso3-core", "dcso3", "bflib"],
    "bflib": ["bflib"], "bfrange": ["bfrange"], "bfdb": ["bfdb"], "bfprotocols": ["bfprotocols", "bflib", "bfdb"],
    "fowlbg": ["fowlbg", "bflib"], "livewx": ["livewx"], "yats": ["yats", "bfdb"],
}


# ---- config -----------------------------------------------------------------------------

@dataclass
class Config:
    report_url: str = ""                 # https://api.vectorstrike.org/api/logs/issues
    report_token: str = ""               # bfdb's --log-read-token
    discord_webhook: str = ""            # the ops channel's webhook
    repo: str = str(REPO)
    remote: str = "origin"
    base_branch: str = "vector"
    github_repo: str = "Dillen198/bfnext"
    claude_cmd: str = "claude"
    model: str = "claude-opus-5-5"
    max_turns: int = 80
    max_budget_usd: float = 6.0
    max_runs_per_day: int = 3
    min_count: int = 3                   # an issue must have been seen this often
    levels: list = field(default_factory=lambda: ["ERROR", "CRASH"])
    # where the issue came from: engine logs and the bot plugin are ours;
    # DCS's own log is mostly DCS (left out by default)
    sources: list = field(default_factory=lambda: ["engine", "bot"])
    skip_patterns: list = field(default_factory=lambda: [
        r"getaddrinfo failed", r"Cannot connect to host discord", r"ClientConnectorDNSError",
    ])
    interval_minutes: int = 15
    cargo_target_dir: str = r"C:\cargo-target\fowl-autofix"
    worktrees: str = str(DATA / "worktrees")
    publish_channel: str = "stable"

    @staticmethod
    def load(path: Path) -> "Config":
        c = Config()
        if path.exists():
            raw = json.loads(path.read_text(encoding="utf-8"))
            for k, v in raw.items():
                if k.startswith("_"):
                    continue
                if not hasattr(c, k):
                    raise SystemExit(f"{path}: unknown setting {k!r}")
                setattr(c, k, v)
        return c


# ---- the issue report ---------------------------------------------------------------------

@dataclass
class Issue:
    id: str
    level: str
    sources: list
    count: int
    status: str
    signature: str
    text: str                            # the whole section, as the agent gets it


HEADING = re.compile(r"^### `([0-9a-f]{6,})` (\w+) in (.+)$", re.M)


def parse_report(md: str) -> list[Issue]:
    """The issues in the analyzer's Markdown report (loganalyzer.render_report)."""
    out = []
    marks = list(HEADING.finditer(md))
    for i, m in enumerate(marks):
        end = marks[i + 1].start() if i + 1 < len(marks) else len(md)
        text = md[m.start():end].strip()
        count = re.search(r"seen \*\*(\d+)×\*\*", text)
        status = re.search(r"^- status: (\w+)", text, re.M)
        sig = re.search(r"^- signature: `(.*)`$", text, re.M)
        out.append(Issue(
            id=m.group(1), level=m.group(2).upper(),
            sources=[s.strip() for s in m.group(3).split(",")],
            count=int(count.group(1)) if count else 0,
            status=status.group(1) if status else "",
            signature=sig.group(1) if sig else "",
            text=text,
        ))
    return out


def worth_fixing(issue: Issue, cfg: Config, handled: dict) -> str | None:
    """None when this issue should go to the agent, else why not."""
    if issue.status not in ("new", "regressed"):
        return f"status {issue.status}"
    if issue.level not in cfg.levels:
        return f"level {issue.level}"
    if issue.count < cfg.min_count:
        return f"seen {issue.count}x, under {cfg.min_count}"
    kinds = {s.split(":", 1)[0] for s in issue.sources}
    if not kinds & set(cfg.sources):
        return f"from {', '.join(sorted(kinds))}"
    for p in cfg.skip_patterns:
        if re.search(p, issue.text, re.I):
            return f"matches skip pattern {p!r}"
    prev = handled.get(issue.id)
    if prev and not (issue.status == "regressed" and prev.get("status") != "regressed"):
        return f"already handled ({prev.get('outcome')})"
    return None


def fetch_report(cfg: Config) -> str:
    q = urllib.parse.urlencode({"min_level": "ERROR", "limit": 60})
    req = urllib.request.Request(f"{cfg.report_url}?{q}", headers={
        "Authorization": f"Bearer {cfg.report_token}", "User-Agent": "fowl-autofix",
    })
    with urllib.request.urlopen(req, timeout=30) as r:
        return r.read().decode("utf-8", "replace")


# ---- state ------------------------------------------------------------------------------

def load_state() -> dict:
    p = DATA / "state.json"
    return json.loads(p.read_text(encoding="utf-8")) if p.exists() else {"handled": {}, "runs": []}


def save_state(state: dict) -> None:
    DATA.mkdir(parents=True, exist_ok=True)
    tmp = DATA / "state.json.tmp"
    tmp.write_text(json.dumps(state, indent=2), encoding="utf-8")
    os.replace(tmp, DATA / "state.json")


def runs_today(state: dict, now: float | None = None) -> int:
    now = now if now is not None else time.time()
    return sum(1 for r in state.get("runs", []) if now - r < 86400)


# ---- git --------------------------------------------------------------------------------

def git(*args, cwd, check=True) -> str:
    r = subprocess.run(["git", *args], cwd=cwd, capture_output=True, text=True, encoding="utf-8", errors="replace")
    if check and r.returncode:
        raise RuntimeError(f"git {' '.join(args)}: {r.stderr.strip() or r.stdout.strip()}")
    return r.stdout


def changed_paths(wt: Path) -> list[str]:
    out = git("status", "--porcelain", "--untracked-files=all", cwd=wt)
    paths = []
    for line in out.splitlines():
        p = line[3:].strip().strip('"')
        if " -> " in p:
            p = p.split(" -> ", 1)[1]
        paths.append(p.replace("\\", "/"))
    return paths


def protected_hits(paths: list[str]) -> list[str]:
    return [p for p in paths if any(p == x or p.startswith(x) for x in PROTECTED)]


def checks_for(paths: list[str]) -> list[list[str]]:
    """The commands that prove a change to `paths`, the way CI would run them."""
    crates, cmds = [], []
    for p in paths:
        top = p.split("/", 1)[0]
        for c in CRATES.get(top, []):
            if c not in crates:
                crates.append(c)
    if crates:
        cmds.append(["cargo", "fmt", "--all", "--check"])
        pk = [a for c in crates for a in ("-p", c)]
        cmds.append(["cargo", "clippy", "--locked", *pk, "--all-targets", "--", "-D", "warnings"])
        cmds.append(["cargo", "test", "--locked", *pk])
    if any(p.startswith("DCSServerBot/") for p in paths):
        cmds.append([sys.executable, "-m", "pytest", "-q", "DCSServerBot/tests"])
    for app in ("bfweb", "bfrsr-web"):
        if any(p.startswith(f"{app}/") for p in paths):
            cmds.append(["npm", "--prefix", app, "run", "typecheck"])
            cmds.append(["npm", "--prefix", app, "test"])
    return cmds


def build_env(cfg: Config, wt: Path) -> dict:
    """What setup-build.ps1 sets, for the worktree: the DCS Lua the engine links."""
    env = dict(os.environ)
    env.update({
        "LUA_LIB": str(wt), "LUA_LINK": "dylib", "LUA_LIB_NAME": "lua",
        "CARGO_TARGET_DIR": cfg.cargo_target_dir, "CARGO_INCREMENTAL": "0",
        "PATH": f"{wt}{os.pathsep}{env.get('PATH', '')}",
    })
    return env


def make_worktree(cfg: Config, issue_id: str) -> tuple[Path, str]:
    repo = Path(cfg.repo)
    branch = f"autofix/{issue_id}"
    wt = Path(cfg.worktrees) / issue_id
    git("fetch", cfg.remote, cfg.base_branch, cwd=repo)
    if wt.exists():
        git("worktree", "remove", "--force", str(wt), cwd=repo, check=False)
    git("worktree", "prune", cwd=repo)
    git("branch", "-D", branch, cwd=repo, check=False)
    wt.parent.mkdir(parents=True, exist_ok=True)
    git("worktree", "add", "-b", branch, str(wt), f"{cfg.remote}/{cfg.base_branch}", cwd=repo)
    # bfdb embeds these with RustEmbed and won't compile without them
    for d in ("bfweb/dist", "bfsite/dist"):
        (wt / d).mkdir(parents=True, exist_ok=True)
    return wt, branch


# ---- the agent --------------------------------------------------------------------------

PROMPT = ("Fix the Fowl Engine bug described in the issue report on standard input, following the "
          "rules you were given. End with the JSON line.")


def claude_command(cfg: Config) -> list[str]:
    exe = shutil.which(cfg.claude_cmd) or cfg.claude_cmd
    return [
        exe, "-p", PROMPT,
        "--output-format", "json",
        "--model", cfg.model,
        "--max-turns", str(cfg.max_turns),
        "--max-budget-usd", str(cfg.max_budget_usd),
        "--append-system-prompt-file", str(RULES),
        "--tools", TOOLS,
        "--permission-mode", "dontAsk",
        "--strict-mcp-config",
        "--setting-sources", "project",
        "--no-session-persistence",
        # one argument per rule, as the CLI docs pass them; last, since it
        # takes every argument up to the next flag
        "--allowedTools", *ALLOWED_TOOLS,
    ]


def agent_verdict(stdout: str) -> dict:
    """The agent's closing JSON line, out of Claude Code's --output-format json reply."""
    try:
        wrapper = json.loads(stdout)
    except json.JSONDecodeError:
        return {"outcome": "error", "summary": "Claude Code gave no JSON reply", "raw": stdout[-2000:]}
    result = str(wrapper.get("result") or "")
    verdict = {"outcome": "error", "summary": "the agent ended without its JSON line"}
    for line in reversed(result.strip().splitlines()):
        line = line.strip().strip("`")
        if line.startswith("{") and line.endswith("}"):
            try:
                verdict = json.loads(line)
                break
            except json.JSONDecodeError:
                continue
    verdict["cost_usd"] = wrapper.get("total_cost_usd")
    verdict["turns"] = wrapper.get("num_turns")
    if wrapper.get("is_error"):
        verdict.setdefault("error", wrapper.get("subtype") or "error")
    return verdict


def run_agent(cfg: Config, wt: Path, issue: Issue) -> dict:
    r = subprocess.run(
        claude_command(cfg), cwd=wt, input=issue.text, capture_output=True, text=True,
        encoding="utf-8", errors="replace", env=build_env(cfg, wt), timeout=3 * 3600,
    )
    v = agent_verdict(r.stdout)
    if r.returncode and v.get("outcome") == "error":
        v["summary"] = f"{v.get('summary')} (exit {r.returncode}: {r.stderr.strip()[-400:]})"
    return v


def run_checks(cfg: Config, wt: Path, cmds: list[list[str]]) -> tuple[bool, str]:
    env = build_env(cfg, wt)
    log = []
    for cmd in cmds:
        exe = shutil.which(cmd[0]) or cmd[0]
        r = subprocess.run([exe, *cmd[1:]], cwd=wt, capture_output=True, text=True,
                           encoding="utf-8", errors="replace", env=env, timeout=3600)
        log.append(f"$ {' '.join(cmd)} -> {r.returncode}")
        if r.returncode:
            tail = (r.stdout + r.stderr).strip().splitlines()[-25:]
            return False, "\n".join(log + tail)
    return True, "\n".join(log)


# ---- Discord ----------------------------------------------------------------------------

def post(cfg: Config, text: str) -> None:
    if not cfg.discord_webhook:
        print(text)
        return
    body = json.dumps({"content": text[:1990], "allowed_mentions": {"parse": []}}).encode()
    req = urllib.request.Request(cfg.discord_webhook, data=body, method="POST", headers={
        "Content-Type": "application/json", "User-Agent": "fowl-autofix",
    })
    try:
        urllib.request.urlopen(req, timeout=20).read()
    except Exception as ex:  # a failed post must not lose the run
        print(f"discord post failed: {ex}\n{text}")


def describe(cfg: Config, issue: Issue, v: dict, branch: str | None, stat: str = "") -> str:
    cost = f"${v['cost_usd']:.2f}" if isinstance(v.get("cost_usd"), (int, float)) else "?"
    head = f"**Auto-fix** `{issue.id}` ({issue.level}, seen {issue.count}x): "
    if branch:
        url = f"https://github.com/{cfg.github_repo}/compare/{cfg.base_branch}...{branch}"
        return (f"{head}**{v.get('title') or 'fix ready'}**\n{v.get('summary', '')}\n"
                f"Tests: {v.get('tests', '')}\n```\n{stat.strip()[:600]}\n```"
                f"Diff: <{url}>\nCost {cost}, {v.get('turns') or '?'} turns. "
                f"To ship it: `python deploy/autofix/autofix.py approve {issue.id}`")
    return f"{head}{v.get('outcome')}: {v.get('summary', '')} (cost {cost})"


# ---- a pass -----------------------------------------------------------------------------

def handle(cfg: Config, issue: Issue, state: dict, dry_run: bool = False) -> dict:
    wt, branch = make_worktree(cfg, issue.id)
    state.setdefault("runs", []).append(time.time())
    save_state(state)
    v = run_agent(cfg, wt, issue)
    paths = changed_paths(wt)
    pushed = None
    stat = ""
    if v.get("outcome") == "fixed" and not paths:
        v.update(outcome="error", summary="said fixed but changed nothing")
    elif v.get("outcome") != "fixed" and paths:
        v["summary"] = f"{v.get('summary', '')} (left edits it did not claim; discarded)"
    elif v.get("outcome") == "fixed":
        hits = protected_hits(paths)
        if hits:
            v.update(outcome="rejected", summary=f"touched protected paths: {', '.join(hits)}")
        else:
            ok, log = run_checks(cfg, wt, checks_for(paths))
            if not ok:
                v.update(outcome="failed_checks", summary=f"{v.get('summary', '')}\nChecks failed here:\n{log[-900:]}")
            else:
                git("add", "-A", cwd=wt)
                msg = (f"autofix: {v.get('title') or issue.id}\n\n{v.get('summary', '')}\n\n"
                       f"Issue {issue.id} from the Fowl Engine issue report.\n\n"
                       f"Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>")
                subprocess.run(["git", "commit", "-q", "-F", "-"], cwd=wt, input=msg,
                               text=True, encoding="utf-8", check=True)
                stat = git("show", "--stat", "--format=", "HEAD", cwd=wt)
                if not dry_run:
                    git("push", "-f", cfg.remote, branch, cwd=wt)
                pushed = branch
    state["handled"][issue.id] = {
        "status": issue.status, "outcome": v.get("outcome"), "branch": pushed,
        "when": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "title": v.get("title"), "cost_usd": v.get("cost_usd"),
    }
    save_state(state)
    post(cfg, describe(cfg, issue, v, pushed, stat))
    if not pushed:
        git("worktree", "remove", "--force", str(wt), cwd=Path(cfg.repo), check=False)
        git("branch", "-D", branch, cwd=Path(cfg.repo), check=False)
    return v


def check(cfg: Config, dry_run: bool = False, report: str | None = None) -> None:
    state = load_state()
    issues = parse_report(report if report is not None else fetch_report(cfg))
    for issue in issues:
        why = worth_fixing(issue, cfg, state["handled"])
        if why:
            continue
        if runs_today(state) >= cfg.max_runs_per_day:
            print(f"daily limit of {cfg.max_runs_per_day} runs reached; {issue.id} waits")
            return
        print(f"working on {issue.id}: {issue.signature[:100]}")
        v = handle(cfg, issue, state, dry_run=dry_run)
        print(f"  -> {v.get('outcome')}: {v.get('title') or v.get('summary', '')[:120]}")


def approve(cfg: Config, issue_id: str, publish: bool = True) -> int:
    """Merge autofix/<id> into the base branch and publish an engine release of
    it. The exit code of the release (0 when only merging)."""
    repo = Path(cfg.repo)
    branch = f"autofix/{issue_id}"
    wt = Path(cfg.worktrees) / f"approve-{issue_id}"
    git("fetch", cfg.remote, cfg.base_branch, branch, cwd=repo)
    git("worktree", "remove", "--force", str(wt), cwd=repo, check=False)
    git("worktree", "add", "--detach", str(wt), f"{cfg.remote}/{cfg.base_branch}", cwd=repo)
    try:
        git("merge", "--no-ff", "-m", f"Merge {branch}", f"{cfg.remote}/{branch}", cwd=wt)
        sha = git("rev-parse", "HEAD", cwd=wt).strip()
        git("push", cfg.remote, f"HEAD:{cfg.base_branch}", cwd=wt)
    finally:
        git("worktree", "remove", "--force", str(wt), cwd=repo, check=False)
    title = load_state()["handled"].get(issue_id, {}).get("title") or issue_id
    if not publish:
        post(cfg, f"**Auto-fix** `{issue_id}` merged into {cfg.base_branch} (not published): {title}")
        return 0
    r = subprocess.run([
        "powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File",
        str(REPO / "deploy" / "publish-release.ps1"), "-Ref", sha, "-Channel", cfg.publish_channel,
        "-BotPlugin", "-Notes", f"Auto-fix, approved: {title}",
    ], cwd=repo)
    post(cfg, f"**Auto-fix** `{issue_id}` approved and "
              f"{'published' if r.returncode == 0 else 'merged, but the release FAILED (exit %d)' % r.returncode}: {title}")
    return r.returncode


def main(argv: list[str] | None = None) -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--config", type=Path, default=DATA / "config.json")
    sub = ap.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("check")
    c.add_argument("--dry-run", action="store_true", help="commit locally, push nothing")
    c.add_argument("--report", type=Path, help="read the report from a file instead of bfdb")
    sub.add_parser("watch")
    a = sub.add_parser("approve")
    a.add_argument("id")
    a.add_argument("--no-publish", action="store_true", help="merge and push, publish no release")
    sub.add_parser("status")
    args = ap.parse_args(argv)
    cfg = Config.load(args.config)
    if args.cmd == "status":
        print(json.dumps(load_state(), indent=2))
        return
    if args.cmd == "approve":
        sys.exit(approve(cfg, args.id, publish=not args.no_publish))
    if not os.environ.get("ANTHROPIC_API_KEY"):
        raise SystemExit("set ANTHROPIC_API_KEY (your Claude API key) first")
    if args.cmd == "check":
        report = args.report.read_text(encoding="utf-8") if args.report else None
        if report is None and not (cfg.report_url and cfg.report_token):
            raise SystemExit(f"set report_url and report_token in {args.config}")
        check(cfg, dry_run=args.dry_run, report=report)
        return
    while True:
        try:
            check(cfg)
        except Exception as ex:  # one bad pass must not stop the watch
            print(f"{dt.datetime.now():%H:%M} pass failed: {ex}")
        time.sleep(max(1, cfg.interval_minutes) * 60)


if __name__ == "__main__":
    main()
