"""
Log analyzer: turns the stack's log files into a short list of distinct issues.

Every `scan_seconds` it reads what was appended to each log since last time:

  engine:<server>   <instance home>/Logs/bfnext.txt   (bflib; bfrange.txt on a range)
  dcs:<server>      <instance home>/Logs/dcs.log      (script errors + DCS crashes only)
  bfdb              bfdb.log_file
  bot               <bot>/logs/dcssb-<node>.log       (Python tracebacks included)

and new DCS crash dumps in each <instance home>/Logs.

Nothing is lost to a restart: everything read is also appended to a persistent
archive, <bfdb.home>/_logarchive/<source>/<UTC date>.log (gzipped once the day
is over, kept `archive_days`, default 90; 0 = forever). DCS overwrites dcs.log
at every start and bfdb's in-memory tails reset with bfdb -- the archive
doesn't. A log rotated between two scans is finished from its renamed file
before the new one is started, so the lines right before a crash are kept.

Multi-line entries (Rust
panics, Python tracebacks, pretty-printed debug) are folded into the line that
started them. Each WARN/ERROR entry is FINGERPRINTED -- timestamps, numbers,
ids, coordinates, UCIDs and IPs normalised away -- so a thousand copies of one
bug are one issue with a count, not a thousand lines.

Per issue it keeps: first/last seen, total count, which engine builds it was
seen on, and up to three scrubbed samples with the lines around them. An issue
that was marked fixed and turns up again is flagged REGRESSED.

Where issues go:
  * the dashboard OPS page (Issues card: acknowledge / ignore / mark fixed,
    copy a report)
  * the ops Discord channel -- once per NEW error-level issue
  * GET <bfdb>/api/logs/issues?token=...  (bfdb's --log-read-token): the same
    report as plain Markdown, which is what Claude fetches to work on them
  * optionally, a GitHub issue per new issue (`issues.github`) -- point it at
    a PRIVATE repo: samples can carry player names.

Scrubbing (always, before anything leaves the box): configured secrets, IPv4
addresses, 32-hex UCIDs, Discord webhook URLs, bearer tokens.
"""
from __future__ import annotations

import asyncio
import hashlib
import json
import os
import re
import time
from collections import deque
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Iterable, Optional

__all__ = [
    "LogAnalyzer", "parse_entries", "fingerprint", "scrub", "Entry", "IssuesConfig",
    "render_report", "LEVEL_RANK",
]

LEVEL_RANK = {"CRASH": 4, "PANIC": 4, "ERROR": 3, "WARN": 2, "INFO": 1, "DEBUG": 0, "TRACE": 0}
MAX_ISSUES = 400
SAMPLES_PER_ISSUE = 3
CONTEXT_BEFORE = 4
READ_CAP_BYTES = 8 * 1024 * 1024       # per file per scan; a bigger backlog catches up over several


# ---- parsing --------------------------------------------------------------------

# One regex per log format. Each yields (level, rest-of-line). A line matching
# none of a source's starters is a continuation of the previous entry.
_STARTERS = {
    # bflib / bfrange (simplelog): 13:05:25 [ERROR] (20) bflib::db: msg
    "engine": re.compile(r"^\d{2}:\d{2}:\d{2}\s+\[(TRACE|DEBUG|INFO|WARN|ERROR)\]\s+(?:\(\d+\)\s+)?(.*)$"),
    # DCS: 2026-09-26 13:04:38.123 ERROR   SCRIPTING (Main): msg
    "dcs": re.compile(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3}\s+(INFO|WARNING|ERROR|ALERT|DEBUG)\s+(.*)$"),
    # bfdb (env_logger): [2026-09-26T13:04:38Z ERROR bfdb::db] msg
    "bfdb": re.compile(r"^\[\d{4}-\d{2}-\d{2}T\S+\s+(TRACE|DEBUG|INFO|WARN|ERROR)\s+(.*)$"),
    # DCSServerBot: 2026-09-26 13:04:38.123 ERROR<TAB>msg
    "bot": re.compile(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}[.,]\d{3}\s+(DEBUG|INFO|WARNING|ERROR|CRITICAL)\s+(.*)$"),
}
_LEVEL_ALIASES = {"WARNING": "WARN", "CRITICAL": "ERROR", "ALERT": "ERROR", "FATAL": "ERROR"}

# DCS.log is thousands of lines of asset/terrain chatter. Only these count.
_DCS_KEEP = re.compile(r"SCRIPTING|LuaNET|bflib|bfrange|fowl|Lua::|lua_|\.lua:\d+", re.I)
# A real DCS crash writes a dump header ("# -------------- 20260926-102233
# --------------") and the exception ("# C0000005 ACCESS_VIOLATION at ...").
# Not just any EDCORE line with "crash" in it: terrain textures are called
# crashmodels / airfieldsbuilding_crash and DCS logs "No suitable driver found
# to mount ..." for them on every start.
_DCS_CRASH = re.compile(r"EDCORE.*(# -{5,}\s*\d{8}-\d{6}|ACCESS_VIOLATION|# C0000\w{3}|EXCEPTION_\w+)", re.I)


@dataclass
class Entry:
    source: str
    level: str
    text: str                      # first line, without the timestamp
    extra: list = field(default_factory=list)   # continuation lines
    context: list = field(default_factory=list)  # a few lines before it

    @property
    def full(self) -> str:
        return "\n".join([self.text, *self.extra])


def _kind_of(source: str) -> str:
    return source.split(":", 1)[0]


def parse_entries(source: str, lines: Iterable[str], min_level: str = "WARN",
                  tail: Optional[deque] = None) -> list[Entry]:
    """Fold raw lines into entries and keep those at/above min_level.
    `tail` carries recent raw lines across calls (context for the first
    entries of the next chunk)."""
    kind = _kind_of(source)
    starter = _STARTERS.get(kind)
    floor = LEVEL_RANK.get(min_level, 2)
    ctx = tail if tail is not None else deque(maxlen=CONTEXT_BEFORE)
    out: list[Entry] = []
    cur: Optional[Entry] = None
    keep_cur = False
    for raw in lines:
        line = raw.rstrip("\r\n")
        m = starter.match(line) if starter else None
        if m is None and starter is None:
            # unknown format: a line with a level word is a new entry
            mm = re.search(r"\b(ERROR|WARN(?:ING)?|CRITICAL|FATAL|PANIC)\b", line)
            m = (mm.group(1), line) if mm else None
            if m:
                m = _FakeMatch(m)
        if m is not None:
            if cur is not None and keep_cur:
                out.append(cur)
            level = _LEVEL_ALIASES.get(m.group(1), m.group(1))
            rest = m.group(2)
            if kind == "dcs":
                if _DCS_CRASH.search(rest):
                    level = "CRASH"
                elif not _DCS_KEEP.search(rest):
                    level = "INFO"  # not ours: never an issue
            if "panicked at" in rest or rest.lstrip().lower().startswith("panic"):
                level = "PANIC"
            cur = Entry(source=source, level=level, text=rest, context=list(ctx))
            keep_cur = LEVEL_RANK.get(level, 0) >= floor
        elif cur is not None:
            if len(cur.extra) < 60:
                cur.extra.append(line)
            if "panicked at" in line and keep_cur is False:
                cur.level, keep_cur = "PANIC", True
        ctx.append(line)
    if cur is not None and keep_cur:
        out.append(cur)
    return out


class _FakeMatch:
    def __init__(self, groups):
        self._g = groups

    def group(self, i):
        return self._g[i - 1]


# ---- fingerprinting + scrubbing -----------------------------------------------------

_NORMALISE = [
    (re.compile(r"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"), "<uuid>"),
    (re.compile(r"\b[0-9a-fA-F]{32}\b"), "<ucid>"),
    (re.compile(r"\b\d{1,3}(?:\.\d{1,3}){3}(?::\d+)?\b"), "<ip>"),
    # names in Debug-quoted strings ("Kutaisi", "Viper 1-1 | Bob"): one bug
    # about many objectives or players is still one issue
    (re.compile(r'"[^"\n]{1,160}"'), '"S"'),
    (re.compile(r"\b0x[0-9a-fA-F]+\b"), "<hex>"),
    (re.compile(r"\b[0-9a-f]{12,}\b"), "<hash>"),
    (re.compile(r"-?\d+(?:\.\d+)?(?:e-?\d+)?"), "N"),
    (re.compile(r"\s+"), " "),
]


def _last_meaningful(extra: list[str]) -> str:
    """For a Python traceback / Rust panic: the exception line and the
    innermost frame are what identify it."""
    exc = next((l for l in reversed(extra) if l.strip() and not l.startswith(" ")), "")
    frame = next((l.strip() for l in reversed(extra) if l.strip().startswith("File ")), "")
    return f"{frame} | {exc}".strip(" |")


def signature(e: Entry) -> str:
    base = e.text
    if e.extra and ("Traceback" in e.full or "panicked at" in e.full or "stack backtrace" in e.full):
        base = f"{base} || {_last_meaningful(e.extra)}"
    s = base
    for rx, rep in _NORMALISE:
        s = rx.sub(rep, s)
    return s.strip()[:240]


def fingerprint(e: Entry) -> str:
    return hashlib.sha1(f"{_kind_of(e.source)}|{signature(e)}".encode("utf-8", "replace")).hexdigest()[:12]


_SCRUB = [
    (re.compile(r"https://(?:ptb\.|canary\.)?discord(?:app)?\.com/api/webhooks/\S+"), "<discord-webhook>"),
    (re.compile(r"(?i)(bearer\s+)[A-Za-z0-9._\-]{12,}"), r"\1<token>"),
    (re.compile(r"(?i)((?:token|password|secret|api[_-]?key)\s*[=:]\s*)\S+"), r"\1<redacted>"),
    (re.compile(r"\b\d{1,3}(?:\.\d{1,3}){3}\b"), "<ip>"),
    (re.compile(r"\b[0-9a-fA-F]{32}\b"), "<ucid>"),
]


def scrub(text: str, secrets: Iterable[str] = ()) -> str:
    for s in secrets:
        if s and len(s) >= 6:
            text = text.replace(s, "<secret>")
    for rx, rep in _SCRUB:
        text = rx.sub(rep, text)
    return text


# ---- config ---------------------------------------------------------------------------

@dataclass
class IssuesConfig:
    enabled: bool = True
    scan_seconds: int = 60
    min_level: str = "WARN"
    notify: bool = True
    notify_min_level: str = "ERROR"
    ignore: list = field(default_factory=list)
    extra_files: list = field(default_factory=list)    # [{name, path, kind?}]
    include_dcs_log: bool = True
    github_repo: Optional[str] = None
    github_token: Optional[str] = None
    github_min_count: int = 3
    github_labels: list = field(default_factory=lambda: ["auto-report"])
    archive: bool = True
    archive_dir: Optional[str] = None
    archive_days: int = 90

    @classmethod
    def from_dict(cls, raw: Optional[dict]) -> "IssuesConfig":
        raw = raw or {}
        c = cls()
        c.enabled = bool(raw.get("enabled", True))
        c.scan_seconds = max(15, int(raw.get("scan_seconds", 60) or 60))
        lvl = str(raw.get("min_level") or "WARN").upper()
        c.min_level = _LEVEL_ALIASES.get(lvl, lvl) if lvl in ("WARN", "WARNING", "ERROR") else "WARN"
        c.notify = bool(raw.get("notify", True))
        nl = str(raw.get("notify_min_level") or "ERROR").upper()
        c.notify_min_level = _LEVEL_ALIASES.get(nl, nl) if nl in ("WARN", "WARNING", "ERROR") else "ERROR"
        c.ignore = [str(x) for x in (raw.get("ignore") or []) if x]
        c.extra_files = [x for x in (raw.get("extra_files") or []) if isinstance(x, dict) and x.get("path")]
        c.include_dcs_log = bool(raw.get("include_dcs_log", True))
        gh = raw.get("github") or {}
        repo = str(gh.get("repo") or "").strip().strip("/")
        c.github_repo = repo or None
        tok = str(gh.get("token") or "").strip()
        c.github_token = os.path.expandvars(tok) if tok else None
        c.github_min_count = max(1, int(gh.get("min_count", 3) or 3))
        c.github_labels = [str(x) for x in (gh.get("labels") or ["auto-report"])]
        c.archive = bool(raw.get("archive", True))
        ad = str(raw.get("archive_dir") or "").strip()
        c.archive_dir = os.path.expandvars(ad) if ad else None
        c.archive_days = max(0, int(raw.get("archive_days", 90) or 0))
        return c


# ---- the analyzer -------------------------------------------------------------------------

def _utc() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


class LogAnalyzer:
    def __init__(self, cog, log, state_path: str):
        self.cog = cog
        self.log = log
        self.state_path = state_path
        self.state = self._load()
        self.cfg = IssuesConfig.from_dict((cog.get_config() or {}).get("issues"))
        self._ignore_rx = self._compile_ignores()
        self._ctx: dict = {}          # source -> deque of recent raw lines
        self._last_scan = 0.0
        self._notices: deque = deque(maxlen=20)   # timestamps of new-issue notices
        self._lock = asyncio.Lock()

    # ---- state -------------------------------------------------------------------

    def _load(self) -> dict:
        try:
            with open(self.state_path, encoding="utf-8") as fh:
                doc = json.load(fh)
            if isinstance(doc, dict):
                doc.setdefault("issues", {})
                doc.setdefault("cursors", {})
                return doc
        except (OSError, ValueError):
            pass
        return {"issues": {}, "cursors": {}}

    def _save(self) -> None:
        try:
            tmp = self.state_path + ".tmp"
            with open(tmp, "w", encoding="utf-8") as fh:
                json.dump(self.state, fh, indent=1)
            os.replace(tmp, self.state_path)
        except OSError as ex:
            self.log.error(f"FowlEngine/issues: could not save {self.state_path}: {ex}")

    def reload_config(self) -> None:
        self.cfg = IssuesConfig.from_dict((self.cog.get_config() or {}).get("issues"))
        self._ignore_rx = self._compile_ignores()

    def _compile_ignores(self) -> list:
        out = []
        for pat in self.cfg.ignore:
            try:
                out.append(re.compile(pat))
            except re.error as ex:
                self.log.warning(f"FowlEngine/issues: bad ignore pattern {pat!r}: {ex}")
        return out

    def _secrets(self) -> list[str]:
        vals: list[str] = []

        def walk(node):
            if isinstance(node, dict):
                for k, v in node.items():
                    kl = str(k).lower()
                    if isinstance(v, str) and v.strip() and any(
                            h in kl for h in ("password", "secret", "api_key", "token", "webhook", "llm_key")):
                        vals.append(v)
                    else:
                        walk(v)
            elif isinstance(node, list):
                for i in node:
                    walk(i)
        walk(getattr(self.cog, "locals", None) or {})
        return vals

    # ---- sources -----------------------------------------------------------------

    def sources(self) -> list[tuple[str, str]]:
        """[(source name, path)] for everything on this PC."""
        out: list[tuple[str, str]] = []
        try:
            from .upload import is_remote_node
        except Exception:  # noqa: BLE001
            is_remote_node = lambda n: False  # noqa: E731
        for s in list(self.cog.bot.servers.values()):
            if is_remote_node(getattr(s, "node", None)):
                continue
            home = getattr(getattr(s, "instance", None), "home", None)
            if not home:
                continue
            is_range = False
            try:
                is_range = self.cog._is_range(s)
            except Exception:  # noqa: BLE001
                pass
            out.append((f"engine:{s.name}", os.path.join(home, "Logs", "bfrange.txt" if is_range else "bfnext.txt")))
            if self.cfg.include_dcs_log:
                out.append((f"dcs:{s.name}", os.path.join(home, "Logs", "dcs.log")))
        cfg = self.cog.get_config() or {}
        b = cfg.get("bfdb") or {}
        lf = b.get("log_file")
        if lf:
            out.append(("bfdb", os.path.expandvars(lf)))
        elif b.get("home"):
            out.append(("bfdb", os.path.join(os.path.expandvars(b["home"]), "Logs", "bfdb.log")))
        node = getattr(self.cog.bot.node, "name", "")
        out.append(("bot", os.path.abspath(os.path.join("logs", f"dcssb-{node}.log"))))
        for x in self.cfg.extra_files:
            kind = str(x.get("kind") or "").strip()
            name = str(x.get("name") or os.path.basename(x["path"]))
            out.append((f"{kind}:{name}" if kind in _STARTERS else f"file:{name}", os.path.expandvars(x["path"])))
        return out

    @staticmethod
    def _ident(st) -> str:
        # NTFS keeps a file's index (st_ino) and creation time across a rename,
        # so a log renamed aside (bfnext.txt -> bfnext<ts>.txt, dcs.log ->
        # dcs.log.old) is still recognisable as the file we were reading.
        return f"{getattr(st, 'st_ino', 0)}:{int(getattr(st, 'st_ctime', 0))}"

    def _find_rotated(self, path: str, ident: str) -> Optional[str]:
        d = os.path.dirname(path)
        stem = os.path.basename(path).split(".")[0].lower()
        try:
            for e in os.scandir(d):
                if not e.is_file() or e.path == path or not e.name.lower().startswith(stem):
                    continue
                try:
                    # os.stat, not e.stat(): on Windows a DirEntry's st_ino is always 0
                    if self._ident(os.stat(e.path)) == ident:
                        return e.path
                except OSError:
                    continue
        except OSError:
            pass
        return None

    @staticmethod
    def _read_chunk(path: str, offset: int, final: bool) -> tuple[list[str], int, bool]:
        """(lines, new offset, reached end). Reads at most READ_CAP_BYTES. A
        half-written last line is left for next time -- unless `final` (a file
        that was rotated away and will never grow again)."""
        try:
            size = os.path.getsize(path)
            with open(path, "rb") as fh:
                fh.seek(offset)
                data = fh.read(min(READ_CAP_BYTES, max(0, size - offset)))
        except OSError:
            return [], offset, True
        at_end = offset + len(data) >= size
        if not (final and at_end):
            cut = data.rfind(b"\n")
            if cut < 0:
                return [], offset, at_end and final
            data = data[:cut + 1]
        return data.decode("utf-8", "replace").splitlines(), offset + len(data), offset + len(data) >= size

    def _read_new(self, source: str, path: str) -> tuple[list[str], bool]:
        """(lines appended since the last scan, still catching up?).

        Nothing is skipped: a file seen for the first time is read from the
        start (a bounded chunk per scan until caught up), and when a log was
        rotated since the last scan -- bflib and bfdb rename theirs aside at
        start, DCS moves dcs.log to dcs.log.old -- the rest of the OLD file is
        read first, so the last lines before a crash/restart are kept."""
        cursors = self.state["cursors"]
        cur = cursors.get(source) or {}
        out: list[str] = []
        try:
            st = os.stat(path)
            ident = self._ident(st)
        except OSError:
            st, ident = None, None
        if cur.get("ident") and cur.get("ident") != ident:
            old = self._find_rotated(path, cur["ident"])
            if old:
                lines, off, done = self._read_chunk(old, int(cur.get("offset") or 0), final=True)
                out.extend(lines)
                if not done:
                    cursors[source] = {**cur, "offset": off}
                    return out, True
            if st is None:
                cursors.pop(source, None)
                return out, False
            cur = {"path": path, "ident": ident, "offset": 0, "seen": True}
        if st is None:
            return out, False
        first_sight = not cur
        offset = int(cur.get("offset") or 0) if cur.get("ident") == ident else 0
        if st.st_size < offset:  # truncated in place
            offset = 0
        lines, off, done = self._read_chunk(path, offset, final=False)
        out.extend(lines)
        cursors[source] = {"path": path, "ident": ident, "offset": off}
        return out, (not done) or first_sight

    # ---- the persistent archive -------------------------------------------------------

    def archive_root(self) -> Optional[str]:
        if not self.cfg.archive:
            return None
        if self.cfg.archive_dir:
            return self.cfg.archive_dir
        home = ((self.cog.get_config() or {}).get("bfdb") or {}).get("home")
        if home:
            return os.path.join(os.path.expandvars(home), "_logarchive")
        return os.path.abspath(os.path.join("logs", "fowl-archive"))

    @staticmethod
    def _safe(source: str) -> str:
        return re.sub(r"[^A-Za-z0-9._-]+", "_", source).strip("_")[:80] or "log"

    def _archive(self, source: str, lines: list[str]) -> None:
        """Append raw lines to <archive>/<source>/<UTC date>.log. Everything the
        analyzer reads lands here, so a log DCS overwrites on restart (dcs.log)
        or the bot rotates away is still there weeks later."""
        root = self.archive_root()
        if not root or not lines:
            return
        d = os.path.join(root, self._safe(source))
        try:
            os.makedirs(d, exist_ok=True)
            day = datetime.now(timezone.utc).strftime("%Y-%m-%d")
            with open(os.path.join(d, f"{day}.log"), "a", encoding="utf-8", newline="\n") as fh:
                fh.write("\n".join(lines) + "\n")
        except OSError as ex:
            self.log.warning(f"FowlEngine/issues: archive write failed for {source}: {ex}")

    def _archive_maintenance(self) -> None:
        """Once a day: gzip finished days, drop days past archive_days."""
        import gzip
        import shutil as _sh

        root = self.archive_root()
        today = datetime.now(timezone.utc).strftime("%Y-%m-%d")
        if not root or self.state.get("archive_maint") == today or not os.path.isdir(root):
            return
        self.state["archive_maint"] = today
        cutoff = (time.time() - self.cfg.archive_days * 86400) if self.cfg.archive_days > 0 else None
        for src in os.scandir(root):
            if not src.is_dir():
                continue
            for f in os.scandir(src.path):
                day = f.name.split(".")[0]
                try:
                    t = datetime.strptime(day, "%Y-%m-%d").replace(tzinfo=timezone.utc).timestamp()
                except ValueError:
                    continue
                if cutoff is not None and t < cutoff:
                    try:
                        os.remove(f.path)
                    except OSError:
                        pass
                    continue
                if f.name.endswith(".log") and day != today:
                    try:
                        with open(f.path, "rb") as fin, gzip.open(f.path + ".gz", "ab") as fout:
                            _sh.copyfileobj(fin, fout)
                        os.remove(f.path)
                    except OSError as ex:
                        self.log.warning(f"FowlEngine/issues: could not compress {f.path}: {ex}")

    def archive_index(self) -> list[dict]:
        root = self.archive_root()
        out: list[dict] = []
        if not root or not os.path.isdir(root):
            return out
        for src in sorted(os.scandir(root), key=lambda e: e.name):
            if not src.is_dir():
                continue
            days: dict = {}
            for f in os.scandir(src.path):
                if f.name.endswith((".log", ".log.gz")):
                    day = f.name.split(".")[0]
                    cur = days.setdefault(day, {"date": day, "size": 0, "compressed": False})
                    cur["size"] += f.stat().st_size
                    cur["compressed"] = cur["compressed"] or f.name.endswith(".gz")
            out.append({"source": src.name, "days": sorted(days.values(), key=lambda d: d["date"], reverse=True)})
        return out

    def archive_read(self, source: str, date: str, lines: int = 500, grep: Optional[str] = None) -> list[str]:
        """Tail (optionally grep-filtered) of one archived day."""
        import gzip

        root = self.archive_root()
        if not root or not re.fullmatch(r"\d{4}-\d{2}-\d{2}", date or ""):
            return []
        d = os.path.join(root, self._safe(source))
        keep: deque = deque(maxlen=max(1, min(int(lines), 50_000)))
        g = (grep or "").lower()
        # a day compressed while still being appended to has both files; the
        # .gz half is the older part
        for name, opener in ((f"{date}.log.gz", gzip.open), (f"{date}.log", open)):
            p = os.path.join(d, name)
            if not os.path.exists(p):
                continue
            try:
                with opener(p, "rt", encoding="utf-8", errors="replace") as fh:
                    for line in fh:
                        if not g or g in line.lower():
                            keep.append(line.rstrip("\n"))
            except OSError:
                pass
        return list(keep)

    def _crash_dumps(self) -> list[Entry]:
        """New minidumps / crash reports in each local DCS instance's Logs dir."""
        out = []
        seen = self.state.setdefault("dumps", {})
        for s in list(self.cog.bot.servers.values()):
            home = getattr(getattr(s, "instance", None), "home", None)
            if not home:
                continue
            d = os.path.join(home, "Logs")
            try:
                files = [e for e in os.scandir(d)
                         if e.is_file() and (e.name.lower().endswith((".dmp", ".crash"))
                                             or e.name.lower().startswith("dcs.crash"))]
            except OSError:
                continue
            first = s.name not in seen
            known = set(seen.get(s.name) or [])
            for e in files:
                key = f"{e.name}:{int(e.stat().st_mtime)}"
                if key in known:
                    continue
                known.add(key)
                if not first:
                    out.append(Entry(source=f"dcs:{s.name}", level="CRASH",
                                     text=f"DCS crashed -- new crash file {e.name}",
                                     extra=[f"{e.path} ({e.stat().st_size} bytes, "
                                            f"{datetime.fromtimestamp(e.stat().st_mtime, timezone.utc).isoformat()})"]))
            seen[s.name] = sorted(known)[-50:]
        return out

    # ---- the scan ------------------------------------------------------------------------

    def _builds(self) -> dict:
        """Which builds were running when an issue was seen (for the report)."""
        out = {}
        upd = getattr(self.cog, "updater", None)
        try:
            for k, v in ((upd.state.get("installed") or {}) if upd else {}).items():
                if v.get("git") or v.get("tag"):
                    out[k] = v.get("git") or v.get("tag")
        except Exception:  # noqa: BLE001
            pass
        for s in list(self.cog.bot.servers.values()):
            home = getattr(getattr(s, "instance", None), "home", None)
            side = os.path.join(home, "Logs", "bfnext-bflib-build.json") if home else None
            if side and os.path.exists(side):
                try:
                    with open(side, encoding="utf-8") as fh:
                        out[f"bflib@{s.name}"] = json.load(fh).get("git")
                except (OSError, ValueError):
                    pass
        return out

    def ingest(self, entries: list[Entry], secrets: list[str], builds: Optional[dict] = None) -> list[dict]:
        """Fold entries into the issue table. Returns issues that are NEW or
        REGRESSED by this batch (for notification)."""
        issues = self.state["issues"]
        fresh: list[dict] = []
        now = _utc()
        build_tags = sorted({str(v) for v in (builds or {}).values() if v})[:6]
        for e in entries:
            full = e.full
            if any(rx.search(full) for rx in self._ignore_rx):
                continue
            fid = fingerprint(e)
            it = issues.get(fid)
            if it is None:
                if len(issues) >= MAX_ISSUES:
                    self._evict(issues)
                it = issues[fid] = {
                    "id": fid, "source": e.source, "kind": _kind_of(e.source), "level": e.level,
                    "signature": scrub(signature(e), secrets), "first_seen": now, "last_seen": now,
                    "count": 0, "status": "new", "samples": [], "builds": [], "sources": [],
                }
                fresh.append(it)
            elif it.get("status") == "fixed":
                it["status"] = "regressed"
                it["regressed_at"] = now
                fresh.append(it)
            elif it.get("status") == "quiet":
                it["status"] = "open"
            it["count"] = int(it.get("count") or 0) + 1
            it["last_seen"] = now
            if LEVEL_RANK.get(e.level, 0) > LEVEL_RANK.get(it.get("level"), 0):
                it["level"] = e.level
            if e.source not in it["sources"]:
                it["sources"] = (it["sources"] + [e.source])[-6:]
            for b in build_tags:
                if b not in it["builds"]:
                    it["builds"] = (it["builds"] + [b])[-8:]
            if len(it["samples"]) < SAMPLES_PER_ISSUE or it["count"] % 50 == 0:
                sample = {
                    "at": now, "source": e.source,
                    "context": [scrub(l, secrets)[:400] for l in e.context[-CONTEXT_BEFORE:]],
                    "text": scrub(full, secrets)[:4000],
                }
                it["samples"] = (it["samples"] + [sample])[-SAMPLES_PER_ISSUE:]
        return fresh

    @staticmethod
    def _evict(issues: dict) -> None:
        """Drop the least interesting: ignored, then oldest-seen low counts."""
        order = sorted(issues.values(), key=lambda i: (
            i.get("status") != "ignored", LEVEL_RANK.get(i.get("level"), 0), i.get("last_seen", "")))
        for it in order[:max(1, len(issues) // 10)]:
            issues.pop(it["id"], None)

    def _age_out(self) -> None:
        """Open issues not seen for a week go quiet (hidden by default)."""
        cutoff = time.time() - 7 * 86400
        for it in self.state["issues"].values():
            if it.get("status") in ("new", "open", "acknowledged", "regressed"):
                try:
                    t = datetime.strptime(it["last_seen"], "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=timezone.utc)
                except (KeyError, ValueError):
                    continue
                if t.timestamp() < cutoff:
                    it["status"] = "quiet"

    async def scan(self) -> dict:
        async with self._lock:
            loop = asyncio.get_running_loop()
            secrets = self._secrets()
            builds = self._builds()
            entries: list[Entry] = []
            read = {}
            backlog_entries: list[Entry] = []
            for source, path in self.sources():
                lines, backlog = await loop.run_in_executor(None, self._read_new, source, path)
                read[source] = len(lines)
                if not lines:
                    continue
                await loop.run_in_executor(None, self._archive, source, lines)
                ctx = self._ctx.setdefault(source, deque(maxlen=CONTEXT_BEFORE))
                found = parse_entries(source, lines, self.cfg.min_level, ctx)
                # history being caught up on is recorded, but not announced
                # as if it had just happened
                (backlog_entries if backlog else entries).extend(found)
            entries.extend(await loop.run_in_executor(None, self._crash_dumps))
            fresh = self.ingest(entries, secrets, builds)
            for it in self.ingest(backlog_entries, secrets, builds):
                it["notified"] = True
            entries += backlog_entries
            self._age_out()
            await loop.run_in_executor(None, self._archive_maintenance)
            self.state["last_scan"] = _utc()
            self._save()
        await self._announce(fresh)
        await self._file_github()
        return {"entries": len(entries), "new": len(fresh), "read": read}

    async def tick(self) -> None:
        if not self.cfg.enabled:
            return
        if time.monotonic() - self._last_scan < self.cfg.scan_seconds:
            return
        self._last_scan = time.monotonic()
        try:
            await self.scan()
        except Exception as ex:  # noqa: BLE001
            self.log.exception(f"FowlEngine/issues: scan failed: {ex}")

    # ---- outputs -------------------------------------------------------------------------

    async def _announce(self, fresh: list[dict]) -> None:
        if not self.cfg.notify or not fresh:
            return
        floor = LEVEL_RANK.get(self.cfg.notify_min_level, 3)
        for it in fresh:
            it["notified"] = True
            if LEVEL_RANK.get(it.get("level"), 0) < floor:
                continue
            now = time.time()
            recent = [t for t in self._notices if now - t < 600]
            if len(recent) >= 5:
                continue  # a storm: the OPS page has the rest
            self._notices.append(now)
            tag = "🔁 REGRESSED" if it.get("status") == "regressed" else "🆕 New"
            try:
                await self.cog.notify_ops(
                    f"{tag} **{it['level']}** issue `{it['id']}` in `{it['source']}`:\n"
                    f"```{it['signature'][:300]}```See the OPS page (Issues) for samples.")
            except Exception as ex:  # noqa: BLE001
                self.log.debug(f"FowlEngine/issues: notice failed: {ex}")
        self._save()

    async def _file_github(self) -> None:
        """Open one GitHub issue per NEW error-level issue once it has recurred
        `min_count` times (a one-off blip isn't worth a ticket)."""
        c = self.cfg
        if not (c.github_repo and c.github_token):
            return
        import aiohttp
        todo = [it for it in self.state["issues"].values()
                if it.get("status") in ("new", "open", "regressed")
                and LEVEL_RANK.get(it.get("level"), 0) >= 3
                and int(it.get("count") or 0) >= c.github_min_count
                and (not it.get("github_url") or (it.get("status") == "regressed" and not it.get("github_regress_noted")))]
        if not todo:
            return
        headers = {"Authorization": f"Bearer {c.github_token}", "Accept": "application/vnd.github+json",
                   "User-Agent": "fowlengine-issues"}
        async with aiohttp.ClientSession() as http:
            for it in todo[:5]:
                try:
                    if it.get("github_url") and it.get("status") == "regressed":
                        num = it["github_url"].rstrip("/").rsplit("/", 1)[-1]
                        async with http.post(
                                f"https://api.github.com/repos/{c.github_repo}/issues/{num}/comments",
                                headers=headers, timeout=20,
                                json={"body": "Seen again after being marked fixed.\n\n" + issue_markdown(it)}) as r:
                            if r.status in (200, 201):
                                it["github_regress_noted"] = True
                        continue
                    body = issue_markdown(it) + f"\n\n<!-- fowl-issue:{it['id']} -->"
                    title = f"[{it['kind']}] {it['level']}: {it['signature'][:90]}"
                    async with http.post(f"https://api.github.com/repos/{c.github_repo}/issues",
                                         headers=headers, timeout=20,
                                         json={"title": title, "body": body, "labels": c.github_labels}) as r:
                        if r.status in (200, 201):
                            it["github_url"] = (await r.json()).get("html_url")
                        else:
                            self.log.warning(f"FowlEngine/issues: GitHub issue create -> HTTP {r.status}: "
                                             f"{(await r.text())[:200]}")
                            break
                except Exception as ex:  # noqa: BLE001
                    self.log.warning(f"FowlEngine/issues: GitHub filing failed: {ex}")
                    break
        self._save()

    # ---- used by the OPS page ---------------------------------------------------------

    def set_status(self, fid: str, status: str, note: Optional[str] = None) -> dict:
        changed = self.set_status_many([fid], status, note)
        if not changed:
            raise KeyError(fid)
        return self.state["issues"][fid]

    def set_status_many(self, ids: list, status: str, note: Optional[str] = None) -> int:
        """Mark several issues at once. "fixed" and "ignored" take them off the
        list; a fixed one that shows up in the logs again comes back as
        REGRESSED, an ignored one stays hidden. Returns how many existed."""
        if status not in ("open", "acknowledged", "ignored", "fixed"):
            raise ValueError("status must be open, acknowledged, ignored or fixed")
        n = 0
        for fid in ids:
            it = self.state["issues"].get(str(fid))
            if it is None:
                continue
            it["status"] = status
            it["status_at"] = _utc()
            if note:
                it["note"] = note[:500]
            if status == "fixed":
                it.pop("github_regress_noted", None)
            n += 1
        self._save()
        return n

    def forget(self, ids: list) -> int:
        """Delete issues outright. A forgotten one that happens again simply
        comes back as a new issue."""
        n = 0
        for fid in ids:
            if self.state["issues"].pop(str(fid), None) is not None:
                n += 1
        self._save()
        return n

    def clear(self, which: str = "closed") -> int:
        """Forget issues: 'closed' (fixed/ignored/quiet) or 'all'."""
        issues = self.state["issues"]
        drop = [k for k, v in issues.items()
                if which == "all" or v.get("status") in ("fixed", "ignored", "quiet")]
        return self.forget(drop)

    def listing(self, include_closed: bool = False) -> list[dict]:
        rows = []
        for it in self.state["issues"].values():
            if not include_closed and it.get("status") in ("fixed", "ignored", "quiet"):
                continue
            rows.append(it)
        rows.sort(key=lambda i: (
            i.get("status") not in ("regressed", "new"), -LEVEL_RANK.get(i.get("level"), 0),
            -(int(i.get("count") or 0)), i.get("last_seen", "")))
        return rows

    def summary(self) -> dict:
        rows = self.listing()
        return {
            "enabled": self.cfg.enabled,
            "last_scan": self.state.get("last_scan"),
            "open": len(rows),
            "new": sum(1 for r in rows if r.get("status") in ("new", "regressed")),
            "errors": sum(1 for r in rows if LEVEL_RANK.get(r.get("level"), 0) >= 3),
            "github": bool(self.cfg.github_repo and self.cfg.github_token),
            "sources": [{"source": s, "path": p, "exists": os.path.exists(p)} for s, p in self.sources()],
        }

    def report(self, include_closed: bool = False, min_level: str = "WARN", limit: int = 40) -> str:
        floor = LEVEL_RANK.get(_LEVEL_ALIASES.get(min_level.upper(), min_level.upper()), 2)
        rows = [r for r in self.listing(include_closed) if LEVEL_RANK.get(r.get("level"), 0) >= floor]
        return render_report(rows[:limit], self._builds(), getattr(self.cog.bot.node, "name", "?"),
                             self.state.get("last_scan"), total=len(rows))


def issue_markdown(it: dict) -> str:
    lines = [
        f"### `{it['id']}` {it['level']} in {', '.join(it.get('sources') or [it['source']])}"
        + (" — REGRESSED" if it.get("status") == "regressed" else ""),
        f"- seen **{it.get('count', 0)}×**, first {it.get('first_seen')}, last {it.get('last_seen')}",
        f"- status: {it.get('status')}" + (f" — note: {it['note']}" if it.get("note") else ""),
    ]
    if it.get("builds"):
        lines.append(f"- builds running when seen: {', '.join(f'`{b}`' for b in it['builds'])}")
    lines.append(f"- signature: `{it['signature']}`")
    for i, s in enumerate(it.get("samples") or [], 1):
        body = "\n".join([*(f"  {c}" for c in s.get("context") or []), s.get("text", "")])
        lines.append(f"\nSample {i} ({s.get('source')}, {s.get('at')}):\n```\n{body}\n```")
    return "\n".join(lines)


def render_report(rows: list[dict], builds: dict, node: str, last_scan: Optional[str], total: int) -> str:
    head = [
        "# Fowl Engine issue report",
        f"Node `{node}` · generated {_utc()} · last scan {last_scan or 'never'}",
        "",
        "Running builds: " + (", ".join(f"{k} `{v}`" for k, v in sorted(builds.items())) or "unknown"),
        "",
        f"{total} open issue(s)" + (f", showing the top {len(rows)}" if total > len(rows) else "")
        + ". Sorted new/regressed first, then by severity and count. Log timestamps are the "
          "server's local/UTC clocks as each log writes them.",
        "",
    ]
    if not rows:
        head.append("No open issues. 🎉")
    body = [issue_markdown(r) + "\n" for r in rows]
    return "\n".join(head + body)
