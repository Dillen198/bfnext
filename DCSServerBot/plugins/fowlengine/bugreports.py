"""
Player bug reports from Discord (/fe_bug), kept on the box for the auto-fix
runner (deploy/autofix) to pick up over the OPS API.

    reporter -> /fe_bug "what went wrong" -> fowlengine_bugs.json
    autofix.py (the admin's PC, over Tailscale) -> GET  <ops>/bugs
                                                -> POST <ops>/bugs/status

What leaves the box is only the report text (scrubbed of IPs, UCIDs and
secrets the same way the log analyzer scrubs log lines), when, which server
and whether the reporter holds a trusted role. Never their Discord id or name:
the runner posts summaries to Discord and pushes public branches.

A report from a trusted reporter (DCS Admin, or `bugs.trusted_roles`) may be
fixed and published with nobody looking. Anyone else's is kept and shown in
Fowl Engine Manager's AUTO-FIX tab, where an admin sends it to Claude. That is
the line between "a stranger typed something" and "code ships to the servers".

Stdlib only; no discord import, so the tests can run it as it is.
"""
from __future__ import annotations

import hashlib
import json
import os
import time
from datetime import datetime, timezone
from typing import Callable, Iterable, Optional

from .loganalyzer import scrub

MAX_REPORTS = 300          # oldest closed ones go first past this
MIN_TEXT = 15
MAX_TEXT = 1500
PER_USER_PER_HOUR = 3
STATUSES = ("open", "working", "fixed", "no_fix", "closed")
CLOSED = ("fixed", "no_fix", "closed")


def _utc(ts: Optional[float] = None) -> str:
    return datetime.fromtimestamp(ts if ts is not None else time.time(), timezone.utc).isoformat(timespec="seconds")


class BugReports:
    def __init__(self, log, path: str, secrets: Callable[[], Iterable[str]] = lambda: ()):
        self.log = log
        self.path = path
        self.secrets = secrets
        self.state = self._load()

    def _load(self) -> dict:
        try:
            with open(self.path, encoding="utf-8") as f:
                doc = json.load(f)
            if isinstance(doc, dict) and isinstance(doc.get("bugs"), dict):
                return doc
        except FileNotFoundError:
            pass
        except Exception as ex:  # noqa: BLE001 -- a bad file must not stop the bot
            self.log.error(f"FowlEngine/bugs: could not read {self.path}: {ex}")
        return {"bugs": {}}

    def _save(self) -> None:
        tmp = self.path + ".tmp"
        try:
            os.makedirs(os.path.dirname(self.path) or ".", exist_ok=True)
            with open(tmp, "w", encoding="utf-8") as f:
                json.dump(self.state, f, indent=1)
            os.replace(tmp, self.path)
        except Exception as ex:  # noqa: BLE001
            self.log.error(f"FowlEngine/bugs: could not save {self.path}: {ex}")

    # ---- writing ---------------------------------------------------------------

    def add(self, *, user_id: str, text: str, trusted: bool, server: Optional[str] = None,
            now: Optional[float] = None) -> dict:
        """Store one report. Raises ValueError (the message is for the reporter)
        when it is too short or they have sent too many lately."""
        now = now if now is not None else time.time()
        text = " ".join((text or "").split())
        if len(text) < MIN_TEXT:
            raise ValueError(f"tell us a bit more: at least {MIN_TEXT} characters")
        text = text[:MAX_TEXT]
        recent = [b for b in self.state["bugs"].values()
                  if b.get("_user") == str(user_id) and now - b.get("_ts", 0) < 3600]
        if len(recent) >= PER_USER_PER_HOUR:
            raise ValueError(f"you've sent {PER_USER_PER_HOUR} reports in the last hour; try again later")
        bid = "bug-" + hashlib.sha1(f"{user_id}|{now}|{text}".encode()).hexdigest()[:8]
        bug = {
            "id": bid, "status": "open", "trusted": bool(trusted),
            "server": scrub(server or "", self.secrets()) or None,
            "text": scrub(text, self.secrets()),
            "reported": _utc(now), "updated": _utc(now), "note": None,
            # kept on the box only: rate limits and telling the reporter
            "_user": str(user_id), "_ts": now,
        }
        self.state["bugs"][bid] = bug
        self._evict()
        self._save()
        return self.public(bug)

    def _evict(self) -> None:
        bugs = self.state["bugs"]
        if len(bugs) <= MAX_REPORTS:
            return
        order = sorted(bugs.values(), key=lambda b: (b["status"] not in CLOSED, b.get("_ts", 0)))
        for b in order[:len(bugs) - MAX_REPORTS]:
            bugs.pop(b["id"], None)

    def set_status(self, bid: str, status: str, note: Optional[str] = None) -> bool:
        if status not in STATUSES:
            raise ValueError(f"status must be one of {', '.join(STATUSES)}")
        bug = self.state["bugs"].get(bid)
        if bug is None:
            return False
        bug["status"] = status
        bug["updated"] = _utc()
        if note is not None:
            bug["note"] = str(note)[:500]
        self._save()
        return True

    def add_detail(self, bid: str, user_id: str, text: str) -> bool:
        """The reporter adding to their own open report (a reply in its forum
        post): appended, scrubbed, while it is still open and under twice the
        report's own limit. False when it was not taken."""
        bug = self.state["bugs"].get(bid)
        text = " ".join((text or "").split())
        if (bug is None or bug.get("_user") != str(user_id) or bug["status"] in CLOSED
                or len(text) < 3 or len(bug["text"]) >= 2 * MAX_TEXT):
            return False
        room = 2 * MAX_TEXT - len(bug["text"])
        bug["text"] = bug["text"] + " | More from the reporter: " + scrub(text, self.secrets())[:room]
        bug["updated"] = _utc()
        self._save()
        return True

    def set_thread(self, bid: str, thread_id: int) -> None:
        """The bug forum post that tracks this report (kept on the box)."""
        bug = self.state["bugs"].get(bid)
        if bug is not None:
            bug["_thread"] = int(thread_id)
            self._save()

    # ---- reading ---------------------------------------------------------------

    def thread(self, bid: str) -> Optional[int]:
        bug = self.state["bugs"].get(bid)
        return bug.get("_thread") if bug else None

    def by_thread(self, thread_id: int) -> Optional[dict]:
        for b in self.state["bugs"].values():
            if b.get("_thread") == int(thread_id):
                return self.public(b)
        return None

    @staticmethod
    def public(bug: dict) -> dict:
        """A report as it may leave the box: no reporter identity."""
        return {k: v for k, v in bug.items() if not k.startswith("_")}

    def reporter(self, bid: str) -> Optional[str]:
        bug = self.state["bugs"].get(bid)
        return bug.get("_user") if bug else None

    def listing(self, include_closed: bool = False) -> list[dict]:
        out = [self.public(b) for b in self.state["bugs"].values()
               if include_closed or b["status"] not in CLOSED]
        return sorted(out, key=lambda b: b["reported"], reverse=True)

    def summary(self) -> dict:
        bugs = list(self.state["bugs"].values())
        return {"open": sum(1 for b in bugs if b["status"] not in CLOSED),
                "trusted_open": sum(1 for b in bugs if b["status"] not in CLOSED and b.get("trusted")),
                "total": len(bugs)}
