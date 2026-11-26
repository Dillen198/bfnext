"""
The community channels (cog_community.py): which Discord channels are the bug
forum, the changelog and the question channels, which releases have had their
changelog posted, and the wording the bot uses in them.

Channels come from fowlengine.yaml (`community:`) unless an admin picked them
in Discord with /fe_community_channels or /fe_community_setup; a pick is kept
in fowlengine_community.json and wins over the YAML.

Stdlib only; no discord import, so the tests run it as it is.
"""
from __future__ import annotations

import json
import os
import random
from typing import Optional

# Forum tags for a bug report's state. Moderated: only people who can manage
# threads (and the bot) set them, so a player can't mark their own post fixed.
STATUS_TAGS = {
    "open": "Open",
    "working": "Being fixed",
    "fixed": "Fixed",
    "no_fix": "Not an engine bug",
    "closed": "Closed",
}

DEFAULT_NAMES = {
    "category": "Fowl Engine",
    "bug_forum": "bug-reports",
    "changelog_channel": "changelog",
    "questions": "ask-a-question",
}

POSTED_KEEP = 200


class CommunityState:
    def __init__(self, log, path: str):
        self.log = log
        self.path = path
        self.state = self._load()

    def _load(self) -> dict:
        try:
            with open(self.path, encoding="utf-8") as f:
                doc = json.load(f)
            if isinstance(doc, dict):
                return doc
        except FileNotFoundError:
            pass
        except Exception as ex:  # noqa: BLE001 -- a bad file must not stop the bot
            self.log.error(f"FowlEngine/community: could not read {self.path}: {ex}")
        return {}

    def _save(self) -> None:
        tmp = self.path + ".tmp"
        try:
            os.makedirs(os.path.dirname(self.path) or ".", exist_ok=True)
            with open(tmp, "w", encoding="utf-8") as f:
                json.dump(self.state, f, indent=1)
            os.replace(tmp, self.path)
        except Exception as ex:  # noqa: BLE001
            self.log.error(f"FowlEngine/community: could not save {self.path}: {ex}")

    # ---- channels ----------------------------------------------------------------

    def channels(self, yaml_cfg: Optional[dict]) -> dict:
        """{bug_forum: id|None, changelog_channel: id|None, questions: [ids]}:
        an admin's pick in Discord, else fowlengine.yaml's community block."""
        y = yaml_cfg or {}
        picked = self.state.get("channels") or {}

        def one(key: str) -> Optional[int]:
            v = picked.get(key, y.get(key))
            try:
                return int(v) if v else None
            except (TypeError, ValueError):
                return None
        qs = picked.get("questions", y.get("questions"))
        if qs and not isinstance(qs, list):
            qs = [qs]
        out_q = []
        for v in qs or []:
            try:
                out_q.append(int(v))
            except (TypeError, ValueError):
                pass
        return {"bug_forum": one("bug_forum"), "changelog_channel": one("changelog_channel"), "questions": out_q}

    def pick(self, **chosen) -> None:
        """Keep an admin's choice. A value of 0 clears that pick (back to the YAML)."""
        picked = self.state.setdefault("channels", {})
        for k, v in chosen.items():
            if v is None:
                continue
            if v == 0 or v == []:
                picked.pop(k, None)
            else:
                picked[k] = v
        self._save()

    # ---- changelog -----------------------------------------------------------------

    def posted(self, tag: str) -> bool:
        return tag in (self.state.get("changelog_posted") or [])

    def mark_posted(self, tag: str) -> None:
        done = list(self.state.get("changelog_posted") or [])
        if tag not in done:
            done.append(tag)
        self.state["changelog_posted"] = done[-POSTED_KEEP:]
        self._save()


def changelog_post(text: str, tag: str, url: Optional[str] = None) -> str:
    """The message for the changelog channel. Discord's -# line is the small grey footer."""
    foot = f"-# {tag}" + (f" · [release]({url})" if url else "")
    return f"{text.strip()}\n{foot}"[:1990]


def thread_title(text: str, limit: int = 90) -> str:
    """A forum post title from the first sentence of a report."""
    t = " ".join((text or "").split())
    for stop in (". ", "? ", "! ", "\n"):
        i = t.find(stop)
        if 15 <= i < limit:
            t = t[:i + 1]
            break
    return (t[:limit - 1] + "…") if len(t) > limit else (t or "Bug report")


# The bot's lines in a bug thread. Plain and short, as a regular would write
# them; a few of each so the forum doesn't read like a form letter.
_NEW = (
    "Cheers, got it, logged as `{id}`. If you can, add the server, the airframe and roughly when it "
    "happened (Zulu). A Tacview or a screenshot helps a lot.",
    "Thanks for flagging it, this one's logged as `{id}`. Which server and airframe were you on, and "
    "about what time (Zulu)? A Tacview or screenshot makes it much quicker to chase down.",
    "Logged as `{id}`, thanks. Drop the server, airframe and rough time (Zulu) in here if you've got "
    "them, plus a Tacview or screenshot if you have one.",
)
_NEW_TRUSTED = " It's gone straight into the fix queue."
_NEW_WAITING = " An admin will give it a look."
_STATUS = {
    "working": ("This one's being worked on now.", "On it now, hang tight."),
    "fixed": ("Fixed. {note} It goes out with the next update; you'll have it once the servers pick it up.",
              "Sorted. {note} It rolls out with the next update."),
    "no_fix": ("Had a look at this one. {note}", "Looked into it. {note}"),
    "closed": ("Closing this one out. Shout if it comes back.",),
}
_NO_NOTE = {
    "fixed": "",
    "no_fix": "It doesn't look like something in the Fowl Engine itself, so there's nothing to change on "
              "our side for now.",
}


def new_report_line(bid: str, trusted: bool, rnd: Optional[random.Random] = None) -> str:
    r = rnd or random
    return r.choice(_NEW).format(id=bid) + (_NEW_TRUSTED if trusted else _NEW_WAITING)


def status_line(status: str, note: Optional[str], rnd: Optional[random.Random] = None) -> Optional[str]:
    if status not in _STATUS:
        return None
    r = rnd or random
    note = (note or "").strip() or _NO_NOTE.get(status, "")
    return " ".join(r.choice(_STATUS[status]).format(note=note).split())
