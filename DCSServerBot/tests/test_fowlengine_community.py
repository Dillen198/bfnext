"""
The community side of the bot: the assistant that answers players
(assistant.py) and the channel picks, changelog bookkeeping and forum wording
(community.py). Both are stdlib-only and loaded straight from the plugin; the
Claude client is a stand-in that records what it was sent.
"""
import asyncio
import importlib.util
import logging
import random
import sys
import types
from pathlib import Path

import pytest

PLUGIN = Path(__file__).resolve().parent.parent / "plugins" / "fowlengine"


def _load(name):
    spec = importlib.util.spec_from_file_location(f"fe_{name}", PLUGIN / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = mod
    spec.loader.exec_module(mod)
    return mod


asst = _load("assistant")
comm = _load("community")
log = logging.getLogger("test")

PAGES = [
    {"slug": "f10-menu/ctld", "title": "Logistics (CTLD)", "content":
        "Intro to logistics.\n\n## Loading crates\nOpen F10 → Logistics → Load crate near a logistics "
        "building. Each crate weighs {{cfg:crate_weight|1000}} kg.\n![shot](/api/wiki/images/1)\n\n"
        "## Unpacking\nLand within 100 m and pick Unpack."},
    {"slug": "gameplay/lives", "title": "Lives", "content":
        "You get {{cfg:default_lives.fighter|3}} fighter lives per reset. Lives come back every "
        "{{cfg:reset_hours|6}} hours."},
    {"slug": "getting-started/slots", "title": "Picking a slot", "content": "Pick a slot at a base your side owns."},
]


def _knowledge():
    k = asst.Knowledge(log, "http://bfdb.invalid", "https://wiki.example")
    k.load(PAGES, {"default_lives": {"fighter": 4}})
    return k


class FakeClient:
    """Records each create() call and answers with the next canned reply."""

    def __init__(self, replies):
        self.replies = list(replies)
        self.calls = []
        self.beta = types.SimpleNamespace(messages=types.SimpleNamespace(create=self._create))

    async def _create(self, **kw):
        self.calls.append(kw)
        r = self.replies.pop(0)
        if isinstance(r, Exception):
            raise r
        stop, text = r if isinstance(r, tuple) else ("end_turn", r)
        return types.SimpleNamespace(stop_reason=stop, content=[
            types.SimpleNamespace(type="thinking", thinking=""),
            types.SimpleNamespace(type="text", text=text)])


def _assistant(replies, **cfg):
    fake = FakeClient(replies)
    a = asst.Assistant(log, asst.AssistantConfig.from_yaml({"enabled": True, **cfg}), _knowledge(),
                       client_factory=lambda c: fake)
    return a, fake


# ---- the wiki as reference notes -------------------------------------------------------

def test_the_wiki_is_cut_into_sections_with_live_numbers_filled_in():
    k = _knowledge()
    titles = [s.title for s in k.sections]
    assert "Logistics (CTLD): Loading crates" in titles and "Logistics (CTLD): Unpacking" in titles
    crates = next(s for s in k.sections if s.title.endswith("Loading crates"))
    assert "1000 kg" in crates.text and "/api/wiki/images" not in crates.text   # default after the bar, no images
    lives = next(s for s in k.sections if s.slug == "gameplay/lives")
    assert "4 fighter lives" in lives.text and "every 6 hours" in lives.text    # live value, then default


def test_search_finds_the_page_a_question_is_about():
    k = _knowledge()
    assert k.search("how do I load crates")[0].title.endswith("Loading crates")
    assert k.search("how many lives do I get")[0].slug == "gameplay/lives"
    assert k.search("the of and") == []
    notes = k.notes_for("unpack crates")
    assert "https://wiki.example" in notes and "Unpacking" in notes and "Live server settings" in notes


def test_a_long_page_is_cut_at_paragraphs():
    long = "\n\n".join(f"Paragraph {i} about FARP resupply." + " word" * 120 for i in range(20))
    secs = asst.split_page("x", "X", long, max_len=2000)
    assert len(secs) > 3 and all(len(s.text) <= 2000 for s in secs)
    assert all(s.text.startswith("Paragraph") for s in secs)


# ---- answering ------------------------------------------------------------------------

def test_an_answer_is_grounded_and_sent_with_the_fallback():
    a, fake = _assistant(["Yeah, F10 → Logistics → Load crate next to a logi building."])
    reply = asyncio.run(a.answer("how do I load a crate?", "Viper", [("Hog", "anyone know?")]))
    assert reply.startswith("Yeah, F10")
    kw = fake.calls[0]
    assert kw["model"] == "claude-opus-5-5" and kw["fallbacks"] == "default"
    assert kw["betas"] == [asst.FALLBACK_BETA] and kw["output_config"] == {"effort": "low"}
    assert "Loading crates" in kw["system"] and kw["system"].startswith(asst.PERSONA)
    msg = kw["messages"][0]["content"]
    assert "Hog: anyone know?" in msg and "from Viper:\nhow do I load a crate?" in msg


def test_no_fallback_for_a_model_that_has_none():
    a, fake = _assistant(["ok"], model="claude-haiku-5-5")
    asyncio.run(a.answer("what is a FARP", "x"))
    assert "fallbacks" not in fake.calls[0] and "betas" not in fake.calls[0]


@pytest.mark.parametrize("reply", ["SKIP", "skip.", ("refusal", "nope"), RuntimeError("overloaded"), ""])
def test_quiet_when_not_for_it_refused_or_failed(reply):
    a, _ = _assistant([reply])
    assert asyncio.run(a.answer("lol nice landing", "x")) is None


def test_replies_never_ping_and_stay_under_discords_limit():
    a, _ = _assistant(["@everyone listen up @here " + "word " * 600])
    reply = asyncio.run(a.answer("tell everyone", "x", mentioned=True))
    assert "@everyone" not in reply and "@here" not in reply and len(reply) <= 2000


def test_a_long_question_is_cut():
    a, fake = _assistant(["ok"])
    asyncio.run(a.answer("q" * 5000, "x"))
    assert "q" * asst.MAX_QUESTION in fake.calls[0]["messages"][0]["content"]
    assert "q" * (asst.MAX_QUESTION + 1) not in fake.calls[0]["messages"][0]["content"]


def test_limits_per_player_and_per_day():
    a, _ = _assistant([], per_user_per_hour=2, per_day=4)
    assert a.allow("1", now=0) is None and a.allow("1", now=1) is None
    assert "hourly" in a.allow("1", now=2)
    assert a.allow("1", now=3700) is None                      # an hour later
    assert a.allow("2", now=3701) is None
    assert "daily" in a.allow("3", now=3702)


def test_without_the_sdk_or_a_key_it_stays_off():
    a = asst.Assistant(log, asst.AssistantConfig.from_yaml({"enabled": True}), _knowledge(),
                       client_factory=lambda c: (_ for _ in ()).throw(ImportError("no anthropic")))
    assert a.client() is None and "anthropic" in a.unavailable and not a.active
    assert asyncio.run(a.answer("hi", "x")) is None


def test_off_unless_enabled():
    assert asst.AssistantConfig.from_yaml(None).enabled is False
    assert asst.AssistantConfig.from_yaml({"enabled": True, "per_day": 0}).per_day >= 1


def test_the_changelog_writer():
    a, fake = _assistant(["- CTLD crates stay put at FARPs after a restart now", "SKIP"])
    assert asyncio.run(a.write_changelog("autofix: keep crates across restart")).startswith("- CTLD")
    assert fake.calls[0]["system"] == asst.CHANGELOG_PROMPT
    assert asyncio.run(a.write_changelog("refactor the build")) is None       # nothing for players
    assert asyncio.run(a.write_changelog("")) is None and len(fake.calls) == 2


def test_a_player_note_is_made_safe():
    assert asst.clean_player_note("Fixed <@123> see https://x.y/z ```code``` @everyone") == "Fixed see code everyone"


# ---- channels, changelog, forum wording ------------------------------------------------

def test_a_pick_in_discord_wins_over_the_yaml_until_reset(tmp_path):
    st = comm.CommunityState(log, str(tmp_path / "c.json"))
    yaml_cfg = {"bug_forum": 11, "changelog_channel": "22", "questions": 33}
    assert st.channels(yaml_cfg) == {"bug_forum": 11, "changelog_channel": 22, "questions": [33]}
    st.pick(bug_forum=44, questions=[55, 66])
    again = comm.CommunityState(log, str(tmp_path / "c.json"))         # survives a restart
    assert again.channels(yaml_cfg) == {"bug_forum": 44, "changelog_channel": 22, "questions": [55, 66]}
    again.pick(bug_forum=0, changelog_channel=0, questions=0)
    assert again.channels(yaml_cfg)["bug_forum"] == 11 and again.channels(None)["questions"] == []


def test_each_release_is_posted_once(tmp_path):
    st = comm.CommunityState(log, str(tmp_path / "c.json"))
    assert not st.posted("engine-1")
    st.mark_posted("engine-1")
    st.mark_posted("engine-1")
    assert comm.CommunityState(log, str(tmp_path / "c.json")).posted("engine-1")
    assert comm.CommunityState(log, str(tmp_path / "c.json")).state["changelog_posted"] == ["engine-1"]
    post = comm.changelog_post("- crates stay put", "engine-1", "https://gh/rel")
    assert post.startswith("- crates stay put\n-# engine-1") and "[release](https://gh/rel)" in post


def test_forum_titles_and_lines():
    assert comm.thread_title("The F10 JTAC list is empty. It was fine yesterday.") == "The F10 JTAC list is empty."
    assert len(comm.thread_title("x" * 300)) == 90
    assert comm.thread_title("") == "Bug report"
    rnd = random.Random(1)
    line = comm.new_report_line("bug-1", True, rnd)
    assert "`bug-1`" in line and "fix queue" in line
    assert "admin" in comm.new_report_line("bug-1", False, rnd)
    assert "crates stay put" in comm.status_line("fixed", "CTLD crates stay put now.", rnd)
    assert "Fowl Engine" in comm.status_line("no_fix", "", rnd)          # a default when there's no note
    assert comm.status_line("open", None) is None
    assert set(comm.STATUS_TAGS) == {"open", "working", "fixed", "no_fix", "closed"}


def test_a_wiki_that_cant_be_read_is_retried_after_a_pause(monkeypatch):
    k = asst.Knowledge(log, "http://127.0.0.1:9")          # nothing listens there
    asyncio.run(k.refresh())
    assert k.sections == [] and k._next_try > asst.time.time() + 200
    calls = []
    monkeypatch.setitem(sys.modules, "aiohttp", types.SimpleNamespace(ClientSession=lambda **kw: calls.append(kw)))
    asyncio.run(k.refresh())                                # inside the pause: no request at all
    assert calls == []
    assert _knowledge()._next_try > asst.time.time() + 3000  # a loaded wiki waits an hour
