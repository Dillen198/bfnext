"""
Offline tests for the round over -> Discord post (roundresult.py). No bot, no
Discord, no bfdb: the /api/instances, summary and report replies are fixtures.

    python -m pytest DCSServerBot/tests -q
"""
import importlib.util
from datetime import datetime, timedelta, timezone
from pathlib import Path

BOT = Path(__file__).resolve().parent.parent          # <repo>/DCSServerBot
PLUGIN = BOT / "plugins" / "fowlengine"

_spec = importlib.util.spec_from_file_location("fe_roundresult", PLUGIN / "roundresult.py")
rr = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(rr)

NOW = datetime(2026, 10, 21, 8, 0, tzinfo=timezone.utc)


def last_round(*, rid=0, number=1, winner="Blue", ended=NOW - timedelta(hours=7), days=12.5):
    start = ended - timedelta(days=days)
    return {
        "id": rid, "number": number, "scenario": "modern",
        "start": start.isoformat(), "end": ended.isoformat(),
        "winner": winner,
        "winner_name": "Coalition" if winner == "Blue" else "Syria",
        "loser_name": "Syria" if winner == "Blue" else "Coalition",
    }


SUMMARY = {
    "id": 0, "number": 1, "pilots": 84, "air_kills": 412, "ground_kills": 2210,
    "captures": 96, "supply_runs": 310, "hours": 1543.2,
    "top": [
        {"ucid": "a", "name": "blueshack112", "air_kills": 33, "ground_kills": 1659,
         "captures": 12, "logistics": 40, "deaths": 9, "hours": 120.5},
        {"ucid": "b", "name": "Mayo", "air_kills": 32, "ground_kills": 403,
         "captures": 3, "logistics": 2, "deaths": 4, "hours": 60.0},
    ],
    "leaders": {
        "air": {"name": "blueshack112", "value": 33},
        "ground": {"name": "blueshack112", "value": 1659},
        "captures": {"name": "OB1", "value": 14},
        "logistics": None,
    },
}

REPORT = {
    "objectives": 116,
    "buckets": [
        {"blue_air": 10, "blue_ground": 50, "red_air": 8, "red_ground": 20,
         "blue_caps": 3, "red_caps": 2, "blue_held": 60, "red_held": 56, "neutral_held": 0},
        {"blue_air": 5, "blue_ground": 30, "red_air": 2, "red_ground": 4,
         "blue_caps": 40, "red_caps": 1, "blue_held": 111, "red_held": 5, "neutral_held": 0},
    ],
    "contested": [{"name": "Kharab Ishk", "captures": 9}, {"name": "Tabqa", "captures": 6}],
}


def test_announces_a_fresh_win_once():
    lr = last_round()
    assert rr.should_announce(lr, None, NOW.timestamp())
    # Already posted: never again, however often the round changes.
    assert not rr.should_announce(lr, 0, NOW.timestamp())
    # The next won round is posted even though one was before.
    assert rr.should_announce(last_round(rid=108000656, number=2), 0, NOW.timestamp())


def test_first_enable_does_not_backfill_old_rounds():
    old = last_round(ended=NOW - timedelta(days=3))
    assert not rr.should_announce(old, None, NOW.timestamp())


def test_no_winner_is_not_announced():
    assert not rr.should_announce(last_round(winner=None), None, NOW.timestamp())
    assert not rr.should_announce(None, None, NOW.timestamp())


def test_find_instance_by_id_then_server_name():
    data = {"instances": [{"id": "vs1", "dcs_server_name": "Modern"}, {"id": "vs2", "dcs_server_name": "2008"}]}
    assert rr.find_instance(data, "vs2", "Modern")["id"] == "vs2"
    assert rr.find_instance(data, None, "Modern")["id"] == "vs1"
    assert rr.find_instance(data, None, "nope") is None


def test_rundown_embed_is_detailed():
    e = rr.build_embed(last_round(), SUMMARY, REPORT, "https://dash.example/", "vs1", "Modern · Syria")
    assert e["title"] == "Round 1 is over: Coalition (Blue) wins"
    assert e["url"] == "https://dash.example/rounds?round=0&instance=vs1"
    assert "12.5 days" in e["description"] and "Modern · Syria" in e["description"]
    fields = {f["name"]: f["value"] for f in e["fields"]}
    assert "**111**" in fields["Final front line"] and "of 116 objectives" in fields["Final front line"]
    assert fields["Kills by Blue"] == "15 air · 80 ground"
    assert fields["Kills by Red"] == "10 air · 24 ground"
    assert fields["Captures"] == "Blue 43 · Red 3"
    assert "84 pilots" in fields["The round in numbers"]
    assert fields["MVP"].startswith("**blueshack112**")
    assert "2. Mayo" in fields["Top pilots"]
    assert "Top capturer: **OB1** (14 captures)" in fields["Best in each line of work"]
    assert "logistician" not in fields["Best in each line of work"]
    assert "Kharab Ishk: changed hands 9x" in fields["Most fought over"]
    assert e["color"] == 0x3B82F6


def test_embed_still_says_who_won_without_stats():
    e = rr.build_embed(last_round(winner="Red", number=None, rid=7), None, None)
    assert e["title"] == "Round 7 is over: Syria (Red) wins"
    assert e["fields"] == [] and "url" not in e
